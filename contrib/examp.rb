## examp.rb -- Guile -> Ruby translation

## Translator/Author: Michael Scholz <scholz-micha@gmx.de>
## Last: Sat Oct 19 02:55:10 CEST 2002
## Version: $Revision: 1.4 $

##
## Utilities
##
## help(func)
## car(v), cadr(v), caddr(v), cdr(v)
## warn(str), die(str), message(str)
## shell(cmd)
## get_func_name(n)
##
## Buffers
## 
## open_buffer(file)
## close_buffer(snd)
## add_to_reopen_menu(snd)
## check_reopen_menu(file)
##
## FM
## 
## fm_violin(start, dur, freq, amp, args)
## jc_reverb(args)
## with_sound(args) { ... }
## fm_play(func, outfile, play_f)
## fm_bell(start, dur, freq, amp, amp_env, index_env, index)
## n_rev(args)
## hello_dentist(frq, amp)
## ring_mod(freq, gliss_env)
## am(freq)
## vibro(speed, depth)
## fp(sr, osamp, osfreq)
## compand(h)
## 

$VERBOSE = true;		# Ruby's -v or -w option
$IN_SND = true;			# script mode: $IN_SND = false

#
# help([func=false])
# 

def help(func = false)
  if(func)
    send(func, :help);
  else
    message("## Functions available
##
## help(func)
## car(v), cadr(v), caddr(v), cdr(v)
## warn(str), die(str), message(str)
## shell(cmd)
## get_func_name(n)
##
## Buffers
## 
## open_buffer(file)
## close_buffer(snd)
## add_to_reopen_menu(snd)
## check_reopen_menu(file)
##
## FM
## 
## fm_violin(start, dur, freq, amp, args) [*]
## jc_reverb(args) [*]
## with_sound(args) { ... } [*]
## fm_play(func, outfile, play_f)
## fm_bell(start, dur, freq, amp, amp_env, index_env, index)
## n_rev(args) [*]
## hello_dentist(frq, amp) [*]
## ring_mod(freq, gliss_env) [*]
## am(freq) [*]
## vibro(speed, depth) [*]
## fp(sr, osamp, osfreq) [*]
## compand(h) [*]
##
## Global variables
##
## $VERBOSE = #{$VERBOSE}
## $IN_SND = #{$IN_SND}
##
## $rbm_file_name = \"#{$rbm_file_name}\"
## $rbm_srate = #{$rbm_srate}
## $rbm_channels = #{$rbm_channels}
## $rbm_header_type = Mus_next (#{$rbm_header_type})
## $rbm_data_format = Mus_bshort (#{$rbm_data_format})
## $rbm_comment = \"#{$rbm_comment}\"
## $rbm_statistics = #{$rbm_statistics}
## $rbm_play = #{$rbm_play}
## $rbm_player = #{$rbm_player}
## $rbm_reverb_file_name = \"#{$rbm_reverb_file_name}\";
## $rbm_reverb_channels = #{$rbm_reverb_channels}
##
##     [*]: function has option :help 

## Example: help :with_sound
##      or: with_sound :help");
  end
  false;
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
# If no error occures it works like snd_print() or print(), if an
# error occures it works like C's perror() function. It works in scipt
# mode and in Snd too.

def warn(str = "Warning")
  if($IN_SND)
    snd_print("\n#{str}#{$! ? ": #{$!}" : ""}");
    snd_print("\n[#{$@}]") if $@ and $VERBOSE;
  else
    STDERR.print("#{str}#{$! ? ": #{$!}" : ""}\n");
    STDERR.print("[#{$@}]\n") if $@ and $VERBOSE;
  end
  false;
end

#
# message([str="Message"])
#

def message(str = "Message")
  if($IN_SND)
    snd_print("\n#{str}");
  else
    print("#{str}\n");
  end
  false;
end

# die([str="Error"[, n=1]])
#
# If no error occures it works like snd_print() or print(), if an
# error occures it works like C's perror() function. It exits only in
# scipt mode with error number n.

def die(str = "Error", n = 1)
  warn(str);
  exit(n) unless $IN_SND;
end

# shell(cmd)
#
# Sends cmd to a shell (executes it as a shell command) and returns
# the result.

def shell(cmd)
  str = String.new;
  f = IO::popen(cmd);
  str << f.getc until f.eof?;
  f.close;
  str;
end

# get_func_name([n=1])
#
# returns function name string

def get_func_name(n = 1)
  caller(n)[0].scan(/^.*:in `(.*)'/)[0][0];
end

##
## Buffers Menu
##
## Usage in ~./snd-ruby.rb
## 
## $buffer_menu = add_to_main_menu("Buffers");
## 
## $open_hook = lambda { |file| open_buffer(file) }
## $close_hook = lambda { |snd| close_buffer(snd) }
##

# open_buffer(file)
#
# Adds a menu item that will select filename (use with $open_hook).

def open_buffer(file)
  add_to_menu($buffer_menu, file, lambda { || select_sound(find_sound(file)) });
  false;
end

# close_buffer(snd)
#
# Removes the menu item associated with snd (use with $close_hook).

def close_buffer(snd)
  remove_from_menu($buffer_menu, file_name(snd));
  false;
end

##
## Reopen Menu
##
## Usage in ~./snd-ruby.rb
## 
## $reopen_menu = add_to_main_menu("Reopen");
## 
## $open_hook = lambda { |file| check_reopen_menu(file) }
## $close_hook = lambda { |snd| add_to_reopen_menu(snd) }
##

$reopen_names = [];

# add_to_reopen_menu(snd)
#
# Adds snd to the Reopen menu (use with $close_hook).

def add_to_reopen_menu(snd)
  brief_name = short_file_name(snd);
  long_name = file_name(snd);
  reopen_max_length = 8;

  unless($reopen_names.member?(brief_name))
    add_to_menu($reopen_menu, brief_name,
		lambda { | |
		  remove_from_menu($reopen_menu, brief_name);
		  open_sound(long_name); 
		}, 0);
    $reopen_names << brief_name;
    if($reopen_names.length > reopen_max_length)
      goner = $reopen_names.shift;
      remove_from_menu($reopen_menu, goner);
    end
  end
  false;
end

# check_reopen_menu(file)
#
# Removes filename from the Reopen menu list (use with $open_hook).

def check_reopen_menu(file)
  just_file = lambda { |name|
    last_slash = -1;
    len = name.length;

    (0...len).each { |i| last_slash = i if name[i] == ?/; }
    name[last_slash + 1];
  }

  brief_name = just_file.call(file);
  
  if($reopen_names.member?(brief_name))
    $reopen_names = remove_if(lambda { |n|
				val = (n == brief_name);
				remove_from_menu($reopen_menu, brief_name) if val;
				val;
			      }, $reopen_names);
  end
  false;
end

##
## FM
##

#
# *clm-like variables used by fm_violin(), jc_reverb(), and
# with_sound()
#

$rbm_output = false;
$rbm_file_name = "test.snd";
$rbm_srate = 22050;
$rbm_channels = 1;
$rbm_header_type = Mus_next;
$rbm_data_format = Mus_bshort;
$rbm_comment = "created by #{ENV["USER"]}";
$rbm_statistics = false;
$rbm_play = false;
$rbm_player = "sndplay";
$rbm_reverb = false;
$rbm_reverb_file_name = "reverb.snd";
$rbm_reverb_channels = 1;

#
# fm_violin([start=0.0[, dur=1.0[, freq=440.0[, amp=0.3[, *args]]]]])
#

def fm_violin(start = 0.0, dur = 1.0, freq = 440.0, amp = 0.3, *args)
  include Math;			# PI

  func_name = "\n" + get_func_name() + "()";
  usage = "fm_violin([start=0.0[, dur=1.0[, freq=440.0[, amp=0.3[, *args]]]]])

fm_violin(:help)

	[:fm_index, 1.0]
	[:amp_env, [0, 0, 25, 1, 75, 1, 100, 0]]
	[:periodic_vibrato_rate, 5.0]
	[:random_vibrato_rate, 16.0]
	[:periodic_vibrato_amp, 0.0025]
	[:random_vibrato_amp, 0.005]
	[:noise_amount, 0.0]
	[:noise_freq, 1000.0]
	[:ind_noise_freq, 10.0]
	[:ind_noise_amount, 0.0]
	[:amp_noise_freq, 20.0]
	[:amp_noise_amount, 0.0]
	[:gliss_env, [0, 0,  100, 0]]
	[:gliss_amount, 0.0]
	[:fm1_env, [0, 1, 25, 0.4, 75, 0.6, 100, 0]]
	[:fm2_env, [0, 1, 25, 0.4, 75, 0.6, 100, 0]]
	[:fm3_env, [0, 1, 25, 0.4, 75, 0.6, 100, 0]]
	[:fm1_rat, 1.0]
	[:fm2_rat, 3.0]
	[:fm3_rat, 4.0]
	[:fm1_index, false]
	[:fm2_index, false]
	[:fm3_index, false]
	[:base, 1.0]
	[:reverb_amount, 0.01]
	[:index_type, :violin]
	[:degree, false]
	[:distance, 1.0]
	[:degrees, false]
	[:help]

   Ruby: fm_violin(0, 1, 440, .1, [[:fm_index, 2.0]])
  Guile: (fm-violin 0 1 440 .1 :fm-index 2.0)

Example: with_sound { fm_violin(0, 1, 440, .1, [[:fm_index, 2.0]]) }\n";

  unless(start == :help or args.member?(:help) or args.assoc(:help))
    fm_index = (args.assoc(:fm_index)[1] rescue 1.0);
    amp_env = (args.assoc(:amp_env)[1] rescue [0, 0, 25, 1, 75, 1, 100, 0]);
    periodic_vibrato_rate = (args.assoc(:periodic_vibrato_rate)[1] rescue 5.0);
    random_vibrato_rate = (args.assoc(:random_vibrato_rate)[1] rescue 16.0);
    periodic_vibrato_amp = (args.assoc(:periodic_vibrato_amp)[1] rescue 0.0025);
    random_vibrato_amp = (args.assoc(:random_vibrato_amp)[1] rescue 0.005);
    noise_amount = (args.assoc(:noise_amount)[1] rescue 0.0);
    noise_freq = (args.assoc(:noise_freq)[1] rescue 1000.0);
    ind_noise_freq = (args.assoc(:ind_noise_freq)[1] rescue 10.0);
    ind_noise_amount = (args.assoc(:ind_noise_amount)[1] rescue 0.0);
    amp_noise_freq = (args.assoc(:amp_noise_freq)[1] rescue 20.0);
    amp_noise_amount = (args.assoc(:amp_noise_amount)[1] rescue 0.0);
    gliss_env = (args.assoc(:gliss_env)[1] rescue [0, 0,  100, 0]);
    gliss_amount = (args.assoc(:gliss_amount)[1] rescue 0.0);
    fm1_env = (args.assoc(:fm1_env)[1] rescue [0, 1, 25, 0.4, 75, 0.6, 100, 0]);
    fm2_env = (args.assoc(:fm2_env)[1] rescue [0, 1, 25, 0.4, 75, 0.6, 100, 0]);
    fm3_env = (args.assoc(:fm3_env)[1] rescue [0, 1, 25, 0.4, 75, 0.6, 100, 0]);
    fm1_rat = (args.assoc(:fm1_rat)[1] rescue 1.0);
    fm2_rat = (args.assoc(:fm2_rat)[1] rescue 3.0);
    fm3_rat = (args.assoc(:fm3_rat)[1] rescue 4.0);
    fm1_index = (args.assoc(:fm1_index)[1] rescue false);
    fm2_index = (args.assoc(:fm2_index)[1] rescue false);
    fm3_index = (args.assoc(:fm3_index)[1] rescue false);
    base = (args.assoc(:base)[1] rescue 1.0);
    reverb_amount = (args.assoc(:reverb_amount)[1] rescue 0.01);
    index_type = (args.assoc(:index_type)[1] rescue :violin);
    degree = (args.assoc(:degree)[1] rescue false);
    distance = (args.assoc(:distance)[1] rescue 1.0);
    degrees = (args.assoc(:degrees)[1] rescue false);

    srate = (mus_srate() rescue $rbm_srate);
    chans = (mus_channels($rbm_output) rescue $rbm_channels);
    beg = (srate * start).round;
    len = beg + (srate * dur).round;
    frq_scl = hz2radians(freq);
    modulate = fm_index.nonzero?;
    maxdev = frq_scl * fm_index;
    vln = (not (index_type == :cello))
    logfreq = log(freq);
    sqrtfreq = sqrt(freq);
    index1 = (fm1_index or [PI, maxdev * (vln ? 5.0 : 7.5) / logfreq].min);
    index2 = (fm2_index or [PI, maxdev * 3.0 * 
		(vln ? ((8.5 - logfreq) / (3.0 + freq * 0.001)) : (15.0 / sqrtfreq))].min);
    index3 = (fm3_index or [PI, maxdev * (vln ? 4.0 : 8.0) / sqrtfreq].min);
    easy_case = (noise_amount.zero? and
		 (fm1_env == fm2_env) and 
		 (fm1_env == fm3_env) and 
		 (fm1_rat - fm1_rat.floor).zero? and 
		 (fm2_rat - fm2_rat.floor).zero? and 
		 (fm3_rat - fm3_rat.floor).zero?);
    coeffs = (easy_case and modulate and 
	      partials2polynomial([fm1_rat, index1, 
				    (fm2_rat / fm1_rat).floor, index2,
				    (fm3_rat / fm1_rat).floor, index3]));
    norm = ((easy_case and modulate and 1.0) or index1);
    carrier = make_oscil(freq);
    fmosc1 = (modulate and make_oscil(fm1_rat * freq));
    fmosc2 = (modulate and (easy_case or make_oscil(fm2_rat * freq)));
    fmosc3 = (modulate and (easy_case or make_oscil(fm3_rat * freq)));
    ampf = make_env(amp_env, amp, dur, 0.0, base);
    indf1 = (modulate and make_env(fm1_env, norm, dur));
    indf2 = (modulate and (easy_case or make_env(fm2_env, index2, dur)));
    indf3 = (modulate and (easy_case or make_env(fm3_env, index3, dur)));
    frqf = make_env(gliss_env, gliss_amount * frq_scl, dur);
    pervib = make_triangle_wave(periodic_vibrato_rate, periodic_vibrato_amp *  frq_scl);
    ranvib = make_rand_interp(random_vibrato_rate, random_vibrato_amp * frq_scl);
    fm_noi = (noise_amount.nonzero? and make_rand(noise_freq, PI * noise_amount));
    ind_noi = ((ind_noise_amount.nonzero? and ind_noise_freq.nonzero?) and 
	       make_rand_interp(ind_noise_freq, ind_noise_amount));
    amp_noi = ((amp_noise_amount.nonzero? and amp_noise_freq.nonzero?) and
	       make_rand_interp(amp_noise_freq, amp_noise_amount));
    vib = 0.0;
    modulation = 0.0;
    loc = make_locsig((degree or degrees or kernel_rand(90.0)), 
		      distance, reverb_amount, $rbm_output, $rbm_reverb, chans);
    fuzz = 0.0;
    ind_fuzz = 1.0;
    amp_fuzz = 1.0;

    beg.upto(len) { |i|
      fuzz = rand(fm_noi) if noise_amount.nonzero?;
      vib = env(frqf) + triangle_wave(pervib) + rand_interp(ranvib);
      ind_fuzz = 1.0 + rand_interp(ind_noi) if ind_noi;
      amp_fuzz = 1.0 + rand_interp(amp_noi) if amp_noi;
      
      if(modulate)
	if(easy_case)
	  modulation = env(indf1) * polynomial(coeffs, oscil(fmosc1, vib));
	else
	  modulation = env(indf1) * oscil(fmosc1, fm1_rat * vib + fuzz) +
	    env(indf2) * oscil(fmosc2, fm2_rat * vib + fuzz) +
	    env(indf3) * oscil(fmosc3, fm3_rat * vib + fuzz);
	end
      end

      locsig(loc, i, env(ampf) * amp_fuzz * oscil(carrier, vib + ind_fuzz * modulation));
    }
  else
    message(usage);
  end
rescue
  die(usage + func_name);
end

#
# jc_reverb([args=[]])
#

def jc_reverb(args = [])
  func_name = "\n" + get_func_name() + "()";
  usage = "jc_reverb([args=[]])

jc_reverb(:help)

	[:decay, 1.0]
	[:low_pass, false]
	[:volume, 1.0]
	[:amp_env1, false]
	[:amp_env2, false]
	[:delay1, 0.013]
	[:delay2, 0.011]
	[:help]

The old Chowning reverberator (see examp.scm).

Usage: jc_reverb([[:decay, 2.0], [:volume, .1]])

       jc_rev = lambda { |args| jc_reverb(args) }
       with_sound([:reverb, jc_rev]) { fm_violin }\n";

  unless(args == :help or args.member?(:help) or args.assoc(:help))
    decay = (args.assoc(:decay)[1] rescue 1.0);
    low_pass = (args.assoc(:low_pass)[1] rescue false);
    vol = (args.assoc(:volume)[1] rescue 1.0);
    amp_env1 = (args.assoc(:amp_env1)[1] rescue false);
    amp_env2 = (args.assoc(:amp_env2)[1] rescue false);
    delay1 = (args.assoc(:delay1)[1] rescue 0.013);
    delay2 = (args.assoc(:delay2)[1] rescue 0.011);
    
    allpass1 = make_all_pass(-0.700, 0.700, 1051);
    allpass2 = make_all_pass(-0.700, 0.700, 337);
    allpass3 = make_all_pass(-0.700, 0.700, 113);
    comb1 = make_comb(0.742, 4799);
    comb2 = make_comb(0.733, 4999);
    comb3 = make_comb(0.715, 5399);
    comb4 = make_comb(0.697, 5801);
    srate = (mus_srate() rescue $rbm_srate);
    chans = (mus_channels($rbm_output) rescue $rbm_channels);
    outdel1 = make_delay((delay1 * srate).round);
    outdel2 = make_delay((delay2 * srate).round) if chans == 2;
    dur = decay + mus_sound_frames($rbm_file_name) / srate.to_f;
    len = (srate * dur).round;
    envA = (amp_env1 ? make_env(amp_env1, vol, dur) : false);
    envB = (amp_env2 ? make_env(amp_env2, vol, dur) : false);
    delA = (envA ? env(envA) : vol);
    delB = (envB ? env(envB) : vol);
    allpass_sum, all_sums = 0.0, 0.0;
    comb_sumA, comb_sum_1A, comb_sum_2A = 0.0, 0.0, 0.0;
    comb_sumB, comb_sum_1B, comb_sum_2B = 0.0, 0.0, 0.0;

    0.upto(len) { |i|
      ho = ina(i, $rbm_reverb);
      allpass_sum = all_pass(allpass3, all_pass(allpass2, all_pass(allpass1, ho)));
      comb_sum_2A = comb_sum_1A;
      comb_sum_1A = comb_sumA;
      comb_sumA = comb(comb1, allpass_sum) + comb(comb2, allpass_sum) +
	comb(comb3, allpass_sum) + comb(comb4, allpass_sum);
      
      if(low_pass)
	all_sums = 0.25 * (comb_sumA + comb_sum_2A) + 0.5 * comb_sum_1A;
      else
	all_sums = comb_sumA;
      end

      outa(i, delA * delay(outdel1, all_sums), $rbm_output);

      if($rbm_reverb_channels == 2)
	ho = inb(i, $rbm_reverb);
	allpass_sum = all_pass(allpass3, all_pass(allpass2, all_pass(allpass1, ho)));
	comb_sum_2B = comb_sum_1B;
	comb_sum_1B = comb_sumB;
	comb_sumB = comb(comb1, allpass_sum) + comb(comb2, allpass_sum) +
	  comb(comb3, allpass_sum) + comb(comb4, allpass_sum);
	
	if(low_pass)
	  all_sums = 0.25 * (comb_sumB + comb_sum_2B) + 0.5 * comb_sum_1B;
	else
	  all_sums = comb_sumB;
	end
      end
      
      outb(i, delB * delay(outdel2, all_sums), $rbm_output) if chans == 2;
    }

  else
    message(usage);
  end
rescue
  die(usage + func_name);
end

#
# with_sound(*args) { ... }
#

def with_sound(*args)
  func_name = "\n" + get_func_name() + "()";
  usage = "with_sound(*args) { ... }

with_sound(:help)

	[:output, $rbm_file_name]
	[:continue_old_file, false]
	[:channels, $rbm_channels]
	[:statistics, $rbm_statistics]
	[:play, $rbm_play]
	[:player, $rbm_player]
	[:srate, $rbm_srate]
	[:header_type, $rbm_header_type]
	[:data_format, $rbm_data_format]
	[:comment, $rbm_comment]
	[:reverb, false]
	[:revfile, $rbm_reverb_file_name]
	[:reverb_channels, $rbm_reverb_channels]
	[:reverb_args, []]
	[:help]

Usage: with_sound([:play, true], [:statistics, true]) { fm_violin }\n";

  unless(args.member?(:help) or args.assoc(:help))
    output = (args.assoc(:output)[1] rescue $rbm_file_name);
    continue_old_file = (args.assoc(:continue_old_file)[1] rescue false);
    channels = (args.assoc(:channels)[1] rescue $rbm_channels);
    statistics = (args.assoc(:statistics)[1] rescue $rbm_statistics);
    play = (args.assoc(:play)[1] rescue $rbm_play);
    player = (args.assoc(:player)[1] rescue $rbm_player);
    srate = (args.assoc(:srate)[1] rescue $rbm_srate);
    header_type = (args.assoc(:header_type)[1] rescue $rbm_header_type);
    data_format = (args.assoc(:data_format)[1] rescue $rbm_data_format);
    comment = (args.assoc(:comment)[1] rescue $rbm_comment);
    reverb = (args.assoc(:reverb)[1] rescue false);
    revfile = (args.assoc(:revfile)[1] rescue $rbm_reverb_file_name);
    reverb_channels = (args.assoc(:reverb_channels)[1] rescue $rbm_reverb_channels);
    reverb_args = (args.assoc(:reverb_args)[1] rescue []);

    $rbm_file_name = output;
    $rbm_channels = channels;
    $rbm_srate = srate;
    $rbm_reverb_file_name = revfile;
    $rbm_reverb_channels = reverb_channels;

    (close_sound(find_sound(output)) rescue false) if $IN_SND;

    unless(continue_old_file)
      old_srate = mus_srate();
      mus_set_srate(srate);
      $rbm_output, $rbm_reverb = false, false;
      File::unlink(output) if File::exist?(output);
      $rbm_output = make_sample2file(output, channels, data_format, header_type,
				     comment) rescue  die(usage + "make_sample2file(#{output})");
      
      if(reverb)
	File::unlink(revfile) if File::exist?(revfile);
	$rbm_reverb = make_sample2file(revfile, reverb_channels, data_format, header_type,
				       "reverb") rescue die(usage +
							    "make_sample2file(#{revfile})");
      end
    else
      $rbm_output = continue_sample2file(output) rescue die(usage + 
							    "continue_sample2file{#{output}}");
      if(reverb)
	$rbm_reverb = continue_sample2file(revfile) rescue die(usage + 
							       "continue_sample2file(#{revfile})");
      end
    end

    atime = Time.new if statistics;
    yield() rescue die(usage + "yield()");

    if(reverb)
      mus_close($rbm_reverb);
      $rbm_reverb = make_file2sample(revfile) rescue die(usage + "make_file2sample(#{revfile})");
      reverb.call(reverb_args) rescue die(usage + "reverb.call()");
    end
    
    unless(continue_old_file)
      if(reverb)
	mus_close($rbm_reverb);
	$rbm_reverb = false;
      end
      mus_close($rbm_output);
      $rbm_output = false;
      mus_set_srate(old_srate);
    end

    if(statistics)
      rtime = Time.new - atime;
      samps = mus_sound_samples(output);
      max_amp = mus_sound_maxamp(output);
      srate = srate.to_f;

      message(format("    Sound File: %s", output));
      message(format("      Duration: %.4f", (samps / srate / channels)));
      message(format("  Compute time: %.3f, Compute ratio: %.2f", rtime, 
		     rtime * (srate / samps) * channels));
      message(format("  OutA max amp: %.3f (near %.3f secs)", 
		     max_amp[1], max_amp[0] / srate));
      message(format("  OutB max amp: %.3f (near %.3f secs)",
		     max_amp[3], max_amp[2] / srate)) if channels == 2;
      if(reverb)
	max_amp = mus_sound_maxamp(revfile);
	
	message(format("  RevA max amp: %.3f (near %.3f secs)", 
		       max_amp[1], max_amp[0] / srate));
	message(format("  RevB max amp: %.3f (near %.3f secs)",
		       max_amp[3], max_amp[2] / srate)) if reverb_channels == 2;
      end
    end

    snd = open_sound(output);
    ($IN_SND ? play_and_wait(snd) : system("#{player} #{output}")) if play;
    output;
  else
    message(usage);
  end
rescue
  die(usage + func_name);
end

=begin
# Examples:

with_sound { fm_violin }

jc_rev = lambda { |args| jc_reverb(args) }

with_sound([:channels, 2],
	   [:play, true],
	   [:statistics, true],
	   [:reverb_channels, 2],
	   [:reverb, jc_rev],
	   [:reverb_args, [[:decay, .8], [:volume, .3]]],
	   [:reverb_channels, 1]) { fm_violin(0, 1, 440, .3) }

=end

# fm_play(func[, outfile="test.snd"[, play_f=true]])
#
# Usage: fm_play(lambda { || fm_bell(0, 1, 440, .1) })

def fm_play(func, outfile = "test.snd", play_f = true)
  snd = new_sound(outfile, Mus_next, Mus_bshort, 22050, 1, "created by fm_play()");
  atime = Time.new;
  func.call();
  btime = Time.new;
  save_sound(snd);
  message("time: #{btime - atime}");
  play() if play_f;
end

##
## Michael McNabb's FM bell (see bell.scm)
##

# fm_bell(start, dur, freq, amp
#         [, amp_env=[0, 0, .1, 1, 10, .6, 25, .3, 50, .15, 90, .1, 100, 0]
#         [, index_env=[0, 1, 2, 1.1, 25, .75, 75, .5, 100, .2]
#         [, index=1.0]]])
#
# mixes in one fm bell note

def fm_bell(start, dur, freq, amp, 
	    amp_env = [0, 0, .1, 1, 10, .6, 25, .3, 50, .15, 90, .1, 100, 0], 
	    index_env = [0, 1, 2, 1.1, 25, .75, 75, .5, 100, .2], 
	    index = 1.0)

  srate = (srate() rescue 22050);
  beg = (srate * start).round;
  len = (srate * dur).round;
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
			  .15 * oscil(car2, fmenv *
				      (fmind2 * oscil(mod2) + fmind3 * oscil(mod3))) +
			  .15 * oscil(car3, fmenv * fmind4 * oscil(mod4)));
	   });

  mix_vct(out_data, beg, false, 0, false);
end

=begin
fm_play(lambda { | |
	  fbell = [0, 1, 2, 1.1000, 25, .7500, 75, .5000, 100, .2000];
	  abell = [0, 0, .1000, 1, 10, .6000, 25, .3000, 50, .1500, 90, .1000, 100, 0];
	  fm_bell(0.0, 1.0, 220.0, .5, abell, fbell, 1.0);
	}, "bell.snd");
=end

#
# n_rev([args=[]])
#

def n_rev(args = [])
  func_name = "\n" + get_func_name() + "()";
  usage = "n_rev([args=[]])

n_rev(:help)

	[:amount, 0.1]
	[:filter, 0.5]
	[:feedback, 1.09]
	[:help]

Reverb from Michael McNabb's Nrev (see new-effects.scm).

Usage: n_rev([[:amount, .2], [:filter, .8]])\n";

  unless(args == :help or args.member?(:help) or args.assoc(:help))
    amount = (args.assoc(:amount)[1] rescue 0.1);
    filter = (args.assoc(:filter)[1] rescue 0.5);
    feedback = (args.assoc(:feedback)[1] rescue 1.09);

    set_reverb_control?(true);
    set_reverb_control_scale(amount);
    set_reverb_control_lowpass(filter);
    set_reverb_control_feedback(feedback);
    apply_controls(selected_sound(), 0);
    restore_controls();
  else
    message(usage);
  end
rescue
  die(usage + func_name);
end

#
# hello_dentist([freq=40.0[, amp=0.1]])
# 

def hello_dentist(freq = 40.0, amp = 0.1)
  func_name = "\n" + get_func_name() + "()";
  usage = "hello_dentist([freq=40.0[, amp=0.1]])

hello_dentist(:help)

Varies the sampling rate randomly, making a voice sound quavery (see
examp.scm).

Usage: hello_dentist(40.0, 0.1)\n";

  unless(freq == :help)
    rn = make_rand_interp(freq, amp);
    i, j = 0, 0;
    len = frames();
    in_data = samples2vct(0, len);
    out_len = (len * (1.0 + 2 * amp)).round;
    out_data = make_vct(out_len);
    # make_src(input, srate=1.0, width=5)
    rd = make_src(lambda { |dir|
		    val = ((i >= 0) and (i < len)) ? vct_ref(in_data, i) : 0.0;
		    i = i + dir;
		    val;
		  });

    vct2channel(vct_map!(out_data, lambda { || src(rd, rand_interp(rn)) }));
  else
    message(usage);
  end
rescue
  die(usage + func_name);
end

#
# ring_mod([freq=10.0[, gliss_env=[0, 0, 1, hz2radians(100)]]])
#

def ring_mod(freq = 10, gliss_env = [0, 0, 1, hz2radians(100)])
  func_name = "\n" + get_func_name() + "()";
  usage = "ring_mod([freq=10.0[, gliss_env=[0, 0, 1, hz2radians(100)]]])

ring_mod(:help)

Returns a time-varying ring-modulation filter (see examp.scm).

Usage: map_chan(ring_mod(10, [0, 0, 1, hz2radians(100)]))\n";

  unless(freq == :help)
    os = make_oscil(freq);
    len = frames();
    srate = (srate() rescue $rbm_srate);
    dur = (len / srate).round;
    genv = make_env(gliss_env, :end, len);
    
    lambda { |i| oscil(os, env(genv)) * i }
  else
    message(usage);
  end
rescue
  die(usage + func_name);
end

#
# am([freq=440.0])
#

def am(freq = 440.0)
  func_name = "\n" + get_func_name() + "()";
  usage = "am([freq=440.0])

am(:help)

Returns an amplitude-modulator (see examp.scm).

Usage: map_chan(am(440.0))\n";

  unless(freq == :help)
    os = make_oscil(freq);
    lambda { |i| amplitude_modulate(1.0, i, oscil(os)) }
  else
    message(usage);
  end
rescue
  die(usage + func_name);
end

#
# vibro([speed=20[, depth=0.5]])
#

def vibro(speed = 20, depth = 0.5)
  func_name = "\n" + get_func_name() + "()";
  usage = "vibro([speed=20[, depth=0.5]])

vibro(:help)

This is taken from sox (vibro.c) (see examp.scm).

Usage: map_chan(vibro(20, .5))\n";

  unless(speed == :help)
    sine = make_oscil(speed);
    scl = 0.5 * depth;
    offset = 1.0 - scl;

    lambda { |i| i * (offset + scl * oscil(sine)) }
  else
    message(usage);
  end
rescue
  die(usage + func_name);
end

#
# fp([sr=1.0[, osamp=0.3[, osfreq=20]]])
#

def fp(sr = 1.0, osamp = 0.3, osfreq = 20)
  func_name = "\n" + get_func_name() + "()";
  usage = "fp([sr=1.0[, osamp=0.3[, osfreq=20]]])

fp(:help)

Varies the sampling rate via an oscil (see examp.scm).

Usage: fp(1.0, 0.3, 20)\n";

  unless(sr == :help)
    os = make_oscil(osfreq);
    s = make_src(:srate, sr);
    len = frames();
    sf = make_sample_reader();
    out_data = make_vct(len);

    vct_map!(out_data, lambda { | |
	       src(s, osamp * oscil(os), lambda { |dir|
		     dir > 0 ? next_sample(sf) : previous_sample(sf);
		   })
	     });
    free_sample_reader(sf);
    vct2samples(0, len, out_data);
  else
    message(usage);
  end
rescue
  die(usage + func_name);
end

#
# compand([h=false])
#

def compand(h = false)
  func_name = "\n" + get_func_name() + "()";
  usage = "compand()

compand(:help)

Since there's no state in this function, it can be used without change
in any of the mapping functions (unlike echo, for example) (see
examp.scm).

Usage: map_chan(compand())\n";

  unless(h == :help)
    tbl = vct(-1.000, -0.960, -0.900, -0.820, -0.720, -0.600, -0.450, -0.250, 
	      0.000, 0.250, 0.450, 0.600, 0.720, 0.820, 0.900, 0.960, 1.000);
    lambda { |i| 
      index = 8.0 + 8.0 * i;
      array_interp(tbl, index, 17);
    }

  else
    message(usage);
  end
rescue
  die(usage + func_name);
end

## examp.rb ends here
