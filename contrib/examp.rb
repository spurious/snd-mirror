## examp.rb -- Guile -> Ruby translation

## Translator/Author: Michael Scholz <scholz-micha@gmx.de>
## Last: Sun Oct 13 05:21:49 CEST 2002
## Version: $Revision: 1.2 $

##
## Utilities
##
## car(v), cadr(v), caddr(v), cdr(v)
## warn(str), die(str), message(str)
## shell(cmd)
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
## fm_play(func, outfile, play_f)
## fm_bell(start, dur, freq, amp, amp_env, index_env, index)
## fm_violin(start, dur, freq, amp, args)
## jc_reverb(args)
## n_rev(args)
## with_sound(args) {|| ...}
## 


$VERBOSE = true;		# Ruby's -v or -w option
$IN_SND = true;			# script mode: $IN_SND = false

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
end

alias display warn

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
## fm_play(func, outfile, play_f)
## fm_bell(start, dur, freq, amp, amp_env, index_env, index)
##

# fm_play(func[, outfile="test.snd"[, play_f=true]])
#
# Usage: fm_play(lambda { || fm_bell(0, 1, 440, .1) })
#        fm_play(lambda { || fm_violin() }, "foo.snd")
#        fm_play(lambda { || fm_violin() }, "bar.snd", false)

def fm_play(func, outfile = "test.snd", play_f = true)
  snd = new_sound(outfile, Mus_next, Mus_bshort, 22050, 1, "created by fm_play()");
  atime = Time.new;
  func.call();
  btime = Time.new;
  save_sound(snd);
  display("time: #{btime - atime}");
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

##
## fm_violin(start, dur, freq, amp, args)
## jc_reverb(args)
## n_rev(args)
## with_sound(args) {|| ...}
## 

##
## variables used by fm_violin, jc_reverb and with_sound
##

$rbm_snd = false unless defined?($rbm_snd);
$rbm_file_name = "test.snd";
$rbm_srate = 22050;
$rbm_channels = 1;
$rbm_reverb_channels = 1;
$rbm_reverb = false;
$rbm_header_type = Mus_next;
$rbm_data_format = Mus_bshort;
$rbm_comment = "created by #{ENV["USER"]}";
$rbm_statistics = false;
$rbm_play = false;
$rbm_player = "sndplay";

#
# fm_violin([start=0.0[, dur=1.0[, freq=440.0[, amp=0.3[, *args]]]]])
#

def fm_violin(start = 0.0, dur = 1.0, freq = 440.0, amp = 0.3, *args)
  include Math;			# PI

  usage = "fm_violin([start=0.0[, dur=1.0[, freq=440.0[, amp=0.3[, *args]]]]])

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

  Ruby: fm_violin(0, 1, 440, .1, [[:fm_index, 2.0]])
 Guile: (fm-violin 0 1 440 .1 :fm-index 2.0)\n\n";

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

  srate = (srate() rescue $rbm_srate);
  chans = (channels() rescue $rbm_channels);
  beg = (srate * start).round;
  len = (srate * dur).round;
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
  # make_locsig(degree=0.0, distance=1.0, reverb=0.0, output, revout, chans=1, type=Mus_linear)
  # Ruby's rand() is shadowed by CLM's rand(), that's why mus_random().abs.
  loc = make_locsig((degree or degrees or mus_random(90.0).abs), 
		    distance, reverb_amount, false, false, chans);
  fuzz = 0.0;
  ind_fuzz = 1.0;
  amp_fuzz = 1.0;
  out_data = make_vct(len);

  vct_map!(out_data,
	   lambda { | |
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

	     env(ampf) * amp_fuzz * oscil(carrier, vib + ind_fuzz * modulation);
	   });

  if(chans == 2)
    mix_vct(vct_scale!(vct_copy(out_data), locsig_ref(loc, 1)), beg, $rbm_snd, 1, false);
    mix_vct(vct_scale!(out_data, locsig_ref(loc, 0)), beg, $rbm_snd, 0, false);
  else
    mix_vct(out_data, beg, $rbm_snd, 0, false);
  end
rescue
  die(usage + "fm_violin()");
end

#
# jc_reverb([args=[]])
#

def jc_reverb(args = [])
  usage = "jc_reverb([args=[]])

	[:decay, 1.0]
	[:low_pass, false]
	[:volume, 1.0]
	[:amp_env, false]
	[:delay1, 0.013]
	[:delay2, 0.011]

Is the old Chowning reverberator (see examp.scm)

Usage: jc_reverb([[:decay, 2.0], [:volume, .1]])

       jc_rev = lambda { |args| jc_reverb(args) }
       with_sound([:reverb, jc_rev]) { || fm_violin }\n\n";

  decay = (args.assoc(:decay)[1] rescue 1.0);
  low_pass = (args.assoc(:low_pass)[1] rescue false);
  vol = (args.assoc(:volume)[1] rescue 1.0);
  amp_env = (args.assoc(:amp_env)[1] rescue false);
  delay1 = (args.assoc(:delay1)[1] rescue 0.013);
  delay2 = (args.assoc(:delay2)[1] rescue 0.011);

  allpass1 = make_all_pass(-0.700, 0.700, 1051);
  allpass2 = make_all_pass(-0.700, 0.700, 337);
  allpass3 = make_all_pass(-0.700, 0.700, 113);
  comb1 = make_comb(0.742, 4799);
  comb2 = make_comb(0.733, 4999);
  comb3 = make_comb(0.715, 5399);
  comb4 = make_comb(0.697, 5801);
  outdel1 = make_delay((delay1 * $rbm_srate).round);
  outdel2 = make_delay((delay2 * $rbm_srate).round) if $rbm_reverb_channels == 2;
  dur = decay + frames() / $rbm_srate;
  envA = amp_env ? make_env(amp_env, vol, dur) : false;
  len = ($rbm_srate * dur).round;

  mapping = lambda { |odel, chan|
    allpass_sum = 0.0;
    comb_sum = 0.0;
    comb_sum_1 = 0.0;
    comb_sum_2 = 0.0;
    all_sums = 0.0;

    map_channel(lambda { |i|
		  allpass_sum = all_pass(allpass3, all_pass(allpass2, all_pass(allpass1, i)));
		  comb_sum_2 = comb_sum_1;
		  comb_sum_1 = comb_sum;
		  comb_sum = comb(comb1, allpass_sum) + comb(comb2, allpass_sum) +
		    comb(comb3, allpass_sum) + comb(comb4, allpass_sum);
		  
		  if(low_pass)
		    all_sums = 0.25 * (comb_sum + comb_sum_2) + 0.5 * comb_sum_1;
		  else
		    all_sums = comb_sum;
		  end
		  
		  i + (envA ? env(envA) * delay(odel, all_sums) : vol * delay(odel, all_sums));
		}, 0, len, $rbm_snd, chan, false);
  }

  mapping.call(outdel1, 0);
  
  if($rbm_channels == 2)
    mapping.call(outdel1, 1) if $rbm_reverb_channels == 1;
    mapping.call(outdel2, 1) if $rbm_reverb_channels == 2;
  end
rescue
  die(usage + "jc_reverb()");
end

#
# n_rev([args=[]])
#

def n_rev(args = [])
  usage = "n_rev([args=[]])

	[:amount, 0.1]
	[:filter, 0.5]
	[:feedback, 1.09]

Reverb from Michael McNabb's Nrev (see new-effects.scm)

Usage: nrev = lambda { |args| nrev(args) }
       with_sound([:reverb, nrev], 
	          [:reverb_args, [[:amount, .2], [:filter, .8]]]) { || fm_violin }\n\n";

  amount = (args.assoc(:amount)[1] rescue 0.1);
  filter = (args.assoc(:filter)[1] rescue 0.5);
  feedback = (args.assoc(:feedback)[1] rescue 1.09);

  set_reverb_control?(true);
  set_reverb_control_scale(amount);
  set_reverb_control_lowpass(filter);
  set_reverb_control_feedback(feedback);
  apply_controls(selected_sound(), 0);
  restore_controls();
rescue
  die(usage + "n_rev()");
end

#
# with_sound(*args) { || ... }
#

def with_sound(*args)
  usage = "with_sound(*args) { || ... }

	[:output, $rbm_file_name]
	[:channels, $rbm_channels]
	[:statistics, $rbm_statistics]
	[:play, $rbm_play]
	[:player, $rbm_player]
	[:srate, $rbm_srate]
	[:header_type, $rbm_header_type]
	[:data_format, $rbm_data_format]
	[:comment, $rbm_comment]
	[:reverb, $rbm_reverb]
	[:reverb_channels, $rbm_reverb_channels]
	[:reverb_args, []]

Usage: with_sound([:play, true], [:statistics, true]) { || fm_violin }\n\n";

  output = (args.assoc(:output)[1] rescue $rbm_file_name);
  channels = (args.assoc(:channels)[1] rescue $rbm_channels);
  statistics = (args.assoc(:statistics)[1] rescue $rbm_statistics);
  play = (args.assoc(:play)[1] rescue $rbm_play);
  player = (args.assoc(:player)[1] rescue $rbm_player);
  srate = (args.assoc(:srate)[1] rescue $rbm_srate);
  header_type = (args.assoc(:header_type)[1] rescue $rbm_header_type);
  data_format = (args.assoc(:data_format)[1] rescue $rbm_data_format);
  comment = (args.assoc(:comment)[1] rescue $rbm_comment);
  reverb = (args.assoc(:reverb)[1] rescue $rbm_reverb);
  reverb_channels = (args.assoc(:reverb_channels)[1] rescue $rbm_reverb_channels);
  reverb_args = (args.assoc(:reverb_args)[1] rescue []);

  $rbm_channels = channels;
  $rbm_srate = srate;
  $rbm_reverb_channels = reverb_channels;

  close_sound($rbm_snd) if sound?($rbm_snd) rescue die(usage + "close_sound()");
  $rbm_snd = new_sound(output, header_type, data_format,
		       srate, channels, comment) rescue  die(usage + "new_sound()");

  atime = Time.new if statistics;
  # calls block of with_sound() { || ... }
  yield() rescue die(usage + "yield()");
  reverb.call(reverb_args) if reverb rescue die(usage + "reverb.call()");
  
  if(statistics)
    rtime = Time.new - atime;
    samps = samples().length;

    display(format("    Sound File: %s", output));
    display(format("      Duration: %.4f", samps / $rbm_srate));
    display(format("  Compute time: %.3f, Compute ratio: %.2f", 
		   rtime, rtime * $rbm_srate / samps));
    display(format("  OutA max amp: %.3f", maxamp($rbm_snd, 0)));
    display(format("  OutB max amp: %.3f", maxamp($rbm_snd, 1))) if channels == 2;
    display(format("        Reverb: %d channel%s%s", 
		   reverb_channels, 
		   reverb_channels == 2 ? "s" : "",
		   reverb_args.empty? ? "" : ", #{reverb_args.join(" ")}")) if reverb;
  end

  if($IN_SND and play)
    play(0, $rbm_snd);
  else
    save_sound($rbm_snd);
    system("#{player} #{output}") if play;
  end
rescue
  die(usage + "with_sound()");
end

=begin
# Examples:

with_sound { fm_violin }

with_sound([:play, true], [:statistics, true]) { | |
  fbell = [0, 1, 2, 1.1000, 25, .7500, 75, .5000, 100, .2000];
  abell = [0, 0, .1000, 1, 10, .6000, 25, .3000, 50, .1500, 90, .1000, 100, 0];
  fm_bell(0.0, 1.0, 220.0, .5, abell, fbell, 1.0);
}

jc_rev = lambda { |args| jc_reverb(args) }

with_sound([:channels, 2],
	   [:play, true],
	   [:statistics, true],
	   [:reverb_channels, 2],
	   [:reverb, jc_rev],
	   [:reverb_args, [[:decay, .8], [:volume, .3]]],
	   [:reverb_channels, 1]) { || fm_violin(0, 1, 440, .3) }

nrev = lambda { |args| n_rev(args) }

with_sound([:channels, 2],
	   [:play, true],
	   [:statistics, true],
	   [:reverb_channels, 2],
	   [:reverb, nrev],
	   [:reverb_args, [[:filter, .8], [ :amount, .3]]],
	   [:reverb_channels, 1]) { || fm_violin(0, 1, 440, .3) }
=end

## examp.rb ends here
