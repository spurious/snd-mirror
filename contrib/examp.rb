## examp.rb -- Guile -> Ruby translation

## Translator/Author: Michael Scholz <scholz-micha@gmx.de>
## Last: Mon Oct 07 01:23:53 CEST 2002
## Version: $Revision: 1.1 $

##
## Utilities
##
## car(v), cadr(v), caddr(v), cdr(v)
## snd_warn(str)
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
## 


$VERBOSE = true;

##
## Utilities
##

def car(v) v[0]; end

def cadr(v) v[1]; end

def caddr(v) v[2]; end

def cdr(v) v.shift; v; end

# snd_warn([str="Warning"])
#
# If no error occures it works like snd_print(),
# if an error occures it works like C's perror() function.

def snd_warn(str = "Warning")
  snd_print("\n#{str}#{$! ? ": #{$!}" : ""}");
  snd_print("\n[#{$@}]") if $@ and $VERBOSE;
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
## fm_violin(start, dur, freq, amp, args)
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
  snd_warn("time: #{btime - atime}");
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
  begin
    srate = srate();
  rescue
    srate = 22050;
  end
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
  mix_vct(out_data, beg);
end

=begin
fm_play(lambda { | |
	  fbell = [0, 1, 2, 1.1000, 25, .7500, 75, .5000, 100, .2000];
	  abell = [0, 0, .1000, 1, 10, .6000, 25, .3000, 50, .1500, 90, .1000, 100, 0];
	  fm_bell(0.0, 1.0, 220.0, .5, abell, fbell, 1.0);
	}, "bell.snd");
=end

##
## fm_violin
##
## CLM version is v.ins, C version is in sndlib.html a version
## treating the entire violin as a generator is in fmv.scm.
## 

# fm_violin([start=0.0[, dur=1.0[, freq=440.0[, amp=0.1[, args={}]]]]])
#
# Values for Hash args:
# 
#        :fm_index => 1.0
#        :amp_env => [0, 0, 25, 1, 75, 1, 100, 0]
#        :periodic_vibrato_rate => 5.0
#        :random_vibrato_rate => 16.0
#        :periodic_vibrato_amp => 0.0025
#        :random_vibrato_amp => 0.005
#        :noise_amount => 0.0
#        :noise_freq => 1000.0
#        :ind_noise_freq => 10.0
#        :ind_noise_amount => 0.0
#        :amp_noise_freq => 20.0
#        :amp_noise_amount => 0.0
#        :gliss_env => [0, 0,  100, 0]
#        :gliss_amount => 0.0
#        :fm1_env => [0, 1, 25, .4, 75, .6, 100, 0]
#        :fm2_env => [0, 1, 25, .4, 75, .6, 100, 0]
#        :fm3_env => [0, 1, 25, .4, 75, .6, 100, 0]
#        :fm1_rat => 1.0
#        :fm2_rat => 3.0
#        :fm3_rat => 4.0
#        :fm1_index => false
#        :fm2_index => false
#        :fm3_index => false
#        :base => 1.0
#        :reverb_amount => .01
#        :degree => false
#        :distance => 1.0
#        :degrees => false
#
#  Ruby: fm_violin(0, 1, 440, .1, {:fm_index => 2.0})
# Guile: (fm-violin 0 1 440 .1 :fm-index 2.0)

def fm_violin(start = 0.0, dur = 1.0, freq = 440.0, amp = 0.1, args = {})
  include Math;			# PI

  h = { :fm_index => 1.0,
    :amp_env => [0, 0, 25, 1, 75, 1, 100, 0],
    :periodic_vibrato_rate => 5.0,
    :random_vibrato_rate => 16.0,
    :periodic_vibrato_amp => 0.0025,
    :random_vibrato_amp => 0.005,
    :noise_amount => 0.0,
    :noise_freq => 1000.0,
    :ind_noise_freq => 10.0,
    :ind_noise_amount => 0.0,
    :amp_noise_freq => 20.0,
    :amp_noise_amount => 0.0,
    :gliss_env => [0, 0,  100, 0],
    :gliss_amount => 0.0,
    :fm1_env => [0, 1, 25, .4, 75, .6, 100, 0],
    :fm2_env => [0, 1, 25, .4, 75, .6, 100, 0],
    :fm3_env => [0, 1, 25, .4, 75, .6, 100, 0],
    :fm1_rat => 1.0,
    :fm2_rat => 3.0,
    :fm3_rat => 4.0,
    :fm1_index => false,
    :fm2_index => false,
    :fm3_index => false,
    :base => 1.0,
    :reverb_amount => .01,
    :degree => false,
    :distance => 1.0,
    :degrees => false };

  begin
    h.update(args) unless args.empty?;
  rescue
    snd_warn("Usage: fm_violin(0, 1, 440, .1, {:fm_index => 2.0})");
  end

  begin
    srate = srate();
  rescue
    srate = 22050;		# if no open snd file
  end

  begin
    chans = channels();
  rescue
    chans = 1;			# if no open snd file
  end

  beg = (srate * start).round;
  len = (srate * dur).round;
  frq_scl = hz2radians(freq);
  modulate = (not h[:fm_index].zero?);
  maxdev = frq_scl * h[:fm_index];
  logfreq = log(freq);
  sqrtfreq = sqrt(freq);
  index1 = (h[:fm1_index] or [PI, maxdev * 5.0 / logfreq].min);
  index2 = (h[:fm2_index] or [PI, maxdev * 3.0 * (8.5 - logfreq) / (3.0 + freq * .001)].min);
  index3 = (h[:fm3_index] or [PI, maxdev * 4.0 / sqrtfreq].min);
  easy_case = (h[:noise_amount].zero? and
	       ((not h[:fm2_env]) or (h[:fm1_env] == h[:fm2_env])) and 
	       ((not h[:fm3_env]) or (h[:fm1_env] == h[:fm3_env])) and 
	       (h[:fm1_rat] == h[:fm1_rat].floor) and 
	       (h[:fm2_rat] == h[:fm2_rat].floor) and 
	       (h[:fm3_rat] == h[:fm3_rat].floor));
  coeffs = (easy_case and modulate and 
	    partials2polynomial([h[:fm1_rat], index1, (h[:fm2_rat] / h[:fm1_rat]).floor, index2,
				  (h[:fm3_rat] / h[:fm1_rat]).floor, index3]));
  norm = ((easy_case and modulate and 1.0) or index1);
  carrier = make_oscil(freq);
  fmosc1 = (modulate and make_oscil(h[:fm1_rat] * freq));
  fmosc2 = (modulate and (easy_case or make_oscil(h[:fm2_rat] * freq)));
  fmosc3 = (modulate and (easy_case or make_oscil(h[:fm3_rat] * freq)));
  ampf = make_env(h[:amp_env], amp, dur, 0.0, h[:base]);
  indf1 = (modulate and make_env(h[:fm1_env], norm, dur));
  indf2 = (modulate and (easy_case or make_env(h[:fm2_env], index2, dur)));
  indf3 = (modulate and (easy_case or make_env(h[:fm3_env], index3, dur)));
  frqf = make_env(h[:gliss_env], h[:gliss_amount] * frq_scl, dur);
  pervib = make_triangle_wave(h[:periodic_vibrato_rate], h[:periodic_vibrato_amp] *  frq_scl);
  ranvib = make_rand_interp(h[:random_vibrato_rate], h[:random_vibrato_amp] * frq_scl);
  fm_noi = ((not h[:noise_amount].zero?) and make_rand(h[:noise_freq], PI * h[:noise_amount]));
  ind_noi = (((not h[:ind_noise_amount].zero?) and (not h[:ind_noise_freq].zero?)) and 
	     make_rand_interp(h[:ind_noise_freq], h[:ind_noise_amount]));
  amp_noi = (((not h[:amp_noise_amount].zero?) and (not h[:amp_noise_freq].zero?)) and
	     make_rand_interp(h[:amp_noise_freq], h[:amp_noise_amount]));
  vib = 0.0;
  modulation = 0.0;
  # make_locsig(degree=0.0, distance=1.0, reverb=0.0, output, revout, chans=1, type=Mus_linear)
  loc = make_locsig((h[:degree] or h[:degrees] or 17), 
		    h[:distance], h[:reverb_amount], false, false, chans);
  fuzz = 0.0;
  ind_fuzz = 1.0;
  amp_fuzz = 1.0;
  out_data = make_vct(len);

  vct_map!(out_data,
	   lambda { | |
	     fuzz = rand(fm_noi) unless h[:noise_amount].zero?;
	     vib = env(frqf) + triangle_wave(pervib) + rand_interp(ranvib);
	     ind_fuzz = 1.0 + rand_interp(ind_noi) if ind_noi;
	     amp_fuzz = 1.0 + rand_interp(amp_noi) if amp_noi;
	     if(modulate)
	       if(easy_case)
		 modulation = env(indf1) * polynomial(coeffs, oscil(fmosc1, vib));
	       else
		 modulation = env(indf1) * oscil(fmosc1, h[:fm1_rat] * vib + fuzz) +
		   env(indf2) * oscil(fmosc2, h[:fm2_rat] * vib + fuzz) +
		   env(indf3) * oscil(fmosc3, h[:fm3_rat] * vib + fuzz);
	       end
	     end
	     env(ampf) * amp_fuzz * oscil(carrier, vib + ind_fuzz * modulation);
	   });

  if(chans == 2)
    mix_vct(vct_scale!(vct_copy(out_data), locsig_ref(loc, 1)), beg, false, 1, false);
    mix_vct(vct_scale!(out_data, locsig_ref(loc, 0)), beg, false, 0, false);
  else
    mix_vct(out_data, beg, false, 0, false);
  end
end

=begin
fm_play(lambda { | |
	  fm_violin();
	  fm_violin(.9, 1, 660, .1, 
		    { :fm_index => 2.0, 
		      :amp_env => [0, 0, 25, .2, 50, .4, 75, 1, 100, 0]});
	}, "v.snd");

lambda { || fm_play(lambda { | |
		      fm_violin(0, 5, 261.62); # C
		      fm_violin(1, 4, 440); # A
		      fm_violin(2, 3, 392); # G
		      fm_violin(3, 2, 329.63); # E
		    }, "cage.snd")
}.call
=end

## examp.rb ends here
