# freeverb.rb -- CLM -> Snd/Ruby translation of freeverb.ins

# Translator/Author: Michael Scholz <scholz-micha@gmx.de>
# Last: Fri Apr 25 04:28:10 CEST 2003
# Version: $Revision: 1.8 $

# Original notes of Fernando Lopez-Lezcano

# ;; Freeverb - Free, studio-quality reverb SOURCE CODE in the public domain
# ;;
# ;; Written by Jezar at Dreampoint, June 2000
# ;; http://www.dreampoint.co.uk
# ;;
# ;; Translated into clm-2 by Fernando Lopez-Lezcano <nando@ccrma.stanford.edu>
# ;; Version 1.0 for clm-2 released in January 2001
# ;; http://www-ccrma.stanford.edu/~nando/clm/freeverb/
# ;;
# ;; Changes to the original code by Jezar (by Fernando Lopez-Lezcano):
# ;; - the clm version can now work with a mono input or an n-channel input
# ;;   stream (in the latter case the number of channels of the input and output
# ;;   streams must match.
# ;; - the "wet" parameter has been eliminated as it does not apply to the model
# ;;   that clm uses to generate reverberation
# ;; - the "width" parameter name has been changed to :global. It now controls the
# ;;   coefficients of an NxN matrix that specifies how the output of the reverbs
# ;;   is mixed into the output stream.
# ;; - predelays for the input channels have been added.
# ;; - damping can be controlled individually for each channel. 

# For more information see clm-2/freeverb/index.html.

# Functions:

# fcomb(comb, input)
# freeverb_rb(startime, dur, *args) (pure Ruby version)
# freeverb(startime, dur, *args)    (with inlined C code)
# run_freeverb(*args)

# Code:

require "examp"
require "ws"
require "v"

Fcomb = Struct.new("Fcomb", :delay, :filter, :feedback)

def fcomb(comb, input)
  delay(comb[:delay], input + one_zero(comb[:filter], tap(comb[:delay])) * comb[:feedback])
end

# The pure Ruby version of freeverb (which is very time consuming, see
# below for a faster one).

def freeverb_rb(startime, dur, *args)
  room_decay        = get_args(args, :room_decay, 0.5)
  damping           = get_args(args, :damping, 0.5)
  global            = get_args(args, :global, 0.3)
  predelay          = get_args(args, :predelay, 0.03)
  output_gain       = get_args(args, :output_gain, 1.0)
  output_mixer      = get_args(args, :output_mixer, nil)
  scale_room_decay  = get_args(args, :scale_room_decay, 0.28)
  offset_room_decay = get_args(args, :offset_room_decay, 0.7)
  combtuning        = get_args(args, :combtuning, [1116, 1188, 1277, 1356, 1422, 1491, 1557, 1617])
  allpasstuning     = get_args(args, :allpasstuning, [556, 441, 341, 225])
  scale_damping     = get_args(args, :scale_damping, 0.4)
  stereo_spread     = get_args(args, :stereo_spread, 23)
  verbose           = get_args(args, :verbose, $rbm_verbose)
  beg, len = times2samples(startime, dur)
  out_chans = mus_channels($rbm_output)
  out_mix = (mixer?(output_mixer) ? output_mixer : make_mixer(out_chans, 0.0))
  out_buf = make_frame(out_chans, 0.0)
  out_gain = output_gain
  f_out = make_frame(out_chans, 0.0)
  in_chans = mus_channels($rbm_reverb)
  f_in = make_frame(in_chans, 0.0)
  predelays = Array.new(in_chans)
  local_gain = (1 - global) / (1 - 1.0 / out_chans) + 1.0 / out_chans
  global_gain = (out_chans - local_gain * out_chans) / (out_chans * out_chans - out_chans)
  srate_scale = $rbm_srate / 44100.0
  room_decay_val = room_decay * scale_room_decay + offset_room_decay
  numcombs = combtuning.length
  numallpasses = allpasstuning.length
  combs = Array.new(out_chans) do Array.new(numcombs) end
  allpasses = Array.new(out_chans) do Array.new(numallpasses) end
  if verbose
    message("%s: %d input channels, %d output channels", get_func_name(), in_chans, out_chans)
  end
  if in_chans > 1 and in_chans != out_chans
    die("input must be mono or input channels must equal output channels")
  end
  unless mixer?(output_mixer)
    if output_mixer.kind_of?(Array)
      out_chans.times do |i|
        out_chans.times do |j|
          mixer_set!(out_mix, i, j, output_mixer[i][j])
        end
      end
    else
      out_chans.times do |i|
        out_chans.times do |j|
          mixer_set!(out_mix, i, j, (out_gain * (i == j ? local_gain : global_gain)) / out_chans)
        end
      end
    end
  end
  in_chans.times do |c|
    predelays[c] = make_delay(:size, $rbm_srate *
                              (predelay.kind_of?(Array) ? predelay[c] : predelay))
  end
  out_chans.times do |c|
    combtuning.each_with_index do |tuning, i|
      l = (srate_scale * tuning).round
      dmp = scale_damping * (damping.kind_of?(Array) ? damping[i] : damping)
      l += (srate_scale * stereo_spread).round if c.odd?
      combs[c][i] = Fcomb.new(make_delay(l), make_one_zero(:a0, 1.0 - dmp, :a1, dmp),
                              room_decay_val)
    end
  end
  out_chans.times do |c|
    allpasstuning.each_with_index do |tuning, i|
      l = (srate_scale * tuning).round
      l += (srate_scale * stereo_spread).round if c.odd?
      allpasses[c][i] = make_all_pass(:size, l, :feedforward, -1, :feedback, 0.5)
    end
  end
  beg.upto(len) do |i|
    file2frame($rbm_reverb, i, f_in)
    if in_chans > 1
      out_chans.times do |c|
        frame_set!(f_in, c, delay(predelays[c], frame_ref(f_in, c)))
        frame_set!(f_out, c, 0.0)
        numcombs.times do |j|
          frame_set!(f_out, c, frame_ref(f_out, c) + fcomb(combs[c][j], frame_ref(f_in, c)))
        end
      end
    else
      frame_set!(f_in, 0, delay(predelays[0], frame_ref(f_in, 0)))
      out_chans.times do |c|
        frame_set!(f_out, c, 0.0)
        numcombs.times do |j|
          frame_set!(f_out, c, frame_ref(f_out, c) + fcomb(combs[c][j], frame_ref(f_in, 0)))
        end
      end
    end
    out_chans.times do |c|
      numallpasses.times do |j|
        frame_set!(f_out, c, all_pass(allpasses[c][j], frame_ref(f_out, c)))
      end
    end
    frame2file($rbm_output, i, frame2frame(out_mix, f_out, out_buf))
  end
end

# The faster version of freeverb with inlined C loop.

def freeverb(startime, dur, *args)
  room_decay        = get_args(args, :room_decay, 0.5)
  damping           = get_args(args, :damping, 0.5)
  global            = get_args(args, :global, 0.3)
  predelay          = get_args(args, :predelay, 0.03)
  output_gain       = get_args(args, :output_gain, 1.0)
  output_mixer      = get_args(args, :output_mixer, nil)
  scale_room_decay  = get_args(args, :scale_room_decay, 0.28)
  offset_room_decay = get_args(args, :offset_room_decay, 0.7)
  combtuning        = get_args(args, :combtuning, [1116, 1188, 1277, 1356, 1422, 1491, 1557, 1617])
  allpasstuning     = get_args(args, :allpasstuning, [556, 441, 341, 225])
  scale_damping     = get_args(args, :scale_damping, 0.4)
  stereo_spread     = get_args(args, :stereo_spread, 23)
  verbose           = get_args(args, :verbose, $rbm_verbose)
  beg, len = times2samples(startime, dur)
  out_chans = mus_channels($rbm_output)
  out_mix = (mixer?(output_mixer) ? output_mixer : make_mixer(out_chans, 0.0))
  out_buf = make_frame(out_chans, 0.0)
  out_gain = output_gain
  f_out = make_frame(out_chans, 0.0)
  in_chans = mus_channels($rbm_reverb)
  f_in = make_frame(in_chans, 0.0)
  predelays = Array.new(in_chans)
  local_gain = (1 - global) / (1 - 1.0 / out_chans) + 1.0 / out_chans
  global_gain = (out_chans - local_gain * out_chans) / (out_chans * out_chans - out_chans)
  srate_scale = $rbm_srate / 44100.0
  room_decay_val = room_decay * scale_room_decay + offset_room_decay
  numcombs = combtuning.length
  numallpasses = allpasstuning.length
  combs = Array.new(out_chans) do Array.new(numcombs) end
  allpasses = Array.new(out_chans) do Array.new(numallpasses) end
  if verbose
    message("%s: %d input channels, %d output channels", get_func_name(), in_chans, out_chans)
  end
  if in_chans > 1 and in_chans != out_chans
    die("input must be mono or input channels must equal output channels")
  end
  unless mixer?(output_mixer)
    if output_mixer.kind_of?(Array)
      out_chans.times do |i|
        out_chans.times do |j|
          mixer_set!(out_mix, i, j, output_mixer[i][j])
        end
      end
    else
      out_chans.times do |i|
        out_chans.times do |j|
          mixer_set!(out_mix, i, j, (out_gain * (i == j ? local_gain : global_gain)) / out_chans)
        end
      end
    end
  end
  in_chans.times do |c|
    predelays[c] = make_delay(:size, $rbm_srate *
                              (predelay.kind_of?(Array) ? predelay[c] : predelay))
  end
  out_chans.times do |c|
    combtuning.each_with_index do |tuning, i|
      l = (srate_scale * tuning).round
      dmp = scale_damping * (damping.kind_of?(Array) ? damping[i] : damping)
      l += (srate_scale * stereo_spread).round if c.odd?
      combs[c][i] = Fcomb.new(make_delay(l), make_one_zero(:a0, 1.0 - dmp, :a1, dmp),
                              room_decay_val)
    end
  end
  out_chans.times do |c|
    allpasstuning.each_with_index do |tuning, i|
      l = (srate_scale * tuning).round
      l += (srate_scale * stereo_spread).round if c.odd?
      allpasses[c][i] = make_all_pass(:size, l, :feedforward, -1, :feedback, 0.5)
    end
  end
  run_freeverb(beg,
               len,
               in_chans,
               out_chans,
               numcombs,
               numallpasses,
               combs,
               predelays,
               allpasses,
               f_in,
               f_out,
               out_mix,
               out_buf)
end

def run_freeverb(*args)
  prelude = %Q{
#include <sndlib.h>
#include <clm.h>

typedef struct {
    mus_any *gen;
    VALUE *vcts;
    int nvcts;
    void *input_ptree;
} mus_xen;
  
#define RSNDGEN(obj) (mus_any *)(((mus_xen *)(DATA_PTR(obj)))->gen)

static Float
fcomb_c(VALUE comb, Float input) {
    mus_any *delay = RSNDGEN(RSTRUCT(comb)->ptr[0]);
    mus_any *filter = RSNDGEN(RSTRUCT(comb)->ptr[1]);
    Float feedback = NUM2DBL(RSTRUCT(comb)->ptr[2]);
    return mus_delay_1(delay, input + mus_one_zero(filter, mus_tap_1(delay)) * feedback);
}
}

  inline args, prelude, %Q{
    off_t i = 0L;
    int c, j;
    off_t beg = FIX2LONG(argv[i++]);
    off_t len = FIX2LONG(argv[i++]);
    int in_chans = FIX2INT(argv[i++]);
    int out_chans = FIX2INT(argv[i++]);
    int numcombs = FIX2INT(argv[i++]);
    int numallpasses = FIX2INT(argv[i++]);
    VALUE combs = argv[i++];
    VALUE predelays = argv[i++];
    VALUE allpasses = argv[i++];
    mus_any *f_in = RSNDGEN(argv[i++]);
    mus_any *f_out = RSNDGEN(argv[i++]);
    mus_any *out_mix = RSNDGEN(argv[i++]);
    mus_any *out_buf = RSNDGEN(argv[i++]);
    mus_any *out = (rb_gv_get("$rbm_output") != Qfalse) ? RSNDGEN(rb_gv_get("$rbm_output")) : NULL;
    mus_any *rev = (rb_gv_get("$rbm_reverb") != Qfalse) ? RSNDGEN(rb_gv_get("$rbm_reverb")) : NULL;

    for(i = beg; i < len; i++) {
        mus_file2frame(rev, i, f_in);
        if(in_chans > 1) {
            for(c = 0; c < out_chans; c++) {
                mus_frame_set(f_in, c, mus_delay_1(RSNDGEN(RARRAY(predelays)->ptr[c]),
						   mus_frame_ref(f_in, c)));
                mus_frame_set(f_out, c, 0.0);
                for(j = 0; j < numcombs; j++)
                    mus_frame_set(f_out, c,
				  mus_frame_ref(f_out, c) +
				  fcomb_c(RARRAY(RARRAY(combs)->ptr[c])->ptr[j],
					  mus_frame_ref(f_in, c)));
            }
        }
        else {
            mus_frame_set(f_in, 0, mus_delay_1(RSNDGEN(RARRAY(predelays)->ptr[0]),
					       mus_frame_ref(f_in, 0)));
            for(c = 0; c < out_chans; c++) {
                mus_frame_set(f_out, c, 0.0);
                for(j = 0; j < numcombs; j++)
                    mus_frame_set(f_out, c,
				  mus_frame_ref(f_out, c) +
				  fcomb_c(RARRAY(RARRAY(combs)->ptr[c])->ptr[j],
					  mus_frame_ref(f_in, 0)));
            }
        }
        for(c = 0; c < out_chans; c++)
            for(j = 0; j < numallpasses; j++)
                mus_frame_set(f_out, c,
			      mus_all_pass_1(RSNDGEN(RARRAY(RARRAY(allpasses)->ptr[c])->ptr[j]),
					     mus_frame_ref(f_out, c)));
        mus_frame2file(out, i, mus_frame2frame(out_mix, f_out, out_buf));
    }
    return INT2FIX(i);
}
end

# freeverb.rb ends here
