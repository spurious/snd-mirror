# v.rb -- Inline FM violin version

# Author: Michael Scholz <scholz-micha@gmx.de>
# Last: Wed Nov 20 22:43:39 CET 2002
# Version: $Revision: 1.2 $

# NOTE: Don't `require' the normal inline.rb but use function `inline'
# included in this file. It will work with Snd as well as with Ruby.

$IN_SND = true unless defined? $IN_SND

require "sndlib"		# produces many but harmless warnings
require "examp"

undef fm_violin if defined? fm_violin
undef jc_reverb if defined? jc_reverb

#
# Inline example (see clm.html):
#

=begin
def run_simp(*args)
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
}

  inline args, prelude, %Q{
  
  long i = 2;
  float amp = NUM2DBL(argv[i++]);
  float index = NUM2DBL(argv[i++]);
  mus_any *os = RSNDGEN(argv[i++]);
  mus_output *out = (mus_output *)RSNDGEN(argv[i++]);

  for(i = FIX2LONG(argv[0]); i < FIX2LONG(argv[1]); i++)
    mus_outa(i, amp * mus_asymmetric_fm(os, index, 0.0), out);

  }
end

def simp(start, dur, amp, freq, index, r = 1.0, ratio = 1.0)
  beg = (start * $rbm_srate).round
  len = beg + (dur * $rbm_srate).round
  os = make_asymmetric_fm(freq, :ratio, ratio, :r, r)
  
  run_simp(beg, len, amp, index, os, $rbm_output)
end

with_sound(:play, 1, :statistics, true) { 
  0.upto(10) { |i| simp(i * .2, .2, .3, 440, .2 * i, (i + .01) / 10) }}
=end

#
# fm_violin([start=0.0[, dur=1.0[, freq=440.0[, amp=0.3[, *args]]]]])
#

def fm_violin(start = 0.0, dur = 1.0, freq = 440.0, amp = 0.3, *args)
  func_name = "\n" + get_func_name() + "()"
  usage = "fm_violin([start=0.0[, dur=1.0[, freq=440.0[, amp=0.3[, *args]]]]])

fm_violin(:help)

	:fm_index,              1.0
	:amp_env,               [0, 0, 25, 1, 75, 1, 100, 0]
	:periodic_vibrato_rate, 5.0
	:periodic_vibrato_amp,  0.0025
	:random_vibrato_rate,   16.0
	:random_vibrato_amp,    0.005
	:noise_freq,            1000.0
	:noise_amount,          0.0
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
	:fm1_index,             0.0
	:fm2_index,             0.0
	:fm3_index,             0.0
	:base,                  1.0
	:reverb_amount,         0.01
	:index_type,            :violin
	:degree,                0.0
	:distance,              1.0
	:degrees,               0.0
	:help

   Ruby: fm_violin(0, 1, 440, .1, :fm_index, 2.0)
  Guile: (fm-violin 0 1 440 .1 :fm-index 2.0)

Example: with_sound { fm_violin(0, 1, 440, .1, :fm_index, 2.0) }\n"

  unless(start == :help or get_args(args, :help, false))
    fm_index              = get_args(args, :fm_index, 1.0)
    amp_env               = get_args(args, :amp_env, [0, 0, 25, 1, 75, 1, 100, 0])
    periodic_vibrato_rate = get_args(args, :periodic_vibrato_rate, 5.0)
    periodic_vibrato_amp  = get_args(args, :periodic_vibrato_amp, 0.0025)
    random_vibrato_rate   = get_args(args, :random_vibrato_rate, 16.0)
    random_vibrato_amp    = get_args(args, :random_vibrato_amp, 0.005)
    noise_freq            = get_args(args, :noise_freq, 1000.0)
    noise_amount          = get_args(args, :noise_amount, 0.0)
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
    index_type            = get_args(args, :index_type, :violin)
    degree                = get_args(args, :degree, false)
    distance              = get_args(args, :distance, 1.0)
    degrees               = get_args(args, :degrees, false)

    run_fm_violin(start, 
		  dur, 
		  freq, 
		  amp,
		  fm_index,
		  amp_env,
		  periodic_vibrato_rate,
		  periodic_vibrato_amp,
		  random_vibrato_rate, 
		  random_vibrato_amp,
		  noise_freq,
		  noise_amount, 
		  ind_noise_freq,
		  ind_noise_amount,
		  amp_noise_freq,
		  amp_noise_amount,
		  gliss_env,
		  gliss_amount, 
		  fm1_env, 
		  fm2_env,
		  fm3_env,
		  fm1_rat, 
		  fm2_rat, 
		  fm3_rat, 
		  (fm1_index or 0.0), 
		  (fm2_index or 0.0),
		  (fm3_index or 0.0),
		  base,
		  reverb_amount,
		  index_type.to_s,
		  (degree or 0.0), 
		  distance, 
		  (degrees or 0.0)) 
  else
    message(usage)
  end
rescue
  die(usage + func_name)
end

#
# run_fm_violin(*args)
#

def run_fm_violin(*args)
  prelude = %Q{
#include <string.h>
#include <math.h>
#include <sndlib.h>
#include <clm.h>
  
typedef struct {
    mus_any *gen;
    VALUE *vcts;
    int nvcts;
    void *input_ptree;
} mus_xen;
  
#define RSNDGEN(obj) (mus_any *)(((mus_xen *)(DATA_PTR(obj)))->gen)

int
feq(float x, int i) {
    return(fabs(x - i) < .00001);
}

float *
get_ary(VALUE ary) {
    long i = 0;
    long len = RARRAY(ary)->len;
    float *fary = ALLOC_N(float, len);
  
    for(i = 0; i < len; i++)
	fary[i] = NUM2DBL(rb_ary_entry(ary, i));
  
    return fary;
}
}

  inline args, prelude, %Q{
    long i = 0L;
    float start = NUM2DBL(argv[i++]);
    float dur = NUM2DBL(argv[i++]);
    float freq = NUM2DBL(argv[i++]);
    float amp = NUM2DBL(argv[i++]);
    float fm_index = NUM2DBL(argv[i++]);
    int namp_env = RARRAY(argv[i])->len;
    float *amp_env = get_ary(argv[i++]);
    float periodic_vibrato_rate = NUM2DBL(argv[i++]);
    float periodic_vibrato_amp = NUM2DBL(argv[i++]);
    float random_vibrato_rate = NUM2DBL(argv[i++]);
    float random_vibrato_amp = NUM2DBL(argv[i++]);
    float noise_freq = NUM2DBL(argv[i++]);
    float noise_amount = NUM2DBL(argv[i++]);
    float ind_noise_freq = NUM2DBL(argv[i++]);
    float ind_noise_amount = NUM2DBL(argv[i++]);
    float amp_noise_freq = NUM2DBL(argv[i++]);
    float amp_noise_amount = NUM2DBL(argv[i++]);
    int ngliss_env = RARRAY(argv[i])->len;
    float *gliss_env = get_ary(argv[i++]);
    float gliss_amount = NUM2DBL(argv[i++]);
    int nfm1_env = RARRAY(argv[i])->len;
    float *fm1_env = get_ary(argv[i++]);
    int nfm2_env = RARRAY(argv[i])->len;
    float *fm2_env = get_ary(argv[i++]);
    int nfm3_env = RARRAY(argv[i])->len;
    float *fm3_env = get_ary(argv[i++]);
    float fm1_rat = NUM2DBL(argv[i++]);
    float fm2_rat = NUM2DBL(argv[i++]);
    float fm3_rat = NUM2DBL(argv[i++]);
    float fm1_index = NUM2DBL(argv[i++]);
    float fm2_index = NUM2DBL(argv[i++]);
    float fm3_index = NUM2DBL(argv[i++]);
    float base = NUM2DBL(argv[i++]);
    float reverb_amount = NUM2DBL(argv[i++]);
    char *index_type = STR2CSTR(argv[i++]);
    float degree = NUM2DBL(argv[i++]);
    float distance = NUM2DBL(argv[i++]);
    float degrees = NUM2DBL(argv[i++]);

    mus_output *out = (rb_gv_get("$rbm_output") != Qfalse) ?
	(mus_output *)RSNDGEN(rb_gv_get("$rbm_output")) : NULL;
    mus_output *rev_out = (rb_gv_get("$rbm_reverb") != Qfalse) ?
	(mus_output *)RSNDGEN(rb_gv_get("$rbm_reverb")) : NULL;

    long beg = 0L;
    long len = 0L;
    int easy_case = 0;
    int npartials = 0;
    int chans = mus_channels((mus_any *)out);
    int vln = 1;

    float *coeffs = NULL;
    float *partials = NULL;
    float frq_scl = 0.0;
    float maxdev = 0.0;
    float logfrq = 0.0;
    float sqrtfrq = 0.0;
    float index1 = 0.0;
    float index2 = 0.0;
    float index3 = 0.0;
    float norm = 0.0;
    float vib = 0.0;
    float modulation = 0.0;
    float fuzz = 0.0;
    float ind_fuzz = 1.0;
    float amp_fuzz = 1.0;

    mus_any *carrier = NULL;
    mus_any *fmosc1 = NULL; 
    mus_any *fmosc2 = NULL;
    mus_any *fmosc3 = NULL;
    mus_any *ampf = NULL;
    mus_any *indf1 = NULL;
    mus_any *indf2 = NULL;
    mus_any *indf3 = NULL;
    mus_any *fm_noi = NULL;
    mus_any *ind_noi = NULL;
    mus_any *amp_noi = NULL;
    mus_any *pervib = NULL;
    mus_any *ranvib = NULL;
    mus_any *frqf = NULL;
    mus_any *loc = NULL;
  
    beg = floor(start * mus_srate());
    len = beg + floor(dur * mus_srate());
    frq_scl = mus_hz2radians(freq);
    maxdev = frq_scl * fm_index;

    if((noise_amount == 0.0) && 
       (feq(fm1_rat, floor(fm1_rat))) &&
       (feq(fm2_rat, floor(fm2_rat))) &&
       (feq(fm3_rat, floor(fm3_rat)))) 
	easy_case = 1;
  
    logfrq = log(freq);
    sqrtfrq = sqrt(freq);
  
    if(strcmp("cello", index_type) == 0)
	vln = 0;
  
    if(fm1_index != 0.0)
	index1 = fm1_index;
    else {
	index1 = vln ? (maxdev * 5.0 / logfrq) : (maxdev * 7.5 / logfrq);
	if(index1 > M_PI)
	    index1 = M_PI;
    }
  
    if(fm2_index != 0.0)
	index2 = fm2_index;
    else {
	index2 = vln ? (maxdev * 3.0 * (8.5 - logfrq) / (3.0 + freq * .001)) :
	    (maxdev * 3.0 * 15.0 / sqrtfrq);
	if(index2 > M_PI)
	    index2 = M_PI;
    }

    if(fm3_index != 0.0)
	index3 = fm3_index;
    else {
	index3 = vln ? (maxdev * 4.0 / sqrtfrq) : (maxdev * 8.0 / sqrtfrq);
	if(index3 > M_PI)
	    index3 = M_PI;
    }

    if(easy_case) {
	npartials = floor(fm1_rat);
	if((floor(fm2_rat)) > npartials)
	    npartials = floor(fm2_rat);
	if((floor(fm3_rat)) > npartials)
	    npartials = floor(fm3_rat);
	npartials++;
	partials = (float *)CALLOC(npartials, sizeof(float));
	partials[(int)(fm1_rat)] = index1;
	partials[(int)(fm2_rat)] = index2;
	partials[(int)(fm3_rat)] = index3;
	coeffs = mus_partials2polynomial(npartials, partials, 1);
	norm = 1.0;
    }
    else
	norm = index1;

    carrier = mus_make_oscil(freq, 0.0);

    fmosc1 = mus_make_oscil(freq * (float)fm1_rat, 0.0);
    if(!(easy_case)) {
	fmosc2 = mus_make_oscil(freq * (float)fm2_rat, 0.0);
	fmosc3 = mus_make_oscil(freq * (float)fm3_rat, 0.0);
    } 
    
    ampf = mus_make_env(amp_env, namp_env / 2, amp, 0.0, base, dur, 0, 0, NULL);
  
    indf1 = mus_make_env(fm1_env, nfm1_env / 2, norm, 0.0, 1.0, dur, 0, 0, NULL);
    if(!(easy_case)) {
	indf2 = mus_make_env(fm2_env, nfm2_env / 2,
			     index2, 0.0, 1.0, dur, 0, 0, NULL);
	indf3 = mus_make_env(fm3_env, nfm3_env / 2,
			     index3, 0.0, 1.0, dur, 0, 0, NULL);
    }

    if(gliss_amount != 0.0) 
	frqf = mus_make_env(gliss_env, ngliss_env / 2, 
			    gliss_amount * (float)frq_scl, 0.0, 1.0, dur, 0, 0, NULL);

    pervib = mus_make_triangle_wave(periodic_vibrato_rate, 
				    (float)frq_scl * periodic_vibrato_amp, 0.0);
    ranvib = mus_make_rand_interp(random_vibrato_rate, (float)frq_scl * random_vibrato_amp);

    if(noise_amount != 0.0) 
	fm_noi = mus_make_rand(noise_freq, (float)noise_amount * M_PI);

    if(ind_noise_amount != 0.0 && ind_noise_freq != 0.0) 
	ind_noi = mus_make_rand_interp(ind_noise_freq, ind_noise_amount);

    if(amp_noise_amount != 0.0 && amp_noise_freq != 0.0)
	amp_noi = mus_make_rand_interp(amp_noise_freq, amp_noise_amount);

    if(degree != 0.0) 
	degree;
    else if(degrees != 0.0) 
	degree = degrees;
    else 
	degree = mus_random(90.0);

    loc = mus_make_locsig(degree, distance, reverb_amount, chans, out, rev_out, MUS_LINEAR);

    for (i = beg; i < len; i++) {
	if(noise_amount != 0.0)
	    fuzz = mus_rand(fm_noi, 0.0);
	vib = frqf ? mus_env(frqf) : 0.0;
	vib += mus_triangle_wave(pervib, 0.0) + mus_rand_interp(ranvib, 0.0);
	if(ind_noi)
	    ind_fuzz = 1.0 + mus_rand_interp(ind_noi, 0.0);
	if(amp_noi)
	    amp_fuzz = 1.0 + mus_rand_interp(amp_noi, 0.0);
	if(easy_case)
	    modulation = mus_env(indf1) *
		mus_polynomial(coeffs, mus_oscil(fmosc1, vib, 0.0), npartials);
	else
	    modulation = mus_env(indf1) *
		mus_oscil(fmosc1, fuzz + fm1_rat * vib, 0.0) + 
		mus_env(indf2) * mus_oscil(fmosc2, (fuzz + fm2_rat * vib), 0.0) + 
		mus_env(indf3) * mus_oscil(fmosc3, (fuzz + fm3_rat * vib), 0.0);
	
	mus_locsig(loc, i, mus_env(ampf) * amp_fuzz *
		   mus_oscil(carrier, vib + ind_fuzz * modulation, 0.0));
    }
  
    mus_free(pervib);
    mus_free(ranvib);
    mus_free(carrier);
    mus_free(fmosc1);
    mus_free(ampf);
    mus_free(indf1);

    if(fm_noi)
	mus_free(fm_noi);
    if(ind_noi)
	mus_free(ind_noi);
    if(amp_noi)
	mus_free(amp_noi);
    if(frqf)
	mus_free(frqf);
    if(!(easy_case)) {
	mus_free(indf2);
	mus_free(indf3);
	mus_free(fmosc2);
	mus_free(fmosc3);
    }
    else
	FREE(partials);

    mus_free(loc);

    return INT2FIX(i);
}
end

#
# jc_reverb([args=[]])
#

def jc_reverb(args = [])
  func_name = "\n" + get_func_name() + "()"
  usage = "jc_reverb([args=[]])

jc_reverb(:help)

	:decay,    1.0
	:low_pass, false
	:volume,   1.0
	:amp_env1, false
	:amp_env2, false
	:delay1,   0.013
	:delay2,   0.011
	:help

The old Chowning reverberator (see snd-6/examp.scm).

Usage: jc_reverb(:decay, 2.0, :volume, .1)
       with_sound(:reverb, :jc_reverb) { fm_violin }\n"

  unless(get_args(args, :help, false))
    decay    = get_args(args, :decay, 1.0)
    low_pass = get_args(args, :low_pass, 0)
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
    outdel1 = make_delay((delay1 * srate).round)
    outdel2 = make_delay((delay2 * srate).round)
    dur = decay + mus_sound_frames($rbm_file_name) / srate.to_f
    len = (srate * dur).round
    envA = (amp_env1 ? make_env(amp_env1, volume, dur) : false)
    envB = (amp_env2 ? make_env(amp_env2, volume, dur) : false)
    delA = (envA ? env(envA) : volume)
    delB = (envB ? env(envB) : volume)
    
    run_jc_reverb(len,
		  low_pass,
		  allpass1,
		  allpass2,
		  allpass3,
		  comb1,
		  comb2,
		  comb3,
		  comb4,
		  outdel1,
		  outdel2,
		  delA,
		  delB)
  else
    message(usage)
  end
rescue
  die(usage + func_name)
end

#
# run_jc_reverb(*args)
#

def run_jc_reverb(*args)
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
}

  inline args, prelude, %Q{
    long i = 0L;
    long len = FIX2LONG(argv[i++]);
    int low_pass = FIX2INT(argv[i++]);
    mus_any *allpass1 = RSNDGEN(argv[i++]);
    mus_any *allpass2 = RSNDGEN(argv[i++]);
    mus_any *allpass3 = RSNDGEN(argv[i++]);
    mus_any *comb1 = RSNDGEN(argv[i++]);
    mus_any *comb2 = RSNDGEN(argv[i++]);
    mus_any *comb3 = RSNDGEN(argv[i++]);
    mus_any *comb4 = RSNDGEN(argv[i++]);
    mus_any *outdel1 = RSNDGEN(argv[i++]);
    mus_any *outdel2 = RSNDGEN(argv[i++]);
    float delA = NUM2DBL(argv[i++]);
    float delB = NUM2DBL(argv[i++]);

    mus_output *out = (rb_gv_get("$rbm_output") != Qfalse) ?
	(mus_output *)RSNDGEN(rb_gv_get("$rbm_output")) : NULL;
    mus_input *rev_in = (rb_gv_get("$rbm_reverb") != Qfalse) ?
	(mus_input *)RSNDGEN(rb_gv_get("$rbm_reverb")) : NULL;
    int chans = mus_channels((mus_any *)out);
    int rev_chans = mus_channels((mus_any *)rev_in);
    float srate = mus_srate();

    float ho = 0.0, allpass_sum = 0.0, all_sums = 0.0;
    float comb_sumA = 0.0, comb_sumB = 0.0;
    float comb_sum_1A = 0.0, comb_sum_2A = 0.0;
    float comb_sum_1B = 0.0, comb_sum_2B = 0.0;

    for(i = 0; i < len; i++) {
	ho = mus_ina(i, rev_in);
	allpass_sum = mus_all_pass(allpass3, 
				   mus_all_pass(allpass2,
						mus_all_pass(allpass1, ho, 0.0), 0.0), 0.0);
	comb_sum_2A = comb_sum_1A;
	comb_sum_1A = comb_sumA;
	comb_sumA = mus_comb(comb1, allpass_sum, 0.0) + mus_comb(comb2, allpass_sum, 0.0) +
	    mus_comb(comb3, allpass_sum, 0.0) + mus_comb(comb4, allpass_sum, 0.0);
      
	if(low_pass)
	    all_sums = 0.25 * (comb_sumA + comb_sum_2A) + 0.5 * comb_sum_1A;
	else
	    all_sums = comb_sumA;
	mus_outa(i, delA * mus_delay(outdel1, all_sums, 0.0), out);
      
	if(rev_chans == 2) {
	    ho = mus_inb(i, rev_in);
	    allpass_sum = mus_all_pass(allpass3,
				       mus_all_pass(allpass2,
						    mus_all_pass(allpass1, ho, 0.0), 0.0), 0.0);
	    comb_sum_2B = comb_sum_1B;
	    comb_sum_1B = comb_sumB;
	    comb_sumB = mus_comb(comb1, allpass_sum, 0.0) +
		mus_comb(comb2, allpass_sum, 0.0) +
		mus_comb(comb3, allpass_sum, 0.0) +
		mus_comb(comb4, allpass_sum, 0.0);
	  
	    if(low_pass)
		all_sums = 0.25 * (comb_sumB + comb_sum_2B) + 0.5 * comb_sum_1B;
	    else
		all_sums = comb_sumB;
	}

	if(chans == 2)
	    mus_outb(i, delB * mus_delay(outdel2, all_sums, 0.0), out);
    }

    return INT2FIX(i);
}
end

### Original README.txt of RubyInline 1.0.7 [MS]

=begin	
Ruby Inline
    http://www.zenspider.com/Languages/Ruby/
    support@zenspider.com

DESCRIPTION:
  
Ruby Inline is my quick attempt to create an analog to Perl's
Inline::C. It allows you to embed C external module code in your ruby
script directly. The code is compiled and run on the fly when
needed. The ruby version isn't near as feature-full as the perl
version, but it is neat!

FEATURES/PROBLEMS:
  
+ Quick and easy inlining of your C code embedded in your ruby script.
+ Only recompiles if the C code has changed.
+ Pretends to be secure.
+ Only uses standard ruby libraries, nothing extra to download.
+ Simple as it can be. Less than 125 lines long.
- Currently doesn't munge ruby names that aren't compatible in C (ex: a!())

SYNOPSYS:

  require "inline"
  class MyTest
    include Inline
    def fastfact(*args)
      inline args, <<-END
      int i, f=1;
      for (i = FIX2INT(argv[0]); i >= 1; i--) { f = f * i; }
      return INT2FIX(f);
      END
    end
  end
  t = MyTest.new()
  factorial_5 = t.fastfact(5)

Produces:

  <502> rm /tmp/Mod_MyTest_fastfact.*; ./example.rb 
  RubyInline 1.0.4
  Building /tmp/Mod_MyTest_fastfact.so with 'cc -shared -O -pipe  -fPIC -I /usr/local/lib/ruby/1.6/i386-freebsd4'
  Type = Inline, Iter = 1000000, time = 5.37746200 sec, 0.00000538 sec / iter
  <503> ./example.rb 
  RubyInline 1.0.4
  Type = Inline, Iter = 1000000, time = 5.26147500 sec, 0.00000526 sec / iter
  <504> ./example.rb native
  RubyInline 1.0.4
  Type = Native, Iter = 1000000, time = 24.09801500 sec, 0.00002410 sec / iter

REQUIREMENTS:

+ Ruby - 1.6.7 has been used on FreeBSD 4.6.
+ POSIX compliant system (ie pretty much any UNIX, or Cygwin on MS platforms).
+ A C compiler (the same one that compiled your ruby interpreter).

INSTALL:

+ no install instructions yet.

LICENSE:

(The MIT License)

Copyright (c) 2001-2002 Ryan Davis, Zen Spider Software

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
=end

### Patched inline.rb of RubyInline 1.0.7 [MS]

require "rbconfig"
require "ftools"

def caller_method_name()
  /\`([^\']+)\'/.match(caller(2).first)[1]
end

def assert_dir_secure(path)
  mode = File.stat(path).mode
  unless ((mode % 01000) & 0022) == 0 then # WARN: POSIX systems only...
    $stderr.puts "#{path} is insecure (#{sprintf('%o', mode)}), needs 0700 for perms" 
    exit 1
  end
end

RB_INLINE_VERSION = '1.0.7.p0'
#  VERSION = '1.0.7' [MS]

# TODO: extend the signature to pass in self in order to zap aliased methods
# def inline(args, prelude, src=nil, instance=self)
def inline(args, prelude, src=nil)

  if src.nil? then
    src = prelude
    prelude = ""
  end

  rootdir = ENV['INLINEDIR'] || ENV['HOME']
  assert_dir_secure(rootdir)

  tmpdir = rootdir + "/.ruby_inline"
  unless File.directory? tmpdir then
    $stderr.puts "NOTE: creating #{tmpdir} for RubyInline" if $DEBUG
    Dir.mkdir(tmpdir, 0700)
  end
  assert_dir_secure(tmpdir)

  myclass = self.class
  mymethod = caller_method_name
  mod_name = "Mod_#{myclass}_#{mymethod}"
  so_name = "#{tmpdir}/#{mod_name}.so"

  # extracted from mkmf.rb
  srcdir  = Config::CONFIG["srcdir"]
  archdir = Config::CONFIG["archdir"]
  if File.exist? archdir + "/ruby.h"
    hdrdir = archdir
  elsif File.exist? srcdir + "/ruby.h"
    hdrdir = srcdir
  else
    $stderr.puts "ERROR: Can't find header files for ruby. Exiting..."
    exit 1
  end

  # Generating code
  src = %Q{
#include <ruby.h>
#{prelude}

  static VALUE t_#{mymethod}(int argc, VALUE *argv, VALUE self) {
    #{src}
  }

  VALUE c#{mod_name};

  void Init_#{mod_name}() {
    c#{mod_name} = rb_define_module("#{mod_name}");
    rb_define_method(c#{mod_name}, "_#{mymethod}", t_#{mymethod}, -1);
  }
}
  
  src_name = "#{tmpdir}/#{mod_name}.c"

  # move previous version to the side if it exists
  test_cmp = false
  old_src_name = src_name + ".old"
  if test ?f, src_name then
    test_cmp = true
    File.rename src_name, old_src_name
  end

  f = File.new(src_name, "w")
  f.puts src
  f.close

  # recompile only if the files are different
  recompile = true
  if test_cmp and File::compare(old_src_name, src_name, $DEBUG) then
    recompile = false
  end

  if recompile then
    cmd = "#{Config::CONFIG['LDSHARED']} #{Config::CONFIG['CFLAGS']} -I #{hdrdir} -o #{so_name} #{src_name}"
    
    if /mswin32/ =~ RUBY_PLATFORM then
      cmd += " -link /INCREMENTAL:no /EXPORT:Init_#{mod_name}"
    end
    
    $stderr.puts "Building #{so_name} with '#{cmd}'" if $DEBUG
    `#{cmd}`
  end
  
  # Loading & Replacing w/ new method
  require "#{so_name}"
  myclass.class_eval("include #{mod_name}")
  myclass.class_eval("alias_method :old_#{mymethod}, :#{mymethod}")
  myclass.class_eval("alias_method :#{mymethod}, :_#{mymethod}")
  
  # Calling
  return method("_#{mymethod}").call(*args)
end

# v.rb ends here
