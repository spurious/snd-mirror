# v.rb -- Inlined fm_violin, jc_reverb, and nrev version

# Author: Michael Scholz <scholz-micha@gmx.de>
# Last: Sat Apr 12 04:39:37 CEST 2003
# Version: $Revision: 1.12 $

# NOTE: Don't `require' the normal inline.rb but use function `inline'
# included in this file.  It will work with Snd as well as with Ruby.

# If you have the libsndlib.so in your ${LD_LIBRARY_PATH} and if you
# have compiled Snd with link option -lsndlib, you don't need `require
# 'sndlib'' (see section `Dynamically loaded modules' in
# snd-6/grfsnd.html for more information about it).  Set in that case
# the Ruby variable $HAVE_SNDLIB_SO to true, here or in your startup
# file (e.g. ~./snd-ruby.rb).

# fm_violin(start, dur, freq, amp, *args)
# run_fm_violin(*args)
# jc_reverb(startime, dur, *args) 1/2 chans and 1/2 rev_chans
# run_jc_reverb(*args)
# nrev(startime, dur, *args)      1/2/4 chans and 1 rev_chan
# run_nrev(*args)

$IN_SND = true unless defined? $IN_SND
$HAVE_SNDLIB_SO = false unless defined? $HAVE_SNDLIB_SO

require "sndlib" unless $HAVE_SNDLIB_SO
require "examp"

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
  mus_any *out = RSNDGEN(argv[i++]);

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
  0.upto(10) { |i| simp(i * 0.2, 0.2, 0.3, 440, 0.2 * i, (i + 0.01) / 10) }}
=end

def fm_violin(start = 0.0, dur = 1.0, freq = 440.0, amp = 0.3, *args)
  doc("fm_violin([start=0.0[, dur=1.0[, freq=440.0[, amp=0.3[, *args]]]]])
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
	:index_type,            :violin [:cello]
	:degree,                0.0
	:distance,              1.0
	:degrees,               0.0
	:help
   Ruby: fm_violin(0, 1, 440, .1, :fm_index, 2.0)
  Guile: (fm-violin 0 1 440 .1 :fm-index 2.0)
Example: with_sound { fm_violin(0, 1, 440, .1, :fm_index, 2.0) }\n") if start == :help
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
rescue
  die get_func_name
end

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

    mus_any *out = (rb_gv_get("$rbm_output") != Qfalse) ? RSNDGEN(rb_gv_get("$rbm_output")) : NULL;
    mus_any *rev_out = (rb_gv_get("$rbm_reverb") != Qfalse) ?
      RSNDGEN(rb_gv_get("$rbm_reverb")) : NULL;

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

def jc_reverb(startime, dur = nil, *args)
  doc("jc_reverb(startime, dur, *args)
	:low_pass, false
	:volume,   1.0
	:amp_env1, false
	:amp_env2, false
	:delay1,   0.013
	:delay2,   0.011
	:help
The old Chowning reverberator (see snd-6/examp.scm).
Usage: with_sound(:reverb, :jc_reverb) { fm_violin }\n") if startime == :help
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
  outdel1 = make_delay((delay1 * $rbm_srate).round)
  outdel2 = make_delay((delay2 * $rbm_srate).round)
  beg, len = times2samples(startime, dur)
  envA = (amp_env1 ? make_env(amp_env1, volume, dur) : false)
  envB = (amp_env2 ? make_env(amp_env2, volume, dur) : false)
  delA = (envA ? env(envA) : volume)
  delB = (envB ? env(envB) : volume)
  run_jc_reverb(beg,
                len,
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
rescue
  die get_func_name
end

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
    long beg = FIX2LONG(argv[i++]);
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

    mus_any *out = (rb_gv_get("$rbm_output") != Qfalse) ?
	RSNDGEN(rb_gv_get("$rbm_output")) : NULL;
    mus_any *rev_in = (rb_gv_get("$rbm_reverb") != Qfalse) ?
	RSNDGEN(rb_gv_get("$rbm_reverb")) : NULL;
    int chans = mus_channels((mus_any *)out);
    int rev_chans = mus_channels((mus_any *)rev_in);
    float srate = mus_srate();

    float ho = 0.0, allpass_sum = 0.0, all_sums = 0.0;
    float comb_sumA = 0.0, comb_sumB = 0.0;
    float comb_sum_1A = 0.0, comb_sum_2A = 0.0;
    float comb_sum_1B = 0.0, comb_sum_2B = 0.0;

    for(i = beg; i < len; i++) {
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

def nrev(startime, dur = nil, *args)
  doc("nrev(startime, dur, *args)
        :reverb_factor, 1.09
        :lp_coeff,      0.7
        :lp_out_coeff,  0.85
        :output_scale,  1.0
        :amp_env,       [0, 1, 1, 1]
        :volume,        1.0
This is a faster version (with C-loop) of dlocnrev (see
clm-2/dlocsig/dlocsig.lisp).\n") if startime == :help
  reverb_factor = get_args(args, :reverb_factor, 1.09)
  lp_coeff      = get_args(args, :lp_coeff, 0.7)
  lp_out_coeff  = get_args(args, :lp_out_coeff, 0.85)
  output_scale  = get_args(args, :output_scale, 1.0)
  amp_env       = get_args(args, :amp_env, [0, 1, 1, 1])
  volume        = get_args(args, :volume, 1.0)
  beg, len = times2samples(startime, dur)
  env_a = make_env(:envelope, amp_env, :scaler, output_scale, :duration, dur)
  srscale = $rbm_srate / 25641.0
  val = 0
  dly_len = [1433, 1601, 1867, 2053, 2251, 2399, 347, 113, 37, 59, 53, 43, 37, 29, 19]
  dly_len.map! do |x|
    val = (srscale * x).floor
    val += 1 if val.modulo(2).zero?
    val += 2 until val.prime?
    val
  end
  comb1 = make_comb(reverb_factor * 0.822, dly_len[0])
  comb2 = make_comb(reverb_factor * 0.802, dly_len[1])
  comb3 = make_comb(reverb_factor * 0.773, dly_len[2])
  comb4 = make_comb(reverb_factor * 0.753, dly_len[3])
  comb5 = make_comb(reverb_factor * 0.753, dly_len[4])
  comb6 = make_comb(reverb_factor * 0.733, dly_len[5])
  low = make_one_pole(lp_out_coeff, lp_coeff - 1.0)
  low_a = make_one_pole(lp_out_coeff, lp_coeff - 1.0)
  low_b = make_one_pole(lp_out_coeff, lp_coeff - 1.0)
  low_c = make_one_pole(lp_out_coeff, lp_coeff - 1.0)
  low_d = make_one_pole(lp_out_coeff, lp_coeff - 1.0)
  allpass1 = make_all_pass(-0.7, 0.7, dly_len[6])
  allpass2 = make_all_pass(-0.7, 0.7, dly_len[7])
  allpass3 = make_all_pass(-0.7, 0.7, dly_len[8])
  allpass4 = make_all_pass(-0.7, 0.7, dly_len[10])  # 9 for stereo
  allpass5 = make_all_pass(-0.7, 0.7, dly_len[11])
  allpass6 = make_all_pass(-0.7, 0.7, dly_len[12])
  allpass7 = make_all_pass(-0.7, 0.7, dly_len[13])
  allpass8 = make_all_pass(-0.7, 0.7, dly_len[14])
  run_nrev(beg,
           len,
           output_scale,
           volume,
           env_a,
           comb1,
           comb2,
           comb3,
           comb4,
           comb5,
           comb6,
           low,
           low_a,
           low_b,
           low_c,
           low_d,
           allpass1,
           allpass2,
           allpass3,
           allpass4,
           allpass5,
           allpass6,
           allpass7,
           allpass8)
rescue
  die get_func_name
end

def run_nrev(*args)
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
    long beg = FIX2LONG(argv[i++]);
    long len = FIX2LONG(argv[i++]);
    float output_scale = NUM2DBL(argv[i++]);
    float volume = NUM2DBL(argv[i++]);
    mus_any *env_a = RSNDGEN(argv[i++]);
    mus_any *comb1 = RSNDGEN(argv[i++]);
    mus_any *comb2 = RSNDGEN(argv[i++]);
    mus_any *comb3 = RSNDGEN(argv[i++]);
    mus_any *comb4 = RSNDGEN(argv[i++]);
    mus_any *comb5 = RSNDGEN(argv[i++]);
    mus_any *comb6 = RSNDGEN(argv[i++]);
    mus_any *low = RSNDGEN(argv[i++]);
    mus_any *low_a = RSNDGEN(argv[i++]);
    mus_any *low_b = RSNDGEN(argv[i++]);
    mus_any *low_c = RSNDGEN(argv[i++]);
    mus_any *low_d = RSNDGEN(argv[i++]);
    mus_any *allpass1 = RSNDGEN(argv[i++]);
    mus_any *allpass2 = RSNDGEN(argv[i++]);
    mus_any *allpass3 = RSNDGEN(argv[i++]);
    mus_any *allpass4 = RSNDGEN(argv[i++]);
    mus_any *allpass5 = RSNDGEN(argv[i++]);
    mus_any *allpass6 = RSNDGEN(argv[i++]);
    mus_any *allpass7 = RSNDGEN(argv[i++]);
    mus_any *allpass8 = RSNDGEN(argv[i++]);
    mus_any *out = (rb_gv_get("$rbm_output") != Qfalse) ?
	RSNDGEN(rb_gv_get("$rbm_output")) : NULL;
    mus_any *rev_in = (rb_gv_get("$rbm_reverb") != Qfalse) ?
	RSNDGEN(rb_gv_get("$rbm_reverb")) : NULL;
    int chans = mus_channels((mus_any *)out);
    float sample_a = 0.0, sample_b = 0.0, sample_c = 0.0, sample_d = 0.0;
    float rev = 0.0, outrev = 0.0;
    for(i = beg; i < len; i++) {
	rev = volume * mus_env(env_a) * mus_ina(i, rev_in);
        outrev = mus_all_pass(allpass4,
                     mus_one_pole(low, 
			 mus_all_pass(allpass3,
			     mus_all_pass(allpass2,
                                 mus_all_pass(allpass1,
                                     mus_comb(comb1, rev, 0.0) +
                                     mus_comb(comb2, rev, 0.0) +
                                     mus_comb(comb3, rev, 0.0) +
                                     mus_comb(comb4, rev, 0.0) +
                                     mus_comb(comb5, rev, 0.0) +
                                     mus_comb(comb6, rev, 0.0), 0.0), 0.0), 0.0)), 0.0);
        sample_a = output_scale * mus_one_pole(low_a, mus_all_pass(allpass5, outrev, 0.0));
        sample_b = output_scale * mus_one_pole(low_b, mus_all_pass(allpass6, outrev, 0.0));
        sample_c = output_scale * mus_one_pole(low_c, mus_all_pass(allpass7, outrev, 0.0));
        sample_d = output_scale * mus_one_pole(low_d, mus_all_pass(allpass8, outrev, 0.0));
        if(chans == 2)
            mus_outa(i, (sample_a + sample_d) / 2.0, out);
        else
            mus_outa(i, sample_a, out);
        if((chans == 2) || (chans == 4)) {
            if(chans == 2)
                mus_outb(i, (sample_b + sample_c) / 2.0, out);
            else
                mus_outb(i, sample_b, out);
        }
        if(chans == 4) {
            mus_outc(i, sample_c, out);
            mus_outd(i, sample_d, out);
        }
    }
    return INT2FIX(i);
}
end

### Original README.txt of RubyInline 2.0.0 [MS]

# Ruby Inline
#     http://www.zenspider.com/Languages/Ruby/
#     support@zenspider.com
# 
# DESCRIPTION:
#   
# Ruby Inline is my quick attempt to create an analog to Perl's
# Inline::C. It allows you to embed C external module code in your ruby
# script directly. The code is compiled and run on the fly when
# needed. The ruby version isn't near as feature-full as the perl
# version, but it is neat!
# 
# FEATURES/PROBLEMS:
#   
# + Quick and easy inlining of your C code embedded in your ruby script.
# + Rudimentary automatic conversion between ruby and C basic types
#   (char, unsigned, unsigned int, char *, int, long, unsigned long).
# + Only recompiles if the C code has changed.
# + Pretends to be secure.
# + Only uses standard ruby libraries, nothing extra to download.
# + Simple as it can be. Less than 350 lines long... um... sorta simple.
# - Currently doesn't munge ruby names that aren't compatible in C (ex: a!())
# 
# SYNOPSYS:
# 
#   require "inline"
#   class MyTest
#     inline_c "
#       long factorial(int max) {
#         int i=max, result=1;
#         while (i >= 2) { result *= i--; }
#         return result;
#       }"
#   end
#   t = MyTest.new()
#   factorial_5 = t.factorial(5)
# 
# Produces:
# 
#   % rm ~/.ruby_inline/*
#   % ./example.rb 0
#   Type = Inline C , Iter = 1000000, T = 7.12203800 sec, 0.00000712 sec / iter
#   % ./example.rb 0
#   Type = Inline C , Iter = 1000000, T = 7.11633600 sec, 0.00000712 sec / iter
#   % ./example.rb 1
#   WARNING: Inline#inline is deprecated, use Module#inline_c
#   Type = Alias    , Iter = 1000000, T = 7.27398900 sec, 0.00000727 sec / iter
#   % ./example.rb 2
#   WARNING: Inline#inline is deprecated, use Module#inline_c
#   Type = InlineOld, Iter = 1000000, T = 7.10194600 sec, 0.00000710 sec / iter
#   % ./example.rb 3
#   Type = Native   , Iter = 1000000, T = 22.10488600 sec, 0.00002210 sec / iter
# 
# REQUIREMENTS:
# 
# + Ruby - 1.6.7 & 1.7.2 has been used on FreeBSD 4.6.
# + POSIX compliant system (ie pretty much any UNIX, or Cygwin on MS platforms).
# + A C compiler (the same one that compiled your ruby interpreter).
# 
# INSTALL:
# 
# + no install instructions yet.
# 
# LICENSE:
# 
# (The MIT License)
# 
# Copyright (c) 2001-2002 Ryan Davis, Zen Spider Software
# 
# Permission is hereby granted, free of charge, to any person obtaining
# a copy of this software and associated documentation files (the
# "Software"), to deal in the Software without restriction, including
# without limitation the rights to use, copy, modify, merge, publish,
# distribute, sublicense, and/or sell copies of the Software, and to
# permit persons to whom the Software is furnished to do so, subject to
# the following conditions:
# 
# The above copyright notice and this permission notice shall be
# included in all copies or substantial portions of the Software.
# 
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
# EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
# MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
# IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
# CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
# TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
# SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

### Patched inline.rb of RubyInline 2.0.0 [MS]

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
public :caller_method_name, :assert_dir_secure

$RUBY_INLINE_COMPAT = 0

# module Inline (commented out [MS])

  INLINE_VERSION = '2.0.0p0'    # 2.0.0 -> 2.0.0p0 [MS]

  def inline(args, prelude, src=nil)

#    $stderr.puts "WARNING: Inline#inline is deprecated, use Module#inline_c"

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

    myclass   = self.class
    mymethod  = self.caller_method_name
    mod_name  = "Mod_#{myclass}_#{mymethod}"
    extension = Config::CONFIG["DLEXT"]
    so_name   = "#{tmpdir}/#{mod_name}.#{extension}"

    f = File.expand_path(caller.first.split(/:/).first)	# [MS]
    unless File.file? so_name and File.mtime(f) < File.mtime(so_name) then
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

static VALUE
t_#{mymethod}(int argc, VALUE *argv, VALUE self) {
    #{src}
}

VALUE c#{mod_name};

void
Init_#{mod_name}() {
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
    end

    # Loading & Replacing w/ new method
    require "#{so_name}"
    myclass.class_eval("include #{mod_name}")
    myclass.class_eval("alias_method :old_#{mymethod}, :#{mymethod}")

    if RUBY_VERSION >= "1.7.2" then
      oldmeth = myclass.instance_method(mymethod)
      old_method_name = "old_#{mymethod}"
      myclass.instance_methods.each { |methodname|
	if methodname != old_method_name then
	  meth = myclass.instance_method(methodname)
	  if meth == oldmeth then
	    myclass.class_eval("alias_method :#{methodname}, :_#{mymethod}")
	  end
	end
      }
    else
      if $RUBY_INLINE_COMPAT == 0 then
	$stderr.puts "WARNING: ruby versions < 1.7.2 cannot inline aliased methods"
	at_exit {
	  $stderr.puts "NOTE: you ran a REALLY slow version of #{mymethod} #{$RUBY_INLINE_COMPAT} times."
	  $stderr.puts "NOTE: Upgrade to 1.7.2 or greater."
	}

      end
      $RUBY_INLINE_COMPAT += 1
      myclass.class_eval("alias_method :#{mymethod}, :_#{mymethod}")
    end    

    # Calling
    return method("_#{mymethod}").call(*args)
  end # def inline

# end # module Inline (commented out [MS])

class Module

  # FIX: this has been modified to be 1.6 specific... 1.7 has better
  # options for longs

  @@type_map = {
    'char'         => [ 'NUM2CHR',  'CHR2FIX' ],
    'unsigned'     => [ 'NUM2UINT', 'UINT2NUM' ],
    'unsigned int' => [ 'NUM2UINT', 'UINT2NUM' ],
    'char *'       => [ 'STR2CSTR', 'rb_str_new2' ],
    
    # slower versions:
    #define INT2NUM(v)
    #define NUM2INT(x)
    'int'  => [ 'FIX2INT', 'INT2FIX' ],
    
    # not sure - faster, but could overflow?
    #define FIX2LONG(x)
    #define LONG2FIX(i)
    'long' => [ 'NUM2INT', 'INT2NUM' ],

    # not sure
    #define FIX2ULONG(x)
    'unsigned long' => [ 'NUM2UINT', 'UINT2NUM' ],

    # Can't do these converters
    #define ID2SYM(x)
    #define SYM2ID(x)
    #define NUM2DBL(x)
    #define FIX2UINT(x)
  }

  def ruby2c(type)
    return @@type_map[type].first
  end
#  module_function :ruby2c

  def c2ruby(type)
    return @@type_map[type].last
  end
#  module_function :c2ruby

  def parse_signature(src)

    sig = src.dup

    # strip c-comments
    sig.gsub!(/(?:(?:\/\*)(?:(?:(?!\*\/)[\s\S])*)(?:\*\/))/, '')
    # strip cpp-comments
    sig.gsub!(/(?:\/\*(?:(?!\*\/)[\s\S])*\*\/|\/\/[^\n]*\n)/, '')
    # strip preprocessor directives
    sig.gsub!(/^\s*\#.*(\\\n.*)*/, '')
    # strip {}s
    sig.gsub!(/\{[^\}]*\}/, '{ }')
    # clean and collapse whitespace
    sig.gsub!(/\s+/, ' ')

    types = 'void|' + @@type_map.keys.map{|x| Regexp.escape(x)}.join('|')
    if /(#{types})\s*(\w+)\s*\(([^)]*)\)/ =~ sig
      return_type, function_name, arg_string = $1, $2, $3
      args = []
      arg_string.split(',').each do |arg|

	# ACK! see if we can't make this go away (FIX)
	# helps normalize into 'char * varname' form
	arg = arg.gsub(/\*/, ' * ').gsub(/\s+/, ' ').strip

	if /(#{types})\s+(\w+)\s*$/ =~ arg
	  args.push([$2, $1])
	end
      end
      return {'return' => return_type,
	  'name' => function_name,
	  'args' => args }
    end
    raise "Bad parser exception: #{sig}"
  end # def parse_signature
#  module_function :parse_signature

  def inline_c_gen(src)
    result = src.dup

    # REFACTOR: this is duplicated from above
    # strip c-comments
    result.gsub!(/(?:(?:\/\*)(?:(?:(?!\*\/)[\s\S])*)(?:\*\/))/, '')
    # strip cpp-comments
    result.gsub!(/(?:\/\*(?:(?!\*\/)[\s\S])*\*\/|\/\/[^\n]*\n)/, '')

    signature = parse_signature(src)
    function_name = signature['name']
    return_type = signature['return']

    prefix = "static VALUE t_#{function_name}(int argc, VALUE *argv, VALUE self) {\n"
    count = 0
    signature['args'].each do |arg, type|
      prefix += "#{type} #{arg} = #{ruby2c(type)}(argv[#{count}]);\n"
      count += 1
    end

    # replace the function signature (hopefully) with new signature (prefix)
    result.sub!(/[^;\/\"]+#{function_name}\s*\([^\{]+\{/, "\n" + prefix)
    result.sub!(/\A\n/, '') # strip off the \n in front in case we added it
    result.gsub!(/return\s+([^\;\}]+)/) do
      "return #{c2ruby(return_type)}(#{$1})"
    end

    return result
  end # def inline_c_gen
#  module_function :inline_c_gen

  def inline_c(src)

    rootdir = ENV['INLINEDIR'] || ENV['HOME']
    assert_dir_secure(rootdir)

    tmpdir = rootdir + "/.ruby_inline"
    unless File.directory? tmpdir then
      $stderr.puts "NOTE: creating #{tmpdir} for RubyInline" if $DEBUG
      Dir.mkdir(tmpdir, 0700)
    end
    assert_dir_secure(tmpdir)

    myclass = self
    mymethod = parse_signature(src)['name']
    mod_name = "Mod_#{myclass}_#{mymethod}"
    extension = Config::CONFIG["DLEXT"]
    so_name = "#{tmpdir}/#{mod_name}.#{extension}"  # REFACTOR

    f = File.expand_path(caller.first.split(/:/).first)	# [MS]
    unless File.file? so_name and File.mtime(f) < File.mtime(so_name) then
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

#{inline_c_gen(src)}

VALUE c#{mod_name};

void
Init_#{mod_name}() {
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
    end

    # Loading & Replacing w/ new method

    require "#{so_name}" or raise "require on #{so_name} failed"
    class_eval("include #{mod_name}")

    eval("alias_method :#{mymethod}, :_#{mymethod}")

  end # def inline_c
#  module_function :inline_c

end # Module

# v.rb ends here
