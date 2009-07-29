/* sndins.h -- Sndins for Snd/CLM
 *
 * Copyright (c) 2003--2009 Michael Scholz <mi-scholz@users.sourceforge.net>
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 * 
 */

#ifndef _SNDINS_H_
#define _SNDINS_H_

#ifndef XEN_PROVIDED_P
# if HAVE_SCHEME
#  define XEN_PROVIDED_P(feature)					\
  XEN_TO_C_BOOLEAN(XEN_MEMBER(C_STRING_TO_XEN_SYMBOL(feature),		\
			      XEN_NAME_AS_C_STRING_TO_VALUE("*features*")))
# elif HAVE_RUBY
#  define XEN_PROVIDED_P(feature)       XEN_TO_C_BOOLEAN(rb_provided(feature))
# elif HAVE_FORTH
#  define XEN_PROVIDED_P(feature)       fth_provided_p(feature)
# else
#  define XEN_PROVIDED_P(feature)       false
# endif
#endif

#define SC_startime  	 	       	"startime"
#define SC_duration  	 	       	"duration"
#define SC_frequency 	 	       	"frequency"
#define SC_amplitude 	 	       	"amplitude"
#define SC_degree        	       	"degree"
#define SC_distance      	       	"distance"
#define SC_volume        	       	"volume"
#define SC_delay1         	       	"delay1"
#define SC_delay2         	       	"delay2"
#define SC_delay3         	       	"delay3"
#define SC_delay4         	       	"delay4"
#define SC_doubled                      "doubled"
#define SC_base                        	"base"
#define SC_damping                      "damping"
#define SC_global                       "global"
#define SC_predelay                     "predelay"
#define SC_combtuning                   "combtuning"
#define SC_allpasstuning                "allpasstuning"
#define SC_scaler                       "scaler"
#define SC_size                         "size"
#define SC_a0                           "a0"
#define SC_a1                           "a1"
#if HAVE_RUBY
# define SC_amp_env   	 	        "amp_env"
# define SC_fm_index  	 	        "fm_index"
# define SC_reverb_amount 	        "reverb_amount"
# define SC_low_pass      	        "low_pass"
# define SC_periodic_vibrato_rate       "periodic_vibrato_rate"
# define SC_periodic_vibrato_amplitude  "periodic_vibrato_amplitude"
# define SC_random_vibrato_rate         "random_vibrato_rate"
# define SC_random_vibrato_amplitude    "random_vibrato_amplitude"
# define SC_noise_freq                  "noise_freq"
# define SC_noise_amount                "noise_amount"
# define SC_ind_noise_freq              "ind_noise_freq"
# define SC_ind_noise_amount            "ind_noise_amount"
# define SC_amp_noise_freq              "amp_noise_freq"
# define SC_amp_noise_amount            "amp_noise_amount"
# define SC_gliss_env                   "gliss_env"
# define SC_glissando_amount            "glissando_amount"
# define SC_fm1_env                     "fm1_env"
# define SC_fm2_env                     "fm2_env"
# define SC_fm3_env                     "fm3_env"
# define SC_fm1_rat                     "fm1_rat"
# define SC_fm2_rat                     "fm2_rat"
# define SC_fm3_rat                     "fm3_rat"
# define SC_fm1_index                   "fm1_index"
# define SC_fm2_index                   "fm2_index"
# define SC_fm3_index                   "fm3_index"
# define SC_index_type                  "index_type"
# define SC_no_waveshaping              "no_waveshaping"
# define SC_reverb_factor               "reverb_factor"
# define SC_lp_coeff                    "lp_coeff"
# define SC_lp_out_coeff                "lp_out_coeff"
# define SC_output_scale                "output_scale"
# define SC_room_decay                  "room_decay"
# define SC_output_gain                 "output_gain"
# define SC_output_mixer                "output_mixer"
# define SC_scale_room_decay            "scale_room_decay"
# define SC_offset_room_decay           "offset_room_decay"
# define SC_scale_damping               "scale_dumping"
# define SC_stereo_spread               "stereo_spread"
#else  /* !HAVE_RUBY */
# define SC_amp_env   	                "amp-env"
# define SC_fm_index  	 	        "fm-index"
# define SC_reverb_amount 	        "reverb-amount"
# define SC_low_pass      	        "low-pass"
# define SC_periodic_vibrato_rate       "periodic-vibrato-rate"
# define SC_periodic_vibrato_amplitude  "periodic-vibrato-amplitude"
# define SC_random_vibrato_rate         "random-vibrato-rate"
# define SC_random_vibrato_amplitude    "random-vibrato-amplitude"
# define SC_noise_freq                  "noise-freq"
# define SC_noise_amount                "noise-amount"
# define SC_ind_noise_freq              "ind-noise-freq"
# define SC_ind_noise_amount            "ind-noise-amount"
# define SC_amp_noise_freq              "amp-noise-freq"
# define SC_amp_noise_amount            "amp-noise-amount"
# define SC_gliss_env                   "gliss-env"
# define SC_glissando_amount            "glissando-amount"
# define SC_fm1_env                     "fm1-env"
# define SC_fm2_env                     "fm2-env"
# define SC_fm3_env                     "fm3-env"
# define SC_fm1_rat                     "fm1-rat"
# define SC_fm2_rat                     "fm2-rat"
# define SC_fm3_rat                     "fm3-rat"
# define SC_fm1_index                   "fm1-index"
# define SC_fm2_index                   "fm2-index"
# define SC_fm3_index                   "fm3-index"
# define SC_index_type                  "index-type"
# define SC_no_waveshaping              "no-waveshaping"
# define SC_reverb_factor               "reverb-factor"
# define SC_lp_coeff                    "lp-coeff"
# define SC_lp_out_coeff                "lp-out-coeff"
# define SC_output_scale                "output-scale"
# define SC_room_decay                  "room-decay"
# define SC_output_gain                 "output-gain"
# define SC_output_mixer                "output-mixer"
# define SC_scale_room_decay            "scale-room-decay"
# define SC_offset_room_decay           "offset-room-decay"
# define SC_scale_damping               "scale-dumping"
# define SC_stereo_spread               "stereo-spread"
#endif	/* !HAVE_RUBY */

#undef __BEGIN_DECLS
#undef __END_DECLS
#ifdef __cplusplus
# define __BEGIN_DECLS extern "C" {
# define __END_DECLS }
#else
# define __BEGIN_DECLS
# define __END_DECLS
#endif

__BEGIN_DECLS

mus_any *mus_make_fcomb   (mus_float_t scaler, int size, mus_float_t a0, mus_float_t a1);
int         mus_fcomb_p   (mus_any *ptr);
mus_float_t mus_fcomb     (mus_any *ptr, mus_float_t input, mus_float_t ignored);

off_t   ins_fm_violin     (mus_float_t start,
			   mus_float_t dur,
			   mus_float_t freq,
			   mus_float_t amp,
			   mus_float_t fm_index,
			   mus_float_t *amp_env,
			   int amp_len,
			   mus_float_t periodic_vibrato_rate,
			   mus_float_t periodic_vibrato_amp,
			   mus_float_t random_vibrato_rate,
			   mus_float_t random_vibrato_amp,
			   mus_float_t noise_freq,
			   mus_float_t noise_amount,
			   mus_float_t ind_noise_freq,
			   mus_float_t ind_noise_amount,
			   mus_float_t amp_noise_freq,
			   mus_float_t amp_noise_amount,
			   mus_float_t *gliss_env,
			   int gliss_len,
			   mus_float_t gliss_amount,
			   mus_float_t *fm1_env,
			   int fm1_len,
			   mus_float_t *fm2_env,
			   int fm2_len,
			   mus_float_t *fm3_env,
			   int fm3_len,
			   mus_float_t fm1_rat,
			   mus_float_t fm2_rat,
			   mus_float_t fm3_rat,
			   mus_float_t fm1_index,
			   mus_float_t fm2_index,
			   mus_float_t fm3_index,
			   mus_float_t base,
			   mus_float_t degree,
			   mus_float_t distance,
			   mus_float_t reverb_amount,
			   bool index_type,
			   bool no_waveshaping,
			   mus_any *out,
			   mus_any *rev,
			   mus_interp_t mode);
off_t   ins_jc_reverb     (mus_float_t start,
			   mus_float_t dur,
			   mus_float_t volume,
			   bool low_pass,
			   bool doubled,
			   mus_float_t delay1,
			   mus_float_t delay2,
			   mus_float_t delay3,
			   mus_float_t delay4,
			   mus_float_t *amp_env,
			   int amp_len,
			   mus_any *out,
			   mus_any *rev);
off_t   ins_nrev          (mus_float_t start,
			   mus_float_t dur,
			   mus_float_t reverb_factor,
			   mus_float_t lp_coeff,
			   mus_float_t lp_out_coeff,
			   mus_float_t output_scale,
			   mus_float_t volume,
			   mus_float_t *amp_env,
			   int amp_len,
			   mus_any *out,
			   mus_any *rev);
off_t   ins_freeverb      (mus_float_t start,
			   mus_float_t dur,
			   mus_float_t room_decay,
			   mus_float_t damping,
			   mus_float_t global,
			   mus_float_t predelay,
			   mus_float_t output_gain,
			   mus_float_t scale_room_decay,
			   mus_float_t offset_room_decay,
			   mus_float_t scale_damping,
			   mus_float_t stereo_spread,
			   int *combtuning,
			   int comb_len,
			   int *allpasstuning,
			   int all_len,
			   mus_any *output_mixer,
			   mus_any *out,
			   mus_any *rev);

void Init_sndins(void);

__END_DECLS

#endif /* _SNDINS_H_ */

/*
 * sndins.h ends here
 */
