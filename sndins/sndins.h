/* sndins.h -- Sndins for Snd
 *
 * Copyright (C) 2003--2006 Michael Scholz
 *
 * Author: Michael Scholz <scholz-micha@gmx.de>
 * Created: Sat Jun 07 02:24:52 CEST 2003
 * Changed: Wed Aug 30 23:32:02 CEST 2006
 * 
 * This file is part of Sndins.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA
 *
 * Code:
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

mus_any *mus_make_fcomb   (Float scaler, int size, Float a0, Float a1);
int     mus_fcomb_p       (mus_any *ptr);
Float   mus_fcomb         (mus_any *ptr, Float input, Float ignored);

off_t   ins_fm_violin     (Float start,
			   Float dur,
			   Float freq,
			   Float amp,
			   Float fm_index,
			   Float *amp_env,
			   int amp_len,
			   Float periodic_vibrato_rate,
			   Float periodic_vibrato_amp,
			   Float random_vibrato_rate,
			   Float random_vibrato_amp,
			   Float noise_freq,
			   Float noise_amount,
			   Float ind_noise_freq,
			   Float ind_noise_amount,
			   Float amp_noise_freq,
			   Float amp_noise_amount,
			   Float *gliss_env,
			   int gliss_len,
			   Float gliss_amount,
			   Float *fm1_env,
			   int fm1_len,
			   Float *fm2_env,
			   int fm2_len,
			   Float *fm3_env,
			   int fm3_len,
			   Float fm1_rat,
			   Float fm2_rat,
			   Float fm3_rat,
			   Float fm1_index,
			   Float fm2_index,
			   Float fm3_index,
			   Float base,
			   Float degree,
			   Float distance,
			   Float reverb_amount,
			   bool index_type,
			   bool no_waveshaping,
			   mus_any *out,
			   mus_any *rev,
			   mus_interp_t mode);
off_t   ins_jc_reverb     (Float start,
			   Float dur,
			   Float volume,
			   bool low_pass,
			   bool doubled,
			   Float delay1,
			   Float delay2,
			   Float delay3,
			   Float delay4,
			   Float *amp_env,
			   int amp_len,
			   mus_any *out,
			   mus_any *rev);
off_t   ins_nrev          (Float start,
			   Float dur,
			   Float reverb_factor,
			   Float lp_coeff,
			   Float lp_out_coeff,
			   Float output_scale,
			   Float volume,
			   Float *amp_env,
			   int amp_len,
			   mus_any *out,
			   mus_any *rev);
off_t   ins_freeverb      (Float start,
			   Float dur,
			   Float room_decay,
			   Float damping,
			   Float global,
			   Float predelay,
			   Float output_gain,
			   Float scale_room_decay,
			   Float offset_room_decay,
			   Float scale_damping,
			   Float stereo_spread,
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
