/* sndins.h -- Sndins for Snd
 *
 * Copyright (C) 2003--2004 Michael Scholz
 *
 * Author: Michael Scholz <scholz-micha@gmx.de>
 * Created: Sat Jun 07 02:24:52 CEST 2003
 * Last: Fri Apr 16 15:59:58 CEST 2004
 * Ident: $Id: sndins.h,v 1.13 2004/04/16 14:54:19 mike Exp $
 * 
 * This file is part of Sndins.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * as published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
 * USA
 *
 * Code:
 */

#ifndef _SNDINS_H_
#define _SNDINS_H_

/* libtool-1.5/demo/foo.h */
/* At some point, cygwin will stop defining __CYGWIN32__, but b19 and
 * earlier do not define __CYGWIN__.  This snippit allows us to check
 * for __CYGWIN__ reliably for both current, old, and (probable) future 
 * releases.
 */
#ifdef __CYGWIN32__
#  ifndef __CYGWIN__
#    define __CYGWIN__
#  endif
#endif

/* __BEGIN_DECLS should be used at the beginning of your declarations,
   so that C++ compilers don't mangle their names.  Use __END_DECLS at
   the end of C declarations. */
#undef __BEGIN_DECLS
#undef __END_DECLS
#ifdef __cplusplus
#  define __BEGIN_DECLS extern "C" {
#  define __END_DECLS }
#else
#  define __BEGIN_DECLS /* empty */
#  define __END_DECLS /* empty */
#endif

/* PARAMS is a macro used to wrap function prototypes, so that compilers
   that don't understand ANSI C prototypes still work, and ANSI C
   compilers can issue warnings about type mismatches. */

#undef PARAMS
#if defined(__STDC__) || defined(_AIX) \
    || (defined(__mips) && defined(_SYSTYPE_SVR4)) \
    || defined(__CYGWIN__) || defined(__cplusplus)
#  define PARAMS(protos) protos
#else
#  define PARAMS(protos) ()
#endif
/* libtool-1.5/demo/foo.h */

#if HAVE_RUBY
#  define NO_MEMORY              XEN_ERROR_TYPE("memory_allocation")
#  define WITH_SOUND_INTERRUPT   XEN_ERROR_TYPE("with_sound_interrupt")
#  define WITH_PSOUND_INTERRUPT  XEN_ERROR_TYPE("with_psound_interrupt")
#else
#  define NO_MEMORY              XEN_ERROR_TYPE("memory-allocation")
#  define WITH_SOUND_INTERRUPT   XEN_ERROR_TYPE("with-sound-interrupt")
#  define WITH_PSOUND_INTERRUPT  XEN_ERROR_TYPE("with-psound-interrupt")
#endif	/* HAVE_RUBY */

#if HAVE_RUBY
#  define XEN_PUTS(C_Str, Dummy)  fputs(C_Str, stdout)
#  define XEN_DISPLAY(Val, Dummy) fputs(XEN_TO_C_STRING(Val), stdout)
#endif	/* HAVE_RUBY */

#define SC_startime  	 	       	 "startime"
#define SC_duration  	 	       	 "duration"
#define SC_frequency 	 	       	 "frequency"
#define SC_amplitude 	 	       	 "amplitude"
#define SC_degree        	       	 "degree"
#define SC_distance      	       	 "distance"
#define SC_volume        	       	 "volume"
#define SC_delay1         	       	 "delay1"
#define SC_delay2         	       	 "delay2"
#define SC_delay3         	       	 "delay3"
#define SC_delay4         	       	 "delay4"
#define SC_double                      	 "double"
#define SC_verbose       	       	 "verbose"
#define SC_base                        	 "base"
#define SC_degrees                     	 "degrees"
#define SC_damping                       "damping"
#define SC_global                        "global"
#define SC_predelay                      "predelay"
#define SC_combtuning                    "combtuning"
#define SC_allpasstuning                 "allpasstuning"
#define SC_scaler                        "scaler"
#define SC_size                          "size"
#define SC_a0                            "a0"
#define SC_a1                            "a1"
#if HAVE_RUBY
#  define SC_amp_env   	 	         "amp_env"
#  define SC_fm_index  	 	         "fm_index"
#  define SC_reverb_amount 	         "reverb_amount"
#  define SC_low_pass      	         "low_pass"
#  define SC_periodic_vibrato_rate       "periodic_vibrato_rate"
#  define SC_periodic_vibrato_amplitude  "periodic_vibrato_amplitude"
#  define SC_random_vibrato_rate         "random_vibrato_rate"
#  define SC_random_vibrato_amplitude    "random_vibrato_amplitude"
#  define SC_noise_freq                  "noise_freq"
#  define SC_noise_amount                "noise_amount"
#  define SC_ind_noise_freq              "ind_noise_freq"
#  define SC_ind_noise_amount            "ind_noise_amount"
#  define SC_amp_noise_freq              "amp_noise_freq"
#  define SC_amp_noise_amount            "amp_noise_amount"
#  define SC_gliss_env                   "gliss_env"
#  define SC_glissando_amount            "glissando_amount"
#  define SC_fm1_env                     "fm1_env"
#  define SC_fm2_env                     "fm2_env"
#  define SC_fm3_env                     "fm3_env"
#  define SC_fm1_rat                     "fm1_rat"
#  define SC_fm2_rat                     "fm2_rat"
#  define SC_fm3_rat                     "fm3_rat"
#  define SC_fm1_index                   "fm1_index"
#  define SC_fm2_index                   "fm2_index"
#  define SC_fm3_index                   "fm3_index"
#  define SC_index_type                  "index_type"
#  define SC_no_waveshaping              "no_waveshaping"
#  define SC_reverb_factor               "reverb_factor"
#  define SC_lp_coeff                    "lp_coeff"
#  define SC_lp_out_coeff                "lp_out_coeff"
#  define SC_output_scale                "output_scale"
#  define SC_room_decay                  "room_decay"
#  define SC_output_gain                 "output_gain"
#  define SC_output_mixer                "output_mixer"
#  define SC_scale_room_decay            "scale_room_decay"
#  define SC_offset_room_decay           "offset_room_decay"
#  define SC_scale_damping               "scale_dumping"
#  define SC_stereo_spread               "stereo_spread"
#else
#  define SC_amp_env   	                 "amp-env"
#  define SC_fm_index  	 	         "fm-index"
#  define SC_reverb_amount 	         "reverb-amount"
#  define SC_low_pass      	         "low-pass"
#  define SC_periodic_vibrato_rate       "periodic-vibrato-rate"
#  define SC_periodic_vibrato_amplitude  "periodic-vibrato-amplitude"
#  define SC_random_vibrato_rate         "random-vibrato-rate"
#  define SC_random_vibrato_amplitude    "random-vibrato-amplitude"
#  define SC_noise_freq                  "noise-freq"
#  define SC_noise_amount                "noise-amount"
#  define SC_ind_noise_freq              "ind-noise-freq"
#  define SC_ind_noise_amount            "ind-noise-amount"
#  define SC_amp_noise_freq              "amp-noise-freq"
#  define SC_amp_noise_amount            "amp-noise-amount"
#  define SC_gliss_env                   "gliss-env"
#  define SC_glissando_amount            "glissando-amount"
#  define SC_fm1_env                     "fm1-env"
#  define SC_fm2_env                     "fm2-env"
#  define SC_fm3_env                     "fm3-env"
#  define SC_fm1_rat                     "fm1-rat"
#  define SC_fm2_rat                     "fm2-rat"
#  define SC_fm3_rat                     "fm3-rat"
#  define SC_fm1_index                   "fm1-index"
#  define SC_fm2_index                   "fm2-index"
#  define SC_fm3_index                   "fm3-index"
#  define SC_index_type                  "index-type"
#  define SC_no_waveshaping              "no-waveshaping"
#  define SC_reverb_factor               "reverb-factor"
#  define SC_lp_coeff                    "lp-coeff"
#  define SC_lp_out_coeff                "lp-out-coeff"
#  define SC_output_scale                "output-scale"
#  define SC_room_decay                  "room-decay"
#  define SC_output_gain                 "output-gain"
#  define SC_output_mixer                "output-mixer"
#  define SC_scale_room_decay            "scale-room-decay"
#  define SC_offset_room_decay           "offset-room-decay"
#  define SC_scale_damping               "scale-dumping"
#  define SC_stereo_spread               "stereo-spread"
#endif	/* HAVE_RUBY */

__BEGIN_DECLS

mus_any *mus_make_fcomb PARAMS((Float scaler, int size, Float a0, Float a1));
int mus_fcomb_p PARAMS((mus_any *ptr));
Float mus_fcomb PARAMS((mus_any *ptr, Float input, Float ignored));

void Init_libsndins PARAMS((void));
void Init_sndins PARAMS((void));
void init_libsndins PARAMS((void));
void init_sndins PARAMS((void));

__END_DECLS

#endif /* _SNDINS_H_ */

/*
 * sndins.h ends here
 */
