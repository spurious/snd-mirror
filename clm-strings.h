#ifndef CLM_STRINGS_H
#define CLM_STRINGS_H

/* there's some inconsistency about the mus- prefix: mus-interp-* but *-window etc */

#define S_all_pass                   "all-pass"
#define S_all_pass_p                 "all-pass?"
#define S_amplitude_modulate         "amplitude-modulate"
#define S_array_interp               "array-interp"
#define S_asymmetric_fm              "asymmetric-fm"
#define S_asymmetric_fm_p            "asymmetric-fm?"
#define S_average                    "average"
#define S_average_p                  "average?"
#define S_bartlett_window            "bartlett-window"
#define S_blackman2_window           "blackman2-window"
#define S_blackman3_window           "blackman3-window"
#define S_blackman4_window           "blackman4-window"
#define S_cauchy_window              "cauchy-window"
#define S_clear_array                "clear-array"
#define S_clear_sincs                "clear-sincs"
#define S_clm_print                  "clm-print"
#define S_clm_table_size             "clm-table-size"
#define S_comb                       "comb"
#define S_comb_p                     "comb?"
#define S_connes_window              "connes-window"
#define S_continue_sample_to_file    "continue-sample->file"
#define S_continue_frame_to_file     "continue-frame->file"
#define S_contrast_enhancement       "contrast-enhancement"
#define S_convolution                "convolution"
#define S_convolve                   "convolve"
#define S_convolve_files             "convolve-files"
#define S_convolve_p                 "convolve?"
#define S_db_to_linear               "db->linear"
#define S_degrees_to_radians         "degrees->radians"
#define S_delay                      "delay"
#define S_delay_tick                 "delay-tick"
#define S_delay_p                    "delay?"
#define S_dolph_chebyshev_window     "dolph-chebyshev-window"
#define S_dot_product                "dot-product"
#define S_env                        "env"
#define S_env_interp                 "env-interp"
#define S_env_p                      "env?"
#define S_exponential_window         "exponential-window"
#define S_file_to_frame              "file->frame"
#define S_file_to_frame_p            "file->frame?"
#define S_file_to_sample             "file->sample"
#define S_file_to_sample_p           "file->sample?"
#define S_filter                     "filter"
#define S_filter_p                   "filter?"
#define S_fir_filter                 "fir-filter"
#define S_fir_filter_p               "fir-filter?"
#define S_formant                    "formant"
#define S_formant_bank               "formant-bank"
#define S_formant_p                  "formant?"
#define S_frame_to_file              "frame->file"
#define S_frame_to_file_p            "frame->file?"
#define S_frame_to_frame             "frame->frame"
#define S_frame_to_list              "frame->list"
#define S_frame_to_sample            "frame->sample"
#if HAVE_RUBY
  #define S_frame_add                "frame_add"
  #define S_frame_multiply           "frame_multiply"
#else
  #define S_frame_add                "frame+"
  #define S_frame_multiply           "frame*"
#endif
#define S_frame_p                    "frame?"
#define S_frame_ref                  "frame-ref"
#define S_frame_set                  "frame-set!"
#define S_gaussian_window            "gaussian-window"
#define S_granulate                  "granulate"
#define S_granulate_p                "granulate?"
#define S_hamming_window             "hamming-window"
#define S_hann_window                "hann-window"
#define S_hann_poisson_window        "hann-poisson-window"
#define S_hz_to_radians              "hz->radians"
#define S_iir_filter                 "iir-filter"
#define S_iir_filter_p               "iir-filter?"
#define S_in_any                     "in-any"
#define S_ina                        "ina"
#define S_inb                        "inb"
#define S_kaiser_window              "kaiser-window"
#define S_linear_to_db               "linear->db"
#define S_locsig                     "locsig"
#define S_locsig_p                   "locsig?"
#define S_locsig_ref                 "locsig-ref"
#define S_locsig_reverb_ref          "locsig-reverb-ref"
#define S_locsig_reverb_set          "locsig-reverb-set!"
#define S_locsig_set                 "locsig-set!"
#define S_locsig_type                "locsig-type"
#define S_make_all_pass              "make-all-pass"
#define S_make_asymmetric_fm         "make-asymmetric-fm"
#define S_make_average               "make-average"
#define S_make_comb                  "make-comb"
#define S_make_convolve              "make-convolve"
#define S_make_delay                 "make-delay"
#define S_make_env                   "make-env"
#define S_make_fft_window            "make-fft-window"
#define S_make_file_to_frame         "make-file->frame"
#define S_make_file_to_sample        "make-file->sample"
#define S_make_filter                "make-filter"
#define S_make_fir_coeffs            "make-fir-coeffs"
#define S_make_fir_filter            "make-fir-filter"
#define S_make_formant               "make-formant"
#define S_make_frame                 "make-frame"
#define S_make_frame_to_file         "make-frame->file"
#define S_make_granulate             "make-granulate"
#define S_make_iir_filter            "make-iir-filter"
#define S_make_locsig                "make-locsig"
#define S_make_mixer                 "make-mixer"
#define S_make_notch                 "make-notch"
#define S_make_one_pole              "make-one-pole"
#define S_make_one_zero              "make-one-zero"
#define S_make_oscil                 "make-oscil"
#define S_make_phase_vocoder         "make-phase-vocoder"
#define S_make_polyshape             "make-polyshape"
#define S_make_ppolar                "make-ppolar"
#define S_make_pulse_train           "make-pulse-train"
#define S_make_rand                  "make-rand"
#define S_make_rand_interp           "make-rand-interp"
#define S_make_readin                "make-readin"
#define S_make_sample_to_file        "make-sample->file"
#define S_make_sawtooth_wave         "make-sawtooth-wave"
#define S_make_scalar_mixer          "make-scalar-mixer"
#define S_make_sine_summation        "make-sine-summation"
#define S_make_square_wave           "make-square-wave"
#define S_make_src                   "make-src"
#define S_make_ssb_am                "make-ssb-am"
#define S_make_sum_of_cosines        "make-sum-of-cosines"
#define S_make_sum_of_sines          "make-sum-of-sines"
#define S_make_table_lookup          "make-table-lookup"
#define S_make_triangle_wave         "make-triangle-wave"
#define S_make_two_pole              "make-two-pole"
#define S_make_two_zero              "make-two-zero"
#define S_make_wave_train            "make-wave-train"
#define S_make_waveshape             "make-waveshape"
#define S_make_zpolar                "make-zpolar"
#if HAVE_RUBY
  #define S_mixer_multiply           "mixer_multiply"
  #define S_mixer_add                "mixer_add"
#else
  #define S_mixer_multiply           "mixer*"
  #define S_mixer_add                "mixer+"
#endif
#define S_mixer_p                    "mixer?"
#define S_mixer_ref                  "mixer-ref"
#define S_mixer_scale                "mixer-scale"
#define S_mixer_set                  "mixer-set!"
#define S_move_locsig                "move-locsig"
#define S_multiply_arrays            "multiply-arrays"
#define S_mus_apply                  "mus-apply"
#define S_mus_array_print_length     "mus-array-print-length"
#define S_mus_channel                "mus-channel"
#define S_mus_channels               "mus-channels"
#define S_mus_close                  "mus-close"
#define S_mus_cosines                "mus-cosines"
#define S_mus_data                   "mus-data"
#define S_mus_describe               "mus-describe"
#define S_mus_feedback               "mus-feedback"
#define S_mus_feedforward            "mus-feedforward"
#define S_mus_fft                    "mus-fft"
#define S_mus_file_buffer_size       "mus-file-buffer-size"
#define S_mus_file_name              "mus-file-name"
#define S_mus_formant_radius         "mus-formant-radius"
#define S_mus_frequency              "mus-frequency"
#define S_mus_generator_p            "mus-generator?"
#define S_mus_hop                    "mus-hop"
#define S_mus_increment              "mus-increment"
#define S_mus_input_p                "mus-input?"
#define S_mus_interpolate            "mus-interpolate"
#define S_mus_interp_all_pass        "mus-interp-all-pass"
#define S_mus_interp_bezier          "mus-interp-bezier"
#define S_mus_interp_hermite         "mus-interp-hermite"
#define S_mus_interp_lagrange        "mus-interp-lagrange"
#define S_mus_interp_linear          "mus-interp-linear"
#define S_mus_interp_none            "mus-interp-none"
#define S_mus_interp_sinusoidal      "mus-interp-sinusoidal"
#define S_mus_interp_type            "mus-interp-type"
#define S_mus_length                 "mus-length"
#define S_mus_location               "mus-location"
#define S_mus_mix                    "mus-mix"
#define S_mus_name                   "mus-name"
#define S_mus_offset                 "mus-offset"
#define S_mus_order                  "mus-order"
#define S_mus_output_p               "mus-output?"
#define S_mus_phase                  "mus-phase"
#define S_mus_ramp                   "mus-ramp"
#define S_mus_rand_seed              "mus-rand-seed"
#define S_mus_random                 "mus-random"
#define S_mus_reset                  "mus-reset"
#define S_mus_run                    "mus-run"
#define S_mus_scaler                 "mus-scaler"
#define S_mus_set_formant_radius_and_frequency "mus-set-formant-radius-and-frequency"
#define S_mus_srate                  "mus-srate"
#define S_mus_width                  "mus-width"
#define S_mus_xcoeff                 "mus-xcoeff"
#define S_mus_xcoeffs                "mus-xcoeffs"
#define S_mus_ycoeff                 "mus-ycoeff"
#define S_mus_ycoeffs                "mus-ycoeffs"
#define S_notch                      "notch"
#define S_notch_p                    "notch?"
#define S_one_pole                   "one-pole"
#define S_one_pole_p                 "one-pole?"
#define S_one_zero                   "one-zero"
#define S_one_zero_p                 "one-zero?"
#define S_oscil                      "oscil"
#define S_oscil_p                    "oscil?"
#define S_out_any                    "out-any"
#define S_outa                       "outa"
#define S_outb                       "outb"
#define S_outc                       "outc"
#define S_outd                       "outd"
#define S_partials_to_polynomial     "partials->polynomial"
#define S_partials_to_wave           "partials->wave"
#define S_partials_to_waveshape      "partials->waveshape"
#define S_parzen_window              "parzen-window"
#define S_phase_vocoder              "phase-vocoder"
#define S_phase_vocoder_p            "phase-vocoder?"
#define S_phase_partials_to_wave     "phase-partials->wave"
#define S_poisson_window             "poisson-window"
#define S_polar_to_rectangular       "polar->rectangular"
#define S_polynomial                 "polynomial"
#define S_polyshape                  "polyshape"
#define S_polyshape_p                "polyshape?"
#define S_pulse_train                "pulse-train"
#define S_pulse_train_p              "pulse-train?"
#define S_phase_vocoder_amp_increments   "phase-vocoder-amp-increments"
#define S_phase_vocoder_amps         "phase-vocoder-amps"
#define S_phase_vocoder_freqs        "phase-vocoder-freqs"
#define S_phase_vocoder_outctr       "phase-vocoder-outctr"
#define S_phase_vocoder_phase_increments "phase-vocoder-phase-increments"
#define S_phase_vocoder_phases       "phase-vocoder-phases"
#define S_phase_vocoder_set_outctr   "phase-vocoder-set-outctr"
#define S_radians_to_degrees         "radians->degrees"
#define S_radians_to_hz              "radians->hz"
#define S_rand                       "rand"
#define S_rand_interp                "rand-interp"
#define S_rand_interp_p              "rand-interp?"
#define S_rand_p                     "rand?"
#define S_readin                     "readin"
#define S_readin_p                   "readin?"
#define S_rectangular_to_polar       "rectangular->polar"
#define S_rectangular_window         "rectangular-window"
#define S_riemann_window             "riemann-window"
#define S_ring_modulate              "ring-modulate"
#define S_sample_to_file             "sample->file"
#define S_sample_to_file_p           "sample->file?"
#define S_sample_to_frame            "sample->frame"
#define S_samples_to_seconds         "samples->seconds"
#define S_sawtooth_wave              "sawtooth-wave"
#define S_sawtooth_wave_p            "sawtooth-wave?"
#define S_seconds_to_samples         "seconds->samples"
#define S_sine_bank                  "sine-bank"
#define S_sine_summation             "sine-summation"
#define S_sine_summation_p           "sine-summation?"
#define S_spectrum                   "spectrum"
#define S_square_wave                "square-wave"
#define S_square_wave_p              "square-wave?"
#define S_src                        "src"
#define S_src_p                      "src?"
#define S_ssb_am                     "ssb-am"
#define S_ssb_am_p                   "ssb-am?"
#define S_sum_of_cosines             "sum-of-cosines"
#define S_sum_of_cosines_p           "sum-of-cosines?"
#define S_sum_of_sines               "sum-of-sines"
#define S_sum_of_sines_p             "sum-of-sines?"
#define S_table_lookup               "table-lookup"
#define S_table_lookup_p             "table-lookup?"
#define S_tap                        "tap"
#define S_triangle_wave              "triangle-wave"
#define S_triangle_wave_p            "triangle-wave?"
#define S_tukey_window               "tukey-window"
#define S_two_pole                   "two-pole"
#define S_two_pole_p                 "two-pole?"
#define S_two_zero                   "two-zero"
#define S_two_zero_p                 "two-zero?"
#define S_wave_train                 "wave-train"
#define S_wave_train_p               "wave-train?"
#define S_waveshape                  "waveshape"
#define S_waveshape_p                "waveshape?"
#define S_welch_window               "welch-window"

#endif
