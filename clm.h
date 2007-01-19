#ifndef CLM_H
#define CLM_H

#define MUS_VERSION 3
#define MUS_REVISION 31
#define MUS_DATE "12-Dec-06"

/*
 * 12-Dec:     removed mus_make_frame|mixer_with_data.
 * 27-Nov:     move-sound array access parallel to locsig.
 * 22-Nov:     had to add non-backwards-compatible reverb chans arg to mus_make_locsig.
 * 21-Nov:     mus_float_equal_fudge_factor, mus_arrays_are_equal.
 * 30-July:    renamed average to moving_average.
 * 28-July:    renamed make_ppolar and make_zpolar to make_two_pole|zero_from_radius_and_frequency.
 *             added mus_scaler and mus_frequency methods for two_pole and two_zero.
 * 21-July:    removed mus_wrapper field -- old way can't work since we need the original XEN object.
 * 3-July:     mus_move_sound (dlocsig) generator.
 *             changed return type of mus_locsig to float.
 * 28-June:    mus_filtered_comb generator.
 * 8-May:      mus_apply now takes 3 args: gen, two doubles (rather than bug-prone varargs).
 * 1-Mar-06:   granulate now has a local random number seed (settable via the mus-location method).
 * --------
 * 20-Dec:     samaraki and ultraspherical windows.
 *               this required a non-backwards-compatible additional argument in mus_make_fft_window_with_window.
 * 1-Nov:      mus_filter_set_x|ycoeffs, mus_filter_set_order (needed by Snd).
 * 1-May:      mus-scaler|feedback ok with delay and average.
 * 18-Apr:     mus_set_environ.
 * 11-Apr:     mus_mixer|frame_offset, mus_frame_scale (for higher level generic functions).
 * 23-Mar:     frame_to_frame arg interpretation changed.
 * 21-Mar:     mus_make_readin|file_to_sample|file_to_frame_with_buffer_size.
 * 16-Mar:     polyshape generator (waveshaper as polynomial + oscil)
 *             mus_chebyshev_first|second_kind.
 *             mus_partials_to_waveshape no longer normalizes the partials.
 * 18-Feb:     mus_interpolate.
 * 14-Feb:     deprecated mus_restart_env and mus_clear_filter_state.
 * 7-Feb-05:   mus_reset method, replaces mus_restart_env and mus_clear_filter_state.
 * --------
 * 20-Dec:     changed "jitter" handling if hop < .05 in granulate.
 * 15-Dec:     mus_generator? for type checks (clm2xen).
 * 14-Dec:     mus_env_linear.
 * 11-Sep:     removed buffer generator.
 * 6-Sep:      removed mus_oscil_bank, mus_bank.
 * 24-Aug:     removed mus_inspect method -- overlaps mus_describe and is useless given gdb capabilities.
 * 27-July:    mus_granulate_with_editor and mus_phase_vocoder_with_editors.
 * 21-July:    edit-func as run-time arg to granulate (for CL/clm compatibility)
 * 19-July:    clm 3.0!
 *             deprecated mus_ina|b, mus-outa|b|c|d.
 *             mus_make_frame_to_file_with_comment, mus_mixer_scale, mus_make_frame|mixer_with_data.
 *             mus_make_scalar_mixer, mus_mixer_add, mus_continue_frame_to_file.
 *             changed pv_* to phase_vocoder_*
 * 28-June:    ssb_am + added fm arg (ssb_am_1 is the previous form).
 * 21-June:    wrapper method.
 * 14-June:    ssb_am generator.
 *             deprecated mus-a*|b*, replaced by mus-x|ycoeff.
 * 9-June:     mus_edot_product.
 * 7-June:     removed mus-x*|y* generic functions.
 * 24-May:     distribution arg to make-rand, make-rand-interp.
 * 11-May:     type arg to mus_make_table_lookup|wave_train, MUS_INTERP_NONE, MUS_INTERP_HERMITE.
 *             mus-interp-type.
 * 10-May:     changed MUS_LINEAR and MUS_SINUSOIDAL to MUS_INTERP_LINEAR and MUS_INTERP_SINUSOIDAL.
 *             mus-linear renamed mus-interp-linear, mus-sinusoidal renamed mus-interp-sinusoidal.
 *             added type arg to mus_make_delay|all_pass|comb|notch.
 *             added mus_delay_tick, all-pass delay line interpolation.
 * 3-May:      envelope arg to make-rand and make-rand-interp to give any arbitrary random number distribution.
 *             added mus_make_rand_with_distribution and mus_make_rand_interp_with_distribution.
 *             rand/rand-interp mus-data returns distribution (weight) function, mus-length its length.
 *             locsig mus-data returns output scalers, mus-xcoeffs returns reverb scalers
 * 26-Apr:     mus_sum_of_sines changed to mus_sine_bank.
 *             new mus_sum_of_sines parallels mus_sum_of_cosines.
 *             deprecated mus_sin.
 * 14-Apr:     changed "2" to "_to_" in several function names.
 * 12-Apr:     mus_average, mus_average_p, mus_make_average.
 * 17-Mar:     edit function added to mus_granulate.
 *             replaced MUS_DATA_POSITION with MUS_DATA_WRAPPER.
 * 22-Jan:     various "environ" variables renamed for Windows' benefit.
 * 5-Jan-04:   env_interp bugfix.
 * --------
 * 29-Sep:     removed length arg from spectrum in clm2xen.
 * 24-Aug:     changed mus_length|ramp|hop type to off_t.
 * 21-Aug:     export MUS_INPUT and friends (needed for specialized INA handlers).
 * 11-Aug:     int -> bool.
 * 7-Aug:      removed mus_type.
 * 20-July:    more run methods.
 * 15-July:    linear->dB check for 0.0 arg.
 * 27-June:    mus_samples_to_seconds and mus_seconds_to_samples.
 * 9-June:     mus_mix_with_reader_and_writer.
 * 27-May:     bugfix: interpolating all-pass ("zall-pass") had an extra delay.
 * 25-Apr:     mus_spectrum and mus_convolution now return Float*.
 * 9-Apr:      removed MUS_HANNING_WINDOW (use MUS_HANN_WINDOW).
 * 3-Mar:      mus_delay_line_p for tap error checking.
 * 27-Feb:     mus_length for env -> original duration in samples.
 * 21-Feb:     mus_set_cosines added, mus_cosines moved to hop slot.
 *             mus_[set_]x1/x2/y1/y2.
 * 10-Feb:     mus_file_name moved into the mus_input|output structs.
 *             folded mus_input|output into mus_any.
 *             moved mus_frame|mixer declarations into clm.c.
 *             all mus_input|output|frame|mixer pointers->mus_any.
 *             all method void pointers->mus_any.
 * 7-Feb:      split strings out of clm2xen.c into clm-strings.h.
 * 3-Feb:      mus_offset for envs, mus_width for square_wave et al.
 *             new core class fields(10) for various methods.
 * 7-Jan-03:   mus_src with very large sr_change segfault bugfix.
 * --------
 * 17-Dec:     mus_env_offset|initial_power for Snd exp env optimizations.
 * 13-Sep:     mus_frandom and mus_irandom(for Snd optimizer).
 * 19-Aug:     changed internal phase-vocoder array accessor names
 * 13-Aug:     set!(*-ref) for frame, locsig, mixer, locsig-reverb.
 * 29-Jul:     various *_1 cases for the optimizer.
 * 15-Jul:     mus_continue_sample2file.
 * 10-Jul:     mus_file_name.
 * 7-Jun:      fftw support added(mus_fftw).
 * 31-May:     changed mus_any_class.
 * 3-May:      many int->off_t changes for large files.
 * 8-Apr:      off-by-1 env bug(Lisp/C are now identical), env_interp of exp env beyond end bugfix.
 * 1-Apr:      sine-summation n=0 bugfix.
 * 27-Mar:     negative degree locsig bugfix.
 * 18-Mar:     mus_move_locsig.
 * 15-Mar:     n-chan locsig(and reverb scalers), 'type' arg to mus_make_locsig.
 * 6-Mar:      mus_scaler in asymmetric-fm now refers to the "r" parameter, "a" in sine-summation.
 * 5-Mar:      dumb typo in asymmetric-fm generator fixed.
 * 19-Feb:     buffer reallocation redundant free bugfix.
 * 25-Jan-02:  mus_increment of env returns base.
 * --------
 * 10-Dec:     add outctr calls, phase-vocoder bugfixes, thanks to Scott Wilson.
 * 21-Oct:     fill in some set-data methods.
 * 1-Sep:      mus_polar2rectangular.
 * 6-July:     scm -> xen.
 * 26-May:     mus_rand_seed.
 * 22-May:     locsig reverb distance calc was upside down.
 * 18-May:     mus_describe and mus_inspect returned string should not be freed any more.
 * 7-May:      filled in some leftover equal_p methods.
 * 1-Apr:      mus_make_file2sample_with_comment and mus_length for file->sample/sample->file.
 *             mus_file_buffer_size.
 * 26-Mar:     extended_type field added to mus_any_class for more robust type checking.
 * 16-Mar:     mus_phase of env -> current_value.
 * 28-Feb:     added mus_position(currently only for envs).
 * 8-Feb:      clm2scm.h.
 * 24-Jan:     mus-bank in clm2scm.
 * 5-Jan:      clm2scm gens are applicable.
 * 4-Jan:      mus_bank.
 * 2-Jan-01:   mus_run method.
 * --------
 * 28-Dec:     mus_clear_filter_state and other minor tweaks for Snd.
 * 28-Nov:     Dolph-Chebyshev window(under HAVE_GSL flag -- needs complex trig support).
 * 8-Nov:      mus_clear_sinc_tables.
 * 12-Oct:     mus_formant_bank takes one input(can't remember why I had an array here)
 * 27-Sep:     mus_array_interp bugfix(imitates mus.lisp now).
 * 18-Sep:     clm now assumes it's used as a part of sndlib.
 * 11-Sep:     generalized set! to generic functions in clm2scm.c.
 * 31-Aug:     changed formant field setters(thanks to Anders Vinjar).
 * 10-Aug:     removed built-in setf support(clm2scm.c).
 * 31-Jul:     mus_granulate tries to protect against illegal length and ramp values.
 * 24-Jul:     mus_make_fir_coeffs.
 * 20-Jul:     sum_of_sines, atan2 to rectangular->polar, phase_vocoder gen.
 * 22-June:    made mus_bessi0 local again.
 * 1-June:     bugfixes for linuxppc 2000.
 * 19-May:     mus_apply.
 * 8-May:      added "const" and XEN_PROCEDURE_CAST(for c++), made mus_bessi0 global.
 * 24-Apr:     changed formant radius to match lisp version(it's now 1-old_radius)
 * 20-Apr:     mus_convolve_files
 * 7-Apr:      src width bug fixed
 * 31-Mar:     finally implemented set-location for envs.
 * 14-Feb:     buffer-full?.
 * 1-Feb:      removed mus_phasepartials2waveshape.
 * 3-Jan-00:   format and type args added to make_sample2file, 
 *             mus_file_close. 
 *             removed make_file_input and make_file_output.
 * --------
 * 29-Dec:     various bugfixes especially in envelope handlers.
 * 19-Nov:     mus_oscil_bank and mus_formant_bank.
 * 5-Nov:      mus_sin exported.
 * 4-Oct:(scm) make-env arg order changed to reflect mus.lisp.
 * 29-Sep:     implemented mus-increment and mus-frequency for granulate(as in mus.lisp).
 *             clm's fft renamed mus-fft to avoid collision with snd's version.
 *             added max_size arg to make_granulate(to reflect mus.lisp).
 * 25-Sep-99:  added width arg to make_src -- forgot this somehow in first pass.
 *             decided to make mus_inspect return char* like mus_describe.
 */

/* isn't mus_env_interp backwards? */

#include "sndlib.h"

#if HAVE_COMPLEX_TRIG
#include <complex.h>
#endif

#if(!defined(M_PI))
  #define M_PI 3.14159265358979323846264338327
  #define M_PI_2 (M_PI / 2.0)
#endif

#define MUS_DEFAULT_SAMPLING_RATE 22050.0
#define MUS_DEFAULT_FILE_BUFFER_SIZE 8192
#define MUS_DEFAULT_ARRAY_PRINT_LENGTH 8

typedef enum {MUS_NOT_SPECIAL, MUS_SIMPLE_FILTER, MUS_FULL_FILTER, MUS_OUTPUT, MUS_INPUT, MUS_DELAY_LINE} mus_clm_extended_t;

typedef struct {
  struct mus_any_class *core;
} mus_any;

typedef struct mus_any_class {
  int type;
  char *name;
  int (*release)(mus_any *ptr);
  char *(*describe)(mus_any *ptr);
  bool (*equalp)(mus_any *gen1, mus_any *gen2);
  Float *(*data)(mus_any *ptr);
  Float *(*set_data)(mus_any *ptr, Float *new_data);
  off_t (*length)(mus_any *ptr);
  off_t (*set_length)(mus_any *ptr, off_t new_length);
  Float (*frequency)(mus_any *ptr);
  Float (*set_frequency)(mus_any *ptr, Float new_freq);
  Float (*phase)(mus_any *ptr); 
  Float (*set_phase)(mus_any *ptr, Float new_phase);
  Float (*scaler)(mus_any *ptr);
  Float (*set_scaler)(mus_any *ptr, Float val);
  Float (*increment)(mus_any *ptr);
  Float (*set_increment)(mus_any *ptr, Float val);
  Float (*run)(mus_any *gen, Float arg1, Float arg2);
  mus_clm_extended_t extended_type;
  void *(*closure)(mus_any *gen);
  int (*channels)(mus_any *ptr);
  Float (*offset)(mus_any *ptr);
  Float (*set_offset)(mus_any *ptr, Float val);
  Float (*width)(mus_any *ptr);
  Float (*set_width)(mus_any *ptr, Float val);
  Float (*xcoeff)(mus_any *ptr, int index);
  Float (*set_xcoeff)(mus_any *ptr, int index, Float val);
  off_t (*hop)(mus_any *ptr);
  off_t (*set_hop)(mus_any *ptr, off_t new_length);
  off_t (*ramp)(mus_any *ptr);
  off_t (*set_ramp)(mus_any *ptr, off_t new_length);
  Float (*read_sample)(mus_any *ptr, off_t samp, int chan);
  Float (*write_sample)(mus_any *ptr, off_t samp, int chan, Float data);
  char *(*file_name)(mus_any *ptr);
  int (*end)(mus_any *ptr);
  off_t (*location)(mus_any *ptr);
  off_t (*set_location)(mus_any *ptr, off_t loc);
  int (*channel)(mus_any *ptr);
  Float (*ycoeff)(mus_any *ptr, int index);
  Float (*set_ycoeff)(mus_any *ptr, int index, Float val);
  Float *(*xcoeffs)(mus_any *ptr);
  Float *(*ycoeffs)(mus_any *ptr);
  void *unused;
  void (*reset)(mus_any *ptr);
  void *(*set_closure)(mus_any *gen, void *e);
} mus_any_class;

typedef enum {MUS_INTERP_NONE, MUS_INTERP_LINEAR, MUS_INTERP_SINUSOIDAL, MUS_INTERP_ALL_PASS, 
	      MUS_INTERP_LAGRANGE, MUS_INTERP_BEZIER, MUS_INTERP_HERMITE, MUS_NUM_INTERPS} mus_interp_t;

typedef enum {MUS_RECTANGULAR_WINDOW, MUS_HANN_WINDOW, MUS_WELCH_WINDOW, MUS_PARZEN_WINDOW, MUS_BARTLETT_WINDOW,
	      MUS_HAMMING_WINDOW, MUS_BLACKMAN2_WINDOW, MUS_BLACKMAN3_WINDOW, MUS_BLACKMAN4_WINDOW,
	      MUS_EXPONENTIAL_WINDOW, MUS_RIEMANN_WINDOW, MUS_KAISER_WINDOW, MUS_CAUCHY_WINDOW, MUS_POISSON_WINDOW,
	      MUS_GAUSSIAN_WINDOW, MUS_TUKEY_WINDOW, MUS_DOLPH_CHEBYSHEV_WINDOW, MUS_HANN_POISSON_WINDOW, 
	      MUS_CONNES_WINDOW, MUS_SAMARAKI_WINDOW, MUS_ULTRASPHERICAL_WINDOW, MUS_NUM_WINDOWS} mus_fft_window_t;

typedef enum {MUS_CHEBYSHEV_OBSOLETE_KIND, MUS_CHEBYSHEV_FIRST_KIND, MUS_CHEBYSHEV_SECOND_KIND} mus_polynomial_t;

#define MUS_INTERP_TYPE_OK(Interp) ((Interp) <= MUS_NUM_INTERPS)
#define MUS_FFT_WINDOW_OK(Window) ((Window) < MUS_NUM_WINDOWS)
#if defined(__GNUC__) && (!(defined(__cplusplus)))
  #define MUS_RUN(GEN, ARG_1, ARG_2) ({ mus_any *_clm_h_1 = (mus_any *)(GEN); \
                                       ((*((_clm_h_1->core)->run))(_clm_h_1, ARG_1, ARG_2)); })
#else
  #define MUS_RUN(GEN, ARG_1, ARG_2) ((*(((GEN)->core)->run))(GEN, ARG_1, ARG_2))
#endif
#define MUS_RUN_P(GEN) (((GEN)->core)->run)
#define MUS_MAX_CLM_SINC_WIDTH 65536
#define MUS_MAX_CLM_SRC 65536.0

#ifdef __cplusplus
extern "C" {
#endif

void init_mus_module(void);

int mus_make_class_tag(void);
Float mus_radians_to_hz(Float radians);
Float mus_hz_to_radians(Float hz);
Float mus_degrees_to_radians(Float degrees);
Float mus_radians_to_degrees(Float radians);
Float mus_db_to_linear(Float x);
Float mus_linear_to_db(Float x);

Float mus_srate(void);
Float mus_set_srate(Float val);
off_t mus_seconds_to_samples(Float secs);
Float mus_samples_to_seconds(off_t samps);
int mus_array_print_length(void);
int mus_set_array_print_length(int val);
Float mus_float_equal_fudge_factor(void);
Float mus_set_float_equal_fudge_factor(Float val);

Float mus_sine_bank(Float *amps, Float *phases, int size);

Float mus_ring_modulate(Float s1, Float s2);
Float mus_amplitude_modulate(Float s1, Float s2, Float s3);
Float mus_contrast_enhancement(Float sig, Float index);
Float mus_dot_product(Float *data1, Float *data2, int size);
#if HAVE_COMPLEX_TRIG
complex double mus_edot_product(complex double freq, complex double *data, int size);
#endif
void mus_clear_array(Float *arr, int size);
bool mus_arrays_are_equal(Float *arr1, Float *arr2, Float fudge, int len);
Float mus_polynomial(Float *coeffs, Float x, int ncoeffs);
void mus_multiply_arrays(Float *data, Float *window, int len);
void mus_rectangular_to_polar(Float *rl, Float *im, int size);
void mus_polar_to_rectangular(Float *rl, Float *im, int size);
Float mus_array_interp(Float *wave, Float phase, int size);
double mus_bessi0(Float x);
Float mus_interpolate(mus_interp_t type, Float x, Float *table, int table_size, Float y);

int mus_free(mus_any *ptr);
char *mus_describe(mus_any *gen);
bool mus_equalp(mus_any *g1, mus_any *g2);
Float mus_phase(mus_any *gen);
Float mus_set_phase(mus_any *gen, Float val);
Float mus_set_frequency(mus_any *gen, Float val);
Float mus_frequency(mus_any *gen);
Float mus_run(mus_any *gen, Float arg1, Float arg2);
off_t mus_length(mus_any *gen);
off_t mus_set_length(mus_any *gen, off_t len);
Float *mus_data(mus_any *gen);
Float *mus_set_data(mus_any *gen, Float *data);
char *mus_name(mus_any *ptr);
Float mus_scaler(mus_any *gen);
Float mus_set_scaler(mus_any *gen, Float val);
Float mus_offset(mus_any *gen);
Float mus_set_offset(mus_any *gen, Float val);
Float mus_width(mus_any *gen);
Float mus_set_width(mus_any *gen, Float val);
char *mus_file_name(mus_any *ptr);
#define mus_interp_type(Gen) mus_channels(Gen)
void mus_reset(mus_any *ptr);

Float mus_oscil(mus_any *o, Float fm, Float pm);
Float mus_oscil_0(mus_any *ptr);
Float mus_oscil_1(mus_any *ptr, Float fm);
bool mus_oscil_p(mus_any *ptr);
mus_any *mus_make_oscil(Float freq, Float phase);

Float mus_sum_of_cosines(mus_any *gen, Float fm);
bool mus_sum_of_cosines_p(mus_any *ptr);
mus_any *mus_make_sum_of_cosines(int cosines, Float freq, Float phase);
#define mus_cosines(Gen) mus_hop(Gen)
#define mus_set_cosines(Gen, Val) mus_set_hop(Gen, Val)
Float mus_sum_of_sines(mus_any *ptr, Float fm);
mus_any *mus_make_sum_of_sines(int sines, Float freq, Float phase);
bool mus_sum_of_sines_p(mus_any *ptr);

Float mus_delay(mus_any *gen, Float input, Float pm);
Float mus_delay_1(mus_any *ptr, Float input);
Float mus_tap(mus_any *gen, Float loc);
Float mus_tap_1(mus_any *gen);
mus_any *mus_make_delay(int size, Float *line, int line_size, mus_interp_t type);
bool mus_delay_p(mus_any *ptr);
bool mus_delay_line_p(mus_any *gen); /* added 2-Mar-03 for tap error checks */
Float mus_delay_tick(mus_any *ptr, Float input);

Float mus_comb(mus_any *gen, Float input, Float pm);
Float mus_comb_1(mus_any *gen, Float input);
mus_any *mus_make_comb(Float scaler, int size, Float *line, int line_size, mus_interp_t type);
bool mus_comb_p(mus_any *ptr);

Float mus_notch(mus_any *gen, Float input, Float pm);
Float mus_notch_1(mus_any *gen, Float input);
mus_any *mus_make_notch(Float scaler, int size, Float *line, int line_size, mus_interp_t type);
bool mus_notch_p(mus_any *ptr);

Float mus_all_pass(mus_any *gen, Float input, Float pm);
Float mus_all_pass_1(mus_any *gen, Float input);
mus_any *mus_make_all_pass(Float backward, Float forward, int size, Float *line, int line_size, mus_interp_t type);
bool mus_all_pass_p(mus_any *ptr);

mus_any *mus_make_moving_average(int size, Float *line);
bool mus_moving_average_p(mus_any *ptr);
Float mus_moving_average(mus_any *ptr, Float input);

#define mus_feedforward(Gen) mus_scaler(Gen)
#define mus_set_feedforward(Gen, Val) mus_set_scaler(Gen, Val)
#define mus_feedback(Gen) mus_increment(Gen)
#define mus_set_feedback(Gen, Val) mus_set_increment(Gen, Val)

Float mus_table_lookup(mus_any *gen, Float fm);
Float mus_table_lookup_1(mus_any *gen);
mus_any *mus_make_table_lookup(Float freq, Float phase, Float *wave, int wave_size, mus_interp_t type);
bool mus_table_lookup_p(mus_any *ptr);
Float *mus_partials_to_wave(Float *partial_data, int partials, Float *table, int table_size, bool normalize);
Float *mus_phase_partials_to_wave(Float *partial_data, int partials, Float *table, int table_size, bool normalize);

Float mus_sawtooth_wave(mus_any *gen, Float fm);
mus_any *mus_make_sawtooth_wave(Float freq, Float amp, Float phase);
bool mus_sawtooth_wave_p(mus_any *gen);

Float mus_square_wave(mus_any *gen, Float fm);
mus_any *mus_make_square_wave(Float freq, Float amp, Float phase);
bool mus_square_wave_p(mus_any *gen);

Float mus_triangle_wave(mus_any *gen, Float fm);
mus_any *mus_make_triangle_wave(Float freq, Float amp, Float phase);
bool mus_triangle_wave_p(mus_any *gen);

Float mus_pulse_train(mus_any *gen, Float fm);
mus_any *mus_make_pulse_train(Float freq, Float amp, Float phase);
bool mus_pulse_train_p(mus_any *gen);

void mus_set_rand_seed(unsigned long seed);
unsigned long mus_rand_seed(void);
Float mus_random(Float amp);
Float mus_frandom(Float amp);
Float mus_random_1(void);
Float mus_frandom_1(void);
int mus_irandom(int amp);

Float mus_rand(mus_any *gen, Float fm);
mus_any *mus_make_rand(Float freq, Float base);
bool mus_rand_p(mus_any *ptr);
mus_any *mus_make_rand_with_distribution(Float freq, Float base, Float *distribution, int distribution_size);

Float mus_rand_interp(mus_any *gen, Float fm);
mus_any *mus_make_rand_interp(Float freq, Float base);
bool mus_rand_interp_p(mus_any *ptr);
mus_any *mus_make_rand_interp_with_distribution(Float freq, Float base, Float *distribution, int distribution_size);

Float mus_asymmetric_fm(mus_any *gen, Float index, Float fm);
Float mus_asymmetric_fm_1(mus_any *gen, Float index);
Float mus_asymmetric_fm_0(mus_any *gen);
mus_any *mus_make_asymmetric_fm(Float freq, Float phase, Float r, Float ratio);
bool mus_asymmetric_fm_p(mus_any *ptr);

Float mus_one_zero(mus_any *gen, Float input);
mus_any *mus_make_one_zero(Float a0, Float a1);
bool mus_one_zero_p(mus_any *gen);

Float mus_one_pole(mus_any *gen, Float input);
mus_any *mus_make_one_pole(Float a0, Float b1);
bool mus_one_pole_p(mus_any *gen);

Float mus_two_zero(mus_any *gen, Float input);
mus_any *mus_make_two_zero(Float a0, Float a1, Float a2);
bool mus_two_zero_p(mus_any *gen);
mus_any *mus_make_two_zero_from_radius_and_frequency(Float radius, Float frequency);

Float mus_two_pole(mus_any *gen, Float input);
mus_any *mus_make_two_pole(Float a0, Float b1, Float b2);
bool mus_two_pole_p(mus_any *gen);
mus_any *mus_make_two_pole_from_radius_and_frequency(Float radius, Float frequency);

Float mus_formant(mus_any *ptr, Float input); 
Float mus_formant_bank(Float *amps, mus_any **formants, Float inval, int size);
mus_any *mus_make_formant(Float radius, Float frequency, Float gain);
bool mus_formant_p(mus_any *ptr);
#define mus_formant_radius(Gen) mus_phase(Gen)
#define mus_set_formant_radius(Gen, Val) mus_set_phase(Gen, Val)
void mus_set_formant_radius_and_frequency(mus_any *ptr, Float radius, Float frequency);

Float mus_sine_summation(mus_any *ptr, Float fm);
mus_any *mus_make_sine_summation(Float frequency, Float phase, int n, Float a, Float b_ratio);
bool mus_sine_summation_p(mus_any *ptr);

Float mus_filter(mus_any *ptr, Float input);
mus_any *mus_make_filter(int order, Float *xcoeffs, Float *ycoeffs, Float *state);
bool mus_filter_p(mus_any *ptr);

Float mus_fir_filter(mus_any *ptr, Float input);
mus_any *mus_make_fir_filter(int order, Float *xcoeffs, Float *state);
bool mus_fir_filter_p(mus_any *ptr);

Float mus_iir_filter(mus_any *ptr, Float input);
mus_any *mus_make_iir_filter(int order, Float *ycoeffs, Float *state);
bool mus_iir_filter_p(mus_any *ptr);
Float *mus_make_fir_coeffs(int order, Float *env, Float *aa);

Float *mus_xcoeffs(mus_any *ptr);
Float *mus_ycoeffs(mus_any *ptr);
Float mus_xcoeff(mus_any *ptr, int index);
Float mus_set_xcoeff(mus_any *ptr, int index, Float val);
Float mus_ycoeff(mus_any *ptr, int index);
Float mus_set_ycoeff(mus_any *ptr, int index, Float val);
Float *mus_filter_set_xcoeffs(mus_any *ptr, Float *new_data);
Float *mus_filter_set_ycoeffs(mus_any *ptr, Float *new_data);
int mus_filter_set_order(mus_any *ptr, int order);
#define mus_order(Gen) mus_length(Gen)

Float mus_filtered_comb(mus_any *ptr, Float input, Float pm);
Float mus_filtered_comb_1(mus_any *ptr, Float input);
bool mus_filtered_comb_p(mus_any *ptr);
mus_any *mus_make_filtered_comb(Float scaler, int size, Float *line, int line_size, mus_interp_t type, mus_any *filt);

Float mus_wave_train(mus_any *gen, Float fm);
Float mus_wave_train_1(mus_any *gen);
mus_any *mus_make_wave_train(Float freq, Float phase, Float *wave, int wsize, mus_interp_t type);
bool mus_wave_train_p(mus_any *gen);

mus_any *mus_make_waveshape(Float frequency, Float phase, Float *table, int size);
Float mus_waveshape(mus_any *ptr, Float index, Float fm);
Float mus_waveshape_2(mus_any *ptr, Float fm);
Float mus_waveshape_1(mus_any *ptr, Float index);
Float mus_waveshape_0(mus_any *ptr);
bool mus_waveshape_p(mus_any *ptr);
Float *mus_partials_to_waveshape(int npartials, Float *partials, int size, Float *table);
Float *mus_partials_to_polynomial(int npartials, Float *partials, mus_polynomial_t kind);
mus_any *mus_make_polyshape(Float frequency, Float phase, Float *coeffs, int size);
Float mus_polyshape(mus_any *ptr, Float index, Float fm);
Float mus_polyshape_2(mus_any *ptr, Float fm);
Float mus_polyshape_1(mus_any *ptr, Float index);
Float mus_polyshape_0(mus_any *ptr);
bool mus_polyshape_p(mus_any *ptr);

Float mus_env(mus_any *ptr);
Float mus_env_linear(mus_any *ptr);
mus_any *mus_make_env(Float *brkpts, int pts, Float scaler, Float offset, Float base, Float duration, off_t start, off_t end, Float *odata);
bool mus_env_p(mus_any *ptr);
bool mus_env_linear_p(mus_any *ptr);
Float mus_env_interp(Float x, mus_any *env);
off_t *mus_env_passes(mus_any *gen); /* for Snd */
double *mus_env_rates(mus_any *gen); /* for Snd */
double mus_env_offset(mus_any *gen); /* for Snd */
double mus_env_scaler(mus_any *gen); /* for Snd */
double mus_env_initial_power(mus_any *gen); /* for Snd */
int mus_env_breakpoints(mus_any *gen); /* for Snd */
#define mus_position(Gen) mus_channels(Gen)

bool mus_frame_p(mus_any *ptr);
mus_any *mus_make_empty_frame(int chans);
mus_any *mus_make_frame(int chans, ...);
mus_any *mus_frame_add(mus_any *f1, mus_any *f2, mus_any *res);
mus_any *mus_frame_multiply(mus_any *f1, mus_any *f2, mus_any *res);
mus_any *mus_frame_scale(mus_any *uf1, Float scl, mus_any *ures);
mus_any *mus_frame_offset(mus_any *uf1, Float offset, mus_any *ures);
Float mus_frame_ref(mus_any *f, int chan);
Float mus_frame_set(mus_any *f, int chan, Float val);

bool mus_mixer_p(mus_any *ptr);
mus_any *mus_make_empty_mixer(int chans);
mus_any *mus_make_identity_mixer(int chans);
mus_any *mus_make_mixer(int chans, ...);
Float mus_mixer_ref(mus_any *f, int in, int out);
Float mus_mixer_set(mus_any *f, int in, int out, Float val);
mus_any *mus_frame_to_frame(mus_any *f, mus_any *in, mus_any *out);
mus_any *mus_sample_to_frame(mus_any *f, Float in, mus_any *out);
Float mus_frame_to_sample(mus_any *f, mus_any *in);
mus_any *mus_mixer_multiply(mus_any *f1, mus_any *f2, mus_any *res);
mus_any *mus_mixer_add(mus_any *f1, mus_any *f2, mus_any *res);
mus_any *mus_mixer_scale(mus_any *uf1, Float scaler, mus_any *ures);
mus_any *mus_mixer_offset(mus_any *uf1, Float offset, mus_any *ures);
mus_any *mus_make_scalar_mixer(int chans, Float scalar);

bool mus_file_to_sample_p(mus_any *ptr);
mus_any *mus_make_file_to_sample(const char *filename);
mus_any *mus_make_file_to_sample_with_buffer_size(const char *filename, int buffer_size);
Float mus_file_to_sample(mus_any *ptr, off_t samp, int chan);

Float mus_readin(mus_any *rd);
mus_any *mus_make_readin_with_buffer_size(const char *filename, int chan, off_t start, int direction, int buffer_size);
#define mus_make_readin(Filename, Chan, Start, Direction) mus_make_readin_with_buffer_size(Filename, Chan, Start, Direction, mus_file_buffer_size())
bool mus_readin_p(mus_any *ptr);
Float mus_increment(mus_any *rd);
Float mus_set_increment(mus_any *rd, Float dir);
off_t mus_location(mus_any *rd);
off_t mus_set_location(mus_any *rd, off_t loc);
int mus_channel(mus_any *rd);

bool mus_output_p(mus_any *ptr);
bool mus_input_p(mus_any *ptr);
Float mus_in_any(off_t frame, int chan, mus_any *IO);

mus_any *mus_make_file_to_frame(const char *filename);
bool mus_file_to_frame_p(mus_any *ptr);
mus_any *mus_file_to_frame(mus_any *ptr, off_t samp, mus_any *f);
mus_any *mus_make_file_to_frame_with_buffer_size(const char *filename, int buffer_size);

bool mus_sample_to_file_p(mus_any *ptr);
mus_any *mus_make_sample_to_file_with_comment(const char *filename, int out_chans, int out_format, int out_type, const char *comment);
#define mus_make_sample_to_file(Filename, Chans, OutFormat, OutType) mus_make_sample_to_file_with_comment(Filename, Chans, OutFormat, OutType, NULL)
Float mus_sample_to_file(mus_any *ptr, off_t samp, int chan, Float val);
mus_any *mus_continue_sample_to_file(const char *filename);
int mus_close_file(mus_any *ptr);

Float mus_out_any(off_t frame, Float val, int chan, mus_any *IO);
bool mus_frame_to_file_p(mus_any *ptr);
mus_any *mus_frame_to_file(mus_any *ptr, off_t samp, mus_any *data);
mus_any *mus_make_frame_to_file_with_comment(const char *filename, int chans, int out_format, int out_type, const char *comment);
#define mus_make_frame_to_file(Filename, Chans, OutFormat, OutType) mus_make_frame_to_file_with_comment(Filename, Chans, OutFormat, OutType, NULL)
mus_any *mus_continue_frame_to_file(const char *filename);

Float mus_locsig(mus_any *ptr, off_t loc, Float val);
mus_any *mus_make_locsig(Float degree, Float distance, Float reverb, int chans, mus_any *output, int rev_chans, mus_any *revput, mus_interp_t type);
bool mus_locsig_p(mus_any *ptr);
int mus_channels(mus_any *ptr);
Float mus_locsig_ref(mus_any *ptr, int chan);
Float mus_locsig_set(mus_any *ptr, int chan, Float val);
Float mus_locsig_reverb_ref(mus_any *ptr, int chan);
Float mus_locsig_reverb_set(mus_any *ptr, int chan, Float val);
void mus_move_locsig(mus_any *ptr, Float degree, Float distance);
void mus_fill_locsig(Float *arr, int chans, Float degree, Float scaler, mus_interp_t type);
mus_any *mus_locsig_outf(mus_any *ptr);
mus_any *mus_locsig_revf(mus_any *ptr);
void *mus_locsig_closure(mus_any *ptr);

bool mus_move_sound_p(mus_any *ptr);
Float mus_move_sound(mus_any *ptr, off_t loc, Float val);
mus_any *mus_make_move_sound(off_t start, off_t end, int out_channels, int rev_channels,
			     mus_any *doppler_delay, mus_any *doppler_env, mus_any *rev_env,
			     mus_any **out_delays, mus_any **out_envs, mus_any **rev_envs,
			     int *out_map, mus_any *output, mus_any *revput, bool free_arrays, bool free_gens);
mus_any *mus_move_sound_outf(mus_any *ptr);
mus_any *mus_move_sound_revf(mus_any *ptr);
void *mus_move_sound_closure(mus_any *ptr);

mus_any *mus_make_src(Float (*input)(void *arg, int direction), Float srate, int width, void *closure);
Float mus_src(mus_any *srptr, Float sr_change, Float (*input)(void *arg, int direction));
bool mus_src_p(mus_any *ptr);
Float mus_src_20(mus_any *srptr, Float (*input)(void *arg, int direction));
Float mus_src_05(mus_any *srptr, Float (*input)(void *arg, int direction));

bool mus_convolve_p(mus_any *ptr);
Float mus_convolve(mus_any *ptr, Float (*input)(void *arg, int direction));
mus_any *mus_make_convolve(Float (*input)(void *arg, int direction), Float *filter, int fftsize, int filtersize, void *closure);
Float *mus_spectrum(Float *rdat, Float *idat, Float *window, int n, int type);
void mus_fft(Float *rl, Float *im, int n, int is);
#if HAVE_FFTW || HAVE_FFTW3
void mus_fftw(Float *rl, int n, int dir);
#endif
Float *mus_make_fft_window(mus_fft_window_t type, int size, Float beta);
Float *mus_make_fft_window_with_window(mus_fft_window_t type, int size, Float beta, Float mu, Float *window);
Float *mus_convolution(Float* rl1, Float* rl2, int n);
void mus_convolve_files(const char *file1, const char *file2, Float maxamp, const char *output_file);

bool mus_granulate_p(mus_any *ptr);
Float mus_granulate(mus_any *ptr, Float (*input)(void *arg, int direction));
Float mus_granulate_with_editor(mus_any *ptr, Float (*input)(void *arg, int direction), int (*edit)(void *closure));
mus_any *mus_make_granulate(Float (*input)(void *arg, int direction), 
			    Float expansion, Float length, Float scaler, 
			    Float hop, Float ramp, Float jitter, int max_size, 
			    int (*edit)(void *closure),
			    void *closure);
int mus_granulate_grain_max_length(mus_any *ptr);
void mus_granulate_set_edit_function(mus_any *ptr, int (*edit)(void *closure));
off_t mus_ramp(mus_any *ptr);
off_t mus_set_ramp(mus_any *ptr, off_t val);
off_t mus_hop(mus_any *ptr);
off_t mus_set_hop(mus_any *ptr, off_t val);

int mus_set_file_buffer_size(int size);
int mus_file_buffer_size(void);

void mus_mix(const char *outfile, const char *infile, off_t out_start, off_t out_samps, off_t in_start, mus_any *mx, mus_any ***envs);
void mus_mix_with_reader_and_writer(mus_any *outf, mus_any *inf, off_t out_start, off_t out_frames, off_t in_start, mus_any *umx, mus_any ***envs);
Float mus_apply(mus_any *gen, Float f1, Float f2);

bool mus_phase_vocoder_p(mus_any *ptr);
mus_any *mus_make_phase_vocoder(Float (*input)(void *arg, int direction), 
				int fftsize, int overlap, int interp,
				Float pitch,
				bool (*analyze)(void *arg, Float (*input)(void *arg1, int direction)),
				int (*edit)(void *arg), /* return value is ignored (int return type is intended to be consistent with granulate) */
				Float (*synthesize)(void *arg), 
				void *closure);
Float mus_phase_vocoder(mus_any *ptr, Float (*input)(void *arg, int direction));
Float mus_phase_vocoder_with_editors(mus_any *ptr, 
				     Float (*input)(void *arg, int direction),
				     bool (*analyze)(void *arg, Float (*input)(void *arg1, int direction)),
				     int (*edit)(void *arg), 
				     Float (*synthesize)(void *arg));

Float *mus_phase_vocoder_amp_increments(mus_any *ptr);
Float *mus_phase_vocoder_amps(mus_any *ptr);
Float *mus_phase_vocoder_freqs(mus_any *ptr);
Float *mus_phase_vocoder_phases(mus_any *ptr);
Float *mus_phase_vocoder_phase_increments(mus_any *ptr);
int mus_phase_vocoder_outctr(mus_any *ptr);
int mus_phase_vocoder_set_outctr(mus_any *ptr, int val);

mus_any *mus_make_ssb_am(Float freq, int order);
bool mus_ssb_am_p(mus_any *ptr);
Float mus_ssb_am_1(mus_any *ptr, Float insig);
Float mus_ssb_am(mus_any *ptr, Float insig, Float fm);

void mus_clear_sinc_tables(void);
void *mus_environ(mus_any *gen);
void *mus_set_environ(mus_any *gen, void *e);


/* used only in run.lisp */
mus_any *mus_make_frame_with_data(int chans, Float *data);
mus_any *mus_make_mixer_with_data(int chans, Float *data);



#ifdef __cplusplus
}
#endif

#endif
