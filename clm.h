#ifndef MUS_H
#define MUS_H

#define MUS_VERSION 2
#define MUS_REVISION 5
#define MUS_DATE "23-Apr-02"

/* 
 * 23-Apr:     mus_environ and mus_set_environ for snd-run optimizer.
 * 8-Apr:      off-by-1 env bug (Lisp/C are now identical), env_interp of exp env beyond end bugfix.
 * 1-Apr:      sine-summation n=0 bugfix.
 * 27-Mar:     negative degree locsig bugfix.
 * 18-Mar:     mus_move_locsig.
 * 15-Mar:     n-chan locsig (and reverb scalers), 'type' arg to mus_make_locsig.
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
                 (previous version was not usable in gdb and was unneeded due to mus_snprintf)
 * 7-May:      filled in some leftover equal_p methods.
 * 1-Apr:      mus_make_file2sample_with_comment and mus_length for file->sample/sample->file.
 *             mus_file_buffer_size.
 * 26-Mar:     extended_type field added to mus_any_class for more robust type checking.
 * 16-Mar:     mus_phase of env -> current_value.
 * 28-Feb:     added mus_position (currently only for envs).
 * 8-Feb:      clm2scm.h.
 * 24-Jan:     mus-bank in clm2scm.
 * 5-Jan:      clm2scm gens are applicable.
 * 4-Jan:      mus_bank.
 * 2-Jan-01:   mus_run method.
 * --------
 * 28-Dec:     mus_clear_filter_state and other minor tweaks for Snd.
 * 28-Nov:     Dolph-Chebyshev window (under HAVE_GSL flag -- needs complex trig support).
 * 8-Nov:      mus_clear_sinc_tables.
 * 12-Oct:     mus_formant_bank takes one input (can't remember why I had an array here)
 * 27-Sep:     mus_array_interp bugfix (imitates mus.lisp now).
 * 18-Sep:     clm now assumes it's used as a part of sndlib.
 * 11-Sep:     generalized set! to generic functions in clm2scm.c.
 * 31-Aug:     changed formant field setters (thanks to Anders Vinjar).
 * 10-Aug:     removed built-in setf support (clm2scm.c).
 * 31-Jul:     mus_granulate tries to protect against illegal length and ramp values.
 * 24-Jul:     mus_make_fir_coeffs.
 * 20-Jul:     sum_of_sines, atan2 to rectangular->polar, phase_vocoder gen.
 * 22-June:    made mus_bessi0 local again.
 * 1-June:     bugfixes for linuxppc 2000.
 * 19-May:     mus_apply.
 * 8-May:      added "const" and XEN_PROCEDURE_CAST (for c++), made mus_bessi0 global.
 * 24-Apr:     changed formant radius to match lisp version (it's now 1-old_radius)
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
 * 4-Oct:      (scm) make-env arg order changed to reflect mus.lisp.
 * 29-Sep:     implemented mus-increment and mus-frequency for granulate (as in mus.lisp).
 *             clm's fft renamed mus-fft to avoid collision with snd's version.
 *             added max_size arg to make_granulate (to reflect mus.lisp).
 * 25-Sep-99:  added width arg to make_src -- forgot this somehow in first pass.
 *             decided to make mus_inspect return char* like mus_describe.
 */

/* compile time switches:
 *
 * WITH_SINE_TABLE (default 1)
 *   if 1, use table lookup for sine, else math library's sin
 *
 * HAVE_GSL (default 0)
 *   if 1, use libgsl for some math functions
 * 
 * Float can be defined to be double or float (default)
 */

/* taken from libtool's demo/foo.h to try to protect us from C++ and ancient C's */
#ifdef __CYGWIN__
#  ifndef __CYGWIN32__
#    define __CYGWIN32__
#  endif
#endif

#undef BEGIN_DECLS
#undef END_DECLS
#ifdef __cplusplus
# define BEGIN_DECLS extern "C" {
# define END_DECLS }
#else
# define BEGIN_DECLS /* empty */
# define END_DECLS /* empty */
#endif

#undef PROTO
#if defined (__STDC__) || defined (_AIX) || (defined (__mips) && defined (_SYSTYPE_SVR4)) || defined(__CYGWIN32__) || defined(__cplusplus)
# define PROTO(protos) protos
#else
# define PROTO(protos) ()
#endif

#ifndef TRUE
  #define TRUE 1
#endif
#ifndef FALSE
  #define FALSE 0
#endif

#include "sndlib.h"
enum {MUS_LINEAR, MUS_SINUSOIDAL};

#if (!defined(M_PI))
  #define M_PI 3.14159265358979323846264338327
  #define M_PI_2 (M_PI / 2.0)
#endif

typedef struct {
  struct mus__any_class *core;
} mus_any;

typedef struct mus__any_class {
  int type;
  char *name;
  int   (*release)(void *ptr);
  char  *(*describe)(void *ptr);
  char  *(*inspect)(void *ptr);
  int   (*equalp)(void *gen1, void *gen2);
  Float *(*data)(void *ptr);
  Float *(*set_data)(void *ptr, Float *new_data);
  int   (*length)(void *ptr);
  int   (*set_length)(void *ptr, int new_length);
  Float (*frequency)(void *ptr);
  Float (*set_frequency)(void *ptr, Float new_freq);
  Float (*phase)(void *ptr); 
  Float (*set_phase)(void *ptr, Float new_phase);
  Float (*scaler)(void *ptr);
  Float (*set_scaler)(void *ptr, Float val);
  Float (*run)(mus_any *gen, Float arg1, Float arg2);
  int extended_type;
} mus_any_class;

typedef struct {
  mus_any_class *core;
  int chans;
  Float *vals;
} mus_frame;

typedef struct {
  mus_any_class *core;
  int chans;
  Float **vals;
} mus_mixer;

typedef struct {
  int type;
  Float (*sample)(void *ptr, int samp, int chan);
  int (*location)(void *ptr);
  int (*set_location)(void *ptr, int loc);
  int (*channel)(void *ptr);
  int (*begin)(void *ptr);
  int (*end)(void *ptr);
} mus_input_class;

typedef struct {
  mus_any_class *core;
  mus_input_class *base;
} mus_input;

typedef struct {
  int type;
  Float (*sample)(void *ptr, int samp, int chan, Float data);
  int (*begin)(void *ptr);
  int (*end)(void *ptr);
} mus_output_class;

typedef struct {
  mus_any_class *core;
  mus_output_class *base;
} mus_output;

enum {MUS_OSCIL, MUS_SUM_OF_COSINES, MUS_DELAY, MUS_COMB, MUS_NOTCH, MUS_ALL_PASS,
      MUS_TABLE_LOOKUP, MUS_SQUARE_WAVE, MUS_SAWTOOTH_WAVE, MUS_TRIANGLE_WAVE, MUS_PULSE_TRAIN,
      MUS_RAND, MUS_RAND_INTERP, MUS_ASYMMETRIC_FM, MUS_ONE_ZERO, MUS_ONE_POLE, MUS_TWO_ZERO, MUS_TWO_POLE, MUS_FORMANT,
      MUS_WAVESHAPE, MUS_SRC, MUS_GRANULATE, MUS_SINE_SUMMATION, MUS_WAVE_TRAIN, MUS_BUFFER,
      MUS_FILTER, MUS_FIR_FILTER, MUS_IIR_FILTER, MUS_CONVOLVE, MUS_ENV, MUS_LOCSIG,
      MUS_FRAME, MUS_READIN, MUS_OUTPUT, MUS_INPUT, MUS_FILE2SAMPLE, MUS_FILE2FRAME,
      MUS_SAMPLE2FILE, MUS_FRAME2FILE, MUS_MIXER, MUS_PHASE_VOCODER,
      MUS_INITIAL_GEN_TAG};

enum {MUS_RECTANGULAR_WINDOW, MUS_HANN_WINDOW, MUS_WELCH_WINDOW, MUS_PARZEN_WINDOW, MUS_BARTLETT_WINDOW,
      MUS_HAMMING_WINDOW, MUS_BLACKMAN2_WINDOW, MUS_BLACKMAN3_WINDOW, MUS_BLACKMAN4_WINDOW,
      MUS_EXPONENTIAL_WINDOW, MUS_RIEMANN_WINDOW, MUS_KAISER_WINDOW, MUS_CAUCHY_WINDOW, MUS_POISSON_WINDOW,
      MUS_GAUSSIAN_WINDOW, MUS_TUKEY_WINDOW, MUS_DOLPH_CHEBYSHEV_WINDOW};
#define MUS_HANNING_WINDOW MUS_HANN_WINDOW

#define MUS_FFT_WINDOW_OK(Window) (((Window) >= MUS_RECTANGULAR_WINDOW) && ((Window) <= MUS_DOLPH_CHEBYSHEV_WINDOW))

#define MUS_RUN(GEN, ARG_1, ARG_2) ((*((GEN->core)->run))(GEN, ARG_1, ARG_2))
#define MUS_RUN_P(GEN) 	              ((GEN->core)->run)


BEGIN_DECLS

void init_mus_module PROTO((void));

int mus_make_class_tag          PROTO((void));
Float mus_radians2hz            PROTO((Float rads));
Float mus_hz2radians            PROTO((Float hz));
Float mus_degrees2radians       PROTO((Float degree));
Float mus_radians2degrees       PROTO((Float rads));
Float mus_db2linear             PROTO((Float x));
Float mus_linear2db             PROTO((Float x));
Float mus_srate                 PROTO((void));
Float mus_set_srate             PROTO((Float val));
int mus_array_print_length      PROTO((void));
int mus_set_array_print_length  PROTO((int val));
Float mus_sin                   PROTO((Float phase));
Float mus_sum_of_sines          PROTO((Float *amps, Float *phases, int size));

Float mus_ring_modulate         PROTO((Float s1, Float s2));
Float mus_amplitude_modulate    PROTO((Float s1, Float s2, Float s3));
Float mus_contrast_enhancement  PROTO((Float sig, Float index));
Float mus_dot_product           PROTO((Float *data1, Float *data2, int size));
void mus_clear_array            PROTO((Float *arr, int size));
Float mus_polynomial            PROTO((Float *coeffs, Float x, int ncoeffs));
void mus_multiply_arrays        PROTO((Float *data, Float *window, int len));
void mus_rectangular2polar      PROTO((Float *rl, Float *im, int size));
void mus_polar2rectangular      PROTO((Float *rl, Float *im, int size));
Float mus_array_interp          PROTO((Float *wave, Float phase, int size));

int mus_free                    PROTO((mus_any *ptr));
char *mus_describe              PROTO((mus_any *gen));
char *mus_inspect               PROTO((mus_any *gen));
int mus_equalp                  PROTO((mus_any *g1, mus_any *g2));
Float mus_phase                 PROTO((mus_any *gen));
Float mus_set_phase             PROTO((mus_any *gen, Float val));
Float mus_set_frequency         PROTO((mus_any *gen, Float val));
Float mus_frequency             PROTO((mus_any *gen));
Float mus_run                   PROTO((mus_any *gen, Float arg1, Float arg2));
int mus_length                  PROTO((mus_any *gen));
int mus_set_length              PROTO((mus_any *gen, int len));
Float *mus_data                 PROTO((mus_any *gen));
Float *mus_set_data             PROTO((mus_any *gen, Float *data));
char *mus_name                  PROTO((mus_any *ptr));
int mus_type                    PROTO((mus_any *ptr));
Float mus_scaler                PROTO((mus_any *gen));
Float mus_set_scaler            PROTO((mus_any *gen, Float val));

Float mus_oscil                 PROTO((mus_any *o, Float fm, Float pm));
Float mus_oscil_bank            PROTO((Float *amps, mus_any **oscils, Float *inputs, int size));
int mus_oscil_p                 PROTO((mus_any *ptr));
mus_any *mus_make_oscil         PROTO((Float freq, Float phase));

Float mus_sum_of_cosines        PROTO((mus_any *gen, Float fm));
int mus_sum_of_cosines_p        PROTO((mus_any *ptr));
mus_any *mus_make_sum_of_cosines PROTO((int cosines, Float freq, Float phase));
int mus_cosines                 PROTO((mus_any *ptr));

Float mus_delay                 PROTO((mus_any *gen, Float input, Float pm));
Float mus_tap                   PROTO((mus_any *gen, Float loc));
mus_any *mus_make_delay         PROTO((int size, Float *line, int line_size));
int mus_delay_p                 PROTO((mus_any *ptr));

Float mus_comb                  PROTO((mus_any *gen, Float input, Float pm));
mus_any *mus_make_comb          PROTO((Float scaler, int size, Float *line, int line_size));
int mus_comb_p                  PROTO((mus_any *ptr));

Float mus_notch                 PROTO((mus_any *gen, Float input, Float pm));
mus_any *mus_make_notch         PROTO((Float scaler, int size, Float *line, int line_size));
int mus_notch_p                 PROTO((mus_any *ptr));

Float mus_all_pass              PROTO((mus_any *gen, Float input, Float pm));
mus_any *mus_make_all_pass      PROTO((Float backward, Float forward, int size, Float *line, int line_size));
int mus_all_pass_p              PROTO((mus_any *ptr));
Float mus_feedback              PROTO((mus_any *ptr));
Float mus_set_feedback          PROTO((mus_any *ptr, Float val));
Float mus_feedforward           PROTO((mus_any *ptr));
Float mus_set_feedforward       PROTO((mus_any *ptr, Float val));

Float mus_table_lookup          PROTO((mus_any *gen, Float fm));
mus_any *mus_make_table_lookup  PROTO((Float freq, Float phase, Float *wave, int wave_size));
int mus_table_lookup_p          PROTO((mus_any *ptr));
Float *mus_partials2wave        PROTO((Float *partial_data, int partials, Float *table, int table_size, int normalize));
Float *mus_phasepartials2wave   PROTO((Float *partial_data, int partials, Float *table, int table_size, int normalize));

Float mus_sawtooth_wave         PROTO((mus_any *gen, Float fm));
mus_any *mus_make_sawtooth_wave PROTO((Float freq, Float amp, Float phase));
int mus_sawtooth_wave_p         PROTO((mus_any *gen));

Float mus_square_wave           PROTO((mus_any *gen, Float fm));
mus_any *mus_make_square_wave   PROTO((Float freq, Float amp, Float phase));
int mus_square_wave_p           PROTO((mus_any *gen));

Float mus_triangle_wave         PROTO((mus_any *gen, Float fm));
mus_any *mus_make_triangle_wave PROTO((Float freq, Float amp, Float phase));
int mus_triangle_wave_p         PROTO((mus_any *gen));

Float mus_pulse_train           PROTO((mus_any *gen, Float fm));
mus_any *mus_make_pulse_train   PROTO((Float freq, Float amp, Float phase));
int mus_pulse_train_p           PROTO((mus_any *gen));

void mus_set_rand_seed          PROTO((unsigned long seed));
unsigned long mus_rand_seed     PROTO((void));
Float mus_random                PROTO((Float amp));

Float mus_rand                  PROTO((mus_any *gen, Float fm));
mus_any *mus_make_rand          PROTO((Float freq, Float base));
int mus_rand_p                  PROTO((mus_any *ptr));

Float mus_rand_interp           PROTO((mus_any *gen, Float fm));
mus_any *mus_make_rand_interp   PROTO((Float freq, Float base));
int mus_rand_interp_p           PROTO((mus_any *ptr));

Float mus_asymmetric_fm         PROTO((mus_any *gen, Float index, Float fm));
mus_any *mus_make_asymmetric_fm PROTO((Float freq, Float phase, Float r, Float ratio));
int mus_asymmetric_fm_p         PROTO((mus_any *ptr));

Float mus_one_zero              PROTO((mus_any *gen, Float input));
mus_any *mus_make_one_zero      PROTO((Float a0, Float a1));
int mus_one_zero_p              PROTO((mus_any *gen));

Float mus_one_pole              PROTO((mus_any *gen, Float input));
mus_any *mus_make_one_pole      PROTO((Float a0, Float b1));
int mus_one_pole_p              PROTO((mus_any *gen));

Float mus_two_zero              PROTO((mus_any *gen, Float input));
mus_any *mus_make_two_zero      PROTO((Float a0, Float a1, Float a2));
int mus_two_zero_p              PROTO((mus_any *gen));
mus_any *mus_make_zpolar        PROTO((Float radius, Float frequency));

Float mus_two_pole              PROTO((mus_any *gen, Float input));
mus_any *mus_make_two_pole      PROTO((Float a0, Float b1, Float b2));
int mus_two_pole_p              PROTO((mus_any *gen));
mus_any *mus_make_ppolar        PROTO((Float radius, Float frequency));

Float mus_a0                    PROTO((mus_any *ptr));
Float mus_set_a0                PROTO((mus_any *ptr, Float val));
Float mus_a1                    PROTO((mus_any *ptr));
Float mus_set_a1                PROTO((mus_any *ptr, Float val));
Float mus_a2                    PROTO((mus_any *ptr));
Float mus_set_a2                PROTO((mus_any *ptr, Float val));
Float mus_b1                    PROTO((mus_any *ptr));
Float mus_set_b1                PROTO((mus_any *ptr, Float val));
Float mus_b2                    PROTO((mus_any *ptr));
Float mus_set_b2                PROTO((mus_any *ptr, Float val));

Float mus_formant               PROTO((mus_any *ptr, Float input)); 
Float mus_formant_bank          PROTO((Float *amps, mus_any **formants, Float inval, int size));
mus_any *mus_make_formant       PROTO((Float radius, Float frequency, Float gain));
int mus_formant_p               PROTO((mus_any *ptr));
Float mus_formant_radius        PROTO((mus_any *ptr));
Float mus_set_formant_radius    PROTO((mus_any *ptr, Float val));
void mus_set_formant_radius_and_frequency PROTO((mus_any *ptr, Float radius, Float frequency));

Float mus_sine_summation        PROTO((mus_any *ptr, Float fm));
mus_any *mus_make_sine_summation PROTO((Float frequency, Float phase, int n, Float a, Float b_ratio));
int mus_sine_summation_p        PROTO((mus_any *ptr));

Float mus_filter                PROTO((mus_any *ptr, Float input));
mus_any *mus_make_filter        PROTO((int order, Float *xcoeffs, Float *ycoeffs, Float *state));
int mus_filter_p                PROTO((mus_any *ptr));

Float mus_fir_filter            PROTO((mus_any *ptr, Float input));
mus_any *mus_make_fir_filter    PROTO((int order, Float *xcoeffs, Float *state));
int mus_fir_filter_p            PROTO((mus_any *ptr));

Float mus_iir_filter            PROTO((mus_any *ptr, Float input));
mus_any *mus_make_iir_filter    PROTO((int order, Float *ycoeffs, Float *state));
int mus_iir_filter_p            PROTO((mus_any *ptr));
Float *mus_make_fir_coeffs      PROTO((int order, Float *env, Float *aa));

Float *mus_xcoeffs              PROTO((mus_any *ptr));
Float *mus_ycoeffs              PROTO((mus_any *ptr));
int mus_order                   PROTO((mus_any *ptr));
void mus_clear_filter_state     PROTO((mus_any *gen));

Float mus_wave_train            PROTO((mus_any *gen, Float fm));
mus_any *mus_make_wave_train    PROTO((Float freq, Float phase, Float *wave, int wsize));
int mus_wave_train_p            PROTO((mus_any *gen));

Float mus_buffer2sample         PROTO((mus_any *ptr));
Float mus_sample2buffer         PROTO((mus_any *ptr, Float val));
mus_any *mus_make_buffer        PROTO((Float *preloaded_buffer, int size, Float current_file_time));
int mus_buffer_p                PROTO((mus_any *ptr));
int mus_buffer_empty_p          PROTO((mus_any *ptr));
int mus_buffer_full_p           PROTO((mus_any *ptr));
mus_any *mus_buffer2frame       PROTO((mus_any *rb, mus_any *fr));
mus_any *mus_frame2buffer       PROTO((mus_any *rb, mus_any *fr));

mus_any *mus_make_waveshape     PROTO((Float frequency, Float phase, Float *table, int size));
Float mus_waveshape             PROTO((mus_any *ptr, Float index, Float fm));
int mus_waveshape_p             PROTO((mus_any *ptr));
Float *mus_partials2waveshape   PROTO((int npartials, Float *partials, int size, Float *table));
Float *mus_partials2polynomial  PROTO((int npartials, Float *partials, int kind));

Float mus_env                   PROTO((mus_any *ptr));
mus_any *mus_make_env           PROTO((Float *brkpts, int pts, Float scaler, Float offset, Float base, Float duration, int start, int end, Float *odata));
int mus_env_p                   PROTO((mus_any *ptr));
void mus_restart_env            PROTO((mus_any *ptr));
Float mus_env_interp            PROTO((Float x, mus_any *env));
int *mus_env_passes             PROTO((mus_any *gen)); /* for Snd */
double *mus_env_rates           PROTO((mus_any *gen)); /* ditto */
int mus_position                PROTO((mus_any *ptr));

int mus_frame_p                 PROTO((mus_any *ptr));
mus_frame *mus_make_empty_frame PROTO((int chans));
mus_frame *mus_make_frame       PROTO((int chans, ...));
mus_frame *mus_frame_add        PROTO((mus_frame *f1, mus_frame *f2, mus_frame *res));
mus_frame *mus_frame_multiply   PROTO((mus_frame *f1, mus_frame *f2, mus_frame *res));
Float mus_frame_ref             PROTO((mus_frame *f, int chan));
Float mus_frame_set             PROTO((mus_frame *f, int chan, Float val));
Float *mus_frame_data           PROTO((mus_frame *f));

int mus_mixer_p                 PROTO((mus_any *ptr));
mus_mixer *mus_make_empty_mixer PROTO((int chans));
mus_mixer *mus_make_identity_mixer PROTO((int chans));
mus_mixer *mus_make_mixer       PROTO((int chans, ...));
Float **mus_mixer_data          PROTO((mus_mixer *f));
Float mus_mixer_ref             PROTO((mus_mixer *f, int in, int out));
Float mus_mixer_set             PROTO((mus_mixer *f, int in, int out, Float val));
mus_frame *mus_frame2frame      PROTO((mus_mixer *f, mus_frame *in, mus_frame *out));
mus_frame *mus_sample2frame     PROTO((mus_any *f, Float in, mus_frame *out));
Float mus_frame2sample          PROTO((mus_any *f, mus_frame *in));
mus_mixer *mus_mixer_multiply   PROTO((mus_mixer *f1, mus_mixer *f2, mus_mixer *res));

int mus_file2sample_p           PROTO((mus_any *ptr));
mus_any *mus_make_file2sample   PROTO((const char *filename));
Float mus_file2sample           PROTO((mus_any *ptr, int samp, int chan));

Float mus_readin                PROTO((mus_any *rd));
mus_any *mus_make_readin        PROTO((const char *filename, int chan, int start, int direction));
int mus_readin_p                PROTO((mus_any *ptr));
Float mus_increment             PROTO((mus_any *rd));
Float mus_set_increment         PROTO((mus_any *rd, Float dir));
int mus_location                PROTO((mus_any *rd));
int mus_set_location            PROTO((mus_any *rd, int loc));
int mus_channel                 PROTO((mus_input *rd));

int mus_output_p                PROTO((void *ptr));
int mus_input_p                 PROTO((void *ptr));
Float mus_in_any                PROTO((int frame, int chan, mus_input *IO));
Float mus_ina                   PROTO((int frame, mus_input *inp));
Float mus_inb                   PROTO((int frame, mus_input *inp));

mus_any *mus_make_file2frame    PROTO((const char *filename));
int mus_file2frame_p            PROTO((mus_any *ptr));
mus_frame *mus_file2frame       PROTO((mus_any *ptr, int samp, mus_frame *f));

int mus_sample2file_p           PROTO((mus_any *ptr));
mus_any *mus_make_sample2file   PROTO((const char *filename, int chans, int out_format, int out_type));
mus_any *mus_make_sample2file_with_comment PROTO((const char *filename, int out_chans, int out_format, int out_type, const char *comment));
Float mus_sample2file           PROTO((mus_any *ptr, int samp, int chan, Float val));
int mus_close_file              PROTO((mus_any *ptr));

Float mus_out_any               PROTO((int frame, Float val, int chan, mus_output *IO));
Float mus_outa                  PROTO((int frame, Float val, mus_output *IO));
Float mus_outb                  PROTO((int frame, Float val, mus_output *IO));
Float mus_outc                  PROTO((int frame, Float val, mus_output *IO));
Float mus_outd                  PROTO((int frame, Float val, mus_output *IO));

mus_any *mus_make_frame2file    PROTO((const char *filename, int chans, int out_format, int out_type));
int mus_frame2file_p            PROTO((mus_any *ptr));
mus_frame *mus_frame2file       PROTO((mus_any *ptr, int samp, mus_frame *data));

mus_frame *mus_locsig           PROTO((mus_any *ptr, int loc, Float val));
mus_any *mus_make_locsig        PROTO((Float degree, Float distance, Float reverb, int chans, mus_output *output, mus_output *revput, int type));
int mus_locsig_p                PROTO((mus_any *ptr));
int mus_channels                PROTO((mus_any *ptr));
Float mus_locsig_ref            PROTO((mus_any *ptr, int chan));
Float mus_locsig_set            PROTO((mus_any *ptr, int chan, Float val));
Float mus_locsig_reverb_ref     PROTO((mus_any *ptr, int chan));
Float mus_locsig_reverb_set     PROTO((mus_any *ptr, int chan, Float val));
void mus_move_locsig            PROTO((mus_any *ptr, Float degree, Float distance));

mus_any *mus_make_src           PROTO((Float (*input)(void *arg, int direction), Float srate, int width, void *environ));
Float mus_src                   PROTO((mus_any *srptr, Float sr_change, Float (*input)(void *arg, int direction)));
int mus_src_p                   PROTO((mus_any *ptr));

int mus_convolve_p              PROTO((mus_any *ptr));
Float mus_convolve              PROTO((mus_any *ptr, Float (*input)(void *arg, int direction)));
mus_any *mus_make_convolve      PROTO((Float (*input)(void *arg, int direction), Float *filter, int fftsize, int filtersize, void *environ));
void mus_spectrum               PROTO((Float *rdat, Float *idat, Float *window, int n, int type));
void mus_fft                    PROTO((Float *rl, Float *im, int n, int is));
Float *mus_make_fft_window      PROTO((int type, int size, Float beta));
Float *mus_make_fft_window_with_window PROTO((int type, int size, Float beta, Float *window));
void mus_convolution            PROTO((Float* rl1, Float* rl2, int n));
void mus_convolve_files         PROTO((const char *file1, const char *file2, Float maxamp, const char *output_file));

int mus_granulate_p             PROTO((mus_any *ptr));
Float mus_granulate             PROTO((mus_any *ptr, Float (*input)(void *arg, int direction)));
mus_any *mus_make_granulate     PROTO((Float (*input)(void *arg, int direction), 
				       Float expansion, Float length, Float scaler, 
				       Float hop, Float ramp, Float jitter, int max_size, void *environ));
int mus_ramp                    PROTO((mus_any *ptr));
int mus_set_ramp                PROTO((mus_any *ptr, int val));
int mus_hop                     PROTO((mus_any *ptr));
int mus_set_hop                 PROTO((mus_any *ptr, int val));

int mus_set_file_buffer_size    PROTO((int size));
int mus_file_buffer_size        PROTO((void));

void mus_mix                    PROTO((const char *outfile, const char *infile, int out_start, int out_samps, int in_start, mus_mixer *mx, mus_any ***envs));
int mus_file2fltarray           PROTO((const char *filename, int chan, int start, int samples, Float *array));
int mus_fltarray2file           PROTO((const char *filename, Float *ddata, int len, int srate, int channels));

Float mus_apply                 PROTO((mus_any *gen, ...));
Float mus_bank                  PROTO((mus_any **gens, Float *scalers, Float *arg1, Float *arg2, int size));

int mus_phase_vocoder_p         PROTO((mus_any *ptr));
mus_any *mus_make_phase_vocoder PROTO((Float (*input)(void *arg, int direction), 
				       int fftsize, int overlap, int interp,
				       Float pitch,
				       int (*analyze)(void *arg, Float (*input)(void *arg1, int direction)),
				       int (*edit)(void *arg), 
				       Float (*synthesize)(void *arg), 
				       void *environ));
Float mus_phase_vocoder         PROTO((mus_any *ptr, Float (*input)(void *arg, int direction)));

/* temporary names -- can't immediately think of better ones */
Float *mus_phase_vocoder_ampinc PROTO((void *ptr));
Float *mus_phase_vocoder_amps   PROTO((void *ptr));
Float *mus_phase_vocoder_freqs  PROTO((void *ptr));
Float *mus_phase_vocoder_phases PROTO((void *ptr));
Float *mus_phase_vocoder_phaseinc PROTO((void *ptr));
Float *mus_phase_vocoder_lastphase PROTO((void *ptr));
int mus_phase_vocoder_outctr    PROTO((void *ptr));
int mus_phase_vocoder_set_outctr PROTO((void *ptr, int val));

void mus_clear_sinc_tables      PROTO((void));
void *mus_environ               PROTO((mus_any *rd));
void *mus_set_environ           PROTO((mus_any *rd, void *ptr));

END_DECLS

#endif
