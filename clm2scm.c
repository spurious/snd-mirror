/* tie CLM module into Guile/Scheme */

/* This module uses the Float array support in vct.c -- it needs to be loaded with vct.o.
 * every generator that has embedded arrays handles these through an extra layer of
 * pointers; the problem here is that we allow the caller to access and set these directly,
 * (and don't want to copy data unnecessarily), so we can easily have many pointers
 * floating around to the same C memory; there's no way at this level to set up
 * reference counters, so in C, the various free_<gen> functions check that they
 * allocated the given memory (and all vct objects are allocated elsewhere),
 * before freeing an embedded array; then here, all such arrays are wrapped up 
 * as separate SCM objects and at every level only the bottom-most reference allows 
 * the free to go forward.
 */

/* it's important to check for mus_scm_p before de-referencing a gen arg */

/* TODO:   add vct-wrappers for other internal arrays?
 */

#if defined(HAVE_CONFIG_H)
  #include "config.h"
#endif

#include <ctype.h>
#include <stddef.h>
#include <math.h>
#include <stdio.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>

#if (defined(NEXT) || (defined(HAVE_LIBC_H) && (!defined(HAVE_UNISTD_H))))
  #include <libc.h>
#else
  #ifndef _MSC_VER
    #include <unistd.h>
  #endif
#endif

#if USE_SND
  #include "snd.h"
#endif

#include "sndlib.h"
#include "clm.h"

#if HAVE_GUILE
  #include <guile/gh.h>
  #include "sg.h"
#else
  #if HAVE_LIBREP 
    #include <rep.h>
    #include "sl.h"
  #else
    #include "noguile.h"
  #endif
#endif

#include "vct.h"
#include "sndlib2scm.h"
#include "clm2scm.h"

#if (!USE_SND)
static int to_c_int_or_else(SCM obj, int fallback, char *origin)
{
  /* don't want errors here about floats with non-zero fractions etc */
  if (INTEGER_P(obj))
    return(TO_C_INT(obj));
  else
    if (NUMBER_P(obj))
      return((int)TO_C_DOUBLE_WITH_ORIGIN(obj, origin));
  return(fallback);
}

static void define_procedure_with_setter(char *get_name, SCM (*get_func)(), char *get_help,
					 char *set_name, SCM (*set_func)(), 
					 SCM local_doc,
					 int get_req, int get_opt, int set_req, int set_opt)
{
  scm_set_object_property_x(
    SCM_CDR(
      gh_define(get_name,
	scm_make_procedure_with_setter(gh_new_procedure("", SCM_FNC get_func, get_req, get_opt, 0),
	  gh_new_procedure(set_name, SCM_FNC set_func, set_req, set_opt, 0)
	  ))),
    local_doc,
    TO_SCM_STRING(get_help));
}
#endif



/* ---------------- keywords ---------------- */

#define SC_frequency        "frequency"
#define SC_initial_phase    "initial-phase"
#define SC_wave             "wave"
#define SC_cosines          "cosines"
#define SC_amplitude        "amplitude"
#define SC_ratio            "ratio"
#define SC_size             "size"
#define SC_a0               "a0"
#define SC_a1               "a1"
#define SC_a2               "a2"
#define SC_b1               "b1"
#define SC_b2               "b2"
#define SC_input            "input"
#define SC_srate            "srate"
#define SC_file             "file"
#define SC_channel          "channel"
#define SC_start            "start"
#define SC_initial_contents "initial-contents"
#define SC_initial_element  "initial-element"
#define SC_scaler           "scaler"
#define SC_feedforward      "feedforward"
#define SC_feedback         "feedback"
#define SC_max_size         "max-size"
#define SC_radius           "radius"
#define SC_gain             "gain"
#define SC_partials         "partials"
#define SC_r                "r"
#define SC_a                "a"
#define SC_n                "n"
#define SC_fill_time        "fill-time"
#define SC_order            "order"
#define SC_x_coeffs         "xcoeffs"
#define SC_y_coeffs         "ycoeffs"
#define SC_envelope         "envelope"
#define SC_base             "base"
#define SC_duration         "duration"
#define SC_offset           "offset"
#define SC_end              "end"
#define SC_direction        "direction"
#define SC_degree           "degree"
#define SC_distance         "distance"
#define SC_reverb           "reverb"
#define SC_output           "output"
#define SC_fft_size         "fft-size"
#define SC_expansion        "expansion"
#define SC_length           "length"
#define SC_hop              "hop"
#define SC_ramp             "ramp"
#define SC_jitter           "jitter"
#define SC_type             "type"
#define SC_format           "format"
#define SC_comment          "comment"
#define SC_channels         "channels"
#define SC_filter           "filter"
#define SC_revout           "revout"
#define SC_width            "width"
#define SC_edit             "edit"
#define SC_synthesize       "synthesize"
#define SC_analyze          "analyze"
#define SC_interp           "interp"
#define SC_overlap          "overlap"
#define SC_pitch            "pitch"

#define NUM_KEYWORDS 62
enum {C_frequency, C_initial_phase, C_wave, C_cosines, C_amplitude,
      C_r, C_ratio, C_size, C_a0, C_a1, C_a2, C_b1, C_b2, C_max_size,
      C_input, C_srate, C_file, C_channel, C_start,
      C_initial_contents, C_initial_element, C_scaler, C_feedforward, C_feedback,
      C_radius, C_gain, C_partials, C_fill_time, C_a, C_n,
      C_order, C_x_coeffs, C_y_coeffs, C_envelope, C_base, C_duration, C_offset, C_end,
      C_direction, C_degree, C_distance, C_reverb, C_output, C_fft_size,
      C_expansion, C_length, C_hop, C_ramp, C_jitter,
      C_type, C_format, C_comment, C_channels, C_filter, C_revout, C_width,
      C_edit, C_synthesize, C_analyze, C_interp, C_overlap, C_pitch
};

static char *keywords[NUM_KEYWORDS] = {SC_frequency, SC_initial_phase, SC_wave, SC_cosines, SC_amplitude,
				       SC_r, SC_ratio, SC_size, SC_a0, SC_a1, SC_a2, SC_b1, SC_b2, SC_max_size,
				       SC_input, SC_srate, SC_file, SC_channel, SC_start,
				       SC_initial_contents, SC_initial_element, SC_scaler, SC_feedforward, SC_feedback,
				       SC_radius, SC_gain, SC_partials, SC_fill_time, SC_a, SC_n,
				       SC_order, SC_x_coeffs, SC_y_coeffs, SC_envelope, SC_base, SC_duration, SC_offset, SC_end,
				       SC_direction, SC_degree, SC_distance, SC_reverb, SC_output, SC_fft_size,
				       SC_expansion, SC_length, SC_hop, SC_ramp, SC_jitter,
				       SC_type, SC_format, SC_comment, SC_channels, SC_filter, SC_revout, SC_width,
				       SC_edit, SC_synthesize, SC_analyze, SC_interp, SC_overlap, SC_pitch
};
static SCM all_keys[NUM_KEYWORDS];

static void init_keywords(void)
{
  int i;
  for (i = 0; i < NUM_KEYWORDS; i++) 
    all_keys[i] = MAKE_KEYWORD(keywords[i]);
}

static int decode_keywords(char *caller, int nkeys, SCM *keys, int nargs, SCM *args, int *orig)
{
  /* implement the "optional-key" notion in CLM */
  int arg_ctr = 0, key_start = 0, rtn_ctr = 0, i, keying = 0, key_found = 0;
  SCM key;
  arg_ctr = 0;
  while ((arg_ctr < nargs) && 
	 (BOUND_P(args[arg_ctr])))
    {
      if (!(KEYWORD_P(args[arg_ctr])))
	{
	  if (keying) 
	    mus_misc_error(caller, "unmatched value within keyword section?", args[arg_ctr]);
	  /* type checking on the actual values has to be the caller's problem */
	  if (arg_ctr > nkeys) 
	    mus_misc_error(caller, "ran out of keyword space!", args[arg_ctr]);
	  keys[arg_ctr] = args[arg_ctr];
	  orig[arg_ctr] = arg_ctr;
	  arg_ctr++;
	  key_start = arg_ctr;
	  rtn_ctr++;
	}
      else
	{
	  if ((arg_ctr == (nargs - 1)) ||
	      (!(BOUND_P(args[arg_ctr + 1]))))
	    mus_misc_error(caller, "keyword without value?", (arg_ctr > 0) ? args[arg_ctr - 1] : TO_SCM_STRING("hmmm... this should not happen"));
	  keying = 1;
	  key = args[arg_ctr];
	  if (KEYWORD_P(args[arg_ctr + 1])) 
	    mus_misc_error(caller, "two keywords in a row?", key);
	  if (key_start > nkeys) 
	    mus_misc_error(caller, "extra trailing args?", key);
	  key_found = 0;
	  for (i = key_start; i < nkeys; i++)
	    {
	      if (SCM_EQ_P(keys[i], key))
		{
		  keys[i] = args[arg_ctr + 1];
		  orig[i] = arg_ctr + 1;
		  arg_ctr += 2;
		  rtn_ctr++;
		  key_found = 1;
		}
	    }
	  if (key_found == 0)
	    {
	      /* either there's a redundant keyword pair or a keyword that 'caller' doesn't recognize */
	      mus_misc_error(caller, "redundant or invalid key found", key);
	      arg_ctr += 2;
	    }
	}
    }
  return(rtn_ctr);
}

static Float fkeyarg (SCM key, char *caller, int n, SCM val, Float def)
{
  if (!(KEYWORD_P(key)))
    {
      if (NUMBER_P(key))
	return(TO_C_DOUBLE(key));
      else scm_wrong_type_arg(caller, n, val);
    }
  return(def);
}

static int ikeyarg (SCM key, char *caller, int n, SCM val, int def)
{
  if (!(KEYWORD_P(key)))
    {
      if (NUMBER_P(key))
	return(TO_C_INT_OR_ELSE(key, 0));
      else scm_wrong_type_arg(caller, n, val);
    }
  return(def);
}

static Float fkeyarg_or_error (SCM key, char *caller, int n, SCM val, Float def)
{
  if (!(KEYWORD_P(key)))
    {
      if (NUMBER_P(key))
	return(TO_C_DOUBLE(key));
      else return(MUS_MISC_ERROR);
    }
  return(def);
}

static int ikeyarg_or_error (SCM key, char *caller, int n, SCM val, int def)
{
  if (!(KEYWORD_P(key)))
    {
      if (NUMBER_P(key))
	return(TO_C_INT_OR_ELSE(key, 0));
      else return(MUS_MISC_ERROR);
    }
  return(def);
}


static SCM local_doc;

/* ---------------- AM and simple stuff ---------------- */

#define S_radians_hz             "radians->hz"
#define S_hz_radians             "hz->radians"
#define S_in_hz                  "in-hz"
#define S_degrees_radians        "degrees->radians"
#define S_radians_degrees        "radians->degrees"
#define S_db_linear              "db->linear"
#define S_linear_db              "linear->db"
#define S_ring_modulate          "ring-modulate"
#define S_amplitude_modulate     "amplitude-modulate"
#define S_contrast_enhancement   "contrast-enhancement"
#define S_mus_set_srate              "mus-set-srate"
#define S_mus_srate                  "mus-srate"
#define S_mus_set_array_print_length "mus-set-array-print-length"
#define S_mus_array_print_length "mus-array-print-length"
#define S_dot_product            "dot-product"
#define S_clear_array            "clear-array"
#define S_polynomial             "polynomial"
#define S_multiply_arrays        "multiply-arrays"
#define S_make_fft_window        "make-fft-window"
#define S_mus_fft                "mus-fft"
#define S_spectrum               "spectrum"
#define S_convolution            "convolution"
#define S_rectangular2polar      "rectangular->polar"
#define S_array_interp           "array-interp"
#define S_sum_of_sines           "sum-of-sines"

#define S_bartlett_window        "bartlett-window"
#define S_blackman2_window       "blackman2-window"
#define S_blackman3_window       "blackman3-window"
#define S_blackman4_window       "blackman4-window"
#define S_cauchy_window          "cauchy-window"
#define S_exponential_window     "exponential-window"
#define S_gaussian_window        "gaussian-window"
#define S_hamming_window         "hamming-window"
#define S_hanning_window         "hanning-window"
#define S_kaiser_window          "kaiser-window"
#define S_parzen_window          "parzen-window"
#define S_poisson_window         "poisson-window"
#define S_rectangular_window     "rectangular-window"
#define S_riemann_window         "riemann-window"
#define S_tukey_window           "tukey-window"
#define S_welch_window           "welch-window"
#define S_dolph_chebyshev_window "dolph-chebyshev-window"

static char *FFT_WINDOW_CONSTANTS[17] = {S_rectangular_window, S_hanning_window, S_welch_window, S_parzen_window, S_bartlett_window,
					 S_hamming_window, S_blackman2_window, S_blackman3_window, S_blackman4_window,
					 S_exponential_window, S_riemann_window, S_kaiser_window, S_cauchy_window,
					 S_poisson_window, S_gaussian_window, S_tukey_window, S_dolph_chebyshev_window
};

char *mus_fft_window_name(int i) {return(FFT_WINDOW_CONSTANTS[i]);}


static SCM g_radians2hz(SCM val) 
{
  #define H_radians_hz "(" S_radians_hz " rads) converts radians/sample to frequency in Hz: rads*srate/(2*pi)"
  SCM_ASSERT(NUMBER_P(val), val, SCM_ARG1, S_radians_hz);
  return(TO_SCM_DOUBLE(mus_radians2hz(TO_C_DOUBLE(val))));
}

static SCM g_hz2radians(SCM val) 
{
  #define H_hz_radians "(" S_hz_radians " hz) converts frequency in Hz to radians/sample: hz*2*pi/srate"
  #define H_in_hz "(" S_in_hz " hz) converts frequency in Hz to radians/sample: hz*2*pi/srate"
  SCM_ASSERT(NUMBER_P(val), val, SCM_ARG1, S_hz_radians); 
  return(TO_SCM_DOUBLE(mus_hz2radians(TO_C_DOUBLE(val))));
}

static SCM g_radians2degrees(SCM val) 
{
  #define H_radians_degrees "(" S_radians_degrees " rads) converts radians to degrees: rads*360/(2*pi)"
  SCM_ASSERT(NUMBER_P(val), val, SCM_ARG1, S_radians_degrees); 
  return(TO_SCM_DOUBLE(mus_radians2degrees(TO_C_DOUBLE(val))));
}

static SCM g_degrees2radians(SCM val) 
{
  #define H_degrees_radians "(" S_degrees_radians " deg) converts degrees to radians: deg*2*pi/360"
  SCM_ASSERT(NUMBER_P(val), val, SCM_ARG1, S_degrees_radians); 
  return(TO_SCM_DOUBLE(mus_degrees2radians(TO_C_DOUBLE(val))));
}

static SCM g_db2linear(SCM val) 
{
  #define H_db_linear "(" S_db_linear " db) converts decibel value db to linear value: pow(10, db/20)"
  SCM_ASSERT(NUMBER_P(val), val, SCM_ARG1, S_db_linear);
  return(TO_SCM_DOUBLE(mus_db2linear(TO_C_DOUBLE(val))));
}

static SCM g_linear2db(SCM val) 
{
  #define H_linear_db "(" S_linear_db " lin) converts linear value to decibels: 20*log10(lin)"
  SCM_ASSERT(NUMBER_P(val), val, SCM_ARG1, S_linear_db);
  return(TO_SCM_DOUBLE(mus_linear2db(TO_C_DOUBLE(val))));
}

/* can't use a variable *srate* directly here because the set! side would not communicate the change to C */
static SCM g_srate(void) 
{
  #define H_mus_srate "(" S_mus_srate ") -> current sampling rate"
  return(TO_SCM_DOUBLE(mus_srate()));
}

static SCM g_set_srate(SCM val) 
{
  SCM_ASSERT(NUMBER_P(val), val, SCM_ARG1, S_mus_set_srate);
  return(TO_SCM_DOUBLE(mus_set_srate(TO_C_DOUBLE(val))));
}

static SCM g_array_print_length(void) 
{
  #define H_mus_array_print_length "(" S_mus_array_print_length ") -> current clm array print length (default is 8)"
  return(TO_SMALL_SCM_INT(mus_array_print_length()));
}

static SCM g_set_array_print_length(SCM val) 
{
  SCM_ASSERT(INTEGER_P(val), val, SCM_ARG1, S_mus_set_array_print_length);
  return(TO_SMALL_SCM_INT(mus_set_array_print_length(TO_C_INT(val))));
}

static SCM g_ring_modulate(SCM val1, SCM val2) 
{
  #define H_ring_modulate "(" S_ring_modulate " s1 s2) -> s1*s2 (sample by sample)"
  SCM_ASSERT(NUMBER_P(val1), val1, SCM_ARG1, S_ring_modulate);
  SCM_ASSERT(NUMBER_P(val2), val2, SCM_ARG2, S_ring_modulate);
  return(TO_SCM_DOUBLE(mus_ring_modulate(TO_C_DOUBLE(val1), TO_C_DOUBLE(val2))));
}

static SCM g_amplitude_modulate(SCM val1, SCM val2, SCM val3) 
{
  #define H_amplitude_modulate "(" S_amplitude_modulate " carrier in1 in2) -> in1*(carrier+in2)"
  SCM_ASSERT(NUMBER_P(val1), val1, SCM_ARG1, S_amplitude_modulate);
  SCM_ASSERT(NUMBER_P(val2), val2, SCM_ARG2, S_amplitude_modulate);
  SCM_ASSERT(NUMBER_P(val3), val3, SCM_ARG3, S_amplitude_modulate);
  return(TO_SCM_DOUBLE(mus_amplitude_modulate(TO_C_DOUBLE(val1), TO_C_DOUBLE(val2), TO_C_DOUBLE(val3))));
}

static SCM g_contrast_enhancement(SCM val1, SCM val2) 
{
  #define H_contrast_enhancement "(" S_contrast_enhancement " sig index) -> sin(sig*pi/2 + index*sin(sig*2*pi))"
  SCM_ASSERT(NUMBER_P(val1), val1, SCM_ARG1, S_contrast_enhancement);
  SCM_ASSERT(NUMBER_P(val2), val2, SCM_ARG2, S_contrast_enhancement);
  return(TO_SCM_DOUBLE(mus_contrast_enhancement(TO_C_DOUBLE(val1), TO_C_DOUBLE(val2))));
}

static SCM g_dot_product(SCM val1, SCM val2) 
{
  #define H_dot_product "(" S_dot_product " v1 v2) -> sum of (vcts) v1[i]*v2[i]"
  vct *v1, *v2;
  SCM_ASSERT(VCT_P(val1), val1, SCM_ARG1, S_dot_product);
  SCM_ASSERT(VCT_P(val2), val2, SCM_ARG2, S_dot_product);
  v1 = TO_VCT(val1);
  v2 = TO_VCT(val2);
  return(scm_return_first(TO_SCM_DOUBLE(mus_dot_product(v1->data, v2->data, v1->length)), val1, val2));
}

static SCM g_sum_of_sines(SCM amps, SCM phases)
{
  #define H_sum_of_sines "(" S_sum_of_sines " amps phases) -> sum of amps[i] * sin(phases[i])"
  vct *v1, *v2;
  SCM_ASSERT(VCT_P(amps), amps, SCM_ARG1, S_sum_of_sines);
  SCM_ASSERT(VCT_P(phases), phases, SCM_ARG2, S_sum_of_sines);
  v1 = TO_VCT(amps);
  v2 = TO_VCT(phases);
  return(scm_return_first(TO_SCM_DOUBLE(mus_sum_of_sines(v1->data, v2->data, v1->length)), amps, phases));
}

static SCM g_fft_window_1(char *caller, int choice, SCM val1, SCM val2, SCM ulen) 
{
  vct *v1, *v2;
  int len;
  SCM_ASSERT(VCT_P(val1), val1, SCM_ARG1, caller);
  SCM_ASSERT(VCT_P(val2), val2, SCM_ARG2, caller);
  v1 = TO_VCT(val1);
  v2 = TO_VCT(val2);
  if (NUMBER_P(ulen)) 
    {
      len = TO_C_INT_OR_ELSE(ulen, 0); 
      if (len > v1->length) len = v1->length;
    }
  else 
    {
      len = v1->length;
      if (v2->length < len) len = v2->length;
    }
  if (choice)
    mus_multiply_arrays(v1->data, v2->data, len);
  else mus_rectangular2polar(v1->data, v2->data, len);
  return(scm_return_first(val1, val2));
}

static SCM g_multiply_arrays(SCM val1, SCM val2, SCM len) 
{
  #define H_multiply_arrays "(" S_multiply_arrays " v1 v2 &optional len) -> (vcts) v1[i] *= v2[i]"
  return(g_fft_window_1(S_multiply_arrays, TRUE, val1, val2, len));
}

static SCM g_rectangular2polar(SCM val1, SCM val2) 
{
  #define H_rectangular2polar "(" S_rectangular2polar " rl im) converts real/imaginary \
data in (vcts) rl and im from rectangular form (fft output) to polar form (a spectrum)"

  return(g_fft_window_1(S_rectangular2polar, FALSE, val1, val2, SCM_UNDEFINED));
}

static SCM g_mus_fft(SCM url, SCM uim, SCM len, SCM usign)
{
  #define H_mus_fft "(" S_mus_fft " rl im len &optional dir) returns the fft of (vcts) rl and im \
the real and imaginary parts of the data, len should be a power of 2, dir = 1 for fft, -1 for inverse-fft"

  int sign, n, np;
  Float nf;
  vct *v1, *v2;
  SCM_ASSERT((VCT_P(url)), url, SCM_ARG1, S_mus_fft);
  SCM_ASSERT((VCT_P(uim)), uim, SCM_ARG2, S_mus_fft);
  v1 = TO_VCT(url);
  v2 = TO_VCT(uim);
  if (INTEGER_P(usign)) sign = TO_C_INT(usign); else sign = 1;
  if (INTEGER_P(len)) 
    {
      n = TO_C_INT(len); 
      if (n <= 0)
	mus_misc_error(S_mus_fft, "size <= 0?", len);
      if (n > v1->length)
	n = v1->length;
    }
  else n = v1->length;
  nf = (log(n) / log(2.0));
  np = (int)nf;
  if (np != nf) n = (int)pow(2.0, np);
  mus_fft(v1->data, v2->data, n, sign);
  return(scm_return_first(url, uim));
}

static SCM g_make_fft_window(SCM type, SCM size, SCM ubeta)
{
  #define H_make_fft_window "(" S_make_fft_window " type size &optional beta) -> fft data window (vct obj) \
type is one of the sndlib fft window identifiers such as kaiser-window \
beta is the window parameter, if any:\n\
     (set! v1 (make-fft-window hamming-window 256))"

  Float beta = 0.0;
  int n, t;
  SCM_ASSERT(INTEGER_P(type), type, SCM_ARG1, S_make_fft_window);
  SCM_ASSERT(INTEGER_P(size), size, SCM_ARG2, S_make_fft_window);
  if (NUMBER_P(ubeta)) beta = TO_C_DOUBLE(ubeta);
  n = TO_C_INT(size);
  if (n <= 0)
    mus_misc_error(S_make_fft_window, "size <= 0?", size);
  t = TO_C_INT(type);
  if (MUS_FFT_WINDOW_OK(t))
    return(make_vct(n, mus_make_fft_window(t, n, beta)));
  mus_misc_error(S_make_fft_window, "unknown fft window", type);
  return(SCM_BOOL_F);
}

static SCM g_spectrum(SCM url, SCM uim, SCM uwin, SCM un, SCM utype)
{
  #define H_mus_spectrum "(" S_spectrum " rl im window &optional len type)\n\
real and imaginary data in (vcts) rl and im, returns (in rl) the spectrum thereof, \
len is the fft size (a power of 2), window is the data window (a vct object as returned by make-fft-window), \
and type determines how the spectral data is scaled:\n\
  1, the default = linear and normalized\n\
  0 = data in dB, anything else = linear and un-normalized."

  int n, type, np;
  Float nf;
  vct *v1, *v2, *v3 = NULL;
  SCM_ASSERT((VCT_P(url)), url, SCM_ARG1, S_spectrum);
  SCM_ASSERT((VCT_P(uim)), uim, SCM_ARG2, S_spectrum);
  if (NOT_FALSE_P(uwin)) SCM_ASSERT((VCT_P(uwin)), uwin, SCM_ARG3, S_spectrum);
  v1 = TO_VCT(url);
  v2 = TO_VCT(uim);
  if (NOT_FALSE_P(uwin)) v3 = TO_VCT(uwin);
  if (INTEGER_P(un)) 
    {
      n = TO_C_INT(un); 
      if (n <= 0)
	mus_misc_error(S_spectrum, "size <= 0?", un);
      if (n > v1->length)
	n = v1->length;
    }
  else n = v1->length;
  nf = (log(n) / log(2.0));
  np = (int)nf;
  if (np != nf) n = (int)pow(2.0, np);
  if (INTEGER_P(utype)) type = TO_C_INT(utype); else type = 1; /* linear normalized */
  mus_spectrum(v1->data, v2->data, (v3) ? (v3->data) : NULL, n, type);
  return(scm_return_first(url, uim, uwin));
}

static SCM g_convolution(SCM url1, SCM url2, SCM un)
{
  #define H_mus_convolution "(" S_convolution " v1 v2 &optional len) -> convolution \
of (vcts) v1 with v2, using fft of size len (a power of 2), result in v1"

  int n, np;
  Float nf;
  vct *v1, *v2;
  SCM_ASSERT((VCT_P(url1)), url1, SCM_ARG1, S_convolution);
  SCM_ASSERT((VCT_P(url2)), url2, SCM_ARG2, S_convolution);
  v1 = TO_VCT(url1);
  v2 = TO_VCT(url2);
  if (INTEGER_P(un)) 
    {
      n = TO_C_INT(un); 
      if (n <= 0)
	mus_misc_error(S_convolution, "size <= 0?", un);
      if (n > v1->length)
	n = v1->length;
      if (n > v2->length)
	n = v2->length;
    }
  else n = v1->length;
  nf = (log(n) / log(2.0));
  np = (int)nf;
  if (np != nf) n = (int)pow(2.0, np);
  mus_convolution(v1->data, v2->data, n);
  return(scm_return_first(url1, url2));
}

static SCM g_clear_array(SCM arr)
{
  #define H_clear_array "(" S_clear_array " v) clears vct v: v[i] = 0.0"
  vct *v;
  SCM_ASSERT(VCT_P(arr), arr, SCM_ARG1, S_clear_array);
  v = TO_VCT(arr);
  mus_clear_array(v->data, v->length);
  return(scm_return_first(arr));
}

static SCM g_polynomial(SCM arr, SCM x)
{
  #define H_polynomial "(" S_polynomial " coeffs x) evaluates a polynomial at x"
  vct *v;
  SCM_ASSERT(VCT_P(arr), arr, SCM_ARG1, S_polynomial);
  SCM_ASSERT(NUMBER_P(x), x, SCM_ARG2, S_polynomial);
  v = TO_VCT(arr);
  return(scm_return_first(TO_SCM_DOUBLE(mus_polynomial(v->data, TO_C_DOUBLE(x), v->length)), arr));
}

static SCM g_array_interp(SCM obj, SCM phase, SCM size) /* opt size */
{
  #define H_array_interp "(" S_array_interp " v phase &optional size) -> v[phase] \
taking into account wrap-around (size = size of data), with linear interpolation if phase is not integral."

  int len;
  vct *v;
  SCM_ASSERT(VCT_P(obj), obj, SCM_ARG1, S_array_interp);
  SCM_ASSERT(NUMBER_P(phase), phase, SCM_ARG2, S_array_interp);
  SCM_ASSERT(INTEGER_IF_BOUND_P(size), size, SCM_ARG3, S_array_interp);
  v = TO_VCT(obj);
  if (BOUND_P(size)) 
    {
      len = TO_C_INT(size); 
      if (len <= 0)
	mus_misc_error(S_array_interp, "size <= 0?", size);
      if (len > v->length) len = v->length;
    }
  else len = v->length;
  return(scm_return_first(TO_SCM_DOUBLE(mus_array_interp(v->data, TO_C_DOUBLE(phase), len)), obj));
}

static void init_simple_stuff(void)
{
  define_procedure_with_setter(S_mus_srate, SCM_FNC g_srate, H_mus_srate,
			       S_mus_set_srate, SCM_FNC g_set_srate, local_doc, 0, 0, 0, 1);

  define_procedure_with_setter(S_mus_array_print_length, SCM_FNC g_array_print_length, H_mus_array_print_length,
			       S_mus_set_array_print_length, SCM_FNC g_set_array_print_length, local_doc, 0, 0, 0, 1);

  DEFINE_PROC(S_radians_hz,           g_radians2hz, 1, 0, 0,           H_radians_hz);
  DEFINE_PROC(S_hz_radians,           g_hz2radians, 1, 0, 0,           H_hz_radians);
  DEFINE_PROC(S_in_hz,                g_hz2radians, 1, 0, 0,           H_in_hz);
  DEFINE_PROC(S_radians_degrees,      g_radians2degrees, 1, 0, 0,      H_radians_degrees);
  DEFINE_PROC(S_degrees_radians,      g_degrees2radians, 1, 0, 0,      H_degrees_radians);
  DEFINE_PROC(S_db_linear,            g_db2linear, 1, 0, 0,            H_db_linear);
  DEFINE_PROC(S_linear_db,            g_linear2db, 1, 0, 0,            H_linear_db);
  DEFINE_PROC(S_ring_modulate,        g_ring_modulate, 2, 0, 0,        H_ring_modulate);
  DEFINE_PROC(S_amplitude_modulate,   g_amplitude_modulate, 3, 0, 0,   H_amplitude_modulate);
  DEFINE_PROC(S_contrast_enhancement, g_contrast_enhancement, 2, 0, 0, H_contrast_enhancement);
  DEFINE_PROC(S_dot_product,          g_dot_product, 2, 0, 0,          H_dot_product);
  DEFINE_PROC(S_clear_array,          g_clear_array, 1, 0, 0,          H_clear_array);
  DEFINE_PROC(S_polynomial,           g_polynomial, 2, 0, 0,           H_polynomial);
  DEFINE_PROC(S_multiply_arrays,      g_multiply_arrays, 2, 1, 0,      H_multiply_arrays);
  DEFINE_PROC(S_make_fft_window,      g_make_fft_window, 2, 1, 0,      H_make_fft_window);
  DEFINE_PROC(S_mus_fft,              g_mus_fft, 3, 1, 0,              H_mus_fft);
  DEFINE_PROC(S_spectrum,             g_spectrum, 3, 2, 0,             H_mus_spectrum); 
  DEFINE_PROC(S_convolution,          g_convolution, 2, 1, 0,          H_mus_convolution);
  DEFINE_PROC(S_rectangular2polar,    g_rectangular2polar, 2, 0, 0,    H_rectangular2polar);
  DEFINE_PROC(S_array_interp,         g_array_interp, 2, 1, 0,         H_array_interp);
  DEFINE_PROC(S_sum_of_sines,         g_sum_of_sines, 2, 0, 0,         H_sum_of_sines);

  #define H_rectangular_window     "The un-window, so to speak"
  #define H_hanning_window         "A simple raised cosine window"
  #define H_welch_window           "A triangular window squared"
  #define H_parzen_window          "A triangular window"
  #define H_bartlett_window        "A triangular window"
  #define H_hamming_window         "A raised cosine"
  #define H_blackman2_window       "2nd order cosine window"
  #define H_blackman3_window       "3rd order cosine window"
  #define H_blackman4_window       "4th order cosine window"
  #define H_exponential_window     "An inverted triangle from exp"
  #define H_riemann_window         "sinc-based window"
  #define H_kaiser_window          "Bessel I0 based window"
  #define H_cauchy_window          "window based on 1/(1+sqr(angle)"
  #define H_poisson_window         "window based on exp(-angle)"
  #define H_gaussian_window        "window based on exp(-sqr(angle))"
  #define H_tukey_window           "window based on truncated cosine"
  #define H_dolph_chebychev_window "window from inverse fft"

  DEFINE_VAR(S_rectangular_window,     MUS_RECTANGULAR_WINDOW,     H_rectangular_window);
  DEFINE_VAR(S_hanning_window,         MUS_HANNING_WINDOW,         H_hanning_window);
  DEFINE_VAR(S_welch_window,           MUS_WELCH_WINDOW,           H_welch_window);
  DEFINE_VAR(S_parzen_window,          MUS_PARZEN_WINDOW,          H_parzen_window);
  DEFINE_VAR(S_bartlett_window,        MUS_BARTLETT_WINDOW,        H_bartlett_window);
  DEFINE_VAR(S_hamming_window,         MUS_HAMMING_WINDOW,         H_hamming_window);
  DEFINE_VAR(S_blackman2_window,       MUS_BLACKMAN2_WINDOW,       H_blackman2_window);
  DEFINE_VAR(S_blackman3_window,       MUS_BLACKMAN3_WINDOW,       H_blackman3_window);
  DEFINE_VAR(S_blackman4_window,       MUS_BLACKMAN4_WINDOW,       H_blackman4_window);
  DEFINE_VAR(S_exponential_window,     MUS_EXPONENTIAL_WINDOW,     H_exponential_window);
  DEFINE_VAR(S_riemann_window,         MUS_RIEMANN_WINDOW,         H_riemann_window);
  DEFINE_VAR(S_kaiser_window,          MUS_KAISER_WINDOW,          H_kaiser_window);
  DEFINE_VAR(S_cauchy_window,          MUS_CAUCHY_WINDOW,          H_cauchy_window);
  DEFINE_VAR(S_poisson_window,         MUS_POISSON_WINDOW,         H_poisson_window);
  DEFINE_VAR(S_gaussian_window,        MUS_GAUSSIAN_WINDOW,        H_gaussian_window);
  DEFINE_VAR(S_tukey_window,           MUS_TUKEY_WINDOW,           H_tukey_window);
  DEFINE_VAR(S_dolph_chebyshev_window, MUS_DOLPH_CHEBYSHEV_WINDOW, H_dolph_chebychev_window);
}


/* ---------------- mus-scm struct ---------------- */

static SND_TAG_TYPE mus_scm_tag = 0;

#define MUS_SCM_P(obj) (SMOB_TYPE_P(obj, mus_scm_tag))
#define TO_MUS_SCM(arg) ((mus_scm *)SND_VALUE_OF(arg))
int mus_scm_p(SCM obj) {return(MUS_SCM_P(obj));}

#define TO_CLM(obj) ((mus_any *)((TO_MUS_SCM(obj))->gen))
mus_any *mus_scm_to_clm(SCM obj) {return(TO_CLM(obj));}

static SCM mark_mus_scm(SCM obj) 
{
  int i;
  mus_scm *ms;
  ms = TO_MUS_SCM(obj);
  if (ms->vcts) 
    {
      for (i = 0; i < ms->nvcts; i++) 
	if (ms->vcts[i]) 
	  scm_gc_mark(ms->vcts[i]);
    }
  SND_SETGCMARK(obj); 
  return(SCM_BOOL_F);
}

static scm_sizet free_mus_scm(SCM obj) 
{
  mus_scm *ms;
  ms = TO_MUS_SCM(obj);
  if (ms->nvcts != -1) mus_free(TO_CLM(obj));
  if (ms->vcts) FREE(ms->vcts);  
  FREE(ms);
  return(0);
}

static int print_mus_scm(SCM obj, SCM port, scm_print_state *pstate)
{
  char *buf;
  buf = mus_describe(TO_CLM(obj));
  if (buf)
    {
      scm_puts(buf, port);
      FREE(buf);
    }
  return(1);
}

static SCM equalp_mus_scm(SCM obj1, SCM obj2) 
{
  return((mus_equalp(TO_CLM(obj1), TO_CLM(obj2))) ? SCM_BOOL_T : SCM_BOOL_F);
}

#if HAVE_APPLICABLE_SMOB
static SCM mus_scm_apply(SCM gen, SCM arg1, SCM arg2)
{
  return(TO_SCM_DOUBLE(mus_run(TO_CLM(gen),
			       (NUMBER_P(arg1)) ? TO_C_DOUBLE(arg1) : 0.0,
			       (NUMBER_P(arg2)) ? TO_C_DOUBLE(arg2) : 0.0)));
}
#endif

static void init_mus_scm(void)
{
  mus_scm_tag = scm_make_smob_type("mus", sizeof(mus_scm));
  scm_set_smob_mark(mus_scm_tag, mark_mus_scm);
  scm_set_smob_print(mus_scm_tag, print_mus_scm);
  scm_set_smob_free(mus_scm_tag, free_mus_scm);
  scm_set_smob_equalp(mus_scm_tag, equalp_mus_scm);
#if HAVE_APPLICABLE_SMOB
  scm_set_smob_apply(mus_scm_tag, SCM_FNC mus_scm_apply, 0, 2, 0);
#endif
}

SCM mus_scm_to_smob(mus_scm *gn)
{
  SND_RETURN_NEWSMOB(mus_scm_tag, gn);
}

#define MUS_DATA_POSITION 0

SCM mus_scm_to_smob_with_vct(mus_scm *gn, SCM v)
{
  SCM new_dly;
  SCM_NEWSMOB(new_dly, mus_scm_tag, gn);
  gn->vcts[MUS_DATA_POSITION] = v;
  return(new_dly);
}



/* ---------------- generic functions ---------------- */

#define S_mus_phase          "mus-phase"
#define S_mus_set_phase      "mus-set-phase"
#define S_mus_frequency      "mus-frequency"
#define S_mus_set_frequency  "mus-set-frequency"
#define S_mus_length         "mus-length"
#define S_mus_set_length     "mus-set-length"
#define S_mus_data           "mus-data"
#define S_mus_set_data       "mus-set-data"
#define S_mus_scaler         "mus-scaler"
#define S_mus_set_scaler     "mus-set-scaler"
#define S_mus_inspect        "mus-inspect"
#define S_mus_describe       "mus-describe"
#define S_mus_name           "mus-name"
#define S_mus_run            "mus-run"
#define S_mus_bank           "mus-bank"

static SCM g_inspect(SCM gen)
{
  #define H_mus_inspect "(" S_mus_inspect " gen) -> the internal state of gen"
  char *buf;
  SCM result;
  SCM_ASSERT((MUS_SCM_P(gen)), gen, SCM_ARG1, S_mus_inspect);
  buf = mus_inspect(TO_CLM(gen));
  result = TO_SCM_STRING(buf);
  FREE(buf);
  return(result);
}

static SCM g_describe(SCM gen) 
{
  #define H_mus_describe "(" S_mus_describe " gen) -> the user's view of the state of gen"
  char *buf;
  SCM result;
  SCM_ASSERT((MUS_SCM_P(gen)), gen, SCM_ARG1, S_mus_describe);
  buf = mus_describe(TO_CLM(gen));
  result = TO_SCM_STRING(buf);
  FREE(buf);
  return(result);
}

static SCM g_phase(SCM gen) 
{
  #define H_mus_phase "(" S_mus_phase " gen) -> gen's current phase (radians)"
  SCM_ASSERT(MUS_SCM_P(gen), gen, SCM_ARG1, S_mus_phase);
  return(TO_SCM_DOUBLE(mus_phase(TO_CLM(gen))));
}

static SCM g_set_phase(SCM gen, SCM val) 
{
  SCM_ASSERT(MUS_SCM_P(gen), gen, SCM_ARG1, S_mus_set_phase);
  SCM_ASSERT(NUMBER_P(val), val, SCM_ARG2, S_mus_set_phase);
  return(TO_SCM_DOUBLE(mus_set_phase(TO_CLM(gen), TO_C_DOUBLE(val))));
}

static SCM g_scaler(SCM gen) 
{
  #define H_mus_scaler "(" S_mus_scaler " gen) -> gen's scaler, if any"
  SCM_ASSERT(MUS_SCM_P(gen), gen, SCM_ARG1, S_mus_scaler);
  return(TO_SCM_DOUBLE(mus_scaler(TO_CLM(gen))));
}

static SCM g_set_scaler(SCM gen, SCM val) 
{
  SCM_ASSERT(MUS_SCM_P(gen), gen, SCM_ARG1, S_mus_set_scaler);
  SCM_ASSERT(NUMBER_P(val), val, SCM_ARG2, S_mus_set_scaler);
  return(TO_SCM_DOUBLE(mus_set_scaler(TO_CLM(gen), TO_C_DOUBLE(val))));
}

static SCM g_frequency(SCM gen) 
{
  #define H_mus_frequency "(" S_mus_frequency " gen) -> gen's frequency (Hz)"
  SCM_ASSERT(MUS_SCM_P(gen), gen, SCM_ARG1, S_mus_frequency);
  return(TO_SCM_DOUBLE(mus_frequency(TO_CLM(gen))));
}

static SCM g_set_frequency(SCM gen, SCM val) 
{
  SCM_ASSERT(MUS_SCM_P(gen), gen, SCM_ARG1, S_mus_frequency);
  SCM_ASSERT(NUMBER_P(val), val, SCM_ARG2, S_mus_set_frequency);
  return(TO_SCM_DOUBLE(mus_set_frequency(TO_CLM(gen), TO_C_DOUBLE(val))));
}

static SCM g_run(SCM gen, SCM arg1, SCM arg2) 
{
  #define H_mus_run "(" S_mus_run " gen &optional arg1 arg2) -> apply gen to arg1 and arg2"
  SCM_ASSERT(MUS_SCM_P(gen), gen, SCM_ARG1, S_mus_run);
  return(TO_SCM_DOUBLE(mus_run(TO_CLM(gen),
			       (NUMBER_P(arg1)) ? TO_C_DOUBLE(arg1) : 0.0,
			       (NUMBER_P(arg2)) ? TO_C_DOUBLE(arg2) : 0.0)));
}

static SCM g_length(SCM gen) 
{
  #define H_mus_length "(" S_mus_length " gen) -> gen's length, if any"
  SCM_ASSERT(MUS_SCM_P(gen), gen, SCM_ARG1, S_mus_length);
  return(TO_SMALL_SCM_INT(mus_length(TO_CLM(gen))));
}

static SCM g_set_length(SCM gen, SCM val) 
{
  SCM_ASSERT(MUS_SCM_P(gen), gen, SCM_ARG1, S_mus_length);
  SCM_ASSERT(NUMBER_P(val), val, SCM_ARG2, S_mus_set_length);
  return(TO_SMALL_SCM_INT(mus_set_length(TO_CLM(gen), TO_C_INT_OR_ELSE(val, 0))));
}

static SCM g_name(SCM gen) 
{
  #define H_mus_name "(" S_mus_name " gen) -> gen's name, if any"
  SCM_ASSERT(MUS_SCM_P(gen), gen, SCM_ARG1, S_mus_name);
  return(TO_SCM_STRING(mus_name(TO_CLM(gen))));
}

static SCM g_data(SCM gen) 
{
  #define H_mus_data "(" S_mus_data " gen) -> gen's internal data (vct), if any"
  mus_scm *ms;
  SCM_ASSERT(MUS_SCM_P(gen), gen, SCM_ARG1, S_mus_data);
  ms = TO_MUS_SCM(gen);
  if (ms->vcts)
    return(ms->vcts[MUS_DATA_POSITION]); 
  else return(SCM_BOOL_F);
}

static SCM g_set_data(SCM gen, SCM val) 
{
  mus_scm *ms;
  mus_any *ma;
  vct *v;
  SCM_ASSERT(MUS_SCM_P(gen), gen, SCM_ARG1, S_mus_set_data);
  SCM_ASSERT((VCT_P(val)), val, SCM_ARG2, S_mus_set_data);
  ms = TO_MUS_SCM(gen);
  if (ms->vcts)
    {
      v = (vct *)SND_VALUE_OF(val);
      ma = ms->gen;
      mus_set_data(ma, v->data);  /* TO REMEMBER: if allocated, should have freed, and set to not allocated */
      ms->vcts[MUS_DATA_POSITION] = val;
      return(val);
    }
  return(scm_return_first(SCM_BOOL_F, gen, val));
}

static Float *copy_vct_data(vct *v)
{
  Float *line = NULL;
  int i;
  line = (Float *)CALLOC(v->length, sizeof(Float));
  if (line) 
    for (i = 0; i < v->length; i++) line[i] = v->data[i];
  return(line);
}

static Float *whatever_to_floats(SCM inp, int size, const char *caller)
{
  Float *invals = NULL;
  int i;
  SCM *data;
  Float inval;
  if (size <= 0) return(NULL);
  if (VECTOR_P(inp))
    {
      invals = (Float *)CALLOC(size, sizeof(Float));
      data = SCM_VELTS(inp);
      for (i = 0; i < size; i++) 
	invals[i] = TO_C_DOUBLE(data[i]);
    }
  else
    {
      if (VCT_P(inp))
	invals = copy_vct_data(TO_VCT(inp));
      else
	{
	  if (NUMBER_P(inp)) 
	    {
	      invals = (Float *)CALLOC(size, sizeof(Float));
	      inval = TO_C_DOUBLE(inp);
	      for (i = 0; i < size; i++) 
		invals[i] = inval;
	    }
	  else
	    {
	      if (procedure_fits(inp, 1))
		{
		  invals = (Float *)CALLOC(size, sizeof(Float));
		  for (i = 0; i < size; i++) 
		    invals[i] = TO_C_DOUBLE(CALL1(inp, TO_SCM_INT(i), caller));
		}
	    }
	}
    }
  return(invals);
}


static SCM g_mus_bank(SCM gens, SCM amps, SCM inp, SCM inp2)
{
  #define H_mus_bank "(" S_mus_bank " gens amps &optional args1 args2) -> sum of (* (amps i) ((gens i) (args1 i) (args2 i)))"
  /* amps and inp1/inp2 can be a Float, a vct object, a function, or a vector of Floats */
  Float outval = 0.0;
  int i, size;
  Float *scls = NULL, *invals = NULL, *invals2 = NULL;
  mus_any **gs;
  SCM *data;
  size = VECTOR_LENGTH(gens);
  scls = whatever_to_floats(amps, size, S_mus_bank);
  if (scls == NULL)
    scm_wrong_type_arg(S_mus_bank, 2, amps);
  gs = (mus_any **)CALLOC(size, sizeof(mus_any *));
  data = SCM_VELTS(gens);
  for (i = 0; i < size; i++) 
    if (MUS_SCM_P(data[i]))
      gs[i] = TO_CLM(data[i]);
    else 
      {
	if (scls) FREE(scls);
	if (gs) FREE(gs);
	mus_misc_error(S_mus_bank, "not a generator!", data[i]);
      }
  invals = whatever_to_floats(inp, size, S_mus_bank);
  invals2 = whatever_to_floats(inp2, size, S_mus_bank);
  outval = mus_bank(gs, scls, invals, invals2, size);
  if (scls) FREE(scls);
  if (invals) FREE(invals);
  if (invals2) FREE(invals2);
  if (gs) FREE(gs);
  return(TO_SCM_DOUBLE(outval));
}

static void init_generic_funcs(void)
{
  DEFINE_PROC(S_mus_inspect,  g_inspect, 1, 0, 0,  H_mus_inspect);
  DEFINE_PROC(S_mus_describe, g_describe, 1, 0, 0, H_mus_describe);
  DEFINE_PROC(S_mus_name,     g_name, 1, 0, 0,     H_mus_name);
  DEFINE_PROC(S_mus_run,      g_run, 1, 2, 0,      H_mus_run);
  DEFINE_PROC(S_mus_bank,     g_mus_bank, 2, 2, 0, H_mus_bank);

  define_procedure_with_setter(S_mus_phase, SCM_FNC g_phase, H_mus_phase,
			       S_mus_set_phase, SCM_FNC g_set_phase, local_doc, 1, 0, 2, 0);

  define_procedure_with_setter(S_mus_scaler, SCM_FNC g_scaler, H_mus_scaler,
			       S_mus_set_scaler, SCM_FNC g_set_scaler, local_doc, 1, 0, 2, 0);

  define_procedure_with_setter(S_mus_frequency, SCM_FNC g_frequency, H_mus_frequency,
			       S_mus_set_frequency, SCM_FNC g_set_frequency, local_doc, 1, 0, 2, 0);

  define_procedure_with_setter(S_mus_length, SCM_FNC g_length, H_mus_length,
			       S_mus_set_length, SCM_FNC g_set_length, local_doc, 1, 0, 2, 0);

  define_procedure_with_setter(S_mus_data, SCM_FNC g_data, H_mus_data,
			       S_mus_set_data, SCM_FNC g_set_data, local_doc, 1, 0, 2, 0);
}



/* ---------------- oscil ---------------- */

#define S_make_oscil "make-oscil"
#define S_oscil      "oscil"
#define S_oscil_bank "oscil-bank"
#define S_oscil_p    "oscil?"
#define S_mus_apply  "mus-apply"

static SCM g_make_oscil(SCM arg1, SCM arg2, SCM arg3, SCM arg4)
{
  #define H_make_oscil "(" S_make_oscil " &opt-key (frequency 440.0) (phase 0.0)) -> a new " S_oscil " (sinewave) generator"
  mus_scm *gn;
  int vals;
  SCM args[4], keys[2];
  int orig_arg[2] = {0, 0};
  Float freq = 440.0, phase = 0.0;
  keys[0] = all_keys[C_frequency];
  keys[1] = all_keys[C_initial_phase];
  args[0] = arg1; args[1] = arg2; args[2] = arg3; args[3] = arg4; 
  vals = decode_keywords(S_make_oscil, 2, keys, 4, args, orig_arg);
  if (vals > 0)
    {
      freq = fkeyarg(keys[0], S_make_oscil, orig_arg[0] + 1, args[orig_arg[0]], freq);
      phase = fkeyarg(keys[1], S_make_oscil, orig_arg[1] + 1, args[orig_arg[1]], phase);
    }
  gn = (mus_scm *)CALLOC(1, sizeof(mus_scm));
  gn->gen = mus_make_oscil(freq, phase);
  gn->nvcts = 0;
  return(mus_scm_to_smob(gn));
}

static SCM g_oscil(SCM os, SCM fm, SCM pm)
{
  #define H_oscil "(" S_oscil " gen &optional fm pm) -> next sample from " S_oscil " gen: rtn = sin(phase+pm) phase+=(freq+fm)"
  Float fm1 = 0.0, pm1 = 0.0;
  SCM_ASSERT((MUS_SCM_P(os)) && (mus_oscil_p(TO_CLM(os))), os, SCM_ARG1, S_oscil);
  if (NUMBER_P(fm)) fm1 = TO_C_DOUBLE(fm); else if (BOUND_P(fm)) scm_wrong_type_arg(S_oscil, 2, fm);
  if (NUMBER_P(pm)) pm1 = TO_C_DOUBLE(pm); else if (BOUND_P(pm)) scm_wrong_type_arg(S_oscil, 3, pm);
  return(TO_SCM_DOUBLE(mus_oscil(TO_CLM(os), fm1, pm1)));
}

static SCM g_oscil_bank(SCM amps, SCM gens, SCM inp, SCM size)
{
  /* size currently ignored */
  #define H_oscil_bank "(" S_oscil_bank " scls gens fms) -> sum a bank of " S_oscil "s: scls[i]*" S_oscil "(gens[i], fms[i])"
  SCM_ASSERT(VECTOR_P(gens), gens, SCM_ARG2, S_oscil_bank);
  return(g_mus_bank(gens, amps, inp, SCM_UNDEFINED));
}

static SCM g_oscil_p(SCM os) 
{
  #define H_oscil_p "(" S_oscil_p " gen) -> #t if gen is an " S_oscil " generator, else #f"
  return(((MUS_SCM_P(os)) && (mus_oscil_p(TO_CLM(os)))) ? SCM_BOOL_T : SCM_BOOL_F);
}

static SCM g_mus_apply(SCM arglist)
{
  #define H_mus_apply "(" S_mus_apply " gen &rest args) applies gen to args"
  int arglist_len;
  mus_any *gen;
  arglist_len = LIST_LENGTH(arglist);
  if ((arglist_len > 3) || (arglist_len == 0)) 
    return(TO_SCM_DOUBLE(0.0));
  gen = TO_CLM(SCM_CAR(arglist));
  if (arglist_len == 1) 
    return(TO_SCM_DOUBLE(mus_apply(gen)));
  if (arglist_len == 2)
    return(TO_SCM_DOUBLE(mus_apply(gen, 
				   TO_C_DOUBLE(SCM_CADR(arglist)))));
  return(TO_SCM_DOUBLE(mus_apply(gen, 
				 TO_C_DOUBLE(SCM_CADR(arglist)), 
				 TO_C_DOUBLE(SCM_CADDR(arglist)))));
}

static void init_oscil(void)
{
  DEFINE_PROC(S_oscil_p,    g_oscil_p, 1, 0, 0,    H_oscil_p);
  DEFINE_PROC(S_make_oscil, g_make_oscil, 0, 4, 0, H_make_oscil);
  DEFINE_PROC(S_oscil,      g_oscil, 1, 2, 0,      H_oscil);
  DEFINE_PROC(S_oscil_bank, g_oscil_bank, 2, 2, 0, H_oscil_bank);
  DEFINE_PROC(S_mus_apply,  g_mus_apply, 0, 0, 1,  H_mus_apply);
}



/* ---------------- delay ---------------- */

#define S_make_delay "make-delay"
#define S_delay "delay"
#define S_delay_p "delay?"
#define S_tap "tap"
#define S_comb "comb"
#define S_make_comb "make-comb"
#define S_comb_p "comb?"
#define S_notch "notch"
#define S_make_notch "make-notch"
#define S_notch_p "notch?"
#define S_all_pass "all-pass"
#define S_make_all_pass "make-all-pass"
#define S_all_pass_p "all-pass?"
#define S_mus_feedback "mus-feedback"
#define S_mus_set_feedback "mus-set-feedback"
#define S_mus_feedforward "mus-feedforward"
#define S_mus_set_feedforward "mus-set-feedforward"

enum {G_DELAY, G_COMB, G_NOTCH, G_ALL_PASS};

static SCM g_make_delay_1(int choice, SCM arglist)
{
  mus_scm *gn;
  char *caller = NULL;
  SCM args[14], keys[7];
  int orig_arg[7] = {0, 0, 0, 0, 0, 0, 0};
  int vals, i, argn = 0, len, arglist_len, keyn, max_size = -1;
  int size = 1, size_key = 0;
  Float *line = NULL;
  Float scaler = 0.0, feedback = 0.0, feedforward = 0.0;
  SCM initial_contents = SCM_UNDEFINED, lst;
  Float initial_element = 0.0;
  
  switch (choice)
    {
    case G_DELAY:   caller = S_make_delay;                                       break;
    case G_COMB:    caller = S_make_comb;     keys[argn++] = all_keys[C_scaler]; break;
    case G_NOTCH:   caller = S_make_notch;    keys[argn++] = all_keys[C_scaler]; break;
    case G_ALL_PASS: caller = S_make_all_pass; keys[argn++] = all_keys[C_feedback]; keys[argn++] = all_keys[C_feedforward]; break;
    }
  keys[argn++] = all_keys[C_size];
  keys[argn++] = all_keys[C_initial_contents];
  keys[argn++] = all_keys[C_initial_element];
  keys[argn++] = all_keys[C_max_size];
  for (i = 0; i < 14; i++) args[i] = SCM_UNDEFINED;
  arglist_len = LIST_LENGTH(arglist);
  for (i = 0; i < arglist_len; i++) args[i] = LIST_REF(arglist, i);
  vals = decode_keywords(caller, argn, keys, argn*2, args, orig_arg);
  if (vals > 0)
    {
      keyn = 0;
      switch (choice)
	{
	case G_DELAY: 
	  break;
	case G_COMB: case G_NOTCH:
	  scaler = fkeyarg(keys[keyn], caller, orig_arg[keyn] + 1, args[orig_arg[keyn]], scaler);
	  keyn++;
	  break;
	case G_ALL_PASS:
	  feedback = fkeyarg(keys[keyn], caller, orig_arg[keyn] + 1, args[orig_arg[keyn]], feedback);
	  keyn++;
	  feedforward = fkeyarg(keys[keyn], caller, orig_arg[keyn] + 1, args[orig_arg[keyn]], feedforward);
	  keyn++;
	  break;
	}
      size = ikeyarg(keys[keyn], caller, orig_arg[keyn] + 1, args[orig_arg[keyn]], size);
      size_key = keyn;
      keyn++;
      if (!(KEYWORD_P(keys[keyn])))
	{
	  initial_contents = keys[keyn];
	  if (VCT_P(initial_contents))
	    line = copy_vct_data(TO_VCT(initial_contents));
	  else
	    {
	      if (LIST_P_WITH_LENGTH(initial_contents, len))
		{
		  if (len == 0) 
		    mus_misc_error(caller, "initial-contents empty?", initial_contents);
		  line = (Float *)CALLOC(len, sizeof(Float));
		  for (i = 0, lst = initial_contents; (i < len) && (NOT_NULL_P(lst)); i++, lst = SCM_CDR(lst)) 
		    line[i] = TO_C_DOUBLE(SCM_CAR(lst));
		}
	    }
	}
      keyn++;
      initial_element = fkeyarg_or_error(keys[keyn], caller, orig_arg[keyn] + 1, args[orig_arg[keyn]], 0.0);
      if (SCM_EQ_P(initial_element, MUS_MISC_ERROR))
	{
	  if (line) FREE(line);
	  scm_wrong_type_arg(caller, orig_arg[keyn] + 1, args[orig_arg[keyn]]);
	}
      keyn++;
      max_size = ikeyarg_or_error(keys[keyn], caller, orig_arg[keyn] + 1, args[orig_arg[keyn]], size);
      if (SCM_EQ_P(max_size, MUS_MISC_ERROR))
	{
	  if (line) FREE(line);
	  scm_wrong_type_arg(caller, orig_arg[keyn] + 1, args[orig_arg[keyn]]);
	}
    }
  if (size < 0)
    {
      if (line) FREE(line);
      mus_misc_error(caller, "size < 0?", keys[size_key]);
    }
  if (max_size == -1) max_size = size;
  if (max_size <= 0)
    {
      if (line) FREE(line);
      mus_misc_error(caller, "invalid delay length", TO_SCM_INT(max_size));
    }
  if (line == NULL)
    {
      line = (Float *)CALLOC(max_size, sizeof(Float));
      if (initial_element != 0.0) 
	for (i = 0; i < max_size; i++) 
	  line[i] = initial_element;
    }
  gn = (mus_scm *)CALLOC(1, sizeof(mus_scm));
  gn->vcts = (SCM *)CALLOC(1, sizeof(SCM));
  gn->vcts[0] = SCM_EOL;
  gn->nvcts = 1;
  switch (choice)
    {
    case G_DELAY: gn->gen = mus_make_delay(size, line, max_size); break;
    case G_COMB: gn->gen = mus_make_comb(scaler, size, line, max_size); break;
    case G_NOTCH: gn->gen = mus_make_notch(scaler, size, line, max_size); break;
    case G_ALL_PASS: gn->gen = mus_make_all_pass(feedback, feedforward, size, line, max_size); break;
    }
  return(mus_scm_to_smob_with_vct(gn, make_vct(max_size, line)));
}

static SCM g_make_delay(SCM args) 
{
  #define H_make_delay "(" S_make_delay " &opt-key size initial-contents (initial-element 0.0) max-size)\n\
returns a new delay line of size elements. \
If the delay length will be changing, max-size determines its maximum length\n\
   (" S_make_delay " len :max-size (+ len 10))\n\
provides 10 extra elements of delay for subsequent phasing. \
initial-contents can be either a list or a vct object."

  return(g_make_delay_1(G_DELAY, args));
}

static SCM g_make_comb(SCM args) 
{
  #define H_make_comb "(" S_make_comb " &opt-key scaler size initial-contents (initial-element 0.0) max-size)\n\
returns a new comb filter (a delay line with a scaler on the feedback) of size elements. \
If the comb length will be changing, max-size determines its maximum length. \
initial-contents can be either a list or a vct object."

  return(g_make_delay_1(G_COMB, args));
}

static SCM g_make_notch(SCM args) 
{
  #define H_make_notch "(" S_make_notch " &opt-key scaler size initial-contents (initial-element 0.0) max-size)\n\
returns a new notch filter (a delay line with a scaler on the feedforward) of size elements. \
If the notch length will be changing, max-size determines its maximum length. \
initial-contents can be either a list or a vct object."

  return(g_make_delay_1(G_NOTCH, args));
}

static SCM g_make_all_pass(SCM args) 
{
  #define H_make_all_pass "(" S_make_all_pass " &opt-key feedback feedforward size initial-contents (initial-element 0.0) max-size)\n\
returns a new allpass filter (a delay line with a scalers on both the feedback and the feedforward). \
If the all-pass length will be changing, max-size determines its maximum length. \
initial-contents can be either a list or a vct object."

  return(g_make_delay_1(G_ALL_PASS, args));
}

static SCM g_delay(SCM obj, SCM input, SCM pm)
{
  #define H_delay "(" S_delay " gen &optional (val 0.0) (pm 0.0))\n\
delays val according to the delay line's length and pm ('phase-modulation'). \
The Scheme built-in 'delay' function is named %delay. \
If pm is greater than 0.0, the max-size argument used to create gen should have accommodated its maximum value."

  Float in1 = 0.0, pm1 = 0.0;
  SCM_ASSERT((MUS_SCM_P(obj)) && (mus_delay_p(TO_CLM(obj))), obj, SCM_ARG1, S_delay);
  if (NUMBER_P(input)) in1 = TO_C_DOUBLE(input); else if (BOUND_P(input)) scm_wrong_type_arg(S_delay, 2, input);
  if (NUMBER_P(pm)) pm1 = TO_C_DOUBLE(pm); else if (BOUND_P(pm)) scm_wrong_type_arg(S_delay, 3, pm);
  return(TO_SCM_DOUBLE(mus_delay(TO_CLM(obj), in1, pm1)));
}

static SCM g_notch(SCM obj, SCM input, SCM pm)
{
  #define H_notch "(" S_notch " gen &optional (val 0.0) (pm 0.0)) notch filters val, pm changes the delay length."
  Float in1 = 0.0, pm1 = 0.0;
  SCM_ASSERT((MUS_SCM_P(obj)) && (mus_notch_p(TO_CLM(obj))), obj, SCM_ARG1, S_notch);
  if (NUMBER_P(input)) in1 = TO_C_DOUBLE(input); else if (BOUND_P(input)) scm_wrong_type_arg(S_notch, 2, input);
  if (NUMBER_P(pm)) pm1 = TO_C_DOUBLE(pm); else if (BOUND_P(pm)) scm_wrong_type_arg(S_notch, 3, pm);
  return(TO_SCM_DOUBLE(mus_notch(TO_CLM(obj), in1, pm1)));
}

static SCM g_comb(SCM obj, SCM input, SCM pm)
{
  #define H_comb "(" S_comb " gen &optional (val 0.0) (pm 0.0)) comb filters val, pm changes the delay length."
  Float in1 = 0.0, pm1 = 0.0;
  SCM_ASSERT((MUS_SCM_P(obj)) && (mus_comb_p(TO_CLM(obj))), obj, SCM_ARG1, S_comb);
  if (NUMBER_P(input)) in1 = TO_C_DOUBLE(input); else if (BOUND_P(input)) scm_wrong_type_arg(S_comb, 2, input);
  if (NUMBER_P(pm)) pm1 = TO_C_DOUBLE(pm); else if (BOUND_P(pm)) scm_wrong_type_arg(S_comb, 3, pm);
  return(TO_SCM_DOUBLE(mus_comb(TO_CLM(obj), in1, pm1)));
}

static SCM g_all_pass(SCM obj, SCM input, SCM pm)
{
  #define H_all_pass "(" S_all_pass " gen &optional (val 0.0) (pm 0.0)) all-pass filters val, pm changes the delay length."
  Float in1 = 0.0, pm1 = 0.0;
  SCM_ASSERT((MUS_SCM_P(obj)) && (mus_all_pass_p(TO_CLM(obj))), obj, SCM_ARG1, S_all_pass);
  if (NUMBER_P(input)) in1 = TO_C_DOUBLE(input); else if (BOUND_P(input)) scm_wrong_type_arg(S_all_pass, 2, input);
  if (NUMBER_P(pm)) pm1 = TO_C_DOUBLE(pm); else if (BOUND_P(pm)) scm_wrong_type_arg(S_all_pass, 3, pm);
  return(TO_SCM_DOUBLE(mus_all_pass(TO_CLM(obj), in1, pm1)));
}

static SCM g_tap(SCM obj, SCM loc)
{
  #define H_tap "(" S_tap " gen &optional (pm 0.0)) taps the " S_delay " generator offset by pm"
  Float dloc = 0.0;
  SCM_ASSERT((MUS_SCM_P(obj)), obj, SCM_ARG1, S_tap);
  if (NUMBER_P(loc)) dloc = TO_C_DOUBLE(loc); else if (BOUND_P(loc)) scm_wrong_type_arg(S_tap, 2, loc);
  return(TO_SCM_DOUBLE(mus_tap(TO_CLM(obj), dloc)));
}

static SCM g_delay_p(SCM obj) 
{
  #define H_delay_p "(" S_delay_p " gen) -> #t if gen is a delay line, else #f"
  return(((MUS_SCM_P(obj)) && (mus_delay_p(TO_CLM(obj)))) ? SCM_BOOL_T : SCM_BOOL_F);
}

static SCM g_comb_p(SCM obj)
{
  #define H_comb_p "(" S_comb_p " gen) -> #t if gen is a comb filter, else #f"
  return(((MUS_SCM_P(obj)) && (mus_comb_p(TO_CLM(obj)))) ? SCM_BOOL_T : SCM_BOOL_F);
}

static SCM g_notch_p(SCM obj) 
{
  #define H_notch_p "(" S_notch_p " gen) -> #t if gen is a notch filter, else #f"
  return(((MUS_SCM_P(obj)) && (mus_notch_p(TO_CLM(obj)))) ? SCM_BOOL_T : SCM_BOOL_F);
}

static SCM g_all_pass_p(SCM obj) 
{
  #define H_all_pass_p "(" S_all_pass_p " gen) -> #t if gen is an all-pass filter, else #f"
  return(((MUS_SCM_P(obj)) && (mus_all_pass_p(TO_CLM(obj)))) ? SCM_BOOL_T : SCM_BOOL_F);
}

static SCM g_feedback(SCM obj)
{
  #define H_mus_feedback "(" S_mus_feedback " gen) -> feedback value of gen"
  SCM_ASSERT(MUS_SCM_P(obj), obj, SCM_ARG1, S_mus_feedback);
  return(TO_SCM_DOUBLE(mus_feedback(TO_CLM(obj))));
}

static SCM g_set_feedback(SCM obj, SCM val)
{
  SCM_ASSERT(MUS_SCM_P(obj), obj, SCM_ARG1, S_mus_set_feedback);
  SCM_ASSERT(NUMBER_P(val), val, SCM_ARG2, S_mus_set_feedback);
  return(TO_SCM_DOUBLE(mus_set_feedback(TO_CLM(obj), TO_C_DOUBLE(val))));
}

static SCM g_feedforward(SCM obj)
{
  #define H_mus_feedforward "(" S_mus_feedforward " gen) -> feedforward term of gen"
  SCM_ASSERT(MUS_SCM_P(obj), obj, SCM_ARG1, S_mus_feedforward);
  return(TO_SCM_DOUBLE(mus_feedforward(TO_CLM(obj))));
}

static SCM g_set_feedforward(SCM obj, SCM val)
{
  SCM_ASSERT(MUS_SCM_P(obj), obj, SCM_ARG1, S_mus_set_feedforward);
  SCM_ASSERT(NUMBER_P(val), val, SCM_ARG2, S_mus_set_feedforward);
  return(TO_SCM_DOUBLE(mus_set_feedforward(TO_CLM(obj), TO_C_DOUBLE(val))));
}

static void init_dly(void)
{
  EVAL_STRING("(define %delay delay)"); /* protect the original meaning */
  DEFINE_PROC(S_make_delay,    g_make_delay, 0, 0, 1,    H_make_delay);
  DEFINE_PROC(S_make_comb,     g_make_comb, 0, 0, 1,     H_make_comb);
  DEFINE_PROC(S_make_notch,    g_make_notch, 0, 0, 1,    H_make_notch); 
  DEFINE_PROC(S_make_all_pass, g_make_all_pass, 0, 0, 1, H_make_all_pass);
  DEFINE_PROC(S_delay,         g_delay, 1, 2, 0,         H_delay); 
  DEFINE_PROC(S_tap,           g_tap, 1, 1, 0,           H_tap);
  DEFINE_PROC(S_notch,         g_notch, 1, 2, 0,         H_notch);
  DEFINE_PROC(S_comb,          g_comb, 1, 2, 0,          H_comb);
  DEFINE_PROC(S_all_pass,      g_all_pass, 1, 2, 0,      H_all_pass);
  DEFINE_PROC(S_delay_p,       g_delay_p, 1, 0, 0,       H_delay_p);
  DEFINE_PROC(S_notch_p,       g_notch_p, 1, 0, 0,       H_notch_p);
  DEFINE_PROC(S_comb_p,        g_comb_p, 1, 0, 0,        H_comb_p);
  DEFINE_PROC(S_all_pass_p,    g_all_pass_p, 1, 0, 0,    H_all_pass_p);

  define_procedure_with_setter(S_mus_feedback, SCM_FNC g_feedback, H_mus_feedback,
			       S_mus_set_feedback, SCM_FNC g_set_feedback, local_doc, 1, 0, 2, 0);

  define_procedure_with_setter(S_mus_feedforward, SCM_FNC g_feedforward, H_mus_feedforward,
			       S_mus_set_feedforward, SCM_FNC g_set_feedforward, local_doc, 1, 0, 2, 0);
}


/* -------- sum-of-cosines -------- */

#define S_make_sum_of_cosines "make-sum-of-cosines"
#define S_sum_of_cosines      "sum-of-cosines"
#define S_sum_of_cosines_p    "sum-of-cosines?"
#define S_mus_cosines         "mus-cosines"

static SCM g_sum_of_cosines_p(SCM obj) 
{
  #define H_sum_of_cosines_p "(" S_sum_of_cosines_p " gen) -> #t if gen is a " S_sum_of_cosines " generator, else #f"
  return(((MUS_SCM_P(obj)) && (mus_sum_of_cosines_p(TO_CLM(obj)))) ? SCM_BOOL_T : SCM_BOOL_F);
}

static SCM g_make_sum_of_cosines(SCM arg1, SCM arg2, SCM arg3, SCM arg4, SCM arg5, SCM arg6)
{
  #define H_make_sum_of_cosines "(" S_make_sum_of_cosines " &opt-key (frequency 440.0) (initial-phase 0.0) (cosines 1))\n\
returns a new " S_sum_of_cosines " generator, producing a band-limited pulse train."

  mus_scm *gn;
  SCM args[6], keys[3];
  int orig_arg[3] = {0, 0, 0};
  int vals;
  int cosines = 1;
  Float freq = 440.0;
  Float phase = 0.0;
  keys[0] = all_keys[C_cosines];
  keys[1] = all_keys[C_frequency];
  keys[2] = all_keys[C_initial_phase];
  args[0] = arg1; args[1] = arg2; args[2] = arg3; args[3] = arg4; args[4] = arg5; args[5] = arg6; 
  vals = decode_keywords(S_make_sum_of_cosines, 3, keys, 6, args, orig_arg);
  if (vals > 0)
    {
      cosines = ikeyarg(keys[0], S_make_sum_of_cosines, orig_arg[0] + 1, args[orig_arg[0]], cosines);
      freq = fkeyarg(keys[1], S_make_sum_of_cosines, orig_arg[1] + 1, args[orig_arg[1]], freq);
      phase = fkeyarg(keys[2], S_make_sum_of_cosines, orig_arg[2] + 1, args[orig_arg[2]], phase);
    }
  if (cosines <= 0)
    mus_misc_error(S_make_sum_of_cosines, "cosines <= 0?", keys[0]);
  gn = (mus_scm *)CALLOC(1, sizeof(mus_scm));
  gn->gen = mus_make_sum_of_cosines(cosines, freq, phase);
  gn->nvcts = 0;
  return(mus_scm_to_smob(gn));
}

static SCM g_sum_of_cosines(SCM obj, SCM fm)
{
  #define H_sum_of_cosines "(" S_sum_of_cosines " gen &optional (fm 0.0))\n\
gets the next sample of the band-limited pulse-train produced by gen"

  Float fm1 = 0.0;
  SCM_ASSERT((MUS_SCM_P(obj)) && (mus_sum_of_cosines_p(TO_CLM(obj))), obj, SCM_ARG1, S_sum_of_cosines);
  if (NUMBER_P(fm)) fm1 = TO_C_DOUBLE(fm); else if (BOUND_P(fm)) scm_wrong_type_arg(S_sum_of_cosines, 2, fm);
  return(TO_SCM_DOUBLE(mus_sum_of_cosines(TO_CLM(obj), fm1)));
}

static SCM g_cosines(SCM obj)
{
  #define H_mus_cosines "(" S_mus_cosines " gen) -> number of cosines produced by gen"
  SCM_ASSERT(MUS_SCM_P(obj), obj, SCM_ARG1, S_mus_cosines);
  return(TO_SCM_DOUBLE(mus_cosines(TO_CLM(obj))));
}

static void init_cosp(void)
{
  DEFINE_PROC(S_make_sum_of_cosines, g_make_sum_of_cosines, 0, 6, 0, H_make_sum_of_cosines); 
  DEFINE_PROC(S_sum_of_cosines,      g_sum_of_cosines, 1, 1, 0,      H_sum_of_cosines);
  DEFINE_PROC(S_sum_of_cosines_p,    g_sum_of_cosines_p, 1, 0, 0,    H_sum_of_cosines_p);
  DEFINE_PROC(S_mus_cosines,         g_cosines, 1, 0, 0,             H_mus_cosines);
}



/* ---------------- rand, rand_interp ---------------- */

#define S_make_rand "make-rand"
#define S_rand "rand"
#define S_rand_p "rand?"
#define S_make_rand_interp "make-rand-interp"
#define S_rand_interp "rand-interp"
#define S_rand_interp_p "rand-interp?"
#define S_mus_set_rand_seed "mus-set-rand-seed"
#define S_mus_random "mus-random"

static SCM g_make_noi(int rand_case, SCM arg1, SCM arg2, SCM arg3, SCM arg4)
{
  mus_scm *gn;
  SCM args[4], keys[2];
  int orig_arg[2] = {0, 0};
  int vals;
  Float freq = 440.0;
  Float base = 1.0;
  keys[0] = all_keys[C_frequency];
  keys[1] = all_keys[C_amplitude];
  args[0] = arg1; args[1] = arg2; args[2] = arg3; args[3] = arg4;
  vals = decode_keywords(S_make_rand, 2, keys, 4, args, orig_arg);
  if (vals > 0)
    {
      freq = fkeyarg(keys[0], S_make_rand, orig_arg[0] + 1, args[orig_arg[0]], freq);
      base = fkeyarg(keys[1], S_make_rand, orig_arg[1] + 1, args[orig_arg[1]], base);
    }
  gn = (mus_scm *)CALLOC(1, sizeof(mus_scm));
  if (rand_case)
    gn->gen = mus_make_rand(freq, base);
  else gn->gen = mus_make_rand_interp(freq, base);
  return(mus_scm_to_smob(gn));
}

static SCM g_make_rand_interp(SCM arg1, SCM arg2, SCM arg3, SCM arg4)
{
  #define H_make_rand_interp "(" S_make_rand_interp " &opt-key (frequency 440.0) (amplitude 1.0))\n\
returns a new " S_rand_interp " generator, producing linearly interpolated random numbers. \
frequency is the rate at which new end-points are chosen."

  return(g_make_noi(FALSE, arg1, arg2, arg3, arg4));
}

static SCM g_make_rand(SCM arg1, SCM arg2, SCM arg3, SCM arg4)
{
  #define H_make_rand "(" S_make_rand " &opt-key (frequency 440.0) (amplitude 1.0))\n\
returns a new " S_rand " generator, producing a sequence of random numbers (a step  function). \
frequency is the rate at which new numbers are chosen."

  return(g_make_noi(TRUE, arg1, arg2, arg3, arg4));
}

static SCM g_rand(SCM obj, SCM fm)
{
  #define H_rand "(" S_rand " gen &optional (fm 0.0)) -> gen's current random number. \
fm can modulate the rate at which the current number is changed."

  Float fm1 = 0.0;
  SCM_ASSERT((MUS_SCM_P(obj)) && (mus_rand_p(TO_CLM(obj))), obj, SCM_ARG1, S_rand);
  if (NUMBER_P(fm)) fm1 = TO_C_DOUBLE(fm); else if (BOUND_P(fm)) scm_wrong_type_arg(S_rand, 2, fm);
  return(TO_SCM_DOUBLE(mus_rand(TO_CLM(obj), fm1)));
}

static SCM g_rand_p(SCM obj) 
{
  #define H_rand_p "(" S_rand_p " gen) -> #t if gen is a " S_rand " generator, else #f"
  return(((MUS_SCM_P(obj)) && (mus_rand_p(TO_CLM(obj)))) ? SCM_BOOL_T : SCM_BOOL_F);
}

static SCM g_rand_interp(SCM obj, SCM fm)
{
  #define H_rand_interp "(" S_rand_interp " gen &optional (fm 0.0)) -> gen's current (interpolating) random number. \
fm can modulate the rate at which new segment end-points are chosen."

  Float fm1 = 0.0;
  SCM_ASSERT((MUS_SCM_P(obj)) && (mus_rand_interp_p(TO_CLM(obj))), obj, SCM_ARG1, S_rand_interp);
  if (NUMBER_P(fm)) fm1 = TO_C_DOUBLE(fm); else if (BOUND_P(fm)) scm_wrong_type_arg(S_rand_interp, 2, fm);
  return(TO_SCM_DOUBLE(mus_rand_interp(TO_CLM(obj), fm1)));
}

static SCM g_rand_interp_p(SCM obj) 
{
  #define H_rand_interp_p "(" S_rand_interp_p " gen) -> #t if gen is a " S_rand_interp " generator, else #f"
  return(((MUS_SCM_P(obj)) && (mus_rand_interp_p(TO_CLM(obj)))) ? SCM_BOOL_T : SCM_BOOL_F);
}

static SCM g_mus_random(SCM a) 
{
  #define H_mus_random "(" S_mus_random " val) -> a random number between -val and val. \
the built-in 'random' function returns values between 0 and its argument"

  SCM_ASSERT(NUMBER_P(a), a, SCM_ARG1, S_mus_random);
  return(TO_SCM_DOUBLE(mus_random(TO_C_DOUBLE(a))));
}

static SCM g_set_rand_seed(SCM a) 
{
  #define H_mus_set_rand_seed "(" S_mus_set_rand_seed " val) sets the random number seed, \
this can be used to re-run a particular random number sequence."

  SCM_ASSERT(INTEGER_P(a), a, SCM_ARG1, S_mus_set_rand_seed);
  mus_set_rand_seed(TO_C_INT(a)); 
  return(a);
}

static void init_noi(void)
{
  DEFINE_PROC(S_make_rand,        g_make_rand, 0, 4, 0,        H_make_rand);
  DEFINE_PROC(S_make_rand_interp, g_make_rand_interp, 0, 4, 0, H_make_rand_interp);
  DEFINE_PROC(S_rand,             g_rand, 1, 1, 0,             H_rand);
  DEFINE_PROC(S_rand_interp,      g_rand_interp, 1, 1, 0,      H_rand_interp);
  DEFINE_PROC(S_rand_p,           g_rand_p, 1, 0, 0,           H_rand_p);
  DEFINE_PROC(S_rand_interp_p,    g_rand_interp_p, 1, 0, 0,    H_rand_interp_p);
  DEFINE_PROC(S_mus_random,            g_mus_random, 1, 0, 0,                H_mus_random);
  DEFINE_PROC(S_mus_set_rand_seed,     g_set_rand_seed, 1, 0, 0,             H_mus_set_rand_seed);
}



/* ---------------- table lookup ---------------- */

static int DEFAULT_TABLE_SIZE = 512;

#define S_table_lookup_p     "table-lookup?"
#define S_make_table_lookup  "make-table-lookup"
#define S_table_lookup       "table-lookup"
#define S_partials2wave      "partials->wave"
#define S_phasepartials2wave "phase-partials->wave"

static SCM g_table_lookup_p(SCM obj) 
{
  #define H_table_lookup_p "(" S_table_lookup_p " gen) -> #t if gen is a " S_table_lookup " generator, else #f"
  return(((MUS_SCM_P(obj)) && (mus_table_lookup_p(TO_CLM(obj)))) ? SCM_BOOL_T : SCM_BOOL_F);
}

static SCM g_partials2wave(SCM partials, SCM utable, SCM normalize)
{
  #define H_partials2wave "(" S_partials2wave " partials &optional wave normalize)\n\
takes a list of partials (harmonic number and associated amplitude) and produces \
a waveform for use in " S_table_lookup ".  If wave (a vct object) is not given, \
a new one is created.  If normalize is #t, the resulting waveform goes between -1 and 1.\n\
  (set! gen (make-table-lookup 440.0 :wave (partials->wave '(1 1.0 2 .5))))"

  vct *f;
  SCM table, lst;
  Float *partial_data, *wave;
  int len, i;
  SCM_ASSERT(LIST_P_WITH_LENGTH(partials, len), partials, SCM_ARG1, S_partials2wave);
  SCM_ASSERT(VCT_P(utable) || FALSE_P(utable) || (!(BOUND_P(utable))), utable, SCM_ARG2, S_partials2wave);
  SCM_ASSERT(BOOLEAN_IF_BOUND_P(normalize), normalize, SCM_ARG3, S_partials2wave);
  if (len == 0)
    mus_misc_error(S_partials2wave, "partials list empty?", partials);
  if ((NOT_BOUND_P(utable)) || (!(VCT_P(utable))))
    {
      wave = (Float *)CALLOC(DEFAULT_TABLE_SIZE, sizeof(Float));
      table = make_vct(DEFAULT_TABLE_SIZE, wave);
    }
  else table = utable;
  f = TO_VCT(table);
  partial_data = (Float *)CALLOC(len, sizeof(Float));
  for (i = 0, lst = partials; i < len; i++, lst = SCM_CDR(lst)) 
    partial_data[i] = TO_C_DOUBLE(SCM_CAR(lst));
  mus_partials2wave(partial_data, len / 2, f->data, f->length, (TRUE_P(normalize)));
  FREE(partial_data);
  return(table);
}

static SCM g_phasepartials2wave(SCM partials, SCM utable, SCM normalize)
{
  vct *f;
  SCM table, lst;
  Float *partial_data, *wave;
  int len, i;

  #define H_phasepartials2wave "(" S_phasepartials2wave " partials &optional wave normalize)\n\
takes a list of partials (harmonic number, amplitude, initial phase) and produces \
a waveform for use in " S_table_lookup ".  If wave (a vct object) is not given, \
a new one is created.  If normalize is #t, the resulting waveform goes between -1 and 1.\n\
  (set! gen (make-table-lookup 440.0 :wave (phase-partials->wave (list 1 .75 0.0 2 .25 (* pi .5)))))"

  SCM_ASSERT(LIST_P_WITH_LENGTH(partials, len), partials, SCM_ARG1, S_phasepartials2wave);
  SCM_ASSERT(VCT_P(utable) || FALSE_P(utable) || (!(BOUND_P(utable))), utable, SCM_ARG2, S_phasepartials2wave);
  SCM_ASSERT(BOOLEAN_IF_BOUND_P(normalize), normalize, SCM_ARG3, S_phasepartials2wave);
  if (len == 0)
    mus_misc_error(S_partials2wave, "partials list empty?", partials);
  if ((NOT_BOUND_P(utable)) || (!(VCT_P(utable))))
    {
      wave = (Float *)CALLOC(DEFAULT_TABLE_SIZE, sizeof(Float));
      table = make_vct(DEFAULT_TABLE_SIZE, wave);
    }
  else table = utable;
  f = TO_VCT(table);
  partial_data = (Float *)CALLOC(len, sizeof(Float));
  for (i = 0, lst = partials; i < len; i++, lst = SCM_CDR(lst)) 
    partial_data[i] = TO_C_DOUBLE(SCM_CAR(lst));
  mus_phasepartials2wave(partial_data, len / 3, f->data, f->length, (TRUE_P(normalize)));
  FREE(partial_data);
  return(table);
}

static SCM g_make_table_lookup (SCM arg1, SCM arg2, SCM arg3, SCM arg4, SCM arg5, SCM arg6)
{
  #define H_make_table_lookup "(" S_make_table_lookup " &opt-key (frequency 440.0) (initial-phase 0.0) wave)\n\
returns a new " S_table_lookup " generator.  This is known as an oscillator in other synthesis systems. \
The default table size is 512; to use some other size, pass your own vct object as the 'wave'.\n\
   (set! gen (make-table-lookup 440.0 :wave (partials->wave '(1 1.0)))\n\
is the same in effect as " S_make_oscil "."

  mus_scm *gn;
  int vals, table_size = DEFAULT_TABLE_SIZE;
  SCM args[6], keys[3];
  int orig_arg[3] = {0, 0, 0};
  Float freq = 440.0, phase = 0.0;
  Float *table = NULL;
  vct *v;
  keys[0] = all_keys[C_frequency];
  keys[1] = all_keys[C_initial_phase];
  keys[2] = all_keys[C_wave];
  args[0] = arg1; args[1] = arg2; args[2] = arg3; args[3] = arg4; args[4] = arg5; args[5] = arg6;
  vals = decode_keywords(S_make_table_lookup, 3, keys, 6, args, orig_arg);
  if (vals > 0)
    {
      freq = fkeyarg(keys[0], S_make_table_lookup, orig_arg[0] + 1, args[orig_arg[0]], freq);
      phase = fkeyarg(keys[1], S_make_table_lookup, orig_arg[1] + 1, args[orig_arg[1]], phase);
      if (!(KEYWORD_P(keys[2])))
	{
	  if (VCT_P(keys[2]))
	    {
	      v = TO_VCT(keys[2]);
	      table = copy_vct_data(v);
	      table_size = v->length;
	    }
	  else scm_wrong_type_arg(S_make_table_lookup, orig_arg[2] + 1, args[orig_arg[2]]);
	}
    }
  if (table == NULL) table = (Float *)CALLOC(table_size, sizeof(Float));
  gn = (mus_scm *)CALLOC(1, sizeof(mus_scm));
  gn->vcts = (SCM *)CALLOC(1, sizeof(SCM));
  gn->nvcts = 1;
  gn->vcts[0] = SCM_EOL;
  gn->gen = mus_make_table_lookup(freq, phase, table, table_size);
  return(mus_scm_to_smob_with_vct(gn, make_vct(table_size, table)));
}

static SCM g_table_lookup (SCM obj, SCM fm) 
{
  #define H_table_lookup "(" S_table_lookup " gen &optional (fm 0.0)) performs interpolated table-lookup \
with 'wrap-around' when gen's phase marches off the end of its table."

  Float fm1 = 0.0;
  SCM_ASSERT((MUS_SCM_P(obj)) && (mus_table_lookup_p(TO_CLM(obj))), obj, SCM_ARG1, S_table_lookup);
  if (NUMBER_P(fm)) fm1 = TO_C_DOUBLE(fm);  else if (BOUND_P(fm)) scm_wrong_type_arg(S_table_lookup, 2, fm);
  return(TO_SCM_DOUBLE(mus_table_lookup(TO_CLM(obj), fm1)));
}

static void init_tbl(void)
{
  DEFINE_PROC(S_table_lookup_p,     g_table_lookup_p, 1, 0, 0,     H_table_lookup_p);
  DEFINE_PROC(S_make_table_lookup,  g_make_table_lookup, 0, 6, 0,  H_make_table_lookup);
  DEFINE_PROC(S_table_lookup,       g_table_lookup, 1, 1, 0,       H_table_lookup);
  DEFINE_PROC(S_partials2wave,      g_partials2wave, 1, 2, 0,      H_partials2wave);
  DEFINE_PROC(S_phasepartials2wave, g_phasepartials2wave, 1, 2, 0, H_phasepartials2wave);
}


/* ---------------- sawtooth et al ---------------- */

#define S_make_sawtooth_wave "make-sawtooth-wave"
#define S_sawtooth_wave      "sawtooth-wave"
#define S_sawtooth_wave_p    "sawtooth-wave?"
#define S_make_square_wave   "make-square-wave"
#define S_square_wave        "square-wave"
#define S_square_wave_p      "square-wave?"
#define S_make_triangle_wave "make-triangle-wave"
#define S_triangle_wave      "triangle-wave"
#define S_triangle_wave_p    "triangle-wave?"
#define S_make_pulse_train   "make-pulse-train"
#define S_pulse_train        "pulse-train"
#define S_pulse_train_p      "pulse-train?"

enum {G_SAWTOOTH_WAVE, G_SQUARE_WAVE, G_TRIANGLE_WAVE, G_PULSE_TRAIN};

static SCM g_make_sw(int type, Float def_phase, SCM arg1, SCM arg2, SCM arg3, SCM arg4, SCM arg5, SCM arg6)
{
  mus_scm *gn;
  char *caller = NULL;
  SCM args[6], keys[3];
  int orig_arg[3] = {0, 0, 0};
  int vals;
  Float freq = 440.0;
  Float base = 1.0;
  Float phase;
  phase = def_phase;
  switch (type)
    {
      case G_SAWTOOTH_WAVE: caller = S_make_sawtooth_wave; break;
      case G_SQUARE_WAVE: caller = S_make_square_wave; break;
      case G_TRIANGLE_WAVE: caller = S_make_triangle_wave; break;
      case G_PULSE_TRAIN: caller = S_make_pulse_train; break;
    }
  keys[0] = all_keys[C_frequency];
  keys[1] = all_keys[C_amplitude];
  keys[2] = all_keys[C_initial_phase];
  args[0] = arg1; args[1] = arg2; args[2] = arg3; args[3] = arg4; args[4] = arg5; args[5] = arg6; 
  vals = decode_keywords(caller, 3, keys, 6, args, orig_arg);
  if (vals > 0)
    {
      freq = fkeyarg(keys[0], caller, orig_arg[0] + 1, args[orig_arg[0]], freq);
      base = fkeyarg(keys[1], caller, orig_arg[1] + 1, args[orig_arg[1]], base);
      phase = fkeyarg(keys[2], caller, orig_arg[2] + 1, args[orig_arg[2]], phase);
    }
  gn = (mus_scm *)CALLOC(1, sizeof(mus_scm));
  switch (type)
    {
    case G_SAWTOOTH_WAVE: gn->gen = mus_make_sawtooth_wave(freq, base, phase); break;
    case G_SQUARE_WAVE: gn->gen = mus_make_square_wave(freq, base, phase); break;
    case G_TRIANGLE_WAVE: gn->gen = mus_make_triangle_wave(freq, base, phase); break;
    case G_PULSE_TRAIN: gn->gen = mus_make_pulse_train(freq, base, phase); break;
    }
  return(mus_scm_to_smob(gn));
}

static SCM g_make_sawtooth_wave(SCM arg1, SCM arg2, SCM arg3, SCM arg4, SCM arg5, SCM arg6) 
{
  #define H_make_sawtooth_wave "(" S_make_sawtooth_wave " &opt-key (frequency 440.0) (amplitude 1.0) (initial-phase 0.0))\n\
returns a new " S_sawtooth_wave " generator."

  return(g_make_sw(G_SAWTOOTH_WAVE, M_PI, arg1, arg2, arg3, arg4, arg5, arg6));
}

static SCM g_make_square_wave(SCM arg1, SCM arg2, SCM arg3, SCM arg4, SCM arg5, SCM arg6) 
{
  #define H_make_square_wave "(" S_make_square_wave " &opt-key (frequency 440.0) (amplitude 1.0) (initial-phase 0.0))\n\
returns a new " S_square_wave " generator."

  return(g_make_sw(G_SQUARE_WAVE, 0.0, arg1, arg2, arg3, arg4, arg5, arg6));
}

static SCM g_make_triangle_wave(SCM arg1, SCM arg2, SCM arg3, SCM arg4, SCM arg5, SCM arg6) 
{
  #define H_make_triangle_wave "(" S_make_triangle_wave " &opt-key (frequency 440.0) (amplitude 1.0) (initial-phase 0.0))\n\
returns a new " S_triangle_wave " generator."

  return(g_make_sw(G_TRIANGLE_WAVE, 0.0, arg1, arg2, arg3, arg4, arg5, arg6));
}

static SCM g_make_pulse_train(SCM arg1, SCM arg2, SCM arg3, SCM arg4, SCM arg5, SCM arg6) 
{
  #define H_make_pulse_train "(" S_make_pulse_train " &opt-key (frequency 440.0) (amplitude 1.0) (initial-phase 0.0))\n\
returns a new " S_pulse_train " generator.  This produces a sequence of impulses."

  return(g_make_sw(G_PULSE_TRAIN, TWO_PI, arg1, arg2, arg3, arg4, arg5, arg6));
}

static SCM g_sawtooth_wave(SCM obj, SCM fm) 
{
  #define H_sawtooth_wave "(" S_sawtooth_wave " gen &optional (fm 0.0)) -> next sawtooth sample from gen"
  Float fm1 = 0.0;
  SCM_ASSERT((MUS_SCM_P(obj)) && (mus_sawtooth_wave_p(TO_CLM(obj))), obj, SCM_ARG1, S_sawtooth_wave);
  if (NUMBER_P(fm)) fm1 = TO_C_DOUBLE(fm); else if (BOUND_P(fm)) scm_wrong_type_arg(S_sawtooth_wave, 2, fm);
  return(TO_SCM_DOUBLE(mus_sawtooth_wave(TO_CLM(obj), fm1)));
}

static SCM g_square_wave(SCM obj, SCM fm) 
{
  #define H_square_wave "(" S_square_wave " gen &optional (fm 0.0)) -> next square wave sample from gen"
  Float fm1 = 0.0;
  SCM_ASSERT((MUS_SCM_P(obj)) && (mus_square_wave_p(TO_CLM(obj))), obj, SCM_ARG1, S_square_wave);
  if (NUMBER_P(fm)) fm1 = TO_C_DOUBLE(fm); else if (BOUND_P(fm)) scm_wrong_type_arg(S_square_wave, 2, fm);
  return(TO_SCM_DOUBLE(mus_square_wave(TO_CLM(obj), fm1)));
}

static SCM g_triangle_wave(SCM obj, SCM fm) 
{
  #define H_triangle_wave "(" S_triangle_wave " gen &optional (fm 0.0)) -> next triangle wave sample from gen"
  Float fm1 = 0.0;
  SCM_ASSERT((MUS_SCM_P(obj)) && (mus_triangle_wave_p(TO_CLM(obj))), obj, SCM_ARG1, S_triangle_wave);
  if (NUMBER_P(fm)) fm1 = TO_C_DOUBLE(fm); else if (BOUND_P(fm)) scm_wrong_type_arg(S_triangle_wave, 2, fm);
  return(TO_SCM_DOUBLE(mus_triangle_wave(TO_CLM(obj), fm1)));
}

static SCM g_pulse_train(SCM obj, SCM fm) 
{
  #define H_pulse_train "(" S_pulse_train " gen &optional (fm 0.0)) -> next (im)pulse train sample from gen"
  Float fm1 = 0.0;
  SCM_ASSERT((MUS_SCM_P(obj)) && (mus_pulse_train_p(TO_CLM(obj))), obj, SCM_ARG1, S_pulse_train);
  if (NUMBER_P(fm)) fm1 = TO_C_DOUBLE(fm); else if (BOUND_P(fm)) scm_wrong_type_arg(S_pulse_train, 2, fm);
  return(TO_SCM_DOUBLE(mus_pulse_train(TO_CLM(obj), fm1)));
}

static SCM g_sawtooth_wave_p(SCM obj) 
{
  #define H_sawtooth_wave_p "(" S_sawtooth_wave_p " gen) -> #t if gen is a " S_sawtooth_wave " generator, else #f"
  return(((MUS_SCM_P(obj)) && (mus_sawtooth_wave_p(TO_CLM(obj)))) ? SCM_BOOL_T : SCM_BOOL_F);
}

static SCM g_square_wave_p(SCM obj) 
{
  #define H_square_wave_p "(" S_square_wave_p " gen) -> #t if gen is a " S_square_wave " generator, else #f"
  return(((MUS_SCM_P(obj)) && (mus_square_wave_p(TO_CLM(obj)))) ? SCM_BOOL_T : SCM_BOOL_F);
}

static SCM g_triangle_wave_p(SCM obj) 
{
  #define H_triangle_wave_p "(" S_triangle_wave_p " gen) -> #t if gen is a " S_triangle_wave " generator, else #f"
  return(((MUS_SCM_P(obj)) && (mus_triangle_wave_p(TO_CLM(obj)))) ? SCM_BOOL_T : SCM_BOOL_F);
}

static SCM g_pulse_train_p(SCM obj) 
{
  #define H_pulse_train_p "(" S_pulse_train_p " gen) -> #t if gen is a " S_pulse_train " generator, else #f"
  return(((MUS_SCM_P(obj)) && (mus_pulse_train_p(TO_CLM(obj)))) ? SCM_BOOL_T : SCM_BOOL_F);
}

static void init_sw(void)
{
  DEFINE_PROC(S_make_sawtooth_wave, g_make_sawtooth_wave, 0, 6, 0, H_make_sawtooth_wave);
  DEFINE_PROC(S_sawtooth_wave,      g_sawtooth_wave, 1, 1, 0,      H_sawtooth_wave);
  DEFINE_PROC(S_sawtooth_wave_p,    g_sawtooth_wave_p, 1, 0, 0,    H_sawtooth_wave_p);
  DEFINE_PROC(S_make_triangle_wave, g_make_triangle_wave, 0, 6, 0, H_make_triangle_wave);
  DEFINE_PROC(S_triangle_wave,      g_triangle_wave, 1, 1, 0,      H_triangle_wave);
  DEFINE_PROC(S_triangle_wave_p,    g_triangle_wave_p, 1, 0, 0,    H_triangle_wave_p);
  DEFINE_PROC(S_make_square_wave,   g_make_square_wave, 0, 6, 0,   H_make_square_wave);
  DEFINE_PROC(S_square_wave,        g_square_wave, 1, 1, 0,        H_square_wave);
  DEFINE_PROC(S_square_wave_p,      g_square_wave_p, 1, 0, 0,      H_square_wave_p);
  DEFINE_PROC(S_make_pulse_train,   g_make_pulse_train, 0, 6, 0,   H_make_pulse_train);
  DEFINE_PROC(S_pulse_train,        g_pulse_train, 1, 1, 0,        H_pulse_train);
  DEFINE_PROC(S_pulse_train_p,      g_pulse_train_p, 1, 0, 0,      H_pulse_train_p);
}


/* ---------------- asymmetric-fm ---------------- */

#define S_make_asymmetric_fm "make-asymmetric-fm"
#define S_asymmetric_fm "asymmetric-fm"
#define S_asymmetric_fm_p "asymmetric-fm?"

static SCM g_make_asymmetric_fm(SCM arg1, SCM arg2, SCM arg3, SCM arg4, SCM arg5, SCM arg6, SCM arg7, SCM arg8)
{
  #define H_make_asymmetric_fm "(" S_make_asymmetric_fm " &opt-key (frequency 440.0) (initial-phase 0.0) (r 1.0) (ratio 1.0))\n\
returns a new " S_asymmetric_fm " generator."

  mus_scm *gn;
  SCM args[8], keys[4];
  int orig_arg[4] = {0, 0, 0};
  int vals;
  Float freq = 440.0;
  Float phase = 0.0;
  Float r = 1.0;
  Float ratio = 1.0;
  keys[0] = all_keys[C_frequency];
  keys[1] = all_keys[C_initial_phase];
  keys[2] = all_keys[C_r];
  keys[3] = all_keys[C_ratio];
  args[0] = arg1; args[1] = arg2; args[2] = arg3; args[3] = arg4; args[4] = arg5; args[5] = arg6; args[6] = arg7; args[7] = arg8; 
  vals = decode_keywords(S_make_asymmetric_fm, 4, keys, 8, args, orig_arg);
  if (vals > 0)
    {
      freq = fkeyarg(keys[0], S_make_asymmetric_fm, orig_arg[0] + 1, args[orig_arg[0]], freq);
      phase = fkeyarg(keys[1], S_make_asymmetric_fm, orig_arg[1] + 1, args[orig_arg[1]], phase);
      r = fkeyarg(keys[2], S_make_asymmetric_fm, orig_arg[2] + 1, args[orig_arg[2]], r);
      ratio = fkeyarg(keys[3], S_make_asymmetric_fm, orig_arg[3] + 1, args[orig_arg[3]], ratio);
    }
  gn = (mus_scm *)CALLOC(1, sizeof(mus_scm));
  gn->gen = mus_make_asymmetric_fm(freq, phase, r, ratio);
  return(mus_scm_to_smob(gn));
}

static SCM g_asymmetric_fm(SCM obj, SCM index, SCM fm)
{
  #define H_asymmetric_fm "(" S_asymmetric_fm " gen &optional (index 0.0) (fm 0.0)) -> next sample from asymmetric fm gen"
  Float fm1 = 0.0, index1 = 0.0;
  SCM_ASSERT((MUS_SCM_P(obj)) && (mus_asymmetric_fm_p(TO_CLM(obj))), obj, SCM_ARG1, S_asymmetric_fm);
  if (NUMBER_P(fm)) fm1 = TO_C_DOUBLE(fm); else if (BOUND_P(fm)) scm_wrong_type_arg(S_asymmetric_fm, 3, fm);
  if (NUMBER_P(index)) index1 = TO_C_DOUBLE(index); else if (BOUND_P(index)) scm_wrong_type_arg(S_asymmetric_fm, 2, index);
  return(TO_SCM_DOUBLE(mus_asymmetric_fm(TO_CLM(obj), index1, fm1)));
}

static SCM g_asymmetric_fm_p(SCM obj) 
{
  #define H_asymmetric_fm_p "(" S_asymmetric_fm_p " gen) -> #t if gen is a " S_asymmetric_fm " generator, else #f"
  return(((MUS_SCM_P(obj)) && (mus_asymmetric_fm_p(TO_CLM(obj)))) ? SCM_BOOL_T : SCM_BOOL_F);
}

static void init_asyfm(void)
{
  DEFINE_PROC(S_make_asymmetric_fm, g_make_asymmetric_fm, 0, 8, 0, H_make_asymmetric_fm);
  DEFINE_PROC(S_asymmetric_fm,      g_asymmetric_fm, 1, 2, 0,      H_asymmetric_fm);
  DEFINE_PROC(S_asymmetric_fm_p,    g_asymmetric_fm_p, 1, 0, 0,    H_asymmetric_fm_p);
}



/* ---------------- simple filters ---------------- */

#define S_make_one_zero "make-one-zero"
#define S_one_zero      "one-zero"
#define S_one_zero_p    "one-zero?"
#define S_make_one_pole "make-one-pole"
#define S_one_pole      "one-pole"
#define S_one_pole_p    "one-pole?"
#define S_make_two_zero "make-two-zero"
#define S_two_zero      "two-zero"
#define S_two_zero_p    "two-zero?"
#define S_make_two_pole "make-two-pole"
#define S_two_pole      "two-pole"
#define S_two_pole_p    "two-pole?"
#define S_make_zpolar   "make-zpolar"
#define S_make_ppolar   "make-ppolar"
#define S_mus_a0        "mus-a0"
#define S_mus_a1        "mus-a1"
#define S_mus_a2        "mus-a2"
#define S_mus_b1        "mus-b1"
#define S_mus_b2        "mus-b2"
#define S_mus_set_a0    "mus-set-a0"
#define S_mus_set_a1    "mus-set-a1"
#define S_mus_set_a2    "mus-set-a2"
#define S_mus_set_b1    "mus-set-b1"
#define S_mus_set_b2    "mus-set-b2"

enum {G_ONE_POLE, G_ONE_ZERO, G_TWO_POLE, G_TWO_ZERO, G_ZPOLAR, G_PPOLAR};
static char *smpflts[6] = {S_make_one_pole, S_make_one_zero, S_make_two_pole, S_make_two_zero, S_make_zpolar, S_make_ppolar};

static SCM g_make_smpflt_1(int choice, SCM arg1, SCM arg2, SCM arg3, SCM arg4)
{
  mus_scm *gn;
  mus_any *gen = NULL;
  SCM args[4], keys[2];
  int orig_arg[2] = {0, 0};
  int vals;
  Float a0 = 0.0;
  Float a1 = 0.0;
  switch (choice)
    {
    case G_ONE_ZERO: keys[0] = all_keys[C_a0]; keys[1] = all_keys[C_a1]; break;
    case G_ONE_POLE: keys[0] = all_keys[C_a0]; keys[1] = all_keys[C_b1]; break;
    default: keys[0] = all_keys[C_r]; keys[1] = all_keys[C_frequency]; break;
    }
  args[0] = arg1; args[1] = arg2; args[2] = arg3; args[3] = arg4;
  vals = decode_keywords(smpflts[choice], 2, keys, 4, args, orig_arg);
  if (vals > 0)
    {
      a0 = fkeyarg(keys[0], smpflts[choice], orig_arg[0] + 1, args[orig_arg[0]], a0);
      a1 = fkeyarg(keys[1], smpflts[choice], orig_arg[1] + 1, args[orig_arg[1]], a1);
    }
  switch (choice)
    {
    case G_ONE_ZERO: gen = mus_make_one_zero(a0, a1); break;
    case G_ONE_POLE: gen = mus_make_one_pole(a0, a1); break;
    case G_ZPOLAR: gen = mus_make_zpolar(a0, a1); break;
    case G_PPOLAR: gen = mus_make_ppolar(a0, a1); break;
    }
  gn = (mus_scm *)CALLOC(1, sizeof(mus_scm));
  gn->gen = gen; /* delayed to here in case mus_error in mus_make_<whatever> above */
  return(mus_scm_to_smob(gn));
}

static SCM g_make_one_zero(SCM arg1, SCM arg2, SCM arg3, SCM arg4)
{
  #define H_make_one_zero "(" S_make_one_zero " a0 a1) -> new " S_one_zero " filter returning a0*x(n) + a1*x(n-1)"
  return(g_make_smpflt_1(G_ONE_ZERO, arg1, arg2, arg3, arg4));
}

static SCM g_make_one_pole(SCM arg1, SCM arg2, SCM arg3, SCM arg4)
{
  #define H_make_one_pole "(" S_make_one_pole " a0 b1) -> new " S_one_pole " filter returning a0*x(n) - b1*y(n-1)"
  return(g_make_smpflt_1(G_ONE_POLE, arg1, arg2, arg3, arg4));
}

static SCM g_make_zpolar(SCM arg1, SCM arg2, SCM arg3, SCM arg4)
{
  #define H_make_zpolar "(" S_make_zpolar " radius frequency) -> new " S_two_zero " filter \
where the coefficients (a0..a2) are set from the desired zero's radius and center frequency. \
Use this in conjunction with the " S_two_zero " generator" 

  return(g_make_smpflt_1(G_ZPOLAR, arg1, arg2, arg3, arg4));
}

static SCM g_make_ppolar(SCM arg1, SCM arg2, SCM arg3, SCM arg4)
{
  #define H_make_ppolar "(" S_make_ppolar " radius frequency) -> new " S_two_pole " filter \
where the coefficients are set from the desired pole's radius and center frequency. \
Use this in conjunction with the " S_two_pole " generator" 

  return(g_make_smpflt_1(G_PPOLAR, arg1, arg2, arg3, arg4));
}

static SCM g_make_smpflt_2(int choice, SCM arg1, SCM arg2, SCM arg3, SCM arg4, SCM arg5, SCM arg6)
{
  mus_scm *gn;
  mus_any *gen = NULL;
  SCM args[6], keys[3];
  int orig_arg[3] = {0, 0, 0};
  int vals;
  Float a0 = 0.0;
  Float a1 = 0.0;
  Float a2 = 0.0;
  if (choice == G_TWO_ZERO)
    {
      keys[0] = all_keys[C_a0];
      keys[1] = all_keys[C_a1];
      keys[2] = all_keys[C_a2];
    }
  else
    {
      keys[0] = all_keys[C_a0];
      keys[1] = all_keys[C_b1];
      keys[2] = all_keys[C_b2];
    }
  args[0] = arg1; args[1] = arg2; args[2] = arg3; args[3] = arg4; args[4] = arg5; args[5] = arg6;
  vals = decode_keywords(smpflts[choice], 3, keys, 6, args, orig_arg);
  if (vals > 0)
    {
      a0 = fkeyarg(keys[0], smpflts[choice], orig_arg[0] + 1, args[orig_arg[0]], a0);
      a1 = fkeyarg(keys[1], smpflts[choice], orig_arg[1] + 1, args[orig_arg[1]], a1);
      a2 = fkeyarg(keys[2], smpflts[choice], orig_arg[2] + 1, args[orig_arg[2]], a2);
    }
  if (choice == G_TWO_ZERO)
    gen = mus_make_two_zero(a0, a1, a2);
  else gen = mus_make_two_pole(a0, a1, a2);
  gn = (mus_scm *)CALLOC(1, sizeof(mus_scm));
  gn->gen = gen;  /* delayed in case of mus_error in make_two_pole */
  return(mus_scm_to_smob(gn));
}

static SCM g_make_two_zero(SCM arg1, SCM arg2, SCM arg3, SCM arg4, SCM arg5, SCM arg6) 
{
  #define H_make_two_zero "(" S_make_two_zero " a0 a1 a2) -> new " S_two_zero " filter \
returning a0*x(n) + a1*x(n-1) + a2*x(n-2)"

  return(g_make_smpflt_2(G_TWO_ZERO, arg1, arg2, arg3, arg4, arg5, arg6));
}

static SCM g_make_two_pole(SCM arg1, SCM arg2, SCM arg3, SCM arg4, SCM arg5, SCM arg6) 
{
  #define H_make_two_pole "(" S_make_two_pole " a0 b1 b2) -> new " S_two_pole " filter \
returning a0*x(n) - b1*y(n-1) - b2*y(n-2)"

  return(g_make_smpflt_2(G_TWO_POLE, arg1, arg2, arg3, arg4, arg5, arg6));
}

static SCM g_one_zero(SCM obj, SCM fm)
{
  #define H_one_zero "(" S_one_zero " gen &optional (input 0.0)) -> one zero filter of input"
  Float fm1 = 0.0;
  SCM_ASSERT((MUS_SCM_P(obj)) && (mus_one_zero_p(TO_CLM(obj))), obj, SCM_ARG1, S_one_zero);
  if (NUMBER_P(fm)) fm1 = TO_C_DOUBLE(fm); else if (BOUND_P(fm)) scm_wrong_type_arg(S_one_zero, 2, fm);
  return(TO_SCM_DOUBLE(mus_one_zero(TO_CLM(obj), fm1)));
}

static SCM g_one_pole(SCM obj, SCM fm)
{
  #define H_one_pole "(" S_one_pole " gen &optional (input 0.0)) -> one pole filter of input"
  Float fm1 = 0.0;
  SCM_ASSERT((MUS_SCM_P(obj)) && (mus_one_pole_p(TO_CLM(obj))), obj, SCM_ARG1, S_one_pole);
  if (NUMBER_P(fm)) fm1 = TO_C_DOUBLE(fm); else if (BOUND_P(fm)) scm_wrong_type_arg(S_one_pole, 2, fm);
  return(TO_SCM_DOUBLE(mus_one_pole(TO_CLM(obj), fm1)));
}

static SCM g_two_zero(SCM obj, SCM fm)
{
  #define H_two_zero "(" S_two_zero " gen &optional (input 0.0)) -> two zero filter of input"
  Float fm1 = 0.0;
  SCM_ASSERT((MUS_SCM_P(obj)) && (mus_two_zero_p(TO_CLM(obj))), obj, SCM_ARG1, S_two_zero);
  if (NUMBER_P(fm)) fm1 = TO_C_DOUBLE(fm); else if (BOUND_P(fm)) scm_wrong_type_arg(S_two_zero, 2, fm);
  return(TO_SCM_DOUBLE(mus_two_zero(TO_CLM(obj), fm1)));
}

static SCM g_two_pole(SCM obj, SCM fm)
{
  #define H_two_pole "(" S_two_pole " gen &optional (input 0.0)) -> two pole filter of input"
  Float fm1 = 0.0;
  SCM_ASSERT((MUS_SCM_P(obj)) && (mus_two_pole_p(TO_CLM(obj))), obj, SCM_ARG1, S_two_pole);
  if (NUMBER_P(fm)) fm1 = TO_C_DOUBLE(fm); else if (BOUND_P(fm)) scm_wrong_type_arg(S_two_pole, 2, fm);
  return(TO_SCM_DOUBLE(mus_two_pole(TO_CLM(obj), fm1)));
}

static SCM g_one_zero_p(SCM obj) 
{
  #define H_one_zero_p "(" S_one_zero_p " gen) -> #t if gen is a " S_one_zero " generator, else #f"
  return(((MUS_SCM_P(obj)) && (mus_one_zero_p(TO_CLM(obj)))) ? SCM_BOOL_T : SCM_BOOL_F);
}

static SCM g_one_pole_p(SCM obj) 
{
  #define H_one_pole_p "(" S_one_pole_p " gen) -> #t if gen is a " S_one_pole " generator, else #f"
  return(((MUS_SCM_P(obj)) && (mus_one_pole_p(TO_CLM(obj)))) ? SCM_BOOL_T : SCM_BOOL_F);
}

static SCM g_two_zero_p(SCM obj) 
{
  #define H_two_zero_p "(" S_two_zero_p " gen) -> #t if gen is a " S_two_zero " generator, else #f"
  return(((MUS_SCM_P(obj)) && (mus_two_zero_p(TO_CLM(obj)))) ? SCM_BOOL_T : SCM_BOOL_F);
}

static SCM g_two_pole_p(SCM obj) 
{
  #define H_two_pole_p "(" S_two_pole_p " gen) -> #t if gen is a " S_two_pole " generator, else #f"
  return(((MUS_SCM_P(obj)) && (mus_two_pole_p(TO_CLM(obj)))) ? SCM_BOOL_T : SCM_BOOL_F);
}

static SCM g_a0(SCM obj) 
{
  #define H_mus_a0 "(" S_mus_a0 " gen) -> gen's " S_mus_a0 " coefficient (scaler on x(n)), if any"
  SCM_ASSERT(MUS_SCM_P(obj), obj, SCM_ARG1, S_mus_a0);
  return(TO_SCM_DOUBLE(mus_a0(TO_CLM(obj))));
}

static SCM g_a1(SCM obj)
{
  #define H_mus_a1 "(" S_mus_a1 " gen) -> gen's " S_mus_a1 " coefficient (scaler on x(n-1)), if any"
  SCM_ASSERT(MUS_SCM_P(obj), obj, SCM_ARG1, S_mus_a1);
  return(TO_SCM_DOUBLE(mus_a1(TO_CLM(obj))));
}

static SCM g_a2(SCM obj)
{
  #define H_mus_a2 "(" S_mus_a2 " gen) -> gen's " S_mus_a2 " coefficient (scaler on x(n-2)), if any"
  SCM_ASSERT(MUS_SCM_P(obj), obj, SCM_ARG1, S_mus_a2);
  return(TO_SCM_DOUBLE(mus_a2(TO_CLM(obj))));
}

static SCM g_b1(SCM obj)
{
  #define H_mus_b1 "(" S_mus_b1 " gen) -> gen's " S_mus_b1 " coefficient (scaler on y(n-1)), if any"
  SCM_ASSERT(MUS_SCM_P(obj), obj, SCM_ARG1, S_mus_b1);
  return(TO_SCM_DOUBLE(mus_b1(TO_CLM(obj))));
}

static SCM g_b2(SCM obj)
{
  #define H_mus_b2 "(" S_mus_b2 " gen) -> gen's " S_mus_b2 " coefficient (scaler on y(n-2)), if any"
  SCM_ASSERT(MUS_SCM_P(obj), obj, SCM_ARG1, S_mus_b2);
  return(TO_SCM_DOUBLE(mus_b2(TO_CLM(obj))));
}

static SCM g_set_a0(SCM obj, SCM val)
{
  SCM_ASSERT(MUS_SCM_P(obj), obj, SCM_ARG1, S_mus_a0);
  SCM_ASSERT(NUMBER_P(val), val, SCM_ARG2, S_mus_a0);
  return(TO_SCM_DOUBLE(mus_set_a0(TO_CLM(obj), TO_C_DOUBLE(val))));
}

static SCM g_set_a1(SCM obj, SCM val)
{
  SCM_ASSERT(MUS_SCM_P(obj), obj, SCM_ARG1, S_mus_a1);
  SCM_ASSERT(NUMBER_P(val), val, SCM_ARG2, S_mus_a1);
  return(TO_SCM_DOUBLE(mus_set_a1(TO_CLM(obj), TO_C_DOUBLE(val))));
}

static SCM g_set_a2(SCM obj, SCM val)
{
  SCM_ASSERT(MUS_SCM_P(obj), obj, SCM_ARG1, S_mus_a2);
  SCM_ASSERT(NUMBER_P(val), val, SCM_ARG2, S_mus_a2);
  return(TO_SCM_DOUBLE(mus_set_a2(TO_CLM(obj), TO_C_DOUBLE(val))));
}

static SCM g_set_b1(SCM obj, SCM val)
{
  SCM_ASSERT(MUS_SCM_P(obj), obj, SCM_ARG1, S_mus_b1);
  SCM_ASSERT(NUMBER_P(val), val, SCM_ARG2, S_mus_b1);
  return(TO_SCM_DOUBLE(mus_set_b1(TO_CLM(obj), TO_C_DOUBLE(val))));
}

static SCM g_set_b2(SCM obj, SCM val)
{
  SCM_ASSERT(MUS_SCM_P(obj), obj, SCM_ARG1, S_mus_b2);
  SCM_ASSERT(NUMBER_P(val), val, SCM_ARG2, S_mus_b2);
  return(TO_SCM_DOUBLE(mus_set_b2(TO_CLM(obj), TO_C_DOUBLE(val))));
}

static void init_smpflt(void)
{
  DEFINE_PROC(S_make_one_zero, g_make_one_zero, 0, 4, 0, H_make_one_zero);
  DEFINE_PROC(S_one_zero,      g_one_zero, 1, 1, 0,      H_one_zero);
  DEFINE_PROC(S_one_zero_p,    g_one_zero_p, 1, 0, 0,    H_one_zero_p);
  DEFINE_PROC(S_make_one_pole, g_make_one_pole, 0, 4, 0, H_make_one_pole);
  DEFINE_PROC(S_one_pole,      g_one_pole, 1, 1, 0,      H_one_pole);
  DEFINE_PROC(S_one_pole_p,    g_one_pole_p, 1, 0, 0,    H_one_pole_p);
  DEFINE_PROC(S_make_two_zero, g_make_two_zero, 0, 6, 0, H_make_two_zero);
  DEFINE_PROC(S_two_zero,      g_two_zero, 1, 1, 0,      H_two_zero);
  DEFINE_PROC(S_two_zero_p,    g_two_zero_p, 1, 0, 0,    H_two_zero_p);
  DEFINE_PROC(S_make_two_pole, g_make_two_pole, 0, 6, 0, H_make_two_pole);
  DEFINE_PROC(S_two_pole,      g_two_pole, 1, 1, 0,      H_two_pole);
  DEFINE_PROC(S_two_pole_p,    g_two_pole_p, 1, 0, 0,    H_two_pole_p);
  DEFINE_PROC(S_make_zpolar,   g_make_zpolar, 0, 4, 0,   H_make_zpolar);
  DEFINE_PROC(S_make_ppolar,   g_make_ppolar, 0, 4, 0,   H_make_ppolar);

  define_procedure_with_setter(S_mus_a0, SCM_FNC g_a0, H_mus_a0,
			       S_mus_set_a0, SCM_FNC g_set_a0, local_doc, 1, 0, 2, 0);

  define_procedure_with_setter(S_mus_a1, SCM_FNC g_a1, H_mus_a1,
			       S_mus_set_a1, SCM_FNC g_set_a1, local_doc, 1, 0, 2, 0);

  define_procedure_with_setter(S_mus_b1, SCM_FNC g_b1, H_mus_b1,
			       S_mus_set_b1, SCM_FNC g_set_b1, local_doc, 1, 0, 2, 0);

  define_procedure_with_setter(S_mus_b2, SCM_FNC g_b2, H_mus_b2,
			       S_mus_set_b2, SCM_FNC g_set_b2, local_doc, 1, 0, 2, 0);

  define_procedure_with_setter(S_mus_a2, SCM_FNC g_a2, H_mus_a2,
			       S_mus_set_a2, SCM_FNC g_set_a2, local_doc, 1, 0, 2, 0);
}



/* ---------------- formant ---------------- */

#define S_make_formant "make-formant"
#define S_formant "formant"
#define S_formant_bank "formant-bank"
#define S_formant_p "formant?"
#define S_mus_formant_radius "mus-formant-radius"
#define S_mus_set_formant_radius "mus-set-formant-radius"
#define S_mus_set_formant_radius_and_frequency "mus-set-formant-radius-and-frequency"

static SCM g_make_formant(SCM arg1, SCM arg2, SCM arg3, SCM arg4, SCM arg5, SCM arg6)
{
  #define H_make_formant "(" S_make_formant " &opt-key radius frequency (gain 1.0))\n\
returns a new formant generator (a resonator).  radius sets the pole radius. \
frequency sets the resonance center frequency (Hz).  gain is an overall amplitude \
control."

  mus_scm *gn;
  int vals;
  SCM args[6], keys[3];
  int orig_arg[3] = {0, 0, 0};
  Float freq = 0.0, radius = 0.0, gain = 1.0;
  keys[0] = all_keys[C_radius];
  keys[1] = all_keys[C_frequency];
  keys[2] = all_keys[C_gain];
  args[0] = arg1; args[1] = arg2; args[2] = arg3; args[3] = arg4; args[4] = arg5; args[5] = arg6;
  vals = decode_keywords(S_make_formant, 3, keys, 6, args, orig_arg);
  if (vals > 0)
    {
      radius = fkeyarg(keys[0], S_make_formant, orig_arg[0] + 1, args[orig_arg[0]], radius);
      freq = fkeyarg(keys[1], S_make_formant, orig_arg[1] + 1, args[orig_arg[1]], freq);
      gain = fkeyarg(keys[2], S_make_formant, orig_arg[2] + 1, args[orig_arg[2]], gain);
    }
  gn = (mus_scm *)CALLOC(1, sizeof(mus_scm));
  gn->gen = mus_make_formant(radius, freq, gain);
  gn->nvcts = 0;
  return(mus_scm_to_smob(gn));
}

static SCM g_formant(SCM gen, SCM input)
{
  #define H_formant "(" S_formant " gen &optional (input 0.0)) -> next sample from resonator gen"
  Float in1 = 0.0;
  SCM_ASSERT((MUS_SCM_P(gen) && (mus_formant_p(TO_CLM(gen)))), gen, SCM_ARG1, S_formant);
  if (NUMBER_P(input)) in1 = TO_C_DOUBLE(input); else if (BOUND_P(input)) scm_wrong_type_arg(S_formant, 2, input);
  return(TO_SCM_DOUBLE(mus_formant(TO_CLM(gen), in1)));
}

static SCM g_formant_bank(SCM amps, SCM gens, SCM inp)
{
  #define H_formant_bank "(" S_formant_bank " scls gens inval) -> sum a bank of " S_formant "s: scls[i]*" S_formant "(gens[i], inval)"
  SCM_ASSERT(VECTOR_P(gens), gens, SCM_ARG2, S_formant_bank);
  return(g_mus_bank(gens, amps, inp, SCM_UNDEFINED));
}

static SCM g_formant_p(SCM os) 
{
  #define H_formant_p "(" S_formant_p " gen) -> #t if gen is a " S_formant " generator, else #f"
  return(((MUS_SCM_P(os)) && (mus_formant_p(TO_CLM(os)))) ? SCM_BOOL_T : SCM_BOOL_F);
}

static SCM g_formant_radius (SCM gen)
{
  #define H_mus_formant_radius  "(" S_mus_formant_radius  " gen) -> (" S_formant " generator) gen's pole radius \
(the closer the radius is to 1.0, the narrower the resonance)."

  SCM_ASSERT((MUS_SCM_P(gen) && (mus_formant_p(TO_CLM(gen)))), gen, SCM_ARG1, S_mus_formant_radius);
  return(TO_SCM_DOUBLE(mus_formant_radius(TO_CLM(gen))));
}

static SCM g_set_formant_radius (SCM gen, SCM val)
{
  #define H_mus_set_formant_radius  "(" S_mus_set_formant_radius  " gen val) sets (" S_formant " generator) gen's " S_mus_formant_radius " to val"
  SCM_ASSERT((MUS_SCM_P(gen) && (mus_formant_p(TO_CLM(gen)))), gen, SCM_ARG1, S_mus_set_formant_radius);
  SCM_ASSERT(NUMBER_P(val), val, SCM_ARG2, S_mus_set_formant_radius);
  return(TO_SCM_DOUBLE(mus_set_formant_radius(TO_CLM(gen), TO_C_DOUBLE(val))));
}

static SCM g_set_formant_radius_and_frequency (SCM gen, SCM rad, SCM frq)
{
  #define H_mus_set_formant_radius_and_frequency  "(" S_mus_set_formant_radius_and_frequency  " gen radius frequency) sets (" S_formant " \
generator) gen's radius and frequency"

  SCM_ASSERT((MUS_SCM_P(gen) && (mus_formant_p(TO_CLM(gen)))), gen, SCM_ARG1, S_mus_set_formant_radius_and_frequency);
  SCM_ASSERT(NUMBER_P(rad), rad, SCM_ARG2, S_mus_set_formant_radius_and_frequency);
  SCM_ASSERT(NUMBER_P(frq), frq, SCM_ARG3, S_mus_set_formant_radius_and_frequency);
  mus_set_formant_radius_and_frequency(TO_CLM(gen), TO_C_DOUBLE(rad), TO_C_DOUBLE(frq));
  return(rad);
}

static void init_formant(void)
{
  DEFINE_PROC(S_formant_bank, g_formant_bank, 2, 1, 0, H_formant_bank);
  DEFINE_PROC(S_formant_p,    g_formant_p, 1, 0, 0,    H_formant_p);
  DEFINE_PROC(S_make_formant, g_make_formant, 0, 6, 0, H_make_formant);
  DEFINE_PROC(S_formant,      g_formant, 1, 1, 0,      H_formant);

  define_procedure_with_setter(S_mus_formant_radius, SCM_FNC g_formant_radius, H_mus_formant_radius,
			       S_mus_set_formant_radius, SCM_FNC g_set_formant_radius, local_doc, 1, 0, 2, 0);

  DEFINE_PROC(S_mus_set_formant_radius_and_frequency, g_set_formant_radius_and_frequency, 3, 0, 0, H_mus_set_formant_radius_and_frequency);
}



/* ---------------- frame ---------------- */

#define S_make_frame       "make-frame"
#define S_frame_p          "frame?"
#define S_frame_add        "frame+"
#define S_frame_multiply   "frame*"
#define S_frame_ref        "frame-ref"
#define S_frame_set        "frame-set!"
#define S_frame            "frame"

static SCM g_make_frame(SCM arglist)
{
  #define H_make_frame "(" S_make_frame " chans val0 val1 ...) returns a new frame object \
with chans samples, each sample set from the trailing arguments (defaulting to 0.0):\n\
   (set! fr0 (make-frame 2 .1 .2))"

  /* make_empty_frame from first of arglist, then if more args, load vals */
  mus_scm *gn;
  mus_frame *fr;
  SCM cararg, lst;
  int size = 0, i, len;
  SCM_ASSERT(LIST_P_WITH_LENGTH(arglist, len), arglist, SCM_ARG1, S_make_frame);
  if (len == 0) scm_wrong_num_args(TO_SCM_STRING(S_make_frame));
  cararg = SCM_CAR(arglist);
  if (!(NUMBER_P(cararg))) scm_wrong_type_arg(S_make_frame, 1, cararg);
  size = TO_C_INT_OR_ELSE(cararg, 0);
  if (len > (size + 1)) len = size + 1;
  if (size <= 0)
    mus_misc_error(S_make_frame, "size: ", TO_SCM_INT(size));
  gn = (mus_scm *)CALLOC(1, sizeof(mus_scm));
  gn->gen = (mus_any *)mus_make_empty_frame(size);
  if (len > 1)
    {
      fr = (mus_frame *)(gn->gen);
      for (i = 1, lst = SCM_CDR(arglist); i < len; i++, lst = SCM_CDR(lst))
	if (NUMBER_P(SCM_CAR(lst)))
	  fr->vals[i - 1] = TO_C_DOUBLE(SCM_CAR(lst));
	else
	  {
	    mus_free(gn->gen);
	    FREE(gn);
	    mus_misc_error(S_make_frame, "invalid arg:", SCM_CAR(lst));
	  }
    }
  return(mus_scm_to_smob(gn));
}

static SCM g_frame_p(SCM obj) 
{
  #define H_frame_p "(" S_frame_p " gen) -> #t if gen is a " S_frame " object, else #f"
  return(((MUS_SCM_P(obj)) && (mus_frame_p(TO_CLM(obj)))) ? SCM_BOOL_T : SCM_BOOL_F);
}

#define DONT_FREE_FRAME -1
#define FREE_FRAME 1

static SCM g_wrap_frame(mus_frame *val, int dealloc)
{
  mus_scm *gn;
  gn = (mus_scm *)CALLOC(1, sizeof(mus_scm));
  gn->gen = (mus_any *)val;
  gn->nvcts = dealloc;
  return(mus_scm_to_smob(gn));
}

static SCM g_frame_add(SCM uf1, SCM uf2, SCM ures) /* optional res */
{
  #define H_frame_add "(" S_frame_add " f1 f2 &optional outf) adds f1 and f2 returning outf; \
if outf is not given, a new frame is created. outf[i] = f1[i] + f2[i]"

  mus_frame *res = NULL;
  SCM_ASSERT((MUS_SCM_P(uf1)) && (mus_frame_p(TO_CLM(uf1))), uf1, SCM_ARG1, S_frame_add);
  SCM_ASSERT((MUS_SCM_P(uf2)) && (mus_frame_p(TO_CLM(uf2))), uf2, SCM_ARG2, S_frame_add);
  if ((MUS_SCM_P(ures)) && 
      (mus_frame_p(TO_CLM(ures)))) 
    res = (mus_frame *)TO_CLM(ures);
  return(g_wrap_frame(mus_frame_add((mus_frame *)TO_CLM(uf1),
				    (mus_frame *)TO_CLM(uf2),
				    res),
		      (res) ? DONT_FREE_FRAME : FREE_FRAME));
}

static SCM g_frame_multiply(SCM uf1, SCM uf2, SCM ures) /* optional res */
{
  #define H_frame_multiply "(" S_frame_multiply " f1 f2 &optional outf) multiplies f1 and f2 returning outf; \
if outf is not given, a new frame is created. outf[i] = f1[i] * f2[i]."

  mus_frame *res = NULL;
  SCM_ASSERT((MUS_SCM_P(uf1)) && (mus_frame_p(TO_CLM(uf1))), uf1, SCM_ARG1, S_frame_multiply);
  SCM_ASSERT((MUS_SCM_P(uf2)) && (mus_frame_p(TO_CLM(uf2))), uf2, SCM_ARG2, S_frame_multiply);
  if ((MUS_SCM_P(ures)) && 
      (mus_frame_p(TO_CLM(ures)))) 
    res = (mus_frame *)TO_CLM(ures);
  return(g_wrap_frame(mus_frame_multiply((mus_frame *)TO_CLM(uf1),
					 (mus_frame *)TO_CLM(uf2),
					 res),
		      (res) ? DONT_FREE_FRAME : FREE_FRAME));
}

static SCM g_frame_ref(SCM uf1, SCM uchan)
{
  #define H_frame_ref "(" S_frame_ref " f chan) -> f[chan] (the chan-th sample in frame f"
  SCM_ASSERT((MUS_SCM_P(uf1)) && (mus_frame_p(TO_CLM(uf1))), uf1, SCM_ARG1, S_frame_ref);
  SCM_ASSERT(INTEGER_P(uchan), uchan, SCM_ARG2, S_frame_ref);
  return(TO_SCM_DOUBLE(mus_frame_ref((mus_frame *)TO_CLM(uf1), TO_C_INT(uchan))));
}

static SCM g_set_frame_ref(SCM uf1, SCM uchan, SCM val)
{
  #define H_frame_set "(" S_frame_set " f chan val) sets frame f's chan-th sample to val: f[chan] = val"
  SCM_ASSERT((MUS_SCM_P(uf1)) && (mus_frame_p(TO_CLM(uf1))), uf1, SCM_ARG1, S_frame_set);
  SCM_ASSERT(INTEGER_P(uchan), uchan, SCM_ARG2, S_frame_set);
  SCM_ASSERT(NUMBER_P(val), val, SCM_ARG3, S_frame_set);
  return(TO_SCM_DOUBLE(mus_frame_set((mus_frame *)TO_CLM(uf1), TO_C_INT(uchan), TO_C_DOUBLE(val))));
}

static void init_frame(void)
{
  DEFINE_PROC(S_make_frame,     g_make_frame, 0, 0, 1,     H_make_frame);
  DEFINE_PROC(S_frame_p,        g_frame_p, 1, 0, 0,        H_frame_p);
  DEFINE_PROC(S_frame_add,      g_frame_add, 2, 1, 0,      H_frame_add);
  DEFINE_PROC(S_frame_multiply, g_frame_multiply, 2, 1, 0, H_frame_multiply);
  DEFINE_PROC(S_frame_ref,      g_frame_ref, 2, 0, 0,      H_frame_ref);
  DEFINE_PROC(S_frame_set,      g_set_frame_ref, 3, 0, 0,  H_frame_set);
}



/* ---------------- mixer ---------------- */

#define S_make_mixer       "make-mixer"
#define S_mixer_p          "mixer?"
#define S_mixer_multiply   "mixer*"
#define S_mixer_ref        "mixer-ref"
#define S_mixer_set        "mixer-set!"
#define S_mixer            "mixer"

#define S_frame2sample     "frame->sample"
#define S_sample2frame     "sample->frame"
#define S_frame2frame      "frame->frame"
#define S_frame2list       "frame->list"

static SCM g_mixer_p(SCM obj) 
{
  #define H_mixer_p "(" S_mixer_p " gen) -> #t if gen is a " S_mixer " object, else #f"
  return(((MUS_SCM_P(obj)) && (mus_mixer_p(TO_CLM(obj)))) ? SCM_BOOL_T : SCM_BOOL_F);
}

static SCM g_mixer_ref(SCM uf1, SCM in, SCM out)
{
  #define H_mixer_ref "(" S_mixer_ref " m in out) -> m[in, out], the mixer coefficient at location (in, out)"
  SCM_ASSERT((MUS_SCM_P(uf1)) && (mus_mixer_p(TO_CLM(uf1))), uf1, SCM_ARG1, S_mixer_ref);
  SCM_ASSERT(INTEGER_P(in), in, SCM_ARG2, S_mixer_ref);
  SCM_ASSERT(INTEGER_P(out), out, SCM_ARG3, S_mixer_ref);
  return(TO_SCM_DOUBLE(mus_mixer_ref((mus_mixer *)TO_CLM(uf1),
				     TO_C_INT(in),
				     TO_C_INT(out))));
}

static SCM g_set_mixer_ref(SCM uf1, SCM in, SCM out, SCM val)
{
  #define H_mixer_set "(" S_mixer_set " m in out val) sets m[in, out] = val"
  SCM_ASSERT((MUS_SCM_P(uf1)) && (mus_mixer_p(TO_CLM(uf1))), uf1, SCM_ARG1, S_mixer_set);
  SCM_ASSERT(INTEGER_P(in), in, SCM_ARG2, S_mixer_set);
  SCM_ASSERT(INTEGER_P(out), out, SCM_ARG2, S_mixer_set);
  SCM_ASSERT(NUMBER_P(val), val, SCM_ARG4, S_mixer_set);
  return(TO_SCM_DOUBLE(mus_mixer_set((mus_mixer *)TO_CLM(uf1),
				     TO_C_INT(in),
				     TO_C_INT(out),
				     TO_C_DOUBLE(val))));
}

#define DONT_FREE_MIXER -1
#define FREE_MIXER 1

static SCM g_wrap_mixer(mus_mixer *val, int dealloc)
{
  mus_scm *gn;
  gn = (mus_scm *)CALLOC(1, sizeof(mus_scm));
  gn->gen = (mus_any *)val;
  gn->nvcts = dealloc;
  return(mus_scm_to_smob(gn));
}

static SCM g_mixer_multiply(SCM uf1, SCM uf2, SCM ures) /* optional res */
{
  #define H_mixer_multiply "(" S_mixer_multiply " m1 m2 &optional outm) multiplies mixers m1 and m2 \
(a matrix multiply), returning the mixer outm, or creating a new mixer if outm is not given."

  mus_mixer *res = NULL;
  SCM_ASSERT((MUS_SCM_P(uf1)) && (mus_mixer_p(TO_CLM(uf1))), uf1, SCM_ARG1, S_mixer_multiply);
  SCM_ASSERT((MUS_SCM_P(uf2)) && (mus_mixer_p(TO_CLM(uf2))), uf2, SCM_ARG2, S_mixer_multiply);
  if ((MUS_SCM_P(ures)) && 
      (mus_mixer_p(TO_CLM(ures))))
    res = (mus_mixer *)TO_CLM(ures);
  return(g_wrap_mixer(mus_mixer_multiply((mus_mixer *)TO_CLM(uf1),
					 (mus_mixer *)TO_CLM(uf2),
					 res),
		      (res) ? DONT_FREE_MIXER : FREE_MIXER));
}

static SCM g_frame2frame(SCM mx, SCM infr, SCM outfr) /* optional outfr */
{
  #define H_frame2frame "(" S_frame2frame " m f &optional outf) passes frame f through mixer m \
returning frame outf (or creating a new frame if necessary); this is a matrix multiply of m and f"

  mus_frame *res = NULL;
  SCM_ASSERT((MUS_SCM_P(mx)) && (mus_mixer_p(TO_CLM(mx))), mx, SCM_ARG1, S_frame2frame);
  SCM_ASSERT((MUS_SCM_P(infr)) && (mus_frame_p(TO_CLM(infr))), infr, SCM_ARG2, S_frame2frame);
  if ((MUS_SCM_P(outfr)) && 
      (mus_frame_p(TO_CLM(outfr)))) 
    res = (mus_frame *)TO_CLM(outfr);
  return(g_wrap_frame(mus_frame2frame((mus_mixer *)TO_CLM(mx),
				      (mus_frame *)TO_CLM(infr),
				      res),
		      (res) ? DONT_FREE_FRAME : FREE_FRAME));
}

static SCM g_frame2list(SCM fr)
{
  #define H_frame2list "(" S_frame2list " f) -> contents of frame f as a list"
  mus_frame *val;
  int i;
  SCM res = SCM_EOL;
  SCM_ASSERT((MUS_SCM_P(fr)) && (mus_frame_p(TO_CLM(fr))), fr, SCM_ARG1, S_frame2list);
  val = (mus_frame *)TO_CLM(fr);
  for (i = (val->chans) - 1; i >= 0; i--) 
    res = CONS(TO_SCM_DOUBLE(val->vals[i]), res);
  return(scm_return_first(res, fr));
}

static SCM g_frame2sample(SCM mx, SCM fr)
{
  #define H_frame2sample "(" S_frame2sample " m f) -> pass frame f through mixer (or frame) m to produce a sample"
  SCM_ASSERT((MUS_SCM_P(mx)), mx, SCM_ARG1, S_frame2sample);
  SCM_ASSERT((MUS_SCM_P(fr)) && (mus_frame_p(TO_CLM(fr))), fr, SCM_ARG2, S_frame2sample);
  return(TO_SCM_DOUBLE(mus_frame2sample(TO_CLM(mx),
					(mus_frame *)TO_CLM(fr))));
}

static SCM g_sample2frame(SCM mx, SCM insp, SCM outfr) /* optional outfr */
{
  #define H_sample2frame "(" S_sample2frame " m val &optional outf) passes the sample val through mixer m \
returning frame outf (creating it if necessary)"

  mus_frame *res = NULL;
  SCM_ASSERT((MUS_SCM_P(mx)), mx, SCM_ARG1, S_sample2frame);
  SCM_ASSERT(NUMBER_P(insp), insp, SCM_ARG2, S_sample2frame);
  if ((MUS_SCM_P(outfr)) && 
      (mus_frame_p(TO_CLM(outfr)))) 
    res = (mus_frame *)TO_CLM(outfr);
  return(g_wrap_frame(mus_sample2frame(TO_CLM(mx),
				       TO_C_DOUBLE(insp),
				       res),
		      (res) ? DONT_FREE_FRAME : FREE_FRAME));
}

static SCM g_make_mixer(SCM arglist)
{
  #define H_make_mixer "(" S_make_mixer " chans val0 val1 ...) makes a new mixer object \
with chans inputs and outputs, initializing the scalers from the rest of the arguments:\n\
   (set! gen (make-mixer 2 .5 .25 .125 1.0))"

  /* make_empty_mixer from first of arglist, then if more args, load vals */
  mus_scm *gn;
  mus_mixer *fr;
  SCM cararg, lst;
  int size = 0, i, j, k, len;
  SCM_ASSERT(LIST_P_WITH_LENGTH(arglist, len), arglist, SCM_ARG1, S_make_mixer);
  if (len == 0) mus_misc_error(S_make_mixer, "need at least 1 arg", arglist);
  cararg = SCM_CAR(arglist);
  if (!(NUMBER_P(cararg))) mus_misc_error(S_make_mixer, "first arg is the number of chans", cararg);
  size = TO_C_INT_OR_ELSE(cararg, 0);
  if (size <= 0) mus_misc_error(S_make_mixer, "chans <= 0?", cararg);
  if (size > 256) mus_misc_error(S_make_mixer, "chans > 256?", cararg);
  gn = (mus_scm *)CALLOC(1, sizeof(mus_scm));
  gn->gen = (mus_any *)mus_make_empty_mixer(size);
  if (len > 1)
    {
      fr = (mus_mixer *)(gn->gen);
      j = 0;
      k = 0;
      for (i = 1, lst = SCM_CDR(arglist); (i < len) && (NOT_NULL_P(lst)); i++, lst = SCM_CDR(lst))
	{
	  if (NUMBER_P(SCM_CAR(lst)))
	    fr->vals[j][k] = TO_C_DOUBLE(SCM_CAR(lst));
	  else
	    {
	      mus_free(gn->gen);
	      FREE(gn);
	      mus_misc_error(S_make_mixer, "invalid arg (not a number):", SCM_CAR(lst));
	    }
	  k++;
	  if (k == size)
	    {
	      k = 0;
	      j++;
	      if (j >= size) break;
	    }
	}
    }
  return(mus_scm_to_smob(gn));
}

static void init_mixer(void)
{
  DEFINE_PROC(S_make_mixer,     g_make_mixer, 0, 0, 1,     H_make_mixer);
  DEFINE_PROC(S_mixer_p,        g_mixer_p, 1, 0, 0,        H_mixer_p);
  DEFINE_PROC(S_mixer_multiply, g_mixer_multiply, 2, 1, 0, H_mixer_multiply);
  DEFINE_PROC(S_mixer_ref,      g_mixer_ref, 3, 0, 0,      H_mixer_ref);
  DEFINE_PROC(S_mixer_set,      g_set_mixer_ref, 4, 0, 0,  H_mixer_set);
  DEFINE_PROC(S_frame2sample,   g_frame2sample, 2, 0, 0,   H_frame2sample);
  DEFINE_PROC(S_frame2list,     g_frame2list, 1, 0, 0,     H_frame2list);
  DEFINE_PROC(S_frame2frame,    g_frame2frame, 2, 1, 0,    H_frame2frame);
  DEFINE_PROC(S_sample2frame,   g_sample2frame, 2, 1, 0,   H_sample2frame);
}


/* ---------------- buffer ---------------- */

#define S_make_buffer    "make-buffer"
#define S_buffer_p       "buffer?"
#define S_buffer2sample  "buffer->sample"
#define S_sample2buffer  "sample->buffer"
#define S_buffer2frame   "buffer->frame"
#define S_frame2buffer   "frame->buffer"
#define S_buffer_empty_p "buffer-empty?"
#define S_buffer_full_p  "buffer-full?"
#define S_buffer         "buffer"

static SCM g_buffer_p(SCM obj) 
{
  #define H_buffer_p "(" S_buffer_p " gen) -> #t if gen is a " S_buffer " object, else #f"
  return(((MUS_SCM_P(obj)) && (mus_buffer_p(TO_CLM(obj)))) ? SCM_BOOL_T : SCM_BOOL_F);
}

static SCM g_make_buffer(SCM arg1, SCM arg2, SCM arg3, SCM arg4)
{
  #define H_make_buffer "(" S_make_buffer " &opt-key (size 512) fill-time) returns a new buffer \
generator, a FIFO for samples. The size argument sets the size of the buffer (not a delay time) \
and fill-time sets the time to the next request for more samples.  The intended use is in block \
processing normally involving overlap-adds."

  mus_scm *gn;
  SCM args[4], keys[2];
  int orig_arg[2] = {0, 0};
  int vals;
  Float *buf;
  int siz = DEFAULT_TABLE_SIZE;
  Float filltime = 0.0;
  keys[0] = all_keys[C_size];
  keys[1] = all_keys[C_fill_time];
  args[0] = arg1; args[1] = arg2; args[2] = arg3; args[3] = arg4;
  vals = decode_keywords(S_make_buffer, 2, keys, 4, args, orig_arg);
  if (vals > 0)
    {
      siz = ikeyarg(keys[0], S_make_buffer, orig_arg[0] + 1, args[orig_arg[0]], siz);
      filltime = fkeyarg(keys[1], S_make_buffer, orig_arg[1] + 1, args[orig_arg[1]], 0.0);
    }
  if (siz <= 0) return(SCM_BOOL_F);
  buf = (Float *)CALLOC(siz, sizeof(Float));
  gn = (mus_scm *)CALLOC(1, sizeof(mus_scm));
  gn->vcts = (SCM *)CALLOC(1, sizeof(SCM));
  gn->vcts[0] = SCM_EOL;
  gn->nvcts = 1;
  gn->gen = mus_make_buffer(buf, siz, filltime);
  return(mus_scm_to_smob_with_vct(gn, make_vct(siz, buf)));
}

static SCM g_buffer2sample(SCM obj)
{
  #define H_buffer2sample "(" S_buffer2sample " gen) -> next sample in buffer, removing it"
  SCM_ASSERT((MUS_SCM_P(obj)) && (mus_buffer_p(TO_CLM(obj))), obj, SCM_ARG1, S_buffer2sample);
  return(TO_SCM_DOUBLE(mus_buffer2sample(TO_CLM(obj))));
}

static SCM g_buffer2frame(SCM obj, SCM fr)
{
  #define H_buffer2frame "(" S_buffer2frame " gen) -> next frame of samples in buffer, removing them"
  SCM_ASSERT((MUS_SCM_P(obj)) && (mus_buffer_p(TO_CLM(obj))), obj, SCM_ARG1, S_buffer2frame);
  SCM_ASSERT((MUS_SCM_P(fr)) && (mus_frame_p(TO_CLM(fr))), fr, SCM_ARG2, S_buffer2frame);
  return(g_wrap_frame((mus_frame *)mus_buffer2frame(TO_CLM(obj),
						    TO_CLM(fr)),
		      DONT_FREE_FRAME));
}

static SCM g_buffer_empty_p(SCM obj)
{
  #define H_buffer_empty_p "(" S_buffer_empty_p " gen) -> #t if buffer is in need of more samples"
  SCM_ASSERT((MUS_SCM_P(obj)) && (mus_buffer_p(TO_CLM(obj))), obj, SCM_ARG1, S_buffer_empty_p);
  return(TO_SMALL_SCM_INT(mus_buffer_empty_p(TO_CLM(obj))));
}

static SCM g_buffer_full_p(SCM obj)
{
  #define H_buffer_full_p "(" S_buffer_full_p " gen) -> #t if buffer has no room for any more samples"
  SCM_ASSERT((MUS_SCM_P(obj)) && (mus_buffer_p(TO_CLM(obj))), obj, SCM_ARG1, S_buffer_full_p);
  return(TO_SMALL_SCM_INT(mus_buffer_full_p(TO_CLM(obj))));
}

static SCM g_sample2buffer(SCM obj, SCM val)
{
  #define H_sample2buffer "(" S_sample2buffer " gen val) append val to current end of data in buffer"
  SCM_ASSERT((MUS_SCM_P(obj)) && (mus_buffer_p(TO_CLM(obj))), obj, SCM_ARG1, S_sample2buffer);
  SCM_ASSERT(NUMBER_P(val), val, SCM_ARG1, S_sample2buffer);
  return(TO_SCM_DOUBLE(mus_sample2buffer(TO_CLM(obj),
					 TO_C_DOUBLE(val))));
}

static SCM g_frame2buffer(SCM obj, SCM val)
{
  #define H_frame2buffer "(" S_frame2buffer " gen f) appends sample in frame f to end of data in buffer"
  SCM_ASSERT((MUS_SCM_P(obj)) && (mus_buffer_p(TO_CLM(obj))), obj, SCM_ARG1, S_frame2buffer);
  SCM_ASSERT((MUS_SCM_P(val)) && (mus_frame_p(TO_CLM(val))), val, SCM_ARG2, S_frame2buffer);
  return(g_wrap_frame((mus_frame *)mus_frame2buffer(TO_CLM(obj),
						    TO_CLM(val)),
		      DONT_FREE_FRAME));
}

static void init_rblk(void)
{
  DEFINE_PROC(S_make_buffer,    g_make_buffer, 0, 4, 0,    H_make_buffer);
  DEFINE_PROC(S_buffer_p,       g_buffer_p, 1, 0, 0,       H_buffer_p);
  DEFINE_PROC(S_buffer_empty_p, g_buffer_empty_p, 1, 0, 0, H_buffer_empty_p);
  DEFINE_PROC(S_buffer_full_p,  g_buffer_full_p, 1, 0, 0,  H_buffer_full_p);
  DEFINE_PROC(S_buffer2sample,  g_buffer2sample, 1, 0, 0,  H_buffer2sample);
  DEFINE_PROC(S_buffer2frame,   g_buffer2frame, 2, 0, 0,   H_buffer2frame);
  DEFINE_PROC(S_sample2buffer,  g_sample2buffer, 2, 0, 0,  H_sample2buffer);
  DEFINE_PROC(S_frame2buffer,   g_frame2buffer, 2, 0, 0,   H_frame2buffer);
}


/* ---------------- wave-train ---------------- */

#define S_make_wave_train "make-wave-train"
#define S_wave_train "wave-train"
#define S_wave_train_p "wave-train?"

static SCM g_make_wave_train(SCM arg1, SCM arg2, SCM arg3, SCM arg4, SCM arg5, SCM arg6)
{
  #define H_make_wave_train "(" S_make_wave_train " &opt-key (frequency 440.0) (initial-phase 0.0) wave)\n\
returns a new wave-train generator (an extension of pulse-train).   Frequency is \
the repetition rate of the wave found in wave. Successive waves can overlap."

  mus_scm *gn;
  SCM args[6], keys[3];
  int orig_arg[3] = {0, 0, 0};
  int vals, wsize;
  vct *v;
  Float freq = 440.0;
  Float phase = 0.0;
  Float *wave = NULL;
  wsize = DEFAULT_TABLE_SIZE;
  keys[0] = all_keys[C_frequency];
  keys[1] = all_keys[C_initial_phase];
  keys[2] = all_keys[C_wave];
  args[0] = arg1; args[1] = arg2; args[2] = arg3; args[3] = arg4; args[4] = arg5; args[5] = arg6; 
  vals = decode_keywords(S_make_wave_train, 3, keys, 6, args, orig_arg);
  if (vals > 0)
    {
      freq = fkeyarg(keys[0], S_make_wave_train, orig_arg[0] + 1, args[orig_arg[0]], freq);
      phase = fkeyarg(keys[1], S_make_wave_train, orig_arg[1] + 1, args[orig_arg[1]], phase);
      if (!(KEYWORD_P(keys[2])))
        {
	  if (VCT_P(keys[2]))
	    {
	      v = TO_VCT(keys[2]);
	      wave = copy_vct_data(v);
	      wsize = v->length;
	    }
          else scm_wrong_type_arg(S_make_wave_train, orig_arg[2] + 1, args[orig_arg[2]]);
        }
    }
  if (wave == NULL) wave = (Float *)CALLOC(wsize, sizeof(Float));
  gn = (mus_scm *)CALLOC(1, sizeof(mus_scm));
  gn->vcts = (SCM *)CALLOC(1, sizeof(SCM));
  gn->vcts[0] = SCM_EOL;
  gn->nvcts = 1;
  gn->gen = mus_make_wave_train(freq, phase, wave, wsize);
  return(mus_scm_to_smob_with_vct(gn, make_vct(wsize, wave)));
}

static SCM g_wave_train(SCM obj, SCM fm)
{
  #define H_wave_train "(" S_wave_train " gen &optional (fm 0.0)) -> next sample of wave-train"
  Float fm1 = 0.0;
  SCM_ASSERT((MUS_SCM_P(obj)) && (mus_wave_train_p(TO_CLM(obj))), obj, SCM_ARG1, S_wave_train);
  if (NUMBER_P(fm)) fm1 = TO_C_DOUBLE(fm); else if (BOUND_P(fm)) scm_wrong_type_arg(S_wave_train, 2, fm);
  return(TO_SCM_DOUBLE(mus_wave_train(TO_CLM(obj), fm1)));
}

static SCM g_wave_train_p(SCM obj) 
{
  #define H_wave_train_p "(" S_wave_train_p " gen) -> #t if gen is a " S_wave_train " generator, else #f"
  return(((MUS_SCM_P(obj)) && (mus_wave_train_p(TO_CLM(obj)))) ? SCM_BOOL_T : SCM_BOOL_F);
}

static void init_wt(void)
{
  DEFINE_PROC(S_make_wave_train, g_make_wave_train, 0, 6, 0, H_make_wave_train);
  DEFINE_PROC(S_wave_train,      g_wave_train, 1, 1, 0,      H_wave_train);
  DEFINE_PROC(S_wave_train_p,    g_wave_train_p, 1, 0, 0,    H_wave_train_p);
}


/* ---------------- waveshape ---------------- */

#define S_make_waveshape       "make-waveshape"
#define S_waveshape            "waveshape"
#define S_waveshape_p          "waveshape?"
#define S_partials2waveshape   "partials->waveshape"
#define S_partials2polynomial  "partials->polynomial"

static Float *list2partials(SCM harms, int *npartials)
{
  int listlen, i, maxpartial, curpartial;
  Float *partials = NULL;
  SCM lst;
  listlen = LIST_LENGTH(harms);
  if (listlen == 0) return(NULL);
  /* the list is '(partial-number partial-amp ... ) */
  maxpartial = TO_C_INT_OR_ELSE(SCM_CAR(harms), 0);
  for (i = 2, lst = SCM_CDDR(harms); i < listlen; i += 2, lst = SCM_CDDR(lst))
    {
      curpartial = TO_C_INT_OR_ELSE(SCM_CAR(lst), 0);
      if (curpartial > maxpartial) maxpartial = curpartial;
    }
  if (maxpartial < 0) return(NULL);
  partials = (Float *)CALLOC(maxpartial + 1, sizeof(Float));
  (*npartials) = maxpartial + 1;
  for (i = 0, lst = harms; i < listlen; i += 2, lst = SCM_CDDR(lst))
    {
      curpartial = TO_C_INT_OR_ELSE(SCM_CAR(lst), 0);
      partials[curpartial] = TO_C_DOUBLE(SCM_CADR(lst));
    }
  return(partials);
}

static SCM g_make_waveshape(SCM arg1, SCM arg2, SCM arg3, SCM arg4, SCM arg5, SCM arg6, SCM arg7, SCM arg8)
{
  #define H_make_waveshape "(" S_make_waveshape " &opt-key (frequency 440.0) (partials '(1 1)) (size 512) wave)\n\
returns a new waveshaping generator (essentially table-lookup driven by a sinewave)\n\
   (make-waveshape :wave (partials->waveshape '(1 1.0)))\n\
is basically the same as make-oscil"

  mus_scm *gn;
  SCM args[8], keys[4];
  int orig_arg[4] = {0, 0, 0, 0};
  int vals, wsize, npartials = 0, partials_allocated = 0;
  vct *v;
  Float freq = 440.0;
  Float *wave = NULL, *partials = NULL;
  wsize = DEFAULT_TABLE_SIZE;
  keys[0] = all_keys[C_frequency];
  keys[1] = all_keys[C_partials];
  keys[2] = all_keys[C_size];
  keys[3] = all_keys[C_wave];
  args[0] = arg1; args[1] = arg2; args[2] = arg3; args[3] = arg4; args[4] = arg5; args[5] = arg6; args[6] = arg7; args[7] = arg8;
  vals = decode_keywords(S_make_waveshape, 4, keys, 8, args, orig_arg);
  if (vals > 0)
    {
      freq = fkeyarg(keys[0], S_make_waveshape, orig_arg[0] + 1, args[orig_arg[0]], freq);
      if (!(KEYWORD_P(keys[1])))
        {
	  if (LIST_P(keys[1]))
	    {
	      partials = list2partials(keys[1], &npartials);
	      if (partials == NULL)
		mus_misc_error(S_make_waveshape, "partials list empty?", keys[1]);
	      partials_allocated = 1;
	    }
          else scm_wrong_type_arg(S_make_waveshape, orig_arg[1] + 1, args[orig_arg[1]]);
        }
      wsize = ikeyarg_or_error(keys[2], S_make_waveshape, orig_arg[2] + 1, args[orig_arg[2]], wsize);
      if (SCM_EQ_P(wsize, MUS_MISC_ERROR))
	{
	  if (partials_allocated) {FREE(partials); partials = NULL;}
	  scm_wrong_type_arg(S_make_waveshape, orig_arg[2] + 1, args[orig_arg[2]]);
	}
      if (!(KEYWORD_P(keys[3])))
        {
	  if (VCT_P(keys[3]))
	    {
	      v = TO_VCT(keys[3]);
	      wave = copy_vct_data(v);
	      wsize = v->length;
	    }
          else 
	    {
	      if (partials_allocated) {FREE(partials); partials = NULL;}
	      scm_wrong_type_arg(S_make_waveshape, orig_arg[3] + 1, args[orig_arg[3]]);
	    }
        }
    }
  if (wsize <= 0)
    mus_misc_error(S_make_waveshape, "size <= 0?", keys[2]);
  if (wave == NULL) 
    {
      if (partials == NULL) return(SCM_BOOL_F);
      wave = mus_partials2waveshape(npartials, partials, wsize, (Float *)CALLOC(wsize, sizeof(Float)));
    }
  if (partials_allocated) {FREE(partials); partials = NULL;}
  gn = (mus_scm *)CALLOC(1, sizeof(mus_scm));
  gn->vcts = (SCM *)CALLOC(1, sizeof(SCM));
  gn->vcts[0] = SCM_EOL;
  gn->nvcts = 1;
  gn->gen = mus_make_waveshape(freq, 0.0, wave, wsize);
  return(mus_scm_to_smob_with_vct(gn, make_vct(wsize, wave)));
}

static SCM g_waveshape(SCM obj, SCM index, SCM fm)
{
  #define H_waveshape "(" S_waveshape " gen &optional (index 1.0) (fm 0.0)) -> next sample of waveshaper"
  Float fm1 = 0.0, index1 = 1.0;
  SCM_ASSERT((MUS_SCM_P(obj)) && (mus_waveshape_p(TO_CLM(obj))), obj, SCM_ARG1, S_waveshape);
  if (NUMBER_P(index)) index1 = TO_C_DOUBLE(index); else if (BOUND_P(index)) scm_wrong_type_arg(S_waveshape, 2, index);
  if (NUMBER_P(fm)) fm1 = TO_C_DOUBLE(fm); else if (BOUND_P(fm)) scm_wrong_type_arg(S_waveshape, 3, fm);
  return(TO_SCM_DOUBLE(mus_waveshape(TO_CLM(obj), index1, fm1)));
}

static SCM g_waveshape_p(SCM obj) 
{
  #define H_waveshape_p "(" S_waveshape_p " gen) -> #t if gen is a " S_waveshape " generator, else #f"
  return(((MUS_SCM_P(obj)) && (mus_waveshape_p(TO_CLM(obj)))) ? SCM_BOOL_T : SCM_BOOL_F);
}

static SCM g_partials2waveshape(SCM amps, SCM s_size)
{
  #define H_partials2waveshape "(" S_partials2waveshape " partials &optional (size 512))\n\
produces a waveshaping lookup table (suitable for the " S_waveshape " generator) \
that will produce the harmonic spectrum given by the partials argument"

  int npartials, size, len;
  Float *partials, *wave;
  SCM gwave;
  SCM_ASSERT(LIST_P_WITH_LENGTH(amps, len), amps, SCM_ARG1, S_partials2waveshape);
  SCM_ASSERT(INTEGER_IF_BOUND_P(s_size), s_size, SCM_ARG2, S_partials2waveshape);
  if (INTEGER_P(s_size))
    size = TO_C_INT(s_size);
  else size = DEFAULT_TABLE_SIZE;
  if (size <= 0)
    mus_misc_error(S_partials2waveshape, "size <= 0?", s_size);
  if (len == 0)
    mus_misc_error(S_partials2waveshape, "partials list empty?", amps);
  partials = list2partials(amps, &npartials);
  wave = mus_partials2waveshape(npartials, partials, size, (Float *)CALLOC(size, sizeof(Float)));
  gwave = make_vct(size, wave);
  FREE(partials);
  return(gwave);
}

static SCM g_partials2polynomial(SCM amps, SCM ukind)
{
  #define H_partials2polynomial "(" S_partials2polynomial " partials &optional (kind 1))\n\
produces a Chebychev polynomial suitable for use with the " S_polynomial " generator \
to create (via waveshaping) the harmonic spectrum described by the partials argument:\n\
   (let ((v0 (partials->polynomial '(1 1 2 1)))\n\
         (os (make-oscil)))\n\
     (polynomial v0 (oscil os)))"

  int npartials, kind, len;
  Float *partials, *wave;
  SCM_ASSERT(LIST_P_WITH_LENGTH(amps, len), amps, SCM_ARG1, S_partials2polynomial);
  SCM_ASSERT(INTEGER_IF_BOUND_P(ukind), ukind, SCM_ARG2, S_partials2polynomial);
  if (INTEGER_P(ukind))
    kind = TO_C_INT(ukind);
  else kind = 1;
  if (len == 0)
    mus_misc_error(S_partials2polynomial, "partials list empty?", amps);
  partials = list2partials(amps, &npartials);
  wave = mus_partials2polynomial(npartials, partials, kind);
  return(make_vct(npartials, wave));
}

static void init_ws(void)
{
  DEFINE_PROC(S_make_waveshape,      g_make_waveshape, 0, 8, 0,      H_make_waveshape);
  DEFINE_PROC(S_waveshape,           g_waveshape, 1, 2, 0,           H_waveshape);
  DEFINE_PROC(S_waveshape_p,         g_waveshape_p, 1, 0, 0,         H_waveshape_p);
  DEFINE_PROC(S_partials2waveshape,  g_partials2waveshape, 1, 1, 0,  H_partials2waveshape);
  DEFINE_PROC(S_partials2polynomial, g_partials2polynomial, 1, 1, 0, H_partials2polynomial);
}


/* ---------------- sine-summation ---------------- */

#define S_make_sine_summation "make-sine-summation"
#define S_sine_summation      "sine-summation"
#define S_sine_summation_p    "sine-summation?"

static SCM g_sine_summation_p(SCM obj) 
{
  #define H_sine_summation_p "(" S_sine_summation_p " gen) -> #t if gen is a " S_sine_summation " generator, else #f"
  return(((MUS_SCM_P(obj)) && (mus_sine_summation_p(TO_CLM(obj)))) ? SCM_BOOL_T : SCM_BOOL_F);
}

static SCM g_sine_summation(SCM obj, SCM fm)
{
  #define H_sine_summation "(" S_sine_summation " gen &optional (fm 0.0)) -> next sample of sine summation generator"
  Float fm1 = 0.0;
  SCM_ASSERT((MUS_SCM_P(obj)) && (mus_sine_summation_p(TO_CLM(obj))), obj, SCM_ARG1, S_sine_summation);
  if (NUMBER_P(fm)) fm1 = TO_C_DOUBLE(fm); else if (BOUND_P(fm)) scm_wrong_type_arg(S_sine_summation, 2, fm);
  return(TO_SCM_DOUBLE(mus_sine_summation(TO_CLM(obj), fm1)));
}

static SCM g_make_sine_summation(SCM arglist)
{
  #define H_make_sine_summation "(" S_make_sine_summation " &opt-key (frequency 440.0) (initial-phase 0.0) (n 1) (a 0.5) (ratio 1.0)\n\
returns a new sine summation synthesis generator."

  mus_scm *gn;
  SCM args[10], keys[5];
  int orig_arg[5] = {0, 0, 0, 0, 0};
  int vals, i, arglist_len;
  Float freq = 440.0, phase = 0.0, a=.5, ratio = 1.0;
  int n = 1;
  keys[0] = all_keys[C_frequency];
  keys[1] = all_keys[C_initial_phase];
  keys[2] = all_keys[C_n];
  keys[3] = all_keys[C_a];
  keys[4] = all_keys[C_ratio];
  for (i = 0; i < 10; i++) args[i] = SCM_UNDEFINED;
  arglist_len = LIST_LENGTH(arglist);
  for (i = 0; i < arglist_len; i++) args[i] = LIST_REF(arglist, i);
  vals = decode_keywords(S_make_sine_summation, 5, keys, 10, args, orig_arg);
  if (vals > 0)
    {
      freq = fkeyarg(keys[0], S_make_sine_summation, orig_arg[0] + 1, args[orig_arg[0]], freq);
      phase = fkeyarg(keys[1], S_make_sine_summation, orig_arg[1] + 1, args[orig_arg[1]], phase);
      n = ikeyarg(keys[2], S_make_sine_summation, orig_arg[2] + 1, args[orig_arg[2]], n);
      a = fkeyarg(keys[3], S_make_sine_summation, orig_arg[3] + 1, args[orig_arg[3]], a);
      ratio = fkeyarg(keys[4], S_make_sine_summation, orig_arg[4] + 1, args[orig_arg[4]], ratio);
    }
  gn = (mus_scm *)CALLOC(1, sizeof(mus_scm));
  gn->gen = mus_make_sine_summation(freq, phase, n, a, ratio);
  return(mus_scm_to_smob(gn));
}

static void init_sss(void)
{
  DEFINE_PROC(S_make_sine_summation, g_make_sine_summation, 0, 0, 1, H_make_sine_summation);
  DEFINE_PROC(S_sine_summation,      g_sine_summation, 1, 1, 0,      H_sine_summation);
  DEFINE_PROC(S_sine_summation_p,    g_sine_summation_p, 1, 0, 0,    H_sine_summation_p);
}



/* ----------------  filter ---------------- */

#define S_filter          "filter"
#define S_filter_p        "filter?"
#define S_make_filter     "make-filter"
#define S_fir_filter      "fir-filter"
#define S_fir_filter_p    "fir-filter?"
#define S_make_fir_filter "make-fir-filter"
#define S_iir_filter      "iir-filter"
#define S_iir_filter_p    "iir-filter?"
#define S_make_iir_filter "make-iir-filter"
#define S_mus_xcoeffs     "mus-xcoeffs"
#define S_mus_ycoeffs     "mus-ycoeffs"
#define S_mus_order       "mus-order"

static SCM g_filter_p(SCM obj) 
{
  #define H_filter_p "(" S_filter_p " gen) -> #t if gen is a " S_filter " generator, else #f"
  return(((MUS_SCM_P(obj)) && (mus_filter_p(TO_CLM(obj)))) ? SCM_BOOL_T : SCM_BOOL_F);
}

static SCM g_fir_filter_p(SCM obj) 
{
  #define H_fir_filter_p "(" S_fir_filter_p " gen) -> #t if gen is a " S_fir_filter " generator, else #f"
  return(((MUS_SCM_P(obj)) && (mus_fir_filter_p(TO_CLM(obj)))) ? SCM_BOOL_T : SCM_BOOL_F);
}

static SCM g_iir_filter_p(SCM obj) 
{
  #define H_iir_filter_p "(" S_iir_filter_p " gen) -> #t if gen is a " S_iir_filter " generator, else #f"
  return(((MUS_SCM_P(obj)) && (mus_iir_filter_p(TO_CLM(obj)))) ? SCM_BOOL_T : SCM_BOOL_F);
}

static SCM g_filter(SCM obj, SCM input)
{
  #define H_filter "(" S_filter " gen &optional (input 0.0)) -> next sample from FIR/IIR filter"
  SCM_ASSERT((MUS_SCM_P(obj)) && (mus_filter_p(TO_CLM(obj))), obj, SCM_ARG1, S_filter);
  SCM_ASSERT(NUMBER_P(input), input, SCM_ARG2, S_filter);
  return(TO_SCM_DOUBLE(mus_filter(TO_CLM(obj), TO_C_DOUBLE(input))));
}

static SCM g_fir_filter(SCM obj, SCM input)
{
  #define H_fir_filter "(" S_fir_filter " gen &optional (input 0.0)) -> next sample from FIR filter"
  SCM_ASSERT((MUS_SCM_P(obj)) && (mus_fir_filter_p(TO_CLM(obj))), obj, SCM_ARG1, S_fir_filter);
  SCM_ASSERT(NUMBER_P(input), input, SCM_ARG2, S_fir_filter);
  return(TO_SCM_DOUBLE(mus_fir_filter(TO_CLM(obj), TO_C_DOUBLE(input))));
}

static SCM g_iir_filter(SCM obj, SCM input)
{
  #define H_iir_filter "(" S_iir_filter " gen &optional (input 0.0)) -> next sample from IIR filter"
  SCM_ASSERT((MUS_SCM_P(obj)) && (mus_iir_filter_p(TO_CLM(obj))), obj, SCM_ARG1, S_iir_filter);
  SCM_ASSERT(NUMBER_P(input), input, SCM_ARG2, S_iir_filter);
  return(TO_SCM_DOUBLE(mus_iir_filter(TO_CLM(obj), TO_C_DOUBLE(input))));
}

enum {G_FILTER, G_FIR_FILTER, G_IIR_FILTER};

static SCM g_make_filter_1(int choice, SCM arg1, SCM arg2, SCM arg3, SCM arg4, SCM arg5, SCM arg6)
{
  SCM xwave = SCM_UNDEFINED, ywave = SCM_UNDEFINED;
  mus_scm *gn = NULL;
  SCM args[6], keys[3];
  int orig_arg[3] = {0, 0, 0};
  vct *x = NULL, *y = NULL;
  int nkeys, vals, order = 0;
  char *caller;
  if (choice == G_FILTER) caller = S_make_filter; else if (choice == G_FIR_FILTER) caller = S_make_fir_filter; else caller = S_make_iir_filter;
  keys[0] = all_keys[C_order];
  if (choice == G_IIR_FILTER)
    keys[1] = all_keys[C_y_coeffs];
  else keys[1] = all_keys[C_x_coeffs];
  if (choice == G_FILTER)
    {
      keys[2] = all_keys[C_y_coeffs];
      nkeys = 3;
    }
  else nkeys = 2;
  args[0] = arg1; args[1] = arg2; args[2] = arg3; args[3] = arg4; args[4] = arg5; args[5] = arg6;
  vals = decode_keywords(caller, nkeys, keys, nkeys * 2, args, orig_arg);
  if (vals > 0)
    {
      order = ikeyarg(keys[0], caller, orig_arg[0] + 1, args[orig_arg[0]], 0);
      if (!(KEYWORD_P(keys[1])))
        {
	  if (VCT_P(keys[1]))
	    {
	      xwave = keys[1];
	      x = TO_VCT(xwave);
	    }
          else scm_wrong_type_arg(caller, orig_arg[1] + 1, args[orig_arg[1]]);
        }
      if (nkeys > 2)
	if (!(KEYWORD_P(keys[2])))
	  {
	    if (VCT_P(keys[2]))
	      {
		ywave = keys[2];
		y = TO_VCT(ywave);
	      }
	    else scm_wrong_type_arg(caller, orig_arg[2] + 1, args[orig_arg[2]]);
	  }
    }
  if (x == NULL)
    mus_misc_error(caller, "no coefficients?", SCM_BOOL_F);
  if ((choice == G_FILTER) && (y == NULL))
    {
      choice = G_FIR_FILTER;
      nkeys = 2;
    }
  gn = (mus_scm *)CALLOC(1, sizeof(mus_scm));
  gn->vcts = (SCM *)CALLOC(nkeys - 1, sizeof(SCM));
  gn->vcts[0] = SCM_EOL;
  if (nkeys > 2) gn->vcts[1] = SCM_EOL;
  gn->nvcts = nkeys - 1;
  switch (choice)
    {
    case G_FILTER: gn->gen = mus_make_filter(order, x->data, y->data, NULL); break;
    case G_FIR_FILTER: gn->gen = mus_make_fir_filter(order, x->data, NULL); break;
    case G_IIR_FILTER: gn->gen = mus_make_iir_filter(order, x->data, NULL); break;
    }
  gn->vcts[0] = xwave;
  if (nkeys > 2) gn->vcts[1] = ywave;
  return(mus_scm_to_smob(gn));
}

static SCM g_make_filter(SCM arg1, SCM arg2, SCM arg3, SCM arg4, SCM arg5, SCM arg6)
{
  #define H_make_filter "(" S_make_filter " &opt-key order xcoeffs ycoeffs) returns a new direct form FIR/IIR filter"
  return(g_make_filter_1(G_FILTER, arg1, arg2, arg3, arg4, arg5, arg6));
}

static SCM g_make_fir_filter(SCM arg1, SCM arg2, SCM arg3, SCM arg4)
{
  #define H_make_fir_filter "(" S_make_fir_filter " &opt-key order xcoeffs) returns a new FIR filter"
  return(g_make_filter_1(G_FIR_FILTER, arg1, arg2, arg3, arg4, SCM_UNDEFINED, SCM_UNDEFINED));
}

static SCM g_make_iir_filter(SCM arg1, SCM arg2, SCM arg3, SCM arg4)
{
  #define H_make_iir_filter "(" S_make_iir_filter " &opt-key order ycoeffs) returns a new IIR filter"
  return(g_make_filter_1(G_IIR_FILTER, arg1, arg2, arg3, arg4, SCM_UNDEFINED, SCM_UNDEFINED));
}

static SCM g_mus_order(SCM obj)
{
  #define H_mus_order "(" S_mus_order " gen) -> gen's filter order"
  SCM_ASSERT(MUS_SCM_P(obj), obj, SCM_ARG1, S_mus_order);
  return(TO_SMALL_SCM_INT(mus_order(TO_CLM(obj))));
}

static SCM g_mus_xcoeffs(SCM gen) 
{
  #define H_mus_xcoeffs "(" S_mus_xcoeffs " gen) -> gen's filter xcoeffs (vct of coefficients on inputs)"
  mus_scm *ms;
  SCM_ASSERT(MUS_SCM_P(gen), gen, SCM_ARG1, S_mus_data);
  ms = TO_MUS_SCM(gen);
  if (ms->vcts)
    return(ms->vcts[0]); 
  return(SCM_BOOL_F);
}

static SCM g_mus_ycoeffs(SCM gen) 
{
  #define H_mus_ycoeffs "(" S_mus_ycoeffs " gen) -> gen's filter ycoeffs (vct of coefficients on outputs)"
  mus_scm *ms;
  SCM_ASSERT(MUS_SCM_P(gen), gen, SCM_ARG1, S_mus_data);
  ms = TO_MUS_SCM(gen);
  if (ms->vcts)
    {
      if (mus_iir_filter_p(TO_CLM(gen)))  
	return(ms->vcts[0]);
      else
	{
	  if (mus_filter_p(TO_CLM(gen)))
	    return(ms->vcts[1]); 
	}
    }
  return(SCM_BOOL_F);
}

static void init_flt(void)
{
  DEFINE_PROC(S_make_filter,     g_make_filter, 0, 6, 0,     H_make_filter);
  DEFINE_PROC(S_filter,          g_filter, 2, 0, 0,          H_filter);
  DEFINE_PROC(S_filter_p,        g_filter_p, 1, 0, 0,        H_filter_p);
  DEFINE_PROC(S_make_fir_filter, g_make_fir_filter, 0, 4, 0, H_make_fir_filter);
  DEFINE_PROC(S_fir_filter,      g_fir_filter, 2, 0, 0,      H_fir_filter);
  DEFINE_PROC(S_fir_filter_p,    g_fir_filter_p, 1, 0, 0,    H_fir_filter_p);
  DEFINE_PROC(S_make_iir_filter, g_make_iir_filter, 0, 4, 0, H_make_iir_filter);
  DEFINE_PROC(S_iir_filter,      g_iir_filter, 2, 0, 0,      H_iir_filter);
  DEFINE_PROC(S_iir_filter_p,    g_iir_filter_p, 1, 0, 0,    H_iir_filter_p);
  DEFINE_PROC(S_mus_order,       g_mus_order, 1, 0, 0,       H_mus_order);
  DEFINE_PROC(S_mus_xcoeffs,     g_mus_xcoeffs, 1, 0, 0,     H_mus_xcoeffs);
  DEFINE_PROC(S_mus_ycoeffs,     g_mus_ycoeffs, 1, 0, 0,     H_mus_ycoeffs);
}



/* ---------------- env ---------------- */

#define S_env_p       "env?"
#define S_env         "env"
#define S_make_env    "make-env"
#define S_restart_env "restart-env"
#define S_env_interp  "env-interp"

static SCM g_env_p(SCM obj) 
{
  #define H_env_p "(" S_env_p " gen) -> #t if gen is a " S_env " generator, else #f"
  return(((MUS_SCM_P(obj)) && (mus_env_p(TO_CLM(obj)))) ? SCM_BOOL_T : SCM_BOOL_F);
}

static SCM g_env(SCM obj) 
{
  #define H_env "(" S_env " gen) -> next sample from envelope generator"
  SCM_ASSERT((MUS_SCM_P(obj)) && (mus_env_p(TO_CLM(obj))), obj, SCM_ARG1, S_env);
  return(TO_SCM_DOUBLE(mus_env(TO_CLM(obj))));
}

static SCM g_restart_env(SCM obj) 
{
  #define H_restart_env "(" S_restart_env " gen) restarts (sets to sample 0) envelope generator gen"
  SCM_ASSERT((MUS_SCM_P(obj)) && (mus_env_p(TO_CLM(obj))), obj, SCM_ARG1, S_restart_env);
  mus_restart_env(TO_CLM(obj));
  return(SCM_BOOL_F);
}

static SCM g_make_env(SCM arglist)
{
  #define H_make_env "(" S_make_env " &opt-key envelope (scaler 1.0) duration (offset 0.0) (base 1.0) end (start 0))\n\
returns a new envelope generator.  'envelope' is a list of break-point pairs. To create the envelope \
these points are offset by 'offset', scaled by 'scaler', mapped over the time interval defined by \
either 'duration' (seconds) or 'start' and 'end' (samples).  If 'base' is 1.0, the connecting segments \
are linear, if 0.0 you get a step function, and anything else produces an exponential connecting segment."

  mus_scm *gn;
  SCM args[14], keys[7];
  int orig_arg[7] = {0, 0, 0, 0, 0, 0, 0};
  int vals, i, len = 0, arglist_len;
  Float base = 1.0, scaler = 1.0, offset = 0.0, duration = 0.0;
  int start = 0, end = 0, npts = 0;
  Float *brkpts = NULL, *odata = NULL;
  SCM lst;
  keys[0] = all_keys[C_envelope];
  keys[1] = all_keys[C_scaler];
  keys[2] = all_keys[C_duration];
  keys[3] = all_keys[C_offset];
  keys[4] = all_keys[C_base];
  keys[5] = all_keys[C_end];
  keys[6] = all_keys[C_start];
  for (i = 0; i < 14; i++) args[i] = SCM_UNDEFINED;
  arglist_len = LIST_LENGTH(arglist);
  for (i = 0; i < arglist_len; i++) args[i] = LIST_REF(arglist, i);
  vals = decode_keywords(S_make_env, 7, keys, 14, args, orig_arg);
  if (vals > 0)
    {
      scaler = fkeyarg(keys[1], S_make_env, orig_arg[1] + 1, args[orig_arg[1]], 1.0);
      duration = fkeyarg(keys[2], S_make_env, orig_arg[2] + 1, args[orig_arg[2]], 0.0);
      offset = fkeyarg(keys[3], S_make_env, orig_arg[3] + 1, args[orig_arg[3]], 0.0);
      base = fkeyarg(keys[4], S_make_env, orig_arg[4] + 1, args[orig_arg[4]], 1.0);
      end = ikeyarg(keys[5], S_make_env, orig_arg[5] + 1, args[orig_arg[5]], 0);
      start = ikeyarg(keys[6], S_make_env, orig_arg[6] + 1, args[orig_arg[6]], 0);
      /* env data is a list, checked last to let the preceding throw wrong-type error before calloc  */
      if (!(KEYWORD_P(keys[0])))
        {
	  if (LIST_P_WITH_LENGTH(keys[0], len))
	    {
	      if (len == 0)
		mus_misc_error(S_make_env, "null env?", keys[0]);
	      npts = len/2;
	      brkpts = (Float *)CALLOC(len, sizeof(Float));
	      odata = (Float *)CALLOC(len, sizeof(Float));
	      for (i = 0, lst = keys[0]; (i < len) && (NOT_NULL_P(lst)); i++, lst = SCM_CDR(lst))
		{
		  brkpts[i] = TO_C_DOUBLE(SCM_CAR(lst));
		  odata[i] = brkpts[i];
		}
	    }
          else scm_wrong_type_arg(S_make_env, orig_arg[0] + 1, args[orig_arg[0]]);
        }
    }
  if (brkpts == NULL) 
    mus_misc_error(S_make_env, "no envelope?", SCM_EOL);
  if ((end < 0) || (base < 0.0) || (duration < 0.0) || (start < 0))
    {
      if (brkpts) FREE(brkpts);
      if (odata) FREE(odata);
      if (end < 0) mus_misc_error(S_make_env, "end < 0?", keys[5]);
      if (base < 0.0) mus_misc_error(S_make_env, "base < 0.0?", keys[4]);
      if (duration < 0.0) mus_misc_error(S_make_env, "duration < 0.0?", keys[2]);
      if (start < 0) mus_misc_error(S_make_env, "start < 0?", keys[6]);
    }
  /* odata = vct->data in this context [vcts[0]] */
  gn = (mus_scm *)CALLOC(1, sizeof(mus_scm));
  gn->vcts = (SCM *)CALLOC(1, sizeof(SCM));
  gn->vcts[0] = SCM_EOL;
  gn->nvcts = 1;
  gn->gen = mus_make_env(brkpts, npts, scaler, offset, base, duration, start, end, odata);
  FREE(brkpts);
  if (gn->gen == NULL)
    {
      FREE(gn->vcts);
      FREE(gn);
      if (odata) FREE(odata);
      return(SCM_BOOL_F);
    }
  return(mus_scm_to_smob_with_vct(gn, make_vct(len, odata)));
}

static SCM g_env_interp(SCM x, SCM env1) /* "env" causes trouble in Objective-C!! */
{
  #define H_env_interp "(" S_env_interp " gen x) -> value of envelope at x"
  SCM_ASSERT(NUMBER_P(x), x, SCM_ARG1, S_env_interp);
  SCM_ASSERT((MUS_SCM_P(env1)) && (mus_env_p(TO_CLM(env1))), env1, SCM_ARG2, S_env_interp);
  return(TO_SCM_DOUBLE(mus_env_interp(TO_C_DOUBLE(x), TO_CLM(env1))));
}

static void init_env(void)
{
  DEFINE_PROC(S_env_p,       g_env_p, 1, 0, 0,       H_env_p);
  DEFINE_PROC(S_env,         g_env, 1, 0, 0,         H_env);
  DEFINE_PROC(S_restart_env, g_restart_env, 1, 0, 0, H_restart_env);
  DEFINE_PROC(S_make_env,    g_make_env, 0, 0, 1,    H_make_env);
  DEFINE_PROC(S_env_interp,  g_env_interp, 2, 0, 0,  H_env_interp);
}


/* ---------------- io ---------------- */

#define S_file2sample      "file->sample"
#define S_file2sample_p    "file->sample?"
#define S_make_file2sample "make-file->sample"
#define S_sample2file      "sample->file"
#define S_sample2file_p    "sample->file?"
#define S_make_sample2file "make-sample->file"
#define S_file2frame       "file->frame"
#define S_file2frame_p     "file->frame?"
#define S_make_file2frame  "make-file->frame"
#define S_frame2file       "frame->file"
#define S_frame2file_p     "frame->file?"
#define S_make_frame2file  "make-frame->file"
#define S_mus_input_p      "mus-input?"
#define S_mus_output_p     "mus-output?"
#define S_in_any           "in-any"
#define S_out_any          "out-any"
#define S_ina              "ina"
#define S_inb              "inb"
#define S_outa             "outa"
#define S_outb             "outb"
#define S_outc             "outc"
#define S_outd             "outd"
#define S_file2array       "file->array"
#define S_array2file       "array->file"
#define S_mus_close        "mus-close"
#define S_mus_file_buffer_size "mus-file-buffer-size"

static SCM g_input_p(SCM obj) 
{
  #define H_mus_input_p "(" S_mus_input_p " gen) -> #t if gen is an input generator, else #f"
  return(((MUS_SCM_P(obj)) && (mus_input_p(TO_CLM(obj)))) ? SCM_BOOL_T : SCM_BOOL_F);
}

static SCM g_output_p(SCM obj) 
{
  #define H_mus_output_p "(" S_mus_output_p " gen) -> #t if gen is an output generator, else #f"
  return(((MUS_SCM_P(obj)) && (mus_output_p(TO_CLM(obj)))) ? SCM_BOOL_T : SCM_BOOL_F);
}

static SCM g_file2sample_p(SCM obj) 
{
  #define H_file2sample_p "(" S_file2sample_p " gen) -> #t if gen is a " S_file2sample " generator, else #f"
  return(((MUS_SCM_P(obj)) && (mus_file2sample_p(TO_CLM(obj)))) ? SCM_BOOL_T : SCM_BOOL_F);
}

static SCM g_file2frame_p(SCM obj) 
{
  #define H_file2frame_p "(" S_file2frame_p " gen) -> #t if gen is a " S_file2frame " generator, else #f"
  return(((MUS_SCM_P(obj)) && (mus_file2frame_p(TO_CLM(obj)))) ? SCM_BOOL_T : SCM_BOOL_F);
}

static SCM g_sample2file_p(SCM obj) 
{
  #define H_sample2file_p "(" S_sample2file_p " gen) -> #t if gen is a " S_sample2file " generator, else #f"
  return(((MUS_SCM_P(obj)) && (mus_sample2file_p(TO_CLM(obj)))) ? SCM_BOOL_T : SCM_BOOL_F);
}

static SCM g_frame2file_p(SCM obj) 
{
  #define H_frame2file_p "(" S_frame2file_p " gen) -> #t if gen is a " S_frame2file " generator, else #f"
  return(((MUS_SCM_P(obj)) && (mus_frame2file_p(TO_CLM(obj)))) ? SCM_BOOL_T : SCM_BOOL_F);
}

static SCM g_in_any_1(char *caller, SCM frame, SCM chan, SCM inp)
{
  SCM_ASSERT(NUMBER_P(frame), frame, SCM_ARG1, caller);
  SCM_ASSERT(INTEGER_P(chan), chan, SCM_ARG2, caller);
  SCM_ASSERT((MUS_SCM_P(inp)) && (mus_input_p(TO_CLM(inp))), inp, SCM_ARG3, caller);
  return(TO_SCM_DOUBLE(mus_in_any(TO_C_INT_OR_ELSE(frame, 0), TO_C_INT(chan), (mus_input *)TO_CLM(inp))));
}

static SCM g_in_any(SCM frame, SCM chan, SCM inp) 
{
  #define H_in_any "(" S_in_any " frame chan &optional stream) -> input stream sample at frame in channel chan"
  return(g_in_any_1(S_in_any, frame, chan, inp));
}

static SCM g_ina(SCM frame, SCM inp) 
{
  #define H_ina "(" S_ina " frame &optional stream) -> input stream sample in channel 0 at frame"
  return(g_in_any_1(S_ina, frame, TO_SMALL_SCM_INT(0), inp));
}

static SCM g_inb(SCM frame, SCM inp) 
{
  #define H_inb "(" S_inb " frame &optional stream) -> input stream sample in channel 1 at frame"
  return(g_in_any_1(S_inb, frame, TO_SMALL_SCM_INT(1), inp));
}

static SCM g_out_any_1(char *caller, SCM frame, SCM chan, SCM val, SCM outp)
{
  SCM_ASSERT(NUMBER_P(frame), frame, SCM_ARG1, caller);
  SCM_ASSERT(INTEGER_P(chan), chan, SCM_ARG2, caller);
  SCM_ASSERT(NUMBER_P(val), val, SCM_ARG3, caller);
  SCM_ASSERT((MUS_SCM_P(outp)) && (mus_output_p(TO_CLM(outp))), outp, SCM_ARG4, caller);
  return(TO_SCM_DOUBLE(mus_out_any(TO_C_INT_OR_ELSE(frame, 0),
				   TO_C_DOUBLE(val),
				   TO_C_INT(chan),
				   (mus_output *)TO_CLM(outp))));
}

static SCM g_out_any(SCM frame, SCM val, SCM chan, SCM outp)
{
  #define H_out_any "(" S_out_any " frame val chan &optional stream) adds val to output stream at frame in channel chan"
  return(g_out_any_1(S_out_any, frame, chan, val, outp));
}

static SCM g_outa(SCM frame, SCM val, SCM outp)
{
  #define H_outa "(" S_outa " frame val &optional stream) adds val to output stream at frame in channel 0"
  return(g_out_any_1(S_outa, frame, TO_SMALL_SCM_INT(0), val, outp));
}

static SCM g_outb(SCM frame, SCM val, SCM outp)
{
  #define H_outb "(" S_outb " frame val &optional stream) adds val to output stream at frame in channel 1"
  return(g_out_any_1(S_outb, frame, TO_SMALL_SCM_INT(1), val, outp));
}

static SCM g_outc(SCM frame, SCM val, SCM outp)
{
  #define H_outc "(" S_outc " frame val &optional stream) adds val to output stream at frame in channel 2"
  return(g_out_any_1(S_outc, frame, TO_SMALL_SCM_INT(2), val, outp));
}

static SCM g_outd(SCM frame, SCM val, SCM outp)
{
  #define H_outd "(" S_outd " frame val &optional stream) adds val to output stream at frame in channel 3"
  return(g_out_any_1(S_outd, frame, TO_SMALL_SCM_INT(3), val, outp));
}

static SCM g_mus_close(SCM ptr)
{
  #define H_mus_close "(" S_mus_close " fd) closes the stream (fd) opened by mus-open-read or write"
  SCM_ASSERT(MUS_SCM_P(ptr), ptr, SCM_ARG1, S_mus_close);
  return(TO_SCM_INT(mus_close_file((mus_any *)TO_CLM(ptr))));
}

static SCM g_make_file2sample(SCM name)
{
  #define H_make_file2sample "(" S_make_file2sample " filename) returns an input generator reading 'filename' (a sound file)"
  mus_scm *gn;
  SCM_ASSERT(STRING_P(name), name, SCM_ARG1, S_make_file2sample);
  if (!(mus_file_probe(TO_C_STRING(name))))
    scm_throw(NO_SUCH_FILE,
	      SCM_LIST3(TO_SCM_STRING(S_make_file2sample),
			name,
			TO_SCM_STRING(strerror(errno))));
  gn = (mus_scm *)CALLOC(1, sizeof(mus_scm));
  gn->gen = mus_make_file2sample(TO_C_STRING(name));
  gn->nvcts = 0;
  return(scm_return_first(mus_scm_to_smob(gn), name));
}

static SCM g_file2sample(SCM obj, SCM samp, SCM chan)
{
  #define H_file2sample "(" S_file2sample " obj frame chan) -> sample value in sound file read by 'obj' in channel chan at frame"
  int channel = 0;
  SCM_ASSERT((MUS_SCM_P(obj)) && (mus_input_p(TO_CLM(obj))), obj, SCM_ARG1, S_file2sample);
  SCM_ASSERT(NUMBER_P(samp), samp, SCM_ARG2, S_file2sample);
  if (BOUND_P(chan))
    {
      SCM_ASSERT(INTEGER_P(chan), chan, SCM_ARG3, S_file2sample);
      channel = TO_C_INT(chan);
    }
  return(TO_SCM_DOUBLE(mus_file2sample(TO_CLM(obj),
				       TO_C_INT_OR_ELSE(samp, 0),
				       channel)));
}

static SCM g_make_sample2file(SCM name, SCM chans, SCM out_format, SCM out_type, SCM comment)
{
  #define H_make_sample2file "(" S_make_sample2file " filename chans data-format header-type comment)\n\
returns an output generator writing the sound file 'filename' which is set up to have \
'chans' channels of 'data-format' samples with a header of 'header-type'.  The latter \
should be sndlib identifiers:\n\
   (make-sample->file \"test.snd\" 2 mus-lshort mus-riff)"

  mus_scm *gn;
  int df, ht, chns;
  SCM_ASSERT(STRING_P(name), name, SCM_ARG1, S_make_sample2file);
  SCM_ASSERT(INTEGER_P(chans), chans, SCM_ARG2, S_make_sample2file);
  SCM_ASSERT(INTEGER_P(out_format), out_format, SCM_ARG3, S_make_sample2file);
  SCM_ASSERT(INTEGER_P(out_type), out_type, SCM_ARG4, S_make_sample2file);
  df = TO_C_INT(out_format);
  if (MUS_DATA_FORMAT_OK(df))
    {
      ht = TO_C_INT(out_type);
      if (MUS_HEADER_TYPE_OK(ht))
	{
	  chns = TO_C_INT(chans);
	  if (chns > 0)
	    {
	      gn = (mus_scm *)CALLOC(1, sizeof(mus_scm));
	      gn->gen = mus_make_sample2file_with_comment(TO_C_STRING(name),
							  chns,
							  df,
							  ht,
							  (STRING_P(comment)) ? TO_C_STRING(comment) : NULL);
	      gn->nvcts = 0;
	      return(scm_return_first(mus_scm_to_smob(gn), name));
	    }
	  else mus_misc_error(S_make_sample2file, "invalid chans", chans);
	}
      else mus_misc_error(S_make_sample2file, "invalid header type", out_type);
    }
  else mus_misc_error(S_make_sample2file, "invalid data format", out_format);
  return(SCM_BOOL_F);
}

static SCM g_sample2file(SCM obj, SCM samp, SCM chan, SCM val)
{
  #define H_sample2file "(" S_sample2file " obj samp chan val) adds val to the output stream \
handled by the output generator 'obj', in channel 'chan' at frame 'samp'"

  SCM_ASSERT((MUS_SCM_P(obj)) && (mus_output_p(TO_CLM(obj))), obj, SCM_ARG1, S_sample2file);
  SCM_ASSERT(NUMBER_P(samp), samp, SCM_ARG2, S_sample2file);
  SCM_ASSERT(INTEGER_P(chan), chan, SCM_ARG3, S_sample2file);
  SCM_ASSERT(NUMBER_P(val), val, SCM_ARG4, S_sample2file);
  return(TO_SCM_DOUBLE(mus_sample2file(TO_CLM(obj),
				       TO_C_INT_OR_ELSE(samp, 0),
				       TO_C_INT(chan),
				       TO_C_DOUBLE(val))));
}

static SCM g_make_file2frame(SCM name)
{
  #define H_make_file2frame "(" S_make_file2frame " filename) returns an input generator reading 'filename' (a sound file)"
  mus_scm *gn;
  SCM_ASSERT(STRING_P(name), name, SCM_ARG1, S_make_file2frame);
  if (!(mus_file_probe(TO_C_STRING(name))))
    scm_throw(NO_SUCH_FILE,
	      SCM_LIST3(TO_SCM_STRING(S_make_file2frame),
			name,
			TO_SCM_STRING(strerror(errno))));
  gn = (mus_scm *)CALLOC(1, sizeof(mus_scm));
  gn->gen = mus_make_file2frame(TO_C_STRING(name));
  gn->nvcts = 0;
  return(scm_return_first(mus_scm_to_smob(gn), name));
}

static SCM g_file2frame(SCM obj, SCM samp, SCM outfr)
{
  #define H_file2frame "(" S_file2frame " obj samp outf) -> frame of samples at frame 'samp' in sound file read by 'obj'"
  mus_frame *res = NULL;
  SCM_ASSERT((MUS_SCM_P(obj)) && (mus_input_p(TO_CLM(obj))), obj, SCM_ARG1, S_file2frame);
  SCM_ASSERT(NUMBER_P(samp), samp, SCM_ARG2, S_file2frame);
  if ((MUS_SCM_P(outfr)) && 
      (mus_frame_p(TO_CLM(outfr)))) 
    res = (mus_frame *)TO_CLM(outfr);
  return(g_wrap_frame(mus_file2frame(TO_CLM(obj),
				     TO_C_INT_OR_ELSE(samp, 0),
				     res),
		      (res) ? DONT_FREE_FRAME : FREE_FRAME));
}

static SCM g_make_frame2file(SCM name, SCM chans, SCM out_format, SCM out_type)
{
  #define H_make_frame2file "(" S_make_frame2file " filename chans data-format header-type)\n\
returns an output generator writing the sound file 'filename' which is set up to have \
'chans' channels of 'data-format' samples with a header of 'header-type'.  The latter \
should be sndlib identifiers:\n\
   (make-frame->file \"test.snd\" 2 mus-lshort mus-riff)"

  mus_scm *gn;
  SCM_ASSERT(STRING_P(name), name, SCM_ARG1, S_make_frame2file);
  SCM_ASSERT(INTEGER_P(chans), chans, SCM_ARG2, S_make_frame2file);
  SCM_ASSERT(INTEGER_P(out_format), out_format, SCM_ARG3, S_make_frame2file);
  SCM_ASSERT(INTEGER_P(out_type), out_type, SCM_ARG4, S_make_frame2file);
  gn = (mus_scm *)CALLOC(1, sizeof(mus_scm));
  gn->gen = mus_make_frame2file(TO_C_STRING(name),
				TO_C_INT(chans),
				TO_C_INT(out_format),
				TO_C_INT(out_type));
  gn->nvcts = 0;
  return(scm_return_first(mus_scm_to_smob(gn), name));
}

static SCM g_frame2file(SCM obj, SCM samp, SCM val)
{
  #define H_frame2file "(" S_frame2file " obj samp val) adds frame 'val' to the output stream \
handled by the output generator 'obj' at frame 'samp'"

  SCM_ASSERT((MUS_SCM_P(obj)) && (mus_output_p(TO_CLM(obj))), obj, SCM_ARG1, S_frame2file);
  SCM_ASSERT(NUMBER_P(samp), samp, SCM_ARG2, S_frame2file);
  SCM_ASSERT((MUS_SCM_P(val)) && (mus_frame_p(TO_CLM(val))), val, SCM_ARG3, S_frame2file);
  return(g_wrap_frame(mus_frame2file(TO_CLM(obj),
				     TO_C_INT_OR_ELSE(samp, 0),
				     (mus_frame *)TO_CLM(val)),
		      DONT_FREE_FRAME));
}

static SCM g_array2file(SCM filename, SCM data, SCM len, SCM srate, SCM channels)
{
  #define H_array2file "(" S_array2file " filename data len srate channels) writes 'data', \
a vct object of interleaved samples to the sound file 'filename' set up to have the given \
srate and channels.  'len' samples are written."

  int olen, samps;
  vct *v;
  SCM_ASSERT(STRING_P(filename), filename, SCM_ARG1, S_array2file);
  SCM_ASSERT(VCT_P(data), data, SCM_ARG2, S_array2file);
  SCM_ASSERT(NUMBER_P(len), len, SCM_ARG3, S_array2file);
  SCM_ASSERT(NUMBER_P(srate), srate, SCM_ARG4, S_array2file);
  SCM_ASSERT(INTEGER_P(channels), channels, SCM_ARG5, S_array2file);
  v = TO_VCT(data);
  samps = TO_C_INT_OR_ELSE(len, 1);
  if (samps <= 0)
    mus_misc_error(S_array2file, "samples <= 0?", len);
  if (samps > v->length)
    samps = v->length;
  olen = mus_fltarray2file(TO_C_STRING(filename),
			   v->data,
			   samps,
			   TO_C_INT_OR_ELSE(srate, 0),
			   TO_C_INT(channels));
  return(scm_return_first(TO_SCM_INT(olen), filename));
}

static SCM g_file2array(SCM filename, SCM chan, SCM start, SCM samples, SCM data)
{
  #define H_file2array "(" S_file2array " filename chan start samples data) reads the sound file \
'filename' placing samples from channel 'chan' into the vct object 'data' starting in the file \
at frame 'start' and reading 'samples' samples altogether."

  int err, chn, samps;
  vct *v;
  char *name;
  SCM_ASSERT(STRING_P(filename), filename, SCM_ARG1, S_file2array);
  name = TO_C_STRING(filename);
  if (!(mus_file_probe(name)))
    scm_throw(NO_SUCH_FILE,
	      SCM_LIST3(TO_SCM_STRING(S_file2array),
			filename,
			TO_SCM_STRING(strerror(errno))));
  SCM_ASSERT(INTEGER_P(chan), chan, SCM_ARG2, S_file2array);
  SCM_ASSERT(NUMBER_P(start), start, SCM_ARG3, S_file2array);
  SCM_ASSERT(NUMBER_P(samples), samples, SCM_ARG4, S_file2array);
  SCM_ASSERT((VCT_P(data)), data, SCM_ARG5, S_file2array);
  v = TO_VCT(data);
  samps = TO_C_INT_OR_ELSE(samples, 1);
  if (samps <= 0) 
    mus_misc_error(S_file2array, "samples <= 0?", samples);
  chn = TO_C_INT(chan);
  if ((chn < 0) || (chn > mus_sound_chans(name)))
    scm_throw(NO_SUCH_CHANNEL,
	      SCM_LIST3(TO_SCM_STRING(S_file2array),
			TO_SCM_STRING("invalid chan"),
			chn));
  if (samps > v->length)
    samps = v->length;
  err = mus_file2fltarray(name,
			  chn,
			  TO_C_INT_OR_ELSE(start, 0),
			  samps,
			  v->data);
  return(scm_return_first(TO_SMALL_SCM_INT(err), filename));
}

static SCM g_mus_file_buffer_size(void)
{
  #define H_mus_file_buffer_size "(" S_mus_file_buffer_size ") -> current CLM IO buffer size (default is 8192)"
  return(TO_SCM_INT(mus_file_buffer_size()));
}

static SCM g_mus_set_file_buffer_size(SCM val)
{
  SCM_ASSERT(INTEGER_P(val), val, SCM_ARG1, "set-" S_mus_file_buffer_size);
  return(TO_SCM_INT(mus_set_file_buffer_size(TO_C_INT(val))));
}


static void init_io(void)
{
  DEFINE_PROC(S_file2sample_p,    g_file2sample_p, 1, 0, 0,    H_file2sample_p);
  DEFINE_PROC(S_make_file2sample, g_make_file2sample, 1, 0, 0, H_make_file2sample);
  DEFINE_PROC(S_file2sample,      g_file2sample, 2, 1, 0,      H_file2sample);
  DEFINE_PROC(S_file2frame_p,     g_file2frame_p, 1, 0, 0,     H_file2frame_p);
  DEFINE_PROC(S_make_file2frame,  g_make_file2frame, 1, 0, 0,  H_make_file2frame);
  DEFINE_PROC(S_file2frame,       g_file2frame, 2, 1, 0,       H_file2frame);
  DEFINE_PROC(S_sample2file_p,    g_sample2file_p, 1, 0, 0,    H_sample2file_p);
  DEFINE_PROC(S_make_sample2file, g_make_sample2file, 4, 1, 0, H_make_sample2file);
  DEFINE_PROC(S_sample2file,      g_sample2file, 4, 0, 0,      H_sample2file);
  DEFINE_PROC(S_frame2file_p,     g_frame2file_p, 1, 0, 0,     H_frame2file_p);
  DEFINE_PROC(S_frame2file,       g_frame2file, 3, 0, 0,       H_frame2file);
  DEFINE_PROC(S_make_frame2file,  g_make_frame2file, 4, 0, 0,  H_make_frame2file);
  DEFINE_PROC(S_mus_input_p,      g_input_p, 1, 0, 0,          H_mus_input_p);
  DEFINE_PROC(S_mus_output_p,     g_output_p, 1, 0, 0,         H_mus_output_p);
  DEFINE_PROC(S_in_any,           g_in_any, 3, 0, 0,           H_in_any);
  DEFINE_PROC(S_ina,              g_ina, 2, 0, 0,              H_ina);  
  DEFINE_PROC(S_inb,              g_inb, 2, 0, 0,              H_inb);
  DEFINE_PROC(S_out_any,          g_out_any, 4, 0, 0,          H_out_any);
  DEFINE_PROC(S_outa,             g_outa, 3, 0, 0,             H_outa);
  DEFINE_PROC(S_outb,             g_outb, 3, 0, 0,             H_outb);
  DEFINE_PROC(S_outc,             g_outc, 3, 0, 0,             H_outc);
  DEFINE_PROC(S_outd,             g_outd, 3, 0, 0,             H_outd);
  DEFINE_PROC(S_array2file,       g_array2file, 5, 0, 0,       H_array2file);
  DEFINE_PROC(S_file2array,       g_file2array, 5, 0, 0,       H_file2array);
  DEFINE_PROC(S_mus_close,        g_mus_close, 1, 0, 0,        H_mus_close);

  define_procedure_with_setter(S_mus_file_buffer_size, SCM_FNC g_mus_file_buffer_size, H_mus_file_buffer_size,
			       "set-" S_mus_file_buffer_size, SCM_FNC g_mus_set_file_buffer_size, local_doc, 0, 0, 1, 0);
}


/* ---------------- readin ---------------- */

#define S_readin            "readin"
#define S_readin_p          "readin?"
#define S_make_readin       "make-readin"
#define S_mus_increment     "mus-increment"
#define S_mus_set_increment "mus-set-increment"
#define S_mus_location      "mus-location"
#define S_mus_set_location  "mus-set-location"
#define S_mus_channel       "mus-channel"

static SCM g_readin_p(SCM obj) 
{
  #define H_readin_p "(" S_readin_p " gen) -> #t if gen is a " S_readin " generator, else #f"
  return(((MUS_SCM_P(obj)) && (mus_readin_p(TO_CLM(obj)))) ? SCM_BOOL_T : SCM_BOOL_F);
}

static SCM g_readin(SCM obj)
{
  #define H_readin "(" S_readin " gen) -> next sample from readin generator (a sound file reader)"
  SCM_ASSERT((MUS_SCM_P(obj)) && (mus_readin_p(TO_CLM(obj))), obj, SCM_ARG1, S_readin);
  return(TO_SCM_DOUBLE(mus_readin(TO_CLM(obj))));
}

static SCM g_make_readin(SCM arg1, SCM arg2, SCM arg3, SCM arg4, SCM arg5, SCM arg6, SCM arg7, SCM arg8)
{
  #define H_make_readin "(" S_make_readin " &opt-key file (channel 0) (start 0) (direction 1))\n\
returns a new readin (file input) generator reading the sound file 'file' starting at frame \
'start' in channel 'channel' and reading forward if 'direction' is not -1"

  /* optkey file channel start direction */
  mus_scm *gn;
  char *file = NULL;
  SCM args[8], keys[4];
  int orig_arg[4] = {0, 0, 0, 0};
  int vals;
  int channel = 0, start = 0, direction = 1;
  keys[0] = all_keys[C_file];
  keys[1] = all_keys[C_channel];
  keys[2] = all_keys[C_start];
  keys[3] = all_keys[C_direction];
  args[0] = arg1; args[1] = arg2; args[2] = arg3; args[3] = arg4; args[4] = arg5; args[5] = arg6; args[6] = arg7; args[7] = arg8; 
  vals = decode_keywords(S_make_readin, 4, keys, 8, args, orig_arg);
  if (vals > 0)
    {
      channel = ikeyarg(keys[1], S_make_readin, orig_arg[1] + 1, args[orig_arg[1]], channel);
      start = ikeyarg(keys[2], S_make_readin, orig_arg[2] + 1, args[orig_arg[2]], start);
      direction = ikeyarg(keys[3], S_make_readin, orig_arg[3] + 1, args[orig_arg[3]], direction);
      if (!(KEYWORD_P(keys[0])))
        {
	  if (STRING_P(keys[0]))
	    file = TO_NEW_C_STRING(keys[0]);
	  else scm_wrong_type_arg(S_make_readin, orig_arg[0] + 1, args[orig_arg[0]]);
	}
    }
  if (channel < 0)
    {
      if (file) free(file);
      mus_misc_error(S_make_readin, "channel < 0?", keys[1]);
    }
  if (!(mus_file_probe(file)))
    {
      if (file) free(file);
      scm_throw(NO_SUCH_FILE,
		SCM_LIST3(TO_SCM_STRING(S_make_readin),
			  TO_SCM_STRING(file),
			  TO_SCM_STRING(strerror(errno))));
    }
  gn = (mus_scm *)CALLOC(1, sizeof(mus_scm));
  gn->gen = mus_make_readin(file, channel, start, direction);
  if (file) free(file);
  return(mus_scm_to_smob(gn));
}

static SCM g_increment(SCM obj)
{
  #define H_mus_increment "(" S_mus_increment " gen) -> gen's " S_mus_increment " field, if any"
  SCM_ASSERT(MUS_SCM_P(obj), obj, SCM_ARG1, S_mus_increment);
  return(TO_SCM_DOUBLE(mus_increment(TO_CLM(obj))));
}

static SCM g_set_increment(SCM obj, SCM val)
{
  SCM_ASSERT(MUS_SCM_P(obj), obj, SCM_ARG1, S_mus_set_increment);
  SCM_ASSERT(NUMBER_P(val), val, SCM_ARG2, S_mus_set_increment);
  return(TO_SCM_DOUBLE(mus_set_increment(TO_CLM(obj), TO_C_DOUBLE(val))));
}

static SCM g_location(SCM obj)
{
  #define H_mus_location "(" S_mus_location " gen) -> gen's " S_mus_location " field, if any"
  SCM_ASSERT(MUS_SCM_P(obj), obj, SCM_ARG1, S_mus_location);
  return(TO_SCM_INT(mus_location(TO_CLM(obj))));
}

static SCM g_set_location(SCM obj, SCM val)
{
  SCM_ASSERT(MUS_SCM_P(obj), obj, SCM_ARG1, S_mus_set_location);
  SCM_ASSERT(NUMBER_P(val), val, SCM_ARG2, S_mus_set_location);
  return(TO_SCM_INT(mus_set_location(TO_CLM(obj), TO_C_INT_OR_ELSE(val, 0))));
}

static SCM g_channel(SCM obj)
{
  #define H_mus_channel "(" S_mus_channel " gen) -> gen's " S_mus_channel " field, if any"
  SCM_ASSERT((MUS_SCM_P(obj)) && (mus_input_p(TO_CLM(obj))), obj, SCM_ARG1, S_mus_channel);
  return(TO_SMALL_SCM_INT(mus_channel((mus_input *)TO_CLM(obj))));
}

static void init_rdin(void)
{
  DEFINE_PROC(S_readin_p,    g_readin_p, 1, 0, 0,    H_readin_p);
  DEFINE_PROC(S_readin,      g_readin, 1, 0, 0,      H_readin);
  DEFINE_PROC(S_make_readin, g_make_readin, 0, 8, 0, H_make_readin);
  DEFINE_PROC(S_mus_channel, g_channel, 1, 0, 0,     H_mus_channel);

  define_procedure_with_setter(S_mus_location, SCM_FNC g_location, H_mus_location,
			       S_mus_set_location, SCM_FNC g_set_location, local_doc, 1, 0, 2, 0);

  define_procedure_with_setter(S_mus_increment, SCM_FNC g_increment, H_mus_increment,
			       S_mus_set_increment, SCM_FNC g_set_increment, local_doc, 1, 0, 2, 0);
}


/* ---------------- locsig ---------------- */

#define S_locsig_p          "locsig?"
#define S_locsig            "locsig"
#define S_make_locsig       "make-locsig"
#define S_mus_channels      "mus-channels"
#define S_locsig_ref        "locsig-ref"
#define S_locsig_set        "locsig-set!"
#define S_locsig_reverb_ref "locsig-reverb-ref"
#define S_locsig_reverb_set "locsig-reverb-set!"

static SCM g_locsig_ref(SCM obj, SCM chan)
{
  #define H_locsig_ref "(" S_locsig_ref " gen chan) -> locsig 'gen' channel 'chan' scaler"
  SCM_ASSERT((MUS_SCM_P(obj)) && (mus_locsig_p(TO_CLM(obj))), obj, SCM_ARG1, S_locsig_ref);
  SCM_ASSERT(INTEGER_P(chan), chan, SCM_ARG2, S_locsig_ref);
  return(TO_SCM_DOUBLE(mus_locsig_ref(TO_CLM(obj), TO_C_INT(chan))));
}

static SCM g_locsig_set(SCM obj, SCM chan, SCM val)
{
  #define H_locsig_set "(" S_locsig_set " gen chan val) sets the locsig generator's channel 'chan' scaler to 'val'"
  SCM_ASSERT((MUS_SCM_P(obj)) && (mus_locsig_p(TO_CLM(obj))), obj, SCM_ARG1, S_locsig_set);
  SCM_ASSERT(INTEGER_P(chan), chan, SCM_ARG2, S_locsig_set);
  SCM_ASSERT(NUMBER_P(val), val, SCM_ARG3, S_locsig_set);
  return(TO_SCM_DOUBLE(mus_locsig_set(TO_CLM(obj),
				      TO_C_INT(chan),
				      TO_C_DOUBLE(val))));
}

static SCM g_locsig_reverb_ref(SCM obj, SCM chan)
{
  #define H_locsig_reverb_ref "(" S_locsig_reverb_ref " gen chan) -> locsig reverb channel 'chan' scaler"
  SCM_ASSERT((MUS_SCM_P(obj)) && (mus_locsig_p(TO_CLM(obj))), obj, SCM_ARG1, S_locsig_reverb_ref);
  SCM_ASSERT(INTEGER_P(chan), chan, SCM_ARG2, S_locsig_reverb_ref);
  return(TO_SCM_DOUBLE(mus_locsig_reverb_ref(TO_CLM(obj), TO_C_INT(chan))));
}

static SCM g_locsig_reverb_set(SCM obj, SCM chan, SCM val)
{
  #define H_locsig_reverb_set "(" S_locsig_reverb_set " gen chan val) sets the locsig reverb channel 'chan' scaler to 'val'"
  SCM_ASSERT((MUS_SCM_P(obj)) && (mus_locsig_p(TO_CLM(obj))), obj, SCM_ARG1, S_locsig_reverb_set);
  SCM_ASSERT(INTEGER_P(chan), chan, SCM_ARG2, S_locsig_reverb_set);
  SCM_ASSERT(NUMBER_P(val), val, SCM_ARG3, S_locsig_reverb_set);
  return(TO_SCM_DOUBLE(mus_locsig_reverb_set(TO_CLM(obj),
					     TO_C_INT(chan),
					     TO_C_DOUBLE(val))));
}

static SCM g_locsig_p(SCM obj)
{
  #define H_locsig_p "(" S_locsig_p " gen) -> #t if gen is a " S_locsig " generator, else #f"
  return(((MUS_SCM_P(obj)) && (mus_locsig_p(TO_CLM(obj)))) ? SCM_BOOL_T : SCM_BOOL_F);
}

static SCM g_locsig(SCM obj, SCM loc, SCM val)
{
  #define H_locsig "(" S_locsig " gen loc val) adds 'val' to the output of locsig at frame 'loc'"
  SCM_ASSERT((MUS_SCM_P(obj)) && (mus_locsig_p(TO_CLM(obj))), obj, SCM_ARG1, S_locsig);
  SCM_ASSERT(NUMBER_P(loc), loc, SCM_ARG2, S_locsig);
  SCM_ASSERT(NUMBER_P(val), val, SCM_ARG3, S_locsig);
  return(g_wrap_frame(mus_locsig(TO_CLM(obj),
				 TO_C_INT_OR_ELSE(loc, 0),
				 TO_C_DOUBLE(val)),
		      DONT_FREE_FRAME));
}

static SCM g_make_locsig(SCM arglist)
{
  #define H_make_locsig "(" S_make_locsig " &opt-key (degree 0.0) (distance 1.0) (reverb 0.0) output revout (channels 1))\n\
returns a new generator for signal placement in up to 4 channels.  Channel 0 corresponds to 0 degrees."

  SCM out_obj = SCM_UNDEFINED, rev_obj = SCM_UNDEFINED;
  mus_scm *gn;
  mus_output *outp = NULL, *revp = NULL;
  SCM args[12], keys[6];
  int orig_arg[6] = {0, 0, 0, 0, 0, 0};
  int vals, i, arglist_len, vlen = 0, out_chans = 1;
  Float degree = 0.0, distance = 1.0, reverb = 0.0;
  keys[0] = all_keys[C_degree];
  keys[1] = all_keys[C_distance];
  keys[2] = all_keys[C_reverb];
  keys[3] = all_keys[C_output];  
  keys[4] = all_keys[C_revout];
  keys[5] = all_keys[C_channels];
  for (i = 0; i < 12; i++) args[i] = SCM_UNDEFINED;
  arglist_len = LIST_LENGTH(arglist);
  for (i = 0; i < arglist_len; i++) args[i] = LIST_REF(arglist, i);
  vals = decode_keywords(S_make_locsig, 6, keys, 12, args, orig_arg);
  if (vals > 0)
    {
      degree = fkeyarg(keys[0], S_make_locsig, orig_arg[0] + 1, args[orig_arg[0]], degree);
      distance = fkeyarg(keys[1], S_make_locsig, orig_arg[1] + 1, args[orig_arg[1]], distance);
      reverb = fkeyarg(keys[2], S_make_locsig, orig_arg[2] + 1, args[orig_arg[2]], reverb);
      if (!(KEYWORD_P(keys[3]))) 
	{
	  if ((MUS_SCM_P(keys[3])) && (mus_output_p(TO_CLM(keys[3]))))
	    {
	      out_obj = keys[3];
	      vlen++;
	      outp = (mus_output *)TO_CLM(keys[3]);
	      out_chans = mus_channels((mus_any *)outp);
	    }
	  else 
	    {
	      if ((BOUND_P(keys[3])) && (!(BOOLEAN_P(keys[3]))))
		scm_wrong_type_arg(S_make_locsig, orig_arg[3] + 1, args[orig_arg[3]]);
	      else keys[3] = SCM_UNDEFINED;
	    }
	}
      if (!(KEYWORD_P(keys[4]))) 
	{
	  if ((MUS_SCM_P(keys[4])) && (mus_output_p(TO_CLM(keys[4]))))
	    {
	      rev_obj = keys[4];
	      vlen++;
	      revp = (mus_output *)TO_CLM(keys[4]);
	    }
	  else 
	    {
	      if ((BOUND_P(keys[4])) && (!(BOOLEAN_P(keys[4]))))
		scm_wrong_type_arg(S_make_locsig, orig_arg[4] + 1, args[orig_arg[4]]);
	      else keys[4] = SCM_UNDEFINED;
	    }
	}
      out_chans = ikeyarg(keys[5], S_make_locsig, orig_arg[5] + 1, args[orig_arg[5]], out_chans);
    }
  gn = (mus_scm *)CALLOC(1, sizeof(mus_scm));
  if (vlen > 0)
    {
      gn->vcts = (SCM *)CALLOC(vlen, sizeof(SCM));
      i = 0;
      if (BOUND_P(out_obj)) gn->vcts[i++] = out_obj;
      if (BOUND_P(rev_obj)) gn->vcts[i] = rev_obj;
      gn->nvcts = vlen;
    }
  gn->gen = mus_make_locsig(degree, distance, reverb, out_chans, outp, revp);
  return(mus_scm_to_smob(gn));
}

static SCM g_channels(SCM obj)
{
  #define H_mus_channels "(" S_mus_channels " gen) -> gen's " S_mus_channels " field, if any"
  SCM_ASSERT(MUS_SCM_P(obj), obj, SCM_ARG1, S_mus_channels);
  return(TO_SMALL_SCM_INT(mus_channels(TO_CLM(obj))));
}

static void init_locs(void)
{
  DEFINE_PROC(S_locsig_p,          g_locsig_p, 1, 0, 0,          H_locsig_p);
  DEFINE_PROC(S_locsig,            g_locsig, 3, 0, 0,            H_locsig);
  DEFINE_PROC(S_make_locsig,       g_make_locsig, 0, 0, 1,       H_make_locsig);
  DEFINE_PROC(S_mus_channels,      g_channels, 1, 0, 0,          H_mus_channels);
  DEFINE_PROC(S_locsig_ref,        g_locsig_ref, 2, 0, 0,        H_locsig_ref);
  DEFINE_PROC(S_locsig_reverb_ref, g_locsig_reverb_ref, 2, 0, 0, H_locsig_reverb_ref);
  DEFINE_PROC(S_locsig_set,        g_locsig_set, 3, 0, 0,        H_locsig_set);
  DEFINE_PROC(S_locsig_reverb_set, g_locsig_reverb_set, 3, 0, 0, H_locsig_reverb_set);
}


/* ---------------- src ---------------- */

enum {INPUT_FUNCTION, ANALYZE_FUNCTION, EDIT_FUNCTION, SYNTHESIZE_FUNCTION, SELF_WRAPPER};

static Float funcall1 (void *ptr, int direction) /* intended for "as-needed" input funcs */
{
  /* if this is called, it's a callback from C, where ptr is a mus_scm object whose vcts[0]
   * field is an SCM procedure to be called, the result being returned back to C.  In the
   * Scheme world, it's a procedure of one arg, the current read direction: 
   *
   * funcall1 is input-func for clm.c make args, or the 2nd arg to the gen (mus_src(gen, input))
   *    it is called in C (*input)(environ, dir)
   *    environ in mus_scm *gn
   *      its gn->vcts array [INPUT_FUNCTION] = scm procedure object (if any) else SCM_EOL
   *      this is set in the gen call if it's passed there, else in the make-gen call
   * so we get here via *funcall1(gn, dir)
   *   and make sure gn->vcts[INPUT_FUNCTION] is a procedure, call it with dir as its arg,
   *   it returns a float which we then return to C
   */
  mus_scm *gn = (mus_scm *)ptr;
  if ((gn) && (gn->vcts) && (gn->vcts[INPUT_FUNCTION]) && (PROCEDURE_P(gn->vcts[INPUT_FUNCTION])))
    /* the gh_procedure_p call can be confused by 0 -> segfault! */
    return(TO_C_DOUBLE(CALL1(gn->vcts[INPUT_FUNCTION], TO_SMALL_SCM_INT(direction), "as-needed-input")));
  else return(0.0);
}

#define S_src         "src"
#define S_src_p       "src?"
#define S_make_src    "make-src"
#define S_clear_sincs "clear-sincs"

static SCM g_clear_sincs(void)
{
  mus_clear_sinc_tables();
  return(SCM_BOOL_F);
}

static SCM g_src_p(SCM obj) 
{
  #define H_src_p "(" S_src_p " gen) -> #t if gen is an " S_src " generator, else #f"
  return(((MUS_SCM_P(obj)) && (mus_src_p(TO_CLM(obj)))) ? SCM_BOOL_T : SCM_BOOL_F);
}

static SCM g_src(SCM obj, SCM pm, SCM func) 
{
  #define H_src "(" S_src " gen &optional (pm 0.0) input-function) -> next sampling rate conversion sample. \
'pm' can be used to change the sampling rate on a sample-by-sample basis.  'input-function' \
is a function of one argument (the current input direction, normally ignored) that is called \
internally whenever a new sample of input data is needed.  If the associated " S_make_src " \
included an 'input' argument, input-function is ignored."

  Float pm1 = 0.0;
  mus_scm *gn;
  SCM_ASSERT((MUS_SCM_P(obj)) && (mus_src_p(TO_CLM(obj))), obj, SCM_ARG1, S_src);
  gn = TO_MUS_SCM(obj);
  if (NUMBER_P(pm)) pm1 = TO_C_DOUBLE(pm); else if (BOUND_P(pm)) scm_wrong_type_arg(S_src, 2, pm);
  if (procedure_fits(func, 1)) gn->vcts[INPUT_FUNCTION] = func;
  return(TO_SCM_DOUBLE(mus_src(TO_CLM(obj), pm1, 0)));
}

static SCM g_make_src(SCM arg1, SCM arg2, SCM arg3, SCM arg4, SCM arg5, SCM arg6)
{
  #define H_make_src "(" S_make_src " &opt-key input (srate 1.0) (width 10))\n\
returns a new sampling-rate conversion generator (using 'warped sinc interpolation'). \
'srate' is the ratio between the new rate and the old. 'width' is the sine \
width (effectively the steepness of the low-pass filter), normally between 10 and 100. \
'input' if given is an open file stream."

  SCM in_obj = SCM_UNDEFINED;
  mus_scm *gn;
  int vals, wid = 0; /* 0 here picks up the current default width in clm.c */
  SCM args[6], keys[3];
  int orig_arg[3] = {0, 0, 0};
  Float srate = 1.0;
  keys[0] = all_keys[C_input];
  keys[1] = all_keys[C_srate];
  keys[2] = all_keys[C_width];
  args[0] = arg1; args[1] = arg2; args[2] = arg3; args[3] = arg4; args[4] = arg5; args[5] = arg6;
  vals = decode_keywords(S_make_src, 3, keys, 6, args, orig_arg);
  if (vals > 0)
    {
      if (!(KEYWORD_P(keys[0]))) 
	{
	  if (procedure_fits(keys[0], 1))
	    in_obj = keys[0];
	  else scm_wrong_type_arg(S_make_src, orig_arg[0] + 1, args[orig_arg[0]]);
	}
      srate = fkeyarg(keys[1], S_make_src, orig_arg[1] + 1, args[orig_arg[1]], srate);
      wid = ikeyarg(keys[2], S_make_src, orig_arg[2] + 1, args[orig_arg[2]], wid);
    }
  if (srate <= 0) mus_misc_error(S_make_src, "srate <= 0.0?", keys[1]);
  if (wid < 0) mus_misc_error(S_make_src, "width < 0?", keys[2]);
  gn = (mus_scm *)CALLOC(1, sizeof(mus_scm));
  gn->vcts = (SCM *)CALLOC(1, sizeof(SCM));
  if (BOUND_P(in_obj)) gn->vcts[INPUT_FUNCTION] = in_obj; else gn->vcts[INPUT_FUNCTION] = SCM_EOL;
  gn->nvcts = 1;
  gn->gen = mus_make_src(funcall1, srate, wid, gn);
  return(mus_scm_to_smob(gn));
}

static void init_sr(void)
{
  DEFINE_PROC(S_clear_sincs, g_clear_sincs, 0, 0, 0, "clears out any sinc tables");
  DEFINE_PROC(S_src_p,       g_src_p, 1, 0, 0,       H_src_p);
  DEFINE_PROC(S_src,         g_src, 1, 2, 0,         H_src);
  DEFINE_PROC(S_make_src,    g_make_src, 0, 6, 0,    H_make_src);
}


/* ---------------- granulate ---------------- */

#define S_granulate_p    "granulate?"
#define S_granulate      "granulate"
#define S_make_granulate "make-granulate"
#define S_mus_ramp       "mus-ramp"
#define S_mus_set_ramp   "mus-set-ramp"
#define S_mus_hop        "mus-hop"
#define S_mus_set_hop    "mus-set-hop"

static SCM g_granulate_p(SCM obj) 
{
  #define H_granulate_p "(" S_granulate_p " gen) -> #t if gen is a " S_granulate " generator, else #f"
  return(((MUS_SCM_P(obj)) && (mus_granulate_p(TO_CLM(obj)))) ? SCM_BOOL_T : SCM_BOOL_F);
}

static SCM g_granulate(SCM obj, SCM func) 
{
  #define H_granulate "(" S_granulate " gen) -> next sample from granular synthesis generator"
  mus_scm *gn;
  SCM_ASSERT((MUS_SCM_P(obj)) && (mus_granulate_p(TO_CLM(obj))), obj, SCM_ARG1, S_granulate);
  gn = TO_MUS_SCM(obj);
  if (procedure_fits(func, 1)) gn->vcts[INPUT_FUNCTION] = func;
  return(TO_SCM_DOUBLE(mus_granulate(TO_CLM(obj), 0)));
}

static SCM g_ramp(SCM obj)
{
  #define H_mus_ramp "(" S_mus_ramp " gen) -> (granulate generator) gen's " S_mus_ramp " field"
  SCM_ASSERT((MUS_SCM_P(obj)) && (mus_granulate_p(TO_CLM(obj))), obj, SCM_ARG1, S_mus_ramp);
  return(TO_SCM_INT(mus_ramp(TO_CLM(obj))));
}

static SCM g_set_ramp(SCM obj, SCM val)
{
  SCM_ASSERT((MUS_SCM_P(obj)) && (mus_granulate_p(TO_CLM(obj))), obj, SCM_ARG1, S_mus_set_ramp);
  SCM_ASSERT(NUMBER_P(val), val, SCM_ARG2, S_mus_set_ramp);
  return(TO_SCM_INT(mus_set_ramp(TO_CLM(obj), TO_C_INT_OR_ELSE(val, 0))));
}

static SCM g_make_granulate(SCM arglist)
{
  #define H_make_granulate "(" S_make_granulate " &opt-key input (expansion 1.0) (length .15)\n\
    (scaler .6) (hop .05) (ramp .4) (jitter 1.0) max-size)\n\
returns a new granular synthesis generator.  'length' is the grain length (seconds), 'expansion' is the ratio in timing \
between the new and old (expansion > 1.0 slows things down), 'scaler' scales the grains \
to avoid overflows, 'hop' is the spacing (seconds) between successive grains upon output \
jitter controls the randomness in that spacing, input can be a file pointer."

  SCM in_obj = SCM_UNDEFINED;
  mus_scm *gn;
  SCM args[16], keys[8];
  int orig_arg[8] = {0, 0, 0, 0, 0, 0, 0, 0};
  int vals, i, arglist_len, maxsize = 0;
  Float expansion = 1.0, segment_length = .15, segment_scaler = .6, ramp_time = .4, output_hop = .05;
  Float jitter = 1.0;
  keys[0] = all_keys[C_input];
  keys[1] = all_keys[C_expansion];
  keys[2] = all_keys[C_length];
  keys[3] = all_keys[C_scaler];
  keys[4] = all_keys[C_hop];
  keys[5] = all_keys[C_ramp];
  keys[6] = all_keys[C_jitter];
  keys[7] = all_keys[C_max_size];
  for (i = 0; i < 16; i++) args[i] = SCM_UNDEFINED;
  arglist_len = LIST_LENGTH(arglist);
  for (i = 0; i < arglist_len; i++) args[i] = LIST_REF(arglist, i);
  vals = decode_keywords(S_make_granulate, 8, keys, 16, args, orig_arg);
  if (vals > 0)
    {
      if (!(KEYWORD_P(keys[0]))) 
	{
	  if (procedure_fits(keys[0], 1))
	    in_obj = keys[0];
	  else scm_wrong_type_arg(S_make_granulate, orig_arg[0] + 1, args[orig_arg[0]]);
	}
      expansion = fkeyarg(keys[1], S_make_granulate, orig_arg[1] + 1, args[orig_arg[1]], expansion);
      segment_length = fkeyarg(keys[2], S_make_granulate, orig_arg[2] + 1, args[orig_arg[2]], segment_length);
      segment_scaler = fkeyarg(keys[3], S_make_granulate, orig_arg[3] + 1, args[orig_arg[3]], segment_scaler);
      output_hop = fkeyarg(keys[4], S_make_granulate, orig_arg[4] + 1, args[orig_arg[4]], output_hop);
      ramp_time = fkeyarg(keys[5], S_make_granulate, orig_arg[5] + 1, args[orig_arg[5]], ramp_time);
      jitter = fkeyarg(keys[6], S_make_granulate, orig_arg[6] + 1, args[orig_arg[6]], jitter);
      maxsize = ikeyarg(keys[7], S_make_granulate, orig_arg[7] + 1, args[orig_arg[7]], maxsize);
    }
  if (expansion <= 0.0) mus_misc_error(S_make_granulate, "expansion < 0.0?", keys[1]);
  if (segment_length <= 0.0) mus_misc_error(S_make_granulate, "segment-length <= 0.0?", keys[2]);
  if (segment_scaler == 0.0) mus_misc_error(S_make_granulate, "segment-scaler: 0.0?", keys[3]);
  if (output_hop < 0.0) mus_misc_error(S_make_granulate, "hop < 0?", keys[4]);
  if ((segment_length + output_hop) > 60.0) /* multiplied by srate in mus_make_granulate in array allocation */
    mus_misc_error(S_make_granulate, "segment_length + output_hop too large!", SCM_LIST2(keys[2], keys[4]));
  gn = (mus_scm *)CALLOC(1, sizeof(mus_scm));
  gn->vcts = (SCM *)CALLOC(1, sizeof(SCM));
  if (BOUND_P(in_obj)) gn->vcts[INPUT_FUNCTION] = in_obj; else gn->vcts[INPUT_FUNCTION] = SCM_EOL;
  gn->nvcts = 1;
  gn->gen = mus_make_granulate(funcall1, expansion, segment_length, segment_scaler, output_hop, ramp_time, jitter, maxsize, gn);
  return(mus_scm_to_smob(gn));
}

static void init_spd(void)
{
  DEFINE_PROC(S_granulate_p,    g_granulate_p, 1, 0, 0,    H_granulate_p);
  DEFINE_PROC(S_granulate,      g_granulate, 1, 1, 0,      H_granulate);
  DEFINE_PROC(S_make_granulate, g_make_granulate, 0, 0, 1, H_make_granulate);

  define_procedure_with_setter(S_mus_ramp, SCM_FNC g_ramp, H_mus_ramp,
			       S_mus_set_ramp, SCM_FNC g_set_ramp, local_doc, 1, 0, 2, 0);
}



/* ---------------- convolve ---------------- */

#define S_convolve_p     "convolve?"
#define S_convolve       "convolve"
#define S_make_convolve  "make-convolve"
#define S_convolve_files "convolve-files"

static SCM g_convolve_p(SCM obj) 
{
  #define H_convolve_p "(" S_convolve_p " gen) -> #t if gen is a " S_convolve " generator, else #f"
  return(((MUS_SCM_P(obj)) && (mus_convolve_p(TO_CLM(obj)))) ? SCM_BOOL_T : SCM_BOOL_F);
}

static SCM g_convolve(SCM obj, SCM func) 
{
  #define H_convolve_gen "(" S_convolve " gen &optional input-func) -> next sample from convolution generator"
  mus_scm *gn;
  SCM_ASSERT((MUS_SCM_P(obj)) && (mus_convolve_p(TO_CLM(obj))), obj, SCM_ARG1, S_convolve);
  gn = TO_MUS_SCM(obj);
  if (procedure_fits(func, 1)) gn->vcts[INPUT_FUNCTION] = func;
  return(TO_SCM_DOUBLE(mus_convolve(TO_CLM(obj), 0)));
}

/* filter-size? */

static SCM g_make_convolve(SCM arglist)
{
  #define H_make_convolve "(" S_make_convolve " &opt-key input filter fft-size)\n\
returns a new convolution generator which convolves its input with the impulse response 'filter'."

  mus_scm *gn;
  SCM args[6], keys[3];
  int orig_arg[3] = {0, 0, 0};
  int vals, i, arglist_len, fftlen;
  vct *filter = NULL;
  SCM filt = SCM_UNDEFINED, in_obj = SCM_UNDEFINED;
  int fft_size = 0;
  keys[0] = all_keys[C_input];
  keys[1] = all_keys[C_filter];
  keys[2] = all_keys[C_fft_size];
  for (i = 0; i < 6; i++) args[i] = SCM_UNDEFINED;
  arglist_len = LIST_LENGTH(arglist);
  for (i = 0; i < arglist_len; i++) args[i] = LIST_REF(arglist, i);
  vals = decode_keywords(S_make_convolve, 3, keys, 6, args, orig_arg);
  if (vals > 0)
    {
      if (!(KEYWORD_P(keys[0]))) 
	{
	  if (procedure_fits(keys[0], 1))
	    in_obj = keys[0];
	  else scm_wrong_type_arg(S_make_convolve, orig_arg[0] + 1, args[orig_arg[0]]);
	}
      if (!(KEYWORD_P(keys[1]))) 
	{
	  if (VCT_P(keys[1]))
	    {
	      filt = keys[1];
	      filter = TO_VCT(filt);
	    }
          else scm_wrong_type_arg(S_make_convolve, orig_arg[1] + 1, args[orig_arg[1]]);
	}
      fft_size = ikeyarg(keys[2], S_make_convolve, orig_arg[2] + 1, args[orig_arg[2]], fft_size);
    }
  if (filter == NULL)
    mus_misc_error(S_make_convolve, "no impulse (filter)?", SCM_BOOL_F);
  fftlen = (int)pow(2.0, 1 + (int)ceil(log((Float)(filter->length)) / log(2.0)));
  if (fft_size < fftlen) fft_size = fftlen;
  gn = (mus_scm *)CALLOC(1, sizeof(mus_scm));
  gn->nvcts = 2;
  gn->vcts = (SCM *)CALLOC(2, sizeof(SCM));
  if (BOUND_P(in_obj)) gn->vcts[INPUT_FUNCTION] = in_obj; else gn->vcts[INPUT_FUNCTION] = SCM_EOL;
  gn->vcts[1] = filt;
  gn->gen = mus_make_convolve(funcall1, filter->data, fft_size, filter->length, gn);
  return(mus_scm_to_smob(gn));
}

static SCM g_convolve_files(SCM file1, SCM file2, SCM maxamp, SCM outfile)
{
  #define H_convolve_files "(" S_convolve_files " file1 file2 maxamp output-file) convolves \
file1 and file2 writing outfile after scaling the convolution result to maxamp."

  char *f1, *f2, *f3;
  Float maxval = 1.0;
  SCM_ASSERT(STRING_P(file1), file1, SCM_ARG1, S_convolve_files);
  SCM_ASSERT(STRING_P(file2), file2, SCM_ARG2, S_convolve_files);
  SCM_ASSERT(NUMBER_IF_BOUND_P(maxamp), maxamp, SCM_ARG3, S_convolve_files);
  SCM_ASSERT((NOT_BOUND_P(outfile)) || (STRING_P(outfile)), outfile, SCM_ARG4, S_convolve_files);
  f1 = TO_NEW_C_STRING(file1);
  f2 = TO_NEW_C_STRING(file2);
  if (STRING_P(outfile)) f3 = TO_NEW_C_STRING(outfile); else f3 = "tmp.snd";
  if (NUMBER_P(maxamp)) maxval = TO_C_DOUBLE(maxamp);
  mus_convolve_files(f1, f2, maxval, f3);
  free(f1);
  free(f2);
  if (STRING_P(outfile)) free(f3);
  return(SCM_BOOL_F);
}

static void init_conv(void)
{
  DEFINE_PROC(S_convolve_p,     g_convolve_p, 1, 0, 0,     H_convolve_p);
  DEFINE_PROC(S_convolve,       g_convolve, 1, 1, 0,       H_convolve_gen);
  DEFINE_PROC(S_make_convolve,  g_make_convolve, 0, 0, 1,  H_make_convolve);
  DEFINE_PROC(S_convolve_files, g_convolve_files, 2, 2, 0, H_convolve_files);
}


/* ---------------- phase-vocoder ---------------- */

/* pvedit pvanalyze pvsynthesize:
 * these three functions provide a path for the call (clm.c) (*(pv->edit))(pv->environ)
 *   which is calling a user-supplied edit function within the particular phase-vocoder
 *   generator's context.  "environ" is an uninterpreted void pointer passed in by the
 *   user, and passed here as the edit function argument.  In this file, pv->edit is
 *   &pvedit, and (void *)ptr is environ; in make_phase_vocoder we set environ to be
 *   the mus_scm object that shadows the phase-vocoder generator, with two special
 *   pointers in the vcts field: vcts[EDIT_FUNCTION] is the (Scheme-side) function
 *   passed by the user, and vcts[SELF_WRAPPER] is a pointer to the (Scheme-relevant)
 *   smob that packages the mus_scm pointer for Scheme.  This way, the user's
 *    (make-phase-vocoder ... (lambda (v) (mus-length v)) ...)
 *   treats v as the current pv gen, vcts[SELF_WRAPPER] = v, vcts[EDIT_FUNCTION] = 
 *   the lambda form, mus_scm obj->gen is the C-side pv struct pointer.  See above
 *   under funcall1 for more verbiage.  (All this complication arises because clm.c
 *   is pure C -- no notion that Scheme might be the caller, and the user's pv.scm
 *   or whatever is pure Scheme -- no notion that C is actually doing the work,
 *   and we have to tie everything together here including the Scheme-C-Scheme-C 
 *   call chains).
 */

static int pvedit (void *ptr)
{
  mus_scm *gn = (mus_scm *)ptr;
  if ((gn) && (gn->vcts) && (gn->vcts[EDIT_FUNCTION]) && (PROCEDURE_P(gn->vcts[EDIT_FUNCTION])))
    return(TO_C_BOOLEAN(CALL1(gn->vcts[EDIT_FUNCTION], gn->vcts[SELF_WRAPPER], __FUNCTION__)));
  return(0);
}

static Float pvsynthesize (void *ptr)
{
  mus_scm *gn = (mus_scm *)ptr;
  if ((gn) && (gn->vcts) && (gn->vcts[SYNTHESIZE_FUNCTION]) && (PROCEDURE_P(gn->vcts[SYNTHESIZE_FUNCTION])))
    return(TO_C_DOUBLE(CALL1(gn->vcts[SYNTHESIZE_FUNCTION], gn->vcts[SELF_WRAPPER], __FUNCTION__)));
  return(0.0);
}

static int pvanalyze (void *ptr, Float (*input)(void *arg1, int direction))
{
  mus_scm *gn = (mus_scm *)ptr;
  if ((gn) && (gn->vcts) && (gn->vcts[SYNTHESIZE_FUNCTION]) && (PROCEDURE_P(gn->vcts[SYNTHESIZE_FUNCTION])))
    /* we can only get input func if it's already set up by the outer gen call, so (?) we can use that function here */
    return(TO_C_BOOLEAN(CALL2(gn->vcts[SYNTHESIZE_FUNCTION], gn->vcts[SELF_WRAPPER], gn->vcts[INPUT_FUNCTION], __FUNCTION__)));
  return(0);
}

#define S_phase_vocoder       "phase-vocoder"
#define S_phase_vocoder_p     "phase-vocoder?"
#define S_make_phase_vocoder  "make-phase-vocoder"

static SCM g_phase_vocoder_p(SCM obj) 
{
  #define H_phase_vocoder_p "(" S_phase_vocoder_p " gen) -> #t if gen is an " S_phase_vocoder " generator, else #f"
  return(((MUS_SCM_P(obj)) && (mus_phase_vocoder_p(TO_CLM(obj)))) ? SCM_BOOL_T : SCM_BOOL_F);
}

static SCM g_phase_vocoder(SCM obj, SCM func) 
{
  #define H_phase_vocoder "(" S_phase_vocoder " gen &optional input-function) -> phase vocoder"
  mus_scm *gn;
  SCM_ASSERT((MUS_SCM_P(obj)) && (mus_phase_vocoder_p(TO_CLM(obj))), obj, SCM_ARG1, S_phase_vocoder);
  gn = TO_MUS_SCM(obj);
  if (procedure_fits(func, 1)) gn->vcts[INPUT_FUNCTION] = func;
  return(TO_SCM_DOUBLE(mus_phase_vocoder(TO_CLM(obj), 0)));
}

static SCM g_make_phase_vocoder(SCM arglist)
{
  #define H_make_phase_vocoder "(" S_make_phase_vocoder " &opt-key input fft-size overlap interp pitch analyze edit synthesize)\n\
returns a new phase-vocoder generator; input is the input function (if can be set at run-time), analyze, edit, \
and synthesize are either #f or functions that replace the default innards of the generator, fft-size, overlap \
and interp set the fftsize, the amount of overlap between ffts, and the time between new analysis calls."

  SCM in_obj = SCM_UNDEFINED, edit_obj = SCM_UNDEFINED, synthesize_obj = SCM_UNDEFINED, analyze_obj = SCM_UNDEFINED;
  mus_scm *gn;
  SCM args[16], keys[8];
  SCM pv_obj;
  int orig_arg[8] = {0, 0, 0, 0, 0, 0, 0, 0};
  int vals, arglist_len, i;
  int fft_size = 512, overlap = 4, interp = 128;
  Float pitch = 1.0;
  keys[0] = all_keys[C_input];
  keys[1] = all_keys[C_fft_size];
  keys[2] = all_keys[C_overlap];
  keys[3] = all_keys[C_interp];
  keys[4] = all_keys[C_pitch];
  keys[5] = all_keys[C_analyze];
  keys[6] = all_keys[C_edit];
  keys[7] = all_keys[C_synthesize];
  for (i = 0; i < 16; i++) args[i] = SCM_UNDEFINED;
  arglist_len = LIST_LENGTH(arglist);
  for (i = 0; i < arglist_len; i++) args[i] = LIST_REF(arglist, i);
  vals = decode_keywords(S_make_phase_vocoder, 8, keys, 16, args, orig_arg);
  if (vals > 0)
    {
      if (procedure_fits(keys[0], 1)) in_obj = keys[0];
      fft_size = ikeyarg(keys[1], S_make_phase_vocoder, orig_arg[1] + 1, args[orig_arg[1]], fft_size);
      overlap = ikeyarg(keys[2], S_make_phase_vocoder, orig_arg[2] + 1, args[orig_arg[2]], overlap);
      interp = ikeyarg(keys[3], S_make_phase_vocoder, orig_arg[3] + 1, args[orig_arg[3]], interp);
      pitch = fkeyarg(keys[4], S_make_phase_vocoder, orig_arg[4] + 1, args[orig_arg[4]], pitch);
      if (procedure_fits(keys[5], 1)) analyze_obj = keys[5];
      if (procedure_fits(keys[6], 1)) edit_obj = keys[6];
      if (procedure_fits(keys[7], 2)) synthesize_obj = keys[7];
    }
  gn = (mus_scm *)CALLOC(1, sizeof(mus_scm));
  gn->nvcts = 5;
  gn->vcts = (SCM *)CALLOC(gn->nvcts, sizeof(SCM));
  if (BOUND_P(in_obj)) gn->vcts[INPUT_FUNCTION] = in_obj; else gn->vcts[INPUT_FUNCTION] = SCM_EOL;
  if (BOUND_P(edit_obj)) gn->vcts[EDIT_FUNCTION] = edit_obj; else gn->vcts[EDIT_FUNCTION] = SCM_EOL;
  if (BOUND_P(analyze_obj)) gn->vcts[ANALYZE_FUNCTION] = analyze_obj; else gn->vcts[ANALYZE_FUNCTION] = SCM_EOL;
  if (BOUND_P(synthesize_obj)) gn->vcts[SYNTHESIZE_FUNCTION] = synthesize_obj; else gn->vcts[SYNTHESIZE_FUNCTION] = SCM_EOL;
  gn->gen = mus_make_phase_vocoder(funcall1,
				   fft_size, overlap, interp, pitch,
				   (NOT_BOUND_P(analyze_obj) ? NULL : pvanalyze),
				   (NOT_BOUND_P(edit_obj) ? NULL : pvedit),
				   (NOT_BOUND_P(synthesize_obj) ? NULL : pvsynthesize),
				   (void *)gn);
  pv_obj = mus_scm_to_smob(gn);
  /* need scheme-relative backpointer for possible function calls */
  gn->vcts[SELF_WRAPPER] = pv_obj;
  return(pv_obj);
}

/* these names are all wrong!! */
static SCM g_pv_amps(SCM pv, SCM ind) 
{
  Float *amps; 
  mus_scm *gn;
  SCM_ASSERT((MUS_SCM_P(pv)) && (mus_phase_vocoder_p(TO_CLM(pv))), pv, SCM_ARG1, __FUNCTION__);
  SCM_ASSERT(INTEGER_P(ind), ind, SCM_ARG2, __FUNCTION__);
  gn = TO_MUS_SCM(pv);
  amps = mus_phase_vocoder_amps((void *)(gn->gen)); 
  return(TO_SCM_DOUBLE(amps[TO_SMALL_C_INT(ind)]));
}

static SCM g_set_pv_amps(SCM pv, SCM ind, SCM val) 
{
  Float *amps; 
  mus_scm *gn;
  SCM_ASSERT((MUS_SCM_P(pv)) && (mus_phase_vocoder_p(TO_CLM(pv))), pv, SCM_ARG1, __FUNCTION__);
  SCM_ASSERT(INTEGER_P(ind), ind, SCM_ARG2, __FUNCTION__);
  SCM_ASSERT(NUMBER_P(val), val, SCM_ARG3, __FUNCTION__);
  gn = TO_MUS_SCM(pv);
  amps = mus_phase_vocoder_amps((void *)(gn->gen)); 
  amps[TO_SMALL_C_INT(ind)] = TO_C_DOUBLE(val); 
  return(val);
}

static SCM g_pv_amps_1(SCM pv) 
{
  Float *amps; 
  int len;
  mus_scm *gn;
  SCM_ASSERT((MUS_SCM_P(pv)) && (mus_phase_vocoder_p(TO_CLM(pv))), pv, SCM_ARG1, __FUNCTION__);
  gn = TO_MUS_SCM(pv);
  amps = mus_phase_vocoder_amps((void *)(gn->gen)); 
  len = mus_length((mus_any *)(gn->gen));
  return(make_vct_wrapper(len / 2, amps));
}
  
static SCM g_pv_freqs(SCM pv, SCM ind) 
{
  Float *freqs; 
  mus_scm *gn;
  SCM_ASSERT((MUS_SCM_P(pv)) && (mus_phase_vocoder_p(TO_CLM(pv))), pv, SCM_ARG1, __FUNCTION__);
  SCM_ASSERT(INTEGER_P(ind), ind, SCM_ARG2, __FUNCTION__);
  gn = TO_MUS_SCM(pv);
  freqs = mus_phase_vocoder_freqs((void *)(gn->gen));
  return(TO_SCM_DOUBLE(freqs[TO_SMALL_C_INT(ind)]));
}

static SCM g_set_pv_freqs(SCM pv, SCM ind, SCM val) 
{
  Float *freqs; 
  mus_scm *gn;
  SCM_ASSERT((MUS_SCM_P(pv)) && (mus_phase_vocoder_p(TO_CLM(pv))), pv, SCM_ARG1, __FUNCTION__);
  SCM_ASSERT(INTEGER_P(ind), ind, SCM_ARG2, __FUNCTION__);
  SCM_ASSERT(NUMBER_P(val), val, SCM_ARG3, __FUNCTION__);
  gn = TO_MUS_SCM(pv);
  freqs = mus_phase_vocoder_freqs((void *)(gn->gen)); 
  freqs[TO_SMALL_C_INT(ind)] = TO_C_DOUBLE(val);
  return(val);
}

static SCM g_pv_freqs_1(SCM pv) 
{
  Float *amps; 
  int len;
  mus_scm *gn;
  SCM_ASSERT((MUS_SCM_P(pv)) && (mus_phase_vocoder_p(TO_CLM(pv))), pv, SCM_ARG1, __FUNCTION__);
  gn = TO_MUS_SCM(pv);
  amps = mus_phase_vocoder_freqs((void *)(gn->gen)); 
  len = mus_length((mus_any *)(gn->gen));
  return(make_vct_wrapper(len, amps));
}
  
static SCM g_pv_phases(SCM pv, SCM ind) 
{
  Float *phases; 
  mus_scm *gn;
  SCM_ASSERT((MUS_SCM_P(pv)) && (mus_phase_vocoder_p(TO_CLM(pv))), pv, SCM_ARG1, __FUNCTION__);
  SCM_ASSERT(INTEGER_P(ind), ind, SCM_ARG2, __FUNCTION__);
  gn = TO_MUS_SCM(pv);
  phases = mus_phase_vocoder_phases((void *)(gn->gen)); 
  return(TO_SCM_DOUBLE(phases[TO_SMALL_C_INT(ind)]));
}

static SCM g_set_pv_phases(SCM pv, SCM ind, SCM val) 
{
  Float *phases; 
  mus_scm *gn;
  SCM_ASSERT((MUS_SCM_P(pv)) && (mus_phase_vocoder_p(TO_CLM(pv))), pv, SCM_ARG1, __FUNCTION__);
  SCM_ASSERT(INTEGER_P(ind), ind, SCM_ARG2, __FUNCTION__);
  SCM_ASSERT(NUMBER_P(val), val, SCM_ARG3, __FUNCTION__);
  gn = TO_MUS_SCM(pv);
  phases = mus_phase_vocoder_phases((void *)(gn->gen)); 
  phases[TO_SMALL_C_INT(ind)] = TO_C_DOUBLE(val); 
  return(val);
}

static SCM g_pv_phases_1(SCM pv) 
{
  Float *amps; 
  int len;
  mus_scm *gn;
  SCM_ASSERT((MUS_SCM_P(pv)) && (mus_phase_vocoder_p(TO_CLM(pv))), pv, SCM_ARG1, __FUNCTION__);
  gn = TO_MUS_SCM(pv);
  amps = mus_phase_vocoder_phases((void *)(gn->gen)); 
  len = mus_length((mus_any *)(gn->gen));
  return(make_vct_wrapper(len / 2, amps));
}
  
/* temporary !?? */
static SCM g_pv_ampinc(SCM pv, SCM ind) 
{
  Float *ampinc; 
  mus_scm *gn;
  SCM_ASSERT((MUS_SCM_P(pv)) && (mus_phase_vocoder_p(TO_CLM(pv))), pv, SCM_ARG1, __FUNCTION__);
  SCM_ASSERT(INTEGER_P(ind), ind, SCM_ARG2, __FUNCTION__);
  gn = TO_MUS_SCM(pv);
  ampinc = mus_phase_vocoder_ampinc((void *)(gn->gen)); 
  return(TO_SCM_DOUBLE(ampinc[TO_SMALL_C_INT(ind)]));
}

static SCM g_set_pv_ampinc(SCM pv, SCM ind, SCM val) 
{
  Float *ampinc; 
  mus_scm *gn;
  SCM_ASSERT((MUS_SCM_P(pv)) && (mus_phase_vocoder_p(TO_CLM(pv))), pv, SCM_ARG1, __FUNCTION__);
  SCM_ASSERT(INTEGER_P(ind), ind, SCM_ARG2, __FUNCTION__);
  SCM_ASSERT(NUMBER_P(val), val, SCM_ARG3, __FUNCTION__);
  gn = TO_MUS_SCM(pv);
  ampinc = mus_phase_vocoder_ampinc((void *)(gn->gen)); 
  ampinc[TO_SMALL_C_INT(ind)] = TO_C_DOUBLE(val); 
  return(val);
}

static SCM g_pv_ampinc_1(SCM pv) 
{
  Float *amps; 
  int len;
  mus_scm *gn;
  SCM_ASSERT((MUS_SCM_P(pv)) && (mus_phase_vocoder_p(TO_CLM(pv))), pv, SCM_ARG1, __FUNCTION__);
  gn = TO_MUS_SCM(pv);
  amps = mus_phase_vocoder_ampinc((void *)(gn->gen)); 
  len = mus_length((mus_any *)(gn->gen));
  return(make_vct_wrapper(len, amps));
}
  
static SCM g_pv_phaseinc(SCM pv, SCM ind) 
{
  Float *phaseinc; 
  mus_scm *gn;
  SCM_ASSERT((MUS_SCM_P(pv)) && (mus_phase_vocoder_p(TO_CLM(pv))), pv, SCM_ARG1, __FUNCTION__);
  SCM_ASSERT(INTEGER_P(ind), ind, SCM_ARG2, __FUNCTION__);
  gn = TO_MUS_SCM(pv);
  phaseinc = mus_phase_vocoder_phaseinc((void *)(gn->gen)); 
  return(TO_SCM_DOUBLE(phaseinc[TO_SMALL_C_INT(ind)]));
}

static SCM g_set_pv_phaseinc(SCM pv, SCM ind, SCM val) 
{
  Float *phaseinc; 
  mus_scm *gn;
  SCM_ASSERT((MUS_SCM_P(pv)) && (mus_phase_vocoder_p(TO_CLM(pv))), pv, SCM_ARG1, __FUNCTION__);
  SCM_ASSERT(INTEGER_P(ind), ind, SCM_ARG2, __FUNCTION__);
  SCM_ASSERT(NUMBER_P(val), val, SCM_ARG3, __FUNCTION__);
  gn = TO_MUS_SCM(pv);
  phaseinc = mus_phase_vocoder_phaseinc((void *)(gn->gen)); 
  phaseinc[TO_SMALL_C_INT(ind)] = TO_C_DOUBLE(val); 
  return(val);
}

static SCM g_pv_phaseinc_1(SCM pv) 
{
  Float *amps; 
  int len;
  mus_scm *gn;
  SCM_ASSERT((MUS_SCM_P(pv)) && (mus_phase_vocoder_p(TO_CLM(pv))), pv, SCM_ARG1, __FUNCTION__);
  gn = TO_MUS_SCM(pv);
  amps = mus_phase_vocoder_phaseinc((void *)(gn->gen)); 
  len = mus_length((mus_any *)(gn->gen));
  return(make_vct_wrapper(len / 2, amps));
}
  
static SCM g_pv_lastphase(SCM pv, SCM ind) 
{
  Float *lastphase; 
  mus_scm *gn;
  SCM_ASSERT((MUS_SCM_P(pv)) && (mus_phase_vocoder_p(TO_CLM(pv))), pv, SCM_ARG1, __FUNCTION__);
  SCM_ASSERT(INTEGER_P(ind), ind, SCM_ARG2, __FUNCTION__);
  gn = TO_MUS_SCM(pv);
  lastphase = mus_phase_vocoder_lastphase((void *)(gn->gen)); 
  return(TO_SCM_DOUBLE(lastphase[TO_SMALL_C_INT(ind)]));
}

static SCM g_set_pv_lastphase(SCM pv, SCM ind, SCM val) 
{
  Float *lastphase; 
  mus_scm *gn;
  SCM_ASSERT((MUS_SCM_P(pv)) && (mus_phase_vocoder_p(TO_CLM(pv))), pv, SCM_ARG1, __FUNCTION__);
  SCM_ASSERT(INTEGER_P(ind), ind, SCM_ARG2, __FUNCTION__);
  SCM_ASSERT(NUMBER_P(val), val, SCM_ARG3, __FUNCTION__);
  gn = TO_MUS_SCM(pv);
  lastphase = mus_phase_vocoder_lastphase((void *)(gn->gen));
  lastphase[TO_SMALL_C_INT(ind)] = TO_C_DOUBLE(val); 
  return(val);
}

static SCM g_pv_lastphase_1(SCM pv) 
{
  Float *amps; 
  int len;
  mus_scm *gn;
  SCM_ASSERT((MUS_SCM_P(pv)) && (mus_phase_vocoder_p(TO_CLM(pv))), pv, SCM_ARG1, __FUNCTION__);
  gn = TO_MUS_SCM(pv);
  amps = mus_phase_vocoder_lastphase((void *)(gn->gen)); 
  len = mus_length((mus_any *)(gn->gen));
  return(make_vct_wrapper(len / 2, amps));
}

static SCM g_hop(SCM obj)
{
  #define H_mus_hop "(" S_mus_hop " gen) -> gen's " S_mus_hop " field"
  SCM_ASSERT(MUS_SCM_P(obj), obj, SCM_ARG1, S_mus_hop);
  return(TO_SMALL_SCM_INT(mus_hop(TO_CLM(obj))));
}

static SCM g_set_hop(SCM obj, SCM val)
{
  SCM_ASSERT(MUS_SCM_P(obj), obj, SCM_ARG1, S_mus_set_hop);
  SCM_ASSERT(NUMBER_P(val), val, SCM_ARG2, S_mus_set_hop);
  return(TO_SMALL_SCM_INT(mus_set_hop(TO_CLM(obj), TO_C_INT_OR_ELSE(val, 0))));
}



static void init_pv(void)
{
  DEFINE_PROC(S_phase_vocoder_p,    g_phase_vocoder_p, 1, 0, 0,    H_phase_vocoder_p);
  DEFINE_PROC(S_phase_vocoder,      g_phase_vocoder, 1, 1, 0,      H_phase_vocoder);
  DEFINE_PROC(S_make_phase_vocoder, g_make_phase_vocoder, 0, 0, 1, H_make_phase_vocoder);

  DEFINE_PROC("pv-ampinc", g_pv_ampinc, 2, 0, 0, "");
  DEFINE_PROC("pv-ampinc-1", g_pv_ampinc_1, 1, 0, 0, "");
  DEFINE_PROC("set-pv-ampinc", g_set_pv_ampinc, 3, 0, 0, "");
  DEFINE_PROC("pv-amps", g_pv_amps, 2, 0, 0, "");
  DEFINE_PROC("pv-amps-1", g_pv_amps_1, 1, 0, 0, "");
  DEFINE_PROC("set-pv-amps", g_set_pv_amps, 3, 0, 0, "");
  DEFINE_PROC("pv-freqs", g_pv_freqs, 2, 0, 0, "");
  DEFINE_PROC("pv-freqs-1", g_pv_freqs_1, 1, 0, 0, "");
  DEFINE_PROC("set-pv-freqs", g_set_pv_freqs, 3, 0, 0, "");
  DEFINE_PROC("pv-phases", g_pv_phases, 2, 0, 0, "");
  DEFINE_PROC("pv-phases-1", g_pv_phases_1, 1, 0, 0, "");
  DEFINE_PROC("set-pv-phases", g_set_pv_phases, 3, 0, 0, "");
  DEFINE_PROC("pv-phaseinc", g_pv_phaseinc, 2, 0, 0, "");
  DEFINE_PROC("pv-phaseinc-1", g_pv_phaseinc_1, 1, 0, 0, "");
  DEFINE_PROC("set-pv-phaseinc", g_set_pv_phaseinc, 3, 0, 0, "");
  DEFINE_PROC("pv-lastphase", g_pv_lastphase, 2, 0, 0, "");
  DEFINE_PROC("pv-lastphase-1", g_pv_lastphase_1, 1, 0, 0, "");
  DEFINE_PROC("set-pv-lastphase", g_set_pv_lastphase, 3, 0, 0, "");

  define_procedure_with_setter(S_mus_hop, SCM_FNC g_hop, H_mus_hop,
			       S_mus_set_hop, SCM_FNC g_set_hop, local_doc, 1, 0, 2, 0);
}


/* ---------------- mix ---------------- */

#define S_mus_mix "mus-mix"

static SCM g_mus_mix(SCM out, SCM in, SCM ost, SCM olen, SCM ist, SCM mx, SCM envs)
{
  #define H_mus_mix "(" S_mus_mix " outfile infile (outloc 0) frames (inloc 0) mixer envs)\n\
mixes infile into outfile starting at outloc in outfile and inloc in infile \
mixing frames frames of infile.  frames defaults to the length of infile. If mixer, \
use it to scale the various channels; if envs (an array of envelope generators), use \
it in conjunction with mixer to scale/envelope all the various ins and outs."

  mus_mixer *mx1 = NULL;
  mus_any ***envs1 = NULL;
  char *outfile = NULL, *infile = NULL;
  int in_len = 0, out_len, i, j, ostart = 0, istart = 0, osamps = 0;
  SCM *vdata0, *vdata1;
  SCM_ASSERT(STRING_P(out), out, SCM_ARG1, S_mus_mix);
  SCM_ASSERT(STRING_P(in), in, SCM_ARG2, S_mus_mix);
  SCM_ASSERT(NUMBER_IF_BOUND_P(ost), ost, SCM_ARG3, S_mus_mix);
  SCM_ASSERT(NUMBER_IF_BOUND_P(olen), olen, SCM_ARG4, S_mus_mix);
  SCM_ASSERT(NUMBER_IF_BOUND_P(ist), ist, SCM_ARG5, S_mus_mix);
  SCM_ASSERT((NOT_BOUND_P(mx)) || ((MUS_SCM_P(mx)) && (mus_mixer_p(TO_CLM(mx)))), mx, SCM_ARG6, S_mus_mix);
  SCM_ASSERT((NOT_BOUND_P(envs)) || (VECTOR_P(envs)), envs, SCM_ARG7, S_mus_mix);
  if (BOUND_P(ost)) ostart = TO_C_INT_OR_ELSE(ost, 0);
  if (BOUND_P(ist)) istart = TO_C_INT_OR_ELSE(ist, 0);
  if (BOUND_P(mx)) mx1 = (mus_mixer *)TO_CLM(mx);
  if (BOUND_P(envs))
    {
      /* pack into a C-style array of arrays of env pointers */
      in_len = VECTOR_LENGTH(envs);
      vdata0 = SCM_VELTS(envs);
      out_len = VECTOR_LENGTH(vdata0[0]);
      envs1 = (mus_any ***)CALLOC(in_len, sizeof(mus_any **));
      for (i = 0; i < in_len; i++)
	{
	  vdata1 = SCM_VELTS(vdata0[i]);
	  envs1[i] = (mus_any **)CALLOC(out_len, sizeof(mus_any *));
	  for (j = 0; j < out_len; j++) 
	    envs1[i][j] = TO_CLM(vdata1[j]);
	}
    }
  outfile = TO_NEW_C_STRING(out);
  infile = TO_NEW_C_STRING(in);
  if (BOUND_P(olen)) osamps = TO_C_INT_OR_ELSE(olen, 0); else osamps = mus_sound_frames(infile);
  mus_mix(outfile, infile, ostart, osamps, istart, mx1, envs1);
  if (outfile) free(outfile);
  if (infile) free(infile);
  if (envs1) 
    {
      for (i = 0; i < in_len; i++) if (envs1[i]) FREE(envs1[i]);
      FREE(envs1);
    }
  return(SCM_BOOL_T);
}

void init_mus2scm_module(void)
{
  local_doc = scm_permanent_object(scm_string_to_symbol(TO_SCM_STRING("documentation")));

  init_mus_module();
  init_mus_scm();
  init_keywords();
  init_simple_stuff();
  init_generic_funcs();
  init_oscil();
  init_dly();
  init_noi();
  init_cosp();
  init_tbl();
  init_sw();
  init_asyfm();
  init_smpflt();
  init_wt();
  init_rblk();
  init_frame();
  init_mixer();
  init_formant();
  init_ws();
  init_sss();
  init_flt();
  init_env();
  init_locs();
  init_io();
  init_rdin();
  init_spd();
  init_sr();
  init_conv();
  init_pv();

  DEFINE_PROC(S_mus_mix, g_mus_mix, 2, 5, 0, H_mus_mix);

  scm_add_feature("clm");
}

/*
void scm_init_sndlib_clm_module ()
{
  scm_register_module_xxx("mus clm", init_mus2scm_module);
}
*/


