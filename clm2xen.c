/* tie CLM module into Guile/Scheme or Ruby */

/* This module uses the Float array support in vct.c -- it needs to be loaded with vct.o.
 * every generator that has embedded arrays handles these through an extra layer of
 * pointers; the problem here is that we allow the caller to access and set these directly,
 * (and don't want to copy data unnecessarily), so we can easily have many pointers
 * floating around to the same C memory; there's no way at this level to set up
 * reference counters, so in C, the various free_<gen> functions check that they
 * allocated the given memory (and all vct objects are allocated elsewhere),
 * before freeing an embedded array; then here, all such arrays are wrapped up 
 * as separate XEN objects and at every level only the bottom-most reference allows 
 * the free to go forward.
 */

/* it's important to check for mus_xen_p before de-referencing a gen arg */

/*     
 *  we have mus-sound-srate in sndlib, mus-srate in clm.c, sound-srate and *clm-srate* in clm, mus-sound-srate and srate in snd
 *    perhaps a mus module, giving mus:sound-srate in xen, mus:sound-srate in clm, mus_sound_srate in C?
 */

#if defined(HAVE_CONFIG_H)
  #include <config.h>
#endif

#if USE_SND
  #include "snd.h"
#else
  #define PRINT_BUFFER_SIZE 512
  #define LABEL_BUFFER_SIZE 64
#endif

#include <stddef.h>
#include <math.h>
#include <stdio.h>
#include <errno.h>
#include <stdlib.h>
#if (!defined(HAVE_CONFIG_H)) || HAVE_STRING_H
  #include <string.h>
#endif
#include <stdarg.h>

#if (defined(HAVE_LIBC_H) && (!defined(HAVE_UNISTD_H)))
  #include <libc.h>
#else
  #ifndef _MSC_VER
    #include <unistd.h>
  #endif
#endif

#include "sndlib.h"
#include "clm.h"
#include "sndlib2xen.h"
#include "vct.h"
#include "clm2xen.h"


static int local_error_type = MUS_NO_ERROR;
static char *local_error_msg = NULL;
static mus_error_handler_t *old_error_handler;

static void local_mus_error(int type, char *msg)
{
  local_error_type = type;
  if (local_error_msg) free(local_error_msg);
  local_error_msg = strdup(msg);
}


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

static const char *keywords[NUM_KEYWORDS] = 
  {SC_frequency, SC_initial_phase, SC_wave, SC_cosines, SC_amplitude,
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
static XEN all_keys[NUM_KEYWORDS];

static void init_keywords(void)
{
  int i;
  for (i = 0; i < NUM_KEYWORDS; i++) 
    all_keys[i] = XEN_MAKE_KEYWORD((char *)(keywords[i]));
}

static int decode_keywords(char *caller, int nkeys, XEN *keys, int nargs, XEN *args, int *orig)
{
  /* implement the &optional-key notion in CLM */
  int arg_ctr = 0, key_start = 0, rtn_ctr = 0, i, keying = 0, key_found = 0;
  XEN key;
  arg_ctr = 0;
  while ((arg_ctr < nargs) && 
	 (XEN_BOUND_P(args[arg_ctr])))
    {
      if (!(XEN_KEYWORD_P(args[arg_ctr])))
	{
	  if (keying) 
	    mus_misc_error(caller, "unmatched value within keyword section?", args[arg_ctr]);
	  /* type checking on the actual values has to be the caller's problem */
	  if (arg_ctr >= nkeys)
	    mus_misc_error(caller, "extra trailing args?", args[arg_ctr]);
	  keys[arg_ctr] = args[arg_ctr];
	  orig[arg_ctr] = arg_ctr;
	  arg_ctr++;
	  key_start = arg_ctr;
	  rtn_ctr++;
	}
      else
	{
	  if ((arg_ctr == (nargs - 1)) ||
	      (!(XEN_BOUND_P(args[arg_ctr + 1]))))
	    mus_misc_error(caller, "keyword without value?", args[arg_ctr]);
	  keying = 1;
	  key = args[arg_ctr];
	  if (XEN_KEYWORD_P(args[arg_ctr + 1])) 
	    mus_misc_error(caller, "two keywords in a row?", key);
	  key_found = 0;
	  for (i = key_start; i < nkeys; i++)
	    {
	      if (XEN_KEYWORD_EQ_P(keys[i], key))
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

static Float fkeyarg (XEN key, char *caller, int n, Float def)
{
  if (!(XEN_KEYWORD_P(key)))
    {
      XEN_ASSERT_TYPE(XEN_NUMBER_P(key), key, n, caller, "a number or keyword");
      return(XEN_TO_C_DOUBLE_WITH_CALLER(key, caller));
    }
  return(def);
}

static int ikeyarg (XEN key, char *caller, int n, int def)
{
  if (!(XEN_KEYWORD_P(key)))
    {
      XEN_ASSERT_TYPE(XEN_NUMBER_P(key), key, n, caller, "a number or keyword");
      return(XEN_TO_C_INT_OR_ELSE_WITH_CALLER(key, def, caller));
    }
  return(def);
}

static off_t okeyarg (XEN key, char *caller, int n, off_t def)
{
  if (!(XEN_KEYWORD_P(key)))
    {
      XEN_ASSERT_TYPE(XEN_NUMBER_P(key), key, n, caller, "a number or keyword");
      return(XEN_TO_C_OFF_T_OR_ELSE(key, def));
    }
  return(def);
}


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
#define S_mus_set_srate          "mus-set-srate"
#define S_mus_srate              "mus-srate"
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
#define S_polar2rectangular      "polar->rectangular"
#define S_array_interp           "array-interp"
#define S_sum_of_sines           "sum-of-sines"

static char *FFT_WINDOW_CONSTANTS[17] = {S_rectangular_window, S_hann_window, S_welch_window, S_parzen_window, S_bartlett_window,
					 S_hamming_window, S_blackman2_window, S_blackman3_window, S_blackman4_window,
					 S_exponential_window, S_riemann_window, S_kaiser_window, S_cauchy_window,
					 S_poisson_window, S_gaussian_window, S_tukey_window, S_dolph_chebyshev_window
};

char *mus_fft_window_name(int i) {return(FFT_WINDOW_CONSTANTS[i]);}


static XEN g_radians2hz(XEN val) 
{
  #define H_radians_hz "(" S_radians_hz " rads) converts radians/sample to frequency in Hz: rads * srate / (2 * pi)"
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ONLY_ARG, S_radians_hz, "a number");
  return(C_TO_XEN_DOUBLE(mus_radians2hz(XEN_TO_C_DOUBLE(val))));
}

static XEN g_hz2radians(XEN val) 
{
  #define H_hz_radians "(" S_hz_radians " hz) converts frequency in Hz to radians/sample: hz * 2 * pi / srate"
  #define H_in_hz "(" S_in_hz " hz) converts frequency in Hz to radians/sample: hz*2*pi/srate"
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ONLY_ARG, S_hz_radians, "a number"); 
  return(C_TO_XEN_DOUBLE(mus_hz2radians(XEN_TO_C_DOUBLE(val))));
}

static XEN g_radians2degrees(XEN val) 
{
  #define H_radians_degrees "(" S_radians_degrees " rads) converts radians to degrees: rads * 360 / (2 * pi)"
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ONLY_ARG, S_radians_degrees, "a number"); 
  return(C_TO_XEN_DOUBLE(mus_radians2degrees(XEN_TO_C_DOUBLE(val))));
}

static XEN g_degrees2radians(XEN val) 
{
  #define H_degrees_radians "(" S_degrees_radians " deg) converts degrees to radians: deg * 2 * pi / 360"
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ONLY_ARG, S_degrees_radians, "a number"); 
  return(C_TO_XEN_DOUBLE(mus_degrees2radians(XEN_TO_C_DOUBLE(val))));
}

static XEN g_db2linear(XEN val) 
{
  #define H_db_linear "(" S_db_linear " db) converts decibel value db to linear value: pow(10, db / 20)"
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ONLY_ARG, S_db_linear, "a number");
  return(C_TO_XEN_DOUBLE(mus_db2linear(XEN_TO_C_DOUBLE(val))));
}

static XEN g_linear2db(XEN val) 
{
  #define H_linear_db "(" S_linear_db " lin) converts linear value to decibels: 20 * log10(lin)"
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ONLY_ARG, S_linear_db, "a number");
  return(C_TO_XEN_DOUBLE(mus_linear2db(XEN_TO_C_DOUBLE(val))));
}

/* can't use a variable *srate* directly here because the set! side would not communicate the change to C */
static XEN g_srate(void) 
{
  #define H_mus_srate "(" S_mus_srate ") -> current sampling rate"
  return(C_TO_XEN_DOUBLE(mus_srate()));
}

static XEN g_set_srate(XEN val) 
{
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ONLY_ARG, S_mus_set_srate, "a number");
  return(C_TO_XEN_DOUBLE(mus_set_srate(XEN_TO_C_DOUBLE(val))));
}

static XEN g_array_print_length(void) 
{
  #define H_mus_array_print_length "(" S_mus_array_print_length ") -> current clm array print length (default is 8)"
  return(C_TO_SMALL_XEN_INT(mus_array_print_length()));
}

static XEN g_set_array_print_length(XEN val) 
{
  XEN_ASSERT_TYPE(XEN_INTEGER_P(val), val, XEN_ONLY_ARG, S_mus_set_array_print_length, "an integer");
  return(C_TO_SMALL_XEN_INT(mus_set_array_print_length(XEN_TO_C_INT(val))));
}

static XEN g_ring_modulate(XEN val1, XEN val2) 
{
  #define H_ring_modulate "(" S_ring_modulate " s1 s2) -> s1*s2 (sample by sample)"
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val1), val1, XEN_ARG_1, S_ring_modulate, "a number");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val2), val2, XEN_ARG_2, S_ring_modulate, "a number");
  return(C_TO_XEN_DOUBLE(mus_ring_modulate(XEN_TO_C_DOUBLE(val1), XEN_TO_C_DOUBLE(val2))));
}

static XEN g_amplitude_modulate(XEN val1, XEN val2, XEN val3) 
{
  #define H_amplitude_modulate "(" S_amplitude_modulate " carrier in1 in2) -> in1 * (carrier + in2)"
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val1), val1, XEN_ARG_1, S_amplitude_modulate, "a number");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val2), val2, XEN_ARG_2, S_amplitude_modulate, "a number");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val3), val3, XEN_ARG_3, S_amplitude_modulate, "a number");
  return(C_TO_XEN_DOUBLE(mus_amplitude_modulate(XEN_TO_C_DOUBLE(val1), XEN_TO_C_DOUBLE(val2), XEN_TO_C_DOUBLE(val3))));
}

static XEN g_contrast_enhancement(XEN val1, XEN val2) 
{
  #define H_contrast_enhancement "(" S_contrast_enhancement " sig index) -> sin(sig * pi / 2 + index * sin(sig * 2 * pi))"
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val1), val1, XEN_ARG_1, S_contrast_enhancement, "a number");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val2), val2, XEN_ARG_2, S_contrast_enhancement, "a number");
  return(C_TO_XEN_DOUBLE(mus_contrast_enhancement(XEN_TO_C_DOUBLE(val1), XEN_TO_C_DOUBLE(val2))));
}

static XEN g_dot_product(XEN val1, XEN val2) 
{
  #define H_dot_product "(" S_dot_product " v1 v2) -> sum of (vcts) v1[i] * v2[i] (scalar product)"
  vct *v1, *v2;
  XEN_ASSERT_TYPE(VCT_P(val1), val1, XEN_ARG_1, S_dot_product, "a vct");
  XEN_ASSERT_TYPE(VCT_P(val2), val2, XEN_ARG_2, S_dot_product, "a vct");
  v1 = TO_VCT(val1);
  v2 = TO_VCT(val2);
  return(xen_return_first(C_TO_XEN_DOUBLE(mus_dot_product(v1->data, v2->data, v1->length)), 
			  val1, 
			  val2));
}

static XEN g_sum_of_sines(XEN amps, XEN phases)
{
  #define H_sum_of_sines "(" S_sum_of_sines " amps phases) -> sum of amps[i] * sin(phases[i])"
  vct *v1, *v2;
  XEN_ASSERT_TYPE(VCT_P(amps), amps, XEN_ARG_1, S_sum_of_sines, "a vct");
  XEN_ASSERT_TYPE(VCT_P(phases), phases, XEN_ARG_2, S_sum_of_sines, "a vct");
  v1 = TO_VCT(amps);
  v2 = TO_VCT(phases);
  return(xen_return_first(C_TO_XEN_DOUBLE(mus_sum_of_sines(v1->data, v2->data, v1->length)), 
			  amps, 
			  phases));
}

enum {G_MULTIPLY_ARRAYS, G_RECTANGULAR_POLAR, G_POLAR_RECTANGULAR};

static XEN g_fft_window_1(char *caller, int choice, XEN val1, XEN val2, XEN ulen) 
{
  vct *v1, *v2;
  int len;
  XEN_ASSERT_TYPE(VCT_P(val1), val1, XEN_ARG_1, caller, "a vct");
  XEN_ASSERT_TYPE(VCT_P(val2), val2, XEN_ARG_2, caller, "a vct");
  v1 = TO_VCT(val1);
  v2 = TO_VCT(val2);
  if (XEN_NUMBER_P(ulen)) 
    {
      len = XEN_TO_C_INT_OR_ELSE_WITH_CALLER(ulen, 0, caller); 
      if (len > v1->length) len = v1->length;
    }
  else 
    {
      len = v1->length;
      if (v2->length < len) len = v2->length;
    }
  switch (choice)
    {
    case G_MULTIPLY_ARRAYS: mus_multiply_arrays(v1->data, v2->data, len); break;
    case G_RECTANGULAR_POLAR: mus_rectangular2polar(v1->data, v2->data, len); break;
    case G_POLAR_RECTANGULAR: mus_polar2rectangular(v1->data, v2->data, len); break;
    }
  return(xen_return_first(val1, val2));
}

static XEN g_multiply_arrays(XEN val1, XEN val2, XEN len) 
{
  #define H_multiply_arrays "(" S_multiply_arrays " v1 v2 &optional len) -> (vcts) v1[i] *= v2[i]"
  return(g_fft_window_1(S_multiply_arrays, G_MULTIPLY_ARRAYS, val1, val2, len));
}

static XEN g_rectangular2polar(XEN val1, XEN val2) 
{
  #define H_rectangular2polar "(" S_rectangular2polar " rl im) converts real/imaginary \
data in (vcts) rl and im from rectangular form (fft output) to polar form (a spectrum)"

  return(g_fft_window_1(S_rectangular2polar, G_RECTANGULAR_POLAR, val1, val2, XEN_UNDEFINED));
}

static XEN g_polar2rectangular(XEN val1, XEN val2) 
{
  #define H_polar2rectangular "(" S_polar2rectangular " rl im) converts real/imaginary \
data in (vcts) rl and im from polar form (spectrum) to rectangular form (fft-style)"

  return(g_fft_window_1(S_polar2rectangular, G_POLAR_RECTANGULAR, val1, val2, XEN_UNDEFINED));
}

static XEN g_mus_fft(XEN url, XEN uim, XEN len, XEN usign)
{
  #define H_mus_fft "(" S_mus_fft " rl im &optional len dir) returns the fft of (vcts) rl and im \
the real and imaginary parts of the data, len should be a power of 2, dir = 1 for fft, -1 for inverse-fft"

  int sign, n, np;
  Float nf;
  vct *v1, *v2;
  XEN_ASSERT_TYPE((VCT_P(url)), url, XEN_ARG_1, S_mus_fft, "a vct");
  XEN_ASSERT_TYPE((VCT_P(uim)), uim, XEN_ARG_2, S_mus_fft, "a vct");
  v1 = TO_VCT(url);
  v2 = TO_VCT(uim);
  if (XEN_INTEGER_P(usign)) sign = XEN_TO_C_INT(usign); else sign = 1;
  if (XEN_INTEGER_P(len)) 
    {
      n = XEN_TO_C_INT(len); 
      if (n <= 0)
	mus_misc_error(S_mus_fft, "size <= 0?", len);
      if (n > v1->length)
	n = v1->length;
    }
  else n = v1->length;
  if (!(POWER_OF_2_P(n)))
    {
      nf = (log(n) / log(2.0));
      np = (int)nf;
      n = (int)pow(2.0, np);
    }
  mus_fft(v1->data, v2->data, n, sign);
  return(xen_return_first(url, uim));
}

static XEN g_make_fft_window(XEN type, XEN size, XEN ubeta)
{
  #define H_make_fft_window "(" S_make_fft_window " type size &optional beta) -> fft data window (vct obj) \
type is one of the sndlib fft window identifiers such as kaiser-window \
beta is the window parameter, if any:\n\
     (set! v1 (make-fft-window hamming-window 256))"

  Float beta = 0.0;
  int n, t;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(type), type, XEN_ARG_1, S_make_fft_window, "an integer (window type)");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(size), size, XEN_ARG_2, S_make_fft_window, "an integer");
  if (XEN_NUMBER_P(ubeta)) beta = XEN_TO_C_DOUBLE(ubeta);
  n = XEN_TO_C_INT(size);
  if (n <= 0)
    mus_misc_error(S_make_fft_window, "size <= 0?", size);
  t = XEN_TO_C_INT(type);
  if (MUS_FFT_WINDOW_OK(t))
    return(make_vct(n, mus_make_fft_window(t, n, beta)));
  mus_misc_error(S_make_fft_window, "unknown fft window", type);
  return(XEN_FALSE);
}

static XEN g_spectrum(XEN url, XEN uim, XEN uwin, XEN un, XEN utype)
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
  XEN_ASSERT_TYPE((VCT_P(url)), url, XEN_ARG_1, S_spectrum, "a vct");
  XEN_ASSERT_TYPE((VCT_P(uim)), uim, XEN_ARG_2, S_spectrum, "a vct");
  if (XEN_NOT_FALSE_P(uwin)) XEN_ASSERT_TYPE((VCT_P(uwin)), uwin, XEN_ARG_3, S_spectrum, "a vct or #f");
  v1 = TO_VCT(url);
  v2 = TO_VCT(uim);
  if (XEN_NOT_FALSE_P(uwin)) v3 = TO_VCT(uwin);
  if (XEN_INTEGER_P(un)) 
    {
      n = XEN_TO_C_INT(un); 
      if (n <= 0)
	mus_misc_error(S_spectrum, "size <= 0?", un);
      if (n > v1->length)
	n = v1->length;
    }
  else n = v1->length;
  if (!(POWER_OF_2_P(n)))
    {
      nf = (log(n) / log(2.0));
      np = (int)nf;
      n = (int)pow(2.0, np);
    }
  if (XEN_INTEGER_P(utype)) type = XEN_TO_C_INT(utype); else type = 1; /* linear normalized */
  mus_spectrum(v1->data, v2->data, (v3) ? (v3->data) : NULL, n, type);
  return(xen_return_first(url, uim, uwin));
}

static XEN g_convolution(XEN url1, XEN url2, XEN un)
{
  #define H_mus_convolution "(" S_convolution " v1 v2 &optional len) -> convolution \
of (vcts) v1 with v2, using fft of size len (a power of 2), result in v1"

  int n, np;
  Float nf;
  vct *v1, *v2;
  XEN_ASSERT_TYPE((VCT_P(url1)), url1, XEN_ARG_1, S_convolution, "a vct");
  XEN_ASSERT_TYPE((VCT_P(url2)), url2, XEN_ARG_2, S_convolution, "a vct");
  v1 = TO_VCT(url1);
  v2 = TO_VCT(url2);
  if (XEN_INTEGER_P(un)) 
    {
      n = XEN_TO_C_INT(un); 
      if (n <= 0)
	mus_misc_error(S_convolution, "size <= 0?", un);
      if (n > v1->length)
	n = v1->length;
      if (n > v2->length)
	n = v2->length;
    }
  else n = v1->length;
  if (!(POWER_OF_2_P(n)))
    {
      nf = (log(n) / log(2.0));
      np = (int)nf;
      n = (int)pow(2.0, np);
    }
  mus_convolution(v1->data, v2->data, n);
  return(xen_return_first(url1, url2));
}

static XEN g_clear_array(XEN arr)
{
  #define H_clear_array "(" S_clear_array " v) clears vct v: v[i] = 0.0"
  vct *v;
  XEN_ASSERT_TYPE(VCT_P(arr), arr, XEN_ONLY_ARG, S_clear_array, "a vct");
  v = TO_VCT(arr);
  mus_clear_array(v->data, v->length);
  return(xen_return_first(arr));
}

static XEN g_polynomial(XEN arr, XEN x)
{
  #define H_polynomial "(" S_polynomial " coeffs x) evaluates a polynomial at x"
  vct *v;
  XEN_ASSERT_TYPE(VCT_P(arr), arr, XEN_ARG_1, S_polynomial, "a vct");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(x), x, XEN_ARG_2, S_polynomial, "a number");
  v = TO_VCT(arr);
  return(xen_return_first(C_TO_XEN_DOUBLE(mus_polynomial(v->data, XEN_TO_C_DOUBLE(x), v->length)), arr));
}

static XEN g_array_interp(XEN obj, XEN phase, XEN size) /* opt size */
{
  #define H_array_interp "(" S_array_interp " v phase &optional size) -> v[phase] \
taking into account wrap-around (size = size of data), with linear interpolation if phase is not integral."

  int len;
  vct *v;
  XEN_ASSERT_TYPE(VCT_P(obj), obj, XEN_ARG_1, S_array_interp, "a vct");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(phase), phase, XEN_ARG_2, S_array_interp, "a number");
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(size), size, XEN_ARG_3, S_array_interp, "an integer");
  v = TO_VCT(obj);
  if (XEN_BOUND_P(size)) 
    {
      len = XEN_TO_C_INT(size); 
      if (len <= 0)
	mus_misc_error(S_array_interp, "size <= 0?", size);
      if (len > v->length) len = v->length;
    }
  else len = v->length;
  return(xen_return_first(C_TO_XEN_DOUBLE(mus_array_interp(v->data, XEN_TO_C_DOUBLE(phase), len)), obj));
}

/* ---------------- mus-xen struct ---------------- */

static XEN_OBJECT_TYPE mus_xen_tag;

#define MUS_XEN_P(obj) (XEN_OBJECT_TYPE_P(obj, mus_xen_tag))
int mus_xen_p(XEN obj) {return(MUS_XEN_P(obj));}

static XEN *make_vcts(int size)
{
  int i;
  XEN *vcts;
  vcts = (XEN *)MALLOC(size * sizeof(XEN));
  for (i = 0; i < size; i++)
    vcts[i] = XEN_UNDEFINED;
  return(vcts);
}

static XEN_MARK_OBJECT_TYPE mark_mus_xen(XEN obj) 
{
  int i;
  mus_xen *ms;
#if HAVE_RUBY
  /* rb_gc_mark passes us the actual value, not the XEN wrapper! */
  ms = (mus_xen *)obj;
#else
  ms = CLM_TO_MUS_XEN(obj);
#endif
  if (ms->vcts) 
    {
      for (i = 0; i < ms->nvcts; i++) 
	if (XEN_BOUND_P(ms->vcts[i]))
	  xen_gc_mark(ms->vcts[i]);
    }
#if HAVE_RUBY
  return(NULL);
#else
  return(XEN_FALSE);
#endif
}

#define DONT_FREE_FRAME -1
#define FREE_FRAME 1

static void mus_xen_free(mus_xen *ms)
{
  if (ms->nvcts != DONT_FREE_FRAME) mus_free(ms->gen);
  ms->gen = NULL;
  if (ms->vcts) FREE(ms->vcts); 
  ms->vcts = NULL;
  FREE(ms);
}

XEN_MAKE_OBJECT_FREE_PROCEDURE(mus_xen, free_mus_xen, mus_xen_free)

#if HAVE_GUILE
static int print_mus_xen(XEN obj, XEN port, scm_print_state *pstate)
{
  XEN_PUTS("#<", port);
  XEN_PUTS(mus_describe(MUS_XEN_TO_CLM(obj)), port);
  XEN_PUTS(">", port);
  return(1);
}
#endif

#if HAVE_RUBY
static XEN mus_xen_to_s(XEN obj)
{
  return(C_TO_XEN_STRING(mus_describe(MUS_XEN_TO_CLM(obj))));
}
#endif

static XEN equalp_mus_xen(XEN obj1, XEN obj2) 
{
#if HAVE_RUBY
  if ((!(MUS_XEN_P(obj1))) || (!(MUS_XEN_P(obj2)))) return(XEN_FALSE);
#endif
  return(C_TO_XEN_BOOLEAN(mus_equalp(MUS_XEN_TO_CLM(obj1), MUS_XEN_TO_CLM(obj2))));
}

#if HAVE_RUBY || HAVE_APPLICABLE_SMOB
static XEN mus_xen_apply(XEN gen, XEN arg1, XEN arg2)
{
  return(C_TO_XEN_DOUBLE(mus_run(MUS_XEN_TO_CLM(gen),
				 (XEN_NUMBER_P(arg1)) ? XEN_TO_C_DOUBLE(arg1) : 0.0,
				 (XEN_NUMBER_P(arg2)) ? XEN_TO_C_DOUBLE(arg2) : 0.0)));
}
#endif

XEN mus_xen_to_object(mus_xen *gn) /* global for user-defined gens */
{
  XEN_MAKE_AND_RETURN_OBJECT(mus_xen_tag, gn, mark_mus_xen, free_mus_xen);
}

#define MUS_DATA_POSITION 0

XEN mus_xen_to_object_with_vct(mus_xen *gn, XEN v) /* global for user-defined gens */
{
  XEN new_dly = XEN_FALSE;
  XEN_MAKE_OBJECT(new_dly, mus_xen_tag, gn, mark_mus_xen, free_mus_xen);
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
#define S_mus_width          "mus-width"
#define S_mus_set_width      "mus-set-width"
#define S_mus_offset         "mus-offset"
#define S_mus_file_name      "mus-file-name"
#define S_mus_inspect        "mus-inspect"
#define S_mus_describe       "mus-describe"
#define S_mus_name           "mus-name"
#define S_mus_run            "mus-run"
#define S_mus_bank           "mus-bank"

static XEN g_inspect(XEN gen)
{
  #define H_mus_inspect "(" S_mus_inspect " gen) -> the internal state of gen"
  XEN_ASSERT_TYPE((MUS_XEN_P(gen)), gen, XEN_ONLY_ARG, S_mus_inspect, "a generator");
  return(C_TO_XEN_STRING(mus_inspect(MUS_XEN_TO_CLM(gen))));
}

static XEN g_describe(XEN gen) 
{
  #define H_mus_describe "(" S_mus_describe " gen) -> the user's view of the state of gen"
  XEN_ASSERT_TYPE((MUS_XEN_P(gen)), gen, XEN_ONLY_ARG, S_mus_describe, "a generator");
  return(C_TO_XEN_STRING(mus_describe(MUS_XEN_TO_CLM(gen))));
}

static XEN g_phase(XEN gen) 
{
  #define H_mus_phase "(" S_mus_phase " gen) -> gen's current phase (radians)"
  XEN_ASSERT_TYPE(MUS_XEN_P(gen), gen, XEN_ONLY_ARG, S_mus_phase, "a generator");
  return(C_TO_XEN_DOUBLE(mus_phase(MUS_XEN_TO_CLM(gen))));
}

static XEN g_set_phase(XEN gen, XEN val) 
{
  XEN_ASSERT_TYPE(MUS_XEN_P(gen), gen, XEN_ARG_1, S_mus_set_phase, "a generator");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ARG_2, S_mus_set_phase, "a number");
  return(C_TO_XEN_DOUBLE(mus_set_phase(MUS_XEN_TO_CLM(gen), XEN_TO_C_DOUBLE(val))));
}

static XEN g_scaler(XEN gen) 
{
  #define H_mus_scaler "(" S_mus_scaler " gen) -> gen's scaler, if any"
  XEN_ASSERT_TYPE(MUS_XEN_P(gen), gen, XEN_ONLY_ARG, S_mus_scaler, "a generator");
  return(C_TO_XEN_DOUBLE(mus_scaler(MUS_XEN_TO_CLM(gen))));
}

static XEN g_set_scaler(XEN gen, XEN val) 
{
  XEN_ASSERT_TYPE(MUS_XEN_P(gen), gen, XEN_ARG_1, S_mus_set_scaler, "a generator");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ARG_2, S_mus_set_scaler, "a number");
  return(C_TO_XEN_DOUBLE(mus_set_scaler(MUS_XEN_TO_CLM(gen), XEN_TO_C_DOUBLE(val))));
}

static XEN g_width(XEN gen) 
{
  #define H_mus_width "(" S_mus_width " gen) -> gen's width, if any"
  XEN_ASSERT_TYPE(MUS_XEN_P(gen), gen, XEN_ONLY_ARG, S_mus_width, "a generator");
  return(C_TO_XEN_DOUBLE(mus_width(MUS_XEN_TO_CLM(gen))));
}

static XEN g_set_width(XEN gen, XEN val) 
{
  XEN_ASSERT_TYPE(MUS_XEN_P(gen), gen, XEN_ARG_1, S_mus_set_width, "a generator");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ARG_2, S_mus_set_width, "a number");
  return(C_TO_XEN_DOUBLE(mus_set_width(MUS_XEN_TO_CLM(gen), XEN_TO_C_DOUBLE(val))));
}

static XEN g_offset(XEN gen) 
{
  #define H_mus_offset "(" S_mus_offset " gen) -> gen's offset, if any"
  XEN_ASSERT_TYPE(MUS_XEN_P(gen), gen, XEN_ONLY_ARG, S_mus_offset, "a generator");
  return(C_TO_XEN_DOUBLE(mus_offset(MUS_XEN_TO_CLM(gen))));
}

static XEN g_frequency(XEN gen) 
{
  #define H_mus_frequency "(" S_mus_frequency " gen) -> gen's frequency (Hz)"
  XEN_ASSERT_TYPE(MUS_XEN_P(gen), gen, XEN_ONLY_ARG, S_mus_frequency, "a generator");
  return(C_TO_XEN_DOUBLE(mus_frequency(MUS_XEN_TO_CLM(gen))));
}

static XEN g_set_frequency(XEN gen, XEN val) 
{
  XEN_ASSERT_TYPE(MUS_XEN_P(gen), gen, XEN_ARG_1, S_mus_frequency, "a generator");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ARG_2, S_mus_set_frequency, "a number");
  return(C_TO_XEN_DOUBLE(mus_set_frequency(MUS_XEN_TO_CLM(gen), XEN_TO_C_DOUBLE(val))));
}

static XEN g_run(XEN gen, XEN arg1, XEN arg2) 
{
  #define H_mus_run "(" S_mus_run " gen &optional arg1 arg2) -> apply gen to arg1 and arg2"
  XEN_ASSERT_TYPE(MUS_XEN_P(gen), gen, XEN_ARG_1, S_mus_run, "a generator");
  return(C_TO_XEN_DOUBLE(mus_run(MUS_XEN_TO_CLM(gen),
				 (XEN_NUMBER_P(arg1)) ? XEN_TO_C_DOUBLE(arg1) : 0.0,
				 (XEN_NUMBER_P(arg2)) ? XEN_TO_C_DOUBLE(arg2) : 0.0)));
}

static XEN g_length(XEN gen) 
{
  #define H_mus_length "(" S_mus_length " gen) -> gen's length, if any"
  XEN_ASSERT_TYPE(MUS_XEN_P(gen), gen, XEN_ONLY_ARG, S_mus_length, "a generator");
  return(C_TO_SMALL_XEN_INT(mus_length(MUS_XEN_TO_CLM(gen))));
}

static XEN g_set_length(XEN gen, XEN val) 
{
  XEN_ASSERT_TYPE(MUS_XEN_P(gen), gen, XEN_ARG_1, S_mus_length, "a generator");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ARG_2, S_mus_set_length, "a number");
  return(C_TO_SMALL_XEN_INT(mus_set_length(MUS_XEN_TO_CLM(gen), XEN_TO_C_INT_OR_ELSE(val, 0))));
}

static XEN g_name(XEN gen) 
{
  /* mus_name points to a constant string, so don't change it directly */
  #define H_mus_name "(" S_mus_name " gen) -> gen's name, if any"
  XEN_ASSERT_TYPE(MUS_XEN_P(gen), gen, XEN_ONLY_ARG, S_mus_name, "a generator");
  return(C_TO_XEN_STRING(mus_name(MUS_XEN_TO_CLM(gen))));
}

static XEN g_data(XEN gen) 
{
  #define H_mus_data "(" S_mus_data " gen) -> gen's internal data (vct), if any"
  mus_xen *ms;
  XEN_ASSERT_TYPE(MUS_XEN_P(gen), gen, XEN_ONLY_ARG, S_mus_data, "a generator");
  ms = CLM_TO_MUS_XEN(gen);
  if (ms->vcts)
    return(ms->vcts[MUS_DATA_POSITION]); 
  else return(XEN_FALSE);
}

static XEN g_set_data(XEN gen, XEN val) 
{
  mus_xen *ms;
  mus_any *ma;
  vct *v;
  XEN_ASSERT_TYPE(MUS_XEN_P(gen), gen, XEN_ARG_1, S_mus_set_data, "a generator");
  XEN_ASSERT_TYPE((VCT_P(val)), val, XEN_ARG_2, S_mus_set_data, "a vct");
  ms = CLM_TO_MUS_XEN(gen);
  if (ms->vcts)
    {
      v = (vct *)XEN_OBJECT_REF(val);
      ma = ms->gen;
      mus_set_data(ma, v->data);  /* TO REMEMBER: if allocated, should have freed, and set to not allocated */
      ms->vcts[MUS_DATA_POSITION] = val;
      return(val);
    }
  return(xen_return_first(XEN_FALSE, gen, val));
}

static XEN g_file_name(XEN gen) 
{
  #define H_mus_file_name "(" S_mus_file_name " gen) -> file associated with gen, if any"
  XEN_ASSERT_TYPE(MUS_XEN_P(gen), gen, XEN_ONLY_ARG, S_mus_file_name, "a generator");
  return(C_TO_XEN_STRING(mus_file_name(MUS_XEN_TO_CLM(gen))));
}

static Float *copy_vct_data(vct *v)
{
  Float *line = NULL;
  line = (Float *)MALLOC(v->length * sizeof(Float));
  if (line) 
    memcpy((void *)line, (void *)(v->data), (v->length * sizeof(Float)));
  return(line);
}

static Float *whatever_to_floats(XEN inp, int size, const char *caller)
{
  Float *invals = NULL;
  int i;
  XEN *data;
  vct *v;
  Float inval;
  if (size <= 0) return(NULL);
  if (XEN_VECTOR_P(inp))
    {
      if (XEN_VECTOR_LENGTH(inp) < size) return(NULL);
      invals = (Float *)MALLOC(size * sizeof(Float));
      data = XEN_VECTOR_ELEMENTS(inp);
      for (i = 0; i < size; i++) 
	invals[i] = XEN_TO_C_DOUBLE(data[i]);
    }
  else
    {
      if (VCT_P(inp))
	{
	  v = TO_VCT(inp);
	  if (v->length < size) return(NULL);
	  invals = copy_vct_data(v);
	}
      else
	{
	  if (XEN_NUMBER_P(inp)) 
	    {
	      invals = (Float *)MALLOC(size * sizeof(Float));
	      inval = XEN_TO_C_DOUBLE(inp);
	      for (i = 0; i < size; i++) 
		invals[i] = inval;
	    }
	  else
	    {
	      if (XEN_PROCEDURE_P(inp))
		{
		  if (XEN_REQUIRED_ARGS(inp) == 1)
		    {
		      invals = (Float *)MALLOC(size * sizeof(Float));
		      for (i = 0; i < size; i++) 
			invals[i] = XEN_TO_C_DOUBLE(XEN_CALL_1_NO_CATCH(inp, C_TO_XEN_INT(i), caller));
		    }
		  else mus_misc_error(caller, "wrong number of args to function (want 1)", inp);
		}
	      /* not an error if #<undefined> or the like -- just return NULL */
	    }
	}
    }
  return(invals);
}


static XEN g_mus_bank(XEN gens, XEN amps, XEN inp, XEN inp2)
{
  #define H_mus_bank "(" S_mus_bank " gens amps &optional args1 args2) -> sum of (* (amps i) ((gens i) (args1 i) (args2 i)))"
  /* amps and inp1/inp2 can be a Float, a vct object, a function, or a vector of Floats */
  Float outval = 0.0;
  int i, size;
  Float *scls = NULL, *invals = NULL, *invals2 = NULL;
  mus_any **gs;
  XEN *data;
  XEN_ASSERT_TYPE(XEN_VECTOR_P(gens), gens, XEN_ARG_1, S_mus_bank, "a vector of generators");
  size = XEN_VECTOR_LENGTH(gens);
  if (size == 0) return(XEN_ZERO);
  scls = whatever_to_floats(amps, size, S_mus_bank);
  if (scls == NULL)
    XEN_ASSERT_TYPE(0, amps, XEN_ARG_2, S_mus_bank, "a vct, vector, number, or procedure(!)");
  gs = (mus_any **)CALLOC(size, sizeof(mus_any *));
  data = XEN_VECTOR_ELEMENTS(gens);
  for (i = 0; i < size; i++) 
    if (MUS_XEN_P(data[i]))
      gs[i] = MUS_XEN_TO_CLM(data[i]);
    else 
      {
	if (scls) FREE(scls);
	if (gs) FREE(gs);
	mus_misc_error(S_mus_bank, "not a generator!", data[i]);
      }
  invals = whatever_to_floats(inp, size, S_mus_bank); /* these two can be null */
  invals2 = whatever_to_floats(inp2, size, S_mus_bank);
  outval = mus_bank(gs, scls, invals, invals2, size);
  if (scls) FREE(scls);
  if (invals) FREE(invals);
  if (invals2) FREE(invals2);
  if (gs) FREE(gs);
  return(C_TO_XEN_DOUBLE(outval));
}


/* ---------------- oscil ---------------- */

#define S_make_oscil "make-oscil"
#define S_oscil      "oscil"
#define S_oscil_bank "oscil-bank"
#define S_oscil_p    "oscil?"
#define S_mus_apply  "mus-apply"

static XEN g_make_oscil(XEN arg1, XEN arg2, XEN arg3, XEN arg4)
{
  #define H_make_oscil "(" S_make_oscil " &opt-key (frequency 440.0) (phase 0.0)) -> a new " S_oscil " (sinewave) generator"
  mus_xen *gn;
  mus_any *ge;
  int vals;
  XEN args[4]; XEN keys[2];
  int orig_arg[2] = {0, 0};
  Float freq = 440.0, phase = 0.0;
  keys[0] = all_keys[C_frequency];
  keys[1] = all_keys[C_initial_phase];
  args[0] = arg1; args[1] = arg2; args[2] = arg3; args[3] = arg4; 
  vals = decode_keywords(S_make_oscil, 2, keys, 4, args, orig_arg);
  if (vals > 0)
    {
      freq = fkeyarg(keys[0], S_make_oscil, orig_arg[0] + 1, freq);
      phase = fkeyarg(keys[1], S_make_oscil, orig_arg[1] + 1, phase);
    }
  ge = mus_make_oscil(freq, phase);
  if (ge)
    {
      gn = (mus_xen *)CALLOC(1, sizeof(mus_xen));
      gn->gen = ge;
      gn->nvcts = 0;
      return(mus_xen_to_object(gn));
    }
  return(XEN_FALSE);
}

static XEN g_oscil(XEN os, XEN fm, XEN pm)
{
  #define H_oscil "(" S_oscil " gen &optional fm pm) -> next sample from " S_oscil " gen: rtn = sin(phase + pm) phase += (freq + fm)"
  Float fm1 = 0.0, pm1 = 0.0;
  XEN_ASSERT_TYPE((MUS_XEN_P(os)) && (mus_oscil_p(MUS_XEN_TO_CLM(os))), os, XEN_ARG_1, S_oscil, "an oscil");
  if (XEN_NUMBER_P(fm)) fm1 = XEN_TO_C_DOUBLE(fm); else XEN_ASSERT_TYPE(XEN_NOT_BOUND_P(fm), fm, XEN_ARG_2, S_oscil, "a number");
  if (XEN_NUMBER_P(pm)) pm1 = XEN_TO_C_DOUBLE(pm); else XEN_ASSERT_TYPE(XEN_NOT_BOUND_P(pm), pm, XEN_ARG_3, S_oscil, "a number");
  return(C_TO_XEN_DOUBLE(mus_oscil(MUS_XEN_TO_CLM(os), fm1, pm1)));
}

static XEN g_oscil_bank(XEN amps, XEN gens, XEN inp, XEN size)
{
  /* size currently ignored */
  #define H_oscil_bank "(" S_oscil_bank " scls gens fms) -> sum a bank of " S_oscil "s: scls[i] * " S_oscil "(gens[i], fms[i])"
  XEN_ASSERT_TYPE(XEN_VECTOR_P(gens), gens, XEN_ARG_2, S_oscil_bank, "a vector of oscils");
  return(g_mus_bank(gens, amps, inp, XEN_UNDEFINED));
}

static XEN g_oscil_p(XEN os) 
{
  #define H_oscil_p "(" S_oscil_p " gen) -> #t if gen is an " S_oscil " generator, else #f"
  return(C_TO_XEN_BOOLEAN((MUS_XEN_P(os)) && (mus_oscil_p(MUS_XEN_TO_CLM(os)))));
}

static XEN g_mus_apply(XEN arglist)
{
  #define H_mus_apply "(" S_mus_apply " gen &rest args) applies gen to args"
  int arglist_len;
  mus_any *gen;
  arglist_len = XEN_LIST_LENGTH(arglist);
  if ((arglist_len > 3) || (arglist_len == 0)) 
    return(C_TO_XEN_DOUBLE(0.0));
  gen = MUS_XEN_TO_CLM(XEN_CAR(arglist));
  if (arglist_len == 1) 
    return(C_TO_XEN_DOUBLE(mus_apply(gen)));
  if (arglist_len == 2)
    return(C_TO_XEN_DOUBLE(mus_apply(gen, 
				     XEN_TO_C_DOUBLE(XEN_CADR(arglist)))));
  return(C_TO_XEN_DOUBLE(mus_apply(gen, 
				   XEN_TO_C_DOUBLE(XEN_CADR(arglist)), 
				   XEN_TO_C_DOUBLE(XEN_CADDR(arglist)))));
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

static XEN g_make_delay_1(int choice, XEN arglist)
{
  mus_xen *gn;
  mus_any *ge = NULL;
  char *caller = NULL;
  XEN args[14]; XEN keys[7];
  int orig_arg[7] = {0, 0, 0, 0, 0, 0, 0};
  int vals, i, argn = 0, len, arglist_len, keyn, max_size = -1, need_free = FALSE;
  int size = 1, size_key = 0;
  Float *line = NULL;
  Float scaler = 0.0, feedback = 0.0, feedforward = 0.0;
  XEN initial_contents = XEN_UNDEFINED; 
  XEN lst;
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
  for (i = 0; i < 14; i++) args[i] = XEN_UNDEFINED;
  arglist_len = XEN_LIST_LENGTH(arglist);
  for (i = 0; i < arglist_len; i++) args[i] = XEN_LIST_REF(arglist, i);
  vals = decode_keywords(caller, argn, keys, argn*2, args, orig_arg);
  if (vals > 0)
    {
      keyn = 0;
      switch (choice)
	{
	case G_DELAY: 
	  break;
	case G_COMB: case G_NOTCH:
	  scaler = fkeyarg(keys[keyn], caller, orig_arg[keyn] + 1, scaler);
	  keyn++;
	  break;
	case G_ALL_PASS:
	  feedback = fkeyarg(keys[keyn], caller, orig_arg[keyn] + 1, feedback);
	  keyn++;
	  feedforward = fkeyarg(keys[keyn], caller, orig_arg[keyn] + 1, feedforward);
	  keyn++;
	  break;
	}
      size = ikeyarg(keys[keyn], caller, orig_arg[keyn] + 1, size);
      size_key = keyn;
      keyn++;
      if (!(XEN_KEYWORD_P(keys[keyn])))
	{
	  initial_contents = keys[keyn];
	  if (VCT_P(initial_contents))
	    line = copy_vct_data(TO_VCT(initial_contents));
	  else
	    {
	      if (XEN_LIST_P_WITH_LENGTH(initial_contents, len))
		{
		  if (len == 0) 
		    mus_misc_error(caller, "initial-contents empty?", initial_contents);
		  line = (Float *)CALLOC(len, sizeof(Float));
		  for (i = 0, lst = XEN_COPY_ARG(initial_contents); (i < len) && (XEN_NOT_NULL_P(lst)); i++, lst = XEN_CDR(lst)) 
		    line[i] = XEN_TO_C_DOUBLE(XEN_CAR(lst));
		}
	    }
	}
      keyn++;
      if (XEN_KEYWORD_P(keys[keyn]))
	initial_element = 0.0;
      else
	{
	  if (XEN_NUMBER_P(keys[keyn]))
	    initial_element = XEN_TO_C_DOUBLE_WITH_CALLER(keys[keyn], caller);
	  else 
	    {
	      if (line) FREE(line);
	      XEN_ASSERT_TYPE(XEN_NUMBER_P(keys[keyn]), keys[keyn], orig_arg[keyn] + 1, caller, "a number");
	    }
	}
      keyn++;

      if (XEN_KEYWORD_P(keys[keyn]))
	max_size = size;
      else
	{
	  if (XEN_NUMBER_P(keys[keyn]))
	    max_size = XEN_TO_C_INT_OR_ELSE_WITH_CALLER(keys[keyn], 0, caller);
	  else
	    {
	      if (line) FREE(line);
	      XEN_ASSERT_TYPE(XEN_NUMBER_P(keys[keyn]), keys[keyn], orig_arg[keyn] + 1, caller, "a number");
	    }
	}
    }
  if (size < 0)
    {
      if (line) FREE(line);
      mus_misc_error(caller, "size < 0?", keys[size_key]);
    }
  if (max_size == -1) max_size = size;
  if ((max_size <= 0) || (max_size < size))
    {
      if (line) FREE(line);
      mus_misc_error(caller, "invalid delay length", C_TO_XEN_INT(max_size));
    }
  if (line == NULL)
    {
      line = (Float *)CALLOC(max_size, sizeof(Float));
      need_free = TRUE;
      if (initial_element != 0.0) 
	for (i = 0; i < max_size; i++) 
	  line[i] = initial_element;
    }
  old_error_handler = mus_error_set_handler(local_mus_error);
  switch (choice)
    {
    case G_DELAY: ge = mus_make_delay(size, line, max_size); break;
    case G_COMB: ge = mus_make_comb(scaler, size, line, max_size); break;
    case G_NOTCH: ge = mus_make_notch(scaler, size, line, max_size); break;
    case G_ALL_PASS: ge = mus_make_all_pass(feedback, feedforward, size, line, max_size); break;
    }
  mus_error_set_handler(old_error_handler);
  if (ge)
    {
      gn = (mus_xen *)CALLOC(1, sizeof(mus_xen));
      gn->vcts = make_vcts(1);
      gn->gen = ge;
      gn->nvcts = 1;
      return(mus_xen_to_object_with_vct(gn, make_vct(max_size, line)));
    }
  if (need_free) FREE(line);
  mus_error(local_error_type, local_error_msg);
  return(XEN_FALSE);
}

static XEN g_make_delay(XEN args) 
{
  #define H_make_delay "(" S_make_delay " &opt-key size initial-contents (initial-element 0.0) max-size)\n\
returns a new delay line of size elements. \
If the delay length will be changing, max-size determines its maximum length\n\
   (" S_make_delay " len :max-size (+ len 10))\n\
provides 10 extra elements of delay for subsequent phasing. \
initial-contents can be either a list or a vct object."

  return(g_make_delay_1(G_DELAY, args));
}

static XEN g_make_comb(XEN args) 
{
  #define H_make_comb "(" S_make_comb " &opt-key scaler size initial-contents (initial-element 0.0) max-size)\n\
returns a new comb filter (a delay line with a scaler on the feedback) of size elements. \
If the comb length will be changing, max-size determines its maximum length. \
initial-contents can be either a list or a vct object."

  return(g_make_delay_1(G_COMB, args));
}

static XEN g_make_notch(XEN args) 
{
  #define H_make_notch "(" S_make_notch " &opt-key scaler size initial-contents (initial-element 0.0) max-size)\n\
returns a new notch filter (a delay line with a scaler on the feedforward) of size elements. \
If the notch length will be changing, max-size determines its maximum length. \
initial-contents can be either a list or a vct object."

  return(g_make_delay_1(G_NOTCH, args));
}

static XEN g_make_all_pass(XEN args) 
{
  #define H_make_all_pass "(" S_make_all_pass " &opt-key feedback feedforward size initial-contents (initial-element 0.0) max-size)\n\
returns a new allpass filter (a delay line with a scalers on both the feedback and the feedforward). \
If the all-pass length will be changing, max-size determines its maximum length. \
initial-contents can be either a list or a vct object."

  return(g_make_delay_1(G_ALL_PASS, args));
}

static XEN g_delay(XEN obj, XEN input, XEN pm)
{
  #define H_delay "(" S_delay " gen &optional (val 0.0) (pm 0.0))\n\
delays val according to the delay line's length and pm ('phase-modulation'). \
If pm is greater than 0.0, the max-size argument used to create gen should have accommodated its maximum value. (The \
Scheme function delay is available as %delay)"

  Float in1 = 0.0, pm1 = 0.0;
  XEN_ASSERT_TYPE((MUS_XEN_P(obj)) && (mus_delay_p(MUS_XEN_TO_CLM(obj))), obj, XEN_ARG_1, S_delay, "a delay line");
  if (XEN_NUMBER_P(input)) in1 = XEN_TO_C_DOUBLE(input); else XEN_ASSERT_TYPE(XEN_NOT_BOUND_P(input), input, XEN_ARG_2, S_delay, "a number");
  if (XEN_NUMBER_P(pm)) pm1 = XEN_TO_C_DOUBLE(pm); else XEN_ASSERT_TYPE(XEN_NOT_BOUND_P(pm), pm, XEN_ARG_3, S_delay, "a number");
  return(C_TO_XEN_DOUBLE(mus_delay(MUS_XEN_TO_CLM(obj), in1, pm1)));
}

static XEN g_notch(XEN obj, XEN input, XEN pm)
{
  #define H_notch "(" S_notch " gen &optional (val 0.0) (pm 0.0)) notch filters val, pm changes the delay length."
  Float in1 = 0.0, pm1 = 0.0;
  XEN_ASSERT_TYPE((MUS_XEN_P(obj)) && (mus_notch_p(MUS_XEN_TO_CLM(obj))), obj, XEN_ARG_1, S_notch, "a notch filter");
  if (XEN_NUMBER_P(input)) in1 = XEN_TO_C_DOUBLE(input); else XEN_ASSERT_TYPE(XEN_NOT_BOUND_P(input), input, XEN_ARG_2, S_notch, "a number");
  if (XEN_NUMBER_P(pm)) pm1 = XEN_TO_C_DOUBLE(pm); else XEN_ASSERT_TYPE(XEN_NOT_BOUND_P(pm), pm, XEN_ARG_3, S_notch, "a number");
  return(C_TO_XEN_DOUBLE(mus_notch(MUS_XEN_TO_CLM(obj), in1, pm1)));
}

static XEN g_comb(XEN obj, XEN input, XEN pm)
{
  #define H_comb "(" S_comb " gen &optional (val 0.0) (pm 0.0)) comb filters val, pm changes the delay length."
  Float in1 = 0.0, pm1 = 0.0;
  XEN_ASSERT_TYPE((MUS_XEN_P(obj)) && (mus_comb_p(MUS_XEN_TO_CLM(obj))), obj, XEN_ARG_1, S_comb, "a comb filter");
  if (XEN_NUMBER_P(input)) in1 = XEN_TO_C_DOUBLE(input); else XEN_ASSERT_TYPE(XEN_NOT_BOUND_P(input), input, XEN_ARG_2, S_comb, "a number");
  if (XEN_NUMBER_P(pm)) pm1 = XEN_TO_C_DOUBLE(pm); else XEN_ASSERT_TYPE(XEN_NOT_BOUND_P(pm), pm, XEN_ARG_3, S_comb, "a number");
  return(C_TO_XEN_DOUBLE(mus_comb(MUS_XEN_TO_CLM(obj), in1, pm1)));
}

static XEN g_all_pass(XEN obj, XEN input, XEN pm)
{
  #define H_all_pass "(" S_all_pass " gen &optional (val 0.0) (pm 0.0)) all-pass filters val, pm changes the delay length."
  Float in1 = 0.0, pm1 = 0.0;
  XEN_ASSERT_TYPE((MUS_XEN_P(obj)) && (mus_all_pass_p(MUS_XEN_TO_CLM(obj))), obj, XEN_ARG_1, S_all_pass, "an all-pass filter");
  if (XEN_NUMBER_P(input)) in1 = XEN_TO_C_DOUBLE(input); else XEN_ASSERT_TYPE(XEN_NOT_BOUND_P(input), input, XEN_ARG_2, S_all_pass, "a number");
  if (XEN_NUMBER_P(pm)) pm1 = XEN_TO_C_DOUBLE(pm); else XEN_ASSERT_TYPE(XEN_NOT_BOUND_P(pm), pm, XEN_ARG_3, S_all_pass, "a number");
  return(C_TO_XEN_DOUBLE(mus_all_pass(MUS_XEN_TO_CLM(obj), in1, pm1)));
}

static XEN g_tap(XEN obj, XEN loc)
{
  #define H_tap "(" S_tap " gen &optional (pm 0.0)) taps the " S_delay " generator offset by pm"
  Float dloc = 0.0;
  XEN_ASSERT_TYPE((MUS_XEN_P(obj)), obj, XEN_ARG_1, S_tap, "a delay line tap");
  if (XEN_NUMBER_P(loc)) dloc = XEN_TO_C_DOUBLE(loc); else XEN_ASSERT_TYPE(XEN_NOT_BOUND_P(loc), loc, XEN_ARG_3, S_tap, "a number");
  return(C_TO_XEN_DOUBLE(mus_tap(MUS_XEN_TO_CLM(obj), dloc)));
}

static XEN g_delay_p(XEN obj) 
{
  #define H_delay_p "(" S_delay_p " gen) -> #t if gen is a delay line, else #f"
  return(C_TO_XEN_BOOLEAN((MUS_XEN_P(obj)) && (mus_delay_p(MUS_XEN_TO_CLM(obj)))));
}

static XEN g_comb_p(XEN obj)
{
  #define H_comb_p "(" S_comb_p " gen) -> #t if gen is a comb filter, else #f"
  return(C_TO_XEN_BOOLEAN((MUS_XEN_P(obj)) && (mus_comb_p(MUS_XEN_TO_CLM(obj)))));
}

static XEN g_notch_p(XEN obj) 
{
  #define H_notch_p "(" S_notch_p " gen) -> #t if gen is a notch filter, else #f"
  return(C_TO_XEN_BOOLEAN((MUS_XEN_P(obj)) && (mus_notch_p(MUS_XEN_TO_CLM(obj)))));
}

static XEN g_all_pass_p(XEN obj) 
{
  #define H_all_pass_p "(" S_all_pass_p " gen) -> #t if gen is an all-pass filter, else #f"
  return(C_TO_XEN_BOOLEAN((MUS_XEN_P(obj)) && (mus_all_pass_p(MUS_XEN_TO_CLM(obj)))));
}



/* -------- sum-of-cosines -------- */

#define S_make_sum_of_cosines "make-sum-of-cosines"
#define S_sum_of_cosines      "sum-of-cosines"
#define S_sum_of_cosines_p    "sum-of-cosines?"
#define S_mus_cosines         "mus-cosines"

static XEN g_sum_of_cosines_p(XEN obj) 
{
  #define H_sum_of_cosines_p "(" S_sum_of_cosines_p " gen) -> #t if gen is a " S_sum_of_cosines " generator, else #f"
  return(C_TO_XEN_BOOLEAN((MUS_XEN_P(obj)) && (mus_sum_of_cosines_p(MUS_XEN_TO_CLM(obj)))));
}

static XEN g_make_sum_of_cosines(XEN arg1, XEN arg2, XEN arg3, XEN arg4, XEN arg5, XEN arg6)
{
  #define H_make_sum_of_cosines "(" S_make_sum_of_cosines " &opt-key (frequency 440.0) (initial-phase 0.0) (cosines 1))\n\
returns a new " S_sum_of_cosines " generator, producing a band-limited pulse train."

  mus_xen *gn;
  mus_any *ge;
  XEN args[6]; XEN keys[3];
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
      cosines = ikeyarg(keys[0], S_make_sum_of_cosines, orig_arg[0] + 1, cosines);
      freq = fkeyarg(keys[1], S_make_sum_of_cosines, orig_arg[1] + 1, freq);
      phase = fkeyarg(keys[2], S_make_sum_of_cosines, orig_arg[2] + 1, phase);
    }
  if (cosines <= 0)
    mus_misc_error(S_make_sum_of_cosines, "cosines <= 0?", keys[0]);
  ge = mus_make_sum_of_cosines(cosines, freq, phase);
  if (ge)
    {
      gn = (mus_xen *)CALLOC(1, sizeof(mus_xen));
      gn->gen = ge;
      gn->nvcts = 0;
      return(mus_xen_to_object(gn));
    }
  return(XEN_FALSE);
}

static XEN g_sum_of_cosines(XEN obj, XEN fm)
{
  #define H_sum_of_cosines "(" S_sum_of_cosines " gen &optional (fm 0.0))\n\
gets the next sample of the band-limited pulse-train produced by gen"

  Float fm1 = 0.0;
  XEN_ASSERT_TYPE((MUS_XEN_P(obj)) && (mus_sum_of_cosines_p(MUS_XEN_TO_CLM(obj))), obj, XEN_ARG_1, S_sum_of_cosines, "a sum-of-cosines gen");
  if (XEN_NUMBER_P(fm)) fm1 = XEN_TO_C_DOUBLE(fm); else XEN_ASSERT_TYPE(XEN_NOT_BOUND_P(fm), fm, XEN_ARG_2, S_sum_of_cosines, "a number");
  return(C_TO_XEN_DOUBLE(mus_sum_of_cosines(MUS_XEN_TO_CLM(obj), fm1)));
}



/* ---------------- rand, rand_interp ---------------- */

#define S_make_rand "make-rand"
#define S_rand "rand"
#define S_rand_p "rand?"
#define S_make_rand_interp "make-rand-interp"
#define S_rand_interp "rand-interp"
#define S_rand_interp_p "rand-interp?"
#define S_mus_set_rand_seed "mus-set-rand-seed"
#define S_mus_rand_seed "mus-rand-seed"
#define S_mus_random "mus-random"

static XEN g_make_noi(int rand_case, XEN arg1, XEN arg2, XEN arg3, XEN arg4)
{
  mus_xen *gn;
  mus_any *ge = NULL;
  XEN args[4]; XEN keys[2];
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
      freq = fkeyarg(keys[0], S_make_rand, orig_arg[0] + 1, freq);
      base = fkeyarg(keys[1], S_make_rand, orig_arg[1] + 1, base);
    }
  if (rand_case)
    ge = mus_make_rand(freq, base);
  else ge = mus_make_rand_interp(freq, base);
  if (ge)
    {
      gn = (mus_xen *)CALLOC(1, sizeof(mus_xen));
      gn->gen = ge;
      return(mus_xen_to_object(gn));
    }
  return(XEN_FALSE);
}

static XEN g_make_rand_interp(XEN arg1, XEN arg2, XEN arg3, XEN arg4)
{
  #define H_make_rand_interp "(" S_make_rand_interp " &opt-key (frequency 440.0) (amplitude 1.0))\n\
returns a new " S_rand_interp " generator, producing linearly interpolated random numbers. \
frequency is the rate at which new end-points are chosen."

  return(g_make_noi(FALSE, arg1, arg2, arg3, arg4));
}

static XEN g_make_rand(XEN arg1, XEN arg2, XEN arg3, XEN arg4)
{
  #define H_make_rand "(" S_make_rand " &opt-key (frequency 440.0) (amplitude 1.0))\n\
returns a new " S_rand " generator, producing a sequence of random numbers (a step  function). \
frequency is the rate at which new numbers are chosen."

  return(g_make_noi(TRUE, arg1, arg2, arg3, arg4));
}

static XEN g_rand(XEN obj, XEN fm)
{
  #define H_rand "(" S_rand " gen &optional (fm 0.0)) -> gen's current random number. \
fm can modulate the rate at which the current number is changed."

  Float fm1 = 0.0;
  XEN_ASSERT_TYPE((MUS_XEN_P(obj)) && (mus_rand_p(MUS_XEN_TO_CLM(obj))), obj, XEN_ARG_1, S_rand, " a rand gen");
  if (XEN_NUMBER_P(fm)) fm1 = XEN_TO_C_DOUBLE(fm); else XEN_ASSERT_TYPE(XEN_NOT_BOUND_P(fm), fm, XEN_ARG_2, S_rand, "a number");
  return(C_TO_XEN_DOUBLE(mus_rand(MUS_XEN_TO_CLM(obj), fm1)));
}

static XEN g_rand_p(XEN obj) 
{
  #define H_rand_p "(" S_rand_p " gen) -> #t if gen is a " S_rand " generator, else #f"
  return(C_TO_XEN_BOOLEAN((MUS_XEN_P(obj)) && (mus_rand_p(MUS_XEN_TO_CLM(obj)))));
}

static XEN g_rand_interp(XEN obj, XEN fm)
{
  #define H_rand_interp "(" S_rand_interp " gen &optional (fm 0.0)) -> gen's current (interpolating) random number. \
fm can modulate the rate at which new segment end-points are chosen."

  Float fm1 = 0.0;
  XEN_ASSERT_TYPE((MUS_XEN_P(obj)) && (mus_rand_interp_p(MUS_XEN_TO_CLM(obj))), obj, XEN_ARG_1, S_rand_interp, "a rand-interp gen");
  if (XEN_NUMBER_P(fm)) fm1 = XEN_TO_C_DOUBLE(fm); else XEN_ASSERT_TYPE(XEN_NOT_BOUND_P(fm), fm, XEN_ARG_2, S_rand_interp, "a number");
  return(C_TO_XEN_DOUBLE(mus_rand_interp(MUS_XEN_TO_CLM(obj), fm1)));
}

static XEN g_rand_interp_p(XEN obj) 
{
  #define H_rand_interp_p "(" S_rand_interp_p " gen) -> #t if gen is a " S_rand_interp " generator, else #f"
  return(C_TO_XEN_BOOLEAN((MUS_XEN_P(obj)) && (mus_rand_interp_p(MUS_XEN_TO_CLM(obj)))));
}

static XEN g_mus_random(XEN a) 
{
  #define H_mus_random "(" S_mus_random " val) -> a random number between -val and val. \
the built-in 'random' function returns values between 0 and its argument"

  XEN_ASSERT_TYPE(XEN_NUMBER_P(a), a, XEN_ONLY_ARG, S_mus_random, "a number");
  return(C_TO_XEN_DOUBLE(mus_random(XEN_TO_C_DOUBLE(a))));
}

static XEN g_rand_seed(void) {return(C_TO_XEN_ULONG(mus_rand_seed()));}
static XEN g_set_rand_seed(XEN a) 
{
  #define H_mus_set_rand_seed "(" S_mus_set_rand_seed " val) sets the random number seed, \
this can be used to re-run a particular random number sequence."

  XEN_ASSERT_TYPE(XEN_NUMBER_P(a), a, XEN_ONLY_ARG, S_mus_set_rand_seed, "an integer");
  mus_set_rand_seed(XEN_TO_C_ULONG(a)); 
  return(a);
}



/* ---------------- table lookup ---------------- */

static int DEFAULT_TABLE_SIZE = 512;

#define S_table_lookup_p     "table-lookup?"
#define S_make_table_lookup  "make-table-lookup"
#define S_table_lookup       "table-lookup"
#define S_partials2wave      "partials->wave"
#define S_phasepartials2wave "phase-partials->wave"

static XEN g_table_lookup_p(XEN obj) 
{
  #define H_table_lookup_p "(" S_table_lookup_p " gen) -> #t if gen is a " S_table_lookup " generator, else #f"
  return(C_TO_XEN_BOOLEAN((MUS_XEN_P(obj)) && (mus_table_lookup_p(MUS_XEN_TO_CLM(obj)))));
}

static XEN g_partials2wave(XEN partials, XEN utable, XEN normalize)
{
  #define H_partials2wave "(" S_partials2wave " partials &optional wave normalize)\n\
takes a list of partials (harmonic number and associated amplitude) and produces \
a waveform for use in " S_table_lookup ".  If wave (a vct object) is not given, \
a new one is created.  If normalize is #t, the resulting waveform goes between -1 and 1.\n\
  (set! gen (make-table-lookup 440.0 :wave (partials->wave '(1 1.0 2 .5))))"

  vct *f;
  XEN table; XEN lst;
  Float *partial_data, *wave;
  int len, i;
  XEN_ASSERT_TYPE(XEN_LIST_P_WITH_LENGTH(partials, len), partials, XEN_ARG_1, S_partials2wave, "a list");
  XEN_ASSERT_TYPE(VCT_P(utable) || XEN_FALSE_P(utable) || (!(XEN_BOUND_P(utable))), utable, XEN_ARG_2, S_partials2wave, "a vct or #f");
  XEN_ASSERT_TYPE(XEN_BOOLEAN_IF_BOUND_P(normalize), normalize, XEN_ARG_3, S_partials2wave, "a boolean");
  if (len == 0)
    mus_misc_error(S_partials2wave, "partials list empty?", partials);
  if ((XEN_NOT_BOUND_P(utable)) || (!(VCT_P(utable))))
    {
      wave = (Float *)CALLOC(DEFAULT_TABLE_SIZE, sizeof(Float));
      table = make_vct(DEFAULT_TABLE_SIZE, wave);
    }
  else table = utable;
  f = TO_VCT(table);
  partial_data = (Float *)CALLOC(len, sizeof(Float));
  for (i = 0, lst = XEN_COPY_ARG(partials); i < len; i++, lst = XEN_CDR(lst)) 
    partial_data[i] = XEN_TO_C_DOUBLE(XEN_CAR(lst));
  mus_partials2wave(partial_data, len / 2, f->data, f->length, (XEN_TRUE_P(normalize)));
  FREE(partial_data);
  return(table);
}

static XEN g_phasepartials2wave(XEN partials, XEN utable, XEN normalize)
{
  vct *f;
  XEN table; XEN lst;
  Float *partial_data, *wave;
  int len, i;

  #define H_phasepartials2wave "(" S_phasepartials2wave " partials &optional wave normalize)\n\
takes a list of partials (harmonic number, amplitude, initial phase) and produces \
a waveform for use in " S_table_lookup ".  If wave (a vct object) is not given, \
a new one is created.  If normalize is #t, the resulting waveform goes between -1 and 1.\n\
  (set! gen (make-table-lookup 440.0 :wave (phase-partials->wave (list 1 .75 0.0 2 .25 (* pi .5)))))"

  XEN_ASSERT_TYPE(XEN_LIST_P_WITH_LENGTH(partials, len), partials, XEN_ARG_1, S_phasepartials2wave, "a list");
  XEN_ASSERT_TYPE(VCT_P(utable) || XEN_FALSE_P(utable) || (!(XEN_BOUND_P(utable))), utable, XEN_ARG_2, S_phasepartials2wave, "a vct or #f");
  XEN_ASSERT_TYPE(XEN_BOOLEAN_IF_BOUND_P(normalize), normalize, XEN_ARG_3, S_phasepartials2wave, "a boolean");
  if (len == 0)
    mus_misc_error(S_partials2wave, "partials list empty?", partials);
  if ((XEN_NOT_BOUND_P(utable)) || (!(VCT_P(utable))))
    {
      wave = (Float *)CALLOC(DEFAULT_TABLE_SIZE, sizeof(Float));
      table = make_vct(DEFAULT_TABLE_SIZE, wave);
    }
  else table = utable;
  f = TO_VCT(table);
  partial_data = (Float *)CALLOC(len, sizeof(Float));
  for (i = 0, lst = XEN_COPY_ARG(partials); i < len; i++, lst = XEN_CDR(lst)) 
    partial_data[i] = XEN_TO_C_DOUBLE(XEN_CAR(lst));
  mus_phasepartials2wave(partial_data, len / 3, f->data, f->length, (XEN_TRUE_P(normalize)));
  FREE(partial_data);
  return(table);
}

static XEN g_make_table_lookup (XEN arg1, XEN arg2, XEN arg3, XEN arg4, XEN arg5, XEN arg6)
{
  #define H_make_table_lookup "(" S_make_table_lookup " &opt-key (frequency 440.0) (initial-phase 0.0) wave)\n\
returns a new " S_table_lookup " generator.  This is known as an oscillator in other synthesis systems. \
The default table size is 512; to use some other size, pass your own vct object as the 'wave'.\n\
   (set! gen (make-table-lookup 440.0 :wave (partials->wave '(1 1.0)))\n\
is the same in effect as " S_make_oscil "."

  mus_xen *gn;
  mus_any *ge;
  int vals, table_size = DEFAULT_TABLE_SIZE, need_free = FALSE;
  XEN args[6]; XEN keys[3];
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
      freq = fkeyarg(keys[0], S_make_table_lookup, orig_arg[0] + 1, freq);
      phase = fkeyarg(keys[1], S_make_table_lookup, orig_arg[1] + 1, phase);
      if (!(XEN_KEYWORD_P(keys[2])))
	{
	  XEN_ASSERT_TYPE(VCT_P(keys[2]), keys[2], orig_arg[2] + 1, S_make_table_lookup, "a vct");
	  v = TO_VCT(keys[2]);
	  table = copy_vct_data(v);
	  table_size = v->length;
	}
    }
  if (table == NULL) 
    {
      table = (Float *)CALLOC(table_size, sizeof(Float));
      need_free = TRUE;
    }
  old_error_handler = mus_error_set_handler(local_mus_error); /* currently not needed (no recoverable errors from mus_make_table_lookup) */
  ge = mus_make_table_lookup(freq, phase, table, table_size);
  mus_error_set_handler(old_error_handler);
  if (ge)
    {
      gn = (mus_xen *)CALLOC(1, sizeof(mus_xen));
      gn->vcts = make_vcts(1);
      gn->nvcts = 1;
      gn->gen = ge;
      return(mus_xen_to_object_with_vct(gn, make_vct(table_size, table)));
    }
  if (need_free) FREE(table);
  mus_error(local_error_type, local_error_msg);
  return(XEN_FALSE);
}

static XEN g_table_lookup (XEN obj, XEN fm) 
{
  #define H_table_lookup "(" S_table_lookup " gen &optional (fm 0.0)) performs interpolated table-lookup \
with 'wrap-around' when gen's phase marches off the end of its table."

  Float fm1 = 0.0;
  XEN_ASSERT_TYPE((MUS_XEN_P(obj)) && (mus_table_lookup_p(MUS_XEN_TO_CLM(obj))), obj, XEN_ARG_1, S_table_lookup, "a table-lookup gen");
  if (XEN_NUMBER_P(fm)) fm1 = XEN_TO_C_DOUBLE(fm);  else XEN_ASSERT_TYPE(XEN_NOT_BOUND_P(fm), fm, XEN_ARG_2, S_table_lookup, "a number");
  return(C_TO_XEN_DOUBLE(mus_table_lookup(MUS_XEN_TO_CLM(obj), fm1)));
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

static XEN g_make_sw(int type, Float def_phase, XEN arg1, XEN arg2, XEN arg3, XEN arg4, XEN arg5, XEN arg6)
{
  mus_xen *gn;
  mus_any *ge = NULL;
  char *caller = NULL;
  XEN args[6]; XEN keys[3];
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
      freq = fkeyarg(keys[0], caller, orig_arg[0] + 1, freq);
      base = fkeyarg(keys[1], caller, orig_arg[1] + 1, base);
      phase = fkeyarg(keys[2], caller, orig_arg[2] + 1, phase);
    }
  switch (type)
    {
    case G_SAWTOOTH_WAVE: ge = mus_make_sawtooth_wave(freq, base, phase); break;
    case G_SQUARE_WAVE: ge = mus_make_square_wave(freq, base, phase); break;
    case G_TRIANGLE_WAVE: ge = mus_make_triangle_wave(freq, base, phase); break;
    case G_PULSE_TRAIN: ge = mus_make_pulse_train(freq, base, phase); break;
    }
  if (ge)
    {
      gn = (mus_xen *)CALLOC(1, sizeof(mus_xen));
      gn->gen = ge;
      return(mus_xen_to_object(gn));
    }
  return(XEN_FALSE);
}

static XEN g_make_sawtooth_wave(XEN arg1, XEN arg2, XEN arg3, XEN arg4, XEN arg5, XEN arg6) 
{
  #define H_make_sawtooth_wave "(" S_make_sawtooth_wave " &opt-key (frequency 440.0) (amplitude 1.0) (initial-phase 0.0))\n\
returns a new " S_sawtooth_wave " generator."

  return(g_make_sw(G_SAWTOOTH_WAVE, M_PI, arg1, arg2, arg3, arg4, arg5, arg6));
}

static XEN g_make_square_wave(XEN arg1, XEN arg2, XEN arg3, XEN arg4, XEN arg5, XEN arg6) 
{
  #define H_make_square_wave "(" S_make_square_wave " &opt-key (frequency 440.0) (amplitude 1.0) (initial-phase 0.0))\n\
returns a new " S_square_wave " generator."

  return(g_make_sw(G_SQUARE_WAVE, 0.0, arg1, arg2, arg3, arg4, arg5, arg6));
}

static XEN g_make_triangle_wave(XEN arg1, XEN arg2, XEN arg3, XEN arg4, XEN arg5, XEN arg6) 
{
  #define H_make_triangle_wave "(" S_make_triangle_wave " &opt-key (frequency 440.0) (amplitude 1.0) (initial-phase 0.0))\n\
returns a new " S_triangle_wave " generator."

  return(g_make_sw(G_TRIANGLE_WAVE, 0.0, arg1, arg2, arg3, arg4, arg5, arg6));
}

static XEN g_make_pulse_train(XEN arg1, XEN arg2, XEN arg3, XEN arg4, XEN arg5, XEN arg6) 
{
  #define H_make_pulse_train "(" S_make_pulse_train " &opt-key (frequency 440.0) (amplitude 1.0) (initial-phase 0.0))\n\
returns a new " S_pulse_train " generator.  This produces a sequence of impulses."

  return(g_make_sw(G_PULSE_TRAIN, TWO_PI, arg1, arg2, arg3, arg4, arg5, arg6));
}

static XEN g_sawtooth_wave(XEN obj, XEN fm) 
{
  #define H_sawtooth_wave "(" S_sawtooth_wave " gen &optional (fm 0.0)) -> next sawtooth sample from gen"
  Float fm1 = 0.0;
  XEN_ASSERT_TYPE((MUS_XEN_P(obj)) && (mus_sawtooth_wave_p(MUS_XEN_TO_CLM(obj))), obj, XEN_ARG_1, S_sawtooth_wave, "a sawtooth-wave gen");
  if (XEN_NUMBER_P(fm)) fm1 = XEN_TO_C_DOUBLE(fm); else XEN_ASSERT_TYPE(XEN_NOT_BOUND_P(fm), fm, XEN_ARG_2, S_sawtooth_wave, "a number");
  return(C_TO_XEN_DOUBLE(mus_sawtooth_wave(MUS_XEN_TO_CLM(obj), fm1)));
}

static XEN g_square_wave(XEN obj, XEN fm) 
{
  #define H_square_wave "(" S_square_wave " gen &optional (fm 0.0)) -> next square wave sample from gen"
  Float fm1 = 0.0;
  XEN_ASSERT_TYPE((MUS_XEN_P(obj)) && (mus_square_wave_p(MUS_XEN_TO_CLM(obj))), obj, XEN_ARG_1, S_square_wave, "a square-wave gen");
  if (XEN_NUMBER_P(fm)) fm1 = XEN_TO_C_DOUBLE(fm); else XEN_ASSERT_TYPE(XEN_NOT_BOUND_P(fm), fm, XEN_ARG_2, S_square_wave, "a number");
  return(C_TO_XEN_DOUBLE(mus_square_wave(MUS_XEN_TO_CLM(obj), fm1)));
}

static XEN g_triangle_wave(XEN obj, XEN fm) 
{
  #define H_triangle_wave "(" S_triangle_wave " gen &optional (fm 0.0)) -> next triangle wave sample from gen"
  Float fm1 = 0.0;
  XEN_ASSERT_TYPE((MUS_XEN_P(obj)) && (mus_triangle_wave_p(MUS_XEN_TO_CLM(obj))), obj, XEN_ARG_1, S_triangle_wave, "a triangle-wave gen");
  if (XEN_NUMBER_P(fm)) fm1 = XEN_TO_C_DOUBLE(fm); else XEN_ASSERT_TYPE(XEN_NOT_BOUND_P(fm), fm, XEN_ARG_2, S_triangle_wave, "a number");
  return(C_TO_XEN_DOUBLE(mus_triangle_wave(MUS_XEN_TO_CLM(obj), fm1)));
}

static XEN g_pulse_train(XEN obj, XEN fm) 
{
  #define H_pulse_train "(" S_pulse_train " gen &optional (fm 0.0)) -> next (im)pulse train sample from gen"
  Float fm1 = 0.0;
  XEN_ASSERT_TYPE((MUS_XEN_P(obj)) && (mus_pulse_train_p(MUS_XEN_TO_CLM(obj))), obj, XEN_ARG_1, S_pulse_train, "a pulse-train gen");
  if (XEN_NUMBER_P(fm)) fm1 = XEN_TO_C_DOUBLE(fm); else XEN_ASSERT_TYPE(XEN_NOT_BOUND_P(fm), fm, XEN_ARG_2, S_pulse_train, "a number");
  return(C_TO_XEN_DOUBLE(mus_pulse_train(MUS_XEN_TO_CLM(obj), fm1)));
}

static XEN g_sawtooth_wave_p(XEN obj) 
{
  #define H_sawtooth_wave_p "(" S_sawtooth_wave_p " gen) -> #t if gen is a " S_sawtooth_wave " generator, else #f"
  return(C_TO_XEN_BOOLEAN((MUS_XEN_P(obj)) && (mus_sawtooth_wave_p(MUS_XEN_TO_CLM(obj)))));
}

static XEN g_square_wave_p(XEN obj) 
{
  #define H_square_wave_p "(" S_square_wave_p " gen) -> #t if gen is a " S_square_wave " generator, else #f"
  return(C_TO_XEN_BOOLEAN((MUS_XEN_P(obj)) && (mus_square_wave_p(MUS_XEN_TO_CLM(obj)))));
}

static XEN g_triangle_wave_p(XEN obj) 
{
  #define H_triangle_wave_p "(" S_triangle_wave_p " gen) -> #t if gen is a " S_triangle_wave " generator, else #f"
  return(C_TO_XEN_BOOLEAN((MUS_XEN_P(obj)) && (mus_triangle_wave_p(MUS_XEN_TO_CLM(obj)))));
}

static XEN g_pulse_train_p(XEN obj) 
{
  #define H_pulse_train_p "(" S_pulse_train_p " gen) -> #t if gen is a " S_pulse_train " generator, else #f"
  return(C_TO_XEN_BOOLEAN((MUS_XEN_P(obj)) && (mus_pulse_train_p(MUS_XEN_TO_CLM(obj)))));
}



/* ---------------- asymmetric-fm ---------------- */

#define S_make_asymmetric_fm "make-asymmetric-fm"
#define S_asymmetric_fm "asymmetric-fm"
#define S_asymmetric_fm_p "asymmetric-fm?"

static XEN g_make_asymmetric_fm(XEN arg1, XEN arg2, XEN arg3, XEN arg4, XEN arg5, XEN arg6, XEN arg7, XEN arg8)
{
  #define H_make_asymmetric_fm "(" S_make_asymmetric_fm " &opt-key (frequency 440.0) (initial-phase 0.0) (r 1.0) (ratio 1.0))\n\
returns a new " S_asymmetric_fm " generator."

  mus_xen *gn;
  mus_any *ge;
  XEN args[8]; XEN keys[4];
  int orig_arg[4] = {0, 0, 0, 0};
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
      freq = fkeyarg(keys[0], S_make_asymmetric_fm, orig_arg[0] + 1, freq);
      phase = fkeyarg(keys[1], S_make_asymmetric_fm, orig_arg[1] + 1, phase);
      r = fkeyarg(keys[2], S_make_asymmetric_fm, orig_arg[2] + 1, r);
      ratio = fkeyarg(keys[3], S_make_asymmetric_fm, orig_arg[3] + 1, ratio);
    }
  ge = mus_make_asymmetric_fm(freq, phase, r, ratio);
  if (ge)
    {
      gn = (mus_xen *)CALLOC(1, sizeof(mus_xen));
      gn->gen = ge;
      return(mus_xen_to_object(gn));
    }
  return(XEN_FALSE);
}

static XEN g_asymmetric_fm(XEN obj, XEN index, XEN fm)
{
  #define H_asymmetric_fm "(" S_asymmetric_fm " gen &optional (index 0.0) (fm 0.0)) -> next sample from asymmetric fm gen"
  Float fm1 = 0.0, index1 = 0.0;
  XEN_ASSERT_TYPE((MUS_XEN_P(obj)) && (mus_asymmetric_fm_p(MUS_XEN_TO_CLM(obj))), obj, XEN_ARG_1, S_asymmetric_fm, "an asymmetric-fm gen");
  if (XEN_NUMBER_P(fm)) fm1 = XEN_TO_C_DOUBLE(fm); else XEN_ASSERT_TYPE(XEN_NOT_BOUND_P(fm), fm, XEN_ARG_2, S_asymmetric_fm, "a number");
  if (XEN_NUMBER_P(index)) index1 = XEN_TO_C_DOUBLE(index); else XEN_ASSERT_TYPE(XEN_NOT_BOUND_P(index), index, XEN_ARG_3, S_asymmetric_fm, "a number");
  return(C_TO_XEN_DOUBLE(mus_asymmetric_fm(MUS_XEN_TO_CLM(obj), index1, fm1)));
}

static XEN g_asymmetric_fm_p(XEN obj) 
{
  #define H_asymmetric_fm_p "(" S_asymmetric_fm_p " gen) -> #t if gen is a " S_asymmetric_fm " generator, else #f"
  return(C_TO_XEN_BOOLEAN((MUS_XEN_P(obj)) && (mus_asymmetric_fm_p(MUS_XEN_TO_CLM(obj)))));
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

static XEN g_make_smpflt_1(int choice, XEN arg1, XEN arg2, XEN arg3, XEN arg4)
{
  mus_xen *gn;
  mus_any *gen = NULL;
  XEN args[4]; XEN keys[2];
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
      a0 = fkeyarg(keys[0], smpflts[choice], orig_arg[0] + 1, a0);
      a1 = fkeyarg(keys[1], smpflts[choice], orig_arg[1] + 1, a1);
    }
  switch (choice)
    {
    case G_ONE_ZERO: gen = mus_make_one_zero(a0, a1); break;
    case G_ONE_POLE: gen = mus_make_one_pole(a0, a1); break;
    case G_ZPOLAR: gen = mus_make_zpolar(a0, a1); break;
    case G_PPOLAR: gen = mus_make_ppolar(a0, a1); break;
    }
  if (gen)
    {
      gn = (mus_xen *)CALLOC(1, sizeof(mus_xen));
      gn->gen = gen; /* delayed to here in case mus_error in mus_make_<whatever> above */
      return(mus_xen_to_object(gn));
    }
  return(XEN_FALSE);
}

static XEN g_make_one_zero(XEN arg1, XEN arg2, XEN arg3, XEN arg4)
{
  #define H_make_one_zero "(" S_make_one_zero " a0 a1) -> new " S_one_zero " filter returning a0*x(n) + a1*x(n-1)"
  return(g_make_smpflt_1(G_ONE_ZERO, arg1, arg2, arg3, arg4));
}

static XEN g_make_one_pole(XEN arg1, XEN arg2, XEN arg3, XEN arg4)
{
  #define H_make_one_pole "(" S_make_one_pole " a0 b1) -> new " S_one_pole " filter returning a0*x(n) - b1*y(n-1)"
  return(g_make_smpflt_1(G_ONE_POLE, arg1, arg2, arg3, arg4));
}

static XEN g_make_zpolar(XEN arg1, XEN arg2, XEN arg3, XEN arg4)
{
  #define H_make_zpolar "(" S_make_zpolar " radius frequency) -> new " S_two_zero " filter \
where the coefficients (a0..a2) are set from the desired zero's radius and center frequency. \
Use this in conjunction with the " S_two_zero " generator" 

  return(g_make_smpflt_1(G_ZPOLAR, arg1, arg2, arg3, arg4));
}

static XEN g_make_ppolar(XEN arg1, XEN arg2, XEN arg3, XEN arg4)
{
  #define H_make_ppolar "(" S_make_ppolar " radius frequency) -> new " S_two_pole " filter \
where the coefficients are set from the desired pole's radius and center frequency. \
Use this in conjunction with the " S_two_pole " generator" 

  return(g_make_smpflt_1(G_PPOLAR, arg1, arg2, arg3, arg4));
}

static XEN g_make_smpflt_2(int choice, XEN arg1, XEN arg2, XEN arg3, XEN arg4, XEN arg5, XEN arg6)
{
  mus_xen *gn;
  mus_any *gen = NULL;
  XEN args[6]; XEN keys[3];
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
      a0 = fkeyarg(keys[0], smpflts[choice], orig_arg[0] + 1, a0);
      a1 = fkeyarg(keys[1], smpflts[choice], orig_arg[1] + 1, a1);
      a2 = fkeyarg(keys[2], smpflts[choice], orig_arg[2] + 1, a2);
    }
  if (choice == G_TWO_ZERO)
    gen = mus_make_two_zero(a0, a1, a2);
  else gen = mus_make_two_pole(a0, a1, a2);
  if (gen)
    {
      gn = (mus_xen *)CALLOC(1, sizeof(mus_xen));
      gn->gen = gen;  /* delayed in case of mus_error in make_two_pole */
      return(mus_xen_to_object(gn));
    }
  return(XEN_FALSE);
}

static XEN g_make_two_zero(XEN arg1, XEN arg2, XEN arg3, XEN arg4, XEN arg5, XEN arg6) 
{
  #define H_make_two_zero "(" S_make_two_zero " a0 a1 a2) -> new " S_two_zero " filter \
returning a0*x(n) + a1*x(n-1) + a2*x(n-2)"

  return(g_make_smpflt_2(G_TWO_ZERO, arg1, arg2, arg3, arg4, arg5, arg6));
}

static XEN g_make_two_pole(XEN arg1, XEN arg2, XEN arg3, XEN arg4, XEN arg5, XEN arg6) 
{
  #define H_make_two_pole "(" S_make_two_pole " a0 b1 b2) -> new " S_two_pole " filter \
returning a0*x(n) - b1*y(n-1) - b2*y(n-2)"

  return(g_make_smpflt_2(G_TWO_POLE, arg1, arg2, arg3, arg4, arg5, arg6));
}

static XEN g_one_zero(XEN obj, XEN fm)
{
  #define H_one_zero "(" S_one_zero " gen &optional (input 0.0)) -> one zero filter of input"
  Float fm1 = 0.0;
  XEN_ASSERT_TYPE((MUS_XEN_P(obj)) && (mus_one_zero_p(MUS_XEN_TO_CLM(obj))), obj, XEN_ARG_1, S_one_zero, "a one-zero filter");
  if (XEN_NUMBER_P(fm)) fm1 = XEN_TO_C_DOUBLE(fm); else XEN_ASSERT_TYPE(XEN_NOT_BOUND_P(fm), fm, XEN_ARG_2, S_one_zero, "a number");
  return(C_TO_XEN_DOUBLE(mus_one_zero(MUS_XEN_TO_CLM(obj), fm1)));
}

static XEN g_one_pole(XEN obj, XEN fm)
{
  #define H_one_pole "(" S_one_pole " gen &optional (input 0.0)) -> one pole filter of input"
  Float fm1 = 0.0;
  XEN_ASSERT_TYPE((MUS_XEN_P(obj)) && (mus_one_pole_p(MUS_XEN_TO_CLM(obj))), obj, XEN_ARG_1, S_one_pole, "a one-pole filter");
  if (XEN_NUMBER_P(fm)) fm1 = XEN_TO_C_DOUBLE(fm); else XEN_ASSERT_TYPE(XEN_NOT_BOUND_P(fm), fm, XEN_ARG_2, S_one_pole, "a number");
  return(C_TO_XEN_DOUBLE(mus_one_pole(MUS_XEN_TO_CLM(obj), fm1)));
}

static XEN g_two_zero(XEN obj, XEN fm)
{
  #define H_two_zero "(" S_two_zero " gen &optional (input 0.0)) -> two zero filter of input"
  Float fm1 = 0.0;
  XEN_ASSERT_TYPE((MUS_XEN_P(obj)) && (mus_two_zero_p(MUS_XEN_TO_CLM(obj))), obj, XEN_ARG_1, S_two_zero, "a two-zero filter");
  if (XEN_NUMBER_P(fm)) fm1 = XEN_TO_C_DOUBLE(fm); else XEN_ASSERT_TYPE(XEN_NOT_BOUND_P(fm), fm, XEN_ARG_2, S_two_zero, "a number");
  return(C_TO_XEN_DOUBLE(mus_two_zero(MUS_XEN_TO_CLM(obj), fm1)));
}

static XEN g_two_pole(XEN obj, XEN fm)
{
  #define H_two_pole "(" S_two_pole " gen &optional (input 0.0)) -> two pole filter of input"
  Float fm1 = 0.0;
  XEN_ASSERT_TYPE((MUS_XEN_P(obj)) && (mus_two_pole_p(MUS_XEN_TO_CLM(obj))), obj, XEN_ARG_1, S_two_pole, "a two-pole filter");
  if (XEN_NUMBER_P(fm)) fm1 = XEN_TO_C_DOUBLE(fm); else XEN_ASSERT_TYPE(XEN_NOT_BOUND_P(fm), fm, XEN_ARG_2, S_two_pole, "a number");
  return(C_TO_XEN_DOUBLE(mus_two_pole(MUS_XEN_TO_CLM(obj), fm1)));
}

static XEN g_one_zero_p(XEN obj) 
{
  #define H_one_zero_p "(" S_one_zero_p " gen) -> #t if gen is a " S_one_zero " generator, else #f"
  return(C_TO_XEN_BOOLEAN((MUS_XEN_P(obj)) && (mus_one_zero_p(MUS_XEN_TO_CLM(obj)))));
}

static XEN g_one_pole_p(XEN obj) 
{
  #define H_one_pole_p "(" S_one_pole_p " gen) -> #t if gen is a " S_one_pole " generator, else #f"
  return(C_TO_XEN_BOOLEAN((MUS_XEN_P(obj)) && (mus_one_pole_p(MUS_XEN_TO_CLM(obj)))));
}

static XEN g_two_zero_p(XEN obj) 
{
  #define H_two_zero_p "(" S_two_zero_p " gen) -> #t if gen is a " S_two_zero " generator, else #f"
  return(C_TO_XEN_BOOLEAN((MUS_XEN_P(obj)) && (mus_two_zero_p(MUS_XEN_TO_CLM(obj)))));
}

static XEN g_two_pole_p(XEN obj) 
{
  #define H_two_pole_p "(" S_two_pole_p " gen) -> #t if gen is a " S_two_pole " generator, else #f"
  return(C_TO_XEN_BOOLEAN((MUS_XEN_P(obj)) && (mus_two_pole_p(MUS_XEN_TO_CLM(obj)))));
}

static XEN g_a1(XEN obj)
{
  #define H_mus_a1 "(" S_mus_a1 " gen) -> gen's " S_mus_a1 " coefficient (scaler on x(n-1)), if any"
  XEN_ASSERT_TYPE(MUS_XEN_P(obj), obj, XEN_ONLY_ARG, S_mus_a1, "a generator");
  return(C_TO_XEN_DOUBLE(mus_a1(MUS_XEN_TO_CLM(obj))));
}

static XEN g_a2(XEN obj)
{
  #define H_mus_a2 "(" S_mus_a2 " gen) -> gen's " S_mus_a2 " coefficient (scaler on x(n-2)), if any"
  XEN_ASSERT_TYPE(MUS_XEN_P(obj), obj, XEN_ONLY_ARG, S_mus_a2, "a generator");
  return(C_TO_XEN_DOUBLE(mus_a2(MUS_XEN_TO_CLM(obj))));
}

static XEN g_b2(XEN obj)
{
  #define H_mus_b2 "(" S_mus_b2 " gen) -> gen's " S_mus_b2 " coefficient (scaler on y(n-2)), if any"
  XEN_ASSERT_TYPE(MUS_XEN_P(obj), obj, XEN_ONLY_ARG, S_mus_b2, "a generator");
  return(C_TO_XEN_DOUBLE(mus_b2(MUS_XEN_TO_CLM(obj))));
}

static XEN g_set_a1(XEN obj, XEN val)
{
  XEN_ASSERT_TYPE(MUS_XEN_P(obj), obj, XEN_ARG_1, S_mus_a1, "a generator");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ARG_2, S_mus_a1, "a number");
  return(C_TO_XEN_DOUBLE(mus_set_a1(MUS_XEN_TO_CLM(obj), XEN_TO_C_DOUBLE(val))));
}

static XEN g_set_a2(XEN obj, XEN val)
{
  XEN_ASSERT_TYPE(MUS_XEN_P(obj), obj, XEN_ARG_1, S_mus_a2, "a generator");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ARG_2, S_mus_a2, "a number");
  return(C_TO_XEN_DOUBLE(mus_set_a2(MUS_XEN_TO_CLM(obj), XEN_TO_C_DOUBLE(val))));
}

static XEN g_set_b2(XEN obj, XEN val)
{
  XEN_ASSERT_TYPE(MUS_XEN_P(obj), obj, XEN_ARG_1, S_mus_b2, "a generator");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ARG_2, S_mus_b2, "a number");
  return(C_TO_XEN_DOUBLE(mus_set_b2(MUS_XEN_TO_CLM(obj), XEN_TO_C_DOUBLE(val))));
}



/* ---------------- formant ---------------- */

#define S_make_formant "make-formant"
#define S_formant "formant"
#define S_formant_bank "formant-bank"
#define S_formant_p "formant?"
#define S_mus_formant_radius "mus-formant-radius"
#define S_mus_set_formant_radius "mus-set-formant-radius"
#define S_mus_set_formant_radius_and_frequency "mus-set-formant-radius-and-frequency"

static XEN g_make_formant(XEN arg1, XEN arg2, XEN arg3, XEN arg4, XEN arg5, XEN arg6)
{
  #define H_make_formant "(" S_make_formant " &opt-key radius frequency (gain 1.0))\n\
returns a new formant generator (a resonator).  radius sets the pole radius. \
frequency sets the resonance center frequency (Hz).  gain is an overall amplitude \
control."

  mus_xen *gn;
  mus_any *ge;
  int vals;
  XEN args[6]; XEN keys[3];
  int orig_arg[3] = {0, 0, 0};
  Float freq = 0.0, radius = 0.0, gain = 1.0;
  keys[0] = all_keys[C_radius];
  keys[1] = all_keys[C_frequency];
  keys[2] = all_keys[C_gain];
  args[0] = arg1; args[1] = arg2; args[2] = arg3; args[3] = arg4; args[4] = arg5; args[5] = arg6;
  vals = decode_keywords(S_make_formant, 3, keys, 6, args, orig_arg);
  if (vals > 0)
    {
      radius = fkeyarg(keys[0], S_make_formant, orig_arg[0] + 1, radius);
      freq = fkeyarg(keys[1], S_make_formant, orig_arg[1] + 1, freq);
      gain = fkeyarg(keys[2], S_make_formant, orig_arg[2] + 1, gain);
    }
  ge = mus_make_formant(radius, freq, gain);
  if (ge)
    {
      gn = (mus_xen *)CALLOC(1, sizeof(mus_xen));
      gn->gen = ge;
      gn->nvcts = 0;
      return(mus_xen_to_object(gn));
    }
  return(XEN_FALSE);
}

static XEN g_formant(XEN gen, XEN input)
{
  #define H_formant "(" S_formant " gen &optional (input 0.0)) -> next sample from resonator gen"
  Float in1 = 0.0;
  XEN_ASSERT_TYPE((MUS_XEN_P(gen) && (mus_formant_p(MUS_XEN_TO_CLM(gen)))), gen, XEN_ARG_1, S_formant, "a formant gen");
  if (XEN_NUMBER_P(input)) in1 = XEN_TO_C_DOUBLE(input); else XEN_ASSERT_TYPE(XEN_NOT_BOUND_P(input), input, XEN_ARG_2, S_formant, "a number");
  return(C_TO_XEN_DOUBLE(mus_formant(MUS_XEN_TO_CLM(gen), in1)));
}

static XEN g_formant_bank(XEN amps, XEN gens, XEN inp)
{
  #define H_formant_bank "(" S_formant_bank " scls gens inval) -> sum a bank of " S_formant "s: scls[i]*" S_formant "(gens[i], inval)"
  XEN_ASSERT_TYPE(XEN_VECTOR_P(gens), gens, XEN_ARG_2, S_formant_bank, "a vector of formants");
  return(g_mus_bank(gens, amps, inp, XEN_UNDEFINED));
}

static XEN g_formant_p(XEN os) 
{
  #define H_formant_p "(" S_formant_p " gen) -> #t if gen is a " S_formant " generator, else #f"
  return(C_TO_XEN_BOOLEAN((MUS_XEN_P(os)) && (mus_formant_p(MUS_XEN_TO_CLM(os)))));
}

static XEN g_set_formant_radius_and_frequency (XEN gen, XEN rad, XEN frq)
{
  #define H_mus_set_formant_radius_and_frequency  "(" S_mus_set_formant_radius_and_frequency  " gen radius frequency) sets (" S_formant " \
generator) gen's radius and frequency"

  XEN_ASSERT_TYPE((MUS_XEN_P(gen) && (mus_formant_p(MUS_XEN_TO_CLM(gen)))), gen, XEN_ARG_1, S_mus_set_formant_radius_and_frequency, "a formant gen");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(rad), rad, XEN_ARG_2, S_mus_set_formant_radius_and_frequency, "a number");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(frq), frq, XEN_ARG_3, S_mus_set_formant_radius_and_frequency, "a number");
  mus_set_formant_radius_and_frequency(MUS_XEN_TO_CLM(gen), XEN_TO_C_DOUBLE(rad), XEN_TO_C_DOUBLE(frq));
  return(rad);
}



/* ---------------- frame ---------------- */

#define S_make_frame       "make-frame"
#define S_frame_p          "frame?"
#define S_frame_add        "frame+"
#define S_frame_multiply   "frame*"
#define S_frame_ref        "frame-ref"
#define S_frame_set        "frame-set!"
#define S_frame            "frame"

static XEN g_make_frame(XEN arglist)
{
  #define H_make_frame "(" S_make_frame " chans val0 val1 ...) returns a new frame object \
with chans samples, each sample set from the trailing arguments (defaulting to 0.0):\n\
   (set! fr0 (make-frame 2 .1 .2))"

  /* make_empty_frame from first of arglist, then if more args, load vals */
  mus_xen *gn;
  mus_any *ge;
  mus_frame *fr;
  XEN cararg; XEN lst;
  int size = 0, i, len;
  XEN_ASSERT_TYPE((XEN_LIST_P_WITH_LENGTH(arglist, len)) && (len > 0), arglist, XEN_ARG_1, S_make_frame, "a list");
  cararg = XEN_CAR(arglist);
  XEN_ASSERT_TYPE(XEN_NUMBER_P(cararg), cararg, 1, S_make_frame, "a number");
  size = XEN_TO_C_INT_OR_ELSE(cararg, 0);
  if (len > (size + 1)) 
    mus_misc_error(S_make_frame, "extra trailing args?", arglist);
  if (size <= 0)
    mus_misc_error(S_make_frame, "size: ", C_TO_XEN_INT(size));
  ge = (mus_any *)mus_make_empty_frame(size);
  if (ge)
    {
      gn = (mus_xen *)CALLOC(1, sizeof(mus_xen));
      gn->gen = ge;
      if (len > 1)
	{
	  fr = (mus_frame *)(gn->gen);
	  for (i = 1, lst = XEN_CDR(XEN_COPY_ARG(arglist)); i < len; i++, lst = XEN_CDR(lst))
	    if (XEN_NUMBER_P(XEN_CAR(lst)))
	      fr->vals[i - 1] = XEN_TO_C_DOUBLE(XEN_CAR(lst));
	    else
	      {
		mus_free(gn->gen);
		FREE(gn);
		mus_misc_error(S_make_frame, "invalid arg:", XEN_CAR(lst));
	      }
	}
      return(mus_xen_to_object(gn));
    }
  return(XEN_FALSE);
}

static XEN g_frame_p(XEN obj) 
{
  #define H_frame_p "(" S_frame_p " gen) -> #t if gen is a " S_frame " object, else #f"
  return(C_TO_XEN_BOOLEAN((MUS_XEN_P(obj)) && (mus_frame_p(MUS_XEN_TO_CLM(obj)))));
}

static XEN g_wrap_frame(mus_frame *val, int dealloc)
{
  mus_xen *gn;
  gn = (mus_xen *)CALLOC(1, sizeof(mus_xen));
  gn->gen = (mus_any *)val;
  gn->nvcts = dealloc;           /* free_mus_xen checks this before deallocating */
  return(mus_xen_to_object(gn));
}

static XEN g_frame_add(XEN uf1, XEN uf2, XEN ures) /* optional res */
{
  #define H_frame_add "(" S_frame_add " f1 f2 &optional outf) adds f1 and f2 returning outf; \
if outf is not given, a new frame is created. outf[i] = f1[i] + f2[i]"

  mus_frame *res = NULL;
  XEN_ASSERT_TYPE((MUS_XEN_P(uf1)) && (mus_frame_p(MUS_XEN_TO_CLM(uf1))), uf1, XEN_ARG_1, S_frame_add, "a frame");
  XEN_ASSERT_TYPE((MUS_XEN_P(uf2)) && (mus_frame_p(MUS_XEN_TO_CLM(uf2))), uf2, XEN_ARG_2, S_frame_add, "a frame");
  if ((MUS_XEN_P(ures)) && 
      (mus_frame_p(MUS_XEN_TO_CLM(ures)))) 
    res = (mus_frame *)MUS_XEN_TO_CLM(ures);
  return(g_wrap_frame(mus_frame_add((mus_frame *)MUS_XEN_TO_CLM(uf1),
				    (mus_frame *)MUS_XEN_TO_CLM(uf2),
				    res),
		      (res) ? DONT_FREE_FRAME : FREE_FRAME));
}

static XEN g_frame_multiply(XEN uf1, XEN uf2, XEN ures) /* optional res */
{
  #define H_frame_multiply "(" S_frame_multiply " f1 f2 &optional outf) multiplies f1 and f2 returning outf; \
if outf is not given, a new frame is created. outf[i] = f1[i] * f2[i]."

  mus_frame *res = NULL;
  XEN_ASSERT_TYPE((MUS_XEN_P(uf1)) && (mus_frame_p(MUS_XEN_TO_CLM(uf1))), uf1, XEN_ARG_1, S_frame_multiply, "a frame");
  XEN_ASSERT_TYPE((MUS_XEN_P(uf2)) && (mus_frame_p(MUS_XEN_TO_CLM(uf2))), uf2, XEN_ARG_2, S_frame_multiply, "a frame");
  if ((MUS_XEN_P(ures)) && 
      (mus_frame_p(MUS_XEN_TO_CLM(ures)))) 
    res = (mus_frame *)MUS_XEN_TO_CLM(ures);
  return(g_wrap_frame(mus_frame_multiply((mus_frame *)MUS_XEN_TO_CLM(uf1),
					 (mus_frame *)MUS_XEN_TO_CLM(uf2),
					 res),
		      (res) ? DONT_FREE_FRAME : FREE_FRAME));
}

static XEN g_frame_ref(XEN uf1, XEN uchan)
{
  #define H_frame_ref "(" S_frame_ref " f chan) -> f[chan] (the chan-th sample in frame f"
  XEN_ASSERT_TYPE((MUS_XEN_P(uf1)) && (mus_frame_p(MUS_XEN_TO_CLM(uf1))), uf1, XEN_ARG_1, S_frame_ref, "a frame");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(uchan), uchan, XEN_ARG_2, S_frame_ref, "an integer");
  return(C_TO_XEN_DOUBLE(mus_frame_ref((mus_frame *)MUS_XEN_TO_CLM(uf1), XEN_TO_C_INT(uchan))));
}

static XEN g_set_frame_ref(XEN uf1, XEN uchan, XEN val)
{
  #define H_frame_set "(" S_frame_set " f chan val) sets frame f's chan-th sample to val: f[chan] = val"
  XEN_ASSERT_TYPE((MUS_XEN_P(uf1)) && (mus_frame_p(MUS_XEN_TO_CLM(uf1))), uf1, XEN_ARG_1, S_frame_set, "a frame");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(uchan), uchan, XEN_ARG_2, S_frame_set, "an integer");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ARG_3, S_frame_set, "a number");
  return(C_TO_XEN_DOUBLE(mus_frame_set((mus_frame *)MUS_XEN_TO_CLM(uf1), XEN_TO_C_INT(uchan), XEN_TO_C_DOUBLE(val))));
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

static XEN g_mixer_p(XEN obj) 
{
  #define H_mixer_p "(" S_mixer_p " gen) -> #t if gen is a " S_mixer " object, else #f"
  return(C_TO_XEN_BOOLEAN((MUS_XEN_P(obj)) && (mus_mixer_p(MUS_XEN_TO_CLM(obj)))));
}

static XEN g_mixer_ref(XEN uf1, XEN in, XEN out)
{
  #define H_mixer_ref "(" S_mixer_ref " m in out) -> m[in, out], the mixer coefficient at location (in, out)"
  XEN_ASSERT_TYPE((MUS_XEN_P(uf1)) && (mus_mixer_p(MUS_XEN_TO_CLM(uf1))), uf1, XEN_ARG_1, S_mixer_ref, "a mixer");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(in), in, XEN_ARG_2, S_mixer_ref, "an integer");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(out), out, XEN_ARG_3, S_mixer_ref, "an integer");
  return(C_TO_XEN_DOUBLE(mus_mixer_ref((mus_mixer *)MUS_XEN_TO_CLM(uf1),
				       XEN_TO_C_INT(in),
				       XEN_TO_C_INT(out))));
}

static XEN g_set_mixer_ref(XEN uf1, XEN in, XEN out, XEN val)
{
  #define H_mixer_set "(" S_mixer_set " m in out val) sets m[in, out] = val"
  XEN_ASSERT_TYPE((MUS_XEN_P(uf1)) && (mus_mixer_p(MUS_XEN_TO_CLM(uf1))), uf1, XEN_ARG_1, S_mixer_set, "a mixer");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(in), in, XEN_ARG_2, S_mixer_set, "an integer");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(out), out, XEN_ARG_2, S_mixer_set, "an integer");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ARG_4, S_mixer_set, "a number");
  return(C_TO_XEN_DOUBLE(mus_mixer_set((mus_mixer *)MUS_XEN_TO_CLM(uf1),
				       XEN_TO_C_INT(in),
				       XEN_TO_C_INT(out),
				       XEN_TO_C_DOUBLE(val))));
}

#define DONT_FREE_MIXER -1
#define FREE_MIXER 1

static XEN g_wrap_mixer(mus_mixer *val, int dealloc)
{
  mus_xen *gn;
  gn = (mus_xen *)CALLOC(1, sizeof(mus_xen));
  gn->gen = (mus_any *)val;
  gn->nvcts = dealloc;
  return(mus_xen_to_object(gn));
}

static XEN g_mixer_multiply(XEN uf1, XEN uf2, XEN ures) /* optional res */
{
  #define H_mixer_multiply "(" S_mixer_multiply " m1 m2 &optional outm) multiplies mixers m1 and m2 \
(a matrix multiply), returning the mixer outm, or creating a new mixer if outm is not given."

  mus_mixer *res = NULL;
  XEN_ASSERT_TYPE((MUS_XEN_P(uf1)) && (mus_mixer_p(MUS_XEN_TO_CLM(uf1))), uf1, XEN_ARG_1, S_mixer_multiply, "a mixer");
  XEN_ASSERT_TYPE((MUS_XEN_P(uf2)) && (mus_mixer_p(MUS_XEN_TO_CLM(uf2))), uf2, XEN_ARG_2, S_mixer_multiply, "a mixer");
  if ((MUS_XEN_P(ures)) && 
      (mus_mixer_p(MUS_XEN_TO_CLM(ures))))
    res = (mus_mixer *)MUS_XEN_TO_CLM(ures);
  return(g_wrap_mixer(mus_mixer_multiply((mus_mixer *)MUS_XEN_TO_CLM(uf1),
					 (mus_mixer *)MUS_XEN_TO_CLM(uf2),
					 res),
		      (res) ? DONT_FREE_MIXER : FREE_MIXER));
}

static XEN g_frame2frame(XEN mx, XEN infr, XEN outfr) /* optional outfr */
{
  #define H_frame2frame "(" S_frame2frame " m f &optional outf) passes frame f through mixer m \
returning frame outf (or creating a new frame if necessary); this is a matrix multiply of m and f"

  mus_frame *res = NULL;
  XEN_ASSERT_TYPE((MUS_XEN_P(mx)) && (mus_mixer_p(MUS_XEN_TO_CLM(mx))), mx, XEN_ARG_1, S_frame2frame, "a mixer");
  XEN_ASSERT_TYPE((MUS_XEN_P(infr)) && (mus_frame_p(MUS_XEN_TO_CLM(infr))), infr, XEN_ARG_2, S_frame2frame, "a frame");
  if ((MUS_XEN_P(outfr)) && 
      (mus_frame_p(MUS_XEN_TO_CLM(outfr)))) 
    res = (mus_frame *)MUS_XEN_TO_CLM(outfr);
  return(g_wrap_frame(mus_frame2frame((mus_mixer *)MUS_XEN_TO_CLM(mx),
				      (mus_frame *)MUS_XEN_TO_CLM(infr),
				      res),
		      (res) ? DONT_FREE_FRAME : FREE_FRAME));
}

static XEN g_frame2list(XEN fr)
{
  #define H_frame2list "(" S_frame2list " f) -> contents of frame f as a list"
  mus_frame *val;
  int i;
  XEN res = XEN_EMPTY_LIST;
  XEN_ASSERT_TYPE((MUS_XEN_P(fr)) && (mus_frame_p(MUS_XEN_TO_CLM(fr))), fr, XEN_ONLY_ARG, S_frame2list, "a frame");
  val = (mus_frame *)MUS_XEN_TO_CLM(fr);
  for (i = (val->chans) - 1; i >= 0; i--) 
    res = XEN_CONS(C_TO_XEN_DOUBLE(val->vals[i]), res);
  return(xen_return_first(res, fr));
}

static XEN g_frame2sample(XEN mx, XEN fr)
{
  #define H_frame2sample "(" S_frame2sample " m f) -> pass frame f through mixer (or frame) m to produce a sample"
  XEN_ASSERT_TYPE((MUS_XEN_P(mx)), mx, XEN_ARG_1, S_frame2sample, "a frame or mixer");
  XEN_ASSERT_TYPE((MUS_XEN_P(fr)) && (mus_frame_p(MUS_XEN_TO_CLM(fr))), fr, XEN_ARG_2, S_frame2sample, "a frame");
  return(C_TO_XEN_DOUBLE(mus_frame2sample(MUS_XEN_TO_CLM(mx),
					  (mus_frame *)MUS_XEN_TO_CLM(fr))));
}

static XEN g_sample2frame(XEN mx, XEN insp, XEN outfr) /* optional outfr */
{
  #define H_sample2frame "(" S_sample2frame " m val &optional outf) passes the sample val through mixer m \
returning frame outf (creating it if necessary)"

  mus_frame *res = NULL;
  XEN_ASSERT_TYPE((MUS_XEN_P(mx)), mx, XEN_ARG_1, S_sample2frame, "a frame or mixer");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(insp), insp, XEN_ARG_2, S_sample2frame, "a number");
  if ((MUS_XEN_P(outfr)) && 
      (mus_frame_p(MUS_XEN_TO_CLM(outfr)))) 
    res = (mus_frame *)MUS_XEN_TO_CLM(outfr);
  return(g_wrap_frame(mus_sample2frame(MUS_XEN_TO_CLM(mx),
				       XEN_TO_C_DOUBLE(insp),
				       res),
		      (res) ? DONT_FREE_FRAME : FREE_FRAME));
}

static XEN g_make_mixer(XEN arglist)
{
  #define H_make_mixer "(" S_make_mixer " chans val0 val1 ...) makes a new mixer object \
with chans inputs and outputs, initializing the scalers from the rest of the arguments:\n\
   (set! gen (make-mixer 2 .5 .25 .125 1.0))\n\
which can be viewed from a matrix multiplication viewpoint as:\n\
\n\
   | a b |  | .5    .25 |\n\
            | .125 1.0  |\n\
\n\
giving | (a*.5 + b*.125) (a*.25 + b*1.0) |"

  /* make_empty_mixer from first of arglist, then if more args, load vals */
  mus_xen *gn;
  mus_any *ge;
  mus_mixer *fr;
  XEN cararg; XEN lst;
  int size = 0, i, j, k, len;
  XEN_ASSERT_TYPE(XEN_LIST_P_WITH_LENGTH(arglist, len), arglist, XEN_ARG_1, S_make_mixer, "a list");
  if (len == 0) mus_misc_error(S_make_mixer, "need at least 1 arg", arglist);
  cararg = XEN_CAR(arglist);
  if (!(XEN_NUMBER_P(cararg))) mus_misc_error(S_make_mixer, "first arg is the number of chans", cararg);
  size = XEN_TO_C_INT_OR_ELSE(cararg, 0);
  if (size <= 0) mus_misc_error(S_make_mixer, "chans <= 0?", cararg);
  if (size > 256) mus_misc_error(S_make_mixer, "chans > 256?", cararg);
  if (len > (size * size + 1)) 
    mus_misc_error(S_make_mixer, "extra trailing args?", arglist);
  ge = (mus_any *)mus_make_empty_mixer(size);
  if (ge)
    {
      gn = (mus_xen *)CALLOC(1, sizeof(mus_xen));
      gn->gen = ge;
      if (len > 1)
	{
	  fr = (mus_mixer *)(gn->gen);
	  j = 0;
	  k = 0;
	  for (i = 1, lst = XEN_CDR(XEN_COPY_ARG(arglist)); (i < len) && (XEN_NOT_NULL_P(lst)); i++, lst = XEN_CDR(lst))
	    {
	      if (XEN_NUMBER_P(XEN_CAR(lst)))
		fr->vals[j][k] = XEN_TO_C_DOUBLE(XEN_CAR(lst));
	      else
		{
		  mus_free(gn->gen);
		  FREE(gn);
		  mus_misc_error(S_make_mixer, "invalid arg (not a number):", XEN_CAR(lst));
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
      return(mus_xen_to_object(gn));
    }
  return(XEN_FALSE);
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

static XEN g_buffer_p(XEN obj) 
{
  #define H_buffer_p "(" S_buffer_p " gen) -> #t if gen is a " S_buffer " object, else #f"
  return(C_TO_XEN_BOOLEAN((MUS_XEN_P(obj)) && (mus_buffer_p(MUS_XEN_TO_CLM(obj)))));
}

static XEN g_make_buffer(XEN arg1, XEN arg2, XEN arg3, XEN arg4)
{
  #define H_make_buffer "(" S_make_buffer " &opt-key (size 512) fill-time) returns a new buffer \
generator, a FIFO for samples. The size argument sets the size of the buffer (not a delay time) \
and fill-time sets the time to the next request for more samples.  The intended use is in block \
processing normally involving overlap-adds."

  mus_xen *gn;
  mus_any *ge;
  XEN args[4]; XEN keys[2];
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
      siz = ikeyarg(keys[0], S_make_buffer, orig_arg[0] + 1, siz);
      filltime = fkeyarg(keys[1], S_make_buffer, orig_arg[1] + 1, 0.0);
    }
  if (siz <= 0) return(XEN_FALSE);
  buf = (Float *)CALLOC(siz, sizeof(Float));
  old_error_handler = mus_error_set_handler(local_mus_error); /* currently not needed */
  ge = mus_make_buffer(buf, siz, filltime);
  mus_error_set_handler(old_error_handler);
  if (ge)
    {
      gn = (mus_xen *)CALLOC(1, sizeof(mus_xen));
      gn->vcts = make_vcts(1);
      gn->nvcts = 1;
      gn->gen = ge;
      return(mus_xen_to_object_with_vct(gn, make_vct(siz, buf)));
    }
  FREE(buf);
  mus_error(local_error_type, local_error_msg);
  return(XEN_FALSE);
}

static XEN g_buffer2sample(XEN obj)
{
  #define H_buffer2sample "(" S_buffer2sample " gen) -> next sample in buffer, removing it"
  XEN_ASSERT_TYPE((MUS_XEN_P(obj)) && (mus_buffer_p(MUS_XEN_TO_CLM(obj))), obj, XEN_ONLY_ARG, S_buffer2sample, "a buffer gen");
  return(C_TO_XEN_DOUBLE(mus_buffer2sample(MUS_XEN_TO_CLM(obj))));
}

static XEN g_buffer2frame(XEN obj, XEN fr)
{
  #define H_buffer2frame "(" S_buffer2frame " gen &optional frame) -> next frame of samples in buffer, removing them"
  XEN_ASSERT_TYPE((MUS_XEN_P(obj)) && (mus_buffer_p(MUS_XEN_TO_CLM(obj))), obj, XEN_ARG_1, S_buffer2frame, "a buffer gen");
  if (XEN_BOUND_P(fr))
    {
      XEN_ASSERT_TYPE((MUS_XEN_P(fr)) && (mus_frame_p(MUS_XEN_TO_CLM(fr))), fr, XEN_ARG_2, S_buffer2frame, "a frame");
      /* the "{}" here are not redundant!!  SCM_ASSERT_TYPE expands into if ... then error... */
    }
  else fr = g_make_frame(XEN_LIST_1(C_TO_XEN_INT(1)));
  mus_buffer2frame((mus_any *)(MUS_XEN_TO_CLM(obj)), (mus_any *)(MUS_XEN_TO_CLM(fr)));
  return(fr);
}

static XEN g_buffer_empty_p(XEN obj)
{
  #define H_buffer_empty_p "(" S_buffer_empty_p " gen) -> #t if buffer is in need of more samples"
  XEN_ASSERT_TYPE((MUS_XEN_P(obj)) && (mus_buffer_p(MUS_XEN_TO_CLM(obj))), obj, XEN_ONLY_ARG, S_buffer_empty_p, "a buffer gen");
  return(C_TO_SMALL_XEN_INT(mus_buffer_empty_p(MUS_XEN_TO_CLM(obj))));
}

static XEN g_buffer_full_p(XEN obj)
{
  #define H_buffer_full_p "(" S_buffer_full_p " gen) -> #t if buffer has no room for any more samples"
  XEN_ASSERT_TYPE((MUS_XEN_P(obj)) && (mus_buffer_p(MUS_XEN_TO_CLM(obj))), obj, XEN_ONLY_ARG, S_buffer_full_p, "a buffer gen");
  return(C_TO_SMALL_XEN_INT(mus_buffer_full_p(MUS_XEN_TO_CLM(obj))));
}

static XEN g_sample2buffer(XEN obj, XEN val)
{
  #define H_sample2buffer "(" S_sample2buffer " gen val) append val to current end of data in buffer"
  XEN_ASSERT_TYPE((MUS_XEN_P(obj)) && (mus_buffer_p(MUS_XEN_TO_CLM(obj))), obj, XEN_ARG_1, S_sample2buffer, "a buffer gen");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ARG_2, S_sample2buffer, "a number");
  return(C_TO_XEN_DOUBLE(mus_sample2buffer(MUS_XEN_TO_CLM(obj),
					   XEN_TO_C_DOUBLE(val))));
}

static XEN g_frame2buffer(XEN obj, XEN val)
{
  #define H_frame2buffer "(" S_frame2buffer " gen f) appends sample in frame f to end of data in buffer"
  XEN_ASSERT_TYPE((MUS_XEN_P(obj)) && (mus_buffer_p(MUS_XEN_TO_CLM(obj))), obj, XEN_ARG_1, S_frame2buffer, "a buffer gen");
  XEN_ASSERT_TYPE((MUS_XEN_P(val)) && (mus_frame_p(MUS_XEN_TO_CLM(val))), val, XEN_ARG_2, S_frame2buffer, "a frame");
  mus_frame2buffer(MUS_XEN_TO_CLM(obj), MUS_XEN_TO_CLM(val));
  return(val);
}



/* ---------------- wave-train ---------------- */

#define S_make_wave_train "make-wave-train"
#define S_wave_train "wave-train"
#define S_wave_train_p "wave-train?"

static XEN g_make_wave_train(XEN arg1, XEN arg2, XEN arg3, XEN arg4, XEN arg5, XEN arg6)
{
  #define H_make_wave_train "(" S_make_wave_train " &opt-key (frequency 440.0) (initial-phase 0.0) wave)\n\
returns a new wave-train generator (an extension of pulse-train).   Frequency is \
the repetition rate of the wave found in wave. Successive waves can overlap."

  mus_xen *gn;
  mus_any *ge;
  XEN args[6]; XEN keys[3];
  int orig_arg[3] = {0, 0, 0};
  int vals, wsize, need_free = FALSE;
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
      freq = fkeyarg(keys[0], S_make_wave_train, orig_arg[0] + 1, freq);
      phase = fkeyarg(keys[1], S_make_wave_train, orig_arg[1] + 1, phase);
      if (!(XEN_KEYWORD_P(keys[2])))
        {
	  XEN_ASSERT_TYPE(VCT_P(keys[2]), keys[2], orig_arg[2] + 1, S_make_wave_train, "a vct");
	  v = TO_VCT(keys[2]);
	  wave = copy_vct_data(v);
	  wsize = v->length;
        }
    }
  if (wave == NULL) 
    {
      wave = (Float *)CALLOC(wsize, sizeof(Float));
      need_free = TRUE;
    }
  old_error_handler = mus_error_set_handler(local_mus_error); /* currently not needed */
  ge = mus_make_wave_train(freq, phase, wave, wsize);
  mus_error_set_handler(old_error_handler);
  if (ge)
    {
      gn = (mus_xen *)CALLOC(1, sizeof(mus_xen));
      gn->vcts = make_vcts(1);
      gn->nvcts = 1;
      gn->gen = ge;
      return(mus_xen_to_object_with_vct(gn, make_vct(wsize, wave)));
    }
  if (need_free) FREE(wave);
  mus_error(local_error_type, local_error_msg);
  return(XEN_FALSE);
}

static XEN g_wave_train(XEN obj, XEN fm)
{
  #define H_wave_train "(" S_wave_train " gen &optional (fm 0.0)) -> next sample of wave-train"
  Float fm1 = 0.0;
  XEN_ASSERT_TYPE((MUS_XEN_P(obj)) && (mus_wave_train_p(MUS_XEN_TO_CLM(obj))), obj, XEN_ARG_1, S_wave_train, "a wave-train gen");
  if (XEN_NUMBER_P(fm)) fm1 = XEN_TO_C_DOUBLE(fm); else XEN_ASSERT_TYPE(XEN_NOT_BOUND_P(fm), fm, XEN_ARG_2, S_wave_train, "a number");
  return(C_TO_XEN_DOUBLE(mus_wave_train(MUS_XEN_TO_CLM(obj), fm1)));
}

static XEN g_wave_train_p(XEN obj) 
{
  #define H_wave_train_p "(" S_wave_train_p " gen) -> #t if gen is a " S_wave_train " generator, else #f"
  return(C_TO_XEN_BOOLEAN((MUS_XEN_P(obj)) && (mus_wave_train_p(MUS_XEN_TO_CLM(obj)))));
}



/* ---------------- waveshape ---------------- */

#define S_make_waveshape       "make-waveshape"
#define S_waveshape            "waveshape"
#define S_waveshape_p          "waveshape?"
#define S_partials2waveshape   "partials->waveshape"
#define S_partials2polynomial  "partials->polynomial"

static Float *list2partials(XEN harms, int *npartials)
{
  int listlen, i, maxpartial, curpartial;
  Float *partials = NULL;
  XEN lst;
  listlen = XEN_LIST_LENGTH(harms);
  if (listlen == 0) return(NULL);
  /* the list is '(partial-number partial-amp ... ) */
  maxpartial = XEN_TO_C_INT_OR_ELSE(XEN_CAR(harms), 0);
  for (i = 2, lst = XEN_CDDR(XEN_COPY_ARG(harms)); i < listlen; i += 2, lst = XEN_CDDR(lst))
    {
      curpartial = XEN_TO_C_INT_OR_ELSE(XEN_CAR(lst), 0);
      if (curpartial > maxpartial) maxpartial = curpartial;
    }
  if (maxpartial < 0) return(NULL);
  partials = (Float *)CALLOC(maxpartial + 1, sizeof(Float));
  (*npartials) = maxpartial + 1;
  for (i = 0, lst = XEN_COPY_ARG(harms); i < listlen; i += 2, lst = XEN_CDDR(lst))
    {
      curpartial = XEN_TO_C_INT_OR_ELSE(XEN_CAR(lst), 0);
      partials[curpartial] = XEN_TO_C_DOUBLE(XEN_CADR(lst));
    }
  return(partials);
}

static XEN g_make_waveshape(XEN arg1, XEN arg2, XEN arg3, XEN arg4, XEN arg5, XEN arg6, XEN arg7, XEN arg8)
{
  #define H_make_waveshape "(" S_make_waveshape " &opt-key (frequency 440.0) (partials '(1 1)) (size 512) wave)\n\
returns a new waveshaping generator (essentially table-lookup driven by a sinewave)\n\
   (make-waveshape :wave (partials->waveshape '(1 1.0)))\n\
is basically the same as make-oscil"

  mus_xen *gn;
  mus_any *ge;
  XEN args[8]; XEN keys[4];
  int orig_arg[4] = {0, 0, 0, 0};
  int vals, wsize = 0, npartials = 0, partials_allocated = 0;
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
      freq = fkeyarg(keys[0], S_make_waveshape, orig_arg[0] + 1, freq);
      if (!(XEN_KEYWORD_P(keys[1])))
        {
	  XEN_ASSERT_TYPE(XEN_LIST_P(keys[1]), keys[1], orig_arg[1] + 1, S_make_waveshape, "a list");
	  partials = list2partials(keys[1], &npartials);
	  if (partials == NULL)
	    mus_misc_error(S_make_waveshape, "partials list empty?", keys[1]);
	  partials_allocated = 1;
        }
      if (!(XEN_KEYWORD_P(keys[2])))
	{
	  if (XEN_NUMBER_P(keys[2]))
	    wsize = XEN_TO_C_INT_OR_ELSE_WITH_CALLER(keys[2], 0, S_make_waveshape);
	  else
	    {
	      if (partials_allocated) {FREE(partials); partials = NULL;}
	      XEN_ASSERT_TYPE(0, keys[2], orig_arg[2] + 1, S_make_waveshape, "a number");
	    }
	}
      if (!(XEN_KEYWORD_P(keys[3])))
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
	      XEN_ASSERT_TYPE(0, keys[3], orig_arg[3] + 1, S_make_waveshape, "a vct");
	    }
        }
    }
  if (wsize <= 0)
    {
      if (partials_allocated) {FREE(partials); partials = NULL;}
      mus_misc_error(S_make_waveshape, "size <= 0?", keys[2]);
    }
  if (wave == NULL) 
    {
      if (partials == NULL) return(XEN_FALSE);
      wave = mus_partials2waveshape(npartials, partials, wsize, (Float *)CALLOC(wsize, sizeof(Float)));
    }
  if (partials_allocated) {FREE(partials); partials = NULL;}
  ge = mus_make_waveshape(freq, 0.0, wave, wsize);
  if (ge)
    {
      gn = (mus_xen *)CALLOC(1, sizeof(mus_xen));
      gn->vcts = make_vcts(1);
      gn->nvcts = 1;
      gn->gen = ge;
      return(mus_xen_to_object_with_vct(gn, make_vct(wsize, wave)));
    }
  return(XEN_FALSE);
}

static XEN g_waveshape(XEN obj, XEN index, XEN fm)
{
  #define H_waveshape "(" S_waveshape " gen &optional (index 1.0) (fm 0.0)) -> next sample of waveshaper"
  Float fm1 = 0.0, index1 = 1.0;
  XEN_ASSERT_TYPE((MUS_XEN_P(obj)) && (mus_waveshape_p(MUS_XEN_TO_CLM(obj))), obj, XEN_ARG_1, S_waveshape, "a waveshape gen");
  if (XEN_NUMBER_P(index)) index1 = XEN_TO_C_DOUBLE(index); else XEN_ASSERT_TYPE(XEN_NOT_BOUND_P(index), index, XEN_ARG_2, S_waveshape, "a number");
  if (XEN_NUMBER_P(fm)) fm1 = XEN_TO_C_DOUBLE(fm); else XEN_ASSERT_TYPE(XEN_NOT_BOUND_P(fm), fm, XEN_ARG_3, S_waveshape, "a number");
  return(C_TO_XEN_DOUBLE(mus_waveshape(MUS_XEN_TO_CLM(obj), index1, fm1)));
}

static XEN g_waveshape_p(XEN obj) 
{
  #define H_waveshape_p "(" S_waveshape_p " gen) -> #t if gen is a " S_waveshape " generator, else #f"
  return(C_TO_XEN_BOOLEAN((MUS_XEN_P(obj)) && (mus_waveshape_p(MUS_XEN_TO_CLM(obj)))));
}

static XEN g_partials2waveshape(XEN amps, XEN s_size)
{
  #define H_partials2waveshape "(" S_partials2waveshape " partials &optional (size 512))\n\
produces a waveshaping lookup table (suitable for the " S_waveshape " generator) \
that will produce the harmonic spectrum given by the partials argument"

  int npartials, size, len;
  Float *partials, *wave;
  XEN gwave;
  XEN_ASSERT_TYPE(XEN_LIST_P_WITH_LENGTH(amps, len), amps, XEN_ARG_1, S_partials2waveshape, "a list");
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(s_size), s_size, XEN_ARG_2, S_partials2waveshape, "an integer");
  if (XEN_INTEGER_P(s_size))
    size = XEN_TO_C_INT(s_size);
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

static XEN g_partials2polynomial(XEN amps, XEN ukind)
{
  #define H_partials2polynomial "(" S_partials2polynomial " partials &optional (kind 1))\n\
produces a Chebyshev polynomial suitable for use with the " S_polynomial " generator \
to create (via waveshaping) the harmonic spectrum described by the partials argument:\n\
   (let ((v0 (partials->polynomial '(1 1 2 1)))\n\
         (os (make-oscil)))\n\
     (polynomial v0 (oscil os)))"

  int npartials, kind, len;
  Float *partials, *wave;
  XEN_ASSERT_TYPE(XEN_LIST_P_WITH_LENGTH(amps, len), amps, XEN_ARG_1, S_partials2polynomial, "a list");
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(ukind), ukind, XEN_ARG_2, S_partials2polynomial, "an integer");
  if (XEN_INTEGER_P(ukind))
    kind = XEN_TO_C_INT(ukind);
  else kind = 1;
  if (len == 0)
    mus_misc_error(S_partials2polynomial, "partials list empty?", amps);
  partials = list2partials(amps, &npartials);
  wave = mus_partials2polynomial(npartials, partials, kind);
  return(make_vct(npartials, wave));
}



/* ---------------- sine-summation ---------------- */

#define S_make_sine_summation "make-sine-summation"
#define S_sine_summation      "sine-summation"
#define S_sine_summation_p    "sine-summation?"

static XEN g_sine_summation_p(XEN obj) 
{
  #define H_sine_summation_p "(" S_sine_summation_p " gen) -> #t if gen is a " S_sine_summation " generator, else #f"
  return(C_TO_XEN_BOOLEAN((MUS_XEN_P(obj)) && (mus_sine_summation_p(MUS_XEN_TO_CLM(obj)))));
}

static XEN g_sine_summation(XEN obj, XEN fm)
{
  #define H_sine_summation "(" S_sine_summation " gen &optional (fm 0.0)) -> next sample of sine summation generator"
  Float fm1 = 0.0;
  XEN_ASSERT_TYPE((MUS_XEN_P(obj)) && (mus_sine_summation_p(MUS_XEN_TO_CLM(obj))), obj, XEN_ARG_1, S_sine_summation, "a sine-summation gen");
  if (XEN_NUMBER_P(fm)) fm1 = XEN_TO_C_DOUBLE(fm); else XEN_ASSERT_TYPE(XEN_NOT_BOUND_P(fm), fm, XEN_ARG_2, S_sine_summation, "a number");
  return(C_TO_XEN_DOUBLE(mus_sine_summation(MUS_XEN_TO_CLM(obj), fm1)));
}

static XEN g_make_sine_summation(XEN arglist)
{
  #define H_make_sine_summation "(" S_make_sine_summation " &opt-key (frequency 440.0) (initial-phase 0.0) (n 1) (a 0.5) (ratio 1.0)\n\
returns a new sine summation synthesis generator."

  mus_xen *gn;
  mus_any *ge;
  XEN args[10]; XEN keys[5];
  int orig_arg[5] = {0, 0, 0, 0, 0};
  int vals, i, arglist_len;
  Float freq = 440.0, phase = 0.0, a=.5, ratio = 1.0;
  int n = 1;
  keys[0] = all_keys[C_frequency];
  keys[1] = all_keys[C_initial_phase];
  keys[2] = all_keys[C_n];
  keys[3] = all_keys[C_a];
  keys[4] = all_keys[C_ratio];
  for (i = 0; i < 10; i++) args[i] = XEN_UNDEFINED;
  arglist_len = XEN_LIST_LENGTH(arglist);
  for (i = 0; i < arglist_len; i++) args[i] = XEN_LIST_REF(arglist, i);
  vals = decode_keywords(S_make_sine_summation, 5, keys, 10, args, orig_arg);
  if (vals > 0)
    {
      freq = fkeyarg(keys[0], S_make_sine_summation, orig_arg[0] + 1, freq);
      phase = fkeyarg(keys[1], S_make_sine_summation, orig_arg[1] + 1, phase);
      n = ikeyarg(keys[2], S_make_sine_summation, orig_arg[2] + 1, n);
      a = fkeyarg(keys[3], S_make_sine_summation, orig_arg[3] + 1, a);
      ratio = fkeyarg(keys[4], S_make_sine_summation, orig_arg[4] + 1, ratio);
    }
  ge = mus_make_sine_summation(freq, phase, n, a, ratio);
  if (ge)
    {
      gn = (mus_xen *)CALLOC(1, sizeof(mus_xen));
      gn->gen = ge;
      return(mus_xen_to_object(gn));
    }
  return(XEN_FALSE);
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

static XEN g_filter_p(XEN obj) 
{
  #define H_filter_p "(" S_filter_p " gen) -> #t if gen is a " S_filter " generator, else #f"
  return(C_TO_XEN_BOOLEAN((MUS_XEN_P(obj)) && (mus_filter_p(MUS_XEN_TO_CLM(obj)))));
}

static XEN g_fir_filter_p(XEN obj) 
{
  #define H_fir_filter_p "(" S_fir_filter_p " gen) -> #t if gen is a " S_fir_filter " generator, else #f"
  return(C_TO_XEN_BOOLEAN((MUS_XEN_P(obj)) && (mus_fir_filter_p(MUS_XEN_TO_CLM(obj)))));
}

static XEN g_iir_filter_p(XEN obj) 
{
  #define H_iir_filter_p "(" S_iir_filter_p " gen) -> #t if gen is a " S_iir_filter " generator, else #f"
  return(C_TO_XEN_BOOLEAN((MUS_XEN_P(obj)) && (mus_iir_filter_p(MUS_XEN_TO_CLM(obj)))));
}

static XEN g_filter(XEN obj, XEN input)
{
  #define H_filter "(" S_filter " gen &optional (input 0.0)) -> next sample from FIR/IIR filter"
  XEN_ASSERT_TYPE((MUS_XEN_P(obj)) && (mus_filter_p(MUS_XEN_TO_CLM(obj))), obj, XEN_ARG_1, S_filter, " a filter");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(input), input, XEN_ARG_2, S_filter, "a number");
  return(C_TO_XEN_DOUBLE(mus_filter(MUS_XEN_TO_CLM(obj), XEN_TO_C_DOUBLE(input))));
}

static XEN g_fir_filter(XEN obj, XEN input)
{
  #define H_fir_filter "(" S_fir_filter " gen &optional (input 0.0)) -> next sample from FIR filter"
  XEN_ASSERT_TYPE((MUS_XEN_P(obj)) && (mus_fir_filter_p(MUS_XEN_TO_CLM(obj))), obj, XEN_ARG_1, S_fir_filter, "an FIR filter");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(input), input, XEN_ARG_2, S_fir_filter, "a number");
  return(C_TO_XEN_DOUBLE(mus_fir_filter(MUS_XEN_TO_CLM(obj), XEN_TO_C_DOUBLE(input))));
}

static XEN g_iir_filter(XEN obj, XEN input)
{
  #define H_iir_filter "(" S_iir_filter " gen &optional (input 0.0)) -> next sample from IIR filter"
  XEN_ASSERT_TYPE((MUS_XEN_P(obj)) && (mus_iir_filter_p(MUS_XEN_TO_CLM(obj))), obj, XEN_ARG_1, S_iir_filter, "an IIR filter");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(input), input, XEN_ARG_2, S_iir_filter, "a number");
  return(C_TO_XEN_DOUBLE(mus_iir_filter(MUS_XEN_TO_CLM(obj), XEN_TO_C_DOUBLE(input))));
}

enum {G_FILTER, G_FIR_FILTER, G_IIR_FILTER};

static XEN g_make_filter_1(int choice, XEN arg1, XEN arg2, XEN arg3, XEN arg4, XEN arg5, XEN arg6)
{
  XEN xwave = XEN_UNDEFINED; XEN ywave = XEN_UNDEFINED;
  mus_any *fgen = NULL;
  mus_xen *gn = NULL;
  XEN args[6]; XEN keys[3];
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
      order = ikeyarg(keys[0], caller, orig_arg[0] + 1, 0);
      if (!(XEN_KEYWORD_P(keys[1])))
        {
	  XEN_ASSERT_TYPE(VCT_P(keys[1]), keys[1], orig_arg[1] + 1, caller, "a vct");
	  xwave = keys[1];
	  x = TO_VCT(xwave);
        }
      if (nkeys > 2)
	if (!(XEN_KEYWORD_P(keys[2])))
	  {
	    XEN_ASSERT_TYPE(VCT_P(keys[2]), keys[2], orig_arg[2] + 1, caller, "a vct");
	    ywave = keys[2];
	    y = TO_VCT(ywave);
	  }
    }
  if (x == NULL)
    mus_misc_error(caller, "no coeffs?", XEN_FALSE);
  if (order < 0)
    mus_misc_error(caller, "order < 0?", keys[0]);
  if ((choice == G_FILTER) && (y == NULL))
    {
      choice = G_FIR_FILTER;
      nkeys = 2;
    }
  if (order == 0) 
    order = x->length;
  else
    {
      if (order > x->length)
	mus_misc_error(caller, "not enough coeffs?", keys[1]);
      else
	if ((x) && (y) && (x->length != y->length))
	  mus_misc_error(caller, "coeffs are not the same length?", XEN_LIST_2(keys[1], keys[2]));
    }
  switch (choice)
    {
    case G_FILTER: fgen = mus_make_filter(order, x->data, y->data, NULL); break;
    case G_FIR_FILTER: fgen = mus_make_fir_filter(order, x->data, NULL); break;
    case G_IIR_FILTER: fgen = mus_make_iir_filter(order, x->data, NULL); break;
    }
  if (fgen)
    {
      gn = (mus_xen *)CALLOC(1, sizeof(mus_xen));
      gn->gen = fgen;                                    /* delay gn allocation since make_filter can throw an error */
      gn->vcts = make_vcts(nkeys - 1);
      gn->nvcts = nkeys - 1;
      gn->vcts[0] = xwave;
      if (nkeys > 2) gn->vcts[1] = ywave;
      return(mus_xen_to_object(gn));
    }
  return(XEN_FALSE);
}

static XEN g_make_filter(XEN arg1, XEN arg2, XEN arg3, XEN arg4, XEN arg5, XEN arg6)
{
  #define H_make_filter "(" S_make_filter " &opt-key order xcoeffs ycoeffs) returns a new direct form FIR/IIR filter"
  return(g_make_filter_1(G_FILTER, arg1, arg2, arg3, arg4, arg5, arg6));
}

static XEN g_make_fir_filter(XEN arg1, XEN arg2, XEN arg3, XEN arg4)
{
  #define H_make_fir_filter "(" S_make_fir_filter " &opt-key order xcoeffs) returns a new FIR filter"
  return(g_make_filter_1(G_FIR_FILTER, arg1, arg2, arg3, arg4, XEN_UNDEFINED, XEN_UNDEFINED));
}

static XEN g_make_iir_filter(XEN arg1, XEN arg2, XEN arg3, XEN arg4)
{
  #define H_make_iir_filter "(" S_make_iir_filter " &opt-key order ycoeffs) returns a new IIR filter"
  return(g_make_filter_1(G_IIR_FILTER, arg1, arg2, arg3, arg4, XEN_UNDEFINED, XEN_UNDEFINED));
}

static XEN g_mus_xcoeffs(XEN gen) 
{
  #define H_mus_xcoeffs "(" S_mus_xcoeffs " gen) -> gen's filter xcoeffs (vct of coefficients on inputs)"
  mus_xen *ms;
  XEN_ASSERT_TYPE(MUS_XEN_P(gen), gen, XEN_ONLY_ARG, S_mus_data, "a generator");
  ms = CLM_TO_MUS_XEN(gen);
  if (ms->vcts)
    return(ms->vcts[0]); 
  return(XEN_FALSE);
}

static XEN g_mus_ycoeffs(XEN gen) 
{
  #define H_mus_ycoeffs "(" S_mus_ycoeffs " gen) -> gen's filter ycoeffs (vct of coefficients on outputs)"
  mus_xen *ms;
  XEN_ASSERT_TYPE(MUS_XEN_P(gen), gen, XEN_ONLY_ARG, S_mus_data, "a generator");
  ms = CLM_TO_MUS_XEN(gen);
  if (ms->vcts)
    {
      if (mus_iir_filter_p(MUS_XEN_TO_CLM(gen)))  
	return(ms->vcts[0]);
      else
	{
	  if (mus_filter_p(MUS_XEN_TO_CLM(gen)))
	    return(ms->vcts[1]); 
	}
    }
  return(XEN_FALSE);
}




/* ---------------- env ---------------- */

#define S_env_p       "env?"
#define S_env         "env"
#define S_make_env    "make-env"
#define S_restart_env "restart-env"
#define S_env_interp  "env-interp"

static XEN g_env_p(XEN obj) 
{
  #define H_env_p "(" S_env_p " gen) -> #t if gen is a " S_env " generator, else #f"
  return(C_TO_XEN_BOOLEAN((MUS_XEN_P(obj)) && (mus_env_p(MUS_XEN_TO_CLM(obj)))));
}

static XEN g_env(XEN obj) 
{
  #define H_env "(" S_env " gen) -> next sample from envelope generator"
  XEN_ASSERT_TYPE((MUS_XEN_P(obj)) && (mus_env_p(MUS_XEN_TO_CLM(obj))), obj, XEN_ONLY_ARG, S_env, "an env gen");
  return(C_TO_XEN_DOUBLE(mus_env(MUS_XEN_TO_CLM(obj))));
}

static XEN g_restart_env(XEN obj) 
{
  #define H_restart_env "(" S_restart_env " gen) restarts (sets to sample 0) envelope generator gen"
  XEN_ASSERT_TYPE((MUS_XEN_P(obj)) && (mus_env_p(MUS_XEN_TO_CLM(obj))), obj, XEN_ONLY_ARG, S_restart_env, "an env gen");
  mus_restart_env(MUS_XEN_TO_CLM(obj));
  return(obj);
}

static XEN g_make_env(XEN arglist)
{
  #define H_make_env "(" S_make_env " &opt-key envelope (scaler 1.0) duration (offset 0.0) (base 1.0) end (start 0))\n\
returns a new envelope generator.  'envelope' is a list of break-point pairs. To create the envelope \
these points are offset by 'offset', scaled by 'scaler', mapped over the time interval defined by \
either 'duration' (seconds) or 'start' and 'end' (samples).  If 'base' is 1.0, the connecting segments \
are linear, if 0.0 you get a step function, and anything else produces an exponential connecting segment."

  mus_xen *gn;
  mus_any *ge;
  XEN args[14]; XEN keys[7];
  int orig_arg[7] = {0, 0, 0, 0, 0, 0, 0};
  int vals, i, len = 0, arglist_len;
  Float base = 1.0, scaler = 1.0, offset = 0.0, duration = 0.0;
  off_t start = 0, end = 0;
  int npts = 0;
  Float *brkpts = NULL, *odata = NULL;
  XEN lst;
  keys[0] = all_keys[C_envelope];
  keys[1] = all_keys[C_scaler];
  keys[2] = all_keys[C_duration];
  keys[3] = all_keys[C_offset];
  keys[4] = all_keys[C_base];
  keys[5] = all_keys[C_end];
  keys[6] = all_keys[C_start];
  for (i = 0; i < 14; i++) args[i] = XEN_UNDEFINED;
  arglist_len = XEN_LIST_LENGTH(arglist);
  for (i = 0; i < arglist_len; i++) args[i] = XEN_LIST_REF(arglist, i);
  vals = decode_keywords(S_make_env, 7, keys, 14, args, orig_arg);
  if (vals > 0)
    {
      scaler = fkeyarg(keys[1], S_make_env, orig_arg[1] + 1, 1.0);
      duration = fkeyarg(keys[2], S_make_env, orig_arg[2] + 1, 0.0);
      offset = fkeyarg(keys[3], S_make_env, orig_arg[3] + 1, 0.0);
      base = fkeyarg(keys[4], S_make_env, orig_arg[4] + 1, 1.0);
      end = okeyarg(keys[5], S_make_env, orig_arg[5] + 1, 0);
      start = okeyarg(keys[6], S_make_env, orig_arg[6] + 1, 0);
      /* env data is a list, checked last to let the preceding throw wrong-type error before calloc  */
      if (!(XEN_KEYWORD_P(keys[0])))
        {
	  XEN_ASSERT_TYPE(XEN_LIST_P_WITH_LENGTH(keys[0], len), keys[0], orig_arg[0] + 1, S_make_env, "a list");
	  if (len == 0)
	    mus_misc_error(S_make_env, "null env?", keys[0]);
	  npts = len / 2;
	  brkpts = (Float *)CALLOC(len, sizeof(Float));
	  odata = (Float *)CALLOC(len, sizeof(Float));
	  for (i = 0, lst = XEN_COPY_ARG(keys[0]); (i < len) && (XEN_NOT_NULL_P(lst)); i++, lst = XEN_CDR(lst))
	    {
	      brkpts[i] = XEN_TO_C_DOUBLE(XEN_CAR(lst));
	      odata[i] = brkpts[i];
	    }
        }
    }
  if (brkpts == NULL) 
    mus_misc_error(S_make_env, "no envelope?", XEN_EMPTY_LIST);
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
  old_error_handler = mus_error_set_handler(local_mus_error);
  ge = mus_make_env(brkpts, npts, scaler, offset, base, duration, start, end, odata);
  mus_error_set_handler(old_error_handler);
  FREE(brkpts);
  if (ge)
    {
      gn = (mus_xen *)CALLOC(1, sizeof(mus_xen));
      gn->vcts = make_vcts(1);
      gn->nvcts = 1;
      gn->gen = ge;
      return(mus_xen_to_object_with_vct(gn, make_vct(len, odata)));
    }
  FREE(odata);
  mus_error(local_error_type, local_error_msg);
  return(XEN_FALSE);
}

static XEN g_env_interp(XEN x, XEN env1) /* "env" causes trouble in Objective-C!! */
{
  #define H_env_interp "(" S_env_interp " gen x) -> value of envelope at x"
  XEN_ASSERT_TYPE(XEN_NUMBER_P(x), x, XEN_ARG_1, S_env_interp, "a number");
  XEN_ASSERT_TYPE((MUS_XEN_P(env1)) && (mus_env_p(MUS_XEN_TO_CLM(env1))), env1, XEN_ARG_2, S_env_interp, "an env gen");
  return(C_TO_XEN_DOUBLE(mus_env_interp(XEN_TO_C_DOUBLE(x), MUS_XEN_TO_CLM(env1))));
}



/* ---------------- io ---------------- */

#define S_file2sample      "file->sample"
#define S_file2sample_p    "file->sample?"
#define S_make_file2sample "make-file->sample"
#define S_sample2file      "sample->file"
#define S_sample2file_p    "sample->file?"
#define S_make_sample2file "make-sample->file"
#define S_continue_sample2file "continue-sample->file"
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

static XEN g_input_p(XEN obj) 
{
  #define H_mus_input_p "(" S_mus_input_p " gen) -> #t if gen is an input generator, else #f"
  return(C_TO_XEN_BOOLEAN((MUS_XEN_P(obj)) && (mus_input_p(MUS_XEN_TO_CLM(obj)))));
}

static XEN g_output_p(XEN obj) 
{
  #define H_mus_output_p "(" S_mus_output_p " gen) -> #t if gen is an output generator, else #f"
  return(C_TO_XEN_BOOLEAN((MUS_XEN_P(obj)) && (mus_output_p(MUS_XEN_TO_CLM(obj)))));
}

static XEN g_file2sample_p(XEN obj) 
{
  #define H_file2sample_p "(" S_file2sample_p " gen) -> #t if gen is a " S_file2sample " generator, else #f"
  return(C_TO_XEN_BOOLEAN((MUS_XEN_P(obj)) && (mus_file2sample_p(MUS_XEN_TO_CLM(obj)))));
}

static XEN g_file2frame_p(XEN obj) 
{
  #define H_file2frame_p "(" S_file2frame_p " gen) -> #t if gen is a " S_file2frame " generator, else #f"
  return(C_TO_XEN_BOOLEAN((MUS_XEN_P(obj)) && (mus_file2frame_p(MUS_XEN_TO_CLM(obj)))));
}

static XEN g_sample2file_p(XEN obj) 
{
  #define H_sample2file_p "(" S_sample2file_p " gen) -> #t if gen is a " S_sample2file " generator, else #f"
  return(C_TO_XEN_BOOLEAN((MUS_XEN_P(obj)) && (mus_sample2file_p(MUS_XEN_TO_CLM(obj)))));
}

static XEN g_frame2file_p(XEN obj) 
{
  #define H_frame2file_p "(" S_frame2file_p " gen) -> #t if gen is a " S_frame2file " generator, else #f"
  return(C_TO_XEN_BOOLEAN((MUS_XEN_P(obj)) && (mus_frame2file_p(MUS_XEN_TO_CLM(obj)))));
}

static XEN g_in_any_1(char *caller, XEN frame, XEN chan, XEN inp)
{
  XEN_ASSERT_TYPE(XEN_NUMBER_P(frame), frame, XEN_ARG_1, caller, "a number");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(chan), chan, XEN_ARG_2, caller, "an integer");
  XEN_ASSERT_TYPE((MUS_XEN_P(inp)) && (mus_input_p(MUS_XEN_TO_CLM(inp))), inp, XEN_ARG_3, caller, "an input gen");
  return(C_TO_XEN_DOUBLE(mus_in_any(XEN_TO_C_OFF_T_OR_ELSE(frame, 0), XEN_TO_C_INT(chan), (mus_input *)MUS_XEN_TO_CLM(inp))));
}

static XEN g_in_any(XEN frame, XEN chan, XEN inp) 
{
  #define H_in_any "(" S_in_any " frame chan &optional stream) -> input stream sample at frame in channel chan"
  return(g_in_any_1(S_in_any, frame, chan, inp));
}

static XEN g_ina(XEN frame, XEN inp) 
{
  #define H_ina "(" S_ina " frame &optional stream) -> input stream sample in channel 0 at frame"
  return(g_in_any_1(S_ina, frame, C_TO_SMALL_XEN_INT(0), inp));
}

static XEN g_inb(XEN frame, XEN inp) 
{
  #define H_inb "(" S_inb " frame &optional stream) -> input stream sample in channel 1 at frame"
  return(g_in_any_1(S_inb, frame, C_TO_SMALL_XEN_INT(1), inp));
}

static XEN g_out_any_1(char *caller, XEN frame, XEN chan, XEN val, XEN outp)
{
  XEN_ASSERT_TYPE(XEN_NUMBER_P(frame), frame, XEN_ARG_1, caller, "a number");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(chan), chan, XEN_ARG_2, caller, "an integer");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ARG_3, caller, "a number");
  XEN_ASSERT_TYPE((MUS_XEN_P(outp)) && (mus_output_p(MUS_XEN_TO_CLM(outp))), outp, XEN_ARG_4, caller, "an output gen");
  return(C_TO_XEN_DOUBLE(mus_out_any(XEN_TO_C_OFF_T_OR_ELSE(frame, 0),
				     XEN_TO_C_DOUBLE(val),
				     XEN_TO_C_INT(chan),
				     (mus_output *)MUS_XEN_TO_CLM(outp))));
}

static XEN g_out_any(XEN frame, XEN val, XEN chan, XEN outp)
{
  #define H_out_any "(" S_out_any " frame val chan &optional stream) adds val to output stream at frame in channel chan"
  return(g_out_any_1(S_out_any, frame, chan, val, outp));
}

static XEN g_outa(XEN frame, XEN val, XEN outp)
{
  #define H_outa "(" S_outa " frame val &optional stream) adds val to output stream at frame in channel 0"
  return(g_out_any_1(S_outa, frame, C_TO_SMALL_XEN_INT(0), val, outp));
}

static XEN g_outb(XEN frame, XEN val, XEN outp)
{
  #define H_outb "(" S_outb " frame val &optional stream) adds val to output stream at frame in channel 1"
  return(g_out_any_1(S_outb, frame, C_TO_SMALL_XEN_INT(1), val, outp));
}

static XEN g_outc(XEN frame, XEN val, XEN outp)
{
  #define H_outc "(" S_outc " frame val &optional stream) adds val to output stream at frame in channel 2"
  return(g_out_any_1(S_outc, frame, C_TO_SMALL_XEN_INT(2), val, outp));
}

static XEN g_outd(XEN frame, XEN val, XEN outp)
{
  #define H_outd "(" S_outd " frame val &optional stream) adds val to output stream at frame in channel 3"
  return(g_out_any_1(S_outd, frame, C_TO_SMALL_XEN_INT(3), val, outp));
}

static XEN g_mus_close(XEN ptr)
{
  #define H_mus_close "(" S_mus_close " fd) closes the stream (fd) opened by mus-open-read or write"
  XEN_ASSERT_TYPE(MUS_XEN_P(ptr), ptr, XEN_ONLY_ARG, S_mus_close, "an IO gen");
  return(C_TO_XEN_INT(mus_close_file((mus_any *)MUS_XEN_TO_CLM(ptr))));
}

static XEN g_make_file2sample(XEN name)
{
  #define H_make_file2sample "(" S_make_file2sample " filename) returns an input generator reading 'filename' (a sound file)"
  mus_xen *gn;
  mus_any *ge;
  XEN_ASSERT_TYPE(XEN_STRING_P(name), name, XEN_ONLY_ARG, S_make_file2sample, "a string");
  if (!(mus_file_probe(XEN_TO_C_STRING(name))))
    XEN_ERROR(NO_SUCH_FILE,
	      XEN_LIST_3(C_TO_XEN_STRING(S_make_file2sample),
			 name,
			 C_TO_XEN_STRING(strerror(errno))));
  ge = mus_make_file2sample(XEN_TO_C_STRING(name));
  if (ge)
    {
      gn = (mus_xen *)CALLOC(1, sizeof(mus_xen));
      gn->gen = ge;
      gn->nvcts = 0;
      return(xen_return_first(mus_xen_to_object(gn), name));
    }
  return(XEN_FALSE);
}

static XEN g_file2sample(XEN obj, XEN samp, XEN chan)
{
  #define H_file2sample "(" S_file2sample " obj frame chan) -> sample value in sound file read by 'obj' in channel chan at frame"
  int channel = 0;
  XEN_ASSERT_TYPE((MUS_XEN_P(obj)) && (mus_input_p(MUS_XEN_TO_CLM(obj))), obj, XEN_ARG_1, S_file2sample, "an input gen");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(samp), samp, XEN_ARG_2, S_file2sample, "a number");
  if (XEN_BOUND_P(chan))
    {
      XEN_ASSERT_TYPE(XEN_INTEGER_P(chan), chan, XEN_ARG_3, S_file2sample, "an integer");
      channel = XEN_TO_C_INT(chan);
    }
  return(C_TO_XEN_DOUBLE(mus_file2sample(MUS_XEN_TO_CLM(obj),
					 XEN_TO_C_OFF_T_OR_ELSE(samp, 0),
					 channel)));
}

static XEN g_make_sample2file(XEN name, XEN chans, XEN out_format, XEN out_type, XEN comment)
{
  #define H_make_sample2file "(" S_make_sample2file " filename chans data-format header-type comment)\n\
returns an output generator writing the sound file 'filename' which is set up to have \
'chans' channels of 'data-format' samples with a header of 'header-type'.  The latter \
should be sndlib identifiers:\n\
   (make-sample->file \"test.snd\" 2 mus-lshort mus-riff)"

  mus_xen *gn;
  mus_any *rgen = NULL;
  int df, ht, chns;
  XEN_ASSERT_TYPE(XEN_STRING_P(name), name, XEN_ARG_1, S_make_sample2file, "a string");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(chans), chans, XEN_ARG_2, S_make_sample2file, "an integer");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(out_format), out_format, XEN_ARG_3, S_make_sample2file, "an integer (data format id)");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(out_type), out_type, XEN_ARG_4, S_make_sample2file, "an integer (header type id)");
  df = XEN_TO_C_INT(out_format);
  if (MUS_DATA_FORMAT_OK(df))
    {
      ht = XEN_TO_C_INT(out_type);
      if (MUS_HEADER_TYPE_OK(ht))
	{
	  chns = XEN_TO_C_INT(chans);
	  if (chns > 0)
	    {
	      rgen = mus_make_sample2file_with_comment(XEN_TO_C_STRING(name),
						       chns,
						       df,
						       ht,
						       (XEN_STRING_P(comment)) ? XEN_TO_C_STRING(comment) : NULL);
	      if (rgen)
		{
		  gn = (mus_xen *)CALLOC(1, sizeof(mus_xen));
		  gn->gen = rgen;
		  gn->nvcts = 0;
		  return(xen_return_first(mus_xen_to_object(gn), name));
		}
	    }
	  else mus_misc_error(S_make_sample2file, "invalid chans", chans);
	}
      else mus_misc_error(S_make_sample2file, "invalid header type", out_type);
    }
  else mus_misc_error(S_make_sample2file, "invalid data format", out_format);
  return(XEN_FALSE);
}

static XEN g_continue_sample2file(XEN name)
{
  #define H_continue_sample2file "(" S_continue_sample2file " filename) returns an output generator \
that reopens an existing sound file 'filename' ready for output via sample->file"

  mus_xen *gn;
  mus_any *rgen = NULL;
  XEN_ASSERT_TYPE(XEN_STRING_P(name), name, XEN_ARG_1, S_continue_sample2file, "a string");
  rgen = mus_continue_sample2file(XEN_TO_C_STRING(name));
  if (rgen)
    {
      gn = (mus_xen *)CALLOC(1, sizeof(mus_xen));
      gn->gen = rgen;
      gn->nvcts = 0;
      return(xen_return_first(mus_xen_to_object(gn), name));
    }
  return(XEN_FALSE);
}

static XEN g_sample2file(XEN obj, XEN samp, XEN chan, XEN val)
{
  #define H_sample2file "(" S_sample2file " obj samp chan val) adds val to the output stream \
handled by the output generator 'obj', in channel 'chan' at frame 'samp'"

  XEN_ASSERT_TYPE((MUS_XEN_P(obj)) && (mus_output_p(MUS_XEN_TO_CLM(obj))), obj, XEN_ARG_1, S_sample2file, "an output gen");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(samp), samp, XEN_ARG_2, S_sample2file, "a number");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(chan), chan, XEN_ARG_3, S_sample2file, "an integer");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ARG_4, S_sample2file, "a number");
  return(C_TO_XEN_DOUBLE(mus_sample2file(MUS_XEN_TO_CLM(obj),
					 XEN_TO_C_OFF_T_OR_ELSE(samp, 0),
					 XEN_TO_C_INT(chan),
					 XEN_TO_C_DOUBLE(val))));
}

static XEN g_make_file2frame(XEN name)
{
  #define H_make_file2frame "(" S_make_file2frame " filename) returns an input generator reading 'filename' (a sound file)"
  mus_xen *gn;
  mus_any *ge;
  XEN_ASSERT_TYPE(XEN_STRING_P(name), name, XEN_ONLY_ARG, S_make_file2frame, "a string");
  if (!(mus_file_probe(XEN_TO_C_STRING(name))))
    XEN_ERROR(NO_SUCH_FILE,
	      XEN_LIST_3(C_TO_XEN_STRING(S_make_file2frame),
			 name,
			 C_TO_XEN_STRING(strerror(errno))));
  ge = mus_make_file2frame(XEN_TO_C_STRING(name));
  if (ge)
    {
      gn = (mus_xen *)CALLOC(1, sizeof(mus_xen));
      gn->gen = ge;
      gn->nvcts = 0;
      return(xen_return_first(mus_xen_to_object(gn), name));
    }
  return(XEN_FALSE);
}

static XEN g_file2frame(XEN obj, XEN samp, XEN outfr)
{
  #define H_file2frame "(" S_file2frame " obj samp outf) -> frame of samples at frame 'samp' in sound file read by 'obj'"
  mus_frame *res = NULL;
  XEN_ASSERT_TYPE((MUS_XEN_P(obj)) && (mus_input_p(MUS_XEN_TO_CLM(obj))), obj, XEN_ARG_1, S_file2frame, "an input gen");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(samp), samp, XEN_ARG_2, S_file2frame, "a number");
  if ((MUS_XEN_P(outfr)) && 
      (mus_frame_p(MUS_XEN_TO_CLM(outfr)))) 
    res = (mus_frame *)MUS_XEN_TO_CLM(outfr);
  return(g_wrap_frame(mus_file2frame(MUS_XEN_TO_CLM(obj),
				     XEN_TO_C_OFF_T_OR_ELSE(samp, 0),
				     res),
		      (res) ? DONT_FREE_FRAME : FREE_FRAME));
}

static XEN g_make_frame2file(XEN name, XEN chans, XEN out_format, XEN out_type)
{
  #define H_make_frame2file "(" S_make_frame2file " filename chans data-format header-type)\n\
returns an output generator writing the sound file 'filename' which is set up to have \
'chans' channels of 'data-format' samples with a header of 'header-type'.  The latter \
should be sndlib identifiers:\n\
   (make-frame->file \"test.snd\" 2 mus-lshort mus-riff)"

  mus_xen *gn;
  mus_any *fgen = NULL;
  XEN_ASSERT_TYPE(XEN_STRING_P(name), name, XEN_ARG_1, S_make_frame2file, "a string");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(chans), chans, XEN_ARG_2, S_make_frame2file, "an integer");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(out_format), out_format, XEN_ARG_3, S_make_frame2file, "an integer (data format id)");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(out_type), out_type, XEN_ARG_4, S_make_frame2file, "an integer (header-type id)");
  fgen = mus_make_frame2file(XEN_TO_C_STRING(name),
			     XEN_TO_C_INT(chans),
			     XEN_TO_C_INT(out_format),
			     XEN_TO_C_INT(out_type));
  if (fgen)
    {
      gn = (mus_xen *)CALLOC(1, sizeof(mus_xen));
      gn->gen = fgen;
      gn->nvcts = 0;
      return(xen_return_first(mus_xen_to_object(gn), name));
    }
  return(XEN_FALSE);
}

static XEN g_frame2file(XEN obj, XEN samp, XEN val)
{
  #define H_frame2file "(" S_frame2file " obj samp val) adds frame 'val' to the output stream \
handled by the output generator 'obj' at frame 'samp'"

  XEN_ASSERT_TYPE((MUS_XEN_P(obj)) && (mus_output_p(MUS_XEN_TO_CLM(obj))), obj, XEN_ARG_1, S_frame2file, "an output gen");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(samp), samp, XEN_ARG_2, S_frame2file, "a number");
  XEN_ASSERT_TYPE((MUS_XEN_P(val)) && (mus_frame_p(MUS_XEN_TO_CLM(val))), val, XEN_ARG_3, S_frame2file, "a frame");
  return(g_wrap_frame(mus_frame2file(MUS_XEN_TO_CLM(obj),
				     XEN_TO_C_OFF_T_OR_ELSE(samp, 0),
				     (mus_frame *)MUS_XEN_TO_CLM(val)),
		      DONT_FREE_FRAME));
}

static XEN g_array2file(XEN filename, XEN data, XEN len, XEN srate, XEN channels)
{
  #define H_array2file "(" S_array2file " filename data len srate channels) writes 'data', \
a vct object of interleaved samples to the sound file 'filename' set up to have the given \
srate and channels.  'len' samples are written."

  int olen, samps;
  vct *v;
  XEN_ASSERT_TYPE(XEN_STRING_P(filename), filename, XEN_ARG_1, S_array2file, "a string");
  XEN_ASSERT_TYPE(VCT_P(data), data, XEN_ARG_2, S_array2file, "a vct");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(len), len, XEN_ARG_3, S_array2file, "a number");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(srate), srate, XEN_ARG_4, S_array2file, "a number");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(channels), channels, XEN_ARG_5, S_array2file, "an integer");
  v = TO_VCT(data);
  samps = XEN_TO_C_INT_OR_ELSE(len, 1);
  if (samps <= 0)
    mus_misc_error(S_array2file, "samples <= 0?", len);
  if (samps > v->length)
    samps = v->length;
  olen = mus_fltarray2file(XEN_TO_C_STRING(filename),
			   v->data,
			   samps,
			   XEN_TO_C_INT_OR_ELSE(srate, 0),
			   XEN_TO_C_INT(channels));
  return(xen_return_first(C_TO_XEN_INT(olen), filename));
}

static XEN g_file2array(XEN filename, XEN chan, XEN start, XEN samples, XEN data)
{
  #define H_file2array "(" S_file2array " filename chan start samples data) reads the sound file \
'filename' placing samples from channel 'chan' into the vct object 'data' starting in the file \
at frame 'start' and reading 'samples' samples altogether."

  int err, chn, samps;
  vct *v;
  char *name;
  XEN_ASSERT_TYPE(XEN_STRING_P(filename), filename, XEN_ARG_1, S_file2array, "a string");
  name = XEN_TO_C_STRING(filename);
  if (!(mus_file_probe(name)))
    XEN_ERROR(NO_SUCH_FILE,
	      XEN_LIST_3(C_TO_XEN_STRING(S_file2array),
			 filename,
			 C_TO_XEN_STRING(strerror(errno))));
  XEN_ASSERT_TYPE(XEN_INTEGER_P(chan), chan, XEN_ARG_2, S_file2array, "an integer");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(start), start, XEN_ARG_3, S_file2array, "a number");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(samples), samples, XEN_ARG_4, S_file2array, "a number");
  XEN_ASSERT_TYPE((VCT_P(data)), data, XEN_ARG_5, S_file2array, "a vct");
  v = TO_VCT(data);
  samps = XEN_TO_C_INT_OR_ELSE(samples, 1);
  if (samps <= 0) 
    mus_misc_error(S_file2array, "samples <= 0?", samples);
  chn = XEN_TO_C_INT(chan);
  if ((chn < 0) || (chn > mus_sound_chans(name)))
    XEN_ERROR(NO_SUCH_CHANNEL,
	      XEN_LIST_3(C_TO_XEN_STRING(S_file2array),
			 C_TO_XEN_STRING("invalid chan"),
			 chan));
  if (samps > v->length)
    samps = v->length;
  err = mus_file2fltarray(name,
			  chn,
			  XEN_TO_C_OFF_T_OR_ELSE(start, 0),
			  samps,
			  v->data);
  return(xen_return_first(C_TO_SMALL_XEN_INT(err), filename));
}

static XEN g_mus_file_buffer_size(void)
{
  #define H_mus_file_buffer_size "(" S_mus_file_buffer_size ") -> current CLM IO buffer size (default is 8192)"
  return(C_TO_XEN_INT(mus_file_buffer_size()));
}

static XEN g_mus_set_file_buffer_size(XEN val)
{
  XEN_ASSERT_TYPE(XEN_INTEGER_P(val), val, XEN_ONLY_ARG, "set! " S_mus_file_buffer_size, "an integer");
  return(C_TO_XEN_INT(mus_set_file_buffer_size(XEN_TO_C_INT(val))));
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

static XEN g_readin_p(XEN obj) 
{
  #define H_readin_p "(" S_readin_p " gen) -> #t if gen is a " S_readin " generator, else #f"
  return(C_TO_XEN_BOOLEAN((MUS_XEN_P(obj)) && (mus_readin_p(MUS_XEN_TO_CLM(obj)))));
}

static XEN g_readin(XEN obj)
{
  #define H_readin "(" S_readin " gen) -> next sample from readin generator (a sound file reader)"
  XEN_ASSERT_TYPE((MUS_XEN_P(obj)) && (mus_readin_p(MUS_XEN_TO_CLM(obj))), obj, XEN_ARG_1, S_readin, "a readin gen");
  return(C_TO_XEN_DOUBLE(mus_readin(MUS_XEN_TO_CLM(obj))));
}

static XEN g_make_readin(XEN arg1, XEN arg2, XEN arg3, XEN arg4, XEN arg5, XEN arg6, XEN arg7, XEN arg8)
{
  #define H_make_readin "(" S_make_readin " &opt-key file (channel 0) (start 0) (direction 1))\n\
returns a new readin (file input) generator reading the sound file 'file' starting at frame \
'start' in channel 'channel' and reading forward if 'direction' is not -1"

  /* optkey file channel start direction */
  mus_xen *gn;
  mus_any *ge;
  char *file = NULL;
  XEN args[8]; XEN keys[4];
  int orig_arg[4] = {0, 0, 0, 0};
  int vals;
  int channel = 0, direction = 1;
  off_t start = 0;
  keys[0] = all_keys[C_file];
  keys[1] = all_keys[C_channel];
  keys[2] = all_keys[C_start];
  keys[3] = all_keys[C_direction];
  args[0] = arg1; args[1] = arg2; args[2] = arg3; args[3] = arg4; args[4] = arg5; args[5] = arg6; args[6] = arg7; args[7] = arg8; 
  vals = decode_keywords(S_make_readin, 4, keys, 8, args, orig_arg);
  if (vals > 0)
    {
      if (!(XEN_KEYWORD_P(keys[0])))
        {
	  XEN_ASSERT_TYPE(XEN_STRING_P(keys[0]), keys[0], orig_arg[0] + 1, S_make_readin, "a string");
	  file = XEN_TO_C_STRING(keys[0]);
	}
      channel = ikeyarg(keys[1], S_make_readin, orig_arg[1] + 1, channel);
      start = okeyarg(keys[2], S_make_readin, orig_arg[2] + 1, start);
      direction = ikeyarg(keys[3], S_make_readin, orig_arg[3] + 1, direction);
    }
  if (channel < 0)
    mus_misc_error(S_make_readin, "channel < 0?", keys[1]);
  if (!(mus_file_probe(file)))
    XEN_ERROR(NO_SUCH_FILE,
	      XEN_LIST_3(C_TO_XEN_STRING(S_make_readin),
			 C_TO_XEN_STRING(file),
			 C_TO_XEN_STRING(strerror(errno))));
  if (channel >= mus_sound_chans(file))
    mus_misc_error(S_make_readin, "channel > available chans?", keys[1]);
  ge = mus_make_readin(file, channel, start, direction);
  if (ge)
    {
      gn = (mus_xen *)CALLOC(1, sizeof(mus_xen));
      gn->gen = ge;
      return(mus_xen_to_object(gn));
    }
  return(XEN_FALSE);
}

static XEN g_increment(XEN obj)
{
  #define H_mus_increment "(" S_mus_increment " gen) -> gen's " S_mus_increment " field, if any"
  XEN_ASSERT_TYPE(MUS_XEN_P(obj), obj, XEN_ONLY_ARG, S_mus_increment, "a generator");
  return(C_TO_XEN_DOUBLE(mus_increment(MUS_XEN_TO_CLM(obj))));
}

static XEN g_set_increment(XEN obj, XEN val)
{
  XEN_ASSERT_TYPE(MUS_XEN_P(obj), obj, XEN_ARG_1, S_mus_set_increment, "a generator");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ARG_2, S_mus_set_increment, "a number");
  return(C_TO_XEN_DOUBLE(mus_set_increment(MUS_XEN_TO_CLM(obj), XEN_TO_C_DOUBLE(val))));
}

static XEN g_location(XEN obj)
{
  #define H_mus_location "(" S_mus_location " gen) -> gen's " S_mus_location " field, if any"
  XEN_ASSERT_TYPE(MUS_XEN_P(obj), obj, XEN_ONLY_ARG, S_mus_location, "a generator");
  return(C_TO_XEN_OFF_T(mus_location(MUS_XEN_TO_CLM(obj))));
}

static XEN g_set_location(XEN obj, XEN val)
{
  XEN_ASSERT_TYPE(MUS_XEN_P(obj), obj, XEN_ARG_1, S_mus_set_location, "a generator");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ARG_2, S_mus_set_location, "a number");
  return(C_TO_XEN_OFF_T(mus_set_location(MUS_XEN_TO_CLM(obj), XEN_TO_C_OFF_T_OR_ELSE(val, 0))));
}

static XEN g_channel(XEN obj)
{
  #define H_mus_channel "(" S_mus_channel " gen) -> gen's " S_mus_channel " field, if any"
  XEN_ASSERT_TYPE((MUS_XEN_P(obj)) && (mus_input_p(MUS_XEN_TO_CLM(obj))), obj, XEN_ONLY_ARG, S_mus_channel, "an input gen");
  return(C_TO_SMALL_XEN_INT(mus_channel((mus_input *)MUS_XEN_TO_CLM(obj))));
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
#define S_mus_linear        "mus-linear"
#define S_mus_sinusoidal    "mus-sinusoidal"
#define S_locsig_type       "locsig-type"
#define S_set_locsig_type   "set-locsig-type"
#define S_move_locsig       "move-locsig"

static XEN g_locsig_ref(XEN obj, XEN chan)
{
  #define H_locsig_ref "(" S_locsig_ref " gen chan) -> locsig 'gen' channel 'chan' scaler"
  XEN_ASSERT_TYPE((MUS_XEN_P(obj)) && (mus_locsig_p(MUS_XEN_TO_CLM(obj))), obj, XEN_ARG_1, S_locsig_ref, "a locsig gen");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(chan), chan, XEN_ARG_2, S_locsig_ref, "an integer");
  return(C_TO_XEN_DOUBLE(mus_locsig_ref(MUS_XEN_TO_CLM(obj), XEN_TO_C_INT(chan))));
}

static XEN g_locsig_set(XEN obj, XEN chan, XEN val)
{
  #define H_locsig_set "(" S_locsig_set " gen chan val) sets the locsig generator's channel 'chan' scaler to 'val'"
  XEN_ASSERT_TYPE((MUS_XEN_P(obj)) && (mus_locsig_p(MUS_XEN_TO_CLM(obj))), obj, XEN_ARG_1, S_locsig_set, "a locsig gen");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(chan), chan, XEN_ARG_2, S_locsig_set, "an integer");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ARG_3, S_locsig_set, "a number");
  return(C_TO_XEN_DOUBLE(mus_locsig_set(MUS_XEN_TO_CLM(obj),
					XEN_TO_C_INT(chan),
					XEN_TO_C_DOUBLE(val))));
}

static XEN g_locsig_reverb_ref(XEN obj, XEN chan)
{
  #define H_locsig_reverb_ref "(" S_locsig_reverb_ref " gen chan) -> locsig reverb channel 'chan' scaler"
  XEN_ASSERT_TYPE((MUS_XEN_P(obj)) && (mus_locsig_p(MUS_XEN_TO_CLM(obj))), obj, XEN_ARG_1, S_locsig_reverb_ref, "a locsig gen");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(chan), chan, XEN_ARG_2, S_locsig_reverb_ref, "an integer");
  return(C_TO_XEN_DOUBLE(mus_locsig_reverb_ref(MUS_XEN_TO_CLM(obj), XEN_TO_C_INT(chan))));
}

static XEN g_locsig_reverb_set(XEN obj, XEN chan, XEN val)
{
  #define H_locsig_reverb_set "(" S_locsig_reverb_set " gen chan val) sets the locsig reverb channel 'chan' scaler to 'val'"
  XEN_ASSERT_TYPE((MUS_XEN_P(obj)) && (mus_locsig_p(MUS_XEN_TO_CLM(obj))), obj, XEN_ARG_1, S_locsig_reverb_set, "a locsig gen");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(chan), chan, XEN_ARG_2, S_locsig_reverb_set, "an integer");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ARG_3, S_locsig_reverb_set, "a number");
  return(C_TO_XEN_DOUBLE(mus_locsig_reverb_set(MUS_XEN_TO_CLM(obj),
					       XEN_TO_C_INT(chan),
					       XEN_TO_C_DOUBLE(val))));
}

static XEN g_locsig_p(XEN obj)
{
  #define H_locsig_p "(" S_locsig_p " gen) -> #t if gen is a " S_locsig " generator, else #f"
  return(C_TO_XEN_BOOLEAN((MUS_XEN_P(obj)) && (mus_locsig_p(MUS_XEN_TO_CLM(obj)))));
}

static XEN g_locsig(XEN obj, XEN loc, XEN val)
{
  #define H_locsig "(" S_locsig " gen loc val) adds 'val' to the output of locsig at frame 'loc'"
  XEN_ASSERT_TYPE((MUS_XEN_P(obj)) && (mus_locsig_p(MUS_XEN_TO_CLM(obj))), obj, XEN_ARG_1, S_locsig, "a locsig gen");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(loc), loc, XEN_ARG_2, S_locsig, "a number");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ARG_3, S_locsig, "a number");
  return(g_wrap_frame(mus_locsig(MUS_XEN_TO_CLM(obj),
				 XEN_TO_C_OFF_T_OR_ELSE(loc, 0),
				 XEN_TO_C_DOUBLE(val)),
		      DONT_FREE_FRAME));
}

static int clm_locsig_type = MUS_LINEAR;

static XEN g_locsig_type()
{
  #define H_locsig_type "(" S_locsig_type ") -> default locsig interpolation type"
  return(C_TO_XEN_INT(clm_locsig_type));
}

static XEN g_set_locsig_type(XEN val)
{
  int newval;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(val), val, XEN_ONLY_ARG, S_locsig_type, "mus-linear or mus-sinusoidal");
  newval = XEN_TO_C_INT(val);
  if ((newval == MUS_LINEAR) || (newval == MUS_SINUSOIDAL))
    clm_locsig_type = newval;
  return(C_TO_XEN_INT(clm_locsig_type));
}

static XEN g_make_locsig(XEN arglist)
{
  #define H_make_locsig "(" S_make_locsig " &opt-key (degree 0.0) (distance 1.0) (reverb 0.0) output revout (channels 1) (type mus-linear))\n\
returns a new generator for signal placement in n channels.  Channel 0 corresponds to 0 degrees."

  XEN out_obj = XEN_UNDEFINED; XEN rev_obj = XEN_UNDEFINED;
  mus_xen *gn;
  mus_any *ge;
  mus_output *outp = NULL, *revp = NULL;
  XEN args[14]; XEN keys[7];
  int orig_arg[7] = {0, 0, 0, 0, 0, 0, 0};
  int vals, i, arglist_len, vlen = 0, out_chans = 1, type;
  Float degree = 0.0, distance = 1.0, reverb = 0.0;
  type = clm_locsig_type;
  keys[0] = all_keys[C_degree];
  keys[1] = all_keys[C_distance];
  keys[2] = all_keys[C_reverb];
  keys[3] = all_keys[C_output];  
  keys[4] = all_keys[C_revout];
  keys[5] = all_keys[C_channels];
  keys[6] = all_keys[C_type];
  for (i = 0; i < 14; i++) args[i] = XEN_UNDEFINED;
  arglist_len = XEN_LIST_LENGTH(arglist);
  for (i = 0; i < arglist_len; i++) args[i] = XEN_LIST_REF(arglist, i);
  vals = decode_keywords(S_make_locsig, 7, keys, 14, args, orig_arg);
  if (vals > 0)
    {
      degree = fkeyarg(keys[0], S_make_locsig, orig_arg[0] + 1, degree);
      distance = fkeyarg(keys[1], S_make_locsig, orig_arg[1] + 1, distance);
      reverb = fkeyarg(keys[2], S_make_locsig, orig_arg[2] + 1, reverb);
      if (!(XEN_KEYWORD_P(keys[3]))) 
	{
	  if ((MUS_XEN_P(keys[3])) && (mus_output_p(MUS_XEN_TO_CLM(keys[3]))))
	    {
	      out_obj = keys[3];
	      vlen++;
	      outp = (mus_output *)MUS_XEN_TO_CLM(keys[3]);
	      out_chans = mus_channels((mus_any *)outp);
	    }
	  else 
	    {
	      XEN_ASSERT_TYPE((XEN_NOT_BOUND_P(keys[3])) || (XEN_BOOLEAN_P(keys[3])), keys[3], orig_arg[3] + 1, S_make_locsig, "an output gen");
	      keys[3] = XEN_UNDEFINED;
	    }
	}
      if (!(XEN_KEYWORD_P(keys[4]))) 
	{
	  if ((MUS_XEN_P(keys[4])) && (mus_output_p(MUS_XEN_TO_CLM(keys[4]))))
	    {
	      rev_obj = keys[4];
	      vlen++;
	      revp = (mus_output *)MUS_XEN_TO_CLM(keys[4]);
	    }
	  else 
	    {
	      XEN_ASSERT_TYPE((XEN_NOT_BOUND_P(keys[4])) || (XEN_BOOLEAN_P(keys[4])), keys[4], orig_arg[4] + 1, S_make_locsig, "a reverb output gen");
	      keys[4] = XEN_UNDEFINED;
	    }
	}
      out_chans = ikeyarg(keys[5], S_make_locsig, orig_arg[5] + 1, out_chans);
      type = ikeyarg(keys[6], S_make_locsig, orig_arg[6] + 1, type);
      XEN_ASSERT_TYPE(out_chans > 0, keys[5], orig_arg[5] + 1, S_make_locsig, "int > 0");
      XEN_ASSERT_TYPE((type == MUS_LINEAR) || (type == MUS_SINUSOIDAL), keys[6], orig_arg[6] + 1, S_make_locsig, "mus-linear or mus-sinusoidal");
    }
  ge = mus_make_locsig(degree, distance, reverb, out_chans, outp, revp, type);
  if (ge)
    {
      gn = (mus_xen *)CALLOC(1, sizeof(mus_xen));
      if (vlen > 0)
	{
	  gn->vcts = make_vcts(vlen);
	  i = 0;
	  if (XEN_BOUND_P(out_obj)) gn->vcts[i++] = out_obj;
	  if (XEN_BOUND_P(rev_obj)) gn->vcts[i] = rev_obj;
	  gn->nvcts = vlen;
	}
      gn->gen = ge;
      return(mus_xen_to_object(gn));
    }
  return(XEN_FALSE);
}

static XEN g_channels(XEN obj)
{
  #define H_mus_channels "(" S_mus_channels " gen) -> gen's " S_mus_channels " field, if any"
  XEN_ASSERT_TYPE(MUS_XEN_P(obj), obj, XEN_ONLY_ARG, S_mus_channels, "a generator");
  return(C_TO_SMALL_XEN_INT(mus_channels(MUS_XEN_TO_CLM(obj))));
}

static XEN g_move_locsig(XEN obj, XEN degree, XEN distance)
{
  #define H_move_locsig "(" S_move_locsig " gen degree distance) moves locsig gen to reflect degree and distance"
  XEN_ASSERT_TYPE((MUS_XEN_P(obj)) && (mus_locsig_p(MUS_XEN_TO_CLM(obj))), obj, XEN_ARG_1, S_move_locsig, "a locsig gen");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(degree), degree, XEN_ARG_2, S_move_locsig, "a number in degrees");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(distance), distance, XEN_ARG_3, S_move_locsig, "a number > 1.0");
  mus_move_locsig(MUS_XEN_TO_CLM(obj),
		  XEN_TO_C_DOUBLE(degree),
		  XEN_TO_C_DOUBLE(distance));
  return(obj);
}


/* ---------------- src ---------------- */

enum {INPUT_FUNCTION, ANALYZE_FUNCTION, EDIT_FUNCTION, SYNTHESIZE_FUNCTION, SELF_WRAPPER};

static Float funcall1 (void *ptr, int direction) /* intended for "as-needed" input funcs */
{
  /* if this is called, it's a callback from C, where ptr is a mus_xen object whose vcts[0]
   * field is a XEN procedure to be called, the result being returned back to C.  In the
   * Scheme world, it's a procedure of one arg, the current read direction: 
   *
   * funcall1 is input-func for clm.c make args, or the 2nd arg to the gen (mus_src(gen, input))
   *    it is called in C (*input)(environ, dir)
   *    environ in mus_xen *gn
   *      its gn->vcts array [INPUT_FUNCTION] = scm procedure object (if any) else EMPTY_LIST
   *      this is set in the gen call if it's passed there, else in the make-gen call
   * so we get here via *funcall1(gn, dir)
   *   and make sure gn->vcts[INPUT_FUNCTION] is a procedure, call it with dir as its arg,
   *   it returns a float which we then return to C
   */
  mus_xen *gn = (mus_xen *)ptr;
  if ((gn) && (gn->vcts) && (XEN_BOUND_P(gn->vcts[INPUT_FUNCTION])) && (XEN_PROCEDURE_P(gn->vcts[INPUT_FUNCTION])))
    /* the gh_procedure_p call can be confused by 0 -> segfault! */
    return(XEN_TO_C_DOUBLE(XEN_CALL_1_NO_CATCH(gn->vcts[INPUT_FUNCTION], C_TO_SMALL_XEN_INT(direction), "as-needed-input")));
  else return(0.0);
}

#define S_src         "src"
#define S_src_p       "src?"
#define S_make_src    "make-src"
#define S_clear_sincs "clear-sincs"

static XEN g_clear_sincs(void)
{
  mus_clear_sinc_tables();
  return(XEN_FALSE);
}

static XEN g_src_p(XEN obj) 
{
  #define H_src_p "(" S_src_p " gen) -> #t if gen is an " S_src " generator, else #f"
  return(C_TO_XEN_BOOLEAN((MUS_XEN_P(obj)) && (mus_src_p(MUS_XEN_TO_CLM(obj)))));
}

static XEN g_src(XEN obj, XEN pm, XEN func) 
{
  #define H_src "(" S_src " gen &optional (pm 0.0) input-function) -> next sampling rate conversion sample. \
'pm' can be used to change the sampling rate on a sample-by-sample basis.  'input-function' \
is a function of one argument (the current input direction, normally ignored) that is called \
internally whenever a new sample of input data is needed.  If the associated " S_make_src " \
included an 'input' argument, input-function is ignored."

  Float pm1 = 0.0;
  mus_xen *gn;
  XEN_ASSERT_TYPE((MUS_XEN_P(obj)) && (mus_src_p(MUS_XEN_TO_CLM(obj))), obj, XEN_ARG_1, S_src, "an src gen");
  gn = CLM_TO_MUS_XEN(obj);
  if (XEN_NUMBER_P(pm)) pm1 = XEN_TO_C_DOUBLE(pm); else XEN_ASSERT_TYPE(XEN_NOT_BOUND_P(pm), pm, XEN_ARG_2, S_src, "a number");
  if (XEN_PROCEDURE_P(func))
    {
      if (XEN_REQUIRED_ARGS(func) == 1)
	gn->vcts[INPUT_FUNCTION] = func;
      else mus_misc_error(S_src, "wrong number of args to input function (want 1)", func);
    }
  return(C_TO_XEN_DOUBLE(mus_src(MUS_XEN_TO_CLM(obj), pm1, 0)));
}

static XEN g_make_src(XEN arg1, XEN arg2, XEN arg3, XEN arg4, XEN arg5, XEN arg6)
{
  #define H_make_src "(" S_make_src " &opt-key input (srate 1.0) (width 10))\n\
returns a new sampling-rate conversion generator (using 'warped sinc interpolation'). \
'srate' is the ratio between the new rate and the old. 'width' is the sine \
width (effectively the steepness of the low-pass filter), normally between 10 and 100. \
'input' if given is an open file stream."

  XEN in_obj = XEN_UNDEFINED;
  mus_xen *gn;
  mus_any *ge;
  int vals, wid = 0; /* 0 here picks up the current default width in clm.c */
  XEN args[6]; XEN keys[3];
  int orig_arg[3] = {0, 0, 0};
  Float srate = 1.0;
  keys[0] = all_keys[C_input];
  keys[1] = all_keys[C_srate];
  keys[2] = all_keys[C_width];
  args[0] = arg1; args[1] = arg2; args[2] = arg3; args[3] = arg4; args[4] = arg5; args[5] = arg6;
  vals = decode_keywords(S_make_src, 3, keys, 6, args, orig_arg);
  if (vals > 0)
    {
      if (!(XEN_KEYWORD_P(keys[0]))) 
	{
	  XEN_ASSERT_TYPE(XEN_PROCEDURE_P(keys[0]) && (XEN_REQUIRED_ARGS(keys[0]) == 1), keys[0], orig_arg[0] + 1, S_make_src, "a procedure of 1 arg");
	  in_obj = keys[0];
	}
      srate = fkeyarg(keys[1], S_make_src, orig_arg[1] + 1, srate);
      wid = ikeyarg(keys[2], S_make_src, orig_arg[2] + 1, wid);
    }
  if (srate <= 0) mus_misc_error(S_make_src, "srate <= 0.0?", keys[1]);
  if (wid < 0) mus_misc_error(S_make_src, "width < 0?", keys[2]);
  gn = (mus_xen *)CALLOC(1, sizeof(mus_xen));
  /* mus_make_src assumes it can invoke the input function! */
  gn->vcts = make_vcts(1);
  gn->vcts[INPUT_FUNCTION] = in_obj;
  gn->nvcts = 1;
  old_error_handler = mus_error_set_handler(local_mus_error);
  ge = mus_make_src(funcall1, srate, wid, gn);
  mus_error_set_handler(old_error_handler);
  if (ge)
    {
      gn->gen = ge;
      return(mus_xen_to_object(gn));
    }
  FREE(gn->vcts);
  FREE(gn);
  mus_error(local_error_type, local_error_msg);
  return(XEN_FALSE);
}




/* ---------------- granulate ---------------- */

#define S_granulate_p    "granulate?"
#define S_granulate      "granulate"
#define S_make_granulate "make-granulate"
#define S_mus_ramp       "mus-ramp"
#define S_mus_set_ramp   "mus-set-ramp"
#define S_mus_hop        "mus-hop"
#define S_mus_set_hop    "mus-set-hop"

static XEN g_granulate_p(XEN obj) 
{
  #define H_granulate_p "(" S_granulate_p " gen) -> #t if gen is a " S_granulate " generator, else #f"
  return(C_TO_XEN_BOOLEAN((MUS_XEN_P(obj)) && (mus_granulate_p(MUS_XEN_TO_CLM(obj)))));
}

static XEN g_granulate(XEN obj, XEN func) 
{
  #define H_granulate "(" S_granulate " gen) -> next sample from granular synthesis generator"
  mus_xen *gn;
  XEN_ASSERT_TYPE((MUS_XEN_P(obj)) && (mus_granulate_p(MUS_XEN_TO_CLM(obj))), obj, XEN_ARG_1, S_granulate, "a granulate gen");
  gn = CLM_TO_MUS_XEN(obj);
  if (XEN_PROCEDURE_P(func))
    {
      if (XEN_REQUIRED_ARGS(func) == 1)
	gn->vcts[INPUT_FUNCTION] = func;
      else mus_misc_error(S_granulate, "wrong number of args to input function (want 1)", func);
    }
  return(C_TO_XEN_DOUBLE(mus_granulate(MUS_XEN_TO_CLM(obj), 0)));
}

static XEN g_ramp(XEN obj)
{
  #define H_mus_ramp "(" S_mus_ramp " gen) -> (granulate generator) gen's " S_mus_ramp " field"
  XEN_ASSERT_TYPE((MUS_XEN_P(obj)) && (mus_granulate_p(MUS_XEN_TO_CLM(obj))), obj, XEN_ONLY_ARG, S_mus_ramp, "a granulate gen");
  return(C_TO_XEN_INT(mus_ramp(MUS_XEN_TO_CLM(obj))));
}

static XEN g_set_ramp(XEN obj, XEN val)
{
  XEN_ASSERT_TYPE((MUS_XEN_P(obj)) && (mus_granulate_p(MUS_XEN_TO_CLM(obj))), obj, XEN_ARG_1, S_mus_set_ramp, "a granulate gen");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ARG_2, S_mus_set_ramp, "a number");
  return(C_TO_XEN_INT(mus_set_ramp(MUS_XEN_TO_CLM(obj), XEN_TO_C_INT_OR_ELSE(val, 0))));
}

static XEN g_make_granulate(XEN arglist)
{
  #define H_make_granulate "(" S_make_granulate " &opt-key input (expansion 1.0) (length .15)\n\
    (scaler .6) (hop .05) (ramp .4) (jitter 1.0) max-size)\n\
returns a new granular synthesis generator.  'length' is the grain length (seconds), 'expansion' is the ratio in timing \
between the new and old (expansion > 1.0 slows things down), 'scaler' scales the grains \
to avoid overflows, 'hop' is the spacing (seconds) between successive grains upon output \
jitter controls the randomness in that spacing, input can be a file pointer."

  XEN in_obj = XEN_UNDEFINED;
  mus_xen *gn;
  mus_any *ge;
  XEN args[16]; XEN keys[8];
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
  for (i = 0; i < 16; i++) args[i] = XEN_UNDEFINED;
  arglist_len = XEN_LIST_LENGTH(arglist);
  for (i = 0; i < arglist_len; i++) args[i] = XEN_LIST_REF(arglist, i);
  vals = decode_keywords(S_make_granulate, 8, keys, 16, args, orig_arg);
  if (vals > 0)
    {
      if (!(XEN_KEYWORD_P(keys[0]))) 
	{
	  XEN_ASSERT_TYPE(XEN_PROCEDURE_P(keys[0]) && (XEN_REQUIRED_ARGS(keys[0]) == 1), keys[0], orig_arg[0] + 1, S_make_granulate, "a procedure of 1 arg");
	  in_obj = keys[0];
	}
      expansion = fkeyarg(keys[1], S_make_granulate, orig_arg[1] + 1, expansion);
      segment_length = fkeyarg(keys[2], S_make_granulate, orig_arg[2] + 1, segment_length);
      segment_scaler = fkeyarg(keys[3], S_make_granulate, orig_arg[3] + 1, segment_scaler);
      output_hop = fkeyarg(keys[4], S_make_granulate, orig_arg[4] + 1, output_hop);
      ramp_time = fkeyarg(keys[5], S_make_granulate, orig_arg[5] + 1, ramp_time);
      jitter = fkeyarg(keys[6], S_make_granulate, orig_arg[6] + 1, jitter);
      XEN_ASSERT_TYPE((jitter >= 0.0) && (jitter < 100.0), keys[6], orig_arg[6] + 1, S_make_granulate, "0.0 .. 100.0");
      maxsize = ikeyarg(keys[7], S_make_granulate, orig_arg[7] + 1, maxsize);
    }
  if (expansion <= 0.0) mus_misc_error(S_make_granulate, "expansion <= 0.0?", keys[1]);
  if (segment_length <= 0.0) mus_misc_error(S_make_granulate, "segment-length <= 0.0?", keys[2]);
  if (segment_scaler == 0.0) mus_misc_error(S_make_granulate, "segment-scaler: 0.0?", keys[3]);
  if (output_hop < 0.0) mus_misc_error(S_make_granulate, "hop < 0?", keys[4]);
  if ((segment_length + output_hop) > 60.0) /* multiplied by srate in mus_make_granulate in array allocation */
    mus_misc_error(S_make_granulate, "segment_length + output_hop too large!", XEN_LIST_2(keys[2], keys[4]));
  gn = (mus_xen *)CALLOC(1, sizeof(mus_xen));
  old_error_handler = mus_error_set_handler(local_mus_error);
  ge = mus_make_granulate(funcall1, expansion, segment_length, segment_scaler, output_hop, ramp_time, jitter, maxsize, gn);
  mus_error_set_handler(old_error_handler);
  if (ge)
    {
      gn->vcts = make_vcts(1);
      gn->vcts[INPUT_FUNCTION] = in_obj;
      gn->nvcts = 1;
      gn->gen = ge;
      return(mus_xen_to_object(gn));
    }
  FREE(gn);
  mus_error(local_error_type, local_error_msg);
  return(XEN_FALSE);
}




/* ---------------- convolve ---------------- */

#define S_convolve_p     "convolve?"
#define S_convolve       "convolve"
#define S_make_convolve  "make-convolve"
#define S_convolve_files "convolve-files"

static XEN g_convolve_p(XEN obj) 
{
  #define H_convolve_p "(" S_convolve_p " gen) -> #t if gen is a " S_convolve " generator, else #f"
  return(C_TO_XEN_BOOLEAN((MUS_XEN_P(obj)) && (mus_convolve_p(MUS_XEN_TO_CLM(obj)))));
}

static XEN g_convolve(XEN obj, XEN func) 
{
  #define H_convolve_gen "(" S_convolve " gen &optional input-func) -> next sample from convolution generator"
  mus_xen *gn;
  XEN_ASSERT_TYPE((MUS_XEN_P(obj)) && (mus_convolve_p(MUS_XEN_TO_CLM(obj))), obj, XEN_ARG_1, S_convolve, "a convolve gen");
  gn = CLM_TO_MUS_XEN(obj);
  if (XEN_PROCEDURE_P(func))
    {
      if (XEN_REQUIRED_ARGS(func) == 1)
	gn->vcts[INPUT_FUNCTION] = func;
      else mus_misc_error(S_convolve, "wrong number of args to input function (want 1)", func);
    }
  return(C_TO_XEN_DOUBLE(mus_convolve(MUS_XEN_TO_CLM(obj), 0)));
}

/* filter-size? */

static XEN g_make_convolve(XEN arglist)
{
  #define H_make_convolve "(" S_make_convolve " &opt-key input filter fft-size)\n\
returns a new convolution generator which convolves its input with the impulse response 'filter'."

  mus_xen *gn;
  mus_any *ge;
  XEN args[6]; XEN keys[3];
  int orig_arg[3] = {0, 0, 0};
  int vals, i, arglist_len, fftlen;
  vct *filter = NULL;
  XEN filt = XEN_UNDEFINED; XEN in_obj = XEN_UNDEFINED;
  int fft_size = 0;
  keys[0] = all_keys[C_input];
  keys[1] = all_keys[C_filter];
  keys[2] = all_keys[C_fft_size];
  for (i = 0; i < 6; i++) args[i] = XEN_UNDEFINED;
  arglist_len = XEN_LIST_LENGTH(arglist);
  for (i = 0; i < arglist_len; i++) args[i] = XEN_LIST_REF(arglist, i);
  vals = decode_keywords(S_make_convolve, 3, keys, 6, args, orig_arg);
  if (vals > 0)
    {
      if (!(XEN_KEYWORD_P(keys[0]))) 
	{
	  XEN_ASSERT_TYPE(XEN_PROCEDURE_P(keys[0]) && (XEN_REQUIRED_ARGS(keys[0]) == 1), keys[0], orig_arg[0] + 1, S_make_convolve, "a procedure of 1 arg");
	  in_obj = keys[0];
	}
      if (!(XEN_KEYWORD_P(keys[1]))) 
	{
	  XEN_ASSERT_TYPE(VCT_P(keys[1]), keys[1], orig_arg[1] + 1, S_make_convolve, "a vct");
	  filt = keys[1];
	  filter = TO_VCT(filt);
	}
      fft_size = ikeyarg(keys[2], S_make_convolve, orig_arg[2] + 1, fft_size);
    }
  if (filter == NULL)
    mus_misc_error(S_make_convolve, "no impulse (filter)?", XEN_FALSE);
  if (POWER_OF_2_P(filter->length))
    fftlen = filter->length * 2;
  else fftlen = (int)pow(2.0, 1 + (int)(log((Float)(filter->length + 1)) / log(2.0)));
  if (fft_size < fftlen) fft_size = fftlen;
  gn = (mus_xen *)CALLOC(1, sizeof(mus_xen));
  old_error_handler = mus_error_set_handler(local_mus_error);
  ge = mus_make_convolve(funcall1, filter->data, fft_size, filter->length, gn);
  mus_error_set_handler(old_error_handler);
  if (ge)
    {
      gn->nvcts = 2;
      gn->vcts = make_vcts(2);
      gn->vcts[INPUT_FUNCTION] = in_obj;
      gn->vcts[1] = filt;
      gn->gen = ge;
      return(mus_xen_to_object(gn));
    }
  FREE(gn);
  mus_error(local_error_type, local_error_msg);
  return(XEN_FALSE);
}

static XEN g_convolve_files(XEN file1, XEN file2, XEN maxamp, XEN outfile)
{
  #define H_convolve_files "(" S_convolve_files " file1 file2 maxamp output-file) convolves \
file1 and file2 writing outfile after scaling the convolution result to maxamp."

  char *f1, *f2, *f3;
  Float maxval = 1.0;
  XEN_ASSERT_TYPE(XEN_STRING_P(file1), file1, XEN_ARG_1, S_convolve_files, "a string");
  XEN_ASSERT_TYPE(XEN_STRING_P(file2), file2, XEN_ARG_2, S_convolve_files, "a string");
  XEN_ASSERT_TYPE(XEN_NUMBER_IF_BOUND_P(maxamp), maxamp, XEN_ARG_3, S_convolve_files, "a number");
  XEN_ASSERT_TYPE((XEN_NOT_BOUND_P(outfile)) || (XEN_STRING_P(outfile)), outfile, XEN_ARG_4, S_convolve_files, "a string");
  f1 = XEN_TO_C_STRING(file1);
  f2 = XEN_TO_C_STRING(file2);
  if (XEN_STRING_P(outfile)) f3 = XEN_TO_C_STRING(outfile); else f3 = "tmp.snd";
  if (XEN_NUMBER_P(maxamp)) maxval = XEN_TO_C_DOUBLE(maxamp);
  mus_convolve_files(f1, f2, maxval, f3);
  return(XEN_FALSE);
}



/* ---------------- phase-vocoder ---------------- */

/* pvedit pvanalyze pvsynthesize:
 * these three functions provide a path for the call (clm.c) (*(pv->edit))(pv->environ)
 *   which is calling a user-supplied edit function within the particular phase-vocoder
 *   generator's context.  "environ" is an uninterpreted void pointer passed in by the
 *   user, and passed here as the edit function argument.  In this file, pv->edit is
 *   &pvedit, and (void *)ptr is environ; in make_phase_vocoder we set environ to be
 *   the mus_xen object that shadows the phase-vocoder generator, with two special
 *   pointers in the vcts field: vcts[EDIT_FUNCTION] is the (Scheme-side) function
 *   passed by the user, and vcts[SELF_WRAPPER] is a pointer to the (Scheme-relevant)
 *   smob that packages the mus_xen pointer for Scheme.  This way, the user's
 *    (make-phase-vocoder ... (lambda (v) (mus-length v)) ...)
 *   treats v as the current pv gen, vcts[SELF_WRAPPER] = v, vcts[EDIT_FUNCTION] = 
 *   the lambda form, mus_xen obj->gen is the C-side pv struct pointer.  See above
 *   under funcall1 for more verbiage.  (All this complication arises because clm.c
 *   is pure C -- no notion that Scheme might be the caller, and the user's pv.scm
 *   or whatever is pure Scheme -- no notion that C is actually doing the work,
 *   and we have to tie everything together here including the Scheme-C-Scheme-C 
 *   call chains).
 */

static int pvedit (void *ptr)
{
  mus_xen *gn = (mus_xen *)ptr;
  return(XEN_TO_C_BOOLEAN(XEN_CALL_1_NO_CATCH(gn->vcts[EDIT_FUNCTION], 
					      gn->vcts[SELF_WRAPPER],
					      "phase-vocoder edit function")));
}

static Float pvsynthesize (void *ptr)
{
  mus_xen *gn = (mus_xen *)ptr;
  return(XEN_TO_C_DOUBLE(XEN_CALL_1_NO_CATCH(gn->vcts[SYNTHESIZE_FUNCTION], 
					     gn->vcts[SELF_WRAPPER], 
					     "phase-vocoder synthesis function")));
}

static int pvanalyze (void *ptr, Float (*input)(void *arg1, int direction))
{
  mus_xen *gn = (mus_xen *)ptr;
  /* we can only get input func if it's already set up by the outer gen call, so (?) we can use that function here */
  return(XEN_TO_C_BOOLEAN(XEN_CALL_2_NO_CATCH(gn->vcts[ANALYZE_FUNCTION], 
					      gn->vcts[SELF_WRAPPER], 
					      gn->vcts[INPUT_FUNCTION], 
					      "phase-vocoder analysis function")));
}

#define S_phase_vocoder       "phase-vocoder"
#define S_phase_vocoder_p     "phase-vocoder?"
#define S_make_phase_vocoder  "make-phase-vocoder"
#define S_pv_outctr           "pv-outctr"
#define S_pv_amps             "pv-amps"
#define S_pv_freqs            "pv-freqs"
#define S_pv_amp_increments   "pv-amp-increments"
#define S_pv_phases           "pv-phases"
#define S_pv_phase_increments "pv-phase-increments"

static XEN g_phase_vocoder_p(XEN obj) 
{
  #define H_phase_vocoder_p "(" S_phase_vocoder_p " gen) -> #t if gen is an " S_phase_vocoder " generator, else #f"
  return(C_TO_XEN_BOOLEAN((MUS_XEN_P(obj)) && (mus_phase_vocoder_p(MUS_XEN_TO_CLM(obj)))));
}

static XEN g_phase_vocoder(XEN obj, XEN func) 
{
  #define H_phase_vocoder "(" S_phase_vocoder " gen &optional input-function) -> phase vocoder"
  mus_xen *gn;
  XEN_ASSERT_TYPE((MUS_XEN_P(obj)) && (mus_phase_vocoder_p(MUS_XEN_TO_CLM(obj))), obj, XEN_ARG_1, S_phase_vocoder, "a phase-vocoder gen");
  gn = CLM_TO_MUS_XEN(obj);
  if (XEN_PROCEDURE_P(func))
    {
      if (XEN_REQUIRED_ARGS(func) == 1)
	gn->vcts[INPUT_FUNCTION] = func;
      else mus_misc_error(S_phase_vocoder, "wrong number of args to input function (want 1)", func);
    }
  return(C_TO_XEN_DOUBLE(mus_phase_vocoder(MUS_XEN_TO_CLM(obj), 0)));
}

static XEN g_make_phase_vocoder(XEN arglist)
{
  #define H_make_phase_vocoder "(" S_make_phase_vocoder " &opt-key input fft-size overlap interp pitch analyze edit synthesize)\n\
returns a new phase-vocoder generator; input is the input function (if can be set at run-time), analyze, edit, \
and synthesize are either #f or functions that replace the default innards of the generator, fft-size, overlap \
and interp set the fftsize, the amount of overlap between ffts, and the time between new analysis calls. \
'analyze', if given, takes 2 args, the generator and the input function; if it returns #t, the default analysis \
code is also called.  'edit', if given, takes 1 arg, the generator; if it returns #t, the default edit code \
is run.  'synthesize' is a function of 1 arg, the generator; it is called to get the current vocoder output. \
\n(make-phase-vocoder #f 512 4 256 1.0 #f #f #f) \n\n(make-phase-vocoder #f 512 4 256 1.0 \n\
  (lambda (v infunc) (set! incalls (+ incalls 1)) #t) \n\
  (lambda (v) (set! editcalls (+ editcalls 1)) #t) \n\
  (lambda (v) (set! outcalls (+ outcalls 1)) 0.0))) \n"

  XEN in_obj = XEN_UNDEFINED; XEN edit_obj = XEN_UNDEFINED; XEN synthesize_obj = XEN_UNDEFINED; XEN analyze_obj = XEN_UNDEFINED;
  mus_xen *gn;
  mus_any *ge;
  XEN args[16]; XEN keys[8];
  XEN pv_obj;
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
  for (i = 0; i < 16; i++) args[i] = XEN_UNDEFINED;
  arglist_len = XEN_LIST_LENGTH(arglist);
  for (i = 0; i < arglist_len; i++) args[i] = XEN_LIST_REF(arglist, i);
  vals = decode_keywords(S_make_phase_vocoder, 8, keys, 16, args, orig_arg);
  if (vals > 0)
    {
      if (XEN_PROCEDURE_P(keys[0]))
	{
	  if (XEN_REQUIRED_ARGS(keys[0]) == 1)
	    in_obj = keys[0];
	  else mus_misc_error(S_make_phase_vocoder, "wrong number of args to input function (want 1)", keys[0]);
	}
      fft_size = ikeyarg(keys[1], S_make_phase_vocoder, orig_arg[1] + 1, fft_size);
      overlap = ikeyarg(keys[2], S_make_phase_vocoder, orig_arg[2] + 1, overlap);
      interp = ikeyarg(keys[3], S_make_phase_vocoder, orig_arg[3] + 1, interp);
      pitch = fkeyarg(keys[4], S_make_phase_vocoder, orig_arg[4] + 1, pitch);
      if (XEN_PROCEDURE_P(keys[5]))
	{
	  if (XEN_REQUIRED_ARGS(keys[5]) == 2)
	    analyze_obj = keys[5];
	  else mus_misc_error(S_make_phase_vocoder, "wrong number of args to analyze function (want 2: pv infunc)", keys[5]);
	}
      if (XEN_PROCEDURE_P(keys[6]))
	{
	  if (XEN_REQUIRED_ARGS(keys[6]) == 1)
	    edit_obj = keys[6];
	  else mus_misc_error(S_make_phase_vocoder, "wrong number of args to edit function (want 1: pv)", keys[6]);
	}
      if (XEN_PROCEDURE_P(keys[7]))
	{
	  if (XEN_REQUIRED_ARGS(keys[7]) == 1)
	    synthesize_obj = keys[7];
	  else mus_misc_error(S_make_phase_vocoder, "wrong number of args to synthesize function (want 1: pv)", keys[7]);
	}
    }
  gn = (mus_xen *)CALLOC(1, sizeof(mus_xen));
  old_error_handler = mus_error_set_handler(local_mus_error);
  ge = mus_make_phase_vocoder(funcall1,
			      fft_size, overlap, interp, pitch,
			      (XEN_NOT_BOUND_P(analyze_obj) ? NULL : pvanalyze),
			      (XEN_NOT_BOUND_P(edit_obj) ? NULL : pvedit),
			      (XEN_NOT_BOUND_P(synthesize_obj) ? NULL : pvsynthesize),
			      (void *)gn);
  mus_error_set_handler(old_error_handler);
  if (ge)
    {
      gn->nvcts = 5;
      gn->vcts = make_vcts(gn->nvcts);
      gn->vcts[INPUT_FUNCTION] = in_obj;
      gn->vcts[EDIT_FUNCTION] = edit_obj;
      gn->vcts[ANALYZE_FUNCTION] = analyze_obj;
      gn->vcts[SYNTHESIZE_FUNCTION] = synthesize_obj;
      gn->gen = ge;
      pv_obj = mus_xen_to_object(gn);
      /* need scheme-relative backpointer for possible function calls */
      gn->vcts[SELF_WRAPPER] = pv_obj;
      return(pv_obj);
    }
  FREE(gn);
  mus_error(local_error_type, local_error_msg);
  return(XEN_FALSE);
}

static XEN g_pv_amps(XEN pv) 
{
  #define H_pv_amps "(" S_pv_amps " gen) -> vct containing the current output sinusoid amplitudes"
  Float *amps; 
  int len;
  mus_xen *gn;
  XEN_ASSERT_TYPE((MUS_XEN_P(pv)) && (mus_phase_vocoder_p(MUS_XEN_TO_CLM(pv))), pv, XEN_ONLY_ARG, S_pv_amps, "a phase-vocoder gen");
  gn = CLM_TO_MUS_XEN(pv);
  amps = mus_phase_vocoder_amps((void *)(gn->gen)); 
  len = mus_length((mus_any *)(gn->gen));
  return(make_vct_wrapper(len / 2, amps));
}
  
static XEN g_pv_freqs(XEN pv) 
{
  #define H_pv_freqs "(" S_pv_freqs " gen) -> vct containing the current output sinusoid frequencies"
  Float *amps; 
  int len;
  mus_xen *gn;
  XEN_ASSERT_TYPE((MUS_XEN_P(pv)) && (mus_phase_vocoder_p(MUS_XEN_TO_CLM(pv))), pv, XEN_ONLY_ARG, S_pv_freqs, "a phase-vocoder gen");
  gn = CLM_TO_MUS_XEN(pv);
  amps = mus_phase_vocoder_freqs((void *)(gn->gen)); 
  len = mus_length((mus_any *)(gn->gen));
  return(make_vct_wrapper(len, amps));
}
  
static XEN g_pv_phases(XEN pv) 
{
  #define H_pv_phases "(" S_pv_phases " gen) -> vct containing the current output sinusoid phases"
  Float *amps; 
  int len;
  mus_xen *gn;
  XEN_ASSERT_TYPE((MUS_XEN_P(pv)) && (mus_phase_vocoder_p(MUS_XEN_TO_CLM(pv))), pv, XEN_ONLY_ARG, S_pv_phases, "a phase-vocoder gen");
  gn = CLM_TO_MUS_XEN(pv);
  amps = mus_phase_vocoder_phases((void *)(gn->gen)); 
  len = mus_length((mus_any *)(gn->gen));
  return(make_vct_wrapper(len / 2, amps));
}
  
static XEN g_pv_amp_increments(XEN pv) 
{
  #define H_pv_amp_increments "(" S_pv_amp_increments " gen) -> vct containing the current output sinusoid amplitude increments per sample"
  Float *amps; 
  int len;
  mus_xen *gn;
  XEN_ASSERT_TYPE((MUS_XEN_P(pv)) && (mus_phase_vocoder_p(MUS_XEN_TO_CLM(pv))), pv, XEN_ONLY_ARG, S_pv_amp_increments, "a phase-vocoder gen");
  gn = CLM_TO_MUS_XEN(pv);
  amps = mus_phase_vocoder_amp_increments((void *)(gn->gen)); 
  len = mus_length((mus_any *)(gn->gen));
  return(make_vct_wrapper(len, amps));
}
  
static XEN g_pv_phase_increments(XEN pv) 
{
  #define H_pv_phase_increments "(" S_pv_phase_increments " gen) -> vct containing the current output sinusoid phase increments"
  Float *amps; 
  int len;
  mus_xen *gn;
  XEN_ASSERT_TYPE((MUS_XEN_P(pv)) && (mus_phase_vocoder_p(MUS_XEN_TO_CLM(pv))), pv, XEN_ONLY_ARG, S_pv_phase_increments, "a phase-vocoder gen");
  gn = CLM_TO_MUS_XEN(pv);
  amps = mus_phase_vocoder_phase_increments((void *)(gn->gen)); 
  len = mus_length((mus_any *)(gn->gen));
  return(make_vct_wrapper(len / 2, amps));
}
  
static XEN g_pv_outctr(XEN obj)
{
  #define H_pv_outctr "(" S_pv_outctr " gen) -> gen's " S_pv_outctr " field"
  XEN_ASSERT_TYPE((MUS_XEN_P(obj)) && (mus_phase_vocoder_p(MUS_XEN_TO_CLM(obj))), obj, XEN_ONLY_ARG, S_pv_outctr, "a phase-vocoder generator");
  return(C_TO_XEN_INT(mus_phase_vocoder_outctr(MUS_XEN_TO_CLM(obj))));
}

static XEN g_pv_set_outctr(XEN obj, XEN val)
{
  XEN_ASSERT_TYPE((MUS_XEN_P(obj)) && (mus_phase_vocoder_p(MUS_XEN_TO_CLM(obj))), obj, XEN_ONLY_ARG, "set! " S_pv_outctr, "a phase-vocoder generator");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ARG_2, "set! " S_pv_outctr, "a number");
  return(C_TO_XEN_INT(mus_phase_vocoder_set_outctr(MUS_XEN_TO_CLM(obj), XEN_TO_C_INT_OR_ELSE(val, 0))));
}

static XEN g_hop(XEN obj)
{
  #define H_mus_hop "(" S_mus_hop " gen) -> gen's " S_mus_hop " field"
  XEN_ASSERT_TYPE(MUS_XEN_P(obj), obj, XEN_ONLY_ARG, S_mus_hop, "a generator");
  return(C_TO_SMALL_XEN_INT(mus_hop(MUS_XEN_TO_CLM(obj))));
}

static XEN g_set_hop(XEN obj, XEN val)
{
  XEN_ASSERT_TYPE(MUS_XEN_P(obj), obj, XEN_ARG_1, S_mus_set_hop, "a generator");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ARG_2, S_mus_set_hop, "a number");
  return(C_TO_SMALL_XEN_INT(mus_set_hop(MUS_XEN_TO_CLM(obj), XEN_TO_C_INT_OR_ELSE(val, 0))));
}





/* ---------------- mix ---------------- */

#define S_mus_mix "mus-mix"

static XEN g_mus_mix(XEN out, XEN in, XEN ost, XEN olen, XEN ist, XEN mx, XEN envs)
{
  #define H_mus_mix "(" S_mus_mix " outfile infile (outloc 0) frames (inloc 0) mixer envs)\n\
mixes infile into outfile starting at outloc in outfile and inloc in infile \
mixing frames frames of infile.  frames defaults to the length of infile. If mixer, \
use it to scale the various channels; if envs (an array of envelope generators), use \
it in conjunction with mixer to scale/envelope all the various ins and outs."

  mus_mixer *mx1 = NULL;
  mus_any ***envs1 = NULL;
  char *outfile = NULL, *infile = NULL;
  int in_len = 0, out_len, i, j;
  off_t ostart = 0, istart = 0, osamps = 0;
  int in_chans, out_chans, in_size = 0, out_size;  /* mus_mix in clm.c assumes the envs array is large enough */
  XEN *vdata0; XEN *vdata1;
  XEN_ASSERT_TYPE(XEN_STRING_P(out), out, XEN_ARG_1, S_mus_mix, "a string");
  XEN_ASSERT_TYPE(XEN_STRING_P(in), in, XEN_ARG_2, S_mus_mix, "a string");
  XEN_ASSERT_TYPE(XEN_NUMBER_IF_BOUND_P(ost), ost, XEN_ARG_3, S_mus_mix, "a number");
  XEN_ASSERT_TYPE(XEN_NUMBER_IF_BOUND_P(olen), olen, XEN_ARG_4, S_mus_mix, "a number");
  XEN_ASSERT_TYPE(XEN_NUMBER_IF_BOUND_P(ist), ist, XEN_ARG_5, S_mus_mix, "a number");
  XEN_ASSERT_TYPE((XEN_NOT_BOUND_P(mx)) || (XEN_FALSE_P(mx)) || ((MUS_XEN_P(mx)) && (mus_mixer_p(MUS_XEN_TO_CLM(mx)))), mx, XEN_ARG_6, S_mus_mix, "a mixer");
  XEN_ASSERT_TYPE((XEN_NOT_BOUND_P(envs)) || (XEN_VECTOR_P(envs)), envs, XEN_ARG_7, S_mus_mix, "an env gen or vector of envs");
  if (XEN_BOUND_P(ost)) ostart = XEN_TO_C_OFF_T_OR_ELSE(ost, 0);
  if (XEN_BOUND_P(ist)) istart = XEN_TO_C_OFF_T_OR_ELSE(ist, 0);
  if ((XEN_BOUND_P(mx)) && (MUS_XEN_P(mx))) mx1 = (mus_mixer *)MUS_XEN_TO_CLM(mx);
  outfile = XEN_TO_C_STRING(out);
  infile = XEN_TO_C_STRING(in);
  if (XEN_BOUND_P(olen)) osamps = XEN_TO_C_OFF_T_OR_ELSE(olen, 0); else osamps = mus_sound_frames(infile);
  if (XEN_BOUND_P(envs))
    {
      /* pack into a C-style array of arrays of env pointers */
      in_len = XEN_VECTOR_LENGTH(envs);
      if (in_len == 0)
	XEN_ERROR(BAD_TYPE,
		  XEN_LIST_3(C_TO_XEN_STRING(S_mus_mix),
			     envs,
			     C_TO_XEN_STRING("env vector can't be empty")));
      vdata0 = XEN_VECTOR_ELEMENTS(envs);
      for (i = 0; i < in_len; i++)
	if (!(XEN_VECTOR_P(vdata0[i])))
	  XEN_ERROR(BAD_TYPE,
		    XEN_LIST_3(C_TO_XEN_STRING(S_mus_mix),
			       vdata0[i],
			       C_TO_XEN_STRING("each element of env vector must be a vector (of envelopes)")));
      out_len = XEN_VECTOR_LENGTH(vdata0[0]);
      in_chans = mus_sound_chans(infile);
      out_chans = mus_sound_chans(outfile);
      if (in_len < in_chans) in_size = in_chans; else in_size = in_len;
      if (out_len < out_chans) out_size = out_chans; else out_size = out_len;
      envs1 = (mus_any ***)CALLOC(in_size, sizeof(mus_any **));
      for (i = 0; i < in_size; i++) envs1[i] = (mus_any **)CALLOC(out_size, sizeof(mus_any *));
      for (i = 0; i < in_len; i++)
	{
	  vdata1 = XEN_VECTOR_ELEMENTS(vdata0[i]);
	  for (j = 0; j < out_len; j++) 
	    if (MUS_XEN_P(vdata1[j]))
	      {
		if (mus_env_p(MUS_XEN_TO_CLM(vdata1[j])))
		  envs1[i][j] = MUS_XEN_TO_CLM(vdata1[j]);
		else 
		  {
		    for (i = 0; i < in_size; i++) if (envs1[i]) FREE(envs1[i]);
		    FREE(envs1);
		    XEN_ERROR(BAD_TYPE,
			      XEN_LIST_5(C_TO_XEN_STRING(S_mus_mix),
					 vdata1[j],
					 C_TO_XEN_STRING("each (non #f) element of (inner) vector must be an envelope: "),
					 C_TO_XEN_INT(i),
					 C_TO_XEN_INT(j)));
		  }
	      }
	}
    }
  mus_mix(outfile, infile, ostart, osamps, istart, mx1, envs1);
  if (envs1) 
    {
      for (i = 0; i < in_size; i++) if (envs1[i]) FREE(envs1[i]);
      FREE(envs1);
    }
  return(XEN_TRUE);
}


#ifdef XEN_ARGIFY_1
XEN_NARGIFY_0(g_srate_w, g_srate)
XEN_ARGIFY_1(g_set_srate_w, g_set_srate)
XEN_NARGIFY_0(g_array_print_length_w, g_array_print_length)
XEN_ARGIFY_1(g_set_array_print_length_w, g_set_array_print_length)
XEN_NARGIFY_1(g_radians2hz_w, g_radians2hz)
XEN_NARGIFY_1(g_hz2radians_w, g_hz2radians)
XEN_NARGIFY_1(g_radians2degrees_w, g_radians2degrees)
XEN_NARGIFY_1(g_degrees2radians_w, g_degrees2radians)
XEN_NARGIFY_1(g_db2linear_w, g_db2linear)
XEN_NARGIFY_1(g_linear2db_w, g_linear2db)
XEN_NARGIFY_2(g_ring_modulate_w, g_ring_modulate)
XEN_NARGIFY_3(g_amplitude_modulate_w, g_amplitude_modulate)
XEN_NARGIFY_2(g_contrast_enhancement_w, g_contrast_enhancement)
XEN_NARGIFY_2(g_dot_product_w, g_dot_product)
XEN_NARGIFY_1(g_clear_array_w, g_clear_array)
XEN_NARGIFY_2(g_polynomial_w, g_polynomial)
XEN_ARGIFY_3(g_multiply_arrays_w, g_multiply_arrays)
XEN_ARGIFY_3(g_make_fft_window_w, g_make_fft_window)
XEN_ARGIFY_4(g_mus_fft_w, g_mus_fft)
XEN_ARGIFY_5(g_spectrum_w, g_spectrum)
XEN_ARGIFY_3(g_convolution_w, g_convolution)
XEN_NARGIFY_2(g_rectangular2polar_w, g_rectangular2polar)
XEN_NARGIFY_2(g_polar2rectangular_w, g_polar2rectangular)
XEN_ARGIFY_3(g_array_interp_w, g_array_interp)
XEN_NARGIFY_2(g_sum_of_sines_w, g_sum_of_sines)
XEN_NARGIFY_1(g_inspect_w, g_inspect)
XEN_NARGIFY_1(g_describe_w, g_describe)
XEN_NARGIFY_1(g_name_w, g_name)
XEN_ARGIFY_3(g_run_w, g_run)
XEN_ARGIFY_4(g_mus_bank_w, g_mus_bank)
XEN_NARGIFY_1(g_phase_w, g_phase)
XEN_NARGIFY_2(g_set_phase_w, g_set_phase)
XEN_NARGIFY_1(g_width_w, g_width)
XEN_NARGIFY_2(g_set_width_w, g_set_width)
XEN_NARGIFY_1(g_scaler_w, g_scaler)
XEN_NARGIFY_2(g_set_scaler_w, g_set_scaler)
XEN_NARGIFY_1(g_offset_w, g_offset)
XEN_NARGIFY_1(g_frequency_w, g_frequency)
XEN_NARGIFY_2(g_set_frequency_w, g_set_frequency)
XEN_NARGIFY_1(g_length_w, g_length)
XEN_NARGIFY_1(g_file_name_w, g_file_name)
XEN_NARGIFY_2(g_set_length_w, g_set_length)
XEN_NARGIFY_1(g_data_w, g_data)
XEN_NARGIFY_2(g_set_data_w, g_set_data)
XEN_NARGIFY_1(g_oscil_p_w, g_oscil_p)
XEN_ARGIFY_4(g_make_oscil_w, g_make_oscil)
XEN_ARGIFY_3(g_oscil_w, g_oscil)
XEN_ARGIFY_4(g_oscil_bank_w, g_oscil_bank)
XEN_VARGIFY(g_mus_apply_w, g_mus_apply)
XEN_VARGIFY(g_make_delay_w, g_make_delay)
XEN_VARGIFY(g_make_comb_w, g_make_comb)
XEN_VARGIFY(g_make_notch_w, g_make_notch)
XEN_VARGIFY(g_make_all_pass_w, g_make_all_pass)
XEN_ARGIFY_3(g_delay_w, g_delay)
XEN_ARGIFY_2(g_tap_w, g_tap)
XEN_ARGIFY_3(g_notch_w, g_notch)
XEN_ARGIFY_3(g_comb_w, g_comb)
XEN_ARGIFY_3(g_all_pass_w, g_all_pass)
XEN_NARGIFY_1(g_delay_p_w, g_delay_p)
XEN_NARGIFY_1(g_notch_p_w, g_notch_p)
XEN_NARGIFY_1(g_comb_p_w, g_comb_p)
XEN_NARGIFY_1(g_all_pass_p_w, g_all_pass_p)
XEN_ARGIFY_6(g_make_sum_of_cosines_w, g_make_sum_of_cosines)
XEN_ARGIFY_2(g_sum_of_cosines_w, g_sum_of_cosines)
XEN_NARGIFY_1(g_sum_of_cosines_p_w, g_sum_of_cosines_p)
XEN_ARGIFY_4(g_make_rand_w, g_make_rand)
XEN_ARGIFY_4(g_make_rand_interp_w, g_make_rand_interp)
XEN_ARGIFY_2(g_rand_w, g_rand)
XEN_ARGIFY_2(g_rand_interp_w, g_rand_interp)
XEN_NARGIFY_1(g_rand_p_w, g_rand_p)
XEN_NARGIFY_1(g_rand_interp_p_w, g_rand_interp_p)
XEN_NARGIFY_1(g_mus_random_w, g_mus_random)
XEN_NARGIFY_0(g_rand_seed_w, g_rand_seed)
XEN_NARGIFY_1(g_set_rand_seed_w, g_set_rand_seed)
XEN_NARGIFY_1(g_table_lookup_p_w, g_table_lookup_p)
XEN_ARGIFY_6(g_make_table_lookup_w, g_make_table_lookup)
XEN_ARGIFY_2(g_table_lookup_w, g_table_lookup)
XEN_ARGIFY_3(g_partials2wave_w, g_partials2wave)
XEN_ARGIFY_3(g_phasepartials2wave_w, g_phasepartials2wave)
XEN_ARGIFY_6(g_make_sawtooth_wave_w, g_make_sawtooth_wave)
XEN_ARGIFY_2(g_sawtooth_wave_w, g_sawtooth_wave)
XEN_NARGIFY_1(g_sawtooth_wave_p_w, g_sawtooth_wave_p)
XEN_ARGIFY_6(g_make_triangle_wave_w, g_make_triangle_wave)
XEN_ARGIFY_2(g_triangle_wave_w, g_triangle_wave)
XEN_NARGIFY_1(g_triangle_wave_p_w, g_triangle_wave_p)
XEN_ARGIFY_6(g_make_square_wave_w, g_make_square_wave)
XEN_ARGIFY_2(g_square_wave_w, g_square_wave)
XEN_NARGIFY_1(g_square_wave_p_w, g_square_wave_p)
XEN_ARGIFY_6(g_make_pulse_train_w, g_make_pulse_train)
XEN_ARGIFY_2(g_pulse_train_w, g_pulse_train)
XEN_NARGIFY_1(g_pulse_train_p_w, g_pulse_train_p)
XEN_ARGIFY_8(g_make_asymmetric_fm_w, g_make_asymmetric_fm)
XEN_ARGIFY_3(g_asymmetric_fm_w, g_asymmetric_fm)
XEN_NARGIFY_1(g_asymmetric_fm_p_w, g_asymmetric_fm_p)
XEN_ARGIFY_4(g_make_one_zero_w, g_make_one_zero)
XEN_ARGIFY_2(g_one_zero_w, g_one_zero)
XEN_NARGIFY_1(g_one_zero_p_w, g_one_zero_p)
XEN_ARGIFY_4(g_make_one_pole_w, g_make_one_pole)
XEN_ARGIFY_2(g_one_pole_w, g_one_pole)
XEN_NARGIFY_1(g_one_pole_p_w, g_one_pole_p)
XEN_ARGIFY_6(g_make_two_zero_w, g_make_two_zero)
XEN_ARGIFY_2(g_two_zero_w, g_two_zero)
XEN_NARGIFY_1(g_two_zero_p_w, g_two_zero_p)
XEN_ARGIFY_6(g_make_two_pole_w, g_make_two_pole)
XEN_ARGIFY_2(g_two_pole_w, g_two_pole)
XEN_NARGIFY_1(g_two_pole_p_w, g_two_pole_p)
XEN_ARGIFY_4(g_make_zpolar_w, g_make_zpolar)
XEN_ARGIFY_4(g_make_ppolar_w, g_make_ppolar)
XEN_NARGIFY_1(g_a1_w, g_a1)
XEN_NARGIFY_2(g_set_a1_w, g_set_a1)
XEN_NARGIFY_1(g_b2_w, g_b2)
XEN_NARGIFY_2(g_set_b2_w, g_set_b2)
XEN_NARGIFY_1(g_a2_w, g_a2)
XEN_NARGIFY_2(g_set_a2_w, g_set_a2)
XEN_ARGIFY_3(g_formant_bank_w, g_formant_bank)
XEN_NARGIFY_1(g_formant_p_w, g_formant_p)
XEN_ARGIFY_6(g_make_formant_w, g_make_formant)
XEN_ARGIFY_2(g_formant_w, g_formant)
XEN_NARGIFY_3(g_set_formant_radius_and_frequency_w, g_set_formant_radius_and_frequency)
XEN_VARGIFY(g_make_frame_w, g_make_frame)
XEN_NARGIFY_1(g_frame_p_w, g_frame_p)
XEN_ARGIFY_3(g_frame_add_w, g_frame_add)
XEN_ARGIFY_3(g_frame_multiply_w, g_frame_multiply)
XEN_NARGIFY_2(g_frame_ref_w, g_frame_ref)
XEN_NARGIFY_3(g_set_frame_ref_w, g_set_frame_ref)
XEN_VARGIFY(g_make_mixer_w, g_make_mixer)
XEN_NARGIFY_1(g_mixer_p_w, g_mixer_p)
XEN_ARGIFY_3(g_mixer_multiply_w, g_mixer_multiply)
XEN_NARGIFY_3(g_mixer_ref_w, g_mixer_ref)
XEN_NARGIFY_4(g_set_mixer_ref_w, g_set_mixer_ref)
XEN_NARGIFY_2(g_frame2sample_w, g_frame2sample)
XEN_NARGIFY_1(g_frame2list_w, g_frame2list)
XEN_ARGIFY_3(g_frame2frame_w, g_frame2frame)
XEN_ARGIFY_3(g_sample2frame_w, g_sample2frame)
XEN_ARGIFY_4(g_make_buffer_w, g_make_buffer)
XEN_NARGIFY_1(g_buffer_p_w, g_buffer_p)
XEN_NARGIFY_1(g_buffer_empty_p_w, g_buffer_empty_p)
XEN_NARGIFY_1(g_buffer_full_p_w, g_buffer_full_p)
XEN_NARGIFY_1(g_buffer2sample_w, g_buffer2sample)
XEN_ARGIFY_2(g_buffer2frame_w, g_buffer2frame)
XEN_NARGIFY_2(g_sample2buffer_w, g_sample2buffer)
XEN_NARGIFY_2(g_frame2buffer_w, g_frame2buffer)
XEN_ARGIFY_6(g_make_wave_train_w, g_make_wave_train)
XEN_ARGIFY_2(g_wave_train_w, g_wave_train)
XEN_NARGIFY_1(g_wave_train_p_w, g_wave_train_p)
XEN_ARGIFY_8(g_make_waveshape_w, g_make_waveshape)
XEN_ARGIFY_3(g_waveshape_w, g_waveshape)
XEN_NARGIFY_1(g_waveshape_p_w, g_waveshape_p)
XEN_ARGIFY_2(g_partials2waveshape_w, g_partials2waveshape)
XEN_ARGIFY_2(g_partials2polynomial_w, g_partials2polynomial)
XEN_VARGIFY(g_make_sine_summation_w, g_make_sine_summation)
XEN_ARGIFY_2(g_sine_summation_w, g_sine_summation)
XEN_NARGIFY_1(g_sine_summation_p_w, g_sine_summation_p)
XEN_ARGIFY_6(g_make_filter_w, g_make_filter)
XEN_NARGIFY_2(g_filter_w, g_filter)
XEN_NARGIFY_1(g_filter_p_w, g_filter_p)
XEN_ARGIFY_4(g_make_fir_filter_w, g_make_fir_filter)
XEN_NARGIFY_2(g_fir_filter_w, g_fir_filter)
XEN_NARGIFY_1(g_fir_filter_p_w, g_fir_filter_p)
XEN_ARGIFY_4(g_make_iir_filter_w, g_make_iir_filter)
XEN_NARGIFY_2(g_iir_filter_w, g_iir_filter)
XEN_NARGIFY_1(g_iir_filter_p_w, g_iir_filter_p)
XEN_NARGIFY_1(g_mus_xcoeffs_w, g_mus_xcoeffs)
XEN_NARGIFY_1(g_mus_ycoeffs_w, g_mus_ycoeffs)
XEN_NARGIFY_1(g_env_p_w, g_env_p)
XEN_NARGIFY_1(g_env_w, g_env)
XEN_NARGIFY_1(g_restart_env_w, g_restart_env)
XEN_VARGIFY(g_make_env_w, g_make_env)
XEN_NARGIFY_2(g_env_interp_w, g_env_interp)
XEN_NARGIFY_1(g_file2sample_p_w, g_file2sample_p)
XEN_NARGIFY_1(g_make_file2sample_w, g_make_file2sample)
XEN_ARGIFY_3(g_file2sample_w, g_file2sample)
XEN_NARGIFY_1(g_file2frame_p_w, g_file2frame_p)
XEN_NARGIFY_1(g_make_file2frame_w, g_make_file2frame)
XEN_ARGIFY_3(g_file2frame_w, g_file2frame)
XEN_NARGIFY_1(g_sample2file_p_w, g_sample2file_p)
XEN_ARGIFY_5(g_make_sample2file_w, g_make_sample2file)
XEN_NARGIFY_1(g_continue_sample2file_w, g_continue_sample2file)
XEN_NARGIFY_4(g_sample2file_w, g_sample2file)
XEN_NARGIFY_1(g_frame2file_p_w, g_frame2file_p)
XEN_NARGIFY_3(g_frame2file_w, g_frame2file)
XEN_NARGIFY_4(g_make_frame2file_w, g_make_frame2file)
XEN_NARGIFY_1(g_input_p_w, g_input_p)
XEN_NARGIFY_1(g_output_p_w, g_output_p)
XEN_NARGIFY_3(g_in_any_w, g_in_any)
XEN_NARGIFY_2(g_ina_w, g_ina)
XEN_NARGIFY_2(g_inb_w, g_inb)
XEN_NARGIFY_4(g_out_any_w, g_out_any)
XEN_NARGIFY_3(g_outa_w, g_outa)
XEN_NARGIFY_3(g_outb_w, g_outb)
XEN_NARGIFY_3(g_outc_w, g_outc)
XEN_NARGIFY_3(g_outd_w, g_outd)
XEN_NARGIFY_5(g_array2file_w, g_array2file)
XEN_NARGIFY_5(g_file2array_w, g_file2array)
XEN_NARGIFY_1(g_mus_close_w, g_mus_close)
XEN_NARGIFY_0(g_mus_file_buffer_size_w, g_mus_file_buffer_size)
XEN_NARGIFY_1(g_mus_set_file_buffer_size_w, g_mus_set_file_buffer_size)
XEN_NARGIFY_1(g_readin_p_w, g_readin_p)
XEN_NARGIFY_1(g_readin_w, g_readin)
XEN_ARGIFY_8(g_make_readin_w, g_make_readin)
XEN_NARGIFY_1(g_channel_w, g_channel)
XEN_NARGIFY_1(g_location_w, g_location)
XEN_NARGIFY_2(g_set_location_w, g_set_location)
XEN_NARGIFY_1(g_increment_w, g_increment)
XEN_NARGIFY_2(g_set_increment_w, g_set_increment)
XEN_NARGIFY_1(g_locsig_p_w, g_locsig_p)
XEN_NARGIFY_3(g_locsig_w, g_locsig)
XEN_VARGIFY(g_make_locsig_w, g_make_locsig)
XEN_NARGIFY_3(g_move_locsig_w, g_move_locsig)
XEN_NARGIFY_0(g_locsig_type_w, g_locsig_type)
XEN_NARGIFY_1(g_set_locsig_type_w, g_set_locsig_type)
XEN_NARGIFY_1(g_channels_w, g_channels)
XEN_NARGIFY_2(g_locsig_ref_w, g_locsig_ref)
XEN_NARGIFY_2(g_locsig_reverb_ref_w, g_locsig_reverb_ref)
XEN_NARGIFY_3(g_locsig_set_w, g_locsig_set)
XEN_NARGIFY_3(g_locsig_reverb_set_w, g_locsig_reverb_set)
XEN_NARGIFY_0(g_clear_sincs_w, g_clear_sincs)
XEN_NARGIFY_1(g_src_p_w, g_src_p)
XEN_ARGIFY_3(g_src_w, g_src)
XEN_ARGIFY_6(g_make_src_w, g_make_src)
XEN_NARGIFY_1(g_granulate_p_w, g_granulate_p)
XEN_ARGIFY_2(g_granulate_w, g_granulate)
XEN_VARGIFY(g_make_granulate_w, g_make_granulate)
XEN_NARGIFY_1(g_ramp_w, g_ramp)
XEN_NARGIFY_2(g_set_ramp_w, g_set_ramp)
XEN_NARGIFY_1(g_convolve_p_w, g_convolve_p)
XEN_ARGIFY_2(g_convolve_w, g_convolve)
XEN_VARGIFY(g_make_convolve_w, g_make_convolve)
XEN_ARGIFY_4(g_convolve_files_w, g_convolve_files)
XEN_NARGIFY_1(g_phase_vocoder_p_w, g_phase_vocoder_p)
XEN_ARGIFY_2(g_phase_vocoder_w, g_phase_vocoder)
XEN_VARGIFY(g_make_phase_vocoder_w, g_make_phase_vocoder)
XEN_NARGIFY_1(g_pv_outctr_w, g_pv_outctr)
XEN_NARGIFY_2(g_pv_set_outctr_w, g_pv_set_outctr)
XEN_NARGIFY_1(g_pv_amp_increments_w, g_pv_amp_increments)
XEN_NARGIFY_1(g_pv_amps_w, g_pv_amps)
XEN_NARGIFY_1(g_pv_freqs_w, g_pv_freqs)
XEN_NARGIFY_1(g_pv_phases_w, g_pv_phases)
XEN_NARGIFY_1(g_pv_phase_increments_w, g_pv_phase_increments)
XEN_NARGIFY_1(g_hop_w, g_hop)
XEN_NARGIFY_2(g_set_hop_w, g_set_hop)
XEN_ARGIFY_7(g_mus_mix_w, g_mus_mix)
#else
#define g_srate_w g_srate
#define g_set_srate_w g_set_srate
#define g_array_print_length_w g_array_print_length
#define g_set_array_print_length_w g_set_array_print_length
#define g_radians2hz_w g_radians2hz
#define g_hz2radians_w g_hz2radians
#define g_radians2degrees_w g_radians2degrees
#define g_degrees2radians_w g_degrees2radians
#define g_db2linear_w g_db2linear
#define g_linear2db_w g_linear2db
#define g_ring_modulate_w g_ring_modulate
#define g_amplitude_modulate_w g_amplitude_modulate
#define g_contrast_enhancement_w g_contrast_enhancement
#define g_dot_product_w g_dot_product
#define g_clear_array_w g_clear_array
#define g_polynomial_w g_polynomial
#define g_multiply_arrays_w g_multiply_arrays
#define g_make_fft_window_w g_make_fft_window
#define g_mus_fft_w g_mus_fft
#define g_spectrum_w g_spectrum
#define g_convolution_w g_convolution
#define g_rectangular2polar_w g_rectangular2polar
#define g_polar2rectangular_w g_polar2rectangular
#define g_array_interp_w g_array_interp
#define g_sum_of_sines_w g_sum_of_sines
#define g_inspect_w g_inspect
#define g_describe_w g_describe
#define g_name_w g_name
#define g_run_w g_run
#define g_mus_bank_w g_mus_bank
#define g_phase_w g_phase
#define g_set_phase_w g_set_phase
#define g_scaler_w g_scaler
#define g_set_scaler_w g_set_scaler
#define g_width_w g_width
#define g_set_width_w g_set_width
#define g_offset_w g_offset
#define g_frequency_w g_frequency
#define g_set_frequency_w g_set_frequency
#define g_length_w g_length
#define g_file_name_w g_file_name
#define g_set_length_w g_set_length
#define g_data_w g_data
#define g_set_data_w g_set_data
#define g_oscil_p_w g_oscil_p
#define g_make_oscil_w g_make_oscil
#define g_oscil_w g_oscil
#define g_oscil_bank_w g_oscil_bank
#define g_mus_apply_w g_mus_apply
#define g_make_delay_w g_make_delay
#define g_make_comb_w g_make_comb
#define g_make_notch_w g_make_notch
#define g_make_all_pass_w g_make_all_pass
#define g_delay_w g_delay
#define g_tap_w g_tap
#define g_notch_w g_notch
#define g_comb_w g_comb
#define g_all_pass_w g_all_pass
#define g_delay_p_w g_delay_p
#define g_notch_p_w g_notch_p
#define g_comb_p_w g_comb_p
#define g_all_pass_p_w g_all_pass_p
#define g_make_sum_of_cosines_w g_make_sum_of_cosines
#define g_sum_of_cosines_w g_sum_of_cosines
#define g_sum_of_cosines_p_w g_sum_of_cosines_p
#define g_make_rand_w g_make_rand
#define g_make_rand_interp_w g_make_rand_interp
#define g_rand_w g_rand
#define g_rand_interp_w g_rand_interp
#define g_rand_p_w g_rand_p
#define g_rand_interp_p_w g_rand_interp_p
#define g_mus_random_w g_mus_random
#define g_rand_seed_w g_rand_seed
#define g_set_rand_seed_w g_set_rand_seed
#define g_table_lookup_p_w g_table_lookup_p
#define g_make_table_lookup_w g_make_table_lookup
#define g_table_lookup_w g_table_lookup
#define g_partials2wave_w g_partials2wave
#define g_phasepartials2wave_w g_phasepartials2wave
#define g_make_sawtooth_wave_w g_make_sawtooth_wave
#define g_sawtooth_wave_w g_sawtooth_wave
#define g_sawtooth_wave_p_w g_sawtooth_wave_p
#define g_make_triangle_wave_w g_make_triangle_wave
#define g_triangle_wave_w g_triangle_wave
#define g_triangle_wave_p_w g_triangle_wave_p
#define g_make_square_wave_w g_make_square_wave
#define g_square_wave_w g_square_wave
#define g_square_wave_p_w g_square_wave_p
#define g_make_pulse_train_w g_make_pulse_train
#define g_pulse_train_w g_pulse_train
#define g_pulse_train_p_w g_pulse_train_p
#define g_make_asymmetric_fm_w g_make_asymmetric_fm
#define g_asymmetric_fm_w g_asymmetric_fm
#define g_asymmetric_fm_p_w g_asymmetric_fm_p
#define g_make_one_zero_w g_make_one_zero
#define g_one_zero_w g_one_zero
#define g_one_zero_p_w g_one_zero_p
#define g_make_one_pole_w g_make_one_pole
#define g_one_pole_w g_one_pole
#define g_one_pole_p_w g_one_pole_p
#define g_make_two_zero_w g_make_two_zero
#define g_two_zero_w g_two_zero
#define g_two_zero_p_w g_two_zero_p
#define g_make_two_pole_w g_make_two_pole
#define g_two_pole_w g_two_pole
#define g_two_pole_p_w g_two_pole_p
#define g_make_zpolar_w g_make_zpolar
#define g_make_ppolar_w g_make_ppolar
#define g_a1_w g_a1
#define g_set_a1_w g_set_a1
#define g_b2_w g_b2
#define g_set_b2_w g_set_b2
#define g_a2_w g_a2
#define g_set_a2_w g_set_a2
#define g_formant_bank_w g_formant_bank
#define g_formant_p_w g_formant_p
#define g_make_formant_w g_make_formant
#define g_formant_w g_formant
#define g_set_formant_radius_and_frequency_w g_set_formant_radius_and_frequency
#define g_make_frame_w g_make_frame
#define g_frame_p_w g_frame_p
#define g_frame_add_w g_frame_add
#define g_frame_multiply_w g_frame_multiply
#define g_frame_ref_w g_frame_ref
#define g_set_frame_ref_w g_set_frame_ref
#define g_make_mixer_w g_make_mixer
#define g_mixer_p_w g_mixer_p
#define g_mixer_multiply_w g_mixer_multiply
#define g_mixer_ref_w g_mixer_ref
#define g_set_mixer_ref_w g_set_mixer_ref
#define g_frame2sample_w g_frame2sample
#define g_frame2list_w g_frame2list
#define g_frame2frame_w g_frame2frame
#define g_sample2frame_w g_sample2frame
#define g_make_buffer_w g_make_buffer
#define g_buffer_p_w g_buffer_p
#define g_buffer_empty_p_w g_buffer_empty_p
#define g_buffer_full_p_w g_buffer_full_p
#define g_buffer2sample_w g_buffer2sample
#define g_buffer2frame_w g_buffer2frame
#define g_sample2buffer_w g_sample2buffer
#define g_frame2buffer_w g_frame2buffer
#define g_make_wave_train_w g_make_wave_train
#define g_wave_train_w g_wave_train
#define g_wave_train_p_w g_wave_train_p
#define g_make_waveshape_w g_make_waveshape
#define g_waveshape_w g_waveshape
#define g_waveshape_p_w g_waveshape_p
#define g_partials2waveshape_w g_partials2waveshape
#define g_partials2polynomial_w g_partials2polynomial
#define g_make_sine_summation_w g_make_sine_summation
#define g_sine_summation_w g_sine_summation
#define g_sine_summation_p_w g_sine_summation_p
#define g_make_filter_w g_make_filter
#define g_filter_w g_filter
#define g_filter_p_w g_filter_p
#define g_make_fir_filter_w g_make_fir_filter
#define g_fir_filter_w g_fir_filter
#define g_fir_filter_p_w g_fir_filter_p
#define g_make_iir_filter_w g_make_iir_filter
#define g_iir_filter_w g_iir_filter
#define g_iir_filter_p_w g_iir_filter_p
#define g_mus_xcoeffs_w g_mus_xcoeffs
#define g_mus_ycoeffs_w g_mus_ycoeffs
#define g_env_p_w g_env_p
#define g_env_w g_env
#define g_restart_env_w g_restart_env
#define g_make_env_w g_make_env
#define g_env_interp_w g_env_interp
#define g_file2sample_p_w g_file2sample_p
#define g_make_file2sample_w g_make_file2sample
#define g_file2sample_w g_file2sample
#define g_file2frame_p_w g_file2frame_p
#define g_make_file2frame_w g_make_file2frame
#define g_file2frame_w g_file2frame
#define g_sample2file_p_w g_sample2file_p
#define g_make_sample2file_w g_make_sample2file
#define g_continue_sample2file_w g_continue_sample2file
#define g_sample2file_w g_sample2file
#define g_frame2file_p_w g_frame2file_p
#define g_frame2file_w g_frame2file
#define g_make_frame2file_w g_make_frame2file
#define g_input_p_w g_input_p
#define g_output_p_w g_output_p
#define g_in_any_w g_in_any
#define g_ina_w g_ina
#define g_inb_w g_inb
#define g_out_any_w g_out_any
#define g_outa_w g_outa
#define g_outb_w g_outb
#define g_outc_w g_outc
#define g_outd_w g_outd
#define g_array2file_w g_array2file
#define g_file2array_w g_file2array
#define g_mus_close_w g_mus_close
#define g_mus_file_buffer_size_w g_mus_file_buffer_size
#define g_mus_set_file_buffer_size_w g_mus_set_file_buffer_size
#define g_readin_p_w g_readin_p
#define g_readin_w g_readin
#define g_make_readin_w g_make_readin
#define g_channel_w g_channel
#define g_location_w g_location
#define g_set_location_w g_set_location
#define g_increment_w g_increment
#define g_set_increment_w g_set_increment
#define g_locsig_p_w g_locsig_p
#define g_locsig_w g_locsig
#define g_make_locsig_w g_make_locsig
#define g_move_locsig_w g_move_locsig
#define g_locsig_type_w g_locsig_type
#define g_set_locsig_type_w g_set_locsig_type
#define g_channels_w g_channels
#define g_locsig_ref_w g_locsig_ref
#define g_locsig_reverb_ref_w g_locsig_reverb_ref
#define g_locsig_set_w g_locsig_set
#define g_locsig_reverb_set_w g_locsig_reverb_set
#define g_clear_sincs_w g_clear_sincs
#define g_src_p_w g_src_p
#define g_src_w g_src
#define g_make_src_w g_make_src
#define g_granulate_p_w g_granulate_p
#define g_granulate_w g_granulate
#define g_make_granulate_w g_make_granulate
#define g_ramp_w g_ramp
#define g_set_ramp_w g_set_ramp
#define g_convolve_p_w g_convolve_p
#define g_convolve_w g_convolve
#define g_make_convolve_w g_make_convolve
#define g_convolve_files_w g_convolve_files
#define g_phase_vocoder_p_w g_phase_vocoder_p
#define g_phase_vocoder_w g_phase_vocoder
#define g_make_phase_vocoder_w g_make_phase_vocoder
#define g_pv_outctr_w g_pv_outctr
#define g_pv_set_outctr_w g_pv_set_outctr
#define g_pv_amp_increments_w g_pv_amp_increments
#define g_pv_amps_w g_pv_amps
#define g_pv_freqs_w g_pv_freqs
#define g_pv_phases_w g_pv_phases
#define g_pv_phase_increments_w g_pv_phase_increments
#define g_hop_w g_hop
#define g_set_hop_w g_set_hop
#define g_mus_mix_w g_mus_mix
#endif

#if WITH_MODULES
static void clm_init(void *ignore)
#else
void mus_xen_init(void)
#endif
{
  init_mus_module();

  mus_xen_tag = XEN_MAKE_OBJECT_TYPE("Mus", sizeof(mus_xen));
#if HAVE_GUILE
  scm_set_smob_mark(mus_xen_tag, mark_mus_xen);
  scm_set_smob_print(mus_xen_tag, print_mus_xen);
  scm_set_smob_free(mus_xen_tag, free_mus_xen);
  scm_set_smob_equalp(mus_xen_tag, equalp_mus_xen);
#if HAVE_APPLICABLE_SMOB
  scm_set_smob_apply(mus_xen_tag, XEN_PROCEDURE_CAST mus_xen_apply, 0, 2, 0);
#endif
#endif
#if HAVE_RUBY
  rb_define_method(mus_xen_tag, "to_s", XEN_PROCEDURE_CAST mus_xen_to_s, 0);
  rb_define_method(mus_xen_tag, "eql?", XEN_PROCEDURE_CAST equalp_mus_xen, 1);
  rb_define_method(mus_xen_tag, "frequency", XEN_PROCEDURE_CAST g_frequency, 0);
  rb_define_method(mus_xen_tag, "phase", XEN_PROCEDURE_CAST g_phase, 0);
  rb_define_method(mus_xen_tag, "scaler", XEN_PROCEDURE_CAST g_scaler, 0);
  rb_define_method(mus_xen_tag, "width", XEN_PROCEDURE_CAST g_width, 0);
  rb_define_method(mus_xen_tag, "offset", XEN_PROCEDURE_CAST g_offset, 0);
  rb_define_method(mus_xen_tag, "length", XEN_PROCEDURE_CAST g_length, 0);
  rb_define_method(mus_xen_tag, "data", XEN_PROCEDURE_CAST g_data, 0);
  rb_define_method(mus_xen_tag, "feedforward", XEN_PROCEDURE_CAST g_scaler, 0);
  rb_define_method(mus_xen_tag, "feedback", XEN_PROCEDURE_CAST g_increment, 0);
  rb_define_method(mus_xen_tag, "cosines", XEN_PROCEDURE_CAST g_channels, 0);
  rb_define_method(mus_xen_tag, "order", XEN_PROCEDURE_CAST g_length, 0);
  rb_define_method(mus_xen_tag, "call", XEN_PROCEDURE_CAST mus_xen_apply, 2);
  rb_define_method(mus_xen_tag, "location", XEN_PROCEDURE_CAST g_location, 0);
  rb_define_method(mus_xen_tag, "increment", XEN_PROCEDURE_CAST g_increment, 0);
  rb_define_method(mus_xen_tag, "channels", XEN_PROCEDURE_CAST g_channels, 0);
  rb_define_method(mus_xen_tag, "channel", XEN_PROCEDURE_CAST g_channel, 0);
  rb_define_method(mus_xen_tag, "xcoeffs", XEN_PROCEDURE_CAST g_mus_xcoeffs, 0);
  rb_define_method(mus_xen_tag, "ycoeffs", XEN_PROCEDURE_CAST g_mus_ycoeffs, 0);
  rb_define_method(mus_xen_tag, "ramp", XEN_PROCEDURE_CAST g_ramp, 0);
  rb_define_method(mus_xen_tag, "hop", XEN_PROCEDURE_CAST g_hop, 0);
  rb_define_method(mus_xen_tag, "a0", XEN_PROCEDURE_CAST g_scaler, 0);
  rb_define_method(mus_xen_tag, "a1", XEN_PROCEDURE_CAST g_a1, 0);
  rb_define_method(mus_xen_tag, "a2", XEN_PROCEDURE_CAST g_a2, 0);
  rb_define_method(mus_xen_tag, "b1", XEN_PROCEDURE_CAST g_increment, 0);
  rb_define_method(mus_xen_tag, "b2", XEN_PROCEDURE_CAST g_b2, 0);
#endif  

  init_keywords();

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mus_srate, g_srate_w, H_mus_srate,
				   S_mus_set_srate, g_set_srate_w,  0, 0, 0, 1);

#if HAVE_GUILE
  XEN_DEFINE_PROCEDURE(S_mus_set_srate, g_set_srate_w, 0, 1, 0, H_mus_srate);
#endif

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mus_array_print_length, g_array_print_length_w, H_mus_array_print_length,
				   S_mus_set_array_print_length, g_set_array_print_length_w,  0, 0, 0, 1);

#if HAVE_GUILE
  XEN_DEFINE_PROCEDURE(S_mus_set_array_print_length, g_set_array_print_length_w, 0, 1, 0, H_mus_array_print_length);
#endif
  XEN_DEFINE_PROCEDURE(S_radians_hz,           g_radians2hz_w, 1, 0, 0,           H_radians_hz);
  XEN_DEFINE_PROCEDURE(S_hz_radians,           g_hz2radians_w, 1, 0, 0,           H_hz_radians);
  XEN_DEFINE_PROCEDURE(S_in_hz,                g_hz2radians_w, 1, 0, 0,           H_in_hz);
  XEN_DEFINE_PROCEDURE(S_radians_degrees,      g_radians2degrees_w, 1, 0, 0,      H_radians_degrees);
  XEN_DEFINE_PROCEDURE(S_degrees_radians,      g_degrees2radians_w, 1, 0, 0,      H_degrees_radians);
  XEN_DEFINE_PROCEDURE(S_db_linear,            g_db2linear_w, 1, 0, 0,            H_db_linear);
  XEN_DEFINE_PROCEDURE(S_linear_db,            g_linear2db_w, 1, 0, 0,            H_linear_db);
  XEN_DEFINE_PROCEDURE(S_ring_modulate,        g_ring_modulate_w, 2, 0, 0,        H_ring_modulate);
  XEN_DEFINE_PROCEDURE(S_amplitude_modulate,   g_amplitude_modulate_w, 3, 0, 0,   H_amplitude_modulate);
  XEN_DEFINE_PROCEDURE(S_contrast_enhancement, g_contrast_enhancement_w, 2, 0, 0, H_contrast_enhancement);
  XEN_DEFINE_PROCEDURE(S_dot_product,          g_dot_product_w, 2, 0, 0,          H_dot_product);
  XEN_DEFINE_PROCEDURE(S_clear_array,          g_clear_array_w, 1, 0, 0,          H_clear_array);
  XEN_DEFINE_PROCEDURE(S_polynomial,           g_polynomial_w, 2, 0, 0,           H_polynomial);
  XEN_DEFINE_PROCEDURE(S_multiply_arrays,      g_multiply_arrays_w, 2, 1, 0,      H_multiply_arrays);
  XEN_DEFINE_PROCEDURE(S_make_fft_window,      g_make_fft_window_w, 2, 1, 0,      H_make_fft_window);
  XEN_DEFINE_PROCEDURE(S_mus_fft,              g_mus_fft_w, 2, 2, 0,              H_mus_fft);
  XEN_DEFINE_PROCEDURE(S_spectrum,             g_spectrum_w, 3, 2, 0,             H_mus_spectrum); 
  XEN_DEFINE_PROCEDURE(S_convolution,          g_convolution_w, 2, 1, 0,          H_mus_convolution);
  XEN_DEFINE_PROCEDURE(S_rectangular2polar,    g_rectangular2polar_w, 2, 0, 0,    H_rectangular2polar);
  XEN_DEFINE_PROCEDURE(S_polar2rectangular,    g_polar2rectangular_w, 2, 0, 0,    H_polar2rectangular);
  XEN_DEFINE_PROCEDURE(S_array_interp,         g_array_interp_w, 2, 1, 0,         H_array_interp);
  XEN_DEFINE_PROCEDURE(S_sum_of_sines,         g_sum_of_sines_w, 2, 0, 0,         H_sum_of_sines);

  #define H_rectangular_window     "The un-window, so to speak"
  #define H_hann_window            "A simple raised cosine window"
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
  #define H_dolph_chebyshev_window "window from inverse fft"

  XEN_DEFINE_CONSTANT(S_rectangular_window,     MUS_RECTANGULAR_WINDOW,     H_rectangular_window);
  XEN_DEFINE_CONSTANT(S_hann_window,            MUS_HANN_WINDOW,            H_hann_window);
  XEN_DEFINE_CONSTANT("hanning-window",         MUS_HANN_WINDOW,            H_hann_window); /* backwards compatibility */
  XEN_DEFINE_CONSTANT(S_welch_window,           MUS_WELCH_WINDOW,           H_welch_window);
  XEN_DEFINE_CONSTANT(S_parzen_window,          MUS_PARZEN_WINDOW,          H_parzen_window);
  XEN_DEFINE_CONSTANT(S_bartlett_window,        MUS_BARTLETT_WINDOW,        H_bartlett_window);
  XEN_DEFINE_CONSTANT(S_hamming_window,         MUS_HAMMING_WINDOW,         H_hamming_window);
  XEN_DEFINE_CONSTANT(S_blackman2_window,       MUS_BLACKMAN2_WINDOW,       H_blackman2_window);
  XEN_DEFINE_CONSTANT(S_blackman3_window,       MUS_BLACKMAN3_WINDOW,       H_blackman3_window);
  XEN_DEFINE_CONSTANT(S_blackman4_window,       MUS_BLACKMAN4_WINDOW,       H_blackman4_window);
  XEN_DEFINE_CONSTANT(S_exponential_window,     MUS_EXPONENTIAL_WINDOW,     H_exponential_window);
  XEN_DEFINE_CONSTANT(S_riemann_window,         MUS_RIEMANN_WINDOW,         H_riemann_window);
  XEN_DEFINE_CONSTANT(S_kaiser_window,          MUS_KAISER_WINDOW,          H_kaiser_window);
  XEN_DEFINE_CONSTANT(S_cauchy_window,          MUS_CAUCHY_WINDOW,          H_cauchy_window);
  XEN_DEFINE_CONSTANT(S_poisson_window,         MUS_POISSON_WINDOW,         H_poisson_window);
  XEN_DEFINE_CONSTANT(S_gaussian_window,        MUS_GAUSSIAN_WINDOW,        H_gaussian_window);
  XEN_DEFINE_CONSTANT(S_tukey_window,           MUS_TUKEY_WINDOW,           H_tukey_window);
  XEN_DEFINE_CONSTANT(S_dolph_chebyshev_window, MUS_DOLPH_CHEBYSHEV_WINDOW, H_dolph_chebyshev_window);

  XEN_DEFINE_CONSTANT(S_mus_linear,             MUS_LINEAR,                 "locsig linear interpolation");
  XEN_DEFINE_CONSTANT(S_mus_sinusoidal,         MUS_SINUSOIDAL,             "locsig sinusoidal interpolation");

  XEN_DEFINE_PROCEDURE(S_mus_inspect,  g_inspect_w, 1, 0, 0,  H_mus_inspect);
  XEN_DEFINE_PROCEDURE(S_mus_describe, g_describe_w, 1, 0, 0, H_mus_describe);
  XEN_DEFINE_PROCEDURE(S_mus_name,     g_name_w, 1, 0, 0,     H_mus_name);
  XEN_DEFINE_PROCEDURE(S_mus_run,      g_run_w, 1, 2, 0,      H_mus_run);
  XEN_DEFINE_PROCEDURE(S_mus_bank,     g_mus_bank_w, 2, 2, 0, H_mus_bank);
  XEN_DEFINE_PROCEDURE(S_mus_offset,   g_offset_w, 1, 0, 0,   H_mus_offset);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mus_phase, g_phase_w, H_mus_phase, S_mus_set_phase, g_set_phase_w,  1, 0, 2, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mus_scaler, g_scaler_w, H_mus_scaler, S_mus_set_scaler, g_set_scaler_w,  1, 0, 2, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mus_width, g_width_w, H_mus_width, S_mus_set_width, g_set_width_w,  1, 0, 2, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mus_frequency, g_frequency_w, H_mus_frequency, S_mus_set_frequency, g_set_frequency_w,  1, 0, 2, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mus_length, g_length_w, H_mus_length, S_mus_set_length, g_set_length_w,  1, 0, 2, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mus_data, g_data_w, H_mus_data, S_mus_set_data, g_set_data_w,  1, 0, 2, 0);
  XEN_DEFINE_PROCEDURE(S_mus_file_name, g_file_name_w, 1, 0, 0, H_mus_file_name);

  XEN_DEFINE_PROCEDURE(S_oscil_p,    g_oscil_p_w, 1, 0, 0,    H_oscil_p);
  XEN_DEFINE_PROCEDURE(S_make_oscil, g_make_oscil_w, 0, 4, 0, H_make_oscil);
  XEN_DEFINE_PROCEDURE(S_oscil,      g_oscil_w, 1, 2, 0,      H_oscil);
  XEN_DEFINE_PROCEDURE(S_oscil_bank, g_oscil_bank_w, 2, 2, 0, H_oscil_bank);
  XEN_DEFINE_PROCEDURE(S_mus_apply,  g_mus_apply_w, 0, 0, 1,  H_mus_apply);


#if HAVE_GUILE
  XEN_EVAL_C_STRING("(define %delay delay)"); /* protect the original meaning (a Scheme built-in function) */
#endif
  XEN_DEFINE_PROCEDURE(S_make_delay,    g_make_delay_w, 0, 0, 1,    H_make_delay);
  XEN_DEFINE_PROCEDURE(S_make_comb,     g_make_comb_w, 0, 0, 1,     H_make_comb);
  XEN_DEFINE_PROCEDURE(S_make_notch,    g_make_notch_w, 0, 0, 1,    H_make_notch); 
  XEN_DEFINE_PROCEDURE(S_make_all_pass, g_make_all_pass_w, 0, 0, 1, H_make_all_pass);
  XEN_DEFINE_PROCEDURE(S_delay,         g_delay_w, 1, 2, 0,         H_delay); 
  XEN_DEFINE_PROCEDURE(S_tap,           g_tap_w, 1, 1, 0,           H_tap);
  XEN_DEFINE_PROCEDURE(S_notch,         g_notch_w, 1, 2, 0,         H_notch);
  XEN_DEFINE_PROCEDURE(S_comb,          g_comb_w, 1, 2, 0,          H_comb);
  XEN_DEFINE_PROCEDURE(S_all_pass,      g_all_pass_w, 1, 2, 0,      H_all_pass);
  XEN_DEFINE_PROCEDURE(S_delay_p,       g_delay_p_w, 1, 0, 0,       H_delay_p);
  XEN_DEFINE_PROCEDURE(S_notch_p,       g_notch_p_w, 1, 0, 0,       H_notch_p);
  XEN_DEFINE_PROCEDURE(S_comb_p,        g_comb_p_w, 1, 0, 0,        H_comb_p);
  XEN_DEFINE_PROCEDURE(S_all_pass_p,    g_all_pass_p_w, 1, 0, 0,    H_all_pass_p);

  #define H_mus_feedback "(" S_mus_feedback " gen) -> feedback value of gen"
  #define H_mus_feedforward "(" S_mus_feedforward " gen) -> feedforward term of gen"
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mus_feedback, g_increment_w, H_mus_feedback, S_mus_set_feedback, g_set_increment_w,  1, 0, 2, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mus_feedforward, g_scaler_w, H_mus_feedforward, S_mus_set_feedforward, g_set_scaler_w,  1, 0, 2, 0);

  XEN_DEFINE_PROCEDURE(S_make_rand,        g_make_rand_w, 0, 4, 0,        H_make_rand);
  XEN_DEFINE_PROCEDURE(S_make_rand_interp, g_make_rand_interp_w, 0, 4, 0, H_make_rand_interp);
#if HAVE_RUBY
  rb_define_alias(rb_mKernel, "kernel_rand", "rand");
#endif
  XEN_DEFINE_PROCEDURE(S_rand,             g_rand_w, 1, 1, 0,             H_rand);
  XEN_DEFINE_PROCEDURE(S_rand_interp,      g_rand_interp_w, 1, 1, 0,      H_rand_interp);
  XEN_DEFINE_PROCEDURE(S_rand_p,           g_rand_p_w, 1, 0, 0,           H_rand_p);
  XEN_DEFINE_PROCEDURE(S_rand_interp_p,    g_rand_interp_p_w, 1, 0, 0,    H_rand_interp_p);
  XEN_DEFINE_PROCEDURE(S_mus_random,       g_mus_random_w, 1, 0, 0,       H_mus_random);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mus_rand_seed, g_rand_seed_w, H_mus_set_rand_seed,
				   S_mus_set_rand_seed, g_set_rand_seed_w, 0, 0, 1, 0);

#if HAVE_GUILE
  XEN_DEFINE_PROCEDURE(S_mus_set_rand_seed, g_set_rand_seed_w, 0, 1, 0, H_mus_set_rand_seed);
#endif

  XEN_DEFINE_PROCEDURE(S_make_sum_of_cosines, g_make_sum_of_cosines_w, 0, 6, 0, H_make_sum_of_cosines); 
  XEN_DEFINE_PROCEDURE(S_sum_of_cosines,      g_sum_of_cosines_w, 1, 1, 0,      H_sum_of_cosines);
  XEN_DEFINE_PROCEDURE(S_sum_of_cosines_p,    g_sum_of_cosines_p_w, 1, 0, 0,    H_sum_of_cosines_p);
  #define H_mus_cosines "(" S_mus_cosines " gen) -> number of cosines produced by gen"
  XEN_DEFINE_PROCEDURE(S_mus_cosines,         g_channels_w, 1, 0, 0,            H_mus_cosines);


  XEN_DEFINE_PROCEDURE(S_table_lookup_p,     g_table_lookup_p_w, 1, 0, 0,     H_table_lookup_p);
  XEN_DEFINE_PROCEDURE(S_make_table_lookup,  g_make_table_lookup_w, 0, 6, 0,  H_make_table_lookup);
  XEN_DEFINE_PROCEDURE(S_table_lookup,       g_table_lookup_w, 1, 1, 0,       H_table_lookup);
  XEN_DEFINE_PROCEDURE(S_partials2wave,      g_partials2wave_w, 1, 2, 0,      H_partials2wave);
  XEN_DEFINE_PROCEDURE(S_phasepartials2wave, g_phasepartials2wave_w, 1, 2, 0, H_phasepartials2wave);


  XEN_DEFINE_PROCEDURE(S_make_sawtooth_wave, g_make_sawtooth_wave_w, 0, 6, 0, H_make_sawtooth_wave);
  XEN_DEFINE_PROCEDURE(S_sawtooth_wave,      g_sawtooth_wave_w, 1, 1, 0,      H_sawtooth_wave);
  XEN_DEFINE_PROCEDURE(S_sawtooth_wave_p,    g_sawtooth_wave_p_w, 1, 0, 0,    H_sawtooth_wave_p);
  XEN_DEFINE_PROCEDURE(S_make_triangle_wave, g_make_triangle_wave_w, 0, 6, 0, H_make_triangle_wave);
  XEN_DEFINE_PROCEDURE(S_triangle_wave,      g_triangle_wave_w, 1, 1, 0,      H_triangle_wave);
  XEN_DEFINE_PROCEDURE(S_triangle_wave_p,    g_triangle_wave_p_w, 1, 0, 0,    H_triangle_wave_p);
  XEN_DEFINE_PROCEDURE(S_make_square_wave,   g_make_square_wave_w, 0, 6, 0,   H_make_square_wave);
  XEN_DEFINE_PROCEDURE(S_square_wave,        g_square_wave_w, 1, 1, 0,        H_square_wave);
  XEN_DEFINE_PROCEDURE(S_square_wave_p,      g_square_wave_p_w, 1, 0, 0,      H_square_wave_p);
  XEN_DEFINE_PROCEDURE(S_make_pulse_train,   g_make_pulse_train_w, 0, 6, 0,   H_make_pulse_train);
  XEN_DEFINE_PROCEDURE(S_pulse_train,        g_pulse_train_w, 1, 1, 0,        H_pulse_train);
  XEN_DEFINE_PROCEDURE(S_pulse_train_p,      g_pulse_train_p_w, 1, 0, 0,      H_pulse_train_p);


  XEN_DEFINE_PROCEDURE(S_make_asymmetric_fm, g_make_asymmetric_fm_w, 0, 8, 0, H_make_asymmetric_fm);
  XEN_DEFINE_PROCEDURE(S_asymmetric_fm,      g_asymmetric_fm_w, 1, 2, 0,      H_asymmetric_fm);
  XEN_DEFINE_PROCEDURE(S_asymmetric_fm_p,    g_asymmetric_fm_p_w, 1, 0, 0,    H_asymmetric_fm_p);


  XEN_DEFINE_PROCEDURE(S_make_one_zero, g_make_one_zero_w, 0, 4, 0, H_make_one_zero);
  XEN_DEFINE_PROCEDURE(S_one_zero,      g_one_zero_w, 1, 1, 0,      H_one_zero);
  XEN_DEFINE_PROCEDURE(S_one_zero_p,    g_one_zero_p_w, 1, 0, 0,    H_one_zero_p);
  XEN_DEFINE_PROCEDURE(S_make_one_pole, g_make_one_pole_w, 0, 4, 0, H_make_one_pole);
  XEN_DEFINE_PROCEDURE(S_one_pole,      g_one_pole_w, 1, 1, 0,      H_one_pole);
  XEN_DEFINE_PROCEDURE(S_one_pole_p,    g_one_pole_p_w, 1, 0, 0,    H_one_pole_p);
  XEN_DEFINE_PROCEDURE(S_make_two_zero, g_make_two_zero_w, 0, 6, 0, H_make_two_zero);
  XEN_DEFINE_PROCEDURE(S_two_zero,      g_two_zero_w, 1, 1, 0,      H_two_zero);
  XEN_DEFINE_PROCEDURE(S_two_zero_p,    g_two_zero_p_w, 1, 0, 0,    H_two_zero_p);
  XEN_DEFINE_PROCEDURE(S_make_two_pole, g_make_two_pole_w, 0, 6, 0, H_make_two_pole);
  XEN_DEFINE_PROCEDURE(S_two_pole,      g_two_pole_w, 1, 1, 0,      H_two_pole);
  XEN_DEFINE_PROCEDURE(S_two_pole_p,    g_two_pole_p_w, 1, 0, 0,    H_two_pole_p);
  XEN_DEFINE_PROCEDURE(S_make_zpolar,   g_make_zpolar_w, 0, 4, 0,   H_make_zpolar);
  XEN_DEFINE_PROCEDURE(S_make_ppolar,   g_make_ppolar_w, 0, 4, 0,   H_make_ppolar);

  #define H_mus_a0 "(" S_mus_a0 " gen) -> gen's " S_mus_a0 " coefficient (scaler on x(n)), if any"
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mus_a0, g_scaler_w, H_mus_a0, S_mus_set_a0, g_set_scaler_w,  1, 0, 2, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mus_a1, g_a1_w, H_mus_a1, S_mus_set_a1, g_set_a1_w,  1, 0, 2, 0);
  #define H_mus_b1 "(" S_mus_b1 " gen) -> gen's " S_mus_b1 " coefficient (scaler on y(n-1)), if any"
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mus_b1, g_increment_w, H_mus_b1, S_mus_set_b1, g_set_increment_w,  1, 0, 2, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mus_b2, g_b2_w, H_mus_b2, S_mus_set_b2, g_set_b2_w,  1, 0, 2, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mus_a2, g_a2_w, H_mus_a2, S_mus_set_a2, g_set_a2_w,  1, 0, 2, 0);

  XEN_DEFINE_PROCEDURE(S_make_wave_train, g_make_wave_train_w, 0, 6, 0, H_make_wave_train);
  XEN_DEFINE_PROCEDURE(S_wave_train,      g_wave_train_w, 1, 1, 0,      H_wave_train);
  XEN_DEFINE_PROCEDURE(S_wave_train_p,    g_wave_train_p_w, 1, 0, 0,    H_wave_train_p);


  XEN_DEFINE_PROCEDURE(S_make_buffer,    g_make_buffer_w, 0, 4, 0,    H_make_buffer);
  XEN_DEFINE_PROCEDURE(S_buffer_p,       g_buffer_p_w, 1, 0, 0,       H_buffer_p);
  XEN_DEFINE_PROCEDURE(S_buffer_empty_p, g_buffer_empty_p_w, 1, 0, 0, H_buffer_empty_p);
  XEN_DEFINE_PROCEDURE(S_buffer_full_p,  g_buffer_full_p_w, 1, 0, 0,  H_buffer_full_p);
  XEN_DEFINE_PROCEDURE(S_buffer2sample,  g_buffer2sample_w, 1, 0, 0,  H_buffer2sample);
  XEN_DEFINE_PROCEDURE(S_buffer2frame,   g_buffer2frame_w, 1, 1, 0,   H_buffer2frame);
  XEN_DEFINE_PROCEDURE(S_sample2buffer,  g_sample2buffer_w, 2, 0, 0,  H_sample2buffer);
  XEN_DEFINE_PROCEDURE(S_frame2buffer,   g_frame2buffer_w, 2, 0, 0,   H_frame2buffer);


  XEN_DEFINE_PROCEDURE(S_make_frame,     g_make_frame_w, 0, 0, 1,     H_make_frame);
#if HAVE_GUILE
  XEN_EVAL_C_STRING("(define %frame? frame?)"); /* protect the original meaning */
  /* frame? is defined in guile stacks.c scm_frame_p, but doesn't appear to be used in ice-9 */
#endif
  XEN_DEFINE_PROCEDURE(S_frame_p,        g_frame_p_w, 1, 0, 0,        H_frame_p);
  XEN_DEFINE_PROCEDURE(S_frame_add,      g_frame_add_w, 2, 1, 0,      H_frame_add);
  XEN_DEFINE_PROCEDURE(S_frame_multiply, g_frame_multiply_w, 2, 1, 0, H_frame_multiply);
#if HAVE_GUILE
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_frame_ref, g_frame_ref_w, H_frame_ref, "set-" S_frame_ref, g_set_frame_ref_w,  2, 0, 3, 0);
#else
  XEN_DEFINE_PROCEDURE(S_frame_ref,      g_frame_ref_w, 2, 0, 0,      H_frame_ref);
#endif
  XEN_DEFINE_PROCEDURE(S_frame_set,      g_set_frame_ref_w, 3, 0, 0,  H_frame_set);


  XEN_DEFINE_PROCEDURE(S_make_mixer,     g_make_mixer_w, 0, 0, 1,     H_make_mixer);
  XEN_DEFINE_PROCEDURE(S_mixer_p,        g_mixer_p_w, 1, 0, 0,        H_mixer_p);
  XEN_DEFINE_PROCEDURE(S_mixer_multiply, g_mixer_multiply_w, 2, 1, 0, H_mixer_multiply);
#if HAVE_GUILE
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mixer_ref, g_mixer_ref_w, H_mixer_ref, "set-" S_mixer_ref, g_set_mixer_ref_w,  3, 0, 4, 0);
#else
  XEN_DEFINE_PROCEDURE(S_mixer_ref,      g_mixer_ref_w, 3, 0, 0,      H_mixer_ref);
#endif
  XEN_DEFINE_PROCEDURE(S_mixer_set,      g_set_mixer_ref_w, 4, 0, 0,  H_mixer_set);
  XEN_DEFINE_PROCEDURE(S_frame2sample,   g_frame2sample_w, 2, 0, 0,   H_frame2sample);
  XEN_DEFINE_PROCEDURE(S_frame2list,     g_frame2list_w, 1, 0, 0,     H_frame2list);
  XEN_DEFINE_PROCEDURE(S_frame2frame,    g_frame2frame_w, 2, 1, 0,    H_frame2frame);
  XEN_DEFINE_PROCEDURE(S_sample2frame,   g_sample2frame_w, 2, 1, 0,   H_sample2frame);


  XEN_DEFINE_PROCEDURE(S_formant_bank, g_formant_bank_w, 2, 1, 0, H_formant_bank);
  XEN_DEFINE_PROCEDURE(S_formant_p,    g_formant_p_w, 1, 0, 0,    H_formant_p);
  XEN_DEFINE_PROCEDURE(S_make_formant, g_make_formant_w, 0, 6, 0, H_make_formant);
  XEN_DEFINE_PROCEDURE(S_formant,      g_formant_w, 1, 1, 0,      H_formant);

  #define H_mus_formant_radius  "(" S_mus_formant_radius  " gen) -> (" S_formant " generator) gen's pole radius; \
the closer the radius is to 1.0, the narrower the resonance."

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mus_formant_radius, g_phase_w, H_mus_formant_radius,
				   S_mus_set_formant_radius, g_set_phase_w,  1, 0, 2, 0);

  XEN_DEFINE_PROCEDURE(S_mus_set_formant_radius_and_frequency, g_set_formant_radius_and_frequency_w, 3, 0, 0, H_mus_set_formant_radius_and_frequency);


  XEN_DEFINE_PROCEDURE(S_make_waveshape,      g_make_waveshape_w, 0, 8, 0,      H_make_waveshape);
  XEN_DEFINE_PROCEDURE(S_waveshape,           g_waveshape_w, 1, 2, 0,           H_waveshape);
  XEN_DEFINE_PROCEDURE(S_waveshape_p,         g_waveshape_p_w, 1, 0, 0,         H_waveshape_p);
  XEN_DEFINE_PROCEDURE(S_partials2waveshape,  g_partials2waveshape_w, 1, 1, 0,  H_partials2waveshape);
  XEN_DEFINE_PROCEDURE(S_partials2polynomial, g_partials2polynomial_w, 1, 1, 0, H_partials2polynomial);


  XEN_DEFINE_PROCEDURE(S_make_sine_summation, g_make_sine_summation_w, 0, 0, 1, H_make_sine_summation);
  XEN_DEFINE_PROCEDURE(S_sine_summation,      g_sine_summation_w, 1, 1, 0,      H_sine_summation);
  XEN_DEFINE_PROCEDURE(S_sine_summation_p,    g_sine_summation_p_w, 1, 0, 0,    H_sine_summation_p);


  XEN_DEFINE_PROCEDURE(S_make_filter,     g_make_filter_w, 0, 6, 0,     H_make_filter);
  XEN_DEFINE_PROCEDURE(S_filter,          g_filter_w, 2, 0, 0,          H_filter);
  XEN_DEFINE_PROCEDURE(S_filter_p,        g_filter_p_w, 1, 0, 0,        H_filter_p);
  XEN_DEFINE_PROCEDURE(S_make_fir_filter, g_make_fir_filter_w, 0, 4, 0, H_make_fir_filter);
  XEN_DEFINE_PROCEDURE(S_fir_filter,      g_fir_filter_w, 2, 0, 0,      H_fir_filter);
  XEN_DEFINE_PROCEDURE(S_fir_filter_p,    g_fir_filter_p_w, 1, 0, 0,    H_fir_filter_p);
  XEN_DEFINE_PROCEDURE(S_make_iir_filter, g_make_iir_filter_w, 0, 4, 0, H_make_iir_filter);
  XEN_DEFINE_PROCEDURE(S_iir_filter,      g_iir_filter_w, 2, 0, 0,      H_iir_filter);
  XEN_DEFINE_PROCEDURE(S_iir_filter_p,    g_iir_filter_p_w, 1, 0, 0,    H_iir_filter_p);
  #define H_mus_order "(" S_mus_order " gen) -> gen's filter order"
  XEN_DEFINE_PROCEDURE(S_mus_order,       g_length_w, 1, 0, 0,          H_mus_order);
  XEN_DEFINE_PROCEDURE(S_mus_xcoeffs,     g_mus_xcoeffs_w, 1, 0, 0,     H_mus_xcoeffs);
  XEN_DEFINE_PROCEDURE(S_mus_ycoeffs,     g_mus_ycoeffs_w, 1, 0, 0,     H_mus_ycoeffs);


  XEN_DEFINE_PROCEDURE(S_env_p,       g_env_p_w, 1, 0, 0,       H_env_p);
  XEN_DEFINE_PROCEDURE(S_env,         g_env_w, 1, 0, 0,         H_env);
  XEN_DEFINE_PROCEDURE(S_restart_env, g_restart_env_w, 1, 0, 0, H_restart_env);
  XEN_DEFINE_PROCEDURE(S_make_env,    g_make_env_w, 0, 0, 1,    H_make_env);
  XEN_DEFINE_PROCEDURE(S_env_interp,  g_env_interp_w, 2, 0, 0,  H_env_interp);


  XEN_DEFINE_PROCEDURE(S_locsig_p,          g_locsig_p_w, 1, 0, 0,          H_locsig_p);
  XEN_DEFINE_PROCEDURE(S_locsig,            g_locsig_w, 3, 0, 0,            H_locsig);
  XEN_DEFINE_PROCEDURE(S_make_locsig,       g_make_locsig_w, 0, 0, 1,       H_make_locsig);
  XEN_DEFINE_PROCEDURE(S_move_locsig,       g_move_locsig_w, 3, 0, 0,       H_move_locsig);
  XEN_DEFINE_PROCEDURE(S_mus_channels,      g_channels_w, 1, 0, 0,          H_mus_channels);
#if HAVE_GUILE
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_locsig_ref, g_locsig_ref_w, H_locsig_ref, "set-" S_locsig_ref, g_locsig_set_w,  2, 0, 3, 0);
#else
  XEN_DEFINE_PROCEDURE(S_locsig_ref,        g_locsig_ref_w, 2, 0, 0,        H_locsig_ref);
#endif
  XEN_DEFINE_PROCEDURE(S_locsig_set,        g_locsig_set_w, 3, 0, 0,        H_locsig_set);
#if HAVE_GUILE
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_locsig_reverb_ref, g_locsig_reverb_ref_w, H_locsig_reverb_ref, 
				   S_locsig_reverb_set, g_locsig_reverb_set_w,  2, 0, 3, 0);
#else
  XEN_DEFINE_PROCEDURE(S_locsig_reverb_ref, g_locsig_reverb_ref_w, 2, 0, 0, H_locsig_reverb_ref);
#endif
  XEN_DEFINE_PROCEDURE(S_locsig_reverb_set, g_locsig_reverb_set_w, 3, 0, 0, H_locsig_reverb_set);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_locsig_type, g_locsig_type_w, H_locsig_type, S_set_locsig_type, g_set_locsig_type_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE(S_file2sample_p,    g_file2sample_p_w, 1, 0, 0,    H_file2sample_p);
  XEN_DEFINE_PROCEDURE(S_make_file2sample, g_make_file2sample_w, 1, 0, 0, H_make_file2sample);
  XEN_DEFINE_PROCEDURE(S_file2sample,      g_file2sample_w, 2, 1, 0,      H_file2sample);
  XEN_DEFINE_PROCEDURE(S_file2frame_p,     g_file2frame_p_w, 1, 0, 0,     H_file2frame_p);
  XEN_DEFINE_PROCEDURE(S_make_file2frame,  g_make_file2frame_w, 1, 0, 0,  H_make_file2frame);
  XEN_DEFINE_PROCEDURE(S_file2frame,       g_file2frame_w, 2, 1, 0,       H_file2frame);
  XEN_DEFINE_PROCEDURE(S_sample2file_p,    g_sample2file_p_w, 1, 0, 0,    H_sample2file_p);
  XEN_DEFINE_PROCEDURE(S_make_sample2file, g_make_sample2file_w, 4, 1, 0, H_make_sample2file);
  XEN_DEFINE_PROCEDURE(S_continue_sample2file, g_continue_sample2file_w, 1, 0, 0, H_continue_sample2file);
  XEN_DEFINE_PROCEDURE(S_sample2file,      g_sample2file_w, 4, 0, 0,      H_sample2file);
  XEN_DEFINE_PROCEDURE(S_frame2file_p,     g_frame2file_p_w, 1, 0, 0,     H_frame2file_p);
  XEN_DEFINE_PROCEDURE(S_frame2file,       g_frame2file_w, 3, 0, 0,       H_frame2file);
  XEN_DEFINE_PROCEDURE(S_make_frame2file,  g_make_frame2file_w, 4, 0, 0,  H_make_frame2file);
  XEN_DEFINE_PROCEDURE(S_mus_input_p,      g_input_p_w, 1, 0, 0,          H_mus_input_p);
  XEN_DEFINE_PROCEDURE(S_mus_output_p,     g_output_p_w, 1, 0, 0,         H_mus_output_p);
  XEN_DEFINE_PROCEDURE(S_in_any,           g_in_any_w, 3, 0, 0,           H_in_any);
  XEN_DEFINE_PROCEDURE(S_ina,              g_ina_w, 2, 0, 0,              H_ina);  
  XEN_DEFINE_PROCEDURE(S_inb,              g_inb_w, 2, 0, 0,              H_inb);
  XEN_DEFINE_PROCEDURE(S_out_any,          g_out_any_w, 4, 0, 0,          H_out_any);
  XEN_DEFINE_PROCEDURE(S_outa,             g_outa_w, 3, 0, 0,             H_outa);
  XEN_DEFINE_PROCEDURE(S_outb,             g_outb_w, 3, 0, 0,             H_outb);
  XEN_DEFINE_PROCEDURE(S_outc,             g_outc_w, 3, 0, 0,             H_outc);
  XEN_DEFINE_PROCEDURE(S_outd,             g_outd_w, 3, 0, 0,             H_outd);
  XEN_DEFINE_PROCEDURE(S_array2file,       g_array2file_w, 5, 0, 0,       H_array2file);
  XEN_DEFINE_PROCEDURE(S_file2array,       g_file2array_w, 5, 0, 0,       H_file2array);
  XEN_DEFINE_PROCEDURE(S_mus_close,        g_mus_close_w, 1, 0, 0,        H_mus_close);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mus_file_buffer_size, g_mus_file_buffer_size_w, H_mus_file_buffer_size,
				   "set-" S_mus_file_buffer_size, g_mus_set_file_buffer_size_w,  0, 0, 1, 0);


  XEN_DEFINE_PROCEDURE(S_readin_p,    g_readin_p_w, 1, 0, 0,    H_readin_p);
  XEN_DEFINE_PROCEDURE(S_readin,      g_readin_w, 1, 0, 0,      H_readin);
  XEN_DEFINE_PROCEDURE(S_make_readin, g_make_readin_w, 0, 8, 0, H_make_readin);
  XEN_DEFINE_PROCEDURE(S_mus_channel, g_channel_w, 1, 0, 0,     H_mus_channel);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mus_location, g_location_w, H_mus_location, S_mus_set_location, g_set_location_w,  1, 0, 2, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mus_increment, g_increment_w, H_mus_increment, S_mus_set_increment, g_set_increment_w,  1, 0, 2, 0);

  XEN_DEFINE_PROCEDURE(S_granulate_p,    g_granulate_p_w, 1, 0, 0,    H_granulate_p);
  XEN_DEFINE_PROCEDURE(S_granulate,      g_granulate_w, 1, 1, 0,      H_granulate);
  XEN_DEFINE_PROCEDURE(S_make_granulate, g_make_granulate_w, 0, 0, 1, H_make_granulate);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mus_ramp, g_ramp_w, H_mus_ramp,
				   S_mus_set_ramp, g_set_ramp_w,  1, 0, 2, 0);


  XEN_DEFINE_PROCEDURE(S_clear_sincs, g_clear_sincs_w, 0, 0, 0, "clears out any sinc tables");
  XEN_DEFINE_PROCEDURE(S_src_p,       g_src_p_w, 1, 0, 0,       H_src_p);
  XEN_DEFINE_PROCEDURE(S_src,         g_src_w, 1, 2, 0,         H_src);
  XEN_DEFINE_PROCEDURE(S_make_src,    g_make_src_w, 0, 6, 0,    H_make_src);


  XEN_DEFINE_PROCEDURE(S_convolve_p,     g_convolve_p_w, 1, 0, 0,     H_convolve_p);
  XEN_DEFINE_PROCEDURE(S_convolve,       g_convolve_w, 1, 1, 0,       H_convolve_gen);
  XEN_DEFINE_PROCEDURE(S_make_convolve,  g_make_convolve_w, 0, 0, 1,  H_make_convolve);
  XEN_DEFINE_PROCEDURE(S_convolve_files, g_convolve_files_w, 2, 2, 0, H_convolve_files);

  XEN_DEFINE_PROCEDURE(S_phase_vocoder_p,       g_phase_vocoder_p_w, 1, 0, 0,     H_phase_vocoder_p);
  XEN_DEFINE_PROCEDURE(S_phase_vocoder,         g_phase_vocoder_w, 1, 1, 0,       H_phase_vocoder);
  XEN_DEFINE_PROCEDURE(S_make_phase_vocoder,    g_make_phase_vocoder_w, 0, 0, 1,  H_make_phase_vocoder);
  XEN_DEFINE_PROCEDURE(S_pv_amp_increments,     g_pv_amp_increments_w, 1, 0, 0,   H_pv_amp_increments);
  XEN_DEFINE_PROCEDURE(S_pv_amps,               g_pv_amps_w, 1, 0, 0,             H_pv_amps);
  XEN_DEFINE_PROCEDURE(S_pv_freqs,              g_pv_freqs_w, 1, 0, 0,            H_pv_freqs);
  XEN_DEFINE_PROCEDURE(S_pv_phases,             g_pv_phases_w, 1, 0, 0,           H_pv_phases);
  XEN_DEFINE_PROCEDURE(S_pv_phase_increments,   g_pv_phase_increments_w, 1, 0, 0, H_pv_phase_increments);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_pv_outctr, g_pv_outctr_w, H_pv_outctr, "set-" S_pv_outctr, g_pv_set_outctr_w,  1, 0, 2, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mus_hop, g_hop_w, H_mus_hop, S_mus_set_hop, g_set_hop_w,  1, 0, 2, 0);

  XEN_DEFINE_PROCEDURE(S_mus_mix, g_mus_mix_w, 2, 5, 0, H_mus_mix);

  XEN_YES_WE_HAVE("clm");
#if WITH_MODULES
  scm_c_export(S_mus_srate,
	       S_mus_set_srate,
	       S_mus_array_print_length,
	       S_mus_set_array_print_length,
	       S_radians_hz,
	       S_hz_radians,
	       S_in_hz,
	       S_radians_degrees,
	       S_degrees_radians,
	       S_db_linear,
	       S_linear_db,
	       S_ring_modulate,
	       S_amplitude_modulate,
	       S_contrast_enhancement,
	       S_dot_product,
	       S_clear_array,
	       S_polynomial,
	       S_multiply_arrays,
	       S_make_fft_window,
	       S_mus_fft,
	       S_spectrum,
	       S_convolution,
	       S_rectangular2polar,
	       S_polar2rectangular,
	       S_array_interp,
	       S_sum_of_sines,
	       S_rectangular_window,
	       S_hann_window,
	       "hanning-window",
	       S_welch_window,
	       S_parzen_window,
	       S_bartlett_window,
	       S_hamming_window,
	       S_blackman2_window,
	       S_blackman3_window,
	       S_blackman4_window,
	       S_exponential_window,
	       S_riemann_window,
	       S_kaiser_window,
	       S_cauchy_window,
	       S_poisson_window,
	       S_gaussian_window,
	       S_tukey_window,
	       S_dolph_chebyshev_window,
	       S_mus_linear,
	       S_mus_sinusoidal,
	       S_mus_inspect,
	       S_mus_describe,
	       S_mus_name,
	       S_mus_run,
	       S_mus_bank,
	       S_mus_phase,
	       S_mus_scaler,
	       S_mus_frequency,
	       S_mus_length,
	       S_mus_data,
	       S_mus_file_name,
	       S_oscil_p,
	       S_make_oscil,
	       S_oscil,
	       S_oscil_bank,
	       S_mus_apply,
	       S_make_delay,
	       S_make_comb,
	       S_make_notch,
	       S_make_all_pass,
	       S_delay,
	       S_tap,
	       S_notch,
	       S_comb,
	       S_all_pass,
	       S_delay_p,
	       S_notch_p,
	       S_comb_p,
	       S_all_pass_p,
	       S_mus_feedback,
	       S_mus_feedforward,
	       S_make_rand,
	       S_make_rand_interp,
	       S_rand,
	       S_rand_interp,
	       S_rand_p,
	       S_rand_interp_p,
	       S_mus_random,
	       S_mus_rand_seed,
	       S_mus_set_rand_seed,
	       S_make_sum_of_cosines,
	       S_sum_of_cosines,
	       S_sum_of_cosines_p,
	       S_mus_cosines,
	       S_table_lookup_p,
	       S_make_table_lookup,
	       S_table_lookup,
	       S_partials2wave,
	       S_phasepartials2wave,
	       S_make_sawtooth_wave,
	       S_sawtooth_wave,
	       S_sawtooth_wave_p,
	       S_make_triangle_wave,
	       S_triangle_wave,
	       S_triangle_wave_p,
	       S_make_square_wave,
	       S_square_wave,
	       S_square_wave_p,
	       S_make_pulse_train,
	       S_pulse_train,
	       S_pulse_train_p,
	       S_make_asymmetric_fm,
	       S_asymmetric_fm,
	       S_asymmetric_fm_p,
	       S_make_one_zero,
	       S_one_zero,
	       S_one_zero_p,
	       S_make_one_pole,
	       S_one_pole,
	       S_one_pole_p,
	       S_make_two_zero,
	       S_two_zero,
	       S_two_zero_p,
	       S_make_two_pole,
	       S_two_pole,
	       S_two_pole_p,
	       S_make_zpolar,
	       S_make_ppolar,
	       S_mus_a0,
	       S_mus_a1,
	       S_mus_b1,
	       S_mus_b2,
	       S_mus_a2,
	       S_make_wave_train,
	       S_wave_train,
	       S_wave_train_p,
	       S_make_buffer,
	       S_buffer_p,
	       S_buffer_empty_p,
	       S_buffer_full_p,
	       S_buffer2sample,
	       S_buffer2frame,
	       S_sample2buffer,
	       S_frame2buffer,
	       S_make_frame,
	       S_frame_p,
	       S_frame_add,
	       S_frame_multiply,
	       S_frame_ref,
	       S_frame_set,
	       S_make_mixer,
	       S_mixer_p,
	       S_mixer_multiply,
	       S_mixer_ref,
	       S_mixer_set,
	       S_frame2sample,
	       S_frame2list,
	       S_frame2frame,
	       S_sample2frame,
	       S_formant_bank,
	       S_formant_p,
	       S_make_formant,
	       S_formant,
	       S_mus_formant_radius,
	       S_mus_set_formant_radius_and_frequency,
	       S_make_waveshape,
	       S_waveshape,
	       S_waveshape_p,
	       S_partials2waveshape,
	       S_partials2polynomial,
	       S_make_sine_summation,
	       S_sine_summation,
	       S_sine_summation_p,
	       S_make_filter,
	       S_filter,
	       S_filter_p,
	       S_make_fir_filter,
	       S_fir_filter,
	       S_fir_filter_p,
	       S_make_iir_filter,
	       S_iir_filter,
	       S_iir_filter_p,
	       S_mus_order,
	       S_mus_xcoeffs,
	       S_mus_ycoeffs,
	       S_env_p,
	       S_env,
	       S_restart_env,
	       S_make_env,
	       S_env_interp,
	       S_locsig_p,
	       S_locsig,
	       S_make_locsig,
	       S_move_locsig,
	       S_mus_channels,
	       S_locsig_ref,
	       S_locsig_set,
	       S_locsig_reverb_ref,
	       S_locsig_reverb_set,
	       S_locsig_type,
	       S_file2sample_p,
	       S_make_file2sample,
	       S_file2sample,
	       S_file2frame_p,
	       S_make_file2frame,
	       S_file2frame,
	       S_sample2file_p,
	       S_make_sample2file,
	       S_continue_sample2file,
	       S_sample2file,
	       S_frame2file_p,
	       S_frame2file,
	       S_make_frame2file,
	       S_mus_input_p,
	       S_mus_output_p,
	       S_in_any,
	       S_ina,
	       S_inb,
	       S_out_any,
	       S_outa,
	       S_outb,
	       S_outc,
	       S_outd,
	       S_array2file,
	       S_file2array,
	       S_mus_close,
	       S_mus_file_buffer_size,
	       S_readin_p,
	       S_readin,
	       S_make_readin,
	       S_mus_channel,
	       S_mus_location,
	       S_mus_increment,
	       S_granulate_p,
	       S_granulate,
	       S_make_granulate,
	       S_mus_ramp,
	       S_clear_sincs,
	       S_src_p,
	       S_src,
	       S_make_src,
	       S_convolve_p,
	       S_convolve,
	       S_make_convolve,
	       S_convolve_files,
	       S_phase_vocoder_p,
	       S_phase_vocoder,
	       S_make_phase_vocoder,
	       S_pv_amp_increments,
	       S_pv_amps,
	       S_pv_freqs,
	       S_pv_phases,
	       S_pv_phase_increments,
	       S_pv_outctr,
	       S_mus_hop,
	       S_mus_mix,
	       NULL);
#endif
}

#if WITH_MODULES
void mus_xen_init(void)
{
  scm_c_define_module("snd clm", clm_init, NULL);
}
#endif


#if HAVE_RUBY
void Init_sndlib(void)
#else
void init_sndlib(void)
#endif
{
  mus_sndlib2xen_initialize();
  init_vct();
  mus_xen_init();
}
