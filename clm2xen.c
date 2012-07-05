/* tie CLM module into Scheme, Ruby, or Forth */

/*     
 *  we have mus-sound-srate in sndlib, mus-srate in clm.c, sound-srate and *clm-srate* in clm, mus-sound-srate and srate in snd
 *    perhaps a mus module, giving mus:sound-srate in xen, mus:sound-srate in clm, mus_sound_srate in C?
 */

/* if the optimizer stops working inexplicably, look for any symbols used before this that
 *    might shadow a generator name; one such case was (make-hook 'env...) in snd-env.c
 */

#include <mus-config.h>

#if USE_SND
  #include "snd.h"
#endif

#include <stddef.h>
#include <math.h>
#include <stdio.h>
#include <errno.h>
#include <stdlib.h>
#include <limits.h>
#if HAVE_STRING_H
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

#include "_sndlib.h"
#include "xen.h"
#include "clm.h"
#include "sndlib2xen.h"
#include "vct.h"
#include "clm2xen.h"
#include "clm-strings.h"

#ifndef TWO_PI
  #define TWO_PI (2.0 * M_PI)
#endif

#ifndef PROC_FALSE
  #if HAVE_RUBY
    #define PROC_FALSE "false"
    #define PROC_TRUE "true"
  #else
    #define PROC_FALSE "#f"
    #define PROC_TRUE  "#t"
  #endif
#endif

struct mus_xen {
  mus_any *gen;
  s7_pointer *fields;
  s7_pointer *methods;
  s7_pointer envir;

  XEN *vcts; /* one for each accessible mus_float_t array (wrapped up here in a vct) */
  int nvcts;
  bool dont_free_gen;

  struct ptree *input_ptree; /* added 24-Apr-02 for run optimizer */
  struct ptree *edit_ptree;  /* ditto 26-Jul-04 */
  struct ptree *analyze_ptree;
  struct ptree *synthesize_ptree;
};

mus_any *mus_xen_gen(mus_xen *x) {return(x->gen);}
struct ptree *mus_xen_input(mus_xen *x) {return(x->input_ptree);}
void mus_xen_set_input(mus_xen *x, struct ptree *input) {x->input_ptree = input;}
struct ptree *mus_xen_edit(mus_xen *x) {return(x->edit_ptree);}
void mus_xen_set_edit(mus_xen *x, struct ptree *edit) {x->edit_ptree = edit;}
struct ptree *mus_xen_analyze(mus_xen *x) {return(x->analyze_ptree);}
void mus_xen_set_analyze(mus_xen *x, struct ptree *analyze) {x->analyze_ptree = analyze;}
struct ptree *mus_xen_synthesize(mus_xen *x) {return(x->synthesize_ptree);}
void mus_xen_set_synthesize(mus_xen *x, struct ptree *synthesize) {x->synthesize_ptree = synthesize;}
void mus_xen_set_dont_free(mus_xen *x, bool val) {x->dont_free_gen = val;}

/* new mus_* accessors -- go direct to class, if null CHECK_METHOD in effect using the envir as an open-env
 *   shadow all the built-in methods with s7_pointers (*methods above), and wrappers that
 *   access them so the path is mus-* -> class -> wrapper -> methods[n] -> s7_call
 * for defgen local fields, they currently use implicit indexing of the generator as a list/vector
 *   this could be an index into *fields above
 * for added methods, I guess envir -- no current defgen uses these
 * make, ?, and apply are defined in place in scheme, but at least make needs to call something
 *   to get the xen side set up.
  osc *gen;
  gen = (osc *)calloc(1, sizeof(osc));
  gen->core = &OSCIL_CLASS;

  gn = (mus_xen *)calloc(1, sizeof(mus_xen));
      gn->gen = ge; -- a bare mus_any struct (i.e. just the core pointer?)
  return mus_xen_to_object(gn)
      do we need vcts/self-wrapper?
 */

#if HAVE_SCHEME
#define DISPLAY(Expr) s7_object_to_c_string(sc, Expr)
#define DISPLAY_80(Expr) s7_object_to_c_string(sc, Expr)
#endif


#define XEN_TO_C_DOUBLE_IF_BOUND(Xen_Arg, C_Val, Caller, ArgNum) \
  if (XEN_BOUND_P(Xen_Arg)) {if (XEN_NUMBER_P(Xen_Arg)) C_Val = XEN_TO_C_DOUBLE(Xen_Arg); else XEN_ASSERT_TYPE(false, Xen_Arg, ArgNum, Caller, "a number");}

#define XEN_TO_C_DOUBLE_OR_ERROR(Xen_Arg, C_Val, Caller, ArgNum) \
   if (XEN_NUMBER_P(Xen_Arg)) C_Val = XEN_TO_C_DOUBLE(Xen_Arg); else XEN_ASSERT_TYPE(false, Xen_Arg, ArgNum, Caller, "a number")

#define XEN_TO_C_INTEGER_OR_ERROR(Xen_Arg, C_Val, Caller, ArgNum) \
  if (XEN_INTEGER_P(Xen_Arg)) C_Val = XEN_TO_C_INT(Xen_Arg); else XEN_ASSERT_TYPE(false, Xen_Arg, ArgNum, Caller, "an integer")

#define XEN_TO_C_GENERATOR(Xen_Arg, C_Val, Checker, Caller, Descr) \
  XEN_ASSERT_TYPE((MUS_XEN_P(Xen_Arg)) && (Checker(C_Val = XEN_TO_MUS_ANY(Xen_Arg))), Xen_Arg, XEN_ARG_1, Caller, Descr)

#define XEN_TO_C_ANY_GENERATOR(Xen_Arg, C_Val, Caller, Descr) \
  XEN_ASSERT_TYPE((MUS_XEN_P(Xen_Arg)) && (C_Val = XEN_TO_MUS_ANY(Xen_Arg)), Xen_Arg, XEN_ARG_1, Caller, Descr)



#define MAX_ARGLIST_LEN 24
/* try to accommodate &other-keys essentially */


static int local_error_type = MUS_NO_ERROR;
static char *local_error_msg = NULL;

static void local_mus_error(int type, char *msg)
{
  local_error_type = type;
  if (local_error_msg) free(local_error_msg);
  local_error_msg = mus_strdup(msg);
}


static XEN clm_mus_error(int type, const char *msg)
{
  mus_error(type, "%s", msg);
  return(XEN_FALSE);
}


#define CLM_ERROR XEN_ERROR_TYPE("mus-error")

static void clm_error(const char *caller, const char *msg, XEN val)
{
  XEN_ERROR(CLM_ERROR,
	    XEN_LIST_4(C_TO_XEN_STRING("~A: ~A ~A"),
		       C_TO_XEN_STRING(caller),
		       C_TO_XEN_STRING(msg),
		       val));
}



/* ---------------- optional-key ---------------- */

int mus_optkey_unscramble(const char *caller, int nkeys, XEN *keys, XEN *args, int *orig)
{
  /* implement the &optional-key notion in CLM */
  /* "keys" holds the keywords the calling function accepts, 
   *   upon return, if a key was given in the arglist or its position had a value, the corresponding value is in its keys location
   * "nkeys is the size of "keys"
   * "args" contains the original arguments passed to the function in order
   *   it should be of size nkeys * 2, and any trailing (unspecified) args should be XEN_UNDEFINED
   * "orig" should be of size nkeys, and will contain upon return the 1-based location of the original keyword value argument
   *  (it is intended for error reports)
   */
  int arg_ctr = 0, key_start = 0, rtn_ctr = 0, nargs;
  bool keying = false, key_found = false;
  nargs = nkeys * 2;

  while ((arg_ctr < nargs) && 
	 (XEN_BOUND_P(args[arg_ctr])))
    {
      if (!(XEN_KEYWORD_P(args[arg_ctr])))
	{
	  if (keying) 
	    clm_error(caller, "unmatched value within keyword section?", args[arg_ctr]);
	  /* type checking on the actual values has to be the caller's problem */

	  if (arg_ctr >= nkeys)
	    clm_error(caller, "extra trailing args?", args[arg_ctr]);

	  keys[arg_ctr] = args[arg_ctr];
	  orig[arg_ctr] = arg_ctr + 1;
	  arg_ctr++;
	  key_start = arg_ctr;
	  rtn_ctr++;
	}
      else
	{
	  XEN key;
	  int i;

	  if ((arg_ctr == (nargs - 1)) ||
	      (!(XEN_BOUND_P(args[arg_ctr + 1]))))
	    clm_error(caller, "keyword without value?", args[arg_ctr]);

	  keying = true;
	  key = args[arg_ctr];

	  if (XEN_KEYWORD_P(args[arg_ctr + 1])) 
	    clm_error(caller, "two keywords in a row?", key);

	  key_found = false;
	  for (i = key_start; i < nkeys; i++)
	    {
	      if (XEN_KEYWORD_EQ_P(keys[i], key))
		{
		  keys[i] = args[arg_ctr + 1];
		  orig[i] = arg_ctr + 2;
		  arg_ctr += 2;
		  rtn_ctr++;
		  key_found = true;
		}
	    }

	  if (!key_found)
	    {
	      /* either there's a redundant keyword pair or a keyword that 'caller' doesn't recognize */
	      clm_error(caller, "redundant or invalid key found", key);
	      /* normally (all local cases) the error returns */
	      arg_ctr += 2;
	    }
	}
    }
  return(rtn_ctr);
}


mus_float_t mus_optkey_to_float(XEN key, const char *caller, int n, mus_float_t def)
{
  if (!(XEN_KEYWORD_P(key)))
    {
      XEN_ASSERT_TYPE(XEN_NUMBER_P(key), key, n, caller, "a number");
      return(XEN_TO_C_DOUBLE(key));
    }
  return(def);
}


int mus_optkey_to_int(XEN key, const char *caller, int n, int def)
{
  if (!(XEN_KEYWORD_P(key)))
    {
      XEN_ASSERT_TYPE(XEN_INTEGER_P(key), key, n, caller, "an integer");
      return(XEN_TO_C_INT(key));
    }
  return(def);
}


bool mus_optkey_to_bool(XEN key, const char *caller, int n, bool def)
{
  if (!(XEN_KEYWORD_P(key)))
    {
      XEN_ASSERT_TYPE(XEN_BOOLEAN_P(key), key, n, caller, "#f or #t");
      return(XEN_TO_C_BOOLEAN(key));
    }
  return(def);
}


mus_long_t mus_optkey_to_mus_long_t(XEN key, const char *caller, int n, mus_long_t def)
{
  if (!(XEN_KEYWORD_P(key)))
    {
      XEN_ASSERT_TYPE(XEN_INTEGER_P(key), key, n, caller, "a sample number");
      return(XEN_TO_C_LONG_LONG_OR_ELSE(key, def));
    }
  return(def);
}


const char *mus_optkey_to_string(XEN key, const char *caller, int n, char *def)
{
  if ((!(XEN_KEYWORD_P(key))) && (!(XEN_FALSE_P(key))))
    {
      XEN_ASSERT_TYPE(XEN_STRING_P(key), key, n, caller, "a string");
      return(XEN_TO_C_STRING(key));
    }
  return(def);
}


static vct *mus_optkey_to_vct(XEN key, const char *caller, int n, vct *def)
{
  if ((!(XEN_KEYWORD_P(key))) && (!(XEN_FALSE_P(key))))
    {
      XEN_ASSERT_TYPE(MUS_VCT_P(key), key, n, caller, "a vct");
      return(XEN_TO_VCT(key));
    }
  return(def);
}


static bool local_arity_ok(XEN proc, int args) /* from snd-xen.c minus (inconvenient) gc protection */
{
  XEN arity;
  int rargs;
  arity = XEN_ARITY(proc);

#if HAVE_RUBY
  rargs = XEN_TO_C_INT(arity);
  return(xen_rb_arity_ok(rargs, args));
#endif

#if HAVE_FORTH
  rargs = XEN_TO_C_INT(arity);
  return(rargs == args);
#endif

#if HAVE_SCHEME
  {
    int oargs, restargs;
    rargs = XEN_TO_C_INT(XEN_CAR(arity));
    oargs = XEN_TO_C_INT(XEN_CADR(arity));
    restargs = ((XEN_TRUE_P(XEN_CADDR(arity))) ? 1 : 0);
    if (rargs > args) return(false);
    if ((restargs == 0) && ((rargs + oargs) < args)) return(false);
  }
#endif

  return(true);
}


XEN mus_optkey_to_procedure(XEN key, const char *caller, int n, XEN def, int required_args, const char *err)
{
  if ((!(XEN_KEYWORD_P(key))) && (!(XEN_FALSE_P(key))))
    {
      XEN_ASSERT_TYPE(XEN_PROCEDURE_P(key), key, n, caller, "a procedure");
      if (!(local_arity_ok(key, required_args)))
	XEN_BAD_ARITY_ERROR(caller, n, key, err);
      return(key);
    }
  return(def);
}

/* mus_optkey_to_mus_any and mus_optkey_to_input_procedure are below where MUS_XEN_P et al are defined */


/* ---------------- clm keywords ---------------- */

static XEN kw_frequency, kw_initial_phase, kw_wave, kw_amplitude,
  kw_r, kw_ratio, kw_size, kw_a0, kw_a1, kw_a2, kw_b1, kw_b2, kw_max_size,
  kw_input, kw_srate, kw_file, kw_channel, kw_start,
  kw_initial_contents, kw_initial_element, kw_scaler, kw_feedforward, kw_feedback,
  kw_radius, kw_partials, kw_a, kw_n,
  kw_order, kw_x_coeffs, kw_y_coeffs, kw_envelope, kw_base, kw_duration, kw_offset, kw_end,
  kw_direction, kw_degree, kw_distance, kw_reverb, kw_output, kw_fft_size,
  kw_expansion, kw_length, kw_hop, kw_ramp, kw_jitter,
  kw_type, kw_channels, kw_filter, kw_revout, kw_width,
  kw_edit, kw_synthesize, kw_analyze, kw_interp, kw_overlap, kw_pitch,
  kw_distribution, kw_coeffs, kw_kind;


static void init_keywords(void)
{
  /* in Ruby there's rb_intern of the symbol -- is it safe? */
  kw_frequency =        XEN_MAKE_KEYWORD("frequency");
  kw_initial_phase =    XEN_MAKE_KEYWORD("initial-phase");
  kw_wave =             XEN_MAKE_KEYWORD("wave");
  kw_amplitude =        XEN_MAKE_KEYWORD("amplitude");
  kw_r =                XEN_MAKE_KEYWORD("r");
  kw_ratio =            XEN_MAKE_KEYWORD("ratio");
  kw_size =             XEN_MAKE_KEYWORD("size");
  kw_a0 =               XEN_MAKE_KEYWORD("a0");
  kw_a1 =               XEN_MAKE_KEYWORD("a1");
  kw_a2 =               XEN_MAKE_KEYWORD("a2");
  kw_b1 =               XEN_MAKE_KEYWORD("b1");
  kw_b2 =               XEN_MAKE_KEYWORD("b2");
  kw_max_size =         XEN_MAKE_KEYWORD("max-size");
  kw_input =            XEN_MAKE_KEYWORD("input");
  kw_srate =            XEN_MAKE_KEYWORD("srate");
  kw_file =             XEN_MAKE_KEYWORD("file");
  kw_channel =          XEN_MAKE_KEYWORD("channel");
  kw_start =            XEN_MAKE_KEYWORD("start");  /* make-readin */
  kw_initial_contents = XEN_MAKE_KEYWORD("initial-contents");
  kw_initial_element =  XEN_MAKE_KEYWORD("initial-element");
  kw_scaler =           XEN_MAKE_KEYWORD("scaler");
  kw_feedforward =      XEN_MAKE_KEYWORD("feedforward");
  kw_feedback =         XEN_MAKE_KEYWORD("feedback");
  kw_radius =           XEN_MAKE_KEYWORD("radius");
  kw_partials =         XEN_MAKE_KEYWORD("partials");
  kw_a =                XEN_MAKE_KEYWORD("a");
  kw_n =                XEN_MAKE_KEYWORD("n");
  kw_order =            XEN_MAKE_KEYWORD("order");
  kw_x_coeffs =         XEN_MAKE_KEYWORD("xcoeffs");
  kw_y_coeffs =         XEN_MAKE_KEYWORD("ycoeffs");
  kw_envelope =         XEN_MAKE_KEYWORD("envelope");
  kw_base =             XEN_MAKE_KEYWORD("base");
  kw_duration =         XEN_MAKE_KEYWORD("duration");
  kw_offset =           XEN_MAKE_KEYWORD("offset");
  kw_end =              XEN_MAKE_KEYWORD("end");
  kw_direction =        XEN_MAKE_KEYWORD("direction");
  kw_degree =           XEN_MAKE_KEYWORD("degree");
  kw_distance =         XEN_MAKE_KEYWORD("distance");
  kw_reverb =           XEN_MAKE_KEYWORD("reverb");
  kw_output =           XEN_MAKE_KEYWORD("output");
  kw_fft_size =         XEN_MAKE_KEYWORD("fft-size");
  kw_expansion =        XEN_MAKE_KEYWORD("expansion");
  kw_length =           XEN_MAKE_KEYWORD("length");
  kw_hop =              XEN_MAKE_KEYWORD("hop");
  kw_ramp =             XEN_MAKE_KEYWORD("ramp");
  kw_jitter =           XEN_MAKE_KEYWORD("jitter");
  kw_type =             XEN_MAKE_KEYWORD("type");
  kw_channels =         XEN_MAKE_KEYWORD("channels");
  kw_filter =           XEN_MAKE_KEYWORD("filter");
  kw_revout =           XEN_MAKE_KEYWORD("revout");
  kw_width =            XEN_MAKE_KEYWORD("width");
  kw_edit =             XEN_MAKE_KEYWORD("edit");
  kw_synthesize =       XEN_MAKE_KEYWORD("synthesize");
  kw_analyze =          XEN_MAKE_KEYWORD("analyze");
  kw_interp =           XEN_MAKE_KEYWORD("interp");
  kw_overlap =          XEN_MAKE_KEYWORD("overlap");
  kw_pitch =            XEN_MAKE_KEYWORD("pitch");
  kw_distribution =     XEN_MAKE_KEYWORD("distribution");
  kw_coeffs =           XEN_MAKE_KEYWORD("coeffs");
  kw_kind =             XEN_MAKE_KEYWORD("kind");
}



/* ---------------- *clm-table-size* ---------------- */

static mus_long_t clm_table_size = MUS_CLM_DEFAULT_TABLE_SIZE;

mus_long_t clm_default_table_size_c(void) {return(clm_table_size);}

static XEN g_clm_table_size(void) {return(C_TO_XEN_LONG_LONG(clm_table_size));}

static XEN g_set_clm_table_size(XEN val) 
{
  mus_long_t size;
  #define H_clm_table_size "(" S_clm_table_size "): the default table size for most generators (512)"
  XEN_ASSERT_TYPE(XEN_LONG_LONG_P(val), val, XEN_ONLY_ARG, S_setB S_clm_table_size, "an integer");
  size = XEN_TO_C_LONG_LONG(val);
  if ((size <= 0) || 
      (size > mus_max_table_size()))
    XEN_OUT_OF_RANGE_ERROR(S_setB S_clm_table_size, XEN_ARG_1, val, "invalid size: ~A (see mus-max-table-size)");
  clm_table_size = size;
  return(C_TO_XEN_LONG_LONG(clm_table_size));
}


/* ---------------- *clm-default-frequency* ---------------- */

static double clm_default_frequency = MUS_CLM_DEFAULT_FREQUENCY;

double clm_default_frequency_c(void) {return(clm_default_frequency);}

static XEN g_clm_default_frequency(void) {return(C_TO_XEN_DOUBLE(clm_default_frequency));}

static XEN g_set_clm_default_frequency(XEN val) 
{
  #define H_clm_default_frequency "(" S_clm_default_frequency "): the default frequency for most generators (0.0)"
  XEN_ASSERT_TYPE(XEN_DOUBLE_P(val), val, XEN_ONLY_ARG, S_setB S_clm_default_frequency, "a number");
  clm_default_frequency = XEN_TO_C_DOUBLE(val);
  return(val);
}


/* ---------------- AM and simple stuff ---------------- */

static const char *fft_window_xen_names[MUS_NUM_FFT_WINDOWS] = 
    {S_rectangular_window, S_hann_window, S_welch_window, S_parzen_window, S_bartlett_window,
     S_hamming_window, S_blackman2_window, S_blackman3_window, S_blackman4_window,
     S_exponential_window, S_riemann_window, S_kaiser_window, S_cauchy_window,
     S_poisson_window, S_gaussian_window, S_tukey_window, S_dolph_chebyshev_window,
     S_hann_poisson_window, S_connes_window, S_samaraki_window, S_ultraspherical_window,
     S_bartlett_hann_window, S_bohman_window, S_flat_top_window,
     S_blackman5_window, S_blackman6_window, S_blackman7_window, S_blackman8_window, S_blackman9_window, S_blackman10_window,
     S_rv2_window, S_rv3_window, S_rv4_window, S_mlt_sine_window, S_papoulis_window, S_dpss_window, S_sinc_window
};


const char *mus_fft_window_xen_name(mus_fft_window_t i) {return(fft_window_xen_names[(int)i]);}


static XEN g_radians_to_hz(XEN val) 
{
  #define H_radians_to_hz "(" S_radians_to_hz " rads): convert radians per sample to frequency in Hz: rads * srate / (2 * pi)"
  mus_float_t x = 0.0;
  XEN_TO_C_DOUBLE_OR_ERROR(val, x, S_radians_to_hz, XEN_ONLY_ARG);
  return(C_TO_XEN_DOUBLE(mus_radians_to_hz(x)));
}


static XEN g_hz_to_radians(XEN val) 
{
  #define H_hz_to_radians "(" S_hz_to_radians " hz): convert frequency in Hz to radians per sample: hz * 2 * pi / srate"
  mus_float_t x = 0.0;
  XEN_TO_C_DOUBLE_OR_ERROR(val, x, S_hz_to_radians, XEN_ONLY_ARG);
  return(C_TO_XEN_DOUBLE(mus_hz_to_radians(x)));
}


static XEN g_radians_to_degrees(XEN val) 
{
  #define H_radians_to_degrees "(" S_radians_to_degrees " rads): convert radians to degrees: rads * 360 / (2 * pi)"
  mus_float_t x = 0.0;
  XEN_TO_C_DOUBLE_OR_ERROR(val, x, S_radians_to_degrees, XEN_ONLY_ARG);
  return(C_TO_XEN_DOUBLE(mus_radians_to_degrees(x)));
}


static XEN g_degrees_to_radians(XEN val) 
{
  #define H_degrees_to_radians "(" S_degrees_to_radians " deg): convert degrees to radians: deg * 2 * pi / 360"
  mus_float_t x = 0.0;
  XEN_TO_C_DOUBLE_OR_ERROR(val, x, S_degrees_to_radians, XEN_ONLY_ARG);
  return(C_TO_XEN_DOUBLE(mus_degrees_to_radians(x)));
}


static XEN g_db_to_linear(XEN val) 
{
  #define H_db_to_linear "(" S_db_to_linear " db): convert decibel value db to linear value: pow(10, db / 20)"
  mus_float_t x = 0.0;
  XEN_TO_C_DOUBLE_OR_ERROR(val, x, S_db_to_linear, XEN_ONLY_ARG);
  return(C_TO_XEN_DOUBLE(mus_db_to_linear(x)));
}


static XEN g_linear_to_db(XEN val) 
{
  #define H_linear_to_db "(" S_linear_to_db " lin): convert linear value to decibels: 20 * log10(lin)"
  mus_float_t x = 0.0;
  XEN_TO_C_DOUBLE_OR_ERROR(val, x, S_linear_to_db, XEN_ONLY_ARG);
  return(C_TO_XEN_DOUBLE(mus_linear_to_db(x)));
}


static XEN g_seconds_to_samples(XEN val) 
{
  #define H_seconds_to_samples "(" S_seconds_to_samples " secs): use " S_mus_srate " to convert seconds to samples"
  mus_float_t x = 0.0;
  XEN_TO_C_DOUBLE_OR_ERROR(val, x, S_seconds_to_samples, XEN_ONLY_ARG);
  return(C_TO_XEN_LONG_LONG(mus_seconds_to_samples(x)));
}


static XEN g_samples_to_seconds(XEN val) 
{
  #define H_samples_to_seconds "(" S_samples_to_seconds " samps): use " S_mus_srate " to convert samples to seconds"
  XEN_ASSERT_TYPE(XEN_LONG_LONG_P(val), val, XEN_ONLY_ARG, S_samples_to_seconds, "a number");
  return(C_TO_XEN_DOUBLE(mus_samples_to_seconds(XEN_TO_C_LONG_LONG(val))));
}


/* can't use a variable *srate* directly here because the set! side would not communicate the change to C */

static XEN g_mus_srate(void) 
{
  #define H_mus_srate "(" S_mus_srate "): current sampling rate"
  return(C_TO_XEN_DOUBLE(mus_srate()));
}


static XEN g_mus_set_srate(XEN val) 
{
  mus_float_t sr;
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ONLY_ARG, S_setB S_mus_srate, "a number");
  sr = XEN_TO_C_DOUBLE(val);
  if (sr <= 0.0) 
    XEN_OUT_OF_RANGE_ERROR(S_setB S_mus_srate, XEN_ONLY_ARG, val, "must be > 0.0");
  return(C_TO_XEN_DOUBLE(mus_set_srate(sr)));
}


static XEN g_mus_float_equal_fudge_factor(void) 
{
  #define H_mus_float_equal_fudge_factor "(" S_mus_float_equal_fudge_factor "): floating point equality fudge factor"
  return(C_TO_XEN_DOUBLE(mus_float_equal_fudge_factor()));
}


static XEN g_mus_set_float_equal_fudge_factor(XEN val) 
{
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ONLY_ARG, S_setB S_mus_float_equal_fudge_factor, "a number");
  return(C_TO_XEN_DOUBLE(mus_set_float_equal_fudge_factor(XEN_TO_C_DOUBLE(val))));
}


static XEN g_mus_array_print_length(void) 
{
  #define H_mus_array_print_length "(" S_mus_array_print_length "): current clm array print length (default is 8).  This \
affects error reporting and generator descriptions.  Array (vct) elements beyond this length are represented by '...'"
  return(C_TO_XEN_INT(mus_array_print_length()));
}


static XEN g_mus_set_array_print_length(XEN val) 
{
  int len;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(val), val, XEN_ONLY_ARG, S_setB S_mus_array_print_length, "an integer");
  len = XEN_TO_C_INT(val);
  if (len < 0)
    XEN_OUT_OF_RANGE_ERROR(S_setB S_mus_array_print_length, XEN_ONLY_ARG, val, "must be >= 0");
  return(C_TO_XEN_INT(mus_set_array_print_length(len)));
}


static XEN g_ring_modulate(XEN val1, XEN val2) 
{
  #define H_ring_modulate "(" S_ring_modulate " s1 s2): s1 * s2 (sample by sample multiply)"
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val1), val1, XEN_ARG_1, S_ring_modulate, "a number");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val2), val2, XEN_ARG_2, S_ring_modulate, "a number");
  return(C_TO_XEN_DOUBLE(mus_ring_modulate(XEN_TO_C_DOUBLE(val1), XEN_TO_C_DOUBLE(val2))));
}


static XEN g_amplitude_modulate(XEN val1, XEN val2, XEN val3) 
{
  #define H_amplitude_modulate "(" S_amplitude_modulate " carrier in1 in2): in1 * (carrier + in2)"
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val1), val1, XEN_ARG_1, S_amplitude_modulate, "a number");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val2), val2, XEN_ARG_2, S_amplitude_modulate, "a number");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val3), val3, XEN_ARG_3, S_amplitude_modulate, "a number");
  return(C_TO_XEN_DOUBLE(mus_amplitude_modulate(XEN_TO_C_DOUBLE(val1), XEN_TO_C_DOUBLE(val2), XEN_TO_C_DOUBLE(val3))));
}


static XEN g_contrast_enhancement(XEN val1, XEN val2) 
{
  mus_float_t index = 1.0; /* this is the default in clm.html and mus.lisp */
  #define H_contrast_enhancement "(" S_contrast_enhancement " sig (index 1.0)): sin(sig * pi / 2 + index * sin(sig * 2 * pi))"
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val1), val1, XEN_ARG_1, S_contrast_enhancement, "a number");
  if (XEN_BOUND_P(val2))
    {
      XEN_ASSERT_TYPE(XEN_NUMBER_P(val2), val2, XEN_ARG_2, S_contrast_enhancement, "a number");
      index = XEN_TO_C_DOUBLE(val2);
    }
  return(C_TO_XEN_DOUBLE(mus_contrast_enhancement(XEN_TO_C_DOUBLE(val1), index)));
}


static XEN g_dot_product(XEN val1, XEN val2, XEN size) 
{
  #define H_dot_product "(" S_dot_product " v1 v2 (size)): sum of (vcts) v1[i] * v2[i] (also named scalar product)"
  vct *v1, *v2;
  mus_long_t len;  

  XEN_ASSERT_TYPE(MUS_VCT_P(val1), val1, XEN_ARG_1, S_dot_product, "a vct");
  XEN_ASSERT_TYPE(MUS_VCT_P(val2), val2, XEN_ARG_2, S_dot_product, "a vct");
  XEN_ASSERT_TYPE(XEN_LONG_LONG_IF_BOUND_P(size), size, XEN_ARG_3, S_dot_product, "an integer");

  v1 = XEN_TO_VCT(val1);
  v2 = XEN_TO_VCT(val2);
  if (XEN_LONG_LONG_P(size))
    {
      len = XEN_TO_C_LONG_LONG(size);
      if (len == 0) return(C_TO_XEN_DOUBLE(0.0));
      if (len < 0)
	XEN_OUT_OF_RANGE_ERROR(S_dot_product, 3, size, "size ~A < 0?");
      if (len > v1->length) len = v1->length;
    }
  else len = v1->length; 
  if (len > v2->length) len = v2->length;

  return(C_TO_XEN_DOUBLE(mus_dot_product(v1->data, v2->data, len)));
}


#if HAVE_COMPLEX_TRIG && XEN_HAVE_COMPLEX_NUMBERS
#define S_edot_product "edot-product"

static XEN g_edot_product(XEN val1, XEN val2) 
{
  #define H_edot_product "(" S_edot_product " freq data): sum of (e^freq*i) * data[i]"
  mus_long_t i, len;
  vct *v = NULL;
  complex double freq;
  complex double *vals;
  XEN result;
  XEN_ASSERT_TYPE(XEN_COMPLEX_P(val1), val1, XEN_ARG_1, S_edot_product, "complex");
  XEN_ASSERT_TYPE((MUS_VCT_P(val2)) || (XEN_VECTOR_P(val2)), val2, XEN_ARG_2, S_edot_product, "a vct");

  freq = XEN_TO_C_COMPLEX(val1);
  if (MUS_VCT_P(val2))
    {
      v = XEN_TO_VCT(val2);
      len = v->length;
    }
  else
    {
      len = XEN_VECTOR_LENGTH(val2);
    }
  vals = (complex double *)calloc(len, sizeof(complex double));
  if (MUS_VCT_P(val2))
    {
      for (i = 0; i < len; i++)
	vals[i] = v->data[i];
    }
  else
    {
      for (i = 0; i < len; i++)
	vals[i] = XEN_TO_C_COMPLEX(XEN_VECTOR_REF(val2, i));
    }
  result = C_TO_XEN_COMPLEX(mus_edot_product(freq, vals, len));
  free(vals);
  return(result);
}
#endif


typedef enum {G_MULTIPLY_ARRAYS, G_RECTANGULAR_POLAR, G_POLAR_RECTANGULAR, G_RECTANGULAR_MAGNITUDES} xclm_window_t;

static XEN g_fft_window_1(xclm_window_t choice, XEN val1, XEN val2, XEN ulen, const char *caller) 
{
  vct *v1, *v2;
  mus_long_t len;

  XEN_ASSERT_TYPE(MUS_VCT_P(val1), val1, XEN_ARG_1, caller, "a vct");
  XEN_ASSERT_TYPE(MUS_VCT_P(val2), val2, XEN_ARG_2, caller, "a vct");
  XEN_ASSERT_TYPE(XEN_LONG_LONG_IF_BOUND_P(ulen), ulen, XEN_ARG_3, caller, "an integer");

  v1 = XEN_TO_VCT(val1);
  v2 = XEN_TO_VCT(val2);
  if (XEN_LONG_LONG_P(ulen))
    {
      len = XEN_TO_C_LONG_LONG(ulen);
      if (len == 0) return(XEN_FALSE);
      if (len < 0)
	XEN_OUT_OF_RANGE_ERROR(caller, 3, ulen, "size ~A < 0?");
      if (len > v1->length) len = v1->length;
    }
  else len = v1->length; 
  if (len > v2->length) len = v2->length;
  switch (choice)
    {
    case G_MULTIPLY_ARRAYS:        mus_multiply_arrays(v1->data, v2->data, len);           break;
    case G_RECTANGULAR_POLAR:      mus_rectangular_to_polar(v1->data, v2->data, len);      break;
    case G_RECTANGULAR_MAGNITUDES: mus_rectangular_to_magnitudes(v1->data, v2->data, len); break;
    case G_POLAR_RECTANGULAR:      mus_polar_to_rectangular(v1->data, v2->data, len);      break;
    }
  return(val1);
}


static XEN g_multiply_arrays(XEN val1, XEN val2, XEN len) 
{
  #define H_multiply_arrays "(" S_multiply_arrays " v1 v2 (len)): vct element-wise multiply: v1[i] *= v2[i]"
  return(g_fft_window_1(G_MULTIPLY_ARRAYS, val1, val2, len, S_multiply_arrays));
}


static XEN g_rectangular_to_polar(XEN val1, XEN val2) 
{
  #define H_rectangular_to_polar "(" S_rectangular_to_polar " rl im): convert real/imaginary \
data in vcts rl and im from rectangular form (fft output) to polar form (a spectrum)"

  return(g_fft_window_1(G_RECTANGULAR_POLAR, val1, val2, XEN_UNDEFINED, S_rectangular_to_polar));
}


static XEN g_rectangular_to_magnitudes(XEN val1, XEN val2) 
{
  #define H_rectangular_to_magnitudes "(" S_rectangular_to_magnitudes " rl im): convert real/imaginary \
data in vcts rl and im from rectangular form (fft output) to polar form, but ignore the phases"

  return(g_fft_window_1(G_RECTANGULAR_MAGNITUDES, val1, val2, XEN_UNDEFINED, S_rectangular_to_magnitudes));
}


static XEN g_polar_to_rectangular(XEN val1, XEN val2) 
{
  #define H_polar_to_rectangular "(" S_polar_to_rectangular " rl im): convert real/imaginary \
data in vcts rl and im from polar (spectrum) to rectangular (fft)"

  return(g_fft_window_1(G_POLAR_RECTANGULAR, val1, val2, XEN_UNDEFINED, S_polar_to_rectangular));
}


static XEN g_mus_fft(XEN url, XEN uim, XEN len, XEN usign)
{
  #define H_mus_fft "(" S_mus_fft " rl im (len) (dir 1)): return the fft of vcts rl and im which contain \
the real and imaginary parts of the data; len should be a power of 2, dir = 1 for fft, -1 for inverse-fft"

  int sign;
  mus_long_t n;
  vct *v1, *v2;

  XEN_ASSERT_TYPE((MUS_VCT_P(url)), url, XEN_ARG_1, S_mus_fft, "a vct");
  XEN_ASSERT_TYPE((MUS_VCT_P(uim)), uim, XEN_ARG_2, S_mus_fft, "a vct");

  v1 = XEN_TO_VCT(url);
  v2 = XEN_TO_VCT(uim);

  if (XEN_INTEGER_P(usign)) 
    sign = XEN_TO_C_INT(usign); 
  else sign = 1;

  if (XEN_LONG_LONG_P(len)) 
    {
      n = XEN_TO_C_LONG_LONG(len); 
      if (n <= 0)
	XEN_OUT_OF_RANGE_ERROR(S_mus_fft, 3, len, "size ~A <= 0?");
      if (n > mus_max_malloc())
	XEN_OUT_OF_RANGE_ERROR(S_mus_fft, 3, len, "size ~A too large (see mus-max-malloc)");
      if (n > v1->length)
	n = v1->length;
    }
  else n = v1->length;

  if (n > v2->length)
    n = v2->length;

  if (!(POWER_OF_2_P(n)))
    {
      mus_float_t nf;
      int np;
      nf = (log(n) / log(2.0));
      np = (int)nf;
      n = (mus_long_t)pow(2.0, np);
    }

  if (n > 0)
    mus_fft(v1->data, v2->data, n, sign);
  /*
   * in fftw, there's the extra complex array allocation, so for n = 2^29
   *   (and doubles for vcts as well as fftw), we need 24.6 Gbytes, and the FFT
   *   takes 144 secs on a 2.4 GHz machine.  (Similarly, 2^28 needs 12.6 Gb
   *   and takes 61 secs).  
   */

  return(url);
}


static XEN g_make_fft_window(XEN type, XEN size, XEN ubeta, XEN ualpha)
{
  #if HAVE_SCHEME
    #define make_window_example "(" S_make_fft_window " " S_hamming_window " 256)"
  #endif
  #if HAVE_RUBY
    #define make_window_example "make_fft_window(Hamming_window, 256)"
  #endif
  #if HAVE_FORTH
    #define make_window_example "hamming-window 256 make-fft-window"
  #endif

  #define H_make_fft_window "(" S_make_fft_window " type size (beta 0.0) (alpha 0.0)): -> fft data window (a vct). \
type is one of the sndlib fft window identifiers such as " S_kaiser_window ", beta \
is the window family parameter, if any:\n  " make_window_example

  mus_float_t beta = 0.0, alpha = 0.0;
  mus_long_t n;
  int fft_window;
  mus_float_t *data;

  XEN_ASSERT_TYPE(XEN_INTEGER_P(type), type, XEN_ARG_1, S_make_fft_window, "an integer (window type)");
  XEN_ASSERT_TYPE(XEN_LONG_LONG_P(size), size, XEN_ARG_2, S_make_fft_window, "an integer");

  if (XEN_NUMBER_P(ubeta)) beta = XEN_TO_C_DOUBLE(ubeta);
  if (XEN_NUMBER_P(ualpha)) alpha = XEN_TO_C_DOUBLE(ualpha);

  n = XEN_TO_C_LONG_LONG(size);
  if (n <= 0)
    XEN_OUT_OF_RANGE_ERROR(S_make_fft_window, 2, size, "size ~A <= 0?");
  if (n > mus_max_malloc())
    XEN_OUT_OF_RANGE_ERROR(S_make_fft_window, 2, size, "size arg ~A too large (see mus-max-malloc)");

  fft_window = XEN_TO_C_INT(type);
  if (!(mus_fft_window_p(fft_window)))
    XEN_OUT_OF_RANGE_ERROR(S_make_fft_window, 1, type, "~A: unknown fft window");

  data = (mus_float_t *)calloc(n, sizeof(mus_float_t));
  mus_make_fft_window_with_window((mus_fft_window_t)fft_window, n, beta, alpha, data);
  return(xen_make_vct(n, data));
}


static XEN g_spectrum(XEN url, XEN uim, XEN uwin, XEN utype)
{
  #define H_mus_spectrum "(" S_spectrum " rl im window (type 1)): \
real and imaginary data in vcts rl and im, returns (in rl) the spectrum thereof; \
window is the fft data window (a vct as returned by " S_make_fft_window "), \
and type determines how the spectral data is scaled:\n\
  0 = data in dB,\n\
  1 (default) = linear and normalized\n\
  2 = linear and un-normalized."

  int type;
  mus_long_t n;
  vct *v1, *v2, *v3 = NULL;

  XEN_ASSERT_TYPE((MUS_VCT_P(url)), url, XEN_ARG_1, S_spectrum, "a vct");
  XEN_ASSERT_TYPE((MUS_VCT_P(uim)), uim, XEN_ARG_2, S_spectrum, "a vct");
  if (XEN_NOT_FALSE_P(uwin)) XEN_ASSERT_TYPE((MUS_VCT_P(uwin)), uwin, XEN_ARG_3, S_spectrum, "a vct or " PROC_FALSE);

  v1 = XEN_TO_VCT(url);
  v2 = XEN_TO_VCT(uim);
  if (XEN_NOT_FALSE_P(uwin)) v3 = XEN_TO_VCT(uwin);

  n = v1->length;
  if (n > v2->length)
    n = v2->length;
  if ((v3) && (n > v3->length))
    n = v3->length;

  if (!(POWER_OF_2_P(n)))
    {
      mus_float_t nf;
      int np;
      nf = (log(n) / log(2.0));
      np = (int)nf;
      n = (int)pow(2.0, np);
    }

  if (XEN_INTEGER_P(utype)) 
    type = XEN_TO_C_INT(utype);
  else type = 1; /* linear normalized */
  if ((type < 0) || (type > 2))
    XEN_OUT_OF_RANGE_ERROR(S_spectrum, 4, utype, "type must be 0..2");
  
  if (n > 0)
    mus_spectrum(v1->data, v2->data, (v3) ? (v3->data) : NULL, n, (mus_spectrum_t)type);
  return(url);
}


static XEN g_autocorrelate(XEN reals)
{
  #define H_autocorrelate "(" S_autocorrelate " data): in place autocorrelation of data (a vct)"
  /* assumes length is power of 2 */
  vct *v1 = NULL;
  XEN_ASSERT_TYPE(MUS_VCT_P(reals), reals, XEN_ONLY_ARG, S_autocorrelate, "a vct");
  v1 = XEN_TO_VCT(reals);
  if (v1->length > 0)
    mus_autocorrelate(v1->data, v1->length);
  return(reals);
}


static XEN g_correlate(XEN data1, XEN data2)
{
  #define H_correlate "(" S_correlate " data1 data2): in place cross-correlation of data1 and data2 (both vcts)"
  mus_long_t size;
  vct *v1 = NULL, *v2 = NULL;

  XEN_ASSERT_TYPE(MUS_VCT_P(data1), data1, XEN_ARG_1, S_correlate, "a vct");
  XEN_ASSERT_TYPE(MUS_VCT_P(data2), data2, XEN_ARG_2, S_correlate, "a vct");

  v1 = XEN_TO_VCT(data1);
  v2 = XEN_TO_VCT(data2);
  if (v1->length < v2->length)
    size = v1->length;
  else size = v2->length;

  if (size > 0)
    mus_correlate(v1->data, v2->data, size);
  return(data1);
}


static XEN g_convolution(XEN url1, XEN url2, XEN un)
{
  #define H_mus_convolution "(" S_convolution " v1 v2 (len)): convolution \
of vcts v1 with v2, using fft of size len (a power of 2), result in v1"

  mus_long_t n;
  vct *v1, *v2;

  XEN_ASSERT_TYPE((MUS_VCT_P(url1)), url1, XEN_ARG_1, S_convolution, "a vct");
  XEN_ASSERT_TYPE((MUS_VCT_P(url2)), url2, XEN_ARG_2, S_convolution, "a vct");

  v1 = XEN_TO_VCT(url1);
  v2 = XEN_TO_VCT(url2);

  if (XEN_INTEGER_P(un)) 
    {
      n = XEN_TO_C_LONG_LONG(un); 
      if (n <= 0)
	XEN_OUT_OF_RANGE_ERROR(S_convolution, 3, un, "size ~A <= 0?");
      if (n > mus_max_malloc())
	XEN_OUT_OF_RANGE_ERROR(S_convolution, 3, un, "size ~A too large (see mus-max-malloc)");
      if (n > v1->length)
	n = v1->length;
    }
  else n = v1->length;
  if (n > v2->length)
    n = v2->length;
  if (!(POWER_OF_2_P(n)))
    {
      mus_float_t nf;
      int np;
      nf = (log(n) / log(2.0));
      np = (int)nf;
      n = (int)pow(2.0, np);
    }
  if (n > 0)
    mus_convolution(v1->data, v2->data, n);
  return(url1);
}


static XEN g_clear_array(XEN arr)
{
  #define H_clear_array "(" S_clear_array " v): clear vct v: v[i] = 0.0"
  vct *v;
  XEN_ASSERT_TYPE(MUS_VCT_P(arr), arr, XEN_ONLY_ARG, S_clear_array, "a vct");
  v = XEN_TO_VCT(arr);
  if (v->length > 0)
    mus_clear_array(v->data, v->length);
  return(arr);
}


static XEN g_polynomial(XEN arr, XEN x)
{
  #define H_polynomial "(" S_polynomial " coeffs x): evaluate a polynomial at x.  coeffs are in order \
of degree, so coeff[0] is the constant term."
  vct *v;
  XEN_ASSERT_TYPE(MUS_VCT_P(arr), arr, XEN_ARG_1, S_polynomial, "a vct");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(x), x, XEN_ARG_2, S_polynomial, "a number");
  v = XEN_TO_VCT(arr);
  return(C_TO_XEN_DOUBLE(mus_polynomial(v->data, XEN_TO_C_DOUBLE(x), v->length)));
}


static XEN g_array_interp(XEN obj, XEN phase, XEN size) /* opt size */
{
  #define H_array_interp "(" S_array_interp " v phase (size)): v[phase] \
taking into account wrap-around (size is size of data), with linear interpolation if phase is not an integer."

  mus_long_t len;
  vct *v;

  XEN_ASSERT_TYPE(MUS_VCT_P(obj), obj, XEN_ARG_1, S_array_interp, "a vct");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(phase), phase, XEN_ARG_2, S_array_interp, "a number");
  XEN_ASSERT_TYPE(XEN_LONG_LONG_IF_BOUND_P(size), size, XEN_ARG_3, S_array_interp, "an integer");

  v = XEN_TO_VCT(obj);
  if (XEN_BOUND_P(size)) 
    {
      len = XEN_TO_C_LONG_LONG(size); 
      if (len <= 0)
	XEN_OUT_OF_RANGE_ERROR(S_array_interp, 3, size, "size ~A <= 0?");
      if (len > v->length) 
	len = v->length;
    }
  else len = v->length;
  if (len == 0)
    return(C_TO_XEN_DOUBLE(0.0));
  return(C_TO_XEN_DOUBLE(mus_array_interp(v->data, XEN_TO_C_DOUBLE(phase), len)));
}


static XEN g_mus_interpolate(XEN type, XEN x, XEN obj, XEN size, XEN yn1)
{
  #define H_mus_interpolate "(" S_mus_interpolate " type x v (size) (yn1 0.0)): interpolate in \
data ('v' is a vct) using interpolation 'type', such as " S_mus_interp_linear "."

  mus_long_t len;
  int itype;
  vct *v;
  mus_float_t y = 0.0;

  XEN_ASSERT_TYPE(XEN_INTEGER_P(type), type, XEN_ARG_1, S_mus_interpolate, "an integer (interp type such as " S_mus_interp_all_pass ")");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(x), x, XEN_ARG_2, S_mus_interpolate, "a number");
  XEN_ASSERT_TYPE(MUS_VCT_P(obj), obj, XEN_ARG_3, S_mus_interpolate, "a vct");
  XEN_ASSERT_TYPE(XEN_LONG_LONG_IF_BOUND_P(size), size, XEN_ARG_4, S_mus_interpolate, "an integer");
  XEN_ASSERT_TYPE(XEN_NUMBER_IF_BOUND_P(yn1), yn1, XEN_ARG_5, S_mus_interpolate, "a number");

  itype = XEN_TO_C_INT(type);
  if (!(mus_interp_type_p(itype)))
    XEN_OUT_OF_RANGE_ERROR(S_mus_interpolate, 1, type, "unknown interp type ~A");

  v = XEN_TO_VCT(obj);

  if (XEN_BOUND_P(size)) 
    {
      len = XEN_TO_C_LONG_LONG(size); 
      if (len <= 0)
	XEN_OUT_OF_RANGE_ERROR(S_mus_interpolate, 4, size, "size ~A <= 0?");
      if (len > v->length) 
	len = v->length;
    }
  else len = v->length;
  if (len == 0)
    return(C_TO_XEN_DOUBLE(0.0));

  if (XEN_NUMBER_P(yn1))
    y = XEN_TO_C_DOUBLE(yn1);

  return(C_TO_XEN_DOUBLE(mus_interpolate((mus_interp_t)itype, XEN_TO_C_DOUBLE(x), v->data, len, y)));
}



/* ---------------- mus-xen struct ---------------- */

static XEN_OBJECT_TYPE mus_xen_tag;

#define MUS_XEN_P(obj) (XEN_OBJECT_TYPE_P(obj, mus_xen_tag))

bool mus_xen_p(XEN obj) {return(MUS_XEN_P(obj));}


static XEN g_mus_generator_p(XEN obj) 
{
  #define H_mus_generator_p "(" S_mus_generator_p " obj): " PROC_TRUE " if 'obj' is a CLM generator."

  if (MUS_XEN_P(obj)) return(XEN_TRUE);

#if HAVE_SCHEME || HAVE_FORTH
  /* defgenerator defines "mus-name" -- we need an actual type here! */
  if ((XEN_LIST_P(obj)) &&
      (XEN_LIST_LENGTH(obj) > 1) &&
      (XEN_SYMBOL_P(XEN_CAR(obj))))
    {
      XEN assoc_list;
      assoc_list = XEN_LIST_REF(obj, XEN_LIST_LENGTH(obj) - 1);

      if ((XEN_LIST_P(assoc_list)) &&               /* avoid type error from assoc */
	  (XEN_LIST_P(XEN_CAR(assoc_list))) &&
	  (XEN_LIST_P(XEN_ASSOC(C_STRING_TO_XEN_SYMBOL("mus-name"), assoc_list))))
	return(XEN_TRUE);
    }
#endif

  return(XEN_FALSE);
}

static XEN *make_vcts(int size)
{
  int i;
  XEN *vcts;
  vcts = (XEN *)malloc(size * sizeof(XEN));
  for (i = 0; i < size; i++)
    vcts[i] = XEN_UNDEFINED;
  return(vcts);
}


enum {MUS_DATA_WRAPPER, MUS_INPUT_FUNCTION, MUS_ANALYZE_FUNCTION, MUS_EDIT_FUNCTION, MUS_SYNTHESIZE_FUNCTION, MUS_SELF_WRAPPER, MUS_MAX_VCTS};

#if HAVE_SCHEME
static XEN_MARK_OBJECT_TYPE mark_mus_xen(void *obj) 
#else
static XEN_MARK_OBJECT_TYPE mark_mus_xen(XEN obj) 
#endif
{
  mus_xen *ms;
#if HAVE_RUBY || HAVE_SCHEME
  /* rb_gc_mark and scheme_mark_object pass us the actual value, not the XEN wrapper */
  ms = (mus_xen *)obj;
#endif
#if HAVE_FORTH
  ms = XEN_TO_MUS_XEN(obj);
#endif
  if (ms->vcts) 
    {
      int i;
      for (i = 0; i < ms->nvcts; i++) 
	if ((i != MUS_SELF_WRAPPER) && 
	    (XEN_BOUND_P(ms->vcts[i])))
      xen_gc_mark(ms->vcts[i]);
    }
#if HAVE_RUBY
  return(NULL);
#endif
}


static void mus_xen_free(mus_xen *ms)
{
  if (!(ms->dont_free_gen)) mus_free(ms->gen);
  ms->gen = NULL;
  if (ms->vcts) free(ms->vcts);
  ms->vcts = NULL;
  free(ms);
}

XEN_MAKE_OBJECT_FREE_PROCEDURE(mus_xen, free_mus_xen, mus_xen_free)


#if HAVE_SCHEME
static char *print_mus_xen(s7_scheme *sc, void *obj)
{
  return(mus_describe(((mus_xen *)obj)->gen));
}

static bool s7_equalp_mus_xen(void *val1, void *val2)
{
  return(mus_equalp(((mus_xen *)val1)->gen, ((mus_xen *)val2)->gen));
}
#endif


#if HAVE_RUBY
static XEN mus_xen_to_s(XEN obj)
{
  char *str;
  XEN result;
  str = mus_describe(XEN_TO_MUS_ANY(obj));
  result = C_TO_XEN_STRING(str);
  if (str) free(str);
  return(result);
}
#endif


#if HAVE_FORTH
static XEN print_mus_xen(XEN obj)
{
  char *str;
  XEN result;
  str = mus_describe(XEN_TO_MUS_ANY(obj));
  result = fth_make_string_format("#<%s>", str);
  if (str) free(str);
  return(result);
}
#endif


#if (!HAVE_SCHEME)
static XEN equalp_mus_xen(XEN obj1, XEN obj2) 
{
  if ((!(MUS_XEN_P(obj1))) || (!(MUS_XEN_P(obj2)))) return(XEN_FALSE);
  return(C_TO_XEN_BOOLEAN(mus_equalp(XEN_TO_MUS_ANY(obj1), XEN_TO_MUS_ANY(obj2))));
}
#endif


#if HAVE_RUBY || HAVE_FORTH
static XEN mus_xen_apply(XEN gen, XEN arg1, XEN arg2)
{
#if HAVE_FORTH
  XEN_ASSERT_TYPE(MUS_XEN_P(gen), gen, XEN_ARG_1, S_mus_apply, "a generator");
#endif
  return(C_TO_XEN_DOUBLE(mus_run(XEN_TO_MUS_ANY(gen),
				 (XEN_NUMBER_P(arg1)) ? XEN_TO_C_DOUBLE(arg1) : 0.0,
				 (XEN_NUMBER_P(arg2)) ? XEN_TO_C_DOUBLE(arg2) : 0.0)));
}
#endif

#if HAVE_SCHEME
#if 0
static s7_pointer g_frame_methods, g_mixer_methods;
#endif

static XEN g_frame_set(XEN uf1, XEN uchan, XEN val);
static XEN g_mixer_set(XEN uf1, XEN in, XEN out, XEN val);

static XEN mus_xen_apply(s7_scheme *sc, XEN gen, XEN args)
{
  mus_float_t arg1, arg2;
  if (s7_is_pair(args))
    {
      arg1 = s7_number_to_real(s7_car(args));
      args = s7_cdr(args);
      if (s7_is_pair(args))
	arg2 = s7_number_to_real(s7_car(args));
      else arg2 = 0.0;
      return(s7_make_real(s7, mus_run(XEN_TO_MUS_ANY(gen), arg1, arg2)));
    }
  return(s7_make_real(s7, mus_run(XEN_TO_MUS_ANY(gen), 0.0, 0.0)));
}

static XEN s7_mus_set(s7_scheme *sc, XEN obj, XEN args)
{
  mus_any *g = NULL;
  g = XEN_TO_MUS_ANY(obj);

  if (mus_frame_p(g))
    return(g_frame_set(obj, XEN_CAR(args), XEN_CADR(args)));
  if (mus_mixer_p(g))
    return(g_mixer_set(obj, XEN_CAR(args), XEN_CADR(args), XEN_CADDR(args)));
  XEN_ASSERT_TYPE(false, obj, XEN_ARG_1, "generalized set!", "a frame or mixer");
  return(XEN_FALSE);
}

static XEN s7_mus_length(s7_scheme *sc, XEN obj)
{
  return(g_mus_length(obj));
}

static XEN s7_mus_copy(s7_scheme *sc, XEN obj)
{
  /* mus_copy in clm.c first */

  mus_any *g = NULL;
  g = XEN_TO_MUS_ANY(obj);

  if (mus_frame_p(g))
    return(mus_xen_to_object(mus_any_to_mus_xen(mus_frame_copy(g))));
  if (mus_mixer_p(g))
    return(mus_xen_to_object(mus_any_to_mus_xen(mus_mixer_copy(g))));

  return(XEN_FALSE);
}

static XEN s7_mus_fill(s7_scheme *sc, XEN obj, XEN val)
{
  /* frame mixer, perhaps anything with an array? mus_fill method? */

  mus_any *g = NULL;
  g = XEN_TO_MUS_ANY(obj);

  if (mus_frame_p(g))
    return(C_TO_XEN_DOUBLE(mus_frame_fill(g, XEN_TO_C_DOUBLE(val))));
  if (mus_mixer_p(g))
    return(C_TO_XEN_DOUBLE(mus_mixer_fill(g, XEN_TO_C_DOUBLE(val))));
  
  return(XEN_FALSE);
}
#endif


XEN mus_xen_to_object(mus_xen *gn) /* global for user-defined gens */
{
  XEN_MAKE_AND_RETURN_OBJECT(mus_xen_tag, gn, mark_mus_xen, free_mus_xen);
}


XEN mus_xen_to_object_with_vct(mus_xen *gn, XEN v) /* global for user-defined gens (not used anymore in this file) */
{
#if HAVE_SCHEME
  if (!mus_vct_p(v)) fprintf(stderr, "vct arg clobbered");
#endif
  gn->vcts[MUS_DATA_WRAPPER] = v;
  XEN_MAKE_AND_RETURN_OBJECT(mus_xen_tag, gn, mark_mus_xen, free_mus_xen);
}


mus_any *mus_optkey_to_mus_any(XEN key, const char *caller, int n, mus_any *def)
{
  /* from Michael Scholz's sndins.c */
  if (!(XEN_KEYWORD_P(key))) 
    {
      XEN_ASSERT_TYPE(MUS_XEN_P(key), key, n, caller, "a clm generator or keyword");
      return(XEN_TO_MUS_ANY(key));
    }
  return(def);
}


static XEN mus_optkey_to_input_procedure(XEN key, const char *caller, int n, XEN def, int required_args, const char *err)
{
  if ((!(XEN_KEYWORD_P(key))) && 
      (!(XEN_FALSE_P(key))))
    {
      XEN_ASSERT_TYPE(XEN_PROCEDURE_P(key) || MUS_XEN_P(key), key, n, caller, "a procedure or input generator");
      
      if ((XEN_PROCEDURE_P(key)) &&
	  (!(local_arity_ok(key, required_args))))
	XEN_BAD_ARITY_ERROR(caller, n, key, err);

      if (MUS_VCT_P(key))
	XEN_WRONG_TYPE_ARG_ERROR(caller, n, key, "an input procedure");

      if ((MUS_XEN_P(key)) &&
	  (!(mus_input_p(XEN_TO_MUS_ANY(key)))))
	XEN_WRONG_TYPE_ARG_ERROR(caller, n, key, "an input generator");
      return(key);
    }
  return(def);
}



/* ---------------- wrappers ---------------- */

mus_xen *mus_any_to_mus_xen(mus_any *ge)
{
  mus_xen *gn;
  gn = (mus_xen *)calloc(1, sizeof(mus_xen));
  gn->gen = ge;
  gn->nvcts = 0;
  gn->vcts = NULL;
  return(gn);
}


static mus_xen *mus_any_to_mus_xen_with_vct(mus_any *ge, XEN v)
{
  mus_xen *gn;
  gn = (mus_xen *)calloc(1, sizeof(mus_xen));
  gn->gen = ge;
  gn->nvcts = 1;
  gn->vcts = make_vcts(gn->nvcts);
  gn->vcts[MUS_DATA_WRAPPER] = v;
  return(gn);
}


static mus_xen *mus_any_to_mus_xen_with_two_vcts(mus_any *ge, XEN v1, XEN v2)
{
  mus_xen *gn;
  gn = (mus_xen *)calloc(1, sizeof(mus_xen));
  gn->gen = ge;
  gn->nvcts = 2;
  gn->vcts = make_vcts(gn->nvcts);
  gn->vcts[MUS_DATA_WRAPPER] = v1;
  gn->vcts[MUS_INPUT_FUNCTION] = v2;
  return(gn);
}



/* ---------------- generic functions ---------------- */

/* these are for user-defined (list-based defgenerator-style) generators 
 *   the methods are in an association list (name func) or (name getter setter)
 */

static XEN call_get_method(XEN gen, const char *method_name)
{
#if HAVE_SCHEME || HAVE_FORTH
  XEN pair;
  pair = XEN_ASSOC(C_STRING_TO_XEN_SYMBOL(method_name), 
		   XEN_LIST_REF(gen, 
				XEN_LIST_LENGTH(gen) - 1));
  if (XEN_LIST_P(pair))
    return(XEN_CALL_1(XEN_CADR(pair),  /* this is the getter proc if procedure-with-setter */
		      gen,
		      method_name));
  XEN_ERROR(XEN_ERROR_TYPE("no-such-method"), 
	    XEN_LIST_3(C_TO_XEN_STRING("no-such-method: ~A for ~A"),
		       C_TO_XEN_STRING(method_name), 
		       gen));
#endif
  return(XEN_FALSE);
}


static XEN call_get_method_2(XEN gen, XEN arg, const char *method_name)
{
#if HAVE_SCHEME || HAVE_FORTH
  XEN pair;
  pair = XEN_ASSOC(C_STRING_TO_XEN_SYMBOL(method_name), 
		   XEN_LIST_REF(gen, 
				XEN_LIST_LENGTH(gen) - 1));
  if (XEN_LIST_P(pair))
    return(XEN_CALL_2(XEN_CADR(pair), 
		      gen, arg,
		      method_name));
  XEN_ERROR(XEN_ERROR_TYPE("no-such-method"), 
	    XEN_LIST_3(C_TO_XEN_STRING("no-such-method: ~A for ~A"),
		       C_TO_XEN_STRING(method_name), 
		       gen));
#endif
  return(XEN_FALSE);
}


static XEN call_get_method_3(XEN gen, XEN arg1, XEN arg2, const char *method_name)
{
#if HAVE_SCHEME || HAVE_FORTH
  XEN pair;
  pair = XEN_ASSOC(C_STRING_TO_XEN_SYMBOL(method_name), 
		   XEN_LIST_REF(gen, 
				XEN_LIST_LENGTH(gen) - 1));
  if (XEN_LIST_P(pair))
    return(XEN_CALL_3(XEN_CADR(pair), 
		      gen, arg1, arg2,
		      method_name));
  XEN_ERROR(XEN_ERROR_TYPE("no-such-method"), 
	    XEN_LIST_3(C_TO_XEN_STRING("no-such-method: ~A for ~A"),
		       C_TO_XEN_STRING(method_name), 
		       gen));
#endif
  return(XEN_FALSE);
}


static XEN call_set_method(XEN gen, XEN value, const char *method_name)
{
#if HAVE_SCHEME || HAVE_FORTH
  XEN pair;
  pair = XEN_ASSOC(C_STRING_TO_XEN_SYMBOL(method_name), 
		   XEN_LIST_REF(gen, 
				XEN_LIST_LENGTH(gen) - 1));
  if (XEN_LIST_P(pair))
    {
#if HAVE_SCHEME
      if (s7_is_procedure_with_setter(XEN_CADR(pair)))
	return(XEN_CALL_2(s7_procedure_setter(s7, XEN_CADR(pair)),
			  gen, value,
			  method_name));	  
#endif
      if (XEN_LIST_LENGTH(pair) == 3)
	return(XEN_CALL_2(XEN_CADDR(pair),
			  gen, value,
			  method_name));
    }
  XEN_ERROR(XEN_ERROR_TYPE("no-such-method"), 
	    XEN_LIST_3(C_TO_XEN_STRING("no-such-method: ~A for ~A"),
		       C_TO_XEN_STRING(method_name), 
		       gen));
#endif
  return(XEN_FALSE);
}


static XEN call_set_method_2(XEN gen, XEN arg, XEN value, const char *method_name)
{
#if HAVE_SCHEME || HAVE_FORTH
  XEN pair;
  pair = XEN_ASSOC(C_STRING_TO_XEN_SYMBOL(method_name), 
		   XEN_LIST_REF(gen, 
				XEN_LIST_LENGTH(gen) - 1));
  if (XEN_LIST_P(pair))
    {
#if HAVE_SCHEME
      if (s7_is_procedure_with_setter(XEN_CADR(pair)))
	return(XEN_CALL_3(s7_procedure_setter(s7, XEN_CADR(pair)),
			  gen, arg, value,
			  method_name));	  
#endif
      if (XEN_LIST_LENGTH(pair) == 3)
	return(XEN_CALL_3(XEN_CADDR(pair),
			  gen, arg, value,
			  method_name));
    }
  XEN_ERROR(XEN_ERROR_TYPE("no-such-method"), 
	    XEN_LIST_3(C_TO_XEN_STRING("no-such-method: ~A for ~A"),
		       C_TO_XEN_STRING(method_name), 
		       gen));
#endif
  return(XEN_FALSE);
}


static XEN g_mus_describe(XEN gen) 
{
  #define H_mus_describe "(" S_mus_describe " gen): return a string describing the state of CLM generator generator"
  char *str;
  XEN result;
  mus_any *g = NULL;

  if (XEN_LIST_P(gen)) return(call_get_method(gen, S_mus_describe));
  XEN_TO_C_ANY_GENERATOR(gen, g, S_mus_describe, "a generator");
  str = mus_describe(g);
  result = C_TO_XEN_STRING(str);
  if (str) free(str);
  return(result);
}


static XEN g_mus_reset(XEN gen) 
{
  #define H_mus_reset "(" S_mus_reset " gen): clear out gen, setting it to its default starting state"
  mus_any *g = NULL;

  if (XEN_LIST_P(gen)) return(call_get_method(gen, S_mus_reset));
  XEN_TO_C_ANY_GENERATOR(gen, g, S_mus_reset, "a generator");
  mus_reset(g);
  return(gen);
}


static XEN g_mus_phase(XEN gen) 
{
  #define H_mus_phase "(" S_mus_phase " gen): gen's current phase (radians)"
  mus_any *g = NULL;

  if (XEN_LIST_P(gen)) return(call_get_method(gen, S_mus_phase));
  XEN_TO_C_ANY_GENERATOR(gen, g, S_mus_phase, "a generator");
  return(C_TO_XEN_DOUBLE(mus_phase(g)));
}


static XEN g_mus_set_phase(XEN gen, XEN val) 
{
  mus_any *g = NULL;
  mus_float_t x = 0.0;

  if (XEN_LIST_P(gen)) return(call_set_method(gen, val, S_mus_phase));
  XEN_TO_C_ANY_GENERATOR(gen, g, S_setB S_mus_phase, "a generator");
  XEN_TO_C_DOUBLE_OR_ERROR(val, x, S_setB S_mus_phase, XEN_ARG_2);

  return(C_TO_XEN_DOUBLE(mus_set_phase(g, x)));
}


static XEN g_mus_scaler(XEN gen) 
{
  #define H_mus_scaler "(" S_mus_scaler " gen): gen's scaler, if any.  This is often an amplitude adjustment of some sort."
  mus_any *g = NULL;

  if (XEN_LIST_P(gen)) return(call_get_method(gen, S_mus_scaler));
  XEN_TO_C_ANY_GENERATOR(gen, g, S_mus_scaler, "a generator");

  return(C_TO_XEN_DOUBLE(mus_scaler(g)));
}


static XEN g_mus_set_scaler(XEN gen, XEN val) 
{
  mus_any *g = NULL;
  mus_float_t x = 0.0;

  if (XEN_LIST_P(gen)) return(call_set_method(gen, val, S_mus_scaler));
  XEN_TO_C_ANY_GENERATOR(gen, g, S_setB S_mus_scaler, "a generator");
  XEN_TO_C_DOUBLE_OR_ERROR(val, x, S_setB S_mus_scaler, XEN_ARG_2);

  return(C_TO_XEN_DOUBLE(mus_set_scaler(g, x)));
}


static XEN g_mus_safety(XEN gen) 
{
  #define H_mus_safety "(" S_mus_safety " gen): gen's safety setting, if any."
  mus_any *g = NULL;

  if (XEN_LIST_P(gen)) return(call_get_method(gen, S_mus_safety));
  XEN_TO_C_ANY_GENERATOR(gen, g, S_mus_safety, "a generator");
  return(C_TO_XEN_INT(mus_safety(g)));
}


static XEN g_mus_set_safety(XEN gen, XEN val) 
{
  mus_any *g = NULL;
  int n = 0;

  if (XEN_LIST_P(gen)) return(call_set_method(gen, val, S_mus_safety));
  XEN_TO_C_ANY_GENERATOR(gen, g, S_setB S_mus_safety, "a generator");
  XEN_TO_C_INTEGER_OR_ERROR(val, n, S_setB S_mus_safety, XEN_ARG_2);

  return(C_TO_XEN_INT(mus_set_safety(g, n)));
}


static XEN g_mus_feedforward(XEN gen) 
{
  #define H_mus_feedforward "(" S_mus_feedforward " gen): gen's feedforward field"
  mus_any *g = NULL;

  if (XEN_LIST_P(gen)) return(call_get_method(gen, S_mus_feedforward));
  XEN_TO_C_ANY_GENERATOR(gen, g, S_mus_feedforward, "a generator");

  return(C_TO_XEN_DOUBLE(mus_feedforward(g)));
}


static XEN g_mus_set_feedforward(XEN gen, XEN val) 
{
  mus_any *g = NULL;
  mus_float_t x = 0.0;

  if (XEN_LIST_P(gen)) return(call_set_method(gen, val, S_mus_feedforward));
  XEN_TO_C_ANY_GENERATOR(gen, g, S_setB S_mus_feedforward, "a generator");
  XEN_TO_C_DOUBLE_OR_ERROR(val, x, S_setB S_mus_feedforward, XEN_ARG_2);

  return(C_TO_XEN_DOUBLE(mus_set_feedforward(g, x)));
}


static XEN g_mus_width(XEN gen) 
{
  #define H_mus_width "(" S_mus_width " gen): gen's width, if any.  This is usually a table size."
  mus_any *g = NULL;

  if (XEN_LIST_P(gen)) return(call_get_method(gen, S_mus_width));
  XEN_TO_C_ANY_GENERATOR(gen, g, S_mus_width, "a generator");

  return(C_TO_XEN_DOUBLE(mus_width(g)));
}


static XEN g_mus_set_width(XEN gen, XEN val) 
{
  mus_any *g = NULL;
  mus_float_t x = 0.0;

  if (XEN_LIST_P(gen)) return(call_set_method(gen, val, S_mus_width));
  XEN_TO_C_ANY_GENERATOR(gen, g, S_setB S_mus_width, "a generator");
  XEN_TO_C_DOUBLE_OR_ERROR(val, x, S_setB S_mus_width, XEN_ARG_2);

  return(C_TO_XEN_DOUBLE(mus_set_width(g, x)));
}


static XEN g_mus_offset(XEN gen) 
{
  #define H_mus_offset "(" S_mus_offset " gen): gen's offset, if any"
  mus_any *g = NULL;

  if (XEN_LIST_P(gen)) return(call_get_method(gen, S_mus_offset));
  XEN_TO_C_ANY_GENERATOR(gen, g, S_mus_offset, "a generator");

  return(C_TO_XEN_DOUBLE(mus_offset(g)));
}


static XEN g_mus_set_offset(XEN gen, XEN val) 
{
  mus_any *g = NULL;
  mus_float_t x = 0.0;

  if (XEN_LIST_P(gen)) return(call_set_method(gen, val, S_mus_offset));
  XEN_TO_C_ANY_GENERATOR(gen, g, S_setB S_mus_offset, "a generator");
  XEN_TO_C_DOUBLE_OR_ERROR(val, x, S_setB S_mus_offset, XEN_ARG_2);

  return(C_TO_XEN_DOUBLE(mus_set_offset(g, x)));
}


static XEN g_mus_frequency(XEN gen) 
{
  #define H_mus_frequency "(" S_mus_frequency " gen): gen's frequency (Hz)"
  mus_any *g = NULL;

  if (XEN_LIST_P(gen)) return(call_get_method(gen, S_mus_frequency));
  XEN_TO_C_ANY_GENERATOR(gen, g, S_mus_frequency, "a generator");

  return(C_TO_XEN_DOUBLE(mus_frequency(g)));
}


static XEN g_mus_set_frequency(XEN gen, XEN val) 
{
  mus_any *g = NULL;
  mus_float_t x = 0.0;

  if (XEN_LIST_P(gen)) return(call_set_method(gen, val, S_mus_frequency));
  XEN_TO_C_ANY_GENERATOR(gen, g, S_setB S_mus_frequency, "a generator");
  XEN_TO_C_DOUBLE_OR_ERROR(val, x, S_setB S_mus_frequency, XEN_ARG_2);

  return(C_TO_XEN_DOUBLE(mus_set_frequency(g, x)));
}


static XEN g_mus_run(XEN gen, XEN arg1, XEN arg2) 
{
  #define H_mus_run "(" S_mus_run " gen (arg1 0.0) (arg2 0.0)): apply gen to arg1 and arg2"
  mus_any *g = NULL;
  mus_float_t a1 = 0.0, a2 = 0.0;

  if (XEN_LIST_P(gen)) return(call_get_method_3(gen, arg1, arg2, S_mus_run));
  XEN_TO_C_ANY_GENERATOR(gen, g, S_mus_run, "a generator");
  XEN_TO_C_DOUBLE_IF_BOUND(arg1, a1, S_mus_run, XEN_ARG_2);
  XEN_TO_C_DOUBLE_IF_BOUND(arg2, a2, S_mus_run, XEN_ARG_3);

  return(C_TO_XEN_DOUBLE(mus_run(g, a1, a2)));
}


XEN g_mus_length(XEN gen)
{
  #define H_mus_length "(" S_mus_length " gen): gen's length, if any"

  if (XEN_LIST_P(gen)) 
    return(call_get_method(gen, S_mus_length));

  if (MUS_XEN_P(gen))
    return(C_TO_XEN_LONG_LONG(mus_length(XEN_TO_MUS_ANY(gen))));

  if (MUS_VCT_P(gen))
    return(C_TO_XEN_INT((XEN_TO_VCT(gen))->length));

  if (sound_data_p(gen))
    return(C_TO_XEN_INT((XEN_TO_SOUND_DATA(gen))->length));

  XEN_ASSERT_TYPE(false, gen, XEN_ONLY_ARG, S_mus_length, "a generator, vct, or sound-data object");
  return(XEN_FALSE); /* make compiler happy */
}


static XEN g_mus_set_length(XEN gen, XEN val) 
{
  mus_long_t len = 0;
  mus_any *ptr = NULL;
  mus_xen *ms;

  if (XEN_LIST_P(gen)) return(call_set_method(gen, val, S_mus_length));
  XEN_TO_C_ANY_GENERATOR(gen, ptr, S_setB S_mus_length, "a generator");
  XEN_TO_C_INTEGER_OR_ERROR(val, len, S_setB S_mus_length, XEN_ARG_2);

  if (len <= 0)
    XEN_OUT_OF_RANGE_ERROR(S_setB S_mus_length, XEN_ONLY_ARG, val, "must be > 0");

  if ((ptr) && (!mus_env_p(ptr)) && (!mus_src_p(ptr))) /* set length doesn't refer to data vct here */
    {
      ms = XEN_TO_MUS_XEN(gen);
      if ((ms->vcts) && (!(XEN_EQ_P(ms->vcts[MUS_DATA_WRAPPER], XEN_UNDEFINED))))
	{
	  vct *v;
	  v = XEN_TO_VCT(ms->vcts[MUS_DATA_WRAPPER]);
	  if ((v) && (len > v->length))
	    XEN_OUT_OF_RANGE_ERROR(S_setB S_mus_length, XEN_ONLY_ARG, val, "must be <= current data size");
	  /* set_offset refers only to env, set_width only to square_wave et al, set_location only readin */
	  /* filters are protected by keeping allocated_size and not allowing arrays to be set */
	}
    }
  return(C_TO_XEN_LONG_LONG(mus_set_length(ptr, len)));
}


static XEN g_mus_order(XEN gen) 
{
  #define H_mus_order "(" S_mus_order " gen): gen's order, if any"
  mus_any *g = NULL;

  if (XEN_LIST_P(gen)) return(call_get_method(gen, S_mus_order));
  XEN_TO_C_ANY_GENERATOR(gen, g, S_mus_order, "a generator");

  return(C_TO_XEN_LONG_LONG(mus_order(g)));
}


static XEN g_mus_type(XEN gen) 
{
  #define H_mus_type "(" S_mus_type " gen): gen's type"
  mus_any *g = NULL;

  if (XEN_LIST_P(gen)) return(call_get_method(gen, S_mus_type));
  XEN_TO_C_ANY_GENERATOR(gen, g, S_mus_type, "a generator");

  return(C_TO_XEN_INT(mus_type(g)));
}


static XEN g_mus_name(XEN gen) 
{
  #define H_mus_name "(" S_mus_name " gen): gen's (type) name, if any"
  mus_any *g = NULL;

  if (XEN_LIST_P(gen)) return(call_get_method(gen, S_mus_name));
  XEN_TO_C_ANY_GENERATOR(gen, g, S_mus_name, "a generator");

  return(C_TO_XEN_STRING(mus_name(g)));
}


static XEN g_mus_set_name(XEN gen, XEN name) 
{
  mus_any *g = NULL;

  if (XEN_LIST_P(gen)) return(call_set_method(gen, name, S_mus_name));
  XEN_TO_C_ANY_GENERATOR(gen, g, S_setB S_mus_name, "a generator");
  XEN_ASSERT_TYPE(XEN_STRING_P(name), name, XEN_ARG_2, S_setB S_mus_name, "a string");

  return(C_TO_XEN_STRING(mus_set_name(g, (const char *)(XEN_TO_C_STRING(name)))));
}


XEN g_mus_data(XEN gen) 
{
  #define H_mus_data "(" S_mus_data " gen): gen's internal data (a vct), if any"
  mus_xen *ms;

  if (XEN_LIST_P(gen)) return(call_get_method(gen, S_mus_data));
  XEN_ASSERT_TYPE(MUS_XEN_P(gen), gen, XEN_ONLY_ARG, S_mus_data, "a generator");

  ms = XEN_TO_MUS_XEN(gen);
  if (ms->vcts)
    return(ms->vcts[MUS_DATA_WRAPPER]); 
  else return(XEN_FALSE);
}


static XEN g_mus_set_data(XEN gen, XEN val) 
{
  mus_xen *ms;

  if (XEN_LIST_P(gen)) return(call_set_method(gen, val, S_mus_data));
  XEN_ASSERT_TYPE(MUS_XEN_P(gen), gen, XEN_ONLY_ARG, S_mus_data, "a generator");
  XEN_ASSERT_TYPE((MUS_VCT_P(val)), val, XEN_ARG_2, S_setB S_mus_data, "a vct");

  ms = XEN_TO_MUS_XEN(gen);
  if (ms->vcts)
    {
      vct *v;
      mus_any *ma;
      v = XEN_TO_VCT(val);
      ma = ms->gen;
      mus_set_data(ma, v->data);  /* TO REMEMBER: if allocated, should have freed, and set to not allocated */
      ms->vcts[MUS_DATA_WRAPPER] = val;
      return(val);
    }
  return(XEN_FALSE);
}


enum {G_FILTER_STATE, G_FILTER_XCOEFFS, G_FILTER_YCOEFFS};
/* G_FILTER_STATE must = MUS_DATA_WRAPPER = 0 */


static XEN g_mus_xcoeffs(XEN gen) 
{
  #define H_mus_xcoeffs "(" S_mus_xcoeffs " gen): gen's filter xcoeffs (vct of coefficients on inputs)"
  mus_xen *ms;

  if (XEN_LIST_P(gen)) return(call_get_method(gen, S_mus_xcoeffs));
  XEN_ASSERT_TYPE(MUS_XEN_P(gen), gen, XEN_ONLY_ARG, S_mus_xcoeffs, "a generator");

  ms = XEN_TO_MUS_XEN(gen);
  if ((ms->vcts) && (ms->nvcts > G_FILTER_XCOEFFS))
    return(ms->vcts[G_FILTER_XCOEFFS]); 
  /* no wrapper -- locsig/ssb-am, all smpflts have xcoeffs, latter have ycoeffs, but how to get array size? */
  return(XEN_FALSE);
}


static XEN g_mus_ycoeffs(XEN gen) 
{
  #define H_mus_ycoeffs "(" S_mus_ycoeffs " gen): gen's filter ycoeffs (vct of coefficients on outputs)"
  mus_xen *ms;

  if (XEN_LIST_P(gen)) return(call_get_method(gen, S_mus_ycoeffs));
  XEN_ASSERT_TYPE(MUS_XEN_P(gen), gen, XEN_ONLY_ARG, S_mus_ycoeffs, "a generator");

  ms = XEN_TO_MUS_XEN(gen);
  if ((ms->vcts) && (ms->nvcts > G_FILTER_YCOEFFS))
    return(ms->vcts[G_FILTER_YCOEFFS]); 
  return(XEN_FALSE);
}


static XEN g_mus_xcoeff(XEN gen, XEN index)
{
  #define H_mus_xcoeff "(" S_mus_xcoeff " gen index): gen's filter xcoeff value at index (0-based)"
  int ind = 0;
  mus_any *g = NULL;

  if (XEN_LIST_P(gen)) return(call_get_method_2(gen, index, S_mus_xcoeff));
  XEN_TO_C_ANY_GENERATOR(gen, g, S_mus_xcoeff, "a generator");
  XEN_TO_C_INTEGER_OR_ERROR(index, ind, S_mus_xcoeff, XEN_ARG_2);

  if (ind < 0)
    XEN_OUT_OF_RANGE_ERROR(S_mus_xcoeff, XEN_ARG_2, index, "index must be non-negative");
  return(C_TO_XEN_DOUBLE(mus_xcoeff(g, ind)));
}


static XEN g_mus_set_xcoeff(XEN gen, XEN index, XEN val)
{
  int ind = 0;
  mus_any *g = NULL;
  mus_float_t x = 0.0;

  if (XEN_LIST_P(gen)) return(call_set_method_2(gen, index, val, S_setB S_mus_xcoeff));
  XEN_TO_C_ANY_GENERATOR(gen, g, S_setB S_mus_xcoeff, "a generator");
  XEN_TO_C_INTEGER_OR_ERROR(index, ind, S_setB S_mus_xcoeff, XEN_ARG_2);
  XEN_TO_C_DOUBLE_OR_ERROR(val, x, S_setB S_mus_xcoeff, XEN_ARG_3);

  if (ind < 0)
    XEN_OUT_OF_RANGE_ERROR(S_setB S_mus_xcoeff, XEN_ARG_2, index, "index must be non-negative");
  return(C_TO_XEN_DOUBLE(mus_set_xcoeff(g, ind, x)));
}


static XEN g_mus_ycoeff(XEN gen, XEN index)
{
  #define H_mus_ycoeff "(" S_mus_ycoeff " gen index): gen's filter ycoeff value at index (0-based)"
  int ind = 0;
  mus_any *g = NULL;

  if (XEN_LIST_P(gen)) return(call_get_method_2(gen, index, S_mus_ycoeff));
  XEN_TO_C_ANY_GENERATOR(gen, g, S_mus_ycoeff, "a generator");
  XEN_TO_C_INTEGER_OR_ERROR(index, ind, S_mus_ycoeff, XEN_ARG_2);

  if (ind < 0)
    XEN_OUT_OF_RANGE_ERROR(S_mus_ycoeff, XEN_ARG_2, index, "index must be non-negative");
  return(C_TO_XEN_DOUBLE(mus_ycoeff(g, ind)));
}


static XEN g_mus_set_ycoeff(XEN gen, XEN index, XEN val)
{
  int ind = 0;
  mus_any *g = NULL;
  mus_float_t x = 0.0;

  if (XEN_LIST_P(gen)) return(call_set_method_2(gen, index, val, S_setB S_mus_ycoeff));
  XEN_TO_C_ANY_GENERATOR(gen, g, S_setB S_mus_ycoeff, "a generator");
  XEN_TO_C_INTEGER_OR_ERROR(index, ind, S_setB S_mus_ycoeff, XEN_ARG_2);
  XEN_TO_C_DOUBLE_OR_ERROR(val, x, S_setB S_mus_ycoeff, XEN_ARG_3);

  if (ind < 0)
    XEN_OUT_OF_RANGE_ERROR(S_setB S_mus_ycoeff, XEN_ARG_2, index, "index must be non-negative");
  return(C_TO_XEN_DOUBLE(mus_set_ycoeff(g, ind, x)));
}


XEN g_mus_file_name(XEN gen) 
{
  #define H_mus_file_name "(" S_mus_file_name " gen): file associated with gen, if any"
  mus_any *g = NULL;

  if (XEN_LIST_P(gen)) return(call_get_method(gen, S_mus_file_name));

  XEN_TO_C_ANY_GENERATOR(gen, g, S_mus_file_name, "a generator");
  return(C_TO_XEN_STRING(mus_file_name(g)));
}



/* ---------------- oscil ---------------- */

static XEN g_make_oscil(XEN arg1, XEN arg2, XEN arg3, XEN arg4)
{
  #define H_make_oscil "(" S_make_oscil " (frequency *clm-default-frequency*) (initial-phase 0.0)): return a new " S_oscil " (sinewave) generator"
  mus_any *ge;
  int vals;
  XEN args[4]; 
  XEN keys[2];
  int orig_arg[2] = {0, 0};
  mus_float_t freq, phase = 0.0;

  freq = clm_default_frequency;
  keys[0] = kw_frequency;
  keys[1] = kw_initial_phase;

  args[0] = arg1; args[1] = arg2; args[2] = arg3; args[3] = arg4; 
  vals = mus_optkey_unscramble(S_make_oscil, 2, keys, args, orig_arg);
  if (vals > 0)
    {
      freq = mus_optkey_to_float(keys[0], S_make_oscil, orig_arg[0], freq);
      if (freq > (0.5 * mus_srate()))
	XEN_OUT_OF_RANGE_ERROR(S_make_oscil, orig_arg[0], keys[0], "freq ~A > srate/2?");
      phase = mus_optkey_to_float(keys[1], S_make_oscil, orig_arg[1], phase);
    }

  ge = mus_make_oscil(freq, phase);
  if (ge) return(mus_xen_to_object(mus_any_to_mus_xen(ge)));
  return(XEN_FALSE);
}


#if (!HAVE_SCHEME)
static XEN g_oscil(XEN os, XEN fm, XEN pm)
{
  #define H_oscil "(" S_oscil " gen (fm 0.0) (pm 0.0)): next sample from " S_oscil " gen: val = sin(phase + pm); phase += (freq + fm)"
  mus_float_t fm1 = 0.0, pm1 = 0.0;
  mus_any *g = NULL;

  XEN_TO_C_GENERATOR(os, g, mus_oscil_p, S_oscil, "an oscil");
  XEN_TO_C_DOUBLE_IF_BOUND(fm, fm1, S_oscil, XEN_ARG_2);
  XEN_TO_C_DOUBLE_IF_BOUND(pm, pm1, S_oscil, XEN_ARG_3);

  return(C_TO_XEN_DOUBLE(mus_oscil(g, fm1, pm1)));
}

#else
/* an experiment -- this appears to be about 1/3 faster then g_oscil + argified g_oscil_w
 */

static XEN g_oscil_w(s7_scheme *sc, s7_pointer args)
{
  #define H_oscil "(" S_oscil " gen (fm 0.0) (pm 0.0)): next sample from " S_oscil " gen: val = sin(phase + pm); phase += (freq + fm)"
  s7_pointer obj;
  mus_any *osc;

  obj = s7_car(args);
  if (s7_object_type(obj) != mus_xen_tag)
    s7_wrong_type_arg_error(s7, S_oscil, 1, obj, "an oscil"); 
  osc = (mus_any *)(((mus_xen *)s7_object_value(obj))->gen);

  if (mus_oscil_p(osc))
    {
      s7_pointer p;
      double fm, pm;
      args = s7_cdr(args);
      if (s7_is_null(s7, args))
	return(s7_make_real(sc, mus_oscil_unmodulated(osc)));

      p = s7_car(args);
      if (!s7_is_real(p))
	s7_wrong_type_arg_error(s7, S_oscil, 2, p, "a real"); 
      fm = s7_number_to_real(p);

      args = s7_cdr(args);
      if (s7_is_null(s7, args))
	return(s7_make_real(sc, mus_oscil_fm(osc, fm)));
      p = s7_car(args);
      if (!s7_is_real(p))
	s7_wrong_type_arg_error(s7, S_oscil, 3, p, "a real"); 
      pm = s7_number_to_real(p);

      return(s7_make_real(sc, mus_oscil(osc, fm, pm)));
    }
  XEN_ASSERT_TYPE(false, obj, XEN_ARG_1, S_oscil, "an oscil");
  return(s7_f(sc));
}
#endif



static XEN g_oscil_p(XEN os) 
{
  #define H_oscil_p "(" S_oscil_p " gen): " PROC_TRUE " if gen is an " S_oscil
  return(C_TO_XEN_BOOLEAN((MUS_XEN_P(os)) && (mus_oscil_p(XEN_TO_MUS_ANY(os)))));
}


static XEN g_mus_apply(XEN arglist)
{
  #define H_mus_apply "(" S_mus_apply " gen args...): apply gen to args"
  int arglist_len;
  mus_any *gen;

  arglist_len = XEN_LIST_LENGTH(arglist);
  if ((arglist_len > 3) || (arglist_len == 0)) 
    return(C_TO_XEN_DOUBLE(0.0));

  XEN_ASSERT_TYPE(MUS_XEN_P(XEN_CAR(arglist)), XEN_CAR(arglist), XEN_ARG_1, S_mus_apply, "a generator");

  gen = XEN_TO_MUS_ANY(XEN_CAR(arglist));
  if (arglist_len == 1) 
    return(C_TO_XEN_DOUBLE(mus_apply(gen, 0.0, 0.0)));
  if (arglist_len == 2)
    return(C_TO_XEN_DOUBLE(mus_apply(gen, 
				     XEN_TO_C_DOUBLE(XEN_CADR(arglist)),
				     0.0)));
  return(C_TO_XEN_DOUBLE(mus_apply(gen, 
				   XEN_TO_C_DOUBLE(XEN_CADR(arglist)), 
				   XEN_TO_C_DOUBLE(XEN_CADDR(arglist)))));
}



/* ---------------- delay ---------------- */


typedef enum {G_DELAY, G_COMB, G_NOTCH, G_ALL_PASS, G_MOVING_AVERAGE, G_FCOMB} xclm_delay_t;

static XEN g_make_delay_1(xclm_delay_t choice, XEN arglist)
{
  mus_any *ge = NULL, *filt = NULL;
  const char *caller = NULL;
  XEN args[MAX_ARGLIST_LEN]; 
  XEN keys[9];
  XEN xen_filt = XEN_FALSE;
  int orig_arg[9] = {0, 0, 0, 0, 0, 0, 0, (int)MUS_INTERP_NONE, 0};
  int vals, i, argn = 0, len = 0, arglist_len;
  mus_long_t max_size = -1, size = -1;
  int interp_type = (int)MUS_INTERP_NONE;
  mus_float_t *line = NULL;
  mus_float_t scaler = 0.0, feedback = 0.0, feedforward = 0.0;
  vct *initial_contents = NULL;
  XEN orig_v = XEN_FALSE;            /* initial-contents can be a vct */
  mus_float_t initial_element = 0.0;
  int scaler_key = -1, feedback_key = -1, feedforward_key = -1, size_key = -1, initial_contents_key = -1;
  int initial_element_key = -1, max_size_key = -1, interp_type_key = -1, filter_key = -1;
  bool size_set = false, max_size_set = false;

  switch (choice)
    {
    case G_DELAY:    caller = S_make_delay;                                                      break;
    case G_MOVING_AVERAGE: caller = S_make_moving_average;                                       break;
    case G_COMB:     caller = S_make_comb;     scaler_key = argn; keys[argn++] = kw_scaler;      break;
    case G_FCOMB:    caller = S_make_filtered_comb; scaler_key = argn; keys[argn++] = kw_scaler; break;
    case G_NOTCH:    caller = S_make_notch;    scaler_key = argn; keys[argn++] = kw_scaler;      break;
    case G_ALL_PASS: 
      caller = S_make_all_pass; 
      feedback_key = argn;
      keys[argn++] = kw_feedback; 
      feedforward_key = argn;
      keys[argn++] = kw_feedforward; 
      break;
    }

  size_key = argn;             keys[argn++] = kw_size;
  initial_contents_key = argn; keys[argn++] = kw_initial_contents;
  initial_element_key = argn;  keys[argn++] = kw_initial_element;
  max_size_key = argn;         keys[argn++] = kw_max_size;
  interp_type_key = argn;      keys[argn++] = kw_type;
  filter_key = argn;           keys[argn++] = kw_filter;

  arglist_len = XEN_LIST_LENGTH(arglist);
  if (arglist_len > MAX_ARGLIST_LEN)
    clm_error(caller, "too many args!", arglist);

  for (i = 0; i < arglist_len; i++) args[i] = XEN_LIST_REF(arglist, i);
  for (i = arglist_len; i < MAX_ARGLIST_LEN; i++) args[i] = XEN_UNDEFINED;
  vals = mus_optkey_unscramble(caller, argn, keys, args, orig_arg);

  if (vals > 0)
    {
      /* try to catch obvious type/range errors before allocations 
       *   a major complication here is that size can be 0
       */

      if (!(XEN_KEYWORD_P(keys[size_key])))
	{
	  size = mus_optkey_to_mus_long_t(keys[size_key], caller, orig_arg[size_key], size); /* size can  be 0? -- surely we need a line in any case? */
	  if (size < 0)
	    XEN_OUT_OF_RANGE_ERROR(caller, orig_arg[size_key], keys[size_key], "size ~A < 0?");
	  if (size > mus_max_table_size())
	    XEN_OUT_OF_RANGE_ERROR(caller, orig_arg[size_key], keys[size_key], "size ~A too large (see mus-max-table-size)");
	  size_set = true;
	}

      if (!(XEN_KEYWORD_P(keys[max_size_key])))
	{
	  max_size = mus_optkey_to_mus_long_t(keys[max_size_key], caller, orig_arg[max_size_key], max_size); /* -1 = unset */
	  if (max_size <= 0)
	    XEN_OUT_OF_RANGE_ERROR(caller, orig_arg[max_size_key], keys[max_size_key], "max-size ~A <= 0?");
	  if (max_size > mus_max_table_size())
	    XEN_OUT_OF_RANGE_ERROR(caller, orig_arg[max_size_key], keys[max_size_key], "max-size ~A too large (see mus-max-table-size)");
	  max_size_set = true;
	}

      if (XEN_KEYWORD_P(keys[interp_type_key]))
	{
	  /* if type not given, if max_size, assume linear interp (for possible tap), else no interp */
	  if ((max_size_set) && (max_size != size))
	    interp_type = (int)MUS_INTERP_LINEAR;
	  else interp_type = (int)MUS_INTERP_NONE;
	}
      else
	{
	  interp_type = mus_optkey_to_int(keys[interp_type_key], caller, orig_arg[interp_type_key], (int)MUS_INTERP_LINEAR);
	  if (!(mus_interp_type_p(interp_type)))
	    XEN_OUT_OF_RANGE_ERROR(caller, orig_arg[interp_type_key], keys[interp_type_key], "no such interp-type: ~A");
	}

      initial_element = mus_optkey_to_float(keys[initial_element_key], caller, orig_arg[initial_element_key], initial_element);

      switch (choice)
	{
	case G_DELAY: 
	case G_MOVING_AVERAGE:
	  break;

	case G_COMB: case G_NOTCH: case G_FCOMB:
	  scaler = mus_optkey_to_float(keys[scaler_key], caller, orig_arg[scaler_key], scaler);
	  break;

	case G_ALL_PASS:
	  feedback = mus_optkey_to_float(keys[feedback_key], caller, orig_arg[feedback_key], feedback);
	  feedforward = mus_optkey_to_float(keys[feedforward_key], caller, orig_arg[feedforward_key], feedforward);
	  break;
	}

      if (!(XEN_KEYWORD_P(keys[filter_key])))
	{
	  if (choice != G_FCOMB)
	    clm_error(caller, "filter arg passed??", keys[filter_key]);
				 
	  XEN_ASSERT_TYPE(MUS_XEN_P(keys[filter_key]), keys[filter_key], orig_arg[filter_key], caller, "filter arg must be a generator");
	  xen_filt = keys[filter_key];
	  filt = XEN_TO_MUS_ANY(xen_filt);
	}

      if (!(XEN_KEYWORD_P(keys[initial_contents_key])))
	{
	  if (!(XEN_KEYWORD_P(keys[initial_element_key])))
	    XEN_OUT_OF_RANGE_ERROR(caller, 
				   orig_arg[initial_contents_key], 
				   keys[initial_contents_key], 
				   "initial-contents and initial-element in same call?");
	  if (MUS_VCT_P(keys[initial_contents_key]))
	    {
	      initial_contents = XEN_TO_VCT(keys[initial_contents_key]);
	      orig_v = keys[initial_contents_key];
	    }
	  else
	    {
	      if (XEN_LIST_P_WITH_LENGTH(keys[initial_contents_key], len))
		{
		  if (len == 0) 
		    XEN_ERROR(NO_DATA,
			      XEN_LIST_2(C_TO_XEN_STRING("~A: initial-contents list empty?"),
					 C_TO_XEN_STRING(caller)));

		  orig_v = xen_list_to_vct(keys[initial_contents_key]);
		  initial_contents = XEN_TO_VCT(orig_v);
		  /* do I need to protect this until we read its contents? -- no extlang stuff except error returns */
		}
	      else XEN_ASSERT_TYPE(XEN_FALSE_P(keys[initial_contents_key]), 
				   keys[initial_contents_key], 
				   orig_arg[initial_contents_key], 
				   caller, "a vct or a list");
	    }
	  if (initial_contents)
	    {
	      if (size_set)
		{
		  if (size > initial_contents->length)
		    XEN_OUT_OF_RANGE_ERROR(caller, orig_arg[initial_contents_key], keys[initial_contents_key], "size > initial-contents length");
		}
	      else size = initial_contents->length;
	      if (max_size_set)
		{
		  if (max_size > initial_contents->length)
		    XEN_OUT_OF_RANGE_ERROR(caller, orig_arg[initial_contents_key], keys[initial_contents_key], "max-size > initial-contents length");
		}
	      else max_size = initial_contents->length;
	    }
	}
    } 
  /* here size can be (user-set to) 0, but max_size needs to be a reasonable allocation size */
  if (size < 0) size = 1;
  if (max_size < size) 
    {
      if (size == 0)
	max_size = 1;
      else max_size = size;
    }
  if ((choice == G_MOVING_AVERAGE) && (max_size != size))
    {
      if (size == 0)
	XEN_OUT_OF_RANGE_ERROR(caller, 0, C_TO_XEN_LONG_LONG(size), "size = 0 for the " S_moving_average " generator is kinda loony?");
      else XEN_OUT_OF_RANGE_ERROR(caller, 0, C_TO_XEN_LONG_LONG(max_size), "max_size is irrelevant to the " S_moving_average " generator");
    }

  if (initial_contents == NULL)
    {
      line = (mus_float_t *)calloc(max_size, sizeof(mus_float_t));
      if (line == NULL)
	return(clm_mus_error(MUS_MEMORY_ALLOCATION_FAILED, "can't allocate delay line"));
      orig_v = xen_make_vct(max_size, line);
      if (initial_element != 0.0) 
	for (i = 0; i < max_size; i++) 
	  line[i] = initial_element;
    }
  else
    {
      line = initial_contents->data;
    }

  {
    mus_error_handler_t *old_error_handler;
    old_error_handler = mus_error_set_handler(local_mus_error);
    switch (choice)
      {
      case G_DELAY:    ge = mus_make_delay(size, line, max_size, (mus_interp_t)interp_type);                           break;
      case G_MOVING_AVERAGE:  ge = mus_make_moving_average(size, line);                                                break;
      case G_COMB:     ge = mus_make_comb(scaler, size, line, max_size, (mus_interp_t)interp_type);                    break;
      case G_NOTCH:    ge = mus_make_notch(scaler, size, line, max_size, (mus_interp_t)interp_type);                   break;
      case G_ALL_PASS: ge = mus_make_all_pass(feedback, feedforward, size, line, max_size, (mus_interp_t)interp_type); break;
      case G_FCOMB:    ge = mus_make_filtered_comb(scaler, size, line, max_size, (mus_interp_t)interp_type, filt);     break;
      }
    mus_error_set_handler(old_error_handler);
  }

  if (ge) 
    {
      if (choice != G_FCOMB)
	return(mus_xen_to_object(mus_any_to_mus_xen_with_vct(ge, orig_v)));
      return(mus_xen_to_object(mus_any_to_mus_xen_with_two_vcts(ge, orig_v, xen_filt)));
    }
  return(clm_mus_error(local_error_type, local_error_msg));
}


static XEN g_make_delay(XEN args) 
{
  #define H_make_delay "(" S_make_delay " (size) (initial-contents) (initial-element 0.0) (max-size) (type mus-interp-linear)): \
return a new delay line of size elements. \
If the delay length will be changing at run-time, max-size sets its maximum length, so\n\
   (" S_make_delay " len :max-size (+ len 10))\n\
provides 10 extra elements of delay for subsequent phasing or flanging. \
initial-contents can be either a list or a vct."

  return(g_make_delay_1(G_DELAY, args));
}


static XEN g_make_comb(XEN args) 
{
  #define H_make_comb "(" S_make_comb " (scaler) (size) (initial-contents) (initial-element 0.0) (max-size) (type " S_mus_interp_linear ")): \
return a new comb filter (a delay line with a scaler on the feedback) of size elements. \
If the comb length will be changing at run-time, max-size sets its maximum length. \
initial-contents can be either a list or a vct."

  return(g_make_delay_1(G_COMB, args));
}


static XEN g_make_filtered_comb(XEN args) 
{
  #define H_make_filtered_comb "(" S_make_filtered_comb " (scaler) (size) (initial-contents) (initial-element 0.0) (max-size) (type " S_mus_interp_linear ") :filter): \
return a new filtered comb filter (a delay line with a scaler and a filter on the feedback) of size elements. \
If the comb length will be changing at run-time, max-size sets its maximum length. \
initial-contents can be either a list or a vct."

  return(g_make_delay_1(G_FCOMB, args));
}


static XEN g_make_notch(XEN args) 
{
  #define H_make_notch "(" S_make_notch " (scaler) (size) (initial-contents) (initial-element 0.0) (max-size) (type " S_mus_interp_linear ")): \
return a new notch filter (a delay line with a scaler on the feedforward) of size elements. \
If the notch length will be changing at run-time, max-size sets its maximum length. \
initial-contents can be either a list or a vct."

  return(g_make_delay_1(G_NOTCH, args));
}


static XEN g_make_all_pass(XEN args) 
{
  #define H_make_all_pass "(" S_make_all_pass " (feedback) (feedforward) (size) (initial-contents) (initial-element 0.0) (max-size) (type " S_mus_interp_linear ")): \
return a new allpass filter (a delay line with a scalers on both the feedback and the feedforward). \
If the " S_all_pass " length will be changing at run-time, max-size sets its maximum length. \
initial-contents can be either a list or a vct."

  return(g_make_delay_1(G_ALL_PASS, args));
}


static XEN g_make_moving_average(XEN args) 
{
  #define H_make_moving_average "(" S_make_moving_average " (size) (initial-contents) (initial-element 0.0)): \
return a new moving_average generator. initial-contents can be either a list or a vct."

  return(g_make_delay_1(G_MOVING_AVERAGE, args));
}


static XEN g_delay(XEN obj, XEN input, XEN pm)
{
  #define H_delay "(" S_delay " gen (val 0.0) (pm 0.0)): \
delay val according to the delay line's length and pm ('phase-modulation'). \
If pm is greater than 0.0, the max-size argument used to create gen should have accommodated its maximum value."

  mus_float_t in1 = 0.0, pm1 = 0.0;
  mus_any *g = NULL;

  XEN_TO_C_GENERATOR(obj, g, mus_delay_p, S_delay, "a delay line");
  XEN_TO_C_DOUBLE_IF_BOUND(input, in1, S_delay, XEN_ARG_2);
  XEN_TO_C_DOUBLE_IF_BOUND(pm, pm1, S_delay, XEN_ARG_3);

  return(C_TO_XEN_DOUBLE(mus_delay(g, in1, pm1)));
}


static XEN g_delay_tick(XEN obj, XEN input)
{
  #define H_delay_tick "(" S_delay_tick " gen (val 0.0)): \
delay val according to the delay line's length. This merely 'ticks' the delay line forward.\
The argument 'val' is returned."

  mus_float_t in1 = 0.0;
  mus_any *g = NULL;

  XEN_TO_C_GENERATOR(obj, g, mus_delay_p, S_delay_tick, "a delay line");
  XEN_TO_C_DOUBLE_IF_BOUND(input, in1, S_delay_tick, XEN_ARG_2);

  return(C_TO_XEN_DOUBLE(mus_delay_tick(g, in1)));
}


static XEN g_notch(XEN obj, XEN input, XEN pm)
{
  #define H_notch "(" S_notch " gen (val 0.0) (pm 0.0)): notch filter val, pm changes the delay length."

  mus_float_t in1 = 0.0, pm1 = 0.0;
  mus_any *g = NULL;

  XEN_TO_C_GENERATOR(obj, g, mus_notch_p, S_notch, "a notch filter");
  XEN_TO_C_DOUBLE_IF_BOUND(input, in1, S_notch, XEN_ARG_2);
  XEN_TO_C_DOUBLE_IF_BOUND(pm, pm1, S_notch, XEN_ARG_3);

  return(C_TO_XEN_DOUBLE(mus_notch(g, in1, pm1)));
}


static XEN g_comb(XEN obj, XEN input, XEN pm)
{
  #define H_comb "(" S_comb " gen (val 0.0) (pm 0.0)): comb filter val, pm changes the delay length."
  mus_float_t in1 = 0.0, pm1 = 0.0;
  mus_any *g = NULL;

  XEN_TO_C_GENERATOR(obj, g, mus_comb_p, S_comb, "a comb generator");
  XEN_TO_C_DOUBLE_IF_BOUND(input, in1, S_comb, XEN_ARG_2);
  XEN_TO_C_DOUBLE_IF_BOUND(pm, pm1, S_comb, XEN_ARG_3);

  return(C_TO_XEN_DOUBLE(mus_comb(g, in1, pm1)));
}


static XEN g_filtered_comb(XEN obj, XEN input, XEN pm)
{
  #define H_filtered_comb "(" S_filtered_comb " gen (val 0.0) (pm 0.0)): filtered comb filter val, pm changes the delay length."
  mus_float_t in1 = 0.0, pm1 = 0.0;
  mus_any *g = NULL;

  XEN_TO_C_GENERATOR(obj, g, mus_filtered_comb_p, S_filtered_comb, "a filtered-comb generator");
  XEN_TO_C_DOUBLE_IF_BOUND(input, in1, S_filtered_comb, XEN_ARG_2);
  XEN_TO_C_DOUBLE_IF_BOUND(pm, pm1, S_filtered_comb, XEN_ARG_3);

  return(C_TO_XEN_DOUBLE(mus_filtered_comb(g, in1, pm1)));
}


static XEN g_all_pass(XEN obj, XEN input, XEN pm)
{
  #define H_all_pass "(" S_all_pass " gen (val 0.0) (pm 0.0)): all-pass filter val, pm changes the delay length."
  mus_float_t in1 = 0.0, pm1 = 0.0;
  mus_any *g = NULL;

  XEN_TO_C_GENERATOR(obj, g, mus_all_pass_p, S_all_pass, "an all-pass filter");
  XEN_TO_C_DOUBLE_IF_BOUND(input, in1, S_all_pass, XEN_ARG_2);
  XEN_TO_C_DOUBLE_IF_BOUND(pm, pm1, S_all_pass, XEN_ARG_3);

  return(C_TO_XEN_DOUBLE(mus_all_pass(g, in1, pm1)));
}


static XEN g_moving_average(XEN obj, XEN input)
{
  #define H_moving_average "(" S_moving_average " gen (val 0.0)): moving window moving_average."
  mus_float_t in1 = 0.0;
  mus_any *g = NULL;

  XEN_TO_C_GENERATOR(obj, g, mus_moving_average_p, S_moving_average, "a moving-average generator");
  XEN_TO_C_DOUBLE_IF_BOUND(input, in1, S_moving_average, XEN_ARG_2);

  return(C_TO_XEN_DOUBLE(mus_moving_average(g, in1)));
}


static XEN g_tap(XEN obj, XEN loc)
{
  #define H_tap "(" S_tap " gen (pm 0.0)): tap the " S_delay " generator offset by pm"
  mus_float_t dloc = 0.0;
  mus_any *g = NULL;

  XEN_TO_C_GENERATOR(obj, g, mus_delay_line_p, S_tap, "a delay line tap");
  XEN_TO_C_DOUBLE_IF_BOUND(loc, dloc, S_tap, XEN_ARG_3);

  return(C_TO_XEN_DOUBLE(mus_tap(g, dloc)));
}


static XEN g_delay_p(XEN obj) 
{
  #define H_delay_p "(" S_delay_p " gen): " PROC_TRUE " if gen is a delay line"
  return(C_TO_XEN_BOOLEAN((MUS_XEN_P(obj)) && (mus_delay_p(XEN_TO_MUS_ANY(obj)))));
}


static XEN g_comb_p(XEN obj)
{
  #define H_comb_p "(" S_comb_p " gen): " PROC_TRUE " if gen is a comb filter"
  return(C_TO_XEN_BOOLEAN((MUS_XEN_P(obj)) && (mus_comb_p(XEN_TO_MUS_ANY(obj)))));
}


static XEN g_filtered_comb_p(XEN obj)
{
  #define H_filtered_comb_p "(" S_filtered_comb_p " gen): " PROC_TRUE " if gen is a filtered-comb filter"
  return(C_TO_XEN_BOOLEAN((MUS_XEN_P(obj)) && (mus_filtered_comb_p(XEN_TO_MUS_ANY(obj)))));
}


static XEN g_notch_p(XEN obj) 
{
  #define H_notch_p "(" S_notch_p " gen): " PROC_TRUE " if gen is a notch filter"
  return(C_TO_XEN_BOOLEAN((MUS_XEN_P(obj)) && (mus_notch_p(XEN_TO_MUS_ANY(obj)))));
}


static XEN g_all_pass_p(XEN obj) 
{
  #define H_all_pass_p "(" S_all_pass_p " gen): " PROC_TRUE " if gen is an all-pass filter"
  return(C_TO_XEN_BOOLEAN((MUS_XEN_P(obj)) && (mus_all_pass_p(XEN_TO_MUS_ANY(obj)))));
}


static XEN g_moving_average_p(XEN obj) 
{
  #define H_moving_average_p "(" S_moving_average_p " gen): " PROC_TRUE " if gen is a moving-average generator"
  return(C_TO_XEN_BOOLEAN((MUS_XEN_P(obj)) && (mus_moving_average_p(XEN_TO_MUS_ANY(obj)))));
}


/* -------- ncos -------- */

static XEN g_ncos_p(XEN obj) 
{
  #define H_ncos_p "(" S_ncos_p " gen): " PROC_TRUE " if gen is an " S_ncos " generator"
  return(C_TO_XEN_BOOLEAN((MUS_XEN_P(obj)) && 
			  (mus_ncos_p(XEN_TO_MUS_ANY(obj)))));
}


static XEN g_make_ncos(XEN arg1, XEN arg2, XEN arg3, XEN arg4)
{
  #define H_make_ncos "(" S_make_ncos " (frequency *clm-default-frequency*) (n 1)): \
return a new " S_ncos " generator, producing a sum of 'n' equal amplitude cosines."

  mus_any *ge;
  XEN args[4]; 
  XEN keys[2];
  int orig_arg[2] = {0, 0};
  int vals, n = 1;
  mus_float_t freq;

  freq = clm_default_frequency;

  keys[0] = kw_frequency;
  keys[1] = kw_n;
  args[0] = arg1; args[1] = arg2; args[2] = arg3; args[3] = arg4;

  vals = mus_optkey_unscramble(S_make_ncos, 2, keys, args, orig_arg);
  if (vals > 0)
    {
      freq = mus_optkey_to_float(keys[0], S_make_ncos, orig_arg[0], freq);
      if (freq > (0.5 * mus_srate()))
	XEN_OUT_OF_RANGE_ERROR(S_make_ncos, orig_arg[0], keys[0], "freq ~A > srate/2?");

      n = mus_optkey_to_int(keys[1], S_make_ncos, orig_arg[1], n);
      if (n <= 0)
	XEN_OUT_OF_RANGE_ERROR(S_make_ncos, orig_arg[1], keys[1], "n: ~A?");
    }

  ge = mus_make_ncos(freq, n);
  if (ge) return(mus_xen_to_object(mus_any_to_mus_xen(ge)));
  return(XEN_FALSE);
}


static XEN g_ncos(XEN obj, XEN fm)
{
  #define H_ncos "(" S_ncos " gen (fm 0.0)): get the next sample from 'gen', an " S_ncos " generator"

  mus_float_t fm1 = 0.0;
  mus_any *g = NULL;

  XEN_TO_C_GENERATOR(obj, g, mus_ncos_p, S_ncos, "an ncos generator");
  XEN_TO_C_DOUBLE_IF_BOUND(fm, fm1, S_ncos, XEN_ARG_2);

  return(C_TO_XEN_DOUBLE(mus_ncos(g, fm1)));
}



/* -------- nsin -------- */

static XEN g_nsin_p(XEN obj) 
{
  #define H_nsin_p "(" S_nsin_p " gen): " PROC_TRUE " if gen is an " S_nsin " generator"
  return(C_TO_XEN_BOOLEAN((MUS_XEN_P(obj)) && 
			  (mus_nsin_p(XEN_TO_MUS_ANY(obj)))));
}


static XEN g_make_nsin(XEN arg1, XEN arg2, XEN arg3, XEN arg4)
{
  #define H_make_nsin "(" S_make_nsin " (frequency *clm-default-frequency*) (n 1)): \
return a new " S_nsin " generator, producing a sum of 'n' equal amplitude sines"

  mus_any *ge;
  XEN args[4]; 
  XEN keys[2];
  int orig_arg[2] = {0, 0};
  int vals, n = 1;
  mus_float_t freq;

  freq = clm_default_frequency;

  keys[0] = kw_frequency;
  keys[1] = kw_n;
  args[0] = arg1; args[1] = arg2; args[2] = arg3; args[3] = arg4;

  vals = mus_optkey_unscramble(S_make_nsin, 2, keys, args, orig_arg);
  if (vals > 0)
    {
      freq = mus_optkey_to_float(keys[0], S_make_nsin, orig_arg[0], freq);
      if (freq > (0.5 * mus_srate()))
	XEN_OUT_OF_RANGE_ERROR(S_make_nsin, orig_arg[0], keys[0], "freq ~A > srate/2?");

      n = mus_optkey_to_int(keys[1], S_make_nsin, orig_arg[1], n);
      if (n <= 0)
	XEN_OUT_OF_RANGE_ERROR(S_make_nsin, orig_arg[1], keys[1], "n: ~A?");
    }

  ge = mus_make_nsin(freq, n);
  if (ge) return(mus_xen_to_object(mus_any_to_mus_xen(ge)));
  return(XEN_FALSE);
}


static XEN g_nsin(XEN obj, XEN fm)
{
  #define H_nsin "(" S_nsin " gen (fm 0.0)): get the next sample from 'gen', an " S_nsin " generator"

  mus_float_t fm1 = 0.0;
  mus_any *g = NULL;

  XEN_TO_C_GENERATOR(obj, g, mus_nsin_p, S_nsin, "an nsin generator");
  XEN_TO_C_DOUBLE_IF_BOUND(fm, fm1, S_nsin, XEN_ARG_2);

  return(C_TO_XEN_DOUBLE(mus_nsin(g, fm1)));
}



/* ---------------- rand, rand_interp ---------------- */

#define RANDOM_DISTRIBUTION_TABLE_SIZE 512
#define RANDOM_DISTRIBUTION_ENVELOPE_SIZE 50

static mus_float_t *inverse_integrate(XEN dist, int data_size)
{
  /* e = env possibly starting < 0 */
  int e_size = RANDOM_DISTRIBUTION_ENVELOPE_SIZE;
  mus_float_t *e, *data;
  int i, e_len, lim, e_loc = 2;
  XEN ex0, ex1, ey0, ey1;
  mus_float_t x, x0, x1, xincr, y0, y1, sum = 0.0, first_sum = 0.0, last_sum = 0.0;

  lim = (e_size + 1) * 2;
  e = (mus_float_t *)calloc(lim, sizeof(mus_float_t));

  e_len = XEN_LIST_LENGTH(dist);
  ex0 = XEN_LIST_REF(dist, 0);
  ex1 = XEN_LIST_REF(dist, e_len - 2);
  x0 = XEN_TO_C_DOUBLE(ex0);
  /* get x range first */
  x1 = XEN_TO_C_DOUBLE(ex1);
  xincr = (x1 - x0) / (mus_float_t)e_size;
  /* now true x1 */
  ex1 = XEN_LIST_REF(dist, 2);
  x1 = XEN_TO_C_DOUBLE(ex1);
  ey0 = XEN_LIST_REF(dist, 1);
  ey1 = XEN_LIST_REF(dist, 3);
  y0 = XEN_TO_C_DOUBLE(ey0);
  y1 = XEN_TO_C_DOUBLE(ey1);
  sum = y0;
  first_sum = sum;

  for (i = 0,  x = x0; i < lim; i += 2, x += xincr)
    {
      e[i] = sum;
      last_sum = sum;
      e[i + 1] = x;
      while ((x >= x1) && ((e_loc + 2) < e_len))
	{
	  x0 = x1;
	  y0 = y1;
	  e_loc += 2;
	  ex1 = XEN_LIST_REF(dist, e_loc);
	  ey1 = XEN_LIST_REF(dist, e_loc + 1);
	  x1 = XEN_TO_C_DOUBLE(ex1);
	  y1 = XEN_TO_C_DOUBLE(ey1);
	}
      if ((x == x0) || (x0 == x1))
	sum += y0;
      else sum += (y0 + (y1 - y0) * (x - x0) / (x1 - x0));
    }

  xincr = (last_sum - first_sum) / (mus_float_t)(data_size - 1);
  data = (mus_float_t *)calloc(data_size, sizeof(mus_float_t));
  x0 = e[0];
  x1 = e[2];
  y0 = e[1];
  y1 = e[3];
  e_len = lim;
  e_loc = 2;

  for (i = 0, x = first_sum; i < data_size; i++, x += xincr)
    {
      while ((x >= x1) && ((e_loc + 2) < e_len))
	{
	  x0 = x1;
	  y0 = y1;
	  e_loc += 2;
	  x1 = e[e_loc];
	  y1 = e[e_loc + 1];
	}
      if ((x == x0) || (x0 == x1))
	data[i] = y0;
      else data[i] = (y0 + (y1 - y0) * (x - x0) / (x1 - x0));
    }
  free(e);
  return(data);
}


static XEN g_make_noi(bool rand_case, const char *caller, XEN arglist)
{
  mus_any *ge = NULL;
  XEN args[MAX_ARGLIST_LEN]; 
  XEN keys[5];
  int orig_arg[5] = {0, 0, 0, 0, 0};
  int i, vals, arglist_len;
  mus_float_t freq, base = 1.0;
  mus_float_t *distribution = NULL;
  vct *v = NULL;
  XEN orig_v = XEN_FALSE;
  int distribution_size = RANDOM_DISTRIBUTION_TABLE_SIZE;

  freq = clm_default_frequency;

  keys[0] = kw_frequency;
  keys[1] = kw_amplitude;
  keys[2] = kw_envelope;
  keys[3] = kw_distribution;
  keys[4] = kw_size;

  arglist_len = XEN_LIST_LENGTH(arglist);
  if (arglist_len > MAX_ARGLIST_LEN)
    clm_error(caller, "too many args!", arglist);

  for (i = 0; i < arglist_len; i++) args[i] = XEN_LIST_REF(arglist, i);
  for (i = arglist_len; i < MAX_ARGLIST_LEN; i++) args[i] = XEN_UNDEFINED;

  vals = mus_optkey_unscramble(caller, 5, keys, args, orig_arg);
  if (vals > 0)
    {
      freq = mus_optkey_to_float(keys[0], caller, orig_arg[0], freq);
      if (freq > mus_srate())
	XEN_OUT_OF_RANGE_ERROR(caller, orig_arg[0], keys[0], "freq ~A > srate/2?");

      base = mus_optkey_to_float(keys[1], caller, orig_arg[1], base);

      distribution_size = mus_optkey_to_int(keys[4], caller, orig_arg[4], distribution_size);
      if (distribution_size <= 0)
	XEN_OUT_OF_RANGE_ERROR(caller, orig_arg[4], keys[4], "distribution size ~A <= 0?");
      if (distribution_size > mus_max_table_size())
	XEN_OUT_OF_RANGE_ERROR(caller, orig_arg[4], keys[4], "distribution size ~A too large (see mus-max-table-size)");

      if (!(XEN_KEYWORD_P(keys[2]))) /* i.e. envelope arg was specified */
        {
	  int len;
	  XEN_ASSERT_TYPE(XEN_LIST_P(keys[2]), keys[2], orig_arg[2], caller, "an envelope");
	  len = XEN_LIST_LENGTH(keys[2]);
	  if ((len < 4) || (len & 1))
	    clm_error(caller, "bad distribution envelope", keys[2]);
	  /* envelope and distribution are incompatible */
	  if (!(XEN_KEYWORD_P(keys[3])))
	    clm_error(caller, ":envelope and :distribution in same call?", keys[3]);
	  distribution = inverse_integrate(keys[2], distribution_size);
	  orig_v = xen_make_vct(distribution_size, distribution);
	}
      else
	{
	  if (!(XEN_KEYWORD_P(keys[3]))) /* i.e. distribution arg was specified */
	    {
	      XEN_ASSERT_TYPE(MUS_VCT_P(keys[3]) || XEN_FALSE_P(keys[3]), keys[3], orig_arg[3], caller, "a vct");
	      if (MUS_VCT_P(keys[3]))
		{
		  orig_v = keys[3];
		  v = mus_optkey_to_vct(orig_v, caller, orig_arg[3], NULL);
		  distribution_size = v->length;
		  distribution = v->data;
		}
	    }
	}
    }
  if (!distribution)
    {
      if (rand_case)
	ge = mus_make_rand(freq, base);
      else ge = mus_make_rand_interp(freq, base);
    }
  else
    {
      if (rand_case)
	ge = mus_make_rand_with_distribution(freq, base, distribution, distribution_size);
      else ge = mus_make_rand_interp_with_distribution(freq, base, distribution, distribution_size);
    }
  if (ge)
    {
      if (MUS_VCT_P(orig_v))
	return(mus_xen_to_object(mus_any_to_mus_xen_with_vct(ge, orig_v)));
      return(mus_xen_to_object(mus_any_to_mus_xen(ge)));
    }
  return(XEN_FALSE);
}


static XEN g_make_rand_interp(XEN arglist)
{
  #define H_make_rand_interp "(" S_make_rand_interp " (frequency *clm-default-frequency*) (amplitude 1.0) (envelope) (distribution) (size)): \
return a new " S_rand_interp " generator, producing linearly interpolated random numbers. \
frequency is the rate at which new end-points are chosen."

  return(g_make_noi(false, S_make_rand_interp, arglist));
}


static XEN g_make_rand(XEN arglist)
{
  #define H_make_rand "(" S_make_rand " (frequency *clm-default-frequency*) (amplitude 1.0) (envelope) (distribution) (size)): \
return a new " S_rand " generator, producing a sequence of random numbers (a step  function). \
frequency is the rate at which new numbers are chosen."

  return(g_make_noi(true, S_make_rand, arglist));
}


static XEN g_rand(XEN obj, XEN fm)
{
  #define H_rand "(" S_rand " gen (fm 0.0)): gen's current random number. \
fm modulates the rate at which the current number is changed."

  mus_float_t fm1 = 0.0;
  mus_any *g = NULL;

  XEN_TO_C_GENERATOR(obj, g, mus_rand_p, S_rand, "a rand generator");
  XEN_TO_C_DOUBLE_IF_BOUND(fm, fm1, S_rand, XEN_ARG_2);

  return(C_TO_XEN_DOUBLE(mus_rand(g, fm1)));
}


static XEN g_rand_p(XEN obj) 
{
  #define H_rand_p "(" S_rand_p " gen): " PROC_TRUE " if gen is a " S_rand
  return(C_TO_XEN_BOOLEAN((MUS_XEN_P(obj)) && (mus_rand_p(XEN_TO_MUS_ANY(obj)))));
}


static XEN g_rand_interp(XEN obj, XEN fm)
{
  #define H_rand_interp "(" S_rand_interp " gen (fm 0.0)): gen's current (interpolating) random number. \
fm modulates the rate at which new segment end-points are chosen."

  mus_float_t fm1 = 0.0;
  mus_any *g = NULL;

  XEN_TO_C_GENERATOR(obj, g, mus_rand_interp_p, S_rand_interp, "a rand-interp generator");
  XEN_TO_C_DOUBLE_IF_BOUND(fm, fm1, S_rand_interp, XEN_ARG_2);

  return(C_TO_XEN_DOUBLE(mus_rand_interp(g, fm1)));
}


static XEN g_rand_interp_p(XEN obj) 
{
  #define H_rand_interp_p "(" S_rand_interp_p " gen): " PROC_TRUE " if gen is a " S_rand_interp
  return(C_TO_XEN_BOOLEAN((MUS_XEN_P(obj)) && (mus_rand_interp_p(XEN_TO_MUS_ANY(obj)))));
}


static XEN g_mus_random(XEN a) 
{
  #define H_mus_random "(" S_mus_random " val): a random number between -val and val. \
the built-in 'random' function returns values between 0 and its argument"
  mus_float_t x = 0.0;

  XEN_TO_C_DOUBLE_OR_ERROR(a, x, S_mus_random, XEN_ONLY_ARG);
  return(C_TO_XEN_DOUBLE(mus_random(x)));
}


static XEN g_mus_rand_seed(void) 
{
  #define H_mus_rand_seed "(" S_mus_rand_seed "): the random number seed; \
this can be used to re-run a particular random number sequence."

  return(C_TO_XEN_INT(mus_rand_seed()));
}


static XEN g_mus_set_rand_seed(XEN a) 
{
  XEN_ASSERT_TYPE(XEN_INTEGER_P(a), a, XEN_ONLY_ARG, S_setB S_mus_rand_seed, "an integer");
  mus_set_rand_seed((unsigned long)XEN_TO_C_INT(a)); 
  return(a);
}



/* ---------------- table lookup ---------------- */

static XEN g_table_lookup_p(XEN obj) 
{
  #define H_table_lookup_p "(" S_table_lookup_p " gen): " PROC_TRUE " if gen is a " S_table_lookup
  return(C_TO_XEN_BOOLEAN((MUS_XEN_P(obj)) && (mus_table_lookup_p(XEN_TO_MUS_ANY(obj)))));
}


static XEN g_partials_to_wave(XEN partials, XEN utable, XEN normalize)
{
  #define H_partials_to_wave "(" S_partials_to_wave " partials wave (normalize " PROC_FALSE ")): \
take a list or vct of partials (harmonic number and associated amplitude) and produce \
a waveform for use in " S_table_lookup ".  If wave (a vct) is not given, \
a new one is created.  If normalize is " PROC_TRUE ", the resulting waveform goes between -1.0 and 1.0.\n\
  (set! gen (" S_make_table_lookup " 440.0 :wave (" S_partials_to_wave " '(1 1.0 2 .5))))"

  vct *f;
  XEN table; 
  XEN lst;
  mus_float_t *partial_data = NULL;
  mus_long_t len = 0, i;
  bool partials_allocated = true;
#if HAVE_SCHEME
  int gc_loc;
#endif

  XEN_ASSERT_TYPE(MUS_VCT_P(partials) || XEN_LIST_P(partials), partials, XEN_ARG_1, S_partials_to_wave, "a list or a vct");
  XEN_ASSERT_TYPE(MUS_VCT_P(utable) || XEN_FALSE_P(utable) || (!(XEN_BOUND_P(utable))), utable, XEN_ARG_2, S_partials_to_wave, "a vct or " PROC_FALSE);
  XEN_ASSERT_TYPE(XEN_BOOLEAN_IF_BOUND_P(normalize), normalize, XEN_ARG_3, S_partials_to_wave, "a boolean");

  if (MUS_VCT_P(partials))
    {
      vct *v;
      v = XEN_TO_VCT(partials);
      partial_data = v->data;
      len = v->length;
      partials_allocated = false;
    }
  else
    {
      len = XEN_LIST_LENGTH(partials);
      if (len == 0)
	XEN_ERROR(NO_DATA, 
		  XEN_LIST_2(C_TO_XEN_STRING("~A: partials list empty?"), 
			     C_TO_XEN_STRING(S_partials_to_wave)));

      if (!(XEN_NUMBER_P(XEN_CAR(partials))))
	XEN_ASSERT_TYPE(false, partials, XEN_ARG_1, S_partials_to_wave, "a list of numbers (partial numbers with amplitudes)");
    }
  if (len & 1)
    XEN_ERROR(BAD_TYPE,
	      XEN_LIST_3(C_TO_XEN_STRING("~A: odd length partials list? ~A"), 
			 C_TO_XEN_STRING(S_partials_to_wave), 
			 partials));

  if ((XEN_NOT_BOUND_P(utable)) || (!(MUS_VCT_P(utable))))
    {
      mus_float_t *wave;
      wave = (mus_float_t *)calloc(clm_table_size, sizeof(mus_float_t));
      if (wave == NULL)
	return(clm_mus_error(MUS_MEMORY_ALLOCATION_FAILED, "can't allocate wave table"));
      table = xen_make_vct(clm_table_size, wave);
    }
  else table = utable;

#if HAVE_SCHEME
  gc_loc = s7_gc_protect(s7, table);
#endif

  f = XEN_TO_VCT(table);

  if (!partial_data)
    {
      partial_data = (mus_float_t *)calloc(len, sizeof(mus_float_t));
      if (partial_data == NULL)
	return(clm_mus_error(MUS_MEMORY_ALLOCATION_FAILED, "can't allocate partials table"));
      for (i = 0, lst = XEN_COPY_ARG(partials); i < len; i++, lst = XEN_CDR(lst)) 
	partial_data[i] = XEN_TO_C_DOUBLE_OR_ELSE(XEN_CAR(lst), 0.0);
    }

  mus_partials_to_wave(partial_data, len / 2, f->data, f->length, (XEN_TRUE_P(normalize)));

  if (partials_allocated)
    free(partial_data);

#if HAVE_SCHEME
  s7_gc_unprotect_at(s7, gc_loc);
#endif

  return(table);
}


static XEN g_phase_partials_to_wave(XEN partials, XEN utable, XEN normalize)
{
  vct *f;
  XEN table, lst;
  mus_float_t *partial_data = NULL, *wave;
  mus_long_t len = 0, i;
  bool partials_allocated = true;
#if HAVE_SCHEME
  int gc_loc;
#endif

  #if HAVE_SCHEME
    #define pp2w_example "(" S_make_table_lookup " 440.0 :wave (" S_phase_partials_to_wave " (list  1 .75 0.0  2 .25 (* 3.14159 .5))))"
  #endif
  #if HAVE_RUBY
    #define pp2w_example "make_table_lookup(440.0, :wave, phase_partials2wave([1.0, 0.75, 0.0,  2.0, 0.25, 3.14159 * 0.5]))"
  #endif
  #if HAVE_FORTH
    #define pp2w_example "440.0 0.0 '( 1.0 0.75 0.0 2.0 0.25 3.14159 0.5 f* ) #f #f phase-partials->wave make-table-lookup"
  #endif

  #define H_phase_partials_to_wave "(" S_phase_partials_to_wave " partials wave (normalize " PROC_FALSE ")): \
take a list or vct of partials (harmonic number, amplitude, initial phase) and produce \
a waveform for use in " S_table_lookup ".  If wave (a vct) is not given, \
a new one is created.  If normalize is " PROC_TRUE ", the resulting waveform goes between -1.0 and 1.0.\n  " pp2w_example

  XEN_ASSERT_TYPE(MUS_VCT_P(partials) || XEN_LIST_P(partials), partials, XEN_ARG_1, S_phase_partials_to_wave, "a list or a vct");
  XEN_ASSERT_TYPE(MUS_VCT_P(utable) || XEN_FALSE_P(utable) || (!(XEN_BOUND_P(utable))), utable, XEN_ARG_2, S_phase_partials_to_wave, "a vct or " PROC_FALSE);
  XEN_ASSERT_TYPE(XEN_BOOLEAN_IF_BOUND_P(normalize), normalize, XEN_ARG_3, S_phase_partials_to_wave, "a boolean");

  if (MUS_VCT_P(partials))
    {
      vct *v;
      v = XEN_TO_VCT(partials);
      partial_data = v->data;
      len = v->length;
      partials_allocated = false;
    }
  else
    {
      len = XEN_LIST_LENGTH(partials);
      if (len == 0)
	XEN_ERROR(NO_DATA,
		  XEN_LIST_2(C_TO_XEN_STRING("~A: partials list empty?"),
			     C_TO_XEN_STRING(S_phase_partials_to_wave)));

      if (!(XEN_NUMBER_P(XEN_CAR(partials))))
	XEN_ASSERT_TYPE(false, partials, XEN_ARG_1, S_phase_partials_to_wave, "a list of numbers (partial numbers with amplitudes and phases)");
    }
  if ((len % 3) != 0)
    XEN_ERROR(XEN_ERROR_TYPE("wrong-type-arg"),
	      XEN_LIST_3(C_TO_XEN_STRING("~A: partials list, ~A, should have 3 entries for each harmonic (number amp phase)"),
			 C_TO_XEN_STRING(S_phase_partials_to_wave), 
			 partials));

  if ((XEN_NOT_BOUND_P(utable)) || (!(MUS_VCT_P(utable))))
    {
      wave = (mus_float_t *)calloc(clm_table_size, sizeof(mus_float_t));
      if (wave == NULL)
	return(clm_mus_error(MUS_MEMORY_ALLOCATION_FAILED, "can't allocate wave table"));
      table = xen_make_vct(clm_table_size, wave);
    }
  else table = utable;

#if HAVE_SCHEME
  gc_loc = s7_gc_protect(s7, table);
#endif

  f = XEN_TO_VCT(table);

  if (!partial_data)
    {
      partial_data = (mus_float_t *)calloc(len, sizeof(mus_float_t));
      if (partial_data == NULL)
	return(clm_mus_error(MUS_MEMORY_ALLOCATION_FAILED, "can't allocate partials table"));
      for (i = 0, lst = XEN_COPY_ARG(partials); i < len; i++, lst = XEN_CDR(lst)) 
	partial_data[i] = XEN_TO_C_DOUBLE_OR_ELSE(XEN_CAR(lst), 0.0);
    }

  mus_phase_partials_to_wave(partial_data, len / 3, f->data, f->length, (XEN_TRUE_P(normalize)));

  if (partials_allocated)
    free(partial_data);

#if HAVE_SCHEME
  s7_gc_unprotect_at(s7, gc_loc);
#endif

  return(table);
}


static XEN g_make_table_lookup(XEN arglist)
{
  #define H_make_table_lookup "(" S_make_table_lookup " (frequency *clm-default-frequency*) (initial-phase 0.0) (wave) (size clm-table-size) (type)): \
return a new " S_table_lookup " generator.  This is known as an oscillator in other synthesis systems. \
The default table size is 512; use :size to set some other size, or pass your own vct as the 'wave'.\n\
   (set! gen (" S_make_table_lookup " 440.0 :wave (" S_partials_to_wave " '(1 1.0)))\n\
is the same in effect as " S_make_oscil ".  'type' sets the interpolation choice which defaults to " S_mus_interp_linear "."

  mus_any *ge;
  int vals, i, arglist_len;
  mus_long_t table_size = clm_table_size;
  XEN args[MAX_ARGLIST_LEN]; 
  XEN keys[5];
  int orig_arg[5] = {0, 0, 0, 0, MUS_INTERP_LINEAR};
  mus_float_t freq, phase = 0.0;
  mus_float_t *table = NULL;
  vct *v = NULL;
  XEN orig_v = XEN_FALSE;
  int interp_type = (int)MUS_INTERP_LINEAR;

  freq = clm_default_frequency;

  keys[0] = kw_frequency;
  keys[1] = kw_initial_phase;
  keys[2] = kw_wave;
  keys[3] = kw_size;
  keys[4] = kw_type;

  arglist_len = XEN_LIST_LENGTH(arglist);
  if (arglist_len > MAX_ARGLIST_LEN)
    clm_error(S_make_table_lookup, "too many args!", arglist);

  for (i = 0; i < arglist_len; i++) args[i] = XEN_LIST_REF(arglist, i);
  for (i = arglist_len; i < MAX_ARGLIST_LEN; i++) args[i] = XEN_UNDEFINED;

  vals = mus_optkey_unscramble(S_make_table_lookup, 5, keys, args, orig_arg);
  if (vals > 0)
    {
      freq = mus_optkey_to_float(keys[0], S_make_table_lookup, orig_arg[0], freq);
      if (freq > (0.5 * mus_srate()))
	XEN_OUT_OF_RANGE_ERROR(S_make_table_lookup, orig_arg[0], keys[0], "freq ~A > srate/2?");

      phase = mus_optkey_to_float(keys[1], S_make_table_lookup, orig_arg[1], phase);
      if (phase < 0.0)
	XEN_OUT_OF_RANGE_ERROR(S_make_table_lookup, orig_arg[1], keys[1], "phase ~A?");

      v = mus_optkey_to_vct(keys[2], S_make_table_lookup, orig_arg[2], NULL);
      if (v) 
	{
	  orig_v = keys[2];
	  table = v->data;
	  table_size = v->length;
	}

      table_size = mus_optkey_to_mus_long_t(keys[3], S_make_table_lookup, orig_arg[3], table_size);
      if (table_size <= 0)
	XEN_OUT_OF_RANGE_ERROR(S_make_table_lookup, orig_arg[3], keys[3], "size ~A <= 0?");
      if (table_size > mus_max_table_size())
	XEN_OUT_OF_RANGE_ERROR(S_make_table_lookup, orig_arg[3], keys[3], "size ~A too large (see mus-max-table-size)");
      if ((v) && (table_size > v->length))
	XEN_OUT_OF_RANGE_ERROR(S_make_table_lookup, orig_arg[3], keys[3], "size arg ~A bigger than size of provided wave");

      interp_type = mus_optkey_to_int(keys[4], S_make_table_lookup, orig_arg[4], interp_type);
      if (!(mus_interp_type_p(interp_type)))
	XEN_OUT_OF_RANGE_ERROR(S_make_table_lookup, orig_arg[4], keys[4], "no such interp-type: ~A");
    }

  if (!(MUS_VCT_P(orig_v)))
    {
      table = (mus_float_t *)calloc(table_size, sizeof(mus_float_t));
      if (table == NULL)
	return(clm_mus_error(MUS_MEMORY_ALLOCATION_FAILED, "can't allocate table-lookup table"));
      orig_v = xen_make_vct(table_size, table);
    }
  ge = mus_make_table_lookup(freq, phase, table, table_size, (mus_interp_t)interp_type);
  return(mus_xen_to_object(mus_any_to_mus_xen_with_vct(ge, orig_v)));
}


static XEN g_table_lookup(XEN obj, XEN fm) 
{
  #define H_table_lookup "(" S_table_lookup " gen (fm 0.0)): interpolated table-lookup \
with 'wrap-around' when gen's phase marches off either end of its table."

  mus_float_t fm1 = 0.0;
  mus_any *g = NULL;

  XEN_TO_C_GENERATOR(obj, g, mus_table_lookup_p, S_table_lookup, "a table-lookup generator");
  XEN_TO_C_DOUBLE_IF_BOUND(fm, fm1, S_table_lookup, XEN_ARG_2);

  return(C_TO_XEN_DOUBLE(mus_table_lookup(g, fm1)));
}



/* ---------------- sawtooth et al ---------------- */

typedef enum {G_SAWTOOTH_WAVE, G_SQUARE_WAVE, G_TRIANGLE_WAVE, G_PULSE_TRAIN} xclm_wave_t;

static XEN g_make_sw(xclm_wave_t type, mus_float_t def_phase, XEN arg1, XEN arg2, XEN arg3, XEN arg4, XEN arg5, XEN arg6)
{
  mus_any *ge = NULL;
  const char *caller = NULL;
  XEN args[6]; 
  XEN keys[3];
  int orig_arg[3] = {0, 0, 0};
  int vals;
  mus_float_t freq, base = 1.0, phase;

  freq = clm_default_frequency;
  phase = def_phase;

  switch (type)
    {
      case G_SAWTOOTH_WAVE: caller = S_make_sawtooth_wave; break;
      case G_SQUARE_WAVE:   caller = S_make_square_wave;   break;
      case G_TRIANGLE_WAVE: caller = S_make_triangle_wave; break;
      case G_PULSE_TRAIN:   caller = S_make_pulse_train;   break;
    }

  keys[0] = kw_frequency;
  keys[1] = kw_amplitude;
  keys[2] = kw_initial_phase;
  args[0] = arg1; args[1] = arg2; args[2] = arg3; args[3] = arg4; args[4] = arg5; args[5] = arg6; 

  vals = mus_optkey_unscramble(caller, 3, keys, args, orig_arg);
  if (vals > 0)
    {
      freq = mus_optkey_to_float(keys[0], caller, orig_arg[0], freq);
      if (freq > mus_srate())
	XEN_OUT_OF_RANGE_ERROR(caller, orig_arg[0], keys[0], "freq ~A > srate/2?");

      base = mus_optkey_to_float(keys[1], caller, orig_arg[1], base);

      phase = mus_optkey_to_float(keys[2], caller, orig_arg[2], phase);
    }

  switch (type)
    {
    case G_SAWTOOTH_WAVE: ge = mus_make_sawtooth_wave(freq, base, phase); break;
    case G_SQUARE_WAVE:   ge = mus_make_square_wave(freq, base, phase);   break;
    case G_TRIANGLE_WAVE: ge = mus_make_triangle_wave(freq, base, phase); break;
    case G_PULSE_TRAIN:   ge = mus_make_pulse_train(freq, base, phase);   break;
    }
  if (ge) return(mus_xen_to_object(mus_any_to_mus_xen(ge)));
  return(XEN_FALSE);
}


static XEN g_make_sawtooth_wave(XEN arg1, XEN arg2, XEN arg3, XEN arg4, XEN arg5, XEN arg6) 
{
  #define H_make_sawtooth_wave "(" S_make_sawtooth_wave " (frequency *clm-default-frequency*) (amplitude 1.0) (initial-phase 0.0)): \
return a new " S_sawtooth_wave " generator."

  return(g_make_sw(G_SAWTOOTH_WAVE, M_PI, arg1, arg2, arg3, arg4, arg5, arg6));
}


static XEN g_make_square_wave(XEN arg1, XEN arg2, XEN arg3, XEN arg4, XEN arg5, XEN arg6) 
{
  #define H_make_square_wave "(" S_make_square_wave " (frequency *clm-default-frequency*) (amplitude 1.0) (initial-phase 0.0)): \
return a new " S_square_wave " generator."

  return(g_make_sw(G_SQUARE_WAVE, 0.0, arg1, arg2, arg3, arg4, arg5, arg6));
}


static XEN g_make_triangle_wave(XEN arg1, XEN arg2, XEN arg3, XEN arg4, XEN arg5, XEN arg6) 
{
  #define H_make_triangle_wave "(" S_make_triangle_wave " (frequency *clm-default-frequency*) (amplitude 1.0) (initial-phase 0.0)): \
return a new " S_triangle_wave " generator."

  return(g_make_sw(G_TRIANGLE_WAVE, 0.0, arg1, arg2, arg3, arg4, arg5, arg6));
}


static XEN g_make_pulse_train(XEN arg1, XEN arg2, XEN arg3, XEN arg4, XEN arg5, XEN arg6) 
{
  #define H_make_pulse_train "(" S_make_pulse_train " (frequency *clm-default-frequency*) (amplitude 1.0) (initial-phase 0.0)): \
return a new " S_pulse_train " generator.  This produces a sequence of impulses."

  return(g_make_sw(G_PULSE_TRAIN, TWO_PI, arg1, arg2, arg3, arg4, arg5, arg6));
}


static XEN g_sawtooth_wave(XEN obj, XEN fm) 
{
  #define H_sawtooth_wave "(" S_sawtooth_wave " gen (fm 0.0)): next sawtooth sample from generator"
  mus_float_t fm1 = 0.0;
  mus_any *g = NULL;

  XEN_TO_C_GENERATOR(obj, g, mus_sawtooth_wave_p, S_sawtooth_wave, "a sawtooth-wave generator");
  XEN_TO_C_DOUBLE_IF_BOUND(fm, fm1, S_sawtooth_wave, XEN_ARG_2);

  return(C_TO_XEN_DOUBLE(mus_sawtooth_wave(g, fm1)));
}


static XEN g_square_wave(XEN obj, XEN fm) 
{
  #define H_square_wave "(" S_square_wave " gen (fm 0.0)): next square wave sample from generator"
  mus_float_t fm1 = 0.0;
  mus_any *g = NULL;

  XEN_TO_C_GENERATOR(obj, g, mus_square_wave_p, S_square_wave, "a square-wave generator");
  XEN_TO_C_DOUBLE_IF_BOUND(fm, fm1, S_square_wave, XEN_ARG_2);

  return(C_TO_XEN_DOUBLE(mus_square_wave(g, fm1)));
}


static XEN g_triangle_wave(XEN obj, XEN fm) 
{
  #define H_triangle_wave "(" S_triangle_wave " gen (fm 0.0)): next triangle wave sample from generator"
  mus_float_t fm1 = 0.0;
  mus_any *g = NULL;

  XEN_TO_C_GENERATOR(obj, g, mus_triangle_wave_p, S_triangle_wave, "a triangle-wave generator");
  XEN_TO_C_DOUBLE_IF_BOUND(fm, fm1, S_triangle_wave, XEN_ARG_2);

  return(C_TO_XEN_DOUBLE(mus_triangle_wave(g, fm1)));
}


static XEN g_pulse_train(XEN obj, XEN fm) 
{
  #define H_pulse_train "(" S_pulse_train " gen (fm 0.0)): next pulse train sample from generator"
  mus_float_t fm1 = 0.0;
  mus_any *g = NULL;

  XEN_TO_C_GENERATOR(obj, g, mus_pulse_train_p, S_pulse_train, "a pulse-train generator");
  XEN_TO_C_DOUBLE_IF_BOUND(fm, fm1, S_pulse_train, XEN_ARG_2);

  return(C_TO_XEN_DOUBLE(mus_pulse_train(g, fm1)));
}


static XEN g_sawtooth_wave_p(XEN obj) 
{
  #define H_sawtooth_wave_p "(" S_sawtooth_wave_p " gen): " PROC_TRUE " if gen is a " S_sawtooth_wave
  return(C_TO_XEN_BOOLEAN((MUS_XEN_P(obj)) && (mus_sawtooth_wave_p(XEN_TO_MUS_ANY(obj)))));
}


static XEN g_square_wave_p(XEN obj) 
{
  #define H_square_wave_p "(" S_square_wave_p " gen): " PROC_TRUE " if gen is a " S_square_wave
  return(C_TO_XEN_BOOLEAN((MUS_XEN_P(obj)) && (mus_square_wave_p(XEN_TO_MUS_ANY(obj)))));
}


static XEN g_triangle_wave_p(XEN obj) 
{
  #define H_triangle_wave_p "(" S_triangle_wave_p " gen): " PROC_TRUE " if gen is a " S_triangle_wave
  return(C_TO_XEN_BOOLEAN((MUS_XEN_P(obj)) && (mus_triangle_wave_p(XEN_TO_MUS_ANY(obj)))));
}


static XEN g_pulse_train_p(XEN obj) 
{
  #define H_pulse_train_p "(" S_pulse_train_p " gen): " PROC_TRUE " if gen is a " S_pulse_train
  return(C_TO_XEN_BOOLEAN((MUS_XEN_P(obj)) && (mus_pulse_train_p(XEN_TO_MUS_ANY(obj)))));
}



/* ---------------- asymmetric-fm ---------------- */

static XEN g_make_asymmetric_fm(XEN arg1, XEN arg2, XEN arg3, XEN arg4, XEN arg5, XEN arg6, XEN arg7, XEN arg8)
{
  #define H_make_asymmetric_fm "(" S_make_asymmetric_fm " (frequency *clm-default-frequency*) (initial-phase 0.0) (r 1.0) (ratio 1.0)): \
return a new " S_asymmetric_fm " generator."

  mus_any *ge;
  XEN args[8]; 
  XEN keys[4];
  int orig_arg[4] = {0, 0, 0, 0};
  int vals;
  mus_float_t freq, phase = 0.0, r = 1.0, ratio = 1.0;

  freq = clm_default_frequency;

  keys[0] = kw_frequency;
  keys[1] = kw_initial_phase;
  keys[2] = kw_r;
  keys[3] = kw_ratio;
  args[0] = arg1; args[1] = arg2; args[2] = arg3; args[3] = arg4; args[4] = arg5; args[5] = arg6; args[6] = arg7; args[7] = arg8; 

  vals = mus_optkey_unscramble(S_make_asymmetric_fm, 4, keys, args, orig_arg);
  if (vals > 0)
    {
      freq = mus_optkey_to_float(keys[0], S_make_asymmetric_fm, orig_arg[0], freq);
      if (freq > (0.5 * mus_srate()))
	XEN_OUT_OF_RANGE_ERROR(S_make_asymmetric_fm, orig_arg[0], keys[0], "freq ~A > srate/2?");

      phase = mus_optkey_to_float(keys[1], S_make_asymmetric_fm, orig_arg[1], phase);

      r = mus_optkey_to_float(keys[2], S_make_asymmetric_fm, orig_arg[2], r);

      ratio = mus_optkey_to_float(keys[3], S_make_asymmetric_fm, orig_arg[3], ratio);
    }

  ge = mus_make_asymmetric_fm(freq, phase, r, ratio);
  if (ge) return(mus_xen_to_object(mus_any_to_mus_xen(ge)));
  return(XEN_FALSE);
}



static XEN g_asymmetric_fm(XEN obj, XEN index, XEN fm)
{
  #define H_asymmetric_fm "(" S_asymmetric_fm " gen (index 0.0) (fm 0.0)): next sample from asymmetric fm generator"
  mus_float_t fm1 = 0.0, index1 = 0.0;
  mus_any *g = NULL;

  XEN_TO_C_GENERATOR(obj, g, mus_asymmetric_fm_p, S_asymmetric_fm, "an asymmetric-fm generator");
  XEN_TO_C_DOUBLE_IF_BOUND(fm, fm1, S_asymmetric_fm, XEN_ARG_2);
  XEN_TO_C_DOUBLE_IF_BOUND(index, index1, S_asymmetric_fm, XEN_ARG_3);

  return(C_TO_XEN_DOUBLE(mus_asymmetric_fm(g, index1, fm1)));
}


static XEN g_asymmetric_fm_p(XEN obj) 
{
  #define H_asymmetric_fm_p "(" S_asymmetric_fm_p " gen): " PROC_TRUE " if gen is a " S_asymmetric_fm
  return(C_TO_XEN_BOOLEAN((MUS_XEN_P(obj)) && (mus_asymmetric_fm_p(XEN_TO_MUS_ANY(obj)))));
}



/* ---------------- simple filters ---------------- */

typedef enum {G_ONE_POLE, G_ONE_ZERO, G_TWO_POLE, G_TWO_ZERO} xclm_filter_t;

static const char *smpflts[6] = {S_make_one_pole, S_make_one_zero, S_make_two_pole, S_make_two_zero};


static XEN g_make_smpflt_1(xclm_filter_t choice, XEN arg1, XEN arg2, XEN arg3, XEN arg4)
{
  mus_any *gen = NULL;
  XEN args[4]; 
  XEN keys[2];
  int orig_arg[2] = {0, 0};
  int vals;
  mus_float_t a0 = 0.0;
  mus_float_t a1 = 0.0;

  switch (choice)
    {
    case G_ONE_ZERO: keys[0] = kw_a0;        keys[1] = kw_a1;     break;
    case G_ONE_POLE: keys[0] = kw_a0;        keys[1] = kw_b1;     break;
    default:         keys[0] = kw_frequency; keys[1] = kw_radius; break;
    }

  args[0] = arg1; args[1] = arg2; args[2] = arg3; args[3] = arg4;
  vals = mus_optkey_unscramble(smpflts[choice], 2, keys, args, orig_arg);
  if (vals > 0)
    {
      a0 = mus_optkey_to_float(keys[0], smpflts[choice], orig_arg[0], a0);
      a1 = mus_optkey_to_float(keys[1], smpflts[choice], orig_arg[1], a1);
    }

  switch (choice)
    {
    case G_ONE_ZERO: gen = mus_make_one_zero(a0, a1); break;
    case G_ONE_POLE: gen = mus_make_one_pole(a0, a1); break;
    case G_TWO_ZERO: gen = mus_make_two_zero_from_frequency_and_radius(a0, a1); break;
    case G_TWO_POLE: gen = mus_make_two_pole_from_frequency_and_radius(a0, a1); break;
    default: break;
    }
  if (gen) return(mus_xen_to_object(mus_any_to_mus_xen(gen)));
  return(XEN_FALSE);
}


static XEN g_make_one_zero(XEN arg1, XEN arg2, XEN arg3, XEN arg4)
{
  #define H_make_one_zero "(" S_make_one_zero " a0 a1): return a new " S_one_zero " filter;  a0*x(n) + a1*x(n-1)"
  return(g_make_smpflt_1(G_ONE_ZERO, arg1, arg2, arg3, arg4));
}


static XEN g_make_one_pole(XEN arg1, XEN arg2, XEN arg3, XEN arg4)
{
  #define H_make_one_pole "(" S_make_one_pole " a0 b1): return a new " S_one_pole " filter; a0*x(n) - b1*y(n-1)"
  return(g_make_smpflt_1(G_ONE_POLE, arg1, arg2, arg3, arg4));
}


static XEN g_make_smpflt_2(xclm_filter_t choice, XEN arg1, XEN arg2, XEN arg3, XEN arg4, XEN arg5, XEN arg6)
{
  mus_any *gen = NULL;
  XEN args[6]; 
  XEN keys[3];
  int orig_arg[3] = {0, 0, 0};
  int vals;
  mus_float_t a0 = 0.0;
  mus_float_t a1 = 0.0;
  mus_float_t a2 = 0.0;
  if (choice == G_TWO_ZERO)
    {
      keys[0] = kw_a0;
      keys[1] = kw_a1;
      keys[2] = kw_a2;
    }
  else
    {
      keys[0] = kw_a0;
      keys[1] = kw_b1;
      keys[2] = kw_b2;
    }
  args[0] = arg1; args[1] = arg2; args[2] = arg3; args[3] = arg4; args[4] = arg5; args[5] = arg6;
  vals = mus_optkey_unscramble(smpflts[choice], 3, keys, args, orig_arg);
  if (vals > 0)
    {
      a0 = mus_optkey_to_float(keys[0], smpflts[choice], orig_arg[0], a0);
      a1 = mus_optkey_to_float(keys[1], smpflts[choice], orig_arg[1], a1);
      a2 = mus_optkey_to_float(keys[2], smpflts[choice], orig_arg[2], a2);
    }
  if (choice == G_TWO_ZERO)
    gen = mus_make_two_zero(a0, a1, a2);
  else gen = mus_make_two_pole(a0, a1, a2);
  if (gen) return(mus_xen_to_object(mus_any_to_mus_xen(gen)));
  return(XEN_FALSE);
}


static bool found_polar_key(XEN arg)
{
  return((XEN_KEYWORD_P(arg)) && 
	 ((XEN_KEYWORD_EQ_P(arg, kw_radius)) ||
	  (XEN_KEYWORD_EQ_P(arg, kw_frequency))));
}


static bool found_coeff_key(XEN arg)
{
  return((XEN_KEYWORD_P(arg)) && 
	 (!(XEN_KEYWORD_EQ_P(arg, kw_radius))) &&
	 (!(XEN_KEYWORD_EQ_P(arg, kw_frequency))));
}


static XEN g_make_two_zero(XEN arg1, XEN arg2, XEN arg3, XEN arg4, XEN arg5, XEN arg6) 
{
  #define H_make_two_zero "(" S_make_two_zero " a0 a1 a2) or (" S_make_two_zero " frequency radius): return a new " S_two_zero " filter; \
a0*x(n) + a1*x(n-1) + a2*x(n-2)"

  if ((XEN_BOUND_P(arg2)) && /* 0 or 1 args -> coeffs */
      (!(XEN_BOUND_P(arg5)))) /* 5 or more args -> coeffs */
    {
      if ((found_polar_key(arg1)) || 
	  (found_polar_key(arg2)) ||    /* if arg1 is frequency as number, then arg2 is either key or number */
	  ((!(XEN_BOUND_P(arg3))) &&    /* make a guess that if 2 args, no keys, and a0 > 20, it is intended as a frequency */
	   (!(found_coeff_key(arg1))) &&
	   ((XEN_NUMBER_P(arg1)) && (XEN_TO_C_DOUBLE(arg1) >= 20.0))))
	return(g_make_smpflt_1(G_TWO_ZERO, arg1, arg2, arg3, arg4));
    }

  return(g_make_smpflt_2(G_TWO_ZERO, arg1, arg2, arg3, arg4, arg5, arg6));
}


static XEN g_make_two_pole(XEN arg1, XEN arg2, XEN arg3, XEN arg4, XEN arg5, XEN arg6) 
{
  #define H_make_two_pole "(" S_make_two_pole " a0 b1 b2) or (" S_make_two_pole " frequency radius): return a new " S_two_pole " filter; \
a0*x(n) - b1*y(n-1) - b2*y(n-2)"

  if ((XEN_BOUND_P(arg2)) && /* 0 or 1 args -> coeffs */
      (!(XEN_BOUND_P(arg5)))) /* 5 or more args -> coeffs */
    {
      if ((found_polar_key(arg1)) || 
	  (found_polar_key(arg2)) ||    /* if arg1 is frequency as number, then arg2 is either key or number */
	  ((!(XEN_BOUND_P(arg3))) &&
	   (!(found_coeff_key(arg1))) &&
	   ((XEN_NUMBER_P(arg1)) && (XEN_TO_C_DOUBLE(arg1) >= 2.0))))
	return(g_make_smpflt_1(G_TWO_POLE, arg1, arg2, arg3, arg4));
    }

  return(g_make_smpflt_2(G_TWO_POLE, arg1, arg2, arg3, arg4, arg5, arg6));
}


static XEN g_one_zero(XEN obj, XEN fm)
{
  #define H_one_zero "(" S_one_zero " gen (input 0.0)): one zero filter of input"
  mus_float_t fm1 = 0.0;
  mus_any *g = NULL;

  XEN_TO_C_GENERATOR(obj, g, mus_one_zero_p, S_one_zero, "a one-zero filter");
  XEN_TO_C_DOUBLE_IF_BOUND(fm, fm1, S_one_zero, XEN_ARG_2);

  return(C_TO_XEN_DOUBLE(mus_one_zero(g, fm1)));
}


static XEN g_one_pole(XEN obj, XEN fm)
{
  #define H_one_pole "(" S_one_pole " gen (input 0.0)): one pole filter of input"
  mus_float_t fm1 = 0.0;
  mus_any *g = NULL;

  XEN_TO_C_GENERATOR(obj, g, mus_one_pole_p, S_one_pole, "a one-pole filter");
  XEN_TO_C_DOUBLE_IF_BOUND(fm, fm1, S_one_pole, XEN_ARG_2);

  return(C_TO_XEN_DOUBLE(mus_one_pole(g, fm1)));
}


static XEN g_two_zero(XEN obj, XEN fm)
{
  #define H_two_zero "(" S_two_zero " gen (input 0.0)): two zero filter of input"
  mus_float_t fm1 = 0.0;
  mus_any *g = NULL;

  XEN_TO_C_GENERATOR(obj, g, mus_two_zero_p, S_two_zero, "a two-zero filter");
  XEN_TO_C_DOUBLE_IF_BOUND(fm, fm1, S_two_zero, XEN_ARG_2);

  return(C_TO_XEN_DOUBLE(mus_two_zero(g, fm1)));
}


static XEN g_two_pole(XEN obj, XEN fm)
{
  #define H_two_pole "(" S_two_pole " gen (input 0.0)): two pole filter of input"
  mus_float_t fm1 = 0.0;
  mus_any *g = NULL;

  XEN_TO_C_GENERATOR(obj, g, mus_two_pole_p, S_two_pole, "a two-pole filter");
  XEN_TO_C_DOUBLE_IF_BOUND(fm, fm1, S_two_pole, XEN_ARG_2);

  return(C_TO_XEN_DOUBLE(mus_two_pole(g, fm1)));
}


static XEN g_one_zero_p(XEN obj) 
{
  #define H_one_zero_p "(" S_one_zero_p " gen): " PROC_TRUE " if gen is a " S_one_zero
  return(C_TO_XEN_BOOLEAN((MUS_XEN_P(obj)) && (mus_one_zero_p(XEN_TO_MUS_ANY(obj)))));
}


static XEN g_one_pole_p(XEN obj) 
{
  #define H_one_pole_p "(" S_one_pole_p " gen): " PROC_TRUE " if gen is a " S_one_pole
  return(C_TO_XEN_BOOLEAN((MUS_XEN_P(obj)) && (mus_one_pole_p(XEN_TO_MUS_ANY(obj)))));
}


static XEN g_two_zero_p(XEN obj) 
{
  #define H_two_zero_p "(" S_two_zero_p " gen): " PROC_TRUE " if gen is a " S_two_zero
  return(C_TO_XEN_BOOLEAN((MUS_XEN_P(obj)) && (mus_two_zero_p(XEN_TO_MUS_ANY(obj)))));
}


static XEN g_two_pole_p(XEN obj) 
{
  #define H_two_pole_p "(" S_two_pole_p " gen): " PROC_TRUE " if gen is a " S_two_pole
  return(C_TO_XEN_BOOLEAN((MUS_XEN_P(obj)) && (mus_two_pole_p(XEN_TO_MUS_ANY(obj)))));
}




/* ---------------- formant ---------------- */

static XEN g_make_frm(bool formant_case, const char *caller, XEN arg1, XEN arg2, XEN arg3, XEN arg4)
{
  mus_any *ge;
  int vals;
  XEN args[4]; 
  XEN keys[2];
  int orig_arg[2] = {0, 0};
  mus_float_t freq = 0.0, radius = 0.0;

  keys[0] = kw_frequency;
  keys[1] = kw_radius;
  args[0] = arg1; args[1] = arg2; args[2] = arg3; args[3] = arg4;

  vals = mus_optkey_unscramble(caller, 2, keys, args, orig_arg);
  if (vals > 0)
    {
      freq = mus_optkey_to_float(keys[0], caller, orig_arg[0], freq);
      if (freq > (0.5 * mus_srate()))
	XEN_OUT_OF_RANGE_ERROR(caller, orig_arg[0], keys[0], "freq ~A > srate/2?");

      radius = mus_optkey_to_float(keys[1], caller, orig_arg[1], radius);
    }

  if (formant_case)
    ge = mus_make_formant(freq, radius);
  else ge = mus_make_firmant(freq, radius);
  if (ge) return(mus_xen_to_object(mus_any_to_mus_xen(ge)));
  return(XEN_FALSE);
}


static XEN g_formant(XEN gen, XEN input, XEN freq)
{
  #define H_formant "(" S_formant " gen (input 0.0) freq-in-radians): next sample from resonator generator"
  mus_float_t in1 = 0.0;
  mus_any *g = NULL;
  XEN_TO_C_GENERATOR(gen, g, mus_formant_p, S_formant, "a formant generator");

  XEN_TO_C_DOUBLE_IF_BOUND(input, in1, S_formant, XEN_ARG_2);
  if (XEN_BOUND_P(freq))
    return(C_TO_XEN_DOUBLE(mus_formant_with_frequency(g, in1, XEN_TO_C_DOUBLE(freq))));

  return(C_TO_XEN_DOUBLE(mus_formant(g, in1)));
}


static XEN g_formant_bank(XEN amps, XEN gens, XEN inp)
{
  #define H_formant_bank "(" S_formant_bank " scls gens inval): sum a bank of " S_formant "s: scls[i]*" S_formant "(gens[i], inval)"
  mus_float_t outval = 0.0, inval;
  int i, size;
  vct *scl;
#if HAVE_SCHEME
  s7_pointer *elements;
#endif

  XEN_ASSERT_TYPE(XEN_VECTOR_P(gens), gens, XEN_ARG_2, S_formant_bank, "a vector of formant generators");
  XEN_ASSERT_TYPE(MUS_VCT_P(amps), amps, XEN_ARG_1, S_formant_bank, "a vct");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(inp), inp, XEN_ARG_3, S_formant_bank, "a number");

  size = XEN_VECTOR_LENGTH(gens);
  if (size == 0) return(XEN_ZERO);
  scl = XEN_TO_VCT(amps);
  inval = XEN_TO_C_DOUBLE(inp);
#if HAVE_SCHEME
  elements = s7_vector_elements(gens);
#endif

  for (i = 0; i < size; i++)
    {
      XEN g;
#if HAVE_SCHEME
      g = elements[i];
#else
      g = XEN_VECTOR_REF(gens, i);
#endif
      if (MUS_XEN_P(g))
	{
	  mus_any *fg;
	  fg = XEN_TO_MUS_ANY(g);
	  if (mus_formant_p(fg))
	    outval += (scl->data[i] * mus_formant(fg, inval));
	  else XEN_WRONG_TYPE_ARG_ERROR(S_formant_bank, i, g, "a formant generator");
	}
      else XEN_WRONG_TYPE_ARG_ERROR(S_formant_bank, i, g, "a formant generator");
    }
  return(C_TO_XEN_DOUBLE(outval));
}


static XEN g_make_formant(XEN arg1, XEN arg2, XEN arg3, XEN arg4)
{
  #define H_make_formant "(" S_make_formant " frequency radius): \
return a new formant generator (a resonator).  radius sets the pole radius (in terms of the 'unit circle'). \
frequency sets the resonance center frequency (Hz)."

  return(g_make_frm(true, S_make_formant, arg1, arg2, arg3, arg4));
}


static XEN g_formant_p(XEN os) 
{
  #define H_formant_p "(" S_formant_p " gen): " PROC_TRUE " if gen is a " S_formant
  return(C_TO_XEN_BOOLEAN((MUS_XEN_P(os)) && (mus_formant_p(XEN_TO_MUS_ANY(os)))));
}


static XEN g_set_formant_radius_and_frequency(XEN gen, XEN rad, XEN frq)
{
  #define H_mus_set_formant_radius_and_frequency  "(" S_mus_set_formant_radius_and_frequency  " gen radius frequency): set (" S_formant " \
generator) gen's radius and frequency"
  mus_any *g = NULL;
  mus_float_t radius = 0.0, frequency = 0.0;

  XEN_TO_C_GENERATOR(gen, g, mus_formant_p, S_mus_set_formant_radius_and_frequency, "a formant generator");
  XEN_TO_C_DOUBLE_OR_ERROR(rad, radius, S_mus_set_formant_radius_and_frequency, XEN_ARG_2);
  XEN_TO_C_DOUBLE_OR_ERROR(frq, frequency, S_mus_set_formant_radius_and_frequency, XEN_ARG_3);
  mus_set_formant_radius_and_frequency(g, radius, frequency);
  return(rad);
}


/* ---------------- firmant ---------------- */

static XEN g_make_firmant(XEN arg1, XEN arg2, XEN arg3, XEN arg4)
{
  #define H_make_firmant "(" S_make_firmant " frequency radius): \
return a new firmant generator (a resonator).  radius sets the pole radius (in terms of the 'unit circle'). \
frequency sets the resonance center frequency (Hz)."

  return(g_make_frm(false, S_make_firmant, arg1, arg2, arg3, arg4));
}


static XEN g_firmant_p(XEN os) 
{
  #define H_firmant_p "(" S_firmant_p " gen): " PROC_TRUE " if gen is a " S_firmant " generator"
  return(C_TO_XEN_BOOLEAN((MUS_XEN_P(os)) && (mus_firmant_p(XEN_TO_MUS_ANY(os)))));
}


static XEN g_firmant(XEN gen, XEN input, XEN freq)
{
  #define H_firmant "(" S_firmant " gen (input 0.0) freq-in-radians): next sample from resonator generator"
  mus_float_t in1 = 0.0;
  mus_any *g = NULL;

  XEN_TO_C_GENERATOR(gen, g, mus_firmant_p, S_firmant, "a firmant generator");
  XEN_TO_C_DOUBLE_IF_BOUND(input, in1, S_firmant, XEN_ARG_2);
  if (XEN_BOUND_P(freq)) 
    return(C_TO_XEN_DOUBLE(mus_firmant_with_frequency(g, in1, XEN_TO_C_DOUBLE(freq))));

  return(C_TO_XEN_DOUBLE(mus_firmant(g, in1)));
}



/* ---------------- frame ---------------- */

#define MUS_MAX_CHANS 128

/* this needs to be a reasonably small number -- user can override the size check using "!" as in make-mixer!
 */

static XEN g_make_frame_2(int len, XEN args)
{
  mus_any *ge;
  ge = (mus_any *)mus_make_empty_frame((len == 0) ? 1 : len);
  if (ge)
    {
      if (len > 0)
	{
	  mus_float_t *vals;
	  XEN lst;
	  int i;
	  vals = mus_data(ge);
	  for (i = 0, lst = XEN_COPY_ARG(args); (i < len) && (XEN_NOT_NULL_P(lst)); i++, lst = XEN_CDR(lst))
	    if (XEN_NUMBER_P(XEN_CAR(lst)))
	      vals[i] = XEN_TO_C_DOUBLE(XEN_CAR(lst));
	    else
	      {
		mus_free(ge);
		XEN_WRONG_TYPE_ARG_ERROR(S_make_frame, i, XEN_CAR(lst), "a number");
	      }
	}
#if (!HAVE_SCHEME)
      return(mus_xen_to_object(mus_any_to_mus_xen_with_vct(ge, xen_make_vct_wrapper(mus_length(ge), mus_data(ge)))));
#else
      {
	s7_pointer nv;
	nv = mus_xen_to_object(mus_any_to_mus_xen_with_vct(ge, xen_make_vct_wrapper(mus_length(ge), mus_data(ge))));
#if 0
	s7_object_set_environment(nv, g_frame_methods);
	s7_open_environment(nv);
#endif
	return(nv);
      }
#endif
    }
  return(XEN_FALSE);
}


static XEN g_make_frame_1(XEN arglist, bool check_size)
{
  #if HAVE_SCHEME
    #define make_frame_example "(" S_make_frame " 2 .1 .2)"
  #endif
  #if HAVE_RUBY
    #define make_frame_example "make_frame(2, 0.1, 0.2)"
  #endif
  #if HAVE_FORTH
    #define make_frame_example "2 0.1 0.2 make-frame"
  #endif

  #define H_make_frame "(" S_make_frame " chans val0 val1 ...): return a new frame object \
with chans samples, each sample set from the trailing arguments (defaulting to 0.0):\n  " make_frame_example

  /* make_empty_frame from first of arglist, then if more args, load vals */
  int len = 0, size = 0;

  XEN_ASSERT_TYPE((XEN_LIST_P_WITH_LENGTH(arglist, len)) && (len >= 0), arglist, XEN_ARG_1, S_make_frame, "a list");

  if (len > 0)
    {
      XEN cararg;
      cararg = XEN_CAR(arglist);
      XEN_ASSERT_TYPE(XEN_NUMBER_P(cararg), cararg, XEN_ARG_1, S_make_frame, "a number");
      size = XEN_TO_C_INT_OR_ELSE(cararg, 0);
      if (size <= 0) 
	XEN_OUT_OF_RANGE_ERROR(S_make_frame, XEN_ARG_1, cararg, "chans ~A <= 0?");
      if (len > (size + 1)) 
	clm_error(S_make_frame, "extra trailing args?", arglist);
      if ((check_size) && 
	  (size > MUS_MAX_CHANS))
	XEN_OUT_OF_RANGE_ERROR(S_make_frame, XEN_ARG_1, C_TO_XEN_INT(size), "size ~A too big");
    }

  return(g_make_frame_2(size, (len <= 1) ? XEN_EMPTY_LIST : XEN_CDR(arglist)));
}


static XEN g_make_frame(XEN arglist)
{
  return(g_make_frame_1(arglist, true));
}


static XEN g_make_frame_unchecked(XEN arglist)
{
  #define H_make_frame_unchecked "(make-frame! chans val0 val1 ...): return a new frame object \
with chans samples, each sample set from the trailing arguments (defaulting to 0.0).  Unlike make-frame, \
make-frame! ignores mus-max-malloc and tries to create a frame of any size."
  return(g_make_frame_1(arglist, false));
}


static XEN g_frame(XEN args) 
{
  #define H_frame "(" S_frame " num ...): returns a new frame with args as its contents: (frame .1 .2)"
  return(g_make_frame_2(XEN_LIST_LENGTH(args), args));
}




static XEN g_frame_p(XEN obj) 
{
  #define H_frame_p "(" S_frame_p " gen): " PROC_TRUE " if gen is a frame"
  return(C_TO_XEN_BOOLEAN((MUS_XEN_P(obj)) && (mus_frame_p(XEN_TO_MUS_ANY(obj)))));
}


static XEN g_frame_add(XEN uf1, XEN uf2, XEN ures) /* optional res */
{
  #define H_frame_add "(" S_frame_add " f1 f2 outf): add f1 and f2 returning outf; \
if outf is not given, a new frame is created. outf[i] = f1[i] + f2[i].  Either f1 or f2 can be a float."

  mus_any *res = NULL, *nf = NULL;

  if ((MUS_XEN_P(ures)) && 
      (mus_frame_p(XEN_TO_MUS_ANY(ures)))) 
    res = (mus_any *)XEN_TO_MUS_ANY(ures);

  if (XEN_NUMBER_P(uf1))
    {
      XEN_ASSERT_TYPE((MUS_XEN_P(uf2)) && (mus_frame_p(XEN_TO_MUS_ANY(uf2))), uf2, XEN_ARG_2, S_frame_add, "a frame");
      nf = mus_frame_offset((mus_any *)XEN_TO_MUS_ANY(uf2), XEN_TO_C_DOUBLE(uf1), res);
    }
  else
    {
      if (XEN_NUMBER_P(uf2))
	{
	  XEN_ASSERT_TYPE((MUS_XEN_P(uf1)) && (mus_frame_p(XEN_TO_MUS_ANY(uf1))), uf1, XEN_ARG_1, S_frame_add, "a frame");
	  nf = mus_frame_offset((mus_any *)XEN_TO_MUS_ANY(uf1), XEN_TO_C_DOUBLE(uf2), res);
	}
    }
  if (!nf)
    {
      XEN_ASSERT_TYPE((MUS_XEN_P(uf1)) && (mus_frame_p(XEN_TO_MUS_ANY(uf1))), uf1, XEN_ARG_1, S_frame_add, "a frame");
      XEN_ASSERT_TYPE((MUS_XEN_P(uf2)) && (mus_frame_p(XEN_TO_MUS_ANY(uf2))), uf2, XEN_ARG_2, S_frame_add, "a frame");
      nf = mus_frame_add((mus_any *)XEN_TO_MUS_ANY(uf1), (mus_any *)XEN_TO_MUS_ANY(uf2), res);
    }

  if (res)
    return(ures);
  return(mus_xen_to_object(mus_any_to_mus_xen_with_vct(nf, xen_make_vct_wrapper(mus_length(nf), mus_data(nf)))));
}


static XEN g_frame_multiply(XEN uf1, XEN uf2, XEN ures) /* optional res */
{
  #define H_frame_multiply "(" S_frame_multiply " f1 f2 outf): multiply f1 and f2 (elementwise) returning outf; \
if outf is not given, a new frame is created. outf[i] = f1[i] * f2[i]."

  mus_any *res = NULL, *nf = NULL;

  if ((MUS_XEN_P(ures)) && 
      (mus_frame_p(XEN_TO_MUS_ANY(ures)))) 
    res = (mus_any *)XEN_TO_MUS_ANY(ures);

  if (XEN_NUMBER_P(uf1))
    {
      XEN_ASSERT_TYPE((MUS_XEN_P(uf2)) && (mus_frame_p(XEN_TO_MUS_ANY(uf2))), uf2, XEN_ARG_2, S_frame_multiply, "a frame");
      nf = mus_frame_scale((mus_any *)XEN_TO_MUS_ANY(uf2), XEN_TO_C_DOUBLE(uf1), res);
    }
  else
    {
      if (XEN_NUMBER_P(uf2))
	{
	  XEN_ASSERT_TYPE((MUS_XEN_P(uf1)) && (mus_frame_p(XEN_TO_MUS_ANY(uf1))), uf1, XEN_ARG_1, S_frame_multiply, "a frame");
	  nf = mus_frame_scale((mus_any *)XEN_TO_MUS_ANY(uf1), XEN_TO_C_DOUBLE(uf2), res);
	}
    }

  if (!nf)
    {
      XEN_ASSERT_TYPE((MUS_XEN_P(uf1)) && (mus_frame_p(XEN_TO_MUS_ANY(uf1))), uf1, XEN_ARG_1, S_frame_multiply, "a frame");
      XEN_ASSERT_TYPE((MUS_XEN_P(uf2)) && (mus_frame_p(XEN_TO_MUS_ANY(uf2))), uf2, XEN_ARG_2, S_frame_multiply, "a frame");
      nf = mus_frame_multiply((mus_any *)XEN_TO_MUS_ANY(uf1), (mus_any *)XEN_TO_MUS_ANY(uf2), res);
    }
  if (res)
    return(ures);
  return(mus_xen_to_object(mus_any_to_mus_xen_with_vct(nf, xen_make_vct_wrapper(mus_length(nf), mus_data(nf)))));
}


static XEN g_frame_ref(XEN uf1, XEN uchan)
{
  #define H_frame_ref "(" S_frame_ref " f chan): f[chan] (the chan-th sample in frame f"
  mus_any *g = NULL;
  int chan = 0;

  XEN_TO_C_GENERATOR(uf1, g, mus_frame_p, S_frame_ref, "a frame");
  XEN_TO_C_INTEGER_OR_ERROR(uchan, chan, S_frame_ref, XEN_ARG_2);
  return(C_TO_XEN_DOUBLE(mus_frame_ref(g, chan)));
}


static XEN g_frame_set(XEN uf1, XEN uchan, XEN val)
{
  #define H_frame_set "(" S_frame_set " f chan val) sets frame f's chan-th sample to val: f[chan] = val"
  mus_float_t x = 0.0;
  mus_any *g = NULL;
  int chan = 0;

  XEN_TO_C_GENERATOR(uf1, g, mus_frame_p, S_frame_set, "a frame");
  XEN_TO_C_INTEGER_OR_ERROR(uchan, chan, S_frame_set, XEN_ARG_2);
  XEN_TO_C_DOUBLE_OR_ERROR(val, x, S_frame_set, XEN_ARG_3);

  return(C_TO_XEN_DOUBLE(mus_frame_set(g, chan, x)));
}



/* ---------------- mixer ---------------- */

static XEN g_mixer_p(XEN obj) 
{
  #define H_mixer_p "(" S_mixer_p " gen): " PROC_TRUE " if gen is a mixer"
  return(C_TO_XEN_BOOLEAN((MUS_XEN_P(obj)) && (mus_mixer_p(XEN_TO_MUS_ANY(obj)))));
}


static XEN g_mixer_ref(XEN uf1, XEN in, XEN out)
{
  #define H_mixer_ref "(" S_mixer_ref " m in out): m[in, out], the mixer coefficient at location (in, out)"
  mus_any *g = NULL;
  int i_chan = 0, o_chan = 0;

  XEN_TO_C_GENERATOR(uf1, g, mus_mixer_p, S_mixer_ref, "a mixer");
  XEN_TO_C_INTEGER_OR_ERROR(in, i_chan, S_mixer_ref, XEN_ARG_2);
  XEN_TO_C_INTEGER_OR_ERROR(out, o_chan, S_mixer_ref, XEN_ARG_3);

  return(C_TO_XEN_DOUBLE(mus_mixer_ref(g, i_chan, o_chan)));
}


static XEN g_mixer_set(XEN uf1, XEN in, XEN out, XEN val)
{
  #define H_mixer_set "(" S_mixer_set " m in out val): set m[in, out] = val"
  mus_any *g = NULL;
  int i_chan = 0, o_chan = 0;
  mus_float_t x = 0.0;

  XEN_TO_C_GENERATOR(uf1, g, mus_mixer_p, S_mixer_set, "a mixer");
  XEN_TO_C_INTEGER_OR_ERROR(in, i_chan, S_mixer_set, XEN_ARG_2);
  XEN_TO_C_INTEGER_OR_ERROR(out, o_chan, S_mixer_set, XEN_ARG_3);
  XEN_TO_C_DOUBLE_OR_ERROR(val, x, S_mixer_set, XEN_ARG_4);

  return(C_TO_XEN_DOUBLE(mus_mixer_set(g, i_chan, o_chan, x)));
}


static XEN g_mixer_multiply(XEN uf1, XEN uf2, XEN ures) /* optional res */
{
  #define H_mixer_multiply "(" S_mixer_multiply " m1 m2 (outm " PROC_FALSE ")): multiply mixers m1 and m2 (a matrix multiply), \
returning the mixer outm, or creating a new mixer if outm is not given.  Either m1 or m2 can be a float, rather than a mixer."

  mus_any *res = NULL, *u1 = NULL, *u2 = NULL, *nm = NULL;
  bool u1_mixer = false, u2_mixer = false;

  if ((MUS_XEN_P(ures)) && 
      (mus_mixer_p(XEN_TO_MUS_ANY(ures))))
    res = (mus_any *)XEN_TO_MUS_ANY(ures);

  if (MUS_XEN_P(uf1))
    {
      u1 = XEN_TO_MUS_ANY(uf1);
      u1_mixer = mus_mixer_p(u1);
      if (!u1_mixer)
	XEN_ASSERT_TYPE(mus_frame_p(u1), uf1, XEN_ARG_1, S_mixer_multiply, "a frame or mixer");
    }
  else XEN_ASSERT_TYPE(XEN_NUMBER_P(uf1), uf1, XEN_ARG_1, S_mixer_multiply, "a number, frame or mixer");

  if (MUS_XEN_P(uf2))
    {
      u2 = XEN_TO_MUS_ANY(uf2);
      u2_mixer = mus_mixer_p(u2);
      if (!u2_mixer)
	XEN_ASSERT_TYPE(mus_frame_p(u2), uf2, XEN_ARG_2, S_mixer_multiply, "a frame or mixer");
    }
  else XEN_ASSERT_TYPE(XEN_NUMBER_P(uf2), uf2, XEN_ARG_2, S_mixer_multiply, "a number, frame or mixer");

  XEN_ASSERT_TYPE(u1_mixer || u2_mixer, uf1, XEN_ARG_1, S_mixer_multiply, "one arg must be a mixer");
  if (!u1)
    {
      XEN_ASSERT_TYPE(u2_mixer, uf2, XEN_ARG_2, S_mixer_multiply, "a mixer");
      nm = mus_mixer_scale(u2, XEN_TO_C_DOUBLE(uf1), res);
    }
  else
    {
      if (!u2)
	{
	  XEN_ASSERT_TYPE(u1_mixer, uf1, XEN_ARG_1, S_mixer_multiply, "a mixer");
	  nm = mus_mixer_scale(u1, XEN_TO_C_DOUBLE(uf2), res);
	}
    }
  if (!nm)
    {
      if ((u1_mixer) && (u2_mixer))
	nm = mus_mixer_multiply(u1, u2, res);
      else nm = mus_frame_to_frame(u1, u2, res);
    }
  if (res)
    return(ures);
  return(mus_xen_to_object(mus_any_to_mus_xen(nm)));

}


static XEN g_mixer_add(XEN uf1, XEN uf2, XEN ures) /* optional res */
{
  #define H_mixer_add "(" S_mixer_add " m1 m2 outm): add mixers m1 and m2 \
returning the mixer outm, or creating a new mixer if outm is not given. \
Either m1 or m2 can be a float, rather than a mixer."

  mus_any *res = NULL, *nm = NULL;

  if ((MUS_XEN_P(ures)) && 
      (mus_mixer_p(XEN_TO_MUS_ANY(ures))))
    res = (mus_any *)XEN_TO_MUS_ANY(ures);

  if (XEN_NUMBER_P(uf1))
    {
      XEN_ASSERT_TYPE((MUS_XEN_P(uf2)) && (mus_mixer_p(XEN_TO_MUS_ANY(uf2))), uf2, XEN_ARG_2, S_mixer_add, "a mixer");
      nm = mus_mixer_offset((mus_any *)XEN_TO_MUS_ANY(uf2), XEN_TO_C_DOUBLE(uf1), res);
    }
  else
    {
      if (XEN_NUMBER_P(uf2))
	{
	  XEN_ASSERT_TYPE((MUS_XEN_P(uf1)) && (mus_mixer_p(XEN_TO_MUS_ANY(uf1))), uf1, XEN_ARG_1, S_mixer_add, "a mixer");
	  nm = mus_mixer_offset((mus_any *)XEN_TO_MUS_ANY(uf1), XEN_TO_C_DOUBLE(uf2), res);
	}
    }

  if (!nm)
    {
      XEN_ASSERT_TYPE((MUS_XEN_P(uf1)) && (mus_mixer_p(XEN_TO_MUS_ANY(uf1))), uf1, XEN_ARG_1, S_mixer_add, "a mixer");
      XEN_ASSERT_TYPE((MUS_XEN_P(uf2)) && (mus_mixer_p(XEN_TO_MUS_ANY(uf2))), uf2, XEN_ARG_2, S_mixer_add, "a mixer");
      nm = mus_mixer_add((mus_any *)XEN_TO_MUS_ANY(uf1), (mus_any *)XEN_TO_MUS_ANY(uf2), res);
    }

  if (res)
    return(ures);
  return(mus_xen_to_object(mus_any_to_mus_xen(nm)));
}


/* frame->frame chooses the multiplication based on arg order */

static XEN g_frame_to_frame(XEN mx, XEN infr, XEN outfr) /* optional outfr */
{
  #define H_frame_to_frame "(" S_frame_to_frame " m f outf): pass frame f through mixer m \
returning frame outf (or creating a new frame if necessary); this is a matrix multiply of m and f"

  mus_any *res = NULL, *arg1, *arg2, *nm = NULL;

  XEN_ASSERT_TYPE(MUS_XEN_P(mx), mx, XEN_ARG_1, S_frame_to_frame, "a mixer or frame");
  XEN_ASSERT_TYPE(MUS_XEN_P(infr), infr, XEN_ARG_2, S_frame_to_frame, "a mixer or frame");

  arg1 = (mus_any *)XEN_TO_MUS_ANY(mx);
  arg2 = (mus_any *)XEN_TO_MUS_ANY(infr);
  XEN_ASSERT_TYPE((mus_frame_p(arg1) && mus_mixer_p(arg2)) || 
		  (mus_mixer_p(arg1) && mus_frame_p(arg2)), 
		  mx, XEN_ARG_1, S_frame_to_frame, "first two args should be mixer and frame");

  if ((MUS_XEN_P(outfr)) && 
      (mus_frame_p(XEN_TO_MUS_ANY(outfr)))) 
    res = (mus_any *)XEN_TO_MUS_ANY(outfr);

  nm = mus_frame_to_frame(arg1, arg2, res);
  if (res)
    return(outfr);
  return(mus_xen_to_object(mus_any_to_mus_xen(nm)));
}


static XEN g_frame_to_list(XEN fr)
{
  #define H_frame_to_list "(" S_frame_to_list " f): return contents of frame f as a list"
  mus_any *val;
  int i;
  mus_float_t *vals;
  XEN res = XEN_EMPTY_LIST;

  XEN_ASSERT_TYPE((MUS_XEN_P(fr)) && (mus_frame_p(XEN_TO_MUS_ANY(fr))), fr, XEN_ONLY_ARG, S_frame_to_list, "a frame");

  val = (mus_any *)XEN_TO_MUS_ANY(fr);
  vals = mus_data(val);
  for (i = (int)mus_length(val) - 1; i >= 0; i--) 
    res = XEN_CONS(C_TO_XEN_DOUBLE(vals[i]), res);
  return(res);
}


static XEN g_frame_to_sample(XEN mx, XEN fr)
{
  #define H_frame_to_sample "(" S_frame_to_sample " m f): pass frame f through mixer (or frame) m to produce a sample"
  XEN_ASSERT_TYPE((MUS_XEN_P(mx)), mx, XEN_ARG_1, S_frame_to_sample, "a frame or mixer");
  XEN_ASSERT_TYPE((MUS_XEN_P(fr)) && (mus_frame_p(XEN_TO_MUS_ANY(fr))), fr, XEN_ARG_2, S_frame_to_sample, "a frame");
  return(C_TO_XEN_DOUBLE(mus_frame_to_sample(XEN_TO_MUS_ANY(mx),
					     (mus_any *)XEN_TO_MUS_ANY(fr))));
}


static XEN g_sample_to_frame(XEN mx, XEN insp, XEN outfr) /* optional outfr */
{
  #define H_sample_to_frame "(" S_sample_to_frame " m val outf): pass the sample val through mixer m \
returning frame outf (creating it if necessary)"

  mus_any *res = NULL, *nf = NULL;

  XEN_ASSERT_TYPE((MUS_XEN_P(mx)), mx, XEN_ARG_1, S_sample_to_frame, "a frame or mixer");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(insp), insp, XEN_ARG_2, S_sample_to_frame, "a number");

  if ((MUS_XEN_P(outfr)) && 
      (mus_frame_p(XEN_TO_MUS_ANY(outfr)))) 
    res = (mus_any *)XEN_TO_MUS_ANY(outfr);

  nf = mus_sample_to_frame(XEN_TO_MUS_ANY(mx), XEN_TO_C_DOUBLE(insp), res);
  if (res)
    return(outfr);
  return(mus_xen_to_object(mus_any_to_mus_xen_with_vct(nf, xen_make_vct_wrapper(mus_length(nf), mus_data(nf)))));
}


static XEN g_make_scalar_mixer(XEN chans, XEN val)
{
  #define H_make_scalar_mixer "(" S_make_scalar_mixer " chans value): return a mixer \
with 'chans' channels, and 'val' along the diagonal"

  mus_any *mx = NULL;
  int size;

  XEN_ASSERT_TYPE(XEN_INTEGER_P(chans), chans, XEN_ARG_1, S_make_scalar_mixer, "an integer");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ARG_2, S_make_scalar_mixer, "a number");

  size = XEN_TO_C_INT(chans);
  if (size <= 0) XEN_OUT_OF_RANGE_ERROR(S_make_scalar_mixer, 1, chans, "chans ~A <= 0?");
  if (size > MUS_MAX_CHANS) XEN_OUT_OF_RANGE_ERROR(S_make_scalar_mixer, 1, chans, "too many chans: ~A");
  mx = mus_make_scalar_mixer(size, XEN_TO_C_DOUBLE(val));
  if (mx)
#if (!HAVE_SCHEME)
    return(mus_xen_to_object(mus_any_to_mus_xen(mx)));
#else
      {
	s7_pointer nv;
	nv = mus_xen_to_object(mus_any_to_mus_xen(mx));
#if 0
	s7_object_set_environment(nv, g_mixer_methods);
	s7_open_environment(nv);
#endif
	return(nv);
      }
#endif
  return(XEN_FALSE);
}


static XEN g_make_mixer_2(int len, XEN args)
{
  mus_any *ge;
  ge = (mus_any *)mus_make_empty_mixer((len == 0) ? 1 : len);
  if (ge)
    {
      if (len > 0)
	{
	  XEN lst;
	  int i, j, k, size;
	  mus_float_t **vals;
	  size = len * len;
	  vals = (mus_float_t **)mus_data(ge);
	  j = 0;
	  k = 0;
	  for (i = 0, lst = XEN_COPY_ARG(args); (i < size) && (XEN_NOT_NULL_P(lst)); i++, lst = XEN_CDR(lst))
	    {
	      if (XEN_NUMBER_P(XEN_CAR(lst)))
		vals[j][k] = XEN_TO_C_DOUBLE(XEN_CAR(lst));
	      else
		{
		  mus_free(ge);
		  XEN_WRONG_TYPE_ARG_ERROR(S_make_mixer, i, XEN_CAR(lst), "a number");
		}
	      k++;
	      if (k == len)
		{
		  k = 0;
		  j++;
		  if (j >= len) break;
		}
	    }
	}
#if (!HAVE_SCHEME)
      return(mus_xen_to_object(mus_any_to_mus_xen(ge)));
#else
      {
	s7_pointer nv;
	nv = mus_xen_to_object(mus_any_to_mus_xen(ge));
#if 0
	s7_object_set_environment(nv, g_mixer_methods);
	s7_open_environment(nv);
#endif
	return(nv);
      }
#endif
    }
  return(XEN_FALSE);

}


static XEN g_make_mixer_1(XEN arglist, bool check_size)
{
  #if HAVE_SCHEME
    #define make_mixer_example "(" S_make_mixer " 2 .5 .25 .125 1.0))"
  #endif
  #if HAVE_RUBY
    #define make_mixer_example "make_mixer(2, 0.5, 0.25, 0.125, 1.0)"
  #endif
  #if HAVE_FORTH
    #define make_mixer_example "2 0.5 0.25 0.125 1.0 make-mixer"
  #endif

  #define H_make_mixer "(" S_make_mixer " chans val0 val1 ...): make a new mixer object \
with chans inputs and outputs, initializing the scalars from the rest of the arguments:\n  " make_mixer_example "\n\
   | .5    .25 |\n\
   | .125 1.0  |\n"

  /* make_empty_mixer from first of arglist, then if more args, load vals */

  XEN cararg;
  int size = 0, len = 0;

  XEN_ASSERT_TYPE(XEN_LIST_P_WITH_LENGTH(arglist, len), arglist, XEN_ARG_1, S_make_mixer, "a list");
  if (len == 0) clm_error(S_make_mixer, "need at least 1 arg", arglist);

  cararg = XEN_CAR(arglist);
  if (!(XEN_NUMBER_P(cararg)))
    XEN_WRONG_TYPE_ARG_ERROR(S_make_mixer, 1, cararg, "an integer = number of chans");

  size = XEN_TO_C_INT_OR_ELSE(cararg, 0);
  if (size <= 0) XEN_OUT_OF_RANGE_ERROR(S_make_mixer, 1, cararg, "chans ~A <= 0?");
  if ((check_size) &&
      (size > MUS_MAX_CHANS))
    XEN_OUT_OF_RANGE_ERROR(S_make_mixer, 1, cararg, "chans ~A too big");
  if (len > (size * size + 1)) 
    clm_error(S_make_mixer, "extra trailing args?", arglist);

  return(g_make_mixer_2(size, (len == 1) ? XEN_EMPTY_LIST : XEN_CDR(arglist)));
}


static XEN g_make_mixer(XEN arglist)
{
  return(g_make_mixer_1(arglist, true));
}


static XEN g_make_mixer_unchecked(XEN arglist)
{
  #define H_make_mixer_unchecked "(make-mixer! chans val0 val1 ...): make a new mixer object \
with chans inputs and outputs, initializing the scalars from the rest of the arguments.  make-mixer! \
ignores mus-max-malloc and tries to return a mixer of any size."

  return(g_make_mixer_1(arglist, false));
}


static XEN g_mixer(XEN args) 
{
  #define H_mixer "(" S_mixer " num ...): returns a new mixer with args as its contents: (mixer .1 .2 .3 .4)"
  return(g_make_mixer_2((int)ceil(sqrt(XEN_LIST_LENGTH(args))), args));
}




/* ---------------- wave-train ---------------- */

static XEN g_make_wave_train(XEN arglist)
{
  #define H_make_wave_train "(" S_make_wave_train " (frequency *clm-default-frequency*) (initial-phase 0.0) (wave) (size clm-table-size) (type)): \
return a new wave-train generator (an extension of pulse-train).   Frequency is \
the repetition rate of the wave found in wave. Successive waves can overlap."

  mus_any *ge;
  XEN args[MAX_ARGLIST_LEN]; 
  XEN keys[5];
  int orig_arg[5] = {0, 0, 0, 0, MUS_INTERP_LINEAR};
  int vals, i, arglist_len;
  mus_long_t wsize = clm_table_size;
  vct *v = NULL;
  XEN orig_v = XEN_FALSE;
  mus_float_t freq, phase = 0.0;
  mus_float_t *wave = NULL;
  int interp_type = (int)MUS_INTERP_LINEAR;

  freq = clm_default_frequency;

  keys[0] = kw_frequency;
  keys[1] = kw_initial_phase;
  keys[2] = kw_wave;
  keys[3] = kw_size;
  keys[4] = kw_type;

  arglist_len = XEN_LIST_LENGTH(arglist);
  if (arglist_len > MAX_ARGLIST_LEN)
    clm_error(S_make_wave_train, "too many args!", arglist);

  for (i = 0; i < arglist_len; i++) args[i] = XEN_LIST_REF(arglist, i);
  for (i = arglist_len; i < MAX_ARGLIST_LEN; i++) args[i] = XEN_UNDEFINED;

  vals = mus_optkey_unscramble(S_make_wave_train, 5, keys, args, orig_arg);
  if (vals > 0)
    {
      freq = mus_optkey_to_float(keys[0], S_make_wave_train, orig_arg[0], freq);
      if (freq > (0.5 * mus_srate()))
	XEN_OUT_OF_RANGE_ERROR(S_make_wave_train, orig_arg[0], keys[0], "freq ~A > srate/2?");
      if (freq < 0.0)
	XEN_OUT_OF_RANGE_ERROR(S_make_wave_train, orig_arg[0], keys[0], "freq ~A?");

      phase = mus_optkey_to_float(keys[1], S_make_wave_train, orig_arg[1], phase);
      if (phase < 0.0)
	XEN_OUT_OF_RANGE_ERROR(S_make_wave_train, orig_arg[1], keys[1], "phase ~A?");

      v = mus_optkey_to_vct(keys[2], S_make_wave_train, orig_arg[2], NULL);
      if (v)
	{
	  orig_v = keys[2];
	  wave = v->data;
	  wsize = v->length;
	}

      wsize = mus_optkey_to_mus_long_t(keys[3], S_make_wave_train, orig_arg[3], wsize);
      if (wsize <= 0)
	XEN_OUT_OF_RANGE_ERROR(S_make_wave_train, orig_arg[3], keys[3], "size ~A <= 0?");
      if (wsize > mus_max_table_size())
	XEN_OUT_OF_RANGE_ERROR(S_make_wave_train, orig_arg[3], keys[3], "size ~A too large (see mus-max-table-size)");
      if ((v) && (wsize > v->length))
	XEN_OUT_OF_RANGE_ERROR(S_make_wave_train, orig_arg[3], keys[3], "size arg ~A bigger than size of provided wave");

      interp_type = mus_optkey_to_int(keys[4], S_make_wave_train, orig_arg[4], interp_type);
      if (!(mus_interp_type_p(interp_type)))
	XEN_OUT_OF_RANGE_ERROR(S_make_wave_train, orig_arg[4], keys[4], "no such interp-type: ~A");
    }

  if (wave == NULL) 
    {
      wave = (mus_float_t *)calloc(wsize, sizeof(mus_float_t));
      if (wave == NULL)
	return(clm_mus_error(MUS_MEMORY_ALLOCATION_FAILED, "can't allocate wave-train table"));
      orig_v = xen_make_vct(wsize, wave);
    }
  ge = mus_make_wave_train(freq, phase, wave, wsize, (mus_interp_t)interp_type);
  return(mus_xen_to_object(mus_any_to_mus_xen_with_vct(ge, orig_v)));
}


static XEN g_wave_train(XEN obj, XEN fm)
{
  #define H_wave_train "(" S_wave_train " gen (fm 0.0)): next sample of " S_wave_train
  mus_float_t fm1 = 0.0;
  mus_any *g = NULL;

  XEN_TO_C_GENERATOR(obj, g, mus_wave_train_p, S_wave_train, "a wave-train generator");
  XEN_TO_C_DOUBLE_IF_BOUND(fm, fm1, S_wave_train, XEN_ARG_2);

  return(C_TO_XEN_DOUBLE(mus_wave_train(g, fm1)));
}


static XEN g_wave_train_p(XEN obj) 
{
  #define H_wave_train_p "(" S_wave_train_p " gen): " PROC_TRUE " if gen is a " S_wave_train
  return(C_TO_XEN_BOOLEAN((MUS_XEN_P(obj)) && (mus_wave_train_p(XEN_TO_MUS_ANY(obj)))));
}



/* ---------------- waveshaping ---------------- */

enum {NO_PROBLEM_IN_LIST, NULL_LIST, ODD_LENGTH_LIST, NON_NUMBER_IN_LIST, NEGATIVE_NUMBER_IN_LIST, HUGE_NUMBER_IN_LIST};

static const char *list_to_partials_error_to_string(int code)
{
  switch (code)
    {
    case NO_PROBLEM_IN_LIST:          return("~A: nothing wrong with partials list?? ~A");                    break;
    case NULL_LIST:                   return("~A: partials list is null, ~A");                                break;
    case ODD_LENGTH_LIST:             return("~A: partials list has an odd number of elements: ~A");          break;
    case NON_NUMBER_IN_LIST:          return("~A: partials list has a non-numerical element: ~A");            break;
    case NEGATIVE_NUMBER_IN_LIST:     return("~A: partials list has a partial number that is negative: ~A");  break;
    case HUGE_NUMBER_IN_LIST:         return("~A: partials list has a partial number that is too large: ~A"); break;
    }
  return("~A: unknown error, ~A");
}


static mus_float_t *list_to_partials(XEN harms, int *npartials, int *error_code)
{
  int listlen, i, maxpartial = 0, curpartial;
  mus_float_t *partials = NULL;
  XEN lst;

  listlen = XEN_LIST_LENGTH(harms);
  if (listlen == 0)
    {
      (*error_code) = NULL_LIST;
      return(NULL);
    }

  if (listlen & 1)
    {
      (*error_code) = ODD_LENGTH_LIST;
      return(NULL);
    }

  if (!(XEN_NUMBER_P(XEN_CAR(harms)))) 
    {
      (*error_code) = NON_NUMBER_IN_LIST;
      return(NULL);
    }
  /* the list is '(partial-number partial-amp ... ) */
  (*error_code) = NO_PROBLEM_IN_LIST;

  for (i = 0, lst = XEN_COPY_ARG(harms); i < listlen; i += 2, lst = XEN_CDDR(lst))
    {
      if ((!(XEN_NUMBER_P(XEN_CAR(lst)))) ||
	  (!(XEN_NUMBER_P(XEN_CADR(lst)))))
	{
	  (*error_code) = NON_NUMBER_IN_LIST;
	  return(NULL);
	}
      curpartial = XEN_TO_C_INT(XEN_CAR(lst));
      if (curpartial < 0)
	{
	  (*error_code) = NEGATIVE_NUMBER_IN_LIST;
	  return(NULL);
	}
      if (curpartial > maxpartial) 
	maxpartial = curpartial;
    }

  if (maxpartial > 10000000)
    {
      (*error_code) = NEGATIVE_NUMBER_IN_LIST;
      return(NULL);
    }

  partials = (mus_float_t *)calloc(maxpartial + 1, sizeof(mus_float_t));
  /* TODO: here and elsewhere? this won't be null until we touch it in linux
   */
  if (partials == NULL)
    mus_error(MUS_MEMORY_ALLOCATION_FAILED, "can't allocate waveshaping partials list");
  (*npartials) = maxpartial + 1;

  for (i = 0, lst = XEN_COPY_ARG(harms); i < listlen; i += 2, lst = XEN_CDDR(lst))
    {
      curpartial = XEN_TO_C_INT(XEN_CAR(lst));
      partials[curpartial] = (mus_float_t)XEN_TO_C_DOUBLE(XEN_CADR(lst));
    }
  return(partials);
}


mus_float_t *mus_vct_to_partials(vct *v, int *npartials, int *error_code)
{
  int len, i, maxpartial, curpartial;
  mus_float_t *partials = NULL;

  len = v->length;
  if (len == 0)
    {
      (*error_code) = NULL_LIST;
      return(NULL);
    }
  if (len & 1)
    {
      (*error_code) = ODD_LENGTH_LIST;
      return(NULL);
    }
  (*error_code) = NO_PROBLEM_IN_LIST;

  maxpartial = (int)(v->data[0]);
  if (maxpartial < 0)
    (*error_code) = NEGATIVE_NUMBER_IN_LIST;
  else
    {
      for (i = 2; i < len; i += 2)
	{
	  curpartial = (int)(v->data[i]);
	  if (curpartial > maxpartial) 
	    maxpartial = curpartial;
	  if (curpartial < 0)
	    (*error_code) = NEGATIVE_NUMBER_IN_LIST;
	}
    }
  if ((*error_code) != NO_PROBLEM_IN_LIST)
    return(NULL);

  partials = (mus_float_t *)calloc(maxpartial + 1, sizeof(mus_float_t));
  if (partials == NULL)
    mus_error(MUS_MEMORY_ALLOCATION_FAILED, "can't allocate waveshaping partials list");
  (*npartials) = maxpartial + 1;

  for (i = 0; i < len; i += 2)
    {
      curpartial = (int)(v->data[i]);
      partials[curpartial] = v->data[i + 1];
    }
  return(partials);
}


static XEN g_partials_to_polynomial(XEN amps, XEN ukind)
{
  #if HAVE_SCHEME
    #define p2p_example "(let ((v0 (partials->polynomial '(1 1.0 2 1.0)))\n        (os (make-oscil)))\n    (polynomial v0 (oscil os)))"
  #endif
  #if HAVE_RUBY
    #define p2p_example "v0 = partials2polynomial([1, 1.0, 2, 1.0])\n  os = make_oscil()\n  polynomial(v0, oscil(os))"
  #endif
  #if HAVE_FORTH
    #define p2p_example "'( 1 1.0 2 1.0 ) partials->polynomial value v0\n  make-oscil value os\n  v0 os 0.0 0.0 oscil polynomial"
  #endif

  #define H_partials_to_polynomial "(" S_partials_to_polynomial " partials (kind " S_mus_chebyshev_first_kind ")): \
produce a Chebyshev polynomial suitable for use with the " S_polynomial " generator \
to create (via waveshaping) the harmonic spectrum described by the partials argument:\n  " p2p_example

  int npartials = 0;
  mus_polynomial_t kind = MUS_CHEBYSHEV_FIRST_KIND;
  mus_float_t *partials = NULL, *wave;
  int error = NO_PROBLEM_IN_LIST;

  XEN_ASSERT_TYPE(MUS_VCT_P(amps) || XEN_LIST_P(amps), amps, XEN_ARG_1, S_partials_to_polynomial, "a list or a vct");
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(ukind), ukind, XEN_ARG_2, S_partials_to_polynomial, "either " S_mus_chebyshev_first_kind " or " S_mus_chebyshev_second_kind);

  if (XEN_INTEGER_P(ukind))
    {
      int ck;
      ck = XEN_TO_C_INT(ukind);
      if ((ck >= MUS_CHEBYSHEV_EITHER_KIND) && (ck <= MUS_CHEBYSHEV_SECOND_KIND))
	kind = (mus_polynomial_t)ck;
      else XEN_OUT_OF_RANGE_ERROR(S_partials_to_polynomial, 2, ukind, "~A: unknown Chebyshev polynomial kind");
    }
  
  if (MUS_VCT_P(amps))
    partials = mus_vct_to_partials(XEN_TO_VCT(amps), &npartials, &error);
  else partials = list_to_partials(amps, &npartials, &error);

  if (partials == NULL)
    XEN_ERROR(NO_DATA, 
	      XEN_LIST_3(C_TO_XEN_STRING(list_to_partials_error_to_string(error)), 
			 C_TO_XEN_STRING(S_partials_to_polynomial), 
			 amps));

  wave = mus_partials_to_polynomial(npartials, partials, kind); /* wave == partials; in both vct and list cases, partials is newly allocated */
  return(xen_make_vct(npartials, wave));
}


static XEN g_normalize_partials(XEN partials)
{
  #define H_normalize_partials "(" S_normalize_partials " partials) scales the \
partial amplitudes in the vct or list 'partials' by the inverse of their sum (so that they add to 1.0)."

  vct *v;
  XEN xv = XEN_FALSE;

  XEN_ASSERT_TYPE(((XEN_LIST_P(partials)) && (XEN_NOT_NULL_P(partials))) || (MUS_VCT_P(partials)), partials, XEN_ONLY_ARG, S_normalize_partials, "a vct or (non-empty) list");

  if (MUS_VCT_P(partials))
    xv = partials;
  else xv = xen_list_to_vct(partials);
  v = XEN_TO_VCT(xv);

  if ((v->length > 1) &&
      ((v->length & 1) == 0))
    mus_normalize_partials(v->length / 2, v->data);
  else XEN_ERROR(BAD_TYPE,
		 XEN_LIST_3(C_TO_XEN_STRING("~A: partials, ~A, must be a non-empty list or vct of even length (partial-number partial-amp ...)"),
			    C_TO_XEN_STRING(S_normalize_partials),
			    partials));
  return(xv);
}


static XEN g_chebyshev_tu_sum(XEN x, XEN tn, XEN un)
{
  #define H_chebyshev_tu_sum "(" S_mus_chebyshev_tu_sum " x tn un) returns the sum of the weighted\
Chebyshev polynomials Tn and Un (vcts), with phase x."

  vct *Tn, *Un;
  
  XEN_ASSERT_TYPE(XEN_DOUBLE_P(x), x, XEN_ARG_1, S_mus_chebyshev_tu_sum, "a float");
  XEN_ASSERT_TYPE(MUS_VCT_P(tn), tn, XEN_ARG_2, S_mus_chebyshev_tu_sum, "a vct");
  XEN_ASSERT_TYPE(MUS_VCT_P(un), un, XEN_ARG_3, S_mus_chebyshev_tu_sum, "a vct");

  Tn = XEN_TO_VCT(tn);
  Un = XEN_TO_VCT(un);

  return(C_TO_XEN_DOUBLE(mus_chebyshev_tu_sum(XEN_TO_C_DOUBLE(x), Tn->length, Tn->data, Un->data)));
}


static XEN g_chebyshev_t_sum(XEN x, XEN tn)
{
  #define H_chebyshev_t_sum "(" S_mus_chebyshev_t_sum " x tn) returns the sum of the weighted \
Chebyshev polynomials Tn (a vct)."

  vct *Tn;
  
  XEN_ASSERT_TYPE(XEN_DOUBLE_P(x), x, XEN_ARG_1, S_mus_chebyshev_tu_sum, "a float");
  XEN_ASSERT_TYPE(MUS_VCT_P(tn), tn, XEN_ARG_2, S_mus_chebyshev_tu_sum, "a vct");

  Tn = XEN_TO_VCT(tn);

  return(C_TO_XEN_DOUBLE(mus_chebyshev_t_sum(XEN_TO_C_DOUBLE(x), Tn->length, Tn->data)));
}


static XEN g_chebyshev_u_sum(XEN x, XEN un)
{
  #define H_chebyshev_u_sum "(" S_mus_chebyshev_u_sum " x un) returns the sum of the weighted \
Chebyshev polynomials Un (a vct)."

  vct *Un;
  
  XEN_ASSERT_TYPE(XEN_DOUBLE_P(x), x, XEN_ARG_1, S_mus_chebyshev_tu_sum, "a float");
  XEN_ASSERT_TYPE(MUS_VCT_P(un), un, XEN_ARG_2, S_mus_chebyshev_tu_sum, "a vct");

  Un = XEN_TO_VCT(un);

  return(C_TO_XEN_DOUBLE(mus_chebyshev_u_sum(XEN_TO_C_DOUBLE(x), Un->length, Un->data)));
}




/* ---------------- polyshape ---------------- */

static XEN g_polyshape(XEN obj, XEN index, XEN fm)
{
  #define H_polyshape "(" S_polyshape " gen (index 1.0) (fm 0.0)): next sample of polynomial-based waveshaper"
  mus_float_t fm1 = 0.0, index1 = 1.0;
  mus_any *g = NULL;

  XEN_TO_C_GENERATOR(obj, g, mus_polyshape_p, S_polyshape, "a polyshape generator");
  XEN_TO_C_DOUBLE_IF_BOUND(index, index1, S_polyshape, XEN_ARG_2);
  XEN_TO_C_DOUBLE_IF_BOUND(fm, fm1, S_polyshape, XEN_ARG_3);

  return(C_TO_XEN_DOUBLE(mus_polyshape(g, index1, fm1)));
}


static XEN g_polyshape_p(XEN obj) 
{
  #define H_polyshape_p "(" S_polyshape_p " gen): " PROC_TRUE " if gen is a " S_polyshape
  return(C_TO_XEN_BOOLEAN((MUS_XEN_P(obj)) && (mus_polyshape_p(XEN_TO_MUS_ANY(obj)))));
}


static XEN g_make_polyshape(XEN arglist)
{
  #define H_make_polyshape "(" S_make_polyshape " (frequency *clm-default-frequency*) (initial-phase 0.0) (coeffs) (partials '(1 1)) (kind " S_mus_chebyshev_first_kind ")): \
return a new polynomial-based waveshaping generator:\n\
   (" S_make_polyshape " :coeffs (" S_partials_to_polynomial " '(1 1.0)))\n\
is the same in effect as " S_make_oscil

  mus_any *ge;
  XEN args[MAX_ARGLIST_LEN]; 
  int arglist_len;
  XEN keys[5];
  int orig_arg[5] = {0, 0, 0, 0, 0};
  int i, ck, vals, csize = 0, npartials = 0;
  vct *v = NULL;
  XEN orig_v = XEN_FALSE;
  mus_float_t freq, phase = 0.0; 
  /* 
   * if we followed the definition directly, the initial phase default would be M_PI_2 (pi/2) so that
   *   we drive the Tn's with a cosine.  But I've always used sine instead, so I think I'll leave
   *   it that way.  There is no difference in the output waveform except an overall phase
   *   offset.  So, with sine, the phases rotate through cos sin -cos -sin... rather than being all cos,
   *   but these add to exactly the same actual wave -- what you'd expect since Tn doesn't know
   *   where we started.  This also does not affect "signification".
   */
  mus_float_t *coeffs = NULL, *partials = NULL;

  mus_polynomial_t kind = MUS_CHEBYSHEV_FIRST_KIND;
  freq = clm_default_frequency;

  keys[0] = kw_frequency;
  keys[1] = kw_initial_phase;
  keys[2] = kw_coeffs;
  keys[3] = kw_partials;
  keys[4] = kw_kind;

  arglist_len = XEN_LIST_LENGTH(arglist);
  if (arglist_len > MAX_ARGLIST_LEN)
    clm_error(S_make_polyshape, "too many args!", arglist);

  for (i = 0; i < arglist_len; i++) args[i] = XEN_LIST_REF(arglist, i);
  for (i = arglist_len; i < MAX_ARGLIST_LEN; i++) args[i] = XEN_UNDEFINED;

  vals = mus_optkey_unscramble(S_make_polyshape, 5, keys, args, orig_arg);
  if (vals > 0)
    {
      freq = mus_optkey_to_float(keys[0], S_make_polyshape, orig_arg[0], freq);
      if (freq > (0.5 * mus_srate()))
	XEN_OUT_OF_RANGE_ERROR(S_make_polyshape, orig_arg[0], keys[0], "freq ~A > srate/2?");

      phase = mus_optkey_to_float(keys[1], S_make_polyshape, orig_arg[2], phase);

      ck = mus_optkey_to_int(keys[4], S_make_polyshape, orig_arg[4], (int)kind);
      if ((ck >= MUS_CHEBYSHEV_EITHER_KIND) && (ck <= MUS_CHEBYSHEV_SECOND_KIND))
	kind = (mus_polynomial_t)ck;
      else XEN_OUT_OF_RANGE_ERROR(S_make_polyshape, orig_arg[4], keys[4], "~A: unknown Chebyshev polynomial kind");

      v = mus_optkey_to_vct(keys[2], S_make_polyshape, orig_arg[2], NULL);
      if (v)
        {
	  orig_v = keys[2];
	  coeffs = v->data;
	  csize = v->length;
	}
      else
	{
	  if (!(XEN_KEYWORD_P(keys[3])))
	    {
	      int error = NO_PROBLEM_IN_LIST;
	      if (MUS_VCT_P(keys[3]))
		partials = mus_vct_to_partials(XEN_TO_VCT(keys[3]), &npartials, &error);
	      else
		{
		  XEN_ASSERT_TYPE(XEN_LIST_P(keys[3]), keys[3], orig_arg[3], S_make_polyshape, "a list or a vct");
		  partials = list_to_partials(keys[3], &npartials, &error);
		}
	      if (partials == NULL)
		XEN_ERROR(NO_DATA, 
			  XEN_LIST_3(C_TO_XEN_STRING(list_to_partials_error_to_string(error)), 
				     C_TO_XEN_STRING(S_make_polyshape), 
				     keys[3]));
	      coeffs = mus_partials_to_polynomial(npartials, partials, kind);
	      csize = npartials;
	      /* coeffs = partials here, so don't delete */ 
	    }
	}
    }

  if (!coeffs)
    {
      /* clm.html says '(1 1) is the default */
      mus_float_t *data;
      data = (mus_float_t *)calloc(2, sizeof(mus_float_t));
      data[0] = 0.0;
      data[1] = 1.0;
      coeffs = mus_partials_to_polynomial(2, data, kind);
      csize = 2;
    }

  if (XEN_FALSE_P(orig_v))
    orig_v = xen_make_vct(csize, coeffs);

  ge = mus_make_polyshape(freq, phase, coeffs, csize, kind);
  if (ge) 
    return(mus_xen_to_object(mus_any_to_mus_xen_with_vct(ge, orig_v)));
  return(XEN_FALSE);
}



/* ---------------- polywave ---------------- */

static XEN g_polywave(XEN obj, XEN fm)
{
  #define H_polywave "(" S_polywave " gen (fm 0.0)): next sample of polywave waveshaper"
  mus_float_t fm1 = 0.0;
  mus_any *g = NULL;

  XEN_TO_C_GENERATOR(obj, g, mus_polywave_p, S_polywave, "a polywave generator");
  XEN_TO_C_DOUBLE_IF_BOUND(fm, fm1, S_polywave, XEN_ARG_3);

  return(C_TO_XEN_DOUBLE(mus_polywave(g, fm1)));
}


static XEN g_polywave_p(XEN obj) 
{
  #define H_polywave_p "(" S_polywave_p " gen): " PROC_TRUE " if gen is a " S_polywave " generator"
  return(C_TO_XEN_BOOLEAN((MUS_XEN_P(obj)) && (mus_polywave_p(XEN_TO_MUS_ANY(obj)))));
}


static XEN g_make_polywave(XEN arglist)
{
  #define H_make_polywave "(" S_make_polywave " (frequency *clm-default-frequency*) (partials '(1 1)) (type " S_mus_chebyshev_first_kind ")): \
return a new polynomial-based waveshaping generator.  (" S_make_polywave " :partials (vct 1 1.0)) is the same in effect as " S_make_oscil "."

  mus_any *ge;
  XEN args[MAX_ARGLIST_LEN]; 
  int arglist_len;
  XEN keys[3];
  int orig_arg[3] = {0, 0, 0};
  int i, type, vals, n = 0, npartials = 0;
  XEN orig_v = XEN_FALSE;
  mus_float_t freq; 
  mus_float_t *coeffs = NULL, *partials = NULL;
  mus_polynomial_t kind = MUS_CHEBYSHEV_FIRST_KIND;

  freq = clm_default_frequency;

  keys[0] = kw_frequency;
  keys[1] = kw_partials;
  keys[2] = kw_type;

  arglist_len = XEN_LIST_LENGTH(arglist);
  if (arglist_len > MAX_ARGLIST_LEN)
    clm_error(S_make_polywave, "too many args!", arglist);

  for (i = 0; i < arglist_len; i++) args[i] = XEN_LIST_REF(arglist, i);
  for (i = arglist_len; i < MAX_ARGLIST_LEN; i++) args[i] = XEN_UNDEFINED;

  vals = mus_optkey_unscramble(S_make_polywave, 3, keys, args, orig_arg);
  if (vals > 0)
    {
      freq = mus_optkey_to_float(keys[0], S_make_polywave, orig_arg[0], freq);
      if (freq > (0.5 * mus_srate()))
	XEN_OUT_OF_RANGE_ERROR(S_make_polywave, orig_arg[0], keys[0], "freq ~A > srate/2?");

      type = mus_optkey_to_int(keys[2], S_make_polywave, orig_arg[2], (int)kind);
      if ((type >= MUS_CHEBYSHEV_EITHER_KIND) && 
	  (type <= MUS_CHEBYSHEV_SECOND_KIND))
	kind = (mus_polynomial_t)type;
      else XEN_OUT_OF_RANGE_ERROR(S_make_polywave, orig_arg[2], keys[2], "~A: unknown Chebyshev polynomial kind");

      if (!(XEN_KEYWORD_P(keys[1])))
	{
	  int error = NO_PROBLEM_IN_LIST;
	  if (MUS_VCT_P(keys[1]))
	    partials = mus_vct_to_partials(XEN_TO_VCT(keys[1]), &npartials, &error);
	  else
	    {
	      XEN_ASSERT_TYPE(XEN_LIST_P(keys[1]), keys[1], orig_arg[1], S_make_polywave, "a list or a vct");
	      partials = list_to_partials(keys[1], &npartials, &error);
	    }

	  if (partials == NULL)
	    XEN_ERROR(NO_DATA, 
		      XEN_LIST_3(C_TO_XEN_STRING(list_to_partials_error_to_string(error)), 
				 C_TO_XEN_STRING(S_make_polywave), 
				 keys[1]));

	  coeffs = partials;
	  n = npartials;
	  /* coeffs = partials here, so don't delete */ 
	}
    }

  if (!coeffs)
    {
      /* clm.html says '(1 1) is the default but table-lookup is 0? */
      mus_float_t *data;
      data = (mus_float_t *)calloc(2, sizeof(mus_float_t));
      data[0] = 0.0;
      data[1] = 1.0;
      coeffs = data;
      n = 2; 
    }
  orig_v = xen_make_vct(n, coeffs);

  ge = mus_make_polywave(freq, coeffs, n, kind);
  if (ge) return(mus_xen_to_object(mus_any_to_mus_xen_with_vct(ge, orig_v)));
  return(XEN_FALSE);
}


/* ---------------- nrxysin and nrxycos ---------------- */

static XEN g_nrxysin_p(XEN obj) 
{
  #define H_nrxysin_p "(" S_nrxysin_p " gen): " PROC_TRUE " if gen is an " S_nrxysin " generator"
  return(C_TO_XEN_BOOLEAN((MUS_XEN_P(obj)) && 
			  (mus_nrxysin_p(XEN_TO_MUS_ANY(obj)))));
}


static XEN g_nrxycos_p(XEN obj) 
{
  #define H_nrxycos_p "(" S_nrxycos_p " gen): " PROC_TRUE " if gen is an " S_nrxycos " generator"
  return(C_TO_XEN_BOOLEAN((MUS_XEN_P(obj)) && 
			  (mus_nrxycos_p(XEN_TO_MUS_ANY(obj)))));
}


static XEN g_nrxysin(XEN obj, XEN fm)
{
  #define H_nrxysin "(" S_nrxysin " gen (fm 0.0)): next sample of nrxysin generator"
  mus_float_t fm1 = 0.0;
  mus_any *g = NULL;

  XEN_TO_C_GENERATOR(obj, g, mus_nrxysin_p, S_nrxysin, "an nrxysin generator");
  XEN_TO_C_DOUBLE_IF_BOUND(fm, fm1, S_nrxysin, XEN_ARG_2);

  return(C_TO_XEN_DOUBLE(mus_nrxysin(g, fm1)));
}

static XEN g_nrxycos(XEN obj, XEN fm)
{
  #define H_nrxycos "(" S_nrxycos " gen (fm 0.0)): next sample of nrxycos generator"
  mus_float_t fm1 = 0.0;
  mus_any *g = NULL;

  XEN_TO_C_GENERATOR(obj, g, mus_nrxycos_p, S_nrxycos, "an nrxycos generator");
  XEN_TO_C_DOUBLE_IF_BOUND(fm, fm1, S_nrxycos, XEN_ARG_2);

  return(C_TO_XEN_DOUBLE(mus_nrxycos(g, fm1)));
}


static XEN g_make_nrxy(bool sin_case, const char *caller, XEN arglist)
{
  mus_any *ge;
  XEN args[MAX_ARGLIST_LEN]; 
  XEN keys[4];
  int orig_arg[4] = {0, 0, 0, 0};
  int vals, i, arglist_len;
  mus_float_t freq, r = 0.5, ratio = 1.0;
  int n = 1;

  freq = clm_default_frequency;

  keys[0] = kw_frequency;
  keys[1] = kw_ratio;
  keys[2] = kw_n;
  keys[3] = kw_r;

  arglist_len = XEN_LIST_LENGTH(arglist);
  if (arglist_len > MAX_ARGLIST_LEN)
    clm_error(caller, "too many args!", arglist);

  for (i = 0; i < arglist_len; i++) args[i] = XEN_LIST_REF(arglist, i);
  for (i = arglist_len; i < MAX_ARGLIST_LEN; i++) args[i] = XEN_UNDEFINED;

  vals = mus_optkey_unscramble(caller, 4, keys, args, orig_arg);
  if (vals > 0)
    {
      freq = mus_optkey_to_float(keys[0], caller, orig_arg[0], freq);
      if (freq > (0.5 * mus_srate()))
	XEN_OUT_OF_RANGE_ERROR(caller, orig_arg[0], keys[0], "freq ~A > srate/2?");

      ratio = mus_optkey_to_float(keys[1], caller, orig_arg[1], ratio);

      n = mus_optkey_to_int(keys[2], caller, orig_arg[2], n);
      if (n < 0)
	XEN_OUT_OF_RANGE_ERROR(caller, orig_arg[2], keys[2], "n (sidebands): ~A?");

      r = mus_optkey_to_float(keys[3], caller, orig_arg[3], r);
      if ((r >= 1.0) ||
	  (r <= -1.0))
	XEN_OUT_OF_RANGE_ERROR(caller, orig_arg[3], keys[3], "r (sideband amp ratio): ~A?");
      /* if not with doubles, this actually maxes out around .99999999 because mus_optkey_to_float (apparently) rounds up */
    }
  if (sin_case)
    ge = mus_make_nrxysin(freq, ratio, n, r);
  else ge = mus_make_nrxycos(freq, ratio, n, r);
  if (ge) return(mus_xen_to_object(mus_any_to_mus_xen(ge)));
  return(XEN_FALSE);
}


static XEN g_make_nrxysin(XEN arglist)
{
  #define H_make_nrxysin "(" S_make_nrxysin " (frequency *clm-default-frequency*) (ratio 1.0) (n 1) (r 0.5)): \
return a new nrxysin generator."

  return(g_make_nrxy(true, S_make_nrxysin, arglist));
}

static XEN g_make_nrxycos(XEN arglist)
{
  #define H_make_nrxycos "(" S_make_nrxycos " (frequency *clm-default-frequency*) (ratio 1.0) (n 1) (r 0.5)): \
return a new nrxycos generator."

  return(g_make_nrxy(false, S_make_nrxycos, arglist));
}



/* ----------------  filter ---------------- */

typedef enum {G_FILTER, G_FIR_FILTER, G_IIR_FILTER} xclm_fir_t;

static XEN g_make_fir_coeffs(XEN order, XEN envl)
{
  #define H_make_fir_coeffs "(" S_make_fir_coeffs " order v): turn spectral envelope in vct v into coeffs for FIR filter"
  int size;
  mus_float_t *a;
  vct *v;

  XEN_ASSERT_TYPE(XEN_INTEGER_P(order), order, XEN_ARG_1, S_make_fir_coeffs, "int");
  XEN_ASSERT_TYPE(MUS_VCT_P(envl), envl, XEN_ARG_2, S_make_fir_coeffs, "a vct");

  v = XEN_TO_VCT(envl);

  size = XEN_TO_C_INT(order);
  if (size != v->length)
    XEN_ERROR(CLM_ERROR,
	      XEN_LIST_3(C_TO_XEN_STRING(S_make_fir_coeffs ": order ~A != vct length ~A"),
			 order, 
			 envl));

  a = mus_make_fir_coeffs(XEN_TO_C_INT(order), v->data, NULL);
  return(xen_make_vct(v->length, a));
}


static XEN g_filter_p(XEN obj) 
{
  #define H_filter_p "(" S_filter_p " gen): " PROC_TRUE " if gen is a " S_filter
  return(C_TO_XEN_BOOLEAN((MUS_XEN_P(obj)) && (mus_filter_p(XEN_TO_MUS_ANY(obj)))));
}


static XEN g_fir_filter_p(XEN obj) 
{
  #define H_fir_filter_p "(" S_fir_filter_p " gen): " PROC_TRUE " if gen is an " S_fir_filter
  return(C_TO_XEN_BOOLEAN((MUS_XEN_P(obj)) && (mus_fir_filter_p(XEN_TO_MUS_ANY(obj)))));
}


static XEN g_iir_filter_p(XEN obj) 
{
  #define H_iir_filter_p "(" S_iir_filter_p " gen): " PROC_TRUE " if gen is an " S_iir_filter
  return(C_TO_XEN_BOOLEAN((MUS_XEN_P(obj)) && (mus_iir_filter_p(XEN_TO_MUS_ANY(obj)))));
}


static XEN g_filter(XEN obj, XEN input)
{
  #define H_filter "(" S_filter " gen (input 0.0)): next sample from filter"
  mus_any *g = NULL;
  XEN_TO_C_GENERATOR(obj, g, mus_filter_p, S_filter, "a filter");

  XEN_ASSERT_TYPE(XEN_NUMBER_P(input), input, XEN_ARG_2, S_filter, "a number");
  return(C_TO_XEN_DOUBLE(mus_filter(g, XEN_TO_C_DOUBLE(input))));
}


static XEN g_fir_filter(XEN obj, XEN input)
{
  #define H_fir_filter "(" S_fir_filter " gen (input 0.0)): next sample from FIR filter"
  mus_any *g = NULL;
  XEN_TO_C_GENERATOR(obj, g, mus_fir_filter_p, S_fir_filter, "an FIR filter");

  XEN_ASSERT_TYPE(XEN_NUMBER_P(input), input, XEN_ARG_2, S_fir_filter, "a number");
  return(C_TO_XEN_DOUBLE(mus_fir_filter(g, XEN_TO_C_DOUBLE(input))));
}


static XEN g_iir_filter(XEN obj, XEN input)
{
  #define H_iir_filter "(" S_iir_filter " gen (input 0.0)): next sample from IIR filter"
  mus_any *g = NULL;
  XEN_TO_C_GENERATOR(obj, g, mus_iir_filter_p, S_iir_filter, "an IIR filter");

  XEN_ASSERT_TYPE(XEN_NUMBER_P(input), input, XEN_ARG_2, S_iir_filter, "a number");
  return(C_TO_XEN_DOUBLE(mus_iir_filter(g, XEN_TO_C_DOUBLE(input))));
}


static XEN g_make_filter_1(xclm_fir_t choice, XEN arg1, XEN arg2, XEN arg3, XEN arg4, XEN arg5, XEN arg6)
{
  XEN xwave = XEN_UNDEFINED, ywave = XEN_UNDEFINED;
  mus_any *fgen = NULL;
  mus_xen *gn = NULL;
  XEN args[8]; 
  XEN keys[4];
  int orig_arg[4] = {0, 0, 0, 0};
  vct *x = NULL, *y = NULL;
  int vals, order = 0;
  const char *caller;
  if (choice == G_FILTER) caller = S_make_filter; else if (choice == G_FIR_FILTER) caller = S_make_fir_filter; else caller = S_make_iir_filter;

  keys[0] = kw_order;
  keys[1] = kw_x_coeffs;
  keys[2] = kw_y_coeffs;
  keys[3] = kw_coeffs;
  args[0] = arg1; args[1] = arg2; args[2] = arg3; args[3] = arg4; args[4] = arg5; args[5] = arg6; args[6] = XEN_UNDEFINED; args[7] = XEN_UNDEFINED;

  vals = mus_optkey_unscramble(caller, 4, keys, args, orig_arg);
  if (vals > 0)
    {
      if (!(XEN_KEYWORD_P(keys[0])))
	{
	  order = mus_optkey_to_int(keys[0], caller, orig_arg[0], 0);
	  if (order <= 0)
	    XEN_OUT_OF_RANGE_ERROR(caller, orig_arg[0], keys[0], "order ~A <= 0?");
	}

      if (!(XEN_KEYWORD_P(keys[1])))
        {
	  XEN_ASSERT_TYPE(MUS_VCT_P(keys[1]), keys[1], orig_arg[1], caller, "a vct");
	  if (choice == G_IIR_FILTER)
	    {
	      ywave = keys[1];
	      y = XEN_TO_VCT(ywave);
	    }
	  else
	    {
	      xwave = keys[1];
	      x = XEN_TO_VCT(xwave);
	    }
        }

      if (!(XEN_KEYWORD_P(keys[2])))
	{
	  XEN_ASSERT_TYPE(MUS_VCT_P(keys[2]), keys[2], orig_arg[2], caller, "a vct");
	  ywave = keys[2];
	  y = XEN_TO_VCT(ywave);
	}

      if ((choice != G_FILTER) && (!(XEN_KEYWORD_P(keys[3]))))
        {
	  if (choice == G_IIR_FILTER)
	    clm_error(caller, "redundant arg passed to " S_make_iir_filter "?", keys[3]);
	  else clm_error(caller, "redundant arg passed to " S_make_fir_filter "?", keys[3]);
        }
    }

  if (choice == G_FILTER)
    {
      if (y == NULL)
	choice = G_FIR_FILTER;
      else 
	{
	  if (x == NULL)
	    choice = G_IIR_FILTER;
	}
    }
  if (((x == NULL) && (choice != G_IIR_FILTER)) ||
      ((y == NULL) && (choice != G_FIR_FILTER)))
    XEN_ERROR(NO_DATA,
	      XEN_LIST_2(C_TO_XEN_STRING("~A: no coeffs?"),
			 C_TO_XEN_STRING(caller)));
  if (order == 0)
    {
      if (x)
	order = x->length;
      else order = y->length;
    }
  else
    {
      if ((x) && (order > x->length))
	{
	  XEN_ERROR(CLM_ERROR,
		    XEN_LIST_4(C_TO_XEN_STRING("~A: xcoeffs, ~A, must match order, ~A"),
			       C_TO_XEN_STRING(caller),
			       keys[1],
			       keys[0]));
	}
      else
	{
	  if ((y) && (order > y->length))
	    XEN_ERROR(CLM_ERROR,
		      XEN_LIST_4(C_TO_XEN_STRING("~A: ycoeffs, ~A, must match order, ~A"),
				 C_TO_XEN_STRING(caller),
				 keys[2],
				 keys[0])); 
	  else
	    {
	      if ((x) && (y) && (x->length != y->length))
		XEN_ERROR(CLM_ERROR,
			  XEN_LIST_4(C_TO_XEN_STRING("~A: coeffs must be same length.  x len: ~A, y len: ~A"),
				     C_TO_XEN_STRING(caller),
				     C_TO_XEN_INT(x->length),
				     C_TO_XEN_INT(y->length)));
	    }
	}
    }
  switch (choice)
    {
    case G_FILTER: fgen = mus_make_filter(order, x->data, y->data, NULL); break;
    case G_FIR_FILTER: fgen = mus_make_fir_filter(order, x->data, NULL); break;
    case G_IIR_FILTER: fgen = mus_make_iir_filter(order, y->data, NULL); break;
    }
  if (fgen)
    {
      gn = (mus_xen *)calloc(1, sizeof(mus_xen));
      gn->gen = fgen;                                    /* delay gn allocation since make_filter can throw an error */
      gn->nvcts = 3;
      gn->vcts = make_vcts(gn->nvcts);
      gn->vcts[G_FILTER_STATE] = xen_make_vct_wrapper(order, mus_data(fgen));
      gn->vcts[G_FILTER_XCOEFFS] = xwave;
      gn->vcts[G_FILTER_YCOEFFS] = ywave;
      return(mus_xen_to_object(gn));
    }
  return(XEN_FALSE);
}


static XEN g_make_filter(XEN arg1, XEN arg2, XEN arg3, XEN arg4, XEN arg5, XEN arg6)
{
  #define H_make_filter "(" S_make_filter " order xcoeffs ycoeffs): return a new direct form FIR/IIR filter, coeff args are vcts"
  return(g_make_filter_1(G_FILTER, arg1, arg2, arg3, arg4, arg5, arg6));
}


static XEN g_make_fir_filter(XEN arg1, XEN arg2, XEN arg3, XEN arg4)
{
  #define H_make_fir_filter "(" S_make_fir_filter " order xcoeffs): return a new FIR filter, xcoeffs a vct"
  return(g_make_filter_1(G_FIR_FILTER, arg1, arg2, arg3, arg4, XEN_UNDEFINED, XEN_UNDEFINED));
}


static XEN g_make_iir_filter(XEN arg1, XEN arg2, XEN arg3, XEN arg4)
{
  #define H_make_iir_filter "(" S_make_iir_filter " order ycoeffs): return a new IIR filter, ycoeffs a vct"
  return(g_make_filter_1(G_IIR_FILTER, arg1, arg2, arg3, arg4, XEN_UNDEFINED, XEN_UNDEFINED));
}




/* ---------------- env ---------------- */

static XEN g_env_p(XEN obj) 
{
  #define H_env_p "(" S_env_p " gen): " PROC_TRUE " if gen is a " S_env
  return(C_TO_XEN_BOOLEAN((MUS_XEN_P(obj)) && (mus_env_p(XEN_TO_MUS_ANY(obj)))));
}


static XEN g_env(XEN obj) 
{
  #define H_env "(" S_env " gen): next sample from envelope generator"
  mus_any *g = NULL;
  XEN_TO_C_GENERATOR(obj, g, mus_env_p, S_env, "an env generator");
  return(C_TO_XEN_DOUBLE(mus_env(g)));
}


static XEN g_make_env(XEN arglist)
{
  #define H_make_env "(" S_make_env " envelope (scaler 1.0) (duration) (offset 0.0) (base 1.0) (end) (length)): \
return a new envelope generator.  'envelope' is a list or vct of break-point pairs. To create the envelope, \
these points are offset by 'offset', scaled by 'scaler', and mapped over the time interval defined by \
either 'duration' (seconds) or 'length' (samples).  If 'base' is 1.0, the connecting segments \
are linear, if 0.0 you get a step function, and anything else produces an exponential connecting segment."

  mus_any *ge;
  XEN args[MAX_ARGLIST_LEN]; 
  XEN keys[7];
  int orig_arg[7] = {0, 0, 0, 0, 0, 0, 0};
  int vals, i, len = 0, arglist_len;
  mus_float_t base = 1.0, scaler = 1.0, offset = 0.0, duration = 0.0;
  mus_long_t end = 0, dur = 0;
  int npts = 0;
  mus_float_t *brkpts = NULL, *odata = NULL;
  XEN lst;

  keys[0] = kw_envelope;
  keys[1] = kw_scaler;
  keys[2] = kw_duration;
  keys[3] = kw_offset;
  keys[4] = kw_base;
  keys[5] = kw_end;
  keys[6] = kw_length;

  arglist_len = XEN_LIST_LENGTH(arglist);
  if (arglist_len > MAX_ARGLIST_LEN)
    clm_error(S_make_env, "too many args!", arglist);

  for (i = 0; i < arglist_len; i++) args[i] = XEN_LIST_REF(arglist, i);
  for (i = arglist_len; i < MAX_ARGLIST_LEN; i++) args[i] = XEN_UNDEFINED;

  vals = mus_optkey_unscramble(S_make_env, 7, keys, args, orig_arg);
  if (vals > 0)
    {
      scaler = mus_optkey_to_float(keys[1], S_make_env, orig_arg[1], 1.0);

      duration = mus_optkey_to_float(keys[2], S_make_env, orig_arg[2], 0.0);
      if ((duration < 0.0) || ((duration == 0.0) && (!XEN_KEYWORD_P(keys[2]))))
	XEN_OUT_OF_RANGE_ERROR(S_make_env, orig_arg[2], keys[2], "duration ~A <= 0.0?");

      offset = mus_optkey_to_float(keys[3], S_make_env, orig_arg[3], 0.0);

      base = mus_optkey_to_float(keys[4], S_make_env, orig_arg[4], 1.0);
      if (base < 0.0) 
	XEN_OUT_OF_RANGE_ERROR(S_make_env, orig_arg[4], keys[4], "base ~A < 0.0?");

      end = mus_optkey_to_mus_long_t(keys[5], S_make_env, orig_arg[5], 0);
      if (end < 0) 
	XEN_OUT_OF_RANGE_ERROR(S_make_env, orig_arg[5], keys[5], "end ~A < 0?");

      dur = mus_optkey_to_mus_long_t(keys[6], S_make_env, orig_arg[6], 0);
      if (dur < 0) 
	XEN_OUT_OF_RANGE_ERROR(S_make_env, orig_arg[6], keys[6], "dur ~A < 0?");

      /* env data is a list, checked last to let the preceding throw wrong-type error before calloc  */
      if (!(XEN_KEYWORD_P(keys[0])))
        {
	  vct *v = NULL;
	  if (MUS_VCT_P(keys[0]))
	    {
	      v = XEN_TO_VCT(keys[0]);
	      len = v->length;
	      if ((len < 2) || (len & 1))
		XEN_ERROR(BAD_TYPE,
			  XEN_LIST_2(C_TO_XEN_STRING(S_make_env ": vct is a bogus breakpoints list, ~A"), 
				     keys[0]));
	    }
	  else
	    {
	      XEN_ASSERT_TYPE(XEN_LIST_P_WITH_LENGTH(keys[0], len), keys[0], orig_arg[0], S_make_env, "a list");
	      if (len == 0)
		XEN_ERROR(NO_DATA,
			  XEN_LIST_2(C_TO_XEN_STRING(S_make_env ": null env? ~A"), 
				     keys[0]));

	      if (XEN_LIST_P(XEN_CAR(keys[0])))
		len *= 2;
	      else
		{
		  if (len & 1)
		    XEN_ERROR(BAD_TYPE,
			      XEN_LIST_2(C_TO_XEN_STRING(S_make_env ": odd length breakpoints list? ~A"), 
					 keys[0]));

		  if (!(XEN_NUMBER_P(XEN_CAR(keys[0]))))
		    XEN_ASSERT_TYPE(false, keys[0], orig_arg[0], S_make_env, "a list of numbers (breakpoints)");
		}
	    }

	  npts = len / 2;
	  brkpts = (mus_float_t *)calloc(len, sizeof(mus_float_t));
	  if (brkpts == NULL)
	    return(clm_mus_error(MUS_MEMORY_ALLOCATION_FAILED, "can't allocate env list"));
	  odata = (mus_float_t *)calloc(len, sizeof(mus_float_t));
	  if (odata == NULL)
	    return(clm_mus_error(MUS_MEMORY_ALLOCATION_FAILED, "can't allocate env copy"));

	  if (v)
	    memcpy((void *)brkpts, (void *)(v->data), len * sizeof(mus_float_t));
	  else
	    {
	      if (XEN_NUMBER_P(XEN_CAR(keys[0])))
		{
		  for (i = 0, lst = XEN_COPY_ARG(keys[0]); (i < len) && (XEN_NOT_NULL_P(lst)); i++, lst = XEN_CDR(lst))
		    brkpts[i] = XEN_TO_C_DOUBLE_OR_ELSE(XEN_CAR(lst), 0.0);
		}
	      else
		{
		  for (i = 0, lst = XEN_COPY_ARG(keys[0]); (i < len) && (XEN_NOT_NULL_P(lst)); i += 2, lst = XEN_CDR(lst))
		    {
		      XEN el;
		      el = XEN_CAR(lst);
		      if ((XEN_PAIR_P(el)) &&
			  (XEN_NUMBER_P(XEN_CAR(el))) &&
			  (XEN_PAIR_P(XEN_CDR(el))) &&
			  (XEN_NUMBER_P(XEN_CADR(el))))
			{
			  brkpts[i] = XEN_TO_C_DOUBLE(XEN_CAR(el));
			  brkpts[i + 1] = XEN_TO_C_DOUBLE(XEN_CADR(el));
			}
		      else XEN_ERROR(BAD_TYPE, 
				     XEN_LIST_2(C_TO_XEN_STRING(S_make_env ": odd breakpoints list? ~A"), 
						keys[0]));
		    }
		}
	    }
	  memcpy((void *)odata, (void *)brkpts, len * sizeof(mus_float_t));
        }
    }

  if (brkpts == NULL) 
    XEN_ERROR(NO_DATA,
	      XEN_LIST_1(C_TO_XEN_STRING(S_make_env ": no envelope?"))); 

  if (dur > 0)
    {
      if ((end > 0) && ((end + 1) != dur))
	{
	  if (brkpts) {free(brkpts); brkpts = NULL;}
	  if (odata) {free(odata); odata = NULL;}
	  XEN_ERROR(CLM_ERROR,
		    XEN_LIST_3(C_TO_XEN_STRING(S_make_env ": end, ~A, and dur, ~A, specified, but dur != end+1"),
			       keys[5], 
			       keys[6]));
	}
      end = dur - 1;
    }

  {
    mus_error_handler_t *old_error_handler;
    old_error_handler = mus_error_set_handler(local_mus_error);
    ge = mus_make_env(brkpts, npts, scaler, offset, base, duration, end, odata);
    mus_error_set_handler(old_error_handler);
  }

  free(brkpts);
  if (ge) return(mus_xen_to_object(mus_any_to_mus_xen_with_vct(ge, xen_make_vct(mus_env_breakpoints(ge) * 2, odata))));
  free(odata);
  return(clm_mus_error(local_error_type, local_error_msg));
}


static XEN g_env_interp(XEN x, XEN env1) /* "env" causes trouble in Objective-C!! */
{
  #define H_env_interp "(" S_env_interp " x env): value of envelope env at x"
  XEN_ASSERT_TYPE(XEN_NUMBER_P(x), x, XEN_ARG_1, S_env_interp, "a number");
  XEN_ASSERT_TYPE((MUS_XEN_P(env1)) && (mus_env_p(XEN_TO_MUS_ANY(env1))), env1, XEN_ARG_2, S_env_interp, "an env generator");
  return(C_TO_XEN_DOUBLE(mus_env_interp(XEN_TO_C_DOUBLE(x), XEN_TO_MUS_ANY(env1))));
}


#if (!HAVE_NESTED_FUNCTIONS) || __cplusplus

/* mus_env_any calls the C function itself, so we pass it connect_func,
 *   connect_func uses the function passed as an argument to g_env_any.
 *   I can't think of a cleaner way to handle this except via nested functions.
 *   Both versions seem to work ok with recursive env-any calls.
 */

static XEN current_connect_func;

static mus_float_t connect_func(mus_float_t val)
{
  return(XEN_TO_C_DOUBLE(XEN_CALL_1(current_connect_func,
				    C_TO_XEN_DOUBLE(val),
				    S_env_any " connect function")));
}

static XEN g_env_any(XEN e, XEN func)
{
  XEN val;
  XEN old_connect_func = XEN_FALSE;

  #define H_env_any "(" S_env_any " e func) uses 'func' to connect the dots in the env 'e'"
  XEN_ASSERT_TYPE((MUS_XEN_P(e)) && (mus_env_p(XEN_TO_MUS_ANY(e))), e, XEN_ARG_1, S_env_any, "an env generator");
  XEN_ASSERT_TYPE((XEN_PROCEDURE_P(func)) && (XEN_REQUIRED_ARGS_OK(func, 1)), func, XEN_ARG_2, S_env_any, "a function of one arg");
  
  old_connect_func = current_connect_func;
  current_connect_func = func;
  val = C_TO_XEN_DOUBLE(mus_env_any(XEN_TO_MUS_ANY(e), connect_func));
  current_connect_func = old_connect_func;

  return(val);
}

#else

static XEN g_env_any(XEN e, XEN func)
{
  #define H_env_any "(" S_env_any " e func) uses 'func' to connect the dots in the env 'e'"

  auto mus_float_t connect_func(mus_float_t val); 
  /* this is apparently how you declare these nested functions!
   *   without that line, the compiler sez: clm2xen.c:5252: warning: no previous prototype for 'connect_func'
   *   if you try to use "static":          clm2xen.c:5251: error: invalid storage class for function 'connect_func'
   *   if nothing:                          clm2xen.c:5257: error: static declaration of 'connect_func' follows non-static declaration
   * but isn't "auto" simply the default?
   */
  mus_float_t connect_func(mus_float_t val)
  {
    return(XEN_TO_C_DOUBLE(XEN_CALL_1(func,
				      C_TO_XEN_DOUBLE(val),
				      S_env_any " connect function")));
  }

  XEN_ASSERT_TYPE((MUS_XEN_P(e)) && (mus_env_p(XEN_TO_MUS_ANY(e))), e, XEN_ARG_1, S_env_any, "an env generator");
  XEN_ASSERT_TYPE((XEN_PROCEDURE_P(func)) && (XEN_REQUIRED_ARGS_OK(func, 1)), func, XEN_ARG_2, S_env_any, "a function of one arg");
  
  return(C_TO_XEN_DOUBLE(mus_env_any(XEN_TO_MUS_ANY(e), connect_func)));
}

#endif



/* ---------------- io ---------------- */

#if (!HAVE_RUBY)
  #define S_output "*output*"
  #define S_reverb "*reverb*"
#else
  #define S_output "output"
  #define S_reverb "reverb"
#endif

static XEN clm_output, clm_reverb; /* *output* and *reverb* at extlang level -- these can be output streams, vct, sound-data objects etc */

#if (HAVE_SCHEME)
static XEN clm_output_slot = NULL, clm_reverb_slot = NULL;
XEN mus_clm_output(void) {return(s7_slot_value(s7, clm_output_slot));}
XEN mus_clm_reverb(void) {return(s7_slot_value(s7, clm_reverb_slot));}

#define CLM_OUTPUT s7_slot_value(s7, clm_output_slot)
#define CLM_REVERB s7_slot_value(s7, clm_reverb_slot)
#endif

#if (!HAVE_SCHEME)
XEN mus_clm_output(void) {return(XEN_VARIABLE_REF(S_output));}
XEN mus_clm_reverb(void) {return(XEN_VARIABLE_REF(S_reverb));}

#define CLM_OUTPUT XEN_VARIABLE_REF(S_output)
#define CLM_REVERB XEN_VARIABLE_REF(S_reverb)
#endif

static XEN g_input_p(XEN obj) 
{
  #define H_mus_input_p "(" S_mus_input_p " gen): " PROC_TRUE " if gen is an input generator"
  return(C_TO_XEN_BOOLEAN((MUS_XEN_P(obj)) && (mus_input_p(XEN_TO_MUS_ANY(obj)))));
}


static XEN g_output_p(XEN obj) 
{
  #define H_mus_output_p "(" S_mus_output_p " gen): " PROC_TRUE " if gen is an output generator"

  return(C_TO_XEN_BOOLEAN((MUS_XEN_P(obj)) && (mus_output_p(XEN_TO_MUS_ANY(obj)))));
}


static XEN g_file_to_sample_p(XEN obj) 
{
  #define H_file_to_sample_p "(" S_file_to_sample_p " gen): " PROC_TRUE " if gen is a " S_file_to_sample " generator"
  return(C_TO_XEN_BOOLEAN((MUS_XEN_P(obj)) && (mus_file_to_sample_p(XEN_TO_MUS_ANY(obj)))));
}


static XEN g_file_to_frame_p(XEN obj) 
{
  #define H_file_to_frame_p "(" S_file_to_frame_p " gen): " PROC_TRUE " if gen is a " S_file_to_frame " generator"
  return(C_TO_XEN_BOOLEAN((MUS_XEN_P(obj)) && (mus_file_to_frame_p(XEN_TO_MUS_ANY(obj)))));
}


static XEN g_sample_to_file_p(XEN obj) 
{
  #define H_sample_to_file_p "(" S_sample_to_file_p " gen): " PROC_TRUE " if gen is a " S_sample_to_file " generator"

  return(C_TO_XEN_BOOLEAN((MUS_XEN_P(obj)) && (mus_sample_to_file_p(XEN_TO_MUS_ANY(obj)))));
}


static XEN g_frame_to_file_p(XEN obj) 
{
  #define H_frame_to_file_p "(" S_frame_to_file_p " gen): " PROC_TRUE " if gen is a " S_frame_to_file " generator"

  return(C_TO_XEN_BOOLEAN((MUS_XEN_P(obj)) && (mus_frame_to_file_p(XEN_TO_MUS_ANY(obj)))));
}


static XEN g_in_any_1(const char *caller, XEN frame, int in_chan, XEN inp)
{
  mus_long_t pos;

  XEN_ASSERT_TYPE(XEN_INTEGER_P(frame), frame, XEN_ARG_1, caller, "an integer");

  pos = XEN_TO_C_LONG_LONG(frame);
  if (pos < 0) 
    XEN_OUT_OF_RANGE_ERROR(caller, XEN_ARG_1, frame, "must be >= 0");    

  if (in_chan < 0) 
    XEN_OUT_OF_RANGE_ERROR(caller, XEN_ARG_2, C_TO_XEN_INT(in_chan), "must be >= 0");    

  if (MUS_XEN_P(inp))
    {
      XEN_ASSERT_TYPE(mus_input_p(XEN_TO_MUS_ANY(inp)), inp, XEN_ARG_3, caller, "an input generator");
      return(C_TO_XEN_DOUBLE(mus_in_any(pos, in_chan, (mus_any *)XEN_TO_MUS_ANY(inp))));
    }

  if (MUS_VCT_P(inp))
    {
      vct *v;
      v = XEN_TO_VCT(inp);
      if (pos < v->length)
	return(C_TO_XEN_DOUBLE(v->data[pos]));
      return(C_TO_XEN_DOUBLE(0.0));
    }

  if (sound_data_p(inp))
    {
      sound_data *sd;
      sd = XEN_TO_SOUND_DATA(inp);
      if ((in_chan < sd->chans) && 
	  (pos < sd->length))
	return(C_TO_XEN_DOUBLE(sd->data[in_chan][pos]));
      return(C_TO_XEN_DOUBLE(0.0)); /* say *reverb* is inp and we're adding decay time, so run off the end... */
      /* in any case, sound-data obj looks like a procedure, and we don't want to hit that in the next block. */
    }

  if (XEN_VECTOR_P(inp))
    {
      if (pos < XEN_VECTOR_LENGTH(inp))
	return(XEN_VECTOR_REF(inp, pos));
    }

  return(C_TO_XEN_DOUBLE(0.0));
}


static XEN g_in_any(XEN frame, XEN chan, XEN inp) 
{
  #define H_in_any "(" S_in_any " frame chan stream): input stream sample at frame in channel chan"
  XEN_ASSERT_TYPE(XEN_INTEGER_P(chan), chan, XEN_ARG_2, S_in_any, "an integer");
  return(g_in_any_1(S_in_any, frame, XEN_TO_C_INT(chan), inp));
}


static XEN g_ina(XEN frame, XEN inp) 
{
  #define H_ina "(" S_ina " frame stream): input stream sample in channel 0 at frame"
  return(g_in_any_1(S_ina, frame, 0, inp));
}


static XEN g_inb(XEN frame, XEN inp) 
{
  #define H_inb "(" S_inb " frame stream): input stream sample in channel 1 at frame"
  return(g_in_any_1(S_inb, frame, 1, inp));
}


static XEN out_any_2(XEN outp, mus_long_t pos, mus_float_t inv, int chn, const char *caller, XEN val)
{
  if (MUS_XEN_P(outp))
    {
      mus_any *o;
      o = XEN_TO_MUS_ANY(outp);
      XEN_ASSERT_TYPE(mus_output_p(o), outp, XEN_ARG_4, caller, "an output generator");
      mus_out_any(pos, inv, chn, o);
      return(val);
    }

  /* adds to existing -- these have to precede procedure check since vcts/sound-data objects are applicable */
  if (MUS_VCT_P(outp))
    {
      if (chn == 0)
	{
	  vct *v;
	  v = xen_to_vct(outp);
	  if (pos < v->length)
	    v->data[pos] += inv;
	}
      return(val);
    }

  if (sound_data_p(outp))
    {
      sound_data *sd;
      sd = XEN_TO_SOUND_DATA(outp);
      if ((chn < sd->chans) &&
	  (pos < sd->length))
	sd->data[chn][pos] += inv;
      return(val);
    }

  if (XEN_VECTOR_P(outp))
    {
      if (pos < XEN_VECTOR_LENGTH(outp))
	XEN_VECTOR_SET(outp, pos, C_TO_XEN_DOUBLE(XEN_TO_C_DOUBLE(XEN_VECTOR_REF(outp, pos)) + inv));
      /* this doesn't handle multiple channels yet, and can't be used with the run macro.
       *    if I had written s7 30 years ago, this would do the right thing...
       */
    }

  return(val);
}

static XEN g_out_any_1(const char *caller, XEN frame, int chn, XEN val, XEN outp)
{
  mus_long_t pos = 0;
  mus_float_t inv = 0.0;

  if (chn < 0)
    XEN_OUT_OF_RANGE_ERROR(caller, XEN_ARG_3, C_TO_XEN_INT(chn), "must be >= 0");    

  XEN_TO_C_INTEGER_OR_ERROR(frame, pos, caller, XEN_ARG_1);
  if (pos < 0) 
    XEN_OUT_OF_RANGE_ERROR(caller, XEN_ARG_1, frame, "must be >= 0");    

  XEN_TO_C_DOUBLE_OR_ERROR(val, inv, caller, XEN_ARG_2);

  if (XEN_NOT_BOUND_P(outp))
    outp = CLM_OUTPUT;

  return(out_any_2(outp, pos, inv, chn, caller, val));
}

static XEN g_out_any(XEN frame, XEN val, XEN chan, XEN outp)
{
  #define H_out_any "(" S_out_any " frame val chan stream): add val to output stream at frame in channel chan"
  XEN_ASSERT_TYPE(XEN_INTEGER_P(chan), chan, XEN_ARG_3, S_out_any, "an integer");
  return(g_out_any_1(S_out_any, frame, XEN_TO_C_INT(chan), val, outp));
}


static XEN g_outa(XEN frame, XEN val, XEN outp)
{
  #define H_outa "(" S_outa " frame val stream): add val to output stream at frame in channel 0"
  return(g_out_any_1(S_outa, frame, 0, val, outp));
}


static XEN g_outb(XEN frame, XEN val, XEN outp)
{
  #define H_outb "(" S_outb " frame val stream): add val to output stream at frame in channel 1"
  return(g_out_any_1(S_outb, frame, 1, val, outp));
}


static XEN g_outc(XEN frame, XEN val, XEN outp)
{
  #define H_outc "(" S_outc " frame val stream): add val to output stream at frame in channel 2"
  return(g_out_any_1(S_outc, frame, 2, val, outp));
}


static XEN g_outd(XEN frame, XEN val, XEN outp)
{
  #define H_outd "(" S_outd " frame val stream): add val to output stream at frame in channel 3"
  return(g_out_any_1(S_outd, frame, 3, val, outp));
}


static XEN g_mus_close(XEN ptr)
{
  #define H_mus_close "(" S_mus_close " gen): close the IO stream managed by 'gen' (a sample->file generator, for example)"

  if (MUS_XEN_P(ptr))
    return(C_TO_XEN_INT(mus_close_file((mus_any *)XEN_TO_MUS_ANY(ptr))));

  XEN_ASSERT_TYPE(MUS_VCT_P(ptr) || XEN_FALSE_P(ptr) || sound_data_p(ptr) || XEN_VECTOR_P(ptr), 
		  ptr, XEN_ONLY_ARG, S_mus_close, "an IO gen or its outa equivalent");
  return(XEN_ZERO);
}


static XEN g_make_file_to_sample(XEN name, XEN buffer_size)
{
  #define H_make_file_to_sample "(" S_make_file_to_sample " filename buffer-size): return an input generator reading 'filename' (a sound file)"

  mus_any *ge;
  mus_long_t size;

  XEN_ASSERT_TYPE(XEN_STRING_P(name), name, XEN_ARG_1, S_make_file_to_sample, "a string");
  XEN_ASSERT_TYPE(XEN_LONG_LONG_IF_BOUND_P(buffer_size), buffer_size, XEN_ARG_2, S_make_file_to_sample, "an integer");

  if (!(mus_file_probe(XEN_TO_C_STRING(name))))
    XEN_ERROR(NO_SUCH_FILE,
	      XEN_LIST_3(C_TO_XEN_STRING(S_make_file_to_sample ": ~S, ~A"),
			 name,
			 C_TO_XEN_STRING(STRERROR(errno))));

  if (XEN_LONG_LONG_P(buffer_size))
    {
      size = XEN_TO_C_LONG_LONG(buffer_size);
      if (size <= 0)
	XEN_OUT_OF_RANGE_ERROR(S_make_file_to_sample, XEN_ARG_2, buffer_size, "must be > 0");
    }
  else size = mus_file_buffer_size();

  ge = mus_make_file_to_sample_with_buffer_size(XEN_TO_C_STRING(name), size);
  if (ge) return(mus_xen_to_object(mus_any_to_mus_xen(ge)));
  return(XEN_FALSE);
}


static XEN g_file_to_sample(XEN obj, XEN samp, XEN chan)
{
  #define H_file_to_sample "(" S_file_to_sample " obj frame chan): sample value in sound file read by 'obj' in channel chan at frame"
  int channel = 0;
  mus_any *g = NULL;
  XEN_TO_C_GENERATOR(obj, g, mus_input_p, S_file_to_sample, "an input generator");

  XEN_ASSERT_TYPE(XEN_NUMBER_P(samp), samp, XEN_ARG_2, S_file_to_sample, "a number");

  if (XEN_BOUND_P(chan))
    {
      XEN_ASSERT_TYPE(XEN_INTEGER_P(chan), chan, XEN_ARG_3, S_file_to_sample, "an integer");
      channel = XEN_TO_C_INT(chan);
    }
  return(C_TO_XEN_DOUBLE(mus_file_to_sample(g,
					    XEN_TO_C_LONG_LONG_OR_ELSE(samp, 0),
					    channel)));
}


static XEN g_make_sample_to_file(XEN name, XEN chans, XEN out_format, XEN out_type, XEN comment)
{
  #if HAVE_SCHEME
    #define make_sample_to_file_example "(" S_make_sample_to_file " \"test.snd\" 2 mus-lshort mus-riff)"
  #endif
  #if HAVE_RUBY
    #define make_sample_to_file_example "\"test.snd\" 2 Mus_lshort Mus_riff make_sample2file"
  #endif
  #if HAVE_FORTH
    #define make_sample_to_file_example "\"test.snd\" 2 mus-lshort mus-riff make-sample->file"
  #endif

  #define H_make_sample_to_file "(" S_make_sample_to_file " filename chans data-format header-type comment): \
return an output generator writing the sound file 'filename' which is set up to have \
'chans' channels of 'data-format' samples with a header of 'header-type'.  The latter \
should be sndlib identifiers:\n  " make_sample_to_file_example

  int df;

  XEN_ASSERT_TYPE(XEN_STRING_P(name), name, XEN_ARG_1, S_make_sample_to_file, "a string");
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(chans), chans, XEN_ARG_2, S_make_sample_to_file, "an integer");
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(out_format), out_format, XEN_ARG_3, S_make_sample_to_file, "an integer (data format id)");
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(out_type), out_type, XEN_ARG_4, S_make_sample_to_file, "an integer (header type id)");

  df = XEN_TO_C_INT_OR_ELSE(out_format, (int)MUS_OUT_FORMAT);
  if (mus_data_format_p(df))
    {
      int ht;
      ht = XEN_TO_C_INT_OR_ELSE(out_type, (int)MUS_NEXT);
      if (mus_header_type_p(ht))
	{
	  int chns;
	  chns = XEN_TO_C_INT_OR_ELSE(chans, 1);
	  if (chns > 0)
	    {
	      mus_any *rgen;
	      rgen = mus_make_sample_to_file_with_comment(XEN_TO_C_STRING(name),
							  chns, df, ht,
							  (XEN_STRING_P(comment)) ? XEN_TO_C_STRING(comment) : NULL);
	      if (rgen) return(mus_xen_to_object(mus_any_to_mus_xen(rgen)));
	    }
	  else XEN_OUT_OF_RANGE_ERROR(S_make_sample_to_file, 2, chans, "chans ~A <= 0?");
	}
      else XEN_OUT_OF_RANGE_ERROR(S_make_sample_to_file, 4, out_type, "~A: invalid header type");
    }
  else XEN_OUT_OF_RANGE_ERROR(S_make_sample_to_file, 3, out_format, "~A: invalid data format");
  return(XEN_FALSE);
}


static XEN g_continue_sample_to_file(XEN name)
{
  #define H_continue_sample_to_file "(" S_continue_sample_to_file " filename): return an output generator \
that reopens an existing sound file 'filename' ready for output via " S_sample_to_file

  mus_any *rgen = NULL;
  XEN_ASSERT_TYPE(XEN_STRING_P(name), name, XEN_ARG_1, S_continue_sample_to_file, "a string");
  rgen = mus_continue_sample_to_file(XEN_TO_C_STRING(name));
  if (rgen) return(mus_xen_to_object(mus_any_to_mus_xen(rgen)));
  return(XEN_FALSE);
}


static XEN g_sample_to_file(XEN obj, XEN samp, XEN chan, XEN val)
{
  #define H_sample_to_file "(" S_sample_to_file " obj samp chan val): add val to the output stream \
handled by the output generator 'obj', in channel 'chan' at frame 'samp'"
  mus_any *g = NULL;
  XEN_TO_C_GENERATOR(obj, g, mus_output_p, S_sample_to_file, "an output generator");

  XEN_ASSERT_TYPE(XEN_INTEGER_P(samp), samp, XEN_ARG_2, S_sample_to_file, "an integer");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(chan), chan, XEN_ARG_3, S_sample_to_file, "an integer");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ARG_4, S_sample_to_file, "a number");

  mus_sample_to_file(g,
		     XEN_TO_C_LONG_LONG(samp),
		     XEN_TO_C_INT(chan),
		     XEN_TO_C_DOUBLE(val));
  return(val);
}


static XEN g_sample_to_file_add(XEN obj1, XEN obj2)
{
  #define H_sample_to_file_add "(" S_sample_to_file_add " obj1 obj2): mixes obj2 (an output generator) into obj1 (also an output generator)"

  XEN_ASSERT_TYPE((MUS_XEN_P(obj1)) && (mus_output_p(XEN_TO_MUS_ANY(obj1))), obj1, XEN_ARG_1, S_sample_to_file_add, "an output generator");
  XEN_ASSERT_TYPE((MUS_XEN_P(obj2)) && (mus_output_p(XEN_TO_MUS_ANY(obj2))), obj2, XEN_ARG_2, S_sample_to_file_add, "an output generator");

  mus_sample_to_file_add(XEN_TO_MUS_ANY(obj1), XEN_TO_MUS_ANY(obj2));
  return(obj1);
}


static XEN g_make_file_to_frame(XEN name, XEN buffer_size)
{
  #define H_make_file_to_frame "(" S_make_file_to_frame " filename buffer-size): return an input generator reading 'filename' (a sound file)"

  mus_any *ge;
  mus_long_t size;

  XEN_ASSERT_TYPE(XEN_STRING_P(name), name, XEN_ARG_1, S_make_file_to_frame, "a string");
  XEN_ASSERT_TYPE(XEN_LONG_LONG_IF_BOUND_P(buffer_size), buffer_size, XEN_ARG_2, S_make_file_to_frame, "an integer");

  if (!(mus_file_probe(XEN_TO_C_STRING(name))))
    XEN_ERROR(NO_SUCH_FILE,
	      XEN_LIST_3(C_TO_XEN_STRING(S_make_file_to_frame ": ~S, ~A"),
			 name,
			 C_TO_XEN_STRING(STRERROR(errno))));

  if (XEN_LONG_LONG_P(buffer_size))
    {
      size = XEN_TO_C_LONG_LONG(buffer_size);
      if (size <= 0)
	XEN_OUT_OF_RANGE_ERROR(S_make_file_to_frame, XEN_ARG_2, buffer_size, "must be > 0");
    }
  else size = mus_file_buffer_size();
  ge = mus_make_file_to_frame_with_buffer_size(XEN_TO_C_STRING(name), size);
  if (ge) return(mus_xen_to_object(mus_any_to_mus_xen(ge)));
  return(XEN_FALSE);
}


static XEN g_file_to_frame(XEN obj, XEN samp, XEN outfr)
{
  #define H_file_to_frame "(" S_file_to_frame " obj samp outf): frame of samples at frame 'samp' in sound file read by 'obj'"
  mus_any *res = NULL, *nf = NULL;

  XEN_ASSERT_TYPE((MUS_XEN_P(obj)) && (mus_input_p(XEN_TO_MUS_ANY(obj))), obj, XEN_ARG_1, S_file_to_frame, "an input generator");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(samp), samp, XEN_ARG_2, S_file_to_frame, "an integer");

  if ((MUS_XEN_P(outfr)) && 
      (mus_frame_p(XEN_TO_MUS_ANY(outfr)))) 
    res = (mus_any *)XEN_TO_MUS_ANY(outfr);

  nf = mus_file_to_frame(XEN_TO_MUS_ANY(obj), XEN_TO_C_LONG_LONG(samp), res);
  if (res)
    return(outfr);
  return(mus_xen_to_object(mus_any_to_mus_xen_with_vct(nf, xen_make_vct_wrapper(mus_length(nf), mus_data(nf)))));
}


static XEN g_make_frame_to_file(XEN name, XEN chans, XEN out_format, XEN out_type, XEN comment)
{
  #if HAVE_SCHEME
    #define make_frame_to_file_example "(" S_make_frame_to_file " \"test.snd\" 2 mus-lshort mus-riff)"
  #endif
  #if HAVE_RUBY
    #define make_frame_to_file_example "\"test.snd\" 2 Mus_lshort Mus_riff make_frame2file"
  #endif
  #if HAVE_FORTH
    #define make_frame_to_file_example "\"test.snd\" 2 mus-lshort mus-riff make-frame->file"
  #endif

  #define H_make_frame_to_file "(" S_make_frame_to_file " filename chans data-format header-type comment): \
return an output generator writing the sound file 'filename' which is set up to have \
'chans' channels of 'data-format' samples with a header of 'header-type'.  The latter \
should be sndlib identifiers:\n  " make_frame_to_file_example

  mus_any *fgen = NULL;

  XEN_ASSERT_TYPE(XEN_STRING_P(name), name, XEN_ARG_1, S_make_frame_to_file, "a string");
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(chans), chans, XEN_ARG_2, S_make_frame_to_file, "an integer");
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(out_format), out_format, XEN_ARG_3, S_make_frame_to_file, "an integer (data format id)");
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(out_type), out_type, XEN_ARG_4, S_make_frame_to_file, "an integer (header-type id)");

  fgen = mus_make_frame_to_file_with_comment(XEN_TO_C_STRING(name),
					     XEN_TO_C_INT_OR_ELSE(chans, 1),
					     XEN_TO_C_INT_OR_ELSE(out_format, (int)MUS_OUT_FORMAT),
					     XEN_TO_C_INT_OR_ELSE(out_type, (int)MUS_NEXT),
					     (XEN_STRING_P(comment)) ? XEN_TO_C_STRING(comment) : NULL);
  if (fgen) return(mus_xen_to_object(mus_any_to_mus_xen(fgen)));
  return(XEN_FALSE);
}


static XEN g_continue_frame_to_file(XEN name)
{
  #define H_continue_frame_to_file "(" S_continue_frame_to_file " filename): return an output generator \
that reopens an existing sound file 'filename' ready for output via " S_frame_to_file

  mus_any *rgen = NULL;
  XEN_ASSERT_TYPE(XEN_STRING_P(name), name, XEN_ARG_1, S_continue_frame_to_file, "a string");
  rgen = mus_continue_frame_to_file(XEN_TO_C_STRING(name));
  if (rgen) return(mus_xen_to_object(mus_any_to_mus_xen(rgen)));
  return(XEN_FALSE);
}


static XEN g_frame_to_file(XEN obj, XEN samp, XEN val)
{
  #define H_frame_to_file "(" S_frame_to_file " obj samp val): add frame 'val' to the output stream \
handled by the output generator 'obj' at frame 'samp'"

  XEN_ASSERT_TYPE((MUS_XEN_P(obj)) && (mus_output_p(XEN_TO_MUS_ANY(obj))), obj, XEN_ARG_1, S_frame_to_file, "an output generator");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(samp), samp, XEN_ARG_2, S_frame_to_file, "an integer");
  XEN_ASSERT_TYPE((MUS_XEN_P(val)) && (mus_frame_p(XEN_TO_MUS_ANY(val))), val, XEN_ARG_3, S_frame_to_file, "a frame");
  mus_frame_to_file(XEN_TO_MUS_ANY(obj),
		    XEN_TO_C_LONG_LONG(samp),
		    (mus_any *)XEN_TO_MUS_ANY(val));
  return(val);
}


static XEN g_mus_file_buffer_size(void)
{
  #define H_mus_file_buffer_size "(" S_mus_file_buffer_size "): current CLM IO buffer size (default is 8192)"
  return(C_TO_XEN_LONG_LONG(mus_file_buffer_size()));
}


static XEN g_mus_set_file_buffer_size(XEN val)
{
  mus_long_t len;
  XEN_ASSERT_TYPE(XEN_LONG_LONG_P(val), val, XEN_ONLY_ARG, S_setB S_mus_file_buffer_size, "an integer");
  len = XEN_TO_C_LONG_LONG(val);
  if (len <= 0) 
    XEN_OUT_OF_RANGE_ERROR(S_setB S_mus_file_buffer_size, XEN_ONLY_ARG, val, "must be > 0");
  return(C_TO_XEN_LONG_LONG(mus_set_file_buffer_size(len)));
}




/* ---------------- readin ---------------- */

static XEN g_readin_p(XEN obj) 
{
  #define H_readin_p "(" S_readin_p " gen): " PROC_TRUE " if gen is a " S_readin
  return(C_TO_XEN_BOOLEAN((MUS_XEN_P(obj)) && (mus_readin_p(XEN_TO_MUS_ANY(obj)))));
}


static XEN g_readin(XEN obj)
{
  #define H_readin "(" S_readin " gen): next sample from readin generator (a sound file reader)"
  mus_any *g = NULL;
  XEN_TO_C_GENERATOR(obj, g, mus_readin_p, S_readin, "a readin generator");
  return(C_TO_XEN_DOUBLE(mus_readin(g)));
}


static XEN g_make_readin(XEN arglist)
{
  #define H_make_readin "(" S_make_readin " file (channel 0) (start 0) (direction 1) size): \
return a new readin (file input) generator reading the sound file 'file' starting at frame \
'start' in channel 'channel' and reading forward if 'direction' is not -1"

  /* optkey file channel start direction size */
  mus_any *ge;
  const char *file = NULL;
  XEN args[MAX_ARGLIST_LEN]; 
  XEN keys[5];
  int orig_arg[5] = {0, 0, 0, 0, 0};
  int i, vals, arglist_len;
  mus_long_t buffer_size;
  int channel = 0, direction = 1;
  mus_long_t start = 0;

  keys[0] = kw_file;
  keys[1] = kw_channel;
  keys[2] = kw_start;
  keys[3] = kw_direction;
  keys[4] = kw_size;

  buffer_size = mus_file_buffer_size();

  arglist_len = XEN_LIST_LENGTH(arglist);
  if (arglist_len > MAX_ARGLIST_LEN)
    clm_error(S_make_readin, "too many args!", arglist);

  for (i = 0; i < arglist_len; i++) args[i] = XEN_LIST_REF(arglist, i);
  for (i = arglist_len; i < MAX_ARGLIST_LEN; i++) args[i] = XEN_UNDEFINED;

  vals = mus_optkey_unscramble(S_make_readin, 5, keys, args, orig_arg);
  if (vals > 0)
    {
      file = mus_optkey_to_string(keys[0], S_make_readin, orig_arg[0], NULL); /* not copied */

      channel = mus_optkey_to_int(keys[1], S_make_readin, orig_arg[1], channel);
      if (channel < 0)
	XEN_OUT_OF_RANGE_ERROR(S_make_readin, orig_arg[1], keys[1], "channel ~A < 0?");

      start = mus_optkey_to_mus_long_t(keys[2], S_make_readin, orig_arg[2], start);

      direction = mus_optkey_to_int(keys[3], S_make_readin, orig_arg[3], direction);

      buffer_size = mus_optkey_to_mus_long_t(keys[4], S_make_readin, orig_arg[4], buffer_size);
      if (buffer_size <= 0)
	XEN_OUT_OF_RANGE_ERROR(S_make_readin, orig_arg[4], keys[4], "must be > 0");
    }

  if (file == NULL)
    XEN_OUT_OF_RANGE_ERROR(S_make_readin, orig_arg[0], keys[0], "no file name given");
  if (!(mus_file_probe(file)))
    XEN_ERROR(NO_SUCH_FILE,
	      XEN_LIST_3(C_TO_XEN_STRING(S_make_readin ": ~S, ~A"),
			 C_TO_XEN_STRING(file),
			 C_TO_XEN_STRING(STRERROR(errno))));

  if (mus_sound_chans(file) <= 0)
    XEN_ERROR(BAD_HEADER,
	      XEN_LIST_2(C_TO_XEN_STRING(S_make_readin ": ~S chans <= 0?"),
			 C_TO_XEN_STRING(file)));

  if (channel >= mus_sound_chans(file))
    XEN_OUT_OF_RANGE_ERROR(S_make_readin, orig_arg[1], keys[1], "channel ~A > available chans?");

  ge = mus_make_readin_with_buffer_size(file, channel, start, direction, buffer_size);
  if (ge) return(mus_xen_to_object(mus_any_to_mus_xen(ge)));
  return(XEN_FALSE);
}


static XEN g_mus_increment(XEN obj)
{
  #define H_mus_increment "(" S_mus_increment " gen): gen's " S_mus_increment " field, if any"
  if (XEN_LIST_P(obj)) return(call_get_method(obj, S_mus_increment));
  XEN_ASSERT_TYPE(MUS_XEN_P(obj), obj, XEN_ONLY_ARG, S_mus_increment, "a generator");
  return(C_TO_XEN_DOUBLE(mus_increment(XEN_TO_MUS_ANY(obj))));
}


static XEN g_mus_set_increment(XEN obj, XEN val)
{
  if (XEN_LIST_P(obj)) return(call_set_method(obj, val, S_mus_increment));
  XEN_ASSERT_TYPE(MUS_XEN_P(obj), obj, XEN_ARG_1, S_setB S_mus_increment, "a generator");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ARG_2, S_setB S_mus_increment, "a number");
  return(C_TO_XEN_DOUBLE(mus_set_increment(XEN_TO_MUS_ANY(obj), XEN_TO_C_DOUBLE(val))));
}


static XEN g_mus_feedback(XEN obj)
{
  #define H_mus_feedback "(" S_mus_feedback " gen): gen's " S_mus_feedback " field"
  if (XEN_LIST_P(obj)) return(call_get_method(obj, S_mus_feedback));
  XEN_ASSERT_TYPE(MUS_XEN_P(obj), obj, XEN_ONLY_ARG, S_mus_feedback, "a generator");
  return(C_TO_XEN_DOUBLE(mus_feedback(XEN_TO_MUS_ANY(obj))));
}


static XEN g_mus_set_feedback(XEN obj, XEN val)
{
  if (XEN_LIST_P(obj)) return(call_set_method(obj, val, S_mus_feedback));
  XEN_ASSERT_TYPE(MUS_XEN_P(obj), obj, XEN_ARG_1, S_setB S_mus_feedback, "a generator");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ARG_2, S_setB S_mus_feedback, "a number");
  return(C_TO_XEN_DOUBLE(mus_set_feedback(XEN_TO_MUS_ANY(obj), XEN_TO_C_DOUBLE(val))));
}


static XEN g_mus_location(XEN obj)
{
  #define H_mus_location "(" S_mus_location " gen): gen's " S_mus_location " field, if any"
  if (XEN_LIST_P(obj)) return(call_get_method(obj, S_mus_location));
  XEN_ASSERT_TYPE(MUS_XEN_P(obj), obj, XEN_ONLY_ARG, S_mus_location, "a generator");
  return(C_TO_XEN_LONG_LONG(mus_location(XEN_TO_MUS_ANY(obj))));
}


static XEN g_mus_set_location(XEN obj, XEN val)
{
  if (XEN_LIST_P(obj)) return(call_set_method(obj, val, S_mus_location));
  XEN_ASSERT_TYPE(MUS_XEN_P(obj), obj, XEN_ARG_1, S_setB S_mus_location, "a generator");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(val), val, XEN_ARG_2, S_setB S_mus_location, "an integer");
  return(C_TO_XEN_LONG_LONG(mus_set_location(XEN_TO_MUS_ANY(obj), XEN_TO_C_LONG_LONG(val))));
}


static XEN g_mus_channel(XEN obj)
{
  #define H_mus_channel "(" S_mus_channel " gen): gen's " S_mus_channel " field, if any"
  if (XEN_LIST_P(obj)) return(call_get_method(obj, S_mus_channel));
  XEN_ASSERT_TYPE((MUS_XEN_P(obj)) && (mus_input_p(XEN_TO_MUS_ANY(obj))), obj, XEN_ONLY_ARG, S_mus_channel, "an input generator");
  return(C_TO_XEN_INT(mus_channel((mus_any *)XEN_TO_MUS_ANY(obj))));
}


static XEN g_mus_interp_type(XEN obj)
{
  #define H_mus_interp_type "(" S_mus_interp_type " gen): gen's " S_mus_interp_type " field, if any"
  if (XEN_LIST_P(obj)) return(call_get_method(obj, S_mus_interp_type));
  XEN_ASSERT_TYPE(MUS_XEN_P(obj), obj, XEN_ONLY_ARG, S_mus_interp_type, "a generator");
  return(C_TO_XEN_INT(mus_interp_type((mus_any *)XEN_TO_MUS_ANY(obj))));
}




/* ---------------- locsig ---------------- */

static XEN g_locsig_ref(XEN obj, XEN chan)
{
  #define H_locsig_ref "(" S_locsig_ref " gen chan): locsig 'gen' channel 'chan' scaler"
  XEN_ASSERT_TYPE((MUS_XEN_P(obj)) && (mus_locsig_p(XEN_TO_MUS_ANY(obj))), obj, XEN_ARG_1, S_locsig_ref, "a locsig generator");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(chan), chan, XEN_ARG_2, S_locsig_ref, "an integer");
  return(C_TO_XEN_DOUBLE(mus_locsig_ref(XEN_TO_MUS_ANY(obj), XEN_TO_C_INT(chan))));
}


static XEN g_locsig_set(XEN obj, XEN chan, XEN val)
{
  #define H_locsig_set "(" S_locsig_set " gen chan val): set the locsig generator's channel 'chan' scaler to 'val'"
  XEN_ASSERT_TYPE((MUS_XEN_P(obj)) && (mus_locsig_p(XEN_TO_MUS_ANY(obj))), obj, XEN_ARG_1, S_locsig_set, "a locsig generator");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(chan), chan, XEN_ARG_2, S_locsig_set, "an integer");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ARG_3, S_locsig_set, "a number");
  return(C_TO_XEN_DOUBLE(mus_locsig_set(XEN_TO_MUS_ANY(obj),
					XEN_TO_C_INT(chan),
					XEN_TO_C_DOUBLE(val))));
}


static XEN g_locsig_reverb_ref(XEN obj, XEN chan)
{
  #define H_locsig_reverb_ref "(" S_locsig_reverb_ref " gen chan): locsig reverb channel 'chan' scaler"
  XEN_ASSERT_TYPE((MUS_XEN_P(obj)) && (mus_locsig_p(XEN_TO_MUS_ANY(obj))), obj, XEN_ARG_1, S_locsig_reverb_ref, "a locsig generator");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(chan), chan, XEN_ARG_2, S_locsig_reverb_ref, "an integer");
  return(C_TO_XEN_DOUBLE(mus_locsig_reverb_ref(XEN_TO_MUS_ANY(obj), XEN_TO_C_INT(chan))));
}


static XEN g_locsig_reverb_set(XEN obj, XEN chan, XEN val)
{
  #define H_locsig_reverb_set "(" S_locsig_reverb_set " gen chan val): set the locsig reverb channel 'chan' scaler to 'val'"
  XEN_ASSERT_TYPE((MUS_XEN_P(obj)) && (mus_locsig_p(XEN_TO_MUS_ANY(obj))), obj, XEN_ARG_1, S_locsig_reverb_set, "a locsig generator");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(chan), chan, XEN_ARG_2, S_locsig_reverb_set, "an integer");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ARG_3, S_locsig_reverb_set, "a number");
  return(C_TO_XEN_DOUBLE(mus_locsig_reverb_set(XEN_TO_MUS_ANY(obj),
					       XEN_TO_C_INT(chan),
					       XEN_TO_C_DOUBLE(val))));
}


static XEN g_locsig_p(XEN obj)
{
  #define H_locsig_p "(" S_locsig_p " gen): " PROC_TRUE " if gen is a " S_locsig
  return(C_TO_XEN_BOOLEAN((MUS_XEN_P(obj)) && (mus_locsig_p(XEN_TO_MUS_ANY(obj)))));
}


enum {G_LOCSIG_DATA, G_LOCSIG_REVDATA, G_LOCSIG_OUT, G_LOCSIG_REVOUT};

mus_float_t mus_locsig_or_move_sound_to_vct_or_sound_data(mus_xen *ms, mus_any *loc_gen, mus_long_t pos, mus_float_t fval, bool from_locsig)
{
  mus_any *outfr = NULL, *revfr = NULL;
  XEN output, reverb;

  if (from_locsig)
    {
      outfr = mus_locsig_outf(loc_gen);
      revfr = mus_locsig_revf(loc_gen);
    }
  else
    {
      outfr = mus_move_sound_outf(loc_gen);
      revfr = mus_move_sound_revf(loc_gen);
    }
  output = ms->vcts[G_LOCSIG_OUT];

  if (outfr)
    {
      if (MUS_VCT_P(output))
	{
	  vct *v;
	  v = xen_to_vct(output);
	  if (pos < v->length)
	    v->data[pos] += mus_frame_ref(outfr, 0);
	}
      else 
	{
	  if (sound_data_p(output))
	    {
	      sound_data *sd;
	      int i;
	      sd = XEN_TO_SOUND_DATA(output);
	      if (pos < sd->length)
		for (i = 0; i < sd->chans; i++)
		  sd->data[i][pos] += mus_frame_ref(outfr, i);
	    }
	}
    }
  
  if ((revfr) && 
      (XEN_BOUND_P(ms->vcts[G_LOCSIG_REVOUT])))
    {
      reverb = ms->vcts[G_LOCSIG_REVOUT];
      if (MUS_VCT_P(reverb))
	{
	  vct *v;
	  v = xen_to_vct(reverb);
	  if (pos < v->length)
	    v->data[pos] += mus_frame_ref(revfr, 0);
	}
      else 
	{
	  if (sound_data_p(reverb))
	    {
	      sound_data *sd;
	      int i;
	      sd = XEN_TO_SOUND_DATA(reverb);
	      if (pos < sd->length)
		for (i = 0; i < sd->chans; i++)
		  sd->data[i][pos] += mus_frame_ref(revfr, i);
	    }
	}
    }
  return(fval);
}


static XEN g_locsig(XEN xobj, XEN xpos, XEN xval)
{
  #define H_locsig "(" S_locsig " gen loc val): add 'val' to the output of locsig at frame 'loc'"
  mus_any *loc_gen;
  mus_xen *ms;
  mus_long_t pos;
  mus_float_t fval;

  XEN_ASSERT_TYPE(MUS_XEN_P(xobj), xobj, XEN_ARG_1, S_locsig, "a locsig generator");
  ms = XEN_TO_MUS_XEN(xobj);
  loc_gen = (mus_any *)(ms->gen);
  XEN_ASSERT_TYPE(mus_locsig_p(loc_gen), xobj, XEN_ARG_1, S_locsig, "a locsig generator");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(xpos), xpos, XEN_ARG_2, S_locsig, "an integer");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(xval), xval, XEN_ARG_3, S_locsig, "a number");

  pos = XEN_TO_C_LONG_LONG(xpos);
  if (pos < 0) 
    XEN_OUT_OF_RANGE_ERROR(S_locsig, XEN_ARG_2, xpos, "must be >= 0");    
  fval = XEN_TO_C_DOUBLE(xval);
  mus_locsig(loc_gen, pos, fval);

  /* now check for vct/sound-data special cases */
  if (ms->nvcts == 4) 
    mus_locsig_or_move_sound_to_vct_or_sound_data(ms, loc_gen, pos, fval, true);

  return(xval);  /* changed 30-June-06 to return val rather than a wrapped frame */
}

static mus_interp_t clm_locsig_type = MUS_INTERP_LINEAR;

static XEN g_locsig_type(void)
{
  #define H_locsig_type "(" S_locsig_type "): locsig interpolation type, either " S_mus_interp_linear " or " S_mus_interp_sinusoidal "."
  return(C_TO_XEN_INT((int)clm_locsig_type));
}


static XEN g_set_locsig_type(XEN val)
{
  mus_interp_t newval;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(val), val, XEN_ONLY_ARG, S_locsig_type, S_mus_interp_linear " or " S_mus_interp_sinusoidal);
  newval = (mus_interp_t)XEN_TO_C_INT(val);
  if ((newval == MUS_INTERP_LINEAR) || (newval == MUS_INTERP_SINUSOIDAL))
    clm_locsig_type = newval;
  return(C_TO_XEN_INT((int)clm_locsig_type));
}


static XEN g_make_locsig(XEN arglist)
{
  #define H_make_locsig "(" S_make_locsig " (degree 0.0) (distance 1.0) (reverb 0.0) (output *output*) (revout *reverb*) (channels (mus-channels *output*)) (type " S_mus_interp_linear ")): \
return a new generator for signal placement in n channels.  Channel 0 corresponds to 0 degrees."

  mus_xen *gn;
  mus_any *ge;
  mus_any *outp = NULL, *revp = NULL;
  XEN args[MAX_ARGLIST_LEN]; 
  XEN keys[7];
  XEN ov = XEN_UNDEFINED, rv = XEN_UNDEFINED;
  XEN keys3 = XEN_UNDEFINED, keys4 = XEN_UNDEFINED;
  int orig_arg[7] = {0, 0, 0, 0, 0, 0, 0};
  int vals, i, arglist_len, out_chans = -1, rev_chans = -1;
  mus_interp_t type;
  mus_float_t degree = 0.0, distance = 1.0, reverb = 0.0;

  type = clm_locsig_type;

  keys[0] = kw_degree;
  keys[1] = kw_distance;
  keys[2] = kw_reverb;
  keys[3] = kw_output;  
  keys[4] = kw_revout;
  keys[5] = kw_channels;
  keys[6] = kw_type;

  arglist_len = XEN_LIST_LENGTH(arglist);
  if (arglist_len > MAX_ARGLIST_LEN)
    clm_error(S_make_locsig, "too many args!", arglist);

  for (i = 0; i < arglist_len; i++) args[i] = XEN_LIST_REF(arglist, i);
  for (i = arglist_len; i < MAX_ARGLIST_LEN; i++) args[i] = XEN_UNDEFINED;

  vals = mus_optkey_unscramble(S_make_locsig, 7, keys, args, orig_arg);
  if (vals > 0)
    {
      degree = mus_optkey_to_float(keys[0], S_make_locsig, orig_arg[0], degree);
      distance = mus_optkey_to_float(keys[1], S_make_locsig, orig_arg[1], distance);
      reverb = mus_optkey_to_float(keys[2], S_make_locsig, orig_arg[2], reverb);

      if (!(XEN_KEYWORD_P(keys[3])))
	keys3 = keys[3];

      if (!(XEN_KEYWORD_P(keys[4])))
	keys4 = keys[4];

      if (!(XEN_KEYWORD_P(keys[5])))
	{
	  XEN_ASSERT_TYPE(XEN_INTEGER_P(keys[5]), keys[5], orig_arg[5], S_make_locsig, "an integer");
	  out_chans = XEN_TO_C_INT(keys[5]);
	  if (out_chans < 0) 
	    XEN_OUT_OF_RANGE_ERROR(S_make_locsig, orig_arg[5], keys[5], "chans ~A < 0?");
	  if (out_chans > mus_max_table_size()) 
	    XEN_OUT_OF_RANGE_ERROR(S_make_locsig, orig_arg[5], keys[5], "chans = ~A?");
	}

      type = (mus_interp_t)mus_optkey_to_int(keys[6], S_make_locsig, orig_arg[6], type);
      if ((type != MUS_INTERP_LINEAR) && (type != MUS_INTERP_SINUSOIDAL))
	XEN_OUT_OF_RANGE_ERROR(S_make_locsig, orig_arg[6], keys[6], "type ~A must be " S_mus_interp_linear " or " S_mus_interp_sinusoidal ".");
    }

  if (XEN_NOT_BOUND_P(keys3))
    keys3 = CLM_OUTPUT;

  if (XEN_NOT_BOUND_P(keys4))
    keys4 = CLM_REVERB;

  /* try to default output to *output* and reverb to *reverb*, if they're currently set and not closed */
  /*   mus_close is actually mus_close_file = sample_to_file_end = free and nullify obufs so we're hoping dynamic-wind works... */

  if ((MUS_XEN_P(keys3)) && 
      (mus_output_p(XEN_TO_MUS_ANY(keys3))))
    {
      outp = (mus_any *)XEN_TO_MUS_ANY(keys3);
      if (out_chans < 0) 
	out_chans = mus_channels((mus_any *)outp);
    }
  else
    {
      if (MUS_VCT_P(keys3))
	ov = keys3;
      else
	{
	  if (sound_data_p(keys3))
	    {
	      ov = keys3;
	      if (out_chans < 0) 
		out_chans = (XEN_TO_SOUND_DATA(ov))->chans;
	    }
	  else XEN_ASSERT_TYPE(XEN_KEYWORD_P(keys[3]) || XEN_FALSE_P(keys[3]), keys[3], orig_arg[3], S_make_locsig, "an output gen, vct, vector, or a sound-data object");
	}
    }

  if ((MUS_XEN_P(keys4)) && 
      (mus_output_p(XEN_TO_MUS_ANY(keys4))))
    {
      revp = (mus_any *)XEN_TO_MUS_ANY(keys4);
      if (rev_chans < 0)
	rev_chans = mus_channels((mus_any *)revp);
    }
  else
    {
      if (MUS_VCT_P(keys4))
	{
	  rv = keys4;
	  rev_chans = 1;
	}
      else
	{
	  if (sound_data_p(keys4))
	    {
	      rv = keys4;
	      if (rev_chans < 0)
		rev_chans = (XEN_TO_SOUND_DATA(rv))->chans;
	    }
	  else XEN_ASSERT_TYPE(XEN_KEYWORD_P(keys[4]) || XEN_FALSE_P(keys[4]), keys[4], orig_arg[4], S_make_locsig, "a reverb output generator");
	}
    }

  if (out_chans < 0) out_chans = 1;
  if (rev_chans < 0) rev_chans = 0;

  ge = mus_make_locsig(degree, distance, reverb, out_chans, outp, rev_chans, revp, type);

  if (ge)
    {
      gn = (mus_xen *)calloc(1, sizeof(mus_xen));

      if ((XEN_BOUND_P(ov)) || (XEN_BOUND_P(rv)))
	gn->nvcts = 4;
      else gn->nvcts = 2;
      gn->vcts = make_vcts(gn->nvcts);

      /* these two are for the mus-data and mus-xcoeffs methods in Scheme (etc) = MUS_DATA_WRAPPER and G_FILTER_XCOEFFS */
      if (out_chans > 0)
	gn->vcts[G_LOCSIG_DATA] = xen_make_vct_wrapper(out_chans, mus_data(ge));
      else gn->vcts[G_LOCSIG_DATA] = XEN_UNDEFINED;
      if (rev_chans > 0)
	gn->vcts[G_LOCSIG_REVDATA] = xen_make_vct_wrapper(rev_chans, mus_xcoeffs(ge));
      else gn->vcts[G_LOCSIG_REVDATA] = XEN_UNDEFINED;

      if (gn->nvcts == 4)
	{
	  gn->vcts[G_LOCSIG_OUT] = ov;
	  gn->vcts[G_LOCSIG_REVOUT] = rv;
	  mus_set_environ(ge, (void *)gn);
	  mus_locsig_function_reset((mus_any *)ge);
	}

      gn->gen = ge;
      return(mus_xen_to_object(gn));
    }
  return(XEN_FALSE);
}


XEN g_mus_channels(XEN obj)
{
  #define H_mus_channels "(" S_mus_channels " gen): gen's " S_mus_channels " field, if any"

  if (XEN_LIST_P(obj)) return(call_get_method(obj, S_mus_channels));

  if (MUS_XEN_P(obj))
    return(C_TO_XEN_INT(mus_channels(XEN_TO_MUS_ANY(obj))));

  if (MUS_VCT_P(obj))
    return(C_TO_XEN_INT(1));

  if (sound_data_p(obj))
    return(C_TO_XEN_INT((XEN_TO_SOUND_DATA(obj))->chans));

  XEN_ASSERT_TYPE(false, obj, XEN_ONLY_ARG, S_mus_channels, "an output generator, vct, or sound-data object");
  return(XEN_FALSE); /* make compiler happy */
}


static XEN g_move_locsig(XEN obj, XEN degree, XEN distance)
{
  #define H_move_locsig "(" S_move_locsig " gen degree distance): move locsig gen to reflect degree and distance"
  XEN_ASSERT_TYPE((MUS_XEN_P(obj)) && (mus_locsig_p(XEN_TO_MUS_ANY(obj))), obj, XEN_ARG_1, S_move_locsig, "a locsig generator");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(degree), degree, XEN_ARG_2, S_move_locsig, "a number in degrees");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(distance), distance, XEN_ARG_3, S_move_locsig, "a number > 1.0");
  mus_move_locsig(XEN_TO_MUS_ANY(obj),
		  XEN_TO_C_DOUBLE(degree),
		  XEN_TO_C_DOUBLE(distance));
  return(obj);
}




/* ---------------- move-sound ---------------- */

static XEN g_move_sound_p(XEN obj)
{
  #define H_move_sound_p "(" S_move_sound_p " gen): " PROC_TRUE " if gen is a " S_move_sound
  return(C_TO_XEN_BOOLEAN((MUS_XEN_P(obj)) && (mus_move_sound_p(XEN_TO_MUS_ANY(obj)))));
}


static XEN g_move_sound(XEN obj, XEN loc, XEN val)
{
  #define H_move_sound "(" S_move_sound " gen loc val): dlocsig run-time generator handling 'val' at sample 'loc'"
  mus_any *move_gen;
  mus_xen *ms;
  mus_long_t pos;
  mus_float_t fval;

  XEN_ASSERT_TYPE(MUS_XEN_P(obj), obj, XEN_ARG_1, S_move_sound, "a move-sound generator");
  ms = XEN_TO_MUS_XEN(obj);
  move_gen = (mus_any *)(ms->gen);

  XEN_ASSERT_TYPE(mus_move_sound_p(move_gen), obj, XEN_ARG_1, S_move_sound, "a move-sound generator");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(loc), loc, XEN_ARG_2, S_move_sound, "an integer");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ARG_3, S_move_sound, "a number");

  pos = XEN_TO_C_LONG_LONG(loc);
  if (pos < 0) 
    XEN_OUT_OF_RANGE_ERROR(S_move_sound, XEN_ARG_2, loc, "must be >= 0");    
  fval = XEN_TO_C_DOUBLE(val);

  mus_move_sound(move_gen, pos, fval);

  /* now check for vct/sound-data special cases */
  if (ms->nvcts == 4) mus_locsig_or_move_sound_to_vct_or_sound_data(ms, move_gen, pos, fval, false);

  return(val);
}


static mus_any **xen_vector_to_mus_any_array(XEN vect)
{
  mus_any **gens;
  mus_long_t i, len;

  if (!(XEN_VECTOR_P(vect))) return(NULL);
  len = XEN_VECTOR_LENGTH(vect);
  gens = (mus_any **)calloc(len, sizeof(mus_any *));

  for (i = 0; i < len; i++)
    if (MUS_XEN_P(XEN_VECTOR_REF(vect, i)))
      gens[i] = XEN_TO_MUS_ANY(XEN_VECTOR_REF(vect, i));
  return(gens);
}


static int *xen_vector_to_int_array(XEN vect)
{
  int *vals;
  mus_long_t i, len;

  len = XEN_VECTOR_LENGTH(vect);
  vals = (int *)calloc(len, sizeof(int));

  for (i = 0; i < len; i++)
    vals[i] = XEN_TO_C_INT(XEN_VECTOR_REF(vect, i));
  return(vals);
}


static XEN g_make_move_sound(XEN dloc_list, XEN outp, XEN revp)
{
  XEN ov = XEN_UNDEFINED, rv = XEN_UNDEFINED;
  mus_xen *gn;
  mus_any *ge, *dopdly, *dopenv, *globrevenv = NULL, *output = NULL, *revput = NULL;
  mus_any **out_delays, **out_envs, **rev_envs;
  int *out_map;
  mus_long_t start, end;
  int outchans = 0, revchans = 0;
  XEN ref;

  #define H_make_move_sound "(" S_make_move_sound " dloc-list (out *output*) (rev *reverb*)): make a dlocsig run-time generator"

  /* dloc-list is (list start end outchans revchans dopdly dopenv revenv outdelays outenvs revenvs outmap) */
  /*   outdelays envs and revenvs are vectors */

  XEN_ASSERT_TYPE(XEN_LIST_P(dloc_list) && (XEN_LIST_LENGTH(dloc_list) == 11), dloc_list, XEN_ARG_1, S_make_move_sound, "a dlocsig list");

  if (XEN_NOT_BOUND_P(outp))
    outp = CLM_OUTPUT;

  if (XEN_NOT_BOUND_P(revp))
    revp = CLM_REVERB;

  if (MUS_XEN_P(outp))
    {
      output = XEN_TO_MUS_ANY(outp);
      XEN_ASSERT_TYPE(mus_output_p(output), outp, XEN_ARG_2, S_make_move_sound, "output stream");
    }
  else
    {
      if ((MUS_VCT_P(outp)) || 
	  (sound_data_p(outp)) || 
	  (XEN_FALSE_P(outp)) || 
	  (XEN_NOT_BOUND_P(outp)))
	ov = outp;
      else XEN_ASSERT_TYPE(false, outp, XEN_ARG_2, S_make_move_sound, "output stream, vct, or a sound-data object");
    }

  if (MUS_XEN_P(revp))
    {
      revput = XEN_TO_MUS_ANY(revp);
      XEN_ASSERT_TYPE(mus_output_p(revput), revp, XEN_ARG_3, S_make_move_sound, "reverb stream");
    }
  else
    {
      if ((MUS_VCT_P(revp)) || 
	  (sound_data_p(revp)) || 
	  (XEN_FALSE_P(revp)) || 
	  (XEN_NOT_BOUND_P(revp)))
	rv = revp;
      else XEN_ASSERT_TYPE(false, revp, XEN_ARG_3, S_make_move_sound, "reverb stream, vct, or a sound-data object");
    }

  ref = XEN_LIST_REF(dloc_list, 0);
  XEN_ASSERT_TYPE(XEN_LONG_LONG_P(ref), ref, XEN_ARG_1, S_make_move_sound, "dlocsig list[0] (start): a sample number");
  start = XEN_TO_C_LONG_LONG(ref);

  ref = XEN_LIST_REF(dloc_list, 1);
  XEN_ASSERT_TYPE(XEN_LONG_LONG_P(ref), ref, XEN_ARG_1, S_make_move_sound, "dlocsig list[1] (end): a sample number");
  end = XEN_TO_C_LONG_LONG(ref);

  ref = XEN_LIST_REF(dloc_list, 2);
  XEN_ASSERT_TYPE(XEN_INTEGER_P(ref), ref, XEN_ARG_1, S_make_move_sound, "dlocsig list[2] (outchans): an integer");
  outchans = XEN_TO_C_INT(ref);

  ref = XEN_LIST_REF(dloc_list, 3);
  XEN_ASSERT_TYPE(XEN_INTEGER_P(ref), ref, XEN_ARG_1, S_make_move_sound, "dlocsig list[3] (revchans): an integer");
  revchans = XEN_TO_C_INT(ref);

  ref = XEN_LIST_REF(dloc_list, 4);
  XEN_ASSERT_TYPE(MUS_XEN_P(ref), ref, XEN_ARG_1, S_make_move_sound, "dlocsig list[4] (doppler delay): a delay generator");
  dopdly = XEN_TO_MUS_ANY(ref);
  XEN_ASSERT_TYPE(mus_delay_p(dopdly), ref, XEN_ARG_1, S_make_move_sound, "dlocsig list[4] (doppler delay): a delay generator");

  ref = XEN_LIST_REF(dloc_list, 5);
  XEN_ASSERT_TYPE(MUS_XEN_P(ref), ref, XEN_ARG_1, S_make_move_sound, "dlocsig list[5] (doppler env): an env generator");
  dopenv = XEN_TO_MUS_ANY(ref);
  XEN_ASSERT_TYPE(mus_env_p(dopenv), ref, XEN_ARG_1, S_make_move_sound, "dlocsig list[5] (doppler env): an env generator");

  ref = XEN_LIST_REF(dloc_list, 6);
  XEN_ASSERT_TYPE(XEN_FALSE_P(ref) || MUS_XEN_P(ref), ref, XEN_ARG_1, S_make_move_sound, "dlocsig list[6] (global rev env): an env generator");
  if (MUS_XEN_P(ref))
    {
      globrevenv = XEN_TO_MUS_ANY(ref);
      XEN_ASSERT_TYPE(mus_env_p(globrevenv), ref, XEN_ARG_1, S_make_move_sound, "dlocsig list[6] (global rev env): an env generator");
    }

  ref = XEN_LIST_REF(dloc_list, 7);
  XEN_ASSERT_TYPE(XEN_VECTOR_P(ref) && ((int)XEN_VECTOR_LENGTH(ref) >= outchans), 
		  ref, XEN_ARG_1, S_make_move_sound, "dlocsig list[7] (out delays): a vector of delay gens");

  ref = XEN_LIST_REF(dloc_list, 8);
  XEN_ASSERT_TYPE(XEN_FALSE_P(ref) || (XEN_VECTOR_P(ref) && ((int)XEN_VECTOR_LENGTH(ref) >= outchans)), 
		  ref, XEN_ARG_1, S_make_move_sound, "dlocsig list[8] (out envs): " PROC_FALSE " or a vector of envs");

  ref = XEN_LIST_REF(dloc_list, 9);
  XEN_ASSERT_TYPE(XEN_FALSE_P(ref) || (XEN_VECTOR_P(ref) && ((int)XEN_VECTOR_LENGTH(ref) >= revchans)), 
		  ref, XEN_ARG_1, S_make_move_sound, "dlocsig list[9] (rev envs): " PROC_FALSE " or a vector of envs");

  ref = XEN_LIST_REF(dloc_list, 10);
  XEN_ASSERT_TYPE(XEN_VECTOR_P(ref) && ((int)XEN_VECTOR_LENGTH(ref) >= outchans), 
		  ref, XEN_ARG_1, S_make_move_sound, "dlocsig list[10] (out map): vector of ints");

  /* put off allocation until all type error checks are done */

  out_delays = xen_vector_to_mus_any_array(XEN_LIST_REF(dloc_list, 7));
  out_envs = xen_vector_to_mus_any_array(XEN_LIST_REF(dloc_list, 8));
  rev_envs = xen_vector_to_mus_any_array(XEN_LIST_REF(dloc_list, 9));
  out_map = xen_vector_to_int_array(XEN_LIST_REF(dloc_list, 10));

  ge = mus_make_move_sound(start, end, outchans, revchans,
			   dopdly, dopenv, globrevenv,
			   out_delays, out_envs, rev_envs, out_map,
			   output, revput,
			   true, false);                  /* free outer arrays but not gens */
  if (ge)
    {
      gn = (mus_xen *)calloc(1, sizeof(mus_xen));
      if ((XEN_BOUND_P(ov)) || (XEN_BOUND_P(rv)))
	gn->nvcts = 4;
      else gn->nvcts = 1;
      gn->vcts = make_vcts(gn->nvcts);
      gn->vcts[G_LOCSIG_DATA] = dloc_list; /* it is crucial that the list be gc-protected! */
      if (gn->nvcts == 4)
	{
	  gn->vcts[G_LOCSIG_OUT] = ov;
	  gn->vcts[G_LOCSIG_REVOUT] = rv;
	  mus_set_environ(ge, (void *)gn);
	}
      gn->gen = ge;
      return(mus_xen_to_object(gn));
    }

  return(XEN_FALSE);
}




/* ---------------- src ---------------- */

static XEN xen_one, xen_minus_one;
#if HAVE_SCHEME
  static XEN as_needed_arglist;
/* I guess these functions can be called recursively -- maybe we need a list of these?
 */
#endif

static mus_float_t as_needed_input_func(void *ptr, int direction) /* intended for "as-needed" input funcs */
{
  /* if this is called, it's a callback from C, where ptr is a mus_xen object whose vcts[0]
   * field is a XEN procedure to be called, the result being returned back to C.  In the
   * Scheme world, it's a procedure of one arg, the current read direction: 
   *
   * as_needed_input_func is input-func for clm.c make args, or the 2nd arg to the gen (mus_src(gen, input))
   *    it is called in C (*input)(closure, dir)
   *    closure in mus_xen *gn
   *      its gn->vcts array [MUS_INPUT_FUNCTION] = scm procedure object (if any) else EMPTY_LIST
   *      this is set in the gen call if it's passed there, else in the make-gen call
   * so we get here via *as_needed_input_func(gn, dir)
   *   and make sure gn->vcts[MUS_INPUT_FUNCTION] is a procedure, call it with dir as its arg,
   *   it returns a float which we then return to C
   */
  mus_xen *gn = (mus_xen *)ptr;
  if ((gn) && 
      (gn->vcts) && 
      (XEN_BOUND_P(gn->vcts[MUS_INPUT_FUNCTION])) && 
      (XEN_PROCEDURE_P(gn->vcts[MUS_INPUT_FUNCTION])))
#if HAVE_SCHEME
    {
      s7_set_car(as_needed_arglist, (direction == 1) ? xen_one : xen_minus_one);
      return(XEN_TO_C_DOUBLE_OR_ELSE(s7_call_with_location(s7, gn->vcts[MUS_INPUT_FUNCTION], as_needed_arglist, c__FUNCTION__, __FILE__, __LINE__), 0.0));
    }
#else
    return(XEN_TO_C_DOUBLE_OR_ELSE(XEN_CALL_1_NO_CATCH(gn->vcts[MUS_INPUT_FUNCTION], (direction == 1) ? xen_one : xen_minus_one), 0.0));
#endif
  /* the "or else" is crucial here -- this can be called unprotected in clm.c make_src during setup, and
   *   an uncaught error there clobbers our local error chain.
   */
  else return(0.0);
}


static mus_float_t as_needed_input_generator(void *ptr, int direction) /* intended for "as-needed" input funcs */
{
  mus_xen *x = (mus_xen *)ptr;
  XEN v;
  v = x->vcts[MUS_INPUT_FUNCTION];
  return(MUS_RUN(XEN_TO_MUS_ANY(v), 0.0, 0.0));
}


static XEN g_mus_clear_sincs(void)
{
  mus_clear_sinc_tables();
  return(XEN_FALSE);
}


static XEN g_src_p(XEN obj) 
{
  #define H_src_p "(" S_src_p " gen): " PROC_TRUE " if gen is an " S_src
  return(C_TO_XEN_BOOLEAN((MUS_XEN_P(obj)) && (mus_src_p(XEN_TO_MUS_ANY(obj)))));
}


#define SRC_CHANGE_MAX 1000000.0

static XEN g_src(XEN obj, XEN pm, XEN func) 
{
  #define H_src "(" S_src " gen (pm 0.0) input-function): next sampling rate conversion sample. \
'pm' can be used to change the sampling rate on a sample-by-sample basis.  'input-function' \
is a function of one argument (the current input direction, normally ignored) that is called \
internally whenever a new sample of input data is needed.  If the associated " S_make_src " \
included an 'input' argument, input-function is ignored."

  mus_float_t pm1 = 0.0;
  mus_xen *gn;
  mus_any *g = NULL;
  XEN_TO_C_GENERATOR(obj, g, mus_src_p, S_src, "an src generator");

  gn = XEN_TO_MUS_XEN(obj);
  XEN_TO_C_DOUBLE_IF_BOUND(pm, pm1, S_src, XEN_ARG_2);
  /* if sr_change (pm1) is ridiculous, complain! */
  if ((pm1 > SRC_CHANGE_MAX) || (pm1 < -SRC_CHANGE_MAX))
    XEN_OUT_OF_RANGE_ERROR(S_src, XEN_ARG_2, pm, "src change ~A too large");

  if (XEN_PROCEDURE_P(func))
    {
      if (XEN_REQUIRED_ARGS_OK(func, 1))
	gn->vcts[MUS_INPUT_FUNCTION] = func;
      else XEN_BAD_ARITY_ERROR(S_src, 3, func, "src input function wants 1 arg");
    }
  return(C_TO_XEN_DOUBLE(mus_src(g, pm1, NULL)));
}


static XEN g_make_src(XEN arg1, XEN arg2, XEN arg3, XEN arg4, XEN arg5, XEN arg6)
{
  #define H_make_src "(" S_make_src " input (srate 1.0) (width 10)): \
return a new sampling-rate conversion generator (using 'warped sinc interpolation'). \
'srate' is the ratio between the new rate and the old. 'width' is the sine \
width (effectively the steepness of the low-pass filter), normally between 10 and 100. \
'input' if given is an open file stream."

  XEN in_obj = XEN_UNDEFINED;
  mus_xen *gn;
  mus_any *ge;
  int vals, wid = 0; /* 0 here picks up the current default width in clm.c */
  XEN args[6]; 
  XEN keys[3];
  int orig_arg[3] = {0, 0, 0};
  mus_float_t srate = 1.0;

  keys[0] = kw_input;
  keys[1] = kw_srate;
  keys[2] = kw_width;
  args[0] = arg1; args[1] = arg2; args[2] = arg3; args[3] = arg4; args[4] = arg5; args[5] = arg6;

  vals = mus_optkey_unscramble(S_make_src, 3, keys, args, orig_arg);
  if (vals > 0)
    {
      in_obj = mus_optkey_to_input_procedure(keys[0], S_make_src, orig_arg[0], XEN_UNDEFINED, 1, "src input procedure takes 1 arg");

      srate = mus_optkey_to_float(keys[1], S_make_src, orig_arg[1], srate);
      /* srate can be negative => read in reverse */

      wid = mus_optkey_to_int(keys[2], S_make_src, orig_arg[2], wid);
      if (wid < 0) 
	XEN_OUT_OF_RANGE_ERROR(S_make_src, orig_arg[2], keys[2], "width ~A < 0?");
      if (wid > 2000) 
	XEN_OUT_OF_RANGE_ERROR(S_make_src, orig_arg[2], keys[2], "width ~A?");
    }

  gn = (mus_xen *)calloc(1, sizeof(mus_xen));
  /* mus_make_src assumes it can invoke the input function! */
  gn->nvcts = MUS_MAX_VCTS;
  gn->vcts = make_vcts(gn->nvcts);
  gn->vcts[MUS_INPUT_FUNCTION] = in_obj;

  {
    mus_error_handler_t *old_error_handler;
    old_error_handler = mus_error_set_handler(local_mus_error);
    ge = mus_make_src((MUS_XEN_P(in_obj)) ? as_needed_input_generator : as_needed_input_func, srate, wid, gn);
    mus_error_set_handler(old_error_handler);
  }

  if (ge)
    {
      gn->gen = ge;
      return(mus_xen_to_object(gn));
    }

  free(gn->vcts);
  free(gn);
  return(clm_mus_error(local_error_type, local_error_msg));
}




/* ---------------- granulate ---------------- */

static XEN g_granulate_p(XEN obj) 
{
  #define H_granulate_p "(" S_granulate_p " gen): " PROC_TRUE " if gen is a " S_granulate " generator"
  return(C_TO_XEN_BOOLEAN((MUS_XEN_P(obj)) && (mus_granulate_p(XEN_TO_MUS_ANY(obj)))));
}


static int grnedit(void *ptr)
{
  mus_xen *gn = (mus_xen *)ptr;
  return(XEN_TO_C_INT(XEN_CALL_1_NO_CATCH(gn->vcts[MUS_EDIT_FUNCTION], gn->vcts[MUS_SELF_WRAPPER])));
}


static XEN g_granulate(XEN obj, XEN func, XEN edit_func) 
{
  #define H_granulate "(" S_granulate " gen input-func edit-func): next sample from granular synthesis generator"
  mus_xen *gn;
  mus_any *g = NULL;
  XEN_TO_C_GENERATOR(obj, g, mus_granulate_p, S_granulate, "a granulate generator");

  gn = XEN_TO_MUS_XEN(obj);
  if (XEN_BOUND_P(func))
    {
      if (XEN_PROCEDURE_P(func))
	{
	  if (XEN_REQUIRED_ARGS_OK(func, 1))
	    gn->vcts[MUS_INPUT_FUNCTION] = func;
	  else XEN_BAD_ARITY_ERROR(S_granulate, 2, func, "granulate input function wants 1 arg");
	}
      if (XEN_PROCEDURE_P(edit_func))
	{
	  if (XEN_REQUIRED_ARGS_OK(edit_func, 1))
	    {
	      if (!(XEN_BOUND_P(gn->vcts[MUS_EDIT_FUNCTION]))) /* default value is XEN_UNDEFINED */
		{
		  mus_granulate_set_edit_function(gn->gen, grnedit);
		  gn->vcts[MUS_EDIT_FUNCTION] = edit_func;
		}
	    }
	  else XEN_BAD_ARITY_ERROR(S_granulate, 3, edit_func, "granulate edit function wants 1 arg");
	}
    }
  return(C_TO_XEN_DOUBLE(mus_granulate(g, NULL)));
}


static XEN g_mus_ramp(XEN obj)
{
  #define H_mus_ramp "(" S_mus_ramp " gen): granulate generator's " S_mus_ramp " field"
  if (XEN_LIST_P(obj)) return(call_get_method(obj, S_mus_ramp));
  XEN_ASSERT_TYPE((MUS_XEN_P(obj)) && (mus_granulate_p(XEN_TO_MUS_ANY(obj))), obj, XEN_ONLY_ARG, S_mus_ramp, "a granulate generator");
  return(C_TO_XEN_LONG_LONG(mus_ramp(XEN_TO_MUS_ANY(obj))));
}


static XEN g_mus_set_ramp(XEN obj, XEN val)
{
  if (XEN_LIST_P(obj)) return(call_set_method(obj, val, S_mus_ramp));
  XEN_ASSERT_TYPE((MUS_XEN_P(obj)) && (mus_granulate_p(XEN_TO_MUS_ANY(obj))), obj, XEN_ARG_1, S_setB S_mus_ramp, "a granulate generator");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(val), val, XEN_ARG_2, S_setB S_mus_ramp, "an integer");
  return(C_TO_XEN_LONG_LONG(mus_set_ramp(XEN_TO_MUS_ANY(obj), XEN_TO_C_LONG_LONG(val))));
}


static XEN g_make_granulate(XEN arglist)
{
  #define H_make_granulate "(" S_make_granulate " input (expansion 1.0) (length .15) (scaler .6) (hop .05) (ramp .4) (jitter 1.0) max-size edit): \
return a new granular synthesis generator.  'length' is the grain length (seconds), 'expansion' is the ratio in timing \
between the new and old (expansion > 1.0 slows things down), 'scaler' scales the grains \
to avoid overflows, 'hop' is the spacing (seconds) between successive grains upon output. \
'jitter' controls the randomness in that spacing, 'input' can be a file pointer. 'edit' can \
be a function of one arg, the current granulate generator.  It is called just before \
a grain is added into the output buffer. The current grain is accessible via " S_mus_data ". \
The edit function, if any, should return the length in samples of the grain, or 0."

  XEN in_obj = XEN_UNDEFINED;
  mus_xen *gn;
  mus_any *ge;
  XEN args[MAX_ARGLIST_LEN]; 
  XEN keys[9];
  int orig_arg[9] = {0, 0, 0, 0, 0, 0, 0, 0, 0};
  int vals, i, arglist_len, maxsize = 0;
  mus_float_t expansion = 1.0, segment_length = .15, segment_scaler = .6, ramp_time = .4, output_hop = .05;
  mus_float_t jitter = 1.0;
  XEN edit_obj = XEN_UNDEFINED, grn_obj;

  keys[0] = kw_input;
  keys[1] = kw_expansion;
  keys[2] = kw_length;
  keys[3] = kw_scaler;
  keys[4] = kw_hop;
  keys[5] = kw_ramp;
  keys[6] = kw_jitter;
  keys[7] = kw_max_size;
  keys[8] = kw_edit;

  arglist_len = XEN_LIST_LENGTH(arglist);
  if (arglist_len > MAX_ARGLIST_LEN)
    clm_error(S_make_granulate, "too many args!", arglist);

  for (i = 0; i < arglist_len; i++) args[i] = XEN_LIST_REF(arglist, i);
  for (i = arglist_len; i < MAX_ARGLIST_LEN; i++) args[i] = XEN_UNDEFINED;

  vals = mus_optkey_unscramble(S_make_granulate, 9, keys, args, orig_arg);
  if (vals > 0)
    {
      in_obj = mus_optkey_to_input_procedure(keys[0], S_make_granulate, orig_arg[0], XEN_UNDEFINED, 1, "granulate input procedure takes 1 arg");

      expansion = mus_optkey_to_float(keys[1], S_make_granulate, orig_arg[1], expansion);
      if (expansion <= 0.0) 
	XEN_OUT_OF_RANGE_ERROR(S_make_granulate, orig_arg[1], keys[1], "expansion ~A <= 0.0?");

      segment_length = mus_optkey_to_float(keys[2], S_make_granulate, orig_arg[2], segment_length);
      if (segment_length <= 0.0) 
	XEN_OUT_OF_RANGE_ERROR(S_make_granulate, orig_arg[2], keys[2], "segment-length ~A <= 0.0?");

      segment_scaler = mus_optkey_to_float(keys[3], S_make_granulate, orig_arg[3], segment_scaler);
      if (segment_scaler == 0.0) 
	XEN_OUT_OF_RANGE_ERROR(S_make_granulate, orig_arg[3], keys[3], "segment-scaler: ~A?");

      output_hop = mus_optkey_to_float(keys[4], S_make_granulate, orig_arg[4], output_hop);
      if (output_hop <= 0.0) 
	XEN_OUT_OF_RANGE_ERROR(S_make_granulate, orig_arg[4], keys[4], "hop ~A <= 0?");
      if (output_hop > 3600.0) 
	XEN_OUT_OF_RANGE_ERROR(S_make_granulate, orig_arg[4], keys[4], "hop ~A?");
      if ((segment_length + output_hop) > 60.0) /* multiplied by srate in mus_make_granulate in array allocation */
	XEN_OUT_OF_RANGE_ERROR(S_make_granulate, orig_arg[2], XEN_LIST_2(keys[2], keys[4]), "segment_length + output_hop = ~A: too large!");

      ramp_time = mus_optkey_to_float(keys[5], S_make_granulate, orig_arg[5], ramp_time);
      if ((ramp_time < 0.0) || (ramp_time > 0.5))
	XEN_OUT_OF_RANGE_ERROR(S_make_granulate, orig_arg[5], keys[5], "ramp ~A must be between 0.0 and 0.5");

      jitter = mus_optkey_to_float(keys[6], S_make_granulate, orig_arg[6], jitter);
      XEN_ASSERT_TYPE((jitter >= 0.0) && (jitter < 100.0), keys[6], orig_arg[6], S_make_granulate, "0.0 .. 100.0");

      maxsize = mus_optkey_to_int(keys[7], S_make_granulate, orig_arg[7], maxsize);
      if ((maxsize > mus_max_malloc()) || 
	  (maxsize < 0) ||
	  ((maxsize == 0) && (!XEN_KEYWORD_P(keys[7]))))
	XEN_OUT_OF_RANGE_ERROR(S_make_granulate, orig_arg[7], keys[7], "max-size ~A?");

      edit_obj = mus_optkey_to_procedure(keys[8], S_make_granulate, orig_arg[8], XEN_UNDEFINED, 1, "granulate edit procedure takes 1 arg");
    }

  gn = (mus_xen *)calloc(1, sizeof(mus_xen));
  {
    mus_error_handler_t *old_error_handler;
    old_error_handler = mus_error_set_handler(local_mus_error);
    ge = mus_make_granulate((MUS_XEN_P(in_obj)) ? as_needed_input_generator : as_needed_input_func, 
			    expansion, segment_length, segment_scaler, output_hop, ramp_time, jitter, maxsize, 
			    (XEN_NOT_BOUND_P(edit_obj) ? NULL : grnedit),
			    (void *)gn);
    mus_error_set_handler(old_error_handler);
  }
  if (ge)
    {
      gn->nvcts = MUS_MAX_VCTS;
      gn->vcts = make_vcts(gn->nvcts);
      gn->vcts[MUS_DATA_WRAPPER] = xen_make_vct_wrapper(mus_granulate_grain_max_length(ge), mus_data(ge));
      gn->vcts[MUS_INPUT_FUNCTION] = in_obj;
      gn->vcts[MUS_EDIT_FUNCTION] = edit_obj;
      gn->gen = ge;
      grn_obj = mus_xen_to_object(gn);
      gn->vcts[MUS_SELF_WRAPPER] = grn_obj;
      return(grn_obj);
    }
  free(gn);
  return(clm_mus_error(local_error_type, local_error_msg));
}




/* ---------------- convolve ---------------- */

static XEN g_convolve_p(XEN obj) 
{
  #define H_convolve_p "(" S_convolve_p " gen): " PROC_TRUE " if gen is a " S_convolve " generator"
  return(C_TO_XEN_BOOLEAN((MUS_XEN_P(obj)) && (mus_convolve_p(XEN_TO_MUS_ANY(obj)))));
}


static XEN g_convolve(XEN obj, XEN func) 
{
  #define H_convolve_gen "(" S_convolve " gen input-func): next sample from convolution generator"
  mus_xen *gn;
  mus_any *g = NULL;
  XEN_TO_C_GENERATOR(obj, g, mus_convolve_p, S_convolve, "a convolve generator");

  gn = XEN_TO_MUS_XEN(obj);
  if (XEN_PROCEDURE_P(func))
    {
      if (XEN_REQUIRED_ARGS_OK(func, 1))
	gn->vcts[MUS_INPUT_FUNCTION] = func;
      else XEN_BAD_ARITY_ERROR(S_convolve, 2, func, "convolve input function wants 1 arg");
    }
  return(C_TO_XEN_DOUBLE(mus_convolve(g, NULL)));
}


/* filter-size? */

static XEN g_make_convolve(XEN arglist)
{
  #define H_make_convolve "(" S_make_convolve " input filter fft-size): \
return a new convolution generator which convolves its input with the impulse response 'filter'."

  mus_xen *gn;
  mus_any *ge;
  XEN args[MAX_ARGLIST_LEN]; 
  XEN keys[3];
  int orig_arg[3] = {0, 0, 0};
  int vals, i, arglist_len;
  vct *filter = NULL;
  XEN filt = XEN_UNDEFINED, in_obj = XEN_UNDEFINED;
  mus_long_t fftlen, fft_size = 0;

  keys[0] = kw_input;
  keys[1] = kw_filter;
  keys[2] = kw_fft_size;

  arglist_len = XEN_LIST_LENGTH(arglist);
  if (arglist_len > MAX_ARGLIST_LEN)
    clm_error(S_make_convolve, "too many args!", arglist);

  for (i = 0; i < arglist_len; i++) args[i] = XEN_LIST_REF(arglist, i);
  for (i = arglist_len; i < MAX_ARGLIST_LEN; i++) args[i] = XEN_UNDEFINED;

  vals = mus_optkey_unscramble(S_make_convolve, 3, keys, args, orig_arg);
  if (vals > 0)
    {
      in_obj = mus_optkey_to_input_procedure(keys[0], S_make_convolve, orig_arg[0], XEN_UNDEFINED, 1, "convolve input procedure takes 1 arg");

      filter = mus_optkey_to_vct(keys[1], S_make_convolve, orig_arg[1], NULL);
      if (filter) filt = keys[1];

      fft_size = mus_optkey_to_mus_long_t(keys[2], S_make_convolve, orig_arg[2], fft_size);
      if ((fft_size  < 0) || 
	  ((fft_size == 0) && (!XEN_KEYWORD_P(keys[2]))) ||
	  (fft_size > mus_max_malloc()))
	XEN_OUT_OF_RANGE_ERROR(S_make_convolve, orig_arg[2], keys[2], "fft-size ~A? (see mus-max-malloc))");
    }

  if (filter == NULL)
    XEN_ERROR(NO_DATA,
	      XEN_LIST_1(C_TO_XEN_STRING(S_make_convolve ": no impulse (filter)?")));

  if (POWER_OF_2_P(filter->length))
    fftlen = filter->length * 2;
  else fftlen = (mus_long_t)pow(2.0, 1 + (int)(log((mus_float_t)(filter->length + 1)) / log(2.0)));
  if (fft_size < fftlen) fft_size = fftlen;

  gn = (mus_xen *)calloc(1, sizeof(mus_xen));
  {
    mus_error_handler_t *old_error_handler;
    old_error_handler = mus_error_set_handler(local_mus_error);
    ge = mus_make_convolve((MUS_XEN_P(in_obj)) ? as_needed_input_generator : as_needed_input_func, filter->data, fft_size, filter->length, gn);
    mus_error_set_handler(old_error_handler);
  }
  if (ge)
    {
      gn->nvcts = MUS_MAX_VCTS;
      gn->vcts = make_vcts(gn->nvcts);
      gn->vcts[MUS_INPUT_FUNCTION] = in_obj;
      gn->vcts[2] = filt; /* why is this here? GC protection? (might be a locally-allocated vct as from file->vct) */
      gn->gen = ge;
      return(mus_xen_to_object(gn));
    }
  free(gn);
  return(clm_mus_error(local_error_type, local_error_msg));
}


static XEN g_convolve_files(XEN file1, XEN file2, XEN maxamp, XEN outfile)
{
  #define H_convolve_files "(" S_convolve_files " file1 file2 maxamp output-file): convolve \
file1 and file2 writing outfile after scaling the convolution result to maxamp."

  const char *f1, *f2, *f3;
  mus_float_t maxval = 1.0;

  XEN_ASSERT_TYPE(XEN_STRING_P(file1), file1, XEN_ARG_1, S_convolve_files, "a string");
  XEN_ASSERT_TYPE(XEN_STRING_P(file2), file2, XEN_ARG_2, S_convolve_files, "a string");
  XEN_ASSERT_TYPE(XEN_NUMBER_IF_BOUND_P(maxamp), maxamp, XEN_ARG_3, S_convolve_files, "a number");
  XEN_ASSERT_TYPE((XEN_NOT_BOUND_P(outfile)) || (XEN_STRING_P(outfile)), outfile, XEN_ARG_4, S_convolve_files, "a string");

  f1 = XEN_TO_C_STRING(file1);
  f2 = XEN_TO_C_STRING(file2);
  if (XEN_STRING_P(outfile)) 
    f3 = XEN_TO_C_STRING(outfile); 
  else f3 = "tmp.snd";
  if (XEN_NUMBER_P(maxamp)) 
    maxval = XEN_TO_C_DOUBLE(maxamp);

  mus_convolve_files(f1, f2, maxval, f3);
  return(C_TO_XEN_STRING(f3));
}




/* ---------------- phase-vocoder ---------------- */

/* pvedit pvanalyze pvsynthesize:
 * these three functions provide a path for the call (clm.c) (*(pv->edit))(pv->closure)
 *   which is calling a user-supplied edit function within the particular phase-vocoder
 *   generator's context.  "closure" is an uninterpreted void pointer passed in by the
 *   user, and passed here as the edit function argument.  In this file, pv->edit is
 *   &pvedit, and (void *)ptr is closure; in make_phase_vocoder we set closure to be
 *   the mus_xen object that shadows the phase-vocoder generator, with two special
 *   pointers in the vcts field: vcts[MUS_EDIT_FUNCTION] is the (Scheme-side) function
 *   passed by the user, and vcts[MUS_SELF_WRAPPER] is a pointer to the (Scheme-relevant)
 *   smob that packages the mus_xen pointer for Scheme.  This way, the user's
 *    (make-phase-vocoder ... (lambda (v) (mus-length v)) ...)
 *   treats v as the current pv gen, vcts[MUS_SELF_WRAPPER] = v, vcts[MUS_EDIT_FUNCTION] = 
 *   the lambda form, mus_xen obj->gen is the C-side pv struct pointer.  See above
 *   under as_needed_input_func for more verbiage.  (All this complication arises because clm.c
 *   is pure C -- no notion that Scheme might be the caller, and the user's pv.scm
 *   or whatever is pure Scheme -- no notion that C is actually doing the work,
 *   and we have to tie everything together here including the Scheme-C-Scheme-C 
 *   call chains).
 */

static int pvedit(void *ptr)
{
  mus_xen *gn = (mus_xen *)ptr;
  return(XEN_TO_C_BOOLEAN(XEN_CALL_1_NO_CATCH(gn->vcts[MUS_EDIT_FUNCTION], gn->vcts[MUS_SELF_WRAPPER])));
}


static mus_float_t pvsynthesize(void *ptr)
{
  mus_xen *gn = (mus_xen *)ptr;
  return(XEN_TO_C_DOUBLE(XEN_CALL_1_NO_CATCH(gn->vcts[MUS_SYNTHESIZE_FUNCTION], gn->vcts[MUS_SELF_WRAPPER])));
}


static bool pvanalyze(void *ptr, mus_float_t (*input)(void *arg1, int direction))
{
  mus_xen *gn = (mus_xen *)ptr;
  /* we can only get input func if it's already set up by the outer gen call, so (?) we can use that function here */
  return(XEN_TO_C_BOOLEAN(XEN_CALL_2_NO_CATCH(gn->vcts[MUS_ANALYZE_FUNCTION], 
					      gn->vcts[MUS_SELF_WRAPPER], 
					      gn->vcts[MUS_INPUT_FUNCTION])));
}


static XEN g_phase_vocoder_p(XEN obj) 
{
  #define H_phase_vocoder_p "(" S_phase_vocoder_p " gen): " PROC_TRUE " if gen is an " S_phase_vocoder
  return(C_TO_XEN_BOOLEAN((MUS_XEN_P(obj)) && (mus_phase_vocoder_p(XEN_TO_MUS_ANY(obj)))));
}


static XEN g_phase_vocoder(XEN obj, XEN func, XEN analyze_func, XEN edit_func, XEN synthesize_func)
{
  #define H_phase_vocoder "(" S_phase_vocoder " gen input-function analyze-func edit-func synthesize-func): next phase vocoder value"
  mus_xen *gn;
  mus_any *g = NULL;
  XEN_TO_C_GENERATOR(obj, g, mus_phase_vocoder_p, S_phase_vocoder, "a phase-vocoder generator");

  gn = XEN_TO_MUS_XEN(obj);
  if (XEN_BOUND_P(func))
    {
      bool (*analyze)(void *arg, mus_float_t (*input)(void *arg1, int direction)) = NULL;
      int (*edit)(void *arg) = NULL;
      mus_float_t (*synthesize)(void *arg) = NULL;
      if (XEN_PROCEDURE_P(func))
	{
	  if (XEN_REQUIRED_ARGS_OK(func, 1))
	    gn->vcts[MUS_INPUT_FUNCTION] = func; /* as_needed_input_func set at make time will pick this up */
	  else XEN_BAD_ARITY_ERROR(S_phase_vocoder, 2, func, S_phase_vocoder " input function wants 1 arg");
	}
      if (XEN_PROCEDURE_P(analyze_func))
	{
	  if (XEN_REQUIRED_ARGS_OK(analyze_func, 2))
	    {
	      gn->vcts[MUS_ANALYZE_FUNCTION] = analyze_func;
	      analyze = pvanalyze;
	    }
	  else XEN_BAD_ARITY_ERROR(S_phase_vocoder, 3, analyze_func, S_phase_vocoder " analyze function wants 2 args");
	}
      if (XEN_PROCEDURE_P(edit_func))
	{
	  if (XEN_REQUIRED_ARGS_OK(edit_func, 1))
	    {
	      gn->vcts[MUS_EDIT_FUNCTION] = edit_func;
	      edit = pvedit;
	    }
	  else XEN_BAD_ARITY_ERROR(S_phase_vocoder, 4, edit_func, S_phase_vocoder " edit function wants 1 arg");
	}
      if (XEN_PROCEDURE_P(synthesize_func))
	{
	  if (XEN_REQUIRED_ARGS_OK(synthesize_func, 1))
	    {
	      gn->vcts[MUS_SYNTHESIZE_FUNCTION] = synthesize_func;
	      synthesize = pvsynthesize;
	    }
	  else XEN_BAD_ARITY_ERROR(S_phase_vocoder, 5, synthesize_func, S_phase_vocoder " synthesize function wants 1 arg");
	}
      return(C_TO_XEN_DOUBLE(mus_phase_vocoder_with_editors(g, NULL, analyze, edit, synthesize)));
    }
  return(C_TO_XEN_DOUBLE(mus_phase_vocoder(g, NULL)));
}


static XEN g_make_phase_vocoder(XEN arglist)
{
  #if HAVE_SCHEME
    #define pv_example "(" S_make_phase_vocoder " #f 512 4 256 1.0 #f #f #f)"
    #define pv_edit_example "(" S_make_phase_vocoder " #f 512 4 256 1.0\n\
    (lambda (v infunc) (snd-print \"analyzing\") #t)\n\
    (lambda (v) (snd-print \"editing\") #t)\n\
    (lambda (v) (snd-print \"resynthesizing\") 0.0))"
  #endif
  #if HAVE_RUBY
    #define pv_example "make_phase_vocoder(false, 512, 4, 256, 1.0, false, false, false)"
    #define pv_edit_example "make_phase_vocoder(false, 512, 4, 256, 1.0,\n\
        lambda do | v, infunc | snd_print(\"analyzing\"); true end,\n\
        lambda do | v | snd_print(\"editing\"); true end,\n\
        lambda do | v | snd_print(\"resynthesizing\"); 0.0 end)"
  #endif
  #if HAVE_FORTH
    #define pv_example "#f 512 4 256 1.0 #f #f #f " S_make_phase_vocoder
    #define pv_edit_example "#f 512 4 256 1.0\n\
    lambda: <{ v infunc -- f }> \"analyzing\" snd-print drop #t ;\n\
    lambda: <{ v -- n }> \"editing\" snd-print drop #t ;\n\
    lambda: <{ v -- r }> \"resynthesizing\" snd-print drop 0.0 ; " S_make_phase_vocoder
  #endif

  #define H_make_phase_vocoder "(" S_make_phase_vocoder " input fft-size overlap interp pitch analyze edit synthesize): \
return a new phase-vocoder generator; input is the input function (it can be set at run-time), analyze, edit, \
and synthesize are either " PROC_FALSE " or functions that replace the default innards of the generator, fft-size, overlap \
and interp set the fftsize, the amount of overlap between ffts, and the time between new analysis calls. \
'analyze', if given, takes 2 args, the generator and the input function; if it returns " PROC_TRUE ", the default analysis \
code is also called.  'edit', if given, takes 1 arg, the generator; if it returns " PROC_TRUE ", the default edit code \
is run.  'synthesize' is a function of 1 arg, the generator; it is called to get the current vocoder \
output. \n\n  " pv_example "\n\n  " pv_edit_example

  XEN in_obj = XEN_UNDEFINED, edit_obj = XEN_UNDEFINED, synthesize_obj = XEN_UNDEFINED, analyze_obj = XEN_UNDEFINED;
  mus_xen *gn;
  mus_any *ge;
  XEN args[MAX_ARGLIST_LEN]; 
  XEN keys[8];
  XEN pv_obj;
  int orig_arg[8] = {0, 0, 0, 0, 0, 0, 0, 0};
  int vals, arglist_len, i;
  int fft_size = 512, overlap = 4, interp = 128;
  mus_float_t pitch = 1.0;

  keys[0] = kw_input;
  keys[1] = kw_fft_size;
  keys[2] = kw_overlap;
  keys[3] = kw_interp;
  keys[4] = kw_pitch;
  keys[5] = kw_analyze;
  keys[6] = kw_edit;
  keys[7] = kw_synthesize;

  arglist_len = XEN_LIST_LENGTH(arglist);
  if (arglist_len > MAX_ARGLIST_LEN)
    clm_error(S_make_phase_vocoder, "too many args!", arglist);

  for (i = 0; i < arglist_len; i++) args[i] = XEN_LIST_REF(arglist, i);
  for (i = arglist_len; i < MAX_ARGLIST_LEN; i++) args[i] = XEN_UNDEFINED;

  vals = mus_optkey_unscramble(S_make_phase_vocoder, 8, keys, args, orig_arg);
  if (vals > 0)
    {
      in_obj = mus_optkey_to_input_procedure(keys[0], S_make_phase_vocoder, orig_arg[0], XEN_UNDEFINED, 1, S_phase_vocoder " input procedure takes 1 arg");

      fft_size = mus_optkey_to_int(keys[1], S_make_phase_vocoder, orig_arg[1], fft_size);
      if (fft_size <= 1) 
	XEN_OUT_OF_RANGE_ERROR(S_make_phase_vocoder, orig_arg[1], keys[1], "fft size ~A <= 1?");
      if (fft_size > mus_max_malloc())
	XEN_OUT_OF_RANGE_ERROR(S_make_phase_vocoder, orig_arg[1], keys[1], "fft size ~A too large (see mus-max-malloc)");
      if (!POWER_OF_2_P(fft_size))
	XEN_OUT_OF_RANGE_ERROR(S_make_phase_vocoder, orig_arg[1], keys[1], "fft size ~A must be power of 2");

      overlap = mus_optkey_to_int(keys[2], S_make_phase_vocoder, orig_arg[2], overlap);
      if (overlap <= 0)
	XEN_OUT_OF_RANGE_ERROR(S_make_phase_vocoder, orig_arg[2], keys[2], "overlap ~A <= 0?");

      interp = mus_optkey_to_int(keys[3], S_make_phase_vocoder, orig_arg[3], interp);
      if (interp <= 0)
	XEN_OUT_OF_RANGE_ERROR(S_make_phase_vocoder, orig_arg[3], keys[3], "interp ~A <= 0?");

      pitch = mus_optkey_to_float(keys[4], S_make_phase_vocoder, orig_arg[4], pitch);

      analyze_obj = mus_optkey_to_procedure(keys[5], S_make_phase_vocoder, orig_arg[5], XEN_UNDEFINED, 2, S_phase_vocoder " analyze procedure takes 2 args");
      edit_obj = mus_optkey_to_procedure(keys[6], S_make_phase_vocoder, orig_arg[6], XEN_UNDEFINED, 1, S_phase_vocoder " edit procedure takes 1 arg");
      synthesize_obj = mus_optkey_to_procedure(keys[7], S_make_phase_vocoder, orig_arg[7], XEN_UNDEFINED, 1, S_phase_vocoder " synthesize procedure takes 1 arg");
    }

  gn = (mus_xen *)calloc(1, sizeof(mus_xen));
  {
    mus_error_handler_t *old_error_handler;
    old_error_handler = mus_error_set_handler(local_mus_error);
    ge = mus_make_phase_vocoder((MUS_XEN_P(in_obj)) ? as_needed_input_generator : as_needed_input_func,
				fft_size, overlap, interp, pitch,
				(XEN_NOT_BOUND_P(analyze_obj) ? NULL : pvanalyze),
				(XEN_NOT_BOUND_P(edit_obj) ? NULL : pvedit),
				(XEN_NOT_BOUND_P(synthesize_obj) ? NULL : pvsynthesize),
				(void *)gn);
    mus_error_set_handler(old_error_handler);
  }
  if (ge)
    {
      gn->nvcts = MUS_MAX_VCTS;
      gn->vcts = make_vcts(gn->nvcts);
      gn->vcts[MUS_INPUT_FUNCTION] = in_obj;
      gn->vcts[MUS_EDIT_FUNCTION] = edit_obj;
      gn->vcts[MUS_ANALYZE_FUNCTION] = analyze_obj;
      gn->vcts[MUS_SYNTHESIZE_FUNCTION] = synthesize_obj;
      gn->gen = ge;
      pv_obj = mus_xen_to_object(gn);
      /* need scheme-relative backpointer for possible function calls */
      gn->vcts[MUS_SELF_WRAPPER] = pv_obj;
      return(pv_obj);
    }
  free(gn);
  return(clm_mus_error(local_error_type, local_error_msg));
}


static XEN g_phase_vocoder_amps(XEN pv) 
{
  #define H_phase_vocoder_amps "(" S_phase_vocoder_amps " gen): vct containing the current output sinusoid amplitudes"
  mus_float_t *amps; 
  int len;
  mus_xen *gn;

  XEN_ASSERT_TYPE((MUS_XEN_P(pv)) && (mus_phase_vocoder_p(XEN_TO_MUS_ANY(pv))), pv, XEN_ONLY_ARG, S_phase_vocoder_amps, "a " S_phase_vocoder " generator");

  gn = XEN_TO_MUS_XEN(pv);
  amps = mus_phase_vocoder_amps(gn->gen); 
  len = (int)mus_length(gn->gen);
  return(xen_make_vct_wrapper(len / 2, amps));
}

  
static XEN g_phase_vocoder_freqs(XEN pv) 
{
  #define H_phase_vocoder_freqs "(" S_phase_vocoder_freqs " gen): vct containing the current output sinusoid frequencies"
  mus_float_t *amps; 
  int len;
  mus_xen *gn;

  XEN_ASSERT_TYPE((MUS_XEN_P(pv)) && (mus_phase_vocoder_p(XEN_TO_MUS_ANY(pv))), pv, XEN_ONLY_ARG, S_phase_vocoder_freqs, "a " S_phase_vocoder " generator");

  gn = XEN_TO_MUS_XEN(pv);
  amps = mus_phase_vocoder_freqs(gn->gen); 
  len = (int)mus_length(gn->gen);
  return(xen_make_vct_wrapper(len, amps));
}

  
static XEN g_phase_vocoder_phases(XEN pv) 
{
  #define H_phase_vocoder_phases "(" S_phase_vocoder_phases " gen): vct containing the current output sinusoid phases"
  mus_float_t *amps; 
  int len;
  mus_xen *gn;

  XEN_ASSERT_TYPE((MUS_XEN_P(pv)) && (mus_phase_vocoder_p(XEN_TO_MUS_ANY(pv))), pv, XEN_ONLY_ARG, S_phase_vocoder_phases, "a " S_phase_vocoder " generator");

  gn = XEN_TO_MUS_XEN(pv);
  amps = mus_phase_vocoder_phases(gn->gen); 
  len = (int)mus_length(gn->gen);
  return(xen_make_vct_wrapper(len / 2, amps));
}
  

static XEN g_phase_vocoder_amp_increments(XEN pv) 
{
  #define H_phase_vocoder_amp_increments "(" S_phase_vocoder_amp_increments " gen): vct containing the current output sinusoid amplitude increments per sample"
  mus_float_t *amps; 
  int len;
  mus_xen *gn;

  XEN_ASSERT_TYPE((MUS_XEN_P(pv)) && (mus_phase_vocoder_p(XEN_TO_MUS_ANY(pv))), pv, XEN_ONLY_ARG, S_phase_vocoder_amp_increments, "a " S_phase_vocoder " generator");

  gn = XEN_TO_MUS_XEN(pv);
  amps = mus_phase_vocoder_amp_increments(gn->gen); 
  len = (int)mus_length(gn->gen);
  return(xen_make_vct_wrapper(len, amps));
}
  

static XEN g_phase_vocoder_phase_increments(XEN pv) 
{
  #define H_phase_vocoder_phase_increments "(" S_phase_vocoder_phase_increments " gen): vct containing the current output sinusoid phase increments"
  mus_float_t *amps; 
  int len;
  mus_xen *gn;

  XEN_ASSERT_TYPE((MUS_XEN_P(pv)) && (mus_phase_vocoder_p(XEN_TO_MUS_ANY(pv))), pv, XEN_ONLY_ARG, S_phase_vocoder_phase_increments, "a " S_phase_vocoder " generator");

  gn = XEN_TO_MUS_XEN(pv);
  amps = mus_phase_vocoder_phase_increments(gn->gen); 
  len = (int)mus_length(gn->gen);
  return(xen_make_vct_wrapper(len / 2, amps));
}

  
static XEN g_mus_hop(XEN obj)
{
  #define H_mus_hop "(" S_mus_hop " gen): gen's " S_mus_hop " field"
  if (XEN_LIST_P(obj)) return(call_get_method(obj, S_mus_hop));
  XEN_ASSERT_TYPE(MUS_XEN_P(obj), obj, XEN_ONLY_ARG, S_mus_hop, "a generator");
  return(C_TO_XEN_LONG_LONG(mus_hop(XEN_TO_MUS_ANY(obj))));
}


static XEN g_mus_set_hop(XEN obj, XEN val)
{
  if (XEN_LIST_P(obj)) return(call_set_method(obj, val, S_mus_hop));
  XEN_ASSERT_TYPE(MUS_XEN_P(obj), obj, XEN_ARG_1, S_setB S_mus_hop, "a generator");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(val), val, XEN_ARG_2, S_setB S_mus_hop, "an integer");
  return(C_TO_XEN_LONG_LONG(mus_set_hop(XEN_TO_MUS_ANY(obj), XEN_TO_C_LONG_LONG(val))));
}





/* ---------------- mix ---------------- */

static XEN g_mus_mix(XEN out, XEN in, XEN ost, XEN olen, XEN ist, XEN mx, XEN envs)
{
  #define H_mus_mix "(" S_mus_mix " outfile infile (outloc 0) (frames) (inloc 0) mixer envs): \
mix infile into outfile starting at outloc in outfile and inloc in infile \
mixing 'frames' frames into 'outfile'.  frames defaults to the length of infile. If mixer, \
use it to scale the various channels; if envs (an array of envelope generators), use \
it in conjunction with mixer to scale/envelope all the various ins and outs. \
'outfile' can also be a " S_frame_to_file " generator, and 'infile' can be a " S_file_to_frame " generator."

  mus_any *outf = NULL, *inf = NULL;
  mus_any *mx1 = NULL;
  mus_any ***envs1 = NULL;
  int i;
  mus_long_t ostart = 0, istart = 0, osamps = 0;
  int in_chans = 0, out_chans = 0, in_size = 0, out_size;  /* mus_mix in clm.c assumes the envs array is large enough */

  XEN_ASSERT_TYPE(XEN_STRING_P(out) || ((MUS_XEN_P(out)) && (mus_output_p(XEN_TO_MUS_ANY(out)))), 
		  out, XEN_ARG_1, S_mus_mix, "a filename or a frame->file generator");
  XEN_ASSERT_TYPE(XEN_STRING_P(in) || ((MUS_XEN_P(in)) && (mus_input_p(XEN_TO_MUS_ANY(in)))), 
		  in, XEN_ARG_2, S_mus_mix, "a filename or a file->frame generator");
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(ost), ost, XEN_ARG_3, S_mus_mix, "an integer");
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(olen), olen, XEN_ARG_4, S_mus_mix, "an integer");
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(ist), ist, XEN_ARG_5, S_mus_mix, "an integer");
  XEN_ASSERT_TYPE((XEN_NOT_BOUND_P(mx)) || (XEN_FALSE_P(mx)) || ((MUS_XEN_P(mx)) && (mus_mixer_p(XEN_TO_MUS_ANY(mx)))), mx, XEN_ARG_6, S_mus_mix, "a mixer");
  XEN_ASSERT_TYPE((XEN_NOT_BOUND_P(envs)) || (XEN_FALSE_P(envs)) || (XEN_VECTOR_P(envs)), envs, XEN_ARG_7, S_mus_mix, "an env gen or vector of envs");
  if (XEN_BOUND_P(ost)) ostart = XEN_TO_C_LONG_LONG(ost);
  if (XEN_BOUND_P(ist)) istart = XEN_TO_C_LONG_LONG(ist);
  if ((XEN_BOUND_P(mx)) && (MUS_XEN_P(mx))) mx1 = (mus_any *)XEN_TO_MUS_ANY(mx);
  if (XEN_STRING_P(out)) 
    {
      const char *tmp_outf = NULL;
      tmp_outf = XEN_TO_C_STRING(out);
      if (!mus_file_probe(tmp_outf)) 
	XEN_ERROR(NO_SUCH_FILE,
		  XEN_LIST_2(C_TO_XEN_STRING(S_mus_mix ": no such file, ~S"),
			     out));
      else out_chans = mus_sound_chans(tmp_outf);
    }
  else 
    {
      outf = XEN_TO_MUS_ANY(out);
      out_chans = mus_channels(outf);
    }

  if (out_chans <= 0)
    XEN_ERROR(BAD_HEADER,
	      XEN_LIST_2(C_TO_XEN_STRING(S_mus_mix ": ~S output chans <= 0"),
			 out));

  if (XEN_STRING_P(in)) 
    {
      const char *tmp_inf = NULL;
      tmp_inf = XEN_TO_C_STRING(in); 
      if (!mus_file_probe(tmp_inf)) 
	XEN_ERROR(NO_SUCH_FILE,
		  XEN_LIST_2(C_TO_XEN_STRING(S_mus_mix ": no such file, ~S"),
			     in));
      else in_chans = mus_sound_chans(tmp_inf);
    }
  else 
    {
      inf = XEN_TO_MUS_ANY(in);
      in_chans = mus_channels(inf);
    }

  if (in_chans <= 0)
    XEN_ERROR(BAD_HEADER,
	      XEN_LIST_2(C_TO_XEN_STRING(S_mus_mix ": ~S input chans <= 0"),
			 in));

  if (XEN_BOUND_P(olen)) 
    osamps = XEN_TO_C_LONG_LONG(olen); 
  else 
    {
      if (XEN_STRING_P(in))
	osamps = mus_sound_frames(XEN_TO_C_STRING(in));
      else osamps = mus_length(inf);
      if (osamps < 0)
	XEN_ERROR(BAD_HEADER,
		  XEN_LIST_2(C_TO_XEN_STRING(S_mus_mix ": ~S input frames < 0"),
			     in));
    }
  if (osamps == 0) return(XEN_FALSE);

  if ((XEN_BOUND_P(envs)) && (!(XEN_FALSE_P(envs))))
    {
      int in_len = 0, out_len, j;
      /* pack into a C-style array of arrays of env pointers */
      in_len = XEN_VECTOR_LENGTH(envs);

      if (in_len == 0)
	XEN_ERROR(BAD_TYPE,
		  XEN_LIST_2(C_TO_XEN_STRING(S_mus_mix ": env vector, ~A, can't be empty"),
			     envs));

      for (i = 0; i < in_len; i++)
	{
	  XEN datum;
	  datum = XEN_VECTOR_REF(envs, i);
	  if (!(XEN_VECTOR_P(datum)))
	    XEN_ERROR(BAD_TYPE,
		      XEN_LIST_2(C_TO_XEN_STRING(S_mus_mix ": each element of env vector, ~A, must be a vector (of envelopes)"),
				 datum));
	}
      out_len = XEN_VECTOR_LENGTH(XEN_VECTOR_REF(envs, 0));
      if (in_len < in_chans) in_size = in_chans; else in_size = in_len;
      if (out_len < out_chans) out_size = out_chans; else out_size = out_len;
      envs1 = (mus_any ***)calloc(in_size, sizeof(mus_any **));
      for (i = 0; i < in_size; i++) envs1[i] = (mus_any **)calloc(out_size, sizeof(mus_any *));
      for (i = 0; i < in_len; i++)
	{
	  for (j = 0; j < out_len; j++) 
	    {
	      XEN datum1;
	      datum1 = XEN_VECTOR_REF(XEN_VECTOR_REF(envs, i), j);
	      if (MUS_XEN_P(datum1))
		{
		  if (mus_env_p(XEN_TO_MUS_ANY(datum1)))
		    envs1[i][j] = XEN_TO_MUS_ANY(datum1);
		  else 
		    {
		      for (i = 0; i < in_size; i++) if (envs1[i]) free(envs1[i]);
		      free(envs1);
		      XEN_ERROR(BAD_TYPE,
				XEN_LIST_4(C_TO_XEN_STRING(S_mus_mix ": each (non " PROC_FALSE ") element of (inner) envs vector, ~A at ~A ~A, must be an envelope"),
					   datum1,
					   C_TO_XEN_INT(i),
					   C_TO_XEN_INT(j)));
		    }
		}
	    }
	}
    }
  {
    char *outfile = NULL, *infile = NULL;
    if (XEN_STRING_P(out)) outfile = mus_strdup(XEN_TO_C_STRING(out));
    if (XEN_STRING_P(in)) infile = mus_strdup(XEN_TO_C_STRING(in));

    if ((infile) && (outfile))
      mus_mix(outfile, infile, ostart, osamps, istart, mx1, envs1);
    else
      {
	if (infile)
	  inf = mus_make_file_to_frame(infile);
	if (outfile)
	  outf = mus_continue_sample_to_file(outfile);
	mus_mix_with_reader_and_writer(outf, inf, ostart, osamps, istart, mx1, envs1);
	if (infile)
	  mus_free((mus_any *)inf);
	if (outfile)
	  mus_free((mus_any *)outf);
      }
    if (envs1) 
      {
	for (i = 0; i < in_size; i++) if (envs1[i]) free(envs1[i]);
	free(envs1);
      }
    if (infile) free(infile);
    if (outfile) free(outfile);
  }
  return(XEN_TRUE);
}




/* -------- ssb-am -------- */

static XEN g_ssb_am_p(XEN obj) 
{
  #define H_ssb_am_p "(" S_ssb_am_p " gen): " PROC_TRUE " if gen is a " S_ssb_am
  return(C_TO_XEN_BOOLEAN((MUS_XEN_P(obj)) && (mus_ssb_am_p(XEN_TO_MUS_ANY(obj)))));
}


static XEN g_make_ssb_am(XEN arg1, XEN arg2, XEN arg3, XEN arg4)
{
  #define H_make_ssb_am "(" S_make_ssb_am " (frequency *clm-default-frequency*) (order 40)): \
return a new " S_ssb_am " generator."
  #define MUS_MAX_SSB_ORDER 65536

  mus_any *ge;
  XEN args[4]; 
  XEN keys[2];
  int orig_arg[2] = {0, 0};
  int vals;
  int order = 40;
  mus_float_t freq;

  freq = clm_default_frequency;

  keys[0] = kw_frequency;
  keys[1] = kw_order;
  args[0] = arg1; args[1] = arg2; args[2] = arg3; args[3] = arg4;

  vals = mus_optkey_unscramble(S_make_ssb_am, 2, keys, args, orig_arg);
  if (vals > 0)
    {
      freq = mus_optkey_to_float(keys[0], S_make_ssb_am, orig_arg[0], freq);
      if (freq > (0.5 * mus_srate()))
	XEN_OUT_OF_RANGE_ERROR(S_make_ssb_am, orig_arg[0], keys[0], "freq ~A > srate/2?");

      order = mus_optkey_to_int(keys[1], S_make_ssb_am, orig_arg[1], order);
      if (order <= 0)
	XEN_OUT_OF_RANGE_ERROR(S_make_ssb_am, orig_arg[1], keys[1], "order ~A <= 0?");
      if (order > MUS_MAX_SSB_ORDER)
	XEN_OUT_OF_RANGE_ERROR(S_make_ssb_am, orig_arg[1], keys[1], "order ~A too large?");
    }

  ge = mus_make_ssb_am(freq, order);
  if (ge) return(mus_xen_to_object(mus_any_to_mus_xen(ge)));
  return(XEN_FALSE);
}


static XEN g_ssb_am(XEN obj, XEN insig, XEN fm)
{
  #define H_ssb_am "(" S_ssb_am " gen (insig 0.0) (fm 0.0)): get the next sample from " S_ssb_am " generator"

  mus_float_t insig1 = 0.0;
  mus_any *g = NULL;

  XEN_TO_C_GENERATOR(obj, g, mus_ssb_am_p, S_ssb_am, "an ssb-am generator");
  XEN_TO_C_DOUBLE_IF_BOUND(insig, insig1, S_ssb_am, XEN_ARG_2);

  if (XEN_BOUND_P(fm))
    {
      XEN_ASSERT_TYPE(XEN_NUMBER_P(fm), fm, XEN_ARG_3, S_ssb_am, "a number");
      return(C_TO_XEN_DOUBLE(mus_ssb_am(g, insig1, XEN_TO_C_DOUBLE(fm))));
    }
  return(C_TO_XEN_DOUBLE(mus_ssb_am_unmodulated(g, insig1)));
}


static XEN g_ssb_bank(XEN ssbs, XEN filters, XEN inval, XEN size)
{
  /* an experiment */
  int i, len;
  mus_float_t sum = 0.0, val = 0.0;

  XEN_ASSERT_TYPE(XEN_VECTOR_P(ssbs), ssbs, XEN_ARG_1, "ssb-bank", "vector of " S_ssb_am " gens");
  XEN_ASSERT_TYPE(XEN_VECTOR_P(filters), filters, XEN_ARG_2, "ssb-bank", "vector of FIR filter gens");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(inval), inval, XEN_ARG_3, "ssb-bank", "number");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(size), size, XEN_ARG_4, "ssb-bank", "int");

  len = XEN_TO_C_INT(size);
  val = XEN_TO_C_DOUBLE(inval);
  for (i = 0; i < len; i++)
    sum += mus_ssb_am_unmodulated(XEN_TO_MUS_ANY(XEN_VECTOR_REF(ssbs, i)),
				  mus_fir_filter(XEN_TO_MUS_ANY(XEN_VECTOR_REF(filters, i)), val));
  return(C_TO_XEN_DOUBLE(sum));
}


#define S_mus_frandom "mus-frandom"
#define S_mus_irandom "mus-irandom"
static XEN g_mus_frandom(XEN val) {return(C_TO_XEN_DOUBLE(mus_frandom(XEN_TO_C_DOUBLE(val))));}
static XEN g_mus_irandom(XEN val) {return(C_TO_XEN_INT(mus_irandom(XEN_TO_C_INT(val))));}



#if HAVE_SCHEME
#if HAVE_GETTIMEOFDAY && HAVE_DIFFTIME && HAVE_SYS_TIME_H && (!_MSC_VER)

#include <time.h>
#include <sys/time.h>

static struct timeval overall_start_time;
#define S_get_internal_real_time "get-internal-real-time"
#define S_internal_time_units_per_second "internal-time-units-per-second"

static XEN g_get_internal_real_time(void) 
{
  #define H_get_internal_real_time "(" S_get_internal_real_time ") returns the number of seconds since \
the program started.  The number is in terms of " S_internal_time_units_per_second ", usually 1"
  struct timezone z0;
  struct timeval t0;
  double secs;
  gettimeofday(&t0, &z0);
  secs = difftime(t0.tv_sec, overall_start_time.tv_sec);
  return(C_TO_XEN_DOUBLE(secs + 0.000001 * (t0.tv_usec - overall_start_time.tv_usec)));
}
#else
static XEN g_get_internal_real_time(void) {return(C_TO_XEN_DOUBLE(0.0));}
#endif

XEN_NARGIFY_0(g_get_internal_real_time_w, g_get_internal_real_time)
#endif



/* ---------------- export ---------------- */

#ifdef XEN_ARGIFY_1
XEN_NARGIFY_0(g_mus_srate_w, g_mus_srate)
XEN_NARGIFY_1(g_mus_set_srate_w, g_mus_set_srate)
XEN_NARGIFY_0(g_mus_float_equal_fudge_factor_w, g_mus_float_equal_fudge_factor)
XEN_NARGIFY_1(g_mus_set_float_equal_fudge_factor_w, g_mus_set_float_equal_fudge_factor)
XEN_NARGIFY_0(g_mus_array_print_length_w, g_mus_array_print_length)
XEN_NARGIFY_1(g_mus_set_array_print_length_w, g_mus_set_array_print_length)
XEN_NARGIFY_1(g_radians_to_hz_w, g_radians_to_hz)
XEN_NARGIFY_1(g_hz_to_radians_w, g_hz_to_radians)
XEN_NARGIFY_1(g_radians_to_degrees_w, g_radians_to_degrees)
XEN_NARGIFY_1(g_degrees_to_radians_w, g_degrees_to_radians)
XEN_NARGIFY_1(g_db_to_linear_w, g_db_to_linear)
XEN_NARGIFY_1(g_linear_to_db_w, g_linear_to_db)
XEN_NARGIFY_1(g_seconds_to_samples_w, g_seconds_to_samples)
XEN_NARGIFY_1(g_samples_to_seconds_w, g_samples_to_seconds)
XEN_NARGIFY_2(g_ring_modulate_w, g_ring_modulate)
XEN_NARGIFY_3(g_amplitude_modulate_w, g_amplitude_modulate)
XEN_ARGIFY_2(g_contrast_enhancement_w, g_contrast_enhancement)
XEN_ARGIFY_3(g_dot_product_w, g_dot_product)
#if HAVE_COMPLEX_TRIG && XEN_HAVE_COMPLEX_NUMBERS
XEN_NARGIFY_2(g_edot_product_w, g_edot_product)
#endif
XEN_NARGIFY_1(g_clear_array_w, g_clear_array)
XEN_NARGIFY_2(g_polynomial_w, g_polynomial)
XEN_ARGIFY_3(g_multiply_arrays_w, g_multiply_arrays)
XEN_ARGIFY_4(g_make_fft_window_w, g_make_fft_window)
XEN_ARGIFY_4(g_mus_fft_w, g_mus_fft)
XEN_ARGIFY_4(g_spectrum_w, g_spectrum)
XEN_NARGIFY_1(g_autocorrelate_w, g_autocorrelate)
XEN_NARGIFY_2(g_correlate_w, g_correlate)
XEN_ARGIFY_3(g_convolution_w, g_convolution)
XEN_NARGIFY_2(g_rectangular_to_polar_w, g_rectangular_to_polar)
XEN_NARGIFY_2(g_rectangular_to_magnitudes_w, g_rectangular_to_magnitudes)
XEN_NARGIFY_2(g_polar_to_rectangular_w, g_polar_to_rectangular)
XEN_ARGIFY_3(g_array_interp_w, g_array_interp)
XEN_ARGIFY_5(g_mus_interpolate_w, g_mus_interpolate)
XEN_NARGIFY_1(g_mus_describe_w, g_mus_describe)
XEN_NARGIFY_1(g_mus_name_w, g_mus_name)
XEN_NARGIFY_2(g_mus_set_name_w, g_mus_set_name)
XEN_ARGIFY_3(g_mus_run_w, g_mus_run)
XEN_NARGIFY_1(g_mus_phase_w, g_mus_phase)
XEN_NARGIFY_2(g_mus_set_phase_w, g_mus_set_phase)
XEN_NARGIFY_1(g_mus_width_w, g_mus_width)
XEN_NARGIFY_2(g_mus_set_width_w, g_mus_set_width)
XEN_NARGIFY_1(g_mus_scaler_w, g_mus_scaler)
XEN_NARGIFY_2(g_mus_set_scaler_w, g_mus_set_scaler)
XEN_NARGIFY_1(g_mus_safety_w, g_mus_safety)
XEN_NARGIFY_2(g_mus_set_safety_w, g_mus_set_safety)
XEN_NARGIFY_1(g_mus_feedforward_w, g_mus_feedforward)
XEN_NARGIFY_2(g_mus_set_feedforward_w, g_mus_set_feedforward)
XEN_NARGIFY_1(g_mus_reset_w, g_mus_reset)
XEN_NARGIFY_1(g_mus_offset_w, g_mus_offset)
XEN_NARGIFY_2(g_mus_set_offset_w, g_mus_set_offset)
XEN_NARGIFY_1(g_mus_frequency_w, g_mus_frequency)
XEN_NARGIFY_2(g_mus_set_frequency_w, g_mus_set_frequency)
XEN_NARGIFY_1(g_mus_length_w, g_mus_length)
XEN_NARGIFY_1(g_mus_file_name_w, g_mus_file_name)
XEN_NARGIFY_2(g_mus_set_length_w, g_mus_set_length)
XEN_NARGIFY_1(g_mus_type_w, g_mus_type)
XEN_NARGIFY_1(g_mus_order_w, g_mus_order)
XEN_NARGIFY_1(g_mus_data_w, g_mus_data)
XEN_NARGIFY_2(g_mus_set_data_w, g_mus_set_data)
XEN_NARGIFY_1(g_oscil_p_w, g_oscil_p)
#if (!HAVE_SCHEME)
XEN_ARGIFY_3(g_oscil_w, g_oscil)
#endif
XEN_VARGIFY(g_mus_apply_w, g_mus_apply)
XEN_VARGIFY(g_make_delay_w, g_make_delay)
XEN_VARGIFY(g_make_comb_w, g_make_comb)
XEN_VARGIFY(g_make_filtered_comb_w, g_make_filtered_comb)
XEN_VARGIFY(g_make_notch_w, g_make_notch)
XEN_VARGIFY(g_make_all_pass_w, g_make_all_pass)
XEN_VARGIFY(g_make_moving_average_w, g_make_moving_average)
XEN_ARGIFY_3(g_delay_w, g_delay)
XEN_ARGIFY_2(g_delay_tick_w, g_delay_tick)
XEN_ARGIFY_2(g_tap_w, g_tap)
XEN_ARGIFY_3(g_notch_w, g_notch)
XEN_ARGIFY_3(g_comb_w, g_comb)
XEN_ARGIFY_3(g_filtered_comb_w, g_filtered_comb)
XEN_ARGIFY_3(g_all_pass_w, g_all_pass)
XEN_ARGIFY_2(g_moving_average_w, g_moving_average)
XEN_NARGIFY_1(g_delay_p_w, g_delay_p)
XEN_NARGIFY_1(g_notch_p_w, g_notch_p)
XEN_NARGIFY_1(g_comb_p_w, g_comb_p)
XEN_NARGIFY_1(g_filtered_comb_p_w, g_filtered_comb_p)
XEN_NARGIFY_1(g_all_pass_p_w, g_all_pass_p)
XEN_NARGIFY_1(g_moving_average_p_w, g_moving_average_p)

XEN_ARGIFY_2(g_ncos_w, g_ncos)
XEN_NARGIFY_1(g_ncos_p_w, g_ncos_p)
XEN_ARGIFY_2(g_nsin_w, g_nsin)
XEN_NARGIFY_1(g_nsin_p_w, g_nsin_p)

XEN_VARGIFY(g_make_rand_w, g_make_rand)
XEN_VARGIFY(g_make_rand_interp_w, g_make_rand_interp)
XEN_ARGIFY_2(g_rand_w, g_rand)
XEN_ARGIFY_2(g_rand_interp_w, g_rand_interp)
XEN_NARGIFY_1(g_rand_p_w, g_rand_p)
XEN_NARGIFY_1(g_rand_interp_p_w, g_rand_interp_p)
XEN_NARGIFY_1(g_mus_random_w, g_mus_random)
XEN_NARGIFY_0(g_mus_rand_seed_w, g_mus_rand_seed)
XEN_NARGIFY_1(g_mus_set_rand_seed_w, g_mus_set_rand_seed)
XEN_NARGIFY_1(g_table_lookup_p_w, g_table_lookup_p)
XEN_VARGIFY(g_make_table_lookup_w, g_make_table_lookup)
XEN_ARGIFY_2(g_table_lookup_w, g_table_lookup)
XEN_ARGIFY_3(g_partials_to_wave_w, g_partials_to_wave)
XEN_ARGIFY_3(g_phase_partials_to_wave_w, g_phase_partials_to_wave)
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
XEN_ARGIFY_3(g_formant_bank_w, g_formant_bank)

XEN_NARGIFY_1(g_formant_p_w, g_formant_p)
XEN_ARGIFY_4(g_make_formant_w, g_make_formant)
XEN_ARGIFY_3(g_formant_w, g_formant)

XEN_NARGIFY_1(g_firmant_p_w, g_firmant_p)
XEN_ARGIFY_4(g_make_firmant_w, g_make_firmant)
XEN_ARGIFY_3(g_firmant_w, g_firmant)

XEN_NARGIFY_3(g_set_formant_radius_and_frequency_w, g_set_formant_radius_and_frequency)
XEN_VARGIFY(g_make_frame_w, g_make_frame)
XEN_VARGIFY(g_make_frame_unchecked_w, g_make_frame_unchecked)
XEN_VARGIFY(g_frame_w, g_frame)
XEN_NARGIFY_1(g_frame_p_w, g_frame_p)
XEN_ARGIFY_3(g_frame_add_w, g_frame_add)
XEN_ARGIFY_3(g_frame_multiply_w, g_frame_multiply)
XEN_NARGIFY_2(g_frame_ref_w, g_frame_ref)
XEN_NARGIFY_3(g_frame_set_w, g_frame_set)
XEN_VARGIFY(g_make_mixer_w, g_make_mixer)
XEN_VARGIFY(g_make_mixer_unchecked_w, g_make_mixer_unchecked)
XEN_VARGIFY(g_mixer_w, g_mixer)
XEN_NARGIFY_1(g_mixer_p_w, g_mixer_p)
XEN_ARGIFY_3(g_mixer_multiply_w, g_mixer_multiply)
XEN_ARGIFY_3(g_mixer_add_w, g_mixer_add)
XEN_NARGIFY_2(g_make_scalar_mixer_w, g_make_scalar_mixer)
XEN_NARGIFY_3(g_mixer_ref_w, g_mixer_ref)
XEN_NARGIFY_4(g_mixer_set_w, g_mixer_set)
XEN_NARGIFY_2(g_frame_to_sample_w, g_frame_to_sample)
XEN_NARGIFY_1(g_frame_to_list_w, g_frame_to_list)
XEN_ARGIFY_3(g_frame_to_frame_w, g_frame_to_frame)
XEN_ARGIFY_3(g_sample_to_frame_w, g_sample_to_frame)
XEN_VARGIFY(g_make_wave_train_w, g_make_wave_train)
XEN_ARGIFY_2(g_wave_train_w, g_wave_train)
XEN_NARGIFY_1(g_wave_train_p_w, g_wave_train_p)
XEN_VARGIFY(g_make_polyshape_w, g_make_polyshape)
XEN_ARGIFY_3(g_polyshape_w, g_polyshape)
XEN_NARGIFY_1(g_polyshape_p_w, g_polyshape_p)
XEN_ARGIFY_2(g_partials_to_polynomial_w, g_partials_to_polynomial)
XEN_NARGIFY_1(g_normalize_partials_w, g_normalize_partials)
XEN_NARGIFY_2(g_chebyshev_t_sum_w, g_chebyshev_t_sum)
XEN_NARGIFY_2(g_chebyshev_u_sum_w, g_chebyshev_u_sum)
XEN_NARGIFY_3(g_chebyshev_tu_sum_w, g_chebyshev_tu_sum)
XEN_VARGIFY(g_make_polywave_w, g_make_polywave)
XEN_ARGIFY_2(g_polywave_w, g_polywave)
XEN_NARGIFY_1(g_polywave_p_w, g_polywave_p)

XEN_VARGIFY(g_make_nrxysin_w, g_make_nrxysin)
XEN_ARGIFY_2(g_nrxysin_w, g_nrxysin)
XEN_NARGIFY_1(g_nrxysin_p_w, g_nrxysin_p)
XEN_VARGIFY(g_make_nrxycos_w, g_make_nrxycos)
XEN_ARGIFY_2(g_nrxycos_w, g_nrxycos)
XEN_NARGIFY_1(g_nrxycos_p_w, g_nrxycos_p)

XEN_ARGIFY_6(g_make_filter_w, g_make_filter)
XEN_NARGIFY_2(g_filter_w, g_filter)
XEN_NARGIFY_1(g_filter_p_w, g_filter_p)
XEN_ARGIFY_4(g_make_fir_filter_w, g_make_fir_filter)
XEN_NARGIFY_2(g_make_fir_coeffs_w, g_make_fir_coeffs)
XEN_NARGIFY_2(g_fir_filter_w, g_fir_filter)
XEN_NARGIFY_1(g_fir_filter_p_w, g_fir_filter_p)
XEN_ARGIFY_4(g_make_iir_filter_w, g_make_iir_filter)
XEN_NARGIFY_2(g_iir_filter_w, g_iir_filter)
XEN_NARGIFY_1(g_iir_filter_p_w, g_iir_filter_p)
XEN_NARGIFY_1(g_mus_xcoeffs_w, g_mus_xcoeffs)
XEN_NARGIFY_1(g_mus_ycoeffs_w, g_mus_ycoeffs)
XEN_NARGIFY_2(g_mus_xcoeff_w, g_mus_xcoeff)
XEN_NARGIFY_3(g_mus_set_xcoeff_w, g_mus_set_xcoeff)
XEN_NARGIFY_2(g_mus_ycoeff_w, g_mus_ycoeff)
XEN_NARGIFY_3(g_mus_set_ycoeff_w, g_mus_set_ycoeff)
XEN_NARGIFY_1(g_env_p_w, g_env_p)
XEN_NARGIFY_1(g_env_w, g_env)
XEN_VARGIFY(g_make_env_w, g_make_env)
XEN_NARGIFY_2(g_env_interp_w, g_env_interp)
XEN_NARGIFY_2(g_env_any_w, g_env_any)
XEN_NARGIFY_1(g_file_to_sample_p_w, g_file_to_sample_p)
XEN_ARGIFY_2(g_make_file_to_sample_w, g_make_file_to_sample)
XEN_ARGIFY_3(g_file_to_sample_w, g_file_to_sample)
XEN_NARGIFY_1(g_file_to_frame_p_w, g_file_to_frame_p)
XEN_ARGIFY_2(g_make_file_to_frame_w, g_make_file_to_frame)
XEN_ARGIFY_3(g_file_to_frame_w, g_file_to_frame)
XEN_NARGIFY_1(g_sample_to_file_p_w, g_sample_to_file_p)
XEN_ARGIFY_5(g_make_sample_to_file_w, g_make_sample_to_file)
XEN_NARGIFY_1(g_continue_sample_to_file_w, g_continue_sample_to_file)
XEN_NARGIFY_1(g_continue_frame_to_file_w, g_continue_frame_to_file)
XEN_NARGIFY_4(g_sample_to_file_w, g_sample_to_file)
XEN_NARGIFY_2(g_sample_to_file_add_w, g_sample_to_file_add)
XEN_NARGIFY_1(g_frame_to_file_p_w, g_frame_to_file_p)
XEN_NARGIFY_3(g_frame_to_file_w, g_frame_to_file)
XEN_ARGIFY_5(g_make_frame_to_file_w, g_make_frame_to_file)
XEN_NARGIFY_1(g_input_p_w, g_input_p)
XEN_NARGIFY_1(g_output_p_w, g_output_p)
XEN_NARGIFY_3(g_in_any_w, g_in_any)
XEN_NARGIFY_2(g_ina_w, g_ina)
XEN_NARGIFY_2(g_inb_w, g_inb)
XEN_ARGIFY_4(g_out_any_w, g_out_any)
XEN_ARGIFY_3(g_outa_w, g_outa)
XEN_ARGIFY_3(g_outb_w, g_outb)
XEN_ARGIFY_3(g_outc_w, g_outc)
XEN_ARGIFY_3(g_outd_w, g_outd)
XEN_NARGIFY_1(g_mus_close_w, g_mus_close)
XEN_NARGIFY_0(g_mus_file_buffer_size_w, g_mus_file_buffer_size)
XEN_NARGIFY_1(g_mus_set_file_buffer_size_w, g_mus_set_file_buffer_size)
XEN_NARGIFY_1(g_readin_p_w, g_readin_p)
XEN_NARGIFY_1(g_readin_w, g_readin)
XEN_VARGIFY(g_make_readin_w, g_make_readin)
XEN_NARGIFY_1(g_mus_channel_w, g_mus_channel)
XEN_NARGIFY_1(g_mus_interp_type_w, g_mus_interp_type)
XEN_NARGIFY_1(g_mus_location_w, g_mus_location)
XEN_NARGIFY_2(g_mus_set_location_w, g_mus_set_location)
XEN_NARGIFY_1(g_mus_increment_w, g_mus_increment)
XEN_NARGIFY_2(g_mus_set_increment_w, g_mus_set_increment)
XEN_NARGIFY_1(g_mus_feedback_w, g_mus_feedback)
XEN_NARGIFY_2(g_mus_set_feedback_w, g_mus_set_feedback)
XEN_NARGIFY_1(g_locsig_p_w, g_locsig_p)
XEN_NARGIFY_3(g_locsig_w, g_locsig)
XEN_VARGIFY(g_make_locsig_w, g_make_locsig)
XEN_NARGIFY_3(g_move_locsig_w, g_move_locsig)
XEN_NARGIFY_0(g_locsig_type_w, g_locsig_type)
XEN_NARGIFY_1(g_set_locsig_type_w, g_set_locsig_type)
XEN_NARGIFY_1(g_mus_channels_w, g_mus_channels)
XEN_NARGIFY_2(g_locsig_ref_w, g_locsig_ref)
XEN_NARGIFY_2(g_locsig_reverb_ref_w, g_locsig_reverb_ref)
XEN_NARGIFY_3(g_locsig_set_w, g_locsig_set)
XEN_NARGIFY_3(g_locsig_reverb_set_w, g_locsig_reverb_set)
XEN_NARGIFY_1(g_move_sound_p_w, g_move_sound_p)
XEN_NARGIFY_3(g_move_sound_w, g_move_sound)
XEN_ARGIFY_3(g_make_move_sound_w, g_make_move_sound)
XEN_NARGIFY_0(g_mus_clear_sincs_w, g_mus_clear_sincs)
XEN_NARGIFY_1(g_src_p_w, g_src_p)
XEN_ARGIFY_3(g_src_w, g_src)
XEN_ARGIFY_6(g_make_src_w, g_make_src)
XEN_NARGIFY_1(g_granulate_p_w, g_granulate_p)
XEN_ARGIFY_3(g_granulate_w, g_granulate)
XEN_VARGIFY(g_make_granulate_w, g_make_granulate)
XEN_NARGIFY_1(g_mus_ramp_w, g_mus_ramp)
XEN_NARGIFY_2(g_mus_set_ramp_w, g_mus_set_ramp)
XEN_NARGIFY_1(g_convolve_p_w, g_convolve_p)
XEN_ARGIFY_2(g_convolve_w, g_convolve)
XEN_VARGIFY(g_make_convolve_w, g_make_convolve)
XEN_ARGIFY_4(g_convolve_files_w, g_convolve_files)
XEN_NARGIFY_1(g_phase_vocoder_p_w, g_phase_vocoder_p)
XEN_ARGIFY_5(g_phase_vocoder_w, g_phase_vocoder)
XEN_VARGIFY(g_make_phase_vocoder_w, g_make_phase_vocoder)
XEN_NARGIFY_1(g_phase_vocoder_amp_increments_w, g_phase_vocoder_amp_increments)
XEN_NARGIFY_1(g_phase_vocoder_amps_w, g_phase_vocoder_amps)
XEN_NARGIFY_1(g_phase_vocoder_freqs_w, g_phase_vocoder_freqs)
XEN_NARGIFY_1(g_phase_vocoder_phases_w, g_phase_vocoder_phases)
XEN_NARGIFY_1(g_phase_vocoder_phase_increments_w, g_phase_vocoder_phase_increments)
XEN_NARGIFY_1(g_mus_hop_w, g_mus_hop)
XEN_NARGIFY_2(g_mus_set_hop_w, g_mus_set_hop)
XEN_ARGIFY_7(g_mus_mix_w, g_mus_mix)
XEN_ARGIFY_4(g_make_ssb_am_w, g_make_ssb_am)
XEN_ARGIFY_3(g_ssb_am_w, g_ssb_am)
XEN_NARGIFY_1(g_ssb_am_p_w, g_ssb_am_p)
XEN_NARGIFY_4(g_ssb_bank_w, g_ssb_bank)
XEN_NARGIFY_0(g_clm_table_size_w, g_clm_table_size)
XEN_NARGIFY_1(g_set_clm_table_size_w, g_set_clm_table_size)
XEN_NARGIFY_0(g_clm_default_frequency_w, g_clm_default_frequency)
XEN_NARGIFY_1(g_set_clm_default_frequency_w, g_set_clm_default_frequency)
XEN_NARGIFY_1(g_mus_generator_p_w, g_mus_generator_p)
XEN_NARGIFY_1(g_mus_frandom_w, g_mus_frandom)
XEN_NARGIFY_1(g_mus_irandom_w, g_mus_irandom)
XEN_ARGIFY_4(g_make_oscil_w, g_make_oscil)
XEN_ARGIFY_4(g_make_ncos_w, g_make_ncos)
XEN_ARGIFY_4(g_make_nsin_w, g_make_nsin)
XEN_ARGIFY_8(g_make_asymmetric_fm_w, g_make_asymmetric_fm)

#else
#define g_mus_srate_w g_mus_srate
#define g_mus_set_srate_w g_mus_set_srate
#define g_mus_float_equal_fudge_factor_w g_mus_float_equal_fudge_factor
#define g_mus_set_float_equal_fudge_factor_w g_mus_set_float_equal_fudge_factor
#define g_mus_array_print_length_w g_mus_array_print_length
#define g_mus_set_array_print_length_w g_mus_set_array_print_length
#define g_radians_to_hz_w g_radians_to_hz
#define g_hz_to_radians_w g_hz_to_radians
#define g_radians_to_degrees_w g_radians_to_degrees
#define g_degrees_to_radians_w g_degrees_to_radians
#define g_db_to_linear_w g_db_to_linear
#define g_linear_to_db_w g_linear_to_db
#define g_seconds_to_samples_w g_seconds_to_samples
#define g_samples_to_seconds_w g_samples_to_seconds
#define g_ring_modulate_w g_ring_modulate
#define g_amplitude_modulate_w g_amplitude_modulate
#define g_contrast_enhancement_w g_contrast_enhancement
#define g_dot_product_w g_dot_product
#if HAVE_COMPLEX_TRIG && XEN_HAVE_COMPLEX_NUMBERS
#define g_edot_product_w g_edot_product
#endif
#define g_clear_array_w g_clear_array
#define g_polynomial_w g_polynomial
#define g_multiply_arrays_w g_multiply_arrays
#define g_make_fft_window_w g_make_fft_window
#define g_mus_fft_w g_mus_fft
#define g_spectrum_w g_spectrum
#define g_autocorrelate_w g_autocorrelate
#define g_correlate_w g_correlate
#define g_convolution_w g_convolution
#define g_rectangular_to_polar_w g_rectangular_to_polar
#define g_rectangular_to_magnitudes_w g_rectangular_to_magnitudes
#define g_polar_to_rectangular_w g_polar_to_rectangular
#define g_array_interp_w g_array_interp
#define g_mus_interpolate_w g_mus_interpolate
#define g_mus_describe_w g_mus_describe
#define g_mus_name_w g_mus_name
#define g_mus_set_name_w g_mus_set_name
#define g_mus_run_w g_mus_run
#define g_mus_phase_w g_mus_phase
#define g_mus_set_phase_w g_mus_set_phase
#define g_mus_scaler_w g_mus_scaler
#define g_mus_set_scaler_w g_mus_set_scaler
#define g_mus_safety_w g_mus_safety
#define g_mus_set_safety_w g_mus_set_safety
#define g_mus_feedforward_w g_mus_feedforward
#define g_mus_set_feedforward_w g_mus_set_feedforward
#define g_mus_width_w g_mus_width
#define g_mus_set_width_w g_mus_set_width
#define g_mus_reset_w g_mus_reset
#define g_mus_offset_w g_mus_offset
#define g_mus_set_offset_w g_mus_set_offset
#define g_mus_frequency_w g_mus_frequency
#define g_mus_set_frequency_w g_mus_set_frequency
#define g_mus_length_w g_mus_length
#define g_mus_type_w g_mus_type
#define g_mus_order_w g_mus_order
#define g_mus_file_name_w g_mus_file_name
#define g_mus_set_length_w g_mus_set_length
#define g_mus_data_w g_mus_data
#define g_mus_set_data_w g_mus_set_data
#define g_oscil_p_w g_oscil_p
#define g_make_oscil_w g_make_oscil
#define g_oscil_w g_oscil
#define g_mus_apply_w g_mus_apply
#define g_make_delay_w g_make_delay
#define g_make_comb_w g_make_comb
#define g_make_filtered_comb_w g_make_filtered_comb
#define g_make_notch_w g_make_notch
#define g_make_all_pass_w g_make_all_pass
#define g_make_moving_average_w g_make_moving_average
#define g_delay_w g_delay
#define g_delay_tick_w g_delay_tick
#define g_tap_w g_tap
#define g_notch_w g_notch
#define g_comb_w g_comb
#define g_filtered_comb_w g_filtered_comb
#define g_all_pass_w g_all_pass
#define g_moving_average_w g_moving_average
#define g_delay_p_w g_delay_p
#define g_notch_p_w g_notch_p
#define g_comb_p_w g_comb_p
#define g_filtered_comb_p_w g_filtered_comb_p
#define g_all_pass_p_w g_all_pass_p
#define g_moving_average_p_w g_moving_average_p

#define g_make_ncos_w g_make_ncos
#define g_ncos_w g_ncos
#define g_ncos_p_w g_ncos_p
#define g_make_nsin_w g_make_nsin
#define g_nsin_w g_nsin
#define g_nsin_p_w g_nsin_p

#define g_make_rand_w g_make_rand
#define g_make_rand_interp_w g_make_rand_interp
#define g_rand_w g_rand
#define g_rand_interp_w g_rand_interp
#define g_rand_p_w g_rand_p
#define g_rand_interp_p_w g_rand_interp_p
#define g_mus_random_w g_mus_random
#define g_mus_rand_seed_w g_mus_rand_seed
#define g_mus_set_rand_seed_w g_mus_set_rand_seed
#define g_table_lookup_p_w g_table_lookup_p
#define g_make_table_lookup_w g_make_table_lookup
#define g_table_lookup_w g_table_lookup
#define g_partials_to_wave_w g_partials_to_wave
#define g_phase_partials_to_wave_w g_phase_partials_to_wave
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
#define g_formant_bank_w g_formant_bank

#define g_formant_p_w g_formant_p
#define g_make_formant_w g_make_formant
#define g_formant_w g_formant

#define g_firmant_p_w g_firmant_p
#define g_make_firmant_w g_make_firmant
#define g_firmant_w g_firmant

#define g_set_formant_radius_and_frequency_w g_set_formant_radius_and_frequency
#define g_make_frame_w g_make_frame
#define g_make_frame_unchecked_w g_make_frame_unchecked
#define g_frame_w g_frame
#define g_frame_p_w g_frame_p
#define g_frame_add_w g_frame_add
#define g_frame_multiply_w g_frame_multiply
#define g_frame_ref_w g_frame_ref
#define g_frame_set_w g_frame_set
#define g_make_mixer_w g_make_mixer
#define g_make_mixer_unchecked_w g_make_mixer_unchecked
#define g_mixer_w g_mixer
#define g_mixer_p_w g_mixer_p
#define g_mixer_multiply_w g_mixer_multiply
#define g_mixer_add_w g_mixer_add
#define g_make_scalar_mixer_w g_make_scalar_mixer
#define g_mixer_ref_w g_mixer_ref
#define g_mixer_set_w g_mixer_set
#define g_frame_to_sample_w g_frame_to_sample
#define g_frame_to_list_w g_frame_to_list
#define g_frame_to_frame_w g_frame_to_frame
#define g_sample_to_frame_w g_sample_to_frame
#define g_make_wave_train_w g_make_wave_train
#define g_wave_train_w g_wave_train
#define g_wave_train_p_w g_wave_train_p
#define g_make_polyshape_w g_make_polyshape
#define g_polyshape_w g_polyshape
#define g_polyshape_p_w g_polyshape_p
#define g_partials_to_polynomial_w g_partials_to_polynomial
#define g_normalize_partials_w g_normalize_partials
#define g_chebyshev_t_sum_w g_chebyshev_t_sum
#define g_chebyshev_u_sum_w g_chebyshev_u_sum
#define g_chebyshev_tu_sum_w g_chebyshev_tu_sum
#define g_make_polywave_w g_make_polywave
#define g_polywave_w g_polywave
#define g_polywave_p_w g_polywave_p

#define g_make_nrxysin_w g_make_nrxysin
#define g_nrxysin_w g_nrxysin
#define g_nrxysin_p_w g_nrxysin_p
#define g_make_nrxycos_w g_make_nrxycos
#define g_nrxycos_w g_nrxycos
#define g_nrxycos_p_w g_nrxycos_p

#define g_make_filter_w g_make_filter
#define g_filter_w g_filter
#define g_filter_p_w g_filter_p
#define g_make_fir_filter_w g_make_fir_filter
#define g_make_fir_coeffs_w g_make_fir_coeffs
#define g_fir_filter_w g_fir_filter
#define g_fir_filter_p_w g_fir_filter_p
#define g_make_iir_filter_w g_make_iir_filter
#define g_iir_filter_w g_iir_filter
#define g_iir_filter_p_w g_iir_filter_p
#define g_mus_xcoeffs_w g_mus_xcoeffs
#define g_mus_xcoeff_w g_mus_xcoeff
#define g_mus_set_xcoeff_w g_mus_set_xcoeff
#define g_mus_ycoeffs_w g_mus_ycoeffs
#define g_mus_ycoeff_w g_mus_ycoeff
#define g_mus_set_ycoeff_w g_mus_set_ycoeff
#define g_env_p_w g_env_p
#define g_env_w g_env
#define g_make_env_w g_make_env
#define g_env_interp_w g_env_interp
#define g_env_any_w g_env_any
#define g_file_to_sample_p_w g_file_to_sample_p
#define g_make_file_to_sample_w g_make_file_to_sample
#define g_file_to_sample_w g_file_to_sample
#define g_file_to_frame_p_w g_file_to_frame_p
#define g_make_file_to_frame_w g_make_file_to_frame
#define g_file_to_frame_w g_file_to_frame
#define g_sample_to_file_p_w g_sample_to_file_p
#define g_make_sample_to_file_w g_make_sample_to_file
#define g_continue_sample_to_file_w g_continue_sample_to_file
#define g_continue_frame_to_file_w g_continue_frame_to_file
#define g_sample_to_file_w g_sample_to_file
#define g_sample_to_file_add_w g_sample_to_file_add
#define g_frame_to_file_p_w g_frame_to_file_p
#define g_frame_to_file_w g_frame_to_file
#define g_make_frame_to_file_w g_make_frame_to_file
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
#define g_mus_close_w g_mus_close
#define g_mus_file_buffer_size_w g_mus_file_buffer_size
#define g_mus_set_file_buffer_size_w g_mus_set_file_buffer_size
#define g_readin_p_w g_readin_p
#define g_readin_w g_readin
#define g_make_readin_w g_make_readin
#define g_mus_channel_w g_mus_channel
#define g_mus_interp_type_w g_mus_interp_type
#define g_mus_location_w g_mus_location
#define g_mus_set_location_w g_mus_set_location
#define g_mus_increment_w g_mus_increment
#define g_mus_set_increment_w g_mus_set_increment
#define g_mus_feedback_w g_mus_feedback
#define g_mus_set_feedback_w g_mus_set_feedback
#define g_locsig_p_w g_locsig_p
#define g_locsig_w g_locsig
#define g_make_locsig_w g_make_locsig
#define g_move_locsig_w g_move_locsig
#define g_locsig_type_w g_locsig_type
#define g_set_locsig_type_w g_set_locsig_type
#define g_mus_channels_w g_mus_channels
#define g_locsig_ref_w g_locsig_ref
#define g_locsig_reverb_ref_w g_locsig_reverb_ref
#define g_locsig_set_w g_locsig_set
#define g_locsig_reverb_set_w g_locsig_reverb_set
#define g_move_sound_p_w g_move_sound_p
#define g_move_sound_w g_move_sound
#define g_make_move_sound_w g_make_move_sound
#define g_mus_clear_sincs_w g_mus_clear_sincs
#define g_src_p_w g_src_p
#define g_src_w g_src
#define g_make_src_w g_make_src
#define g_granulate_p_w g_granulate_p
#define g_granulate_w g_granulate
#define g_make_granulate_w g_make_granulate
#define g_mus_ramp_w g_mus_ramp
#define g_mus_set_ramp_w g_mus_set_ramp
#define g_convolve_p_w g_convolve_p
#define g_convolve_w g_convolve
#define g_make_convolve_w g_make_convolve
#define g_convolve_files_w g_convolve_files
#define g_phase_vocoder_p_w g_phase_vocoder_p
#define g_phase_vocoder_w g_phase_vocoder
#define g_make_phase_vocoder_w g_make_phase_vocoder
#define g_phase_vocoder_amp_increments_w g_phase_vocoder_amp_increments
#define g_phase_vocoder_amps_w g_phase_vocoder_amps
#define g_phase_vocoder_freqs_w g_phase_vocoder_freqs
#define g_phase_vocoder_phases_w g_phase_vocoder_phases
#define g_phase_vocoder_phase_increments_w g_phase_vocoder_phase_increments
#define g_mus_hop_w g_mus_hop
#define g_mus_set_hop_w g_mus_set_hop
#define g_mus_mix_w g_mus_mix
#define g_make_ssb_am_w g_make_ssb_am
#define g_ssb_am_w g_ssb_am
#define g_ssb_am_p_w g_ssb_am_p
#define g_ssb_bank_w g_ssb_bank
#define g_clm_table_size_w g_clm_table_size
#define g_set_clm_table_size_w g_set_clm_table_size
#define g_clm_default_frequency_w g_clm_default_frequency
#define g_set_clm_default_frequency_w g_set_clm_default_frequency
#define g_mus_generator_p_w g_mus_generator_p
#define g_mus_frandom_w g_mus_frandom
#define g_mus_irandom_w g_mus_irandom
#endif


/* -------------------------------- scheme-side (run) optimization -------------------------------- */

#if HAVE_SCHEME
#define car(E)    s7_car(E)
#define cdr(E)    s7_cdr(E)
#define cadr(E)   s7_cadr(E)
#define caddr(E)  s7_caddr(E)
#define cadddr(E) s7_cadddr(E)
#define cddr(E)   s7_cddr(E)
#define cadar(E)  s7_cadar(E)
#define slot_value(E) s7_slot_value(s7, E)

static mus_float_t mus_nsin_unmodulated(mus_any *p) {return(mus_nsin(p, 0.0));}
static mus_float_t mus_ncos_unmodulated(mus_any *p) {return(mus_ncos(p, 0.0));}
static mus_float_t mus_nrxysin_unmodulated(mus_any *p) {return(mus_nrxysin(p, 0.0));}
static mus_float_t mus_nrxycos_unmodulated(mus_any *p) {return(mus_nrxycos(p, 0.0));}
static mus_float_t mus_square_wave_unmodulated(mus_any *p) {return(mus_square_wave(p, 0.0));}
static mus_float_t mus_sawtooth_wave_unmodulated(mus_any *p) {return(mus_sawtooth_wave(p, 0.0));}
static mus_float_t mus_pulse_train_unmodulated(mus_any *p) {return(mus_pulse_train(p, 0.0));}
static mus_float_t mus_triangle_wave_unmodulated(mus_any *p) {return(mus_triangle_wave(p, 0.0));}


static mus_any *get_generator(s7_scheme *sc, s7_pointer sym)
{
  s7_pointer g;
  g = s7_symbol_value(sc, sym);
  if (s7_object_type(g) == mus_xen_tag)
    return((mus_any *)(((mus_xen *)s7_object_value(g))->gen));
  return(NULL);
}

#define GET_GENERATOR(Obj, Type, Safe, Val)		\
  do { s7_pointer _Obj_ = Obj; mus_any *_Val_ = NULL;	\
       if (Safe) \
         _Val_ = (mus_any *)s7_symbol_accessor_data(_Obj_); \
       if (!_Val_) \
         { \
           s7_pointer g; \
           g = s7_symbol_value(sc, _Obj_); \
           if (s7_object_type(g) == mus_xen_tag)    \
	     { \
	       _Val_ = (mus_any *)(((mus_xen *)s7_object_value(g))->gen); \
	       if (s7_is_do_global(sc, _Obj_)) \
	         s7_symbol_set_accessor_data(_Obj_, (void *)_Val_); \
	     } \
           if ((!_Val_) || (!mus_ ## Type ## _p(_Val_))) \
	     XEN_ASSERT_TYPE(false, g, XEN_ARG_1, S_ ## Type, #Type " generator"); \
         } \
       Val = _Val_; } while (0)


#define GET_NUMBER(Obj, Caller, Safe, Val)			\
  do { s7_pointer _Obj_ = Obj; s7_pointer _Val_ = NULL;	\
       if (Safe) \
         _Val_ = (s7_pointer)s7_symbol_accessor_data(_Obj_); \
       if (!_Val_) \
         { \
           _Val_ = s7_slot(sc, _Obj_); \
           if (s7_is_do_local_or_global(sc, _Obj_)) \
 	     s7_symbol_set_accessor_data(_Obj_, (void *)_Val_); \
           if (!s7_is_number(slot_value(_Val_))) \
	     XEN_ASSERT_TYPE(false, _Val_, XEN_ARG_2, Caller, "a number"); \
         } \
       Val = slot_value(_Val_); } while (0)


#define GET_REAL(Obj, Caller, Safe, Val)			\
  do { s7_pointer _Obj_ = Obj; s7_pointer _Val_ = NULL;	\
       if (Safe) \
         _Val_ = (s7_pointer)s7_symbol_accessor_data(_Obj_); \
       if (!_Val_) \
         { \
           _Val_ = s7_slot(sc, _Obj_); \
           if (s7_is_do_local_or_global(sc, _Obj_)) \
 	     s7_symbol_set_accessor_data(_Obj_, (void *)_Val_); \
           if (!s7_is_real(slot_value(_Val_))) \
	     XEN_ASSERT_TYPE(false, _Val_, XEN_ARG_2, S_ ## Caller, "a real"); \
         } \
       Val = s7_number_to_real(slot_value(_Val_)); } while (0)


#define GET_INTEGER(Obj, Caller, Safe, Val)			\
  do { s7_pointer _Obj_ = Obj; s7_pointer _Val_ = NULL;	\
       if (Safe) \
         _Val_ = (s7_pointer)s7_symbol_accessor_data(_Obj_); \
       if (!_Val_) \
         { \
           _Val_ = s7_slot(sc, _Obj_); \
           if (s7_is_do_local_or_global(sc, _Obj_)) \
 	     s7_symbol_set_accessor_data(_Obj_, (void *)_Val_); \
           if (!s7_is_integer(slot_value(_Val_))) \
	     XEN_ASSERT_TYPE(false, _Val_, XEN_ARG_2, S_ ## Caller, "an integer"); \
         } \
       Val = s7_integer(slot_value(_Val_)); } while (0)


#define GEN_1(Type, Func) \
  static s7_pointer Type ## _1; \
  static s7_pointer g_ ## Type ## _1(s7_scheme *sc, s7_pointer args) \
  { \
    mus_any *_o_;	  \
    GET_GENERATOR(car(args), Type, s7_in_safe_do(sc), _o_); \
    return(s7_make_real(sc, Func(_o_))); \
  } \
  static s7_pointer mul_c_ ## Type ## _1; \
  static s7_pointer g_mul_c_ ## Type ## _1(s7_scheme *sc, s7_pointer args) \
  { \
    mus_any *_o_; \
    double _mul_;    \
    bool _its_safe_; \
    _its_safe_ = s7_in_safe_do(sc); \
   \
    _mul_ = s7_number_to_real(car(args)); /* we checked that it's not complex in the chooser */	\
    GET_GENERATOR(s7_cadr(s7_cadr(args)), Type, _its_safe_, _o_);	\
    return(s7_make_real(sc, _mul_ * Func(_o_))); \
  } \
  static s7_pointer mul_s_ ## Type ## _1; \
  static s7_pointer g_mul_s_ ## Type ## _1(s7_scheme *sc, s7_pointer args) \
  { \
    mus_any *_o_; \
    s7_pointer _mul_;    \
    double _f_; \
    bool _its_safe_; \
    _its_safe_ = s7_in_safe_do(sc); \
   \
    GET_NUMBER(car(args), "*", _its_safe_, _mul_); \
    GET_GENERATOR(s7_cadr(s7_cadr(args)), Type, _its_safe_, _o_);	\
    _f_ = Func(_o_); \
    if (s7_is_real(_mul_))				      \
      return(s7_make_real(sc, s7_number_to_real(_mul_) * _f_)); \
    return(s7_make_complex(sc, s7_real_part(_mul_) * _f_, s7_imag_part(_mul_) * _f_)); \
  } \
  static s7_pointer env_ ## Type ## _1; \
  static s7_pointer g_env_ ## Type ## _1(s7_scheme *sc, s7_pointer args) \
  { \
    mus_any *_o_, *_e_;  \
    bool _its_safe_; \
    _its_safe_ = s7_in_safe_do(sc); \
   \
    GET_GENERATOR(s7_cadr(car(args)), env, _its_safe_, _e_);		\
    GET_GENERATOR(s7_cadr(s7_cadr(args)), Type, _its_safe_, _o_);	\
    return(s7_make_real(sc, mus_env(_e_) * Func(_o_)));		\
  }

/* (define (hi) (let ((o (make-oscil 440.0)) (scl 0+i)) (oscil o) (* scl (oscil o))))
 * (define (hi) (let ((o (make-oscil 440.0)) (scl 0+i) (val 0.0)) (do ((i 0 (+ i 1))) ((= i 3)) (set! val (* scl (oscil o)))) val))
 * (define (hi) (let ((o (make-oscil 440.0)) (fm 1.0) (scl 0+i) (val 0.0)) (do ((i 0 (+ i 1))) ((= i 3)) (set! val (* scl (oscil o fm)))) val))
 * (define (hi) (let ((o (make-oscil 440.0)) (fm 0.0) (scl 1.0) (val 0.0)) (do ((i 0 (+ i 1))) ((= i 3)) (set! val (* scl (oscil o fm)))) val))
 * (define (hi) (let ((o (make-oscil 440.0)) (fm 1.0) (scl 0.0) (val 0.0)) (do ((i 0 (+ i 1))) ((= i 3)) (set! val (* scl (oscil o fm)))) val))
 * (define (hi) (let ((o (make-oscil 440.0)) (fm 1.0) (scl 1.0) (val 0.0)) (do ((i 0 (+ i 1))) ((= i 3)) (set! val (* scl (oscil o fm)))) val))
 */

#define GEN_2(Type, Func) \
  static s7_pointer Type ## _2; \
  static s7_pointer g_ ## Type ## _2(s7_scheme *sc, s7_pointer args) \
  { \
    mus_any *_o_; \
    double _fm_; \
    bool _its_safe_; \
    _its_safe_ = s7_in_safe_do(sc); \
   \
    GET_GENERATOR(car(args), Type, _its_safe_, _o_); \
    GET_REAL(cadr(args), Type, _its_safe_, _fm_); \
    return(s7_make_real(sc, Func(_o_, _fm_))); \
  } \
  static s7_pointer mul_c_ ## Type ## _2; \
  static s7_pointer g_mul_c_ ## Type ## _2(s7_scheme *sc, s7_pointer args) \
  { \
    mus_any *_o_; \
    double _fm_, _mul_;    \
    bool _its_safe_; \
    _its_safe_ = s7_in_safe_do(sc); \
   \
    _mul_ = s7_number_to_real(car(args)); \
    GET_GENERATOR(s7_cadr(s7_cadr(args)), Type, _its_safe_, _o_);	\
    GET_REAL(s7_caddr(s7_cadr(args)), Type, _its_safe_, _fm_);		\
    return(s7_make_real(sc, _mul_ * Func(_o_, _fm_))); \
  } \
  static s7_pointer mul_s_ ## Type ## _2; \
  static s7_pointer g_mul_s_ ## Type ## _2(s7_scheme *sc, s7_pointer args) \
  { \
    mus_any *_o_; \
    double _fm_, _f_;     \
    s7_pointer _mul_; \
    bool _its_safe_; \
    _its_safe_ = s7_in_safe_do(sc); \
   \
    GET_NUMBER(car(args), "*", _its_safe_, _mul_); \
    GET_GENERATOR(s7_cadr(s7_cadr(args)), Type, _its_safe_, _o_);	\
    GET_REAL(s7_caddr(s7_cadr(args)), Type, _its_safe_, _fm_);		\
    _f_ = Func(_o_, _fm_);							\
    if (s7_is_real(_mul_)) \
      return(s7_make_real(sc, s7_number_to_real(_mul_) * _f_)); \
    return(s7_make_complex(sc, s7_real_part(_mul_) * _f_, s7_imag_part(_mul_) * _f_)); \
  } \
  static s7_pointer env_ ## Type ## _2; \
  static s7_pointer g_env_ ## Type ## _2(s7_scheme *sc, s7_pointer args) \
  { \
    mus_any *_o_, *_e_;  \
    double _fm_;    \
    bool _its_safe_; \
    _its_safe_ = s7_in_safe_do(sc); \
   \
    GET_GENERATOR(s7_cadr(car(args)), env, _its_safe_, _e_);		\
    GET_GENERATOR(s7_cadr(s7_cadr(args)), Type, _its_safe_, _o_);	\
    GET_REAL(s7_caddr(s7_cadr(args)), Type, _its_safe_, _fm_);		\
    return(s7_make_real(sc, mus_env(_e_) * Func(_o_, _fm_)));		\
  } \
  static s7_pointer direct_ ## Type ## _2; \
  static s7_pointer g_direct_ ## Type ## _2(s7_scheme *sc, s7_pointer args) \
  { \
    mus_any *_o_;	  \
    double _fm_; \
    s7_pointer rl; \
    _fm_ = s7_real(rl = s7_call_direct(sc, cadr(args)));	\
    GET_GENERATOR(car(args), Type, s7_in_safe_do(sc), _o_); \
    return(s7_remake_real(sc, rl, Func(_o_, _fm_)));		    \
  } \
  static s7_pointer indirect_ ## Type ## _2; \
  static s7_pointer g_indirect_ ## Type ## _2(s7_scheme *sc, s7_pointer args) \
  { \
    mus_any *_o_;	  \
    double _fm_; \
    _fm_ = s7_number_to_real(s7_call_direct(sc, cadr(args)));	\
    GET_GENERATOR(car(args), Type, s7_in_safe_do(sc), _o_); \
    return(s7_make_real(sc, Func(_o_, _fm_)));		    \
  }

 

GEN_1(oscil, mus_oscil_unmodulated)
GEN_2(oscil, mus_oscil_fm)

GEN_1(polywave, mus_polywave_unmodulated)
GEN_2(polywave, mus_polywave)

GEN_1(rand, mus_rand_unmodulated)
GEN_1(rand_interp, mus_rand_interp_unmodulated)
GEN_1(table_lookup, mus_table_lookup_unmodulated)
GEN_1(wave_train, mus_wave_train_unmodulated)
GEN_1(pulse_train, mus_pulse_train_unmodulated)
GEN_1(triangle_wave, mus_triangle_wave_unmodulated)
GEN_1(square_wave, mus_square_wave_unmodulated)
GEN_1(sawtooth_wave, mus_sawtooth_wave_unmodulated)
GEN_1(env, mus_env)
GEN_1(ncos, mus_ncos_unmodulated)
GEN_1(nsin, mus_nsin_unmodulated)
GEN_1(nrxycos, mus_nrxycos_unmodulated)
GEN_1(nrxysin, mus_nrxysin_unmodulated)

GEN_1(readin, mus_readin)

GEN_2(comb, mus_comb_unmodulated_noz)
GEN_2(notch, mus_notch_unmodulated_noz)
GEN_2(all_pass, mus_all_pass_unmodulated_noz)
GEN_2(delay, mus_delay_unmodulated_noz)

GEN_2(moving_average, mus_moving_average)
GEN_2(rand, mus_rand)
GEN_2(rand_interp, mus_rand_interp)
GEN_2(ncos, mus_ncos)
GEN_2(nsin, mus_nsin)
GEN_2(sawtooth_wave, mus_sawtooth_wave)
GEN_2(pulse_train, mus_pulse_train)
GEN_2(square_wave, mus_square_wave)
GEN_2(triangle_wave, mus_triangle_wave)
GEN_2(nrxysin, mus_nrxysin)
GEN_2(nrxycos, mus_nrxycos)
GEN_2(one_zero, mus_one_zero)
GEN_2(one_pole, mus_one_pole)
GEN_2(two_zero, mus_two_zero)
GEN_2(two_pole, mus_two_pole)
GEN_2(filter, mus_filter)
GEN_2(fir_filter, mus_fir_filter)
GEN_2(iir_filter, mus_iir_filter)

GEN_2(formant, mus_formant)
GEN_2(firmant, mus_firmant)

GEN_2(wave_train, mus_wave_train)
GEN_2(table_lookup, mus_table_lookup)

GEN_2(ssb_am, mus_ssb_am_unmodulated)
GEN_2(asymmetric_fm, mus_asymmetric_fm_unmodulated)
GEN_2(polyshape, mus_polyshape_unmodulated)
GEN_2(filtered_comb, mus_filtered_comb_unmodulated)

/*
GEN3(ssb_am)         ;(ssb-am gen (insig 0.0) (fm 0.0)), mus_ssb_am_unmodulated
GEN3(asymmetric_fm)  ;(asymmetric-fm gen (index 0.0) (fm 0.0)), mus_*_no_input(1), unmod(2)
GEN3(polyshape)      ;(polyshape gen (index 1.0) (fm 0.0)), unmod(2), no_input(1)
GEN3(filtered_comb)  ;(filtered-comb gen (val 0.0) (pm 0.0)), unmod(1)

GEN3(locsig)         ;(locsig gen loc val)
GEN3(move-sound)     ;(move-sound gen loc val)

GEN_P(file_to_sample)
GEN_P(sample_to_file)
GEN_P(file_to_frame)
GEN_P(frame_to_file)

MUS_SRC, 
MUS_GRANULATE,
MUS_CONVOLVE,
MUS_PHASE_VOCODER,

;(set! (mus-location ...) ...)?
*/


/* for the noz part, run seems to be checking that safety==0 -- this is zdly!
 *   that the gen arg is correct, but we check that too.
 * perhaps size must be > 0?  -- make this choice at init time (how?)
 */

/* clm changes: the list-as-generator business needs to be fixed
 *         see snd_to_sample in snd-edits for an example, defgenerator is in ws.scm
 *         need to export mus_make_class_tag to ffi for the type, then add a way
 *         to associate a xen func with a class field via a wrapper.  We also need
 *         a way to access the current "'tag" (type) for xen level type checks
 *         (oscil? looks at the current mus_any->type and compares it to oscil's).
 * [mus-type -- done], mus-make-type(name)
 * mus-method, mus-set-method
 * mus-make-any [make a bare mus_any class with a settable type and class ptr,
 *   is passed known type, use associated class] use mus_any for the C side pointer
 *   as in clm.c mus_set_name.
 * the class pointer can use XEN_WRAP_C_POINTER and friends
 * method(cls, name), g? needs type embedded as in s_type case
 *     (begin (define all names to #f)
 *        (let ((gen-type mus-make-type gen-name))
 *          (set! ,(string->symbol (string-append sname "?")) (lambda (obj) (= (mus-type obj) gen-type)))
 *          ;; but this also needs a mus-any? or generator? check so that mus-type makes sense
 *          ;; is the -methods list needed?
 *          (set! (,(string->symbol (string-append "make-" sname)) args as in ws, list as in 2nd (no-method-list) case?
 *            or would a vector be better now?  (this is the equivalent of the C-side struct for each built-in gen)
 *          ;; for each field, can this be mapped instead to the new mus_any_class fields?
 *          ;;   -- no because these are also the local struct fields.
 * is this an improvement?
 */



/* ---------------- special cases ---------------- */

/* "direct" => callable via s7_call_direct, one of our local functions, so we can safely reuse its output if it's real (might be complex)
 * "indirect" => callable via s7_call_direct, but might be (+ 0.1) or equivalent, so we have to make a new real on output
 * here's an example that forces this distinction:
 *    (define (hi) (let ((o (make-oscil 440.0))) (do ((i 0 (+ i 1))) ((= i 100)) (outa i (oscil o (+ 0.1)))))) (with-sound () (hi))
 */

static s7_pointer oscil_pm_direct;
static s7_pointer g_oscil_pm_direct(s7_scheme *sc, s7_pointer args)
{
  /* (oscil g 0.0 ...), args is (g 0.0 ...) */
  mus_any *o;
  s7_pointer x;
  bool its_safe;

  its_safe = s7_in_safe_do(sc);
  GET_GENERATOR(car(args), oscil, its_safe, o);
  x = s7_call_direct(sc, caddr(args));
  return(s7_remake_real(sc, x, mus_oscil_pm(o, s7_real(x))));
}

static s7_pointer oscil_mul_c_s;
static s7_pointer g_oscil_mul_c_s(s7_scheme *sc, s7_pointer args)
{
  /* (oscil g (* c s)), args is (g (* c s)) */
  mus_any *o;
  double x;
  bool its_safe;
  s7_pointer vargs;

  its_safe = s7_in_safe_do(sc);
  GET_GENERATOR(car(args), oscil, its_safe, o);
  vargs = s7_cdadr(args);
  GET_REAL(cadr(vargs), oscil, its_safe, x);
  return(s7_make_real(sc, mus_oscil_fm(o, s7_number_to_real(car(vargs)) * x)));
}

static s7_pointer oscil_mul_s_c;
static s7_pointer g_oscil_mul_s_c(s7_scheme *sc, s7_pointer args)
{
  /* (oscil g (* s c)), args is (g (* s c)) */
  mus_any *o;
  double x;
  bool its_safe;
  s7_pointer vargs;

  its_safe = s7_in_safe_do(sc);
  GET_GENERATOR(car(args), oscil, its_safe, o);
  vargs = s7_cdadr(args);
  GET_REAL(car(vargs), oscil, its_safe, x);
  return(s7_make_real(sc, mus_oscil_fm(o, s7_number_to_real(cadr(vargs)) * x)));
}

static s7_pointer polywave_mul_c_s;
static s7_pointer g_polywave_mul_c_s(s7_scheme *sc, s7_pointer args)
{
  /* (polywave g (* c s)), args is (g (* c s)) */
  mus_any *o;
  double x;
  bool its_safe;
  s7_pointer vargs;

  its_safe = s7_in_safe_do(sc);
  GET_GENERATOR(car(args), polywave, its_safe, o);
  vargs = s7_cdadr(args);
  GET_REAL(cadr(vargs), polywave, its_safe, x);
  return(s7_make_real(sc, mus_polywave(o, s7_number_to_real(car(vargs)) * x)));
}

/* (with-sound () (fm-violin 0 .0001 440 .1)) */
static s7_pointer fm_violin_vibrato_fallback(s7_scheme *sc, s7_pointer args)
{
  mus_any *e = NULL, *t = NULL, *r = NULL;
  
  GET_GENERATOR(cadar(args), env, false, e);
  args = cdr(args);
  GET_GENERATOR(cadar(args), triangle_wave, false, t);
  args = cdr(args);
  GET_GENERATOR(cadar(args), rand_interp, false, r);
  
  return(s7_make_real(sc, mus_env(e) + mus_triangle_wave(t, 0.0) + mus_rand_interp(r, 0.0)));			
}

static s7_pointer abs_rand_interp;
static s7_pointer g_abs_rand_interp(s7_scheme *sc, s7_pointer args)
{
  mus_any *o;
  bool its_safe;
  its_safe = s7_in_safe_do(sc);
  GET_GENERATOR(cadar(args), rand_interp, its_safe, o);
  return(s7_make_real(sc, fabs(mus_rand_interp_unmodulated(o))));
}

static s7_pointer abs_oscil;
static s7_pointer g_abs_oscil(s7_scheme *sc, s7_pointer args)
{
  mus_any *o;
  bool its_safe;
  its_safe = s7_in_safe_do(sc);
  GET_GENERATOR(cadar(args), oscil, its_safe, o);
  return(s7_make_real(sc, fabs(mus_oscil_unmodulated(o))));
}

static s7_pointer abs_triangle_wave;
static s7_pointer g_abs_triangle_wave(s7_scheme *sc, s7_pointer args)
{
  mus_any *o;
  bool its_safe;
  its_safe = s7_in_safe_do(sc);
  GET_GENERATOR(cadar(args), triangle_wave, its_safe, o);
  return(s7_make_real(sc, fabs(mus_triangle_wave_unmodulated(o))));
}



static void *get_env_func(void *ptr)
{
  switch (mus_env_type((mus_any *)ptr))
    {
    case MUS_ENV_LINEAR: return((void *)mus_env_linear);
    case MUS_ENV_STEP:   return((void *)mus_env_step);
    default:             return((void *)mus_env_exponential);
    }
}

static s7_pointer fm_violin_vibrato;
static s7_pointer g_fm_violin_vibrato(s7_scheme *sc, s7_pointer args)
{
  void **syms;
  mus_float_t (*env_func)(mus_any *g);

  syms = s7_expression_data(sc, args);
  if (!syms)
    {
      if (!s7_in_safe_do(sc)) 
	return(fm_violin_vibrato_fallback(sc, args));
      syms = s7_expression_make_data(sc, args, 4); 
    }

  /* now check for start of a new note */
  if (!syms[0])
    {
      syms[0] = get_generator(sc, cadar(args));
      XEN_ASSERT_TYPE((syms[0]) && (mus_env_p((mus_any *)syms[0])), cadar(args), XEN_ONLY_ARG, "env", "env generator");
      syms[1] = get_env_func(syms[0]);

      args = cdr(args);
      syms[2] = get_generator(sc, cadar(args));
      XEN_ASSERT_TYPE((syms[2]) && (mus_triangle_wave_p((mus_any *)syms[2])), cadar(args), XEN_ARG_1, "triangle-wave", "triangle-wave generator");

      args = cdr(args);
      syms[3] = get_generator(sc, cadar(args));
      XEN_ASSERT_TYPE((syms[3]) && (mus_rand_interp_p((mus_any *)syms[3])), cadar(args), XEN_ARG_1, "rand-interp", "rand-interp generator");
    }

  env_func = (mus_float_t (*)(mus_any *))syms[1];

  return(s7_make_real(sc, env_func((mus_any *)syms[0]) + 
		          mus_triangle_wave((mus_any *)syms[2], 0.0) + 
		          mus_rand_interp_unmodulated((mus_any *)syms[3])));			
}

static s7_pointer env_polywave;
static s7_pointer g_env_polywave(s7_scheme *sc, s7_pointer args)
{
  mus_any *e = NULL, *t = NULL;
  double fm;
  bool its_safe;
  its_safe = s7_in_safe_do(sc);

  GET_GENERATOR(cadar(args), env, its_safe, e);
  args = cadr(args);
  GET_GENERATOR(cadr(args), polywave, its_safe, t);
  GET_REAL(caddr(args), polywave, its_safe, fm);

  return(s7_make_real(sc, mus_env(e) * mus_polywave(t, fm)));
}

static s7_pointer fm_violin_modulation;
static s7_pointer g_fm_violin_modulation(s7_scheme *sc, s7_pointer args)
{
  s7_pointer vargs;
  mus_any *e = NULL, *t = NULL;
  bool its_safe;
  double vibrato;
  its_safe = s7_in_safe_do(sc);

  vargs = s7_cdadr(args); /* (* ... ) */
  GET_GENERATOR(s7_cadar(vargs), env, its_safe, e);
  GET_GENERATOR(s7_cadadr(vargs), polywave, its_safe, t);
  GET_REAL(car(args), polywave, its_safe, vibrato);

  return(s7_make_real(sc, vibrato + (mus_env(e) * mus_polywave(t, vibrato))));
}

static s7_pointer fm_violin_with_modulation;
static s7_pointer g_fm_violin_with_modulation(s7_scheme *sc, s7_pointer args)
{
  s7_pointer vargs;
  mus_any *e = NULL, *t = NULL, *o = NULL;
  bool its_safe;
  double vibrato;
  its_safe = s7_in_safe_do(sc);

  GET_GENERATOR(car(args), oscil, its_safe, o);
  vargs = s7_cdaddr(cadr(args)); /* (* ... ) */
  GET_GENERATOR(s7_cadar(vargs), env, its_safe, e);
  GET_GENERATOR(s7_cadadr(vargs), polywave, its_safe, t);
  GET_REAL(s7_cadadr(args), polywave, its_safe, vibrato);

  return(s7_make_real(sc, mus_oscil_fm(o, vibrato + (mus_env(e) * mus_polywave(t, vibrato)))));
}

static s7_pointer fm_violin_1;
static s7_pointer g_fm_violin_1(s7_scheme *sc, s7_pointer args)
{
  s7_pointer vargs;
  mus_any *e = NULL, *t = NULL, *o = NULL, *a = NULL;
  bool its_safe;
  double vibrato;
  its_safe = s7_in_safe_do(sc);

  GET_GENERATOR(s7_cadar(args), env, its_safe, a);
  vargs = cadr(args);
  GET_GENERATOR(cadr(vargs), oscil, its_safe, o);
  vargs = s7_cdaddr(caddr(vargs));
  GET_GENERATOR(s7_cadar(vargs), env, its_safe, e);
  GET_GENERATOR(s7_cadadr(vargs), polywave, its_safe, t);
  GET_REAL(cadr(caddr(cadr(args))), polywave, its_safe, vibrato);

  return(s7_make_real(sc, mus_env(a) * mus_oscil_fm(o, vibrato + (mus_env(e) * mus_polywave(t, vibrato)))));
}


static s7_pointer fm_violin_2_fallback(s7_scheme *sc, s7_pointer args)
{
  s7_pointer vargs;
  double vibrato, val;
  mus_long_t pos;
  mus_xen *ms = NULL;
  mus_any *e = NULL, *t = NULL, *o = NULL, *a = NULL, *lc = NULL;

  GET_GENERATOR(car(args), locsig, false, lc);
  GET_INTEGER(cadr(args), locsig, false, pos);
  vargs = s7_cdaddr(args);
  GET_GENERATOR(s7_cadar(vargs), env, false, a);
  vargs = s7_cadr(vargs);
  GET_GENERATOR(s7_cadr(vargs), oscil, false, o);
  vargs = s7_cdaddr(s7_caddr(vargs));
  GET_GENERATOR(s7_cadar(vargs), env, false, e);
  vargs = s7_cdadr(vargs);
  GET_GENERATOR(car(vargs), polywave, false, t);
  GET_REAL(cadr(vargs), polywave, false, vibrato);
  
  val = mus_env(a) * mus_oscil_fm(o, vibrato + (mus_env(e) * mus_polywave(t, vibrato)));
  mus_locsig(lc, pos, val);
  
  /* we can't tell until run-time whether we need this -- perhaps an additional layer of optimization? 
   */
  ms = (mus_xen *)mus_locsig_closure(lc);
  if ((ms) && (ms->nvcts == 4)) /* (vct-peak (with-sound (:output (make-vct 1000 0.0)) (fm-violin 0 .01 440 .5))) */
    mus_locsig_or_move_sound_to_vct_or_sound_data(ms, lc, pos, val, true);
  
  return(args); /* just return something! */
}

static s7_pointer fm_violin_2;
static s7_pointer g_fm_violin_2(s7_scheme *sc, s7_pointer args)
{
  double vibrato, val;
  mus_long_t pos;
  mus_xen *ms = NULL;
  void **syms;
  s7_pointer sym;

  syms = s7_expression_data(sc, args);
  if (!syms)
    {
      if (!s7_in_safe_do(sc)) 
	return(fm_violin_2_fallback(sc, args));
      syms = s7_expression_make_data(sc, args, 9); 
    }

  if (!syms[0])
    {
      s7_pointer vargs;
      syms[0] = get_generator(sc, car(args));
      XEN_ASSERT_TYPE((syms[0]) && (mus_locsig_p((mus_any *)syms[0])), car(args), XEN_ARG_1, "locsig", "locsig generator");
      
      sym = cadr(args);
      if (s7_is_do_local_or_global(sc, sym))
	{
	  syms[1] = s7_slot(sc, sym);
	  XEN_ASSERT_TYPE(s7_is_integer(slot_value((s7_pointer)syms[1])), sym, XEN_ARG_2, "locsig", "an integer");
	  syms[2] = NULL;
	}
      else
	{
	  syms[1] = (void *)sym;
	  syms[2] = (void *)sym;
	}
      
      vargs = s7_cdaddr(args);
      syms[3] = get_generator(sc, s7_cadar(vargs));
      XEN_ASSERT_TYPE((syms[3]) && (mus_env_p((mus_any *)syms[3])), s7_cadar(vargs), XEN_ONLY_ARG, "env", "env generator");
      
      vargs = s7_cadr(vargs);
      syms[4] = get_generator(sc, s7_cadr(vargs));
      XEN_ASSERT_TYPE((syms[4]) && (mus_oscil_p((mus_any *)syms[4])), s7_cadr(vargs), XEN_ARG_1, "oscil", "oscil generator");
      
      vargs = s7_cdaddr(s7_caddr(vargs));
      syms[5] = get_generator(sc, s7_cadar(vargs));
      XEN_ASSERT_TYPE((syms[5]) && (mus_env_p((mus_any *)syms[5])), s7_cadar(vargs), XEN_ONLY_ARG, "env", "env generator");
      
      vargs = s7_cdadr(vargs);
      syms[6] = get_generator(sc, car(vargs));
      XEN_ASSERT_TYPE((syms[6]) && (mus_polywave_p((mus_any *)syms[6])), s7_car(vargs), XEN_ARG_1, "polywave", "polywave generator");
      
      sym = cadr(vargs);
      if (s7_is_do_local_or_global(sc, sym))
	{
	  syms[7] = s7_slot(sc, sym);
	  XEN_ASSERT_TYPE(s7_is_real(slot_value((s7_pointer)syms[7])), sym, XEN_ARG_2, "polywave", "a real");
	  syms[8] = NULL;
	}
      else
	{
	  syms[7] = (void *)sym;
	  syms[8] = (void *)sym;
	}
      
      /* choosing the env/locsig/real funcs ahead of time just breaks even??
       */
    }

  if (syms[2])
    {
      s7_pointer x;
      sym = (s7_pointer)syms[1];
      x = s7_symbol_value(sc, sym);
      XEN_ASSERT_TYPE(s7_is_integer(x), sym, XEN_ARG_2, "locsig", "an integer");
      pos = s7_integer(x);
    }
  else pos = s7_integer(slot_value((s7_pointer)syms[1]));

  if (syms[8])
    {
      s7_pointer x;
      sym = (s7_pointer)syms[7];
      x = s7_symbol_value(sc, sym);
      XEN_ASSERT_TYPE(s7_is_real(x), sym, XEN_ARG_2, "polywave", "a real");
      vibrato = s7_number_to_real(x);
    }
  else vibrato = s7_number_to_real(slot_value((s7_pointer)syms[7]));

  val = mus_env((mus_any *)syms[3]) * mus_oscil_fm((mus_any *)syms[4], 
						   vibrato + (mus_env((mus_any *)syms[5]) * mus_polywave((mus_any *)syms[6], vibrato)));
  mus_locsig((mus_any *)syms[0], pos, val);
  ms = (mus_xen *)mus_locsig_closure((mus_any *)syms[0]);
  if ((ms) && (ms->nvcts == 4)) 
    mus_locsig_or_move_sound_to_vct_or_sound_data(ms, (mus_any *)syms[0], pos, val, true);
  return(args);
}


static s7_pointer env_polywave_env_fallback(s7_scheme *sc, s7_pointer args)
{
  mus_any *e1 = NULL, *t = NULL, *e2 = NULL;
  bool its_safe;
  its_safe = s7_in_safe_do(sc);

  GET_GENERATOR(cadar(args), env, its_safe, e1);
  args = cadr(args);
  GET_GENERATOR(cadr(args), polywave, its_safe, t);
  args = caddr(args);
  GET_GENERATOR(cadr(args), env, its_safe, e2);

  return(s7_make_real(sc, mus_env(e1) * mus_polywave(t, mus_env(e2))));
}


static s7_pointer env_polywave_env;
static s7_pointer g_env_polywave_env(s7_scheme *sc, s7_pointer args)
{
  void **syms;

  syms = s7_expression_data(sc, args);
  if (!syms)
    {
      if (!s7_in_safe_do(sc)) 
	return(env_polywave_env_fallback(sc, args));
      syms = s7_expression_make_data(sc, args, 3);
    }

  if (!syms[0])
    {
      syms[0] = get_generator(sc, cadar(args));
      XEN_ASSERT_TYPE((syms[0]) && (mus_env_p((mus_any *)syms[0])), car(args), XEN_ONLY_ARG, "env", "env generator");
      args = cadr(args);
      syms[1] = get_generator(sc, cadr(args));
      args = caddr(args);
      syms[2] = get_generator(sc, cadr(args));
    }

  return(s7_make_real(sc, mus_env((mus_any *)syms[0]) * mus_polywave((mus_any *)syms[1], mus_env((mus_any *)syms[2]))));
}


/* (with-sound (:reverb jc-reverb) (outa 0 .1 *reverb*)) */
static s7_pointer jc_reverb_combs_fallback(s7_scheme *sc, s7_pointer args)
{
  s7_pointer vargs;
  mus_any *c1 = NULL, *c2, *c3, *c4;
  double fm;

  GET_GENERATOR(s7_cadar(args), comb, false, c1);
  vargs = cdr(args);
  GET_GENERATOR(s7_cadar(vargs), comb, false, c2);
  vargs = cdr(vargs);
  GET_GENERATOR(s7_cadar(vargs), comb, false, c3);
  vargs = cdr(vargs);
  GET_GENERATOR(s7_cadar(vargs), comb, false, c4);
  
  fm = s7_number_to_real(s7_symbol_value(sc, s7_caddar(args)));

  return(s7_make_real(sc, mus_comb_unmodulated_noz(c1, fm) + 
		          mus_comb_unmodulated_noz(c2, fm) + 
		          mus_comb_unmodulated_noz(c3, fm) + 
		          mus_comb_unmodulated_noz(c4, fm)));
}


static s7_pointer jc_reverb_combs;
static s7_pointer g_jc_reverb_combs(s7_scheme *sc, s7_pointer args)
{
  double fm;
  s7_pointer sym;
  void **syms;

  syms = s7_expression_data(sc, args);
  if (!syms)
    {
      if (!s7_in_safe_do(sc)) 
	return(jc_reverb_combs_fallback(sc, args));
      syms = s7_expression_make_data(sc, args, 6);
    }

  if (!syms[0])
    {
      s7_pointer vargs;
      syms[0] = get_generator(sc, s7_cadar(args));
      XEN_ASSERT_TYPE((syms[0]) && (mus_comb_p((mus_any *)syms[0])), cadar(args), XEN_ARG_1, "comb", "comb generator");

      vargs = cdr(args);
      syms[1] = get_generator(sc, s7_cadar(vargs));
      XEN_ASSERT_TYPE((syms[1]) && (mus_comb_p((mus_any *)syms[1])), cadar(args), XEN_ARG_1, "comb", "comb generator");

      vargs = cdr(vargs);
      syms[2] = get_generator(sc, s7_cadar(vargs));
      XEN_ASSERT_TYPE((syms[2]) && (mus_comb_p((mus_any *)syms[2])), cadar(args), XEN_ARG_1, "comb", "comb generator");

      vargs = cdr(vargs);
      syms[3] = get_generator(sc, s7_cadar(vargs));
      XEN_ASSERT_TYPE((syms[3]) && (mus_comb_p((mus_any *)syms[3])), cadar(args), XEN_ARG_1, "comb", "comb generator");

      sym = s7_caddar(args);
      if (s7_is_do_local_or_global(sc, sym))
	{
	  syms[4] = s7_slot(sc, sym);
	  XEN_ASSERT_TYPE(s7_is_real(slot_value((s7_pointer)syms[4])), sym, XEN_ARG_2, "comb", "a real");
	  syms[5] = NULL;
	}
      else
	{
	  syms[4] = (void *)sym;
	  syms[5] = (void *)sym;
	}
    }
  
  if (syms[5])
    {
      s7_pointer x;
      sym = (s7_pointer)syms[4];
      x = s7_symbol_value(sc, sym);
      XEN_ASSERT_TYPE(s7_is_real(x), sym, XEN_ARG_2, "comb", "a real");
      fm = s7_number_to_real(x);
    }
  else fm = s7_number_to_real(slot_value((s7_pointer)syms[4]));

  return(s7_make_real(sc, mus_comb_unmodulated_noz((mus_any *)syms[0], fm) + 
		          mus_comb_unmodulated_noz((mus_any *)syms[1], fm) + 
		          mus_comb_unmodulated_noz((mus_any *)syms[2], fm) + 
		          mus_comb_unmodulated_noz((mus_any *)syms[3], fm)));
}


static s7_pointer jc_reverb_all_passes;
static s7_pointer g_jc_reverb_all_passes(s7_scheme *sc, s7_pointer args)
{
  /* (with-sound (:reverb jc-reverb) (outa 0 .1 *reverb*)) */

  s7_pointer vargs, rev = NULL, sym;
  mus_any *a1 = NULL, *a2, *a3, *reverb;
  double x = 0.0;
  void **syms;
  s7_Int pos;

  syms = s7_expression_data(sc, args);
  if (!syms)
    {
      if (!s7_in_safe_do(sc)) 
	{
	  GET_GENERATOR(car(args), all_pass, false, a1);
	  vargs = s7_cdr(args);
	  GET_GENERATOR(cadar(vargs), all_pass, false, a2);
	  vargs = s7_cddar(vargs);
	  GET_GENERATOR(cadar(vargs), all_pass, false, a3);
	  vargs = s7_caddar(vargs);
	  GET_INTEGER(cadr(vargs), ina, false, pos);

	  goto JC_REV;
	}
	
      syms = s7_expression_make_data(sc, args, 5);
    }

  if (!(syms[0])) 
    {
      syms[0] = get_generator(sc, car(args));
      XEN_ASSERT_TYPE((syms[0]) && (mus_all_pass_p((mus_any *)syms[0])), car(args), XEN_ARG_1, "all-pass", "all-pass generator");

      vargs = s7_cdr(args);
      syms[1] = get_generator(sc, cadar(vargs));
      XEN_ASSERT_TYPE((syms[1]) && (mus_all_pass_p((mus_any *)syms[1])), cadar(args), XEN_ARG_1, "all-pass", "all-pass generator");
      
      vargs = s7_cddar(vargs);
      syms[2] = get_generator(sc, cadar(vargs));
      XEN_ASSERT_TYPE((syms[2]) && (mus_all_pass_p((mus_any *)syms[2])), cadar(args), XEN_ARG_1, "all-pass", "all-pass generator");

      vargs = s7_caddar(vargs);
      sym = cadr(vargs);
      if (s7_is_do_local_or_global(sc, sym))
	{
	  syms[3] = s7_slot(sc, sym);
	  XEN_ASSERT_TYPE(s7_is_integer(slot_value((s7_pointer)syms[3])), sym, XEN_ARG_1, "ina", "an integer");
	  syms[4] = NULL;
	}
      else
	{
	  syms[3] = (void *)sym;
	  syms[4] = (void *)sym;
	}
    }

  a1 = (mus_any *)syms[0];
  a2 = (mus_any *)syms[1];
  a3 = (mus_any *)syms[2];
  
  if (syms[4])
    {
      s7_pointer x;
      sym = (s7_pointer)syms[3];
      x = s7_symbol_value(sc, sym);
      XEN_ASSERT_TYPE(s7_is_integer(x), sym, XEN_ARG_1, "ina", "an integer");
      pos = s7_integer(x);
    }
  else pos = s7_integer(slot_value((s7_pointer)syms[3]));

 JC_REV:
  rev = slot_value(clm_reverb_slot);
  if (s7_object_type(rev) == mus_xen_tag)
    {
      mus_xen *gx;
      gx = (mus_xen *)s7_object_value(rev);
      reverb = (mus_any *)(gx->gen);
      if (mus_input_p(reverb))
	x = mus_in_any(pos, 0, reverb);
      else XEN_ASSERT_TYPE(false, rev, XEN_ARG_1, S_ina, "an input generator");
    }
  else x = s7_real(g_in_any_1(S_ina, s7_make_integer(sc, pos), 0, rev));

  return(s7_make_real(sc, mus_all_pass_unmodulated_noz(a1, mus_all_pass_unmodulated_noz(a2, mus_all_pass_unmodulated_noz(a3, x)))));
}

/* (with-sound (:reverb jc-reverb) (outa 0 .1 *reverb*)) */


/* (with-sound (:reverb nrev) (outa 0 .1 *reverb*)) */
static s7_pointer nrev_combs_fallback(s7_scheme *sc, s7_pointer args)
{
  s7_pointer vargs;
  mus_any *c1 = NULL, *c2, *c3, *c4, *c5, *c6;
  double fm;

  GET_GENERATOR(s7_cadar(args), comb, false, c1);
  vargs = cdr(args);
  GET_GENERATOR(s7_cadar(vargs), comb, false, c2);
  vargs = cdr(vargs);
  GET_GENERATOR(s7_cadar(vargs), comb, false, c3);
  vargs = cdr(vargs);
  GET_GENERATOR(s7_cadar(vargs), comb, false, c4);
  vargs = cdr(vargs);
  GET_GENERATOR(s7_cadar(vargs), comb, false, c5);
  vargs = cdr(vargs);
  GET_GENERATOR(s7_cadar(vargs), comb, false, c6);
  
  fm = s7_number_to_real(s7_symbol_value(sc, s7_caddar(args)));

  return(s7_make_real(sc, mus_comb_unmodulated_noz(c1, fm) + 
		          mus_comb_unmodulated_noz(c2, fm) + 
		          mus_comb_unmodulated_noz(c3, fm) + 
		          mus_comb_unmodulated_noz(c4, fm) +
		          mus_comb_unmodulated_noz(c5, fm) +
		          mus_comb_unmodulated_noz(c6, fm)));
}


static s7_pointer nrev_combs;
static s7_pointer g_nrev_combs(s7_scheme *sc, s7_pointer args)
{
  double fm;
  s7_pointer sym;
  void **syms;
  
  syms = s7_expression_data(sc, args);
  if (!syms)
    {
      if (!s7_in_safe_do(sc)) 
	return(nrev_combs_fallback(sc, args));
      syms = s7_expression_make_data(sc, args, 8);
    }

  if (!syms[0])
    {
      s7_pointer vargs;
      syms[0] = get_generator(sc, s7_cadar(args));
      XEN_ASSERT_TYPE((syms[0]) && (mus_comb_p((mus_any *)syms[0])), cadar(args), XEN_ARG_1, "comb", "comb generator");

      vargs = cdr(args);
      syms[1] = get_generator(sc, s7_cadar(vargs));
      XEN_ASSERT_TYPE((syms[1]) && (mus_comb_p((mus_any *)syms[1])), cadar(args), XEN_ARG_1, "comb", "comb generator");

      vargs = cdr(vargs);
      syms[2] = get_generator(sc, s7_cadar(vargs));
      XEN_ASSERT_TYPE((syms[2]) && (mus_comb_p((mus_any *)syms[2])), cadar(args), XEN_ARG_1, "comb", "comb generator");

      vargs = cdr(vargs);
      syms[3] = get_generator(sc, s7_cadar(vargs));
      XEN_ASSERT_TYPE((syms[3]) && (mus_comb_p((mus_any *)syms[3])), cadar(args), XEN_ARG_1, "comb", "comb generator");

      vargs = cdr(vargs);
      syms[6] = get_generator(sc, s7_cadar(vargs));
      XEN_ASSERT_TYPE((syms[6]) && (mus_comb_p((mus_any *)syms[6])), cadar(args), XEN_ARG_1, "comb", "comb generator");

      vargs = cdr(vargs);
      syms[7] = get_generator(sc, s7_cadar(vargs));
      XEN_ASSERT_TYPE((syms[7]) && (mus_comb_p((mus_any *)syms[7])), cadar(args), XEN_ARG_1, "comb", "comb generator");

      sym = s7_caddar(args);
      if (s7_is_do_local_or_global(sc, sym))
	{
	  syms[4] = s7_slot(sc, sym);
	  XEN_ASSERT_TYPE(s7_is_real(slot_value((s7_pointer)syms[4])), sym, XEN_ARG_2, "comb", "a real");
	  syms[5] = NULL;
	}
      else
	{
	  syms[4] = (void *)sym;
	  syms[5] = (void *)sym;
	}
    }
  
  if (syms[5])
    {
      s7_pointer x;
      sym = (s7_pointer)syms[4];
      x = s7_symbol_value(sc, sym);
      XEN_ASSERT_TYPE(s7_is_real(x), sym, XEN_ARG_2, "comb", "a real");
      fm = s7_number_to_real(x);
    }
  else fm = s7_number_to_real(slot_value((s7_pointer)syms[4]));

  return(s7_make_real(sc, mus_comb_unmodulated_noz((mus_any *)syms[0], fm) + 
		          mus_comb_unmodulated_noz((mus_any *)syms[1], fm) + 
		          mus_comb_unmodulated_noz((mus_any *)syms[2], fm) + 
		          mus_comb_unmodulated_noz((mus_any *)syms[3], fm) +
		          mus_comb_unmodulated_noz((mus_any *)syms[6], fm) +
		          mus_comb_unmodulated_noz((mus_any *)syms[7], fm)));
}



static s7_pointer ina_reverb_2;
static s7_pointer g_ina_reverb_2(s7_scheme *sc, s7_pointer args)
{
  s7_Int pos;
  s7_pointer rev;
  
  GET_INTEGER(car(args), ina, s7_in_safe_do(sc), pos);

  rev = slot_value(clm_reverb_slot);
  if (s7_object_type(rev) == mus_xen_tag)
    {
      mus_xen *gx;
      mus_any *reverb;
      gx = (mus_xen *)s7_object_value(rev);
      reverb = (mus_any *)(gx->gen);
      if (mus_input_p(reverb))
	return(s7_make_real(sc, mus_in_any(pos, 0, reverb)));
      XEN_ASSERT_TYPE(false, rev, XEN_ARG_1, S_ina, "an input generator");
    }
  return(g_in_any_1(S_ina, s7_make_integer(sc, pos), 0, rev));
}


static s7_pointer mul_s_ina_reverb_2;
static s7_pointer g_mul_s_ina_reverb_2(s7_scheme *sc, s7_pointer args)
{
  s7_Int pos;
  s7_pointer rev, scl_ptr = NULL, sym;
  double scl;
  bool its_safe;
  its_safe = s7_in_safe_do(sc);
  
  sym = car(args);
  if (its_safe)
    scl_ptr = (s7_pointer)s7_symbol_accessor_data(sym);
  if (!scl_ptr)
    {
      scl_ptr = s7_slot(sc, sym);
      if (s7_is_do_local_or_global(sc, sym))
	s7_symbol_set_accessor_data(sym, (void *)scl_ptr);
      if (!s7_is_real(slot_value(scl_ptr)))
	XEN_ASSERT_TYPE(false, scl_ptr, XEN_ARG_1, "*", "a real");
    }
  scl = s7_number_to_real(slot_value(scl_ptr));

  args = s7_cdadr(args);
  GET_INTEGER(car(args), ina, its_safe, pos);

  rev = slot_value(clm_reverb_slot);
  if (s7_object_type(rev) == mus_xen_tag)
    {
      mus_xen *gx;
      mus_any *reverb;
      gx = (mus_xen *)s7_object_value(rev);
      reverb = (mus_any *)(gx->gen);
      if (mus_input_p(reverb))
	return(s7_make_real(sc, scl * mus_in_any(pos, 0, reverb)));
      XEN_ASSERT_TYPE(false, rev, XEN_ARG_1, S_ina, "an input generator");
    }
  return(s7_make_real(sc, scl * s7_real(g_in_any_1(S_ina, s7_make_integer(sc, pos), 0, rev))));
}


static s7_pointer indirect_locsig_3;
static s7_pointer g_indirect_locsig_3(s7_scheme *sc, s7_pointer args)
{
  s7_Int pos;
  s7_pointer x;
  mus_any *locs;
  bool its_safe;
  its_safe = s7_in_safe_do(sc);

  GET_GENERATOR(car(args), locsig, its_safe, locs);
  GET_INTEGER(cadr(args), outa, its_safe, pos);
  x = s7_call_direct(sc, caddr(args));
  mus_locsig(locs, pos, s7_number_to_real(x));
  return(x);
}


static s7_pointer indirect_outa_2;
static s7_pointer g_indirect_outa_2(s7_scheme *sc, s7_pointer args)
{
  s7_Int pos;
  s7_pointer x;
  GET_INTEGER(car(args), outa, s7_in_safe_do(sc), pos);
  x = s7_call_direct(sc, cadr(args));
  return(out_any_2(slot_value(clm_output_slot), pos, s7_number_to_real(x), 0, "outa", x));
}

static s7_pointer indirect_outa_sub_2;
static s7_pointer g_indirect_outa_sub_2(s7_scheme *sc, s7_pointer args)
{
  s7_Int pos;
  double x, y;
  bool its_safe;
  its_safe = s7_in_safe_do(sc);
  
  GET_INTEGER(car(args), outa, its_safe, pos);
  GET_REAL(cadr(cadr(args)), outa, its_safe, x);
  GET_REAL(caddr(cadr(args)), outa, its_safe, y);
  return(out_any_2(slot_value(clm_output_slot), pos, x - y, 0, "outa", xen_zero));
}

static s7_pointer indirect_outa_ss;
static s7_pointer g_indirect_outa_ss(s7_scheme *sc, s7_pointer args)
{
  s7_Int pos;
  double x;
  bool its_safe;
  its_safe = s7_in_safe_do(sc);
  
  GET_INTEGER(car(args), outa, its_safe, pos);
  GET_REAL(cadr(args), outa, its_safe, x);
  return(out_any_2(slot_value(clm_output_slot), pos, x, 0, "outa", xen_zero));
}

static s7_pointer indirect_outa_add_2;
static s7_pointer g_indirect_outa_add_2(s7_scheme *sc, s7_pointer args)
{
  s7_Int pos1, pos2;
  s7_pointer x;
  bool its_safe;
  its_safe = s7_in_safe_do(sc);

  GET_INTEGER(cadr(car(args)), outa, its_safe, pos1);
  GET_INTEGER(caddr(car(args)), outa, its_safe, pos2);
  x = s7_call_direct(sc, cadr(args));
  return(out_any_2(slot_value(clm_output_slot), pos1 + pos2, s7_number_to_real(x), 0, "outa", x));
}


static s7_pointer outa_mul_s_delay;
static s7_pointer g_outa_mul_s_delay(s7_scheme *sc, s7_pointer args)
{
  /* (outa i (* scl (delay outdel1 comb-sum))) 
   */
  void **syms;
  s7_pointer sym;
  double scl, inval;
  s7_Int pos;

  syms = s7_expression_data(sc, args);
  if (!syms)
    {
      if (!s7_in_safe_do(sc)) 
	{
	  s7_Int pos;
	  double val, scl;
	  mus_any *d;
	  GET_INTEGER(car(args), outa, false, pos);
	  args = cadr(args);
	  GET_REAL(cadr(args), outa, false, scl);
	  args = caddr(args);
	  GET_GENERATOR(cadr(args), delay, false, d);
	  GET_REAL(caddr(args), delay, false, val);
	  return(out_any_2(CLM_OUTPUT, pos, scl * mus_delay_unmodulated_noz(d, val), 0, "outa", xen_zero));
	}
      syms = s7_expression_make_data(sc, args, 7);
    }

  if (!syms[0])
    {
      sym = car(args);
      if (s7_is_do_local_or_global(sc, sym))
	{
	  syms[0] = (void *)s7_slot(sc, sym);
	  XEN_ASSERT_TYPE(s7_is_integer(slot_value((s7_pointer)syms[0])), sym, XEN_ARG_1, "outa", "an integer");
	  syms[1] = NULL;
	}
      else
	{
	  syms[0] = (void *)sym;
	  syms[1] = (void *)sym;
	}

      args = cadr(args);
      sym = cadr(args);
      if (s7_is_do_local_or_global(sc, sym))
	{
	  syms[2] = (void *)s7_slot(sc, sym);
	  XEN_ASSERT_TYPE(s7_is_real(slot_value((s7_pointer)syms[2])), sym, XEN_ARG_1, "*", "a real");
	  syms[3] = NULL;
	}
      else
	{
	  syms[2] = (void *)sym;
	  syms[3] = (void *)sym;
	}

      args = caddr(args);
      syms[4] = get_generator(sc, cadr(args));
      XEN_ASSERT_TYPE((syms[4]) && (mus_delay_p((mus_any *)syms[4])), cadr(args), XEN_ARG_1, "delay", "delay generator");

      sym = caddr(args);
      if (s7_is_do_local_or_global(sc, sym))
	{
	  syms[5] = s7_slot(sc, sym);
	  XEN_ASSERT_TYPE(s7_is_real(slot_value((s7_pointer)syms[5])), sym, XEN_ARG_2, "delay", "a real");
	  syms[6] = NULL;
	}
      else
	{
	  syms[5] = (void *)sym;
	  syms[6] = (void *)sym;
	}
    }
  
  if (syms[1])
    {
      s7_pointer x;
      sym = (s7_pointer)syms[0];
      x = s7_symbol_value(sc, sym);
      XEN_ASSERT_TYPE(s7_is_integer(x), sym, XEN_ARG_1, "outa", "an integer");
      pos = s7_integer(x);
    }
  else pos = s7_integer(slot_value((s7_pointer)syms[0]));

  if (syms[3])
    {
      s7_pointer x;
      sym = (s7_pointer)syms[2];
      x = s7_symbol_value(sc, sym);
      XEN_ASSERT_TYPE(s7_is_real(x), sym, XEN_ARG_2, "*", "a real");
      scl = s7_number_to_real(x);
    }
  else scl = s7_number_to_real(slot_value((s7_pointer)syms[2]));

  if (syms[6])
    {
      s7_pointer x;
      sym = (s7_pointer)syms[5];
      x = s7_symbol_value(sc, sym);
      XEN_ASSERT_TYPE(s7_is_real(x), sym, XEN_ARG_2, "delay", "a real");
      inval = s7_number_to_real(x);
    }
  else inval = s7_number_to_real(slot_value((s7_pointer)syms[5]));

  return(out_any_2(slot_value(clm_output_slot), pos, scl * mus_delay_unmodulated_noz((mus_any *)syms[4], inval), 0, "outa", xen_zero));
}


static s7_pointer outa_env_polywave_env;
static s7_pointer g_outa_env_polywave_env(s7_scheme *sc, s7_pointer args)
{
  /* (outa i (* (env e1) (polywave p (env e2))))
   */
  void **syms;
  s7_pointer sym;
  s7_Int pos;

  syms = s7_expression_data(sc, args);
  if (!syms)
    {
      if (!s7_in_safe_do(sc)) 
	{
	  s7_Int pos;
	  mus_any *e1, *e2, *t;
	  GET_INTEGER(car(args), outa, false, pos);
	  args = cdr(cadr(args));
	  GET_GENERATOR(cadar(args), env, false, e1);
	  args = cadr(args);
	  GET_GENERATOR(cadr(args), polywave, false, t);
	  args = caddr(args);
	  GET_GENERATOR(cadr(args), env, false, e2);
	  return(out_any_2(CLM_OUTPUT, pos, mus_env(e1) * mus_polywave(t, mus_env(e2)), 0, "outa", xen_zero));
	}

      syms = s7_expression_make_data(sc, args, 5);
    }

  if (!syms[0])
    {
      sym = car(args);
      if (s7_is_do_local_or_global(sc, sym))
	{
	  syms[0] = (void *)s7_slot(sc, sym);
	  XEN_ASSERT_TYPE(s7_is_integer(slot_value((s7_pointer)syms[0])), sym, XEN_ARG_1, "outa", "an integer");
	  syms[1] = NULL;
	}
      else
	{
	  syms[0] = (void *)sym;
	  syms[1] = (void *)sym;
	}

      args = cdr(cadr(args));
      sym = cadar(args);
      syms[2] = get_generator(sc, sym);
      XEN_ASSERT_TYPE((syms[2]) && (mus_env_p((mus_any *)syms[2])), sym, XEN_ARG_1, "env", "env generator");

      args = cadr(args);
      sym = cadr(args);
      syms[3] = get_generator(sc, sym);
      XEN_ASSERT_TYPE((syms[3]) && (mus_polywave_p((mus_any *)syms[3])), sym, XEN_ARG_1, "polywave", "polywave generator");

      args = caddr(args);
      sym = cadr(args);
      syms[4] = get_generator(sc, sym);
      XEN_ASSERT_TYPE((syms[4]) && (mus_env_p((mus_any *)syms[4])), sym, XEN_ARG_1, "env", "env generator");
    }
  
  if (syms[1])
    {
      s7_pointer x;
      sym = (s7_pointer)syms[0];
      x = s7_symbol_value(sc, sym);
      XEN_ASSERT_TYPE(s7_is_integer(x), sym, XEN_ARG_1, "outa", "an integer");
      pos = s7_integer(x);
    }
  else pos = s7_integer(slot_value((s7_pointer)syms[0]));

  return(out_any_2(slot_value(clm_output_slot), pos, mus_env((mus_any *)syms[2]) * mus_polywave((mus_any *)syms[3], mus_env((mus_any *)syms[4])), 0, "outa", xen_zero));
}

static s7_pointer frame_to_frame_sss;
static s7_pointer g_frame_to_frame_sss(s7_scheme *sc, s7_pointer args)
{
  /* can't use syms here as we'd like because there might be many of these things in an instrument
   */
  s7_pointer sym, result = NULL;
  mus_any *mx = NULL, *inf = NULL, *outf = NULL;

  if (s7_in_safe_do(sc))
    {
      sym = car(args);
      mx = (mus_any *)s7_symbol_accessor_data(sym);
      if (!mx)
	{
	  mx = get_generator(sc, sym);
	  if (s7_is_do_global(sc, sym))
	    s7_symbol_set_accessor_data(sym, (void *)mx);
	}

      sym = cadr(args);
      inf = (mus_any *)s7_symbol_accessor_data(sym);
      if (!inf)
	{
	  inf = get_generator(sc, sym);
	  if (s7_is_do_global(sc, sym))
	    s7_symbol_set_accessor_data(sym, (void *)inf);
	}

      sym = caddr(args);
      result = (s7_pointer)s7_symbol_accessor_data(sym);
      if (!result)
	{
	  result = s7_symbol_value(sc, sym);
	  if (s7_object_type(result) != mus_xen_tag)
	    result = NULL;
	  else
	    {
	      if (s7_is_do_global(sc, sym))
		s7_symbol_set_accessor_data(sym, (void *)result);
	    }
	}
    }
  else
    {
      mx = get_generator(sc, car(args));
      inf = get_generator(sc, cadr(args));
      sym = caddr(args);
      result = s7_symbol_value(sc, sym);
      if (s7_object_type(result) != mus_xen_tag)
	result = NULL;
    }

  if (result)
    outf = (((mus_xen *)s7_object_value(result))->gen);

  XEN_ASSERT_TYPE((mx) && (inf) && 
		  ((mus_frame_p(mx) && mus_mixer_p(inf)) || 
		   (mus_mixer_p(mx) && mus_frame_p(inf))),
		  car(args), XEN_ARG_1, S_frame_to_frame, "first two args should be mixer and frame");
  XEN_ASSERT_TYPE((outf) && (mus_frame_p(outf)), caddr(args), XEN_ARG_3, S_frame_to_frame, "a frame");

  mus_frame_to_frame(mx, inf, outf);
  return(result);
}


static s7_pointer indirect_frame_to_file_3;
static s7_pointer g_indirect_frame_to_file_3(s7_scheme *sc, s7_pointer args)
{
  /* no need for the direct case -- this does not return anything new */
  s7_Int pos;
  s7_pointer data;

  GET_INTEGER(cadr(args), frame_to_file, s7_in_safe_do(sc), pos);
  data = s7_call_direct(sc, caddr(args));
  mus_frame_to_file(XEN_TO_MUS_ANY(slot_value(clm_output_slot)), pos, (mus_any *)(((mus_xen *)s7_object_value(data))->gen));
  return(data);
}




/* s7: 495, run: 479, C: 370
 * the sin numbers callgrind reports are completely bogus, but they're the same here,
 *    the real factor is 1.2 (576 to 479 in callgrind numbers)
 */



/* ---------------- */

static s7_pointer mul_direct_2;
static s7_pointer g_mul_direct_2(s7_scheme *sc, s7_pointer args)
{
  s7_pointer y;
  double x;
  x = s7_real(s7_call_direct(sc, car(args)));
  y = s7_call_direct(sc, cadr(args));
  return(s7_remake_real(sc, y, x * s7_real(y)));
}

static s7_pointer add_direct_2;
static s7_pointer g_add_direct_2(s7_scheme *sc, s7_pointer args)
{
  s7_pointer y;
  double x;
  x = s7_real(s7_call_direct(sc, car(args)));
  y = s7_call_direct(sc, cadr(args));
  return(s7_remake_real(sc, y, x + s7_real(y)));
}

static s7_pointer mul_direct_3;
static s7_pointer g_mul_direct_3(s7_scheme *sc, s7_pointer args)
{
  s7_pointer z;
  double x, y;
  x = s7_real(s7_call_direct(sc, car(args)));
  y = s7_real(s7_call_direct(sc, cadr(args)));
  z = s7_call_direct(sc, caddr(args));
  return(s7_remake_real(sc, z, x * y * s7_real(z)));
}

static s7_pointer add_direct_3;
static s7_pointer g_add_direct_3(s7_scheme *sc, s7_pointer args)
{
  s7_pointer z;
  double x, y;
  x = s7_real(s7_call_direct(sc, car(args)));
  y = s7_real(s7_call_direct(sc, cadr(args)));
  z = s7_call_direct(sc, caddr(args));
  return(s7_remake_real(sc, z, x + y + s7_real(z)));
}

static s7_pointer mul_env_direct;
static s7_pointer g_mul_env_direct(s7_scheme *sc, s7_pointer args)
{
  /* (* (env e) ...) */
  s7_pointer x;
  mus_any *e = NULL;
  bool its_safe;
  its_safe = s7_in_safe_do(sc);

  GET_GENERATOR(s7_cadar(args), env, its_safe, e);
  x = s7_call_direct(sc, cadr(args));
  return(s7_remake_real(sc, x, mus_env(e) * s7_real(x)));
}

static s7_pointer mul_c_direct;
static s7_pointer g_mul_c_direct(s7_scheme *sc, s7_pointer args)
{
  /* (* c ...) */
  s7_pointer x;
  x = s7_call_direct(sc, cadr(args));
  return(s7_remake_real(sc, x, s7_number_to_real(car(args)) * s7_real(x)));
}

static s7_pointer add_c_direct;
static s7_pointer g_add_c_direct(s7_scheme *sc, s7_pointer args)
{
  /* (+ c ...) */
  s7_pointer x;
  x = s7_call_direct(sc, cadr(args));
  return(s7_remake_real(sc, x, s7_number_to_real(car(args)) + s7_real(x)));
}

static s7_pointer mul_s_direct;
static s7_pointer g_mul_s_direct(s7_scheme *sc, s7_pointer args)
{
  /* (* s ...) */
  s7_pointer x, mul;
  double xval;
  bool its_safe;

  its_safe = s7_in_safe_do(sc);
  GET_NUMBER(car(args), "*", its_safe, mul);

  x = s7_call_direct(sc, cadr(args));
  xval = s7_real(x);

  if (s7_is_real(mul))
    return(s7_remake_real(sc, x, s7_number_to_real(mul) * xval));
  return(s7_make_complex(sc, s7_real_part(mul) * xval, s7_imag_part(mul) * xval));
}

static s7_pointer mul_1s_direct;
static s7_pointer g_mul_1s_direct(s7_scheme *sc, s7_pointer args)
{
  /* (* (- 1.0 s) ...) */
  s7_pointer x, mul;
  double xval;
  bool its_safe;

  its_safe = s7_in_safe_do(sc);
  GET_NUMBER(caddr(car(args)), "*", its_safe, mul);

  x = s7_call_direct(sc, cadr(args));
  xval = s7_real(x);

  if (s7_is_real(mul))
    return(s7_remake_real(sc, x, (1.0 - s7_number_to_real(mul)) * xval));
  return(s7_make_complex(sc, (1.0 - s7_real_part(mul)) * xval, -s7_imag_part(mul) * xval));
}

static s7_pointer add_s_direct;
static s7_pointer g_add_s_direct(s7_scheme *sc, s7_pointer args)
{
  /* (+ s ...) */
  s7_pointer x, mul;
  double xval;
  bool its_safe;

  its_safe = s7_in_safe_do(sc);
  GET_NUMBER(car(args), "*", its_safe, mul);

  x = s7_call_direct(sc, cadr(args));
  xval = s7_real(x);

  if (s7_is_real(mul))
    return(s7_remake_real(sc, x, s7_number_to_real(mul) + xval));
  return(s7_make_complex(sc, s7_real_part(mul) + xval, s7_imag_part(mul)));
}

static s7_pointer add_1s_direct;
static s7_pointer g_add_1s_direct(s7_scheme *sc, s7_pointer args)
{
  /* (+ (- 1.0 s) ...) */
  s7_pointer x, mul;
  double xval;
  bool its_safe;

  its_safe = s7_in_safe_do(sc);
  GET_NUMBER(caddr(car(args)), "+", its_safe, mul);

  x = s7_call_direct(sc, cadr(args));
  xval = s7_real(x);

  if (s7_is_real(mul))
    return(s7_remake_real(sc, x, (1.0 - s7_number_to_real(mul)) + xval));
  return(s7_make_complex(sc, (1.0 - s7_real_part(mul)) + xval, -s7_imag_part(mul)));
}

static s7_pointer add_cs_direct;
static s7_pointer g_add_cs_direct(s7_scheme *sc, s7_pointer args)
{
  /* (+ (* c s) ...) */
  s7_pointer x, mul;
  double xval, cval;
  bool its_safe;

  its_safe = s7_in_safe_do(sc);
  GET_NUMBER(caddr(car(args)), "+", its_safe, mul);

  x = s7_call_direct(sc, cadr(args));
  xval = s7_real(x);
  cval = s7_number_to_real(cadr(car(args)));

  if (s7_is_real(mul))
    return(s7_remake_real(sc, x, (cval * s7_number_to_real(mul)) + xval));
  return(s7_make_complex(sc, (cval * s7_real_part(mul)) + xval, -s7_imag_part(mul) * cval));
}

/* ---------------- */


#define CLM_GEN 0
#define CLM_GENERATOR xen_zero

#define MUL_C_GEN 1
#define MUL_S_GEN 2
#define ENV_GEN 3

#define MUL_C_GEN_1 4
#define MUL_S_GEN_1 5
#define ENV_GEN_1 6

#define NUM_CHOICES 7

static s7_pointer *make_choices(s7_pointer mul_c, s7_pointer mul_s, s7_pointer e, s7_pointer mul_c1, s7_pointer mul_s1, s7_pointer e1)
{
  s7_pointer *choices;
  choices = (s7_pointer *)calloc(NUM_CHOICES, sizeof(s7_pointer));
  choices[CLM_GEN] = CLM_GENERATOR; /* just something that won't be confused */
  choices[MUL_C_GEN] = mul_c;
  choices[MUL_S_GEN] = mul_s;
  choices[ENV_GEN] = e;
  choices[MUL_C_GEN_1] = mul_c1;
  choices[MUL_S_GEN_1] = mul_s1;
  choices[ENV_GEN_1] = e1;
  return(choices);
}


static bool expr_is_clm_gen(s7_scheme *sc, s7_pointer expr)
{
  s7_pointer *choices;
  choices = (s7_pointer *)s7_function_chooser_data(sc, expr);
  return((choices) &&
	 (choices[CLM_GEN] == CLM_GENERATOR));
}


static s7_pointer env_symbol, all_pass_symbol, ina_symbol, comb_symbol, polywave_symbol, triangle_wave_symbol;
static s7_pointer rand_interp_symbol, oscil_symbol, add_symbol, subtract_symbol, reverb_symbol, output_symbol;
static s7_pointer multiply_symbol, vector_ref_symbol;

static s7_pointer (*initial_add_chooser)(s7_scheme *sc, s7_pointer f, int args, s7_pointer expr);

static s7_pointer clm_add_chooser(s7_scheme *sc, s7_pointer f, int args, s7_pointer expr)
{
  if ((args == 3) &&
      (s7_is_pair(cadr(expr))) &&
      (car(cadr(expr)) == env_symbol) &&
      (s7_is_symbol(cadr(cadr(expr)))) &&
      (s7_is_pair(caddr(expr))) &&
      (cddr(caddr(expr)) == s7_nil(sc)) &&
      (car(caddr(expr)) == triangle_wave_symbol) &&
      (s7_is_symbol(cadr(caddr(expr)))) &&
      (s7_is_pair(cadddr(expr))) &&
      (cddr(cadddr(expr)) == s7_nil(sc)) &&
      (car(cadddr(expr)) == rand_interp_symbol) &&
      (s7_is_symbol(cadr(cadddr(expr)))))
    {
      s7_function_choice_set_direct(sc, expr);
      return(fm_violin_vibrato);
    }

  if ((args == 2) &&
      (s7_is_symbol(cadr(expr))) &&
      (s7_is_pair(caddr(expr))))
    {
      if (s7_function_choice(sc, caddr(expr)) == g_env_polywave)
	{
	  s7_function_choice_set_direct(sc, expr);
	  return(fm_violin_modulation);
	}
    }

  if (args == 4)
    {
      s7_pointer p, fm = NULL;
      for (p = cdr(expr); s7_is_pair(p); p = cdr(p))
	if ((!s7_is_pair(car(p))) ||
	    (!s7_is_symbol(cadar(p))) ||
	    (!s7_is_symbol(s7_caddar(p))) ||
	    (car(car(p)) != comb_symbol))
	  return(f);
	else
	  {
	    if (!fm) 
	      fm = s7_caddar(p);
	    else
	      {
		if (fm != s7_caddar(p))
		  return(f);
	      }
	  }
      s7_function_choice_set_direct(sc, expr);
      return(jc_reverb_combs);
    }

  if (args == 6)
    {
      s7_pointer p, fm = NULL;
      for (p = cdr(expr); s7_is_pair(p); p = cdr(p))
	if ((!s7_is_pair(car(p))) ||
	    (!s7_is_symbol(cadar(p))) ||
	    (!s7_is_symbol(s7_caddar(p))) ||
	    (car(car(p)) != comb_symbol))
	  return(f);
	else
	  {
	    if (!fm) 
	      fm = s7_caddar(p);
	    else
	      {
		if (fm != s7_caddar(p))
		  return(f);
	      }
	  }
      s7_function_choice_set_direct(sc, expr);
      return(nrev_combs);
    }

  if ((args == 2) &&
      (s7_is_pair(caddr(expr))) &&
      (s7_function_choice_is_direct(sc, caddr(expr))) &&
      (expr_is_clm_gen(sc, caddr(expr))))
    {
      if (s7_is_real(cadr(expr)))
	{
	  /* fprintf(stderr, "\nadd c direct: %s\n\n", DISPLAY_80(expr)); */
	  s7_function_choice_set_direct(sc, expr);
	  return(add_c_direct);
	}

      if (s7_is_symbol(cadr(expr)))
	{
	  /* fprintf(stderr, "\nadd s direct: %s\n\n", DISPLAY_80(expr)); */
	  s7_function_choice_set_direct(sc, expr);
	  return(add_s_direct);
	}

      if ((s7_is_pair(cadr(expr))) &&
	  (s7_function_choice_is_direct(sc, cadr(expr))) &&
	  (expr_is_clm_gen(sc, cadr(expr))))
	{
	  /* fprintf(stderr, "add 2 direct\n"); */
	  s7_function_choice_set_direct(sc, expr);
	  return(add_direct_2);
	}
      
      if ((s7_is_pair(cadr(expr))) &&
	  (s7_list_length(sc, cadr(expr)) == 3) &&
	  (s7_is_real(cadr(cadr(expr)))) &&
	  (s7_is_symbol(caddr(cadr(expr)))))
	{
	  if (car(cadr(expr)) == multiply_symbol)
	    {
	      s7_function_choice_set_direct(sc, expr);
	      return(add_cs_direct);
	    }
	  if ((car(cadr(expr)) == subtract_symbol) &&
	      (s7_number_to_real(cadr(cadr(expr))) == 1.0))
	    {
	      s7_function_choice_set_direct(sc, expr);
	      return(add_1s_direct);
	    }
	}
    }

  if ((args == 3) &&
      (s7_is_pair(cadr(expr))) &&
      (s7_is_pair(caddr(expr))) &&
      (s7_is_pair(cadddr(expr))) &&
      (s7_function_choice_is_direct(sc, cadr(expr))) &&
      (expr_is_clm_gen(sc, cadr(expr))) &&
      (s7_function_choice_is_direct(sc, caddr(expr))) &&
      (expr_is_clm_gen(sc, caddr(expr))) &&
      (s7_function_choice_is_direct(sc, cadddr(expr))) &&
      (expr_is_clm_gen(sc, cadddr(expr))))
    {
      /* fprintf(stderr, "add 3 direct\n"); */
      s7_function_choice_set_direct(sc, expr);
      return(add_direct_3);
    }

  return((*initial_add_chooser)(sc, f, args, expr));
}


static s7_pointer (*initial_multiply_chooser)(s7_scheme *sc, s7_pointer f, int args, s7_pointer expr);

static s7_pointer clm_multiply_chooser(s7_scheme *sc, s7_pointer f, int args, s7_pointer expr)
{
  /* fprintf(stderr, "* expr: %s\n", DISPLAY(expr)); */

  if ((args == 2) &&
      (s7_is_pair(cadr(expr))) &&
      (car(cadr(expr)) == env_symbol) &&
      (s7_is_symbol(cadr(cadr(expr)))) &&
      (s7_is_pair(caddr(expr))))
    {
      if (s7_function_choice(sc, caddr(expr)) == g_fm_violin_with_modulation)
	{
	  /* fprintf(stderr, "fm-violin-1\n"); */
	  s7_function_choice_set_direct(sc, expr);
	  return(fm_violin_1);
	}

      /* fprintf(stderr, "env * %s %p %p\n", DISPLAY_80(caddr(expr)), g_direct_polywave_2, s7_function_choice(sc, caddr(expr))); */

      if ((car(caddr(expr)) == polywave_symbol) &&
	  (s7_is_pair(cdr(caddr(expr)))) &&
	  (s7_is_symbol(cadr(caddr(expr)))) &&
	  (s7_is_pair(cddr(caddr(expr)))))
	{
	  if (s7_is_symbol(caddr(caddr(expr))))
	    {
	      /* fprintf(stderr, "env_polywave\n"); */
	      s7_function_choice_set_direct(sc, expr);
	      return(env_polywave);
	    }
	  if ((s7_is_pair(caddr(caddr(expr)))) &&
	      (car(caddr(caddr(expr))) == env_symbol) &&
	      (s7_is_symbol(cadr(caddr(caddr(expr))))))
	    {
	      /* fprintf(stderr, "env-polywave-env\n"); */
	      s7_function_choice_set_direct(sc, expr);
	      return(env_polywave_env);
	    }
	}
    }

  if ((args == 2) &&
      (s7_is_pair(caddr(expr))))
    {
      if (s7_is_real(cadr(expr)))
	{
	  /* (* num (gen...))
	   */
	  s7_pointer *choices;
	  choices = (s7_pointer *)s7_function_chooser_data(sc, caddr(expr));
	  if (choices)
	    {
	      if (choices[MUL_C_GEN])
		{
		  /* fprintf(stderr, "mul c gen2\n"); */
		  s7_function_choice_set_direct(sc, expr);
		  return(choices[MUL_C_GEN]);
		}

	      if (choices[MUL_C_GEN_1])
		{
		  /* fprintf(stderr, "mul c gen1\n"); */
		  s7_function_choice_set_direct(sc, expr);
		  return(choices[MUL_C_GEN_1]);
		}

	      if ((s7_function_choice_is_direct(sc, caddr(expr))) &&
		  (choices[CLM_GEN] == CLM_GENERATOR))
		{
		  s7_function_choice_set_direct(sc, expr);
		  return(mul_c_direct);
		}
	    }
	}
      if ((s7_is_symbol(cadr(expr))) &&
	  (s7_is_pair(cddr(expr))) &&          /* (* + '(vector?)) -- this is the optimizer's fault */
	  (s7_is_pair(caddr(expr))))
	{
	  s7_pointer *choices;
	  choices = (s7_pointer *)s7_function_chooser_data(sc, caddr(expr));
	  if (choices)
	    {
	      if (choices[MUL_S_GEN])
		{
		  /* fprintf(stderr, "mul s gen2\n"); */
		  s7_function_choice_set_direct(sc, expr);
		  return(choices[MUL_S_GEN]);
		}
	      if (choices[MUL_S_GEN_1])
		{
		  /* fprintf(stderr, "mul s gen1\n"); */
		  s7_function_choice_set_direct(sc, expr);
		  return(choices[MUL_S_GEN_1]);
		}

	      if ((s7_function_choice_is_direct(sc, caddr(expr))) &&
		  (choices[CLM_GEN] == CLM_GENERATOR))
		{
		  /* fprintf(stderr, "\nmul s direct: %s\n\n", DISPLAY_80(expr)); */
		  s7_function_choice_set_direct(sc, expr);
		  return(mul_s_direct);
		}
	    }
	}
      
      if ((s7_is_pair(cadr(expr))) &&
	  (car(cadr(expr)) == env_symbol))
	{
	  s7_pointer *choices;
	  choices = (s7_pointer *)s7_function_chooser_data(sc, caddr(expr));
	  /* fprintf(stderr, "mul direct: %s: %p (%d)\n", DISPLAY(expr), choices, s7_function_choice_is_direct(sc, caddr(expr))); */
	  if (choices)
	    {
	      if (choices[ENV_GEN])
		{
		  /* fprintf(stderr, "env2\n"); */
		  s7_function_choice_set_direct(sc, expr);
		  return(choices[ENV_GEN]);
		}
	      if (choices[ENV_GEN_1])
		{
		  /* fprintf(stderr, "env1\n"); */
		  s7_function_choice_set_direct(sc, expr);
		  return(choices[ENV_GEN_1]);
		}
	    }
	  if ((s7_function_choice_is_direct(sc, caddr(expr))) &&
	      (expr_is_clm_gen(sc, caddr(expr))))
	    {
	      /* fprintf(stderr, "\nmul env direct: %s\n\n", DISPLAY_80(expr)); */
	      s7_function_choice_set_direct(sc, expr);
	      return(mul_env_direct);
	    }
	}

      if ((s7_is_pair(cadr(expr))) &&
	  (car(cadr(expr)) == subtract_symbol) &&
	  (s7_list_length(sc, cadr(expr)) == 3) &&
	  (s7_is_real(cadr(cadr(expr)))) &&
	  (s7_is_symbol(caddr(cadr(expr)))) &&
	  (s7_number_to_real(cadr(cadr(expr))) == 1.0) &&
	  (s7_function_choice_is_direct(sc, caddr(expr))) &&
	  (expr_is_clm_gen(sc, caddr(expr))))
	{
	  /* fprintf(stderr, "\nmul (- 1 s) direct: %s\n\n", DISPLAY_80(expr)); */
	  s7_function_choice_set_direct(sc, expr);
	  return(mul_1s_direct);
	}
    }

  if ((args == 2) &&
      (s7_is_pair(cadr(expr))) &&
      (s7_is_pair(caddr(expr))) &&
      (s7_function_choice_is_direct(sc, cadr(expr))) &&
      (expr_is_clm_gen(sc, cadr(expr))) &&
      (s7_function_choice_is_direct(sc, caddr(expr))) &&
      (expr_is_clm_gen(sc, caddr(expr))))
    {
      s7_function_choice_set_direct(sc, expr);
      /* fprintf(stderr, "mul 2 direct\n"); */
      return(mul_direct_2);
    }

  if ((args == 3) &&
      (s7_is_pair(cadr(expr))) &&
      (s7_is_pair(caddr(expr))) &&
      (s7_is_pair(cadddr(expr))) &&
      (s7_function_choice_is_direct(sc, cadr(expr))) &&
      (expr_is_clm_gen(sc, cadr(expr))) &&
      (s7_function_choice_is_direct(sc, caddr(expr))) &&
      (expr_is_clm_gen(sc, caddr(expr))) &&
      (s7_function_choice_is_direct(sc, cadddr(expr))) &&
      (expr_is_clm_gen(sc, cadddr(expr))))
    {
      s7_function_choice_set_direct(sc, expr);
      /* fprintf(stderr, "mul 3 direct: %s\n", DISPLAY(expr)); */
      return(mul_direct_3);
    }

  return((*initial_multiply_chooser)(sc, f, args, expr));
}

/* (define (hi) (let ((fm .1) (o (make-oscil 440)) (e (make-env (list 0 0 1 1) :duration .01))) (do ((i 0 (+ i 1))) ((= i 100)) (* (env e) (oscil o fm)))))
 */


static s7_pointer (*initial_abs_chooser)(s7_scheme *sc, s7_pointer f, int args, s7_pointer expr);

static s7_pointer clm_abs_chooser(s7_scheme *sc, s7_pointer f, int args, s7_pointer expr)
{
  if ((args == 1) &&
      (s7_is_pair(cadr(expr))) &&
      (s7_list_length(sc, cadr(expr)) == 2) &&
      (s7_is_symbol(s7_cadadr(expr))))
    {
      if (s7_caadr(expr) == rand_interp_symbol)
	{
	  s7_function_choice_set_direct(sc, expr);
	  return(abs_rand_interp);
	}
      if (s7_caadr(expr) == oscil_symbol)
	{
	  s7_function_choice_set_direct(sc, expr);
	  return(abs_oscil);
	}
      if (s7_caadr(expr) == triangle_wave_symbol)
	{
	  /* fprintf(stderr, "abs triangle-wave: %s\n", DISPLAY(expr)); */
	  s7_function_choice_set_direct(sc, expr);
	  return(abs_triangle_wave);
	}
    }
      
  return((*initial_abs_chooser)(sc, f, args, expr));
}

static s7_pointer oscil_chooser(s7_scheme *sc, s7_pointer f, int args, s7_pointer expr)
{
  /* fprintf(stderr, "oscil: %s\n", DISPLAY(expr)); */

  if ((args == 1) &&
      (s7_is_symbol(cadr(expr))))
    {
      s7_function_choice_set_direct(sc, expr);
      return(oscil_1);
    }

  if ((args == 2) &&
      (s7_is_symbol(cadr(expr))))
    {
      if (s7_is_symbol(caddr(expr)))
	{
	  /* fprintf(stderr, "oscil_2\n"); */
	  s7_function_choice_set_direct(sc, expr);
	  return(oscil_2);
	}
      if (s7_is_pair(caddr(expr)))
	{
	  if (s7_function_choice(sc, caddr(expr)) == g_fm_violin_modulation)
	    {
	      s7_function_choice_set_direct(sc, expr);
	      return(fm_violin_with_modulation);
	    }

	  if ((s7_list_length(sc, caddr(expr)) == 3) &&
	      (s7_caaddr(expr) == s7_make_symbol(sc, "*")) &&
	      (s7_is_real(cadr(caddr(expr)))) &&
	      (s7_is_symbol(caddr(caddr(expr)))))
	    {
	      s7_function_choice_set_direct(sc, expr);
	      return(oscil_mul_c_s);
	    }

	  if ((s7_list_length(sc, caddr(expr)) == 3) &&
	      (s7_caaddr(expr) == s7_make_symbol(sc, "*")) &&
	      (s7_is_real(caddr(caddr(expr)))) &&
	      (s7_is_symbol(cadr(caddr(expr)))))
	    {
	      s7_function_choice_set_direct(sc, expr);
	      return(oscil_mul_s_c);
	    }

	  if (s7_function_choice_is_direct(sc, caddr(expr)))
	    {
	      s7_function_choice_set_direct(sc, expr);
	      if (expr_is_clm_gen(sc, caddr(expr))) return(direct_oscil_2);
	      return(indirect_oscil_2);
	    }
	}
    }
  if ((args == 3) &&
      (s7_is_real(caddr(expr))) &&
      (s7_number_to_real(caddr(expr)) == 0.0) &&
      (s7_is_pair(cadddr(expr))) &&
      (s7_function_choice_is_direct(sc, cadddr(expr))) &&
      (expr_is_clm_gen(sc, cadddr(expr))))
    {
      s7_function_choice_set_direct(sc, expr);
      return(oscil_pm_direct);
    }

  return(f);
}

static s7_pointer polywave_chooser(s7_scheme *sc, s7_pointer f, int args, s7_pointer expr)
{
  if ((args == 1) &&
      (s7_is_symbol(cadr(expr))))
    {
      s7_function_choice_set_direct(sc, expr);
      return(polywave_1);
    }

  if ((args == 2) &&
      (s7_is_symbol(cadr(expr))))
    {
      if (s7_is_symbol(caddr(expr)))
	{
	  s7_function_choice_set_direct(sc, expr);
	  return(polywave_2);
	}

      if (s7_is_pair(caddr(expr)))
	{
	  if ((s7_list_length(sc, caddr(expr)) == 3) &&
	      (s7_caaddr(expr) == s7_make_symbol(sc, "*")) &&
	      (s7_is_real(cadr(caddr(expr)))) &&
	      (s7_is_symbol(caddr(caddr(expr)))))
	    {
	      s7_function_choice_set_direct(sc, expr);
	      return(polywave_mul_c_s);
	    }
	      
	  if (s7_function_choice_is_direct(sc, caddr(expr)))
	    {
	      s7_function_choice_set_direct(sc, expr);
	      if (expr_is_clm_gen(sc, caddr(expr))) return(direct_polywave_2);
	      return(indirect_polywave_2);
	    }
	}
    }
  return(f);
}

static s7_pointer table_lookup_chooser(s7_scheme *sc, s7_pointer f, int args, s7_pointer expr)
{
  if ((args == 1) &&
      (s7_is_symbol(cadr(expr))))
    {
      s7_function_choice_set_direct(sc, expr);
      return(table_lookup_1);
    }

  if ((args == 2) &&
      (s7_is_symbol(cadr(expr))))
    {
      if (s7_is_symbol(caddr(expr)))
	{
	  s7_function_choice_set_direct(sc, expr);
	  return(table_lookup_2);
	}
      if (s7_is_pair(caddr(expr)))
	{
	  if (s7_function_choice_is_direct(sc, caddr(expr)))
	    {
	      s7_function_choice_set_direct(sc, expr);
	      if (expr_is_clm_gen(sc, caddr(expr))) return(direct_table_lookup_2);
	      return(indirect_table_lookup_2);
	    }
	}
    }
  return(f);
}

static s7_pointer wave_train_chooser(s7_scheme *sc, s7_pointer f, int args, s7_pointer expr)
{
  if ((args == 1) &&
      (s7_is_symbol(cadr(expr))))
    {
      s7_function_choice_set_direct(sc, expr);
      return(wave_train_1);
    }

  if ((args == 2) &&
      (s7_is_symbol(cadr(expr))))
    {
      if (s7_is_symbol(caddr(expr)))
	{
	  s7_function_choice_set_direct(sc, expr);
	  return(wave_train_2);
	}
      if (s7_is_pair(caddr(expr)))
	{
	  if (s7_function_choice_is_direct(sc, caddr(expr)))
	    {
	      s7_function_choice_set_direct(sc, expr);
	      if (expr_is_clm_gen(sc, caddr(expr))) return(direct_wave_train_2);
	      return(indirect_wave_train_2);
	    }
	}
    }
  return(f);
}

static s7_pointer nsin_chooser(s7_scheme *sc, s7_pointer f, int args, s7_pointer expr)
{
  if ((args == 1) &&
      (s7_is_symbol(cadr(expr))))
    {
      s7_function_choice_set_direct(sc, expr);
      return(nsin_1);
    }
  if ((args == 2) &&
      (s7_is_symbol(cadr(expr))) &&
      (s7_is_symbol(caddr(expr))))
    {
      s7_function_choice_set_direct(sc, expr);
      return(nsin_2);
    }

  if ((args == 2) &&
      (s7_is_symbol(cadr(expr))) &&
      (s7_is_pair(caddr(expr))) &&
      (s7_function_choice_is_direct(sc, caddr(expr))))
    {
      s7_function_choice_set_direct(sc, expr);
      if (expr_is_clm_gen(sc, caddr(expr))) return(direct_nsin_2);
      return(indirect_nsin_2);
    }
  return(f);
}

static s7_pointer ncos_chooser(s7_scheme *sc, s7_pointer f, int args, s7_pointer expr)
{
  if ((args == 1) &&
      (s7_is_symbol(cadr(expr))))
    {
      s7_function_choice_set_direct(sc, expr);
      return(ncos_1);
    }
  if ((args == 2) &&
      (s7_is_symbol(cadr(expr))) &&
      (s7_is_symbol(caddr(expr))))
    {
      s7_function_choice_set_direct(sc, expr);
      return(ncos_2);
    }
  if ((args == 2) &&
      (s7_is_symbol(cadr(expr))) &&
      (s7_is_pair(caddr(expr))) &&
      (s7_function_choice_is_direct(sc, caddr(expr))))
    {
      s7_function_choice_set_direct(sc, expr);
      if (expr_is_clm_gen(sc, caddr(expr))) return(direct_ncos_2);
      return(indirect_ncos_2);
    }
  return(f);
}

static s7_pointer nrxysin_chooser(s7_scheme *sc, s7_pointer f, int args, s7_pointer expr)
{
  if ((args == 1) &&
      (s7_is_symbol(cadr(expr))))
    {
      s7_function_choice_set_direct(sc, expr);
      return(nrxysin_1);
    }
  if ((args == 2) &&
      (s7_is_symbol(cadr(expr))) &&
      (s7_is_symbol(caddr(expr))))
    {
      s7_function_choice_set_direct(sc, expr);
      return(nrxysin_2);
    }
  if ((args == 2) &&
      (s7_is_symbol(cadr(expr))) &&
      (s7_is_pair(caddr(expr))) &&
      (s7_function_choice_is_direct(sc, caddr(expr))))
    {
      s7_function_choice_set_direct(sc, expr);
      if (expr_is_clm_gen(sc, caddr(expr))) return(direct_nrxysin_2);
      return(indirect_nrxysin_2);
    }
  return(f);
}

static s7_pointer nrxycos_chooser(s7_scheme *sc, s7_pointer f, int args, s7_pointer expr)
{
  if ((args == 1) &&
      (s7_is_symbol(cadr(expr))))
    {
      s7_function_choice_set_direct(sc, expr);
      return(nrxycos_1);
    }
  if ((args == 2) &&
      (s7_is_symbol(cadr(expr))) &&
      (s7_is_symbol(caddr(expr))))
    {
      s7_function_choice_set_direct(sc, expr);
      return(nrxycos_2);
    }
  if ((args == 2) &&
      (s7_is_symbol(cadr(expr))) &&
      (s7_is_pair(caddr(expr))) &&
      (s7_function_choice_is_direct(sc, caddr(expr))))
    {
      s7_function_choice_set_direct(sc, expr);
      if (expr_is_clm_gen(sc, caddr(expr))) return(direct_nrxycos_2);
      return(indirect_nrxycos_2);
    }
  return(f);
}

static s7_pointer env_chooser(s7_scheme *sc, s7_pointer f, int args, s7_pointer expr)
{
  if ((args == 1) &&
      (s7_is_symbol(cadr(expr))))
    {
      s7_function_choice_set_direct(sc, expr);
      return(env_1);
    }
  return(f);
}

static s7_pointer readin_chooser(s7_scheme *sc, s7_pointer f, int args, s7_pointer expr)
{
  if ((args == 1) &&
      (s7_is_symbol(cadr(expr))))
    {
      s7_function_choice_set_direct(sc, expr);
      return(readin_1);
    }
  return(f);
}

static s7_pointer comb_chooser(s7_scheme *sc, s7_pointer f, int args, s7_pointer expr)
{
  if ((args == 2) &&
      (s7_is_symbol(cadr(expr))) &&
      (s7_is_symbol(caddr(expr))))
    {
      s7_function_choice_set_direct(sc, expr);
      return(comb_2);
    }
  if ((args == 2) &&
      (s7_is_symbol(cadr(expr))) &&
      (s7_is_pair(caddr(expr))) &&
      (s7_function_choice_is_direct(sc, caddr(expr))))
    {
      s7_function_choice_set_direct(sc, expr);
      if (expr_is_clm_gen(sc, caddr(expr))) return(direct_comb_2);
      return(indirect_comb_2);
    }
  return(f);
}

static s7_pointer notch_chooser(s7_scheme *sc, s7_pointer f, int args, s7_pointer expr)
{
  if ((args == 2) &&
      (s7_is_symbol(cadr(expr))) &&
      (s7_is_symbol(caddr(expr))))
    {
      s7_function_choice_set_direct(sc, expr);
      return(notch_2);
    }
  if ((args == 2) &&
      (s7_is_symbol(cadr(expr))) &&
      (s7_is_pair(caddr(expr))) &&
      (s7_function_choice_is_direct(sc, caddr(expr))))
    {
      s7_function_choice_set_direct(sc, expr);
      if (expr_is_clm_gen(sc, caddr(expr))) return(direct_notch_2);
      return(indirect_notch_2);
    }
  return(f);
}


static s7_pointer all_pass_chooser(s7_scheme *sc, s7_pointer f, int args, s7_pointer expr)
{
  if ((args == 2) &&
      (s7_is_symbol(cadr(expr))))
    {
      if (s7_is_symbol(caddr(expr)))
	{
	  s7_function_choice_set_direct(sc, expr);
	  return(all_pass_2);
	}
      if ((s7_is_pair(caddr(expr))) &&
	  (car(caddr(expr)) == s7_make_symbol(sc, "all-pass")) &&
	  (s7_is_symbol(cadr(caddr(expr)))) &&
	  (s7_is_pair(caddr(caddr(expr)))) &&
	  (car(caddr(caddr(expr))) == s7_make_symbol(sc, "all-pass")) &&
	  (s7_is_symbol(cadr(caddr(caddr(expr))))) &&
	  (s7_is_pair(caddr(caddr(caddr(expr))))) &&
	  (car(caddr(caddr(caddr(expr)))) == s7_make_symbol(sc, "ina")) &&
	  (s7_is_symbol(cadr(caddr(caddr(caddr(expr)))))) &&
	  (caddr(caddr(caddr(caddr(expr)))) == reverb_symbol))
	{
	  s7_function_choice_set_direct(sc, expr);
	  return(jc_reverb_all_passes);
	}
    }
  if ((args == 2) &&
      (s7_is_symbol(cadr(expr))) &&
      (s7_is_pair(caddr(expr))) &&
      (s7_function_choice_is_direct(sc, caddr(expr))))
    {
      s7_function_choice_set_direct(sc, expr);
      if (expr_is_clm_gen(sc, caddr(expr))) return(direct_all_pass_2);
      return(indirect_all_pass_2);
    }
  return(f);
}


static s7_pointer delay_chooser(s7_scheme *sc, s7_pointer f, int args, s7_pointer expr)
{
  if ((args == 2) &&
      (s7_is_symbol(cadr(expr))) &&
      (s7_is_symbol(caddr(expr))))
    {
      s7_function_choice_set_direct(sc, expr);
      return(delay_2);
    }
  if ((args == 2) &&
      (s7_is_symbol(cadr(expr))) &&
      (s7_is_pair(caddr(expr))) &&
      (s7_function_choice_is_direct(sc, caddr(expr))))
    {
      s7_function_choice_set_direct(sc, expr);
      if (expr_is_clm_gen(sc, caddr(expr))) return(direct_delay_2);
      return(indirect_delay_2);
    }
  return(f);
}


static s7_pointer one_pole_chooser(s7_scheme *sc, s7_pointer f, int args, s7_pointer expr)
{
  if ((args == 2) &&
      (s7_is_symbol(cadr(expr))) &&
      (s7_is_symbol(caddr(expr))))
    {
      s7_function_choice_set_direct(sc, expr);
      return(one_pole_2);
    }
  if ((args == 2) &&
      (s7_is_symbol(cadr(expr))) &&
      (s7_is_pair(caddr(expr))) &&
      (s7_function_choice_is_direct(sc, caddr(expr))))
    {
      s7_function_choice_set_direct(sc, expr);
      if (expr_is_clm_gen(sc, caddr(expr))) return(direct_one_pole_2);
      return(indirect_one_pole_2);
    }
  return(f);
}

static s7_pointer two_pole_chooser(s7_scheme *sc, s7_pointer f, int args, s7_pointer expr)
{
  if ((args == 2) &&
      (s7_is_symbol(cadr(expr))) &&
      (s7_is_symbol(caddr(expr))))
    {
      s7_function_choice_set_direct(sc, expr);
      return(two_pole_2);
    }
  if ((args == 2) &&
      (s7_is_symbol(cadr(expr))) &&
      (s7_is_pair(caddr(expr))) &&
      (s7_function_choice_is_direct(sc, caddr(expr))))
    {
      s7_function_choice_set_direct(sc, expr);
      if (expr_is_clm_gen(sc, caddr(expr))) return(direct_two_pole_2);
      return(indirect_two_pole_2);
    }
  return(f);
}

static s7_pointer one_zero_chooser(s7_scheme *sc, s7_pointer f, int args, s7_pointer expr)
{
  if ((args == 2) &&
      (s7_is_symbol(cadr(expr))) &&
      (s7_is_symbol(caddr(expr))))
    {
      s7_function_choice_set_direct(sc, expr);
      return(one_zero_2);
    }
  if ((args == 2) &&
      (s7_is_symbol(cadr(expr))) &&
      (s7_is_pair(caddr(expr))) &&
      (s7_function_choice_is_direct(sc, caddr(expr))))
    {
      s7_function_choice_set_direct(sc, expr);
      if (expr_is_clm_gen(sc, caddr(expr))) return(direct_one_zero_2);
      return(indirect_one_zero_2);
    }
  return(f);
}

static s7_pointer two_zero_chooser(s7_scheme *sc, s7_pointer f, int args, s7_pointer expr)
{
  if ((args == 2) &&
      (s7_is_symbol(cadr(expr))) &&
      (s7_is_symbol(caddr(expr))))
    {
      s7_function_choice_set_direct(sc, expr);
      return(two_zero_2);
    }
  if ((args == 2) &&
      (s7_is_symbol(cadr(expr))) &&
      (s7_is_pair(caddr(expr))) &&
      (s7_function_choice_is_direct(sc, caddr(expr))))
    {
      s7_function_choice_set_direct(sc, expr);
      if (expr_is_clm_gen(sc, caddr(expr))) return(direct_two_zero_2);
      return(indirect_two_zero_2);
    }
  return(f);
}

static s7_pointer moving_average_chooser(s7_scheme *sc, s7_pointer f, int args, s7_pointer expr)
{
  if ((args == 2) &&
      (s7_is_symbol(cadr(expr))) &&
      (s7_is_symbol(caddr(expr))))
    {
      s7_function_choice_set_direct(sc, expr);
      return(moving_average_2);
    }
  if ((args == 2) &&
      (s7_is_symbol(cadr(expr))) &&
      (s7_is_pair(caddr(expr))) &&
      (s7_function_choice_is_direct(sc, caddr(expr))))
    {
      s7_function_choice_set_direct(sc, expr);
      if (expr_is_clm_gen(sc, caddr(expr))) return(direct_moving_average_2);
      return(indirect_moving_average_2);
    }
  return(f);
}

static s7_pointer formant_chooser(s7_scheme *sc, s7_pointer f, int args, s7_pointer expr)
{
  if ((args == 2) &&
      (s7_is_symbol(cadr(expr))) &&
      (s7_is_symbol(caddr(expr))))
    {
      s7_function_choice_set_direct(sc, expr);
      return(formant_2);
    }
  if ((args == 2) &&
      (s7_is_symbol(cadr(expr))) &&
      (s7_is_pair(caddr(expr))) &&
      (s7_function_choice_is_direct(sc, caddr(expr))))
    {
      s7_function_choice_set_direct(sc, expr);
      if (expr_is_clm_gen(sc, caddr(expr))) return(direct_formant_2);
      return(indirect_formant_2);
    }
  return(f);
}

static s7_pointer firmant_chooser(s7_scheme *sc, s7_pointer f, int args, s7_pointer expr)
{
  if ((args == 2) &&
      (s7_is_symbol(cadr(expr))) &&
      (s7_is_symbol(caddr(expr))))
    {
      s7_function_choice_set_direct(sc, expr);
      return(firmant_2);
    }
  if ((args == 2) &&
      (s7_is_symbol(cadr(expr))) &&
      (s7_is_pair(caddr(expr))) &&
      (s7_function_choice_is_direct(sc, caddr(expr))))
    {
      s7_function_choice_set_direct(sc, expr);
      if (expr_is_clm_gen(sc, caddr(expr))) return(direct_firmant_2);
      return(indirect_firmant_2);
    }
  return(f);
}

static s7_pointer filter_chooser(s7_scheme *sc, s7_pointer f, int args, s7_pointer expr)
{
  if ((args == 2) &&
      (s7_is_symbol(cadr(expr))) &&
      (s7_is_symbol(caddr(expr))))
    {
      s7_function_choice_set_direct(sc, expr);
      return(filter_2);
    }
  if ((args == 2) &&
      (s7_is_symbol(cadr(expr))) &&
      (s7_is_pair(caddr(expr))) &&
      (s7_function_choice_is_direct(sc, caddr(expr))))
    {
      s7_function_choice_set_direct(sc, expr);
      if (expr_is_clm_gen(sc, caddr(expr))) return(direct_filter_2);
      return(indirect_filter_2);
    }
  return(f);
}

static s7_pointer fir_filter_chooser(s7_scheme *sc, s7_pointer f, int args, s7_pointer expr)
{
  if ((args == 2) &&
      (s7_is_symbol(cadr(expr))) &&
      (s7_is_symbol(caddr(expr))))
    {
      s7_function_choice_set_direct(sc, expr);
      return(fir_filter_2);
    }
  if ((args == 2) &&
      (s7_is_symbol(cadr(expr))) &&
      (s7_is_pair(caddr(expr))) &&
      (s7_function_choice_is_direct(sc, caddr(expr))))
    {
      s7_function_choice_set_direct(sc, expr);
      if (expr_is_clm_gen(sc, caddr(expr))) return(direct_fir_filter_2);
      return(indirect_fir_filter_2);
    }
  return(f);
}

static s7_pointer iir_filter_chooser(s7_scheme *sc, s7_pointer f, int args, s7_pointer expr)
{
  if ((args == 2) &&
      (s7_is_symbol(cadr(expr))) &&
      (s7_is_symbol(caddr(expr))))
    {
      s7_function_choice_set_direct(sc, expr);
      return(iir_filter_2);
    }
  if ((args == 2) &&
      (s7_is_symbol(cadr(expr))) &&
      (s7_is_pair(caddr(expr))) &&
      (s7_function_choice_is_direct(sc, caddr(expr))))
    {
      s7_function_choice_set_direct(sc, expr);
      if (expr_is_clm_gen(sc, caddr(expr))) return(direct_iir_filter_2);
      return(indirect_iir_filter_2);
    }
  return(f);
}

static s7_pointer triangle_wave_chooser(s7_scheme *sc, s7_pointer f, int args, s7_pointer expr)
{
  if ((args == 1) &&
      (s7_is_symbol(cadr(expr))))
    {
      s7_function_choice_set_direct(sc, expr);
      return(triangle_wave_1);
    }
  if ((args == 2) &&
      (s7_is_symbol(cadr(expr))) &&
      (s7_is_symbol(caddr(expr))))
    {
      s7_function_choice_set_direct(sc, expr);
      return(triangle_wave_2);
    }
  if ((args == 2) &&
      (s7_is_symbol(cadr(expr))) &&
      (s7_is_pair(caddr(expr))) &&
      (s7_function_choice_is_direct(sc, caddr(expr))))
    {
      s7_function_choice_set_direct(sc, expr);
      if (expr_is_clm_gen(sc, caddr(expr))) return(direct_triangle_wave_2);
      return(indirect_triangle_wave_2);
    }
  return(f);
}

static s7_pointer sawtooth_wave_chooser(s7_scheme *sc, s7_pointer f, int args, s7_pointer expr)
{
  if ((args == 1) &&
      (s7_is_symbol(cadr(expr))))
    {
      s7_function_choice_set_direct(sc, expr);
      return(sawtooth_wave_1);
    }
  if ((args == 2) &&
      (s7_is_symbol(cadr(expr))) &&
      (s7_is_symbol(caddr(expr))))
    {
      s7_function_choice_set_direct(sc, expr);
      return(sawtooth_wave_2);
    }
  if ((args == 2) &&
      (s7_is_symbol(cadr(expr))) &&
      (s7_is_pair(caddr(expr))) &&
      (s7_function_choice_is_direct(sc, caddr(expr))))
    {
      s7_function_choice_set_direct(sc, expr);
      if (expr_is_clm_gen(sc, caddr(expr))) return(direct_sawtooth_wave_2);
      return(indirect_sawtooth_wave_2);
    }
  return(f);
}

static s7_pointer square_wave_chooser(s7_scheme *sc, s7_pointer f, int args, s7_pointer expr)
{
  if ((args == 1) &&
      (s7_is_symbol(cadr(expr))))
    {
      s7_function_choice_set_direct(sc, expr);
      return(square_wave_1);
    }
  if ((args == 2) &&
      (s7_is_symbol(cadr(expr))) &&
      (s7_is_symbol(caddr(expr))))
    {
      s7_function_choice_set_direct(sc, expr);
      return(square_wave_2);
    }
  if ((args == 2) &&
      (s7_is_symbol(cadr(expr))) &&
      (s7_is_pair(caddr(expr))) &&
      (s7_function_choice_is_direct(sc, caddr(expr))))
    {
      s7_function_choice_set_direct(sc, expr);
      if (expr_is_clm_gen(sc, caddr(expr))) return(direct_square_wave_2);
      return(indirect_square_wave_2);
    }
  return(f);
}

static s7_pointer pulse_train_chooser(s7_scheme *sc, s7_pointer f, int args, s7_pointer expr)
{
  if ((args == 1) &&
      (s7_is_symbol(cadr(expr))))
    {
      s7_function_choice_set_direct(sc, expr);
      return(pulse_train_1);
    }
  if ((args == 2) &&
      (s7_is_symbol(cadr(expr))) &&
      (s7_is_symbol(caddr(expr))))
    {
      s7_function_choice_set_direct(sc, expr);
      return(pulse_train_2);
    }
  if ((args == 2) &&
      (s7_is_symbol(cadr(expr))) &&
      (s7_is_pair(caddr(expr))) &&
      (s7_function_choice_is_direct(sc, caddr(expr))))
    {
      s7_function_choice_set_direct(sc, expr);
      if (expr_is_clm_gen(sc, caddr(expr))) return(direct_pulse_train_2);
      return(indirect_pulse_train_2);
    }
  return(f);
}


static s7_pointer rand_chooser(s7_scheme *sc, s7_pointer f, int args, s7_pointer expr)
{
  if ((args == 1) &&
      (s7_is_symbol(cadr(expr))))
    {
      s7_function_choice_set_direct(sc, expr);
      return(rand_1);
    }
  if ((args == 2) &&
      (s7_is_symbol(cadr(expr))) &&
      (s7_is_symbol(caddr(expr))))
    {
      s7_function_choice_set_direct(sc, expr);
      return(rand_2);
    }
  if ((args == 2) &&
      (s7_is_symbol(cadr(expr))) &&
      (s7_is_pair(caddr(expr))) &&
      (s7_function_choice_is_direct(sc, caddr(expr))))
    {
      s7_function_choice_set_direct(sc, expr);
      if (expr_is_clm_gen(sc, caddr(expr))) return(direct_rand_2);
      return(indirect_rand_2);
    }
  return(f);
}

static s7_pointer rand_interp_chooser(s7_scheme *sc, s7_pointer f, int args, s7_pointer expr)
{
  if ((args == 1) &&
      (s7_is_symbol(cadr(expr))))
    {
      s7_function_choice_set_direct(sc, expr);
      return(rand_interp_1);
    }
  if ((args == 2) &&
      (s7_is_symbol(cadr(expr))) &&
      (s7_is_symbol(caddr(expr))))
    {
      s7_function_choice_set_direct(sc, expr);
      return(rand_interp_2);
    }
  if ((args == 2) &&
      (s7_is_symbol(cadr(expr))) &&
      (s7_is_pair(caddr(expr))) &&
      (s7_function_choice_is_direct(sc, caddr(expr))))
    {
      s7_function_choice_set_direct(sc, expr);
      if (expr_is_clm_gen(sc, caddr(expr))) return(direct_rand_interp_2);
      return(indirect_rand_interp_2);
    }
  return(f);
}

static s7_pointer polyshape_chooser(s7_scheme *sc, s7_pointer f, int args, s7_pointer expr)
{
  if ((args == 2) &&
      (s7_is_symbol(cadr(expr))) &&
      (s7_is_symbol(caddr(expr))))
    {
      s7_function_choice_set_direct(sc, expr);
      return(polyshape_2);
    }
  if ((args == 2) &&
      (s7_is_symbol(cadr(expr))) &&
      (s7_is_pair(caddr(expr))) &&
      (s7_function_choice_is_direct(sc, caddr(expr))))
    {
      s7_function_choice_set_direct(sc, expr);
      if (expr_is_clm_gen(sc, caddr(expr))) return(direct_polyshape_2);
      return(indirect_polyshape_2);
    }
  return(f);
}

static s7_pointer ssb_am_chooser(s7_scheme *sc, s7_pointer f, int args, s7_pointer expr)
{
  if ((args == 2) &&
      (s7_is_symbol(cadr(expr))) &&
      (s7_is_symbol(caddr(expr))))
    {
      s7_function_choice_set_direct(sc, expr);
      return(ssb_am_2);
    }
  if ((args == 2) &&
      (s7_is_symbol(cadr(expr))) &&
      (s7_is_pair(caddr(expr))) &&
      (s7_function_choice_is_direct(sc, caddr(expr))))
    {
      s7_function_choice_set_direct(sc, expr);
      if (expr_is_clm_gen(sc, caddr(expr))) return(direct_ssb_am_2);
      return(indirect_ssb_am_2);
    }
  return(f);
}

static s7_pointer asymmetric_fm_chooser(s7_scheme *sc, s7_pointer f, int args, s7_pointer expr)
{
  if ((args == 2) &&
      (s7_is_symbol(cadr(expr))) &&
      (s7_is_symbol(caddr(expr))))
    {
      s7_function_choice_set_direct(sc, expr);
      return(asymmetric_fm_2);
    }
  if ((args == 2) &&
      (s7_is_symbol(cadr(expr))) &&
      (s7_is_pair(caddr(expr))) &&
      (s7_function_choice_is_direct(sc, caddr(expr))))
    {
      s7_function_choice_set_direct(sc, expr);
      if (expr_is_clm_gen(sc, caddr(expr))) return(direct_asymmetric_fm_2);
      return(indirect_asymmetric_fm_2);
    }
  return(f);
}

static s7_pointer filtered_comb_chooser(s7_scheme *sc, s7_pointer f, int args, s7_pointer expr)
{
  if ((args == 2) &&
      (s7_is_symbol(cadr(expr))) &&
      (s7_is_symbol(caddr(expr))))
    {
      s7_function_choice_set_direct(sc, expr);
      return(filtered_comb_2);
    }
  if ((args == 2) &&
      (s7_is_symbol(cadr(expr))) &&
      (s7_is_pair(caddr(expr))) &&
      (s7_function_choice_is_direct(sc, caddr(expr))))
    {
      s7_function_choice_set_direct(sc, expr);
      if (expr_is_clm_gen(sc, caddr(expr))) return(direct_filtered_comb_2);
      return(indirect_filtered_comb_2);
    }
  return(f);
}


static s7_pointer frame_to_frame_chooser(s7_scheme *sc, s7_pointer f, int args, s7_pointer expr)
{
  if ((args == 3) &&
      (s7_is_symbol(s7_cadr(expr))) &&
      (s7_is_symbol(s7_caddr(expr))) &&
      (s7_is_symbol(s7_cadddr(expr))))
    {
      s7_function_choice_set_direct(sc, expr);
      return(frame_to_frame_sss);
    }

  /* PERHAPS: (sample->frame s s s) (sample->file ?)
   * TODO: in snd-test: 17835: ;delay size 0: #<vct[len=5]: 0.000 1.000 0.000 0.000 0.000>
   */
  return(f);
}


static s7_pointer locsig_chooser(s7_scheme *sc, s7_pointer f, int args, s7_pointer expr)
{
  if ((args == 3) &&
      (s7_is_symbol(cadr(expr))) &&
      (s7_is_symbol(caddr(expr))) &&
      (s7_is_pair(cadr(cddr(expr)))))
    {
      if (s7_function_choice(sc, cadr(cddr(expr))) == g_fm_violin_1)
	{
	  s7_function_choice_set_direct(sc, expr);
	  return(fm_violin_2);
	}
      if (s7_function_choice_is_direct(sc, cadr(cddr(expr))))
	{
	  s7_function_choice_set_direct(sc, expr);
	  return(indirect_locsig_3);
	}
    }
  return(f);
}


static s7_pointer outa_chooser(s7_scheme *sc, s7_pointer f, int args, s7_pointer expr)
{
  if (args == 2)
    {
      if ((s7_is_symbol(cadr(expr))) &&
	  (s7_is_symbol(caddr(expr))))
	{
	  /* fprintf(stderr, "\nouta ss: %s\n", DISPLAY(expr)); */
	  s7_function_choice_set_direct(sc, expr);
	  return(indirect_outa_ss);
	}

      if ((s7_is_symbol(cadr(expr))) &&
	  (s7_is_pair(caddr(expr))))
	{
	  /*
	    if (!s7_function_choice_is_direct(sc, caddr(expr)))
	      fprintf(stderr, ";----------------\n;   %s\n;----------------\n", DISPLAY(caddr(expr)));
	  */
	  if (s7_function_choice(sc, caddr(expr)) == g_mul_s_delay_2)
	    {
	      s7_function_choice_set_direct(sc, expr);
	      return(outa_mul_s_delay);
	    }
	  if (s7_function_choice(sc, caddr(expr)) == g_env_polywave_env)
	    {
	      s7_function_choice_set_direct(sc, expr);
	      return(outa_env_polywave_env);
	    }
	  if (s7_function_choice_is_direct(sc, caddr(expr)))
	    {
	      s7_function_choice_set_direct(sc, expr);
	      return(indirect_outa_2);
	    }
	  
	  if ((car(caddr(expr)) == subtract_symbol) &&
	      (s7_list_length(sc, caddr(expr)) == 3) &&
	      (s7_is_symbol(cadr(caddr(expr)))) &&
	      (s7_is_symbol(caddr(caddr(expr)))))
	    {
	      /* fprintf(stderr, "\nouta sub: %s\n", DISPLAY(expr)); */
	      s7_function_choice_set_direct(sc, expr);
	      return(indirect_outa_sub_2);
	    }
	}
      if ((s7_is_pair(cadr(expr))) &&
	  (car(cadr(expr)) == add_symbol) &&
	  (s7_list_length(sc, cadr(expr)) == 3) &&
	  (s7_is_symbol(cadr(cadr(expr)))) &&
	  (s7_is_symbol(caddr(cadr(expr)))) &&
	  (s7_is_pair(caddr(expr))) &&
	  (s7_function_choice_is_direct(sc, caddr(expr))))
	{
	  /* fprintf(stderr, "\nouta add: %s\n", DISPLAY(expr)); */
	  s7_function_choice_set_direct(sc, expr);
	  return(indirect_outa_add_2);
	}
    }
  /* fprintf(stderr, "\nouta nope: %s\n", DISPLAY(expr)); */
  return(f);
}

static s7_pointer ina_chooser(s7_scheme *sc, s7_pointer f, int args, s7_pointer expr)
{
  if (args == 2)
    {
      if ((s7_is_symbol(cadr(expr))) &&
	  (s7_is_symbol(caddr(expr))) &&
	  (caddr(expr) == reverb_symbol))
	{
	  s7_function_choice_set_direct(sc, expr);
	  return(ina_reverb_2);
	}
    }
  return(f);
}


static s7_pointer frame_to_file_chooser(s7_scheme *sc, s7_pointer f, int args, s7_pointer expr)
{
  /* fprintf(stderr, "f->file %s\n", DISPLAY(expr)); */
  if (args == 3)
    {
      if ((s7_cadr(expr) == output_symbol) &&
	  (s7_is_symbol(s7_caddr(expr))) &&
	  (s7_is_pair(s7_cadddr(expr))) &&
	  (s7_function_choice_is_direct(sc, s7_cadddr(expr))))
	{
	  s7_function_choice_set_direct(sc, expr);
	  return(indirect_frame_to_file_3);
	}
    }
  return(f);
}




static s7_pointer clm_make_function(s7_scheme *sc, const char *name, s7_function f, 
				    int required_args, int optional_args, bool rest_arg, const char *doc,
				    unsigned int gclass,
				    s7_pointer mul_c, s7_pointer mul_s, s7_pointer e, s7_pointer mul_c1, s7_pointer mul_s1, s7_pointer e1)
{		
  s7_pointer fin;
  fin = s7_make_function(sc, name, f, required_args, optional_args, rest_arg, doc);
  s7_function_set_class(sc, fin, gclass);
  s7_function_chooser_set_data(sc, fin, (void *)make_choices(mul_c, mul_s, e, mul_c1, mul_s1, e1));
  return(fin);
}

static s7_pointer clm_make_function_no_choice(s7_scheme *sc, const char *name, s7_function f, 
					      int required_args, int optional_args, bool rest_arg, const char *doc,
					      unsigned int gclass)
{		
  s7_pointer fin;
  fin = s7_make_function(sc, name, f, required_args, optional_args, rest_arg, doc);
  s7_function_set_class(sc, fin, gclass);
  return(fin);
}



static void init_choosers(s7_scheme *sc)
{
  s7_pointer f;
  unsigned int gen_class;

  env_symbol = s7_make_symbol(sc, "env");
  vector_ref_symbol = s7_make_symbol(sc, "vector-ref");
  all_pass_symbol = s7_make_symbol(sc, "all-pass");
  ina_symbol = s7_make_symbol(sc, "ina");
  comb_symbol = s7_make_symbol(sc, "comb");
  polywave_symbol = s7_make_symbol(sc, "polywave");
  triangle_wave_symbol = s7_make_symbol(sc, "triangle-wave");
  rand_interp_symbol = s7_make_symbol(sc, "rand-interp");
  oscil_symbol = s7_make_symbol(sc, "oscil");
  add_symbol = s7_make_symbol(sc, "+");
  subtract_symbol = s7_make_symbol(sc, "-");
  multiply_symbol = s7_make_symbol(sc, "*");
  reverb_symbol = s7_make_symbol(sc, "*reverb*");
  output_symbol = s7_make_symbol(sc, "*output*");

  f = s7_name_to_value(sc, "*");
  initial_multiply_chooser = s7_function_chooser(sc, f);
  s7_function_set_chooser(sc, f, clm_multiply_chooser);
  gen_class = s7_function_class(sc, f);

  fm_violin_1 = clm_make_function(sc, "*", g_fm_violin_1, 2, 0, false, "fm-violin optimization", gen_class, NULL, NULL, NULL, NULL, NULL, NULL);
  env_polywave = clm_make_function(sc, "*", g_env_polywave, 2, 0, false, "fm-violin optimization", gen_class, NULL, NULL, NULL, NULL, NULL, NULL);
  mul_direct_2 = clm_make_function(sc, "*", g_mul_direct_2, 2, 0, false, "* optimization", gen_class, NULL, NULL, NULL, NULL, NULL, NULL);
  mul_direct_3 = clm_make_function(sc, "*", g_mul_direct_3, 3, 0, false, "* optimization", gen_class, NULL, NULL, NULL, NULL, NULL, NULL);
  mul_c_direct = clm_make_function(sc, "*", g_mul_c_direct, 2, 0, false, "* optimization", gen_class, NULL, NULL, NULL, NULL, NULL, NULL);
  mul_s_direct = clm_make_function_no_choice(sc, "*", g_mul_s_direct, 2, 0, false, "* optimization", gen_class);
  mul_1s_direct = clm_make_function_no_choice(sc, "*", g_mul_1s_direct, 2, 0, false, "* optimization", gen_class);
  mul_env_direct = clm_make_function(sc, "*", g_mul_env_direct, 2, 0, false, "* optimization", gen_class, NULL, NULL, NULL, NULL, NULL, NULL);
  env_polywave_env = clm_make_function(sc, "*", g_env_polywave_env, 2, 0, false, "animals optimization", gen_class, NULL, NULL, NULL, NULL, NULL, NULL);
  mul_c_oscil_2 = clm_make_function(sc, "*", g_mul_c_oscil_2, 2, 0, false, "* optimization", gen_class, NULL, NULL, NULL, NULL, NULL, NULL);
  mul_c_oscil_1 = clm_make_function(sc, "*", g_mul_c_oscil_1, 1, 0, false, "* optimization", gen_class, NULL, NULL, NULL, NULL, NULL, NULL);
  mul_c_env_1 = clm_make_function(sc, "*", g_mul_c_env_1, 1, 0, false, "* optimization", gen_class, NULL, NULL, NULL, NULL, NULL, NULL);
  mul_c_readin_1 = clm_make_function(sc, "*", g_mul_c_readin_1, 1, 0, false, "* optimization", gen_class, NULL, NULL, NULL, NULL, NULL, NULL);
  mul_c_polywave_2 = clm_make_function(sc, "*", g_mul_c_polywave_2, 2, 0, false, "* optimization", gen_class, NULL, NULL, NULL, NULL, NULL, NULL);
  mul_c_polywave_1 = clm_make_function(sc, "*", g_mul_c_polywave_1, 1, 0, false, "* optimization", gen_class, NULL, NULL, NULL, NULL, NULL, NULL);
  mul_c_table_lookup_2 = clm_make_function(sc, "*", g_mul_c_table_lookup_2, 2, 0, false, "* optimization", gen_class, NULL, NULL, NULL, NULL, NULL, NULL);
  mul_c_table_lookup_1 = clm_make_function(sc, "*", g_mul_c_table_lookup_1, 1, 0, false, "* optimization", gen_class, NULL, NULL, NULL, NULL, NULL, NULL);
  mul_c_wave_train_2 = clm_make_function(sc, "*", g_mul_c_wave_train_2, 2, 0, false, "* optimization", gen_class, NULL, NULL, NULL, NULL, NULL, NULL);
  mul_c_wave_train_1 = clm_make_function(sc, "*", g_mul_c_wave_train_1, 1, 0, false, "* optimization", gen_class, NULL, NULL, NULL, NULL, NULL, NULL);
  mul_c_comb_2 = clm_make_function(sc, "*", g_mul_c_comb_2, 2, 0, false, "* optimization", gen_class, NULL, NULL, NULL, NULL, NULL, NULL);
  mul_c_notch_2 = clm_make_function(sc, "*", g_mul_c_notch_2, 2, 0, false, "* optimization", gen_class, NULL, NULL, NULL, NULL, NULL, NULL);
  mul_c_all_pass_2 = clm_make_function(sc, "*", g_mul_c_all_pass_2, 2, 0, false, "* optimization", gen_class, NULL, NULL, NULL, NULL, NULL, NULL);
  mul_c_delay_2 = clm_make_function(sc, "*", g_mul_c_delay_2, 2, 0, false, "* optimization", gen_class, NULL, NULL, NULL, NULL, NULL, NULL);
  mul_c_moving_average_2 = clm_make_function(sc, "*", g_mul_c_moving_average_2, 2, 0, false, "* optimization", gen_class, NULL, NULL, NULL, NULL, NULL, NULL);
  mul_c_rand_2 = clm_make_function(sc, "*", g_mul_c_rand_2, 2, 0, false, "* optimization", gen_class, NULL, NULL, NULL, NULL, NULL, NULL);
  mul_c_rand_1 = clm_make_function(sc, "*", g_mul_c_rand_1, 1, 0, false, "* optimization", gen_class, NULL, NULL, NULL, NULL, NULL, NULL);
  mul_c_rand_interp_2 = clm_make_function(sc, "*", g_mul_c_rand_interp_2, 2, 0, false, "* optimization", gen_class, NULL, NULL, NULL, NULL, NULL, NULL);
  mul_c_rand_interp_1 = clm_make_function(sc, "*", g_mul_c_rand_interp_1, 1, 0, false, "* optimization", gen_class, NULL, NULL, NULL, NULL, NULL, NULL);
  mul_c_ncos_2 = clm_make_function(sc, "*", g_mul_c_ncos_2, 2, 0, false, "* optimization", gen_class, NULL, NULL, NULL, NULL, NULL, NULL);
  mul_c_ncos_1 = clm_make_function(sc, "*", g_mul_c_ncos_1, 1, 0, false, "* optimization", gen_class, NULL, NULL, NULL, NULL, NULL, NULL);
  mul_c_nsin_2 = clm_make_function(sc, "*", g_mul_c_nsin_2, 2, 0, false, "* optimization", gen_class, NULL, NULL, NULL, NULL, NULL, NULL);
  mul_c_nsin_1 = clm_make_function(sc, "*", g_mul_c_nsin_1, 1, 0, false, "* optimization", gen_class, NULL, NULL, NULL, NULL, NULL, NULL);
  mul_c_sawtooth_wave_2 = clm_make_function(sc, "*", g_mul_c_sawtooth_wave_2, 2, 0, false, "* optimization", gen_class, NULL, NULL, NULL, NULL, NULL, NULL);
  mul_c_sawtooth_wave_1 = clm_make_function(sc, "*", g_mul_c_sawtooth_wave_1, 1, 0, false, "* optimization", gen_class, NULL, NULL, NULL, NULL, NULL, NULL);
  mul_c_pulse_train_2 = clm_make_function(sc, "*", g_mul_c_pulse_train_2, 2, 0, false, "* optimization", gen_class, NULL, NULL, NULL, NULL, NULL, NULL);
  mul_c_pulse_train_1 = clm_make_function(sc, "*", g_mul_c_pulse_train_1, 1, 0, false, "* optimization", gen_class, NULL, NULL, NULL, NULL, NULL, NULL);
  mul_c_square_wave_2 = clm_make_function(sc, "*", g_mul_c_square_wave_2, 2, 0, false, "* optimization", gen_class, NULL, NULL, NULL, NULL, NULL, NULL);
  mul_c_square_wave_1 = clm_make_function(sc, "*", g_mul_c_square_wave_1, 1, 0, false, "* optimization", gen_class, NULL, NULL, NULL, NULL, NULL, NULL);
  mul_c_triangle_wave_2 = clm_make_function(sc, "*", g_mul_c_triangle_wave_2, 2, 0, false, "* optimization", gen_class, NULL, NULL, NULL, NULL, NULL, NULL);
  mul_c_triangle_wave_1 = clm_make_function(sc, "*", g_mul_c_triangle_wave_1, 1, 0, false, "* optimization", gen_class, NULL, NULL, NULL, NULL, NULL, NULL);
  mul_c_nrxysin_2 = clm_make_function(sc, "*", g_mul_c_nrxysin_2, 2, 0, false, "* optimization", gen_class, NULL, NULL, NULL, NULL, NULL, NULL);
  mul_c_nrxysin_1 = clm_make_function(sc, "*", g_mul_c_nrxysin_1, 1, 0, false, "* optimization", gen_class, NULL, NULL, NULL, NULL, NULL, NULL);
  mul_c_nrxycos_2 = clm_make_function(sc, "*", g_mul_c_nrxycos_2, 2, 0, false, "* optimization", gen_class, NULL, NULL, NULL, NULL, NULL, NULL);
  mul_c_nrxycos_1 = clm_make_function(sc, "*", g_mul_c_nrxycos_1, 1, 0, false, "* optimization", gen_class, NULL, NULL, NULL, NULL, NULL, NULL);
  mul_c_one_zero_2 = clm_make_function(sc, "*", g_mul_c_one_zero_2, 2, 0, false, "* optimization", gen_class, NULL, NULL, NULL, NULL, NULL, NULL);
  mul_c_one_pole_2 = clm_make_function(sc, "*", g_mul_c_one_pole_2, 2, 0, false, "* optimization", gen_class, NULL, NULL, NULL, NULL, NULL, NULL);
  mul_c_two_zero_2 = clm_make_function(sc, "*", g_mul_c_two_zero_2, 2, 0, false, "* optimization", gen_class, NULL, NULL, NULL, NULL, NULL, NULL);
  mul_c_two_pole_2 = clm_make_function(sc, "*", g_mul_c_two_pole_2, 2, 0, false, "* optimization", gen_class, NULL, NULL, NULL, NULL, NULL, NULL);
  mul_c_filter_2 = clm_make_function(sc, "*", g_mul_c_filter_2, 2, 0, false, "* optimization", gen_class, NULL, NULL, NULL, NULL, NULL, NULL);
  mul_c_fir_filter_2 = clm_make_function(sc, "*", g_mul_c_fir_filter_2, 2, 0, false, "* optimization", gen_class, NULL, NULL, NULL, NULL, NULL, NULL);
  mul_c_iir_filter_2 = clm_make_function(sc, "*", g_mul_c_iir_filter_2, 2, 0, false, "* optimization", gen_class, NULL, NULL, NULL, NULL, NULL, NULL);
  mul_c_formant_2 = clm_make_function(sc, "*", g_mul_c_formant_2, 2, 0, false, "* optimization", gen_class, NULL, NULL, NULL, NULL, NULL, NULL);
  mul_c_firmant_2 = clm_make_function(sc, "*", g_mul_c_firmant_2, 2, 0, false, "* optimization", gen_class, NULL, NULL, NULL, NULL, NULL, NULL);
  mul_c_polyshape_2 = clm_make_function(sc, "*", g_mul_c_polyshape_2, 2, 0, false, "* optimization", gen_class, NULL, NULL, NULL, NULL, NULL, NULL);
  mul_c_filtered_comb_2 = clm_make_function(sc, "*", g_mul_c_filtered_comb_2, 2, 0, false, "* optimization", gen_class, NULL, NULL, NULL, NULL, NULL, NULL);
  mul_c_asymmetric_fm_2 = clm_make_function(sc, "*", g_mul_c_asymmetric_fm_2, 2, 0, false, "* optimization", gen_class, NULL, NULL, NULL, NULL, NULL, NULL);
  mul_c_ssb_am_2 = clm_make_function(sc, "*", g_mul_c_ssb_am_2, 2, 0, false, "* optimization", gen_class, NULL, NULL, NULL, NULL, NULL, NULL);

  mul_s_oscil_2 = clm_make_function_no_choice(sc, "*", g_mul_s_oscil_2, 2, 0, false, "* optimization", gen_class);
  mul_s_oscil_1 = clm_make_function_no_choice(sc, "*", g_mul_s_oscil_1, 1, 0, false, "* optimization", gen_class);
  mul_s_env_1 = clm_make_function_no_choice(sc, "*", g_mul_s_env_1, 1, 0, false, "* optimization", gen_class);
  mul_s_readin_1 = clm_make_function_no_choice(sc, "*", g_mul_s_readin_1, 1, 0, false, "* optimization", gen_class);
  mul_s_polywave_2 = clm_make_function_no_choice(sc, "*", g_mul_s_polywave_2, 2, 0, false, "* optimization", gen_class);
  mul_s_polywave_1 = clm_make_function_no_choice(sc, "*", g_mul_s_polywave_1, 1, 0, false, "* optimization", gen_class);
  mul_s_table_lookup_2 = clm_make_function_no_choice(sc, "*", g_mul_s_table_lookup_2, 2, 0, false, "* optimization", gen_class);
  mul_s_table_lookup_1 = clm_make_function_no_choice(sc, "*", g_mul_s_table_lookup_1, 1, 0, false, "* optimization", gen_class);
  mul_s_wave_train_2 = clm_make_function_no_choice(sc, "*", g_mul_s_wave_train_2, 2, 0, false, "* optimization", gen_class);
  mul_s_wave_train_1 = clm_make_function_no_choice(sc, "*", g_mul_s_wave_train_1, 1, 0, false, "* optimization", gen_class);
  mul_s_comb_2 = clm_make_function_no_choice(sc, "*", g_mul_s_comb_2, 2, 0, false, "* optimization", gen_class);
  mul_s_notch_2 = clm_make_function_no_choice(sc, "*", g_mul_s_notch_2, 2, 0, false, "* optimization", gen_class);
  mul_s_all_pass_2 = clm_make_function_no_choice(sc, "*", g_mul_s_all_pass_2, 2, 0, false, "* optimization", gen_class);
  mul_s_delay_2 = clm_make_function_no_choice(sc, "*", g_mul_s_delay_2, 2, 0, false, "* optimization", gen_class);
  mul_s_moving_average_2 = clm_make_function_no_choice(sc, "*", g_mul_s_moving_average_2, 2, 0, false, "* optimization", gen_class);
  mul_s_rand_2 = clm_make_function_no_choice(sc, "*", g_mul_s_rand_2, 2, 0, false, "* optimization", gen_class);
  mul_s_rand_1 = clm_make_function_no_choice(sc, "*", g_mul_s_rand_1, 1, 0, false, "* optimization", gen_class);
  mul_s_rand_interp_2 = clm_make_function_no_choice(sc, "*", g_mul_s_rand_interp_2, 2, 0, false, "* optimization", gen_class);
  mul_s_rand_interp_1 = clm_make_function_no_choice(sc, "*", g_mul_s_rand_interp_1, 1, 0, false, "* optimization", gen_class);
  mul_s_ncos_2 = clm_make_function_no_choice(sc, "*", g_mul_s_ncos_2, 2, 0, false, "* optimization", gen_class);
  mul_s_ncos_1 = clm_make_function_no_choice(sc, "*", g_mul_s_ncos_1, 1, 0, false, "* optimization", gen_class);
  mul_s_nsin_2 = clm_make_function_no_choice(sc, "*", g_mul_s_nsin_2, 2, 0, false, "* optimization", gen_class);
  mul_s_nsin_1 = clm_make_function_no_choice(sc, "*", g_mul_s_nsin_1, 1, 0, false, "* optimization", gen_class);
  mul_s_sawtooth_wave_2 = clm_make_function_no_choice(sc, "*", g_mul_s_sawtooth_wave_2, 2, 0, false, "* optimization", gen_class);
  mul_s_sawtooth_wave_1 = clm_make_function_no_choice(sc, "*", g_mul_s_sawtooth_wave_1, 1, 0, false, "* optimization", gen_class);
  mul_s_pulse_train_2 = clm_make_function_no_choice(sc, "*", g_mul_s_pulse_train_2, 2, 0, false, "* optimization", gen_class);
  mul_s_pulse_train_1 = clm_make_function_no_choice(sc, "*", g_mul_s_pulse_train_1, 1, 0, false, "* optimization", gen_class);
  mul_s_square_wave_2 = clm_make_function_no_choice(sc, "*", g_mul_s_square_wave_2, 2, 0, false, "* optimization", gen_class);
  mul_s_square_wave_1 = clm_make_function_no_choice(sc, "*", g_mul_s_square_wave_1, 1, 0, false, "* optimization", gen_class);
  mul_s_triangle_wave_2 = clm_make_function_no_choice(sc, "*", g_mul_s_triangle_wave_2, 2, 0, false, "* optimization", gen_class);
  mul_s_triangle_wave_1 = clm_make_function_no_choice(sc, "*", g_mul_s_triangle_wave_1, 1, 0, false, "* optimization", gen_class);
  mul_s_nrxysin_2 = clm_make_function_no_choice(sc, "*", g_mul_s_nrxysin_2, 2, 0, false, "* optimization", gen_class);
  mul_s_nrxysin_1 = clm_make_function_no_choice(sc, "*", g_mul_s_nrxysin_1, 1, 0, false, "* optimization", gen_class);
  mul_s_nrxycos_2 = clm_make_function_no_choice(sc, "*", g_mul_s_nrxycos_2, 2, 0, false, "* optimization", gen_class);
  mul_s_nrxycos_1 = clm_make_function_no_choice(sc, "*", g_mul_s_nrxycos_1, 1, 0, false, "* optimization", gen_class);
  mul_s_one_zero_2 = clm_make_function_no_choice(sc, "*", g_mul_s_one_zero_2, 2, 0, false, "* optimization", gen_class);
  mul_s_one_pole_2 = clm_make_function_no_choice(sc, "*", g_mul_s_one_pole_2, 2, 0, false, "* optimization", gen_class);
  mul_s_two_zero_2 = clm_make_function_no_choice(sc, "*", g_mul_s_two_zero_2, 2, 0, false, "* optimization", gen_class);
  mul_s_two_pole_2 = clm_make_function_no_choice(sc, "*", g_mul_s_two_pole_2, 2, 0, false, "* optimization", gen_class);
  mul_s_filter_2 = clm_make_function_no_choice(sc, "*", g_mul_s_filter_2, 2, 0, false, "* optimization", gen_class);
  mul_s_fir_filter_2 = clm_make_function_no_choice(sc, "*", g_mul_s_fir_filter_2, 2, 0, false, "* optimization", gen_class);
  mul_s_iir_filter_2 = clm_make_function_no_choice(sc, "*", g_mul_s_iir_filter_2, 2, 0, false, "* optimization", gen_class);
  mul_s_formant_2 = clm_make_function_no_choice(sc, "*", g_mul_s_formant_2, 2, 0, false, "* optimization", gen_class);
  mul_s_firmant_2 = clm_make_function_no_choice(sc, "*", g_mul_s_firmant_2, 2, 0, false, "* optimization", gen_class);
  mul_s_polyshape_2 = clm_make_function_no_choice(sc, "*", g_mul_s_polyshape_2, 2, 0, false, "* optimization", gen_class);
  mul_s_ssb_am_2 = clm_make_function_no_choice(sc, "*", g_mul_s_ssb_am_2, 2, 0, false, "* optimization", gen_class);
  mul_s_filtered_comb_2 = clm_make_function_no_choice(sc, "*", g_mul_s_filtered_comb_2, 2, 0, false, "* optimization", gen_class);
  mul_s_asymmetric_fm_2 = clm_make_function_no_choice(sc, "*", g_mul_s_asymmetric_fm_2, 2, 0, false, "* optimization", gen_class);

  env_oscil_2 = clm_make_function(sc, "*", g_env_oscil_2, 2, 0, false, "* optimization", gen_class, NULL, NULL, NULL, NULL, NULL, NULL);
  env_oscil_1 = clm_make_function(sc, "*", g_env_oscil_1, 1, 0, false, "* optimization", gen_class, NULL, NULL, NULL, NULL, NULL, NULL);
  env_env_1 = clm_make_function(sc, "*", g_env_env_1, 1, 0, false, "* optimization", gen_class,	NULL, NULL, NULL, NULL, NULL, NULL);
  env_readin_1 = clm_make_function(sc, "*", g_env_readin_1, 1, 0, false, "* optimization", gen_class, NULL, NULL, NULL, NULL, NULL, NULL);
  env_polywave_2 = clm_make_function(sc, "*", g_env_polywave_2, 2, 0, false, "* optimization", gen_class, NULL, NULL, NULL, NULL, NULL, NULL);
  env_polywave_1 = clm_make_function(sc, "*", g_env_polywave_1, 1, 0, false, "* optimization", gen_class, NULL, NULL, NULL, NULL, NULL, NULL);
  env_table_lookup_2 = clm_make_function(sc, "*", g_env_table_lookup_2, 2, 0, false, "* optimization", gen_class, NULL, NULL, NULL, NULL, NULL, NULL);
  env_table_lookup_1 = clm_make_function(sc, "*", g_env_table_lookup_1, 1, 0, false, "* optimization", gen_class, NULL, NULL, NULL, NULL, NULL, NULL);
  env_wave_train_2 = clm_make_function(sc, "*", g_env_wave_train_2, 2, 0, false, "* optimization", gen_class, NULL, NULL, NULL, NULL, NULL, NULL);
  env_wave_train_1 = clm_make_function(sc, "*", g_env_wave_train_1, 1, 0, false, "* optimization", gen_class, NULL, NULL, NULL, NULL, NULL, NULL);
  env_comb_2 = clm_make_function(sc, "*", g_env_comb_2, 2, 0, false, "* optimization", gen_class, NULL, NULL, NULL, NULL, NULL, NULL);
  env_notch_2 = clm_make_function(sc, "*", g_env_notch_2, 2, 0, false, "* optimization", gen_class, NULL, NULL, NULL, NULL, NULL, NULL);
  env_all_pass_2 = clm_make_function(sc, "*", g_env_all_pass_2, 2, 0, false, "* optimization", gen_class, NULL, NULL, NULL, NULL, NULL, NULL);
  env_delay_2 = clm_make_function(sc, "*", g_env_delay_2, 2, 0, false, "* optimization", gen_class, NULL, NULL, NULL, NULL, NULL, NULL);
  env_moving_average_2 = clm_make_function(sc, "*", g_env_moving_average_2, 2, 0, false, "* optimization", gen_class, NULL, NULL, NULL, NULL, NULL, NULL);
  env_rand_2 = clm_make_function(sc, "*", g_env_rand_2, 2, 0, false, "* optimization", gen_class, NULL, NULL, NULL, NULL, NULL, NULL);
  env_rand_1 = clm_make_function(sc, "*", g_env_rand_1, 1, 0, false, "* optimization", gen_class, NULL, NULL, NULL, NULL, NULL, NULL);
  env_rand_interp_2 = clm_make_function(sc, "*", g_env_rand_interp_2, 2, 0, false, "* optimization", gen_class,	NULL, NULL, NULL, NULL, NULL, NULL);
  env_rand_interp_1 = clm_make_function(sc, "*", g_env_rand_interp_1, 1, 0, false, "* optimization", gen_class,	NULL, NULL, NULL, NULL, NULL, NULL);
  env_ncos_2 = clm_make_function(sc, "*", g_env_ncos_2, 2, 0, false, "* optimization", gen_class, NULL, NULL, NULL, NULL, NULL, NULL);
  env_ncos_1 = clm_make_function(sc, "*", g_env_ncos_1, 1, 0, false, "* optimization", gen_class, NULL, NULL, NULL, NULL, NULL, NULL);
  env_nsin_2 = clm_make_function(sc, "*", g_env_nsin_2, 2, 0, false, "* optimization", gen_class, NULL, NULL, NULL, NULL, NULL, NULL);
  env_nsin_1 = clm_make_function(sc, "*", g_env_nsin_1, 1, 0, false, "* optimization", gen_class, NULL, NULL, NULL, NULL, NULL, NULL);
  env_sawtooth_wave_2 = clm_make_function(sc, "*", g_env_sawtooth_wave_2, 2, 0, false, "* optimization", gen_class, NULL, NULL, NULL, NULL, NULL, NULL);
  env_sawtooth_wave_1 = clm_make_function(sc, "*", g_env_sawtooth_wave_1, 1, 0, false, "* optimization", gen_class, NULL, NULL, NULL, NULL, NULL, NULL);
  env_pulse_train_2 = clm_make_function(sc, "*", g_env_pulse_train_2, 2, 0, false, "* optimization", gen_class,	NULL, NULL, NULL, NULL, NULL, NULL);
  env_pulse_train_1 = clm_make_function(sc, "*", g_env_pulse_train_1, 1, 0, false, "* optimization", gen_class,	NULL, NULL, NULL, NULL, NULL, NULL);
  env_square_wave_2 = clm_make_function(sc, "*", g_env_square_wave_2, 2, 0, false, "* optimization", gen_class,	NULL, NULL, NULL, NULL, NULL, NULL);
  env_square_wave_1 = clm_make_function(sc, "*", g_env_square_wave_1, 1, 0, false, "* optimization", gen_class,	NULL, NULL, NULL, NULL, NULL, NULL);
  env_triangle_wave_2 = clm_make_function(sc, "*", g_env_triangle_wave_2, 2, 0, false, "* optimization", gen_class, NULL, NULL, NULL, NULL, NULL, NULL);
  env_triangle_wave_1 = clm_make_function(sc, "*", g_env_triangle_wave_1, 1, 0, false, "* optimization", gen_class, NULL, NULL, NULL, NULL, NULL, NULL);
  env_nrxysin_2 = clm_make_function(sc, "*", g_env_nrxysin_2, 2, 0, false, "* optimization", gen_class, NULL, NULL, NULL, NULL, NULL, NULL);
  env_nrxysin_1 = clm_make_function(sc, "*", g_env_nrxysin_1, 1, 0, false, "* optimization", gen_class, NULL, NULL, NULL, NULL, NULL, NULL);
  env_nrxycos_2 = clm_make_function(sc, "*", g_env_nrxycos_2, 2, 0, false, "* optimization", gen_class,	NULL, NULL, NULL, NULL, NULL, NULL);
  env_nrxycos_1 = clm_make_function(sc, "*", g_env_nrxycos_1, 1, 0, false, "* optimization", gen_class, NULL, NULL, NULL, NULL, NULL, NULL);
  env_one_zero_2 = clm_make_function(sc, "*", g_env_one_zero_2, 2, 0, false, "* optimization", gen_class, NULL, NULL, NULL, NULL, NULL, NULL);
  env_one_pole_2 = clm_make_function(sc, "*", g_env_one_pole_2, 2, 0, false, "* optimization", gen_class, NULL, NULL, NULL, NULL, NULL, NULL);
  env_two_zero_2 = clm_make_function(sc, "*", g_env_two_zero_2, 2, 0, false, "* optimization", gen_class, NULL, NULL, NULL, NULL, NULL, NULL);
  env_two_pole_2 = clm_make_function(sc, "*", g_env_two_pole_2, 2, 0, false, "* optimization", gen_class, NULL, NULL, NULL, NULL, NULL, NULL);
  env_filter_2 = clm_make_function(sc, "*", g_env_filter_2, 2, 0, false, "* optimization", gen_class, NULL, NULL, NULL, NULL, NULL, NULL);
  env_fir_filter_2 = clm_make_function(sc, "*", g_env_fir_filter_2, 2, 0, false, "* optimization", gen_class, NULL, NULL, NULL, NULL, NULL, NULL);
  env_iir_filter_2 = clm_make_function(sc, "*", g_env_iir_filter_2, 2, 0, false, "* optimization", gen_class, NULL, NULL, NULL, NULL, NULL, NULL);
  env_formant_2 = clm_make_function(sc, "*", g_env_formant_2, 2, 0, false, "* optimization", gen_class,	NULL, NULL, NULL, NULL, NULL, NULL);
  env_firmant_2 = clm_make_function(sc, "*", g_env_firmant_2, 2, 0, false, "* optimization", gen_class, NULL, NULL, NULL, NULL, NULL, NULL);
  env_polyshape_2 = clm_make_function(sc, "*", g_env_polyshape_2, 2, 0, false, "* optimization", gen_class, NULL, NULL, NULL, NULL, NULL, NULL);
  env_ssb_am_2 = clm_make_function(sc, "*", g_env_ssb_am_2, 2, 0, false, "* optimization", gen_class, NULL, NULL, NULL, NULL, NULL, NULL);
  env_asymmetric_fm_2 = clm_make_function(sc, "*", g_env_asymmetric_fm_2, 2, 0, false, "* optimization", gen_class, NULL, NULL, NULL, NULL, NULL, NULL);
  env_filtered_comb_2 = clm_make_function(sc, "*", g_env_filtered_comb_2, 2, 0, false, "* optimization", gen_class, NULL, NULL, NULL, NULL, NULL, NULL);


  f = s7_name_to_value(sc, "+");
  initial_add_chooser = s7_function_chooser(sc, f);
  s7_function_set_chooser(sc, f, clm_add_chooser);
  gen_class = s7_function_class(sc, f);

  fm_violin_vibrato = clm_make_function(sc, "+", g_fm_violin_vibrato, 3, 0, false, "fm-violin optimization", gen_class,
					NULL, NULL, NULL, NULL, NULL, NULL);
  fm_violin_modulation = clm_make_function(sc, "+", g_fm_violin_modulation, 2, 0, false, "fm-violin optimization", gen_class,
					   NULL, NULL, NULL, NULL, NULL, NULL);
  jc_reverb_combs = clm_make_function(sc, "+", g_jc_reverb_combs, 2, 0, false, "jc-reverb optimization", gen_class,
				      NULL, NULL, NULL, NULL, NULL, NULL);
  nrev_combs = clm_make_function(sc, "+", g_nrev_combs, 2, 0, false, "nrev optimization", gen_class,
				 NULL, NULL, NULL, NULL, NULL, NULL);
  add_direct_2 = clm_make_function(sc, "+", g_add_direct_2, 2, 0, false, "+ optimization", gen_class,
				   NULL, NULL, NULL, NULL, NULL, NULL);
  add_direct_3 = clm_make_function(sc, "+", g_add_direct_3, 3, 0, false, "+ optimization", gen_class,
				   NULL, NULL, NULL, NULL, NULL, NULL);
  add_c_direct = clm_make_function(sc, "+", g_add_c_direct, 2, 0, false, "+ optimization", gen_class,
				   NULL, NULL, NULL, NULL, NULL, NULL);
  add_s_direct = clm_make_function_no_choice(sc, "+", g_add_s_direct, 2, 0, false, "+ optimization", gen_class);
  add_1s_direct = clm_make_function_no_choice(sc, "+", g_add_1s_direct, 2, 0, false, "+ optimization", gen_class);
  add_cs_direct = clm_make_function_no_choice(sc, "+", g_add_cs_direct, 2, 0, false, "+ optimization", gen_class);


  f = s7_name_to_value(sc, "abs");
  initial_abs_chooser = s7_function_chooser(sc, f);
  s7_function_set_chooser(sc, f, clm_abs_chooser);
  gen_class = s7_function_class(sc, f);

  abs_rand_interp = clm_make_function(sc, "abs", g_abs_rand_interp, 1, 0, false, "abs optimization", gen_class,
				      NULL, NULL, NULL, NULL, NULL, NULL);
  abs_oscil = clm_make_function(sc, "abs", g_abs_oscil, 1, 0, false, "abs optimization", gen_class,
				      NULL, NULL, NULL, NULL, NULL, NULL);
  abs_triangle_wave = clm_make_function(sc, "abs", g_abs_triangle_wave, 1, 0, false, "abs optimization", gen_class,
				      NULL, NULL, NULL, NULL, NULL, NULL);



  /* oscil */
  f = s7_name_to_value(sc, "oscil");
  s7_function_set_chooser(sc, f, oscil_chooser);
  gen_class = s7_function_class(sc, f);

  oscil_1 = clm_make_function(sc, "oscil", g_oscil_1, 1, 0, false, "oscil optimization", gen_class, 
			      NULL, NULL, NULL, mul_c_oscil_1, mul_s_oscil_1, env_oscil_1);
  oscil_2 = clm_make_function(sc, "oscil", g_oscil_2, 2, 0, false, "oscil optimization", gen_class, 
			      mul_c_oscil_2, mul_s_oscil_2, env_oscil_2, NULL, NULL, NULL);

  direct_oscil_2 = clm_make_function(sc, "oscil", g_direct_oscil_2, 2, 0, false, "oscil optimization", gen_class, 
				     NULL, NULL, NULL, NULL, NULL, NULL);
  indirect_oscil_2 = clm_make_function(sc, "oscil", g_indirect_oscil_2, 2, 0, false, "oscil optimization", gen_class, 
				     NULL, NULL, NULL, NULL, NULL, NULL);
  oscil_pm_direct = clm_make_function(sc, "oscil", g_oscil_pm_direct, 3, 0, false, "oscil optimization", gen_class, 
				     NULL, NULL, NULL, NULL, NULL, NULL);
  fm_violin_with_modulation = clm_make_function(sc, "oscil", g_fm_violin_with_modulation, 2, 0, false, "fm-violin optimization", gen_class, 
						NULL, NULL, NULL, NULL, NULL, NULL);
  oscil_mul_c_s = clm_make_function(sc, "oscil", g_oscil_mul_c_s, 2, 0, false, "oscil optimization", gen_class, 
				    NULL, NULL, NULL, NULL, NULL, NULL);
  oscil_mul_s_c = clm_make_function(sc, "oscil", g_oscil_mul_s_c, 2, 0, false, "oscil optimization", gen_class, 
				    NULL, NULL, NULL, NULL, NULL, NULL);


  /* polywave */
  f = s7_name_to_value(sc, "polywave");
  s7_function_set_chooser(sc, f, polywave_chooser);
  gen_class = s7_function_class(sc, f);

  polywave_1 = clm_make_function(sc, "polywave", g_polywave_1, 1, 0, false, "polywave optimization", gen_class,
				 NULL, NULL, NULL, mul_c_polywave_1, mul_s_polywave_1, env_polywave_1);
  polywave_2 = clm_make_function(sc, "polywave", g_polywave_2, 2, 0, false, "polywave optimization", gen_class,
				 mul_c_polywave_2, mul_s_polywave_2, env_polywave_2, NULL, NULL, NULL);

  direct_polywave_2 = clm_make_function(sc, "polywave", g_direct_polywave_2, 2, 0, false, "polywave optimization", gen_class,
					NULL, NULL, NULL, NULL, NULL, NULL);
  indirect_polywave_2 = clm_make_function(sc, "polywave", g_indirect_polywave_2, 2, 0, false, "polywave optimization", gen_class,
					NULL, NULL, NULL, NULL, NULL, NULL);
  polywave_mul_c_s = clm_make_function(sc, "polywave", g_polywave_mul_c_s, 2, 0, false, "polywave optimization", gen_class,
				       NULL, NULL, NULL, NULL, NULL, NULL);


  /* table-lookup */
  f = s7_name_to_value(sc, "table-lookup");
  s7_function_set_chooser(sc, f, table_lookup_chooser);
  gen_class = s7_function_class(sc, f);

  table_lookup_1 = clm_make_function(sc, "table-lookup", g_table_lookup_1, 1, 0, false, "table-lookup optimization", gen_class,
				     NULL, NULL, NULL, mul_c_table_lookup_1, mul_s_table_lookup_1, env_table_lookup_1);
  table_lookup_2 = clm_make_function(sc, "table-lookup", g_table_lookup_2, 2, 0, false, "table-lookup optimization", gen_class,
				     mul_c_table_lookup_2, mul_s_table_lookup_2, env_table_lookup_2, NULL, NULL, NULL);
  direct_table_lookup_2 = clm_make_function(sc, "table-lookup", g_direct_table_lookup_2, 2, 0, false, "table-lookup optimization", gen_class,
					    NULL, NULL, NULL, NULL, NULL, NULL);
  indirect_table_lookup_2 = clm_make_function(sc, "table-lookup", g_indirect_table_lookup_2, 2, 0, false, "table-lookup optimization", gen_class,
					    NULL, NULL, NULL, NULL, NULL, NULL);


  /* wave-train */
  f = s7_name_to_value(sc, "wave-train");
  s7_function_set_chooser(sc, f, wave_train_chooser);
  gen_class = s7_function_class(sc, f);

  wave_train_1 = clm_make_function(sc, "wave-train", g_wave_train_1, 1, 0, false, "wave-train optimization", gen_class,
				   NULL, NULL, NULL, mul_c_wave_train_1, mul_s_wave_train_1, env_wave_train_1);
  wave_train_2 = clm_make_function(sc, "wave-train", g_wave_train_2, 2, 0, false, "wave-train optimization", gen_class,
				   mul_c_wave_train_2, mul_s_wave_train_2, env_wave_train_2, NULL, NULL, NULL);
  direct_wave_train_2 = clm_make_function(sc, "wave-train", g_direct_wave_train_2, 2, 0, false, "wave-train optimization", gen_class,
					  NULL, NULL, NULL, NULL, NULL, NULL);
  indirect_wave_train_2 = clm_make_function(sc, "wave-train", g_indirect_wave_train_2, 2, 0, false, "wave-train optimization", gen_class,
					  NULL, NULL, NULL, NULL, NULL, NULL);


  f = s7_name_to_value(sc, "nsin");
  s7_function_set_chooser(sc, f, nsin_chooser);
  gen_class = s7_function_class(sc, f);

  nsin_2 = clm_make_function(sc, "nsin", g_nsin_2, 2, 0, false, "nsin optimization", gen_class,
			     mul_c_nsin_2, mul_s_nsin_2, env_nsin_2, NULL, NULL, NULL);
  nsin_1 = clm_make_function(sc, "nsin", g_nsin_1, 1, 0, false, "nsin optimization", gen_class,
			     NULL, NULL, NULL, mul_c_nsin_1, mul_s_nsin_1, env_nsin_1);			     
  direct_nsin_2 = clm_make_function(sc, "nsin", g_direct_nsin_2, 2, 0, false, "nsin optimization", gen_class,
				    NULL, NULL, NULL, NULL, NULL, NULL);
  indirect_nsin_2 = clm_make_function(sc, "nsin", g_indirect_nsin_2, 2, 0, false, "nsin optimization", gen_class,
				    NULL, NULL, NULL, NULL, NULL, NULL);


  f = s7_name_to_value(sc, "ncos");
  s7_function_set_chooser(sc, f, ncos_chooser);
  gen_class = s7_function_class(sc, f);

  ncos_2 = clm_make_function(sc, "ncos", g_ncos_2, 2, 0, false, "ncos optimization", gen_class,
			     mul_c_ncos_2, mul_s_ncos_2, env_ncos_2, NULL, NULL, NULL);
  direct_ncos_2 = clm_make_function(sc, "ncos", g_direct_ncos_2, 2, 0, false, "ncos optimization", gen_class,
				    NULL, NULL, NULL, NULL, NULL, NULL);
  indirect_ncos_2 = clm_make_function(sc, "ncos", g_indirect_ncos_2, 2, 0, false, "ncos optimization", gen_class,
				    NULL, NULL, NULL, NULL, NULL, NULL);
  ncos_1 = clm_make_function(sc, "ncos", g_ncos_1, 1, 0, false, "ncos optimization", gen_class,
			     NULL, NULL, NULL, mul_c_ncos_1, mul_s_ncos_1, env_ncos_1);


  f = s7_name_to_value(sc, "nrxysin");
  s7_function_set_chooser(sc, f, nrxysin_chooser);
  gen_class = s7_function_class(sc, f);

  nrxysin_2 = clm_make_function(sc, "nrxysin", g_nrxysin_2, 2, 0, false, "nrxysin optimization", gen_class,
				mul_c_nrxysin_2, mul_s_nrxysin_2, env_nrxysin_2, NULL, NULL, NULL);
  direct_nrxysin_2 = clm_make_function(sc, "nrxysin", g_direct_nrxysin_2, 2, 0, false, "nrxysin optimization", gen_class,
				       NULL, NULL, NULL, NULL, NULL, NULL);
  indirect_nrxysin_2 = clm_make_function(sc, "nrxysin", g_indirect_nrxysin_2, 2, 0, false, "nrxysin optimization", gen_class,
				       NULL, NULL, NULL, NULL, NULL, NULL);
  nrxysin_1 = clm_make_function(sc, "nrxysin", g_nrxysin_1, 1, 0, false, "nrxysin optimization", gen_class,
				NULL, NULL, NULL, mul_c_nrxysin_1, mul_s_nrxysin_1, env_nrxysin_1);


  f = s7_name_to_value(sc, "nrxycos");
  s7_function_set_chooser(sc, f, nrxycos_chooser);
  gen_class = s7_function_class(sc, f);

  nrxycos_2 = clm_make_function(sc, "nrxycos", g_nrxycos_2, 2, 0, false, "nrxycos optimization", gen_class,
				mul_c_nrxycos_2, mul_s_nrxycos_2, env_nrxycos_2, NULL, NULL, NULL);
  direct_nrxycos_2 = clm_make_function(sc, "nrxycos", g_direct_nrxycos_2, 2, 0, false, "nrxycos optimization", gen_class,
				       NULL, NULL, NULL, NULL, NULL, NULL);
  indirect_nrxycos_2 = clm_make_function(sc, "nrxycos", g_indirect_nrxycos_2, 2, 0, false, "nrxycos optimization", gen_class,
				       NULL, NULL, NULL, NULL, NULL, NULL);
  nrxycos_1 = clm_make_function(sc, "nrxycos", g_nrxycos_1, 1, 0, false, "nrxycos optimization", gen_class,
				NULL, NULL, NULL, mul_c_nrxycos_1, mul_s_nrxycos_1, env_nrxycos_1);


  f = s7_name_to_value(sc, "env");
  s7_function_set_chooser(sc, f, env_chooser);
  gen_class = s7_function_class(sc, f);

  env_1 = clm_make_function(sc, "env", g_env_1, 1, 0, false, "env optimization", gen_class,
			    NULL, NULL, NULL, mul_c_env_1, mul_s_env_1, env_env_1);


  f = s7_name_to_value(sc, "readin");
  s7_function_set_chooser(sc, f, readin_chooser);
  gen_class = s7_function_class(sc, f);

  readin_1 = clm_make_function(sc, "readin", g_readin_1, 1, 0, false, "readin optimization", gen_class,
			       NULL, NULL, NULL, mul_c_readin_1, mul_s_readin_1, env_readin_1);


  f = s7_name_to_value(sc, "comb");
  s7_function_set_chooser(sc, f, comb_chooser);
  gen_class = s7_function_class(sc, f);

  comb_2 = clm_make_function(sc, "comb", g_comb_2, 2, 0, false, "comb optimization", gen_class,
			     mul_c_comb_2, mul_s_comb_2, env_comb_2, NULL, NULL, NULL);
  direct_comb_2 = clm_make_function(sc, "comb", g_direct_comb_2, 2, 0, false, "comb optimization", gen_class,
				    NULL, NULL, NULL, NULL, NULL, NULL);
  indirect_comb_2 = clm_make_function(sc, "comb", g_indirect_comb_2, 2, 0, false, "comb optimization", gen_class,
				    NULL, NULL, NULL, NULL, NULL, NULL);

  f = s7_name_to_value(sc, "notch");
  s7_function_set_chooser(sc, f, notch_chooser);
  gen_class = s7_function_class(sc, f);

  notch_2 = clm_make_function(sc, "notch", g_notch_2, 2, 0, false, "notch optimization", gen_class,
			      mul_c_notch_2, mul_s_notch_2, env_notch_2, NULL, NULL, NULL);
  direct_notch_2 = clm_make_function(sc, "notch", g_direct_notch_2, 2, 0, false, "notch optimization", gen_class,
				     NULL, NULL, NULL, NULL, NULL, NULL);
  indirect_notch_2 = clm_make_function(sc, "notch", g_indirect_notch_2, 2, 0, false, "notch optimization", gen_class,
				     NULL, NULL, NULL, NULL, NULL, NULL);


  f = s7_name_to_value(sc, "one-pole");
  s7_function_set_chooser(sc, f, one_pole_chooser);
  gen_class = s7_function_class(sc, f);

  one_pole_2 = clm_make_function(sc, "one-pole", g_one_pole_2, 2, 0, false, "one-pole optimization", gen_class,
				 mul_c_one_pole_2, mul_s_one_pole_2, env_one_pole_2, NULL, NULL, NULL);
  direct_one_pole_2 = clm_make_function(sc, "one-pole", g_direct_one_pole_2, 2, 0, false, "one-pole optimization", gen_class,
					NULL, NULL, NULL, NULL, NULL, NULL);
  indirect_one_pole_2 = clm_make_function(sc, "one-pole", g_indirect_one_pole_2, 2, 0, false, "one-pole optimization", gen_class,
					NULL, NULL, NULL, NULL, NULL, NULL);


  f = s7_name_to_value(sc, "two-pole");
  s7_function_set_chooser(sc, f, two_pole_chooser);
  gen_class = s7_function_class(sc, f);

  two_pole_2 = clm_make_function(sc, "two-pole", g_two_pole_2, 2, 0, false, "two-pole optimization", gen_class,
				 mul_c_two_pole_2, mul_s_two_pole_2, env_two_pole_2, NULL, NULL, NULL);
  direct_two_pole_2 = clm_make_function(sc, "two-pole", g_direct_two_pole_2, 2, 0, false, "two-pole optimization", gen_class,
					NULL, NULL, NULL, NULL, NULL, NULL);
  indirect_two_pole_2 = clm_make_function(sc, "two-pole", g_indirect_two_pole_2, 2, 0, false, "two-pole optimization", gen_class,
					NULL, NULL, NULL, NULL, NULL, NULL);


  f = s7_name_to_value(sc, "one-zero");
  s7_function_set_chooser(sc, f, one_zero_chooser);
  gen_class = s7_function_class(sc, f);

  one_zero_2 = clm_make_function(sc, "one-zero", g_one_zero_2, 2, 0, false, "one-zero optimization", gen_class,
				 mul_c_one_zero_2, mul_s_one_zero_2, env_one_zero_2, NULL, NULL, NULL);
  direct_one_zero_2 = clm_make_function(sc, "one-zero", g_direct_one_zero_2, 2, 0, false, "one-zero optimization", gen_class,
					NULL, NULL, NULL, NULL, NULL, NULL);
  indirect_one_zero_2 = clm_make_function(sc, "one-zero", g_indirect_one_zero_2, 2, 0, false, "one-zero optimization", gen_class,
					NULL, NULL, NULL, NULL, NULL, NULL);


  f = s7_name_to_value(sc, "two-zero");
  s7_function_set_chooser(sc, f, two_zero_chooser);
  gen_class = s7_function_class(sc, f);

  two_zero_2 = clm_make_function(sc, "two-zero", g_two_zero_2, 2, 0, false, "two-zero optimization", gen_class,
				 mul_c_two_zero_2, mul_s_two_zero_2, env_two_zero_2, NULL, NULL, NULL);
  direct_two_zero_2 = clm_make_function(sc, "two-zero", g_direct_two_zero_2, 2, 0, false, "two-zero optimization", gen_class,
					NULL, NULL, NULL, NULL, NULL, NULL);
  indirect_two_zero_2 = clm_make_function(sc, "two-zero", g_indirect_two_zero_2, 2, 0, false, "two-zero optimization", gen_class,
					NULL, NULL, NULL, NULL, NULL, NULL);

  f = s7_name_to_value(sc, "moving-average");
  s7_function_set_chooser(sc, f, moving_average_chooser);
  gen_class = s7_function_class(sc, f);

  moving_average_2 = clm_make_function(sc, "moving-average", g_moving_average_2, 2, 0, false, "moving-average optimization", gen_class,
				       mul_c_moving_average_2, mul_s_moving_average_2, env_moving_average_2, NULL, NULL, NULL);
  direct_moving_average_2 = clm_make_function(sc, "moving-average", g_direct_moving_average_2, 2, 0, false, "moving-average optimization", gen_class,
					      NULL, NULL, NULL, NULL, NULL, NULL);
  indirect_moving_average_2 = clm_make_function(sc, "moving-average", g_indirect_moving_average_2, 2, 0, false, "moving-average optimization", gen_class,
					      NULL, NULL, NULL, NULL, NULL, NULL);

  f = s7_name_to_value(sc, "filter");
  s7_function_set_chooser(sc, f, filter_chooser);
  gen_class = s7_function_class(sc, f);

  filter_2 = clm_make_function(sc, "filter", g_filter_2, 2, 0, false, "filter optimization", gen_class,
			       mul_c_filter_2, mul_s_filter_2, env_filter_2, NULL, NULL, NULL);
  direct_filter_2 = clm_make_function(sc, "filter", g_direct_filter_2, 2, 0, false, "filter optimization", gen_class,
				      NULL, NULL, NULL, NULL, NULL, NULL);
  indirect_filter_2 = clm_make_function(sc, "filter", g_indirect_filter_2, 2, 0, false, "filter optimization", gen_class,
				      NULL, NULL, NULL, NULL, NULL, NULL);

  f = s7_name_to_value(sc, "fir-filter");
  s7_function_set_chooser(sc, f, fir_filter_chooser);
  gen_class = s7_function_class(sc, f);

  fir_filter_2 = clm_make_function(sc, "fir-filter", g_fir_filter_2, 2, 0, false, "fir-filter optimization", gen_class,
				   mul_c_fir_filter_2, mul_s_fir_filter_2, env_fir_filter_2, NULL, NULL, NULL);
  direct_fir_filter_2 = clm_make_function(sc, "fir-filter", g_direct_fir_filter_2, 2, 0, false, "fir-filter optimization", gen_class,
					  NULL, NULL, NULL, NULL, NULL, NULL);
  indirect_fir_filter_2 = clm_make_function(sc, "fir-filter", g_indirect_fir_filter_2, 2, 0, false, "fir-filter optimization", gen_class,
					  NULL, NULL, NULL, NULL, NULL, NULL);

  f = s7_name_to_value(sc, "iir-filter");
  s7_function_set_chooser(sc, f, iir_filter_chooser);
  gen_class = s7_function_class(sc, f);

  iir_filter_2 = clm_make_function(sc, "iir-filter", g_iir_filter_2, 2, 0, false, "iir-filter optimization", gen_class,
				   mul_c_iir_filter_2, mul_s_iir_filter_2, env_iir_filter_2, NULL, NULL, NULL);
  direct_iir_filter_2 = clm_make_function(sc, "iir-filter", g_direct_iir_filter_2, 2, 0, false, "iir-filter optimization", gen_class,
					  NULL, NULL, NULL, NULL, NULL, NULL);
  indirect_iir_filter_2 = clm_make_function(sc, "iir-filter", g_indirect_iir_filter_2, 2, 0, false, "iir-filter optimization", gen_class,
					  NULL, NULL, NULL, NULL, NULL, NULL);

  f = s7_name_to_value(sc, "triangle-wave");
  s7_function_set_chooser(sc, f, triangle_wave_chooser);
  gen_class = s7_function_class(sc, f);

  triangle_wave_2 = clm_make_function(sc, "triangle-wave", g_triangle_wave_2, 2, 0, false, "triangle-wave optimization", gen_class,
				      mul_c_triangle_wave_2, mul_s_triangle_wave_2, env_triangle_wave_2, NULL, NULL, NULL);
  direct_triangle_wave_2 = clm_make_function(sc, "triangle-wave", g_direct_triangle_wave_2, 2, 0, false, "triangle-wave optimization", gen_class,
					     NULL, NULL, NULL, NULL, NULL, NULL);
  indirect_triangle_wave_2 = clm_make_function(sc, "triangle-wave", g_indirect_triangle_wave_2, 2, 0, false, "triangle-wave optimization", gen_class,
					     NULL, NULL, NULL, NULL, NULL, NULL);
  triangle_wave_1 = clm_make_function(sc, "triangle-wave", g_triangle_wave_1, 1, 0, false, "triangle-wave optimization", gen_class,
				      NULL, NULL, NULL, mul_c_triangle_wave_1, mul_s_triangle_wave_1, env_triangle_wave_1);

  f = s7_name_to_value(sc, "sawtooth-wave");
  s7_function_set_chooser(sc, f, sawtooth_wave_chooser);
  gen_class = s7_function_class(sc, f);

  sawtooth_wave_2 = clm_make_function(sc, "sawtooth-wave", g_sawtooth_wave_2, 2, 0, false, "sawtooth-wave optimization", gen_class,
				      mul_c_sawtooth_wave_2, mul_s_sawtooth_wave_2, env_sawtooth_wave_2, NULL, NULL, NULL);
  direct_sawtooth_wave_2 = clm_make_function(sc, "sawtooth-wave", g_direct_sawtooth_wave_2, 2, 0, false, "sawtooth-wave optimization", gen_class,
					     NULL, NULL, NULL, NULL, NULL, NULL);
  indirect_sawtooth_wave_2 = clm_make_function(sc, "sawtooth-wave", g_indirect_sawtooth_wave_2, 2, 0, false, "sawtooth-wave optimization", gen_class,
					     NULL, NULL, NULL, NULL, NULL, NULL);
  sawtooth_wave_1 = clm_make_function(sc, "sawtooth-wave", g_sawtooth_wave_1, 1, 0, false, "sawtooth-wave optimization", gen_class,
				      NULL, NULL, NULL, mul_c_sawtooth_wave_1, mul_s_sawtooth_wave_1, env_sawtooth_wave_1);

  f = s7_name_to_value(sc, "square-wave");
  s7_function_set_chooser(sc, f, square_wave_chooser);
  gen_class = s7_function_class(sc, f);

  square_wave_2 = clm_make_function(sc, "square-wave", g_square_wave_2, 2, 0, false, "square-wave optimization", gen_class,
				    mul_c_square_wave_2, mul_s_square_wave_2, env_square_wave_2, NULL, NULL, NULL);
  direct_square_wave_2 = clm_make_function(sc, "square-wave", g_direct_square_wave_2, 2, 0, false, "square-wave optimization", gen_class,
					   NULL, NULL, NULL, NULL, NULL, NULL);
  indirect_square_wave_2 = clm_make_function(sc, "square-wave", g_indirect_square_wave_2, 2, 0, false, "square-wave optimization", gen_class,
					   NULL, NULL, NULL, NULL, NULL, NULL);
  square_wave_1 = clm_make_function(sc, "square-wave", g_square_wave_1, 1, 0, false, "square-wave optimization", gen_class,
				    NULL, NULL, NULL, mul_c_square_wave_1, mul_s_square_wave_1, env_square_wave_1);

  f = s7_name_to_value(sc, "pulse-train");
  s7_function_set_chooser(sc, f, pulse_train_chooser);
  gen_class = s7_function_class(sc, f);

  pulse_train_2 = clm_make_function(sc, "pulse-train", g_pulse_train_2, 2, 0, false, "pulse-train optimization", gen_class,
				    mul_c_pulse_train_2, mul_s_pulse_train_2, env_pulse_train_2, NULL, NULL, NULL);
  direct_pulse_train_2 = clm_make_function(sc, "pulse-train", g_direct_pulse_train_2, 2, 0, false, "pulse-train optimization", gen_class,
					   NULL, NULL, NULL, NULL, NULL, NULL);
  indirect_pulse_train_2 = clm_make_function(sc, "pulse-train", g_indirect_pulse_train_2, 2, 0, false, "pulse-train optimization", gen_class,
					   NULL, NULL, NULL, NULL, NULL, NULL);
  pulse_train_1 = clm_make_function(sc, "pulse-train", g_pulse_train_1, 1, 0, false, "pulse-train optimization", gen_class,
				    NULL, NULL, NULL, mul_c_pulse_train_1, mul_s_pulse_train_1, env_pulse_train_1);


  f = s7_name_to_value(sc, "rand");
  s7_function_set_chooser(sc, f, rand_chooser);
  gen_class = s7_function_class(sc, f);

  rand_1 = clm_make_function(sc, "rand", g_rand_1, 1, 0, false, "rand optimization", gen_class,
			     NULL, NULL, NULL, mul_c_rand_1, mul_s_rand_1, env_rand_1);
  rand_2 = clm_make_function(sc, "rand", g_rand_2, 2, 0, false, "rand optimization", gen_class,
			     mul_c_rand_2, mul_s_rand_2, env_rand_2, NULL, NULL, NULL);
  direct_rand_2 = clm_make_function(sc, "rand", g_direct_rand_2, 2, 0, false, "rand optimization", gen_class,
				    NULL, NULL, NULL, NULL, NULL, NULL);
  indirect_rand_2 = clm_make_function(sc, "rand", g_indirect_rand_2, 2, 0, false, "rand optimization", gen_class,
				    NULL, NULL, NULL, NULL, NULL, NULL);

  f = s7_name_to_value(sc, "rand-interp");
  s7_function_set_chooser(sc, f, rand_interp_chooser);
  gen_class = s7_function_class(sc, f);

  rand_interp_1 = clm_make_function(sc, "rand-interp", g_rand_interp_1, 1, 0, false, "rand-interp optimization", gen_class,
				    NULL, NULL, NULL, mul_c_rand_interp_1, mul_s_rand_interp_1, env_rand_interp_1);
  rand_interp_2 = clm_make_function(sc, "rand-interp", g_rand_interp_2, 2, 0, false, "rand-interp optimization", gen_class,
				    mul_c_rand_interp_2, mul_s_rand_interp_2, env_rand_interp_2, NULL, NULL, NULL);
  direct_rand_interp_2 = clm_make_function(sc, "rand-interp", g_direct_rand_interp_2, 2, 0, false, "rand-interp optimization", gen_class,
					   NULL, NULL, NULL, NULL, NULL, NULL);
  indirect_rand_interp_2 = clm_make_function(sc, "rand-interp", g_indirect_rand_interp_2, 2, 0, false, "rand-interp optimization", gen_class,
					   NULL, NULL, NULL, NULL, NULL, NULL);


  f = s7_name_to_value(sc, "formant");
  s7_function_set_chooser(sc, f, formant_chooser);
  gen_class = s7_function_class(sc, f);

  formant_2 = clm_make_function(sc, "formant", g_formant_2, 2, 0, false, "formant optimization", gen_class,
				mul_c_formant_2, mul_s_formant_2, env_formant_2, NULL, NULL, NULL);
  direct_formant_2 = clm_make_function(sc, "formant", g_direct_formant_2, 2, 0, false, "formant optimization", gen_class,
				       NULL, NULL, NULL, NULL, NULL, NULL);
  indirect_formant_2 = clm_make_function(sc, "formant", g_indirect_formant_2, 2, 0, false, "formant optimization", gen_class,
				       NULL, NULL, NULL, NULL, NULL, NULL);

  f = s7_name_to_value(sc, "firmant");
  s7_function_set_chooser(sc, f, firmant_chooser);
  gen_class = s7_function_class(sc, f);

  firmant_2 = clm_make_function(sc, "firmant", g_firmant_2, 2, 0, false, "firmant optimization", gen_class,
				mul_c_firmant_2, mul_s_firmant_2, env_firmant_2, NULL, NULL, NULL);
  direct_firmant_2 = clm_make_function(sc, "firmant", g_direct_firmant_2, 2, 0, false, "firmant optimization", gen_class,
				       NULL, NULL, NULL, NULL, NULL, NULL);
  indirect_firmant_2 = clm_make_function(sc, "firmant", g_indirect_firmant_2, 2, 0, false, "firmant optimization", gen_class,
				       NULL, NULL, NULL, NULL, NULL, NULL);


  f = s7_name_to_value(sc, "all-pass");
  s7_function_set_chooser(sc, f, all_pass_chooser);
  gen_class = s7_function_class(sc, f);

  all_pass_2 = clm_make_function(sc, "all-pass", g_all_pass_2, 2, 0, false, "all-pass optimization", gen_class,
				 mul_c_all_pass_2, mul_s_all_pass_2, env_all_pass_2, NULL, NULL, NULL);
  direct_all_pass_2 = clm_make_function(sc, "all-pass", g_direct_all_pass_2, 2, 0, false, "all-pass optimization", gen_class,
					NULL, NULL, NULL, NULL, NULL, NULL);
  indirect_all_pass_2 = clm_make_function(sc, "all-pass", g_indirect_all_pass_2, 2, 0, false, "all-pass optimization", gen_class,
					NULL, NULL, NULL, NULL, NULL, NULL);
  jc_reverb_all_passes = clm_make_function(sc, "all-pass", g_jc_reverb_all_passes, 2, 0, false, "all-pass optimization", gen_class,
					   NULL, NULL, NULL, NULL, NULL, NULL);


  f = s7_name_to_value(sc, "delay");
  s7_function_set_chooser(sc, f, delay_chooser);
  gen_class = s7_function_class(sc, f);

  delay_2 = clm_make_function(sc, "delay", g_delay_2, 2, 0, false, "delay optimization", gen_class,
			      mul_c_delay_2, mul_s_delay_2, env_delay_2, NULL, NULL, NULL);
  direct_delay_2 = clm_make_function(sc, "delay", g_direct_delay_2, 2, 0, false, "delay optimization", gen_class,
				     NULL, NULL, NULL, NULL, NULL, NULL);
  indirect_delay_2 = clm_make_function(sc, "delay", g_indirect_delay_2, 2, 0, false, "delay optimization", gen_class,
				     NULL, NULL, NULL, NULL, NULL, NULL);


  f = s7_name_to_value(sc, "polyshape");
  s7_function_set_chooser(sc, f, polyshape_chooser);
  gen_class = s7_function_class(sc, f);

  polyshape_2 = clm_make_function(sc, "polyshape", g_polyshape_2, 2, 0, false, "polyshape optimization", gen_class,
				mul_c_polyshape_2, mul_s_polyshape_2, env_polyshape_2, NULL, NULL, NULL);
  direct_polyshape_2 = clm_make_function(sc, "polyshape", g_direct_polyshape_2, 2, 0, false, "polyshape optimization", gen_class,
				       NULL, NULL, NULL, NULL, NULL, NULL);
  indirect_polyshape_2 = clm_make_function(sc, "polyshape", g_indirect_polyshape_2, 2, 0, false, "polyshape optimization", gen_class,
				       NULL, NULL, NULL, NULL, NULL, NULL);

  f = s7_name_to_value(sc, "ssb-am");
  s7_function_set_chooser(sc, f, ssb_am_chooser);
  gen_class = s7_function_class(sc, f);

  ssb_am_2 = clm_make_function(sc, "ssb-am", g_ssb_am_2, 2, 0, false, "ssb-am optimization", gen_class,
				mul_c_ssb_am_2, mul_s_ssb_am_2, env_ssb_am_2, NULL, NULL, NULL);
  direct_ssb_am_2 = clm_make_function(sc, "ssb-am", g_direct_ssb_am_2, 2, 0, false, "ssb-am optimization", gen_class,
				       NULL, NULL, NULL, NULL, NULL, NULL);
  indirect_ssb_am_2 = clm_make_function(sc, "ssb-am", g_indirect_ssb_am_2, 2, 0, false, "ssb-am optimization", gen_class,
				       NULL, NULL, NULL, NULL, NULL, NULL);

  f = s7_name_to_value(sc, "asymmetric-fm");
  s7_function_set_chooser(sc, f, asymmetric_fm_chooser);
  gen_class = s7_function_class(sc, f);

  asymmetric_fm_2 = clm_make_function(sc, "asymmetric-fm", g_asymmetric_fm_2, 2, 0, false, "asymmetric-fm optimization", gen_class,
				mul_c_asymmetric_fm_2, mul_s_asymmetric_fm_2, env_asymmetric_fm_2, NULL, NULL, NULL);
  direct_asymmetric_fm_2 = clm_make_function(sc, "asymmetric-fm", g_direct_asymmetric_fm_2, 2, 0, false, "asymmetric-fm optimization", gen_class,
				       NULL, NULL, NULL, NULL, NULL, NULL);
  indirect_asymmetric_fm_2 = clm_make_function(sc, "asymmetric-fm", g_indirect_asymmetric_fm_2, 2, 0, false, "asymmetric-fm optimization", gen_class,
				       NULL, NULL, NULL, NULL, NULL, NULL);

  f = s7_name_to_value(sc, "filtered-comb");
  s7_function_set_chooser(sc, f, filtered_comb_chooser);
  gen_class = s7_function_class(sc, f);

  filtered_comb_2 = clm_make_function(sc, "filtered-comb", g_filtered_comb_2, 2, 0, false, "filtered-comb optimization", gen_class,
				mul_c_filtered_comb_2, mul_s_filtered_comb_2, env_filtered_comb_2, NULL, NULL, NULL);
  direct_filtered_comb_2 = clm_make_function(sc, "filtered-comb", g_direct_filtered_comb_2, 2, 0, false, "filtered-comb optimization", gen_class,
				       NULL, NULL, NULL, NULL, NULL, NULL);
  indirect_filtered_comb_2 = clm_make_function(sc, "filtered-comb", g_indirect_filtered_comb_2, 2, 0, false, "filtered-comb optimization", gen_class,
				       NULL, NULL, NULL, NULL, NULL, NULL);


  f = s7_name_to_value(sc, "frame->frame");
  s7_function_set_chooser(sc, f, frame_to_frame_chooser);
  gen_class = s7_function_class(sc, f);
  frame_to_frame_sss = s7_make_function(sc, "frame->frame", g_frame_to_frame_sss, 3, 0, false, "frame->frame optimization");
  s7_function_set_class(sc, frame_to_frame_sss, gen_class);


  f = s7_name_to_value(sc, "locsig");
  s7_function_set_chooser(sc, f, locsig_chooser);
  gen_class = s7_function_class(sc, f);

  fm_violin_2 = s7_make_function(sc, "locsig", g_fm_violin_2, 3, 0, false, "fm-violin optimization");
  s7_function_set_class(sc, fm_violin_2, gen_class);
  indirect_locsig_3 = s7_make_function(sc, "locsig", g_indirect_locsig_3, 2, 0, false, "locsig optimization");
  s7_function_set_class(sc, indirect_locsig_3, gen_class);


  f = s7_name_to_value(sc, "outa");
  s7_function_set_chooser(sc, f, outa_chooser);
  gen_class = s7_function_class(sc, f);

  outa_mul_s_delay = s7_make_function(sc, "outa", g_outa_mul_s_delay, 2, 0, false, "outa optimization");
  s7_function_set_class(sc, outa_mul_s_delay, gen_class);
  outa_env_polywave_env = s7_make_function(sc, "outa", g_outa_env_polywave_env, 2, 0, false, "outa optimization");
  s7_function_set_class(sc, outa_env_polywave_env, gen_class);
  indirect_outa_2 = s7_make_function(sc, "outa", g_indirect_outa_2, 2, 0, false, "outa optimization");
  s7_function_set_class(sc, indirect_outa_2, gen_class);
  indirect_outa_add_2 = s7_make_function(sc, "outa", g_indirect_outa_add_2, 2, 0, false, "outa optimization");
  s7_function_set_class(sc, indirect_outa_add_2, gen_class);
  indirect_outa_sub_2 = s7_make_function(sc, "outa", g_indirect_outa_sub_2, 2, 0, false, "outa optimization");
  s7_function_set_class(sc, indirect_outa_sub_2, gen_class);
  indirect_outa_ss = s7_make_function(sc, "outa", g_indirect_outa_ss, 2, 0, false, "outa optimization");
  s7_function_set_class(sc, indirect_outa_ss, gen_class);

  f = s7_name_to_value(sc, "ina");
  s7_function_set_chooser(sc, f, ina_chooser);
  gen_class = s7_function_class(sc, f);

  ina_reverb_2 = s7_make_function(sc, "ina", g_ina_reverb_2, 2, 0, false, "ina optimization");
  s7_function_set_class(sc, ina_reverb_2, gen_class);
  mul_s_ina_reverb_2 = s7_make_function(sc, "*", g_mul_s_ina_reverb_2, 2, 0, false, "* optimization");
  s7_function_set_class(sc, mul_s_ina_reverb_2, gen_class);
  s7_function_chooser_set_data(sc, ina_reverb_2, (void *)make_choices(NULL, mul_s_ina_reverb_2, NULL, NULL, NULL, NULL));

  f = s7_name_to_value(sc, "frame->file");
  s7_function_set_chooser(sc, f, frame_to_file_chooser);
  gen_class = s7_function_class(sc, f);

  indirect_frame_to_file_3 = s7_make_function(sc, "frame->file", g_indirect_frame_to_file_3, 3, 0, false, "frame->file optimization");
  s7_function_set_class(sc, indirect_frame_to_file_3, gen_class);

}

#endif
/* -------------------------------------------------------------------------------- */


static void mus_xen_init(void)
{
  mus_initialize();

#if (!HAVE_NESTED_FUNCTIONS) || __cplusplus
  current_connect_func = XEN_FALSE;
#endif

#if HAVE_SCHEME
  mus_xen_tag = XEN_MAKE_OBJECT_TYPE("<generator>", print_mus_xen, free_mus_xen, s7_equalp_mus_xen, mark_mus_xen, 
				     mus_xen_apply, s7_mus_set, s7_mus_length, s7_mus_copy, NULL, s7_mus_fill);
  as_needed_arglist = XEN_LIST_1(XEN_ZERO);
  XEN_PROTECT_FROM_GC(as_needed_arglist);
#else
  mus_xen_tag = XEN_MAKE_OBJECT_TYPE("Mus", sizeof(mus_xen));
#endif

  xen_one = C_TO_XEN_INT(1);
  XEN_PROTECT_FROM_GC(xen_one);
  xen_minus_one = C_TO_XEN_INT(-1);
  XEN_PROTECT_FROM_GC(xen_minus_one);

#if HAVE_FORTH
  fth_set_object_inspect(mus_xen_tag, print_mus_xen);
  fth_set_object_equal(mus_xen_tag, equalp_mus_xen);
  fth_set_object_mark(mus_xen_tag, mark_mus_xen);
  fth_set_object_free(mus_xen_tag, free_mus_xen);
  fth_set_object_apply(mus_xen_tag, XEN_PROCEDURE_CAST mus_xen_apply, 0, 2, 0);
#endif

#if HAVE_RUBY
  rb_define_method(mus_xen_tag, "to_s", XEN_PROCEDURE_CAST mus_xen_to_s, 0);
  rb_define_method(mus_xen_tag, "eql?", XEN_PROCEDURE_CAST equalp_mus_xen, 1);
  rb_define_method(mus_xen_tag, "frequency", XEN_PROCEDURE_CAST g_mus_frequency, 0);
  rb_define_method(mus_xen_tag, "frequency=", XEN_PROCEDURE_CAST g_mus_set_frequency, 1);
  rb_define_method(mus_xen_tag, "phase", XEN_PROCEDURE_CAST g_mus_phase, 0);
  rb_define_method(mus_xen_tag, "phase=", XEN_PROCEDURE_CAST g_mus_set_phase, 1);
  rb_define_method(mus_xen_tag, "scaler", XEN_PROCEDURE_CAST g_mus_scaler, 0);
  rb_define_method(mus_xen_tag, "scaler=", XEN_PROCEDURE_CAST g_mus_set_scaler, 1);
  rb_define_method(mus_xen_tag, "width", XEN_PROCEDURE_CAST g_mus_width, 0);
  rb_define_method(mus_xen_tag, "width=", XEN_PROCEDURE_CAST g_mus_set_width, 1);
  rb_define_method(mus_xen_tag, "offset", XEN_PROCEDURE_CAST g_mus_offset, 0);
  rb_define_method(mus_xen_tag, "offset=", XEN_PROCEDURE_CAST g_mus_set_offset, 1);
  rb_define_method(mus_xen_tag, "reset", XEN_PROCEDURE_CAST g_mus_reset, 0);
  rb_define_method(mus_xen_tag, "length", XEN_PROCEDURE_CAST g_mus_length, 0);
  rb_define_method(mus_xen_tag, "length=", XEN_PROCEDURE_CAST g_mus_set_length, 1);
  rb_define_method(mus_xen_tag, "data", XEN_PROCEDURE_CAST g_mus_data, 0);
  rb_define_method(mus_xen_tag, "data=", XEN_PROCEDURE_CAST g_mus_set_data, 1);
  rb_define_method(mus_xen_tag, "feedforward", XEN_PROCEDURE_CAST g_mus_feedforward, 0);
  rb_define_method(mus_xen_tag, "feedforward=", XEN_PROCEDURE_CAST g_mus_set_feedforward, 1);
  rb_define_method(mus_xen_tag, "feedback", XEN_PROCEDURE_CAST g_mus_feedback, 0);
  rb_define_method(mus_xen_tag, "feedback=", XEN_PROCEDURE_CAST g_mus_set_increment, 1);
  rb_define_method(mus_xen_tag, "order", XEN_PROCEDURE_CAST g_mus_order, 0);
  rb_define_method(mus_xen_tag, "type", XEN_PROCEDURE_CAST g_mus_type, 0);
  rb_define_method(mus_xen_tag, "order=", XEN_PROCEDURE_CAST g_mus_set_length, 1);
  rb_define_method(mus_xen_tag, "call", XEN_PROCEDURE_CAST mus_xen_apply, 2);
  rb_define_method(mus_xen_tag, "location", XEN_PROCEDURE_CAST g_mus_location, 0);
  rb_define_method(mus_xen_tag, "location=", XEN_PROCEDURE_CAST g_mus_set_location, 1);
  rb_define_method(mus_xen_tag, "increment", XEN_PROCEDURE_CAST g_mus_increment, 0);
  rb_define_method(mus_xen_tag, "increment=", XEN_PROCEDURE_CAST g_mus_set_increment, 1);
  rb_define_method(mus_xen_tag, "channels", XEN_PROCEDURE_CAST g_mus_channels, 0);
  rb_define_method(mus_xen_tag, "channel", XEN_PROCEDURE_CAST g_mus_channel, 0);
  rb_define_method(mus_xen_tag, "interp_type", XEN_PROCEDURE_CAST g_mus_interp_type, 0);
  rb_define_method(mus_xen_tag, "xcoeffs", XEN_PROCEDURE_CAST g_mus_xcoeffs, 0);
  rb_define_method(mus_xen_tag, "ycoeffs", XEN_PROCEDURE_CAST g_mus_ycoeffs, 0);
  rb_define_method(mus_xen_tag, "xcoeff", XEN_PROCEDURE_CAST g_mus_xcoeff, 1);
  rb_define_method(mus_xen_tag, "ycoeff", XEN_PROCEDURE_CAST g_mus_ycoeff, 1);
  /*
  rb_define_method(mus_xen_tag, "xcoeff=", XEN_PROCEDURE_CAST g_mus_set_xcoeff, 1);
  rb_define_method(mus_xen_tag, "ycoeff=", XEN_PROCEDURE_CAST g_mus_set_ycoeff, 1);
  */
  rb_define_method(mus_xen_tag, "ramp", XEN_PROCEDURE_CAST g_mus_ramp, 0);
  rb_define_method(mus_xen_tag, "ramp=", XEN_PROCEDURE_CAST g_mus_set_ramp, 1);
  rb_define_method(mus_xen_tag, "hop", XEN_PROCEDURE_CAST g_mus_hop, 0);
  rb_define_method(mus_xen_tag, "hop=", XEN_PROCEDURE_CAST g_mus_set_hop, 1);
  rb_define_method(mus_xen_tag, "name", XEN_PROCEDURE_CAST g_mus_name, 0);
  rb_define_method(mus_xen_tag, "file_name", XEN_PROCEDURE_CAST g_mus_file_name, 0);
#endif  

  init_keywords();

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mus_srate, g_mus_srate_w, H_mus_srate,
				   S_setB S_mus_srate, g_mus_set_srate_w, 0, 0, 1, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mus_float_equal_fudge_factor, g_mus_float_equal_fudge_factor_w, H_mus_float_equal_fudge_factor,
				   S_setB S_mus_float_equal_fudge_factor, g_mus_set_float_equal_fudge_factor_w, 0, 0, 1, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mus_array_print_length, g_mus_array_print_length_w, H_mus_array_print_length,
				   S_setB S_mus_array_print_length, g_mus_set_array_print_length_w, 0, 0, 1, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_clm_table_size, g_clm_table_size_w, H_clm_table_size,
				   S_setB S_clm_table_size, g_set_clm_table_size_w, 0, 0, 1, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_clm_default_frequency, g_clm_default_frequency_w, H_clm_default_frequency,
				   S_setB S_clm_default_frequency, g_set_clm_default_frequency_w, 0, 0, 1, 0);

  XEN_DEFINE_SAFE_PROCEDURE(S_radians_to_hz,        g_radians_to_hz_w,        1, 0, 0, H_radians_to_hz);
  XEN_DEFINE_SAFE_PROCEDURE(S_hz_to_radians,        g_hz_to_radians_w,        1, 0, 0, H_hz_to_radians);
  XEN_DEFINE_SAFE_PROCEDURE(S_radians_to_degrees,   g_radians_to_degrees_w,   1, 0, 0, H_radians_to_degrees);
  XEN_DEFINE_SAFE_PROCEDURE(S_degrees_to_radians,   g_degrees_to_radians_w,   1, 0, 0, H_degrees_to_radians);
  XEN_DEFINE_SAFE_PROCEDURE(S_db_to_linear,         g_db_to_linear_w,         1, 0, 0, H_db_to_linear);
  XEN_DEFINE_SAFE_PROCEDURE(S_linear_to_db,         g_linear_to_db_w,         1, 0, 0, H_linear_to_db);
  XEN_DEFINE_SAFE_PROCEDURE(S_seconds_to_samples,   g_seconds_to_samples_w,   1, 0, 0, H_seconds_to_samples);
  XEN_DEFINE_SAFE_PROCEDURE(S_samples_to_seconds,   g_samples_to_seconds_w,   1, 0, 0, H_samples_to_seconds);
  XEN_DEFINE_SAFE_PROCEDURE(S_ring_modulate,        g_ring_modulate_w,        2, 0, 0, H_ring_modulate);
  XEN_DEFINE_SAFE_PROCEDURE(S_amplitude_modulate,   g_amplitude_modulate_w,   3, 0, 0, H_amplitude_modulate);
  XEN_DEFINE_SAFE_PROCEDURE(S_contrast_enhancement, g_contrast_enhancement_w, 1, 1, 0, H_contrast_enhancement);
  XEN_DEFINE_SAFE_PROCEDURE(S_dot_product,          g_dot_product_w,          2, 1, 0, H_dot_product);
#if HAVE_COMPLEX_TRIG && XEN_HAVE_COMPLEX_NUMBERS
  XEN_DEFINE_SAFE_PROCEDURE(S_edot_product,         g_edot_product_w,         2, 0, 0, H_edot_product);
#endif
  XEN_DEFINE_SAFE_PROCEDURE(S_clear_array,          g_clear_array_w,          1, 0, 0, H_clear_array);
  XEN_DEFINE_SAFE_PROCEDURE(S_polynomial,           g_polynomial_w,           2, 0, 0, H_polynomial);
  XEN_DEFINE_SAFE_PROCEDURE(S_multiply_arrays,      g_multiply_arrays_w,      2, 1, 0, H_multiply_arrays);
  XEN_DEFINE_SAFE_PROCEDURE(S_make_fft_window,      g_make_fft_window_w,      2, 2, 0, H_make_fft_window);
  XEN_DEFINE_SAFE_PROCEDURE(S_mus_fft,              g_mus_fft_w,              2, 2, 0, H_mus_fft);
  XEN_DEFINE_SAFE_PROCEDURE(S_spectrum,             g_spectrum_w,             3, 1, 0, H_mus_spectrum); 
  XEN_DEFINE_SAFE_PROCEDURE(S_autocorrelate,        g_autocorrelate_w,        1, 0, 0, H_autocorrelate);
  XEN_DEFINE_SAFE_PROCEDURE(S_correlate,            g_correlate_w,            2, 0, 0, H_correlate);
  XEN_DEFINE_SAFE_PROCEDURE(S_convolution,          g_convolution_w,          2, 1, 0, H_mus_convolution);
  XEN_DEFINE_SAFE_PROCEDURE(S_rectangular_to_polar, g_rectangular_to_polar_w, 2, 0, 0, H_rectangular_to_polar);
  XEN_DEFINE_SAFE_PROCEDURE(S_rectangular_to_magnitudes, g_rectangular_to_magnitudes_w, 2, 0, 0, H_rectangular_to_magnitudes);
  XEN_DEFINE_SAFE_PROCEDURE(S_polar_to_rectangular, g_polar_to_rectangular_w, 2, 0, 0, H_polar_to_rectangular);
  XEN_DEFINE_SAFE_PROCEDURE(S_array_interp,         g_array_interp_w,         2, 1, 0, H_array_interp);
  XEN_DEFINE_SAFE_PROCEDURE(S_mus_interpolate,      g_mus_interpolate_w,      3, 2, 0, H_mus_interpolate);
  XEN_DEFINE_SAFE_PROCEDURE(S_mus_frandom,          g_mus_frandom_w,          1, 0, 0, "random reals");
  XEN_DEFINE_SAFE_PROCEDURE(S_mus_irandom,          g_mus_irandom_w,          1, 0, 0, "random integers");

  XEN_DEFINE_CONSTANT(S_rectangular_window,     MUS_RECTANGULAR_WINDOW,     "The un-window, so to speak");
  XEN_DEFINE_CONSTANT(S_hann_window,            MUS_HANN_WINDOW,            "A simple raised cosine window");
  XEN_DEFINE_CONSTANT(S_welch_window,           MUS_WELCH_WINDOW,           "A triangular window squared");
  XEN_DEFINE_CONSTANT(S_parzen_window,          MUS_PARZEN_WINDOW,          "A triangular window");
  XEN_DEFINE_CONSTANT(S_bartlett_window,        MUS_BARTLETT_WINDOW,        "A triangular window");
  XEN_DEFINE_CONSTANT(S_bartlett_hann_window,   MUS_BARTLETT_HANN_WINDOW,   "A combination of the bartlett and hann windows");
  XEN_DEFINE_CONSTANT(S_bohman_window,          MUS_BOHMAN_WINDOW,          "A weighted cosine window");
  XEN_DEFINE_CONSTANT(S_flat_top_window,        MUS_FLAT_TOP_WINDOW,        "A sum of cosines window");
  XEN_DEFINE_CONSTANT(S_hamming_window,         MUS_HAMMING_WINDOW,         "A raised cosine");
  XEN_DEFINE_CONSTANT(S_blackman2_window,       MUS_BLACKMAN2_WINDOW,       "2nd order cosine window");
  XEN_DEFINE_CONSTANT(S_blackman3_window,       MUS_BLACKMAN3_WINDOW,       "3rd order cosine window");
  XEN_DEFINE_CONSTANT(S_blackman4_window,       MUS_BLACKMAN4_WINDOW,       "4th order cosine window");
  XEN_DEFINE_CONSTANT(S_blackman5_window,       MUS_BLACKMAN5_WINDOW,       "5th order cosine window");
  XEN_DEFINE_CONSTANT(S_blackman6_window,       MUS_BLACKMAN6_WINDOW,       "6th order cosine window");
  XEN_DEFINE_CONSTANT(S_blackman7_window,       MUS_BLACKMAN7_WINDOW,       "7th order cosine window");
  XEN_DEFINE_CONSTANT(S_blackman8_window,       MUS_BLACKMAN8_WINDOW,       "8th order cosine window");
  XEN_DEFINE_CONSTANT(S_blackman9_window,       MUS_BLACKMAN9_WINDOW,       "9th order cosine window");
  XEN_DEFINE_CONSTANT(S_blackman10_window,      MUS_BLACKMAN10_WINDOW,      "10th order cosine window");
  XEN_DEFINE_CONSTANT(S_exponential_window,     MUS_EXPONENTIAL_WINDOW,     "An inverted triangle from exp");
  XEN_DEFINE_CONSTANT(S_riemann_window,         MUS_RIEMANN_WINDOW,         "sinc-based window");
  XEN_DEFINE_CONSTANT(S_kaiser_window,          MUS_KAISER_WINDOW,          "Bessel I0 based window");
  XEN_DEFINE_CONSTANT(S_cauchy_window,          MUS_CAUCHY_WINDOW,          "window based on 1/(1+sqr(angle)");
  XEN_DEFINE_CONSTANT(S_poisson_window,         MUS_POISSON_WINDOW,         "window based on exp(-angle)");
  XEN_DEFINE_CONSTANT(S_gaussian_window,        MUS_GAUSSIAN_WINDOW,        "window based on exp(-sqr(angle))");
  XEN_DEFINE_CONSTANT(S_tukey_window,           MUS_TUKEY_WINDOW,           "window based on truncated cosine");
  XEN_DEFINE_CONSTANT(S_dolph_chebyshev_window, MUS_DOLPH_CHEBYSHEV_WINDOW, "window from inverse fft (using Chebyshev Tn)");
  XEN_DEFINE_CONSTANT(S_connes_window,          MUS_CONNES_WINDOW,          "triangle window squared twice");
  XEN_DEFINE_CONSTANT(S_hann_poisson_window,    MUS_HANN_POISSON_WINDOW,    "poisson window * hann window");
  XEN_DEFINE_CONSTANT(S_samaraki_window,        MUS_SAMARAKI_WINDOW,        "window from inverse fft (using Chebyshev Un)");
  XEN_DEFINE_CONSTANT(S_ultraspherical_window,  MUS_ULTRASPHERICAL_WINDOW,  "window from inverse fft (using Ultraspherical Cn)");
  XEN_DEFINE_CONSTANT(S_rv2_window,             MUS_RV2_WINDOW,             "Rife-Vincent 2nd order window (Hann extension)");
  XEN_DEFINE_CONSTANT(S_rv3_window,             MUS_RV3_WINDOW,             "Rife-Vincent 3rd order window (Hann extension)");
  XEN_DEFINE_CONSTANT(S_rv4_window,             MUS_RV4_WINDOW,             "Rife-Vincent 4th order window (Hann extension)");
  XEN_DEFINE_CONSTANT(S_mlt_sine_window,        MUS_MLT_SINE_WINDOW,        "modulated lapped transform sine window");
  XEN_DEFINE_CONSTANT(S_papoulis_window,        MUS_PAPOULIS_WINDOW,        "papoulise window");
  XEN_DEFINE_CONSTANT(S_dpss_window,            MUS_DPSS_WINDOW,            "proplate spheroidal (slepian) window");
  XEN_DEFINE_CONSTANT(S_sinc_window,            MUS_SINC_WINDOW,            "sinc (Lanczos) window");

  XEN_DEFINE_CONSTANT(S_mus_interp_linear,      MUS_INTERP_LINEAR,          "locsig/delay linear interpolation");
  XEN_DEFINE_CONSTANT(S_mus_interp_sinusoidal,  MUS_INTERP_SINUSOIDAL,      "locsig sinusoidal interpolation");
  XEN_DEFINE_CONSTANT(S_mus_interp_all_pass,    MUS_INTERP_ALL_PASS,        "delay interpolation");
  XEN_DEFINE_CONSTANT(S_mus_interp_lagrange,    MUS_INTERP_LAGRANGE,        "2nd order lagrange interpolation");
  XEN_DEFINE_CONSTANT(S_mus_interp_hermite,     MUS_INTERP_HERMITE,         "3rd order hermite interpolation");
  XEN_DEFINE_CONSTANT(S_mus_interp_none,        MUS_INTERP_NONE,            "no interpolation -- step func");
  XEN_DEFINE_CONSTANT(S_mus_interp_bezier,      MUS_INTERP_BEZIER,          "bezier interpolation");

  XEN_DEFINE_CONSTANT(S_mus_chebyshev_first_kind,  MUS_CHEBYSHEV_FIRST_KIND,  "Chebyshev polynomial of first kind, for " S_partials_to_polynomial);
  XEN_DEFINE_CONSTANT(S_mus_chebyshev_second_kind, MUS_CHEBYSHEV_SECOND_KIND, "Chebyshev polynomial of second kind, for " S_partials_to_polynomial);

  XEN_DEFINE_SAFE_PROCEDURE(S_mus_describe,  g_mus_describe_w,  1, 0, 0,  H_mus_describe);
  XEN_DEFINE_SAFE_PROCEDURE(S_mus_file_name, g_mus_file_name_w, 1, 0, 0,  H_mus_file_name);
  XEN_DEFINE_SAFE_PROCEDURE(S_mus_reset,     g_mus_reset_w,     1, 0, 0,  H_mus_reset);
  XEN_DEFINE_PROCEDURE(S_mus_run,       g_mus_run_w,       1, 2, 0,  H_mus_run);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mus_name,      g_mus_name_w,      H_mus_name,      S_setB S_mus_name,      g_mus_set_name_w,       1, 0, 2, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mus_phase,     g_mus_phase_w,     H_mus_phase,     S_setB S_mus_phase,     g_mus_set_phase_w,      1, 0, 2, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mus_scaler,    g_mus_scaler_w,    H_mus_scaler,    S_setB S_mus_scaler,    g_mus_set_scaler_w,     1, 0, 2, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mus_safety,    g_mus_safety_w,    H_mus_safety,    S_setB S_mus_safety,    g_mus_set_safety_w,     1, 0, 2, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mus_width,     g_mus_width_w,     H_mus_width,     S_setB S_mus_width,     g_mus_set_width_w,      1, 0, 2, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mus_frequency, g_mus_frequency_w, H_mus_frequency, S_setB S_mus_frequency, g_mus_set_frequency_w,  1, 0, 2, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mus_length,    g_mus_length_w,    H_mus_length,    S_setB S_mus_length,    g_mus_set_length_w,     1, 0, 2, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mus_data,      g_mus_data_w,      H_mus_data,      S_setB S_mus_data,      g_mus_set_data_w,       1, 0, 2, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mus_xcoeff,    g_mus_xcoeff_w,    H_mus_xcoeff,    S_setB S_mus_xcoeff,    g_mus_set_xcoeff_w,     2, 0, 3, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mus_ycoeff,    g_mus_ycoeff_w,    H_mus_ycoeff,    S_setB S_mus_ycoeff,    g_mus_set_ycoeff_w,     2, 0, 3, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mus_offset,    g_mus_offset_w,    H_mus_offset,    S_setB S_mus_offset,    g_mus_set_offset_w,     1, 0, 2, 0);

  XEN_DEFINE_SAFE_PROCEDURE(S_mus_xcoeffs, g_mus_xcoeffs_w, 1, 0, 0, H_mus_xcoeffs);
  XEN_DEFINE_SAFE_PROCEDURE(S_mus_ycoeffs, g_mus_ycoeffs_w, 1, 0, 0, H_mus_ycoeffs);
  XEN_DEFINE_SAFE_PROCEDURE(S_oscil_p,g_oscil_p_w,     1, 0, 0, H_oscil_p);
  XEN_DEFINE_SAFE_PROCEDURE(S_oscil,  g_oscil_w,       1, 2, 0, H_oscil);
  XEN_DEFINE_PROCEDURE(S_mus_apply,   g_mus_apply_w,   0, 0, 1, H_mus_apply);


  XEN_DEFINE_PROCEDURE(S_make_delay,           g_make_delay_w,      0, 0, 1, H_make_delay);
  XEN_DEFINE_PROCEDURE(S_make_comb,            g_make_comb_w,       0, 0, 1, H_make_comb);
  XEN_DEFINE_PROCEDURE(S_make_filtered_comb,   g_make_filtered_comb_w, 0, 0, 1, H_make_filtered_comb);
  XEN_DEFINE_PROCEDURE(S_make_notch,           g_make_notch_w,      0, 0, 1, H_make_notch); 
  XEN_DEFINE_PROCEDURE(S_make_all_pass,        g_make_all_pass_w,   0, 0, 1, H_make_all_pass);
  XEN_DEFINE_PROCEDURE(S_make_moving_average,  g_make_moving_average_w, 0, 0, 1, H_make_moving_average);
  XEN_DEFINE_SAFE_PROCEDURE(S_delay,           g_delay_w,           1, 2, 0, H_delay); 
  XEN_DEFINE_SAFE_PROCEDURE(S_delay_tick,      g_delay_tick_w,      1, 1, 0, H_delay_tick); 
  XEN_DEFINE_SAFE_PROCEDURE(S_tap,             g_tap_w,             1, 1, 0, H_tap);
  XEN_DEFINE_SAFE_PROCEDURE(S_notch,           g_notch_w,           1, 2, 0, H_notch);
  XEN_DEFINE_SAFE_PROCEDURE(S_comb,            g_comb_w,            1, 2, 0, H_comb);
  XEN_DEFINE_SAFE_PROCEDURE(S_filtered_comb,   g_filtered_comb_w,   1, 2, 0, H_filtered_comb);
  XEN_DEFINE_SAFE_PROCEDURE(S_all_pass,        g_all_pass_w,        1, 2, 0, H_all_pass);
  XEN_DEFINE_SAFE_PROCEDURE(S_moving_average,  g_moving_average_w,  1, 1, 0, H_moving_average);
  XEN_DEFINE_SAFE_PROCEDURE(S_delay_p,         g_delay_p_w,         1, 0, 0, H_delay_p);
  XEN_DEFINE_SAFE_PROCEDURE(S_notch_p,         g_notch_p_w,         1, 0, 0, H_notch_p);
  XEN_DEFINE_SAFE_PROCEDURE(S_comb_p,          g_comb_p_w,          1, 0, 0, H_comb_p);
  XEN_DEFINE_SAFE_PROCEDURE(S_filtered_comb_p, g_filtered_comb_p_w, 1, 0, 0, H_filtered_comb_p);
  XEN_DEFINE_SAFE_PROCEDURE(S_all_pass_p,      g_all_pass_p_w,      1, 0, 0, H_all_pass_p);
  XEN_DEFINE_SAFE_PROCEDURE(S_moving_average_p, g_moving_average_p_w, 1, 0, 0, H_moving_average_p);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mus_feedback, g_mus_feedback_w, H_mus_feedback, S_setB S_mus_feedback, g_mus_set_feedback_w,  1, 0, 2, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mus_feedforward, g_mus_feedforward_w, H_mus_feedforward, S_setB S_mus_feedforward, g_mus_set_feedforward_w,  1, 0, 2, 0);

  XEN_DEFINE_PROCEDURE(S_make_rand,        g_make_rand_w,        0, 0, 1, H_make_rand);
  XEN_DEFINE_PROCEDURE(S_make_rand_interp, g_make_rand_interp_w, 0, 0, 1, H_make_rand_interp);
#if HAVE_RUBY
  rb_define_alias(rb_mKernel, "kernel_rand", "rand");
#endif
  XEN_DEFINE_SAFE_PROCEDURE(S_rand,             g_rand_w,          1, 1, 0, H_rand);
  XEN_DEFINE_SAFE_PROCEDURE(S_rand_interp,      g_rand_interp_w,   1, 1, 0, H_rand_interp);
  XEN_DEFINE_SAFE_PROCEDURE(S_rand_p,           g_rand_p_w,        1, 0, 0, H_rand_p);
  XEN_DEFINE_SAFE_PROCEDURE(S_rand_interp_p,    g_rand_interp_p_w, 1, 0, 0, H_rand_interp_p);
  XEN_DEFINE_SAFE_PROCEDURE(S_mus_random,       g_mus_random_w,    1, 0, 0, H_mus_random);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mus_rand_seed, g_mus_rand_seed_w, H_mus_rand_seed,
				   S_setB S_mus_rand_seed, g_mus_set_rand_seed_w, 0, 0, 1, 0);

  XEN_DEFINE_SAFE_PROCEDURE(S_ncos,                g_ncos_w,                1, 1, 0, H_ncos);
  XEN_DEFINE_SAFE_PROCEDURE(S_ncos_p,              g_ncos_p_w,              1, 0, 0, H_ncos_p);
  XEN_DEFINE_SAFE_PROCEDURE(S_nsin,                g_nsin_w,                1, 1, 0, H_nsin);
  XEN_DEFINE_SAFE_PROCEDURE(S_nsin_p,              g_nsin_p_w,              1, 0, 0, H_nsin_p);

  XEN_DEFINE_SAFE_PROCEDURE(S_table_lookup_p,     g_table_lookup_p_w,     1, 0, 0, H_table_lookup_p);
  XEN_DEFINE_PROCEDURE(S_make_table_lookup,       g_make_table_lookup_w,  0, 0, 1, H_make_table_lookup);
  XEN_DEFINE_SAFE_PROCEDURE(S_table_lookup,       g_table_lookup_w,       1, 1, 0, H_table_lookup);
  XEN_DEFINE_SAFE_PROCEDURE(S_partials_to_wave,   g_partials_to_wave_w,   1, 2, 0, H_partials_to_wave);
  XEN_DEFINE_SAFE_PROCEDURE(S_phase_partials_to_wave, g_phase_partials_to_wave_w, 1, 2, 0, H_phase_partials_to_wave);


  XEN_DEFINE_SAFE_PROCEDURE(S_make_sawtooth_wave, g_make_sawtooth_wave_w, 0, 6, 0, H_make_sawtooth_wave);
  XEN_DEFINE_SAFE_PROCEDURE(S_sawtooth_wave,      g_sawtooth_wave_w,      1, 1, 0, H_sawtooth_wave);
  XEN_DEFINE_SAFE_PROCEDURE(S_sawtooth_wave_p,    g_sawtooth_wave_p_w,    1, 0, 0, H_sawtooth_wave_p);
  XEN_DEFINE_SAFE_PROCEDURE(S_make_triangle_wave, g_make_triangle_wave_w, 0, 6, 0, H_make_triangle_wave);
  XEN_DEFINE_SAFE_PROCEDURE(S_triangle_wave,      g_triangle_wave_w,      1, 1, 0, H_triangle_wave);
  XEN_DEFINE_SAFE_PROCEDURE(S_triangle_wave_p,    g_triangle_wave_p_w,    1, 0, 0, H_triangle_wave_p);
  XEN_DEFINE_SAFE_PROCEDURE(S_make_square_wave,   g_make_square_wave_w,   0, 6, 0, H_make_square_wave);
  XEN_DEFINE_SAFE_PROCEDURE(S_square_wave,        g_square_wave_w,        1, 1, 0, H_square_wave);
  XEN_DEFINE_SAFE_PROCEDURE(S_square_wave_p,      g_square_wave_p_w,      1, 0, 0, H_square_wave_p);
  XEN_DEFINE_SAFE_PROCEDURE(S_make_pulse_train,   g_make_pulse_train_w,   0, 6, 0, H_make_pulse_train);
  XEN_DEFINE_SAFE_PROCEDURE(S_pulse_train,        g_pulse_train_w,        1, 1, 0, H_pulse_train);
  XEN_DEFINE_SAFE_PROCEDURE(S_pulse_train_p,      g_pulse_train_p_w,      1, 0, 0, H_pulse_train_p);


  XEN_DEFINE_SAFE_PROCEDURE(S_asymmetric_fm,      g_asymmetric_fm_w,      1, 2, 0, H_asymmetric_fm);
  XEN_DEFINE_SAFE_PROCEDURE(S_asymmetric_fm_p,    g_asymmetric_fm_p_w,    1, 0, 0, H_asymmetric_fm_p);


  XEN_DEFINE_SAFE_PROCEDURE(S_make_one_zero, g_make_one_zero_w, 0, 4, 0, H_make_one_zero);
  XEN_DEFINE_SAFE_PROCEDURE(S_one_zero,      g_one_zero_w,      1, 1, 0, H_one_zero);
  XEN_DEFINE_SAFE_PROCEDURE(S_one_zero_p,    g_one_zero_p_w,    1, 0, 0, H_one_zero_p);
  XEN_DEFINE_SAFE_PROCEDURE(S_make_one_pole, g_make_one_pole_w, 0, 4, 0, H_make_one_pole);
  XEN_DEFINE_SAFE_PROCEDURE(S_one_pole,      g_one_pole_w,      1, 1, 0, H_one_pole);
  XEN_DEFINE_SAFE_PROCEDURE(S_one_pole_p,    g_one_pole_p_w,    1, 0, 0, H_one_pole_p);
  XEN_DEFINE_SAFE_PROCEDURE(S_make_two_zero, g_make_two_zero_w, 0, 6, 0, H_make_two_zero);
  XEN_DEFINE_SAFE_PROCEDURE(S_two_zero,      g_two_zero_w,      1, 1, 0, H_two_zero);
  XEN_DEFINE_SAFE_PROCEDURE(S_two_zero_p,    g_two_zero_p_w,    1, 0, 0, H_two_zero_p);
  XEN_DEFINE_SAFE_PROCEDURE(S_make_two_pole, g_make_two_pole_w, 0, 6, 0, H_make_two_pole);
  XEN_DEFINE_SAFE_PROCEDURE(S_two_pole,      g_two_pole_w,      1, 1, 0, H_two_pole);
  XEN_DEFINE_SAFE_PROCEDURE(S_two_pole_p,    g_two_pole_p_w,    1, 0, 0, H_two_pole_p);

  XEN_DEFINE_PROCEDURE(S_make_wave_train,     g_make_wave_train_w, 0, 0, 1, H_make_wave_train);
  XEN_DEFINE_SAFE_PROCEDURE(S_wave_train,     g_wave_train_w,      1, 1, 0, H_wave_train);
  XEN_DEFINE_SAFE_PROCEDURE(S_wave_train_p,   g_wave_train_p_w,    1, 0, 0, H_wave_train_p);

  XEN_DEFINE_PROCEDURE(S_make_frame,          g_make_frame_w,            0, 0, 1, H_make_frame);
  XEN_DEFINE_PROCEDURE(S_make_frame "!",      g_make_frame_unchecked_w,  0, 0, 1, H_make_frame_unchecked);
  XEN_DEFINE_PROCEDURE(S_frame,               g_frame_w,                 0, 0, 1, H_frame);

  XEN_DEFINE_SAFE_PROCEDURE(S_frame_p,        g_frame_p_w,        1, 0, 0, H_frame_p);
  XEN_DEFINE_SAFE_PROCEDURE(S_frame_add,      g_frame_add_w,      2, 1, 0, H_frame_add);
  XEN_DEFINE_SAFE_PROCEDURE(S_frame_multiply, g_frame_multiply_w, 2, 1, 0, H_frame_multiply);
#if HAVE_SCHEME || HAVE_FORTH
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_frame_ref, g_frame_ref_w, H_frame_ref, S_setB S_frame_ref, g_frame_set_w,  2, 0, 3, 0);
#endif
#if HAVE_RUBY
  XEN_DEFINE_PROCEDURE(S_frame_ref,           g_frame_ref_w,  2, 0, 0, H_frame_ref);
#endif
  XEN_DEFINE_SAFE_PROCEDURE(S_frame_set,      g_frame_set_w,  3, 0, 0, H_frame_set);


  XEN_DEFINE_PROCEDURE(S_make_mixer,        g_make_mixer_w,           0, 0, 1, H_make_mixer);
  XEN_DEFINE_PROCEDURE(S_make_mixer "!",    g_make_mixer_unchecked_w, 0, 0, 1, H_make_mixer_unchecked);
  XEN_DEFINE_PROCEDURE(S_mixer,             g_mixer_w,                0, 0, 1, H_mixer);
  XEN_DEFINE_SAFE_PROCEDURE(S_mixer_p,      g_mixer_p_w,              1, 0, 0, H_mixer_p);
  XEN_DEFINE_SAFE_PROCEDURE(S_mixer_multiply,    g_mixer_multiply_w,       2, 1, 0, H_mixer_multiply);
  XEN_DEFINE_SAFE_PROCEDURE(S_mixer_add,         g_mixer_add_w,            2, 1, 0, H_mixer_add);
  XEN_DEFINE_SAFE_PROCEDURE(S_make_scalar_mixer, g_make_scalar_mixer_w,    2, 0, 0, H_make_scalar_mixer);
#if HAVE_SCHEME || HAVE_FORTH
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mixer_ref, g_mixer_ref_w, H_mixer_ref, S_setB S_mixer_ref, g_mixer_set_w,  3, 0, 4, 0);
#endif
#if HAVE_RUBY
  XEN_DEFINE_PROCEDURE(S_mixer_ref,         g_mixer_ref_w,         3, 0, 0, H_mixer_ref);
#endif
  XEN_DEFINE_SAFE_PROCEDURE(S_mixer_set,    g_mixer_set_w,         4, 0, 0, H_mixer_set);
  XEN_DEFINE_SAFE_PROCEDURE(S_frame_to_sample,   g_frame_to_sample_w,   2, 0, 0, H_frame_to_sample);
  XEN_DEFINE_SAFE_PROCEDURE(S_frame_to_list,     g_frame_to_list_w,     1, 0, 0, H_frame_to_list);
  XEN_DEFINE_SAFE_PROCEDURE(S_frame_to_frame,    g_frame_to_frame_w,    2, 1, 0, H_frame_to_frame);
  XEN_DEFINE_SAFE_PROCEDURE(S_sample_to_frame,   g_sample_to_frame_w,   2, 1, 0, H_sample_to_frame);


  XEN_DEFINE_SAFE_PROCEDURE(S_formant_bank, g_formant_bank_w, 2, 1, 0, H_formant_bank);
  XEN_DEFINE_SAFE_PROCEDURE(S_formant_p,    g_formant_p_w,    1, 0, 0, H_formant_p);
  XEN_DEFINE_SAFE_PROCEDURE(S_make_formant, g_make_formant_w, 0, 4, 0, H_make_formant);
  XEN_DEFINE_SAFE_PROCEDURE(S_formant,      g_formant_w,      1, 2, 0, H_formant);
  XEN_DEFINE_SAFE_PROCEDURE(S_firmant_p,    g_firmant_p_w,    1, 0, 0, H_firmant_p);
  XEN_DEFINE_SAFE_PROCEDURE(S_make_firmant, g_make_firmant_w, 0, 4, 0, H_make_firmant);
  XEN_DEFINE_SAFE_PROCEDURE(S_firmant,      g_firmant_w,      1, 2, 0, H_firmant);

  XEN_DEFINE_PROCEDURE(S_mus_set_formant_radius_and_frequency, g_set_formant_radius_and_frequency_w, 3, 0, 0, H_mus_set_formant_radius_and_frequency);


  XEN_DEFINE_PROCEDURE(S_make_polyshape,              g_make_polyshape_w,         0, 0, 1, H_make_polyshape);
  XEN_DEFINE_SAFE_PROCEDURE(S_polyshape,              g_polyshape_w,              1, 2, 0, H_polyshape);
  XEN_DEFINE_SAFE_PROCEDURE(S_polyshape_p,            g_polyshape_p_w,            1, 0, 0, H_polyshape_p);
  XEN_DEFINE_SAFE_PROCEDURE(S_partials_to_polynomial, g_partials_to_polynomial_w, 1, 1, 0, H_partials_to_polynomial);
  XEN_DEFINE_SAFE_PROCEDURE(S_normalize_partials,     g_normalize_partials_w,     1, 0, 0, H_normalize_partials);
  XEN_DEFINE_SAFE_PROCEDURE(S_mus_chebyshev_t_sum,    g_chebyshev_t_sum_w,        2, 0, 0, H_chebyshev_t_sum);
  XEN_DEFINE_SAFE_PROCEDURE(S_mus_chebyshev_u_sum,    g_chebyshev_u_sum_w,        2, 0, 0, H_chebyshev_u_sum);
  XEN_DEFINE_SAFE_PROCEDURE(S_mus_chebyshev_tu_sum,   g_chebyshev_tu_sum_w,       3, 0, 0, H_chebyshev_tu_sum);
  XEN_DEFINE_PROCEDURE(S_make_polywave,               g_make_polywave_w,          0, 0, 1, H_make_polywave);
  XEN_DEFINE_SAFE_PROCEDURE(S_polywave,               g_polywave_w,               1, 1, 0, H_polywave);
  XEN_DEFINE_SAFE_PROCEDURE(S_polywave_p,             g_polywave_p_w,             1, 0, 0, H_polywave_p);

  XEN_DEFINE_PROCEDURE(S_make_nrxysin,                g_make_nrxysin_w,           0, 0, 1, H_make_nrxysin);
  XEN_DEFINE_SAFE_PROCEDURE(S_nrxysin,                g_nrxysin_w,                1, 1, 0, H_nrxysin);
  XEN_DEFINE_SAFE_PROCEDURE(S_nrxysin_p,              g_nrxysin_p_w,              1, 0, 0, H_nrxysin_p);
  XEN_DEFINE_PROCEDURE(S_make_nrxycos,                g_make_nrxycos_w,           0, 0, 1, H_make_nrxycos);
  XEN_DEFINE_SAFE_PROCEDURE(S_nrxycos,                g_nrxycos_w,                1, 1, 0, H_nrxycos);
  XEN_DEFINE_SAFE_PROCEDURE(S_nrxycos_p,              g_nrxycos_p_w,              1, 0, 0, H_nrxycos_p);


  XEN_DEFINE_SAFE_PROCEDURE(S_make_filter,     g_make_filter_w,     0, 6, 0, H_make_filter);
  XEN_DEFINE_SAFE_PROCEDURE(S_filter,          g_filter_w,          2, 0, 0, H_filter);
  XEN_DEFINE_SAFE_PROCEDURE(S_filter_p,        g_filter_p_w,        1, 0, 0, H_filter_p);
  XEN_DEFINE_SAFE_PROCEDURE(S_make_fir_coeffs, g_make_fir_coeffs_w, 2, 0, 0, H_make_fir_coeffs);
  XEN_DEFINE_SAFE_PROCEDURE(S_make_fir_filter, g_make_fir_filter_w, 0, 4, 0, H_make_fir_filter);
  XEN_DEFINE_SAFE_PROCEDURE(S_fir_filter,      g_fir_filter_w,      2, 0, 0, H_fir_filter);
  XEN_DEFINE_SAFE_PROCEDURE(S_fir_filter_p,    g_fir_filter_p_w,    1, 0, 0, H_fir_filter_p);
  XEN_DEFINE_SAFE_PROCEDURE(S_make_iir_filter, g_make_iir_filter_w, 0, 4, 0, H_make_iir_filter);
  XEN_DEFINE_SAFE_PROCEDURE(S_iir_filter,      g_iir_filter_w,      2, 0, 0, H_iir_filter);
  XEN_DEFINE_SAFE_PROCEDURE(S_iir_filter_p,    g_iir_filter_p_w,    1, 0, 0, H_iir_filter_p);
  XEN_DEFINE_PROCEDURE(S_mus_order,            g_mus_order_w,       1, 0, 0, H_mus_order);
  XEN_DEFINE_PROCEDURE(S_mus_type,             g_mus_type_w,        1, 0, 0, H_mus_type);


  XEN_DEFINE_SAFE_PROCEDURE(S_env_p,       g_env_p_w,       1, 0, 0, H_env_p);
  XEN_DEFINE_SAFE_PROCEDURE(S_env,         g_env_w,         1, 0, 0, H_env);
  XEN_DEFINE_PROCEDURE(S_make_env,         g_make_env_w,    0, 0, 1, H_make_env);
  XEN_DEFINE_SAFE_PROCEDURE(S_env_interp,  g_env_interp_w,  2, 0, 0, H_env_interp);
  XEN_DEFINE_PROCEDURE(S_env_any,          g_env_any_w,     2, 0, 0, H_env_any);


  XEN_DEFINE_SAFE_PROCEDURE(S_locsig_p,     g_locsig_p_w,     1, 0, 0, H_locsig_p);
  XEN_DEFINE_SAFE_PROCEDURE(S_locsig,       g_locsig_w,       3, 0, 0, H_locsig);
  XEN_DEFINE_PROCEDURE(S_make_locsig,       g_make_locsig_w,  0, 0, 1, H_make_locsig);
  XEN_DEFINE_PROCEDURE(S_move_locsig,       g_move_locsig_w,  3, 0, 0, H_move_locsig);
  XEN_DEFINE_PROCEDURE(S_mus_channels,      g_mus_channels_w, 1, 0, 0, H_mus_channels);

#if HAVE_SCHEME || HAVE_FORTH
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_locsig_ref, g_locsig_ref_w, H_locsig_ref, S_setB S_locsig_ref, g_locsig_set_w,  2, 0, 3, 0);
#endif

#if HAVE_RUBY
  XEN_DEFINE_PROCEDURE(S_locsig_ref,        g_locsig_ref_w,   2, 0, 0, H_locsig_ref);
#endif

  XEN_DEFINE_SAFE_PROCEDURE(S_locsig_set,   g_locsig_set_w,   3, 0, 0, H_locsig_set);

#if HAVE_SCHEME || HAVE_FORTH
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_locsig_reverb_ref, g_locsig_reverb_ref_w, H_locsig_reverb_ref, 
				   S_locsig_reverb_set, g_locsig_reverb_set_w,  2, 0, 3, 0);
#endif

#if HAVE_RUBY
  XEN_DEFINE_PROCEDURE(S_locsig_reverb_ref, g_locsig_reverb_ref_w, 2, 0, 0, H_locsig_reverb_ref);
#endif

  XEN_DEFINE_SAFE_PROCEDURE(S_locsig_reverb_set, g_locsig_reverb_set_w, 3, 0, 0, H_locsig_reverb_set);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_locsig_type, g_locsig_type_w, H_locsig_type, S_setB S_locsig_type, g_set_locsig_type_w,  0, 0, 1, 0);

  XEN_DEFINE_SAFE_PROCEDURE(S_move_sound_p,            g_move_sound_p_w,     1, 0, 0, H_move_sound_p);
  XEN_DEFINE_SAFE_PROCEDURE(S_move_sound,              g_move_sound_w,       3, 0, 0, H_move_sound);
  XEN_DEFINE_SAFE_PROCEDURE(S_make_move_sound,         g_make_move_sound_w,  1, 2, 0, H_make_move_sound);

  XEN_DEFINE_SAFE_PROCEDURE(S_file_to_sample_p,        g_file_to_sample_p_w,        1, 0, 0, H_file_to_sample_p);
  XEN_DEFINE_SAFE_PROCEDURE(S_make_file_to_sample,     g_make_file_to_sample_w,     1, 1, 0, H_make_file_to_sample);
  XEN_DEFINE_SAFE_PROCEDURE(S_file_to_sample,          g_file_to_sample_w,          2, 1, 0, H_file_to_sample);
  XEN_DEFINE_SAFE_PROCEDURE(S_file_to_frame_p,         g_file_to_frame_p_w,         1, 0, 0, H_file_to_frame_p);
  XEN_DEFINE_SAFE_PROCEDURE(S_make_file_to_frame,      g_make_file_to_frame_w,      1, 1, 0, H_make_file_to_frame);
  XEN_DEFINE_SAFE_PROCEDURE(S_file_to_frame,           g_file_to_frame_w,           2, 1, 0, H_file_to_frame);
  XEN_DEFINE_SAFE_PROCEDURE(S_sample_to_file_p,        g_sample_to_file_p_w,        1, 0, 0, H_sample_to_file_p);
  XEN_DEFINE_SAFE_PROCEDURE(S_make_sample_to_file,     g_make_sample_to_file_w,     1, 4, 0, H_make_sample_to_file);
  XEN_DEFINE_SAFE_PROCEDURE(S_continue_sample_to_file, g_continue_sample_to_file_w, 1, 0, 0, H_continue_sample_to_file);
  XEN_DEFINE_SAFE_PROCEDURE(S_continue_frame_to_file,  g_continue_frame_to_file_w,  1, 0, 0, H_continue_frame_to_file);
  XEN_DEFINE_SAFE_PROCEDURE(S_sample_to_file,          g_sample_to_file_w,          4, 0, 0, H_sample_to_file);
  XEN_DEFINE_SAFE_PROCEDURE(S_sample_to_file_add,      g_sample_to_file_add_w,      2, 0, 0, H_sample_to_file_add);
  XEN_DEFINE_SAFE_PROCEDURE(S_frame_to_file_p,         g_frame_to_file_p_w,         1, 0, 0, H_frame_to_file_p);
  XEN_DEFINE_SAFE_PROCEDURE(S_frame_to_file,           g_frame_to_file_w,           3, 0, 0, H_frame_to_file);
  XEN_DEFINE_SAFE_PROCEDURE(S_make_frame_to_file,      g_make_frame_to_file_w,      1, 4, 0, H_make_frame_to_file);
  XEN_DEFINE_SAFE_PROCEDURE(S_mus_input_p,             g_input_p_w,                 1, 0, 0, H_mus_input_p);
  XEN_DEFINE_SAFE_PROCEDURE(S_mus_output_p,            g_output_p_w,                1, 0, 0, H_mus_output_p);
  XEN_DEFINE_SAFE_PROCEDURE(S_in_any,                  g_in_any_w,                  3, 0, 0, H_in_any);
  XEN_DEFINE_SAFE_PROCEDURE(S_ina,                     g_ina_w,                     2, 0, 0, H_ina);  
  XEN_DEFINE_SAFE_PROCEDURE(S_inb,                     g_inb_w,                     2, 0, 0, H_inb);
  XEN_DEFINE_SAFE_PROCEDURE(S_out_any,                 g_out_any_w,                 3, 1, 0, H_out_any);
  XEN_DEFINE_SAFE_PROCEDURE(S_outa,                    g_outa_w,                    2, 1, 0, H_outa);
  XEN_DEFINE_SAFE_PROCEDURE(S_outb,                    g_outb_w,                    2, 1, 0, H_outb);
  XEN_DEFINE_SAFE_PROCEDURE(S_outc,                    g_outc_w,                    2, 1, 0, H_outc);
  XEN_DEFINE_SAFE_PROCEDURE(S_outd,                    g_outd_w,                    2, 1, 0, H_outd);
  XEN_DEFINE_SAFE_PROCEDURE(S_mus_close,               g_mus_close_w,               1, 0, 0, H_mus_close);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mus_file_buffer_size, g_mus_file_buffer_size_w, H_mus_file_buffer_size,
				   S_setB S_mus_file_buffer_size, g_mus_set_file_buffer_size_w,  0, 0, 1, 0);


  XEN_DEFINE_SAFE_PROCEDURE(S_readin_p,        g_readin_p_w,        1, 0, 0, H_readin_p);
  XEN_DEFINE_SAFE_PROCEDURE(S_readin,          g_readin_w,          1, 0, 0, H_readin);
  XEN_DEFINE_PROCEDURE(S_make_readin,          g_make_readin_w,     0, 0, 1, H_make_readin);
  XEN_DEFINE_PROCEDURE(S_mus_channel,          g_mus_channel_w,     1, 0, 0, H_mus_channel);
  XEN_DEFINE_PROCEDURE(S_mus_interp_type,      g_mus_interp_type_w, 1, 0, 0, H_mus_interp_type);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mus_location, g_mus_location_w, H_mus_location, S_setB S_mus_location, g_mus_set_location_w,  1, 0, 2, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mus_increment, g_mus_increment_w, H_mus_increment, S_setB S_mus_increment, g_mus_set_increment_w,  1, 0, 2, 0);

  XEN_DEFINE_SAFE_PROCEDURE(S_granulate_p,    g_granulate_p_w,    1, 0, 0, H_granulate_p);
  XEN_DEFINE_PROCEDURE(S_granulate,           g_granulate_w,      1, 2, 0, H_granulate);
  XEN_DEFINE_PROCEDURE(S_make_granulate,      g_make_granulate_w, 0, 0, 1, H_make_granulate);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mus_ramp, g_mus_ramp_w, H_mus_ramp,
				   S_setB S_mus_ramp, g_mus_set_ramp_w,  1, 0, 2, 0);


  XEN_DEFINE_SAFE_PROCEDURE(S_clear_sincs, g_mus_clear_sincs_w, 0, 0, 0, "clears out any sinc tables");
  XEN_DEFINE_SAFE_PROCEDURE(S_src_p,       g_src_p_w,       1, 0, 0, H_src_p);
  XEN_DEFINE_PROCEDURE(S_src,              g_src_w,         1, 2, 0, H_src);
  XEN_DEFINE_PROCEDURE(S_make_src,         g_make_src_w,    0, 6, 0, H_make_src);


  XEN_DEFINE_SAFE_PROCEDURE(S_convolve_p,     g_convolve_p_w,     1, 0, 0, H_convolve_p);
  XEN_DEFINE_PROCEDURE(S_convolve,            g_convolve_w,       1, 1, 0, H_convolve_gen);
  XEN_DEFINE_PROCEDURE(S_make_convolve,       g_make_convolve_w,  0, 0, 1, H_make_convolve);
  XEN_DEFINE_SAFE_PROCEDURE(S_convolve_files, g_convolve_files_w, 2, 2, 0, H_convolve_files);

  XEN_DEFINE_SAFE_PROCEDURE(S_phase_vocoder_p,             g_phase_vocoder_p_w,                1, 0, 0, H_phase_vocoder_p);
  XEN_DEFINE_PROCEDURE(S_phase_vocoder,                    g_phase_vocoder_w,                  1, 4, 0, H_phase_vocoder);
  XEN_DEFINE_PROCEDURE(S_make_phase_vocoder,               g_make_phase_vocoder_w,             0, 0, 1, H_make_phase_vocoder);
  XEN_DEFINE_PROCEDURE(S_phase_vocoder_amp_increments,     g_phase_vocoder_amp_increments_w,   1, 0, 0, H_phase_vocoder_amp_increments);
  XEN_DEFINE_PROCEDURE(S_phase_vocoder_amps,               g_phase_vocoder_amps_w,             1, 0, 0, H_phase_vocoder_amps);
  XEN_DEFINE_PROCEDURE(S_phase_vocoder_freqs,              g_phase_vocoder_freqs_w,            1, 0, 0, H_phase_vocoder_freqs);
  XEN_DEFINE_PROCEDURE(S_phase_vocoder_phases,             g_phase_vocoder_phases_w,           1, 0, 0, H_phase_vocoder_phases);
  XEN_DEFINE_PROCEDURE(S_phase_vocoder_phase_increments,   g_phase_vocoder_phase_increments_w, 1, 0, 0, H_phase_vocoder_phase_increments);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mus_hop, g_mus_hop_w, H_mus_hop, S_setB S_mus_hop, g_mus_set_hop_w,  1, 0, 2, 0);

  XEN_DEFINE_SAFE_PROCEDURE(S_mus_mix, g_mus_mix_w, 2, 5, 0, H_mus_mix);

  XEN_DEFINE_SAFE_PROCEDURE(S_make_ssb_am,   g_make_ssb_am_w,   0, 4, 0, H_make_ssb_am); 
  XEN_DEFINE_SAFE_PROCEDURE(S_ssb_am,        g_ssb_am_w,        1, 2, 0, H_ssb_am);
  XEN_DEFINE_SAFE_PROCEDURE(S_ssb_am_p,      g_ssb_am_p_w,      1, 0, 0, H_ssb_am_p);
  XEN_DEFINE_SAFE_PROCEDURE("mus-ssb-bank",  g_ssb_bank_w,      4, 0, 0, "an experiment");

  XEN_DEFINE_SAFE_PROCEDURE(S_mus_generator_p, g_mus_generator_p_w, 1, 0, 0, H_mus_generator_p);

  XEN_DEFINE_VARIABLE(S_output, clm_output, XEN_FALSE);
  XEN_DEFINE_VARIABLE(S_reverb, clm_reverb, XEN_FALSE);
#if HAVE_SCHEME
  /* these are globals in s7, so they aren't going to move */
  clm_output_slot = s7_slot(s7, clm_output);
  clm_reverb_slot = s7_slot(s7, clm_reverb);
#endif

#if HAVE_SCHEME && HAVE_GETTIMEOFDAY && HAVE_DIFFTIME && HAVE_SYS_TIME_H && (!_MSC_VER)
  XEN_DEFINE_SAFE_PROCEDURE(S_get_internal_real_time, g_get_internal_real_time_w, 0, 0, 0, H_get_internal_real_time);
  XEN_DEFINE_CONSTANT(S_internal_time_units_per_second, 1, "units used by " S_get_internal_real_time);
#endif

  XEN_DEFINE_SAFE_PROCEDURE(S_make_oscil,          g_make_oscil_w,          0, 4, 0, H_make_oscil);
  XEN_DEFINE_SAFE_PROCEDURE(S_make_ncos,           g_make_ncos_w,           0, 4, 0, H_make_ncos); 
  XEN_DEFINE_SAFE_PROCEDURE(S_make_nsin,           g_make_nsin_w,           0, 4, 0, H_make_nsin); 
  XEN_DEFINE_SAFE_PROCEDURE(S_make_asymmetric_fm,  g_make_asymmetric_fm_w,  0, 8, 0, H_make_asymmetric_fm);

#if HAVE_SCHEME
  init_choosers(s7);
#if 0
  g_frame_methods = s7_eval_c_string(s7, "(augment-environment ()                                        \n\
                                            (cons 'vector? (lambda (p) #t))                              \n\
                                            (cons 'vector-length mus-length)                             \n\
                                            (cons 'vector-dimensions (lambda (p) (list (mus-length p)))) \n\
                                            (cons 'vector-ref frame-ref)                                 \n\
                                            (cons 'vector-set! frame-set!)                               \n\
                                            (cons 'vector-fill! fill!)                                   \n\
                                            (cons 'vector->list frame->list))");
  s7_gc_protect(s7, g_frame_methods);

  g_mixer_methods = s7_eval_c_string(s7, "(augment-environment ()                                        \n\
                                            (cons 'vector? (lambda (p) #t))                              \n\
                                            (cons 'vector-length                                         \n\
                                                  (lambda (p) (* (mus-length p) (mus-length p))))        \n\
                                            (cons 'vector-dimensions                                     \n\
                                                  (lambda (p) (list (mus-length p) (mus-length p))))     \n\
                                            (cons 'vector-ref mixer-ref)                                 \n\
                                            (cons 'vector-set! mixer-set!)                               \n\
                                            (cons 'vector-fill! fill!)                                   \n\
                                            (cons 'vector->list                                          \n\
                                                  (lambda (p)                                            \n\
                                                    (do ((i (- (mus-length p) 1) (- i 1))                \n\
                                                         (lst ()))                                       \n\
                                                        ((< i 0) lst)                                    \n\
                                                      (do ((j (- (mus-length p) 1) (- j 1)))             \n\
                                                          ((< j 0))                                      \n\
                                                        (set! lst (cons (p i j) lst)))))))");
  s7_gc_protect(s7, g_mixer_methods);
#endif
#endif


  /* -------- clm-print (see also snd-xen.c) -------- */
#if (!USE_SND)

#if HAVE_FORTH
XEN_EVAL_C_STRING("<'> fth-print alias clm-print ( fmt args -- )"); 
#endif

#if HAVE_RUBY
  XEN_EVAL_C_STRING("def clm_print(str, *args)\n\
                      $stdout.print format(str, *args)\n\
                      end");
#endif

#if HAVE_SCHEME
  XEN_EVAL_C_STRING("(define (clm-print . args) (apply format #t args))");
#endif
#endif


  XEN_PROVIDE("clm");
  {
    char *clm_version;
    clm_version = mus_format("clm%d", MUS_VERSION);
    XEN_PROVIDE(clm_version);
    free(clm_version);
  }

#if HAVE_SCHEME && HAVE_GETTIMEOFDAY && HAVE_DIFFTIME && HAVE_SYS_TIME_H && (!_MSC_VER)
  {
    struct timezone z0;
    gettimeofday(&overall_start_time, &z0);
  }
#endif
}


void mus_init_run(void);

void Init_sndlib(void)
{
  mus_sndlib_xen_initialize();
  mus_vct_init();
  mus_xen_init();
#if (!USE_SND)
  mus_init_run();
#endif
}
