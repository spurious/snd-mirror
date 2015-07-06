/* tie CLM module into Scheme, Ruby, or Forth */

/* if the optimizer stops working inexplicably, look for any symbols used before this that
 *    might shadow a generator name; one such case was (make-hook 'env...) in snd-env.c
 *
 * (env env) is accepted by the optimizer in error
 */

#include "mus-config.h"

#if USE_SND
  #include "snd.h"
#endif

#include <stddef.h>
#include <math.h>
#include <stdio.h>
#include <errno.h>
#include <stdlib.h>
#include <limits.h>
#include <string.h>
#include <stdarg.h>

#ifndef _MSC_VER
  #include <unistd.h>
#else
  #include <io.h>
  #pragma warning(disable: 4244)
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

#if (!DISABLE_SINCOS) && defined(__GNUC__) && defined(__linux__)
  void sincos(double x, double *sin, double *cos);
  #define HAVE_SINCOS 1
#else
  #define HAVE_SINCOS 0
#endif


/* -------------------------------------------------------------------------------- */
#if HAVE_SCHEME
static bool mus_simple_out_any_to_file(mus_long_t samp, mus_float_t val, int chan, mus_any *IO)
{
  rdout *gen = (rdout *)IO;
  if ((chan < gen->chans) &&
      (samp <= gen->data_end) &&
      (samp >= gen->data_start))
    {
      gen->obufs[chan][samp - gen->data_start] += val;
      if (samp > gen->out_end) 
	gen->out_end = samp;
      return(true);
    }
  return(false);
}
#endif

/* -------------------------------------------------------------------------------- */

struct mus_xen {
  mus_any *gen;
  int nvcts;
  Xen *vcts; /* one for each accessible mus_float_t array (wrapped up here in a vct) */
  struct mus_xen *next;
};


enum {MUS_DATA_WRAPPER, MUS_INPUT_FUNCTION, MUS_ANALYZE_FUNCTION, MUS_EDIT_FUNCTION, MUS_SYNTHESIZE_FUNCTION, MUS_SELF_WRAPPER, MUS_INPUT_DATA, MUS_MAX_VCTS};

static mus_xen *mx_free_lists[8] = {NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL};

static mus_xen *mx_alloc(int vcts)
{
  mus_xen *p;
  if (mx_free_lists[vcts])
    {
      p = mx_free_lists[vcts];
      mx_free_lists[vcts] = p->next;
      return(p);
    }
  p = (mus_xen *)malloc(sizeof(mus_xen));
  p->nvcts = vcts;
  if (vcts > 0)
    p->vcts = (Xen *)malloc(vcts * sizeof(Xen));
  else p->vcts = NULL;
  return(p);
}


static void mx_free(mus_xen *p)
{
  p->next = mx_free_lists[p->nvcts];
  mx_free_lists[p->nvcts] = p;
}


mus_any *mus_xen_gen(mus_xen *x) {return(x->gen);}
#define mus_xen_to_mus_any(Gn) (((mus_xen *)Gn)->gen)

#if (!HAVE_SCHEME)
#define XEN_NULL 0

#define Xen_real_to_C_double_if_bound(Xen_Arg, C_Val, Caller, ArgNum) \
  if (Xen_is_bound(Xen_Arg)) {if (Xen_is_number(Xen_Arg)) C_Val = Xen_real_to_C_double(Xen_Arg); else Xen_check_type(false, Xen_Arg, ArgNum, Caller, "a number");}

#define Xen_to_C_double_or_error(Xen_Arg, C_Val, Caller, ArgNum) \
  do {C_Val = 0.0; if (Xen_is_number(Xen_Arg)) C_Val = Xen_real_to_C_double(Xen_Arg); else Xen_check_type(false, Xen_Arg, ArgNum, Caller, "a number");} while (0)

#define Xen_to_C_integer_or_error(Xen_Arg, C_Val, Caller, ArgNum) \
  do {C_Val = 0; if (Xen_is_number(Xen_Arg)) C_Val = Xen_integer_to_C_int(Xen_Arg); else Xen_check_type(false, Xen_Arg, ArgNum, Caller, "an integer");} while (0)

#define Xen_real_to_C_double_with_caller(Xen_Arg, Caller) Xen_real_to_C_double(Xen_Arg)

#if (HAVE_FORTH) || (HAVE_RUBY)
  #define Xen_object_ref_checked(Obj, Type) (Xen_c_object_is_type(Obj, Type) ? Xen_object_ref(Obj) : NULL)
#else
  #define Xen_object_ref_checked(Obj, Type) NULL
#endif

#else
#define imported_s7_object_value_checked(Obj, Typ) s7_object_value_checked(Obj, Typ)

#define Xen_real_to_C_double_if_bound(Xen_Arg, C_Val, Caller, ArgNum) if (Xen_is_bound(Xen_Arg)) C_Val = (double)s7_number_to_real_with_caller(s7, Xen_Arg, Caller)
#define Xen_to_C_double_or_error(Xen_Arg, C_Val, Caller, ArgNum) C_Val = (double)s7_number_to_real_with_caller(s7, Xen_Arg, Caller)
#define Xen_to_C_integer_or_error(Xen_Arg, C_Val, Caller, ArgNum) C_Val = s7_number_to_integer_with_caller(s7, Xen_Arg, Caller)
#define Xen_real_to_C_double_with_caller(Xen_Arg, Caller) s7_number_to_real_with_caller(s7, Xen_Arg, Caller)

#define Xen_object_ref_checked(Obj, Type) imported_s7_object_value_checked(Obj, Type)
#define XEN_NULL NULL
#endif


static int local_error_type = MUS_NO_ERROR;
static char *local_error_msg = NULL;

static void local_mus_error(int type, char *msg)
{
  local_error_type = type;
  if (local_error_msg) free(local_error_msg);
  local_error_msg = mus_strdup(msg);
}


static Xen clm_mus_error(int type, const char *msg, const char *caller)
{
  /* mus_error returns an int, which is a bother in this context */
  mus_error(type, "%s: %s", caller, msg);
  return(Xen_false);
}


#define CLM_ERROR Xen_make_error_type("mus-error")

static void clm_error(const char *caller, const char *msg, Xen val)
{
  Xen_error(CLM_ERROR,
	    Xen_list_4(C_string_to_Xen_string("~A: ~A ~A"),
		       C_string_to_Xen_string(caller),
		       C_string_to_Xen_string(msg),
		       val));
}



/* ---------------- optional-key ---------------- */

int mus_optkey_unscramble(const char *caller, int nkeys, Xen *keys, Xen *args, int *orig)
{
  /* implement the &optional-key notion in CLM */
  /* "keys" holds the keywords the calling function accepts, 
   *   upon return, if a key was given in the arglist or its position had a value, the corresponding value is in its keys location
   * "nkeys is the size of "keys"
   * "args" contains the original arguments passed to the function in order
   *   it should be of size nkeys * 2, and any trailing (unspecified) args should be Xen_undefined
   * "orig" should be of size nkeys, and will contain upon return the 1-based location of the original keyword value argument
   *  (it is intended for error reports)
   */
  int arg_ctr = 0, key_start = 0, rtn_ctr = 0, nargs, nargs_end;
  bool keying = false, key_found = false;
  nargs = nkeys * 2;
  nargs_end = nargs - 1;

  while ((arg_ctr < nargs) && 
	 (Xen_is_bound(args[arg_ctr])))
    {
      Xen key;
      key = args[arg_ctr];
      if (!(Xen_is_keyword(key)))
	{
	  if (keying) 
	    clm_error(caller, "unmatched value within keyword section?", key);
	  /* type checking on the actual values has to be the caller's problem */

	  if (arg_ctr >= nkeys) /* we aren't handling a keyword arg, so the underlying args should only take nkeys args */
	    clm_error(caller, "extra trailing args?", key);

	  keys[arg_ctr] = key;
	  orig[arg_ctr] = arg_ctr + 1;
	  arg_ctr++;
	  key_start = arg_ctr;
	  rtn_ctr++;
	}
      else
	{
	  int i;
	  Xen val;
	  val = args[arg_ctr + 1];
	  if ((arg_ctr == nargs_end) ||
	      (!(Xen_is_bound(val))))
	    clm_error(caller, "keyword without value?", key);

	  if (Xen_is_keyword(val))
	    clm_error(caller, "two keywords in a row?", key);

	  keying = true;
	  key_found = false;
	  for (i = key_start; i < nkeys; i++)
	    {
	      if (Xen_keyword_is_eq(keys[i], key))
		{
		  keys[i] = val;
		  arg_ctr += 2;
		  orig[i] = arg_ctr;
		  rtn_ctr++;
		  key_found = true;
		  break;
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


static mus_float_t optkey_float_error(Xen key, int n, const char *caller)
{
  Xen_check_type(false, key, n, caller, "a number");
  return(0.0);
}

#define Xen_optkey_to_float(Original_key, Key, Caller, N, Def) \
  ((Xen_keyword_is_eq(Original_key, Key)) ? Def : ((Xen_is_number(Key)) ? Xen_real_to_C_double(Key) : optkey_float_error(Key, N, Caller)))

mus_float_t mus_optkey_to_float(Xen key, const char *caller, int n, mus_float_t def)
{
  if (Xen_is_number(key))
    return(Xen_real_to_C_double(key));
  if (!(Xen_is_keyword(key)))
    Xen_check_type(false, key, n, caller, "a number");
  return(def);
}


static int optkey_int_error(Xen key, int n, const char *caller)
{
  Xen_check_type(false, key, n, caller, "an integer");
  return(0);
}

#define Xen_optkey_to_int(Original_key, Key, Caller, N, Def) \
  ((Xen_keyword_is_eq(Original_key, Key)) ? Def : ((Xen_is_integer(Key)) ? Xen_integer_to_C_int(Key) : optkey_int_error(Key, N, Caller)))

int mus_optkey_to_int(Xen key, const char *caller, int n, int def)
{
  if (Xen_is_integer(key))
    return(Xen_integer_to_C_int(key));
  if (!(Xen_is_keyword(key)))
    Xen_check_type(false, key, n, caller, "an integer");
  return(def);
}


bool mus_optkey_to_bool(Xen key, const char *caller, int n, bool def)
{
  if (Xen_is_boolean(key))
    return(Xen_boolean_to_C_bool(key));
  if (!(Xen_is_keyword(key)))
    Xen_check_type(false, key, n, caller, "#f or #t");
  return(def);
}


static mus_long_t optkey_llong_error(Xen key, int n, const char *caller)
{
  Xen_check_type(false, key, n, caller, "an integer");
  return(0);
}

#define Xen_optkey_to_mus_long_t(Original_key, Key, Caller, N, Def) \
  ((Xen_keyword_is_eq(Original_key, Key)) ? Def : ((Xen_is_integer(Key)) ? Xen_llong_to_C_llong(Key) : optkey_llong_error(Key, N, Caller)))

mus_long_t mus_optkey_to_mus_long_t(Xen key, const char *caller, int n, mus_long_t def)
{
  if (Xen_is_integer(key))
    return(Xen_llong_to_C_llong(key));
  if (!(Xen_is_keyword(key)))
    Xen_check_type(false, key, n, caller, "a sample number or size");
  return(def);
}


const char *mus_optkey_to_string(Xen key, const char *caller, int n, char *def)
{
  if (Xen_is_string(key))
    return(Xen_string_to_C_string(key));
  if ((!(Xen_is_keyword(key))) && (!(Xen_is_false(key))))
    Xen_check_type(false, key, n, caller, "a string");
  return(def);
}


static vct *mus_optkey_to_vct(Xen key, const char *caller, int n, vct *def)
{
  if (mus_is_vct(key))
    return(Xen_to_vct(key));
  if ((!(Xen_is_keyword(key))) && (!(Xen_is_false(key))))
    Xen_check_type(false, key, n, caller, "a " S_vct);
  return(def);
}


static bool local_arity_ok(Xen proc, int args) /* from snd-xen.c minus (inconvenient) gc protection */
{
#if HAVE_SCHEME
  return(s7_is_aritable(s7, proc, args));
#else
  Xen arity;
  int rargs;
  arity = Xen_arity(proc);
  rargs = Xen_integer_to_C_int(arity);

#if HAVE_RUBY
  return(xen_rb_arity_ok(rargs, args));
#endif

#if HAVE_FORTH
  return(rargs == args);
#endif
#endif
  return(true);
}


Xen mus_optkey_to_procedure(Xen key, const char *caller, int n, Xen def, int required_args, const char *err)
{
  /* in this case, it's faster to look for the keyword first */
  if ((!(Xen_is_keyword(key))) && 
      (!(Xen_is_false(key))))
    {
      Xen_check_type(Xen_is_procedure(key), key, n, caller, "a procedure");
      if (!(local_arity_ok(key, required_args)))
	Xen_bad_arity_error(caller, n, key, err);
      return(key);
    }
  return(def);
}



/* ---------------- clm keywords ---------------- */

static Xen kw_frequency, kw_initial_phase, kw_wave, kw_amplitude,
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
  kw_frequency =        Xen_make_keyword("frequency");
  kw_initial_phase =    Xen_make_keyword("initial-phase");
  kw_wave =             Xen_make_keyword("wave");
  kw_amplitude =        Xen_make_keyword("amplitude");
  kw_r =                Xen_make_keyword("r");
  kw_ratio =            Xen_make_keyword("ratio");
  kw_size =             Xen_make_keyword("size");
  kw_a0 =               Xen_make_keyword("a0");
  kw_a1 =               Xen_make_keyword("a1");
  kw_a2 =               Xen_make_keyword("a2");
  kw_b1 =               Xen_make_keyword("b1");
  kw_b2 =               Xen_make_keyword("b2");
  kw_max_size =         Xen_make_keyword("max-size");
  kw_input =            Xen_make_keyword("input");
  kw_srate =            Xen_make_keyword("srate");
  kw_file =             Xen_make_keyword("file");
  kw_channel =          Xen_make_keyword("channel");
  kw_start =            Xen_make_keyword("start");  /* make-readin */
  kw_initial_contents = Xen_make_keyword("initial-contents");
  kw_initial_element =  Xen_make_keyword("initial-element");
  kw_scaler =           Xen_make_keyword("scaler");
  kw_feedforward =      Xen_make_keyword("feedforward");
  kw_feedback =         Xen_make_keyword("feedback");
  kw_radius =           Xen_make_keyword("radius");
  kw_partials =         Xen_make_keyword("partials");
  kw_a =                Xen_make_keyword("a");
  kw_n =                Xen_make_keyword("n");
  kw_order =            Xen_make_keyword("order");
  kw_x_coeffs =         Xen_make_keyword("xcoeffs");
  kw_y_coeffs =         Xen_make_keyword("ycoeffs");
  kw_envelope =         Xen_make_keyword("envelope");
  kw_base =             Xen_make_keyword("base");
  kw_duration =         Xen_make_keyword("duration");
  kw_offset =           Xen_make_keyword("offset");
  kw_end =              Xen_make_keyword("end");
  kw_direction =        Xen_make_keyword("direction");
  kw_degree =           Xen_make_keyword("degree");
  kw_distance =         Xen_make_keyword("distance");
  kw_reverb =           Xen_make_keyword("reverb");
  kw_output =           Xen_make_keyword("output");
  kw_fft_size =         Xen_make_keyword("fft-size");
  kw_expansion =        Xen_make_keyword("expansion");
  kw_length =           Xen_make_keyword("length");
  kw_hop =              Xen_make_keyword("hop");
  kw_ramp =             Xen_make_keyword("ramp");
  kw_jitter =           Xen_make_keyword("jitter");
  kw_type =             Xen_make_keyword("type");
  kw_channels =         Xen_make_keyword("channels");
  kw_filter =           Xen_make_keyword("filter");
  kw_revout =           Xen_make_keyword("revout");
  kw_width =            Xen_make_keyword("width");
  kw_edit =             Xen_make_keyword("edit");
  kw_synthesize =       Xen_make_keyword("synthesize");
  kw_analyze =          Xen_make_keyword("analyze");
  kw_interp =           Xen_make_keyword("interp");
  kw_overlap =          Xen_make_keyword("overlap");
  kw_pitch =            Xen_make_keyword("pitch");
  kw_distribution =     Xen_make_keyword("distribution");
  kw_coeffs =           Xen_make_keyword("coeffs");
  kw_kind =             Xen_make_keyword("kind");
}



/* ---------------- *clm-table-size* ---------------- */

static mus_long_t clm_table_size = MUS_CLM_DEFAULT_TABLE_SIZE;
#if HAVE_SCHEME
  static s7_pointer clm_table_size_symbol;
#endif

mus_long_t clm_default_table_size_c(void) {return(clm_table_size);}

static Xen g_clm_table_size(void) {return(C_llong_to_Xen_llong(clm_table_size));}

static Xen g_set_clm_table_size(Xen val) 
{
  mus_long_t size;
  #define H_clm_table_size "(" S_clm_table_size "): the default table size for most generators (512)"
  Xen_check_type(Xen_is_llong(val), val, 1, S_setB S_clm_table_size, "an integer");
  size = Xen_llong_to_C_llong(val);
  if ((size <= 0) || 
      (size > mus_max_table_size()))
    Xen_out_of_range_error(S_setB S_clm_table_size, 1, val, "invalid size (see mus-max-table-size)");
  clm_table_size = size;
#if HAVE_SCHEME
  s7_symbol_set_value(s7, clm_table_size_symbol, s7_make_integer(s7, clm_table_size));
#endif
  return(C_llong_to_Xen_llong(clm_table_size));
}


/* ---------------- *clm-default-frequency* ---------------- */

static mus_float_t clm_default_frequency = MUS_CLM_DEFAULT_FREQUENCY;
#if HAVE_SCHEME
  static s7_pointer clm_default_frequency_symbol;
#endif

mus_float_t clm_default_frequency_c(void) {return(clm_default_frequency);}

static Xen g_clm_default_frequency(void) {return(C_double_to_Xen_real(clm_default_frequency));}

static Xen g_set_clm_default_frequency(Xen val) 
{
  #define H_clm_default_frequency "(" S_clm_default_frequency "): the default frequency for most generators (0.0)"
  Xen_check_type(Xen_is_double(val), val, 1, S_setB S_clm_default_frequency, "a number");
  clm_default_frequency = Xen_real_to_C_double(val);
#if HAVE_SCHEME
  s7_symbol_set_value(s7, clm_default_frequency_symbol, s7_make_real(s7, clm_default_frequency));
#endif
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


static Xen g_mus_file_buffer_size(void)
{
  #define H_mus_file_buffer_size "(" S_mus_file_buffer_size "): current CLM IO buffer size (default is 8192)"
  return(C_llong_to_Xen_llong(mus_file_buffer_size()));
}


#if HAVE_SCHEME
  static s7_pointer mus_file_buffer_size_symbol;
#endif

static Xen g_mus_set_file_buffer_size(Xen val)
{
  mus_long_t len;
  Xen_check_type(Xen_is_llong(val), val, 1, S_setB S_mus_file_buffer_size, "an integer");
  len = Xen_llong_to_C_llong(val);
  if (len <= 0) 
    Xen_out_of_range_error(S_setB S_mus_file_buffer_size, 1, val, "must be > 0");
  mus_set_file_buffer_size(len);
#if HAVE_SCHEME
  s7_symbol_set_value(s7, mus_file_buffer_size_symbol, s7_make_integer(s7, len));
#endif
  return(val);
}


static Xen g_radians_to_hz(Xen val) 
{
  #define H_radians_to_hz "(" S_radians_to_hz " rads): convert radians per sample to frequency in Hz: rads * srate / (2 * pi)"
  mus_float_t x;
  Xen_to_C_double_or_error(val, x, S_radians_to_hz, 1);
  return(C_double_to_Xen_real(mus_radians_to_hz(x)));
}


static Xen g_hz_to_radians(Xen val) 
{
  #define H_hz_to_radians "(" S_hz_to_radians " hz): convert frequency in Hz to radians per sample: hz * 2 * pi / srate"
  mus_float_t x;
  Xen_to_C_double_or_error(val, x, S_hz_to_radians, 1);
  return(C_double_to_Xen_real(mus_hz_to_radians(x)));
}


static Xen g_radians_to_degrees(Xen val) 
{
  #define H_radians_to_degrees "(" S_radians_to_degrees " rads): convert radians to degrees: rads * 360 / (2 * pi)"
  mus_float_t x;
  Xen_to_C_double_or_error(val, x, S_radians_to_degrees, 1);
  return(C_double_to_Xen_real(mus_radians_to_degrees(x)));
}


static Xen g_degrees_to_radians(Xen val) 
{
  #define H_degrees_to_radians "(" S_degrees_to_radians " deg): convert degrees to radians: deg * 2 * pi / 360"
  mus_float_t x;
  Xen_to_C_double_or_error(val, x, S_degrees_to_radians, 1);
  return(C_double_to_Xen_real(mus_degrees_to_radians(x)));
}


static Xen g_db_to_linear(Xen val) 
{
  #define H_db_to_linear "(" S_db_to_linear " db): convert decibel value db to linear value: pow(10, db / 20)"
  mus_float_t x;
  Xen_to_C_double_or_error(val, x, S_db_to_linear, 1);
  return(C_double_to_Xen_real(mus_db_to_linear(x)));
}


static Xen g_linear_to_db(Xen val) 
{
  #define H_linear_to_db "(" S_linear_to_db " lin): convert linear value to decibels: 20 * log10(lin)"
  mus_float_t x;
  Xen_to_C_double_or_error(val, x, S_linear_to_db, 1);
  return(C_double_to_Xen_real(mus_linear_to_db(x)));
}


static Xen g_even_weight(Xen val) 
{
  #define H_even_weight "(" S_even_weight " x): return the even weight of x"
  mus_float_t x;
  Xen_to_C_double_or_error(val, x, S_even_weight, 1);
  return(C_double_to_Xen_real(mus_even_weight(x)));
}


static Xen g_odd_weight(Xen val) 
{
  #define H_odd_weight "(" S_odd_weight " x): return the odd weight of x"
  mus_float_t x;
  Xen_to_C_double_or_error(val, x, S_odd_weight, 1);
  return(C_double_to_Xen_real(mus_odd_weight(x)));
}


static Xen g_even_multiple(Xen val1, Xen val2) 
{
  #define H_even_multiple "(" S_even_multiple " x y): return the even multiple of x and y"
  mus_float_t x, y;
  Xen_to_C_double_or_error(val1, x, S_even_multiple, 1);
  Xen_to_C_double_or_error(val2, y, S_even_multiple, 2);
  return(C_double_to_Xen_real(mus_even_multiple(x, y)));
}


static Xen g_odd_multiple(Xen val1, Xen val2) 
{
  #define H_odd_multiple "(" S_odd_multiple " x y): return the odd multiple of x and y"
  mus_float_t x, y;
  Xen_to_C_double_or_error(val1, x, S_odd_multiple, 1);
  Xen_to_C_double_or_error(val2, y, S_odd_multiple, 2);
  return(C_double_to_Xen_real(mus_odd_multiple(x, y)));
}


static Xen g_seconds_to_samples(Xen val) 
{
  #define H_seconds_to_samples "(" S_seconds_to_samples " secs): use " S_mus_srate " to convert seconds to samples"
  mus_float_t x;
  Xen_to_C_double_or_error(val, x, S_seconds_to_samples, 1);
  return(C_llong_to_Xen_llong(mus_seconds_to_samples(x)));
}


static Xen g_samples_to_seconds(Xen val) 
{
  #define H_samples_to_seconds "(" S_samples_to_seconds " samps): use " S_mus_srate " to convert samples to seconds"
  Xen_check_type(Xen_is_llong(val), val, 1, S_samples_to_seconds, "a number");
  return(C_double_to_Xen_real(mus_samples_to_seconds(Xen_llong_to_C_llong(val))));
}


#if HAVE_SCHEME
  static s7_pointer clm_srate_symbol;
#endif

static Xen g_mus_srate(void) 
{
  #define H_mus_srate "(" S_mus_srate "): current sampling rate"
  return(C_double_to_Xen_real(mus_srate()));
}


static Xen g_mus_set_srate(Xen val) 
{
  mus_float_t sr;
  Xen_check_type(Xen_is_number(val), val, 1, S_setB S_mus_srate, "a number");
  sr = Xen_real_to_C_double(val);
  if (sr != mus_srate())
    {
      if (sr <= 0.0) 
	Xen_out_of_range_error(S_setB S_mus_srate, 1, val, "must be > 0.0");
      mus_set_srate(sr);
#if HAVE_SCHEME
      s7_symbol_set_value(s7, clm_srate_symbol, s7_make_real(s7, sr));
#endif
    }
  return(val);
}


#if HAVE_SCHEME
  static s7_pointer mus_float_equal_fudge_factor_symbol;
#endif

static Xen g_mus_float_equal_fudge_factor(void) 
{
  #define H_mus_float_equal_fudge_factor "(" S_mus_float_equal_fudge_factor "): floating point equality fudge factor"
  return(C_double_to_Xen_real(mus_float_equal_fudge_factor()));
}


static Xen g_mus_set_float_equal_fudge_factor(Xen val) 
{
  mus_float_t factor;
  Xen_check_type(Xen_is_number(val), val, 1, S_setB S_mus_float_equal_fudge_factor, "a number");
  factor = Xen_real_to_C_double(val);
  if (factor != mus_float_equal_fudge_factor())
    {
      mus_set_float_equal_fudge_factor(factor);
#if HAVE_SCHEME
      s7_symbol_set_value(s7, mus_float_equal_fudge_factor_symbol, s7_make_real(s7, factor));
#endif
    }
  return(val);
}


#if HAVE_SCHEME
  static s7_pointer mus_array_print_length_symbol;
#endif

static Xen g_mus_array_print_length(void) 
{
  #define H_mus_array_print_length "(" S_mus_array_print_length "): current clm array print length (default is 8).  This \
affects error reporting and generator descriptions.  Array (" S_vct ") elements beyond this length are represented by '...'"
  return(C_int_to_Xen_integer(mus_array_print_length()));
}


static Xen g_mus_set_array_print_length(Xen val) 
{
  int len;
  Xen_check_type(Xen_is_integer(val), val, 1, S_setB S_mus_array_print_length, "an integer");
  len = Xen_integer_to_C_int(val);
  if (len != mus_array_print_length())
    {
      if (len < 0)
	Xen_out_of_range_error(S_setB S_mus_array_print_length, 1, val, "must be >= 0");
      mus_set_array_print_length(len);
#if HAVE_SCHEME
      s7_symbol_set_value(s7, mus_array_print_length_symbol, s7_make_integer(s7, len));
#endif
    }
  return(val);
}


static Xen g_ring_modulate(Xen val1, Xen val2) 
{
  #define H_ring_modulate "(" S_ring_modulate " s1 s2): s1 * s2 (sample by sample multiply)"
  Xen_check_type(Xen_is_number(val1), val1, 1, S_ring_modulate, "a number");
  Xen_check_type(Xen_is_number(val2), val2, 2, S_ring_modulate, "a number");
  return(C_double_to_Xen_real(mus_ring_modulate(Xen_real_to_C_double(val1), Xen_real_to_C_double(val2))));
}


static Xen g_amplitude_modulate(Xen val1, Xen val2, Xen val3) 
{
  #define H_amplitude_modulate "(" S_amplitude_modulate " carrier in1 in2): in1 * (carrier + in2)"
  Xen_check_type(Xen_is_number(val1), val1, 1, S_amplitude_modulate, "a number");
  Xen_check_type(Xen_is_number(val2), val2, 2, S_amplitude_modulate, "a number");
  Xen_check_type(Xen_is_number(val3), val3, 3, S_amplitude_modulate, "a number");
  return(C_double_to_Xen_real(mus_amplitude_modulate(Xen_real_to_C_double(val1), Xen_real_to_C_double(val2), Xen_real_to_C_double(val3))));
}


static Xen g_contrast_enhancement(Xen val1, Xen val2) 
{
  mus_float_t index = 1.0; /* this is the default in clm.html and mus.lisp */
  #define H_contrast_enhancement "(" S_contrast_enhancement " sig (index 1.0)): sin(sig * pi / 2 + index * sin(sig * 2 * pi))"
  Xen_check_type(Xen_is_number(val1), val1, 1, S_contrast_enhancement, "a number");
  if (Xen_is_bound(val2))
    {
      Xen_check_type(Xen_is_number(val2), val2, 2, S_contrast_enhancement, "a number");
      index = Xen_real_to_C_double(val2);
    }
  return(C_double_to_Xen_real(mus_contrast_enhancement(Xen_real_to_C_double(val1), index)));
}


static Xen g_dot_product(Xen val1, Xen val2, Xen size) 
{
  #define H_dot_product "(" S_dot_product " v1 v2 (size)): sum of v1[i] * v2[i] (also named scalar product)"
  vct *v1, *v2;
  mus_long_t len;  

  Xen_check_type(mus_is_vct(val1), val1, 1, S_dot_product, "a " S_vct);
  Xen_check_type(mus_is_vct(val2), val2, 2, S_dot_product, "a " S_vct);
  Xen_check_type(Xen_is_llong_or_unbound(size), size, 3, S_dot_product, "an integer");

  v1 = Xen_to_vct(val1);
  v2 = Xen_to_vct(val2);
  if (Xen_is_llong(size))
    {
      len = Xen_llong_to_C_llong(size);
      if (len == 0) return(C_double_to_Xen_real(0.0));
      if (len < 0)
	Xen_out_of_range_error(S_dot_product, 3, size, "size < 0?");
      if (len > mus_vct_length(v1)) len = mus_vct_length(v1);
    }
  else len = mus_vct_length(v1); 
  if (len > mus_vct_length(v2)) len = mus_vct_length(v2);

  return(C_double_to_Xen_real(mus_dot_product(mus_vct_data(v1), mus_vct_data(v2), len)));
}


#if HAVE_COMPLEX_TRIG && HAVE_COMPLEX_NUMBERS && (!HAVE_RUBY)

#if defined(__sun) && defined(__SVR4)
  #undef _Complex_I
  #define _Complex_I 1.0fi
#endif

#define S_edot_product "edot-product"

static Xen g_edot_product(Xen val1, Xen val2) 
{
  #define H_edot_product "(" S_edot_product " freq data): sum of (e^freq*i) * data[i]"
  mus_long_t i, len;
  vct *v = NULL;
  complex double freq;
  complex double *vals;
  Xen result;

  Xen_check_type(Xen_is_complex(val1), val1, 1, S_edot_product, "complex");
  Xen_check_type((mus_is_vct(val2)) || (Xen_is_vector(val2)), val2, 2, S_edot_product, "a " S_vct);

  freq = Xen_complex_to_C_complex(val1);
  if (mus_is_vct(val2))
    {
      v = Xen_to_vct(val2);
      len = mus_vct_length(v);
    }
  else
    {
      len = Xen_vector_length(val2);
    }
  vals = (complex double *)calloc(len, sizeof(complex double));
  if (mus_is_vct(val2))
    {
      mus_float_t *vdata;
      vdata = mus_vct_data(v);
      for (i = 0; i < len; i++)
	vals[i] = vdata[i];
    }
  else
    {
      for (i = 0; i < len; i++)
	vals[i] = Xen_complex_to_C_complex(Xen_vector_ref(val2, i));
    }
  result = C_complex_to_Xen_complex(mus_edot_product(freq, vals, len));
  free(vals);
  return(result);
}
#endif

typedef enum {G_RECTANGULAR_POLAR, G_POLAR_RECTANGULAR, G_RECTANGULAR_MAGNITUDES} xclm_window_t;

static Xen g_fft_window_1(xclm_window_t choice, Xen val1, Xen val2, Xen ulen, const char *caller) 
{
  vct *v1, *v2;
  mus_long_t len;

  Xen_check_type(mus_is_vct(val1), val1, 1, caller, "a " S_vct);
  Xen_check_type(mus_is_vct(val2), val2, 2, caller, "a " S_vct);
  Xen_check_type(Xen_is_llong_or_unbound(ulen), ulen, 3, caller, "an integer");

  v1 = Xen_to_vct(val1);
  v2 = Xen_to_vct(val2);
  if (Xen_is_llong(ulen))
    {
      len = Xen_llong_to_C_llong(ulen);
      if (len == 0) return(Xen_false);
      if (len < 0)
	Xen_out_of_range_error(caller, 3, ulen, "size < 0?");
      if (len > mus_vct_length(v1)) len = mus_vct_length(v1);
    }
  else len = mus_vct_length(v1); 
  if (len > mus_vct_length(v2)) len = mus_vct_length(v2);
  switch (choice)
    {
    case G_RECTANGULAR_POLAR:      mus_rectangular_to_polar(mus_vct_data(v1), mus_vct_data(v2), len);      break;
    case G_RECTANGULAR_MAGNITUDES: mus_rectangular_to_magnitudes(mus_vct_data(v1), mus_vct_data(v2), len); break;
    case G_POLAR_RECTANGULAR:      mus_polar_to_rectangular(mus_vct_data(v1), mus_vct_data(v2), len);      break;
    }
  return(val1);
}


static Xen g_rectangular_to_polar(Xen val1, Xen val2) 
{
  #define H_rectangular_to_polar "(" S_rectangular_to_polar " rl im): convert real/imaginary \
data in " S_vct "s rl and im from rectangular form (fft output) to polar form (a spectrum)"

  return(g_fft_window_1(G_RECTANGULAR_POLAR, val1, val2, Xen_undefined, S_rectangular_to_polar));
}


static Xen g_rectangular_to_magnitudes(Xen val1, Xen val2) 
{
  #define H_rectangular_to_magnitudes "(" S_rectangular_to_magnitudes " rl im): convert real/imaginary \
data in " S_vct "s rl and im from rectangular form (fft output) to polar form, but ignore the phases"

  return(g_fft_window_1(G_RECTANGULAR_MAGNITUDES, val1, val2, Xen_undefined, S_rectangular_to_magnitudes));
}


static Xen g_polar_to_rectangular(Xen val1, Xen val2) 
{
  #define H_polar_to_rectangular "(" S_polar_to_rectangular " rl im): convert real/imaginary \
data in " S_vct "s rl and im from polar (spectrum) to rectangular (fft)"

  return(g_fft_window_1(G_POLAR_RECTANGULAR, val1, val2, Xen_undefined, S_polar_to_rectangular));
}


static Xen g_mus_fft(Xen url, Xen uim, Xen len, Xen usign)
{
  #define H_mus_fft "(" S_mus_fft " rl im (len) (dir 1)): return the fft of " S_vct "s rl and im which contain \
the real and imaginary parts of the data; len should be a power of 2, dir = 1 for fft, -1 for inverse-fft"

  int sign;
  mus_long_t n;
  vct *v1, *v2;

  Xen_check_type((mus_is_vct(url)), url, 1, S_mus_fft, "a " S_vct);
  Xen_check_type((mus_is_vct(uim)), uim, 2, S_mus_fft, "a " S_vct);

  v1 = Xen_to_vct(url);
  v2 = Xen_to_vct(uim);

  if (Xen_is_integer(usign)) 
    sign = Xen_integer_to_C_int(usign); 
  else sign = 1;

  if (Xen_is_llong(len)) 
    {
      n = Xen_llong_to_C_llong(len); 
      if (n <= 0)
	Xen_out_of_range_error(S_mus_fft, 3, len, "size <= 0?");
      if (n > mus_max_malloc())
	Xen_out_of_range_error(S_mus_fft, 3, len, "size too large (see mus-max-malloc)");
      if (n > mus_vct_length(v1))
	n = mus_vct_length(v1);
    }
  else n = mus_vct_length(v1);

  if (n > mus_vct_length(v2))
    n = mus_vct_length(v2);

  if (!(is_power_of_2(n)))
    {
      mus_float_t nf;
      int np;
      nf = (log(n) / log(2.0));
      np = (int)nf;
      n = (mus_long_t)pow(2.0, np);
    }

  if (n > 0)
    mus_fft(mus_vct_data(v1), mus_vct_data(v2), n, sign);
  /*
   * in fftw, there's the extra complex array allocation, so for n = 2^29
   *   (and doubles for vcts as well as fftw), we need 24.6 Gbytes, and the FFT
   *   takes 144 secs on a 2.4 GHz machine.  (Similarly, 2^28 needs 12.6 Gb
   *   and takes 61 secs).  
   */

  return(url);
}


static Xen g_make_fft_window(Xen type, Xen size, Xen ubeta, Xen ualpha)
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

  #define H_make_fft_window "(" S_make_fft_window " type size (beta 0.0) (alpha 0.0)): -> fft data window (a " S_vct "). \
type is one of the sndlib fft window identifiers such as " S_kaiser_window ", beta \
is the window family parameter, if any:\n  " make_window_example

  mus_float_t beta = 0.0, alpha = 0.0;
  mus_long_t n;
  int fft_window;
  mus_float_t *data;

  Xen_check_type(Xen_is_integer(type), type, 1, S_make_fft_window, "an integer (window type)");
  Xen_check_type(Xen_is_llong(size), size, 2, S_make_fft_window, "an integer");

  if (Xen_is_number(ubeta)) beta = Xen_real_to_C_double(ubeta);
  if (Xen_is_number(ualpha)) alpha = Xen_real_to_C_double(ualpha);

  n = Xen_llong_to_C_llong(size);
  if (n <= 0)
    Xen_out_of_range_error(S_make_fft_window, 2, size, "size <= 0?");
  if (n > mus_max_malloc())
    Xen_out_of_range_error(S_make_fft_window, 2, size, "size too large (see mus-max-malloc)");

  fft_window = Xen_integer_to_C_int(type);
  if (!(mus_is_fft_window(fft_window)))
    Xen_out_of_range_error(S_make_fft_window, 1, type, "unknown fft window");

  data = (mus_float_t *)malloc(n * sizeof(mus_float_t));
  mus_make_fft_window_with_window((mus_fft_window_t)fft_window, n, beta, alpha, data);
  return(xen_make_vct(n, data));
}


static Xen g_spectrum(Xen url, Xen uim, Xen uwin, Xen utype)
{
  #define H_mus_spectrum "(" S_spectrum " rl im window (type 1)): \
real and imaginary data in " S_vct "s rl and im, returns (in rl) the spectrum thereof; \
window is the fft data window (a " S_vct " as returned by " S_make_fft_window "), \
and type determines how the spectral data is scaled:\n\
  0 = data in dB,\n\
  1 (default) = linear and normalized\n\
  2 = linear and un-normalized."

  int type;
  mus_long_t n;
  vct *v1, *v2, *v3 = NULL;

  Xen_check_type((mus_is_vct(url)), url, 1, S_spectrum, "a " S_vct);
  Xen_check_type((mus_is_vct(uim)), uim, 2, S_spectrum, "a " S_vct);
  if (!Xen_is_false(uwin)) Xen_check_type((mus_is_vct(uwin)), uwin, 3, S_spectrum, "a " S_vct " or " PROC_FALSE);

  v1 = Xen_to_vct(url);
  v2 = Xen_to_vct(uim);
  if (!Xen_is_false(uwin)) v3 = Xen_to_vct(uwin);

  n = mus_vct_length(v1);
  if (n > mus_vct_length(v2))
    n = mus_vct_length(v2);
  if ((v3) && (n > mus_vct_length(v3)))
    n = mus_vct_length(v3);

  if (!(is_power_of_2(n)))
    {
      mus_float_t nf;
      int np;
      nf = (log(n) / log(2.0));
      np = (int)nf;
      n = (int)pow(2.0, np);
    }

  if (Xen_is_integer(utype)) 
    type = Xen_integer_to_C_int(utype);
  else type = 1; /* linear normalized */
  if ((type < 0) || (type > 2))
    Xen_out_of_range_error(S_spectrum, 4, utype, "type must be 0..2");
  
  if (n > 0)
    mus_spectrum(mus_vct_data(v1), mus_vct_data(v2), (v3) ? (mus_vct_data(v3)) : NULL, n, (mus_spectrum_t)type);
  return(url);
}


static Xen g_autocorrelate(Xen reals)
{
  #define H_autocorrelate "(" S_autocorrelate " data): in place autocorrelation of data (a " S_vct ")"
  /* assumes length is power of 2 */
  vct *v1 = NULL;
  Xen_check_type(mus_is_vct(reals), reals, 1, S_autocorrelate, "a " S_vct);
  v1 = Xen_to_vct(reals);
  if (mus_vct_length(v1) > 0)
    mus_autocorrelate(mus_vct_data(v1), mus_vct_length(v1));
  return(reals);
}


static Xen g_correlate(Xen data1, Xen data2)
{
  #define H_correlate "(" S_correlate " data1 data2): in place cross-correlation of data1 and data2 (both " S_vct "s)"
  mus_long_t size;
  vct *v1 = NULL, *v2 = NULL;

  Xen_check_type(mus_is_vct(data1), data1, 1, S_correlate, "a " S_vct);
  Xen_check_type(mus_is_vct(data2), data2, 2, S_correlate, "a " S_vct);

  v1 = Xen_to_vct(data1);
  v2 = Xen_to_vct(data2);
  if (mus_vct_length(v1) < mus_vct_length(v2))
    size = mus_vct_length(v1);
  else size = mus_vct_length(v2);

  if (size > 0)
    mus_correlate(mus_vct_data(v1), mus_vct_data(v2), size);
  return(data1);
}


static Xen g_convolution(Xen url1, Xen url2, Xen un)
{
  #define H_mus_convolution "(" S_convolution " v1 v2 (len)): convolution \
of " S_vct "s v1 with v2, using fft of size len (a power of 2), result in v1"

  mus_long_t n;
  vct *v1, *v2;

  Xen_check_type((mus_is_vct(url1)), url1, 1, S_convolution, "a " S_vct);
  Xen_check_type((mus_is_vct(url2)), url2, 2, S_convolution, "a " S_vct);

  v1 = Xen_to_vct(url1);
  v2 = Xen_to_vct(url2);

  if (Xen_is_integer(un)) 
    {
      n = Xen_llong_to_C_llong(un); 
      if (n <= 0)
	Xen_out_of_range_error(S_convolution, 3, un, "size <= 0?");
      if (n > mus_max_malloc())
	Xen_out_of_range_error(S_convolution, 3, un, "size too large (see mus-max-malloc)");
      if (n > mus_vct_length(v1))
	n = mus_vct_length(v1);
    }
  else n = mus_vct_length(v1);
  if (n > mus_vct_length(v2))
    n = mus_vct_length(v2);
  if (!(is_power_of_2(n)))
    {
      mus_float_t nf;
      int np;
      nf = (log(n) / log(2.0));
      np = (int)nf;
      n = (int)pow(2.0, np);
    }
  if (n > 0)
    mus_convolution(mus_vct_data(v1), mus_vct_data(v2), n);
  return(url1);
}


static Xen g_polynomial(Xen arr, Xen x)
{
  #define H_polynomial "(" S_polynomial " coeffs x): evaluate a polynomial at x.  coeffs are in order \
of degree, so coeff[0] is the constant term."

#if (!HAVE_SCHEME)
  Xen_check_type(Xen_is_number(x), x, 2, S_polynomial, "a number");
#endif
  if (mus_is_vct(arr))
    {
      vct *v;
      v = Xen_to_vct(arr);
      return(C_double_to_Xen_real(mus_polynomial(mus_vct_data(v), Xen_real_to_C_double_with_caller(x, S_polynomial), mus_vct_length(v))));
    }

  Xen_check_type(Xen_is_vector(arr), arr, 1, S_polynomial, "a vector or " S_vct);
  {
    mus_float_t sum, cx;
    int i, ncoeffs;

    ncoeffs = Xen_vector_length(arr);
    if (ncoeffs <= 0) return(C_double_to_Xen_real(0.0));
    if (ncoeffs == 1) return(Xen_vector_ref(arr, 0)); /* just a constant term */

    cx = Xen_real_to_C_double_with_caller(x, S_polynomial);
    sum = Xen_real_to_C_double_with_caller(Xen_vector_ref(arr, ncoeffs - 1), S_polynomial);
    for (i = ncoeffs - 2; i >= 0; i--) 
      sum = (sum * cx) + Xen_real_to_C_double_with_caller(Xen_vector_ref(arr, i), S_polynomial);
    return(C_double_to_Xen_real(sum));
  }
}


static Xen g_array_interp(Xen obj, Xen phase, Xen size) /* opt size */
{
  #define H_array_interp "(" S_array_interp " v phase (size)): v[phase] \
taking into account wrap-around (size is size of data), with linear interpolation if phase is not an integer."

  mus_long_t len;
  vct *v;

  Xen_check_type(mus_is_vct(obj), obj, 1, S_array_interp, "a " S_vct);
#if (!HAVE_SCHEME)
  Xen_check_type(Xen_is_number(phase), phase, 2, S_array_interp, "a number");
#endif
  Xen_check_type(Xen_is_llong_or_unbound(size), size, 3, S_array_interp, "an integer");

  v = Xen_to_vct(obj);
  if (Xen_is_bound(size)) 
    {
      len = Xen_llong_to_C_llong(size); 
      if (len <= 0)
	Xen_out_of_range_error(S_array_interp, 3, size, "size <= 0?");
      if (len > mus_vct_length(v)) 
	len = mus_vct_length(v);
    }
  else len = mus_vct_length(v);
  if (len == 0)
    return(C_double_to_Xen_real(0.0));
  return(C_double_to_Xen_real(mus_array_interp(mus_vct_data(v), Xen_real_to_C_double_with_caller(phase, S_array_interp), len)));
}


static Xen g_mus_interpolate(Xen type, Xen x, Xen obj, Xen size, Xen yn1)
{
  #define H_mus_interpolate "(" S_mus_interpolate " type x v (size) (yn1 0.0)): interpolate in \
data ('v' is a " S_vct ") using interpolation 'type', such as " S_mus_interp_linear "."

  mus_long_t len;
  int itype;
  vct *v;
  mus_float_t y = 0.0;

  Xen_check_type(Xen_is_integer(type), type, 1, S_mus_interpolate, "an integer (interp type such as " S_mus_interp_all_pass ")");
  Xen_check_type(Xen_is_number(x), x, 2, S_mus_interpolate, "a number");
  Xen_check_type(mus_is_vct(obj), obj, 3, S_mus_interpolate, "a " S_vct);
  Xen_check_type(Xen_is_llong_or_unbound(size), size, 4, S_mus_interpolate, "an integer");
  Xen_check_type(Xen_is_number_or_unbound(yn1), yn1, 5, S_mus_interpolate, "a number");

  itype = Xen_integer_to_C_int(type);
  if (!(mus_is_interp_type(itype)))
    Xen_out_of_range_error(S_mus_interpolate, 1, type, "unknown interp type");

  v = Xen_to_vct(obj);

  if (Xen_is_bound(size)) 
    {
      len = Xen_llong_to_C_llong(size); 
      if (len <= 0)
	Xen_out_of_range_error(S_mus_interpolate, 4, size, "size <= 0?");
      if (len > mus_vct_length(v)) 
	len = mus_vct_length(v);
    }
  else len = mus_vct_length(v);
  if (len == 0)
    return(C_double_to_Xen_real(0.0));

  if (Xen_is_number(yn1))
    y = Xen_real_to_C_double(yn1);

  return(C_double_to_Xen_real(mus_interpolate((mus_interp_t)itype, Xen_real_to_C_double(x), mus_vct_data(v), len, y)));
}



/* ---------------- mus-xen struct ---------------- */

static Xen_object_type_t mus_xen_tag;

bool mus_is_xen(Xen obj) {return(Xen_c_object_is_type(obj, mus_xen_tag));}

#define Xen_to_C_generator(Xen_Arg, X_Val, C_Val, Checker, Caller, Descr) \
  Xen_check_type((X_Val = (mus_xen *)Xen_object_ref_checked(Xen_Arg, mus_xen_tag)) && (Checker(C_Val = (mus_any *)mus_xen_to_mus_any(X_Val))), Xen_Arg, 1, Caller, Descr)

#define Xen_to_C_any_generator(Xen_Arg, X_Val, C_Val, Caller, Descr) \
  Xen_check_type((X_Val = (mus_xen *)Xen_object_ref_checked(Xen_Arg, mus_xen_tag)) && (C_Val = (mus_any *)mus_xen_to_mus_any(X_Val)), Xen_Arg, 1, Caller, Descr)



static Xen g_is_mus_generator(Xen obj) 
{
  #define H_is_mus_generator "(" S_is_mus_generator " obj): " PROC_TRUE " if 'obj' is a CLM generator."
  return(C_bool_to_Xen_boolean(mus_is_xen(obj)));
}


#if HAVE_SCHEME
static Xen_object_mark_t mark_mus_xen(void *obj) 
#else
static Xen_object_mark_t mark_mus_xen(Xen obj) 
#endif
{
  mus_xen *ms;
#if HAVE_RUBY || HAVE_SCHEME
  /* rb_gc_mark and scheme_mark_object pass us the actual value, not the Xen wrapper */
  ms = (mus_xen *)obj;
#endif
#if HAVE_FORTH
  ms = Xen_to_mus_xen(obj);
#endif
  if (ms->vcts) 
    {
      int i, lim;
      lim = MUS_SELF_WRAPPER;
      if (ms->nvcts < lim) lim = ms->nvcts;
      for (i = 0; i < lim; i++) 
	if (Xen_is_bound(ms->vcts[i]))
	  xen_gc_mark(ms->vcts[i]);
    }
#if HAVE_RUBY
  return(NULL);
#endif
#if (!HAVE_EXTENSION_LANGUAGE)
  return(0);
#endif
}


static void mus_xen_free(mus_xen *ms)
{
  mus_free(ms->gen);
  ms->gen = NULL;
  mx_free(ms);
}

Xen_wrap_free(mus_xen, free_mus_xen, mus_xen_free)


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


enum {G_FILTER_STATE, G_FILTER_XCOEFFS, G_FILTER_YCOEFFS};
/* G_FILTER_STATE must = MUS_DATA_WRAPPER = 0 */
enum {G_LOCSIG_DATA, G_LOCSIG_REVDATA, G_LOCSIG_OUT, G_LOCSIG_REVOUT};

static Xen mus_xen_copy(mus_xen *ms)
{
  /* return an object -> copied mus_xen -> copied mus_any gen */
  mus_xen *np;

  np = mx_alloc(ms->nvcts);
  np->gen = mus_copy(ms->gen);
  if (ms->nvcts > 0)
    {
      if (ms->nvcts == 1)
	{
	  if ((mus_is_env(np->gen)) || /* do the most common case first */
	      (mus_is_formant_bank(np->gen)))
	    np->vcts[MUS_DATA_WRAPPER] = ms->vcts[MUS_DATA_WRAPPER];
	  else
	    {
	      if ((mus_is_comb_bank(np->gen)) ||
		  (mus_is_all_pass_bank(np->gen)) ||
		  (mus_is_filtered_comb_bank(np->gen)))
		{
		  /* set up objects for new gens so that they will eventually be GC'd */
		  Xen v;
		  int i, len;
		  len = Xen_vector_length(ms->vcts[MUS_DATA_WRAPPER]);
		  v = Xen_make_vector(len, Xen_false);
		  np->vcts[MUS_DATA_WRAPPER] = v;
		  for (i = 0; i < len; i++)
		    Xen_vector_set(v, i, mus_xen_to_object(mus_any_to_mus_xen(mus_bank_generator(np->gen, i))));
		}
	      else np->vcts[MUS_DATA_WRAPPER] = xen_make_vct_wrapper(mus_length(np->gen), mus_data(np->gen));
	    }
	}
      else
	{
	  if (ms->nvcts == 2)
	    {
	      if (mus_is_pulsed_env(np->gen))
		{
		  /* mus_free taken care of by copied pulsed_env gen */
		  np->vcts[0] = Xen_false;
		  np->vcts[1] = Xen_false;
		}
	      else
		{
		  if (mus_is_filtered_comb(np->gen))
		    {
		      np->vcts[0] = xen_make_vct_wrapper(mus_length(np->gen), mus_data(np->gen));
		      np->vcts[1] = Xen_false; /* filt gen but it's not wrapped */
		    }
		  else
		    {
		      np->vcts[0] = ms->vcts[0];
		      np->vcts[1] = ms->vcts[1];
		    }
		}
	    }
	  else
	    {
	      if (ms->nvcts == 3)
		{
		  if (mus_is_oscil_bank(np->gen))
		    {
		      np->vcts[0] = ms->vcts[0];
		      np->vcts[1] = xen_make_vct_wrapper(mus_length(np->gen), mus_data(np->gen));
		      np->vcts[2] = ms->vcts[2];
		    }
		  else
		    {
		      np->vcts[G_FILTER_STATE] = xen_make_vct_wrapper(mus_length(np->gen), mus_data(np->gen));
		      np->vcts[G_FILTER_XCOEFFS] = ms->vcts[G_FILTER_XCOEFFS];
		      np->vcts[G_FILTER_YCOEFFS] = ms->vcts[G_FILTER_YCOEFFS];
		    }
		}
	      else
		{
		  int i;
		  for (i = 0; i < ms->nvcts; i++)
		    np->vcts[i] = ms->vcts[i];
		  
		  if (mus_is_granulate(np->gen))
		    np->vcts[MUS_DATA_WRAPPER] = xen_make_vct_wrapper(mus_granulate_grain_max_length(np->gen), mus_data(np->gen));
		  
		  if ((mus_is_convolve(np->gen)) ||
		      (mus_is_src(np->gen)) ||
		      (mus_is_granulate(np->gen)) ||
		      (mus_is_phase_vocoder(np->gen)))
		    {
		      Xen c_obj;
		      c_obj = mus_xen_to_object(np);
		      np->vcts[MUS_SELF_WRAPPER] = c_obj;
		      mus_generator_copy_feeders(np->gen, ms->gen);
		      return(c_obj);
		    }
		}
	    }
	}
    }
  return(mus_xen_to_object(np));
}


#if HAVE_RUBY
static Xen mus_xen_to_s(Xen obj)
{
  char *str;
  Xen result;
  str = mus_describe(Xen_to_mus_any(obj));
  result = C_string_to_Xen_string(str);
  if (str) free(str);
  return(result);
}
#endif


#if HAVE_FORTH
static Xen print_mus_xen(Xen obj)
{
  char *str;
  Xen result;
  str = mus_describe(Xen_to_mus_any(obj));
  result = fth_make_string_format("#<%s>", str);
  if (str) free(str);
  return(result);
}
#endif


#if (!HAVE_SCHEME)
static Xen equalp_mus_xen(Xen obj1, Xen obj2) 
{
  if ((!(mus_is_xen(obj1))) || (!(mus_is_xen(obj2)))) return(Xen_false);
  return(C_bool_to_Xen_boolean(mus_equalp(Xen_to_mus_any(obj1), Xen_to_mus_any(obj2))));
}
#endif


#if HAVE_RUBY || HAVE_FORTH
static Xen mus_xen_apply(Xen gen, Xen arg1, Xen arg2)
{
#if HAVE_FORTH
  Xen_check_type(mus_is_xen(gen), gen, 1, S_mus_apply, "a generator");
#endif
  return(C_double_to_Xen_real(mus_run(Xen_to_mus_any(gen),
				 (Xen_is_number(arg1)) ? Xen_real_to_C_double(arg1) : 0.0,
				 (Xen_is_number(arg2)) ? Xen_real_to_C_double(arg2) : 0.0)));
}
#endif

#if HAVE_SCHEME

/* these are for mus_xen_tag, so need not handle float-vectors */

static Xen mus_xen_apply(s7_scheme *sc, Xen gen, Xen args)
{
  if (s7_is_pair(args))
    {
      mus_float_t arg1, arg2;
      arg1 = s7_number_to_real_with_caller(sc, s7_car(args), "mus-apply");
      args = s7_cdr(args);
      if (s7_is_pair(args))
	arg2 = s7_number_to_real_with_caller(sc, s7_car(args), "mus-apply");
      else arg2 = 0.0;
      return(s7_make_real(s7, mus_run(Xen_to_mus_any(gen), arg1, arg2)));
    }
  return(s7_make_real(s7, mus_run(Xen_to_mus_any(gen), 0.0, 0.0)));
}

static Xen s7_mus_length(s7_scheme *sc, Xen obj)
{
  return(g_mus_length(obj));
}

static Xen g_mus_copy(Xen gen);
static Xen s7_mus_copy(s7_scheme *sc, Xen args)
{
  return(g_mus_copy(s7_car(args)));
}

#endif


Xen mus_xen_to_object(mus_xen *gn) /* global for user-defined gens */
{
  return(Xen_make_object(mus_xen_tag, gn, mark_mus_xen, free_mus_xen));
}


Xen mus_xen_to_object_with_vct(mus_xen *gn, Xen v) /* global for user-defined gens */
{
  gn->vcts[MUS_DATA_WRAPPER] = v;
  return(Xen_make_object(mus_xen_tag, gn, mark_mus_xen, free_mus_xen));
}


mus_any *mus_optkey_to_mus_any(Xen key, const char *caller, int n, mus_any *def)
{
  /* from Michael Scholz's sndins.c */
  if (!(Xen_is_keyword(key))) 
    {
      Xen_check_type(mus_is_xen(key), key, n, caller, "a clm generator or keyword");
      return(Xen_to_mus_any(key));
    }
  return(def);
}


static Xen mus_optkey_to_input_procedure(Xen key, const char *caller, int n, Xen def, int required_args, const char *err)
{
  if (Xen_is_procedure(key))
    {
      if (!(local_arity_ok(key, required_args)))
	Xen_bad_arity_error(caller, n, key, err);
      return(key);
    }

  if (mus_is_xen(key))
    {
      if (!(mus_is_input(Xen_to_mus_any(key))))
	Xen_wrong_type_arg_error(caller, n, key, "an input generator");
      return(key);
    }

  if ((!(Xen_is_keyword(key))) && 
      (!(Xen_is_false(key))))
    Xen_check_type(false, key, n, caller, "a procedure or input generator");
  return(def);
}



/* ---------------- wrappers ---------------- */

mus_xen *mus_any_to_mus_xen(mus_any *ge)
{
  mus_xen *gn;
  gn = mx_alloc(0);
  gn->gen = ge;
  return(gn);
}


mus_xen *mus_any_to_mus_xen_with_vct(mus_any *ge, Xen v)
{
  mus_xen *gn;
  gn = mx_alloc(1);
  gn->gen = ge;
  gn->vcts[MUS_DATA_WRAPPER] = v;
  return(gn);
}


mus_xen *mus_any_to_mus_xen_with_two_vcts(mus_any *ge, Xen v1, Xen v2)
{
  mus_xen *gn;
  gn = mx_alloc(2);
  gn->gen = ge;
  gn->vcts[MUS_DATA_WRAPPER] = v1;
  gn->vcts[MUS_INPUT_FUNCTION] = v2;
  return(gn);
}



/* ---------------- generic functions ---------------- */


static Xen g_mus_reset(Xen gen) 
{
  #define H_mus_reset "(" S_mus_reset " gen): clear out gen, setting it to its default starting state"
  mus_xen *ms;
  ms = (mus_xen *)Xen_object_ref_checked(gen, mus_xen_tag);
  if (ms)
    {
      mus_reset(ms->gen);
      return(gen);
    }
#if HAVE_SCHEME
  if (s7_is_float_vector(gen))
    {
      memset((void *)s7_float_vector_elements(gen), 0, s7_vector_length(gen) * sizeof(s7_double));
      return(gen);
    }
  {
    s7_pointer func; 
    func = s7_method(s7, gen, s7_make_symbol(s7, "mus-reset"));
    if (func != Xen_undefined) return(s7_apply_function(s7, func, s7_list(s7, 1, gen))); 
  } 
#endif
  Xen_check_type(false, gen, 1, S_mus_reset, "a generator");
  return(gen);
}

#if HAVE_SCHEME
  static s7_pointer mus_copy_symbol, copy_function;
#endif

static Xen g_mus_copy(Xen gen) 
{
  #define H_mus_copy "(" S_mus_copy " gen): return a copy of gen"
  mus_xen *ms;

  ms = (mus_xen *)Xen_object_ref_checked(gen, mus_xen_tag);
  if (ms)
    return(mus_xen_copy(ms));
#if HAVE_SCHEME
  {
    s7_pointer func; 
    func = s7_method(s7, gen, mus_copy_symbol);
    if (func == copy_function)
      return(s7_copy(s7, s7_list(s7, 1, gen)));
    if (func != Xen_undefined) 
      return(s7_apply_function(s7, func, s7_list(s7, 1, gen))); 
  }
#endif
  Xen_check_type(false, gen, 1, S_mus_copy, "a generator");
  return(gen);
}


static Xen g_mus_run(Xen gen, Xen arg1, Xen arg2) 
{
  #define H_mus_run "(" S_mus_run " gen (arg1 0.0) (arg2 0.0)): apply gen to arg1 and arg2"
  mus_xen *ms;

  ms = (mus_xen *)Xen_object_ref_checked(gen, mus_xen_tag);
  if (ms)
    {
      mus_float_t a1 = 0.0, a2 = 0.0;
      Xen_real_to_C_double_if_bound(arg1, a1, S_mus_run, 2);
      Xen_real_to_C_double_if_bound(arg2, a2, S_mus_run, 3);
      return(C_double_to_Xen_real(mus_run(ms->gen, a1, a2)));
    }
#if HAVE_SCHEME
  {
    s7_pointer func; 
    func = s7_method(s7, gen, s7_make_symbol(s7, "mus-run"));
    if (func != Xen_undefined) return(s7_apply_function(s7, func, s7_list(s7, 3, gen, arg1, arg2))); 
  }
#endif
  Xen_check_type(false, gen, 1, S_mus_run, "a generator");
  return(C_double_to_Xen_real(0.0));
}


static Xen g_mus_apply(Xen arglist)
{
  #define H_mus_apply "(" S_mus_apply " gen args...): apply gen to args"
  mus_xen *ms;
  Xen gen;
  int arglist_len;

  arglist_len = Xen_list_length(arglist);
  if ((arglist_len > 3) || (arglist_len == 0)) 
    return(C_double_to_Xen_real(0.0));

  gen = Xen_car(arglist);
  ms = (mus_xen *)Xen_object_ref_checked(gen, mus_xen_tag);
  if (ms)
    {
      mus_any *g;
      g = ms->gen;

      if (arglist_len == 1) 
	return(C_double_to_Xen_real(mus_apply(g, 0.0, 0.0)));

      if (arglist_len == 2)
	return(C_double_to_Xen_real(mus_apply(g, Xen_real_to_C_double(Xen_cadr(arglist)), 0.0)));

      return(C_double_to_Xen_real(mus_apply(g, 
					    Xen_real_to_C_double_with_caller(Xen_cadr(arglist), "mus-apply"), 
					    Xen_real_to_C_double_with_caller(Xen_caddr(arglist), "mus-apply"))));
    }
#if HAVE_SCHEME
  {
    s7_pointer func; 
    func = s7_method(s7, gen, s7_make_symbol(s7, "mus-apply"));
    if (func != Xen_undefined) return(s7_apply_function(s7, func, arglist));
  }
#endif
  Xen_check_type(false, Xen_car(arglist), 1, S_mus_apply, "a generator");
  return(C_double_to_Xen_real(0.0));
}


static Xen g_mus_describe(Xen gen) 
{
  #define H_mus_describe "(" S_mus_describe " gen): return a string describing the state of CLM generator generator"
  mus_xen *ms;

  ms = (mus_xen *)Xen_object_ref_checked(gen, mus_xen_tag);
  if (ms)
    {
      Xen result;
      char *str;
      str = mus_describe(ms->gen);
      result = C_string_to_Xen_string(str);
      if (str) free(str);
      return(result);
    }
#if HAVE_SCHEME
  {
    s7_pointer func; 
    func = s7_method(s7, gen, s7_make_symbol(s7, "mus-describe"));
    if (func != Xen_undefined) return(s7_apply_function(s7, func, s7_list(s7, 1, gen))); 
  }
#endif
  Xen_check_type(false, gen, 1, S_mus_describe, "a generator");
  return(gen);
}


#if HAVE_SCHEME
#define mus_double_generic(Caller, CLM_case, Symbol)				\
  mus_xen *gn;                                                                \
  s7_pointer func; \
  gn = (mus_xen *)Xen_object_ref_checked(gen, mus_xen_tag);		\
  if (gn) return(C_double_to_Xen_real(CLM_case(gn->gen))); \
  func = s7_method(s7, gen, Symbol);				\
  if (func != Xen_undefined) return(s7_apply_function(s7, func, s7_list(s7, 1, gen))); \
  Xen_check_type(false, gen, 1, Caller, "a generator");  \
  return(gen);

#define mus_set_double_generic(Caller, CLM_case)                   \
  mus_xen *gn;                                                             \
  s7_pointer func; \
  gn = (mus_xen *)Xen_object_ref_checked(gen, mus_xen_tag);		\
  Xen_check_type(Xen_is_double(val), val, 2, S_setB Caller, "a float");   \
  if (gn) {CLM_case(gn->gen, Xen_real_to_C_double(val)); return(val);}	\
  func = s7_method(s7, gen, s7_make_symbol(s7, Caller));		\
  if ((func != Xen_undefined) && (s7_procedure_setter(s7, func)))	\
    return(s7_apply_function(s7, s7_procedure_setter(s7, func), s7_list(s7, 2, gen, val))); \
  Xen_check_type(false, gen, 1, S_setB Caller, "a generator");  \
  return(val);

#define mus_long_long_generic(Caller, CLM_case)                       \
  mus_xen *gn;                                                                \
  s7_pointer func; \
  gn = (mus_xen *)Xen_object_ref_checked(gen, mus_xen_tag);		\
  if (gn) return(C_llong_to_Xen_llong(CLM_case(gn->gen))); \
  func = s7_method(s7, gen, s7_make_symbol(s7, Caller));		\
  if (func != Xen_undefined) return(s7_apply_function(s7, func, s7_list(s7, 1, gen))); \
  Xen_check_type(false, gen, 1, Caller, "a generator");  \
  return(gen);

#define mus_set_long_long_generic(Caller, CLM_case)                \
  mus_xen *gn;                                                             \
  s7_pointer func; \
  gn = (mus_xen *)Xen_object_ref_checked(gen, mus_xen_tag);		\
  Xen_check_type(Xen_is_integer(val), val, 2, Caller, "an integer"); \
  if (gn) {CLM_case(gn->gen, Xen_llong_to_C_llong(val)); return(val);}	\
  func = s7_method(s7, gen, s7_make_symbol(s7, Caller));		\
  if ((func != Xen_undefined) && (s7_procedure_setter(s7, func)))	\
    return(s7_apply_function(s7, s7_procedure_setter(s7, func), s7_list(s7, 2, gen, val))); \
  Xen_check_type(false, gen, 1, Caller, "a generator");  \
  return(val);

#define mus_int_generic(Caller, CLM_case)                             \
  mus_xen *gn;                                                                \
  s7_pointer func; \
  gn = (mus_xen *)Xen_object_ref_checked(gen, mus_xen_tag);		\
  if (gn) return(C_int_to_Xen_integer(CLM_case(gn->gen)));			\
  func = s7_method(s7, gen, s7_make_symbol(s7, Caller));		\
  if (func != Xen_undefined) return(s7_apply_function(s7, func, s7_list(s7, 1, gen))); \
  Xen_check_type(false, gen, 1, Caller, "a generator");	\
  return(gen);

#else

#define mus_double_generic(Caller, CLM_case, Symbol)				\
  mus_xen *gn;                                                                \
  gn = (mus_xen *)Xen_object_ref_checked(gen, mus_xen_tag);		\
  if (!gn) Xen_check_type(false, gen, 1, Caller, "a generator");  \
  return(C_double_to_Xen_real(CLM_case(gn->gen)));

#define mus_set_double_generic(Caller, CLM_case)                   \
  mus_xen *gn;                                                             \
  gn = (mus_xen *)Xen_object_ref_checked(gen, mus_xen_tag);		\
  if (!gn) Xen_check_type(false, gen, 1, S_setB Caller, "a generator");  \
  Xen_check_type(Xen_is_double(val), val, 2, S_setB Caller, "a float");   \
  CLM_case(gn->gen, Xen_real_to_C_double(val));				   \
  return(val);

#define mus_long_long_generic(Caller, CLM_case)                          \
  mus_xen *gn;                                                                \
  gn = (mus_xen *)Xen_object_ref_checked(gen, mus_xen_tag);		\
  if (!gn) Xen_check_type(false, gen, 1, Caller, "a generator");  \
  return(C_llong_to_Xen_llong(CLM_case(gn->gen)));

#define mus_set_long_long_generic(Caller, CLM_case)                   \
  mus_xen *gn;                                                             \
  gn = (mus_xen *)Xen_object_ref_checked(gen, mus_xen_tag);		\
  if (!gn) Xen_check_type(false, gen, 1, S_setB Caller, "a generator");  \
  Xen_check_type(Xen_is_integer(val), val, 2, S_setB Caller, "an integer");   \
  CLM_case(gn->gen, Xen_llong_to_C_llong(val));				   \
  return(val);

#define mus_int_generic(Caller, CLM_case)                             \
  mus_xen *gn;                                                                \
  gn = (mus_xen *)Xen_object_ref_checked(gen, mus_xen_tag);		\
  if (!gn) Xen_check_type(false, gen, 1, Caller, "a generator");  \
  return(C_int_to_Xen_integer(CLM_case(gn->gen))); 

#endif

#if HAVE_SCHEME
static Xen sym_frequency, sym_phase, sym_scaler, sym_increment, sym_width, sym_offset, sym_feedforward, sym_feedback;
#endif

static Xen g_mus_frequency(Xen gen) 
{
  #define H_mus_frequency "(" S_mus_frequency " gen): gen's frequency (Hz)"
  mus_double_generic(S_mus_frequency, mus_frequency, sym_frequency);
}

static Xen g_mus_set_frequency(Xen gen, Xen val) 
{
  mus_set_double_generic(S_mus_frequency, mus_set_frequency);
}


static Xen g_mus_phase(Xen gen) 
{
  #define H_mus_phase "(" S_mus_phase " gen): gen's current phase (radians)"
  mus_double_generic(S_mus_phase, mus_phase, sym_phase);
}

static Xen g_mus_set_phase(Xen gen, Xen val) 
{
  mus_set_double_generic(S_mus_phase, mus_set_phase);
}


static Xen g_mus_scaler(Xen gen) 
{
  #define H_mus_scaler "(" S_mus_scaler " gen): gen's scaler, if any.  This is often an amplitude adjustment of some sort."
  mus_double_generic(S_mus_scaler, mus_scaler, sym_scaler);
}

static Xen g_mus_set_scaler(Xen gen, Xen val) 
{
  mus_set_double_generic(S_mus_scaler, mus_set_scaler);
}


static Xen g_mus_feedforward(Xen gen) 
{
  #define H_mus_feedforward "(" S_mus_feedforward " gen): gen's feedforward field"
  mus_double_generic(S_mus_feedforward, mus_scaler, sym_feedforward);
}

static Xen g_mus_set_feedforward(Xen gen, Xen val) 
{
  mus_set_double_generic(S_mus_feedforward, mus_set_scaler);
}


static Xen g_mus_feedback(Xen gen)
{
  #define H_mus_feedback "(" S_mus_feedback " gen): gen's " S_mus_feedback " field"
  mus_double_generic(S_mus_feedback, mus_increment, sym_feedback);
}

static Xen g_mus_set_feedback(Xen gen, Xen val)
{
  mus_set_double_generic(S_mus_feedback, mus_set_increment);
}


static Xen g_mus_width(Xen gen) 
{
  #define H_mus_width "(" S_mus_width " gen): gen's width, if any.  This is usually a table size."
  mus_double_generic(S_mus_width, mus_width, sym_width);
}

static Xen g_mus_set_width(Xen gen, Xen val) 
{
  mus_set_double_generic(S_mus_width, mus_set_width);
}


static Xen g_mus_offset(Xen gen) 
{
  #define H_mus_offset "(" S_mus_offset " gen): gen's offset, if any"
  mus_double_generic(S_mus_offset, mus_offset, sym_offset);
}

static Xen g_mus_set_offset(Xen gen, Xen val) 
{
  mus_set_double_generic(S_mus_offset, mus_set_offset);
}


static Xen g_mus_increment(Xen gen)
{
  #define H_mus_increment "(" S_mus_increment " gen): gen's " S_mus_increment " field, if any"
  mus_double_generic(S_mus_increment, mus_increment, sym_increment);
}

static Xen g_mus_set_increment(Xen gen, Xen val)
{
  mus_set_double_generic(S_mus_increment, mus_set_increment);
}


static Xen g_mus_hop(Xen gen)
{
  #define H_mus_hop "(" S_mus_hop " gen): gen's " S_mus_hop " field"
  mus_long_long_generic(S_mus_hop, mus_hop);
}

static Xen g_mus_set_hop(Xen gen, Xen val)
{
  mus_set_long_long_generic(S_mus_hop, mus_set_hop);
}


static Xen g_mus_ramp(Xen gen)
{
  #define H_mus_ramp "(" S_mus_ramp " gen): granulate generator's " S_mus_ramp " field"
  mus_long_long_generic(S_mus_ramp, mus_ramp);
}

static Xen g_mus_set_ramp(Xen gen, Xen val)
{
  mus_set_long_long_generic(S_mus_ramp, mus_set_ramp);
}


static Xen g_mus_location(Xen gen)
{
  #define H_mus_location "(" S_mus_location " gen): gen's " S_mus_location " field, if any"
  mus_long_long_generic(S_mus_location, mus_location);
}

static Xen g_mus_set_location(Xen gen, Xen val)
{
  mus_set_long_long_generic(S_mus_location, mus_set_location);
}


static Xen g_mus_order(Xen gen) 
{
  #define H_mus_order "(" S_mus_order " gen): gen's order, if any"
  mus_long_long_generic(S_mus_order, mus_length);
}


static Xen g_mus_channel(Xen gen)
{
  #define H_mus_channel "(" S_mus_channel " gen): gen's " S_mus_channel " field, if any"
  mus_int_generic(S_mus_channel, mus_channel);
}


static Xen g_mus_interp_type(Xen gen)
{
  #define H_mus_interp_type "(" S_mus_interp_type " gen): gen's " S_mus_interp_type " field, if any"
  mus_int_generic(S_mus_interp_type, mus_channels);
}


static Xen g_mus_type(Xen gen) 
{
  #define H_mus_type "(" S_mus_type " gen): gen's type"
  mus_int_generic(S_mus_type, mus_type);
}


static Xen g_mus_name(Xen gen) 
{
  #define H_mus_name "(" S_mus_name " gen): gen's (type) name, if any"
  mus_xen *ms;

  ms = (mus_xen *)Xen_object_ref_checked(gen, mus_xen_tag);
  if (ms) 
    return(C_string_to_Xen_string(mus_name(mus_xen_to_mus_any(ms))));
#if HAVE_SCHEME
  {
    s7_pointer func; 
    func = s7_method(s7, gen, s7_make_symbol(s7, "mus-name"));
    if (func != Xen_undefined) return(s7_apply_function(s7, func, s7_list(s7, 1, gen))); 
  }
#endif
  Xen_check_type(false, gen, 1, S_mus_name, "a generator");
  return(gen);
}


Xen g_mus_file_name(Xen gen) 
{
  #define H_mus_file_name "(" S_mus_file_name " gen): file associated with gen, if any"
  mus_xen *gn;             
  gn = (mus_xen *)Xen_object_ref_checked(gen, mus_xen_tag);
  if (gn) 
    return(C_string_to_Xen_string(mus_file_name(gn->gen)));
#if HAVE_SCHEME
  {
    s7_pointer func; 
    func = s7_method(s7, gen, s7_make_symbol(s7, S_mus_file_name));
    if (func != Xen_undefined) return(s7_apply_function(s7, func, s7_list(s7, 1, gen))); 
  }
#endif
  Xen_check_type(false, gen, 1, S_mus_file_name, "a generator"); 
  return(gen);
}


Xen g_mus_data(Xen gen) 
{
  #define H_mus_data "(" S_mus_data " gen): gen's internal data (a " S_vct "), if any"
  mus_xen *ms;

  ms = (mus_xen *)Xen_object_ref_checked(gen, mus_xen_tag);
  if (ms) 
    {
      if (ms->vcts)
	return(ms->vcts[MUS_DATA_WRAPPER]); 
      else return(Xen_false);
    }
#if HAVE_SCHEME
  {
    s7_pointer func; 
    func = s7_method(s7, gen, s7_make_symbol(s7, "mus-data"));
    if (func != Xen_undefined) return(s7_apply_function(s7, func, s7_list(s7, 1, gen))); 
  }
#endif
  Xen_check_type(false, gen, 1, S_mus_data, "a generator");
  return(gen);
}


static Xen g_mus_set_data(Xen gen, Xen val) 
{
  mus_xen *ms;

  ms = (mus_xen *)Xen_object_ref_checked(gen, mus_xen_tag);
  if (ms) 
    {
      Xen_check_type((mus_is_vct(val)), val, 2, S_setB S_mus_data, "a " S_vct);
      if (ms->vcts)
	{
	  vct *v;
	  mus_any *ma;
	  v = Xen_to_vct(val);
	  ma = ms->gen;
	  mus_set_data(ma, mus_vct_data(v));  /* TO REMEMBER: if allocated, should have freed, and set to not allocated */
	  ms->vcts[MUS_DATA_WRAPPER] = val;
	  return(val);
	}
    }
#if HAVE_SCHEME
  {
    s7_pointer func; 
    func = s7_method(s7, gen, s7_make_symbol(s7, "mus-data"));
    if ((func != Xen_undefined) && (s7_procedure_setter(s7, func)))		      
      return(s7_apply_function(s7, s7_procedure_setter(s7, func), s7_list(s7, 2, gen, val)));
  }
#endif

  Xen_check_type(false, gen, 1, S_setB S_mus_data, "a generator with a data field");
  return(Xen_false);
}


static Xen g_mus_xcoeffs(Xen gen) 
{
  #define H_mus_xcoeffs "(" S_mus_xcoeffs " gen): gen's filter xcoeffs (" S_vct " of coefficients on inputs)"
  mus_xen *ms;
  
  ms = (mus_xen *)Xen_object_ref_checked(gen, mus_xen_tag);
  if (ms) 
    {
      mus_any *g;
      g = ms->gen;
      if (ms->vcts)
	{
	  if (mus_is_polywave(g))
	    return(ms->vcts[0]);
	  if (ms->nvcts > G_FILTER_XCOEFFS)
	    return(ms->vcts[G_FILTER_XCOEFFS]); 
	}
      if ((mus_is_one_zero(g)) ||
	  (mus_is_one_pole(g)) ||
	  (mus_is_two_zero(g)) ||
	  (mus_is_two_pole(g)))
	return(xen_make_vct_wrapper(3, mus_xcoeffs(g)));
      return(Xen_false);
    }
#if HAVE_SCHEME
  {
    s7_pointer func; 
    func = s7_method(s7, gen, s7_make_symbol(s7, "mus-xcoeffs"));
    if (func != Xen_undefined) return(s7_apply_function(s7, func, s7_list(s7, 1, gen))); 
  }
#endif
  Xen_check_type(false, gen, 1, S_mus_xcoeffs, "a generator");
  return(gen);
}


static Xen g_mus_ycoeffs(Xen gen) 
{
  #define H_mus_ycoeffs "(" S_mus_ycoeffs " gen): gen's filter ycoeffs (" S_vct " of coefficients on outputs)"
  mus_xen *ms;

  ms = (mus_xen *)Xen_object_ref_checked(gen, mus_xen_tag);
  if (ms) 
    {
      mus_any *g;
      g = ms->gen;
      if (ms->vcts)
	{
	  if ((mus_is_polywave(Xen_to_mus_any(gen))) && (ms->nvcts == 2))
	    return(ms->vcts[1]);
	  if (ms->nvcts > G_FILTER_YCOEFFS)
	    return(ms->vcts[G_FILTER_YCOEFFS]);
	}
      if ((mus_is_one_zero(g)) ||
	  (mus_is_one_pole(g)) ||
	  (mus_is_two_zero(g)) ||
	  (mus_is_two_pole(g)))
	return(xen_make_vct_wrapper(3, mus_ycoeffs(g)));
      return(Xen_false);
    }
#if HAVE_SCHEME
  {
    s7_pointer func; 
    func = s7_method(s7, gen, s7_make_symbol(s7, "mus-ycoeffs"));
    if (func != Xen_undefined) return(s7_apply_function(s7, func, s7_list(s7, 1, gen))); 
  }
#endif
  Xen_check_type(false, gen, 1, S_mus_ycoeffs, "a generator");
  return(gen);
}


static Xen g_mus_xcoeff(Xen gen, Xen index)
{
  #define H_mus_xcoeff "(" S_mus_xcoeff " gen index): gen's filter xcoeff value at index (0-based)"
  mus_xen *ms;

  ms = (mus_xen *)Xen_object_ref_checked(gen, mus_xen_tag);
  if (ms) 
    {
      int ind = 0;
      Xen_to_C_integer_or_error(index, ind, S_mus_xcoeff, 2);
      if (ind < 0)
	Xen_out_of_range_error(S_mus_xcoeff, 2, index, "index must be non-negative");
      return(C_double_to_Xen_real(mus_xcoeff(mus_xen_to_mus_any(ms), ind)));
    }
#if HAVE_SCHEME
  {
    s7_pointer func; 
    func = s7_method(s7, gen, s7_make_symbol(s7, "mus-xcoeff"));
    if (func != Xen_undefined) return(s7_apply_function(s7, func, s7_list(s7, 2, gen, index))); 
  }
#endif
  Xen_check_type(false, gen, 1, S_mus_xcoeff, "a generator");
  return(index);
}


static Xen g_mus_set_xcoeff(Xen gen, Xen index, Xen val)
{
  mus_xen *ms;

  ms = (mus_xen *)Xen_object_ref_checked(gen, mus_xen_tag);
  if (ms) 
    {
      int ind = 0;
      mus_float_t x;
      Xen_to_C_integer_or_error(index, ind, S_setB S_mus_xcoeff, 2);
      Xen_to_C_double_or_error(val, x, S_setB S_mus_xcoeff, 3);
      if (ind < 0)
	Xen_out_of_range_error(S_setB S_mus_xcoeff, 2, index, "index must be non-negative");
      mus_set_xcoeff(mus_xen_to_mus_any(ms), ind, x);
      return(val);
    }
#if HAVE_SCHEME
  {
    s7_pointer func; 
    func = s7_method(s7, gen, s7_make_symbol(s7, "mus-xcoeff"));
    if ((func != Xen_undefined) && (s7_procedure_setter(s7, func)))		      
      return(s7_apply_function(s7, s7_procedure_setter(s7, func), s7_list(s7, 3, gen, index, val)));
  }
#endif
  Xen_check_type(false, gen, 1, S_setB S_mus_xcoeff, "a generator");
  return(val);
}


static Xen g_mus_ycoeff(Xen gen, Xen index)
{
  #define H_mus_ycoeff "(" S_mus_ycoeff " gen index): gen's filter ycoeff value at index (0-based)"
  mus_xen *ms;

  ms = (mus_xen *)Xen_object_ref_checked(gen, mus_xen_tag);
  if (ms) 
    {
      int ind = 0;
      Xen_to_C_integer_or_error(index, ind, S_mus_ycoeff, 2);
      if (ind < 0)
	Xen_out_of_range_error(S_mus_ycoeff, 2, index, "index must be non-negative");
      return(C_double_to_Xen_real(mus_ycoeff(mus_xen_to_mus_any(ms), ind)));
    }
#if HAVE_SCHEME
  {
    s7_pointer func; 
    func = s7_method(s7, gen, s7_make_symbol(s7, "mus-ycoeff"));
    if (func != Xen_undefined) return(s7_apply_function(s7, func, s7_list(s7, 2, gen, index))); 
  }
#endif
  Xen_check_type(false, gen, 1, S_mus_ycoeff, "a generator");
  return(index);
}


static Xen g_mus_set_ycoeff(Xen gen, Xen index, Xen val)
{
  mus_xen *ms;

  ms = (mus_xen *)Xen_object_ref_checked(gen, mus_xen_tag);
  if (ms) 
    {
      int ind = 0;
      mus_float_t x;
      Xen_to_C_integer_or_error(index, ind, S_setB S_mus_ycoeff, 2);
      Xen_to_C_double_or_error(val, x, S_setB S_mus_ycoeff, 3);
      if (ind < 0)
	Xen_out_of_range_error(S_setB S_mus_ycoeff, 2, index, "index must be non-negative");
      mus_set_ycoeff(mus_xen_to_mus_any(ms), ind, x);
      return(val);
    }
#if HAVE_SCHEME
  {
    s7_pointer func; 
    func = s7_method(s7, gen, s7_make_symbol(s7, "mus-ycoeff"));
    if ((func != Xen_undefined) && (s7_procedure_setter(s7, func)))		      
      return(s7_apply_function(s7, s7_procedure_setter(s7, func), s7_list(s7, 3, gen, index, val)));
  }
#endif
  Xen_check_type(false, gen, 1, S_setB S_mus_ycoeff, "a generator");
  return(val);
}


Xen g_mus_channels(Xen gen)
{
  #define H_mus_channels "(" S_mus_channels " gen): gen's " S_mus_channels " field, if any"
  mus_xen *gn;

  gn = (mus_xen *)Xen_object_ref_checked(gen, mus_xen_tag);
  if (gn)
    return(C_int_to_Xen_integer(mus_channels(gn->gen)));

#if HAVE_SCHEME
  if (mus_is_vct(gen))
    {
      if (Xen_vector_rank(gen) > 1)
	return(C_int_to_Xen_integer(s7_vector_dimensions(gen)[0]));
      else return(C_int_to_Xen_integer(1));
    }
#else
  if (mus_is_vct(gen))
    return(C_int_to_Xen_integer(1));
#endif

#if HAVE_SCHEME
  {
    s7_pointer func; 
    func = s7_method(s7, gen, s7_make_symbol(s7, "mus-channels"));
    if (func != Xen_undefined) return(s7_apply_function(s7, func, s7_list(s7, 1, gen))); 
  }
#endif

  Xen_check_type(false, gen, 1, S_mus_channels, "an output generator, " S_vct ", or sound-data object");
  return(Xen_false); /* make compiler happy */
}


Xen g_mus_length(Xen gen)
{
  #define H_mus_length "(" S_mus_length " gen): gen's length, if any"
  mus_xen *gn;

  gn = (mus_xen *)Xen_object_ref_checked(gen, mus_xen_tag);
  if (gn)
    return(C_llong_to_Xen_llong(mus_length(gn->gen)));

  if (mus_is_vct(gen))
    return(C_int_to_Xen_integer(mus_vct_length(Xen_to_vct(gen))));

#if HAVE_SCHEME
  {
    s7_pointer func; 
    func = s7_method(s7, gen, s7_make_symbol(s7, "mus-length"));
    if (func != Xen_undefined) return(s7_apply_function(s7, func, s7_list(s7, 1, gen))); 
  }
#endif

  Xen_check_type(false, gen, 1, S_mus_length, "a generator, " S_vct ", or sound-data object");
  return(Xen_false); /* make compiler happy */
}


static Xen g_mus_set_length(Xen gen, Xen val) 
{
  mus_xen *ms;

  ms = (mus_xen *)Xen_object_ref_checked(gen, mus_xen_tag);
  if (ms)
    {
      mus_long_t len = 0;
      mus_any *ptr = NULL;

      Xen_to_C_integer_or_error(val, len, S_setB S_mus_length, 2);
      if (len <= 0)
	Xen_out_of_range_error(S_setB S_mus_length, 1, val, "must be > 0");

      ptr = ms->gen;
      if ((!mus_is_env(ptr)) && (!mus_is_src(ptr))) /* set length doesn't refer to data vct here */
	{
	  if ((ms->vcts) && (!(Xen_is_eq(ms->vcts[MUS_DATA_WRAPPER], Xen_undefined))))
	    {
	      vct *v;
	      v = Xen_to_vct(ms->vcts[MUS_DATA_WRAPPER]);
	      if ((v) && (len > mus_vct_length(v)))
		Xen_out_of_range_error(S_setB S_mus_length, 1, val, "must be <= current data size");
	      /* set_offset refers only to env, set_width only to square_wave et al, set_location only readin */
	      /* filters are protected by keeping allocated_size and not allowing arrays to be set */
	    }
	}
      mus_set_length(ptr, len);
      return(val);
    }
#if HAVE_SCHEME
  {
    s7_pointer func; 
    func = s7_method(s7, gen, s7_make_symbol(s7, "mus-length"));
    if ((func != Xen_undefined) && (s7_procedure_setter(s7, func)))		      
      return(s7_apply_function(s7, s7_procedure_setter(s7, func), s7_list(s7, 2, gen, val)));
  }
#endif
  Xen_check_type(false, gen, 1, S_setB S_mus_length, "a generator");
  return(val);
}





/* ---------------- oscil ---------------- */

#if HAVE_SCHEME 

/* an experiment (see g_make_ncos also) -- this is essentially the same speed as using unscramble_optkey.
 */
static s7_pointer g_make_oscil(s7_scheme *sc, s7_pointer args)
{
  #define H_make_oscil "(" S_make_oscil " (frequency *clm-default-frequency*) (initial-phase 0.0)): return a new " S_oscil " (sinewave) generator"
  mus_any *ge;
  mus_float_t freq, phase;
  s7_pointer f, p;

  f = s7_car(args);
  p = s7_cadr(args);

  if (s7_is_real(f))
    freq = s7_number_to_real_with_caller(sc, f, S_make_oscil);
  else {freq = 0.0; Xen_check_type(false, f, 1, S_make_oscil, "a number");}
  if (freq > (0.5 * mus_srate()))
    Xen_out_of_range_error(S_make_oscil, 1, f, "freq > srate/2?");

  if (s7_is_real(p))
    phase = s7_number_to_real_with_caller(sc, p, S_make_oscil);
  else {phase = 0.0; Xen_check_type(false, p, 2, S_make_oscil, "a number");}

  ge = mus_make_oscil(freq, phase);
  if (ge) return(mus_xen_to_object(mus_any_to_mus_xen(ge)));
  return(Xen_false);
}

#else

static Xen g_make_oscil(Xen arg1, Xen arg2, Xen arg3, Xen arg4)
{
  #define H_make_oscil "(" S_make_oscil " (frequency *clm-default-frequency*) (initial-phase 0.0)): return a new " S_oscil " (sinewave) generator"
  mus_any *ge;
  mus_float_t freq, phase = 0.0;

  freq = clm_default_frequency;
  if (Xen_is_bound(arg1))
    {
      if (!Xen_is_bound(arg2))
	{
	  Xen_check_type(Xen_is_number(arg1), arg1, 1, S_make_oscil, "a number");
	  freq = Xen_real_to_C_double(arg1);
	  if (freq > (0.5 * mus_srate()))
	    Xen_out_of_range_error(S_make_oscil, 1, arg1, "freq > srate/2?");
	}
      else
	{
	  int vals;
	  Xen args[4]; 
	  Xen keys[2];
	  int orig_arg[2] = {0, 0};
	  keys[0] = kw_frequency;
	  keys[1] = kw_initial_phase;

	  args[0] = arg1; args[1] = arg2; args[2] = arg3; args[3] = arg4; 
	  vals = mus_optkey_unscramble(S_make_oscil, 2, keys, args, orig_arg);
	  if (vals > 0)
	    {
	      freq = Xen_optkey_to_float(kw_frequency, keys[0], S_make_oscil, orig_arg[0], freq);
	      if (freq > (0.5 * mus_srate()))
		Xen_out_of_range_error(S_make_oscil, orig_arg[0], keys[0], "freq > srate/2?");
	      phase = Xen_optkey_to_float(kw_initial_phase, keys[1], S_make_oscil, orig_arg[1], phase);
	    }
	}
    }
 
  ge = mus_make_oscil(freq, phase);
  if (ge) return(mus_xen_to_object(mus_any_to_mus_xen(ge)));
  return(Xen_false);
}
#endif

static Xen g_oscil(Xen osc, Xen fm, Xen pm)
{
  #define H_oscil "(" S_oscil " gen (fm 0.0) (pm 0.0)): next sample from " S_oscil " gen: val = sin(phase + pm); phase += (freq + fm)"
  mus_float_t fm1;
  mus_any *g = NULL;
  mus_xen *gn;

  Xen_to_C_generator(osc, gn, g, mus_is_oscil, S_oscil, "an oscil");
  if (!Xen_is_bound(fm))
    return(C_double_to_Xen_real(mus_oscil_unmodulated(g)));
  if (!Xen_is_bound(pm))
    return(C_double_to_Xen_real(mus_oscil_fm(g, Xen_real_to_C_double(fm))));
  fm1 = Xen_real_to_C_double(fm);
  if (fm1 == 0.0)
    return(C_double_to_Xen_real(mus_oscil_pm(g, Xen_real_to_C_double(pm))));
  return(C_double_to_Xen_real(mus_oscil(g, fm1, Xen_real_to_C_double(pm))));
}


static Xen g_is_oscil(Xen os) 
{
  #define H_is_oscil "(" S_is_oscil " gen): " PROC_TRUE " if gen is an " S_oscil
  return(C_bool_to_Xen_boolean((mus_is_xen(os)) && (mus_is_oscil(Xen_to_mus_any(os)))));
}


static Xen g_make_oscil_bank(Xen freqs, Xen phases, Xen amps, XEN stable)
{
  #define H_make_oscil_bank "(" S_make_oscil_bank " freqs phases amps stable): return a new oscil-bank generator. (freqs in radians)"

  mus_any *ge = NULL;
  vct *f, *p, *a = NULL;
  mus_xen *gn;

  Xen_check_type(mus_is_vct(freqs), freqs, 1, S_make_oscil_bank, "a " S_vct);
  Xen_check_type(mus_is_vct(phases), phases, 2, S_make_oscil_bank, "a " S_vct);
  Xen_check_type(Xen_is_boolean_or_unbound(stable), stable, 3, S_make_oscil_bank, "a boolean");

  f = Xen_to_vct(freqs);
  p = Xen_to_vct(phases);
  if (mus_is_vct(amps)) a = Xen_to_vct(amps);
  
  ge = mus_make_oscil_bank(mus_vct_length(f), mus_vct_data(f), mus_vct_data(p), (a) ? mus_vct_data(a) : NULL, Xen_is_true(stable));
  /* Xen_is_true looks specifically for #t */

  gn = mx_alloc(3);
  gn->gen = ge;
  gn->vcts[0] = freqs;
  gn->vcts[1] = phases;
  gn->vcts[2] = amps;

  return(mus_xen_to_object(gn));
}


static Xen g_is_oscil_bank(Xen os) 
{
  #define H_is_oscil_bank "(" S_is_oscil_bank " gen): " PROC_TRUE " if gen is an " S_oscil_bank
  return(C_bool_to_Xen_boolean((mus_is_xen(os)) && (mus_is_oscil_bank(Xen_to_mus_any(os)))));
}


static Xen g_oscil_bank(Xen g)
{
  #define H_oscil_bank "(" S_oscil_bank " bank): sum an array of oscils"
  mus_any *ob = NULL;
  mus_xen *gn;

  Xen_to_C_generator(g, gn, ob, mus_is_oscil_bank, S_oscil_bank, "an oscil-bank generator");
  return(C_double_to_Xen_real(mus_oscil_bank(ob)));
}




/* ---------------- delay ---------------- */


typedef enum {G_DELAY, G_COMB, G_NOTCH, G_ALL_PASS, G_FCOMB} xclm_delay_t;

static Xen g_make_delay_1(xclm_delay_t choice, Xen arglist)
{
  mus_any *ge = NULL, *filt = NULL;
  const char *caller = NULL;
  Xen args[18];
  Xen keys[9];
  Xen xen_filt = Xen_false;
  int orig_arg[9] = {0, 0, 0, 0, 0, 0, 0, (int)MUS_INTERP_NONE, 0};
  int vals, i, argn = 0;
  mus_long_t max_size = -1, size = -1;
  int interp_type = (int)MUS_INTERP_NONE;
  mus_float_t *line = NULL;
  mus_float_t scaler = 0.0, feedback = 0.0, feedforward = 0.0;
  vct *initial_contents = NULL;
  Xen orig_v = Xen_false;            /* initial-contents can be a vct */
  mus_float_t initial_element = 0.0;
  int scaler_key = -1, feedback_key = -1, feedforward_key = -1, size_key = -1, initial_contents_key = -1;
  int initial_element_key = -1, max_size_key = -1, interp_type_key = -1, filter_key = -1;

  switch (choice)
    {
    case G_DELAY:          caller = S_make_delay;                                                       break;
    case G_COMB:           caller = S_make_comb;           scaler_key = argn; keys[argn++] = kw_scaler; break;
    case G_FCOMB:          caller = S_make_filtered_comb;  scaler_key = argn; keys[argn++] = kw_scaler; break;
    case G_NOTCH:          caller = S_make_notch;          scaler_key = argn; keys[argn++] = kw_scaler; break;
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

  {
    Xen p;
    int a2, arglist_len;
    a2 = argn * 2;
    arglist_len = Xen_list_length(arglist);
    if (arglist_len > a2) clm_error(caller, "too many arguments!", arglist);
    for (i = 0, p = arglist; i < arglist_len; i++, p = Xen_cdr(p)) args[i] = Xen_car(p);
    for (i = arglist_len; i < a2; i++) args[i] = Xen_undefined;
  }

  vals = mus_optkey_unscramble(caller, argn, keys, args, orig_arg);

  if (vals > 0)
    {
      bool size_set = false, max_size_set = false;
      /* try to catch obvious type/range errors before allocations 
       *   a major complication here is that size can be 0
       */

      if (!(Xen_is_keyword(keys[size_key])))
	{
	  size = Xen_optkey_to_mus_long_t(kw_size, keys[size_key], caller, orig_arg[size_key], size); /* size can  be 0? -- surely we need a line in any case? */
	  if (size < 0)
	    Xen_out_of_range_error(caller, orig_arg[size_key], keys[size_key], "size < 0?");
	  if (size > mus_max_table_size())
	    Xen_out_of_range_error(caller, orig_arg[size_key], keys[size_key], "size too large (see mus-max-table-size)");
	  size_set = true;
	}

      if (!(Xen_is_keyword(keys[max_size_key])))
	{
	  max_size = Xen_optkey_to_mus_long_t(kw_max_size, keys[max_size_key], caller, orig_arg[max_size_key], max_size); /* -1 = unset */
	  if (max_size <= 0)
	    Xen_out_of_range_error(caller, orig_arg[max_size_key], keys[max_size_key], "max-size <= 0?");
	  if (max_size > mus_max_table_size())
	    Xen_out_of_range_error(caller, orig_arg[max_size_key], keys[max_size_key], "max-size too large (see mus-max-table-size)");
	  max_size_set = true;
	}

      if (Xen_is_keyword(keys[interp_type_key]))
	{
	  /* if type not given, if max_size, assume linear interp (for possible tap), else no interp */
	  if ((max_size_set) && (max_size != size))
	    interp_type = (int)MUS_INTERP_LINEAR;
	  else interp_type = (int)MUS_INTERP_NONE;
	}
      else
	{
	  interp_type = Xen_optkey_to_int(kw_type, keys[interp_type_key], caller, orig_arg[interp_type_key], (int)MUS_INTERP_LINEAR);
	  if (!(mus_is_interp_type(interp_type)))
	    Xen_out_of_range_error(caller, orig_arg[interp_type_key], keys[interp_type_key], "no such interp-type");
	}

      initial_element = Xen_optkey_to_float(kw_initial_element, keys[initial_element_key], caller, orig_arg[initial_element_key], initial_element);

      switch (choice)
	{
	case G_DELAY: 
	  break;

	case G_COMB: case G_NOTCH: case G_FCOMB:
	  scaler = Xen_optkey_to_float(kw_scaler, keys[scaler_key], caller, orig_arg[scaler_key], scaler);
	  break;

	case G_ALL_PASS:
	  feedback = Xen_optkey_to_float(kw_feedback, keys[feedback_key], caller, orig_arg[feedback_key], feedback);
	  feedforward = Xen_optkey_to_float(kw_feedforward, keys[feedforward_key], caller, orig_arg[feedforward_key], feedforward);
	  break;
	}

      if (!(Xen_is_keyword(keys[filter_key])))
	{
	  if (choice != G_FCOMB)
	    clm_error(caller, "filter arg passed??", keys[filter_key]);
				 
	  Xen_check_type(mus_is_xen(keys[filter_key]), keys[filter_key], orig_arg[filter_key], caller, "filter arg must be a generator");
	  xen_filt = keys[filter_key];
	  filt = Xen_to_mus_any(xen_filt);
	}

      if (!(Xen_is_keyword(keys[initial_contents_key])))
	{
	  if (!(Xen_is_keyword(keys[initial_element_key])))
	    Xen_out_of_range_error(caller, 
				   orig_arg[initial_contents_key], 
				   keys[initial_contents_key], 
				   "initial-contents and initial-element in same call?");
	  if (mus_is_vct(keys[initial_contents_key]))
	    {
	      initial_contents = Xen_to_vct(keys[initial_contents_key]);
	      orig_v = keys[initial_contents_key];
	    }
	  else
	    {
	      if (Xen_is_list(keys[initial_contents_key]))
		{
		  int len;
		  len = Xen_list_length(keys[initial_contents_key]);
		  if (len <= 0) 
		    Xen_error(NO_DATA,
			      Xen_list_2(C_string_to_Xen_string("~A: initial-contents not a proper list?"),
					 C_string_to_Xen_string(caller)));

		  orig_v = xen_list_to_vct(keys[initial_contents_key]);
		  initial_contents = Xen_to_vct(orig_v);
		  /* do I need to protect this until we read its contents? -- no extlang stuff except error returns */
		}
	      else Xen_check_type(Xen_is_false(keys[initial_contents_key]), 
				   keys[initial_contents_key], 
				   orig_arg[initial_contents_key], 
				   caller, "a " S_vct " or a list");
	    }
	  if (initial_contents)
	    {
	      if (size_set)
		{
		  if (size > mus_vct_length(initial_contents))
		    Xen_out_of_range_error(caller, orig_arg[initial_contents_key], keys[initial_contents_key], "size > initial-contents length");
		}
	      else size = mus_vct_length(initial_contents);
	      if (max_size_set)
		{
		  if (max_size > mus_vct_length(initial_contents))
		    Xen_out_of_range_error(caller, orig_arg[initial_contents_key], keys[initial_contents_key], "max-size > initial-contents length");
		}
	      else max_size = mus_vct_length(initial_contents);
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

  if (initial_contents == NULL)
    {
      line = (mus_float_t *)malloc(max_size * sizeof(mus_float_t));
      if (line == NULL)
	return(clm_mus_error(MUS_MEMORY_ALLOCATION_FAILED, "can't allocate delay line", caller));
      orig_v = xen_make_vct(max_size, line);
      if (initial_element != 0.0)
	{
	  for (i = 0; i < max_size; i++) 
	    line[i] = initial_element;
	}
      else memset((void *)line, 0, max_size * sizeof(mus_float_t));
    }
  else
    {
      line = mus_vct_data(initial_contents);
    }

  {
    mus_error_handler_t *old_error_handler;
    old_error_handler = mus_error_set_handler(local_mus_error);
    switch (choice)
      {
      case G_DELAY:           ge = mus_make_delay(size, line, max_size, (mus_interp_t)interp_type);                           break;
      case G_COMB:            ge = mus_make_comb(scaler, size, line, max_size, (mus_interp_t)interp_type);                    break;
      case G_NOTCH:           ge = mus_make_notch(scaler, size, line, max_size, (mus_interp_t)interp_type);                   break;
      case G_ALL_PASS:        ge = mus_make_all_pass(feedback, feedforward, size, line, max_size, (mus_interp_t)interp_type); break;
      case G_FCOMB:           ge = mus_make_filtered_comb(scaler, size, line, max_size, (mus_interp_t)interp_type, filt);     break;
      }
    mus_error_set_handler(old_error_handler);
  }

  if (ge) 
    {
      if (choice != G_FCOMB)
	return(mus_xen_to_object(mus_any_to_mus_xen_with_vct(ge, orig_v)));
      return(mus_xen_to_object(mus_any_to_mus_xen_with_two_vcts(ge, orig_v, xen_filt)));
    }
  return(clm_mus_error(local_error_type, local_error_msg, caller));
}


static Xen g_make_delay(Xen args) 
{
  #define H_make_delay "(" S_make_delay " (size) (initial-contents) (initial-element 0.0) (max-size) (type mus-interp-linear)): \
return a new delay line of size elements. \
If the delay length will be changing at run-time, max-size sets its maximum length, so\n\
   (" S_make_delay " len :max-size (+ len 10))\n\
provides 10 extra elements of delay for subsequent phasing or flanging. \
initial-contents can be either a list or a " S_vct "."

  if ((Xen_is_pair(args)) && (!Xen_is_pair(Xen_cdr(args))))
    {
      Xen val, v;
      mus_any *ge;
      mus_long_t size, max_size;
      mus_float_t *line;
      mus_error_handler_t *old_error_handler;

      val = Xen_car(args);
      Xen_check_type(Xen_is_integer(val), val, 1, S_make_delay, "an integer");
      size = Xen_integer_to_C_int(val);
      if (size < 0)
	Xen_out_of_range_error(S_make_delay, 1, val, "size < 0?");
      if (size > mus_max_table_size())
	Xen_out_of_range_error(S_make_delay, 1, val, "size too large (see mus-max-table-size)");
      if (size == 0) max_size = 1; else max_size = size;

      line = (mus_float_t *)calloc(max_size, sizeof(mus_float_t));
      v = xen_make_vct(max_size, line); /* we need this for mus-data */

      old_error_handler = mus_error_set_handler(local_mus_error);
      ge = mus_make_delay(size, line, max_size, MUS_INTERP_NONE);
      mus_error_set_handler(old_error_handler);
      if (ge) return(mus_xen_to_object(mus_any_to_mus_xen_with_vct(ge, v)));
      return(clm_mus_error(local_error_type, local_error_msg, S_make_delay));
    }

  return(g_make_delay_1(G_DELAY, args));
}


static Xen g_make_comb(Xen args) 
{
  #define H_make_comb "(" S_make_comb " (scaler) (size) (initial-contents) (initial-element 0.0) (max-size) (type " S_mus_interp_linear ")): \
return a new comb filter (a delay line with a scaler on the feedback) of size elements. \
If the comb length will be changing at run-time, max-size sets its maximum length. \
initial-contents can be either a list or a " S_vct "."

  return(g_make_delay_1(G_COMB, args));
}


static Xen g_make_filtered_comb(Xen args) 
{
  #define H_make_filtered_comb "(" S_make_filtered_comb " (scaler) (size) (initial-contents) (initial-element 0.0) (max-size) (type " S_mus_interp_linear ") :filter): \
return a new filtered comb filter (a delay line with a scaler and a filter on the feedback) of size elements. \
If the comb length will be changing at run-time, max-size sets its maximum length. \
initial-contents can be either a list or a " S_vct "."

  return(g_make_delay_1(G_FCOMB, args));
}


static Xen g_make_notch(Xen args) 
{
  #define H_make_notch "(" S_make_notch " (scaler) (size) (initial-contents) (initial-element 0.0) (max-size) (type " S_mus_interp_linear ")): \
return a new notch filter (a delay line with a scaler on the feedforward) of size elements. \
If the notch length will be changing at run-time, max-size sets its maximum length. \
initial-contents can be either a list or a " S_vct "."

  return(g_make_delay_1(G_NOTCH, args));
}


static Xen g_make_all_pass(Xen args) 
{
  #define H_make_all_pass "(" S_make_all_pass " (feedback) (feedforward) (size) (initial-contents) (initial-element 0.0) (max-size) (type " S_mus_interp_linear ")): \
return a new allpass filter (a delay line with a scalers on both the feedback and the feedforward). \
If the " S_all_pass " length will be changing at run-time, max-size sets its maximum length. \
initial-contents can be either a list or a " S_vct "."

  return(g_make_delay_1(G_ALL_PASS, args));
}


typedef enum {G_MOVING_AVERAGE, G_MOVING_MAX, G_MOVING_NORM} xclm_moving_t;

static Xen g_make_moving_any(xclm_moving_t choice, const char *caller, Xen arglist)
{
  mus_any *ge = NULL;
  Xen args[8];
  Xen keys[4];
  int orig_arg[4] = {0, 0, 0, 0};
  int vals, i, argn = 0, arglist_len;
  mus_long_t size = -1;
  mus_float_t scaler = 1.0, sum = 0.0;
  vct *initial_contents = NULL;
  Xen orig_v = Xen_false, p; 
  mus_float_t initial_element = 0.0;
  mus_float_t *line = NULL;
  int scaler_key = -1, size_key, initial_contents_key, initial_element_key;
  mus_error_handler_t *old_error_handler;

  size_key = argn;                 
  keys[argn++] = kw_size;
  if (choice == G_MOVING_NORM) 
    {
      scaler_key = argn;       
      keys[argn++] = kw_scaler;
    }
  initial_contents_key = argn; 
  keys[argn++] = kw_initial_contents;
  initial_element_key = argn;  
  keys[argn++] = kw_initial_element;

  arglist_len = Xen_list_length(arglist);
  if (arglist_len > 8) clm_error(caller, "too many arguments!", arglist);
  for (i = 0, p = arglist; i < arglist_len; i++, p = Xen_cdr(p)) args[i] = Xen_car(p);
  for (i = arglist_len; i < argn * 2; i++) args[i] = Xen_undefined;
  vals = mus_optkey_unscramble(caller, argn, keys, args, orig_arg);

  if (vals > 0)
    {
      bool size_set = false;
      if (!(Xen_is_keyword(keys[size_key])))
	{
	  size = Xen_optkey_to_mus_long_t(kw_size, keys[size_key], caller, orig_arg[size_key], size); /* size can  be 0? -- surely we need a line in any case? */
	  if (size < 0)
	    Xen_out_of_range_error(caller, orig_arg[size_key], keys[size_key], "size < 0?");
	  if (size > mus_max_table_size())
	    Xen_out_of_range_error(caller, orig_arg[size_key], keys[size_key], "size too large (see mus-max-table-size)");
	  size_set = true;
	}

      if (choice == G_MOVING_NORM)
	scaler = Xen_optkey_to_float(kw_scaler, keys[scaler_key], caller, orig_arg[scaler_key], scaler);

      initial_element = Xen_optkey_to_float(kw_initial_element, keys[initial_element_key], caller, orig_arg[initial_element_key], initial_element);
      if (!(Xen_is_keyword(keys[initial_contents_key])))
	{
	  if (!(Xen_is_keyword(keys[initial_element_key])))
	    Xen_out_of_range_error(caller, 
				   orig_arg[initial_contents_key], 
				   keys[initial_contents_key], 
				   "initial-contents and initial-element in same call?");
	  if (mus_is_vct(keys[initial_contents_key]))
	    {
	      initial_contents = Xen_to_vct(keys[initial_contents_key]);
	      orig_v = keys[initial_contents_key];
	    }
	  else
	    {
	      if (Xen_is_list(keys[initial_contents_key]))
		{
		  int len;
		  len = Xen_list_length(keys[initial_contents_key]);
		  if (len <= 0) 
		    Xen_error(NO_DATA,
			      Xen_list_2(C_string_to_Xen_string("~A: initial-contents not a proper list?"),
					 C_string_to_Xen_string(caller)));
		  
		  orig_v = xen_list_to_vct(keys[initial_contents_key]);
		  initial_contents = Xen_to_vct(orig_v);
		  /* do I need to protect this until we read its contents? -- no extlang stuff except error returns */
		}
	      else Xen_check_type(Xen_is_false(keys[initial_contents_key]), 
				  keys[initial_contents_key], 
				  orig_arg[initial_contents_key], 
				  caller, "a " S_vct " or a list");
	    }
	  if (initial_contents)
	    {
	      if (size_set)
		{
		  if (size > mus_vct_length(initial_contents))
		    Xen_out_of_range_error(caller, orig_arg[initial_contents_key], keys[initial_contents_key], "size > initial-contents length");
		}
	      else size = mus_vct_length(initial_contents);
	    }
	}
    }

  if (size < 0) size = 1;
  if (size == 0)
    Xen_out_of_range_error(caller, 0, C_llong_to_Xen_llong(size), "size = 0?");

  if (initial_contents == NULL)
    {
      line = (mus_float_t *)malloc(size * sizeof(mus_float_t));
      if (line == NULL)
	return(clm_mus_error(MUS_MEMORY_ALLOCATION_FAILED, "can't allocate delay line", caller));
      orig_v = xen_make_vct(size, line);
      if (initial_element != 0.0)
	{
	  for (i = 0; i < size; i++) 
	    line[i] = initial_element;
	  sum = initial_element * size;
	}
      else memset((void *)line, 0, size * sizeof(mus_float_t));
    }
  else
    {
      line = mus_vct_data(initial_contents);
      if ((line) && (choice == G_MOVING_AVERAGE))
	{
	  sum = line[0];
	  for (i = 1; i < size; i++)
	    sum += line[i];
	}
    }

  old_error_handler = mus_error_set_handler(local_mus_error);
  switch (choice)
    {
    case G_MOVING_AVERAGE:  ge = mus_make_moving_average_with_initial_sum(size, line, sum); break;
    case G_MOVING_MAX:      ge = mus_make_moving_max(size, line);                           break;
    case G_MOVING_NORM:     ge = mus_make_moving_norm(size, line, scaler);                  break;
    }
  mus_error_set_handler(old_error_handler);

  if (ge) 
    return(mus_xen_to_object(mus_any_to_mus_xen_with_vct(ge, orig_v)));
  return(clm_mus_error(local_error_type, local_error_msg, caller));
}


static Xen g_make_moving_average(Xen args) 
{
  #define H_make_moving_average "(" S_make_moving_average " (size) (initial-contents) (initial-element 0.0)): \
return a new moving_average generator. initial-contents can be either a list or a " S_vct "."

  return(g_make_moving_any(G_MOVING_AVERAGE, S_make_moving_average, args));
}


static Xen g_make_moving_max(Xen args) 
{
  #define H_make_moving_max "(" S_make_moving_max " (size) (initial-contents) (initial-element 0.0)): \
return a new moving-max generator. initial-contents can be either a list or a " S_vct "."

  return(g_make_moving_any(G_MOVING_MAX, S_make_moving_max, args));
}


static Xen g_make_moving_norm(Xen args) 
{
  #define H_make_moving_norm "(" S_make_moving_norm " (size (scaler 1.0))): return a new moving-norm generator."

  return(g_make_moving_any(G_MOVING_NORM, S_make_moving_norm, args));
}


static Xen g_delay(Xen obj, Xen input, Xen pm)
{
  #define H_delay "(" S_delay " gen (val 0.0) (pm 0.0)): \
delay val according to the delay line's length and pm ('phase-modulation'). \
If pm is greater than 0.0, the max-size argument used to create gen should have accommodated its maximum value."

  mus_any *g = NULL;
  mus_xen *gn;

  Xen_to_C_generator(obj, gn, g, mus_is_delay, S_delay, "a delay line");
  if (!Xen_is_bound(input))
    return(C_double_to_Xen_real(mus_delay_unmodulated(g, 0.0)));
  if (!Xen_is_bound(pm))
    return(C_double_to_Xen_real(mus_delay_unmodulated(g, Xen_real_to_C_double(input))));
  return(C_double_to_Xen_real(mus_delay(g, Xen_real_to_C_double(input), Xen_real_to_C_double(pm))));
}


static Xen g_delay_tick(Xen obj, Xen input)
{
  #define H_delay_tick "(" S_delay_tick " gen (val 0.0)): \
delay val according to the delay line's length. This merely 'ticks' the delay line forward.\
The argument 'val' is returned."

  mus_float_t in1 = 0.0;
  mus_any *g = NULL;
  mus_xen *gn;

  Xen_to_C_generator(obj, gn, g, mus_is_delay, S_delay_tick, "a delay line");
  Xen_real_to_C_double_if_bound(input, in1, S_delay_tick, 2);

  return(C_double_to_Xen_real(mus_delay_tick(g, in1)));
}


static Xen g_notch(Xen obj, Xen input, Xen pm)
{
  #define H_notch "(" S_notch " gen (val 0.0) (pm 0.0)): notch filter val, pm changes the delay length."

  mus_float_t in1 = 0.0, pm1 = 0.0;
  mus_any *g = NULL;
  mus_xen *gn;

  Xen_to_C_generator(obj, gn, g, mus_is_notch, S_notch, "a notch filter");
  Xen_real_to_C_double_if_bound(input, in1, S_notch, 2);
  Xen_real_to_C_double_if_bound(pm, pm1, S_notch, 3);

  return(C_double_to_Xen_real(mus_notch(g, in1, pm1)));
}


static Xen g_comb(Xen obj, Xen input, Xen pm)
{
  #define H_comb "(" S_comb " gen (val 0.0) (pm 0.0)): comb filter val, pm changes the delay length."
  mus_float_t in1 = 0.0, pm1 = 0.0;
  mus_any *g = NULL;
  mus_xen *gn;

  Xen_to_C_generator(obj, gn, g, mus_is_comb, S_comb, "a comb generator");
  Xen_real_to_C_double_if_bound(input, in1, S_comb, 2);
  Xen_real_to_C_double_if_bound(pm, pm1, S_comb, 3);

  return(C_double_to_Xen_real(mus_comb(g, in1, pm1)));
}


static Xen g_make_comb_bank(Xen arg)
{
  #define H_make_comb_bank "(" S_make_comb_bank " gens): return a new comb-bank generator."

  mus_any *ge = NULL;
  mus_any **gens;
  int i, j, size;

  Xen_check_type(Xen_is_vector(arg), arg, 1, S_make_comb_bank, "a vector of comb generators");

  size = Xen_vector_length(arg);
  if (size == 0) return(Xen_false);
  gens = (mus_any **)calloc(size, sizeof(mus_any *));

  for (i = 0, j = 0; i < size; i++)
    {
      Xen g;
      g = Xen_vector_ref(arg, i);
      if (mus_is_xen(g))
	{
	  mus_any *fg;
	  fg = Xen_to_mus_any(g);
	  if (mus_is_comb(fg))
	    gens[j++] = fg;
	}
    }
  if (j > 0)
    ge = mus_make_comb_bank(j, gens);
  free(gens);

  if (ge) 
    return(mus_xen_to_object(mus_any_to_mus_xen_with_vct(ge, arg)));
  return(Xen_false);
}


static Xen g_is_comb_bank(Xen os) 
{
  #define H_is_comb_bank "(" S_is_comb_bank " gen): " PROC_TRUE " if gen is a " S_comb_bank
  return(C_bool_to_Xen_boolean((mus_is_xen(os)) && (mus_is_comb_bank(Xen_to_mus_any(os)))));
}


static Xen g_comb_bank(Xen gens, Xen inp)
{
  #define H_comb_bank "(" S_comb_bank " bank inval): sum an array of " S_comb " filters."
  mus_any *bank = NULL;
  mus_xen *gn;
  mus_float_t x = 0.0;

  Xen_to_C_generator(gens, gn, bank, mus_is_comb_bank, S_comb_bank, "a comb-bank generator");
  Xen_real_to_C_double_if_bound(inp, x, S_comb_bank, 2);

  return(C_double_to_Xen_real(mus_comb_bank(bank, x)));
}



static Xen g_filtered_comb(Xen obj, Xen input, Xen pm)
{
  #define H_filtered_comb "(" S_filtered_comb " gen (val 0.0) (pm 0.0)): filtered comb filter val, pm changes the delay length."
  mus_float_t in1 = 0.0, pm1 = 0.0;
  mus_any *g = NULL;
  mus_xen *gn;

  Xen_to_C_generator(obj, gn, g, mus_is_filtered_comb, S_filtered_comb, "a filtered-comb generator");
  Xen_real_to_C_double_if_bound(input, in1, S_filtered_comb, 2);
  Xen_real_to_C_double_if_bound(pm, pm1, S_filtered_comb, 3);

  return(C_double_to_Xen_real(mus_filtered_comb(g, in1, pm1)));
}


static Xen g_make_filtered_comb_bank(Xen arg)
{
  #define H_make_filtered_comb_bank "(" S_make_filtered_comb_bank " gens): return a new filtered_comb-bank generator."

  mus_any *ge = NULL;
  mus_any **gens;
  int i, j, size;

  Xen_check_type(Xen_is_vector(arg), arg, 1, S_make_filtered_comb_bank, "a vector of filtered_comb generators");

  size = Xen_vector_length(arg);
  if (size == 0) return(Xen_false);
  gens = (mus_any **)calloc(size, sizeof(mus_any *));

  for (i = 0, j = 0; i < size; i++)
    {
      Xen g;
      g = Xen_vector_ref(arg, i);
      if (mus_is_xen(g))
	{
	  mus_any *fg;
	  fg = Xen_to_mus_any(g);
	  if (mus_is_filtered_comb(fg))
	    gens[j++] = fg;
	}
    }
  if (j > 0)
    ge = mus_make_filtered_comb_bank(j, gens);
  free(gens);

  if (ge) 
    return(mus_xen_to_object(mus_any_to_mus_xen_with_vct(ge, arg)));
  return(Xen_false);
}


static Xen g_is_filtered_comb_bank(Xen os) 
{
  #define H_is_filtered_comb_bank "(" S_is_filtered_comb_bank " gen): " PROC_TRUE " if gen is a " S_filtered_comb_bank
  return(C_bool_to_Xen_boolean((mus_is_xen(os)) && (mus_is_filtered_comb_bank(Xen_to_mus_any(os)))));
}


static Xen g_filtered_comb_bank(Xen gens, Xen inp)
{
  #define H_filtered_comb_bank "(" S_filtered_comb_bank " bank inval): sum an array of " S_filtered_comb " filters."
  mus_any *bank = NULL;
  mus_xen *gn;
  mus_float_t x = 0.0;

  Xen_to_C_generator(gens, gn, bank, mus_is_filtered_comb_bank, S_filtered_comb_bank, "a filtered-comb-bank generator");
  Xen_real_to_C_double_if_bound(inp, x, S_filtered_comb_bank, 2);

  return(C_double_to_Xen_real(mus_filtered_comb_bank(bank, x)));
}



static Xen g_all_pass(Xen obj, Xen input, Xen pm)
{
  #define H_all_pass "(" S_all_pass " gen (val 0.0) (pm 0.0)): all-pass filter val, pm changes the delay length."
  mus_float_t in1 = 0.0, pm1 = 0.0;
  mus_any *g = NULL;
  mus_xen *gn;

  Xen_to_C_generator(obj, gn, g, mus_is_all_pass, S_all_pass, "an all-pass filter");
  Xen_real_to_C_double_if_bound(input, in1, S_all_pass, 2);
  Xen_real_to_C_double_if_bound(pm, pm1, S_all_pass, 3);

  return(C_double_to_Xen_real(mus_all_pass(g, in1, pm1)));
}


static Xen g_make_all_pass_bank(Xen arg)
{
  #define H_make_all_pass_bank "(" S_make_all_pass_bank " gens): return a new all_pass-bank generator."

  mus_any *ge = NULL;
  mus_any **gens;
  int i, j, size;

  Xen_check_type(Xen_is_vector(arg), arg, 1, S_make_all_pass_bank, "a vector of all_pass generators");

  size = Xen_vector_length(arg);
  if (size == 0) return(Xen_false);
  gens = (mus_any **)calloc(size, sizeof(mus_any *));

  for (i = 0, j = 0; i < size; i++)
    {
      Xen g;
      g = Xen_vector_ref(arg, i);
      if (mus_is_xen(g))
	{
	  mus_any *fg;
	  fg = Xen_to_mus_any(g);
	  if (mus_is_all_pass(fg))
	    gens[j++] = fg;
	}
    }
  if (j > 0)
    ge = mus_make_all_pass_bank(j, gens);
  free(gens);

  if (ge) 
    return(mus_xen_to_object(mus_any_to_mus_xen_with_vct(ge, arg)));
  return(Xen_false);
}


static Xen g_is_all_pass_bank(Xen os) 
{
  #define H_is_all_pass_bank "(" S_is_all_pass_bank " gen): " PROC_TRUE " if gen is a " S_all_pass_bank
  return(C_bool_to_Xen_boolean((mus_is_xen(os)) && (mus_is_all_pass_bank(Xen_to_mus_any(os)))));
}


static Xen g_all_pass_bank(Xen gens, Xen inp)
{
  #define H_all_pass_bank "(" S_all_pass_bank " bank inval): sum an array of " S_all_pass " filters."
  mus_any *bank = NULL;
  mus_xen *gn;
  mus_float_t x = 0.0;

  Xen_to_C_generator(gens, gn, bank, mus_is_all_pass_bank, S_all_pass_bank, "an all-pass-bank generator");
  Xen_real_to_C_double_if_bound(inp, x, S_all_pass_bank, 2);

  return(C_double_to_Xen_real(mus_all_pass_bank(bank, x)));
}



static Xen g_moving_average(Xen obj, Xen input)
{
  #define H_moving_average "(" S_moving_average " gen (val 0.0)): moving window average."
  mus_float_t in1 = 0.0;
  mus_any *g = NULL;
  mus_xen *gn;

  Xen_to_C_generator(obj, gn, g, mus_is_moving_average, S_moving_average, "a moving-average generator");
  Xen_real_to_C_double_if_bound(input, in1, S_moving_average, 2);

  return(C_double_to_Xen_real(mus_moving_average(g, in1)));
}


static Xen g_moving_max(Xen obj, Xen input)
{
  #define H_moving_max "(" S_moving_max " gen (val 0.0)): moving window max."
  mus_float_t in1 = 0.0;
  mus_any *g = NULL;
  mus_xen *gn;

  Xen_to_C_generator(obj, gn, g, mus_is_moving_max, S_moving_max, "a moving-max generator");
  Xen_real_to_C_double_if_bound(input, in1, S_moving_max, 2);

  return(C_double_to_Xen_real(mus_moving_max(g, in1)));
}


static Xen g_moving_norm(Xen obj, Xen input)
{
  #define H_moving_norm "(" S_moving_norm " gen (val 0.0)): moving window norm."
  mus_float_t in1 = 0.0;
  mus_any *g = NULL;
  mus_xen *gn;

  Xen_to_C_generator(obj, gn, g, mus_is_moving_norm, S_moving_norm, "a moving-norm generator");
  Xen_real_to_C_double_if_bound(input, in1, S_moving_norm, 2);

  return(C_double_to_Xen_real(mus_moving_norm(g, in1)));
}


static Xen g_tap(Xen obj, Xen loc)
{
  #define H_tap "(" S_tap " gen (pm 0.0)): tap the " S_delay " generator offset by pm"
  mus_float_t dloc = 0.0;
  mus_any *g = NULL;
  mus_xen *gn;

  Xen_to_C_generator(obj, gn, g, mus_is_tap, S_tap, "a delay line tap");
  Xen_real_to_C_double_if_bound(loc, dloc, S_tap, 3);

  return(C_double_to_Xen_real(mus_tap(g, dloc)));
}


static Xen g_is_tap(Xen obj) 
{
  #define H_is_tap "(" S_is_tap " gen): " PROC_TRUE " if gen is a delay line tap"
  return(C_bool_to_Xen_boolean((mus_is_xen(obj)) && (mus_is_tap(Xen_to_mus_any(obj)))));
}


static Xen g_is_delay(Xen obj) 
{
  #define H_is_delay "(" S_is_delay " gen): " PROC_TRUE " if gen is a delay line"
  return(C_bool_to_Xen_boolean((mus_is_xen(obj)) && (mus_is_delay(Xen_to_mus_any(obj)))));
}


static Xen g_is_comb(Xen obj)
{
  #define H_is_comb "(" S_is_comb " gen): " PROC_TRUE " if gen is a comb filter"
  return(C_bool_to_Xen_boolean((mus_is_xen(obj)) && (mus_is_comb(Xen_to_mus_any(obj)))));
}


static Xen g_is_filtered_comb(Xen obj)
{
  #define H_is_filtered_comb "(" S_is_filtered_comb " gen): " PROC_TRUE " if gen is a filtered-comb filter"
  return(C_bool_to_Xen_boolean((mus_is_xen(obj)) && (mus_is_filtered_comb(Xen_to_mus_any(obj)))));
}


static Xen g_is_notch(Xen obj) 
{
  #define H_is_notch "(" S_is_notch " gen): " PROC_TRUE " if gen is a notch filter"
  return(C_bool_to_Xen_boolean((mus_is_xen(obj)) && (mus_is_notch(Xen_to_mus_any(obj)))));
}


static Xen g_is_all_pass(Xen obj) 
{
  #define H_is_all_pass "(" S_is_all_pass " gen): " PROC_TRUE " if gen is an all-pass filter"
  return(C_bool_to_Xen_boolean((mus_is_xen(obj)) && (mus_is_all_pass(Xen_to_mus_any(obj)))));
}


static Xen g_is_moving_average(Xen obj) 
{
  #define H_is_moving_average "(" S_is_moving_average " gen): " PROC_TRUE " if gen is a moving-average generator"
  return(C_bool_to_Xen_boolean((mus_is_xen(obj)) && (mus_is_moving_average(Xen_to_mus_any(obj)))));
}


static Xen g_is_moving_max(Xen obj) 
{
  #define H_is_moving_max "(" S_is_moving_max " gen): " PROC_TRUE " if gen is a moving-max generator"
  return(C_bool_to_Xen_boolean((mus_is_xen(obj)) && (mus_is_moving_max(Xen_to_mus_any(obj)))));
}


static Xen g_is_moving_norm(Xen obj) 
{
  #define H_is_moving_norm "(" S_is_moving_norm " gen): " PROC_TRUE " if gen is a moving-norm generator"
  return(C_bool_to_Xen_boolean((mus_is_xen(obj)) && (mus_is_moving_norm(Xen_to_mus_any(obj)))));
}


/* -------- ncos -------- */

static Xen g_is_ncos(Xen obj) 
{
  #define H_is_ncos "(" S_is_ncos " gen): " PROC_TRUE " if gen is an " S_ncos " generator"
  return(C_bool_to_Xen_boolean((mus_is_xen(obj)) && 
			  (mus_is_ncos(Xen_to_mus_any(obj)))));
}

#if HAVE_SCHEME
static s7_pointer g_make_ncos(s7_scheme *sc, s7_pointer args)
{
  #define H_make_ncos "(" S_make_ncos " (frequency *clm-default-frequency*) (n 1)): \
return a new " S_ncos " generator, producing a sum of 'n' equal amplitude cosines."

  mus_any *ge;
  mus_float_t freq;
  s7_pointer f, p;
  int n = 1;

  f = s7_car(args);
  p = s7_cadr(args);

  if (s7_is_real(f))
    freq = s7_number_to_real_with_caller(sc, f, S_make_ncos);
  else {freq = 0.0; Xen_check_type(false, f, 1, S_make_ncos, "a number");}
  if (freq > (0.5 * mus_srate()))
    Xen_out_of_range_error(S_make_ncos, 1, f, "freq > srate/2?");

  if (s7_is_integer(p))
    n = s7_number_to_integer_with_caller(sc, p, S_make_ncos);
  else Xen_check_type(false, p, 2, S_make_ncos, "an integer");
  if (n <= 0)
    Xen_out_of_range_error(S_make_ncos, 2, p, "n <= 0?");

  ge = mus_make_ncos(freq, n);
  if (ge) return(mus_xen_to_object(mus_any_to_mus_xen(ge)));
  return(Xen_false);
}

#else

static Xen g_make_ncos(Xen arg1, Xen arg2, Xen arg3, Xen arg4)
{
  #define H_make_ncos "(" S_make_ncos " (frequency *clm-default-frequency*) (n 1)): \
return a new " S_ncos " generator, producing a sum of 'n' equal amplitude cosines."

  mus_any *ge;
  Xen args[4]; 
  Xen keys[2];
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
      freq = Xen_optkey_to_float(kw_frequency, keys[0], S_make_ncos, orig_arg[0], freq);
      if (freq > (0.5 * mus_srate()))
	Xen_out_of_range_error(S_make_ncos, orig_arg[0], keys[0], "freq > srate/2?");

      n = Xen_optkey_to_int(kw_n, keys[1], S_make_ncos, orig_arg[1], n);
      if (n <= 0)
	Xen_out_of_range_error(S_make_ncos, orig_arg[1], keys[1], "n <= 0?");
    }

  ge = mus_make_ncos(freq, n);
  if (ge) return(mus_xen_to_object(mus_any_to_mus_xen(ge)));
  return(Xen_false);
}

#endif

static Xen g_ncos(Xen obj, Xen fm)
{
  #define H_ncos "(" S_ncos " gen (fm 0.0)): get the next sample from 'gen', an " S_ncos " generator"

  mus_float_t fm1 = 0.0;
  mus_any *g = NULL;
  mus_xen *gn;

  Xen_to_C_generator(obj, gn, g, mus_is_ncos, S_ncos, "an ncos generator");
  Xen_real_to_C_double_if_bound(fm, fm1, S_ncos, 2);

  return(C_double_to_Xen_real(mus_ncos(g, fm1)));
}



/* -------- nsin -------- */

static Xen g_is_nsin(Xen obj) 
{
  #define H_is_nsin "(" S_is_nsin " gen): " PROC_TRUE " if gen is an " S_nsin " generator"
  return(C_bool_to_Xen_boolean((mus_is_xen(obj)) && 
			  (mus_is_nsin(Xen_to_mus_any(obj)))));
}


static Xen g_make_nsin(Xen arg1, Xen arg2, Xen arg3, Xen arg4)
{
  #define H_make_nsin "(" S_make_nsin " (frequency *clm-default-frequency*) (n 1)): \
return a new " S_nsin " generator, producing a sum of 'n' equal amplitude sines"

  mus_any *ge;
  Xen args[4]; 
  Xen keys[2];
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
      freq = Xen_optkey_to_float(kw_frequency, keys[0], S_make_nsin, orig_arg[0], freq);
      if (freq > (0.5 * mus_srate()))
	Xen_out_of_range_error(S_make_nsin, orig_arg[0], keys[0], "freq > srate/2?");

      n = Xen_optkey_to_int(kw_n, keys[1], S_make_nsin, orig_arg[1], n);
      if (n <= 0)
	Xen_out_of_range_error(S_make_nsin, orig_arg[1], keys[1], "n <= 0?");
    }

  ge = mus_make_nsin(freq, n);
  if (ge) return(mus_xen_to_object(mus_any_to_mus_xen(ge)));
  return(Xen_false);
}


static Xen g_nsin(Xen obj, Xen fm)
{
  #define H_nsin "(" S_nsin " gen (fm 0.0)): get the next sample from 'gen', an " S_nsin " generator"

  mus_float_t fm1 = 0.0;
  mus_any *g = NULL;
  mus_xen *gn;

  Xen_to_C_generator(obj, gn, g, mus_is_nsin, S_nsin, "an nsin generator");
  Xen_real_to_C_double_if_bound(fm, fm1, S_nsin, 2);

  return(C_double_to_Xen_real(mus_nsin(g, fm1)));
}



/* ---------------- rand, rand_interp ---------------- */

#define RANDOM_DISTRIBUTION_TABLE_SIZE 512
#define RANDOM_DISTRIBUTION_ENVELOPE_SIZE 50

static mus_float_t *inverse_integrate(Xen dist, int data_size)
{
  /* e = env possibly starting < 0 */
  int e_size = RANDOM_DISTRIBUTION_ENVELOPE_SIZE;
  mus_float_t *e, *data;
  int i, e_len, lim, e_loc = 2;
  Xen ex0, ex1, ey0, ey1;
  mus_float_t x, x0, x1, xincr, y0, y1, sum, first_sum, last_sum = 0.0;

  lim = (e_size + 1) * 2;
  e = (mus_float_t *)calloc(lim, sizeof(mus_float_t));

  e_len = Xen_list_length(dist);
  ex0 = Xen_list_ref(dist, 0);
  ex1 = Xen_list_ref(dist, e_len - 2);
  x0 = Xen_real_to_C_double(ex0);
  /* get x range first */
  x1 = Xen_real_to_C_double(ex1);
  xincr = (x1 - x0) / (mus_float_t)e_size;
  /* now true x1 */
  ex1 = Xen_list_ref(dist, 2);
  x1 = Xen_real_to_C_double(ex1);
  ey0 = Xen_list_ref(dist, 1);
  ey1 = Xen_list_ref(dist, 3);
  y0 = Xen_real_to_C_double(ey0);
  y1 = Xen_real_to_C_double(ey1);
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
	  ex1 = Xen_list_ref(dist, e_loc);
	  ey1 = Xen_list_ref(dist, e_loc + 1);
	  x1 = Xen_real_to_C_double(ex1);
	  y1 = Xen_real_to_C_double(ey1);
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


static Xen g_make_noi(bool rand_case, const char *caller, Xen arglist)
{
  mus_any *ge = NULL;
  Xen args[10];
  Xen keys[5];
  int orig_arg[5] = {0, 0, 0, 0, 0};
  int vals;
  mus_float_t freq, base = 1.0;
  mus_float_t *distribution = NULL;
  Xen orig_v = Xen_false;
  int distribution_size = RANDOM_DISTRIBUTION_TABLE_SIZE;

  freq = clm_default_frequency;

  keys[0] = kw_frequency;
  keys[1] = kw_amplitude;
  keys[2] = kw_envelope;
  keys[3] = kw_distribution;
  keys[4] = kw_size;

  {
    int i, arglist_len;
    Xen p;
    arglist_len = Xen_list_length(arglist);
    if (arglist_len > 10) clm_error(caller, "too many arguments!", arglist);
    for (i = 0, p = arglist; i < arglist_len; i++, p = Xen_cdr(p)) args[i] = Xen_car(p);
    for (i = arglist_len; i < 10; i++) args[i] = Xen_undefined;
  }

  vals = mus_optkey_unscramble(caller, 5, keys, args, orig_arg);
  if (vals > 0)
    {
      freq = Xen_optkey_to_float(kw_frequency, keys[0], caller, orig_arg[0], freq);
      if (freq > mus_srate())
	Xen_out_of_range_error(caller, orig_arg[0], keys[0], "freq > srate/2?");

      base = Xen_optkey_to_float(kw_amplitude, keys[1], caller, orig_arg[1], base);

      distribution_size = Xen_optkey_to_int(kw_size, keys[4], caller, orig_arg[4], distribution_size);
      if (distribution_size <= 0)
	Xen_out_of_range_error(caller, orig_arg[4], keys[4], "distribution size <= 0?");
      if (distribution_size > mus_max_table_size())
	Xen_out_of_range_error(caller, orig_arg[4], keys[4], "distribution size too large (see mus-max-table-size)");

      if (!(Xen_is_keyword(keys[2]))) /* i.e. envelope arg was specified */
        {
	  int len;
	  Xen_check_type(Xen_is_list(keys[2]), keys[2], orig_arg[2], caller, "an envelope");
	  len = Xen_list_length(keys[2]);
	  if ((len < 4) || (len & 1))
	    clm_error(caller, "bad distribution envelope", keys[2]);
	  /* envelope and distribution are incompatible */
	  if (!(Xen_is_keyword(keys[3])))
	    clm_error(caller, ":envelope and :distribution in same call?", keys[3]);
	  distribution = inverse_integrate(keys[2], distribution_size);
	  orig_v = xen_make_vct(distribution_size, distribution);
	}
      else
	{
	  if (!(Xen_is_keyword(keys[3]))) /* i.e. distribution arg was specified */
	    {
	      Xen_check_type(mus_is_vct(keys[3]) || Xen_is_false(keys[3]), keys[3], orig_arg[3], caller, "a " S_vct);
	      if (mus_is_vct(keys[3]))
		{
		  vct *v = NULL;
		  orig_v = keys[3];
		  v = mus_optkey_to_vct(orig_v, caller, orig_arg[3], NULL);
		  distribution_size = mus_vct_length(v);
		  distribution = mus_vct_data(v);
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
      if (mus_is_vct(orig_v))
	return(mus_xen_to_object(mus_any_to_mus_xen_with_vct(ge, orig_v)));
      return(mus_xen_to_object(mus_any_to_mus_xen(ge)));
    }
  return(Xen_false);
}


static Xen g_make_rand_interp(Xen arglist)
{
  #define H_make_rand_interp "(" S_make_rand_interp " (frequency *clm-default-frequency*) (amplitude 1.0) (envelope) (distribution) (size)): \
return a new " S_rand_interp " generator, producing linearly interpolated random numbers. \
frequency is the rate at which new end-points are chosen."

  return(g_make_noi(false, S_make_rand_interp, arglist));
}


static Xen g_make_rand(Xen arglist)
{
  #define H_make_rand "(" S_make_rand " (frequency *clm-default-frequency*) (amplitude 1.0) (envelope) (distribution) (size)): \
return a new " S_rand " generator, producing a sequence of random numbers (a step  function). \
frequency is the rate at which new numbers are chosen."

  return(g_make_noi(true, S_make_rand, arglist));
}


static Xen g_rand(Xen obj, Xen fm)
{
  #define H_rand "(" S_rand " gen (fm 0.0)): gen's current random number. \
fm modulates the rate at which the current number is changed."

  mus_float_t fm1 = 0.0;
  mus_any *g = NULL;
  mus_xen *gn;

  Xen_to_C_generator(obj, gn, g, mus_is_rand, S_rand, "a rand generator");
  Xen_real_to_C_double_if_bound(fm, fm1, S_rand, 2);

  return(C_double_to_Xen_real(mus_rand(g, fm1)));
}


static Xen g_is_rand(Xen obj) 
{
  #define H_is_rand "(" S_is_rand " gen): " PROC_TRUE " if gen is a " S_rand
  return(C_bool_to_Xen_boolean((mus_is_xen(obj)) && (mus_is_rand(Xen_to_mus_any(obj)))));
}


static Xen g_rand_interp(Xen obj, Xen fm)
{
  #define H_rand_interp "(" S_rand_interp " gen (fm 0.0)): gen's current (interpolating) random number. \
fm modulates the rate at which new segment end-points are chosen."

  mus_float_t fm1 = 0.0;
  mus_any *g = NULL;
  mus_xen *gn;

  Xen_to_C_generator(obj, gn, g, mus_is_rand_interp, S_rand_interp, "a rand-interp generator");
  Xen_real_to_C_double_if_bound(fm, fm1, S_rand_interp, 2);

  return(C_double_to_Xen_real(mus_rand_interp(g, fm1)));
}


static Xen g_is_rand_interp(Xen obj) 
{
  #define H_is_rand_interp "(" S_is_rand_interp " gen): " PROC_TRUE " if gen is a " S_rand_interp
  return(C_bool_to_Xen_boolean((mus_is_xen(obj)) && (mus_is_rand_interp(Xen_to_mus_any(obj)))));
}


static Xen g_mus_random(Xen a) 
{
  #define H_mus_random "(" S_mus_random " val): a random number between -val and val. \
the built-in 'random' function returns values between 0 and its argument"
  mus_float_t x;

  Xen_to_C_double_or_error(a, x, S_mus_random, 1);
  return(C_double_to_Xen_real(mus_random(x)));
}


static Xen g_mus_rand_seed(void) 
{
  #define H_mus_rand_seed "(" S_mus_rand_seed "): the random number seed; \
this can be used to re-run a particular random number sequence."

  return(C_int_to_Xen_integer(mus_rand_seed()));
}


static Xen g_mus_set_rand_seed(Xen a) 
{
  Xen_check_type(Xen_is_integer(a), a, 1, S_setB S_mus_rand_seed, "an integer");
  mus_set_rand_seed((unsigned long)Xen_integer_to_C_int(a)); 
  return(a);
}



/* ---------------- table lookup ---------------- */

static Xen g_is_table_lookup(Xen obj) 
{
  #define H_is_table_lookup "(" S_is_table_lookup " gen): " PROC_TRUE " if gen is a " S_table_lookup
  return(C_bool_to_Xen_boolean((mus_is_xen(obj)) && (mus_is_table_lookup(Xen_to_mus_any(obj)))));
}


static Xen g_partials_to_wave(Xen partials, Xen utable, Xen normalize)
{
  #define H_partials_to_wave "(" S_partials_to_wave " partials wave (normalize " PROC_FALSE ")): \
take a list or " S_vct " of partials (harmonic number and associated amplitude) and produce \
a waveform for use in " S_table_lookup ".  If wave (a " S_vct ") is not given, \
a new one is created.  If normalize is " PROC_TRUE ", the resulting waveform goes between -1.0 and 1.0.\n\
  (set! gen (" S_make_table_lookup " 440.0 :wave (" S_partials_to_wave " '(1 1.0 2 .5))))"

  vct *f;
  Xen table; 
  mus_float_t *partial_data = NULL;
  mus_long_t len = 0;
  bool partials_allocated = true;
#if HAVE_SCHEME
  int gc_loc;
#endif

  Xen_check_type(mus_is_vct(partials) || Xen_is_list(partials), partials, 1, S_partials_to_wave, "a list or a " S_vct);
  Xen_check_type(mus_is_vct(utable) || Xen_is_false(utable) || (!(Xen_is_bound(utable))), utable, 2, S_partials_to_wave, "a " S_vct " or " PROC_FALSE);
  Xen_check_type(Xen_is_boolean_or_unbound(normalize), normalize, 3, S_partials_to_wave, "a boolean");

  if (mus_is_vct(partials))
    {
      vct *v;
      v = Xen_to_vct(partials);
      partial_data = mus_vct_data(v);
      len = mus_vct_length(v);
      partials_allocated = false;
    }
  else
    {
      len = Xen_list_length(partials);
      if (len == 0)
	Xen_error(NO_DATA, 
		  Xen_list_2(C_string_to_Xen_string("~A: partials list empty?"), 
			     C_string_to_Xen_string(S_partials_to_wave)));

      if (!(Xen_is_number(Xen_car(partials))))
	Xen_check_type(false, partials, 1, S_partials_to_wave, "a list of numbers (partial numbers with amplitudes)");
    }
  if (len & 1)
    Xen_error(BAD_TYPE,
	      Xen_list_3(C_string_to_Xen_string("~A: odd length partials list? ~A"), 
			 C_string_to_Xen_string(S_partials_to_wave), 
			 partials));

  if ((!Xen_is_bound(utable)) || (!(mus_is_vct(utable))))
    {
      mus_float_t *wave;
      wave = (mus_float_t *)calloc(clm_table_size, sizeof(mus_float_t));
      if (wave == NULL)
	return(clm_mus_error(MUS_MEMORY_ALLOCATION_FAILED, "can't allocate wave table", S_partials_to_wave));
      table = xen_make_vct(clm_table_size, wave);
    }
  else table = utable;

#if HAVE_SCHEME
  gc_loc = s7_gc_protect(s7, table);
#endif

  f = Xen_to_vct(table);

  if (!partial_data)
    {
      Xen lst;
      int i;
      partial_data = (mus_float_t *)malloc(len * sizeof(mus_float_t));
      if (partial_data == NULL)
	return(clm_mus_error(MUS_MEMORY_ALLOCATION_FAILED, "can't allocate partials table", S_partials_to_wave));
      for (i = 0, lst = Xen_copy_arg(partials); i < len; i++, lst = Xen_cdr(lst)) 
	partial_data[i] = Xen_real_to_C_double(Xen_car(lst));
    }

  mus_partials_to_wave(partial_data, len / 2, mus_vct_data(f), mus_vct_length(f), (Xen_is_true(normalize)));

  if (partials_allocated)
    free(partial_data);

#if HAVE_SCHEME
  s7_gc_unprotect_at(s7, gc_loc);
#endif

  return(table);
}


static Xen g_phase_partials_to_wave(Xen partials, Xen utable, Xen normalize)
{
  vct *f;
  Xen table;
  mus_float_t *partial_data = NULL;
  mus_long_t len = 0;
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
take a list or " S_vct " of partials (harmonic number, amplitude, initial phase) and produce \
a waveform for use in " S_table_lookup ".  If wave (a " S_vct ") is not given, \
a new one is created.  If normalize is " PROC_TRUE ", the resulting waveform goes between -1.0 and 1.0.\n  " pp2w_example

  Xen_check_type(mus_is_vct(partials) || Xen_is_list(partials), partials, 1, S_phase_partials_to_wave, "a list or a " S_vct);
  Xen_check_type(mus_is_vct(utable) || Xen_is_false(utable) || (!(Xen_is_bound(utable))), utable, 2, S_phase_partials_to_wave, "a " S_vct " or " PROC_FALSE);
  Xen_check_type(Xen_is_boolean_or_unbound(normalize), normalize, 3, S_phase_partials_to_wave, "a boolean");

  if (mus_is_vct(partials))
    {
      vct *v;
      v = Xen_to_vct(partials);
      partial_data = mus_vct_data(v);
      len = mus_vct_length(v);
      partials_allocated = false;
    }
  else
    {
      len = Xen_list_length(partials);
      if (len == 0)
	Xen_error(NO_DATA,
		  Xen_list_2(C_string_to_Xen_string("~A: partials list empty?"),
			     C_string_to_Xen_string(S_phase_partials_to_wave)));

      if (!(Xen_is_number(Xen_car(partials))))
	Xen_check_type(false, partials, 1, S_phase_partials_to_wave, "a list of numbers (partial numbers with amplitudes and phases)");
    }
  if ((len % 3) != 0)
    Xen_error(Xen_make_error_type("wrong-type-arg"),
	      Xen_list_3(C_string_to_Xen_string("~A: partials list, ~A, should have 3 entries for each harmonic (number amp phase)"),
			 C_string_to_Xen_string(S_phase_partials_to_wave), 
			 partials));

  if ((!Xen_is_bound(utable)) || (!(mus_is_vct(utable))))
    {
      mus_float_t *wave;
      wave = (mus_float_t *)calloc(clm_table_size, sizeof(mus_float_t));
      if (wave == NULL)
	return(clm_mus_error(MUS_MEMORY_ALLOCATION_FAILED, "can't allocate wave table", S_phase_partials_to_wave));
      table = xen_make_vct(clm_table_size, wave);
    }
  else table = utable;

#if HAVE_SCHEME
  gc_loc = s7_gc_protect(s7, table);
#endif

  f = Xen_to_vct(table);

  if (!partial_data)
    {
      int i;
      Xen lst;
      partial_data = (mus_float_t *)malloc(len * sizeof(mus_float_t));
      if (partial_data == NULL)
	return(clm_mus_error(MUS_MEMORY_ALLOCATION_FAILED, "can't allocate partials table", S_phase_partials_to_wave));
      for (i = 0, lst = Xen_copy_arg(partials); i < len; i++, lst = Xen_cdr(lst)) 
	partial_data[i] = Xen_real_to_C_double(Xen_car(lst));
    }

  mus_phase_partials_to_wave(partial_data, len / 3, mus_vct_data(f), mus_vct_length(f), (Xen_is_true(normalize)));

  if (partials_allocated)
    free(partial_data);

#if HAVE_SCHEME
  s7_gc_unprotect_at(s7, gc_loc);
#endif

  return(table);
}


static Xen g_make_table_lookup(Xen arglist)
{
  #define H_make_table_lookup "(" S_make_table_lookup " (frequency *clm-default-frequency*) (initial-phase 0.0) (wave) (size clm-table-size) (type)): \
return a new " S_table_lookup " generator.  \
The default table size is 512; use :size to set some other size, or pass your own " S_vct " as the 'wave'.\n\
   (set! gen (" S_make_table_lookup " 440.0 :wave (" S_partials_to_wave " '(1 1.0))))\n\
is the same in effect as " S_make_oscil ".  'type' sets the interpolation choice which defaults to " S_mus_interp_linear "."

  mus_any *ge;
  int vals;
  mus_long_t table_size = clm_table_size;
  Xen args[10];
  Xen keys[5];
  int orig_arg[5] = {0, 0, 0, 0, MUS_INTERP_LINEAR};
  mus_float_t freq, phase = 0.0;
  mus_float_t *table = NULL;
  Xen orig_v = Xen_false;
  int interp_type = (int)MUS_INTERP_LINEAR;

  freq = clm_default_frequency;

  keys[0] = kw_frequency;
  keys[1] = kw_initial_phase;
  keys[2] = kw_wave;
  keys[3] = kw_size;
  keys[4] = kw_type;

  {
    int i, arglist_len;
    Xen p;
    arglist_len = Xen_list_length(arglist);
    if (arglist_len > 10) clm_error(S_make_table_lookup, "too many arguments!", arglist);
    for (i = 0, p = arglist; i < arglist_len; i++, p = Xen_cdr(p)) args[i] = Xen_car(p);
    for (i = arglist_len; i < 10; i++) args[i] = Xen_undefined;
  }

  vals = mus_optkey_unscramble(S_make_table_lookup, 5, keys, args, orig_arg);
  if (vals > 0)
    {
      vct *v = NULL;
      freq = Xen_optkey_to_float(kw_frequency, keys[0], S_make_table_lookup, orig_arg[0], freq);
      if (freq > (0.5 * mus_srate()))
	Xen_out_of_range_error(S_make_table_lookup, orig_arg[0], keys[0], "freq > srate/2?");

      phase = Xen_optkey_to_float(kw_initial_phase, keys[1], S_make_table_lookup, orig_arg[1], phase);
      if (phase < 0.0)
	Xen_out_of_range_error(S_make_table_lookup, orig_arg[1], keys[1], "initial phase <= 0.0?"); /* is this actually an error? */

      v = mus_optkey_to_vct(keys[2], S_make_table_lookup, orig_arg[2], NULL);
      if (v) 
	{
	  orig_v = keys[2];
	  table = mus_vct_data(v);
	  table_size = mus_vct_length(v);
	}

      table_size = Xen_optkey_to_mus_long_t(kw_size, keys[3], S_make_table_lookup, orig_arg[3], table_size);
      if (table_size <= 0)
	Xen_out_of_range_error(S_make_table_lookup, orig_arg[3], keys[3], "size <= 0?");
      if (table_size > mus_max_table_size())
	Xen_out_of_range_error(S_make_table_lookup, orig_arg[3], keys[3], "size too large (see mus-max-table-size)");
      if ((v) && (table_size > mus_vct_length(v)))
	Xen_out_of_range_error(S_make_table_lookup, orig_arg[3], keys[3], "table size > wave size");

      interp_type = Xen_optkey_to_int(kw_type, keys[4], S_make_table_lookup, orig_arg[4], interp_type);
      if (!(mus_is_interp_type(interp_type)))
	Xen_out_of_range_error(S_make_table_lookup, orig_arg[4], keys[4], "no such interp-type");
    }

  if (!(mus_is_vct(orig_v)))
    {
      table = (mus_float_t *)calloc(table_size, sizeof(mus_float_t));
      if (table == NULL)
	return(clm_mus_error(MUS_MEMORY_ALLOCATION_FAILED, "can't allocate table-lookup table", S_make_table_lookup));
      orig_v = xen_make_vct(table_size, table);
    }
  ge = mus_make_table_lookup(freq, phase, table, table_size, (mus_interp_t)interp_type);
  return(mus_xen_to_object(mus_any_to_mus_xen_with_vct(ge, orig_v)));
}


static Xen g_table_lookup(Xen obj, Xen fm) 
{
  #define H_table_lookup "(" S_table_lookup " gen (fm 0.0)): interpolated table-lookup \
with 'wrap-around' when gen's phase marches off either end of its table."

  mus_float_t fm1 = 0.0;
  mus_any *g = NULL;
  mus_xen *gn;

  Xen_to_C_generator(obj, gn, g, mus_is_table_lookup, S_table_lookup, "a table-lookup generator");
  Xen_real_to_C_double_if_bound(fm, fm1, S_table_lookup, 2);

  return(C_double_to_Xen_real(mus_table_lookup(g, fm1)));
}



/* ---------------- sawtooth et al ---------------- */

typedef enum {G_SAWTOOTH_WAVE, G_SQUARE_WAVE, G_TRIANGLE_WAVE, G_PULSE_TRAIN} xclm_wave_t;

static Xen g_make_sw(xclm_wave_t type, mus_float_t def_phase, Xen arg1, Xen arg2, Xen arg3, Xen arg4, Xen arg5, Xen arg6)
{
  mus_any *ge = NULL;
  const char *caller = NULL;
  Xen args[6]; 
  Xen keys[3];
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
      freq = Xen_optkey_to_float(kw_frequency, keys[0], caller, orig_arg[0], freq);
      if (freq > mus_srate())
	Xen_out_of_range_error(caller, orig_arg[0], keys[0], "freq > srate/2?");

      base = Xen_optkey_to_float(kw_amplitude, keys[1], caller, orig_arg[1], base);
      phase = Xen_optkey_to_float(kw_initial_phase, keys[2], caller, orig_arg[2], phase);
    }

  switch (type)
    {
    case G_SAWTOOTH_WAVE: ge = mus_make_sawtooth_wave(freq, base, phase); break;
    case G_SQUARE_WAVE:   ge = mus_make_square_wave(freq, base, phase);   break;
    case G_TRIANGLE_WAVE: ge = mus_make_triangle_wave(freq, base, phase); break;
    case G_PULSE_TRAIN:   ge = mus_make_pulse_train(freq, base, phase);   break;
    }
  if (ge) return(mus_xen_to_object(mus_any_to_mus_xen(ge)));
  return(Xen_false);
}


static Xen g_make_sawtooth_wave(Xen arg1, Xen arg2, Xen arg3, Xen arg4, Xen arg5, Xen arg6) 
{
  #define H_make_sawtooth_wave "(" S_make_sawtooth_wave " (frequency *clm-default-frequency*) (amplitude 1.0) (initial-phase 0.0)): \
return a new " S_sawtooth_wave " generator."

  return(g_make_sw(G_SAWTOOTH_WAVE, M_PI, arg1, arg2, arg3, arg4, arg5, arg6));
}


static Xen g_make_square_wave(Xen arg1, Xen arg2, Xen arg3, Xen arg4, Xen arg5, Xen arg6) 
{
  #define H_make_square_wave "(" S_make_square_wave " (frequency *clm-default-frequency*) (amplitude 1.0) (initial-phase 0.0)): \
return a new " S_square_wave " generator."

  return(g_make_sw(G_SQUARE_WAVE, 0.0, arg1, arg2, arg3, arg4, arg5, arg6));
}


static Xen g_make_triangle_wave(Xen arg1, Xen arg2, Xen arg3, Xen arg4, Xen arg5, Xen arg6) 
{
  #define H_make_triangle_wave "(" S_make_triangle_wave " (frequency *clm-default-frequency*) (amplitude 1.0) (initial-phase 0.0)): \
return a new " S_triangle_wave " generator."

  return(g_make_sw(G_TRIANGLE_WAVE, 0.0, arg1, arg2, arg3, arg4, arg5, arg6));
}


static Xen g_make_pulse_train(Xen arg1, Xen arg2, Xen arg3, Xen arg4, Xen arg5, Xen arg6) 
{
  #define H_make_pulse_train "(" S_make_pulse_train " (frequency *clm-default-frequency*) (amplitude 1.0) (initial-phase 0.0)): \
return a new " S_pulse_train " generator.  This produces a sequence of impulses."

  return(g_make_sw(G_PULSE_TRAIN, TWO_PI, arg1, arg2, arg3, arg4, arg5, arg6));
}


static Xen g_sawtooth_wave(Xen obj, Xen fm) 
{
  #define H_sawtooth_wave "(" S_sawtooth_wave " gen (fm 0.0)): next sawtooth sample from generator"
  mus_float_t fm1 = 0.0;
  mus_any *g = NULL;
  mus_xen *gn;

  Xen_to_C_generator(obj, gn, g, mus_is_sawtooth_wave, S_sawtooth_wave, "a sawtooth-wave generator");
  Xen_real_to_C_double_if_bound(fm, fm1, S_sawtooth_wave, 2);

  return(C_double_to_Xen_real(mus_sawtooth_wave(g, fm1)));
}


static Xen g_square_wave(Xen obj, Xen fm) 
{
  #define H_square_wave "(" S_square_wave " gen (fm 0.0)): next square wave sample from generator"
  mus_float_t fm1 = 0.0;
  mus_any *g = NULL;
  mus_xen *gn;

  Xen_to_C_generator(obj, gn, g, mus_is_square_wave, S_square_wave, "a square-wave generator");
  Xen_real_to_C_double_if_bound(fm, fm1, S_square_wave, 2);

  return(C_double_to_Xen_real(mus_square_wave(g, fm1)));
}


static Xen g_triangle_wave(Xen obj, Xen fm) 
{
  #define H_triangle_wave "(" S_triangle_wave " gen (fm 0.0)): next triangle wave sample from generator"
  mus_float_t fm1 = 0.0;
  mus_any *g = NULL;
  mus_xen *gn;

  Xen_to_C_generator(obj, gn, g, mus_is_triangle_wave, S_triangle_wave, "a triangle-wave generator");
  Xen_real_to_C_double_if_bound(fm, fm1, S_triangle_wave, 2);

  return(C_double_to_Xen_real(mus_triangle_wave(g, fm1)));
}


static Xen g_pulse_train(Xen obj, Xen fm) 
{
  #define H_pulse_train "(" S_pulse_train " gen (fm 0.0)): next pulse train sample from generator"
  mus_float_t fm1 = 0.0;
  mus_any *g = NULL;
  mus_xen *gn;

  Xen_to_C_generator(obj, gn, g, mus_is_pulse_train, S_pulse_train, "a pulse-train generator");
  Xen_real_to_C_double_if_bound(fm, fm1, S_pulse_train, 2);

  return(C_double_to_Xen_real(mus_pulse_train(g, fm1)));
}


static Xen g_is_sawtooth_wave(Xen obj) 
{
  #define H_is_sawtooth_wave "(" S_is_sawtooth_wave " gen): " PROC_TRUE " if gen is a " S_sawtooth_wave
  return(C_bool_to_Xen_boolean((mus_is_xen(obj)) && (mus_is_sawtooth_wave(Xen_to_mus_any(obj)))));
}


static Xen g_is_square_wave(Xen obj) 
{
  #define H_is_square_wave "(" S_is_square_wave " gen): " PROC_TRUE " if gen is a " S_square_wave
  return(C_bool_to_Xen_boolean((mus_is_xen(obj)) && (mus_is_square_wave(Xen_to_mus_any(obj)))));
}


static Xen g_is_triangle_wave(Xen obj) 
{
  #define H_is_triangle_wave "(" S_is_triangle_wave " gen): " PROC_TRUE " if gen is a " S_triangle_wave
  return(C_bool_to_Xen_boolean((mus_is_xen(obj)) && (mus_is_triangle_wave(Xen_to_mus_any(obj)))));
}


static Xen g_is_pulse_train(Xen obj) 
{
  #define H_is_pulse_train "(" S_is_pulse_train " gen): " PROC_TRUE " if gen is a " S_pulse_train
  return(C_bool_to_Xen_boolean((mus_is_xen(obj)) && (mus_is_pulse_train(Xen_to_mus_any(obj)))));
}



/* ---------------- asymmetric-fm ---------------- */

static Xen g_make_asymmetric_fm(Xen arg1, Xen arg2, Xen arg3, Xen arg4, Xen arg5, Xen arg6, Xen arg7, Xen arg8)
{
  #define H_make_asymmetric_fm "(" S_make_asymmetric_fm " (frequency *clm-default-frequency*) (initial-phase 0.0) (r 1.0) (ratio 1.0)): \
return a new " S_asymmetric_fm " generator."

  mus_any *ge;
  Xen args[8]; 
  Xen keys[4];
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
      freq = Xen_optkey_to_float(kw_frequency, keys[0], S_make_asymmetric_fm, orig_arg[0], freq);
      if (freq > (0.5 * mus_srate()))
	Xen_out_of_range_error(S_make_asymmetric_fm, orig_arg[0], keys[0], "freq > srate/2?");

      phase = Xen_optkey_to_float(kw_initial_phase, keys[1], S_make_asymmetric_fm, orig_arg[1], phase);

      r = Xen_optkey_to_float(kw_r, keys[2], S_make_asymmetric_fm, orig_arg[2], r);

      ratio = Xen_optkey_to_float(kw_ratio, keys[3], S_make_asymmetric_fm, orig_arg[3], ratio);
    }

  ge = mus_make_asymmetric_fm(freq, phase, r, ratio);
  if (ge) return(mus_xen_to_object(mus_any_to_mus_xen(ge)));
  return(Xen_false);
}



static Xen g_asymmetric_fm(Xen obj, Xen index, Xen fm)
{
  #define H_asymmetric_fm "(" S_asymmetric_fm " gen (index 0.0) (fm 0.0)): next sample from asymmetric fm generator"
  mus_float_t fm1 = 0.0, index1 = 0.0;
  mus_any *g = NULL;
  mus_xen *gn;

  Xen_to_C_generator(obj, gn, g, mus_is_asymmetric_fm, S_asymmetric_fm, "an asymmetric-fm generator");
  Xen_real_to_C_double_if_bound(fm, fm1, S_asymmetric_fm, 2);
  Xen_real_to_C_double_if_bound(index, index1, S_asymmetric_fm, 3);

  return(C_double_to_Xen_real(mus_asymmetric_fm(g, index1, fm1)));
}


static Xen g_is_asymmetric_fm(Xen obj) 
{
  #define H_is_asymmetric_fm "(" S_is_asymmetric_fm " gen): " PROC_TRUE " if gen is a " S_asymmetric_fm
  return(C_bool_to_Xen_boolean((mus_is_xen(obj)) && (mus_is_asymmetric_fm(Xen_to_mus_any(obj)))));
}



/* ---------------- simple filters ---------------- */

typedef enum {G_ONE_POLE, G_ONE_ZERO, G_TWO_POLE, G_TWO_ZERO} xclm_filter_t;

static const char *smpflts[6] = {S_make_one_pole, S_make_one_zero, S_make_two_pole, S_make_two_zero};


static Xen g_make_smpflt_1(xclm_filter_t choice, Xen arg1, Xen arg2, Xen arg3, Xen arg4)
{
  mus_any *gen = NULL;
  Xen args[4]; 
  Xen keys[2];
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
  return(Xen_false);
}


static Xen g_make_one_zero(Xen arg1, Xen arg2, Xen arg3, Xen arg4)
{
  #define H_make_one_zero "(" S_make_one_zero " a0 a1): return a new " S_one_zero " filter;  a0*x(n) + a1*x(n-1)"
  return(g_make_smpflt_1(G_ONE_ZERO, arg1, arg2, arg3, arg4));
}


static Xen g_make_one_pole(Xen arg1, Xen arg2, Xen arg3, Xen arg4)
{
  #define H_make_one_pole "(" S_make_one_pole " a0 b1): return a new " S_one_pole " filter; a0*x(n) - b1*y(n-1)"
  return(g_make_smpflt_1(G_ONE_POLE, arg1, arg2, arg3, arg4));
}


static Xen g_make_smpflt_2(xclm_filter_t choice, Xen arg1, Xen arg2, Xen arg3, Xen arg4, Xen arg5, Xen arg6)
{
  mus_any *gen = NULL;
  Xen args[6]; 
  Xen keys[3];
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
      a0 = Xen_optkey_to_float(kw_a0, keys[0], smpflts[choice], orig_arg[0], a0);
      a1 = mus_optkey_to_float(keys[1], smpflts[choice], orig_arg[1], a1);
      a2 = mus_optkey_to_float(keys[2], smpflts[choice], orig_arg[2], a2);
    }
  if (choice == G_TWO_ZERO)
    gen = mus_make_two_zero(a0, a1, a2);
  else gen = mus_make_two_pole(a0, a1, a2);
  if (gen) return(mus_xen_to_object(mus_any_to_mus_xen(gen)));
  return(Xen_false);
}


static bool found_polar_key(Xen arg)
{
  return((Xen_is_keyword(arg)) && 
	 ((Xen_keyword_is_eq(arg, kw_radius)) ||
	  (Xen_keyword_is_eq(arg, kw_frequency))));
}


static bool found_coeff_key(Xen arg)
{
  return((Xen_is_keyword(arg)) && 
	 (!(Xen_keyword_is_eq(arg, kw_radius))) &&
	 (!(Xen_keyword_is_eq(arg, kw_frequency))));
}


static Xen g_make_two_zero(Xen arg1, Xen arg2, Xen arg3, Xen arg4, Xen arg5, Xen arg6) 
{
  #define H_make_two_zero "(" S_make_two_zero " a0 a1 a2) or (" S_make_two_zero " frequency radius): return a new " S_two_zero " filter; \
a0*x(n) + a1*x(n-1) + a2*x(n-2)"

  if ((Xen_is_bound(arg2)) && /* 0 or 1 args -> coeffs */
      (!(Xen_is_bound(arg5)))) /* 5 or more args -> coeffs */
    {
      if ((found_polar_key(arg1)) || 
	  (found_polar_key(arg2)) ||    /* if arg1 is frequency as number, then arg2 is either key or number */
	  ((!(Xen_is_bound(arg3))) &&    /* make a guess that if 2 args, no keys, and a0 > 20, it is intended as a frequency */
	   (!(found_coeff_key(arg1))) &&
	   ((Xen_is_number(arg1)) && (Xen_real_to_C_double(arg1) >= 20.0))))
	return(g_make_smpflt_1(G_TWO_ZERO, arg1, arg2, arg3, arg4));
    }

  return(g_make_smpflt_2(G_TWO_ZERO, arg1, arg2, arg3, arg4, arg5, arg6));
}


static Xen g_make_two_pole(Xen arg1, Xen arg2, Xen arg3, Xen arg4, Xen arg5, Xen arg6) 
{
  #define H_make_two_pole "(" S_make_two_pole " a0 b1 b2) or (" S_make_two_pole " frequency radius): return a new " S_two_pole " filter; \
a0*x(n) - b1*y(n-1) - b2*y(n-2)"

  if ((Xen_is_bound(arg2)) && /* 0 or 1 args -> coeffs */
      (!(Xen_is_bound(arg5)))) /* 5 or more args -> coeffs */
    {
      if ((found_polar_key(arg1)) || 
	  (found_polar_key(arg2)) ||    /* if arg1 is frequency as number, then arg2 is either key or number */
	  ((!(Xen_is_bound(arg3))) &&
	   (!(found_coeff_key(arg1))) &&
	   ((Xen_is_number(arg1)) && (Xen_real_to_C_double(arg1) >= 2.0))))
	return(g_make_smpflt_1(G_TWO_POLE, arg1, arg2, arg3, arg4));
    }

  return(g_make_smpflt_2(G_TWO_POLE, arg1, arg2, arg3, arg4, arg5, arg6));
}


static Xen g_one_zero(Xen obj, Xen fm)
{
  #define H_one_zero "(" S_one_zero " gen (input 0.0)): one zero filter of input"
  mus_float_t fm1 = 0.0;
  mus_any *g = NULL;
  mus_xen *gn;

  Xen_to_C_generator(obj, gn, g, mus_is_one_zero, S_one_zero, "a one-zero filter");
  Xen_real_to_C_double_if_bound(fm, fm1, S_one_zero, 2);

  return(C_double_to_Xen_real(mus_one_zero(g, fm1)));
}


static Xen g_one_pole(Xen obj, Xen fm)
{
  #define H_one_pole "(" S_one_pole " gen (input 0.0)): one pole filter of input"
  mus_float_t fm1 = 0.0;
  mus_any *g = NULL;
  mus_xen *gn;

  Xen_to_C_generator(obj, gn, g, mus_is_one_pole, S_one_pole, "a one-pole filter");
  Xen_real_to_C_double_if_bound(fm, fm1, S_one_pole, 2);

  return(C_double_to_Xen_real(mus_one_pole(g, fm1)));
}


static Xen g_two_zero(Xen obj, Xen fm)
{
  #define H_two_zero "(" S_two_zero " gen (input 0.0)): two zero filter of input"
  mus_float_t fm1 = 0.0;
  mus_any *g = NULL;
  mus_xen *gn;

  Xen_to_C_generator(obj, gn, g, mus_is_two_zero, S_two_zero, "a two-zero filter");
  Xen_real_to_C_double_if_bound(fm, fm1, S_two_zero, 2);

  return(C_double_to_Xen_real(mus_two_zero(g, fm1)));
}


static Xen g_two_pole(Xen obj, Xen fm)
{
  #define H_two_pole "(" S_two_pole " gen (input 0.0)): two pole filter of input"
  mus_float_t fm1 = 0.0;
  mus_any *g = NULL;
  mus_xen *gn;

  Xen_to_C_generator(obj, gn, g, mus_is_two_pole, S_two_pole, "a two-pole filter");
  Xen_real_to_C_double_if_bound(fm, fm1, S_two_pole, 2);

  return(C_double_to_Xen_real(mus_two_pole(g, fm1)));
}


static Xen g_is_one_zero(Xen obj) 
{
  #define H_is_one_zero "(" S_is_one_zero " gen): " PROC_TRUE " if gen is a " S_one_zero
  return(C_bool_to_Xen_boolean((mus_is_xen(obj)) && (mus_is_one_zero(Xen_to_mus_any(obj)))));
}


static Xen g_is_one_pole(Xen obj) 
{
  #define H_is_one_pole "(" S_is_one_pole " gen): " PROC_TRUE " if gen is a " S_one_pole
  return(C_bool_to_Xen_boolean((mus_is_xen(obj)) && (mus_is_one_pole(Xen_to_mus_any(obj)))));
}


static Xen g_is_two_zero(Xen obj) 
{
  #define H_is_two_zero "(" S_is_two_zero " gen): " PROC_TRUE " if gen is a " S_two_zero
  return(C_bool_to_Xen_boolean((mus_is_xen(obj)) && (mus_is_two_zero(Xen_to_mus_any(obj)))));
}


static Xen g_is_two_pole(Xen obj) 
{
  #define H_is_two_pole "(" S_is_two_pole " gen): " PROC_TRUE " if gen is a " S_two_pole
  return(C_bool_to_Xen_boolean((mus_is_xen(obj)) && (mus_is_two_pole(Xen_to_mus_any(obj)))));
}




/* ---------------- formant ---------------- */

static Xen g_make_frm(bool formant_case, const char *caller, Xen arg1, Xen arg2, Xen arg3, Xen arg4)
{
  mus_any *ge;
  int vals;
  Xen args[4]; 
  Xen keys[2];
  int orig_arg[2] = {0, 0};
  mus_float_t freq = 0.0, radius = 0.0;

  keys[0] = kw_frequency;
  keys[1] = kw_radius;
  args[0] = arg1; args[1] = arg2; args[2] = arg3; args[3] = arg4;

  vals = mus_optkey_unscramble(caller, 2, keys, args, orig_arg);
  if (vals > 0)
    {
      freq = Xen_optkey_to_float(kw_frequency, keys[0], caller, orig_arg[0], freq);
      if (freq > (0.5 * mus_srate()))
	Xen_out_of_range_error(caller, orig_arg[0], keys[0], "freq > srate/2?");

      radius = Xen_optkey_to_float(kw_radius, keys[1], caller, orig_arg[1], radius);
    }

  if (formant_case)
    {
      ge = mus_make_formant(freq, radius);
      if (ge)
	{
	  mus_xen *gn;
	  gn = mus_any_to_mus_xen(ge);
	  return(mus_xen_to_object(gn));
	}
    }
  else 
    {
      ge = mus_make_firmant(freq, radius);
      if (ge) 
	return(mus_xen_to_object(mus_any_to_mus_xen(ge)));
    }
  return(Xen_false);
}


static Xen g_formant(Xen gen, Xen input, Xen freq)
{
  #define H_formant "(" S_formant " gen (input 0.0) freq-in-radians): next sample from resonator generator"
  mus_float_t in1 = 0.0;
  mus_any *g = NULL;
  mus_xen *gn;

  Xen_to_C_generator(gen, gn, g, mus_is_formant, S_formant, "a formant generator");

  Xen_real_to_C_double_if_bound(input, in1, S_formant, 2);
  if (Xen_is_bound(freq))
    return(C_double_to_Xen_real(mus_formant_with_frequency(g, in1, Xen_real_to_C_double(freq))));

  return(C_double_to_Xen_real(mus_formant(g, in1)));
}


static Xen g_make_formant(Xen arg1, Xen arg2, Xen arg3, Xen arg4)
{
  #define H_make_formant "(" S_make_formant " frequency radius): \
return a new formant generator (a resonator).  radius sets the pole radius (in terms of the 'unit circle'). \
frequency sets the resonance center frequency (Hz)."

  return(g_make_frm(true, S_make_formant, arg1, arg2, arg3, arg4));
}


static Xen g_is_formant(Xen os) 
{
  #define H_is_formant "(" S_is_formant " gen): " PROC_TRUE " if gen is a " S_formant
  return(C_bool_to_Xen_boolean((mus_is_xen(os)) && (mus_is_formant(Xen_to_mus_any(os)))));
}


static Xen g_set_formant_radius_and_frequency(Xen gen, Xen rad, Xen frq)
{
  #define H_mus_set_formant_radius_and_frequency  "(" S_mus_set_formant_radius_and_frequency  " gen radius frequency): set " S_formant " \
generator gen's radius and frequency"
  mus_any *g = NULL;
  mus_float_t radius, frequency;
  mus_xen *gn;

  Xen_to_C_generator(gen, gn, g, mus_is_formant, S_mus_set_formant_radius_and_frequency, "a formant generator");

  Xen_to_C_double_or_error(rad, radius, S_mus_set_formant_radius_and_frequency, 2);
  Xen_to_C_double_or_error(frq, frequency, S_mus_set_formant_radius_and_frequency, 3);

  mus_set_formant_radius_and_frequency(g, radius, frequency);
  return(rad);
}


static Xen g_set_formant_frequency(Xen gen, Xen frq)
{
  #define H_mus_set_formant_frequency  "(" S_mus_set_formant_frequency  " gen frequency): set " S_formant " generator gen's frequency"
  mus_any *g = NULL;
  mus_float_t frequency;
  mus_xen *gn;

  Xen_to_C_generator(gen, gn, g, mus_is_formant, S_mus_set_formant_frequency, "a formant generator");
  Xen_to_C_double_or_error(frq, frequency, S_mus_set_formant_frequency, 2);

  mus_set_formant_frequency(g, frequency);
  return(frq);
}


static Xen g_make_formant_bank(Xen frms, Xen amps)
{
  #define H_make_formant_bank "(" S_make_formant_bank " gens amps): return a new formant-bank generator."

  mus_any *ge = NULL;
  mus_any **gens;
  int i, j, size;
  vct *v = NULL;

  Xen_check_type(Xen_is_vector(frms), frms, 1, S_make_formant_bank, "a vector of formant generators");
  /* need size and elements -> mus_any */

  size = Xen_vector_length(frms);
  if (size == 0) return(Xen_false);
  gens = (mus_any **)calloc(size, sizeof(mus_any *));

  if (Xen_is_bound(amps))
    {
      v = Xen_to_vct(amps);
      if (!v) Xen_check_type(false, amps, 2, S_make_formant_bank, "a " S_vct " if anything");
    }

  for (i = 0, j = 0; i < size; i++)
    {
      Xen g;
      g = Xen_vector_ref(frms, i);
      if (mus_is_xen(g))
	{
	  mus_any *fg;
	  fg = Xen_to_mus_any(g);
	  if (mus_is_formant(fg))
	    gens[j++] = fg;
	}
    }
  if (j > 0)
    ge = mus_make_formant_bank(j, gens, (v) ? mus_vct_data(v) : NULL);
  free(gens);

  if (ge) 
    {
      if (v)
	return(mus_xen_to_object(mus_any_to_mus_xen_with_two_vcts(ge, frms, amps)));
      return(mus_xen_to_object(mus_any_to_mus_xen_with_vct(ge, frms)));
    }
  return(Xen_false);
}


static Xen g_is_formant_bank(Xen os) 
{
  #define H_is_formant_bank "(" S_is_formant_bank " gen): " PROC_TRUE " if gen is a " S_formant_bank
  return(C_bool_to_Xen_boolean((mus_is_xen(os)) && (mus_is_formant_bank(Xen_to_mus_any(os)))));
}


static Xen g_formant_bank(Xen gens, Xen inp)
{
  #define H_formant_bank "(" S_formant_bank " gens inval): sum a bank of " S_formant " generators"
  mus_any *bank = NULL;
  mus_xen *gn;

  Xen_check_type((Xen_is_number(inp)) || (mus_is_vct(inp)), inp, 2, S_formant_bank, "a number or a " S_vct);
  Xen_to_C_generator(gens, gn, bank, mus_is_formant_bank, S_formant_bank, "a formant-bank generator");

  if (Xen_is_number(inp))
    return(C_double_to_Xen_real(mus_formant_bank(bank, Xen_real_to_C_double(inp))));
  return(C_double_to_Xen_real(mus_formant_bank_with_inputs(bank, mus_vct_data(Xen_to_vct(inp)))));
}




/* ---------------- one-pole-all-pass ---------------- */

static Xen g_make_one_pole_all_pass(Xen arg1, Xen arg2)
{
  #define H_make_one_pole_all_pass "(" S_make_one_pole_all_pass " size coeff): return a new one-pole-all-pass generator."

  mus_any *ge = NULL;
  int size;
  mus_float_t coeff;

  Xen_check_type(Xen_is_integer(arg1), arg1, 1, S_make_one_pole_all_pass, "an integer");
#if (!HAVE_SCHEME)
  Xen_check_type(Xen_is_number(arg2), arg2, 2, S_make_one_pole_all_pass, "a number");
#endif

  size = Xen_integer_to_C_int(arg1);
  if (size < 0)
    Xen_out_of_range_error(S_make_one_pole_all_pass, 1, arg1, "size < 0?");
  if (size == 0) return(Xen_false);
  coeff = Xen_real_to_C_double(arg2);

  ge = mus_make_one_pole_all_pass(size, coeff);
  if (ge) 
    return(mus_xen_to_object(mus_any_to_mus_xen(ge)));
  return(Xen_false);
}


static Xen g_is_one_pole_all_pass(Xen os) 
{
  #define H_is_one_pole_all_pass "(" S_is_one_pole_all_pass " gen): " PROC_TRUE " if gen is a " S_one_pole_all_pass
  return(C_bool_to_Xen_boolean((mus_is_xen(os)) && (mus_is_one_pole_all_pass(Xen_to_mus_any(os)))));
}


static Xen g_one_pole_all_pass(Xen gen, Xen fm)
{
  #define H_one_pole_all_pass "(" S_one_pole_all_pass " gen (input 0.0)): run a one-pole-all-pass generator"
  mus_float_t in1 = 0.0;
  mus_any *g = NULL;
  mus_xen *gn;

  Xen_to_C_generator(gen, gn, g, mus_is_one_pole_all_pass, S_one_pole_all_pass, "a one-pole-all-pass generator");
  Xen_real_to_C_double_if_bound(fm, in1, S_one_pole_all_pass, 2);
  return(C_double_to_Xen_real(mus_one_pole_all_pass(g, in1)));
}




/* ---------------- firmant ---------------- */

static Xen g_make_firmant(Xen arg1, Xen arg2, Xen arg3, Xen arg4)
{
  #define H_make_firmant "(" S_make_firmant " frequency radius): \
return a new firmant generator (a resonator).  radius sets the pole radius (in terms of the 'unit circle'). \
frequency sets the resonance center frequency (Hz)."

  return(g_make_frm(false, S_make_firmant, arg1, arg2, arg3, arg4));
}


static Xen g_is_firmant(Xen os) 
{
  #define H_is_firmant "(" S_is_firmant " gen): " PROC_TRUE " if gen is a " S_firmant " generator"
  return(C_bool_to_Xen_boolean((mus_is_xen(os)) && (mus_is_firmant(Xen_to_mus_any(os)))));
}


static Xen g_firmant(Xen gen, Xen input, Xen freq)
{
  #define H_firmant "(" S_firmant " gen (input 0.0) freq-in-radians): next sample from resonator generator"
  mus_float_t in1 = 0.0;
  mus_any *g = NULL;
  mus_xen *gn;

  Xen_to_C_generator(gen, gn, g, mus_is_firmant, S_firmant, "a firmant generator");

  Xen_real_to_C_double_if_bound(input, in1, S_firmant, 2);
  if (Xen_is_bound(freq)) 
    return(C_double_to_Xen_real(mus_firmant_with_frequency(g, in1, Xen_real_to_C_double(freq))));

  return(C_double_to_Xen_real(mus_firmant(g, in1)));
}




#define S_pink_noise "pink-noise"
static Xen g_pink_noise(Xen gens)
{
  #define H_pink_noise "(pink-noise gens) generates an approximation to pink noise."
  int i, size;
  mus_float_t sum = 0.0, amp, x;
  vct *v;
  mus_float_t *data;

  Xen_check_type((mus_is_vct(gens)) && (Xen_vector_rank(gens) == 1), gens, 1, S_pink_noise, "a " S_vct);
  v = Xen_to_vct(gens);

  size = mus_vct_length(v);
  if (size == 0)
    return(XEN_ZERO); /* needs to be upper case for Forth/Ruby */
  Xen_check_type((size & 1) == 0, gens, 1, S_pink_noise, "an even length " S_vct);
    
  data = mus_vct_data(v);
  amp = data[0];

  for (i = 2, x = 0.5; i < size; i += 2, x *= 0.5)
    {
      sum += data[i];
      data[i + 1] -= x;
      if (data[i + 1] < 0.0)
	{
	  data[i] = mus_random(amp);
	  data[i + 1] += 1.0;
	}
    }

  return(C_double_to_Xen_real(sum + mus_random(amp)));
}



/* ---------------- wave-train ---------------- */

static Xen g_make_wave_train(Xen arglist)
{
  #define H_make_wave_train "(" S_make_wave_train " (frequency *clm-default-frequency*) (initial-phase 0.0) (wave) (size clm-table-size) (type)): \
return a new wave-train generator (an extension of pulse-train).   Frequency is \
the repetition rate of the wave found in wave. Successive waves can overlap."

  mus_any *ge;
  Xen args[10];
  Xen keys[5];
  int orig_arg[5] = {0, 0, 0, 0, MUS_INTERP_LINEAR};
  int vals;
  mus_long_t wsize = clm_table_size;
  Xen orig_v = Xen_false;
  mus_float_t freq, phase = 0.0;
  mus_float_t *wave = NULL;
  int interp_type = (int)MUS_INTERP_LINEAR;

  freq = clm_default_frequency;

  keys[0] = kw_frequency;
  keys[1] = kw_initial_phase;
  keys[2] = kw_wave;
  keys[3] = kw_size;
  keys[4] = kw_type;

  {
    Xen p;
    int i, arglist_len;
    arglist_len = Xen_list_length(arglist);
    if (arglist_len > 10) clm_error(S_make_wave_train, "too many arguments!", arglist);
    for (i = 0, p = arglist; i < arglist_len; i++, p = Xen_cdr(p)) args[i] = Xen_car(p);
    for (i = arglist_len; i < 10; i++) args[i] = Xen_undefined;
  }

  vals = mus_optkey_unscramble(S_make_wave_train, 5, keys, args, orig_arg);
  if (vals > 0)
    {
      vct *v = NULL;
      freq = Xen_optkey_to_float(kw_frequency, keys[0], S_make_wave_train, orig_arg[0], freq);
      if (freq > (0.5 * mus_srate()))
	Xen_out_of_range_error(S_make_wave_train, orig_arg[0], keys[0], "freq > srate/2?");
      if (freq < 0.0)
	Xen_out_of_range_error(S_make_wave_train, orig_arg[0], keys[0], "freq < 0.0?");

      phase = Xen_optkey_to_float(kw_initial_phase, keys[1], S_make_wave_train, orig_arg[1], phase);
      if (phase < 0.0)
	Xen_out_of_range_error(S_make_wave_train, orig_arg[1], keys[1], "phase < 0.0?");

      v = mus_optkey_to_vct(keys[2], S_make_wave_train, orig_arg[2], NULL);
      if (v)
	{
	  orig_v = keys[2];
	  wave = mus_vct_data(v);
	  wsize = mus_vct_length(v);
	}

      wsize = Xen_optkey_to_mus_long_t(kw_size, keys[3], S_make_wave_train, orig_arg[3], wsize);
      if (wsize <= 0)
	Xen_out_of_range_error(S_make_wave_train, orig_arg[3], keys[3], "size <= 0?");
      if (wsize > mus_max_table_size())
	Xen_out_of_range_error(S_make_wave_train, orig_arg[3], keys[3], "size too large (see mus-max-table-size)");
      if ((v) && (wsize > mus_vct_length(v)))
	Xen_out_of_range_error(S_make_wave_train, orig_arg[3], keys[3], "table size > wave size");

      interp_type = Xen_optkey_to_int(kw_type, keys[4], S_make_wave_train, orig_arg[4], interp_type);
      if (!(mus_is_interp_type(interp_type)))
	Xen_out_of_range_error(S_make_wave_train, orig_arg[4], keys[4], "no such interp-type");
    }

  if (wave == NULL) 
    {
      wave = (mus_float_t *)calloc(wsize, sizeof(mus_float_t));
      if (wave == NULL)
	return(clm_mus_error(MUS_MEMORY_ALLOCATION_FAILED, "can't allocate wave-train table", S_make_wave_train));
      orig_v = xen_make_vct(wsize, wave);
    }
  ge = mus_make_wave_train(freq, phase, wave, wsize, (mus_interp_t)interp_type);
  return(mus_xen_to_object(mus_any_to_mus_xen_with_vct(ge, orig_v)));
}


static Xen g_wave_train(Xen obj, Xen fm)
{
  #define H_wave_train "(" S_wave_train " gen (fm 0.0)): next sample of " S_wave_train
  mus_float_t fm1 = 0.0;
  mus_any *g = NULL;
  mus_xen *gn;

  Xen_to_C_generator(obj, gn, g, mus_is_wave_train, S_wave_train, "a wave-train generator");
  Xen_real_to_C_double_if_bound(fm, fm1, S_wave_train, 2);

  return(C_double_to_Xen_real(mus_wave_train(g, fm1)));
}


static Xen g_is_wave_train(Xen obj) 
{
  #define H_is_wave_train "(" S_is_wave_train " gen): " PROC_TRUE " if gen is a " S_wave_train
  return(C_bool_to_Xen_boolean((mus_is_xen(obj)) && (mus_is_wave_train(Xen_to_mus_any(obj)))));
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


static mus_float_t *list_to_partials(Xen harms, int *npartials, int *error_code)
{
  int listlen, i, maxpartial = 0, curpartial;
  mus_float_t *partials = NULL;
  Xen lst;

  listlen = Xen_list_length(harms);
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

  if (!(Xen_is_number(Xen_car(harms)))) 
    {
      (*error_code) = NON_NUMBER_IN_LIST;
      return(NULL);
    }
  /* the list is '(partial-number partial-amp ... ) */
  (*error_code) = NO_PROBLEM_IN_LIST;

  for (i = 0, lst = Xen_copy_arg(harms); i < listlen; i += 2, lst = Xen_cddr(lst))
    {
      if ((!(Xen_is_number(Xen_car(lst)))) ||
	  (!(Xen_is_number(Xen_cadr(lst)))))
	{
	  (*error_code) = NON_NUMBER_IN_LIST;
	  return(NULL);
	}
      curpartial = Xen_integer_to_C_int(Xen_car(lst));
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
  /* here and elsewhere? this won't be null until we touch it in linux, but that gloms up all our
   *   code with once-in-a-billion-years error checks.
   */
  if (partials == NULL)
    mus_error(MUS_MEMORY_ALLOCATION_FAILED, "can't allocate waveshaping partials list");
  (*npartials) = maxpartial + 1;

  for (i = 0, lst = Xen_copy_arg(harms); i < listlen; i += 2, lst = Xen_cddr(lst))
    {
      curpartial = Xen_integer_to_C_int(Xen_car(lst));
      partials[curpartial] = (mus_float_t)Xen_real_to_C_double(Xen_cadr(lst));
    }
  return(partials);
}


static mus_float_t *mus_vct_to_partials(vct *v, int *npartials, int *error_code)
{
  int len, i, maxpartial, curpartial;
  mus_float_t *partials = NULL, *vdata;

  len = mus_vct_length(v);
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

  vdata = mus_vct_data(v);
  maxpartial = (int)(vdata[0]);
  if (maxpartial < 0)
    (*error_code) = NEGATIVE_NUMBER_IN_LIST;
  else
    {
      for (i = 2; i < len; i += 2)
	{
	  curpartial = (int)(vdata[i]);
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
      curpartial = (int)(vdata[i]);
      partials[curpartial] = vdata[i + 1];
    }
  return(partials);
}


static mus_float_t *mus_vector_to_partials(Xen v, int *npartials, int *error_code)
{
  int len, i, maxpartial, curpartial;
  mus_float_t *partials = NULL;

  len = Xen_vector_length(v);
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

  maxpartial = (int)(Xen_integer_to_C_int(Xen_vector_ref(v, 0)));
  if (maxpartial < 0)
    (*error_code) = NEGATIVE_NUMBER_IN_LIST;
  else
    {
      for (i = 2; i < len; i += 2)
	{
	  curpartial = (int)(Xen_integer_to_C_int(Xen_vector_ref(v, i)));
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
      curpartial = (int)(Xen_integer_to_C_int(Xen_vector_ref(v, i)));
      partials[curpartial] = Xen_real_to_C_double(Xen_vector_ref(v, i + 1));
    }
  return(partials);
}


static Xen g_partials_to_polynomial(Xen amps, Xen ukind)
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

  Xen_check_type(mus_is_vct(amps) || Xen_is_list(amps), amps, 1, S_partials_to_polynomial, "a list or a " S_vct);
  Xen_check_type(Xen_is_integer_or_unbound(ukind), ukind, 2, S_partials_to_polynomial, "either " S_mus_chebyshev_first_kind " or " S_mus_chebyshev_second_kind);

  if (Xen_is_integer(ukind))
    {
      int ck;
      ck = Xen_integer_to_C_int(ukind);
      if ((ck >= MUS_CHEBYSHEV_EITHER_KIND) && (ck <= MUS_CHEBYSHEV_SECOND_KIND))
	kind = (mus_polynomial_t)ck;
      else Xen_out_of_range_error(S_partials_to_polynomial, 2, ukind, "unknown Chebyshev polynomial kind");
    }
  
  if (mus_is_vct(amps))
    partials = mus_vct_to_partials(Xen_to_vct(amps), &npartials, &error);
  else partials = list_to_partials(amps, &npartials, &error);

  if (partials == NULL)
    Xen_error(NO_DATA, 
	      Xen_list_3(C_string_to_Xen_string(list_to_partials_error_to_string(error)), 
			 C_string_to_Xen_string(S_partials_to_polynomial), 
			 amps));

  wave = mus_partials_to_polynomial(npartials, partials, kind); /* wave == partials; in both vct and list cases, partials is newly allocated */
  return(xen_make_vct(npartials, wave));
}


static Xen g_normalize_partials(Xen partials)
{
  #define H_normalize_partials "(" S_normalize_partials " partials) scales the \
partial amplitudes in the " S_vct " or list 'partials' by the inverse of their sum (so that they add to 1.0)."

  vct *v;
  Xen xv = Xen_false;

  Xen_check_type(((Xen_is_list(partials)) && (!Xen_is_null(partials))) || (mus_is_vct(partials)), partials, 1, S_normalize_partials, "a " S_vct " or (non-empty) list");

  if (mus_is_vct(partials))
    xv = partials;
  else xv = xen_list_to_vct(partials);
  v = Xen_to_vct(xv);

  if ((mus_vct_length(v) > 1) &&
      ((mus_vct_length(v) & 1) == 0))
    mus_normalize_partials(mus_vct_length(v) / 2, mus_vct_data(v));
  else Xen_error(BAD_TYPE,
		 Xen_list_3(C_string_to_Xen_string("~A: partials, ~A, must be a non-empty list or " S_vct " of even length (partial-number partial-amp ...)"),
			    C_string_to_Xen_string(S_normalize_partials),
			    partials));
  return(xv);
}


static mus_float_t *vector_to_float_array(Xen v)
{
  mus_float_t *data;
  mus_long_t i, len;
  len = Xen_vector_length(v);
  data = (mus_float_t *)malloc(len * sizeof(mus_float_t));
  for (i = 0; i < len; i++)
    data[i] = Xen_real_to_C_double(Xen_vector_ref(v, i));
  return(data);
}

static Xen g_chebyshev_tu_sum(Xen x, Xen tn, Xen un)
{
  #define H_chebyshev_tu_sum "(" S_mus_chebyshev_tu_sum " x tn un) returns the sum of the weighted\
Chebyshev polynomials Tn and Un (vectors or " S_vct "s), with phase x."

  bool need_free = false;
  int len = 0;
  mus_float_t *tdata = NULL, *udata = NULL;
  Xen result;
  
  Xen_check_type(Xen_is_double(x), x, 1, S_mus_chebyshev_tu_sum, "a float");

  if ((mus_is_vct(tn)) &&
      (mus_is_vct(un)))
    {
      vct *Tn, *Un;
      Tn = Xen_to_vct(tn);
      tdata = mus_vct_data(Tn);
      Un = Xen_to_vct(un);
      udata = mus_vct_data(Un);
      len = mus_vct_length(Tn);
      if (len == 0) return(C_double_to_Xen_real(0.0));
      if (len != mus_vct_length(Un)) return(C_double_to_Xen_real(0.0));
    }
  else
    {
      if ((Xen_is_vector(tn)) && 
	  (Xen_is_vector(un)))
	{
	  len = Xen_vector_length(tn);
	  if (len == 0) return(C_double_to_Xen_real(0.0));
	  if (len != Xen_vector_length(un)) return(C_double_to_Xen_real(0.0));
	  tdata = vector_to_float_array(tn);
	  udata = vector_to_float_array(un);
	  need_free = true;
	}
      else
	{
	  Xen_check_type(false, tn, 1, S_mus_chebyshev_tu_sum, "both arrays should be either " S_vct "s or vectors");
	}
    }

  result = C_double_to_Xen_real(mus_chebyshev_tu_sum(Xen_real_to_C_double(x), len, tdata, udata));
  if (need_free)
    {
      free(tdata);
      free(udata);
    }

  return(result);
}


static Xen g_chebyshev_t_sum(Xen x, Xen tn)
{
  #define H_chebyshev_t_sum "(" S_mus_chebyshev_t_sum " x tn) returns the sum of the weighted \
Chebyshev polynomials Tn (a " S_vct ")."

  bool need_free = false;
  int len = 0;
  mus_float_t *data = NULL;
  Xen result;
  
  Xen_check_type(Xen_is_double(x), x, 1, S_mus_chebyshev_t_sum, "a float");
  if (mus_is_vct(tn))
    {
      vct *Tn;
      Tn = Xen_to_vct(tn);
      data = mus_vct_data(Tn);
      len = mus_vct_length(Tn);
      if (len == 0) return(C_double_to_Xen_real(0.0));
    }
  else
    {
      if (Xen_is_vector(tn))
	{
	  len = Xen_vector_length(tn);
	  if (len == 0) return(C_double_to_Xen_real(0.0));
	  data = vector_to_float_array(tn);
	  need_free = true;
	}
      else Xen_check_type(false, tn, 1, S_mus_chebyshev_t_sum, "a " S_vct " or a vector");
    }
  result = C_double_to_Xen_real(mus_chebyshev_t_sum(Xen_real_to_C_double(x), len, data));
  if (need_free)
    free(data);
  return(result);
}


static Xen g_chebyshev_u_sum(Xen x, Xen un)
{
  #define H_chebyshev_u_sum "(" S_mus_chebyshev_u_sum " x un) returns the sum of the weighted \
Chebyshev polynomials Un (a " S_vct ")."

  bool need_free = false;
  int len = 0;
  mus_float_t *data = NULL;
  Xen result;

  Xen_check_type(Xen_is_double(x), x, 1, S_mus_chebyshev_u_sum, "a float");

  if (mus_is_vct(un))
    {
      vct *Un;
      Un = Xen_to_vct(un);
      len = mus_vct_length(Un);
      if (len == 0) return(C_double_to_Xen_real(0.0));
      data = mus_vct_data(Un);
    }
  else
    {
      if (Xen_is_vector(un))
	{
	  len = Xen_vector_length(un);
	  if (len == 0) return(C_double_to_Xen_real(0.0));
	  data = vector_to_float_array(un);
	  need_free = true;
	}
      else Xen_check_type(false, un, 1, S_mus_chebyshev_u_sum, "a " S_vct " or a vector");
    }
  result = C_double_to_Xen_real(mus_chebyshev_u_sum(Xen_real_to_C_double(x), len, data));
  if (need_free)
    free(data);
  return(result);
}




/* ---------------- polyshape ---------------- */

static Xen g_polyshape(Xen obj, Xen index, Xen fm)
{
  #define H_polyshape "(" S_polyshape " gen (index 1.0) (fm 0.0)): next sample of polynomial-based waveshaper"
  mus_float_t fm1 = 0.0, index1 = 1.0;
  mus_any *g = NULL;
  mus_xen *gn;

  Xen_to_C_generator(obj, gn, g, mus_is_polyshape, S_polyshape, "a polyshape generator");
  Xen_real_to_C_double_if_bound(index, index1, S_polyshape, 2);
  Xen_real_to_C_double_if_bound(fm, fm1, S_polyshape, 3);

  return(C_double_to_Xen_real(mus_polyshape(g, index1, fm1)));
}


static Xen g_is_polyshape(Xen obj) 
{
  #define H_is_polyshape "(" S_is_polyshape " gen): " PROC_TRUE " if gen is a " S_polyshape
  return(C_bool_to_Xen_boolean((mus_is_xen(obj)) && (mus_is_polyshape(Xen_to_mus_any(obj)))));
}


static Xen g_make_polyshape(Xen arglist)
{
  #define H_make_polyshape "(" S_make_polyshape " (frequency *clm-default-frequency*) (initial-phase 0.0) (coeffs) (partials '(1 1)) (kind " S_mus_chebyshev_first_kind ")): \
return a new polynomial-based waveshaping generator:\n\
   (" S_make_polyshape " :coeffs (" S_partials_to_polynomial " '(1 1.0)))\n\
is the same in effect as " S_make_oscil

  mus_any *ge;
  Xen args[10];
  Xen keys[5];
  int orig_arg[5] = {0, 0, 0, 0, 0};
  int vals, csize = 0, npartials = 0;
  Xen orig_v = Xen_false;
  mus_float_t freq, phase = 0.0; 
  /* 
   * if we followed the definition directly, the initial phase default would be M_PI_2 (pi/2) so that
   *   we drive the Tn's with a cosine.  But I've always used sine instead, so I think I'll leave
   *   it that way.  There is no difference in the output waveform except an overall phase
   *   offset.  So, with sine, the phases rotate through cos sin -cos -sin... rather than being all cos,
   *   but these add to exactly the same actual wave -- what you'd expect since Tn doesn't know
   *   where we started.  This also does not affect "signification".
   */
  mus_float_t *coeffs = NULL;

  mus_polynomial_t kind = MUS_CHEBYSHEV_FIRST_KIND;
  freq = clm_default_frequency;

  keys[0] = kw_frequency;
  keys[1] = kw_initial_phase;
  keys[2] = kw_coeffs;
  keys[3] = kw_partials;
  keys[4] = kw_kind;

  {
    int i, arglist_len;
    Xen p;
    arglist_len = Xen_list_length(arglist);
    if (arglist_len > 10) clm_error(S_make_polyshape, "too many arguments!", arglist);
    for (i = 0, p = arglist; i < arglist_len; i++, p = Xen_cdr(p)) args[i] = Xen_car(p);
    for (i = arglist_len; i < 10; i++) args[i] = Xen_undefined;
  }

  vals = mus_optkey_unscramble(S_make_polyshape, 5, keys, args, orig_arg);
  if (vals > 0)
    {
      vct *v = NULL;
      int ck;

      freq = Xen_optkey_to_float(kw_frequency, keys[0], S_make_polyshape, orig_arg[0], freq);
      if (freq > (0.5 * mus_srate()))
	Xen_out_of_range_error(S_make_polyshape, orig_arg[0], keys[0], "freq > srate/2?");

      phase = Xen_optkey_to_float(kw_initial_phase, keys[1], S_make_polyshape, orig_arg[2], phase);

      ck = Xen_optkey_to_int(kw_kind, keys[4], S_make_polyshape, orig_arg[4], (int)kind);
      if ((ck >= MUS_CHEBYSHEV_EITHER_KIND) && (ck <= MUS_CHEBYSHEV_SECOND_KIND))
	kind = (mus_polynomial_t)ck;
      else Xen_out_of_range_error(S_make_polyshape, orig_arg[4], keys[4], "unknown Chebyshev polynomial kind");

      v = mus_optkey_to_vct(keys[2], S_make_polyshape, orig_arg[2], NULL);
      if (v)
        {
	  orig_v = keys[2];
	  coeffs = mus_vct_data(v);
	  csize = mus_vct_length(v);
	}
      else
	{
	  if (!(Xen_is_keyword(keys[3])))
	    {
	      mus_float_t *partials = NULL;
	      int error = NO_PROBLEM_IN_LIST;
	      if (mus_is_vct(keys[3]))
		partials = mus_vct_to_partials(Xen_to_vct(keys[3]), &npartials, &error);
	      else
		{
		  Xen_check_type(Xen_is_list(keys[3]), keys[3], orig_arg[3], S_make_polyshape, "a list or a " S_vct);
		  partials = list_to_partials(keys[3], &npartials, &error);
		}
	      if (partials == NULL)
		Xen_error(NO_DATA, 
			  Xen_list_3(C_string_to_Xen_string(list_to_partials_error_to_string(error)), 
				     C_string_to_Xen_string(S_make_polyshape), 
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
      data = (mus_float_t *)malloc(2 * sizeof(mus_float_t));
      data[0] = 0.0;
      data[1] = 1.0;
      coeffs = mus_partials_to_polynomial(2, data, kind);
      csize = 2;
    }

  if (Xen_is_false(orig_v))
    orig_v = xen_make_vct(csize, coeffs);

  ge = mus_make_polyshape(freq, phase, coeffs, csize, kind);
  if (ge) 
    return(mus_xen_to_object(mus_any_to_mus_xen_with_vct(ge, orig_v)));
  return(Xen_false);
}



/* ---------------- polywave ---------------- */

static Xen g_polywave(Xen obj, Xen fm)
{
  #define H_polywave "(" S_polywave " gen (fm 0.0)): next sample of polywave waveshaper"
  mus_float_t fm1 = 0.0;
  mus_any *g = NULL;
  mus_xen *gn;

  Xen_to_C_generator(obj, gn, g, mus_is_polywave, S_polywave, "a polywave generator");
  Xen_real_to_C_double_if_bound(fm, fm1, S_polywave, 3);

  return(C_double_to_Xen_real(mus_polywave(g, fm1)));
}


static Xen g_is_polywave(Xen obj) 
{
  #define H_is_polywave "(" S_is_polywave " gen): " PROC_TRUE " if gen is a " S_polywave " generator"
  return(C_bool_to_Xen_boolean((mus_is_xen(obj)) && (mus_is_polywave(Xen_to_mus_any(obj)))));
}


static Xen g_make_polywave(Xen arglist)
{
  #define H_make_polywave "(" S_make_polywave " (frequency *clm-default-frequency*) (partials '(1 1)) (type " S_mus_chebyshev_first_kind ") xcoeffs ycoeffs): \
return a new polynomial-based waveshaping generator.  (" S_make_polywave " :partials (float-vector 1 1.0)) is the same in effect as " S_make_oscil "."

  mus_any *ge;
  Xen args[10];
  Xen keys[5];
  int orig_arg[5] = {0, 0, 0, 0, 0};
  int vals, n = 0, npartials = 0;
  Xen orig_x = Xen_false, orig_y = Xen_false;
  mus_float_t freq; 
  mus_float_t *xcoeffs = NULL, *ycoeffs = NULL, *partials = NULL;
  mus_polynomial_t kind = MUS_CHEBYSHEV_FIRST_KIND;
  int error = NO_PROBLEM_IN_LIST;

  freq = clm_default_frequency;

  keys[0] = kw_frequency;
  keys[1] = kw_partials;
  keys[2] = kw_type;
  keys[3] = kw_x_coeffs;
  keys[4] = kw_y_coeffs;

  {
    int i, arglist_len;
    Xen p;
    arglist_len = Xen_list_length(arglist);
    if (arglist_len > 10) clm_error(S_make_polywave, "too many arguments!", arglist);
    for (i = 0, p = arglist; i < arglist_len; i++, p = Xen_cdr(p)) args[i] = Xen_car(p);
    for (i = arglist_len; i < 10; i++) args[i] = Xen_undefined;
  }

  vals = mus_optkey_unscramble(S_make_polywave, 5, keys, args, orig_arg);
  if (vals > 0)
    {
      vct *v;
      int type;
      freq = Xen_optkey_to_float(kw_frequency, keys[0], S_make_polywave, orig_arg[0], freq);
      if (freq > (0.5 * mus_srate()))
	Xen_out_of_range_error(S_make_polywave, orig_arg[0], keys[0], "freq > srate/2?");

      type = Xen_optkey_to_int(kw_type, keys[2], S_make_polywave, orig_arg[2], (int)kind);
      if ((type >= MUS_CHEBYSHEV_EITHER_KIND) && 
	  (type <= MUS_CHEBYSHEV_BOTH_KINDS))
	kind = (mus_polynomial_t)type;
      else Xen_out_of_range_error(S_make_polywave, orig_arg[2], keys[2], "unknown Chebyshev polynomial kind");

      if (!(Xen_is_keyword(keys[1]))) /* partials were supplied */
	{
	  if (mus_is_vct(keys[1]))
	    partials = mus_vct_to_partials(Xen_to_vct(keys[1]), &npartials, &error);
	  else
	    {
	      if (Xen_is_vector(keys[1]))
		partials = mus_vector_to_partials(keys[1], &npartials, &error);
	      else
		{
		  Xen_check_type(Xen_is_list(keys[1]), keys[1], orig_arg[1], S_make_polywave, "a list or a " S_vct);
		  partials = list_to_partials(keys[1], &npartials, &error);
		}
	    }
	  if (partials == NULL) /* here if null, something went wrong in the translation functions */
	    Xen_error(NO_DATA, 
		      Xen_list_3(C_string_to_Xen_string(list_to_partials_error_to_string(error)), 
				 C_string_to_Xen_string(S_make_polywave), 
				 keys[1]));

	  xcoeffs = partials;
	  n = npartials;
	  orig_x = xen_make_vct(n, xcoeffs);
	  /* xcoeffs = partials here, so don't delete */ 
	}

      if (!(Xen_is_keyword(keys[3])))
        {
	  Xen_check_type(mus_is_vct(keys[3]), keys[3], orig_arg[3], S_make_polywave, "a " S_vct);
	  orig_x = keys[3];
	  v = Xen_to_vct(orig_x);
	  n = mus_vct_length(v);
	  xcoeffs = mus_vct_data(v);
        }
      
      if (!(Xen_is_keyword(keys[4])))
	{
	  /* make-polyoid in generators.scm */
	  int yn;
	  Xen_check_type(mus_is_vct(keys[4]), keys[4], orig_arg[4], S_make_polywave, "a " S_vct);
	  orig_y = keys[4];
	  v = Xen_to_vct(orig_y);
	  yn = mus_vct_length(v);
	  if ((n == 0) || (yn < n))
	    n = yn;
	  ycoeffs = mus_vct_data(v);
	}
    }

  if (!xcoeffs)
    {
      /* clm.html says '(1 1) is the default but table-lookup is 0? */
      mus_float_t *data;
      data = (mus_float_t *)malloc(2 * sizeof(mus_float_t));
      data[0] = 0.0;
      data[1] = 1.0;
      xcoeffs = data;
      n = 2; 
      orig_x = xen_make_vct(n, xcoeffs);
    }

  if (ycoeffs)
    {
      ge = mus_make_polywave_tu(freq, xcoeffs, ycoeffs, n);
      if (ge) return(mus_xen_to_object(mus_any_to_mus_xen_with_two_vcts(ge, orig_x, orig_y)));
    }
  ge = mus_make_polywave(freq, xcoeffs, n, kind);
  if (ge) return(mus_xen_to_object(mus_any_to_mus_xen_with_vct(ge, orig_x)));
  return(Xen_false);
}


/* ---------------- nrxysin and nrxycos ---------------- */

static Xen g_is_nrxysin(Xen obj) 
{
  #define H_is_nrxysin "(" S_is_nrxysin " gen): " PROC_TRUE " if gen is an " S_nrxysin " generator"
  return(C_bool_to_Xen_boolean((mus_is_xen(obj)) && 
			  (mus_is_nrxysin(Xen_to_mus_any(obj)))));
}


static Xen g_is_nrxycos(Xen obj) 
{
  #define H_is_nrxycos "(" S_is_nrxycos " gen): " PROC_TRUE " if gen is an " S_nrxycos " generator"
  return(C_bool_to_Xen_boolean((mus_is_xen(obj)) && 
			  (mus_is_nrxycos(Xen_to_mus_any(obj)))));
}


static Xen g_nrxysin(Xen obj, Xen fm)
{
  #define H_nrxysin "(" S_nrxysin " gen (fm 0.0)): next sample of nrxysin generator"
  mus_float_t fm1 = 0.0;
  mus_any *g = NULL;
  mus_xen *gn;

  Xen_to_C_generator(obj, gn, g, mus_is_nrxysin, S_nrxysin, "an nrxysin generator");
  Xen_real_to_C_double_if_bound(fm, fm1, S_nrxysin, 2);

  return(C_double_to_Xen_real(mus_nrxysin(g, fm1)));
}

static Xen g_nrxycos(Xen obj, Xen fm)
{
  #define H_nrxycos "(" S_nrxycos " gen (fm 0.0)): next sample of nrxycos generator"
  mus_float_t fm1 = 0.0;
  mus_any *g = NULL;
  mus_xen *gn;

  Xen_to_C_generator(obj, gn, g, mus_is_nrxycos, S_nrxycos, "an nrxycos generator");
  Xen_real_to_C_double_if_bound(fm, fm1, S_nrxycos, 2);

  return(C_double_to_Xen_real(mus_nrxycos(g, fm1)));
}


static Xen g_make_nrxy(bool sin_case, const char *caller, Xen arglist)
{
  mus_any *ge;
  Xen args[8];
  Xen keys[4];
  int orig_arg[4] = {0, 0, 0, 0};
  int vals;
  mus_float_t freq, r = 0.5, ratio = 1.0;
  int n = 1;

  freq = clm_default_frequency;

  keys[0] = kw_frequency;
  keys[1] = kw_ratio;
  keys[2] = kw_n;
  keys[3] = kw_r;

  {
    int i, arglist_len;
    Xen p;
    arglist_len = Xen_list_length(arglist);
    if (arglist_len > 8) clm_error(caller, "too many arguments!", arglist);
    for (i = 0, p = arglist; i < arglist_len; i++, p = Xen_cdr(p)) args[i] = Xen_car(p);
    for (i = arglist_len; i < 8; i++) args[i] = Xen_undefined;
  }

  vals = mus_optkey_unscramble(caller, 4, keys, args, orig_arg);
  if (vals > 0)
    {
      freq = Xen_optkey_to_float(kw_frequency, keys[0], caller, orig_arg[0], freq);
      if (freq > (0.5 * mus_srate()))
	Xen_out_of_range_error(caller, orig_arg[0], keys[0], "freq > srate/2?");

      ratio = Xen_optkey_to_float(kw_ratio, keys[1], caller, orig_arg[1], ratio);

      n = Xen_optkey_to_int(kw_n, keys[2], caller, orig_arg[2], n);
      if (n < 0)
	Xen_out_of_range_error(caller, orig_arg[2], keys[2], "n (sidebands) < 0?");

      r = Xen_optkey_to_float(kw_r, keys[3], caller, orig_arg[3], r);
      if ((r >= 1.0) ||
	  (r <= -1.0))
	Xen_out_of_range_error(caller, orig_arg[3], keys[3], "r (sideband amp ratio) not within -1.0 to 1.0?");
      /* if not with doubles, this actually maxes out around .99999999 because mus_optkey_to_float (apparently) rounds up */
    }
  if (sin_case)
    ge = mus_make_nrxysin(freq, ratio, n, r);
  else ge = mus_make_nrxycos(freq, ratio, n, r);
  if (ge) return(mus_xen_to_object(mus_any_to_mus_xen(ge)));
  return(Xen_false);
}


static Xen g_make_nrxysin(Xen arglist)
{
  #define H_make_nrxysin "(" S_make_nrxysin " (frequency *clm-default-frequency*) (ratio 1.0) (n 1) (r 0.5)): \
return a new nrxysin generator."

  return(g_make_nrxy(true, S_make_nrxysin, arglist));
}

static Xen g_make_nrxycos(Xen arglist)
{
  #define H_make_nrxycos "(" S_make_nrxycos " (frequency *clm-default-frequency*) (ratio 1.0) (n 1) (r 0.5)): \
return a new nrxycos generator."

  return(g_make_nrxy(false, S_make_nrxycos, arglist));
}



/* ---------------- rxyksin and rxykcos ---------------- */

static Xen g_is_rxyksin(Xen obj) 
{
  #define H_is_rxyksin "(" S_is_rxyksin " gen): " PROC_TRUE " if gen is an " S_rxyksin " generator"
  return(C_bool_to_Xen_boolean((mus_is_xen(obj)) && 
			  (mus_is_rxyksin(Xen_to_mus_any(obj)))));
}


static Xen g_is_rxykcos(Xen obj) 
{
  #define H_is_rxykcos "(" S_is_rxykcos " gen): " PROC_TRUE " if gen is an " S_rxykcos " generator"
  return(C_bool_to_Xen_boolean((mus_is_xen(obj)) && 
			  (mus_is_rxykcos(Xen_to_mus_any(obj)))));
}


static Xen g_rxyksin(Xen obj, Xen fm)
{
  #define H_rxyksin "(" S_rxyksin " gen (fm 0.0)): next sample of rxyksin generator"
  mus_float_t fm1 = 0.0;
  mus_any *g = NULL;
  mus_xen *gn;

  Xen_to_C_generator(obj, gn, g, mus_is_rxyksin, S_rxyksin, "an rxyksin generator");
  Xen_real_to_C_double_if_bound(fm, fm1, S_rxyksin, 2);

  return(C_double_to_Xen_real(mus_rxyksin(g, fm1)));
}

static Xen g_rxykcos(Xen obj, Xen fm)
{
  #define H_rxykcos "(" S_rxykcos " gen (fm 0.0)): next sample of rxykcos generator"
  mus_float_t fm1 = 0.0;
  mus_any *g = NULL;
  mus_xen *gn;

  Xen_to_C_generator(obj, gn, g, mus_is_rxykcos, S_rxykcos, "an rxykcos generator");
  Xen_real_to_C_double_if_bound(fm, fm1, S_rxykcos, 2);

  return(C_double_to_Xen_real(mus_rxykcos(g, fm1)));
}


static Xen g_make_rxyk(bool sin_case, const char *caller, Xen arglist)
{
  mus_any *ge;
  Xen args[6];
  Xen keys[3];
  int orig_arg[3] = {0, 0, 0};
  int vals;
  mus_float_t freq, r = 0.5, ratio = 1.0; /* original in generators.scm assumes initial-phase = 0.0 */

  freq = clm_default_frequency;

  keys[0] = kw_frequency;
  keys[1] = kw_ratio;
  keys[2] = kw_r;

  {
    int i, arglist_len;
    Xen p;
    arglist_len = Xen_list_length(arglist);
    if (arglist_len > 6) clm_error(caller, "too many arguments!", arglist);
    for (i = 0, p = arglist; i < arglist_len; i++, p = Xen_cdr(p)) args[i] = Xen_car(p);
    for (i = arglist_len; i < 6; i++) args[i] = Xen_undefined;
  }

  vals = mus_optkey_unscramble(caller, 3, keys, args, orig_arg);
  if (vals > 0)
    {
      freq = Xen_optkey_to_float(kw_frequency, keys[0], caller, orig_arg[0], freq);
      if (freq > (0.5 * mus_srate()))
	Xen_out_of_range_error(caller, orig_arg[0], keys[0], "freq > srate/2?");

      ratio = Xen_optkey_to_float(kw_ratio, keys[1], caller, orig_arg[1], ratio);
      r = Xen_optkey_to_float(kw_r, keys[2], caller, orig_arg[2], r);
    }
  if (sin_case)
    ge = mus_make_rxyksin(freq, 0.0, r, ratio);
  else ge = mus_make_rxykcos(freq, 0.0, r, ratio);
  if (ge) return(mus_xen_to_object(mus_any_to_mus_xen(ge)));
  return(Xen_false);
}


static Xen g_make_rxyksin(Xen arglist)
{
  #define H_make_rxyksin "(" S_make_rxyksin " (frequency *clm-default-frequency*) (initial-phase 0.0) (ratio 1.0) (r 0.5)): \
return a new rxyksin generator."

  return(g_make_rxyk(true, S_make_rxyksin, arglist));
}

static Xen g_make_rxykcos(Xen arglist)
{
  #define H_make_rxykcos "(" S_make_rxykcos " (frequency *clm-default-frequency*) (initial-phase 0.0) (ratio 1.0) (r 0.5)): \
return a new rxykcos generator."

  return(g_make_rxyk(false, S_make_rxykcos, arglist));
}



/* ----------------  filter ---------------- */

typedef enum {G_FILTER, G_FIR_FILTER, G_IIR_FILTER} xclm_fir_t;

static Xen g_make_fir_coeffs(Xen order, Xen envl)
{
  #define H_make_fir_coeffs "(" S_make_fir_coeffs " order v): turn spectral envelope in " S_vct " v into coeffs for FIR filter"
  int size;
  mus_float_t *a;
  vct *v;

  Xen_check_type(Xen_is_integer(order), order, 1, S_make_fir_coeffs, "int");
  Xen_check_type(mus_is_vct(envl), envl, 2, S_make_fir_coeffs, "a " S_vct);

  v = Xen_to_vct(envl);

  size = Xen_integer_to_C_int(order);
  if (size != mus_vct_length(v))
    Xen_error(CLM_ERROR,
	      Xen_list_3(C_string_to_Xen_string(S_make_fir_coeffs ": order ~A != " S_vct " length ~A"),
			 order, 
			 envl));

  a = mus_make_fir_coeffs(Xen_integer_to_C_int(order), mus_vct_data(v), NULL);
  return(xen_make_vct(mus_vct_length(v), a));
}


static Xen g_is_filter(Xen obj) 
{
  #define H_is_filter "(" S_is_filter " gen): " PROC_TRUE " if gen is a " S_filter
  return(C_bool_to_Xen_boolean((mus_is_xen(obj)) && (mus_is_filter(Xen_to_mus_any(obj)))));
}


static Xen g_is_fir_filter(Xen obj) 
{
  #define H_is_fir_filter "(" S_is_fir_filter " gen): " PROC_TRUE " if gen is an " S_fir_filter
  return(C_bool_to_Xen_boolean((mus_is_xen(obj)) && (mus_is_fir_filter(Xen_to_mus_any(obj)))));
}


static Xen g_is_iir_filter(Xen obj) 
{
  #define H_is_iir_filter "(" S_is_iir_filter " gen): " PROC_TRUE " if gen is an " S_iir_filter
  return(C_bool_to_Xen_boolean((mus_is_xen(obj)) && (mus_is_iir_filter(Xen_to_mus_any(obj)))));
}


static Xen g_filter(Xen obj, Xen input)
{
  #define H_filter "(" S_filter " gen (input 0.0)): next sample from filter"
  mus_any *g = NULL;
  mus_xen *gn;
  mus_float_t x = 0.0;

  Xen_to_C_generator(obj, gn, g, mus_is_filter, S_filter, "a filter");
  Xen_real_to_C_double_if_bound(input, x, S_filter, 2);

  return(C_double_to_Xen_real(mus_filter(g, x)));
}


static Xen g_fir_filter(Xen obj, Xen input)
{
  #define H_fir_filter "(" S_fir_filter " gen (input 0.0)): next sample from FIR filter"
  mus_any *g = NULL;
  mus_xen *gn;
  mus_float_t x = 0.0;

  Xen_to_C_generator(obj, gn, g, mus_is_fir_filter, S_fir_filter, "an FIR filter");
  Xen_real_to_C_double_if_bound(input, x, S_fir_filter, 2);

  return(C_double_to_Xen_real(mus_fir_filter(g, x)));
}


static Xen g_iir_filter(Xen obj, Xen input)
{
  #define H_iir_filter "(" S_iir_filter " gen (input 0.0)): next sample from IIR filter"
  mus_any *g = NULL;
  mus_xen *gn;
  mus_float_t x = 0.0;

  Xen_to_C_generator(obj, gn, g, mus_is_iir_filter, S_iir_filter, "an IIR filter");
  Xen_real_to_C_double_if_bound(input, x, S_iir_filter, 2);

  return(C_double_to_Xen_real(mus_iir_filter(g, x)));
}


static Xen g_make_filter_1(xclm_fir_t choice, Xen arg1, Xen arg2, Xen arg3, Xen arg4, Xen arg5, Xen arg6)
{
  Xen xwave = Xen_undefined, ywave = Xen_undefined;
  mus_any *fgen = NULL;
  Xen args[8]; 
  Xen keys[4];
  int orig_arg[4] = {0, 0, 0, 0};
  vct *x = NULL, *y = NULL;
  int vals, order = 0;
  const char *caller;
  if (choice == G_FILTER) caller = S_make_filter; else if (choice == G_FIR_FILTER) caller = S_make_fir_filter; else caller = S_make_iir_filter;

  keys[0] = kw_order;
  keys[1] = kw_x_coeffs;
  keys[2] = kw_y_coeffs;
  keys[3] = kw_coeffs;
  args[0] = arg1; args[1] = arg2; args[2] = arg3; args[3] = arg4; args[4] = arg5; args[5] = arg6; args[6] = Xen_undefined; args[7] = Xen_undefined;

  vals = mus_optkey_unscramble(caller, 4, keys, args, orig_arg);
  if (vals > 0)
    {
      if (!(Xen_is_keyword(keys[0])))
	{
	  order = Xen_optkey_to_int(kw_order, keys[0], caller, orig_arg[0], 0);
	  if (order <= 0)
	    Xen_out_of_range_error(caller, orig_arg[0], keys[0], "order <= 0?");
	}

      if (!(Xen_is_keyword(keys[1])))
        {
	  Xen_check_type(mus_is_vct(keys[1]), keys[1], orig_arg[1], caller, "a " S_vct);
	  if (choice == G_IIR_FILTER)
	    {
	      ywave = keys[1];
	      y = Xen_to_vct(ywave);
	    }
	  else
	    {
	      xwave = keys[1];
	      x = Xen_to_vct(xwave);
	    }
        }

      if (!(Xen_is_keyword(keys[2])))
	{
	  Xen_check_type(mus_is_vct(keys[2]), keys[2], orig_arg[2], caller, "a " S_vct);
	  ywave = keys[2];
	  y = Xen_to_vct(ywave);
	}

      if ((choice != G_FILTER) && (!(Xen_is_keyword(keys[3]))))
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
    Xen_error(NO_DATA,
	      Xen_list_2(C_string_to_Xen_string("~A: no coeffs?"),
			 C_string_to_Xen_string(caller)));
  if (order == 0)
    {
      if (x)
	order = mus_vct_length(x);
      else order = mus_vct_length(y);
    }
  else
    {
      if ((x) && (order > mus_vct_length(x)))
	{
	  Xen_error(CLM_ERROR,
		    Xen_list_4(C_string_to_Xen_string("~A: xcoeffs, ~A, must match order, ~A"),
			       C_string_to_Xen_string(caller),
			       keys[1],
			       keys[0]));
	}
      else
	{
	  if ((y) && (order > mus_vct_length(y)))
	    Xen_error(CLM_ERROR,
		      Xen_list_4(C_string_to_Xen_string("~A: ycoeffs, ~A, must match order, ~A"),
				 C_string_to_Xen_string(caller),
				 keys[2],
				 keys[0])); 
	  else
	    {
	      if ((x) && (y) && (mus_vct_length(x) != mus_vct_length(y)))
		Xen_error(CLM_ERROR,
			  Xen_list_4(C_string_to_Xen_string("~A: coeffs must be same length.  x len: ~A, y len: ~A"),
				     C_string_to_Xen_string(caller),
				     C_int_to_Xen_integer(mus_vct_length(x)),
				     C_int_to_Xen_integer(mus_vct_length(y))));
	    }
	}
    }
  switch (choice)
    {
    case G_FILTER: fgen = mus_make_filter(order, mus_vct_data(x), mus_vct_data(y), NULL); break;
    case G_FIR_FILTER: fgen = mus_make_fir_filter(order, mus_vct_data(x), NULL); break;
    case G_IIR_FILTER: fgen = mus_make_iir_filter(order, mus_vct_data(y), NULL); break;
    }
  if (fgen)
    {
      mus_xen *gn = NULL;
      gn = mx_alloc(3);
      gn->gen = fgen;                                    /* delay gn allocation since make_filter can throw an error */
      gn->vcts[G_FILTER_STATE] = xen_make_vct_wrapper(order, mus_data(fgen));
      gn->vcts[G_FILTER_XCOEFFS] = xwave;
      gn->vcts[G_FILTER_YCOEFFS] = ywave;
      return(mus_xen_to_object(gn));
    }
  return(Xen_false);
}


static Xen g_make_filter(Xen arg1, Xen arg2, Xen arg3, Xen arg4, Xen arg5, Xen arg6)
{
  #define H_make_filter "(" S_make_filter " order xcoeffs ycoeffs): return a new direct form FIR/IIR filter, coeff args are " S_vct "s"
  return(g_make_filter_1(G_FILTER, arg1, arg2, arg3, arg4, arg5, arg6));
}


static Xen g_make_fir_filter(Xen arg1, Xen arg2, Xen arg3, Xen arg4)
{
  #define H_make_fir_filter "(" S_make_fir_filter " order xcoeffs): return a new FIR filter, xcoeffs a " S_vct
  return(g_make_filter_1(G_FIR_FILTER, arg1, arg2, arg3, arg4, Xen_undefined, Xen_undefined));
}


static Xen g_make_iir_filter(Xen arg1, Xen arg2, Xen arg3, Xen arg4)
{
  #define H_make_iir_filter "(" S_make_iir_filter " order ycoeffs): return a new IIR filter, ycoeffs a " S_vct
  return(g_make_filter_1(G_IIR_FILTER, arg1, arg2, arg3, arg4, Xen_undefined, Xen_undefined));
}




/* ---------------- env ---------------- */

static Xen g_is_env(Xen obj) 
{
  #define H_is_env "(" S_is_env " gen): " PROC_TRUE " if gen is a " S_env
  return(C_bool_to_Xen_boolean((mus_is_xen(obj)) && (mus_is_env(Xen_to_mus_any(obj)))));
}


static Xen g_env(Xen obj) 
{
  #define H_env "(" S_env " gen): next sample from envelope generator"
  mus_any *g = NULL;
  mus_xen *gn;

  Xen_to_C_generator(obj, gn, g, mus_is_env, S_env, "an env generator");

  return(C_double_to_Xen_real(mus_env(g)));
}


static Xen g_make_env(Xen arglist)
{
  #define H_make_env "(" S_make_env " envelope (scaler 1.0) (duration) (offset 0.0) (base 1.0) (end) (length)): \
return a new envelope generator.  'envelope' is a list, vector, or " S_vct " of break-point pairs. To create the envelope, \
these points are offset by 'offset', scaled by 'scaler', and mapped over the time interval defined by \
either 'duration' (seconds) or 'length' (samples).  If 'base' is 1.0, the connecting segments \
are linear, if 0.0 you get a step function, and anything else produces an exponential connecting segment."

  mus_any *ge;
  Xen args[14];
  Xen keys[7];
  int orig_arg[7] = {0, 0, 0, 0, 0, 0, 0};
  int vals, i;
  mus_float_t base = 1.0, scaler = 1.0, offset = 0.0, duration = 0.0;
  mus_long_t end = 0, dur = -1;
  int npts = 0;
  mus_float_t *brkpts = NULL;
  vct *v = NULL;

  keys[0] = kw_envelope;
  keys[1] = kw_scaler;
  keys[2] = kw_duration;
  keys[3] = kw_offset;
  keys[4] = kw_base;
  keys[5] = kw_end;
  keys[6] = kw_length;

  {
    int arglist_len;
    Xen p;
    arglist_len = Xen_list_length(arglist);
    if (arglist_len > 14) clm_error(S_make_env, "too many arguments!", arglist);
    for (i = 0, p = arglist; i < arglist_len; i++, p = Xen_cdr(p)) args[i] = Xen_car(p);
    for (i = arglist_len; i < 14; i++) args[i] = Xen_undefined;
  }

  vals = mus_optkey_unscramble(S_make_env, 7, keys, args, orig_arg);
  if (vals > 0)
    {
      scaler = Xen_optkey_to_float(kw_scaler, keys[1], S_make_env, orig_arg[1], 1.0);

      duration = Xen_optkey_to_float(kw_duration, keys[2], S_make_env, orig_arg[2], 0.0);
      if ((duration < 0.0) || ((duration == 0.0) && (!Xen_is_keyword(keys[2]))))
	Xen_out_of_range_error(S_make_env, orig_arg[2], keys[2], "duration <= 0.0?");

      offset = Xen_optkey_to_float(kw_offset, keys[3], S_make_env, orig_arg[3], 0.0);

      base = Xen_optkey_to_float(kw_base, keys[4], S_make_env, orig_arg[4], 1.0);
      if (base < 0.0) 
	Xen_out_of_range_error(S_make_env, orig_arg[4], keys[4], "base < 0.0?");

      end = Xen_optkey_to_mus_long_t(kw_end, keys[5], S_make_env, orig_arg[5], 0);
      if (end < 0) 
	Xen_out_of_range_error(S_make_env, orig_arg[5], keys[5], "end < 0?");

      dur = Xen_optkey_to_mus_long_t(kw_length, keys[6], S_make_env, orig_arg[6], 0);
      if (dur < 0) 
	Xen_out_of_range_error(S_make_env, orig_arg[6], keys[6], "length < 0?");

      /* env data is a list, checked last to let the preceding throw wrong-type error before calloc  */
      if (!(Xen_is_keyword(keys[0])))
        {
	  int len;
	  Xen vect = XEN_NULL;
	  if (mus_is_vct(keys[0]))
	    {
	      v = Xen_to_vct(keys[0]);
	      len = mus_vct_length(v);
	      if ((len < 2) || (len & 1))
		Xen_error(BAD_TYPE,
			  Xen_list_2(C_string_to_Xen_string(S_make_env ": " S_vct " is a bogus breakpoints list, ~A"), 
				     keys[0]));
	    }
	  else
	    {
#if HAVE_SCHEME
	      /* in Ruby and Forth vectors and lists are the same, so stay with the old code */
	      if (Xen_is_vector(keys[0]))
		{
		  vect = keys[0];
		  len = Xen_vector_length(vect);
		  if ((len < 2) || (len & 1))
		    Xen_error(BAD_TYPE, Xen_list_2(C_string_to_Xen_string(S_make_env ": vector is a bogus breakpoints list, ~A"), vect));
		}
	      else
		{
#endif
		  Xen_check_type(Xen_is_list(keys[0]), keys[0], orig_arg[0], S_make_env, "a list, vector, or " S_vct);
		  len = Xen_list_length(keys[0]);
		  if (len == 0)
		    Xen_error(NO_DATA,
			      Xen_list_2(C_string_to_Xen_string(S_make_env ": null env? ~A"), 
					 keys[0]));
		  
		  if (Xen_is_list(Xen_car(keys[0])))
		    len *= 2;
		  else
		    {
		      if (len & 1)
			Xen_error(BAD_TYPE,
				  Xen_list_2(C_string_to_Xen_string(S_make_env ": odd length breakpoints list? ~A"), 
					     keys[0]));
		      
		      if (!(Xen_is_number(Xen_car(keys[0]))))
			Xen_check_type(false, keys[0], orig_arg[0], S_make_env, "a list of numbers (breakpoints)");
		    }
		}
#if HAVE_SCHEME
	    }
#endif
	  npts = len / 2;
	  if (v)
	    brkpts = mus_vct_data(v);
	  else
	    {
	      brkpts = (mus_float_t *)malloc(len * sizeof(mus_float_t));
	      if (brkpts == NULL)
		return(clm_mus_error(MUS_MEMORY_ALLOCATION_FAILED, "can't allocate env list", S_make_env));
	      if (vect)
		{
		  for (i = 0; i < len; i++)
		    brkpts[i] = Xen_real_to_C_double(Xen_vector_ref(vect, i));
		}
	      else
		{
		  Xen lst;
		  if (Xen_is_number(Xen_car(keys[0])))
		    {
		      for (i = 0, lst = Xen_copy_arg(keys[0]); (i < len) && (!Xen_is_null(lst)); i++, lst = Xen_cdr(lst))
			brkpts[i] = Xen_real_to_C_double(Xen_car(lst));
		    }
		  else
		    {
		      for (i = 0, lst = Xen_copy_arg(keys[0]); (i < len) && (!Xen_is_null(lst)); i += 2, lst = Xen_cdr(lst))
			{
			  Xen el;
			  el = Xen_car(lst);
			  if ((Xen_is_pair(el)) &&
			      (Xen_is_number(Xen_car(el))) &&
			      (Xen_is_pair(Xen_cdr(el))) &&
			      (Xen_is_number(Xen_cadr(el))))
			    {
			      brkpts[i] = Xen_real_to_C_double(Xen_car(el));
			      brkpts[i + 1] = Xen_real_to_C_double(Xen_cadr(el));
			    }
			  else 
			    {
			      Xen_error(BAD_TYPE, 
					Xen_list_2(C_string_to_Xen_string(S_make_env ": odd breakpoints list? ~A"), 
						    keys[0]));
			    }
			}
		    }
		}
	    }
        }
    }

  if (brkpts == NULL) 
    {
      Xen_error(NO_DATA,
		Xen_list_1(C_string_to_Xen_string(S_make_env ": no envelope?"))); 
    }

  if (dur > 0)
    {
      if ((end > 0) && ((end + 1) != dur))
	{
	  if ((!v) && (brkpts)) {free(brkpts); brkpts = NULL;}
	  Xen_error(CLM_ERROR,
		    Xen_list_3(C_string_to_Xen_string(S_make_env ": end, ~A, and dur, ~A, specified, but dur != end+1"),
			       keys[5], 
			       keys[6]));
	}
      end = dur - 1;
    }

  /* (make-env '(0 1 1 0) :duration most-positive-fixnum) -> env linear, pass: 0 (dur: -9223372036854775808)...
   */
  if ((end <= 0) && (duration <= 0.0))
    Xen_out_of_range_error(S_make_env, 0, C_double_to_Xen_real(duration), "duration <= 0.0?");
  if (duration > (24 * 3600 * 365))
    Xen_out_of_range_error(S_make_env, 0, C_double_to_Xen_real(duration), "duration > year?");

  {
    mus_error_handler_t *old_error_handler;
    old_error_handler = mus_error_set_handler(local_mus_error);
    ge = mus_make_env(brkpts, npts, scaler, offset, base, duration, end, NULL);
    mus_error_set_handler(old_error_handler);
  }
  
  if (ge) 
    {
      if (v) 
	return(mus_xen_to_object(mus_any_to_mus_xen_with_vct(ge, keys[0]))); /* in s7, keys[0] == v */
      return(mus_xen_to_object(mus_any_to_mus_xen_with_vct(ge, xen_make_vct(mus_env_breakpoints(ge) * 2, brkpts))));
    }
  return(clm_mus_error(local_error_type, local_error_msg, S_make_env));
}


static Xen g_env_interp(Xen x, Xen env1) /* "env" causes trouble in Objective-C!! */
{
  #define H_env_interp "(" S_env_interp " x env): value of envelope env at x"
  Xen_check_type(Xen_is_number(x), x, 1, S_env_interp, "a number");
  Xen_check_type((mus_is_xen(env1)) && (mus_is_env(Xen_to_mus_any(env1))), env1, 2, S_env_interp, "an env generator");
  return(C_double_to_Xen_real(mus_env_interp(Xen_real_to_C_double(x), Xen_to_mus_any(env1))));
}



/* mus_env_any calls the C function itself, so we pass it connect_func,
 *   connect_func uses the function passed as an argument to g_env_any.
 *   I can't think of a cleaner way to handle this except via nested functions.
 *   Both versions seem to work ok with recursive env-any calls.
 */

static Xen current_connect_func;

static mus_float_t connect_func(mus_float_t val)
{
  return(Xen_real_to_C_double(Xen_call_with_1_arg(current_connect_func,
				    C_double_to_Xen_real(val),
				    S_env_any " connect function")));
}

static Xen g_env_any(Xen e, Xen func)
{
  Xen val;
  Xen old_connect_func = Xen_false;

  #define H_env_any "(" S_env_any " e func) uses 'func' to connect the dots in the env 'e'"
  Xen_check_type((mus_is_xen(e)) && (mus_is_env(Xen_to_mus_any(e))), e, 1, S_env_any, "an env generator");
  Xen_check_type((Xen_is_procedure(func)) && (Xen_is_aritable(func, 1)), func, 2, S_env_any, "a function of one arg");
  
  old_connect_func = current_connect_func;
  current_connect_func = func;
  val = C_double_to_Xen_real(mus_env_any(Xen_to_mus_any(e), connect_func));
  current_connect_func = old_connect_func;

  return(val);
}



#define S_envelope_interp "envelope-interp"

static Xen g_envelope_interp(Xen ux, Xen e, Xen ubase)
{
  #define H_envelope_interp "(envelope-interp x e (base 1.0)) -> value of e at x; base controls connecting segment type: (envelope-interp .3 '(0 0 .5 1 1 0)) -> .6"
  mus_float_t x, base = 1.0, x0, y0, y1;
  Xen_check_type(Xen_is_number(ux), ux, 1, S_envelope_interp, "a number");
  Xen_check_type(Xen_is_list(e), e, 2, S_envelope_interp, "a list");

  if (Xen_is_null(e))
    return(Xen_integer_zero);

  x = Xen_real_to_C_double(ux);
  if (Xen_is_bound(ubase)) base = Xen_real_to_C_double(ubase);
  x0 = Xen_real_to_C_double(Xen_car(e));

  while (true)
    {
      mus_float_t x1;
      Xen ey;
      if (!Xen_is_pair(Xen_cdr(e)))
	Xen_check_type(false, e, 2, S_envelope_interp, "a list of breakpoint values");
      ey = Xen_cadr(e);
      if ((x <= x0) ||
	  (!Xen_is_pair(Xen_cddr(e))))
	return(ey);
      x1 = Xen_real_to_C_double(Xen_caddr(e));
      if (x < x1)
	{
	  if (base == 0.0)
	    return(ey);
	  y0 = Xen_real_to_C_double(ey);
	  y1 = Xen_real_to_C_double(Xen_cadddr(e));
	  if (y0 == y1)
	    return(ey);
	  if (base == 1.0)
	    return(C_double_to_Xen_real(y0 + ((x - x0) * (y1 - y0) / (x1 - x0))));
	  return(C_double_to_Xen_real(y0 + (((y1 - y0) / (base - 1.0)) * (pow(base, (x - x0) / (x1 - x0)) - 1.0))));
	}
      e = Xen_cddr(e);
      x0 = x1;
    }
  return(Xen_false);
}

/* -------------------------------- pulsed-env -------------------------------- */

static Xen g_make_pulsed_env(Xen e, Xen dur, Xen frq)
{
  #define H_make_pulsed_env "(" S_make_pulsed_env " envelope duration frequency) returns a pulsed-env generator."
  Xen gp, ge;
  mus_any *pl;

  gp = g_make_pulse_train(frq, Xen_undefined, Xen_undefined, Xen_undefined, Xen_undefined, Xen_undefined);
  ge = g_make_env(Xen_list_3(e, C_double_to_Xen_real(1.0), dur));

  pl = mus_make_pulsed_env(Xen_to_mus_any(ge), Xen_to_mus_any(gp));
  return(mus_xen_to_object(mus_any_to_mus_xen_with_two_vcts(pl, ge, gp)));
}

static Xen g_is_pulsed_env(Xen os)
{
  #define H_is_pulsed_env "(" S_is_pulsed_env " gen) returns " PROC_TRUE " if gen is a pulsed-env generator."
  return(C_bool_to_Xen_boolean((mus_is_xen(os)) && (mus_is_pulsed_env(Xen_to_mus_any(os)))));
}

static Xen g_pulsed_env(Xen g, Xen fm)
{
  #define H_pulsed_env "(" S_pulsed_env " gen fm) runs a pulsed-env generator."
  mus_any *pl = NULL;

  Xen_check_type((mus_is_xen(g)) && (mus_is_pulsed_env(pl = Xen_to_mus_any(g))), g, 1, S_pulsed_env, "a pulsed-env object");

  if (Xen_is_number(fm))
    return(C_double_to_Xen_real(mus_pulsed_env(pl, Xen_real_to_C_double(fm))));
  return(C_double_to_Xen_real(mus_pulsed_env_unmodulated(pl)));
}




/* ---------------- io ---------------- */

#if (!HAVE_RUBY)
  #define S_output "*output*"
  #define S_reverb "*reverb*"
#else
  #define S_output "output"
  #define S_reverb "reverb"
#endif

static Xen clm_output, clm_reverb; /* *output* and *reverb* at extlang level -- these can be output streams, vct, sound-data objects etc */

#if (HAVE_SCHEME)
static Xen clm_output_slot = NULL, clm_reverb_slot = NULL;

#define CLM_OUTPUT s7_slot_value(clm_output_slot)
#define CLM_REVERB s7_slot_value(clm_reverb_slot)

#else

#define CLM_OUTPUT Xen_variable_ref(S_output)
#define CLM_REVERB Xen_variable_ref(S_reverb)

#endif


static Xen g_is_mus_input(Xen obj) 
{
  #define H_is_mus_input "(" S_is_mus_input " gen): " PROC_TRUE " if gen is an input generator"
  return(C_bool_to_Xen_boolean((mus_is_xen(obj)) && (mus_is_input(Xen_to_mus_any(obj)))));
}


static Xen g_is_mus_output(Xen obj) 
{
  #define H_is_mus_output "(" S_is_mus_output " gen): " PROC_TRUE " if gen is an output generator"

  return(C_bool_to_Xen_boolean((mus_is_xen(obj)) && (mus_is_output(Xen_to_mus_any(obj)))));
}


static Xen g_is_file_to_sample(Xen obj) 
{
  #define H_is_file_to_sample "(" S_is_file_to_sample " gen): " PROC_TRUE " if gen is a " S_file_to_sample " generator"
  return(C_bool_to_Xen_boolean((mus_is_xen(obj)) && (mus_is_file_to_sample(Xen_to_mus_any(obj)))));
}

static Xen mus_clm_output(void) {return(CLM_OUTPUT);}
static Xen mus_clm_reverb(void) {return(CLM_REVERB);}

static Xen g_is_file_to_frample(Xen obj) 
{
  #define H_is_file_to_frample "(" S_is_file_to_frample " gen): " PROC_TRUE " if gen is a " S_file_to_frample " generator"
  return(C_bool_to_Xen_boolean((mus_is_xen(obj)) && (mus_is_file_to_frample(Xen_to_mus_any(obj)))));
}


static Xen g_is_sample_to_file(Xen obj) 
{
  #define H_is_sample_to_file "(" S_is_sample_to_file " gen): " PROC_TRUE " if gen is a " S_sample_to_file " generator"

  return(C_bool_to_Xen_boolean((mus_is_xen(obj)) && (mus_is_sample_to_file(Xen_to_mus_any(obj)))));
}


static Xen g_is_frample_to_file(Xen obj) 
{
  #define H_is_frample_to_file "(" S_is_frample_to_file " gen): " PROC_TRUE " if gen is a " S_frample_to_file " generator"

  return(C_bool_to_Xen_boolean((mus_is_xen(obj)) && (mus_is_frample_to_file(Xen_to_mus_any(obj)))));
}


#if HAVE_SCHEME
static mus_float_t (*in_any_2)(mus_long_t pos, int chn);
#endif

static Xen g_in_any_1(const char *caller, Xen frample, int in_chan, Xen inp)
{
  mus_long_t pos;

  Xen_check_type(Xen_is_integer(frample), frample, 1, caller, "an integer");

  pos = Xen_llong_to_C_llong(frample);
  if (pos < 0) 
    Xen_out_of_range_error(caller, 1, frample, "location should be >= 0");    

  if (in_chan < 0) 
    Xen_out_of_range_error(caller, 2, C_int_to_Xen_integer(in_chan), "must be >= 0");    

#if HAVE_SCHEME
  if (Xen_is_false(inp)) return(C_double_to_Xen_real(0.0)); /* ws.scm default for *clm-reverb* is #f */
  if (inp == CLM_REVERB)
    return(s7_make_real(s7, in_any_2(pos, in_chan)));
#endif

  if (mus_is_xen(inp))
    {
      Xen_check_type(mus_is_input(Xen_to_mus_any(inp)), inp, 3, caller, "an input generator");
      return(C_double_to_Xen_real(mus_in_any(pos, in_chan, (mus_any *)Xen_to_mus_any(inp))));
    }

  if (mus_is_vct(inp))
    {
#if HAVE_SCHEME
      if (pos < s7_vector_length(inp))
	{
	  if (s7_vector_rank(inp) > 1)
	    return(s7_vector_ref_n(s7, inp, 2, in_chan, pos));
	  return(s7_vector_ref(s7, inp, pos));
	}
      return(C_double_to_Xen_real(0.0));
#else
      vct *v;
      mus_float_t *vdata;
      v = Xen_to_vct(inp);
      vdata = mus_vct_data(v);
      if (pos < mus_vct_length(v))
	return(C_double_to_Xen_real(vdata[pos]));
      return(C_double_to_Xen_real(0.0));
#endif
    }

  if (Xen_is_vector(inp))
    {
      if (pos < Xen_vector_length(inp))
	return(Xen_vector_ref(inp, pos));
    }

  return(C_double_to_Xen_real(0.0));
}


static Xen g_in_any(Xen frample, Xen chan, Xen inp) 
{
  #define H_in_any "(" S_in_any " frample chan stream): input stream sample at frample in channel chan"
  Xen_check_type(Xen_is_integer(chan), chan, 2, S_in_any, "an integer");
  return(g_in_any_1(S_in_any, frample, Xen_integer_to_C_int(chan), inp));
}


static Xen g_ina(Xen frample, Xen inp) 
{
  #define H_ina "(" S_ina " frample stream): input stream sample in channel 0 at frample"
  return(g_in_any_1(S_ina, frample, 0, inp));
}


static Xen g_inb(Xen frample, Xen inp) 
{
  #define H_inb "(" S_inb " frample stream): input stream sample in channel 1 at frample"
  return(g_in_any_1(S_inb, frample, 1, inp));
}


#if (!HAVE_SCHEME)
static Xen out_any_2(Xen outp, mus_long_t pos, mus_float_t inv, int chn, const char *caller)
#else
static Xen fallback_out_any_2(Xen outp, mus_long_t pos, mus_float_t inv, int chn, const char *caller)
#endif
{
  mus_xen *gn;

  gn = (mus_xen *)Xen_object_ref_checked(outp, mus_xen_tag);
  if (gn)
    {
      /* mus_out_any will check the writer so output_p is pointless */
      mus_out_any(pos, inv, chn, mus_xen_to_mus_any(gn));
      return(Xen_integer_zero);
    }

  if (mus_is_vct(outp))
    {
      mus_float_t *vdata;
      vct *v;
      v = xen_to_vct(outp);
      vdata = mus_vct_data(v);
      if (Xen_vector_rank(outp) == 1)
	{
	  if (chn == 0)
	    {
	      if (pos < mus_vct_length(v))
		vdata[pos] += inv;
	    }
	}
#if HAVE_SCHEME
      else
	{
	  s7_int *offsets;
	  offsets = s7_vector_offsets(outp);
	  pos += (chn * offsets[0]);
	  if (pos < mus_vct_length(v))
	    vdata[pos] += inv;
	}
#endif
      return(Xen_integer_zero);
    }

  if (Xen_is_vector(outp))
    {
      if (pos < Xen_vector_length(outp))
	Xen_vector_set(outp, pos, C_double_to_Xen_real(Xen_real_to_C_double(Xen_vector_ref(outp, pos)) + inv));
    }

  return(Xen_integer_zero);
}

#if HAVE_SCHEME

static Xen (*out_any_2)(mus_long_t pos, mus_float_t inv, int chn, const char *caller);

bool mus_simple_out_any_to_file(mus_long_t samp, mus_float_t val, int chan, mus_any *IO);
bool mus_simple_outa_to_file(mus_long_t samp, mus_float_t val, mus_any *IO);

static mus_xen *clm_output_gn = NULL;
static mus_any *clm_output_gen = NULL;
static vct *clm_output_vct;

static Xen out_any_2_to_mus_xen(mus_long_t pos, mus_float_t inv, int chn, const char *caller)
{
  mus_out_any(pos, inv, chn, clm_output_gen);
  return(xen_zero);
}

static Xen safe_out_any_2_to_mus_xen(mus_long_t pos, mus_float_t inv, int chn, const char *caller)
{
  if (!mus_simple_out_any_to_file(pos, inv, chn, clm_output_gen))
    mus_safe_out_any_to_file(pos, inv, chn, clm_output_gen);
  return(xen_zero);
}


static Xen out_any_2_to_vct(mus_long_t pos, mus_float_t inv, int chn, const char *caller)
{
  mus_float_t *vdata;
  vdata = mus_vct_data(clm_output_vct);

#if (!HAVE_SCHEME)
  if ((chn == 0) &&
      (pos < mus_vct_length(clm_output_vct)))
    vdata[pos] += inv;
#else
  if (Xen_vector_rank(clm_output_vct) == 1)
    {
      if ((chn == 0) &&
	  (pos < mus_vct_length(clm_output_vct)))
	vdata[pos] += inv;
    }
  else
    {
      s7_int chans;
      chans = s7_vector_dimensions(clm_output_vct)[0];
      if (chn < chans)
	{
	  s7_int chan_len;
	  chan_len = s7_vector_dimensions(clm_output_vct)[1];
	  if (pos < chan_len)
	    vdata[chn * chan_len + pos] += inv;
	}
    }
#endif
  return(xen_zero);
}


static Xen out_any_2_to_vector(mus_long_t pos, mus_float_t inv, int chn, const char *caller)
{
  if (pos < Xen_vector_length(CLM_OUTPUT))
    Xen_vector_set(CLM_OUTPUT, pos, C_double_to_Xen_real(Xen_real_to_C_double(Xen_vector_ref(CLM_OUTPUT, pos)) + inv));
  return(xen_zero);
}

static Xen out_any_2_no_op(mus_long_t pos, mus_float_t inv, int chn, const char *caller)
{
  return(xen_zero);
}

static s7_pointer g_clm_output_set(s7_scheme *sc, s7_pointer args)
{
  s7_pointer new_output;
  new_output = s7_cadr(args);

  clm_output_gn = (mus_xen *)Xen_object_ref_checked(new_output, mus_xen_tag);
  if (clm_output_gn)
    {
      out_any_2 = out_any_2_to_mus_xen;
      clm_output_gen = clm_output_gn->gen;

      if (mus_out_any_is_safe(clm_output_gen))
	out_any_2 = safe_out_any_2_to_mus_xen;
    }
  else
    {
      clm_output_gen = NULL;
      if (mus_is_vct(new_output))
	{
	  out_any_2 = out_any_2_to_vct;
	  clm_output_vct = xen_to_vct(new_output);
	}
      else
	{
	  if (Xen_is_vector(new_output))
	    {
	      out_any_2 = out_any_2_to_vector;
	    }
	  else out_any_2 = out_any_2_no_op;
	}
    }
  return(new_output);
}


/* need in_any_2(pos, 0, caller) -> double + safe case + none-file cases
 */

static mus_xen *clm_input_gn;
static mus_any *clm_input_gen;
static vct *clm_input_vct;

static mus_float_t in_any_2_to_mus_xen(mus_long_t pos, int chn)
{
  return(mus_in_any(pos, chn, clm_input_gen));
}

static mus_float_t safe_in_any_2_to_mus_xen(mus_long_t pos, int chn)
{
  return(mus_file_to_sample(clm_input_gen, pos, chn));
}

static mus_float_t in_any_2_to_vct(mus_long_t pos, int chn)
{
  mus_float_t *vdata;
  vdata = mus_vct_data(clm_input_vct);
  if ((chn == 0) &&
      (pos < mus_vct_length(clm_input_vct)))
    return(vdata[pos]);
  return(0.0);
}

static mus_float_t in_any_2_to_vector(mus_long_t pos, int chn)
{
  if (pos < Xen_vector_length(CLM_REVERB))
    return(Xen_real_to_C_double(Xen_vector_ref(CLM_REVERB, pos)));
  return(0.0);
}

static mus_float_t in_any_2_no_op(mus_long_t pos, int chn)
{
  return(0.0);
}

static s7_pointer g_clm_reverb_set(s7_scheme *sc, s7_pointer args)
{
  s7_pointer new_input;
  new_input = s7_cadr(args);

  clm_input_gn = (mus_xen *)Xen_object_ref_checked(new_input, mus_xen_tag);
  if (clm_input_gn)
    {
      in_any_2 = in_any_2_to_mus_xen;
      clm_input_gen = clm_input_gn->gen;

      if (mus_in_any_is_safe(clm_input_gen))
	in_any_2 = safe_in_any_2_to_mus_xen;
    }
  else
    {
      if (mus_is_vct(new_input))
	{
	  in_any_2 = in_any_2_to_vct;
	  clm_input_vct = xen_to_vct(new_input);
	}
      else
	{
	  if (Xen_is_vector(new_input))
	    {
	      in_any_2 = in_any_2_to_vector;
	    }
	  else in_any_2 = in_any_2_no_op;
	}
    }
  return(new_input);
}


#endif


#define S_out_bank "out-bank"
static Xen g_out_bank(Xen gens, Xen loc, Xen inval)
{
  #define H_out_bank "(out-bank gens location val) calls each generator in the gens vector, passing it the argument val, then \
sends that output to the output channels in the vector order (the first generator writes to outa, the second to outb, etc)."

  mus_long_t pos;
  int i, size;
  mus_float_t x = 0.0;
  mus_any *g = NULL;
  mus_xen *gn;

  Xen_check_type(Xen_is_integer(loc), loc, 2, S_out_bank, "an integer");
  pos = Xen_llong_to_C_llong(loc);
  if (pos < 0) 
    Xen_out_of_range_error(S_out_bank, 2, loc, "must be >= 0");    

  Xen_check_type(Xen_is_vector(gens), gens, 1, S_out_bank, "a vector of generators");
  size = Xen_vector_length(gens);

  Xen_check_type(Xen_is_number(inval), inval, 3, S_out_bank, "a number");
  x = Xen_real_to_C_double(inval);

#if HAVE_SCHEME  
  for (i = 0; i < size; i++)
    {
      Xen_to_C_any_generator(Xen_vector_ref(gens, i), gn, g, S_out_bank, "an output generator");
      out_any_2(pos, mus_apply(g, x, 0.0), i, S_out_bank);
    }
#else
  for (i = 0; i < size; i++)
    {
      Xen_to_C_any_generator(Xen_vector_ref(gens, i), gn, g, S_out_bank, "an output generator");
      out_any_2(CLM_OUTPUT, pos, mus_apply(g, x, 0.0), i, S_out_bank);
    }
#endif

  return(inval);
}


static Xen g_out_any_1(const char *caller, Xen frample, int chn, Xen val, Xen outp)
{
  mus_long_t pos = 0;
  mus_float_t inv;

  if (chn < 0)
    Xen_out_of_range_error(caller, 3, C_int_to_Xen_integer(chn), "must be >= 0");    

  Xen_to_C_integer_or_error(frample, pos, caller, 1);
  if (pos < 0) 
    Xen_out_of_range_error(caller, 1, frample, "must be >= 0");    

  Xen_to_C_double_or_error(val, inv, caller, 2);

  if (!Xen_is_bound(outp))
#if (!HAVE_SCHEME)
    return(out_any_2(CLM_OUTPUT, pos, inv, chn, caller));
#else
    return(out_any_2(pos, inv, chn, caller));
#endif

#if (!HAVE_SCHEME)
  return(out_any_2(outp, pos, inv, chn, caller));
#else
  if (outp == CLM_OUTPUT)
    return(out_any_2(pos, inv, chn, caller));
  return(fallback_out_any_2(outp, pos, inv, chn, caller));
#endif
}

static Xen g_out_any(Xen frample, Xen val, Xen chan, Xen outp)
{
  #define H_out_any "(" S_out_any " frample val chan stream): add val to output stream at frample in channel chan"
  Xen_check_type(Xen_is_integer(chan), chan, 3, S_out_any, "an integer");
  return(g_out_any_1(S_out_any, frample, Xen_integer_to_C_int(chan), val, outp));
}


static Xen g_outa(Xen frample, Xen val, Xen outp)
{
  #define H_outa "(" S_outa " frample val stream): add val to output stream at frample in channel 0"
  return(g_out_any_1(S_outa, frample, 0, val, outp));
}


static Xen g_outb(Xen frample, Xen val, Xen outp)
{
  #define H_outb "(" S_outb " frample val stream): add val to output stream at frample in channel 1"
  return(g_out_any_1(S_outb, frample, 1, val, outp));
}


static Xen g_outc(Xen frample, Xen val, Xen outp)
{
  #define H_outc "(" S_outc " frample val stream): add val to output stream at frample in channel 2"
  return(g_out_any_1(S_outc, frample, 2, val, outp));
}


static Xen g_outd(Xen frample, Xen val, Xen outp)
{
  #define H_outd "(" S_outd " frample val stream): add val to output stream at frample in channel 3"
  return(g_out_any_1(S_outd, frample, 3, val, outp));
}


static Xen g_mus_close(Xen ptr)
{
  #define H_mus_close "(" S_mus_close " gen): close the IO stream managed by 'gen' (a sample->file generator, for example)"

  if (mus_is_xen(ptr))
    return(C_int_to_Xen_integer(mus_close_file((mus_any *)Xen_to_mus_any(ptr))));

  Xen_check_type(mus_is_vct(ptr) || Xen_is_false(ptr) || Xen_is_vector(ptr), 
		  ptr, 1, S_mus_close, "an IO gen or its outa equivalent");
  return(Xen_integer_zero);
}


static Xen g_make_file_to_sample(Xen name, Xen buffer_size)
{
  #define H_make_file_to_sample "(" S_make_file_to_sample " filename buffer-size): return an input generator reading 'filename' (a sound file)"

  mus_any *ge;
  mus_long_t size;

  Xen_check_type(Xen_is_string(name), name, 1, S_make_file_to_sample, "a string");
  Xen_check_type(Xen_is_llong_or_unbound(buffer_size), buffer_size, 2, S_make_file_to_sample, "an integer");

  if (!(mus_file_probe(Xen_string_to_C_string(name))))
    Xen_error(NO_SUCH_FILE,
	      Xen_list_3(C_string_to_Xen_string(S_make_file_to_sample ": ~S, ~A"),
			 name,
			 C_string_to_Xen_string(STRERROR(errno))));

  if (Xen_is_llong(buffer_size))
    {
      size = Xen_llong_to_C_llong(buffer_size);
      if (size <= 0)
	Xen_out_of_range_error(S_make_file_to_sample, 2, buffer_size, "must be > 0");
    }
  else size = mus_file_buffer_size();

  ge = mus_make_file_to_sample_with_buffer_size(Xen_string_to_C_string(name), size);
  if (ge) return(mus_xen_to_object(mus_any_to_mus_xen(ge)));
  return(Xen_false);
}


static Xen g_file_to_sample(Xen obj, Xen samp, Xen chan)
{
  #define H_file_to_sample "(" S_file_to_sample " obj frample chan): sample value in sound file read by 'obj' in channel chan at frample"
  int channel = 0;
  mus_any *g = NULL;
  mus_xen *gn;

  Xen_to_C_generator(obj, gn, g, mus_is_input, S_file_to_sample, "an input generator");
  Xen_check_type(Xen_is_llong(samp), samp, 2, S_file_to_sample, "an integer");

  if (Xen_is_bound(chan))
    {
      Xen_check_type(Xen_is_integer(chan), chan, 3, S_file_to_sample, "an integer");
      channel = Xen_integer_to_C_int(chan);
    }
  return(C_double_to_Xen_real(mus_file_to_sample(g, Xen_llong_to_C_llong(samp), channel)));
}


static Xen g_make_sample_to_file(Xen name, Xen chans, Xen out_format, Xen out_type, Xen comment)
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

  #define H_make_sample_to_file "(" S_make_sample_to_file " filename chans sample-type header-type comment): \
return an output generator writing the sound file 'filename' which is set up to have \
'chans' channels of 'sample-type' samples with a header of 'header-type'.  The latter \
should be sndlib identifiers:\n  " make_sample_to_file_example

  mus_sample_t df = MUS_OUT_SAMPLE_TYPE;

  Xen_check_type(Xen_is_string(name), name, 1, S_make_sample_to_file, "a string");
  Xen_check_type(Xen_is_integer_or_unbound(chans), chans, 2, S_make_sample_to_file, "an integer");
  Xen_check_type(Xen_is_integer_or_unbound(out_format), out_format, 3, S_make_sample_to_file, "an integer (sample type)");
  Xen_check_type(Xen_is_integer_or_unbound(out_type), out_type, 4, S_make_sample_to_file, "an integer (header type)");

  if (Xen_is_integer(out_format)) df = (mus_sample_t)Xen_integer_to_C_int(out_format);
  if (mus_is_sample_type(df))
    {
      mus_header_t ht = MUS_NEXT;
      if (Xen_is_integer(out_type)) ht = (mus_header_t)Xen_integer_to_C_int(out_type);
      if (mus_is_header_type(ht))
	{
	  int chns = 1;
	  if (Xen_is_integer(chans)) chns = Xen_integer_to_C_int(chans);
	  if (chns > 0)
	    {
	      mus_any *rgen;
	      rgen = mus_make_sample_to_file_with_comment(Xen_string_to_C_string(name),
							  chns, df, ht,
							  (Xen_is_string(comment)) ? Xen_string_to_C_string(comment) : NULL);
	      if (rgen) return(mus_xen_to_object(mus_any_to_mus_xen(rgen)));
	    }
	  else Xen_out_of_range_error(S_make_sample_to_file, 2, chans, "chans <= 0?");
	}
      else Xen_out_of_range_error(S_make_sample_to_file, 4, out_type, "invalid header type");
    }
  else Xen_out_of_range_error(S_make_sample_to_file, 3, out_format, "invalid sample type");
  return(Xen_false);
}


static Xen g_continue_sample_to_file(Xen name)
{
  #define H_continue_sample_to_file "(" S_continue_sample_to_file " filename): return an output generator \
that reopens an existing sound file 'filename' ready for output via " S_sample_to_file

  mus_any *rgen = NULL;
  Xen_check_type(Xen_is_string(name), name, 1, S_continue_sample_to_file, "a string");
  rgen = mus_continue_sample_to_file(Xen_string_to_C_string(name));
  if (rgen) return(mus_xen_to_object(mus_any_to_mus_xen(rgen)));
  return(Xen_false);
}


static Xen g_sample_to_file(Xen obj, Xen samp, Xen chan, Xen val)
{
  #define H_sample_to_file "(" S_sample_to_file " obj samp chan val): add val to the output stream \
handled by the output generator 'obj', in channel 'chan' at frample 'samp'"

  mus_any *g = NULL;
  mus_xen *gn;

  Xen_to_C_any_generator(obj, gn, g, S_sample_to_file, "an output generator");
  Xen_check_type(mus_is_output(g), obj, 1, S_sample_to_file, "an output generator");

  Xen_check_type(Xen_is_integer(samp), samp, 2, S_sample_to_file, "an integer");
  Xen_check_type(Xen_is_integer(chan), chan, 3, S_sample_to_file, "an integer");
  Xen_check_type(Xen_is_number(val), val, 4, S_sample_to_file, "a number");

  mus_sample_to_file(g,
		     Xen_llong_to_C_llong(samp),
		     Xen_integer_to_C_int(chan),
		     Xen_real_to_C_double(val));
  return(val);
}


static Xen g_sample_to_file_add(Xen obj1, Xen obj2)
{
  #define H_sample_to_file_add "(" S_sample_to_file_add " obj1 obj2): mixes obj2 (an output generator) into obj1 (also an output generator)"
  mus_any *g1 = NULL, *g2 = NULL;
  mus_xen *gn1, *gn2;

  Xen_to_C_any_generator(obj1, gn1, g1, S_sample_to_file_add, "an output generator");
  Xen_to_C_any_generator(obj2, gn2, g2, S_sample_to_file_add, "an output generator");
  Xen_check_type(mus_is_output(g1), obj1, 1, S_sample_to_file_add, "an output generator");
  Xen_check_type(mus_is_output(g2), obj2, 2, S_sample_to_file_add, "an output generator");

  mus_sample_to_file_add(g1, g2);
  return(obj1);
}

static Xen g_make_file_to_frample(Xen name, Xen buffer_size)
{
  #define H_make_file_to_frample "(" S_make_file_to_frample " filename buffer-size): return an input generator reading 'filename' (a sound file)"

  mus_any *ge;
  mus_long_t size;

  Xen_check_type(Xen_is_string(name), name, 1, S_make_file_to_frample, "a string");
  Xen_check_type(Xen_is_llong_or_unbound(buffer_size), buffer_size, 2, S_make_file_to_frample, "an integer");

  if (!(mus_file_probe(Xen_string_to_C_string(name))))
    Xen_error(NO_SUCH_FILE,
	      Xen_list_3(C_string_to_Xen_string(S_make_file_to_frample ": ~S, ~A"),
			 name,
			 C_string_to_Xen_string(STRERROR(errno))));

  if (Xen_is_llong(buffer_size))
    {
      size = Xen_llong_to_C_llong(buffer_size);
      if (size <= 0)
	Xen_out_of_range_error(S_make_file_to_frample, 2, buffer_size, "must be > 0");
    }
  else size = mus_file_buffer_size();
  ge = mus_make_file_to_frample_with_buffer_size(Xen_string_to_C_string(name), size);
  if (ge) return(mus_xen_to_object(mus_any_to_mus_xen(ge)));
  return(Xen_false);
}


static Xen g_file_to_frample(Xen obj, Xen samp, Xen outfr)
{
  #define H_file_to_frample "(" S_file_to_frample " obj samp outf): frample of samples at frample 'samp' in sound file read by 'obj'"
  
  Xen_check_type((mus_is_xen(obj)) && (mus_is_input(Xen_to_mus_any(obj))), obj, 1, S_file_to_frample, "an input generator");
  Xen_check_type(Xen_is_integer(samp), samp, 2, S_file_to_frample, "an integer");

  mus_file_to_frample(Xen_to_mus_any(obj), Xen_llong_to_C_llong(samp), mus_vct_data(Xen_to_vct(outfr)));
  return(outfr);
}


static Xen g_make_frample_to_file(Xen name, Xen chans, Xen out_format, Xen out_type, Xen comment)
{
  #if HAVE_SCHEME
    #define make_frample_to_file_example "(" S_make_frample_to_file " \"test.snd\" 2 mus-lshort mus-riff)"
  #endif
  #if HAVE_RUBY
    #define make_frample_to_file_example "\"test.snd\" 2 Mus_lshort Mus_riff make_frample2file"
  #endif
  #if HAVE_FORTH
    #define make_frample_to_file_example "\"test.snd\" 2 mus-lshort mus-riff make-frample->file"
  #endif

  #define H_make_frample_to_file "(" S_make_frample_to_file " filename chans sample-type header-type comment): \
return an output generator writing the sound file 'filename' which is set up to have \
'chans' channels of 'sample-type' samples with a header of 'header-type'.  The latter \
should be sndlib identifiers:\n  " make_frample_to_file_example

  mus_any *fgen = NULL;

  Xen_check_type(Xen_is_string(name), name, 1, S_make_frample_to_file, "a string");
  Xen_check_type(Xen_is_integer_or_unbound(chans), chans, 2, S_make_frample_to_file, "an integer");
  Xen_check_type(Xen_is_integer_or_unbound(out_format), out_format, 3, S_make_frample_to_file, "an integer (sample type id)");
  Xen_check_type(Xen_is_integer_or_unbound(out_type), out_type, 4, S_make_frample_to_file, "an integer (header-type id)");

  fgen = mus_make_frample_to_file_with_comment(Xen_string_to_C_string(name),
					     (Xen_is_integer(chans)) ? Xen_integer_to_C_int(chans) : 1,
					     (Xen_is_integer(out_format)) ? (mus_sample_t)Xen_integer_to_C_int(out_format) : MUS_OUT_SAMPLE_TYPE,
					     (Xen_is_integer(out_type)) ? (mus_header_t)Xen_integer_to_C_int(out_type) : MUS_NEXT,
					     (Xen_is_string(comment)) ? Xen_string_to_C_string(comment) : NULL);
  if (fgen) return(mus_xen_to_object(mus_any_to_mus_xen(fgen)));
  return(Xen_false);
}


static Xen g_continue_frample_to_file(Xen name)
{
  #define H_continue_frample_to_file "(" S_continue_frample_to_file " filename): return an output generator \
that reopens an existing sound file 'filename' ready for output via " S_frample_to_file

  mus_any *rgen = NULL;
  Xen_check_type(Xen_is_string(name), name, 1, S_continue_frample_to_file, "a string");
  rgen = mus_continue_frample_to_file(Xen_string_to_C_string(name));
  if (rgen) return(mus_xen_to_object(mus_any_to_mus_xen(rgen)));
  return(Xen_false);
}


static Xen g_frample_to_file(Xen obj, Xen samp, Xen val)
{
  #define H_frample_to_file "(" S_frample_to_file " obj samp val): add frample 'val' to the output stream \
handled by the output generator 'obj' at frample 'samp'"
  mus_xen *gn;

  gn = (mus_xen *)Xen_object_ref_checked(obj, mus_xen_tag);
  Xen_check_type(((gn) && (mus_is_output(gn->gen))), obj, 1, S_frample_to_file, "an output generator");
  Xen_check_type(Xen_is_integer(samp), samp, 2, S_frample_to_file, "an integer");

  mus_frample_to_file(gn->gen, Xen_llong_to_C_llong(samp), mus_vct_data(Xen_to_vct(val)));
  return(val);
}




/* ---------------- readin ---------------- */

static Xen g_is_readin(Xen obj) 
{
  #define H_is_readin "(" S_is_readin " gen): " PROC_TRUE " if gen is a " S_readin
  return(C_bool_to_Xen_boolean((mus_is_xen(obj)) && (mus_is_readin(Xen_to_mus_any(obj)))));
}


static Xen g_readin(Xen obj)
{
  #define H_readin "(" S_readin " gen): next sample from readin generator (a sound file reader)"
  mus_any *g = NULL;
  mus_xen *gn;

  Xen_to_C_generator(obj, gn, g, mus_is_readin, S_readin, "a readin generator");

  return(C_double_to_Xen_real(mus_readin(g)));
}


static Xen g_make_readin(Xen arglist)
{
  #define H_make_readin "(" S_make_readin " file (channel 0) (start 0) (direction 1) size): \
return a new readin (file input) generator reading the sound file 'file' starting at frample \
'start' in channel 'channel' and reading forward if 'direction' is not -1"

  /* optkey file channel start direction size */

  mus_any *ge;
  const char *file = NULL;
  Xen args[10];
  Xen keys[5];
  int orig_arg[5] = {0, 0, 0, 0, 0};
  int vals, chans;
  mus_long_t buffer_size;
  int channel = 0, direction = 1;
  mus_long_t start = 0;

  keys[0] = kw_file;
  keys[1] = kw_channel;
  keys[2] = kw_start;
  keys[3] = kw_direction;
  keys[4] = kw_size;

  buffer_size = mus_file_buffer_size();
  /* this is only 8192! (clm.h MUS_DEFAULT_FILE_BUFFER_SIZE) */

  {
    int i, arglist_len;
    Xen p;
    arglist_len = Xen_list_length(arglist);
    if (arglist_len > 10) clm_error(S_make_readin, "too many arguments!", arglist);
    for (i = 0, p = arglist; i < arglist_len; i++, p = Xen_cdr(p)) args[i] = Xen_car(p);
    for (i = arglist_len; i < 10; i++) args[i] = Xen_undefined;
  }

  vals = mus_optkey_unscramble(S_make_readin, 5, keys, args, orig_arg);
  if (vals > 0)
    {
      file = mus_optkey_to_string(keys[0], S_make_readin, orig_arg[0], NULL); /* not copied */

      channel = Xen_optkey_to_int(kw_channel, keys[1], S_make_readin, orig_arg[1], channel);
      if (channel < 0)
	Xen_out_of_range_error(S_make_readin, orig_arg[1], keys[1], "channel < 0?");

      start = Xen_optkey_to_mus_long_t(kw_start, keys[2], S_make_readin, orig_arg[2], start);

      direction = Xen_optkey_to_int(kw_direction, keys[3], S_make_readin, orig_arg[3], direction);

      buffer_size = Xen_optkey_to_mus_long_t(kw_size, keys[4], S_make_readin, orig_arg[4], buffer_size);
      if (buffer_size <= 0)
	Xen_out_of_range_error(S_make_readin, orig_arg[4], keys[4], "must be > 0");
    }

  if (file == NULL)
    Xen_out_of_range_error(S_make_readin, orig_arg[0], keys[0], "no file name given");
  if (!(mus_file_probe(file)))
    Xen_error(NO_SUCH_FILE,
	      Xen_list_3(C_string_to_Xen_string(S_make_readin ": ~S, ~A"),
			 C_string_to_Xen_string(file),
			 C_string_to_Xen_string(STRERROR(errno))));

  chans = mus_sound_chans(file);
  if (chans <= 0)
    Xen_error(BAD_HEADER,
	      Xen_list_2(C_string_to_Xen_string(S_make_readin ": ~S chans <= 0?"),
			 C_string_to_Xen_string(file)));

  if (channel >= chans)
    Xen_out_of_range_error(S_make_readin, orig_arg[1], keys[1], "channel > available chans?");

  ge = mus_make_readin_with_buffer_size(file, channel, start, direction, buffer_size);
  if (ge) return(mus_xen_to_object(mus_any_to_mus_xen(ge)));
  return(Xen_false);
}


/* ---------------- locsig ---------------- */

static Xen g_locsig_ref(Xen obj, Xen chan)
{
  #define H_locsig_ref "(" S_locsig_ref " gen chan): locsig 'gen' channel 'chan' scaler"
  Xen_check_type((mus_is_xen(obj)) && (mus_is_locsig(Xen_to_mus_any(obj))), obj, 1, S_locsig_ref, "a locsig generator");
  Xen_check_type(Xen_is_integer(chan), chan, 2, S_locsig_ref, "an integer");
  return(C_double_to_Xen_real(mus_locsig_ref(Xen_to_mus_any(obj), Xen_integer_to_C_int(chan))));
}


static Xen g_locsig_set(Xen obj, Xen chan, Xen val)
{
  #define H_locsig_set "(" S_locsig_set " gen chan val): set the locsig generator's channel 'chan' scaler to 'val'"
  Xen_check_type((mus_is_xen(obj)) && (mus_is_locsig(Xen_to_mus_any(obj))), obj, 1, S_locsig_set, "a locsig generator");
  Xen_check_type(Xen_is_integer(chan), chan, 2, S_locsig_set, "an integer");
#if (!HAVE_SCHEME)
  Xen_check_type(Xen_is_number(val), val, 3, S_locsig_set, "a number");
  mus_locsig_set(Xen_to_mus_any(obj), Xen_integer_to_C_int(chan), Xen_real_to_C_double(val));
#else
  mus_locsig_set(Xen_to_mus_any(obj), Xen_integer_to_C_int(chan), s7_number_to_real_with_caller(s7, val, S_locsig_set));
#endif
  return(val);
}


static Xen g_locsig_reverb_ref(Xen obj, Xen chan)
{
  #define H_locsig_reverb_ref "(" S_locsig_reverb_ref " gen chan): locsig reverb channel 'chan' scaler"
  Xen_check_type((mus_is_xen(obj)) && (mus_is_locsig(Xen_to_mus_any(obj))), obj, 1, S_locsig_reverb_ref, "a locsig generator");
  Xen_check_type(Xen_is_integer(chan), chan, 2, S_locsig_reverb_ref, "an integer");
  return(C_double_to_Xen_real(mus_locsig_reverb_ref(Xen_to_mus_any(obj), Xen_integer_to_C_int(chan))));
}


static Xen g_locsig_reverb_set(Xen obj, Xen chan, Xen val)
{
  #define H_locsig_reverb_set "(" S_locsig_reverb_set " gen chan val): set the locsig reverb channel 'chan' scaler to 'val'"
  Xen_check_type((mus_is_xen(obj)) && (mus_is_locsig(Xen_to_mus_any(obj))), obj, 1, S_locsig_reverb_set, "a locsig generator");
  Xen_check_type(Xen_is_integer(chan), chan, 2, S_locsig_reverb_set, "an integer");
#if (!HAVE_SCHEME)
  Xen_check_type(Xen_is_number(val), val, 3, S_locsig_reverb_set, "a number");
  mus_locsig_reverb_set(Xen_to_mus_any(obj), Xen_integer_to_C_int(chan), Xen_real_to_C_double(val));
#else
  mus_locsig_reverb_set(Xen_to_mus_any(obj), Xen_integer_to_C_int(chan), s7_number_to_real_with_caller(s7, val, S_locsig_reverb_set));
#endif
  return(val);
}


static Xen g_is_locsig(Xen obj)
{
  #define H_is_locsig "(" S_is_locsig " gen): " PROC_TRUE " if gen is a " S_locsig
  return(C_bool_to_Xen_boolean((mus_is_xen(obj)) && (mus_is_locsig(Xen_to_mus_any(obj)))));
}


static void mus_locsig_or_move_sound_to_vct_or_sound_data(mus_xen *ms, mus_any *loc_gen, mus_long_t pos, bool from_locsig)
{
  mus_float_t *outfr = NULL, *revfr = NULL;
  Xen output, reverb;
#if HAVE_SCHEME
  int chans, rev_chans;
#endif
  if (pos < 0) return;

  if (from_locsig)
    {
      outfr = mus_locsig_outf(loc_gen);
      revfr = mus_locsig_revf(loc_gen);
#if HAVE_SCHEME
      chans = mus_locsig_channels(loc_gen);
      rev_chans = mus_locsig_reverb_channels(loc_gen);
#endif
    }
  else
    {
      outfr = mus_move_sound_outf(loc_gen);
      revfr = mus_move_sound_revf(loc_gen);
#if HAVE_SCHEME
      chans = mus_move_sound_channels(loc_gen);
      rev_chans = mus_move_sound_reverb_channels(loc_gen);
#endif
    }
  output = ms->vcts[G_LOCSIG_OUT];

  if (outfr)
    {
      if (mus_is_vct(output))
	{
	  vct *v;
	  mus_float_t *vdata;
	  v = Xen_to_vct(output);
	  vdata = mus_vct_data(v);
	  if (Xen_vector_rank(output) == 1)
	    {
	      if (pos < mus_vct_length(v))
		vdata[pos] += outfr[0];
	    }
#if HAVE_SCHEME
	  else
	    {
	      s7_int chan_len;
	      chan_len = s7_vector_dimensions(output)[1]; /* '(4 20) so each chan len is [1] */
	      if (pos < chan_len)
		{
		  int i;
		  for (i = 0; i < chans; i++)
		    vdata[i * chan_len + pos] += outfr[i];
		}
	    }
#endif
	}
      else 
	{
	  if ((Xen_is_vector(output)) &&
	      (pos < Xen_vector_length(output)))
	    Xen_vector_set(output, pos, C_double_to_Xen_real(Xen_real_to_C_double(Xen_vector_ref(output, pos)) + outfr[0]));
	}
    }
  
  if ((revfr) && 
      (Xen_is_bound(ms->vcts[G_LOCSIG_REVOUT])))
    {
      reverb = ms->vcts[G_LOCSIG_REVOUT];
      if (mus_is_vct(reverb))
	{
	  vct *v;
	  mus_float_t *vdata;
	  v = Xen_to_vct(reverb);
	  vdata = mus_vct_data(v);
	  if (Xen_vector_rank(reverb) == 1)
	    {
	      if (pos < mus_vct_length(v))
		vdata[pos] += revfr[0];
	    }
#if HAVE_SCHEME
	  else
	    {
	      s7_int chan_len;
	      chan_len = s7_vector_dimensions(reverb)[1];
	      if (pos < chan_len)
		{
		  int i;
		  for (i = 0; i < rev_chans; i++)
		    vdata[i * chan_len + pos] += revfr[i];
		}
	    }
#endif
	}
      else 
	{
	  if ((Xen_is_vector(reverb)) &&
	      (pos < Xen_vector_length(reverb)))
	    Xen_vector_set(reverb, pos, C_double_to_Xen_real(Xen_real_to_C_double(Xen_vector_ref(reverb, pos)) + revfr[0]));
	}
    }
}


static Xen g_locsig(Xen xobj, Xen xpos, Xen xval)
{
  #define H_locsig "(" S_locsig " gen loc val): add 'val' to the output of locsig at frample 'loc'"
  mus_any *loc_gen;
  mus_xen *ms;
  mus_long_t pos;
  mus_float_t fval;

  ms = (mus_xen *)Xen_object_ref_checked(xobj, mus_xen_tag);
  if (!ms) Xen_check_type(false, xobj, 1, S_locsig, "a locsig generator");
  loc_gen = ms->gen;
  Xen_check_type(mus_is_locsig(loc_gen), xobj, 1, S_locsig, "a locsig generator");

  Xen_check_type(Xen_is_integer(xpos), xpos, 2, S_locsig, "an integer");

  pos = Xen_llong_to_C_llong(xpos);
  if (pos < 0) 
    Xen_out_of_range_error(S_locsig, 2, xpos, "must be >= 0");    

#if (!HAVE_SCHEME)
  Xen_check_type(Xen_is_number(xval), xval, 3, S_locsig, "a number");
  fval = Xen_real_to_C_double(xval);
#else
  fval = s7_number_to_real_with_caller(s7, xval, S_locsig);
#endif

  mus_locsig(loc_gen, pos, fval);

  return(xval);  /* changed 30-June-06 to return val rather than a wrapped frample */
}

static mus_interp_t clm_locsig_type = MUS_INTERP_LINEAR;

static Xen g_locsig_type(void)
{
  #define H_locsig_type "(" S_locsig_type "): locsig interpolation type, either " S_mus_interp_linear " or " S_mus_interp_sinusoidal "."
  return(C_int_to_Xen_integer((int)clm_locsig_type));
}


static Xen g_set_locsig_type(Xen val)
{
  mus_interp_t newval;
  Xen_check_type(Xen_is_integer(val), val, 1, S_locsig_type, S_mus_interp_linear " or " S_mus_interp_sinusoidal);
  newval = (mus_interp_t)Xen_integer_to_C_int(val);
  if ((newval == MUS_INTERP_LINEAR) || (newval == MUS_INTERP_SINUSOIDAL))
    clm_locsig_type = newval;
  return(C_int_to_Xen_integer((int)clm_locsig_type));
}


static void clm_locsig_detour(mus_any *ptr, mus_long_t pos)
{
  mus_xen *ms;
  ms = (mus_xen *)mus_locsig_closure(ptr);
  /* now check for vct/sound-data special cases */
  if (ms->nvcts == 4) 
    mus_locsig_or_move_sound_to_vct_or_sound_data(ms, ms->gen, pos, true);
}

static Xen g_make_locsig(Xen arglist)
{
  #define H_make_locsig "(" S_make_locsig " (degree 0.0) (distance 1.0) (reverb 0.0) (output *output*) (revout *reverb*) (channels (mus-channels *output*)) (type " S_mus_interp_linear ")): \
return a new generator for signal placement in n channels.  Channel 0 corresponds to 0 degrees."

  mus_any *ge;
  mus_any *outp = NULL, *revp = NULL;
  Xen args[14];
  Xen keys[7];
  Xen ov = Xen_undefined, rv = Xen_undefined;
  Xen keys3 = Xen_undefined, keys4 = Xen_undefined;
  int orig_arg[7] = {0, 0, 0, 0, 0, 0, 0};
  int vals, out_chans = -1, rev_chans = -1;
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

  {
    int i, arglist_len;
    Xen p;
    arglist_len = Xen_list_length(arglist);
    if (arglist_len > 14) clm_error(S_make_locsig, "too many arguments!", arglist);
    for (i = 0, p = arglist; i < arglist_len; i++, p = Xen_cdr(p)) args[i] = Xen_car(p);
    for (i = arglist_len; i < 14; i++) args[i] = Xen_undefined;
  }

  vals = mus_optkey_unscramble(S_make_locsig, 7, keys, args, orig_arg);
  if (vals > 0)
    {
      degree = Xen_optkey_to_float(kw_degree, keys[0], S_make_locsig, orig_arg[0], degree);
      distance = Xen_optkey_to_float(kw_distance, keys[1], S_make_locsig, orig_arg[1], distance);
      reverb = Xen_optkey_to_float(kw_reverb, keys[2], S_make_locsig, orig_arg[2], reverb);

      if (!(Xen_is_keyword(keys[3])))
	keys3 = keys[3];

      if (!(Xen_is_keyword(keys[4])))
	keys4 = keys[4];

      if (!(Xen_is_keyword(keys[5])))
	{
	  Xen_check_type(Xen_is_integer(keys[5]), keys[5], orig_arg[5], S_make_locsig, "an integer");
	  out_chans = Xen_integer_to_C_int(keys[5]);
	  if (out_chans < 0) 
	    Xen_out_of_range_error(S_make_locsig, orig_arg[5], keys[5], "chans < 0?");
	  if (out_chans > mus_max_table_size()) 
	    Xen_out_of_range_error(S_make_locsig, orig_arg[5], keys[5], "too many chans");
	}

      type = (mus_interp_t)Xen_optkey_to_int(kw_type, keys[6], S_make_locsig, orig_arg[6], (int)type);
      if ((type != MUS_INTERP_LINEAR) && (type != MUS_INTERP_SINUSOIDAL))
	Xen_out_of_range_error(S_make_locsig, orig_arg[6], keys[6], "type must be " S_mus_interp_linear " or " S_mus_interp_sinusoidal ".");
    }

  if (!Xen_is_bound(keys3))
    keys3 = CLM_OUTPUT;

  if (!Xen_is_bound(keys4))
    keys4 = CLM_REVERB;

  /* try to default output to *output* and reverb to *reverb*, if they're currently set and not closed */
  /*   mus_close is actually mus_close_file = sample_to_file_end = free and nullify obufs so we're hoping dynamic-wind works... */

  if ((mus_is_xen(keys3)) && 
      (mus_is_output(Xen_to_mus_any(keys3))))
    {
      outp = (mus_any *)Xen_to_mus_any(keys3);
      if (out_chans < 0) 
	out_chans = mus_channels((mus_any *)outp);
    }
  else
    {
      if (mus_is_vct(keys3))
	ov = keys3;
      else Xen_check_type(Xen_is_keyword(keys[3]) || Xen_is_false(keys[3]), keys[3], orig_arg[3], S_make_locsig, "an output gen, " S_vct ", vector, or a sound-data object");
#if HAVE_SCHEME
      if ((out_chans < 0) &&
	  (Xen_vector_rank(ov) > 1))
	out_chans = s7_vector_dimensions(ov)[0];
#endif
    }

  if ((mus_is_xen(keys4)) && 
      (mus_is_output(Xen_to_mus_any(keys4))))
    {
      revp = (mus_any *)Xen_to_mus_any(keys4);
      if (rev_chans < 0)
	rev_chans = mus_channels((mus_any *)revp);
    }
  else
    {
      if (mus_is_vct(keys4))
	{
	  rv = keys4;
	  rev_chans = 1;
#if HAVE_SCHEME
	  if (Xen_vector_rank(rv) > 1)
	    rev_chans = s7_vector_dimensions(rv)[0];
#endif
	}
      else Xen_check_type(Xen_is_keyword(keys[4]) || Xen_is_false(keys[4]), keys[4], orig_arg[4], S_make_locsig, "a reverb output generator");
    }

  if (out_chans < 0) out_chans = 1;
  if (rev_chans < 0) rev_chans = 0;

  ge = mus_make_locsig(degree, distance, reverb, out_chans, outp, rev_chans, revp, type);

  if (ge)
    {
      mus_xen *gn;
      if (((Xen_is_bound(ov)) && (!Xen_is_false(ov))) || 
	  ((Xen_is_bound(rv)) && (!Xen_is_false(rv))))
	gn = mx_alloc(4);
      else gn = mx_alloc(2);

      /* these two are for the mus-data and mus-xcoeffs methods in Scheme (etc) = MUS_DATA_WRAPPER and G_FILTER_XCOEFFS */
      if (out_chans > 0)
	gn->vcts[G_LOCSIG_DATA] = xen_make_vct_wrapper(out_chans, mus_data(ge));
      else gn->vcts[G_LOCSIG_DATA] = Xen_undefined;
      if (rev_chans > 0)
	gn->vcts[G_LOCSIG_REVDATA] = xen_make_vct_wrapper(rev_chans, mus_xcoeffs(ge));
      else gn->vcts[G_LOCSIG_REVDATA] = Xen_undefined;

      if (gn->nvcts == 4)
	{
	  mus_locsig_set_detour(ge, clm_locsig_detour);
	  gn->vcts[G_LOCSIG_OUT] = ov;
	  gn->vcts[G_LOCSIG_REVOUT] = rv;
	  mus_set_environ(ge, (void *)gn);
	}

      gn->gen = ge;
      return(mus_xen_to_object(gn));
    }
  return(Xen_false);
}


static Xen g_move_locsig(Xen obj, Xen degree, Xen distance)
{
  #define H_move_locsig "(" S_move_locsig " gen degree distance): move locsig gen to reflect degree and distance"
  Xen_check_type((mus_is_xen(obj)) && (mus_is_locsig(Xen_to_mus_any(obj))), obj, 1, S_move_locsig, "a locsig generator");
#if (!HAVE_SCHEME)
  Xen_check_type(Xen_is_number(degree), degree, 2, S_move_locsig, "a number in degrees");
  Xen_check_type(Xen_is_number(distance), distance, 3, S_move_locsig, "a number > 1.0");
  mus_move_locsig(Xen_to_mus_any(obj), Xen_real_to_C_double(degree), Xen_real_to_C_double(distance));
#else
  mus_move_locsig(Xen_to_mus_any(obj), 
		  s7_number_to_real_with_caller(s7, degree, S_move_locsig), 
		  s7_number_to_real_with_caller(s7, distance, S_move_locsig));
#endif
  return(obj);
}




/* ---------------- move-sound ---------------- */

static Xen g_is_move_sound(Xen obj)
{
  #define H_is_move_sound "(" S_is_move_sound " gen): " PROC_TRUE " if gen is a " S_move_sound
  return(C_bool_to_Xen_boolean((mus_is_xen(obj)) && (mus_is_move_sound(Xen_to_mus_any(obj)))));
}


static Xen g_move_sound(Xen obj, Xen loc, Xen val)
{
  #define H_move_sound "(" S_move_sound " gen loc val): dlocsig run-time generator handling 'val' at sample 'loc'"
  mus_any *move_gen;
  mus_xen *ms;
  mus_long_t pos;
  mus_float_t fval;

  Xen_check_type(mus_is_xen(obj), obj, 1, S_move_sound, "a move-sound generator");
  ms = Xen_to_mus_xen(obj);
  move_gen = (mus_any *)(ms->gen);

  Xen_check_type(mus_is_move_sound(move_gen), obj, 1, S_move_sound, "a move-sound generator");
  Xen_check_type(Xen_is_integer(loc), loc, 2, S_move_sound, "an integer");
  Xen_check_type(Xen_is_number(val), val, 3, S_move_sound, "a number");

  pos = Xen_llong_to_C_llong(loc);
  if (pos < 0) 
    Xen_out_of_range_error(S_move_sound, 2, loc, "must be >= 0");    
  fval = Xen_real_to_C_double(val);

  mus_move_sound(move_gen, pos, fval);
  return(val);
}


static mus_any **xen_vector_to_mus_any_array(Xen vect)
{
  mus_any **gens;
  mus_long_t i, len;

  if (!(Xen_is_vector(vect))) return(NULL);
  len = Xen_vector_length(vect);
  gens = (mus_any **)calloc(len, sizeof(mus_any *));

  for (i = 0; i < len; i++)
    if (mus_is_xen(Xen_vector_ref(vect, i)))
      gens[i] = Xen_to_mus_any(Xen_vector_ref(vect, i));
  return(gens);
}


static int *xen_vector_to_int_array(Xen vect)
{
  int *vals;
  mus_long_t i, len;

  len = Xen_vector_length(vect);
  vals = (int *)calloc(len, sizeof(int));

  for (i = 0; i < len; i++)
    vals[i] = Xen_integer_to_C_int(Xen_vector_ref(vect, i));
  return(vals);
}


static void clm_move_sound_detour(mus_any *ptr, mus_long_t pos)
{
  mus_xen *ms;
  ms = (mus_xen *)mus_move_sound_closure(ptr);
  /* now check for vct/sound-data special cases */
  if (ms->nvcts == 4) 
    mus_locsig_or_move_sound_to_vct_or_sound_data(ms, ms->gen, pos, false);
}


static Xen g_make_move_sound(Xen dloc_list, Xen outp, Xen revp)
{
  Xen ov = Xen_undefined, rv = Xen_undefined;
  mus_any *ge, *dopdly, *dopenv, *globrevenv = NULL, *output = NULL, *revput = NULL;
  mus_any **out_delays, **out_envs, **rev_envs;
  int *out_map;
  mus_long_t start, end;
  int outchans = 0, revchans = 0;
  Xen ref;

  #define H_make_move_sound "(" S_make_move_sound " dloc-list (out *output*) (rev *reverb*)): make a dlocsig run-time generator"

  /* dloc-list is (list start end outchans revchans dopdly dopenv revenv outdelays outenvs revenvs outmap) */
  /*   outdelays envs and revenvs are vectors */

  Xen_check_type(Xen_is_list(dloc_list) && (Xen_list_length(dloc_list) == 11), dloc_list, 1, S_make_move_sound, "a dlocsig list");

  if (!Xen_is_bound(outp))
    outp = CLM_OUTPUT;

  if (!Xen_is_bound(revp))
    revp = CLM_REVERB;

  if (mus_is_xen(outp))
    {
      output = Xen_to_mus_any(outp);
      Xen_check_type(mus_is_output(output), outp, 2, S_make_move_sound, "output stream");
    }
  else
    {
      if ((mus_is_vct(outp)) || 
	  (Xen_is_false(outp)) || 
	  (!Xen_is_bound(outp)))
	ov = outp;
      else Xen_check_type(false, outp, 2, S_make_move_sound, "output stream, " S_vct ", or a sound-data object");
    }

  if (mus_is_xen(revp))
    {
      revput = Xen_to_mus_any(revp);
      Xen_check_type(mus_is_output(revput), revp, 3, S_make_move_sound, "reverb stream");
    }
  else
    {
      if ((mus_is_vct(revp)) || 
	  (Xen_is_false(revp)) || 
	  (!Xen_is_bound(revp)))
	rv = revp;
      else Xen_check_type(false, revp, 3, S_make_move_sound, "reverb stream, " S_vct ", or a sound-data object");
    }

  ref = Xen_list_ref(dloc_list, 0);
  Xen_check_type(Xen_is_llong(ref), ref, 1, S_make_move_sound, "dlocsig list[0] (start): a sample number");
  start = Xen_llong_to_C_llong(ref);

  ref = Xen_list_ref(dloc_list, 1);
  Xen_check_type(Xen_is_llong(ref), ref, 1, S_make_move_sound, "dlocsig list[1] (end): a sample number");
  end = Xen_llong_to_C_llong(ref);

  ref = Xen_list_ref(dloc_list, 2);
  Xen_check_type(Xen_is_integer(ref), ref, 1, S_make_move_sound, "dlocsig list[2] (outchans): an integer");
  outchans = Xen_integer_to_C_int(ref);

  ref = Xen_list_ref(dloc_list, 3);
  Xen_check_type(Xen_is_integer(ref), ref, 1, S_make_move_sound, "dlocsig list[3] (revchans): an integer");
  revchans = Xen_integer_to_C_int(ref);

  ref = Xen_list_ref(dloc_list, 4);
  Xen_check_type(mus_is_xen(ref), ref, 1, S_make_move_sound, "dlocsig list[4] (doppler delay): a delay generator");
  dopdly = Xen_to_mus_any(ref);
  Xen_check_type(mus_is_delay(dopdly), ref, 1, S_make_move_sound, "dlocsig list[4] (doppler delay): a delay generator");

  ref = Xen_list_ref(dloc_list, 5);
  Xen_check_type(mus_is_xen(ref), ref, 1, S_make_move_sound, "dlocsig list[5] (doppler env): an env generator");
  dopenv = Xen_to_mus_any(ref);
  Xen_check_type(mus_is_env(dopenv), ref, 1, S_make_move_sound, "dlocsig list[5] (doppler env): an env generator");

  ref = Xen_list_ref(dloc_list, 6);
  Xen_check_type(Xen_is_false(ref) || mus_is_xen(ref), ref, 1, S_make_move_sound, "dlocsig list[6] (global rev env): an env generator");
  if (mus_is_xen(ref))
    {
      globrevenv = Xen_to_mus_any(ref);
      Xen_check_type(mus_is_env(globrevenv), ref, 1, S_make_move_sound, "dlocsig list[6] (global rev env): an env generator");
    }

  ref = Xen_list_ref(dloc_list, 7);
  Xen_check_type(Xen_is_vector(ref) && ((int)Xen_vector_length(ref) >= outchans), 
		  ref, 1, S_make_move_sound, "dlocsig list[7] (out delays): a vector of delay gens");

  ref = Xen_list_ref(dloc_list, 8);
  Xen_check_type(Xen_is_false(ref) || (Xen_is_vector(ref) && ((int)Xen_vector_length(ref) >= outchans)), 
		  ref, 1, S_make_move_sound, "dlocsig list[8] (out envs): " PROC_FALSE " or a vector of envs");

  ref = Xen_list_ref(dloc_list, 9);
  Xen_check_type(Xen_is_false(ref) || (Xen_is_vector(ref) && ((int)Xen_vector_length(ref) >= revchans)), 
		  ref, 1, S_make_move_sound, "dlocsig list[9] (rev envs): " PROC_FALSE " or a vector of envs");

  ref = Xen_list_ref(dloc_list, 10);
  Xen_check_type(Xen_is_vector(ref) && ((int)Xen_vector_length(ref) >= outchans), 
		  ref, 1, S_make_move_sound, "dlocsig list[10] (out map): vector of ints");

  /* put off allocation until all type error checks are done */

  out_delays = xen_vector_to_mus_any_array(Xen_list_ref(dloc_list, 7));
  out_envs = xen_vector_to_mus_any_array(Xen_list_ref(dloc_list, 8));
  rev_envs = xen_vector_to_mus_any_array(Xen_list_ref(dloc_list, 9));
  out_map = xen_vector_to_int_array(Xen_list_ref(dloc_list, 10));

  ge = mus_make_move_sound(start, end, outchans, revchans,
			   dopdly, dopenv, globrevenv,
			   out_delays, out_envs, rev_envs, out_map,
			   output, revput,
			   true, false);                  /* free outer arrays but not gens */
  if (ge)
    {
      mus_xen *gn;
      if (((Xen_is_bound(ov)) && (!Xen_is_false(ov))) || 
	  ((Xen_is_bound(rv)) && (!Xen_is_false(rv))))
	gn = mx_alloc(4);
      else gn = mx_alloc(1);
      gn->vcts[G_LOCSIG_DATA] = dloc_list; /* it is crucial that the list be gc-protected! */
      if (gn->nvcts == 4)
	{
	  mus_move_sound_set_detour(ge, clm_move_sound_detour);
	  gn->vcts[G_LOCSIG_OUT] = ov;
	  gn->vcts[G_LOCSIG_REVOUT] = rv;
	  gn->vcts[G_LOCSIG_REVDATA] = Xen_undefined;
	  mus_set_environ(ge, (void *)gn);
	}
      gn->gen = ge;
      return(mus_xen_to_object(gn));
    }

  return(Xen_false);
}




/* ---------------- src ---------------- */

static Xen xen_one, xen_minus_one;

#if HAVE_SCHEME
static Xen as_needed_arglist;

static s7_pointer env_symbol, all_pass_symbol, ina_symbol, polywave_symbol, triangle_wave_symbol;
static s7_pointer rand_interp_symbol, oscil_symbol, add_symbol, subtract_symbol, reverb_symbol, output_symbol, outa_symbol;
static s7_pointer multiply_symbol, vector_ref_symbol, quote_symbol, sin_symbol, cos_symbol, readin_symbol, abs_symbol;
static s7_pointer comb_bank_symbol, all_pass_bank_symbol, one_pole_symbol;


static mus_float_t as_needed_input_float(void *ptr, int direction)
{
  mus_xen *gn = (mus_xen *)ptr;
  return(s7_real(gn->vcts[MUS_INPUT_DATA]));
}

static mus_float_t as_needed_block_input_float(void *ptr, int direction, mus_float_t *data, mus_long_t start, mus_long_t end)
{
  mus_xen *gn = (mus_xen *)ptr;
  mus_float_t val;
  mus_long_t i, lim4;

  lim4 = end - 4;
  val = (mus_float_t)s7_real(gn->vcts[MUS_INPUT_DATA]); /* set in the chooser below */

  for (i = start; i <= lim4;)
    {
      data[i++] = val;
      data[i++] = val;
      data[i++] = val;
      data[i++] = val;
    }
  for (;i < end; i++)
    data[i] = val;
  return(val);
}


static mus_float_t as_needed_input_any(void *ptr, int direction)
{
  mus_xen *gn = (mus_xen *)ptr;
  s7_set_car(as_needed_arglist, (direction == 1) ? xen_one : xen_minus_one);
  return(s7_number_to_real(s7, s7_apply_function(s7, gn->vcts[MUS_INPUT_FUNCTION], as_needed_arglist)));
}
#endif


static mus_float_t as_needed_input_generator(void *ptr, int direction)
{
#if HAVE_EXTENSION_LANGUAGE
  return(mus_apply((mus_any *)(((mus_xen *)ptr)->vcts[MUS_INPUT_DATA]), 0.0, 0.0));
#else
  return(0.0);
#endif
}

static mus_float_t as_needed_block_input_generator(void *ptr, int direction, mus_float_t *data, mus_long_t start, mus_long_t end)
{
#if HAVE_EXTENSION_LANGUAGE
  mus_any *g;
  mus_long_t i;
  g = (mus_any *)(((mus_xen *)ptr)->vcts[MUS_INPUT_DATA]);
  for (i = start; i < end; i++)
    data[i] = mus_apply(g, 0.0, 0.0);
#endif
  return(0.0);
}


static mus_float_t as_needed_input_readin(void *ptr, int direction)
{
#if HAVE_EXTENSION_LANGUAGE
  return(mus_readin((mus_any *)(((mus_xen *)ptr)->vcts[MUS_INPUT_DATA])));
#else
  return(0.0);
#endif
}

static mus_float_t as_needed_block_input_readin(void *ptr, int direction, mus_float_t *data, mus_long_t start, mus_long_t end)
{
#if HAVE_EXTENSION_LANGUAGE
  mus_any *g;
  mus_long_t i;
  g = (mus_any *)(((mus_xen *)ptr)->vcts[MUS_INPUT_DATA]);
  for (i = start; i < end; i++)
    data[i] = mus_readin(g);
#endif
  return(0.0);
}


#if USE_SND && HAVE_SCHEME
static mus_float_t as_needed_input_sampler(void *ptr, int direction)
{
  return(read_sample((snd_fd *)(((mus_xen *)ptr)->vcts[MUS_INPUT_DATA])));
}

static mus_float_t as_needed_block_input_sampler(void *ptr, int direction, mus_float_t *data, mus_long_t start, mus_long_t end)
{
  snd_fd *p;
  mus_long_t i;
  p = (snd_fd *)(((mus_xen *)ptr)->vcts[MUS_INPUT_DATA]);
  for (i = start; i < end; i++)
    data[i] = read_sample(p);
  return(0.0);
}


mus_float_t read_sample_with_direction(void *p, int dir);
static mus_float_t as_needed_input_sampler_with_direction(void *ptr, int direction)
{
  return(read_sample_with_direction((snd_fd *)(((mus_xen *)ptr)->vcts[MUS_INPUT_DATA]), direction));
}

static mus_float_t as_needed_block_input_sampler_with_direction(void *ptr, int direction, mus_float_t *data, mus_long_t start, mus_long_t end)
{
  snd_fd *p;
  mus_long_t i;
  p = (snd_fd *)(((mus_xen *)ptr)->vcts[MUS_INPUT_DATA]);
  for (i = start; i < end; i++)
    data[i] = read_sample_with_direction(p, direction);
  return(0.0);
}
#endif


static mus_float_t as_needed_input_func(void *ptr, int direction) /* intended for "as-needed" input funcs */
{
  mus_xen *gn = (mus_xen *)ptr;
  if (gn)
    {
      Xen in_obj;
      in_obj = gn->vcts[MUS_INPUT_FUNCTION];
      if (Xen_is_procedure(in_obj))
	return(Xen_real_to_C_double(Xen_unprotected_call_with_1_arg(gn->vcts[MUS_INPUT_FUNCTION], (direction == 1) ? xen_one : xen_minus_one)));
    }
  return(0.0);
}


static void set_as_needed_input_choices(mus_any *gen, Xen obj, mus_xen *gn)
{
  /* fprintf(stderr, "set_as_needed_input for %s: %s\n", mus_name(gen), DISPLAY(obj)); */
  if (mus_is_xen(obj)) /* input function is a generator */
    {
      mus_any *p;
      p = Xen_to_mus_any(obj);
      if (p) 
	{
#if HAVE_EXTENSION_LANGUAGE
	  gn->vcts[MUS_INPUT_DATA] = (Xen)p; 
#endif
	  if (mus_is_readin(p))
	    mus_generator_set_feeders(gen, as_needed_input_readin, as_needed_block_input_readin);
	  else mus_generator_set_feeders(gen, as_needed_input_generator, as_needed_block_input_generator);
	  return;
	}
    }

#if HAVE_SCHEME
  if (Xen_is_procedure(obj))
    {
      s7_pointer body;
      body = s7_closure_body(s7, obj);
      if (s7_is_pair(body))
	{
	  if (s7_is_null(s7, s7_cdr(body)))
	    {
	      s7_pointer res;
	      res = s7_car(body);
	      if (s7_is_real(res))
		{
		  gn->vcts[MUS_INPUT_DATA] = res;
		  mus_generator_set_feeders(gen, as_needed_input_float, as_needed_block_input_float);
		  return;
		}
	      if (s7_is_pair(res))
		{
		  s7_pointer arg;
		  arg = s7_car(s7_closure_args(s7, obj));
#if USE_SND
		  if ((arg == s7_caddr(res)) &&
		      (s7_car(res) == s7_make_symbol(s7, "read-sample-with-direction")))
		    {
		      gn->vcts[MUS_INPUT_DATA] = (Xen)xen_to_sampler(s7_symbol_local_value(s7, s7_cadr(res), s7_closure_let(s7, obj)));
		      mus_generator_set_feeders(gen, as_needed_input_sampler_with_direction, as_needed_block_input_sampler_with_direction);
		      return;
		    }
#endif
		}
	    }
	}
#if USE_SND
      /* check for a sampler (snd-edits.c) */
      if (is_sampler(obj))
	{
	  gn->vcts[MUS_INPUT_DATA] = (Xen)xen_to_sampler(obj);
	  mus_generator_set_feeders(gen, as_needed_input_sampler, as_needed_block_input_sampler);
	  return;
	}
      mus_generator_set_feeders(gen, as_needed_input_any, NULL);
      return;
#endif	  
    }
#endif
  mus_generator_set_feeders(gen, as_needed_input_func, NULL);
}


static Xen g_mus_clear_sincs(void)
{
  mus_clear_sinc_tables();
  return(Xen_false);
}


static Xen g_is_src(Xen obj) 
{
  #define H_is_src "(" S_is_src " gen): " PROC_TRUE " if gen is an " S_src
  return(C_bool_to_Xen_boolean((mus_is_xen(obj)) && (mus_is_src(Xen_to_mus_any(obj)))));
}


#define SRC_CHANGE_MAX 1000000.0

static Xen g_src(Xen obj, Xen pm, Xen func) 
{
  #define H_src "(" S_src " gen (pm 0.0) input-function): next sampling rate conversion sample. \
'pm' can be used to change the sampling rate on a sample-by-sample basis.  'input-function' \
is a function of one argument (the current input direction, normally ignored) that is called \
internally whenever a new sample of input data is needed.  If the associated " S_make_src " \
included an 'input' argument, input-function is ignored."

  mus_float_t pm1 = 0.0;
  mus_xen *gn;
  mus_any *g = NULL;

  Xen_to_C_generator(obj, gn, g, mus_is_src, S_src, "an src generator");
  Xen_real_to_C_double_if_bound(pm, pm1, S_src, 2);

  /* if sr_change (pm1) is ridiculous, complain! */
  if ((pm1 > SRC_CHANGE_MAX) || (pm1 < -SRC_CHANGE_MAX))
    Xen_out_of_range_error(S_src, 2, pm, "src change too large");

  if (!Xen_is_bound(gn->vcts[MUS_INPUT_DATA]))
    {
      if (Xen_is_procedure(func))
	{
	  if (Xen_is_aritable(func, 1))
	    gn->vcts[MUS_INPUT_FUNCTION] = func;
	  else Xen_bad_arity_error(S_src, 3, func, "src input function wants 1 arg");
	}
    }
  return(C_double_to_Xen_real(mus_src(g, pm1, NULL)));
}

static void set_gn_gen(void *p, mus_any *g)
{
  mus_xen *gn = (mus_xen *)p;
  gn->gen = g;
}

static Xen g_make_src(Xen arg1, Xen arg2, Xen arg3, Xen arg4, Xen arg5, Xen arg6)
{
  #define H_make_src "(" S_make_src " input (srate 1.0) (width 10)): \
return a new sampling-rate conversion generator (using 'warped sinc interpolation'). \
'srate' is the ratio between the new rate and the old. 'width' is the sine \
width (effectively the steepness of the low-pass filter), normally between 10 and 100. \
'input' if given is an open file stream."

  Xen in_obj = Xen_undefined;
  mus_xen *gn;
  mus_any *ge = NULL;
  int vals, wid = 0; /* 0 here picks up the current default width in clm.c */
  Xen args[6]; 
  Xen keys[3];
  int orig_arg[3] = {0, 0, 0};
  mus_float_t srate = 1.0;

  keys[0] = kw_input;
  keys[1] = kw_srate;
  keys[2] = kw_width;
  args[0] = arg1; args[1] = arg2; args[2] = arg3; args[3] = arg4; args[4] = arg5; args[5] = arg6;

  vals = mus_optkey_unscramble(S_make_src, 3, keys, args, orig_arg);
  if (vals > 0)
    {
      in_obj = mus_optkey_to_input_procedure(keys[0], S_make_src, orig_arg[0], Xen_undefined, 1, "src input procedure takes 1 arg");

      srate = Xen_optkey_to_float(kw_srate, keys[1], S_make_src, orig_arg[1], srate);
      /* srate can be negative => read in reverse */

      wid = Xen_optkey_to_int(kw_width, keys[2], S_make_src, orig_arg[2], wid);
      if (wid < 0) 
	Xen_out_of_range_error(S_make_src, orig_arg[2], keys[2], "width < 0?");
      if (wid > 2000) 
	Xen_out_of_range_error(S_make_src, orig_arg[2], keys[2], "width > 2000?");
    }

  gn = mx_alloc(MUS_MAX_VCTS);
  {int i; for (i = 0; i < MUS_MAX_VCTS; i++) gn->vcts[i] = Xen_undefined;}
  /* mus_make_src assumes it can invoke the input function! */
  gn->vcts[MUS_INPUT_FUNCTION] = in_obj;

  {
    mus_error_handler_t *old_error_handler;
    old_error_handler = mus_error_set_handler(local_mus_error);
    ge = mus_make_src_with_init(NULL, srate, wid, gn, set_gn_gen);
    mus_error_set_handler(old_error_handler);
  }

  if (ge)
    {
      Xen src_obj;
#if HAVE_SCHEME
      int loc;
#endif

      gn->gen = ge;
      src_obj = mus_xen_to_object(gn);
#if HAVE_SCHEME
      loc = s7_gc_protect(s7, src_obj);
#endif
      /* src_init can call an input function which can trigger the GC, so we need to GC-protect the new object */

      gn->vcts[MUS_SELF_WRAPPER] = src_obj;
      set_as_needed_input_choices(ge, in_obj, gn);
      mus_src_init(ge);
#if HAVE_SCHEME
      s7_gc_unprotect_at(s7, loc);
#endif
      return(src_obj);
    }

  free(gn->vcts);
  free(gn);
  return(clm_mus_error(local_error_type, local_error_msg, S_make_src));
}




/* ---------------- granulate ---------------- */

static Xen g_is_granulate(Xen obj) 
{
  #define H_is_granulate "(" S_is_granulate " gen): " PROC_TRUE " if gen is a " S_granulate " generator"
  return(C_bool_to_Xen_boolean((mus_is_xen(obj)) && (mus_is_granulate(Xen_to_mus_any(obj)))));
}


static int grnedit(void *ptr)
{
  mus_xen *gn = (mus_xen *)ptr;
  return(Xen_integer_to_C_int(Xen_unprotected_call_with_1_arg(gn->vcts[MUS_EDIT_FUNCTION], gn->vcts[MUS_SELF_WRAPPER])));
}


static Xen g_granulate(Xen obj, Xen func, Xen edit_func) 
{
  #define H_granulate "(" S_granulate " gen input-func edit-func): next sample from granular synthesis generator"
  mus_xen *gn;
  mus_any *g = NULL;

  Xen_to_C_generator(obj, gn, g, mus_is_granulate, S_granulate, "a granulate generator");

  if ((Xen_is_bound(func)) &&
      (!Xen_is_bound(gn->vcts[MUS_INPUT_DATA])))
    {
      if (Xen_is_procedure(func))
	{
	  if (Xen_is_aritable(func, 1))
	    gn->vcts[MUS_INPUT_FUNCTION] = func;
	  else Xen_bad_arity_error(S_granulate, 2, func, "granulate input function wants 1 arg");
	}
      if (Xen_is_procedure(edit_func))
	{
	  if (Xen_is_aritable(edit_func, 1))
	    {
	      if (!(Xen_is_bound(gn->vcts[MUS_EDIT_FUNCTION]))) /* default value is Xen_undefined */
		{
		  mus_granulate_set_edit_function(gn->gen, grnedit);
		  gn->vcts[MUS_EDIT_FUNCTION] = edit_func;
		}
	    }
	  else Xen_bad_arity_error(S_granulate, 3, edit_func, "granulate edit function wants 1 arg");
	}
    }
  return(C_double_to_Xen_real(mus_granulate(g, NULL)));
}


static Xen g_make_granulate(Xen arglist)
{
  #define H_make_granulate "(" S_make_granulate " input (expansion 1.0) (length .15) (scaler .6) (hop .05) (ramp .4) (jitter 1.0) max-size edit): \
return a new granular synthesis generator.  'length' is the grain length (seconds), 'expansion' is the ratio in timing \
between the new and old (expansion > 1.0 slows things down), 'scaler' scales the grains \
to avoid overflows, 'hop' is the spacing (seconds) between successive grains upon output. \
'jitter' controls the randomness in that spacing, 'input' can be a file pointer. 'edit' can \
be a function of one arg, the current granulate generator.  It is called just before \
a grain is added into the output buffer. The current grain is accessible via " S_mus_data ". \
The edit function, if any, should return the length in samples of the grain, or 0."

  Xen in_obj = Xen_undefined;
  mus_xen *gn;
  mus_any *ge;
  Xen args[18];
  Xen keys[9];
  int orig_arg[9] = {0, 0, 0, 0, 0, 0, 0, 0, 0};
  int vals, maxsize = 0;
  mus_float_t expansion = 1.0, segment_length = .15, segment_scaler = .6, ramp_time = .4, output_hop = .05;
  mus_float_t jitter = 1.0;
  Xen edit_obj = Xen_undefined, grn_obj;

  keys[0] = kw_input;
  keys[1] = kw_expansion;
  keys[2] = kw_length;
  keys[3] = kw_scaler;
  keys[4] = kw_hop;
  keys[5] = kw_ramp;
  keys[6] = kw_jitter;
  keys[7] = kw_max_size;
  keys[8] = kw_edit;

  {
    int i, arglist_len;
    Xen p;
    arglist_len = Xen_list_length(arglist);
    if (arglist_len > 18) clm_error(S_make_granulate, "too many arguments!", arglist);
    for (i = 0, p = arglist; i < arglist_len; i++, p = Xen_cdr(p)) args[i] = Xen_car(p);
    for (i = arglist_len; i < 18; i++) args[i] = Xen_undefined;
  }

  vals = mus_optkey_unscramble(S_make_granulate, 9, keys, args, orig_arg);
  if (vals > 0)
    {
      in_obj = mus_optkey_to_input_procedure(keys[0], S_make_granulate, orig_arg[0], Xen_undefined, 1, "granulate input procedure takes 1 arg");

      expansion = Xen_optkey_to_float(kw_expansion, keys[1], S_make_granulate, orig_arg[1], expansion);
      if (expansion <= 0.0) 
	Xen_out_of_range_error(S_make_granulate, orig_arg[1], keys[1], "expansion <= 0.0?");

      segment_length = Xen_optkey_to_float(kw_length, keys[2], S_make_granulate, orig_arg[2], segment_length);
      if (segment_length <= 0.0) 
	Xen_out_of_range_error(S_make_granulate, orig_arg[2], keys[2], "segment-length <= 0.0?");

      segment_scaler = Xen_optkey_to_float(kw_scaler, keys[3], S_make_granulate, orig_arg[3], segment_scaler);
      if (segment_scaler == 0.0) 
	Xen_out_of_range_error(S_make_granulate, orig_arg[3], keys[3], "segment-scaler should be greater than 0.0?");

      output_hop = Xen_optkey_to_float(kw_hop, keys[4], S_make_granulate, orig_arg[4], output_hop);
      if (output_hop <= 0.0) 
	Xen_out_of_range_error(S_make_granulate, orig_arg[4], keys[4], "hop <= 0?");
      if (output_hop > 3600.0) 
	Xen_out_of_range_error(S_make_granulate, orig_arg[4], keys[4], "hop > 3600?");
      if ((segment_length + output_hop) > 60.0) /* multiplied by srate in mus_make_granulate in array allocation */
	Xen_out_of_range_error(S_make_granulate, orig_arg[2], Xen_list_2(keys[2], keys[4]), "segment_length + output_hop too large!");

      ramp_time = Xen_optkey_to_float(kw_ramp, keys[5], S_make_granulate, orig_arg[5], ramp_time);
      if ((ramp_time < 0.0) || (ramp_time > 0.5))
	Xen_out_of_range_error(S_make_granulate, orig_arg[5], keys[5], "ramp must be between 0.0 and 0.5");

      jitter = Xen_optkey_to_float(kw_jitter, keys[6], S_make_granulate, orig_arg[6], jitter);
      Xen_check_type((jitter >= 0.0) && (jitter < 100.0), keys[6], orig_arg[6], S_make_granulate, "0.0 .. 100.0");

      maxsize = Xen_optkey_to_int(kw_max_size, keys[7], S_make_granulate, orig_arg[7], maxsize);
      if ((maxsize > mus_max_malloc()) || 
	  (maxsize < 0) ||
	  ((maxsize == 0) && (!Xen_is_keyword(keys[7]))))
	Xen_out_of_range_error(S_make_granulate, orig_arg[7], keys[7], "max-size invalid");

      edit_obj = mus_optkey_to_procedure(keys[8], S_make_granulate, orig_arg[8], Xen_undefined, 1, "granulate edit procedure takes 1 arg");
    }

  gn = mx_alloc(MUS_MAX_VCTS);
  {int i; for (i = 0; i < MUS_MAX_VCTS; i++) gn->vcts[i] = Xen_undefined;}
  {
    mus_error_handler_t *old_error_handler;
    old_error_handler = mus_error_set_handler(local_mus_error);
    ge = mus_make_granulate(NULL, 
			    expansion, segment_length, segment_scaler, output_hop, ramp_time, jitter, maxsize, 
			    (!Xen_is_bound(edit_obj) ? NULL : grnedit),
			    (void *)gn);
    mus_error_set_handler(old_error_handler);
  }

  if (ge)
    {
      gn->vcts[MUS_DATA_WRAPPER] = xen_make_vct_wrapper(mus_granulate_grain_max_length(ge), mus_data(ge));
      gn->vcts[MUS_INPUT_FUNCTION] = in_obj;
      gn->vcts[MUS_EDIT_FUNCTION] = edit_obj;
      gn->gen = ge;
      grn_obj = mus_xen_to_object(gn);
      gn->vcts[MUS_SELF_WRAPPER] = grn_obj;
      set_as_needed_input_choices(ge, in_obj, gn);
      return(grn_obj);
    }

  free(gn->vcts);
  free(gn);
  return(clm_mus_error(local_error_type, local_error_msg, S_make_granulate));
}




/* ---------------- convolve ---------------- */

static Xen g_is_convolve(Xen obj) 
{
  #define H_is_convolve "(" S_is_convolve " gen): " PROC_TRUE " if gen is a " S_convolve " generator"
  return(C_bool_to_Xen_boolean((mus_is_xen(obj)) && (mus_is_convolve(Xen_to_mus_any(obj)))));
}


static Xen g_convolve(Xen obj, Xen func) 
{
  #define H_convolve_gen "(" S_convolve " gen input-func): next sample from convolution generator"
  mus_xen *gn;
  mus_any *g = NULL;

  Xen_to_C_generator(obj, gn, g, mus_is_convolve, S_convolve, "a convolve generator");

  if (!Xen_is_bound(gn->vcts[MUS_INPUT_DATA]))
    {
      if (Xen_is_procedure(func))
	{
	  if (Xen_is_aritable(func, 1))
	    gn->vcts[MUS_INPUT_FUNCTION] = func;
	  else Xen_bad_arity_error(S_convolve, 2, func, "convolve input function wants 1 arg");
	}
    }
  return(C_double_to_Xen_real(mus_convolve(g, NULL)));
}


/* filter-size? */

static Xen g_make_convolve(Xen arglist)
{
  #define H_make_convolve "(" S_make_convolve " input filter fft-size): \
return a new convolution generator which convolves its input with the impulse response 'filter'."

  mus_xen *gn;
  mus_any *ge;
  Xen args[6];
  Xen keys[3];
  int orig_arg[3] = {0, 0, 0};
  int vals;
  vct *filter = NULL;
  Xen filt = Xen_undefined, in_obj = Xen_undefined;
  mus_long_t fftlen, fft_size = 0;

  keys[0] = kw_input;
  keys[1] = kw_filter;
  keys[2] = kw_fft_size;

  {
    int i, arglist_len;
    Xen p;
    arglist_len = Xen_list_length(arglist);
    if (arglist_len > 6) clm_error(S_make_convolve, "too many arguments!", arglist);
    for (i = 0, p = arglist; i < arglist_len; i++, p = Xen_cdr(p)) args[i] = Xen_car(p);
    for (i = arglist_len; i < 6; i++) args[i] = Xen_undefined;
  }

  vals = mus_optkey_unscramble(S_make_convolve, 3, keys, args, orig_arg);
  if (vals > 0)
    {
      in_obj = mus_optkey_to_input_procedure(keys[0], S_make_convolve, orig_arg[0], Xen_undefined, 1, "convolve input procedure takes 1 arg");

      filter = mus_optkey_to_vct(keys[1], S_make_convolve, orig_arg[1], NULL);
      if (filter) filt = keys[1];

      fft_size = Xen_optkey_to_mus_long_t(kw_fft_size, keys[2], S_make_convolve, orig_arg[2], fft_size);
      if ((fft_size  < 0) || 
	  ((fft_size == 0) && (!Xen_is_keyword(keys[2]))) ||
	  (fft_size > mus_max_malloc()))
	Xen_out_of_range_error(S_make_convolve, orig_arg[2], keys[2], "fft-size invalid (see mus-max-malloc))");
    }

  if (filter == NULL)
    Xen_error(NO_DATA,
	      Xen_list_1(C_string_to_Xen_string(S_make_convolve ": no impulse (filter)?")));

  if (is_power_of_2(mus_vct_length(filter)))
    fftlen = mus_vct_length(filter) * 2;
  else fftlen = (mus_long_t)pow(2.0, 1 + (int)(log((mus_float_t)(mus_vct_length(filter) + 1)) / log(2.0)));
  if (fft_size < fftlen) fft_size = fftlen;

  gn = mx_alloc(MUS_MAX_VCTS);
  {int i; for (i = 0; i < MUS_MAX_VCTS; i++) gn->vcts[i] = Xen_undefined;}
  {
    mus_error_handler_t *old_error_handler;
    old_error_handler = mus_error_set_handler(local_mus_error);
    ge = mus_make_convolve(NULL, mus_vct_data(filter), fft_size, mus_vct_length(filter), gn);
    mus_error_set_handler(old_error_handler);
  }

  if (ge)
    {
      Xen c_obj;
      gn->vcts[MUS_INPUT_FUNCTION] = in_obj;
      gn->vcts[MUS_ANALYZE_FUNCTION] = filt; /* why is this here? GC protection? (might be a locally-allocated vct as from file->vct) */
      gn->gen = ge;
      c_obj = mus_xen_to_object(gn);
      gn->vcts[MUS_SELF_WRAPPER] = c_obj;
      set_as_needed_input_choices(ge, in_obj, gn);
      return(c_obj);
    }

  free(gn->vcts);
  free(gn);
  return(clm_mus_error(local_error_type, local_error_msg, S_make_convolve));
}


static Xen g_convolve_files(Xen file1, Xen file2, Xen maxamp, Xen outfile)
{
  #define H_convolve_files "(" S_convolve_files " file1 file2 maxamp output-file): convolve \
file1 and file2 writing outfile after scaling the convolution result to maxamp."

  const char *f1, *f2, *f3;
  mus_float_t maxval = 1.0;

  Xen_check_type(Xen_is_string(file1), file1, 1, S_convolve_files, "a string");
  Xen_check_type(Xen_is_string(file2), file2, 2, S_convolve_files, "a string");
  Xen_check_type(Xen_is_number_or_unbound(maxamp), maxamp, 3, S_convolve_files, "a number");
  Xen_check_type((!Xen_is_bound(outfile)) || (Xen_is_string(outfile)), outfile, 4, S_convolve_files, "a string");

  f1 = Xen_string_to_C_string(file1);
  f2 = Xen_string_to_C_string(file2);
  if (Xen_is_string(outfile)) 
    f3 = Xen_string_to_C_string(outfile); 
  else f3 = "tmp.snd";
  if (Xen_is_number(maxamp)) 
    maxval = Xen_real_to_C_double(maxamp);

  mus_convolve_files(f1, f2, maxval, f3);
  return(C_string_to_Xen_string(f3));
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
 *   object that packages the mus_xen pointer for Scheme.  This way, the user's
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
  return(Xen_boolean_to_C_bool(Xen_unprotected_call_with_1_arg(gn->vcts[MUS_EDIT_FUNCTION], gn->vcts[MUS_SELF_WRAPPER])));
}


static mus_float_t pvsynthesize(void *ptr)
{
  mus_xen *gn = (mus_xen *)ptr;
  return(Xen_real_to_C_double(Xen_unprotected_call_with_1_arg(gn->vcts[MUS_SYNTHESIZE_FUNCTION], gn->vcts[MUS_SELF_WRAPPER])));
}


static bool pvanalyze(void *ptr, mus_float_t (*input)(void *arg1, int direction))
{
  mus_xen *gn = (mus_xen *)ptr;
  /* we can only get input func if it's already set up by the outer gen call, so (?) we can use that function here */
  return(Xen_boolean_to_C_bool(Xen_unprotected_call_with_2_args(gn->vcts[MUS_ANALYZE_FUNCTION], 
					      gn->vcts[MUS_SELF_WRAPPER], 
					      gn->vcts[MUS_INPUT_FUNCTION])));
}


static Xen g_is_phase_vocoder(Xen obj) 
{
  #define H_is_phase_vocoder "(" S_is_phase_vocoder " gen): " PROC_TRUE " if gen is an " S_phase_vocoder
  return(C_bool_to_Xen_boolean((mus_is_xen(obj)) && (mus_is_phase_vocoder(Xen_to_mus_any(obj)))));
}


static Xen g_phase_vocoder(Xen obj, Xen func, Xen analyze_func, Xen edit_func, Xen synthesize_func)
{
  #define H_phase_vocoder "(" S_phase_vocoder " gen input-function analyze-func edit-func synthesize-func): next phase vocoder value"
  mus_xen *gn;
  mus_any *g = NULL;

  Xen_to_C_generator(obj, gn, g, mus_is_phase_vocoder, S_phase_vocoder, "a phase-vocoder generator");

  if (Xen_is_bound(func))
    {
      bool (*analyze)(void *arg, mus_float_t (*input)(void *arg1, int direction)) = NULL;
      int (*edit)(void *arg) = NULL;
      mus_float_t (*synthesize)(void *arg) = NULL;

      if ((Xen_is_procedure(func)) &&
	  (!Xen_is_bound(gn->vcts[MUS_INPUT_DATA])))
	{
	  if (Xen_is_aritable(func, 1))
	    gn->vcts[MUS_INPUT_FUNCTION] = func; /* as_needed_input_func set at make time will pick this up */
	  else Xen_bad_arity_error(S_phase_vocoder, 2, func, S_phase_vocoder " input function wants 1 arg");
	}
      if (Xen_is_procedure(analyze_func))
	{
	  if (Xen_is_aritable(analyze_func, 2))
	    {
	      gn->vcts[MUS_ANALYZE_FUNCTION] = analyze_func;
	      analyze = pvanalyze;
	    }
	  else Xen_bad_arity_error(S_phase_vocoder, 3, analyze_func, S_phase_vocoder " analyze function wants 2 args");
	}
      if (Xen_is_procedure(edit_func))
	{
	  if (Xen_is_aritable(edit_func, 1))
	    {
	      gn->vcts[MUS_EDIT_FUNCTION] = edit_func;
	      edit = pvedit;
	    }
	  else Xen_bad_arity_error(S_phase_vocoder, 4, edit_func, S_phase_vocoder " edit function wants 1 arg");
	}
      if (Xen_is_procedure(synthesize_func))
	{
	  if (Xen_is_aritable(synthesize_func, 1))
	    {
	      gn->vcts[MUS_SYNTHESIZE_FUNCTION] = synthesize_func;
	      synthesize = pvsynthesize;
	    }
	  else Xen_bad_arity_error(S_phase_vocoder, 5, synthesize_func, S_phase_vocoder " synthesize function wants 1 arg");
	}
      return(C_double_to_Xen_real(mus_phase_vocoder_with_editors(g, NULL, analyze, edit, synthesize)));
    }
  return(C_double_to_Xen_real(mus_phase_vocoder(g, NULL)));
}


static Xen g_make_phase_vocoder(Xen arglist)
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

  Xen in_obj = Xen_undefined, edit_obj = Xen_undefined, synthesize_obj = Xen_undefined, analyze_obj = Xen_undefined;
  mus_xen *gn;
  mus_any *ge;
  Xen args[16];
  Xen keys[8];
  Xen pv_obj;
  int orig_arg[8] = {0, 0, 0, 0, 0, 0, 0, 0};
  int vals;
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

  {
    int i, arglist_len;
    Xen p;
    arglist_len = Xen_list_length(arglist);
    if (arglist_len > 16) clm_error(S_make_phase_vocoder, "too many arguments!", arglist);
    for (i = 0, p = arglist; i < arglist_len; i++, p = Xen_cdr(p)) args[i] = Xen_car(p);
    for (i = arglist_len; i < 16; i++) args[i] = Xen_undefined;
  }

  vals = mus_optkey_unscramble(S_make_phase_vocoder, 8, keys, args, orig_arg);
  if (vals > 0)
    {
      in_obj = mus_optkey_to_input_procedure(keys[0], S_make_phase_vocoder, orig_arg[0], Xen_undefined, 1, S_phase_vocoder " input procedure takes 1 arg");

      fft_size = Xen_optkey_to_int(kw_fft_size, keys[1], S_make_phase_vocoder, orig_arg[1], fft_size);
      if (fft_size <= 1) 
	Xen_out_of_range_error(S_make_phase_vocoder, orig_arg[1], keys[1], "fft size <= 1?");
      if (fft_size > mus_max_malloc())
	Xen_out_of_range_error(S_make_phase_vocoder, orig_arg[1], keys[1], "fft size too large (see mus-max-malloc)");
      if (!is_power_of_2(fft_size))
	Xen_out_of_range_error(S_make_phase_vocoder, orig_arg[1], keys[1], "fft size must be power of 2");

      overlap = Xen_optkey_to_int(kw_overlap, keys[2], S_make_phase_vocoder, orig_arg[2], overlap);
      if (overlap <= 0)
	Xen_out_of_range_error(S_make_phase_vocoder, orig_arg[2], keys[2], "overlap <= 0?");

      interp = Xen_optkey_to_int(kw_interp, keys[3], S_make_phase_vocoder, orig_arg[3], interp);
      if (interp <= 0)
	Xen_out_of_range_error(S_make_phase_vocoder, orig_arg[3], keys[3], "interp <= 0?");

      pitch = Xen_optkey_to_float(kw_pitch, keys[4], S_make_phase_vocoder, orig_arg[4], pitch);

      analyze_obj = mus_optkey_to_procedure(keys[5], S_make_phase_vocoder, orig_arg[5], Xen_undefined, 2, S_phase_vocoder " analyze procedure takes 2 args");
      edit_obj = mus_optkey_to_procedure(keys[6], S_make_phase_vocoder, orig_arg[6], Xen_undefined, 1, S_phase_vocoder " edit procedure takes 1 arg");
      synthesize_obj = mus_optkey_to_procedure(keys[7], S_make_phase_vocoder, orig_arg[7], Xen_undefined, 1, S_phase_vocoder " synthesize procedure takes 1 arg");
    }

  gn = mx_alloc(MUS_MAX_VCTS);
  {int i; for (i = 0; i < MUS_MAX_VCTS; i++) gn->vcts[i] = Xen_undefined;}
  {
    mus_error_handler_t *old_error_handler;
    old_error_handler = mus_error_set_handler(local_mus_error);
    ge = mus_make_phase_vocoder(NULL,
				fft_size, overlap, interp, pitch,
				(!Xen_is_bound(analyze_obj) ? NULL : pvanalyze),
				(!Xen_is_bound(edit_obj) ? NULL : pvedit),
				(!Xen_is_bound(synthesize_obj) ? NULL : pvsynthesize),
				(void *)gn);
    mus_error_set_handler(old_error_handler);
  }

  if (ge)
    {
      gn->vcts[MUS_INPUT_FUNCTION] = in_obj;
      gn->vcts[MUS_EDIT_FUNCTION] = edit_obj;
      gn->vcts[MUS_ANALYZE_FUNCTION] = analyze_obj;
      gn->vcts[MUS_SYNTHESIZE_FUNCTION] = synthesize_obj;
      gn->gen = ge;
      pv_obj = mus_xen_to_object(gn);
      /* need scheme-relative backpointer for possible function calls */
      gn->vcts[MUS_SELF_WRAPPER] = pv_obj;
      set_as_needed_input_choices(ge, in_obj, gn);
      return(pv_obj);
    }

  free(gn->vcts);
  free(gn);
  return(clm_mus_error(local_error_type, local_error_msg, S_make_phase_vocoder));
}


static Xen g_phase_vocoder_amps(Xen pv) 
{
  #define H_phase_vocoder_amps "(" S_phase_vocoder_amps " gen): " S_vct " containing the current output sinusoid amplitudes"
  mus_float_t *amps; 
  int len;
  mus_xen *gn;

  Xen_check_type((mus_is_xen(pv)) && (mus_is_phase_vocoder(Xen_to_mus_any(pv))), pv, 1, S_phase_vocoder_amps, "a " S_phase_vocoder " generator");

  gn = Xen_to_mus_xen(pv);
  amps = mus_phase_vocoder_amps(gn->gen); 
  len = (int)mus_length(gn->gen);
  return(xen_make_vct_wrapper(len / 2, amps));
}

  
static Xen g_phase_vocoder_freqs(Xen pv) 
{
  #define H_phase_vocoder_freqs "(" S_phase_vocoder_freqs " gen): " S_vct " containing the current output sinusoid frequencies"
  mus_float_t *amps; 
  int len;
  mus_xen *gn;

  Xen_check_type((mus_is_xen(pv)) && (mus_is_phase_vocoder(Xen_to_mus_any(pv))), pv, 1, S_phase_vocoder_freqs, "a " S_phase_vocoder " generator");

  gn = Xen_to_mus_xen(pv);
  amps = mus_phase_vocoder_freqs(gn->gen); 
  len = (int)mus_length(gn->gen);
  return(xen_make_vct_wrapper(len, amps));
}

  
static Xen g_phase_vocoder_phases(Xen pv) 
{
  #define H_phase_vocoder_phases "(" S_phase_vocoder_phases " gen): " S_vct " containing the current output sinusoid phases"
  mus_float_t *amps; 
  int len;
  mus_xen *gn;

  Xen_check_type((mus_is_xen(pv)) && (mus_is_phase_vocoder(Xen_to_mus_any(pv))), pv, 1, S_phase_vocoder_phases, "a " S_phase_vocoder " generator");

  gn = Xen_to_mus_xen(pv);
  amps = mus_phase_vocoder_phases(gn->gen); 
  len = (int)mus_length(gn->gen);
  return(xen_make_vct_wrapper(len / 2, amps));
}
  

static Xen g_phase_vocoder_amp_increments(Xen pv) 
{
  #define H_phase_vocoder_amp_increments "(" S_phase_vocoder_amp_increments " gen): " S_vct " containing the current output sinusoid amplitude increments per sample"
  mus_float_t *amps; 
  int len;
  mus_xen *gn;

  Xen_check_type((mus_is_xen(pv)) && (mus_is_phase_vocoder(Xen_to_mus_any(pv))), pv, 1, S_phase_vocoder_amp_increments, "a " S_phase_vocoder " generator");

  gn = Xen_to_mus_xen(pv);
  amps = mus_phase_vocoder_amp_increments(gn->gen); 
  len = (int)mus_length(gn->gen);
  return(xen_make_vct_wrapper(len, amps));
}
  

static Xen g_phase_vocoder_phase_increments(Xen pv) 
{
  #define H_phase_vocoder_phase_increments "(" S_phase_vocoder_phase_increments " gen): " S_vct " containing the current output sinusoid phase increments"
  mus_float_t *amps; 
  int len;
  mus_xen *gn;

  Xen_check_type((mus_is_xen(pv)) && (mus_is_phase_vocoder(Xen_to_mus_any(pv))), pv, 1, S_phase_vocoder_phase_increments, "a " S_phase_vocoder " generator");

  gn = Xen_to_mus_xen(pv);
  amps = mus_phase_vocoder_phase_increments(gn->gen); 
  len = (int)mus_length(gn->gen);
  return(xen_make_vct_wrapper(len / 2, amps));
}

  


/* -------- ssb-am -------- */

static Xen g_is_ssb_am(Xen obj) 
{
  #define H_is_ssb_am "(" S_is_ssb_am " gen): " PROC_TRUE " if gen is a " S_ssb_am
  return(C_bool_to_Xen_boolean((mus_is_xen(obj)) && (mus_is_ssb_am(Xen_to_mus_any(obj)))));
}


static Xen g_make_ssb_am(Xen arg1, Xen arg2, Xen arg3, Xen arg4)
{
  #define H_make_ssb_am "(" S_make_ssb_am " (frequency *clm-default-frequency*) (order 40)): \
return a new " S_ssb_am " generator."
  #define MUS_MAX_SSB_ORDER 65536

  mus_any *ge;
  Xen args[4]; 
  Xen keys[2];
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
      freq = Xen_optkey_to_float(kw_frequency, keys[0], S_make_ssb_am, orig_arg[0], freq);
      if (freq > (0.5 * mus_srate()))
	Xen_out_of_range_error(S_make_ssb_am, orig_arg[0], keys[0], "freq > srate/2?");

      order = Xen_optkey_to_int(kw_order, keys[1], S_make_ssb_am, orig_arg[1], order);
      if (order <= 0)
	Xen_out_of_range_error(S_make_ssb_am, orig_arg[1], keys[1], "order <= 0?");
      if (order > MUS_MAX_SSB_ORDER)
	Xen_out_of_range_error(S_make_ssb_am, orig_arg[1], keys[1], "order too large?");
    }

  ge = mus_make_ssb_am(freq, order);
  if (ge) return(mus_xen_to_object(mus_any_to_mus_xen(ge)));
  return(Xen_false);
}


static Xen g_ssb_am(Xen obj, Xen insig, Xen fm)
{
  #define H_ssb_am "(" S_ssb_am " gen (insig 0.0) (fm 0.0)): get the next sample from " S_ssb_am " generator"

  mus_float_t insig1 = 0.0;
  mus_any *g = NULL;
  mus_xen *gn;

  Xen_to_C_generator(obj, gn, g, mus_is_ssb_am, S_ssb_am, "an ssb-am generator");
  Xen_real_to_C_double_if_bound(insig, insig1, S_ssb_am, 2);

  if (Xen_is_bound(fm))
    {
      Xen_check_type(Xen_is_number(fm), fm, 3, S_ssb_am, "a number");
      return(C_double_to_Xen_real(mus_ssb_am(g, insig1, Xen_real_to_C_double(fm))));
    }
  return(C_double_to_Xen_real(mus_ssb_am_unmodulated(g, insig1)));
}


#define S_mus_frandom "mus-frandom"
#define S_mus_irandom "mus-irandom"

static Xen g_mus_frandom(Xen val) {return(C_double_to_Xen_real(mus_frandom(Xen_real_to_C_double_with_caller(val, S_mus_frandom))));}
static Xen g_mus_irandom(Xen val) {return(C_int_to_Xen_integer(mus_irandom(Xen_integer_to_C_int(val))));}


static Xen mus_clm_output(void);
static Xen mus_clm_reverb(void);



/* Xen out, Xen in, Xen ost, Xen olen, Xen ist, Xen mx, Xen envs */
static Xen g_mus_file_mix(Xen args)
{
  #define H_mus_file_mix "(" S_mus_file_mix " outfile infile (outloc 0) (framples) (inloc 0) matrix envs): \
mix infile into outfile starting at outloc in outfile and inloc in infile \
mixing 'framples' framples into 'outfile'.  framples defaults to the length of infile. If matrix, \
use it to scale the various channels; if envs (an array of envelope generators), use \
it in conjunction with matrix to scale/envelope all the various ins and outs. \
'outfile' can also be a " S_frample_to_file " generator, and 'infile' can be a " S_file_to_frample " generator."

  Xen arg, out, in;
  mus_any *outf = NULL, *inf = NULL;
  mus_float_t *matrix = NULL;
  mus_any ***envs1 = NULL;
  int i;
  mus_long_t ostart = 0, istart = 0, osamps = 0;
  int in_chans = 0, out_chans = 0, mx_chans = 0, in_size = 0;  /* mus_mix in clm.c assumes the envs array is large enough */
  const char *outfile = NULL, *infile = NULL;

  /* -------- setup output gen -------- */
  arg = args; 
  out = Xen_car(arg);
  Xen_check_type(Xen_is_string(out) || ((mus_is_xen(out)) && (mus_is_output(Xen_to_mus_any(out)))), 
		 out, 1, S_mus_file_mix, "a filename or a " S_frample_to_file " generator");
  if (Xen_is_string(out)) 
    {
      outfile = Xen_string_to_C_string(out);
      if (!mus_file_probe(outfile))
	Xen_error(NO_SUCH_FILE, Xen_list_2(C_string_to_Xen_string(S_mus_file_mix ": no such file, ~S"), out));
      out_chans = mus_sound_chans(outfile);
      if (out_chans <= 0)
	Xen_error(BAD_HEADER, Xen_list_2(C_string_to_Xen_string(S_mus_file_mix ": ~S output chans <= 0"), out));
    }
  else 
    {
      outf = Xen_to_mus_any(out);
      out_chans = mus_channels(outf);
    }

  /* -------- setup input gen -------- */
  arg = Xen_cdr(arg); 
  in = Xen_car(arg);
  Xen_check_type(Xen_is_string(in) || ((mus_is_xen(in)) && (mus_is_input(Xen_to_mus_any(in)))), 
		 in, 2, S_mus_file_mix, "a filename or a " S_file_to_frample " generator");
  if (Xen_is_string(in)) 
    {
      infile = Xen_string_to_C_string(in);
      if (!mus_file_probe(infile))
	Xen_error(NO_SUCH_FILE, Xen_list_2(C_string_to_Xen_string(S_mus_file_mix ": no such file, ~S"), in));
      in_chans = mus_sound_chans(infile);
      if (in_chans <= 0)
	Xen_error(BAD_HEADER, Xen_list_2(C_string_to_Xen_string(S_mus_file_mix ": ~S input chans <= 0"), in));
      osamps = mus_sound_framples(infile);
    }
  else 
    {
      inf = Xen_to_mus_any(in);
      in_chans = mus_channels(inf);
      osamps = mus_length(inf);
    }

  /* inf and outf only exist during the rest of the arglist scan if not infile or outfile.
   *    we need to delay making the inf/outf gens in this case to greatly simplify error handling.
   */

  /* rest of args are optional */
  arg = Xen_cdr(arg); 
  if (!Xen_is_null(arg))
    {
      Xen ost;
      ost = Xen_car(arg);
      Xen_check_type(Xen_is_integer(ost), ost, 3, S_mus_file_mix, "an integer");
      ostart = Xen_llong_to_C_llong(ost);

      arg = Xen_cdr(arg); 
      if (!Xen_is_null(arg))
	{
	  Xen olen;
	  olen = Xen_car(arg);
	  Xen_check_type(Xen_is_integer(olen), olen, 4, S_mus_file_mix, "an integer");
	  osamps = Xen_llong_to_C_llong(olen); 
	  if (osamps <= 0) return(Xen_false);
	  
	  arg = Xen_cdr(arg); 
	  if (!Xen_is_null(arg))
	    {
	      Xen ist;
	      ist = Xen_car(arg);
	      Xen_check_type(Xen_is_integer(ist), ist, 5, S_mus_file_mix, "an integer");
	      istart = Xen_llong_to_C_llong(ist);
	      
	      arg = Xen_cdr(arg); 
	      if (!Xen_is_null(arg))
		{
		  Xen mx;
		  mx = Xen_car(arg);
		  Xen_check_type((mus_is_vct(mx)) || (Xen_is_false(mx)), mx, 6, S_mus_file_mix, "a " S_vct);
		  if (mus_is_vct(mx))
		    {
		      matrix = mus_vct_data(Xen_to_vct(mx));
		      mx_chans = (int)sqrt(mus_vct_length(Xen_to_vct(mx)));
		    }

		  arg = Xen_cdr(arg); 
		  if (!Xen_is_null(arg))
		    {
		      Xen envs;
		      envs = Xen_car(arg);
		      Xen_check_type((Xen_is_false(envs)) || (Xen_is_vector(envs)), envs, 7, S_mus_file_mix, "a vector of envs");
		      if (Xen_is_vector(envs))
			{
			  int in_len = 0, out_len, j, out_size;
			  /* pack into a C-style array of arrays of env pointers */
			  in_len = Xen_vector_length(envs);
			  if (in_len == 0)
			    Xen_error(BAD_TYPE, Xen_list_2(C_string_to_Xen_string(S_mus_file_mix ": env vector, ~A, can't be empty"), envs));
			  
			  for (i = 0; i < in_len; i++)
			    {
			      Xen datum;
			      datum = Xen_vector_ref(envs, i);
			      if (!(Xen_is_vector(datum)))
				Xen_error(BAD_TYPE, Xen_list_2(C_string_to_Xen_string(S_mus_file_mix ": vector, ~A, must contain vectors of envelopes"), datum));
			    }
			  
			  out_len = Xen_vector_length(Xen_vector_ref(envs, 0));
			  if (in_len < in_chans) in_size = in_chans; else in_size = in_len;
			  if (out_len < out_chans) out_size = out_chans; else out_size = out_len;
			  
			  envs1 = (mus_any ***)malloc(in_size * sizeof(mus_any **));
			  for (i = 0; i < in_size; i++) 
			    envs1[i] = (mus_any **)calloc(out_size, sizeof(mus_any *));
			  
			  for (i = 0; i < in_len; i++)
			    for (j = 0; j < out_len; j++) 
			      {
				Xen datum1;
				datum1 = Xen_vector_ref(Xen_vector_ref(envs, i), j);
				if (mus_is_xen(datum1))
				  {
				    if (mus_is_env(Xen_to_mus_any(datum1)))
				      envs1[i][j] = Xen_to_mus_any(datum1);
				    else 
				      {
					for (i = 0; i < in_size; i++) if (envs1[i]) free(envs1[i]);
					free(envs1);
					Xen_error(BAD_TYPE, Xen_list_4(C_string_to_Xen_string(S_mus_file_mix ": vector, ~A at ~A ~A, must contain an envelope"), 
								       datum1,
								       C_int_to_Xen_integer(i),
								       C_int_to_Xen_integer(j)));
				      }
				  }
			    }
			}
		    }
		}
	    }
	}
    }

  if ((infile) && (outfile))
    mus_file_mix(outfile, infile, ostart, osamps, istart, matrix, mx_chans, envs1);
  else
    {
      if (infile)
	inf = mus_make_file_to_frample(infile);
      if (outfile)
	outf = mus_continue_sample_to_file(outfile);
      mus_file_mix_with_reader_and_writer(outf, inf, ostart, osamps, istart, matrix, mx_chans, envs1);
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
  return(Xen_true);
}


/* Xen file, Xen beg, Xen dur, Xen mx, Xen revmx, Xen envs, Xen srcs, Xen srcenv, Xen outstream, Xen revstream */

static Xen g_mus_file_mix_with_envs(Xen args)
{
  #define H_mus_file_mix_with_envs "(" S_mus_file_mix_with_envs " file beg dur mx revmx envs srcs srcenv out rev) is an extension of " S_mus_file_mix ", primarily \
intended to speed up the fullmix instrument.  file is a vector of readin generators.  beg is the sample at which to start mixing \
output, dur is the number of samples to write. mx is a matrix, revmx is either #f or a matrix. "

  int i, in_chans, out_chans, mx_chans = 0, rev_chans = 0, rev_mix_chans = 0;
  mus_long_t st, nd;
  mus_any *s_env = NULL, *ostr, *rstr = NULL;
  mus_any **mix_envs, **mix_srcs, **mix_rds;
  mus_xen *gn;
  Xen ve, arg, file, beg, dur, mx, revmx, envs, srcs, srcenv, outstream, revstream;
  mus_float_t *mix = NULL, *rev_mix = NULL;

  i = Xen_list_length(args);
  if ((i < 8) || (i > 10)) /* no wrong-number-of-args error in xen.h, so I'll use out-of-range */
    Xen_out_of_range_error(S_mus_file_mix_with_envs, 0, args, "wrong number of args");
    
  arg = args;
  file = Xen_car(arg);
  Xen_check_type(Xen_is_vector(file), file, 1, S_mus_file_mix_with_envs, "a vector of readin generators");
  in_chans = Xen_vector_length(file);
  
  arg = Xen_cdr(arg);
  beg = Xen_car(arg);
  Xen_check_type(Xen_is_integer(beg), beg, 2, S_mus_file_mix_with_envs, "an integer");
  st = Xen_integer_to_C_int(beg);

  arg = Xen_cdr(arg);
  dur = Xen_car(arg);
  Xen_check_type(Xen_is_integer(dur), dur, 3, S_mus_file_mix_with_envs, "an integer");
  nd = st + Xen_integer_to_C_int(dur);

  arg = Xen_cdr(arg);
  mx = Xen_car(arg);
  if (mus_is_vct(mx))
    {
      mix = mus_vct_data(Xen_to_vct(mx));
      mx_chans = (int)sqrt(mus_vct_length(Xen_to_vct(mx)));
    }

  arg = Xen_cdr(arg);
  revmx = Xen_car(arg);
  if (mus_is_vct(revmx))
    {
      rev_mix = mus_vct_data(Xen_to_vct(revmx));
      rev_mix_chans = (int)sqrt(mus_vct_length(Xen_to_vct(revmx)));
    }

  arg = Xen_cdr(arg);
  envs = Xen_car(arg);
  if (!Xen_is_false(envs))
    Xen_check_type(Xen_is_vector(envs), envs, 6, S_mus_file_mix_with_envs, "a vector of env generators");

  arg = Xen_cdr(arg);
  srcs = Xen_car(arg);
  if (!Xen_is_false(srcs))
    Xen_check_type(Xen_is_vector(srcs), srcs, 7, S_mus_file_mix_with_envs, "a vector of src generators");

  arg = Xen_cdr(arg);
  srcenv = Xen_car(arg);
  if (!Xen_is_false(srcenv))
    {
      gn = (mus_xen *)Xen_object_ref_checked(srcenv, mus_xen_tag);
      if (!gn) Xen_check_type(false, srcenv, 8, S_mus_file_mix_with_envs, "an env generator");
      s_env = gn->gen;
      Xen_check_type(mus_is_env(s_env), srcenv, 8, S_mus_file_mix_with_envs, "an env generator");
    }

  revstream = Xen_false;
  arg = Xen_cdr(arg);
  if (!Xen_is_null(arg))
    {
      outstream = Xen_car(arg);
      gn = (mus_xen *)Xen_object_ref_checked(outstream, mus_xen_tag);
      if (!gn)
	Xen_check_type(false, outstream, 9, S_mus_file_mix_with_envs, "an output generator");
      ostr = gn->gen;

      arg = Xen_cdr(arg);
      if (!Xen_is_null(arg))
	revstream = Xen_car(arg);
    }
  else ostr = Xen_to_mus_any(mus_clm_output());
  out_chans = mus_channels(ostr);

  if (rev_mix)
    {
      if (!Xen_is_false(revstream))
	{
	  gn = (mus_xen *)Xen_object_ref_checked(revstream, mus_xen_tag);
	  if (!gn)
	    Xen_check_type(false, revstream, 10, S_mus_file_mix_with_envs, "an output generator");
	  rstr = gn->gen;
	}
      else rstr = Xen_to_mus_any(mus_clm_reverb());
      rev_chans = mus_channels(rstr);
    }

  mix_rds = (mus_any **)calloc(in_chans, sizeof(mus_any *));
  mix_srcs = (mus_any **)calloc(in_chans, sizeof(mus_any *));

  for (i = 0; i < in_chans; i++)
    mix_rds[i] = Xen_to_mus_any(Xen_vector_ref(file, i));
    
  if (Xen_is_vector(srcs))
    {
      for (i = 0; i < in_chans; i++)
	{
	  ve = Xen_vector_ref(srcs, i);
	  if (!Xen_is_false(ve)) mix_srcs[i] = Xen_to_mus_any(ve);
	}
    }

  mix_envs = (mus_any **)calloc(in_chans * out_chans, sizeof(mus_any *));
  if (Xen_is_vector(envs))
    for (i = 0; i < in_chans * out_chans; i++)
      {
	ve = Xen_vector_ref(envs, i);
	if (!Xen_is_false(ve)) mix_envs[i] = Xen_to_mus_any(ve);
      }

  {
    mus_long_t samp;
    int outp;
    mus_float_t src_env_val = 0.0;
    mus_float_t *infs, *out_frample, *rev_frample = NULL;

    infs = (mus_float_t *)calloc(in_chans, sizeof(mus_float_t));
    out_frample = (mus_float_t *)calloc(out_chans, sizeof(mus_float_t));
    if (rev_mix) rev_frample = (mus_float_t *)calloc(rev_chans, sizeof(mus_float_t));

    if (in_chans == 1)
      {
	mus_any *s = NULL, *r = NULL;
	s = mix_srcs[0];
	if (!s) r = mix_rds[0];

	for (samp = st; samp < nd; samp++)
	  {
	    for (outp = 0; outp < out_chans; outp++)
	      {
		mus_any *e;
		e = mix_envs[outp];
		if (e)
		  mix[outp] = mus_env(e);
	      }
	    if (s_env)
	      src_env_val = mus_env(s_env);
	    if (s)
	      infs[0] = mus_src(s, src_env_val, NULL);
	    else 
	      {
		if (r) 
		  infs[0] = mus_readin(r);
		else infs[0] = 0.0;
	      }
	    mus_frample_to_file(ostr, samp, mus_frample_to_frample(mix, mx_chans, infs, in_chans, out_frample, out_chans));
	    if (rev_mix) mus_frample_to_file(rstr, samp, mus_frample_to_frample(rev_mix, rev_mix_chans, infs, in_chans, rev_frample, rev_chans));
	  }
      }
    else
      {
	for (samp = st; samp < nd; samp++)
	  {
	    int inp, off;
	    for (inp = 0, off = 0; inp < in_chans; inp++, off += mx_chans)
	      for (outp = 0; outp < out_chans; outp++)
		{
		  mus_any *e;
		  e = mix_envs[inp * out_chans + outp]; /* this is different from the matrix setup -- I don't know why */
		  if (e)
		    mix[off + outp] = mus_env(e);
		}
	    if (s_env)
	      src_env_val = mus_env(s_env);
	    for (inp = 0; inp < in_chans; inp++)
	      {
		mus_any *s;
		s = mix_srcs[inp];
		if (s)
		  infs[inp] = mus_src(s, src_env_val, NULL);
		else 
		  {
		    s = mix_rds[inp];
		    if (s) 
		      infs[inp] = mus_readin(s);
		    else infs[inp] = 0.0;
		  }
	      }
	    mus_frample_to_file(ostr, samp, mus_frample_to_frample(mix, mx_chans, infs, in_chans, out_frample, out_chans));
	    if (rev_mix) mus_frample_to_file(rstr, samp, mus_frample_to_frample(rev_mix, rev_mix_chans, infs, in_chans, rev_frample, rev_chans));
	  }
      }
    free(infs);
    free(out_frample);
    if (rev_frample) free(rev_frample);
  }

  free(mix_rds);
  free(mix_srcs);
  free(mix_envs);
  return(Xen_false);
}


static Xen g_frample_to_frample(Xen mx, Xen infr, Xen inchans, Xen outfr, Xen outchans)
{
  #define H_frample_to_frample "(" S_frample_to_frample " matrix in-data in-chans out-data out-chans): pass frample in-data through matrix \
returning frample out-data; this is a matrix multiply of matrix and in-data"
  int ins, outs, mxs;
  vct *vin, *vout, *vmx;

  Xen_check_type(mus_is_vct(mx), mx, 1, S_frample_to_frample, "a " S_vct);
  Xen_check_type(mus_is_vct(infr), infr, 2, S_frample_to_frample, "a " S_vct);
  Xen_check_type(mus_is_vct(outfr), outfr, 4, S_frample_to_frample, "a " S_vct);
  Xen_check_type(Xen_is_integer(inchans), inchans, 3, S_frample_to_frample, "an integer");
  Xen_check_type(Xen_is_integer(outchans), outchans, 5, S_frample_to_frample, "an integer");

  ins = Xen_integer_to_C_int(inchans);
  vin = Xen_to_vct(infr);
  if (mus_vct_length(vin) < ins) ins = mus_vct_length(vin);
  if (ins <= 0) return(outfr);

  outs = Xen_integer_to_C_int(outchans);
  vout = Xen_to_vct(outfr);
  if (mus_vct_length(vout) < outs) outs = mus_vct_length(vout);
  if (outs <= 0) return(outfr);

  vmx = Xen_to_vct(mx);
  mxs = (int)sqrt(mus_vct_length(vmx));

  mus_frample_to_frample(mus_vct_data(vmx), mxs, mus_vct_data(vin), ins, mus_vct_data(vout), outs);
  return(outfr);
}




#if HAVE_SCHEME
#ifndef _MSC_VER

#include <time.h>
#include <sys/time.h>

static struct timeval overall_start_time;
#define S_get_internal_real_time "get-internal-real-time"
#define S_internal_time_units_per_second "internal-time-units-per-second"

static Xen g_get_internal_real_time(void) 
{
  #define H_get_internal_real_time "(" S_get_internal_real_time ") returns the number of seconds since \
the program started.  The number is in terms of " S_internal_time_units_per_second ", usually 1"
  struct timezone z0;
  struct timeval t0;
  mus_float_t secs;
  gettimeofday(&t0, &z0);
  secs = difftime(t0.tv_sec, overall_start_time.tv_sec);
  return(C_double_to_Xen_real(secs + 0.000001 * (t0.tv_usec - overall_start_time.tv_usec)));
}
#else
static Xen g_get_internal_real_time(void) {return(C_double_to_Xen_real(0.0));}
#endif

Xen_wrap_no_args(g_get_internal_real_time_w, g_get_internal_real_time)
#endif




/* -------------------------------- scheme-side optimization -------------------------------- */

#if HAVE_SCHEME
/* many of these are not in xen.h */
#define car(E)    s7_car(E)
/* #define caar(E)   s7_caar(E) */
#define cdr(E)    s7_cdr(E)
#define cadr(E)   s7_cadr(E)
/* #define cadar(E)  s7_cadar(E) */
#define caddr(E)  s7_caddr(E)
#define cadddr(E) s7_cadddr(E)
#define cddr(E)   s7_cddr(E)
/* #define cdddr(E)  s7_cdddr(E) */
#define cdar(E)   s7_cdar(E)
#define cdaddr(E) s7_cdaddr(E)
#define cdadr(E)  s7_cdadr(E)
#define cadadr(E) s7_cadadr(E)
#define caadr(E)  s7_caadr(E)
#define cadddr(E) s7_cadddr(E)

static mus_float_t mus_nsin_unmodulated(mus_any *p) {return(mus_nsin(p, 0.0));}
static mus_float_t mus_ncos_unmodulated(mus_any *p) {return(mus_ncos(p, 0.0));}
static mus_float_t mus_nrxysin_unmodulated(mus_any *p) {return(mus_nrxysin(p, 0.0));}
static mus_float_t mus_nrxycos_unmodulated(mus_any *p) {return(mus_nrxycos(p, 0.0));}
static mus_float_t mus_rxyksin_unmodulated(mus_any *p) {return(mus_rxyksin(p, 0.0));}
static mus_float_t mus_rxykcos_unmodulated(mus_any *p) {return(mus_rxykcos(p, 0.0));}
static mus_float_t mus_square_wave_unmodulated(mus_any *p) {return(mus_square_wave(p, 0.0));}
static mus_float_t mus_sawtooth_wave_unmodulated(mus_any *p) {return(mus_sawtooth_wave(p, 0.0));}

static mus_float_t mus_src_simple(mus_any *p) {return(mus_src(p, 0.0, NULL));}
static mus_float_t mus_src_two(mus_any *p, mus_float_t x) {return(mus_src(p, x, NULL));}
static mus_float_t mus_granulate_simple(mus_any *p) {return(mus_granulate_with_editor(p, NULL, NULL));}
static mus_float_t mus_convolve_simple(mus_any *p) {return(mus_convolve(p, NULL));}
static mus_float_t mus_phase_vocoder_simple(mus_any *p) {return(mus_phase_vocoder(p, NULL));}


#define mus_oscil_rf mus_oscil_unmodulated
#define mus_polywave_rf mus_polywave_unmodulated
#define mus_ncos_rf mus_ncos_unmodulated
#define mus_nsin_rf mus_nsin_unmodulated
#define mus_nrxycos_rf mus_nrxycos_unmodulated
#define mus_nrxysin_rf mus_nrxysin_unmodulated
#define mus_rxykcos_rf mus_rxykcos_unmodulated
#define mus_rxyksin_rf mus_rxyksin_unmodulated
#define mus_rand_rf mus_rand_unmodulated
#define mus_rand_interp_rf mus_rand_interp_unmodulated
#define mus_readin_rf mus_readin
#define mus_env_rf mus_env
#define mus_pulsed_env_rf mus_pulsed_env_unmodulated
#define mus_oscil_bank_rf mus_oscil_bank
#define mus_table_lookup_rf mus_table_lookup_unmodulated
#define mus_sawtooth_wave_rf mus_sawtooth_wave_unmodulated
#define mus_pulse_train_rf mus_pulse_train_unmodulated
#define mus_triangle_wave_rf mus_triangle_wave_unmodulated
#define mus_square_wave_rf mus_square_wave_unmodulated
#define mus_wave_train_rf mus_wave_train_unmodulated

#define mus_convolve_rf mus_convolve_simple
#define mus_src_rf mus_src_simple
#define mus_granulate_rf mus_granulate_simple
#define mus_phase_vocoder_rf mus_phase_vocoder_simple

static mus_float_t mus_one_pole_rf(mus_any *p) {return(mus_one_pole(p, 0.0));}
static mus_float_t mus_two_pole_rf(mus_any *p) {return(mus_two_pole(p, 0.0));}
static mus_float_t mus_one_zero_rf(mus_any *p) {return(mus_one_zero(p, 0.0));}
static mus_float_t mus_two_zero_rf(mus_any *p) {return(mus_two_zero(p, 0.0));}
static mus_float_t mus_delay_rf(mus_any *p) {return(mus_delay_unmodulated(p, 0.0));}
static mus_float_t mus_comb_rf(mus_any *p) {return(mus_comb_unmodulated(p, 0.0));}
static mus_float_t mus_comb_bank_rf(mus_any *p) {return(mus_comb_bank(p, 0.0));}
static mus_float_t mus_all_pass_bank_rf(mus_any *p) {return(mus_all_pass_bank(p, 0.0));}
static mus_float_t mus_notch_rf(mus_any *p) {return(mus_notch_unmodulated(p, 0.0));}
static mus_float_t mus_all_pass_rf(mus_any *p) {return(mus_all_pass_unmodulated(p, 0.0));}
static mus_float_t mus_one_pole_all_pass_rf(mus_any *p) {return(mus_one_pole_all_pass(p, 0.0));}
static mus_float_t mus_moving_average_rf(mus_any *p) {return(mus_moving_average(p, 0.0));}
static mus_float_t mus_moving_max_rf(mus_any *p) {return(mus_moving_max(p, 0.0));}
static mus_float_t mus_moving_norm_rf(mus_any *p) {return(mus_moving_norm(p, 0.0));}
static mus_float_t mus_filter_rf(mus_any *p) {return(mus_filter(p, 0.0));}
static mus_float_t mus_fir_filter_rf(mus_any *p) {return(mus_fir_filter(p, 0.0));}
static mus_float_t mus_iir_filter_rf(mus_any *p) {return(mus_iir_filter(p, 0.0));}
static mus_float_t mus_polyshape_rf(mus_any *p) {return(mus_polyshape_unmodulated(p, 1.0));}
static mus_float_t mus_filtered_comb_rf(mus_any *p) {return(mus_filtered_comb_unmodulated(p, 0.0));}
static mus_float_t mus_filtered_comb_bank_rf(mus_any *p) {return(mus_filtered_comb_bank(p, 0.0));}
static mus_float_t mus_asymmetric_fm_rf(mus_any *p) {return(mus_asymmetric_fm_unmodulated(p, 0.0));}
static mus_float_t mus_formant_rf(mus_any *p) {return(mus_formant(p, 0.0));}
static mus_float_t mus_firmant_rf(mus_any *p) {return(mus_firmant(p, 0.0));}

static mus_float_t mus_ssb_am_rf_1(mus_any *p) {return(mus_ssb_am(p, 0.0, 0.0));}

static mus_any *cadr_gen(s7_scheme *sc, s7_pointer expr)
{
  s7_pointer sym, o;
  mus_xen *gn;

  sym = s7_cadr(expr);
  if (!s7_is_symbol(sym)) return(NULL);
  if (s7_xf_is_stepper(sc, sym)) return(NULL);
  o = s7_symbol_value(sc, sym);
  gn = (mus_xen *)s7_object_value_checked(o, mus_xen_tag);
  if (!gn) return(NULL);
  return(gn->gen);
}

static s7_rf_t caddr_rf(s7_scheme *sc, s7_pointer a2, s7_rf_t func)
{
  s7_int loc;
  s7_pointer val_sym, val;
  s7_rf_t rf; 
  s7_rp_t rp;

  val_sym = car(a2);
  if (!s7_is_symbol(val_sym)) return(NULL);
  val = s7_symbol_value(sc, val_sym);
  rp = s7_rf_function(sc, val); 
  if (!rp) return(NULL);
  loc = s7_xf_store(sc, NULL);
  rf = rp(sc, a2);
  if (!rf) return(NULL);
  s7_xf_store_at(sc, loc, (s7_pointer)rf);
  return(func);
}

#define GEN_RF_1(Type, Func)					\
  static s7_double Type ## _rf(s7_scheme *sc, s7_pointer **p)	\
  {								\
    mus_any *g; g = (mus_any *)(*(*p));	(*p)++;			\
    return(Func(g));						\
  }								\
  static s7_rf_t is_ ## Type ## _rf(s7_scheme *sc, s7_pointer expr) \
  {									\
    mus_any *g;								\
    if (!s7_is_null(sc, s7_cddr(expr))) return(NULL);			\
    g = cadr_gen(sc, expr);						\
    if ((g) && (mus_is_ ## Type(g))) {s7_xf_store(sc, (s7_pointer)g); return(Type ## _rf);} \
    return(NULL);							\
  }


#define GEN_RF(Type, Func1, Func2)				\
  static s7_double Type ## _rf(s7_scheme *sc, s7_pointer **p)	\
  {								\
    mus_any *g; g = (mus_any *)(*(*p));	(*p)++;			\
    return(Func1(g));						\
  }								\
  static s7_double Type ## _rf_r(s7_scheme *sc, s7_pointer **p)	\
  {								\
    s7_pointer a2;						\
    mus_any *g; g = (mus_any *)(*(*p));	(*p)++;			\
    a2 = (**p);	(*p)++;						\
    return(Func2(g, s7_number_to_real(sc, a2)));		\
  }								\
  static s7_double Type ## _rf_s(s7_scheme *sc, s7_pointer **p)	\
  {								\
    s7_double a2;						\
    s7_pointer slot;						\
    mus_any *g; g = (mus_any *)(*(*p));	(*p)++;			\
    slot = **p;							\
    a2 = s7_number_to_real(sc, s7_slot_value(slot)); (*p)++;	\
    return(Func2(g, a2));					\
  }								\
  static s7_double Type ## _rf_x(s7_scheme *sc, s7_pointer **p)	\
  {								\
    s7_rf_t f;							\
    mus_any *g; g = (mus_any *)(*(*p)); (*p)++;			\
    f = (s7_rf_t)(**p); (*p)++;				\
    return(Func2(g, f(sc, p)));					\
  }								\
  static s7_rf_t is_ ## Type ## _rf(s7_scheme *sc, s7_pointer expr) \
  {									\
    mus_any *g;								\
    g = cadr_gen(sc, expr);						\
    if ((g) && (mus_is_ ## Type(g)))					\
      {									\
        s7_pointer a2;							\
        s7_xf_store(sc, (s7_pointer)g);					\
        if (s7_is_null(sc, s7_cddr(expr))) return(Type ## _rf);		\
	if (!s7_is_null(sc, s7_cdddr(expr))) return(NULL);		\
        a2 = caddr(expr);						\
        if (s7_is_real(a2)) {s7_xf_store(sc, a2); return(Type ## _rf_r);} \
        if (s7_is_symbol(a2))						\
          {								\
	    s7_pointer slot;						\
	    slot = s7_slot(sc, a2);					\
            if (slot) {s7_xf_store(sc, (s7_pointer)slot); return(Type ## _rf_s);} \
	    return(NULL);						\
	  }								\
        if (s7_is_pair(a2))						\
          return(caddr_rf(sc, a2, Type ## _rf_x));			\
      }									\
    return(NULL);							\
  }

GEN_RF(all_pass, mus_all_pass_rf, mus_all_pass_unmodulated)
GEN_RF(asymmetric_fm, mus_asymmetric_fm_rf, mus_asymmetric_fm_unmodulated)
GEN_RF(comb, mus_comb_rf, mus_comb_unmodulated)
GEN_RF(comb_bank, mus_comb_bank_rf, mus_comb_bank)
GEN_RF(all_pass_bank, mus_all_pass_bank_rf, mus_all_pass_bank)
GEN_RF_1(convolve, mus_convolve_rf)
GEN_RF(delay, mus_delay_rf, mus_delay_unmodulated)
GEN_RF_1(env, mus_env_rf)
GEN_RF(filter, mus_filter_rf, mus_filter)
GEN_RF(filtered_comb, mus_filtered_comb_rf, mus_filtered_comb_unmodulated)
GEN_RF(filtered_comb_bank, mus_filtered_comb_bank_rf, mus_filtered_comb_bank)
GEN_RF(fir_filter, mus_fir_filter_rf, mus_fir_filter)
GEN_RF(firmant, mus_firmant_rf, mus_firmant)
GEN_RF(formant, mus_formant_rf, mus_formant)
GEN_RF_1(granulate, mus_granulate_rf)
GEN_RF(iir_filter, mus_iir_filter_rf, mus_iir_filter)
GEN_RF(moving_average, mus_moving_average_rf, mus_moving_average)
GEN_RF(moving_max, mus_moving_max_rf, mus_moving_max)
GEN_RF(moving_norm, mus_moving_norm_rf, mus_moving_norm)
GEN_RF(ncos, mus_ncos_rf, mus_ncos)
GEN_RF(notch, mus_notch_rf, mus_notch_unmodulated)
GEN_RF(nrxycos, mus_nrxycos_rf, mus_nrxycos)
GEN_RF(nrxysin, mus_nrxysin_rf, mus_nrxysin)
GEN_RF(nsin, mus_nsin_rf, mus_nsin)
GEN_RF(one_pole, mus_one_pole_rf, mus_one_pole)
GEN_RF(one_pole_all_pass, mus_one_pole_all_pass_rf, mus_one_pole_all_pass)
GEN_RF(one_zero, mus_one_zero_rf, mus_one_zero)
GEN_RF(oscil, mus_oscil_rf, mus_oscil_fm)
GEN_RF_1(oscil_bank, mus_oscil_bank_rf)
GEN_RF_1(phase_vocoder, mus_phase_vocoder_rf)
GEN_RF(polyshape, mus_polyshape_rf, mus_polyshape_unmodulated)
GEN_RF(polywave, mus_polywave_rf, mus_polywave)
GEN_RF(pulse_train, mus_pulse_train_rf, mus_pulse_train)
GEN_RF(pulsed_env, mus_pulsed_env_rf, mus_pulsed_env)
GEN_RF(rand, mus_rand_rf, mus_rand)
GEN_RF(rand_interp, mus_rand_interp_rf, mus_rand_interp)
GEN_RF_1(readin, mus_readin_rf)
GEN_RF(rxykcos, mus_rxykcos_rf, mus_rxykcos)
GEN_RF(rxyksin, mus_rxyksin_rf, mus_rxyksin)
GEN_RF(sawtooth_wave, mus_sawtooth_wave_rf, mus_sawtooth_wave)
GEN_RF(square_wave, mus_square_wave_rf, mus_square_wave)
GEN_RF(src, mus_src_rf, mus_src_two)
GEN_RF(table_lookup, mus_table_lookup_rf, mus_table_lookup)
GEN_RF(triangle_wave, mus_triangle_wave_rf, mus_triangle_wave)
GEN_RF(two_pole, mus_two_pole_rf, mus_two_pole)
GEN_RF(two_zero, mus_two_zero_rf, mus_two_zero)
GEN_RF(wave_train, mus_wave_train_rf, mus_wave_train)
GEN_RF(ssb_am, mus_ssb_am_rf_1, mus_ssb_am_unmodulated)
GEN_RF(tap, mus_tap_unmodulated, mus_tap)


static s7_double oscil_rf_sxx(s7_scheme *sc, s7_pointer **p)
{
  s7_rf_t rf1, rf2;
  s7_double v1;
  mus_any *g; g = (mus_any *)(*(*p)); (*p)++;
  rf1 = (s7_rf_t)(**p); (*p)++;
  v1 = rf1(sc, p);
  rf2 = (s7_rf_t)(**p); (*p)++;
  return(mus_oscil(g, v1, rf2(sc, p)));
}

static s7_double oscil_rf_ssx(s7_scheme *sc, s7_pointer **p)
{
  s7_rf_t rf1;
  s7_pointer s1;
  mus_any *g; g = (mus_any *)(*(*p)); (*p)++;
  s1 = (**p); (*p)++;
  rf1 = (s7_rf_t)(**p); (*p)++;
  return(mus_oscil(g, s7_number_to_real(sc, s7_slot_value(s1)), rf1(sc, p)));
}

static s7_double oscil_rf_sss(s7_scheme *sc, s7_pointer **p)
{
  s7_pointer s1, s2;
  mus_any *g; g = (mus_any *)(*(*p)); (*p)++;
  s1 = (**p); (*p)++;
  s2 = (**p); (*p)++;
  return(mus_oscil(g, s7_number_to_real(sc, s7_slot_value(s1)), s7_number_to_real(sc, s7_slot_value(s2))));
}

static s7_double oscil_rf_srs(s7_scheme *sc, s7_pointer **p)
{
  s7_pointer s1, s2;
  mus_any *g; g = (mus_any *)(*(*p)); (*p)++;
  s1 = (**p); (*p)++;
  s2 = (**p); (*p)++;
  return(mus_oscil(g, s7_number_to_real(sc, s1), s7_number_to_real(sc, s7_slot_value(s2))));
}

static s7_double oscil_rf_srx(s7_scheme *sc, s7_pointer **p)
{
  s7_rf_t rf1;
  s7_pointer s1;
  mus_any *g; g = (mus_any *)(*(*p)); (*p)++;
  s1 = (**p); (*p)++;
  rf1 = (s7_rf_t)(**p); (*p)++;
  return(mus_oscil(g, s7_number_to_real(sc, s1), rf1(sc, p)));
}

static s7_rf_t is_oscil_rf_3(s7_scheme *sc, s7_pointer expr)
{
  mus_any *g;
  int len;
  len = s7_list_length(sc, expr);
  if (len < 4) return(is_oscil_rf(sc, expr));
  if (len > 5) return(NULL);
  g = cadr_gen(sc, expr);
  if ((!g) || (!mus_is_oscil(g))) return(NULL);
  s7_xf_store(sc, (s7_pointer)g);
  return(s7_is_rf_2(sc, cdr(expr), NULL, NULL, NULL, oscil_rf_srs, oscil_rf_sss, NULL, oscil_rf_srx, oscil_rf_ssx, oscil_rf_sxx));
}


static s7_double comb_rf_sxx(s7_scheme *sc, s7_pointer **p)
{
  s7_rf_t rf1, rf2;
  s7_double v1;
  mus_any *g; g = (mus_any *)(*(*p)); (*p)++;
  rf1 = (s7_rf_t)(**p); (*p)++;
  v1 = rf1(sc, p);
  rf2 = (s7_rf_t)(**p); (*p)++;
  return(mus_comb(g, v1, rf2(sc, p)));
}

static s7_double comb_rf_ssx(s7_scheme *sc, s7_pointer **p)
{
  s7_rf_t rf1;
  s7_pointer s1;
  mus_any *g; g = (mus_any *)(*(*p)); (*p)++;
  s1 = (**p); (*p)++;
  rf1 = (s7_rf_t)(**p); (*p)++;
  return(mus_comb(g, s7_number_to_real(sc, s7_slot_value(s1)), rf1(sc, p)));
}

static s7_double comb_rf_sss(s7_scheme *sc, s7_pointer **p)
{
  s7_pointer s1, s2;
  mus_any *g; g = (mus_any *)(*(*p)); (*p)++;
  s1 = (**p); (*p)++;
  s2 = (**p); (*p)++;
  return(mus_comb(g, s7_number_to_real(sc, s7_slot_value(s1)), s7_number_to_real(sc, s7_slot_value(s2))));
}

static s7_rf_t is_comb_rf_3(s7_scheme *sc, s7_pointer expr)
{
  mus_any *g;
  int len;
  len = s7_list_length(sc, expr);
  if (len < 4) return(is_comb_rf(sc, expr));
  if (len > 5) return(NULL);
  g = cadr_gen(sc, expr);
  if ((!g) || (!mus_is_comb(g))) return(NULL);
  s7_xf_store(sc, (s7_pointer)g);
  return(s7_is_rf_2(sc, cdr(expr), NULL, NULL, NULL, NULL, comb_rf_sss, NULL, NULL, comb_rf_ssx, comb_rf_sxx));
}


static s7_double formant_rf_ssx(s7_scheme *sc, s7_pointer **p)
{
  s7_rf_t rf1;
  s7_pointer s1;
  mus_any *g; g = (mus_any *)(*(*p)); (*p)++;
  s1 = (**p); (*p)++;
  rf1 = (s7_rf_t)(**p); (*p)++;
  return(mus_formant_with_frequency(g, s7_number_to_real(sc, s7_slot_value(s1)), rf1(sc, p)));
}

static s7_double formant_rf_sss(s7_scheme *sc, s7_pointer **p)
{
  s7_pointer s1, s2;
  mus_any *g; g = (mus_any *)(*(*p)); (*p)++;
  s1 = (**p); (*p)++;
  s2 = (**p); (*p)++;
  return(mus_formant_with_frequency(g, s7_number_to_real(sc, s7_slot_value(s1)), s7_number_to_real(sc, s7_slot_value(s2))));
}

static s7_rf_t is_formant_rf_3(s7_scheme *sc, s7_pointer expr)
{
  mus_any *g;
  int len;
  len = s7_list_length(sc, expr);
  if (len < 4) return(is_formant_rf(sc, expr));
  if (len > 5) return(NULL);
  g = cadr_gen(sc, expr);
  if ((!g) || (!mus_is_formant(g))) return(NULL);
  s7_xf_store(sc, (s7_pointer)g);
  return(s7_is_rf_2(sc, cdr(expr), NULL, NULL, NULL, NULL, formant_rf_sss, NULL, NULL, formant_rf_ssx, NULL));
}


  /* formant-bank: c g r, or v for with_inputs */
static s7_double formant_bank_rf_s(s7_scheme *sc, s7_pointer **p)
{								
  mus_any *bank;
  s7_pointer slot;
  bank = (mus_any *)(**p); (*p)++;
  slot = (**p); (*p)++;
  return(mus_formant_bank(bank, s7_number_to_real(sc, s7_slot_value(slot))));
}	
							
static s7_double formant_bank_rf_r(s7_scheme *sc, s7_pointer **p)
{								
  mus_any *bank;
  s7_pointer slot;
  bank = (mus_any *)(**p); (*p)++;
  slot = (**p); (*p)++;
  return(mus_formant_bank(bank, s7_number_to_real(sc, slot)));
}	
							
static s7_double formant_bank_rf_x(s7_scheme *sc, s7_pointer **p)
{								
  mus_any *bank;
  s7_rf_t r1;
  bank = (mus_any *)(**p); (*p)++;
  r1 = (s7_rf_t)(**p); (*p)++;
  return(mus_formant_bank(bank, r1(sc, p)));
}								

static s7_double formant_bank_rf_v(s7_scheme *sc, s7_pointer **p)
{								
  mus_any *bank;
  s7_double *els;
  bank = (mus_any *)(**p); (*p)++;
  els = (s7_double *)(**p); (*p)++;
  return(mus_formant_bank_with_inputs(bank, els));
}	
							
static s7_rf_t is_formant_bank_rf(s7_scheme *sc, s7_pointer expr)
{									
  mus_any *g;					
  if (!s7_is_null(sc, s7_cdddr(expr))) return(NULL);
  g = cadr_gen(sc, expr);						
  if ((g) && (mus_is_formant_bank(g)))
    {
      s7_pointer a1, val_sym, val;
      s7_int loc;
      s7_rf_t rf;

      s7_xf_store(sc, (s7_pointer)g);
      a1 = caddr(expr);
      if (s7_is_symbol(a1))
	{
	  s7_pointer slot;
	  slot = s7_slot(sc, a1);
	  if (s7_is_real(s7_slot_value(slot)))
	    {
	      s7_xf_store(sc, (s7_pointer)s7_slot(sc, a1));
	      return(formant_bank_rf_s);
	    }
	  if (s7_is_float_vector(s7_slot_value(slot)))
	    {
	      s7_xf_store(sc, (s7_pointer)s7_float_vector_elements(s7_slot_value(s7_slot(sc, a1))));
	      return(formant_bank_rf_v);
	    }
	  return(NULL);
	}
      if (s7_is_real(a1))
	{
	  s7_xf_store(sc, (s7_pointer)s7_slot(sc, a1));
	  return(formant_bank_rf_r);
	}
      if (!s7_is_pair(a1)) return(NULL);
      val_sym = car(a1);
      if (!s7_is_symbol(val_sym)) return(NULL);
      val = s7_symbol_value(sc, val_sym);
      if (!s7_rf_function(sc, val)) return(NULL);
      loc = s7_xf_store(sc, NULL);
      rf = s7_rf_function(sc, val)(sc, a1);
      if (!rf) return(NULL);
      s7_xf_store_at(sc, loc, (s7_pointer)rf);
      return(formant_bank_rf_x);
    }
  return(NULL);
}


static s7_double outa_rf(s7_scheme *sc, s7_pointer **p)
{
  s7_pointer ind_slot;
  s7_double val;
  s7_rf_t rf;

  ind_slot = **p; (*p)++;
  rf = (s7_rf_t)(**p); (*p)++;
  val = rf(sc, p);
  out_any_2(s7_integer(s7_slot_value(ind_slot)), val, 0, S_outa); 
  return(val);
}

static s7_double outa_rf_to_mus_xen(s7_scheme *sc, s7_pointer **p)
{
  s7_pointer ind_slot;
  s7_double val;
  s7_int pos;
  s7_rf_t rf;

  ind_slot = s7_slot_value(**p); (*p)++;
  rf = (s7_rf_t)(**p); (*p)++;
  val = rf(sc, p);
  pos = s7_integer(ind_slot);
  if (!mus_simple_out_any_to_file(pos, val, 0, clm_output_gen))
    mus_safe_out_any_to_file(pos, val, 0, clm_output_gen);
  return(val);
}

static s7_rf_t is_outa_rf(s7_scheme *sc, s7_pointer expr)
{
  s7_pointer ind_sym, ind, ind_slot, val_sym, val, val_expr;
  s7_int loc;
  s7_rf_t rf;
  
  if (!s7_is_null(sc, s7_cdddr(expr))) return(NULL);
  ind_sym = s7_cadr(expr);
  if (!s7_is_symbol(ind_sym)) return(NULL);
  ind_slot = s7_slot(sc, ind_sym);
  if (!ind_slot) return(NULL);
  ind = s7_slot_value(ind_slot);
  if (!s7_is_integer(ind)) return(NULL);
  if (ind < 0) return(NULL);
  s7_xf_store(sc, ind_slot);

  val_expr = s7_caddr(expr);
  if (!s7_is_pair(val_expr)) return(NULL);
  val_sym = car(val_expr);
  if (!s7_is_symbol(val_sym)) return(NULL);
  val = s7_symbol_value(sc, val_sym);
  if (!s7_rf_function(sc, val)) return(NULL);
  loc = s7_xf_store(sc, NULL);
  rf = s7_rf_function(sc, val)(sc, val_expr);
  if (!rf) return(NULL);
  s7_xf_store_at(sc, loc, (s7_pointer)rf);

  if (out_any_2 == safe_out_any_2_to_mus_xen)
    return(outa_rf_to_mus_xen);
  return(outa_rf);
}


static s7_double sample_to_file_rf(s7_scheme *sc, s7_pointer **p)
{
  /* (sample->file obj samp chan[always int] val) */
  s7_pointer ind_slot, chan;
  mus_any *lc;
  s7_double val;
  s7_rf_t rf;

  lc = (mus_any *)(**p); (*p)++;
  ind_slot = s7_slot_value(**p); (*p)++;
  chan = (**p); (*p)++;
  rf = (s7_rf_t)(**p); (*p)++;
  val = rf(sc, p);
  mus_sample_to_file(lc, s7_integer(ind_slot), s7_integer(chan), val);
  return(val);
}

static s7_rf_t is_sample_to_file_rf(s7_scheme *sc, s7_pointer expr)
{
  s7_pointer ind_sym, ind, ind_slot, chan, val_sym, val, val_expr;
  s7_int loc;
  s7_rf_t rf;
  mus_any *lc;

  lc = cadr_gen(sc, expr);
  if ((!lc) || (!mus_is_sample_to_file(lc))) return(NULL);

  ind_sym = s7_caddr(expr);
  if (!s7_is_symbol(ind_sym)) return(NULL);
  ind_slot = s7_slot(sc, ind_sym);
  if (!ind_slot) return(NULL);
  ind = s7_slot_value(ind_slot);
  if (!s7_is_integer(ind)) return(NULL);

  chan = s7_cadddr(expr);
  if (!s7_is_integer(chan)) return(NULL);

  val_expr = s7_car(s7_cddddr(expr));
  if (!s7_is_pair(val_expr)) return(NULL);
  val_sym = s7_car(val_expr);
  if (!s7_is_symbol(val_sym)) return(NULL);
  val = s7_symbol_value(sc, val_sym);
  if (!s7_rf_function(sc, val)) return(NULL);

  s7_xf_store(sc, (s7_pointer)lc);
  s7_xf_store(sc, ind_slot);
  s7_xf_store(sc, chan);
  loc = s7_xf_store(sc, NULL);
  rf = s7_rf_function(sc, val)(sc, val_expr);
  if (!rf) return(NULL);
  s7_xf_store_at(sc, loc, (s7_pointer)rf);

  return(sample_to_file_rf);
}


static s7_double locsig_rf(s7_scheme *sc, s7_pointer **p)
{
  s7_pointer ind_slot;
  mus_any *lc;
  s7_double val;
  s7_rf_t rf;

  lc = (mus_any *)(**p); (*p)++;
  ind_slot = s7_slot_value(**p); (*p)++;
  rf = (s7_rf_t)(**p); (*p)++;
  val = rf(sc, p);
  mus_locsig(lc, s7_integer(ind_slot), val);
  return(val);
}

static s7_rf_t is_locsig_rf(s7_scheme *sc, s7_pointer expr)
{
  s7_pointer ind_sym, ind, ind_slot, val_sym, val, val_expr;
  s7_int loc;
  s7_rf_t rf;
  mus_any *lc;

  lc = cadr_gen(sc, expr);
  if ((!lc) || (!mus_is_locsig(lc))) return(NULL);

  ind_sym = s7_caddr(expr);
  if (!s7_is_symbol(ind_sym)) return(NULL);
  ind_slot = s7_slot(sc, ind_sym);
  if (!ind_slot) return(NULL);
  ind = s7_slot_value(ind_slot);
  if (!s7_is_integer(ind)) return(NULL);

  val_expr = s7_cadddr(expr);
  if (!s7_is_pair(val_expr)) return(NULL);
  val_sym = s7_car(val_expr);
  if (!s7_is_symbol(val_sym)) return(NULL);
  val = s7_symbol_value(sc, val_sym);
  if (!s7_rf_function(sc, val)) return(NULL);

  s7_xf_store(sc, (s7_pointer)lc);
  s7_xf_store(sc, ind_slot);
  loc = s7_xf_store(sc, NULL);
  rf = s7_rf_function(sc, val)(sc, val_expr);
  if (!rf) return(NULL);
  s7_xf_store_at(sc, loc, (s7_pointer)rf);

  return(locsig_rf);
}


static s7_double move_sound_rf(s7_scheme *sc, s7_pointer **p)
{
  s7_pointer ind_slot;
  mus_any *lc;
  s7_double val;
  s7_rf_t rf;

  lc = (mus_any *)(**p); (*p)++;
  ind_slot = s7_slot_value(**p); (*p)++;
  rf = (s7_rf_t)(**p); (*p)++;
  val = rf(sc, p);
  mus_move_sound(lc, s7_integer(ind_slot), val);
  return(val);
}

static s7_rf_t is_move_sound_rf(s7_scheme *sc, s7_pointer expr)
{
  s7_pointer ind_sym, ind, ind_slot, val_sym, val, val_expr;
  s7_int loc;
  s7_rf_t rf;
  mus_any *lc;

  lc = cadr_gen(sc, expr);
  if ((!lc) || (!mus_is_move_sound(lc))) return(NULL);

  ind_sym = s7_caddr(expr);
  if (!s7_is_symbol(ind_sym)) return(NULL);
  ind_slot = s7_slot(sc, ind_sym);
  if (!ind_slot) return(NULL);
  ind = s7_slot_value(ind_slot);
  if (!s7_is_integer(ind)) return(NULL);

  val_expr = s7_cadddr(expr);
  if (!s7_is_pair(val_expr)) return(NULL);
  val_sym = s7_car(val_expr);
  if (!s7_is_symbol(val_sym)) return(NULL);
  val = s7_symbol_value(sc, val_sym);
  if (!s7_rf_function(sc, val)) return(NULL);

  s7_xf_store(sc, (s7_pointer)lc);
  s7_xf_store(sc, ind_slot);
  loc = s7_xf_store(sc, NULL);
  rf = s7_rf_function(sc, val)(sc, val_expr);
  if (!rf) return(NULL);
  s7_xf_store_at(sc, loc, (s7_pointer)rf);

  return(move_sound_rf);
}


static s7_double out_bank_rf_1(s7_scheme *sc, s7_pointer **p)
{
  s7_pointer ind_slot;
  s7_double val;
  s7_rf_t rf;
  s7_int loc;
  mus_any *g1;

  g1 = (mus_any *)(**p); (*p)++;
  ind_slot = (**p); (*p)++;
  loc = s7_integer(s7_slot_value(ind_slot));
  rf = (s7_rf_t)(**p); (*p)++;
  val = rf(sc, p);
  if (mus_is_delay(g1))
    out_any_2(loc, mus_delay_unmodulated_noz(g1, val), 0, "out-bank");
  else out_any_2(loc, mus_all_pass_unmodulated_noz(g1, val), 0, "out-bank");
  return(val);
}

static s7_double out_bank_rf_2(s7_scheme *sc, s7_pointer **p)
{
  s7_pointer ind_slot;
  s7_double val;
  s7_rf_t rf;
  s7_int loc;
  mus_any *g1, *g2;

  g1 = (mus_any *)(**p); (*p)++;
  g2 = (mus_any *)(**p); (*p)++;
  ind_slot = (**p); (*p)++;
  loc = s7_integer(s7_slot_value(ind_slot));
  rf = (s7_rf_t)(**p); (*p)++;
  val = rf(sc, p);
  if (mus_is_delay(g1))
    {
      out_any_2(loc, mus_delay_unmodulated_noz(g1, val), 0, "out-bank");
      out_any_2(loc, mus_delay_unmodulated_noz(g2, val), 1, "out-bank");
    }
  else
    {
      out_any_2(loc, mus_all_pass_unmodulated_noz(g1, val), 0, "out-bank");
      out_any_2(loc, mus_all_pass_unmodulated_noz(g2, val), 1, "out-bank");
    }
  return(val);
}

static s7_double out_bank_rf_4(s7_scheme *sc, s7_pointer **p)
{
  s7_pointer ind_slot;
  s7_double val;
  s7_rf_t rf;
  s7_int loc;
  mus_any *g1, *g2, *g3, *g4;

  g1 = (mus_any *)(**p); (*p)++;
  g2 = (mus_any *)(**p); (*p)++;
  g3 = (mus_any *)(**p); (*p)++;
  g4 = (mus_any *)(**p); (*p)++;
  ind_slot = (**p); (*p)++;
  loc = s7_integer(s7_slot_value(ind_slot));
  rf = (s7_rf_t)(**p); (*p)++;
  val = rf(sc, p);
  if (mus_is_delay(g1))
    {
      out_any_2(loc, mus_delay_unmodulated_noz(g1, val), 0, "out-bank");
      out_any_2(loc, mus_delay_unmodulated_noz(g2, val), 1, "out-bank");
      out_any_2(loc, mus_delay_unmodulated_noz(g3, val), 2, "out-bank");
      out_any_2(loc, mus_delay_unmodulated_noz(g4, val), 3, "out-bank");
    }
  else
    {
      out_any_2(loc, mus_all_pass_unmodulated_noz(g1, val), 0, "out-bank");
      out_any_2(loc, mus_all_pass_unmodulated_noz(g2, val), 1, "out-bank");
      out_any_2(loc, mus_all_pass_unmodulated_noz(g3, val), 2, "out-bank");
      out_any_2(loc, mus_all_pass_unmodulated_noz(g4, val), 3, "out-bank");
    }
  return(val);
}

static s7_rf_t is_out_bank_rf(s7_scheme *sc, s7_pointer expr)
{
  s7_pointer ind_sym, ind, ind_slot, val_sym, val, val_expr, filts;
  s7_int loc;
  s7_rf_t rf;
  s7_int i, len;
  mus_xen *gn;
  mus_any *g;
  s7_pointer *els;

  filts = s7_cadr(expr);
  if (!s7_is_symbol(filts)) return(NULL);
  filts = s7_symbol_value(sc, filts);
  if (!s7_is_vector(filts)) return(NULL);
  len = s7_vector_length(filts);
  if ((len != 1) && (len != 2) && (len != 4)) return(NULL);
  els = s7_vector_elements(filts);
  gn = (mus_xen *)s7_object_value_checked(els[0], mus_xen_tag);
  if (!gn) return(NULL);
  g = gn->gen;
  if ((!mus_is_delay(g)) && (!mus_is_all_pass(g))) return(NULL);
  for (i = 0; i < len; i++)
    s7_xf_store(sc, (s7_pointer)((mus_xen *)s7_object_value(els[i]))->gen);

  ind_sym = s7_caddr(expr);
  if (!s7_is_symbol(ind_sym)) return(NULL);
  ind_slot = s7_slot(sc, ind_sym);
  if (!ind_slot) return(NULL);
  ind = s7_slot_value(ind_slot);
  if (!s7_is_integer(ind)) return(NULL);
  s7_xf_store(sc, ind_slot);

  val_expr = s7_cadddr(expr);
  if (!s7_is_pair(val_expr)) return(NULL);
  val_sym = s7_car(val_expr);
  if (!s7_is_symbol(val_sym)) return(NULL);
  val = s7_symbol_value(sc, val_sym);
  if (!s7_rf_function(sc, val)) return(NULL);

  loc = s7_xf_store(sc, NULL);
  rf = s7_rf_function(sc, val)(sc, val_expr);
  if (!rf) return(NULL);
  s7_xf_store_at(sc, loc, (s7_pointer)rf);

  if (len == 1) return(out_bank_rf_1);
  if (len == 2) return(out_bank_rf_2);
  return(out_bank_rf_4);
}


static s7_double file_to_sample_rf_ss(s7_scheme *sc, s7_pointer **p)
{
  s7_pointer ind;
  mus_any *stream;
  stream = (mus_any *)(**p); (*p)++;
  ind = s7_slot_value(**p); (*p)++;
  return(mus_file_to_sample(stream, s7_integer(ind), 0));
}

static s7_rf_t is_file_to_sample_rf(s7_scheme *sc, s7_pointer expr)
{
  s7_pointer ind_sym, ind_slot, ind, sym, o;
  mus_xen *gn;
  
  sym = s7_cadr(expr);
  if (!s7_is_symbol(sym)) return(NULL);
  o = s7_symbol_value(sc, sym);
  gn = (mus_xen *)s7_object_value_checked(o, mus_xen_tag);
  if (!gn) return(NULL);
  s7_xf_store(sc, (s7_pointer)(gn->gen));

  if (!s7_is_null(sc, s7_cdddr(expr))) return(NULL);
  ind_sym = s7_caddr(expr);
  if (!s7_is_symbol(ind_sym)) return(NULL);
  ind_slot = s7_slot(sc, ind_sym);
  if (!ind_slot) return(NULL);
  ind = s7_slot_value(ind_slot);
  if (!s7_is_integer(ind)) return(NULL);
  s7_xf_store(sc, ind_slot);

  return(file_to_sample_rf_ss);
}


static s7_double ina_rf_ss(s7_scheme *sc, s7_pointer **p)
{
  s7_pointer ind_slot;
  mus_any *stream;
  ind_slot = **p; (*p)++;
  stream = (mus_any *)(**p); (*p)++;
  return(mus_in_any(s7_integer(s7_slot_value(ind_slot)), 0, stream));
}

static s7_double ina_rf_fv(s7_scheme *sc, s7_pointer **p)
{
  s7_pointer ind_slot, fv;
  s7_int index;
  ind_slot = **p; (*p)++;
  index = s7_integer(s7_slot_value(ind_slot));
  fv = (**p); (*p)++;
  if ((index >= 0) && (index < s7_vector_length(fv)))
    return(s7_float_vector_elements(fv)[index]);
  return(0.0);
}

static s7_rf_t is_ina_rf(s7_scheme *sc, s7_pointer expr)
{
  s7_pointer ind_sym, ind_slot, ind, sym, o;
  mus_xen *gn;
  
  if (!s7_is_null(sc, s7_cdddr(expr))) return(NULL);
  ind_sym = s7_cadr(expr);
  if (!s7_is_symbol(ind_sym)) return(NULL);
  ind_slot = s7_slot(sc, ind_sym);
  if (!ind_slot) return(NULL);
  ind = s7_slot_value(ind_slot);
  if (!s7_is_integer(ind)) return(NULL);
  s7_xf_store(sc, ind_slot);

  sym = s7_caddr(expr);
  if (!s7_is_symbol(sym)) return(NULL);
  o = s7_symbol_value(sc, sym);
  if (s7_is_float_vector(o))
    {
      s7_xf_store(sc, o);
      return(ina_rf_fv);
    }
  gn = (mus_xen *)s7_object_value_checked(o, mus_xen_tag);
  if (!gn) return(NULL);
  s7_xf_store(sc, (s7_pointer)(gn->gen));
  return(ina_rf_ss);
}


#define RS_0(Call) \
  static s7_double Call ## _rf(s7_scheme *sc, s7_pointer **p) \
  {								\
    return(mus_ ## Call());					\
  }								\
  static s7_rf_t is_ ## Call ## _rf(s7_scheme *sc, s7_pointer expr)	\
  {									\
    if (!s7_is_null(sc, s7_cdr(expr))) return(NULL);			\
    return(Call ## _rf);						\
  }

RS_0(srate)


#define RS_1(Call)						\
  static s7_double Call ## _rf_s(s7_scheme *sc, s7_pointer **p) \
  {								\
    s7_pointer slot;						\
    slot = (**p); (*p)++;					\
    return(mus_ ## Call(s7_number_to_real(sc, s7_slot_value(slot))));	\
  }								\
  static s7_double Call ## _rf_c(s7_scheme *sc, s7_pointer **p) \
  {								\
    s7_pointer slot;						\
    slot = (**p); (*p)++;					\
    return(mus_ ## Call(s7_number_to_real(sc, slot)));			\
    }								\
  static s7_double Call ## _rf_r(s7_scheme *sc, s7_pointer **p) \
  {								\
    s7_rf_t r;							\
    r = (s7_rf_t)(**p); (*p)++;					\
    return(mus_ ## Call(r(sc, p)));					\
  }								\
  static s7_rf_t is_ ## Call ## _rf(s7_scheme *sc, s7_pointer expr)	\
  {									\
    return(s7_is_rf_1(sc, expr, Call ## _rf_c, Call ## _rf_s, Call ## _rf_r)); \
  }

RS_1(odd_weight)
RS_1(even_weight)
RS_1(hz_to_radians)
RS_1(radians_to_hz)
RS_1(random)

#define mus_sin sin
#define mus_cos cos
RS_1(sin)
RS_1(cos)

static s7_double contrast_enhancement_sx_rf(s7_scheme *sc, s7_pointer **p)
{
  s7_pointer s1;
  s7_rf_t rf;
  s7_double x;
  s1 = s7_slot_value(**p); (*p)++;
  rf = (s7_rf_t)(**p); (*p)++;
  x = rf(sc, p);
  return(mus_contrast_enhancement(s7_number_to_real(sc, s1), x));
}

static s7_double contrast_enhancement_xs_rf(s7_scheme *sc, s7_pointer **p)
{
  s7_pointer s1;
  s7_rf_t rf;
  s7_double x;
  rf = (s7_rf_t)(**p); (*p)++;
  x = rf(sc, p);
  s1 = s7_slot_value(**p); (*p)++;
  return(mus_contrast_enhancement(x, s7_number_to_real(sc, s1)));
}

static s7_rf_t is_contrast_enhancement_rf(s7_scheme *sc, s7_pointer expr)
{
  return(s7_is_rf_2(sc, expr, NULL, NULL, NULL, NULL, NULL, contrast_enhancement_xs_rf, NULL, contrast_enhancement_sx_rf, NULL));
}


static s7_double odd_multiple_ss_rf(s7_scheme *sc, s7_pointer **p)
{
  s7_pointer s1, s2;
  s1 = (**p); (*p)++;
  s2 = (**p); (*p)++;
  return(mus_odd_multiple(s7_number_to_real(sc, s7_slot_value(s1)), s7_number_to_real(sc, s7_slot_value(s2))));
}

static s7_double even_multiple_ss_rf(s7_scheme *sc, s7_pointer **p)
{
  s7_pointer s1, s2;
  s1 = (**p); (*p)++;
  s2 = (**p); (*p)++;
  return(mus_even_multiple(s7_number_to_real(sc, s7_slot_value(s1)), s7_number_to_real(sc, s7_slot_value(s2))));
}

static s7_rf_t is_odd_multiple_rf(s7_scheme *sc, s7_pointer expr)
{
  return(s7_is_rf_2(sc, expr, NULL, NULL, NULL, NULL, odd_multiple_ss_rf, NULL, NULL, NULL, NULL));
}

static s7_rf_t is_even_multiple_rf(s7_scheme *sc, s7_pointer expr)
{
  return(s7_is_rf_2(sc, expr, NULL, NULL, NULL, NULL, even_multiple_ss_rf, NULL, NULL, NULL, NULL));
}


static s7_double polynomial_ss_rf(s7_scheme *sc, s7_pointer **p)
{
  s7_pointer s1, s2;
  s1 = (**p); (*p)++;
  s1 = s7_slot_value(s1);
  s2 = (**p); (*p)++;
  s2 = s7_slot_value(s2);
  return(mus_polynomial(s7_float_vector_elements(s1), s7_number_to_real(sc, s2), s7_vector_length(s1)));
}

static s7_double polynomial_sx_rf(s7_scheme *sc, s7_pointer **p)
{
  s7_pointer s1;
  s7_rf_t r1;
  s1 = (**p); (*p)++;
  s1 = s7_slot_value(s1);
  r1 = (s7_rf_t)(**p); (*p)++;
  return(mus_polynomial(s7_float_vector_elements(s1), r1(sc, p), s7_vector_length(s1)));
}

static s7_rf_t is_polynomial_rf(s7_scheme *sc, s7_pointer expr)
{
  if ((s7_is_symbol(s7_cadr(expr))) &&
      (s7_is_float_vector(s7_symbol_value(sc, s7_cadr(expr)))))
    return(s7_is_rf_2(sc, expr, NULL, NULL, NULL, NULL, polynomial_ss_rf, NULL, NULL, polynomial_sx_rf, NULL));
  return(NULL);
}


static s7_double array_interp_sxr_rf(s7_scheme *sc, s7_pointer **p)
{
  s7_pointer s1, c2;
  s7_rf_t r1;
  s7_double x;
  s1 = s7_slot_value(**p); (*p)++;
  r1 = (s7_rf_t)(**p); (*p)++;
  x = r1(sc, p);
  c2 = **p; (*p)++;
  return(mus_array_interp(s7_float_vector_elements(s1), x, s7_integer(c2)));
}

static s7_rf_t is_array_interp_rf(s7_scheme *sc, s7_pointer expr)
{
  if (s7_is_symbol(s7_cadr(expr)))
    {
      s7_pointer rst, fv;
      rst = cdr(expr);
      fv = s7_slot(sc, s7_car(rst));
      if ((fv) && (s7_is_float_vector(s7_slot_value(fv))))
	{
	  if ((!s7_is_null(sc, s7_cddr(rst))) &&
	      (s7_is_null(sc, s7_cdddr(rst))))
	    {
	      s7_xf_store(sc, fv);
	      return(s7_is_rf_2(sc, rst, NULL, NULL, array_interp_sxr_rf, NULL, NULL, NULL, NULL, NULL, NULL));
	    }
	}
    }
  return(NULL);
}


static s7_double am_rsx_rf(s7_scheme *sc, s7_pointer **p)
{
  s7_pointer c1, s1;
  s7_rf_t r1;
  c1 = (**p); (*p)++;
  s1 = s7_slot_value(**p); (*p)++;
  r1 = (s7_rf_t)(**p); (*p)++;
  return(mus_amplitude_modulate(s7_number_to_real(sc, c1), s7_number_to_real(sc, s1), r1(sc, p)));
}

static s7_rf_t is_am_rf(s7_scheme *sc, s7_pointer expr)
{
  s7_pointer a1, a2, a3;
  a1 = s7_cadr(expr);
  a2 = s7_caddr(expr);
  a3 = s7_cadddr(expr);
  if ((s7_is_real(a1)) &&
      (s7_is_symbol(a2)) &&
      (s7_is_pair(a3)))
    {
      s7_rp_t rp;
      s7_rf_t rf;
      s7_int loc;
      s7_pointer sym, val;

      s7_xf_store(sc, a1);
      s7_xf_store(sc, s7_slot(sc, a2));

      sym = car(a3);
      if (!s7_is_symbol(sym)) return(NULL);
      val = s7_symbol_value(sc, sym);
      rp = s7_rf_function(sc, val); 
      if (!rp) return(NULL);
      loc = s7_xf_store(sc, NULL);
      rf = rp(sc, a3);
      if (!rf) return(NULL);
      s7_xf_store_at(sc, loc, (s7_pointer)rf);

      return(am_rsx_rf);
    }
  return(NULL);
}


static s7_double mul_env_x_rf(s7_scheme *sc, s7_pointer **p)
{
  s7_rf_t r2;
  mus_any *g; 
  (*p)++; r2 = (s7_rf_t)(**p); (*p)++;
  g = (mus_any *)(**p); (*p)++;
  return(mus_env(g) * r2(sc, p));
}

static s7_double mul_env_oscil_x_rf(s7_scheme *sc, s7_pointer **p)
{
  s7_rf_t r2;
  mus_any *e, *o; 
  (*p)++; (*p)++; /* skip env and oscil */
  e = (mus_any *)(**p); (*p)++;
  o = (mus_any *)(**p); (*p)++;
  r2 = (s7_rf_t)(**p); (*p)++;
  return(mus_env(e) * mus_oscil_fm(o, r2(sc, p)));
}

static s7_double mul_env_polywave_x_rf(s7_scheme *sc, s7_pointer **p)
{
  s7_rf_t r2;
  mus_any *e, *o; 
  (*p)++; (*p)++; /* skip env and oscil */
  e = (mus_any *)(**p); (*p)++;
  o = (mus_any *)(**p); (*p)++;
  r2 = (s7_rf_t)(**p); (*p)++;
  return(mus_env(e) * mus_polywave(o, r2(sc, p)));
}

static s7_rp_t initial_multiply_rf;
static s7_rf_t clm_multiply_rf(s7_scheme *sc, s7_pointer expr)
{
  s7_rf_t f;
  f = initial_multiply_rf(sc, expr);
  if (f)
    {
      s7_pointer a1, a2;
      a1 = s7_cadr(expr);
      a2 = s7_caddr(expr);
      if ((s7_is_pair(a1)) &&
	  (s7_car(a1) == env_symbol) &&
	  (s7_is_pair(a2)) &&
	  (s7_is_symbol(s7_cadr(a1))) &&
	  (s7_is_null(sc, s7_cdddr(expr))))
	{
	  if ((s7_is_symbol(s7_cadr(a2))) &&
	      (s7_is_pair(s7_caddr(a2))) &&
	      (s7_is_null(sc, s7_cdddr(a2))))
	    {
	      if (s7_car(a2) == oscil_symbol)
		return(mul_env_oscil_x_rf);
	      else
		{
		  if (s7_car(a2) == polywave_symbol)
		    return(mul_env_polywave_x_rf);
		}
	    }
	  return(mul_env_x_rf);
	}
    }
  return(f);
}


/* -------------------------------------------------------------------------------- */

#define s7_cell_integer s7_integer
#define s7_cell_real s7_real
#define GET_INTEGER(Obj, Caller, Val) Val = s7_integer(s7_car_value(s7, Obj))

#define GET_GENERATOR(Obj, Type, Val) \
  do { \
  s7_pointer gp; \
  mus_xen *gn; \
  gp = s7_car_value(s7, Obj); \
  gn = (mus_xen *)imported_s7_object_value_checked(gp, mus_xen_tag); \
  if (gn) \
    Val = gn->gen;							\
  else {Val = NULL; Xen_check_type(false, gp, 1, #Type " lookup", "a generator");} \
  } while (0)

#define GET_GENERATOR_CADR(Obj, Type, Val) \
  do { \
  s7_pointer gp; \
  mus_xen *gn; \
  gp = s7_cadr_value(s7, Obj); \
  gn = (mus_xen *)imported_s7_object_value_checked(gp, mus_xen_tag); \
  if (gn) \
    Val = gn->gen;							\
  else {Val = NULL; Xen_check_type(false, gp, 1, #Type " lookup", "a generator");} \
  } while (0)

#define GET_GENERATOR_CADAR(Obj, Type, Val) \
  do { \
  s7_pointer gp; \
  mus_xen *gn; \
  gp = s7_cadar_value(s7, Obj); \
  gn = (mus_xen *)imported_s7_object_value_checked(gp, mus_xen_tag); \
  if (gn) \
    Val = gn->gen;							\
  else {Val = NULL; Xen_check_type(false, gp, 1, #Type " lookup", "a generator");} \
  } while (0)


#define GET_NUMBER(Obj, Caller, Val) Val = s7_car_value(s7, Obj)
#define GET_REAL(Obj, Caller, Val) Val = s7_number_to_real_with_caller(sc, s7_car_value(s7, Obj), #Caller)
/* #define GET_NUMBER_CADR(Obj, Caller, Val) Val = s7_cadr_value(s7, Obj) */
#define GET_REAL_CADR(Obj, Caller, Val) Val = s7_number_to_real_with_caller(sc, s7_cadr_value(s7, Obj), #Caller)
#define GET_INTEGER_CADR(Obj, Caller, Val) Val = s7_cell_integer(s7_cadr_value(s7, Obj))

#define GEN_1(Type, Func) \
  static mus_float_t wrapped_ ## Type ## _1(mus_xen *p) {return(Func(p->gen));} \
  static s7_pointer Type ## _1; \
  static s7_pointer g_ ## Type ## _1(s7_scheme *sc, s7_pointer args) \
  { \
    mus_any *_o_;	  \
    GET_GENERATOR(args, Type, _o_); \
    return(s7_make_real(sc, Func(_o_))); \
  } \
  static s7_pointer env_ ## Type ## _1; \
  static s7_pointer g_env_ ## Type ## _1(s7_scheme *sc, s7_pointer args) \
  { \
    mus_any *_o_, *_e_;  \
   \
    GET_GENERATOR_CADAR(args , env, _e_);	\
    GET_GENERATOR_CADR(cadr(args), Type, _o_);	\
    return(s7_make_real(sc, mus_env(_e_) * Func(_o_)));		\
  }

static s7_pointer mul_s_oscil_1;
static s7_pointer g_mul_s_oscil_1(s7_scheme *sc, s7_pointer args)
{
  mus_any *_o_;
  s7_pointer _mul_;
  mus_float_t _f_;
  GET_NUMBER(args, "*", _mul_); 
  GET_GENERATOR_CADR(cadr(args), oscil, _o_);
  _f_ = mus_oscil_unmodulated(_o_);
  if (s7_is_real(_mul_))
    return(s7_make_real(sc, s7_number_to_real(sc, _mul_) * _f_));
  return(s7_make_complex(sc, s7_real_part(_mul_) * _f_, s7_imag_part(_mul_) * _f_));
} 

static s7_pointer mul_s_rand_interp_1;
static s7_pointer g_mul_s_rand_interp_1(s7_scheme *sc, s7_pointer args)
{
  mus_any *_o_;
  s7_pointer _mul_;
  mus_float_t _f_;
  GET_NUMBER(args, "*", _mul_); 
  GET_GENERATOR_CADR(cadr(args), oscil, _o_);
  _f_ = mus_rand_interp_unmodulated(_o_);
  if (s7_is_real(_mul_))
    return(s7_make_real(sc, s7_number_to_real(sc, _mul_) * _f_));
  return(s7_make_complex(sc, s7_real_part(_mul_) * _f_, s7_imag_part(_mul_) * _f_));
} 

static s7_pointer mul_s_env_1;
static s7_pointer g_mul_s_env_1(s7_scheme *sc, s7_pointer args)
{
  mus_any *_o_;
  s7_pointer _mul_;
  mus_float_t _f_;
  GET_NUMBER(args, "*", _mul_); 
  GET_GENERATOR_CADR(cadr(args), oscil, _o_);
  _f_ = mus_env(_o_);
  if (s7_is_real(_mul_))
    return(s7_make_real(sc, s7_number_to_real(sc, _mul_) * _f_));
  return(s7_make_complex(sc, s7_real_part(_mul_) * _f_, s7_imag_part(_mul_) * _f_));
} 

static s7_pointer mul_s_granulate_1;
static s7_pointer g_mul_s_granulate_1(s7_scheme *sc, s7_pointer args)
{
  mus_any *_o_;
  s7_pointer _mul_;
  mus_float_t _f_;
  GET_NUMBER(args, "*", _mul_); 
  GET_GENERATOR_CADR(cadr(args), oscil, _o_);
  _f_ = mus_granulate_simple(_o_);
  if (s7_is_real(_mul_))
    return(s7_make_real(sc, s7_number_to_real(sc, _mul_) * _f_));
  return(s7_make_complex(sc, s7_real_part(_mul_) * _f_, s7_imag_part(_mul_) * _f_));
} 

/* gen1's with no gen2 */
static bool wrapped_env_p(s7_pointer obj) {return((mus_is_xen(obj)) && (mus_is_env(Xen_to_mus_any(obj))));}
static bool wrapped_oscil_bank_p(s7_pointer obj) {return((mus_is_xen(obj)) && (mus_is_oscil_bank(Xen_to_mus_any(obj))));}
static bool wrapped_readin_p(s7_pointer obj) {return((mus_is_xen(obj)) && (mus_is_readin(Xen_to_mus_any(obj))));}
static bool wrapped_granulate_p(s7_pointer obj) {return((mus_is_xen(obj)) && (mus_is_granulate(Xen_to_mus_any(obj))));}
static bool wrapped_phase_vocoder_p(s7_pointer obj) {return((mus_is_xen(obj)) && (mus_is_phase_vocoder(Xen_to_mus_any(obj))));}
static bool wrapped_convolve_p(s7_pointer obj) {return((mus_is_xen(obj)) && (mus_is_convolve(Xen_to_mus_any(obj))));}
static bool wrapped_tap_p(s7_pointer obj) {return((mus_is_xen(obj)) && (mus_is_tap(Xen_to_mus_any(obj))));}

/* special formant-bank case */
static bool wrapped_formant_bank_p(s7_pointer obj) {return((mus_is_xen(obj)) && (mus_is_formant_bank(Xen_to_mus_any(obj))));}
static mus_float_t wrapped_formant_bank_2(mus_xen *p, mus_float_t x) {return(mus_formant_bank(p->gen, x));}

/* special polynomial experiment */
static bool wrapped_polynomial_p(s7_pointer obj) {return(mus_is_vct(obj));}
static mus_float_t wrapped_polynomial_2(vct *p, mus_float_t x) {return(mus_polynomial(mus_vct_data(p), x, mus_vct_length(p)));}

/* same for mus-random */
static bool wrapped_mus_random_p(s7_pointer obj) {return(s7_is_real(obj) && (!s7_is_rational(obj)));}
static mus_float_t wrapped_mus_random_1(s7_pointer x) {return(mus_random(s7_cell_real(x)));}

/* special pm oscil case */
static mus_float_t wrapped_oscil_3(mus_xen *p, mus_float_t x, mus_float_t y) {return(mus_oscil(p->gen, x, y));}
static mus_float_t wrapped_delay_3(mus_xen *p, mus_float_t x, mus_float_t y) {return(mus_delay(p->gen, x, y));}
static mus_float_t wrapped_comb_3(mus_xen *p, mus_float_t x, mus_float_t y) {return(mus_comb(p->gen, x, y));}
static mus_float_t wrapped_filtered_comb_3(mus_xen *p, mus_float_t x, mus_float_t y) {return(mus_filtered_comb(p->gen, x, y));}
static mus_float_t wrapped_notch_3(mus_xen *p, mus_float_t x, mus_float_t y) {return(mus_notch(p->gen, x, y));}
static mus_float_t wrapped_all_pass_3(mus_xen *p, mus_float_t x, mus_float_t y) {return(mus_all_pass(p->gen, x, y));}
static mus_float_t wrapped_asymmetric_fm_3(mus_xen *p, mus_float_t x, mus_float_t y) {return(mus_asymmetric_fm(p->gen, x, y));}
static mus_float_t wrapped_tap_1(mus_xen *p) {return(mus_tap_unmodulated(p->gen));}
static mus_float_t wrapped_formant_3(mus_xen *p, mus_float_t x, mus_float_t y) {return(mus_formant_with_frequency(p->gen, x, y));}
static mus_float_t wrapped_firmant_3(mus_xen *p, mus_float_t x, mus_float_t y) {return(mus_firmant_with_frequency(p->gen, x, y));}

static mus_float_t wrapped_ssb_am_3(mus_xen *p, mus_float_t x, mus_float_t y) {return(mus_ssb_am(p->gen, x, y));}
static mus_float_t wrapped_ssb_am_1(mus_xen *p) {return(mus_ssb_am_unmodulated(p->gen, 0.0));}

static mus_float_t wrapped_one_pole_1(mus_xen *p) {return(mus_one_pole(p->gen, 0.0));}
static mus_float_t wrapped_two_pole_1(mus_xen *p) {return(mus_two_pole(p->gen, 0.0));}
static mus_float_t wrapped_one_zero_1(mus_xen *p) {return(mus_one_zero(p->gen, 0.0));}
static mus_float_t wrapped_two_zero_1(mus_xen *p) {return(mus_two_zero(p->gen, 0.0));}
static mus_float_t wrapped_delay_1(mus_xen *p) {return(mus_delay_unmodulated(p->gen, 0.0));}
static mus_float_t wrapped_comb_1(mus_xen *p) {return(mus_comb_unmodulated(p->gen, 0.0));}
static mus_float_t wrapped_comb_bank_1(mus_xen *p) {return(mus_comb_bank(p->gen, 0.0));}
static mus_float_t wrapped_notch_1(mus_xen *p) {return(mus_notch_unmodulated(p->gen, 0.0));}
static mus_float_t wrapped_all_pass_1(mus_xen *p) {return(mus_all_pass_unmodulated(p->gen, 0.0));}
static mus_float_t wrapped_all_pass_bank_1(mus_xen *p) {return(mus_all_pass_bank(p->gen, 0.0));}
static mus_float_t wrapped_one_pole_all_pass_1(mus_xen *p) {return(mus_one_pole_all_pass(p->gen, 0.0));}
static mus_float_t wrapped_moving_average_1(mus_xen *p) {return(mus_moving_average(p->gen, 0.0));}
static mus_float_t wrapped_moving_max_1(mus_xen *p) {return(mus_moving_max(p->gen, 0.0));}
static mus_float_t wrapped_moving_norm_1(mus_xen *p) {return(mus_moving_norm(p->gen, 0.0));}
static mus_float_t wrapped_filter_1(mus_xen *p) {return(mus_filter(p->gen, 0.0));}
static mus_float_t wrapped_fir_filter_1(mus_xen *p) {return(mus_fir_filter(p->gen, 0.0));}
static mus_float_t wrapped_iir_filter_1(mus_xen *p) {return(mus_iir_filter(p->gen, 0.0));}
static mus_float_t wrapped_polyshape_1(mus_xen *p) {return(mus_polyshape_unmodulated(p->gen, 1.0));}
static mus_float_t wrapped_filtered_comb_1(mus_xen *p) {return(mus_filtered_comb_unmodulated(p->gen, 0.0));}
static mus_float_t wrapped_filtered_comb_bank_1(mus_xen *p) {return(mus_filtered_comb_bank(p->gen, 0.0));}
static mus_float_t wrapped_asymmetric_fm_1(mus_xen *p) {return(mus_asymmetric_fm_unmodulated(p->gen, 0.0));}
static mus_float_t wrapped_formant_1(mus_xen *p) {return(mus_formant(p->gen, 0.0));}
static mus_float_t wrapped_firmant_1(mus_xen *p) {return(mus_firmant(p->gen, 0.0));}

/* (define (hi) (let ((o (make-oscil 440.0)) (scl 0+i)) (oscil o) (* scl (oscil o))))
 * (define (hi) (let ((o (make-oscil 440.0)) (scl 0+i) (val 0.0)) (do ((i 0 (+ i 1))) ((= i 3)) (set! val (* scl (oscil o)))) val))
 * (define (hi) (let ((o (make-oscil 440.0)) (fm 1.0) (scl 0+i) (val 0.0)) (do ((i 0 (+ i 1))) ((= i 3)) (set! val (* scl (oscil o fm)))) val))
 * (define (hi) (let ((o (make-oscil 440.0)) (fm 0.0) (scl 1.0) (val 0.0)) (do ((i 0 (+ i 1))) ((= i 3)) (set! val (* scl (oscil o fm)))) val))
 * (define (hi) (let ((o (make-oscil 440.0)) (fm 1.0) (scl 0.0) (val 0.0)) (do ((i 0 (+ i 1))) ((= i 3)) (set! val (* scl (oscil o fm)))) val))
 * (define (hi) (let ((o (make-oscil 440.0)) (fm 1.0) (scl 1.0) (val 0.0)) (do ((i 0 (+ i 1))) ((= i 3)) (set! val (* scl (oscil o fm)))) val))
 */

#define GEN_2(Type, Func) \
  static mus_float_t wrapped_ ## Type ## _2(mus_xen *p, mus_float_t x) {return(Func(p->gen, x));} \
  static bool wrapped_ ## Type ## _p(s7_pointer obj) {return((mus_is_xen(obj)) && (mus_is_ ## Type (Xen_to_mus_any(obj))));} \
  static s7_pointer Type ## _2; \
  static s7_pointer g_ ## Type ## _2(s7_scheme *sc, s7_pointer args) \
  { \
    mus_any *_o_; \
    mus_float_t _fm_; \
   \
    GET_GENERATOR(args, Type, _o_); \
    GET_REAL_CADR(args, Type, _fm_);	       \
    return(s7_make_real(sc, Func(_o_, _fm_))); \
  } \
  static s7_pointer direct_ ## Type ## _2; \
  static s7_pointer g_direct_ ## Type ## _2(s7_scheme *sc, s7_pointer args) \
  { \
    mus_any *_o_;	  \
    mus_float_t _fm_; \
    s7_pointer rl; \
    _fm_ = s7_cell_real(rl = s7_call_direct(sc, cadr(args)));	\
    GET_GENERATOR(args, Type, _o_); \
    return(s7_remake_real(sc, rl, Func(_o_, _fm_)));		    \
  }

static s7_pointer mul_c_oscil_2;
static s7_pointer g_mul_c_oscil_2(s7_scheme *sc, s7_pointer args)
{
  mus_any *_o_;
  mus_float_t _fm_, _mul_;
  _mul_ = s7_number_to_real(sc, car(args));
  args = cdr(args);
  GET_GENERATOR_CADAR(args, oscil, _o_);
  GET_REAL(s7_cddar(args), oscil, _fm_);
  return(s7_make_real(sc, _mul_ * mus_oscil_fm(_o_, _fm_)));
}

static s7_pointer mul_s_oscil_2;
static s7_pointer g_mul_s_oscil_2(s7_scheme *sc, s7_pointer args)
{
  mus_any *_o_;
  mus_float_t _fm_, _f_;
  s7_pointer _mul_;
  GET_NUMBER(args, "*", _mul_);
  args = cdr(args);
  GET_GENERATOR_CADAR(args, oscil, _o_);
  GET_REAL(s7_cddar(args), oscil, _fm_);
  _f_ = mus_oscil_fm(_o_, _fm_);
  if (s7_is_real(_mul_))
    return(s7_make_real(sc, s7_number_to_real(sc, _mul_) * _f_));
  return(s7_make_complex(sc, s7_real_part(_mul_) * _f_, s7_imag_part(_mul_) * _f_));
}

static s7_pointer env_oscil_2;
static s7_pointer g_env_oscil_2(s7_scheme *sc, s7_pointer args)
{
  mus_any *_o_, *_e_;
  mus_float_t _fm_;
  GET_GENERATOR_CADAR(args, env, _e_);
  args = cdr(args);
  GET_GENERATOR_CADAR(args, oscil, _o_);
  GET_REAL(s7_cddar(args), oscil, _fm_);
  return(s7_make_real(sc, mus_env(_e_) * mus_oscil_fm(_o_, _fm_)));
}

static s7_pointer env_wave_train_2;
static s7_pointer g_env_wave_train_2(s7_scheme *sc, s7_pointer args)
{
  mus_any *_o_, *_e_;
  mus_float_t _fm_;
  GET_GENERATOR_CADAR(args, env, _e_);
  args = cdr(args);
  GET_GENERATOR_CADAR(args, wave_train, _o_);
  GET_REAL(s7_cddar(args), wave_train, _fm_);
  return(s7_make_real(sc, mus_env(_e_) * mus_wave_train(_o_, _fm_)));
}


GEN_1(oscil, mus_oscil_unmodulated)
GEN_2(oscil, mus_oscil_fm)

GEN_1(oscil_bank, mus_oscil_bank)

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
GEN_1(rxykcos, mus_rxykcos_unmodulated)
GEN_1(rxyksin, mus_rxyksin_unmodulated)
GEN_1(pulsed_env, mus_pulsed_env_unmodulated)

GEN_1(readin, mus_readin)

GEN_2(comb, mus_comb_unmodulated)
GEN_2(notch, mus_notch_unmodulated)
GEN_2(all_pass, mus_all_pass_unmodulated)
GEN_2(pulsed_env, mus_pulsed_env)
GEN_2(delay, mus_delay_unmodulated)

GEN_2(moving_average, mus_moving_average)
GEN_2(moving_max, mus_moving_max)
GEN_2(moving_norm, mus_moving_norm)
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
GEN_2(rxyksin, mus_rxyksin)
GEN_2(rxykcos, mus_rxykcos)
GEN_2(one_zero, mus_one_zero)
GEN_2(one_pole, mus_one_pole)
GEN_2(two_zero, mus_two_zero)
GEN_2(two_pole, mus_two_pole)
GEN_2(filter, mus_filter)
GEN_2(fir_filter, mus_fir_filter)
GEN_2(iir_filter, mus_iir_filter)
GEN_2(one_pole_all_pass, mus_one_pole_all_pass)
GEN_2(formant, mus_formant)
GEN_2(firmant, mus_firmant)
GEN_2(comb_bank, mus_comb_bank)
GEN_2(all_pass_bank, mus_all_pass_bank)
GEN_2(filtered_comb_bank, mus_filtered_comb_bank)

GEN_2(wave_train, mus_wave_train)
GEN_2(table_lookup, mus_table_lookup)

GEN_2(ssb_am, mus_ssb_am_unmodulated)
GEN_2(asymmetric_fm, mus_asymmetric_fm_unmodulated)
GEN_2(polyshape, mus_polyshape_unmodulated)
GEN_2(filtered_comb, mus_filtered_comb_unmodulated)

GEN_1(granulate, mus_granulate_simple)
GEN_1(src, mus_src_simple)
GEN_2(src, mus_src_two)
GEN_1(convolve, mus_convolve_simple)
GEN_1(phase_vocoder, mus_phase_vocoder_simple)


/* ---------------- special cases ---------------- */

/* "direct" => callable via s7_call_direct, one of our local functions, so we can safely reuse its output if it's real (might be complex)
 * "indirect" => callable via s7_call_direct, but might be (+ 0.1) or equivalent, so we have to make a new real on output
 * here's an example that forces this distinction:
 *    (define (hi) (let ((o (make-oscil 440.0))) (do ((i 0 (+ i 1))) ((= i 100)) (outa i (oscil o (+ 0.1)))))) (with-sound () (hi))
 */

static s7_pointer oscil_one;
static s7_pointer g_oscil_one(s7_scheme *sc, s7_pointer args)
{
  mus_xen *gn;
  gn = (mus_xen *)imported_s7_object_value_checked(car(args), mus_xen_tag);
  if (gn)
    return(s7_make_real(sc, mus_oscil_unmodulated(gn->gen)));
  Xen_check_type(false, car(args), 1, S_oscil, "a generator");
  return(s7_f(sc));
}

static s7_pointer oscil_two;
static s7_pointer g_oscil_two(s7_scheme *sc, s7_pointer args)
{
  mus_xen *gn;
  gn = (mus_xen *)imported_s7_object_value_checked(car(args), mus_xen_tag);
  if (gn)
    return(s7_make_real(sc, mus_oscil_fm(gn->gen, s7_number_to_real_with_caller(sc, cadr(args), S_oscil))));
  Xen_check_type(false, car(args), 1, S_oscil, "a generator");
  return(s7_f(sc));
}

static s7_pointer formant_two;
static s7_pointer g_formant_two(s7_scheme *sc, s7_pointer args)
{
  mus_xen *gn;
  gn = (mus_xen *)imported_s7_object_value_checked(car(args), mus_xen_tag);
  if ((gn) &&
      (mus_is_formant(gn->gen)))
    return(s7_make_real(sc, mus_formant(gn->gen, s7_number_to_real_with_caller(sc, cadr(args), S_formant))));
  Xen_check_type(false, car(args), 1, S_formant, "a " S_formant " generator");
  return(s7_f(sc));
}

static s7_pointer formant_bank_ss, formant_bank_sz;
static s7_pointer g_formant_bank_ss(s7_scheme *sc, s7_pointer args)
{
  return(g_formant_bank(s7_car_value(sc, args), s7_cadr_value(sc, args)));
}

static s7_pointer g_formant_bank_sz(s7_scheme *sc, s7_pointer args)
{
  return(g_formant_bank(s7_car_value(sc, args), s7_call_direct(sc, cadr(args))));
}

static s7_pointer outa_two;
static s7_pointer g_outa_two(s7_scheme *sc, s7_pointer args)
{
  mus_long_t pos;
  pos = (mus_long_t)s7_number_to_integer_with_caller(sc, car(args), "outa");
  if (pos < 0) 
    Xen_out_of_range_error(S_outa, 1, car(args), "must be >= 0");    
  return(out_any_2(pos, s7_number_to_real_with_caller(sc, cadr(args), S_outa), 0, S_outa));
}

static s7_pointer outb_two;
static s7_pointer g_outb_two(s7_scheme *sc, s7_pointer args)
{
  mus_long_t pos;
  pos = (mus_long_t)s7_number_to_integer_with_caller(sc, car(args), "outb");
  if (pos < 0) 
    Xen_out_of_range_error(S_outb, 1, car(args), "must be >= 0");    
  return(out_any_2(pos, s7_number_to_real_with_caller(sc, cadr(args), S_outb), 1, S_outb));
}


static s7_pointer oscil_mul_c_s;
static s7_pointer g_oscil_mul_c_s(s7_scheme *sc, s7_pointer args)
{
  /* (oscil g (* c s)), args is (g (* c s)) */
  mus_any *o;
  mus_float_t x;
  s7_pointer vargs;

  GET_GENERATOR(args, oscil, o);
  vargs = cdadr(args);
  GET_REAL(cdr(vargs), oscil, x);
  return(s7_make_real(sc, mus_oscil_fm(o, x * s7_number_to_real_with_caller(sc, car(vargs), S_oscil))));
}

static s7_pointer oscil_mul_ss;
static s7_pointer g_oscil_mul_ss(s7_scheme *sc, s7_pointer args)
{
  /* (oscil g (* s s)), args is (g (* s s)) */
  mus_any *o;
  mus_float_t x, y;
  s7_pointer vargs;

  GET_GENERATOR(args, oscil, o);
  vargs = cdadr(args);
  GET_REAL(vargs, oscil, x);
  GET_REAL(cdr(vargs), oscil, y);
  return(s7_make_real(sc, mus_oscil_fm(o, x * y)));
}

/* (with-sound () (fm-violin 0 .0001 440 .1)) */
static s7_pointer fm_violin_vibrato;
static s7_pointer g_fm_violin_vibrato(s7_scheme *sc, s7_pointer args)
{
  mus_any *e = NULL, *t = NULL, *r = NULL;
  
  GET_GENERATOR_CADAR(args, env, e);
  args = cdr(args);
  GET_GENERATOR_CADAR(args, triangle_wave, t);
  args = cdr(args);
  GET_GENERATOR_CADAR(args, rand_interp, r);
  
  return(s7_make_real(sc, mus_env(e) + mus_triangle_wave_unmodulated(t) + mus_rand_interp_unmodulated(r)));			
}

static s7_pointer env_vss;
static s7_pointer g_env_vss(s7_scheme *sc, s7_pointer args)
{
  mus_xen *gn;
  mus_any *g = NULL;

  /* args is ((vector-ref v i))" */
  gn = (mus_xen *)s7_vector_ref_object_value_checked(sc, cdar(args), mus_xen_tag);
  if (gn) g = mus_xen_to_mus_any(gn);
  Xen_check_type((g) && (mus_is_env(g)), cdar(args), 1, S_env, "an env generator");

  return(C_double_to_Xen_real(mus_env(g)));
}

static s7_pointer oscil_vss_s;
static s7_pointer g_oscil_vss_s(s7_scheme *sc, s7_pointer args)
{
  mus_float_t x;
  mus_xen *gn;
  mus_any *g = NULL;

  GET_REAL_CADR(args, oscil, x);
  gn = (mus_xen *)s7_vector_ref_object_value_checked(sc, cdar(args), mus_xen_tag);
  if (gn) g = mus_xen_to_mus_any(gn);
  Xen_check_type((g) && (mus_is_oscil(g)), cdar(args), 1, S_oscil, "an env generator");

  return(C_double_to_Xen_real(mus_oscil_fm(g, x)));
}


static s7_pointer env_polywave_env;
static s7_pointer g_env_polywave_env(s7_scheme *sc, s7_pointer args)
{
  mus_any *e1, *t, *e2;

  GET_GENERATOR_CADAR(args, env, e1);
  args = cadr(args);
  GET_GENERATOR_CADR(args, polywave, t);
  args = caddr(args);
  GET_GENERATOR_CADR(args, env, e2);

  return(s7_make_real(sc, mus_env(e1) * mus_polywave(t, mus_env(e2))));
}

static s7_pointer ina_ss;
static s7_pointer g_ina_ss(s7_scheme *sc, s7_pointer args)
{
  return(g_in_any_1(S_ina, s7_car_value(sc, args), 0, s7_cadr_value(sc, args)));
}

static s7_pointer file_to_sample_ss;
static s7_pointer g_file_to_sample_ss(s7_scheme *sc, s7_pointer args)
{
  return(g_in_any_1(S_ina, s7_cadr_value(sc, args), 0, s7_car_value(sc, args)));
}


static s7_pointer indirect_locsig_3;
static s7_pointer g_indirect_locsig_3(s7_scheme *sc, s7_pointer args)
{
  s7_int pos;
  s7_pointer x;
  mus_any *locs;

  GET_GENERATOR(args, locsig, locs);
  GET_INTEGER_CADR(args, outa, pos);
  x = s7_call_direct(sc, caddr(args));
  mus_locsig(locs, pos, s7_number_to_real_with_caller(sc, x, S_locsig));
  return(Xen_integer_zero);
}


typedef struct {
  s7_pointer mul_c_gen, mul_c_gen_1, mul_s_gen, mul_s_gen_1, env_gen, env_gen_1;
  mus_float_t (*gen_direct_1)(mus_xen *p);
  mus_float_t (*gen_direct_2)(mus_xen *p, mus_float_t f1);
  mus_float_t (*gen_direct_3)(mus_xen *p, mus_float_t f1, mus_float_t f2);
  bool (*gen_direct_checker)(s7_pointer obj);
} gen_choices;

static s7_pointer outa_ss;
static s7_pointer g_outa_ss(s7_scheme *sc, s7_pointer args)
{
  s7_int pos;
  mus_float_t x, y;
  s7_pointer vargs;
  GET_INTEGER(args, outa, pos);
  vargs = cdadr(args);
  GET_REAL(vargs, outa, x);
  GET_REAL(cdr(vargs), outa, y);
  return(out_any_2(pos, x * y, 0, S_outa));
}

static s7_pointer indirect_outa_2;
static s7_pointer g_indirect_outa_2(s7_scheme *sc, s7_pointer args)
{
  s7_int pos;
  s7_pointer x;
  GET_INTEGER(args, outa, pos);
  x = s7_call_direct(sc, cadr(args));
  return(out_any_2(pos, s7_number_to_real_with_caller(sc, x, S_outa), 0, S_outa));
}


static s7_pointer indirect_outa_2_temp;
static s7_pointer g_indirect_outa_2_temp(s7_scheme *sc, s7_pointer args)
{
  s7_int pos;
  mus_float_t x;
  GET_INTEGER(args, outa, pos);
  x = s7_call_direct_to_real_and_free(sc, cadr(args));
  return(out_any_2(pos, x, 0, S_outa));
}


static s7_pointer indirect_outa_2_env;
static s7_pointer g_indirect_outa_2_env(s7_scheme *sc, s7_pointer args)
{
  s7_int pos;
  mus_float_t x;
  mus_any *e = NULL;

  GET_INTEGER(args, outa, pos);
  args = cdadr(args);
  GET_GENERATOR_CADAR(args, env, e);
  /*
    80262: (+ (polywave gen1 frq1) (* (env ampf2) (oscil gen2 frq2)) (* (env ampf3) (polywave gen3 frq3)))
    56007: (formant-bank fb (* (+ 0.9 (rand-interp rnd1)) (polywave gen1 (+ (env frqf) (* (env intrpf) (+ (* hz7 (oscil vib)) (rand-interp rnd)))))))
  */

  x = s7_call_direct_to_real_and_free(sc, cadr(args)); /* g_mul_env_direct from chooser, so it's a temp -- the free might be slower */
  return(out_any_2(pos, mus_env(e) * x, 0, S_outa));
}


static s7_pointer indirect_outa_ss;
static s7_pointer g_indirect_outa_ss(s7_scheme *sc, s7_pointer args)
{
  s7_int pos;
  mus_float_t x;

  GET_INTEGER(args, outa, pos);
  GET_REAL_CADR(args, outa, x);
  return(out_any_2(pos, x, 0, S_outa));
}


static gen_choices *make_choices(s7_pointer mul_c, s7_pointer mul_s, s7_pointer e, s7_pointer mul_c1, s7_pointer mul_s1, s7_pointer e1)
{
  gen_choices *choices;
  choices = (gen_choices *)calloc(1, sizeof(gen_choices));
  choices->mul_c_gen = mul_c;
  choices->mul_s_gen = mul_s;
  choices->env_gen = e;
  choices->mul_c_gen_1 = mul_c1;
  choices->mul_s_gen_1 = mul_s1;
  choices->env_gen_1 = e1;
  return(choices);
}

void store_choices(s7_scheme *sc, 
		   s7_pointer base_f, 
		   mus_float_t (*g1)(mus_xen *p),
		   mus_float_t (*g2)(mus_xen *p, mus_float_t f1),
		   mus_float_t (*g3)(mus_xen *p, mus_float_t f1, mus_float_t f2),
		   bool (*isg)(s7_pointer obj))
{
  gen_choices *choices;
  choices = (gen_choices *)s7_function_chooser_data(sc, base_f);
  if (!choices)
    {
      choices = (gen_choices *)calloc(1, sizeof(gen_choices));
      s7_function_chooser_set_data(sc, base_f, (void *)choices);
    }
  choices->gen_direct_1 = g1;
  choices->gen_direct_2 = g2;
  choices->gen_direct_3 = g3;
  choices->gen_direct_checker = isg;
}

static void direct_choice_1(s7_scheme *sc, 
			    s7_pointer f, 
			    mus_float_t (*g1)(mus_xen *p),
			    bool (*isg)(s7_pointer obj))
{
  gen_choices *choices;
  choices = (gen_choices *)s7_function_chooser_data(sc, f);
  if (!choices)
    {
      choices = (gen_choices *)calloc(1, sizeof(gen_choices));
      s7_function_chooser_set_data(sc, f, (void *)choices);
    }
  choices->gen_direct_1 = g1;
  choices->gen_direct_checker = isg;
}

static void direct_choice_2(s7_scheme *sc, 
			    s7_pointer f, 
			    mus_float_t (*g2)(mus_xen *p, mus_float_t f1),
			    bool (*isg)(s7_pointer obj))
{
  gen_choices *choices;
  choices = (gen_choices *)s7_function_chooser_data(sc, f);
  if (!choices)
    {
      choices = (gen_choices *)calloc(1, sizeof(gen_choices));
      s7_function_chooser_set_data(sc, f, (void *)choices);
    }
  choices->gen_direct_2 = g2;
  choices->gen_direct_checker = isg;
}

static void init_choices(void)
{
  gen_choices *choices;

#define SET_GEN_1(Gen) \
  do {\
    choices = (gen_choices *)s7_function_chooser_data_direct(Gen ## _1); \
    choices->gen_direct_1 = wrapped_ ## Gen ## _1; \
    choices->gen_direct_checker = wrapped_ ## Gen ## _p; \
  } while (0)

  SET_GEN_1(oscil);
  SET_GEN_1(oscil_bank);
  SET_GEN_1(polywave);
  SET_GEN_1(rand);
  SET_GEN_1(rand_interp);
  SET_GEN_1(table_lookup);
  SET_GEN_1(wave_train);
  SET_GEN_1(pulse_train);
  SET_GEN_1(triangle_wave);
  SET_GEN_1(square_wave);
  SET_GEN_1(sawtooth_wave);
  SET_GEN_1(env);
  SET_GEN_1(ncos);
  SET_GEN_1(nsin);
  SET_GEN_1(nrxycos);
  SET_GEN_1(nrxysin);
  SET_GEN_1(rxykcos);
  SET_GEN_1(rxyksin);
  SET_GEN_1(readin);
  SET_GEN_1(granulate);
  SET_GEN_1(src);
  SET_GEN_1(convolve);
  SET_GEN_1(phase_vocoder);
  SET_GEN_1(pulsed_env);

#define SET_GEN_2(Gen) \
  do {\
    choices = (gen_choices *)s7_function_chooser_data_direct(Gen ## _2); \
    choices->gen_direct_2 = wrapped_ ## Gen ## _2; \
    choices->gen_direct_checker = wrapped_ ## Gen ## _p; \
  } while(0)

  SET_GEN_2(polywave);
  SET_GEN_2(oscil);
  SET_GEN_2(comb);
  SET_GEN_2(notch);
  SET_GEN_2(all_pass);
  SET_GEN_2(delay);
  SET_GEN_2(moving_average);
  SET_GEN_2(moving_max);
  SET_GEN_2(moving_norm);
  SET_GEN_2(rand);
  SET_GEN_2(rand_interp);
  SET_GEN_2(ncos);
  SET_GEN_2(nsin);
  SET_GEN_2(sawtooth_wave);
  SET_GEN_2(pulse_train);
  SET_GEN_2(square_wave);
  SET_GEN_2(triangle_wave);
  SET_GEN_2(nrxysin);
  SET_GEN_2(nrxycos);
  SET_GEN_2(rxyksin);
  SET_GEN_2(rxykcos);
  SET_GEN_2(one_zero);
  SET_GEN_2(one_pole);
  SET_GEN_2(two_zero);
  SET_GEN_2(two_pole);
  SET_GEN_2(filter);
  SET_GEN_2(fir_filter);
  SET_GEN_2(iir_filter);
  SET_GEN_2(formant);
  SET_GEN_2(firmant);
  SET_GEN_2(one_pole_all_pass);
  SET_GEN_2(wave_train);
  SET_GEN_2(table_lookup);
  SET_GEN_2(ssb_am);
  SET_GEN_2(asymmetric_fm);
  SET_GEN_2(polyshape);
  SET_GEN_2(filtered_comb);
  SET_GEN_2(pulsed_env);
  SET_GEN_2(comb_bank);
  SET_GEN_2(all_pass_bank);
  SET_GEN_2(filtered_comb_bank);
}


/* ---------------- */

static s7_pointer mul_direct_2;
static s7_pointer g_mul_direct_2(s7_scheme *sc, s7_pointer args)
{
  s7_pointer y;
  mus_float_t x;

  x = s7_call_direct_to_real_and_free(sc, car(args));
  y = s7_call_direct(sc, cadr(args));
  return(s7_remake_real(sc, y, x * s7_cell_real(y)));
}

static s7_pointer add_direct_2;
static s7_pointer g_add_direct_2(s7_scheme *sc, s7_pointer args)
{
  s7_pointer y;
  mus_float_t x;
  x = s7_call_direct_to_real_and_free(sc, car(args));
  y = s7_call_direct(sc, cadr(args));
  return(s7_remake_real(sc, y, x + s7_cell_real(y)));
}

static s7_pointer add_env_direct_2;
static s7_pointer g_add_env_direct_2(s7_scheme *sc, s7_pointer args)
{
  mus_float_t x;
  s7_pointer y;
  mus_any *e;
  
  GET_GENERATOR_CADAR(args, env, e);
  x = mus_env(e);
  y = s7_call_direct(sc, cadr(args));
  return(s7_remake_real(sc, y, x + s7_cell_real(y)));
}

static s7_pointer add_env_direct_3;
static s7_pointer g_add_env_direct_3(s7_scheme *sc, s7_pointer args)
{
  s7_pointer y;
  mus_any *e;
  mus_float_t x, z;
  
  GET_GENERATOR_CADAR(args, env, e);
  x = mus_env(e);
  args = cdr(args);
  y = s7_call_direct(sc, car(args));
  z = s7_call_direct_to_real_and_free(sc, cadr(args));
  return(s7_remake_real(sc, y, x + z + s7_cell_real(y)));
}

static s7_pointer add_direct_any;
static s7_pointer g_add_direct_any(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, z;
  mus_float_t sum;

  z = s7_call_direct(sc, car(args));
  sum = s7_cell_real(z);
  for (p = cdr(args); s7_is_pair(p); p = cdr(p))
    sum += s7_call_direct_to_real_and_free(sc, car(p));

  return(s7_remake_real(sc, z, sum));
}

static s7_pointer mul_env_direct;
static s7_pointer g_mul_env_direct(s7_scheme *sc, s7_pointer args)
{
  /* (* (env e) ...) */
  s7_pointer x;
  mus_float_t y;
  mus_any *e = NULL;

  GET_GENERATOR_CADAR(args, env, e);
  y = mus_env(e);
  x = s7_call_direct(sc, cadr(args));
  return(s7_remake_real(sc, x, y * s7_cell_real(x)));
}

static s7_pointer add_c_direct;
static s7_pointer g_add_c_direct(s7_scheme *sc, s7_pointer args)
{
  /* (+ c ...) */
  s7_pointer x;
  x = s7_call_direct(sc, cadr(args));
  return(s7_remake_real(sc, x, s7_number_to_real(sc, car(args)) + s7_cell_real(x)));
}

static s7_pointer add_c_rand_interp;
static s7_pointer g_add_c_rand_interp(s7_scheme *sc, s7_pointer args)
{
  /* (+ c (rand-interp g)) */
  mus_any *r;
  mus_float_t x;

  x = s7_number_to_real(sc, car(args));
  args = cdr(args);
  GET_GENERATOR_CADAR(args, rand-interp, r);

  return(s7_make_real(sc, x + mus_rand_interp_unmodulated(r)));
}

static s7_pointer (*initial_add_chooser)(s7_scheme *sc, s7_pointer f, int args, s7_pointer expr);
static s7_pointer clm_add_chooser(s7_scheme *sc, s7_pointer f, int args, s7_pointer expr)
{
  if (args == 2)
    {
      s7_pointer arg1, arg2;
      arg1 = cadr(expr);
      arg2 = caddr(expr);
      if (s7_is_pair(arg2))
	{
	  if ((car(arg2) != quote_symbol) &&
	      (s7_function_choice_is_direct_to_real(sc, arg2)))
	    {
	      if (s7_is_real(arg1))
		{
		  s7_function_choice_set_direct(sc, expr);
		  if ((cddr(arg2) == s7_nil(sc)) &&
		      (car(arg2) == rand_interp_symbol) &&
		      (s7_is_symbol(cadr(arg2))))
		    return(add_c_rand_interp);
		  
		  return(add_c_direct);
		}
	      
	      if (s7_is_pair(arg1))
		{
		  if (s7_function_choice_is_direct_to_real(sc, arg1))
		    {
		      s7_function_choice_set_direct(sc, expr);
		      if (caadr(expr) == env_symbol)
			return(add_env_direct_2);
		      return(add_direct_2);
		    }
		}
	    }
	}
    }
  else
    {
      bool happy = true;
      s7_pointer p;

      if (args == 3)
	{
	  if ((s7_is_pair(cadr(expr))) &&
	      (caadr(expr) == env_symbol) &&
	      (s7_is_symbol(cadadr(expr))) &&
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
	}
	
      if (args < 2) 
	return((*initial_add_chooser)(sc, f, args, expr));

      for (p = cdr(expr); s7_is_pair(p); p = cdr(p))
	if ((!s7_is_pair(car(p))) ||
	    (!(s7_function_choice_is_direct_to_real(sc, car(p)))))
	  {
	    happy = false;
	    break;
	  }
      if (happy)
	{
	  s7_function_choice_set_direct(sc, expr);
	  if ((caadr(expr) == env_symbol) && (args == 3))
	    {
	      if (args == 3)
		return(add_env_direct_3);
	    }
	  return(add_direct_any);
	}
    }
  return((*initial_add_chooser)(sc, f, args, expr));
}


static s7_pointer (*initial_multiply_chooser)(s7_scheme *sc, s7_pointer f, int args, s7_pointer expr);
static s7_pointer clm_multiply_chooser(s7_scheme *sc, s7_pointer f, int args, s7_pointer expr)
{
  /* fprintf(stderr, "* expr: %s\n", DISPLAY(expr)); */

  if (args == 2)
    {
      s7_pointer arg1, arg2;
      arg1 = cadr(expr);
      arg2 = caddr(expr);

      if (s7_is_pair(arg2))
	{
	  if (s7_is_pair(arg1))
	    {
	      if (car(arg1) == env_symbol)
		{
		  gen_choices *choices;
		  if (s7_is_symbol(cadr(arg1)))
		    {
		      if ((car(arg2) == polywave_symbol) &&
			  (s7_is_pair(cdr(arg2))) &&
			  (s7_is_symbol(cadr(arg2))) &&
			  (s7_is_pair(cddr(arg2))))
			{
			  s7_pointer caddr_arg2 = caddr(arg2);
			  if (s7_is_pair(caddr_arg2))
			    {
			      if ((car(caddr_arg2) == env_symbol) &&
				  (s7_is_symbol(cadr(caddr_arg2))))
				{
				  s7_function_choice_set_direct(sc, expr);
				  return(env_polywave_env);
				}
			    }
			  /* (* (env ampf) (polywave gen1 (+ (env frqf) (rand-interp noise)))) */
			}
		    }
		  
		  choices = (gen_choices *)s7_function_chooser_data(sc, arg2);
		  if (choices)
		    {
		      if (choices->env_gen)
			{
			  /* fprintf(stderr, "env_gen\n"); */
			  s7_function_choice_set_direct(sc, expr);
			  return(choices->env_gen);
			}
		      if (choices->env_gen_1)
			{
			  /* fprintf(stderr, "env_gen_1\n"); */
			  s7_function_choice_set_direct(sc, expr);
			  return(choices->env_gen_1);
			}
		    }
		  if (s7_function_choice_is_direct_to_real(sc, arg2))
		    {
		      /* fprintf(stderr, "mul_env_direct\n"); */
		      s7_function_choice_set_direct(sc, expr);
		      return(mul_env_direct);
		    }
		}
	      
	      if ((s7_function_choice_is_direct_to_real(sc, arg1)) &&
		  (s7_function_choice_is_direct_to_real(sc, arg2)))
		{
		  s7_function_choice_set_direct(sc, expr);
		  /* fprintf(stderr, "mul_direct_2\n"); */
		  return(mul_direct_2);
		}
	    }
	  else
	    {
	      if (s7_is_real(arg1))
		{
		  /* (* num (gen...))
		   */
		  gen_choices *choices;
		  choices = (gen_choices *)s7_function_chooser_data(sc, arg2);
		  if (choices)
		    {
		      if (choices->mul_c_gen)
			{
			  s7_function_choice_set_direct(sc, expr);
			  /* fprintf(stderr, "mul_c_gen\n"); */
			  return(choices->mul_c_gen);
			}
#if 0
		      if (choices->mul_c_gen_1)
			{
			  s7_function_choice_set_direct(sc, expr);
			  /* fprintf(stderr, "mul_c_gen_1\n"); */
			  return(choices->mul_c_gen_1);
			}
#endif
		    }
		}
	      else
		{
		  if ((s7_is_symbol(arg1)) &&
		      (s7_is_pair(cddr(expr))))          /* (* + '(vector?)) -- this is the optimizer's fault */
		    {
		      gen_choices *choices;
		      choices = (gen_choices *)s7_function_chooser_data(sc, arg2);
		      if (choices)
			{
			  if (choices->mul_s_gen)
			    {
			      s7_function_choice_set_direct(sc, expr);
			      /* fprintf(stderr, "mul_s_gen\n"); */
			      return(choices->mul_s_gen);
			    }
			  if (choices->mul_s_gen_1)
			    {
			      s7_function_choice_set_direct(sc, expr);
			      /* fprintf(stderr, "mul_s_gen_1\n"); */
			      return(choices->mul_s_gen_1);
			    }
			}
		    }
		}
	    }
	}
    }
  return((*initial_multiply_chooser)(sc, f, args, expr));
}

static s7_pointer oscil_chooser(s7_scheme *sc, s7_pointer f, int args, s7_pointer expr)
{
  s7_pointer arg1, arg2; 

  arg1 = cadr(expr);

  if (args == 1)
    {
      if (s7_is_symbol(arg1))
	{
	  s7_function_choice_set_direct(sc, expr);
	  return(oscil_1);
	}
      return(oscil_one);
    }

  if (args == 2)
    {
      arg2 = caddr(expr);
      if (s7_is_symbol(arg1))
	{
	  if (s7_is_symbol(arg2))
	    {
	      s7_function_choice_set_direct(sc, expr);
	      return(oscil_2);
	    }
	  if (s7_is_pair(arg2))
	    {
	      if (s7_list_length(sc, arg2) == 3)
		{
		  if (s7_caaddr(expr) == multiply_symbol)
		    {
		      if ((s7_is_real(cadr(arg2))) &&
			  (s7_is_symbol(caddr(arg2))))
			{
			  s7_function_choice_set_direct(sc, expr);
			  return(oscil_mul_c_s);
			}
		      if ((s7_is_symbol(cadr(arg2))) &&
			  (s7_is_symbol(caddr(arg2))))
			{
			  s7_function_choice_set_direct(sc, expr);
			  return(oscil_mul_ss);
			}
		    }
		}

	      if (s7_function_choice_is_direct(sc, arg2))
		{
		  
		  if (s7_function_returns_temp(sc, arg2)) 
		    {
		      s7_function_choice_set_direct(sc, expr);
		      return(direct_oscil_2);
		    }
		}
	    }
	}
      if ((s7_is_symbol(arg2)) &&
	  (s7_is_pair(arg1)) &&
	  (car(arg1) == vector_ref_symbol) &&
	  (s7_is_symbol(cadr(arg1))) &&
	  (s7_is_symbol(caddr(arg1))) &&
	  (s7_is_null(s7, s7_cdddr(arg1))))
	{
	  s7_function_choice_set_direct(sc, expr);
	  return(oscil_vss_s);
	}
      return(oscil_two);
    }
  return(f);
}

static s7_pointer oscil_bank_chooser(s7_scheme *sc, s7_pointer f, int args, s7_pointer expr)
{
  if (args == 1)
    {
      if (s7_is_symbol(cadr(expr)))
	{
	  s7_function_choice_set_direct(sc, expr);
	  return(oscil_bank_1);
	}
    }
  return(f);
}


static s7_pointer comb_bank_chooser(s7_scheme *sc, s7_pointer f, int args, s7_pointer expr)
{
  if (args == 2)
    {
      if (s7_is_symbol(cadr(expr)))
	{
	  if (s7_is_symbol(caddr(expr)))
	    {
	      s7_function_choice_set_direct(sc, expr);
	      return(comb_bank_2);
	    }
	  if (s7_function_choice_is_direct(sc, caddr(expr)))
	    {
	      if (s7_function_returns_temp(sc, caddr(expr))) 
		{
		  s7_function_choice_set_direct(sc, expr);
		  return(direct_comb_bank_2);
		}
	    }
	}
    }
  return(f);
}


static s7_pointer filtered_comb_bank_chooser(s7_scheme *sc, s7_pointer f, int args, s7_pointer expr)
{
  if (args == 2)
    {
      if (s7_is_symbol(cadr(expr)))
	{
	  if (s7_is_symbol(caddr(expr)))
	    {
	      s7_function_choice_set_direct(sc, expr);
	      return(filtered_comb_bank_2);
	    }
	  if (s7_function_choice_is_direct(sc, caddr(expr)))
	    {
	      if (s7_function_returns_temp(sc, caddr(expr))) 
		{
		  s7_function_choice_set_direct(sc, expr);
		  return(direct_filtered_comb_bank_2);
		}
	    }
	}
    }
  return(f);
}


static s7_pointer all_pass_bank_chooser(s7_scheme *sc, s7_pointer f, int args, s7_pointer expr)
{
  if (args == 2)
    {
      if (s7_is_symbol(cadr(expr)))
	{
	  if (s7_is_symbol(caddr(expr)))
	    {
	      s7_function_choice_set_direct(sc, expr);
	      return(all_pass_bank_2);
	    }
	  if (s7_function_choice_is_direct(sc, caddr(expr)))
	    {
	      if (s7_function_returns_temp(sc, caddr(expr))) 
		{
		  s7_function_choice_set_direct(sc, expr);
		  return(direct_all_pass_bank_2);
		}
	    }
	}
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
      (s7_is_symbol(cadr(expr)))) /* the polywave gen */
    {
      s7_pointer arg2;
      arg2 = caddr(expr);

      if (s7_is_symbol(arg2))
	{
	  s7_function_choice_set_direct(sc, expr);
	  return(polywave_2);
	}
      if (s7_is_pair(arg2))
	{
	  if (s7_function_choice_is_direct(sc, arg2))
	    {
	      if (s7_function_returns_temp(sc, arg2)) 
		{
		  s7_function_choice_set_direct(sc, expr);
		  return(direct_polywave_2);
		}
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
	      if (s7_function_returns_temp(sc, caddr(expr))) 
		{
		  s7_function_choice_set_direct(sc, expr);
		  return(direct_table_lookup_2);
		}
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
	      if (s7_function_returns_temp(sc, caddr(expr))) 
		{
		  s7_function_choice_set_direct(sc, expr);
		  return(direct_wave_train_2);
		}
	    }
	}
    }
  return(f);
}

static s7_pointer src_chooser(s7_scheme *sc, s7_pointer f, int args, s7_pointer expr)
{
  if ((args == 2) &&
      (s7_is_symbol(cadr(expr))))
    {
      if (s7_is_symbol(caddr(expr)))
	{
	  s7_function_choice_set_direct(sc, expr);
	  return(src_2);
	}
      if (s7_is_pair(caddr(expr)))
	{
	  if (s7_function_choice_is_direct(sc, caddr(expr)))
	    {
	      if (s7_function_returns_temp(sc, caddr(expr))) 
		{
		  s7_function_choice_set_direct(sc, expr);
		  return(direct_src_2);
		}
	    }
	}
    }
  return(f);
}

static s7_pointer granulate_chooser(s7_scheme *sc, s7_pointer f, int args, s7_pointer expr)
{
  if ((args == 1) &&
      (s7_is_symbol(cadr(expr))))
    {
      s7_function_choice_set_direct(sc, expr);
      return(granulate_1);
    }
  return(f);
}

static s7_pointer convolve_chooser(s7_scheme *sc, s7_pointer f, int args, s7_pointer expr)
{
  if ((args == 1) &&
      (s7_is_symbol(cadr(expr))))
    {
      s7_function_choice_set_direct(sc, expr);
      return(convolve_1);
    }
  return(f);
}


static s7_pointer phase_vocoder_chooser(s7_scheme *sc, s7_pointer f, int args, s7_pointer expr)
{
  if ((args == 1) &&
      (s7_is_symbol(cadr(expr))))
    {
      s7_function_choice_set_direct(sc, expr);
      return(phase_vocoder_1);
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
      if (s7_function_returns_temp(sc, caddr(expr))) 
	{
	  s7_function_choice_set_direct(sc, expr);
	  return(direct_nsin_2);
	}
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
      if (s7_function_returns_temp(sc, caddr(expr))) 
	{
	  s7_function_choice_set_direct(sc, expr);
	  return(direct_ncos_2);
	}
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
      (s7_is_pair(caddr(expr))))
    {
      s7_pointer arg2;
      arg2 = caddr(expr);
      if (s7_function_choice_is_direct(sc, arg2))
	{
	  if (s7_function_returns_temp(sc, arg2))
	    {
	      s7_function_choice_set_direct(sc, expr);
	      return(direct_nrxysin_2);
	    }
	}
    }
  return(f);
}

static s7_pointer rxykcos_chooser(s7_scheme *sc, s7_pointer f, int args, s7_pointer expr)
{
  if ((args == 1) &&
      (s7_is_symbol(cadr(expr))))
    {
      s7_function_choice_set_direct(sc, expr);
      return(rxykcos_1);
    }
  if ((args == 2) &&
      (s7_is_symbol(cadr(expr))) &&
      (s7_is_symbol(caddr(expr))))
    {
      s7_function_choice_set_direct(sc, expr);
      return(rxykcos_2);
    }
  return(f);
}


static s7_pointer rxyksin_chooser(s7_scheme *sc, s7_pointer f, int args, s7_pointer expr)
{
  if ((args == 1) &&
      (s7_is_symbol(cadr(expr))))
    {
      s7_function_choice_set_direct(sc, expr);
      return(rxyksin_1);
    }
  if ((args == 2) &&
      (s7_is_symbol(cadr(expr))) &&
      (s7_is_symbol(caddr(expr))))
    {
      s7_function_choice_set_direct(sc, expr);
      return(rxyksin_2);
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
      if (s7_function_returns_temp(sc, caddr(expr))) 
	{
	  s7_function_choice_set_direct(sc, expr);
	  return(direct_nrxycos_2);
	}
    }
  return(f);
}


static s7_pointer env_chooser(s7_scheme *sc, s7_pointer f, int args, s7_pointer expr)
{
  if (args == 1)
    {
      s7_pointer arg;
      arg = cadr(expr);
      if (s7_is_symbol(arg))
	{
	  if (arg == env_symbol)
	    fprintf(stderr, "(env env) is unlikely to work in Scheme\n");
	  s7_function_choice_set_direct(sc, expr);
	  return(env_1);
	}
      if ((s7_is_pair(arg)) &&
	  (car(arg) == vector_ref_symbol) &&
	  (s7_is_symbol(cadr(arg))) &&
	  (s7_is_symbol(caddr(arg))) &&
	  (s7_is_null(s7, s7_cdddr(arg))))
	{
	  s7_function_choice_set_direct(sc, expr);
	  return(env_vss);
	}
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
      if (s7_function_returns_temp(sc, caddr(expr))) 
	{
	  s7_function_choice_set_direct(sc, expr);
	  return(direct_comb_2);
	}
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
      if (s7_function_returns_temp(sc, caddr(expr)))
	{
	  s7_function_choice_set_direct(sc, expr);
	  return(direct_notch_2);
	}
    }
  return(f);
}


static s7_pointer one_pole_all_pass_chooser(s7_scheme *sc, s7_pointer f, int args, s7_pointer expr)
{
  if ((args == 2) &&
      (s7_is_symbol(cadr(expr))))
    {
      if (s7_is_symbol(caddr(expr)))
	{
	  s7_function_choice_set_direct(sc, expr);
	  return(one_pole_all_pass_2);
	}
    }
  if ((args == 2) &&
      (s7_is_symbol(cadr(expr))) &&
      (s7_is_pair(caddr(expr))) &&
      (s7_function_choice_is_direct(sc, caddr(expr))))
    {
      if (s7_function_returns_temp(sc, caddr(expr))) 
	{
	  s7_function_choice_set_direct(sc, expr);
	  return(direct_one_pole_all_pass_2);
	}
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
    }
  if ((args == 2) &&
      (s7_is_symbol(cadr(expr))) &&
      (s7_is_pair(caddr(expr))) &&
      (s7_function_choice_is_direct(sc, caddr(expr))))
    {
      if (s7_function_returns_temp(sc, caddr(expr))) 
	{
	  s7_function_choice_set_direct(sc, expr);
	  return(direct_all_pass_2);
	}
    }
  return(f);
}


static s7_pointer delay_chooser(s7_scheme *sc, s7_pointer f, int args, s7_pointer expr)
{
  if ((args == 2) &&
      (s7_is_symbol(cadr(expr))))
    {
      if (s7_is_symbol(caddr(expr)))
	{
	  s7_function_choice_set_direct(sc, expr);
	  return(delay_2);
	}
      if ((s7_is_pair(caddr(expr))) &&
	  (s7_function_choice_is_direct(sc, caddr(expr))))
	{
	  if (s7_function_returns_temp(sc, caddr(expr))) 
	    {
	      s7_function_choice_set_direct(sc, expr);
	      return(direct_delay_2);
	    }
	}
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
      if (s7_function_returns_temp(sc, caddr(expr))) 
	{
	  s7_function_choice_set_direct(sc, expr);
	  return(direct_one_pole_2);
	}
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
      if (s7_function_returns_temp(sc, caddr(expr)))
	{
	  s7_function_choice_set_direct(sc, expr);
	  return(direct_two_pole_2);
	}
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
      if (s7_function_returns_temp(sc, caddr(expr))) 
	{
	  s7_function_choice_set_direct(sc, expr);
	  return(direct_one_zero_2);
	}
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
      if (s7_function_returns_temp(sc, caddr(expr))) 
	{
	  s7_function_choice_set_direct(sc, expr);
	  return(direct_two_zero_2);
	}
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
      if (s7_function_returns_temp(sc, caddr(expr))) 
	{
	  s7_function_choice_set_direct(sc, expr);
	  return(direct_moving_average_2);
	}
    }
  return(f);
}

static s7_pointer moving_max_chooser(s7_scheme *sc, s7_pointer f, int args, s7_pointer expr)
{
  if ((args == 2) &&
      (s7_is_symbol(cadr(expr))) &&
      (s7_is_symbol(caddr(expr))))
    {
      s7_function_choice_set_direct(sc, expr);
      return(moving_max_2);
    }
  if ((args == 2) &&
      (s7_is_symbol(cadr(expr))) &&
      (s7_is_pair(caddr(expr))) &&
      (s7_function_choice_is_direct(sc, caddr(expr))))
    {
      if (s7_function_returns_temp(sc, caddr(expr))) 
	{
	  s7_function_choice_set_direct(sc, expr);
	  return(direct_moving_max_2);
	}
    }
  return(f);
}

static s7_pointer moving_norm_chooser(s7_scheme *sc, s7_pointer f, int args, s7_pointer expr)
{
  /* these are not currently used */
  if ((args == 2) &&
      (s7_is_symbol(cadr(expr))) &&
      (s7_is_symbol(caddr(expr))))
    {
      s7_function_choice_set_direct(sc, expr);
      return(moving_norm_2);
    }
  if ((args == 2) &&
      (s7_is_symbol(cadr(expr))) &&
      (s7_is_pair(caddr(expr))) &&
      (s7_function_choice_is_direct(sc, caddr(expr))))
    {
      if (s7_function_returns_temp(sc, caddr(expr))) 
	{
	  s7_function_choice_set_direct(sc, expr);
	  return(direct_moving_norm_2);
	}
    }
  return(f);
}

static s7_pointer formant_chooser(s7_scheme *sc, s7_pointer f, int args, s7_pointer expr)
{
  if (args == 2)
    {
      if ((s7_is_symbol(cadr(expr))) &&
	  (s7_is_symbol(caddr(expr))))
	{
	  s7_function_choice_set_direct(sc, expr);
	  return(formant_2);
	}
      if ((s7_is_symbol(cadr(expr))) &&
	  (s7_is_pair(caddr(expr))) &&
	  (s7_function_choice_is_direct(sc, caddr(expr))))
	{
	  if (s7_function_returns_temp(sc, caddr(expr))) 
	    {
	      s7_function_choice_set_direct(sc, expr);
	      return(direct_formant_2);
	    }
	}
      return(formant_two);
    }
  return(f);
}

static s7_pointer formant_bank_chooser(s7_scheme *sc, s7_pointer f, int args, s7_pointer expr)
{
  if ((args == 2) &&
      (s7_is_symbol(cadr(expr))))
    {
      if (s7_is_symbol(caddr(expr)))
	{
	  s7_function_choice_set_direct(sc, expr);
	  return(formant_bank_ss);
	}
      if ((s7_is_pair(caddr(expr))) &&
	  (s7_function_choice_is_direct(sc, caddr(expr))))
	{
	  s7_function_choice_set_direct(sc, expr);
	  return(formant_bank_sz);
	}
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
      if (s7_function_returns_temp(sc, caddr(expr))) 
	{
	  s7_function_choice_set_direct(sc, expr);
	  return(direct_firmant_2);
	}
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
      if (s7_function_returns_temp(sc, caddr(expr))) 
	{
	  s7_function_choice_set_direct(sc, expr);
	  return(direct_filter_2);
	}
    }
  return(f);
}

static s7_pointer fir_filter_chooser(s7_scheme *sc, s7_pointer f, int args, s7_pointer expr)
{
  s7_pointer arg1;
  arg1 = cadr(expr);
  if ((args == 2) &&
      (s7_is_symbol(arg1)))
    {
      s7_pointer arg2;
      arg2 = caddr(expr);
      if (s7_is_symbol(arg2))
	{
	  s7_function_choice_set_direct(sc, expr);
	  return(fir_filter_2);
	}

      if (s7_is_pair(arg2))
	{
	  if (s7_function_choice_is_direct(sc, arg2))
	    {
	      if (s7_function_returns_temp(sc, caddr(expr))) 
		{
		  s7_function_choice_set_direct(sc, expr);
		  return(direct_fir_filter_2);
		}
	    }
	}
    }

  /* fprintf(stderr, "filter: %s\n", DISPLAY(expr)); */

  /* (fir-filter flt (* scaler (+ (tap del) inval)))
     (hilbert-transform (gen 'hlb) y)
     (fir-filter gen 1.0)
     (fir-filter flt (* scaler (+ (tap del) (if (<= samp input-samps) inval 0.0))))
     (bandpass (vector-ref bands i) input)
     (fir-filter flt (data i))
  */
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
      if (s7_function_returns_temp(sc, caddr(expr))) 
	{
	  s7_function_choice_set_direct(sc, expr);
	  return(direct_iir_filter_2);
	}
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
      if (s7_function_returns_temp(sc, caddr(expr))) 
	{
	  s7_function_choice_set_direct(sc, expr);
	  return(direct_triangle_wave_2);
	}
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
      if (s7_function_returns_temp(sc, caddr(expr))) 
	{
	  s7_function_choice_set_direct(sc, expr);
	  return(direct_sawtooth_wave_2);
	}
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
      if (s7_function_returns_temp(sc, caddr(expr))) 
	{
	  s7_function_choice_set_direct(sc, expr);
	  return(direct_square_wave_2);
	}
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
      if (s7_function_returns_temp(sc, caddr(expr))) 
	{
	  s7_function_choice_set_direct(sc, expr);
	  return(direct_pulse_train_2);
	}
    }
  return(f);
}

static s7_pointer pulsed_env_chooser(s7_scheme *sc, s7_pointer f, int args, s7_pointer expr)
{
  if ((args == 1) &&
      (s7_is_symbol(cadr(expr))))
    {
      s7_function_choice_set_direct(sc, expr);
      return(pulsed_env_1);
    }
  if ((args == 2) &&
      (s7_is_symbol(cadr(expr))) &&
      (s7_is_symbol(caddr(expr))))
    {
      s7_function_choice_set_direct(sc, expr);
      return(pulsed_env_2);
    }
  if ((args == 2) &&
      (s7_is_symbol(cadr(expr))) &&
      (s7_is_pair(caddr(expr))) &&
      (s7_function_choice_is_direct(sc, caddr(expr))))
    {
      if (s7_function_returns_temp(sc, caddr(expr))) 
	{
	  s7_function_choice_set_direct(sc, expr);
	  return(direct_pulsed_env_2);
	}
    }
  return(f);
}


static s7_pointer rand_chooser(s7_scheme *sc, s7_pointer f, int args, s7_pointer expr)
{
  if (args == 1)
    {
      if (s7_is_symbol(cadr(expr)))
	{
	  s7_function_choice_set_direct(sc, expr);
	  return(rand_1);
	}
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
      if (s7_function_returns_temp(sc, caddr(expr))) 
	{
	  s7_function_choice_set_direct(sc, expr);
	  return(direct_rand_2);
	}
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
      if (s7_function_returns_temp(sc, caddr(expr))) 
	{
	  s7_function_choice_set_direct(sc, expr);
	  return(direct_rand_interp_2);
	}
    }
  return(f);
}

static s7_pointer polynomial_temp;
static s7_pointer g_polynomial_temp(s7_scheme *sc, s7_pointer args)
{
  vct *v;
  s7_pointer vc;
  vc = s7_car_value(sc, args);
  v = xen_to_vct(vc);
  if (v)
    return(s7_make_real(sc, mus_polynomial(mus_vct_data(v), s7_call_direct_to_real_and_free(sc, cadr(args)), mus_vct_length(v))));
  return(g_polynomial(vc, s7_call_direct(sc, cadr(args))));
}

static s7_pointer polynomial_cos;
static s7_pointer g_polynomial_cos(s7_scheme *sc, s7_pointer args)
{
  vct *v;
  s7_pointer vc, cs;
  vc = s7_car_value(sc, args);
  cs = s7_cadr_value(sc, cadr(args));
  v = xen_to_vct(vc);
  if (v)
    return(s7_make_real(sc, mus_polynomial(mus_vct_data(v), cos(s7_number_to_real_with_caller(sc, cs, "cos")), mus_vct_length(v))));
  return(g_polynomial(vc, s7_cos(sc, cs)));
}

static s7_pointer polynomial_chooser(s7_scheme *sc, s7_pointer f, int args, s7_pointer expr)
{
  /* these choices are just placeholders to trigger further optimizations later -- the code above is not used. */
  if (args == 2)
    {
      if ((s7_is_symbol(cadr(expr))) &&
	  (s7_is_pair(caddr(expr))))
	{
	  if ((s7_function_choice_is_direct(sc, caddr(expr))) &&
	      (s7_function_returns_temp(sc, caddr(expr))))
	    {
	      /* this is not currently used anywhere */
	      s7_function_choice_set_direct(sc, expr);
	      return(polynomial_temp);
	    }
	  if ((car(caddr(expr)) == cos_symbol) &&
	      (s7_is_symbol(cadr(caddr(expr)))))
	    {
	      /* aimed, I think, at (polynomial coeffs (cos x)) which is now built-in, so this is not used
	       */
	      s7_function_choice_set_direct(sc, expr);
	      return(polynomial_cos);
	    }
	}
    }
  return(f);
}

static s7_pointer polyshape_chooser(s7_scheme *sc, s7_pointer f, int args, s7_pointer expr)
{
  if (args == 2)
    {
      if ((s7_is_symbol(cadr(expr))) &&
	  (s7_is_symbol(caddr(expr))))
	{
	  s7_function_choice_set_direct(sc, expr);
	  return(polyshape_2);
	}
      if ((s7_is_symbol(cadr(expr))) &&
	  (s7_is_pair(caddr(expr))) &&
	  (s7_function_choice_is_direct(sc, caddr(expr))))
	{
	  if (s7_function_returns_temp(sc, caddr(expr))) 
	    {
	      s7_function_choice_set_direct(sc, expr);
	      return(direct_polyshape_2);
	    }
	}
    }
  return(f);
}

static s7_pointer ssb_am_chooser(s7_scheme *sc, s7_pointer f, int args, s7_pointer expr)
{
  if ((args == 2) &&
      (s7_is_symbol(cadr(expr))))
    {
      if (s7_is_symbol(caddr(expr)))
	{
	  s7_function_choice_set_direct(sc, expr);
	  return(ssb_am_2);
	}
      if ((s7_is_pair(caddr(expr))) &&
	  (s7_function_choice_is_direct(sc, caddr(expr))) &&
	  (s7_function_returns_temp(sc, caddr(expr))))
	{
	  s7_function_choice_set_direct(sc, expr);
	  return(direct_ssb_am_2);
	}
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
      if (s7_function_returns_temp(sc, caddr(expr))) 
	{
	  s7_function_choice_set_direct(sc, expr);
	  return(direct_asymmetric_fm_2);
	}
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
      if (s7_function_returns_temp(sc, caddr(expr))) 
	{
	  s7_function_choice_set_direct(sc, expr);
	  return(direct_filtered_comb_2);
	}
    }
  return(f);
}


static s7_pointer locsig_chooser(s7_scheme *sc, s7_pointer f, int args, s7_pointer expr)
{
  if ((args == 3) &&
      (s7_is_symbol(cadr(expr))) &&
      (s7_is_symbol(caddr(expr))) &&
      (s7_is_pair(cadr(cddr(expr)))))
    {
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
  /* fprintf(stderr, "\nouta: %s\n", DISPLAY(expr)); */

  if (args == 2)
    {
      s7_pointer arg1, arg2;
      arg1 = cadr(expr);
      arg2 = caddr(expr);

      if (s7_is_symbol(arg1))
	{
	  if (s7_is_symbol(arg2))
	    {
	      /* never happens in a simple looped context (also outa_ss below) */
	      s7_function_choice_set_direct(sc, expr);
	      return(indirect_outa_ss);
	    }

	  if (s7_is_pair(arg2))     /* (outa i (...)) */
	    {
	      if ((car(arg2) == multiply_symbol) &&
		  (s7_list_length(sc, arg2) == 3) &&
		  (s7_is_symbol(cadr(arg2))) &&
		  (s7_is_symbol(caddr(arg2))))
		{
		  s7_function_choice_set_direct(sc, expr);
		  return(outa_ss);
		}
	      if (s7_function_choice_is_direct(sc, arg2))
		{
		  s7_function_choice_set_direct(sc, expr);
		  if (s7_function_choice(sc, arg2) == g_mul_env_direct)
		    return(indirect_outa_2_env);
		  
		  if (s7_function_returns_temp(sc, arg2))
		    return(indirect_outa_2_temp);
		  return(indirect_outa_2);
		}
	    }
	}
      /* fprintf(stderr, "outa two: %s\n", DISPLAY(expr)); */
      return(outa_two);
    }
  /* fprintf(stderr, "\nouta nope: %s\n", DISPLAY(expr)); */
  return(f);
}

static s7_pointer outb_chooser(s7_scheme *sc, s7_pointer f, int args, s7_pointer expr)
{
  if (args == 2)
    return(outb_two);
  return(f);
}


static s7_pointer out_any_chooser(s7_scheme *sc, s7_pointer f, int args, s7_pointer expr)
{
  if (args == 3)
    {
      s7_pointer arg3;
      arg3 = cadddr(expr);
      if (s7_is_integer(arg3))
	{
	  s7_int c;
	  c = s7_integer(arg3);
	  if (c == 0)
	    return(outa_chooser(sc, f, 2, expr));
	  if (c == 1)
	    return(outb_chooser(sc, f, 2, expr));
	}
    }
  return(f);
}


static s7_pointer ina_chooser(s7_scheme *sc, s7_pointer f, int args, s7_pointer expr)
{
  if (args == 2)
    {
      if ((s7_is_symbol(cadr(expr))) &&
	  (s7_is_symbol(caddr(expr))))
	{
	  s7_function_choice_set_direct(sc, expr);
	  return(ina_ss);
	}
    }
  return(f);
}

/* (file->sample obj loc chan) -> (ina loc obj)?? only ina_ss but that requires expr not be a temp? */
static s7_pointer file_to_sample_chooser(s7_scheme *sc, s7_pointer f, int args, s7_pointer expr)
{
  if ((args == 2) ||
      ((args == 3) &&
       (s7_is_integer(cadddr(expr))) &&
       (s7_integer(cadddr(expr)) == 0)))
    {
      if ((s7_is_symbol(cadr(expr))) &&
	  (s7_is_symbol(caddr(expr))))
	{
	  s7_function_choice_set_direct(sc, expr);
	  return(file_to_sample_ss);
	}
    }
  return(f);
}


static s7_pointer clm_make_temp_function(s7_scheme *sc, const char *name, s7_function f, 
					 int required_args, int optional_args, bool rest_arg, const char *doc,
					 s7_pointer base_f,
					 s7_pointer mul_c, 
					 s7_pointer mul_s, 
					 s7_pointer e, 
					 s7_pointer mul_c1,
					 s7_pointer mul_s1, 
					 s7_pointer e1)
{		
  s7_pointer fin;
  /* s7_make_safe_function works here and elsewhere (but not for symbol accessors perhaps), but is
   *   a no-op -- it is the base function that the optimizer checks for safety, not the chooser-related stuff.
   */
  fin = s7_make_function(sc, name, f, required_args, optional_args, rest_arg, doc);
  s7_function_set_class(fin, base_f);
  s7_function_chooser_set_data(sc, fin, (void *)make_choices(mul_c, mul_s, e, mul_c1, mul_s1, e1));
  s7_function_set_returns_temp(fin);
  return(fin);
}

static s7_pointer clm_make_temp_function_no_choice(s7_scheme *sc, const char *name, s7_function f, 
						   int required_args, int optional_args, bool rest_arg, const char *doc,
						   s7_pointer base_f)
{		
  s7_pointer fin;
  fin = s7_make_function(sc, name, f, required_args, optional_args, rest_arg, doc);
  s7_function_set_class(fin, base_f);
  s7_function_set_returns_temp(fin);
  return(fin);
}

static s7_pointer clm_make_function_no_choice(s7_scheme *sc, const char *name, s7_function f, 
					      int required_args, int optional_args, bool rest_arg, const char *doc,
					      s7_pointer base_f)
{		
  s7_pointer fin;
  fin = s7_make_function(sc, name, f, required_args, optional_args, rest_arg, doc);
  s7_function_set_class(fin, base_f);
  /* s7_function_set_returns_temp(fin); */ /* mul_s_temp etc -- not necessarily a temp, adding this makes only a small difference in speed */
  return(fin);
}


static void init_choosers(s7_scheme *sc)
{
  s7_pointer f;

  abs_symbol = s7_make_symbol(sc, "abs");
  env_symbol = s7_make_symbol(sc, "env");
  vector_ref_symbol = s7_make_symbol(sc, "vector-ref");
  all_pass_symbol = s7_make_symbol(sc, "all-pass");
  one_pole_symbol = s7_make_symbol(sc, "one-pole");
  ina_symbol = s7_make_symbol(sc, S_ina);
  polywave_symbol = s7_make_symbol(sc, "polywave");
  triangle_wave_symbol = s7_make_symbol(sc, "triangle-wave");
  rand_interp_symbol = s7_make_symbol(sc, "rand-interp");
  oscil_symbol = s7_make_symbol(sc, "oscil");
  add_symbol = s7_make_symbol(sc, "+");
  subtract_symbol = s7_make_symbol(sc, "-");
  multiply_symbol = s7_make_symbol(sc, "*");
  reverb_symbol = s7_make_symbol(sc, "*reverb*");
  output_symbol = s7_make_symbol(sc, "*output*");
  outa_symbol = s7_make_symbol(sc, S_outa);
  quote_symbol = s7_make_symbol(sc, "quote");
  sin_symbol = s7_make_symbol(sc, "sin");
  cos_symbol = s7_make_symbol(sc, "cos");
  readin_symbol = s7_make_symbol(sc, "readin");
  comb_bank_symbol = s7_make_symbol(sc, "comb-bank");                   
  all_pass_bank_symbol = s7_make_symbol(sc, "all-pass-bank");
  mus_copy_symbol = s7_make_symbol(sc, "mus-copy");
  copy_function = s7_name_to_value(sc, "copy");

  sym_frequency = s7_make_symbol(sc, S_mus_frequency);
  sym_phase = s7_make_symbol(sc, S_mus_phase);
  sym_scaler = s7_make_symbol(sc, S_mus_scaler);
  sym_increment = s7_make_symbol(sc, S_mus_increment);
  sym_width = s7_make_symbol(sc, S_mus_width);
  sym_offset = s7_make_symbol(sc, S_mus_offset);
  sym_feedforward = s7_make_symbol(sc, S_mus_feedforward);
  sym_feedback = s7_make_symbol(sc, S_mus_feedback);

  f = s7_name_to_value(sc, "*");
  initial_multiply_chooser = s7_function_chooser(sc, f);
  s7_function_set_chooser(sc, f, clm_multiply_chooser);
  initial_multiply_rf = s7_rf_function(sc, f);
  s7_rf_set_function(f, clm_multiply_rf);

  mul_direct_2 = clm_make_temp_function_no_choice(sc, "*", g_mul_direct_2, 2, 0, false, "* opt", f);
  mul_env_direct = clm_make_temp_function_no_choice(sc, "*", g_mul_env_direct, 2, 0, false, "* opt", f);
  env_polywave_env = clm_make_temp_function_no_choice(sc, "*", g_env_polywave_env, 2, 0, false, "animals opt", f);
  mul_c_oscil_2 = clm_make_temp_function_no_choice(sc, "*", g_mul_c_oscil_2, 2, 0, false, "* opt", f);

  mul_s_oscil_2 = clm_make_function_no_choice(sc, "*", g_mul_s_oscil_2, 2, 0, false, "* opt", f);
  mul_s_oscil_1 = clm_make_function_no_choice(sc, "*", g_mul_s_oscil_1, 1, 0, false, "* opt", f);
  mul_s_env_1 = clm_make_function_no_choice(sc, "*", g_mul_s_env_1, 1, 0, false, "* opt", f);
  mul_s_granulate_1 = clm_make_function_no_choice(sc, "*", g_mul_s_granulate_1, 1, 0, false, "* opt", f);
  mul_s_rand_interp_1 = clm_make_function_no_choice(sc, "*", g_mul_s_rand_interp_1, 1, 0, false, "* opt", f);

  env_oscil_2 = clm_make_temp_function_no_choice(sc, "*", g_env_oscil_2, 2, 0, false, "* opt", f);
  env_oscil_1 = clm_make_temp_function_no_choice(sc, "*", g_env_oscil_1, 1, 0, false, "* opt", f);
  env_oscil_bank_1 = clm_make_temp_function_no_choice(sc, "*", g_env_oscil_bank_1, 1, 0, false, "* opt", f);
  env_env_1 = clm_make_temp_function_no_choice(sc, "*", g_env_env_1, 1, 0, false, "* opt", f);
  env_readin_1 = clm_make_temp_function_no_choice(sc, "*", g_env_readin_1, 1, 0, false, "* opt", f);
  env_polywave_1 = clm_make_temp_function_no_choice(sc, "*", g_env_polywave_1, 1, 0, false, "* opt", f);
  env_table_lookup_1 = clm_make_temp_function_no_choice(sc, "*", g_env_table_lookup_1, 1, 0, false, "* opt", f);
  env_wave_train_2 = clm_make_temp_function_no_choice(sc, "*", g_env_wave_train_2, 2, 0, false, "* opt", f);
  env_wave_train_1 = clm_make_temp_function_no_choice(sc, "*", g_env_wave_train_1, 1, 0, false, "* opt", f);
  env_src_1 = clm_make_temp_function_no_choice(sc, "*", g_env_src_1, 1, 0, false, "* opt", f);
  env_phase_vocoder_1 = clm_make_temp_function_no_choice(sc, "*", g_env_phase_vocoder_1, 1, 0, false, "* opt", f);
  env_convolve_1 = clm_make_temp_function_no_choice(sc, "*", g_env_convolve_1, 1, 0, false, "* opt", f);
  env_granulate_1 = clm_make_temp_function_no_choice(sc, "*", g_env_granulate_1, 1, 0, false, "* opt", f);
  env_rand_1 = clm_make_temp_function_no_choice(sc, "*", g_env_rand_1, 1, 0, false, "* opt", f);
  env_rand_interp_1 = clm_make_temp_function_no_choice(sc, "*", g_env_rand_interp_1, 1, 0, false, "* opt", f);
  env_ncos_1 = clm_make_temp_function_no_choice(sc, "*", g_env_ncos_1, 1, 0, false, "* opt", f);
  env_nsin_1 = clm_make_temp_function_no_choice(sc, "*", g_env_nsin_1, 1, 0, false, "* opt", f);
  env_sawtooth_wave_1 = clm_make_temp_function_no_choice(sc, "*", g_env_sawtooth_wave_1, 1, 0, false, "* opt", f);
  env_pulse_train_1 = clm_make_temp_function_no_choice(sc, "*", g_env_pulse_train_1, 1, 0, false, "* opt", f);
  env_pulsed_env_1 = clm_make_temp_function_no_choice(sc, "*", g_env_pulsed_env_1, 1, 0, false, "* opt", f);
  env_square_wave_1 = clm_make_temp_function_no_choice(sc, "*", g_env_square_wave_1, 1, 0, false, "* opt", f);
  env_triangle_wave_1 = clm_make_temp_function_no_choice(sc, "*", g_env_triangle_wave_1, 1, 0, false, "* opt", f);
  env_nrxysin_1 = clm_make_temp_function_no_choice(sc, "*", g_env_nrxysin_1, 1, 0, false, "* opt", f);
  env_nrxycos_1 = clm_make_temp_function_no_choice(sc, "*", g_env_nrxycos_1, 1, 0, false, "* opt", f);
  env_rxyksin_1 = clm_make_temp_function_no_choice(sc, "*", g_env_rxyksin_1, 1, 0, false, "* opt", f);
  env_rxykcos_1 = clm_make_temp_function_no_choice(sc, "*", g_env_rxykcos_1, 1, 0, false, "* opt", f);


  f = s7_name_to_value(sc, "+");
  initial_add_chooser = s7_function_chooser(sc, f);
  s7_function_set_chooser(sc, f, clm_add_chooser);

  fm_violin_vibrato = clm_make_temp_function_no_choice(sc, "+", g_fm_violin_vibrato, 3, 0, false, "fm-violin opt", f);
  add_direct_2 = clm_make_temp_function_no_choice(sc, "+", g_add_direct_2, 2, 0, false, "+ opt", f);
  add_env_direct_2 = clm_make_temp_function_no_choice(sc, "+", g_add_env_direct_2, 2, 0, false, "+ opt", f);
  add_env_direct_3 = clm_make_temp_function_no_choice(sc, "+", g_add_env_direct_3, 3, 0, false, "+ opt", f);
  add_direct_any = clm_make_temp_function_no_choice(sc, "+", g_add_direct_any, 3, 0, true, "+ opt", f);
  add_c_direct = clm_make_temp_function_no_choice(sc, "+", g_add_c_direct, 2, 0, false, "+ opt", f);
  add_c_rand_interp = clm_make_temp_function_no_choice(sc, "+", g_add_c_rand_interp, 2, 0, false, "+ opt", f);



#define GEN_F(Name, Type)				\
  f = s7_name_to_value(sc, Name);			\
  s7_function_set_chooser(sc, f, Type ## _chooser);			\
  s7_rf_set_function(f, is_ ## Type ## _rf); \
  store_choices(sc, f, wrapped_ ## Type ## _1, wrapped_ ## Type ## _2, NULL, wrapped_ ## Type ## _p);
  
#define GEN_F1(Name, Type)				\
  f = s7_name_to_value(sc, Name);			\
  s7_function_set_chooser(sc, f, Type ## _chooser);			\
  s7_rf_set_function(f, is_ ## Type ## _rf); \
  store_choices(sc, f, wrapped_ ## Type ## _1, NULL, NULL, wrapped_ ## Type ## _p);

#define GEN_F3(Name, Type)				\
  f = s7_name_to_value(sc, Name);			\
  s7_function_set_chooser(sc, f, Type ## _chooser);			\
  s7_rf_set_function(f, is_ ## Type ## _rf); \
  store_choices(sc, f, wrapped_ ## Type ## _1, wrapped_ ## Type ## _2, wrapped_ ## Type ## _3, wrapped_ ## Type ## _p);


  /* oscil */
  GEN_F3(S_oscil, oscil);
  /* override 2-arg rf case for 3-arg additions */
  s7_rf_set_function(f, is_oscil_rf_3);

  oscil_1 = clm_make_temp_function(sc, S_oscil, g_oscil_1, 1, 0, false, "oscil opt", f, NULL, NULL, NULL, NULL, mul_s_oscil_1, env_oscil_1);
  oscil_2 = clm_make_temp_function(sc, S_oscil, g_oscil_2, 2, 0, false, "oscil opt", f, mul_c_oscil_2, mul_s_oscil_2, env_oscil_2, NULL, NULL, NULL);

  direct_oscil_2 = clm_make_temp_function_no_choice(sc, S_oscil, g_direct_oscil_2, 2, 0, false, "oscil opt", f);
  oscil_one = clm_make_temp_function_no_choice(sc, S_oscil, g_oscil_one, 1, 0, false, "oscil opt", f);
  oscil_two = clm_make_temp_function_no_choice(sc, S_oscil, g_oscil_two, 2, 0, false, "oscil opt", f);
  oscil_mul_c_s = clm_make_temp_function_no_choice(sc, S_oscil, g_oscil_mul_c_s, 2, 0, false, "oscil opt", f);
  oscil_mul_ss = clm_make_temp_function(sc, S_oscil, g_oscil_mul_ss, 2, 0, false, "oscil opt", f,  NULL, NULL, NULL, NULL, NULL, NULL);
  oscil_vss_s = clm_make_temp_function_no_choice(sc, S_oscil, g_oscil_vss_s, 2, 0, false, "oscil opt", f);


  GEN_F1("oscil-bank", oscil_bank);
  oscil_bank_1 = clm_make_temp_function(sc, "oscil-bank", g_oscil_bank_1, 1, 0, false, "oscil-bank opt", f,  
					NULL, NULL, NULL, NULL, NULL, env_oscil_bank_1);


  /* polywave */
  GEN_F(S_polywave, polywave);

  polywave_1 = clm_make_temp_function(sc, S_polywave, g_polywave_1, 1, 0, false, "polywave opt", f,
				      NULL, NULL, NULL, NULL, NULL, env_polywave_1);
  polywave_2 = clm_make_temp_function(sc, S_polywave, g_polywave_2, 2, 0, false, "polywave opt", f,
				      NULL, NULL, NULL, NULL, NULL, NULL);

  direct_polywave_2 = clm_make_temp_function_no_choice(sc, S_polywave, g_direct_polywave_2, 2, 0, false, "polywave opt", f);

  /* table-lookup */
  GEN_F(S_table_lookup, table_lookup);

  table_lookup_1 = clm_make_temp_function(sc, S_table_lookup, g_table_lookup_1, 1, 0, false, "table-lookup opt", f,
					  NULL, NULL, NULL, NULL, NULL, env_table_lookup_1);
  table_lookup_2 = clm_make_temp_function(sc, S_table_lookup, g_table_lookup_2, 2, 0, false, "table-lookup opt", f,
					  NULL, NULL, NULL, NULL, NULL, NULL);
  direct_table_lookup_2 = clm_make_temp_function_no_choice(sc, S_table_lookup, g_direct_table_lookup_2, 2, 0, false, "table-lookup opt", f);


  /* wave-train */
  GEN_F(S_wave_train, wave_train);

  wave_train_1 = clm_make_temp_function(sc, S_wave_train, g_wave_train_1, 1, 0, false, "wave-train opt", f,
					NULL, NULL, NULL, NULL, NULL, env_wave_train_1);
  wave_train_2 = clm_make_temp_function(sc, S_wave_train, g_wave_train_2, 2, 0, false, "wave-train opt", f,
					NULL, NULL, env_wave_train_2, NULL, NULL, NULL);
  direct_wave_train_2 = clm_make_temp_function_no_choice(sc, S_wave_train, g_direct_wave_train_2, 2, 0, false, "wave-train opt", f);


  /* src */
  GEN_F(S_src, src);

  src_1 = clm_make_temp_function(sc, S_src, g_src_1, 1, 0, false, "src opt", f, NULL, NULL, NULL, NULL, NULL, env_src_1);

  src_2 = clm_make_temp_function(sc, S_src, g_src_2, 2, 0, false, "src opt", f, NULL, NULL, NULL, NULL, NULL, NULL);
  direct_src_2 = clm_make_temp_function_no_choice(sc, S_src, g_direct_src_2, 2, 0, false, "src opt", f);


  /* phase_vocoder */
  GEN_F1("phase-vocoder", phase_vocoder);

  phase_vocoder_1 = clm_make_temp_function(sc, "phase-vocoder", g_phase_vocoder_1, 1, 0, false, "phase-vocoder opt", f,
					   NULL, NULL, NULL, NULL, NULL, env_phase_vocoder_1);


  /* convolve */
  GEN_F1("convolve", convolve);

  convolve_1 = clm_make_temp_function(sc, "convolve", g_convolve_1, 1, 0, false, "convolve opt", f,
				      NULL, NULL, NULL, NULL, NULL, env_convolve_1);


  /* granulate */
  GEN_F1("granulate", granulate);

  granulate_1 = clm_make_temp_function(sc, "granulate", g_granulate_1, 1, 0, false, "granulate opt", f,
				       NULL, NULL, NULL, NULL, mul_s_granulate_1, env_granulate_1);


  GEN_F(S_nsin, nsin);

  nsin_2 = clm_make_temp_function(sc, S_nsin, g_nsin_2, 2, 0, false, "nsin opt", f, NULL, NULL, NULL, NULL, NULL, NULL);
  nsin_1 = clm_make_temp_function(sc, S_nsin, g_nsin_1, 1, 0, false, "nsin opt", f, NULL, NULL, NULL, NULL, NULL, env_nsin_1);			     
  direct_nsin_2 = clm_make_temp_function_no_choice(sc, S_nsin, g_direct_nsin_2, 2, 0, false, "nsin opt", f);


  GEN_F(S_ncos, ncos);

  ncos_2 = clm_make_temp_function(sc, S_ncos, g_ncos_2, 2, 0, false, "ncos opt", f, NULL, NULL, NULL, NULL, NULL, NULL);
  direct_ncos_2 = clm_make_temp_function_no_choice(sc, S_ncos, g_direct_ncos_2, 2, 0, false, "ncos opt", f);
  ncos_1 = clm_make_temp_function(sc, S_ncos, g_ncos_1, 1, 0, false, "ncos opt", f, NULL, NULL, NULL, NULL, NULL, env_ncos_1);


  GEN_F(S_nrxysin, nrxysin);

  nrxysin_2 = clm_make_temp_function(sc, S_nrxysin, g_nrxysin_2, 2, 0, false, "nrxysin opt", f,
				     NULL, NULL, NULL, NULL, NULL, NULL);
  direct_nrxysin_2 = clm_make_temp_function_no_choice(sc, S_nrxysin, g_direct_nrxysin_2, 2, 0, false, "nrxysin opt", f);
  nrxysin_1 = clm_make_temp_function(sc, S_nrxysin, g_nrxysin_1, 1, 0, false, "nrxysin opt", f,
				     NULL, NULL, NULL, NULL, NULL, env_nrxysin_1);

  GEN_F(S_nrxycos, nrxycos);

  nrxycos_2 = clm_make_temp_function(sc, S_nrxycos, g_nrxycos_2, 2, 0, false, "nrxycos opt", f,
				     NULL, NULL, NULL, NULL, NULL, NULL);
  direct_nrxycos_2 = clm_make_temp_function_no_choice(sc, S_nrxycos, g_direct_nrxycos_2, 2, 0, false, "nrxycos opt", f);
  nrxycos_1 = clm_make_temp_function(sc, S_nrxycos, g_nrxycos_1, 1, 0, false, "nrxycos opt", f,
				     NULL, NULL, NULL, NULL, NULL, env_nrxycos_1);


  GEN_F("rxyk!sin", rxyksin);

  rxyksin_2 = clm_make_temp_function(sc, "rxyk!sin", g_rxyksin_2, 2, 0, false, "rxyk!sin opt", f,
				     NULL, NULL, NULL, NULL, NULL, NULL);
  direct_rxyksin_2 = clm_make_temp_function_no_choice(sc, "rxyk!sin", g_direct_rxyksin_2, 2, 0, false, "rxyk!sin opt", f);
  rxyksin_1 = clm_make_temp_function(sc, "rxyk!sin", g_rxyksin_1, 1, 0, false, "rxyk!sin opt", f,
				     NULL, NULL, NULL, NULL, NULL, env_rxyksin_1);


  GEN_F("rxyk!cos", rxykcos);

  rxykcos_2 = clm_make_temp_function(sc, "rxyk!cos", g_rxykcos_2, 2, 0, false, "rxykcos opt", f,
				     NULL, NULL, NULL, NULL, NULL, NULL);
  direct_rxykcos_2 = clm_make_temp_function_no_choice(sc, "rxyk!cos", g_direct_rxykcos_2, 2, 0, false, "rxyk!cos opt", f);
  rxykcos_1 = clm_make_temp_function(sc, "rxyk!cos", g_rxykcos_1, 1, 0, false, "rxyk!cos opt", f,
				     NULL, NULL, NULL, NULL, NULL, env_rxykcos_1);


  GEN_F1("env", env);
  env_1 = clm_make_temp_function(sc, "env", g_env_1, 1, 0, false, "env opt", f, NULL, NULL, NULL, NULL, mul_s_env_1, env_env_1);
  env_vss = clm_make_temp_function_no_choice(sc, "env", g_env_vss, 1, 0, false, "env opt", f);


  GEN_F1("readin", readin);
  readin_1 = clm_make_temp_function(sc, "readin", g_readin_1, 1, 0, false, "readin opt", f,
				    NULL, NULL, NULL, NULL, NULL, env_readin_1);


  f = s7_name_to_value(sc, "sample->file");
  s7_rf_set_function(f, is_sample_to_file_rf);

  f = s7_name_to_value(sc, "tap");
  s7_rf_set_function(f, is_tap_rf);
  store_choices(sc, f, wrapped_tap_1, NULL, NULL, wrapped_tap_p);


  GEN_F3("comb", comb);
  s7_rf_set_function(f, is_comb_rf_3);

  comb_2 = clm_make_temp_function(sc, "comb", g_comb_2, 2, 0, false, "comb opt", f, NULL, NULL, NULL, NULL, NULL, NULL);
  direct_comb_2 = clm_make_temp_function_no_choice(sc, "comb", g_direct_comb_2, 2, 0, false, "comb opt", f);


  GEN_F("comb-bank", comb_bank);

  comb_bank_2 = clm_make_temp_function(sc, "comb-bank", g_comb_bank_2, 2, 0, false, "comb-bank opt", f, 
				       NULL, NULL, NULL, NULL, NULL, NULL);
  direct_comb_bank_2 = clm_make_temp_function_no_choice(sc, "comb-bank", g_direct_comb_bank_2, 2, 0, false, "comb-bank opt", f);


  GEN_F3("notch", notch);

  notch_2 = clm_make_temp_function(sc, "notch", g_notch_2, 2, 0, false, "notch opt", f, NULL, NULL, NULL, NULL, NULL, NULL);
  direct_notch_2 = clm_make_temp_function_no_choice(sc, "notch", g_direct_notch_2, 2, 0, false, "notch opt", f);

  GEN_F("one-pole", one_pole);

  one_pole_2 = clm_make_temp_function(sc, "one-pole", g_one_pole_2, 2, 0, false, "one-pole opt", f,
				      NULL, NULL, NULL, NULL, NULL, NULL);
  direct_one_pole_2 = clm_make_temp_function_no_choice(sc, "one-pole", g_direct_one_pole_2, 2, 0, false, "one-pole opt", f);


  GEN_F("two-pole", two_pole);

  two_pole_2 = clm_make_temp_function(sc, "two-pole", g_two_pole_2, 2, 0, false, "two-pole opt", f,
				      NULL, NULL, NULL, NULL, NULL, NULL);
  direct_two_pole_2 = clm_make_temp_function_no_choice(sc, "two-pole", g_direct_two_pole_2, 2, 0, false, "two-pole opt", f);


  GEN_F("one-zero", one_zero);

  one_zero_2 = clm_make_temp_function(sc, "one-zero", g_one_zero_2, 2, 0, false, "one-zero opt", f,
				      NULL, NULL, NULL, NULL, NULL, NULL);
  direct_one_zero_2 = clm_make_temp_function_no_choice(sc, "one-zero", g_direct_one_zero_2, 2, 0, false, "one-zero opt", f);


  GEN_F("two-zero", two_zero);

  two_zero_2 = clm_make_temp_function(sc, "two-zero", g_two_zero_2, 2, 0, false, "two-zero opt", f,
				      NULL, NULL, NULL, NULL, NULL, NULL);
  direct_two_zero_2 = clm_make_temp_function_no_choice(sc, "two-zero", g_direct_two_zero_2, 2, 0, false, "two-zero opt", f);

  GEN_F("moving-average", moving_average);

  moving_average_2 = clm_make_temp_function(sc, "moving-average", g_moving_average_2, 2, 0, false, "moving-average opt", f,
					    NULL, NULL, NULL, NULL, NULL, NULL);
  direct_moving_average_2 = clm_make_temp_function_no_choice(sc, "moving-average", g_direct_moving_average_2, 2, 0, false, "moving-average opt", f);

  GEN_F("moving-max", moving_max);

  moving_max_2 = clm_make_temp_function(sc, "moving-max", g_moving_max_2, 2, 0, false, "moving-max opt", f,
					NULL, NULL, NULL, NULL, NULL, NULL);
  direct_moving_max_2 = clm_make_temp_function_no_choice(sc, "moving-max", g_direct_moving_max_2, 2, 0, false, "moving-max opt", f);

  GEN_F("moving-norm", moving_norm);

  moving_norm_2 = clm_make_temp_function(sc, "moving-norm", g_moving_norm_2, 2, 0, false, "moving-norm opt", f,
					 NULL, NULL, NULL, NULL, NULL, NULL);
  direct_moving_norm_2 = clm_make_temp_function_no_choice(sc, "moving-norm", g_direct_moving_norm_2, 2, 0, false, "moving-norm opt", f);

  GEN_F("filter", filter);

  filter_2 = clm_make_temp_function(sc, "filter", g_filter_2, 2, 0, false, "filter opt", f,
				    NULL, NULL, NULL, NULL, NULL, NULL);
  direct_filter_2 = clm_make_temp_function_no_choice(sc, "filter", g_direct_filter_2, 2, 0, false, "filter opt", f);


  GEN_F("fir-filter", fir_filter);

  fir_filter_2 = clm_make_temp_function(sc, "fir-filter", g_fir_filter_2, 2, 0, false, "fir-filter opt", f,
					NULL, NULL, NULL, NULL, NULL, NULL);
  direct_fir_filter_2 = clm_make_temp_function_no_choice(sc, "fir-filter", g_direct_fir_filter_2, 2, 0, false, "fir-filter opt", f);

  GEN_F("iir-filter", iir_filter);

  iir_filter_2 = clm_make_temp_function(sc, "iir-filter", g_iir_filter_2, 2, 0, false, "iir-filter opt", f,
					NULL, NULL, NULL, NULL, NULL, NULL);
  direct_iir_filter_2 = clm_make_temp_function_no_choice(sc, "iir-filter", g_direct_iir_filter_2, 2, 0, false, "iir-filter opt", f);

  GEN_F("triangle-wave", triangle_wave);

  triangle_wave_2 = clm_make_temp_function(sc, "triangle-wave", g_triangle_wave_2, 2, 0, false, "triangle-wave opt", f,
					   NULL, NULL, NULL, NULL, NULL, NULL);
  direct_triangle_wave_2 = clm_make_temp_function_no_choice(sc, "triangle-wave", g_direct_triangle_wave_2, 2, 0, false, "triangle-wave opt", f);
  triangle_wave_1 = clm_make_temp_function(sc, "triangle-wave", g_triangle_wave_1, 1, 0, false, "triangle-wave opt", f,
					   NULL, NULL, NULL, NULL, NULL, env_triangle_wave_1);

  GEN_F("sawtooth-wave", sawtooth_wave);

  sawtooth_wave_2 = clm_make_temp_function(sc, "sawtooth-wave", g_sawtooth_wave_2, 2, 0, false, "sawtooth-wave opt", f,
					   NULL, NULL, NULL, NULL, NULL, NULL);
  direct_sawtooth_wave_2 = clm_make_temp_function_no_choice(sc, "sawtooth-wave", g_direct_sawtooth_wave_2, 2, 0, false, "sawtooth-wave opt", f);
  sawtooth_wave_1 = clm_make_temp_function(sc, "sawtooth-wave", g_sawtooth_wave_1, 1, 0, false, "sawtooth-wave opt", f,
					   NULL, NULL, NULL, NULL, NULL, env_sawtooth_wave_1);

  GEN_F("square-wave", square_wave);

  square_wave_2 = clm_make_temp_function(sc, "square-wave", g_square_wave_2, 2, 0, false, "square-wave opt", f,
					 NULL, NULL, NULL, NULL, NULL, NULL);
  direct_square_wave_2 = clm_make_temp_function_no_choice(sc, "square-wave", g_direct_square_wave_2, 2, 0, false, "square-wave opt", f);
  square_wave_1 = clm_make_temp_function(sc, "square-wave", g_square_wave_1, 1, 0, false, "square-wave opt", f,
					 NULL, NULL, NULL, NULL, NULL, env_square_wave_1);

  GEN_F("pulse-train", pulse_train);

  pulse_train_2 = clm_make_temp_function(sc, "pulse-train", g_pulse_train_2, 2, 0, false, "pulse-train opt", f,
					 NULL, NULL, NULL, NULL, NULL, NULL);
  direct_pulse_train_2 = clm_make_temp_function_no_choice(sc, "pulse-train", g_direct_pulse_train_2, 2, 0, false, "pulse-train opt", f);
  pulse_train_1 = clm_make_temp_function(sc, "pulse-train", g_pulse_train_1, 1, 0, false, "pulse-train opt", f,
					 NULL, NULL, NULL, NULL, NULL, env_pulse_train_1);


  f = s7_name_to_value(sc, "mus-random");
  direct_choice_1(sc, f, (mus_float_t (*)(mus_xen *))wrapped_mus_random_1, wrapped_mus_random_p);


  GEN_F("rand", rand);

  rand_1 = clm_make_temp_function(sc, "rand", g_rand_1, 1, 0, false, "rand opt", f,
				  NULL, NULL, NULL, NULL, NULL, env_rand_1);
  rand_2 = clm_make_temp_function(sc, "rand", g_rand_2, 2, 0, false, "rand opt", f,
				  NULL, NULL, NULL, NULL, NULL, NULL);
  direct_rand_2 = clm_make_temp_function_no_choice(sc, "rand", g_direct_rand_2, 2, 0, false, "rand opt", f);

  GEN_F("rand-interp", rand_interp);

  rand_interp_1 = clm_make_temp_function(sc, "rand-interp", g_rand_interp_1, 1, 0, false, "rand-interp opt", f,
					 NULL, NULL, NULL, NULL, mul_s_rand_interp_1, env_rand_interp_1);
  rand_interp_2 = clm_make_temp_function(sc, "rand-interp", g_rand_interp_2, 2, 0, false, "rand-interp opt", f,
					 NULL, NULL, NULL, NULL, NULL, NULL);
  direct_rand_interp_2 = clm_make_temp_function_no_choice(sc, "rand-interp", g_direct_rand_interp_2, 2, 0, false, "rand-interp opt", f);


  GEN_F3("formant", formant);
  s7_rf_set_function(f, is_formant_rf_3);

  formant_2 = clm_make_temp_function(sc, "formant", g_formant_2, 2, 0, false, "formant opt", f,
				     NULL, NULL, NULL, NULL, NULL, NULL);
  formant_two = clm_make_temp_function_no_choice(sc, "formant", g_formant_two, 2, 0, false, "formant opt", f);
  direct_formant_2 = clm_make_temp_function_no_choice(sc, "formant", g_direct_formant_2, 2, 0, false, "formant opt", f);


  f = s7_name_to_value(sc, "formant-bank");
  s7_rf_set_function(f, is_formant_bank_rf);
  s7_function_set_chooser(sc, f, formant_bank_chooser);
  direct_choice_2(sc, f, wrapped_formant_bank_2, wrapped_formant_bank_p);
  formant_bank_ss = clm_make_temp_function_no_choice(sc, "formant-bank", g_formant_bank_ss, 3, 0, false, "formant-bank opt", f);
  formant_bank_sz = clm_make_temp_function_no_choice(sc, "formant-bank", g_formant_bank_sz, 3, 0, false, "formant-bank opt", f);


  GEN_F3("firmant", firmant);

  firmant_2 = clm_make_temp_function(sc, "firmant", g_firmant_2, 2, 0, false, "firmant opt", f, NULL, NULL, NULL, NULL, NULL, NULL);
  direct_firmant_2 = clm_make_temp_function_no_choice(sc, "firmant", g_direct_firmant_2, 2, 0, false, "firmant opt", f);


  GEN_F3("all-pass", all_pass);

  all_pass_2 = clm_make_temp_function(sc, "all-pass", g_all_pass_2, 2, 0, false, "all-pass opt", f, NULL, NULL, NULL, NULL, NULL, NULL);
  direct_all_pass_2 = clm_make_temp_function_no_choice(sc, "all-pass", g_direct_all_pass_2, 2, 0, false, "all-pass opt", f);


  GEN_F("all-pass-bank", all_pass_bank);

  all_pass_bank_2 = clm_make_temp_function(sc, "all-pass-bank", g_all_pass_bank_2, 2, 0, false, "all-pass-bank opt", f, 
					   NULL, NULL, NULL, NULL, NULL, NULL);
  direct_all_pass_bank_2 = clm_make_temp_function_no_choice(sc, "all-pass-bank", g_direct_all_pass_bank_2, 2, 0, false, "all-pass-bank opt", f);


  GEN_F("one-pole-all-pass", one_pole_all_pass);
  one_pole_all_pass_2 = clm_make_temp_function(sc, "one-pole-all-pass", g_one_pole_all_pass_2, 2, 0, false, "one-pole-all-pass opt", f, 
					       NULL, NULL, NULL, NULL, NULL, NULL);
  direct_one_pole_all_pass_2 = clm_make_temp_function_no_choice(sc, "one-pole-all-pass", g_direct_one_pole_all_pass_2, 2, 0, false, "one-pole-all-pass opt", f);


  GEN_F3("delay", delay);

  delay_2 = clm_make_temp_function(sc, "delay", g_delay_2, 2, 0, false, "delay opt", f,
				   NULL, NULL, NULL, NULL, NULL, NULL);
  direct_delay_2 = clm_make_temp_function_no_choice(sc, "delay", g_direct_delay_2, 2, 0, false, "delay opt", f);


  f = s7_name_to_value(sc, "polynomial");
  s7_rf_set_function(f, is_polynomial_rf);
  s7_function_set_chooser(sc, f, polynomial_chooser);
  direct_choice_2(sc, f, (mus_float_t (*)(mus_xen *, mus_float_t))wrapped_polynomial_2, wrapped_polynomial_p);
  polynomial_temp = clm_make_function_no_choice(sc, "polynomial", g_polynomial_temp, 2, 0, false, "polynomial optimization", f);
  polynomial_cos = clm_make_function_no_choice(sc, "polynomial", g_polynomial_cos, 2, 0, false, "polynomial optimization", f);


  GEN_F("polyshape", polyshape);

  polyshape_2 = clm_make_temp_function(sc, "polyshape", g_polyshape_2, 2, 0, false, "polyshape opt", f,
				       NULL, NULL, NULL, NULL, NULL, NULL);
  direct_polyshape_2 = clm_make_temp_function_no_choice(sc, "polyshape", g_direct_polyshape_2, 2, 0, false, "polyshape opt", f);

  GEN_F("pulsed-env", pulsed_env);

  pulsed_env_2 = clm_make_temp_function(sc, "pulsed-env", g_pulsed_env_2, 2, 0, false, "pulsed-env opt", f,
					NULL, NULL, NULL, NULL, NULL, NULL);
  direct_pulsed_env_2 = clm_make_temp_function_no_choice(sc, "pulsed-env", g_direct_pulsed_env_2, 2, 0, false, "pulsed-env opt", f);
  pulsed_env_1 = clm_make_temp_function(sc, "pulsed-env", g_pulsed_env_1, 1, 0, false, "pulsed-env opt", f,
					NULL, NULL, NULL, NULL, NULL, env_pulsed_env_1);


  GEN_F3(S_ssb_am, ssb_am);

  ssb_am_2 = clm_make_temp_function(sc, S_ssb_am, g_ssb_am_2, 2, 0, false, "ssb-am opt", f,
				    NULL, NULL, NULL, NULL, NULL, NULL);
  direct_ssb_am_2 = clm_make_temp_function_no_choice(sc, S_ssb_am, g_direct_ssb_am_2, 2, 0, false, "ssb-am opt", f);


  GEN_F3("asymmetric-fm", asymmetric_fm);

  asymmetric_fm_2 = clm_make_temp_function(sc, "asymmetric-fm", g_asymmetric_fm_2, 2, 0, false, "asymmetric-fm opt", f,
					   NULL, NULL, NULL, NULL, NULL, NULL);
  direct_asymmetric_fm_2 = clm_make_temp_function_no_choice(sc, "asymmetric-fm", g_direct_asymmetric_fm_2, 2, 0, false, "asymmetric-fm opt", f);
  GEN_F3("filtered-comb", filtered_comb);

  filtered_comb_2 = clm_make_temp_function(sc, "filtered-comb", g_filtered_comb_2, 2, 0, false, "filtered-comb opt", f,
					   NULL, NULL, NULL, NULL, NULL, NULL);
  direct_filtered_comb_2 = clm_make_temp_function_no_choice(sc, "filtered-comb", g_direct_filtered_comb_2, 2, 0, false, "filtered-comb opt", f);

  GEN_F("filtered-comb-bank", filtered_comb_bank);

  filtered_comb_bank_2 = clm_make_temp_function(sc, "filtered-comb-bank", g_filtered_comb_bank_2, 2, 0, false, "filtered-comb-bank opt", f,
						NULL, NULL, NULL, NULL, NULL, NULL);
  direct_filtered_comb_bank_2 = clm_make_temp_function_no_choice(sc, "filtered-comb-bank", g_direct_filtered_comb_bank_2, 2, 0, false, "filtered-comb-bank opt", f);

  f = s7_name_to_value(sc, "move-sound");
  s7_rf_set_function(f, is_move_sound_rf);

  f = s7_name_to_value(sc, S_locsig);
  s7_rf_set_function(f, is_locsig_rf);
  s7_function_set_chooser(sc, f, locsig_chooser);

  indirect_locsig_3 = clm_make_function_no_choice(sc, S_locsig, g_indirect_locsig_3, 3, 0, false, "locsig opt", f);


  f = s7_name_to_value(sc, S_out_bank);
  s7_rf_set_function(f, is_out_bank_rf);

  f = s7_name_to_value(sc, S_outa);
  s7_function_set_chooser(sc, f, outa_chooser);
  s7_rf_set_function(f, is_outa_rf);

  outa_two = clm_make_function_no_choice(sc, S_outa, g_outa_two, 2, 0, false, "outa opt", f);
  indirect_outa_ss = clm_make_function_no_choice(sc, S_outa, g_indirect_outa_ss, 2, 0, false, "outa opt", f);
  outa_ss = clm_make_function_no_choice(sc, S_outa, g_outa_ss, 2, 0, false, "outa opt", f);
  indirect_outa_2 = clm_make_function_no_choice(sc, S_outa, g_indirect_outa_2, 2, 0, false, "outa opt", f);
  indirect_outa_2_temp = clm_make_function_no_choice(sc, S_outa, g_indirect_outa_2_temp, 2, 0, false, "outa opt", f);
  indirect_outa_2_env = clm_make_function_no_choice(sc, S_outa, g_indirect_outa_2_env, 2, 0, false, "outa opt", f);


  f = s7_name_to_value(sc, S_outb);
  s7_function_set_chooser(sc, f, outb_chooser);
  outb_two = clm_make_function_no_choice(sc, S_outb, g_outb_two, 2, 0, false, "outb opt", f);

  f = s7_name_to_value(sc, S_out_any);
  s7_function_set_chooser(sc, f, out_any_chooser);

  f = s7_name_to_value(sc, S_ina);
  s7_rf_set_function(f, is_ina_rf);
  s7_function_set_chooser(sc, f, ina_chooser);
  ina_ss = clm_make_function_no_choice(sc, S_ina, g_ina_ss, 2, 0, false, "ina opt", f);

  f = s7_name_to_value(sc, S_file_to_sample);
  s7_rf_set_function(f, is_file_to_sample_rf);
  s7_function_set_chooser(sc, f, file_to_sample_chooser);
  file_to_sample_ss = clm_make_function_no_choice(sc, S_file_to_sample, g_file_to_sample_ss, 2, 1, false, "file->sample opt", f);


  f = s7_name_to_value(sc, "mus-srate");
  s7_rf_set_function(f, is_srate_rf);

  f = s7_name_to_value(sc, "contrast-enhancement");
  s7_rf_set_function(f, is_contrast_enhancement_rf);

  f = s7_name_to_value(sc, "odd-weight");
  s7_rf_set_function(f, is_odd_weight_rf);

  f = s7_name_to_value(sc, "even-weight");
  s7_rf_set_function(f, is_even_weight_rf);

  f = s7_name_to_value(sc, "odd-multiple");
  s7_rf_set_function(f, is_odd_multiple_rf);

  f = s7_name_to_value(sc, "even-multiple");
  s7_rf_set_function(f, is_even_multiple_rf);

  f = s7_name_to_value(sc, "hz->radians");
  s7_rf_set_function(f, is_hz_to_radians_rf);

  f = s7_name_to_value(sc, "radians->hz");
  s7_rf_set_function(f, is_radians_to_hz_rf);

  f = s7_name_to_value(sc, "sin");
  s7_rf_set_function(f, is_sin_rf);

  f = s7_name_to_value(sc, "cos");
  s7_rf_set_function(f, is_cos_rf);

  f = s7_name_to_value(sc, "mus-random");
  s7_rf_set_function(f, is_random_rf);

  f = s7_name_to_value(sc, "amplitude-modulate");
  s7_rf_set_function(f, is_am_rf);

  f = s7_name_to_value(sc, "array-interp");
  s7_rf_set_function(f, is_array_interp_rf);
}


static char *mus_generator_to_readable_string(s7_scheme *sc, void *obj)
{
  char *str;
  str = (char *)malloc(64 * sizeof(char));
  snprintf(str, 64, "#<%s>", mus_name(((mus_xen *)obj)->gen));
  return(str);
  /* we need a new function to fill this role */
  /* s7_error(sc, s7_make_symbol(sc, "io-error"), s7_list(sc, 1, s7_make_string(sc, "can't write a clm generator readably"))); */
  /* return(NULL); */
}
#endif


Xen_wrap_no_args(g_mus_srate_w, g_mus_srate)
Xen_wrap_1_arg(g_mus_set_srate_w, g_mus_set_srate)
Xen_wrap_no_args(g_mus_float_equal_fudge_factor_w, g_mus_float_equal_fudge_factor)
Xen_wrap_1_arg(g_mus_set_float_equal_fudge_factor_w, g_mus_set_float_equal_fudge_factor)
Xen_wrap_no_args(g_mus_array_print_length_w, g_mus_array_print_length)
Xen_wrap_1_arg(g_mus_set_array_print_length_w, g_mus_set_array_print_length)
Xen_wrap_1_arg(g_radians_to_hz_w, g_radians_to_hz)
Xen_wrap_1_arg(g_hz_to_radians_w, g_hz_to_radians)
Xen_wrap_1_arg(g_radians_to_degrees_w, g_radians_to_degrees)
Xen_wrap_1_arg(g_degrees_to_radians_w, g_degrees_to_radians)
Xen_wrap_1_arg(g_db_to_linear_w, g_db_to_linear)
Xen_wrap_1_arg(g_linear_to_db_w, g_linear_to_db)
Xen_wrap_1_arg(g_even_weight_w, g_even_weight)
Xen_wrap_1_arg(g_odd_weight_w, g_odd_weight)
Xen_wrap_2_args(g_even_multiple_w, g_even_multiple)
Xen_wrap_2_args(g_odd_multiple_w, g_odd_multiple)
Xen_wrap_1_arg(g_seconds_to_samples_w, g_seconds_to_samples)
Xen_wrap_1_arg(g_samples_to_seconds_w, g_samples_to_seconds)
Xen_wrap_2_args(g_ring_modulate_w, g_ring_modulate)
Xen_wrap_3_args(g_amplitude_modulate_w, g_amplitude_modulate)
Xen_wrap_2_optional_args(g_contrast_enhancement_w, g_contrast_enhancement)
Xen_wrap_3_optional_args(g_dot_product_w, g_dot_product)
#if HAVE_COMPLEX_TRIG && HAVE_COMPLEX_NUMBERS && (!HAVE_RUBY)
Xen_wrap_2_args(g_edot_product_w, g_edot_product)
#endif
Xen_wrap_2_args(g_polynomial_w, g_polynomial)
Xen_wrap_4_optional_args(g_make_fft_window_w, g_make_fft_window)
Xen_wrap_4_optional_args(g_mus_fft_w, g_mus_fft)
Xen_wrap_4_optional_args(g_spectrum_w, g_spectrum)
Xen_wrap_1_arg(g_autocorrelate_w, g_autocorrelate)
Xen_wrap_2_args(g_correlate_w, g_correlate)
Xen_wrap_3_optional_args(g_convolution_w, g_convolution)
Xen_wrap_2_args(g_rectangular_to_polar_w, g_rectangular_to_polar)
Xen_wrap_2_args(g_rectangular_to_magnitudes_w, g_rectangular_to_magnitudes)
Xen_wrap_2_args(g_polar_to_rectangular_w, g_polar_to_rectangular)
Xen_wrap_3_optional_args(g_array_interp_w, g_array_interp)
Xen_wrap_5_optional_args(g_mus_interpolate_w, g_mus_interpolate)
Xen_wrap_1_arg(g_mus_describe_w, g_mus_describe)
Xen_wrap_1_arg(g_mus_name_w, g_mus_name)
Xen_wrap_3_optional_args(g_mus_run_w, g_mus_run)
Xen_wrap_1_arg(g_mus_phase_w, g_mus_phase)
Xen_wrap_2_args(g_mus_set_phase_w, g_mus_set_phase)
Xen_wrap_1_arg(g_mus_width_w, g_mus_width)
Xen_wrap_2_args(g_mus_set_width_w, g_mus_set_width)
Xen_wrap_1_arg(g_mus_scaler_w, g_mus_scaler)
Xen_wrap_2_args(g_mus_set_scaler_w, g_mus_set_scaler)
Xen_wrap_1_arg(g_mus_feedforward_w, g_mus_feedforward)
Xen_wrap_2_args(g_mus_set_feedforward_w, g_mus_set_feedforward)
Xen_wrap_1_arg(g_mus_reset_w, g_mus_reset)
Xen_wrap_1_arg(g_mus_copy_w, g_mus_copy)
Xen_wrap_1_arg(g_mus_offset_w, g_mus_offset)
Xen_wrap_2_args(g_mus_set_offset_w, g_mus_set_offset)
Xen_wrap_1_arg(g_mus_frequency_w, g_mus_frequency)
Xen_wrap_2_args(g_mus_set_frequency_w, g_mus_set_frequency)
Xen_wrap_1_arg(g_mus_length_w, g_mus_length)
Xen_wrap_1_arg(g_mus_file_name_w, g_mus_file_name)
Xen_wrap_2_args(g_mus_set_length_w, g_mus_set_length)
Xen_wrap_1_arg(g_mus_type_w, g_mus_type)
Xen_wrap_1_arg(g_mus_order_w, g_mus_order)
Xen_wrap_1_arg(g_mus_data_w, g_mus_data)
Xen_wrap_2_args(g_mus_set_data_w, g_mus_set_data)
Xen_wrap_1_arg(g_is_oscil_w, g_is_oscil)
Xen_wrap_3_optional_args(g_oscil_w, g_oscil)
Xen_wrap_1_arg(g_is_oscil_bank_w, g_is_oscil_bank)
Xen_wrap_1_arg(g_oscil_bank_w, g_oscil_bank)
Xen_wrap_any_args(g_mus_apply_w, g_mus_apply)
Xen_wrap_any_args(g_make_delay_w, g_make_delay)
Xen_wrap_any_args(g_make_comb_w, g_make_comb)
Xen_wrap_any_args(g_make_filtered_comb_w, g_make_filtered_comb)
Xen_wrap_any_args(g_make_notch_w, g_make_notch)
Xen_wrap_any_args(g_make_all_pass_w, g_make_all_pass)
Xen_wrap_any_args(g_make_moving_average_w, g_make_moving_average)
Xen_wrap_any_args(g_make_moving_max_w, g_make_moving_max)
Xen_wrap_any_args(g_make_moving_norm_w, g_make_moving_norm)
Xen_wrap_3_optional_args(g_delay_w, g_delay)
Xen_wrap_2_optional_args(g_delay_tick_w, g_delay_tick)
Xen_wrap_2_optional_args(g_tap_w, g_tap)
Xen_wrap_3_optional_args(g_notch_w, g_notch)
Xen_wrap_3_optional_args(g_comb_w, g_comb)
Xen_wrap_3_optional_args(g_filtered_comb_w, g_filtered_comb)
Xen_wrap_3_optional_args(g_all_pass_w, g_all_pass)
Xen_wrap_2_optional_args(g_moving_average_w, g_moving_average)
Xen_wrap_2_optional_args(g_moving_max_w, g_moving_max)
Xen_wrap_2_optional_args(g_moving_norm_w, g_moving_norm)
Xen_wrap_1_arg(g_is_tap_w, g_is_tap)
Xen_wrap_1_arg(g_is_delay_w, g_is_delay)
Xen_wrap_1_arg(g_is_notch_w, g_is_notch)
Xen_wrap_1_arg(g_is_comb_w, g_is_comb)
Xen_wrap_1_arg(g_is_filtered_comb_w, g_is_filtered_comb)
Xen_wrap_1_arg(g_is_all_pass_w, g_is_all_pass)
Xen_wrap_1_arg(g_is_moving_average_w, g_is_moving_average)
Xen_wrap_1_arg(g_is_moving_max_w, g_is_moving_max)
Xen_wrap_1_arg(g_is_moving_norm_w, g_is_moving_norm)

Xen_wrap_2_optional_args(g_ncos_w, g_ncos)
Xen_wrap_1_arg(g_is_ncos_w, g_is_ncos)
Xen_wrap_2_optional_args(g_nsin_w, g_nsin)
Xen_wrap_1_arg(g_is_nsin_w, g_is_nsin)

Xen_wrap_any_args(g_make_rand_w, g_make_rand)
Xen_wrap_any_args(g_make_rand_interp_w, g_make_rand_interp)
Xen_wrap_2_optional_args(g_rand_w, g_rand)
Xen_wrap_2_optional_args(g_rand_interp_w, g_rand_interp)
Xen_wrap_1_arg(g_is_rand_w, g_is_rand)
Xen_wrap_1_arg(g_is_rand_interp_w, g_is_rand_interp)
Xen_wrap_1_arg(g_mus_random_w, g_mus_random)
Xen_wrap_no_args(g_mus_rand_seed_w, g_mus_rand_seed)
Xen_wrap_1_arg(g_mus_set_rand_seed_w, g_mus_set_rand_seed)
Xen_wrap_1_arg(g_is_table_lookup_w, g_is_table_lookup)
Xen_wrap_any_args(g_make_table_lookup_w, g_make_table_lookup)
Xen_wrap_2_optional_args(g_table_lookup_w, g_table_lookup)
Xen_wrap_3_optional_args(g_partials_to_wave_w, g_partials_to_wave)
Xen_wrap_3_optional_args(g_phase_partials_to_wave_w, g_phase_partials_to_wave)
Xen_wrap_6_optional_args(g_make_sawtooth_wave_w, g_make_sawtooth_wave)
Xen_wrap_2_optional_args(g_sawtooth_wave_w, g_sawtooth_wave)
Xen_wrap_1_arg(g_is_sawtooth_wave_w, g_is_sawtooth_wave)
Xen_wrap_6_optional_args(g_make_triangle_wave_w, g_make_triangle_wave)
Xen_wrap_2_optional_args(g_triangle_wave_w, g_triangle_wave)
Xen_wrap_1_arg(g_is_triangle_wave_w, g_is_triangle_wave)
Xen_wrap_6_optional_args(g_make_square_wave_w, g_make_square_wave)
Xen_wrap_2_optional_args(g_square_wave_w, g_square_wave)
Xen_wrap_1_arg(g_is_square_wave_w, g_is_square_wave)
Xen_wrap_6_optional_args(g_make_pulse_train_w, g_make_pulse_train)
Xen_wrap_2_optional_args(g_pulse_train_w, g_pulse_train)
Xen_wrap_1_arg(g_is_pulse_train_w, g_is_pulse_train)

Xen_wrap_3_args(g_make_pulsed_env_w, g_make_pulsed_env)
Xen_wrap_2_optional_args(g_pulsed_env_w, g_pulsed_env)
Xen_wrap_1_arg(g_is_pulsed_env_w, g_is_pulsed_env)

Xen_wrap_3_optional_args(g_asymmetric_fm_w, g_asymmetric_fm)
Xen_wrap_1_arg(g_is_asymmetric_fm_w, g_is_asymmetric_fm)
Xen_wrap_4_optional_args(g_make_one_zero_w, g_make_one_zero)
Xen_wrap_2_optional_args(g_one_zero_w, g_one_zero)
Xen_wrap_1_arg(g_is_one_zero_w, g_is_one_zero)
Xen_wrap_4_optional_args(g_make_one_pole_w, g_make_one_pole)
Xen_wrap_2_optional_args(g_one_pole_w, g_one_pole)
Xen_wrap_1_arg(g_is_one_pole_w, g_is_one_pole)
Xen_wrap_6_optional_args(g_make_two_zero_w, g_make_two_zero)
Xen_wrap_2_optional_args(g_two_zero_w, g_two_zero)
Xen_wrap_1_arg(g_is_two_zero_w, g_is_two_zero)
Xen_wrap_6_optional_args(g_make_two_pole_w, g_make_two_pole)
Xen_wrap_2_optional_args(g_two_pole_w, g_two_pole)
Xen_wrap_1_arg(g_is_two_pole_w, g_is_two_pole)

Xen_wrap_1_arg(g_is_formant_w, g_is_formant)
Xen_wrap_4_optional_args(g_make_formant_w, g_make_formant)
Xen_wrap_3_optional_args(g_formant_w, g_formant)

Xen_wrap_2_args(g_formant_bank_w, g_formant_bank)
Xen_wrap_1_arg(g_is_formant_bank_w, g_is_formant_bank)
Xen_wrap_2_optional_args(g_make_formant_bank_w, g_make_formant_bank)

Xen_wrap_1_arg(g_is_firmant_w, g_is_firmant)
Xen_wrap_4_optional_args(g_make_firmant_w, g_make_firmant)
Xen_wrap_3_optional_args(g_firmant_w, g_firmant)

Xen_wrap_1_arg(g_is_one_pole_all_pass_w, g_is_one_pole_all_pass)
Xen_wrap_2_args(g_make_one_pole_all_pass_w, g_make_one_pole_all_pass)
Xen_wrap_2_optional_args(g_one_pole_all_pass_w, g_one_pole_all_pass)

Xen_wrap_2_args(g_set_formant_frequency_w, g_set_formant_frequency)
Xen_wrap_3_args(g_set_formant_radius_and_frequency_w, g_set_formant_radius_and_frequency)

Xen_wrap_5_args(g_frample_to_frample_w, g_frample_to_frample)

Xen_wrap_any_args(g_make_wave_train_w, g_make_wave_train)
Xen_wrap_2_optional_args(g_wave_train_w, g_wave_train)
Xen_wrap_1_arg(g_is_wave_train_w, g_is_wave_train)
Xen_wrap_any_args(g_make_polyshape_w, g_make_polyshape)
Xen_wrap_3_optional_args(g_polyshape_w, g_polyshape)
Xen_wrap_1_arg(g_is_polyshape_w, g_is_polyshape)
Xen_wrap_2_optional_args(g_partials_to_polynomial_w, g_partials_to_polynomial)
Xen_wrap_1_arg(g_normalize_partials_w, g_normalize_partials)
Xen_wrap_2_args(g_chebyshev_t_sum_w, g_chebyshev_t_sum)
Xen_wrap_2_args(g_chebyshev_u_sum_w, g_chebyshev_u_sum)
Xen_wrap_3_args(g_chebyshev_tu_sum_w, g_chebyshev_tu_sum)
Xen_wrap_any_args(g_make_polywave_w, g_make_polywave)
Xen_wrap_2_optional_args(g_polywave_w, g_polywave)
Xen_wrap_1_arg(g_is_polywave_w, g_is_polywave)

Xen_wrap_any_args(g_make_nrxysin_w, g_make_nrxysin)
Xen_wrap_2_optional_args(g_nrxysin_w, g_nrxysin)
Xen_wrap_1_arg(g_is_nrxysin_w, g_is_nrxysin)
Xen_wrap_any_args(g_make_nrxycos_w, g_make_nrxycos)
Xen_wrap_2_optional_args(g_nrxycos_w, g_nrxycos)
Xen_wrap_1_arg(g_is_nrxycos_w, g_is_nrxycos)

Xen_wrap_any_args(g_make_rxyksin_w, g_make_rxyksin)
Xen_wrap_2_optional_args(g_rxyksin_w, g_rxyksin)
Xen_wrap_1_arg(g_is_rxyksin_w, g_is_rxyksin)
Xen_wrap_any_args(g_make_rxykcos_w, g_make_rxykcos)
Xen_wrap_2_optional_args(g_rxykcos_w, g_rxykcos)
Xen_wrap_1_arg(g_is_rxykcos_w, g_is_rxykcos)

Xen_wrap_6_optional_args(g_make_filter_w, g_make_filter)
Xen_wrap_2_optional_args(g_filter_w, g_filter)
Xen_wrap_1_arg(g_is_filter_w, g_is_filter)
Xen_wrap_4_optional_args(g_make_fir_filter_w, g_make_fir_filter)
Xen_wrap_2_args(g_make_fir_coeffs_w, g_make_fir_coeffs)
Xen_wrap_2_optional_args(g_fir_filter_w, g_fir_filter)
Xen_wrap_1_arg(g_is_fir_filter_w, g_is_fir_filter)
Xen_wrap_4_optional_args(g_make_iir_filter_w, g_make_iir_filter)
Xen_wrap_2_optional_args(g_iir_filter_w, g_iir_filter)
Xen_wrap_1_arg(g_is_iir_filter_w, g_is_iir_filter)
Xen_wrap_1_arg(g_mus_xcoeffs_w, g_mus_xcoeffs)
Xen_wrap_1_arg(g_mus_ycoeffs_w, g_mus_ycoeffs)
Xen_wrap_2_args(g_mus_xcoeff_w, g_mus_xcoeff)
Xen_wrap_3_args(g_mus_set_xcoeff_w, g_mus_set_xcoeff)
Xen_wrap_2_args(g_mus_ycoeff_w, g_mus_ycoeff)
Xen_wrap_3_args(g_mus_set_ycoeff_w, g_mus_set_ycoeff)
Xen_wrap_1_arg(g_is_env_w, g_is_env)
Xen_wrap_1_arg(g_env_w, g_env)
Xen_wrap_any_args(g_make_env_w, g_make_env)
Xen_wrap_2_args(g_env_interp_w, g_env_interp)
Xen_wrap_3_optional_args(g_envelope_interp_w, g_envelope_interp)
Xen_wrap_2_args(g_env_any_w, g_env_any)
Xen_wrap_1_arg(g_is_file_to_sample_w, g_is_file_to_sample)
Xen_wrap_2_optional_args(g_make_file_to_sample_w, g_make_file_to_sample)
Xen_wrap_3_optional_args(g_file_to_sample_w, g_file_to_sample)
Xen_wrap_1_arg(g_is_sample_to_file_w, g_is_sample_to_file)
Xen_wrap_5_optional_args(g_make_sample_to_file_w, g_make_sample_to_file)
Xen_wrap_1_arg(g_continue_sample_to_file_w, g_continue_sample_to_file)
Xen_wrap_4_args(g_sample_to_file_w, g_sample_to_file)
Xen_wrap_2_args(g_sample_to_file_add_w, g_sample_to_file_add)

Xen_wrap_1_arg(g_is_file_to_frample_w, g_is_file_to_frample)
Xen_wrap_2_optional_args(g_make_file_to_frample_w, g_make_file_to_frample)
Xen_wrap_3_optional_args(g_file_to_frample_w, g_file_to_frample)
Xen_wrap_1_arg(g_continue_frample_to_file_w, g_continue_frample_to_file)
Xen_wrap_1_arg(g_is_frample_to_file_w, g_is_frample_to_file)
Xen_wrap_3_args(g_frample_to_file_w, g_frample_to_file)
Xen_wrap_5_optional_args(g_make_frample_to_file_w, g_make_frample_to_file)


Xen_wrap_1_arg(g_is_mus_input_w, g_is_mus_input)
Xen_wrap_1_arg(g_is_mus_output_w, g_is_mus_output)
Xen_wrap_3_args(g_in_any_w, g_in_any)
Xen_wrap_2_args(g_ina_w, g_ina)
Xen_wrap_2_args(g_inb_w, g_inb)
Xen_wrap_4_optional_args(g_out_any_w, g_out_any)
Xen_wrap_3_optional_args(g_outa_w, g_outa)
Xen_wrap_3_optional_args(g_outb_w, g_outb)
Xen_wrap_3_optional_args(g_outc_w, g_outc)
Xen_wrap_3_optional_args(g_outd_w, g_outd)
Xen_wrap_1_arg(g_mus_close_w, g_mus_close)
Xen_wrap_no_args(g_mus_file_buffer_size_w, g_mus_file_buffer_size)
Xen_wrap_1_arg(g_mus_set_file_buffer_size_w, g_mus_set_file_buffer_size)
Xen_wrap_1_arg(g_is_readin_w, g_is_readin)
Xen_wrap_1_arg(g_readin_w, g_readin)
Xen_wrap_any_args(g_make_readin_w, g_make_readin)
Xen_wrap_1_arg(g_mus_channel_w, g_mus_channel)
Xen_wrap_1_arg(g_mus_interp_type_w, g_mus_interp_type)
Xen_wrap_1_arg(g_mus_location_w, g_mus_location)
Xen_wrap_2_args(g_mus_set_location_w, g_mus_set_location)
Xen_wrap_1_arg(g_mus_increment_w, g_mus_increment)
Xen_wrap_2_args(g_mus_set_increment_w, g_mus_set_increment)
Xen_wrap_1_arg(g_mus_feedback_w, g_mus_feedback)
Xen_wrap_2_args(g_mus_set_feedback_w, g_mus_set_feedback)
Xen_wrap_1_arg(g_is_locsig_w, g_is_locsig)
Xen_wrap_3_args(g_locsig_w, g_locsig)
Xen_wrap_any_args(g_make_locsig_w, g_make_locsig)
Xen_wrap_3_args(g_move_locsig_w, g_move_locsig)
Xen_wrap_no_args(g_locsig_type_w, g_locsig_type)
Xen_wrap_1_arg(g_set_locsig_type_w, g_set_locsig_type)
Xen_wrap_1_arg(g_mus_channels_w, g_mus_channels)
Xen_wrap_2_args(g_locsig_ref_w, g_locsig_ref)
Xen_wrap_2_args(g_locsig_reverb_ref_w, g_locsig_reverb_ref)
Xen_wrap_3_args(g_locsig_set_w, g_locsig_set)
Xen_wrap_3_args(g_locsig_reverb_set_w, g_locsig_reverb_set)
Xen_wrap_1_arg(g_is_move_sound_w, g_is_move_sound)
Xen_wrap_3_args(g_move_sound_w, g_move_sound)
Xen_wrap_3_optional_args(g_make_move_sound_w, g_make_move_sound)
Xen_wrap_no_args(g_mus_clear_sincs_w, g_mus_clear_sincs)
Xen_wrap_1_arg(g_is_src_w, g_is_src)
Xen_wrap_3_optional_args(g_src_w, g_src)
Xen_wrap_6_optional_args(g_make_src_w, g_make_src)
Xen_wrap_1_arg(g_is_granulate_w, g_is_granulate)
Xen_wrap_3_optional_args(g_granulate_w, g_granulate)
Xen_wrap_any_args(g_make_granulate_w, g_make_granulate)
Xen_wrap_1_arg(g_mus_ramp_w, g_mus_ramp)
Xen_wrap_2_args(g_mus_set_ramp_w, g_mus_set_ramp)
Xen_wrap_1_arg(g_is_convolve_w, g_is_convolve)
Xen_wrap_2_optional_args(g_convolve_w, g_convolve)
Xen_wrap_any_args(g_make_convolve_w, g_make_convolve)
Xen_wrap_4_optional_args(g_convolve_files_w, g_convolve_files)
Xen_wrap_1_arg(g_is_phase_vocoder_w, g_is_phase_vocoder)
Xen_wrap_5_optional_args(g_phase_vocoder_w, g_phase_vocoder)
Xen_wrap_any_args(g_make_phase_vocoder_w, g_make_phase_vocoder)
Xen_wrap_1_arg(g_phase_vocoder_amp_increments_w, g_phase_vocoder_amp_increments)
Xen_wrap_1_arg(g_phase_vocoder_amps_w, g_phase_vocoder_amps)
Xen_wrap_1_arg(g_phase_vocoder_freqs_w, g_phase_vocoder_freqs)
Xen_wrap_1_arg(g_phase_vocoder_phases_w, g_phase_vocoder_phases)
Xen_wrap_1_arg(g_phase_vocoder_phase_increments_w, g_phase_vocoder_phase_increments)
Xen_wrap_1_arg(g_mus_hop_w, g_mus_hop)
Xen_wrap_2_args(g_mus_set_hop_w, g_mus_set_hop)
Xen_wrap_4_optional_args(g_make_ssb_am_w, g_make_ssb_am)
Xen_wrap_3_optional_args(g_ssb_am_w, g_ssb_am)
Xen_wrap_1_arg(g_is_ssb_am_w, g_is_ssb_am)
Xen_wrap_no_args(g_clm_table_size_w, g_clm_table_size)
Xen_wrap_1_arg(g_set_clm_table_size_w, g_set_clm_table_size)
Xen_wrap_no_args(g_clm_default_frequency_w, g_clm_default_frequency)
Xen_wrap_1_arg(g_set_clm_default_frequency_w, g_set_clm_default_frequency)
Xen_wrap_1_arg(g_is_mus_generator_w, g_is_mus_generator)
Xen_wrap_1_arg(g_mus_frandom_w, g_mus_frandom)
Xen_wrap_1_arg(g_mus_irandom_w, g_mus_irandom)
#if (!HAVE_SCHEME)
Xen_wrap_4_optional_args(g_make_oscil_w, g_make_oscil)
Xen_wrap_4_optional_args(g_make_ncos_w, g_make_ncos)
#endif
Xen_wrap_4_optional_args(g_make_oscil_bank_w, g_make_oscil_bank)
Xen_wrap_4_optional_args(g_make_nsin_w, g_make_nsin)
Xen_wrap_8_optional_args(g_make_asymmetric_fm_w, g_make_asymmetric_fm)

Xen_wrap_any_args(g_mus_file_mix_w, g_mus_file_mix)
Xen_wrap_any_args(g_mus_file_mix_with_envs_w, g_mus_file_mix_with_envs)

Xen_wrap_2_optional_args(g_comb_bank_w, g_comb_bank)
Xen_wrap_1_arg(g_is_comb_bank_w, g_is_comb_bank)
Xen_wrap_1_arg(g_make_comb_bank_w, g_make_comb_bank)
Xen_wrap_2_optional_args(g_filtered_comb_bank_w, g_filtered_comb_bank)
Xen_wrap_1_arg(g_is_filtered_comb_bank_w, g_is_filtered_comb_bank)
Xen_wrap_1_arg(g_make_filtered_comb_bank_w, g_make_filtered_comb_bank)
Xen_wrap_2_optional_args(g_all_pass_bank_w, g_all_pass_bank)
Xen_wrap_1_arg(g_is_all_pass_bank_w, g_is_all_pass_bank)
Xen_wrap_1_arg(g_make_all_pass_bank_w, g_make_all_pass_bank)
Xen_wrap_1_arg(g_pink_noise_w, g_pink_noise)
Xen_wrap_3_args(g_out_bank_w, g_out_bank)

#if HAVE_SCHEME
#define Xen_define_real_procedure(Name, Func, ReqArg, OptArg, RstArg, Doc) \
  do { \
  s7_pointer sym, f;							\
  sym = s7_define_safe_function(s7, Name, Func, ReqArg, OptArg, RstArg, Doc); \
  f = s7_value(s7, sym); \
  s7_function_set_returns_temp(f);\
  } while (0)
#else
#define Xen_define_real_procedure(Name, Func, ReqArg, OptArg, RstArg, Doc) Xen_define_safe_procedure(Name, Func, ReqArg, OptArg, RstArg, Doc)
#endif

#if HAVE_SCHEME
  static s7_pointer acc_clm_srate(s7_scheme *sc, s7_pointer args) {return(g_mus_set_srate(s7_cadr(args)));}  
  static s7_pointer acc_clm_default_frequency(s7_scheme *sc, s7_pointer args) {return(g_set_clm_default_frequency(s7_cadr(args)));}  
  static s7_pointer acc_clm_table_size(s7_scheme *sc, s7_pointer args) {return(g_set_clm_table_size(s7_cadr(args)));}  
  static s7_pointer acc_mus_file_buffer_size(s7_scheme *sc, s7_pointer args) {return(g_mus_set_file_buffer_size(s7_cadr(args)));}  
  static s7_pointer acc_mus_float_equal_fudge_factor(s7_scheme *sc, s7_pointer args) {return(g_mus_set_float_equal_fudge_factor(s7_cadr(args)));}  
  static s7_pointer acc_mus_array_print_length(s7_scheme *sc, s7_pointer args) {return(g_mus_set_array_print_length(s7_cadr(args)));}  
#endif


static void mus_xen_init(void)
{
  mus_initialize();
  current_connect_func = Xen_false;

#if HAVE_SCHEME
  mus_xen_tag = s7_new_type_x(s7, "<generator>", print_mus_xen, free_mus_xen, s7_equalp_mus_xen, mark_mus_xen, mus_xen_apply, NULL, s7_mus_length, s7_mus_copy, NULL, NULL);
  as_needed_arglist = Xen_list_1(Xen_integer_zero);
  Xen_GC_protect(as_needed_arglist);
  s7_set_object_print_readably(mus_xen_tag, mus_generator_to_readable_string);
#else
  mus_xen_tag = Xen_make_object_type("Mus", sizeof(mus_xen));
#endif

  xen_one = C_int_to_Xen_integer(1);
  Xen_GC_protect(xen_one);
  xen_minus_one = C_int_to_Xen_integer(-1);
  Xen_GC_protect(xen_minus_one);

#if HAVE_FORTH
  fth_set_object_inspect(mus_xen_tag, print_mus_xen);
  fth_set_object_equal(mus_xen_tag, equalp_mus_xen);
  fth_set_object_mark(mus_xen_tag, mark_mus_xen);
  fth_set_object_free(mus_xen_tag, free_mus_xen);
  fth_set_object_apply(mus_xen_tag, Xen_procedure_cast mus_xen_apply, 0, 2, 0);
#endif

#if HAVE_RUBY
  rb_define_method(mus_xen_tag, "to_s", Xen_procedure_cast mus_xen_to_s, 0);
  rb_define_method(mus_xen_tag, "eql?", Xen_procedure_cast equalp_mus_xen, 1);
  rb_define_method(mus_xen_tag, "frequency", Xen_procedure_cast g_mus_frequency, 0);
  rb_define_method(mus_xen_tag, "frequency=", Xen_procedure_cast g_mus_set_frequency, 1);
  rb_define_method(mus_xen_tag, "phase", Xen_procedure_cast g_mus_phase, 0);
  rb_define_method(mus_xen_tag, "phase=", Xen_procedure_cast g_mus_set_phase, 1);
  rb_define_method(mus_xen_tag, "scaler", Xen_procedure_cast g_mus_scaler, 0);
  rb_define_method(mus_xen_tag, "scaler=", Xen_procedure_cast g_mus_set_scaler, 1);
  rb_define_method(mus_xen_tag, "width", Xen_procedure_cast g_mus_width, 0);
  rb_define_method(mus_xen_tag, "width=", Xen_procedure_cast g_mus_set_width, 1);
  rb_define_method(mus_xen_tag, "offset", Xen_procedure_cast g_mus_offset, 0);
  rb_define_method(mus_xen_tag, "offset=", Xen_procedure_cast g_mus_set_offset, 1);
  rb_define_method(mus_xen_tag, "reset", Xen_procedure_cast g_mus_reset, 0);
  /* rb_define_method(mus_xen_tag, "copy", Xen_procedure_cast g_mus_copy, 0); */
  rb_define_method(mus_xen_tag, "length", Xen_procedure_cast g_mus_length, 0);
  rb_define_method(mus_xen_tag, "length=", Xen_procedure_cast g_mus_set_length, 1);
  rb_define_method(mus_xen_tag, "data", Xen_procedure_cast g_mus_data, 0);
  rb_define_method(mus_xen_tag, "data=", Xen_procedure_cast g_mus_set_data, 1);
  rb_define_method(mus_xen_tag, "feedforward", Xen_procedure_cast g_mus_feedforward, 0);
  rb_define_method(mus_xen_tag, "feedforward=", Xen_procedure_cast g_mus_set_feedforward, 1);
  rb_define_method(mus_xen_tag, "feedback", Xen_procedure_cast g_mus_feedback, 0);
  rb_define_method(mus_xen_tag, "feedback=", Xen_procedure_cast g_mus_set_increment, 1);
  rb_define_method(mus_xen_tag, "order", Xen_procedure_cast g_mus_order, 0);
  rb_define_method(mus_xen_tag, "type", Xen_procedure_cast g_mus_type, 0);
  rb_define_method(mus_xen_tag, "order=", Xen_procedure_cast g_mus_set_length, 1);
  rb_define_method(mus_xen_tag, "call", Xen_procedure_cast mus_xen_apply, 2);
  rb_define_method(mus_xen_tag, "location", Xen_procedure_cast g_mus_location, 0);
  rb_define_method(mus_xen_tag, "location=", Xen_procedure_cast g_mus_set_location, 1);
  rb_define_method(mus_xen_tag, "increment", Xen_procedure_cast g_mus_increment, 0);
  rb_define_method(mus_xen_tag, "increment=", Xen_procedure_cast g_mus_set_increment, 1);
  rb_define_method(mus_xen_tag, "channels", Xen_procedure_cast g_mus_channels, 0);
  rb_define_method(mus_xen_tag, "channel", Xen_procedure_cast g_mus_channel, 0);
  rb_define_method(mus_xen_tag, "interp_type", Xen_procedure_cast g_mus_interp_type, 0);
  rb_define_method(mus_xen_tag, "xcoeffs", Xen_procedure_cast g_mus_xcoeffs, 0);
  rb_define_method(mus_xen_tag, "ycoeffs", Xen_procedure_cast g_mus_ycoeffs, 0);
  rb_define_method(mus_xen_tag, "xcoeff", Xen_procedure_cast g_mus_xcoeff, 1);
  rb_define_method(mus_xen_tag, "ycoeff", Xen_procedure_cast g_mus_ycoeff, 1);
  /*
  rb_define_method(mus_xen_tag, "xcoeff=", Xen_procedure_cast g_mus_set_xcoeff, 1);
  rb_define_method(mus_xen_tag, "ycoeff=", Xen_procedure_cast g_mus_set_ycoeff, 1);
  */
  rb_define_method(mus_xen_tag, "ramp", Xen_procedure_cast g_mus_ramp, 0);
  rb_define_method(mus_xen_tag, "ramp=", Xen_procedure_cast g_mus_set_ramp, 1);
  rb_define_method(mus_xen_tag, "hop", Xen_procedure_cast g_mus_hop, 0);
  rb_define_method(mus_xen_tag, "hop=", Xen_procedure_cast g_mus_set_hop, 1);
  rb_define_method(mus_xen_tag, "name", Xen_procedure_cast g_mus_name, 0);
  rb_define_method(mus_xen_tag, "file_name", Xen_procedure_cast g_mus_file_name, 0);
#endif  

  init_keywords();

  Xen_define_dilambda(S_mus_srate, g_mus_srate_w, H_mus_srate, S_setB S_mus_srate, g_mus_set_srate_w, 0, 0, 1, 0);
  Xen_define_dilambda(S_mus_float_equal_fudge_factor, g_mus_float_equal_fudge_factor_w, H_mus_float_equal_fudge_factor,
		      S_setB S_mus_float_equal_fudge_factor, g_mus_set_float_equal_fudge_factor_w, 0, 0, 1, 0);
  Xen_define_dilambda(S_mus_array_print_length, g_mus_array_print_length_w, H_mus_array_print_length,
		      S_setB S_mus_array_print_length, g_mus_set_array_print_length_w, 0, 0, 1, 0);
  Xen_define_dilambda(S_clm_table_size, g_clm_table_size_w, H_clm_table_size, S_setB S_clm_table_size, g_set_clm_table_size_w, 0, 0, 1, 0);
  Xen_define_dilambda(S_clm_default_frequency, g_clm_default_frequency_w, H_clm_default_frequency,
		      S_setB S_clm_default_frequency, g_set_clm_default_frequency_w, 0, 0, 1, 0);

#if HAVE_SCHEME
  clm_srate_symbol = s7_define_variable(s7, "*clm-srate*", s7_make_real(s7, MUS_DEFAULT_SAMPLING_RATE));
  s7_symbol_set_access(s7, clm_srate_symbol, s7_make_function(s7, "[acc-clm-srate]", acc_clm_srate, 2, 0, false, "accessor"));

  clm_default_frequency_symbol = s7_define_variable(s7, "*" S_clm_default_frequency "*", s7_make_real(s7, MUS_CLM_DEFAULT_FREQUENCY));
  s7_symbol_set_documentation(s7, clm_default_frequency_symbol, "*clm-default-frequency*: the default frequency for most generators (0.0)");
  s7_symbol_set_access(s7, clm_default_frequency_symbol, s7_make_function(s7, "[acc-clm-default-frequency]", acc_clm_default_frequency, 2, 0, false, "accessor"));

  clm_table_size_symbol = s7_define_variable(s7, "*" S_clm_table_size "*", s7_make_integer(s7, MUS_CLM_DEFAULT_TABLE_SIZE));
  s7_symbol_set_documentation(s7, clm_table_size_symbol, "*clm-table-size*: the default table size for most generators (512)");
  s7_symbol_set_access(s7, clm_table_size_symbol, s7_make_function(s7, "[acc-clm-table-size]", acc_clm_table_size, 2, 0, false, "accessor"));

  mus_file_buffer_size_symbol = s7_define_variable(s7, "*clm-file-buffer-size*", s7_make_integer(s7, MUS_DEFAULT_FILE_BUFFER_SIZE));
  s7_symbol_set_access(s7, mus_file_buffer_size_symbol, s7_make_function(s7, "[acc-mus-file-buffer-size]", acc_mus_file_buffer_size, 2, 0, false, "accessor"));

  mus_float_equal_fudge_factor_symbol = s7_define_variable(s7, "*" S_mus_float_equal_fudge_factor "*", s7_make_real(s7, 0.0000001)); /* clm.c */
  s7_symbol_set_documentation(s7, mus_float_equal_fudge_factor_symbol, "*mus-float-equal-fudge-factor*: floating point equality fudge factor");
  s7_symbol_set_access(s7, mus_float_equal_fudge_factor_symbol, s7_make_function(s7, "[acc-mus-float-equal-fudge-factor]", acc_mus_float_equal_fudge_factor, 2, 0, false, "accessor"));

  mus_array_print_length_symbol = s7_define_variable(s7, "*" S_mus_array_print_length "*", s7_make_integer(s7, MUS_DEFAULT_ARRAY_PRINT_LENGTH));
  s7_symbol_set_documentation(s7, mus_array_print_length_symbol, "*mus-array-print-length*: current clm array print length (default is 8).");
  s7_symbol_set_access(s7, mus_array_print_length_symbol, s7_make_function(s7, "[acc-mus-array-print-length]", acc_mus_array_print_length, 2, 0, false, "accessor"));
#endif

  Xen_define_real_procedure(S_radians_to_hz,        g_radians_to_hz_w,        1, 0, 0, H_radians_to_hz);
  Xen_define_real_procedure(S_hz_to_radians,        g_hz_to_radians_w,        1, 0, 0, H_hz_to_radians);
  Xen_define_real_procedure(S_radians_to_degrees,   g_radians_to_degrees_w,   1, 0, 0, H_radians_to_degrees);
  Xen_define_real_procedure(S_degrees_to_radians,   g_degrees_to_radians_w,   1, 0, 0, H_degrees_to_radians);
  Xen_define_real_procedure(S_db_to_linear,         g_db_to_linear_w,         1, 0, 0, H_db_to_linear);
  Xen_define_real_procedure(S_linear_to_db,         g_linear_to_db_w,         1, 0, 0, H_linear_to_db);
  Xen_define_real_procedure(S_even_weight,          g_even_weight_w,          1, 0, 0, H_even_weight);
  Xen_define_real_procedure(S_odd_weight,           g_odd_weight_w,           1, 0, 0, H_odd_weight);
  Xen_define_real_procedure(S_even_multiple,        g_even_multiple_w,        2, 0, 0, H_even_multiple);
  Xen_define_real_procedure(S_odd_multiple,         g_odd_multiple_w,         2, 0, 0, H_odd_multiple);
  Xen_define_integer_procedure(S_seconds_to_samples,g_seconds_to_samples_w,   1, 0, 0, H_seconds_to_samples);
  Xen_define_real_procedure(S_samples_to_seconds,   g_samples_to_seconds_w,   1, 0, 0, H_samples_to_seconds);
  Xen_define_real_procedure(S_ring_modulate,        g_ring_modulate_w,        2, 0, 0, H_ring_modulate);
  Xen_define_real_procedure(S_amplitude_modulate,   g_amplitude_modulate_w,   3, 0, 0, H_amplitude_modulate);
  Xen_define_real_procedure(S_contrast_enhancement, g_contrast_enhancement_w, 1, 1, 0, H_contrast_enhancement);
  Xen_define_real_procedure(S_dot_product,          g_dot_product_w,          2, 1, 0, H_dot_product);
#if HAVE_COMPLEX_TRIG && HAVE_COMPLEX_NUMBERS && (!HAVE_RUBY)
  Xen_define_safe_procedure(S_edot_product,         g_edot_product_w,         2, 0, 0, H_edot_product);
#endif
  Xen_define_real_procedure(S_polynomial,           g_polynomial_w,           2, 0, 0, H_polynomial);
  Xen_define_safe_procedure(S_make_fft_window,      g_make_fft_window_w,      2, 2, 0, H_make_fft_window);
  Xen_define_safe_procedure(S_mus_fft,              g_mus_fft_w,              2, 2, 0, H_mus_fft);
  Xen_define_safe_procedure(S_spectrum,             g_spectrum_w,             3, 1, 0, H_mus_spectrum); 
  Xen_define_safe_procedure(S_autocorrelate,        g_autocorrelate_w,        1, 0, 0, H_autocorrelate);
  Xen_define_safe_procedure(S_correlate,            g_correlate_w,            2, 0, 0, H_correlate);
  Xen_define_safe_procedure(S_convolution,          g_convolution_w,          2, 1, 0, H_mus_convolution);
  Xen_define_safe_procedure(S_rectangular_to_polar, g_rectangular_to_polar_w, 2, 0, 0, H_rectangular_to_polar);
  Xen_define_safe_procedure(S_rectangular_to_magnitudes, g_rectangular_to_magnitudes_w, 2, 0, 0, H_rectangular_to_magnitudes);
  Xen_define_safe_procedure(S_polar_to_rectangular, g_polar_to_rectangular_w, 2, 0, 0, H_polar_to_rectangular);
  Xen_define_real_procedure(S_array_interp,         g_array_interp_w,         2, 1, 0, H_array_interp);
  Xen_define_real_procedure(S_mus_interpolate,      g_mus_interpolate_w,      3, 2, 0, H_mus_interpolate);
  Xen_define_real_procedure(S_mus_frandom,          g_mus_frandom_w,          1, 0, 0, "random reals");
  Xen_define_integer_procedure(S_mus_irandom,       g_mus_irandom_w,          1, 0, 0, "random integers");

  Xen_define_constant(S_rectangular_window,     MUS_RECTANGULAR_WINDOW,     "The un-window, so to speak");
  Xen_define_constant(S_hann_window,            MUS_HANN_WINDOW,            "A simple raised cosine window");
  Xen_define_constant(S_welch_window,           MUS_WELCH_WINDOW,           "A triangular window squared");
  Xen_define_constant(S_parzen_window,          MUS_PARZEN_WINDOW,          "A triangular window");
  Xen_define_constant(S_bartlett_window,        MUS_BARTLETT_WINDOW,        "A triangular window");
  Xen_define_constant(S_bartlett_hann_window,   MUS_BARTLETT_HANN_WINDOW,   "A combination of the bartlett and hann windows");
  Xen_define_constant(S_bohman_window,          MUS_BOHMAN_WINDOW,          "A weighted cosine window");
  Xen_define_constant(S_flat_top_window,        MUS_FLAT_TOP_WINDOW,        "A sum of cosines window");
  Xen_define_constant(S_hamming_window,         MUS_HAMMING_WINDOW,         "A raised cosine");
  Xen_define_constant(S_blackman2_window,       MUS_BLACKMAN2_WINDOW,       "2nd order cosine window");
  Xen_define_constant(S_blackman3_window,       MUS_BLACKMAN3_WINDOW,       "3rd order cosine window");
  Xen_define_constant(S_blackman4_window,       MUS_BLACKMAN4_WINDOW,       "4th order cosine window");
  Xen_define_constant(S_blackman5_window,       MUS_BLACKMAN5_WINDOW,       "5th order cosine window");
  Xen_define_constant(S_blackman6_window,       MUS_BLACKMAN6_WINDOW,       "6th order cosine window");
  Xen_define_constant(S_blackman7_window,       MUS_BLACKMAN7_WINDOW,       "7th order cosine window");
  Xen_define_constant(S_blackman8_window,       MUS_BLACKMAN8_WINDOW,       "8th order cosine window");
  Xen_define_constant(S_blackman9_window,       MUS_BLACKMAN9_WINDOW,       "9th order cosine window");
  Xen_define_constant(S_blackman10_window,      MUS_BLACKMAN10_WINDOW,      "10th order cosine window");
  Xen_define_constant(S_exponential_window,     MUS_EXPONENTIAL_WINDOW,     "An inverted triangle from exp");
  Xen_define_constant(S_riemann_window,         MUS_RIEMANN_WINDOW,         "sinc-based window");
  Xen_define_constant(S_kaiser_window,          MUS_KAISER_WINDOW,          "Bessel I0 based window");
  Xen_define_constant(S_cauchy_window,          MUS_CAUCHY_WINDOW,          "window based on 1/(1+sqr(angle)");
  Xen_define_constant(S_poisson_window,         MUS_POISSON_WINDOW,         "window based on exp(-angle)");
  Xen_define_constant(S_gaussian_window,        MUS_GAUSSIAN_WINDOW,        "window based on exp(-sqr(angle))");
  Xen_define_constant(S_tukey_window,           MUS_TUKEY_WINDOW,           "window based on truncated cosine");
  Xen_define_constant(S_dolph_chebyshev_window, MUS_DOLPH_CHEBYSHEV_WINDOW, "window from inverse fft (using Chebyshev Tn)");
  Xen_define_constant(S_connes_window,          MUS_CONNES_WINDOW,          "triangle window squared twice");
  Xen_define_constant(S_hann_poisson_window,    MUS_HANN_POISSON_WINDOW,    "poisson window * hann window");
  Xen_define_constant(S_samaraki_window,        MUS_SAMARAKI_WINDOW,        "window from inverse fft (using Chebyshev Un)");
  Xen_define_constant(S_ultraspherical_window,  MUS_ULTRASPHERICAL_WINDOW,  "window from inverse fft (using Ultraspherical Cn)");
  Xen_define_constant(S_rv2_window,             MUS_RV2_WINDOW,             "Rife-Vincent 2nd order window (Hann extension)");
  Xen_define_constant(S_rv3_window,             MUS_RV3_WINDOW,             "Rife-Vincent 3rd order window (Hann extension)");
  Xen_define_constant(S_rv4_window,             MUS_RV4_WINDOW,             "Rife-Vincent 4th order window (Hann extension)");
  Xen_define_constant(S_mlt_sine_window,        MUS_MLT_SINE_WINDOW,        "modulated lapped transform sine window");
  Xen_define_constant(S_papoulis_window,        MUS_PAPOULIS_WINDOW,        "papoulise window");
  Xen_define_constant(S_dpss_window,            MUS_DPSS_WINDOW,            "proplate spheroidal (slepian) window");
  Xen_define_constant(S_sinc_window,            MUS_SINC_WINDOW,            "sinc (Lanczos) window");

  Xen_define_constant(S_mus_interp_linear,      MUS_INTERP_LINEAR,          "locsig/delay linear interpolation");
  Xen_define_constant(S_mus_interp_sinusoidal,  MUS_INTERP_SINUSOIDAL,      "locsig sinusoidal interpolation");
  Xen_define_constant(S_mus_interp_all_pass,    MUS_INTERP_ALL_PASS,        "delay interpolation");
  Xen_define_constant(S_mus_interp_lagrange,    MUS_INTERP_LAGRANGE,        "2nd order lagrange interpolation");
  Xen_define_constant(S_mus_interp_hermite,     MUS_INTERP_HERMITE,         "3rd order hermite interpolation");
  Xen_define_constant(S_mus_interp_none,        MUS_INTERP_NONE,            "no interpolation -- step func");
  Xen_define_constant(S_mus_interp_bezier,      MUS_INTERP_BEZIER,          "bezier interpolation");

  Xen_define_constant(S_mus_chebyshev_first_kind,   MUS_CHEBYSHEV_FIRST_KIND,  "Chebyshev polynomial of first kind, for " S_partials_to_polynomial);
  Xen_define_constant(S_mus_chebyshev_second_kind,  MUS_CHEBYSHEV_SECOND_KIND, "Chebyshev polynomial of second kind, for " S_partials_to_polynomial);
  Xen_define_constant(S_mus_chebyshev_both_kinds,   MUS_CHEBYSHEV_BOTH_KINDS,  "use both Chebyshev polynomials in polywave");

  Xen_define_safe_procedure(S_mus_describe,         g_mus_describe_w,  1, 0, 0,  H_mus_describe);
  Xen_define_safe_procedure(S_mus_file_name,        g_mus_file_name_w, 1, 0, 0,  H_mus_file_name);
  Xen_define_safe_procedure(S_mus_reset,            g_mus_reset_w,     1, 0, 0,  H_mus_reset);
  Xen_define_safe_procedure(S_mus_copy,             g_mus_copy_w,      1, 0, 0,  H_mus_copy);
  Xen_define_procedure(S_mus_run,                   g_mus_run_w,       1, 2, 0,  H_mus_run);

  Xen_define_safe_procedure(S_mus_name, g_mus_name_w, 1, 0, 0,  H_mus_name);
  Xen_define_dilambda(S_mus_phase,     g_mus_phase_w,     H_mus_phase,     S_setB S_mus_phase,     g_mus_set_phase_w,      1, 0, 2, 0);
  Xen_define_dilambda(S_mus_scaler,    g_mus_scaler_w,    H_mus_scaler,    S_setB S_mus_scaler,    g_mus_set_scaler_w,     1, 0, 2, 0);
  Xen_define_dilambda(S_mus_width,     g_mus_width_w,     H_mus_width,     S_setB S_mus_width,     g_mus_set_width_w,      1, 0, 2, 0);
  Xen_define_dilambda(S_mus_frequency, g_mus_frequency_w, H_mus_frequency, S_setB S_mus_frequency, g_mus_set_frequency_w,  1, 0, 2, 0);
  Xen_define_dilambda(S_mus_length,    g_mus_length_w,    H_mus_length,    S_setB S_mus_length,    g_mus_set_length_w,     1, 0, 2, 0);
  Xen_define_dilambda(S_mus_data,      g_mus_data_w,      H_mus_data,      S_setB S_mus_data,      g_mus_set_data_w,       1, 0, 2, 0);
  Xen_define_dilambda(S_mus_xcoeff,    g_mus_xcoeff_w,    H_mus_xcoeff,    S_setB S_mus_xcoeff,    g_mus_set_xcoeff_w,     2, 0, 3, 0);
  Xen_define_dilambda(S_mus_ycoeff,    g_mus_ycoeff_w,    H_mus_ycoeff,    S_setB S_mus_ycoeff,    g_mus_set_ycoeff_w,     2, 0, 3, 0);
  Xen_define_dilambda(S_mus_offset,    g_mus_offset_w,    H_mus_offset,    S_setB S_mus_offset,    g_mus_set_offset_w,     1, 0, 2, 0);

  Xen_define_safe_procedure(S_mus_xcoeffs,          g_mus_xcoeffs_w,           1, 0, 0, H_mus_xcoeffs);
  Xen_define_safe_procedure(S_mus_ycoeffs,          g_mus_ycoeffs_w,           1, 0, 0, H_mus_ycoeffs);
  Xen_define_safe_procedure(S_is_oscil,             g_is_oscil_w,              1, 0, 0, H_is_oscil);
  Xen_define_real_procedure(S_oscil,                g_oscil_w,                 1, 2, 0, H_oscil);
  Xen_define_safe_procedure(S_is_oscil_bank,        g_is_oscil_bank_w,         1, 0, 0, H_is_oscil_bank);
  Xen_define_real_procedure(S_oscil_bank,           g_oscil_bank_w,            1, 0, 0, H_oscil_bank);

  Xen_define_procedure(S_mus_apply,                 g_mus_apply_w,             0, 0, 1, H_mus_apply);

  Xen_define_safe_procedure(S_make_delay,           g_make_delay_w,            0, 0, 1, H_make_delay);
  Xen_define_safe_procedure(S_make_comb,            g_make_comb_w,             0, 0, 1, H_make_comb);
  Xen_define_safe_procedure(S_make_filtered_comb,   g_make_filtered_comb_w,    0, 0, 1, H_make_filtered_comb);
  Xen_define_safe_procedure(S_make_notch,           g_make_notch_w,            0, 0, 1, H_make_notch); 
  Xen_define_safe_procedure(S_make_all_pass,        g_make_all_pass_w,         0, 0, 1, H_make_all_pass);
  Xen_define_safe_procedure(S_make_moving_average,  g_make_moving_average_w,   0, 0, 1, H_make_moving_average);
  Xen_define_safe_procedure(S_make_moving_max,      g_make_moving_max_w,       0, 0, 1, H_make_moving_max);
  Xen_define_safe_procedure(S_make_moving_norm,     g_make_moving_norm_w,      0, 0, 1, H_make_moving_norm);
  Xen_define_real_procedure(S_delay,                g_delay_w,                 1, 2, 0, H_delay); 
  Xen_define_safe_procedure(S_delay_tick,           g_delay_tick_w,            1, 1, 0, H_delay_tick); 
  Xen_define_real_procedure(S_tap,                  g_tap_w,                   1, 1, 0, H_tap);
  Xen_define_real_procedure(S_notch,                g_notch_w,                 1, 2, 0, H_notch);
  Xen_define_real_procedure(S_comb,                 g_comb_w,                  1, 2, 0, H_comb);
  Xen_define_real_procedure(S_filtered_comb,        g_filtered_comb_w,         1, 2, 0, H_filtered_comb);
  Xen_define_real_procedure(S_all_pass,             g_all_pass_w,              1, 2, 0, H_all_pass);
  Xen_define_real_procedure(S_moving_average,       g_moving_average_w,        1, 1, 0, H_moving_average);
  Xen_define_real_procedure(S_moving_max,           g_moving_max_w,            1, 1, 0, H_moving_max);
  Xen_define_real_procedure(S_moving_norm,          g_moving_norm_w,           1, 1, 0, H_moving_norm);
  Xen_define_safe_procedure(S_is_tap,               g_is_tap_w,                1, 0, 0, H_is_tap);
  Xen_define_safe_procedure(S_is_delay,             g_is_delay_w,              1, 0, 0, H_is_delay);
  Xen_define_safe_procedure(S_is_notch,             g_is_notch_w,              1, 0, 0, H_is_notch);
  Xen_define_safe_procedure(S_is_comb,              g_is_comb_w,               1, 0, 0, H_is_comb);
  Xen_define_safe_procedure(S_is_filtered_comb,     g_is_filtered_comb_w,      1, 0, 0, H_is_filtered_comb);
  Xen_define_safe_procedure(S_is_all_pass,          g_is_all_pass_w,           1, 0, 0, H_is_all_pass);
  Xen_define_safe_procedure(S_is_moving_average,    g_is_moving_average_w,     1, 0, 0, H_is_moving_average);
  Xen_define_safe_procedure(S_is_moving_max,        g_is_moving_max_w,         1, 0, 0, H_is_moving_max);
  Xen_define_safe_procedure(S_is_moving_norm,       g_is_moving_norm_w,        1, 0, 0, H_is_moving_norm);

  Xen_define_real_procedure(S_comb_bank,            g_comb_bank_w,             1, 1, 0, H_comb_bank);
  Xen_define_safe_procedure(S_is_comb_bank,         g_is_comb_bank_w,          1, 0, 0, H_is_comb_bank);
  Xen_define_safe_procedure(S_make_comb_bank,       g_make_comb_bank_w,        1, 0, 0, H_make_comb_bank);

  Xen_define_real_procedure(S_filtered_comb_bank,   g_filtered_comb_bank_w,    1, 1, 0, H_filtered_comb_bank);
  Xen_define_safe_procedure(S_is_filtered_comb_bank, g_is_filtered_comb_bank_w,  1, 0, 0, H_is_filtered_comb_bank);
  Xen_define_safe_procedure(S_make_filtered_comb_bank, g_make_filtered_comb_bank_w, 1, 0, 0, H_make_filtered_comb_bank);

  Xen_define_real_procedure(S_all_pass_bank,        g_all_pass_bank_w,         1, 1, 0, H_all_pass_bank);
  Xen_define_safe_procedure(S_is_all_pass_bank,     g_is_all_pass_bank_w,      1, 0, 0, H_is_all_pass_bank);
  Xen_define_safe_procedure(S_make_all_pass_bank,   g_make_all_pass_bank_w,    1, 0, 0, H_make_all_pass_bank);

  Xen_define_real_procedure(S_pink_noise,           g_pink_noise_w,            1, 0, 0, H_pink_noise);

  Xen_define_real_procedure(S_out_bank,             g_out_bank_w,              3, 0, 0, H_out_bank);

  Xen_define_dilambda(S_mus_feedback,  g_mus_feedback_w, H_mus_feedback, S_setB S_mus_feedback, g_mus_set_feedback_w,  1, 0, 2, 0);
  Xen_define_dilambda(S_mus_feedforward, g_mus_feedforward_w, H_mus_feedforward, S_setB S_mus_feedforward, g_mus_set_feedforward_w,  1, 0, 2, 0);

  Xen_define_safe_procedure(S_make_rand,            g_make_rand_w,             0, 0, 1, H_make_rand);
  Xen_define_safe_procedure(S_make_rand_interp,     g_make_rand_interp_w,      0, 0, 1, H_make_rand_interp);
#if HAVE_RUBY
  rb_define_alias(rb_mKernel, "kernel_rand", "rand");
#endif
  Xen_define_real_procedure(S_rand,                 g_rand_w,                  1, 1, 0, H_rand);
  Xen_define_real_procedure(S_rand_interp,          g_rand_interp_w,           1, 1, 0, H_rand_interp);
  Xen_define_safe_procedure(S_is_rand,              g_is_rand_w,               1, 0, 0, H_is_rand);
  Xen_define_safe_procedure(S_is_rand_interp,       g_is_rand_interp_w,        1, 0, 0, H_is_rand_interp);
  Xen_define_real_procedure(S_mus_random,           g_mus_random_w,            1, 0, 0, H_mus_random);

  Xen_define_dilambda(S_mus_rand_seed, g_mus_rand_seed_w, H_mus_rand_seed, S_setB S_mus_rand_seed, g_mus_set_rand_seed_w, 0, 0, 1, 0);

  Xen_define_real_procedure(S_ncos,                 g_ncos_w,                  1, 1, 0, H_ncos);
  Xen_define_safe_procedure(S_is_ncos,              g_is_ncos_w,               1, 0, 0, H_is_ncos);
  Xen_define_real_procedure(S_nsin,                 g_nsin_w,                  1, 1, 0, H_nsin);
  Xen_define_safe_procedure(S_is_nsin,              g_is_nsin_w,               1, 0, 0, H_is_nsin);

  Xen_define_safe_procedure(S_is_table_lookup,      g_is_table_lookup_w,       1, 0, 0, H_is_table_lookup);
  Xen_define_safe_procedure(S_make_table_lookup,    g_make_table_lookup_w,     0, 0, 1, H_make_table_lookup);
  Xen_define_real_procedure(S_table_lookup,         g_table_lookup_w,          1, 1, 0, H_table_lookup);
  Xen_define_safe_procedure(S_partials_to_wave,     g_partials_to_wave_w,      1, 2, 0, H_partials_to_wave);
  Xen_define_safe_procedure(S_phase_partials_to_wave, g_phase_partials_to_wave_w, 1, 2, 0, H_phase_partials_to_wave);


  Xen_define_safe_procedure(S_make_sawtooth_wave,   g_make_sawtooth_wave_w,    0, 6, 0, H_make_sawtooth_wave);
  Xen_define_real_procedure(S_sawtooth_wave,        g_sawtooth_wave_w,         1, 1, 0, H_sawtooth_wave);
  Xen_define_safe_procedure(S_is_sawtooth_wave,     g_is_sawtooth_wave_w,      1, 0, 0, H_is_sawtooth_wave);
  Xen_define_safe_procedure(S_make_triangle_wave,   g_make_triangle_wave_w,    0, 6, 0, H_make_triangle_wave);
  Xen_define_real_procedure(S_triangle_wave,        g_triangle_wave_w,         1, 1, 0, H_triangle_wave);
  Xen_define_safe_procedure(S_is_triangle_wave,     g_is_triangle_wave_w,      1, 0, 0, H_is_triangle_wave);
  Xen_define_safe_procedure(S_make_square_wave,     g_make_square_wave_w,      0, 6, 0, H_make_square_wave);
  Xen_define_real_procedure(S_square_wave,          g_square_wave_w,           1, 1, 0, H_square_wave);
  Xen_define_safe_procedure(S_is_square_wave,       g_is_square_wave_w,        1, 0, 0, H_is_square_wave);
  Xen_define_safe_procedure(S_make_pulse_train,     g_make_pulse_train_w,      0, 6, 0, H_make_pulse_train);
  Xen_define_real_procedure(S_pulse_train,          g_pulse_train_w,           1, 1, 0, H_pulse_train);
  Xen_define_safe_procedure(S_is_pulse_train,       g_is_pulse_train_w,        1, 0, 0, H_is_pulse_train);


  Xen_define_safe_procedure(S_make_pulsed_env,      g_make_pulsed_env_w,       3, 0, 0, H_make_pulsed_env);
  Xen_define_real_procedure(S_pulsed_env,           g_pulsed_env_w,            1, 1, 0, H_pulsed_env);
  Xen_define_safe_procedure(S_is_pulsed_env,        g_is_pulsed_env_w,         1, 0, 0, H_is_pulsed_env);


  Xen_define_real_procedure(S_asymmetric_fm,        g_asymmetric_fm_w,         1, 2, 0, H_asymmetric_fm);
  Xen_define_safe_procedure(S_is_asymmetric_fm,     g_is_asymmetric_fm_w,      1, 0, 0, H_is_asymmetric_fm);


  Xen_define_safe_procedure(S_make_one_zero,        g_make_one_zero_w,         0, 4, 0, H_make_one_zero);
  Xen_define_real_procedure(S_one_zero,             g_one_zero_w,              1, 1, 0, H_one_zero);
  Xen_define_safe_procedure(S_is_one_zero,          g_is_one_zero_w,           1, 0, 0, H_is_one_zero);
  Xen_define_safe_procedure(S_make_one_pole,        g_make_one_pole_w,         0, 4, 0, H_make_one_pole);
  Xen_define_real_procedure(S_one_pole,             g_one_pole_w,              1, 1, 0, H_one_pole);
  Xen_define_safe_procedure(S_is_one_pole,          g_is_one_pole_w,           1, 0, 0, H_is_one_pole);
  Xen_define_safe_procedure(S_make_two_zero,        g_make_two_zero_w,         0, 6, 0, H_make_two_zero);
  Xen_define_real_procedure(S_two_zero,             g_two_zero_w,              1, 1, 0, H_two_zero);
  Xen_define_safe_procedure(S_is_two_zero,          g_is_two_zero_w,           1, 0, 0, H_is_two_zero);
  Xen_define_safe_procedure(S_make_two_pole,        g_make_two_pole_w,         0, 6, 0, H_make_two_pole);
  Xen_define_real_procedure(S_two_pole,             g_two_pole_w,              1, 1, 0, H_two_pole);
  Xen_define_safe_procedure(S_is_two_pole,          g_is_two_pole_w,           1, 0, 0, H_is_two_pole)  ;

  Xen_define_safe_procedure(S_make_wave_train,      g_make_wave_train_w,       0, 0, 1, H_make_wave_train);
  Xen_define_real_procedure(S_wave_train,           g_wave_train_w,            1, 1, 0, H_wave_train);
  Xen_define_safe_procedure(S_is_wave_train,        g_is_wave_train_w,         1, 0, 0, H_is_wave_train);

  Xen_define_safe_procedure(S_is_formant,           g_is_formant_w,            1, 0, 0, H_is_formant);
  Xen_define_safe_procedure(S_make_formant,         g_make_formant_w,          0, 4, 0, H_make_formant);
  Xen_define_real_procedure(S_formant,              g_formant_w,               1, 2, 0, H_formant);

  Xen_define_real_procedure(S_formant_bank,         g_formant_bank_w,          2, 0, 0, H_formant_bank);
  Xen_define_safe_procedure(S_is_formant_bank,      g_is_formant_bank_w,       1, 0, 0, H_is_formant_bank);
  Xen_define_safe_procedure(S_make_formant_bank,    g_make_formant_bank_w,     1, 1, 0, H_make_formant_bank);

  Xen_define_safe_procedure(S_is_firmant,           g_is_firmant_w,            1, 0, 0, H_is_firmant);
  Xen_define_safe_procedure(S_make_firmant,         g_make_firmant_w,          0, 4, 0, H_make_firmant);
  Xen_define_real_procedure(S_firmant,              g_firmant_w,               1, 2, 0, H_firmant);

  Xen_define_safe_procedure(S_is_one_pole_all_pass,  g_is_one_pole_all_pass_w,   1, 0, 0, H_is_one_pole_all_pass);
  Xen_define_safe_procedure(S_make_one_pole_all_pass, g_make_one_pole_all_pass_w, 2, 0, 0, H_make_one_pole_all_pass);
  Xen_define_real_procedure(S_one_pole_all_pass,    g_one_pole_all_pass_w,     1, 1, 0, H_one_pole_all_pass);

  Xen_define_safe_procedure(S_mus_set_formant_frequency, g_set_formant_frequency_w, 2, 0, 0, H_mus_set_formant_frequency);
  Xen_define_safe_procedure(S_mus_set_formant_radius_and_frequency, g_set_formant_radius_and_frequency_w, 3, 0, 0, H_mus_set_formant_radius_and_frequency);


  Xen_define_safe_procedure(S_make_polyshape,       g_make_polyshape_w,        0, 0, 1, H_make_polyshape);
  Xen_define_real_procedure(S_polyshape,            g_polyshape_w,             1, 2, 0, H_polyshape);
  Xen_define_safe_procedure(S_is_polyshape,         g_is_polyshape_w,          1, 0, 0, H_is_polyshape);
  Xen_define_safe_procedure(S_partials_to_polynomial, g_partials_to_polynomial_w, 1, 1, 0, H_partials_to_polynomial);
  Xen_define_safe_procedure(S_normalize_partials,   g_normalize_partials_w,    1, 0, 0, H_normalize_partials);
  Xen_define_real_procedure(S_mus_chebyshev_t_sum,  g_chebyshev_t_sum_w,       2, 0, 0, H_chebyshev_t_sum);
  Xen_define_real_procedure(S_mus_chebyshev_u_sum,  g_chebyshev_u_sum_w,       2, 0, 0, H_chebyshev_u_sum);
  Xen_define_real_procedure(S_mus_chebyshev_tu_sum, g_chebyshev_tu_sum_w,      3, 0, 0, H_chebyshev_tu_sum);
  Xen_define_safe_procedure(S_make_polywave,        g_make_polywave_w,         0, 0, 1, H_make_polywave);
  Xen_define_real_procedure(S_polywave,             g_polywave_w,              1, 1, 0, H_polywave);
  Xen_define_safe_procedure(S_is_polywave,          g_is_polywave_w,           1, 0, 0, H_is_polywave);

  Xen_define_safe_procedure(S_make_nrxysin,         g_make_nrxysin_w,          0, 0, 1, H_make_nrxysin);
  Xen_define_real_procedure(S_nrxysin,              g_nrxysin_w,               1, 1, 0, H_nrxysin);
  Xen_define_safe_procedure(S_is_nrxysin,           g_is_nrxysin_w,            1, 0, 0, H_is_nrxysin);
  Xen_define_safe_procedure(S_make_nrxycos,         g_make_nrxycos_w,          0, 0, 1, H_make_nrxycos);
  Xen_define_real_procedure(S_nrxycos,              g_nrxycos_w,               1, 1, 0, H_nrxycos);
  Xen_define_safe_procedure(S_is_nrxycos,           g_is_nrxycos_w,            1, 0, 0, H_is_nrxycos);

  Xen_define_safe_procedure(S_make_rxyksin,         g_make_rxyksin_w,          0, 0, 1, H_make_rxyksin);
  Xen_define_real_procedure(S_rxyksin,              g_rxyksin_w,               1, 1, 0, H_rxyksin);
  Xen_define_safe_procedure(S_is_rxyksin,           g_is_rxyksin_w,            1, 0, 0, H_is_rxyksin);
  Xen_define_safe_procedure(S_make_rxykcos,         g_make_rxykcos_w,          0, 0, 1, H_make_rxykcos);
  Xen_define_real_procedure(S_rxykcos,              g_rxykcos_w,               1, 1, 0, H_rxykcos);
  Xen_define_safe_procedure(S_is_rxykcos,           g_is_rxykcos_w,            1, 0, 0, H_is_rxykcos);


  Xen_define_safe_procedure(S_make_filter,          g_make_filter_w,           0, 6, 0, H_make_filter);
  Xen_define_real_procedure(S_filter,               g_filter_w,                1, 1, 0, H_filter);
  Xen_define_safe_procedure(S_is_filter,            g_is_filter_w,             1, 0, 0, H_is_filter);
  Xen_define_safe_procedure(S_make_fir_coeffs,      g_make_fir_coeffs_w,       2, 0, 0, H_make_fir_coeffs);
  Xen_define_safe_procedure(S_make_fir_filter,      g_make_fir_filter_w,       0, 4, 0, H_make_fir_filter);
  Xen_define_real_procedure(S_fir_filter,           g_fir_filter_w,            1, 1, 0, H_fir_filter);
  Xen_define_safe_procedure(S_is_fir_filter,        g_is_fir_filter_w,         1, 0, 0, H_is_fir_filter);
  Xen_define_safe_procedure(S_make_iir_filter,      g_make_iir_filter_w,       0, 4, 0, H_make_iir_filter);
  Xen_define_real_procedure(S_iir_filter,           g_iir_filter_w,            1, 1, 0, H_iir_filter);
  Xen_define_safe_procedure(S_is_iir_filter,        g_is_iir_filter_w,         1, 0, 0, H_is_iir_filter);
  Xen_define_integer_procedure(S_mus_order,         g_mus_order_w,             1, 0, 0, H_mus_order);
  Xen_define_integer_procedure(S_mus_type,          g_mus_type_w,              1, 0, 0, H_mus_type);


  Xen_define_safe_procedure(S_is_env,               g_is_env_w,                1, 0, 0, H_is_env);
  Xen_define_real_procedure(S_env,                  g_env_w,                   1, 0, 0, H_env);
  Xen_define_safe_procedure(S_make_env,             g_make_env_w,              0, 0, 1, H_make_env);
  Xen_define_real_procedure(S_env_interp,           g_env_interp_w,            2, 0, 0, H_env_interp);
  Xen_define_safe_procedure(S_envelope_interp,      g_envelope_interp_w,       2, 1, 0, H_envelope_interp);
  Xen_define_procedure(S_env_any,                   g_env_any_w,               2, 0, 0, H_env_any);


  Xen_define_safe_procedure(S_is_locsig,            g_is_locsig_w,             1, 0, 0, H_is_locsig);
  Xen_define_safe_procedure(S_locsig,               g_locsig_w,                3, 0, 0, H_locsig);
  Xen_define_safe_procedure(S_make_locsig,          g_make_locsig_w,           0, 0, 1, H_make_locsig);
  Xen_define_safe_procedure(S_move_locsig,          g_move_locsig_w,           3, 0, 0, H_move_locsig);
  Xen_define_integer_procedure(S_mus_channels,      g_mus_channels_w,          1, 0, 0, H_mus_channels);

#if HAVE_SCHEME || HAVE_FORTH
  Xen_define_dilambda(S_locsig_ref, g_locsig_ref_w, H_locsig_ref, S_setB S_locsig_ref, g_locsig_set_w,  2, 0, 3, 0);
#endif

#if HAVE_RUBY
  Xen_define_procedure(S_locsig_ref,                g_locsig_ref_w,            2, 0, 0, H_locsig_ref);
#endif

  Xen_define_safe_procedure(S_locsig_set,           g_locsig_set_w,            3, 0, 0, H_locsig_set);

#if HAVE_SCHEME || HAVE_FORTH
  Xen_define_dilambda(S_locsig_reverb_ref, g_locsig_reverb_ref_w, H_locsig_reverb_ref, S_locsig_reverb_set, g_locsig_reverb_set_w,  2, 0, 3, 0);
#endif

#if HAVE_RUBY
  Xen_define_procedure(S_locsig_reverb_ref,         g_locsig_reverb_ref_w,     2, 0, 0, H_locsig_reverb_ref);
#endif

  Xen_define_safe_procedure(S_locsig_reverb_set,    g_locsig_reverb_set_w,     3, 0, 0, H_locsig_reverb_set);
  Xen_define_dilambda(S_locsig_type,   g_locsig_type_w, H_locsig_type, S_setB S_locsig_type, g_set_locsig_type_w,  0, 0, 1, 0);

  Xen_define_safe_procedure(S_is_move_sound,        g_is_move_sound_w,         1, 0, 0, H_is_move_sound);
  Xen_define_safe_procedure(S_move_sound,           g_move_sound_w,            3, 0, 0, H_move_sound);
  Xen_define_safe_procedure(S_make_move_sound,      g_make_move_sound_w,       1, 2, 0, H_make_move_sound);

  Xen_define_safe_procedure(S_is_file_to_sample,    g_is_file_to_sample_w,     1, 0, 0, H_is_file_to_sample);
  Xen_define_safe_procedure(S_make_file_to_sample,  g_make_file_to_sample_w,   1, 1, 0, H_make_file_to_sample);
  Xen_define_real_procedure(S_file_to_sample,       g_file_to_sample_w,        2, 1, 0, H_file_to_sample);
  Xen_define_safe_procedure(S_is_sample_to_file,    g_is_sample_to_file_w,     1, 0, 0, H_is_sample_to_file);
  Xen_define_safe_procedure(S_make_sample_to_file,  g_make_sample_to_file_w,   1, 4, 0, H_make_sample_to_file);
  Xen_define_safe_procedure(S_continue_sample_to_file, g_continue_sample_to_file_w, 1, 0, 0, H_continue_sample_to_file);
  Xen_define_safe_procedure(S_sample_to_file,       g_sample_to_file_w,        4, 0, 0, H_sample_to_file);
  Xen_define_safe_procedure(S_sample_to_file_add,   g_sample_to_file_add_w,    2, 0, 0, H_sample_to_file_add);

  Xen_define_safe_procedure(S_is_file_to_frample,   g_is_file_to_frample_w,    1, 0, 0, H_is_file_to_frample);
  Xen_define_safe_procedure(S_make_file_to_frample, g_make_file_to_frample_w,  1, 1, 0, H_make_file_to_frample);
  Xen_define_safe_procedure(S_file_to_frample,      g_file_to_frample_w,       2, 1, 0, H_file_to_frample);
  Xen_define_safe_procedure(S_continue_frample_to_file, g_continue_frample_to_file_w, 1, 0, 0, H_continue_frample_to_file);
  Xen_define_safe_procedure(S_is_frample_to_file,   g_is_frample_to_file_w,    1, 0, 0, H_is_frample_to_file);
  Xen_define_safe_procedure(S_frample_to_file,      g_frample_to_file_w,       3, 0, 0, H_frample_to_file);
  Xen_define_safe_procedure(S_make_frample_to_file, g_make_frample_to_file_w,  1, 4, 0, H_make_frample_to_file);

  Xen_define_safe_procedure(S_is_mus_input,         g_is_mus_input_w,          1, 0, 0, H_is_mus_input);
  Xen_define_safe_procedure(S_is_mus_output,        g_is_mus_output_w,         1, 0, 0, H_is_mus_output);
  Xen_define_real_procedure(S_in_any,               g_in_any_w,                3, 0, 0, H_in_any);
  Xen_define_real_procedure(S_ina,                  g_ina_w,                   2, 0, 0, H_ina);  
  Xen_define_real_procedure(S_inb,                  g_inb_w,                   2, 0, 0, H_inb);
  Xen_define_safe_procedure(S_out_any,              g_out_any_w,               3, 1, 0, H_out_any);
  Xen_define_safe_procedure(S_outa,                 g_outa_w,                  2, 1, 0, H_outa);
  Xen_define_safe_procedure(S_outb,                 g_outb_w,                  2, 1, 0, H_outb);
  Xen_define_safe_procedure(S_outc,                 g_outc_w,                  2, 1, 0, H_outc);
  Xen_define_safe_procedure(S_outd,                 g_outd_w,                  2, 1, 0, H_outd);
  Xen_define_safe_procedure(S_mus_close,            g_mus_close_w,             1, 0, 0, H_mus_close);

  Xen_define_dilambda(S_mus_file_buffer_size, g_mus_file_buffer_size_w, H_mus_file_buffer_size,
		      S_setB S_mus_file_buffer_size, g_mus_set_file_buffer_size_w,  0, 0, 1, 0);


  Xen_define_safe_procedure(S_is_readin,            g_is_readin_w,             1, 0, 0, H_is_readin);
  Xen_define_real_procedure(S_readin,               g_readin_w,                1, 0, 0, H_readin);
  Xen_define_safe_procedure(S_make_readin,          g_make_readin_w,           0, 0, 1, H_make_readin);
  Xen_define_integer_procedure(S_mus_channel,       g_mus_channel_w,           1, 0, 0, H_mus_channel);
  Xen_define_integer_procedure(S_mus_interp_type,   g_mus_interp_type_w,       1, 0, 0, H_mus_interp_type);

  Xen_define_dilambda(S_mus_location,  g_mus_location_w, H_mus_location, S_setB S_mus_location, g_mus_set_location_w,  1, 0, 2, 0);
  Xen_define_dilambda(S_mus_increment, g_mus_increment_w, H_mus_increment, S_setB S_mus_increment, g_mus_set_increment_w,  1, 0, 2, 0);

  Xen_define_safe_procedure(S_is_granulate,         g_is_granulate_w,          1, 0, 0, H_is_granulate);
  Xen_define_real_procedure(S_granulate,            g_granulate_w,             1, 2, 0, H_granulate);
  Xen_define_procedure(S_make_granulate,            g_make_granulate_w,        0, 0, 1, H_make_granulate);

  Xen_define_dilambda(S_mus_ramp, g_mus_ramp_w, H_mus_ramp, S_setB S_mus_ramp, g_mus_set_ramp_w,  1, 0, 2, 0);


  Xen_define_safe_procedure(S_clear_sincs,          g_mus_clear_sincs_w,       0, 0, 0, "clears out any sinc tables");
  Xen_define_safe_procedure(S_is_src,               g_is_src_w,                1, 0, 0, H_is_src);
  Xen_define_real_procedure(S_src,                  g_src_w,                   1, 2, 0, H_src); /* live dangerously? */
  Xen_define_procedure(S_make_src,                  g_make_src_w,              0, 6, 0, H_make_src);


  Xen_define_safe_procedure(S_is_convolve,          g_is_convolve_w,           1, 0, 0, H_is_convolve);
  Xen_define_real_procedure(S_convolve,             g_convolve_w,              1, 1, 0, H_convolve_gen);
  Xen_define_procedure(S_make_convolve,             g_make_convolve_w,         0, 0, 1, H_make_convolve);
  Xen_define_safe_procedure(S_convolve_files,       g_convolve_files_w,        2, 2, 0, H_convolve_files);

  Xen_define_safe_procedure(S_is_phase_vocoder,     g_is_phase_vocoder_w,      1, 0, 0, H_is_phase_vocoder);
  Xen_define_real_procedure(S_phase_vocoder,        g_phase_vocoder_w,         1, 4, 0, H_phase_vocoder);
  Xen_define_procedure(S_make_phase_vocoder,        g_make_phase_vocoder_w,    0, 0, 1, H_make_phase_vocoder);
  Xen_define_safe_procedure(S_phase_vocoder_amp_increments, g_phase_vocoder_amp_increments_w, 1, 0, 0, H_phase_vocoder_amp_increments);
  Xen_define_safe_procedure(S_phase_vocoder_amps,   g_phase_vocoder_amps_w,    1, 0, 0, H_phase_vocoder_amps);
  Xen_define_safe_procedure(S_phase_vocoder_freqs,  g_phase_vocoder_freqs_w,   1, 0, 0, H_phase_vocoder_freqs);
  Xen_define_safe_procedure(S_phase_vocoder_phases, g_phase_vocoder_phases_w,  1, 0, 0, H_phase_vocoder_phases);
  Xen_define_safe_procedure(S_phase_vocoder_phase_increments, g_phase_vocoder_phase_increments_w, 1, 0, 0, H_phase_vocoder_phase_increments);

  Xen_define_dilambda(S_mus_hop, g_mus_hop_w, H_mus_hop, S_setB S_mus_hop, g_mus_set_hop_w,  1, 0, 2, 0);

  Xen_define_safe_procedure(S_make_ssb_am,          g_make_ssb_am_w,           0, 4, 0, H_make_ssb_am); 
  Xen_define_real_procedure(S_ssb_am,               g_ssb_am_w,                1, 2, 0, H_ssb_am);
  Xen_define_safe_procedure(S_is_ssb_am,            g_is_ssb_am_w,             1, 0, 0, H_is_ssb_am);

  Xen_define_safe_procedure(S_is_mus_generator,     g_is_mus_generator_w,      1, 0, 0, H_is_mus_generator);

  Xen_define_variable(S_output, clm_output, Xen_false);
  Xen_define_variable(S_reverb, clm_reverb, Xen_false);

#if HAVE_SCHEME
  {
    s7_pointer clm_output_accessor, clm_reverb_accessor;
    /* these are globals in s7, so they aren't going to move */
    clm_output_slot = s7_slot(s7, clm_output);
    clm_reverb_slot = s7_slot(s7, clm_reverb);
    out_any_2 = out_any_2_to_mus_xen;
    /* these can't be safe functions */
    clm_output_accessor = s7_make_function(s7, "(set " S_output ")", g_clm_output_set, 2, 0, false, "called if " S_output " is set");
    s7_symbol_set_access(s7, s7_make_symbol(s7, S_output), clm_output_accessor);

    clm_reverb_accessor = s7_make_function(s7, "(set " S_reverb ")", g_clm_reverb_set, 2, 0, false, "called if " S_reverb " is set");
    s7_symbol_set_access(s7, s7_make_symbol(s7, S_reverb), clm_reverb_accessor);
  }
#endif

#if HAVE_SCHEME && (!_MSC_VER)
  Xen_define_safe_procedure(S_get_internal_real_time, g_get_internal_real_time_w, 0, 0, 0, H_get_internal_real_time);
  Xen_define_constant(S_internal_time_units_per_second, 1, "units used by " S_get_internal_real_time);
#endif

#if HAVE_SCHEME
  s7_define_safe_function_star(s7, S_make_oscil, g_make_oscil, "(frequency 0.0) (initial-phase 0.0)", H_make_oscil);
  s7_define_safe_function_star(s7, S_make_ncos,  g_make_ncos,  "(frequency 0.0) (n 1)",               H_make_ncos);
#else
  Xen_define_safe_procedure(S_make_oscil,          g_make_oscil_w,          0, 4, 0, H_make_oscil);
  Xen_define_safe_procedure(S_make_ncos,           g_make_ncos_w,           0, 4, 0, H_make_ncos); 
#endif
  Xen_define_safe_procedure(S_make_oscil_bank,     g_make_oscil_bank_w,     2, 2, 0, H_make_oscil_bank);
  Xen_define_safe_procedure(S_make_nsin,           g_make_nsin_w,           0, 4, 0, H_make_nsin); 
  Xen_define_safe_procedure(S_make_asymmetric_fm,  g_make_asymmetric_fm_w,  0, 8, 0, H_make_asymmetric_fm);

  Xen_define_safe_procedure(S_mus_file_mix,         g_mus_file_mix_w,          0, 0, 1, H_mus_file_mix);
  Xen_define_safe_procedure(S_mus_file_mix_with_envs, g_mus_file_mix_with_envs_w, 0, 0, 1, H_mus_file_mix_with_envs); /* actually 8 2 0 I think */
  Xen_define_safe_procedure(S_frample_to_frample,   g_frample_to_frample_w,    5, 0, 0, H_frample_to_frample);

#if HAVE_SCHEME
  init_choosers(s7);
#endif



  /* -------- clm-print (see also snd-xen.c) -------- */
#if (!USE_SND)

#if HAVE_FORTH
Xen_eval_C_string("<'> fth-print alias clm-print ( fmt args -- )"); 
#endif

#if HAVE_RUBY
  Xen_eval_C_string("def clm_print(str, *args)\n\
                      $stdout.print format(str, *args)\n\
                      end");
#endif
#endif

  Xen_provide_feature("clm");
  {
    char *clm_version;
    clm_version = mus_format("clm%d", MUS_VERSION);
    Xen_provide_feature(clm_version);
    free(clm_version);
  }

#if HAVE_SCHEME && (!_MSC_VER)
  {
    struct timezone z0;
    gettimeofday(&overall_start_time, &z0);
  }
#endif
}


void Init_sndlib(void)
{
  mus_sndlib_xen_initialize();
  mus_vct_init();
  mus_xen_init();

#if HAVE_SCHEME
  if (sizeof(mus_float_t) != sizeof(s7_double))
    fprintf(stderr, "in s7-clm, s7_double must match mus_float_t.  Currently s7_double has %d bytes, but mus_float_t has %d\n", 
	    (int)sizeof(s7_double), 
	    (int)sizeof(mus_float_t));

  init_choices();
#endif
}

#if HAVE_SCHEME
void s7_init_sndlib(s7_scheme *sc)
{
  s7_xen_initialize(sc);
  Init_sndlib();
}
#endif
