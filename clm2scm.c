/* tie CLM module into Guile/Scheme */
/* see clm.html and extsnd.html for full documentation */

/* This module uses the Float array support in vct.c -- it needs to be loaded with vct.o */

/* every generator that has embedded arrays handles these through an extra layer of
 * pointers; the problem here is that we allow the caller to access and set these directly,
 * (and don't want to copy data unnecessarily), so we can easily have many pointers
 * floating around to the same C memory; there's no way at this level to set up
 * reference counters, so in C, the various free_<gen> functions check that they
 * allocated the given memory (and all vct objects are allocated elsewhere),
 * before freeing an embedded array; then here, all such arrays are wrapped up 
 * as separate SCM objects and at every level only the bottom-most reference allows 
 * the free to go forward.
 */

#if defined(HAVE_CONFIG_H)
  #include "config.h"
#endif

#ifndef HAVE_GUILE
  #define HAVE_GUILE 1
#endif

#if HAVE_GUILE

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

#ifndef HAVE_SNDLIB
  #define HAVE_SNDLIB 1
#endif

#if USE_SND
  #include "snd.h"
#endif

#include "clm.h"
#include "vct.h"
#include "sg.h"

#ifndef CALLOC
  #define CALLOC(a,b)  calloc(a,b)
  #define MALLOC(a,b)  malloc(a,b)
  #define FREE(a)      free(a)
  #define REALLOC(a,b) realloc(a,b)
#endif

void init_mus2scm_module(void);

static void mus_error2scm(int type, char *msg)
{
  scm_misc_error("mus_error",msg,SCM_EOL);
}

static int g_scm2int(SCM obj)
{
  /* don't want errors here about floats with non-zero fractions etc */
  if (SCM_INUMP(obj))
    return(SCM_INUM(obj));
  else
    if (gh_number_p(obj))
      return((int)scm_num2dbl(obj,"g_scm2int"));
  return(0);
}

#if (!HAVE_GUILE_1_3_0)
/* new version uses guile's built-in keyword support */

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

#define NUM_KEYWORDS 56
enum {C_frequency,C_initial_phase,C_wave,C_cosines,C_amplitude,
      C_r,C_ratio,C_size,C_a0,C_a1,C_a2,C_b1,C_b2,C_max_size,
      C_input,C_srate,C_file,C_channel,C_start,
      C_initial_contents,C_initial_element,C_scaler,C_feedforward,C_feedback,
      C_radius,C_gain,C_partials,C_fill_time,C_a,C_n,
      C_order,C_x_coeffs,C_y_coeffs,C_envelope,C_base,C_duration,C_offset,C_end,
      C_direction,C_degree,C_distance,C_reverb,C_output,C_fft_size,
      C_expansion,C_length,C_hop,C_ramp,C_jitter,
      C_type,C_format,C_comment,C_channels,C_filter,C_revout,C_width
};

static char *keywords[NUM_KEYWORDS] = {SC_frequency,SC_initial_phase,SC_wave,SC_cosines,SC_amplitude,
				       SC_r,SC_ratio,SC_size,SC_a0,SC_a1,SC_a2,SC_b1,SC_b2,SC_max_size,
				       SC_input,SC_srate,SC_file,SC_channel,SC_start,
				       SC_initial_contents,SC_initial_element,SC_scaler,SC_feedforward,SC_feedback,
				       SC_radius,SC_gain,SC_partials,SC_fill_time,SC_a,SC_n,
				       SC_order,SC_x_coeffs,SC_y_coeffs,SC_envelope,SC_base,SC_duration,SC_offset,SC_end,
				       SC_direction,SC_degree,SC_distance,SC_reverb,SC_output,SC_fft_size,
				       SC_expansion,SC_length,SC_hop,SC_ramp,SC_jitter,
				       SC_type,SC_format,SC_comment,SC_channels,SC_filter,SC_revout,SC_width
};
static SCM all_keys[NUM_KEYWORDS];

static void init_keywords(void)
{
  int i;
  for (i=0;i<NUM_KEYWORDS;i++) 
    all_keys[i] = scm_c_make_keyword(keywords[i]);
}

static int keyword_p(SCM obj) {return(SCM_NFALSEP(scm_keyword_p(obj)));}

static int decode_keywords(char *caller, int nkeys, SCM *keys, int nargs, SCM *args, int *orig)
{
  /* implement the "optional-key" notion in CLM */
  int arg_ctr = 0,key_start = 0,rtn_ctr = 0,i,keying = 0,key_found = 0;
  SCM key;
  arg_ctr = 0;
  while ((arg_ctr < nargs) && (!(SCM_UNBNDP(args[arg_ctr]))))
    {
      if (!(keyword_p(args[arg_ctr])))
	{
	  if (keying) {fprintf(stderr,"unmatched value within keyword section?");}
	  /* type checking on the actual values has to be the caller's problem */
	  if (arg_ctr > nkeys) {fprintf(stderr,"%s (%d > %d) ran out of key space!",caller,arg_ctr,nkeys);}
	  keys[arg_ctr] = args[arg_ctr];
	  orig[arg_ctr] = arg_ctr;
	  arg_ctr++;
	  key_start = arg_ctr;
	  rtn_ctr++;
	}
      else
	{
	  if (arg_ctr == (nargs-1)) 
	    {
	      fprintf(stderr,"key without value? ");
	      break;
	    }
	  keying = 1;
	  key = args[arg_ctr];
	  if (keyword_p(args[arg_ctr+1])) {fprintf(stderr,"two keys in a row?");}
	  if (key_start > nkeys) {fprintf(stderr,"%s has extra trailing args?",caller);}
	  key_found = 0;
	  for (i=key_start;i<nkeys;i++)
	    {
	      if ((keyword_p(keys[i])) && (keys[i] == key))
		{
		  keys[i] = args[arg_ctr+1];
		  orig[i] = arg_ctr+1;
		  arg_ctr += 2;
		  rtn_ctr++;
		  key_found = 1;
		}
	    }
	  if (key_found == 0)
	    {
	      /* either there's a redundant keyword pair or a keyword that 'caller' doesn't recognize */
	      fprintf(stderr,"redundant or invalid key found");
	      arg_ctr += 2;
	    }
	}
    }
  return(rtn_ctr);
}

#else

/* old guile (no keyword support, so we roll our own) */
/* ---------------- keywords ---------------- */

#define SC_frequency        ":frequency"
#define SC_initial_phase    ":initial-phase"
#define SC_wave             ":wave"
#define SC_cosines          ":cosines"
#define SC_amplitude        ":amplitude"
#define SC_ratio            ":ratio"
#define SC_size             ":size"
#define SC_a0               ":a0"
#define SC_a1               ":a1"
#define SC_a2               ":a2"
#define SC_b1               ":b1"
#define SC_b2               ":b2"
#define SC_input            ":input"
#define SC_srate            ":srate"
#define SC_file             ":file"
#define SC_channel          ":channel"
#define SC_start            ":start"
#define SC_initial_contents ":initial-contents"
#define SC_initial_element  ":initial-element"
#define SC_scaler           ":scaler"
#define SC_feedforward      ":feedforward"
#define SC_feedback         ":feedback"
#define SC_max_size         ":max-size"
#define SC_radius           ":radius"
#define SC_gain             ":gain"
#define SC_partials         ":partials"
#define SC_r                ":r"
#define SC_a                ":a"
#define SC_n                ":n"
#define SC_fill_time        ":fill-time"
#define SC_order            ":order"
#define SC_x_coeffs         ":xcoeffs"
#define SC_y_coeffs         ":ycoeffs"
#define SC_envelope         ":envelope"
#define SC_base             ":base"
#define SC_duration         ":duration"
#define SC_offset           ":offset"
#define SC_end              ":end"
#define SC_direction        ":direction"
#define SC_degree           ":degree"
#define SC_distance         ":distance"
#define SC_reverb           ":reverb"
#define SC_output           ":output"
#define SC_fft_size         ":fft-size"
#define SC_expansion        ":expansion"
#define SC_length           ":length"
#define SC_hop              ":hop"
#define SC_ramp             ":ramp"
#define SC_jitter           ":jitter"
#define SC_type             ":type"
#define SC_format           ":format"
#define SC_comment          ":comment"
#define SC_channels         ":channels"
#define SC_filter           ":filter"
#define SC_revout           ":revout"
#define SC_width            ":width"

#define NUM_KEYWORDS 56
enum {C_frequency,C_initial_phase,C_wave,C_cosines,C_amplitude,
      C_r,C_ratio,C_size,C_a0,C_a1,C_a2,C_b1,C_b2,C_max_size,
      C_input,C_srate,C_file,C_channel,C_start,
      C_initial_contents,C_initial_element,C_scaler,C_feedforward,C_feedback,
      C_radius,C_gain,C_partials,C_fill_time,C_a,C_n,
      C_order,C_x_coeffs,C_y_coeffs,C_envelope,C_base,C_duration,C_offset,C_end,
      C_direction,C_degree,C_distance,C_reverb,C_output,C_fft_size,
      C_expansion,C_length,C_hop,C_ramp,C_jitter,
      C_type,C_format,C_comment,C_channels,C_filter,C_revout,C_width
};

static char *keywords[NUM_KEYWORDS] = {SC_frequency,SC_initial_phase,SC_wave,SC_cosines,SC_amplitude,
				       SC_r,SC_ratio,SC_size,SC_a0,SC_a1,SC_a2,SC_b1,SC_b2,SC_max_size,
				       SC_input,SC_srate,SC_file,SC_channel,SC_start,
				       SC_initial_contents,SC_initial_element,SC_scaler,SC_feedforward,SC_feedback,
				       SC_radius,SC_gain,SC_partials,SC_fill_time,SC_a,SC_n,
				       SC_order,SC_x_coeffs,SC_y_coeffs,SC_envelope,SC_base,SC_duration,SC_offset,SC_end,
				       SC_direction,SC_degree,SC_distance,SC_reverb,SC_output,SC_fft_size,
				       SC_expansion,SC_length,SC_hop,SC_ramp,SC_jitter,
				       SC_type,SC_format,SC_comment,SC_channels,SC_filter,SC_revout,SC_width
};
static SCM all_keys[NUM_KEYWORDS];
/* what about user-declared keywords? */

short keyword_tag = 0;
static SCM mark_keyword(SCM obj) {SCM_SETGC8MARK(obj); return(SCM_BOOL_F);}
static int keyword_p(SCM obj) {return((SCM_NIMP(obj)) && (SCM_TYP16(obj) == (SCM)keyword_tag));}
#define get_keyword(arg) ((int)GH_VALUE_OF(arg))
static scm_sizet free_keyword(SCM obj) {return(0);}
static int print_keyword(SCM obj, SCM port, scm_print_state *pstate) {scm_puts(keywords[get_keyword(obj)],port); return(1);}

#if HAVE_GUILE_1_3_0
static scm_smobfuns keyword_smobfuns = {
  &mark_keyword,
  &free_keyword,
  &print_keyword,
  0};
#endif

static SCM make_keyword(int val)
{
#if (!HAVE_GUILE_1_3_0)
  SCM_RETURN_NEWSMOB(keyword_tag,val);
#else
  SCM new_keyword;
  SCM_NEWCELL(new_keyword);
  SCM_SETCDR(new_keyword,(SCM)val);
  SCM_SETCAR(new_keyword,keyword_tag);
  return(new_keyword);
#endif
}

static void init_keywords(void)
{
  int i;
#if (!HAVE_GUILE_1_3_0)
  /* keyword_tag = scm_make_smob_type_mfpe("keyword",sizeof(SCM),mark_keyword,free_keyword,print_keyword,0); */
  keyword_tag = scm_make_smob_type("keyword",sizeof(SCM));
  scm_set_smob_mark(keyword_tag,mark_keyword);
  scm_set_smob_print(keyword_tag,print_keyword);
  scm_set_smob_free(keyword_tag,free_keyword);
  /* scm_set_smob_equalp(keyword_tag,NULL); */
#else
  keyword_tag = scm_newsmob(&keyword_smobfuns);
#endif
  for (i=0;i<NUM_KEYWORDS;i++) 
    {
      all_keys[i] = make_keyword(i);
      gh_define(keywords[i],all_keys[i]);
    }
}

static int decode_keywords(char *caller, int nkeys, SCM *keys, int nargs, SCM *args, int *orig)
{
  /* implement the "optional-key" notion in CLM */
  int arg_ctr = 0,key_start = 0,rtn_ctr = 0,i,key,keying = 0,key_found = 0;
  arg_ctr = 0;
  while ((arg_ctr < nargs) && (!(SCM_UNBNDP(args[arg_ctr]))))
    {
      if (!(keyword_p(args[arg_ctr])))
	{
	  if (keying) {fprintf(stderr,"unmatched value within keyword section?");}
	  /* type checking on the actual values has to be the caller's problem */
	  if (arg_ctr > nkeys) {fprintf(stderr,"%s (%d > %d) ran out of key space!",caller,arg_ctr,nkeys);}
	  keys[arg_ctr] = args[arg_ctr];
	  orig[arg_ctr] = arg_ctr;
	  arg_ctr++;
	  key_start = arg_ctr;
	  rtn_ctr++;
	}
      else
	{
	  if (arg_ctr == (nargs-1)) 
	    {
	      fprintf(stderr,"key without value? ");
	      break;
	    }
	  keying = 1;
	  key = get_keyword(args[arg_ctr]);
	  if (keyword_p(args[arg_ctr+1])) {fprintf(stderr,"two keys in a row?");}
	  if (key_start > nkeys) {fprintf(stderr,"%s has extra trailing args?",caller);}
	  key_found = 0;
	  for (i=key_start;i<nkeys;i++)
	    {
	      if ((keyword_p(keys[i])) && (get_keyword(keys[i]) == key))
		{
		  keys[i] = args[arg_ctr+1];
		  orig[i] = arg_ctr+1;
		  arg_ctr += 2;
		  rtn_ctr++;
		  key_found = 1;
		}
	    }
	  if (key_found == 0)
	    {
	      /* either there's a redundant keyword pair or a keyword that 'caller' doesn't recognize */
	      fprintf(stderr,"redundant or invalid key found");
	      arg_ctr += 2;
	    }
	}
    }
  return(rtn_ctr);
}

#endif

static Float fkeyarg (SCM key, char *caller, int n, SCM val, Float def)
{
  if (!(keyword_p(key)))
    {
      if (SCM_NFALSEP(scm_real_p(key)))
	return(gh_scm2double(key));
      else scm_wrong_type_arg(caller,n,val);
    }
  return(def);
}

static int ikeyarg (SCM key, char *caller, int n, SCM val, int def)
{
  if (!(keyword_p(key)))
    {
      if (SCM_NFALSEP(scm_real_p(key)))
	return(g_scm2int(key));
      else scm_wrong_type_arg(caller,n,val);
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

static char *FFT_WINDOW_CONSTANTS[16] = {S_rectangular_window,S_hanning_window,S_welch_window,S_parzen_window,S_bartlett_window,
					 S_hamming_window,S_blackman2_window,S_blackman3_window,S_blackman4_window,
					 S_exponential_window,S_riemann_window,S_kaiser_window,S_cauchy_window,
					 S_poisson_window,S_gaussian_window,S_tukey_window
};

char *mus_fft_window_name(int i);
char *mus_fft_window_name(int i) {return(FFT_WINDOW_CONSTANTS[i]);}


static SCM g_radians2hz(SCM val) 
{
  #define H_radians_hz "(" S_radians_hz " rads) converts radians/sample to frequency in Hz: rads*srate/(2*pi)"
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(val)),val,SCM_ARG1,S_radians_hz);
  return(gh_double2scm(mus_radians2hz(gh_scm2double(val))));
}

static SCM g_hz2radians(SCM val) 
{
  #define H_hz_radians "(" S_hz_radians " hz) converts frequency in Hz to radians/sample: hz*2*pi/srate"
  #define H_in_hz "(" S_in_hz " hz) converts frequency in Hz to radians/sample: hz*2*pi/srate"
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(val)),val,SCM_ARG1,S_hz_radians); 
  return(gh_double2scm(mus_hz2radians(gh_scm2double(val))));
}

static SCM g_radians2degrees(SCM val) 
{
  #define H_radians_degrees "(" S_radians_degrees " rads) converts radians to degrees: rads*360/(2*pi)"
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(val)),val,SCM_ARG1,S_radians_degrees); 
  return(gh_double2scm(mus_radians2degrees(gh_scm2double(val))));
}

static SCM g_degrees2radians(SCM val) 
{
  #define H_degrees_radians "(" S_degrees_radians " deg) converts degrees to radians: deg*2*pi/360"
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(val)),val,SCM_ARG1,S_degrees_radians); 
  return(gh_double2scm(mus_degrees2radians(gh_scm2double(val))));
}

static SCM g_db2linear(SCM val) 
{
  #define H_db_linear "(" S_db_linear " db) converts decibel value db to linear value: pow(10,db/20)"
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(val)),val,SCM_ARG1,S_db_linear);
  return(gh_double2scm(mus_db2linear(gh_scm2double(val))));
}

static SCM g_linear2db(SCM val) 
{
  #define H_linear_db "(" S_linear_db " lin) converts linear value to decibels: 20*log10(lin)"
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(val)),val,SCM_ARG1,S_linear_db);
  return(gh_double2scm(mus_linear2db(gh_scm2double(val))));
}

/* can't use a variable *srate* directly here because the set! side would not communicate the change to C */
static SCM g_srate(void) 
{
  #define H_mus_srate "(" S_mus_srate ") -> current sampling rate"
  return(gh_double2scm(mus_srate()));
}

static SCM g_set_srate(SCM val) 
{
  #define H_mus_set_srate "(" S_mus_set_srate " val) sets the current sampling rate to val"
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(val)),val,SCM_ARG1,S_mus_set_srate);
  return(gh_double2scm(mus_set_srate(gh_scm2double(val))));
}

static SCM g_array_print_length(void) 
{
  #define H_mus_array_print_length "(" S_mus_array_print_length ") -> current clm array print length (default is 8)"
  return(gh_int2scm(mus_array_print_length()));
}

static SCM g_set_array_print_length(SCM val) 
{
  #define H_mus_set_array_print_length "(" S_mus_set_array_print_length " val) sets the current clm array print length to val"
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(val)),val,SCM_ARG1,S_mus_set_array_print_length);
  return(gh_int2scm(mus_set_array_print_length(g_scm2int(val))));
}

static SCM g_ring_modulate(SCM val1, SCM val2) 
{
  #define H_ring_modulate "(" S_ring_modulate " s1 s2) -> s1*s2 (sample by sample)"
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(val1)),val1,SCM_ARG1,S_ring_modulate);
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(val2)),val2,SCM_ARG2,S_ring_modulate);
  return(gh_double2scm(mus_ring_modulate(gh_scm2double(val1),gh_scm2double(val2))));
}

static SCM g_amplitude_modulate(SCM val1, SCM val2, SCM val3) 
{
  #define H_amplitude_modulate "(" S_amplitude_modulate " carrier in1 in2) -> in1*(carrier+in2)"
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(val1)),val1,SCM_ARG1,S_amplitude_modulate);
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(val2)),val2,SCM_ARG2,S_amplitude_modulate);
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(val3)),val3,SCM_ARG3,S_amplitude_modulate);
  return(gh_double2scm(mus_amplitude_modulate(gh_scm2double(val1),gh_scm2double(val2),gh_scm2double(val3))));
}

static SCM g_contrast_enhancement(SCM val1, SCM val2) 
{
  #define H_contrast_enhancement "(" S_contrast_enhancement " sig index) -> sin(sig*pi/2 + index*sin(sig*2*pi))"
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(val1)),val1,SCM_ARG1,S_contrast_enhancement);
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(val2)),val2,SCM_ARG2,S_contrast_enhancement);
  return(gh_double2scm(mus_contrast_enhancement(gh_scm2double(val1),gh_scm2double(val2))));
}

static SCM g_dot_product(SCM val1, SCM val2) 
{
  #define H_dot_product "(" S_dot_product " v1 v2) -> sum of (vcts) v1[i]*v2[i]"
  vct *v1,*v2;
  SCM_ASSERT(vct_p(val1),val1,SCM_ARG1,S_dot_product);
  SCM_ASSERT(vct_p(val2),val2,SCM_ARG2,S_dot_product);
  v1 = get_vct(val1);
  v2 = get_vct(val2);
  return(scm_return_first(gh_double2scm(mus_dot_product(v1->data,v2->data,v1->length)),val1,val2));
}

static SCM g_fft_window_1(char *caller, int choice, SCM val1, SCM val2, SCM ulen) 
{
  vct *v1,*v2;
  int len;
  SCM_ASSERT(vct_p(val1),val1,SCM_ARG1,caller);
  SCM_ASSERT(vct_p(val2),val2,SCM_ARG2,caller);
  v1 = get_vct(val1);
  v2 = get_vct(val2);
  if (gh_number_p(ulen)) 
    len = g_scm2int(ulen); 
  else 
    {
      len = v1->length;
      if (v2->length < len) len = v2->length;
    }
  if (choice)
    mus_multiply_arrays(v1->data,v2->data,len);
  else mus_rectangular2polar(v1->data,v2->data,len);
  return(scm_return_first(val1,val2));
}

static SCM g_multiply_arrays(SCM val1, SCM val2, SCM len) 
{
  #define H_multiply_arrays "(" S_multiply_arrays " v1 v2 &optional len) -> (vcts) v1[i] *= v2[i]"
  return(g_fft_window_1(S_multiply_arrays,TRUE,val1,val2,len));
}

static SCM g_rectangular2polar(SCM val1, SCM val2) 
{
  #define H_rectangular2polar "(" S_rectangular2polar " rl im) converts real/imaginary\n\
   data in (vcts) rl and im from rectangular form (fft output) to polar form (a spectrum)"

  return(g_fft_window_1(S_rectangular2polar,FALSE,val1,val2,SCM_UNDEFINED));
}

static SCM g_mus_fft(SCM url, SCM uim, SCM len, SCM usign)
{
  #define H_mus_fft "(" S_mus_fft " rl im len &optional dir) returns the fft of (vcts) rl and im\n\
   the real and imaginary parts of the data, len should be a power of 2\n\
   dir = 1 for fft, -1 for inverse-fft"

  int sign,n;
  vct *v1,*v2;
  SCM_ASSERT((vct_p(url)),url,SCM_ARG1,S_mus_fft);
  SCM_ASSERT((vct_p(uim)),uim,SCM_ARG2,S_mus_fft);
  v1 = get_vct(url);
  v2 = get_vct(uim);
  if (SCM_NFALSEP(scm_real_p(usign))) sign = g_scm2int(usign); else sign = 1;
  if (SCM_NFALSEP(scm_real_p(len))) n = g_scm2int(len); else n = v1->length;
  mus_fft(v1->data,v2->data,n,sign);
  return(scm_return_first(url,uim));
}

static SCM g_make_fft_window(SCM type, SCM size, SCM ubeta)
{
  #define H_make_fft_window "(" S_make_fft_window " type size &optional beta) -> fft data window (vct obj)\n\
   type is one of the sndlib fft window identifiers such as kaiser-window\n\
   beta is the window parameter, if any:\n\
     (set! v1 (make-fft-window hamming-window 256))"

  Float beta = 0.0;
  int n;
  SCM_ASSERT((SCM_NFALSEP(scm_real_p(type))),type,SCM_ARG1,S_make_fft_window);
  SCM_ASSERT((SCM_NFALSEP(scm_real_p(size))),size,SCM_ARG2,S_make_fft_window);
  if (SCM_NFALSEP(scm_real_p(ubeta))) beta = gh_scm2double(ubeta);
  n = g_scm2int(size);
  return(make_vct(n,mus_make_fft_window(g_scm2int(type),n,beta)));
}

static SCM g_spectrum(SCM url, SCM uim, SCM uwin, SCM un, SCM utype)
{
  #define H_mus_spectrum "(" S_spectrum " rl im window &optional len type)\n\
   real and imaginary data in (vcts) rl and im, returns (in rl) the spectrum thereof\n\
   len is the fft size (a power of 2),\n\
   window is the data window (a vct object as returned by make-fft-window)\n\
   and type determines how the spectral data is scaled:\n\
     1, the default = linear and normalized\n\
     0 = data in dB, anything else = linear and un-normalized."

  int n,type;
  vct *v1,*v2,*v3 = NULL;
  SCM_ASSERT((vct_p(url)),url,SCM_ARG1,S_spectrum);
  SCM_ASSERT((vct_p(uim)),uim,SCM_ARG2,S_spectrum);
  if (SCM_NFALSEP(uwin)) SCM_ASSERT((vct_p(uwin)),uwin,SCM_ARG3,S_spectrum);
  v1 = get_vct(url);
  v2 = get_vct(uim);
  if (SCM_NFALSEP(uwin)) v3 = get_vct(uwin);
  if (SCM_NFALSEP(scm_real_p(un))) n = g_scm2int(un); else n = v1->length;
  if (SCM_NFALSEP(scm_real_p(utype))) type = g_scm2int(utype); else type = 1; /* linear normalized */
  mus_spectrum(v1->data,v2->data,(v3) ? (v3->data) : NULL,n,type);
  return(scm_return_first(url,uim,uwin));
}

static SCM g_convolution(SCM url1, SCM url2, SCM un)
{
  #define H_mus_convolution "(" S_convolution " v1 v2 &optional len) -> convolution\n\
   of (vcts) v1 with v2, using fft of size len (a power of 2), result in v1"

  int n;
  vct *v1,*v2;
  SCM_ASSERT((vct_p(url1)),url1,SCM_ARG1,S_convolution);
  SCM_ASSERT((vct_p(url2)),url2,SCM_ARG2,S_convolution);
  v1 = get_vct(url1);
  v2 = get_vct(url2);
  if (SCM_NFALSEP(scm_real_p(un))) n = g_scm2int(un); else n = v1->length;
  mus_convolution(v1->data,v2->data,n);
  return(scm_return_first(url1,url2));
}

static SCM g_clear_array(SCM arr)
{
  #define H_clear_array "(" S_clear_array " v) clears vct v: v[i]=0.0"
  vct *v;
  SCM_ASSERT(vct_p(arr),arr,SCM_ARG1,S_clear_array);
  v = get_vct(arr);
  mus_clear_array(v->data,v->length);
  return(scm_return_first(arr));
}

static SCM g_polynomial(SCM arr, SCM x)
{
  #define H_polynomial "(" S_polynomial " coeffs x) evaluates a polynomial at x"
  vct *v;
  SCM_ASSERT(vct_p(arr),arr,SCM_ARG1,S_polynomial);
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(x)),x,SCM_ARG2,S_polynomial);
  v = get_vct(arr);
  return(scm_return_first(gh_double2scm(mus_polynomial(v->data,gh_scm2double(x),v->length)),arr));
}

static SCM g_array_interp(SCM obj, SCM phase, SCM size) /* opt size */
{
  #define H_array_interp "(" S_array_interp " v phase &optional size) -> v[phase]\n\
   taking into account wrap-around (size = size of data), with linear interpolation if phase is not integral."

  int len;
  vct *v;
  SCM_ASSERT(vct_p(obj),obj,SCM_ARG1,S_array_interp);
  SCM_ASSERT((SCM_NFALSEP(scm_real_p(phase))),phase,SCM_ARG2,S_array_interp);
  v = get_vct(obj);
  if (SCM_NFALSEP(scm_real_p(size))) len = g_scm2int(size); else len = v->length;
  return(scm_return_first(gh_double2scm(mus_array_interp(v->data,gh_scm2double(phase),len)),obj));
}

#ifdef __cplusplus
  static SCM gh_new_procedure3_1 (char *proc_name,SCM (*fn)(SCM,SCM,SCM,SCM)) {return(gh_new_procedure(proc_name,(SCM (*)(...))fn,3,1,0));}
#else
  static SCM gh_new_procedure3_1 (char *proc_name,SCM (*fn)()) {return(gh_new_procedure(proc_name,fn,3,1,0));}
#endif

static void init_simple_stuff(void)
{
  DEFINE_PROC(gh_new_procedure1_0(S_radians_hz,g_radians2hz),H_radians_hz);
  DEFINE_PROC(gh_new_procedure1_0(S_hz_radians,g_hz2radians),H_hz_radians);
  DEFINE_PROC(gh_new_procedure1_0(S_in_hz,g_hz2radians),H_in_hz);
  DEFINE_PROC(gh_new_procedure1_0(S_radians_degrees,g_radians2degrees),H_radians_degrees);
  DEFINE_PROC(gh_new_procedure1_0(S_degrees_radians,g_degrees2radians),H_degrees_radians);
  DEFINE_PROC(gh_new_procedure1_0(S_db_linear,g_db2linear),H_db_linear);
  DEFINE_PROC(gh_new_procedure1_0(S_linear_db,g_linear2db),H_linear_db);
  DEFINE_PROC(gh_new_procedure0_0(S_mus_srate,g_srate),H_mus_srate);
  DEFINE_PROC(gh_new_procedure1_0(S_mus_set_srate,g_set_srate),H_mus_set_srate);
  DEFINE_PROC(gh_new_procedure2_0(S_ring_modulate,g_ring_modulate),H_ring_modulate);
  DEFINE_PROC(gh_new_procedure3_0(S_amplitude_modulate,g_amplitude_modulate),H_amplitude_modulate);
  DEFINE_PROC(gh_new_procedure2_0(S_contrast_enhancement,g_contrast_enhancement),H_contrast_enhancement);
  DEFINE_PROC(gh_new_procedure2_0(S_dot_product,g_dot_product),H_dot_product);
  DEFINE_PROC(gh_new_procedure1_0(S_clear_array,g_clear_array),H_clear_array);
  DEFINE_PROC(gh_new_procedure2_0(S_polynomial,g_polynomial),H_polynomial);
  DEFINE_PROC(gh_new_procedure2_1(S_multiply_arrays,g_multiply_arrays),H_multiply_arrays);
  DEFINE_PROC(gh_new_procedure0_0(S_mus_array_print_length,g_array_print_length),H_mus_array_print_length);
  DEFINE_PROC(gh_new_procedure1_0(S_mus_set_array_print_length,g_set_array_print_length),H_mus_set_array_print_length);
  DEFINE_PROC(gh_new_procedure2_1(S_make_fft_window,g_make_fft_window),H_make_fft_window);
  DEFINE_PROC(gh_new_procedure3_1(S_mus_fft,g_mus_fft),H_mus_fft);
  DEFINE_PROC(gh_new_procedure(S_spectrum,SCM_FNC g_spectrum,3,2,0),H_mus_spectrum); 
  DEFINE_PROC(gh_new_procedure2_1(S_convolution,g_convolution),H_mus_convolution);
  DEFINE_PROC(gh_new_procedure2_0(S_rectangular2polar,g_rectangular2polar),H_rectangular2polar);
  DEFINE_PROC(gh_new_procedure(S_array_interp,SCM_FNC g_array_interp,2,1,0),H_array_interp);

  gh_define(S_rectangular_window,gh_int2scm(MUS_RECTANGULAR_WINDOW));
  gh_define(S_hanning_window,gh_int2scm(MUS_HANNING_WINDOW));
  gh_define(S_welch_window,gh_int2scm(MUS_WELCH_WINDOW));
  gh_define(S_parzen_window,gh_int2scm(MUS_PARZEN_WINDOW));
  gh_define(S_bartlett_window,gh_int2scm(MUS_BARTLETT_WINDOW));
  gh_define(S_hamming_window,gh_int2scm(MUS_HAMMING_WINDOW));
  gh_define(S_blackman2_window,gh_int2scm(MUS_BLACKMAN2_WINDOW));
  gh_define(S_blackman3_window,gh_int2scm(MUS_BLACKMAN3_WINDOW));
  gh_define(S_blackman4_window,gh_int2scm(MUS_BLACKMAN4_WINDOW));
  gh_define(S_exponential_window,gh_int2scm(MUS_EXPONENTIAL_WINDOW));
  gh_define(S_riemann_window,gh_int2scm(MUS_RIEMANN_WINDOW));
  gh_define(S_kaiser_window,gh_int2scm(MUS_KAISER_WINDOW));
  gh_define(S_cauchy_window,gh_int2scm(MUS_CAUCHY_WINDOW));
  gh_define(S_poisson_window,gh_int2scm(MUS_POISSON_WINDOW));
  gh_define(S_gaussian_window,gh_int2scm(MUS_GAUSSIAN_WINDOW));
  gh_define(S_tukey_window,gh_int2scm(MUS_TUKEY_WINDOW));
}


/* ---------------- mus-scm struct ---------------- */

typedef struct {
  mus_any *gen;
  SCM *vcts; /* one for each accessible Float array (wrapped up here in a vct object) */
  int nvcts;
} mus_scm;

static short mus_scm_tag = 0;
#define mus_get_any(arg) (((mus_scm *)GH_VALUE_OF(arg))->gen)
#define mus_get_scm(arg) ((mus_scm *)GH_VALUE_OF(arg))

static int mus_scm_p(SCM obj) {return((SCM_NIMP(obj)) && (GH_TYPE_OF(obj) == (SCM)mus_scm_tag));}

static SCM mark_mus_scm(SCM obj) 
{
  int i;
  mus_scm *ms;
  ms = mus_get_scm(obj);
  if (ms->vcts) {for (i=0;i<ms->nvcts;i++) if (ms->vcts[i]) scm_gc_mark(ms->vcts[i]);}
  SCM_SETGC8MARK(obj); 
  return(SCM_BOOL_F);
}

static scm_sizet free_mus_scm(SCM obj) 
{
  mus_scm *ms;
  ms = mus_get_scm(obj);
  if (ms->nvcts != -1) mus_free(mus_get_any(obj));
  if (ms->vcts) FREE(ms->vcts);  
  FREE(ms);
  return(0);
}

static int print_mus_scm(SCM obj, SCM port, scm_print_state *pstate)
{
  char *buf;
  buf = mus_describe(mus_get_any(obj));
  scm_puts(buf,port);
  FREE(buf);
  return(1);
}

static SCM equalp_mus_scm(SCM obj1, SCM obj2) 
{
  return((mus_equalp(mus_get_any(obj1),mus_get_any(obj2))) ? SCM_BOOL_T : SCM_BOOL_F);
}

#if HAVE_GUILE_1_3_0
static scm_smobfuns mus_scm_smobfuns = {
  &mark_mus_scm,
  &free_mus_scm,
  &print_mus_scm,
  &equalp_mus_scm};
#endif

static void init_mus_scm(void)
{
#if (!HAVE_GUILE_1_3_0)
  mus_scm_tag = scm_make_smob_type_mfpe("mus",sizeof(mus_scm),mark_mus_scm,free_mus_scm,print_mus_scm,equalp_mus_scm);
  mus_scm_tag = scm_make_smob_type("mus",sizeof(mus_scm));
  scm_set_smob_mark(mus_scm_tag,mark_mus_scm);
  scm_set_smob_print(mus_scm_tag,print_mus_scm);
  scm_set_smob_free(mus_scm_tag,free_mus_scm);
  scm_set_smob_equalp(mus_scm_tag,equalp_mus_scm);
#else
  mus_scm_tag = scm_newsmob(&mus_scm_smobfuns); 
#endif
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

static SCM g_inspect(SCM gen)
{
  #define H_mus_inspect "(" S_mus_inspect " gen) -> the internal state of gen"
  SCM_ASSERT((mus_scm_p(gen)),gen,SCM_ARG1,S_mus_inspect);
  return(gh_str02scm(mus_inspect(mus_get_any(gen))));
}

static SCM g_describe(SCM gen) 
{
  #define H_mus_describe "(" S_mus_describe " gen) -> the user's view of the state of gen"
  SCM_ASSERT((mus_scm_p(gen)),gen,SCM_ARG1,S_mus_describe);
  return(gh_str02scm(mus_describe(mus_get_any(gen))));
}

static SCM g_phase(SCM gen) 
{
  #define H_mus_phase "(" S_mus_phase " gen) -> gen's current phase (radians)"
  SCM_ASSERT((mus_scm_p(gen)),gen,SCM_ARG1,S_mus_phase);
  return(gh_double2scm(mus_phase(mus_get_any(gen))));
}

static SCM g_set_phase(SCM gen, SCM val) 
{
  #define H_mus_set_phase "(" S_mus_set_phase " gen val) sets gen's phase to val (radians)"
  SCM_ASSERT((mus_scm_p(gen)),gen,SCM_ARG1,S_mus_set_phase);
  SCM_ASSERT((SCM_NFALSEP(scm_real_p(val))),val,SCM_ARG2,S_mus_set_phase);
  return(gh_double2scm(mus_set_phase(mus_get_any(gen),gh_scm2double(val))));
}

static SCM g_scaler(SCM gen) 
{
  #define H_mus_scaler "(" S_mus_scaler " gen) -> gen's scaler, if any"
  SCM_ASSERT((mus_scm_p(gen)),gen,SCM_ARG1,S_mus_scaler);
  return(gh_double2scm(mus_scaler(mus_get_any(gen))));
}

static SCM g_set_scaler(SCM gen, SCM val) 
{
  #define H_mus_set_scaler "(" S_mus_set_scaler " gen val) sets gen's scaler to val"
  SCM_ASSERT((mus_scm_p(gen)),gen,SCM_ARG1,S_mus_set_scaler);
  SCM_ASSERT((SCM_NFALSEP(scm_real_p(val))),val,SCM_ARG2,S_mus_set_scaler);
  return(gh_double2scm(mus_set_scaler(mus_get_any(gen),gh_scm2double(val))));
}

static SCM g_frequency(SCM gen) 
{
  #define H_mus_frequency "(" S_mus_frequency " gen) -> gen's frequency (Hz)"
  SCM_ASSERT((mus_scm_p(gen)),gen,SCM_ARG1,S_mus_frequency);
  return(gh_double2scm(mus_frequency(mus_get_any(gen))));
}

static SCM g_set_frequency(SCM gen, SCM val) 
{
  #define H_mus_set_frequency "(" S_mus_set_frequency " gen val) sets gen's frequency to val (Hz)"
  SCM_ASSERT((mus_scm_p(gen)),gen,SCM_ARG1,S_mus_frequency);
  SCM_ASSERT((SCM_NFALSEP(scm_real_p(val))),val,SCM_ARG2,S_mus_set_frequency);
  return(gh_double2scm(mus_set_frequency(mus_get_any(gen),gh_scm2double(val))));
}

static SCM g_length(SCM gen) 
{
  #define H_mus_length "(" S_mus_length " gen) -> gen's length, if any"
  SCM_ASSERT((mus_scm_p(gen)),gen,SCM_ARG1,S_mus_length);
  return(gh_int2scm(mus_length(mus_get_any(gen))));
}

static SCM g_set_length(SCM gen, SCM val) 
{
  #define H_mus_set_length "(" S_mus_set_length " gen val) sets gen's length to val"
  SCM_ASSERT((mus_scm_p(gen)),gen,SCM_ARG1,S_mus_length);
  SCM_ASSERT((SCM_NFALSEP(scm_real_p(val))),val,SCM_ARG2,S_mus_set_length);
  return(gh_int2scm(mus_set_length(mus_get_any(gen),g_scm2int(val))));
}

#define MUS_DATA_POSITION 0

static SCM g_data(SCM gen) 
{
  #define H_mus_data "(" S_mus_data " gen) -> gen's internal data (vct), if any"
  mus_scm *ms;
  SCM_ASSERT((mus_scm_p(gen)),gen,SCM_ARG1,S_mus_data);
  ms = mus_get_scm(gen);
  if (ms->vcts)
    return(ms->vcts[MUS_DATA_POSITION]); 
  else return(SCM_BOOL_F);
}

static SCM g_set_data(SCM gen, SCM val) 
{
  #define H_mus_set_data "(" S_mus_set_data " gen v) sets gen's internal data to v (vct)"
  mus_scm *ms;
  mus_any *ma;
  vct *v;
  SCM_ASSERT((mus_scm_p(gen)),gen,SCM_ARG1,S_mus_set_data);
  SCM_ASSERT((vct_p(val)),val,SCM_ARG2,S_mus_set_data);
  ms = mus_get_scm(gen);
  if (ms->vcts)
    {
      v = (vct *)GH_VALUE_OF(val);
      ma = ms->gen;
      mus_set_data(ma,v->data);  /* TO REMEMBER: if allocated, should have freed, and set to not allocated */
      ms->vcts[MUS_DATA_POSITION] = val;
      return(val);
    }
  return(scm_return_first(SCM_BOOL_F,gen,val));
}

static void init_generic_funcs(void)
{
  DEFINE_PROC(gh_new_procedure1_0(S_mus_inspect,g_inspect),H_mus_inspect);
  DEFINE_PROC(gh_new_procedure1_0(S_mus_describe,g_describe),H_mus_describe);
  DEFINE_PROC(gh_new_procedure1_0(S_mus_phase,g_phase),H_mus_phase);
  DEFINE_PROC(gh_new_procedure2_0(S_mus_set_phase,g_set_phase),H_mus_set_phase);
  DEFINE_PROC(gh_new_procedure1_0(S_mus_scaler,g_scaler),H_mus_scaler);
  DEFINE_PROC(gh_new_procedure2_0(S_mus_set_scaler,g_set_scaler),H_mus_set_scaler);
  DEFINE_PROC(gh_new_procedure1_0(S_mus_frequency,g_frequency),H_mus_frequency);
  DEFINE_PROC(gh_new_procedure2_0(S_mus_set_frequency,g_set_frequency),H_mus_set_frequency);
  DEFINE_PROC(gh_new_procedure1_0(S_mus_length,g_length),H_mus_length);
  DEFINE_PROC(gh_new_procedure2_0(S_mus_set_length,g_set_length),H_mus_set_length);
  DEFINE_PROC(gh_new_procedure1_0(S_mus_data,g_data),H_mus_data);
  DEFINE_PROC(gh_new_procedure2_0(S_mus_set_data,g_set_data),H_mus_set_data);
}

/* someday this should use a run pointer */
static SCM g_mus_bank1(SCM amps, SCM gens, SCM inp, int type, char *caller)
{
  /* amps and inp can be a Float, a vct object, or a vector of Floats */
  Float outval = 0.0,inval = 0.0, scl = 1.0;
  int i,size,free_scls=0,free_invals=0;
  Float *scls=NULL,*invals=NULL;
  vct *v;
  SCM_ASSERT(gh_vector_p(gens),gens,SCM_ARG2,caller);
  size = gh_vector_length(gens);
  if (gh_vector_p(inp))
    {
      invals = (Float *)CALLOC(size,sizeof(Float));
      free_invals=1;
      for (i=0;i<size;i++) invals[i] = gh_scm2double(gh_vector_ref(inp,gh_int2scm(i)));
    }
  else
    {
      if (vct_p(inp))
	{
	  v = get_vct(inp);
	  invals = v->data;
	}
      else
	{
	  invals = (Float *)CALLOC(size,sizeof(Float));
	  free_invals=1;
	  if (SCM_NFALSEP(scm_real_p(inp))) 
	    {
	      inval = gh_scm2double(inp);
	      for (i=0;i<size;i++) invals[i] = inval;
	    }
	  else
	    {
	      if (gh_procedure_p(inp))
		{
#if USE_SND
		  for (i=0;i<size;i++) invals[i] = gh_scm2double(g_call1(inp,gh_int2scm(i)));
#else
		  for (i=0;i<size;i++) invals[i] = gh_scm2double(gh_call1(inp,gh_int2scm(i)));
#endif
		}
	      else scm_misc_error("mus_bank","invalid input arg: ~S",SCM_LIST1(inp));
	    }
	}
    }
  if (gh_vector_p(amps))
    {
      scls = (Float *)CALLOC(size,sizeof(Float));
      free_scls=1;
      for (i=0;i<size;i++) scls[i] = gh_scm2double(gh_vector_ref(amps,gh_int2scm(i)));
    }
  else
    {
      if (vct_p(amps))
	{
	  v = get_vct(amps);
	  scls = v->data;
	}
      else
	{
	  scls = (Float *)CALLOC(size,sizeof(Float));
	  free_scls=1;
	  if (SCM_NFALSEP(scm_real_p(amps))) 
	    {
	      scl = gh_scm2double(amps);
	      for (i=0;i<size;i++) scls[i] = scl;
	    }
	  else
	    {
	      if (gh_procedure_p(amps))
		{
#if USE_SND
		  for (i=0;i<size;i++) scls[i] = gh_scm2double(g_call1(amps,gh_int2scm(i)));
#else
		  for (i=0;i<size;i++) scls[i] = gh_scm2double(gh_call1(amps,gh_int2scm(i)));
#endif
		}
	      else scm_misc_error("mus_bank","invalid scaler arg: ~S",SCM_LIST1(amps));
	    }
	}
    }
  switch (type)
    {
    case MUS_OSCIL:
      for (i=0;i<size;i++)
	outval += (scls[i] * mus_oscil(mus_get_any(gh_vector_ref(gens,gh_int2scm(i))),invals[i],0.0));
      break;
    case MUS_FORMANT:
      for (i=0;i<size;i++)
	outval += (scls[i] * mus_formant(mus_get_any(gh_vector_ref(gens,gh_int2scm(i))),invals[i]));
      break;
    }
  if (free_scls) FREE(scls);
  if (free_invals) FREE(invals);
  return(gh_double2scm(outval));
}


/* ---------------- oscil ---------------- */

#define S_make_oscil "make-oscil"
#define S_oscil "oscil"
#define S_oscil_bank "oscil-bank"
#define S_oscil_p "oscil?"
#define S_mus_apply "mus-apply"

static SCM g_make_oscil(SCM arg1, SCM arg2, SCM arg3, SCM arg4)
{
  #define H_make_oscil "(" S_make_oscil " &opt-key (frequency 440.0) (phase 0.0)) -> a new " S_oscil " (sinewave) generator"
#if HAVE_GUILE_1_3_0
  SCM new_osc;
#endif
  mus_scm *gn;
  int vals;
  SCM args[4],keys[2];
  int orig_arg[2] = {0,0};
  Float freq = 440.0,phase = 0.0;
  keys[0] = all_keys[C_frequency];
  keys[1] = all_keys[C_initial_phase];
  args[0] = arg1; args[1] = arg2; args[2] = arg3; args[3] = arg4; 
  vals = decode_keywords(S_make_oscil,2,keys,4,args,orig_arg);
  if (vals > 0)
    {
      freq = fkeyarg(keys[0],S_make_oscil,orig_arg[0]+1,args[orig_arg[0]],freq);
      phase = fkeyarg(keys[1],S_make_oscil,orig_arg[1]+1,args[orig_arg[1]],phase);
    }
  gn = (mus_scm *)CALLOC(1,sizeof(mus_scm));
  gn->gen = mus_make_oscil(freq,phase);
  gn->nvcts = 0;
#if (!HAVE_GUILE_1_3_0)
  SCM_RETURN_NEWSMOB(mus_scm_tag,gn);
#else
  SCM_NEWCELL(new_osc);
  SCM_SETCDR(new_osc,(SCM)gn);
  SCM_SETCAR(new_osc,mus_scm_tag);
  return(new_osc);
#endif
}

static SCM g_oscil(SCM os, SCM fm, SCM pm)
{
  #define H_oscil "(" S_oscil " gen &optional fm pm) -> next sample from " S_oscil " gen: rtn=sin(phase+pm) phase+=(freq+fm)"
  Float fm1 = 0.0,pm1 = 0.0;
  SCM_ASSERT(((mus_scm_p(os)) && (mus_oscil_p(mus_get_any(os)))),os,SCM_ARG1,S_oscil);
  if (SCM_NFALSEP(scm_real_p(fm))) fm1 = gh_scm2double(fm); else if (!(SCM_UNBNDP(fm))) scm_wrong_type_arg(S_oscil,2,fm);
  if (SCM_NFALSEP(scm_real_p(pm))) pm1 = gh_scm2double(pm); else if (!(SCM_UNBNDP(pm))) scm_wrong_type_arg(S_oscil,3,pm);
  return(gh_double2scm(mus_oscil(mus_get_any(os),fm1,pm1)));
}

static SCM g_oscil_bank(SCM amps, SCM gens, SCM inp, SCM size)
{
  /* size currently ignored */
  #define H_oscil_bank "(" S_oscil_bank " scls gens fms) -> sum a bank of " S_oscil "s: scls[i]*" S_oscil "(gens[i],fms[i])"
  return(g_mus_bank1(amps,gens,inp,MUS_OSCIL,S_oscil_bank));
}

static SCM g_oscil_p(SCM os) 
{
  #define H_oscil_p "(" S_oscil_p " gen) -> #t if gen is an " S_oscil " generator, else #f"
  return(((mus_scm_p(os)) && (mus_oscil_p(mus_get_any(os)))) ? SCM_BOOL_T : SCM_BOOL_F);
}

static SCM g_mus_apply(SCM arglist)
{
  #define H_mus_apply "(" S_mus_apply " gen &rest args) applies gen to args"
  /* weird that this is such a mess when it's inspired by lisp */
  int arglist_len;
  mus_any *gen;
  SCM arg2,arg3;
  arglist_len = gh_length(arglist);
  if ((arglist_len > 3) || (arglist_len == 0)) return(gh_double2scm(0.0));
  gen = mus_get_any(gh_car(arglist));
  if (arglist_len == 1) return(gh_double2scm(mus_apply(gen)));
  arg2 = gh_cadr(arglist);
  if (arglist_len == 2)
    {
      if (SCM_INUMP(arg2))
	return(gh_double2scm(mus_apply(gen,gh_scm2int(arg2))));
      else return(gh_double2scm(mus_apply(gen,gh_scm2double(arg2))));
    }
  arg3 = gh_caddr(arglist);
  if (SCM_INUMP(arg2))
    {
      if (SCM_INUMP(arg3))
	return(gh_double2scm(mus_apply(gen,gh_scm2int(arg2),gh_scm2int(arg3))));
      else return(gh_double2scm(mus_apply(gen,gh_scm2int(arg2),gh_scm2double(arg3))));
    }
  else
    {
      if (SCM_INUMP(arg3))
	return(gh_double2scm(mus_apply(gen,gh_scm2double(arg2),gh_scm2int(arg3))));
      else return(gh_double2scm(mus_apply(gen,gh_scm2double(arg2),gh_scm2double(arg3))));
    }
  return(gh_double2scm(0.0));
}

static void init_oscil(void)
{
  DEFINE_PROC(gh_new_procedure1_0(S_oscil_p,SCM_FNC g_oscil_p),H_oscil_p);
  DEFINE_PROC(gh_new_procedure(S_make_oscil,SCM_FNC g_make_oscil,0,4,0),H_make_oscil);
  DEFINE_PROC(gh_new_procedure1_2(S_oscil,SCM_FNC g_oscil),H_oscil);
  DEFINE_PROC(gh_new_procedure2_2(S_oscil_bank,SCM_FNC g_oscil_bank),H_oscil_bank);
  DEFINE_PROC(gh_new_procedure(S_mus_apply,SCM_FNC g_mus_apply,0,0,1),H_mus_apply);
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

enum {G_DELAY,G_COMB,G_NOTCH,G_ALL_PASS};

static SCM g_make_delay_1(int choice, SCM arglist)
{
  SCM new_dly;
  mus_scm *gn;
  vct *v;
  char *caller=NULL,*errstr;
  SCM args[14],keys[7];
  int orig_arg[7] = {0,0,0,0,0,0,0};
  int vals,i,argn=0,len,arglist_len,keyn,max_size = -1;
  int size = 1;
  Float *line = NULL;
  Float scaler = 0.0, feedback = 0.0, feedforward = 0.0;
  SCM initial_contents = SCM_UNDEFINED;
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
  for (i=0;i<14;i++) args[i] = SCM_UNDEFINED;
  arglist_len = gh_length(arglist);
  for (i=0;i<arglist_len;i++) args[i] = gh_list_ref(arglist,gh_int2scm(i));
  vals = decode_keywords(caller,argn,keys,argn*2,args,orig_arg);
  if (vals > 0)
    {
      keyn = 0;
      switch (choice)
	{
	case G_DELAY: 
	  break;
	case G_COMB: case G_NOTCH:
	  scaler = fkeyarg(keys[keyn],caller,orig_arg[keyn]+1,args[orig_arg[keyn]],scaler);
	  keyn++;
	  break;
	case G_ALL_PASS:
	  feedback = fkeyarg(keys[keyn],caller,orig_arg[keyn]+1,args[orig_arg[keyn]],feedback);
	  keyn++;
	  feedforward = fkeyarg(keys[keyn],caller,orig_arg[keyn]+1,args[orig_arg[keyn]],feedforward);
	  keyn++;
	  break;
	}
      size = ikeyarg(keys[keyn],caller,orig_arg[keyn]+1,args[orig_arg[keyn]],size);
      keyn++;
      if (!(keyword_p(keys[keyn])))
	{
	  initial_contents = keys[keyn];
	  if (vct_p(initial_contents))
	    {
	      v = get_vct(initial_contents);
	      line = v->data;
	    }
	  else
	    {
	      if (gh_list_p(initial_contents))
		{
		  len = gh_length(initial_contents);
		  line = (Float *)CALLOC(len,sizeof(Float));
		  for (i=0;i<len;i++) line[i] = gh_scm2double(gh_list_ref(initial_contents,gh_int2scm(i)));
		}
	    }
	}
      keyn++;
      initial_element = fkeyarg(keys[keyn],caller,orig_arg[keyn]+1,args[orig_arg[keyn]],0.0);
      keyn++;
      max_size = ikeyarg(keys[keyn],caller,orig_arg[keyn]+1,args[orig_arg[keyn]],size);
    }
  if (max_size == -1) max_size = size;
  if (max_size <= 0)
    {
      errstr = (char *)CALLOC(64,sizeof(char));
      sprintf(errstr,"%s: delay line length is %d?",caller,max_size);
      scm_misc_error(caller,errstr,SCM_EOL);
      FREE(errstr);
    }
  if (line == NULL) line = (Float *)CALLOC(max_size,sizeof(Float));
  if (initial_element != 0.0) for (i=0;i<max_size;i++) line[i] = initial_element;
  gn = (mus_scm *)CALLOC(1,sizeof(mus_scm));
  gn->vcts = (SCM *)CALLOC(1,sizeof(SCM));
  gn->vcts[0] = SCM_EOL;
  gn->nvcts = 1;
  switch (choice)
    {
    case G_DELAY: gn->gen = mus_make_delay(size,line,max_size); break;
    case G_COMB: gn->gen = mus_make_comb(scaler,size,line,max_size); break;
    case G_NOTCH: gn->gen = mus_make_notch(scaler,size,line,max_size); break;
    case G_ALL_PASS: gn->gen = mus_make_all_pass(feedback,feedforward,size,line,max_size); break;
    }
#if (!HAVE_GUILE_1_3_0)
  SCM_NEWSMOB(new_dly,mus_scm_tag,gn);
#else
  SCM_NEWCELL(new_dly);
  SCM_SETCDR(new_dly,(SCM)gn);
  SCM_SETCAR(new_dly,mus_scm_tag);
#endif
  gn->vcts[MUS_DATA_POSITION] = make_vct(max_size,line);
  return(new_dly);
}

static SCM g_make_delay(SCM args) 
{
  #define H_make_delay "(" S_make_delay " &opt-key size initial-contents (initial-element 0.0) max-size)\n\
   returns a new delay line of size elements.\n\
   If the delay length will be changing, max-size determines its maximum length\n\
     (" S_make_delay " len :max-size (+ len 10))\n\
   provides 10 extra elements of delay for subsequent phasing.\n\
   initial-contents can be either a list or a vct object."

  return(g_make_delay_1(G_DELAY,args));
}

static SCM g_make_comb(SCM args) 
{
  #define H_make_comb "(" S_make_comb " &opt-key scaler size initial-contents (initial-element 0.0) max-size)\n\
   returns a new comb filter (a delay line with a scaler on the feedback) of size elements.\n\
   If the comb length will be changing, max-size determines its maximum length.\n\
   initial-contents can be either a list or a vct object."

  return(g_make_delay_1(G_COMB,args));
}

static SCM g_make_notch(SCM args) 
{
  #define H_make_notch "(" S_make_notch " &opt-key scaler size initial-contents (initial-element 0.0) max-size)\n\
   returns a new notch filter (a delay line with a scaler on the feedforward) of size elements.\n\
   If the notch length will be changing, max-size determines its maximum length.\n\
   initial-contents can be either a list or a vct object."

  return(g_make_delay_1(G_NOTCH,args));
}

static SCM g_make_all_pass(SCM args) 
{
  #define H_make_all_pass "(" S_make_all_pass " &opt-key feedback feedforward size initial-contents (initial-element 0.0) max-size)\n\
   returns a new allpass filter (a delay line with a scalers on both the feedback and the feedforward).\n\
   If the all-pass length will be changing, max-size determines its maximum length.\n\
   initial-contents can be either a list or a vct object."

  return(g_make_delay_1(G_ALL_PASS,args));
}

static SCM g_delay(SCM obj, SCM input, SCM pm)
{
  #define H_delay "(" S_delay " gen &optional (val 0.0) (pm 0.0))\n\
   delays val according to the delay line's length and pm ('phase-modulation').\n\
   The Scheme built-in 'delay' function is named %delay.\n\
   If pm is greater than 0.0, the max-size argument used to create gen should have accomodated its maximum value."

  Float in1 = 0.0, pm1 = 0.0;
  SCM_ASSERT(((mus_scm_p(obj)) && (mus_delay_p(mus_get_any(obj)))),obj,SCM_ARG1,S_delay);
  if (SCM_NFALSEP(scm_real_p(input))) in1 = gh_scm2double(input); else if (!(SCM_UNBNDP(input))) scm_wrong_type_arg(S_delay,2,input);
  if (SCM_NFALSEP(scm_real_p(pm))) pm1 = gh_scm2double(pm); else if (!(SCM_UNBNDP(pm))) scm_wrong_type_arg(S_delay,3,pm);
  return(gh_double2scm(mus_delay(mus_get_any(obj),in1,pm1)));
}

static SCM g_notch(SCM obj, SCM input, SCM pm)
{
  #define H_notch "(" S_notch " gen &optional (val 0.0) (pm 0.0)) notch filters val, pm changes the delay length."
  Float in1 = 0.0, pm1 = 0.0;
  SCM_ASSERT(((mus_scm_p(obj)) && (mus_notch_p(mus_get_any(obj)))),obj,SCM_ARG1,S_notch);
  if (SCM_NFALSEP(scm_real_p(input))) in1 = gh_scm2double(input); else if (!(SCM_UNBNDP(input))) scm_wrong_type_arg(S_notch,2,input);
  if (SCM_NFALSEP(scm_real_p(pm))) pm1 = gh_scm2double(pm); else if (!(SCM_UNBNDP(pm))) scm_wrong_type_arg(S_notch,3,pm);
  return(gh_double2scm(mus_notch(mus_get_any(obj),in1,pm1)));
}

static SCM g_comb(SCM obj, SCM input, SCM pm)
{
  #define H_comb "(" S_comb " gen &optional (val 0.0) (pm 0.0)) comb filters val, pm changes the delay length."
  Float in1 = 0.0, pm1 = 0.0;
  SCM_ASSERT(((mus_scm_p(obj)) && (mus_comb_p(mus_get_any(obj)))),obj,SCM_ARG1,S_comb);
  if (SCM_NFALSEP(scm_real_p(input))) in1 = gh_scm2double(input); else if (!(SCM_UNBNDP(input))) scm_wrong_type_arg(S_comb,2,input);
  if (SCM_NFALSEP(scm_real_p(pm))) pm1 = gh_scm2double(pm); else if (!(SCM_UNBNDP(pm))) scm_wrong_type_arg(S_comb,3,pm);
  return(gh_double2scm(mus_comb(mus_get_any(obj),in1,pm1)));
}

static SCM g_all_pass(SCM obj, SCM input, SCM pm)
{
  #define H_all_pass "(" S_all_pass " gen &optional (val 0.0) (pm 0.0)) all-pass filters val, pm changes the delay length."
  Float in1 = 0.0, pm1 = 0.0;
  SCM_ASSERT(((mus_scm_p(obj)) && (mus_all_pass_p(mus_get_any(obj)))),obj,SCM_ARG1,S_all_pass);
  if (SCM_NFALSEP(scm_real_p(input))) in1 = gh_scm2double(input); else if (!(SCM_UNBNDP(input))) scm_wrong_type_arg(S_all_pass,2,input);
  if (SCM_NFALSEP(scm_real_p(pm))) pm1 = gh_scm2double(pm); else if (!(SCM_UNBNDP(pm))) scm_wrong_type_arg(S_all_pass,3,pm);
  return(gh_double2scm(mus_all_pass(mus_get_any(obj),in1,pm1)));
}

static SCM g_tap(SCM obj, SCM loc)
{
  #define H_tap "(" S_tap " gen &optional (pm 0.0)) taps the " S_delay " generator offset by pm"
  Float dloc = 0.0;
  SCM_ASSERT((mus_scm_p(obj)),obj,SCM_ARG1,S_tap);
  if (SCM_NFALSEP(scm_real_p(loc))) dloc = gh_scm2double(loc); else if (!(SCM_UNBNDP(loc))) scm_wrong_type_arg(S_tap,2,loc);
  return(gh_double2scm(mus_tap(mus_get_any(obj),dloc)));
}

static SCM g_delay_p(SCM obj) 
{
  #define H_delay_p "(" S_delay_p " gen) -> #t if gen is a delay line, else #f"
  return(((mus_scm_p(obj)) && (mus_delay_p(mus_get_any(obj)))) ? SCM_BOOL_T : SCM_BOOL_F);
}

static SCM g_comb_p(SCM obj)
{
  #define H_comb_p "(" S_comb_p " gen) -> #t if gen is a comb filter, else #f"
  return(((mus_scm_p(obj)) && (mus_comb_p(mus_get_any(obj)))) ? SCM_BOOL_T : SCM_BOOL_F);
}

static SCM g_notch_p(SCM obj) 
{
  #define H_notch_p "(" S_notch_p " gen) -> #t if gen is a notch filter, else #f"
  return(((mus_scm_p(obj)) && (mus_notch_p(mus_get_any(obj)))) ? SCM_BOOL_T : SCM_BOOL_F);
}

static SCM g_all_pass_p(SCM obj) 
{
  #define H_all_pass_p "(" S_all_pass_p " gen) -> #t if gen is an all-pass filter, else #f"
  return(((mus_scm_p(obj)) && (mus_all_pass_p(mus_get_any(obj)))) ? SCM_BOOL_T : SCM_BOOL_F);
}

static SCM g_feedback(SCM obj)
{
  #define H_mus_feedback "(" S_mus_feedback " gen) -> feedback value of gen"
  SCM_ASSERT((mus_scm_p(obj)),obj,SCM_ARG1,S_mus_feedback);
  return(gh_double2scm(mus_feedback(mus_get_any(obj))));
}

static SCM g_set_feedback(SCM obj, SCM val)
{
  #define H_mus_set_feedback "(" S_mus_set_feedback " gen val) sets gen's feedback term to val"
  SCM_ASSERT((mus_scm_p(obj)),obj,SCM_ARG1,S_mus_set_feedback);
  SCM_ASSERT((SCM_NFALSEP(scm_real_p(val))),val,SCM_ARG2,S_mus_set_feedback);
  return(gh_double2scm(mus_set_feedback(mus_get_any(obj),gh_scm2double(val))));
}

static SCM g_feedforward(SCM obj)
{
  #define H_mus_feedforward "(" S_mus_feedforward " gen) -> feedforward term of gen"
  SCM_ASSERT((mus_scm_p(obj)),obj,SCM_ARG1,S_mus_feedforward);
  return(gh_double2scm(mus_feedforward(mus_get_any(obj))));
}

static SCM g_set_feedforward(SCM obj, SCM val)
{
  #define H_mus_set_feedforward "(" S_mus_set_feedforward " gen val) sets gen's feedforward terms to val"
  SCM_ASSERT((mus_scm_p(obj)),obj,SCM_ARG1,S_mus_set_feedforward);
  SCM_ASSERT((SCM_NFALSEP(scm_real_p(val))),val,SCM_ARG2,S_mus_set_feedforward);
  return(gh_double2scm(mus_set_feedforward(mus_get_any(obj),gh_scm2double(val))));
}

static void init_dly(void)
{
  gh_eval_str("(define %delay delay)"); /* protect the original meaning */
  DEFINE_PROC(gh_new_procedure(S_make_delay,SCM_FNC g_make_delay,0,0,1),H_make_delay);
  DEFINE_PROC(gh_new_procedure(S_make_comb,SCM_FNC g_make_comb,0,0,1),H_make_comb);
  DEFINE_PROC(gh_new_procedure(S_make_notch,SCM_FNC g_make_notch,0,0,1),H_make_notch); 
  DEFINE_PROC(gh_new_procedure(S_make_all_pass,SCM_FNC g_make_all_pass,0,0,1),H_make_all_pass);
  DEFINE_PROC(gh_new_procedure(S_delay,SCM_FNC g_delay,1,2,0),H_delay); 
  DEFINE_PROC(gh_new_procedure(S_tap,SCM_FNC g_tap,1,1,0),H_tap);
  DEFINE_PROC(gh_new_procedure(S_notch,SCM_FNC g_notch,1,2,0),H_notch);
  DEFINE_PROC(gh_new_procedure(S_comb,SCM_FNC g_comb,1,2,0),H_comb);
  DEFINE_PROC(gh_new_procedure(S_all_pass,SCM_FNC g_all_pass,1,2,0),H_all_pass);
  DEFINE_PROC(gh_new_procedure(S_delay_p,SCM_FNC g_delay_p,1,0,0),H_delay_p);
  DEFINE_PROC(gh_new_procedure(S_notch_p,SCM_FNC g_notch_p,1,0,0),H_notch_p);
  DEFINE_PROC(gh_new_procedure(S_comb_p,SCM_FNC g_comb_p,1,0,0),H_comb_p);
  DEFINE_PROC(gh_new_procedure(S_all_pass_p,SCM_FNC g_all_pass_p,1,0,0),H_all_pass_p);
  DEFINE_PROC(gh_new_procedure(S_mus_feedback,SCM_FNC g_feedback,1,0,0),H_mus_feedback);
  DEFINE_PROC(gh_new_procedure(S_mus_set_feedback,SCM_FNC g_set_feedback,2,0,0),H_mus_set_feedback);
  DEFINE_PROC(gh_new_procedure(S_mus_feedforward,SCM_FNC g_feedforward,1,0,0),H_mus_feedforward);
  DEFINE_PROC(gh_new_procedure(S_mus_set_feedforward,SCM_FNC g_set_feedforward,2,0,0),H_mus_set_feedforward);
}


/* -------- sum-of-cosines -------- */

#define S_make_sum_of_cosines "make-sum-of-cosines"
#define S_sum_of_cosines "sum-of-cosines"
#define S_sum_of_cosines_p "sum-of-cosines?"
#define S_mus_cosines "mus-cosines"

static SCM g_sum_of_cosines_p(SCM obj) 
{
  #define H_sum_of_cosines_p "(" S_sum_of_cosines_p " gen) -> #t if gen is a " S_sum_of_cosines " generator, else #f"
  return(((mus_scm_p(obj)) && (mus_sum_of_cosines_p(mus_get_any(obj)))) ? SCM_BOOL_T : SCM_BOOL_F);
}

static SCM g_make_sum_of_cosines(SCM arg1, SCM arg2, SCM arg3, SCM arg4, SCM arg5, SCM arg6)
{
  #define H_make_sum_of_cosines "(" S_make_sum_of_cosines " &opt-key (frequency 440.0) (initial-phase 0.0) (cosines 1))\n\
   returns a new " S_sum_of_cosines " generator, producing a band-limited pulse train."

#if HAVE_GUILE_1_3_0
  SCM new_cosp;
#endif
  mus_scm *gn;
  SCM args[6],keys[3];
  int orig_arg[3] = {0,0,0};
  int vals;
  int cosines = 1;
  Float freq = 440.0;
  Float phase = 0.0;
  keys[0] = all_keys[C_cosines];
  keys[1] = all_keys[C_frequency];
  keys[2] = all_keys[C_initial_phase];
  args[0] = arg1; args[1] = arg2; args[2] = arg3; args[3] = arg4; args[4] = arg5; args[5] = arg6; 
  vals = decode_keywords(S_make_sum_of_cosines,3,keys,6,args,orig_arg);
  if (vals > 0)
    {
      cosines = ikeyarg(keys[0],S_make_sum_of_cosines,orig_arg[0]+1,args[orig_arg[0]],cosines);
      freq = fkeyarg(keys[1],S_make_sum_of_cosines,orig_arg[1]+1,args[orig_arg[1]],freq);
      phase = fkeyarg(keys[2],S_make_sum_of_cosines,orig_arg[2]+1,args[orig_arg[2]],phase);
    }
  gn = (mus_scm *)CALLOC(1,sizeof(mus_scm));
  gn->gen = mus_make_sum_of_cosines(cosines,freq,phase);
  gn->nvcts = 0;
#if (!HAVE_GUILE_1_3_0)
  SCM_RETURN_NEWSMOB(mus_scm_tag,gn);
#else
  SCM_NEWCELL(new_cosp);
  SCM_SETCDR(new_cosp,(SCM)gn);
  SCM_SETCAR(new_cosp,mus_scm_tag);
  return(new_cosp);
#endif
}

static SCM g_sum_of_cosines(SCM obj, SCM fm)
{
  #define H_sum_of_cosines "(" S_sum_of_cosines " gen &optional (fm 0.0))\n\
    gets the next sample of the band-limited pulse-train produced by gen"

  Float fm1 = 0.0;
  SCM_ASSERT(((mus_scm_p(obj)) && (mus_sum_of_cosines_p(mus_get_any(obj)))),obj,SCM_ARG1,S_sum_of_cosines);
  if (SCM_NFALSEP(scm_real_p(fm))) fm1 = gh_scm2double(fm); else if (!(SCM_UNBNDP(fm))) scm_wrong_type_arg(S_sum_of_cosines,2,fm);
  return(gh_double2scm(mus_sum_of_cosines(mus_get_any(obj),fm1)));
}

static SCM g_cosines(SCM obj)
{
  #define H_mus_cosines "(" S_mus_cosines " gen) -> number of cosines produced by gen"
  SCM_ASSERT((mus_scm_p(obj)),obj,SCM_ARG1,S_mus_cosines);
  return(gh_double2scm(mus_cosines(mus_get_any(obj))));
}

static void init_cosp(void)
{
  DEFINE_PROC(gh_new_procedure(S_make_sum_of_cosines,SCM_FNC g_make_sum_of_cosines,0,6,0),H_make_sum_of_cosines); 
  DEFINE_PROC(gh_new_procedure(S_sum_of_cosines,SCM_FNC g_sum_of_cosines,1,1,0),H_sum_of_cosines);
  DEFINE_PROC(gh_new_procedure(S_sum_of_cosines_p,SCM_FNC g_sum_of_cosines_p,1,0,0),H_sum_of_cosines_p);
  DEFINE_PROC(gh_new_procedure(S_mus_cosines,SCM_FNC g_cosines,1,0,0),H_mus_cosines);
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
#if HAVE_GUILE_1_3_0
  SCM new_noi;
#endif
  mus_scm *gn;
  SCM args[4],keys[2];
  int orig_arg[2] = {0,0};
  int vals;
  Float freq = 440.0;
  Float base = 1.0;
  keys[0] = all_keys[C_frequency];
  keys[1] = all_keys[C_amplitude];
  args[0] = arg1; args[1] = arg2; args[2] = arg3; args[3] = arg4;
  vals = decode_keywords(S_make_rand,2,keys,4,args,orig_arg);
  if (vals > 0)
    {
      freq = fkeyarg(keys[0],S_make_rand,orig_arg[0]+1,args[orig_arg[0]],freq);
      base = fkeyarg(keys[1],S_make_rand,orig_arg[1]+1,args[orig_arg[1]],base);
    }
  gn = (mus_scm *)CALLOC(1,sizeof(mus_scm));
  if (rand_case)
    gn->gen = mus_make_rand(freq,base);
  else gn->gen = mus_make_rand_interp(freq,base);
#if (!HAVE_GUILE_1_3_0)
  SCM_RETURN_NEWSMOB(mus_scm_tag,gn);
#else
  SCM_NEWCELL(new_noi);
  SCM_SETCDR(new_noi,(SCM)gn);
  SCM_SETCAR(new_noi,mus_scm_tag);
  return(new_noi);
#endif
}

static SCM g_make_rand_interp(SCM arg1, SCM arg2, SCM arg3, SCM arg4)
{
  #define H_make_rand_interp "(" S_make_rand_interp " &opt-key (frequency 440.0) (amplitude 1.0))\n\
   returns a new " S_rand_interp " generator, producing linearly interpolated random numbers.\n\
   frequency is the rate at which new end-points are chosen."

  return(g_make_noi(FALSE,arg1,arg2,arg3,arg4));
}

static SCM g_make_rand(SCM arg1, SCM arg2, SCM arg3, SCM arg4)
{
  #define H_make_rand "(" S_make_rand " &opt-key (frequency 440.0) (amplitude 1.0))\n\
   returns a new " S_rand " generator, producing a sequence of random numbers (a step  function).\n\
   frequency is the rate at which new numbers are chosen."

  return(g_make_noi(TRUE,arg1,arg2,arg3,arg4));
}

static SCM g_rand(SCM obj, SCM fm)
{
  #define H_rand "(" S_rand " gen &optional (fm 0.0)) -> gen's current random number.\n\
   fm can modulate the rate at which the current number is changed."

  Float fm1 = 0.0;
  SCM_ASSERT(((mus_scm_p(obj)) && (mus_rand_p(mus_get_any(obj)))),obj,SCM_ARG1,S_rand);
  if (SCM_NFALSEP(scm_real_p(fm))) fm1 = gh_scm2double(fm); else if (!(SCM_UNBNDP(fm))) scm_wrong_type_arg(S_rand,2,fm);
  return(gh_double2scm(mus_rand(mus_get_any(obj),fm1)));
}

static SCM g_rand_p(SCM obj) 
{
  #define H_rand_p "(" S_rand_p " gen) -> #t if gen is a " S_rand " generator, else #f"
  return(((mus_scm_p(obj)) && (mus_rand_p(mus_get_any(obj)))) ? SCM_BOOL_T : SCM_BOOL_F);
}

static SCM g_rand_interp(SCM obj, SCM fm)
{
  #define H_rand_interp "(" S_rand_interp " gen &optional (fm 0.0)) -> gen's current (interpolating) random number.\n\
   fm can modulate the rate at which new segment end-points are chosen."

  Float fm1 = 0.0;
  SCM_ASSERT(((mus_scm_p(obj)) && (mus_rand_interp_p(mus_get_any(obj)))),obj,SCM_ARG1,S_rand_interp);
  if (SCM_NFALSEP(scm_real_p(fm))) fm1 = gh_scm2double(fm); else if (!(SCM_UNBNDP(fm))) scm_wrong_type_arg(S_rand_interp,2,fm);
  return(gh_double2scm(mus_rand_interp(mus_get_any(obj),fm1)));
}

static SCM g_rand_interp_p(SCM obj) 
{
  #define H_rand_interp_p "(" S_rand_interp_p " gen) -> #t if gen is a " S_rand_interp " generator, else #f"
  return(((mus_scm_p(obj)) && (mus_rand_interp_p(mus_get_any(obj)))) ? SCM_BOOL_T : SCM_BOOL_F);
}

static SCM g_mus_random(SCM a) 
{
  #define H_mus_random "(" S_mus_random " val) -> a random number between -val and val\n\
   the built-in 'random' function returns values between 0 and its argument"

  SCM_ASSERT((SCM_NFALSEP(scm_real_p(a))),a,SCM_ARG1,S_mus_random);
  return(gh_double2scm(mus_random(gh_scm2double(a))));
}

static SCM g_set_rand_seed(SCM a) 
{
  #define H_mus_set_rand_seed "(" S_mus_set_rand_seed " val) sets the random number seed\n\
   this can be used to re-run a particular random number sequence."

  SCM_ASSERT((SCM_NFALSEP(scm_real_p(a))),a,SCM_ARG1,S_mus_set_rand_seed);
  mus_set_rand_seed(g_scm2int(a)); 
  return(a);
}

static void init_noi(void)
{
  DEFINE_PROC(gh_new_procedure(S_make_rand,SCM_FNC g_make_rand,0,4,0),H_make_rand);
  DEFINE_PROC(gh_new_procedure(S_make_rand_interp,SCM_FNC g_make_rand_interp,0,4,0),H_make_rand_interp);
  DEFINE_PROC(gh_new_procedure(S_rand,SCM_FNC g_rand,1,1,0),H_rand);
  DEFINE_PROC(gh_new_procedure(S_rand_interp,SCM_FNC g_rand_interp,1,1,0),H_rand_interp);
  DEFINE_PROC(gh_new_procedure(S_rand_p,SCM_FNC g_rand_p,1,0,0),H_rand_p);
  DEFINE_PROC(gh_new_procedure(S_rand_interp_p,SCM_FNC g_rand_interp_p,1,0,0),H_rand_interp_p);
  DEFINE_PROC(gh_new_procedure1_0(S_mus_random,g_mus_random),H_mus_random);
  DEFINE_PROC(gh_new_procedure1_0(S_mus_set_rand_seed,g_set_rand_seed),H_mus_set_rand_seed);
}



/* ---------------- table lookup ---------------- */

static int DEFAULT_TABLE_SIZE = 512;

#define S_table_lookup_p "table-lookup?"
#define S_make_table_lookup "make-table-lookup"
#define S_table_lookup "table-lookup"
#define S_partials2wave "partials->wave"
#define S_phasepartials2wave "phase-partials->wave"

static SCM g_table_lookup_p(SCM obj) 
{
  #define H_table_lookup_p "(" S_table_lookup_p " gen) -> #t if gen is a " S_table_lookup " generator, else #f"
  return(((mus_scm_p(obj)) && (mus_table_lookup_p(mus_get_any(obj)))) ? SCM_BOOL_T : SCM_BOOL_F);
}

static SCM g_partials2wave(SCM partials, SCM utable, SCM normalize)
{
  #define H_partials2wave "(" S_partials2wave " partials &optional wave normalize)\n\
   takes a list of partials (harmonic number and associated amplitude) and produces\n\
   a waveform for use in " S_table_lookup ".  If wave (a vct object) is not given,\n\
   a new one is created.  If normalize is #t, the resulting waveform goes between -1 and 1.\n\
     (set! gen (make-table-lookup 440.0 :wave (partials->wave '(1 1.0 2 .5))))"

  vct *f;
  SCM table;
  Float *partial_data,*wave;
  int len,i;
  SCM_ASSERT(gh_list_p(partials),partials,SCM_ARG1,S_partials2wave);
  if (SCM_UNBNDP(utable))
    {
      wave = (Float *)CALLOC(DEFAULT_TABLE_SIZE,sizeof(Float));
      table = make_vct(DEFAULT_TABLE_SIZE,wave);
    }
  else table = utable;
  f = get_vct(table);
  len = gh_length(partials);
  partial_data = (Float *)CALLOC(len,sizeof(Float));
  for (i=0;i<len;i++) partial_data[i] = gh_scm2double(gh_list_ref(partials,gh_int2scm(i)));
  mus_partials2wave(partial_data,len / 2,f->data,f->length,(SCM_TRUE_P(normalize)));
  FREE(partial_data);
  return(table);
}

static SCM g_phasepartials2wave(SCM partials, SCM utable, SCM normalize)
{
  vct *f;
  SCM table;
  Float *partial_data,*wave;
  int len,i;

  #define H_phasepartials2wave "(" S_phasepartials2wave " partials &optional wave normalize)\n\
   takes a list of partials (harmonic number, amplitude, initial phase) and produces\n\
   a waveform for use in " S_table_lookup ".  If wave (a vct object) is not given,\n\
   a new one is created.  If normalize is #t, the resulting waveform goes between -1 and 1.\n\
     (set! gen (make-table-lookup 440.0 :wave (phase-partials->wave (list 1 .75 0.0 2 .25 (* pi .5)))))"

  SCM_ASSERT(gh_list_p(partials),partials,SCM_ARG1,S_phasepartials2wave);
  if (SCM_UNBNDP(utable))
    {
      wave = (Float *)CALLOC(DEFAULT_TABLE_SIZE,sizeof(Float));
      table = make_vct(DEFAULT_TABLE_SIZE,wave);
    }
  else table = utable;
  f = get_vct(table);
  len = gh_length(partials);
  partial_data = (Float *)CALLOC(len,sizeof(Float));
  for (i=0;i<len;i++) partial_data[i] = gh_scm2double(gh_list_ref(partials,gh_int2scm(i)));
  mus_phasepartials2wave(partial_data,len / 3,f->data,f->length,(SCM_TRUE_P(normalize)));
  FREE(partial_data);
  return(table);
}

static SCM g_make_table_lookup (SCM arg1, SCM arg2, SCM arg3, SCM arg4, SCM arg5, SCM arg6)
{
  #define H_make_table_lookup "(" S_make_table_lookup " &opt-key (frequency 440.0) (initial-phase 0.0) wave)\n\
   returns a new " S_table_lookup " generator.  This is known as an oscillator in other synthesis systems.\n\
   The default table size is 512; to use some other size, pass your own vct object as the 'wave'.\n\
     (set! gen (make-table-lookup 440.0 :wave (partials->wave '(1 1.0)))\n\
   is the same in effect as " S_make_oscil "."

  SCM new_tbl;
  mus_scm *gn;
  int vals,table_size = DEFAULT_TABLE_SIZE;
  SCM args[6],keys[3];
  int orig_arg[3] = {0,0,0};
  Float freq = 440.0,phase = 0.0;
  Float *table = NULL;
  SCM wave = SCM_UNDEFINED;
  vct *v;
  keys[0] = all_keys[C_frequency];
  keys[1] = all_keys[C_initial_phase];
  keys[2] = all_keys[C_wave];
  args[0] = arg1; args[1] = arg2; args[2] = arg3; args[3] = arg4; args[4] = arg5; args[5] = arg6;
  vals = decode_keywords(S_make_table_lookup,3,keys,6,args,orig_arg);
  if (vals > 0)
    {
      freq = fkeyarg(keys[0],S_make_table_lookup,orig_arg[0]+1,args[orig_arg[0]],freq);
      phase = fkeyarg(keys[1],S_make_table_lookup,orig_arg[1]+1,args[orig_arg[1]],phase);
      if (!(keyword_p(keys[2])))
	{
	  if (vct_p(keys[2]))
	    {
	      wave = keys[2];
	      v = get_vct(wave);
	      table = v->data;
	      table_size = v->length;
	    }
	  else scm_wrong_type_arg(S_make_table_lookup,orig_arg[2]+1,args[orig_arg[2]]);
	}
    }
  if (table == NULL) table = (Float *)CALLOC(table_size,sizeof(Float));
  gn = (mus_scm *)CALLOC(1,sizeof(mus_scm));
  gn->vcts = (SCM *)CALLOC(1,sizeof(SCM));
  gn->nvcts = 1;
  gn->vcts[0] = SCM_EOL;
  gn->gen = mus_make_table_lookup(freq,phase,table,table_size);
#if (!HAVE_GUILE_1_3_0)
  SCM_NEWSMOB(new_tbl,mus_scm_tag,gn);
#else
  SCM_NEWCELL(new_tbl);
  SCM_SETCDR(new_tbl,(SCM)gn);
  SCM_SETCAR(new_tbl,mus_scm_tag);
#endif
  if (SCM_UNBNDP(wave)) wave = make_vct(table_size,table);
  gn->vcts[0] = wave;
  return(new_tbl);
}

static SCM g_table_lookup (SCM obj, SCM fm) 
{
  #define H_table_lookup "(" S_table_lookup " gen &optional (fm 0.0)) performs interpolated table-lookup\n\
   with 'wrap-around' when gen's phase marches off the end of its table."

  Float fm1 = 0.0;
  SCM_ASSERT(((mus_scm_p(obj)) && (mus_table_lookup_p(mus_get_any(obj)))),obj,SCM_ARG1,S_table_lookup);
  if (SCM_NFALSEP(scm_real_p(fm))) fm1 = gh_scm2double(fm);  else if (!(SCM_UNBNDP(fm))) scm_wrong_type_arg(S_table_lookup,2,fm);
  return(gh_double2scm(mus_table_lookup(mus_get_any(obj),fm1)));
}

static void init_tbl(void)
{
  DEFINE_PROC(gh_new_procedure1_0(S_table_lookup_p,g_table_lookup_p),H_table_lookup_p);
  DEFINE_PROC(gh_new_procedure(S_make_table_lookup,SCM_FNC g_make_table_lookup,0,6,0),H_make_table_lookup);
  DEFINE_PROC(gh_new_procedure(S_table_lookup,SCM_FNC g_table_lookup,1,1,0),H_table_lookup);
  DEFINE_PROC(gh_new_procedure1_2(S_partials2wave,g_partials2wave),H_partials2wave);
  DEFINE_PROC(gh_new_procedure1_2(S_phasepartials2wave,g_phasepartials2wave),H_phasepartials2wave);
}


/* ---------------- sawtooth et al ---------------- */

#define S_make_sawtooth_wave "make-sawtooth-wave"
#define S_sawtooth_wave "sawtooth-wave"
#define S_sawtooth_wave_p "sawtooth-wave?"
#define S_make_square_wave "make-square-wave"
#define S_square_wave "square-wave"
#define S_square_wave_p "square-wave?"
#define S_make_triangle_wave "make-triangle-wave"
#define S_triangle_wave "triangle-wave"
#define S_triangle_wave_p "triangle-wave?"
#define S_make_pulse_train "make-pulse-train"
#define S_pulse_train "pulse-train"
#define S_pulse_train_p "pulse-train?"

enum {G_SAWTOOTH_WAVE,G_SQUARE_WAVE,G_TRIANGLE_WAVE,G_PULSE_TRAIN};

static SCM g_make_sw(int type, Float def_phase, SCM arg1, SCM arg2, SCM arg3, SCM arg4, SCM arg5, SCM arg6)
{
#if HAVE_GUILE_1_3_0
  SCM new_sw;
#endif
  mus_scm *gn;
  char *caller=NULL;
  SCM args[6],keys[3];
  int orig_arg[3] = {0,0,0};
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
  vals = decode_keywords(caller,3,keys,6,args,orig_arg);
  if (vals > 0)
    {
      freq = fkeyarg(keys[0],caller,orig_arg[0]+1,args[orig_arg[0]],freq);
      base = fkeyarg(keys[1],caller,orig_arg[1]+1,args[orig_arg[1]],base);
      phase = fkeyarg(keys[2],caller,orig_arg[2]+1,args[orig_arg[2]],phase);
    }
  gn = (mus_scm *)CALLOC(1,sizeof(mus_scm));
  switch (type)
    {
    case G_SAWTOOTH_WAVE: gn->gen = mus_make_sawtooth_wave(freq,base,phase); break;
    case G_SQUARE_WAVE: gn->gen = mus_make_square_wave(freq,base,phase); break;
    case G_TRIANGLE_WAVE: gn->gen = mus_make_triangle_wave(freq,base,phase); break;
    case G_PULSE_TRAIN: gn->gen = mus_make_pulse_train(freq,base,phase); break;
    }
#if (!HAVE_GUILE_1_3_0)
  SCM_RETURN_NEWSMOB(mus_scm_tag,gn);
#else
  SCM_NEWCELL(new_sw);
  SCM_SETCDR(new_sw,(SCM)gn);
  SCM_SETCAR(new_sw,mus_scm_tag);
  return(new_sw);
#endif
}

static SCM g_make_sawtooth_wave(SCM arg1, SCM arg2, SCM arg3, SCM arg4, SCM arg5, SCM arg6) 
{
  #define H_make_sawtooth_wave "(" S_make_sawtooth_wave " &opt-key (frequency 440.0) (amplitude 1.0) (initial-phase 0.0))\n\
   returns a new " S_sawtooth_wave " generator."

  return(g_make_sw(G_SAWTOOTH_WAVE,M_PI,arg1,arg2,arg3,arg4,arg5,arg6));
}

static SCM g_make_square_wave(SCM arg1, SCM arg2, SCM arg3, SCM arg4, SCM arg5, SCM arg6) 
{
  #define H_make_square_wave "(" S_make_square_wave " &opt-key (frequency 440.0) (amplitude 1.0) (initial-phase 0.0))\n\
   returns a new " S_square_wave " generator."

  return(g_make_sw(G_SQUARE_WAVE,0.0,arg1,arg2,arg3,arg4,arg5,arg6));
}

static SCM g_make_triangle_wave(SCM arg1, SCM arg2, SCM arg3, SCM arg4, SCM arg5, SCM arg6) 
{
  #define H_make_triangle_wave "(" S_make_triangle_wave " &opt-key (frequency 440.0) (amplitude 1.0) (initial-phase 0.0))\n\
   returns a new " S_triangle_wave " generator."

  return(g_make_sw(G_TRIANGLE_WAVE,0.0,arg1,arg2,arg3,arg4,arg5,arg6));
}

static SCM g_make_pulse_train(SCM arg1, SCM arg2, SCM arg3, SCM arg4, SCM arg5, SCM arg6) 
{
  #define H_make_pulse_train "(" S_make_pulse_train " &opt-key (frequency 440.0) (amplitude 1.0) (initial-phase 0.0))\n\
   returns a new " S_pulse_train " generator.  This produces a sequence of impulses."

  return(g_make_sw(G_PULSE_TRAIN,TWO_PI,arg1,arg2,arg3,arg4,arg5,arg6));
}

static SCM g_sawtooth_wave(SCM obj, SCM fm) 
{
  #define H_sawtooth_wave "(" S_sawtooth_wave " gen &optional (fm 0.0)) -> next sawtooth sample from gen"
  Float fm1 = 0.0;
  SCM_ASSERT(((mus_scm_p(obj)) && (mus_sawtooth_wave_p(mus_get_any(obj)))),obj,SCM_ARG1,S_sawtooth_wave);
  if (SCM_NFALSEP(scm_real_p(fm))) fm1 = gh_scm2double(fm); else if (!(SCM_UNBNDP(fm))) scm_wrong_type_arg(S_sawtooth_wave,2,fm);
  return(gh_double2scm(mus_sawtooth_wave(mus_get_any(obj),fm1)));
}

static SCM g_square_wave(SCM obj, SCM fm) 
{
  #define H_square_wave "(" S_square_wave " gen &optional (fm 0.0)) -> next square wave sample from gen"
  Float fm1 = 0.0;
  SCM_ASSERT(((mus_scm_p(obj)) && (mus_square_wave_p(mus_get_any(obj)))),obj,SCM_ARG1,S_square_wave);
  if (SCM_NFALSEP(scm_real_p(fm))) fm1 = gh_scm2double(fm); else if (!(SCM_UNBNDP(fm))) scm_wrong_type_arg(S_square_wave,2,fm);
  return(gh_double2scm(mus_square_wave(mus_get_any(obj),fm1)));
}

static SCM g_triangle_wave(SCM obj, SCM fm) 
{
  #define H_triangle_wave "(" S_triangle_wave " gen &optional (fm 0.0)) -> next triangle wave sample from gen"
  Float fm1 = 0.0;
  SCM_ASSERT(((mus_scm_p(obj)) && (mus_triangle_wave_p(mus_get_any(obj)))),obj,SCM_ARG1,S_triangle_wave);
  if (SCM_NFALSEP(scm_real_p(fm))) fm1 = gh_scm2double(fm); else if (!(SCM_UNBNDP(fm))) scm_wrong_type_arg(S_triangle_wave,2,fm);
  return(gh_double2scm(mus_triangle_wave(mus_get_any(obj),fm1)));
}

static SCM g_pulse_train(SCM obj, SCM fm) 
{
  #define H_pulse_train "(" S_pulse_train " gen &optional (fm 0.0)) -> next (im)pulse train sample from gen"
  Float fm1 = 0.0;
  SCM_ASSERT(((mus_scm_p(obj)) && (mus_pulse_train_p(mus_get_any(obj)))),obj,SCM_ARG1,S_pulse_train);
  if (SCM_NFALSEP(scm_real_p(fm))) fm1 = gh_scm2double(fm); else if (!(SCM_UNBNDP(fm))) scm_wrong_type_arg(S_pulse_train,2,fm);
  return(gh_double2scm(mus_pulse_train(mus_get_any(obj),fm1)));
}

static SCM g_sawtooth_wave_p(SCM obj) 
{
  #define H_sawtooth_wave_p "(" S_sawtooth_wave_p " gen) -> #t if gen is a " S_sawtooth_wave " generator, else #f"
  return(((mus_scm_p(obj)) && (mus_sawtooth_wave_p(mus_get_any(obj)))) ? SCM_BOOL_T : SCM_BOOL_F);
}

static SCM g_square_wave_p(SCM obj) 
{
  #define H_square_wave_p "(" S_square_wave_p " gen) -> #t if gen is a " S_square_wave " generator, else #f"
  return(((mus_scm_p(obj)) && (mus_square_wave_p(mus_get_any(obj)))) ? SCM_BOOL_T : SCM_BOOL_F);
}

static SCM g_triangle_wave_p(SCM obj) 
{
  #define H_triangle_wave_p "(" S_triangle_wave_p " gen) -> #t if gen is a " S_triangle_wave " generator, else #f"
  return(((mus_scm_p(obj)) && (mus_triangle_wave_p(mus_get_any(obj)))) ? SCM_BOOL_T : SCM_BOOL_F);
}

static SCM g_pulse_train_p(SCM obj) 
{
  #define H_pulse_train_p "(" S_pulse_train_p " gen) -> #t if gen is a " S_pulse_train " generator, else #f"
  return(((mus_scm_p(obj)) && (mus_pulse_train_p(mus_get_any(obj)))) ? SCM_BOOL_T : SCM_BOOL_F);
}

static void init_sw(void)
{
  DEFINE_PROC(gh_new_procedure(S_make_sawtooth_wave,SCM_FNC g_make_sawtooth_wave,0,6,0),H_make_sawtooth_wave);
  DEFINE_PROC(gh_new_procedure(S_sawtooth_wave,SCM_FNC g_sawtooth_wave,1,1,0),H_sawtooth_wave);
  DEFINE_PROC(gh_new_procedure(S_sawtooth_wave_p,SCM_FNC g_sawtooth_wave_p,1,0,0),H_sawtooth_wave_p);
  DEFINE_PROC(gh_new_procedure(S_make_triangle_wave,SCM_FNC g_make_triangle_wave,0,6,0),H_make_triangle_wave);
  DEFINE_PROC(gh_new_procedure(S_triangle_wave,SCM_FNC g_triangle_wave,1,1,0),H_triangle_wave);
  DEFINE_PROC(gh_new_procedure(S_triangle_wave_p,SCM_FNC g_triangle_wave_p,1,0,0),H_triangle_wave_p);
  DEFINE_PROC(gh_new_procedure(S_make_square_wave,SCM_FNC g_make_square_wave,0,6,0),H_make_square_wave);
  DEFINE_PROC(gh_new_procedure(S_square_wave,SCM_FNC g_square_wave,1,1,0),H_square_wave);
  DEFINE_PROC(gh_new_procedure(S_square_wave_p,SCM_FNC g_square_wave_p,1,0,0),H_square_wave_p);
  DEFINE_PROC(gh_new_procedure(S_make_pulse_train,SCM_FNC g_make_pulse_train,0,6,0),H_make_pulse_train);
  DEFINE_PROC(gh_new_procedure(S_pulse_train,SCM_FNC g_pulse_train,1,1,0),H_pulse_train);
  DEFINE_PROC(gh_new_procedure(S_pulse_train_p,SCM_FNC g_pulse_train_p,1,0,0),H_pulse_train_p);
}


/* ---------------- asymmetric-fm ---------------- */

#define S_make_asymmetric_fm "make-asymmetric-fm"
#define S_asymmetric_fm "asymmetric-fm"
#define S_asymmetric_fm_p "asymmetric-fm?"

static SCM g_make_asymmetric_fm(SCM arg1, SCM arg2, SCM arg3, SCM arg4, SCM arg5, SCM arg6, SCM arg7, SCM arg8)
{
  #define H_make_asymmetric_fm "(" S_make_asymmetric_fm " &opt-key (frequency 440.0) (initial-phase 0.0) (r 1.0) (ratio 1.0))\n\
   returns a new " S_asymmetric_fm " generator."

#if HAVE_GUILE_1_3_0
  SCM new_asyfm;
#endif
  mus_scm *gn;
  SCM args[8],keys[4];
  int orig_arg[4] = {0,0,0};
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
  vals = decode_keywords(S_make_asymmetric_fm,4,keys,8,args,orig_arg);
  if (vals > 0)
    {
      freq = fkeyarg(keys[0],S_make_asymmetric_fm,orig_arg[0]+1,args[orig_arg[0]],freq);
      phase = fkeyarg(keys[1],S_make_asymmetric_fm,orig_arg[1]+1,args[orig_arg[1]],phase);
      r = fkeyarg(keys[2],S_make_asymmetric_fm,orig_arg[2]+1,args[orig_arg[2]],r);
      ratio = fkeyarg(keys[3],S_make_asymmetric_fm,orig_arg[3]+1,args[orig_arg[3]],ratio);
    }
  gn = (mus_scm *)CALLOC(1,sizeof(mus_scm));
  gn->gen = mus_make_asymmetric_fm(freq,phase,r,ratio);
#if (!HAVE_GUILE_1_3_0)
  SCM_RETURN_NEWSMOB(mus_scm_tag,gn);
#else
  SCM_NEWCELL(new_asyfm);
  SCM_SETCDR(new_asyfm,(SCM)gn);
  SCM_SETCAR(new_asyfm,mus_scm_tag);
  return(new_asyfm);
#endif
}

static SCM g_asymmetric_fm(SCM obj, SCM index, SCM fm)
{
  #define H_asymmetric_fm "(" S_asymmetric_fm " gen &optional (index 0.0) (fm 0.0)) -> next sample from asymmetric fm gen"
  Float fm1 = 0.0, index1 = 0.0;
  SCM_ASSERT(((mus_scm_p(obj)) && (mus_asymmetric_fm_p(mus_get_any(obj)))),obj,SCM_ARG1,S_asymmetric_fm);
  if (SCM_NFALSEP(scm_real_p(fm))) fm1 = gh_scm2double(fm); else if (!(SCM_UNBNDP(fm))) scm_wrong_type_arg(S_asymmetric_fm,3,fm);
  if (SCM_NFALSEP(scm_real_p(index))) index1 = gh_scm2double(index); else if (!(SCM_UNBNDP(index))) scm_wrong_type_arg(S_asymmetric_fm,2,index);
  return(gh_double2scm(mus_asymmetric_fm(mus_get_any(obj),index1,fm1)));
}

static SCM g_asymmetric_fm_p(SCM obj) 
{
  #define H_asymmetric_fm_p "(" S_asymmetric_fm_p " gen) -> #t if gen is a " S_asymmetric_fm " generator, else #f"
  return(((mus_scm_p(obj)) && (mus_asymmetric_fm_p(mus_get_any(obj)))) ? SCM_BOOL_T : SCM_BOOL_F);
}

static void init_asyfm(void)
{
  DEFINE_PROC(gh_new_procedure(S_make_asymmetric_fm,SCM_FNC g_make_asymmetric_fm,0,8,0),H_make_asymmetric_fm);
  DEFINE_PROC(gh_new_procedure(S_asymmetric_fm,SCM_FNC g_asymmetric_fm,1,2,0),H_asymmetric_fm);
  DEFINE_PROC(gh_new_procedure(S_asymmetric_fm_p,SCM_FNC g_asymmetric_fm_p,1,0,0),H_asymmetric_fm_p);
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
#define S_mus_a0            "mus-a0"
#define S_mus_a1            "mus-a1"
#define S_mus_a2            "mus-a2"
#define S_mus_b1            "mus-b1"
#define S_mus_b2            "mus-b2"
#define S_mus_set_a0        "mus-set-a0"
#define S_mus_set_a1        "mus-set-a1"
#define S_mus_set_a2        "mus-set-a2"
#define S_mus_set_b1        "mus-set-b1"
#define S_mus_set_b2        "mus-set-b2"

enum {G_ONE_POLE,G_ONE_ZERO,G_TWO_POLE,G_TWO_ZERO,G_ZPOLAR,G_PPOLAR};
static char *smpflts[6] = {S_make_one_pole,S_make_one_zero,S_make_two_pole,S_make_two_zero,S_make_zpolar,S_make_ppolar};

static SCM g_make_smpflt_1(int choice, SCM arg1, SCM arg2, SCM arg3, SCM arg4)
{
#if HAVE_GUILE_1_3_0
  SCM new_smpflt;
#endif
  mus_scm *gn;
  SCM args[4],keys[2];
  int orig_arg[2] = {0,0};
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
  vals = decode_keywords(smpflts[choice],2,keys,4,args,orig_arg);
  if (vals > 0)
    {
      a0 = fkeyarg(keys[0],smpflts[choice],orig_arg[0]+1,args[orig_arg[0]],a0);
      a1 = fkeyarg(keys[1],smpflts[choice],orig_arg[1]+1,args[orig_arg[1]],a1);
    }
  gn = (mus_scm *)CALLOC(1,sizeof(mus_scm));
  switch (choice)
    {
    case G_ONE_ZERO: gn->gen = mus_make_one_zero(a0,a1); break;
    case G_ONE_POLE: gn->gen = mus_make_one_pole(a0,a1); break;
    case G_ZPOLAR: gn->gen = mus_make_zpolar(a0,a1); break;
    case G_PPOLAR: gn->gen = mus_make_ppolar(a0,a1); break;
    }
#if (!HAVE_GUILE_1_3_0)
  SCM_RETURN_NEWSMOB(mus_scm_tag,gn);
#else
  SCM_NEWCELL(new_smpflt);
  SCM_SETCDR(new_smpflt,(SCM)gn);
  SCM_SETCAR(new_smpflt,mus_scm_tag);
  return(new_smpflt);
#endif
}

static SCM g_make_one_zero(SCM arg1, SCM arg2, SCM arg3, SCM arg4)
{
  #define H_make_one_zero "(" S_make_one_zero " a0 a1) -> new " S_one_zero " filter returning a0*x(n) + a1*x(n-1)"
  return(g_make_smpflt_1(G_ONE_ZERO,arg1,arg2,arg3,arg4));
}

static SCM g_make_one_pole(SCM arg1, SCM arg2, SCM arg3, SCM arg4)
{
  #define H_make_one_pole "(" S_make_one_pole " a0 b1) -> new " S_one_pole " filter returning a0*x(n) - b1*y(n-1)"
  return(g_make_smpflt_1(G_ONE_POLE,arg1,arg2,arg3,arg4));
}

static SCM g_make_zpolar(SCM arg1, SCM arg2, SCM arg3, SCM arg4)
{
  #define H_make_zpolar "(" S_make_zpolar " radius frequency) -> new " S_two_zero " filter\n\
   where the coefficients (a0..a2) are set from the desired zero's radius and center frequency.\n\
   Use this in conjunction with the " S_two_zero " generator" 

  return(g_make_smpflt_1(G_ZPOLAR,arg1,arg2,arg3,arg4));
}

static SCM g_make_ppolar(SCM arg1, SCM arg2, SCM arg3, SCM arg4)
{
  #define H_make_ppolar "(" S_make_ppolar " radius frequency) -> new " S_two_pole " filter\n\
   where the coefficients are set from the desired pole's radius and center frequency.\n\
   Use this in conjunction with the " S_two_pole " generator" 

  return(g_make_smpflt_1(G_PPOLAR,arg1,arg2,arg3,arg4));
}

static SCM g_make_smpflt_2(int choice, SCM arg1, SCM arg2, SCM arg3, SCM arg4, SCM arg5, SCM arg6)
{
#if HAVE_GUILE_1_3_0
  SCM new_smpflt;
#endif
  mus_scm *gn;
  SCM args[6],keys[3];
  int orig_arg[3] = {0,0,0};
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
  vals = decode_keywords(smpflts[choice],3,keys,6,args,orig_arg);
  if (vals > 0)
    {
      a0 = fkeyarg(keys[0],smpflts[choice],orig_arg[0]+1,args[orig_arg[0]],a0);
      a1 = fkeyarg(keys[1],smpflts[choice],orig_arg[1]+1,args[orig_arg[1]],a1);
      a2 = fkeyarg(keys[2],smpflts[choice],orig_arg[2]+1,args[orig_arg[2]],a2);
    }
  gn = (mus_scm *)CALLOC(1,sizeof(mus_scm));
  if (choice == G_TWO_ZERO)
    gn->gen = mus_make_two_zero(a0,a1,a2);
  else gn->gen = mus_make_two_pole(a0,a1,a2);
#if (!HAVE_GUILE_1_3_0)
  SCM_RETURN_NEWSMOB(mus_scm_tag,gn);
#else
  SCM_NEWCELL(new_smpflt);
  SCM_SETCDR(new_smpflt,(SCM)gn);
  SCM_SETCAR(new_smpflt,mus_scm_tag);
  return(new_smpflt);
#endif
}

static SCM g_make_two_zero(SCM arg1, SCM arg2, SCM arg3, SCM arg4, SCM arg5, SCM arg6) 
{
  #define H_make_two_zero "(" S_make_two_zero " a0 a1 a2) -> new " S_two_zero " filter\n\
   returning a0*x(n) + a1*x(n-1) + a2*x(n-2)"
  return(g_make_smpflt_2(G_TWO_ZERO,arg1,arg2,arg3,arg4,arg5,arg6));
}

static SCM g_make_two_pole(SCM arg1, SCM arg2, SCM arg3, SCM arg4, SCM arg5, SCM arg6) 
{
  #define H_make_two_pole "(" S_make_two_pole " a0 b1 b2) -> new " S_two_pole " filter\n\
   returning a0*x(n) - b1*y(n-1) - b2*y(n-2)"
  return(g_make_smpflt_2(G_TWO_POLE,arg1,arg2,arg3,arg4,arg5,arg6));
}

static SCM g_one_zero(SCM obj, SCM fm)
{
  #define H_one_zero "(" S_one_zero " gen &optional (input 0.0)) -> one zero filter of input"
  Float fm1 = 0.0;
  SCM_ASSERT(((mus_scm_p(obj)) && (mus_one_zero_p(mus_get_any(obj)))),obj,SCM_ARG1,S_one_zero);
  if (SCM_NFALSEP(scm_real_p(fm))) fm1 = gh_scm2double(fm); else if (!(SCM_UNBNDP(fm))) scm_wrong_type_arg(S_one_zero,2,fm);
  return(gh_double2scm(mus_one_zero(mus_get_any(obj),fm1)));
}

static SCM g_one_pole(SCM obj, SCM fm)
{
  #define H_one_pole "(" S_one_pole " gen &optional (input 0.0)) -> one pole filter of input"
  Float fm1 = 0.0;
  SCM_ASSERT(((mus_scm_p(obj)) && (mus_one_pole_p(mus_get_any(obj)))),obj,SCM_ARG1,S_one_pole);
  if (SCM_NFALSEP(scm_real_p(fm))) fm1 = gh_scm2double(fm); else if (!(SCM_UNBNDP(fm))) scm_wrong_type_arg(S_one_pole,2,fm);
  return(gh_double2scm(mus_one_pole(mus_get_any(obj),fm1)));
}

static SCM g_two_zero(SCM obj, SCM fm)
{
  #define H_two_zero "(" S_two_zero " gen &optional (input 0.0)) -> two zero filter of input"
  Float fm1 = 0.0;
  SCM_ASSERT(((mus_scm_p(obj)) && (mus_two_zero_p(mus_get_any(obj)))),obj,SCM_ARG1,S_two_zero);
  if (SCM_NFALSEP(scm_real_p(fm))) fm1 = gh_scm2double(fm); else if (!(SCM_UNBNDP(fm))) scm_wrong_type_arg(S_two_zero,2,fm);
  return(gh_double2scm(mus_two_zero(mus_get_any(obj),fm1)));
}

static SCM g_two_pole(SCM obj, SCM fm)
{
  #define H_two_pole "(" S_two_pole " gen &optional (input 0.0)) -> two pole filter of input"
  Float fm1 = 0.0;
  SCM_ASSERT(((mus_scm_p(obj)) && (mus_two_pole_p(mus_get_any(obj)))),obj,SCM_ARG1,S_two_pole);
  if (SCM_NFALSEP(scm_real_p(fm))) fm1 = gh_scm2double(fm); else if (!(SCM_UNBNDP(fm))) scm_wrong_type_arg(S_two_pole,2,fm);
  return(gh_double2scm(mus_two_pole(mus_get_any(obj),fm1)));
}

static SCM g_one_zero_p(SCM obj) 
{
  #define H_one_zero_p "(" S_one_zero_p " gen) -> #t if gen is a " S_one_zero " generator, else #f"
  return(((mus_scm_p(obj)) && (mus_one_zero_p(mus_get_any(obj)))) ? SCM_BOOL_T : SCM_BOOL_F);
}

static SCM g_one_pole_p(SCM obj) 
{
  #define H_one_pole_p "(" S_one_pole_p " gen) -> #t if gen is a " S_one_pole " generator, else #f"
  return(((mus_scm_p(obj)) && (mus_one_pole_p(mus_get_any(obj)))) ? SCM_BOOL_T : SCM_BOOL_F);
}

static SCM g_two_zero_p(SCM obj) 
{
  #define H_two_zero_p "(" S_two_zero_p " gen) -> #t if gen is a " S_two_zero " generator, else #f"
  return(((mus_scm_p(obj)) && (mus_two_zero_p(mus_get_any(obj)))) ? SCM_BOOL_T : SCM_BOOL_F);
}

static SCM g_two_pole_p(SCM obj) 
{
  #define H_two_pole_p "(" S_two_pole_p " gen) -> #t if gen is a " S_two_pole " generator, else #f"
  return(((mus_scm_p(obj)) && (mus_two_pole_p(mus_get_any(obj)))) ? SCM_BOOL_T : SCM_BOOL_F);
}

static SCM g_a0(SCM obj) 
{
  #define H_mus_a0 "(" S_mus_a0 " gen) -> gen's " S_mus_a0 " coefficient (scaler on x(n)), if any"
  return(gh_double2scm(mus_a0(mus_get_any(obj))));
}

static SCM g_a1(SCM obj)
{
  #define H_mus_a1 "(" S_mus_a1 " gen) -> gen's " S_mus_a1 " coefficient (scaler on x(n-1)), if any"
  return(gh_double2scm(mus_a1(mus_get_any(obj))));
}

static SCM g_a2(SCM obj)
{
  #define H_mus_a2 "(" S_mus_a2 " gen) -> gen's " S_mus_a2 " coefficient (scaler on x(n-2)), if any"
  return(gh_double2scm(mus_a2(mus_get_any(obj))));
}

static SCM g_b1(SCM obj)
{
  #define H_mus_b1 "(" S_mus_b1 " gen) -> gen's " S_mus_b1 " coefficient (scaler on y(n-1)), if any"
  return(gh_double2scm(mus_b1(mus_get_any(obj))));
}

static SCM g_b2(SCM obj)
{
  #define H_mus_b2 "(" S_mus_b2 " gen) -> gen's " S_mus_b2 " coefficient (scaler on y(n-2)), if any"
  return(gh_double2scm(mus_b2(mus_get_any(obj))));
}

static SCM g_set_a0(SCM obj, SCM val)
{
  #define H_mus_set_a0 "(" S_mus_set_a0 " gen val) sets gen's " S_mus_a0 " coefficient, if any"
  return(gh_double2scm(mus_set_a0(mus_get_any(obj),gh_scm2double(val))));
}

static SCM g_set_a1(SCM obj, SCM val)
{
  #define H_mus_set_a1 "(" S_mus_set_a1 " gen val) sets gen's " S_mus_a1 " coefficient, if any"
  return(gh_double2scm(mus_set_a1(mus_get_any(obj),gh_scm2double(val))));
}

static SCM g_set_a2(SCM obj, SCM val)
{
  #define H_mus_set_a2 "(" S_mus_set_a2 " gen val) sets gen's " S_mus_a2 " coefficient, if any"
  return(gh_double2scm(mus_set_a2(mus_get_any(obj),gh_scm2double(val))));
}

static SCM g_set_b1(SCM obj, SCM val)
{
  #define H_mus_set_b1 "(" S_mus_set_b1 " gen val) sets gen's " S_mus_b1 " coefficient, if any"
  return(gh_double2scm(mus_set_b1(mus_get_any(obj),gh_scm2double(val))));
}

static SCM g_set_b2(SCM obj, SCM val)
{
  #define H_mus_set_b2 "(" S_mus_set_b2 " gen val) sets gen's " S_mus_b2 " coefficient, if any"
  return(gh_double2scm(mus_set_b2(mus_get_any(obj),gh_scm2double(val))));
}

static void init_smpflt(void)
{
  DEFINE_PROC(gh_new_procedure(S_make_one_zero,SCM_FNC g_make_one_zero,0,4,0),H_make_one_zero);
  DEFINE_PROC(gh_new_procedure(S_one_zero,SCM_FNC g_one_zero,1,1,0),H_one_zero);
  DEFINE_PROC(gh_new_procedure(S_one_zero_p,SCM_FNC g_one_zero_p,1,0,0),H_one_zero_p);
  DEFINE_PROC(gh_new_procedure(S_make_one_pole,SCM_FNC g_make_one_pole,0,4,0),H_make_one_pole);
  DEFINE_PROC(gh_new_procedure(S_one_pole,SCM_FNC g_one_pole,1,1,0),H_one_pole);
  DEFINE_PROC(gh_new_procedure(S_one_pole_p,SCM_FNC g_one_pole_p,1,0,0),H_one_pole_p);
  DEFINE_PROC(gh_new_procedure(S_make_two_zero,SCM_FNC g_make_two_zero,0,6,0),H_make_two_zero);
  DEFINE_PROC(gh_new_procedure(S_two_zero,SCM_FNC g_two_zero,1,1,0),H_two_zero);
  DEFINE_PROC(gh_new_procedure(S_two_zero_p,SCM_FNC g_two_zero_p,1,0,0),H_two_zero_p);
  DEFINE_PROC(gh_new_procedure(S_make_two_pole,SCM_FNC g_make_two_pole,0,6,0),H_make_two_pole);
  DEFINE_PROC(gh_new_procedure(S_two_pole,SCM_FNC g_two_pole,1,1,0),H_two_pole);
  DEFINE_PROC(gh_new_procedure(S_two_pole_p,SCM_FNC g_two_pole_p,1,0,0),H_two_pole_p);
  DEFINE_PROC(gh_new_procedure(S_make_zpolar,SCM_FNC g_make_zpolar,0,4,0),H_make_zpolar);
  DEFINE_PROC(gh_new_procedure(S_make_ppolar,SCM_FNC g_make_ppolar,0,4,0),H_make_ppolar);
  DEFINE_PROC(gh_new_procedure(S_mus_a0,SCM_FNC g_a0,1,0,0),H_mus_a0);
  DEFINE_PROC(gh_new_procedure(S_mus_a1,SCM_FNC g_a1,1,0,0),H_mus_a1);
  DEFINE_PROC(gh_new_procedure(S_mus_a2,SCM_FNC g_a2,1,0,0),H_mus_a2);
  DEFINE_PROC(gh_new_procedure(S_mus_b1,SCM_FNC g_b1,1,0,0),H_mus_b1);
  DEFINE_PROC(gh_new_procedure(S_mus_b2,SCM_FNC g_b2,1,0,0),H_mus_b2);
  DEFINE_PROC(gh_new_procedure(S_mus_set_a0,SCM_FNC g_set_a0,2,0,0),H_mus_set_a0);
  DEFINE_PROC(gh_new_procedure(S_mus_set_a1,SCM_FNC g_set_a1,2,0,0),H_mus_set_a1);
  DEFINE_PROC(gh_new_procedure(S_mus_set_a2,SCM_FNC g_set_a2,2,0,0),H_mus_set_a2);
  DEFINE_PROC(gh_new_procedure(S_mus_set_b1,SCM_FNC g_set_b1,2,0,0),H_mus_set_b1);
  DEFINE_PROC(gh_new_procedure(S_mus_set_b2,SCM_FNC g_set_b2,2,0,0),H_mus_set_b2);
}



/* ---------------- formant ---------------- */

#define S_make_formant "make-formant"
#define S_formant "formant"
#define S_formant_bank "formant-bank"
#define S_formant_p "formant?"
#define S_mus_formant_radius "mus-formant-radius"
#define S_mus_set_formant_radius "mus-set-formant-radius"

static SCM g_make_formant(SCM arg1, SCM arg2, SCM arg3, SCM arg4, SCM arg5, SCM arg6)
{
  #define H_make_formant "(" S_make_formant " &opt-key radius frequency (gain 1.0))\n\
   returns a new formant generator (a resonator).  radius sets the pole radius.\n\
   frequency sets the resonance center frequency (Hz).  gain is an overall amplitude\n\
   control."

#if HAVE_GUILE_1_3_0
  SCM new_osc;
#endif
  mus_scm *gn;
  int vals;
  SCM args[6],keys[3];
  int orig_arg[3] = {0,0,0};
  Float freq = 0.0,radius = 0.0,gain=1.0;
  keys[0] = all_keys[C_radius];
  keys[1] = all_keys[C_frequency];
  keys[2] = all_keys[C_gain];
  args[0] = arg1; args[1] = arg2; args[2] = arg3; args[3] = arg4; args[4] = arg5; args[5] = arg6;
  vals = decode_keywords(S_make_formant,3,keys,6,args,orig_arg);
  if (vals > 0)
    {
      radius = fkeyarg(keys[0],S_make_formant,orig_arg[0]+1,args[orig_arg[0]],radius);
      freq = fkeyarg(keys[1],S_make_formant,orig_arg[1]+1,args[orig_arg[1]],freq);
      gain = fkeyarg(keys[2],S_make_formant,orig_arg[2]+1,args[orig_arg[2]],gain);
    }
  gn = (mus_scm *)CALLOC(1,sizeof(mus_scm));
  gn->gen = mus_make_formant(radius,freq,gain);
  gn->nvcts = 0;
#if (!HAVE_GUILE_1_3_0)
  SCM_RETURN_NEWSMOB(mus_scm_tag,gn);
#else
  SCM_NEWCELL(new_osc);
  SCM_SETCDR(new_osc,(SCM)gn);
  SCM_SETCAR(new_osc,mus_scm_tag);
  return(new_osc);
#endif
}

static SCM g_formant(SCM gen, SCM input)
{
  #define H_formant "(" S_formant " gen &optional (input 0.0)) -> next sample from resonator gen"
  Float in1 = 0.0;
  SCM_ASSERT(((mus_scm_p(gen)) && (mus_formant_p(mus_get_any(gen)))),gen,SCM_ARG1,S_formant);
  if (SCM_NFALSEP(scm_real_p(input))) in1 = gh_scm2double(input); else if (!(SCM_UNBNDP(input))) scm_wrong_type_arg(S_formant,2,input);
  return(gh_double2scm(mus_formant(mus_get_any(gen),in1)));
}

static SCM g_formant_bank(SCM amps, SCM gens, SCM inp)
{
  #define H_formant_bank "(" S_formant_bank " scls gens inputs) -> sum a bank of " S_formant "s: scls[i]*" S_formant "(gens[i],fms[i])"
  return(g_mus_bank1(amps,gens,inp,MUS_FORMANT,S_formant_bank));
}

static SCM g_formant_p(SCM os) 
{
  #define H_formant_p "(" S_formant_p " gen) -> #t if gen is a " S_formant " generator, else #f"
  return(((mus_scm_p(os)) && (mus_formant_p(mus_get_any(os)))) ? SCM_BOOL_T : SCM_BOOL_F);
}

static SCM g_formant_radius (SCM gen)
{
  #define H_mus_formant_radius  "(" S_mus_formant_radius  " gen) -> (" S_formant " generator) gen's pole radius\n\
   (the closer the radius is to 1.0, the narrower the resonance)."
  SCM_ASSERT(((mus_scm_p(gen)) && (mus_formant_p(mus_get_any(gen)))),gen,SCM_ARG1,S_mus_formant_radius);
  return(gh_double2scm(mus_formant_radius(mus_get_any(gen))));
}

static SCM g_set_formant_radius (SCM gen, SCM val)
{
  #define H_mus_set_formant_radius  "(" S_mus_set_formant_radius  " gen val) sets (" S_formant " generator) gen's " S_mus_formant_radius " to val"
  SCM_ASSERT(((mus_scm_p(gen)) && (mus_formant_p(mus_get_any(gen)))),gen,SCM_ARG1,S_mus_formant_radius);
  SCM_ASSERT((SCM_NFALSEP(scm_real_p(val))),val,SCM_ARG2,S_mus_set_formant_radius);
  return(gh_double2scm(mus_set_formant_radius(mus_get_any(gen),gh_scm2double(val))));
}

static void init_formant(void)
{
  DEFINE_PROC(gh_new_procedure2_1(S_formant_bank,g_formant_bank),H_formant_bank);
  DEFINE_PROC(gh_new_procedure1_0(S_formant_p,g_formant_p),H_formant_p);
  DEFINE_PROC(gh_new_procedure(S_make_formant,SCM_FNC g_make_formant,0,6,0),H_make_formant);
  DEFINE_PROC(gh_new_procedure1_1(S_formant,g_formant),H_formant);
  DEFINE_PROC(gh_new_procedure1_0(S_mus_formant_radius,g_formant_radius),H_mus_formant_radius);
  DEFINE_PROC(gh_new_procedure2_0(S_mus_set_formant_radius,g_set_formant_radius),H_mus_set_formant_radius);
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
  #define H_make_frame "(" S_make_frame " chans val0 val1 ...) returns a new frame object\n\
   with chans samples, each sample set from the trailing arguments (defaulting to 0.0):\n\
     (set! fr0 (make-frame 2 .1 .2))"

  /* make_empty_frame from first of arglist, then if more args, load vals */
  mus_scm *gn;
  mus_frame *fr;
#if HAVE_GUILE_1_3_0
  SCM new_frame;
#endif
  SCM cararg;
  int size = 0,i,len;
  SCM_ASSERT((gh_list_p(arglist)),arglist,SCM_ARG1,S_make_frame);
  len = gh_length(arglist);
  if (len == 0) scm_misc_error(S_make_frame,"no arguments?",SCM_EOL);
  cararg = gh_list_ref(arglist,gh_int2scm(0));
  if (!(SCM_NFALSEP(scm_real_p(cararg)))) scm_wrong_type_arg(S_make_frame,1,cararg);
  size = g_scm2int(cararg);
  gn = (mus_scm *)CALLOC(1,sizeof(mus_scm));
  gn->gen = (mus_any *)mus_make_empty_frame(size);
  if (len > 1)
    {
      fr = (mus_frame *)(gn->gen);
      for (i=1;i<len;i++)
	fr->vals[i-1] = gh_scm2double(gh_list_ref(arglist,gh_int2scm(i)));
    }
#if (!HAVE_GUILE_1_3_0)
  SCM_RETURN_NEWSMOB(mus_scm_tag,gn);
#else
  SCM_NEWCELL(new_frame);
  SCM_SETCDR(new_frame,(SCM)gn);
  SCM_SETCAR(new_frame,mus_scm_tag);
  return(new_frame);
#endif
}

static SCM g_frame_p(SCM obj) 
{
  #define H_frame_p "(" S_frame_p " gen) -> #t if gen is a " S_frame " object, else #f"
  return(((mus_scm_p(obj)) && (mus_frame_p(mus_get_any(obj)))) ? SCM_BOOL_T : SCM_BOOL_F);
}

#define DONT_FREE_FRAME -1
#define FREE_FRAME 1

static SCM g_wrap_frame(mus_frame *val, int dealloc)
{
  mus_scm *gn;
#if HAVE_GUILE_1_3_0
  SCM new_frame;
#endif
  gn = (mus_scm *)CALLOC(1,sizeof(mus_scm));
  gn->gen = (mus_any *)val;
  gn->nvcts = dealloc;
#if (!HAVE_GUILE_1_3_0)
  SCM_RETURN_NEWSMOB(mus_scm_tag,gn);
#else
  SCM_NEWCELL(new_frame);
  SCM_SETCDR(new_frame,(SCM)gn);
  SCM_SETCAR(new_frame,mus_scm_tag);
  return(new_frame);
#endif
}

static SCM g_frame_add(SCM uf1, SCM uf2, SCM ures) /* optional res */
{
  #define H_frame_add "(" S_frame_add " f1 f2 &optional outf) adds f1 and f2 returning outf\n\
   if outf is not given, a new frame is created. outf[i] = f1[i] + f2[i]"

  mus_frame *res = NULL;
  SCM_ASSERT(((mus_scm_p(uf1)) && (mus_frame_p(mus_get_any(uf1)))),uf1,SCM_ARG1,S_frame_add);
  SCM_ASSERT(((mus_scm_p(uf2)) && (mus_frame_p(mus_get_any(uf2)))),uf2,SCM_ARG2,S_frame_add);
  if ((mus_scm_p(ures)) && (mus_frame_p(mus_get_any(ures)))) res = (mus_frame *)mus_get_any(ures);
  return(g_wrap_frame(mus_frame_add((mus_frame *)mus_get_any(uf1),(mus_frame *)mus_get_any(uf2),res),(res) ? DONT_FREE_FRAME : FREE_FRAME));
}

static SCM g_frame_multiply(SCM uf1, SCM uf2, SCM ures) /* optional res */
{
  #define H_frame_multiply "(" S_frame_multiply " f1 f2 &optional outf) multiplies f1 and f2 returning outf\n\
   if outf is not given, a new frame is created. outf[i] = f1[i] * f2[i]."

  mus_frame *res = NULL;
  SCM_ASSERT(((mus_scm_p(uf1)) && (mus_frame_p(mus_get_any(uf1)))),uf1,SCM_ARG1,S_frame_multiply);
  SCM_ASSERT(((mus_scm_p(uf2)) && (mus_frame_p(mus_get_any(uf2)))),uf2,SCM_ARG2,S_frame_multiply);
  if ((mus_scm_p(ures)) && (mus_frame_p(mus_get_any(ures)))) res = (mus_frame *)mus_get_any(ures);
  return(g_wrap_frame(mus_frame_multiply((mus_frame *)mus_get_any(uf1),(mus_frame *)mus_get_any(uf2),res),(res) ? DONT_FREE_FRAME : FREE_FRAME));
}

static SCM g_frame_ref(SCM uf1, SCM uchan)
{
  #define H_frame_ref "(" S_frame_ref " f chan) -> f[chan] (the chan-th sample in frame f"
  SCM_ASSERT(((mus_scm_p(uf1)) && (mus_frame_p(mus_get_any(uf1)))),uf1,SCM_ARG1,S_frame_ref);
  SCM_ASSERT((SCM_NFALSEP(scm_real_p(uchan))),uchan,SCM_ARG2,S_frame_ref);
  return(gh_double2scm(mus_frame_ref((mus_frame *)mus_get_any(uf1),g_scm2int(uchan))));
}

static SCM g_set_frame_ref(SCM uf1, SCM uchan, SCM val)
{
  #define H_frame_set "(" S_frame_set " f chan val) sets frame f's chan-th sample to val: f[chan]=val"
  SCM_ASSERT(((mus_scm_p(uf1)) && (mus_frame_p(mus_get_any(uf1)))),uf1,SCM_ARG1,S_frame_set);
  SCM_ASSERT((SCM_NFALSEP(scm_real_p(uchan))),uchan,SCM_ARG2,S_frame_set);
  SCM_ASSERT((SCM_NFALSEP(scm_real_p(val))),val,SCM_ARG3,S_frame_set);
  return(gh_double2scm(mus_frame_set((mus_frame *)mus_get_any(uf1),g_scm2int(uchan),gh_scm2double(val))));
}

static void init_frame(void)
{
  DEFINE_PROC(gh_new_procedure(S_make_frame,SCM_FNC g_make_frame,0,0,1),H_make_frame);
  DEFINE_PROC(gh_new_procedure(S_frame_p,SCM_FNC g_frame_p,1,0,0),H_frame_p);
  DEFINE_PROC(gh_new_procedure(S_frame_add,SCM_FNC g_frame_add,2,1,0),H_frame_add);
  DEFINE_PROC(gh_new_procedure(S_frame_multiply,SCM_FNC g_frame_multiply,2,1,0),H_frame_multiply);
  DEFINE_PROC(gh_new_procedure(S_frame_ref,SCM_FNC g_frame_ref,2,0,0),H_frame_ref);
  DEFINE_PROC(gh_new_procedure(S_frame_set,SCM_FNC g_set_frame_ref,3,0,0),H_frame_set);
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
  return(((mus_scm_p(obj)) && (mus_mixer_p(mus_get_any(obj)))) ? SCM_BOOL_T : SCM_BOOL_F);
}

static SCM g_mixer_ref(SCM uf1, SCM in, SCM out)
{
  #define H_mixer_ref "(" S_mixer_ref " m in out) -> m[in,out], the mixer coefficient at location (in, out)"
  SCM_ASSERT(((mus_scm_p(uf1)) && (mus_mixer_p(mus_get_any(uf1)))),uf1,SCM_ARG1,S_mixer_ref);
  SCM_ASSERT((SCM_NFALSEP(scm_real_p(in))),in,SCM_ARG2,S_mixer_ref);
  SCM_ASSERT((SCM_NFALSEP(scm_real_p(out))),out,SCM_ARG3,S_mixer_ref);
  return(gh_double2scm(mus_mixer_ref((mus_mixer *)mus_get_any(uf1),g_scm2int(in),g_scm2int(out))));
}

static SCM g_set_mixer_ref(SCM uf1, SCM in, SCM out, SCM val)
{
  #define H_mixer_set "(" S_mixer_set " m in out val) sets m[in,out] = val"
  SCM_ASSERT(((mus_scm_p(uf1)) && (mus_mixer_p(mus_get_any(uf1)))),uf1,SCM_ARG1,S_mixer_set);
  SCM_ASSERT((SCM_NFALSEP(scm_real_p(in))),in,SCM_ARG2,S_mixer_set);
  SCM_ASSERT((SCM_NFALSEP(scm_real_p(out))),out,SCM_ARG2,S_mixer_set);
  SCM_ASSERT((SCM_NFALSEP(scm_real_p(val))),val,SCM_ARG4,S_mixer_set);
  return(gh_double2scm(mus_mixer_set((mus_mixer *)mus_get_any(uf1),g_scm2int(in),g_scm2int(out),gh_scm2double(val))));
}

#define DONT_FREE_MIXER -1
#define FREE_MIXER 1

static SCM g_wrap_mixer(mus_mixer *val, int dealloc)
{
  mus_scm *gn;
#if HAVE_GUILE_1_3_0
  SCM new_mixer;
#endif
  gn = (mus_scm *)CALLOC(1,sizeof(mus_scm));
  gn->gen = (mus_any *)val;
  gn->nvcts = dealloc;
#if (!HAVE_GUILE_1_3_0)
  SCM_RETURN_NEWSMOB(mus_scm_tag,gn);
#else
  SCM_NEWCELL(new_mixer);
  SCM_SETCDR(new_mixer,(SCM)gn);
  SCM_SETCAR(new_mixer,mus_scm_tag);
  return(new_mixer);
#endif  
}

static SCM g_mixer_multiply(SCM uf1, SCM uf2, SCM ures) /* optional res */
{
  #define H_mixer_multiply "(" S_mixer_multiply " m1 m2 &optional outm) multiplies mixers m1 and m2\n\
   (a matrix multiply), returning the mixer outm, or creating a new mixer if outm is not given."

  mus_mixer *res = NULL;
  SCM_ASSERT(((mus_scm_p(uf1)) && (mus_mixer_p(mus_get_any(uf1)))),uf1,SCM_ARG1,S_mixer_multiply);
  SCM_ASSERT(((mus_scm_p(uf2)) && (mus_mixer_p(mus_get_any(uf2)))),uf2,SCM_ARG2,S_mixer_multiply);
  if ((mus_scm_p(ures)) && (mus_mixer_p(mus_get_any(ures)))) res = (mus_mixer *)mus_get_any(ures);
  return(g_wrap_mixer(mus_mixer_multiply((mus_mixer *)mus_get_any(uf1),(mus_mixer *)mus_get_any(uf2),res),(res) ? DONT_FREE_MIXER : FREE_MIXER));
}

static SCM g_frame2frame(SCM mx, SCM infr, SCM outfr) /* optional outfr */
{
  #define H_frame2frame "(" S_frame2frame " m f &optional outf) passes frame f through mixer m\n\
    returning frame outf (or creating a new frame if necessary); this is a matrix multiply of m and f"

  mus_frame *res = NULL;
  SCM_ASSERT(((mus_scm_p(mx)) && (mus_mixer_p(mus_get_any(mx)))),mx,SCM_ARG1,S_frame2frame);
  SCM_ASSERT(((mus_scm_p(infr)) && (mus_frame_p(mus_get_any(infr)))),infr,SCM_ARG2,S_frame2frame);
  if ((mus_scm_p(outfr)) && (mus_frame_p(mus_get_any(outfr)))) res = (mus_frame *)mus_get_any(outfr);
  return(g_wrap_frame(mus_frame2frame((mus_mixer *)mus_get_any(mx),(mus_frame *)mus_get_any(infr),res),(res) ? DONT_FREE_FRAME : FREE_FRAME));
}

static SCM g_frame2list(SCM fr)
{
  #define H_frame2list "(" S_frame2list " f) -> contents of frame f as a list"
  mus_frame *val;
  int i;
  SCM res = SCM_EOL;
  SCM_ASSERT(((mus_scm_p(fr)) && (mus_frame_p(mus_get_any(fr)))),fr,SCM_ARG1,S_frame2list);
  val = (mus_frame *)mus_get_any(fr);
  for (i=(val->chans)-1;i>=0;i--) res = scm_cons(gh_double2scm(val->vals[i]),res);
  return(res);
}

static SCM g_frame2sample(SCM mx, SCM fr)
{
  #define H_frame2sample "(" S_frame2sample " m f) -> pass frame f through mixer (or frame) m to produce a sample"
  SCM_ASSERT((mus_scm_p(mx)),mx,SCM_ARG1,S_frame2sample);
  SCM_ASSERT(((mus_scm_p(fr)) && (mus_frame_p(mus_get_any(fr)))),fr,SCM_ARG2,S_frame2sample);
  return(gh_double2scm(mus_frame2sample(mus_get_any(mx),(mus_frame *)mus_get_any(fr))));
}

static SCM g_sample2frame(SCM mx, SCM insp, SCM outfr) /* optional outfr */
{
  #define H_sample2frame "(" S_sample2frame " m val &optional outf) passes the sample val through mixer m\n\
   returning frame outf (creating it if necessary)"

  mus_frame *res = NULL;
  SCM_ASSERT((mus_scm_p(mx)),mx,SCM_ARG1,S_sample2frame);
  SCM_ASSERT((SCM_NFALSEP(scm_real_p(insp))),insp,SCM_ARG2,S_sample2frame);
  if ((mus_scm_p(outfr)) && (mus_frame_p(mus_get_any(outfr)))) res = (mus_frame *)mus_get_any(outfr);
  return(g_wrap_frame(mus_sample2frame(mus_get_any(mx),gh_scm2double(insp),res),(res) ? DONT_FREE_FRAME : FREE_FRAME));
}

static SCM g_make_mixer(SCM arglist)
{
  #define H_make_mixer "(" S_make_mixer " chans val0 val1 ...) makes a new mixer object\n\
   with chans inputs and outputs, initializing the scalers from the rest of the arguments:\n\
      (set! gen (make-mixer 2 .5 .25 .125 1.0))"

  /* make_empty_mixer from first of arglist, then if more args, load vals */
  mus_scm *gn;
  mus_mixer *fr;
#if HAVE_GUILE_1_3_0
  SCM new_mixer;
#endif
  SCM cararg;
  int size = 0,i,j,k,len;
  SCM_ASSERT((gh_list_p(arglist)),arglist,SCM_ARG1,S_make_mixer);
  len = gh_length(arglist);
  if (len == 0) scm_misc_error(S_make_mixer,"no arguments?",SCM_EOL);
  cararg = gh_list_ref(arglist,gh_int2scm(0));
  if (!(SCM_NFALSEP(scm_real_p(cararg)))) scm_wrong_type_arg(S_make_mixer,1,cararg);
  size = g_scm2int(cararg);
  gn = (mus_scm *)CALLOC(1,sizeof(mus_scm));
  gn->gen = (mus_any *)mus_make_empty_mixer(size);
  if (len > 1)
    {
      fr = (mus_mixer *)(gn->gen);
      j = 0;
      k = 0;
      for (i=1;i<len;i++)
	{
	  fr->vals[j][k] = gh_scm2double(gh_list_ref(arglist,gh_int2scm(i)));
	  k++;
	  if (k == size)
	    {
	      k = 0;
	      j++;
	    }
	}
    }
#if (!HAVE_GUILE_1_3_0)
  SCM_RETURN_NEWSMOB(mus_scm_tag,gn);
#else
  SCM_NEWCELL(new_mixer);
  SCM_SETCDR(new_mixer,(SCM)gn);
  SCM_SETCAR(new_mixer,mus_scm_tag);
  return(new_mixer);
#endif
}

static void init_mixer(void)
{
  DEFINE_PROC(gh_new_procedure(S_make_mixer,SCM_FNC g_make_mixer,0,0,1),H_make_mixer);
  DEFINE_PROC(gh_new_procedure(S_mixer_p,SCM_FNC g_mixer_p,1,0,0),H_mixer_p);
  DEFINE_PROC(gh_new_procedure(S_mixer_multiply,SCM_FNC g_mixer_multiply,2,1,0),H_mixer_multiply);
  DEFINE_PROC(gh_new_procedure(S_mixer_ref,SCM_FNC g_mixer_ref,3,0,0),H_mixer_ref);
  DEFINE_PROC(gh_new_procedure(S_mixer_set,SCM_FNC g_set_mixer_ref,4,0,0),H_mixer_set);
  DEFINE_PROC(gh_new_procedure(S_frame2sample,SCM_FNC g_frame2sample,2,0,0),H_frame2sample);
  DEFINE_PROC(gh_new_procedure(S_frame2list,SCM_FNC g_frame2list,1,0,0),H_frame2list);
  DEFINE_PROC(gh_new_procedure(S_frame2frame,SCM_FNC g_frame2frame,2,1,0),H_frame2frame);
  DEFINE_PROC(gh_new_procedure(S_sample2frame,SCM_FNC g_sample2frame,2,1,0),H_sample2frame);
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
  return(((mus_scm_p(obj)) && (mus_buffer_p(mus_get_any(obj)))) ? SCM_BOOL_T : SCM_BOOL_F);
}

static SCM g_make_buffer(SCM arg1, SCM arg2, SCM arg3, SCM arg4)
{
  #define H_make_buffer "(" S_make_buffer " &opt-key (size 512) fill-time) returns a new buffer\n\
   generator, a FIFO for samples. The size argument sets the size of the buffer (not a delay time)\n\
   and fill-time sets the time to the next request for more samples.  The intended use is in block\n\
   processing normally involving overlap-adds."

  SCM new_rblk;
  mus_scm *gn;
  SCM args[4],keys[2];
  int orig_arg[2] = {0,0};
  int vals;
  Float *buf;
  int siz = DEFAULT_TABLE_SIZE;
  Float filltime = 0.0;
  keys[0] = all_keys[C_size];
  keys[1] = all_keys[C_fill_time];
  args[0] = arg1; args[1] = arg2; args[2] = arg3; args[3] = arg4;
  vals = decode_keywords(S_make_buffer,3,keys,6,args,orig_arg);
  if (vals > 0)
    {
      siz = ikeyarg(keys[0],S_make_buffer,orig_arg[0]+1,args[orig_arg[0]],siz);
      filltime = fkeyarg(keys[1],S_make_buffer,orig_arg[1]+1,args[orig_arg[1]],0.0);
    }
  buf = (Float *)CALLOC(siz,sizeof(Float));
  gn = (mus_scm *)CALLOC(1,sizeof(mus_scm));
  gn->vcts = (SCM *)CALLOC(1,sizeof(SCM));
  gn->vcts = (SCM *)CALLOC(1,sizeof(SCM));
  gn->vcts[0] = SCM_EOL;
  gn->nvcts = 1;
  gn->gen = mus_make_buffer(buf,siz,filltime);
#if (!HAVE_GUILE_1_3_0)
  SCM_NEWSMOB(new_rblk,mus_scm_tag,gn);
#else
  SCM_NEWCELL(new_rblk);
  SCM_SETCDR(new_rblk,(SCM)gn);
  SCM_SETCAR(new_rblk,mus_scm_tag);
#endif
  gn->vcts[0] = make_vct(siz,buf);
  return(new_rblk);
}

static SCM g_buffer2sample(SCM obj)
{
  #define H_buffer2sample "(" S_buffer2sample " gen) -> next sample in buffer, removing it"
  SCM_ASSERT(((mus_scm_p(obj)) && (mus_buffer_p(mus_get_any(obj)))),obj,SCM_ARG1,S_buffer2sample);
  return(gh_double2scm(mus_buffer2sample(mus_get_any(obj))));
}

static SCM g_buffer2frame(SCM obj, SCM fr)
{
  #define H_buffer2frame "(" S_buffer2frame " gen) -> next frame of samples in buffer, removing them"
  SCM_ASSERT(((mus_scm_p(obj)) && (mus_buffer_p(mus_get_any(obj)))),obj,SCM_ARG1,S_buffer2frame);
  SCM_ASSERT(((mus_scm_p(fr)) && (mus_frame_p(mus_get_any(fr)))),fr,SCM_ARG2,S_buffer2frame);
  return(g_wrap_frame((mus_frame *)mus_buffer2frame(mus_get_any(obj),mus_get_any(fr)),DONT_FREE_FRAME));
}

static SCM g_buffer_empty_p(SCM obj)
{
  #define H_buffer_empty_p "(" S_buffer_empty_p " gen) -> #t if buffer is in need of more samples"
  SCM_ASSERT(((mus_scm_p(obj)) && (mus_buffer_p(mus_get_any(obj)))),obj,SCM_ARG1,S_buffer_empty_p);
  return(gh_int2scm(mus_buffer_empty_p(mus_get_any(obj))));
}

static SCM g_buffer_full_p(SCM obj)
{
  #define H_buffer_full_p "(" S_buffer_full_p " gen) -> #t if buffer has no room for any more samples"
  SCM_ASSERT(((mus_scm_p(obj)) && (mus_buffer_p(mus_get_any(obj)))),obj,SCM_ARG1,S_buffer_full_p);
  return(gh_int2scm(mus_buffer_full_p(mus_get_any(obj))));
}

static SCM g_sample2buffer(SCM obj, SCM val)
{
  #define H_sample2buffer "(" S_sample2buffer " gen val) append val to current end of data in buffer"
  SCM_ASSERT(((mus_scm_p(obj)) && (mus_buffer_p(mus_get_any(obj)))),obj,SCM_ARG1,S_sample2buffer);
  SCM_ASSERT((SCM_NFALSEP(scm_real_p(val))),val,SCM_ARG1,S_sample2buffer);
  return(gh_double2scm(mus_sample2buffer(mus_get_any(obj),gh_scm2double(val))));
}

static SCM g_frame2buffer(SCM obj, SCM val)
{
  #define H_frame2buffer "(" S_frame2buffer " gen f) appends sample in frame f to end of data in buffer"
  SCM_ASSERT(((mus_scm_p(obj)) && (mus_buffer_p(mus_get_any(obj)))),obj,SCM_ARG1,S_frame2buffer);
  SCM_ASSERT(((mus_scm_p(val)) && (mus_frame_p(mus_get_any(val)))),val,SCM_ARG2,S_frame2buffer);
  return(g_wrap_frame((mus_frame *)mus_frame2buffer(mus_get_any(obj),mus_get_any(val)),DONT_FREE_FRAME));
}

static void init_rblk(void)
{
  DEFINE_PROC(gh_new_procedure(S_make_buffer,SCM_FNC g_make_buffer,0,4,0),H_make_buffer);
  DEFINE_PROC(gh_new_procedure(S_buffer_p,SCM_FNC g_buffer_p,1,0,0),H_buffer_p);
  DEFINE_PROC(gh_new_procedure(S_buffer_empty_p,SCM_FNC g_buffer_empty_p,1,0,0),H_buffer_empty_p);
  DEFINE_PROC(gh_new_procedure(S_buffer_full_p,SCM_FNC g_buffer_full_p,1,0,0),H_buffer_full_p);
  DEFINE_PROC(gh_new_procedure(S_buffer2sample,SCM_FNC g_buffer2sample,1,0,0),H_buffer2sample);
  DEFINE_PROC(gh_new_procedure(S_buffer2frame,SCM_FNC g_buffer2frame,2,0,0),H_buffer2frame);
  DEFINE_PROC(gh_new_procedure(S_sample2buffer,SCM_FNC g_sample2buffer,2,0,0),H_sample2buffer);
  DEFINE_PROC(gh_new_procedure(S_frame2buffer,SCM_FNC g_frame2buffer,2,0,0),H_frame2buffer);
}


/* ---------------- wave-train ---------------- */

#define S_make_wave_train "make-wave-train"
#define S_wave_train "wave-train"
#define S_wave_train_p "wave-train?"

static SCM g_make_wave_train(SCM arg1, SCM arg2, SCM arg3, SCM arg4, SCM arg5, SCM arg6)
{
  #define H_make_wave_train "(" S_make_wave_train " &opt-key (frequency 440.0) (initial-phase 0.0) wave)\n\
   returns a new wave-train generator (an extension of pulse-train).   Frequency is\n\
   the repetition rate of the wave found in wave. Successive waves can overlap."

  SCM new_wt,gwave = SCM_UNDEFINED;
  mus_scm *gn;
  SCM args[6],keys[3];
  int orig_arg[3] = {0,0,0};
  int vals,wsize;
  vct *v;
  Float freq = 440.0;
  Float phase = 0.0;
  Float *wave = NULL;
  wsize = DEFAULT_TABLE_SIZE;
  keys[0] = all_keys[C_frequency];
  keys[1] = all_keys[C_initial_phase];
  keys[2] = all_keys[C_wave];
  args[0] = arg1; args[1] = arg2; args[2] = arg3; args[3] = arg4; args[4] = arg5; args[5] = arg6; 
  vals = decode_keywords(S_make_wave_train,3,keys,6,args,orig_arg);
  if (vals > 0)
    {
      freq = fkeyarg(keys[0],S_make_wave_train,orig_arg[0]+1,args[orig_arg[0]],freq);
      phase = fkeyarg(keys[1],S_make_wave_train,orig_arg[1]+1,args[orig_arg[1]],phase);
      if (!(keyword_p(keys[2])))
        {
	  if (vct_p(keys[2]))
	    {
	      gwave = keys[2];
	      v = get_vct(gwave);
	      wave = v->data;
	      wsize = v->length;
	    }
          else scm_wrong_type_arg(S_make_wave_train,orig_arg[2]+1,args[orig_arg[2]]);
        }
    }
  if (wave == NULL) wave = (Float *)CALLOC(wsize,sizeof(Float));
  gn = (mus_scm *)CALLOC(1,sizeof(mus_scm));
  gn->vcts = (SCM *)CALLOC(1,sizeof(SCM));
  gn->vcts[0] = SCM_EOL;
  gn->nvcts = 1;
  gn->gen = mus_make_wave_train(freq,phase,wave,wsize);
#if (!HAVE_GUILE_1_3_0)
  SCM_NEWSMOB(new_wt,mus_scm_tag,gn);
#else
  SCM_NEWCELL(new_wt);
  SCM_SETCDR(new_wt,(SCM)gn);
  SCM_SETCAR(new_wt,mus_scm_tag);
#endif
  if (SCM_UNBNDP(gwave)) gwave = make_vct(wsize,wave);
  gn->vcts[0] = gwave;
  return(new_wt);
}

static SCM g_wave_train(SCM obj, SCM fm)
{
  #define H_wave_train "(" S_wave_train " gen &optional (fm 0.0)) -> next sample of wave-train"
  Float fm1 = 0.0;
  SCM_ASSERT(((mus_scm_p(obj)) && (mus_wave_train_p(mus_get_any(obj)))),obj,SCM_ARG1,S_wave_train);
  if (SCM_NFALSEP(scm_real_p(fm))) fm1 = gh_scm2double(fm); else if (!(SCM_UNBNDP(fm))) scm_wrong_type_arg(S_wave_train,2,fm);
  return(gh_double2scm(mus_wave_train(mus_get_any(obj),fm1)));
}

static SCM g_wave_train_p(SCM obj) 
{
  #define H_wave_train_p "(" S_wave_train_p " gen) -> #t if gen is a " S_wave_train " generator, else #f"
  return(((mus_scm_p(obj)) && (mus_wave_train_p(mus_get_any(obj)))) ? SCM_BOOL_T : SCM_BOOL_F);
}

static void init_wt(void)
{
  DEFINE_PROC(gh_new_procedure(S_make_wave_train,SCM_FNC g_make_wave_train,0,6,0),H_make_wave_train);
  DEFINE_PROC(gh_new_procedure(S_wave_train,SCM_FNC g_wave_train,1,1,0),H_wave_train);
  DEFINE_PROC(gh_new_procedure(S_wave_train_p,SCM_FNC g_wave_train_p,1,0,0),H_wave_train_p);
}


/* ---------------- waveshape ---------------- */

#define S_make_waveshape           "make-waveshape"
#define S_waveshape                "waveshape"
#define S_waveshape_p              "waveshape?"
#define S_partials2waveshape       "partials->waveshape"
#define S_partials2polynomial      "partials->polynomial"

static Float *list2partials(SCM harms, int *npartials)
{
  int listlen,i,maxpartial,curpartial;
  Float *partials;
  listlen = gh_length(harms);
  /* the list is '(partial-number partial-amp ... ) */
  maxpartial = g_scm2int(gh_list_ref(harms,gh_int2scm(0)));
  for (i=2;i<listlen;i+=2)
    {
      curpartial = g_scm2int(gh_list_ref(harms,gh_int2scm(i)));
      if (curpartial > maxpartial) maxpartial = curpartial;
    }
  partials = (Float *)CALLOC(maxpartial+1,sizeof(Float));
  (*npartials) = maxpartial+1;
  for (i=0;i<listlen;i+=2)
    {
      curpartial = g_scm2int(gh_list_ref(harms,gh_int2scm(i)));
      partials[curpartial] = gh_scm2double(gh_list_ref(harms,gh_int2scm(i+1)));
    }
  return(partials);
}

static SCM g_make_waveshape(SCM arg1, SCM arg2, SCM arg3, SCM arg4, SCM arg5, SCM arg6, SCM arg7, SCM arg8)
{
  #define H_make_waveshape "(" S_make_waveshape " &opt-key (frequency 440.0) (partials '(1 1)) (size 512) wave)\n\
   returns a new waveshaping generator (essentially table-lookup driven by a sinewave)\n\
      (make-waveshape :wave (partials->waveshape '(1 1.0)))\n\
   is basically the same as make-oscil"

  SCM new_wt,gwave = SCM_UNDEFINED;
  mus_scm *gn;
  SCM args[8],keys[4];
  int orig_arg[4] = {0,0,0,0};
  int vals,wsize,npartials = 0,partials_allocated = 0;
  vct *v;
  Float freq = 440.0;
  Float *wave = NULL,*partials = NULL;
  wsize = DEFAULT_TABLE_SIZE;
  keys[0] = all_keys[C_frequency];
  keys[1] = all_keys[C_partials];
  keys[2] = all_keys[C_size];
  keys[3] = all_keys[C_wave];
  args[0] = arg1; args[1] = arg2; args[2] = arg3; args[3] = arg4; args[4] = arg5; args[5] = arg6; args[6] = arg7; args[7] = arg8;
  vals = decode_keywords(S_make_waveshape,4,keys,8,args,orig_arg);
  if (vals > 0)
    {
      freq = fkeyarg(keys[0],S_make_waveshape,orig_arg[0]+1,args[orig_arg[0]],freq);
      if (!(keyword_p(keys[1])))
        {
	  if (gh_list_p(keys[1]))
	    {
	      partials = list2partials(keys[1],&npartials);
	      partials_allocated = 1;
	    }
          else scm_wrong_type_arg(S_make_waveshape,orig_arg[1]+1,args[orig_arg[1]]);
        }
      wsize = ikeyarg(keys[2],S_make_waveshape,orig_arg[2]+1,args[orig_arg[2]],wsize);
      if (!(keyword_p(keys[3])))
        {
	  if (vct_p(keys[3]))
	    {
	      gwave = keys[3];
	      v = get_vct(gwave);
	      wave = v->data;
	      wsize = v->length;
	    }
          else scm_wrong_type_arg(S_make_waveshape,orig_arg[3]+1,args[orig_arg[3]]);
        }
    }
  if (wave == NULL) 
    {
      if (partials == NULL) return(SCM_BOOL_F);
      wave = mus_partials2waveshape(npartials,partials,wsize,(Float *)CALLOC(wsize,sizeof(Float)));
    }
  if (partials_allocated) {FREE(partials); partials = NULL;}
  gn = (mus_scm *)CALLOC(1,sizeof(mus_scm));
  gn->vcts = (SCM *)CALLOC(1,sizeof(SCM));
  gn->vcts[0] = SCM_EOL;
  gn->nvcts = 1;
  gn->gen = mus_make_waveshape(freq,0.0,wave,wsize);
#if (!HAVE_GUILE_1_3_0)
  SCM_NEWSMOB(new_wt,mus_scm_tag,gn);
#else
  SCM_NEWCELL(new_wt);
  SCM_SETCDR(new_wt,(SCM)gn);
  SCM_SETCAR(new_wt,mus_scm_tag);
#endif
  if (SCM_UNBNDP(gwave)) gwave = make_vct(wsize,wave);
  gn->vcts[0] = gwave;
  return(new_wt);
}

static SCM g_waveshape(SCM obj, SCM index, SCM fm)
{
  #define H_waveshape "(" S_waveshape " gen &optional (index 1.0) (fm 0.0)) -> next sample of waveshaper"
  Float fm1 = 0.0, index1 = 1.0;
  SCM_ASSERT(((mus_scm_p(obj)) && (mus_waveshape_p(mus_get_any(obj)))),obj,SCM_ARG1,S_waveshape);
  if (SCM_NFALSEP(scm_real_p(index))) index1 = gh_scm2double(index); else if (!(SCM_UNBNDP(index))) scm_wrong_type_arg(S_waveshape,2,index);
  if (SCM_NFALSEP(scm_real_p(fm))) fm1 = gh_scm2double(fm); else if (!(SCM_UNBNDP(fm))) scm_wrong_type_arg(S_waveshape,3,fm);
  return(gh_double2scm(mus_waveshape(mus_get_any(obj),index1,fm1)));
}

static SCM g_waveshape_p(SCM obj) 
{
  #define H_waveshape_p "(" S_waveshape_p " gen) -> #t if gen is a " S_waveshape " generator, else #f"
  return(((mus_scm_p(obj)) && (mus_waveshape_p(mus_get_any(obj)))) ? SCM_BOOL_T : SCM_BOOL_F);
}

static SCM g_partials2waveshape(SCM amps, SCM s_size)
{
  #define H_partials2waveshape "(" S_partials2waveshape " partials &optional (size 512))\n\
   produces a waveshaping lookup table (suitable for the " S_waveshape " generator)\n\
   that will produce the harmonic spectrum given by the partials argument"

  int npartials,size;
  Float *partials,*wave;
  SCM gwave;
  if (SCM_NFALSEP(scm_real_p(s_size)))
    size = g_scm2int(s_size);
  else size = DEFAULT_TABLE_SIZE;
  partials = list2partials(amps,&npartials);
  wave = mus_partials2waveshape(npartials,partials,size,(Float *)CALLOC(size,sizeof(Float)));
  gwave = make_vct(size,wave);
  FREE(partials);
  return(gwave);
}

static SCM g_partials2polynomial(SCM amps, SCM ukind)
{
  #define H_partials2polynomial "(" S_partials2polynomial " partials &optional (kind 1))\n\
   produces a Chebychev polynomial suitable for use with the " S_polynomial " generator\n\
   to create (via waveshaping) the harmonic spectrum described by the partials argument:\n\
     (let ((v0 (partials->polynomial '(1 1 2 1)))\n\
           (os (make-oscil)))\n\
       (polynomial v0 (oscil os)))"

  int npartials,kind;
  Float *partials,*wave;
  if (SCM_NFALSEP(scm_real_p(ukind)))
    kind = g_scm2int(ukind);
  else kind = 1;
  partials = list2partials(amps,&npartials);
  wave = mus_partials2polynomial(npartials,partials,kind);
  return(make_vct(npartials,wave));
}

static void init_ws(void)
{
  DEFINE_PROC(gh_new_procedure(S_make_waveshape,SCM_FNC g_make_waveshape,0,8,0),H_make_waveshape);
  DEFINE_PROC(gh_new_procedure(S_waveshape,SCM_FNC g_waveshape,1,2,0),H_waveshape);
  DEFINE_PROC(gh_new_procedure(S_waveshape_p,SCM_FNC g_waveshape_p,1,0,0),H_waveshape_p);
  DEFINE_PROC(gh_new_procedure(S_partials2waveshape,SCM_FNC g_partials2waveshape,1,1,0),H_partials2waveshape);
  DEFINE_PROC(gh_new_procedure(S_partials2polynomial,SCM_FNC g_partials2polynomial,1,1,0),H_partials2polynomial);
}


/* ---------------- sine-summation ---------------- */

#define S_make_sine_summation "make-sine-summation"
#define S_sine_summation      "sine-summation"
#define S_sine_summation_p    "sine-summation?"

static SCM g_sine_summation_p(SCM obj) 
{
  #define H_sine_summation_p "(" S_sine_summation_p " gen) -> #t if gen is a " S_sine_summation " generator, else #f"
  return(((mus_scm_p(obj)) && (mus_sine_summation_p(mus_get_any(obj)))) ? SCM_BOOL_T : SCM_BOOL_F);
}

static SCM g_sine_summation(SCM obj, SCM fm)
{
  #define H_sine_summation "(" S_sine_summation " gen &optional (fm 0.0)) -> next sample of sine summation generator"
  Float fm1 = 0.0;
  SCM_ASSERT(((mus_scm_p(obj)) && (mus_sine_summation_p(mus_get_any(obj)))),obj,SCM_ARG1,S_sine_summation);
  if (SCM_NFALSEP(scm_real_p(fm))) fm1 = gh_scm2double(fm); else if (!(SCM_UNBNDP(fm))) scm_wrong_type_arg(S_sine_summation,2,fm);
  return(gh_double2scm(mus_sine_summation(mus_get_any(obj),fm1)));
}

static SCM g_make_sine_summation(SCM arglist)
{
  #define H_make_sine_summation "(" S_make_sine_summation " &opt-key (frequency 440.0) (initial-phase 0.0) (n 1) (a 0.5) (ratio 1.0)\n\
   returns a new sine summation synthesis generator."

#if HAVE_GUILE_1_3_0
  SCM new_sss;
#endif
  mus_scm *gn;
  SCM args[10],keys[5];
  int orig_arg[5] = {0,0,0,0,0};
  int vals,i,arglist_len;
  Float freq = 440.0,phase = 0.0,a=.5,ratio=1.0;
  int n=1;
  keys[0] = all_keys[C_frequency];
  keys[1] = all_keys[C_initial_phase];
  keys[2] = all_keys[C_n];
  keys[3] = all_keys[C_a];
  keys[4] = all_keys[C_ratio];
  for (i=0;i<10;i++) args[i] = SCM_UNDEFINED;
  arglist_len = gh_length(arglist);
  for (i=0;i<arglist_len;i++) args[i] = gh_list_ref(arglist,gh_int2scm(i));
  vals = decode_keywords(S_make_sine_summation,5,keys,10,args,orig_arg);
  if (vals > 0)
    {
      freq = fkeyarg(keys[0],S_make_sine_summation,orig_arg[0]+1,args[orig_arg[0]],freq);
      phase = fkeyarg(keys[1],S_make_sine_summation,orig_arg[1]+1,args[orig_arg[1]],phase);
      n = ikeyarg(keys[2],S_make_sine_summation,orig_arg[2]+1,args[orig_arg[2]],n);
      a = fkeyarg(keys[3],S_make_sine_summation,orig_arg[3]+1,args[orig_arg[3]],a);
      ratio = fkeyarg(keys[4],S_make_sine_summation,orig_arg[4]+1,args[orig_arg[4]],ratio);
    }
  gn = (mus_scm *)CALLOC(1,sizeof(mus_scm));
  gn->gen = mus_make_sine_summation(freq,phase,n,a,ratio);
#if (!HAVE_GUILE_1_3_0)
  SCM_RETURN_NEWSMOB(mus_scm_tag,gn);
#else
  SCM_NEWCELL(new_sss);
  SCM_SETCDR(new_sss,(SCM)gn);
  SCM_SETCAR(new_sss,mus_scm_tag);
  return(new_sss);
#endif
}

static void init_sss(void)
{
  DEFINE_PROC(gh_new_procedure(S_make_sine_summation,SCM_FNC g_make_sine_summation,0,0,1),H_make_sine_summation);
  DEFINE_PROC(gh_new_procedure(S_sine_summation,SCM_FNC g_sine_summation,1,1,0),H_sine_summation);
  DEFINE_PROC(gh_new_procedure(S_sine_summation_p,SCM_FNC g_sine_summation_p,1,0,0),H_sine_summation_p);
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
  return(((mus_scm_p(obj)) && (mus_filter_p(mus_get_any(obj)))) ? SCM_BOOL_T : SCM_BOOL_F);
}

static SCM g_fir_filter_p(SCM obj) 
{
  #define H_fir_filter_p "(" S_fir_filter_p " gen) -> #t if gen is a " S_fir_filter " generator, else #f"
  return(((mus_scm_p(obj)) && (mus_fir_filter_p(mus_get_any(obj)))) ? SCM_BOOL_T : SCM_BOOL_F);
}

static SCM g_iir_filter_p(SCM obj) 
{
  #define H_iir_filter_p "(" S_iir_filter_p " gen) -> #t if gen is a " S_iir_filter " generator, else #f"
  return(((mus_scm_p(obj)) && (mus_iir_filter_p(mus_get_any(obj)))) ? SCM_BOOL_T : SCM_BOOL_F);
}

static SCM g_filter(SCM obj, SCM input)
{
  #define H_filter "(" S_filter " gen &optional (input 0.0)) -> next sample from FIR/IIR filter"
  SCM_ASSERT(((mus_scm_p(obj)) && (mus_filter_p(mus_get_any(obj)))),obj,SCM_ARG1,S_filter);
  SCM_ASSERT((SCM_NFALSEP(scm_real_p(input))),input,SCM_ARG2,S_filter);
  return(gh_double2scm(mus_filter(mus_get_any(obj),gh_scm2double(input))));
}

static SCM g_fir_filter(SCM obj, SCM input)
{
  #define H_fir_filter "(" S_fir_filter " gen &optional (input 0.0)) -> next sample from FIR filter"
  SCM_ASSERT(((mus_scm_p(obj)) && (mus_fir_filter_p(mus_get_any(obj)))),obj,SCM_ARG1,S_fir_filter);
  SCM_ASSERT((SCM_NFALSEP(scm_real_p(input))),input,SCM_ARG2,S_fir_filter);
  return(gh_double2scm(mus_fir_filter(mus_get_any(obj),gh_scm2double(input))));
}

static SCM g_iir_filter(SCM obj, SCM input)
{
  #define H_iir_filter "(" S_iir_filter " gen &optional (input 0.0)) -> next sample from IIR filter"
  SCM_ASSERT(((mus_scm_p(obj)) && (mus_iir_filter_p(mus_get_any(obj)))),obj,SCM_ARG1,S_iir_filter);
  SCM_ASSERT((SCM_NFALSEP(scm_real_p(input))),input,SCM_ARG2,S_iir_filter);
  return(gh_double2scm(mus_iir_filter(mus_get_any(obj),gh_scm2double(input))));
}

enum {G_FILTER,G_FIR_FILTER,G_IIR_FILTER};

static SCM g_make_filter_1(int choice, SCM arg1, SCM arg2, SCM arg3, SCM arg4, SCM arg5, SCM arg6)
{
#if HAVE_GUILE_1_3_0
  SCM new_flt;
#endif
  SCM xwave=SCM_UNDEFINED,ywave=SCM_UNDEFINED;
  mus_scm *gn;
  SCM args[6],keys[3];
  int orig_arg[3] = {0,0,0};
  vct *x = NULL,*y = NULL;
  int nkeys,vals,order=0;
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
  vals = decode_keywords(caller,nkeys,keys,nkeys*2,args,orig_arg);
  if (vals > 0)
    {
      order = ikeyarg(keys[0],caller,orig_arg[0]+1,args[orig_arg[0]],0);
      if (!(keyword_p(keys[1])))
        {
	  if (vct_p(keys[1]))
	    {
	      xwave = keys[1];
	      x = get_vct(xwave);
	    }
          else scm_wrong_type_arg(caller,orig_arg[1]+1,args[orig_arg[1]]);
        }
      if (nkeys > 2)
	if (!(keyword_p(keys[2])))
	  {
	    if (vct_p(keys[2]))
	      {
		ywave = keys[2];
		y = get_vct(ywave);
	      }
	    else scm_wrong_type_arg(caller,orig_arg[2]+1,args[orig_arg[2]]);
	  }
    }
  gn = (mus_scm *)CALLOC(1,sizeof(mus_scm));
  gn->vcts = (SCM *)CALLOC(nkeys-1,sizeof(SCM));
  gn->vcts[0] = SCM_EOL;
  if (nkeys > 2) gn->vcts[1] = SCM_EOL;
  gn->nvcts = nkeys-1;
  switch (choice)
    {
    case G_FILTER: gn->gen = mus_make_filter(order,x->data,y->data,NULL); break;
    case G_FIR_FILTER: gn->gen = mus_make_fir_filter(order,x->data,NULL); break;
    case G_IIR_FILTER: gn->gen = mus_make_iir_filter(order,x->data,NULL); break;
    }
  gn->vcts[0] = xwave;
  if (nkeys > 2) gn->vcts[1] = ywave;
#if (!HAVE_GUILE_1_3_0)
  SCM_RETURN_NEWSMOB(mus_scm_tag,gn);
#else
  SCM_NEWCELL(new_flt);
  SCM_SETCDR(new_flt,(SCM)gn);
  SCM_SETCAR(new_flt,mus_scm_tag);
  return(new_flt);
#endif
}

static SCM g_make_filter(SCM arg1, SCM arg2, SCM arg3, SCM arg4, SCM arg5, SCM arg6)
{
  #define H_make_filter "(" S_make_filter " &opt-key order xcoeffs ycoeffs) returns a new direct form FIR/IIR filter"
  return(g_make_filter_1(G_FILTER,arg1,arg2,arg3,arg4,arg5,arg6));
}

static SCM g_make_fir_filter(SCM arg1, SCM arg2, SCM arg3, SCM arg4)
{
  #define H_make_fir_filter "(" S_make_fir_filter " &opt-key order xcoeffs) returns a new FIR filter"
  return(g_make_filter_1(G_FIR_FILTER,arg1,arg2,arg3,arg4,SCM_UNDEFINED,SCM_UNDEFINED));
}

static SCM g_make_iir_filter(SCM arg1, SCM arg2, SCM arg3, SCM arg4)
{
  #define H_make_iir_filter "(" S_make_iir_filter " &opt-key order ycoeffs) returns a new IIR filter"
  return(g_make_filter_1(G_IIR_FILTER,arg1,arg2,arg3,arg4,SCM_UNDEFINED,SCM_UNDEFINED));
}

static SCM g_mus_order(SCM obj)
{
  #define H_mus_order "(" S_mus_order " gen) -> gen's filter order"
  SCM_ASSERT((mus_scm_p(obj)),obj,SCM_ARG1,S_mus_order);
  return(gh_int2scm(mus_order(mus_get_any(obj))));
}

static SCM g_mus_xcoeffs(SCM gen) 
{
  #define H_mus_xcoeffs "(" S_mus_xcoeffs " gen) -> gen's filter xcoeffs (vct of coefficients on inputs)"
  mus_scm *ms;
  SCM_ASSERT((mus_scm_p(gen)),gen,SCM_ARG1,S_mus_data);
  ms = mus_get_scm(gen);
  if (ms->vcts)
    return(ms->vcts[0]); 
  else return(SCM_BOOL_F);
}

static SCM g_mus_ycoeffs(SCM gen) 
{
  #define H_mus_ycoeffs "(" S_mus_ycoeffs " gen) -> gen's filter ycoeffs (vct of coefficients on outputs)"
  mus_scm *ms;
  SCM_ASSERT((mus_scm_p(gen)),gen,SCM_ARG1,S_mus_data);
  ms = mus_get_scm(gen);
  if (ms->vcts)
    {
      if (mus_iir_filter_p(mus_get_any(gen)))  
	return(ms->vcts[0]);
      else return(ms->vcts[1]); 
    }
  else return(SCM_BOOL_F);
}

static void init_flt(void)
{
  DEFINE_PROC(gh_new_procedure(S_make_filter,SCM_FNC g_make_filter,0,6,0),H_make_filter);
  DEFINE_PROC(gh_new_procedure(S_filter,SCM_FNC g_filter,2,0,0),H_filter);
  DEFINE_PROC(gh_new_procedure(S_filter_p,SCM_FNC g_filter_p,1,0,0),H_filter_p);
  DEFINE_PROC(gh_new_procedure(S_make_fir_filter,SCM_FNC g_make_fir_filter,0,4,0),H_make_fir_filter);
  DEFINE_PROC(gh_new_procedure(S_fir_filter,SCM_FNC g_fir_filter,2,0,0),H_fir_filter);
  DEFINE_PROC(gh_new_procedure(S_fir_filter_p,SCM_FNC g_fir_filter_p,1,0,0),H_fir_filter_p);
  DEFINE_PROC(gh_new_procedure(S_make_iir_filter,SCM_FNC g_make_iir_filter,0,4,0),H_make_iir_filter);
  DEFINE_PROC(gh_new_procedure(S_iir_filter,SCM_FNC g_iir_filter,2,0,0),H_iir_filter);
  DEFINE_PROC(gh_new_procedure(S_iir_filter_p,SCM_FNC g_iir_filter_p,1,0,0),H_iir_filter_p);
  DEFINE_PROC(gh_new_procedure(S_mus_order,SCM_FNC g_mus_order,1,0,0),H_mus_order);
  DEFINE_PROC(gh_new_procedure(S_mus_xcoeffs,SCM_FNC g_mus_xcoeffs,1,0,0),H_mus_xcoeffs);
  DEFINE_PROC(gh_new_procedure(S_mus_ycoeffs,SCM_FNC g_mus_ycoeffs,1,0,0),H_mus_ycoeffs);
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
  return(((mus_scm_p(obj)) && (mus_env_p(mus_get_any(obj)))) ? SCM_BOOL_T : SCM_BOOL_F);
}

static SCM g_env(SCM obj) 
{
  #define H_env "(" S_env " gen) -> next sample from envelope generator"
  SCM_ASSERT(((mus_scm_p(obj)) && (mus_env_p(mus_get_any(obj)))),obj,SCM_ARG1,S_env);
  return(gh_double2scm(mus_env(mus_get_any(obj))));
}

static SCM g_restart_env(SCM obj) 
{
  #define H_restart_env "(" S_restart_env " gen) restarts (sets to sample 0) envelope generator gen"
  SCM_ASSERT(((mus_scm_p(obj)) && (mus_env_p(mus_get_any(obj)))),obj,SCM_ARG1,S_restart_env);
  mus_restart_env(mus_get_any(obj));
  return(SCM_BOOL_F);
}

static SCM g_make_env(SCM arglist)
{
  #define H_make_env "(" S_make_env " &opt-key envelope (scaler 1.0) duration (offset 0.0) (base 1.0) end (start 0))\n\
   returns a new envelope generator.  'envelope' is a list of break-point pairs. To create the envelope\n\
   these points are offset by 'offset', scaled by 'scaler', mapped over the time interval defined by\n\
   either 'duration' (seconds) or 'start' and 'end' (samples).  If 'base' is 1.0, the connecting segments\n\
   are linear, if 0.0 you get a step function, and anything else produces an exponential connecting segment."

  SCM new_e;
  mus_scm *gn;
  SCM args[14],keys[7];
  int orig_arg[7] = {0,0,0,0,0,0,0};
  int vals,i,len=0,arglist_len;
  Float base=1.0,scaler=1.0,offset=0.0,duration=0.0;
  int start=0,end=0,npts=0;
  Float *brkpts = NULL,*odata = NULL;
  keys[0] = all_keys[C_envelope];
  keys[1] = all_keys[C_scaler];
  keys[2] = all_keys[C_duration];
  keys[3] = all_keys[C_offset];
  keys[4] = all_keys[C_base];
  keys[5] = all_keys[C_end];
  keys[6] = all_keys[C_start];
  for (i=0;i<14;i++) args[i] = SCM_UNDEFINED;
  arglist_len = gh_length(arglist);
  for (i=0;i<arglist_len;i++) args[i] = gh_list_ref(arglist,gh_int2scm(i));
  vals = decode_keywords(S_make_env,7,keys,14,args,orig_arg);
  if (vals > 0)
    {
      /* env data is a list */
      if (!(keyword_p(keys[0])))
        {
	  if (gh_list_p(keys[0]))
	    {
	      len = gh_length(keys[0]);
	      npts = len/2;
	      brkpts = (Float *)CALLOC(len,sizeof(Float));
	      odata = (Float *)CALLOC(len,sizeof(Float));
	      for (i=0;i<len;i++)
		{
		  brkpts[i] = gh_scm2double(gh_list_ref(keys[0],gh_int2scm(i)));
		  odata[i] = brkpts[i];
		}
	    }
          else scm_wrong_type_arg(S_make_env,orig_arg[0]+1,args[orig_arg[0]]);
        }
      scaler = fkeyarg(keys[1],S_make_env,orig_arg[1]+1,args[orig_arg[1]],1.0);
      duration = fkeyarg(keys[2],S_make_env,orig_arg[2]+1,args[orig_arg[2]],0.0);
      offset = fkeyarg(keys[3],S_make_env,orig_arg[3]+1,args[orig_arg[3]],0.0);
      base = fkeyarg(keys[4],S_make_env,orig_arg[4]+1,args[orig_arg[4]],1.0);
      end = ikeyarg(keys[5],S_make_env,orig_arg[5]+1,args[orig_arg[5]],0);
      start = ikeyarg(keys[6],S_make_env,orig_arg[6]+1,args[orig_arg[6]],0);
    }
  if (brkpts == NULL) scm_misc_error(S_make_env,"no envelope?",SCM_EOL);
  /* odata = vct->data in this context [vcts[0]] */
  gn = (mus_scm *)CALLOC(1,sizeof(mus_scm));
  gn->vcts = (SCM *)CALLOC(1,sizeof(SCM));
  gn->vcts[0] = SCM_EOL;
  gn->nvcts = 1;
  gn->gen = mus_make_env(brkpts,npts,scaler,offset,base,duration,start,end,odata);
  FREE(brkpts);
#if (!HAVE_GUILE_1_3_0)
  SCM_NEWSMOB(new_e,mus_scm_tag,gn);
#else
  SCM_NEWCELL(new_e);
  SCM_SETCDR(new_e,(SCM)gn);
  SCM_SETCAR(new_e,mus_scm_tag);
#endif
  gn->vcts[MUS_DATA_POSITION] = make_vct(len,odata);
  return(new_e);
}

static SCM g_env_interp(SCM x, SCM env)
{
  #define H_env_interp "(" S_env_interp " gen x) -> value of envelope at x"
  SCM_ASSERT((SCM_NFALSEP(scm_real_p(x))),x,SCM_ARG1,S_env_interp);
  SCM_ASSERT(((mus_scm_p(env)) && (mus_env_p(mus_get_any(env)))),env,SCM_ARG2,S_env_interp);
  return(gh_double2scm(mus_env_interp(gh_scm2double(x),mus_get_any(env))));
}

static void init_env(void)
{
  DEFINE_PROC(gh_new_procedure(S_env_p,SCM_FNC g_env_p,1,0,0),H_env_p);
  DEFINE_PROC(gh_new_procedure(S_env,SCM_FNC g_env,1,0,0),H_env);
  DEFINE_PROC(gh_new_procedure(S_restart_env,SCM_FNC g_restart_env,1,0,0),H_restart_env);
  DEFINE_PROC(gh_new_procedure(S_make_env,SCM_FNC g_make_env,0,0,1),H_make_env);
  DEFINE_PROC(gh_new_procedure(S_env_interp,SCM_FNC g_env_interp,2,0,0),H_env_interp);
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
#define S_mus_input_p          "mus-input?"
#define S_mus_output_p         "mus-output?"
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

static SCM g_input_p(SCM obj) 
{
  #define H_mus_input_p "(" S_mus_input_p " gen) -> #t if gen is an input generator, else #f"
  return(((mus_scm_p(obj)) && (mus_input_p(mus_get_any(obj)))) ? SCM_BOOL_T : SCM_BOOL_F);
}

static SCM g_output_p(SCM obj) 
{
  #define H_mus_output_p "(" S_mus_output_p " gen) -> #t if gen is an output generator, else #f"
  return(((mus_scm_p(obj)) && (mus_output_p(mus_get_any(obj)))) ? SCM_BOOL_T : SCM_BOOL_F);
}

static SCM g_file2sample_p(SCM obj) 
{
  #define H_file2sample_p "(" S_file2sample_p " gen) -> #t if gen is a " S_file2sample " generator, else #f"
  return(((mus_scm_p(obj)) && (mus_file2sample_p(mus_get_any(obj)))) ? SCM_BOOL_T : SCM_BOOL_F);
}

static SCM g_file2frame_p(SCM obj) 
{
  #define H_file2frame_p "(" S_file2frame_p " gen) -> #t if gen is a " S_file2frame " generator, else #f"
  return(((mus_scm_p(obj)) && (mus_file2frame_p(mus_get_any(obj)))) ? SCM_BOOL_T : SCM_BOOL_F);
}

static SCM g_sample2file_p(SCM obj) 
{
  #define H_sample2file_p "(" S_sample2file_p " gen) -> #t if gen is a " S_sample2file " generator, else #f"
  return(((mus_scm_p(obj)) && (mus_sample2file_p(mus_get_any(obj)))) ? SCM_BOOL_T : SCM_BOOL_F);
}

static SCM g_frame2file_p(SCM obj) 
{
  #define H_frame2file_p "(" S_frame2file_p " gen) -> #t if gen is a " S_frame2file " generator, else #f"
  return(((mus_scm_p(obj)) && (mus_frame2file_p(mus_get_any(obj)))) ? SCM_BOOL_T : SCM_BOOL_F);
}

static SCM g_in_any_1(char *caller, SCM frame, SCM chan, SCM inp)
{
  SCM_ASSERT((SCM_NFALSEP(scm_real_p(frame))),frame,SCM_ARG1,caller);
  SCM_ASSERT((SCM_NFALSEP(scm_real_p(chan))),chan,SCM_ARG2,caller);
  SCM_ASSERT(((mus_scm_p(inp)) && (mus_input_p(mus_get_any(inp)))),inp,SCM_ARG3,caller);
  return(gh_double2scm(mus_in_any(g_scm2int(frame),g_scm2int(chan),(mus_input *)mus_get_any(inp))));
}

static SCM g_in_any(SCM frame, SCM chan, SCM inp) 
{
  #define H_in_any "(" S_in_any " frame chan &optional stream) -> input stream sample at frame in channel chan"
  return(g_in_any_1(S_in_any,frame,chan,inp));
}

static SCM g_ina(SCM frame, SCM inp) 
{
  #define H_ina "(" S_ina " frame &optional stream) -> input stream sample in channel 0 at frame"
  return(g_in_any_1(S_ina,frame,gh_int2scm(0),inp));
}

static SCM g_inb(SCM frame, SCM inp) 
{
  #define H_inb "(" S_inb " frame &optional stream) -> input stream sample in channel 1 at frame"
  return(g_in_any_1(S_inb,frame,gh_int2scm(1),inp));
}

static SCM g_out_any_1(char *caller, SCM frame, SCM chan, SCM val, SCM outp)
{
  SCM_ASSERT((SCM_NFALSEP(scm_real_p(frame))),frame,SCM_ARG1,caller);
  SCM_ASSERT((SCM_NFALSEP(scm_real_p(chan))),chan,SCM_ARG2,caller);
  SCM_ASSERT((SCM_NFALSEP(scm_real_p(val))),val,SCM_ARG3,caller);
  SCM_ASSERT(((mus_scm_p(outp)) && (mus_output_p(mus_get_any(outp)))),outp,SCM_ARG4,caller);
  return(gh_double2scm(mus_out_any(g_scm2int(frame),gh_scm2double(val),g_scm2int(chan),(mus_output *)mus_get_any(outp))));
}

static SCM g_out_any(SCM frame, SCM val, SCM chan, SCM outp)
{
  #define H_out_any "(" S_out_any " frame val chan &optional stream) adds val to output stream at frame in channel chan"
  return(g_out_any_1(S_out_any,frame,chan,val,outp));
}

static SCM g_outa(SCM frame, SCM val, SCM outp)
{
  #define H_outa "(" S_outa " frame val &optional stream) adds val to output stream at frame in channel 0"
  return(g_out_any_1(S_outa,frame,gh_int2scm(0),val,outp));
}

static SCM g_outb(SCM frame, SCM val, SCM outp)
{
  #define H_outb "(" S_outb " frame val &optional stream) adds val to output stream at frame in channel 1"
  return(g_out_any_1(S_outb,frame,gh_int2scm(1),val,outp));
}

static SCM g_outc(SCM frame, SCM val, SCM outp)
{
  #define H_outc "(" S_outc " frame val &optional stream) adds val to output stream at frame in channel 2"
  return(g_out_any_1(S_outc,frame,gh_int2scm(2),val,outp));
}

static SCM g_outd(SCM frame, SCM val, SCM outp)
{
  #define H_outd "(" S_outd " frame val &optional stream) adds val to output stream at frame in channel 3"
  return(g_out_any_1(S_outd,frame,gh_int2scm(3),val,outp));
}

static SCM g_mus_close(SCM ptr)
{
  #define H_mus_close "(" S_mus_close " fd) closes the stream (fd) opened by mus-open-read or write"
  return(gh_int2scm(mus_close_file((mus_any *)mus_get_any(ptr))));
}

static SCM g_make_file2sample(SCM name)
{
  #define H_make_file2sample "(" S_make_file2sample " filename) returns an input generator reading 'filename' (a sound file)"
  char *filename;
#if HAVE_GUILE_1_3_0
  SCM new_e;
#endif
  mus_scm *gn;
  SCM_ASSERT((gh_string_p(name)),name,SCM_ARG1,S_make_file2sample);
  gn = (mus_scm *)CALLOC(1,sizeof(mus_scm));
  filename = gh_scm2newstr(name,NULL);
  gn->gen = mus_make_file2sample(filename);
  free(filename);
  gn->nvcts = 0;
#if (!HAVE_GUILE_1_3_0)
  SCM_RETURN_NEWSMOB(mus_scm_tag,gn);
#else
  SCM_NEWCELL(new_e);
  SCM_SETCDR(new_e,(SCM)gn);
  SCM_SETCAR(new_e,mus_scm_tag);
  return(new_e);
#endif
}

static SCM g_file2sample(SCM obj, SCM samp, SCM chan)
{
  #define H_file2sample "(" S_file2sample " obj frame chan) -> sample value in sound file read by 'obj' in channel chan at frame"
  int channel = 0;
  SCM_ASSERT(((mus_scm_p(obj)) && (mus_input_p(mus_get_any(obj)))),obj,SCM_ARG1,S_file2sample);
  SCM_ASSERT((SCM_NFALSEP(scm_real_p(samp))),samp,SCM_ARG2,S_file2sample);
  if (!(SCM_UNBNDP(chan)))
    {
      SCM_ASSERT((SCM_NFALSEP(scm_real_p(chan))),chan,SCM_ARG3,S_file2sample);
      channel = g_scm2int(chan);
    }
  return(gh_double2scm(mus_file2sample(mus_get_any(obj),g_scm2int(samp),channel)));
}

static SCM g_make_sample2file(SCM name, SCM chans, SCM out_format, SCM out_type)
{
  #define H_make_sample2file "(" S_make_sample2file " filename chans data-format header-type)\n\
   returns an output generator writing the sound file 'filename' which is set up to have\n\
   'chans' channels of 'data-format' samples with a header of 'header-type'.  The latter\n\
   should be sndlib identifiers:\n\
      (make-sample->file \"test.snd\" 2 mus-lshort mus-riff)"

  char *filename;
#if HAVE_GUILE_1_3_0
  SCM new_e;
#endif
  mus_scm *gn;
  SCM_ASSERT((gh_string_p(name)),name,SCM_ARG1,S_make_sample2file);
  SCM_ASSERT((SCM_NFALSEP(scm_real_p(chans))),chans,SCM_ARG2,S_make_sample2file);
  SCM_ASSERT((SCM_NFALSEP(scm_real_p(out_format))),out_format,SCM_ARG3,S_make_sample2file);
  SCM_ASSERT((SCM_NFALSEP(scm_real_p(out_type))),out_type,SCM_ARG4,S_make_sample2file);
  gn = (mus_scm *)CALLOC(1,sizeof(mus_scm));
  filename = gh_scm2newstr(name,NULL);
  gn->gen = mus_make_sample2file(filename,g_scm2int(chans),g_scm2int(out_format),g_scm2int(out_type));
  free(filename);
  gn->nvcts = 0;
#if (!HAVE_GUILE_1_3_0)
  SCM_RETURN_NEWSMOB(mus_scm_tag,gn);
#else
  SCM_NEWCELL(new_e);
  SCM_SETCDR(new_e,(SCM)gn);
  SCM_SETCAR(new_e,mus_scm_tag);
  return(new_e);
#endif
}

static SCM g_sample2file(SCM obj, SCM samp, SCM chan, SCM val)
{
  #define H_sample2file "(" S_sample2file " obj samp chan val) adds val to the output stream\n\
   handled by the output generator 'obj', in channel 'chan' at frame 'samp'"

  SCM_ASSERT(((mus_scm_p(obj)) && (mus_output_p(mus_get_any(obj)))),obj,SCM_ARG1,S_sample2file);
  SCM_ASSERT((SCM_NFALSEP(scm_real_p(samp))),samp,SCM_ARG2,S_sample2file);
  SCM_ASSERT((SCM_NFALSEP(scm_real_p(chan))),chan,SCM_ARG3,S_sample2file);
  SCM_ASSERT((SCM_NFALSEP(scm_real_p(val))),val,SCM_ARG4,S_sample2file);
  return(gh_double2scm(mus_sample2file(mus_get_any(obj),g_scm2int(samp),g_scm2int(chan),gh_scm2double(val))));
}

static SCM g_make_file2frame(SCM name)
{
  #define H_make_file2frame "(" S_make_file2frame " filename) returns an input generator reading 'filename' (a sound file)"
  char *filename;
#if HAVE_GUILE_1_3_0
  SCM new_e;
#endif
  mus_scm *gn;
  SCM_ASSERT((gh_string_p(name)),name,SCM_ARG1,S_make_file2frame);
  gn = (mus_scm *)CALLOC(1,sizeof(mus_scm));
  filename = gh_scm2newstr(name,NULL);
  gn->gen = mus_make_file2frame(filename);
  free(filename);
  gn->nvcts = 0;
#if (!HAVE_GUILE_1_3_0)
  SCM_RETURN_NEWSMOB(mus_scm_tag,gn);
#else
  SCM_NEWCELL(new_e);
  SCM_SETCDR(new_e,(SCM)gn);
  SCM_SETCAR(new_e,mus_scm_tag);
  return(new_e);
#endif
}

static SCM g_file2frame(SCM obj, SCM samp, SCM outfr)
{
  #define H_file2frame "(" S_file2frame " obj samp outf) -> frame of samples at frame 'samp' in sound file read by 'obj'"
  mus_frame *res = NULL;
  SCM_ASSERT(((mus_scm_p(obj)) && (mus_input_p(mus_get_any(obj)))),obj,SCM_ARG1,S_file2frame);
  SCM_ASSERT((SCM_NFALSEP(scm_real_p(samp))),samp,SCM_ARG2,S_file2frame);
  if ((mus_scm_p(outfr)) && (mus_frame_p(mus_get_any(outfr)))) res = (mus_frame *)mus_get_any(outfr);
  return(g_wrap_frame(mus_file2frame(mus_get_any(obj),g_scm2int(samp),res),(res) ? DONT_FREE_FRAME : FREE_FRAME));
}

static SCM g_make_frame2file(SCM name, SCM chans, SCM out_format, SCM out_type)
{
  #define H_make_frame2file "(" S_make_frame2file " filename chans data-format header-type)\n\
   returns an output generator writing the sound file 'filename' which is set up to have\n\
   'chans' channels of 'data-format' samples with a header of 'header-type'.  The latter\n\
   should be sndlib identifiers:\n\
      (make-frame->file \"test.snd\" 2 mus-lshort mus-riff)"

  char *filename;
#if HAVE_GUILE_1_3_0
  SCM new_e;
#endif
  mus_scm *gn;
  SCM_ASSERT((gh_string_p(name)),name,SCM_ARG1,S_make_frame2file);
  SCM_ASSERT((SCM_NFALSEP(scm_real_p(chans))),chans,SCM_ARG2,S_make_frame2file);
  SCM_ASSERT((SCM_NFALSEP(scm_real_p(out_format))),out_format,SCM_ARG3,S_make_frame2file);
  SCM_ASSERT((SCM_NFALSEP(scm_real_p(out_type))),out_type,SCM_ARG4,S_make_frame2file);
  gn = (mus_scm *)CALLOC(1,sizeof(mus_scm));
  filename = gh_scm2newstr(name,NULL);
  gn->gen = mus_make_frame2file(filename,g_scm2int(chans),g_scm2int(out_format),g_scm2int(out_type));
  free(filename);
  gn->nvcts = 0;
#if (!HAVE_GUILE_1_3_0)
  SCM_RETURN_NEWSMOB(mus_scm_tag,gn);
#else
  SCM_NEWCELL(new_e);
  SCM_SETCDR(new_e,(SCM)gn);
  SCM_SETCAR(new_e,mus_scm_tag);
  return(new_e);
#endif
}

static SCM g_frame2file(SCM obj, SCM samp, SCM val)
{
  #define H_frame2file "(" S_frame2file " obj samp val) adds frame 'val' to the output stream\n\
   handled by the output generator 'obj' at frame 'samp'"

  SCM_ASSERT(((mus_scm_p(obj)) && (mus_output_p(mus_get_any(obj)))),obj,SCM_ARG1,S_frame2file);
  SCM_ASSERT((SCM_NFALSEP(scm_real_p(samp))),samp,SCM_ARG2,S_frame2file);
  SCM_ASSERT(((mus_scm_p(val)) && (mus_frame_p(mus_get_any(val)))),val,SCM_ARG3,S_frame2file);
  return(g_wrap_frame(mus_frame2file(mus_get_any(obj),g_scm2int(samp),(mus_frame *)mus_get_any(val)),DONT_FREE_FRAME));
}

static SCM g_array2file(SCM filename, SCM data, SCM len, SCM srate, SCM channels)
{
  #define H_array2file "(" S_array2file " filename data len srate channels) writes 'data',\n\
   a vct object of interleaved samples to the sound file 'filename' set up to have the given\n\
   srate and channels.  'len' samples are written."

  int olen;
  char *name = NULL;
  vct *v;
  SCM_ASSERT((gh_string_p(filename)),filename,SCM_ARG1,S_array2file);
  SCM_ASSERT((vct_p(data)),data,SCM_ARG2,S_array2file);
  SCM_ASSERT((SCM_NFALSEP(scm_real_p(len))),len,SCM_ARG3,S_array2file);
  SCM_ASSERT((SCM_NFALSEP(scm_real_p(srate))),srate,SCM_ARG4,S_array2file);
  SCM_ASSERT((SCM_NFALSEP(scm_real_p(channels))),channels,SCM_ARG5,S_array2file);
  name = gh_scm2newstr(filename,NULL);
  v = get_vct(data);
  olen = mus_fltarray2file(name,v->data,g_scm2int(len),g_scm2int(srate),g_scm2int(channels));
  if (name) free(name);
  return(gh_int2scm(olen));
}

static SCM g_file2array(SCM filename, SCM chan, SCM start, SCM samples, SCM data)
{
  #define H_file2array "(" S_file2array " filename chan start samples data) reads the sound file\n\
   'filename' placing samples from channel 'chan' into the vct object 'data' starting in the file\n\
   at frame 'start' and reading 'samples' samples altogether."

  int err;
  char *name = NULL;
  vct *v;
  SCM_ASSERT((gh_string_p(filename)),filename,SCM_ARG1,S_file2array);
  SCM_ASSERT((SCM_NFALSEP(scm_real_p(chan))),chan,SCM_ARG2,S_file2array);
  SCM_ASSERT((SCM_NFALSEP(scm_real_p(start))),start,SCM_ARG3,S_file2array);
  SCM_ASSERT((SCM_NFALSEP(scm_real_p(samples))),samples,SCM_ARG4,S_file2array);
  SCM_ASSERT((vct_p(data)),data,SCM_ARG5,S_file2array);
  name = gh_scm2newstr(filename,NULL);
  v = get_vct(data);
  err = mus_file2fltarray(name,g_scm2int(chan),g_scm2int(start),g_scm2int(samples),v->data);
  if (name) free(name);
  return(gh_int2scm(err));
}

static void init_io(void)
{
  DEFINE_PROC(gh_new_procedure(S_file2sample_p,SCM_FNC g_file2sample_p,1,0,0),H_file2sample_p);
  DEFINE_PROC(gh_new_procedure(S_make_file2sample,SCM_FNC g_make_file2sample,1,0,0),H_make_file2sample);
  DEFINE_PROC(gh_new_procedure(S_file2sample,SCM_FNC g_file2sample,2,1,0),H_file2sample);
  DEFINE_PROC(gh_new_procedure(S_file2frame_p,SCM_FNC g_file2frame_p,1,0,0),H_file2frame_p);
  DEFINE_PROC(gh_new_procedure(S_make_file2frame,SCM_FNC g_make_file2frame,1,0,0),H_make_file2frame);
  DEFINE_PROC(gh_new_procedure(S_file2frame,SCM_FNC g_file2frame,2,1,0),H_file2frame);
  DEFINE_PROC(gh_new_procedure(S_sample2file_p,SCM_FNC g_sample2file_p,1,0,0),H_sample2file_p);
  DEFINE_PROC(gh_new_procedure(S_make_sample2file,SCM_FNC g_make_sample2file,4,0,0),H_make_sample2file);
  DEFINE_PROC(gh_new_procedure(S_sample2file,SCM_FNC g_sample2file,4,0,0),H_sample2file);
  DEFINE_PROC(gh_new_procedure(S_frame2file_p,SCM_FNC g_frame2file_p,1,0,0),H_frame2file_p);
  DEFINE_PROC(gh_new_procedure(S_frame2file,SCM_FNC g_frame2file,3,0,0),H_frame2file);
  DEFINE_PROC(gh_new_procedure(S_make_frame2file,SCM_FNC g_make_frame2file,4,0,0),H_make_frame2file);
  DEFINE_PROC(gh_new_procedure(S_mus_input_p,SCM_FNC g_input_p,1,0,0),H_mus_input_p);
  DEFINE_PROC(gh_new_procedure(S_mus_output_p,SCM_FNC g_output_p,1,0,0),H_mus_output_p);
  DEFINE_PROC(gh_new_procedure(S_in_any,SCM_FNC g_in_any,3,0,0),H_in_any);
  DEFINE_PROC(gh_new_procedure(S_ina,SCM_FNC g_ina,2,0,0),H_ina);  
  DEFINE_PROC(gh_new_procedure(S_inb,SCM_FNC g_inb,2,0,0),H_inb);
  DEFINE_PROC(gh_new_procedure(S_out_any,SCM_FNC g_out_any,4,0,0),H_out_any);
  DEFINE_PROC(gh_new_procedure(S_outa,SCM_FNC g_outa,3,0,0),H_outa);
  DEFINE_PROC(gh_new_procedure(S_outb,SCM_FNC g_outb,3,0,0),H_outb);
  DEFINE_PROC(gh_new_procedure(S_outc,SCM_FNC g_outc,3,0,0),H_outc);
  DEFINE_PROC(gh_new_procedure(S_outd,SCM_FNC g_outd,3,0,0),H_outd);
  DEFINE_PROC(gh_new_procedure(S_array2file,SCM_FNC g_array2file,5,0,0),H_array2file);
  DEFINE_PROC(gh_new_procedure(S_file2array,SCM_FNC g_file2array,5,0,0),H_file2array);
  DEFINE_PROC(gh_new_procedure(S_mus_close,SCM_FNC g_mus_close,1,0,0),H_mus_close);
}


/* ---------------- readin ---------------- */

#define S_readin        "readin"
#define S_readin_p      "readin?"
#define S_make_readin   "make-readin"
#define S_mus_increment     "mus-increment"
#define S_mus_set_increment "mus-set-increment"
#define S_mus_location      "mus-location"
#define S_mus_set_location  "mus-set-location"
#define S_mus_channel       "mus-channel"

static SCM g_readin_p(SCM obj) 
{
  #define H_readin_p "(" S_readin_p " gen) -> #t if gen is a " S_readin " generator, else #f"
  return(((mus_scm_p(obj)) && (mus_readin_p(mus_get_any(obj)))) ? SCM_BOOL_T : SCM_BOOL_F);
}

static SCM g_readin(SCM obj)
{
  #define H_readin "(" S_readin " gen) -> next sample from readin generator (a sound file reader)"
  SCM_ASSERT(((mus_scm_p(obj)) && (mus_readin_p(mus_get_any(obj)))),obj,SCM_ARG1,S_readin);
  return(gh_double2scm(mus_readin(mus_get_any(obj))));
}

static SCM g_make_readin(SCM arg1, SCM arg2, SCM arg3, SCM arg4, SCM arg5, SCM arg6, SCM arg7, SCM arg8)
{
  #define H_make_readin "(" S_make_readin " &opt-key file (channel 0) (start 0) (direction 1))\n\
   returns a new readin (file input) generator reading the sound file 'file' starting at frame\n\
   'start' in channel 'channel' and reading forward if 'direction' is not -1"

  /* optkey file channel start direction */
  SCM new_rd;
  mus_scm *gn;
  char *file = NULL;
  SCM args[8],keys[4];
  int orig_arg[4] = {0,0,0,0};
  int vals;
  int channel = 0, start = 0, direction = 1;
  keys[0] = all_keys[C_file];
  keys[1] = all_keys[C_channel];
  keys[2] = all_keys[C_start];
  keys[3] = all_keys[C_direction];
  args[0] = arg1; args[1] = arg2; args[2] = arg3; args[3] = arg4; args[4] = arg5; args[5] = arg6; args[6] = arg7; args[7] = arg8; 
  vals = decode_keywords(S_make_readin,4,keys,8,args,orig_arg);
  if (vals > 0)
    {
      if (!(keyword_p(keys[0])))
        {
	  if (gh_string_p(keys[0]))
	    file = gh_scm2newstr(keys[0],NULL);
	  else scm_wrong_type_arg(S_make_readin,orig_arg[0]+1,args[orig_arg[0]]);
	}
      channel = ikeyarg(keys[1],S_make_readin,orig_arg[1]+1,args[orig_arg[1]],channel);
      start = ikeyarg(keys[2],S_make_readin,orig_arg[2]+1,args[orig_arg[2]],start);
      direction = ikeyarg(keys[3],S_make_readin,orig_arg[3]+1,args[orig_arg[3]],direction);
    }
  gn = (mus_scm *)CALLOC(1,sizeof(mus_scm));
  gn->gen = mus_make_readin(file,channel,start,direction);
#if (!HAVE_GUILE_1_3_0)
  SCM_NEWSMOB(new_rd,mus_scm_tag,gn);
#else
  SCM_NEWCELL(new_rd);
  SCM_SETCDR(new_rd,(SCM)gn);
  SCM_SETCAR(new_rd,mus_scm_tag);
#endif
  if (file) free(file); /* copied by mus_make_readin, allocated by gh_scm2newstr */
  return(new_rd);
}

static SCM g_increment(SCM obj)
{
  #define H_mus_increment "(" S_mus_increment " gen) -> gen's " S_mus_increment " field, if any"
  SCM_ASSERT((mus_scm_p(obj)),obj,SCM_ARG1,S_mus_increment);
  return(gh_double2scm(mus_increment(mus_get_any(obj))));
}

static SCM g_set_increment(SCM obj, SCM val)
{
  #define H_mus_set_increment "(" S_mus_set_increment " gen val) sets gen's " S_mus_increment " field to val"
  SCM_ASSERT((mus_scm_p(obj)),obj,SCM_ARG1,S_mus_set_increment);
  SCM_ASSERT((SCM_NFALSEP(scm_real_p(val))),val,SCM_ARG2,S_mus_set_increment);
  return(gh_double2scm(mus_set_increment(mus_get_any(obj),gh_scm2double(val))));
}

static SCM g_location(SCM obj)
{
  #define H_mus_location "(" S_mus_location " gen) -> gen's " S_mus_location " field, if any"
  SCM_ASSERT((mus_scm_p(obj)),obj,SCM_ARG1,S_mus_location);
  return(gh_int2scm(mus_location(mus_get_any(obj))));
}

static SCM g_set_location(SCM obj, SCM val)
{
  #define H_mus_set_location "(" S_mus_set_location " gen val) sets gen's " S_mus_location " field to val"
  SCM_ASSERT((mus_scm_p(obj)),obj,SCM_ARG1,S_mus_set_location);
  SCM_ASSERT((SCM_NFALSEP(scm_real_p(val))),val,SCM_ARG2,S_mus_set_location);
  return(gh_int2scm(mus_set_location(mus_get_any(obj),g_scm2int(val))));
}

static SCM g_channel(SCM obj)
{
  #define H_mus_channel "(" S_mus_channel " gen) -> gen's " S_mus_channel " field, if any"
  SCM_ASSERT((mus_scm_p(obj)),obj,SCM_ARG1,S_mus_channel);
  return(gh_int2scm(mus_channel((mus_input *)mus_get_any(obj))));
}

static void init_rdin(void)
{
  DEFINE_PROC(gh_new_procedure(S_readin_p,SCM_FNC g_readin_p,1,0,0),H_readin_p);
  DEFINE_PROC(gh_new_procedure(S_readin,SCM_FNC g_readin,1,0,0),H_readin);
  DEFINE_PROC(gh_new_procedure(S_make_readin,SCM_FNC g_make_readin,0,8,0),H_make_readin);
  DEFINE_PROC(gh_new_procedure(S_mus_location,SCM_FNC g_location,1,0,0),H_mus_location);
  DEFINE_PROC(gh_new_procedure(S_mus_set_location,SCM_FNC g_set_location,2,0,0),H_mus_set_location);
  DEFINE_PROC(gh_new_procedure(S_mus_increment,SCM_FNC g_increment,1,0,0),H_mus_increment);
  DEFINE_PROC(gh_new_procedure(S_mus_set_increment,SCM_FNC g_set_increment,2,0,0),H_mus_set_increment);
  DEFINE_PROC(gh_new_procedure(S_mus_channel,SCM_FNC g_channel,1,0,0),H_mus_channel);
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
  SCM_ASSERT(((mus_scm_p(obj)) && (mus_locsig_p(mus_get_any(obj)))),obj,SCM_ARG1,S_locsig_ref);
  SCM_ASSERT((SCM_NFALSEP(scm_real_p(chan))),chan,SCM_ARG2,S_locsig_ref);
  return(gh_double2scm(mus_locsig_ref(mus_get_any(obj),g_scm2int(chan))));
}

static SCM g_locsig_set(SCM obj, SCM chan, SCM val)
{
  #define H_locsig_set "(" S_locsig_set " gen chan val) sets the locsig generator's channel 'chan' scaler to 'val'"
  SCM_ASSERT(((mus_scm_p(obj)) && (mus_locsig_p(mus_get_any(obj)))),obj,SCM_ARG1,S_locsig_set);
  SCM_ASSERT((SCM_NFALSEP(scm_real_p(chan))),chan,SCM_ARG2,S_locsig_set);
  SCM_ASSERT((SCM_NFALSEP(scm_real_p(val))),val,SCM_ARG3,S_locsig_set);
  return(gh_double2scm(mus_locsig_set(mus_get_any(obj),g_scm2int(chan),gh_scm2double(val))));
}

static SCM g_locsig_reverb_ref(SCM obj, SCM chan)
{
  #define H_locsig_reverb_ref "(" S_locsig_reverb_ref " gen chan) -> locsig reverb channel 'chan' scaler"
  SCM_ASSERT(((mus_scm_p(obj)) && (mus_locsig_p(mus_get_any(obj)))),obj,SCM_ARG1,S_locsig_reverb_ref);
  SCM_ASSERT((SCM_NFALSEP(scm_real_p(chan))),chan,SCM_ARG2,S_locsig_reverb_ref);
  return(gh_double2scm(mus_locsig_reverb_ref(mus_get_any(obj),g_scm2int(chan))));
}

static SCM g_locsig_reverb_set(SCM obj, SCM chan, SCM val)
{
  #define H_locsig_reverb_set "(" S_locsig_reverb_set " gen chan val) sets the locsig reverb channel 'chan' scaler to 'val'"
  SCM_ASSERT(((mus_scm_p(obj)) && (mus_locsig_p(mus_get_any(obj)))),obj,SCM_ARG1,S_locsig_reverb_set);
  SCM_ASSERT((SCM_NFALSEP(scm_real_p(chan))),chan,SCM_ARG2,S_locsig_reverb_set);
  SCM_ASSERT((SCM_NFALSEP(scm_real_p(val))),val,SCM_ARG3,S_locsig_reverb_set);
  return(gh_double2scm(mus_locsig_reverb_set(mus_get_any(obj),g_scm2int(chan),gh_scm2double(val))));
}

static SCM g_locsig_p(SCM obj)
{
  #define H_locsig_p "(" S_locsig_p " gen) -> #t if gen is a " S_locsig " generator, else #f"
  return(((mus_scm_p(obj)) && (mus_locsig_p(mus_get_any(obj)))) ? SCM_BOOL_T : SCM_BOOL_F);
}

static SCM g_locsig(SCM obj, SCM loc, SCM val)
{
  #define H_locsig "(" S_locsig " gen loc val) adds 'val' to the output of locsig at frame 'loc'"
  SCM_ASSERT(((mus_scm_p(obj)) && (mus_locsig_p(mus_get_any(obj)))),obj,SCM_ARG1,S_locsig);
  SCM_ASSERT((SCM_NFALSEP(scm_real_p(loc))),loc,SCM_ARG2,S_locsig);
  SCM_ASSERT((SCM_NFALSEP(scm_real_p(val))),val,SCM_ARG3,S_locsig);
  return(g_wrap_frame(mus_locsig(mus_get_any(obj),g_scm2int(loc),gh_scm2double(val)),DONT_FREE_FRAME));
}

static SCM g_make_locsig(SCM arglist)
{
  #define H_make_locsig "(" S_make_locsig " &opt-key (degree 0.0) (distance 1.0) (reverb 0.0) output revout (channels 1))\n\
   returns a new generator for signal placement in up to 4 channels.  Channel 0 corresponds to 0 degrees."

#if HAVE_GUILE_1_3_0
  SCM new_loc;
#endif
  SCM out_obj = SCM_UNDEFINED,rev_obj = SCM_UNDEFINED;
  mus_scm *gn;
  mus_output *outp = NULL, *revp = NULL;
  SCM args[12],keys[6];
  int orig_arg[6] = {0,0,0,0,0,0};
  int vals,i,arglist_len,vlen = 0,out_chans = 1;
  Float degree = 0.0, distance = 1.0, reverb = 0.0;
  keys[0] = all_keys[C_degree];
  keys[1] = all_keys[C_distance];
  keys[2] = all_keys[C_reverb];
  keys[3] = all_keys[C_output];  
  keys[4] = all_keys[C_revout];
  keys[5] = all_keys[C_channels];
  for (i=0;i<12;i++) args[i] = SCM_UNDEFINED;
  arglist_len = gh_length(arglist);
  for (i=0;i<arglist_len;i++) args[i] = gh_list_ref(arglist,gh_int2scm(i));
  vals = decode_keywords(S_make_locsig,6,keys,12,args,orig_arg);
  if (vals > 0)
    {
      degree = fkeyarg(keys[0],S_make_locsig,orig_arg[0]+1,args[orig_arg[0]],degree);
      distance = fkeyarg(keys[1],S_make_locsig,orig_arg[1]+1,args[orig_arg[1]],distance);
      reverb = fkeyarg(keys[2],S_make_locsig,orig_arg[2]+1,args[orig_arg[2]],reverb);
      if (!(keyword_p(keys[3]))) 
	{
	  if ((mus_scm_p(keys[3])) && (mus_output_p(mus_get_any(keys[3]))))
	    {
	      out_obj = keys[3];
	      vlen++;
	      outp = (mus_output *)mus_get_any(keys[3]);
	      out_chans = mus_channels((mus_any *)outp);
	    }
	  else scm_wrong_type_arg(S_make_locsig,orig_arg[3]+1,args[orig_arg[3]]);
	}
      if (!(keyword_p(keys[4]))) 
	{
	  if ((mus_scm_p(keys[4])) && (mus_output_p(mus_get_any(keys[4]))))
	    {
	      rev_obj = keys[4];
	      vlen++;
	      revp = (mus_output *)mus_get_any(keys[4]);
	    }
	  else scm_wrong_type_arg(S_make_locsig,orig_arg[4]+1,args[orig_arg[4]]);
	}
      out_chans = ikeyarg(keys[5],S_make_locsig,orig_arg[5]+1,args[orig_arg[5]],out_chans);
    }
  gn = (mus_scm *)CALLOC(1,sizeof(mus_scm));
  if (vlen > 0)
    {
      gn->vcts = (SCM *)CALLOC(vlen,sizeof(SCM));
      i = 0;
      if (!(SCM_UNBNDP(out_obj))) gn->vcts[i++] = out_obj;
      if (!(SCM_UNBNDP(rev_obj))) gn->vcts[i] = rev_obj;
      gn->nvcts = vlen;
    }
  gn->gen = mus_make_locsig(degree,distance,reverb,out_chans,outp,revp);
#if (!HAVE_GUILE_1_3_0)
  SCM_RETURN_NEWSMOB(mus_scm_tag,gn);
#else
  SCM_NEWCELL(new_loc);
  SCM_SETCDR(new_loc,(SCM)gn);
  SCM_SETCAR(new_loc,mus_scm_tag);
  return(new_loc);
#endif
}

static SCM g_channels(SCM obj)
{
  #define H_mus_channels "(" S_mus_channels " gen) -> gen's " S_mus_channels " field, if any"
  SCM_ASSERT((mus_scm_p(obj)),obj,SCM_ARG1,S_mus_channels);
  return(gh_int2scm(mus_channels(mus_get_any(obj))));
}

static void init_locs(void)
{
  DEFINE_PROC(gh_new_procedure(S_locsig_p,SCM_FNC g_locsig_p,1,0,0),H_locsig_p);
  DEFINE_PROC(gh_new_procedure(S_locsig,SCM_FNC g_locsig,3,0,0),H_locsig);
  DEFINE_PROC(gh_new_procedure(S_make_locsig,SCM_FNC g_make_locsig,0,0,1),H_make_locsig);
  DEFINE_PROC(gh_new_procedure(S_mus_channels,SCM_FNC g_channels,1,0,0),H_mus_channels);
  DEFINE_PROC(gh_new_procedure(S_locsig_ref,SCM_FNC g_locsig_ref,2,0,0),H_locsig_ref);
  DEFINE_PROC(gh_new_procedure(S_locsig_reverb_ref,SCM_FNC g_locsig_reverb_ref,2,0,0),H_locsig_reverb_ref);
  DEFINE_PROC(gh_new_procedure(S_locsig_set,SCM_FNC g_locsig_set,3,0,0),H_locsig_set);
  DEFINE_PROC(gh_new_procedure(S_locsig_reverb_set,SCM_FNC g_locsig_reverb_set,3,0,0),H_locsig_reverb_set);
}


/* ---------------- src ---------------- */

static Float funcall_reader (void *ptr, int direction)
{
  /* if this is called, it's a callback from C, where ptr is a mus_scm object whose vcts[0]
   * field is an SCM procedure to be called, the result being returned back to C.  In the
   * Scheme world, it's a procedure of one arg, the current read direction
   */
  mus_scm *gn = (mus_scm *)ptr;
  if ((gn) && (gn->vcts) && (gn->vcts[0]) && (gh_procedure_p(gn->vcts[0])))
    /* the gh_procedure_p call can be confused by 0 -> segfault! */
#if USE_SND
    return(gh_scm2double(g_call1(gn->vcts[0],gh_int2scm(direction))));
#else
    return(gh_scm2double(gh_call1(gn->vcts[0],gh_int2scm(direction))));
#endif
  else return(0.0);
}

#define S_src       "src"
#define S_src_p     "src?"
#define S_make_src  "make-src"

static SCM g_src_p(SCM obj) 
{
  #define H_src_p "(" S_src_p " gen) -> #t if gen is an " S_src " generator, else #f"
  return(((mus_scm_p(obj)) && (mus_src_p(mus_get_any(obj)))) ? SCM_BOOL_T : SCM_BOOL_F);
}

static SCM g_src(SCM obj, SCM pm, SCM func) 
{
  #define H_src "(" S_src " gen &optional (pm 0.0) input-function) -> next sampling rate conversion sample.\n\
   'pm' can be used to change the sampling rate on a sample-by-sample basis.  'input-function'\n\
   is a function of one argument (the current input direction, normally ignored) that is called\n\
   internally whenever a new sample of input data is needed.  If the associated " S_make_src "\n\
   included an 'input' argument, input-function is ignored."

  Float pm1 = 0.0;
  mus_scm *gn = mus_get_scm(obj);
  SCM_ASSERT(((mus_scm_p(obj)) && (mus_src_p(mus_get_any(obj)))),obj,SCM_ARG1,S_src);
  if (SCM_NFALSEP(scm_real_p(pm))) pm1 = gh_scm2double(pm); else if (!(SCM_UNBNDP(pm))) scm_wrong_type_arg(S_src,2,pm);
  if (gh_procedure_p(func)) gn->vcts[0] = func;
  return(gh_double2scm(mus_src(mus_get_any(obj),pm1,0)));
}

static SCM g_make_src(SCM arg1, SCM arg2, SCM arg3, SCM arg4, SCM arg5, SCM arg6)
{
  #define H_make_src "(" S_make_src " &opt-key input (srate 1.0) (width 10))\n\
   returns a new sampling-rate conversion generator (using 'warped sinc interpolation').\n\
   'srate' is the ratio between the new rate and the old. 'width' is the sine\n\
   width (effectively the steepness of the low-pass filter), normally between 10 and 100.\n\
   'input' if given is an open file stream."

#if HAVE_GUILE_1_3_0
  SCM new_src;
#endif
  SCM in_obj = SCM_UNDEFINED;
  mus_scm *gn;
  int vals,wid = 0;
  SCM args[6],keys[3];
  int orig_arg[3] = {0,0,0};
  Float srate = 1.0;
  keys[0] = all_keys[C_input];
  keys[1] = all_keys[C_srate];
  keys[2] = all_keys[C_width];
  args[0] = arg1; args[1] = arg2; args[2] = arg3; args[3] = arg4; args[4] = arg5; args[5] = arg6;
  vals = decode_keywords(S_make_src,3,keys,6,args,orig_arg);
  if (vals > 0)
    {
      if (!(keyword_p(keys[0]))) 
	{
	  if (gh_procedure_p(keys[0]))
	    in_obj = keys[0];
	  else scm_wrong_type_arg(S_make_src,orig_arg[0]+1,args[orig_arg[0]]);
	}
      srate = fkeyarg(keys[1],S_make_src,orig_arg[1]+1,args[orig_arg[1]],srate);
      wid = ikeyarg(keys[2],S_make_src,orig_arg[2]+1,args[orig_arg[2]],wid);
    }
  gn = (mus_scm *)CALLOC(1,sizeof(mus_scm));
  gn->vcts = (SCM *)CALLOC(1,sizeof(SCM));
  if (!(SCM_UNBNDP(in_obj))) gn->vcts[0] = in_obj; else gn->vcts[0] = SCM_EOL;
  gn->nvcts = 1;
  gn->gen = mus_make_src(funcall_reader,srate,wid,gn);
#if (!HAVE_GUILE_1_3_0)
  SCM_RETURN_NEWSMOB(mus_scm_tag,gn);
#else
  SCM_NEWCELL(new_src);
  SCM_SETCDR(new_src,(SCM)gn);
  SCM_SETCAR(new_src,mus_scm_tag);
  return(new_src);
#endif
}

static void init_sr(void)
{
  DEFINE_PROC(gh_new_procedure(S_src_p,SCM_FNC g_src_p,1,0,0),H_src_p);
  DEFINE_PROC(gh_new_procedure(S_src,SCM_FNC g_src,1,2,0),H_src);
  DEFINE_PROC(gh_new_procedure(S_make_src,SCM_FNC g_make_src,0,6,0),H_make_src);
}


/* ---------------- granulate ---------------- */

#define S_granulate_p    "granulate?"
#define S_granulate      "granulate"
#define S_make_granulate "make-granulate"
#define S_mus_ramp           "mus-ramp"
#define S_mus_set_ramp       "mus-set-ramp"
#define S_mus_hop            "mus-hop"
#define S_mus_set_hop        "mus-set-hop"

static SCM g_granulate_p(SCM obj) 
{
  #define H_granulate_p "(" S_granulate_p " gen) -> #t if gen is a " S_granulate " generator, else #f"
  return(((mus_scm_p(obj)) && (mus_granulate_p(mus_get_any(obj)))) ? SCM_BOOL_T : SCM_BOOL_F);
}

static SCM g_granulate(SCM obj, SCM func) 
{
  #define H_granulate "(" S_granulate " gen) -> next sample from granular synthesis generator"
  mus_scm *gn = mus_get_scm(obj);
  SCM_ASSERT(((mus_scm_p(obj)) && (mus_granulate_p(mus_get_any(obj)))),obj,SCM_ARG1,S_granulate);
  if (gh_procedure_p(func)) gn->vcts[0] = func;
  return(gh_double2scm(mus_granulate(mus_get_any(obj),0)));
}

static SCM g_ramp(SCM obj)
{
  #define H_mus_ramp "(" S_mus_ramp " gen) -> (granulate generator) gen's " S_mus_ramp " field"
  SCM_ASSERT(((mus_scm_p(obj)) && (mus_granulate_p(mus_get_any(obj)))),obj,SCM_ARG1,S_mus_ramp);
  return(gh_int2scm(mus_ramp(mus_get_any(obj))));
}

static SCM g_set_ramp(SCM obj, SCM val)
{
  #define H_mus_set_ramp "(" S_mus_set_ramp " gen val) sets (granulate generator) gen's " S_mus_ramp " field"
  SCM_ASSERT(((mus_scm_p(obj)) && (mus_granulate_p(mus_get_any(obj)))),obj,SCM_ARG1,S_mus_set_ramp);
  SCM_ASSERT((SCM_NFALSEP(scm_real_p(val))),val,SCM_ARG2,S_mus_set_ramp);
  return(gh_int2scm(mus_set_ramp(mus_get_any(obj),g_scm2int(val))));
}

static SCM g_hop(SCM obj)
{
  #define H_mus_hop "(" S_mus_hop " gen) -> (granulate generator) gen's " S_mus_hop " field"
  SCM_ASSERT(((mus_scm_p(obj)) && (mus_granulate_p(mus_get_any(obj)))),obj,SCM_ARG1,S_mus_hop);
  return(gh_int2scm(mus_hop(mus_get_any(obj))));
}

static SCM g_set_hop(SCM obj, SCM val)
{
  #define H_mus_set_hop "(" S_mus_set_hop " gen val) sets (granulate generator) gen's " S_mus_hop " field"
  SCM_ASSERT(((mus_scm_p(obj)) && (mus_granulate_p(mus_get_any(obj)))),obj,SCM_ARG1,S_mus_set_hop);
  SCM_ASSERT((SCM_NFALSEP(scm_real_p(val))),val,SCM_ARG2,S_mus_set_hop);
  return(gh_int2scm(mus_set_hop(mus_get_any(obj),g_scm2int(val))));
}

static SCM g_make_granulate(SCM arglist)
{
  #define H_make_granulate "(" S_make_granulate " &opt-key input (expansion 1.0) (length .15)\n\
       (scaler .6) (hop .05) (ramp .4) (jitter 1.0) max-size) returns a new granular synthesis\n\
   generator.  'length' is the grain length (seconds), 'expansion' is the ratio in timing\n\
   between the new and old (expansion > 1.0 slows things down), 'scaler' scales the grains\n\
   to avoid overflows, 'hop' is the spacing (seconds) between successive grains upon output\n\
   jitter controls the randomness in that spacing, input can be a file pointer."

#if HAVE_GUILE_1_3_0
  SCM new_e;
#endif
  SCM in_obj = SCM_UNDEFINED;
  mus_scm *gn;
  SCM args[16],keys[8];
  int orig_arg[8] = {0,0,0,0,0,0,0,0};
  int vals,i,arglist_len,maxsize = 0;
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
  for (i=0;i<16;i++) args[i] = SCM_UNDEFINED;
  arglist_len = gh_length(arglist);
  for (i=0;i<arglist_len;i++) args[i] = gh_list_ref(arglist,gh_int2scm(i));
  vals = decode_keywords(S_make_granulate,8,keys,16,args,orig_arg);
  if (vals > 0)
    {
      if (!(keyword_p(keys[0]))) 
	{
	  if (gh_procedure_p(keys[0]))
	    in_obj = keys[0];
	  else scm_wrong_type_arg(S_make_granulate,orig_arg[0]+1,args[orig_arg[0]]);
	}
      expansion = fkeyarg(keys[1],S_make_granulate,orig_arg[1]+1,args[orig_arg[1]],expansion);
      segment_length = fkeyarg(keys[2],S_make_granulate,orig_arg[2]+1,args[orig_arg[2]],segment_length);
      segment_scaler = fkeyarg(keys[3],S_make_granulate,orig_arg[3]+1,args[orig_arg[3]],segment_scaler);
      output_hop = fkeyarg(keys[4],S_make_granulate,orig_arg[4]+1,args[orig_arg[4]],output_hop);
      ramp_time = fkeyarg(keys[5],S_make_granulate,orig_arg[5]+1,args[orig_arg[5]],ramp_time);
      jitter = fkeyarg(keys[6],S_make_granulate,orig_arg[6]+1,args[orig_arg[6]],jitter);
      maxsize = ikeyarg(keys[7],S_make_granulate,orig_arg[7]+1,args[orig_arg[7]],maxsize);
    }
  gn = (mus_scm *)CALLOC(1,sizeof(mus_scm));
  gn->vcts = (SCM *)CALLOC(1,sizeof(SCM));
  if (!(SCM_UNBNDP(in_obj))) gn->vcts[0] = in_obj; else gn->vcts[0] = SCM_EOL;
  gn->nvcts = 1;
  gn->gen = mus_make_granulate(funcall_reader,expansion,segment_length,segment_scaler,output_hop,ramp_time,jitter,maxsize,gn);
#if (!HAVE_GUILE_1_3_0)
  SCM_RETURN_NEWSMOB(mus_scm_tag,gn);
#else
  SCM_NEWCELL(new_e);
  SCM_SETCDR(new_e,(SCM)gn);
  SCM_SETCAR(new_e,mus_scm_tag);
  return(new_e);
#endif
}

static void init_spd(void)
{
  DEFINE_PROC(gh_new_procedure(S_granulate_p,SCM_FNC g_granulate_p,1,0,0),H_granulate_p);
  DEFINE_PROC(gh_new_procedure(S_granulate,SCM_FNC g_granulate,1,1,0),H_granulate);
  DEFINE_PROC(gh_new_procedure(S_mus_ramp,SCM_FNC g_ramp,1,0,0),H_mus_ramp);
  DEFINE_PROC(gh_new_procedure(S_mus_set_ramp,SCM_FNC g_set_ramp,2,0,0),H_mus_set_ramp);
  DEFINE_PROC(gh_new_procedure(S_mus_hop,SCM_FNC g_hop,1,0,0),H_mus_hop);
  DEFINE_PROC(gh_new_procedure(S_mus_set_hop,SCM_FNC g_set_hop,2,0,0),H_mus_set_hop);
  DEFINE_PROC(gh_new_procedure(S_make_granulate,SCM_FNC g_make_granulate,0,0,1),H_make_granulate);
}



/* ---------------- convolve ---------------- */

#define S_convolve_p     "convolve?"
#define S_convolve       "convolve"
#define S_make_convolve  "make-convolve"
#define S_convolve_files "convolve-files"

static SCM g_convolve_p(SCM obj) 
{
  #define H_convolve_p "(" S_convolve_p " gen) -> #t if gen is a " S_convolve " generator, else #f"
  return(((mus_scm_p(obj)) && (mus_convolve_p(mus_get_any(obj)))) ? SCM_BOOL_T : SCM_BOOL_F);
}

static SCM g_convolve(SCM obj, SCM func) 
{
  #define H_convolve "(" S_convolve " gen &optional input-func) -> next sample from convolution generator"
  mus_scm *gn = mus_get_scm(obj);
  SCM_ASSERT(((mus_scm_p(obj)) && (mus_convolve_p(mus_get_any(obj)))),obj,SCM_ARG1,S_convolve);
  if (gh_procedure_p(func)) gn->vcts[0] = func;
  return(gh_double2scm(mus_convolve(mus_get_any(obj),0)));
}

/* filter-size? */

static SCM g_make_convolve(SCM arglist)
{
  #define H_make_convolve "(" S_make_convolve " &opt-key input filter fft-size) returns\n\
   a new convolution generator which convolves its input with the impulse response 'filter'."

#if HAVE_GUILE_1_3_0
  SCM new_e;
#endif
  mus_scm *gn;
  SCM args[6],keys[3];
  int orig_arg[3] = {0,0,0};
  int vals,i,arglist_len,fftlen;
  vct *filter=NULL;
  SCM filt = SCM_UNDEFINED,in_obj = SCM_UNDEFINED;
  int fft_size = 0;
  keys[0] = all_keys[C_input];
  keys[1] = all_keys[C_filter];
  keys[2] = all_keys[C_fft_size];
  for (i=0;i<6;i++) args[i] = SCM_UNDEFINED;
  arglist_len = gh_length(arglist);
  for (i=0;i<arglist_len;i++) args[i] = gh_list_ref(arglist,gh_int2scm(i));
  vals = decode_keywords(S_make_convolve,3,keys,6,args,orig_arg);
  if (vals > 0)
    {
      if (!(keyword_p(keys[0]))) 
	{
	  if (gh_procedure_p(keys[0]))
	    in_obj = keys[0];
	  else scm_wrong_type_arg(S_make_convolve,orig_arg[0]+1,args[orig_arg[0]]);
	}
      if (!(keyword_p(keys[1]))) 
	{
	  if (vct_p(keys[1]))
	    {
	      filt = keys[1];
	      filter = get_vct(filt);
	    }
          else scm_wrong_type_arg(S_make_convolve,orig_arg[1]+1,args[orig_arg[1]]);
	}
      fft_size = ikeyarg(keys[2],S_make_convolve,orig_arg[2]+1,args[orig_arg[2]],fft_size);
    }
  fftlen = (int)pow(2.0,1 + (int)ceil(log((Float)(filter->length))/log(2.0)));
  if (fft_size < fftlen) fft_size = fftlen;
  gn = (mus_scm *)CALLOC(1,sizeof(mus_scm));
  gn->nvcts = 2;
  gn->vcts = (SCM *)CALLOC(2,sizeof(SCM));
  if (!(SCM_UNBNDP(in_obj))) gn->vcts[0] = in_obj; else gn->vcts[0] = SCM_EOL;
  gn->vcts[1] = filt;
  gn->gen = mus_make_convolve(funcall_reader,filter->data,fft_size,filter->length,gn);
#if (!HAVE_GUILE_1_3_0)
  SCM_RETURN_NEWSMOB(mus_scm_tag,gn);
#else
  SCM_NEWCELL(new_e);
  SCM_SETCDR(new_e,(SCM)gn);
  SCM_SETCAR(new_e,mus_scm_tag);
  return(new_e);
#endif
}

static SCM g_convolve_files(SCM file1, SCM file2, SCM maxamp, SCM outfile)
{
  #define H_convolve_files "(" S_convolve_files " file1 file2 maxamp output-file) convolves\n\
   file1 and file2 writing outfile after scaling the convolution result to maxamp."

  char *f1,*f2,*f3;
  Float maxval=1.0;
  SCM_ASSERT((gh_string_p(file1)),file1,SCM_ARG1,S_convolve_files);
  SCM_ASSERT((gh_string_p(file2)),file2,SCM_ARG2,S_convolve_files);
  SCM_ASSERT((SCM_UNBNDP(maxamp)) || (SCM_NFALSEP(scm_real_p(maxamp))),maxamp,SCM_ARG3,S_convolve_files);
  SCM_ASSERT((SCM_UNBNDP(outfile)) || (gh_string_p(outfile)),outfile,SCM_ARG4,S_convolve_files);
  f1 = gh_scm2newstr(file1,NULL);
  f2 = gh_scm2newstr(file2,NULL);
  if (gh_string_p(outfile)) f3 = gh_scm2newstr(outfile,NULL); else f3 = "tmp.snd";
  if (SCM_NFALSEP(scm_real_p(maxamp))) maxval = gh_scm2double(maxamp);
  mus_convolve_files(f1,f2,maxval,f3);
  free(f1);
  free(f2);
  if (gh_string_p(outfile)) free(f3);
  return(SCM_BOOL_F);
}

static void init_conv(void)
{
  DEFINE_PROC(gh_new_procedure(S_convolve_p,SCM_FNC g_convolve_p,1,0,0),H_convolve_p);
  DEFINE_PROC(gh_new_procedure(S_convolve,SCM_FNC g_convolve,1,1,0),H_convolve);
  DEFINE_PROC(gh_new_procedure(S_make_convolve,SCM_FNC g_make_convolve,0,0,1),H_make_convolve);
  DEFINE_PROC(gh_new_procedure(S_convolve_files,SCM_FNC g_convolve_files,2,2,0),H_convolve_files);
}


/* ---------------- mix ---------------- */

#define S_mus_mix "mus-mix"

static SCM g_mus_mix(SCM out, SCM in, SCM ost, SCM olen, SCM ist, SCM mx, SCM envs)
{
  #define H_mus_mix "(" S_mus_mix " outfile infile (outloc 0) frames (inloc 0) mixer envs)\n\
    mixes infile into outfile starting at outloc in outfile and inloc in infile\n\
    mixing frames frames of infile.  frames defaults to the length of infile. If mixer,\n\
    use it to scale the various channels; if envs (an array of envelope generators), use\n\
    it in conjunction with mixer to scale/envelope all the various ins and outs."

  mus_mixer *mx1 = NULL;
  mus_any ***envs1 = NULL;
  char *outfile = NULL,*infile = NULL;
  int in_len=0,out_len,i,j,ostart = 0,istart = 0,osamps = 0;
  SCM_ASSERT((gh_string_p(out)),out,SCM_ARG1,S_mus_mix);
  SCM_ASSERT((gh_string_p(in)),in,SCM_ARG2,S_mus_mix);
  SCM_ASSERT((SCM_UNBNDP(ost)) || (SCM_NFALSEP(scm_real_p(ost))),ost,SCM_ARG3,S_mus_mix);
  SCM_ASSERT((SCM_UNBNDP(olen)) || (SCM_NFALSEP(scm_real_p(olen))),olen,SCM_ARG4,S_mus_mix);
  SCM_ASSERT((SCM_UNBNDP(ist)) || (SCM_NFALSEP(scm_real_p(ist))),ist,SCM_ARG5,S_mus_mix);
  SCM_ASSERT((SCM_UNBNDP(mx)) || ((mus_scm_p(mx)) && (mus_mixer_p(mus_get_any(mx)))),mx,SCM_ARG6,S_mus_mix);
  SCM_ASSERT((SCM_UNBNDP(envs)) || (gh_vector_p(envs)),envs,SCM_ARG7,S_mus_mix);
  if (!(SCM_UNBNDP(ost))) ostart = g_scm2int(ost);
  if (!(SCM_UNBNDP(ist))) istart = g_scm2int(ist);
  if (!(SCM_UNBNDP(mx))) mx1 = (mus_mixer *)mus_get_any(mx);
  if (!(SCM_UNBNDP(envs)))
    {
      /* pack into a C-style array of arrays of env pointers */
      in_len = gh_vector_length(envs);
      out_len = gh_vector_length(gh_vector_ref(envs,gh_int2scm(0)));
      envs1 = (mus_any ***)CALLOC(in_len,sizeof(mus_any **));
      for (i=0;i<in_len;i++)
	{
	  envs1[i] = (mus_any **)CALLOC(out_len,sizeof(mus_any *));
	  for (j=0;j<out_len;j++) 
	    envs1[i][j] = mus_get_any(gh_vector_ref(gh_vector_ref(envs,gh_int2scm(i)),gh_int2scm(j)));
	}
    }
  outfile = gh_scm2newstr(out,NULL);
  infile = gh_scm2newstr(in,NULL);
#if HAVE_SNDLIB
  if (!(SCM_UNBNDP(olen))) osamps = g_scm2int(olen); else osamps = mus_sound_frames(infile);
#else
  osamps = g_scm2int(olen);
#endif
  mus_mix(outfile,infile,ostart,osamps,istart,mx1,envs1);
  if (outfile) free(outfile);
  if (infile) free(infile);
  if (envs1) 
    {
      for (i=0;i<in_len;i++) if (envs1[i]) FREE(envs1[i]);
      FREE(envs1);
    }
  return(SCM_BOOL_T);
}

void init_mus2scm_module(void)
{
  local_doc = scm_permanent_object(scm_string_to_symbol(gh_str02scm("documentation")));

  init_mus_module();
  init_mus_scm();
  mus_error_set_handler(mus_error2scm);
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
  init_spd();

  DEFINE_PROC(gh_new_procedure(S_mus_mix,SCM_FNC g_mus_mix,2,5,0),H_mus_mix);

  /* this next code implements (setf (mus-frequency gen) val) constructs */
  /* it is from the guile mailing list, written by Maciej Stachowiak <mstachow@mit.edu> */
  gh_eval_str("(defmacro setf (place value) (if (pair? place) `((setter ,(car place)) ,@(cdr place) ,value) `(set! place value)))\n\
               (define (setter proc) (procedure-property proc 'setter))\n\
               (set-procedure-property! setter 'setter (lambda (proc setter) (set-procedure-property! proc 'setter setter)))\n\
\n\
               (setf (setter " S_mus_phase ") " S_mus_set_phase ")\n\
               (setf (setter " S_mus_data ") " S_mus_set_data ")\n\
               (setf (setter " S_mus_length ") " S_mus_set_length ")\n\
               (setf (setter " S_mus_frequency ") " S_mus_set_frequency ")\n\
               (setf (setter " S_mus_scaler ") " S_mus_set_scaler ")\n\
               (setf (setter " S_mus_a0 ") " S_mus_set_a0 ")\n\
               (setf (setter " S_mus_a1 ") " S_mus_set_a1 ")\n\
               (setf (setter " S_mus_a2 ") " S_mus_set_a2 ")\n\
               (setf (setter " S_mus_b1 ") " S_mus_set_b1 ")\n\
               (setf (setter " S_mus_b2 ") " S_mus_set_b2 ")\n\
               (setf (setter " S_mus_formant_radius ") " S_mus_set_formant_radius ")\n\
               (setf (setter " S_mus_feedback ") " S_mus_set_feedback ")\n\
               (setf (setter " S_mus_feedforward ") " S_mus_set_feedforward ")\n\
               (setf (setter " S_mus_location ") " S_mus_set_location ")\n\
               (setf (setter " S_mus_increment ") " S_mus_set_increment ")\n\
               (setf (setter " S_mus_ramp ") " S_mus_set_ramp ")\n\
               (setf (setter " S_mus_hop ") " S_mus_set_hop ")\n\
               (setf (setter " S_frame_ref ") " S_frame_set ")\n\
               (setf (setter " S_mixer_ref ") " S_mixer_set ")\n\
               (setf (setter " S_locsig_ref ") " S_locsig_set ")\n\
               (setf (setter " S_locsig_reverb_ref ") " S_locsig_reverb_set ")\n\
\n\
");

  scm_add_feature("clm");

}


/* (definstrument a (arg ...) (let (... (e (make-env))) (... (run (loop ... (env e) (outa ...))))))
 *   return closure of outer let and run as appliable func?
 *   could all CL-compatibility stuff be local to definstrument?  (i.e. first for car)
 *
 *  (let (...) (let (beg end) (init envs) (loop...)))
 *
 *  (with-sound (...) ...) -> current chans, create if none?
 *  sampling_rate from :srate
 *  with-sound would need fluid-let as outer
 *
 * here is something close to with-sound body handling:
 * (defmacro with-fluids (bindings . body)
 *   `(with-fluids* (list ,@(map car bindings)) (list ,@(map cadr bindings))
 *      (lambda () ,@body)))
 * 
 * (defmacro with-sound (bindings . body)
 *   `(with-sound-internal (fluid-let ,bindings . ,body)))
 */


#define NUM_CLM_NAMES 242
static char *clm_names[NUM_CLM_NAMES] = {
S_all_pass,S_all_pass_p,S_amplitude_modulate,S_array2file,S_array_interp,S_asymmetric_fm,S_asymmetric_fm_p,S_bartlett_window,
S_blackman2_window,S_blackman3_window,S_blackman4_window,S_buffer2frame,S_buffer2sample,S_buffer_empty_p,S_buffer_full_p,
S_buffer_p,S_cauchy_window,S_clear_array,S_comb,S_comb_p,S_contrast_enhancement,S_convolution,S_convolve,S_convolve_files,
S_convolve_p,S_db_linear,S_degrees_radians,S_delay,S_delay_p,S_dot_product,S_env,S_env_interp,S_env_p,S_exponential_window,
S_file2array,S_file2frame,S_file2frame_p,S_file2sample,S_file2sample_p,S_filter,S_filter_p,S_fir_filter,S_fir_filter_p,S_formant,
S_formant_bank,S_formant_p,S_frame_multiply,S_frame_add,S_frame2buffer,S_frame2file,S_frame2file_p,S_frame2frame,S_frame2list,
S_frame2sample,S_frame_ref,S_frame_set,S_frame_p,S_gaussian_window,S_granulate,S_granulate_p,S_hamming_window,S_hanning_window,
S_hz_radians,S_iir_filter,S_iir_filter_p,S_in_any,S_in_hz,S_ina,S_inb,S_kaiser_window,S_linear_db,S_locsig,S_locsig_ref,S_locsig_reverb_ref,
S_locsig_reverb_set,S_locsig_set,S_locsig_p,S_make_all_pass,S_make_asymmetric_fm,S_make_buffer,S_make_comb,S_make_convolve,
S_make_delay,S_make_env,S_make_fft_window,S_make_file2frame,S_make_file2sample,S_make_filter,S_make_fir_filter,S_make_formant,
S_make_frame,S_make_frame2file,S_make_granulate,S_make_iir_filter,S_make_locsig,S_make_mixer,S_make_notch,S_make_one_pole,
S_make_one_zero,S_make_oscil,S_make_ppolar,S_make_pulse_train,S_make_rand,S_make_rand_interp,S_make_readin,S_make_sample2file,
S_make_sawtooth_wave,S_make_sine_summation,S_make_square_wave,S_make_src,S_make_sum_of_cosines,S_make_table_lookup,S_make_triangle_wave,
S_make_two_pole,S_make_two_zero,S_make_wave_train,S_make_waveshape,S_make_zpolar,S_mixer_multiply,S_mixer_ref,S_mixer_set,
S_mixer_p,S_multiply_arrays,S_mus_a0,S_mus_a1,S_mus_a2,S_mus_array_print_length,S_mus_b1,S_mus_b2,S_mus_channel,S_mus_channels,
S_mus_close,S_mus_cosines,S_mus_data,S_mus_describe,S_mus_feedback,S_mus_feedforward,S_mus_fft,S_mus_formant_radius,S_mus_frequency,
S_mus_hop,S_mus_increment,S_mus_input_p,S_mus_inspect,S_mus_length,S_mus_location,S_mus_mix,S_mus_order,S_mus_output_p,S_mus_phase,
S_mus_ramp,S_mus_random,S_mus_scaler,S_mus_set_a0,S_mus_set_a1,S_mus_set_a2,S_mus_set_array_print_length,S_mus_set_b1,S_mus_set_b2,
S_mus_set_data,S_mus_set_feedback,S_mus_set_feedforward,S_mus_set_formant_radius,S_mus_set_frequency,S_mus_set_hop,S_mus_set_increment,
S_mus_set_length,S_mus_set_location,S_mus_set_phase,S_mus_set_ramp,S_mus_set_rand_seed,S_mus_set_scaler,S_mus_set_srate,S_mus_srate,
S_mus_xcoeffs,S_mus_ycoeffs,S_notch,S_notch_p,S_one_pole,S_one_pole_p,S_one_zero,S_one_zero_p,S_oscil,S_oscil_bank,S_oscil_p,
S_out_any,S_outa,S_outb,S_outc,S_outd,S_partials2polynomial,S_partials2wave,S_partials2waveshape,S_parzen_window,S_phasepartials2wave,
S_poisson_window,S_polynomial,S_pulse_train,S_pulse_train_p,S_radians_degrees,S_radians_hz,S_rand,S_rand_interp,S_rand_interp_p,S_rand_p,
S_readin,S_readin_p,S_rectangular2polar,S_rectangular_window,S_restart_env,S_riemann_window,S_ring_modulate,S_sample2buffer,S_sample2file,
S_sample2file_p,S_sample2frame,S_sawtooth_wave,S_sawtooth_wave_p,S_sine_summation,S_sine_summation_p,S_spectrum,S_square_wave,
S_square_wave_p,S_src,S_src_p,S_sum_of_cosines,S_sum_of_cosines_p,S_table_lookup,S_table_lookup_p,S_tap,S_triangle_wave,S_triangle_wave_p,
S_tukey_window,S_two_pole,S_two_pole_p,S_two_zero,S_two_zero_p,S_wave_train,S_wave_train_p,S_waveshape,S_waveshape_p,S_welch_window
};

int mus_num_commands(void);
char **mus_commands(void);

int mus_num_commands(void) {return(NUM_CLM_NAMES);}
char **mus_commands(void) {return(clm_names);}

static char CLM_help_string[] = 
"  all-pass            (gen input pm)       all-pass filter\n\
  all-pass?           (gen)                #t if gen is all-pass filter\n\
  amplitude-modulate  (carrier in1 in2)    amplitude modulation\n\
  array-interp        (arr x)              interpolated array lookup\n\
  array->file         (filename vct len srate channels)\n\
  asymmetric-fm       (gen index fm)       asymmetric-fm generator\n\
  asymmetric-fm?      (gen)                #t if gen is asymmetric-fm generator\n\
  buffer->frame       (gen frame           buffer generator returning frame\n\
  buffer->sample      (gen)                buffer generator returning sample\n\
  buffer-empty?       (gen)                #t if buffer has no data\n\
  buffer?             (gen)                #t if gen is buffer generator\n\
  clear-array         (arr)                set all elements of arr to 0.0\n\
  comb                (gen input pm)       comb filter\n\
  comb?               (gen)                #t if gen is comb filter\n\
  contrast-enhancement(input (index 1.0))  a kind of phase modulation\n\
  convolution         (sig1 sig2 n)        convolve sig1 with sig2 (size n), returning new sig1\n\
  convolve            (gen input-function) convolve generator\n\
  convolve?           (gen)                #t if gen is convolve generator\n\
  convolve-files      (f1 f2 maxamp outf)  convolve f1 with f2, normalize to maxamp, write outf\n\
  db->linear          (db)                 translate dB value to linear\n\
  degrees->radians    (deg)                translate degrees to radians\n\
  delay               (gen input pm)       delay line\n\
  delay?              (gen)                #t if gen is delay line\n\
  dot-product         (sig1 sig2)          return dot-product of sig1 with sig2\n\
  env                 (gen)                envelope generator\n\
  env-interp          (x env (base 1.0))   return value of env at x\n\
  env?                (gen)                #t if gen is env (from make-env)\n\
  mus-fft             (rl im n sign)       fft of rl and im (sign = -1 for ifft), result in rl\n\
  file->array         (filename chan start len vct)\n\
  file->frame         (gen loc frame)      return frame from file at loc\n\
  file->frame?        (gen)                #t if gen is file->frame generator\n\
  file->sample        (gen loc chan)       return sample from file at loc\n\
  file->sample?       (gen)                #t if gen is file->sample generator\n\
  filter              (gen input)          filter\n\
  filter?             (gen)                #t if gen is filter\n\
  fir-filter          (gen input)          FIR filter\n\
  fir-filter?         (gen)                #t if gen is fir filter\n\
  formant             (gen input)          formant generator\n\
  formant-bank        (scls gens invals)   bank for formants\n\
  formant?            (gen)                #t if gen is formant generator\n\
  frame*              (fr1 fr2 outfr)      element-wise multiply\n\
  frame+              (fr1 fr2 outfr)      element-wise add\n\
  frame->buffer       (buf frame)          add frame to buffer\n\
  frame->file         (gen loc frame)      write (add) frame to file at loc\n\
  frame->file?        (gen)                #t if gen is frame->file generator\n\
  frame->frame        (mixer frame outfr)  pass frame through mixer\n\
  frame->list         (frame)              return list of  frame's contents\n\
  frame-ref           (frame chan)         return frame[chan]\n\
  frame->sample       (frmix frame)        pass frame through frame or mixer to produce sample\n\
  frame-set!          (frame chan val)     frame[chan]=val\n\
  frame?              (gen)                #t if gen is frame object\n\
  granulate           (gen input-function) granular synthesis generator\n\
  granulate?          (gen)                #t if gen is granulate generator\n\
  hz->radians         (freq)               translate freq to radians/sample\n\
  iir-filter          (gen input)          IIR filter\n\
  iir-filter?         (gen)                #t if gen is iir-filter\n\
  in-any              (loc chan stream)    return sample in stream at loc and chan\n\
  in-hz               (freq)               translate freq to radians/sample\n\
  ina                 (loc stream)         return sample in stream at loc, chan 0\n\
  inb                 (loc stream)         return sample in stream at loc, chan 1\n\
  linear->db          (val)                translate linear val to dB\n\
  locsig              (gen loc input)      place input in output channels at loc\n\
  locsig-ref          (gen chan)           locsig-scaler[chan]\n\
  locsig-reverb-ref   (gen chan)           locsig-reverb-scaler[chan]\n\
  locsig-set!         (gen chan val)       locsig-scaler[chan] = val\n\
  locsig-reverb-set!  (gen chan val)       locsig-reverb-scaler[chan] = val\n\
  locsig?             (gen)                #t if gen is locsig generator\n\
  ;; all the make function arguments are optional-key args\n\
  make-all-pass       (feedback feedforward size max-size initial-contents initial-element)\n\
  make-asymmetric-fm  (frequency initial-phase r ratio)\n\
  make-buffer         (size fill-time)\n\
  make-comb           (scaler size max-size initial-contents initial-element)\n\
  make-convolve       (input filter fft-size)\n\
  make-delay          (size initial-contents initial-element max-size)\n\
  make-env            (envelope scaler duration offset base end start)\n\
  make-fft-window     (type size)\n\
  make-file->frame    (name)\n\
  make-file->sample   (name)\n\
  make-filter         (order xcoeffs ycoeffs)\n\
  make-fir-filter     (order xcoeffs)\n\
  make-formant        (radius frequency gain)\n\
  make-frame          (chans &rest vals)\n\
  make-frame->file    (name chans format type)\n\
  make-granulate      (input expansion length scaler hop ramp jitter max-size)\n\
  make-iir-filter     (order ycoeffs)\n\
  make-locsig         (degree distance reverb output revout channels)\n\
  make-mixer          (chans &rest vals)\n\
  make-notch          (scaler size max-size initial-contents initial-element)\n\
  make-one-pole       (a0 b1)\n\
  make-one-zero       (a0 a1)\n\
  make-oscil          (frequency initial-phase)\n\
  make-ppolar         (radius frequency)\n\
  make-pulse-train    (frequency amplitude initial-phase)\n\
  make-rand           (frequency amplitude)\n\
  make-rand-interp    (frequency amplitude)\n\
  make-readin         (file channel start)\n\
  make-sample->file   (name chans format type)\n\
  make-sawtooth-wave  (frequency amplitude initial-phase)\n\
  make-sine-summation (frequency initial-phase n a ratio)\n\
  make-square-wave    (frequency amplitude initial-phase)\n\
  make-src            (input srate width)\n\
  make-sum-of-cosines (frequency initial-phase cosines)\n\
  make-table-lookup   (frequency initial-phase wave)\n\
  make-triangle-wave  (frequency amplitude initial-phase)\n\
  make-two-pole       (a0 b1 b2)\n\
  make-two-zero       (a0 a1 a2)\n\
  make-wave-train     (frequency initial-phase wave)\n\
  make-waveshape      (frequency partials)\n\
  make-zpolar         (radius frequency)\n\
  mixer*              (mix1 mix2 outmx)    matrix multiply of mix1 and mix2\n\
  mixer-ref           (mix in out)         mix-scaler[in,out]\n\
  mixer-set!          (mix in out val)     mix-scaler[in,out] = val\n\
  mixer?              (gen)                #t if gen is mixer object\n\
  multiply-arrays     (arr1 arr2)          arr1[i] *= arr2[i]\n\
  ;; the \"mus-\" functions are generic functions, to set use mus-set-var as in mus-set-frequency\n\
  mus-a0              (gen)                a0 field (simple filters)\n\
  mus-a1              (gen)                a1 field (simple filters)\n\
  mus-a2              (gen)                a2 field (simple filters)\n\
  mus-array-print-length ()                how many array elements to print in mus_describe\n\
  mus-b1              (gen)                b1 field (simple filters)\n\
  mus-b2              (gen)                b2 field (simple filters)\n\
  mus-channel         (gen)                channel of gen\n\
  mus-channels        (gen)                channels of gen\n\
  mus-cosines         (gen)                cosines of sum-of-cosines gen\n\
  mus-data            (gen)                data array of gen\n\
  mus-feedback        (gen)                feedback term of gen (simple filters)\n\
  mus-feedforward     (gen)                feedforward term of gen (all-pass)\n\
  mus-formant-radius  (gen)                formant radius\n\
  mus-frequency       (gen)                frequency of gen (Hz)\n\
  mus-hop             (gen)                hop amount of gen (granulate)\n\
  mus-increment       (gen)                increment of gen (src, readin, granulate)\n\
  mus-input?          (gen)                #t if gen is input source\n\
  mus-length          (gen)                length of gen\n\
  mus-location        (gen)                location (read point) of gen\n\
  mus-mix             (outfile infile outloc frames inloc mixer envs)\n\
  mus-order           (gen)                order of gen (filters)\n\
  mus-output?         (gen)                #t if gen is output generator\n\
  mus-phase           (gen)                phase of gen (radians)\n\
  mus-ramp            (gen)                ramp time of gen (granulate)\n\
  mus-random          (val)                random numbers bewteen -val and val\n\
  mus-scaler          (gen)                scaler of gen\n\
  mus-set-rand-seed   (val)                set random number generator seed to val\n\
  mus-set-srate       (val)                set sampling rate to val\n\
  mus-srate           ()                   current sampling rate\n\
  mus-xcoeffs         (gen)                feedforward (FIR) coeffs of filter\n\
  mus-ycoeffs         (gen)                feedback (IIR) coeefs of filter\n\
  notch               (gen input pm)       notch filter\n\
  notch?              (gen)                #t if gen is notch filter\n\
  one-pole            (gen input)          one-pole filter\n\
  one-pole?           (gen)                #t if gen is one-pole filter\n\
  one-zero            (gen input)          one-zero filter\n\
  one-zero?           (gen)                #t if gen is one-zero filter\n\
  oscil               (gen fm pm)          sine wave generator\n\
  oscil-bank          (scls gens invals)   bank for oscils\n\
  oscil?              (gen)                #t if gen is oscil generator\n\
  out-any             (loc samp chan stream) write (add) samp to stream at loc in channel chan\n\
  outa                (loc samp stream)    write (add) samp to stream at loc in chan 0\n\
  outb                (loc samp stream)    write (add) samp to stream at loc in chan 1\n\
  outc                (loc samp stream)    write (add) samp to stream at loc in chan 2\n\
  outd                (loc samp stream)    write (add) samp to stream at loc in chan 3\n\
  partials->polynomial(partials kind)      create waveshaping polynomial from partials\n\
  partials->wave      (synth-data table norm) load table from synth-data\n\
  partials->waveshape (partials norm size) create waveshaping table from partials\n\
  phase-partials->wave(synth-data table norm) load table from synth-data\n\
  polynomial          (coeffs x)           evaluate polynomial at x\n\
  pulse-train         (gen fm)             pulse-train generator\n\
  pulse-train?        (gen)                #t if gen is pulse-train generator\n\
  radians->degrees    (rads)               convert radians to degrees\n\
  radians->hz         (rads)               convert radians/sample to Hz\n\
  rand                (gen fm)             random number generator\n\
  rand-interp         (gen fm)             interpolating random number generator\n\
  rand-interp?        (gen)                #t if gen is interpolating random number generator\n\
  rand?               (gen)                #t if gen is random number generator\n\
  readin              (gen)                read one value from associated input stream\n\
  readin?             (gen)                #t if gen is readin generator\n\
  rectangular->polar  (rl im)              translate from rectangular to polar coordinates\n\
  restart-env         (env)                return to start of env\n\
  ring-modulate       (sig1 sig2)          sig1 * sig2 (element-wise)\n\
  sample->buffer      (buf samp)           store samp in buffer\n\
  sample->file        (gen loc chan val)   store val in file at loc in channel chan\n\
  sample->file?       (gen)                #t if gen is sample->file generator\n\
  sample->frame       (frmix samp outfr)   convert samp to frame\n\
  sawtooth-wave       (gen fm)             sawtooth-wave generator\n\
  sawtooth-wave?      (gen)                #t if gen is sawtooth-wave generator\n\
  sine-summation      (gen fm)             sine-summation generator\n\
  sine-summation?     (gen)                #t if gen is sine-summation generator\n\
  spectrum            (rl im win type)     produce spectrum of data in rl\n\
  square-wave         (gen fm)             square-wave generator\n\
  square-wave?        (gen)                #t if gen is square-wave generator\n\
  src                 (gen fm input-function) sample rate converter\n\
  src?                (gen)                #t if gen is sample-rate converter\n\
  sum-of-cosines      (gen fm)             sum-of-cosines (pulse-train) generator\n\
  sum-of-cosines?     (gen)                #t if gen is sum-of-cosines generator\n\
  table-lookup        (gen fm)             table-lookup generator\n\
  table-lookup?       (gen)                #t if gen is table-lookup generator\n\
  tap                 (gen pm)             delay line tap\n\
  triangle-wave       (gen fm)             triangle-wave generator\n\
  triangle-wave?      (gen)                #t if gen is triangle-wave generator\n\
  two-pole            (gen input)          two-pole filter\n\
  two-pole?           (gen)                #t if gen is two-pole filter\n\
  two-zero            (gen input)          two-zero filter\n\
  two-zero?           (gen)                #t if gen is two-zero filter\n\
  wave-train          (gen fm)             wave-train generator\n\
  wave-train?         (gen)                #t if gen is wave-train generator\n\
  waveshape           (gen index fm)       waveshaping generator\n\
  waveshape?          (gen)                #t if gen is waveshape generator\n\
";

char *CLM_help(void);
char *CLM_help(void)
{
  return(CLM_help_string);
}
#endif

/*
void scm_init_sndlib_clm_module ()
{
  scm_register_module_xxx("sndlib clm",init_mus2scm_module);
}
*/
