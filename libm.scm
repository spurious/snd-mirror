;;; libm.scm
;;;
;;; tie the math library into the *libm* environment

(if (not (provided? 'cload.scm)) (load "cload.scm"))
(provide 'libm.scm)

(if (not (defined? '*libm*))
    (define-constant *libm*
      (with-environment (initial-environment)
	
	(c-define '((double j0 (double)) 
		    (double j1 (double)) 
		    (double erf (double)) 
		    (double erfc (double))
		    (double lgamma (double))
		    
		    (double fabs (double))
		    (double ceil (double))
		    (double nearbyint (double))
		    (double rint (double))
		    (int llrint (double))
		    (int llround (double))
		    (double trunc (double))
		    (double fmod (double double))
		    (double ldexp (double int))
		    (double scalbn (double int))
		    (double scalbln (double int))
		    (double exp2 (double))
		    (double expm1 (double))
		    (double log10 (double))
		    (double log1p (double))
		    (double log2 (double))
		    (int ilogb (double))
		    (double cbrt (double))
		    (double hypot (double double))
		    (double fma (double double double))
		    (double pow (double double))
		    (double fdim (double double))
		    (double tgamma (double))
		    (double copysign (double double))
		    (double nan (char*))
		    (double nextafter (double double))
		    (double nexttoward (double double))
		    
		    (int fpclassify (double))
		    (int isfinite (double))
		    (int isinf (double))
		    (int isnan (double))
		    (int isnormal (double))
		    (int signbit (double))
		    
		    ;; exporting these will overwrite the built-in versions
		    (double floor (double))
		    (double round (double))
		    (double remainder (double double))
		    (double exp (double))
		    (double log (double))
		    (double sqrt (double))
		    (double cos (double))
		    (double sin (double))
		    (double tan (double))
		    (double cosh (double))
		    (double sinh (double))
		    (double tanh (double))
		    (double acos (double))
		    (double asin (double))
		    (double atan (double))
		    (double atan2 (double double))
		    (double acosh (double))
		    (double asinh (double))
		    (double atanh  (double))
		    
		    (int (FP_NAN FP_INFINITE FP_ZERO FP_SUBNORMAL FP_NORMAL))
		    (double (M_E M_LOG2E M_LOG10E M_LN2 M_LN10 M_PI M_PI_2 M_PI_4 M_1_PI M_2_PI M_2_SQRTPI M_SQRT2 M_SQRT1_2))
		    
		    (C-macro (double (__DBL_DENORM_MIN__ __DBL_MAX__ __DBL_MIN__ __DBL_EPSILON__)))
		    (C-macro (int (__DBL_MIN_10_EXP__ __DBL_MAX_10_EXP__ __DBL_DIG__ __DBL_MANT_DIG__ __DBL_MIN_EXP__ __DBL_MAX_EXP__ 
						      __SIZEOF_DOUBLE__ __SIZEOF_LONG_LONG__ __LONG_LONG_MAX__)))
		    
		    ;; these have arg by reference, return list in s7
		    (in-C "
static s7_pointer g_remquo(s7_scheme *sc, s7_pointer args)                           \n\
{                                                                                    \n\
  if (s7_is_real(s7_car(args)))                                                      \n\
    {                                                                                \n\
      if (s7_is_real(s7_cadr(args)))                                                 \n\
        {                                                                            \n\
          int quo = 0;                                                               \n\
          double rem;                                                                \n\
          rem = remquo(s7_number_to_real(sc, s7_car(args)), s7_number_to_real(sc, s7_cadr(args)), &quo); \n\
          return(s7_list(sc, 2, s7_make_real(sc, rem), s7_make_integer(sc, quo)));   \n\
        }                                                                            \n\
      return(s7_wrong_type_arg_error(sc, \"remquo\", 2, s7_cadr(args), \"a real\")); \n\
     }                                                                               \n\
  return(s7_wrong_type_arg_error(sc, \"remquo\", 1, s7_car(args), \"a real\"));      \n\
}                                                                                    \n\
static s7_pointer g_frexp(s7_scheme *sc, s7_pointer args)                            \n\
{                                                                                    \n\
  if (s7_is_real(s7_car(args)))                                                      \n\
    {                                                                                \n\
      int ex = 0;                                                                    \n\
      double frac;                                                                   \n\
      frac = frexp(s7_number_to_real(sc, s7_car(args)), &ex);                        \n\
      return(s7_list(sc, 2, s7_make_real(sc, frac), s7_make_integer(sc, ex)));       \n\
     }                                                                               \n\
  return(s7_wrong_type_arg_error(sc, \"frexp\", 1, s7_car(args), \"a real\"));       \n\
}                                                                                    \n\
static s7_pointer g_modf(s7_scheme *sc, s7_pointer args)                             \n\
{                                                                                    \n\
  if (s7_is_real(s7_car(args)))                                                      \n\
    {                                                                                \n\
      double frac, ip = 0.0;                                                         \n\
      frac = modf(s7_number_to_real(sc, s7_car(args)), &ip);                         \n\
      return(s7_list(sc, 2, s7_make_real(sc, frac), s7_make_real(sc, ip)));          \n\
     }                                                                               \n\
  return(s7_wrong_type_arg_error(sc, \"modf\", 1, s7_car(args), \"a real\"));        \n\
}                                                                                    \n\
")
                    (C-function ("remquo" g_remquo "(remquo x y) returns a list: (remainder messed-up-quotient)" 2))
                    (C-function ("frexp" g_frexp "(frexp x) returns a list: (fraction exponent)" 1))
                    (C-function ("modf" g_modf "(modf x) returns a list: (int-part frac-part) -- this is not the same as fmod!" 1))
		    )
		  "" "math.h" "" "" "libm_s7")
	
	(current-environment))))

*libm*
;; the loader will return *libm*


#|
rand/srand?
complex cases aren't handled in cload I think
|#
