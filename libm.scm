;;; libm.scm
;;;
;;; tie the math library into the *libm* environment

(if (not (provided? 'cload.scm)) (load "cload.scm"))
(provide 'libm.scm)

(if (not (defined? '*libm*))
    (define-constant *libm*
      (with-environment (initial-environment)
	
	(define-c-function '((double j0 (double)) 
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
			     )
	  "" "math.h")
    
	(current-environment))))

*libm*
;; the loader will return *libm*


#|
special:
double remquo double double [int]?
double frexp double [int]
double modf double [double]

ridiculous:
double fmax double double
double fmin double double
int isgreater double double (etc)

rand/srand?

gnu_c additions? jn y0 y1 yn
complex cases aren't handled in cload I think
|#
