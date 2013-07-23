;;; libm.scm
;;;
;;; tie the math library into the *libm* environment

(if (not (provided? 'cload.scm)) (load "cload.scm"))

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
			     (int lrint (double))
			     (double round (double))
			     (int lround (double))
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
			     )
	  "" "math.h")
    
	(current-environment))))

*libm*
;; the loader will return *libm*


#|
names collide:
double floor double
double remainder double
double exp double
double log double
double sqrt double
also cos sin tan cosh sinh tanh acos asin atan atan2 acosh asinh atanh and all the complex cases if complex.h

special:
double remoquo double *int?
double frexp double *int
double modf double *double

ridiculous:
double fmax double double
double fmin double double
int isgreater double double (etc)

gnu_c additions?
complex cases aren't handled in cload I think
|#
