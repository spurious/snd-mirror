/* create test.scm to test scheme numerical funcs
 *
 *  gcc numtst.c -o numtst -lm
 *  numtst <name>
 *    <name> defaults to s7, can also be guile, gauche, stklos
 *  run scheme and load test.scm
 *
 * this does not pay much attention to inexact integers or ratios
 *
 * in Gauche, it omits arg errors because they seem to be uncatchable.
 *   also Gauche hangs on (expt 1/500029 120960)
 *
 * SCM thinks 1.0 is an unbound variable??
 *
 * stklos segfaults on (sin 0/1234000000) or anything involving that fraction
 *
 * the trig functions can return many equally correct results, and have arbitrary
 *   ranges, so unless something is way off, those reports should be treated
 *   as merely curiousities.  I didn't include much of complex tanh because
 *   C's ctanh is incredibly buggy.  (It over/underflows at the drop of a
 *   hat, and returns +/-0 when the answer is cleary (almost)+/-1).
 */

#define _FILE_OFFSET_BITS 64

#include <stdio.h>
#include <unistd.h>
#include <math.h>
#include <limits.h>
#include <float.h>
#include <ctype.h>
#include <strings.h>
#include <string.h>
#include <stdlib.h>
#include <sys/types.h>
#include <ctype.h>
#include <complex.h>

#ifndef __cplusplus
#if HAVE_STDBOOL_H
#include <stdbool.h>
#else
#ifndef true
#define bool	int
#define true	1
#define false	0
#endif
#endif
#endif

/* -------------------------------------------------------------------------------- */
static bool test_ratios = true;
static bool test_complex = true;
static const char *scheme_name = "s7"; /* also guile, gauche, stklos */
/* -------------------------------------------------------------------------------- */


static const char *op_names[] = {
  "sin", "cos", "tan", "asin", "acos", "atan", "sinh", "cosh", "tanh", "asinh", "acosh", "atanh", "sqrt", "exp", "log"
};

static const char *inverse_op_names[] = {
  "asin", "acos", "atan", "sin", "cos", "tan", "asinh", "acosh", "atanh", "sinh", "cosh", "tanh", "square", "log", "exp"
};

#define OP_NAMES 15


#define ARG_INT 0
#define ARG_RATIO 1
#define ARG_REAL 2
#define ARG_REAL2 3
#define ARG_COMPLEX 4
#define ARG_ANY 5

typedef struct {
  char *name;
  int min_args, max_args;
  int arg_type;
} op_stuff;

#define NUMERIC_FUNCS 57

static op_stuff numeric_data[NUMERIC_FUNCS] = {
  {"exact?", 1, 1, ARG_COMPLEX},
  {"inexact?", 1, 1, ARG_COMPLEX}, 
  {"=", -1, -1, ARG_COMPLEX}, 
  {"<", -1, -1, ARG_COMPLEX}, 
  {">", -1, -1, ARG_COMPLEX}, 
  {"<=", -1, -1, ARG_COMPLEX}, 
  {">=", -1, -1, ARG_COMPLEX}, 
  {"+", -1, -1, ARG_COMPLEX}, 
  {"-", -1, -1, ARG_COMPLEX}, 
  {"/", 1, -1, ARG_COMPLEX}, 
  {"*", -1, -1, ARG_COMPLEX}, 
  {"zero?", 1, 1, ARG_COMPLEX}, 
  {"positive?", 1, 1, ARG_REAL}, 
  {"negative?", 1, 1, ARG_REAL}, 
  {"even?", 1, 1, ARG_INT}, 
  {"odd?", 1, 1, ARG_INT}, 
  {"quotient", 2, 2, ARG_INT}, 
  {"remainder", 2, 2, ARG_INT}, 
  {"modulo", 2, 2, ARG_INT}, 
  {"truncate", 1, 1, ARG_REAL}, 
  {"floor", 1, 1, ARG_REAL}, 
  {"ceiling", 1, 1, ARG_REAL}, 
  {"round", 1, 1, ARG_REAL}, 
  {"abs", 1, 1, ARG_REAL}, 
  {"max", 1, -1, ARG_REAL}, 
  {"min", 1, -1, ARG_REAL}, 
  {"gcd", 0, -1, ARG_INT}, 
  {"lcm", 0, -1, ARG_INT}, 
  {"expt", 2, 2, ARG_COMPLEX}, 
  {"exact->inexact", 1, 1, ARG_COMPLEX}, 
  {"inexact->exact", 1, 1, ARG_COMPLEX}, 
  {"rationalize", 2, 2, ARG_REAL}, 
  {"numerator", 1, 1, ARG_REAL}, /* stupid guile... */ 
  {"denominator", 1, 1, ARG_REAL}, 
  {"imag-part", 1, 1, ARG_COMPLEX}, 
  {"real-part", 1, 1, ARG_COMPLEX}, 
  {"magnitude", 1, 1, ARG_COMPLEX}, 
  {"angle", 1, 1, ARG_COMPLEX}, 
  {"make-polar", 2, 2, ARG_REAL}, 
  {"make-rectangular", 2, 2, ARG_REAL}, 
  {"sqrt", 1, 1, ARG_COMPLEX}, 
  {"exp", 1, 1, ARG_COMPLEX}, 
  {"log", 1, 1, ARG_COMPLEX}, 
  {"sin", 1, 1, ARG_COMPLEX}, 
  {"cos", 1, 1, ARG_COMPLEX}, 
  {"tan", 1, 1, ARG_COMPLEX}, 
  {"asin", 1, 1, ARG_COMPLEX}, 
  {"acos", 1, 1, ARG_COMPLEX}, 
  {"atan", 1, 1, ARG_COMPLEX}, 
  {"sinh", 1, 1, ARG_COMPLEX}, 
  {"cosh", 1, 1, ARG_COMPLEX}, 
  {"tanh", 1, 1, ARG_COMPLEX}, 
  {"asinh", 1, 1, ARG_COMPLEX}, 
  {"acosh", 1, 1, ARG_COMPLEX}, 
  {"atanh", 1, 1, ARG_COMPLEX}, 
  {"string->number", 1, 1, ARG_ANY}, 
  {"number->string", 1, 1, ARG_COMPLEX}, 
};

static char *non_numeric_args[] =
  {"'symbol", "\"hi\"", "(1 . 2)", "#\\c", "'#(1 2)",
  };

#define NON_NUMERIC_ARGS 5



static double complex csquare(double complex z)
{
  return(cpow(z, 2));
}

static double square(double x)
{
  return(pow(x, 2));
}

static double complex anyf_1(int op, double complex arg)
{
  switch (op)
    {
    case 0: return(csin(arg));
    case 1: return(ccos(arg));
    case 2: return(ctan(arg));
    case 3: return(casin(arg));
    case 4: return(cacos(arg));
    case 5: return(catan(arg));
    case 6: return(csinh(arg));
    case 7: return(ccosh(arg));
    case 8: return(ctanh(arg));
    case 9: return(casinh(arg));
    case 10: return(cacosh(arg));
    case 11: return(catanh(arg));
    case 12: return(csqrt(arg));
    case 13: return(cexp(arg));
    case 14: return(clog(arg));
    }
  return(0.0);
}

static double complex inverse_anyf_1(int op, double complex arg)
{
  switch (op)
    {
    case 0: return(casin(arg));
    case 1: return(cacos(arg));
    case 2: return(catan(arg));
    case 3: return(csin(arg));
    case 4: return(ccos(arg));
    case 5: return(ctan(arg));
    case 6: return(casinh(arg));
    case 7: return(cacosh(arg));
    case 8: return(catanh(arg));
    case 9: return(csinh(arg));
    case 10: return(ccosh(arg));
    case 11: return(ctanh(arg));
    case 12: return(csquare(arg));
    case 13: return(clog(arg));
    case 14: return(cexp(arg));
    }
  return(0.0);
}

static double anyf_2(int op, double arg)
{
  switch (op)
    {
    case 0: return(sin(arg));
    case 1: return(cos(arg));
    case 2: return(tan(arg));
    case 3: return(asin(arg));
    case 4: return(acos(arg));
    case 5: return(atan(arg));
    case 6: return(sinh(arg));
    case 7: return(cosh(arg));
    case 8: return(tanh(arg));
    case 9: return(asinh(arg));
    case 10: return(acosh(arg));
    case 11: return(atanh(arg));
    case 12: return(sqrt(arg));
    case 13: return(exp(arg));
    case 14: return(log(arg));
    }
  return(0.0);
}

static double inverse_anyf_2(int op, double arg)
{
  switch (op)
    {
    case 0: return(asin(arg));
    case 1: return(acos(arg));
    case 2: return(atan(arg));
    case 3: return(sin(arg));
    case 4: return(cos(arg));
    case 5: return(tan(arg));
    case 6: return(asinh(arg));
    case 7: return(acosh(arg));
    case 8: return(atanh(arg));
    case 9: return(sinh(arg));
    case 10: return(cosh(arg));
    case 11: return(tanh(arg));
    case 12: return(square(arg));
    case 13: return(log(arg));
    case 14: return(exp(arg));
    }
  return(0.0);
}

#define DEFAULT_RATIONALIZE_ERROR 1.0e-12

static off_t c_mod(off_t x, off_t y)
{
  off_t z;
  if (y == 0) return(x); /* else arithmetic exception */
  z = x % y;
  if (((y < 0) && (z > 0)) ||
      ((y > 0) && (z < 0)))
    return(z + y);
  return(z);
}

static off_t c_gcd(off_t u, off_t v)
{
  off_t a, b, temp;
  
  a = abs(u);
  b = abs(v);
  while (b != 0)
    {
      temp = a % b;
      a = b;
      b = temp;
    }
  if (a < 0)
    return(-a);
  return(a);
}


static off_t c_lcm(off_t a, off_t b)
{
  if ((a == 0) || (b == 0)) return(0);
  if (a < 0) a = -a;
  if (b < 0) b = -b;
  return((a * b) / c_gcd(a, b));
}


static bool c_rationalize(double ux, double error, off_t *numer, off_t *denom)
{
  off_t a1 = 0, a2 = 1, b1 = 1, b2 = 0, tt = 1, a = 0, b = 0, ctr;
  double x;
  
  if (ux == 0.0)
    {
      (*numer) = 0;
      (*denom) = 1;
      return(true);
    }
  if (ux == 1.0)
    {
      (*numer) = 1;
      (*denom) = 1;
      return(true);
    }
  
  x = 1.0 / ux;
  for (ctr = 0; (ctr < 100) && (x != tt); ctr++)
    {
      a = a2 + a1 * tt;
      b = b2 + b1 * tt;
      if (fabs(ux - (double)a / (double)b) < error)
	{
	  (*numer) = a;
	  (*denom) = b;
	  return(true);
	}
      if (x == tt)
	return(false);
      x = 1.0 / (x - tt);
      tt = (off_t)floor(x);
      a2 = a1;
      b2 = b1;
      a1 = a;
      b1 = b;
    }
  return(false);
}

static off_t oround(double x)
{
  off_t i;
  i = (off_t)floor(x + .0001);
  if ((x - i) > 0.5) return(i + 1);
  return(i);
}


/* we are plagued by under/overflows here that are not reported as NaN or inf */
#define isnormal(x) ((!isnan(x)) && (!isinf(x)))

static double complex anyf(int op, double complex arg)
{
  double complex result, inversed;
  bool baddy = false;
  
  result = anyf_1(op, arg);
  
  if ((!isnormal(creal(result))) ||
      (!isnormal(cimag(result))) ||
      (cabs(result) > 1.0e12))
    baddy = true;
  else
    {
      /* check that we can invert this silly thing */
      inversed = inverse_anyf_1(op, result);
      
      if ((abs(creal(inversed) - creal(arg)) > 1.0e-2) ||
	  (abs(cimag(inversed) - cimag(arg)) > 1.0e-2))
	baddy = true;
      
      if ((cimag(arg) == 0.0) &&
	  (cimag(result) == 0.0))
	{
	  double dres, vres;
	  dres = anyf_2(op, creal(arg));
	  vres = inverse_anyf_2(op, dres);
	  
	  if (abs(dres) > 1.0e12)
	    baddy = true;
	  else
	    {
	      if ((fabs(dres - creal(result)) > .1) &&
		  (fabs(vres - creal(arg)) < 1.0e-2))
		return(dres + 0.0 * _Complex_I);
	    }
	}
    }
  
  if (baddy)
    return(54321.0); /* our error signal... */
  return(result);
}


static off_t int_args[] = {
  0, 1, 2, 3, 10, 1234, 1234000000, 500029, 362880,
};

#define INT_ARGS 9

static double double_args[] = {
  0.0, 0.00000001, 1.0, 
  3.141592653589793, 2.718281828459045, 
  1234.0, 1234000000.0
};

#define DOUBLE_ARGS 7

static double complex expt_op(double complex arg1, double complex arg2)
{
  double complex result, inversed;
  bool baddy = false;
  
  result = cpow(arg1, arg2);
  
  if ((!isnormal(creal(result))) ||
      (!isnormal(cimag(result))) ||
      (cabs(result) > 1.0e12))
    baddy = true;
  else
    {
      /* check that we can invert this silly thing */
      inversed = log(result) / log(arg1);
      
      if ((abs(creal(inversed) - creal(arg2)) > 1.0e-2) ||
	  (abs(cimag(inversed) - cimag(arg2)) > 1.0e-2))
	baddy = true;
      
      if ((cimag(arg1) == 0.0) &&
	  (cimag(arg2) == 0.0) &&
	  (cimag(result) == 0.0))
	{
	  double dres, vres;
	  dres = pow(creal(arg1), creal(arg2));
	  vres = log(dres) / log(arg1);
	  
	  if (abs(dres) > 1.0e12)
	    baddy = true;
	  else
	    {
	      if ((fabs(dres - creal(result)) > .1) &&
		  (fabs(vres - creal(arg2)) < 1.0e-2))
		return(dres + 0.0 * _Complex_I);
	    }
	}
    }
  
  if (baddy)
    return(54321.0); /* our error signal... */
  return(result);
}



double complex int_to_complex(off_t i)
{
  return((double)i + 0.0 * _Complex_I);
}

double complex float_to_complex(double x)
{
  return(x + 0.0 * _Complex_I);
}

static char *gstr(double val)
{
  int i, len;
  char *temp;
  temp = (char *)calloc(128, sizeof(char));
  snprintf(temp, 128, "%.14f", val);
  len = strlen(temp);
  for (i = len - 1; i > 0; i--)
    if (temp[i] != '0') break;
  if ((temp[i] == '.') &&
      (i < len - 2))
    temp[i + 2] = '\0';
  return(temp);
}

static void complex_to_string(FILE *fp, double complex z)
{
  double rl, im;
  char *t1, *t2;
  rl = creal(z);
  im = cimag(z);
  if (im != 0.0)
    {
      if (im > 0.0)
	fprintf(fp, "%s+%si", t1 = gstr(rl), t2 = gstr(im));
      else fprintf(fp, "%s-%si", t1 = gstr(rl), t2 = gstr(fabs(im)));
      free(t1);
      free(t2);
    }
  else 
    {
      fprintf(fp, "%s", t1 = gstr(rl));
      free(t1);
    }
}


static int ctr = 0;
static char argbuf[1024];

static char *int_arg_name(int n)
{
  ctr++;
  if (ctr > 4)
    {
      sprintf(argbuf, "int-%d", n);
      ctr = 0;
    }
  else sprintf(argbuf, "%lld", int_args[n]);
  return(argbuf);
}

static char *ratio_arg_name(int a1, int a2)
{
  sprintf(argbuf, "%lld/%lld", int_args[a1], int_args[a2]);
  ctr++;
  if (ctr > 4)
    {
      ctr = 0;
      if (a2 == 3)
	sprintf(argbuf, "ratio-%d", a1);
    }
  return(argbuf);
}

static char *double_arg_name(int n)
{
  ctr++;
  if (ctr > 4)
    {
      sprintf(argbuf, "double-%d", n);
      ctr = 0;
    }
  else 
    {
      char *tmp;
      sprintf(argbuf, "%s", tmp = gstr(double_args[n]));
      free(tmp);
    }
  return(argbuf);
}

static char *complex_arg_name(int a1, int a2, int s1, int s2)
{
  char *t1, *t2;
  
  sprintf(argbuf, "%s%s%s%si", (s1 == -1) ? "-" : "", t1 = gstr(double_args[a1]), (s2 == -1) ? "-" : "+", t2 = gstr(double_args[a2]));
  free(t1);
  free(t2);
  ctr++;
  if (ctr > 4)
    {
      if ((a2 == 3) &&
	  (s1 == 1) &&
	  (s2 == 1))
	sprintf(argbuf, "complex-%d", a1);
      ctr = 0;
    }
  return(argbuf);
}



/* -------------------------------------------------------------------------------- */

int main(int argc, char **argv)
{
  int i, j, k, op;
  double complex result;
  char *argstr = NULL;
  
  FILE *fp;
  fp = fopen("test.scm", "w");

  if (argc > 1)
    scheme_name = argv[1];

  if ((strcmp(scheme_name, "gauche") == 0) ||
      (strcmp(scheme_name, "stklos") == 0))
    fprintf(fp, "\n\
(define-syntax defmacro\n\
  (syntax-rules ()\n\
    ((_ name params . body) (define-macro (name . params) . body))))\n\
\n\
(define (1+ n) (+ n 1))\n\
(define (throw . args) (raise args))\n\
\n\
(define (catch tag body error-handler)\n\
  (guard (err (else (apply error-handler (if (list? err) err (list err)))))\n\
	 (body)))\n");

  if ((strcmp(scheme_name, "s7") == 0) ||
      (strcmp(scheme_name, "guile") == 0) ||
      (strcmp(scheme_name, "gauche") == 0))
    {
      fprintf(fp, "\n\
(defmacro test (tst expected)\n\
  `(let ((result (catch #t (lambda () ,tst) (lambda args 'error))))\n\
     (if (or (and (eq? ,expected 'error)\n\
		  (not (eq? result 'error)))\n\
	     (and (eq? ,expected #t)\n\
		  (not result))\n\
	     (and (eq? ,expected #f)\n\
		  result)\n\
             (and (integer? ,expected)\n\
		  (integer? result)\n\
		  (not (= result ,expected)))\n\
	     (and (real? ,expected)\n\
		  (real? result)\n\
		  (> (abs (- result ,expected)) 1.0e-6))\n\
	     (and (number? result)\n\
	          (or (not (real? ,expected))\n\
		      (not (real? result)))\n\
		  (or (and (> (abs (real-part (- result ,expected))) 1.0e-4)\n\
			   (> (abs (real-part (+ result ,expected))) 1.0e-4))\n\
		      (and (> (abs (imag-part (- result ,expected))) 1.0e-4)\n\
			   (> (abs (imag-part (+ result ,expected))) 1.0e-4)))))\n\
	 (display (format #f \";~A got ~A, but expected ~A~%%\" ',tst result ',expected)))))\n\n");
    }

  fprintf(fp, "\n\
(for-each\n\
 (lambda (op)\n\
   (for-each\n\
    (lambda (arg)\n\
      (if (op arg)\n\
	  (display (format #f \";(~A ~A) returned #t?~%%\" op arg))))\n\
    (list \"hi\" (integer->char 65) #f '(1 2) 'a-symbol (cons 1 2) (make-vector 3) abs)))\n\
 (list number? complex? real? rational? integer?))\n\
\n\
(for-each\n\
 (lambda (arg) \n\
   (if (not (complex? arg))\n\
       (display (format #f \";~A is not complex?~%%\" arg))))\n\
 (list 1 1.0 1.0+0.5i 1/2))\n\
 \n\
(if (not (integer? 1234)) (display \";1234 is not an integer?~%%\"))\n\
(for-each\n\
 (lambda (arg) \n\
   (if (integer? arg)\n\
       (display (format #f \";~A is an integer?~%%\" arg))))\n\
 (list 1.5 1.0+0.5i 1/2))\n\
\n\
(if (real? 1.0+1.0i) (display \";1.0+1.0i is real?~%%\"))\n\
(for-each\n\
 (lambda (arg) \n\
   (if (not (real? arg))\n\
       (display (format #f \";~A is not a real?~%%\" arg))))\n\
 (list 1 1.0 1/2))\n\
\n\
(if (not (rational? 1/2)) (snd-display \";1/2 is not rational?~%%\"))\n\
(if (not (rational? 2)) (snd-display \";2 is not rational?~%%\"))\n\n");

  fprintf(fp, "\n\
(for-each\n\
 (lambda (n)\n\
   (if (not (even? n))\n\
       (display (format #f \";~A is not even?\" arg))))\n\
 (list 0 2 1234 -4 -10000002 1000000006))\n\
\n\
(for-each\n\
 (lambda (n)\n\
   (if (odd? n)\n\
       (display (format #f \";~A is odd?\" arg))))\n\
 (list 0 2 1234 -4 -10000002 1000000006))\n\
\n\
(for-each\n\
 (lambda (n)\n\
   (if (even? n)\n\
       (display (format #f \";~A is even?\" arg))))\n\
 (list 1 -1 31 50001 543321))\n\
\n\
(for-each\n\
 (lambda (n)\n\
   (if (not (odd? n))\n\
       (display (format #f \";~A is not odd?\" arg))))\n\
 (list 1 -1 31 50001 543321))\n\
\n\
(for-each\n\
 (lambda (n)\n\
   (if (not (positive? n))\n\
       (display (format #f \";~A is not positive?\" arg))))\n\
 (list 1 123 123456123 1.4 0.001 1/2 124124124.2))\n\
\n\
(for-each\n\
 (lambda (n)\n\
   (if (negative? n)\n\
       (display (format #f \";~A is negative?\" arg))))\n\
 (list 1 123 123456123 1.4 0.001 1/2 12341243124.2))\n\
\n\
(for-each\n\
 (lambda (n)\n\
   (if (positive? n)\n\
       (display (format #f \";~A is positive?\" arg))))\n\
 (list -1 -123 -123456123 -3/2 -0.00001 -1.4 -123124124.1))\n\
\n\
(for-each\n\
 (lambda (n)\n\
   (if (not (negative? n))\n\
       (display (format #f \";~A is not negative?\" arg))))\n\
 (list -1 -123 -123456123 -2/3 -0.00001 -1.4 -123124124.1))\n\
\n\
(for-each\n\
 (lambda (n)\n\
   (if (not (zero? n))\n\
       (display (format #f \";~A is not zero?\" arg))))\n\
 (list 0 0.0 0+0i 0/1 0.0-0.0i))\n\
\n\
(for-each\n\
 (lambda (n)\n\
   (if (zero? n)\n\
       (display (format #f \";~A is zero?\" arg))))\n\
 (list 1 1/100 -0.001 0.0+1.0i))\n\n\
(let* ((last-good 0)\n\
       (first-bad \n\
	(call/cc\n\
	 (lambda (return)\n\
	   (do ((i 1 (1+ i))\n\
		(k 1 (* k 2)))\n\
	       ((= i 62) #f)\n\
	     (if (and (eqv? k (string->number (number->string k)))\n\
		      (eqv? (- k) (string->number (number->string (- k)))))\n\
		 (set! last-good k)\n\
		 (let ((search \n\
			(lambda (lo hi)\n\
			  (if (= lo hi)\n\
			      hi\n\
			      (let ((mid (inexact->exact (floor (/ (+ hi lo) 2)))))\n\
				(if (and (eqv? mid (string->number (number->string mid)))\n\
					 (eqv? (- mid) (string->number (number->string (- mid)))))\n\
				    (search mid hi)\n\
				    (search lo mid)))))))\n\
		   (return (search last-good (* 2 last-good))))))))))\n\
  (if (integer? first-bad)\n\
      (display (format #f \";string->number ints fail around ~A (2^~A)~%%\" first-bad (/ (log first-bad) (log 2.0))))))\n\
\n\
(let* ((last-good 0)\n\
       (first-bad \n\
	(call/cc\n\
	 (lambda (return)\n\
	   (do ((i 1 (1+ i))\n\
		(k 1.0 (* k 2.0)))\n\
	       ((= i 62) #f)\n\
	     (if (and (eqv? k (string->number (number->string k)))\n\
		      (eqv? (- k) (string->number (number->string (- k)))))\n\
		 (set! last-good k)\n\
		 (letrec ((search \n\
			   (lambda (lo hi)\n\
			     (if (= (inexact->exact (floor lo)) (inexact->exact (floor hi)))\n\
				 hi\n\
				 (let ((mid (/ (+ hi lo) 2)))\n\
				   (if (and (eqv? mid (string->number (number->string mid)))\n\
					    (eqv? (- mid) (string->number (number->string (- mid)))))\n\
				       (search mid hi)\n\
				       (search lo mid)))))))\n\
		   (return (search last-good (* 2 last-good))))))))))\n\
  (if (number? first-bad)\n\
      (display (format #f \";string->number floats fail around ~A (2^~A)~%%\" first-bad (/ (log first-bad) (log 2.0))))))\n\
\n\
(let* ((last-good 0)\n\
       (first-bad \n\
	(call/cc\n\
	 (lambda (return)\n\
	   (do ((i 1 (1+ i))\n\
		(k 1/3 (* k 2)))\n\
	       ((= i 62) #f)\n\
	     (if (and (eqv? k (string->number (number->string k)))\n\
		      (eqv? (- k) (string->number (number->string (- k)))))\n\
		 (set! last-good k)\n\
		 (letrec ((search \n\
			   (lambda (lo hi)\n\
			     (if (= (inexact->exact (floor lo)) (inexact->exact (floor hi)))\n\
				 hi\n\
				 (let ((mid (/ (+ hi lo) 2)))\n\
				   (if (and (eqv? mid (string->number (number->string mid)))\n\
					    (eqv? (- mid) (string->number (number->string (- mid)))))\n\
				       (search mid hi)\n\
				       (search lo mid)))))))\n\
		   (return (search last-good (* 2 last-good))))))))))\n\
  (if (number? first-bad)\n\						\
      (display (format #f \";string->number ratios fail around ~A (2^~A)~%%\" first-bad (/ (log first-bad) (log 2.0))))))\n\n");
  
  for (i = 0; i < INT_ARGS; i++)
    fprintf(fp, "(if (not (eqv? %lld (string->number (number->string %lld))))\n\
    (display (format #f \";string<->number ~A -> ~A -> ~A?~%%\" %lld (number->string %lld) (string->number (number->string %lld)))))\n",
	    int_args[i], int_args[i], int_args[i], 
	    int_args[i], int_args[i]);
  
  for (i = 0; i < INT_ARGS; i++)
    for (j = 0; j < INT_ARGS; j++)
      if (int_args[j] != 0)
	fprintf(fp, "(if (not (eqv? %lld/%lld (string->number (number->string %lld/%lld))))\n\
    (display (format #f \";string<->number ~A -> ~A -> ~A?~%%\" %lld/%lld (number->string %lld/%lld) (string->number (number->string %lld/%lld)))))\n",
		int_args[i], int_args[j], int_args[i], int_args[j],
		int_args[i], int_args[j], int_args[i], int_args[j], 
		int_args[i], int_args[j]);
  
  for (i = 0; i < DOUBLE_ARGS; i++)
    fprintf(fp, "(if (not (eqv? %f (string->number (number->string %f))))\n\
    (display (format #f \";string<->number ~A -> ~A -> ~A?~%%\" %f (number->string %f) (string->number (number->string %f)))))\n",
	    double_args[i], double_args[i], double_args[i],
	    double_args[i], double_args[i]);
  
  for (i = 0; i < DOUBLE_ARGS; i++)
    for (j = 0; j < DOUBLE_ARGS; j++)
      fprintf(fp, "(if (not (eqv? %f+%fi (string->number (number->string %f+%fi))))\n\
    (display (format #f \";string<->number ~A -> ~A -> ~A?~%%\" %f+%fi (number->string %f+%fi) (string->number (number->string %f+%fi)))))\n",
	      double_args[i], double_args[j], double_args[i], double_args[j],
	      double_args[i], double_args[j], double_args[i], double_args[j], 
	      double_args[i], double_args[j]);


  fprintf(fp, ";;; --------------------------------------------------------------------------------\n\n");

  /* Gauche "compile error" seems to be uncatchable, and halts the load */
  if (strcmp(scheme_name, "gauche") != 0)
    {
  for (i = 0; i < NUMERIC_FUNCS; i++)
    {
      if (numeric_data[i].min_args != -1)
	{
	  /* try correct args but wrong type, and incorrect num args high and low */
	  for (j = 0; j < numeric_data[i].min_args; j++)
	    {
	      fprintf(fp, "(test (%s", numeric_data[i].name);
	      if (j > 0)
		{
		  fprintf(fp, " ");
		  for (k = 0; k < j; k++)
		    {
		      switch (numeric_data[i].arg_type)
			{
			case ARG_INT: fprintf(fp, "123"); break;
			case ARG_RATIO: fprintf(fp, "1/3"); break;
			case ARG_REAL: fprintf(fp, "1.23"); break;
			case ARG_COMPLEX: fprintf(fp, "1.0+23.0i"); break;
			case ARG_ANY: fprintf(fp, "\"1.0\""); break;
			}
		      if (k < j - 1) fprintf(fp, " "); 
		    }
		}
	      fprintf(fp, ") 'error)\n");
	    }
	}
      for (j = numeric_data[i].min_args; j <= numeric_data[i].min_args; j++)
	{
	  if (j > 0)
	    {
	      fprintf(fp, "(test (%s ", numeric_data[i].name);
	      for (k = 0; k < j; k++)
		{
		  switch (numeric_data[i].arg_type)
		    {
		    case ARG_INT: fprintf(fp, "1.23"); break;
		    case ARG_RATIO: fprintf(fp, "1.3"); break;
		    case ARG_REAL: fprintf(fp, "1.23+1.0i"); break;
		    case ARG_COMPLEX: fprintf(fp, "\"hi\""); break;
		    case ARG_ANY: fprintf(fp, "'symbol"); break;
		    }
		  if (k < j - 1) fprintf(fp, " "); 
		}
	      fprintf(fp, ") 'error)\n");
	    }
	}
      if (numeric_data[i].max_args != -1)
	{
	  fprintf(fp, "(test (%s ", numeric_data[i].name);
	  for (k = 0; k <= numeric_data[i].max_args; k++)
	    {
	      switch (numeric_data[i].arg_type)
		{
		case ARG_INT: fprintf(fp, "123"); break;
		case ARG_RATIO: fprintf(fp, "1/3"); break;
		case ARG_REAL: fprintf(fp, "1.23"); break;
		case ARG_COMPLEX: fprintf(fp, "1.0+23.0i"); break;
		case ARG_ANY: fprintf(fp, "\"1.0\""); break;
		}
	      if (k < numeric_data[i].max_args) fprintf(fp, " "); 
	    }
	  fprintf(fp, ") 'error)\n");
	}
    }
    }
  
  fprintf(fp, "\n;;; --------------------------------------------------------------------------------\n");

  for (i = 0; i < INT_ARGS; i++)
    {
      fprintf(fp, "(define int-%d %lld)\n", i, int_args[i]);
      fprintf(fp, "(define ratio-%d %lld/%lld)\n", i, int_args[i], int_args[3]);
    }
  for (i = 0; i < DOUBLE_ARGS; i++)
    {
      char *t1, *t2;
      fprintf(fp, "(define real-%d %s)\n", i, t1 = gstr(double_args[i]));
      fprintf(fp, "(define complex-%d %s+%si)\n", i, t1, t2 = gstr(double_args[3]));
      free(t1);
      free(t2);
    }

  fprintf(fp, "\n\n");

  fprintf(fp, "\n\
(define (morally-equal x y)\n\
  (or (= x y)\n\
      (= (exact->inexact x) (exact->inexact y))\n\
      (< (abs (- (magnitude x) (magnitude y))) 1.0e-4)\n\
      (< (abs (- (magnitude x) (magnitude (- 3.14159265 y)))) 1.0e-4)\n\
      (< (abs (- (magnitude x) (magnitude (+ 3.14159265 y)))) 1.0e-4)))\n\n");
  
  for (op = 0; op < OP_NAMES; op++)
    {
      for (i = 0; i < INT_ARGS; i++)
	{
	  result = anyf(op, int_to_complex(int_args[i]));
	  if (creal(result) == 54321.0) continue;
	  if ((isnormal(creal(result))) &&
	      (isnormal(cimag(result))))
	    {
	      argstr = int_arg_name(i);
	      fprintf(fp, "(test (%s %s) ", op_names[op], argstr);
	      complex_to_string(fp, result);
	      fprintf(fp, ")\n");
	      if ((op != 12) &&
		  (cimag(result) == 0.0) &&
		  (strcmp(op_names[op], "cosh") != 0) &&
		  (strcmp(op_names[op], "acosh") != 0) &&
		  (strcmp(op_names[op], "exp") != 0))
		fprintf(fp, "(test (morally-equal (%s (%s %s)) %s) #t)\n", 
			inverse_op_names[op], op_names[op], argstr, argstr);
	    }
	  if (int_args[i] != 0)
	    {
	      result = anyf(op, int_to_complex(-int_args[i]));
	      if (creal(result) == 54321.0) continue;
	      if ((isnormal(creal(result))) &&
		  (isnormal(cimag(result))))
		{
		  fprintf(fp, "(test (%s %lld) ", op_names[op], -int_args[i]);
		  complex_to_string(fp, result);
		  fprintf(fp, ")\n");
		}
	    }
	}
      
      for (i = 0; i < INT_ARGS; i++)
	for (j = 0; j < INT_ARGS; j++)
	  if (int_args[j] != 0)
	    {
	      result = anyf(op, float_to_complex((double)int_args[i]/(double)int_args[j]));
	      if (creal(result) == 54321.0) continue;
	      if ((isnormal(creal(result))) &&
		  (isnormal(cimag(result))))
		{
		  argstr = ratio_arg_name(i, j);
		  fprintf(fp, "(test (%s %s) ", op_names[op], ratio_arg_name(i, j));
		  complex_to_string(fp, result);
		  fprintf(fp, ")\n");
		  if ((op != 12) &&
		      (cimag(result) == 0.0) &&
		      (strcmp(op_names[op], "cosh") != 0) &&
		      (strcmp(op_names[op], "acosh") != 0) &&
		      (strcmp(op_names[op], "exp") != 0))
		    fprintf(fp, "(test (morally-equal (%s (%s %s)) %s) #t)\n", 
			    inverse_op_names[op], op_names[op], argstr, argstr);
		}
	      if (int_args[i] != 0)
		{
		  result = anyf(op, float_to_complex(-(double)int_args[i]/(double)int_args[j]));
		  if (creal(result) == 54321.0) continue;
		  if ((isnormal(creal(result))) &&
		      (isnormal(cimag(result))))
		    {
		      fprintf(fp, "(test (%s %lld/%lld) ", op_names[op], -int_args[i], int_args[j]);
		      complex_to_string(fp, result);
		      fprintf(fp, ")\n");
		    }
		}
	    }
      
      for (i = 0; i < DOUBLE_ARGS; i++)
	{
	  result = anyf(op, float_to_complex(double_args[i]));
	  if (creal(result) == 54321.0) continue;
	  if ((isnormal(creal(result))) &&
	      (isnormal(cimag(result))))
	    {
	      argstr = double_arg_name(i);
	      fprintf(fp, "(test (%s %s) ", op_names[op], argstr);
	      complex_to_string(fp, result);
	      fprintf(fp, ")\n");
	      if ((op != 12) &&
		  (cimag(result) == 0.0) &&
		  (strcmp(op_names[op], "cosh") != 0) &&
		  (strcmp(op_names[op], "acosh") != 0) &&
		  (strcmp(op_names[op], "exp") != 0))
		fprintf(fp, "(test (< (abs (- (%s (%s %s)) %s)) 1.0e-4) #t)\n", 
			inverse_op_names[op], op_names[op], argstr, argstr);
	    }
	  if (double_args[i] != 0.0)
	    {
	      result = anyf(op, float_to_complex(-double_args[i]));
	      if (creal(result) == 54321.0) continue;
	      if ((isnormal(creal(result))) &&
		  (isnormal(cimag(result))))
		{
		  char *t1;
		  fprintf(fp, "(test (%s %s) ", op_names[op], t1 = gstr(-double_args[i]));
		  free(t1);
		  complex_to_string(fp, result);
		  fprintf(fp, ")\n");
		}
	    }
	}
      
      for (i = 0; i < DOUBLE_ARGS; i++)
	for (j = 0; j < DOUBLE_ARGS; j++)
	  for (k = 0; k < 4; k++)
	    {
	      int s1 = 1, s2 = 1;
	      switch (k)
		{
		case 0: break;
		case 1: s1 = -1; break;
		case 2: s2 = -1; break;
		case 3: s1 = -1; s2 = -1; break;
		}
	      result = anyf(op, s1 * double_args[i] + s2 * double_args[j] * _Complex_I);
	      if (creal(result) == 54321.0) continue;
	      if ((isnormal(creal(result))) &&
		  (isnormal(cimag(result))))
		{
		  argstr = complex_arg_name(i, j, s1, s2);
		  fprintf(fp, "(test (%s %s) ", op_names[op], argstr);
		  complex_to_string(fp, result);
		  fprintf(fp, ")\n");
		  if ((op != 12) &&
		      (strcmp(op_names[op], "cosh") != 0) &&
		      (strcmp(op_names[op], "acosh") != 0) &&
		      (strcmp(op_names[op], "exp") != 0))
		    fprintf(fp, "(test (< (magnitude (- (%s (%s %s)) %s)) 1.0e-4) #t)\n", 
			    inverse_op_names[op], op_names[op], 
			    argstr, argstr);
		}
	    }
    }
  
  fprintf(fp, "\n;;; --------------------------------------------------------------------------------\n");
  
  
  for (i = 0; i < INT_ARGS; i++)
    for (j = 0; j < INT_ARGS; j++)
      {
	result = expt_op(float_to_complex((double)int_args[i]), float_to_complex((double)int_args[j]));
	if (creal(result) == 54321.0) continue;
	if ((isnormal(creal(result))) &&
	    (isnormal(cimag(result))) &&
	    (cabs(result) < 1.0e9) &&
	    ((strcmp(scheme_name, "gauche") != 0) ||
	     ((int_args[i] < 100) && (int_args[j] < 100))))
	  {
	    fprintf(fp, "(test (expt %lld %lld) ", int_args[i], int_args[j]);
	    fprintf(fp, "%lld", (off_t)oround(creal(result)));
	    fprintf(fp, ")\n");
	  }
	if (int_args[i] != 0)
	  {
	    result = expt_op(float_to_complex(-(double)int_args[i]), float_to_complex((double)int_args[j]));
	    if (creal(result) == 54321.0) continue;
	    if ((isnormal(creal(result))) &&
		(isnormal(cimag(result))) &&
		(cabs(result) < 1.0e9) &&
		((strcmp(scheme_name, "gauche") != 0) ||
		 ((int_args[i] < 100) && (int_args[j] < 100))))
	      {
		fprintf(fp, "(test (expt %lld %lld) ", -int_args[i], int_args[j]);
		fprintf(fp, "%lld", (off_t)oround(creal(result)));
		fprintf(fp, ")\n");
	      }
	    result = expt_op(float_to_complex((double)int_args[i]), float_to_complex(-(double)int_args[j]));
	    if (creal(result) == 54321.0) continue;
	    if ((isnormal(creal(result))) &&
		(isnormal(cimag(result))) &&
		((strcmp(scheme_name, "gauche") != 0) ||
		 ((int_args[i] < 100) && (int_args[j] < 100))))
	      {
		fprintf(fp, "(test (expt %lld %lld) ", int_args[i], -int_args[j]);
		complex_to_string(fp, result);
		fprintf(fp, ")\n");
	      }
	  }
      }
  
  for (i = 0; i < INT_ARGS; i++)
    for (j = 0; j < INT_ARGS; j++)
      {
	int arg3, arg4;
	arg3 = c_mod(i + j, INT_ARGS);
	arg4 = c_mod(i - j, INT_ARGS);
	
	if ((int_args[j] != 0) &&
	    (int_args[arg4] != 0) &&
	    (((double)int_args[arg3]/(double)int_args[arg4]) < 1.0e6))
	  {
	    result = expt_op(float_to_complex((double)int_args[i]/(double)int_args[j]),
			     float_to_complex((double)int_args[arg3]/(double)int_args[arg4]));
	    if (creal(result) == 54321.0) continue;
	    if ((isnormal(creal(result))) &&
		(isnormal(cimag(result))) &&
		(cabs(result) < 1.0e9) &&
		((strcmp(scheme_name, "gauche") != 0) ||
		 ((int_args[i] < 100) && (int_args[j] < 100) && (int_args[arg3] < 100) && (int_args[arg4] < 100))))
	      {
		fprintf(fp, "(test (expt %lld/%lld %lld/%lld) ", int_args[i], int_args[j], int_args[arg3], int_args[arg4]);
		complex_to_string(fp, result);
		fprintf(fp, ")\n");
	      }
	  }
	if ((int_args[i] != 0) &&
	    (int_args[j] != 0) &&
	    (int_args[arg4] != 0) &&
	    (((double)int_args[arg3]/(double)int_args[arg4]) < 1.0e6))
	  {
	    result = expt_op(float_to_complex(-(double)int_args[i]/(double)int_args[j]),
			     float_to_complex((double)int_args[arg3]/(double)int_args[arg4]));
	    if (creal(result) == 54321.0) continue;
	    if ((isnormal(creal(result))) &&
		(isnormal(cimag(result))) &&
		(cabs(result) < 1.0e9) &&
		((strcmp(scheme_name, "gauche") != 0) ||
		 ((int_args[i] < 100) && (int_args[j] < 100) && (int_args[arg3] < 100) && (int_args[arg4] < 100))))
	      {
		fprintf(fp, "(test (expt %lld/%lld %lld/%lld) ", -int_args[i], int_args[j], int_args[arg3], int_args[arg4]);
		complex_to_string(fp, result);
		fprintf(fp, ")\n");
	      }
	    
	  }
      }
  
  
  
  fprintf(fp, "\n;;; --------------------------------------------------------------------------------\n");
  
  /* expt with float and complex
     remainder modulo quotient gcd lcm
     + - * / < <= > >= = max min
     abs rationalize 
     real-part imag-part numerator denominator make-rectangular make-polar 
     angle magnitude

     string->n going 1/2, exprs as args, funcs
     
  */
  
  fclose(fp);
  
  return(0);
}
