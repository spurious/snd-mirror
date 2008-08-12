/* create test.scm to test scheme numerical funcs
 *
 *  gcc numtst.c -o numtst -lm
 *  numtst <name>
 *    <name> defaults to s7, can also be guile, gauche, stklos, gambit
 *  run scheme and load test.scm
 *
 * Leppie (comp.lang.scheme) says:
 *    To compile on Cygwin: 
 *    gcc -mno-cygwin -o numtest numtst.c 
 *    Else complex.h seems to be not included :| 
 *  also
 *    #define off_t long long
 *  at the top somewhere
 *
 * this does not pay much attention to inexact integers or ratios
 *
 * in Gauche, it omits arg errors because they seem to be uncatchable.
 *   also Gauche hangs on (expt 1/500029 120960) -- I removed this test
 *
 * SCM thinks 1.0 is an unbound variable??
 *
 * stklos segfaults on (sin 0/1234000000) or anything involving that fraction
 *
 * Gambit support thanks to Brad Lucier
 *
 * the trig functions can return many equally correct results, and have arbitrary
 *   ranges, so unless something is way off, those reports should be treated
 *   as merely curiousities.  I didn't include much of complex tanh because
 *   C's ctanh is incredibly buggy.  (It over/underflows at the drop of a
 *   hat, and returns +/-0 when the answer is clearly (almost)+/-1).
 *   I added tanh (and other) tests via maxima.
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
#include <stdarg.h>

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
static const char *scheme_name = "s7"; /* also guile, gauche, stklos, gambit */

static bool ask_maxima = false;
static bool include_big_fractions_in_expt = true;
static bool use_continued_fractions_in_rationalize = false;
/* -------------------------------------------------------------------------------- */


static const char *op_names[] = {
  "sin", "cos", "tan", "asin", "acos", "atan", "sinh", "cosh", "tanh", "asinh", "acosh", "atanh", "sqrt", "exp", "log"
};

#define OP_NAMES 15


#define ARG_INT 0
#define ARG_RATIO 1
#define ARG_REAL 2
#define ARG_COMPLEX 3
#define ARG_ANY 4

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
  {"log", 1, 2, ARG_COMPLEX},       /* apparently r6rs includes the base? */
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
  {"number->string", 1, 2, ARG_COMPLEX}, 
};


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

static off_t cint_mod(int x, int y)
{
  int z;
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
  if (a < 0) a = -a;   /* in Scheme, lcm is always positive */
  if (b < 0) b = -b;
  return((a * b) / c_gcd(a, b));
}


static bool continued_fraction_rationalize(double ux, double error, off_t *numer, off_t *denom)
{
  off_t a1 = 0, a2 = 1, b1 = 1, b2 = 0, tt = 1, a = 0, b = 0, ctr, int_part = 0;
  bool neg = false;
  double x;
  
  if (ux == 0.0)
    {
      (*numer) = 0;
      (*denom) = 1;
      return(true);
    }

  if (ux < 0.0)
    {
      neg = true;
      ux = -ux;
    }

  if (ux == 1.0)
    {
      (*numer) = (neg) ? -1 : 1;
      (*denom) = 1;
      return(true);
    }

  if (ux > 1.0)
    {
      int_part = (off_t)floor(ux);
      ux -= int_part;
    }

  if (ux < error)
    {
      if (ux > 0.5) int_part++;
      (*numer) = (neg) ? -int_part : int_part;
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
	  a += (b * int_part);
	  (*numer) = (neg) ? -a : a;
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

/* this is UGLY! */
static bool c_rationalize(double ux, double error, off_t *n, off_t *d)
{
  off_t numer, denom, lim, sign = 0;
  lim = 1.0 / error;
  if (ux < 0.0)
    {
      ux = -ux;
      sign = 1;
    }

  /* perhaps use continued fractions to get a good result, then
   *   back up the denominator until we lose?  As it is, this
   *   is very slow!
   */
  for (denom = 1; denom <= lim; denom++)
    {
      numer = (off_t)floor(ux * denom);
      if ((((double)numer / (double)denom) + error) >= ux)
	{
	  if (sign)
	    (*n) = -numer;
	  else (*n) = numer;
	  (*d) = denom;
	  return(true);
	}
      numer++;
      if ((((double)numer / (double)denom) - error) <= ux)
	{
	  if (sign)
	    (*n) = -numer;
	  else (*n) = numer;
	  (*d) = denom;
	  return(true);
	}
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

static void complex_to_file(FILE *fp, double complex z)
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

static char *split_complex_to_string(double rl, double im)
{
  if (im != 0.0)
    {
      char *t1, *t2, *temp;
      t1 = gstr(rl);
      if (im > 0.0)
	t2 = gstr(im);
      else t2 = gstr(fabs(im));
      temp = (char *)calloc(4 + strlen(t1) + strlen(t2), sizeof(char));
      sprintf(temp, "%s%s%si", t1, (im > 0.0) ? "+" : "-", t2);
      free(t1);
      free(t2);
      return(temp);
    }
  else 
    {
      return(gstr(rl));
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


static char *complex_arg(int a1, int a2, int s1, int s2)
{
  char *t1, *t2;
  
  sprintf(argbuf, "%s%s%s%si", (s1 == -1) ? "-" : "", t1 = gstr(double_args[a1]), (s2 == -1) ? "-" : "+", t2 = gstr(double_args[a2]));
  free(t1);
  free(t2);
  return(argbuf);
}



typedef struct {
  char *name;
  int type;
  off_t n;
  off_t d;
  double x;
  double complex z;
} arg_stuff;

#define NUMERIC_ARGS 12

static arg_stuff arg_data[NUMERIC_ARGS] = {
  {"1",            ARG_INT,     1,    0,  0.0,   0.0},
  {"1.0",          ARG_REAL,    0,    0,  1.0,   0.0},
  {"1/1",          ARG_RATIO,   1,    1,  0.0,   0.0},
  {"1.0+1.0i",     ARG_COMPLEX, 0,    0,  0.0,   1.0 + 1.0 * _Complex_I},
  {"0",            ARG_INT,     0,    0,  0.0,   0.0},
  {"0.0",          ARG_REAL,    0,    0,  0.0,   0.0},
  {"1234",         ARG_INT,     1234, 0,  0.0,   0.0},
  {"123.4",        ARG_REAL,    0,    0,  123.4, 0.0},
  {"1234/11",      ARG_RATIO,   1234, 11, 0.0,   0.0},
  {"1.234+1.234i", ARG_COMPLEX, 0,    0,  0.0,   1.234 + 1.234 * _Complex_I},
  {"-1.0+1.0i",    ARG_COMPLEX, 0,    0,  0.0,   -1.0 + 1.0 * _Complex_I},
  {"0.0+1.0i",     ARG_COMPLEX, 0,    0,  0.0,   0.0 + 1.0 * _Complex_I},
};

static char *add(int args, ...)
{
  int i, cur_arg, type = ARG_INT;
  va_list ap;
  char *temp;
  off_t on = 0, od = 1;
  double ox = 0.0;
  double complex oz = 0.0 + 0.0 * _Complex_I;
  va_start(ap, args);
  for (i = 0; i < args; i++)
    {
      cur_arg = va_arg(ap, int);
      if (arg_data[cur_arg].type > type)
	type = arg_data[cur_arg].type;
    }
  va_end(ap);
  va_start(ap, args);
  for (i = 0; i < args; i++)
    {
      cur_arg = va_arg(ap, int);
      switch (type)
	{
	case ARG_INT: 
	  on += arg_data[cur_arg].n; 
	  break;
	case ARG_RATIO: 
	  if (arg_data[cur_arg].type == ARG_INT)
	    ox += (double)arg_data[cur_arg].n;
	  else ox += (double)arg_data[cur_arg].n / (double)arg_data[cur_arg].d; 
	  break;
	case ARG_REAL: 
	  if (arg_data[cur_arg].type == ARG_INT)
	    ox += (double)arg_data[cur_arg].n;
	  else 
	    {
	      if (arg_data[cur_arg].type == ARG_RATIO)
		ox += (double)arg_data[cur_arg].n / (double)arg_data[cur_arg].d; 
	      else ox += arg_data[cur_arg].x;
	    }
	  break;
	case ARG_COMPLEX: 
	  switch (arg_data[cur_arg].type)
	    {
	    case ARG_INT: oz += (double)arg_data[cur_arg].n; break;
	    case ARG_RATIO: oz += (double)arg_data[cur_arg].n / (double)arg_data[cur_arg].d; break;
	    case ARG_REAL: oz += arg_data[cur_arg].x; break;
	    case ARG_COMPLEX: oz += arg_data[cur_arg].z; break;
	    }
	  break;
	}
    }
  va_end(ap);
  switch (type)
    {
    case ARG_INT: 
      temp = (char *)calloc(128, sizeof(char));
      sprintf(temp, "%lld", on);
      break;
    case ARG_RATIO: 
      temp = (char *)calloc(128, sizeof(char));
      c_rationalize(ox, 0.000001, &on, &od);
      if (od == 1)
	sprintf(temp, "%lld", on);
      else sprintf(temp, "%lld/%lld", on, od);
      break;
    case ARG_REAL:
      temp = gstr(ox);
      break;
    case ARG_COMPLEX:
      temp = split_complex_to_string(creal(oz), cimag(oz));
      break;
    }
  return(temp);
}

static char *subtract(int args, ...)
{
  int i, cur_arg, type = ARG_INT;
  va_list ap;
  char *temp;
  off_t on = 0, od = 1;
  double ox = 0.0;
  double complex oz = 0.0 + 0.0 * _Complex_I;
  va_start(ap, args);
  for (i = 0; i < args; i++)
    {
      cur_arg = va_arg(ap, int);
      if (arg_data[cur_arg].type > type)
	type = arg_data[cur_arg].type;
    }
  va_end(ap);
  va_start(ap, args);
  cur_arg = va_arg(ap, int);
  switch (type)
    {
    case ARG_INT: 
      on = arg_data[cur_arg].n; 
      break;
    case ARG_RATIO: 
      if (arg_data[cur_arg].type == ARG_INT)
	ox = (double)arg_data[cur_arg].n;
      else ox = (double)arg_data[cur_arg].n / (double)arg_data[cur_arg].d; 
      break;
    case ARG_REAL: 
      if (arg_data[cur_arg].type == ARG_INT)
	ox = (double)arg_data[cur_arg].n;
      else 
	{
	  if (arg_data[cur_arg].type == ARG_RATIO)
	    ox = (double)arg_data[cur_arg].n / (double)arg_data[cur_arg].d; 
	  else ox = arg_data[cur_arg].x;
	}
      break;
    case ARG_COMPLEX: 
      switch (arg_data[cur_arg].type)
	{
	case ARG_INT: oz = (double)arg_data[cur_arg].n; break;
	case ARG_RATIO: oz = (double)arg_data[cur_arg].n / (double)arg_data[cur_arg].d; break;
	case ARG_REAL: oz = arg_data[cur_arg].x; break;
	case ARG_COMPLEX: oz = arg_data[cur_arg].z; break;
	}
      break;
    }
  for (i = 1; i < args; i++)
    {
      cur_arg = va_arg(ap, int);
      switch (type)
	{
	case ARG_INT: 
	  on -= arg_data[cur_arg].n; 
	  break;
	case ARG_RATIO: 
	  if (arg_data[cur_arg].type == ARG_INT)
	    ox -= (double)arg_data[cur_arg].n;
	  else ox -= (double)arg_data[cur_arg].n / (double)arg_data[cur_arg].d; 
	  break;
	case ARG_REAL: 
	  if (arg_data[cur_arg].type == ARG_INT)
	    ox -= (double)arg_data[cur_arg].n;
	  else 
	    {
	      if (arg_data[cur_arg].type == ARG_RATIO)
		ox -= (double)arg_data[cur_arg].n / (double)arg_data[cur_arg].d; 
	      else ox -= arg_data[cur_arg].x;
	    }
	  break;
	case ARG_COMPLEX: 
	  switch (arg_data[cur_arg].type)
	    {
	    case ARG_INT: oz -= (double)arg_data[cur_arg].n; break;
	    case ARG_RATIO: oz -= (double)arg_data[cur_arg].n / (double)arg_data[cur_arg].d; break;
	    case ARG_REAL: oz -= arg_data[cur_arg].x; break;
	    case ARG_COMPLEX: oz -= arg_data[cur_arg].z; break;
	    }
	  break;
	}
    }
  va_end(ap);
  switch (type)
    {
    case ARG_INT: 
      temp = (char *)calloc(128, sizeof(char));
      sprintf(temp, "%lld", on);
      break;
    case ARG_RATIO: 
      temp = (char *)calloc(128, sizeof(char));
      c_rationalize(ox, 0.000001, &on, &od);
      if (od == 1)
	sprintf(temp, "%lld", on);
      else sprintf(temp, "%lld/%lld", on, od);
      break;
    case ARG_REAL:
      temp = gstr(ox);
      break;
    case ARG_COMPLEX:
      temp = split_complex_to_string(creal(oz), cimag(oz));
      break;
    }
  return(temp);
}

static char *multiply(int args, ...)
{
  int i, cur_arg, type = ARG_INT;
  va_list ap;
  char *temp;
  off_t on = 1, od = 1;
  double ox = 1.0;
  double complex oz = 1.0 + 0.0 * _Complex_I;
  va_start(ap, args);
  for (i = 0; i < args; i++)
    {
      cur_arg = va_arg(ap, int);
      if (arg_data[cur_arg].type > type)
	type = arg_data[cur_arg].type;
    }
  va_end(ap);
  va_start(ap, args);
  for (i = 0; i < args; i++)
    {
      cur_arg = va_arg(ap, int);
      switch (type)
	{
	case ARG_INT: 
	  on *= arg_data[cur_arg].n; 
	  break;
	case ARG_RATIO: 
	  if (arg_data[cur_arg].type == ARG_INT)
	    ox *= (double)arg_data[cur_arg].n;
	  else ox *= (double)arg_data[cur_arg].n / (double)arg_data[cur_arg].d; 
	  break;
	case ARG_REAL: 
	  if (arg_data[cur_arg].type == ARG_INT)
	    ox *= (double)arg_data[cur_arg].n;
	  else 
	    {
	      if (arg_data[cur_arg].type == ARG_RATIO)
		ox *= (double)arg_data[cur_arg].n / (double)arg_data[cur_arg].d; 
	      else ox *= arg_data[cur_arg].x;
	    }
	  break;
	case ARG_COMPLEX: 
	  switch (arg_data[cur_arg].type)
	    {
	    case ARG_INT: oz *= (double)arg_data[cur_arg].n; break;
	    case ARG_RATIO: oz *= (double)arg_data[cur_arg].n / (double)arg_data[cur_arg].d; break;
	    case ARG_REAL: oz *= arg_data[cur_arg].x; break;
	    case ARG_COMPLEX: oz *= arg_data[cur_arg].z; break;
	    }
	  break;
	}
    }
  va_end(ap);
  switch (type)
    {
    case ARG_INT: 
      temp = (char *)calloc(128, sizeof(char));
      sprintf(temp, "%lld", on);
      break;
    case ARG_RATIO: 
      temp = (char *)calloc(128, sizeof(char));
      c_rationalize(ox, 0.000001, &on, &od);
      if (od == 1)
	sprintf(temp, "%lld", on);
      else sprintf(temp, "%lld/%lld", on, od);
      break;
    case ARG_REAL:
      temp = gstr(ox);
      break;
    case ARG_COMPLEX:
      temp = split_complex_to_string(creal(oz), cimag(oz));
      break;
    }
  return(temp);
}

static char *divide(int args, ...)
{
  int i, cur_arg, type = ARG_INT;
  va_list ap;
  char *temp;
  off_t on = 0, od = 1;
  double ox = 0.0;
  double complex oz = 0.0 + 0.0 * _Complex_I;

  va_start(ap, args);
  for (i = 0; i < args; i++)
    {
      cur_arg = va_arg(ap, int);
      if (arg_data[cur_arg].type > type)
	type = arg_data[cur_arg].type;
    }
  va_end(ap);
  if (type == ARG_INT) type = ARG_RATIO;

  va_start(ap, args);
  cur_arg = va_arg(ap, int);
  switch (type)
    {
    case ARG_RATIO: 
      if (arg_data[cur_arg].type == ARG_INT)
	ox = (double)arg_data[cur_arg].n;
      else ox = (double)arg_data[cur_arg].n / (double)arg_data[cur_arg].d; 
      break;
    case ARG_REAL: 
      if (arg_data[cur_arg].type == ARG_INT)
	ox = (double)arg_data[cur_arg].n;
      else 
	{
	  if (arg_data[cur_arg].type == ARG_RATIO)
	    ox = (double)arg_data[cur_arg].n / (double)arg_data[cur_arg].d; 
	  else ox = arg_data[cur_arg].x;
	}
      break;
    case ARG_COMPLEX: 
      switch (arg_data[cur_arg].type)
	{
	case ARG_INT: oz = (double)arg_data[cur_arg].n; break;
	case ARG_RATIO: oz = (double)arg_data[cur_arg].n / (double)arg_data[cur_arg].d; break;
	case ARG_REAL: oz = arg_data[cur_arg].x; break;
	case ARG_COMPLEX: oz = arg_data[cur_arg].z; break;
	}
      break;
    }

  for (i = 1; i < args; i++)
    {
      cur_arg = va_arg(ap, int);
      switch (type)
	{
	case ARG_RATIO: 
	  if (arg_data[cur_arg].type == ARG_INT)
	    ox /= (double)arg_data[cur_arg].n;
	  else ox /= (double)arg_data[cur_arg].n / (double)arg_data[cur_arg].d; 
	  break;
	case ARG_REAL: 
	  if (arg_data[cur_arg].type == ARG_INT)
	    ox /= (double)arg_data[cur_arg].n;
	  else 
	    {
	      if (arg_data[cur_arg].type == ARG_RATIO)
		ox /= (double)arg_data[cur_arg].n / (double)arg_data[cur_arg].d; 
	      else ox /= arg_data[cur_arg].x;
	    }
	  break;
	case ARG_COMPLEX: 
	  switch (arg_data[cur_arg].type)
	    {
	    case ARG_INT: oz /= (double)arg_data[cur_arg].n; break;
	    case ARG_RATIO: oz /= (double)arg_data[cur_arg].n / (double)arg_data[cur_arg].d; break;
	    case ARG_REAL: oz /= arg_data[cur_arg].x; break;
	    case ARG_COMPLEX: oz /= arg_data[cur_arg].z; break;
	    }
	  break;
	}
    }
  va_end(ap);
  switch (type)
    {
    case ARG_RATIO: 
      temp = (char *)calloc(128, sizeof(char));
      c_rationalize(ox, 0.000001, &on, &od);
      if (od == 1)
	sprintf(temp, "%lld", on);
      else sprintf(temp, "%lld/%lld", on, od);
      break;
    case ARG_REAL:
      temp = gstr(ox);
      break;
    case ARG_COMPLEX:
      temp = split_complex_to_string(creal(oz), cimag(oz));
      break;
    }
  return(temp);
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
(define (throw . args) (raise args))\n\
\n\
(define (catch tag body error-handler)\n\
  (guard (err (else (apply error-handler (if (list? err) err (list err)))))\n\
	 (body)))\n");

  if (strcmp(scheme_name, "gauche") == 0)
    fprintf(fp, "\n\
(define object->string x->string)\n");

  if ((strcmp(scheme_name, "s7") == 0) ||
      (strcmp(scheme_name, "guile") == 0) ||
      (strcmp(scheme_name, "gambit") == 0) ||
      (strcmp(scheme_name, "gauche") == 0))
    fprintf(fp, "\n\
(define (format dest str . args)\n\
  (let ((len (string-length str))\n\
	(tilde #f)\n\
	(result \"\"))\n\
    (do ((i 0 (+ i 1)))\n\
	((= i len))\n\
      (let ((c (string-ref str i)))\n\
	(if (char=? c #\\~)\n\
	    (set! tilde #t)\n\
	    (if (not tilde)\n\
		(set! result (string-append result (string c)))\n\
		(begin\n\
		  (set! tilde #f)\n\
		  (if (member c (list #\\A #\\D #\\F))\n\
		      (begin\n\
			(set! result (string-append result (object->string (car args))))\n\
			(set! args (cdr args)))\n\
		      (if (char=? c #\\%%)\n\
			  (set! result (string-append result (string #\\newline)))\n\
			  (display (string-append \";unknown format directive: ~\" (string c))))))))))\n\
    result))\n\n");


  if ((strcmp(scheme_name, "s7") == 0) ||
      (strcmp(scheme_name, "guile") == 0) ||
      (strcmp(scheme_name, "gauche") == 0))
    fprintf(fp, "\n\
(defmacro test (tst expected)\n\
  `(let ((result (catch #t (lambda () ,tst) (lambda args 'error))))\n\
     (if (or (and (eq? ,expected 'error)\n\
		  (not (eq? result 'error)))\n\
             (and (eq? result 'error)\n\
                  (not (eq? ,expected 'error)))\n\
	     (and (eq? ,expected #t)\n\
		  (not result))\n\
	     (and (eq? ,expected #f)\n\
		  result)\n\
             (and (integer? ,expected)\n\
		  (integer? result)\n\
		  (not (= result ,expected))\n\
		  (or (< (abs ,expected) 1.0e9)\n\
		      (> (abs (- (log (abs ,expected)) (log (abs result)))) 1.0e-2)))\n\
	     (and (real? ,expected)\n\
		  (real? result)\n\
		  (> (abs (- result ,expected)) 1.0e-5)\n\
		  (or (< 1.0e-6 (abs ,expected) 1.0e6)\n\
		      (> (abs (- (log (abs ,expected)) (log (abs result)))) 1.0e-2)))\n\
	     (and (number? result)\n\
	          (or (not (real? ,expected))\n\
		      (not (real? result)))\n\
		  (or (and (> (abs (real-part (- result ,expected))) 1.0e-4)\n\
			   (> (abs (real-part (+ result ,expected))) 1.0e-4))\n\
		      (and (> (abs (imag-part (- result ,expected))) 1.0e-4)\n\
			   (> (abs (imag-part (+ result ,expected))) 1.0e-4)))))\n\
	 (display (format #f \";~A got ~A, but expected ~A~%%\" ',tst result ',expected)))))\n\n");


  if (strcmp(scheme_name, "gambit") == 0)
    fprintf(stderr, "\n\
(define-macro (test tst expected)\n\
  `(let ((result\n\
     (with-exception-handler (lambda e 'error)\n\
                             (lambda () ,tst))))\n\
     (if (or (and (eq? result 'error)\n\
                  (not (eq? ,expected 'error)))\n\
	     (and (eq? ,expected #t)\n\
		  (not result))\n\
	     (and (eq? ,expected #t)\n\
		  (not result))\n\
	     (and (eq? ,expected #f)\n\
		  result)\n\
             (and (integer? ,expected)\n\
		  (integer? result)\n\
		  (not (= result ,expected)))\n\
	     (and (real? ,expected)\n\
		  (real? result)\n\
		  (> (abs (- result ,expected)) 1.0e-5))\n\
	     (and (number? result)\n\
	          (or (not (real? ,expected))\n\
		      (not (real? result)))\n\
		  (or (and (> (abs (real-part (- result ,expected))) 1.0e-4)\n\
			   (> (abs (real-part (+ result ,expected))) 1.0e-4))\n\
		      (and (> (abs (imag-part (- result ,expected))) 1.0e-4)\n\
			   (> (abs (imag-part (+ result ,expected))) 1.0e-4)))))\n\
	 (display (format #f \";~A got ~A, but expected ~A~%%\" ',tst result ',expected)))))\n\n");


  for (i = 0; i < INT_ARGS; i++)
    {
      fprintf(fp, "(define int-%d %lld)\n", i, int_args[i]);
      fprintf(fp, "(define ratio-%d %lld/%lld)\n", i, int_args[i], int_args[3]);
    }

  for (i = 0; i < DOUBLE_ARGS; i++)
    {
      char *t1, *t2;
      fprintf(fp, "(define double-%d %s)\n", i, t1 = gstr(double_args[i]));
      fprintf(fp, "(define complex-%d %s+%si)\n", i, t1, t2 = gstr(double_args[3]));
      free(t1);
      free(t2);
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
(if (not (rational? 1/2)) (display (format #f \";1/2 is not rational?~%%\")))\n\
(if (not (rational? 2)) (display (format #f \";2 is not rational?~%%\")))\n\n");

  fprintf(fp, "\n\
(for-each\n\
 (lambda (n)\n\
   (if (not (even? n))\n\
       (display (format #f \";~A is not even?\" n))))\n\
 (list 0 2 1234 -4 -10000002 1000000006))\n\
\n\
(for-each\n\
 (lambda (n)\n\
   (if (odd? n)\n\
       (display (format #f \";~A is odd?\" n))))\n\
 (list 0 2 1234 -4 -10000002 1000000006))\n\
\n\
(for-each\n\
 (lambda (n)\n\
   (if (even? n)\n\
       (display (format #f \";~A is even?\" n))))\n\
 (list 1 -1 31 50001 543321))\n\
\n\
(for-each\n\
 (lambda (n)\n\
   (if (not (odd? n))\n\
       (display (format #f \";~A is not odd?\" n))))\n\
 (list 1 -1 31 50001 543321))\n\
\n\
(for-each\n\
 (lambda (n)\n\
   (if (not (positive? n))\n\
       (display (format #f \";~A is not positive?\" n))))\n\
 (list 1 123 123456123 1.4 0.001 1/2 124124124.2))\n\
\n\
(for-each\n\
 (lambda (n)\n\
   (if (negative? n)\n\
       (display (format #f \";~A is negative?\" n))))\n\
 (list 1 123 123456123 1.4 0.001 1/2 12341243124.2))\n\
\n\
(for-each\n\
 (lambda (n)\n\
   (if (positive? n)\n\
       (display (format #f \";~A is positive?\" n))))\n\
 (list -1 -123 -123456123 -3/2 -0.00001 -1.4 -123124124.1))\n\
\n\
(for-each\n\
 (lambda (n)\n\
   (if (not (negative? n))\n\
       (display (format #f \";~A is not negative?\" n))))\n\
 (list -1 -123 -123456123 -2/3 -0.00001 -1.4 -123124124.1))\n\
\n\
(for-each\n\
 (lambda (n)\n\
   (if (not (zero? n))\n\
       (display (format #f \";~A is not zero?\" n))))\n\
 (list 0 0.0 0+0i 0/1 0.0-0.0i))\n\
\n\
(for-each\n\
 (lambda (n)\n\
   (if (zero? n)\n\
       (display (format #f \";~A is zero?\" n))))\n\
 (list 1 1/100 -0.001 0.0+1.0i))\n\n\
(let* ((last-good 0)\n\
       (first-bad \n\
	(call/cc\n\
	 (lambda (return)\n\
	   (do ((i 1 (+ i 1))\n\
		(k 1 (* k 2)))\n\
	       ((= i 62) #f)\n\
	     (if (and (eqv? k (string->number (number->string k)))\n\
		      (eqv? (- k) (string->number (number->string (- k)))))\n\
		 (set! last-good k)\n\
		 (letrec ((search \n\
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
	   (do ((i 1 (+ i 1))\n\
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
	   (do ((i 1 (+ i 1))\n\
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
  (if (number? first-bad)\n\
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
  
  fprintf(fp, "\n\n");
  
  for (op = 0; op < OP_NAMES; op++)
    {
      for (i = 0; i < INT_ARGS; i++)
	{
	  result = anyf(op, int_to_complex(int_args[i]));
	  if (creal(result) == 54321.0) 
	    {
	      if (ask_maxima)
		fprintf(stderr, "print(\"(test (%s %lld)\", %s(%.14g), \")\");\n", 
			op_names[op], int_args[i],
			op_names[op], (double)int_args[i]);
	      continue;
	    }
	  if ((isnormal(creal(result))) &&
	      (isnormal(cimag(result))))
	    {
	      argstr = int_arg_name(i);
	      fprintf(fp, "(test (%s %s) ", op_names[op], argstr);
	      complex_to_file(fp, result);
	      fprintf(fp, ")\n");
	    }
	  if (int_args[i] != 0)
	    {
	      result = anyf(op, int_to_complex(-int_args[i]));
	      if (creal(result) == 54321.0) 
		{
		  if (ask_maxima)
		    fprintf(stderr, "print(\"(test (%s %lld)\", %s(%.14g), \")\");\n", 
			    op_names[op], -int_args[i],
			    op_names[op], -(double)int_args[i]);
		  continue;
		}
	      if ((isnormal(creal(result))) &&
		  (isnormal(cimag(result))))
		{
		  fprintf(fp, "(test (%s %lld) ", op_names[op], -int_args[i]);
		  complex_to_file(fp, result);
		  fprintf(fp, ")\n");
		}
	    }
	}
      
      for (i = 0; i < INT_ARGS; i++)
	for (j = 0; j < INT_ARGS; j++)
	  if (int_args[j] != 0)
	    {
	      result = anyf(op, float_to_complex((double)int_args[i]/(double)int_args[j]));
	      if (creal(result) == 54321.0) 
		{
		  if (ask_maxima)
		    fprintf(stderr, "print(\"(test (%s %lld/%lld)\", %s(%.14g), \")\");\n", 
			    op_names[op], int_args[i], int_args[j],
			    op_names[op], (double)int_args[i]/(double)int_args[j]);
		  continue;
		}
	      if ((isnormal(creal(result))) &&
		  (isnormal(cimag(result))))
		{
		  argstr = ratio_arg_name(i, j);
		  fprintf(fp, "(test (%s %s) ", op_names[op], ratio_arg_name(i, j));
		  complex_to_file(fp, result);
		  fprintf(fp, ")\n");
		}
	      if (int_args[i] != 0)
		{
		  result = anyf(op, float_to_complex(-(double)int_args[i]/(double)int_args[j]));
		  if (creal(result) == 54321.0) 
		    {
		      if (ask_maxima)
			fprintf(stderr, "print(\"(test (%s %lld/%lld)\", %s(%.14g), \")\");\n", 
				op_names[op], -int_args[i], int_args[j],
				op_names[op], -(double)int_args[i]/(double)int_args[j]);
		      continue;
		    }
		  if ((isnormal(creal(result))) &&
		      (isnormal(cimag(result))))
		    {
		      fprintf(fp, "(test (%s %lld/%lld) ", op_names[op], -int_args[i], int_args[j]);
		      complex_to_file(fp, result);
		      fprintf(fp, ")\n");
		    }
		}
	    }
      
      for (i = 0; i < DOUBLE_ARGS; i++)
	{
	  char *t1;
	  result = anyf(op, float_to_complex(double_args[i]));
	  if (creal(result) == 54321.0) 
	    {
	      if (ask_maxima)
		{
		  fprintf(stderr, "print(\"(test (%s %s)\", %s(%.14g), \")\");\n", 
			  op_names[op], t1 = gstr(double_args[i]),
			  op_names[op], double_args[i]);
		  free(t1);
		}
	      continue;
	    }
	  if ((isnormal(creal(result))) &&
	      (isnormal(cimag(result))))
	    {
	      argstr = double_arg_name(i);
	      fprintf(fp, "(test (%s %s) ", op_names[op], argstr);
	      complex_to_file(fp, result);
	      fprintf(fp, ")\n");
	    }
	  if (double_args[i] != 0.0)
	    {
	      result = anyf(op, float_to_complex(-double_args[i]));
	      if (creal(result) == 54321.0) 
		{
		  if (ask_maxima)
		    {
		      fprintf(stderr, "print(\"(test (%s %s)\", %s(%.14g), \")\");\n", 
			      op_names[op], t1 = gstr(-double_args[i]),
			      op_names[op], -double_args[i]);
		      free(t1);
		    }
		  continue;
		}
	      if ((isnormal(creal(result))) &&
		  (isnormal(cimag(result))))
		{
		  fprintf(fp, "(test (%s %s) ", op_names[op], t1 = gstr(-double_args[i]));
		  free(t1);
		  complex_to_file(fp, result);
		  fprintf(fp, ")\n");
		}
	    }
	}
      
      for (i = 0; i < DOUBLE_ARGS; i++)
	for (j = 0; j < DOUBLE_ARGS; j++)
	  for (k = 0; k < 4; k++)
	    {
	      char *t1, *t2;
	      int s1 = 1, s2 = 1;
	      switch (k)
		{
		case 0: break;
		case 1: s1 = -1; break;
		case 2: s2 = -1; break;
		case 3: s1 = -1; s2 = -1; break;
		}
	      result = anyf(op, s1 * double_args[i] + s2 * double_args[j] * _Complex_I);
	      if (creal(result) == 54321.0) 
		{
		  if (ask_maxima)
		    {
		      fprintf(stderr, "print(\"(test (%s %s+%si)\", %s(%.14g + %.14g*%%i), \")\");\n", 
			      op_names[op], t1 = gstr(double_args[i]), t2 = gstr(double_args[j]),
			      op_names[op], double_args[i], double_args[j]);
		      free(t1);
		      free(t2);
		    }
		  continue;
		}
	      if ((isnormal(creal(result))) &&
		  (isnormal(cimag(result))))
		{
		  argstr = complex_arg_name(i, j, s1, s2);
		  fprintf(fp, "(test (%s %s) ", op_names[op], argstr);
		  complex_to_file(fp, result);
		  fprintf(fp, ")\n");
		}
	    }
    }

  fprintf(fp, "\n\n\
(test (sin 10/3) -0.1905679628754527) \n\
(test (sin 1234/3) 0.213644699569724) \n\
(test (sin 1234/10) -0.7693905459455223) \n\
(test (sin 1234000000/3) .9985856466749967) \n\
(test (sin 1234000000/500029) -0.9907886154453116) \n\
(test (sin 1234000000/362880) .9798963032808383) \n\
(test (sin 500029/2) 0.270047165973401) \n\
(test (sin 500029/3) .7610322618332298) \n\
(test (sin 500029/10) .9665258739436294) \n\
(test (sin 500029/1234) .05553717596791147) \n\
(test (sin 362880/1234) -0.9463147870254296) \n\
(test (sin 3.14159265358979) -6.982889851335445E-15) \n\
(test (sin 2.71828182845905) .4107812905029501) \n\
(test (sin 3.14159265358979+0.0i) -6.982889851335445E-15) \n\
(test (sin 3.14159265358979+0.00000001000000i) -6.982889851335445E-15-1.0E-8i) \n\
(test (sin 3.14159265358979+1.0i) -1.077516210464362E-14-1.175201193643801i) \n\
(test (sin 3.14159265358979+3.14159265358979i) -8.094533288479446E-14-11.54873935725783i) \n\
(test (sin 3.14159265358979+2.71828182845905i) -5.314066559815525E-14-7.54413710281663i) \n\
(test (sin 2.71828182845905+0.0i) .4107812905029501) \n\
(test (sin 2.71828182845905+0.00000001000000i) .4107812905029501-9.117339147869465E-9i) \n\
(test (sin 2.71828182845905+1.0i) .6338686545195173-1.071470784943156i) \n\
(test (sin 2.71828182845905+3.14159265358979i) 4.761757525968664-10.52937734504676i) \n\
(test (sin 2.71828182845905+2.71828182845905i) 3.126097025348496-6.878245654440458i) \n\
(test (sin 1234.0+0.00000001000000i) .6019276547624973-7.985506235875843E-9i) \n\
(test (sin 1234.0+3.14159265358979i) 6.977517249251167-9.222253015388718i) \n\
(test (sin 1234.0+2.71828182845905i) 4.58074477716391-6.024375387884452i) \n\
(test (sin 1234000000.0+0.00000001000000i) -0.9872932128398908+1.5890913089022285E-9i) \n\
(test (sin 1234000000.0+3.14159265358979i) -11.44465679247962+1.835200134139553i) \n\
(test (sin 1234000000.0+2.71828182845905i) -7.513424898263172+1.198832270325275i) \n\
(test (cos -2/3) .7858872607769459) \n\
(test (cos -3/2) 0.0707372016677029) \n\
(test (cos -10/3) -0.9816740047110853) \n\
(test (cos 1234/3) -0.9769114301438807) \n\
(test (cos 1234/10) -0.6387786688749486) \n\
(test (cos 1234000000/3) -0.05316677773458422) \n\
(test (cos 1234000000/500029) .1354175745756898) \n\
(test (cos 1234000000/362880) .1995074806029773) \n\
(test (cos 500029/2) 0.962847094896035) \n\
(test (cos 500029/3) -0.6487140328750399) \n\
(test (cos 500029/10) .2565691622107056) \n\
(test (cos 500029/1234) -0.9984566200318916) \n\
(test (cos -500029/362880) .1916572772946199) \n\
(test (cos 362880/1234) .3232465372699541) \n\
(test (cos -362880/500029) .7480231586426291) \n\
(test (cos -3.14159265358979) -1.0) \n\
(test (cos -2.71828182845905) -0.9117339147869464) \n\
(test (cos 0.0+3.14159265358979i) 11.5919532755216) \n\
(test (cos 0.0+2.71828182845905i) 7.610125138661946) \n\
(test (cos 0.00000001000000+1.0i) 1.543080634815244-1.1752011936438014E-8i) \n\
(test (cos 0.00000001000000+3.14159265358979i) 11.5919532755216-1.154873935725783E-7i) \n\
(test (cos 0.00000001000000+2.71828182845905i) 7.610125138661945-7.54413710281663E-8i) \n\
(test (cos 1.0+0.00000001000000i) .5403023058681398-8.414709848078964E-9i) \n\
(test (cos 1.0+3.14159265358979i) 6.263159084280057-9.71792908024146i) \n\
(test (cos 1.0+2.71828182845905i) 4.111768160364146-6.348172477432901i) \n\
(test (cos 3.14159265358979+0.0i) -1.0) \n\
(test (cos 3.14159265358979+0.00000001000000i) -1.0+6.982889851335445E-23i) \n\
(test (cos 3.14159265358979+1.0i) -1.543080634815244+8.206300488372603E-15i) \n\
(test (cos 3.14159265358979+3.14159265358979i) -11.5919532755216+8.064357485351393E-14i) \n\
(test (cos 3.14159265358979+2.71828182845905i) -7.610125138661946+5.267987841234144E-14i) \n\
(test (cos 2.71828182845905+0.0i) -0.9117339147869464) \n\
(test (cos 2.71828182845905+0.00000001000000i) -0.9117339147869464-4.1078129050295015E-9i) \n\
(test (cos 2.71828182845905+1.0i) -1.406878948012029-0.4827506629256081i) \n\
(test (cos 2.71828182845905+3.14159265358979i) -10.56877693991868-4.744006056856582i) \n\
(test (cos 2.71828182845905+2.71828182845905i) -6.93840918469081-3.098990374826203i) \n\
(test (cos 1234.0+0.00000001000000i) -0.7985506235875843-6.019276547624973E-9i) \n\
(test (cos 1234.0+3.14159265358979i) -9.256761516765916-6.951505596777556i) \n\
(test (cos 1234.0+2.71828182845905i) -6.077070175058048-4.541024753505155i) \n\
(test (cos 1234000000.0+0.00000001000000i) +0.1589091308902228+9.872932128398908E-9i) \n\
(test (cos 1234000000.0+3.14159265358979i) +1.84206722033321+11.40199198427758i) \n\
(test (cos 1234000000.0+2.71828182845905i) +1.209318371750606+7.448275358344457i) \n\
(test (tan 10/3) .1941255059835657) \n\
(test (tan 1234/3) -0.2186940320047828) \n\
(test (tan 1234/10) 1.204471256531804) \n\
(test (tan 1234000000/3) -18.78094727276203)\n\
(test (tan 1234000000/500029) -7.31654379832009) \n\
(test (tan 1234000000/362880) 4.911576750502133) \n\
(test (tan 500029/2) .2804673425353792) \n\
(test (tan 500029/3) -1.173139817032177) \n\
(test (tan 500029/10) 3.767116303516932) \n\
(test (tan 500029/1234) -0.05562302342803592) \n\
(test (tan 362880/1234) -2.927532635052267) \n\
(test (tan 3.14159265358979) 6.982889851335445E-15) \n\
(test (tan 2.71828182845905) -0.4505495340698621) \n\
(test (tan 0.00000001000000+1234.0i) +8.077935669463161E-28+1.0i) \n\
(test (tan 0.00000001000000+1234000000.0i) 0.0+1.0i) \n\
(test (tan 3.14159265358979+0.0i) 6.982889851335445E-15) \n\
(test (tan 3.14159265358979+0.00000001000000i) +6.982889851335444E-15+1.0E-8i) \n\
(test (tan 3.14159265358979+1.0i) +2.932634567877868E-15+0.7615941559557649i) \n\
(test (tan 3.14159265358979+3.14159265358979i) +5.196631812627532E-17+0.99627207622075i) \n\
(test (tan 3.14159265358979+2.71828182845905i) +1.205734242765375E-16+0.991328915800599i) \n\
(test (tan 3.14159265358979+1234.0i) 0.0+1.0i) \n\
(test (tan 3.14159265358979+1234000000.0i) -7.703719777548943E-34+1.0i) \n\
(test (tan 2.71828182845905+0.0i) -0.4505495340698621) \n\
(test (tan 2.71828182845905+0.00000001000000i) -0.4505495340698621+1.2029948826505699E-8i) \n\
(test (tan 2.71828182845905+1.0i) -0.1692870118766369+0.8196826057997404i) \n\
(test (tan 2.71828182845905+3.14159265358979i) -0.002790687681003331+0.9975247319761639i) \n\
(test (tan 2.71828182845905+2.71828182845905i) -0.00648578276962794+0.9942257438545914i) \n\
(test (tan 2.71828182845905+1234.0i) +2.710505431213761E-20+1.0i) \n\
(test (tan 2.71828182845905+1234000000.0i) +2.710505431213761E-20+1.0i) \n\
(test (tan 1234.0+0.00000001000000i) -0.7537751984442328+1.5681770497896427E-8i) \n\
(test (tan 1234.0+3.14159265358979i) -0.003586791196867043+0.9989656315245496i) \n\
(test (tan 1234.0+2.71828182845905i) -0.008351965390936033+0.9975698313220817i) \n\
(test (tan 1234000000.0+0.00000001000000i) -6.212941995900324+3.9600648244422054E-7i) \n\
(test (tan 1234000000.0+3.14159265358979i) -0.001176098307980411+1.003551866736695i) \n\
(test (tan 1234000000.0+2.71828182845905i) -0.002755390838840499+1.008299558244272i) \n\
(test (asin 1234000000/3) 1.570796326794897-20.52806165432143i) \n\
(test (asin 500029/2) 1.570796326794897-13.12242137571839i) \n\
(test (asin 0.00000001000000+1234000000.0i) +8.103725052149596E-18+21.62667380877375i) \n\
(test (asin 3.14159265358979+1234000000.0i) +2.545860611523387E-9+21.62667380877375i) \n\
(test (asin 2.71828182845905+1234000000.0i) +2.2028207814967068E-9+21.62667380877375i) \n\
(test (asin 1234000000.0+0.00000001000000i) +1.570796327200083+21.62667394298955i) \n\
(test (asin 1234000000.0+3.14159265358979i) +1.570796327200083+21.62667394298955i) \n\
(test (acos 0.00000001000000+1234000000.0i) 1.570796326794897-21.62667380877375i) \n\
(test (acos 3.14159265358979+1234000000.0i) 1.570796324249036-21.62667380877375i) \n\
(test (acos 2.71828182845905+1234000000.0i) 1.570796324592076-21.62667380877375i) \n\
(test (acos 1234000000.0+0.00000001000000i) -4.051865509779873E-10-21.62667394298955i) \n\
(test (acos 1234000000.0+3.14159265358979i) -4.051865509779873E-10-21.62667394298955i) \n\
(test (acos 1234000000.0+2.71828182845905i) -4.051865509779873E-10-21.62667394298955i) \n\
(test (atan 1234000000/3) 1.570796324363778) \n\
(test (atan 0.00000001000000+1234000000.0i) +1.570796326794897+8.103727714748784E-10i) \n\
(test (atan 3.14159265358979+1234000000.0i) +1.570796326794897+8.103727714748784E-10i) \n\
(test (atan 2.71828182845905+1234000000.0i) +1.570796326794897+8.103727714748784E-10i) \n\
(test (atan 1234000000.0+0.00000001000000i) +1.570796325984524+6.567040287478756E-27i) \n\
(test (atan 1234000000.0+3.14159265358979i) +1.570796325984524+2.063096552297151E-18i) \n\
(test (atan 1234000000.0+2.71828182845905i) +1.570796325984524+1.785106628021167E-18i) \n\
(test (sinh 1234/3) 2.18155865313939E+178) \n\
(test (sinh 1234/10) 1.953930316004457E+53) \n\
(test (sinh 0.0+3.14159265358979i) 0.0-6.982889851335445E-15i) \n\
(test (sinh 0.0+2.71828182845905i) 0.0+0.4107812905029501i) \n\
(test (sinh 0.00000001000000+3.14159265358979i) 1.0e-8-6.982889851335445E-15i) \n\
(test (sinh 0.00000001000000+2.71828182845905i) -9.117339147869465E-9+0.4107812905029501i) \n\
(test (sinh 0.00000001000000+1234.0i) -7.985506235875843E-9+0.6019276547624973i) \n\
(test (sinh 0.00000001000000+1234000000.0i) 1.5890913089022285E-9-0.9872932128398908i) \n\
(test (sinh 1.0+3.14159265358979i) -1.175201193643801-1.077516210464362E-14i) \n\
(test (sinh 1.0+2.71828182845905i) -1.071470784943156+0.6338686545195173i) \n\
(test (sinh 3.14159265358979+3.14159265358979i) -11.54873935725783-8.094533288479446E-14i) \n\
(test (sinh 3.14159265358979+2.71828182845905i) -10.52937734504676+4.761757525968664i) \n\
(test (sinh 3.14159265358979+1234.0i) -9.222253015388718+6.977517249251167i) \n\
(test (sinh 3.14159265358979+1234000000.0i) 1.835200134139553-11.44465679247962i) \n\
(test (sinh 2.71828182845905+3.14159265358979i) -7.54413710281663-5.314066559815525E-14i) \n\
(test (sinh 2.71828182845905+2.71828182845905i) -6.878245654440458+3.126097025348496i) \n\
(test (sinh 2.71828182845905+1234.0i) -6.024375387884452+4.58074477716391i) \n\
(test (sinh 2.71828182845905+1234000000.0i) 1.198832270325275-7.513424898263172i) \n\
(test (cosh -2/3) 1.230575580043636) \n\
(test (cosh -3/2) 2.352409615243247) \n\
(test (cosh -10/3) 14.03364944393623) \n\
(test (cosh 1234/3) 2.18155865313939E+178) \n\
(test (cosh 1234/10) 1.953930316004457E+53) \n\
(test (cosh 500029/1234) 4.77955809407816E+175) \n\
(test (cosh -500029/362880) 2.109420464922257) \n\
(test (cosh 362880/1234) 2.57697781296564E+127) \n\
(test (cosh -362880/500029) 1.275095456482107) \n\
(test (cosh -3.14159265358979) 11.5919532755216) \n\
(test (cosh -2.71828182845905) 7.610125138661946) \n\
(test (cosh 0.0+3.14159265358979i) -1.0) \n\
(test (cosh 0.0+2.71828182845905i) -0.9117339147869464) \n\
(test (cosh 0.00000001000000+1.0i) +0.5403023058681398+8.414709848078964E-9i) \n\
(test (cosh 0.00000001000000+3.14159265358979i) -1.0-6.982889851335445E-23i) \n\
(test (cosh 0.00000001000000+2.71828182845905i) -0.9117339147869464+4.1078129050295015E-9i) \n\
(test (cosh 0.00000001000000+1234.0i) -0.7985506235875843+6.019276547624973E-9i) \n\
(test (cosh 0.00000001000000+1234000000.0i) .1589091308902228-9.872932128398908E-9i) \n\
(test (cosh 1.0+0.00000001000000i) +1.543080634815244+1.1752011936438014E-8i) \n\
(test (cosh 1.0+3.14159265358979i) -1.543080634815244-8.206300488372603E-15i) \n\
(test (cosh 1.0+2.71828182845905i) -1.406878948012029+0.4827506629256081i) \n\
(test (cosh 3.14159265358979+0.0i) 11.5919532755216) \n\
(test (cosh 3.14159265358979+0.00000001000000i) +11.5919532755216+1.154873935725783E-7i) \n\
(test (cosh 3.14159265358979+1.0i) +6.263159084280057+9.71792908024146i) \n\
(test (cosh 3.14159265358979+3.14159265358979i) -11.5919532755216-8.064357485351393E-14i) \n\
(test (cosh 3.14159265358979+2.71828182845905i) -10.56877693991868+4.744006056856582i) \n\
(test (cosh 3.14159265358979+1234.0i) -9.256761516765916+6.951505596777556i) \n\
(test (cosh 3.14159265358979+1234000000.0i) 1.84206722033321-11.40199198427758i) \n\
(test (cosh 2.71828182845905+0.0i) 7.610125138661946) \n\
(test (cosh 2.71828182845905+0.00000001000000i) +7.610125138661945+7.54413710281663E-8i) \n\
(test (cosh 2.71828182845905+1.0i) +4.111768160364146+6.348172477432901i) \n\
(test (cosh 2.71828182845905+3.14159265358979i) -7.610125138661946-5.267987841234144E-14i) \n\
(test (cosh 2.71828182845905+2.71828182845905i) -6.93840918469081+3.098990374826203i) \n\
(test (cosh 2.71828182845905+1234.0i) -6.077070175058048+4.541024753505155i) \n\
(test (cosh 2.71828182845905+1234000000.0i) 1.209318371750606-7.448275358344457i) \n\
(test (tanh 1234/3) 1.0) \n\
(test (tanh 1234000000/3) 1.0) \n\
(test (tanh 1234000000/500029) 1.0) \n\
(test (tanh 1234000000/362880) 1.0) \n\
(test (tanh 500029/2) 1.0) \n\
(test (tanh 500029/3) 1.0) \n\
(test (tanh 500029/10) 1.0) \n\
(test (tanh 500029/1234) 1.0) \n\
(test (tanh 0.0+3.14159265358979i) 0.0+6.982889851335445E-15i) \n\
(test (tanh 0.0+2.71828182845905i) 0.0-0.4505495340698621i) \n\
(test (tanh 0.00000001000000+3.14159265358979i) 1.0e-8+6.982889851335444E-15i) \n\
(test (tanh 0.00000001000000+2.71828182845905i) 1.2029948826505699E-8-0.4505495340698621i) \n\
(test (tanh 0.00000001000000+1234.0i) 1.5681770497896427E-8-0.7537751984442328i) \n\
(test (tanh 0.00000001000000+1234000000.0i) 3.9600648244422054E-7-6.212941995900324i) \n\
(test (tanh 1.0+3.14159265358979i) +0.7615941559557649+2.932634567877868E-15i) \n\
(test (tanh 1.0+2.71828182845905i) .8196826057997404-0.1692870118766369i) \n\
(test (tanh 3.14159265358979+3.14159265358979i) +0.99627207622075+5.196631812627532E-17i) \n\
(test (tanh 3.14159265358979+2.71828182845905i) .9975247319761639-0.002790687681003331i) \n\
(test (tanh 3.14159265358979+1234.0i) .9989656315245496-0.003586791196867043i) \n\
(test (tanh 3.14159265358979+1234000000.0i) 1.003551866736695-0.001176098307980411i) \n\
(test (tanh 2.71828182845905+3.14159265358979i) +0.991328915800599+1.205734242765375E-16i) \n\
(test (tanh 2.71828182845905+2.71828182845905i) .9942257438545914-0.00648578276962794i) \n\
(test (tanh 2.71828182845905+1234.0i) .9975698313220817-0.008351965390936033i) \n\
(test (tanh 2.71828182845905+1234000000.0i) 1.008299558244272-0.002755390838840499i) \n\
(test (tanh 1234.0+0.00000001000000i) +1.0+8.077935669463161E-28i) \n\
(test (tanh 1234.0+3.14159265358979i) 1.0) \n\
(test (tanh 1234.0+2.71828182845905i) +1.0+2.710505431213761E-20i) \n\
(test (tanh 1234000000.0+0.00000001000000i) 1.0) \n\
(test (tanh 1234000000.0+3.14159265358979i) 1.0-7.703719777548943E-34i) \n\
(test (tanh 1234000000.0+2.71828182845905i) +1.0+2.710505431213761E-20i) \n\
(test (asinh -1234000000/3) -20.52806165432143) \n\
(test (asinh -500029/2) -13.12242137572639) \n\
(test (asinh 0.00000001000000+1234000000.0i) +21.62667394298955+1.570796327200083i) \n\
(test (asinh 3.14159265358979+1234000000.0i) +21.62667394298955+1.570796327200083i) \n\
(test (asinh 2.71828182845905+1234000000.0i) +21.62667394298955+1.570796327200083i) \n\
(test (asinh 1234000000.0+0.00000001000000i) +21.62667380877375+8.103725052149596E-18i) \n\
(test (asinh 1234000000.0+2.71828182845905i) +21.62667380877375+2.2028207814967068E-9i) \n\
(test (exp 0.00000001000000+1234.0i) -0.7985506315730906+0.601927660781774i) \n\
(test (exp 0.00000001000000+1234000000.0i) .1589091324793142-0.987293222712823i) \n\
(test (exp 3.14159265358979+1234.0i) -18.47901453215463+13.92902284602872i) \n\
(test (exp 3.14159265358979+1234000000.0i) 3.677267354472762-22.8466487767572i) \n\
(test (exp 2.71828182845905+1234.0i) -12.1014455629425+9.121769530669065i) \n\
(test (exp 2.71828182845905+1234000000.0i) 2.408150642075881-14.96170025660763i)\n");


  fprintf(fp, "\n;;; --------------------------------------------------------------------------------\n");
  
  /* int ^ int */
  for (i = 0; i < INT_ARGS; i++)
    for (j = 0; j < INT_ARGS; j++)
      {
	result = expt_op(float_to_complex((double)int_args[i]), float_to_complex((double)int_args[j]));
	if (creal(result) == 54321.0) 
	  {
	    continue;
	  }
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
	    if (creal(result) == 54321.0) 
	      {
		continue;
	      }
	    if ((isnormal(creal(result))) &&
		(isnormal(cimag(result))) &&
		(cabs(result) < 1.0e9) &&
		((int_args[i] != 0) || (int_args[j] != 0)))
	      {
		fprintf(fp, "(test (expt %lld %lld) ", -int_args[i], int_args[j]);
		fprintf(fp, "%lld", (off_t)oround(creal(result)));
		fprintf(fp, ")\n");
	      }
	    result = expt_op(float_to_complex((double)int_args[i]), float_to_complex(-(double)int_args[j]));
	    if (creal(result) == 54321.0) 
	      {
		continue;
	      }
	    if ((isnormal(creal(result))) &&
		(isnormal(cimag(result))) &&
		((strcmp(scheme_name, "gauche") != 0) ||
		 ((int_args[i] < 100) && (int_args[j] < 100))))
	      {
		fprintf(fp, "(test (expt %lld %lld) ", int_args[i], -int_args[j]);
		complex_to_file(fp, result);
		fprintf(fp, ")\n");
	      }
	  }
      }
  
  /* ratio ^ ratio */
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
	    if (creal(result) == 54321.0) 
	      {
		continue;
	      }
	    if ((isnormal(creal(result))) &&
		(isnormal(cimag(result))) &&
		(cabs(result) < 1.0e9) &&
		((include_big_fractions_in_expt) ||
		 ((int_args[i] < 1000) && (int_args[j] < 1000) && (int_args[arg3] < 1000) && (int_args[arg4] < 1000))) &&
		((int_args[i] != 0) || (int_args[arg3] != 0)))
	      {
		fprintf(fp, "(test (expt %lld/%lld %lld/%lld) ", int_args[i], int_args[j], int_args[arg3], int_args[arg4]);
		complex_to_file(fp, result);
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
	    if (creal(result) == 54321.0) 
	      {
		continue;
	      }
	    if ((isnormal(creal(result))) &&
		(isnormal(cimag(result))) &&
		(cabs(result) < 1.0e9) &&
		((include_big_fractions_in_expt) ||
		 ((int_args[i] < 1000) && (int_args[j] < 1000) && (int_args[arg3] < 1000) && (int_args[arg4] < 1000))) &&
		((int_args[i] != 0) || (int_args[arg3] != 0)))
	      {
		fprintf(fp, "(test (expt %lld/%lld %lld/%lld) ", -int_args[i], int_args[j], int_args[arg3], int_args[arg4]);
		complex_to_file(fp, result);
		fprintf(fp, ")\n");
	      }
	    
	  }
      }

  /* int ^ double */
  for (i = 0; i < INT_ARGS; i++)
    for (j = 0; j < DOUBLE_ARGS; j++)
      {
	char *t1;
	result = expt_op(float_to_complex((double)int_args[i]), float_to_complex(double_args[j]));
	if (creal(result) == 54321.0) 
	  {
	    continue;
	  }
	if ((isnormal(creal(result))) &&
	    (isnormal(cimag(result))) &&
	    (cabs(result) < 1.0e9))
	  {
	    fprintf(fp, "(test (expt %lld %s) ", int_args[i], t1 = gstr(double_args[j]));
	    free(t1);
	    complex_to_file(fp, result);
	    fprintf(fp, ")\n");
	  }
	if (int_args[i] != 0)
	  {
	    result = expt_op(float_to_complex(-(double)int_args[i]), float_to_complex(double_args[j]));
	    if (creal(result) == 54321.0) 
	      {
		continue;
	      }
	    if ((isnormal(creal(result))) &&
		(isnormal(cimag(result))) &&
		(cabs(result) < 1.0e9) &&
		((int_args[i] != 0) || double_args[j] != 0.0))
	      {
		fprintf(fp, "(test (expt %lld %s) ", -int_args[i], t1 = gstr(double_args[j]));
		free(t1);
		complex_to_file(fp, result);			
		fprintf(fp, ")\n");
	      }
	    result = expt_op(float_to_complex((double)int_args[i]), float_to_complex(-double_args[j]));
	    if (creal(result) == 54321.0) 
	      {
		continue;
	      }
	    if ((isnormal(creal(result))) &&
		(isnormal(cimag(result))) &&
		((int_args[i] != 0) || double_args[j] != 0.0))
	      {
		fprintf(fp, "(test (expt %lld %s) ", int_args[i], t1 = gstr(-double_args[j]));
		free(t1);
		complex_to_file(fp, result);
		fprintf(fp, ")\n");
	      }
	  }
      }
  
  /* double ^ double */
  for (i = 0; i < DOUBLE_ARGS; i++)
    for (j = 0; j < DOUBLE_ARGS; j++)
      {
	char *t1, *t2;
	result = expt_op(float_to_complex(double_args[i]), float_to_complex(double_args[j]));
	if (creal(result) == 54321.0) 
	  {
	    continue;
	  }
	if ((isnormal(creal(result))) &&
	    (isnormal(cimag(result))) &&
	    (cabs(result) < 1.0e9))
	  {
	    fprintf(fp, "(test (expt %s %s) ", t2 = gstr(double_args[i]), t1 = gstr(double_args[j]));
	    free(t1);
	    free(t2);
	    complex_to_file(fp, result);
	    fprintf(fp, ")\n");
	  }

	result = expt_op(float_to_complex(-double_args[i]), float_to_complex(double_args[j]));
	if (creal(result) == 54321.0) 
	  {
	    continue;
	  }
	if ((isnormal(creal(result))) &&
	    (isnormal(cimag(result))) &&
	    (cabs(result) < 1.0e9))
	  {
	    fprintf(fp, "(test (expt %s %s) ", t2 = gstr(-double_args[i]), t1 = gstr(double_args[j]));
	    free(t1);
	    free(t2);
	    complex_to_file(fp, result);			
	    fprintf(fp, ")\n");
	  }

	result = expt_op(float_to_complex(double_args[i]), float_to_complex(-double_args[j]));
	if (creal(result) == 54321.0) 
	  {
	    continue;
	  }
	if ((isnormal(creal(result))) &&
	    (isnormal(cimag(result))) &&
	    (cabs(result) < 1.0e9))
	  {
	    fprintf(fp, "(test (expt %s %s) ", t2 = gstr(double_args[i]), t1 = gstr(-double_args[j]));
	    free(t1);
	    free(t2);
	    complex_to_file(fp, result);			
	    fprintf(fp, ")\n");
	  }

	result = expt_op(float_to_complex((double)-double_args[i]), float_to_complex(-double_args[j]));
	if (creal(result) == 54321.0) 
	  {
	    continue;
	  }
	if ((isnormal(creal(result))) &&
	    (isnormal(cimag(result))))
	  {
	    fprintf(fp, "(test (expt %s %s) ", t2 = gstr(-double_args[i]), t1 = gstr(-double_args[j]));
	    free(t1);
	    free(t2);
	    complex_to_file(fp, result);
	    fprintf(fp, ")\n");
	  }
      }
  
  /* double ^ int */
  for (i = 0; i < DOUBLE_ARGS; i++)
    for (j = 0; j < INT_ARGS; j++)
      {
	char *t2;
	result = expt_op(float_to_complex(double_args[i]), float_to_complex((double)int_args[j]));
	if (creal(result) == 54321.0) 
	  {
	    continue;
	  }
	if ((isnormal(creal(result))) &&
	    (isnormal(cimag(result))) &&
	    (cabs(result) < 1.0e9) &&
	    ((double_args[i] != 0.0) || int_args[i] != 0))
	  {
	    fprintf(fp, "(test (expt %s %lld) ", t2 = gstr(double_args[i]), int_args[j]);
	    free(t2);
	    complex_to_file(fp, result);
	    fprintf(fp, ")\n");
	  }
	result = expt_op(float_to_complex(-double_args[i]), float_to_complex((double)int_args[j]));
	if (creal(result) == 54321.0) 
	  {
	    continue;
	  }
	if ((isnormal(creal(result))) &&
	    (isnormal(cimag(result))) &&
	    (cabs(result) < 1.0e9) &&
	    ((double_args[i] != 0.0) || int_args[i] != 0))
	  {
	    fprintf(fp, "(test (expt %s %lld) ", t2 = gstr(-double_args[i]), int_args[j]);
	    free(t2);
	    complex_to_file(fp, result);			
	    fprintf(fp, ")\n");
	  }
	result = expt_op(float_to_complex(double_args[i]), float_to_complex((double)-int_args[j]));
	if (creal(result) == 54321.0) 
	  {
	    continue;
	  }
	if ((isnormal(creal(result))) &&
	    (isnormal(cimag(result))) &&
	    (cabs(result) < 1.0e9) &&
	    ((double_args[i] != 0.0) || int_args[i] != 0))
	  {
	    fprintf(fp, "(test (expt %s %lld) ", t2 = gstr(double_args[i]), -int_args[j]);
	    free(t2);
	    complex_to_file(fp, result);			
	    fprintf(fp, ")\n");
	  }
	result = expt_op(float_to_complex(-double_args[i]), float_to_complex(-(double)int_args[j]));
	if (creal(result) == 54321.0) 
	  {
	    continue;
	  }
	if ((isnormal(creal(result))) &&
	    (isnormal(cimag(result))) &&
	    ((double_args[i] != 0.0) || int_args[i] != 0))
	  {
	    fprintf(fp, "(test (expt %s %lld) ", t2 = gstr(-double_args[i]), -int_args[j]);
	    free(t2);
	    complex_to_file(fp, result);
	    fprintf(fp, ")\n");
	  }
      }

#if 0
  int ^ rat
  rat ^ int
  rat ^ double
  rat ^ cmp
    
    cmp ^ cmp
    cmp ^ double
    int ^ cmp
    cmp ^ int

	int arg3, arg4;
	arg3 = c_mod(i + j, INT_ARGS);
	arg4 = c_mod(i - j, INT_ARGS);
#endif
  

  fprintf(fp, "\n;;; --------------------------------------------------------------------------------\n");


  {
    #define ERROR_ARGS 7
    double error_args[ERROR_ARGS] = {
      1.0, 0.5, 0.1, .001, 0.003, .00002, 0.00000001
    };

    #define RAT_ARGS 13
    static double rat_args[RAT_ARGS] = {
      0.0, 0.00000001, 1.0, 
      3.141592653589793, 2.718281828459045, 
      1234.1234, 1234000000.01234,
      0.33, 0.9999, 0.501, 0.499, 1.501, 1.499
    };

    for (i = 0; i < RAT_ARGS; i++)
      for (j = 0; j < ERROR_ARGS; j++)
	{
	  off_t num = 0, den = 0;
	  bool happy = true;
	  char *t1, *t2;
	  if (use_continued_fractions_in_rationalize)
	    happy = continued_fraction_rationalize(rat_args[i], error_args[j], &num, &den);
	  else happy = c_rationalize(rat_args[i], error_args[j], &num, &den);
	  if (happy)
	    {
	      fprintf(fp, "(test (rationalize %s %s) %lld/%lld)\n", 
		      t2 = gstr(rat_args[i]), 
		      t1 = gstr(error_args[j]),
		      num, den);
	      free(t1);
	      free(t2);
	    }
	  else fprintf(stderr, "oops: %f %f?", rat_args[i], error_args[j]);

	  if (use_continued_fractions_in_rationalize)
	    happy = continued_fraction_rationalize(-rat_args[i], error_args[j], &num, &den);
	  else happy = c_rationalize(-rat_args[i], error_args[j], &num, &den);
	  if (happy)
	    {
	      fprintf(fp, "(test (rationalize %s %s) %lld/%lld)\n", 
		      t2 = gstr(-rat_args[i]), 
		      t1 = gstr(error_args[j]),
		      num, den);
	      free(t1);
	      free(t2);
	    }
	  else fprintf(stderr, "oops: %f %f?", rat_args[i], error_args[j]);
	}
  }

  fprintf(fp, "\n\n\
(test (gcd) 0)\n\
(test (lcm) 1)\n\
(test (gcd 1.4 2.3) 'error)\n\
(test (lcm 1.4 2.3) 'error)\n\
(test (gcd 2/3 1) 'error)\n\
(test (lcm 2/3 1) 'error)\n\
(test (gcd 2 1.0+0.5i) 'error)\n\
(test (lcm 2 1.0+0.5i) 'error)\n");

  for (i = 0; i < INT_ARGS; i++)
    {
      fprintf(fp, "(test (gcd %lld) %lld)\n", int_args[i], int_args[i]);
      fprintf(fp, "(test (lcm %lld) %lld)\n", int_args[i], int_args[i]);
      fprintf(fp, "(test (gcd %lld) %lld)\n", -int_args[i], int_args[i]);
      fprintf(fp, "(test (lcm %lld) %lld)\n", -int_args[i], int_args[i]);
    }

  for (i = 0; i < INT_ARGS; i++)
    for (j = 0; j < INT_ARGS; j++)
      {
	fprintf(fp, "(test (gcd %lld %lld) %lld)\n", int_args[i], int_args[j], c_gcd(int_args[i], int_args[j]));
	fprintf(fp, "(test (lcm %lld %lld) %lld)\n", int_args[i], int_args[j], c_lcm(int_args[i], int_args[j]));

	fprintf(fp, "(test (gcd %lld %lld) %lld)\n", -int_args[i], int_args[j], c_gcd(-int_args[i], int_args[j]));
	fprintf(fp, "(test (lcm %lld %lld) %lld)\n", -int_args[i], int_args[j], c_lcm(-int_args[i], int_args[j]));

	fprintf(fp, "(test (gcd %lld %lld) %lld)\n", int_args[i], -int_args[j], c_gcd(int_args[i], -int_args[j]));
	fprintf(fp, "(test (lcm %lld %lld) %lld)\n", int_args[i], -int_args[j], c_lcm(int_args[i], -int_args[j]));

	fprintf(fp, "(test (gcd %lld %lld) %lld)\n", -int_args[i], -int_args[j], c_gcd(-int_args[i], -int_args[j]));
	fprintf(fp, "(test (lcm %lld %lld) %lld)\n", -int_args[i], -int_args[j], c_lcm(-int_args[i], -int_args[j]));
      }

  k = 0;
  for (i = 0; i < INT_ARGS; i++)
    for (j = 0; j < INT_ARGS; j++)
      {
	int a1, a2, s1 = 1, s2 = 1;
	a1 = cint_mod(i + j, INT_ARGS);
	a2 = cint_mod(i - j, INT_ARGS);
	if (a2 == 0) a2 = 4;
	switch (k)
	  {
	  case 0: s1 = 1; s2 = 1; break;
	  case 1: s1 = -1; s2 = 1; break;
	  case 2: s1 = 1; s2 = -1; break;
	  case 3: s1 = -1; s2 = -1; break;
	  }
	k++;
	if (k == 4) k = 0;
	fprintf(fp, "(test (gcd %lld %lld %lld %lld) %lld)\n", 
		int_args[i], int_args[j], s1 * int_args[a1], s2 * int_args[a2],
		c_gcd(s2 * int_args[a2], c_gcd(s1 * int_args[a1], c_gcd(int_args[i], int_args[j]))));

	if ((int_args[i] < 10000) && 
	    (int_args[j] < 10000) &&
	    (int_args[a1] < 10000) &&
	    (int_args[a2] < 10000))
	  fprintf(fp, "(test (lcm %lld %lld %lld %lld) %lld)\n", 
		  int_args[i], int_args[j], s1 * int_args[a1], s2 * int_args[a2],
		  c_lcm(s2 * int_args[a2], c_lcm(s1 * int_args[a1], c_lcm(int_args[i], int_args[j]))));
      }

  fprintf(fp, "\n\n\
(test (gcd 323 28747 27113) 19)\n\
(test (lcm 323 28747 27113) 41021969)\n\n");

  fprintf(fp, "\n\n\
(test (real-part 1) 1)\n\
(test (imag-part 1) 0)\n\
(test (real-part 2.0) 2.0)\n\
(test (imag-part -2.0) 0.0)\n\
(test (real-part 2/3) 2/3)\n\
(test (imag-part 2/3) 0)\n");

  k = 0;
  for (i = 0; i < DOUBLE_ARGS; i++)
    for (j = 0; j < DOUBLE_ARGS; j++)
      {
	int s1 = 1, s2 = 1;
	char *t1;
	switch (k)
	  {
	  case 0: s1 = 1; s2 = 1; break;
	  case 1: s1 = -1; s2 = 1; break;
	  case 2: s1 = 1; s2 = -1; break;
	  case 3: s1 = -1; s2 = -1; break;
	  }
	k++;
	if (k == 4) k = 0;
	fprintf(fp, "(test (real-part %s) %s)\n", complex_arg(i, j, s1, s2), t1 = gstr(s1 * double_args[i]));
	free(t1);
	fprintf(fp, "(test (imag-part %s) %s)\n", complex_arg(i, j, s1, s2), t1 = gstr(s2 * double_args[j]));
      }

  fprintf(fp, "\n\
(test (numerator 1) 1)\n\
(test (numerator 2/3) 2)\n\
(test (numerator -2/3) -2)\n\
(test (denominator -2/3) 3)\n\
(test (denominator 1) 1)\n\
(test (denominator 2/3) 3)\n\
(test (numerator 2.3+0.5i) 'error)\n\
(test (denominator 2.3+0.5i) 'error)\n");

  k = 0;
  for (i = 0; i < INT_ARGS; i++)
    for (j = 0; j < INT_ARGS; j++)
      {
	int a1, a2, s1 = 1, s2 = 1;
	a1 = cint_mod(i + j, INT_ARGS);
	a2 = cint_mod(i - j, INT_ARGS);
	if (a2 == 0) a2 = 4;
	switch (k)
	  {
	  case 0: s1 = 1; s2 = 1; break;
	  case 1: s1 = -1; s2 = 1; break;
	  case 2: s1 = 1; s2 = -1; break;
	  case 3: s1 = -1; s2 = -1; break;
	  }
	k++;
	if (k == 4) k = 0;
	if (int_args[j] != 0)
	  {
	    off_t divisor;
	    divisor = c_gcd(int_args[i], int_args[j]);
	    fprintf(fp, "(test (numerator %lld/%lld) %lld)\n", s1 * int_args[i], int_args[j], s1 * int_args[i] / divisor);
	    fprintf(fp, "(test (denominator %lld/%lld) %lld)\n", s1 * int_args[i], int_args[j], int_args[j] / divisor);
	  }
      }

  for (i = 0; i < INT_ARGS; i++)
    for (j = 0; j < INT_ARGS; j++)
      {
	if (int_args[j] != 0)
	  {
	    fprintf(fp, "(test (modulo %lld %lld) %lld)\n", int_args[i], int_args[j], c_mod(int_args[i], int_args[j]));
	    fprintf(fp, "(test (remainder %lld %lld) %lld)\n", int_args[i], int_args[j], (int_args[i] % int_args[j]));

	    fprintf(fp, "(test (modulo %lld %lld) %lld)\n", -int_args[i], int_args[j], c_mod(-int_args[i], int_args[j]));
	    fprintf(fp, "(test (remainder %lld %lld) %lld)\n", -int_args[i], int_args[j], (-int_args[i] % int_args[j]));

	    fprintf(fp, "(test (modulo %lld %lld) %lld)\n", int_args[i], -int_args[j], c_mod(int_args[i], -int_args[j]));
	    fprintf(fp, "(test (remainder %lld %lld) %lld)\n", int_args[i], -int_args[j], (int_args[i] % -int_args[j]));

	    fprintf(fp, "(test (modulo %lld %lld) %lld)\n", -int_args[i], -int_args[j], c_mod(-int_args[i], -int_args[j]));
	    fprintf(fp, "(test (remainder %lld %lld) %lld)\n", -int_args[i], -int_args[j], (-int_args[i] % -int_args[j]));
	  }
      }

  fprintf(fp, "\n\
(test (modulo 3 2.3) 'error)\n\
(test (modulo 2.3 3) 'error)\n\
(test (modulo 1/3 2.3) 'error)\n\
(test (modulo 2.3 1.0+0.1i) 'error)\n\
(test (modulo 3.0+2.3i 3) 'error)\n\
(test (remainder 3 2.3) 'error)\n\
(test (remainder 2.3 3) 'error)\n\
(test (remainder 1/3 2.3) 'error)\n\
(test (remainder 2.3 1.0+0.1i) 'error)\n\
(test (remainder 3.0+2.3i 3) 'error)\n");

  for (i = 0; i < INT_ARGS; i++)
    {
      fprintf(fp, "(test (abs %lld) %lld)\n", int_args[i], int_args[i]);
      fprintf(fp, "(test (abs -%lld) %lld)\n", int_args[i], int_args[i]);
      fprintf(fp, "(test (magnitude %lld) %lld)\n", int_args[i], int_args[i]);
      fprintf(fp, "(test (magnitude -%lld) %lld)\n", int_args[i], int_args[i]);
      fprintf(fp, "(test (angle %lld) 0)\n", int_args[i]);
      fprintf(fp, "(test (angle -%lld) %s)\n", int_args[i], (int_args[i] == 0) ? "0" : "3.14159265");
      
      for (j = 1; j < INT_ARGS; j++)
	{
	  char *t1;
	  double complex z;
	  fprintf(fp, "(test (abs %lld/%lld) %lld/%lld)\n", int_args[i], int_args[j], int_args[i], int_args[j]);
	  fprintf(fp, "(test (abs -%lld/%lld) %lld/%lld)\n", int_args[i], int_args[j], int_args[i], int_args[j]);
	  fprintf(fp, "(test (magnitude %lld/%lld) %lld/%lld)\n", int_args[i], int_args[j], int_args[i], int_args[j]);
	  fprintf(fp, "(test (magnitude -%lld/%lld) %lld/%lld)\n", int_args[i], int_args[j], int_args[i], int_args[j]);
	  fprintf(fp, "(test (angle %lld/%lld) 0)\n", int_args[i], int_args[j]);
	  fprintf(fp, "(test (angle -%lld/%lld) %s)\n", int_args[i], int_args[j], (int_args[i] == 0) ? "0" : "3.14159265");

	  t1 = split_complex_to_string((double)int_args[i], (double)int_args[j]);
	  fprintf(fp, "(test (make-rectangular %lld %lld) %s)\n", int_args[i], int_args[j], t1);
	  free(t1);	

	  t1 = split_complex_to_string(-(double)int_args[i], (double)int_args[j]);
	  fprintf(fp, "(test (make-rectangular -%lld %lld) %s)\n", int_args[i], int_args[j], t1);
	  free(t1);	

	  t1 = split_complex_to_string((double)int_args[i], -(double)int_args[j]);
	  fprintf(fp, "(test (make-rectangular %lld -%lld) %s)\n", int_args[i], int_args[j], t1);
	  free(t1);	

	  t1 = split_complex_to_string(-(double)int_args[i], -(double)int_args[j]);
	  fprintf(fp, "(test (make-rectangular -%lld -%lld) %s)\n", int_args[i], int_args[j], t1);
	  free(t1);	

	  
	  z = (double)int_args[i] * (cos((double)int_args[j]) + sin((double)int_args[j]) * _Complex_I);
	  t1 = split_complex_to_string(creal(z), cimag(z));
	  fprintf(fp, "(test (make-polar %lld %lld) %s)\n", int_args[i], int_args[j], t1);
	  free(t1);	

	  z = -(double)int_args[i] * (cos((double)int_args[j]) + sin((double)int_args[j]) * _Complex_I);
	  t1 = split_complex_to_string(creal(z), cimag(z));
	  fprintf(fp, "(test (make-polar -%lld %lld) %s)\n", int_args[i], int_args[j], t1);
	  free(t1);	

	  z = (double)int_args[i] * (cos(-(double)int_args[j]) + sin(-(double)int_args[j]) * _Complex_I);
	  t1 = split_complex_to_string(creal(z), cimag(z));
	  fprintf(fp, "(test (make-polar %lld -%lld) %s)\n", int_args[i], int_args[j], t1);
	  free(t1);	

	  z = -(double)int_args[i] * (cos(-(double)int_args[j]) + sin(-(double)int_args[j]) * _Complex_I);
	  t1 = split_complex_to_string(creal(z), cimag(z));
	  fprintf(fp, "(test (make-polar -%lld -%lld) %s)\n", int_args[i], int_args[j], t1);
	  free(t1);	
	}
    }
  for (i = 0; i < DOUBLE_ARGS; i++)
    {
      char *t1;
      t1 = gstr(double_args[i]);
      fprintf(fp, "(test (abs %s) %s)\n", t1, t1);
      fprintf(fp, "(test (abs -%s) %s)\n", t1, t1);
      free(t1);
      t1 = gstr(double_args[i]);
      fprintf(fp, "(test (magnitude %s) %s)\n", t1, t1);
      fprintf(fp, "(test (magnitude -%s) %s)\n", t1, t1);
      fprintf(fp, "(test (angle %s) 0.0)\n", t1);
      fprintf(fp, "(test (angle -%s) %s)\n", t1, (double_args[i] == 0.0) ? "0.0" : "3.14159265");

      for (j = 1; j < INT_ARGS; j++)
	{
	  double mag, ang;
	  double complex z;
	  char *t2, *t3;
	  mag = cabs(double_args[i] + double_args[j] * _Complex_I);
	  t2 = gstr(mag);
	  t1 = split_complex_to_string(double_args[i], double_args[j]);
	  ang = carg(double_args[i] + double_args[j] * _Complex_I);
	  t3 = split_complex_to_string(creal(ang), cimag(ang));
	  fprintf(fp, "(test (magnitude %s) %s)\n", t1, t2);
	  fprintf(fp, "(test (angle %s) %s)\n", t1, t3);
	  free(t1); free(t3);

	  t1 = split_complex_to_string(-double_args[i], double_args[j]);
	  ang = carg(-double_args[i] + double_args[j] * _Complex_I);
	  t3 = split_complex_to_string(creal(ang), cimag(ang));
	  fprintf(fp, "(test (magnitude %s) %s)\n", t1, t2);
	  fprintf(fp, "(test (angle %s) %s)\n", t1, t3);
	  free(t1); free(t3);

	  t1 = split_complex_to_string(double_args[i], -double_args[j]);
	  ang = carg(double_args[i] - double_args[j] * _Complex_I);
	  t3 = split_complex_to_string(creal(ang), cimag(ang));
	  fprintf(fp, "(test (magnitude %s) %s)\n", t1, t2);
	  fprintf(fp, "(test (angle %s) %s)\n", t1, t3);
	  free(t1); free(t3);

	  t1 = split_complex_to_string(-double_args[i], -double_args[j]);
	  ang = carg(-double_args[i] - double_args[j] * _Complex_I);
	  t3 = split_complex_to_string(creal(ang), cimag(ang));
	  fprintf(fp, "(test (magnitude %s) %s)\n", t1, t2);
	  fprintf(fp, "(test (angle %s) %s)\n", t1, t3);
	  free(t1); free(t2); free(t3);

	  t1 = split_complex_to_string(double_args[i], double_args[j]);
	  fprintf(fp, "(test (make-rectangular %s %s) %s)\n", t2 = gstr(double_args[i]), t3 = gstr(double_args[j]), t1);
	  free(t1);	

	  t1 = split_complex_to_string(-double_args[i], double_args[j]);
	  fprintf(fp, "(test (make-rectangular -%s %s) %s)\n", t2 = gstr(double_args[i]), t3 = gstr(double_args[j]), t1);
	  free(t1);	

	  t1 = split_complex_to_string(double_args[i], -double_args[j]);
	  fprintf(fp, "(test (make-rectangular %s -%s) %s)\n", t2 = gstr(double_args[i]), t3 = gstr(double_args[j]), t1);
	  free(t1);	

	  t1 = split_complex_to_string(-double_args[i], -double_args[j]);
	  fprintf(fp, "(test (make-rectangular -%s -%s) %s)\n", t2 = gstr(double_args[i]), t3 = gstr(double_args[j]), t1);
	  free(t1);	

	  
	  z = double_args[i] * (cos(double_args[j]) + sin(double_args[j]) * _Complex_I);
	  t1 = split_complex_to_string(creal(z), cimag(z));
	  fprintf(fp, "(test (make-polar %s %s) %s)\n", t2 = gstr(double_args[i]), t3 = gstr(double_args[j]), t1);
	  free(t1);	

	  z = -double_args[i] * (cos(double_args[j]) + sin(double_args[j]) * _Complex_I);
	  t1 = split_complex_to_string(creal(z), cimag(z));
	  fprintf(fp, "(test (make-polar -%s %s) %s)\n", t2 = gstr(double_args[i]), t3 = gstr(double_args[j]), t1);
	  free(t1);	

	  z = double_args[i] * (cos(-double_args[j]) + sin(-double_args[j]) * _Complex_I);
	  t1 = split_complex_to_string(creal(z), cimag(z));
	  fprintf(fp, "(test (make-polar %s -%s) %s)\n", t2 = gstr(double_args[i]), t3 = gstr(double_args[j]), t1);
	  free(t1);	

	  z = -double_args[i] * (cos(-double_args[j]) + sin(-double_args[j]) * _Complex_I);
	  t1 = split_complex_to_string(creal(z), cimag(z));
	  fprintf(fp, "(test (make-polar -%s -%s) %s)\n", t2 = gstr(double_args[i]), t3 = gstr(double_args[j]), t1);
	  free(t1);	

	}
    }
  
  fprintf(fp, "\n\
(test (abs 1.0+0.1i) 'error)\n\
(test (make-polar 1.0 1.0+0.1i) 'error)\n\
(test (make-polar 1.0+0.1i 0.0) 'error)\n\
(test (make-rectangular 1.0 1.0+0.1i) 'error)\n\
(test (make-rectangular 1.0+0.1i 1.0) 'error)\n");


  fprintf(fp, ";;; --------------------------------------------------------------------------------\n\n");

  for (i = 0; i < INT_ARGS; i += 2)
    {
      fprintf(fp, "(test (min %lld) %lld)\n", int_args[i], int_args[i]);
      fprintf(fp, "(test (max %lld) %lld)\n", int_args[i], int_args[i]);
      fprintf(fp, "(test (+ %lld) %lld)\n", int_args[i], int_args[i]);
      fprintf(fp, "(test (- %lld) %lld)\n", int_args[i], -int_args[i]);
      fprintf(fp, "(test (* %lld) %lld)\n", int_args[i], int_args[i]);
      if (int_args[i] != 0)
	fprintf(fp, "(test (/ %lld) 1/%lld)\n", int_args[i], int_args[i]);

      fprintf(fp, "(test (< %lld) #t)\n", int_args[i]);
      fprintf(fp, "(test (<= %lld) #t)\n", int_args[i]);
      fprintf(fp, "(test (= %lld) #t)\n", int_args[i]);
      fprintf(fp, "(test (> %lld) #t)\n", int_args[i]);
      fprintf(fp, "(test (>= %lld) #t)\n", int_args[i]);

      fprintf(fp, "(test (abs %lld) %lld)\n", int_args[i], int_args[i]);

      fprintf(fp, "(test (min %lld) %lld)\n", -int_args[i], -int_args[i]);
      fprintf(fp, "(test (max %lld) %lld)\n", -int_args[i], -int_args[i]);
      fprintf(fp, "(test (+ %lld) %lld)\n", -int_args[i], -int_args[i]);
      fprintf(fp, "(test (- %lld) %lld)\n", -int_args[i], int_args[i]);
      fprintf(fp, "(test (* %lld) %lld)\n", -int_args[i], -int_args[i]);
      if (int_args[i] != 0)
	fprintf(fp, "(test (/ %lld) -1/%lld)\n", -int_args[i], int_args[i]);

      fprintf(fp, "(test (abs %lld) %lld)\n", -int_args[i], int_args[i]);
    }

  j = 1;
  for (i = 0; i < INT_ARGS; i += 2, j++)
    {
      if (j == INT_ARGS) j = 0;
      if (int_args[j] == 0) j++;

      fprintf(fp, "(test (min %lld/%lld) %lld/%lld)\n", int_args[i], int_args[j], int_args[i], int_args[j]);
      fprintf(fp, "(test (max %lld/%lld) %lld/%lld)\n", int_args[i], int_args[j], int_args[i], int_args[j]);
      fprintf(fp, "(test (+ %lld/%lld) %lld/%lld)\n", int_args[i], int_args[j], int_args[i], int_args[j]);
      fprintf(fp, "(test (- %lld/%lld) %lld/%lld)\n", int_args[i], int_args[j], -int_args[i], int_args[j]);
      fprintf(fp, "(test (* %lld/%lld) %lld/%lld)\n", int_args[i], int_args[j], int_args[i], int_args[j]);
      if (int_args[i] != 0)
	fprintf(fp, "(test (/ %lld/%lld) %lld/%lld)\n", int_args[i], int_args[j], int_args[j], int_args[i]);
      
      fprintf(fp, "(test (< %lld/%lld) #t)\n", int_args[i], int_args[j]);
      fprintf(fp, "(test (<= %lld/%lld) #t)\n", int_args[i], int_args[j]);
      fprintf(fp, "(test (= %lld/%lld) #t)\n", int_args[i], int_args[j]);
      fprintf(fp, "(test (> %lld/%lld) #t)\n", int_args[i], int_args[j]);
      fprintf(fp, "(test (>= %lld/%lld) #t)\n", int_args[i], int_args[j]);
      
      fprintf(fp, "(test (abs %lld/%lld) %lld/%lld)\n", int_args[i], int_args[j], int_args[i], int_args[j]);
      
      fprintf(fp, "(test (min %lld/%lld) %lld/%lld)\n", -int_args[i], int_args[j], -int_args[i], int_args[j]);
      fprintf(fp, "(test (max %lld/%lld) %lld/%lld)\n", -int_args[i], int_args[j], -int_args[i], int_args[j]);
      fprintf(fp, "(test (+ %lld/%lld) %lld/%lld)\n", -int_args[i], int_args[j], -int_args[i], int_args[j]);
      fprintf(fp, "(test (- %lld/%lld) %lld/%lld)\n", -int_args[i], int_args[j], int_args[i], int_args[j]);
      fprintf(fp, "(test (* %lld/%lld) %lld/%lld)\n", -int_args[i], int_args[j], -int_args[i], int_args[j]);
      if (int_args[i] != 0)
	fprintf(fp, "(test (/ %lld/%lld) %lld/%lld)\n", -int_args[i], int_args[j], -int_args[j], int_args[i]);
      
      fprintf(fp, "(test (abs %lld/%lld) %lld/%lld)\n", -int_args[i], int_args[j], int_args[i], int_args[j]);
    }

  for (i = 0; i < DOUBLE_ARGS; i += 2)
    {
      char *t1, *t2;
      fprintf(fp, "(test (min %s) %s)\n", t1 = gstr(double_args[i]), t2 = gstr(double_args[i])); free(t1); free(t2);
      fprintf(fp, "(test (max %s) %s)\n", t1 = gstr(double_args[i]), t2 = gstr(double_args[i])); free(t1); free(t2);
      fprintf(fp, "(test (+ %s) %s)\n", t1 = gstr(double_args[i]), t2 = gstr(double_args[i])); free(t1); free(t2);
      fprintf(fp, "(test (- %s) %s)\n", t1 = gstr(double_args[i]), t2 = gstr(-double_args[i])); free(t1); free(t2);
      fprintf(fp, "(test (* %s) %s)\n", t1 = gstr(double_args[i]), t2 = gstr(double_args[i])); free(t1); free(t2);
      if (double_args[i] != 0)
	{
	  fprintf(fp, "(test (/ %s) %s)\n", t1 = gstr(double_args[i]), t2 = gstr(1.0 / double_args[i])); 
	  free(t1); free(t2);
	}

      fprintf(fp, "(test (< %s) #t)\n", t1 = gstr(double_args[i])); free(t1);
      fprintf(fp, "(test (<= %s) #t)\n", t1 = gstr(double_args[i])); free(t1);
      fprintf(fp, "(test (= %s) #t)\n", t1 = gstr(double_args[i])); free(t1);
      fprintf(fp, "(test (> %s) #t)\n", t1 = gstr(double_args[i])); free(t1);
      fprintf(fp, "(test (>= %s) #t)\n", t1 = gstr(double_args[i])); free(t1);

      fprintf(fp, "(test (abs %s) %s)\n", t1 = gstr(double_args[i]), t2 = gstr(double_args[i])); free(t1); free(t2);

      fprintf(fp, "(test (min %s) %s)\n", t1 = gstr(-double_args[i]), t2 = gstr(-double_args[i])); free(t1); free(t2);
      fprintf(fp, "(test (max %s) %s)\n", t1 = gstr(-double_args[i]), t2 = gstr(-double_args[i])); free(t1); free(t2);
      fprintf(fp, "(test (+ %s) %s)\n", t1 = gstr(-double_args[i]), t2 = gstr(-double_args[i])); free(t1); free(t2);
      fprintf(fp, "(test (- %s) %s)\n", t1 = gstr(-double_args[i]), t2 = gstr(double_args[i])); free(t1); free(t2);
      fprintf(fp, "(test (* %s) %s)\n", t1 = gstr(-double_args[i]), t2 = gstr(-double_args[i])); free(t1); free(t2);
      if (double_args[i] != 0.0)
	{
	  fprintf(fp, "(test (/ %s) %s)\n", t1 = gstr(-double_args[i]), t2 = gstr(-1.0 / double_args[i])); 
	  free(t1); free(t2);
	}

      fprintf(fp, "(test (abs %s) %s)\n", t1 = gstr(-double_args[i]), t2 = gstr(double_args[i])); free(t1); free(t2);
    }

  j = 1;
  for (i = 0; i < DOUBLE_ARGS; i += 2, j++)
    {
      char *t1, *t2;
      if (j == DOUBLE_ARGS) j = 0;
      if (double_args[j] == 0.0) j++;

      fprintf(fp, "(test (min %s) 'error)\n",
	      t1 = split_complex_to_string(double_args[i], double_args[j]));
      free(t1); 
      fprintf(fp, "(test (max %s) 'error)\n", 
	      t1 = split_complex_to_string(double_args[i], double_args[j]));
      free(t1); 
      fprintf(fp, "(test (+ %s) %s)\n", 
	      t1 = split_complex_to_string(double_args[i], double_args[j]), 
	      t2 = split_complex_to_string(double_args[i], double_args[j]));
      free(t1); free(t2);
      fprintf(fp, "(test (- %s) %s)\n", 
	      t1 = split_complex_to_string(double_args[i], double_args[j]), 
	      t2 = split_complex_to_string(-double_args[i],double_args[j]));
      free(t1); free(t2); 
      fprintf(fp, "(test (* %s) %s)\n", 
	      t1 = split_complex_to_string(double_args[i], double_args[j]), 
	      t2 = split_complex_to_string(double_args[i], double_args[j]));
      free(t1); free(t2); 

      if ((double_args[i] != 0.0) || (double_args[j] != 0.0))
	{
	  double complex z;
	  z = 1.0 / (double_args[i] + double_args[j] * _Complex_I);
	  fprintf(fp, "(test (/ %s) %s)\n", 
		  t1 = split_complex_to_string(double_args[i], double_args[j]),
		  t2 = split_complex_to_string(creal(z), cimag(z)));
	  free(t1); free(t2);
	}
      
      fprintf(fp, "(test (< %s) 'error)\n", t1 = split_complex_to_string(double_args[i], double_args[j]));
      free(t1);
	      fprintf(fp, "(test (<= %s) 'error)\n", t1 = split_complex_to_string(double_args[i], double_args[j]));
      free(t1); 
      fprintf(fp, "(test (= %s) #t)\n", t1 = split_complex_to_string(double_args[i], double_args[j]));
      free(t1); 
      fprintf(fp, "(test (> %s) 'error)\n", t1 = split_complex_to_string(double_args[i], double_args[j]));
      free(t1); 
      fprintf(fp, "(test (>= %s) 'error)\n", t1 = split_complex_to_string(double_args[i], double_args[j]));
      free(t1); 
      
      {
	double mag;
	mag = cabs(double_args[i] + double_args[j] * _Complex_I);
	fprintf(fp, "(test (magnitude %s) %s)\n", 
		t1 = split_complex_to_string(double_args[i], double_args[j]), 
		t2 = split_complex_to_string(creal(mag), cimag(mag)));
	free(t1); free(t2);
      }
      
      fprintf(fp, "(test (min %s) 'error)\n", 
	      t1 = split_complex_to_string(-double_args[i], double_args[j]));
      free(t1); 
      fprintf(fp, "(test (max %s) 'error)\n", 
	      t1 = split_complex_to_string(-double_args[i], double_args[j]));
      free(t1); 
      fprintf(fp, "(test (+ %s) %s)\n", 
	      t1 = split_complex_to_string(-double_args[i], double_args[j]), 
	      t2 = split_complex_to_string(-double_args[i], double_args[j]));
      free(t1); free(t2);
      fprintf(fp, "(test (- %s) %s)\n", 
	      t1 = split_complex_to_string(-double_args[i], double_args[j]), 
	      t2 = split_complex_to_string(double_args[i], double_args[j]));
      free(t1); free(t2);
      fprintf(fp, "(test (* %s) %s)\n", 
	      t1 = split_complex_to_string(-double_args[i], double_args[j]), 
	      t2 = split_complex_to_string(-double_args[i], double_args[j]));
      free(t1); free(t2);
      if ((double_args[i] != 0.0) || (double_args[j] != 0.0))
	{
	  double complex z;
	  z = 1.0 / (-double_args[i] + double_args[j] * _Complex_I);
	  fprintf(fp, "(test (/ %s) %s)\n", 
		  t1 = split_complex_to_string(-double_args[i], double_args[j]),
		  t2 = split_complex_to_string(creal(z), cimag(z)));
	  free(t1); free(t2);
	}

      {
	double mag;
	mag = cabs(-double_args[i] + double_args[j] * _Complex_I);
	fprintf(fp, "(test (magnitude %s) %s)\n", 
		t1 = split_complex_to_string(-double_args[i], double_args[j]), 
		t2 = split_complex_to_string(creal(mag), cimag(mag)));
	free(t1); free(t2);
      }
    }

  for (i = 0; i < NUMERIC_ARGS; i++)
    for (j = 0; j < NUMERIC_ARGS; j++)
      {
	char *t1;
	fprintf(fp, "(test (+ %s %s) %s)\n", arg_data[i].name, arg_data[j].name, t1 = add(2, i, j));
	free(t1);
	fprintf(fp, "(test (- %s %s) %s)\n", arg_data[i].name, arg_data[j].name, t1 = subtract(2, i, j));
	free(t1);
	fprintf(fp, "(test (* %s %s) %s)\n", arg_data[i].name, arg_data[j].name, t1 = multiply(2, i, j));
	free(t1);
	/* skip divide by 0 cases -- these can generate many different things */
	switch (arg_data[j].type)
	  {
	  case ARG_INT: if (arg_data[j].n == 0) continue; break;
	  case ARG_RATIO: if (arg_data[j].n == 0) continue; break;
	  case ARG_REAL: if (arg_data[j].x == 0.0) continue; break;
	  case ARG_COMPLEX: if (arg_data[j].z == 0.0 + 0.0 * _Complex_I) continue; break;
	}
	fprintf(fp, "(test (/ %s %s) %s)\n", arg_data[i].name, arg_data[j].name, t1 = divide(2, i, j));
	free(t1);


      }

  for (i = 0; i < NUMERIC_ARGS; i++)
    for (j = 0; j < NUMERIC_ARGS; j++)
      {
	int k;
	for (k = 0; k < NUMERIC_ARGS; k++)
	  {
	    char *t1;
	    fprintf(fp, "(test (+ %s %s %s) %s)\n", arg_data[i].name, arg_data[j].name, arg_data[k].name, t1 = add(3, i, j, k));
	    free(t1);
	    fprintf(fp, "(test (- %s %s %s) %s)\n", arg_data[i].name, arg_data[j].name, arg_data[k].name, t1 = subtract(3, i, j, k));
	    free(t1);
	    fprintf(fp, "(test (* %s %s %s) %s)\n", arg_data[i].name, arg_data[j].name, arg_data[k].name, t1 = multiply(3, i, j, k));
	    free(t1);
	    /* skip divide by 0 cases -- these can generate many different things */
	    switch (arg_data[j].type)
	      {
	      case ARG_INT: if (arg_data[j].n == 0) continue; break;
	      case ARG_RATIO: if (arg_data[j].n == 0) continue; break;
	      case ARG_REAL: if (arg_data[j].x == 0.0) continue; break;
	      case ARG_COMPLEX: if (arg_data[j].z == 0.0 + 0.0 * _Complex_I) continue; break;
	      }
	    switch (arg_data[k].type)
	      {
	      case ARG_INT: if (arg_data[k].n == 0) continue; break;
	      case ARG_RATIO: if (arg_data[k].n == 0) continue; break;
	      case ARG_REAL: if (arg_data[k].x == 0.0) continue; break;
	      case ARG_COMPLEX: if (arg_data[k].z == 0.0 + 0.0 * _Complex_I) continue; break;
	      }
	    fprintf(fp, "(test (/ %s %s %s) %s)\n", arg_data[i].name, arg_data[j].name, arg_data[k].name, t1 = divide(3, i, j, k));
	    free(t1);
	  }

      }

    fprintf(fp, "(display \";all done!\") (newline)\n");

/* remaining expt cases
   quotient
   + - * / < <= > >= = max min
   2 arg atan
   string->n going 1/2
   
*/

fclose(fp);

return(0);
}
