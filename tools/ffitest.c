/* s7 ffi tester
 *
 * gcc -o ffitest ffitest.c -g3 -Wall s7.o -lm -I. -ldl
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#define HAVE_STDBOOL_H 1
#include "s7.h"

#define TO_STR(x) s7_object_to_c_string(sc, x)
#define TO_S7_INT(x) s7_make_integer(sc, x)

static s7_pointer a_function(s7_scheme *sc, s7_pointer args)
{
  return(s7_car(args));
}

typedef struct {
  s7_Double x;
  s7_pointer data;
} dax;

static char *print_dax(s7_scheme *sc, void *val)
{
  char *data_str, *str;
  int data_str_len;
  dax *o = (dax *)val;
  data_str = s7_object_to_c_string(sc, o->data);
  data_str_len = strlen(data_str);
  str = (char *)calloc(data_str_len + 32, sizeof(char));
  snprintf(str, data_str_len + 32, "#<dax %.3f %s>", o->x, data_str);
  free(data_str);
  return(str);
}

static void free_dax(void *val)
{
  if (val) free(val);
}

static bool equal_dax(void *val1, void *val2)
{
  return(val1 == val2);
}

static void mark_dax(void *val)
{
  dax *o = (dax *)val;
  if (o) s7_mark_object(o->data);
}

static int dax_type_tag = 0;

static s7_pointer make_dax(s7_scheme *sc, s7_pointer args)
{
  dax *o;
  o = (dax *)malloc(sizeof(dax));
  o->x = s7_real(s7_car(args));
  if (s7_cdr(args) != s7_nil(sc))
    o->data = s7_car(s7_cdr(args));
  else o->data = s7_nil(sc);
  return(s7_make_object(sc, dax_type_tag, (void *)o));
}

static s7_pointer is_dax(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_boolean(sc, 
			 s7_is_object(s7_car(args)) &&
			 s7_object_type(s7_car(args)) == dax_type_tag));
}

static s7_pointer dax_x(s7_scheme *sc, s7_pointer args)
{
  dax *o;
  o = (dax *)s7_object_value(s7_car(args));
  return(s7_make_real(sc, o->x));
}

static s7_pointer set_dax_x(s7_scheme *sc, s7_pointer args)
{
  dax *o;
  o = (dax *)s7_object_value(s7_car(args));
  o->x = s7_real(s7_car(s7_cdr(args)));
  return(s7_car(s7_cdr(args)));
}

static s7_pointer dax_data(s7_scheme *sc, s7_pointer args)
{
  dax *o;
  o = (dax *)s7_object_value(s7_car(args));
  return(o->data);
}

static s7_pointer set_dax_data(s7_scheme *sc, s7_pointer args)
{
  dax *o;
  o = (dax *)s7_object_value(s7_car(args));
  o->data = s7_car(s7_cdr(args));
  return(o->data);
}

static s7_pointer plus(s7_scheme *sc, s7_pointer args)
{
  /* (define* (plus (red 32) blue) (+ (* 2 red) blue)) */
  return(TO_S7_INT(2 * s7_integer(s7_car(args)) + s7_integer(s7_car(s7_cdr(args)))));
}


int main(int argc, char **argv)
{
  s7_scheme *sc;
  s7_pointer p, p1;
  int i, gc_loc;
  char *s1, *s2;

  sc = s7_init(); 
  
  /* try each straight (no errors) case */

  if (!s7_is_null(sc, s7_nil(sc))) 
    {fprintf(stderr, "%d: %s is not null?\n", __LINE__, s1 = TO_STR(s7_nil(sc))); free(s1);}

  if (s7_is_pair(s7_nil(sc))) 
    {fprintf(stderr, "%d: %s is a pair?\n", __LINE__, s1 = TO_STR(s7_nil(sc))); free(s1);}

  if (!s7_is_boolean(s7_t(sc))) 
    {fprintf(stderr, "%d: %s is not boolean?\n", __LINE__, s1 = TO_STR(s7_t(sc))); free(s1);}

  if (!s7_is_boolean(s7_f(sc))) 
    {fprintf(stderr, "%d: %s is not boolean?\n", __LINE__, s1 = TO_STR(s7_f(sc))); free(s1);}

  if (s7_boolean(sc, s7_f(sc)))
    {fprintf(stderr, "%d: %s is #t?\n", __LINE__, s1 = TO_STR(s7_f(sc))); free(s1);}

  if (!s7_boolean(sc, s7_t(sc)))
    {fprintf(stderr, "%d: %s is #f?\n", __LINE__, s1 = TO_STR(s7_t(sc))); free(s1);}

  p = s7_make_boolean(sc, true);
  if (p != s7_t(sc))
    {fprintf(stderr, "%d: %s is not #t?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  p = s7_make_boolean(sc, false);
  if (p != s7_f(sc))
    {fprintf(stderr, "%d: %s is not #f?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  if (!s7_is_eq(s7_f(sc), s7_f(sc))) 
    {fprintf(stderr, "%d: (eq? %s %s) -> #f?\n", __LINE__, s1 = TO_STR(s7_f(sc)), s2 = TO_STR(s7_f(sc))); free(s1); free(s2);}

  if (!s7_is_eqv(s7_f(sc), s7_f(sc))) 
    {fprintf(stderr, "%d: (eqv? %s %s) -> #f?\n", __LINE__, s1 = TO_STR(s7_f(sc)), s2 = TO_STR(s7_f(sc))); free(s1); free(s2);}

  if (!s7_is_equal(sc, s7_f(sc), s7_f(sc))) 
    {fprintf(stderr, "%d: (equal? %s %s) -> #f?\n", __LINE__, s1 = TO_STR(s7_f(sc)), s2 = TO_STR(s7_f(sc))); free(s1); free(s2);}

  if (!s7_is_unspecified(sc, s7_unspecified(sc))) 
    {fprintf(stderr, "%d: %s is not #<unspecified>?\n", __LINE__, s1 = TO_STR(s7_unspecified(sc))); free(s1);}

  if (s7_is_eq(s7_eof_object(sc), s7_undefined(sc)))
    {fprintf(stderr, "%d: (eq? %s %s) -> #t?\n", __LINE__, s1 = TO_STR(s7_eof_object(sc)), s2 = TO_STR(s7_undefined(sc))); free(s1); free(s2);}

  if (s7_is_eqv(s7_eof_object(sc), s7_undefined(sc)))
    {fprintf(stderr, "%d: (eqv? %s %s) -> #t?\n", __LINE__, s1 = TO_STR(s7_eof_object(sc)), s2 = TO_STR(s7_undefined(sc))); free(s1); free(s2);}

  if (s7_is_equal(sc, s7_eof_object(sc), s7_undefined(sc)))
    {fprintf(stderr, "%d: (equal? %s %s) -> #t?\n", __LINE__, s1 = TO_STR(s7_eof_object(sc)), s2 = TO_STR(s7_undefined(sc))); free(s1); free(s2);}

  if (!s7_is_valid_pointer(s7_t(sc)))
    {fprintf(stderr, "%d: %s is not a valid pointer?\n", __LINE__, s1 = TO_STR(s7_t(sc))); free(s1);}

  if (s7_is_c_pointer(s7_t(sc)))
    {fprintf(stderr, "%d: %s is a raw c pointer?\n", __LINE__, s1 = TO_STR(s7_t(sc))); free(s1);}

  i = 32;
  p = s7_make_c_pointer(sc, (void *)(&i));
  if (!s7_is_c_pointer(p))
    {fprintf(stderr, "%d: %s is not a raw c pointer?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  i = (*((int *)s7_c_pointer(p)));
  if (i != 32)
    fprintf(stderr, "%d: 32 -> %d via raw c pointer?\n", __LINE__, i);

  s7_provide(sc, "ffitest");
  if (!s7_is_provided(sc, "ffitest"))
    {fprintf(stderr, "%d: *features* %s doesn't provide 'ffitest?\n", __LINE__, s1 = TO_STR(s7_name_to_value(sc, "*features*"))); free(s1);}

  p = s7_cons(sc, s7_f(sc), s7_t(sc));
  gc_loc = s7_gc_protect(sc, p);
  if (p != s7_gc_protected_at(sc, gc_loc))
    {fprintf(stderr, "%d: %s is not gc protected at %d: %s?\n", __LINE__, s1 = TO_STR(p), gc_loc, s2 = TO_STR(s7_gc_protected_at(sc, gc_loc))); free(s1); free(s2);}
  
  if (s7_car(p) != s7_f(sc))
    {fprintf(stderr, "%d: (car %s) is not #f?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  if (s7_cdr(p) != s7_t(sc))
    {fprintf(stderr, "%d: (cdr %s) is not #t?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
  
  if (!s7_is_pair(p))
    {fprintf(stderr, "%d: %s is not a pair?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  s7_set_car(p, s7_eof_object(sc));
  if (s7_car(p) != s7_eof_object(sc))
    {fprintf(stderr, "%d: (car %s) is not #<eof>?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  s7_set_cdr(p, s7_unspecified(sc));
  if (s7_cdr(p) != s7_unspecified(sc))
    {fprintf(stderr, "%d: (cdr %s) is not #<unspecified>?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  s7_gc_unprotect_at(sc, gc_loc);


  
  p = TO_S7_INT(123);
  gc_loc = s7_gc_protect(sc, p);

  if (!s7_is_integer(p))
    {fprintf(stderr, "%d: %s is not integral?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  if (!s7_is_rational(p))
    {fprintf(stderr, "%d: %s is not rational?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  if (s7_is_ratio(p))
    {fprintf(stderr, "%d: %s is a ratio?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  if (!s7_is_real(p))
    {fprintf(stderr, "%d: %s is not real?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  if (!s7_is_complex(p))
    {fprintf(stderr, "%d: %s is not complex?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  if (!s7_is_number(p))
    {fprintf(stderr, "%d: %s is not complex?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  if (s7_integer(p) != 123)
    {fprintf(stderr, "%d: %s is not 123?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  s2 = s7_number_to_string(sc, p, 10);
  if (strcmp(s2, "123") != 0)
    {fprintf(stderr, "%d: (number->string %s) is not \"123\"?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
  free(s2);

  s7_gc_unprotect_at(sc, gc_loc);
  

  p = s7_make_ratio(sc, 123, 5);
  gc_loc = s7_gc_protect(sc, p);

  if (s7_is_integer(p))
    {fprintf(stderr, "%d: %s is integral?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  if (!s7_is_rational(p))
    {fprintf(stderr, "%d: %s is not rational?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  if (!s7_is_ratio(p))
    {fprintf(stderr, "%d: %s is not a ratio?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  if (!s7_is_real(p))
    {fprintf(stderr, "%d: %s is not real?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  if (!s7_is_complex(p))
    {fprintf(stderr, "%d: %s is not complex?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  if (!s7_is_number(p))
    {fprintf(stderr, "%d: %s is not complex?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  if (s7_numerator(p) != 123)
    {fprintf(stderr, "%d: (numerator %s) is not 123?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  if (s7_denominator(p) != 5)
    {fprintf(stderr, "%d: (denominator %s) is not 5?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  s2 = s7_number_to_string(sc, p, 10);
  if (strcmp(s2, "123/5") != 0)
    {fprintf(stderr, "%d: (number->string %s) is not \"123/5\"?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
  free(s2);

  s7_gc_unprotect_at(sc, gc_loc);


  p = s7_make_real(sc, 1.5);
  gc_loc = s7_gc_protect(sc, p);

  if (s7_is_integer(p))
    {fprintf(stderr, "%d: %s is integral?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  if (s7_is_rational(p))
    {fprintf(stderr, "%d: %s is rational?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  if (s7_is_ratio(p))
    {fprintf(stderr, "%d: %s is a ratio?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  if (!s7_is_real(p))
    {fprintf(stderr, "%d: %s is not real?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  if (!s7_is_complex(p))
    {fprintf(stderr, "%d: %s is not complex?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  if (!s7_is_number(p))
    {fprintf(stderr, "%d: %s is not complex?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  if (s7_real(p) != 1.5)
    {fprintf(stderr, "%d: %s is not 1.5?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  s2 = s7_number_to_string(sc, p, 10);
  if (strcmp(s2, "1.5") != 0)
    {fprintf(stderr, "%d: (number->string %s) is not \"1.5\"?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
  free(s2);

  s7_gc_unprotect_at(sc, gc_loc);
  

  p = s7_make_complex(sc, 1.0, 1.0);
  gc_loc = s7_gc_protect(sc, p);

  if (s7_is_integer(p))
    {fprintf(stderr, "%d: %s is integral?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  if (s7_is_rational(p))
    {fprintf(stderr, "%d: %s is rational?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  if (s7_is_ratio(p))
    {fprintf(stderr, "%d: %s is a ratio?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  if (s7_is_real(p))
    {fprintf(stderr, "%d: %s is real?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  if (!s7_is_complex(p))
    {fprintf(stderr, "%d: %s is not complex?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  if (!s7_is_number(p))
    {fprintf(stderr, "%d: %s is not complex?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  if (s7_real_part(p) != 1.0)
    {fprintf(stderr, "%d: (real-part %s) is not 1.0?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  if (s7_imag_part(p) != 1.0)
    {fprintf(stderr, "%d: (imag-part %s) is not 1.0?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  s2 = s7_number_to_string(sc, p, 10);
  if (strcmp(s2, "1+1i") != 0)
    {fprintf(stderr, "%d: (number->string %s) is not \"1+1i\"?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
  free(s2);

  s7_gc_unprotect_at(sc, gc_loc);
  

  p = s7_make_vector(sc, 12);
  gc_loc = s7_gc_protect(sc, p);

  if (!s7_is_vector(p))
    {fprintf(stderr, "%d: %s is not a vector?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  if (s7_vector_rank(p) != 1)
    {fprintf(stderr, "%d: (dimensions %s) is not 1?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  s7_vector_set(sc, p, 1, s7_t(sc));
  if (s7_vector_ref(sc, p, 1) != s7_t(sc))
    {fprintf(stderr, "%d: (%s 1) is not #t?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  s7_vector_fill(sc, p, TO_S7_INT(123));
  if (s7_integer(s7_vector_ref(sc, p, 1)) != 123)
    {fprintf(stderr, "%d: (%s 1) is not 123?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  s7_gc_unprotect_at(sc, gc_loc);


  p = s7_make_string(sc, "1234");
  gc_loc = s7_gc_protect(sc, p);

  if (!s7_is_string(p))
    {fprintf(stderr, "%d: %s is not a string?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  if (s7_string_length(p) != 4)
    {fprintf(stderr, "%d: (length %s) is 4?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  if (strcmp(s7_string(p), "1234") != 0)
    {fprintf(stderr, "%d: %s is \"1234\"?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  s7_gc_unprotect_at(sc, gc_loc);

  
  p = s7_make_character(sc, 65);
  if (!s7_is_character(p))
    {fprintf(stderr, "%d: %s is not a character?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  if (s7_character(p) != 'A')
    {fprintf(stderr, "%d: %s is not #\\A?\n", __LINE__, s1 = TO_STR(p)); free(s1);}


  p = s7_list(sc, 3, TO_S7_INT(1), TO_S7_INT(2), TO_S7_INT(3));
  gc_loc = s7_gc_protect(sc, p);

  if (!s7_is_list(sc, p))
    {fprintf(stderr, "%d: %s is not a list?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
  
  if (s7_list_length(sc, p) != 3)
    {fprintf(stderr, "%d: (length %s) is not 3?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
  
  if (s7_integer(s7_list_ref(sc, p, 1)) != 2)
    {fprintf(stderr, "%d: (%s 1) is not 2?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
  
  if (s7_integer(s7_car(p)) != 1)
    {fprintf(stderr, "%d: (car %s) is not 1?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
  
  if (s7_integer(s7_cadr(p)) != 2)
    {fprintf(stderr, "%d: (cadr %s) is not 2?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
  
  if (s7_integer(s7_caddr(p)) != 3)
    {fprintf(stderr, "%d: (caddr %s) is not 2?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
  
  if (s7_integer(s7_car(s7_cddr(p))) != 3)
    {fprintf(stderr, "%d: (car (cddr %s)) is not 2?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
  
  s7_list_set(sc, p, 1, s7_f(sc));
  if (s7_list_ref(sc, p, 1) != s7_f(sc))
    {fprintf(stderr, "%d: (%s 1) is not #f?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
  
  s7_gc_unprotect_at(sc, gc_loc);


  {
    s7_pointer c1, c2, c3, c12, c23, c123, c1234, c1d2, c2d3, c3d4, c12d3, c23d4, c123d4, c1234d5;
    s7_gc_on(sc, false);
    c1 = s7_list(sc, 1, TO_S7_INT(1));                                              /* (1) */
    c2 = s7_list(sc, 1, TO_S7_INT(2));                                              /* (2) */
    c3 = s7_list(sc, 1, TO_S7_INT(3));                                              /* (3) */
    c12 = s7_list(sc, 2, TO_S7_INT(1), TO_S7_INT(2));                               /* (1 2) */
    c23 = s7_list(sc, 2, TO_S7_INT(2), TO_S7_INT(3));                               /* (2 3) */
    c123 = s7_list(sc, 3, TO_S7_INT(1), TO_S7_INT(2), TO_S7_INT(3));                /* (1 2 3) */
    c1234 = s7_list(sc, 4, TO_S7_INT(1), TO_S7_INT(2), TO_S7_INT(3), TO_S7_INT(4)); /* (1 2 3 4) */
    c1d2 = s7_cons(sc, TO_S7_INT(1), TO_S7_INT(2));                                 /* (1 . 2) */
    c2d3 = s7_cons(sc, TO_S7_INT(2), TO_S7_INT(3));                                 /* (2 . 3) */
    c3d4 = s7_cons(sc, TO_S7_INT(3), TO_S7_INT(4));                                 /* (3 . 4) */
    c12d3 = s7_cons(sc, TO_S7_INT(1), s7_cons(sc, TO_S7_INT(2), TO_S7_INT(3)));     /* (1 2 . 3) */
    c23d4 = s7_cons(sc, TO_S7_INT(2), s7_cons(sc, TO_S7_INT(3), TO_S7_INT(4)));     /* (2 3 . 4) */
    c123d4 = s7_cons(sc, TO_S7_INT(1), s7_cons(sc, TO_S7_INT(2), s7_cons(sc, TO_S7_INT(3), TO_S7_INT(4))));                             /* (1 2 3 . 4) */
    c1234d5 = s7_cons(sc, TO_S7_INT(1), s7_cons(sc, TO_S7_INT(2), s7_cons(sc, TO_S7_INT(3), s7_cons(sc, TO_S7_INT(4), TO_S7_INT(5))))); /* (1 2 3 4 . 5) */
    
    if (s7_integer(p = s7_caar(s7_list(sc, 1, c1))) != 1)
      {fprintf(stderr, "%d: caar is %s?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
    
    if (s7_integer(p = s7_cadr(c12)) != 2)
      {fprintf(stderr, "%d: cadr is %s?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
    
    if (s7_integer(p = s7_cdar(s7_list(sc, 1, c1d2))) != 2)
      {fprintf(stderr, "%d: cdar is %s?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
    
    if (s7_integer(p = s7_cddr(c12d3)) != 3)
      {fprintf(stderr, "%d: cddr is %s?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
    
    if (s7_integer(p = s7_caaar(s7_list(sc, 1, s7_list(sc, 1, c1)))) != 1)
      {fprintf(stderr, "%d: caaar is %s?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
    
    if (s7_integer(p = s7_caadr(s7_list(sc, 2, TO_S7_INT(1), c2))) != 2)
      {fprintf(stderr, "%d: caadr is %s?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
    
    if (s7_integer(p = s7_cadar(s7_list(sc, 1, c12))) != 2)
      {fprintf(stderr, "%d: cadar is %s?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
    
    if (s7_integer(p = s7_cdaar(s7_list(sc, 1, s7_list(sc, 1, c1d2)))) != 2)
      {fprintf(stderr, "%d: cdaar is %s?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
    
    if (s7_integer(p = s7_caddr(c123)) != 3)
      {fprintf(stderr, "%d: caddr is %s?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
    
    if (s7_integer(p = s7_cdddr(c123d4)) != 4)
      {fprintf(stderr, "%d: cdddr is %s?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
    
    if (s7_integer(p = s7_cdadr(s7_list(sc, 2, TO_S7_INT(1), c2d3))) != 3)
      {fprintf(stderr, "%d: cdadr is %s?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
    
    if (s7_integer(p = s7_cddar(s7_list(sc, 1, c12d3))) != 3)
      {fprintf(stderr, "%d: cddar is %s?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
    
    if (s7_integer(p = s7_caaaar(s7_list(sc, 1, s7_list(sc, 1, s7_list(sc, 1, c1))))) != 1)
      {fprintf(stderr, "%d: caaaar is %s?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
    
    if (s7_integer(p = s7_caaadr(s7_list(sc, 2, TO_S7_INT(1), s7_list(sc, 1, c2)))) != 2)
      {fprintf(stderr, "%d: caaadr is %s?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
    
    if (s7_integer(p = s7_caadar(s7_list(sc, 1, s7_list(sc, 2, TO_S7_INT(1), c2)))) != 2)
      {fprintf(stderr, "%d: caadar is %s?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
    
    if (s7_integer(p = s7_cadaar(s7_list(sc, 1, s7_list(sc, 1, c12)))) != 2)
      {fprintf(stderr, "%d: cadaar is %s?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
    
    if (s7_integer(p = s7_caaddr(s7_list(sc, 3, TO_S7_INT(1), TO_S7_INT(2), c3))) != 3)
      {fprintf(stderr, "%d: caaddr is %s?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
    
    if (s7_integer(p = s7_cadddr(c1234)) != 4)
      {fprintf(stderr, "%d: cadddr is %s?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
    
    if (s7_integer(p = s7_cadadr(s7_list(sc, 2, TO_S7_INT(1), c23))) != 3)
      {fprintf(stderr, "%d: cadadr is %s?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
    
    if (s7_integer(p = s7_caddar(s7_list(sc, 1, c123))) != 3)
      {fprintf(stderr, "%d: caddar is %s?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
    
    if (s7_integer(p = s7_cdaaar(s7_list(sc, 1, s7_list(sc, 1, s7_list(sc, 1, c1d2))))) != 2)
      {fprintf(stderr, "%d: cdaaar is %s?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
    
    if (s7_integer(p = s7_cdaadr(s7_list(sc, 2, TO_S7_INT(1), s7_list(sc, 1, c2d3)))) != 3)
      {fprintf(stderr, "%d: cdaadr is %s?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
    
    if (s7_integer(p = s7_cdadar(s7_list(sc, 1, s7_list(sc, 2, TO_S7_INT(1), c2d3)))) != 3)
      {fprintf(stderr, "%d: cdadar is %s?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
    
    if (s7_integer(p = s7_cddaar(s7_list(sc, 1, s7_list(sc, 1, c12d3)))) != 3)
      {fprintf(stderr, "%d: cddaar is %s?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
    
    if (s7_integer(p = s7_cdaddr(s7_list(sc, 3, TO_S7_INT(1), TO_S7_INT(2), c3d4))) != 4)
      {fprintf(stderr, "%d: cdaddr is %s?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
    
    if (s7_integer(p = s7_cddddr(c1234d5)) != 5)
      {fprintf(stderr, "%d: cdddd is %s?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
    
    if (s7_integer(p = s7_cddadr(s7_list(sc, 2, TO_S7_INT(1), c23d4))) != 4)
      {fprintf(stderr, "%d: cddadr is %s?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
    
    if (s7_integer(p = s7_cdddar(s7_list(sc, 1, c123d4))) != 4)
      {fprintf(stderr, "%d: cdddar is %s?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
    
    s7_gc_on(sc, true);
  }


  p = s7_make_ulong(sc, 123);
  gc_loc = s7_gc_protect(sc, p);

  if (!s7_is_ulong(p))
    {fprintf(stderr, "%d: %s is not a ulong?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
  
  if (s7_ulong(p) != (unsigned long)123)
    {fprintf(stderr, "%d: %s is not 123?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
  s7_gc_unprotect_at(sc, gc_loc);


  p = s7_make_ulong_long(sc, 123);
  gc_loc = s7_gc_protect(sc, p);

  if (!s7_is_ulong_long(p))
    {fprintf(stderr, "%d: %s is not a ulong_long?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
  
  if (s7_ulong_long(p) != (unsigned long long)123)
    {fprintf(stderr, "%d: %s is not 123?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
  s7_gc_unprotect_at(sc, gc_loc);


  p = s7_make_hash_table(sc, 255);
  gc_loc = s7_gc_protect(sc, p);

  if (!s7_is_hash_table(p))
    {fprintf(stderr, "%d: %s is not a hash-table?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
  
  if (s7_hash_table_ref(sc, p, s7_eof_object(sc)) != s7_f(sc))
    {fprintf(stderr, "%d: (hash-table-ref %s #<eof>) is not #f?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  s7_hash_table_set(sc, p, s7_eof_object(sc), s7_unspecified(sc));
  if (s7_hash_table_ref(sc, p, s7_eof_object(sc)) != s7_unspecified(sc))
    {fprintf(stderr, "%d: (hash-table-ref %s #<eof>) is not #<unspecified>?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
  s7_gc_unprotect_at(sc, gc_loc);

  p = s7_current_input_port(sc);
  if (!s7_is_input_port(sc, p))
    {fprintf(stderr, "%d: %s is not an input port?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  p = s7_current_output_port(sc);
  if (!s7_is_output_port(sc, p))
    {fprintf(stderr, "%d: %s is not an output port?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  p = s7_name_to_value(sc, "abs");
  if (!s7_is_procedure(p))
    {fprintf(stderr, "%d: %s is not a procedure?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
  
  p = s7_make_symbol(sc, "abs");
  if (!s7_is_symbol(p))
    {fprintf(stderr, "%d: %s is not a symbol?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
  
  p = s7_gensym(sc, "abs");
  if (!s7_is_symbol(p))
    {fprintf(stderr, "%d: %s is not a symbol?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
  
  p = s7_make_keyword(sc, "key");
  if (!s7_is_keyword(p))
    {fprintf(stderr, "%d: %s is not a keyword?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  if (!s7_keyword_eq_p(p, p))
    {fprintf(stderr, "%d: %s is not a self-eq??\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  p = s7_global_environment(sc);
  if (!s7_is_environment(p))
    {fprintf(stderr, "%d: %s is not an environment?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
  
  p = s7_current_environment(sc);
  if ((!s7_is_null(sc, p)) && (!s7_is_environment(p)))
    {fprintf(stderr, "%d: %s is not an environment?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
  
  s7_define_constant(sc, "a_constant", s7_t(sc));
  if (!s7_is_constant(s7_name_to_value(sc, "a_constant")))
    {fprintf(stderr, "%d: a_constant is not a constant?\n", __LINE__);}
  if (!s7_is_defined(sc, "a_constant"))
    {fprintf(stderr, "%d: a_constant is not defined?\n", __LINE__);}
  
  s7_define_function(sc, "a_function", a_function, 1, 0, false, "a function");
  if (!s7_is_defined(sc, "a_function"))
    {fprintf(stderr, "%d: a_function is not defined?\n", __LINE__);}
  if (!s7_is_function(s7_name_to_value(sc, "a_function")))
    {fprintf(stderr, "%d: a_function is not a function?\n", __LINE__);}

  p = s7_apply_function(sc, s7_name_to_value(sc, "a_function"), s7_cons(sc, TO_S7_INT(32), s7_nil(sc)));
  if (!s7_is_integer(p))
    {fprintf(stderr, "%d: %s is not an integer?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
  if (s7_integer(p) != 32)
    {fprintf(stderr, "%d: %s is not 32?\n", __LINE__, s1 = TO_STR(p)); free(s1);}


  dax_type_tag = s7_new_type("dax", print_dax, free_dax, equal_dax, mark_dax, NULL, NULL);
  s7_define_function(sc, "make-dax", make_dax, 2, 0, false, "(make-dax x data) makes a new dax");
  s7_define_function(sc, "dax?", is_dax, 1, 0, false, "(dax? anything) returns #t if its argument is a dax object");

  s7_define_variable(sc, "dax-x", 
                     s7_make_procedure_with_setter(sc, "dax-x", dax_x, 1, 0, set_dax_x, 2, 0, "dax x field (a real)"));

  s7_define_variable(sc, "dax-data", 
                     s7_make_procedure_with_setter(sc, "dax-data", dax_data, 1, 0, set_dax_data, 2, 0, "dax data field"));

  if (!s7_is_procedure_with_setter(s7_name_to_value(sc, "dax-x")))
    {fprintf(stderr, "%d: dax-x is not a pws?\n", __LINE__);}

  p = make_dax(sc, s7_cons(sc, s7_make_real(sc, 1.0), s7_cons(sc, TO_S7_INT(2), s7_nil(sc))));
  gc_loc = s7_gc_protect(sc, p);

  if (!s7_is_object(p))
    {fprintf(stderr, "%d: %s is not an object?\n", __LINE__, s1 = TO_STR(p)); free(s1);}    

  p1 = s7_apply_function(sc, s7_name_to_value(sc, "dax?"), s7_cons(sc, p, s7_nil(sc)));
  if (p1 != s7_t(sc))
    {fprintf(stderr, "%d: %s is not a dax object?\n", __LINE__, s1 = TO_STR(p)); free(s1);}

  s1 = TO_STR(p);
  if (strcmp(s1, "#<dax 1.000 2>") != 0)
    {fprintf(stderr, "%d: dax prints as %s?\n", __LINE__, s2 = TO_STR(p)); free(s2);}    
  free(s1);

  p1 = s7_apply_function(sc, s7_name_to_value(sc, "dax-data"), s7_cons(sc, p, s7_nil(sc)));
  if (!s7_is_integer(p1))
    {fprintf(stderr, "%d: %s is not an integer?\n", __LINE__, s1 = TO_STR(p1)); free(s1);}
  if (s7_integer(p1) != 2)
    {fprintf(stderr, "%d: %s is not 2?\n", __LINE__, s1 = TO_STR(p1)); free(s1);}

  s7_apply_function(sc, s7_procedure_setter(sc, s7_name_to_value(sc, "dax-data")), s7_cons(sc, p, s7_cons(sc, TO_S7_INT(32), s7_nil(sc))));
  p1 = s7_apply_function(sc, s7_name_to_value(sc, "dax-data"), s7_cons(sc, p, s7_nil(sc)));
  if (!s7_is_integer(p1))
    {fprintf(stderr, "%d: %s is not an integer?\n", __LINE__, s1 = TO_STR(p1)); free(s1);}
  if (s7_integer(p1) != 32)
    {fprintf(stderr, "%d: %s is not 32?\n", __LINE__, s1 = TO_STR(p1)); free(s1);}

  s7_gc_unprotect_at(sc, gc_loc);


  s7_define_function_star(sc, "plus", plus, "(red 32) blue", "an example of define* from C");
  if (!s7_is_procedure(s7_name_to_value(sc, "plus")))
    {fprintf(stderr, "%d: plus is not a function?\n", __LINE__);}

  p = s7_apply_function(sc, s7_name_to_value(sc, "plus"), s7_cons(sc, TO_S7_INT(1), s7_cons(sc, TO_S7_INT(2), s7_nil(sc))));
  if (!s7_is_integer(p))
    {fprintf(stderr, "%d: %s is not an integer?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
  if (s7_integer(p) != 4)
    {fprintf(stderr, "%d: %s is not 4?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
  
  p = s7_apply_function(sc, s7_name_to_value(sc, "plus"), s7_cons(sc, s7_make_keyword(sc, "blue"), s7_cons(sc, TO_S7_INT(2), s7_nil(sc))));
  if (!s7_is_integer(p))
    {fprintf(stderr, "%d: %s is not an integer?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
  if (s7_integer(p) != 66)
    {fprintf(stderr, "%d: %s is not 66?\n", __LINE__, s1 = TO_STR(p)); free(s1);}
  

  return(0);
}

