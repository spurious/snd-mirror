/* s7 ffi tester [work in progress...]
 *
 * gcc -o ffitest ffitest.c -g3 -Wall s7.o -lm -I. -ldl
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#define HAVE_STDBOOL_H 1
#include "s7.h"

#define TO_C(x) s7_object_to_c_string(sc, x)


int main(int argc, char **argv)
{
  s7_scheme *sc;
  s7_pointer p;
  int i, gc_loc;
  char *s1, *s2;

  sc = s7_init(); 
  
  /* try each straight (no errors) case */

  if (!s7_is_null(sc, s7_nil(sc))) 
    {fprintf(stderr, "%d: %s is not null?\n", __LINE__, s1 = TO_C(s7_nil(sc))); free(s1);}

  if (s7_is_pair(s7_nil(sc))) 
    {fprintf(stderr, "%d: %s is a pair?\n", __LINE__, s1 = TO_C(s7_nil(sc))); free(s1);}

  if (!s7_is_boolean(s7_t(sc))) 
    {fprintf(stderr, "%d: %s is not boolean?\n", __LINE__, s1 = TO_C(s7_t(sc))); free(s1);}

  if (!s7_is_boolean(s7_f(sc))) 
    {fprintf(stderr, "%d: %s is not boolean?\n", __LINE__, s1 = TO_C(s7_f(sc))); free(s1);}

  if (s7_boolean(sc, s7_f(sc)))
    {fprintf(stderr, "%d: %s is #t?\n", __LINE__, s1 = TO_C(s7_f(sc))); free(s1);}

  if (!s7_boolean(sc, s7_t(sc)))
    {fprintf(stderr, "%d: %s is #f?\n", __LINE__, s1 = TO_C(s7_t(sc))); free(s1);}

  p = s7_make_boolean(sc, true);
  if (p != s7_t(sc))
    {fprintf(stderr, "%d: %s is not #t?\n", __LINE__, s1 = TO_C(p)); free(s1);}

  p = s7_make_boolean(sc, false);
  if (p != s7_f(sc))
    {fprintf(stderr, "%d: %s is not #f?\n", __LINE__, s1 = TO_C(p)); free(s1);}

  if (!s7_is_eq(s7_f(sc), s7_f(sc))) 
    {fprintf(stderr, "%d: (eq? %s %s) -> #f?\n", __LINE__, s1 = TO_C(s7_f(sc)), s2 = TO_C(s7_f(sc))); free(s1); free(s2);}

  if (!s7_is_eqv(s7_f(sc), s7_f(sc))) 
    {fprintf(stderr, "%d: (eqv? %s %s) -> #f?\n", __LINE__, s1 = TO_C(s7_f(sc)), s2 = TO_C(s7_f(sc))); free(s1); free(s2);}

  if (!s7_is_equal(sc, s7_f(sc), s7_f(sc))) 
    {fprintf(stderr, "%d: (equal? %s %s) -> #f?\n", __LINE__, s1 = TO_C(s7_f(sc)), s2 = TO_C(s7_f(sc))); free(s1); free(s2);}

  if (!s7_is_unspecified(sc, s7_unspecified(sc))) 
    {fprintf(stderr, "%d: %s is not #<unspecified>?\n", __LINE__, s1 = TO_C(s7_unspecified(sc))); free(s1);}

  if (s7_is_eq(s7_eof_object(sc), s7_undefined(sc)))
    {fprintf(stderr, "%d: (eq? %s %s) -> #t?\n", __LINE__, s1 = TO_C(s7_eof_object(sc)), s2 = TO_C(s7_undefined(sc))); free(s1); free(s2);}

  if (s7_is_eqv(s7_eof_object(sc), s7_undefined(sc)))
    {fprintf(stderr, "%d: (eqv? %s %s) -> #t?\n", __LINE__, s1 = TO_C(s7_eof_object(sc)), s2 = TO_C(s7_undefined(sc))); free(s1); free(s2);}

  if (s7_is_equal(sc, s7_eof_object(sc), s7_undefined(sc)))
    {fprintf(stderr, "%d: (equal? %s %s) -> #t?\n", __LINE__, s1 = TO_C(s7_eof_object(sc)), s2 = TO_C(s7_undefined(sc))); free(s1); free(s2);}

  if (!s7_is_valid_pointer(s7_t(sc)))
    {fprintf(stderr, "%d: %s is not a valid pointer?\n", __LINE__, s1 = TO_C(s7_t(sc))); free(s1);}

  if (s7_is_c_pointer(s7_t(sc)))
    {fprintf(stderr, "%d: %s is a raw c pointer?\n", __LINE__, s1 = TO_C(s7_t(sc))); free(s1);}

  i = 32;
  p = s7_make_c_pointer(sc, (void *)(&i));
  if (!s7_is_c_pointer(p))
    {fprintf(stderr, "%d: %s is not a raw c pointer?\n", __LINE__, s1 = TO_C(p)); free(s1);}

  i = (*((int *)s7_c_pointer(p)));
  if (i != 32)
    fprintf(stderr, "%d: 32 -> %d via raw c pointer?\n", __LINE__, i);

  s7_provide(sc, "ffitest");
  if (!s7_is_provided(sc, "ffitest"))
    {fprintf(stderr, "%d: *features* %s doesn't provide 'ffitest?\n", __LINE__, s1 = TO_C(s7_name_to_value(sc, "*features*"))); free(s1);}

  p = s7_cons(sc, s7_f(sc), s7_t(sc));
  gc_loc = s7_gc_protect(sc, p);
  if (p != s7_gc_protected_at(sc, gc_loc))
    {fprintf(stderr, "%d: %s is not gc protected at %d: %s?\n", __LINE__, s1 = TO_C(p), gc_loc, s2 = TO_C(s7_gc_protected_at(sc, gc_loc))); free(s1); free(s2);}
  
  if (s7_car(p) != s7_f(sc))
    {fprintf(stderr, "%d: (car %s) is not #f?\n", __LINE__, s1 = TO_C(p)); free(s1);}

  if (s7_cdr(p) != s7_t(sc))
    {fprintf(stderr, "%d: (cdr %s) is not #t?\n", __LINE__, s1 = TO_C(p)); free(s1);}
  
  if (!s7_is_pair(p))
    {fprintf(stderr, "%d: %s is not a pair?\n", __LINE__, s1 = TO_C(p)); free(s1);}

  s7_set_car(p, s7_eof_object(sc));
  if (s7_car(p) != s7_eof_object(sc))
    {fprintf(stderr, "%d: (car %s) is not #<eof>?\n", __LINE__, s1 = TO_C(p)); free(s1);}

  s7_set_cdr(p, s7_unspecified(sc));
  if (s7_cdr(p) != s7_unspecified(sc))
    {fprintf(stderr, "%d: (cdr %s) is not #<unspecified>?\n", __LINE__, s1 = TO_C(p)); free(s1);}

  s7_gc_unprotect_at(sc, gc_loc);


  return(0);
}

