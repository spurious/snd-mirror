#ifndef _S7_H
#define _S7_H

#define S7_VERSION "0.9"
#define S7_DATE "25-Sep-08"

#include <stdio.h>

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

#define s7_Int off_t
#define s7_Int_d "%lld"


typedef struct s7_scheme s7_scheme;
typedef struct s7_cell *s7_pointer;

typedef s7_pointer (*s7_function)(s7_scheme *sc, s7_pointer a);

s7_pointer s7_F(s7_scheme *sc);
s7_pointer s7_T(s7_scheme *sc);
s7_pointer s7_NIL(s7_scheme *sc);
s7_pointer s7_UNDEFINED(s7_scheme *sc);
s7_pointer s7_EOF_OBJECT(s7_scheme *sc);

s7_pointer s7_global_environment(s7_scheme *sc);
s7_pointer s7_current_environment(s7_scheme *sc);

bool s7_is_immutable(s7_pointer p);
s7_pointer s7_set_immutable(s7_pointer p);
s7_pointer s7_immutable_cons(s7_scheme *sc, s7_pointer a, s7_pointer b);

s7_pointer s7_cons(s7_scheme *sc, s7_pointer a, s7_pointer b);
s7_pointer s7_car(s7_pointer p);
s7_pointer s7_cdr(s7_pointer p);
bool s7_is_pair(s7_pointer p);
s7_pointer s7_set_car(s7_pointer p, s7_pointer q);
s7_pointer s7_set_cdr(s7_pointer p, s7_pointer q);

int s7_list_length(s7_scheme *sc, s7_pointer a);
s7_pointer s7_reverse(s7_scheme *sc, s7_pointer a);
s7_pointer s7_reverse_in_place(s7_scheme *sc, s7_pointer term, s7_pointer list);
s7_pointer s7_append(s7_scheme *sc, s7_pointer a, s7_pointer b);
s7_pointer s7_list_ref(s7_scheme *sc, s7_pointer lst, int num);
s7_pointer s7_list_set(s7_scheme *sc, s7_pointer lst, int num, s7_pointer val);
s7_pointer s7_assoc(s7_scheme *sc, s7_pointer sym, s7_pointer lst);
s7_pointer s7_member(s7_scheme *sc, s7_pointer sym, s7_pointer lst);
s7_pointer s7_remv(s7_scheme *sc, s7_pointer a, s7_pointer obj);
bool s7_is_list(s7_scheme *sc, s7_pointer p);

bool s7_is_string(s7_pointer p);
char *s7_string(s7_pointer p);
s7_pointer s7_make_string(s7_scheme *sc, const char *str);
s7_pointer s7_make_counted_string(s7_scheme *sc, const char *str, int len);

bool s7_is_character(s7_pointer p);
char s7_character(s7_pointer p);
s7_pointer s7_make_character(s7_scheme *sc, int c);

bool s7_is_number(s7_pointer p);
bool s7_is_exact(s7_pointer p);
bool s7_is_inexact(s7_pointer p);

s7_Int s7_integer(s7_pointer p);
bool s7_is_integer(s7_pointer p);
s7_pointer s7_make_integer(s7_scheme *sc, s7_Int num);

bool s7_is_real(s7_pointer p);
double s7_real(s7_pointer p);
s7_pointer s7_make_real(s7_scheme *sc, double num);

bool s7_is_ulong(s7_pointer arg);
unsigned long s7_ulong(s7_pointer num);
s7_pointer s7_make_ulong(s7_scheme *sc, unsigned long num);

bool s7_is_rational(s7_pointer arg);
bool s7_is_ratio(s7_pointer arg);
s7_pointer s7_make_ratio(s7_scheme *sc, s7_Int a, s7_Int b);
s7_pointer s7_rationalize(s7_scheme *sc, double x, double error);
s7_Int s7_numerator(s7_pointer x);
s7_Int s7_denominator(s7_pointer x);

bool s7_is_complex(s7_pointer arg);
s7_pointer s7_make_complex(s7_scheme *sc, double a, double b);
double s7_real_part(s7_pointer z);
double s7_imag_part(s7_pointer z);

char *s7_number_to_string(s7_scheme *sc, s7_pointer obj, int radix);

bool s7_is_vector(s7_pointer p);
void s7_vector_fill(s7_pointer vec, s7_pointer obj);
s7_pointer s7_vector_ref(s7_pointer vec, int ielem);
s7_pointer s7_vector_set(s7_pointer vec, int ielem, s7_pointer a);
s7_pointer s7_make_vector(s7_scheme *sc, int len);
s7_pointer s7_make_and_fill_vector(s7_scheme *sc, int len, s7_pointer fill);
int s7_vector_length(s7_pointer vec);
s7_pointer s7_vector_to_list(s7_scheme *sc, s7_pointer vect);

bool s7_is_symbol(s7_pointer p);
char *s7_symbol_name(s7_pointer p);
s7_pointer s7_make_symbol(s7_scheme *sc, const char *name);
s7_pointer s7_gensym(s7_scheme *sc, const char *prefix);

bool s7_is_keyword(s7_pointer obj);
s7_pointer s7_make_keyword(s7_scheme *sc, const char *key);
bool s7_keyword_eq_p(s7_pointer obj1, s7_pointer obj2);

bool s7_is_function(s7_pointer p);
s7_pointer s7_make_function(s7_scheme *sc, const char *name, s7_function fnc, int required_args, int optional_args, bool rest_arg, const char *doc);
void s7_define_function(s7_scheme *sc, const char *name, s7_function fnc, int required_args, int optional_args, bool rest_arg, const char *doc);
char *s7_procedure_documentation(s7_scheme *sc, s7_pointer p);
s7_pointer s7_procedure_arity(s7_scheme *sc, s7_pointer x);

int s7_new_type(const char *name, 
		char *(*print)(void *value), 
		void (*free)(void *value), 
		bool (*equal)(void *val1, void *val2),
		void (*gc_mark)(void *val),
		s7_pointer (*apply)(s7_scheme *sc, s7_pointer obj, s7_pointer args),
		s7_pointer (*set)(s7_scheme *sc, s7_pointer obj, s7_pointer args));

void *s7_object_value(s7_pointer obj);
int s7_object_type(s7_pointer obj);
bool s7_is_object(s7_pointer p);
s7_pointer s7_make_object(s7_scheme *sc, int type, void *value);
char *s7_describe_object(s7_pointer a);
void s7_free_object(s7_pointer a);
bool s7_equalp_objects(s7_pointer a, s7_pointer b);
void s7_mark_object(s7_pointer a);

void s7_define_variable(s7_scheme *sc, const char *name, s7_pointer value);


bool s7_is_closure(s7_pointer p);
bool s7_is_procedure(s7_pointer x);
s7_pointer s7_procedure_source(s7_scheme *sc, s7_pointer p);
s7_pointer s7_procedure_environment(s7_pointer p);
s7_pointer s7_make_closure(s7_scheme *sc, s7_pointer c, s7_pointer e);
bool s7_is_continuation(s7_pointer p);

s7_scheme *s7_init(void);

s7_pointer s7_call(s7_scheme *sc, s7_pointer func, s7_pointer args);

bool s7_is_eqv(s7_pointer a, s7_pointer b);
bool s7_is_eq(s7_pointer obj1, s7_pointer obj2);
bool s7_is_equal(s7_pointer obj1, s7_pointer obj2);

void s7_define(s7_scheme *sc, s7_pointer env, s7_pointer symbol, s7_pointer value);
s7_pointer s7_name_to_value(s7_scheme *sc, const char *name);
s7_pointer s7_symbol_value(s7_scheme *sc, s7_pointer sym);
s7_pointer s7_symbol_set_value(s7_scheme *sc, s7_pointer sym, s7_pointer val);
s7_pointer s7_symbol_local_value(s7_scheme *sc, s7_pointer sym, s7_pointer local_env);

s7_pointer s7_eval_c_string(s7_scheme *sc, const char *str);

s7_pointer s7_object_to_string(s7_scheme *sc, s7_pointer arg);
char *s7_object_to_c_string(s7_scheme *sc, s7_pointer obj);

s7_pointer s7_load(s7_scheme *sc, const char *file);
s7_pointer s7_load_path(s7_scheme *sc);
s7_pointer s7_add_to_load_path(s7_scheme *sc, const char *dir);

void s7_provide(s7_scheme *sc, const char *feature);

s7_pointer s7_error(s7_scheme *sc, s7_pointer type, s7_pointer info);
s7_pointer s7_error_and_exit(s7_scheme *sc, s7_pointer type, s7_pointer info);
s7_pointer s7_wrong_type_arg_error(s7_scheme *sc, const char *caller, int arg_n, s7_pointer arg, const char *descr);
s7_pointer s7_out_of_range_error(s7_scheme *sc, const char *caller, int arg_n, s7_pointer arg, const char *descr);
void s7_set_error_exiter(s7_scheme *sc, void (*error_exiter)(void));

int s7_gc_protect(s7_scheme *sc, s7_pointer x);
void s7_gc_unprotect(s7_scheme *sc, s7_pointer x);
void s7_gc_unprotect_at(s7_scheme *sc, int loc);
s7_pointer s7_local_gc_protect(s7_pointer p);
s7_pointer s7_local_gc_unprotect(s7_pointer p);
s7_pointer s7_gc_on(s7_scheme *sc, bool on, s7_pointer p);
s7_pointer s7_ungc(s7_scheme *sc, s7_pointer p);

void s7_for_each_symbol_name(s7_scheme *sc, bool (*symbol_func)(const char *symbol_name, void *data), void *data);
void s7_for_each_symbol(s7_scheme *sc, bool (*symbol_func)(const char *symbol_name, s7_pointer symbol_value, void *data), void *data);

bool s7_is_input_port(s7_scheme *sc, s7_pointer p);
bool s7_is_output_port(s7_scheme *sc, s7_pointer p);
s7_pointer s7_current_input_port(s7_scheme *sc);
s7_pointer s7_current_output_port(s7_scheme *sc);
s7_pointer s7_current_error_port(s7_scheme *sc);
s7_pointer s7_set_current_error_port(s7_scheme *sc, s7_pointer port);
void s7_close_input_port(s7_scheme *sc, s7_pointer p);
void s7_close_output_port(s7_scheme *sc, s7_pointer p);
s7_pointer s7_open_input_file(s7_scheme *sc, const char *name, const char *mode);
s7_pointer s7_open_output_file(s7_scheme *sc, const char *name, const char *mode);
s7_pointer s7_open_input_string(s7_scheme *sc, const char *input_string);
s7_pointer s7_open_output_string(s7_scheme *sc);
char *s7_get_output_string(s7_scheme *sc, s7_pointer out_port);
char s7_read_char(s7_scheme *sc, s7_pointer port);
char s7_peek_char(s7_scheme *sc, s7_pointer port);
s7_pointer s7_read(s7_scheme *sc, s7_pointer port);
void s7_newline(s7_scheme *sc, s7_pointer port);
void s7_write_char(s7_scheme *sc, char c, s7_pointer port);
void s7_write(s7_scheme *sc, s7_pointer obj, s7_pointer port);
void s7_display(s7_scheme *sc, s7_pointer obj, s7_pointer port);

s7_pointer s7_make_procedure_with_setter(s7_scheme *sc, 
					 s7_pointer (*getter)(s7_scheme *sc, s7_pointer args), 
					 int get_req_args, int get_opt_args,
					 s7_pointer (*setter)(s7_scheme *sc, s7_pointer args),
					 int set_req_args, int set_opt_args,
					 const char *documentation);
bool s7_is_procedure_with_setter(s7_pointer obj);
s7_pointer s7_procedure_with_setter_setter(s7_pointer obj);
s7_pointer s7_procedure_with_setter_getter(s7_pointer obj);

#define s7_make_hash_table(Sc, Size) s7_make_vector(Sc, Size)
s7_pointer s7_hash_table_ref(s7_scheme *sc, s7_pointer table, const char *name);
s7_pointer s7_hash_table_set(s7_scheme *sc, s7_pointer table, const char *name, s7_pointer value);

#endif


/* -------------------------------- examples --------------------------------
 *
 *  these examples use:
 *
 *    #define s7_Int int
 *    #define s7_Int_d "%d"
 *
 *  in s7.h
 *
 *
 * a read-eval-print loop using S7: 
 */

#if 0

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "s7.h"

static s7_pointer our_exit(s7_scheme *sc, s7_pointer args)
{                                     /* all added functions have this form, args is a list, 
				       *   s7_car(args) is the 1st arg, etc 
				       */
  exit(1);
  return(s7_NIL(sc));                 /* just to be pedantic */
}

int main(int argc, char **argv)
{
  s7_scheme *s7;
  char buffer[512];
  char response[1024];

  s7 = s7_init();                     /* initialize the interpreter */
  
  s7_define_function(s7, "exit", our_exit, 0, 0, false, "(exit) exits the program");

                                      /* add the function "exit" to the interpreter.
                                       *   0, 0, false -> no required args,
				       *                  no optional args,
				       *                  no "rest" arg
				       */
  while (1)                           /* fire up a "repl" */
    {
      fprintf(stdout, "\n> ");        /* prompt for input */
      fgets(buffer, 512, stdin);

      if ((buffer[0] != '\n') || 
	  (strlen(buffer) > 1))
	{                            
	  sprintf(response, "(write %s)", buffer);
	  s7_eval_c_string(s7, response); /* evaluate input and write the result */
	}
    }
}

/* make mus-config.h (it can be empty), then
 *
 *   gcc -c s7.c -I.
 *   gcc -o doc7 doc7.c s7.o -lm -I.
 *
 * run it:
 *
 *    > (+ 1 2)
 *    3
 *    > (define (add1 x) (+ 1 x))
 *    add1
 *    > (add1 2)
 *    3
 *    > (exit)
 */

#endif

/* -------------------------------------------------------------------------------- */

/* define a function with args/returned value, and a variable */

#if 0

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "s7.h"

static s7_pointer our_exit(s7_scheme *sc, s7_pointer args)
{
  exit(1);
  return(s7_NIL(sc));
}

static s7_pointer add1(s7_scheme *sc, s7_pointer args)
{
  if (s7_is_integer(s7_car(args)))
    return(s7_make_integer(sc, 1 + s7_integer(s7_car(args))));
  return(s7_wrong_type_arg_error(sc, "add1", 1, s7_car(args), "an integer"));
}

int main(int argc, char **argv)
{
  s7_scheme *s7;
  char buffer[512];
  char response[1024];

  s7 = s7_init();                     /* initialize the interpreter */
  
  s7_define_function(s7, "exit", our_exit, 0, 0, false, "(exit) exits the program");
  s7_define_function(s7, "add1", add1, 1, 0, false, "(add1 int) adds 1 to int");

  s7_define_variable(s7, "pi", s7_make_real(s7, 3.14159265));

  while (1)                           /* fire up a "repl" */
    {
      fprintf(stdout, "\n> ");        /* prompt for input */
      fgets(buffer, 512, stdin);

      if ((buffer[0] != '\n') || 
	  (strlen(buffer) > 1))
	{                            
	  sprintf(response, "(write %s)", buffer);
	  s7_eval_c_string(s7, response); /* evaluate input and write the result */
	}
    }
}

/* 
 *    /home/bil/cl/ doc7
 *    > pi
 *    3.14159265
 *    > (+ 1 (add1 1))
 *    3
 *    > (exit)
 */

#endif

 /* -------------------------------------------------------------------------------- */

#if 0

/* call a scheme-defined function from C, and get/set scheme variable values in C */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "s7.h"

int main(int argc, char **argv)
{
  s7_scheme *s7;
  s7 = s7_init();

  s7_define_variable(s7, "an-integer", s7_make_integer(s7, 1));

  s7_eval_c_string(s7, "(define (add1 a) (+ a 1))");
  
  fprintf(stderr, "an-integer: %d\n", s7_integer(s7_name_to_value(s7, "an-integer")));

  s7_symbol_set_value(s7, s7_make_symbol(s7, "an-integer"), s7_make_integer(s7, 32));

  fprintf(stderr, "now an-integer: %d\n", s7_integer(s7_name_to_value(s7, "an-integer")));

  fprintf(stderr, "(add1 2): %d\n", 
	  s7_integer(s7_call(s7, 
			     s7_name_to_value(s7, "add1"), 
			     s7_cons(s7, s7_make_integer(s7, 2), s7_NIL(s7)))));
}

/*
 *    /home/bil/cl/ doc7
 *    an-integer: 1
 *    now an-integer: 32
 *    (add1 2): 3
 */

#endif

/* -------------------------------------------------------------------------------- */
