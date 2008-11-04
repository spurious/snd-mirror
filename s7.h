#ifndef _S7_H
#define _S7_H

#define S7_VERSION "1.1"
#define S7_DATE "25-Oct-08"

/*
 * 25-Oct:    added name arg to s7_make_procedure_with_setter, 
 *              and s7_scheme arg to new_type print func.
 * 1-Oct-08   version 1.0
 */

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


#define s7_Int long long int
#define s7_Int_d "%lld"
/* s7_Int can be any (signed) integer type: "int" is ok */


#ifdef __cplusplus
extern "C" {
#endif

typedef struct s7_scheme s7_scheme;
typedef struct s7_cell *s7_pointer;

typedef s7_pointer (*s7_function)(s7_scheme *sc, s7_pointer a);

s7_pointer s7_F(s7_scheme *sc);
s7_pointer s7_T(s7_scheme *sc);
s7_pointer s7_NIL(s7_scheme *sc);
s7_pointer s7_UNDEFINED(s7_scheme *sc);
s7_pointer s7_EOF_OBJECT(s7_scheme *sc);

bool s7_boolean(s7_scheme *sc, s7_pointer x);
bool s7_is_boolean(s7_scheme *sc, s7_pointer x);
s7_pointer s7_make_boolean(s7_scheme *sc, bool x);

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
double s7_number_to_real(s7_pointer x); /* x can be any kind of number */

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
s7_pointer s7_vector_ref(s7_scheme *sc, s7_pointer vec, int index);
s7_pointer s7_vector_set(s7_scheme *sc, s7_pointer vec, int index, s7_pointer a);
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
		char *(*print)(s7_scheme *sc, void *value), 
		void (*free)(void *value), 
		bool (*equal)(void *val1, void *val2),
		void (*gc_mark)(void *val),
		s7_pointer (*apply)(s7_scheme *sc, s7_pointer obj, s7_pointer args),
		s7_pointer (*set)(s7_scheme *sc, s7_pointer obj, s7_pointer args));

void *s7_object_value(s7_pointer obj);
int s7_object_type(s7_pointer obj);
bool s7_is_object(s7_pointer p);
s7_pointer s7_make_object(s7_scheme *sc, int type, void *value);
char *s7_describe_object(s7_scheme *sc, s7_pointer a);
void s7_free_object(s7_pointer a);
bool s7_equalp_objects(s7_pointer a, s7_pointer b);
void s7_mark_object(s7_pointer a);

void s7_define_variable(s7_scheme *sc, const char *name, s7_pointer value);


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

void s7_for_each_symbol_name(s7_scheme *sc, bool (*symbol_func)(const char *symbol_name, void *data), void *data);
void s7_for_each_symbol(s7_scheme *sc, bool (*symbol_func)(const char *symbol_name, s7_pointer value, void *data), void *data);

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
					 const char *name,
					 s7_pointer (*getter)(s7_scheme *sc, s7_pointer args), 
					 int get_req_args, int get_opt_args,
					 s7_pointer (*setter)(s7_scheme *sc, s7_pointer args),
					 int set_req_args, int set_opt_args,
					 const char *documentation);
bool s7_is_procedure_with_setter(s7_pointer obj);
s7_pointer s7_procedure_with_setter_setter(s7_pointer obj);
s7_pointer s7_procedure_with_setter_getter(s7_pointer obj);

bool s7_is_hash_table(s7_pointer p);
s7_pointer s7_make_hash_table(s7_scheme *sc, int size);
s7_pointer s7_hash_table_ref(s7_scheme *sc, s7_pointer table, const char *name);
s7_pointer s7_hash_table_set(s7_scheme *sc, s7_pointer table, const char *name, s7_pointer value);

#if HAVE_PTHREADS
bool s7_is_thread_variable(s7_pointer obj);
s7_pointer s7_thread_variable_value(s7_scheme *sc, s7_pointer obj);
#endif

#ifdef __cplusplus
}
#endif

#endif


/* -------------------------------- examples --------------------------------
 *
 *  these examples use:
 *
 *    #define s7_Int int
 *    #define s7_Int_d "%d"
 *
 *  in s7.h.   See xen.h and the Snd package for extended examples.
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
  
  fprintf(stderr, "an-integer: %d\n", 
	  s7_integer(s7_name_to_value(s7, "an-integer")));

  s7_symbol_set_value(s7, s7_make_symbol(s7, "an-integer"), s7_make_integer(s7, 32));

  fprintf(stderr, "now an-integer: %d\n", 
	  s7_integer(s7_name_to_value(s7, "an-integer")));

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

#if 0

/* here's an example using C++ and Juce that Rick sent me: */

int main(int argc, const char* argv[]) 
{ 
  initialiseJuce_NonGUI(); 

  s7_scheme *s7=s7_init(); 
  if (!s7) 
    { 
      std::cout <<  "Can't start S7!\n"; 
      return -1; 
    } 

  s7_pointer val; 
  std::string str; 
  while (true) 
    { 
      std::cout << "\ns7> "; 
      std::getline(std::cin, str); 
      val=s7_eval_c_string(s7, str.c_str()); 
      std::cout << s7_object_to_c_string(s7, val); 
    } 

  free(s7); 
  std::cout << "Bye!\n"; 
  return 0; 
} 

#endif


/* -------------------------------------------------------------------------------- */

#if 0

/* here's an example that loads sndlib into an s7 repl */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

/* assume we've configured and built sndlib, so it has created a mus-config.h file */

#include "mus-config.h"
#include "s7.h"
#include "xen.h"
#include "clm.h"
#include "clm2xen.h"

static s7_pointer our_exit(s7_scheme *sc, s7_pointer args)
{
  exit(1);
  return(s7_NIL(sc));
}

/* the next functions are needed for either with-sound or many standard instruments, like fm-violin */
/*   (these are in the xen-style FFI) */

static XEN g_file_exists_p(XEN name)
{
  #define H_file_exists_p "(file-exists? filename): #t if the file exists"
  XEN_ASSERT_TYPE(XEN_STRING_P(name), name, XEN_ONLY_ARG, "file-exists?", "a string");
  return(C_TO_XEN_BOOLEAN(mus_file_probe(XEN_TO_C_STRING(name))));
}

XEN_NARGIFY_1(g_file_exists_p_w, g_file_exists_p)

static XEN g_delete_file(XEN name)
{
  #define H_delete_file "(delete-file filename): deletes the file"
  XEN_ASSERT_TYPE(XEN_STRING_P(name), name, XEN_ONLY_ARG, "delete-file", "a string");
  return(C_TO_XEN_BOOLEAN(unlink(XEN_TO_C_STRING(name))));
}

XEN_NARGIFY_1(g_delete_file_w, g_delete_file)

static XEN g_random(XEN val)
{
  if (XEN_INTEGER_P(val))
    return(C_TO_XEN_INT(mus_irandom(XEN_TO_C_INT(val))));
  return(C_TO_XEN_DOUBLE(mus_frandom(XEN_TO_C_DOUBLE(val))));
}

XEN_NARGIFY_1(g_random_w, g_random)



int main(int argc, char **argv)
{
  char buffer[512];
  char response[1024];


  s7 = s7_init();                     /* initialize the interpreter; s7 is declared in xen.h */
  xen_initialize();                   /* initialize the xen stuff (hooks and the xen s7 FFI) */
  Init_sndlib();                      /* initialize sndlib with all the functions linked into s7 */  

  /* these next lines are for compatibility with Guile (ws.scm has Guile-specific junk) */
  XEN_EVAL_C_STRING("(defmacro use-modules (arg . args) #f)");
  XEN_EVAL_C_STRING("(define (make-soft-port . args) #f)");
  XEN_EVAL_C_STRING("(define (current-module) (current-environment))");
  XEN_EVAL_C_STRING("(define load-from-path load)");
  
  XEN_DEFINE_PROCEDURE("file-exists?", g_file_exists_p_w, 1, 0, 0, H_file_exists_p);
  XEN_DEFINE_PROCEDURE("delete-file",  g_delete_file_w,   1, 0, 0, H_delete_file);
  XEN_DEFINE_PROCEDURE("random",       g_random_w,        1, 0, 0, "(random arg): random number between 0 and arg ");

  /* deal with the ubiquitous run macro */
  XEN_EVAL_C_STRING("(define (run-safety) 0)");
  XEN_EVAL_C_STRING("(defmacro run (thunk) `(,thunk))");
  XEN_EVAL_C_STRING("(define (1+ x) (+ x 1))");

  s7_define_function(s7, "exit", our_exit, 0, 0, false, "(exit) exits the program");
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

/* gcc -o doc7 doc7.c -lm -I. /home/bil/test/sndlib/sndlib.a -lgsl -lgslcblas 
 *
 *   gsl and gslcblas are the Gnu Scientific Library that the configure script found -- those 
 *   may not be necessary on other systems
 *
 * to load ws.scm
 *   (load "s7-optargs.scm")
 *   (load "sndlib-ws.scm")
 *   (with-sound () (outa 10 .1))
 *   (load "v.scm")
 *   (with-sound () (fm-violin 0 .1 440 .1))
 */

#endif


/* -------------------------------------------------------------------------------- */

#if 0

/* an example of adding a new type and procedure-with-setters */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "s7.h"

static s7_pointer our_exit(s7_scheme *sc, s7_pointer args)
{
  exit(1);
  return(s7_NIL(sc));
}


/* define *listener-prompt* in scheme, add two accessors for C get/set */

static const char *listener_prompt(s7_scheme *sc)
{
  return(s7_string(s7_name_to_value(sc, "*listener-prompt*")));
}

static void set_listener_prompt(s7_scheme *sc, const char *new_prompt)
{
  s7_symbol_set_value(sc, s7_make_symbol(sc, "*listener-prompt*"), s7_make_string(sc, new_prompt));
}


/* now add a new type, a struct named "dax" with two fields, a real "x" and a list "data" */
/*   since the data field is an s7 object, we'll need to mark it to protect it from the GC */

typedef struct {
  double x;
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
  if (o)
    s7_mark_object(o->data);
}

static int dax_type_tag = 0;

static s7_pointer make_dax(s7_scheme *sc, s7_pointer args)
{
  dax *o;
  o = (dax *)malloc(sizeof(dax));
  o->x = s7_real(s7_car(args));
  if (s7_cdr(args) != s7_NIL(sc))
    o->data = s7_car(s7_cdr(args));
  else o->data = s7_NIL(sc);
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


int main(int argc, char **argv)
{
  s7_scheme *s7;
  char buffer[512];
  char response[1024];

  s7 = s7_init();
  
  s7_define_function(s7, "exit", our_exit, 0, 0, false, "(exit) exits the program");
  s7_define_variable(s7, "*listener-prompt*", s7_make_string(s7, ">"));

  dax_type_tag = s7_new_type("dax", print_dax, free_dax, equal_dax, mark_dax, NULL, NULL);
  s7_define_function(s7, "make-dax", make_dax, 2, 0, false, "(make-dax x data) makes a new dax");
  s7_define_function(s7, "dax?", is_dax, 1, 0, false, "(dax? anything) returns #t if its argument is a dax object");

  s7_define_variable(s7, "dax-x", 
		     s7_make_procedure_with_setter(s7, "dax-x", dax_x, 1, 0, set_dax_x, 2, 0, "dax x field"));

  s7_define_variable(s7, "dax-data", 
		     s7_make_procedure_with_setter(s7, "dax-data", dax_data, 1, 0, set_dax_data, 2, 0, "dax data field"));

  while (1)
    {
      fprintf(stdout, "\n%s ", listener_prompt(s7));
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
 *    gcc -o doc7 doc7.c s7.o -lm
 *    > *listener-prompt*
 *    ">"
 *    > (set! *listener-prompt* ":")
 *    ":"
 *    : (define obj (make-dax 1.0 (list 1 2 3)))
 *    obj
 *    : obj
 *    #<dax 1.000 (1 2 3)>
 *    : (dax-x obj)
 *    1.0
 *    : (dax-data obj)
 *    (1 2 3)
 *    : (set! (dax-x obj) 123.0)
 *    123.0
 *    : obj
 *    #<dax 123.000 (1 2 3)>
 *    : (dax? obj)
 *    #t
 *    : (exit)
 */
#endif


/* -------------------------------------------------------------------------------- */
