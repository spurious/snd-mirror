
/* -------------------------------- examples --------------------------------
 *
 * a read-eval-print loop using S7: 
 */

#if EX1

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



/* --------------------------------------------------------------------------------
 *
 * define a function with arguments and a returned value, and a variable 
 */

#if EX2

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

  s7_define_variable(s7, "my-pi", s7_make_real(s7, 3.14159265));

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
 *    > my-pi
 *    3.14159265
 *    > (+ 1 (add1 1))
 *    3
 *    > (exit)
 */

#endif



/* --------------------------------------------------------------------------------
 *
 * call a scheme-defined function from C, and get/set scheme variable values in C:
 */

#if EX3

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


/* --------------------------------------------------------------------------------
 *
 * here's an example using C++ and Juce that Rick sent me: 
 */

#if 0

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


/* --------------------------------------------------------------------------------
 *
 * here's an example that loads sndlib using the XEN functions and macros into an s7 repl:
 */

#if EX4

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
  Xen_check_type(Xen_is_string(name), name, 1, "file-exists?", "a string");
  return(C_bool_to_Xen_boolean(mus_file_probe(Xen_string_to_C_string(name))));
}

Xen_wrap_1_arg(g_file_exists_p_w, g_file_exists_p)

static XEN g_delete_file(XEN name)
{
  #define H_delete_file "(delete-file filename): deletes the file"
  Xen_check_type(Xen_is_string(name), name, 1, "delete-file", "a string");
  return(C_bool_to_Xen_boolean(unlink(Xen_string_to_C_string(name))));
}

Xen_wrap_1_arg(g_delete_file_w, g_delete_file)

static XEN g_random(XEN val)
{
  if (Xen_is_integer(val))
    return(C_int_to_Xen_integer(mus_irandom(Xen_integer_to_C_int(val))));
  return(C_double_to_Xen_real(mus_frandom(Xen_real_to_C_double(val))));
}

Xen_wrap_1_arg(g_random_w, g_random)



int main(int argc, char **argv)
{
  char buffer[512];
  char response[1024];


  s7 = s7_init();                     /* initialize the interpreter; s7 is declared in xen.h */
  xen_initialize();                   /* initialize the xen stuff (hooks and the xen s7 FFI used by sndlib) */
  Init_sndlib();                      /* initialize sndlib with all the functions linked into s7 */  

  Xen_eval_C_string("(define load-from-path load)");
  
  Xen_define_procedure("file-exists?", g_file_exists_p_w, 1, 0, 0, H_file_exists_p);
  Xen_define_procedure("delete-file",  g_delete_file_w,   1, 0, 0, H_delete_file);
  Xen_define_procedure("random",       g_random_w,        1, 0, 0, "(random arg): random number between 0 and arg ");
  Xen_eval_C_string("(define (run-safety) 0)");        /* for the run macro and CLM */
  Xen_eval_C_string("(define (1+ x) (+ x 1))");        /* lots of the CLM instruments use this macro */

  s7_define_function(s7, "exit", our_exit, 0, 0, false, "(exit) exits the program");

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
 * run a CLM instrument:
 *
 *   (load "sndlib-ws.scm")
 *   (with-sound () (outa 10 .1))
 *   (load "v.scm")
 *   (with-sound () (fm-violin 0 .1 440 .1))
 */

#endif


/* --------------------------------------------------------------------------------
 *
 * an example of adding a new type and procedure-with-setters:
 */

#if EX5

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



/* --------------------------------------------------------------------------------
 *
 * call scheme with catch from C (debugging...)
 */

#if EX6


#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "s7.h"

int main(int argc, char **argv)
{
  s7_scheme *s7;
  s7_pointer call_no_error, call_with_error;

  s7 = s7_init();

  s7_eval_c_string(s7, "\
(define callback-no-error \
  (lambda (x) \
    (catch #t \
           (lambda () (display \"Hiho!\") -1) \
           (lambda args -2))))\
\
(define callback-with-error \
  (lambda (x) \
    (catch #t \
           (lambda () (display \"Hiho!\") (set! foo bar) -1) \
     (lambda args -2))))");

  call_no_error = s7_name_to_value(s7, "callback-no-error");
  call_with_error = s7_name_to_value(s7, "callback-with-error");

  fprintf(stderr, "%f %f\n",
	  s7_number_to_real(s7, s7_call(s7, 
				    call_no_error,
				    s7_cons(s7, 
					    s7_make_real(s7, 1.0),
					    s7_NIL(s7)))),
	  s7_number_to_real(s7, s7_call(s7, 
				    call_with_error,
				    s7_cons(s7, 
					    s7_make_real(s7, 1.0),
					    s7_NIL(s7)))));
}

#endif
