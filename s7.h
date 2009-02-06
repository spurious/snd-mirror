#ifndef S7_H
#define S7_H

#define S7_VERSION "1.12"
#define S7_DATE "29-Jan-09"


typedef long long int s7_Int;
/* This sets the size of integers in scheme and s7.c; s7_Int can be any (signed) integer type: "int" is ok */

typedef double s7_Double;
/* similarly for doubles (reals in scheme) -- only "double" works in C++ */


  /* s7 itself is based on the types and functions in this file, so the first place to look for examples
   *   is s7.c.  There are also a few variations on a REPL at the end of this file.  s7test.scm (in the
   *   Snd tarball) or r5rstest.scm (at the ccrma ftp site) are regression tests for s7 -- they still
   *   turn up a few problems.  More tests are certainly welcome!  Extended examples of s7 usage
   *   are:
   *
   *   Snd: ftp://ccrma-ftp.stanford.edu/pub/Lisp/snd-10.tar.gz (a sound editor)
   *     which includes:
   *       libxm: libxm.tar.gz (Motif and Gtk bindings)
   *       sndlib: sndlib.tar.gz (sound file, audio port, and CLM bindings plus an optimizer (run))
   *
   *   Common Music by Rick Taube: http://camil.music.uiuc.edu/Software/grace/downloads/cm3.tar.gz (composition)
   *     which can use sndlib -- see Snd's grfsnd.html or the cmdist archives for details
   *
   *
   * s7 (scheme) variables:
   *
   *    *load-path*             a list of directory names that "load" searches for scheme input files (initially '())
   *    *vector-print-length*   how many elements of a vector are printed (initially 8)
   *    *features*              a list of symbols describing what is current available (initially '(s7))
   *
   * s7 constants:
   *
   *    most-positive-fixnum
   *    most-negative-fixnum    integer limits (the names come from Common Lisp)
   *    pi                      3.1415...
   *
   * s7 non-standard functions:
   *
   *    load-verbose            if argument is #t, "load" prints out the name of the file it is loading
   *    gc-verbose              if argument is #t, the garbage collector reports on its activities
   *    provided?               checks the *features* list for a symbol
   *    provided                adds a symbol to the *features* list
   *    port-line-number        current line during loading
   *    port-filename           current file name during loading
   *    gc                      calls the GC, if its argument is #f, the GC is turned off
   *    quit                    exits s7
   *    call-with-exit          just like call/cc but no jump back into a context
   *    continuation?           #t if its argument is a continuation (as opposed to an ordinary procedure)
   *    backtrace               prints the "evaluation history"
   *    clear-backtrace         clears the evaluation history
   *    set-backtrace-length    sets how many entries are saved in the evaluation history
   *    backtracing             if argument is #t, error messages include a backtrace printout
   *    procedure-documentation doc string associated with a procedure
   *    procedure-arity         a list describing the arglist of a function: '(required-args optional-args rest-arg)
   *    procedure-source        returns the source (a list) of a procedure
   *    help                    tries to find a help string associated with its argument
   *
   * and various others mentioned at the start of s7.c -- nearly every Scheme implementation includes
   * stuff like logior, sinh, read-line, format, define*, etc.  See also the start of s7.c for choices
   * such as case-insensitive symbol names, multiprecision arithmetic, initial heap and stack size, etc.
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


#ifdef __cplusplus
extern "C" {
#endif

typedef struct s7_scheme s7_scheme;
typedef struct s7_cell *s7_pointer;

s7_scheme *s7_init(void);

  /* s7_scheme is our interpreter (or each thread's interpreter),
   * s7_pointer is a scheme object of any (scheme) type
   *
   * s7_init creates the interpreter.
   */

typedef s7_pointer (*s7_function)(s7_scheme *sc, s7_pointer args);   /* obj = func(s7, args) -- args is a list of arguments */


s7_pointer s7_F(s7_scheme *sc);                                      /* #f */
s7_pointer s7_T(s7_scheme *sc);                                      /* #t */
s7_pointer s7_NIL(s7_scheme *sc);                                    /* () */
s7_pointer s7_UNDEFINED(s7_scheme *sc);                              /* #<undefined> */
s7_pointer s7_UNSPECIFIED(s7_scheme *sc);                            /* #<unspecified> */
bool s7_is_unspecified(s7_scheme *sc, s7_pointer val);               /*     returns true if val is #<unspecified> */
s7_pointer s7_EOF_OBJECT(s7_scheme *sc);                             /* #<eof> */

  /* these are the scheme constants; they do not change in value during a run, and
   *   are the same across all threads, so they can be safely assigned to C global variables if desired. 
   */


s7_pointer s7_eval_c_string(s7_scheme *sc, const char *str);         /* (eval-string str) */
s7_pointer s7_object_to_string(s7_scheme *sc, s7_pointer arg);       /* (object->string obj) */
char *s7_object_to_c_string(s7_scheme *sc, s7_pointer obj);          /* same as object->string but returns a C char* directly */
                                                                     /*   the returned value should be freed by the caller */

s7_pointer s7_load(s7_scheme *sc, const char *file);                 /* (load file) */
s7_pointer s7_load_path(s7_scheme *sc);                              /* *load-path* */
s7_pointer s7_add_to_load_path(s7_scheme *sc, const char *dir);      /* (set! *load-path* (cons dir *load-path*)) */

  /* the load path is a list of directories to search if load can't find the file passed as its argument.
   */

void s7_provide(s7_scheme *sc, const char *feature);                 /* add feature (as a symbol) to the *features* list */


s7_pointer s7_error(s7_scheme *sc, s7_pointer type, s7_pointer info);
s7_pointer s7_error_and_exit(s7_scheme *sc, s7_pointer type, s7_pointer info);
s7_pointer s7_wrong_type_arg_error(s7_scheme *sc, const char *caller, int arg_n, s7_pointer arg, const char *descr);
  /* set arg_n to 0 to indicate that caller takes only one argument (so the argument number need not be reported */
s7_pointer s7_out_of_range_error(s7_scheme *sc, const char *caller, int arg_n, s7_pointer arg, const char *descr);
s7_pointer s7_wrong_number_of_args_error(s7_scheme *sc, const char *caller, s7_pointer args);
void s7_set_error_exiter(s7_scheme *sc, void (*error_exiter)(void));

  /* these are equivalent to (error ...) in scheme
   *   the first argument to s7_error is a symbol that can be caught (via (catch tag ...))
   *   the rest of the arguments are passed to the error handler (if in catch) 
   *   or printed out (in the default case).  If the first element of the list
   *   of args ("info") is a string, the default error handler treats it as
   *   a format control string, and passes it to format with the rest of the
   *   info list as the format function arguments.
   *
   *   s7_error_and_exit jumps to some arbitrary place provided by s7_set_error_exiter
   *   s7_wrong_type_arg_error is equivalent to s7_error with a type of 'wrong-type-arg
   *   and similarly s7_out_of_range_error with type 'out-of-range.
   *
   * catch in scheme is taken from Guile:
   *
   *  (catch tag thunk handler)
   *
   *  evaluates 'thunk'.  If an error occurs, and the type matches 'tag' (or if 'tag' is #t),
   *  the handler is called, passing it the arguments (including the type) passed to the
   *  error function.  If no handler is found, the default error handler is called,
   *  normally printing the error arguments and a backtrace to current-error-port.
   */


int s7_gc_protect(s7_scheme *sc, s7_pointer x);
void s7_gc_unprotect(s7_scheme *sc, s7_pointer x);
void s7_gc_unprotect_at(s7_scheme *sc, int loc);
s7_pointer s7_gc_protected_at(s7_scheme *sc, int loc);

s7_pointer s7_local_gc_protect(s7_pointer p);
s7_pointer s7_local_gc_unprotect(s7_pointer p);

s7_pointer s7_gc_on(s7_scheme *sc, bool on);

  /* any s7_pointer object held in C (as a local variable for example) needs to be
   *   protected from garbage collection if there is any chance the GC may run without
   *   an existing scheme-level reference to that object.  s7_gc_protect is intended
   *   for longer term protection (a hook function, for example).  It places the
   *   object in a vector that the GC always checks, returning the object's location
   *   in that table.  s7_gc_unprotect and s7_gc_unprotect_at unprotect the object
   *   (remove it from the vector) -- s7_gc_unprotect_at uses the location passed
   *   to it, whereas s7_gc_unprotect scans the vector to find the object.  
   *   s7_gc_protected_at returns the object at the given location.
   *
   * for short-term protection (a local variable held across a function call that
   *   might trigger a GC, for example), use s7_local_gc_protect and s7_local_gc_unprotect.
   * 
   * You can turn the GC on and off via s7_gc_on.
   *
   * There is a built-in lag between the creation of a new object and its first possible GC
   *    (the lag time is set indirectly by GC_TEMPS_SIZE in s7.c), so you don't need to worry about
   *    very short term temps such as the arguments to s7_cons in:
   *
   *    s7_cons(s7, s7_make_real(s7, 3.14), 
   *                s7_cons(s7, s7_make_integer(s7, 123), 
   *                            s7_NIL(s7)));
   */


bool s7_is_eq(s7_pointer a, s7_pointer b);                                   /* (eq? a b) */
bool s7_is_eqv(s7_pointer a, s7_pointer b);                                  /* (eqv? a b) */
bool s7_is_equal(s7_pointer a, s7_pointer b);                                /* (equal? a b) */


bool s7_is_boolean(s7_scheme *sc, s7_pointer x);                             /* (boolean? x) */
bool s7_boolean(s7_scheme *sc, s7_pointer x);                                /* scheme boolean -> C bool */
s7_pointer s7_make_boolean(s7_scheme *sc, bool x);                           /* C bool -> scheme boolean */

  /* for each scheme type (boolean, integer, string, etc), there are three
   *   functions: s7_<type>(...), s7_make_<type>(...), and s7_is_<type>(...):
   *
   *   s7_boolean(s7, obj) returns the C bool corresponding to the value of 'obj' (#f -> false)
   *   s7_make_boolean(s7, false|true) returns the s7 boolean corresponding to the C bool argument (false -> #f)
   *   s7_is_boolean(s7, obj) returns true if 'obj' has a boolean value (#f or #t).
   */


bool s7_is_pair(s7_pointer p);                                               /* (pair? p) */
s7_pointer s7_cons(s7_scheme *sc, s7_pointer a, s7_pointer b);               /* (cons a b) */
s7_pointer s7_car(s7_pointer p);                                             /* (car p) */
s7_pointer s7_cdr(s7_pointer p);                                             /* (cdr p) */
s7_pointer s7_set_car(s7_pointer p, s7_pointer q);                           /* (set-car! p q) */
s7_pointer s7_set_cdr(s7_pointer p, s7_pointer q);                           /* (set-cdr! p q) */


bool s7_is_list(s7_scheme *sc, s7_pointer p);                                /* (list? p) */
int s7_list_length(s7_scheme *sc, s7_pointer a);                             /* (length a) */
s7_pointer s7_reverse(s7_scheme *sc, s7_pointer a);                          /* (reverse a) */
s7_pointer s7_append(s7_scheme *sc, s7_pointer a, s7_pointer b);             /* (append a b) */
s7_pointer s7_list_ref(s7_scheme *sc, s7_pointer lst, int num);              /* (list-ref lst num) */
s7_pointer s7_list_set(s7_scheme *sc, s7_pointer lst, int num, s7_pointer val); /* (list-set! lst num val) */
s7_pointer s7_assoc(s7_scheme *sc, s7_pointer sym, s7_pointer lst);          /* (assoc sym lst) */
s7_pointer s7_member(s7_scheme *sc, s7_pointer sym, s7_pointer lst);         /* (member sym lst) */
s7_pointer s7_remv(s7_scheme *sc, s7_pointer a, s7_pointer obj);             /* (remv a obj) */


bool s7_is_string(s7_pointer p);                                             /* (string? p) */
const char *s7_string(s7_pointer p);                                         /* scheme string -> C string (do not free the string) */
s7_pointer s7_make_string(s7_scheme *sc, const char *str);                   /* C string -> scheme string (str is copied) */
s7_pointer s7_make_string_with_length(s7_scheme *sc, const char *str, int len);  /* same as s7_make_string, but provides strlen */

bool s7_is_character(s7_pointer p);                                          /* (character? p) */
char s7_character(s7_pointer p);                                             /* scheme character -> C char */
s7_pointer s7_make_character(s7_scheme *sc, int c);                          /* C char (as int) -> scheme character */


bool s7_is_number(s7_pointer p);                                             /* (number? p) */
bool s7_is_exact(s7_pointer p);                                              /* (exact? p) */
bool s7_is_inexact(s7_pointer p);                                            /* (inexact? p) */

bool s7_is_integer(s7_pointer p);                                            /* (integer? p) */
s7_Int s7_integer(s7_pointer p);                                             /* scheme integer -> C int (long long int probably) */
s7_pointer s7_make_integer(s7_scheme *sc, s7_Int num);                       /* C long long int -> scheme integer */

bool s7_is_real(s7_pointer p);                                               /* (real? p) */
s7_Double s7_real(s7_pointer p);                                             /* scheme real -> C double */
s7_pointer s7_make_real(s7_scheme *sc, s7_Double num);                       /* C double -> scheme real */
s7_Double s7_number_to_real(s7_pointer x);                                   /* x can be any kind of number */

bool s7_is_ulong(s7_pointer arg);                                            /* returns true if arg is an unsigned long */
unsigned long s7_ulong(s7_pointer p);                                        /* scheme unsigned long -> C */
s7_pointer s7_make_ulong(s7_scheme *sc, unsigned long n);                    /* C unsigned lonog -> scheme */
bool s7_is_ulong_long(s7_pointer arg);                                       /* returns true if arg is an unsigned long long */
unsigned long long s7_ulong_long(s7_pointer p);                              /* scheme unsigned long long -> C */
s7_pointer s7_make_ulong_long(s7_scheme *sc, unsigned long long n);          /* C unsigned long long -> scheme */
  /* the ulong stuff is intended for passing uninterpreted C pointers through scheme and back to C */

bool s7_is_rational(s7_pointer arg);                                        /* (rational? arg) -- integer or ratio */
bool s7_is_ratio(s7_pointer arg);                                           /* true if arg is a ratio, not an integer */
s7_pointer s7_make_ratio(s7_scheme *sc, s7_Int a, s7_Int b);                /* returns the scheme object a/b */
s7_pointer s7_rationalize(s7_scheme *sc, s7_Double x, s7_Double error);     /* (rationalize x error) */
s7_Int s7_numerator(s7_pointer x);                                          /* (numerator x) */
s7_Int s7_denominator(s7_pointer x);                                        /* (denominator x) */

bool s7_is_complex(s7_pointer arg);                                         /* (complex? arg) */
s7_pointer s7_make_complex(s7_scheme *sc, s7_Double a, s7_Double b);        /* returns the scheme object a+bi */
s7_Double s7_real_part(s7_pointer z);                                       /* (real-part z) */
s7_Double s7_imag_part(s7_pointer z);                                       /* (imag-part z) */
char *s7_number_to_string(s7_scheme *sc, s7_pointer obj, int radix);        /* (number->string obj radix) */


bool s7_is_vector(s7_pointer p);                                            /* (vector? p) */
void s7_vector_fill(s7_scheme *sc, s7_pointer vec, s7_pointer obj);         /* (vector-fill! vec obj) */
s7_pointer s7_vector_ref(s7_scheme *sc, s7_pointer vec, int index);         /* (vector-ref vec index) */
s7_pointer s7_vector_set(s7_scheme *sc, s7_pointer vec, int index, s7_pointer a);  /* (vector-set! vec index a) */
s7_pointer s7_make_vector(s7_scheme *sc, int len);                          /* (make-vector len) */
s7_pointer s7_make_and_fill_vector(s7_scheme *sc, int len, s7_pointer fill);/* (make-vector len fill) */
int s7_vector_length(s7_pointer vec);                                       /* (vector-length vec) */
s7_pointer s7_vector_to_list(s7_scheme *sc, s7_pointer vect);               /* (vector->list vect) */
s7_pointer *s7_vector_elements(s7_pointer vec);                             /* a pointer to the array of s7_pointers */


bool s7_is_hash_table(s7_pointer p);                                        /* (hash-table? p) */
s7_pointer s7_make_hash_table(s7_scheme *sc, int size);                     /* (make-hash-table size) */
s7_pointer s7_hash_table_ref(s7_scheme *sc, s7_pointer table, const char *name);   
                                                                            /* (hash-table-ref table name) */
s7_pointer s7_hash_table_set(s7_scheme *sc, s7_pointer table, const char *name, s7_pointer value);  
                                                                            /* (hash-table-set! table name value) */
  /* a hash-table is a vector of alists '((symbol value)), so to iterate over a hash-table
   *   use vector-ref and cdr down the lists.  An entry defaults to '() -- see "apropos" below
   */

bool s7_is_input_port(s7_scheme *sc, s7_pointer p);                         /* (input-port? p) */
bool s7_is_output_port(s7_scheme *sc, s7_pointer p);                        /* (output-port? p) */
s7_pointer s7_current_input_port(s7_scheme *sc);                            /* (current-input-port) */
s7_pointer s7_current_output_port(s7_scheme *sc);                           /* (current-output-port) */
s7_pointer s7_set_current_output_port(s7_scheme *sc, s7_pointer p);         /* (set-current-output-port) */
s7_pointer s7_current_error_port(s7_scheme *sc);                            /* (current-error-port) */
s7_pointer s7_set_current_error_port(s7_scheme *sc, s7_pointer port);       /* (set-current-error-port port) */
void s7_close_input_port(s7_scheme *sc, s7_pointer p);                      /* (close-input-port p) */
void s7_close_output_port(s7_scheme *sc, s7_pointer p);                     /* (close-output-port p) */
s7_pointer s7_open_input_file(s7_scheme *sc, const char *name, const char *mode);  
                                                                            /* (open-input-file name mode) */
s7_pointer s7_open_output_file(s7_scheme *sc, const char *name, const char *mode); 
                                                                            /* (open-output-file name mode) */
  /* mode here is an optional C style flag, "ra" for "read-alter", etc ("r" is the input default, "w" is the output default) */
s7_pointer s7_open_input_string(s7_scheme *sc, const char *input_string);  
                                                                            /* (open-input-string str) */
s7_pointer s7_open_output_string(s7_scheme *sc);                            /* (open-output-string) */
const char *s7_get_output_string(s7_scheme *sc, s7_pointer out_port);       /* (get-output-string port) -- current contents of output string */
  /*    don't free the string */
s7_pointer s7_open_output_function(s7_scheme *sc, void (*function)(s7_scheme *sc, char c, s7_pointer port));                                                                            
char s7_read_char(s7_scheme *sc, s7_pointer port);                          /* (read-char port) */
char s7_peek_char(s7_scheme *sc, s7_pointer port);                          /* (peek-char port) */
s7_pointer s7_read(s7_scheme *sc, s7_pointer port);                         /* (read port) */
void s7_newline(s7_scheme *sc, s7_pointer port);                            /* (newline port) */
void s7_write_char(s7_scheme *sc, char c, s7_pointer port);                 /* (write-char c port) */
void s7_write(s7_scheme *sc, s7_pointer obj, s7_pointer port);              /* (write obj port) */
void s7_display(s7_scheme *sc, s7_pointer obj, s7_pointer port);            /* (display obj port) */


bool s7_is_procedure(s7_pointer x);                                         /* (procedure? x) */
s7_pointer s7_procedure_source(s7_scheme *sc, s7_pointer p);                /* (procedure-source x) if it can be found */
s7_pointer s7_procedure_environment(s7_pointer p);
const char *s7_procedure_documentation(s7_scheme *sc, s7_pointer p);        /* (procedure-documentation x) if any (don't free the string) */
s7_pointer s7_procedure_arity(s7_scheme *sc, s7_pointer x);                 /* (procedure-arity x) -- returns a list (required optional rest?) */
bool s7_is_continuation(s7_pointer p);                                      /* (continuation? p) */


bool s7_is_symbol(s7_pointer p);                                            /* (symbol? p) */
const char *s7_symbol_name(s7_pointer p);                                   /* (symbol->string p) -- don't free the string */
s7_pointer s7_make_symbol(s7_scheme *sc, const char *name);                 /* (string->symbol name) */
s7_pointer s7_gensym(s7_scheme *sc, const char *prefix);                    /* (gensym prefix) */

bool s7_is_keyword(s7_pointer obj);                                         /* (keyword? obj) */
s7_pointer s7_make_keyword(s7_scheme *sc, const char *key);                 /* (make-keyword key) */
bool s7_keyword_eq_p(s7_pointer obj1, s7_pointer obj2);                     /* (eq? obj1 obj2) -- objs are keys */


s7_pointer s7_global_environment(s7_scheme *sc);                            /* (global-environment) */
s7_pointer s7_current_environment(s7_scheme *sc);                           /* (current-environment) */

  /* each environment is a list of the current frames (alists of symbols and values)
   *   and the global (top-level) definitions, a vector of alists (a hash-table).
   * Here is an example of "apropos" that accesses both kinds of environment:
   *
        (define (apropos name)
          ;; (apropos "name") prints out a list of all symbols whose name includes "name" as a substring

          (define (substring? subs s) ; from larceny
            (let* ((start 0)
	           (ls (string-length s))
	           (lu (string-length subs))
	           (limit (- ls lu)))
              (let loop ((i start))
	        (cond ((> i limit) #f)
	              ((do ((j i (+ j 1))
	        	    (k 0 (+ k 1)))
	        	   ((or (= k lu)
	        		(not (char=? (string-ref subs k) (string-ref s j))))
	        	    (= k lu))) i)
	              (else (loop (+ i 1)))))))

          (define (apropos-1 alist)
            (for-each
             (lambda (binding)
               (if (substring? name (symbol->string (car binding)))
	           (format (current-output-port) "~A: ~A~%" 
	        	   (car binding) 
	        	   (if (procedure? (cdr binding))
	        	       (procedure-documentation (cdr binding))
	        	       (cdr binding)))))
             alist))

          (for-each
           (lambda (frame)
             (if (vector? frame) ; the global environment
	         (let ((len (vector-length frame)))
	           (do ((i 0 (+ i 1)))
	               ((= i len))
	             (apropos-1 (vector-ref frame i))))
	         (apropos-1 frame)))
           (current-environment)))
   *
   */

s7_pointer s7_name_to_value(s7_scheme *sc, const char *name);
s7_pointer s7_symbol_value(s7_scheme *sc, s7_pointer sym);
s7_pointer s7_symbol_set_value(s7_scheme *sc, s7_pointer sym, s7_pointer val);
s7_pointer s7_symbol_local_value(s7_scheme *sc, s7_pointer sym, s7_pointer local_env);
void s7_for_each_symbol_name(s7_scheme *sc, bool (*symbol_func)(const char *symbol_name, void *data), void *data);
void s7_for_each_symbol(s7_scheme *sc, bool (*symbol_func)(const char *symbol_name, s7_pointer value, void *data), void *data);
  
  /* these access the current environment and symbol table, providing
   *   a symbol's current binding (s7_name_to_value takes the symbol name as a char*,
   *   s7_symbol_value takes the symbol itself, s7_symbol_set_value changes the
   *   current binding, and s7_symbol_local_value uses the environment passed
   *   as its third argument).
   *
   * To iterate over the complete symbol table, use s7_for_each_symbol_name,
   *   and s7_for_each_symbol.  The latter calls the 'symbol_func' on each
   *   symbol, passing the symbol name, its current binding, and the uninterpreted
   *   'data' pointer.  s7_for_each_symbol_name is similar, but does not include
   *   the current binding.
   */


void s7_define(s7_scheme *sc, s7_pointer env, s7_pointer symbol, s7_pointer value);
void s7_define_variable(s7_scheme *sc, const char *name, s7_pointer value);
void s7_define_constant(s7_scheme *sc, const char *name, s7_pointer value);

  /* These three functions add a symbol and its binding to either the top-level environment
   *    or the 'env' passed as the second argument to s7_define.
   *
   *    s7_define_variable(sc, "*features*", sc->NIL);
   *
   * in s7.c is equivalent to the top level form
   *
   *    (define *features* '())
   *
   * s7_define_variable is simply s7_define with string->symbol and the global environment.
   * s7_define_constant is s7_define_variable but makes its "definee" immutable.
   */

bool s7_is_function(s7_pointer p); 
s7_pointer s7_make_function(s7_scheme *sc, const char *name, s7_function fnc, int required_args, int optional_args, bool rest_arg, const char *doc);
void s7_define_function(s7_scheme *sc, const char *name, s7_function fnc, int required_args, int optional_args, bool rest_arg, const char *doc);
s7_pointer s7_apply_function(s7_scheme *sc, s7_pointer fnc, s7_pointer args);

  /* s7_make_function creates a scheme function object from the s7_function 'fnc'.
   *   Its name (for s7_describe_object) is 'name', it requires 'required_args' arguments,
   *   can accept 'optional_args' other arguments, and if 'rest_arg' is true, it accepts
   *   a "rest" argument (a list of all the trailing arguments).  The function's documentation
   *   is 'doc'.
   *
   * s7_define_function is the same as s7_make_function, but it also adds 'name' (as a symbol) to the
   *   global (top-level) environment, with the function as its value.  For example, the scheme
   *   function 'car' is essentially:
   *
   *     s7_pointer g_car(s7_scheme *sc, s7_pointer args) 
   *       {return(s7_car(sc, s7_car(sc, args)));}
   *
   *   then bound to the name "car":
   *
   *     s7_define_function(sc, "car", g_car, 1, 0, false, "(car obj)");
   *                                          one required arg, no optional arg, no "rest" arg
   *
   * s7_is_function returns true if its argument is a function defined in this manner.
   * s7_apply_function applies the function (the result of s7_make_function) to the arguments.
   */

s7_pointer s7_call(s7_scheme *sc, s7_pointer func, s7_pointer args);
  
  /* s7_call takes a scheme function (e.g. g_car above), and applies it to 'args' (a list of arguments)
   *   returning the result.
   *   
   *   s7_integer(s7_call(s7, g_car, s7_cons(s7, s7_make_integer(sc, 123), s7_NIL(s7))));
   *  
   *   returns 123.
   */

s7_pointer s7_make_closure(s7_scheme *sc, s7_pointer c, s7_pointer e);
s7_pointer s7_make_closure_star(s7_scheme *sc, s7_pointer c, s7_pointer e);

  /* these create closures with 'c' as the function, and 'e' as the environment (arglist)
   *   closure_star corresponds to define* and lambda*
   *
   * In s7, (define* (name . args) body) or (define name (lambda* args body))
   *   define a function that takes optional (keyword) named arguments.
   *   The keywords :key and :optional are ok, but they are ignored --
   *   they exist to be compatible with other define* implementations.  
   *   The "args" is a list that can contain either names (normal arguments),
   *   or lists of the form (name default-value), in any order.  When called,
   *   the names are bound to their default values (or #f), then the function's
   *   current arglist is scanned.  Any name that occurs as a keyword (":name")
   *   precedes that argument's new value.  Otherwise, as values occur, they
   *   are plugged into the environment based on their position in the arglist
   *   (as normal for a function).  So,
   *   
   *   (define* (hi a (b 32) (c "hi")) (list a b c))
   *
   *   is equivalent to other implementations (define* (hi a :key (b 32) ...))
   *   or (define* (hi a :optional (b 32) ...)) -- these args are all
   *   "optional-key" args in CLM jargon.
   *
   *   (hi 1) -> '(1 32 "hi")
   *   (hi :b 2 :a 3) -> '(3 2 "hi")
   *   (hi 3 2 1) -> '(3 2 1)
   *
   *   and so on.  :rest causes its argument to be bound to the rest
   *   of the arguments at that point.
   *
   *   The point of s7_make_closure_star is that you can create
   *   your own define* funcs in C -- I'll make a simpler way to do this
   *   eventually.
   */

bool s7_is_procedure_with_setter(s7_pointer obj);
s7_pointer s7_make_procedure_with_setter(s7_scheme *sc, 
					 const char *name,
					 s7_pointer (*getter)(s7_scheme *sc, s7_pointer args), 
					 int get_req_args, int get_opt_args,
					 s7_pointer (*setter)(s7_scheme *sc, s7_pointer args),
					 int set_req_args, int set_opt_args,
					 const char *documentation);
s7_pointer s7_procedure_with_setter_setter(s7_pointer obj);
s7_pointer s7_procedure_with_setter_getter(s7_pointer obj);

  /* a procedure_with_setter is an object that can be called either as a normal function,
   *   or as the object of set!  There is an extended example below.  The 'getter'
   *   is the normal (outside set!) function (normally a struct field reader of some sort),
   *   and the 'setter' is the set! function (a field writer in most cases).
   *
   *   In the example below we have dax-x as the procedure-with-setter,
   *     (dac-x obj)              returns the x field of obj
   *     (set! (dac-x obj) value) sets that field to value
   *   
   * In the set! case, the new value is the last of the args passed to the setter.
   * s7_make_procedure_with_setter is equivalent to s7_make_function, so to bind it
   *   to some name, you need to call s7_define_variable.
   */


int s7_new_type(const char *name, 
		char *(*print)(s7_scheme *sc, void *value), 
		void (*free)(void *value), 
		bool (*equal)(void *val1, void *val2),
		void (*gc_mark)(void *val),
		s7_pointer (*apply)(s7_scheme *sc, s7_pointer obj, s7_pointer args),
		s7_pointer (*set)(s7_scheme *sc, s7_pointer obj, s7_pointer args));

bool s7_is_object(s7_pointer p);
int s7_object_type(s7_pointer obj);
void *s7_object_value(s7_pointer obj);
s7_pointer s7_make_object(s7_scheme *sc, int type, void *value);
void s7_mark_object(s7_pointer p);
  
  /* These functions create a new scheme object type.  There is a simple example below.
   *
   * s7_new_type describes the type for scheme:
   *   name:    the name used by describe-object
   *   print:   the function called whenever s7 is asked to display a value with this type
   *   free:    the function called when an object of this type is about to be garbage collected
   *   equal:   compare two objects of this type; (equal? obj1 obj2)
   *   gc_mark: called during the GC mark pass -- you should call s7_mark_object
   *            on any embedded s7_pointer associated with the object.
   *   apply:   a function that is called whenever an object of this type
   *            occurs in the function position (at the car of a list; the rest of the list
   *            is passed to the apply function as the arguments).
   *   set:     a function that is called whenever an object of this type occurs as
   *            the target of a generalized set!
   *
   *   s7_new_type returns an integer that identifies the new type for the other functions.
   *
   * s7_is_object returns true if 'p' holds a value of a type created by s7_new_type.
   * s7_object_type returns the object's type
   * s7_object_value returns the value bound to that object (the void *value of s7_make_object)
   * s7_make_object creates a new scheme entity of the given type with the given (uninterpreted) value
   * s7_mark_object marks any scheme object as in-use (use this in the gc_mark function to mark
   *    any embedded s7_pointer variables).
   */


#if HAVE_PTHREADS
  bool s7_is_thread_variable(s7_pointer obj);
  s7_pointer s7_thread_variable_value(s7_scheme *sc, s7_pointer obj);
#endif

/* Threads in s7 share the heap and symbol table, but have their own local environment, stack,
 *   and evaluator locals.  The two functions above refer to thread-local variables
 *   known as "keys" in pthreads.  s7_is_thread_variable returns true if its argument
 *   is associated with a key, and s7_thread_variable_value returns the current thread's
 *   value for that key (a scheme object).  More of the multithreading functions could
 *   be brought out to C if there's interest.  define HAVE_PTHREADS to get the multithread
 *   support.  In scheme,
 *
 *   (thread? obj)             returns #t if 'obj' is a thread object
 *   (make-thread thunk)       creates a thread that will evaluate 'thunk' (a function with no args)
 *   (join-thread thread)      causes the current thread to wait for 'thread' to finish
 *
 *   (lock? obj)               returns #t if 'obj' is a lock (a mutex in pthread jargon)
 *   (make-lock)               creates a new lock and initializes it (pthread_mutex_init)
 *   (grab-lock lock)          pthread_mutex_lock 
 *   (release-lock lock)       pthread_mutex_unlock
 *
 *   (thread-variable? obj)    returns #t if 'obj' is a key
 *   (make-thread-variable)    returns a new key (pthread_key_create)
 *     thereafter (obj) returns the current thread's value for that key (pthread_getspecific), and
 *                (set! (obj) value) sets its value (pthread_setspecific)
 *
 * see with-threaded-sound in ws.scm or sndlib-ws.scm for an example.
 */

#if WITH_GMP
  #include <gmp.h>
  #include <mpfr.h>
  #include <mpc.h>

  bool s7_is_bignum(s7_pointer obj);
  mpfr_t *s7_big_real(s7_pointer x);
  mpz_t *s7_big_integer(s7_pointer x);
  mpq_t *s7_big_ratio(s7_pointer x);
  mpc_t *s7_big_complex(s7_pointer x);
  s7_pointer s7_make_big_integer(s7_scheme *sc, mpz_t *val);
  s7_pointer s7_make_big_ratio(s7_scheme *sc, mpq_t *val);
  s7_pointer s7_make_big_real(s7_scheme *sc, mpfr_t *val);
  s7_pointer s7_make_big_complex(s7_scheme *sc, mpc_t *val);
#endif


#ifdef __cplusplus
}
#endif

#endif


/* -------------------------------- examples --------------------------------
 *
 * These are simple, but complete programs illustrating various ways to use s7:
 *
 *   read-eval-print loop
 *   define a function with arguments and a returned value, and define a variable 
 *   call a scheme-defined function from C, and get/set scheme variable values in C
 *   use s7 in C++ and Juce
 *   load sndlib using the XEN functions and macros into a REPL
 *   add a new type and procedure-with-setters
 *   redirect display/write output to a C procedure
 *   extend a built-in operator ("+" in this case)
 */


/*--------------------------------------------------------------------------------
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



/* --------------------------------------------------------------------------------
 *
 * define a function with arguments and a returned value, and a variable 
 */

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

#if 0

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

#if 0

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
  xen_initialize();                   /* initialize the xen stuff (hooks and the xen s7 FFI used by sndlib) */
  Init_sndlib();                      /* initialize sndlib with all the functions linked into s7 */  

  /* these next lines are for compatibility with Guile */
  XEN_EVAL_C_STRING("(defmacro use-modules (arg . args) #f)");
  XEN_EVAL_C_STRING("(define (make-soft-port . args) #f)");
  XEN_EVAL_C_STRING("(define (current-module) (current-environment))");
  XEN_EVAL_C_STRING("(define load-from-path load)");
  
  XEN_DEFINE_PROCEDURE("file-exists?", g_file_exists_p_w, 1, 0, 0, H_file_exists_p);
  XEN_DEFINE_PROCEDURE("delete-file",  g_delete_file_w,   1, 0, 0, H_delete_file);
  XEN_DEFINE_PROCEDURE("random",       g_random_w,        1, 0, 0, "(random arg): random number between 0 and arg ");
  XEN_EVAL_C_STRING("(define (run-safety) 0)");        /* for the run macro and CLM */
  XEN_EVAL_C_STRING("(define (1+ x) (+ x 1))");        /* lots of the CLM instruments use this macro */

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
 * an example of redirecting output to a C procedure:
 */

#if 0
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "s7.h"

static void my_print(s7_scheme *sc, char c, s7_pointer port)
{
  fprintf(stderr, "[%c] ", c);
}

int main(int argc, char **argv)
{
  s7_scheme *s7;
  char buffer[512];
  char response[1024];

  s7 = s7_init();  

  s7_set_current_output_port(s7, s7_open_output_function(s7, my_print));

  while (1) 
    {
      fprintf(stdout, "\n> ");
      fgets(buffer, 512, stdin);

      if ((buffer[0] != '\n') || 
	  (strlen(buffer) > 1))
	{                            
	  sprintf(response, "(write %s)", buffer);
	  s7_eval_c_string(s7, response);
	}
    }
}

/* 
 *   gcc -c s7.c -I.
 *   gcc -o doc7 doc7.c s7.o -lm -I.
 *
 *    doc7
 *    > (+ 1 2)
 *    [3]
 *    > (display "hiho")
 *    [h] [i] [h] [o] [#] [<] [u] [n] [s] [p] [e] [c] [i] [f] [i] [e] [d] [>] 
 *    > (define (add1 x) (+ 1 x))
 *    [a] [d] [d] [1] 
 *    > (add1 123)
 *    [1] [2] [4] 
 */

#endif



/* --------------------------------------------------------------------------------
 *
 * an example of extending a built-in operator ("+" in this case):
 */

#if 0

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "s7.h"

static s7_pointer old_add;           /* the original "+" function for non-string cases */
static s7_pointer old_string_append; /* same, for "string-append" */

static s7_pointer our_add(s7_scheme *sc, s7_pointer args)
{
  /* this will replace the built-in "+" operator, extending it to include strings:
   *   (+ "hi" "ho") -> "hiho" and  (+ 3 4) -> 7
   */
  if ((s7_is_pair(args)) &&
      (s7_is_string(s7_car(args))))
    return(s7_apply_function(sc, old_string_append, args));

  return(s7_apply_function(sc, old_add, args));
}

int main(int argc, char **argv)
{
  s7_scheme *s7;
  char buffer[512];
  char response[1024];

  s7 = s7_init();
  /* get built-in + and string-append */
  old_add = s7_name_to_value(s7, "+");      
  old_string_append = s7_name_to_value(s7, "string-append");
  /* redefine "+" */
  s7_define_function(s7, "+", our_add, 0, 0, true, "(+ ...) adds or appends its arguments");

  while (1)
    {
      fprintf(stdout, "\n> ");
      fgets(buffer, 512, stdin);
      if ((buffer[0] != '\n') || 
	  (strlen(buffer) > 1))
	{                            
	  sprintf(response, "(write %s)", buffer);
	  s7_eval_c_string(s7, response);
	}
    }
}

/* 
 *    > (+ 1 2)
 *    3
 *    > (+ "hi" "ho")
 *    "hiho"
 */

#endif



/* --------------------------------------------------------------------------------
 * 
 *        s7 changes
 *
 * 29-Jan:    s7_is_bignum and friends.
 * 26-Jan:    added s7_scheme arg to s7_vector_fill.
 * 16-Jan:    s7_is_ulong_long and friends for C pointers in 64-bit situations.
 * 9-Jan-09   multiprecision arithmetic (gmp, mpfr, mpc) on the WITH_GMP switch
 * --------
 * 29-Dec:    "+" specialization example, s7_apply_function.
 * 3-Dec:     s7_open_output_function.
 * 30-Nov:    s7_wrong_number_of_args_error.
 * 24-Nov:    changed s7_make_counted_string to s7_make_string_with_length.
 *              also added built-in format and define*
 * 10-Nov:    s7_define_constant,
 *              built-in (scheme-side) pi, most-positive-fixnum, most-negative-fixnum
 * 7-Nov:     removed s7_is_immutable and friends, s7_reverse_in_place.
 *              removed the s7_pointer arg to s7_gc_on.
 *              added s7_UNSPECIFIED
 * 25-Oct:    added name arg to s7_make_procedure_with_setter, 
 *              and s7_scheme arg to new_type print func.
 * 1-Oct-08   version 1.0
 */

