/* S7, a Scheme interpreter
 *
 *    derived from:
 */

/* T I N Y S C H E M E    1 . 3 9
 *   Dimitrios Souflis (dsouflis@acm.org)
 *   Based on MiniScheme (original credits follow)
 * (MINISCM)               coded by Atsushi Moriwaki (11/5/1989)
 * (MINISCM)           E-MAIL :  moriwaki@kurims.kurims.kyoto-u.ac.jp
 * (MINISCM) This version has been modified by R.C. Secrist.
 * (MINISCM)
 * (MINISCM) Mini-Scheme is now maintained by Akira KIDA.
 * (MINISCM)
 * (MINISCM) This is a revised and modified version by Akira KIDA.
 * (MINISCM)	current version is 0.85k4 (15 May 1994)
 */


/* S7, Bill Schottstaedt, Aug-08
 *
 *   major changes from tinyScheme:
 *        just two files: s7.c and s7.h, source-level embeddable (no library, no run-time init files)
 *        full continuations, call-with-exit for goto or return, dynamic-wind
 *        ratios and complex numbers (and ints are 64-bit)
 *        generalized set!, procedure-with-setter, applicable objects
 *        defmacro and define-macro, keywords, hash tables, block comments, define*
 *        error handling using error and catch
 *        in sndlib, the run macro works giving S7 a (somewhat limited) byte compiler
 *        no invidious distinction between built-in and "foreign"
 *        threads
 *
 *   many minor changes!
 *
 *   deliberate omission from r5rs: 
 *        no inexact integer or ratio (so, for example, truncate returns an exact integer), no exact complex or exact real
 *           (exact? has no obvious meaning in regard to complex numbers anyway -- are we referring to the polar or
 *            the rectangular form, and are both real and imaginary parts included? -- why can't they be separate?)
 *           In s7, exact? is a synonym for rational?
 *           Also, why isn't 1e2 considered exact?  The e2 business is 10^2 -- not a float!
 *           
 *        '#' does not stand for an unknown digit, and the '@' complex number notation is ignored
 *           I also choose not to include numbers such as +i (= 0+i) -- include the real part!
 *
 *   deliberate difference from r5rs:
 *        modulo, remainder, and quotient take integer, ratio, or real args
 *        lcm and gcd can take integer or ratio args
 *        delay is renamed make-promise to avoid collisions in CLM
 *        continuation? function to distinguish a continuation from a procedure
 *        log takes an optional 2nd arg (base)
 *
 *   other additions: 
 *        procedure-source, procedure-arity, procedure-documentation, help
 *        symbol-table, symbol->value, global-environment, current-environment
 *        provide, provided?, defined?
 *        port-line-number, port-filename
 *        read-line, read-byte, write-byte
 *        logior, logxor, logand, lognot, ash
 *        sinh, cosh, tanh, asinh, acosh, atanh
 *        object->string, eval-string
 *        reverse!, list-set!
 *        gc, gc-verbose, load-verbose, quit
 *        backtrace, backtrace-length, clear-backtrace
 *        *features*, *load-path*, *vector-print-length*
 *        define-constant, pi, most-positive-fixnum, most-negative-fixnum
 *        format (only the simple directives)
 *
 *
 * still to do:
 *   syntax-rules and friends
 *
 *
 * Mike Scholz provided the FreeBSD support (complex trig funcs, etc)
 *
 *
 * Documentation is in s7.h.
 *
 *
 * ---------------- compile time switches ---------------- 
 */

#include <mus-config.h>

/* 
 * Your config file goes here, or just replace that #include line with the defines you need.
 * The only compile-time switches involve booleans, threads, and complex numbers.
 * Currently we assume we have setjmp.h (used by the error handlers).
 *
 * If pthreads are available:
 *
 *   #define HAVE_PTHREADS 1
 *
 * s7.h includes stdbool.h if HAVE_STDBOOL_H is 1 and we're not in C++.
 *
 *
 * Complex number support (which is problematic in C++, Solaris, and netBSD)
 *   is on the WITH_COMPLEX switch. On a Mac, or in Linux, if you're not using C++,
 *   you can use:
 *
 *   #define WITH_COMPLEX 1
 *   #define HAVE_COMPLEX_TRIG 1
 *
 *   Define the first if your compiler has any support for complex numbers.
 *   Define the second if functions like csin are defined in the math library.
 *
 *   In C++ use:
 *
 *   #define WITH_COMPLEX 1
 *   #define HAVE_COMPLEX_TRIG 0
 *
 *   Some systems (freeBSD) have complex.h, but not the trig funcs, so
 *   WITH_COMPLEX means we can find
 *
 *      cimag creal cabs csqrt carg conj
 *
 *   and HAVE_COMPLEX_TRIG means we have
 *
 *      cacos cacosh casin casinh catan catanh ccos ccosh cexp clog cpow csin csinh ctan ctanh
 *
 * When WITH_COMPLEX is 0 or undefined, the complex functions are stubs that simply return their
 *   argument -- this will be very confusing for the s7 user because, for example, (sqrt -2)
 *   will return something bogus (it will not signal an error).
 *
 * Snd's configure.ac has m4 code to handle WITH_COMPLEX and HAVE_COMPLEX_TRIG.
 */

#if __cplusplus
  #ifndef WITH_COMPLEX
    #define WITH_COMPLEX 1
  #endif
  #ifndef HAVE_COMPLEX_TRIG
    #define HAVE_COMPLEX_TRIG 0
  #endif
#endif


#include <unistd.h>
#include <limits.h>
#include <float.h>
#include <ctype.h>
#include <strings.h>
#include <string.h>
#include <stdlib.h>
#include <sys/types.h>

#if __cplusplus
  #include <cmath>
#else
  #include <math.h>
#endif

#if WITH_COMPLEX
  #if __cplusplus
    #include <complex>
  #else
    #include <complex.h>
  #endif
#endif

#include <setjmp.h>

#if HAVE_PTHREADS
#include <pthread.h>
#endif

#include "s7.h"


/* ---------------- initial sizes ---------------- */

#define INITIAL_HEAP_SIZE 128000
/* the heap grows as needed, this is its initial size.
 * in Snd, there are about 10000 permanent objects sitting in the heap, so the bigger the size, the less
 *    often we mark these objects -- the GC is more efficient as the heap size increases.  Each object
 *    is about 20 bytes, so even 256K is nothing in modern memory sizes.  The heaps grows as needed,
 *    so any number > 0 is ok.  The smaller the heap, the smaller the initial run-time memory footprint.
 *    The bigger the heap, the faster s7 runs.
 */

#define SYMBOL_TABLE_SIZE 9601
/* names are hashed into the symbol table (a vector) and collisions are chained as lists; was 461, then 4603.
 *   setting it to 100043 did improve performance.  Max list size (at 9601) in snd-test.scm is 6.
 */

#define INITIAL_STACK_SIZE 1000            
/* the stack grows as needed, each frame takes 4 entries, this is its initial size */

#define INITIAL_PROTECTED_OBJECTS_SIZE 16  
/* a vector of objects that are (semi-permanently) protected from the GC, grows as needed */

#define GC_TEMPS_SIZE 4096
/* the number of recent objects that are temporarily gc-protected; 512 is too small in the threads case (generators.scm) */

#define S7_DEBUGGING 0
/* this is for a bunch of sanity checks */

#define WITH_READ_LINE 1
/* this includes the (non-standard) read-line function */

#define INITIAL_BACKTRACE_SIZE 16
/* this is the number of entries in the backtrace printout of previous evaluations */

#define CASE_SENSITIVE 1
/* this determines whether names are case sensitive */

#define WITH_FORMAT 1
/* this includes a simple (no Roman numerals!) version of format */


/* -------------------------------------------------------------------------------- */

/* this file is organized as follows:
 *    structs and type flags
 *    constants
 *    GC
 *    stacks
 *    symbols
 *    environments
 *    continuations
 *    numbers
 *    characters
 *    strings
 *    ports
 *    lists
 *    vectors
 *    objects and functions
 *    eq and such
 *    format
 *    error handlers
 *    sundry leftovers
 *    eval
 *    quasiquote
 *    threads
 *    s7 init
 */


typedef enum {OP_READ_INTERNAL, OP_EVAL, OP_EVAL_ARGS0, OP_EVAL_ARGS1, OP_APPLY, OP_DOMACRO, OP_LAMBDA, OP_QUOTE, 
	      OP_DEFINE0, OP_DEFINE1, OP_BEGIN, OP_IF0, OP_IF1, 
	      OP_SET0, OP_SET1, OP_SET2, OP_DEFINE_CONSTANT0, OP_DEFINE_CONSTANT1, 
	      OP_LET0, OP_LET1, OP_LET2, OP_LET_STAR0, OP_LET_STAR1, OP_LET_STAR2, 
	      OP_LETREC0, OP_LETREC1, OP_LETREC2, OP_COND0, OP_COND1, OP_MAKE_PROMISE, OP_AND0, OP_AND1, 
	      OP_OR0, OP_OR1, OP_CONS_STREAM0, OP_CONS_STREAM1, 
	      OP_DEFMACRO, OP_MACRO0, OP_MACRO1, OP_DEFINE_MACRO,
	      OP_CASE0, OP_CASE1, OP_CASE2, OP_READ_LIST, OP_READ_DOT, OP_READ_QUOTE, 
	      OP_READ_QUASIQUOTE, OP_READ_QUASIQUOTE_VECTOR, OP_READ_UNQUOTE, OP_READ_UNQUOTE_SPLICING, 
	      OP_READ_VECTOR, OP_FORCE, OP_READ_RETURN_EXPRESSION,
	      OP_READ_POP_AND_RETURN_EXPRESSION, OP_LOAD_RETURN_IF_EOF, OP_LOAD_CLOSE_AND_POP_IF_EOF, 
	      OP_EVAL_STRING, OP_EVAL_STRING_DONE, OP_QUIT, OP_CATCH, OP_DYNAMIC_WIND, OP_FOR_EACH, OP_MAP, 
	      OP_DO, OP_DO_END0, OP_DO_END1, OP_DO_STEP0, OP_DO_STEP1, OP_DO_STEP2, OP_DO_INIT,
	      OP_DEFINE_STAR, OP_LAMBDA_STAR,
	      OP_MAX_DEFINED
} opcode_t;


/* num, for generic arithmetic */
typedef struct num {
  char type;
  union {
    
    s7_Int ivalue;
    
    s7_Double rvalue;
    
    struct {
      s7_Int numerator;
      s7_Int denominator;
    } fvalue;
    
    struct {
      s7_Double real;
      s7_Double imag;
    } cvalue;
    
  } value;
} num;


typedef struct rport {
  bool is_closed;
  bool is_file;
  bool needs_free;
  FILE *file;
  int line_number;
  char *filename;
  int file_number;
  char *value;
  int size, point;
} rport;


typedef struct continuation {
  int cc_stack_size, cc_stack_top;
  s7_pointer cc_stack;
} continuation;


typedef struct ffunc {
  s7_function ff;
  const char *name;
  char *doc;
  int required_args, optional_args;
  bool rest_arg;
} ffunc;


typedef struct rcatch {
  int goto_loc;
  s7_pointer tag;
  s7_pointer handler;
} rcatch;


typedef struct dwind {
  int state;
  s7_pointer in, out, body;
} dwind;


/* cell structure */
typedef struct s7_cell {
  unsigned int flag;
  union {
    
    struct {
      char *svalue;
      int  length;
    } string;
    
    num number;
    
    rport *port;
    
    char cvalue;
    
    opcode_t proc_num;
    
    struct {
      int length;
      s7_pointer *elements;
    } vector;
    
    ffunc *ffptr;
    
    struct {
      struct s7_cell *car;
      struct s7_cell *cdr;
      /* num is the biggest entry here = 20 bytes, so there's a lot of unused space in most entries */
      /*   so even with 64 bit pointers, we have room here for an int */
      int line;
    } cons;
    
    struct {
      int type;
      void *value;
    } fobj;
    
    continuation *cc;
    
    int goto_loc;
    
    rcatch *catcher;
    
    dwind *winder;
    
  } object;
} s7_cell;


struct s7_scheme {  
  s7_cell **heap, **free_heap;
  int heap_size, free_heap_top;
  
  s7_pointer args;                    /* arguments of current function */
  s7_pointer envir;                   /* current environment */
  s7_pointer code;                    /* current code */
  
  s7_pointer stack;                   /* stack is a vector in this case */
  int stack_size, stack_top;
  s7_pointer *small_ints;             /* permanent numbers for opcode entries in the stack */
  
  s7_pointer protected_objects;       /* a vector of gc-protected objects */
  int *protected_objects_size, *protected_objects_loc; /* pointers so they're global across threads */
  
  struct s7_cell _NIL;
  s7_pointer NIL;                     /* empty list */
  
  struct s7_cell _HASHT;
  s7_pointer T;                       /* #t */
  
  struct s7_cell _HASHF;
  s7_pointer F;                       /* #f */
  
  struct s7_cell _EOF_OBJECT;
  s7_pointer EOF_OBJECT;              /* end-of-file object */
  
  struct s7_cell _UNDEFINED;  
  s7_pointer UNDEFINED;               /* unset or undefined object */
  
  struct s7_cell _UNSPECIFIED;
  s7_pointer UNSPECIFIED;             /* the unspecified value */
  
  s7_pointer symbol_table;            /* symbol table */
  s7_pointer global_env;              /* global environment */
  
  s7_pointer LAMBDA;                  /* syntax lambda */
  s7_pointer LAMBDA_STAR;             /* syntax lambda* (for define*) */
  s7_pointer QUOTE;                   /* syntax quote */
  s7_pointer QUASIQUOTE;              /* symbol quasiquote */
  s7_pointer UNQUOTE;                 /* symbol unquote */
  s7_pointer UNQUOTE_SPLICING;        /* symbol unquote-splicing */
  s7_pointer FEED_TO;                 /* => */
  s7_pointer SET_OBJECT;              /* object set method */
  s7_pointer APPLY, VECTOR, CONS, APPEND, CDR, VECTOR_FUNCTION, VALUES;
  s7_pointer ERROR, WRONG_TYPE_ARG, OUT_OF_RANGE, FORMAT_ERROR;
  s7_pointer KEY_KEY, KEY_OPTIONAL, KEY_REST;
  
  s7_pointer input_port;              /* current-input-port (nil = stdin) */
  s7_pointer input_port_stack;        /*   input port stack (load and read internally) */
  s7_pointer output_port;             /* current-output-port (nil = stderr) */
  s7_pointer error_port;              /* current-error-port (nil = stderr) */
  
  /* these 5 are pointers so that all thread refs are to the same thing */
  bool *gc_off;                       /* if true, the GC won't run */
  bool *gc_verbose;                   /* if gc_verbose is true, print gc status */
  bool *load_verbose;                 /* if load_verbose is true, print file names as they are loaded */
  long *gensym_counter;
  
  /* these are locals in eval, but we want that code to be context-free */
  #define INITIAL_STRBUF_SIZE 1024
  int strbuf_size;
  char *strbuf;
  
#if WITH_READ_LINE
  char *read_line_buf;
  int read_line_buf_size;
#endif

  int tok;
  s7_pointer value;
  opcode_t op;
  s7_pointer x, y, z; /* evaluator local vars */
  s7_pointer *temps;
  int temps_ctr, temps_size;
  num v;
  
  jmp_buf goto_start;
  bool longjmp_ok;
  void (*error_exiter)(void);
  
#if HAVE_PTHREADS
  struct s7_scheme *orig_sc;
  s7_pointer key_values;
#endif

  /* the internal stack does not help the s7 user find a bug.
   *   It is primarily controlling eval-internal loops, and has
   *   little information for tracking call sequences.
   * So, I think I'll try to conjure up a sort of fake stack
   *   that tracks high-level eval/apply sequences.  The main
   *   thing to avoid here is the clisp-style flood of useless
   *   printout.  So the next fields are for our backtrace.
   */

  int saved_line_number;
  int backtrace_size, backtrace_top;
  s7_pointer *backtrace_ops, *backtrace_args;
#if HAVE_PTHREADS
  int *backtrace_thread_ids;
  int thread_id;
  int *thread_ids; /* global current top thread_id */
#endif
};


enum scheme_types {
  T_NIL_TYPE = 1,
  T_STRING = 2,
  T_NUMBER = 3,
  T_SYMBOL = 4,
  T_PAIR = 6,
  T_CLOSURE = 7,
  T_CONTINUATION = 8,
  T_S7_FUNCTION = 9,
  T_CHARACTER = 10,
  T_INPUT_PORT = 11,
  T_VECTOR = 12,
  T_MACRO = 13,
  T_PROMISE = 14,
  T_S7_OBJECT = 15,
  T_GOTO = 16,
  T_OUTPUT_PORT = 17,
  T_CATCH = 18,
  T_DYNAMIC_WIND = 19,
  T_HASH_TABLE = 20,
  T_CLOSURE_STAR = 21,
  T_LAST_TYPE = 21
};


#define TYPE_BITS                     16
#define T_MASKTYPE                    0xffff

#define typeflag(p)                   ((p)->flag)
#define type(p)                       (typeflag(p) & T_MASKTYPE)
/* set_type below -- needs to maintain mark setting */

#define T_SYNTAX                      (1 << (TYPE_BITS + 1))
#define is_syntax(p)                  (typeflag(p) & T_SYNTAX)
#define syntax_opcode(x)              cdr(x)

#define T_IMMUTABLE                   (1 << (TYPE_BITS + 2))
#define is_immutable(p)               (typeflag(p) & T_IMMUTABLE)
#define set_immutable(p)              typeflag(p) |= T_IMMUTABLE

#define T_ATOM                        (1 << (TYPE_BITS + 3))
#define is_atom(p)                    (typeflag(p) & T_ATOM)

#define T_GC_MARK                     (1 << (TYPE_BITS + 4))
#define is_marked(p)                  (typeflag(p) &  T_GC_MARK)
#define set_mark(p)                   typeflag(p)  |= T_GC_MARK
#define clear_mark(p)                 typeflag(p)  &= (~T_GC_MARK)
/* making this a separate bool field in the cell struct slightly speeds up the mark function,
 *   but at the cost of 4 bytes per object. Since the speedup was about 2% overall, I think
 *   the size increase is more important.
 */

#define T_CONSTANT                    (1 << (TYPE_BITS + 5))
#define is_constant(p)                (typeflag(p) & T_CONSTANT)
#define local_protect(p)              typeflag(p) |= T_CONSTANT
#define local_unprotect(p)            typeflag(p) &= (~T_CONSTANT)

#define T_OBJECT                      (1 << (TYPE_BITS + 6))
#define T_FINALIZABLE                 (1 << (TYPE_BITS + 7))
#define T_SIMPLE                      (1 << (TYPE_BITS + 8))
#define is_simple(p)                  (typeflag(p) & T_SIMPLE)

#define T_DONT_COPY                   (1 << (TYPE_BITS + 9))
#define dont_copy(p)                  (typeflag(p) & T_DONT_COPY)

#define T_PROCEDURE                   (1 << (TYPE_BITS + 10))
#define is_procedure(p)               (typeflag(p) & T_PROCEDURE)

#define T_ETERNAL                     (1 << (TYPE_BITS + 11))
#define is_eternal(p)                 (typeflag(p) & T_ETERNAL)

#define T_UNUSED_BITS                 0xf0000000

#if HAVE_PTHREADS
#define set_type(p, f)                typeflag(p) = ((typeflag(p) & T_GC_MARK) | (f) | T_OBJECT)
/* the gc call can be interrupted, leaving mark bits set -- we better not clear those bits */
#else
#define set_type(p, f)                typeflag(p) = ((f) | T_OBJECT)
#endif

#define is_object(x)                  ((x) && (((typeflag(x) & (T_UNUSED_BITS | T_OBJECT)) == T_OBJECT) && (type(x) <= T_LAST_TYPE)))

#define is_true(p)                    ((p) != sc->F)
#define is_false(p)                   ((p) == sc->F)
#define make_boolean(sc, Val)         ((Val) ? sc->T : sc->F)

#define is_pair(p)                    (type(p) == T_PAIR)
#define car(p)                        ((p)->object.cons.car)
#define cdr(p)                        ((p)->object.cons.cdr)
#define caar(p)                       car(car(p))
#define cadr(p)                       car(cdr(p))
#define cdar(p)                       cdr(car(p))
#define cddr(p)                       cdr(cdr(p))
#define caaar(p)                      car(car(car(p)))
#define cadar(p)                      car(cdr(car(p)))
#define cdadr(p)                      cdr(car(cdr(p)))
#define caddr(p)                      car(cdr(cdr(p)))
#define caadr(p)                      car(car(cdr(p)))
#define cdaar(p)                      cdr(car(car(p)))
#define cdddr(p)                      cdr(cdr(cdr(p)))
#define cddar(p)                      cdr(cdr(car(p)))
#define caaadr(p)                     car(car(car(cdr(p))))
#define cadaar(p)                     car(cdr(car(car(p))))
#define cadddr(p)                     car(cdr(cdr(cdr(p))))
#define cddddr(p)                     cdr(cdr(cdr(cdr(p))))
#define caddar(p)                     car(cdr(cdr(car(p))))
#define pair_line_number(p)           (p)->object.cons.line

#define symbol_location(p)            (p)->object.cons.line

#define string_value(p)               ((p)->object.string.svalue)
#define string_length(p)              ((p)->object.string.length)
#define character(p)                  ((p)->object.cvalue)

#define vector_length(p)              ((p)->object.vector.length)
#define vector_element(p, i)          ((p)->object.vector.elements[i])
#define small_int(Sc, Val)            (Sc)->small_ints[Val]
#define small_int_as_num(Sc, Val)     (Sc)->small_ints[Val]->object.number

#define is_input_port(p)              (type(p) == T_INPUT_PORT) 
#define is_output_port(p)             (type(p) == T_OUTPUT_PORT)
#define is_string_port(p)             (!((p)->object.port->is_file))
#define is_file_port(p)               (p)->object.port->is_file
#define port_line_number(p)           (p)->object.port->line_number
#define port_file_number(p)           (p)->object.port->file_number
#define port_filename(p)              (p)->object.port->filename
#define port_file(p)                  (p)->object.port->file
#define port_is_closed(p)             (p)->object.port->is_closed
#define port_string(p)                (p)->object.port->value
#define port_string_length(p)         (p)->object.port->size
#define port_string_point(p)          (p)->object.port->point
#define port_needs_free(p)            (p)->object.port->needs_free

#define function_call(f)              (f)->object.ffptr->ff
#define function_name(f)              (f)->object.ffptr->name
#define function_documentation(f)     (f)->object.ffptr->doc
#define function_required_args(f)     (f)->object.ffptr->required_args
#define function_optional_args(f)     (f)->object.ffptr->optional_args
#define function_has_rest_arg(f)      (f)->object.ffptr->rest_arg

#define continuation_cc_stack(p)      (p)->object.cc->cc_stack

#define is_goto(p)                    (type(p) == T_GOTO)
#define is_macro(p)                   (type(p) == T_MACRO)
#define is_promise(p)                 (type(p) == T_PROMISE)
#define is_closure(p)                 (type(p) == T_CLOSURE)
#define is_closure_star(p)            (type(p) == T_CLOSURE_STAR)

#define is_catch(p)                   (type(p) == T_CATCH)
#define catch_tag(p)                  (p)->object.catcher->tag
#define catch_goto_loc(p)             (p)->object.catcher->goto_loc
#define catch_handler(p)              (p)->object.catcher->handler

#define is_dynamic_wind(p)            (type(p) == T_DYNAMIC_WIND)
#define dynamic_wind_state(p)         (p)->object.winder->state
#define dynamic_wind_in(p)            (p)->object.winder->in
#define dynamic_wind_out(p)           (p)->object.winder->out
#define dynamic_wind_body(p)          (p)->object.winder->body
enum {DWIND_INIT, DWIND_BODY, DWIND_FINISH};


#define NUM_INT 0
#define NUM_RATIO 1
#define NUM_REAL 2
#define NUM_REAL2 3
#define NUM_COMPLEX 4

/* so int = 0, exact < 2, ratio = 1, complex >= 4, real 2 or 3
 *   (I'm not going to support exact complex or inexact ratio etc)
 */

#define num_type(n)                   (n.type)
#define object_number_type(p)         (p->object.number.type)

#define numerator(n)                  n.value.fvalue.numerator
#define denominator(n)                n.value.fvalue.denominator
#define fraction(n)                   (((s7_Double)numerator(n)) / ((s7_Double)denominator(n)))

#define real_part(n)                  n.value.cvalue.real
#define imag_part(n)                  n.value.cvalue.imag

#define integer(n)                    n.value.ivalue

#if __cplusplus
  using namespace std;
  typedef complex<s7_Double> s7_Complex;
  static s7_Double Real(complex<s7_Double> x) {return(real(x));} /* protect the C++ name */
  static s7_Double Imag(complex<s7_Double> x) {return(imag(x));}
#endif

#define real(n)                       n.value.rvalue



#if S7_DEBUGGING
static const char *type_names[T_LAST_TYPE + 1] = {
  "unused!", "nil", "string", "number", "symbol", "procedure", "pair", "closure", "continuation",
  "s7-function", "character", "input port", "vector", "macro", "promise", "s7-object", 
  "goto", "output port", "catch", "dynamic-wind", "hash-table", "closure*"
};

static char *describe_type(s7_pointer p)
{
  char *buf;
  if (!p)
    return(strdup("null pointer"));

  buf = (char *)calloc(1024, sizeof(char));
  snprintf(buf, 1024, "%s%s%s%s%s%s%s%s%s%s%s",
	  ((type(p) >= 0) && (type(p) <= T_LAST_TYPE)) ? type_names[type(p)] : "bogus type",
	  (typeflag(p) & T_SYNTAX) ? " syntax" : "",
	  (typeflag(p) & T_IMMUTABLE) ? " immutable" : "",
	  (typeflag(p) & T_ATOM) ? " atom" : "",
	  (typeflag(p) & T_GC_MARK) ? " marked" : "",
	  (typeflag(p) & T_OBJECT) ? " object" : "",
	  (typeflag(p) & T_FINALIZABLE) ? " gc-freeable" : "",
	  (typeflag(p) & T_SIMPLE) ? " simple" : "",
	  (typeflag(p) & T_ETERNAL) ? " eternal" : "",
	  (typeflag(p) & T_UNUSED_BITS) ? " and other garbage bits!" : "",
	  ((typeflag(p) == 0) && (car(p) == 0) && (cdr(p) == 0) && (pair_line_number(p) == 0)) ? " [recently GC'd (all 0)]" : "");
  return(buf);
}

#define ASSERT_IS_OBJECT(Obj, Name) \
  if (!(is_object(Obj))) \
    {fprintf(stderr, "%s[%d]: %s is not an object: %p %s\n", __FUNCTION__, __LINE__, Name, Obj, describe_type(Obj)); abort();}

#else

#define ASSERT_IS_OBJECT(Obj, Name)

#endif


#define TOK_EOF     (-1)
#define TOK_LPAREN  0
#define TOK_RPAREN  1
#define TOK_DOT     2
#define TOK_ATOM    3
#define TOK_QUOTE   4
#define TOK_DQUOTE  6
#define TOK_BQUOTE  7
#define TOK_COMMA   8
#define TOK_ATMARK  9
#define TOK_SHARP_CONST 10
#define TOK_VEC     11

#define BACKQUOTE '`'


#if CASE_SENSITIVE

#define string_downcase(Str) Str
#define STRCMP(Str1, Str2) strcmp(Str1, Str2)
/* STRCMP currently only used in symbol table (hash) refs, and only checks == 0 */

#else

#define STRCMP(Str1, Str2) strcasecmp(Str1, Str2)
static const char *string_downcase(char *s) 
{
  const char *p = s;
  while (*s) 
    {
      *s = tolower(*s);
      s++;
    }
  return(p);
}

#endif


static int safe_strlen(const char *str)
{
  if ((str) && (*str))
    return(strlen(str));
  return(0);
}

static void s7_mark_object_1(s7_pointer p);
static void s7_mark_embedded_objects(s7_pointer a); /* called by gc, calls fobj's mark func */
static s7_pointer eval(s7_scheme *sc, opcode_t first_op);
static s7_pointer s7_division_by_zero_error(s7_scheme *sc, const char *caller, s7_pointer arg);
static s7_pointer s7_file_error(s7_scheme *sc, const char *caller, const char *descr, const char *name);
static void s7_free_function(s7_pointer a);
static int remember_file_name(const char *file);
static s7_pointer safe_reverse_in_place(s7_scheme *sc, s7_pointer list);
static s7_pointer s7_immutable_cons(s7_scheme *sc, s7_pointer a, s7_pointer b);
static void s7_free_object(s7_pointer a);
static char *s7_describe_object(s7_scheme *sc, s7_pointer a);
static s7_pointer make_atom(s7_scheme *sc, char *q, int radix, bool want_symbol);
static bool s7_is_applicable_object(s7_pointer x);
static s7_pointer make_list_1(s7_scheme *sc, s7_pointer a);



/* -------------------------------- constants -------------------------------- */

s7_pointer s7_F(s7_scheme *sc) 
{
  return(sc->F);
}


s7_pointer s7_T(s7_scheme *sc) 
{
  return(sc->T);
}


s7_pointer s7_NIL(s7_scheme *sc) 
{
  return(sc->NIL);
}


s7_pointer s7_UNDEFINED(s7_scheme *sc) 
{
  return(sc->UNDEFINED);
}


s7_pointer s7_UNSPECIFIED(s7_scheme *sc) 
{
  return(sc->UNSPECIFIED);
}


bool s7_is_unspecified(s7_scheme *sc, s7_pointer val)
{
  return(val == sc->UNSPECIFIED);
}


s7_pointer s7_EOF_OBJECT(s7_scheme *sc) 
{
  return(sc->EOF_OBJECT);
}


static s7_pointer g_not(s7_scheme *sc, s7_pointer args)
{
  #define H_not "(not obj) returns #t if obj is #f, otherwise #t"
  return(make_boolean(sc, is_false(car(args))));
}


bool s7_boolean(s7_scheme *sc, s7_pointer x)
{
  return(x != sc->F);
}


bool s7_is_boolean(s7_scheme *sc, s7_pointer x)
{
  return((x == sc->F) || (x == sc->T));
}


s7_pointer s7_make_boolean(s7_scheme *sc, bool x)
{
  return(make_boolean(sc, x));
}


static s7_pointer g_is_boolean(s7_scheme *sc, s7_pointer args)
{
  #define H_is_boolean "(boolean? obj) returns #t if obj is #f or #t"
  return(s7_make_boolean(sc, s7_is_boolean(sc, car(args))));
}


static bool s7_is_immutable(s7_pointer p) 
{ 
  return(typeflag(p) & T_IMMUTABLE);
}


static s7_pointer s7_set_immutable(s7_pointer p) 
{ 
  typeflag(p) |= T_IMMUTABLE;
  return(p);
}


static void set_pair_line_number(s7_pointer p, int n)
{
  if (!is_eternal(p))
    p->object.cons.line = n;
}



/* -------------------------------- GC -------------------------------- */

#if HAVE_PTHREADS
static pthread_mutex_t protected_objects_lock = PTHREAD_MUTEX_INITIALIZER;
#endif


int s7_gc_protect(s7_scheme *sc, s7_pointer x)
{
  int i, loc, new_size;
  
#if HAVE_PTHREADS
  pthread_mutex_lock(&protected_objects_lock);
#endif

  if (vector_element(sc->protected_objects, (*(sc->protected_objects_loc))) == sc->NIL)
    {
      vector_element(sc->protected_objects, (*(sc->protected_objects_loc))) = x;
      loc = (*(sc->protected_objects_loc))++;
      if ((*(sc->protected_objects_loc)) >= (*(sc->protected_objects_size)))
	(*(sc->protected_objects_loc)) = 0;
      {
#if HAVE_PTHREADS
	pthread_mutex_unlock(&protected_objects_lock);
#endif
	return(loc);
      }
    }
  
  for (i = 0; i < (*(sc->protected_objects_size)); i++)
    if (vector_element(sc->protected_objects, i) == sc->NIL)
      {
	vector_element(sc->protected_objects, i) = x;
#if HAVE_PTHREADS
	pthread_mutex_unlock(&protected_objects_lock);
#endif
	return(i);
      }
  
  loc = (*(sc->protected_objects_size));
  new_size = 2 * (*(sc->protected_objects_size));
  sc->protected_objects->object.vector.elements = (s7_pointer *)realloc(sc->protected_objects->object.vector.elements, new_size * sizeof(s7_pointer));
  for (i = (*(sc->protected_objects_size)) + 1; i < new_size; i++)
    vector_element(sc->protected_objects, i) = sc->NIL;
  sc->protected_objects->object.vector.length = new_size;
  (*(sc->protected_objects_size)) = new_size;
  vector_element(sc->protected_objects, loc) = x;
  
#if HAVE_PTHREADS
  pthread_mutex_unlock(&protected_objects_lock);
#endif
  return(loc);
}


void s7_gc_unprotect(s7_scheme *sc, s7_pointer x)
{
  int i;

#if HAVE_PTHREADS
  pthread_mutex_lock(&protected_objects_lock);
#endif

  for (i = 0; i < (*(sc->protected_objects_size)); i++)
    if (vector_element(sc->protected_objects, i) == x)
      {
	vector_element(sc->protected_objects, i) = sc->NIL;
	(*(sc->protected_objects_loc)) = i;
#if HAVE_PTHREADS
	pthread_mutex_unlock(&protected_objects_lock);
#endif
	return;
      }

#if HAVE_PTHREADS
  pthread_mutex_unlock(&protected_objects_lock);
#endif
}


void s7_gc_unprotect_at(s7_scheme *sc, int loc)
{
#if HAVE_PTHREADS
  pthread_mutex_lock(&protected_objects_lock);
#endif

  vector_element(sc->protected_objects, loc) = sc->NIL;
  (*(sc->protected_objects_loc)) = loc;

#if HAVE_PTHREADS
  pthread_mutex_unlock(&protected_objects_lock);
#endif
}


s7_pointer s7_gc_protected_at(s7_scheme *sc, int loc)
{
  s7_pointer obj;

#if HAVE_PTHREADS
  pthread_mutex_lock(&protected_objects_lock);
#endif

  obj = vector_element(sc->protected_objects, loc);

#if HAVE_PTHREADS
  pthread_mutex_unlock(&protected_objects_lock);
#endif

  return(obj);
}


static void finalize_s7_cell(s7_scheme *sc, s7_pointer a) 
{
  switch (type(a))
    {
    case T_STRING:
      free(string_value(a)); /* calloc'd in make-*-string */
      break;
      
    case T_INPUT_PORT:
      if (port_needs_free(a))
	{
	  if (port_string(a))
	    {
	      free(port_string(a));
	      port_string(a) = NULL;
	    }
	  port_needs_free(a) = false;
	}
      if (port_filename(a))
	{
	  free(port_filename(a));
	  port_filename(a) = NULL;
	}
      free(a->object.port);
      break;
      
    case T_OUTPUT_PORT:
      s7_close_output_port(sc, a);
      if ((is_file_port(a)) && 
	  (port_filename(a)))
	{
	  free(port_filename(a));
	  port_filename(a) = NULL;
	}
      free(a->object.port);
      break;
      
    case T_S7_OBJECT:
      s7_free_object(a);
      break;
      
    case T_S7_FUNCTION:
      s7_free_function(a);
      break;
      
    case T_VECTOR:
    case T_HASH_TABLE:
      if (vector_length(a) > 0)
	free(a->object.vector.elements);
      break;
      
    case T_CONTINUATION:
      if (a->object.cc)
	free(a->object.cc);
      break;
      
    case T_CATCH:
      free(a->object.catcher);
      break;
      
    case T_DYNAMIC_WIND:
      free(a->object.winder);
      break;
      
    default:
      break;
    }
}


#if defined(__GNUC__) && (!(defined(__cplusplus)))
  #define S7_MARK(Obj) ({ s7_pointer _p_; _p_ = Obj; if (!is_marked(_p_)) s7_mark_object_1(_p_); })
#else
  #define S7_MARK(Obj) if (!is_marked(Obj)) s7_mark_object_1(Obj)  
#endif
/* this is slightly faster than if we first call s7_mark_object, then check the mark bit */


static void mark_vector(s7_pointer p, int top)
{
  int i;
  s7_pointer *tp;
  set_mark(p);
  tp = (s7_pointer *)(p->object.vector.elements);
  for(i = 0; i < top; i++) 
    S7_MARK(tp[i]);
}


static void s7_mark_object_1(s7_pointer p)
{
  /*
  if (is_marked(p)) return; 
  */
  ASSERT_IS_OBJECT(p, "mark");
  
  set_mark(p);
  
  if (is_simple(p)) return;
  
  switch (type(p))
    {
    case T_VECTOR:
    case T_HASH_TABLE:
      mark_vector(p, vector_length(p));
      return;
      
    case T_S7_OBJECT:
      s7_mark_embedded_objects(p);
      return;

    case T_CONTINUATION:
      S7_MARK(continuation_cc_stack(p));
      return;

    case T_CATCH:
      S7_MARK(catch_tag(p));
      S7_MARK(catch_handler(p));
      return;

    case T_DYNAMIC_WIND:
      S7_MARK(dynamic_wind_in(p));
      S7_MARK(dynamic_wind_out(p));
      S7_MARK(dynamic_wind_body(p));
      return;

    default:
      break;
    }
  
  /* this should follow s7_is_object -- the latter is an atom, but we have to run the object's internal mark function */
  if (is_atom(p))
    return;
  
  if (car(p))
    S7_MARK(car(p));
  
  if (cdr(p))
    S7_MARK(cdr(p));
}


void s7_mark_object(s7_pointer p)
{
  S7_MARK(p);
}


#if S7_DEBUGGING
static int gc(s7_scheme *sc, const char *function, int line)
#else
static int gc(s7_scheme *sc)
#endif
{
  int i, old_free_heap_top;
  
  if ((*(sc->gc_verbose)) &&
      (sc->output_port == sc->NIL))
#if S7_DEBUGGING
    fprintf(stderr, "\n%s[%d] gc...", function, line);
#else
    fprintf(stderr, "gc...");
#endif
  
  /* mark all live objects (the symbol table is in permanent memory, not the heap) */
  S7_MARK(sc->global_env);
  S7_MARK(sc->args);
  S7_MARK(sc->envir);
  S7_MARK(sc->code);
  mark_vector(sc->stack, sc->stack_top);
  S7_MARK(sc->x);
  S7_MARK(sc->y);
  S7_MARK(sc->z);
  S7_MARK(sc->value);  

  S7_MARK(sc->input_port);
  S7_MARK(sc->input_port_stack);
  S7_MARK(sc->output_port);
  S7_MARK(sc->error_port);
  
  S7_MARK(sc->protected_objects);
  {
    s7_pointer *tmps;
    tmps = sc->temps;
    for (i = 0; i < sc->temps_size; i++)
      S7_MARK(tmps[i]);
  }

  for (i = 0; i < sc->backtrace_size; i++)
    {
      S7_MARK(sc->backtrace_ops[i]);
      S7_MARK(sc->backtrace_args[i]);
    }
  
  /* free up all other objects */
  old_free_heap_top = sc->free_heap_top;

  {
    s7_pointer *fp, *tp;
    fp = sc->free_heap;
    for (tp = sc->heap; (*tp); tp++) /* this form of the loop is slightly faster than counting i with sc->heap[i] */
      {
	s7_pointer p;
	p = (*tp);
	if (typeflag(p) != 0) /* an already-free object? */
	  {
	    if (is_marked(p)) 
	      clear_mark(p);
	    else 
	      {
		if (!is_constant(p))
		  {
		    if (typeflag(p) & T_FINALIZABLE)
		      finalize_s7_cell(sc, p); 
		    typeflag(p) = 0;
		    fp[sc->free_heap_top++] = p;
		  }
	      }
	  }
      }
  }
  
  if ((*(sc->gc_verbose)) &&
      (sc->output_port == sc->NIL))
    fprintf(stderr, "done: %d heap were recovered, total heap: %d\n", sc->free_heap_top - old_free_heap_top, sc->heap_size);
  
  return(sc->free_heap_top - old_free_heap_top); /* needed by cell allocator to decide when to increase heap size */
}


#if HAVE_PTHREADS
static pthread_mutex_t alloc_lock = PTHREAD_MUTEX_INITIALIZER;
#endif

#if S7_DEBUGGING
#define new_cell(Sc) new_cell_1(Sc, __FUNCTION__, __LINE__)
#if HAVE_PTHREADS
static s7_pointer new_cell_1(s7_scheme *nsc, const char *function, int line)
#else
static s7_pointer new_cell_1(s7_scheme *sc, const char *function, int line)
#endif
#else
#if HAVE_PTHREADS
static s7_pointer new_cell(s7_scheme *nsc)
#else
static s7_pointer new_cell(s7_scheme *sc)
#endif
#endif
{
  s7_pointer p;
  
#if HAVE_PTHREADS
  s7_scheme *sc;
  pthread_mutex_lock(&alloc_lock);
  sc = nsc->orig_sc;
#endif

  if (sc->free_heap_top <= 0)
    {
      /* no free heap */
      int k, old_size, freed_heap = 0;
      
      if (!(*(sc->gc_off)))
#if S7_DEBUGGING
	freed_heap = gc(sc, function, line);
#else
        freed_heap = gc(sc);
#endif
      /* when threads, the gc function can be interrupted at any point and resumed later -- mark bits need to be preserved during this interruption */
      
      if (freed_heap < sc->heap_size / 4) /* was 1000, setting it to 2 made no difference in run time */
	{
	  /* alloc more heap */
	  old_size = sc->heap_size;
	  sc->heap_size *= 2;
	  sc->heap = (s7_cell **)realloc(sc->heap, (sc->heap_size + 1) * sizeof(s7_cell *));
	  sc->free_heap = (s7_cell **)realloc(sc->free_heap, sc->heap_size * sizeof(s7_cell *));

	  for (k = old_size; k < sc->heap_size; k++)
	    {
	      sc->heap[k] = (s7_cell *)calloc(1, sizeof(s7_cell));
	      sc->free_heap[sc->free_heap_top++] = sc->heap[k];
	    }
	  sc->heap[sc->heap_size] = NULL; /* end mark for GC loop */
	}
    }

  p = sc->free_heap[--(sc->free_heap_top)];
  
  /* originally I tried to mark each temporary value until I was done with it, but
   *   that way madness lies... By delaying GC of _every_ %$^#%@ pointer, I can dispense
   *   with hundreds of individual protections.
   */

#if HAVE_PTHREADS
#if S7_DEBUGGING
  typeflag(p) = T_OBJECT; /* turn off the is_object bug checks in case we're interrupted and a GC (mark pass) intervenes */
#endif

  nsc->temps[nsc->temps_ctr++] = p;
  if (nsc->temps_ctr >= nsc->temps_size)
    nsc->temps_ctr = 0;

  pthread_mutex_unlock(&alloc_lock);

#else

  sc->temps[sc->temps_ctr++] = p;
  if (sc->temps_ctr >= sc->temps_size)
    sc->temps_ctr = 0;
#endif
  
  return(p);
}


static s7_pointer g_gc_verbose(s7_scheme *sc, s7_pointer a)
{
  #define H_gc_verbose "(gc-verbose bool) turns GC reportage on or off"
  s7_pointer old_val;
  old_val = (*(sc->gc_verbose)) ? sc->T : sc->F;
  (*(sc->gc_verbose)) = (car(a) != sc->F);
  return(old_val);
}


static s7_pointer g_gc(s7_scheme *sc, s7_pointer args)
{
  #define H_gc "(gc :optional on) runs the garbage collector.  If 'on' is supplied, it turns the GC on or off."
  
  if (args != sc->NIL)
    {
      (*(sc->gc_off)) = (car(args) == sc->F);
      if (*(sc->gc_off)) return(sc->F);
    }

#if S7_DEBUGGING
#if HAVE_PTHREADS
  pthread_mutex_lock(&alloc_lock);
  gc(sc->orig_sc, __FUNCTION__, __LINE__);
  pthread_mutex_unlock(&alloc_lock);
#else
  gc(sc, __FUNCTION__, __LINE__);
#endif
#else
#if HAVE_PTHREADS
  pthread_mutex_lock(&alloc_lock);
  gc(sc->orig_sc);
  pthread_mutex_unlock(&alloc_lock);
#else
  gc(sc);
#endif
#endif
  
  return(sc->UNSPECIFIED);
}


s7_pointer s7_local_gc_protect(s7_pointer p)
{
  local_protect(p);
  return(p);
}


s7_pointer s7_local_gc_unprotect(s7_pointer p)
{
  local_unprotect(p);
  return(p);
}


s7_pointer s7_gc_on(s7_scheme *sc, bool on)
{
  (*(sc->gc_off)) = !on;
  return(s7_make_boolean(sc, on));
}




/* -------------------------------- stack -------------------------------- */

static void stack_reset(s7_scheme *sc) 
{ 
  sc->stack_top = 0;
} 


static void pop_stack(s7_scheme *sc) 
{ 
  s7_pointer *vel;
  sc->stack_top -= 4;
  vel = (s7_pointer *)(sc->stack->object.vector.elements + sc->stack_top);
  sc->code =  vel[0];
  sc->envir = vel[1];
  sc->args =  vel[2];
  sc->op =    (opcode_t)integer(vel[3]->object.number);
} 


static void push_stack(s7_scheme *sc, opcode_t op, s7_pointer args, s7_pointer code) 
{ 
  s7_pointer *vel;
  sc->stack_top += 4;
  
  if (sc->stack_top >= sc->stack_size)
    {
      int i, new_size;
      new_size = sc->stack_size * 2;
      sc->stack->object.vector.elements = (s7_pointer *)realloc(sc->stack->object.vector.elements, new_size * sizeof(s7_pointer));
      for (i = sc->stack_size; i < new_size; i++)
	vector_element(sc->stack, i) = sc->NIL;
      sc->stack->object.vector.length = new_size;
      sc->stack_size = new_size;
    }

  vel = (s7_pointer *)(sc->stack->object.vector.elements + sc->stack_top - 4);
  vel[0] = code;
  vel[1] = sc->envir;
  vel[2] = args;
  vel[3] = sc->small_ints[(int)op];
} 




/* -------------------------------- symbols -------------------------------- */

static int hash_fn(const char *key, int table_size) 
{ 
  /* I tried several other hash functions, but they gave about the same incidence of collisions */
  unsigned int hashed = 0; 
  const char *c; 
  for (c = key; *c; c++) 
    hashed = *c + hashed * 37;
  return(hashed % table_size); 
} 


#if S7_DEBUGGING
static s7_pointer g_builtin_hash(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_integer(sc, hash_fn(string_value(car(args)), SYMBOL_TABLE_SIZE)));
}
#endif


#if HAVE_PTHREADS
static pthread_mutex_t symtab_lock = PTHREAD_MUTEX_INITIALIZER;
#endif


static s7_pointer s7_make_permanent_string(const char *str);
static s7_pointer s7_permanent_cons(s7_pointer a, s7_pointer b, int type);

static s7_pointer symbol_table_add_by_name_at_location(s7_scheme *sc, const char *name, int location) 
{ 
  s7_pointer x, str; 
  
  str = s7_make_permanent_string(name);
  x = s7_permanent_cons(str, sc->NIL, T_SYMBOL | T_ATOM | T_SIMPLE | T_CONSTANT | T_DONT_COPY | T_ETERNAL);
  symbol_location(x) = location;

#if HAVE_PTHREADS
  pthread_mutex_lock(&symtab_lock);
#endif

  vector_element(sc->symbol_table, location) = s7_permanent_cons(x, 
								 vector_element(sc->symbol_table, location), 
								 T_PAIR | T_ATOM | T_SIMPLE | T_CONSTANT | T_IMMUTABLE | T_DONT_COPY);

#if HAVE_PTHREADS
  pthread_mutex_unlock(&symtab_lock);
#endif
  
  return(x); 
} 


static s7_pointer symbol_table_add_by_name(s7_scheme *sc, const char *name) 
{
  return(symbol_table_add_by_name_at_location(sc, name, hash_fn(name, vector_length(sc->symbol_table)))); 
}


static  s7_pointer symbol_table_find_by_name(s7_scheme *sc, const char *name, int location) 
{ 
  s7_pointer x; 

#if HAVE_PTHREADS
  pthread_mutex_lock(&symtab_lock);
#endif

  for (x = vector_element(sc->symbol_table, location); x != sc->NIL; x = cdr(x)) 
    { 
      const char *s; 
      s = s7_symbol_name(car(x)); 
      if ((s) && (STRCMP(name, s) == 0))
	{
#if HAVE_PTHREADS
	  pthread_mutex_unlock(&symtab_lock);
#endif
	  return(car(x)); 
	}
    } 
#if HAVE_PTHREADS
  pthread_mutex_unlock(&symtab_lock);
#endif

  return(sc->NIL); 
} 


static s7_pointer g_symbol_table(s7_scheme *sc, s7_pointer args)
{
  #define H_symbol_table "(symbol-table) returns the symbol table (a vector)"
  return(sc->symbol_table);
}


void s7_for_each_symbol_name(s7_scheme *sc, bool (*symbol_func)(const char *symbol_name, void *data), void *data)
{
  int i; 
  s7_pointer x; 

#if HAVE_PTHREADS
  pthread_mutex_lock(&symtab_lock);
#endif

  for (i = 0; i < vector_length(sc->symbol_table); i++) 
    for (x  = vector_element(sc->symbol_table, i); x != sc->NIL; x = cdr(x)) 
      if (symbol_func(s7_symbol_name(car(x)), data))
	{
#if HAVE_PTHREADS
	  pthread_mutex_unlock(&symtab_lock);
#endif
	  return;
	}

#if HAVE_PTHREADS
  pthread_mutex_unlock(&symtab_lock);
#endif
}


void s7_for_each_symbol(s7_scheme *sc, bool (*symbol_func)(const char *symbol_name, s7_pointer value, void *data), void *data)
{
  int i; 
  s7_pointer x; 

#if HAVE_PTHREADS
  pthread_mutex_lock(&symtab_lock);
#endif

  for (i = 0; i < vector_length(sc->symbol_table); i++) 
    for (x  = vector_element(sc->symbol_table, i); x != sc->NIL; x = cdr(x)) 
      if (symbol_func(s7_symbol_name(car(x)), cdr(x), data))
	{
#if HAVE_PTHREADS
	  pthread_mutex_unlock(&symtab_lock);
#endif
	  return;
	}

#if HAVE_PTHREADS
  pthread_mutex_unlock(&symtab_lock);
#endif
}


s7_pointer s7_make_symbol(s7_scheme *sc, const char *name) 
{ 
  s7_pointer x; 
  int location;
  location = hash_fn(name, vector_length(sc->symbol_table)); 
  x = symbol_table_find_by_name(sc, name, location); 
  if (x != sc->NIL) 
    return(x); 
  return(symbol_table_add_by_name_at_location(sc, name, location)); 
} 


s7_pointer s7_gensym(s7_scheme *sc, const char *prefix)
{ 
  char *name;
  int len, location;
  s7_pointer x;
  
  len = safe_strlen(prefix) + 32;
  name = (char *)calloc(len, sizeof(char));
  
  for(; (*(sc->gensym_counter)) < LONG_MAX; ) 
    { 
      snprintf(name, len, "%s-%ld", prefix, (*(sc->gensym_counter))++); 
      location = hash_fn(name, vector_length(sc->symbol_table)); 
      x = symbol_table_find_by_name(sc, name, location); 
      if (x != sc->NIL)
	{
	  if (s7_symbol_value(sc, x) != sc->UNDEFINED)
	    continue; 
	  free(name);
	  return(x); 
	}
      
      x = symbol_table_add_by_name_at_location(sc, name, location); 
      free(name);
      return(x); 
    } 
  
  free(name);
  return(sc->NIL); 
} 


static s7_pointer g_gensym(s7_scheme *sc, s7_pointer args) 
{
  #define H_gensym "(gensym :optional prefix) returns a new (or at least an un-used) symbol"
  if (is_pair(args))
    {
      if (!s7_is_string(car(args)))
	return(s7_wrong_type_arg_error(sc, "gensym", 1, car(args), "a string"));
      return(s7_gensym(sc, string_value(car(args))));
    }
  return(s7_gensym(sc, "gensym"));
}


s7_pointer s7_name_to_value(s7_scheme *sc, const char *name)
{
  return(s7_symbol_value(sc, s7_make_symbol(sc, name)));
}


bool s7_is_symbol(s7_pointer p)   
{ 
  return(type(p) == T_SYMBOL);
}


static s7_pointer g_is_symbol(s7_scheme *sc, s7_pointer args)
{
  #define H_is_symbol "(symbol? obj) returns #t if obj is a symbol"
  return(make_boolean(sc, s7_is_symbol(car(args))));
}


const char *s7_symbol_name(s7_pointer p)   
{ 
  return(string_value(car(p)));
}


static s7_pointer g_symbol_to_string(s7_scheme *sc, s7_pointer args)
{
  #define H_symbol_to_string "(symbol->string sym) returns sym converted to a string"
  if (!s7_is_symbol(car(args)))
    return(s7_wrong_type_arg_error(sc, "symbol->string", 1, car(args), "a symbol"));
  return(s7_set_immutable(s7_make_string(sc, s7_symbol_name(car(args)))));
}


static s7_pointer g_string_to_symbol(s7_scheme *sc, s7_pointer args)
{
  #define H_string_to_symbol "(string->symbol str) returns str converted to a symbol"
  if (!s7_is_string(car(args)))
    return(s7_wrong_type_arg_error(sc, "string->symbol", 1, car(args), "a string"));
  return(s7_make_symbol(sc, string_value(car(args))));
}


void s7_provide(s7_scheme *sc, const char *feature)
{
  char *expr;
  int len;
  len = safe_strlen(feature) + 64;
  expr = (char *)calloc(len, sizeof(char));
  snprintf(expr, len, "(set! *features* (cons '%s *features*))", feature);
  s7_eval_c_string(sc, expr);
  free(expr);
}





/* -------------------------------- environments -------------------------------- */

static s7_pointer new_frame_in_env(s7_scheme *sc, s7_pointer old_env) 
{ 
  return(s7_immutable_cons(sc, sc->NIL, old_env));
} 


static s7_pointer add_to_environment(s7_scheme *sc, s7_pointer env, s7_pointer variable, s7_pointer value) 
{ 
  s7_pointer slot;

  ASSERT_IS_OBJECT(variable, "new slot variable");
  ASSERT_IS_OBJECT(value, "new slot value");
  ASSERT_IS_OBJECT(env, "new slot env");
  ASSERT_IS_OBJECT(car(env), "new slot car env");

  slot = s7_immutable_cons(sc, variable, value); 
  if (s7_is_vector(car(env))) 
    vector_element(car(env), symbol_location(variable)) = s7_immutable_cons(sc, slot, vector_element(car(env), symbol_location(variable))); 
  else car(env) = s7_immutable_cons(sc, slot, car(env));
  return(slot);
} 


static s7_pointer s7_find_symbol_in_environment(s7_scheme *sc, s7_pointer env, s7_pointer hdl, bool all) 
{ 
  s7_pointer x, y;
  /* this is a list (of alists, each representing a frame) ending with a vector (the global environment) */

  for (x = env; is_pair(x); x = cdr(x)) 
    { 
      if (s7_is_vector(car(x))) 
	y = vector_element(car(x), symbol_location(hdl));
      else y = car(x); 
      
      for ( ; is_pair(y); y = cdr(y)) 
	if (caar(y) == hdl)
	  return(car(y));

      if (!all) 
	return(sc->NIL); 
    } 
  return(sc->NIL); 
} 


static s7_pointer add_to_current_environment(s7_scheme *sc, s7_pointer variable, s7_pointer value) 
{ 
  ASSERT_IS_OBJECT(sc->envir, "new slot envir");

  return(add_to_environment(sc, sc->envir, variable, value)); 
} 


#define symbol_value(Sym) cdr(Sym)
#define set_symbol_value(Sym, Val) cdr(Sym) = (Val)


s7_pointer s7_symbol_value(s7_scheme *sc, s7_pointer sym) /* was searching just the global environment? */
{
  s7_pointer x;
  x = s7_find_symbol_in_environment(sc, sc->envir, sym, true);
  if (x != sc->NIL)
    return(symbol_value(x));
  return(sc->UNDEFINED);
}


s7_pointer s7_symbol_local_value(s7_scheme *sc, s7_pointer sym, s7_pointer local_env)
{
  s7_pointer x;
  x = s7_find_symbol_in_environment(sc, local_env, sym, true);
  if (x != sc->NIL)
    return(symbol_value(x));
  return(s7_symbol_value(sc, sym)); /* try sc->envir */
}


static s7_pointer g_symbol_to_value(s7_scheme *sc, s7_pointer args)
{
  #define H_symbol_to_value "(symbol->value sym) returns the current binding of (value associated with) the symbol sym"
  if (!s7_is_symbol(car(args)))
    return(s7_wrong_type_arg_error(sc, "symbol->value", 1, car(args), "a symbol"));
  return(s7_symbol_value(sc, car(args)));
}


s7_pointer s7_symbol_set_value(s7_scheme *sc, s7_pointer sym, s7_pointer val)
{
  s7_pointer x;
  x = s7_find_symbol_in_environment(sc, sc->envir, sym, true);
  if (x != sc->NIL)
    set_symbol_value(x, val);
  return(val);
}


static bool lambda_star_argument_set_value(s7_scheme *sc, s7_pointer sym, s7_pointer val)
{
  s7_pointer x;

  for (x = car(sc->envir) /* presumably the arglist */; is_pair(x); x = cdr(x))
    if (caar(x) == sym)
      {
	/* car(x) is our binding (symbol value) */
	cdar(x) = val;
	return(true);
      }
  return(false);
}


static s7_pointer lambda_star_argument_default_value(s7_scheme *sc, s7_pointer val)
{
  s7_pointer x;

  if (s7_is_symbol(val))
    {
      x = s7_find_symbol_in_environment(sc, sc->envir, val, true);
      if (x != sc->NIL) 
	return(symbol_value(x));
    }
  if ((is_pair(val)) &&
      (car(val) == sc->QUOTE))
    return(cadr(val));
  return(val);
}


static s7_pointer g_global_environment(s7_scheme *sc, s7_pointer p)
{
  #define H_global_environment "(global-environment) returns the top-level definitions (symbol bindings)"
  return(sc->global_env);
}


#if HAVE_PTHREADS
static s7_pointer g_is_thread(s7_scheme *sc, s7_pointer args);
static s7_pointer thread_environment(s7_scheme *sc, s7_pointer obj);
#endif

static s7_pointer g_current_environment(s7_scheme *sc, s7_pointer args)
{
  #define H_current_environment "(current-environment :optional thread) returns the current definitions (symbol bindings)"
#if HAVE_PTHREADS
  if (args != sc->NIL)
    {
      if (g_is_thread(sc, args) == sc->F)
	return(s7_wrong_type_arg_error(sc, "current-environment", 1, car(args), "a thread object"));
      return(thread_environment(sc, car(args)));
    }
#endif
  return(sc->envir);
}


#define closure_source(Obj) car(Obj)

static s7_pointer make_closure(s7_scheme *sc, s7_pointer c, s7_pointer e, int type) 
{
  /* c is code. e is environment */
     
  s7_pointer x = new_cell(sc);
  
  set_type(x, type | T_PROCEDURE);
  
  ASSERT_IS_OBJECT(c, "closure code");
  ASSERT_IS_OBJECT(e, "closure env");
  
  car(x) = c;
  cdr(x) = e;
  return(x);
}


s7_pointer s7_make_closure(s7_scheme *sc, s7_pointer c, s7_pointer e)
{
  return(make_closure(sc, c, e, T_CLOSURE));
}


s7_pointer s7_make_closure_star(s7_scheme *sc, s7_pointer c, s7_pointer e) 
{
  return(make_closure(sc, c, e, T_CLOSURE_STAR));
}


s7_pointer s7_global_environment(s7_scheme *sc) 
{
  return(sc->global_env);
}


s7_pointer s7_current_environment(s7_scheme *sc) 
{
  return(sc->envir);
}


static s7_pointer g_is_defined(s7_scheme *sc, s7_pointer args)
{
  #define H_is_defined "(defined? obj :optional env) returns #t if obj has a binding (a value) in the environment env"
  
  if (!s7_is_symbol(car(args)))
    return(s7_wrong_type_arg_error(sc, "defined?", 1, car(args), "a symbol"));
  
  if (is_syntax(car(args)))
    return(sc->T);
  
  if (cdr(args) != sc->NIL)
    {
      if (!is_pair(cadr(args)))
	return(s7_wrong_type_arg_error(sc, "defined?", 2, cadr(args), "an enivronment"));
      sc->x = cadr(args);
    }
  else sc->x = sc->envir;
  return(make_boolean(sc, s7_find_symbol_in_environment(sc, sc->x, car(args), true) != sc->NIL));
}


void s7_define(s7_scheme *sc, s7_pointer envir, s7_pointer symbol, s7_pointer value) 
{
  s7_pointer x;
  x = s7_find_symbol_in_environment(sc, envir, symbol, false);
  if (x != sc->NIL) 
    set_symbol_value(x, value); 
  else add_to_environment(sc, envir, symbol, value); 
}


void s7_define_variable(s7_scheme *sc, const char *name, s7_pointer value)
{
  s7_pointer sym;
  
  sym = s7_make_symbol(sc, name);
  s7_define(sc, s7_global_environment(sc), sym, value);
}


void s7_define_constant(s7_scheme *sc, const char *name, s7_pointer value)
{
  s7_pointer x, sym;
  
  sym = s7_make_symbol(sc, name);
  s7_define(sc, s7_global_environment(sc), sym, value);

  x = s7_find_symbol_in_environment(sc, s7_global_environment(sc), sym, false);
  s7_set_immutable(car(x));
}


static s7_pointer s7_search_environment(s7_scheme *sc, s7_pointer env, bool (*searcher)(s7_scheme *sc, s7_pointer binding, s7_pointer data), s7_pointer data)
{ 
  int i, len;
  s7_pointer x, y, vec; 
  for (x = env; (x != sc->NIL) && (!s7_is_vector(car(x))); x = cdr(x)) 
    for (y = car(x); y != sc->NIL; y = cdr(y)) 
      if (searcher(sc, car(y), data))
	return(car(y));
  
  if (s7_is_vector(car(x)))
    {
      vec = car(x);
      len = vector_length(vec);
      for (i = 0; i < len; i++)
	if (vector_element(vec, i) != sc->NIL)
	  for (y = vector_element(vec, i); y != sc->NIL; y = cdr(y)) 
	    if (searcher(sc, car(y), data))
	      return(car(y));
    }
  return(sc->F); 
} 


static bool find_value(s7_scheme *sc, s7_pointer binding, s7_pointer val)
{
  return(cdr(binding) == val);
}


static s7_pointer s7_find_value_in_environment(s7_scheme *sc, s7_pointer val)
{
  return(s7_search_environment(sc, sc->envir, find_value, val));
}



/* -------------------------------- continuations and gotos -------------------------------- */

bool s7_is_continuation(s7_pointer p)    
{ 
  return(type(p) == T_CONTINUATION);
}


static s7_pointer g_is_continuation(s7_scheme *sc, s7_pointer args)
{
  #define H_is_continuation "(continuation? obj) returns #t if obj is a continuation"
  return(make_boolean(sc, (s7_is_continuation(car(args))) ||
		          (is_goto(car(args)))));
}


static s7_pointer copy_list(s7_scheme *sc, s7_pointer lst)
{
  if (lst == sc->NIL)
    return(sc->NIL);
  return(s7_cons(sc, car(lst), copy_list(sc, cdr(lst))));
}


static s7_pointer copy_object(s7_scheme *sc, s7_pointer obj)
{
  s7_pointer nobj;

  if (dont_copy(obj))
    return(obj);
  
  nobj = new_cell(sc);
  memcpy((void *)nobj, (void *)obj, sizeof(s7_cell));
  
  car(nobj) = copy_object(sc, car(obj));
  if ((is_closure(obj)) ||
      (is_closure_star(obj)) ||
      (is_macro(obj)) || 
      (is_promise(obj)) ||
      (s7_is_function(obj)))
    cdr(nobj) = cdr(obj); /* the environment of the closure */
  else cdr(nobj) = copy_object(sc, cdr(obj));
  
  return(nobj);
}


static s7_pointer copy_stack(s7_scheme *sc, s7_pointer old_v, int top)
{
  int i, len;
  s7_pointer new_v;
  s7_pointer *nv, *ov;
  len = vector_length(old_v);
  new_v = s7_make_vector(sc, len);

  nv = new_v->object.vector.elements;
  ov = old_v->object.vector.elements;
  
  (*(sc->gc_off)) = true;
  
  for (i = 0; i < top; i += 4)
    {
      nv[i + 0] = copy_object(sc, ov[i + 0]); /* code */
      nv[i + 1] = ov[i + 1];                  /* environment pointer */
      nv[i + 2] = copy_list(sc, ov[i + 2]);   /* args (copy is needed -- see s7test.scm) */
      nv[i + 3] = ov[i + 3];                  /* op (constant int) */
    }
  
  (*(sc->gc_off)) = false;
  
  return(new_v);
}


static s7_pointer s7_make_goto(s7_scheme *sc) 
{
  s7_pointer x;
  x = new_cell(sc);
  set_type(x, T_ATOM | T_GOTO | T_SIMPLE | T_DONT_COPY | T_PROCEDURE);
  x->object.goto_loc = sc->stack_top;
  return(x);
}


static s7_pointer s7_make_continuation(s7_scheme *sc) 
{
  continuation *c;
  s7_pointer x = new_cell(sc);
  set_type(x, T_CONTINUATION | T_FINALIZABLE | T_DONT_COPY | T_PROCEDURE);
  c = (continuation *)calloc(1, sizeof(continuation));
  
  /* save current state */
  c->cc_stack_top = sc->stack_top;
  c->cc_stack_size = sc->stack_size;
  c->cc_stack = copy_stack(sc, sc->stack, sc->stack_top);
  x->object.cc = c;
  
  return(x);
}


static void check_for_dynamic_winds(s7_scheme *sc, continuation *c)
{
  int i, s_base = 0, c_base = -1;
  
  for (i = sc->stack_top - 1; i > 0; i -= 4)
    {
      s7_pointer x;
      if ((opcode_t)s7_integer(vector_element(sc->stack, i)) == OP_DYNAMIC_WIND)
	{
	  int j;
	  x = vector_element(sc->stack, i - 3);
	  for (j = 3; j < c->cc_stack_top; j += 4)
	    if (((opcode_t)s7_integer(vector_element(c->cc_stack, j)) == OP_DYNAMIC_WIND) &&
		(x == vector_element(c->cc_stack, j - 3)))
	      {
		s_base = i;
		c_base = j;
		break;
	      }
	  
	  if (s_base != 0)
	    break;	  
	  
	  if (dynamic_wind_state(x) == DWIND_BODY)
	    s7_call(sc, dynamic_wind_out(x), sc->NIL);
	}
    }
  
  for (i = c_base + 4; i < c->cc_stack_top; i += 4)
    if ((opcode_t)s7_integer(vector_element(c->cc_stack, i)) == OP_DYNAMIC_WIND)
      {
	s7_pointer x;
	x = vector_element(c->cc_stack, i - 3);
	s7_call(sc, dynamic_wind_in(x), sc->NIL);
	dynamic_wind_state(x) = DWIND_BODY;
      }
}


static s7_pointer s7_call_continuation(s7_scheme *sc, s7_pointer p)
{
  continuation *c;
  c = p->object.cc;
  check_for_dynamic_winds(sc, c);
  sc->stack = copy_stack(sc, c->cc_stack, c->cc_stack_top);
  sc->stack_size = c->cc_stack_size;
  sc->stack_top = c->cc_stack_top;
  return(sc->value);
}


static s7_pointer g_call_cc(s7_scheme *sc, s7_pointer args)
{
  #define H_call_cc "(call-with-current-continuation ...) needs more than a one sentence explanation"
  sc->code = car(args);
  sc->args = make_list_1(sc, s7_make_continuation(sc));
  push_stack(sc, OP_APPLY, sc->args, sc->code);
  return(sc->NIL);
}


static s7_pointer g_call_with_exit(s7_scheme *sc, s7_pointer args)
{
  #define H_call_with_exit "(call-with-exit ...) is a simplified call/cc"
  
  /* (call-with-exit (lambda (return) ...)) */
  
  sc->code = car(args);                              /* the lambda form */
  sc->args = make_list_1(sc, s7_make_goto(sc));      /*   the argument to the lambda (the goto = "return" above) */
  push_stack(sc, OP_APPLY, sc->args, sc->code);      /* apply looks at sc->code to decide what to do (it will see the lambda) */
  
  /* if the lambda body calls the argument as a function, 
   *   it is applied to its arguments, apply notices that it is a goto, and...
   *   
   *      sc->stack_top = (sc->code)->object.goto_loc;           
   *      s_pop(sc, sc->args != sc->NIL ? car(sc->args) : sc->NIL);
   * 
   *   which jumps to the point of the goto returning car(args)
   */
  
  return(sc->NIL);
}





/* -------------------------------- numbers -------------------------------- */


#define s7_Int_abs(x) (x >= 0 ? x : -x)
/* can't use abs even in gcc -- it doesn't work with long long ints! */
#define s7_Double_abs(x) fabs(x)


#if WITH_COMPLEX

#if __cplusplus
  #define _Complex_I (complex<s7_Double>(0.0, 1.0))
  #define creal(x) Real(x)
  #define cimag(x) Imag(x)
  #define carg(x) arg(x)
  #define cabs(x) abs(x)
  #define csqrt(x) sqrt(x)
  #define cpow(x, y) pow(x, y)
  #define s7_Int_pow(x, y) (s7_Int)pow((s7_Double)(x), (s7_Double)(y))
  #define clog(x) log(x)
  #define cexp(x) exp(x)
  #define csin(x) sin(x)
  #define ccos(x) cos(x)
  #define csinh(x) sinh(x)
  #define ccosh(x) cosh(x)
#else
  typedef double complex s7_Complex;
  #define s7_Int_pow(x, y) (s7_Int)pow(x, y)
#endif

/* Trigonometric functions. FreeBSD's math library does not include the complex form of the trig funcs. */ 

/* FreeBSD supplies cabs carg cimag creal conj csqrt, so can we assume those exist if complex.h exists?
 */

#if 0
static s7_Double carg(s7_Complex z)
{ 
  return(atan2(cimag(z), creal(z))); 
} 


static s7_Double cabs(s7_Complex z) 
{ 
  return(hypot(creal(z), cimag(z))); 
} 


static s7_Complex conj(s7_Complex z) 
{ 
  return(~z); 
} 


static s7_Complex csqrt(s7_Complex z) 
{ 
  if (cimag(z) < 0.0) 
    return(conj(csqrt(conj(z)))); 
  else 
    { 
      s7_Double r = cabs(z); 
      s7_Double x = creal(z); 
      
      return(sqrt((r + x) / 2.0) + sqrt((r - x) / 2.0) * _Complex_I); 
    } 
} 
#endif


#if (!HAVE_COMPLEX_TRIG)

#if (!__cplusplus)
static s7_Complex csin(s7_Complex z) 
{ 
  return(sin(creal(z)) * cosh(cimag(z)) + (cos(creal(z)) * sinh(cimag(z))) * _Complex_I); 
} 


static s7_Complex ccos(s7_Complex z) 
{ 
  return(cos(creal(z)) * cosh(cimag(z)) + (-sin(creal(z)) * sinh(cimag(z))) * _Complex_I); 
} 


static s7_Complex csinh(s7_Complex z) 
{ 
  return(sinh(creal(z)) * cos(cimag(z)) + (cosh(creal(z)) * sin(cimag(z))) * _Complex_I); 
} 


static s7_Complex ccosh(s7_Complex z) 
{ 
  return(cosh(creal(z)) * cos(cimag(z)) + (sinh(creal(z)) * sin(cimag(z))) * _Complex_I); 
} 
#endif


static s7_Complex ctan(s7_Complex z) 
{ 
  return(csin(z) / ccos(z)); 
} 


static s7_Complex ctanh(s7_Complex z) 
{ 
  return(csinh(z) / ccosh(z)); 
} 


#if (!__cplusplus)
static s7_Complex cexp(s7_Complex z) 
{ 
  return(exp(creal(z)) * cos(cimag(z)) + (exp(creal(z)) * sin(cimag(z))) * _Complex_I); 
} 


static s7_Complex clog(s7_Complex z) 
{ 
  return(log(s7_Double_abs(cabs(z))) + carg(z) * _Complex_I); 
} 


static s7_Complex cpow(s7_Complex x, s7_Complex y) 
{ 
  s7_Double r = cabs(x); 
  s7_Double theta = carg(x); 
  s7_Double yre = creal(y); 
  s7_Double yim = cimag(y); 
  s7_Double nr = exp(yre * log(r) - yim * theta); 
  s7_Double ntheta = yre * theta + yim * log(r); 
  
  return(nr * cos(ntheta) + (nr * sin(ntheta)) * _Complex_I); /* make-polar */ 
} 
#endif


static s7_Complex casin(s7_Complex z) 
{ 
  return(-_Complex_I * clog(_Complex_I * z + csqrt(1.0 - z * z))); 
} 


static s7_Complex cacos(s7_Complex z) 
{ 
  return(-_Complex_I * clog(z + _Complex_I * csqrt(1.0 - z * z))); 
} 


static s7_Complex catan(s7_Complex z) 
{ 
  return(_Complex_I * clog((_Complex_I + z) / (_Complex_I - z)) / 2.0); 
} 


static s7_Complex casinh(s7_Complex z) 
{ 
  return(clog(z + csqrt(1.0 + z * z))); 
} 


static s7_Complex cacosh(s7_Complex z) 
{ 
  return(clog(z + csqrt(z * z - 1.0))); 
} 


static s7_Complex catanh(s7_Complex z) 
{ 
  return(clog((1.0 + z) / (1.0 - z)) / 2.0); 
} 
#endif

#else
/* not WITH_COMPLEX */
  typedef double s7_Complex;
  #define s7_Int_pow(x, y) (s7_Int)pow(x, y)
  #define _Complex_I 1
  #define creal(x) x
  #define cimag(x) x
  #define csin(x) sin(x)
  #define casin(x) x
  #define ccos(x) cos(x)
  #define cacos(x) x
  #define ctan(x) x
  #define catan(x) x
  #define csinh(x) x
  #define casinh(x) x
  #define ccosh(x) x
  #define cacosh(x) x
  #define ctanh(x) x
  #define catanh(x) x
  #define cexp(x) exp(x)
  #define cpow(x, y) pow(x, y)
  #define clog(x) log(x)
  #define csqrt(x) sqrt(x)
  #define conj(x) x
#endif



bool s7_is_number(s7_pointer p)
{
  return(type(p) == T_NUMBER);
}


bool s7_is_integer(s7_pointer p) 
{ 
  if (!(s7_is_number(p)))
    return(false);
  
  return(object_number_type(p) == NUM_INT);
}


bool s7_is_real(s7_pointer p) 
{ 
  if (!(s7_is_number(p)))
    return(false);
  
  return(object_number_type(p) < NUM_COMPLEX);
}


bool s7_is_rational(s7_pointer p)
{
  if (!(s7_is_number(p)))
    return(false);
  
  return(object_number_type(p) <= NUM_RATIO);
}


bool s7_is_ratio(s7_pointer p)
{
  if (!(s7_is_number(p)))
    return(false);
  
  return(object_number_type(p) == NUM_RATIO);
}


bool s7_is_complex(s7_pointer p)
{
  return(s7_is_number(p));
}


bool s7_is_exact(s7_pointer p)
{
  return(s7_is_rational(p));
}


bool s7_is_inexact(s7_pointer p)
{
  return(!s7_is_rational(p));
}


static num nvalue(s7_pointer p)       
{ 
  return((p)->object.number);
}


static double default_rationalize_error = 1.0e-12;

static s7_Int c_mod(s7_Int x, s7_Int y)
{
  s7_Int z;
  if (y == 0) return(x); /* else arithmetic exception */
  z = x % y;
  if (((y < 0) && (z > 0)) ||
      ((y > 0) && (z < 0)))
    return(z + y);
  return(z);
}


static s7_Int c_gcd(s7_Int u, s7_Int v)
{
  s7_Int a, b, temp;
  
  a = s7_Int_abs(u);
  b = s7_Int_abs(v);
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


static s7_Int c_lcm(s7_Int a, s7_Int b)
{
  if ((a == 0) || (b == 0)) return(0);
  if (a < 0) a = -a;
  if (b < 0) b = -b;
  return((a / c_gcd(a, b)) * b);
}


static bool c_rationalize(s7_Double ux, s7_Double error, s7_Int *numer, s7_Int *denom)
{
  s7_Int a1 = 0, a2 = 1, b1 = 1, b2 = 0, tt = 1, a = 0, b = 0, ctr, int_part = 0;
  bool neg = false;
  s7_Double x;
  
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
      int_part = (s7_Int)floor(ux);
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
  for (ctr = 0; ctr < 100; ctr++)
    {
      a = a2 + a1 * tt;
      b = b2 + b1 * tt;
      
      if (s7_Double_abs(ux - (s7_Double)a / (s7_Double)b) < error)
	{
	  a += (b * int_part);
	  (*numer) = (neg) ? -a : a;
	  (*denom) = b;
	  return(true);
	}
      if (x == tt)
	return(false);
      
      x = 1.0 / (x - tt);
      tt = (s7_Int)floor(x);
      a2 = a1;
      b2 = b1;
      a1 = a;
      b1 = b;
    }
  return(false);
}


#if 0
/* this version follows the (silly) Scheme spec, but it's ugly */
static bool c_rationalize(s7_Double ux, s7_Double error, s7_Int *n, s7_Int *d)
{
  s7_Int numer, denom, lim, sign = 0;
  lim = 1.0 / error;
  if (ux < 0.0)
    {
      ux = -ux;
      sign = 1;
    }
  
  for (denom = 1; denom <= lim; denom++)
    {
      numer = (s7_Int)floor(ux * denom);
      if ((((s7_Double)numer / (s7_Double)denom) + error) >= ux)
	{
	  if (sign)
	    (*n) = -numer;
	  else (*n) = numer;
	  (*d) = denom;
	  return(true);
	}
      numer++;
      if ((((s7_Double)numer / (s7_Double)denom) - error) <= ux)
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
#endif


s7_pointer s7_rationalize(s7_scheme *sc, s7_Double x, s7_Double error)
{
  s7_Int numer = 0, denom = 1;
  if (c_rationalize(x, error, &numer, &denom))
    return(s7_make_ratio(sc, numer, denom));
#if S7_DEBUGGING
  else fprintf(stderr, "rationalize(%lf, %lf) did not converge?\n", x, error);
#endif
  return(s7_make_real(sc, x));
}


static s7_Double num_to_real(num n)
{
  if (n.type >= NUM_REAL)
    return(real(n));
  if (n.type == NUM_INT)
    return((s7_Double)integer(n));
  return(fraction(n));
}


s7_Double s7_number_to_real(s7_pointer x)
{
  switch (object_number_type(x))
    {
    case NUM_INT:   return((s7_Double)s7_integer(x));
    case NUM_RATIO: return((s7_Double)s7_numerator(x) / (s7_Double)s7_denominator(x));
    case NUM_REAL:
    case NUM_REAL2: return(s7_real(x));
    default:        return(s7_real_part(x));
    }
}


static s7_Int num_to_numerator(num n)
{
  if (n.type == NUM_RATIO)
    return(numerator(n));
  return(integer(n));
}


static s7_Int num_to_denominator(num n)
{
  if (n.type == NUM_RATIO)
    return(denominator(n));
  return(1);
}


static s7_Double num_to_real_part(num n)
{
  switch (n.type)
    {
    case NUM_INT:   return((s7_Double)integer(n));
    case NUM_RATIO: return(fraction(n));
    case NUM_REAL:
    case NUM_REAL2: return(real(n));
    default:        return(real_part(n));
    }
}


static s7_Double num_to_imag_part(num n)
{
  if (n.type >= NUM_COMPLEX)
    return(imag_part(n));
  return(0.0);
}


static num make_ratio(s7_Int numer, s7_Int denom)
{
  num ret;
  s7_Int divisor;

  if (denom == 0)
    {
      fprintf(stderr, "%lld/%lld currently just returns %lld in s7", (long long int)numer, (long long int)denom, (long long int)numer);
      /* I can't decide how to handle this */
      ret.type = NUM_INT;
      integer(ret) = numer;
      return(ret);
    }

  if (numer == 0)
    {
      ret.type = NUM_INT;
      integer(ret) = 0;
      return(ret);
    }
  
  if (denom < 0)
    {
      numer = -numer;
      denom = -denom;
    }
  
  divisor = c_gcd(numer, denom);
  if (divisor != 1)
    {
      numer /= divisor;
      denom /= divisor;
    }
  
  if (denom == 1)
    {
      ret.type = NUM_INT;
      integer(ret) = numer;
    }
  else
    {
      ret.type = NUM_RATIO;
      numerator(ret) = numer;
      denominator(ret) = denom;
    }
  return(ret);
}


static num make_complex(s7_Double rl, s7_Double im)
{
  num ret;
  if (im == 0.0)
    {
      ret.type = NUM_REAL;
      real(ret) = rl;
    }
  else
    {
      ret.type = NUM_COMPLEX;
      real_part(ret) = rl;
      imag_part(ret) = im;
    }
  return(ret);
}


s7_pointer s7_make_integer(s7_scheme *sc, s7_Int n) 
{
  s7_pointer x;
  x = new_cell(sc);
  set_type(x, T_NUMBER | T_ATOM | T_SIMPLE | T_DONT_COPY);
  
  x->object.number.type = NUM_INT;
  integer(x->object.number) = n;
  
  return(x);
}


s7_pointer s7_make_real(s7_scheme *sc, s7_Double n) 
{
  s7_pointer x = new_cell(sc);
  set_type(x, T_NUMBER | T_ATOM | T_SIMPLE | T_DONT_COPY);
  
  x->object.number.type = NUM_REAL;
  real(x->object.number) = n;
  
  return(x);
}


s7_pointer s7_make_complex(s7_scheme *sc, s7_Double a, s7_Double b)
{
  s7_pointer x;
  x = new_cell(sc);
  set_type(x, T_NUMBER | T_ATOM | T_SIMPLE | T_DONT_COPY);
  if (b == 0.0)
    {
      x->object.number.type = NUM_REAL;
      real(x->object.number) = a;
    }
  else
    {
      x->object.number.type = NUM_COMPLEX;
      real_part(x->object.number) = a;
      imag_part(x->object.number) = b;
    }
  return(x);
}


s7_pointer s7_make_ratio(s7_scheme *sc, s7_Int a, s7_Int b)
{
  /* make_number calls us, so we can't call it as a convenience! */
  
  num ret;
  s7_pointer x;

  if (b == 0)
    return(sc->F); /* ?? */

  x = new_cell(sc);
  set_type(x, T_NUMBER | T_ATOM | T_SIMPLE | T_DONT_COPY);

  ret = make_ratio(a, b);
  
  x->object.number.type = ret.type;
  if (ret.type == NUM_INT)
    integer(x->object.number) = numerator(ret);
  else
    {
      numerator(x->object.number) = numerator(ret);
      denominator(x->object.number) = denominator(ret);
    }
  
  return(x);
}


static s7_pointer make_number(s7_scheme *sc, num n) 
{
  switch (num_type(n))
    {
    case NUM_INT:     return(s7_make_integer(sc, integer(n)));
    case NUM_RATIO:   return(s7_make_ratio(sc, numerator(n), denominator(n)));
    case NUM_REAL2:
    case NUM_REAL:    return(s7_make_real(sc, real(n)));
    default:          return(s7_make_complex(sc, real_part(n), imag_part(n)));
    }
}


s7_Int s7_numerator(s7_pointer x)
{
  if (x->object.number.type == NUM_RATIO)
    return(numerator(x->object.number));
  return(integer(x->object.number));
}


s7_Int s7_denominator(s7_pointer x)
{
  if (x->object.number.type == NUM_RATIO)
    return(denominator(x->object.number));
  return(1);
}


s7_Double s7_real_part(s7_pointer x)
{
  return(num_to_real_part(x->object.number));
}


s7_Double s7_imag_part(s7_pointer x)
{
  return(num_to_imag_part(x->object.number));
}


s7_Int s7_integer(s7_pointer p)
{
  return(integer(p->object.number));
}


s7_Double s7_real(s7_pointer p)
{
  return(real(p->object.number));
}


static s7_Complex s7_complex(s7_pointer p)
{
  return(num_to_real_part(p->object.number) + num_to_imag_part(p->object.number) * _Complex_I);
}


static s7_pointer s7_from_c_complex(s7_scheme *sc, s7_Complex z)
{
  return(s7_make_complex(sc, creal(z), cimag(z)));
}


static num num_max(num a, num b) 
{
  num ret;
  ret.type = a.type | b.type;
  
  switch (num_type(ret))
    {
    case NUM_INT: 
      if (integer(a) >= integer(b))
	integer(ret) = integer(a);
      else integer(ret) = integer(b);
      break;
      
    case NUM_RATIO:
      if (num_to_real(a) >= num_to_real(b))
	ret = make_ratio(num_to_numerator(a), num_to_denominator(a));
      else ret = make_ratio(num_to_numerator(b), num_to_denominator(b));
      break;
      
    default:
      if (num_to_real(a) >= num_to_real(b))
	real(ret) = num_to_real(a);
      else real(ret) = num_to_real(b);
      break;
    }
  
  return(ret);
}


static num num_min(num a, num b) 
{
  num ret;
  ret.type = a.type | b.type;
  
  switch (num_type(ret))
    {
    case NUM_INT: 
      if (integer(a) < integer(b))
	integer(ret) = integer(a);
      else integer(ret) = integer(b);
      break;
      
    case NUM_RATIO:
      if (num_to_real(a) < num_to_real(b))
	ret = make_ratio(num_to_numerator(a), num_to_denominator(a));
      else ret = make_ratio(num_to_numerator(b), num_to_denominator(b));
      break;
      
    default:
      if (num_to_real(a) < num_to_real(b))
	real(ret) = num_to_real(a);
      else real(ret) = num_to_real(b);
      break;
    }
  
  return(ret);
}


static num num_add(num a, num b) 
{
  num ret;
  ret.type = a.type | b.type;
  
  switch (num_type(ret))
    {
    case NUM_INT: 
      integer(ret) = integer(a) + integer(b);
      break;
      
    case NUM_RATIO:
      ret = make_ratio((num_to_numerator(a) * num_to_denominator(b)) + (num_to_denominator(a) * num_to_numerator(b)),
		       (num_to_denominator(a) * num_to_denominator(b)));
      break;
      
    case NUM_REAL2:
    case NUM_REAL:
      real(ret) = num_to_real(a) + num_to_real(b);
      break;
      
    default:
      /* NUM_COMPLEX is 4 separate types */
      ret = make_complex(num_to_real_part(a) + num_to_real_part(b),
			 num_to_imag_part(a) + num_to_imag_part(b));
      break;
    }
  
  return(ret);
}


static num num_sub(num a, num b) 
{
  num ret;
  ret.type = a.type | b.type;
  
  switch (num_type(ret))
    {
    case NUM_INT: 
      integer(ret) = integer(a) - integer(b);
      break;
      
    case NUM_RATIO:
      ret = make_ratio((num_to_numerator(a) * num_to_denominator(b)) - (num_to_denominator(a) * num_to_numerator(b)),
		       (num_to_denominator(a) * num_to_denominator(b)));
      break;
      
    case NUM_REAL2:
    case NUM_REAL:
      real(ret) = num_to_real(a) - num_to_real(b);
      break;
      
    default:
      ret = make_complex(num_to_real_part(a) - num_to_real_part(b),
			 num_to_imag_part(a) - num_to_imag_part(b));
      break;
    }
  
  return(ret);
}


static num num_mul(num a, num b) 
{
  num ret;
  ret.type = a.type | b.type;
  
  switch (num_type(ret))
    {
    case NUM_INT: 
      integer(ret) = integer(a) * integer(b);
      break;
      
    case NUM_RATIO:
      ret = make_ratio((num_to_numerator(a) * num_to_numerator(b)),
		       (num_to_denominator(a) * num_to_denominator(b)));
      break;
      
    case NUM_REAL2:
    case NUM_REAL:
      real(ret) = num_to_real(a) * num_to_real(b);
      break;
      
    default:
      {
	s7_Double r1, r2, i1, i2;
	r1 = num_to_real_part(a);
	r2 = num_to_real_part(b);
	i1 = num_to_imag_part(a);
	i2 = num_to_imag_part(b);
	ret = make_complex(r1 * r2 - i1 * i2, 
			   r1 * i2 + r2 * i1);
      }
      break;
    }
  
  return(ret);
}


static num num_div(num a, num b) 
{
  num ret;
  ret.type = a.type | b.type;
  
  switch (num_type(ret))
    {
    case NUM_INT: 
      ret = make_ratio(integer(a), integer(b));
      break;
      
    case NUM_RATIO:
      ret = make_ratio((num_to_numerator(a) * num_to_denominator(b)),
		       (num_to_denominator(a) * num_to_numerator(b)));
      break;
      
    case NUM_REAL2:
    case NUM_REAL:
      {
	s7_Double rb;
	rb = num_to_real(b);
	real(ret) = num_to_real(a) / rb;
      }
      break;
      
    default:
      {
	s7_Double r1, r2, i1, i2, den;
	r1 = num_to_real_part(a);
	r2 = num_to_real_part(b);
	i1 = num_to_imag_part(a);
	i2 = num_to_imag_part(b);
	den = (r2 * r2 + i2 * i2);
	ret = make_complex((r1 * r2 + i1 * i2) / den, 
			   (r2 * i1 - r1 * i2) / den);
      }
      break;
    }
  
  return(ret);
}


static s7_Int s7_truncate(s7_Double xf)
{
  if (xf > 0.0)
    return((s7_Int)floor(xf));
  return((s7_Int)ceil(xf));
}


static num num_quotient(num a, num b) 
{
  /* (define (quo x1 x2) (truncate (/ x1 x2))) ; slib */
  num ret;
  ret.type = NUM_INT;
  if ((a.type | b.type) == NUM_INT)
    integer(ret) = integer(a) / integer(b);
  else integer(ret) = s7_truncate(num_to_real(a) / num_to_real(b));
  return(ret);
}


static num num_rem(num a, num b) 
{
  /* (define (rem x1 x2) (- x1 (* x2 (quo x1 x2)))) ; slib */
  num ret;
  ret.type = a.type | b.type;
  switch (ret.type)
    {
    case NUM_INT: 
      integer(ret) = integer(a) % integer(b);
      break;
    case NUM_RATIO: 
      ret = make_ratio(num_to_numerator(a) * num_to_denominator(b) - 
		       num_to_numerator(b) * num_to_denominator(a) * integer(num_quotient(a, b)),
		       num_to_denominator(a) * num_to_denominator(b));
      break;
    default:
      real(ret) = num_to_real(a) - num_to_real(b) * integer(num_quotient(a, b));
      break;
    }
  return(ret);
}


static num num_mod(num a, num b) 
{
  /* (define (mod x1 x2) (- x1 (* x2 (floor (/ x1 x2))))) ; slib */
  num ret;
  ret.type = a.type | b.type;
  switch (ret.type)
    {
    case NUM_INT:
      integer(ret) = c_mod(integer(a), integer(b));
      break;
    case NUM_RATIO:
      ret = make_ratio(num_to_numerator(a) * num_to_denominator(b) - 
		       num_to_numerator(b) * num_to_denominator(a) * (s7_Int)floor(num_to_real(a) / num_to_real(b)),
		       num_to_denominator(a) * num_to_denominator(b));
      break;
    default:
      real(ret) = num_to_real(a) - num_to_real(b) * (s7_Int)floor(num_to_real(a) / num_to_real(b));
      break;
    }
  
  return(ret);
}


static bool num_eq(num a, num b) 
{
  switch (num_type(a))
    {
    case NUM_INT:
      switch (num_type(b))
	{
	case NUM_INT: 
	  return(integer(a) == integer(b));
	case NUM_RATIO:
	  return(false);
	case NUM_REAL:
	case NUM_REAL2:
	  return(integer(a) == real(b));
	default: 
	  return((real_part(b) == integer(a)) &&
		 (imag_part(b) == 0.0));
	}
      break;
      
    case NUM_RATIO:  
      switch (num_type(b))
	{
	case NUM_RATIO:
	  return((numerator(a) == numerator(b)) &&
		 (denominator(a) == denominator(b)));
	case NUM_REAL:
	case NUM_REAL2:
	  return(num_to_real(a) == real(b));
	default:
	  return(false);
	}
      break;
      
    case NUM_REAL2:
    case NUM_REAL:    
      switch (num_type(b))
	{
	case NUM_INT:
	  return(real(a) == integer(b));
	case NUM_RATIO:
	  return(real(a) == num_to_real(b));
	case NUM_REAL:
	case NUM_REAL2:
	  return(real(a) == real(b));
	default:
	  return((real_part(b) == real(a)) &&
		 (imag_part(b) == 0.0));
	}
      break;
      
    default:
      switch (num_type(b))
	{
	case NUM_INT:
	  return((real_part(a) == integer(b)) &&
		 (imag_part(a) == 0.0));
	case NUM_RATIO:
	  return((real_part(a) == num_to_real(b)) &&
		 (imag_part(a) == 0.0));
	case NUM_REAL:
	case NUM_REAL2:
	  return((real_part(a) == real(b)) &&
		 (imag_part(a) == 0.0));
	default:
	  return((real_part(a) == real_part(b)) &&
		 (imag_part(a) == imag_part(b)));
	}
      break;
    }
  return(false);
}


static bool num_gt(num a, num b) 
{
  /* the ">" operator here is a problem.
   *   we get different results depending on the gcc optimization level for cases like (< 1234/11 1234/11)
   *   so, to keep ratios honest, we'll use num_sub and compare against 0
   */
  num val;
  val = num_sub(a, b);
  switch (num_type(val))
    {
    case NUM_INT:   return(integer(val) > 0);
    case NUM_RATIO: return(numerator(val) > 0);
    default:        return(real(val) > 0.0);
    }

#if 0  
  /* this is the flakey version */
  if ((num_type(a) == NUM_INT) &&
      (num_type(b) == NUM_INT))
    return(integer(a) > integer(b));
  return(num_to_real(a) > num_to_real(b));
#endif
}


static bool num_lt(num a, num b) 
{
  num val;
  val = num_sub(a, b);
  switch (num_type(val))
    {
    case NUM_INT:   return(integer(val) < 0);
    case NUM_RATIO: return(numerator(val) < 0);
    default:        return(real(val) < 0.0);
    }
}


static bool num_ge(num a, num b) 
{
  return(!num_lt(a, b));
}


static bool num_le(num a, num b) 
{
  return(!num_gt(a, b));
}


static s7_Double round_per_R5RS(s7_Double x) 
{
  s7_Double fl = floor(x);
  s7_Double ce = ceil(x);
  s7_Double dfl = x - fl;
  s7_Double dce = ce - x;
  
  if (dfl > dce) return(ce);
  if (dfl < dce) return(fl);
  if (fmod(fl, 2.0) == 0.0) return(fl);
  return(ce);
}


static bool s7_is_negative(s7_pointer obj)
{
  switch (object_number_type(obj))
    {
    case NUM_INT:   return(s7_integer(obj) < 0);
    case NUM_RATIO: return(s7_numerator(obj) < 0);
    default:        return(s7_real(obj) < 0);
    }
}


static bool s7_is_positive(s7_pointer x)
{
  switch (object_number_type(x))
    {
    case NUM_INT:   return(s7_integer(x) > 0);
    case NUM_RATIO: return(s7_numerator(x) > 0);
    default:        return(s7_real(x) > 0);
    }
}


static bool s7_is_zero(s7_pointer x)
{
  switch (object_number_type(x))
    {
    case NUM_INT:   return(s7_integer(x) == 0);
    case NUM_REAL2:
    case NUM_REAL:  return(s7_real(x) == 0.0);
    case NUM_RATIO: return(s7_numerator(x) == 0);
    default:        return((s7_real_part(x) == 0.0) &&
			   (s7_imag_part(x) == 0.0));
    }
}


static void s7_Int_to_string(char *p, s7_Int n, int radix, int width)
{
  static char dignum[] = "0123456789abcdef";
  int i = 2, len, start = 0, end = 0;
  s7_Int pown = (s7_Int)1;
  bool sign;

  if ((radix < 2) || (radix > 16))
    return;
  if (n == 0)
    {
      p[0] = '0';
      p[1] = '\0';
      return;
    }

  sign = (n < 0);
  n = s7_Int_abs(n); /* most-negative-fixnum loses... */

  for (i = 1; i < 100; i++)
    {
      if ((pown > n) || (pown <= (s7_Int)0))
	break;
      pown *= (s7_Int)radix;
    }

  len = i - 2;
  /* len = (int)(floor(log((s7_Double)n) / log((s7_Double)radix))); */

  if (sign) len++;

  if (width > len) /* (format #f "~10B" 123) */
    {
      start = width - len - 1;
      end += start;
      for (i = 0; i < start; i++) 
	p[i] = ' ';
    }

  if (sign)
    {
      p[start] = '-';
      end++;
    }

  for (i = start + len; i >= end; i--)
    {
      p[i] = dignum[n % (s7_Int)radix];
      n /= (s7_Int)radix;
    }
  p[len + start + 1] = '\0';
}


static char *s7_number_to_string_1(s7_scheme *sc, s7_pointer obj, int radix, int width, int precision, char float_choice)
{
  #define NUM_SIZE 256
  char *p;
  p = (char *)malloc(NUM_SIZE * sizeof(char));

  switch (object_number_type(obj))
    {
    case NUM_INT:
      if (radix == 10)
	snprintf(p, NUM_SIZE, 
		 (sizeof(int) >= sizeof(s7_Int)) ? "%*d" : "%*lld",
		 width, s7_integer(obj));
      else s7_Int_to_string(p, s7_integer(obj), radix, width);
      break;
      
    case NUM_RATIO:
      if (radix == 10)
	snprintf(p, NUM_SIZE, 
		 (sizeof(int) >= sizeof(s7_Int)) ? "%d/%d" : "%lld/%lld", 
		 s7_numerator(obj), s7_denominator(obj));
      else
	{
	  char *n, *d;
	  n = (char *)calloc(NUM_SIZE, sizeof(char));
	  d = (char *)calloc(NUM_SIZE, sizeof(char));
	  s7_Int_to_string(n, s7_numerator(obj), radix, 0);
	  s7_Int_to_string(d, s7_denominator(obj), radix, 0);
	  snprintf(p, NUM_SIZE, "%s/%s", n, d);
	  free(n);
	  free(d);
	}
      break;
      
      /* SOMEDAY: need radix in number->string float/complex: (number->string 0.75 2) -> "0.75" */
    case NUM_REAL2:
    case NUM_REAL:
      {
	int i, loc = -1, len;
	const char *frmt;
	if (sizeof(double) >= sizeof(s7_Double))
	  frmt = (float_choice == 'g') ? "%*.*g" : ((float_choice == 'f') ? "%*.*f" : "%*.*e");
	else frmt = (float_choice == 'g') ? "%*.*Lg" : ((float_choice == 'f') ? "%*.*Lf" : "%*.*Le");

	len = snprintf(p, NUM_SIZE, frmt, width, precision, s7_real(obj));
	/* len = safe_strlen(p); */
	for (i = 0; i < len; i++) /* does it have an exponent (if so, it's already a float) */
	  if (p[i] == 'e')
	    {
	      loc = i;
	      break;
	    }
	if (loc == -1)            /* no, so make it explicitly a float! */
	  {
	    for (i = 0; i < len; i++)  
	      if (p[i] == '.') break;
	    if (i == len)
	      {
		p[i]='.';
		p[i+1]='0';
		p[i+2]='\0';
	      }
	  }
      }
      break;
      
    default:
      {
	const char *frmt;
	if (sizeof(double) >= sizeof(s7_Double))
	  {
	    if (s7_imag_part(obj) >= 0.0)
	      frmt = (float_choice == 'g') ? "%.*g+%.*gi" : ((float_choice == 'f') ? "%.*f+%.*fi" : "%.*e+%.*ei"); 
	    else frmt = (float_choice == 'g') ? "%.*g%.*gi" : ((float_choice == 'f') ? "%.*f-%.*fi" :"%.*e-%.*ei");
	  }
	else 
	  {
	    if (s7_imag_part(obj) >= 0.0)
	      frmt = (float_choice == 'g') ? "%.*Lg+%.*Lgi" : ((float_choice == 'f') ? "%.*Lf+%.*Lfi" : "%.*Le+%.*Lei");
	    else frmt = (float_choice == 'g') ? "%.*Lg%.*Lgi" : ((float_choice == 'f') ? "%.*Lf-%.*Lfi" : "%.*Le-%.*Lei");
	  }

	snprintf(p, NUM_SIZE, frmt, precision, s7_real_part(obj), precision, s7_imag_part(obj));
      }
      break;
      
    }
  return(p);
}


char *s7_number_to_string(s7_scheme *sc, s7_pointer obj, int radix)
{
  return(s7_number_to_string_1(sc, obj, radix, 0, 20, 'g')); /* (log top 10) so we get all the digits */
}


static s7_pointer g_number_to_string(s7_scheme *sc, s7_pointer args)
{
  #define H_number_to_string "(number->string num :optional (radix 10)) converts num into a string"
  int radix = 0;
  char *res;
  if (!s7_is_number(car(args)))
    return(s7_wrong_type_arg_error(sc, "number->string", 1, car(args), "a number"));
  if (is_pair(cdr(args)))
    {
      if (s7_is_integer(cadr(args)))
	radix = s7_integer(cadr(args));
      if ((radix < 2) || (radix > 16))
	return(s7_out_of_range_error(sc, "number->string", 2, cadr(args), "between 2 and 16"));
    }
  else radix = 10;
  res = s7_number_to_string(sc, car(args), radix);
  sc->y = s7_make_string(sc, res);
  free(res);
  return(sc->y);
}


#define CTABLE_SIZE 128
static bool *whitespace_table, *atom_delimiter_table, *sharp_const_table, *exponent_table, *slashify_table, *string_delimiter_table;
static int *digits;

static bool is_one_of(bool *ctable, int c) 
{
  return((c == EOF) || (ctable[c]));
}


static void init_ctables(void)
{
  int i;

  whitespace_table = (bool *)calloc(CTABLE_SIZE, sizeof(bool));
  atom_delimiter_table = (bool *)calloc(CTABLE_SIZE, sizeof(bool));
  sharp_const_table = (bool *)calloc(CTABLE_SIZE, sizeof(bool));
  exponent_table = (bool *)calloc(CTABLE_SIZE, sizeof(bool));
  slashify_table = (bool *)calloc(CTABLE_SIZE, sizeof(bool));
  string_delimiter_table = (bool *)calloc(CTABLE_SIZE, sizeof(bool));
  
  whitespace_table['\n'] = true;
  whitespace_table['\t'] = true;
  whitespace_table[' '] = true;
  
  sharp_const_table[' '] = true;
  sharp_const_table['t'] = true;
  sharp_const_table['f'] = true;
  sharp_const_table['o'] = true;
  sharp_const_table['d'] = true;
  sharp_const_table['x'] = true;
  sharp_const_table['b'] = true;
  sharp_const_table['i'] = true;
  sharp_const_table['e'] = true;
  sharp_const_table['\\'] = true;
  
  atom_delimiter_table['('] = true;
  atom_delimiter_table[')'] = true;
  atom_delimiter_table[';'] = true;
  atom_delimiter_table['\t'] = true;
  atom_delimiter_table['\n'] = true;
  atom_delimiter_table['\r'] = true;
  atom_delimiter_table[' '] = true;

  exponent_table['e'] = true;
  exponent_table['E'] = true;
  exponent_table['s'] = true;
  exponent_table['f'] = true;
  exponent_table['d'] = true;
  exponent_table['l'] = true;

  for (i = 0; i < ' '; i++)
    slashify_table[i] = true;
  slashify_table['\n'] = false;
  slashify_table['\\'] = true;
  slashify_table['"'] = true;

  for (i = 1; i < CTABLE_SIZE; i++)
    string_delimiter_table[i] = true;
  string_delimiter_table[0] = false;
  string_delimiter_table['('] = false;
  string_delimiter_table[')'] = false;
  string_delimiter_table[';'] = false;
  string_delimiter_table['\t'] = false;
  string_delimiter_table['\n'] = false;
  string_delimiter_table['\r'] = false;
  string_delimiter_table[' '] = false;

  digits = (int *)malloc(CTABLE_SIZE * sizeof(int));
  for (i = 0; i < CTABLE_SIZE; i++)
    digits[i] = 256;

  digits['0'] = 1;
  digits['1'] = 1;
  digits['2'] = 2;
  digits['3'] = 3;
  digits['4'] = 4;
  digits['5'] = 5;  
  digits['6'] = 6;
  digits['7'] = 7;
  digits['8'] = 8;
  digits['9'] = 9;
  digits['a'] = 10;
  digits['A'] = 10;
  digits['b'] = 11;
  digits['B'] = 11;
  digits['c'] = 12;
  digits['C'] = 12;
  digits['d'] = 13;
  digits['D'] = 13;
  digits['e'] = 14;
  digits['E'] = 14;
  digits['f'] = 15;
  digits['F'] = 15;
}


static bool is_radix_prefix(char prefix)
{
  return((prefix == 'b') ||
	 (prefix == 'd') ||
	 (prefix == 'x') ||
	 (prefix == 'o'));
}


static int char_to_num(const char c)
{
  if (isdigit(c))
    return(c - '0');
  return((tolower(c) - 'a') + 10);
}


static s7_pointer make_sharp_constant(s7_scheme *sc, char *name) 
{
  /* name is the stuff after the '#', return sc->NIL if not a recognized #... entity */
  int len;
  s7_pointer x;
  
  if (strcmp(name, "t") == 0)
    return(sc->T);
  
  if (strcmp(name, "f") == 0)
    return(sc->F);
  
  len = strlen(name);
  if (len == 0)
    return(sc->NIL);

  if ((len < 2) &&
      (is_radix_prefix(name[0])))
    return(sc->NIL);
      
  switch (name[0])
    {
    case 'o':   /* #o (octal) */
    case 'd':   /* #d (decimal) */
    case 'x':   /* #x (hex) */
    case 'b':   /* #b (binary) */
      {
	bool to_inexact = false;
	int num_at = 1;
  
	if (name[1] == '#')
	  {
	    if ((len > 2) && ((name[2] == 'e') || (name[2] == 'i')))
	      {
		to_inexact = (name[2] == 'i');
		num_at = 3;
	      }
	    else return(sc->NIL);
	  }
	x = make_atom(sc, (char *)(name + num_at), (name[0] == 'o') ? 8 : ((name[0] == 'x') ? 16 : ((name[0] == 'b') ? 2 : 10)), false);
	if (!s7_is_integer(x))
	  return(sc->NIL);
	if (to_inexact)
	  return(s7_make_real(sc, s7_number_to_real(x)));
	return(x);  /* we already checked that it is a number */
      }

    case 'i':   /* #i<num> = ->inexact (see token, is_one_of for table of choices here) */
      if (name[1] == '#')
	{
	  if (is_radix_prefix(name[2]))
	    {
	      s7_pointer i_arg;
	      i_arg = make_sharp_constant(sc, (char *)(name + 2));
	      if (!s7_is_integer(i_arg))
		return(sc->NIL);
	      return(s7_make_real(sc, (s7_Double)s7_integer(i_arg)));
	    }
	  return(sc->NIL);
	}
      x = make_atom(sc, (char *)(name + 1), 10, false);
      if (s7_is_number(x))
	{
	  if (s7_is_rational(x))
	    return(s7_make_real(sc, s7_number_to_real(x)));
	  return(x);  /* already an inexact number */
	}
      return(sc->NIL);
  
    case 'e':   /* #e<num> = ->exact */
      if (name[1] == '#')
	{
	  if (is_radix_prefix(name[2]))
	    return(make_sharp_constant(sc, (char *)(name + 2)));
	  return(sc->NIL);
	}
      x = make_atom(sc, (char *)(name + 1), 10, false);
      if (s7_is_number(x))
	{
	  if (!(s7_is_rational(x)))
	    {
	      s7_Int numer = 0, denom = 1;
	      if (c_rationalize(s7_real_part(x), default_rationalize_error, &numer, &denom))
		return(s7_make_ratio(sc, numer, denom));
	    }
	  return(x); /* already an exact number */
	}
      return(sc->NIL);

    case '\\':
      /* #\space or whatever (named character) */
      { 
	int c = 0;
	if (STRCMP(name + 1, "space") == 0) 
	  c =' ';
	else 
	  {
	    if (STRCMP(name + 1, "newline") == 0)
	      c ='\n';
	    else 
	      {
		if (STRCMP(name + 1, "return") == 0) 
		  c ='\r';
		else 
		  {
		    if (STRCMP(name + 1, "tab") == 0) 
		      c ='\t';
		    else 
		      {
			if (STRCMP(name + 1, "null") == 0) 
			  c ='\0';
			else 
			  {
			    if ((name[1] == 'x') && 
				(name[2] != 0))          /* hunh?? -- #\x12?? */
			      {
				int c1= 0;
				if ((sscanf(name + 2, "%x", &c1) == 1) && 
				    (c1 < 256))
				  c = c1;
				else return(sc->NIL);
			      }
			    else 
			      {
				if (name[2] == 0) 
				  c = name[1];
				else return(sc->NIL);
			      }}}}}}
	return(s7_make_character(sc, c));
      }
    }
  return(sc->NIL);
}


static s7_Int string_to_int_with_radix(const char *str,  int radix)
{
  s7_Int x = 0, rad;
  int i, lim = 0, len, sign = 1;

  len = safe_strlen(str);
  for (i = 0; i < len; i++)
    if ((str[i] == '/') ||
	(str[i] == '.') ||
	((str[i] == 'e') && (radix < 15)))    /* '.' and 'e' for ATOF */
      {
	len = i;
	break;
      }

  if ((str[0] == '+') || (str[0] == '-'))
    {
      lim = 1;
      if (str[0] == '-') sign = -1;
    }
  
  for (i = len - 1, rad = 1; i >= lim; i--, rad *= radix)
    x += (char_to_num(str[i]) * rad);
  
  return(sign * x);
}


static s7_Double string_to_double_with_radix(char *str, int radix)
{
  /* (do ((i 2 (+ i 1))) ((= i 17)) (display (inexact->exact (floor (log (expt 2 62) i)))) (display ", ")) */
  static int digits[] = {0, 0, 62, 39, 31, 26, 23, 22, 20, 19, 18, 17, 17, 16, 16, 15, 15};

  int i, len, iloc = 0, floc = -1, eloc = -1, sign = 1, flen = 0;
  s7_Int int_part = 0, frac_part = 0, exponent = 0;

  /* there's an ambiguity in number notation here if we allow "1e1" or "1.e1" in base 16 (or 15) -- is e a digit or an exponent marker? */
  /*   but 1e+1, for example disambiguates it -- kind of messy! */

  if ((str[0] == '+') || (str[0] == '-'))
    {
      iloc = 1;
      if (str[0] == '-') sign = -1;
    }

  len = safe_strlen(str);
  for (i = iloc; i < len; i++)
    if (str[i] == '.')
      floc = i;
    else
      if ((str[i] == 'e') && (radix < 15))
	eloc = i;

  if ((floc == -1) || (floc > iloc))
    int_part = string_to_int_with_radix((char *)(str + iloc), radix);

  if ((floc != -1) &&          /* has a "." */
      (floc < len - 1) &&      /* doesn't end in "." */
      (floc != eloc - 1))      /* "1.e1" for example */
    {
      flen = ((eloc == -1) ? (len - floc - 1) : (eloc - floc - 1));
      if (flen <= digits[radix])
	frac_part = string_to_int_with_radix((char *)(str + floc + 1), radix);
      else
	{
	  char old_c;
	  /* we need to ignore extra trailing digits here to avoid later overflow */
	  /* (string->number "3.1415926535897932384626433832795029" 11) */
	  flen = digits[radix];
	  old_c = str[floc + flen + 1];
	  str[floc + flen + 1] = '\0';
	  frac_part = string_to_int_with_radix((char *)(str + floc + 1), radix);
	  str[floc + flen + 1] = old_c;
	}
    }

  if (eloc != -1)
    exponent = string_to_int_with_radix((char *)(str + eloc + 1), radix);

  if (frac_part > 0)
    return(sign * (int_part + ((s7_Double)frac_part / pow((s7_Double)radix, (s7_Double)flen))) * pow((s7_Double)radix, (s7_Double)exponent));
  if (exponent != 0)
    return(sign * (int_part * pow((s7_Double)radix, (s7_Double)exponent)));
  return(sign * int_part);
}


/* make symbol or number atom from string */

static s7_pointer make_atom(s7_scheme *sc, char *q, int radix, bool want_symbol) 
{
  #define ISDIGIT(Chr, Rad) (digits[(int)Chr] < Rad)

  char c, *p, *slash1 = NULL, *slash2 = NULL, *plus = NULL, *ex1 = NULL, *ex2 = NULL;
  bool has_dec_point1 = false, has_i = false, has_dec_point2 = false; 
  int has_plus_or_minus = 0;
  
  #define ATOLL(x) ((radix == 10) ? atoll(x) : string_to_int_with_radix(x, radix))
  #define ATOF(x) ((radix == 10) ? atof(x) : string_to_double_with_radix(x, radix))

  p = q;
  c = *p++; 
  
  /* a number starts with + - . or digit, but so does 1+ for example */
  
  if (c == '#')
    return(make_sharp_constant(sc, p)); /* make_sharp_constant expects the '#' to be removed */
  
  if ((c == '+') || (c == '-')) 
    { 
      c = *p++; 
      if (c == '.') 
	{ 
	  has_dec_point1 = true; 
	  c = *p++; 
	} 
      if (!ISDIGIT(c, radix))
	return((want_symbol) ? s7_make_symbol(sc, string_downcase(q)) : sc->F);  /* if CASE_SENSITIVE, string_downcase is a no-op */
    } 
  else 
    {
      if (c == '.')         /* .0 */
	{ 
	  has_dec_point1 = true; 
	  c = *p++; 
	  if (!ISDIGIT(c, radix))
	    return((want_symbol) ? s7_make_symbol(sc, string_downcase(q)) : sc->F); 
	} 
      else 
	{
	  if (!ISDIGIT(c, radix))
	    return((want_symbol) ? s7_make_symbol(sc, string_downcase(q)) : sc->F); 
	}
    }
  
  for ( ; (c = *p) != 0; ++p) 
    {
      if (!ISDIGIT(c, radix)) 
	{
	  if (c =='.') 
	    {
	      if (((has_dec_point1) ||
		   (slash1)) &&
		  (has_plus_or_minus == 0)) /* 1.. or 1/2. */
		return((want_symbol) ? s7_make_symbol(sc, string_downcase(q)) : sc->F); 

	      if (((has_dec_point2) ||
		   (slash2)) &&
		  (has_plus_or_minus != 0)) /* 1+1.. or 1+1/2. */
		return((want_symbol) ? s7_make_symbol(sc, string_downcase(q)) : sc->F); 

	      if ((!ISDIGIT(p[1], radix)) &&
		  (!ISDIGIT(p[-1], radix))) 
		return((want_symbol) ? s7_make_symbol(sc, string_downcase(q)) : sc->F); 
	      
	      if (has_plus_or_minus == 0)
		has_dec_point1 = true;
	      else has_dec_point2 = true;
	      continue;
	    }
	  else 
	    {
	      if (exponent_table[(int)c]) /* sigh -- what's the difference between these endless (e s f d l) exponent chars? */
		{
		  if (((ex1) ||
		       (slash1)) &&
		      (has_plus_or_minus == 0)) /* ee */
		    return((want_symbol) ? s7_make_symbol(sc, string_downcase(q)) : sc->F); 

		  if (((ex2) ||
		       (slash2)) &&
		      (has_plus_or_minus != 0)) /* 1+1.0ee */
		    return((want_symbol) ? s7_make_symbol(sc, string_downcase(q)) : sc->F); 

		  if ((!ISDIGIT(p[-1], radix)) &&
		      (p[-1] != '.'))
		    return((want_symbol) ? s7_make_symbol(sc, string_downcase(q)) : sc->F); 

		  if (has_plus_or_minus == 0)
		    {
		      ex1 = p;
		      has_dec_point1 = true; /* decimal point illegal from now on */
		    }
		  else 
		    {
		      ex2 = p;
		      has_dec_point2 = true;
		    }
		  p++;
		  if ((*p == '-') || (*p == '+')) p++;
		  if (ISDIGIT(*p, radix))
		    continue;
		}
	      else
		{
		  if ((c == '+') || (c == '-'))
		    {
		      if (has_plus_or_minus != 0) /* already have the separator */
			return((want_symbol) ? s7_make_symbol(sc, string_downcase(q)) : sc->F);
		      
		      if (c == '+') has_plus_or_minus = 1; else has_plus_or_minus = -1;
		      plus = (char *)(p + 1);
		      continue;
		    }
		  else
		    {
		      if (c == '/')
			{
			  if ((has_plus_or_minus == 0) &&
			      ((ex1) ||
			       (slash1) ||
			       (has_dec_point1)))
			    return((want_symbol) ? s7_make_symbol(sc, string_downcase(q)) : sc->F);

			  if ((has_plus_or_minus != 0) &&
			      ((ex2) ||
			       (slash2) ||
			       (has_dec_point2)))
			    return((want_symbol) ? s7_make_symbol(sc, string_downcase(q)) : sc->F);
			  
			  if (has_plus_or_minus == 0)
			    slash1 = (char *)(p + 1);
			  else slash2 = (char *)(p + 1);

			  if ((!ISDIGIT(p[1], radix)) ||
			      (!ISDIGIT(p[-1], radix)))
			    return((want_symbol) ? s7_make_symbol(sc, string_downcase(q)) : sc->F);

			  continue;
			}
		      else
			{
			  if ((has_plus_or_minus != 0) && 
			      (!has_i) && 
			      (c == 'i'))
			    {
			      has_i = true;
			      continue;
			    }
			}
		    }
		}
	    }
	  return((want_symbol) ? s7_make_symbol(sc, string_downcase(q)) : sc->F);
	}
    }
  
  if ((has_plus_or_minus != 0) &&
      (!has_i))
    return((want_symbol) ? s7_make_symbol(sc, string_downcase(q)) : sc->F);
  
  if (has_i)
    {
      s7_Double rl = 0.0, im = 0.0;
      int len;
      char *saved_q;
      len = strlen(q);
      
      if (q[len - 1] != 'i')
	return((want_symbol) ? s7_make_symbol(sc, string_downcase(q)) : sc->F);

      saved_q = strdup(q);

      /* look for cases like 1+i */
      if ((q[len - 2] == '+') || (q[len - 2] == '-'))
	q[len - 1] = '1';
      else q[len - 1] = '\0'; /* remove 'i' */
      
      if (ex1) (*ex1) = 'e';
      if (ex2) (*ex2) = 'e';
      
      if ((has_dec_point1) ||
	  (ex1))
	rl = ATOF(q);
      else
	{
	  if (slash1)
	    rl = (s7_Double)ATOLL(q) / (s7_Double)ATOLL(slash1);
	  else rl = (s7_Double)ATOLL(q);
	}
      if (rl == -0.0) rl = 0.0;

      if ((has_dec_point2) ||
	  (ex2))
	im = ATOF(plus);
      else
	{
	  if (slash2)
	    im = (s7_Double)ATOLL(plus) / (s7_Double)ATOLL(slash2);
	  else im = (s7_Double)ATOLL(plus);
	}
      if ((has_plus_or_minus == -1) && 
	  (im != 0.0))
	im = -im;

      {
	int i;
	for (i = 0; i < len; i++)
	  q[i] = saved_q[i];
	free(saved_q);
      }

      return(s7_make_complex(sc, rl, im));
    }
  
  if ((has_dec_point1) ||
      (ex1))
    {
      double x;

      if (slash1)  /* not complex, so slash and "." is not a number */
	return((want_symbol) ? s7_make_symbol(sc, string_downcase(q)) : sc->F);

      if (ex1)
	{
	  char old_e;
	  old_e = (*ex1);
	  (*ex1) = 'e';
	  x = ATOF(q);
	  (*ex1) = old_e;
	}
      else x = ATOF(q);

      return(s7_make_real(sc, x));
    }
  
  if (slash1)
    return(s7_make_ratio(sc, ATOLL(q), ATOLL(slash1)));
  
  return(s7_make_integer(sc, ATOLL(q)));
}


static s7_pointer s7_string_to_number(s7_scheme *sc, char *str, int radix)
{
  s7_pointer x;
  x = make_atom(sc, str, radix, true);
  if (s7_is_number(x))
    return(x);
  return(sc->F);
}


static s7_pointer g_string_to_number(s7_scheme *sc, s7_pointer args)
{
  #define H_string_to_number "(string->number str :optional (radix 10)) converts str into a number"
  int radix = 0;
  if (!s7_is_string(car(args)))
    return(s7_wrong_type_arg_error(sc, "string->number", 1, car(args), "a string"));
  if (is_pair(cdr(args)))
    {
      if (s7_is_integer(cadr(args)))
	radix = s7_integer(cadr(args));
      if ((radix < 2) ||
	  (radix > 36))
	return(s7_out_of_range_error(sc, "string->number", 2, cadr(args), "between 2 and 36"));
    }
  else radix = 10;
  return(s7_string_to_number(sc, string_value(car(args)), radix));
}


static bool numbers_are_eqv(s7_pointer a, s7_pointer b)
{
  /* (eqv? 1 1.0) -> #f! */
  if (s7_is_integer(a))
    return((s7_is_integer(b)) &&
	   (integer(a->object.number) == integer(b->object.number)));
  
  if (s7_is_ratio(a))
    return((s7_is_ratio(b)) &&
	   (numerator(a->object.number) == numerator(b->object.number)) &&
	   (denominator(a->object.number) == denominator(b->object.number)));

  if (s7_is_real(a))
    return((s7_is_real(b)) &&
	   (real(a->object.number) == real(b->object.number)));
  
  if (s7_is_complex(a))
    return((s7_is_complex(b)) &&
	   (real_part(a->object.number) == real_part(b->object.number)) &&
	   (imag_part(a->object.number) == imag_part(b->object.number)));
  
  return(false);
}


static s7_pointer g_make_polar(s7_scheme *sc, s7_pointer args)
{
  s7_Double ang, mag;
  #define H_make_polar "(make-polar mag ang) returns a complex number with magnitude mag and angle ang"
  
  if (!s7_is_real(car(args)))
    return(s7_wrong_type_arg_error(sc, "make-polar", 1, car(args), "a real"));
  if (!s7_is_real(cadr(args)))
    return(s7_wrong_type_arg_error(sc, "make-polar", 2, cadr(args), "a real"));
  
  mag = num_to_real((car(args))->object.number);
  ang = num_to_real((cadr(args))->object.number);
  return(s7_make_complex(sc, mag * cos(ang), mag * sin(ang)));
}


static s7_pointer g_make_rectangular(s7_scheme *sc, s7_pointer args)
{
  #define H_make_rectangular "(make-rectangular x1 x2) returns a complex number with real-part x1 and imaginary-part x2"
  
  if (!s7_is_real(car(args)))
    return(s7_wrong_type_arg_error(sc, "make-rectangular", 1, car(args), "a real"));
  if (!s7_is_real(cadr(args)))
    return(s7_wrong_type_arg_error(sc, "make-rectangular", 2, cadr(args), "a real"));
  
  return(s7_make_complex(sc, 
			 num_to_real((car(args))->object.number), 
			 num_to_real((cadr(args))->object.number)));
}


static s7_pointer g_magnitude(s7_scheme *sc, s7_pointer args)
{
  #define H_magnitude "(magnitude z) returns the magnitude of z"
  if (!s7_is_number(car(args)))
    return(s7_wrong_type_arg_error(sc, "magnitude", 1, car(args), "a number"));
  sc->v = nvalue(car(args));
  switch (num_type(sc->v))
    {
    case NUM_REAL2: 
    case NUM_REAL: 
      real(sc->v) = s7_Double_abs(real(sc->v)); 
      break;
    case NUM_INT:
      integer(sc->v) = s7_Int_abs(integer(sc->v));
      break;
    case NUM_RATIO:
      numerator(sc->v) = s7_Int_abs(numerator(sc->v));
      break;
    default:
      {
	s7_Double a, b;
	a = imag_part(sc->v);
	b = real_part(sc->v);
	sc->v.type = NUM_REAL;
	real(sc->v) = hypot(a, b);
      }
      break;
    }
  return(make_number(sc, sc->v));
}


static s7_pointer g_angle(s7_scheme *sc, s7_pointer args)
{
  #define H_angle "(angle z) returns the angle of z"
  if (!s7_is_number(car(args)))
    return(s7_wrong_type_arg_error(sc, "angle", 1, car(args), "a number"));
  sc->x = car(args);
  if (!s7_is_real(sc->x))
    return(s7_make_real(sc, atan2(s7_imag_part(sc->x), s7_real_part(sc->x))));
  if (num_to_real(sc->x->object.number) < 0.0)
    return(s7_make_real(sc, atan2(0.0, -1.0)));
  return(s7_make_real(sc, 0.0));
}


static s7_pointer g_rationalize(s7_scheme *sc, s7_pointer args)
{
  s7_Double err;
  s7_Int numer = 0, denom = 1;
  #define H_rationalize "(rationalize x err) returns a ratio within err of x"
  if (!s7_is_real(car(args)))
    return(s7_wrong_type_arg_error(sc, "rationalize", 1, car(args), "a real"));
  sc->x = car(args);
  if (s7_is_exact(sc->x)) 
    return(sc->x);
  err = s7_real(cadr(args));
  if (c_rationalize(s7_real(sc->x), err, &numer, &denom))
    return(s7_make_ratio(sc, numer, denom));
  return(sc->F);
}


static s7_pointer g_abs(s7_scheme *sc, s7_pointer args)
{
  #define H_abs "(abs x) returns the absolute value of x"
  if (!s7_is_real(car(args)))
    return(s7_wrong_type_arg_error(sc, "abs", 1, car(args), "a real"));
  sc->v = nvalue(car(args));
  if (num_type(sc->v) >= NUM_REAL)
    real(sc->v) = s7_Double_abs(real(sc->v));
  else
    {
      if (num_type(sc->v) == NUM_INT)
	integer(sc->v) = s7_Int_abs(integer(sc->v));
      else numerator(sc->v) = s7_Int_abs(numerator(sc->v));
    }
  return(make_number(sc, sc->v));      
}


static s7_pointer g_exp(s7_scheme *sc, s7_pointer args)
{
  #define H_exp "(exp z) returns e^z"
  if (!s7_is_number(car(args)))
    return(s7_wrong_type_arg_error(sc, "exp", 1, car(args), "a number"));
  sc->x = car(args);
  if (s7_is_real(sc->x))
    return(s7_make_real(sc, exp(num_to_real(sc->x->object.number))));
  return(s7_from_c_complex(sc, cexp(s7_complex(sc->x))));
}


static s7_pointer g_log(s7_scheme *sc, s7_pointer args)
{
  #define H_log "(log z1 :optional z2) returns log(z1) / log(z2) where z2 defaults to e"
  
  if (!s7_is_number(car(args)))
    return(s7_wrong_type_arg_error(sc, "log", 1, car(args), "a number"));
  if ((is_pair(cdr(args))) &&
      (!(s7_is_number(cadr(args)))))
    return(s7_wrong_type_arg_error(sc, "log", 2, cadr(args), "a number"));
  
  sc->x = car(args);
  if (is_pair(cdr(args)))
    {
      sc->y = cadr(args);
      if (s7_is_zero(sc->y))
	return(s7_out_of_range_error(sc, "log", 2, sc->y, "not zero"));
      
      if ((s7_is_real(sc->x)) &&
	  (s7_is_real(sc->y)) &&
	  (s7_is_positive(sc->x)) &&
	  (s7_is_positive(sc->y)))
	{
	  if ((s7_is_integer(sc->x)) &&
	      (s7_is_integer(sc->y)))
	    {
	      s7_Double res;
	      s7_Int ires;
	      res = log(num_to_real(sc->x->object.number)) / log(num_to_real(sc->y->object.number));
	      ires = (s7_Int)res;
	      if (res - ires == 0.0)
		return(s7_make_integer(sc, ires));
	      return(s7_make_real(sc, res));
	    }
	  return(s7_make_real(sc, log(num_to_real(sc->x->object.number)) / log(num_to_real(sc->y->object.number))));
	}
      return(s7_from_c_complex(sc, clog(s7_complex(sc->x)) / clog(s7_complex(sc->y))));
    }

  if ((s7_is_real(sc->x)) &&
      (s7_is_positive(sc->x)))
    return(s7_make_real(sc, log(num_to_real(sc->x->object.number))));
  /* if < 0 use log(-x) + pi*i */
  return(s7_from_c_complex(sc, clog(s7_complex(sc->x))));
}


static s7_pointer g_sin(s7_scheme *sc, s7_pointer args)
{
  #define H_sin "(sin z) returns sin(z)"
  if (!s7_is_number(car(args)))
    return(s7_wrong_type_arg_error(sc, "sin", 1, car(args), "a number"));
  sc->x = car(args);
  if (s7_is_real(sc->x))
    return(s7_make_real(sc, sin(num_to_real(sc->x->object.number))));
  return(s7_from_c_complex(sc, csin(s7_complex(sc->x))));
}


static s7_pointer g_cos(s7_scheme *sc, s7_pointer args)
{
  #define H_cos "(cos z) returns cos(z)"
  if (!s7_is_number(car(args)))
    return(s7_wrong_type_arg_error(sc, "cos", 1, car(args), "a number"));
  sc->x = car(args);
  if (s7_is_real(sc->x))
    return(s7_make_real(sc, cos(num_to_real(sc->x->object.number))));
  return(s7_from_c_complex(sc, ccos(s7_complex(sc->x))));
}


static s7_pointer g_tan(s7_scheme *sc, s7_pointer args)
{
  #define H_tan "(tan z) returns tan(z)"
  if (!s7_is_number(car(args)))
    return(s7_wrong_type_arg_error(sc, "tan", 1, car(args), "a number"));
  sc->x = car(args);
  if (s7_is_real(sc->x))
    return(s7_make_real(sc, tan(num_to_real(sc->x->object.number))));

  if (s7_imag_part(sc->x) > 350.0)
    return(s7_make_complex(sc, 0.0, 1.0));
  if (s7_imag_part(sc->x) < -350.0)
    return(s7_make_complex(sc, 0.0, -1.0));

  return(s7_from_c_complex(sc, ctan(s7_complex(sc->x))));
}


static s7_pointer g_asin(s7_scheme *sc, s7_pointer args)
{
  #define H_asin "(asin z) returns asin(z)"
  if (!s7_is_number(car(args)))
    return(s7_wrong_type_arg_error(sc, "asin", 1, car(args), "a number"));

  sc->x = car(args);
  if (s7_is_real(sc->x))
    {
      s7_Double x, absx, recip;
      s7_Complex result;
      x = num_to_real(sc->x->object.number);
      absx = s7_Double_abs(x);
      if (absx <= 1.0)
	return(s7_make_real(sc, asin(x)));
      
      /* otherwise use maxima code: */
      recip = 1.0 / absx;
      result = (M_PI / 2.0) - (_Complex_I * clog(absx * (1.0 + (sqrt(1.0 + recip) * csqrt(1.0 - recip)))));
      if (x < 0.0)
	return(s7_from_c_complex(sc, -result));
      return(s7_from_c_complex(sc, result));
    }

#if WITH_COMPLEX
  /* if either real or imag part is very large, use explicit formula, not casin */
  /*   this code taken from sbcl's src/code/irrat.lisp */
  /* break is around x+70000000i */
  if ((s7_Double_abs(s7_real_part(sc->x)) > 1.0e7) ||
      (s7_Double_abs(s7_imag_part(sc->x)) > 1.0e7))
    {
      s7_Complex sq1mz, sq1pz, z;

      z = s7_complex(sc->x);
      sq1mz = csqrt(1.0 - z);
      sq1pz = csqrt(1.0 + z);
      return(s7_make_complex(sc, 
			     atan(s7_real_part(sc->x) / creal(sq1mz * sq1pz)),
			     asinh(cimag(sq1pz * conj(sq1mz)))));
    }
#endif

  return(s7_from_c_complex(sc, casin(s7_complex(sc->x))));
}


static s7_pointer g_acos(s7_scheme *sc, s7_pointer args)
{
  #define H_acos "(acos z) returns acos(z)"
  if (!s7_is_number(car(args)))
    return(s7_wrong_type_arg_error(sc, "acos", 1, car(args), "a number"));
  sc->x = car(args);
  if (s7_is_real(sc->x))
    {
      s7_Double x, absx, recip;
      s7_Complex result;
      x = num_to_real(sc->x->object.number);
      absx = s7_Double_abs(x);
      if (absx <= 1.0)
	return(s7_make_real(sc, acos(x)));
      
      /* else follow maxima again: */
      recip = 1.0 / absx;
      if (x > 0.0)
	result = _Complex_I * clog(absx * (1.0 + (sqrt(1.0 + recip) * csqrt(1.0 - recip))));
      else result = M_PI - _Complex_I * clog(absx * (1.0 + (sqrt(1.0 + recip) * csqrt(1.0 - recip))));
      return(s7_from_c_complex(sc, result));
    }

#if WITH_COMPLEX
  /* if either real or imag part is very large, use explicit formula, not cacos */
  /*   this code taken from sbcl's src/code/irrat.lisp */

  if ((s7_Double_abs(s7_real_part(sc->x)) > 1.0e7) ||
      (s7_Double_abs(s7_imag_part(sc->x)) > 1.0e7))
    {
      s7_Complex sq1mz, sq1pz, z;

      z = s7_complex(sc->x);
      sq1mz = csqrt(1.0 - z);
      sq1pz = csqrt(1.0 + z);
      return(s7_make_complex(sc, 
			     2.0 * atan(creal(sq1mz) / creal(sq1pz)),
			     asinh(cimag(sq1mz * conj(sq1pz)))));
    }
#endif

  return(s7_from_c_complex(sc, cacos(s7_complex(sc->x))));
}


static s7_pointer g_atan(s7_scheme *sc, s7_pointer args)
{
  #define H_atan "(atan z) returns atan(z)"
  sc->x = car(args);
  if (!is_pair(cdr(args)))
    {
      if (!s7_is_number(sc->x))
	return(s7_wrong_type_arg_error(sc, "atan", 1, sc->x, "a number"));
      if (s7_is_real(sc->x))
	return(s7_make_real(sc, atan(num_to_real(sc->x->object.number))));
      return(s7_from_c_complex(sc, catan(s7_complex(sc->x))));
    } 

  sc->y = cadr(args);
  if (!s7_is_real(sc->x))
    return(s7_wrong_type_arg_error(sc, "atan", 1, sc->x, "a real"));
  if (!s7_is_real(sc->y))
    return(s7_wrong_type_arg_error(sc, "atan", 2, sc->y, "a real"));
  return(s7_make_real(sc, atan2(num_to_real(sc->x->object.number), 
				num_to_real(sc->y->object.number))));
}  


static s7_pointer g_sinh(s7_scheme *sc, s7_pointer args)
{
  #define H_sinh "(sinh z) returns sinh(z)"
  if (!s7_is_number(car(args)))
    return(s7_wrong_type_arg_error(sc, "sinh", 1, car(args), "a number"));
  sc->x = car(args);
  if (s7_is_real(sc->x))
    return(s7_make_real(sc, sinh(num_to_real(sc->x->object.number))));
  return(s7_from_c_complex(sc, csinh(s7_complex(sc->x))));
}


static s7_pointer g_cosh(s7_scheme *sc, s7_pointer args)
{
  #define H_cosh "(cosh z) returns cosh(z)"
  if (!s7_is_number(car(args)))
    return(s7_wrong_type_arg_error(sc, "cosh", 1, car(args), "a number"));
  sc->x = car(args);
  if (s7_is_real(sc->x))
    return(s7_make_real(sc, cosh(num_to_real(sc->x->object.number))));
  return(s7_from_c_complex(sc, ccosh(s7_complex(sc->x))));
}


static s7_pointer g_tanh(s7_scheme *sc, s7_pointer args)
{
  #define H_tanh "(tanh z) returns tanh(z)"
  if (!s7_is_number(car(args)))
    return(s7_wrong_type_arg_error(sc, "tanh", 1, car(args), "a number"));
  sc->x = car(args);
  if (s7_is_real(sc->x))
    return(s7_make_real(sc, tanh(num_to_real(sc->x->object.number))));
  if (s7_real_part(sc->x) > 350.0)
    return(s7_make_real(sc, 1.0)); /* closer than 0.0 which is what ctanh is about to return! */
  if (s7_real_part(sc->x) < -350.0)
    return(s7_make_real(sc, -1.0)); /* closer than -0.0 which is what ctanh is about to return! */
  return(s7_from_c_complex(sc, ctanh(s7_complex(sc->x))));
}


static s7_pointer g_asinh(s7_scheme *sc, s7_pointer args)
{
  #define H_asinh "(asinh z) returns asinh(z)"
  if (!s7_is_number(car(args)))
    return(s7_wrong_type_arg_error(sc, "asinh", 1, car(args), "a number"));
  sc->x = car(args);
  if (s7_is_real(sc->x))
    return(s7_make_real(sc, asinh(num_to_real(sc->x->object.number))));
  return(s7_from_c_complex(sc, casinh(s7_complex(sc->x))));
}


static s7_pointer g_acosh(s7_scheme *sc, s7_pointer args)
{
  #define H_acosh "(acosh z) returns acosh(z)"
  if (!s7_is_number(car(args)))
    return(s7_wrong_type_arg_error(sc, "acosh", 1, car(args), "a number"));
  sc->x = car(args);
  if ((s7_is_real(sc->x)) &&
      (num_to_real(sc->x->object.number) >= 1.0))
    return(s7_make_real(sc, acosh(num_to_real(sc->x->object.number))));
  return(s7_from_c_complex(sc, cacosh(s7_complex(sc->x))));
}


static s7_pointer g_atanh(s7_scheme *sc, s7_pointer args)
{
  #define H_atanh "(atanh z) returns atanh(z)"
  if (!s7_is_number(car(args)))
    return(s7_wrong_type_arg_error(sc, "atanh", 1, car(args), "a number"));
  sc->x = car(args);
  if ((s7_is_real(sc->x)) &&
      (s7_Double_abs(num_to_real(sc->x->object.number)) < 1.0))
    return(s7_make_real(sc, atanh(num_to_real(sc->x->object.number))));
  return(s7_from_c_complex(sc, catanh(s7_complex(sc->x))));
}


static s7_pointer g_sqrt(s7_scheme *sc, s7_pointer args)
{
  #define H_sqrt "(sqrt z) returns sqrt(z)"
  if (!s7_is_number(car(args)))
    return(s7_wrong_type_arg_error(sc, "sqrt", 1, car(args), "a number"));
  sc->x = car(args);
  if (s7_is_real(sc->x))
    {
      s7_Double x, sqx;
      x = num_to_real(sc->x->object.number);
      if (x >= 0.0)
	{
	  sqx = sqrt(x);
	  if (s7_is_integer(sc->x))
	    {
	      s7_Int ix;
	      ix = (s7_Int)sqx;
	      if ((ix * ix) == integer(sc->x->object.number))
		return(s7_make_integer(sc, ix));
	    }
	  return(s7_make_real(sc, sqx));
	}
    }
  return(s7_from_c_complex(sc, csqrt(s7_complex(sc->x))));
}


static s7_Double top_log = 43.0;  /* approx log(2^63) */

static s7_pointer g_expt(s7_scheme *sc, s7_pointer args)
{
  #define H_expt "(expt z1 z2) returns z1^z2"
  
  if (!s7_is_number(car(args)))
    return(s7_wrong_type_arg_error(sc, "expt", 1, car(args), "a number"));
  if (!s7_is_number(cadr(args)))
    return(s7_wrong_type_arg_error(sc, "expt", 2, cadr(args), "a number"));
  
  sc->x = car(args);
  sc->y = cadr(args);
  
  if (object_number_type(sc->y) == NUM_INT)
    {
      if (object_number_type(sc->x) == NUM_INT)
	{
	  if (s7_integer(sc->y) == 0)
	    return(s7_make_integer(sc, 1));
	  
	  if (top_log > s7_Int_abs(s7_integer(sc->y)) * log(s7_Int_abs(s7_integer(sc->x)))) /* else over/underflow; a^b < 2^63 or > 2^-63 */
	    {
	      if ((s7_integer(sc->y) > 0) || 
		  (s7_Int_abs(s7_integer(sc->x)) == 1))
		return(s7_make_integer(sc, s7_Int_pow(s7_integer(sc->x), s7_integer(sc->y))));
	      
	      if (s7_integer(sc->x) == 0)
		return(sc->x);
	      
	      return(s7_make_ratio(sc, 1, s7_Int_pow(s7_integer(sc->x), -s7_integer(sc->y))));
	    }
	}
      else
	{
	  if (object_number_type(sc->x) == NUM_RATIO)
	    {
	      s7_Int n, d, p;
	      p = s7_integer(sc->y);
	      
	      if (p == 0)
		return(s7_make_integer(sc, 1));
	      
	      n = numerator(sc->x->object.number);
	      d = denominator(sc->x->object.number);
	      
	      if ((top_log > log(s7_Int_abs(n)) * s7_Int_abs(p)) &&
		  (top_log > log(d) * s7_Int_abs(p)))	
		{
		  if (p > 0)
		    return(s7_make_ratio(sc, s7_Int_pow(n, p), s7_Int_pow(d, p)));
		  return(s7_make_ratio(sc, s7_Int_pow(d, -p), s7_Int_pow(n, -p)));
		}
	    }
	  /* occasionally int^rat can be int but it happens so infrequently it's not worth checking */
	}
    }
  
  if ((s7_is_real(sc->x)) &&
      (s7_is_real(sc->y)))
    {
      s7_Double x, y;
      x = num_to_real(sc->x->object.number);
      y = num_to_real(sc->y->object.number);
      if (((x > 0.0) && (y >= 0.0)) ||
	  ((y - floor(y)) == 0.0))
	return(s7_make_real(sc, pow(x, y)));
    }
  
  return(s7_from_c_complex(sc, cpow(s7_complex(sc->x), s7_complex(sc->y))));
}


static s7_pointer g_floor(s7_scheme *sc, s7_pointer args)
{
  #define H_floor "(floor x) returns the integer closest to x toward -inf"
  if (!s7_is_real(car(args)))
    return(s7_wrong_type_arg_error(sc, "floor", 1, car(args), "a real"));
  sc->x = car(args);
  return(s7_make_integer(sc, (s7_Int)floor(num_to_real(sc->x->object.number)))); /* used to be real result */
}


static s7_pointer g_ceiling(s7_scheme *sc, s7_pointer args)
{
  #define H_ceiling "(ceiling x) returns the integer closest to x toward inf"
  if (!s7_is_real(car(args)))
    return(s7_wrong_type_arg_error(sc, "ceiling", 1, car(args), "a real"));
  sc->x = car(args);
  return(s7_make_integer(sc, (s7_Int)ceil(num_to_real(sc->x->object.number))));
}


static s7_pointer g_truncate(s7_scheme *sc, s7_pointer args)
{
  #define H_truncate "(truncate x) returns the integer closest to x toward 0"
  if (!s7_is_real(car(args)))
    return(s7_wrong_type_arg_error(sc, "truncate", 1, car(args), "a real"));
  return(s7_make_integer(sc, s7_truncate(num_to_real(car(args)->object.number))));
}


static s7_pointer g_round(s7_scheme *sc, s7_pointer args)
{
  #define H_round "(round x) returns the integer closest to x"
  if (!s7_is_real(car(args)))
    return(s7_wrong_type_arg_error(sc, "round", 1, car(args), "a real"));
  sc->x = car(args);
  return(s7_make_integer(sc, (s7_Int)round_per_R5RS(num_to_real(sc->x->object.number))));
}


static s7_pointer g_lcm(s7_scheme *sc, s7_pointer args)
{
  int i;
  s7_Int n = 1, d = 0;
  bool rats = false;
  #define H_lcm "(lcm ...) returns the least common multiple of its rational arguments"
  
  for (i = 1, sc->x = args; sc->x != sc->NIL; i++, sc->x = cdr(sc->x)) 
    if (!s7_is_rational(car(sc->x)))
      return(s7_wrong_type_arg_error(sc, "lcm", i, car(sc->x), "an integer or ratio"));
    else rats = ((rats) || (object_number_type(car(sc->x)) == NUM_RATIO));

  if (!rats)
    {
      for (sc->x = args; sc->x != sc->NIL; sc->x = cdr(sc->x)) 
	{
	  n = c_lcm(n, s7_integer(car(sc->x)));
	  if (n == 0)
	    return(s7_make_integer(sc, 0));
	}
      return(s7_make_integer(sc, n));
    }
  /* from A Jaffer */
  for (sc->x = args; sc->x != sc->NIL; sc->x = cdr(sc->x)) 
    {
      n = c_lcm(n, s7_numerator(car(sc->x)));
      if (n == 0)
	return(s7_make_integer(sc, 0));
      d = c_gcd(d, s7_denominator(car(sc->x)));
    }
  return(s7_make_ratio(sc, n, d));
}


static s7_pointer g_gcd(s7_scheme *sc, s7_pointer args)
{
  int i;
  bool rats = false;
  s7_Int n = 0, d = 1;
  #define H_gcd "(gcd ...) returns the greatest common divisor of its rational arguments"
  
  for (i = 1, sc->x = args; sc->x != sc->NIL; i++, sc->x = cdr(sc->x)) 
    if (!s7_is_rational(car(sc->x)))
      return(s7_wrong_type_arg_error(sc, "gcd", i, car(sc->x), "an integer"));
    else rats = ((rats) || (object_number_type(car(sc->x)) == NUM_RATIO));
  
  if (!rats)
    {
      for (sc->x = args; sc->x != sc->NIL; sc->x = cdr(sc->x)) 
	{
	  n = c_gcd(n, s7_integer(car(sc->x)));
	  if (n == 1)
	    return(s7_make_integer(sc, 1));
	}
      return(s7_make_integer(sc, n));
    }
  /* from A Jaffer */
  for (sc->x = args; sc->x != sc->NIL; sc->x = cdr(sc->x)) 
    {
      n = c_gcd(n, s7_numerator(car(sc->x)));
      d = c_lcm(d, s7_denominator(car(sc->x)));
    }
  return(s7_make_ratio(sc, n, d));
}


static s7_pointer g_add(s7_scheme *sc, s7_pointer args)
{
  #define H_add "(+ ...) adds its arguments"
  int i;
  sc->v = small_int_as_num(sc, 0);
  for (i = 1, sc->x = args; sc->x != sc->NIL; i++, sc->x = cdr(sc->x)) 
    {
      if (!s7_is_number(car(sc->x)))
	return(s7_wrong_type_arg_error(sc, "+", i, car(sc->x), "a number"));
      sc->v = num_add(sc->v, nvalue(car(sc->x)));
    }
  return(make_number(sc, sc->v));
}


static s7_pointer g_subtract(s7_scheme *sc, s7_pointer args)
{
  #define H_subtract "(- x1 ...) subtract its trailing arguments from the first, or negates the first if only one argument is given"
  int i;
  if (!s7_is_number(car(args)))
    return(s7_wrong_type_arg_error(sc, "-", 1, car(args), "a number"));
  
  if (cdr(args) == sc->NIL) 
    {
      sc->x = args;
      sc->v = small_int_as_num(sc, 0);
    } 
  else 
    {
      sc->x = cdr(args);
      sc->v = nvalue(car(args));
    }
  for (i = 2; sc->x != sc->NIL; i++, sc->x = cdr(sc->x)) 
    {
      if (!s7_is_number(car(sc->x)))
	return(s7_wrong_type_arg_error(sc, "-", i, car(sc->x), "a number"));
      sc->v = num_sub(sc->v, nvalue(car(sc->x)));
    }
  return(make_number(sc, sc->v));
}


static s7_pointer g_multiply(s7_scheme *sc, s7_pointer args)
{
  #define H_multiply "(* ...) multiplies its arguments"
  int i;
  sc->v = small_int_as_num(sc, 1);
  for (i = 1, sc->x = args; sc->x != sc->NIL; i++, sc->x = cdr(sc->x)) 
    {
      if (!s7_is_number(car(sc->x)))
	return(s7_wrong_type_arg_error(sc, "*", i, car(sc->x), "a number"));
      sc->v = num_mul(sc->v, nvalue(car(sc->x)));
    }
  return(make_number(sc, sc->v));
}


static s7_pointer g_divide(s7_scheme *sc, s7_pointer args)
{
  #define H_divide "(- x1 ...) divides its first argument by the rest, or inverts the first if there is only one argument"
  int i;
  if (!s7_is_number(car(args)))
    return(s7_wrong_type_arg_error(sc, "/", 1, car(args), "a number"));
  
  if (cdr(args) == sc->NIL) 
    {
      sc->x = args;
      sc->v = small_int_as_num(sc, 1);
    } 
  else 
    {
      sc->x = cdr(args);
      sc->v = nvalue(car(args));
    }
  for (i = 2; sc->x != sc->NIL; i++, sc->x = cdr(sc->x))
    {
      
      if (!s7_is_number(car(sc->x)))
	return(s7_wrong_type_arg_error(sc, "/", i, car(sc->x), "a number"));
      if (s7_is_zero(car(sc->x)))
	return(s7_division_by_zero_error(sc, "/", args));
      
      sc->v = num_div(sc->v, nvalue(car(sc->x)));
    }
  return(make_number(sc, sc->v));
}


static s7_pointer g_max(s7_scheme *sc, s7_pointer args)
{
  #define H_max "(max ...) returns the maximum of its arguments"
  int i;
  if (!s7_is_real(car(args)))
    return(s7_wrong_type_arg_error(sc, "max", 1, car(args), "a real"));
  sc->v = nvalue(car(args));
  for (i = 2, sc->x = cdr(args); sc->x != sc->NIL; i++, sc->x = cdr(sc->x)) 
    {
      if (!s7_is_real(car(sc->x)))
	return(s7_wrong_type_arg_error(sc, "max", i, car(sc->x), "a real"));
      sc->v = num_max(sc->v, nvalue(car(sc->x)));
    }
  return(make_number(sc, sc->v));
}


static s7_pointer g_min(s7_scheme *sc, s7_pointer args)
{
  #define H_min "(min ...) returns the minimum of its arguments"
  int i;
  if (!s7_is_real(car(args)))
    return(s7_wrong_type_arg_error(sc, "min", 1, car(args), "a real"));
  sc->v = nvalue(car(args));
  for (i = 2, sc->x = cdr(args); sc->x != sc->NIL; i++, sc->x = cdr(sc->x)) 
    {
      if (!s7_is_real(car(sc->x)))
	return(s7_wrong_type_arg_error(sc, "min", i, car(sc->x), "a real"));
      sc->v = num_min(sc->v, nvalue(car(sc->x)));
    }
  return(make_number(sc, sc->v));
}


static s7_pointer g_quotient(s7_scheme *sc, s7_pointer args)
{
  #define H_quotient "(quotient x1 x2) returns the integer quotient of x1 and x2"
  
  if (!s7_is_real(car(args)))
    return(s7_wrong_type_arg_error(sc, "quotient", 1, car(args), "a real"));
  if (!s7_is_real(cadr(args)))
    return(s7_wrong_type_arg_error(sc, "quotient", 2, cadr(args), "a real"));
  
  sc->v = nvalue(car(args));
  if (!s7_is_zero(cadr(args)))
    sc->v = num_quotient(sc->v, nvalue(cadr(args)));
  else return(s7_division_by_zero_error(sc, "quotient", args));
  return(make_number(sc, sc->v));
}


static s7_pointer g_remainder(s7_scheme *sc, s7_pointer args)
{
  #define H_remainder "(remainder x1 x2) returns the integer remainder of x1 and x2"
  
  if (!s7_is_real(car(args)))
    return(s7_wrong_type_arg_error(sc, "remainder", 1, car(args), "a real"));
  if (!s7_is_real(cadr(args)))
    return(s7_wrong_type_arg_error(sc, "remainder", 2, cadr(args), "a real"));
  
  sc->v = nvalue(car(args));
  if (!s7_is_zero(cadr(args)))
    sc->v = num_rem(sc->v, nvalue(cadr(args)));
  else return(s7_division_by_zero_error(sc, "remainder", args));
  return(make_number(sc, sc->v));
}


static s7_pointer g_modulo(s7_scheme *sc, s7_pointer args)
{
  #define H_modulo "(modulo x1 x2) returns x1 mod x2"
  
  if (!s7_is_real(car(args)))
    return(s7_wrong_type_arg_error(sc, "modulo", 1, car(args), "a real"));
  if (!s7_is_real(cadr(args)))
    return(s7_wrong_type_arg_error(sc, "modulo", 2, cadr(args), "a real"));
  
  sc->v = nvalue(car(args));
  if (!s7_is_zero(cadr(args)))
    sc->v = num_mod(sc->v, nvalue(cadr(args)));
  else return(s7_division_by_zero_error(sc, "modulo", args));
  return(make_number(sc, sc->v));
}


enum {N_EQUAL, N_LESS, N_GREATER, N_LESS_OR_EQUAL, N_GREATER_OR_EQUAL};

static s7_pointer compare_numbers(s7_scheme *sc, int op, s7_pointer args)
{
  int i;
  bool (*comp_func)(num a, num b) = NULL;
  bool (*arg_checker)(s7_pointer x) = NULL;
  const char *arg_type = NULL, *op_name = NULL;
  switch (op)
    {
    case N_EQUAL:            comp_func = num_eq; arg_checker = s7_is_number; arg_type = "number"; op_name = "=";  break;
    case N_LESS:             comp_func = num_lt; arg_checker = s7_is_real;   arg_type = "real";   op_name = "<";  break;
    case N_GREATER:          comp_func = num_gt; arg_checker = s7_is_real;   arg_type = "real";   op_name = ">";  break;
    case N_LESS_OR_EQUAL:    comp_func = num_le; arg_checker = s7_is_real;   arg_type = "real";   op_name = "<="; break;
    case N_GREATER_OR_EQUAL: comp_func = num_ge; arg_checker = s7_is_real;   arg_type = "real";   op_name = ">="; break;
    }
  
  for (i = 1, sc->x = args; sc->x != sc->NIL; i++, sc->x = cdr(sc->x))
    if (!arg_checker(car(sc->x)))
      s7_wrong_type_arg_error(sc, op_name, i, car(sc->x), arg_type);
  
  sc->v = nvalue(car(args));
  for (sc->x = cdr(args); sc->x != sc->NIL; sc->x = cdr(sc->x)) 
    {
      if (!comp_func(sc->v, nvalue(car(sc->x)))) 
	return(sc->F);
      sc->v = nvalue(car(sc->x));
    }
  return(sc->T);
}


static s7_pointer g_equal(s7_scheme *sc, s7_pointer args)
{
  #define H_equal "(= z1 ...) returns #t if all its arguments are equal"
  return(compare_numbers(sc, N_EQUAL, args));
}


static s7_pointer g_less(s7_scheme *sc, s7_pointer args)
{
  #define H_less "(< x1 ...) returns #t if its arguments are in increasing order"
  return(compare_numbers(sc, N_LESS, args));
}


static s7_pointer g_greater(s7_scheme *sc, s7_pointer args)
{
  #define H_greater "(> x1 ...) returns #t if its arguments are in decreasing order"
  return(compare_numbers(sc, N_GREATER, args));
}


static s7_pointer g_less_or_equal(s7_scheme *sc, s7_pointer args)
{
  #define H_less_or_equal "(<= x1 ...) returns #t if its arguments are in increasing order"
  return(compare_numbers(sc, N_LESS_OR_EQUAL, args));
}


static s7_pointer g_greater_or_equal(s7_scheme *sc, s7_pointer args)
{
  #define H_greater_or_equal "(>= x1 ...) returns #t if its arguments are in decreasing order"
  return(compare_numbers(sc, N_GREATER_OR_EQUAL, args));
}


static s7_pointer g_real_part(s7_scheme *sc, s7_pointer args)
{
  #define H_real_part "(real-part num) returns the real part of num"
  if (!s7_is_number(car(args)))
    return(s7_wrong_type_arg_error(sc, "real-part", 1, car(args), "a number"));
  return(s7_make_real(sc, s7_real_part(car(args))));
}


static s7_pointer g_imag_part(s7_scheme *sc, s7_pointer args)
{
  #define H_imag_part "(imag-part num) returns the imaginary part of num"
  if (!s7_is_number(car(args)))
    return(s7_wrong_type_arg_error(sc, "imag-part", 1, car(args), "a number"));
  return(s7_make_real(sc, s7_imag_part(car(args))));
}


static s7_pointer g_numerator(s7_scheme *sc, s7_pointer args)
{
  #define H_numerator "(numerator rat) returns the numerator of the rational number rat"
  if (!s7_is_rational(car(args)))
    return(s7_wrong_type_arg_error(sc, "numerator", 1, car(args), "a rational"));
  return(s7_make_integer(sc, num_to_numerator((car(args))->object.number)));
}


static s7_pointer g_denominator(s7_scheme *sc, s7_pointer args)
{
  #define H_denominator "(denominator rat) returns the denominator of the rational number rat"
  if (!s7_is_rational(car(args)))
    return(s7_wrong_type_arg_error(sc, "denominator", 1, car(args), "a rational"));
  return(s7_make_integer(sc, num_to_denominator((car(args))->object.number)));
}


static s7_pointer g_is_number(s7_scheme *sc, s7_pointer args) 
{
  #define H_is_number "(number? obj) returns #t if obj is a number"
  return(make_boolean(sc, s7_is_number(car(args))));
}


static s7_pointer g_is_integer(s7_scheme *sc, s7_pointer args) 
{
  #define H_is_integer "(integer? obj) returns #t if obj is an integer"
  return(make_boolean(sc, s7_is_integer(car(args))));
}


static s7_pointer g_is_real(s7_scheme *sc, s7_pointer args) 
{
  #define H_is_real "(real? obj) returns #t if obj is a real number"
  return(make_boolean(sc, s7_is_real(car(args))));
}


static s7_pointer g_is_complex(s7_scheme *sc, s7_pointer args) 
{
  #define H_is_complex "(complex? obj) returns #t if obj is a complex number"
  return(make_boolean(sc, s7_is_complex(car(args))));
}


static s7_pointer g_is_rational(s7_scheme *sc, s7_pointer args) 
{
  #define H_is_rational "(rational? obj) returns #t if obj is a rational number"
  return(make_boolean(sc, s7_is_rational(car(args))));
}


static s7_pointer g_is_even(s7_scheme *sc, s7_pointer args)
{
  #define H_is_even "(even? int) returns #t if the integer int is even"
  if (!s7_is_integer(car(args)))
    return(s7_wrong_type_arg_error(sc, "even?", 1, car(args), "an integer"));
  return(make_boolean(sc, (s7_integer(car(args)) & 1) == 0));
}


static s7_pointer g_is_odd(s7_scheme *sc, s7_pointer args)
{
  #define H_is_odd "(odd? int) returns #t if the integer int is odd"
  if (!s7_is_integer(car(args)))
    return(s7_wrong_type_arg_error(sc, "odd?", 1, car(args), "an integer"));
  return(make_boolean(sc, (s7_integer(car(args)) & 1) == 1));
}


static s7_pointer g_is_zero(s7_scheme *sc, s7_pointer args)
{
  #define H_is_zero "(zero? num) returns #t if the number num is zero"
  if (!s7_is_number(car(args)))
    return(s7_wrong_type_arg_error(sc, "zero?", 1, car(args), "a number"));
  return(make_boolean(sc, s7_is_zero(car(args))));
}


static s7_pointer g_is_positive(s7_scheme *sc, s7_pointer args)
{
  #define H_is_positive "(positive? num) returns #t if the real number num is positive"
  if (!s7_is_real(car(args)))
    return(s7_wrong_type_arg_error(sc, "positive?", 1, car(args), "a real"));
  return(make_boolean(sc, s7_is_positive(car(args))));
}


static s7_pointer g_is_negative(s7_scheme *sc, s7_pointer args)
{
  #define H_is_negative "(negative? num) returns #t if the real number num is negative"
  if (!s7_is_real(car(args)))
    return(s7_wrong_type_arg_error(sc, "negative?", 1, car(args), "a real"));
  return(make_boolean(sc, s7_is_negative(car(args))));
}


static s7_pointer g_inexact_to_exact(s7_scheme *sc, s7_pointer args)
{
  #define H_inexact_to_exact "(inexact->exact num) converts num to an exact number"
  s7_Int numer = 0, denom = 1;
  if (!s7_is_number(car(args)))
    return(s7_wrong_type_arg_error(sc, "inexact->exact", 1, car(args), "a number"));
  sc->x = car(args);
  if (s7_is_exact(sc->x)) 
    return(sc->x);
  if (c_rationalize(s7_real(sc->x), default_rationalize_error, &numer, &denom))
    return(s7_make_ratio(sc, numer, denom));
  return(s7_make_integer(sc, (s7_Int)s7_real(sc->x)));
}


static s7_pointer g_exact_to_inexact(s7_scheme *sc, s7_pointer args)
{
  #define H_exact_to_inexact "(exact->inexact num) converts num to an inexact number"
  if (!s7_is_number(car(args)))
    return(s7_wrong_type_arg_error(sc, "exact->inexact", 1, car(args), "a number"));
  sc->x = car(args);
  if (s7_is_inexact(sc->x)) 
    return(sc->x);
  if (s7_is_integer(sc->x))
    return(s7_make_real(sc, (s7_Double)s7_integer(sc->x)));
  return(s7_make_real(sc, fraction(sc->x->object.number)));
}


static s7_pointer g_is_exact(s7_scheme *sc, s7_pointer args)
{
  #define H_is_exact "(exact? num) returns #t if num is exact"
  if (!s7_is_number(car(args)))
    return(s7_wrong_type_arg_error(sc, "exact?", 1, car(args), "a number"));
  return(make_boolean(sc, s7_is_exact(car(args))));
}


static s7_pointer g_is_inexact(s7_scheme *sc, s7_pointer args)
{
  #define H_is_inexact "(inexact? num) returns #t if num is inexact"
  if (!s7_is_number(car(args)))
    return(s7_wrong_type_arg_error(sc, "inexact?", 1, car(args), "a number"));
  return(make_boolean(sc, s7_is_inexact(car(args))));
}


bool s7_is_ulong(s7_pointer arg)
{
  return(s7_is_integer(arg));
}


unsigned long s7_ulong(s7_pointer num)
{
  return((unsigned long)s7_integer(num));
}


s7_pointer s7_make_ulong(s7_scheme *sc, unsigned long num)
{
  return(s7_make_integer(sc, num));
}


static s7_pointer g_logior(s7_scheme *sc, s7_pointer args)
{
  #define H_logior "(logior i1 ...) returns the bitwise OR of its integer arguments"
  int result = 0, i;
  s7_pointer x;
  for (i = 0, x = args; x != sc->NIL; i++, x = cdr(x))
    if (!s7_is_integer(car(x)))
      return(s7_wrong_type_arg_error(sc, "logior", i, car(x), "an integer"));
    else result |= s7_integer(car(x));
  return(s7_make_integer(sc, result));
}


static s7_pointer g_logxor(s7_scheme *sc, s7_pointer args)
{
  #define H_logxor "(logxor i1 ...) returns the bitwise XOR of its integer arguments"
  int result = 0, i;
  s7_pointer x;
  for (i = 0, x = args; x != sc->NIL; i++, x = cdr(x))
    if (!s7_is_integer(car(x)))
      return(s7_wrong_type_arg_error(sc, "logxor", i, car(x), "an integer"));
    else result ^= s7_integer(car(x));
  return(s7_make_integer(sc, result));
}


static s7_pointer g_logand(s7_scheme *sc, s7_pointer args)
{
  #define H_logand "(logand i1 ...) returns the bitwise AND of its integer arguments"
  int result = -1, i;
  s7_pointer x;
  for (i = 0, x = args; x != sc->NIL; i++, x = cdr(x))
    if (!s7_is_integer(car(x)))
      return(s7_wrong_type_arg_error(sc, "logand", i, car(x), "an integer"));
    else result &= s7_integer(car(x));
  return(s7_make_integer(sc, result));
}


static s7_pointer g_lognot(s7_scheme *sc, s7_pointer args)
{
  #define H_lognot "(lognot i1) returns the bitwise negation of i1"
  if (!s7_is_integer(car(args)))
    return(s7_wrong_type_arg_error(sc, "lognot", 1, car(args), "an integer"));
  return(s7_make_integer(sc, ~s7_integer(car(args))));
}


static s7_pointer g_ash(s7_scheme *sc, s7_pointer args)
{
  #define H_ash "(ash i1 i2) returns i1 shifted right or left i2 times, i1 << i2"
  s7_Int arg1, arg2;
  if (!s7_is_integer(car(args)))
    return(s7_wrong_type_arg_error(sc, "ash", 1, car(args), "an integer"));
  if (!s7_is_integer(cadr(args)))
    return(s7_wrong_type_arg_error(sc, "ash", 2, cadr(args), "an integer"));
  
  arg1 = s7_integer(car(args));
  arg2 = s7_integer(cadr(args));
  if (arg2 >= 0)
    return(s7_make_integer(sc, arg1 << arg2));
  return(s7_make_integer(sc, arg1 >> -arg2));
}




/* -------------------------------- characters -------------------------------- */

static s7_pointer g_char_to_integer(s7_scheme *sc, s7_pointer args)
{
  #define H_char_to_integer "(char->integer c) converts the character c to an integer"
  if (!s7_is_character(car(args)))
    return(s7_wrong_type_arg_error(sc, "char->integer", 1, car(args), "a character"));
  return(s7_make_integer(sc, (unsigned char)character(car(args))));
}


static s7_pointer g_integer_to_char(s7_scheme *sc, s7_pointer args)
{
  #define H_integer_to_char "(integer->char i) converts the non-negative integer i to a character"
  if ((!s7_is_integer(car(args))) || 
      (s7_integer(car(args)) < 0) ||
      (s7_integer(car(args)) > 255))
    return(s7_wrong_type_arg_error(sc, "integer->char", 1, car(args), "an integer between 0 and 255"));
  return(s7_make_character(sc, (char)s7_integer(car(args))));
}


static s7_pointer g_char_upcase(s7_scheme *sc, s7_pointer args)
{
  #define H_char_upcase "(char-upcase c) converts the character c to upper case"
  if (!s7_is_character(car(args)))
    return(s7_wrong_type_arg_error(sc, "char-upcase", 1, car(args), "a character"));
  return(s7_make_character(sc, (char)toupper((unsigned char)character(car(args)))));
}


static s7_pointer g_char_downcase(s7_scheme *sc, s7_pointer args)
{
  #define H_char_downcase "(char-downcase c) converts the character c to lower case"
  if (!s7_is_character(car(args)))
    return(s7_wrong_type_arg_error(sc, "char-downcase", 1, car(args), "a character"));
  return(s7_make_character(sc, (char)tolower((unsigned char)character(car(args)))));
}


static s7_pointer g_is_char_alphabetic(s7_scheme *sc, s7_pointer args)
{
  #define H_is_char_alphabetic "(char-alphabetic? c) returns #t if the character c is alphabetic"
  if (!s7_is_character(car(args)))
    return(s7_wrong_type_arg_error(sc, "char-alphabetic?", 1, car(args), "a character"));
  return(make_boolean(sc, isalpha(character(car(args)))));
}


static s7_pointer g_is_char_numeric(s7_scheme *sc, s7_pointer args)
{
  #define H_is_char_numeric "(char-numeric? c) returns #t if the character c is a digit"
  if (!s7_is_character(car(args)))
    return(s7_wrong_type_arg_error(sc, "char-numeric?", 1, car(args), "a character"));
  return(make_boolean(sc, isdigit(character(car(args)))));
}


static s7_pointer g_is_char_whitespace(s7_scheme *sc, s7_pointer args)
{
  #define H_is_char_whitespace "(char-whitespace? c) returns #t if the character c is non-printing character"
  if (!s7_is_character(car(args)))
    return(s7_wrong_type_arg_error(sc, "char-whitespace?", 1, car(args), "a character"));
  return(make_boolean(sc, isspace(character(car(args)))));
}


static s7_pointer g_is_char_upper_case(s7_scheme *sc, s7_pointer args)
{
  #define H_is_char_upper_case "(char-upper-case? c) returns #t if the character c is in upper case"
  if (!s7_is_character(car(args)))
    return(s7_wrong_type_arg_error(sc, "char-upper-case?", 1, car(args), "a character"));
  return(make_boolean(sc, isupper(character(car(args)))));
}


static s7_pointer g_is_char_lower_case(s7_scheme *sc, s7_pointer args)
{
  #define H_is_char_lower_case "(char-lower-case? c) returns #t if the character c is in lower case"
  if (!s7_is_character(car(args)))
    return(s7_wrong_type_arg_error(sc, "char-lower-case?", 1, car(args), "a character"));
  return(make_boolean(sc, islower(character(car(args)))));
}


static s7_pointer g_is_char(s7_scheme *sc, s7_pointer args)
{
  #define H_is_char "(char? obj) returns #t if obj is a character"
  return(make_boolean(sc, s7_is_character(car(args))));
}


s7_pointer s7_make_character(s7_scheme *sc, int c) 
{
  s7_pointer x = new_cell(sc);
  set_type(x, T_CHARACTER | T_ATOM | T_SIMPLE | T_DONT_COPY);
  character(x) = c;
  return(x);
}


bool s7_is_character(s7_pointer p) 
{ 
  return(type(p) == T_CHARACTER);
}


char s7_character(s7_pointer p)  
{ 
  return(character(p));
}


static int charcmp(char c1, char c2, bool ci)
{
  if (ci)
    return(charcmp(tolower(c1), tolower(c2), false));
  
  if (c1 == c2)
    return(0);
  if (c1 < c2)
    return(-1);
  return(1);
}


static s7_pointer g_char_cmp(s7_scheme *sc, s7_pointer args, int val, const char *name, bool ci)
{
  int i;
  s7_pointer x;
  char last_chr;
  
  for (i = 1, x = args; x != sc->NIL; i++, x = cdr(x))  
    if (!s7_is_character(car(x)))
      return(s7_wrong_type_arg_error(sc, name, i, car(x), "a character"));
  
  last_chr = character(car(args));
  for (i = 2, x = cdr(args); x != sc->NIL; i++, x = cdr(x))
    {
      if (charcmp(last_chr, character(car(x)), ci) != val)
	return(sc->F);
      last_chr = character(car(x));
    }
  return(sc->T);
}


static s7_pointer g_char_cmp_not(s7_scheme *sc, s7_pointer args, int val, const char *name, bool ci)
{
  int i;
  s7_pointer x;
  char last_chr;
  
  for (i = 1, x = args; x != sc->NIL; i++, x = cdr(x))  
    if (!s7_is_character(car(x)))
      return(s7_wrong_type_arg_error(sc, name, i, car(x), "a character"));
  
  last_chr = character(car(args));
  for (i = 2, x = cdr(args); x != sc->NIL; i++, x = cdr(x))
    {
      if (charcmp(last_chr, character(car(x)), ci) == val)
	return(sc->F);
      last_chr = character(car(x));
    }
  return(sc->T);
}


static s7_pointer g_chars_are_equal(s7_scheme *sc, s7_pointer args)
{
  #define H_chars_are_equal "(char=? chr...) returns #t if all the character arguments are equal"
  return(g_char_cmp(sc, args, 0, "char=?", false));
}	


static s7_pointer g_chars_are_less(s7_scheme *sc, s7_pointer args)
{
  #define H_chars_are_less "(char<? chr...) returns #t if all the character arguments are increasing"
  return(g_char_cmp(sc, args, -1, "char<?", false));
}	


static s7_pointer g_chars_are_greater(s7_scheme *sc, s7_pointer args)
{
  #define H_chars_are_greater "(char>? chr...) returns #t if all the character arguments are decreasing"
  return(g_char_cmp(sc, args, 1, "char>?", false));
}


static s7_pointer g_chars_are_geq(s7_scheme *sc, s7_pointer args)
{
  #define H_chars_are_geq "(char>=? chr...) returns #t if all the character arguments are equal or decreasing"
  return(g_char_cmp_not(sc, args, -1, "char>=?", false));
}	


static s7_pointer g_chars_are_leq(s7_scheme *sc, s7_pointer args)
{
  #define H_chars_are_leq "(char<=? chr...) returns #t if all the character arguments are equal or increasing"
  return(g_char_cmp_not(sc, args, 1, "char<=?", false));
}


static s7_pointer g_chars_are_ci_equal(s7_scheme *sc, s7_pointer args)
{
  #define H_chars_are_ci_equal "(char-ci=? chr...) returns #t if all the character arguments are equal, ignoring case"
  return(g_char_cmp(sc, args, 0, "char-ci=?", true));
}


static s7_pointer g_chars_are_ci_less(s7_scheme *sc, s7_pointer args)
{
  #define H_chars_are_ci_less "(char-ci<? chr...) returns #t if all the character arguments are increasing, ignoring case"
  return(g_char_cmp(sc, args, -1, "char-ci<?", true));
}	


static s7_pointer g_chars_are_ci_greater(s7_scheme *sc, s7_pointer args)
{
  #define H_chars_are_ci_greater "(char-ci>? chr...) returns #t if all the character arguments are decreasing, ignoring case"
  return(g_char_cmp(sc, args, 1, "char-ci>?", true));
}	


static s7_pointer g_chars_are_ci_geq(s7_scheme *sc, s7_pointer args)
{
  #define H_chars_are_ci_geq "(char-ci>=? chr...) returns #t if all the character arguments are equal or decreasing, ignoring case"
  return(g_char_cmp_not(sc, args, -1, "char-ci>=?", true));
}


static s7_pointer g_chars_are_ci_leq(s7_scheme *sc, s7_pointer args)
{
  #define H_chars_are_ci_leq "(char-ci<=? chr...) returns #t if all the character arguments are equal or increasing, ignoring case"
  return(g_char_cmp_not(sc, args, 1, "char-ci<=?", true));
}




/* -------------------------------- strings -------------------------------- */


s7_pointer s7_make_string_with_length(s7_scheme *sc, const char *str, int len) 
{
  s7_pointer x;
  
  x = new_cell(sc);
  set_type(x, T_STRING | T_ATOM | T_FINALIZABLE | T_SIMPLE | T_DONT_COPY);
  if (str)
    string_value(x) = strdup(str);
  else string_value(x) = NULL;
  string_length(x) = len;
  return(x);
}


static s7_pointer make_empty_string(s7_scheme *sc, int len, char fill) 
{
  s7_pointer x;
  x = new_cell(sc);
  set_type(x, T_STRING | T_ATOM | T_FINALIZABLE | T_SIMPLE | T_DONT_COPY);
  string_value(x) = (char *)calloc(len + 1, sizeof(char));
  if (fill != 0)
    memset((void *)(string_value(x)), fill, len);
  string_length(x) = len;
  return(x);
}


s7_pointer s7_make_string(s7_scheme *sc, const char *str) 
{
  return(s7_make_string_with_length(sc, str, safe_strlen(str)));
}


static s7_pointer s7_make_permanent_string(const char *str) 
{
  /* for the symbol table which is never GC'd */
  s7_pointer x;
  x = (s7_cell *)malloc(sizeof(s7_cell));
  set_type(x, T_STRING | T_ATOM | T_SIMPLE | T_CONSTANT | T_IMMUTABLE | T_DONT_COPY);
  if (str)
    {
      string_value(x) = strdup(str);
      string_length(x) = strlen(str);
    }
  else 
    {
      string_value(x) = NULL;
      string_length(x) = 0;
    }
  return(x);
}


bool s7_is_string(s7_pointer p)
{
  return((type(p) == T_STRING)); 
}


const char *s7_string(s7_pointer p) 
{ 
  return(string_value(p));
}


static s7_pointer g_is_string(s7_scheme *sc, s7_pointer args)
{
  #define H_is_string "(string? obj) returns #t if obj is a string"
  return(make_boolean(sc, s7_is_string(car(args))));
}


static s7_pointer g_make_string(s7_scheme *sc, s7_pointer args)
{
  #define H_make_string "(make-string len :optional val) makes a string of length len filled with the character val (default: space)"
  int len;
  char fill = ' ';
  
  if ((!s7_is_integer(car(args))) || (s7_integer(car(args)) < 0))
    return(s7_wrong_type_arg_error(sc, "make-string", 1, car(args), "a non-negative integer"));
  
  len = s7_integer(car(args));
  if (cdr(args) != sc->NIL) 
    {
      if (!s7_is_character(cadr(args)))
	return(s7_wrong_type_arg_error(sc, "make-string", 2, cadr(args), "a character"));
      fill = s7_character(cadr(args));
    }
  return(make_empty_string(sc, len, fill));
}


static s7_pointer g_string_length(s7_scheme *sc, s7_pointer args)
{
  #define H_string_length "(string-length str) returns the length of the string str"
  if (!s7_is_string(car(args)))
    return(s7_wrong_type_arg_error(sc, "string-length", 1, car(args), "string"));
  return(s7_make_integer(sc, string_length(car(args))));
}


static s7_pointer g_string_ref(s7_scheme *sc, s7_pointer args)
{
  #define H_string_ref "(string-ref str index) returns the character at the index-th element of the string str"
  
  s7_pointer index;
  char *str;
  index = cadr(args);
  
  if (!s7_is_string(car(args)))
    return(s7_wrong_type_arg_error(sc, "string-ref", 1, car(args), "a string"));
  
  if ((!s7_is_integer(index)) || (s7_integer(index) < 0))
    return(s7_wrong_type_arg_error(sc, "string-ref", 2, index, "a non-negative integer"));
  if (s7_integer(index) >= string_length(car(args)))
    return(s7_out_of_range_error(sc, "string-ref", 2, index, "less than string length"));
  
  str = string_value(car(args));
  return(s7_make_character(sc, ((unsigned char*)str)[s7_integer(index)]));
}


static s7_pointer g_string_set(s7_scheme *sc, s7_pointer args)
{
  #define H_string_set "(string-set! str index chr) sets the index-th element of the string str to the character chr"
  
  s7_pointer index;
  char *str;
  index = cadr(args);
  
  if (!s7_is_string(car(args)))
    return(s7_wrong_type_arg_error(sc, "string-set!", 1, car(args), "a string"));
  if (!s7_is_character(caddr(args)))
    return(s7_wrong_type_arg_error(sc, "string-set!", 3, caddr(args), "a character"));
  
  if ((!s7_is_integer(index)) || (s7_integer(index) < 0))
    return(s7_wrong_type_arg_error(sc, "string-set!", 2, index, "a non-negative integer"));
  if (s7_integer(index) >= string_length(car(args)))
    return(s7_out_of_range_error(sc, "string-set!", 2, index, "less than string length"));
  
  if (s7_is_immutable(car(args)))
    return(s7_wrong_type_arg_error(sc, "string-set!", 1, car(args), "a mutable string"));
  
  str = string_value(car(args));
  str[s7_integer(index)] = (char)s7_character(caddr(args));
  return(car(args));
}


static s7_pointer g_string_append_1(s7_scheme *sc, s7_pointer args, const char *name)
{
  int i, len = 0;
  s7_pointer newstr;
  char *pos;
  
  if (car(args) == sc->NIL)
    return(s7_make_string_with_length(sc, "", 0));
  
  /* get length for new string */
  for (i = 1, sc->x = args; sc->x != sc->NIL; i++, sc->x = cdr(sc->x)) 
    {
      if (!s7_is_string(car(sc->x)))
	return(s7_wrong_type_arg_error(sc, name, i, car(sc->x), "a string"));
      len += string_length(car(sc->x));
    }
  
  /* store the contents of the argument strings into the new string */
  newstr = make_empty_string(sc, len, 0);
  for (pos = string_value(newstr), sc->x = args; sc->x != sc->NIL; pos += string_length(car(sc->x)), sc->x = cdr(sc->x)) 
    memcpy(pos, string_value(car(sc->x)), string_length(car(sc->x)));
  
  return(newstr);
}


static s7_pointer g_string_append(s7_scheme *sc, s7_pointer args)
{
  #define H_string_append "(string-append str1 ...) appends all its string arguments into one string"
  return(g_string_append_1(sc, args, "string-append"));
}


static s7_pointer g_string_copy(s7_scheme *sc, s7_pointer args)
{
  #define H_string_copy "(string-copy str) returns a copy of its string argument"
  if (car(args) == sc->NIL)
    return(s7_wrong_type_arg_error(sc, "string-copy", 1, car(args), "a string"));
  
  return(g_string_append_1(sc, args, "string-copy"));
}


static s7_pointer g_substring(s7_scheme *sc, s7_pointer args)
{
  #define H_substring "(substring str start :optional end) returns the portion of the string str between start and end"
  
  s7_pointer start, end, str;
  int i0, i1, len;
  char *s;
  
  str = car(args);
  start = cadr(args);
  
  if (!s7_is_string(str))
    return(s7_wrong_type_arg_error(sc, "substring", 1, car(args), "a string"));
  
  if ((!s7_is_integer(start)) || (s7_integer(start) < 0))
    return(s7_wrong_type_arg_error(sc, "substring", 2, start, "a non-negative integer"));
  
  if (cddr(args) != sc->NIL)
    {
      end = caddr(args);
      if ((!s7_is_integer(end)) || (s7_integer(end) < 0))
	return(s7_wrong_type_arg_error(sc, "substring", 3, end, "an integer > start"));
      i1 = s7_integer(end);
    }
  else i1 = string_length(str);
  
  i0 = s7_integer(start);
  s = string_value(str);
  
  if ((i0 > i1) || 
      (i1 > string_length(str)))
    return(s7_out_of_range_error(sc, "substring", 2, start, "start <= end <= string length"));
  
  len = i1 - i0;
  sc->x = make_empty_string(sc, len, 0);
  memcpy(string_value(sc->x), s + i0, len);
  string_value(sc->x)[len] = 0;
  return(sc->x);
}


static s7_pointer g_object_to_string(s7_scheme *sc, s7_pointer args)
{
  #define H_object_to_string "(object->string obj) returns a string representation of obj"
  return(s7_object_to_string(sc, car(args)));
}

static int safe_strcmp(const char *s1, const char *s2)
{
  int val;
  if (s1 == NULL)
    {
      if (s2 == NULL)
	return(0);
      return(-1);
    }

  if (s2 == NULL)
    return(1);

  val = strcmp(s1, s2); /* strcmp can return stuff like -97, but we want -1, 0, or 1 */
  if (val <= -1)
    return(-1);
  if (val >= 1)
    return(1);
  return(val);
}


static s7_pointer g_string_cmp(s7_scheme *sc, s7_pointer args, int val, const char *name)
{
  int i;
  s7_pointer x;
  const char *last_str = NULL;
  
  for (i = 1, x = args; x != sc->NIL; i++, x = cdr(x))  
    if (!s7_is_string(car(x)))
      return(s7_wrong_type_arg_error(sc, name, i, car(x), "a string"));
  
  last_str = string_value(car(args));
  for (i = 2, x = cdr(args); x != sc->NIL; i++, x = cdr(x))
    {
      if (safe_strcmp(last_str, string_value(car(x))) != val)
	return(sc->F);
      last_str = string_value(car(x));
    }
  return(sc->T);
}

static s7_pointer g_string_cmp_not(s7_scheme *sc, s7_pointer args, int val, const char *name)
{
  int i;
  s7_pointer x;
  const char *last_str = NULL;
  
  for (i = 1, x = args; x != sc->NIL; i++, x = cdr(x))  
    if (!s7_is_string(car(x)))
      return(s7_wrong_type_arg_error(sc, name, i, car(x), "a string"));
  
  last_str = string_value(car(args));
  for (i = 2, x = cdr(args); x != sc->NIL; i++, x = cdr(x))
    {
      if (safe_strcmp(last_str, string_value(car(x))) == val)
	return(sc->F);
      last_str = string_value(car(x));
    }
  return(sc->T);
}


static s7_pointer g_strings_are_equal(s7_scheme *sc, s7_pointer args)
{
  #define H_strings_are_equal "(string=? str...) returns #t if all the string arguments are equal"
  return(g_string_cmp(sc, args, 0, "string=?"));
}	


static s7_pointer g_strings_are_less(s7_scheme *sc, s7_pointer args)
{
  #define H_strings_are_less "(string<? str...) returns #t if all the string arguments are increasing"
  return(g_string_cmp(sc, args, -1, "string<?"));
}	


static s7_pointer g_strings_are_greater(s7_scheme *sc, s7_pointer args)
{
  #define H_strings_are_greater "(string>? str...) returns #t if all the string arguments are decreasing"
  return(g_string_cmp(sc, args, 1, "string>?"));
}	


static s7_pointer g_strings_are_geq(s7_scheme *sc, s7_pointer args)
{
  #define H_strings_are_geq "(string>=? str...) returns #t if all the string arguments are equal or decreasing"
  return(g_string_cmp_not(sc, args, -1, "string>=?"));
}	


static s7_pointer g_strings_are_leq(s7_scheme *sc, s7_pointer args)
{
  #define H_strings_are_leq "(string<=? str...) returns #t if all the string arguments are equal or increasing"
  return(g_string_cmp_not(sc, args, 1, "string<=?"));
}	


static int safe_strcasecmp(const char *s1, const char *s2)
{
  int len1, len2;
  int i;
  if (s1 == NULL)
    {
      if (s2 == NULL)
	return(0);
      return(-1);
    }

  if (s2 == NULL)
    return(1);

  len1 = strlen(s1);
  len2 = strlen(s2);
  if (len1 < len2) 
    return(-1);
  if (len1 > len2)
    return(1);
  /* can't use idiotic strcasecmp! 9<0?? this has to be a bug in strcasecmp */
  for (i = 0; i < len1; i++)
    if (tolower(s1[i]) < tolower(s2[i]))
      return(-1);
    else
      {
	if (tolower(s1[i]) > tolower(s2[i]))
	  return(1);
      }
  return(0);
}


static s7_pointer g_string_ci_cmp(s7_scheme *sc, s7_pointer args, int val, const char *name)
{
  int i;
  s7_pointer x;
  const char *last_str = NULL;
  
  for (i = 1, x = args; x != sc->NIL; i++, x = cdr(x))  
    if (!s7_is_string(car(x)))
      return(s7_wrong_type_arg_error(sc, name, i, car(x), "a string"));
  
  last_str = string_value(car(args));
  for (i = 2, x = cdr(args); x != sc->NIL; i++, x = cdr(x))
    {
      if (safe_strcasecmp(last_str, string_value(car(x))) != val)
	return(sc->F);
      last_str = string_value(car(x));
    }
  return(sc->T);
}


static s7_pointer g_string_ci_cmp_not(s7_scheme *sc, s7_pointer args, int val, const char *name)
{
  int i;
  s7_pointer x;
  const char *last_str = NULL;
  
  for (i = 1, x = args; x != sc->NIL; i++, x = cdr(x))  
    if (!s7_is_string(car(x)))
      return(s7_wrong_type_arg_error(sc, name, i, car(x), "a string"));
  
  last_str = string_value(car(args));
  for (i = 2, x = cdr(args); x != sc->NIL; i++, x = cdr(x))
    {
      if (safe_strcasecmp(last_str, string_value(car(x))) == val)
	return(sc->F);
      last_str = string_value(car(x));
    }
  return(sc->T);
}


static s7_pointer g_strings_are_ci_equal(s7_scheme *sc, s7_pointer args)
{
  #define H_strings_are_ci_equal "(string-ci=? str...) returns #t if all the string arguments are equal, ignoring case"
  return(g_string_ci_cmp(sc, args, 0, "string-ci=?"));
}	


static s7_pointer g_strings_are_ci_less(s7_scheme *sc, s7_pointer args)
{
  #define H_strings_are_ci_less "(string-ci<? str...) returns #t if all the string arguments are increasing, ignoring case"
  return(g_string_ci_cmp(sc, args, -1, "string-ci<?"));
}	


static s7_pointer g_strings_are_ci_greater(s7_scheme *sc, s7_pointer args)
{
  #define H_strings_are_ci_greater "(string-ci>? str...) returns #t if all the string arguments are decreasing, ignoring case"
  return(g_string_ci_cmp(sc, args, 1, "string-ci>?"));
}	


static s7_pointer g_strings_are_ci_geq(s7_scheme *sc, s7_pointer args)
{
  #define H_strings_are_ci_geq "(string-ci>=? str...) returns #t if all the string arguments are equal or decreasing, ignoring case"
  return(g_string_ci_cmp_not(sc, args, -1, "string-ci>=?"));
}	


static s7_pointer g_strings_are_ci_leq(s7_scheme *sc, s7_pointer args)
{
  #define H_strings_are_ci_leq "(string-ci<=? str...) returns #t if all the string arguments are equal or increasing, ignoring case"
  return(g_string_ci_cmp_not(sc, args, 1, "string-ci<=?"));
}	


static s7_pointer g_string_fill(s7_scheme *sc, s7_pointer args)
{
  #define H_string_fill "(string-fill! str chr) fills the string str with the character chr"
  int i, len = 0;
  char *str;
  char c;
  
  if (!s7_is_string(car(args)))
    return(s7_wrong_type_arg_error(sc, "string-fill", 1, car(args), "a string"));
  if (!s7_is_character(cadr(args)))
    return(s7_wrong_type_arg_error(sc, "string-fill", 2, cadr(args), "a character"));
  
  str = string_value(car(args));
  c = character(cadr(args));
  
  if (str) len = strlen(str);
  if (len > 0)
    for (i = 0; i < len; i++)
      str[i] = c;
  return(car(args)); /* or perhaps sc->UNSPECIFIED */
}


static s7_pointer g_string_1(s7_scheme *sc, s7_pointer args, const char *name)
{
  int i, len;
  s7_pointer newstr;
  
  /* get length for new string and check arg types */
  for (len = 0, sc->x = args; sc->x != sc->NIL; len++, sc->x = cdr(sc->x)) 
    if (!s7_is_character(car(sc->x)))
      return(s7_wrong_type_arg_error(sc, name, len + 1, car(sc->x), "a character"));
  
  newstr = make_empty_string(sc, len, 0);
  for (i = 0, sc->x = args; sc->x != sc->NIL; i++, sc->x = cdr(sc->x)) 
    string_value(newstr)[i] = character(car(sc->x));
  
  return(newstr);
}


static s7_pointer g_string(s7_scheme *sc, s7_pointer args)
{
  #define H_string "(string chr...) appends all its character arguments into one string"
  if (car(args) == sc->NIL)
    return(s7_make_string_with_length(sc, "", 0));
  return(g_string_1(sc, args, "string"));
}


static s7_pointer g_is_list(s7_scheme *sc, s7_pointer args);

static s7_pointer g_list_to_string(s7_scheme *sc, s7_pointer args)
{
  #define H_list_to_string "(list->string lst) appends all the lists characters into one string"
  if (car(args) == sc->NIL)
    return(s7_make_string_with_length(sc, "", 0));
  
  if (g_is_list(sc, args) == sc->F)
    return(s7_wrong_type_arg_error(sc, "list->string", 1, car(args), "a (proper, non-circular) list of characters"));
  
  return(g_string_1(sc, car(args), "list->string"));
}


static s7_pointer g_string_to_list(s7_scheme *sc, s7_pointer args)
{
  #define H_string_to_list "(string->list str) returns the elements of the string str in a list"
  
  int i, len = 0;
  char *str;
  s7_pointer lst;
  lst = sc->NIL;
  
  if (!s7_is_string(car(args)))
    return(s7_wrong_type_arg_error(sc, "string->list", 1, car(args), "a string"));
  
  str = string_value(car(args));
  if (str) len = strlen(str);
  if (len == 0)
    return(sc->NIL);
  
  (*(sc->gc_off)) = true;
  for (i = 0; i < len; i++)
    lst = s7_cons(sc, s7_make_character(sc, str[i]), lst);
  (*(sc->gc_off)) = false;
  return(safe_reverse_in_place(sc, lst));
}




/* -------------------------------- ports -------------------------------- */

static char *describe_port(s7_scheme *sc, s7_pointer p)
{
  char *desc;
  desc = (char *)malloc(64 * sizeof(char));
  snprintf(desc, 64, "<port%s%s%s>",
  	   (is_file_port(p)) ? " file" : " string",
	   (is_input_port(p)) ? " input" : " output",
	   (port_is_closed(p)) ? " (closed)" : "");
  return(desc);
}


/* -------- port-line-number -------- */

static s7_pointer g_port_line_number(s7_scheme *sc, s7_pointer args)
{
  #define H_port_line_number "(port-line-number input-file-port) returns the current read line number of port"
  s7_pointer x = car(args);
  if ((is_input_port(x)) &&
      (is_file_port(x)))
    return(s7_make_integer(sc, port_line_number(x)));
  return(sc->F); /* not an error! */
}


/* -------- port-filename -------- */

static s7_pointer g_port_filename(s7_scheme *sc, s7_pointer args)
{
  #define H_port_filename "(port-filename file-port) returns the filename associated with port"
  s7_pointer x = car(args);
  if (is_input_port(x))
    return(s7_make_string(sc, port_filename(x)));
  return(sc->F); /* not an error! */
}


/* -------- input-port? -------- */

bool s7_is_input_port(s7_scheme *sc, s7_pointer p)   
{ 
  return((p == sc->NIL) || /* stdin? */
	 (is_input_port(p)));
}


static s7_pointer g_is_input_port(s7_scheme *sc, s7_pointer args)
{
  #define H_is_input_port "(input-port? p) returns #t is p is an input port"
  return(make_boolean(sc, s7_is_input_port(sc, car(args))));
}


/* -------- output-port? -------- */

bool s7_is_output_port(s7_scheme *sc, s7_pointer p)     
{ 
  return((p == sc->NIL) ||  /* stderr? */
	 (is_output_port(p)));
}


static s7_pointer g_is_output_port(s7_scheme *sc, s7_pointer args)
{
  #define H_is_output_port "(output-port? p) returns #t is p is an output port"
  return(make_boolean(sc, s7_is_output_port(sc, car(args))));
}


/* -------- current-input-port -------- */

s7_pointer s7_current_input_port(s7_scheme *sc)
{
  return(sc->input_port);
}


static s7_pointer g_current_input_port(s7_scheme *sc, s7_pointer args)
{
  #define H_current_input_port "(current-input-port) returns the current input port"
  return(sc->input_port);
}


static s7_pointer g_set_current_input_port(s7_scheme *sc, s7_pointer args)
{
  #define H_set_current_input_port "(set-current-input-port port) sets the current-input port to port and returns the previous value of the input port"
  s7_pointer old_port, port;
  old_port = sc->input_port;
  port = car(args);
  if (s7_is_input_port(sc, port))
    sc->input_port = port;
  else return(s7_wrong_type_arg_error(sc, "set-current-input-port", 1, car(args), "an input port or nil"));
  return(old_port);
}


/* -------- current-output-port -------- */

s7_pointer s7_current_output_port(s7_scheme *sc)
{
  return(sc->output_port);
}


static s7_pointer g_current_output_port(s7_scheme *sc, s7_pointer args)
{
  #define H_current_output_port "(current-output-port) returns the current output port"
  return(sc->output_port);
}


static s7_pointer g_set_current_output_port(s7_scheme *sc, s7_pointer args)
{
  #define H_set_current_output_port "(set-current-output-port port) sets the current-output port to port and returns the previous value of the output port"
  s7_pointer old_port, port;
  old_port = sc->output_port;
  port = car(args);
  if (s7_is_output_port(sc, port))
    sc->output_port = port;
  else return(s7_wrong_type_arg_error(sc, "set-current-output-port", 1, car(args), "an output port or nil"));
  return(old_port);
}


/* -------- current-error-port -------- */

s7_pointer s7_current_error_port(s7_scheme *sc)
{
  return(sc->error_port);
}


s7_pointer s7_set_current_error_port(s7_scheme *sc, s7_pointer port)
{
  s7_pointer old_port;
  old_port = sc->error_port;
  sc->error_port = port;
  return(old_port);
}


static s7_pointer g_current_error_port(s7_scheme *sc, s7_pointer args)
{
  #define H_current_error_port "(current-error-port) returns the current error port"
  return(sc->error_port);
}


static s7_pointer g_set_current_error_port(s7_scheme *sc, s7_pointer args)
{
  #define H_set_current_error_port "(set-current-error-port port) sets the current-error port to port and returns the previous value of the error port"
  s7_pointer old_port, port;
  old_port = sc->error_port;
  port = car(args);
  if (s7_is_output_port(sc, port))
    sc->error_port = port;
  else return(s7_wrong_type_arg_error(sc, "set-current-error-port", 1, car(args), "an output port or nil"));
  return(old_port);
}


/* -------- char-ready? -------- */

static s7_pointer g_is_char_ready(s7_scheme *sc, s7_pointer args)
{
  #define H_is_char_ready "(char-ready? :optional port) returns #t if a character is ready for input on the given port"
  if (is_pair(args))
    {
      s7_pointer pt = car(args);
      if (!s7_is_input_port(sc, pt))
	return(s7_wrong_type_arg_error(sc, "char-ready?", 1, car(args), "an input port"));
      
      return(make_boolean(sc, is_string_port(pt)));
    }
  return(make_boolean(sc, (is_input_port(sc->input_port)) && (is_string_port(sc->input_port))));
}      


/* -------- eof-object? -------- */

static s7_pointer g_is_eof_object(s7_scheme *sc, s7_pointer args)
{
  #define H_is_eof_object "(eof-object? val) returns #t is val is the end-of-file object"
  return(make_boolean(sc, car(args) == sc->EOF_OBJECT));
}


/* -------- close-input-port -------- */

void s7_close_input_port(s7_scheme *sc, s7_pointer p)
{
  if ((p == sc->NIL) ||
      ((is_input_port(p)) && (port_is_closed(p))))
    return;
  
  if ((is_file_port(p)) &&
      (port_file(p)))
    {
      fclose(port_file(p));
      port_file(p) = NULL;
    }
  if (port_needs_free(p))
    {
      if (port_string(p))
	{
	  free(port_string(p));
	  port_string(p) = NULL;
	}
      port_needs_free(p) = false;
    }
  /* if input string, someone else is dealing with GC */
  port_is_closed(p) = true;
}


static s7_pointer g_close_input_port(s7_scheme *sc, s7_pointer args)
{
  #define H_close_input_port "(close-input-port port) closes the port"
  s7_pointer pt = car(args);
  if (!s7_is_input_port(sc, pt))
    return(s7_wrong_type_arg_error(sc, "close-input-port", 1, pt, "an input port"));
  s7_close_input_port(sc, pt);
  return(sc->UNSPECIFIED);
}


/* -------- close-output-port -------- */

void s7_close_output_port(s7_scheme *sc, s7_pointer p)
{
  if ((p == sc->NIL) ||
      ((is_output_port(p)) && (port_is_closed(p))))
    return;
  
  if (is_file_port(p))
    {
      if (port_file(p))
	{
	  fclose(port_file(p));
	  port_file(p) = NULL;
	}
    }
  else
    {
      if (port_string(p))
	{
	  free(port_string(p));
	  port_string(p) = NULL;
	  port_needs_free(p) = false;
	}
    }
  port_is_closed(p) = true;
}


static s7_pointer g_close_output_port(s7_scheme *sc, s7_pointer args)
{
  #define H_close_output_port "(close-output-port port) closes the port"
  s7_pointer pt = car(args);
  if (!s7_is_output_port(sc, pt))
    return(s7_wrong_type_arg_error(sc, "close-output-port", 1, pt, "an output port"));
  s7_close_output_port(sc, pt);
  return(sc->UNSPECIFIED);
}


/* -------- open-input-file -------- */

static s7_pointer s7_make_input_file(s7_scheme *sc, const char *name, FILE *fp)
{
  s7_pointer x;
  x = new_cell(sc);
  set_type(x, T_INPUT_PORT | T_ATOM | T_FINALIZABLE | T_SIMPLE | T_DONT_COPY);
  
  /* set up the port struct */
  x->object.port = (rport *)calloc(1, sizeof(rport));
  port_file(x) = fp;
  is_file_port(x) = true;
  port_is_closed(x) = false;
  port_filename(x) = strdup(name);
  port_line_number(x) = 1;  /* 1st line is numbered 1 */
  port_needs_free(x) = false;
  
  return(x);
}


s7_pointer s7_open_input_file(s7_scheme *sc, const char *name, const char *mode)
{
  FILE *fp;
  /* see if we can open this file before allocating a port */
  
  fp = fopen(name, mode);
  if (!fp)
    return(s7_file_error(sc, "open-input-file", "can't open", name));
  
  return(s7_make_input_file(sc, name, fp));
}


static s7_pointer g_open_input_file(s7_scheme *sc, s7_pointer args)
{
  #define H_open_input_file "(open-input-file filename :optional mode) opens filename for reading"
  s7_pointer name = car(args);
  if (!s7_is_string(name))
    return(s7_wrong_type_arg_error(sc, "open-input-file", 1, car(args), "a string (a filename)"));
  
  if (is_pair(cdr(args)))
    {
      if (!s7_is_string(cadr(args)))
	return(s7_wrong_type_arg_error(sc, "open-input-file", 1, cadr(args), "a string (a mode)"));
      return(s7_open_input_file(sc, s7_string(name), s7_string(cadr(args))));
    }
  return(s7_open_input_file(sc, s7_string(name), "r"));
}


/* -------- open-output-file -------- */

s7_pointer s7_open_output_file(s7_scheme *sc, const char *name, const char *mode)
{
  FILE *fp;
  s7_pointer x;
  /* see if we can open this file before allocating a port */
  
  fp = fopen(name, mode);
  if (!fp)
    return(s7_file_error(sc, "open-output-file", "can't open", name));
  
  x = new_cell(sc);
  set_type(x, T_OUTPUT_PORT | T_ATOM | T_FINALIZABLE | T_SIMPLE | T_DONT_COPY);
  
  /* set up the port struct */
  x->object.port = (rport *)calloc(1, sizeof(rport));
  is_file_port(x) = true;
  port_is_closed(x) = false;
  port_filename(x) = strdup(name);
  port_line_number(x) = 1;
  port_file(x) = fp;
  port_needs_free(x) = false;
  
  return(x);
}


static s7_pointer g_open_output_file(s7_scheme *sc, s7_pointer args)
{
  #define H_open_output_file "(open-output-file filename :optional mode) opens filename for writing"
  s7_pointer name = car(args);
  if (!s7_is_string(name))
    return(s7_wrong_type_arg_error(sc, "open-output-file", 1, car(args), "a string (a filename)"));
  
  if (is_pair(cdr(args)))
    {
      if (!s7_is_string(cadr(args)))
	return(s7_wrong_type_arg_error(sc, "open-output-file", 1, cadr(args), "a string (a mode)"));
      return(s7_open_output_file(sc, s7_string(name), s7_string(cadr(args))));
    }
  
  return(s7_open_output_file(sc, s7_string(name), "w"));
}


/* -------- open-input-string -------- */

s7_pointer s7_open_input_string(s7_scheme *sc, const char *input_string)
{
  s7_pointer x;
  x = new_cell(sc);
  set_type(x, T_INPUT_PORT | T_ATOM | T_FINALIZABLE | T_SIMPLE | T_DONT_COPY);
  
  /* set up the port struct */
  x->object.port = (rport *)calloc(1, sizeof(rport));
  is_file_port(x) = false;
  port_is_closed(x) = false;
  port_string(x) = (char *)input_string;
  port_string_length(x) = safe_strlen(input_string);
  port_string_point(x) = 0;
  port_filename(x) = NULL;
  port_file_number(x) = -1;
  port_needs_free(x) = false;
  
  return(x);
}


static s7_pointer g_open_input_string(s7_scheme *sc, s7_pointer args)
{
  #define H_open_input_string "(open-input-string str) opens an input port reading str"
  s7_pointer input_string = car(args);
  if (!s7_is_string(input_string))
    return(s7_wrong_type_arg_error(sc, "open-input-string", 1, car(args), "a string"));
  
  return(s7_open_input_string(sc, s7_string(input_string))); /* presumably the caller is protecting the input string?? */
}


/* -------- open-output-string -------- */

#define STRING_PORT_INITIAL_LENGTH 128

s7_pointer s7_open_output_string(s7_scheme *sc)
{
  s7_pointer x;
  x = new_cell(sc);
  set_type(x, T_OUTPUT_PORT | T_ATOM | T_FINALIZABLE | T_SIMPLE | T_DONT_COPY);
  
  /* set up the port struct */
  x->object.port = (rport *)calloc(1, sizeof(rport));
  is_file_port(x) = false;
  port_is_closed(x) = false;
  port_string_length(x) = STRING_PORT_INITIAL_LENGTH;
  port_string(x) = (char *)calloc(STRING_PORT_INITIAL_LENGTH, sizeof(char));
  port_string_point(x) = 0;
  port_needs_free(x) = true;
  
  return(x);
}


static s7_pointer g_open_output_string(s7_scheme *sc, s7_pointer args)
{
  #define H_open_output_string "(open-output-string) opens an output string port"
  return(s7_open_output_string(sc));
}


/* -------- get-output-string -------- */

const char *s7_get_output_string(s7_scheme *sc, s7_pointer p)
{
  return(port_string(p));
}


static s7_pointer g_get_output_string(s7_scheme *sc, s7_pointer args)
{
  #define H_get_output_string "(get-output-string port) returns the output accumulated in port"
  s7_pointer p = car(args);
  if ((!is_output_port(p)) ||
      (!is_string_port(p)))
    return(s7_wrong_type_arg_error(sc, "get-output-string", 1, car(args), "an output string port"));
  return(s7_make_string(sc, s7_get_output_string(sc, p)));
}


static bool push_input_port(s7_scheme *sc, s7_pointer new_port)
{
  sc->input_port_stack = s7_cons(sc, sc->input_port, sc->input_port_stack);
  sc->input_port = new_port;
  return(sc->input_port != sc->NIL);
}


static s7_pointer pop_input_port(s7_scheme *sc)
{
  if (is_pair(sc->input_port_stack))
    {
      sc->input_port = car(sc->input_port_stack);
      sc->input_port_stack = cdr(sc->input_port_stack);
    }
  else sc->input_port = sc->NIL;
  return(sc->input_port);
}


/* -------- get new character from input file -------- */

static int inchar(s7_scheme *sc, s7_pointer pt)
{
  int c;
  if (pt == sc->NIL) return(EOF);
  
  if (is_file_port(pt))
    c = fgetc(port_file(pt));
  else 
    {
      if ((!(port_string(pt))) ||
	  (port_string_length(pt) <= port_string_point(pt)))
	return(EOF);
      c = port_string(pt)[port_string_point(pt)++];
    }

  if (c == '\n')
    port_line_number(pt)++;

  return(c);
}


/* -------- put character back into input buffer -------- */

static void backchar(s7_scheme *sc, char c, s7_pointer pt) 
{
  if (pt == sc->NIL) return;
  
  if (c == '\n')
    port_line_number(pt)--;

  if (is_file_port(pt))
    ungetc(c, port_file(pt));
  else 
    {
      if (port_string_point(pt) > 0)
	port_string_point(pt)--;
    }
}


/* -------- read token or expression -------- */

static void resize_strbuf(s7_scheme *sc)
{
  int i, old_size;
  old_size = sc->strbuf_size;
  sc->strbuf_size *= 2;
  sc->strbuf = (char *)realloc(sc->strbuf, sc->strbuf_size * sizeof(char));
  for (i = old_size; i < sc->strbuf_size; i++) sc->strbuf[i] = '\0';
}


static char *read_string_upto(s7_scheme *sc) 
{
  int i = 0, c;
  s7_pointer pt;
  pt = sc->input_port;

  if (is_file_port(pt))
    {
      do
	{
	  c = fgetc(port_file(pt)); /* might return EOF */
	  if (c == '\n')
	    port_line_number(pt)++;

	  sc->strbuf[i++] = c;
	  if (i >= sc->strbuf_size)
	    resize_strbuf(sc);
	}
      while (!((c == EOF) || (atom_delimiter_table[c])));

      if ((i == 2) && 
	  (sc->strbuf[0] == '\\'))
	sc->strbuf[i] = 0;
      else 
	{
	  if (c != EOF)
	    {
	      if (c == '\n')
		port_line_number(pt)--;
	      ungetc(c, port_file(pt));
	    }
	  sc->strbuf[i - 1] = '\0';
	}
    }
  else
    {
      if (port_string(pt))
	{
	  do
	    {
	      c = port_string(pt)[port_string_point(pt)++];

	      sc->strbuf[i++] = c;
	      if (i >= sc->strbuf_size)
		resize_strbuf(sc);
	    }
	  while (string_delimiter_table[c]);

	  if ((i == 2) && 
	      (sc->strbuf[0] == '\\'))
	    sc->strbuf[i] = 0;
	  else 
	    {
	      if (c != 0)
		{
		  port_string_point(pt)--;
		  sc->strbuf[i - 1] = '\0';
		}
	    }
	}
      else
	{
	  sc->strbuf[0] = '\0';
	}
    }

  return(sc->strbuf);
}


static s7_pointer read_string_expression(s7_scheme *sc, s7_pointer pt) 
{
  int c, c1 = 0, i = 0;
  enum {ST_OK, ST_BSL, ST_X1, ST_X2, ST_OCT1, ST_OCT2, ST_OCT3} state = ST_OK; /* BSL=backslash? X1 or X2 = hex (ctrs) as in oct case */
  
  for (;;) 
    {
      c = inchar(sc, pt);
      if (c == EOF)
	return(sc->F);
      
      switch(state) 
	{
	case ST_OK:
	  switch(c) 
	    {
	    case '\\':
	      state = ST_BSL;
	      break;
	      
	    case '"':
	      sc->strbuf[i] = '\0';
	      return(s7_make_string_with_length(sc, sc->strbuf, i));
	      
	    default:
	      sc->strbuf[i++] = c;
	      break;
	    }
	  break;
	  
	case ST_BSL:
	  switch(c) 
	    {
	    case '0':
	    case '1':
	    case '2':
	    case '3':
	    case '4':
	    case '5':
	    case '6':
	    case '7':
	      state = ST_OCT1;
	      c1 = c - '0';
	      break;
	      
	    case 'x':
	    case 'X':
	      state = ST_X1;
	      c1 = 0;
	      break;
	      
	    case 'n':
	      sc->strbuf[i++] = '\n';
	      state = ST_OK;
	      break;
	      
	    case 't':
	      sc->strbuf[i++] = '\t';
	      state = ST_OK;
	      break;
	      
	    case 'r':
	      sc->strbuf[i++] = '\r';
	      state = ST_OK;
	      break;
	      
	    case '"':
	      sc->strbuf[i++] = '"';
	      state = ST_OK;
	      break;
	      
	    default:
	      sc->strbuf[i++] = c;
	      state = ST_OK;
	      break;
	    }
	  break;
	  
	case ST_X1:
	case ST_X2:
	  c = toupper(c);
	  if ((c >= '0') && (c <= 'F'))
	    {
	      if (c <= '9') 
		c1 = (c1 << 4 ) + c - '0';
	      else c1 = (c1 << 4) + c - 'A' + 10;
	      
	      if (state == ST_X1) 
		state = ST_X2;
	      else 
		{
		  sc->strbuf[i++] = c1;
		  state = ST_OK;
		}
	    } 
	  else return(sc->F);
	  break;
	  
	case ST_OCT1:
	case ST_OCT2:
	case ST_OCT3:
	  if (c < '0' || c > '7')
	    {
	      if (state == ST_OCT1)
		return(sc->F);
	      
	      sc->strbuf[i++] = c1;
	      backchar(sc, c, pt);
	      state = ST_OK;
	    }
	  else
	    {
	      c1 = (c1 << 3) + (c - '0');
	      switch (state)
		{
		case ST_OCT1:
		  state = ST_OCT2;
		  break;
		  
		case ST_OCT2:
		  state = ST_OCT3;
		  break;
		  
		default:
		  sc->strbuf[i++] = c1;
		  state = ST_OK;
		  break;
		}
	    }
	  break;
	}
      
      if (i >= sc->strbuf_size)
	resize_strbuf(sc);
    }
}


static char skip_space(s7_scheme *sc, s7_pointer pt) 
{
  int c;
  if (is_file_port(pt))
    {
      while (isspace(c = fgetc(port_file(pt))))
	if (c == '\n')
	  port_line_number(pt)++;
    }
  else 
    {
      while (isspace(c = port_string(pt)[port_string_point(pt)++]))
	if (c == '\n')
	  port_line_number(pt)++;
    }
  return(c);
}


static int token(s7_scheme *sc, s7_pointer pt)
{
  int c;
  c = skip_space(sc, pt);
  switch (c) 
    {
    case EOF:
    case 0:
      return(TOK_EOF);
      
    case '(':
      return(TOK_LPAREN);
      
    case ')':
      return(TOK_RPAREN);
      
    case '.':
      c = inchar(sc, pt);
      if (is_one_of(whitespace_table, c)) 
	return(TOK_DOT);
      backchar(sc, c, pt);
      backchar(sc, '.', pt);
      return(TOK_ATOM);
      
    case '\'':
      return(TOK_QUOTE);
      
    case ';':
      while ((c = inchar(sc, pt)) != '\n' && (c != EOF))
	;
      return(token(sc, pt));
      
    case '"':
      return(TOK_DQUOTE);
      
    case BACKQUOTE:
      return(TOK_BQUOTE);
      
    case ',':
      if ((c = inchar(sc, pt)) == '@') 
	return(TOK_ATMARK);
      
      backchar(sc, c, pt);
      return(TOK_COMMA);
      
    case '#':
      c = inchar(sc, pt);
      if (c == '(') 
	return(TOK_VEC);
      
      /* block comments in either #! ... !# */
      if (c == '!') 
	{
	  char last_char;
	  last_char = ' ';
	  while ((c = inchar(sc, pt)) != EOF)
	    {
	      if ((c == '#') &&
		  (last_char == '!'))
		break;
	      last_char = c;
	    }
	  return(token(sc, pt));
	}
      
      /*   or #| ... |# */
      if (c == '|') 
	{
	  char last_char;
	  last_char = ' ';
	  while ((c = inchar(sc, pt)) != EOF)
	    {
	      if ((c == '#') &&
		  (last_char == '|'))
		break;
	      last_char = c;
	    }
	  return(token(sc, pt));
	}
      
      backchar(sc, c, pt);
      if (is_one_of(sharp_const_table, c)) 
	return(TOK_SHARP_CONST);
      return(TOK_ATOM);
      
    default:
      backchar(sc, c, pt);
      return(TOK_ATOM);
    }
}



/* -------- read|peek-char -------- */

static char s7_read_char_1(s7_scheme *sc, s7_pointer port, bool peek)
{
  /* port nil -> as if read-char with no arg -> use current input port */
  int c;
  c = inchar(sc, port);
  if ((peek) && (c != EOF))
    backchar(sc, c, port);
  return(c);
}


char s7_read_char(s7_scheme *sc, s7_pointer port)
{
  return(s7_read_char_1(sc, port, false));
}


char s7_peek_char(s7_scheme *sc, s7_pointer port)
{
  return(s7_read_char_1(sc, port, true));
}


static s7_pointer g_read_char_1(s7_scheme *sc, s7_pointer args, bool peek)
{
  char c;
  if (is_pair(args))
    c = s7_read_char_1(sc, car(args), peek);
  else c = s7_read_char_1(sc, sc->input_port, peek);
  if (c == EOF)
    return(sc->EOF_OBJECT); 
  return(s7_make_character(sc, c));
}


static s7_pointer g_read_char(s7_scheme *sc, s7_pointer args)
{
  #define H_read_char "(read-char :optional port) returns the next character in the input port"
  return(g_read_char_1(sc, args, false));
}


static s7_pointer g_peek_char(s7_scheme *sc, s7_pointer args)
{
  #define H_peek_char "(peek-char :optional port) returns the next character in the input port, but does not remove it from the input stream"
  return(g_read_char_1(sc, args, true));
}


#if WITH_READ_LINE

static s7_pointer g_read_line(s7_scheme *sc, s7_pointer args)
{
  #define H_read_line "(read-line port) returns the next line from port, or EOF"
  s7_pointer port;
  int i;

  if (args != sc->NIL)
    {
      port = car(args);
      if (!s7_is_input_port(sc, port))
	return(s7_wrong_type_arg_error(sc, "read-line", 1, port, "an input port"));
    }
  else port = sc->input_port;

  if (sc->read_line_buf == NULL)
    {
      sc->read_line_buf_size = 256;
      sc->read_line_buf = (char *)malloc(sc->read_line_buf_size * sizeof(char));
    }

  for (i = 0; ; i++)
    {
      int c;
      if (i + 1 >= sc->read_line_buf_size)
	{
	  sc->read_line_buf_size *= 2;
	  sc->read_line_buf = (char *)realloc(sc->read_line_buf, sc->read_line_buf_size * sizeof(char));
	}

      c = inchar(sc, port);
      if (c == EOF)
	{
	  if (i == 0)
	    return(sc->EOF_OBJECT);
	  sc->read_line_buf[i + 1] = 0;
	  return(s7_make_string_with_length(sc, sc->read_line_buf, i + 1));
	}

      sc->read_line_buf[i] = (char)c;
      if (c == '\n')
	{
	  sc->read_line_buf[i + 1] = 0;
	  return(s7_make_string_with_length(sc, sc->read_line_buf, i + 1));
	}
    }
  return(sc->EOF_OBJECT);
}
#endif



/* -------- read -------- */

s7_pointer s7_read(s7_scheme *sc, s7_pointer port)
{
  if (s7_is_input_port(sc, port))
    {
      bool old_longjmp;
      old_longjmp = sc->longjmp_ok;
      if (!sc->longjmp_ok)
	{
	  sc->longjmp_ok = true;
	  if (setjmp(sc->goto_start) != 0)
	    return(sc->value);
	}
      push_input_port(sc, port);
      push_stack(sc, OP_READ_RETURN_EXPRESSION, port, sc->NIL);
      eval(sc, OP_READ_INTERNAL);
      sc->longjmp_ok = old_longjmp;
      pop_input_port(sc);
      return(sc->value);
    }
  return(s7_wrong_type_arg_error(sc, "read", 1, port, "an input port"));
}


static s7_pointer g_read(s7_scheme *sc, s7_pointer args)
{
  #define H_read "(read :optional port) returns the next object in the input port"
  s7_pointer port;
  
  if (is_pair(args))
    port = car(args);
  else port = sc->input_port;
  
  if (!s7_is_input_port(sc, port))
    return(s7_wrong_type_arg_error(sc, "read", 1, car(args), "an input port"));
  
  push_input_port(sc, port);
  push_stack(sc, OP_READ_POP_AND_RETURN_EXPRESSION, sc->NIL, sc->NIL); /* this stops the internal read process so we only get one form */
  push_stack(sc, OP_READ_INTERNAL, sc->NIL, sc->NIL);
  return(port);
}


/* -------- load -------- */

static FILE *search_load_path(s7_scheme *sc, const char *name)
{
  int i, len, name_len;
  s7_pointer lst;
  lst = s7_load_path(sc);
  len = s7_list_length(sc, lst);
  name_len = strlen(name);
  for (i = 0; i < len; i++)
    {
      const char *new_dir;
      int size;
      new_dir = s7_string(s7_list_ref(sc, lst, i));
      if (new_dir)
	{
	  char *new_name;
	  FILE *fp;
	  size = name_len + strlen(new_dir) + 2;
	  new_name = (char *)malloc(size * sizeof(char));
	  snprintf(new_name, size, "%s/%s", new_dir, name);
	  fp = fopen(new_name, "r");
	  free(new_name);
	  if (fp) return(fp);
	}
    }
  return(NULL);
}


static s7_pointer load_file(s7_scheme *sc, FILE *fp)
{
  s7_pointer port;
  long size;
  char *content = NULL;

  port = new_cell(sc);
  set_type(port, T_INPUT_PORT | T_ATOM | T_FINALIZABLE | T_SIMPLE | T_DONT_COPY);
  port->object.port = (rport *)calloc(1, sizeof(rport));
  is_file_port(port) = false;
  port_is_closed(port) = false;

  fseek(fp, 0, SEEK_END);
  size = ftell(fp);
  if (size > 0)
    {
      size_t bytes;
      rewind(fp);
      content = (char *)malloc(size + 1);
      bytes = fread(content, sizeof(char), size, fp);
      content[size] = '\0';
    }
  fclose(fp);

  port_string(port) = content;
  port_string_length(port) = size;
  port_string_point(port) = 0;
  port_line_number(port) = 1;
  port_filename(port) = NULL;
  port_needs_free(port) = true;

  return(port);
}


s7_pointer s7_load(s7_scheme *sc, const char *filename)
{
  bool old_longjmp;
  s7_pointer port;
  FILE *fp;
  
  fp = fopen(filename, "r");
  if (!fp)
    fp = search_load_path(sc, filename);
  if (!fp)
    return(s7_file_error(sc, "open-input-file", "can't open", filename));
  
  if (*(sc->load_verbose))
    fprintf(stderr, "s7_load(\"%s\")\n", filename);

  port = load_file(sc, fp);
  port_file_number(port) = remember_file_name(filename);
  push_input_port(sc, port);
  
  /* it's possible to call this recursively (s7_load is XEN_LOAD_FILE which can be invoked via s7_call)
   *   but in that case, we actually want it to behave like g_load and continue the evaluation upon completion
   */
  
  if (!sc->longjmp_ok)
    {
      push_stack(sc, OP_LOAD_RETURN_IF_EOF, port, sc->NIL);
      
      old_longjmp = sc->longjmp_ok;
      if (!sc->longjmp_ok)
	{
	  sc->longjmp_ok = true;
	  if (setjmp(sc->goto_start) != 0)
	    eval(sc, sc->op);
	  else eval(sc, OP_READ_INTERNAL);
	}
      sc->longjmp_ok = old_longjmp;  
      pop_input_port(sc);
      s7_close_input_port(sc, port);
    }
  else
    {
      /* caller here is assuming the load will be complete before this function returns */
      push_stack(sc, OP_LOAD_RETURN_IF_EOF, sc->args, sc->code);
      eval(sc, OP_READ_INTERNAL);
      pop_input_port(sc);
      s7_close_input_port(sc, port);
    }
  return(sc->UNSPECIFIED);
}


static s7_pointer g_load(s7_scheme *sc, s7_pointer args)
{
  FILE *fp = NULL;
  s7_pointer name, port;
  const char *fname;
  
  name = car(args);
  if (!s7_is_string(name))
    return(s7_wrong_type_arg_error(sc, "load", 1, car(args), "a string (a filename)"));
  
  fname = s7_string(name);
  
  fp = fopen(fname, "r");
  if (!fp)
    fp = search_load_path(sc, fname);
  if (!fp)
    return(s7_file_error(sc, "open-input-file", "can't open", fname));
  
  if (*(sc->load_verbose))
    fprintf(stderr, "(load \"%s\")\n", fname);
  /* TODO: shouldn't the load-verbose and gc-verbose stuff go to output or error port? */
  
  port = load_file(sc, fp);
  port_file_number(port) = remember_file_name(fname);
  push_input_port(sc, port);

  push_stack(sc, OP_LOAD_CLOSE_AND_POP_IF_EOF, sc->args, sc->code);
  push_stack(sc, OP_READ_INTERNAL, sc->NIL, sc->NIL);
  
  return(sc->UNSPECIFIED);
}


s7_pointer s7_load_path(s7_scheme *sc)
{
  return(s7_symbol_value(sc, s7_make_symbol(sc, "*load-path*")));
}


s7_pointer s7_add_to_load_path(s7_scheme *sc, const char *dir)
{
  s7_symbol_set_value(sc, 
		      s7_make_symbol(sc, "*load-path*"),
		      s7_cons(sc, 
			      s7_make_string(sc, dir), 
			      s7_load_path(sc)));
  return(s7_load_path(sc));
}


static s7_pointer g_load_verbose(s7_scheme *sc, s7_pointer a)
{
  #define H_load_verbose "(load-verbose bool) if #t prints out file names as they are loaded"
  s7_pointer old_val;
  old_val = (*(sc->load_verbose)) ? sc->T : sc->F;
  (*(sc->load_verbose)) = (car(a) != sc->F);
  return(old_val);
}




/* -------- eval string -------- */

static s7_pointer g_eval_string(s7_scheme *sc, s7_pointer args)
{
  #define H_eval_string "(eval-string str) returns the result of evaluating the string str as Scheme code"
  s7_pointer port;
  if (!s7_is_string(car(args)))
    return(s7_wrong_type_arg_error(sc, "eval-string", 1, car(args), "a string"));
  
  port = s7_open_input_string(sc, s7_string(car(args)));
  push_input_port(sc, port);
  push_stack(sc, OP_EVAL_STRING, sc->args, sc->code);
  eval(sc, OP_READ_INTERNAL);
  pop_input_port(sc);
  s7_close_input_port(sc, port);
  return(sc->value);
}


s7_pointer s7_eval_c_string(s7_scheme *sc, const char *str)
{
  bool old_longjmp;
  s7_pointer port;
  /* this can be called recursively via s7_call */
  
  if (sc->longjmp_ok)
    return(g_eval_string(sc, make_list_1(sc, s7_make_string(sc, str))));
  
  stack_reset(sc); 
  sc->envir = sc->global_env;
  port = s7_open_input_string(sc, str);
  push_input_port(sc, port);
  push_stack(sc, OP_EVAL_STRING, sc->NIL, sc->NIL);
  
  old_longjmp = sc->longjmp_ok;
  if (!sc->longjmp_ok)
    {
      sc->longjmp_ok = true;
      if (setjmp(sc->goto_start) != 0)
	eval(sc, sc->op);
      else eval(sc, OP_READ_INTERNAL);
    }
  
  sc->longjmp_ok = old_longjmp;
  pop_input_port(sc);
  s7_close_input_port(sc, port);
  
  return(sc->value);
}


static s7_pointer call_with_input(s7_scheme *sc, s7_pointer port, s7_pointer args)
{
  push_stack(sc, OP_EVAL_STRING_DONE, sc->args, sc->code);
  sc->code = cadr(args);
  sc->args = make_list_1(sc, port);
  eval(sc, OP_APPLY);
  s7_close_input_port(sc, port);
  return(sc->value);
}


static s7_pointer g_call_with_input_string(s7_scheme *sc, s7_pointer args)
{
  #define H_call_with_input_string "(call-with-input-string str proc) opens a string port for str and applies proc to it"
  
  /* (call-with-input-string "44" (lambda (p) (+ 1 (read p)))) -> 45
   */
  
  if (!s7_is_string(car(args)))
    return(s7_wrong_type_arg_error(sc, "call-with-input-string", 1, car(args), "a string"));
  
  return(call_with_input(sc, s7_open_input_string(sc, s7_string(car(args))), args));
}


static s7_pointer g_call_with_input_file(s7_scheme *sc, s7_pointer args)
{
  #define H_call_with_input_file "(call-with-input-file filename proc) opens filename and calls proc with the input port as its argument"
  
  if (!s7_is_string(car(args)))
    return(s7_wrong_type_arg_error(sc, "call-with-input-file", 1, car(args), "a string (a filename)"));
  
  return(call_with_input(sc, s7_open_input_file(sc, s7_string(car(args)), "r"), args));
}


static s7_pointer with_input(s7_scheme *sc, s7_pointer port, s7_pointer args)
{
  s7_pointer old_input_port;
  old_input_port = sc->input_port;
  sc->input_port = port;
  
  push_stack(sc, OP_EVAL_STRING_DONE, sc->args, sc->code);
  sc->code = cadr(args);
  sc->args = sc->NIL;
  eval(sc, OP_APPLY);
  
  s7_close_input_port(sc, sc->input_port);
  sc->input_port = old_input_port;
  return(sc->value);
}


static s7_pointer g_with_input_from_string(s7_scheme *sc, s7_pointer args)
{
  #define H_with_input_from_string "(with-input-from-string str thunk) opens str as the temporary current-input-port and calls thunk"
  
  if (!s7_is_string(car(args)))
    return(s7_wrong_type_arg_error(sc, "with-input-from-string", 1, car(args), "a string"));
  
  return(with_input(sc, s7_open_input_string(sc, s7_string(car(args))), args));
}


static s7_pointer g_with_input_from_file(s7_scheme *sc, s7_pointer args)
{
  #define H_with_input_from_file "(with-input-from-file filename thunk) opens filename as the temporary current-input-port and calls thunk"
  
  if (!s7_is_string(car(args)))
    return(s7_wrong_type_arg_error(sc, "with-input-from-file", 1, car(args), "a string (a filename)"));
  
  return(with_input(sc, s7_open_input_file(sc, s7_string(car(args)), "r"), args));
}





/* -------- output -------- */

static void char_to_string_port(char c, s7_pointer pt)
{
  if (port_string_point(pt) >= port_string_length(pt))
    {
      int loc;
      loc = port_string_length(pt);
      port_string_length(pt) *= 2;
      port_string(pt) = (char *)realloc(port_string(pt), port_string_length(pt) * sizeof(char));
      memset((void *)(port_string(pt) + loc), 0, loc);
    }
  port_string(pt)[port_string_point(pt)++] = c;
}


static void write_char(s7_scheme *sc, char c, s7_pointer pt) 
{
  if (pt == sc->NIL)
    fputc(c, stderr);
  else
    {
      if (port_is_closed(pt))
	return;
      if (is_file_port(pt))
	fputc(c, port_file(pt));
      else char_to_string_port(c, pt);
    }
}


static void write_string(s7_scheme *sc, const char *s, s7_pointer pt) 
{
  if (!s) return;
  
  if (pt == sc->NIL)
    fputs(s, stderr);
  else
    {
      if (port_is_closed(pt))
	return;
      
      if (is_file_port(pt))
	fputs(s, port_file(pt));
      else 
	{
	  for(; *s; s++)
	    char_to_string_port(*s, pt);
	}
    }
}


static char *slashify_string(const char *p)
{
  int i, j = 0, len;
  char *s;
  len = safe_strlen(p);
  s = (char *)calloc(len + 256, sizeof(char));
  
  s[j++] = '"';
  for (i = 0; i < len; i++) 
    {
      if (slashify_table[(int)p[i]])
	{
	  s[j++] = '\\';
	  switch(p[i]) 
	    {
	    case '"':
	      s[j++] = '"';
	      break;
	      
#if 0
	    case '\n':
	      s[j++] = 'n';
	      break;
#endif
	      
	    case '\t':
	      s[j++] = 't';
	      break;
	      
	    case '\r':
	      s[j++] = 'r';
	      break;
	      
	    case '\\':
	      s[j++] = '\\';
	      break;
	      
	    default: 
	      { 
		int d = p[i] / 16;
		s[j++] = 'x';
		if (d < 10) 
		  s[j++] = d + '0';
		else s[j++] = d - 10 + 'A';
		
		d = p[i] % 16;
		if (d < 10) 
		  s[j++] = d + '0';
		else s[j++] = d - 10 + 'A';
	      }
	    }
	}
      else s[j++] = p[i];
    }
  s[j++] = '"';
  return(s);
}


static char *s7_atom_to_c_string(s7_scheme *sc, s7_pointer obj, bool use_write)
{
  switch(type(obj))
    {
    case T_NIL_TYPE:
      if (obj == sc->NIL) 
	return(strdup("()"));
  
      if (obj == sc->T)
	return(strdup("#t"));
  
      if (obj == sc->F) 
	return(strdup("#f"));
  
      if (obj == sc->EOF_OBJECT)
	return(strdup("#<eof>"));
  
      if (obj == sc->UNDEFINED) 
	return(strdup("#<undefined>"));
  
      if (obj == sc->UNSPECIFIED) 
	return(strdup("#<unspecified>"));

      break;

    case T_INPUT_PORT:
    case T_OUTPUT_PORT:
      return(describe_port(sc, obj));

    case T_NUMBER:
      return(s7_number_to_string_1(sc, obj, 10, 0, 14, 'g')); /* 20 digits is excessive in this context */
  
    case T_STRING:
      if (string_length(obj) > 0)
	{
	  if (!use_write) 
	    return(strdup(string_value(obj)));
	  return(slashify_string(string_value(obj)));
	}
      if (!use_write)
	return(NULL);
      else return(strdup("\"\""));

    case T_CHARACTER:
      {
	#define P_SIZE 16
	char *p;
	p = (char *)malloc(P_SIZE * sizeof(char));
	char c = s7_character(obj);
	if (!use_write) 
	  {
	    p[0]= c;
	    p[1]= 0;
	  } 
	else 
	  {
	    switch(c) 
	      {
	      case ' ':
		snprintf(p, P_SIZE, "#\\space"); 
		break;

	      case '\n':
		snprintf(p, P_SIZE, "#\\newline"); 
		break;

	      case '\r':
		snprintf(p, P_SIZE, "#\\return"); 
		break;

	      case '\t':
		snprintf(p, P_SIZE, "#\\tab"); 
		break;

	      case '\0':
		snprintf(p, P_SIZE, "#\\null");
		break;

	      default:
		if (c < 32) 
		  snprintf(p, P_SIZE, "#\\x%x", c);
		else snprintf(p, P_SIZE, "#\\%c", c); 
		break;
	      }
	  }
	return(p);
      }
  
    case T_SYMBOL:
      return(strdup(s7_symbol_name(obj)));
  
    case T_MACRO:
      return(strdup("#<macro>"));
  
    case T_CLOSURE:
    case T_CLOSURE_STAR:
      {
	/* try to find obj in the current environment and return its name */
	s7_pointer binding;
	binding = s7_find_value_in_environment(sc, obj);
	if (is_pair(binding))
	  return(strdup(s7_symbol_name(car(binding))));
	return(strdup("#<closure>"));
      }
  
    case T_PROMISE:
      return(strdup("#<promise>"));
  
    case T_S7_FUNCTION:
      return(strdup(function_name(obj)));
  
    case T_CONTINUATION:
      return(strdup("#<continuation>"));
  
    case T_GOTO:
      return(strdup("#<goto>"));
  
    case T_CATCH:
      return(strdup("#<catch>"));
  
    case T_DYNAMIC_WIND:
      return(strdup("#<dynamic-wind>"));
  
    case T_S7_OBJECT:
      return(s7_describe_object(sc, obj)); /* this allocates already */
    }
  return(strdup("#<unknown object!>"));
}


static char *s7_vector_to_c_string(s7_scheme *sc, s7_pointer vect)
{
  int i, len, plen, bufsize = 0;
  bool too_long = false;
  char **elements = NULL;
  char *buf;
  
  len = vector_length(vect);
  if (len == 0)
    return(strdup("#()"));
  
  plen = s7_integer(s7_symbol_value(sc, s7_make_symbol(sc, "*vector-print-length*")));
  if (len > plen)
    {
      too_long = true;
      len = plen;
    }
  elements = (char **)malloc(len * sizeof(char *));
  for (i = 0; i < len; i++)
    {
      
      elements[i] = s7_object_to_c_string(sc, vector_element(vect, i));
      bufsize += safe_strlen(elements[i]);
    }
  bufsize += (len * 2 + 256);
  buf = (char *)malloc(bufsize * sizeof(char));
  sprintf(buf, "#(");
  for (i = 0; i < len - 1; i++)
    {
      if (elements[i])
	{
	  strcat(buf, elements[i]);
	  free(elements[i]);
	  strcat(buf, " ");
	}
    }
  if (elements[len - 1])
    {
      strcat(buf, elements[len - 1]);
      free(elements[len - 1]);
    }
  free(elements);
  if (too_long)
    strcat(buf, " ...");
  strcat(buf, ")");
  return(buf);
}


static s7_pointer s7_vector_to_string(s7_scheme *sc, s7_pointer vect)
{
  char *buf;
  s7_pointer result;
  buf = s7_vector_to_c_string(sc, vect);
  result = s7_make_string(sc, buf);
  free(buf);
  return(result);
}


static char *s7_list_to_c_string(s7_scheme *sc, s7_pointer lst)
{
  bool dotted = false;
  s7_pointer x;
  int i, len, bufsize = 0;
  char **elements = NULL;
  char *buf;
  len = s7_list_length(sc, lst);
  if (len < 0)                    /* a dotted list -- handle cars, then final cdr */
    {
      len = (-len + 1);
      dotted = true;
    }
  
  if (len == 0)                   /* either '() or a circular list */
    {
      if (lst != sc->NIL)
	return(strdup("[circular list!]"));
      return(strdup("()"));
    }
  
  elements = (char **)malloc(len * sizeof(char *));
  for (x = lst, i = 0; is_pair(x) && (i < len); i++, x = cdr(x))
    {
      elements[i] = s7_object_to_c_string(sc, car(x));
      bufsize += safe_strlen(elements[i]);
    }
  if (dotted)
    {
      elements[i] = s7_object_to_c_string(sc, x);
      bufsize += safe_strlen(elements[i]);
    }
  
  bufsize += (256 + len * 2); /* len spaces */
  buf = (char *)malloc(bufsize * sizeof(char));
  
  sprintf(buf, "(");
  for (i = 0; i < len - 1; i++)
    {
      if (elements[i])
	{
	  strcat(buf, elements[i]);
	  strcat(buf, " ");
	}
    }
  if (dotted) strcat(buf, ". ");
  if (elements[len - 1])
    {
      strcat(buf, elements[len - 1]);
      strcat(buf, ")");
    }
  for (i = 0; i < len; i++)
    if (elements[i])
      free(elements[i]);
  free(elements);
  return(buf);
}


static s7_pointer s7_list_to_string(s7_scheme *sc, s7_pointer lst)
{
  s7_pointer result;
  char *buf;
  buf = s7_list_to_c_string(sc, lst);
  result = s7_make_string(sc, buf);
  free(buf);
  return(result);
}


static char *s7_object_to_c_string_1(s7_scheme *sc, s7_pointer obj, bool use_write)
{
  if ((s7_is_vector(obj)) ||
      (s7_is_hash_table(obj)))
    return(s7_vector_to_c_string(sc, obj));
  if (is_pair(obj))
    return(s7_list_to_c_string(sc, obj));
  return(s7_atom_to_c_string(sc, obj, use_write));
}


char *s7_object_to_c_string(s7_scheme *sc, s7_pointer obj)
{
  return(s7_object_to_c_string_1(sc, obj, true));
}


s7_pointer s7_object_to_string(s7_scheme *sc, s7_pointer obj)
{
  char *str = NULL;
  s7_pointer x;
  if ((s7_is_vector(obj)) ||
      (s7_is_hash_table(obj)))
    return(s7_vector_to_string(sc, obj));
  if (is_pair(obj))
    return(s7_list_to_string(sc, obj));
  x = s7_make_string(sc, str = s7_atom_to_c_string(sc, obj, true));
  if (str) free(str);
  return(x);
}



/* -------- newline -------- */

void s7_newline(s7_scheme *sc, s7_pointer port)
{
  write_char(sc, '\n', port);
}


static s7_pointer g_newline(s7_scheme *sc, s7_pointer args)
{
  #define H_newline "(newline :optional port) writes a carriage return to the port"
  s7_pointer port;
  
  if (is_pair(args))
    {
      port = car(args);
      if (!s7_is_output_port(sc, port))
	return(s7_wrong_type_arg_error(sc, "newline", 1, car(args), "an output port"));
    }
  else port = sc->output_port;
  
  s7_newline(sc, port);
  return(sc->UNSPECIFIED);
}


/* -------- write-char -------- */

void s7_write_char(s7_scheme *sc, char c, s7_pointer port)
{
  write_char(sc, c, port);
}


static s7_pointer g_write_char(s7_scheme *sc, s7_pointer args)
{
  #define H_write_char "(write-char char :optional port) writes char to the output port"
  s7_pointer port;
  
  if (!s7_is_character(car(args)))
    return(s7_wrong_type_arg_error(sc, "write-char", 1, car(args), "a character"));
  
  if (is_pair(cdr(args)))
    {
      port = cadr(args);
      if (!s7_is_output_port(sc, port))
	return(s7_wrong_type_arg_error(sc, "write-char", 2, port, "an output port"));
    }
  else port = sc->output_port;
  s7_write_char(sc, s7_character(car(args)), port);
  return(sc->UNSPECIFIED);
}


/* -------- write/display -------- */

static void write_or_display(s7_scheme *sc, s7_pointer obj, s7_pointer port, bool use_write)
{
  char *val;
  val = s7_object_to_c_string_1(sc, obj, use_write);
  write_string(sc, val, port);
  if (val) free(val);
}


void s7_write(s7_scheme *sc, s7_pointer obj, s7_pointer port)
{
  write_or_display(sc, obj, port, true);
}


static s7_pointer g_write(s7_scheme *sc, s7_pointer args)
{
  #define H_write "(write str :optional port) writes str (a string) to the output port"
  s7_pointer port;
  
  if (is_pair(cdr(args)))
    {
      port = cadr(args);
      if (!s7_is_output_port(sc, port))
	return(s7_wrong_type_arg_error(sc, "write", 2, port, "an output port"));
    }
  else port = sc->output_port;
  write_or_display(sc, car(args), port, true);
  return(sc->UNSPECIFIED);
}


void s7_display(s7_scheme *sc, s7_pointer obj, s7_pointer port)
{
  write_or_display(sc, obj, port, false);
}


static s7_pointer g_display(s7_scheme *sc, s7_pointer args)
{
  #define H_display "(display str :optional port) writes str (a string) to the output port"
  s7_pointer port;
  
  if (is_pair(cdr(args)))
    {
      port = cadr(args);
      if (!s7_is_output_port(sc, port))
	return(s7_wrong_type_arg_error(sc, "display", 2, port, "an output port"));
    }
  else port = sc->output_port;
  write_or_display(sc, car(args), port, false);
  return(sc->UNSPECIFIED);
}


/* -------- read|write-byte -------- */

static s7_pointer g_read_byte(s7_scheme *sc, s7_pointer args)
{
  #define H_read_byte "(read-byte :optional port): reads a byte from the input port"
  s7_pointer port;
  
  if (is_pair(args))
    {
      port = car(args);
      if ((!s7_is_input_port(sc, port)) ||
	  (!is_file_port(port)))
	return(s7_wrong_type_arg_error(sc, "read-byte", 1, car(args), "an input file port"));
    }
  else port = sc->input_port;
  return(s7_make_integer(sc, fgetc(port_file(port))));
}


static s7_pointer g_write_byte(s7_scheme *sc, s7_pointer args)
{
  #define H_write_byte "(write-byte byte :optional port): writes byte to the output port"
  s7_pointer port;
  
  if (!s7_is_integer(car(args)))
    return(s7_wrong_type_arg_error(sc, "write-byte", 1, car(args), "an integer"));
  
  if (is_pair(cdr(args)))
    {
      port = cadr(args);
      if ((!s7_is_output_port(sc, port)) ||
	  (!is_file_port(port)))
	return(s7_wrong_type_arg_error(sc, "write-byte", 2, port, "an output file port"));
    }
  else port = sc->output_port;
  
  fputc((unsigned char)s7_integer(car(args)), port_file(port));
  return(car(args));
}


static s7_pointer g_call_with_output_string(s7_scheme *sc, s7_pointer args)
{
  #define H_call_with_output_string "(call-with-output-string proc) opens a string port applies proc to it, then returns the collected output"
  s7_pointer port, result;
  
  port = s7_open_output_string(sc);
  push_stack(sc, OP_EVAL_STRING_DONE, sc->args, sc->code);
  sc->code = car(args);
  sc->args = make_list_1(sc, port);
  eval(sc, OP_APPLY);
  result = s7_make_string(sc, s7_get_output_string(sc, port));
  s7_close_output_port(sc, port);
  return(result);
}


static s7_pointer g_call_with_output_file(s7_scheme *sc, s7_pointer args)
{
  #define H_call_with_output_file "(call-with-output-file filename proc) opens filename and calls proc with the output port as its argument"
  s7_pointer port;
  
  if (!s7_is_string(car(args)))
    return(s7_wrong_type_arg_error(sc, "call-with-output-file", 1, car(args), "a string (a filename)"));
  
  port = s7_open_output_file(sc, s7_string(car(args)), "w");
  push_stack(sc, OP_EVAL_STRING_DONE, sc->args, sc->code);
  sc->code = cadr(args);
  sc->args = make_list_1(sc, port);
  eval(sc, OP_APPLY);
  s7_close_output_port(sc, port);
  return(sc->value);
}


static s7_pointer g_with_output_to_string(s7_scheme *sc, s7_pointer args)
{
  #define H_with_output_to_string "(with-output-to-string thunk) opens a string as a temporary current-output-port, calls thunk, then returns the collected output"
  s7_pointer old_output_port, result;
  
  old_output_port = sc->output_port;
  sc->output_port = s7_open_output_string(sc);
  push_stack(sc, OP_EVAL_STRING_DONE, sc->args, sc->code);
  sc->code = car(args);
  sc->args = sc->NIL;
  eval(sc, OP_APPLY);
  result = s7_make_string(sc, s7_get_output_string(sc, sc->output_port));
  s7_close_output_port(sc, sc->output_port);
  sc->output_port = old_output_port;
  return(result);
}


static s7_pointer g_with_output_to_file(s7_scheme *sc, s7_pointer args)
{
  #define H_with_output_to_file "(with-output-to-file filename thunk) opens filename as the temporary current-output-port and calls thunk"
  s7_pointer old_output_port;
  
  if (!s7_is_string(car(args)))
    return(s7_wrong_type_arg_error(sc, "with-output-to-file", 1, car(args), "a string (a filename)"));
  
  old_output_port = sc->output_port;
  sc->output_port = s7_open_output_file(sc, s7_string(car(args)), "w");
  push_stack(sc, OP_EVAL_STRING_DONE, sc->args, sc->code);
  sc->code = cadr(args);
  sc->args = sc->NIL;
  eval(sc, OP_APPLY);
  s7_close_output_port(sc, sc->output_port);
  sc->output_port = old_output_port;
  return(sc->value);
}





/* -------------------------------- lists -------------------------------- */

static s7_pointer cons_untyped(s7_scheme *sc, s7_pointer a, s7_pointer b) 
{
  s7_pointer x;
  
  ASSERT_IS_OBJECT(a, "cons car");
  ASSERT_IS_OBJECT(b, "cons cdr");
  
  x = new_cell(sc); /* might trigger gc */
  car(x) = a;
  cdr(x) = b;
  return(x);
}


static s7_pointer s7_immutable_cons(s7_scheme *sc, s7_pointer a, s7_pointer b)
{
  s7_pointer x;
  x = cons_untyped(sc, a, b);
  set_type(x, T_PAIR | T_IMMUTABLE);
  return(x);
}


s7_pointer s7_cons(s7_scheme *sc, s7_pointer a, s7_pointer b) 
{
  s7_pointer x;
  x = cons_untyped(sc, a, b);
  set_type(x, T_PAIR);
  return(x);
}


static s7_pointer s7_permanent_cons(s7_pointer a, s7_pointer b, int type)
{
  /* for the symbol table which is never GC'd */
  s7_pointer x;
  x = (s7_cell *)calloc(1, sizeof(s7_cell));
  car(x) = a;
  cdr(x) = b;
  set_type(x, type);
  return(x);
}


bool s7_is_pair(s7_pointer p)     
{ 
  return(is_pair(p));
}


s7_pointer s7_car(s7_pointer p)           
{
  return((p)->object.cons.car);
}


s7_pointer s7_cdr(s7_pointer p)           
{
  return((p)->object.cons.cdr);
}


s7_pointer s7_set_car(s7_pointer p, s7_pointer q) 
{ 
  car(p) = q;
  return(p);
}


s7_pointer s7_set_cdr(s7_pointer p, s7_pointer q) 
{ 
  cdr(p) = q;
  return(p);
}


static s7_pointer make_list_1(s7_scheme *sc, s7_pointer a) 
{
  s7_pointer x;
  x = new_cell(sc);
  set_type(x, T_PAIR);
  car(x) = a;
  cdr(x) = sc->NIL;
  return(x);
}


static s7_pointer make_list_2(s7_scheme *sc, s7_pointer a, s7_pointer b) 
{
  s7_pointer x, y;
  y = new_cell(sc);
  set_type(y, T_PAIR);
  car(y) = b;
  cdr(y) = sc->NIL;
  x = new_cell(sc); /* order matters because the GC will see "y" and expect it to have legit car/cdr */
  set_type(x, T_PAIR);
  car(x) = a;
  cdr(x) = y;
  return(x);
}


static s7_pointer make_list_3(s7_scheme *sc, s7_pointer a, s7_pointer b, s7_pointer c) 
{
  s7_pointer x, y, z;
  z = new_cell(sc);
  set_type(z, T_PAIR);
  car(z) = c;
  cdr(z) = sc->NIL;
  y = new_cell(sc);
  set_type(y, T_PAIR);
  car(y) = b;
  cdr(y) = z;
  x = new_cell(sc);
  set_type(x, T_PAIR);
  car(x) = a;
  cdr(x) = y;
  return(x);
}


s7_pointer s7_list_ref(s7_scheme *sc, s7_pointer lst, int num)
{
  int i;
  s7_pointer x;
  
  if (num == 0)
    return(s7_car(lst));
  if (num < 0)
    return(sc->NIL);
  for (x = lst, i = 0; (i < num) && (is_pair(x)); i++, x = cdr(x)) {}
  if ((i == num) &&
      (is_pair(x)))
    return(car(x));
  return(sc->NIL);
}


s7_pointer s7_list_set(s7_scheme *sc, s7_pointer lst, int num, s7_pointer val)
{
  int i;
  s7_pointer x;
  for (x = lst, i = 0; (i < num) && (is_pair(x)); i++, x = cdr(x)) {}
  if ((i == num) &&
      (is_pair(x)))
    car(x) = val;
  return(val);
}


s7_pointer s7_member(s7_scheme *sc, s7_pointer sym, s7_pointer lst)
{
  s7_pointer x;
  int v;
  for (x = lst, v = 0; is_pair(x); x = cdr(x))
    if (s7_is_equal(sym, car(x)))
      return(x);
  return(sc->F);
}


s7_pointer s7_assoc(s7_scheme *sc, s7_pointer sym, s7_pointer lst)
{
  s7_pointer x;
  int v;
  for (x = lst, v = 0; is_pair(x); x = cdr(x))
    if ((is_pair(s7_car(x))) &&
	(s7_is_equal(sym, car(car(x)))))
      return(car(x));
  return(sc->F);
}


s7_pointer s7_reverse(s7_scheme *sc, s7_pointer a) 
{
  /* reverse list -- produce new list */
  s7_pointer p = sc->NIL;
  for ( ; is_pair(a); a = cdr(a)) 
    p = s7_cons(sc, car(a), p);
  if (a == sc->NIL)
    return(p);
  return(sc->NIL);
}


static s7_pointer s7_reverse_in_place(s7_scheme *sc, s7_pointer term, s7_pointer list) 
{
  s7_pointer p = list, result = term, q;
  while (p != sc->NIL)
    {
      q = cdr(p);
      if ((!is_pair(q)) &&
	  (q != sc->NIL))
	return(sc->NIL); /* improper list? */
      cdr(p) = result;
      result = p;
      p = q;
    }
  return(result);
}


static s7_pointer safe_reverse_in_place(s7_scheme *sc, s7_pointer list) 
{
  s7_pointer p = list, result, q;
  result = sc->NIL;

  while (p != sc->NIL)
    {
      q = cdr(p);
      /*   also if (list == sc->NIL) || (cdr(list) == sc->NIL) return(list) */
      cdr(p) = result;
      result = p;
      p = q;
    }
  return(result);
}


s7_pointer s7_append(s7_scheme *sc, s7_pointer a, s7_pointer b) 
{
  s7_pointer p = b, q;
  
  if (a != sc->NIL) 
    {
      a = s7_reverse(sc, a);
      while (a != sc->NIL) 
	{
	  q = cdr(a);
	  cdr(a) = p;
	  p = a;
	  a = q;
	}
    }
  return(p);
}


static int safe_list_length(s7_scheme *sc, s7_pointer a)
{
  /* assume that "a" is a proper list */
  int i = 0;
  s7_pointer b;
  for (b = a; b != sc->NIL; i++, b = cdr(b)) {};
  return(i);
}


int s7_list_length(s7_scheme *sc, s7_pointer a) 
{
  int i;
  s7_pointer slow, fast;
  
  slow = fast = a;
  for (i = 0; ; i += 2)
    {
      if (!is_pair(fast))
	{
	  if (fast == sc->NIL)
	    return(i);
	  return(-i);
	}
      
      fast = cdr(fast);
      if (!is_pair(fast)) 
	{
	  if (fast == sc->NIL)
	    return(i + 1);
	  return(-i - 1);
	}
      
      fast = cdr(fast);
      slow = cdr(slow);
      if (fast == slow) 
	{
	  /* the fast pointer has looped back around and caught up
	     with the slow pointer, hence the structure is circular,
	     not of finite length, and therefore not a list */
	  return(0);
	}
    }
  return(0);
}


static s7_pointer g_is_null(s7_scheme *sc, s7_pointer args)
{
  #define H_is_null "(null? obj) returns #t if obj is the empty list"
  return(make_boolean(sc, car(args) == sc->NIL));
}


static s7_pointer g_is_pair(s7_scheme *sc, s7_pointer args)
{
  #define H_is_pair "(pair? obj) returns #t if obj is a pair (a non-empty list)"
  return(make_boolean(sc, is_pair(car(args))));
}


bool s7_is_list(s7_scheme *sc, s7_pointer p)
{
  return((p == sc->NIL) ||
	 (is_pair(p)));
}


static s7_pointer g_is_list(s7_scheme *sc, s7_pointer args)
{
  s7_pointer slow, fast;
  #define H_is_list "(list? obj) returns #t if obj is a list"
  
  slow = fast = car(args);
  while (true)
    {
      if (!is_pair(fast)) 
	return(make_boolean(sc, (fast == sc->NIL))); /* else it's an improper list */
      
      fast = cdr(fast);
      if (!is_pair(fast)) 
	return(make_boolean(sc, (fast == sc->NIL)));
      
      fast = cdr(fast);
      slow = cdr(slow);
      if (fast == slow) 
	return(sc->F);
    }
  return(sc->T);
}


static s7_pointer g_list_ref(s7_scheme *sc, s7_pointer args)
{
  #define H_list_ref "(list-ref lst i) returns the i-th element (0-based) of the list"
  
  int i, index;
  s7_pointer p;
  if (!is_pair(car(args)))
    return(s7_wrong_type_arg_error(sc, "list-ref", 1, car(args), "a pair"));
  if ((!s7_is_integer(cadr(args))) ||
      (s7_integer(cadr(args)) < 0))
    return(s7_wrong_type_arg_error(sc, "list-ref", 2, cadr(args), "a non-negative integer"));
  
  index = s7_integer(cadr(args));
  if (index < 0)
    return(s7_out_of_range_error(sc, "list-ref", 2, cadr(args), "non-negative"));
  
  for (i = 0, p = car(args); (i < index) && is_pair(p); i++, p = cdr(p)) {}
  
  if (p == sc->NIL)
    return(s7_out_of_range_error(sc, "list-ref", 2, cadr(args), "less than list length"));
  if (!is_pair(p))
    return(s7_wrong_type_arg_error(sc, "list-ref", i, p, "a proper list"));
  
  return(car(p));
}


static s7_pointer g_list_set(s7_scheme *sc, s7_pointer args)
{
  #define H_list_set "(list-set! lst i val) sets the i-th element (0-based) of the list to val"
  
  int i, index;
  s7_pointer p;
  
  if (!is_pair(car(args)))
    return(s7_wrong_type_arg_error(sc, "list-set!", 1, car(args), "a pair"));
  if ((!s7_is_integer(cadr(args))) ||
      (s7_integer(cadr(args)) < 0))
    return(s7_wrong_type_arg_error(sc, "list-set!", 2, cadr(args), "a non-negative integer"));
  
  index = s7_integer(cadr(args));
  if (index < 0)
    return(s7_out_of_range_error(sc, "list-set!", 2, cadr(args), "non-negative"));
  
  for (i = 0, p = car(args); (i < index) && is_pair(p); i++, p = cdr(p)) {}
  
  if (p == sc->NIL)
    return(s7_out_of_range_error(sc, "list-set!", 2, cadr(args), "less than list length"));
  if (!is_pair(p))
    return(s7_wrong_type_arg_error(sc, "list-set!", i, p, "a proper list"));
  
  car(p) = caddr(args);
  return(caddr(args));
}


static s7_pointer g_list_tail(s7_scheme *sc, s7_pointer args)
{
  #define H_list_tail "(list-tail lst i) returns the list from the i-th element on"
  
  int i, index;
  s7_pointer p;
  if ((!is_pair(car(args))) &&
      (car(args) != sc->NIL))
    return(s7_wrong_type_arg_error(sc, "list-tail", 1, car(args), "a list"));
  if (!s7_is_integer(cadr(args)))
    return(s7_wrong_type_arg_error(sc, "list-tail", 2, cadr(args), "an integer"));
  
  index = s7_integer(cadr(args));
  if (index < 0)
    return(s7_out_of_range_error(sc, "list-tail", 2, cadr(args), "non-negative"));
  
  for (i = 0, p = car(args); (i < index) && is_pair(p); i++, p = cdr(p)) {}
  
  if (i < index)
    return(s7_out_of_range_error(sc, "list-tail", 2, cadr(args), "less than list length"));
  
  return(p);
}


static s7_pointer g_car(s7_scheme *sc, s7_pointer args)
{
  #define H_car "(car pair) returns the first element of the pair"
  
  if (!is_pair(car(args)))
    return(s7_wrong_type_arg_error(sc, "car", 1, car(args), "a pair"));
  
  return(caar(args));
}


static s7_pointer g_cdr(s7_scheme *sc, s7_pointer args)
{
  #define H_cdr "(cdr pair) returns the second element of the pair"
  
  if (!is_pair(car(args)))
    return(s7_wrong_type_arg_error(sc, "cdr", 1, car(args), "a pair"));
  
  return(cdar(args));
}


static s7_pointer g_cons(s7_scheme *sc, s7_pointer args)
{
  #define H_cons "(cons a b) returns a pair containing a and b"
  
  cdr(args) = cadr(args);
  return(args);
}


static s7_pointer g_set_car(s7_scheme *sc, s7_pointer args)
{
  #define H_set_car "(set-car! pair val) sets the pair's first element to val"
  
  if (!is_pair(car(args)))
    return(s7_wrong_type_arg_error(sc, "set-car!", 1, car(args), "a pair"));
  
  if (s7_is_immutable(car(args))) 
    return(s7_wrong_type_arg_error(sc, "set-car!", 1, car(args), "a mutable pair"));
  
  caar(args) = cadr(args);
  return(args);
}


static s7_pointer g_set_cdr(s7_scheme *sc, s7_pointer args)
{
  #define H_set_cdr "(set-cdr! pair val) sets the pair's second element to val"
  
  if (!is_pair(car(args)))
    return(s7_wrong_type_arg_error(sc, "set-cdr!", 1, car(args), "a pair"));
  
  if (s7_is_immutable(car(args))) 
    return(s7_wrong_type_arg_error(sc, "set-cdr!", 1, car(args), "a mutable pair"));
  
  cdar(args) = cadr(args);
  return(args);
}


static s7_pointer g_caar(s7_scheme *sc, s7_pointer args)
{
  #define H_caar "(caar lst) returns (car (car lst))"
  s7_pointer lst = car(args);
  if ((is_pair(lst)) && (is_pair(car(lst)))) return(car(car(lst)));
  return(s7_wrong_type_arg_error(sc, "caar", 1, car(args), "a pair"));
}


static s7_pointer g_cadr(s7_scheme *sc, s7_pointer args)
{
  #define H_cadr "(cadr lst) returns (car (cdr lst))"
  s7_pointer lst = car(args);
  if ((is_pair(lst)) && (is_pair(cdr(lst)))) return(car(cdr(lst)));
  return(s7_wrong_type_arg_error(sc, "cadr", 1, car(args), "a pair"));
}


static s7_pointer g_cdar(s7_scheme *sc, s7_pointer args)
{
  #define H_cdar "(cdar lst) returns (cdr (car lst))"
  s7_pointer lst = car(args);
  if ((is_pair(lst)) && (is_pair(car(lst)))) return(cdr(car(lst)));
  return(s7_wrong_type_arg_error(sc, "cdar", 1, car(args), "a pair"));
}


static s7_pointer g_cddr(s7_scheme *sc, s7_pointer args)
{
  #define H_cddr "(cddr lst) returns (cdr (cdr lst))"
  s7_pointer lst = car(args);
  if ((is_pair(lst)) && (is_pair(cdr(lst)))) return(cdr(cdr(lst)));
  return(s7_wrong_type_arg_error(sc, "cddr", 1, car(args), "a pair"));
}


static s7_pointer g_caaar(s7_scheme *sc, s7_pointer args)
{
  #define H_caaar "(caaar lst) returns (car (car (car lst)))"
  s7_pointer lst = car(args);
  if ((is_pair(lst)) && (is_pair(car(lst))) && (is_pair(car(car(lst))))) return(car(car(car(lst))));
  return(s7_wrong_type_arg_error(sc, "caaar", 1, car(args), "a pair"));
}


static s7_pointer g_caadr(s7_scheme *sc, s7_pointer args)
{
  #define H_caadr "(caadr lst) returns (car (car (cdr lst)))"
  s7_pointer lst = car(args);
  if ((is_pair(lst)) && (is_pair(cdr(lst))) && (is_pair(cadr(lst)))) return(car(car(cdr(lst))));
  return(s7_wrong_type_arg_error(sc, "caadr", 1, car(args), "a pair"));
}


static s7_pointer g_cadar(s7_scheme *sc, s7_pointer args)
{
  #define H_cadar "(cadar lst) returns (car (cdr (car lst)))"
  s7_pointer lst = car(args);
  if ((is_pair(lst)) && (is_pair(car(lst))) && (is_pair(cdr(car(lst))))) return(car(cdr(car(lst))));
  return(s7_wrong_type_arg_error(sc, "cadar", 1, car(args), "a pair"));
}


static s7_pointer g_cdaar(s7_scheme *sc, s7_pointer args)
{
  #define H_cdaar "(cdaar lst) returns (cdr (car (car lst)))"
  s7_pointer lst = car(args);
  if ((is_pair(lst)) && (is_pair(car(lst))) && (is_pair(car(car(lst))))) return(cdr(car(car(lst))));
  return(s7_wrong_type_arg_error(sc, "cdaar", 1, car(args), "a pair"));
}


static s7_pointer g_caddr(s7_scheme *sc, s7_pointer args)
{
  #define H_caddr "(caddr lst) returns (car (cdr (cdr lst)))"
  s7_pointer lst = car(args);
  if ((is_pair(lst)) && (is_pair(cdr(lst))) && (is_pair(cddr(lst)))) return(car(cdr(cdr(lst))));
  return(s7_wrong_type_arg_error(sc, "caddr", 1, car(args), "a pair"));
}


static s7_pointer g_cdddr(s7_scheme *sc, s7_pointer args)
{
  #define H_cdddr "(cdddr lst) returns (cdr (cdr (cdr lst)))"
  s7_pointer lst = car(args);
  if ((is_pair(lst)) && (is_pair(cdr(lst))) && (is_pair(cddr(lst)))) return(cdr(cdr(cdr(lst))));
  return(s7_wrong_type_arg_error(sc, "cdddr", 1, car(args), "a pair"));
}


static s7_pointer g_cdadr(s7_scheme *sc, s7_pointer args)
{
  #define H_cdadr "(cdadr lst) returns (cdr (car (cdr lst)))"
  s7_pointer lst = car(args);
  if ((is_pair(lst)) && (is_pair(cdr(lst))) && (is_pair(cadr(lst)))) return(cdr(car(cdr(lst))));
  return(s7_wrong_type_arg_error(sc, "cdadr", 1, car(args), "a pair"));
}


static s7_pointer g_cddar(s7_scheme *sc, s7_pointer args)
{
  #define H_cddar "(cddar lst) returns (cdr (cdr (car lst)))"
  s7_pointer lst = car(args);
  if ((is_pair(lst)) && (is_pair(car(lst))) && (is_pair(cdar(lst)))) return(cdr(cdr(car(lst))));
  return(s7_wrong_type_arg_error(sc, "cddar", 1, car(args), "a pair"));
}


static s7_pointer g_caaaar(s7_scheme *sc, s7_pointer args)
{
  #define H_caaaar "(caaaar lst) returns (car (car (car (car lst))))"
  s7_pointer lst = car(args);
  if ((is_pair(lst)) && (is_pair(car(lst))) && (is_pair(caar(lst))) && (is_pair(caaar(lst)))) return(car(car(car(car(lst)))));
  return(s7_wrong_type_arg_error(sc, "caaaar", 1, car(args), "a pair"));
}


static s7_pointer g_caaadr(s7_scheme *sc, s7_pointer args)
{
  #define H_caaadr "(caaadr lst) returns (car (car (car (cdr lst))))"
  s7_pointer lst = car(args);
  if ((is_pair(lst)) && (is_pair(cdr(lst))) && (is_pair(cadr(lst))) && (is_pair(caadr(lst)))) return(car(car(car(cdr(lst)))));
  return(s7_wrong_type_arg_error(sc, "caaadr", 1, car(args), "a pair"));
}


static s7_pointer g_caadar(s7_scheme *sc, s7_pointer args)
{
  #define H_caadar "(caadar lst) returns (car (car (cdr (car lst))))"
  s7_pointer lst = car(args);
  if ((is_pair(lst)) && (is_pair(car(lst))) && (is_pair(cdar(lst))) && (is_pair(cadar(lst)))) return(car(car(cdr(car(lst)))));
  return(s7_wrong_type_arg_error(sc, "caadar", 1, car(args), "a pair"));
}


static s7_pointer g_cadaar(s7_scheme *sc, s7_pointer args)
{
  #define H_cadaar "(cadaar lst) returns (car (cdr (car (car lst))))"
  s7_pointer lst = car(args);
  if ((is_pair(lst)) && (is_pair(car(lst))) && (is_pair(caar(lst))) && (is_pair(cdaar(lst)))) return(car(cdr(car(car(lst)))));
  return(s7_wrong_type_arg_error(sc, "cadaar", 1, car(args), "a pair"));
}


static s7_pointer g_caaddr(s7_scheme *sc, s7_pointer args)
{
  #define H_caaddr "(caaddr lst) returns (car (car (cdr (cdr lst))))"
  s7_pointer lst = car(args);
  if ((is_pair(lst)) && (is_pair(cdr(lst))) && (is_pair(cddr(lst))) && (is_pair(caddr(lst)))) return(car(car(cdr(cdr(lst)))));
  return(s7_wrong_type_arg_error(sc, "caaddr", 1, car(args), "a pair"));
}


static s7_pointer g_cadddr(s7_scheme *sc, s7_pointer args)
{
  #define H_cadddr "(cadddr lst) returns (car (cdr (cdr (cdr lst))))"
  s7_pointer lst = car(args);
  if ((is_pair(lst)) && (is_pair(cdr(lst))) && (is_pair(cddr(lst))) && (is_pair(cdddr(lst)))) return(car(cdr(cdr(cdr(lst)))));
  return(s7_wrong_type_arg_error(sc, "cadddr", 1, car(args), "a pair"));
}


static s7_pointer g_cadadr(s7_scheme *sc, s7_pointer args)
{
  #define H_cadadr "(cadadr lst) returns (car (cdr (car (cdr lst))))"
  s7_pointer lst = car(args);
  if ((is_pair(lst)) && (is_pair(cdr(lst))) && (is_pair(cadr(lst))) && (is_pair(cdadr(lst)))) return(car(cdr(car(cdr(lst)))));
  return(s7_wrong_type_arg_error(sc, "cadadr", 1, car(args), "a pair"));
}


static s7_pointer g_caddar(s7_scheme *sc, s7_pointer args)
{
  #define H_caddar "(caddar lst) returns (car (cdr (cdr (car lst))))"
  s7_pointer lst = car(args);
  if ((is_pair(lst)) && (is_pair(car(lst))) && (is_pair(cdar(lst))) && (is_pair(cddar(lst)))) return(car(cdr(cdr(car(lst)))));
  return(s7_wrong_type_arg_error(sc, "caddar", 1, car(args), "a pair"));
}


static s7_pointer g_cdaaar(s7_scheme *sc, s7_pointer args)
{
  #define H_cdaaar "(cdaaar lst) returns (cdr (car (car (car lst))))"
  s7_pointer lst = car(args);
  if ((is_pair(lst)) && (is_pair(car(lst))) && (is_pair(caar(lst))) && (is_pair(caaar(lst)))) return(cdr(car(car(car(lst)))));
  return(s7_wrong_type_arg_error(sc, "cdaaar", 1, car(args), "a pair"));
}


static s7_pointer g_cdaadr(s7_scheme *sc, s7_pointer args)
{
  #define H_cdaadr "(cdaadr lst) returns (cdr (car (car (cdr lst))))"
  s7_pointer lst = car(args);
  if ((is_pair(lst)) && (is_pair(cdr(lst))) && (is_pair(cadr(lst))) && (is_pair(caadr(lst)))) return(cdr(car(car(cdr(lst)))));
  return(s7_wrong_type_arg_error(sc, "cdaadr", 1, car(args), "a pair"));
}


static s7_pointer g_cdadar(s7_scheme *sc, s7_pointer args)
{
  #define H_cdadar "(cdadar lst) returns (cdr (car (cdr (car lst))))"
  s7_pointer lst = car(args);
  if ((is_pair(lst)) && (is_pair(car(lst))) && (is_pair(cdar(lst))) && (is_pair(cadar(lst)))) return(cdr(car(cdr(car(lst)))));
  return(s7_wrong_type_arg_error(sc, "cdadar", 1, car(args), "a pair"));
}


static s7_pointer g_cddaar(s7_scheme *sc, s7_pointer args)
{
  #define H_cddaar "(cddaar lst) returns (cdr (cdr (car (car lst))))"
  s7_pointer lst = car(args);
  if ((is_pair(lst)) && (is_pair(car(lst))) && (is_pair(caar(lst))) && (is_pair(cdaar(lst)))) return(cdr(cdr(car(car(lst)))));
  return(s7_wrong_type_arg_error(sc, "cddaar", 1, car(args), "a pair"));
}


static s7_pointer g_cdaddr(s7_scheme *sc, s7_pointer args)
{
  #define H_cdaddr "(cdaddr lst) returns (cdr (car (cdr (cdr lst))))"
  s7_pointer lst = car(args);
  if ((is_pair(lst)) && (is_pair(cdr(lst))) && (is_pair(cddr(lst))) && (is_pair(caddr(lst)))) return(cdr(car(cdr(cdr(lst)))));
  return(s7_wrong_type_arg_error(sc, "cdaddr", 1, car(args), "a pair"));
}


static s7_pointer g_cddddr(s7_scheme *sc, s7_pointer args)
{
  #define H_cddddr "(cddddr lst) returns (cdr (cdr (cdr (cdr lst))))"
  s7_pointer lst = car(args);
  if ((is_pair(lst)) && (is_pair(cdr(lst))) && (is_pair(cddr(lst))) && (is_pair(cdddr(lst)))) return(cdr(cdr(cdr(cdr(lst)))));
  return(s7_wrong_type_arg_error(sc, "cddddr", 1, car(args), "a pair"));
}


static s7_pointer g_cddadr(s7_scheme *sc, s7_pointer args)
{
  #define H_cddadr "(cddadr lst) returns (cdr (cdr (car (cdr lst))))"
  s7_pointer lst = car(args);
  if ((is_pair(lst)) && (is_pair(cdr(lst))) && (is_pair(cadr(lst))) && (is_pair(cdadr(lst)))) return(cdr(cdr(car(cdr(lst)))));
  return(s7_wrong_type_arg_error(sc, "cddadr", 1, car(args), "a pair"));
}


static s7_pointer g_cdddar(s7_scheme *sc, s7_pointer args)
{
  #define H_cdddar "(cdddar lst) returns (cdr (cdr (cdr (car lst))))"
  s7_pointer lst = car(args);
  if ((is_pair(lst)) && (is_pair(car(lst))) && (is_pair(cdar(lst))) && (is_pair(cddar(lst)))) return(cdr(cdr(cdr(car(lst)))));
  return(s7_wrong_type_arg_error(sc, "cdddar", 1, car(args), "a pair"));
}


static s7_pointer g_reverse(s7_scheme *sc, s7_pointer args)
{
  #define H_reverse "(reverse lst) returns a list with the elements of lst in reverse order"
  s7_pointer p, np;
  
  p = car(args);
  if (p == sc->NIL)
    return(sc->NIL);
  
  if (!is_pair(p))
    return(s7_wrong_type_arg_error(sc, "reverse", 1, p, "a list"));
  
  np = s7_reverse(sc, p);
  if (np == sc->NIL)
    return(s7_wrong_type_arg_error(sc, "reverse", 1, p, "a proper list"));
  
  return(np);
}


static s7_pointer g_reverse_in_place(s7_scheme *sc, s7_pointer args)
{
  #define H_reverse_in_place "(reverse! lst) reverses lst in place"
  s7_pointer p, np;
  
  p = car(args);
  if (p == sc->NIL)
    return(sc->NIL);
  
  if (!is_pair(p))
    return(s7_wrong_type_arg_error(sc, "reverse!", 1, p, "a list"));
  
  np = s7_reverse_in_place(sc, sc->NIL, p);
  if (np == sc->NIL)
    return(s7_wrong_type_arg_error(sc, "reverse!", 1, p, "a proper list"));
  
  return(np);
}


s7_pointer s7_remv(s7_scheme *sc, s7_pointer a, s7_pointer obj) 
{
  /* used in xen.c */
  s7_pointer p = sc->NIL;
  for ( ; is_pair(a); a = cdr(a))
    if (car(a) != obj)
      p = s7_cons(sc, car(a), p);
  return(s7_reverse(sc, p));
}


static s7_pointer g_length(s7_scheme *sc, s7_pointer args)
{
  #define H_length "(length lst) returns the length of the list lst"
  
  int len;
  
  if (car(args) == sc->NIL)
    return(s7_make_integer(sc, 0));
  
  if (!is_pair(car(args))) 
    return(s7_wrong_type_arg_error(sc, "length", 1, car(args), "a list"));
  
  len = s7_list_length(sc, car(args));
  
  if (len < 0) 
    return(s7_wrong_type_arg_error(sc, "length:", 1, car(args), "a proper (not a dotted) list"));
  
  if (len == 0)
    return(s7_wrong_type_arg_error(sc, "length:", 1, car(args), "a proper (not a circular) list"));
  
  return(s7_make_integer(sc, len));
}


static s7_pointer g_assq_1(s7_scheme *sc, s7_pointer args, const char *name, bool (*eq_func)(s7_pointer a, s7_pointer b))
{
  #define H_assq "(assq obj alist) returns the key-value pair associated (via eq?) with the key obj in the association list alist"
  #define H_assv "(assv obj alist) returns the key-value pair associated (via eqv?) with the key obj in the association list alist"
  #define H_assoc "(assoc obj alist) returns the key-value pair associated (via equal?) with the key obj in the association list alist"
  
  if ((!is_pair(cadr(args))) && (cadr(args) != sc->NIL))
    return(s7_wrong_type_arg_error(sc, name, 2, cadr(args), "a list"));
  
  sc->x = car(args);
  for (sc->y = cadr(args); is_pair(sc->y); sc->y = cdr(sc->y)) 
    if (eq_func(sc->x, caar(sc->y)))
      return(car(sc->y));
  
  return(sc->F);
}      


static s7_pointer g_assq(s7_scheme *sc, s7_pointer args) {return(g_assq_1(sc, args, "assq", s7_is_eq));}
static s7_pointer g_assv(s7_scheme *sc, s7_pointer args) {return(g_assq_1(sc, args, "assv", s7_is_eqv));}
static s7_pointer g_assoc(s7_scheme *sc, s7_pointer args) {return(g_assq_1(sc, args, "assoc", s7_is_equal));}


static s7_pointer g_memq_1(s7_scheme *sc, s7_pointer args, const char *name, bool (*eq_func)(s7_pointer a, s7_pointer b))
{
  #define H_memq "(memq obj list) looks for obj in list and returns the list from that point if it is found, otherwise #f. memq uses eq?"
  #define H_memv "(memv obj list) looks for obj in list and returns the list from that point if it is found, otherwise #f. memv uses eqv?"
  #define H_member "(member obj list) looks for obj in list and returns the list from that point if it is found, otherwise #f. member uses equal?"
  
  if ((!is_pair(cadr(args))) && (cadr(args) != sc->NIL))
    return(s7_wrong_type_arg_error(sc, name, 2, cadr(args), "a list"));
  
  for (sc->x = cadr(args); is_pair(sc->x); sc->x = cdr(sc->x)) 
    if (eq_func(car(args), car(sc->x)))
      return(sc->x);
  
  return(sc->F);
}     


static s7_pointer g_memq(s7_scheme *sc, s7_pointer args) {return(g_memq_1(sc, args, "memq", s7_is_eq));}
static s7_pointer g_memv(s7_scheme *sc, s7_pointer args) {return(g_memq_1(sc, args, "memv", s7_is_eqv));}
static s7_pointer g_member(s7_scheme *sc, s7_pointer args) {return(g_memq_1(sc, args, "member", s7_is_equal));}


static s7_pointer g_is_provided(s7_scheme *sc, s7_pointer args)
{
  #define H_is_provided "(provided? sym) returns #t if sym is a member of the *features* list"
  if (!s7_is_symbol(car(args)))
    return(s7_wrong_type_arg_error(sc, "provided?", 1, car(args), "a symbol"));
  return(make_boolean(sc, is_pair(g_member(sc, make_list_2(sc, car(args), s7_name_to_value(sc, "*features*"))))));
}


static s7_pointer g_provide(s7_scheme *sc, s7_pointer args)
{
  #define H_provide "(provide sym) adds sym to the *features* list"
  if (!s7_is_symbol(car(args)))
    return(s7_wrong_type_arg_error(sc, "provide", 1, car(args), "a symbol"));
  s7_symbol_set_value(sc, 
		      s7_make_symbol(sc, "*features*"),
		      s7_cons(sc, 
			      car(args), 
			      s7_name_to_value(sc, "*features*")));
  return(car(args));
}


static s7_pointer g_list(s7_scheme *sc, s7_pointer args)
{
  #define H_list "(list ...) returns its arguments in a list"
  return(args);
}


static s7_pointer g_append(s7_scheme *sc, s7_pointer args)
{
  #define H_append "(append ...) returns its argument lists appended into one list"
  
  int i;
  if (args == sc->NIL) 
    return(sc->NIL);
  
  if (cdr(args) == sc->NIL)
    return(car(args)); 
  
  sc->x = sc->NIL;
  
  for (i = 1, sc->y = args; sc->y != sc->NIL; i++, sc->y = cdr(sc->y)) 
    {
      if (cdr(sc->y) == sc->NIL)
	return(s7_append(sc, sc->x, car(sc->y)));
      
      if (g_is_list(sc, sc->y) == sc->F)
	return(s7_wrong_type_arg_error(sc, "append", i, car(sc->y), "a list"));
      
      sc->x = s7_append(sc, sc->x, car(sc->y));
    }
  return(sc->x);
}


/* -------- list-line-number -------- */

static s7_pointer g_list_line_number(s7_scheme *sc, s7_pointer args)
{
  #define H_list_line_number "(list-line-number lst) returns the line number where it thinks lst occurred"
  if (is_pair(car(args)))
    return(s7_make_integer(sc, pair_line_number(car(args))));
  return(sc->F);
}




/* -------------------------------- vectors -------------------------------- */


bool s7_is_vector(s7_pointer p)    
{ 
  return(type(p) == T_VECTOR);
}


s7_pointer s7_make_vector(s7_scheme *sc, int len) 
{
  s7_pointer x;
  x = new_cell(sc);
  set_type(x, T_VECTOR | T_FINALIZABLE | T_DONT_COPY);
  vector_length(x) = len;
  if (len > 0)
    {
      x->object.vector.elements = (s7_pointer *)malloc(len * sizeof(s7_pointer));
      s7_vector_fill(x, sc->NIL);
    }
  else x->object.vector.elements = NULL;
  return(x);
}


int s7_vector_length(s7_pointer vec)
{
  return(vector_length(vec));
}


void s7_vector_fill(s7_pointer vec, s7_pointer obj) 
{
  int i, len;
  s7_pointer *tp;
  len = vector_length(vec);
  tp = (s7_pointer *)(vec->object.vector.elements);
  for(i = 0; i < len; i++) 
    tp[i] = obj;
}


static s7_pointer g_vector_fill(s7_scheme *sc, s7_pointer args)
{
  #define H_vector_fill "(vector-fill! v val) sets all elements of the vector v to val"
  if (!s7_is_vector(car(args)))
    return(s7_wrong_type_arg_error(sc, "vector-fill!", 1, car(args), "a vector"));
  s7_vector_fill(car(args), cadr(args));
  return(sc->UNSPECIFIED);
}


s7_pointer s7_vector_ref(s7_scheme *sc, s7_pointer vec, int index) 
{
  if (index >= vector_length(vec))
    return(s7_out_of_range_error(sc, "vector-ref", 2, s7_make_integer(sc, index), "index is too high"));

  return(vector_element(vec, index));
}


s7_pointer s7_vector_set(s7_scheme *sc, s7_pointer vec, int index, s7_pointer a) 
{
  if (index >= vector_length(vec))
    return(s7_out_of_range_error(sc, "vector-set!", 2, s7_make_integer(sc, index), "index is too high"));

  vector_element(vec, index) = a;
  return(a);
}


s7_pointer s7_vector_to_list(s7_scheme *sc, s7_pointer vect)
{
  s7_pointer lst = sc->NIL;
  int i, len;
  len = vector_length(vect);
  for (i = len - 1; i >= 0; i--)
    lst = s7_cons(sc, vector_element(vect, i), lst);
  return(lst);
}


static s7_pointer g_vector_to_list(s7_scheme *sc, s7_pointer args)
{
  #define H_vector_to_list "(vector->list v) returns the elements of the vector v as a list"
  if (!s7_is_vector(car(args)))
    return(s7_wrong_type_arg_error(sc, "vector->list", 1, car(args), "a vector"));
  return(s7_vector_to_list(sc, car(args)));
}


static bool vectors_equal(s7_pointer x, s7_pointer y)
{
  int i, len;
  len = vector_length(x);
  if (len != vector_length(y)) return(false);
  for (i = 0; i < len; i++)
    if (!(s7_is_equal(vector_element(x, i), vector_element(y, i))))
      return(false);
  return(true);
}


s7_pointer s7_make_and_fill_vector(s7_scheme *sc, int len, s7_pointer fill)
{
  s7_pointer vect;
  vect = s7_make_vector(sc, len);
  if (fill != sc->NIL)
    s7_vector_fill(vect, fill);
  return(vect);
}


static s7_pointer g_vector(s7_scheme *sc, s7_pointer args)
{
  #define H_vector "(vector ...) returns a vector whose elements are the arguments"
  int i, len;
  s7_pointer vec;
  
  len = s7_list_length(sc, args);
  if (len < 0) 
    return(s7_wrong_type_arg_error(sc, "vector", 1, car(args), "a proper list"));
  
  vec = s7_make_vector(sc, len);
  if (len > 0)
    {
      for (sc->x = args, i = 0; is_pair(sc->x); sc->x = cdr(sc->x), i++) 
	vector_element(vec, i) =  car(sc->x);
    }
  return(vec);
}


static s7_pointer g_list_to_vector(s7_scheme *sc, s7_pointer args)
{
  #define H_list_to_vector "(list->vector lst) returns a vector containing the elements of lst"
  
  if (car(args) == sc->NIL)
    return(s7_make_vector(sc, 0));
  if (g_is_list(sc, args) == sc->F)
    return(s7_wrong_type_arg_error(sc, "list->vector", 1, car(args), "a proper list"));
  return(g_vector(sc, car(args)));
}


static s7_pointer g_vector_length(s7_scheme *sc, s7_pointer args)
{
  #define H_vector_length "(vector-length v) returns the length of vector v"
  if (!s7_is_vector(car(args)))
    return(s7_wrong_type_arg_error(sc, "vector-length", 1, car(args), "a vector"));
  return(s7_make_integer(sc, vector_length(car(args))));
}


static s7_pointer g_vector_ref(s7_scheme *sc, s7_pointer args)
{
  #define H_vector_ref "(vector-ref v i) returns the i-th element of vector v"
  s7_pointer vec, index;
  vec = car(args);
  index = cadr(args);
  
  if (!s7_is_vector(vec))
    return(s7_wrong_type_arg_error(sc, "vector-ref", 1, car(args), "a vector"));
  
  if ((!s7_is_integer(index)) || (s7_integer(index) < 0))
    return(s7_wrong_type_arg_error(sc, "vector-ref", 2, index, "a non-negative integer"));
  
  if (s7_integer(index) >= vector_length(vec))
    return(s7_out_of_range_error(sc, "vector-ref", 2, index, "less than vector length"));
  
  return(vector_element(vec, s7_integer(index)));
}


static s7_pointer g_vector_set(s7_scheme *sc, s7_pointer args)
{
  #define H_vector_set "(vector-set! v i value) sets the i-th element of vector v to value"
  s7_pointer vec, index, val;
  
  vec = car(args);
  index = cadr(args);
  val = caddr(args);
  
  if (!s7_is_vector(vec))
    return(s7_wrong_type_arg_error(sc, "vector-set!", 1, car(args), "a vector"));
  
  if ((!s7_is_integer(index)) || (s7_integer(index) < 0))
    return(s7_wrong_type_arg_error(sc, "vector-set!", 2, index, "a non-negative integer"));
  
  if (s7_integer(index) >= vector_length(vec))
    return(s7_out_of_range_error(sc, "vector-set!", 2, index, "less than vector length"));
  
  if (s7_is_immutable(vec))
    return(s7_wrong_type_arg_error(sc, "vector-set!", 1, vec, "a mutable vector"));
  
  vector_element(vec, s7_integer(index)) = val;
  return(val);
}


static s7_pointer g_make_vector(s7_scheme *sc, s7_pointer args)
{
  #define H_make_vector "(make-vector len :optional (value #f)) returns a vector of len elements initialized to value"
  int len;
  s7_pointer fill = sc->UNSPECIFIED, vec;
  
  if ((!s7_is_integer(car(args))) || (s7_integer(car(args)) < 0))
    return(s7_wrong_type_arg_error(sc, "make-vector", 1, car(args), "a non-negative integer"));
  
  len = s7_integer(car(args));
  if (cdr(args) != sc->NIL) 
    fill = cadr(args);
  
  vec = s7_make_vector(sc, len);
  if (fill != sc->NIL)
    s7_vector_fill(vec, fill);
  
  return(vec);
}


static s7_pointer g_is_vector(s7_scheme *sc, s7_pointer args)
{
  #define H_is_vector "(vector? obj) returns #t if obj is a vector"
  return(make_boolean(sc, s7_is_vector(car(args))));
}



/* -------------------------------- objects and functions --------------------------------
 *
 * this could be made available in Scheme:
 *
 * define-type (make-type in ext)
 * (define-type "name"
 *              (lambda (obj) (display obj))       ; print
 *	        #f                                 ; free
 *	        (lambda (obj1 obj2) (= obj1 obj2)) ; equal?
 *	        #f                                 ; gc mark
 *	        (lambda (obj arg) arg)             ; apply
 *	        #f)                                ; set
 *  (make-object <tag> value)
 *  (object? <tag> obj) or maybe (name? obj) and (make-name value)
 */

bool s7_is_function(s7_pointer p)  
{ 
  return(type(p) == T_S7_FUNCTION);
}


bool s7_is_object(s7_pointer p) 
{ 
  return(type(p) == T_S7_OBJECT);
}


s7_pointer s7_make_function(s7_scheme *sc, const char *name, s7_function f, int required_args, int optional_args, bool rest_arg, const char *doc)
{
  ffunc *ptr;
  s7_pointer x = new_cell(sc);
  ptr = (ffunc *)calloc(1, sizeof(ffunc));
  set_type(x, T_S7_FUNCTION | T_ATOM | T_SIMPLE | T_FINALIZABLE | T_DONT_COPY | T_PROCEDURE);
  /* was T_CONSTANT, but these guys can be freed -- in Snd, for example, "random" is defined in C, but then later redefined in snd-test.scm */
  x->object.ffptr = ptr;
  x->object.ffptr->ff = f;
  x->object.ffptr->name = name;
  if (doc)
    x->object.ffptr->doc = strdup(doc);
  x->object.ffptr->required_args = required_args;
  x->object.ffptr->optional_args = optional_args;
  x->object.ffptr->rest_arg = rest_arg;
  return(x);
}


static void s7_free_function(s7_pointer a)
{
  if (a->object.ffptr->doc)
    free(a->object.ffptr->doc);
  free(a->object.ffptr);
}


bool s7_is_procedure(s7_pointer x)
{
  return(is_procedure(x));
}


static s7_pointer g_is_procedure(s7_scheme *sc, s7_pointer args)
{
  #define H_is_procedure "(procedure? obj) returns #t if obj is a procedure"
  return(make_boolean(sc, s7_is_procedure(car(args))));
}


static char *pws_documentation(s7_pointer x);
static s7_pointer pws_source(s7_scheme *sc, s7_pointer x);
static int pws_get_req_args(s7_pointer x);
static int pws_get_opt_args(s7_pointer x);


s7_pointer s7_procedure_source(s7_scheme *sc, s7_pointer p)
{
  /* make it look like an internal lambda form */
  
  /* in this context, there's no way to distinguish between:
   *    (procedure-source (let ((b 1)) (lambda (a) (+ a b))))
   * and
   *    (let ((b 1)) (procedure-source (lambda (a) (+ a b))))
   * both become:
   * ((a) (+ a b)) (((b . 1)) #(() () () () () ((make-filtered-comb . make-filtered-comb)) () () ...))
   */
  
  if (is_closure(p) || is_closure_star(p) || is_macro(p) || is_promise(p)) 
    {
      return(s7_cons(sc, 
		     s7_append(sc, 
			       make_list_2(sc, 
					   (is_closure_star(p)) ? sc->LAMBDA_STAR : sc->LAMBDA, 
					   car(car(p))),
			       cdr(car(p))),
		     cdr(p)));
    }
  
  if (s7_is_procedure_with_setter(p))
    return(pws_source(sc, p));
  return(sc->NIL);
}


static s7_pointer g_procedure_source(s7_scheme *sc, s7_pointer args)
{
  /* make it look like a scheme-level lambda */
  s7_pointer p;
  
  #define H_procedure_source "(procedure-source func) tries to return the definition of func"
  
  if (s7_is_symbol(car(args)))
    p = s7_symbol_value(sc, car(args));
  else p = car(args);

  if (is_closure(p) || is_closure_star(p) || is_macro(p) || is_promise(p)) 
    return(s7_append(sc, 
		     make_list_2(sc, 
				 (is_closure_star(p)) ? sc->LAMBDA_STAR : sc->LAMBDA, 
				 car(car(p))),
		     cdr(car(p))));

  if (s7_is_procedure_with_setter(p))
    return(pws_source(sc, p));
  return(sc->NIL);
}


s7_pointer s7_procedure_environment(s7_pointer p)    
{ 
  return(cdr(p));
}


void s7_define_function(s7_scheme *sc, const char *name, s7_function fnc, int required_args, int optional_args, bool rest_arg, const char *doc)
{
  s7_pointer func, sym;
  
  func = s7_make_function(sc, name, fnc, required_args, optional_args, rest_arg, doc);
  sym = s7_make_symbol(sc, name);
  s7_define(sc, s7_global_environment(sc), sym, func);
}


const char *s7_procedure_documentation(s7_scheme *sc, s7_pointer p)
{
  s7_pointer x;
  
  if (s7_is_symbol(p))
    x = s7_symbol_value(sc, p);
  else x = p;
  
  if (s7_is_function(x))
    return((char *)function_documentation(x));
  
  if (((is_closure(x)) || (is_closure_star(p))) &&
      (s7_is_string(cadar(x))))
    return(s7_string(cadar(x)));
  
  if (s7_is_procedure_with_setter(x))
    return(pws_documentation(x));
  
  return(NULL);
}


static s7_pointer g_procedure_documentation(s7_scheme *sc, s7_pointer args)
{
  #define H_procedure_documentation "(procedure-documentation func) returns func's documentation string"
  return(s7_make_string(sc, s7_procedure_documentation(sc, car(args))));
}


s7_pointer s7_procedure_arity(s7_scheme *sc, s7_pointer x)
{
  if (s7_is_symbol(x))
    x = s7_symbol_value(sc, x);
  
  if (s7_is_function(x))
    return(make_list_3(sc, 
		       s7_make_integer(sc, x->object.ffptr->required_args), 
		       s7_make_integer(sc, x->object.ffptr->optional_args),
		       (x->object.ffptr->rest_arg) ? sc->T : sc->F));
  
  if ((is_closure(x)) ||
      (is_closure_star(x)) ||
      (is_pair(x)))
    {
      int len;
      
      if (is_pair(x))
	len = s7_list_length(sc, car(x));
      else 
	{
	  if (s7_is_symbol(caar(x)))
	    return(make_list_3(sc, s7_make_integer(sc, 0), s7_make_integer(sc, 0), sc->T));
	  len = s7_list_length(sc, caar(x));
	}
      
      if (len >= 0)
	return(make_list_3(sc, s7_make_integer(sc, len), s7_make_integer(sc, 0), sc->F));
      return(make_list_3(sc, s7_make_integer(sc, abs(len)), s7_make_integer(sc, 0), sc->T));
    }
  
  if (s7_is_procedure_with_setter(x))
    return(make_list_3(sc, 
		       s7_make_integer(sc, pws_get_req_args(x)),
		       s7_make_integer(sc, pws_get_opt_args(x)),
		       sc->F));
  
  if (s7_is_applicable_object(x))
    return(make_list_3(sc, s7_make_integer(sc, 0), s7_make_integer(sc, 0), sc->T));
  return(sc->NIL);
}


static s7_pointer g_procedure_arity(s7_scheme *sc, s7_pointer args)
{
  #define H_procedure_arity "(procedure-arity func) returns a list '(required optional rest)"
  return(s7_procedure_arity(sc, car(args)));
}


typedef struct {
  int type;
  const char *name;
  char *(*print)(s7_scheme *sc, void *value);
  void (*free)(void *value);
  bool (*equal)(void *val1, void *val2);
  void (*gc_mark)(void *val);
  s7_pointer (*apply)(s7_scheme *sc, s7_pointer obj, s7_pointer args);
  s7_pointer (*set)(s7_scheme *sc, s7_pointer obj, s7_pointer args);
} fobject;


static fobject *object_types = NULL;
static int object_types_size = 0;
static int num_types = 0;


int s7_new_type(const char *name, 
		char *(*print)(s7_scheme *sc, void *value), 
		void (*free)(void *value), 
		bool (*equal)(void *val1, void *val2),
		void (*gc_mark)(void *val),
                s7_pointer (*apply)(s7_scheme *sc, s7_pointer obj, s7_pointer args),
                s7_pointer (*set)(s7_scheme *sc, s7_pointer obj, s7_pointer args))
{
  int tag;
  tag = num_types++;
  if (tag >= object_types_size)
    {
      if (object_types_size == 0)
	{
	  object_types_size = 8;
	  object_types = (fobject *)calloc(object_types_size, sizeof(fobject));
	}
      else
	{
	  object_types_size = tag + 8;
	  object_types = (fobject *)realloc((void *)object_types, object_types_size * sizeof(fobject));
	}
    }
  object_types[tag].type = tag;
  object_types[tag].name = strdup(name);
  object_types[tag].free = free;
  object_types[tag].print = print;
  object_types[tag].equal = equal;
  object_types[tag].gc_mark = gc_mark;
  object_types[tag].apply = apply;
  object_types[tag].set = set;
  return(tag);
}


static char *s7_describe_object(s7_scheme *sc, s7_pointer a)
{
  int tag;
  tag = a->object.fobj.type;
  if (object_types[tag].print)
    return((*(object_types[tag].print))(sc, a->object.fobj.value)); /* assume allocation here (so we'll free the string later) */
  return(strdup(object_types[tag].name));
}


static void s7_free_object(s7_pointer a)
{
  int tag;
  tag = a->object.fobj.type;
  if (object_types[tag].free)
    (*(object_types[tag].free))(a->object.fobj.value);
}


static bool s7_equalp_objects(s7_pointer a, s7_pointer b)
{
  if ((s7_is_object(a)) &&
      (s7_is_object(b)) &&
      (a->object.fobj.type == b->object.fobj.type))
    {
      int tag;
      tag = a->object.fobj.type;
      if (object_types[tag].equal)
	return((*(object_types[tag].equal))(a->object.fobj.value, b->object.fobj.value));
      return(a == b);
    }
  return(false);
}


static void s7_mark_embedded_objects(s7_pointer a) /* called by gc, calls fobj's mark func */
{
  int tag;
  tag = a->object.fobj.type;
  if (tag < num_types)
    {
      if (object_types[tag].gc_mark)
	(*(object_types[tag].gc_mark))(a->object.fobj.value);
    }
  else fprintf(stderr, "%p, found bad type: %x %d %x\n", a, tag, tag, a->flag); /* TODO: real error here */
}


static bool s7_is_applicable_object(s7_pointer x)
{
  return((s7_is_object(x)) &&
	 (object_types[x->object.fobj.type].apply));
}


static s7_pointer s7_apply_object(s7_scheme *sc, s7_pointer obj, s7_pointer args)
{
  int tag;
  tag = obj->object.fobj.type;
  if (object_types[tag].apply)
    return((*(object_types[tag].apply))(sc, obj, args));
  return(sc->F);
}


#define object_set_function(Obj) object_types[(Obj)->object.fobj.type].set

static s7_pointer s7_set_object(s7_scheme *sc, s7_pointer obj, s7_pointer args)
{
  int tag;
  tag = obj->object.fobj.type;
  if (object_types[tag].set)
    return((*(object_types[tag].set))(sc, obj, args));
  return(sc->UNSPECIFIED);
}


/* generalized set! calls g_set_object which then calls the object's set function */

static s7_pointer g_set_object(s7_scheme *sc, s7_pointer args)
{
  return(s7_set_object(sc, car(args), cdr(args)));
}


void *s7_object_value(s7_pointer obj)
{
  return(obj->object.fobj.value);
}


int s7_object_type(s7_pointer obj)
{
  return(obj->object.fobj.type);
}


s7_pointer s7_make_object(s7_scheme *sc, int type, void *value)
{
  s7_pointer x;
  x = new_cell(sc);
  set_type(x, T_S7_OBJECT | T_ATOM | T_FINALIZABLE | T_DONT_COPY);
  x->object.fobj.type = type;
  x->object.fobj.value = value;
  if (object_types[type].apply)
    typeflag(x) |= T_PROCEDURE;
  return(x);
}



/* -------- procedure-with-setter -------- */

static int pws_tag;

typedef struct {
  s7_pointer (*getter)(s7_scheme *sc, s7_pointer args);
  int get_req_args, get_opt_args;
  s7_pointer (*setter)(s7_scheme *sc, s7_pointer args);
  int set_req_args, set_opt_args;
  s7_pointer scheme_getter;
  s7_pointer scheme_setter;
  char *documentation;
  char *name;
} pws;


s7_pointer s7_make_procedure_with_setter(s7_scheme *sc, 
					 const char *name,
					 s7_pointer (*getter)(s7_scheme *sc, s7_pointer args), 
					 int get_req_args, int get_opt_args,
					 s7_pointer (*setter)(s7_scheme *sc, s7_pointer args),
					 int set_req_args, int set_opt_args,
					 const char *documentation)
{
  pws *f;
  s7_pointer obj;
  f = (pws *)calloc(1, sizeof(pws));
  f->getter = getter;
  f->get_req_args = get_req_args;
  f->get_opt_args = get_opt_args;
  f->setter = setter;
  f->set_req_args = set_req_args;
  f->set_opt_args = set_opt_args;
  if (documentation)
    f->documentation = strdup(documentation);
  else f->documentation = NULL;
  if (name)
    f->name = strdup(name);
  else f->name = NULL;
  f->scheme_getter = sc->NIL;
  f->scheme_setter = sc->NIL;
  obj = s7_make_object(sc, pws_tag, (void *)f);
  typeflag(obj) |= T_PROCEDURE;
  return(obj);
}


static char *pws_print(s7_scheme *sc, void *obj)
{
  pws *f = (pws *)obj;
  if (f->name)
    return(strdup(f->name));
  return(strdup((char *)"#<procedure-with-setter>"));
}


static void pws_free(void *obj)
{
  pws *f = (pws *)obj;
  if (f)
    {
      if (f->documentation)
	free(f->documentation);
      if (f->name)
	free(f->name);
      free(f);
    }
}


static void pws_mark(void *val)
{
  pws *f = (pws *)val;
  S7_MARK(f->scheme_getter);
  S7_MARK(f->scheme_setter);
}


static bool pws_equal(void *obj1, void *obj2)
{
  return(obj1 == obj2);
}


/* this is called as the pws object apply method, not as the actual getter */
static s7_pointer pws_apply(s7_scheme *sc, s7_pointer obj, s7_pointer args)
{
  pws *f;
  f = (pws *)s7_object_value(obj);
  if (f->getter != NULL)
    return((*(f->getter))(sc, args));
  return(s7_call(sc, f->scheme_getter, args));
}


/* this is the pws set method, not the actual setter */
static s7_pointer pws_set(s7_scheme *sc, s7_pointer obj, s7_pointer args)
{
  pws *f;
  f = (pws *)s7_object_value(obj);
  if (f->setter != NULL)
    return((*(f->setter))(sc, args));
  return(s7_call(sc, f->scheme_setter, args));
}


static s7_pointer g_make_procedure_with_setter(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p;
  pws *f;
  p = s7_make_procedure_with_setter(sc, NULL, NULL, -1, 0, NULL, -1, 0, NULL);
  
  f = (pws *)s7_object_value(p);
  f->scheme_getter = car(args);
  if ((is_closure(car(args))) ||
      (is_closure_star(car(args))))
    f->get_req_args = s7_list_length(sc, caaar(args));
  else f->get_req_args = s7_list_length(sc, caar(args));
  
  f->scheme_setter = cadr(args);
  if ((is_closure(cadr(args))) ||
      (is_closure_star(cadr(args))))
    f->set_req_args = s7_list_length(sc, caaadr(args));
  else f->set_req_args = s7_list_length(sc, caadr(args));
  
  return(p);
}


bool s7_is_procedure_with_setter(s7_pointer obj)
{
  return((s7_is_object(obj)) &&
	 (s7_object_type(obj) == pws_tag));
}


s7_pointer s7_procedure_with_setter_getter(s7_pointer obj)
{
  pws *f;
  f = (pws *)s7_object_value(obj);
  return(f->scheme_getter);
}


s7_pointer s7_procedure_with_setter_setter(s7_pointer obj)
{
  pws *f;
  f = (pws *)s7_object_value(obj);
  return(f->scheme_setter);
}


static s7_pointer g_is_procedure_with_setter(s7_scheme *sc, s7_pointer args)
{
  #define H_is_procedure_with_setter "(procedure-with-setter? obj) returns #t if obj is a procedure-with-setter"
  return(make_boolean(sc, s7_is_procedure_with_setter(car(args))));
}


static char *pws_documentation(s7_pointer x)
{
  pws *f = (pws *)s7_object_value(x);
  return(f->documentation);
}


static char *pws_name(s7_pointer x)
{
  pws *f = (pws *)s7_object_value(x);
  return(f->name);
}


static int pws_get_req_args(s7_pointer x)
{
  pws *f = (pws *)s7_object_value(x);
  return(f->get_req_args);
}


static int pws_get_opt_args(s7_pointer x)
{
  pws *f = (pws *)s7_object_value(x);
  return(f->get_opt_args);
}


static s7_pointer g_procedure_with_setter_setter_arity(s7_scheme *sc, s7_pointer args)
{
  pws *f = (pws *)s7_object_value(car(args));
  return(make_list_3(sc,
		     s7_make_integer(sc, f->set_req_args),
		     s7_make_integer(sc, f->set_opt_args),
		     sc->F));
}


static s7_pointer pws_source(s7_scheme *sc, s7_pointer x)
{
  pws *f = (pws *)s7_object_value(x);
  if ((is_closure(f->scheme_getter)) ||
      (is_closure_star(f->scheme_getter)))
    return(s7_append(sc, 
		     make_list_2(sc,
				 (is_closure(f->scheme_getter)) ? sc->LAMBDA : sc->LAMBDA_STAR,
				 car(car(f->scheme_getter))),
		     cdr(car(f->scheme_getter))));
  return(sc->NIL);
}



/* -------------------------------- eq etc -------------------------------- */

bool s7_is_eq(s7_pointer obj1, s7_pointer obj2)
{
  return(obj1 == obj2);
}


bool s7_is_eqv(s7_pointer a, s7_pointer b) 
{
  if (a == b) 
    return(true);
  
  if (type(a) != type(b)) 
    return(false);
  
  if (s7_is_string(a)) 
    return(string_value(a) == string_value(b));
  
  if (s7_is_number(a))
    return(numbers_are_eqv(a, b));
  
  if (s7_is_character(a))
    return(s7_character(a) == s7_character(b));
  
  return(false);
}


bool s7_is_equal(s7_pointer x, s7_pointer y)
{
  if (x == y) 
    return(true);
  
  if (type(x) != type(y)) 
    return(false);
  
  if (is_pair(x))
    return((s7_is_equal(car(x), car(y))) &&
	   (s7_is_equal(cdr(x), cdr(y))));
  
  if (s7_is_string(x))
    return((string_length(x) == string_length(y)) &&
	   ((string_length(x) == 0) ||
	    (strcmp(string_value(x), string_value(y)) == 0)));
  
  if (s7_is_object(x))
    return(s7_equalp_objects(x, y));
  
  if ((s7_is_vector(x)) ||
      (s7_is_hash_table(x)))
    return(vectors_equal(x, y));
  
  if (s7_is_character(x)) 
    return(s7_character(x) == s7_character(y));
  
  if (s7_is_number(x))
    return(numbers_are_eqv(x, y));
  
  return(false); /* we already checked that x != y (port etc) */
}


static s7_pointer g_is_eq(s7_scheme *sc, s7_pointer args)
{
  #define H_is_eq "(eq? obj1 obj2) returns #t if obj1 is eq to (the same object as) obj2"
  return(make_boolean(sc, car(args) == cadr(args)));
}


static s7_pointer g_is_eqv(s7_scheme *sc, s7_pointer args)
{
  #define H_is_eqv "(eqv? obj1 obj2) returns #t if obj1 is equivalent to obj2"
  return(make_boolean(sc, s7_is_eqv(car(args), cadr(args))));
}


static s7_pointer g_is_equal(s7_scheme *sc, s7_pointer args)
{
  #define H_is_equal "(equal? obj1 obj2) returns #t if obj1 is equal to obj2"
  return(make_boolean(sc, s7_is_equal(car(args), cadr(args))));
}




/* -------- keywords -------- */

bool s7_keyword_eq_p(s7_pointer obj1, s7_pointer obj2)
{
  return(obj1 == obj2);
}


bool s7_is_keyword(s7_pointer obj)
{
  return((s7_is_symbol(obj)) &&
	 (s7_symbol_name(obj)[0] == ':'));
}


static s7_pointer g_is_keyword(s7_scheme *sc, s7_pointer args)
{
  #define H_is_keyword "(keyword? obj) returns #t if obj is a keyword"
  return(make_boolean(sc, s7_is_keyword(car(args))));
}


s7_pointer s7_make_keyword(s7_scheme *sc, const char *key)
{
  s7_pointer sym, x;
  char *name;
  
  name = (char *)malloc((safe_strlen(key) + 2) * sizeof(char));
  sprintf(name, ":%s", key);                     /* prepend ":" */
  sym = s7_make_symbol(sc, name);
  typeflag(sym) |= (T_IMMUTABLE | T_CONSTANT | T_DONT_COPY); 
  free(name);
  
#if 0
  x = s7_find_symbol_in_environment(sc, sc->global_env, sym, false); /* is it already defined? */
  if (x == sc->NIL) 
    add_to_environment(sc, sc->global_env, sym, sym); /* its value is itself */
#endif
  x = s7_find_symbol_in_environment(sc, sc->envir, sym, true); /* is it already defined? */
  if (x == sc->NIL) 
    add_to_current_environment(sc, sym, sym); /* its value is itself */
  
  return(sym);
}


static s7_pointer g_make_keyword(s7_scheme *sc, s7_pointer args)
{
  #define H_make_keyword "(make-keyword str) prepends ':' to str and defines that as a keyword"
  if (!s7_is_string(car(args)))
    return(s7_wrong_type_arg_error(sc, "make-keyword", 1, car(args), "a string"));
  return(s7_make_keyword(sc, string_value(car(args))));
}


static s7_pointer g_keyword_to_symbol(s7_scheme *sc, s7_pointer args)
{
  #define H_keyword_to_symbol "(keyword->symbol key) returns a symbol with the same name as key but no prepended colon"
  const char *name;
  if (!s7_is_keyword(car(args)))
    return(s7_wrong_type_arg_error(sc, "keyword->symbol", 1, car(args), "a keyword"));
  name = s7_symbol_name(car(args));
  return(s7_make_symbol(sc, (const char *)(name + 1)));
}


static s7_pointer g_symbol_to_keyword(s7_scheme *sc, s7_pointer args)
{
  #define H_symbol_to_keyword "(symbol->keyword sym) returns a keyword with the same name as sym, but with a colon prepended"
  if (!s7_is_symbol(car(args)))
    return(s7_wrong_type_arg_error(sc, "symbol->keyword", 1, car(args), "a symbol"));
  return(s7_make_keyword(sc, s7_symbol_name(car(args))));
}




/* -------- hash tables -------- */

bool s7_is_hash_table(s7_pointer p)
{
  return(type(p) == T_HASH_TABLE);
}


static s7_pointer g_is_hash_table(s7_scheme *sc, s7_pointer args)
{
  #define H_is_hash_table "(hash-table? obj) returns #t if obj is a hash-table"
  return(make_boolean(sc, s7_is_hash_table(car(args))));
}


static s7_pointer g_hash_table_size(s7_scheme *sc, s7_pointer args)
{
  #define H_hash_table_size "(hash-table-size obj) returnsthe size of the hash-table obj"
  if (!s7_is_hash_table(car(args)))
    return(s7_wrong_type_arg_error(sc, "hash-table-size", 1, car(args), "a hash-table"));
  return(s7_make_integer(sc, vector_length(car(args))));
}


s7_pointer s7_make_hash_table(s7_scheme *sc, int size)
{
  s7_pointer table;
  table = s7_make_vector(sc, size);
  set_type(table, T_HASH_TABLE | T_FINALIZABLE | T_DONT_COPY);
  return(table);
}


static s7_pointer g_make_hash_table(s7_scheme *sc, s7_pointer args)
{
  #define H_make_hash_table "(make-hash-table :optional size) returns a new hash table"
  int size = 461;

  if (args != sc->NIL)
    {
      if (s7_is_integer(car(args)))
	{
	  size = s7_integer(car(args));
	  if (size <= 0)
	    return(s7_out_of_range_error(sc, "make-hash-table", 1, car(args), "a positive integer"));
	}
      else return(s7_wrong_type_arg_error(sc, "make-hash-table", 1, car(args), "an integer"));
    }
  return(s7_make_hash_table(sc, size));
}


s7_pointer s7_hash_table_ref(s7_scheme *sc, s7_pointer table, const char *name)
{
  int location;
  s7_pointer x;
  
  location = hash_fn(name, vector_length(table));
  for (x = vector_element(table, location); x != sc->NIL; x = cdr(x)) 
    if (STRCMP(name, string_value(caar(x))) == 0) 
      return(cdar(x)); 
  
  return(sc->F);
}


s7_pointer s7_hash_table_set(s7_scheme *sc, s7_pointer table, const char *name, s7_pointer value)
{
  int location;
  s7_pointer x;
  location = hash_fn(name, vector_length(table)); 
  
  /* if it exists, update value, else add to table */
  for (x = vector_element(table, location); x != sc->NIL; x = cdr(x)) 
    if (STRCMP(name, string_value(caar(x))) == 0)
      {
	cdar(x) = value;
	return(value);
      }
  vector_element(table, location) = s7_cons(sc, 
					    s7_cons(sc, 
						    s7_make_string(sc, name), 
						    value),
					    vector_element(table, location)); 
  return(value);
}


#define HASHED_INTEGER_BUFFER_SIZE 32

static char *s7_hashed_integer_name(s7_Int key, char *intbuf)
{
  snprintf(intbuf, HASHED_INTEGER_BUFFER_SIZE, "\b%lld\b", (long long int)key);
  return(intbuf);
}


static char *s7_hashed_real_name(s7_Double key, char *intbuf)
{
  /* this is actually not safe due to the challenges faced by %f */
  snprintf(intbuf, HASHED_INTEGER_BUFFER_SIZE, "\b%f\b", key);
  return(intbuf);
}


static s7_pointer g_hash_table_ref(s7_scheme *sc, s7_pointer args)
{
  /* basically the same layout as the global symbol table */
  #define H_hash_table_ref "(hash-table-ref table key) returns the value associated with key (a string or symbol) in the hash table"
  const char *name;
  char intbuf[HASHED_INTEGER_BUFFER_SIZE];
  s7_pointer table, key;
  table = car(args);
  key = cadr(args);
  
  if (!s7_is_hash_table(table))
    return(s7_wrong_type_arg_error(sc, "hash-table-ref", 1, table, "a hash-table"));

  if (s7_is_string(key))
    name = string_value(key);
  else 
    {
      if (s7_is_symbol(key))
	name = s7_symbol_name(key);
      else
	{
	  if (s7_is_integer(key))
	    name = s7_hashed_integer_name(s7_integer(key), intbuf);
	  else
	    {
	      if ((s7_is_real(key)) && (!s7_is_ratio(key)))
		name = s7_hashed_real_name(s7_real(key), intbuf);
	      else return(s7_wrong_type_arg_error(sc, "hash-table-ref", 2, key, "a string, symbol, integer, or (non-ratio) real"));
	    }
	}
    }
  return(s7_hash_table_ref(sc, table, name));
}


static s7_pointer g_hash_table_set(s7_scheme *sc, s7_pointer args)
{
  #define H_hash_table_set "(hash-table-set! table key value) sets the value associated with key (a string or symbol) in the hash table to value"
  const char *name;
  char intbuf[HASHED_INTEGER_BUFFER_SIZE];
  s7_pointer table, key;
  table = car(args);
  key = cadr(args);
  
  if (!s7_is_hash_table(table))
    return(s7_wrong_type_arg_error(sc, "hash-table-set!", 1, table, "a hash-table"));

  if (s7_is_string(key))
    name = string_value(key);
  else 
    {
      if (s7_is_symbol(key))
	name = s7_symbol_name(key);
      else
	{
	  if (s7_is_integer(key))
	    name = s7_hashed_integer_name(s7_integer(key), intbuf);
	  else
	    {
	      if ((s7_is_real(key)) && (!s7_is_ratio(key)))
		name = s7_hashed_real_name(s7_real(key), intbuf);
	      else return(s7_wrong_type_arg_error(sc, "hash-table-set!", 2, key, "a string, symbol, integer, or (non-ratio) real"));
	    }
	}
    }
  
  return(s7_hash_table_set(sc, table, name, caddr(args)));
}




/* -------------------------------- format -------------------------------- */

#if WITH_FORMAT

typedef struct {
  char *str;
  int len, loc;
  s7_pointer args;
} format_data;


static char *s7_format_error(s7_scheme *sc, const char *msg, const char *str, s7_pointer args, format_data *dat)
{
  int len, slen = 0;
  char *errmsg;

  len = safe_strlen(msg) + (2 * safe_strlen(str)) + 32;
  errmsg = (char *)malloc(len * sizeof(char));
  if (dat->loc == 0)
    slen = snprintf(errmsg, len, "format ~S ~{~A~^ ~}: %s", msg);
  else 
    {
      char *filler;
      int i;
      filler = (char *)calloc(dat->loc + 12, sizeof(char));
      for (i = 0; i < dat->loc + 11; i++)
	filler[i] = ' ';
      slen = snprintf(errmsg, len, "\nformat: ~S ~{~A~^ ~}\n%s^: %s", filler, msg);
      free(filler);
    }

  sc->x = make_list_3(sc, s7_make_string_with_length(sc, errmsg, slen), s7_make_string(sc, str), args);

  free(errmsg);
  if (dat->str) free(dat->str);
  free(dat);

  s7_error(sc, sc->FORMAT_ERROR, sc->x);
  return(NULL);
}


static void format_append_char(format_data *dat, char c)
{
  if (dat->len <= dat->loc + 2)
    {
      dat->len *= 2;
      dat->str = (char *)realloc(dat->str, dat->len);
    }
  dat->str[dat->loc++] = c;
}


static void format_append_string(format_data *dat, char *str)
{
  int i, len;
  len = safe_strlen(str);
  for (i = 0; i < len; i++)
    format_append_char(dat, str[i]);
}


static int format_read_integer(s7_scheme *sc, int *cur_i, int str_len, const char *str, s7_pointer args, format_data *fdat)
{
  int i, arg1 = -1;
  char *tmp;
  i = *cur_i;
  if (isdigit(str[i]))
    {
      tmp = (char *)(str + i);
      if (sscanf(tmp, "%d", &arg1) < 1)
	s7_format_error(sc, "bad number?", str, args, fdat);

      for (i = i + 1; i < str_len - 1; i++)
	if (!isdigit(str[i]))
	  break;
      if (i >= str_len)
	s7_format_error(sc, "numeric argument, but no directive!", str, args, fdat);
    }
  *cur_i = i;
  return(arg1);
}


static void format_number(s7_scheme *sc, format_data *fdat, int radix, int width, int precision, char float_choice)
{
  char *tmp;
  if (width < 0) width = 0;
  tmp = s7_number_to_string_1(sc, car(fdat->args), radix, width, precision, float_choice);
  format_append_string(fdat, tmp);
  free(tmp);
  fdat->args = cdr(fdat->args);
}


static char *format_to_c_string(s7_scheme *sc, const char* str, s7_pointer args, s7_pointer *next_arg)
{
  #define INITIAL_FORMAT_LENGTH 128
  int i = 0, str_len = 0;
  format_data *fdat = NULL;
  char *result, *tmp = NULL;

  str_len = strlen(str);

  fdat = (format_data *)malloc(sizeof(format_data));
  fdat->loc = 0;
  fdat->len = INITIAL_FORMAT_LENGTH;
  fdat->str = (char *)calloc(fdat->len, sizeof(char)); /* ~nT col checks need true current string length, so malloc here is messy */
  fdat->str[0] = '\0';
  fdat->args = args;

  if (str_len == 0)
    {
      if ((args != sc->NIL) &&
	  (next_arg == NULL))
	return(s7_format_error(sc, "too many arguments", str, args, fdat));
    }
  else
    {
      for (i = 0; i < str_len - 1; i++)
	{
	  if (str[i] == '~')
	    {
	      switch (str[i + 1])
		{
		  /* -------- newline -------- */
		case '%': 
		  format_append_char(fdat, '\n');
		  i++;
		  break;

		  /* -------- tilde -------- */
		case '~':
		  format_append_char(fdat, '~');
		  i++;
		  break;

		  /* -------- trim white-space -------- */
		case '\n':
		  for (i = i + 2; i <str_len - 1; i++)
		    if (!(isspace(str[i])))
		      {
			i--;
			break;
		      }
		  break;

		  /* -------- ignore arg -------- */
		case '*':
		  i++;
		  fdat->args = cdr(fdat->args);
		  break;

		  /* -------- exit -------- */
		case '^':
		  if (fdat->args == sc->NIL)
		    {
		      i = str_len;
		      goto ALL_DONE;
		    }
		  i++;
		  break;

		  /* -------- object->string -------- */
		case 'A': case 'a': 
		case 'C': case 'c':
		case 'S': case 's':
		  if (fdat->args == sc->NIL)
		    return(s7_format_error(sc, "missing argument", str, args, fdat));
		  i++;
		  if (((str[i] == 'C') || (str[i] == 'c')) &&
		      (!s7_is_character(car(fdat->args))))
		    return(s7_format_error(sc, "~C directive requires a character argument", str, args, fdat));

		  tmp = s7_object_to_c_string_1(sc, car(fdat->args), (str[i] == 'S') || (str[i] == 's'));
		  format_append_string(fdat, tmp);
		  if (tmp) free(tmp);
		  fdat->args = cdr(fdat->args);
		  break;

		  /* -------- iteration -------- */
		case '{':
		  {
		    s7_pointer curly_arg;
		    char *curly_str = NULL;
		    int k, curly_len = -1, curly_nesting = 1;
		    if (!s7_is_list(sc, car(fdat->args)))
		      return(s7_format_error(sc, "'{' directive argument should be a list", str, args, fdat));

		    for (k = i + 2; k < str_len - 1; k++)
		      if (str[k] == '~')
			{
			  if (str[k + 1] == '}')
			    {
			      curly_nesting--;
			      if (curly_nesting == 0)
				{
				  curly_len = k - i - 1;
				  break;
				}
			    }
			  else
			    {
			      if (str[k + 1] == '{')
				curly_nesting++;
			    }
			}
		    if (curly_len == -1)
		      return(s7_format_error(sc, "'{' directive, but no matching '}'", str, args, fdat));

		    if (curly_len <= 1)
		      return(s7_format_error(sc, "~{...~} doesn't consume any arguments!", str, args, fdat));

		    curly_str = (char *)malloc(curly_len);
		    for (k = 0; k < curly_len - 1; k++)
		      curly_str[k] = str[i + 2 + k];
		    curly_str[curly_len - 1] = '\0';

		    curly_arg = car(fdat->args); 
		    while (curly_arg != sc->NIL)
		      {
			s7_pointer new_arg = sc->NIL;
			tmp = format_to_c_string(sc, curly_str, curly_arg, &new_arg);
			format_append_string(fdat, tmp);
			if (tmp) free(tmp);
			if (curly_arg == new_arg)
			  {
			    if (curly_str) free(curly_str);
			    return(s7_format_error(sc, "~{...~} doesn't consume any arguments!", str, args, fdat));
			  }
			curly_arg = new_arg;
		      }

		    i += (curly_len + 2); /* jump past the ending '}' too */
		    fdat->args = cdr(fdat->args);
		    free(curly_str);
		  }
		  break;
		  
		case '}':
		  return(s7_format_error(sc, "unmatched '}'", str, args, fdat));
		  
		  /* -------- numeric args -------- */
		case '0': case '1': case '2': case '3': case '4': case '5':
		case '6': case '7': case '8': case '9': case ',':
		case 'B': case 'b':
		case 'D': case 'd':
		case 'E': case 'e':
		case 'F': case 'f':
		case 'G': case 'g':
		case 'O': case 'o':
		case 'T': case 't':
		case 'X': case 'x':
		  {
		    int width = -1, precision = -1;
		    i++;
		    if (isdigit(str[i]))
		      width = format_read_integer(sc, &i, str_len, str, args, fdat);
		    if (str[i] == ',')
		      {
			i++;
			if (isdigit(str[i]))
			  precision = format_read_integer(sc, &i, str_len, str, args, fdat);
		      }
		    
		    if ((str[i] != 'T') && (str[i] != 't'))
		      {
			if (fdat->args == sc->NIL)
			  return(s7_format_error(sc, "missing argument", str, args, fdat));
			if (!(s7_is_number(car(fdat->args))))
			  return(s7_format_error(sc, "numeric argument required", str, args, fdat));
		      }

		    switch (str[i])
		      {
			/* -------- pad to column -------- */
			/*   are columns numbered from 1 or 0?  there seems to be disagreement about this directive */
			/*   does "space over to" mean including? */
		      case 'T': case 't':
			if (width == -1) width = 0;
			if (precision == -1) precision = 0;
			if ((width > 0) || (precision > 0))         /* (format #f "a~8Tb") */
			  {
			    int j, k, outstr_len;
			    outstr_len = safe_strlen(fdat->str);
			    for (k = outstr_len - 1; k > 0; k--)
			      if (fdat->str[k] == '\n')
				break;
			    
			    if (precision > 0)
			      {
				int mult;
				mult = (int)(ceil((s7_Double)(outstr_len - k - width) / (s7_Double)precision)); /* CLtL2 ("least positive int") */
				if (mult < 1) mult = 1;
				width += (precision * mult);
			      }

			    for (j = outstr_len - k; j < width; j++)
			      format_append_char(fdat, ' ');
			  }
			break;

			/* -------- numbers -------- */
		      case 'F': case 'f':
			format_number(sc, fdat, 10, width, (precision < 0) ? 6 : precision, 'f');
			break;

		      case 'G': case 'g':
			format_number(sc, fdat, 10, width, (precision < 0) ? 6 : precision, 'g');
			break;

		      case 'E': case 'e':
			format_number(sc, fdat, 10, width, (precision < 0) ? 6 : precision, 'e');
			break;

		      case 'D': case 'd':
			format_number(sc, fdat, 10, width, (precision < 0) ? 0 : precision, 'd');
			break;

		      case 'O': case 'o':
			format_number(sc, fdat, 8, width, (precision < 0) ? 0 : precision, 'o');
			break;

		      case 'X': case 'x':
			format_number(sc, fdat, 16, width, (precision < 0) ? 0 : precision, 'x');
			break;

		      case 'B': case 'b':
			format_number(sc, fdat, 2, width, (precision < 0) ? 0 : precision, 'b');
			break;
		      
		      default:
			return(s7_format_error(sc, "unimplemented format directive", str, args, fdat));
		      }
		  }
		  break;

		default:
		  return(s7_format_error(sc, "unimplemented format directive", str, args, fdat));
		}
	    }
	  else format_append_char(fdat, str[i]);
	}
    }

 ALL_DONE:
  if (next_arg)
    (*next_arg) = fdat->args;
  else
    {
      if (fdat->args != sc->NIL)
	return(s7_format_error(sc, "too many arguments", str, args, fdat));
    }
  if (i < str_len)
    format_append_char(fdat, str[i]);    /* possible trailing ~ is sent out */
  format_append_char(fdat, '\0');
  result = fdat->str;
  free(fdat);
  return(result);
}


static s7_pointer format_to_output(s7_scheme *sc, s7_pointer out_loc, const char *in_str, s7_pointer args)
{
  char *out_str;
  s7_pointer result;

  out_str = format_to_c_string(sc, in_str, args, NULL);
  result = s7_make_string(sc, out_str);
  if (out_str) free(out_str);

  if (out_loc == sc->F)
    return(result);

  s7_display(sc, result, out_loc);
  return(sc->F);
}


static s7_pointer g_format(s7_scheme *sc, s7_pointer args)
{
  #define H_format "(format out str . args) substitutes args into str sending the result to out."

  if (s7_is_string(car(args)))
    return(format_to_output(sc, sc->F, s7_string(car(args)), cdr(args)));

  if (!s7_is_string(cadr(args)))
    return(s7_wrong_type_arg_error(sc, "format", 2, cadr(args), "a string"));
    
  if (!((s7_is_boolean(sc, car(args))) ||    /* #f or #t */
	(car(args) == sc->NIL) ||            /* default current-output-port = stdout -> nil */
	(s7_is_output_port(sc, car(args))))) /* (current-output-port) or call-with-open-file arg, etc */
    return(s7_wrong_type_arg_error(sc, "format", 1, car(args), "#f, #t, or an output port"));

  return(format_to_output(sc, (car(args) == sc->T) ? sc->output_port : car(args), s7_string(cadr(args)), cddr(args)));
}


const char *s7_format(s7_scheme *sc, s7_pointer args);
const char *s7_format(s7_scheme *sc, s7_pointer args)
{
  return(s7_string(g_format(sc, args))); /* for the run macro in run.c */
}
#endif




/* -------------------------------- errors -------------------------------- */

static char *no_outer_parens(char *str)
{
  int len;
  len = safe_strlen(str);
  if (len > 1)
    {
      int i, stop = 0;
      for (i = 0; i < len; i++)
	if (str[i] == '(')
	  {
	    stop = i;
	    str[i] = ' ';
	    break;
	  }
      if (i < len)
	for (i = len - 1; i > stop; i--)
	  if (str[i] == ')')
	    {
	      str[i] = ' ';
	      break;
	    }
    }
  return(str);
}


/* debugging (error reporting) info */

#define INITIAL_FILE_NAMES_SIZE 8
static char **file_names = NULL;
static int file_names_size = 0;
static int file_names_top = -1;

#if HAVE_PTHREADS
static pthread_mutex_t remember_files_lock = PTHREAD_MUTEX_INITIALIZER;
#endif

#define remembered_line_number(Line) (Line & 0xfffff)
#define remembered_file_name(Line)   (((Line >> 20) <= file_names_top) ? file_names[Line >> 20] : "?")
/* this gives room for 4000 files each of 1000000 lines */

static s7_pointer remember_line(s7_scheme *sc, s7_pointer obj)
{
  if (sc->input_port != sc->NIL)
    set_pair_line_number(obj, port_line_number(sc->input_port) | (port_file_number(sc->input_port) << 20));
  return(obj);
}


static int remember_file_name(const char *file)
{
#if HAVE_PTHREADS
  pthread_mutex_lock(&remember_files_lock);
#endif

  file_names_top++;
  if (file_names_top >= file_names_size)
    {
      if (file_names_size == 0)
	{
	  file_names_size = INITIAL_FILE_NAMES_SIZE;
	  file_names = (char **)calloc(file_names_size, sizeof(char *));
	}
      else
	{
	  int i, old_size;
	  old_size = file_names_size;
	  file_names_size *= 2;
	  file_names = (char **)realloc(file_names, file_names_size * sizeof(char *));
	  for (i = old_size; i < file_names_size; i++)
	    file_names[i] = NULL;
	}
    }
  file_names[file_names_top] = strdup(file);

#if HAVE_PTHREADS
  pthread_mutex_unlock(&remember_files_lock);
#endif

  return(file_names_top);
}


/* -------- backtrace -------- */

#if HAVE_PTHREADS
static pthread_mutex_t backtrace_lock = PTHREAD_MUTEX_INITIALIZER;
#endif


static void print_backtrace_entry(s7_scheme *sc, int n)
{
  char *str = NULL;
  #define NUMBUF_SIZE 64
  char numbuf[NUMBUF_SIZE];
  int line = 0;
  s7_pointer code, args;

#if HAVE_PTHREADS
  sc = sc->orig_sc;
#endif

  code = sc->backtrace_ops[n];
  args = sc->backtrace_args[n];

  if ((code == sc->NIL) && (args == sc->NIL))
    return;

#if HAVE_PTHREADS
  if (sc->backtrace_thread_ids[n] != 0)
    {
      snprintf(numbuf, NUMBUF_SIZE, "[%d] ", sc->backtrace_thread_ids[n]);
      write_string(sc, numbuf, sc->error_port);
    }
#endif

  /* we have backtrace_ops|args equivalent to earlier code|args */

  write_string(sc, "(", sc->error_port);
  if (s7_is_function(code))
    write_string(sc, function_name(code), sc->error_port);
  else
    {
      if (s7_is_procedure_with_setter(code))
	write_string(sc, pws_name(code), sc->error_port);
      else 
	{
	  str = s7_object_to_c_string(sc, code); /* do we need the thread-local env here? unfortunately, yes... */
	  write_string(sc, str, sc->error_port);
	  if (str) free(str);
	}
    }

  write_string(sc, " ", sc->error_port);
  str = no_outer_parens(s7_object_to_c_string(sc, args));
  if (safe_strlen(str) > 128)
    {
      str[124] = '.';
      str[125] = '.';
      str[126] = '.';
      str[127] = '\0';
    }
  write_string(sc, str, sc->error_port);
  if (str) free(str);
  write_string(sc, ")", sc->error_port);

  line = pair_line_number(code);
  if (line > 0)
    {
      if ((remembered_line_number(line) != 0) &&
	  (remembered_file_name(line)))
	{
	  write_string(sc, "        ; ", sc->error_port);
	  write_string(sc, remembered_file_name(line), sc->error_port);
	  snprintf(numbuf, NUMBUF_SIZE, "[%d]", remembered_line_number(line));
	  write_string(sc, numbuf, sc->error_port);
	}
    }
  write_string(sc, "\n", sc->error_port);
}


static s7_pointer g_backtrace(s7_scheme *sc, s7_pointer args)
{
  #define H_backtrace "(backtrace) prints out the last few evaluated expressions, somewhat like a C stacktrace."
  int i;

#if HAVE_PTHREADS
  pthread_mutex_lock(&backtrace_lock);
#endif

  write_string(sc, "\neval history:\n", sc->error_port);
  for (i = 0; i < sc->backtrace_size; i++)
    print_backtrace_entry(sc, (sc->backtrace_size + sc->backtrace_top - i - 1) % sc->backtrace_size);

#if HAVE_PTHREADS
  pthread_mutex_unlock(&backtrace_lock);
#endif

  return(sc->F);
}


static void add_backtrace_entry(s7_scheme *sc, s7_pointer code, s7_pointer args)
{
  int n;

#if HAVE_PTHREADS
  int id;
  pthread_mutex_lock(&backtrace_lock);
  id = sc->thread_id;
  sc = sc->orig_sc;
#endif

  n = sc->backtrace_top++;
  if (sc->backtrace_top >= sc->backtrace_size)
    sc->backtrace_top = 0;

  sc->backtrace_ops[n] = code;
  sc->backtrace_args[n] = args;

#if HAVE_PTHREADS
  sc->backtrace_thread_ids[n] = id;
  pthread_mutex_unlock(&backtrace_lock);
#endif
}


static s7_pointer g_clear_backtrace(s7_scheme *sc, s7_pointer args)
{
  int i;
  #define H_clear_backtrace "(clear-backtrace) erases any current backtrace info."

#if HAVE_PTHREADS
  pthread_mutex_lock(&backtrace_lock);
  sc = sc->orig_sc;
#endif

  sc->backtrace_top = 0;
  for (i = 0; i < sc->backtrace_size; i++)
    {
      sc->backtrace_ops[i] = sc->NIL;
      sc->backtrace_args[i] = sc->NIL;
    }

#if HAVE_PTHREADS
  pthread_mutex_unlock(&backtrace_lock);
#endif

  return(sc->F);
}


static void make_backtrace_buffer(s7_scheme *sc, int size)
{
  int i;

#if HAVE_PTHREADS
  pthread_mutex_lock(&backtrace_lock);
  sc = sc->orig_sc;
  if (sc->backtrace_thread_ids) free(sc->backtrace_thread_ids);
#endif

  if (sc->backtrace_ops) free(sc->backtrace_ops);
  if (sc->backtrace_args) free(sc->backtrace_args);

  sc->backtrace_top = 0;
  sc->backtrace_size = size;
  sc->backtrace_ops = (s7_pointer *)malloc(sc->backtrace_size * sizeof(s7_pointer));
  sc->backtrace_args = (s7_pointer *)malloc(sc->backtrace_size * sizeof(s7_pointer));
  for (i = 0; i < sc->backtrace_size; i++)
    {
      sc->backtrace_ops[i] = sc->NIL;
      sc->backtrace_args[i] = sc->NIL;
    }

#if HAVE_PTHREADS
  sc->backtrace_thread_ids = (int *)calloc(sc->backtrace_size, sizeof(int));
  pthread_mutex_unlock(&backtrace_lock);
#endif
}


static s7_pointer g_set_backtrace_length(s7_scheme *sc, s7_pointer args)
{
  #define H_set_backtrace_length "(set-backtrace-length length) sets the number of entries displayed in a backtrace"
  int len;
  if (!s7_is_integer(car(args)))
    return(s7_wrong_type_arg_error(sc, "set-backtrace-length", 1, car(args), "an integer"));
  len = s7_integer(car(args));
  if (len > 0)
    make_backtrace_buffer(sc, len);
  return(s7_make_integer(sc, sc->backtrace_size));
}


s7_pointer s7_wrong_type_arg_error(s7_scheme *sc, const char *caller, int arg_n, s7_pointer arg, const char *descr)
{
  int len, slen;
  char *errmsg;

  len = safe_strlen(descr) + safe_strlen(caller) + 64;
  errmsg = (char *)malloc(len * sizeof(char));
  if (arg_n <= 0) arg_n = 1;
  slen = snprintf(errmsg, len, "%s: argument %d (~A) has wrong type (expecting %s)", caller, arg_n, descr);
  sc->x = make_list_2(sc, s7_make_string_with_length(sc, errmsg, slen), arg);
  free(errmsg);

  return(s7_error(sc, sc->WRONG_TYPE_ARG, sc->x));
}


s7_pointer s7_out_of_range_error(s7_scheme *sc, const char *caller, int arg_n, s7_pointer arg, const char *descr)
{
  int len, slen;
  char *errmsg;
  
  len = safe_strlen(descr) + safe_strlen(caller) + 64;
  errmsg = (char *)malloc(len * sizeof(char));
  if (arg_n <= 0) arg_n = 1;
  slen = snprintf(errmsg, len, "%s: argument %d (~A) is out of range (expecting %s)", caller, arg_n, descr);
  sc->x = make_list_2(sc, s7_make_string_with_length(sc, errmsg, slen), arg);
  free(errmsg);

  return(s7_error(sc, sc->OUT_OF_RANGE, sc->x));
}


static s7_pointer s7_division_by_zero_error(s7_scheme *sc, const char *caller, s7_pointer arg)
{
  int len, slen;
  char *errmsg;

  len = safe_strlen(caller) + 128;
  errmsg = (char *)malloc(len * sizeof(char));
  slen = snprintf(errmsg, len, "%s: division by zero in ~A", caller);
  sc->x = make_list_2(sc, s7_make_string_with_length(sc, errmsg, slen), arg);
  free(errmsg);

  return(s7_error(sc, s7_make_symbol(sc, "division-by-zero"), sc->x));
}


static s7_pointer s7_file_error(s7_scheme *sc, const char *caller, const char *descr, const char *name)
{
  int len, slen;
  char *errmsg;

  len = safe_strlen(descr) + safe_strlen(name) + safe_strlen(caller) + 8;
  errmsg = (char *)malloc(len * sizeof(char));
  slen = snprintf(errmsg, len, "%s: %s %s", caller, descr, name);
  sc->x = s7_make_string_with_length(sc, errmsg, slen);
  free(errmsg);

  return(s7_error(sc, s7_make_symbol(sc, "io-error"), sc->x));
}


void s7_set_error_exiter(s7_scheme *sc, void (*error_exiter)(void))
{
  sc->error_exiter = error_exiter;
}


static s7_pointer g_dynamic_wind(s7_scheme *sc, s7_pointer args)
{
  #define H_dynamic_wind "(dynamic-wind init body finish) calls init, then body, then finish, guaranteeing that finish is called even if body is exited"
  s7_pointer p;
  dwind *dw;
  
  dw = (dwind *)calloc(1, sizeof(dwind));
  dw->in = car(args);
  dw->body = cadr(args);
  dw->out = caddr(args);
  dw->state = DWIND_INIT;
  
  p = new_cell(sc);
  set_type(p, T_DYNAMIC_WIND | T_ATOM | T_FINALIZABLE | T_DONT_COPY); /* atom -> don't mark car/cdr, don't copy */
  p->object.winder = dw;
  push_stack(sc, OP_DYNAMIC_WIND, sc->NIL, p);          /* args will be the saved result, code = dwind obj */
  
  sc->args = sc->NIL;
  sc->code = dw->in;
  push_stack(sc, OP_APPLY, sc->args, sc->code);
  return(sc->F);
}


static s7_pointer g_catch(s7_scheme *sc, s7_pointer args)
{
  #define H_catch "(catch tag thunk handler) evaluates thunk; if an error occurs that matches tag (#t matches all), the handler is called"
  s7_pointer p;
  rcatch *c;
  
  c = (rcatch *)calloc(1, sizeof(rcatch));
  c->tag = car(args);
  c->goto_loc = sc->stack_top;
  c->handler = caddr(args);
  
  p = new_cell(sc);
  set_type(p, T_CATCH | T_ATOM | T_FINALIZABLE | T_DONT_COPY); /* atom -> don't mark car/cdr, don't copy */
  p->object.catcher = c;
  push_stack(sc, OP_CATCH, sc->NIL, p);
  
  sc->args = sc->NIL;
  sc->code = cadr(args);
  push_stack(sc, OP_APPLY, sc->args, sc->code);
  return(sc->F);
}


static s7_pointer s7_error_1(s7_scheme *sc, s7_pointer type, s7_pointer info, bool exit_eval)
{
  int i;
  s7_pointer catcher;
  catcher = sc->F;
  
#if 0
  fprintf(stderr, "%p s7_error: %s %s\n", sc, s7_object_to_c_string(sc, type), s7_object_to_c_string(sc, info));
#endif

  /* top is 1 past actual top, top - 1 is op, if op = OP_CATCH, top - 4 is the cell containing the catch struct */
  /*
  fprintf(stderr, "check stack %d\n", sc->stack_top);
  */
  for (i = sc->stack_top - 1; i >= 3; i -= 4)
    {
      opcode_t op;
      s7_pointer x;
      op = (opcode_t)s7_integer(vector_element(sc->stack, i));
      
      /*
      fprintf(stderr, "op: %lld\n", s7_integer(vector_element(sc->stack, i)));
      */

      if (op == OP_DYNAMIC_WIND)
	{
	  x = vector_element(sc->stack, i - 3);
	  if (dynamic_wind_state(x) == DWIND_BODY)
	    s7_call(sc, dynamic_wind_out(x), sc->NIL);
	  /* TODO: if an error happens in this call, we can get an infinite loop -- need to just get back to top-level */
	}
      else
	{
	  if (op == OP_CATCH)
	    {
	      x = vector_element(sc->stack, i - 3);

	      /*
	      fprintf(stderr, "found catch %s\n", s7_object_to_c_string(sc, catch_tag(x)));
	      */

	      if ((type == sc->T) ||
		  (catch_tag(x) == sc->T) ||
		  (s7_is_eq(catch_tag(x), type)))
		{
		  /*
		  fprintf(stderr, "got one\n");
		  */
		  catcher = x;
		  break;
		}
	    }
	}
    }
  
  if (catcher != sc->F)
    {
      /*
      fprintf(stderr, "we have a catcher ");
      */
      sc->args = make_list_2(sc, type, info);
      sc->code = catch_handler(catcher);
      sc->stack_top = catch_goto_loc(catcher);
      sc->op = OP_APPLY;

      /* explicit eval needed if s7_call called into scheme where a caught error occurred (ex6 in exs7.c)
       *  but putting it here (via eval(sc, OP_APPLY)) means the C stack is not cleared correctly in non-s7-call cases, 
       *  so defer it until s7_call 
       */
    }
  else
    {
      /* if info is not a list, send object->string to current error port,
       *   else assume car(info) is a format control string, and cdr(info) are its args
       */

      if ((!s7_is_list(sc, info)) ||
	  (!s7_is_string(car(info))))
	format_to_output(sc, 
			 s7_current_error_port(sc), 
			 "\n;~A ~A",
			 make_list_2(sc, type, info));
      else
	{
	  char *errstr;
	  int len;
	  len = 8 + string_length(car(info));
	  errstr = (char *)malloc(len * sizeof(char));
	  snprintf(errstr, len, "\n;%s", s7_string(car(info)));
	  format_to_output(sc,
			   s7_current_error_port(sc), 
			   errstr,
			   cdr(info));
	  free(errstr);
	}

      /* now display location and \n at end */

      if (is_input_port(sc->input_port))
	{
	  const char *filename = NULL;
	  int line;

	  if (port_filename(sc->input_port))
	    filename = port_filename(sc->input_port);
	  else
	    {
	      if ((port_file_number(sc->input_port) >= 0) &&
		  (file_names[port_file_number(sc->input_port)]))
		filename = file_names[port_file_number(sc->input_port)];
	    }

	  line = port_line_number(sc->input_port);
    
	  if (filename)
	    format_to_output(sc,
			     s7_current_error_port(sc), 
			     ", ~A[~D]",
			     make_list_2(sc, s7_make_string(sc, filename), s7_make_integer(sc, line)));
	  else 
	    {
	      if (line > 0)
		format_to_output(sc,
				 s7_current_error_port(sc), 
				 ", line ~D", 
				 make_list_1(sc, s7_make_integer(sc, line)));
	    }
	}

      s7_newline(sc, s7_current_error_port(sc));
      g_backtrace(sc, sc->NIL);
      
      if ((exit_eval) &&
	  (sc->error_exiter))
	(*(sc->error_exiter))();
      
      sc->value = type;
      stack_reset(sc);
      sc->op = OP_QUIT;
    }
  
  if (sc->longjmp_ok)
    {
      longjmp(sc->goto_start, 1); /* this is trying to clear the C stack back to some clean state */
    }

  return(type);
}


s7_pointer s7_error(s7_scheme *sc, s7_pointer type, s7_pointer info)
{
  return(s7_error_1(sc, type, info, false));
}


s7_pointer s7_error_and_exit(s7_scheme *sc, s7_pointer type, s7_pointer info)
{
  return(s7_error_1(sc, type, info, true));
}


static s7_pointer eval_error_0(s7_scheme *sc, const char *errmsg)
{
  return(s7_error(sc, sc->ERROR, make_list_1(sc, s7_make_string(sc, errmsg))));
}


static s7_pointer eval_error(s7_scheme *sc, const char *errmsg, s7_pointer obj)
{
  return(s7_error(sc, sc->ERROR, make_list_2(sc, s7_make_string(sc, errmsg), obj)));
}


static s7_pointer eval_error_2(s7_scheme *sc, const char *errmsg, s7_pointer obj1, s7_pointer obj2)
{
  return(s7_error(sc, sc->ERROR, make_list_3(sc, s7_make_string(sc, errmsg), obj1, obj2)));
}


static s7_pointer g_error(s7_scheme *sc, s7_pointer args)
{
  #define H_error "(error type ...) signals an error"
  if (is_pair(args))
    return(s7_error(sc, car(args), cdr(args)));
  return(s7_error(sc, sc->NIL, sc->NIL));
}



/* -------------------------------- leftovers -------------------------------- */

static s7_pointer g_quit(s7_scheme *sc, s7_pointer args)
{
  #define H_quit "(quit) returns from the evaluator"
  push_stack(sc, OP_QUIT, sc->NIL, sc->NIL);
  return(sc->NIL);
}


static s7_pointer g_force(s7_scheme *sc, s7_pointer args)
{
  #define H_force "(force obj) lazily evaluates obj"
  if (is_promise(car(args)))
    {
      sc->code = car(args);
      push_stack(sc, OP_FORCE, sc->NIL, sc->code);
      sc->args = sc->NIL;
      push_stack(sc, OP_APPLY, sc->args, sc->code);
      return(sc->NIL);
    }
  /* already forced, presumably */
  return(car(args));
}


static s7_pointer apply_list_star(s7_scheme *sc, s7_pointer d) 
{
  s7_pointer p, q;
  if (cdr(d) == sc->NIL) 
    return(car(d));
  
  p = s7_cons(sc, car(d), cdr(d));
  q = p;
  while(cdr(cdr(p)) != sc->NIL) 
    {
      d = s7_cons(sc, car(p), cdr(p));
      if (cdr(cdr(p)) != sc->NIL) 
	p = cdr(d);
    }
  if (!s7_is_list(sc, car(cdr(p))))
    return(s7_error(sc, sc->WRONG_TYPE_ARG, s7_make_string(sc, "apply's last argument should be a list")));
  
  cdr(p) = car(cdr(p));
  return(q);
}


static s7_pointer g_apply(s7_scheme *sc, s7_pointer args)
{
  #define H_apply "(apply func ...) applies func to the rest of the arguments"
  sc->code = car(args);
  if (cdr(args) == sc->NIL)
    sc->args = sc->NIL;
  else 
    {
      sc->args = apply_list_star(sc, cdr(args));
      
      if (g_is_list(sc, make_list_1(sc, sc->args)) == sc->F)
	return(s7_error(sc, sc->WRONG_TYPE_ARG, s7_make_string(sc, "apply's last argument should be a list")));
    }
  push_stack(sc, OP_APPLY, sc->args, sc->code);
  return(sc->NIL);
}


static s7_pointer g_eval(s7_scheme *sc, s7_pointer args)
{
  #define H_eval "(eval code :optional env) evaluates code in the environment env"
  
  if (cdr(args)!= sc->NIL) 
    sc->envir= cadr(args);
  sc->code = car(args);
  push_stack(sc, OP_EVAL, sc->args, sc->code);
  return(sc->NIL);
}


static void assign_syntax(s7_scheme *sc, const char *name, opcode_t op) 
{
  s7_pointer x;
  x = symbol_table_add_by_name(sc, name); 
  typeflag(x) |= (T_SYNTAX | T_IMMUTABLE | T_CONSTANT | T_DONT_COPY); 
  typeflag(x) &= (~T_FINALIZABLE);
  syntax_opcode(x) = small_int(sc, (int)op);
}


s7_pointer s7_call(s7_scheme *sc, s7_pointer func, s7_pointer args)
{ 
  bool old_longjmp;
  /* this can be called while we are in the eval loop (within eval_c_string for instance),
   *   and if we reset the stack, the previously running evaluation steps off the end
   *   of the stack == segfault. 
   */
  old_longjmp = sc->longjmp_ok;
  if (!sc->longjmp_ok)
    {
      sc->longjmp_ok = true;
      if (setjmp(sc->goto_start) != 0)
	{
	  eval(sc, sc->op);
	  return(sc->value);
	}
    }
  
  push_stack(sc, OP_EVAL_STRING_DONE, sc->args, sc->code); /* this saves the current evaluation and will eventually finish this (possibly) nested call */
  sc->args = args; 
  sc->code = func; 
  eval(sc, OP_APPLY);
  sc->longjmp_ok = old_longjmp;
  return(sc->value);
} 


static s7_pointer g_scheme_implementation(s7_scheme *sc, s7_pointer args)
{
  #define H_scheme_implementation "(scheme-implementation) returns some string describing the current S7"
  return(s7_make_string(sc, "s7 " S7_VERSION ", " S7_DATE));
}


static s7_pointer g_for_each(s7_scheme *sc, s7_pointer args)
{
  #define H_for_each "(for-each proc lst . lists) applies proc to a list made up of the car of each arg list"
  s7_pointer lists;
  int i;
  
  sc->code = car(args);
  lists = cdr(args);
  if (car(lists) == sc->NIL)
    return(sc->NIL);
  
  sc->x = sc->NIL;
  
  /* get car of each arg list making the current proc arglist */
  sc->args = sc->NIL;
  
  for (i = 2, sc->y = lists; sc->y != sc->NIL; i++, sc->y = cdr(sc->y))
    {
      if (g_is_list(sc, sc->y) != sc->T)
	return(s7_wrong_type_arg_error(sc, "for-each", i, car(sc->y), "a list"));
      
      sc->args = s7_cons(sc, caar(sc->y), sc->args);
      sc->x = s7_cons(sc, cdar(sc->y), sc->x);
    }
  sc->args = safe_reverse_in_place(sc, sc->args);
  sc->x = safe_reverse_in_place(sc, sc->x);
  
  /* if lists have no cdr (just 1 set of args), apply the proc to them */
  if (car(sc->x) == sc->NIL)
    {
      push_stack(sc, OP_APPLY, sc->args, sc->code);
      return(sc->NIL);
    }
  
  /* set up for repeated call walking down the lists of args */
  push_stack(sc, OP_FOR_EACH, sc->x, sc->code);
  push_stack(sc, OP_APPLY, sc->args, sc->code);
  return(sc->NIL);
}


static s7_pointer g_map(s7_scheme *sc, s7_pointer args)
{
  #define H_map "(map proc lst . lists) applies proc to a list made up of the car of each arg list, returning a list of the values returned by proc"
  s7_pointer lists;
  int i;
  
  sc->code = car(args);
  lists = cdr(args);
  if (car(lists) == sc->NIL)
    return(sc->NIL);
  
  sc->x = sc->NIL;
  
  /* get car of each arg list making the current proc arglist */
  sc->args = sc->NIL;
  for (i = 2, sc->y = lists; sc->y != sc->NIL; i++, sc->y = cdr(sc->y))
    {
      if (g_is_list(sc, sc->y) != sc->T)
	return(s7_wrong_type_arg_error(sc, "map", i, car(sc->y), "a list"));
      
      sc->args = s7_cons(sc, caar(sc->y), sc->args);
      /* car(sc->y) = cdar(sc->y); */ /* this clobbers the original lists -- we need to copy */
      sc->x = s7_cons(sc, cdar(sc->y), sc->x);
    }
  sc->args = safe_reverse_in_place(sc, sc->args);
  sc->x = safe_reverse_in_place(sc, sc->x);
  
  /* set up for repeated call walking down the lists of args, values list is cdr, current args is car */
  push_stack(sc, OP_MAP, make_list_1(sc, sc->x), sc->code);
  push_stack(sc, OP_APPLY, sc->args, sc->code);
  return(sc->NIL);
}


static s7_pointer g_values(s7_scheme *sc, s7_pointer args)
{
  /* I can't see any point in this thing, even in its fancy version (which this is not) */
  #define H_values "(values obj ...) returns its arguments"
  
  if (args == sc->NIL)
    return(sc->NIL);
  
  if (s7_list_length(sc, args) == 1)
    return(car(args));
  
  return(s7_append(sc, make_list_1(sc, sc->VALUES), args));
}


/* (call-with-values (lambda () (values 1 2 3)) +) */

static s7_pointer g_call_with_values(s7_scheme *sc, s7_pointer args)
{
  #define H_call_with_values "(call-with-values producer consumer) applies consumer to the multiple values returned by producer"
  s7_pointer result;
  result = s7_call(sc, car(args), sc->NIL);
  
  if ((is_pair(result)) &&
      (s7_is_eq(car(result), sc->VALUES)))
    return(s7_call(sc, cadr(args), cdr(result)));
  
  return(s7_call(sc, cadr(args), make_list_1(sc, result)));
}


static s7_pointer eval_symbol(s7_scheme *sc, s7_pointer sym)
{
  sc->x = s7_find_symbol_in_environment(sc, sc->envir, sym, true);
  if (sc->x != sc->NIL) 
    return(symbol_value(sc->x));
  if (s7_is_keyword(sym))
    return(sym);
	  
  /* isn't this in the global env? keyword also -- both these checks seem useless */
  sc->x = symbol_table_find_by_name(sc, s7_symbol_name(sym), symbol_location(sym));
  if (is_syntax(sc->x))
    return(sc->x);

  return(eval_error(sc, "~A: unbound variable", sym));
}


static s7_pointer read_expression(s7_scheme *sc)
{
  while (true) 
    {
      /*
      fprintf(stderr, "tok: %d ", sc->tok);
      */
      switch (sc->tok) 
	{
	case TOK_EOF:
	  return(sc->EOF_OBJECT);
	  
	case TOK_VEC:
	  push_stack(sc, OP_READ_VECTOR, sc->NIL, sc->NIL);
	  /* fall through */
	  
	case TOK_LPAREN:
	  sc->tok = token(sc, sc->input_port);
	  if (sc->tok == TOK_RPAREN)
	    return(sc->NIL);

	  if (sc->tok == TOK_DOT) 
	    return(eval_error_0(sc, "syntax error: illegal dot expression: '( . ...)"));

	  push_stack(sc, OP_READ_LIST, sc->NIL, sc->NIL);
	  break;
	  
	case TOK_QUOTE:
	  push_stack(sc, OP_READ_QUOTE, sc->NIL, sc->NIL);
	  sc->tok = token(sc, sc->input_port);
	  break;
	  
	case TOK_BQUOTE:
	  sc->tok = token(sc, sc->input_port);
	  if (sc->tok == TOK_VEC) 
	    {
	      push_stack(sc, OP_READ_QUASIQUOTE_VECTOR, sc->NIL, sc->NIL);
	      sc->tok= TOK_LPAREN;
	    } 
	  else push_stack(sc, OP_READ_QUASIQUOTE, sc->NIL, sc->NIL);
	  break;
	  
	case TOK_COMMA:
	  push_stack(sc, OP_READ_UNQUOTE, sc->NIL, sc->NIL);
	  sc->tok = token(sc, sc->input_port);
	  break;
	  
	case TOK_ATMARK:
	  push_stack(sc, OP_READ_UNQUOTE_SPLICING, sc->NIL, sc->NIL);
	  sc->tok = token(sc, sc->input_port);
	  break;
	  
	case TOK_ATOM:
	  return(make_atom(sc, read_string_upto(sc), 10, true)); /* true = not just string->number */
	  /* if reading list (from lparen), this will finally get us to op_read_list */
	  
	case TOK_DQUOTE:
	  sc->value = read_string_expression(sc, sc->input_port);
	  if (sc->value == sc->F) 
	    return(eval_error(sc, "error reading string: ~A", sc->code));
	  return(sc->value);
	  
	case TOK_SHARP_CONST:
	  {
	    char *expr;
	    expr = read_string_upto(sc);
	    sc->value = make_sharp_constant(sc, expr);
	    if (sc->value == sc->NIL)
	      return(eval_error(sc, "undefined sharp expression: #~A", s7_make_string(sc, expr)));
	    return(sc->value);
	  }

	case TOK_DOT:
	  return(eval_error(sc, "stray dot?", sc->code));

	default:
	  if (sc->tok == TOK_RPAREN)
	    return(eval_error(sc, "unexpected close paren: ~A", sc->code));
	  return(eval_error(sc, "syntax error: illegal token: ~A", sc->code));
	}
    }
  return(sc->NIL);
}
      
      


/* -------------------------------- eval -------------------------------- */

/* all explicit write-* in eval assume current-output-port -- error fallback handling, etc */
/*   internal reads assume sc->input_port is the input port */

static s7_pointer eval(s7_scheme *sc, opcode_t first_op) 
{
  sc->op = first_op;
  
  /* this procedure can be entered recursively (via s7_call for example), so it's no place for a setjmp
   *   I don't think the recursion can hurt our continuations because s7_call is coming from hooks and
   *   callbacks that are implicit in our stack.
   */
  
 START:

#if S7_DEBUGGING
  sc->x = sc->NIL;
  sc->y = sc->NIL;
  sc->z = sc->NIL;
#endif
  
  switch (sc->op) 
    {
    case OP_READ_INTERNAL:
      sc->tok = token(sc, sc->input_port);
      if (sc->tok == TOK_EOF) 
	{
	  pop_stack(sc);
	  goto START;
	}
      sc->value = read_expression(sc);
      pop_stack(sc);
      goto START;
      
      
      /* g_read(p) from C 
       *   read one expr, return it, let caller deal with input port setup 
       */
    case OP_READ_RETURN_EXPRESSION:
      return(sc->F);
      
      
      /* (read p) from scheme
       *    "p" becomes current input port for eval's duration, then pops back before returning value into calling expr
       */
    case OP_READ_POP_AND_RETURN_EXPRESSION:
      pop_input_port(sc);
      
      if (sc->tok == TOK_EOF)
	{
	  sc->value = sc->EOF_OBJECT;
	  pop_stack(sc);
	  goto START;
	}
      pop_stack(sc);
      goto START;
      
      
      /* load("file"); from C (g_load) -- assume caller will clean up
       *   read and evaluate exprs until EOF that matches (stack reflects nesting)
       */
    case OP_LOAD_RETURN_IF_EOF:  /* loop here until eof (via push stack below) */
      if (sc->tok != TOK_EOF)
	{
	  push_stack(sc, OP_LOAD_RETURN_IF_EOF, sc->NIL, sc->NIL);
	  push_stack(sc, OP_READ_INTERNAL, sc->NIL, sc->NIL);
	  sc->code = sc->value;
	  goto EVAL;             /* we read an expression, now evaluate it, and return to read the next */
	}
      return(sc->F);
      
      
      /* (load "file") in scheme 
       *    read and evaluate all exprs, then upon EOF, close current and pop input port stack
       */
    case OP_LOAD_CLOSE_AND_POP_IF_EOF:
      if (sc->tok != TOK_EOF)
	{
	  push_stack(sc, OP_LOAD_CLOSE_AND_POP_IF_EOF, sc->args, sc->code);
	  push_stack(sc, OP_READ_INTERNAL, sc->NIL, sc->NIL);
	  sc->code = sc->value;
	  goto EVAL;             /* we read an expression, now evaluate it, and return to read the next */
	}
      
      s7_close_input_port(sc, sc->input_port);
      pop_input_port(sc);
      pop_stack(sc);
      goto START;
      
      
      /* read and evaluate string expression(s?)
       *    assume caller (C via g_eval_c_string) is dealing with the string port
       */
    case OP_EVAL_STRING:
      if (sc->tok != TOK_EOF)
	{
	  if (s7_peek_char(sc, sc->input_port) != EOF)
	    {
	      push_stack(sc, OP_EVAL_STRING, sc->NIL, sc->value);
	      push_stack(sc, OP_READ_INTERNAL, sc->NIL, sc->NIL);
	    }
	  else push_stack(sc, OP_EVAL_STRING_DONE, sc->NIL, sc->value);
	}
      else push_stack(sc, OP_EVAL_STRING_DONE, sc->NIL, sc->value);
      sc->code = sc->value;
      goto EVAL;
      
      
    case OP_EVAL_STRING_DONE:
     return(sc->F);
      

    case OP_FOR_EACH:
      sc->x = sc->args; /* save lists */
      sc->args = sc->NIL;
      for (sc->y = sc->x; sc->y != sc->NIL; sc->y = cdr(sc->y))
	{
	  sc->args = s7_cons(sc, caar(sc->y), sc->args);
	  car(sc->y) = cdar(sc->y);
	}
      sc->args = safe_reverse_in_place(sc, sc->args);
      if (car(sc->x) == sc->NIL)
	goto APPLY;
      
      push_stack(sc, OP_FOR_EACH, sc->x, sc->code);
      goto APPLY;
      
      
    case OP_MAP:
      /* car of args incoming is arglist, cdr is values list (nil to start) */
      sc->x = sc->args;
      cdr(sc->x) = s7_cons(sc, sc->value, cdr(sc->x)); /* add current value to list */
      
      if (caar(sc->x) == sc->NIL)
	{
	  sc->value = safe_reverse_in_place(sc, cdr(sc->x));
	  pop_stack(sc);
	  goto START;
	}
      
      sc->args = sc->NIL;
      for (sc->y = car(sc->x); sc->y != sc->NIL; sc->y = cdr(sc->y))
	{
	  sc->args = s7_cons(sc, caar(sc->y), sc->args);
	  car(sc->y) = cdar(sc->y);
	}
      sc->args = safe_reverse_in_place(sc, sc->args);
      
      push_stack(sc, OP_MAP, sc->x, sc->code);
      goto APPLY;
      
      
    case OP_DO: 
      /* setup is very similar to let */
      if (!s7_is_list(sc, car(sc->code))) /* (do 123) */
	return(eval_error(sc, "do var list is not a list: ~A", sc->code));

      if (!s7_is_list(sc, cadr(sc->code))) /* (do ((i 0)) 123) */
	return(eval_error(sc, "do end-test and end-value list is not a list: ~A", sc->code));

      if (car(sc->code) == sc->NIL)            /* (do () ...) */
	{
	  sc->envir = new_frame_in_env(sc, sc->envir); 
	  sc->args = s7_cons(sc, sc->NIL, cadr(sc->code));
	  sc->code = cddr(sc->code);
	  goto DO_END0;
	}
      
      /* eval each init value, then set up the new frame (like let, not let*) */
      
      sc->args = sc->NIL;       /* the evaluated var-data */
      sc->value = sc->code;     /* protect it */
      sc->code = car(sc->code); /* the vars */
      
      
    case OP_DO_INIT:
      sc->args = s7_cons(sc, sc->value, sc->args); /* code will be last element (first after reverse) */
      if (is_pair(sc->code))
	{
	  /* here sc->code is a list like: ((i 0 (+ i 1)) ...)
	   *   so cadar gets the init value
	   */
	  if (!(is_pair(car(sc->code))))
	    return(eval_error(sc, "do var slot is empty? ~A", sc->code));

	  if ((is_pair(cdar(sc->code))) &&
	      (is_pair(cddar(sc->code))) && 
	      (cdr(cddar(sc->code)) != sc->NIL))  /* (do ((i 0 1 (+ i 1))) ...) */
	    return(eval_error(sc, "do var has extra stuff after the increment expression: ~A", sc->code));

	  push_stack(sc, OP_DO_INIT, sc->args, cdr(sc->code));
	  sc->code = cadar(sc->code);
	  sc->args = sc->NIL;
	  goto EVAL;
	}

      if (sc->code != sc->NIL)
	return(eval_error(sc, "do var list is improper: ~A", sc->code));
      
      /* all done */
      sc->args = safe_reverse_in_place(sc, sc->args);
      sc->code = car(sc->args); /* saved at the start */
      sc->args = cdr(sc->args); /* init values */
      sc->envir = new_frame_in_env(sc, sc->envir); 
      
      sc->value = sc->NIL;
      for (sc->x = car(sc->code), sc->y = sc->args; sc->y != sc->NIL; sc->x = cdr(sc->x), sc->y = cdr(sc->y)) 
	sc->value = s7_cons(sc, add_to_current_environment(sc, caar(sc->x), car(sc->y)), sc->value);
      
      /* now we've set up the environment, next set up for loop */
      
      sc->y = safe_reverse_in_place(sc, sc->value);
      sc->args = sc->NIL;
      for (sc->x = car(sc->code); sc->y != sc->NIL; sc->x = cdr(sc->x), sc->y = cdr(sc->y))       
	if (cddar(sc->x) != sc->NIL) /* no incr expr, so ignore it henceforth */
	  {
	    sc->value = s7_cons(sc, caddar(sc->x), sc->NIL);
	    sc->value = s7_cons(sc, car(sc->y), sc->value);
	    sc->args = s7_cons(sc, sc->value, sc->args);
	  }
      sc->value = safe_reverse_in_place(sc, sc->args);
      sc->args = s7_cons(sc, sc->value, cadr(sc->code));
      sc->code = cddr(sc->code);
      
      
    DO_END0:
    case OP_DO_END0:
      /* here vars have been init'd or incr'd
       *    args = (cons var-data end-data)
       *    code = body
       */
      
      push_stack(sc, OP_DO_END1, sc->args, sc->code);
      /* evaluate the endtest */
      sc->code = cadr(sc->args);
      sc->args = sc->NIL;
      goto EVAL;
      
      
    case OP_DO_END1:
      /* sc->value should be result of endtest evaluation */
      if (is_true(sc->value))
	{
	  /* we're done -- deal with result exprs */
	  sc->code = cddr(sc->args);
	  sc->args = sc->NIL;
	  goto BEGIN;
	}
      
      /* evaluate the body and step vars, etc */
      push_stack(sc, OP_DO_STEP0, sc->args, sc->code);
      /* sc->code is ready to go */
      sc->args = sc->NIL;
      goto BEGIN;
      
      
    case OP_DO_STEP0:
      /* increment all vars, return to endtest 
       *   these are also updated in parallel at the end, so we gather all the incremented values first
       */
      if (car(sc->args) == sc->NIL)
	goto DO_END0;
      
      push_stack(sc, OP_DO_END0, sc->args, sc->code);
      sc->code = s7_cons(sc, sc->NIL, car(sc->args));   /* car = list of newly incremented values, cdr = list of slots */
      sc->args = car(sc->args);
      
      
    DO_STEP1:
    case OP_DO_STEP1:
      if (sc->args == sc->NIL)
	{
	  sc->y = cdr(sc->code);
	  sc->code = safe_reverse_in_place(sc, car(sc->code));
	  for (sc->x = sc->code; sc->y != sc->NIL && sc->x != sc->NIL; sc->x = cdr(sc->x), sc->y = cdr(sc->y))
	    set_symbol_value(caar(sc->y), car(sc->x));

	  /* "real" schemes rebind here, rather than reset, but that is expensive,
	   *    and only matters once in a blue moon (closure over enclosed lambda referring to a do var)
	   *    and the caller can easily mimic the correct behavior in that case by adding a let,
	   *    making the rebinding explicit.
	   */
	  
	  sc->value = sc->NIL;
	  pop_stack(sc); 
	  goto DO_END0;
	}
      push_stack(sc, OP_DO_STEP2, sc->args, sc->code);
      /* here sc->args is a list like (((i . 0) (1+ i)) ...)
       *   so sc->code becomes (1+ i) in this case 
       */
      sc->code = cadar(sc->args);
      sc->args = sc->NIL;
      goto EVAL;
      
      
    case OP_DO_STEP2:
      car(sc->code) = s7_cons(sc, sc->value, car(sc->code));  /* add this value to our growing list */
      sc->args = cdr(sc->args);                               /* go to next */
      goto DO_STEP1;
      
      
    BEGIN:
    case OP_BEGIN:
      if (!is_pair(sc->code)) 
	{
	  sc->value = sc->code;
	  pop_stack(sc);
	  goto START;
	}
      
      if (cdr(sc->code) != sc->NIL) 
	push_stack(sc, OP_BEGIN, sc->NIL, cdr(sc->code));
      
      sc->code = car(sc->code);
      /* goto EVAL; */
      
      
    EVAL:
    case OP_EVAL:       /* main part of evaluation */
      ASSERT_IS_OBJECT(sc->envir, "env");
      ASSERT_IS_OBJECT(sc->code, "code");

      if (is_pair(sc->code)) 
	{
	  sc->x = car(sc->code);
	  if (is_syntax(sc->x))
	    {     
	      sc->code = cdr(sc->code);
	      sc->op = (opcode_t)integer(syntax_opcode(sc->x)->object.number);
	      goto START;
	    } 
	  push_stack(sc, OP_EVAL_ARGS0, sc->NIL, sc->code);
	  sc->code = car(sc->code);
	  goto EVAL;
	} 

      if (s7_is_symbol(sc->code))
	{
	  sc->value = eval_symbol(sc, sc->code);
	  pop_stack(sc);
	  goto START;
	}

      sc->value = sc->code;
      pop_stack(sc);
      goto START;
      
      
    case OP_EVAL_ARGS0:
      sc->saved_line_number = pair_line_number(sc->code);

      if (is_macro(sc->value)) 
	{    
	  /* macro expansion */
	  push_stack(sc, OP_DOMACRO, sc->NIL, sc->NIL);
	  sc->args = make_list_1(sc, sc->code);
	  sc->code = sc->value;
	  set_pair_line_number(sc->code, sc->saved_line_number);
	  goto APPLY;
	} 
      else 
	{
	  sc->code = cdr(sc->code);

	  /* here [after the cdr] sc->args is nil, sc->value is the operator (car of list), sc->code is the rest -- the args.
	   *   EVAL_ARGS0 can be called within the EVAL_ARGS1 loop if it's a nested expression:
	   * (+ 1 2 (* 2 3)):
	   *   e0args: (), value: +, code: (1 2 (* 2 3))
	   *   e1args: (+), value: +, code: (1 2 (* 2 3))
	   *   e1args: (1 +), value: +, code: (2 (* 2 3))
	   *   e1args: (2 1 +), value: +, code: ((* 2 3))
	   *   e0args: (), value: *, code: (2 3)
	   *   e1args: (*), value: *, code: (2 3)
	   *   e1args: (2 *), value: *, code: (3)
	   *   e1args: (3 2 *), value: *, code: ()
	   *   <end -> apply the * op>
	   *   e1args: (6 2 1 +), value: +, code: ()
	   */
	}
      
      
    EVAL_ARGS:
    case OP_EVAL_ARGS1:
      sc->args = cons_untyped(sc, sc->value, sc->args);
      set_type(sc->args, T_PAIR);

      /* 1st time, value = op, args=nil (only e0 entry is from op_eval above), code is full list (at e0) */

      if (is_pair(sc->code))  /* evaluate current arg */
	{ 
	  int typ;
	  typ = type(car(sc->code));
	  if (typ == T_PAIR)
	    {
	      push_stack(sc, OP_EVAL_ARGS1, sc->args, cdr(sc->code));
	      sc->code = car(sc->code);
	      sc->args = sc->NIL;
	      goto EVAL;
	    }
	  if (typ == T_SYMBOL)
	    {
	      sc->value = eval_symbol(sc, car(sc->code));
	      sc->code = cdr(sc->code);
	      goto EVAL_ARGS;
	    }
	  sc->value = car(sc->code);
	  sc->code = cdr(sc->code);
	  goto EVAL_ARGS;
	}
      else                       /* got all args -- go to apply */
	{
	  sc->args = safe_reverse_in_place(sc, sc->args); 
	  sc->code = car(sc->args);
	  set_pair_line_number(sc->code, sc->saved_line_number);
	  sc->args = cdr(sc->args);
	  /* goto APPLY;  */
	}
      
      
      /* ---------------- OP_APPLY ---------------- */
    APPLY:
    case OP_APPLY:      /* apply 'code' to 'args' */
      add_backtrace_entry(sc, sc->code, sc->args);
      
      switch (type(sc->code))
	{
	case T_S7_FUNCTION: 	                  /* -------- C-based function -------- */
	  {
	    int len;
	    len = safe_list_length(sc, sc->args);
	    if (len < function_required_args(sc->code))
	      return(eval_error_2(sc, "~A: not enough arguments: ~A", sc->code, sc->args));
	    
	    if ((!function_has_rest_arg(sc->code)) &&
		((function_required_args(sc->code) + function_optional_args(sc->code)) < len))
	      return(eval_error_2(sc, "~A: too many arguments: ~A", sc->code, sc->args));
	    
	    sc ->value = function_call(sc->code)(sc, sc->args);
	    ASSERT_IS_OBJECT(sc->value, "function returned value");
	    pop_stack(sc);
	    goto START;
	  }
	  
	case T_CLOSURE:
	case T_MACRO:
	case T_PROMISE:             	          /* -------- normal function (lambda), macro, or delay -------- */

	  sc->envir = new_frame_in_env(sc, s7_procedure_environment(sc->code)); 
	  
	  /* load up the current args into the ((args) (lambda)) layout [via the current environment] */
	  for (sc->x = car(closure_source(sc->code)), sc->y = sc->args; is_pair(sc->x); sc->x = cdr(sc->x), sc->y = cdr(sc->y)) 
	    {
	      if (sc->y == sc->NIL)
		return(eval_error_2(sc, "~A: not enough arguments: ~A", g_procedure_source(sc, make_list_1(sc, sc->code)), sc->args));
	      
	      ASSERT_IS_OBJECT(sc->x, "parameter to closure");
	      ASSERT_IS_OBJECT(sc->y, "arg to closure");
	      
	      add_to_current_environment(sc, car(sc->x), car(sc->y));
	    }
	  
	  if (sc->x == sc->NIL) 
	    {
	      if (sc->y != sc->NIL)
		return(eval_error(sc, "too many arguments: ~A", sc->args));
	    } 
	  else 
	    {
	      if (s7_is_symbol(sc->x))
		add_to_current_environment(sc, sc->x, sc->y); 
	      else 
		{
		  if (is_macro(sc->code))
		    return(eval_error(sc, "~A: undefined argument to macro?", sc->x));
		  else return(eval_error(sc, "~A: undefined argument to function?", sc->x));
		}
	    }
	  sc->code = cdr(closure_source(sc->code));
	  sc->args = sc->NIL;
	  goto BEGIN;
	  
	case T_CLOSURE_STAR:	                  /* -------- define* (lambda*) -------- */
	  { 
	    sc->envir = new_frame_in_env(sc, s7_procedure_environment(sc->code)); 
	    
	    /* here the car(closure_source) is the uninterpreted argument list:
	     * (define* (hi a (b 1)) (+ a b))
	     * (procedure-source hi) -> (lambda* (a (b 1)) (+ a b))
	     *
	     * so rather than spinning through the args binding names to values in the
	     *   procedure's new environment (as in the usual closure case above),
	     *   we scan the current args, and match against the
	     *   template in the car of the closure, binding as we go.
	     *
	     * for each actual arg, if it's not a keyword that matches a member of the 
	     *   template, bind it to its current (place-wise) arg, else bind it to
	     *   that arg.  If it's the symbol :key or :optional, just go on.
	     *   If it's :rest bind the next arg to the trailing args at this point.
	     *   All args can be accessed by their name as a keyword.
	     *   In other words (define* (hi (a 1)) ...) is the same as (define* (hi :key (a 1)) ...) etc.
	     *
	     * all args are optional, any arg with no default value defaults to #f.
	     */
	    
	    /* set all default values */
	    for (sc->z = car(closure_source(sc->code)); is_pair(sc->z); sc->z = cdr(sc->z))
	      {
		/* bind all the args to something (default value or #f or maybe #undefined) */
		if (!((car(sc->z) == sc->KEY_KEY) ||
		      (car(sc->z) == sc->KEY_OPTIONAL) ||
		      (car(sc->z) == sc->KEY_REST)))                  /* :optional and :key always ignored, :rest dealt with later */
		  {
		    if (is_pair(car(sc->z)))                       /* (define* (hi (a mus-next)) a) */
		      add_to_current_environment(sc,                  /* or (define* (hi (a 'hi)) (list a (eq? a 'hi))) */
						 caar(sc->z), 
						 lambda_star_argument_default_value(sc, cadar(sc->z)));
		                                                  /* mus-next, for example, needs to be evaluated before binding */
		    else add_to_current_environment(sc, car(sc->z), sc->F);
		  }
	      }
	    if (s7_is_symbol(sc->z)) /* dotted arg? -- make sure its name exists in the current environment */
	      add_to_current_environment(sc, sc->z, sc->F);
	    
	    /* now get the current args, re-setting args that have explicit values */
	    sc->x = car(closure_source(sc->code));
	    sc->y = sc->args; 
	    sc->z = sc->NIL;
	    while ((is_pair(sc->x)) &&
		   (is_pair(sc->y)))
	      {
		if ((car(sc->x) == sc->KEY_KEY) ||
		    (car(sc->x) == sc->KEY_OPTIONAL))
		  sc->x = cdr(sc->x);
		else
		  {
		    if (car(sc->x) == sc->KEY_REST)
		      {
			/* next arg is bound to trailing args from this point as a list */
			sc->z = sc->KEY_REST;
			sc->x = cdr(sc->x);
			if (is_pair(car(sc->x)))
			  lambda_star_argument_set_value(sc, caar(sc->x), sc->y);
			else lambda_star_argument_set_value(sc, car(sc->x), sc->y);
			sc->y = cdr(sc->y);
			sc->x = cdr(sc->x);
		      }
		    else
		      {
			if (s7_is_keyword(car(sc->y)))
			  {
			    const char *name;
			    name = s7_symbol_name(car(sc->y));
			    if (!(lambda_star_argument_set_value(sc, 
								 s7_make_symbol(sc, (const char *)(name + 1)), 
								 car(cdr(sc->y)))))
			      return(eval_error(sc, "unknown argument name: ~A", sc->y));
			    sc->y = cddr(sc->y);
			  }
			else 
			  {
			    if (is_pair(car(sc->x)))
			      lambda_star_argument_set_value(sc, caar(sc->x), car(sc->y));
			    else lambda_star_argument_set_value(sc, car(sc->x), car(sc->y));
			    sc->y = cdr(sc->y);
			  }
			sc->x = cdr(sc->x);
		      }
		  }
	      }

	    /* check for trailing args with no :rest arg */
	    if (sc->y != sc->NIL)
	      {
		if ((sc->x == sc->NIL) &&
		    (is_pair(sc->y)))
		  {
		    if (sc->z != sc->KEY_REST)
		      return(eval_error(sc, "too many arguments: ~A", sc->args));
		  } 
		else 
		  {
		    /* final arg was dotted? */
		    if (s7_is_symbol(sc->x))
		      add_to_current_environment(sc, sc->x, sc->y); 
		  }
	      }

	    /* evaluate the function body */
	    sc->code = cdr(closure_source(sc->code));
	    sc->args = sc->NIL;
	    goto BEGIN;
	  }

	  /* for C level define*, we need to make the arg list from the current values in the
	   *   environment, then 
	   *     sc ->value = function_call(sc->code)(sc, sc->args);
	   *     pop_stack(sc);
	   *     goto START;
	   * (assuming sc->code was saved across the arg setup stuff)
	   * the c-define* object would need to have the arg template somewhere accessible
	   *   then somehow share the merging code.
	   *   T_S7_FUNCTION_STAR?
	   */
		
	case T_CONTINUATION:	                  /* -------- continuation ("call-with-continuation") -------- */
	  s7_call_continuation(sc, sc->code);
	  sc->value = (sc->args != sc->NIL) ? car(sc->args) : sc->NIL;
	  pop_stack(sc);
	  goto START;

	case T_GOTO:	                          /* -------- goto ("call-with-exit") -------- */
	  {
	    int i, new_stack_top;
	    new_stack_top = (sc->code)->object.goto_loc;
	    /* look for dynamic-wind in the stack section that we are jumping out of */
	    for (i = sc->stack_top - 1; i > new_stack_top; i -= 4)
	      {
		if ((opcode_t)s7_integer(vector_element(sc->stack, i)) == OP_DYNAMIC_WIND)
		  {
		    sc->z = vector_element(sc->stack, i - 3);
		    if (dynamic_wind_state(sc->z) == DWIND_BODY)
		      s7_call(sc, dynamic_wind_out(sc->z), sc->NIL);
		  }
	      }
	    
	    sc->stack_top = new_stack_top;
	    sc->value = (sc->args != sc->NIL) ? car(sc->args) : sc->NIL;
	    pop_stack(sc);
	    goto START;
	  }

	case T_S7_OBJECT:	                  /* -------- applicable object -------- */
	  sc ->value = s7_apply_object(sc, sc->code, sc->args);
	  pop_stack(sc);
	  goto START;

	default:
	  return(eval_error(sc, "~A: apply of non-function?", sc->code));
	}
      /* ---------------- end OP_APPLY ---------------- */

      
    case OP_DOMACRO:    /* macro after args are gathered */
      sc->code = sc->value;
      goto EVAL;
      
      
    case OP_LAMBDA: 
      if ((!is_pair(sc->code)) ||
	  (!is_pair(cdr(sc->code)))) /* (lambda) or (lambda #f) */
	return(eval_error(sc, "lambda: no args? ~A", sc->code));

      if (!s7_is_list(sc, car(sc->code)))
	{
	  if (!s7_is_symbol(car(sc->code))) /* (lambda "hi" ...) */
	    return(eval_error(sc, "lambda parameter ~A is not a symbol", sc->code));
	}
      else
	{
	  /* look for (lambda (1) ...) etc */
	  for (sc->x = car(sc->code); sc->x != sc->NIL; sc->x = cdr(sc->x))
	    if ((!s7_is_symbol(sc->x)) && /* dotted list */
		(!s7_is_symbol(car(sc->x))))
	      return(eval_error(sc, "lambda parameter ~A is not a symbol", sc->code));
	}
      
      sc->value = make_closure(sc, sc->code, sc->envir, T_CLOSURE);
      pop_stack(sc);
      goto START;


    case OP_LAMBDA_STAR:
      sc->value = make_closure(sc, sc->code, sc->envir, T_CLOSURE_STAR);
      pop_stack(sc);
      goto START;
      
      
    case OP_QUOTE:
      sc->value = car(sc->code);
      pop_stack(sc);
      goto START;
      

    case OP_DEFINE_CONSTANT1:
      /* define-constant -> OP_DEFINE_CONSTANT0 -> OP_DEFINE0..1, then back to here */
      /*   at this point, sc->value is the symbol that we want to be immutable, sc->code is the original pair */
      sc->x = s7_find_symbol_in_environment(sc, sc->envir, sc->value, false);
      s7_set_immutable(car(sc->x));
      pop_stack(sc);
      goto START;


    case OP_DEFINE_CONSTANT0:
      push_stack(sc, OP_DEFINE_CONSTANT1, sc->NIL, sc->code);

      
    case OP_DEFINE_STAR:
    case OP_DEFINE0:
      if (!is_pair(sc->code))
	return(eval_error(sc, "define: nothing to define? ~A", sc->code));

      if (!is_pair(cdr(sc->code)))
	return(eval_error(sc, "define: no value? ~A", sc->code));

      if ((!is_pair(car(sc->code))) &&
	  (is_pair(cddr(sc->code))))
	return(eval_error(sc, "define: more than 1 value? ~A", sc->code));

      if (s7_is_immutable(car(sc->code)))
	return(eval_error(sc, "define: can't alter immutable object: ~A", car(sc->code)));
      
      if (is_pair(car(sc->code))) 
	{
	  sc->x = caar(sc->code);
	  if (sc->op == OP_DEFINE_STAR)
	    sc->code = s7_cons(sc, sc->LAMBDA_STAR, s7_cons(sc, cdar(sc->code), cdr(sc->code)));
	  else sc->code = s7_cons(sc, sc->LAMBDA, s7_cons(sc, cdar(sc->code), cdr(sc->code)));
	} 
      else 
	{
	  sc->x = car(sc->code);
	  sc->code = cadr(sc->code);
	}
      if (!s7_is_symbol(sc->x))
	return(eval_error(sc, "define a non-symbol? ~A", sc->x));
      
      push_stack(sc, OP_DEFINE1, sc->NIL, sc->x);
      goto EVAL;
      
      
    case OP_DEFINE1:
      /* sc->code is the symbol being defined, sc->value is its value
       *   if sc->value is a closure, car is of the form ((args...) body...)
       *   so the doc string if any is (cadr (car value))
       *   and the arg list gives the number of required args up to the dot
       */
      sc->x = s7_find_symbol_in_environment(sc, sc->envir, sc->code, false);
      if (sc->x != sc->NIL) 
	set_symbol_value(sc->x, sc->value); 
      else add_to_current_environment(sc, sc->code, sc->value); 
      sc->value = sc->code;
      pop_stack(sc);
      goto START;
      
      
    case OP_SET2:
      sc->code = s7_cons(sc, s7_cons(sc, sc->value, sc->args), sc->code);

      
    case OP_SET0:
      if (s7_is_immutable(car(sc->code)))
	return(eval_error(sc, "set!: unable to alter immutable variable: ~A", car(sc->code)));
      
      if ((cdr(sc->code) == sc->NIL) ||
	  (cddr(sc->code) != sc->NIL))
	return(eval_error(sc, "~A: wrong number of args to set!", sc->code));
      
      if (is_pair(car(sc->code))) /* has accessor */
	{
	  if (is_pair(caar(sc->code)))
	    {
	      push_stack(sc, OP_SET2, cdar(sc->code), cdr(sc->code));
	      sc->code = caar(sc->code);
	      goto EVAL;
	    }
	  
	  sc->x = s7_symbol_value(sc, caar(sc->code));
	  if ((s7_is_object(sc->x)) &&
	      (object_set_function(sc->x)))
	    sc->code = s7_cons(sc, sc->SET_OBJECT, s7_append(sc, car(sc->code), cdr(sc->code)));   /* use set method */
	  else return(eval_error(sc, "no generalized set for ~A", caar(sc->code)));
	}
      else 
	{
	  if (!s7_is_symbol(car(sc->code)))
	    return(eval_error(sc, "trying to set! ~A", car(sc->code)));
	  
	  push_stack(sc, OP_SET1, sc->NIL, car(sc->code));
	  sc->code = cadr(sc->code);
	}
      goto EVAL;
      
      
    case OP_SET1:      
      sc->y = s7_find_symbol_in_environment(sc, sc->envir, sc->code, true);
      if (sc->y != sc->NIL) 
	{
	  set_symbol_value(sc->y, sc->value); 
	  pop_stack(sc);
	  goto START;
	}
      return(eval_error(sc, "set! ~A: unbound variable", sc->code));
      
      
    case OP_IF0:
      /* check number of "args" */
      if ((sc->code == sc->NIL) ||
	  (cdr(sc->code) == sc->NIL) ||
	  ((cddr(sc->code) != sc->NIL) && 
	   (cdddr(sc->code) != sc->NIL)))
	return(eval_error(sc, "if: syntax error: ~A", sc->code));
      
      push_stack(sc, OP_IF1, sc->NIL, cdr(sc->code));
      sc->code = car(sc->code);
      goto EVAL;
      
      
    case OP_IF1:
      if (is_true(sc->value))
	sc->code = car(sc->code);
      else
	sc->code = cadr(sc->code);  /* (if #f 1) ==> #<unspecified> because car(sc->NIL) = sc->UNSPECIFIED */
      goto EVAL;
      
      
    case OP_LET0:
      if ((!is_pair(sc->code)) ||
	  (!is_pair(cdr(sc->code))))
	return(eval_error(sc, "let syntax error: ~A", sc->code));
      
      sc->args = sc->NIL;
      sc->value = sc->code;
      sc->code = s7_is_symbol(car(sc->code)) ? cadr(sc->code) : car(sc->code);
      
      
    case OP_LET1:       /* let -- calculate parameters */
      sc->args = s7_cons(sc, sc->value, sc->args);
      if (is_pair(sc->code)) 
	{ 
	  if ((!is_pair(car(sc->code))) ||
	      (!(is_pair(cdar(sc->code)))))   /* (let ((x . 1))...) */
	    return(eval_error(sc, "let syntax error (not a proper list?): ~A", car(sc->code)));

	  push_stack(sc, OP_LET1, sc->args, cdr(sc->code));
	  sc->code = cadar(sc->code);
	  sc->args = sc->NIL;
	  goto EVAL;
	} 

      sc->args = safe_reverse_in_place(sc, sc->args);
      sc->code = car(sc->args);
      sc->args = cdr(sc->args);
      
      
    case OP_LET2:
      sc->envir = new_frame_in_env(sc, sc->envir); 
      for (sc->x = s7_is_symbol(car(sc->code)) ? cadr(sc->code) : car(sc->code), sc->y = sc->args; sc->y != sc->NIL; sc->x = cdr(sc->x), sc->y = cdr(sc->y)) 
	{
	  if (!(s7_is_symbol(caar(sc->x))))
	    return(eval_error(sc, "bad variable ~A in let bindings", car(sc->code)));

	  add_to_current_environment(sc, caar(sc->x), car(sc->y)); 
	}
      if (s7_is_symbol(car(sc->code))) 
	{    /* named let */
	  for (sc->x = cadr(sc->code), sc->args = sc->NIL; sc->x != sc->NIL; sc->x = cdr(sc->x)) 
	    sc->args = s7_cons(sc, caar(sc->x), sc->args);
	  
	  sc->x = s7_make_closure(sc, s7_cons(sc, safe_reverse_in_place(sc, sc->args), cddr(sc->code)), sc->envir); 
	  add_to_current_environment(sc, car(sc->code), sc->x); 
	  sc->code = cddr(sc->code);
	  sc->args = sc->NIL;
	} 
      else 
	{
	  sc->code = cdr(sc->code);
	  sc->args = sc->NIL;
	}
      goto BEGIN;
      
      
    case OP_LET_STAR0:
      if (!is_pair(cdr(sc->code)))
	return(eval_error(sc, "let* syntax error: ~A", sc->code));
      
      if (car(sc->code) == sc->NIL) 
	{
	  sc->envir = new_frame_in_env(sc, sc->envir); 
	  sc->code = cdr(sc->code);
	  goto BEGIN;
	}
      
      if ((!is_pair(car(sc->code))) ||
	  (!is_pair(caar(sc->code))))
	return(eval_error(sc, "let* variable list syntax error: ~A", sc->code));
      
      push_stack(sc, OP_LET_STAR1, cdr(sc->code), car(sc->code));
      sc->code = cadaar(sc->code);
      goto EVAL;
      
      
    case OP_LET_STAR1:    /* let* -- make new frame */
      sc->envir = new_frame_in_env(sc, sc->envir); 
      
      
    case OP_LET_STAR2:    /* let* -- calculate parameters */
      if (!(s7_is_symbol(caar(sc->code))))
	return(eval_error(sc, "bad variable ~A in let* bindings", car(sc->code)));

      add_to_current_environment(sc, caar(sc->code), sc->value); 
      sc->code = cdr(sc->code);
      if (is_pair(sc->code)) 
	{ 
	  push_stack(sc, OP_LET_STAR2, sc->args, sc->code);
	  sc->code = cadar(sc->code);
	  sc->args = sc->NIL;
	  goto EVAL;
	} 

      sc->code = sc->args;
      sc->args = sc->NIL;
      goto BEGIN;
      
      
    case OP_LETREC0:
      if (!is_pair(cdr(sc->code)))
	return(eval_error(sc, "letrec syntax error: ~A", sc->code));
      
      sc->envir = new_frame_in_env(sc, sc->envir); 
      sc->args = sc->NIL;
      sc->value = sc->code;
      sc->code = car(sc->code);
      
      
    case OP_LETREC1:    /* letrec -- calculate parameters */
      sc->args = s7_cons(sc, sc->value, sc->args);
      if (is_pair(sc->code)) 
	{ 
	  push_stack(sc, OP_LETREC1, sc->args, cdr(sc->code));
	  sc->code = cadar(sc->code);
	  sc->args = sc->NIL;
	  goto EVAL;
	} 

      sc->args = safe_reverse_in_place(sc, sc->args); 
      sc->code = car(sc->args);
      sc->args = cdr(sc->args);
      

    case OP_LETREC2:
      for (sc->x = car(sc->code), sc->y = sc->args; sc->y != sc->NIL; sc->x = cdr(sc->x), sc->y = cdr(sc->y)) 
	{
	  if (!(s7_is_symbol(caar(sc->x))))
	    return(eval_error(sc, "bad variable ~A in letrec bindings", car(sc->x)));

	  add_to_current_environment(sc, caar(sc->x), car(sc->y)); 
	}
      sc->code = cdr(sc->code);
      sc->args = sc->NIL;
      goto BEGIN;
      
      
    case OP_COND0:
      if ((!is_pair(sc->code)) ||
	  (!is_pair(car (sc->code)))) /* (cond 1) */
	return(eval_error(sc, "syntax error in cond: ~A", sc->code));

      push_stack(sc, OP_COND1, sc->NIL, sc->code);
      sc->code = caar(sc->code);
      goto EVAL;
      
      
    case OP_COND1:
      if (is_true(sc->value))     /* got a hit */
	{
	  sc->code = cdar(sc->code);
	  if (sc->code == sc->NIL)
	    {
	      pop_stack(sc);
	      goto START;
	    }
	  if (!is_pair(sc->code)) /* (cond (1 . 2)...) */
	    return(eval_error(sc, "syntax error in cond: ~A", sc->code));
	  
	  if (car(sc->code) == sc->FEED_TO) 
	    {
	      if (!is_pair(cdr(sc->code))) 
		return(eval_error(sc, "syntax error in cond: ~A", cdr(sc->code)));

	      sc->x = make_list_2(sc, sc->QUOTE, sc->value); 
	      sc->code = make_list_2(sc, cadr(sc->code), sc->x);
	      goto EVAL;
	    }
	  
	  goto BEGIN;
	}

      sc->code = cdr(sc->code);
      if (sc->code == sc->NIL)
	{
	  sc->value = sc->NIL;
	  pop_stack(sc);
	  goto START;
	} 
	  
      push_stack(sc, OP_COND1, sc->NIL, sc->code);
      sc->code = caar(sc->code);
      goto EVAL;
      
      
    case OP_MAKE_PROMISE: 
      if (sc->code == sc->NIL)  /* (make-promise) */
	return(eval_error(sc, "make-promise needs an argument: ~A", sc->code));

      if (cdr(sc->code) != sc->NIL)
	return(eval_error(sc, "make-promise takes one argument: ~A", sc->code));

      sc->value = s7_make_closure(sc, s7_cons(sc, sc->NIL, sc->code), sc->envir);
      set_type(sc->value, T_PROMISE);
      pop_stack(sc);
      goto START;
      
      
    case OP_FORCE:     /* Save forced value replacing promise */

      memcpy(sc->code, sc->value, sizeof(s7_cell));

      /* memcpy is trouble:
       * if, for example, sc->value is a string, after memcpy we have two (string) objects in the heap
       *   pointing to the same string.  When they are GC'd, we try to free the same pointer twice.
       *   But we can't clear sc->value and reset its type -- it might be #t for example!
       *   We can't just say sc->code = sc->value because we're playing funny games with
       *   self-modifying code here.  So...
       */
      typeflag(sc->value)  &= (~T_FINALIZABLE); /* make sure GC calls free once */
      pop_stack(sc);
      goto START;
      
      
    case OP_CONS_STREAM0:
      push_stack(sc, OP_CONS_STREAM1, sc->NIL, cdr(sc->code));
      sc->code = car(sc->code);
      goto EVAL;
      
      
    case OP_CONS_STREAM1:
      sc->args = sc->value;  /* save sc->value to register sc->args for gc */
      sc->x = s7_make_closure(sc, s7_cons(sc, sc->NIL, sc->code), sc->envir);
      set_type(sc->x, T_PROMISE);
      sc->value = s7_cons(sc, sc->args, sc->x);
      pop_stack(sc);
      goto START;      
      
      
    case OP_AND0:
      if (sc->code == sc->NIL) 
	{
	  sc->value = sc->T;
	  pop_stack(sc);
	  goto START;
	}
      push_stack(sc, OP_AND1, sc->NIL, cdr(sc->code));
      sc->code = car(sc->code);
      goto EVAL;
      
      
    case OP_AND1:
      if ((is_false(sc->value)) ||
	  (sc->code == sc->NIL))
	{
	  pop_stack(sc);
	  goto START;
	}
      if (cdr(sc->code) != sc->NIL)
	push_stack(sc, OP_AND1, sc->NIL, cdr(sc->code));
      sc->code = car(sc->code);
      goto EVAL;
      
      
    case OP_OR0:
      if (sc->code == sc->NIL) 
	{
	  sc->value = sc->F;
	  pop_stack(sc);
	  goto START;
	}
      push_stack(sc, OP_OR1, sc->NIL, cdr(sc->code));
      sc->code = car(sc->code);
      goto EVAL;
      
      
    case OP_OR1:
      if ((is_true(sc->value)) ||
	  (sc->code == sc->NIL))
	{
	  pop_stack(sc);
	  goto START;
	}
      if (cdr(sc->code) != sc->NIL)
	push_stack(sc, OP_OR1, sc->NIL, cdr(sc->code));
      sc->code = car(sc->code);
      goto EVAL;
      
      
    case OP_MACRO0:     /* this is tinyscheme's weird macro syntax */
      /*
	(macro (when form)
	`(if ,(cadr form) (begin ,@(cddr form))))
      */
      /* (macro (when form) ...) or (macro do (lambda (form) ...))
       *   sc->code is the business after the "macro"
       *   so in 1st case, car(sc->code) is '(when form), and in 2nd it is 'do
       *   in 1st case, put caar(sc->code) "when" into sc->x for later symbol definition, in 2nd use car(sc->code)
       *   in 1st case, wrap up a lambda:
       *      '(lambda (form) ...)
       *   in 2nd case, it's ready to go
       * goto eval popping to OP_MACRO1
       *   eval sees the lambda and creates a closure (s7_make_closure): car => code, cdr => environment
       */
      if (is_pair(car(sc->code))) 
	{
	  sc->x = caar(sc->code);
	  sc->code = s7_cons(sc, sc->LAMBDA, s7_cons(sc, cdar(sc->code), cdr(sc->code)));
	} 
      else 
	{
	  sc->x = car(sc->code);
	  sc->code = cadr(sc->code);
	}
      if (!s7_is_symbol(sc->x)) 
	return(eval_error(sc, "~A: variable is not a symbol", sc->x));

      push_stack(sc, OP_MACRO1, sc->NIL, sc->x);   /* sc->x (the name symbol) will be sc->code when we pop to OP_MACRO1 */
      goto EVAL;
      
      
    case OP_MACRO1:
      /* here sc->code is the name (a symbol), sc->value is a closure object, its car is the form as called
       *   
       *     (macro (when form)
       *       `(if ,(cadr form) (begin ,@(cddr form))))
       * has become:
       *     ((form) 
       *      (quasiquote 
       *        (if (unquote (cadr form)) 
       *            (begin (unquote-splicing (cddr form))))))
       * with 
       *   sc->code: when 
       *   sc->value: #<closure> 
       * where "form" is the thing presented to us in the code, i.e. (when mumble do-this)
       *   and the following code takes that as its argument and transforms it in some way
       */

      set_type(sc->value, T_MACRO);
      
      /* find name in environment, and define it */
      sc->x = s7_find_symbol_in_environment(sc, sc->envir, sc->code, false); 
      if (sc->x != sc->NIL) 
	set_symbol_value(sc->x, sc->value); 
      else add_to_current_environment(sc, sc->code, sc->value); 
      
      /* pop back to wherever the macro call was */
      sc->value = sc->code;
      pop_stack(sc);
      goto START;
      
      
      /* defmacro* via lambda* ? */
    case OP_DEFMACRO:
      
      /* (defmacro name (args) body) ->
       *
       *    (macro (defmacro dform)
       *      (let ((form (gensym "defmac")))            
       *        `(macro (,(cadr dform) ,form)   
       *          (apply
       *            (lambda ,(caddr dform)      
       *             ,@(cdddr dform))          
       *            (cdr ,form)))))             
       *    
       *    end up with name as sc->x going to OP_MACRO1, ((gensym) (lambda (args) body) going to eval
       */
      sc->y = s7_gensym(sc, "defmac");
      sc->x = car(sc->code);
      sc->code = s7_cons(sc,
			 sc->LAMBDA,
			 s7_cons(sc, 
				 make_list_1(sc, sc->y),
				 make_list_1(sc, 
					     s7_cons(sc, 
						     sc->APPLY,
						     s7_cons(sc, 
							     s7_cons(sc, 
								     sc->LAMBDA, 
								     cdr(sc->code)), /* sc->value is a temp */
							     make_list_1(sc, make_list_2(sc, sc->CDR, sc->y)))))));
      /* so, (defmacro hi (a b) `(+ ,a ,b)) becomes:
       *   sc->x: hi
       *   sc->code: (lambda (defmac-51) (apply (lambda (a b) (quasiquote (+ (unquote a) (unquote b)))) (cdr defmac-51)))
       */
      push_stack(sc, OP_MACRO1, /* sc->code */ sc->NIL, sc->x);   /* sc->x (the name symbol) will be sc->code when we pop to OP_MACRO1 */
                                                                  /* sc->code is merely being protected */
      goto EVAL;
      
      
    case OP_DEFINE_MACRO:

      sc->y = s7_gensym(sc, "defmac");
      sc->x = caar(sc->code);
      sc->code = s7_cons(sc,
			 sc->LAMBDA,
			 s7_cons(sc, 
				 make_list_1(sc, sc->y),
				 make_list_1(sc, 
					     s7_cons(sc, 
						     sc->APPLY,
						     s7_cons(sc, 
							     s7_cons(sc, 
								     sc->LAMBDA,
								     s7_cons(sc, 
									     cdar(sc->code),  /* cdadr value */
									     cdr(sc->code))), /* cddr value */
							     make_list_1(sc, make_list_2(sc, sc->CDR, sc->y)))))));
      /* (define-macro (hi a b) `(+ ,a ,b)) becomes:
       *   sc->x: hi
       *   sc->code: (lambda (defmac-51) (apply (lambda (a b) (quasiquote (+ (unquote a) (unquote b)))) (cdr defmac-51)))
       */
      push_stack(sc, OP_MACRO1, sc->NIL, sc->x);   /* sc->x (the name symbol) will be sc->code when we pop to OP_MACRO1 */
      goto EVAL;
      
      
    case OP_CASE0:      /* case, car(sc->code) is the selector */
      if ((!is_pair(sc->code)) ||
	  (!is_pair(cdr(sc->code))) ||
	  (!is_pair(cadr (sc->code)))) 
	return(eval_error(sc, "syntax error in case: ~A", sc->code));
      
      push_stack(sc, OP_CASE1, sc->NIL, cdr(sc->code));
      sc->code = car(sc->code);
      goto EVAL;
      
      
    case OP_CASE1: 
      for (sc->x = sc->code; sc->x != sc->NIL; sc->x = cdr(sc->x)) 
	{
	  if (!is_pair(sc->y = caar(sc->x)))
	    {
	      if ((sc->y != sc->T) &&
		  ((!s7_is_symbol(sc->y)) ||
		   (strcmp(s7_symbol_name(sc->y), "else") != 0)))
		return(eval_error(sc, "case clause key list ~A is not a list or 'else'", sc->y));
	      break;
	    }

	  for ( ; sc->y != sc->NIL; sc->y = cdr(sc->y)) 
	    if (s7_is_eqv(car(sc->y), sc->value)) 
	      break;
	  
	  if (sc->y != sc->NIL) 
	    break;
	}
      
      if (sc->x != sc->NIL) 
	{
	  if (is_pair(caar(sc->x))) 
	    {
	      sc->code = cdar(sc->x);
	      goto BEGIN;
	    } 
	  push_stack(sc, OP_CASE2, sc->NIL, cdar(sc->x));
	  sc->code = caar(sc->x);
	  goto EVAL;
	} 

      sc->value = sc->NIL;
      pop_stack(sc);
      goto START;
      
      
    case OP_CASE2: 
      if (is_true(sc->value)) 
	goto BEGIN;
      sc->value = sc->NIL;
      pop_stack(sc);
      goto START;
      
      
    case OP_QUIT:
      return(sc->F);
      break;
      
      
    case OP_DYNAMIC_WIND:
      
      switch (dynamic_wind_state(sc->code))
	{
	case DWIND_INIT:
	  dynamic_wind_state(sc->code) = DWIND_BODY;
	  push_stack(sc, OP_DYNAMIC_WIND, sc->NIL, sc->code);
	  sc->args = sc->NIL;
	  sc->code = dynamic_wind_body(sc->code);
	  goto APPLY;
	  
	case DWIND_BODY:
	  dynamic_wind_state(sc->code) = DWIND_FINISH;
	  push_stack(sc, OP_DYNAMIC_WIND, sc->value, sc->code);
	  sc->args = sc->NIL;
	  sc->code = dynamic_wind_out(sc->code);
	  goto APPLY;
	  
	case DWIND_FINISH:
	  sc->value = sc->args; /* value saved above */
	  pop_stack(sc); 
	  goto START;
	}
      break;
      
      
    case OP_CATCH:
      pop_stack(sc);
      goto START;
      break;
      
      
    case OP_READ_LIST: 
      sc->args = s7_cons(sc, sc->value, sc->args);
      sc->tok = token(sc, sc->input_port);

      if (sc->tok == TOK_RPAREN) 
	{
	  int c;
	  c = inchar(sc, sc->input_port);
	  if ((c != '\n') && (c != EOF))
	    backchar(sc, c, sc->input_port);
	  
	  sc->value = remember_line(sc, safe_reverse_in_place(sc, sc->args));
	  pop_stack(sc);
	  goto START;
	} 

      if (sc->tok == TOK_DOT) 
	{
	  push_stack(sc, OP_READ_DOT, sc->args, sc->NIL);
	  sc->tok = token(sc, sc->input_port);
	  sc->value = read_expression(sc);
	  pop_stack(sc);
	  goto START;
	} 

      if (sc->tok == TOK_EOF)
	return(eval_error(sc, "~A: missing close paren?", sc->NIL));

      push_stack(sc, OP_READ_LIST, sc->args, sc->NIL);
      sc->value = read_expression(sc);
      pop_stack(sc);
      goto START;

      
    case OP_READ_DOT:
      if (token(sc, sc->input_port) != TOK_RPAREN)
	return(eval_error(sc, "syntax error: illegal dot expression: '(~{~A~^ ~} . )", sc->args));

      /* TODO: (catch #t (lambda () (car '( . ))) (lambda args 'error))
       *   should not try to deal with the bad list outside the catch
       * TODO: all the token errors need better error messages -- give the actual expression, not sc->code!
       * TODO: who pops off the catch op above??
       */

      sc->value = s7_reverse_in_place(sc, sc->value, sc->args);
      pop_stack(sc);
      goto START;
      
      
    case OP_READ_QUOTE:
      sc->value = make_list_2(sc, sc->QUOTE, sc->value);
      pop_stack(sc);
      goto START;      
      
      
    case OP_READ_QUASIQUOTE:
      sc->value = make_list_2(sc, sc->QUASIQUOTE, sc->value);
      pop_stack(sc);
      goto START;
      
      
    case OP_READ_QUASIQUOTE_VECTOR:
      sc->value = make_list_3(sc,
			      sc->APPLY,
			      sc->VECTOR,
			      make_list_2(sc, sc->QUASIQUOTE, sc->value));
      pop_stack(sc);
      goto START;
      
      
    case OP_READ_UNQUOTE:
      sc->value = make_list_2(sc, sc->UNQUOTE, sc->value);
      pop_stack(sc);
      goto START;
      
      
    case OP_READ_UNQUOTE_SPLICING:
      sc->value = make_list_2(sc, sc->UNQUOTE_SPLICING, sc->value);
      pop_stack(sc);
      goto START;
      
      
    case OP_READ_VECTOR:
      sc->value = g_vector(sc, sc->value);
      pop_stack(sc);
      goto START;

      
    default:
      return(eval_error(sc, "~A: unknown operator!", s7_make_integer(sc, sc->op)));
    }
  return(sc->F);
}




/* -------------------------------- quasiquote -------------------------------- */

static s7_pointer g_quasiquote_1(s7_scheme *sc, int level, s7_pointer form)
{
  if (!is_pair(form))
    {
      if ((s7_is_number(form)) ||
	  (s7_is_string(form)) ||
	  (s7_is_procedure(form)))
	return(form);
      return(make_list_2(sc, sc->QUOTE, form));
    }
  /* from here, form is a pair */

  if (car(form) == sc->QUASIQUOTE)
    {
      s7_pointer r;
      r = g_quasiquote_1(sc, level + 1, cdr(form));
      if ((is_pair(r)) &&
	  (car(r) == sc->QUOTE) &&
	  (car(cdr(r)) == cdr(form)))
	return(make_list_2(sc, sc->QUOTE, form));
      return(make_list_3(sc, sc->CONS, make_list_2(sc, sc->QUOTE, sc->QUASIQUOTE), r));
    }

  if (level == 0)
    {
      if (car(form) == sc->UNQUOTE)
	return(car(cdr(form)));
	      
      if (car(form) == sc->UNQUOTE_SPLICING)
	return(form);
	      
      if ((is_pair(car(form))) &&
	  (caar(form) == sc->UNQUOTE_SPLICING))
	{
	  s7_pointer l, r;
	  l = car(cdr(car(form)));
	  if (cdr(form) == sc->NIL)
	    return(l);

	  r = g_quasiquote_1(sc, level, cdr(form));

	  if ((is_pair(r)) &&
	      (car(r) == sc->QUOTE) &&
	      (car(cdr(r)) == sc->NIL))
	    return(l);
	  return(make_list_3(sc, sc->APPEND, l, r));
	}
      
      goto MCONS;
    }

  /* level != 0 */
  if (car(form) == sc->UNQUOTE)
    {
      s7_pointer r;
      r = g_quasiquote_1(sc, level - 1, cdr(form));

      if ((is_pair(r)) &&
	  (car(r) == sc->QUOTE) &&
	  (car(cdr(r)) == cdr(form)))
	return(make_list_2(sc, sc->QUOTE, form));

      return(make_list_3(sc, sc->CONS, make_list_2(sc, sc->QUOTE, sc->UNQUOTE), r));
    }
  
  if (car(form) == sc->UNQUOTE_SPLICING)
    {
      s7_pointer r;
      r = g_quasiquote_1(sc, level - 1, cdr(form));

      if ((is_pair(r)) &&
	  (car(r) == sc->QUOTE) &&
	  (car(cdr(r)) == cdr(form)))
	return(make_list_2(sc, sc->QUOTE, form));

      return(make_list_3(sc, sc->CONS, make_list_2(sc, sc->QUOTE, sc->UNQUOTE_SPLICING), r));
    }
	      
 MCONS:
  {
    s7_pointer l, r;
    l = g_quasiquote_1(sc, level, car(form));
    r = g_quasiquote_1(sc, level, cdr(form));

    if ((is_pair(r)) &&
	(is_pair(l)) &&
	(car(r) == sc->QUOTE) &&
	(car(l) == car(r)) &&
	(car(cdr(r)) == cdr(form)) &&
	(car(cdr(l)) == car(form)))
      return(make_list_2(sc, sc->QUOTE, form));

    if (l == sc->VECTOR_FUNCTION)
      return(g_vector(sc, make_list_1(sc, r))); /* eval? */

    return(make_list_3(sc, sc->CONS, l, r));
  }
}


static s7_pointer g_quasiquote(s7_scheme *sc, s7_pointer args)
{
  return(g_quasiquote_1(sc, s7_integer(car(args)), cadr(args)));
}



/* -------------------------------- threads -------------------------------- */

#if HAVE_PTHREADS

static s7_scheme *clone_s7(s7_scheme *sc, s7_pointer vect);
static s7_scheme *close_s7(s7_scheme *sc);
static void mark_s7(s7_scheme *sc);


typedef struct {
  s7_scheme *sc;
  s7_pointer func;
  pthread_t *thread;
} thred;

static int thread_tag = 0;


static char *thread_print(s7_scheme *sc, void *obj)
{
  char *buf;
  thred *p = (thred *)obj;
  buf = (char *)malloc(32 * sizeof(char));
  snprintf(buf, 32, "#<thread %d>", p->sc->thread_id);
  return(buf);
}


static void thread_free(void *obj)
{
  thred *f = (thred *)obj;
  if (f)
    {
      /* pthread_detach(*(f->thread)); */
      free(f->thread);
      f->thread = NULL;
      f->sc = close_s7(f->sc);
      free(f);
    }
}


static void thread_mark(void *val)
{
  thred *f = (thred *)val;
  if ((f) && (f->sc)) /* possibly still in make_thread */
    {
      mark_s7(f->sc);
      S7_MARK(f->func);
    }
}


static bool thread_equal(void *obj1, void *obj2)
{
  return(obj1 == obj2);
}


static void *run_thread_func(void *obj)
{
  thred *f = (thred *)obj;
  return((void *)s7_call(f->sc, f->func, f->sc->NIL));
}


/* (define hi (make-thread (lambda () (display "hi")))) */

static s7_pointer g_make_thread(s7_scheme *sc, s7_pointer args)
{
  #define H_make_thread "(make-thread thunk) creates a new thread running thunk"
  thred *f;
  s7_pointer obj, vect, frame;
  
  pthread_mutex_lock(&alloc_lock); /* if currently in GC in some thread, wait for it */
  pthread_mutex_unlock(&alloc_lock);
  
  frame = s7_immutable_cons(sc, sc->NIL, sc->envir);
  vect = s7_make_vector(sc, INITIAL_STACK_SIZE);
  
  f = (thred *)calloc(1, sizeof(thred));
  f->func = car(args);
  
  obj = s7_make_object(sc, thread_tag, (void *)f);
  
  pthread_mutex_lock(&alloc_lock);

  f->sc = clone_s7(sc, vect);
  f->sc->envir = frame;
  f->sc->y = obj;
  f->thread = (pthread_t *)malloc(sizeof(pthread_t));

  pthread_create(f->thread, NULL, run_thread_func, (void *)f);
  pthread_mutex_unlock(&alloc_lock);
  
  return(obj);
}


static s7_pointer g_is_thread(s7_scheme *sc, s7_pointer args)
{
  #define H_is_thread "(thread? obj) returns #t if obj is a thread object"
  return(make_boolean(sc, 
		    (s7_is_object(car(args))) &&
		    (s7_object_type(car(args)) == thread_tag)));
}


static s7_pointer g_join_thread(s7_scheme *sc, s7_pointer args)
{
  #define H_join_thread "(join-thread thread) causes the current thread to wait for the thread to finish"
  thred *f;
  if (g_is_thread(sc, args) == sc->F)
    return(s7_wrong_type_arg_error(sc, "join-thread", 1, car(args), "a thread"));
  
  f = (thred *)s7_object_value(car(args));
  pthread_join(*(f->thread), NULL);
  return(car(args));
}


static s7_pointer thread_environment(s7_scheme *sc, s7_pointer obj)
{
  thred *f;
  f = (thred *)s7_object_value(obj);
  if ((f) && (f->sc) && (f->sc->envir))
    return(f->sc->envir);
  return(sc->NIL);
}


/* -------- locks -------- */

static int lock_tag = 0;


static char *lock_print(s7_scheme *sc, void *obj)
{
  char *buf;
  buf = (char *)malloc(64 * sizeof(char));
  snprintf(buf, 64, "#<lock %p>", obj);
  return(buf);
}


static void lock_free(void *obj)
{
  pthread_mutex_t *lock = (pthread_mutex_t *)obj;
  if (lock)
    {
      pthread_mutex_destroy(lock);
      free(lock);
    }
}


static bool lock_equal(void *obj1, void *obj2)
{
  return(obj1 == obj2);
}


static s7_pointer g_is_lock(s7_scheme *sc, s7_pointer args)
{
  #define H_is_lock "(lock? obj) returns #t if obj is a lock (mutex) object"
  return(make_boolean(sc, 
		    (s7_is_object(car(args))) &&
		    (s7_object_type(car(args)) == lock_tag)));
}


static s7_pointer g_make_lock(s7_scheme *sc, s7_pointer args)
{
  #define H_make_lock "(make-lock) creates a new lock (mutex variable)"
  pthread_mutex_t *lock;
  lock = (pthread_mutex_t *)malloc(sizeof(pthread_mutex_t));
  pthread_mutex_init(lock, NULL);
  return(s7_make_object(sc, lock_tag, (void *)lock));  
}


static s7_pointer g_grab_lock(s7_scheme *sc, s7_pointer args)
{
  #define H_grab_lock "(grab-lock lock) stops the current thread until it can grab the lock."
  if (g_is_lock(sc, args) == sc->F)
    return(s7_wrong_type_arg_error(sc, "grab-lock", 1, car(args), "a lock (mutex)"));

  return(s7_make_integer(sc, pthread_mutex_lock((pthread_mutex_t *)s7_object_value(car(args)))));
}


static s7_pointer g_release_lock(s7_scheme *sc, s7_pointer args)
{
  #define H_release_lock "(release-lock lock) releases the lock"
  if (g_is_lock(sc, args) == sc->F)
    return(s7_wrong_type_arg_error(sc, "release-lock", 1, car(args), "a lock (mutex)"));

  return(s7_make_integer(sc, pthread_mutex_unlock((pthread_mutex_t *)s7_object_value(car(args)))));
}



/* -------- thread variables (pthread keys) -------- */

static int key_tag = 0;

static void *key_value(s7_pointer obj)
{
  pthread_key_t *key; 
  key = (pthread_key_t *)s7_object_value(obj);
  return(pthread_getspecific(*key));                  /* returns NULL if never set */
}


static char *key_print(s7_scheme *sc, void *obj)
{
  char *buf, *val_str;
  s7_pointer val;
  void *kval;
  int len;
  kval = key_value((s7_pointer)obj);
  if (kval)
    val = (s7_pointer)kval;
  else val = sc->F;
  val_str = s7_object_to_c_string(sc, val);
  len = 64 + safe_strlen(val_str);
  buf = (char *)malloc(len * sizeof(char));
  snprintf(buf, len, "#<[thread %d] key: %s>", sc->thread_id, val_str);
  if (val_str) free(val_str);
  return(buf);
}


static void key_free(void *obj)
{
  pthread_key_t *key = (pthread_key_t *)obj;
  if (key)
    {
      pthread_key_delete(*key);
      free(key);
    }
}


static bool key_equal(void *obj1, void *obj2)
{
  return(obj1 == obj2);
}


bool s7_is_thread_variable(s7_pointer obj)
{
  return((s7_is_object(obj)) &&
	 (s7_object_type(obj) == key_tag));
}


s7_pointer s7_thread_variable_value(s7_scheme *sc, s7_pointer obj)
{
  void *val;
  val = key_value(obj);
  if (val)
    return((s7_pointer)val);
  return(sc->F);
}


static s7_pointer g_is_thread_variable(s7_scheme *sc, s7_pointer args)
{
  #define H_is_thread_variable "(thread-variable? obj) returns #t if obj is a thread variable (a pthread key)"
  return(make_boolean(sc, s7_is_thread_variable(car(args))));
}


static s7_pointer g_make_thread_variable(s7_scheme *sc, s7_pointer args)
{
  #define H_make_thread_variable "(make-thread-variable) returns a new thread specific variable (a pthread key)"
  pthread_key_t *key;
  key = (pthread_key_t *)malloc(sizeof(pthread_key_t));
  pthread_key_create(key, NULL);
  return(s7_make_object(sc, key_tag, (void *)key));  
}


static s7_pointer get_key(s7_scheme *sc, s7_pointer obj, s7_pointer args)
{
  if (args != sc->NIL)
    return(s7_error(sc, 
		    s7_make_symbol(sc, "wrong-number-of-args"), 
		    make_list_3(sc, s7_make_string(sc, "thread variable is a function of no arguments: ~A ~A"),	obj, args)));
  return(s7_thread_variable_value(sc, obj));
}


static s7_pointer set_key(s7_scheme *sc, s7_pointer obj, s7_pointer args)
{
  pthread_key_t *key;  
  key = (pthread_key_t *)s7_object_value(obj);
  pthread_setspecific(*key, (void *)car(args)); 

  /* to protect from the GC until either the local key value is set again, or the thread is done,
   *   we store the key's local value in an alist '(obj value)
   */
  {
    s7_pointer curval;
    curval = g_assq(sc, make_list_2(sc, obj, sc->key_values));
    if (curval == sc->F)
      sc->key_values = s7_cons(sc, s7_cons(sc, obj, car(args)), sc->key_values);
    else cdr(curval) = car(args);
  }
  return(car(args));
}


static s7_scheme *clone_s7(s7_scheme *sc, s7_pointer vect)
{
  int i;
  s7_scheme *new_sc;
  new_sc = (s7_scheme *)malloc(sizeof(s7_scheme));
  memcpy((void *)new_sc, (void *)sc, sizeof(s7_scheme));
  
  /* share the heap, symbol table and global environment, all the startup stuff,
   *   but have separate stacks and eval locals
   */
  
  new_sc->longjmp_ok = false;
  new_sc->strbuf_size = INITIAL_STRBUF_SIZE;
  new_sc->strbuf = (char*)calloc(new_sc->strbuf_size, sizeof(char));

#if WITH_READ_LINE
  new_sc->read_line_buf = NULL;
  new_sc->read_line_buf_size = 0;
#endif
  
  new_sc->stack_top = 0;
  new_sc->stack = vect;
  new_sc->stack_size = INITIAL_STACK_SIZE;
  
  new_sc->x = new_sc->NIL;
  new_sc->y = new_sc->NIL;
  new_sc->z = new_sc->NIL;
  new_sc->code = new_sc->NIL;
  new_sc->args = new_sc->NIL;
  new_sc->value = new_sc->NIL;
  
  new_sc->temps_size = GC_TEMPS_SIZE;
  new_sc->temps_ctr = 0;
  new_sc->temps = (s7_pointer *)calloc(new_sc->temps_size, sizeof(s7_pointer));
  for (i = 0; i < new_sc->temps_size; i++)
    new_sc->temps[i] = new_sc->NIL;

  new_sc->key_values = sc->NIL;

  pthread_mutex_lock(&backtrace_lock);     /* probably unnecessary... */
  (*(sc->thread_ids))++;                   /* in case a spawned thread spawns another, we need this variable to be global to all */
  new_sc->thread_id = (*(sc->thread_ids)); /* for more readable debugging printout -- main thread is thread 0 */
  pthread_mutex_unlock(&backtrace_lock);

  /* use the main interpreter for these */
  new_sc->backtrace_size = 0;
  new_sc->backtrace_ops = NULL;
  new_sc->backtrace_args = NULL;
  new_sc->backtrace_thread_ids = NULL;

  return(new_sc);
}


static s7_scheme *close_s7(s7_scheme *sc)
{
  free(sc->strbuf);
  free(sc->temps);
#if WITH_READ_LINE
  if (sc->read_line_buf) free(sc->read_line_buf);
#endif
  free(sc);
  return(NULL);
}


static void mark_s7(s7_scheme *sc)
{
  int i;
  S7_MARK(sc->args);
  S7_MARK(sc->envir);
  S7_MARK(sc->code);
  mark_vector(sc->stack, sc->stack_top);
  S7_MARK(sc->value);
  S7_MARK(sc->x);
  S7_MARK(sc->y);
  S7_MARK(sc->z);
  for (i = 0; i < sc->temps_size; i++)
    S7_MARK(sc->temps[i]);
  S7_MARK(sc->key_values);
}

#endif



/* -------------------------------- initialization -------------------------------- */

s7_scheme *s7_init(void) 
{
  int i;
  s7_pointer x;
  s7_scheme *sc;
  
  init_ctables();
  
  sc = (s7_scheme *)malloc(sizeof(s7_scheme));
#if HAVE_PTHREADS
  sc->orig_sc = sc;
#endif
  
  sc->gc_off = (bool *)calloc(1, sizeof(bool));
  (*(sc->gc_off)) = true; /* sc->args and so on are not set yet, so a gc during init -> segfault */
  
  sc->longjmp_ok = false;
  
  sc->strbuf_size = INITIAL_STRBUF_SIZE;
  sc->strbuf = (char*)calloc(sc->strbuf_size, sizeof(char));
  
#if WITH_READ_LINE
  sc->read_line_buf = NULL;
  sc->read_line_buf_size = 0;
#endif

  sc->NIL = &sc->_NIL;
  sc->T = &sc->_HASHT;
  sc->F = &sc->_HASHF;
  sc->EOF_OBJECT = &sc->_EOF_OBJECT;
  sc->UNDEFINED = &sc->_UNDEFINED;
  sc->UNSPECIFIED = &sc->_UNSPECIFIED;
  
  set_type(sc->UNSPECIFIED, T_NIL_TYPE | T_ATOM | T_GC_MARK | T_IMMUTABLE | T_CONSTANT | T_SIMPLE | T_DONT_COPY);
  car(sc->UNSPECIFIED) = cdr(sc->UNSPECIFIED) = sc->UNSPECIFIED;
  
  set_type(sc->NIL, T_NIL_TYPE | T_ATOM | T_GC_MARK | T_IMMUTABLE | T_CONSTANT | T_SIMPLE | T_DONT_COPY);
  car(sc->NIL) = cdr(sc->NIL) = sc->UNSPECIFIED;
  
  set_type(sc->T, T_NIL_TYPE | T_ATOM | T_GC_MARK | T_IMMUTABLE | T_CONSTANT | T_SIMPLE | T_DONT_COPY);
  car(sc->T) = cdr(sc->T) = sc->UNSPECIFIED;
  
  set_type(sc->F, T_NIL_TYPE | T_ATOM | T_GC_MARK | T_IMMUTABLE | T_CONSTANT | T_SIMPLE | T_DONT_COPY);
  car(sc->F) = cdr(sc->F) = sc->UNSPECIFIED;
  
  set_type(sc->EOF_OBJECT, T_NIL_TYPE | T_ATOM | T_GC_MARK | T_IMMUTABLE | T_CONSTANT | T_SIMPLE | T_DONT_COPY);
  car(sc->EOF_OBJECT) = cdr(sc->EOF_OBJECT) = sc->UNSPECIFIED;
  
  set_type(sc->UNDEFINED, T_NIL_TYPE | T_ATOM | T_GC_MARK | T_IMMUTABLE | T_CONSTANT | T_SIMPLE | T_DONT_COPY);
  car(sc->UNDEFINED) = cdr(sc->UNDEFINED) = sc->UNSPECIFIED;
  
  sc->input_port = sc->NIL;
  sc->input_port_stack = sc->NIL;
  sc->output_port = sc->NIL;
  sc->error_port = sc->NIL;
  
  sc->x = sc->NIL;
  sc->y = sc->NIL;
  sc->z = sc->NIL;
  sc->error_exiter = NULL;
  
  sc->heap_size = INITIAL_HEAP_SIZE;
  sc->heap = (s7_pointer *)malloc((sc->heap_size + 1) * sizeof(s7_pointer));
  
  sc->free_heap = (s7_cell **)malloc(sc->heap_size * sizeof(s7_cell *));
  sc->free_heap_top = INITIAL_HEAP_SIZE;
  {
    for (i = 0; i < INITIAL_HEAP_SIZE; i++)
      {
	sc->heap[i] = (s7_cell *)calloc(1, sizeof(s7_cell));
	sc->free_heap[i] = sc->heap[i];
      }
    sc->heap[sc->heap_size] = NULL;
  }
  
  /* this has to precede s7_make_* allocations */
  sc->temps_size = GC_TEMPS_SIZE;
  sc->temps_ctr = 0;
  sc->temps = (s7_pointer *)malloc(sc->temps_size * sizeof(s7_pointer));
  for (i = 0; i < sc->temps_size; i++)
    sc->temps[i] = sc->NIL;
  
  sc->protected_objects_size = (int *)malloc(sizeof(int));
  (*(sc->protected_objects_size)) = INITIAL_PROTECTED_OBJECTS_SIZE;
  sc->protected_objects_loc = (int *)malloc(sizeof(int));
  (*(sc->protected_objects_loc)) = 0;
  sc->protected_objects = s7_make_vector(sc, INITIAL_PROTECTED_OBJECTS_SIZE); /* realloc happens to the embedded array, so this pointer is global */
  set_immutable(sc->protected_objects);
  typeflag(sc->protected_objects) |= (T_CONSTANT | T_DONT_COPY);
  
  sc->stack_top = 0;
  sc->stack = s7_make_vector(sc, INITIAL_STACK_SIZE);
  sc->stack_size = INITIAL_STACK_SIZE;
  
  sc->symbol_table = s7_make_vector(sc, SYMBOL_TABLE_SIZE);
  set_immutable(sc->symbol_table);
  typeflag(sc->symbol_table) |= (T_CONSTANT | T_DONT_COPY);
  
  sc->gc_verbose = (bool *)calloc(1, sizeof(bool));
  sc->load_verbose = (bool *)calloc(1, sizeof(bool));
  sc->gensym_counter = (long *)calloc(1, sizeof(long));

  make_backtrace_buffer(sc, INITIAL_BACKTRACE_SIZE);
#if HAVE_PTHREADS
  sc->thread_ids = (int *)calloc(1, sizeof(int));
  sc->thread_id = 0;
#endif
  
  sc->code = sc->NIL;
  sc->args = sc->NIL;
  sc->value = sc->NIL;
  
  sc->global_env = s7_immutable_cons(sc, s7_make_vector(sc, SYMBOL_TABLE_SIZE), sc->NIL);
  sc->envir = sc->global_env;
  
  x = s7_make_symbol(sc, "else");
  add_to_current_environment(sc, x, sc->T); 
  
  x = s7_make_vector(sc, OP_MAX_DEFINED + 1);
  typeflag(x) |= (T_CONSTANT | T_DONT_COPY);
  sc->small_ints = x->object.vector.elements;
  
  for(i = 0; i < OP_MAX_DEFINED; i++) 
    {
      s7_pointer p;
      p = (s7_cell *)calloc(1, sizeof(s7_cell));
      p->flag = T_OBJECT | T_IMMUTABLE | T_ATOM | T_NUMBER | T_CONSTANT | T_SIMPLE | T_DONT_COPY;
      p->object.number.type = NUM_INT;
      integer(p->object.number) = (s7_Int)i;
      sc->small_ints[i] = p;
    }
  
  /* initialization of global pointers to special symbols */
  assign_syntax(sc, "lambda",          OP_LAMBDA);
  assign_syntax(sc, "lambda*",         OP_LAMBDA_STAR);
  assign_syntax(sc, "quote",           OP_QUOTE);
  assign_syntax(sc, "define",          OP_DEFINE0);
  assign_syntax(sc, "define*",         OP_DEFINE_STAR);
  assign_syntax(sc, "def-optkey-fun",  OP_DEFINE_STAR); /* for CLM */
  assign_syntax(sc, "define-constant", OP_DEFINE_CONSTANT0);
  assign_syntax(sc, "if",              OP_IF0);
  assign_syntax(sc, "begin",           OP_BEGIN);
  assign_syntax(sc, "set!",            OP_SET0);
  assign_syntax(sc, "let",             OP_LET0);
  assign_syntax(sc, "let*",            OP_LET_STAR0);
  assign_syntax(sc, "letrec",          OP_LETREC0);
  assign_syntax(sc, "cond",            OP_COND0);
  assign_syntax(sc, "make-promise",    OP_MAKE_PROMISE);
  assign_syntax(sc, "and",             OP_AND0);
  assign_syntax(sc, "or",              OP_OR0);
  assign_syntax(sc, "cons-stream",     OP_CONS_STREAM0); 
  assign_syntax(sc, "macro",           OP_MACRO0);
  assign_syntax(sc, "case",            OP_CASE0);
  assign_syntax(sc, "defmacro",        OP_DEFMACRO);
  assign_syntax(sc, "define-macro",    OP_DEFINE_MACRO);
  assign_syntax(sc, "do",              OP_DO);
  
  
  sc->LAMBDA = s7_make_symbol(sc, "lambda");
  typeflag(sc->LAMBDA) |= (T_IMMUTABLE | T_CONSTANT | T_DONT_COPY); 
  
  sc->LAMBDA_STAR = s7_make_symbol(sc, "lambda*");
  typeflag(sc->LAMBDA_STAR) |= (T_IMMUTABLE | T_CONSTANT | T_DONT_COPY); 
  
  sc->QUOTE = s7_make_symbol(sc, "quote");
  typeflag(sc->QUOTE) |= (T_IMMUTABLE | T_CONSTANT | T_DONT_COPY); 
  
  sc->QUASIQUOTE = s7_make_symbol(sc, "quasiquote");
  typeflag(sc->QUASIQUOTE) |= (T_IMMUTABLE | T_CONSTANT | T_DONT_COPY); 
  
  sc->UNQUOTE = s7_make_symbol(sc, "unquote");
  typeflag(sc->UNQUOTE) |= (T_IMMUTABLE | T_CONSTANT | T_DONT_COPY); 
  
  sc->UNQUOTE_SPLICING = s7_make_symbol(sc, "unquote-splicing");
  typeflag(sc->UNQUOTE_SPLICING) |= (T_IMMUTABLE | T_CONSTANT | T_DONT_COPY); 
  
  sc->FEED_TO = s7_make_symbol(sc, "=>");
  typeflag(sc->FEED_TO) |= (T_IMMUTABLE | T_CONSTANT | T_DONT_COPY); 
  
  #define set_object_name "(generalized set!)"
  sc->SET_OBJECT = s7_make_symbol(sc, set_object_name);
  typeflag(sc->SET_OBJECT) |= (T_CONSTANT | T_DONT_COPY); 
  
  /* save us the symbol table lookups later */
  
  sc->APPLY = s7_make_symbol(sc, "apply");
  typeflag(sc->APPLY) |= (T_CONSTANT | T_DONT_COPY); 
  
  sc->CONS = s7_make_symbol(sc, "cons");
  typeflag(sc->CONS) |= (T_CONSTANT | T_DONT_COPY); 
  
  sc->APPEND = s7_make_symbol(sc, "append");
  typeflag(sc->APPEND) |= (T_CONSTANT | T_DONT_COPY); 
  
  sc->CDR = s7_make_symbol(sc, "cdr");
  typeflag(sc->CDR) |= (T_CONSTANT | T_DONT_COPY); 
  
  sc->VECTOR = s7_make_symbol(sc, "vector");
  typeflag(sc->VECTOR) |= (T_CONSTANT | T_DONT_COPY); 
  sc->VECTOR_FUNCTION = s7_name_to_value(sc, "vector");
  
  sc->VALUES = s7_make_symbol(sc, "values");
  typeflag(sc->VALUES) |= (T_CONSTANT | T_DONT_COPY); 

  sc->ERROR = s7_make_symbol(sc, "error");
  typeflag(sc->ERROR) |= (T_CONSTANT | T_DONT_COPY); 

  sc->WRONG_TYPE_ARG = s7_make_symbol(sc, "wrong-type-arg");
  typeflag(sc->WRONG_TYPE_ARG) |= (T_CONSTANT | T_DONT_COPY); 

  sc->FORMAT_ERROR = s7_make_symbol(sc, "format-error");
  typeflag(sc->FORMAT_ERROR) |= (T_CONSTANT | T_DONT_COPY); 

  sc->OUT_OF_RANGE = s7_make_symbol(sc, "out-of-range");
  typeflag(sc->OUT_OF_RANGE) |= (T_CONSTANT | T_DONT_COPY); 

  sc->KEY_KEY = s7_make_keyword(sc, "key");
  typeflag(sc->KEY_KEY) |= (T_CONSTANT | T_DONT_COPY); 
  
  sc->KEY_OPTIONAL = s7_make_keyword(sc, "optional");
  typeflag(sc->KEY_OPTIONAL) |= (T_CONSTANT | T_DONT_COPY); 
  
  sc->KEY_REST = s7_make_keyword(sc, "rest");
  typeflag(sc->KEY_REST) |= (T_CONSTANT | T_DONT_COPY); 
  
  /* symbols */
  s7_define_function(sc, "gensym",                  g_gensym,                  0, 1, false, H_gensym);
  s7_define_function(sc, "symbol-table",            g_symbol_table,            0, 0, false, H_symbol_table);
  s7_define_function(sc, "symbol?",                 g_is_symbol,               1, 0, false, H_is_symbol);
  s7_define_function(sc, "symbol->string",          g_symbol_to_string,        1, 0, false, H_symbol_to_string);
  s7_define_function(sc, "string->symbol",          g_string_to_symbol,        1, 0, false, H_string_to_symbol);
  s7_define_function(sc, "symbol->value",           g_symbol_to_value,         1, 0, false, H_symbol_to_value);
  
  s7_define_function(sc, "global-environment",      g_global_environment,      0, 0, false, H_global_environment);
  s7_define_function(sc, "current-environment",     g_current_environment,     0, 1, false, H_current_environment);
  s7_define_function(sc, "provided?",               g_is_provided,             1, 0, false, H_is_provided);
  s7_define_function(sc, "provide",                 g_provide,                 1, 0, false, H_provide);
  s7_define_function(sc, "defined?",                g_is_defined,              1, 1, false, H_is_defined);
  
  s7_define_function(sc, "keyword?",                g_is_keyword,              1, 0, false, H_is_keyword);
  s7_define_function(sc, "make-keyword",            g_make_keyword,            1, 0, false, H_make_keyword);
  s7_define_function(sc, "symbol->keyword",         g_symbol_to_keyword,       1, 0, false, H_symbol_to_keyword);
  s7_define_function(sc, "keyword->symbol",         g_keyword_to_symbol,       1, 0, false, H_keyword_to_symbol);
  

  /* hash-tables */
  s7_define_function(sc, "hash-table?",             g_is_hash_table,           1, 0, false, H_is_hash_table);
  s7_define_function(sc, "make-hash-table",         g_make_hash_table,         0, 1, false, H_make_hash_table);
  s7_define_function(sc, "hash-table-ref",          g_hash_table_ref,          2, 0, false, H_hash_table_ref);
  s7_define_function(sc, "hash-table-set!",         g_hash_table_set,          3, 0, false, H_hash_table_set);
  s7_define_function(sc, "hash-table-size",         g_hash_table_size,         1, 0, false, H_hash_table_size);
  
  
  /* ports */
  s7_define_function(sc, "port-line-number",        g_port_line_number,        1, 0, false, H_port_line_number);
  s7_define_function(sc, "port-filename",           g_port_filename,           1, 0, false, H_port_filename);
  
  s7_define_function(sc, "input-port?",             g_is_input_port,           1, 0, false, H_is_input_port);
  s7_define_function(sc, "output-port?",            g_is_output_port,          1, 0, false, H_is_output_port);
  s7_define_function(sc, "char-ready?",             g_is_char_ready,           0, 1, false, H_is_char_ready);
  s7_define_function(sc, "eof-object?",             g_is_eof_object,           1, 0, false, H_is_eof_object);
  
  s7_define_function(sc, "current-input-port",      g_current_input_port,      0, 0, false, H_current_input_port);
  s7_define_function(sc, "set-current-input-port",  g_set_current_input_port,  1, 0, false, H_set_current_input_port);
  s7_define_function(sc, "current-output-port",     g_current_output_port,     0, 0, false, H_current_output_port);
  s7_define_function(sc, "set-current-output-port", g_set_current_output_port, 1, 0, false, H_set_current_output_port);
  s7_define_function(sc, "current-error-port",      g_current_error_port,      0, 0, false, H_current_error_port);
  s7_define_function(sc, "set-current-error-port",  g_set_current_error_port,  1, 0, false, H_set_current_error_port);
  s7_define_function(sc, "close-input-port",        g_close_input_port,        1, 0, false, H_close_input_port);
  s7_define_function(sc, "close-output-port",       g_close_output_port,       1, 0, false, H_close_output_port);
  s7_define_function(sc, "open-input-file",         g_open_input_file,         1, 1, false, H_open_input_file);
  s7_define_function(sc, "open-output-file",        g_open_output_file,        1, 1, false, H_open_output_file);
  s7_define_function(sc, "open-input-string",       g_open_input_string,       1, 0, false, H_open_input_string);
  s7_define_function(sc, "open-output-string",      g_open_output_string,      0, 0, false, H_open_output_string);
  s7_define_function(sc, "get-output-string",       g_get_output_string,       1, 0, false, H_get_output_string);
  
  s7_define_function(sc, "read-char",               g_read_char,               0, 1, false, H_read_char);
  s7_define_function(sc, "peek-char",               g_peek_char,               0, 1, false, H_peek_char);
  s7_define_function(sc, "read",                    g_read,                    0, 1, false, H_read);
  s7_define_function(sc, "newline",                 g_newline,                 0, 1, false, H_newline);
  s7_define_function(sc, "write-char",              g_write_char,              1, 1, false, H_write_char);
  s7_define_function(sc, "write",                   g_write,                   1, 1, false, H_write);
  s7_define_function(sc, "display",                 g_display,                 1, 1, false, H_display);
  s7_define_function(sc, "read-byte",               g_read_byte,               0, 1, false, H_read_byte);
  s7_define_function(sc, "write-byte",              g_write_byte,              1, 1, false, H_write_byte);
#if WITH_READ_LINE
  s7_define_function(sc, "read-line",               g_read_line,               0, 0, true,  H_read_line);
#endif
  
  s7_define_function(sc, "call-with-input-string",  g_call_with_input_string,  2, 0, false, H_call_with_input_string);
  s7_define_function(sc, "call-with-input-file",    g_call_with_input_file,    2, 0, false, H_call_with_input_file);
  s7_define_function(sc, "with-input-from-string",  g_with_input_from_string,  2, 0, false, H_with_input_from_string);
  s7_define_function(sc, "with-input-from-file",    g_with_input_from_file,    2, 0, false, H_with_input_from_file);
  
  s7_define_function(sc, "call-with-output-string", g_call_with_output_string, 1, 0, false, H_call_with_output_string);
  s7_define_function(sc, "call-with-output-file",   g_call_with_output_file,   2, 0, false, H_call_with_output_file);
  s7_define_function(sc, "with-output-to-string",   g_with_output_to_string,   1, 0, false, H_with_output_to_string);
  s7_define_function(sc, "with-output-to-file",     g_with_output_to_file,     2, 0, false, H_with_output_to_file);
  
  
  /* numbers */
  s7_define_function(sc, "number->string",          g_number_to_string,        1, 2, false, H_number_to_string);
  s7_define_function(sc, "string->number",          g_string_to_number,        1, 2, false, H_string_to_number);
  s7_define_function(sc, "make-polar",              g_make_polar,              2, 0, false, H_make_polar);
  s7_define_function(sc, "make-rectangular",        g_make_rectangular,        2, 0, false, H_make_rectangular);
  s7_define_function(sc, "magnitude",               g_magnitude,               1, 0, false, H_magnitude);
  s7_define_function(sc, "angle",                   g_angle,                   1, 0, false, H_angle);
  s7_define_function(sc, "rationalize",             g_rationalize,             2, 0, false, H_rationalize);
  s7_define_function(sc, "abs",                     g_abs,                     1, 0, false, H_abs);
  s7_define_function(sc, "exp",                     g_exp,                     1, 0, false, H_exp);
  s7_define_function(sc, "log",                     g_log,                     1, 1, false, H_log);
  s7_define_function(sc, "sin",                     g_sin,                     1, 0, false, H_sin);
  s7_define_function(sc, "cos",                     g_cos,                     1, 0, false, H_cos);
  s7_define_function(sc, "tan",                     g_tan,                     1, 0, false, H_tan);
  s7_define_function(sc, "asin",                    g_asin,                    1, 0, false, H_asin);
  s7_define_function(sc, "acos",                    g_acos,                    1, 0, false, H_acos);
  s7_define_function(sc, "atan",                    g_atan,                    1, 1, false, H_atan);
  s7_define_function(sc, "sinh",                    g_sinh,                    1, 0, false, H_sinh);
  s7_define_function(sc, "cosh",                    g_cosh,                    1, 0, false, H_cosh);
  s7_define_function(sc, "tanh",                    g_tanh,                    1, 0, false, H_tanh);
  s7_define_function(sc, "asinh",                   g_asinh,                   1, 0, false, H_asinh);
  s7_define_function(sc, "acosh",                   g_acosh,                   1, 0, false, H_acosh);
  s7_define_function(sc, "atanh",                   g_atanh,                   1, 0, false, H_atanh);
  s7_define_function(sc, "sqrt",                    g_sqrt,                    1, 0, false, H_sqrt);
  s7_define_function(sc, "expt",                    g_expt,                    2, 0, false, H_expt);
  s7_define_function(sc, "floor",                   g_floor,                   1, 0, false, H_floor);
  s7_define_function(sc, "ceiling",                 g_ceiling,                 1, 0, false, H_ceiling);
  s7_define_function(sc, "truncate",                g_truncate,                1, 0, false, H_truncate);
  s7_define_function(sc, "round",                   g_round,                   1, 0, false, H_round);
  s7_define_function(sc, "lcm",                     g_lcm,                     0, 0, true,  H_lcm);
  s7_define_function(sc, "gcd",                     g_gcd,                     0, 0, true,  H_gcd);
  s7_define_function(sc, "+",                       g_add,                     0, 0, true,  H_add);
  s7_define_function(sc, "-",                       g_subtract,                1, 0, true,  H_subtract);
  s7_define_function(sc, "*",                       g_multiply,                0, 0, true,  H_multiply);
  s7_define_function(sc, "/",                       g_divide,                  1, 0, true,  H_divide);
  s7_define_function(sc, "max",                     g_max,                     1, 0, true,  H_max);
  s7_define_function(sc, "min",                     g_min,                     1, 0, true,  H_min);
  s7_define_function(sc, "quotient",                g_quotient,                2, 0, false, H_quotient);
  s7_define_function(sc, "remainder",               g_remainder,               2, 0, false, H_remainder);
  s7_define_function(sc, "modulo",                  g_modulo,                  2, 0, false, H_modulo);
  s7_define_function(sc, "=",                       g_equal,                   2, 0, true,  H_equal);
  s7_define_function(sc, "<",                       g_less,                    2, 0, true,  H_less);
  s7_define_function(sc, ">",                       g_greater,                 2, 0, true,  H_greater);
  s7_define_function(sc, "<=",                      g_less_or_equal,           2, 0, true,  H_less_or_equal);
  s7_define_function(sc, ">=",                      g_greater_or_equal,        2, 0, true,  H_greater_or_equal);
  s7_define_function(sc, "number?",                 g_is_number,               1, 0, false, H_is_number);
  s7_define_function(sc, "integer?",                g_is_integer,              1, 0, false, H_is_integer);
  s7_define_function(sc, "real?",                   g_is_real,                 1, 0, false, H_is_real);
  s7_define_function(sc, "complex?",                g_is_complex,              1, 0, false, H_is_complex);
  s7_define_function(sc, "rational?",               g_is_rational,             1, 0, false, H_is_rational);
  s7_define_function(sc, "even?",                   g_is_even,                 1, 0, false, H_is_even);
  s7_define_function(sc, "odd?",                    g_is_odd,                  1, 0, false, H_is_odd);
  s7_define_function(sc, "zero?",                   g_is_zero,                 1, 0, false, H_is_zero);
  s7_define_function(sc, "positive?",               g_is_positive,             1, 0, false, H_is_positive);
  s7_define_function(sc, "negative?",               g_is_negative,             1, 0, false, H_is_negative);
  s7_define_function(sc, "real-part",               g_real_part,               1, 0, false, H_real_part);
  s7_define_function(sc, "imag-part",               g_imag_part,               1, 0, false, H_imag_part);
  s7_define_function(sc, "numerator",               g_numerator,               1, 0, false, H_numerator);
  s7_define_function(sc, "denominator",             g_denominator,             1, 0, false, H_denominator);
  s7_define_function(sc, "inexact->exact",          g_inexact_to_exact,        1, 0, false, H_inexact_to_exact);
  s7_define_function(sc, "exact->inexact",          g_exact_to_inexact,        1, 0, false, H_exact_to_inexact);
  s7_define_function(sc, "exact?",                  g_is_exact,                1, 0, false, H_is_exact);
  s7_define_function(sc, "inexact?",                g_is_inexact,              1, 0, false, H_is_inexact);
  s7_define_function(sc, "logior",                  g_logior,                  1, 0, true,  H_logior);
  s7_define_function(sc, "logxor",                  g_logxor,                  1, 0, true,  H_logxor);
  s7_define_function(sc, "logand",                  g_logand,                  1, 0, true,  H_logand);
  s7_define_function(sc, "lognot",                  g_lognot,                  1, 0, false, H_lognot);
  s7_define_function(sc, "ash",                     g_ash,                     2, 0, false, H_ash);
  
  
  /* chars */
  s7_define_function(sc, "char-upcase",             g_char_upcase,             1, 0, false, H_char_upcase);
  s7_define_function(sc, "char-downcase",           g_char_downcase,           1, 0, false, H_char_downcase);
  s7_define_function(sc, "char->integer",           g_char_to_integer,         1, 0, false, H_char_to_integer);
  s7_define_function(sc, "integer->char",           g_integer_to_char,         1, 0, false, H_integer_to_char);
  
  s7_define_function(sc, "char-upper-case?",        g_is_char_upper_case,      1, 0, false, H_is_char_upper_case);
  s7_define_function(sc, "char-lower-case?",        g_is_char_lower_case,      1, 0, false, H_is_char_lower_case);
  s7_define_function(sc, "char-alphabetic?",        g_is_char_alphabetic,      1, 0, false, H_is_char_alphabetic);
  s7_define_function(sc, "char-numeric?",           g_is_char_numeric,         1, 0, false, H_is_char_numeric);
  s7_define_function(sc, "char-whitespace?",        g_is_char_whitespace,      1, 0, false, H_is_char_whitespace);
  s7_define_function(sc, "char?",                   g_is_char,                 1, 0, false, H_is_char);
  
  s7_define_function(sc, "char=?",                  g_chars_are_equal,         2, 0, true,  H_chars_are_equal);
  s7_define_function(sc, "char<?",                  g_chars_are_less,          2, 0, true,  H_chars_are_less);
  s7_define_function(sc, "char>?",                  g_chars_are_greater,       2, 0, true,  H_chars_are_greater);
  s7_define_function(sc, "char<=?",                 g_chars_are_leq,           2, 0, true,  H_chars_are_leq);
  s7_define_function(sc, "char>=?",                 g_chars_are_geq,           2, 0, true,  H_chars_are_geq);
  s7_define_function(sc, "char-ci=?",               g_chars_are_ci_equal,      2, 0, true,  H_chars_are_ci_equal);
  s7_define_function(sc, "char-ci<?",               g_chars_are_ci_less,       2, 0, true,  H_chars_are_ci_less);
  s7_define_function(sc, "char-ci>?",               g_chars_are_ci_greater,    2, 0, true,  H_chars_are_ci_greater);
  s7_define_function(sc, "char-ci<=?",              g_chars_are_ci_leq,        2, 0, true,  H_chars_are_ci_leq);
  s7_define_function(sc, "char-ci>=?",              g_chars_are_ci_geq,        2, 0, true,  H_chars_are_ci_geq);
  
  
  /* strings */
  s7_define_function(sc, "string?",                 g_is_string,               1, 0, false, H_is_string);
  s7_define_function(sc, "make-string",             g_make_string,             1, 1, false, H_make_string);
  s7_define_function(sc, "string-length",           g_string_length,           1, 0, false, H_string_length);
  s7_define_function(sc, "string-ref",              g_string_ref,              2, 0, false, H_string_ref);
  s7_define_function(sc, "string-set!",             g_string_set,              3, 0, false, H_string_set);
  
  s7_define_function(sc, "string=?",                g_strings_are_equal,       2, 0, true,  H_strings_are_equal);
  s7_define_function(sc, "string<?",                g_strings_are_less,        2, 0, true,  H_strings_are_less);
  s7_define_function(sc, "string>?",                g_strings_are_greater,     2, 0, true,  H_strings_are_greater);
  s7_define_function(sc, "string<=?",               g_strings_are_leq,         2, 0, true,  H_strings_are_leq);
  s7_define_function(sc, "string>=?",               g_strings_are_geq,         2, 0, true,  H_strings_are_geq);
  s7_define_function(sc, "string-ci=?",             g_strings_are_ci_equal,    2, 0, true,  H_strings_are_ci_equal);
  s7_define_function(sc, "string-ci<?",             g_strings_are_ci_less,     2, 0, true,  H_strings_are_ci_less);
  s7_define_function(sc, "string-ci>?",             g_strings_are_ci_greater,  2, 0, true,  H_strings_are_ci_greater);
  s7_define_function(sc, "string-ci<=?",            g_strings_are_ci_leq,      2, 0, true,  H_strings_are_ci_leq);
  s7_define_function(sc, "string-ci>=?",            g_strings_are_ci_geq,      2, 0, true,  H_strings_are_ci_geq);
  
  s7_define_function(sc, "string-append",           g_string_append,           0, 0, true,  H_string_append);
  s7_define_function(sc, "string-fill!",            g_string_fill,             2, 0, false, H_string_fill);
  s7_define_function(sc, "string-copy",             g_string_copy,             1, 0, false, H_string_copy);
  s7_define_function(sc, "substring",               g_substring,               2, 1, false, H_substring);
  s7_define_function(sc, "string",                  g_string,                  0, 0, true,  H_string);
  s7_define_function(sc, "list->string",            g_list_to_string,          1, 0, false, H_list_to_string);
  s7_define_function(sc, "string->list",            g_string_to_list,          1, 0, false, H_string_to_list);
  s7_define_function(sc, "object->string",          g_object_to_string,        1, 0, false, H_object_to_string);
#if WITH_FORMAT
  s7_define_function(sc, "format",                  g_format,                  1, 0, true,  H_format);
#endif  
  
  /* lists */
  s7_define_function(sc, "null?",                   g_is_null,                 1, 0, false, H_is_null);
  s7_define_function(sc, "list?",                   g_is_list,                 1, 0, false, H_is_list);
  s7_define_function(sc, "pair?",                   g_is_pair,                 1, 0, false, H_is_pair);
  s7_define_function(sc, "reverse",                 g_reverse,                 1, 0, false, H_reverse);
  s7_define_function(sc, "reverse!",                g_reverse_in_place,        1, 0, false, H_reverse_in_place); /* used by Snd code */
  s7_define_function(sc, "cons",                    g_cons,                    2, 0, false, H_cons);
  s7_define_function(sc, "car",                     g_car,                     1, 0, false, H_car);
  s7_define_function(sc, "cdr",                     g_cdr,                     1, 0, false, H_cdr);
  s7_define_function(sc, "set-car!",                g_set_car,                 2, 0, false, H_set_car);
  s7_define_function(sc, "set-cdr!",                g_set_cdr,                 2, 0, false, H_set_cdr);
  s7_define_function(sc, "caar",                    g_caar,                    1, 0, false, H_caar);
  s7_define_function(sc, "cadr",                    g_cadr,                    1, 0, false, H_cadr);
  s7_define_function(sc, "cdar",                    g_cdar,                    1, 0, false, H_cdar);
  s7_define_function(sc, "cddr",                    g_cddr,                    1, 0, false, H_cddr);
  s7_define_function(sc, "caaar",                   g_caaar,                   1, 0, false, H_caaar);
  s7_define_function(sc, "caadr",                   g_caadr,                   1, 0, false, H_caadr);
  s7_define_function(sc, "cadar",                   g_cadar,                   1, 0, false, H_cadar);
  s7_define_function(sc, "cdaar",                   g_cdaar,                   1, 0, false, H_cdaar);
  s7_define_function(sc, "caddr",                   g_caddr,                   1, 0, false, H_caddr);
  s7_define_function(sc, "cdddr",                   g_cdddr,                   1, 0, false, H_cdddr);
  s7_define_function(sc, "cdadr",                   g_cdadr,                   1, 0, false, H_cdadr);
  s7_define_function(sc, "cddar",                   g_cddar,                   1, 0, false, H_cddar);
  s7_define_function(sc, "caaaar",                  g_caaaar,                  1, 0, false, H_caaaar);
  s7_define_function(sc, "caaadr",                  g_caaadr,                  1, 0, false, H_caaadr);
  s7_define_function(sc, "caadar",                  g_caadar,                  1, 0, false, H_caadar);
  s7_define_function(sc, "cadaar",                  g_cadaar,                  1, 0, false, H_cadaar);
  s7_define_function(sc, "caaddr",                  g_caaddr,                  1, 0, false, H_caaddr);
  s7_define_function(sc, "cadddr",                  g_cadddr,                  1, 0, false, H_cadddr);
  s7_define_function(sc, "cadadr",                  g_cadadr,                  1, 0, false, H_cadadr);
  s7_define_function(sc, "caddar",                  g_caddar,                  1, 0, false, H_caddar);
  s7_define_function(sc, "cdaaar",                  g_cdaaar,                  1, 0, false, H_cdaaar);
  s7_define_function(sc, "cdaadr",                  g_cdaadr,                  1, 0, false, H_cdaadr);
  s7_define_function(sc, "cdadar",                  g_cdadar,                  1, 0, false, H_cdadar);
  s7_define_function(sc, "cddaar",                  g_cddaar,                  1, 0, false, H_cddaar);
  s7_define_function(sc, "cdaddr",                  g_cdaddr,                  1, 0, false, H_cdaddr);
  s7_define_function(sc, "cddddr",                  g_cddddr,                  1, 0, false, H_cddddr);
  s7_define_function(sc, "cddadr",                  g_cddadr,                  1, 0, false, H_cddadr);
  s7_define_function(sc, "cdddar",                  g_cdddar,                  1, 0, false, H_cdddar);
  s7_define_function(sc, "length",                  g_length,                  1, 0, false, H_length);
  s7_define_function(sc, "assq",                    g_assq,                    2, 0, false, H_assq);
  s7_define_function(sc, "assv",                    g_assv,                    2, 0, false, H_assv);
  s7_define_function(sc, "assoc",                   g_assoc,                   2, 0, false, H_assoc);
  s7_define_function(sc, "memq",                    g_memq,                    2, 0, false, H_memq);
  s7_define_function(sc, "memv",                    g_memv,                    2, 0, false, H_memv);
  s7_define_function(sc, "member",                  g_member,                  2, 0, false, H_member);
  s7_define_function(sc, "append",                  g_append,                  0, 0, true,  H_append);
  s7_define_function(sc, "list",                    g_list,                    0, 0, true,  H_list);
  s7_define_function(sc, "list-ref",                g_list_ref,                2, 0, false, H_list_ref);
  s7_define_function(sc, "list-set!",               g_list_set,                3, 0, false, H_list_set);
  s7_define_function(sc, "list-tail",               g_list_tail,               2, 0, false, H_list_tail);
  s7_define_function(sc, "list-line-number",        g_list_line_number,        1, 0, false, H_list_line_number);
  
  
  /* vectors */
  s7_define_function(sc, "vector?",                 g_is_vector,               1, 0, false, H_is_vector);
  s7_define_function(sc, "vector->list",            g_vector_to_list,          1, 0, false, H_vector_to_list);
  s7_define_function(sc, "list->vector",            g_list_to_vector,          1, 0, false, H_list_to_vector);
  s7_define_function(sc, "vector-fill!",            g_vector_fill,             2, 0, false, H_vector_fill);
  s7_define_function(sc, "vector",                  g_vector,                  0, 0, true,  H_vector);
  s7_define_function(sc, "vector-length",           g_vector_length,           1, 0, false, H_vector_length);
  s7_define_function(sc, "vector-ref",              g_vector_ref,              2, 0, false, H_vector_ref);
  s7_define_function(sc, "vector-set!",             g_vector_set,              3, 0, false, H_vector_set);
  s7_define_function(sc, "make-vector",             g_make_vector,             1, 1, false, H_make_vector);
  
  
  s7_define_function(sc, "call/cc",                 g_call_cc,                 1, 0, false, H_call_cc);
  s7_define_function(sc, "call-with-current-continuation", g_call_cc,          1, 0, false, H_call_cc);
  s7_define_function(sc, "call-with-exit",          g_call_with_exit,          1, 0, false, H_call_with_exit);
  s7_define_function(sc, "continuation?",           g_is_continuation,         1, 0, false, H_is_continuation);

  s7_define_function(sc, "load",                    g_load,                    1, 0, false, H_display);
  s7_define_function(sc, "eval",                    g_eval,                    1, 1, false, H_eval);
  s7_define_function(sc, "eval-string",             g_eval_string,             1, 0, false, H_eval_string);
  s7_define_function(sc, "apply",                   g_apply,                   1, 0, true,  H_apply);
  s7_define_function(sc, "force",                   g_force,                   1, 0, false, H_force);

  s7_define_function(sc, "for-each",                g_for_each,                2, 0, true,  H_for_each);
  s7_define_function(sc, "map",                     g_map,                     2, 0, true,  H_map);

  s7_define_function(sc, "values",                  g_values,                  0, 0, true,  H_values);
  s7_define_function(sc, "call-with-values",        g_call_with_values,        2, 0, false, H_call_with_values);

  s7_define_function(sc, "dynamic-wind",            g_dynamic_wind,            3, 0, false, H_dynamic_wind);
  s7_define_function(sc, "catch",                   g_catch,                   3, 0, false, H_catch);
  s7_define_function(sc, "error",                   g_error,                   0, 0, true,  H_error);
  
  s7_define_function(sc, "gc-verbose",              g_gc_verbose,              1, 0, false, H_gc_verbose);
  s7_define_function(sc, "load-verbose",            g_load_verbose,            1, 0, false, H_load_verbose);
  s7_define_function(sc, "backtrace",               g_backtrace,               0, 0, false, H_backtrace);
  s7_define_function(sc, "clear-backtrace",         g_clear_backtrace,         0, 0, false, H_clear_backtrace);
  s7_define_function(sc, "set-backtrace-length",    g_set_backtrace_length,    1, 0, false, H_set_backtrace_length);
  s7_define_function(sc, "gc",                      g_gc,                      0, 1, false, H_gc);
  s7_define_function(sc, "quit",                    g_quit,                    0, 0, false, H_quit);

  s7_define_function(sc, "procedure?",              g_is_procedure,            1, 0, false, H_is_procedure);
  s7_define_function(sc, "procedure-documentation", g_procedure_documentation, 1, 0, false, H_procedure_documentation);
  s7_define_function(sc, "help",                    g_procedure_documentation, 1, 0, false, H_procedure_documentation);
  s7_define_function(sc, "procedure-arity",         g_procedure_arity,         1, 0, false, H_procedure_arity);
  s7_define_function(sc, "procedure-source",        g_procedure_source,        1, 0, false, H_procedure_source);
  
  s7_define_function(sc, "make-procedure-with-setter",         g_make_procedure_with_setter,         2, 0, false, "...");
  s7_define_function(sc, "procedure-with-setter?",             g_is_procedure_with_setter,           1, 0, false, H_is_procedure_with_setter);
  s7_define_function(sc, "procedure-with-setter-setter-arity", g_procedure_with_setter_setter_arity, 1, 0, false, "kludge to get setter's arity");
  pws_tag = s7_new_type("<procedure-with-setter>", pws_print, pws_free,	pws_equal, pws_mark, pws_apply,	pws_set);
  
  
  s7_define_function(sc, "not",                     g_not,                     1, 0, false, H_not);
  s7_define_function(sc, "boolean?",                g_is_boolean,              1, 0, false, H_is_boolean);
  s7_define_function(sc, "eq?",                     g_is_eq,                   2, 0, false, H_is_eq);
  s7_define_function(sc, "eqv?",                    g_is_eqv,                  2, 0, false, H_is_eqv);
  s7_define_function(sc, "equal?",                  g_is_equal,                2, 0, false, H_is_equal);
  
  s7_define_function(sc, "scheme-implementation",   g_scheme_implementation,   0, 0, false, H_scheme_implementation);
  s7_define_function(sc, set_object_name,           g_set_object,              1, 0, true, "internal setter redirection");
  s7_define_function(sc, "_quasiquote_",            g_quasiquote,              2, 0, false, "internal quasiquote handler");
  
  s7_define_variable(sc, "*features*", sc->NIL);
  s7_define_variable(sc, "*load-path*", sc->NIL);
  s7_define_variable(sc, "*vector-print-length*", sc->small_ints[8]);
  
  g_provide(sc, make_list_1(sc, s7_make_symbol(sc, "s7")));


#if HAVE_PTHREADS
  thread_tag = s7_new_type("<thread>",          thread_print, thread_free, thread_equal, thread_mark, NULL, NULL);
  lock_tag =   s7_new_type("<lock>",            lock_print,   lock_free,   lock_equal,   NULL,        NULL, NULL);
  key_tag =    s7_new_type("<thread-variable>", key_print,    key_free,    key_equal,    NULL,        get_key, set_key);

  s7_define_function(sc, "make-thread",             g_make_thread,             1, 0, false, H_make_thread);
  s7_define_function(sc, "join-thread",             g_join_thread,             1, 0, false, H_join_thread);
  s7_define_function(sc, "thread?",                 g_is_thread,               1, 0, false, H_is_thread);

  s7_define_function(sc, "make-lock",               g_make_lock,               0, 0, false, H_make_lock); /* "mutex" is ugly (and opaque) jargon */
  s7_define_function(sc, "grab-lock",               g_grab_lock,               1, 0, false, H_grab_lock);
  s7_define_function(sc, "release-lock",            g_release_lock,            1, 0, false, H_release_lock);
  s7_define_function(sc, "lock?",                   g_is_lock,                 1, 0, false, H_is_lock);

  s7_define_function(sc, "make-thread-variable",    g_make_thread_variable,    0, 0, false, H_make_thread_variable);
  s7_define_function(sc, "thread-variable?",        g_is_thread_variable,      1, 0, false, H_is_thread_variable);

  g_provide(sc, make_list_1(sc, s7_make_symbol(sc, "threads")));
#endif

#if S7_DEBUGGING
  s7_define_function(sc, "builtin-hash", g_builtin_hash, 1, 0, false, "built-in hash function");
#endif
  
  (*(sc->gc_off)) = false;
  
  s7_eval_c_string(sc, "(macro quasiquote (lambda (l) (_quasiquote_ 0 (cadr l))))");
  
  {
    int top;
#ifndef LLONG_MAX
    #define LLONG_MAX 9223372036854775807LL
    #define LLONG_MIN (-LLONG_MAX - 1LL)
#endif
    top = sizeof(s7_Int);
    s7_define_constant(sc, "most-positive-fixnum", s7_make_integer(sc, (top == 8) ? LLONG_MAX : ((top == 4) ? LONG_MAX : SHRT_MAX)));
    s7_define_constant(sc, "most-negative-fixnum", s7_make_integer(sc, (top == 8) ? LLONG_MIN : ((top == 4) ? LONG_MIN : SHRT_MIN)));
    if (top == 4) default_rationalize_error = 1.0e-6;
    s7_define_constant(sc, "pi", s7_make_real(sc, 3.1415926535897932384626433832795029L)); /* M_PI is not good enough for s7_Double = long double */
    top_log = floor(log(pow(2.0, (s7_Double)((top * 8) - 1))));

    /* for s7_Double, float gives about 9 digits, double 18, long Double claims 28 but I don't see more than about 22? */
  }
  return(sc);
}
