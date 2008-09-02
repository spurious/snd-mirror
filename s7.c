/* S7
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
 *        full continuations, call-with-exit for goto or return
 *        ratios and complex numbers (and ints are 64-bit)
 *        generalized set!, procedure-with-setter
 *        defmacro and define-macro, keywords, hash tables, block comments
 *        applicable objects
 *        error handling using error and catch
 *        in Snd, the run macro works giving S7 a (somewhat limited) byte compiler
 *        no invidious distinction between built-in and "foreign"
 *
 *   many minor changes!
 *
 *   deliberate omission from r5rs: 
 *        no inexact integer or ratio (so, for example, truncate returns an exact integer), no exact complex or exact real
 *
 *   deliberate difference from r5rs:
 *        modulo, remainder, and quotient take integer, ratio, or real args
 *        delay is renamed make-promise to avoid collisions in CLM
 *
 * still to do:
 *   dynamic-wind and its interaction with continuations
 *   threads: call-with-new-thread[s7?], join-thread
 *   there's no arg number mismatch check for caller-defined functions!
 *   help for vars (objects)
 *   figure out how add syntax-rules, for-each, map, do
 *   the rest of the call-with funcs (call-with-input-string is done)
 *   can any part of quasiquote be optimized?
 *
 *
 * define-type (make-type in ext)
 * (define-type "name"
 *              (lambda (obj) (display obj))       ; print
 *	        #f                                 ; free
 *	        (lambda (obj1 obj2) (= obj1 obj2)) ;equal?
 *	        #f                                 ; gc mark
 *	        (lambda (obj arg) arg)             ; apply
 *	        #f)                                ; set
 *  (make-object <tag> value)
 *  (object? <tag> obj) or maybe (name? obj) and (make-name value)
 *
 * Mike Scholz provided the FreeBSD support
 *
 * in snd-test: 9, 10, and 14 need sort, 8 gets abs of 0 but complex, 7 has rgb scope problem?
 *                with sort.scm (slib), 9 needs string-downcase
 *                                     10 is unhappy about all the key args to open-output-file (:if-does-not-exist)
 *                                     14 gets "trouble in region or something", then (+ <promise>???)
 *              5 hits a segfault in src-sound when trying to complain about an env hitting 0 (but there's no env??)
 *              0..4 are ok, 6 is ok, 12 is ok, 18 is ok
 *              13 has some problem with "no widgets added" -- 21 hits very similar problem with "add-comment failed" (in a list)
 *              15 seems to hit (= x (1 0)), 16 says apply last arg should be a list
 *              17 is still unhappy about progn, I think
 *              19 hits abort (pop off top of stack), 20 hits division by 0
 *              22 is unhappy about (lambda () (set! int-var (car pv))) -- car?
 *              23 find-sound arg1 is procedure-with-setter??
 *              24..27 run but I'm not sure they actually did anything
 *              28 hits a bunch of arity errors? segfaults in count-matches
 */


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
 *    error handlers
 *    sundry leftovers
 *    eval
 *    s7 init
 */


#define TIMING 0
/* 
 * test.scm, with 256k heap: mark: 1.5 secs, sweep: 10.5 secs, of total 65 secs
 *   the problem here is macro expansion -- if I use a function instead, .3 secs:
 *
 *     time guile -l test.scm
 *     0.900u 0.018s 0:00.92 98.9%     0+0k 0+0io 0pf+0w
 *     time snd test.scm
 *     0.362u 0.019s 0:00.40 92.5%     0+0k 0+0io 0pf+0w
 */

#define USE_SND 0
/* this is for memory leak and pointer debugging stuff */
#define S7_DEBUGGING 1
/* this is for a bunch of sanity checks */



/* -------------------------------------------------------------------------------- */

/* your config file goes here.  Currently we assume we have complex.h and setjmp.h,
 *   but it would be easy to put those on switches.  The only other compile-time
 *   flags involve the functions:
 *
 *     cabs cacos cacosh carg casin casinh catan catanh ccos ccosh cexp clog conj cpow csin csinh csqrt ctan ctanh
 *
 */

#include <mus-config.h>

/* -------------------------------------------------------------------------------- */



#define _FILE_OFFSET_BITS 64  /* off_t's -- I could use long long, but old habits die hard */

#include <unistd.h>
#include <math.h>
#include <limits.h>
#include <float.h>
#include <ctype.h>
#include <strings.h>
#include <string.h>
#include <stdlib.h>
#include <sys/types.h>
#include <complex.h>
#include <setjmp.h>


#if TIMING
#include <time.h>
#endif

#if USE_SND

  #define MUS_DEBUGGING 1
  #define copy_string(Str) copy_string_1(Str, __FUNCTION__, __FILE__, __LINE__)
  char *copy_string_1(const char *str, const char *func, const char *file, int line);
  #include "_sndlib.h"

#else

  #define copy_string(str) strdup(str)
  #define CALLOC(a, b)  calloc((size_t)(a), (size_t)(b))
  #define MALLOC(a)     malloc((size_t)(a))
  #define FREE(a)       free(a)
  #define REALLOC(a, b) realloc(a, (size_t)(b))

#endif

#include "s7.h"


#define INITIAL_HEAP_SIZE 256000
/* in Snd, there are about 10000 permanent objects sitting in the heap, so the bigger the size, the less
 *    often we mark these objects -- the GC is more efficient as the heap size increases.  Each object
 *    is about 20 bytes, so even 256K is nothing in modern memory sizes.  The heaps grows as needed,
 *    so almost any number is ok, but it has to bebig enough to handle the startup allocations
 *    when the gc is turned off.
 */

#define SYMBOL_TABLE_SIZE 9601
/* names are hashed into the symbol table (a vector) and collisions are chained as lists; was 461, then 4603.
 */

#define INITIAL_STACK_SIZE 1000            /* each frame takes 4 entries */

#define INITIAL_PROTECTED_OBJECTS_SIZE 16  /* a vector of objects that are being protected from the GC */



typedef enum {OP_TOP_LEVEL, OP_T1LVL, OP_READ_INTERNAL, OP_VALUEPRINT, OP_EVAL, OP_REAL_EVAL, 
	      OP_E0ARGS, OP_E1ARGS, OP_APPLY, OP_REAL_APPLY, OP_DOMACRO, OP_LAMBDA, OP_QUOTE, 
	      OP_DEF0, OP_DEF1, OP_BEGIN, OP_IF0, OP_IF1, OP_SET0, OP_SET1,
	      OP_LET0, OP_LET1, OP_LET2, OP_LET0AST, OP_LET1AST, OP_LET2AST, 
	      OP_LET0REC, OP_LET1REC, OP_LET2REC, OP_COND0, OP_COND1, OP_DELAY, OP_AND0, OP_AND1, 
	      OP_OR0, OP_OR1, OP_C0STREAM, OP_C1STREAM, OP_DEFMACRO, OP_MACRO0, OP_MACRO1, OP_DEFINE_MACRO,
	      OP_CASE0, OP_CASE1, OP_CASE2, OP_READ_EXPRESSION, OP_READ_LIST, OP_READ_DOT, OP_READ_QUOTE, 
	      OP_READ_QUASIQUOTE, OP_READ_QUASIQUOTE_VECTOR, OP_READ_UNQUOTE, OP_READ_UNQUOTE_SPLICING, OP_READ_VEC, OP_P0LIST, OP_P1LIST, 
	      OP_PVECFROM, OP_SAVE_FORCED, OP_READ_RETURN_EXPRESSION, 
	      OP_READ_POP_AND_RETURN_EXPRESSION, OP_LOAD_RETURN_IF_EOF, OP_LOAD_CLOSE_AND_POP_IF_EOF, 
	      OP_EVAL_STRING, OP_EVAL_STRING_DONE, OP_QUIT, OP_CATCH, OP_DYNAMIC_WIND, OP_MAX_DEFINED
} opcode_t;


static const char *opcode_names[OP_MAX_DEFINED] = {
              "op_top_level", "op_t1lvl", "op_read_internal", "op_valueprint", "op_eval", "op_real_eval", 
	      "op_e0args", "op_e1args", "op_apply", "op_real_apply", "op_domacro", "op_lambda", "op_quote", 
	      "op_def0", "op_def1", "op_begin", "op_if0", "op_if1", "op_set0", "op_set1",
	      "op_let0", "op_let1", "op_let2", "op_let0ast", "op_let1ast", "op_let2ast", 
	      "op_let0rec", "op_let1rec", "op_let2rec", "op_cond0", "op_cond1", "op_delay", "op_and0", "op_and1", 
	      "op_or0", "op_or1", "op_c0stream", "op_c1stream", "op_defmacro", "op_macro0", "op_macro1", "op_define_macro",
	      "op_case0", "op_case1", "op_case2", "op_read_expression", "op_rdlist", "op_rddot", "op_rdquote", 
	      "op_rdqquote", "op_rdqquotevec", "op_rdunquote", "op_rduqtsp", "op_rdvec", "op_p0list", "op_p1list", 
	      "op_pvecfrom", "op_save_forced", "op_read_return_expression", 
	      "op_read_pop_and_return_expression", "op_load_return_if_eof", "op_load_close_and_pop_if_eof", 
	      "op_eval_string", "op_eval_string_done", "op_quit", "op_catch", "op_dynamic_wind"
};



/* num, for generic arithmetic */
typedef struct num {
  char type;
  union {

    s7_Int ivalue;

    double rvalue;

    struct {
      s7_Int numerator;
      s7_Int denominator;
    } fvalue;

    struct {
      double real;
      double imag;
    } cvalue;

  } value;
} num;


typedef struct rport {
  bool is_closed;
  bool is_file;
  bool needs_close;
  int paren_depth;
  union {
    struct {
      FILE *file;
      int line_number;
      char *filename;
    } stdio;
    struct {
      char *value;
      int size, point;
    } string;
  } rep;
} rport;


typedef struct continuation {
  int cc_stack_size, cc_stack_top;
  s7_pointer cc_stack;
} continuation;


typedef struct ffunc {
  s7_function ff;
  const char *name;
  const char *doc;
  int required_args, optional_args;
  bool rest_arg;
} ffunc;


typedef struct rcatch {
  int goto_loc;
  s7_pointer tag;
  s7_pointer handler;
} rcatch;


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
    
  } object;
} s7_cell;


struct s7_scheme {
  bool tracing;
  bool gc_off;
  
  int *free_heap;
  int free_heap_top, free_heap_size;

  s7_cell **heap;
  int heap_size;
  
  s7_pointer args;            /* arguments of current function */
  s7_pointer envir;           /* current environment */
  s7_pointer code;            /* current code */

  s7_pointer stack;           /* stack is a vector in this case */
  int stack_size, stack_top;
  s7_pointer small_ints;      /* permanent numbers for opcode entries in the stack */

  s7_pointer protected_objects; /* a vector of gc-protected objects */
  int protected_objects_size, gc_loc;
  
  struct s7_cell _NIL;
  s7_pointer NIL;             /* empty list */
  struct s7_cell _HASHT;
  s7_pointer T;               /* #t */
  struct s7_cell _HASHF;
  s7_pointer F;               /* #f */
  struct s7_cell _EOF_OBJECT;
  s7_pointer EOF_OBJECT;      /* end-of-file object */
  
  struct s7_cell _UNDEFINED;  
  s7_pointer UNDEFINED;       /* unset or undefined object */
  
  struct s7_cell _UNSPECIFIED;
  s7_pointer UNSPECIFIED;     /* the unspecified value */
  
  s7_pointer symbol_table;    /* symbol table */
  s7_pointer global_env;      /* global environment */
  
  /* global s7_pointers to special symbols */
  s7_pointer LAMBDA;          /* syntax lambda */
  s7_pointer QUOTE;           /* syntax quote */
  s7_pointer QUASIQUOTE;      /* symbol quasiquote */
  s7_pointer UNQUOTE;         /* symbol unquote */
  s7_pointer UNQUOTE_SPLICING;/* symbol unquote-splicing */
  s7_pointer FEED_TO;         /* => */
  s7_pointer SET_OBJECT;      /* object set method */
  
  s7_pointer input_port;      /* current-input-port (nil = stdin) */
  s7_pointer input_port_stack;/*   input port stack (load and read internally) */
  s7_pointer output_port;     /* current-output-port (nil = stderr) */
  s7_pointer error_port;      /* current-error-port (nil = stderr) */

  bool gc_verbose;            /* if gc_verbose is true, print gc status */
  
  #define INITIAL_STRBUF_SIZE 1024
  int strbuf_size;
  char *strbuf;
  
  int tok;
  s7_pointer value;

  opcode_t op;
  long gensym_cnt;
#if TIMING
  off_t gc_mark_time, gc_sweep_time, total_freed, gc_sweeps;
#endif

  /* these are locals in eval, but we want that code to be context-free */
  s7_pointer x, y;
  num v;

  jmp_buf goto_start;
  bool longjmp_ok;
  void (*error_exiter)(void);
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
  T_LAST_TYPE = 18
};

static const char *type_names[T_LAST_TYPE + 1] = {
  "unused!", "nil", "string", "number", "symbol", "procedure", "pair", "closure", "continuation",
  "s7-function", "character", "input port", "vector", "macro", "promise", "s7-object", 
  "goto", "output port", "catch"
};


#define TYPE_BITS                     16
#define T_MASKTYPE                    0xffff

#define typeflag(p)                   ((p)->flag)
#define type(p)                       (typeflag(p) & T_MASKTYPE)
#define set_type_1(p, f)              typeflag(p) = ((f) | T_OBJECT)

#define T_SYNTAX                      (1 << (TYPE_BITS + 1))
#define is_syntax(p)                  (typeflag(p) & T_SYNTAX)
#define syntax_opcode(x)              cdr(x)

#define T_IMMUTABLE                   (1 << (TYPE_BITS + 2))
#define is_immutable(p)               (typeflag(p) & T_IMMUTABLE)
#define set_immutable(p)              typeflag(p) |= T_IMMUTABLE
#define set_mutable(p)                typeflag(p) &= (~T_IMMUTABLE)

#define T_ATOM                        (1 << (TYPE_BITS + 3))
#define is_atom(p)                    (typeflag(p) & T_ATOM)
#define set_atom(p)                   typeflag(p) |= T_ATOM

#define T_GC_MARK                     (1 << (TYPE_BITS + 4))
#define is_marked(p)                  (typeflag(p) &  T_GC_MARK)
#define set_mark(p)                   typeflag(p)  |= T_GC_MARK
#define clear_mark(p)                 typeflag(p)  &= (~T_GC_MARK)

#define T_CONSTANT                    (1 << (TYPE_BITS + 5))
#define is_constant(p)                (typeflag(p) & T_CONSTANT)
#define local_protect(p)              typeflag(p) |= T_CONSTANT
#define local_unprotect(p)            typeflag(p) &= (~T_CONSTANT)

#define T_UNUSED_BITS                 0xff000000
#define T_OBJECT                      (1 << (TYPE_BITS + 6))
#define T_FINALIZABLE                 (1 << (TYPE_BITS + 7))

#define is_object(x)                  ((x) && (((typeflag(x) & (T_UNUSED_BITS | T_OBJECT)) == T_OBJECT) && (type(x) != 0) && (type(x) <= T_LAST_TYPE)))

#define is_true(p)                    ((p) != sc->F)
#define is_false(p)                   ((p) == sc->F)
#define to_s7_bool(sc, Val)           ((Val) ? sc->T : sc->F)

#define is_pair(p)                    (type(p) == T_PAIR)
#define cons(sc, a, b)                cons_1(sc, a, b, false)
#define car(p)                        ((p)->object.cons.car)
#define cdr(p)                        ((p)->object.cons.cdr)
#define caar(p)                       car(car(p))
#define cadr(p)                       car(cdr(p))
#define cdar(p)                       cdr(car(p))
#define cddr(p)                       cdr(cdr(p))
#define cadar(p)                      car(cdr(car(p)))
#define cdadr(p)                      cdr(car(cdr(p)))
#define caddr(p)                      car(cdr(cdr(p)))
#define cadaar(p)                     car(cdr(car(car(p))))
#define cadddr(p)                     car(cdr(cdr(cdr(p))))
#define cddddr(p)                     cdr(cdr(cdr(cdr(p))))
#define pair_line_number(p)           (p)->object.cons.line

#define string_value(p)               ((p)->object.string.svalue)
#define string_length(p)              ((p)->object.string.length)
#define character(p)                  ((p)->object.cvalue)

#define vector_length(p)              ((p)->object.vector.length)
#define vector_element(p, i)          ((p)->object.vector.elements[i])
#define small_int(Sc, Val)            vector_element(Sc->small_ints, Val)
#define small_int_as_num(Sc, Val)     small_int(Sc, Val)->object.number

#define is_input_port(p)              (type(p) == T_INPUT_PORT) 
#define is_output_port(p)             (type(p) == T_OUTPUT_PORT)
#define port_paren_depth(p)           (p)->object.port->paren_depth
#define is_string_port(p)             (!((p)->object.port->is_file))
#define is_file_port(p)               (p)->object.port->is_file
#define port_line_number(p)           (p)->object.port->rep.stdio.line_number
#define port_filename(p)              (p)->object.port->rep.stdio.filename
#define port_file(p)                  (p)->object.port->rep.stdio.file
#define port_is_closed(p)             (p)->object.port->is_closed
#define port_needs_close(p)           (p)->object.port->needs_close
#define port_string(p)                (p)->object.port->rep.string.value
#define port_string_length(p)         (p)->object.port->rep.string.size
#define port_string_point(p)          (p)->object.port->rep.string.point

#define function_call(f)              (f)->object.ffptr->ff
#define function_name(f)              (f)->object.ffptr->name
#define function_documentation(f)     (f)->object.ffptr->doc
#define function_required_args(f)     (f)->object.ffptr->required_args
#define function_optional_args(f)     (f)->object.ffptr->optional_args
#define function_has_rest_arg(f)      (f)->object.ffptr->rest_arg

#define continuation_cc_stack_size(p) (p)->object.cc->cc_stack_size
#define continuation_cc_stack_top(p)  (p)->object.cc->cc_stack_top
#define continuation_cc_stack(p)      (p)->object.cc->cc_stack

#define is_goto(p)                    (type(p) == T_GOTO)
#define is_macro(p)                   (type(p) == T_MACRO)
#define is_promise(p)                 (type(p) == T_PROMISE)

#define is_catch(p)                   (type(p) == T_CATCH)
#define catch_tag(p)                  (p)->object.catcher->tag
#define catch_goto_loc(p)             (p)->object.catcher->goto_loc
#define catch_handler(p)              (p)->object.catcher->handler


#define NUM_INT 0
#define NUM_RATIO 1
#define NUM_REAL 2
#define NUM_REAL2 3
#define NUM_COMPLEX 4

/* so int = 0, exact < 2, ratio = 1, complex >= 4, real 2 or 3
 *   (I'm not going to support exact complex or inexact ratio etc)
 */

#define num_is_fixnum(n)              (n.type == NUM_INT)
#define object_is_fixnum(p)           (p->object.number.type == NUM_INT)

#define num_is_ratio(n)               (n.type == NUM_RATIO)
#define object_is_ratio(p)            (p->object.number.type == NUM_RATIO)

#define num_is_real(n)                ((n.type & (NUM_REAL | NUM_COMPLEX)) == NUM_REAL)
#define object_is_real(p)             ((p->object.number.type & (NUM_REAL | NUM_COMPLEX)) == NUM_REAL

#define num_is_complex(n)             (n.type & NUM_COMPLEX)
#define object_is_complex(p)          (p->object.number.type & NUM_COMPLEX)

#define num_type(n)                   (n.type)
#define object_number_type(p)         (p->object.number.type)

#define numerator(n)                  n.value.fvalue.numerator
#define denominator(n)                n.value.fvalue.denominator

#define real_part(n)                  n.value.cvalue.real
#define imag_part(n)                  n.value.cvalue.imag

#define fraction(n)                   (((double)numerator(n)) / ((double)denominator(n)))

#define integer(n)                    n.value.ivalue
#define real(n)                       n.value.rvalue

#define loop_counter(p)               ((p)->object.number.value.ivalue)



static char *describe_type(s7_pointer p)
{
  char *buf;
  buf = (char *)CALLOC(1024, sizeof(char));
  sprintf(buf, "%s%s%s%s%s%s%s%s",
	  ((type(p) >= 0) && (type(p) <= T_LAST_TYPE)) ? type_names[type(p)] : "bogus type",
	  (typeflag(p) & T_SYNTAX) ? " syntax" : "",
	  (typeflag(p) & T_IMMUTABLE) ? " immutable" : "",
	  (typeflag(p) & T_ATOM) ? " atom" : "",
	  (typeflag(p) & T_GC_MARK) ? " marked" : "",
	  (typeflag(p) & T_OBJECT) ? " object" : "",
	  (typeflag(p) & T_FINALIZABLE) ? " gc-freeable" : "",
	  (typeflag(p) & T_UNUSED_BITS) ? " and other garbage bits!" : "");
  return(buf);
}

#define set_type(p, f) set_type_2(sc, p, f, __FUNCTION__, __LINE__)
static void set_type_2(s7_scheme *sc, s7_pointer p, int f, const char *func, int line)
{
  if (is_immutable(p))
    {
      fprintf(stderr, "%s[%d]: set type wants to set immutable element: %s type to %s\n", 
	      func, line, s7_object_to_c_string(sc, p), type_names[f & T_MASKTYPE]);
      abort();
    }
  set_type_1(p, f);
}


#define TOK_EOF     (-1)
#define TOK_LPAREN  0
#define TOK_RPAREN  1
#define TOK_DOT     2
#define TOK_ATOM    3
#define TOK_QUOTE   4
#define TOK_COMMENT 5
#define TOK_DQUOTE  6
#define TOK_BQUOTE  7
#define TOK_COMMA   8
#define TOK_ATMARK  9
#define TOK_SHARP_CONST 10
#define TOK_VEC     11

#define BACKQUOTE '`'

#define CASE_SENSITIVE 1
#if CASE_SENSITIVE
#define string_downcase(Str) Str
#else
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

static void s7_mark_embedded_objects(s7_pointer a); /* called by gc, calls fobj's mark func */
static void eval(s7_scheme *sc, opcode_t first_op);
static s7_pointer g_stacktrace(s7_scheme *sc, s7_pointer args);
static s7_pointer cons_1(s7_scheme *sc, s7_pointer a, s7_pointer b, bool immutable);
static s7_pointer s7_string_concatenate(s7_scheme *sc, const char *s1, const char *s2);
static s7_pointer s7_division_by_zero_error(s7_scheme *sc, const char *caller, s7_pointer arg);
static s7_pointer s7_file_error(s7_scheme *sc, const char *caller, const char *descr, const char *name);


static void gsp(s7_scheme *sc) {g_stacktrace(sc, sc->args);} /* for gdb */
static void gop(s7_scheme *sc, s7_pointer obj) {s7_object_to_c_string(sc, obj);}


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

s7_pointer s7_EOF_OBJECT(s7_scheme *sc) 
{
  return(sc->EOF_OBJECT);
}

static s7_pointer g_not(s7_scheme *sc, s7_pointer args)
{
  #define H_not "(not obj) returns #t if obj is #f, otherwise #t"
  return(to_s7_bool(sc, is_false(car(args))));
}

static s7_pointer s7_not(s7_scheme *sc, s7_pointer x)
{
  return((x == sc->F) ? sc->T : sc->F);
}

static s7_pointer g_is_boolean(s7_scheme *sc, s7_pointer args)
{
  #define H_is_boolean "(boolean? obj) returns #t if obj is #f or #t"
  return(to_s7_bool(sc, ((car(args) == sc->F) || (car(args) == sc->T))));
}

bool s7_is_immutable(s7_pointer p) 
{ 
  return(typeflag(p) & T_IMMUTABLE);
}

void s7_set_immutable(s7_pointer p) 
{ 
  typeflag(p) |= T_IMMUTABLE;
}





/* -------------------------------- GC -------------------------------- */


int s7_gc_protect(s7_scheme *sc, s7_pointer x)
{
  int i, loc, new_size;
  if (vector_element(sc->protected_objects, sc->gc_loc) == sc->NIL)
    {
      vector_element(sc->protected_objects, sc->gc_loc) = x;
      loc = sc->gc_loc++;
      if (sc->gc_loc >= sc->protected_objects_size)
	sc->gc_loc = 0;
      return(loc);
    }

  for (i = 0; i < sc->protected_objects_size; i++)
    if (vector_element(sc->protected_objects, i) == sc->NIL)
      {
	vector_element(sc->protected_objects, i) = x;
	return(i);
      }
  
  loc = sc->protected_objects_size;
  new_size = 2 * sc->protected_objects_size;
  sc->protected_objects->object.vector.elements = (s7_pointer *)REALLOC(sc->protected_objects->object.vector.elements, new_size * sizeof(s7_pointer));
  for (i = sc->protected_objects_size + 1; i < new_size; i++)
    vector_element(sc->protected_objects, i) = sc->NIL;
  sc->protected_objects->object.vector.length = new_size;
  sc->protected_objects_size = new_size;
  vector_element(sc->protected_objects, loc) = x;

  return(loc);

}

void s7_gc_unprotect(s7_scheme *sc, s7_pointer x)
{
  int i;
  for (i = 0; i < sc->protected_objects_size; i++)
    if (vector_element(sc->protected_objects, i) == x)
      {
	vector_element(sc->protected_objects, i) = sc->NIL;
	sc->gc_loc = i;
	return;
      }
}

void s7_gc_unprotect_at(s7_scheme *sc, int loc)
{
  vector_element(sc->protected_objects, loc) = sc->NIL;
  sc->gc_loc = loc;
}


static void finalize_s7_cell(s7_scheme *sc, s7_pointer a) 
{
  if (typeflag(a) & T_FINALIZABLE)
    {
      switch (type(a))
	{
	case T_STRING:
	case T_SYMBOL:
	  FREE(string_value(a)); /* calloc'd in make-*-string */
	  break;

	case T_INPUT_PORT:
	  if (port_needs_close(a))
	    s7_close_input_port(sc, a);
	  if ((is_file_port(a)) && 
	      (port_filename(a)))
	    {
	      FREE(port_filename(a));
	      port_filename(a) = NULL;
	    }
	  FREE(a->object.port);
	  break;

	case T_OUTPUT_PORT:
	  s7_close_output_port(sc, a);
	  if ((is_file_port(a)) && 
	      (port_filename(a)))
	    {
	      FREE(port_filename(a));
	      port_filename(a) = NULL;
	    }
	  FREE(a->object.port);
	  break;

	case T_S7_OBJECT:
	  s7_free_object(a);
	  break;

	case T_VECTOR:
	  if (vector_length(a) > 0)
	    FREE(a->object.vector.elements);
	  break;

	case T_CONTINUATION:
	  if (a->object.cc)
	    FREE(a->object.cc);
	  break;

	case T_CATCH:
	  /* fprintf(stderr, "free %p\n", a->object.catcher); */
	  FREE(a->object.catcher);
	  break;

	default:
	  break;
	}
    }
#if USE_SND
  FREE(a);
#else
  memset((void *)a, 0, sizeof(s7_cell));
#endif
}
  
static void mark_vector(s7_pointer p, int top)
{
  int i;
  set_mark(p);
  for(i = 0; i < top; i++) 
    s7_mark_object(vector_element(p, i));
}


void s7_mark_object(s7_pointer p)
{
#if USE_SND
  if (!p) return;
#endif

  if (is_marked(p)) return; 

  set_mark(p);

  if (s7_is_vector(p)) 
    {
      mark_vector(p, vector_length(p));
      return;
    }

  if (s7_is_object(p))
    {
      s7_mark_embedded_objects(p);
      return;
    }

  if (s7_is_continuation(p))
    {
      s7_mark_object(continuation_cc_stack(p));
      return;
    }

  if (is_catch(p))
    {
      s7_mark_object(catch_tag(p));
      s7_mark_object(catch_handler(p));
      return;
    }

  /* this should follow s7_is_object -- the latter is an atom, but we have to run the object's internal mark function */
  if (is_atom(p))
    return;

  if (car(p))
    s7_mark_object(car(p));

  if (cdr(p))
    s7_mark_object(cdr(p));
}


static void free_s7_cell(s7_scheme *sc, int loc)
{
  /* free_heap_top points past end 
   *   free -> top++
   *   alloc <- --top
   */
  if (sc->free_heap_top >= sc->free_heap_size)
    {
      int k, old_size;
      old_size = sc->free_heap_size;
      sc->free_heap_size *= 2;
      sc->free_heap = (int *)REALLOC(sc->free_heap, sc->free_heap_size * sizeof(int));
      for (k = old_size; k < sc->free_heap_size; k++)
	sc->free_heap[k] = -1;
    }
  
#if S7_DEBUGGING
  if ((loc < 0) ||
      (sc->free_heap_top < 0))
    {
      fprintf(stderr, "free %d %d\n", loc, sc->free_heap_top);
      abort();
    }
#endif

#if USE_SND
  if (sc->heap == NULL) fprintf(stderr, "freeing same loc twice?");
  sc->heap[loc] = NULL;
#endif

  sc->free_heap[sc->free_heap_top++] = loc;
}


static s7_pointer alloc_s7_cell(s7_scheme *sc)
{
  if (sc->free_heap_top <= 0)
    return(NULL);

#if USE_SND
  {
    int loc;
    loc = sc->free_heap[--(sc->free_heap_top)];
    sc->heap[loc] = (s7_cell *)CALLOC(1, sizeof(s7_cell));
    return(sc->heap[loc]);
  }
#else
  return(sc->heap[sc->free_heap[--(sc->free_heap_top)]]);
#endif
}


static int gc(s7_scheme *sc, const char *function, int line)
{
  s7_pointer p;
  int i, freed_heap = 0;

#if TIMING
  clock_t start;
  start = clock();
  sc->gc_sweeps++;
#endif
  
  if ((sc->gc_verbose) &&
      (sc->output_port == sc->NIL))
    fprintf(stderr, "\n%s[%d] gc...", function, line);
  
  s7_mark_object(sc->symbol_table);
  s7_mark_object(sc->global_env);
  
  s7_mark_object(sc->args);
  s7_mark_object(sc->envir);
  s7_mark_object(sc->code);
  mark_vector(sc->stack, sc->stack_top);

  s7_mark_object(sc->value);
  s7_mark_object(sc->input_port);
  s7_mark_object(sc->input_port_stack);
  s7_mark_object(sc->output_port);
  s7_mark_object(sc->error_port);

  s7_mark_object(sc->protected_objects);
  s7_mark_object(sc->x);
  s7_mark_object(sc->y);

  clear_mark(sc->NIL);

  for (i = 0; i < sc->heap_size; i++)
    {
      p = sc->heap[i];
#if USE_SND
      if (!p) continue;
#else
      if (typeflag(p) == 0) continue; /* an already-free object */
#endif
      if (is_marked(p)) 
	clear_mark(p);
      else 
	{
	  if (!is_constant(p))
	    {
	      /* reclaim s7_cell */
	      finalize_s7_cell(sc, p); 
	      free_s7_cell(sc, i);
	      freed_heap++;
	    }
	}
    }
  
  if ((sc->gc_verbose) &&
      (sc->output_port == sc->NIL))
    fprintf(stderr, "done: %d heap were recovered, total heap: %d\n", freed_heap, sc->heap_size);
  return(freed_heap);
}


#define new_cell(Sc) new_cell_1(Sc, __FUNCTION__, __LINE__)
static s7_pointer new_cell_1(s7_scheme *sc, const char *function, int line)
{
  s7_pointer p;
  p = alloc_s7_cell(sc);
  if (!p)
    {
      /* no free heap */
      int k, old_size, freed_heap = 0;

      if (!(sc->gc_off)) 
	freed_heap = gc(sc, function, line);

      if (freed_heap < 1000)
	{
	  /* alloc more heap */
	  old_size = sc->heap_size;
	  sc->heap_size *= 2;
	  sc->heap = (s7_cell **)REALLOC(sc->heap, sc->heap_size * sizeof(s7_cell *));
	  for (k = old_size; k < sc->heap_size; k++)
	    {
	      sc->heap[k] = (s7_cell *)CALLOC(1, sizeof(s7_cell));
	      free_s7_cell(sc, k);
	    }
	}
      p = alloc_s7_cell(sc);
    }

#if S7_DEBUGGING
  if (is_immutable(p)) 
    fprintf(stderr, "%d %p: alloc immutable?? %x %s", sc->free_heap_top, p, typeflag(p), describe_type(p));
#endif

  return(p);
}


static s7_pointer g_gc_verbose(s7_scheme *sc, s7_pointer a)
{
  #define H_gc_verbose "(gc-verbose bool) turns GC reportage on or off"
  s7_pointer old_val;
  old_val = (sc->gc_verbose) ? sc->T : sc->F;
  sc->gc_verbose = (car(a) != sc->F);
  return(old_val);
}


static s7_pointer g_gc(s7_scheme *sc, s7_pointer a)
{
  #define H_gc "(gc) runs the garbage collector"
  gc(sc, __FUNCTION__, __LINE__);
  return(sc->UNSPECIFIED);
}


static void search_heap(s7_scheme *sc, s7_pointer obj)
{
  int i;
  for (i = 0; i < sc->heap_size; i++)
    if (obj == sc->heap[i])
      break;
  if (i < sc->heap_size)
    fprintf(stderr, "unknown object in heap: %d %p\n", i, obj);
  else fprintf(stderr, "unknown object %p not in heap", obj);
}



/* -------------------------------- stack -------------------------------- */

static void stack_reset(s7_scheme *sc) 
{ 
  sc->stack_top = 0;
} 


static void pop_stack(s7_scheme *sc, s7_pointer a) 
{ 
  int top;

  sc->value = a; 
  top = sc->stack_top;

#if S7_DEBUGGING
  if (top < 4)
    {
      fprintf(stderr, "attempt to pop off top of stack!");
      abort();
      return;
    }
  if (!is_object(a))
    {
      fprintf(stderr, "stack value popped: %p\n", a);
      search_heap(sc, a);
      abort();
    }
#endif

  sc->op =         (opcode_t)integer(vector_element(sc->stack, top - 1)->object.number);
  sc->args =       vector_element(sc->stack, top - 2);
  sc->envir =      vector_element(sc->stack, top - 3);
  sc->code =       vector_element(sc->stack, top - 4);
  sc->stack_top -= 4;
} 


static void push_stack(s7_scheme *sc, opcode_t op, s7_pointer args, s7_pointer code) 
{ 
  int top;

  top = sc->stack_top;
  sc->stack_top += 4;

#if S7_DEBUGGING
  if (!is_object(args))
    {
      fprintf(stderr, "stack args pushed: %p\n", args);
      abort();
    }
  if (!is_object(code))
    {
      fprintf(stderr, "stack code pushed: %p\n", code);
      abort();
    }
  if ((op < 0 ) || (op > OP_MAX_DEFINED))
    {
      fprintf(stderr, "push bad op: %d\n", op);
      abort();
    }
#endif

  if (sc->stack_top >= sc->stack_size)
    {
      int i, new_size;
      new_size = sc->stack_size * 2;

#if S7_DEBUGGING
      if (new_size > 200000) 
	{
	  fprintf(stderr, "stack is growing too big");
	  abort();
	}
#endif

      sc->stack->object.vector.elements = (s7_pointer *)REALLOC(sc->stack->object.vector.elements, new_size * sizeof(s7_pointer));
      for (i = sc->stack_size; i < new_size; i++)
	vector_element(sc->stack, i) = sc->NIL;
      sc->stack->object.vector.length = new_size;
      sc->stack_size = new_size;
    }

  vector_element(sc->stack, top + 0) = code;
  vector_element(sc->stack, top + 1) = sc->envir;
  vector_element(sc->stack, top + 2) = args;
  vector_element(sc->stack, top + 3) = vector_element(sc->small_ints, (int)op);
} 


/* PERHAPS: scheme output? */

static void print_stack_entry(s7_scheme *sc, opcode_t op, s7_pointer code, s7_pointer args)
{
  char *str1 = NULL, *str2 = NULL;
  if (op != OP_APPLY)
    {
      char *temp;
      temp = copy_string(opcode_names[op]);
      str1 = s7_object_to_c_string(sc, args);
      str2 = s7_object_to_c_string(sc, code);
      fprintf(stderr, "\n%s: args: [%p] %s, code: %s", string_downcase(temp), args, str1, str2);
      FREE(temp);
    }
  else
    {
      if (s7_is_function(code))
	fprintf(stderr, "\n(%s %s)", 
		function_name(code), 
		str1 = s7_object_to_c_string(sc, args));
      else fprintf(stderr, "\n(%s %s)", 
		   str1 = s7_object_to_c_string(sc, code), 
		   str2 = s7_object_to_c_string(sc, args));
    }
  if (str1) FREE(str1);
  if (str2) FREE(str2);
}


static s7_pointer g_stacktrace(s7_scheme *sc, s7_pointer args)
{
  #define H_stacktrace "(stacktrace) prints out the current stack contents"
  int i;
  sc->gc_off = true;

  for (i = 0; i < sc->stack_top; i +=4)
    print_stack_entry(sc, 
		      (opcode_t)s7_integer(vector_element(sc->stack, i + 3)),
		      vector_element(sc->stack, i + 0),
		      vector_element(sc->stack, i + 2));
  print_stack_entry(sc, sc->op, sc->code, sc->args);
  fprintf(stderr,"\n");
  sc->gc_off = false;
  return(sc->UNSPECIFIED);
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


static s7_pointer symbol_table_add_by_name(s7_scheme *sc, const char *name) 
{ 
  s7_pointer x, str; 
  int location;
  
  str = s7_make_string(sc, name);
  local_protect(str);
  x = cons(sc, str, sc->NIL); 
  set_type(x, (T_SYMBOL | T_FINALIZABLE));
  local_protect(x);
  s7_set_immutable(car(x)); 

  location = hash_fn(name, vector_length(sc->symbol_table)); 
  vector_element(sc->symbol_table, location) = s7_immutable_cons(sc, x, vector_element(sc->symbol_table, location)); 

  local_unprotect(str);
  local_unprotect(x);
  return(x); 
} 


static  s7_pointer symbol_table_find_by_name(s7_scheme *sc, const char *name) 
{ 
  int location; 
  s7_pointer x; 
  char *s; 
  
  location = hash_fn(name, vector_length(sc->symbol_table)); 
  for (x = vector_element(sc->symbol_table, location); x != sc->NIL; x = cdr(x)) 
    { 
      s = s7_symbol_name(car(x)); 
      if ((s) && (strcasecmp(name, s) == 0))
	return(car(x)); 
    } 
  return sc->NIL; 
} 


static s7_pointer symbol_table_all_symbols(s7_scheme *sc) 
{ 
  int i; 
  s7_pointer x; 
  s7_pointer ob_list = sc->NIL; 

  sc->gc_off = true;
  for (i = 0; i < vector_length(sc->symbol_table); i++) 
    for (x  = vector_element(sc->symbol_table, i); x != sc->NIL; x = cdr(x)) 
      ob_list = cons(sc, x, ob_list); 
  sc->gc_off = false;

  return(ob_list); 
} 


static s7_pointer g_symbol_table(s7_scheme *sc, s7_pointer args)
{
  #define H_symbol_table "(symbol-table) returns a list of all known symbols"
  return(symbol_table_all_symbols(sc));
}


void s7_for_each_symbol_name(s7_scheme *sc, bool (*symbol_func)(const char *symbol_name, void *data), void *data)
{
  int i; 
  s7_pointer x; 
  for (i = 0; i < vector_length(sc->symbol_table); i++) 
    for (x  = vector_element(sc->symbol_table, i); x != sc->NIL; x = cdr(x)) 
      if (symbol_func(s7_symbol_name(car(x)), data))
	return;
}


void s7_for_each_symbol(s7_scheme *sc, bool (*symbol_func)(const char *symbol_name, s7_pointer symbol_value, void *data), void *data)
{
  int i; 
  s7_pointer x; 
  for (i = 0; i < vector_length(sc->symbol_table); i++) 
    for (x  = vector_element(sc->symbol_table, i); x != sc->NIL; x = cdr(x)) 
      if (symbol_func(s7_symbol_name(car(x)), cdr(x), data))
	return;
}


s7_pointer s7_make_symbol(s7_scheme *sc, const char *name) 
{ 
  s7_pointer x; 
  x = symbol_table_find_by_name(sc, name); 
  if (x != sc->NIL) 
    return(x); 
  return(symbol_table_add_by_name(sc, name)); 
} 


s7_pointer s7_gensym(s7_scheme *sc, const char *prefix)
{ 
  char *name;
  int len;
  s7_pointer x;

  len = safe_strlen(prefix) + 32;
  name = (char *)CALLOC(len, sizeof(char));
  
  for(; sc->gensym_cnt < LONG_MAX; sc->gensym_cnt++) 
    { 
      snprintf(name, len, "%s-%ld", prefix, sc->gensym_cnt); 
      
      /* first check symbol_table */ 
      x = symbol_table_find_by_name(sc, name); 
      if (x != sc->NIL) 
	continue; 

      x = symbol_table_add_by_name(sc, name); 
      FREE(name);
      return(x); 
    } 
  FREE(name);
  return(sc->NIL); 
} 


static s7_pointer g_gensym(s7_scheme *sc, s7_pointer args) 
{
  #define H_gensym "(gensym :optional prefix) creates a new symbol"
  if (s7_is_pair(args))
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
  return(to_s7_bool(sc, s7_is_symbol(car(args))));
}


char *s7_symbol_name(s7_pointer p)   
{ 
  return(string_value(car(p)));
}


static s7_pointer g_symbol_to_string(s7_scheme *sc, s7_pointer args)
{
  #define H_symbol_to_string "(symbol->string sym) returns sym converted to a string"
  if (!s7_is_symbol(car(args)))
    return(s7_wrong_type_arg_error(sc, "symbol->string", 1, car(args), "a symbol"));
  return(s7_make_string(sc, s7_symbol_name(car(args))));
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
  expr = (char *)CALLOC(len, sizeof(char));
  snprintf(expr, len, "(set! *features* (cons '%s *features*))", feature);
  s7_eval_c_string(sc, expr);
  FREE(expr);
}





/* -------------------------------- environments -------------------------------- */

/* 
 * In this implementation, the top level environment is 
 * a hash table: a vector of alists hashed by variable name. 
 * Subsequent frames prepend an alist to this vector.
 */ 

static s7_pointer new_frame_in_env(s7_scheme *sc, s7_pointer old_env) 
{ 
  s7_pointer new_frame; 

  /* The interaction-environment has about 300 variables in it. */ 
  if (old_env == sc->NIL) 
    new_frame = s7_make_vector(sc, SYMBOL_TABLE_SIZE); 
  else new_frame = sc->NIL; 
  local_protect(new_frame);
   
  sc->envir = s7_immutable_cons(sc, new_frame, old_env); 
  local_unprotect(new_frame);
  return(sc->envir);
} 

static void s7_new_slot_spec_in_env(s7_scheme *sc, s7_pointer env, s7_pointer variable, s7_pointer value) 
{ 
  s7_pointer slot;
  local_protect(variable);
  local_protect(value);
  slot = s7_immutable_cons(sc, variable, value); 
  local_protect(slot);
  if (s7_is_vector(car(env))) 
    { 
      int location = hash_fn(s7_symbol_name(variable), vector_length(car(env))); 
      vector_element(car(env), location) = s7_immutable_cons(sc, slot, vector_element(car(env), location)); 
    } 
  else car(env) = s7_immutable_cons(sc, slot, car(env));
  local_unprotect(slot);
  local_unprotect(variable);
  local_unprotect(value);
} 

static s7_pointer s7_find_slot_in_env(s7_scheme *sc, s7_pointer env, s7_pointer hdl, bool all) 
{ 
  s7_pointer x, y; 

  /* this is a list ending with a vector */
  
  for (x = env; x != sc->NIL; x = cdr(x)) 
    { 
      if (s7_is_vector(car(x))) 
	y = vector_element(car(x), hash_fn(s7_symbol_name(hdl), vector_length(car(x))));
      else y = car(x); 

      for ( ; y != sc->NIL; y = cdr(y)) 
	if (caar(y) == hdl) 
	  break; 

      if (y != sc->NIL) 
	break; 

      if (!all) 
	return(sc->NIL); 
    } 

  if (x != sc->NIL) 
    return(car(y)); 

  return(sc->NIL); 
} 

static void new_slot_in_env(s7_scheme *sc, s7_pointer variable, s7_pointer value) 
{ 
  s7_new_slot_spec_in_env(sc, sc->envir, variable, value); 
} 

static void set_slot_in_env(s7_scheme *sc, s7_pointer slot, s7_pointer value) 
{ 
  cdr(slot) = value; 
} 

static s7_pointer s7_slot_value_in_env(s7_pointer slot) 
{ 
  return(cdr(slot)); 
} 


s7_pointer s7_symbol_value(s7_scheme *sc, s7_pointer sym) /* was searching just the global environment? */
{
  s7_pointer x;
  x = s7_find_slot_in_env(sc, sc->envir, sym, true);
  if (x != s7_NIL(sc))
    return(s7_slot_value_in_env(x));
  return(sc->F);
}


s7_pointer s7_symbol_set_value(s7_scheme *sc, s7_pointer sym, s7_pointer val)
{
  s7_pointer x;
  x = s7_find_slot_in_env(sc, sc->envir, sym, true);
  if (x != s7_NIL(sc))
    set_slot_in_env(sc, x, val);
  return(val);
}

static s7_pointer g_global_environment(s7_scheme *sc, s7_pointer p)
{
  #define H_global_environment "(global-environment) returns the top-level definitions (symbol bindings)"
  return(sc->global_env);
}

static s7_pointer g_current_environment(s7_scheme *sc, s7_pointer p)
{
  #define H_current_environment "(current-environment) returns the current definitions (symbol bindings)"
  return(sc->envir);
}
      
/* make closure. c is code. e is environment */
s7_pointer s7_make_closure(s7_scheme *sc, s7_pointer c, s7_pointer e) 
{
  s7_pointer x = new_cell(sc);

  set_type(x, T_CLOSURE);

#if S7_DEBUGGING
  if (!is_object(c))
    {
      fprintf(stderr, "closure code is not an object!");
      abort();
    }
  if (!is_object(e))
    {
      fprintf(stderr, "closure environment is not an object!");
      abort();
    }
#endif

  car(x) = c;
  cdr(x) = e;
  return(x);
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

  if (cdr(args) != sc->NIL)
    {
      if (!s7_is_pair(cadr(args)))
	return(s7_wrong_type_arg_error(sc, "defined?", 2, cadr(args), "an enivronment"));
      sc->x = cadr(args);
    }
  else sc->x = sc->envir;
  return(to_s7_bool(sc, s7_find_slot_in_env(sc, sc->x, car(args), true) != sc->NIL));
}
      
void s7_define(s7_scheme *sc, s7_pointer envir, s7_pointer symbol, s7_pointer value) 
{
  s7_pointer x;
  x = s7_find_slot_in_env(sc, envir, symbol, false);
  if (x != sc->NIL) 
    set_slot_in_env(sc, x, value); 
  else s7_new_slot_spec_in_env(sc, envir, symbol, value); 
}

void s7_define_variable(s7_scheme *sc, const char *name, s7_pointer value)
{
  s7_pointer sym;
  
  local_protect(value);
  sym = s7_make_symbol(sc, name);
  local_protect(sym);
  s7_define(sc, s7_global_environment(sc), sym, value);
  local_unprotect(sym);
  local_unprotect(value);
}

#if 0
static s7_pointer s7_search_environment(s7_scheme *sc, s7_pointer env, bool (*searcher)(s7_scheme *sc, s7_pointer binding, s7_pointer data), s7_pointer data)
{ 
  int i, len;
  s7_pointer x, y, vec; 

  /* fprintf(stderr, "search: %s\n", s7_object_to_c_string(sc, env)); */

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
#endif



/* -------------------------------- continuations and gotos -------------------------------- */

bool s7_is_continuation(s7_pointer p)    
{ 
  return(type(p) == T_CONTINUATION);
}

static s7_pointer copy_list(s7_scheme *sc, s7_pointer lst)
{
  if (lst == sc->NIL)
    return(sc->NIL);
  return(cons(sc, car(lst), copy_list(sc, cdr(lst))));
}

static s7_pointer copy_object(s7_scheme *sc, s7_pointer obj)
{
  s7_pointer nobj;

  if ((is_constant(obj)) || 
      (s7_is_symbol(obj)) ||
      (is_atom(obj)) ||
      (s7_is_object(obj)) ||
      (s7_is_vector(obj)) ||
      (s7_is_continuation(obj)) ||
      (is_input_port(obj)) ||
      (is_output_port(obj)))
    return(obj);

  nobj = new_cell(sc);
  memcpy((void *)nobj, (void *)obj, sizeof(s7_cell));

  local_protect(nobj);
  car(nobj) = copy_object(sc, car(obj));
  if ((s7_is_closure(obj)) ||
      (is_macro(obj)) || 
      (is_promise(obj)) ||
      (s7_is_function(obj)))
    cdr(nobj) = cdr(obj); /* the environment of the closure */
  else cdr(nobj) = copy_object(sc, cdr(obj));
  local_unprotect(nobj);

  return(nobj);
}

static s7_pointer copy_stack(s7_scheme *sc, s7_pointer old_v, int top)
{
  int i, len;
  s7_pointer new_v;
  len = vector_length(old_v);
  new_v = s7_make_vector(sc, len);
  local_protect(new_v);

  sc->gc_off = true;

  for (i = 0; i < top; i += 4)
    {
      vector_element(new_v, i + 0) = copy_object(sc, vector_element(old_v, i + 0));             /* code */
      vector_element(new_v, i + 1) = vector_element(old_v, i + 1);                              /* environment pointer */
      vector_element(new_v, i + 2) = copy_list(sc, vector_element(old_v, i + 2));               /* args */

      /* I'm not sure the args list actually needs to be copied -- it seems to work
       *   if I simply copy the element directly, but I don't trust it -- I need
       *   some comprehensive call/cc tests.
       */

      vector_element(new_v, i + 3) = vector_element(old_v, i + 3);                              /* op (constant int) */
    }

  sc->gc_off = false;

  local_unprotect(new_v);
  return(new_v);
}

static s7_pointer s7_make_goto(s7_scheme *sc) 
{
  s7_pointer x;
  x = new_cell(sc);
  set_type(x, T_ATOM | T_GOTO);
  x->object.goto_loc = sc->stack_top;
  return(x);
}

static s7_pointer s7_make_continuation(s7_scheme *sc) 
{
  continuation *c;
  s7_pointer x = new_cell(sc);
  set_type(x, T_CONTINUATION | T_FINALIZABLE);
  local_protect(x);
  c = (continuation *)CALLOC(1, sizeof(continuation));

  /* save current state */
  c->cc_stack_top = sc->stack_top;
  c->cc_stack_size = sc->stack_size;
  c->cc_stack = copy_stack(sc, sc->stack, sc->stack_top);
  x->object.cc = c;
  local_unprotect(x);

  return(x);
}

static s7_pointer s7_call_continuation(s7_scheme *sc, s7_pointer p)
{
  continuation *c;
  c = p->object.cc;

  sc->stack = copy_stack(sc, c->cc_stack, c->cc_stack_top);
  sc->stack_size = c->cc_stack_size;
  sc->stack_top = c->cc_stack_top;

#if S7_DEBUGGING
  if (sc->stack_top > sc->stack_size)
    {
      fprintf(stderr, "continuation set top to %d (%d)\n", sc->stack_top, sc->stack_size);
      abort();
    }
#endif

  return(sc->value);
}

static s7_pointer g_call_cc(s7_scheme *sc, s7_pointer args)
{
  #define H_call_cc "(call-with-current-continuation ...) needs more than a one sentence explanation"
  sc->code = car(args);
  sc->args = cons(sc, s7_make_continuation(sc), sc->NIL);
  push_stack(sc, OP_APPLY, sc->args, sc->code);
  return(sc->NIL);
}

static s7_pointer g_call_with_exit(s7_scheme *sc, s7_pointer args)
{
  #define H_call_with_exit "(call-with-exit ...) is a simplified call/cc"

  /* (call-with-exit (lambda (return) ...)) */

  sc->code = car(args);                           /* the lambda form */
  sc->args = cons(sc, s7_make_goto(sc), sc->NIL); /*   the argument to the lambda (the goto = "return" above) */
  push_stack(sc, OP_APPLY, sc->args, sc->code);   /* apply looks at sc->code to decide what to do (it will see the lambda) */

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

/* Trigonometric functions. FreeBSD's math library does not include the complex form of the trig funcs. */ 
 
#if !HAVE_CSIN 
double complex csin(double complex z) 
{ 
  return sin(creal(z)) * cosh(cimag(z)) + (cos(creal(z)) * sinh(cimag(z))) * _Complex_I; 
} 
#endif 
 
#if !HAVE_CCOS 
double complex ccos(double complex z) 
{ 
  return cos(creal(z)) * cosh(cimag(z)) + (-sin(creal(z)) * sinh(cimag(z))) * _Complex_I; 
} 
#endif 
 
#if !HAVE_CTAN 
double complex ctan(double complex z) 
{ 
  return csin(z) / ccos(z); 
} 
#endif 
 
#if !HAVE_CASIN 
double complex casin(double complex z) 
{ 
  return -_Complex_I * clog(_Complex_I * z + csqrt(1.0 - z * z)); 
} 
#endif 
 
#if !HAVE_CACOS 
double complex cacos(double complex z) 
{ 
  return -_Complex_I * clog(z + _Complex_I * csqrt(1.0 - z * z)); 
} 
#endif 
 
#if !HAVE_CATAN 
double complex catan(double complex z) 
{ 
  return _Complex_I * clog((_Complex_I + z) / (_Complex_I - z)) / 2.0; 
} 
#endif 
 

/* Hyperbolic functions. */ 
 
#if !HAVE_CSINH 
double complex csinh(double complex z) 
{ 
  return sinh(creal(z)) * cos(cimag(z)) + (cosh(creal(z)) * sin(cimag(z))) * _Complex_I; 
} 
#endif 
 
#if !HAVE_CCOSH 
double complex ccosh(double complex z) 
{ 
  return cosh(creal(z)) * cos(cimag(z)) + (sinh(creal(z)) * sin(cimag(z))) * _Complex_I; 
} 
#endif 
 
#if !HAVE_CTANH 
double complex ctanh(double complex z) 
{ 
  return csinh(z) / ccosh(z); 
} 
#endif 
 
#if !HAVE_CASINH 
double complex casinh(double complex z) 
{ 
  return clog(z + csqrt(1.0 + z * z)); 
} 
#endif 
 
#if !HAVE_CACOSH 
double complex cacosh(double complex z) 
{ 
  return clog(z + csqrt(z * z - 1.0)); 
} 
#endif 
 
#if !HAVE_CATANH 
double complex catanh(double complex z) 
{ 
  return clog((1.0 + z) / (1.0 - z)) / 2.0; 
} 
#endif 
 
/* Exponential and logarithmic functions. */ 
 
#if !HAVE_CEXP 
double complex cexp(double complex z) 
{ 
  return exp(creal(z)) * cos(cimag(z)) + (exp(creal(z)) * sin(cimag(z))) * _Complex_I; 
} 
#endif 
 
#if !HAVE_CLOG 
double complex clog(double complex z) 
{ 
  return log(fabs(cabs(z))) + carg(z) * _Complex_I; 
} 
#endif 
 
/* Power functions. */ 
 
#if !HAVE_CPOW 
double complex cpow(double complex x, double complex y) 
{ 
  double r = cabs(x); 
  double theta = carg(x); 
  double yre = creal(y); 
  double yim = cimag(y); 
  double nr = exp(yre * log(r) - yim * theta); 
  double ntheta = yre * theta + yim * log(r); 
 
  return nr * cos(ntheta) + (nr * sin(ntheta)) * _Complex_I; /* make-polar */ 
} 
#endif 
 
#if !HAVE_CSQRT 
double complex csqrt(double complex z) 
{ 
  if (cimag(z) < 0.0) 
    return conj(csqrt(conj(z))); 
  else 
  { 
    double r = cabs(z); 
    double x = creal(z); 
       
    return sqrt((r + x) / 2.0) + sqrt((r - x) / 2.0) * _Complex_I; 
  } 
} 
#endif 
 
/* Absolute value and conjugates. */ 
 
#if !HAVE_CABS 
double cabs(double complex z) 
{ 
  return hypot(creal(z), cimag(z)); 
} 
#endif 
 
#if !HAVE_CARG 
double carg(double complex z) 
{ 
  return atan2(cimag(z), creal(z)); 
} 
#endif 
 
#if !HAVE_CONJ 
double complex conj(double complex z) 
{ 
  return ~z; 
} 
#endif 
 
/* -------------------------------- */



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

#define DEFAULT_RATIONALIZE_ERROR 1.0e-12

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


static s7_Int c_lcm(s7_Int a, s7_Int b)
{
  if ((a == 0) || (b == 0)) return(0);
  if (a < 0) a = -a;
  if (b < 0) b = -b;
  return((a / c_gcd(a, b)) * b);
}

static bool c_rationalize(double ux, double error, off_t *numer, off_t *denom)
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
  for (ctr = 0; ctr < 100; ctr++)
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

#if 0
static bool c_rationalize(double ux, double error, off_t *n, off_t *d)
{
  off_t numer, denom, lim, sign = 0;
  lim = 1.0 / error;
  if (ux < 0.0)
    {
      ux = -ux;
      sign = 1;
    }

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
#endif

s7_pointer s7_rationalize(s7_scheme *sc, double x, double error)
{
  s7_Int numer = 0, denom = 1;
  if (c_rationalize(x, error, &numer, &denom))
    return(s7_make_ratio(sc, numer, denom));
#if S7_DEBUGGING
  else fprintf(stderr, "rationalize(%lf, %lf) did not converge?\n", x, error);
#endif
  return(s7_make_real(sc, x));
}



static double num_to_real(num n)
{
  if (n.type >= NUM_REAL)
    return(real(n));
  if (n.type == NUM_INT)
    return((double)integer(n));
  return(fraction(n));
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

static double num_to_real_part(num n)
{
  if (n.type >= NUM_COMPLEX)
    return(real_part(n));
  if (n.type >= NUM_REAL)
    return(real(n));
  if (n.type == NUM_RATIO)
    return(fraction(n));
  return((double)integer(n));
}

static double num_to_imag_part(num n)
{
  if (n.type >= NUM_COMPLEX)
    return(imag_part(n));
  return(0.0);
}

static num make_ratio(s7_scheme *sc, s7_Int numer, s7_Int denom)
{
  num ret;
  s7_Int divisor;

#if S7_DEBUGGING
  if (denom == 0)
    {
      fprintf(stderr, "make ratio %lld %lld\n", numer, denom);
      abort();
    }
#endif

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

static num make_complex(double rl, double im)
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
  set_type(x, T_NUMBER | T_ATOM);

  x->object.number.type = NUM_INT;
  integer(x->object.number) = n;

  return(x);
}

s7_pointer s7_make_real(s7_scheme *sc, double n) 
{
  s7_pointer x = new_cell(sc);
  set_type(x, T_NUMBER | T_ATOM);

  x->object.number.type = NUM_REAL;
  real(x->object.number) = n;

  return(x);
}


s7_pointer s7_make_complex(s7_scheme *sc, double a, double b)
{
  num ret;
  s7_pointer x = new_cell(sc);
  set_type(x, T_NUMBER | T_ATOM);
  ret = make_complex(a, b);

  x->object.number.type = ret.type;
  if (ret.type == NUM_REAL)
    real(x->object.number) = real_part(ret);
  else
    {
      real_part(x->object.number) = real_part(ret);
      imag_part(x->object.number) = imag_part(ret);
    }
  return(x);
}

s7_pointer s7_make_ratio(s7_scheme *sc, s7_Int a, s7_Int b)
{
  /* make_number calls us, so we can't call it as a convenience! */

  num ret;
  s7_pointer x = new_cell(sc);
  set_type(x, T_NUMBER | T_ATOM);
  ret = make_ratio(sc, a, b);

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

double s7_real_part(s7_pointer x)
{
  return(num_to_real_part(x->object.number));
}

double s7_imag_part(s7_pointer x)
{
  return(num_to_imag_part(x->object.number));
}

s7_Int s7_integer(s7_pointer p)
{
  return(integer(p->object.number));
}

double s7_real(s7_pointer p)
{
  return(real(p->object.number));
}

static double complex s7_complex(s7_pointer p)
{
  return(num_to_real_part(p->object.number) + num_to_imag_part(p->object.number) * _Complex_I);
}

static s7_pointer s7_from_c_complex(s7_scheme *sc, double complex z)
{
  return(s7_make_complex(sc, creal(z), cimag(z)));
}


static num num_max(s7_scheme *sc, num a, num b) 
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
	ret = make_ratio(sc, num_to_numerator(a), num_to_denominator(a));
      else ret = make_ratio(sc, num_to_numerator(b), num_to_denominator(b));
      break;

    default:
      if (num_to_real(a) >= num_to_real(b))
	real(ret) = num_to_real(a);
      else real(ret) = num_to_real(b);
      break;
    }

  return(ret);
}


static num num_min(s7_scheme *sc, num a, num b) 
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
	ret = make_ratio(sc, num_to_numerator(a), num_to_denominator(a));
      else ret = make_ratio(sc, num_to_numerator(b), num_to_denominator(b));
      break;

    default:
      if (num_to_real(a) < num_to_real(b))
	real(ret) = num_to_real(a);
      else real(ret) = num_to_real(b);
      break;
    }

  return(ret);
}


static num num_add(s7_scheme *sc, num a, num b) 
{
  num ret;
  ret.type = a.type | b.type;

  switch (num_type(ret))
    {
    case NUM_INT: 
      integer(ret) = integer(a) + integer(b);
      break;

    case NUM_RATIO:
      ret = make_ratio(sc,
		       (num_to_numerator(a) * num_to_denominator(b)) + (num_to_denominator(a) * num_to_numerator(b)),
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


static num num_sub(s7_scheme *sc, num a, num b) 
{
  num ret;
  ret.type = a.type | b.type;

  switch (num_type(ret))
    {
    case NUM_INT: 
      integer(ret) = integer(a) - integer(b);
      break;

    case NUM_RATIO:
      ret = make_ratio(sc,
		       (num_to_numerator(a) * num_to_denominator(b)) - (num_to_denominator(a) * num_to_numerator(b)),
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


static num num_mul(s7_scheme *sc, num a, num b) 
{
  num ret;
  ret.type = a.type | b.type;

  switch (num_type(ret))
    {
    case NUM_INT: 
      integer(ret) = integer(a) * integer(b);
      break;

    case NUM_RATIO:
      ret = make_ratio(sc,
		       (num_to_numerator(a) * num_to_numerator(b)),
		       (num_to_denominator(a) * num_to_denominator(b)));
      break;

    case NUM_REAL2:
    case NUM_REAL:
      real(ret) = num_to_real(a) * num_to_real(b);
      break;
      
    default:
      {
	double r1, r2, i1, i2;
	r1 = num_to_real_part(a);
	r2 = num_to_real_part(b);
	i1 = num_to_imag_part(a);
	i2 = num_to_imag_part(b);
	ret = make_complex(r1 * r2 - i1 * i2, r1 * i2 + r2 * i1);
      }
      break;
    }

  return(ret);
}

static num num_div(s7_scheme *sc, num a, num b) 
{
  num ret;
  ret.type = a.type | b.type;

  switch (num_type(ret))
    {
    case NUM_INT: 
      ret = make_ratio(sc, integer(a), integer(b));
      break;

    case NUM_RATIO:
      ret = make_ratio(sc,
		       (num_to_numerator(a) * num_to_denominator(b)),
		       (num_to_denominator(a) * num_to_numerator(b)));
      break;

    case NUM_REAL2:
    case NUM_REAL:
      {
	double rb;
	rb = num_to_real(b);
	real(ret) = num_to_real(a) / rb;
      }
      break;
      
    default:
      {
	double r1, r2, i1, i2, den;
	r1 = num_to_real_part(a);
	r2 = num_to_real_part(b);
	i1 = num_to_imag_part(a);
	i2 = num_to_imag_part(b);
	den = (r2 * r2 + i2 * i2);
	ret = make_complex((r1 * r2 + i1 * i2) / den, (r2 * i1 - r1 * i2) / den);
      }
      break;
    }

  return(ret);
}


static s7_Int s7_truncate(double xf)
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

static num num_rem(s7_scheme *sc, num a, num b) 
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
      ret = make_ratio(sc, 
		       numerator(a) * denominator(b) - numerator(b) * denominator(a) * integer(num_quotient(a, b)),
		       denominator(a) * denominator(b));
      break;
    default:
      real(ret) = num_to_real(a) - num_to_real(b) * integer(num_quotient(a, b));
      break;
    }
  return(ret);
}

static num num_mod(s7_scheme *sc, num a, num b) 
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
      ret = make_ratio(sc, 
		       numerator(a) * denominator(b) - numerator(b) * denominator(a) * (s7_Int)floor(num_to_real(a) / num_to_real(b)),
		       denominator(a) * denominator(b));
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
      if (num_type(b) != NUM_RATIO)
	return(false);
      return((numerator(a) == numerator(b)) &&
	     (denominator(a) == denominator(b)));
      break;

    case NUM_REAL2:
    case NUM_REAL:    
      switch (num_type(b))
	{
	case NUM_INT:
	  return(real(a) == integer(b));
	case NUM_RATIO:
	  return(false);
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
	  return(false);
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
  if ((num_type(a) == NUM_INT) &&
      (num_type(b) == NUM_INT))
    return(integer(a) > integer(b));
  return(num_to_real(a) > num_to_real(b));
}

static bool num_lt(num a, num b) 
{
  if ((num_type(a) == NUM_INT) &&
      (num_type(b) == NUM_INT))
    return(integer(a) < integer(b));
  return(num_to_real(a) < num_to_real(b));
}

static bool num_ge(num a, num b) 
{
  return(!num_lt(a, b));
}

static bool num_le(num a, num b) 
{
  return(!num_gt(a, b));
}


/* Round to nearest. Round to even if midway */
static double round_per_R5RS(double x) 
{
  double fl = floor(x);
  double ce = ceil(x);
  double dfl = x - fl;
  double dce = ce - x;

  if (dfl > dce) return(ce);
  if (dfl < dce) return(fl);
  if (fmod(fl, 2.0) == 0.0) return(fl);
  return(ce);
}

static long binary_decode(const char *s) 
{
  long x = 0;
  while(*s != 0 && (*s == '1' || *s == '0')) 
    {
      x <<= 1;
      x += *s - '0';
      s++;
    }
  return x;
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


static void num2str(char *p, s7_Int n, int radix)
{
  static char dignum[] = "0123456789abcdef";
  int i, sign, len, end = 0;
  if ((radix < 2) || (radix > 16))
    return;
  sign = (n < 0);
  n = abs(n);
  len = (int)(floor(log(n) / log(radix)));
  if (sign)
    {
      p[0] = '-';
      len++;
      end++;
    }
  p[len + 1] = '\0';
  for (i = len; i >= end; i--)
    {
      p[i] = dignum[n % radix];
      n /= radix;
    }
}

char *s7_number_to_string(s7_scheme *sc, s7_pointer obj, int radix)
{
  char *p;
  p = (char *)CALLOC(256, sizeof(char));

  switch (object_number_type(obj))
    {
    case NUM_INT:
      switch (radix)
	{
	case 10:
	  snprintf(p, 256, s7_Int_d, s7_integer(obj));
	  break;
	case 8:
	  {
	    bool sign;
	    unsigned int x;
	    sign = s7_is_negative(obj);
	    x = (unsigned int)abs(s7_integer(obj));
	    snprintf(p, 256, "%s%o", (sign) ? "-" : "", x);
	  }
	  break;
	case 16:
	  snprintf(p, 256, "%llx", s7_integer(obj));
	  break;
	default:
	  num2str(p, s7_integer(obj), radix);
	  break;
	}
      break;

    case NUM_RATIO:
      snprintf(p, 256, s7_Int_d "/" s7_Int_d, s7_numerator(obj), s7_denominator(obj));
      break;

    case NUM_REAL2:
    case NUM_REAL:
      {
	int i, len;
	snprintf(p, 256, "%.14g", s7_real(obj));
	len = safe_strlen(p);
	for (i = 0; i < len; i++)  /* make it explicitly a float! */
	  if (p[i] == '.') break;
	if (i == len)
	  {
	    p[i]='.';
	    p[i+1]='0';
	    p[i+2]='\0';
	  }
      }
      break;

    default:
      if (s7_imag_part(obj) >= 0.0)
	snprintf(p, 256, "%.14g+%.14gi", s7_real_part(obj), s7_imag_part(obj));
      else snprintf(p, 256, "%.14g-%.14gi", s7_real_part(obj), fabs(s7_imag_part(obj)));
      break;

    }
  return(p);
}


static s7_pointer g_number_to_string(s7_scheme *sc, s7_pointer args)
{
  #define H_number_to_string "(number->string num :optional (radix 10)) converts num into a string"
  int radix = 0;
  char *res;
  if (!s7_is_number(car(args)))
    return(s7_wrong_type_arg_error(sc, "number->string", 1, car(args), "a number"));
  if (s7_is_pair(cdr(args)))
    {
      if (s7_is_integer(cadr(args)))
	radix = s7_integer(cadr(args));
      if ((radix < 2) || (radix > 16))
	return(s7_out_of_range_error(sc, "number->string", 2, cadr(args), "between 2 and 16"));
    }
  else radix = 10;
  res = s7_number_to_string(sc, car(args), radix);
  sc->y = s7_make_string(sc, res);
  FREE(res);
  return(sc->y);
}

      
/* make symbol or number atom from string */

static s7_pointer make_atom(s7_scheme *sc, char *q, int radix) 
{
  char c, *p, *slash = NULL, *plus = NULL;
  bool has_dec_point = false, has_slash = false, has_i = false, has_previous_dec_point = false; 
  int has_plus_or_minus = 0;
  bool has_fp_exp = false;

  p = q;
  c = *p++; 

  /* a number starts with + - . or digit, but so does 1+ for example */

  if ((c == '+') || (c == '-')) 
    { 
      c = *p++; 
      if (c == '.') 
	{ 
	  has_dec_point = true; 
	  c = *p++; 
	} 
      if (!isdigit(c)) 
	return(s7_make_symbol(sc, string_downcase(q))); 
    } 
  else 
    {
      if (c == '.') 
	{ 
	  has_dec_point = true; 
	  c = *p++; 
	  if (!isdigit(c)) 
	    return(s7_make_symbol(sc, string_downcase(q))); 
	} 
      else 
	{
	  if (!isdigit(c)) 
	    return(s7_make_symbol(sc, string_downcase(q))); 
	}
    }

  for ( ; (c = *p) != 0; ++p) 
    {
      if (!isdigit(c)) 
	{
	  if (c =='.') 
	    {
	      if (!has_dec_point) 
		{
		  has_dec_point = true;
		  continue;
		}
	    }
	  else 
	    {
	      if ((c == 'e') || (c == 'E')) 
		{
		  if (!has_fp_exp) 
		    {
		      has_dec_point = true; /* decimal point illegal from now on */
		      p++;
		      if ((*p == '-') || (*p == '+') || isdigit(*p)) 
			{
			  continue;
			}
		    }  
		}
	      else
		{
		  if ((c == '+') || (c == '-'))
		    {
		      if ((has_slash) || (has_plus_or_minus != 0) || (has_fp_exp))
			return(s7_make_symbol(sc, string_downcase(q)));

		      if (c == '+') has_plus_or_minus = 1; else has_plus_or_minus = -1;
		      has_previous_dec_point = has_dec_point;
		      has_dec_point = false;
		      plus = (char *)(p + 1);
		      continue;
		    }
		  else
		    {
		      if (c == '/')
			{
			  if ((has_dec_point) || (has_plus_or_minus != 0) || (has_slash) || (has_fp_exp) || (has_previous_dec_point))
			    return(s7_make_symbol(sc, string_downcase(q)));

			  has_slash = true;
			  slash = (char *)(p + 1);
			  continue;
			}
		      else
			{
			  if ((has_plus_or_minus != 0) && (!has_i) && (c == 'i'))
			    {
			      if (has_slash)
				return(s7_make_symbol(sc, string_downcase(q)));
			      has_i = true;
			      continue;
			    }
			}
		    }
		}
	    }
	  return(s7_make_symbol(sc, string_downcase(q)));
	}
    }

  if ((has_plus_or_minus != 0) &&
      (!has_i))
    return(s7_make_symbol(sc, string_downcase(q)));

  if ((has_slash) &&
      ((has_dec_point) || 
       (has_previous_dec_point) ||
       (has_plus_or_minus != 0)))
    return(s7_make_symbol(sc, string_downcase(q)));

  if (has_i)
    {
      int len;
      len = safe_strlen(q);

      if (q[len - 1] != 'i')
	return(s7_make_symbol(sc, string_downcase(q)));

      /* look for cases like 1+i */
      if ((q[len - 2] == '+') || (q[len - 2] == '-'))
	q[len - 1] = '1';
      else q[len - 1] = '\0'; /* remove 'i' */
      
      if (has_previous_dec_point)
	{
	  if (has_dec_point)
	    {
	      /* both are floats */
	      if (has_plus_or_minus == 1)
		return(s7_make_complex(sc, atof(q), atof(plus)));
	      return(s7_make_complex(sc, atof(q), -atof(plus)));
	    }
	  else
	    {
	      /* first was float */
	      if (has_plus_or_minus == 1)
		return(s7_make_complex(sc, atof(q), atoll(plus)));
	      return(s7_make_complex(sc, atof(q), -atoll(plus)));
	    }
	}
      else
	{
	  if (has_dec_point)
	    {
	      /* second is float */
	      if (has_plus_or_minus == 1)
		return(s7_make_complex(sc, atoll(q), atof(plus)));
	      return(s7_make_complex(sc, atoll(q), -atof(plus)));
	    }
	  else
	    {
	      /* both are ints */
	      if (has_plus_or_minus == 1)
		return(s7_make_complex(sc, atoll(q), atoll(plus)));
	      return(s7_make_complex(sc, atoll(q), -atoll(plus)));
	    }
	}
    }

  if (has_dec_point)
    {
      if ((has_plus_or_minus != 0) || (has_previous_dec_point))
	return(s7_make_symbol(sc, string_downcase(q)));

      return(s7_make_real(sc, atof(q)));
    }
  
  if (has_slash)
    return(s7_make_ratio(sc, atoll(q), atoll(slash)));

  return(s7_make_integer(sc, atoll(q)));
}

static s7_pointer s7_string_to_number(s7_scheme *sc, char *str, int radix)
{
  /* TODO: this actually isn't right -- in string->number there's no "#x" or whatever */
  s7_pointer x;
  x = make_atom(sc, str, radix);
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
  if (s7_is_pair(cdr(args)))
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


/* make constant */
static s7_pointer make_sharp_const(s7_scheme *sc, char *name) 
{
  long x;
  char tmp[256];

  if (strcmp(name, "t") == 0)
    return(sc->T);

  if (strcmp(name, "f") == 0)
    return(sc->F);

  if (*name == 'o') /* #o (octal) */
    {
      snprintf(tmp, sizeof(tmp), "0%s", name + 1);
      sscanf(tmp, "%lo", &x);
      return(s7_make_integer(sc, x));
    }

  if (*name == 'd') /* #d (decimal) */
    {    
      sscanf(name + 1, "%ld", &x);
      return(s7_make_integer(sc, x));
    } 

  if (*name == 'x') /* #x (hex) */
    {    
      snprintf(tmp, sizeof(tmp), "0x%s", name + 1);
      sscanf(tmp, "%lx", &x);
      return(s7_make_integer(sc, x));
    } 

  if (*name == 'b') /* #b (binary) */
    {    
      x = binary_decode(name + 1);
      return(s7_make_integer(sc, x));
    } 

  if (*name == 'i')  /* #i<num> = ->inexact (see token, is_one_of for table of choices here) */
    {
      double xf;
      sscanf(name + 1, "%lf", &xf);
      return(s7_make_real(sc, xf));
    }

  if (*name == 'e')  /* #e<num> = ->exact */
    {
      double xf;
      s7_Int numer = 0, denom = 1;
      sscanf(name + 1, "%lf", &xf);
      if (c_rationalize(xf, DEFAULT_RATIONALIZE_ERROR, &numer, &denom))
	return(s7_make_ratio(sc, numer, denom));
      return(s7_make_integer(sc, (s7_Int)xf));
    }

  if (*name == '\\')  /* #\w (character) */
    { 
      int c = 0;
      if (strcasecmp(name + 1, "space") == 0) 
	{
	  c =' ';
	} 
      else if (strcasecmp(name + 1, "newline") == 0)
	{
	  c ='\n';
	} 
      else if (strcasecmp(name + 1, "return") == 0) 
	{
	  c ='\r';
	} 
      else if (strcasecmp(name + 1, "tab") == 0) 
	{
	  c ='\t';
	} 
      else if (strcasecmp(name + 1, "null") == 0) 
	{
	  c ='\0';
	} 
      else if ((name[1] == 'x') && (name[2] != 0))
	{
	  int c1= 0;
	  if ((sscanf(name + 2, "%x", &c1) == 1) && 
	      (c1 < 256))
	    {
	      c = c1;
	    } 
	  else 
	    {
	      return(sc->NIL);
	    }
	} 
      else if (name[2] == 0) 
	{
	  c = name[1];
	} 
      else 
	{
	  return(sc->NIL);
	}
      return(s7_make_character(sc, c));
    }
  return(sc->NIL);
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


/* -------- tie into eval -------- */

static s7_pointer g_make_polar(s7_scheme *sc, s7_pointer args)
{
  double ang, mag;
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
      real(sc->v) = fabs(real(sc->v)); 
      break;
    case NUM_INT:
      integer(sc->v) = abs(integer(sc->v));
      break;
    case NUM_RATIO:
      numerator(sc->v) = abs(numerator(sc->v));
      break;
    default:
      {
	double a, b;
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
  double err;
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
    real(sc->v) = fabs(real(sc->v));
  else
    {
      if (num_type(sc->v) == NUM_INT)
	integer(sc->v) = abs(integer(sc->v));
      else numerator(sc->v) = abs(numerator(sc->v));
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
  #define H_log "(log z1 z2) returns log(z1) / log(z2) where z2 defaults to e"

  if (!s7_is_number(car(args)))
    return(s7_wrong_type_arg_error(sc, "log", 1, car(args), "a number"));
  if ((s7_is_pair(cdr(args))) &&
      (!(s7_is_number(cadr(args)))))
    return(s7_wrong_type_arg_error(sc, "log", 2, cadr(args), "a number"));

  sc->x = car(args);
  if (s7_is_pair(cdr(args)))
    {
      sc->y = cadr(args);
      if ((s7_is_real(sc->x)) &&
	  (s7_is_real(sc->y)) &&
	  (num_to_real(sc->x->object.number) > 0.0))
	return(s7_make_real(sc, log(num_to_real(sc->x->object.number)) / log(num_to_real(sc->y->object.number))));
      /* if < 0 use log(-x) + pi*i */
      return(s7_from_c_complex(sc, clog(s7_complex(sc->x)) / clog(s7_complex(sc->y))));
    }
  else
    {
      if ((s7_is_real(sc->x)) &&
	  (num_to_real(sc->x->object.number) > 0.0))
	return(s7_make_real(sc, log(num_to_real(sc->x->object.number))));
      /* if < 0 use log(-x) + pi*i */
      return(s7_from_c_complex(sc, clog(s7_complex(sc->x))));
    }
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
  return(s7_from_c_complex(sc, ctan(s7_complex(sc->x))));
}

static s7_pointer g_asin(s7_scheme *sc, s7_pointer args)
{
  #define H_asin "(asin z) returns asin(z)"
  if (!s7_is_number(car(args)))
    return(s7_wrong_type_arg_error(sc, "asin", 1, car(args), "a number"));
  sc->x = car(args);
  if ((s7_is_real(sc->x)) &&
      (fabs(num_to_real(sc->x->object.number)) <= 1.0))
    return(s7_make_real(sc, asin(num_to_real(sc->x->object.number))));
  return(s7_from_c_complex(sc, casin(s7_complex(sc->x))));
}

static s7_pointer g_acos(s7_scheme *sc, s7_pointer args)
{
  #define H_acos "(acos z) returns acos(z)"
  if (!s7_is_number(car(args)))
    return(s7_wrong_type_arg_error(sc, "acos", 1, car(args), "a number"));
  sc->x = car(args);
  if ((s7_is_real(sc->x)) &&
      (fabs(num_to_real(sc->x->object.number)) <= 1.0))
    return(s7_make_real(sc, acos(num_to_real(sc->x->object.number))));
  return(s7_from_c_complex(sc, cacos(s7_complex(sc->x))));
}

static s7_pointer g_atan(s7_scheme *sc, s7_pointer args)
{
  #define H_atan "(atan z) returns atan(z)"
  sc->x = car(args);
  if (!s7_is_pair(cdr(args)))
    {
      if (!s7_is_number(sc->x))
	return(s7_wrong_type_arg_error(sc, "atan", 1, sc->x, "a number"));
      if (s7_is_real(sc->x))
	return(s7_make_real(sc, atan(num_to_real(sc->x->object.number))));
      return(s7_from_c_complex(sc, catan(s7_complex(sc->x))));
    } 
  else 
    {
      sc->y = cadr(args);
      if (!s7_is_real(sc->x))
	return(s7_wrong_type_arg_error(sc, "atan", 1, sc->x, "a real"));
      if (!s7_is_real(sc->y))
	return(s7_wrong_type_arg_error(sc, "atan", 2, sc->y, "a real"));
      return(s7_make_real(sc, atan2(num_to_real(sc->x->object.number), 
				    num_to_real(sc->y->object.number))));
    }
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
      (fabs(num_to_real(sc->x->object.number)) < 1.0))
    return(s7_make_real(sc, atanh(num_to_real(sc->x->object.number))));
  return(s7_from_c_complex(sc, catanh(s7_complex(sc->x))));
}

static s7_pointer g_sqrt(s7_scheme *sc, s7_pointer args)
{
  #define H_sqrt "(sqrt z) returns sqrt(z)"
  if (!s7_is_number(car(args)))
    return(s7_wrong_type_arg_error(sc, "sqrt", 1, car(args), "a number"));
  sc->x = car(args);
  if ((s7_is_real(sc->x)) &&
      (num_to_real(sc->x->object.number) >= 0.0))
    return(s7_make_real(sc, sqrt(num_to_real(sc->x->object.number))));
  /* if < 0 use sqrt(-num)*i */
  return(s7_from_c_complex(sc, csqrt(s7_complex(sc->x))));
}


static s7_pointer g_expt(s7_scheme *sc, s7_pointer args)
{
  #define H_expt "(expt z1 z2) returns z1^z2"
  #define TOP_LOG 43.0 /* approx log(2^63) */

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

	  if (TOP_LOG > abs(s7_integer(sc->y)) * log(abs(s7_integer(sc->x)))) /* else over/underflow */
	    {
	      if ((s7_integer(sc->y) > 0) || 
		  (abs(s7_integer(sc->x)) == 1))
		return(s7_make_integer(sc, (s7_Int)pow(s7_integer(sc->x), s7_integer(sc->y))));
	      
	      return(s7_make_ratio(sc, 1, (s7_Int)pow(s7_integer(sc->x), -s7_integer(sc->y))));
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

	      if ((TOP_LOG > log(abs(n)) * abs(p)) &&
		  (TOP_LOG > log(d) * abs(p)))	
		{
		  if (p > 0)
		    return(s7_make_ratio(sc, (s7_Int)pow(n, p), (s7_Int)pow(d, p)));
		  return(s7_make_ratio(sc, (s7_Int)pow(d, -p), (s7_Int)pow(n, -p)));
		}
	    }
	  /* occasionally int^rat can be int but it happens so infrequently it's not worth checking */
	}
    }
	
  if ((s7_is_real(sc->x)) &&
      (s7_is_real(sc->y)))
    {
      double x, y;
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
  s7_Int val = 1;
  #define H_lcm "(lcm ...) returns the least common multiple of its arguments"

  for (i = 1, sc->x = args; sc->x != sc->NIL; i++, sc->x = cdr(sc->x)) 
    if (!s7_is_integer(car(sc->x)))
      return(s7_wrong_type_arg_error(sc, "lcm", i, car(sc->x), "an integer"));

  for (sc->x = args; sc->x != sc->NIL; sc->x = cdr(sc->x)) 
    {
      val = c_lcm(val, s7_integer(car(sc->x)));
      if (val == 0)
	return(s7_make_integer(sc, 0));
    }
  return(s7_make_integer(sc, val));
}

static s7_pointer g_gcd(s7_scheme *sc, s7_pointer args)
{
  int i;
  s7_Int val = 0;
  #define H_gcd "(gcd ...) returns the greatest common divisor of its arguments"

  for (i = 1, sc->x = args; sc->x != sc->NIL; i++, sc->x = cdr(sc->x)) 
    if (!s7_is_integer(car(sc->x)))
      return(s7_wrong_type_arg_error(sc, "gcd", i, car(sc->x), "an integer"));

  for (sc->x = args; sc->x != sc->NIL; sc->x = cdr(sc->x)) 
    {
      val = c_gcd(val, s7_integer(car(sc->x)));
      if (val == 1)
	return(s7_make_integer(sc, 1));
    }
  return(s7_make_integer(sc, val));
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
      sc->v = num_add(sc, sc->v, nvalue(car(sc->x)));
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
      sc->v = num_sub(sc, sc->v, nvalue(car(sc->x)));
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
      sc->v = num_mul(sc, sc->v, nvalue(car(sc->x)));
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
	return(s7_division_by_zero_error(sc, "/", car(sc->x)));

      sc->v = num_div(sc, sc->v, nvalue(car(sc->x)));
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
      sc->v = num_max(sc, sc->v, nvalue(car(sc->x)));
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
      sc->v = num_min(sc, sc->v, nvalue(car(sc->x)));
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
  else return(s7_division_by_zero_error(sc, "quotient", cadr(args)));
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
    sc->v = num_rem(sc, sc->v, nvalue(cadr(args)));
  else return(s7_division_by_zero_error(sc, "remainder", cadr(args)));
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
    sc->v = num_mod(sc, sc->v, nvalue(cadr(args)));
  else return(s7_division_by_zero_error(sc, "modulo", cadr(args)));
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
  
  if (!arg_checker(car(args)))
    s7_wrong_type_arg_error(sc, op_name, 1, car(args), arg_type);

  sc->v = nvalue(car(args));
  for (i = 2, sc->x = cdr(args); sc->x != sc->NIL; i++, sc->x = cdr(sc->x)) 
    {
      if (!arg_checker(car(sc->x)))
	s7_wrong_type_arg_error(sc, op_name, i, car(sc->x), arg_type);

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
  return(to_s7_bool(sc, s7_is_number(car(args))));
}

static s7_pointer g_is_integer(s7_scheme *sc, s7_pointer args) 
{
  #define H_is_integer "(integer? obj) returns #t if obj is an integer"
  return(to_s7_bool(sc, s7_is_integer(car(args))));
}

static s7_pointer g_is_real(s7_scheme *sc, s7_pointer args) 
{
  #define H_is_real "(real? obj) returns #t if obj is a real number"
  return(to_s7_bool(sc, s7_is_real(car(args))));
}

static s7_pointer g_is_complex(s7_scheme *sc, s7_pointer args) 
{
  #define H_is_complex "(complex? obj) returns #t if obj is a complex number"
  return(to_s7_bool(sc, s7_is_complex(car(args))));
}

static s7_pointer g_is_rational(s7_scheme *sc, s7_pointer args) 
{
  #define H_is_rational "(rational? obj) returns #t if obj is a rational number"
  return(to_s7_bool(sc, s7_is_rational(car(args))));
}

static s7_pointer g_is_even(s7_scheme *sc, s7_pointer args)
{
  #define H_is_even "(even? int) returns #t if the integer int is even"
  if (!s7_is_integer(car(args)))
    return(s7_wrong_type_arg_error(sc, "even?", 1, car(args), "an integer"));
  return(to_s7_bool(sc, (s7_integer(car(args)) & 1) == 0));
}

static s7_pointer g_is_odd(s7_scheme *sc, s7_pointer args)
{
  #define H_is_odd "(odd? int) returns #t if the integer int is odd"
  if (!s7_is_integer(car(args)))
    return(s7_wrong_type_arg_error(sc, "odd?", 1, car(args), "an integer"));
  return(to_s7_bool(sc, (s7_integer(car(args)) & 1) == 1));
}

static s7_pointer g_is_zero(s7_scheme *sc, s7_pointer args)
{
  #define H_is_zero "(zero? num) returns #t if the number num is zero"
  if (!s7_is_number(car(args)))
    return(s7_wrong_type_arg_error(sc, "zero?", 1, car(args), "a number"));
  return(to_s7_bool(sc, s7_is_zero(car(args))));
}

static s7_pointer g_is_positive(s7_scheme *sc, s7_pointer args)
{
  #define H_is_positive "(positive? num) returns #t if the real number num is positive"
  if (!s7_is_real(car(args)))
    return(s7_wrong_type_arg_error(sc, "positive?", 1, car(args), "a real"));
  return(to_s7_bool(sc, s7_is_positive(car(args))));
}

static s7_pointer g_is_negative(s7_scheme *sc, s7_pointer args)
{
  #define H_is_negative "(negative? num) returns #t if the real number num is negative"
  if (!s7_is_real(car(args)))
    return(s7_wrong_type_arg_error(sc, "negative?", 1, car(args), "a real"));
  return(to_s7_bool(sc, s7_is_negative(car(args))));
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
  if (c_rationalize(s7_real(sc->x), DEFAULT_RATIONALIZE_ERROR, &numer, &denom))
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
    return(s7_make_real(sc, (double)s7_integer(sc->x)));
  return(s7_make_real(sc, fraction(sc->x->object.number)));
}

static s7_pointer g_is_exact(s7_scheme *sc, s7_pointer args)
{
  #define H_is_exact "(exact? num) returns #t if num is exact"
  if (!s7_is_number(car(args)))
    return(s7_wrong_type_arg_error(sc, "exact?", 1, car(args), "a number"));
  return(to_s7_bool(sc, s7_is_exact(car(args))));
}

static s7_pointer g_is_inexact(s7_scheme *sc, s7_pointer args)
{
  #define H_is_inexact "(inexact? num) returns #t if num is inexact"
  if (!s7_is_number(car(args)))
    return(s7_wrong_type_arg_error(sc, "inexact?", 1, car(args), "a number"));
  return(to_s7_bool(sc, s7_is_inexact(car(args))));
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
  if ((!s7_is_integer(car(args))) || (s7_integer(car(args)) < 0))
    return(s7_wrong_type_arg_error(sc, "integer->char", 1, car(args), "a non-negative integer"));
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

static int c_is_alpha(int c) 
{ 
  return(isascii(c) && isalpha(c));
}

static s7_pointer g_is_char_alphabetic(s7_scheme *sc, s7_pointer args)
{
  #define H_is_char_alphabetic "(char-alphabetic? c) returns #t if the character c is alphabetic"
  if (!s7_is_character(car(args)))
    return(s7_wrong_type_arg_error(sc, "char-alphabetic?", 1, car(args), "a character"));
  return(to_s7_bool(sc, c_is_alpha(character(car(args)))));
}

static int c_is_digit(int c) 
{ 
  return(isascii(c) && isdigit(c));
}

static s7_pointer g_is_char_numeric(s7_scheme *sc, s7_pointer args)
{
  #define H_is_char_numeric "(char-numeric? c) returns #t if the character c is a digit"
  if (!s7_is_character(car(args)))
    return(s7_wrong_type_arg_error(sc, "char-numeric?", 1, car(args), "a character"));
  return(to_s7_bool(sc, c_is_digit(character(car(args)))));
}

static int c_is_space(int c) 
{ 
  return(isascii(c) && isspace(c));
}

static s7_pointer g_is_char_whitespace(s7_scheme *sc, s7_pointer args)
{
  #define H_is_char_whitespace "(char-whitespace? c) returns #t if the character c is non-printing character"
  if (!s7_is_character(car(args)))
    return(s7_wrong_type_arg_error(sc, "char-whitespace?", 1, car(args), "a character"));
  return(to_s7_bool(sc, c_is_space(character(car(args)))));
}

static int c_is_upper(int c) 
{ 
  return(isascii(c) && isupper(c));
}

static s7_pointer g_is_char_upper_case(s7_scheme *sc, s7_pointer args)
{
  #define H_is_char_upper_case "(char-upper-case? c) returns #t if the character c is in upper case"
  if (!s7_is_character(car(args)))
    return(s7_wrong_type_arg_error(sc, "char-upper-case?", 1, car(args), "a character"));
  return(to_s7_bool(sc, c_is_upper(character(car(args)))));
}

static int c_is_lower(int c) 
{ 
  return(isascii(c) && islower(c));
}

static s7_pointer g_is_char_lower_case(s7_scheme *sc, s7_pointer args)
{
  #define H_is_char_lower_case "(char-lower-case? c) returns #t if the character c is in lower case"
  if (!s7_is_character(car(args)))
    return(s7_wrong_type_arg_error(sc, "char-lower-case?", 1, car(args), "a character"));
  return(to_s7_bool(sc, c_is_lower(character(car(args)))));
}


static s7_pointer g_is_char(s7_scheme *sc, s7_pointer args)
{
  #define H_is_char "(char? obj) returns #t if obj is a character"
  return(to_s7_bool(sc, s7_is_character(car(args))));
}


s7_pointer s7_make_character(s7_scheme *sc, int c) 
{
  s7_pointer x = new_cell(sc);
  set_type(x, T_CHARACTER | T_ATOM);
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
  bool happy = true;
  char last_chr;

  if (!s7_is_character(car(args)))
    return(s7_wrong_type_arg_error(sc, name, 1, car(args), "a character"));

  last_chr = character(car(args));
  for (i = 2, x = cdr(args); x != sc->NIL; i++, x = cdr(x))
    {
      if (!s7_is_character(car(x)))
	return(s7_wrong_type_arg_error(sc, name, i, car(x), "a character"));
      if (happy)
	{
	  happy = (charcmp(last_chr, character(car(x)), ci) == val);
	  last_chr = character(car(x));
	}
    }
  return(to_s7_bool(sc, happy));
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
  return(s7_not(sc, g_char_cmp(sc, args, -1, "char>=?", false)));
}	

static s7_pointer g_chars_are_leq(s7_scheme *sc, s7_pointer args)
{
  #define H_chars_are_leq "(char<=? chr...) returns #t if all the character arguments are equal or increasing"
  return(s7_not(sc, g_char_cmp(sc, args, 1, "char<=?", false)));
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
  return(s7_not(sc, g_char_cmp(sc, args, -1, "char-ci>=?", true)));
}	

static s7_pointer g_chars_are_ci_leq(s7_scheme *sc, s7_pointer args)
{
  #define H_chars_are_ci_leq "(char-ci<=? chr...) returns #t if all the character arguments are equal or increasing, ignoring case"
  return(s7_not(sc, g_char_cmp(sc, args, 1, "char-ci<=?", true)));
}	


/* -------------------------------- strings -------------------------------- */


/* get new string */
s7_pointer s7_make_counted_string(s7_scheme *sc, const char *str, int len) 
{
  s7_pointer x;

  x = new_cell(sc);

#if S7_DEBUGGING
  if (is_immutable(x))
    {
      fprintf(stderr, "oops %s\n", str);
      abort();
    }
  if (len != safe_strlen(str))
    {
      fprintf(stderr, "make_counted_string: %s len passed in %d is not true len %d\n", str, len, safe_strlen(str));
      abort();
    }
#endif

  set_type(x, T_STRING | T_ATOM | T_FINALIZABLE);
  local_protect(x);
  if (str)
    string_value(x) = copy_string(str);
  else string_value(x) = NULL;
  string_length(x) = len;
  local_unprotect(x);
  return(x);
}

static s7_pointer make_empty_string(s7_scheme *sc, int len, char fill) 
{
  s7_pointer x;
  x = new_cell(sc);
  set_type(x, T_STRING | T_ATOM | T_FINALIZABLE);
  local_protect(x);
  string_value(x) = (char *)CALLOC(len + 1, sizeof(char));
  if (fill != 0)
    memset((void *)(string_value(x)), fill, len);
  string_length(x) = len;
  local_unprotect(x);
  return(x);
}

s7_pointer s7_make_string(s7_scheme *sc, const char *str) 
{
  return(s7_make_counted_string(sc, str, safe_strlen(str)));
}

bool s7_is_string(s7_pointer p)
{
  return((type(p) == T_STRING)); 
}

char *s7_string(s7_pointer p) 
{ 
  return(string_value(p));
}

static s7_pointer g_is_string(s7_scheme *sc, s7_pointer args)
{
  #define H_is_string "(string? obj) returns #t if obj is a string"
  return(to_s7_bool(sc, s7_is_string(car(args))));
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

static s7_pointer s7_string_concatenate(s7_scheme *sc, const char *s1, const char *s2)
{
  int len;
  s7_pointer newstr;
  len = strlen(s1) + strlen(s2) + 8;
  newstr = make_empty_string(sc, len, 0);
  strcat(string_value(newstr), s2);
  strcat(string_value(newstr), ": ");
  strcat(string_value(newstr), s1);
  return(newstr);
}

static s7_pointer g_string_append_1(s7_scheme *sc, s7_pointer args, const char *name)
{
  int i, len = 0;
  s7_pointer newstr;
  char *pos;

  if (car(args) == sc->NIL)
    return(s7_make_string(sc, ""));
	
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
  if (s1 == NULL)
    {
      if (s2 == NULL)
	return(0);
      else return(-1);
    }
  else
    {
      if (s2 == NULL)
	return(1);
    }
  return(strcmp(s1, s2));
}


static s7_pointer g_string_cmp(s7_scheme *sc, s7_pointer args, int val, const char *name)
{
  int i;
  s7_pointer x;
  bool happy = true;
  const char *last_str = NULL;

  if (!s7_is_string(car(args)))
    return(s7_wrong_type_arg_error(sc, name, 1, car(args), "a string"));

  last_str = string_value(car(args));
  for (i = 2, x = cdr(args); x != sc->NIL; i++, x = cdr(x))
    {
      if (!s7_is_string(car(x)))
	return(s7_wrong_type_arg_error(sc, name, i, car(x), "a string"));
      if (happy)
	{
	  happy = (safe_strcmp(last_str, string_value(car(x))) == val);
	  last_str = string_value(car(x));
	}
    }
  return(to_s7_bool(sc, happy));
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
  return(s7_not(sc, g_string_cmp(sc, args, -1, "string>=?")));
}	

static s7_pointer g_strings_are_leq(s7_scheme *sc, s7_pointer args)
{
  #define H_strings_are_leq "(string<=? str...) returns #t if all the string arguments are equal or increasing"
  return(s7_not(sc, g_string_cmp(sc, args, 1, "string<=?")));
}	

static int safe_strcasecmp(const char *s1, const char *s2)
{
  int len1, len2;
  int i;
  if (s1 == NULL)
    {
      if (s2 == NULL)
	return(0);
      else return(-1);
    }
  else
    {
      if (s2 == NULL)
	return(1);
    }
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
  bool happy = true;
  const char *last_str = NULL;

  if (!s7_is_string(car(args)))
    return(s7_wrong_type_arg_error(sc, name, 1, car(args), "a string"));

  last_str = string_value(car(args));
  for (i = 2, x = cdr(args); x != sc->NIL; i++, x = cdr(x))
    {
      if (!s7_is_string(car(x)))
	return(s7_wrong_type_arg_error(sc, name, i, car(x), "a string"));
      if (happy)
	{
	  happy = (safe_strcasecmp(last_str, string_value(car(x))) == val);
	  last_str = string_value(car(x));
	}
    }
  return(to_s7_bool(sc, happy));
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
  return(s7_not(sc, g_string_ci_cmp(sc, args, -1, "string-ci>=?")));
}	

static s7_pointer g_strings_are_ci_leq(s7_scheme *sc, s7_pointer args)
{
  #define H_strings_are_ci_leq "(string-ci<=? str...) returns #t if all the string arguments are equal or increasing, ignoring case"
  return(s7_not(sc, g_string_ci_cmp(sc, args, 1, "string-ci<=?")));
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
    return(s7_make_string(sc, ""));
  return(g_string_1(sc, args, "string"));
}

static s7_pointer g_list_to_string(s7_scheme *sc, s7_pointer args)
{
  #define H_list_to_string "(list->string lst) appends all the lists characters into one string"
  if (car(args) == sc->NIL)
    return(s7_make_string(sc, ""));
  if (!s7_is_pair(car(args)))
    return(s7_wrong_type_arg_error(sc, "list->string", 1, car(args), "a list of characters"));
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

  sc->gc_off = true;
  for (i = 0; i < len; i++)
    lst = cons(sc, s7_make_character(sc, str[i]), lst);
  sc->gc_off = false;
  return(s7_reverse_in_place(sc, sc->NIL, lst));
}




/* -------------------------------- ports -------------------------------- */

static char *describe_port(s7_scheme *sc, s7_pointer p)
{
  char *desc;
  desc = (char *)CALLOC(64, sizeof(char));
  snprintf(desc, 64, "<port");

  if (is_file_port(p))
    strcat(desc, " file");
  else strcat(desc, " string");

  if (is_input_port(p))
    strcat(desc, " input");
  else strcat(desc, " output");

  if (port_is_closed(p))
    strcat(desc, " (closed)");

  strcat(desc, ">");
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
  if ((is_input_port(x)) &&
      (is_file_port(x)))
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
  return(to_s7_bool(sc, s7_is_input_port(sc, car(args))));
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
  return(to_s7_bool(sc, s7_is_output_port(sc, car(args))));
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
  if ((port == sc->NIL) ||
      (is_input_port(port)))
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
  if ((port == sc->NIL) ||
      (is_output_port(port)))
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
  if ((port == sc->NIL) ||
      (is_output_port(port)))
    sc->error_port = port;
  else return(s7_wrong_type_arg_error(sc, "set-current-error-port", 1, car(args), "an output port or nil"));
  return(old_port);
}


/* -------- char-ready? -------- */

static s7_pointer g_is_char_ready(s7_scheme *sc, s7_pointer args)
{
  #define H_is_char_ready "(char-ready? :optional port) returns #t if a character is ready for input on the given port"
  if (s7_is_pair(args))
    {
      s7_pointer pt = car(args);
      if (!s7_is_input_port(sc, pt))
	return(s7_wrong_type_arg_error(sc, "char-ready?", 1, car(args), "an input port"));

      return(to_s7_bool(sc, is_string_port(pt)));
    }
  return(to_s7_bool(sc, (is_input_port(sc->input_port)) && (is_string_port(sc->input_port))));  /* TODO: need to check for waiting input on files */
}      


/* -------- eof-object? -------- */

static s7_pointer g_is_eof_object(s7_scheme *sc, s7_pointer args)
{
  #define H_is_eof_object "(eof-object? val) returns #t is val is the end-of-file object"
  return(to_s7_bool(sc, car(args) == sc->EOF_OBJECT));
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
      /* fprintf(stderr, "---------------------------------------- close %s\n", port_filename(p)); */

      fclose(port_file(p));
      port_file(p) = NULL;
      port_needs_close(p) = false;
    }
  /* if input string, someone else is dealing with GC */
  port_is_closed(p) = true;
}

static s7_pointer g_close_input_port(s7_scheme *sc, s7_pointer args)
{
  #define H_close_input_port "(close-input-port port) closes the port"
  s7_pointer pt = car(args);
  if (!is_input_port(pt))
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
	  /* fprintf(stderr, "---------------------------------------- close %s\n", port_filename(p)); */

	  fclose(port_file(p));
	  port_file(p) = NULL;
	  port_needs_close(p) = false;
	}
    }
  else
    {
      if (port_string(p))
	{
	  FREE(port_string(p));
	  port_string(p) = NULL;
	}
    }
  port_is_closed(p) = true;
}

static s7_pointer g_close_output_port(s7_scheme *sc, s7_pointer args)
{
  #define H_close_output_port "(close-output-port port) closes the port"
  s7_pointer pt = car(args);
  if (!is_output_port(pt))
    return(s7_wrong_type_arg_error(sc, "close-output-port", 1, pt, "an output port"));
  s7_close_output_port(sc, pt);
  return(sc->UNSPECIFIED);
}


/* -------- open-input-file -------- */

static s7_pointer s7_make_input_file(s7_scheme *sc, const char *name, FILE *fp)
{
  s7_pointer x;
  x = new_cell(sc);
  set_type(x, T_INPUT_PORT | T_ATOM | T_FINALIZABLE);
  local_protect(x);

  /* set up the port struct */
  x->object.port = (rport *)MALLOC(sizeof(rport));
  port_file(x) = fp;
  is_file_port(x) = true;
  port_is_closed(x) = false;
  port_filename(x) = copy_string(name);
  port_line_number(x) = 0;
  port_paren_depth(x) = 0;
  port_needs_close(x) = false;
  local_unprotect(x);

  return(x);
}

s7_pointer s7_open_input_file(s7_scheme *sc, const char *name)
{
  FILE *fp;
  /* see if we can open this file before allocating a port */

  fp = fopen(name, "r");
  if (!fp)
    return(s7_file_error(sc, "open-input-file", "can't open", name));
  
  return(s7_make_input_file(sc, name, fp));
}


static s7_pointer g_open_input_file(s7_scheme *sc, s7_pointer args)
{
  #define H_open_input_file "(open-input-file filename) opens filename for reading"
  s7_pointer name = car(args);
  if (!s7_is_string(name))
    return(s7_wrong_type_arg_error(sc, "open-input-file", 1, car(args), "a string (a filename)"));
  
  return(s7_open_input_file(sc, s7_string(name)));
}


/* -------- open-output-file -------- */

s7_pointer s7_open_output_file(s7_scheme *sc, const char *name)
{
  FILE *fp;
  s7_pointer x;
  /* see if we can open this file before allocating a port */

  fp = fopen(name, "w");
  if (!fp)
    return(s7_file_error(sc, "open-output-file", "can't open", name));

  /* fprintf(stderr, "---------------------------------------- open %s\n", name); */

  x = new_cell(sc);
  set_type(x, T_OUTPUT_PORT | T_ATOM | T_FINALIZABLE);
  local_protect(x);

  /* set up the port struct */
  x->object.port = (rport *)MALLOC(sizeof(rport));
  is_file_port(x) = true;
  port_is_closed(x) = false;
  port_filename(x) = copy_string(name);
  port_line_number(x) = 0;
  port_paren_depth(x) = 0;
  port_needs_close(x) = false;
  port_file(x) = fp;
  local_unprotect(x);

  return(x);
}

static s7_pointer g_open_output_file(s7_scheme *sc, s7_pointer args)
{
  #define H_open_output_file "(open-output-file filename) opens filename for writing"
  s7_pointer name = car(args);
  if (!s7_is_string(name))
    return(s7_wrong_type_arg_error(sc, "open-output-file", 1, car(args), "a string (a filename)"));
  
  return(s7_open_output_file(sc, s7_string(name)));
}


/* -------- open-input-string -------- */

s7_pointer s7_open_input_string(s7_scheme *sc, const char *input_string)
{
  s7_pointer x;
  x = new_cell(sc);
  set_type(x, T_INPUT_PORT | T_ATOM | T_FINALIZABLE);
  local_protect(x);

  /* set up the port struct */
  x->object.port = (rport *)MALLOC(sizeof(rport));
  is_file_port(x) = false;
  port_is_closed(x) = false;
  port_paren_depth(x) = 0;
  port_string(x) = (char *)input_string;
  port_string_length(x) = safe_strlen(input_string);
  port_string_point(x) = 0;
  local_unprotect(x);

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
  set_type(x, T_OUTPUT_PORT | T_ATOM | T_FINALIZABLE);
  local_protect(x);

  /* set up the port struct */
  x->object.port = (rport *)MALLOC(sizeof(rport));
  is_file_port(x) = false;
  port_is_closed(x) = false;
  port_string_length(x) = STRING_PORT_INITIAL_LENGTH;
  port_string(x) = (char *)CALLOC(STRING_PORT_INITIAL_LENGTH, sizeof(char));
  port_string_point(x) = 0;
  local_unprotect(x);

  return(x);
}

static s7_pointer g_open_output_string(s7_scheme *sc, s7_pointer args)
{
  #define H_open_output_string "(open-output-string) opens an output string port"
  return(s7_open_output_string(sc));
}


/* -------- get-output-string -------- */

char *s7_get_output_string(s7_scheme *sc, s7_pointer p)
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
  sc->input_port_stack = cons(sc, sc->input_port, sc->input_port_stack);
  sc->input_port = new_port;
  return(sc->input_port != sc->NIL);
}

static s7_pointer pop_input_port(s7_scheme *sc)
{
  if (s7_is_pair(sc->input_port_stack))
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
  if (pt == sc->NIL) return(EOF);

  if (is_file_port(pt))
    {
      int c;
      c = fgetc(port_file(pt));
      if (c == '\n')
	port_line_number(pt)++;
      return(c);
    }
  else 
    {
      if ((!(port_string(pt))) ||
	  (port_string_length(pt) <= port_string_point(pt)))
	return(EOF);
      return(port_string(pt)[port_string_point(pt)++]);
    }
}


/* -------- put character back into input buffer -------- */

static void backchar(s7_scheme *sc, char c, s7_pointer pt) 
{
  if (pt == sc->NIL) return;

  if (is_file_port(pt))
    {
      ungetc(c, port_file(pt));
      if (c == '\n')
	port_line_number(pt)--;
    }
  else 
    {
      if (port_string_point(pt) > 0)
	port_string_point(pt)--;
    }
}


/* -------- read token or expression -------- */

/* check c is in chars */

static  bool is_one_of(const char *s, int c) 
{
  if (c == EOF) 
    return(true);

  while (*s)
    if (*s++ == c)
      return(true);
  return(false);
}

/* read characters up to delimiter, but cater to character constants */

static void resize_strbuf(s7_scheme *sc)
{
  int i, old_size;
  old_size = sc->strbuf_size;
  sc->strbuf_size *= 2;
  sc->strbuf = (char *)REALLOC(sc->strbuf, sc->strbuf_size * sizeof(char));
  for (i = old_size; i < sc->strbuf_size; i++) sc->strbuf[i] = '\0';
}

static char *read_string_upto(s7_scheme *sc, const char *delim, s7_pointer pt) 
{
  int i = 0;
  
  while (!is_one_of(delim, (sc->strbuf[i++] = inchar(sc, pt))))
    {
      if (i >= sc->strbuf_size)
	resize_strbuf(sc);
    }

  if ((i == 2) && 
      (sc->strbuf[0] == '\\'))
    sc->strbuf[i] = 0;
  else 
    {
      if (sc->strbuf[i - 1] != EOF)
	backchar(sc, sc->strbuf[i - 1], pt);
      sc->strbuf[i - 1] = '\0';
    }

  return(sc->strbuf);
}


/* read string expression "xxx...xxx" */

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
	      return(s7_make_counted_string(sc, sc->strbuf, i));

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


/* skip white characters */

static  void skipspace(s7_scheme *sc, s7_pointer pt) 
{
  int c;
  while (isspace(c = inchar(sc, pt)))
    ;
  if (c != EOF) 
    backchar(sc, c, pt);
}

/* get token */

#define token(Sc, Pt) token_1(Sc, Pt, __FUNCTION__, __LINE__)
static int token_1(s7_scheme *sc, s7_pointer pt, const char *func, int line)
{
  int c;
  skipspace(sc, pt);
  switch (c = inchar(sc, pt)) 
    {
    case EOF:
      /* fprintf(stderr, "otk %s %d ", func, line); */
      return(TOK_EOF);

    case '(':
      return(TOK_LPAREN);

    case ')':
      return(TOK_RPAREN);

    case '.':
      c = inchar(sc, pt);
      if (is_one_of(" \n\t", c)) 
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
	  last_char = c;
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
	  last_char = c;
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
      if (is_one_of(" tfodxbie\\", c)) 
	return TOK_SHARP_CONST;

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
  if (s7_is_pair(args))
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


/* -------- read -------- */

s7_pointer s7_read(s7_scheme *sc, s7_pointer port)
{
  if (s7_is_input_port(sc, port))
    {
      if (sc->stack_top == 0)
	{
	  if (setjmp(sc->goto_start) != 0)
	    {
	      return(sc->value);
	    }
	  sc->longjmp_ok = true;
	}
      push_input_port(sc, port);
      push_stack(sc, OP_READ_RETURN_EXPRESSION, port, sc->NIL);
      eval(sc, OP_READ_INTERNAL);
      pop_input_port(sc);
      return(sc->value);
    }
  return(s7_wrong_type_arg_error(sc, "read", 1, port, "an input port"));
}


static s7_pointer g_read(s7_scheme *sc, s7_pointer args)
{
  #define H_read "(read :optional port) returns the next object in the input port"
  s7_pointer port;

  if (s7_is_pair(args))
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
      char *new_name, *new_dir;
      int size;
      new_dir = s7_string(s7_list_ref(sc, lst, i));
      if (new_dir)
	{
	  FILE *fp;
	  size = name_len + strlen(new_dir) + 2;
	  new_name = (char *)CALLOC(size, sizeof(char));
	  snprintf(new_name, size, "%s/%s", new_dir, name);
	  fp = fopen(new_name, "r");
	  FREE(new_name);
	  if (fp) return(fp);
	}
    }
  return(NULL);
}

s7_pointer s7_load(s7_scheme *sc, const char *filename)
{
  s7_pointer port;
  FILE *fp;

  fp = fopen(filename, "r");
  if (!fp)
    fp = search_load_path(sc, filename);
  if (!fp)
    return(s7_file_error(sc, "open-input-file", "can't open", filename));

  port = s7_make_input_file(sc, filename, fp); 
  port_needs_close(port) = true;

  push_input_port(sc, port);
  push_stack(sc, OP_LOAD_RETURN_IF_EOF, port, sc->NIL);

  if (setjmp(sc->goto_start) != 0)
    {
      eval(sc, sc->op);
    }
  else 
    {
      sc->longjmp_ok = true;
      eval(sc, OP_READ_INTERNAL);
    }
  /* TODO: check paren count and below */
  
  pop_input_port(sc);
  s7_close_input_port(sc, port);

  return(sc->UNSPECIFIED);
}

static s7_pointer g_load(s7_scheme *sc, s7_pointer args)
{
  FILE *fp = NULL;
  s7_pointer name, port;
  char *fname;

  name = car(args);
  if (!s7_is_string(name))
    return(s7_wrong_type_arg_error(sc, "load", 1, car(args), "a string (a filename)"));

  fname = s7_string(name);

  fp = fopen(fname, "r");
  if (!fp)
    fp = search_load_path(sc, fname);
  if (!fp)
    return(s7_file_error(sc, "open-input-file", "can't open", fname));

  port = s7_make_input_file(sc, fname, fp);
  port_needs_close(port) = true;

  push_input_port(sc, port);
  push_stack(sc, OP_LOAD_CLOSE_AND_POP_IF_EOF, port, sc->NIL);
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
		      cons(sc, 
			   s7_make_string(sc, dir), 
			   s7_load_path(sc)));
  return(s7_load_path(sc));
}




/* -------- eval string -------- */

s7_pointer s7_eval_c_string(s7_scheme *sc, const char *str)
{
  s7_pointer port;
  /* fprintf(stderr, "eval c string: %s with top: %d\n", str, sc->stack_top); */
  stack_reset(sc); 
  sc->envir = sc->global_env;
  port = s7_open_input_string(sc, str);
  push_input_port(sc, port);
  push_stack(sc, OP_EVAL_STRING, sc->NIL, sc->NIL);

  if (setjmp(sc->goto_start) != 0)
    {
      eval(sc, sc->op);
    }
  else 
    {
      sc->longjmp_ok = true;
      eval(sc, OP_READ_INTERNAL);
    }
  pop_input_port(sc);
  s7_close_input_port(sc, port);
  return(sc->value);
}

static s7_pointer g_call_with_input_string(s7_scheme *sc, s7_pointer args)
{
  #define H_call_with_input_string "(call-with-input-string str thunk) opens a string port for str and applies thunk to it"

  /* (call-with-input-string "44" (lambda (p) (+ 1 (read p)))) -> 45
   */

  s7_pointer port;
  if (!s7_is_string(car(args)))
    return(s7_wrong_type_arg_error(sc, "call-with-input-string", 1, car(args), "a string"));

  port = s7_open_input_string(sc, s7_string(car(args)));
  push_input_port(sc, port);

  push_stack(sc, OP_EVAL_STRING_DONE, sc->args, sc->code);
  sc->args = cons(sc, port, sc->NIL);
  sc->code = cadr(args);
  eval(sc, OP_APPLY);

  pop_input_port(sc);
  s7_close_input_port(sc, port);
  return(sc->value);
}




/* -------- output -------- */

static void char_to_string_port(char c, s7_pointer pt)
{
  if (port_string_point(pt) >= port_string_length(pt))
    {
      int loc;
      loc = port_string_length(pt);
      port_string_length(pt) *= 2;
      port_string(pt) = (char *)REALLOC(port_string(pt), port_string_length(pt) * sizeof(char));
      memset((void *)(port_string(pt) + loc), 0, loc);
#if 0
      for (i = loc; i < port_string_length(pt); i++)
	port_string(pt)[i] = '\0';
#endif
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

#if S7_DEBUGGING
      if (!is_output_port(pt))
	{
	  fprintf(stderr, "write_char port not an output port");
	  abort();
	}
#endif

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

#if S7_DEBUGGING
      if (!is_output_port(pt))
	{
	  fprintf(stderr, "write_string port not an output port");
	  abort();
	}
#endif

      /* fprintf(stderr, "%s: output: [%s]\n", describe_port(sc, pt), s); */

      if (is_file_port(pt))
	fputs(s, port_file(pt));
      else 
	{
	  for(; *s; s++)
	    char_to_string_port(*s, pt);
	}
    }
}


static char *slashify_string(s7_scheme *sc, const char *p)
{
  int i, j = 0, len;
  char *s;
  len = safe_strlen(p);
  s = (char *)CALLOC(len + 256, sizeof(char));

  s[j++] = '"';
  for (i = 0; i < len; i++) 
    {
      if ((p[i] == 0xff) || (p[i] == '"') || (p[i] < ' ') || (p[i] == '\\'))
	{
	  s[j++] = '\\';
	  switch(p[i]) 
	    {
	    case '"':
	      s[j++] = '"';
	      break;

	    case '\n':
	      s[j++] = 'n';
	      break;

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
  if (obj == sc->NIL) 
    return(copy_string("()"));

  if (obj == sc->T)
    return(copy_string("#t"));

  if (obj == sc->F) 
    return(copy_string("#f"));

  if (obj == sc->EOF_OBJECT)
    return(copy_string("#<eof>"));

  if (obj == sc->UNDEFINED) 
    return(copy_string("#<undefined>"));

  if (obj == sc->UNSPECIFIED) 
    return(copy_string("#<unspecified>"));

  if ((is_input_port(obj)) || (is_output_port(obj)))
    return(describe_port(sc, obj));

  if (s7_is_number(obj))
    return(s7_number_to_string(sc, obj, 10));

  if (s7_is_string(obj)) 
    {
      if (string_length(obj) > 0)
	{
	  if (!use_write) 
	    return(copy_string(string_value(obj)));
	  return(slashify_string(sc, string_value(obj)));
	}
      else 
	{
	  if (!use_write)
	    return(NULL);
	  else return(copy_string("\"\""));
	}
    }
 
  if (s7_is_character(obj)) 
    {
      char *p;
      p = (char *)CALLOC(32, sizeof(char));
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
	      sprintf(p, "#\\space"); 
	      break;
	    case '\n':
	      sprintf(p, "#\\newline"); 
	      break;
	    case '\r':
	      sprintf(p, "#\\return"); 
	      break;
	    case '\t':
	      sprintf(p, "#\\tab"); 
	      break;
	    default:
	      if (c < 32) 
		sprintf(p, "#\\x%x", c);
	      else sprintf(p, "#\\%c", c); 
	      break;
	    }
	}
      return(p);
    }

  if (s7_is_symbol(obj))
    return(copy_string(s7_symbol_name(obj)));

  if (is_macro(obj)) 
    return(copy_string("#<macro>"));

  if (s7_is_closure(obj)) 
    return(copy_string("#<closure>"));

  if (is_promise(obj)) 
    return(copy_string("#<promise>"));

  if (s7_is_function(obj)) 
    {
      char *p;
      p = (char *)CALLOC(32 + safe_strlen(function_name(obj)), sizeof(char));
      sprintf(p, "%s", function_name(obj));
      return(p);
    } 
  
  if (s7_is_continuation(obj)) 
    return(copy_string("#<continuation>"));

  if (is_goto(obj)) 
    return(copy_string("#<goto>"));

  if (is_catch(obj)) 
    return(copy_string("#<catch>"));

  if (s7_is_object(obj)) 
    return(copy_string(s7_describe_object(obj)));

#if S7_DEBUGGING
  search_heap(sc, obj);
#endif
  return(copy_string("#<unknown object!>"));
}


static char *s7_vector_to_c_string(s7_scheme *sc, s7_pointer vect)
{
  int i, len, plen, bufsize = 0;
  bool too_long = false;
  char **elements = NULL;
  char *buf;

  len = vector_length(vect);
  if (len == 0)
    return(copy_string("#()"));

  plen = s7_integer(s7_symbol_value(sc, s7_make_symbol(sc, "*vector-print-length*")));
  if (len > plen)
    {
      too_long = true;
      len = plen;
    }
  elements = (char **)MALLOC(len * sizeof(char *));
  for (i = 0; i < len; i++)
    {

      elements[i] = s7_object_to_c_string(sc, vector_element(vect, i));
      bufsize += safe_strlen(elements[i]);
    }
  bufsize += 128;
  buf = (char *)CALLOC(bufsize, sizeof(char));
  sprintf(buf, "#(");
  for (i = 0; i < len - 1; i++)
    {
      if (elements[i])
	{
	  strcat(buf, elements[i]);
	  FREE(elements[i]);
	  strcat(buf, " ");
	}
    }
  if (elements[len - 1])
    {
      strcat(buf, elements[len - 1]);
      FREE(elements[len - 1]);
    }
  FREE(elements);
  if (too_long)
    strcat(buf, " ...");
  strcat(buf, ")");
  return(buf);
}

static s7_pointer s7_vector_to_string(s7_scheme *sc, s7_pointer vect)
{
  char *buf;
  s7_pointer result;
  if (sc->tracing)
    fprintf(stderr, "vector...");
  buf = s7_vector_to_c_string(sc, vect);
  result = s7_make_string(sc, buf);
  FREE(buf);
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
  if (len < 0)
    {
      /* presumably a dotted list -- handle cars, then final cdr */
      len = (-len + 1);
      dotted = true;
    }

  if (len == 0)
    return(copy_string("()"));

  elements = (char **)MALLOC(len * sizeof(char *));
  for (x = lst, i = 0; s7_is_pair(x); i++, x = s7_cdr(x))
    {
      elements[i] = s7_object_to_c_string(sc, car(x));
      bufsize += safe_strlen(elements[i]);
    }
  if (dotted)
    {
      elements[i] = s7_object_to_c_string(sc, x);
      bufsize += safe_strlen(elements[i]);
    }

  bufsize += (128 + len); /* len spaces */
  buf = (char *)CALLOC(bufsize, sizeof(char));

#if S7_DEBUGGING
  sprintf(buf, "[%d](", pair_line_number(car(lst)));
#else
  sprintf(buf, "(");
#endif

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
      FREE(elements[i]);
  FREE(elements);

  /* fprintf(stderr, "list->c_string (%d %d %d): [%s]\n", len, bufsize, safe_strlen(buf), buf); */

  return(buf);
}

static s7_pointer s7_list_to_string(s7_scheme *sc, s7_pointer lst)
{
  s7_pointer result;
  char *buf;
  buf = s7_list_to_c_string(sc, lst);
  result = s7_make_string(sc, buf);
  FREE(buf);
  return(result);
}

static char *s7_object_to_c_string_1(s7_scheme *sc, s7_pointer obj, bool use_write)
{
  if (s7_is_vector(obj))
    return(s7_vector_to_c_string(sc, obj));
  if (s7_is_pair(obj))
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
  if (s7_is_vector(obj))
    return(s7_vector_to_string(sc, obj));
  if (s7_is_pair(obj))
    return(s7_list_to_string(sc, obj));
  x = s7_make_string(sc, str = s7_atom_to_c_string(sc, obj, true));
  if (str) FREE(str);
  return(x);
}


static void print_atom(s7_scheme *sc, s7_pointer obj)
{
  /* this is used in the eval loop, always assuming current-output-port (eval|error output, etc) */
  char *p;
  p = s7_atom_to_c_string(sc, obj, true);
  if (p)
    {
      write_string(sc, p, sc->output_port);
      FREE(p);
    }
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

  if (s7_is_pair(args))
    {
      port = car(args);
      if (!is_output_port(port))
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

  if (s7_is_pair(cdr(args)))
    {
      port = cadr(args);
      if (!is_output_port(port))
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
  
  /* fprintf(stderr, "%s write: [%s]\n", describe_port(sc, port), val); */

  write_string(sc, val, port);
  if (val) FREE(val);
}

void s7_write(s7_scheme *sc, s7_pointer obj, s7_pointer port)
{
  write_or_display(sc, obj, port, true);
}

static s7_pointer g_write(s7_scheme *sc, s7_pointer args)
{
  #define H_write "(write str :optional port) writes str (a string) to the output port"
  s7_pointer port;

  if (s7_is_pair(cdr(args)))
    {
      port = cadr(args);
      if (!is_output_port(port))
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

  if (s7_is_pair(cdr(args)))
    {
      port = cadr(args);
      if (!is_output_port(port))
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

  if (s7_is_pair(args))
    {
      port = car(args);
      if ((!is_input_port(port)) ||
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

  if (s7_is_pair(cdr(args)))
    {
      port = cadr(args);
      if ((!is_output_port(port)) ||
	  (!is_file_port(port)))
	return(s7_wrong_type_arg_error(sc, "write-byte", 2, port, "an output file port"));
    }
  else port = sc->output_port;

  fputc((unsigned char)s7_integer(car(args)), port_file(port));
  return(car(args));
}



/* -------------------------------- lists -------------------------------- */

/* static off_t conses = 0; */

static s7_pointer cons_1(s7_scheme *sc, s7_pointer a, s7_pointer b, bool immutable) 
{
  s7_pointer x;

#if S7_DEBUGGING
  if (!is_object(a))
    {
      fprintf(stderr, "cons car is bad");
      abort();
    }
  if (!is_object(b))
    {
      fprintf(stderr, "cons cdr is bad");
      abort();
    }
#endif

  local_protect(a);
  local_protect(b);
  x = new_cell(sc);

  if (immutable) 
    set_type(x, T_PAIR | T_IMMUTABLE);
  else set_type(x, T_PAIR);   

  car(x) = a;
  cdr(x) = b;

  local_unprotect(a);
  local_unprotect(b);
  return(x);
}


s7_pointer s7_immutable_cons(s7_scheme *sc, s7_pointer a, s7_pointer b)
{
  return(cons_1(sc, a, b, true));
}

s7_pointer s7_cons(s7_scheme *sc, s7_pointer a, s7_pointer b) 
{
  return(cons_1(sc, a, b, false));
}

bool s7_is_pair(s7_pointer p)     
{ 
  return(type(p) == T_PAIR);
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
#if S7_DEBUGGING
  if (is_immutable(car(p)))
    {
      fprintf(stderr, "set-car! wants to clobber immutable element");
      abort();
    }
#endif

  car(p) = q;
  return(q);
}

s7_pointer s7_set_cdr(s7_pointer p, s7_pointer q) 
{ 
#if S7_DEBUGGING
  if (is_immutable(car(p)))
    {
      fprintf(stderr, "set-cdr! wants to clobber immutable element");
      abort();
    }
#endif

  cdr(p) = q;
  return(q);
}

s7_pointer s7_list_ref(s7_scheme *sc, s7_pointer lst, int num)
{
  int i;
  s7_pointer x;

  if (num == 0)
    return(s7_car(lst));
  if (num < 0)
    return(sc->NIL);
  for (x = lst, i = 0; (i < num) && (s7_is_pair(x)); i++, x = cdr(x)) {}
  if ((i == num) &&
      (s7_is_pair(x)))
    return(car(x));
  return(sc->NIL);
}

s7_pointer s7_list_set(s7_scheme *sc, s7_pointer lst, int num, s7_pointer val)
{
  int i;
  s7_pointer x;
  for (x = lst, i = 0; (i < num) && (s7_is_pair(x)); i++, x = cdr(x)) {}
  if ((i == num) &&
      (s7_is_pair(x)))
    car(x) = val;
  return(val);
}

s7_pointer s7_member(s7_scheme *sc, s7_pointer sym, s7_pointer lst)
{
  s7_pointer x;
  int v;
  for (x = lst, v = 0; s7_is_pair(x); x = s7_cdr(x))
    if (s7_is_equal(sym, car(x)))
      return(x);
  return(sc->F);
}

s7_pointer s7_assoc(s7_scheme *sc, s7_pointer sym, s7_pointer lst)
{
  s7_pointer x;
  int v;
  for (x = lst, v = 0; s7_is_pair(x); x = s7_cdr(x))
    if ((s7_is_pair(s7_car(x))) &&
	(s7_is_equal(sym, car(car(x)))))
      return(car(x));
  return(sc->F);
}


/* reverse list -- produce new list */
s7_pointer s7_reverse(s7_scheme *sc, s7_pointer a) 
{
  s7_pointer p = sc->NIL;
  local_protect(a);
  for ( ; s7_is_pair(a); a = cdr(a)) 
    p = cons(sc, car(a), p);
  local_unprotect(a);
  return(p);
}

/* reverse list --- in-place */
s7_pointer s7_reverse_in_place(s7_scheme *sc, s7_pointer term, s7_pointer list) 
{
  s7_pointer p = list, result = term, q;
  while (p != sc->NIL) 
    {
      q = cdr(p);
      cdr(p) = result;
      result = p;
      p = q;
    }
  return(result);
}

/* append list -- produce new list */
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

/* TODO: combine or fixup somehow the various list_lengths */

int s7_list_length(s7_scheme *sc, s7_pointer a) 
{
  int v = 0;
  s7_pointer x;
  for (x = a, v = 0; s7_is_pair(x); x = cdr(x)) 
    ++v;
  if (x == sc->NIL) 
    return(v);
  return(-v); /* a dotted list? */
}



static s7_pointer g_is_null(s7_scheme *sc, s7_pointer args)
{
  #define H_is_null "(null? obj) returns #t if obj is the empty list"
  return(to_s7_bool(sc, car(args) == sc->NIL));
}

static s7_pointer g_is_pair(s7_scheme *sc, s7_pointer args)
{
  #define H_is_pair "(pair? obj) returns #t if obj is a pair (a non-empty list)"
  return(to_s7_bool(sc, s7_is_pair(car(args))));
}

bool s7_is_list(s7_scheme *sc, s7_pointer p)
{
  return((p == sc->NIL) ||
	 (s7_is_pair(p)));
}
      
static s7_pointer g_is_list(s7_scheme *sc, s7_pointer args)
{
  s7_pointer slow, fast;
  #define H_is_list "(list? obj) returns #t if obj is a list"

  slow = fast = car(args);
  while (true)
    {
      if (!s7_is_pair(fast)) 
	return(to_s7_bool(sc, (fast == sc->NIL)));

      fast = cdr(fast);
      if (!s7_is_pair(fast)) 
	return(to_s7_bool(sc, (fast == sc->NIL)));

      fast = cdr(fast);
      slow = cdr(slow);
      if (fast == slow) 
	{
	  /* the fast pointer has looped back around and caught up
	     with the slow pointer, hence the structure is circular,
	     not of finite length, and therefore not a list */
	  return(sc->F);
	}
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

  for (i = 0, p = car(args); (i < index) && (p != sc->NIL); i++, p = cdr(p)) {}
  if (p == sc->NIL)
    return(s7_out_of_range_error(sc, "list-ref", 2, cadr(args), "less than list length"));
  if (!s7_is_pair(p))
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

  for (i = 0, p = car(args); (i < index) && (p != sc->NIL); i++, p = cdr(p)) {}
  if (p == sc->NIL)
    return(s7_out_of_range_error(sc, "list-set!", 2, cadr(args), "less than list length"));

  car(p) = caddr(args);
  return(caddr(args));
}

static s7_pointer g_list_tail(s7_scheme *sc, s7_pointer args)
{
  #define H_list_tail "(list-tail lst i) returns the list from the i-th element on"

  int i, index;
  s7_pointer p;
  if (!is_pair(car(args)))
    return(s7_wrong_type_arg_error(sc, "list-tail", 1, car(args), "a list"));
  if (!s7_is_integer(cadr(args)))
    return(s7_wrong_type_arg_error(sc, "list-tail", 2, cadr(args), "an integer"));

  index = s7_integer(cadr(args));
  if (index < 0)
    return(s7_out_of_range_error(sc, "list-tail", 2, cadr(args), "non-negative"));

  for (i = 0, p = car(args); (i < index) && (p != sc->NIL); i++, p = cdr(p)) {}
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

static s7_pointer g_reverse(s7_scheme *sc, s7_pointer args)
{
  #define H_reverse "(reverse lst) returns a list with the elements of lst in reverse order"
  s7_pointer p;

  p = car(args);
  if (p == sc->NIL)
    return(sc->NIL);

  if (!s7_is_pair(p))
    return(s7_wrong_type_arg_error(sc, "reverse", 1, p, "a list"));

  return(s7_reverse(sc, p));
}

static s7_pointer g_reverse_in_place(s7_scheme *sc, s7_pointer args)
{
  #define H_reverse_in_place "(reverse! lst) reverses lst in place"
  s7_pointer p;

  p = car(args);
  if (p == sc->NIL)
    return(sc->NIL);

  if (!s7_is_pair(p))
    return(s7_wrong_type_arg_error(sc, "reverse!", 1, p, "a list"));

  return(s7_reverse_in_place(sc, sc->NIL, p));
}

/* remv -- produce new list */
s7_pointer s7_remv(s7_scheme *sc, s7_pointer a, s7_pointer obj) 
{
  s7_pointer p = sc->NIL;
  for ( ; s7_is_pair(a); a = cdr(a))
    if (car(a) != obj)
      p = cons(sc, car(a), p);
  return(s7_reverse(sc, p));
}


static s7_pointer g_length(s7_scheme *sc, s7_pointer args)
{
  #define H_length "(length lst) returns the length of the list lst"

  int len;

  if (car(args) == sc->NIL)
    return(s7_make_integer(sc, 0));

  if (!s7_is_pair(car(args))) 
    return(s7_wrong_type_arg_error(sc, "length", 1, car(args), "a list"));

  len = s7_list_length(sc, car(args));

  if (len < 0) 
    return(s7_wrong_type_arg_error(sc, "length:", 1, car(args), "a proper (not a dotted) list"));
  return(s7_make_integer(sc, len));
}
      

static s7_pointer g_assq_1(s7_scheme *sc, s7_pointer args, const char *name, bool (*eq_func)(s7_pointer a, s7_pointer b))
{
  #define H_assq "(assq obj alist) returns the key-value pair associated (via eq?) with the key obj in the association list alist"
  #define H_assv "(assv obj alist) returns the key-value pair associated (via eqv?) with the key obj in the association list alist"
  #define H_assoc "(assoc obj alist) returns the key-value pair associated (via equal?) with the key obj in the association list alist"

  if ((!s7_is_pair(cadr(args))) && (cadr(args) != sc->NIL))
    return(s7_wrong_type_arg_error(sc, name, 2, cadr(args), "a list"));

  sc->x = car(args);
  for (sc->y = cadr(args); s7_is_pair(sc->y); sc->y = cdr(sc->y)) 
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

  if ((!s7_is_pair(cadr(args))) && (cadr(args) != sc->NIL))
    return(s7_wrong_type_arg_error(sc, name, 2, cadr(args), "a list"));

  for (sc->x = cadr(args); s7_is_pair(sc->x); sc->x = cdr(sc->x)) 
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
  return(g_member(sc, 
		  cons(sc, 
		       car(args),
		       cons(sc, s7_name_to_value(sc, "*features*"), sc->NIL))));
}

static s7_pointer g_provide(s7_scheme *sc, s7_pointer args)
{
  #define H_provide "(provide sym) adds sym to the *features* list"
  if (!s7_is_symbol(car(args)))
    return(s7_wrong_type_arg_error(sc, "provide", 1, car(args), "a symbol"));
  s7_symbol_set_value(sc, 
		      s7_make_symbol(sc, "*features*"),
		      cons(sc, 
			   car(args), 
			   s7_name_to_value(sc, "*features*")));
  return(sc->UNSPECIFIED);
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
      
  sc->x = car(args);
  for (i = 1, sc->y = cdr(args); sc->y != sc->NIL; i++, sc->y = cdr(sc->y)) 
    {
      if (!s7_is_list(sc, sc->x))
	return(s7_wrong_type_arg_error(sc, "append", i, sc->x, "a list"));
      sc->x = s7_append(sc, sc->x, car(sc->y));
    }
  return(sc->x);
}

/* -------- list-line-number -------- */

static s7_pointer g_list_line_number(s7_scheme *sc, s7_pointer args)
{
  #define H_list_line_number "(list-line-number lst) returns the line number it thinks lst occurred at"
  if (s7_is_pair(car(args)))
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
  set_type(x, T_VECTOR | T_FINALIZABLE);
  vector_length(x) = len;
  if (len > 0)
    {
      x->object.vector.elements = (s7_pointer *)CALLOC(len, sizeof(s7_pointer));
      s7_vector_fill(x, sc->NIL);
    }
  return(x);
}


int s7_vector_length(s7_pointer vec)
{
  return(vector_length(vec));
}

void s7_vector_fill(s7_pointer vec, s7_pointer obj) 
{
  int i, len;
  len = vector_length(vec);
  for(i = 0; i < len; i++) 
    vector_element(vec, i) = obj;
}

static s7_pointer g_vector_fill(s7_scheme *sc, s7_pointer args)
{
  #define H_vector_fill "(vector-fill! v val) sets all elements of the vector v to val"
  if (!s7_is_vector(car(args)))
    return(s7_wrong_type_arg_error(sc, "vector-fill!", 1, car(args), "a vector"));
  s7_vector_fill(car(args), cadr(args));
  return(sc->UNSPECIFIED);
}

s7_pointer s7_vector_ref(s7_pointer vec, int elem) 
{
  if (elem >= vector_length(vec))
    fprintf(stderr, "vector-ref past end of vector: %d %d\n", elem, vector_length(vec));
  return(vector_element(vec, elem));
}

s7_pointer s7_vector_set(s7_pointer vec, int elem, s7_pointer a) 
{
#if S7_DEBUGGING
  if (!is_object(a))
    {
      fprintf(stderr, "vector-set! value is not an object!");
      abort();
    }
#endif

  if (elem >= vector_length(vec))
    fprintf(stderr, "vector-set past end of vector: %d %d\n", elem, vector_length(vec));

  vector_element(vec, elem) = a;
  return(a);
}

s7_pointer s7_vector_to_list(s7_scheme *sc, s7_pointer vect)
{
  s7_pointer lst = sc->NIL;
  int i, len;
  len = vector_length(vect);
  local_protect(lst);
  for (i = len - 1; i >= 0; i--)
    lst = cons(sc, vector_element(vect, i), lst);
  local_unprotect(lst);
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
      local_protect(vec);
      for (sc->x = args, i = 0; s7_is_pair(sc->x); sc->x = cdr(sc->x), i++) 
	vector_element(vec, i) =  car(sc->x);
      local_unprotect(vec);
    }
  return(vec);
}

static s7_pointer g_list_to_vector(s7_scheme *sc, s7_pointer args)
{
  #define H_list_to_vector "(list->vector lst) returns a vector containing the elements of lst"

  if (car(args) == sc->NIL)
    return(s7_make_vector(sc, 0));

  if (!s7_is_pair(car(args)))
    return(s7_wrong_type_arg_error(sc, "list->vector", 1, car(args), "a list"));
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
  return(to_s7_bool(sc, s7_is_vector(car(args))));
}
      

/* -------------------------------- objects and functions -------------------------------- */

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
  ptr = (ffunc *)CALLOC(1, sizeof(ffunc));
  set_type(x, T_S7_FUNCTION | T_ATOM | T_CONSTANT);
  x->object.ffptr = ptr;
  x->object.ffptr->ff = f;
  x->object.ffptr->name = name;
  if (doc)
    x->object.ffptr->doc = copy_string(doc);
  x->object.ffptr->required_args = required_args;
  x->object.ffptr->optional_args = optional_args;
  x->object.ffptr->rest_arg = rest_arg;
  return(x);
}

static s7_pointer g_is_procedure(s7_scheme *sc, s7_pointer args)
{
  #define H_is_procedure "(procedure? obj) returns #t if obj is a procedure"
  return(to_s7_bool(sc, ((s7_is_closure(car(args))) || 
			 (is_goto(car(args))) || 
			 (s7_is_continuation(car(args))) || 
			 (s7_is_function(car(args))))));
}

static s7_pointer procedure_source(s7_pointer p)   
{ 
  return(car(p));
}

/* TODO: how to expand: (let ((i 0)) (lambda () i)) */

s7_pointer s7_procedure_source(s7_scheme *sc, s7_pointer p)
{
  /* make it look like a lambda form */
  if (s7_is_closure(p) || is_macro(p) || is_promise(p)) 
    return(s7_append(sc, 
		     cons(sc, 
			  sc->LAMBDA, 
			  cons(sc,
			       car(car(p)),
			       sc->NIL)),
		     cdr(car(p))));
  return(sc->F);
}

static s7_pointer g_procedure_source(s7_scheme *sc, s7_pointer args)
{
  #define H_procedure_source "(procedure-source func) tries to return the definition of func"

  if (s7_is_symbol(car(args)))
    sc->y = s7_symbol_value(sc, car(args));
  else sc->y = car(args);
  if (sc->y != sc->NIL)
    return(s7_procedure_source(sc, sc->y));
  return(sc->NIL);
}

s7_pointer s7_procedure_environment(s7_pointer p)    
{ 
  return(cdr(p));
}

bool s7_is_closure(s7_pointer p)  
{ 
  return(type(p) == T_CLOSURE);
}

static s7_pointer g_is_closure(s7_scheme *sc, s7_pointer args)
{
  #define H_is_closure "(closure? obj) returns #t if obj is a closure"
  return(to_s7_bool(sc, s7_is_closure(car(args))));
}


static s7_pointer g_make_closure(s7_scheme *sc, s7_pointer args)
{
  #define H_make_closure "(make_closure obj :optional env) returns a closure of obj within env"

  if (!s7_is_pair(car(args)))
    return(s7_wrong_type_arg_error(sc, "make-closure", 1, car(args), "a lambda form"));

  sc->x = car(args);
  if (car(sc->x) == sc->LAMBDA) 
    sc->x = cdr(sc->x);
      
  if (cdr(args) == sc->NIL)
    sc->y = sc->envir;
  else 
    {
      if (!s7_is_pair(cadr(args)))
	return(s7_wrong_type_arg_error(sc, "make-closure", 2, cadr(args), "an environment"));
      sc->y = cadr(args);
    }
      
  return(s7_make_closure(sc, sc->x, sc->y));
}
      
void s7_define_function(s7_scheme *sc, const char *name, s7_function fnc, int required_args, int optional_args, bool rest_arg, const char *doc)
{
  s7_pointer func, sym;
  
  func = s7_make_function(sc, name, fnc, required_args, optional_args, rest_arg, doc);
  local_protect(func);
  sym = s7_make_symbol(sc, name);
  local_protect(sym);
  s7_define(sc, s7_global_environment(sc), sym, func);
  local_unprotect(func);
  local_unprotect(sym);
}


const char *s7_procedure_documentation(s7_pointer x)
{
  return(function_documentation(x));
}

static s7_pointer g_procedure_documentation(s7_scheme *sc, s7_pointer args)
{
  #define H_procedure_documentation "(procedure-documentation func) returns func's documentation string"
  s7_pointer x;

  x = car(args);
  if (s7_is_symbol(x))
    x = s7_symbol_value(sc, x);

  if (s7_is_function(x))
    return(s7_make_string(sc, s7_procedure_documentation(x)));

  if ((s7_is_closure(x)) &&
      (s7_is_string(cadar(x))))
    return(cadar(x));

  return(sc->F);
}


s7_pointer s7_procedure_arity(s7_scheme *sc, s7_pointer args)
{
  #define H_procedure_arity "(procedure-arity func) returns a list '(required optional rest)"
  s7_pointer lst = sc->NIL;

  s7_pointer x;
  x = car(args);
  /* fprintf(stderr, "arity: %s %d %d %d\n", s7_object_to_c_string(sc, x), s7_is_symbol(x), s7_is_function(x), s7_is_closure(x)); */

  if (s7_is_symbol(x))
    x = s7_symbol_value(sc, x);

  if (s7_is_function(x))
    {
      lst = s7_cons(sc, (x->object.ffptr->rest_arg) ? sc->T : sc->F, lst);
      local_protect(lst);
      lst = s7_cons(sc, s7_make_integer(sc, x->object.ffptr->optional_args), lst);
      lst = s7_cons(sc, s7_make_integer(sc, x->object.ffptr->required_args), lst);
      local_unprotect(lst);
    }
  else
    {
      if ((s7_is_closure(x)) ||
	  (s7_is_closure(car(args))) ||
	  (s7_is_pair(x)))
	{
	  int len;

	  if (s7_is_pair(x))
	    len = s7_list_length(sc, car(x));
	  else len = s7_list_length(sc, caar(x));

	  /* fprintf(stderr, "len %d %s\n", len, s7_object_to_c_string(sc, caar(x))); */

	  if (len >= 0)
	    lst = s7_cons(sc, s7_make_integer(sc, len),
			  s7_cons(sc, s7_make_integer(sc, 0),
				  s7_cons(sc, sc->F, sc->NIL)));
	  else
	    lst = s7_cons(sc, s7_make_integer(sc, abs(len)),
			  s7_cons(sc, s7_make_integer(sc, 0),
				  s7_cons(sc, sc->T, sc->NIL)));
	}
    }
  return(lst);
}


typedef struct {
  int type;
  const char *name;
  char *(*print)(void *value);
  void (*free)(void *value);
  bool (*equal)(void *val1, void *val2);
  void (*gc_mark)(void *val);
  s7_pointer (*apply)(s7_scheme *sc, s7_pointer obj, s7_pointer args);
  void (*set)(s7_scheme *sc, s7_pointer obj, s7_pointer args);
} fobject;

static fobject *object_types = NULL;
static int object_types_size = 0;
static int num_types = 0;

int s7_new_type(const char *name, 
		char *(*print)(void *value), 
		void (*free)(void *value), 
		bool (*equal)(void *val1, void *val2),
		void (*gc_mark)(void *val),
                s7_pointer (*apply)(s7_scheme *sc, s7_pointer obj, s7_pointer args),
                void (*set)(s7_scheme *sc, s7_pointer obj, s7_pointer args))
{
  int tag;
  tag = num_types++;
  if (tag >= object_types_size)
    {
      if (object_types_size == 0)
	{
	  object_types_size = 8;
	  object_types = (fobject *)CALLOC(object_types_size, sizeof(fobject));
	}
      else
	{
	  object_types_size = tag + 8;
	  object_types = (fobject *)REALLOC((void *)object_types, object_types_size * sizeof(fobject));
	}
    }
  object_types[tag].type = tag;
  object_types[tag].name = copy_string(name);
  object_types[tag].free = free;
  object_types[tag].print = print;
  object_types[tag].equal = equal;
  object_types[tag].gc_mark = gc_mark;
  object_types[tag].apply = apply;
  object_types[tag].set = set;
  return(tag);
}

char *s7_describe_object(s7_pointer a)
{
  int tag;
  tag = a->object.fobj.type;
  if (object_types[tag].print)
    return((*(object_types[tag].print))(a->object.fobj.value));
  return(copy_string(object_types[tag].name));
}

void s7_free_object(s7_pointer a)
{
  int tag;
  tag = a->object.fobj.type;
  if (object_types[tag].free)
    (*(object_types[tag].free))(a->object.fobj.value);
}

bool s7_equalp_objects(s7_pointer a, s7_pointer b)
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
  else fprintf(stderr, "%p, found bad type: %x %d %x\n", a, tag, tag, a->flag);
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

static void s7_set_object(s7_scheme *sc, s7_pointer obj, s7_pointer args)
{
  int tag;
  tag = obj->object.fobj.type;
  if (object_types[tag].set)
    (*(object_types[tag].set))(sc, obj, args);
}

/* generalized set! calls g_set_object which then calls the object's set function */

static s7_pointer g_set_object(s7_scheme *sc, s7_pointer args)
{
  s7_set_object(sc, car(args), cdr(args));
  return(car(args));
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
  set_type(x, T_S7_OBJECT | T_ATOM | T_FINALIZABLE);
  x->object.fobj.type = type;
  x->object.fobj.value = value;
  return(x);
}



/* -------- procedure-with-setter -------- */

static int pws_tag;

typedef struct {
  s7_pointer (*getter)(s7_scheme *sc, s7_pointer args);
  s7_pointer (*setter)(s7_scheme *sc, s7_pointer args);
  s7_pointer scheme_getter;
  s7_pointer scheme_setter;
  char *documentation;
} pws;
  
s7_pointer s7_make_procedure_with_setter(s7_scheme *sc, 
					 s7_pointer (*getter)(s7_scheme *sc, s7_pointer args), 
					 s7_pointer (*setter)(s7_scheme *sc, s7_pointer args),
					 const char *documentation)
{
  pws *f;
  f = (pws *)CALLOC(1, sizeof(pws));
  f->getter = getter;
  f->setter = setter;
  if (documentation)
    f->documentation = copy_string(documentation);
  else f->documentation = NULL;
  f->scheme_getter = sc->NIL;
  f->scheme_setter = sc->NIL;
  return(s7_make_object(sc, pws_tag, (void *)f));
}

static char *pws_print(void *obj)
{
  return((char *)"#<procedure-with-setter>");
}

static void pws_free(void *obj)
{
  pws *f = (pws *)obj;
  if (f)
    {
#if S7_DEBUGGING
      fprintf(stderr, "freeing pws?");
#endif
      if (f->documentation)
	FREE(f->documentation);
      FREE(f);
    }
}

static void pws_mark(void *val)
{
  pws *f = (pws *)val;
  s7_mark_object(f->scheme_getter);
  s7_mark_object(f->scheme_setter);
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
static void pws_set(s7_scheme *sc, s7_pointer obj, s7_pointer args)
{
  pws *f;
  f = (pws *)s7_object_value(obj);
  if (f->setter != NULL)
    (*(f->setter))(sc, args);
  else s7_call(sc, f->scheme_setter, args);
}

static s7_pointer g_make_procedure_with_setter(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p;
  pws *f;
  p = s7_make_procedure_with_setter(sc, NULL, NULL, NULL);
  f = (pws *)s7_object_value(p);
  f->scheme_getter = car(args);
  f->scheme_setter = cadr(args);
  return(p);
}

bool s7_is_procedure_with_setter(s7_pointer obj)
{
  return((s7_is_object(obj)) &&
	 (s7_object_type(obj) == pws_tag));
}

static s7_pointer g_is_procedure_with_setter(s7_scheme *sc, s7_pointer args)
{
  #define H_is_procedure_with_setter "(procedure-with-setter? obj) returns #t if obj is a procedure-with-setter"
  return(to_s7_bool(sc, s7_is_procedure_with_setter(car(args))));
}




/* -------------------------------- eq etc -------------------------------- */

bool s7_is_eq(s7_pointer obj1, s7_pointer obj2)
{
  return(obj1 == obj2);
}

/* equivalence of atoms */
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

/* To do: promise should be forced ONCE only */

bool s7_is_equal(s7_pointer x, s7_pointer y)
{
  if (x == y) 
    return(true);
  
  if (type(x) != type(y)) 
    return(false);
  
  if (s7_is_pair(x))
    return((s7_is_equal(car(x), car(y))) &&
	   (s7_is_equal(cdr(x), cdr(y))));
  
  if (s7_is_string(x))
    return((string_length(x) == string_length(y)) &&
	   ((string_length(x) == 0) ||
	    (strcmp(string_value(x), string_value(y)) == 0)));
  
  if (s7_is_object(x))
    return(s7_equalp_objects(x, y));
  
  if (s7_is_vector(x))
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
  return(to_s7_bool(sc, car(args) == cadr(args)));
}
      
static s7_pointer g_is_eqv(s7_scheme *sc, s7_pointer args)
{
  #define H_is_eqv "(eqv? obj1 obj2) returns #t if obj1 is equivalent to obj2"
  return(to_s7_bool(sc, s7_is_eqv(car(args), cadr(args))));
}
      
static s7_pointer g_is_equal(s7_scheme *sc, s7_pointer args)
{
  #define H_is_equal "(equal? obj1 obj2) returns #t if obj1 is equal to obj2"
  return(to_s7_bool(sc, s7_is_equal(car(args), cadr(args))));
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
  return(to_s7_bool(sc, s7_is_keyword(car(args))));
}

s7_pointer s7_make_keyword(s7_scheme *sc, const char *key)
{
  s7_pointer sym;
  char *name;
  name = (char *)CALLOC(safe_strlen(key) + 2, sizeof(char));
  sprintf(name, ":%s", key);                     /* prepend ":" */
  sym = s7_make_symbol(sc, name);
  local_protect(sym);
  typeflag(sym) |= (T_IMMUTABLE | T_CONSTANT); 
  FREE(name);
  s7_new_slot_spec_in_env(sc, s7_global_environment(sc), sym, sym); 
  local_unprotect(sym);
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
  char *name;
  if (!s7_is_keyword(car(args)))
    return(s7_wrong_type_arg_error(sc, "keyword->symbol", 1, car(args), "a keyword"));
  name = s7_symbol_name(car(args));
  return(s7_make_symbol(sc, ++name));
}

static s7_pointer g_symbol_to_keyword(s7_scheme *sc, s7_pointer args)
{
  #define H_symbol_to_keyword "(symbol->keyword sym) returns a keyword with the same name as sym, but with a colon prepended"
  if (!s7_is_symbol(car(args)))
    return(s7_wrong_type_arg_error(sc, "symbol->keyword", 1, car(args), "a symbol"));
  return(s7_make_keyword(sc, s7_symbol_name(car(args))));
}



/* -------- hash tables -------- */

static s7_pointer g_make_hash_table(s7_scheme *sc, s7_pointer args)
{
  #define H_make_hash_table "(make-hash-table :optional size) returns a new hash table"
  int size = 461;
  if (car(args) != sc->NIL)
    {
      if (s7_is_integer(car(args)))
	size = s7_integer(car(args));
      else return(s7_wrong_type_arg_error(sc, "make-hash-table", 1, car(args), "an integer"));
    }
  return(s7_make_vector(sc, size));
}

s7_pointer s7_hash_table_ref(s7_scheme *sc, s7_pointer table, const char *name)
{
  int location;
  s7_pointer x;
  
  location = hash_fn(name, vector_length(table));
  for (x = vector_element(table, location); x != sc->NIL; x = cdr(x)) 
    if (strcasecmp(name, string_value(caar(x))) == 0) 
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
    if (strcasecmp(name, string_value(caar(x))) == 0)
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

static s7_pointer g_hash_table_ref(s7_scheme *sc, s7_pointer args)
{
  /* basically the same layout as the global symbol table */
  #define H_hash_table_ref "(hash-table-ref table key) returns the value associated with key (a string or symbol) in the hash table"
  const char *name;
  s7_pointer table, key;
  table = car(args);
  key = cadr(args);

  if (!s7_is_vector(table))
    return(s7_wrong_type_arg_error(sc, "hash-table-ref", 1, table, "a hash-table"));
  if (s7_is_string(key))
    name = string_value(key);
  else 
    {
      if (s7_is_symbol(key))
	name = s7_symbol_name(key);
      else return(s7_wrong_type_arg_error(sc, "hash-table-ref", 2, key, "a string or symbol"));
    }
  
  return(s7_hash_table_ref(sc, table, name));
}
 
static s7_pointer g_hash_table_set(s7_scheme *sc, s7_pointer args)
{
  #define H_hash_table_set "(hash-table-set! table key value) sets the value associated with key (a string or symbol) in the hash table to value"
  const char *name;
  s7_pointer table, key;
  table = car(args);
  key = cadr(args);
  
  if (!s7_is_vector(table))
    return(s7_wrong_type_arg_error(sc, "hash-table-set!", 1, table, "a hash-table"));
  if (s7_is_string(key))
    name = string_value(key);
  else 
    {
      if (s7_is_symbol(key))
	name = s7_symbol_name(key);
      else return(s7_wrong_type_arg_error(sc, "hash-table-set!", 2, key, "a string or symbol"));
    }
  
  return(s7_hash_table_set(sc, table, name, caddr(args)));
}
 



/* -------------------------------- errors -------------------------------- */

s7_pointer s7_wrong_type_arg_error(s7_scheme *sc, const char *caller, int arg_n, s7_pointer arg, const char *descr)
{
  int len;
  char *errmsg, *argstr;
  argstr = s7_object_to_c_string(sc, arg);
  len = safe_strlen(argstr) + safe_strlen(descr) + safe_strlen(caller) + 128;
  errmsg = (char *)CALLOC(len, sizeof(char));
  if (arg_n <= 0) arg_n = 1;
  snprintf(errmsg, len, "%s: argument %d (%s) has wrong type (expecting %s)", caller, arg_n, argstr, descr);
  sc->x = s7_make_string(sc, errmsg);
  FREE(errmsg);
  if (argstr) FREE(argstr);
  return(s7_error(sc, s7_make_symbol(sc, "wrong-type-arg"), sc->x));
}

s7_pointer s7_out_of_range_error(s7_scheme *sc, const char *caller, int arg_n, s7_pointer arg, const char *descr)
{
  int len;
  char *errmsg, *argstr;
  argstr = s7_object_to_c_string(sc, arg);
  len = safe_strlen(argstr) + safe_strlen(descr) + safe_strlen(caller) + 128;
  errmsg = (char *)CALLOC(len, sizeof(char));
  if (arg_n <= 0) arg_n = 1;
  snprintf(errmsg, len, "%s: argument %d (%s) is out of range (expecting %s)", caller, arg_n, argstr, descr);
  sc->x = s7_make_string(sc, errmsg);
  FREE(errmsg);
  if (argstr) FREE(argstr);
  return(s7_error(sc, s7_make_symbol(sc, "out-of-range"), sc->x));
}

static s7_pointer s7_division_by_zero_error(s7_scheme *sc, const char *caller, s7_pointer arg)
{
  int len;
  char *errmsg;
  len = safe_strlen(caller) + 128;
  errmsg = (char *)CALLOC(len, sizeof(char));
  snprintf(errmsg, len, "%s: division by zero", caller);
  sc->x = s7_make_string(sc, errmsg);
  FREE(errmsg);
  return(s7_error(sc, s7_make_symbol(sc, "division-by-zero"), sc->x));
}

static s7_pointer s7_file_error(s7_scheme *sc, const char *caller, const char *descr, const char *name)
{
  int len;
  char *errmsg;
  len = safe_strlen(descr) + safe_strlen(name) + safe_strlen(caller) + 128;
  errmsg = (char *)CALLOC(len, sizeof(char));
  snprintf(errmsg, len, "%s: %s %s", caller, descr, name);
  sc->x = s7_make_string(sc, errmsg);
  FREE(errmsg);
  return(s7_error(sc, s7_make_symbol(sc, "io-error"), sc->x));
}

void s7_set_error_exiter(s7_scheme *sc, void (*error_exiter)(void))
{
  sc->error_exiter = error_exiter;
}

static s7_pointer g_catch(s7_scheme *sc, s7_pointer args)
{
  #define H_catch "(catch tag thunk handler) evaluates thunk; if an error occurs that matches tag (#t matches all), the handler is called"
  s7_pointer p;
  rcatch *c;

  c = (rcatch *)CALLOC(1, sizeof(rcatch));
  c->tag = car(args);
  c->goto_loc = sc->stack_top;
  /* fprintf(stderr, "catch %p loc: %d\n", c, c->goto_loc); */
  c->handler = caddr(args);

  p = new_cell(sc);
  set_type(p, T_CATCH | T_FINALIZABLE);
  local_protect(p);
  p->object.catcher = c;

  push_stack(sc, OP_CATCH, sc->NIL, p);
  sc->args = sc->NIL;
  sc->code = cadr(args);

  local_unprotect(p);
  push_stack(sc, OP_APPLY, sc->args, sc->code);

  return(sc->F);
}


static s7_pointer s7_error_1(s7_scheme *sc, s7_pointer type, s7_pointer info, bool exit_eval)
{
  /* (catch #t (lambda () (display "hi") (error 'ho "a error") (display "oops")) (lambda args 'error)) */
  /* (catch #t (lambda () (display "a") (catch 'ha (lambda () (display "hi") (error 'ho "a error") (display "oops")) (lambda args 'error)) (display "yow")) (lambda args 'a-error)) */

  int i;
  s7_pointer catcher;
  catcher = sc->F;

  /* top is 1 past actual top, top - 1 is op, if op = OP_CATCH, top - 4 is the cell containing the catch struct */

  for (i = sc->stack_top - 1; i >= 3; i -= 4)
    {
      if (s7_integer(vector_element(sc->stack, i)) == OP_CATCH)
	{
	  s7_pointer x;
	  x = vector_element(sc->stack, i - 3);
	  if ((type == sc->T) ||
	      (catch_tag(x) == sc->T) ||
	      (s7_is_eq(catch_tag(x), type)))
	    {
	      catcher = x;
	      break;
	    }
	}
    }

  if (catcher != sc->F)
    {
      /* fprintf(stderr, "goto: %p %d\n", catcher->object.catcher, catch_goto_loc(catcher)); */
      sc->args = cons(sc, type, sc->x = cons(sc, info, sc->NIL));
      sc->code = catch_handler(catcher);
      sc->stack_top = catch_goto_loc(catcher);

#if S7_DEBUGGING
      if (sc->stack_top > sc->stack_size)
	{
	  fprintf(stderr, "catch set top to %d (%d)\n", sc->stack_top, sc->stack_size);
	  abort();
	}
#endif

      sc->op = OP_APPLY;
    }
  else
    {
      char *str1, *str2;
      str1 = s7_object_to_c_string(sc, type);
      str2 = s7_object_to_c_string(sc, info);

      write_string(sc, "\n;", sc->error_port);
      write_string(sc, str1, sc->error_port);
      write_char(sc, ' ', sc->error_port);
      write_string(sc, str2, sc->error_port);

      if (str1) FREE(str1);
      if (str2) FREE(str2);

      if ((is_input_port(sc->input_port)) &&
	  (is_file_port(sc->input_port)))
	{
	  char *numstr;
	  numstr = (char *)CALLOC(64, sizeof(char));
	  snprintf(numstr, 64, "%d", port_line_number(sc->input_port));

	  write_string(sc, ", ", sc->error_port);
	  write_string(sc, port_filename(sc->input_port), sc->error_port);
	  write_string(sc, " line ", sc->error_port);
	  write_string(sc, numstr, sc->error_port);

	  FREE(numstr);
	}
      write_char(sc, '\n', sc->error_port);

      if ((exit_eval) &&
	  (sc->error_exiter))
	(*(sc->error_exiter))();

      sc->value = type;
      stack_reset(sc);
      sc->op = OP_QUIT;
    }

  if (sc->longjmp_ok)
    longjmp(sc->goto_start, 1); /* this is trying to clear the C stack back to some clean state */

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

static s7_pointer eval_error(s7_scheme *sc, const char *errmsg, s7_pointer obj)
{
  char *str;
  str = s7_object_to_c_string(sc, obj);
  sc->x = s7_string_concatenate(sc, errmsg, str);
  if (str) FREE(str);
  return(s7_error(sc, s7_make_symbol(sc, "error"), sc->x));
}


static s7_pointer g_error(s7_scheme *sc, s7_pointer args)
{
  #define H_error "(error type ...) signals an error"
  if (s7_is_pair(args))
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
  sc->code = car(args);
  if (is_promise(sc->code)) 
    {
      /* Should change type to closure here */
      push_stack(sc, OP_SAVE_FORCED, sc->NIL, sc->code);
      sc->args = sc->NIL;
      push_stack(sc, OP_APPLY, sc->args, sc->code);
      return(sc->NIL);
    } 
  return(sc->code);
}

static s7_pointer apply_list_star(s7_scheme *sc, s7_pointer d) 
{
  s7_pointer p, q;
  if (cdr(d) == sc->NIL) 
    return(car(d));

  p = cons(sc, car(d), cdr(d));
  q = p;
  local_protect(q);
  while(cdr(cdr(p)) != sc->NIL) 
    {
      d = cons(sc, car(p), cdr(p));
      if (cdr(cdr(p)) != sc->NIL) 
	p = cdr(d);
    }
  if (!s7_is_pair(car(cdr(p))))
    return(s7_error(sc, 
		    s7_make_symbol(sc, "wrong-type-arg-error"), 
		    s7_make_string(sc, "apply last argument should be a list")));

  cdr(p) = car(cdr(p));
  local_unprotect(q);
  return(q);
}

static s7_pointer g_apply(s7_scheme *sc, s7_pointer args)
{
  #define H_apply "(apply func ...) applies func to the rest of the arguments"
  sc->code = car(args);

  /* fprintf(stderr, "apply: %s\n", s7_object_to_c_string(sc, cdr(args))); */

  if (cdr(args) == sc->NIL)
    sc->args = sc->NIL;
  else 
    {
      sc->args = apply_list_star(sc, cdr(args));
      /*
      fprintf(stderr, "apply: %s\n", s7_object_to_c_string(sc, sc->args));
      */
      if ((!s7_is_pair(sc->args)) &&
	  (sc->args != sc->NIL))
	return(s7_error(sc, 
			s7_make_symbol(sc, "wrong-type-arg-error"), 
			s7_make_string(sc, "apply last argument should be a list")));
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
  typeflag(x) |= (T_SYNTAX | T_IMMUTABLE | T_CONSTANT); 
  syntax_opcode(x) = small_int(sc, (int)op);
}

s7_pointer s7_call(s7_scheme *sc, s7_pointer func, s7_pointer args)
{ 
  /*
  stack_reset(sc); 
  sc->envir = sc->global_env; 
  */
  /* this can be called while we are in the eval loop (within eval_c_string for instance),
   *   and if we reset the stack, the previously running evaluation steps off the end
   *   of the stack == segfault. 
   */

  if (sc->stack_top == 0)
    {
      if (setjmp(sc->goto_start) != 0)
	{
	  return(sc->value);
	}
      sc->longjmp_ok = true;
    }
  push_stack(sc, OP_EVAL_STRING_DONE, sc->args, sc->code); /* this saves the current evaluation and will eventually finish this (possibly) nested call */
  sc->args = args; 
  sc->code = func; 
  eval(sc, OP_APPLY);
  return(sc->value);
} 

static s7_pointer g_tracing(s7_scheme *sc, s7_pointer a)
{
  #define H_tracing "(tracing bool) turns tracing on or off"
  s7_pointer old_val;
  old_val = (sc->tracing) ? sc->T : sc->F;
  sc->tracing = (car(a) != sc->F);
  return(old_val);
}

static s7_pointer g_scheme_implementation(s7_scheme *sc, s7_pointer a)
{
  #define H_scheme_implementation "(scheme-implementation) returns some string describing the current S7"
  return(s7_make_string(sc, "s7 " S7_VERSION ", " S7_DATE));
}







/* -------------------------------- eval -------------------------------- */

/* all explicit write-* in eval assume current-output-port -- tracing, error fallback handling, etc */
/*   internal reads assume sc->input_port is the input port */

static s7_pointer remember_line(s7_scheme *sc, s7_pointer obj)
{
  if ((sc->input_port != sc->NIL) && 
      (is_file_port(sc->input_port)))
    obj->object.cons.line = port_line_number(sc->input_port);
  return(obj);
}

static void eval(s7_scheme *sc, opcode_t first_op) 
{
  #define   ok_abbrev(x)   (s7_is_pair(x) && cdr(x) == sc->NIL)

  sc->op = first_op;

  /* this procedure can be entered recursively (via s7_call for example), so it's no place for a setjmp
   *   I don't think the recursion can hurt our continuations because s7_call is coming from hooks and
   *   callbacks that are implicit in our stack.
   */

  START:
  
  /*
  if (sc->tracing)
    g_stacktrace(sc, sc->NIL);
  */

  switch (sc->op) 
    {

    case OP_TOP_LEVEL:
      if (is_input_port(sc->input_port))
	port_paren_depth(sc->input_port) = 0;
      
      stack_reset(sc); 
      sc->envir = sc->global_env;
      push_stack(sc, OP_TOP_LEVEL, sc->NIL, sc->NIL);
      push_stack(sc, OP_VALUEPRINT, sc->NIL, sc->NIL);
      push_stack(sc, OP_T1LVL, sc->NIL, sc->NIL);
      goto READ_INTERNAL;
      
      
    case OP_T1LVL: /* top level */
      sc->code = sc->value;
      goto EVAL;
      
      
    READ_INTERNAL:
    case OP_READ_INTERNAL:
      sc->tok = token(sc, sc->input_port);
      if (sc->tok == TOK_EOF) 
	{
	  pop_stack(sc, sc->value);
	  goto START;
	}
      goto READ_EXPRESSION;
      
      
      /* g_read(p) from C 
       *   read one expr, return it, let caller deal with input port setup 
       */
    case OP_READ_RETURN_EXPRESSION:
      /* fprintf(stderr, "read rtn %s\n", s7_object_to_c_string(sc, sc->value)); */
      return;


      /* (read p) from scheme
       *    "p" becomes current input port for eval's duration, then pops back before returning value into calling expr
       */
    case OP_READ_POP_AND_RETURN_EXPRESSION:
      pop_input_port(sc);

      if (sc->tok == TOK_EOF)
	{
	  pop_stack(sc, sc->EOF_OBJECT);
	  goto START;
	}
      pop_stack(sc, sc->value);
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
      return;
      
      
      /* (load "file") in scheme 
       *    read and evaluate all exprs, then upon EOF, close current and pop input port stack
       */
    case OP_LOAD_CLOSE_AND_POP_IF_EOF:
      if (sc->tok != TOK_EOF)
	{
	  push_stack(sc, OP_LOAD_CLOSE_AND_POP_IF_EOF, sc->NIL, sc->NIL);
	  push_stack(sc, OP_READ_INTERNAL, sc->NIL, sc->NIL);
	  sc->code = sc->value;
	  goto EVAL;             /* we read an expression, now evaluate it, and return to read the next */
	}
      s7_close_input_port(sc, sc->input_port);
      pop_input_port(sc);
      pop_stack(sc, sc->UNSPECIFIED);
      goto START;


      /* read and evaluate string expression(s?)
       *    assume caller (C via g_eval_c_string) is dealing with the string port
       */
    case OP_EVAL_STRING:
      /* fprintf(stderr, "op eval string"); */
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
      /* fprintf(stderr, "pushed value: %s (%d)\n", s7_object_to_c_string(sc, sc->value), sc->tok == TOK_EOF); */
      sc->code = sc->value;
      goto EVAL;


    case OP_EVAL_STRING_DONE:
      /* fprintf(stderr, "value: %s\n", s7_object_to_c_string(sc, sc->value)); */
      return;

      
    case OP_VALUEPRINT: /* print evaluation result */
      if (sc->tracing) 
	write_string(sc, "\nGives: ", sc->output_port);
      pop_stack(sc, sc->value);
      goto START;

      
    BEGIN:
    case OP_BEGIN:      /* begin */
      if (!s7_is_pair(sc->code)) 
	{
	  pop_stack(sc, sc->code);
	  goto START;
	}
      
      if (cdr(sc->code) != sc->NIL) 
	push_stack(sc, OP_BEGIN, sc->NIL, cdr(sc->code));
      
      sc->code = car(sc->code);
      /* goto EVAL; */
      
      
    EVAL:
    case OP_EVAL:       /* main part of evaluation */
      
      if (sc->tracing) 
	{
	  /*push_stack(sc, OP_VALUEPRINT, sc->NIL, sc->NIL);*/
	  push_stack(sc, OP_REAL_EVAL, sc->args, sc->code);
	  sc->args = sc->code;
	  write_string(sc, "\nEval: ", sc->output_port);
	  goto P0LIST;
	}
      /* fall through */
      
      
    case OP_REAL_EVAL:
      
      if (s7_is_symbol(sc->code)) 
	{
	  sc->x = s7_find_slot_in_env(sc, sc->envir, sc->code, true);
	  if (sc->x != sc->NIL) 
	    {
	      pop_stack(sc, s7_slot_value_in_env(sc->x));
	      goto START;
	    }

	  if (s7_is_keyword(sc->code))
	    {
	      pop_stack(sc, sc->code); /* a keyword evaluates to itself */
	      goto START;
	    }

	  pop_stack(sc, eval_error(sc, "unbound variable", sc->code));
	  goto START;
	} 
      else 
	if (s7_is_pair(sc->code)) 
	  {
	    sc->x = car(sc->code);
	    if (is_syntax(sc->x))
	      {     
		sc->code = cdr(sc->code);
		sc->op = (opcode_t)integer(syntax_opcode(sc->x)->object.number);
		goto START;
	      } 
	    else 
	      {
		/* first, eval top element and eval arguments */
		push_stack(sc, OP_E0ARGS, sc->NIL, sc->code);
		sc->code = car(sc->code);
		goto EVAL;
	      }
	  } 
	else 
	  {
	    pop_stack(sc, sc->code);
	    goto START;
	  }
      
    case OP_E0ARGS:     /* eval arguments */
      if (is_macro(sc->value)) 
	{    
	  /* macro expansion */
	  push_stack(sc, OP_DOMACRO, sc->NIL, sc->NIL);
	  sc->args = cons(sc, sc->code, sc->NIL);
	  sc->code = sc->value;
	  goto APPLY;
	} 
      else 
	{
	  sc->code = cdr(sc->code);
	  
	  /* here args is nil, value is the operator (car of list), code is the rest -- the args.
	   *   e0args can be called within e1args loop if it's a nested expression:
	   * (+ 1 2 (* 2 3)):
	   e0args: (), value: +, code: (1 2 (* 2 3))
	   e1args: (+), value: +, code: (1 2 (* 2 3))
	   e1args: (1 +), value: +, code: (2 (* 2 3))
	   e1args: (2 1 +), value: +, code: ((* 2 3))
	   e0args: (), value: *, code: (2 3)
	   e1args: (*), value: *, code: (2 3)
	   e1args: (2 *), value: *, code: (3)
	   e1args: (3 2 *), value: *, code: ()
	   <end -> apply the * op>
	   e1args: (6 2 1 +), value: +, code: ()
	  */
	  
	}
      
      /* e1args causes 80 to 90% of the consing in the evaluator! */

      
    case OP_E1ARGS:     /* eval arguments */
      /*
      fprintf(stderr, "e1args: [%p]%s [%p]%s\n", sc->value, s7_object_to_c_string(sc, sc->value), sc->args, s7_object_to_c_string(sc, sc->args));
      */
      sc->args = cons(sc, sc->value, sc->args);  /* this line is the speed killer for the whole program */
      /*   I've failed twice to find a way to reduce the consing */
      /*  the no cons method:
       * E0ARGS:	    sc->args = sc->code;
       * E1ARGS:          car(sc->code) = sc->value;
       *                  sc->code = cdr(sc->code);
       *                  if (s7_is_pair(sc->code))
       *                    {
       *                      push_stack(sc, OP_E1ARGS, sc->args, sc->code);
       *                      sc->code = car(sc->code);
       *                      sc->args = sc->NIL;
       *                      goto EVAL;
       *                    }
       *                  else
       *                    {
       *                      sc->code = car(sc->args);
       *                      sc->args = cdr(sc->args);
       *                      -- drop into apply --
       *                    }
       * but even with copy-tree of the procedure-source (below), this fails in odd cases --
       * somehow the current arg list infects the original procedure-source.
       */
      
      if (s7_is_pair(sc->code)) 
	{ /* continue */
	  push_stack(sc, OP_E1ARGS, sc->args, cdr(sc->code));
	  
	  sc->code = car(sc->code);
	  sc->args = sc->NIL;
	  goto EVAL;
	} 
      else 
	{  /* end */
	  sc->args = s7_reverse_in_place(sc, sc->NIL, sc->args); 
	  sc->code = car(sc->args);
	  sc->args = cdr(sc->args);
	  /* goto APPLY;  */
	}
      

    APPLY:
    case OP_APPLY:      /* apply 'code' to 'args' */
      
      if (sc->tracing) 
	{
	  push_stack(sc, OP_REAL_APPLY, sc->args, sc->code);
	  write_string(sc, "\nApply to: ", sc->output_port);
	  goto P0LIST;
	}
      /* fall through */
      

    case OP_REAL_APPLY:
      
      if (s7_is_function(sc->code))
	{
	  int len;
	  len = s7_list_length(sc, sc->args);
	  if (len < function_required_args(sc->code))
	    {
	      pop_stack(sc, eval_error(sc, "not enough arguments", sc->code));
	      goto START;
	    }

	  if ((!function_has_rest_arg(sc->code)) &&
	      ((function_required_args(sc->code) + function_optional_args(sc->code)) < len))
	    {
	      pop_stack(sc, eval_error(sc, "too many arguments", sc->x = cons(sc, sc->code, sc->args)));
	      goto START;
	    }

	  sc ->x = function_call(sc->code)(sc, sc->args);
	  
#if S7_DEBUGGING
	  if (!is_object(sc->x))
	    {
	      fprintf(stderr, "%s returned: %p\n", function_name(sc->code), sc->x);
	      abort();
	    }
#endif

	  pop_stack(sc, sc->x);
	  goto START;
	}
      
      else 
	{
	  if (s7_is_closure(sc->code) || is_macro(sc->code) || is_promise(sc->code)) 
	    { 
	      new_frame_in_env(sc, s7_procedure_environment(sc->code)); 

	      /* load up the current args into the ((args) (lambda)) layout [via the current environment] */
	      for (sc->x = car(procedure_source(sc->code)), sc->y = sc->args; s7_is_pair(sc->x); sc->x = cdr(sc->x), sc->y = cdr(sc->y)) 
		{
		  if (sc->y == sc->NIL)
		    {
		      pop_stack(sc, eval_error(sc, "not enough arguments", sc->code));
		      goto START;
		    }
		  
		  new_slot_in_env(sc, car(sc->x), car(sc->y));
		}
	      
	      if (sc->x == sc->NIL) 
		{
		  if (sc->y != sc->NIL)
		    fprintf(stderr, "%s too many args? %s\n", 
			    s7_object_to_c_string(sc, sc->code),
			    s7_object_to_c_string(sc, sc->args));
		} 
	      else 
		{
		  if (s7_is_symbol(sc->x))
		    new_slot_in_env(sc, sc->x, sc->y); 
		  else 
		    {
		      pop_stack(sc, eval_error(sc, "syntax error in closure: not a symbol", sc->x));
		      goto START;
		    }
		}
	      sc->code = cdr(procedure_source(sc->code));
	      sc->args = sc->NIL;
	      goto BEGIN;
	    }
	  else 
	    {
	      if (s7_is_continuation(sc->code)) 
		{ 
		  s7_call_continuation(sc, sc->code);
		  pop_stack(sc, sc->args != sc->NIL ? car(sc->args) : sc->NIL);
		  goto START;
		} 
	      else 
		{
		  if (is_goto(sc->code))
		    {
		      sc->stack_top = (sc->code)->object.goto_loc;

#if S7_DEBUGGING
		      if (sc->stack_top > sc->stack_size)
			{
			  fprintf(stderr, "goto set top to %d (%d)\n", sc->stack_top, sc->stack_size);
			  abort();
			}
#endif

		      pop_stack(sc, sc->args != sc->NIL ? car(sc->args) : sc->NIL);
		      goto START;
		    }
		  else
		    {
		      if (s7_is_object(sc->code))
			{
			  sc ->x = s7_apply_object(sc, sc->code, sc->args);
			  pop_stack(sc, sc->x);
			  goto START;
			}
		      else 
			{
			  pop_stack(sc, eval_error(sc, "apply of non-function?", sc->code));
			  goto START;
			}
		    }
		}
	    }
	}
      

    case OP_DOMACRO:    /* macro after args are gathered */
      sc->code = sc->value;
      goto EVAL;
      

    case OP_LAMBDA:     /* lambda */
      pop_stack(sc, s7_make_closure(sc, sc->code, sc->envir));
      goto START;
      

    case OP_QUOTE:      /* quote */
      pop_stack(sc, car(sc->code));
      goto START;
      

    case OP_DEF0:  /* define */
      if (s7_is_immutable(car(sc->code)))
	{
	  pop_stack(sc, eval_error(sc, "define: unable to alter immutable object", car(sc->code)));
	  goto START;
	}
      
      if (s7_is_pair(car(sc->code))) 
	{
	  sc->x = caar(sc->code);
	  sc->code = cons(sc, sc->LAMBDA, cons(sc, cdar(sc->code), cdr(sc->code)));
	} 
      else 
	{
	  sc->x = car(sc->code);
	  sc->code = cadr(sc->code);
	}
      if (!s7_is_symbol(sc->x))
	{
	  pop_stack(sc, eval_error(sc, "define a non-symbol?", sc->x));
	  goto START;
	}
      
      push_stack(sc, OP_DEF1, sc->NIL, sc->x);
      goto EVAL;
      

    case OP_DEF1:  /* define */
      /* sc->code is the symbol being defined, sc->value is its value
       *   if sc->value is a closure, car is or the form ((args...) body...)
       *   so the doc string if any is (cadr (car value))
       *   and the arg list gives the number of required args up to the dot
       */
      /*
      fprintf(stderr, "define %s %s\n", s7_object_to_c_string(sc, sc->code), s7_object_to_c_string(sc, sc->value));
      if (s7_is_closure(sc->value))
	fprintf(stderr, "-> %s %s\n", s7_object_to_c_string(sc, car(sc->value)), s7_object_to_c_string(sc, cdr(sc->value)));
      */
      sc->x = s7_find_slot_in_env(sc, sc->envir, sc->code, false);
      if (sc->x != sc->NIL) 
	set_slot_in_env(sc, sc->x, sc->value); 
      else new_slot_in_env(sc, sc->code, sc->value); 
      pop_stack(sc, sc->code);
      goto START;
      

    case OP_SET0:
      if (s7_is_immutable(car(sc->code)))
	{
	  pop_stack(sc, eval_error(sc, "set!: unable to alter immutable variable", car(sc->code)));
	  goto START;
	}
      
      if (s7_is_pair(car(sc->code))) /* has accessor */
	{
	  sc->x = s7_symbol_value(sc, caar(sc->code));
	  if ((s7_is_object(sc->x)) &&
	      (object_set_function(sc->x)))
	    sc->code = cons(sc, sc->SET_OBJECT, s7_append(sc, car(sc->code), cdr(sc->code)));   /* use set method */
	  else 
	    {
	      pop_stack(sc, eval_error(sc, "no generalized set for this variable", caar(sc->code)));
	      goto START;
	    }
	}
      else 
	{
	  push_stack(sc, OP_SET1, sc->NIL, car(sc->code));
	  sc->code = cadr(sc->code);
	}
      goto EVAL;
      

    case OP_SET1:      
      sc->y = s7_find_slot_in_env(sc, sc->envir, sc->code, true);
      if (sc->y != sc->NIL) 
	{
	  set_slot_in_env(sc, sc->y, sc->value); 
	  pop_stack(sc, sc->value);
	  goto START;
	}
      else 
	{
	  pop_stack(sc, eval_error(sc, "set!: unbound variable", sc->code));
	  goto START;
	}
      

    case OP_IF0:
      push_stack(sc, OP_IF1, sc->NIL, cdr(sc->code));
      sc->code = car(sc->code);
      goto EVAL;
      

    case OP_IF1:
      if (is_true(sc->value))
	sc->code = car(sc->code);
      else
	sc->code = cadr(sc->code);  /* (if #f 1) ==> #<unspecified> because car(sc->NIL) = sc->UNSPECIFIED */
      goto EVAL;
      

    case OP_LET0:       /* let */
      sc->args = sc->NIL;
      sc->value = sc->code;
      sc->code = s7_is_symbol(car(sc->code)) ? cadr(sc->code) : car(sc->code);
      

    case OP_LET1:       /* let (calculate parameters) */
      sc->args = cons(sc, sc->value, sc->args);
      if (s7_is_pair(sc->code)) 
	{ /* continue */
	  push_stack(sc, OP_LET1, sc->args, cdr(sc->code));
	  sc->code = cadar(sc->code);
	  sc->args = sc->NIL;
	  goto EVAL;
	} 
      else 
	{  /* end */
	  sc->args = s7_reverse_in_place(sc, sc->NIL, sc->args);
	  sc->code = car(sc->args);
	  sc->args = cdr(sc->args);
	}
      

    case OP_LET2:       /* let */
      new_frame_in_env(sc, sc->envir); 
      for (sc->x = s7_is_symbol(car(sc->code)) ? cadr(sc->code) : car(sc->code), sc->y = sc->args;
	   sc->y != sc->NIL; sc->x = cdr(sc->x), sc->y = cdr(sc->y)) 
	{
	  new_slot_in_env(sc, caar(sc->x), car(sc->y)); 
	}
      if (s7_is_symbol(car(sc->code))) 
	{    /* named let */
	  for (sc->x = cadr(sc->code), sc->args = sc->NIL; sc->x != sc->NIL; sc->x = cdr(sc->x)) 
	    sc->args = cons(sc, caar(sc->x), sc->args);
	  
	  sc->x = s7_make_closure(sc, cons(sc, s7_reverse_in_place(sc, sc->NIL, sc->args), cddr(sc->code)), sc->envir); 
	  new_slot_in_env(sc, car(sc->code), sc->x); 
	  sc->code = cddr(sc->code);
	  sc->args = sc->NIL;
	} 
      else 
	{
	  sc->code = cdr(sc->code);
	  sc->args = sc->NIL;
	}
      goto BEGIN;
      

    case OP_LET0AST:    /* let* */
      if (car(sc->code) == sc->NIL) 
	{
	  new_frame_in_env(sc, sc->envir); 
	  sc->code = cdr(sc->code);
	  goto BEGIN;
	}
      push_stack(sc, OP_LET1AST, cdr(sc->code), car(sc->code));
      sc->code = cadaar(sc->code);
      goto EVAL;
      

    case OP_LET1AST:    /* let* (make new frame) */
      new_frame_in_env(sc, sc->envir); 
      

    case OP_LET2AST:    /* let* (calculate parameters) */
      new_slot_in_env(sc, caar(sc->code), sc->value); 
      sc->code = cdr(sc->code);
      if (s7_is_pair(sc->code)) 
	{ /* continue */
	  push_stack(sc, OP_LET2AST, sc->args, sc->code);
	  sc->code = cadar(sc->code);
	  sc->args = sc->NIL;
	  goto EVAL;
	} 
      else 
	{  /* end */
	  sc->code = sc->args;
	  sc->args = sc->NIL;
	  goto BEGIN;
	}
      
      
    case OP_LET0REC:    /* letrec */
      new_frame_in_env(sc, sc->envir); 
      sc->args = sc->NIL;
      sc->value = sc->code;
      sc->code = car(sc->code);
      

    case OP_LET1REC:    /* letrec (calculate parameters) */
      sc->args = cons(sc, sc->value, sc->args);
      if (s7_is_pair(sc->code)) 
	{ /* continue */
	  push_stack(sc, OP_LET1REC, sc->args, cdr(sc->code));
	  sc->code = cadar(sc->code);
	  sc->args = sc->NIL;
	  goto EVAL;
	} 
      else 
	{  /* end */
	  sc->args = s7_reverse_in_place(sc, sc->NIL, sc->args); 
	  sc->code = car(sc->args);
	  sc->args = cdr(sc->args);
	}

      
    case OP_LET2REC:    /* letrec */
      for (sc->x = car(sc->code), sc->y = sc->args; sc->y != sc->NIL; sc->x = cdr(sc->x), sc->y = cdr(sc->y)) 
	new_slot_in_env(sc, caar(sc->x), car(sc->y)); 
      
      sc->code = cdr(sc->code);
      sc->args = sc->NIL;
      goto BEGIN;
      

    case OP_COND0:      /* cond */
      if (!s7_is_pair(sc->code)) 
	{
	  pop_stack(sc, eval_error(sc, "syntax error in cond", sc->code));
	  goto START;
	}
      push_stack(sc, OP_COND1, sc->NIL, sc->code);
      sc->code = caar(sc->code);
      goto EVAL;
      

    case OP_COND1:      /* cond */
      if (is_true(sc->value)) 
	{
	  sc->code = cdar(sc->code);
	  if (sc->code == sc->NIL)
	    {
	      pop_stack(sc, sc->value);
	      goto START;
	    }
	  if (car(sc->code) == sc->FEED_TO) 
	    {
	      if (!s7_is_pair(cdr(sc->code))) 
		{
		  pop_stack(sc, eval_error(sc, "syntax error in cond", cdr(sc->code)));
		  goto START;
		}
	      sc->x = cons(sc, sc->QUOTE, cons(sc, sc->value, sc->NIL));
	      sc->code = cons(sc, cadr(sc->code), cons(sc, sc->x, sc->NIL));
	      goto EVAL;
	    }
	  goto BEGIN;
	}
      else 
	{
	  sc->code = cdr(sc->code);
	  if (sc->code == sc->NIL)
	    {
	      pop_stack(sc, sc->NIL);
	      goto START;
	    } 
	  else 
	    {
	      push_stack(sc, OP_COND1, sc->NIL, sc->code);
	      sc->code = caar(sc->code);
	      goto EVAL;
	    }
	}

      
    case OP_DELAY:      /* delay */
      sc->x = s7_make_closure(sc, cons(sc, sc->NIL, sc->code), sc->envir);
      set_type(sc->x, T_PROMISE);
      pop_stack(sc, sc->x);
      goto START;
      

    case OP_AND0:       /* and */
      if (sc->code == sc->NIL) 
	{
	  pop_stack(sc, sc->T);
	  goto START;
	}
      push_stack(sc, OP_AND1, sc->NIL, cdr(sc->code));
      sc->code = car(sc->code);
      goto EVAL;

      
    case OP_AND1:       /* and */
      if (is_false(sc->value)) 
	{
	  pop_stack(sc, sc->value);
	  goto START;
	}
      if (sc->code == sc->NIL) 
	{
	  pop_stack(sc, sc->value);
	  goto START;
	}
      if (cdr(sc->code) != sc->NIL)
	push_stack(sc, OP_AND1, sc->NIL, cdr(sc->code));
      sc->code = car(sc->code);
      goto EVAL;
      

    case OP_OR0:        /* or */
      if (sc->code == sc->NIL) 
	{
	  pop_stack(sc, sc->F);
	  goto START;
	}
      push_stack(sc, OP_OR1, sc->NIL, cdr(sc->code));
      sc->code = car(sc->code);
      goto EVAL;
      

    case OP_OR1:        /* or */
      if (is_true(sc->value)) 
	{
	  pop_stack(sc, sc->value);
	  goto START;
	}
      if (sc->code == sc->NIL) 
	{
	  pop_stack(sc, sc->value);
	  goto START;
	}
      if (cdr(sc->code) != sc->NIL)
	push_stack(sc, OP_OR1, sc->NIL, cdr(sc->code));
      sc->code = car(sc->code);
      goto EVAL;

      
    case OP_C0STREAM:   /* cons-stream */
      push_stack(sc, OP_C1STREAM, sc->NIL, cdr(sc->code));
      sc->code = car(sc->code);
      goto EVAL;

      
    case OP_C1STREAM:   /* cons-stream */
      sc->args = sc->value;  /* save sc->value to register sc->args for gc */
      sc->x = s7_make_closure(sc, cons(sc, sc->NIL, sc->code), sc->envir);
      set_type(sc->x, T_PROMISE);
      pop_stack(sc, cons(sc, sc->args, sc->x));
      goto START;      


    case OP_MACRO0:     /* macro */
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
      if (s7_is_pair(car(sc->code))) 
	{
	  sc->x = caar(sc->code);
	  sc->code = cons(sc, sc->LAMBDA, cons(sc, cdar(sc->code), cdr(sc->code)));
	} 
      else 
	{
	  sc->x = car(sc->code);
	  sc->code = cadr(sc->code);
	}
      
      if (!s7_is_symbol(sc->x)) 
	{
	  pop_stack(sc, eval_error(sc, "variable is not a symbol", sc->x));
	  goto START;
	}
      /*
      fprintf(stderr, "orig macro mid: %s\n", 
	      s7_object_to_c_string(sc, sc->code));
      */
      
      push_stack(sc, OP_MACRO1, sc->NIL, sc->x);   /* sc->x (the name symbol) will be sc->code when we pop to OP_MACRO1 */
      goto EVAL;
      

    case OP_MACRO1:     /* macro */

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
      sc->x = s7_find_slot_in_env(sc, sc->envir, sc->code, false); 
      if (sc->x != sc->NIL) 
	set_slot_in_env(sc, sc->x, sc->value); 
      else new_slot_in_env(sc, sc->code, sc->value); 
      
      /* pop back to wherever the macro call was */
      pop_stack(sc, sc->code);
      goto START;


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

      sc->code = cons(sc,
		      sc->LAMBDA,
		      cons(sc, 
			   cons(sc, sc->y, sc->NIL),
			   cons(sc, 
				cons(sc, 
				     s7_make_symbol(sc, "apply"),
				     cons(sc, 
					  cons(sc, sc->LAMBDA, cddr(sc->value)),
					  cons(sc, 
					       cons(sc,
						    s7_make_symbol(sc, "cdr"),
						    cons(sc, 
							 sc->y, 
							 sc->NIL)),
					       sc->NIL))),
				sc->NIL)));
      
      /* so, (defmacro hi (a b) `(+ ,a ,b)) becomes:
       *   sc->x: hi
       *   sc->code: (lambda (defmac-51) (apply (lambda (a b) (quasiquote (+ (unquote a) (unquote b)))) (cdr defmac-51)))
       */

      /* fprintf(stderr, "sc->x: %s, sc->code: %s\n", s7_object_to_c_string(sc, sc->x), s7_object_to_c_string(sc, sc->code)); */

      push_stack(sc, OP_MACRO1, sc->NIL, sc->x);   /* sc->x (the name symbol) will be sc->code when we pop to OP_MACRO1 */
      goto EVAL;


    case OP_DEFINE_MACRO:

      sc->y = s7_gensym(sc, "defmac");
      sc->x = caar(sc->code);

      sc->code = cons(sc,
		      sc->LAMBDA,
		      cons(sc, 
			   cons(sc, sc->y, sc->NIL),
			   cons(sc, 
				cons(sc, 
				     s7_make_symbol(sc, "apply"),
				     cons(sc, 
					  cons(sc, 
					       sc->LAMBDA,
					       cons(sc, cdadr(sc->value), cddr(sc->value))),
					  cons(sc, 
					       cons(sc,
						    s7_make_symbol(sc, "cdr"), 
						    cons(sc, 
							 sc->y,
							 sc->NIL)),
					       sc->NIL))),
				sc->NIL)));

      /* fprintf(stderr, "sc->x: %s, sc->code: %s\n", s7_object_to_c_string(sc, sc->x), s7_object_to_c_string(sc, sc->code)); */

      /* (define-macro (hi a b) `(+ ,a ,b)) becomes:
       *   sc->x: hi
       *   sc->code: (lambda (defmac-51) (apply (lambda (a b) (quasiquote (+ (unquote a) (unquote b)))) (cdr defmac-51)))
       */

      push_stack(sc, OP_MACRO1, sc->NIL, sc->x);   /* sc->x (the name symbol) will be sc->code when we pop to OP_MACRO1 */
      goto EVAL;


    case OP_CASE0:      /* case */
      push_stack(sc, OP_CASE1, sc->NIL, cdr(sc->code));
      sc->code = car(sc->code);
      goto EVAL;
      

    case OP_CASE1:      /* case */
      for (sc->x = sc->code; sc->x != sc->NIL; sc->x = cdr(sc->x)) 
	{
	  if (!s7_is_pair(sc->y = caar(sc->x))) 
	    break;

	  for ( ; sc->y != sc->NIL; sc->y = cdr(sc->y)) 
	    if (s7_is_eqv(car(sc->y), sc->value)) 
	      break;

	  if (sc->y != sc->NIL) 
	    break;
	}

      if (sc->x != sc->NIL) 
	{
	  if (s7_is_pair(caar(sc->x))) 
	    {
	      sc->code = cdar(sc->x);
	      goto BEGIN;
	    } 
	  else 
	    {/* else */
	      push_stack(sc, OP_CASE2, sc->NIL, cdar(sc->x));
	      sc->code = caar(sc->x);
	      goto EVAL;
	    }
	} 
      else 
	{
	  pop_stack(sc, sc->NIL);
	  goto START;
	}
      

    case OP_CASE2:      /* case */
      if (is_true(sc->value)) 
	goto BEGIN;
      pop_stack(sc, sc->NIL);
      goto START;

      
    case OP_SAVE_FORCED:     /* Save forced value replacing promise */
      memcpy(sc->code, sc->value, sizeof(struct s7_cell));
      pop_stack(sc, sc->value);
      goto START;
      

    case OP_QUIT:
      return;
      break;


    case OP_CATCH:
      pop_stack(sc, sc->value);
      goto START;
      break;
      
      
    READ_EXPRESSION:
    case OP_READ_EXPRESSION:
      
      switch (sc->tok) 
	{
	case TOK_EOF:
	  pop_stack(sc, sc->EOF_OBJECT);
	  goto START;

	case TOK_VEC:
	  push_stack(sc, OP_READ_VEC, sc->NIL, sc->NIL);
	  /* fall through */
	  
	case TOK_LPAREN:
	  sc->tok = token(sc, sc->input_port);
	  if (sc->tok == TOK_RPAREN) 
	    {
	      pop_stack(sc, sc->NIL);
	      goto START;
	    }
	  else 
	    {
	      if (sc->tok == TOK_DOT) 
		{
		  pop_stack(sc, eval_error(sc, "syntax error: illegal dot expression", sc->code)); /* just a guess -- maybe sc->args */
		  goto START;
		}
	      else 
		{
		  if (is_input_port(sc->input_port))
		    port_paren_depth(sc->input_port)++;
		  push_stack(sc, OP_READ_LIST, sc->NIL, sc->NIL);
		  goto READ_EXPRESSION;
		}
	    }
	  
	case TOK_QUOTE:
	  push_stack(sc, OP_READ_QUOTE, sc->NIL, sc->NIL);
	  sc->tok = token(sc, sc->input_port);
	  goto READ_EXPRESSION;
	  
	case TOK_BQUOTE:
	  sc->tok = token(sc, sc->input_port);
	  if (sc->tok== TOK_VEC) 
	    {
	      push_stack(sc, OP_READ_QUASIQUOTE_VECTOR, sc->NIL, sc->NIL);
	      sc->tok= TOK_LPAREN;
	      goto READ_EXPRESSION;
	    } 
	  else push_stack(sc, OP_READ_QUASIQUOTE, sc->NIL, sc->NIL);
	  goto READ_EXPRESSION;
	  
	case TOK_COMMA:
	  push_stack(sc, OP_READ_UNQUOTE, sc->NIL, sc->NIL);
	  sc->tok = token(sc, sc->input_port);
	  goto READ_EXPRESSION;
	  
	case TOK_ATMARK:
	  push_stack(sc, OP_READ_UNQUOTE_SPLICING, sc->NIL, sc->NIL);
	  sc->tok = token(sc, sc->input_port);
	  goto READ_EXPRESSION;
	  
	case TOK_ATOM:
	  pop_stack(sc, make_atom(sc, read_string_upto(sc, "();\t\n\r ", sc->input_port), 10)); /* if reading list (from lparen), this will finally get us to rdlist */
	  goto START;
	  
	case TOK_DQUOTE:
	  sc->x = read_string_expression(sc, sc->input_port);
	  if (sc->x == sc->F) 
	    {
	      pop_stack(sc, eval_error(sc, "error reading string", sc->code));
	      goto START;
	    }
	  s7_set_immutable(sc->x);
	  pop_stack(sc, sc->x);
	  goto START;
	  
	case TOK_SHARP_CONST:
	  {
	    char *expr;
	    expr = read_string_upto(sc, "();\t\n\r ", sc->input_port);
	    if ((sc->x = make_sharp_const(sc, expr)) == sc->NIL)
	      {
		pop_stack(sc, eval_error(sc, "undefined sharp expression", sc->code));
		goto START;
	      }
	    else 
	      {
		pop_stack(sc, sc->x);
		goto START;
	      }
	  }
	default:
	  if (sc->tok == TOK_RPAREN)
	    {
	      pop_stack(sc, eval_error(sc, "too many close parens", sc->code));
	      goto START;
	    }
	  else 
	    {
	      pop_stack(sc, eval_error(sc, "syntax error: illegal token", sc->code));
	      goto START;
	    }
	}
      break;
      

    case OP_READ_LIST: 
      sc->args = cons(sc, sc->value, sc->args);
      sc->tok = token(sc, sc->input_port);
      if (sc->tok == TOK_RPAREN) 
	{
	  int c;
	  if (is_input_port(sc->input_port))
	    port_paren_depth(sc->input_port)--;
	  
	  c = inchar(sc, sc->input_port);
	  if ((c != '\n') && (c != EOF))
	    backchar(sc, c, sc->input_port);
	    
	  pop_stack(sc, remember_line(sc, s7_reverse_in_place(sc, sc->NIL, sc->args)));
	  goto START;
	} 
      else 
	{
	  if (sc->tok == TOK_DOT) 
	    {
	      push_stack(sc, OP_READ_DOT, sc->args, sc->NIL);
	      sc->tok = token(sc, sc->input_port);
	      goto READ_EXPRESSION;
	    } 
	  else 
	    {
	      if (sc->tok == TOK_EOF)
		{
		  /* `(+ 2 3) as a stand-alone expression confuses the close paren check */
		  if ((sc->stack_top > 4) &&
		      ((opcode_t)s7_integer(vector_element(sc->stack, sc->stack_top - 1)) == OP_READ_QUASIQUOTE))
		    {
		      pop_stack(sc, sc->code);
		      goto START;

		      /* TODO: fix unlisted quasiquote */
		    }

		  pop_stack(sc, eval_error(sc, "missing close paren?", sc->NIL));
		  goto START;
		}
	      else
		{
		  push_stack(sc, OP_READ_LIST, sc->args, sc->NIL);;
		  goto READ_EXPRESSION;
		}
	    }
	}
      

    case OP_READ_DOT:
      if (token(sc, sc->input_port) != TOK_RPAREN)
	{
	  pop_stack(sc, eval_error(sc, "syntax error: illegal dot expression", sc->code));
	  goto START;
	}
      else 
	{
	  if (is_input_port(sc->input_port))
	    port_paren_depth(sc->input_port)--;
	  
	  pop_stack(sc, s7_reverse_in_place(sc, sc->value, sc->args));
	  goto START;
	}
      

    case OP_READ_QUOTE:
      pop_stack(sc, cons(sc, sc->QUOTE, cons(sc, sc->value, sc->NIL)));
      goto START;      
      

    case OP_READ_QUASIQUOTE:
      pop_stack(sc, cons(sc, sc->QUASIQUOTE, cons(sc, sc->value, sc->NIL)));
      goto START;
      

    case OP_READ_QUASIQUOTE_VECTOR:
      pop_stack(sc, cons(sc, s7_make_symbol(sc, "apply"),
			 cons(sc, s7_make_symbol(sc, "vector"), 
			      cons(sc, 
				   cons(sc, 
					sc->QUASIQUOTE, 
					cons(sc, sc->value, sc->NIL)),
				   sc->NIL))));
      goto START;
      

    case OP_READ_UNQUOTE:
      pop_stack(sc, cons(sc, sc->UNQUOTE, cons(sc, sc->value, sc->NIL)));
      goto START;
      

    case OP_READ_UNQUOTE_SPLICING:
      pop_stack(sc, cons(sc, sc->UNQUOTE_SPLICING, cons(sc, sc->value, sc->NIL)));
      goto START;
      

    case OP_READ_VEC:
      sc->args = sc->value;
      pop_stack(sc, g_vector(sc, sc->args));
      goto START;
      
      
    P0LIST:
    case OP_P0LIST:
      if (s7_is_vector(sc->args)) 
	{
	  write_string(sc, "#(", sc->output_port);
	  sc->args = cons(sc, sc->args, s7_make_integer(sc, 0)); /* loop_counter? */
	  goto PVECFROM;
	} 
      else if (!s7_is_pair(sc->args)) 
	{
	  print_atom(sc, sc->args);
	  pop_stack(sc, sc->T);
	  goto START;
	} 
      else if (car(sc->args) == sc->QUOTE && ok_abbrev(cdr(sc->args))) 
	{
	  write_string(sc, "'", sc->output_port);
	  sc->args = cadr(sc->args);
	  goto P0LIST;
	} 
      else if (car(sc->args) == sc->QUASIQUOTE && ok_abbrev(cdr(sc->args))) 
	{
	  write_string(sc, "`", sc->output_port);
	  sc->args = cadr(sc->args);
	  goto P0LIST;
	} 
      else if (car(sc->args) == sc->UNQUOTE && ok_abbrev(cdr(sc->args))) 
	{
	  write_string(sc, ", ", sc->output_port);
	  sc->args = cadr(sc->args);
	  goto P0LIST;
	} 
      else if (car(sc->args) == sc->UNQUOTE_SPLICING && ok_abbrev(cdr(sc->args))) 
	{
	  write_string(sc, ",@", sc->output_port);
	  sc->args = cadr(sc->args);
	  goto P0LIST;
	} 
      else 
	{
	  write_string(sc, "(", sc->output_port);
	  push_stack(sc, OP_P1LIST, cdr(sc->args), sc->NIL);
	  sc->args = car(sc->args);
	  goto P0LIST;
	}
      

    case OP_P1LIST:
      if (s7_is_pair(sc->args)) 
	{
	  push_stack(sc, OP_P1LIST, cdr(sc->args), sc->NIL);
	  write_string(sc, " ", sc->output_port);
	  sc->args = car(sc->args);
	  goto P0LIST;
	} 
      else if (s7_is_vector(sc->args)) 
	{
	  push_stack(sc, OP_P1LIST, sc->NIL, sc->NIL);
	  write_string(sc, " . ", sc->output_port);
	  goto P0LIST;
	} 
      else 
	{
	  if (sc->args != sc->NIL) 
	    {
	      write_string(sc, " . ", sc->output_port);
	      print_atom(sc, sc->args);
	    }
	  write_string(sc, ")", sc->output_port);
	  pop_stack(sc, sc->T);
	  goto START;
	}
      

    PVECFROM:
    case OP_PVECFROM: 
      {
	int i = loop_counter(cdr(sc->args));
	s7_pointer vec = car(sc->args);
	int len = vector_length(vec);
	if (i == len) 
	  {
	    write_string(sc, ")", sc->output_port);
	    pop_stack(sc, sc->T);
	    goto START;
	  } 
	else 
	  {
	    s7_pointer elem = vector_element(vec, i);
	    loop_counter(cdr(sc->args)) = i + 1;
	    push_stack(sc, OP_PVECFROM, sc->args, sc->NIL);
	    sc->args = elem;
	    write_string(sc, " ", sc->output_port);
	    goto P0LIST;
	  }
      }
      

    default:
      pop_stack(sc, eval_error(sc, "unknown operator!", s7_make_integer(sc, sc->op)));
      goto START;
    }
}



/* -------------------------------- S7 init -------------------------------- */

s7_scheme *s7_init(void) 
{
  int i;
  s7_pointer x;
  
  s7_scheme *sc;
  sc = (s7_scheme *)MALLOC(sizeof(s7_scheme));
  
  sc->gc_off = true; /* sc->args and so on are not set yet, so a gc during init -> segfault */
  sc->longjmp_ok = false;
  sc->gensym_cnt = 0;
  sc->strbuf_size = INITIAL_STRBUF_SIZE;
  sc->strbuf = (char*)CALLOC(sc->strbuf_size, sizeof(char));
  
  sc->NIL = &sc->_NIL;
  sc->T = &sc->_HASHT;
  sc->F = &sc->_HASHF;
  sc->EOF_OBJECT = &sc->_EOF_OBJECT;
  sc->UNDEFINED = &sc->_UNDEFINED;
  sc->UNSPECIFIED = &sc->_UNSPECIFIED;
  
  set_type(sc->UNSPECIFIED, T_NIL_TYPE | T_ATOM | T_GC_MARK | T_IMMUTABLE | T_CONSTANT);
  car(sc->UNSPECIFIED) = cdr(sc->UNSPECIFIED) = sc->UNSPECIFIED;
  
  set_type(sc->NIL, T_NIL_TYPE | T_ATOM | T_GC_MARK | T_IMMUTABLE | T_CONSTANT);
  car(sc->NIL) = cdr(sc->NIL) = sc->UNSPECIFIED;
  
  set_type(sc->T, T_NIL_TYPE | T_ATOM | T_GC_MARK | T_IMMUTABLE | T_CONSTANT);
  car(sc->T) = cdr(sc->T) = sc->UNSPECIFIED;
  
  set_type(sc->F, T_NIL_TYPE | T_ATOM | T_GC_MARK | T_IMMUTABLE | T_CONSTANT);
  car(sc->F) = cdr(sc->F) = sc->UNSPECIFIED;
  
  set_type(sc->EOF_OBJECT, T_NIL_TYPE | T_ATOM | T_GC_MARK | T_IMMUTABLE | T_CONSTANT);
  car(sc->EOF_OBJECT) = cdr(sc->EOF_OBJECT) = sc->UNSPECIFIED;
  
  set_type(sc->UNDEFINED, T_NIL_TYPE | T_ATOM | T_GC_MARK | T_IMMUTABLE | T_CONSTANT);
  car(sc->UNDEFINED) = cdr(sc->UNDEFINED) = sc->UNSPECIFIED;
  
  sc->input_port = sc->NIL;
  sc->input_port_stack = sc->NIL;
  sc->output_port = sc->NIL;
  sc->error_port = sc->NIL;
  
  sc->x = sc->NIL;
  sc->y = sc->NIL;
  
  sc->error_exiter = NULL;
  
#if TIMING
  sc->gc_mark_time = 0;
  sc->gc_sweep_time = 0;
  sc->total_freed = 0;
  sc->gc_sweeps = 0;
#endif
  
  sc->heap_size = INITIAL_HEAP_SIZE;
  sc->heap = (s7_pointer *)CALLOC(sc->heap_size, sizeof(s7_pointer));
  
  sc->free_heap_size = INITIAL_HEAP_SIZE;
  sc->free_heap = (int *)CALLOC(sc->free_heap_size, sizeof(int));
  sc->free_heap_top = 0;
  {
    for (i = 0; i < INITIAL_HEAP_SIZE; i++)
      {
	sc->heap[i] = (s7_cell *)CALLOC(1, sizeof(s7_cell));
	free_s7_cell(sc, i);
      }
  }

  /* fprintf(stderr, "cell: %d\n", sizeof(s7_cell)); */ /* 24 */
  
  /* this has to precede s7_make_* allocations */
  sc->protected_objects = s7_make_vector(sc, INITIAL_PROTECTED_OBJECTS_SIZE);
  sc->protected_objects_size = INITIAL_PROTECTED_OBJECTS_SIZE;
  set_immutable(sc->protected_objects);
  typeflag(sc->protected_objects) |= T_CONSTANT;
  sc->gc_loc = 0;
  
  sc->stack_top = 0;
  sc->stack = s7_make_vector(sc, INITIAL_STACK_SIZE);
  sc->stack_size = INITIAL_STACK_SIZE;
  
  sc->symbol_table = s7_make_vector(sc, SYMBOL_TABLE_SIZE);
  set_immutable(sc->symbol_table);
  typeflag(sc->symbol_table) |= T_CONSTANT;
  
  sc->gc_verbose = false;
  sc->tracing = false;
  
  sc->code = sc->NIL;
  sc->args = sc->NIL;
  sc->value = sc->NIL;

  new_frame_in_env(sc, sc->NIL); 
  
  sc->global_env = sc->envir; 
  set_immutable(sc->global_env);
  
  x = s7_make_symbol(sc, "else");
  new_slot_in_env(sc, x, sc->T); 
  
  sc->small_ints = s7_make_vector(sc, OP_MAX_DEFINED + 1);
  typeflag(sc->small_ints) |= T_CONSTANT;
  
  for(i = 0; i < OP_MAX_DEFINED; i++) 
    {
      s7_pointer p;
      p = (s7_cell *)CALLOC(1, sizeof(s7_cell));
      p->flag = T_OBJECT | T_IMMUTABLE | T_ATOM | T_NUMBER | T_CONSTANT;
      p->object.number.type = NUM_INT;
      integer(p->object.number) = (off_t)i;
      vector_element(sc->small_ints, i) = p;
    }
  
  /* initialization of global pointers to special symbols */
  assign_syntax(sc, "lambda",      OP_LAMBDA);
  assign_syntax(sc, "quote",       OP_QUOTE);
  assign_syntax(sc, "define",      OP_DEF0);
  assign_syntax(sc, "if",          OP_IF0);
  assign_syntax(sc, "begin",       OP_BEGIN);
  assign_syntax(sc, "set!",        OP_SET0);
  assign_syntax(sc, "let",         OP_LET0);
  assign_syntax(sc, "let*",        OP_LET0AST);
  assign_syntax(sc, "letrec",      OP_LET0REC);
  assign_syntax(sc, "cond",        OP_COND0);
  assign_syntax(sc, "make-promise",OP_DELAY);
  assign_syntax(sc, "and",         OP_AND0);
  assign_syntax(sc, "or",          OP_OR0);
  assign_syntax(sc, "cons-stream", OP_C0STREAM); /* what is this?? -- something to do with "promise"? */
  assign_syntax(sc, "macro",       OP_MACRO0);
  assign_syntax(sc, "case",        OP_CASE0);
  assign_syntax(sc, "defmacro",    OP_DEFMACRO);
  assign_syntax(sc, "define-macro",OP_DEFINE_MACRO);

  
  sc->LAMBDA = s7_make_symbol(sc, "lambda");
  typeflag(sc->LAMBDA) |= (T_IMMUTABLE | T_CONSTANT); 
  sc->QUOTE = s7_make_symbol(sc, "quote");
  typeflag(sc->QUOTE) |= (T_IMMUTABLE | T_CONSTANT); 
  sc->QUASIQUOTE = s7_make_symbol(sc, "quasiquote");
  typeflag(sc->QUASIQUOTE) |= (T_IMMUTABLE | T_CONSTANT); 
  sc->UNQUOTE = s7_make_symbol(sc, "unquote");
  typeflag(sc->UNQUOTE) |= (T_IMMUTABLE | T_CONSTANT); 
  sc->UNQUOTE_SPLICING = s7_make_symbol(sc, "unquote-splicing");
  typeflag(sc->UNQUOTE_SPLICING) |= (T_IMMUTABLE | T_CONSTANT); 
  sc->FEED_TO = s7_make_symbol(sc, "=>");
  typeflag(sc->FEED_TO) |= (T_IMMUTABLE | T_CONSTANT); 
  #define set_object_name "(generalized set!)"
  sc->SET_OBJECT = s7_make_symbol(sc, set_object_name);
  typeflag(sc->SET_OBJECT) |= T_CONSTANT; 


  /* symbols */
  s7_define_function(sc, "gensym",              g_gensym,              0, 1, false, H_gensym);
  s7_define_function(sc, "symbol-table",        g_symbol_table,        0, 0, false, H_symbol_table);
  s7_define_function(sc, "symbol?",             g_is_symbol,           1, 0, false, H_is_symbol);
  s7_define_function(sc, "symbol->string",      g_symbol_to_string,    1, 0, false, H_symbol_to_string);
  s7_define_function(sc, "string->symbol",      g_string_to_symbol,    1, 0, false, H_string_to_symbol);

  s7_define_function(sc, "global-environment",  g_global_environment,  0, 0, false, H_global_environment);
  s7_define_function(sc, "current-environment", g_current_environment, 0, 0, false, H_current_environment);
  s7_define_function(sc, "provided?",           g_is_provided,         1, 0, false, H_is_provided);
  s7_define_function(sc, "provide",             g_provide,             1, 0, false, H_provide);
  s7_define_function(sc, "defined?",            g_is_defined,          1, 1, false, H_is_defined);

  s7_define_function(sc, "keyword?",            g_is_keyword,          1, 0, false, H_is_keyword);
  s7_define_function(sc, "make-keyword",        g_make_keyword,        1, 0, false, H_make_keyword);
  s7_define_function(sc, "symbol->keyword",     g_symbol_to_keyword,   1, 0, false, H_symbol_to_keyword);
  s7_define_function(sc, "keyword->symbol",     g_keyword_to_symbol,   1, 0, false, H_keyword_to_symbol);

  s7_define_function(sc, "make-hash-table",     g_make_hash_table,     0, 1, false, H_make_hash_table);
  s7_define_function(sc, "hash-table-ref",      g_hash_table_ref,      2, 0, false, H_hash_table_ref);
  s7_define_function(sc, "hash-table-set!",     g_hash_table_set,      3, 0, false, H_hash_table_set);


  /* ports */
  s7_define_function(sc, "port-line-number",    g_port_line_number,    1, 0, false, H_port_line_number);
  s7_define_function(sc, "port-filename",       g_port_filename,       1, 0, false, H_port_filename);

  s7_define_function(sc, "input-port?",         g_is_input_port,       1, 0, false, H_is_input_port);
  s7_define_function(sc, "output-port?",        g_is_output_port,      1, 0, false, H_is_output_port);
  s7_define_function(sc, "char-ready?",         g_is_char_ready,       0, 1, false, H_is_char_ready);
  s7_define_function(sc, "eof-object?",         g_is_eof_object,       1, 0, false, H_is_eof_object);

  s7_define_function(sc, "current-input-port",  g_current_input_port,  0, 0, false, H_current_input_port);
  s7_define_function(sc, "set-current-input-port", g_set_current_input_port, 1, 0, false, H_set_current_input_port);
  s7_define_function(sc, "current-output-port", g_current_output_port, 0, 0, false, H_current_output_port);
  s7_define_function(sc, "set-current-output-port", g_set_current_output_port, 1, 0, false, H_set_current_output_port);
  s7_define_function(sc, "current-error-port",  g_current_error_port,  0, 0, false, H_current_error_port);
  s7_define_function(sc, "set-current-error-port", g_set_current_error_port, 1, 0, false, H_set_current_error_port);
  s7_define_function(sc, "close-input-port",    g_close_input_port,    1, 0, false, H_close_input_port);
  s7_define_function(sc, "close-output-port",   g_close_output_port,   1, 0, false, H_close_output_port);
  s7_define_function(sc, "open-input-file",     g_open_input_file,     1, 0, false, H_open_input_file);
  s7_define_function(sc, "open-output-file",    g_open_output_file,    1, 0, false, H_open_output_file);
  s7_define_function(sc, "open-input-string",   g_open_input_string,   1, 0, false, H_open_input_string);
  s7_define_function(sc, "open-output-string",  g_open_output_string,  0, 0, false, H_open_output_string);
  s7_define_function(sc, "get-output-string",   g_get_output_string,   1, 0, false, H_get_output_string);

  s7_define_function(sc, "read-char",           g_read_char,           0, 1, false, H_read_char);
  s7_define_function(sc, "peek-char",           g_peek_char,           0, 1, false, H_peek_char);
  s7_define_function(sc, "read",                g_read,                0, 1, false, H_read);
  s7_define_function(sc, "newline",             g_newline,             0, 1, false, H_newline);
  s7_define_function(sc, "write-char",          g_write_char,          1, 1, false, H_write_char);
  s7_define_function(sc, "write",               g_write,               1, 1, false, H_write);
  s7_define_function(sc, "display",             g_display,             1, 1, false, H_display);
  s7_define_function(sc, "read-byte",           g_read_byte,           0, 1, false, H_read_byte);
  s7_define_function(sc, "write-byte",          g_write_byte,          1, 1, false, H_write_byte);

  /* call-with-output|input-file with-output-to-file with-input-from-file */
  s7_define_function(sc, "call-with-input-string", g_call_with_input_string, 2, 0, false, H_call_with_input_string);


  /* numbers */
  s7_define_function(sc, "number->string",      g_number_to_string,    1, 2, false, H_number_to_string);
  s7_define_function(sc, "string->number",      g_string_to_number,    1, 2, false, H_string_to_number);
  s7_define_function(sc, "make-polar",          g_make_polar,          2, 0, false, H_make_polar);
  s7_define_function(sc, "make-rectangular",    g_make_rectangular,    2, 0, false, H_make_rectangular);
  s7_define_function(sc, "magnitude",           g_magnitude,           1, 0, false, H_magnitude);
  s7_define_function(sc, "angle",               g_angle,               1, 0, false, H_angle);
  s7_define_function(sc, "rationalize",         g_rationalize,         2, 0, false, H_rationalize);
  s7_define_function(sc, "abs",                 g_abs,                 1, 0, false, H_abs);
  s7_define_function(sc, "exp",                 g_exp,                 1, 0, false, H_exp);
  s7_define_function(sc, "log",                 g_log,                 1, 1, false, H_log);
  s7_define_function(sc, "sin",                 g_sin,                 1, 0, false, H_sin);
  s7_define_function(sc, "cos",                 g_cos,                 1, 0, false, H_cos);
  s7_define_function(sc, "tan",                 g_tan,                 1, 0, false, H_tan);
  s7_define_function(sc, "asin",                g_asin,                1, 0, false, H_asin);
  s7_define_function(sc, "acos",                g_acos,                1, 0, false, H_acos);
  s7_define_function(sc, "atan",                g_atan,                1, 1, false, H_atan);
  s7_define_function(sc, "sinh",                g_sinh,                1, 0, false, H_sinh);
  s7_define_function(sc, "cosh",                g_cosh,                1, 0, false, H_cosh);
  s7_define_function(sc, "tanh",                g_tanh,                1, 0, false, H_tanh);
  s7_define_function(sc, "asinh",               g_asinh,               1, 0, false, H_asinh);
  s7_define_function(sc, "acosh",               g_acosh,               1, 0, false, H_acosh);
  s7_define_function(sc, "atanh",               g_atanh,               1, 0, false, H_atanh);
  s7_define_function(sc, "sqrt",                g_sqrt,                1, 0, false, H_sqrt);
  s7_define_function(sc, "expt",                g_expt,                2, 0, false, H_expt);
  s7_define_function(sc, "floor",               g_floor,               1, 0, false, H_floor);
  s7_define_function(sc, "ceiling",             g_ceiling,             1, 0, false, H_ceiling);
  s7_define_function(sc, "truncate",            g_truncate,            1, 0, false, H_truncate);
  s7_define_function(sc, "round",               g_round,               1, 0, false, H_round);
  s7_define_function(sc, "lcm",                 g_lcm,                 0, 0, true,  H_lcm);
  s7_define_function(sc, "gcd",                 g_gcd,                 0, 0, true,  H_gcd);
  s7_define_function(sc, "+",                   g_add,                 0, 0, true,  H_add);
  s7_define_function(sc, "-",                   g_subtract,            1, 0, true,  H_subtract);
  s7_define_function(sc, "*",                   g_multiply,            0, 0, true,  H_multiply);
  s7_define_function(sc, "/",                   g_divide,              1, 0, true,  H_divide);
  s7_define_function(sc, "max",                 g_max,                 1, 0, true,  H_max);
  s7_define_function(sc, "min",                 g_min,                 1, 0, true,  H_min);
  s7_define_function(sc, "quotient",            g_quotient,            2, 0, false, H_quotient);
  s7_define_function(sc, "remainder",           g_remainder,           2, 0, false, H_remainder);
  s7_define_function(sc, "modulo",              g_modulo,              2, 0, false, H_modulo);
  s7_define_function(sc, "=",                   g_equal,               2, 0, true,  H_equal);
  s7_define_function(sc, "<",                   g_less,                2, 0, true,  H_less);
  s7_define_function(sc, ">",                   g_greater,             2, 0, true,  H_greater);
  s7_define_function(sc, "<=",                  g_less_or_equal,       2, 0, true,  H_less_or_equal);
  s7_define_function(sc, ">=",                  g_greater_or_equal,    2, 0, true,  H_greater_or_equal);
  s7_define_function(sc, "number?",             g_is_number,           1, 0, false, H_is_number);
  s7_define_function(sc, "integer?",            g_is_integer,          1, 0, false, H_is_integer);
  s7_define_function(sc, "real?",               g_is_real,             1, 0, false, H_is_real);
  s7_define_function(sc, "complex?",            g_is_complex,          1, 0, false, H_is_complex);
  s7_define_function(sc, "rational?",           g_is_rational,         1, 0, false, H_is_rational);
  s7_define_function(sc, "even?",               g_is_even,             1, 0, false, H_is_even);
  s7_define_function(sc, "odd?",                g_is_odd,              1, 0, false, H_is_odd);
  s7_define_function(sc, "zero?",               g_is_zero,             1, 0, false, H_is_zero);
  s7_define_function(sc, "positive?",           g_is_positive,         1, 0, false, H_is_positive);
  s7_define_function(sc, "negative?",           g_is_negative,         1, 0, false, H_is_negative);
  s7_define_function(sc, "real-part",           g_real_part,           1, 0, false, H_real_part);
  s7_define_function(sc, "imag-part",           g_imag_part,           1, 0, false, H_imag_part);
  s7_define_function(sc, "numerator",           g_numerator,           1, 0, false, H_numerator);
  s7_define_function(sc, "denominator",         g_denominator,         1, 0, false, H_denominator);
  s7_define_function(sc, "inexact->exact",      g_inexact_to_exact,    1, 0, false, H_inexact_to_exact);
  s7_define_function(sc, "exact->inexact",      g_exact_to_inexact,    1, 0, false, H_exact_to_inexact);
  s7_define_function(sc, "exact?",              g_is_exact,            1, 0, false, H_is_exact);
  s7_define_function(sc, "inexact?",            g_is_inexact,          1, 0, false, H_is_inexact);
  s7_define_function(sc, "logior",              g_logior,              1, 0, true,  H_logior);
  s7_define_function(sc, "logxor",              g_logxor,              1, 0, true,  H_logxor);
  s7_define_function(sc, "logand",              g_logand,              1, 0, true,  H_logand);
  s7_define_function(sc, "lognot",              g_lognot,              1, 0, false, H_lognot);

  
  /* chars */
  s7_define_function(sc, "char-upcase",         g_char_upcase,         1, 0, false, H_char_upcase);
  s7_define_function(sc, "char-downcase",       g_char_downcase,       1, 0, false, H_char_downcase);
  s7_define_function(sc, "char->integer",       g_char_to_integer,     1, 0, false, H_char_to_integer);
  s7_define_function(sc, "integer->char",       g_integer_to_char,     1, 0, false, H_integer_to_char);

  s7_define_function(sc, "char-upper-case?",    g_is_char_upper_case,  1, 0, false, H_is_char_upper_case);
  s7_define_function(sc, "char-lower-case?",    g_is_char_lower_case,  1, 0, false, H_is_char_lower_case);
  s7_define_function(sc, "char-alphabetic?",    g_is_char_alphabetic,  1, 0, false, H_is_char_alphabetic);
  s7_define_function(sc, "char-numeric?",       g_is_char_numeric,     1, 0, false, H_is_char_numeric);
  s7_define_function(sc, "char-whitespace?",    g_is_char_whitespace,  1, 0, false, H_is_char_whitespace);
  s7_define_function(sc, "char?",               g_is_char,             1, 0, false, H_is_char);

  s7_define_function(sc, "char=?",              g_chars_are_equal,     1, 0, true,  H_chars_are_equal);
  s7_define_function(sc, "char<?",              g_chars_are_less,      1, 0, true,  H_chars_are_less);
  s7_define_function(sc, "char>?",              g_chars_are_greater,   1, 0, true,  H_chars_are_greater);
  s7_define_function(sc, "char<=?",             g_chars_are_leq,       1, 0, true,  H_chars_are_leq);
  s7_define_function(sc, "char>=?",             g_chars_are_geq,       1, 0, true,  H_chars_are_geq);
  s7_define_function(sc, "char-ci=?",           g_chars_are_ci_equal,  1, 0, true,  H_chars_are_ci_equal);
  s7_define_function(sc, "char-ci<?",           g_chars_are_ci_less,   1, 0, true,  H_chars_are_ci_less);
  s7_define_function(sc, "char-ci>?",           g_chars_are_ci_greater, 1, 0, true, H_chars_are_ci_greater);
  s7_define_function(sc, "char-ci<=?",          g_chars_are_ci_leq,    1, 0, true,  H_chars_are_ci_leq);
  s7_define_function(sc, "char-ci>=?",          g_chars_are_ci_geq,    1, 0, true,  H_chars_are_ci_geq);


  /* strings */
  s7_define_function(sc, "string?",             g_is_string,           1, 0, false, H_is_string);
  s7_define_function(sc, "make-string",         g_make_string,         1, 1, false, H_make_string);
  s7_define_function(sc, "string-length",       g_string_length,       1, 0, false, H_string_length);
  s7_define_function(sc, "string-ref",          g_string_ref,          2, 0, false, H_string_ref);
  s7_define_function(sc, "string-set!",         g_string_set,          3, 0, false, H_string_set);

  s7_define_function(sc, "string=?",            g_strings_are_equal,   1, 0, true,  H_strings_are_equal);
  s7_define_function(sc, "string<?",            g_strings_are_less,    1, 0, true,  H_strings_are_less);
  s7_define_function(sc, "string>?",            g_strings_are_greater, 1, 0, true,  H_strings_are_greater);
  s7_define_function(sc, "string<=?",           g_strings_are_leq,     1, 0, true,  H_strings_are_leq);
  s7_define_function(sc, "string>=?",           g_strings_are_geq,     1, 0, true,  H_strings_are_geq);
  s7_define_function(sc, "string-ci=?",         g_strings_are_ci_equal, 1, 0, true, H_strings_are_ci_equal);
  s7_define_function(sc, "string-ci<?",         g_strings_are_ci_less, 1, 0, true,  H_strings_are_ci_less);
  s7_define_function(sc, "string-ci>?",         g_strings_are_ci_greater, 1, 0, true,  H_strings_are_ci_greater);
  s7_define_function(sc, "string-ci<=?",        g_strings_are_ci_leq,  1, 0, true,  H_strings_are_ci_leq);
  s7_define_function(sc, "string-ci>=?",        g_strings_are_ci_geq,  1, 0, true,  H_strings_are_ci_geq);

  s7_define_function(sc, "string-append",       g_string_append,       0, 0, true,  H_string_append);
  s7_define_function(sc, "string-fill!",        g_string_fill,         2, 0, false, H_string_fill);
  s7_define_function(sc, "string-copy",         g_string_copy,         1, 0, false, H_string_copy);
  s7_define_function(sc, "substring",           g_substring,           2, 1, false, H_substring);
  s7_define_function(sc, "string",              g_string,              0, 0, true,  H_string);
  s7_define_function(sc, "list->string",        g_list_to_string,      1, 0, false, H_list_to_string);
  s7_define_function(sc, "string->list",        g_string_to_list,      1, 0, false, H_string_to_list);
  s7_define_function(sc, "object->string",      g_object_to_string,    1, 0, false, H_object_to_string);


  /* lists */
  s7_define_function(sc, "null?",               g_is_null,             1, 0, false, H_is_null);
  s7_define_function(sc, "list?",               g_is_list,             1, 0, false, H_is_list);
  s7_define_function(sc, "pair?",               g_is_pair,             1, 0, false, H_is_pair);
  s7_define_function(sc, "reverse",             g_reverse,             1, 0, false, H_reverse);
  s7_define_function(sc, "reverse!",            g_reverse_in_place,    1, 0, false, H_reverse_in_place); /* used by Snd code */
  s7_define_function(sc, "cons",                g_cons,                2, 0, false, H_cons);
  s7_define_function(sc, "car",                 g_car,                 1, 0, false, H_car);
  s7_define_function(sc, "cdr",                 g_cdr,                 1, 0, false, H_cdr);
  s7_define_function(sc, "set-car!",            g_set_car,             2, 0, false, H_set_car);
  s7_define_function(sc, "set-cdr!",            g_set_cdr,             2, 0, false, H_set_cdr);
  s7_define_function(sc, "caar",                g_caar,                1, 0, false, H_caar);
  s7_define_function(sc, "cadr",                g_cadr,                1, 0, false, H_cadr);
  s7_define_function(sc, "cdar",                g_cdar,                1, 0, false, H_cdar);
  s7_define_function(sc, "cddr",                g_cddr,                1, 0, false, H_cddr);
  s7_define_function(sc, "caaar",               g_caaar,               1, 0, false, H_caaar);
  s7_define_function(sc, "caadr",               g_caadr,               1, 0, false, H_caadr);
  s7_define_function(sc, "cadar",               g_cadar,               1, 0, false, H_cadar);
  s7_define_function(sc, "cdaar",               g_cdaar,               1, 0, false, H_cdaar);
  s7_define_function(sc, "caddr",               g_caddr,               1, 0, false, H_caddr);
  s7_define_function(sc, "cdddr",               g_cdddr,               1, 0, false, H_cdddr);
  s7_define_function(sc, "cdadr",               g_cdadr,               1, 0, false, H_cdadr);
  s7_define_function(sc, "cddar",               g_cddar,               1, 0, false, H_cddar);
  s7_define_function(sc, "length",              g_length,              1, 0, false, H_length);
  s7_define_function(sc, "assq",                g_assq,                2, 0, false, H_assq);
  s7_define_function(sc, "assv",                g_assv,                2, 0, false, H_assv);
  s7_define_function(sc, "assoc",               g_assoc,               2, 0, false, H_assoc);
  s7_define_function(sc, "memq",                g_memq,                2, 0, false, H_memq);
  s7_define_function(sc, "memv",                g_memv,                2, 0, false, H_memv);
  s7_define_function(sc, "member",              g_member,              2, 0, false, H_member);
  s7_define_function(sc, "append",              g_append,              0, 0, true,  H_append);
  s7_define_function(sc, "list",                g_list,                0, 0, true,  H_list);
  s7_define_function(sc, "list-ref",            g_list_ref,            2, 0, false, H_list_ref);
  s7_define_function(sc, "list-set!",           g_list_set,            3, 0, false, H_list_set);
  s7_define_function(sc, "list-tail",           g_list_tail,           2, 0, false, H_list_tail);
  s7_define_function(sc, "list-line-number",    g_list_line_number,    1, 0, false, H_list_line_number);


  /* vectors */
  s7_define_function(sc, "vector?",             g_is_vector,           1, 0, false, H_is_vector);
  s7_define_function(sc, "vector->list",        g_vector_to_list,      1, 0, false, H_vector_to_list);
  s7_define_function(sc, "list->vector",        g_list_to_vector,      1, 0, false, H_list_to_vector);
  s7_define_function(sc, "vector-fill!",        g_vector_fill,         2, 0, false, H_vector_fill);
  s7_define_function(sc, "vector",              g_vector,              0, 0, true,  H_vector);
  s7_define_function(sc, "vector-length",       g_vector_length,       1, 0, false, H_vector_length);
  s7_define_function(sc, "vector-ref",          g_vector_ref,          2, 0, false, H_vector_ref);
  s7_define_function(sc, "vector-set!",         g_vector_set,          3, 0, false, H_vector_set);
  s7_define_function(sc, "make-vector",         g_make_vector,         1, 1, false, H_make_vector);



  s7_define_function(sc, "call/cc",             g_call_cc,             1, 0, false, H_call_cc);
  s7_define_function(sc, "call-with-current-continuation", g_call_cc,  1, 0, false, H_call_cc);
  s7_define_function(sc, "call-with-exit",      g_call_with_exit,      1, 0, false, H_call_with_exit);
  s7_define_function(sc, "load",                g_load,                1, 0, false, H_display);
  s7_define_function(sc, "eval",                g_eval,                1, 1, false, H_eval);
  s7_define_function(sc, "apply",               g_apply,               1, 0, true,  H_apply);
  
  s7_define_function(sc, "tracing",             g_tracing,             1, 0, false, H_tracing);
  s7_define_function(sc, "gc-verbose",          g_gc_verbose,          1, 0, false, H_gc_verbose);
  s7_define_function(sc, "stacktrace",          g_stacktrace,          0, 0, false, H_stacktrace);
  s7_define_function(sc, "gc",                  g_gc,                  0, 0, false, H_gc);
  s7_define_function(sc, "quit",                g_quit,                0, 0, false, H_quit);
  s7_define_function(sc, "catch",               g_catch,               3, 0, false, H_catch);
  s7_define_function(sc, "error",               g_error,               0, 0, true,  H_error);
  s7_define_function(sc, "force",               g_force,               1, 0, false, H_force);
  s7_define_function(sc, "make-closure",        g_make_closure,        1, 1, false, H_make_closure);
  s7_define_function(sc, "closure?",            g_is_closure,          1, 0, false, H_is_closure);
  
  s7_define_function(sc, "procedure?",          g_is_procedure,        1, 0, false, H_is_procedure);
  s7_define_function(sc, "procedure-documentation", g_procedure_documentation, 1, 0, false, H_procedure_documentation);
  s7_define_function(sc, "help",                g_procedure_documentation, 1, 0, false, H_procedure_documentation);
  s7_define_function(sc, "procedure-arity",     s7_procedure_arity,     1, 0, false, H_procedure_arity);
  s7_define_function(sc, "procedure-source",    g_procedure_source,     1, 0, false, H_procedure_source);
  

  s7_define_function(sc, "not",                 g_not,                 1, 0, false, H_not);
  s7_define_function(sc, "boolean?",            g_is_boolean,          1, 0, false, H_is_boolean);
  s7_define_function(sc, "eq?",                 g_is_eq,               2, 0, false, H_is_eq);
  s7_define_function(sc, "eqv?",                g_is_eqv,              2, 0, false, H_is_eqv);
  s7_define_function(sc, "equal?",              g_is_equal,            2, 0, false, H_is_equal);

  s7_define_function(sc, "scheme-implementation", g_scheme_implementation, 0, 0, false, H_scheme_implementation);
  s7_define_function(sc, set_object_name,       g_set_object,          1, 0, true, "internal setter redirection");
  s7_define_function(sc, "make-procedure-with-setter", g_make_procedure_with_setter, 2, 0, false, "...");
  s7_define_function(sc, "procedure-with-setter?", g_is_procedure_with_setter, 1, 0, false, H_is_procedure_with_setter);

  pws_tag = s7_new_type("<procedure-with-setter>",
			pws_print,
			pws_free,
			pws_equal,
			pws_mark,
			pws_apply,
			pws_set);

  s7_define_variable(sc, "*features*", sc->NIL);
  s7_define_variable(sc, "*load-path*", sc->NIL);
  s7_define_variable(sc, "*vector-print-length*", vector_element(sc->small_ints, 8));
  
  sc->gc_off = false;

  /* leftovers from s7.scm -- this stuff will mostly go away! */

  s7_eval_c_string(sc, "\n\
(define (caaaar x) (car (caaar x)))\n\
(define (caaadr x) (car (caadr x)))\n\
(define (caadar x) (car (cadar x)))\n\
(define (caaddr x) (car (caddr x)))\n\
(define (cadaar x) (car (cdaar x)))\n\
(define (cadadr x) (car (cdadr x)))\n\
(define (caddar x) (car (cddar x)))\n\
(define (cadddr x) (car (cdddr x)))\n\
(define (cdaaar x) (cdr (caaar x)))\n\
(define (cdaadr x) (cdr (caadr x)))\n\
(define (cdadar x) (cdr (cadar x)))\n\
(define (cdaddr x) (cdr (caddr x)))\n\
(define (cddaar x) (cdr (cdaar x)))\n\
(define (cddadr x) (cdr (cdadr x)))\n\
(define (cdddar x) (cdr (cddar x)))\n\
(define (cddddr x) (cdr (cdddr x)))\n\
\n\
(define (unzip1-with-cdr . lists)\n\
  (unzip1-with-cdr-iterative lists '() '()))\n\
\n\
(define (unzip1-with-cdr-iterative lists cars cdrs)\n\
  (if (null? lists)\n\
      (cons cars cdrs)\n\
      (let ((car1 (caar lists))\n\
	    (cdr1 (cdar lists)))\n\
	(unzip1-with-cdr-iterative \n\
	 (cdr lists) \n\
	 (append cars (list car1))\n\
	 (append cdrs (list cdr1))))))\n\
\n\
(define (map proc . lists)\n\
  (if (null? lists)\n\
      (apply proc)\n\
      (if (null? (car lists))\n\
	  '()\n\
	  (let* ((unz (apply unzip1-with-cdr lists))\n\
		 (cars (car unz))\n\
		 (cdrs (cdr unz)))\n\
	    (cons (apply proc cars) (apply map (cons proc cdrs)))))))\n\
\n\
\n\
(define (for-each proc . lists)\n\
  (if (null? lists)\n\
      (apply proc)\n\
      (if (null? (car lists))\n\
	  #t\n\
	  (let* ((unz (apply unzip1-with-cdr lists))\n\
		 (cars (car unz))\n\
		 (cdrs (cdr unz)))\n\
	    (apply proc cars) (apply map (cons proc cdrs))))))\n\
\n\
;; The following quasiquote macro is due to Eric S. Tiedemann.\n\
;;   Copyright 1988 by Eric S. Tiedemann; all rights reserved.\n\
;;\n\
;; Subsequently modified to handle vectors: D. Souflis\n\
\n\
(macro\n\
    quasiquote\n\
  (lambda (l)\n\
\n\
    (define (mcons f l r)\n\
      (if (and (pair? r)\n\
	       (eq? (car r) 'quote)\n\
	       (eq? (car (cdr r)) (cdr f))\n\
	       (pair? l)\n\
	       (eq? (car l) 'quote)\n\
	       (eq? (car (cdr l)) (car f)))\n\
	  (if (or (procedure? f) (number? f) (string? f))\n\
	      f\n\
	      (list 'quote f))\n\
	  (if (eqv? l vector)\n\
	      (apply l (eval r))\n\
	      (list 'cons l r)\n\
	      )))\n\
\n\
    (define (mappend f l r)\n\
      (if (or (null? (cdr f))\n\
	      (and (pair? r)\n\
		   (eq? (car r) 'quote)\n\
		   (eq? (car (cdr r)) '())))\n\
	  l\n\
	  (list 'append l r)))\n\
\n\
    (define (foo level form)\n\
      (cond ((not (pair? form))\n\
	     (if (or (procedure? form) (number? form) (string? form))\n\
		 form\n\
		 (list 'quote form))\n\
	     )\n\
	    ((eq? 'quasiquote (car form))\n\
	     (mcons form ''quasiquote (foo (+ level 1) (cdr form))))\n\
\n\
	    (#t (if (zero? level)\n\
		    (cond ((eq? (car form) 'unquote)\n\
			   (car (cdr form)))\n\
\n\
			  ((eq? (car form) 'unquote-splicing)\n\
			   form)\n\
\n\
			  ((and (pair? (car form))\n\
				(eq? (car (car form)) 'unquote-splicing))\n\
\n\
			   (mappend form \n\
				    (car (cdr (car form)))\n\
				    (foo level (cdr form))))\n\
\n\
			  (#t (mcons form (foo level (car form))\n\
				     (foo level (cdr form)))))\n\
\n\
		    (cond ((eq? (car form) 'unquote)\n\
			   (mcons form ''unquote (foo (- level 1)\n\
						      (cdr form))))\n\
			  ((eq? (car form) 'unquote-splicing)\n\
			   (mcons form ''unquote-splicing\n\
				  (foo (- level 1) (cdr form))))\n\
			  (#t (mcons form (foo level (car form))\n\
				     (foo level (cdr form)))))))))\n\
    (foo 0 (car (cdr l)))))\n\
\n\
;;;; (do ((var init inc) ...) (endtest result ...) body ...)\n\
;;\n\
(macro do\n\
  (lambda (do-macro)\n\
    (apply (lambda (do vars endtest . body)\n\
             (let ((do-loop (gensym)))\n\
               `(letrec ((,do-loop\n\
			  (lambda ,(map (lambda (x)\n\
					  (if (pair? x) (car x) x))\n\
					`,vars)\n\
			    (if ,(car endtest)\n\
				(begin ,@(cdr endtest))\n\
				(begin\n\
				  ,@body\n\
				  (,do-loop\n\
                                   ,@(map (lambda (x)\n\
                                            (cond\n\
					     ((not (pair? x)) x)\n\
					     ((< (length x) 3) (car x))\n\
					     (else (car (cdr (cdr x))))))\n\
					  `,vars)))))))\n\
                  (,do-loop\n\
		   ,@(map (lambda (x)\n\
			    (if (and (pair? x) (cdr x))\n\
				(car (cdr x))\n\
				'()))\n\
			  `,vars)))))\n\
	   do-macro)))\n\
\n\
(define (call-with-input-file filename func)\n\
  (let ((inport (open-input-file filename)))\n\
    (and (input-port? inport)\n\
	 (let ((res (func inport)))\n\
	   (close-input-port inport)\n\
	   res))))\n\
\n\
(define (call-with-output-file filename func)\n\
  (let ((outport (open-output-file filename)))\n\
    (and (output-port? outport)\n\
	 (let ((res (func outport)))\n\
	   (close-output-port outport)\n\
	   res))))\n\
\n\
(define (with-input-from-file s p)\n\
  (let ((inport (open-input-file s)))\n\
    (and inport\n\
	(let ((prev-inport (current-input-port)))\n\
	  (set-current-input-port inport)\n\
	  (let ((res (p)))\n\
	    (close-input-port inport)\n\
	    (set-current-input-port prev-inport)\n\
	    res)))))\n\
\n\
(define (with-output-to-file s p)\n\
  (let ((outport (open-output-file s)))\n\
    (if (eq? outport #f)\n\
	#f\n\
	(let ((prev-outport (current-output-port)))\n\
	  (set-current-output-port outport)\n\
	  (let ((res (p)))\n\
	    (close-output-port outport)\n\
	    (set-current-output-port prev-outport)\n\
	    res)))))\n\
\n\
(define (call-with-output-string t)\n\
  (let* ((p (open-output-string))\n\
	 (r (t p))\n\
	 (s (get-output-string p)))\n\
    (close-output-port p)\n\
    s))\n\
\n\
(define (with-output-to-string thunk)\n\
  (let* ((output-port (current-output-port))\n\
	 (string-port (open-output-string)))\n\
    (set-current-output-port string-port)\n\
    (thunk)\n\
    (let ((result (get-output-string string-port)))\n\
      (close-output-port string-port)\n\
      (set-current-output-port output-port)\n\
      result)))\n\
\n\
(define (dynamic-wind in body out)\n\
  (in)\n\
  (catch #t\n\
	 (lambda ()\n\
	   (body)\n\
	   (out))\n\
	 (lambda args\n\
	   (out)\n\
	   (apply error args))))\n\
\n");

  return(sc);
}


