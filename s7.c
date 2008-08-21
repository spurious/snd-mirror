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
 *
 */

/* changed to fit into Snd, Bill Schottstaedt, n-Aug-08
 *
 *     made several procedures global (eqv, list_length, append, reverse, reverse_in_place, atom_to_c_string and others)
 *     scheme_call returns sc->value
 *     merged scheme-private.h into scheme.h and changed names to s7.[ch], init.scm -> s7.scm, opdefines.h -> s7-ops.h
 *     defined *load-path*, defmacro, a stub for make-procedure-with-setter (s7.scm)
 *     changed final initializer at dispatch_table preload to make C happier
 *     added T_OBJECT (and renamed old T_FOREIGN to T_FUNCTION, is_foreign to is_function)
 *       and various code to support it (is_object, _fobj struct in s7.h etc, make_object)
 *       added OP_EQUAL and is_equal for equal? (needs to be in C, not s7.scm because foreign objects have private equality tests)
 *     sc->UNDEFINED for unset (optional) args
 *     no inexact ints, so print 440.0 as 440.0 (not 440)
 *     added OP_SET1 code for generalized set!
 *     added atan asin acos atanh asinh acosh, also log10  in s7.scm
 *     moved lcm and gcd from s7.scm to c to handle argnum!= 2 cases
 *     added case for expt of ints with 2nd arg >= 0 or 1st arg +/-1 (return an int)
 *     (eqv? 0 0.0) should return #f I believe
 *       so leaving aside a few quibbles, this passes most of Jaffer's r4rstest.scm tests
 *     decided (sigh) to clean up the names throughout -- exported functions start with "s7_", "mk"->"make", etc
 *       also, I've tried to hide all struct details in this file -- only s7.h stuff is exported.
 *     added doc field for foreign funcs
 *     floor/ceiling/truncate/round return ints! (This is not RnRS)
 *     guile-style catch/throw in s7.scm
 *     added get-output-string for open-string-output (useless otherwise)
 *     open-output-string takes 0 args, reallocs internal buffer as needed
 *     started reformatting the text so I don't have to squint all the time
 *     added complex and ratio numbers
 *        make-polar make-rectangular real-part imag-part magnitude angle
 *        rationalize numerator denominator
 *        ints are off_t's in this case
 *        had to move all the numeric stuff in s7.scm into s7-ops.h and s7.c
 *     split the hidden ivalue uses off into separate structs
 *     changed the layout of vectors (found the GC bug related to it: (make-vector 0) in r4rstest.scm was deadly to preceding version)
 *     removed the old memory allocation junk -- the current GC is about as simple as it can be
 *        with this change, all the old segfaults appear to be fixed
 *     s7_for_each_symbol_name for tab-completions (in the Snd listener).
 *     stack is now a vector, not a list.
 *     fully functional continuations (I hope...) and a goto replacement: call-with-exit
 *       call-with-exit is a goto with a value, with no way to return via a continuation
 *       the continuation copies the stack contents both ways (slow if deeply nested)
 *     removed *error-hook*, *colon-hook*, *sharp-hook*, error, atom->string and string->atom (use object<-> or number<->)
 *     tracing and gc-verbose are now "foreign functions", not built-in operators
 *     removed environment?, changed interaction-environment to global-environment
 *     changed get-closure-code|env to procedure-source|environment
 *     added #<unspecified>
 *     added block comments in #| |# and fixed block comments in #! !#
 *     all internal errors go through the check for a user-defined error func, and give filename and line-number
 *     added defmacro
 *
 * there's no arg number mismatch check for caller-defined functions!
 * dies if undefined func? (asb 1) as typo
 *
 * need dynamic-wind tested, 
 *      define* et al (gauche-optargs loads but doesn't work yet).
 *      applicable types
 *      procedure arity -- is this in the "env"?
 *      s7_write so strings are quoted in object->string
 *      soft port for Snd (need to trap error reports somehow)
 *      doc strings for scheme funcs
 *      <CLOSURE> should give the name [s7_define could a backpointer to the symbol in the value,
 *          or perhaps search for a match -- the symbol_table is not that big -- closure code is car(p)]
 *      list or list* for xen connection
 *      (keyword? :asd) segfaults! -- need the names make-keyword, keyword? 
 *      need much better error handling and backtrace
 *      why is there a symbol table and a global environment?
 *      r4rstest.scm turns up some bugs in number reading
 *      threads: call-with-new-thread[s7?], join-thread
 *      move rest of s7.scm into init funcs, and maybe more of s7-ops (see g_cadr)
 *
 * [number->string has no "radix" arg]
 * [log in r6rs has a 2nd arg, probably base]
 * [need to get rid of all 1+'s in my scheme code]
 *
 * slib logical.scm has logior etc
 * slib dynwind.scm has dynamic-wind
 * guile's optargs.scm has define*
 *
 * TODO: remove built-in even? odd? positive? negative? zero?
 * TODO: exact? -> rational? and inexact -> not something
 * TODO: rename the ops?
 * TODO: define modulo to handle ratios and reals
 *
 *  test-cont in r4rs hangs?
 *
 * figure out how to replace macro with defmacro, add syntax-rules
 * (set! (x-bounds) (list 0.0 1.0)) gets #<unspecified> for all args
 *
 * error line nums not sent out for 1st file?  but are ok after that?
 *   stack trace is doable -- maybe a switch if saving line # on stack?
 *
 * TODO: the s7.scm versions of the char funcs need to take any number of args
 * TODO: the main loop should be able to check arg arity troubles for all funcs

 * s7_error after cleanup call eval(sc, OP_ERROR) to jump out -- this provides a throw as well?
 * catch = put op_catch + tag + handler on stack
 * throw = go up stack looking for matching catch, if found call its handler, else goto error
 * same for unwind-protect -- and throw could notice them and run their exit funcs
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
 *    so almost any number is ok -- I've run with 500.
 */

#define SYMBOL_TABLE_SIZE 9601
/* names are hashed into the symbol table (a vector) and collisions are chained as lists, was 461, then 4603.
 */

#define INITIAL_STACK_SIZE 1000            /* each frame takes 4 entries */

#define INITIAL_PROTECTED_OBJECTS_SIZE 16  /* a vector of objects that are being protected from the GC */


/* operator code */
typedef enum { 
#define _OP_DEF(B, C, D, E, OP) OP,
#include "s7-ops.h" 
  OP_MAXDEFINED 
} opcode_t;


static char *opcode_names[] = {
#define _OP_DEF(B, C, D, E, OP) #OP,
#include "s7-ops.h"
};


static char error_buf[1024];

#include <complex.h>


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



typedef struct port {
  bool is_closed;
  bool is_file;
  bool needs_close;
  int paren_depth;
  union {
    struct {
      FILE *file;
      int line_number;
      const char *filename;
    } stdio;
    struct {
      char *start;
      char *past_the_end;
      char *curr;
    } string;
  } rep;
} port;


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


/* cell structure */
typedef struct s7_cell {
  unsigned int flag;
  union {
    
    struct {
      char *svalue;
      int  length;
    } string;
    
    num number;
    
    port *port;

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
    } cons;
    
    struct {
      int type;
      void *value;
    } fobj;

    continuation *cc;

    int goto_loc;
    
  } object;
} s7_cell;


struct s7_scheme {
  bool tracing;
  bool gc_off;
  
  int *free_heap;
  int free_heap_top, free_heap_size;

  s7_cell **heap;
  int heap_size;
  
  /* We use 4 registers. */
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
  
  struct s7_cell _ERROR;
  s7_pointer ERROR;           /* error indication */
  
  s7_pointer symbol_table;    /* symbol table */
  s7_pointer global_env;      /* global environment */
  
  /* global s7_pointers to special symbols */
  s7_pointer LAMBDA;          /* syntax lambda */
  s7_pointer QUOTE;           /* syntax quote */
  
  s7_pointer QQUOTE;          /* symbol quasiquote */
  s7_pointer UNQUOTE;         /* symbol unquote */
  s7_pointer UNQUOTESP;       /* symbol unquote-splicing */
  s7_pointer FEED_TO;         /* => */
  
  s7_pointer inport;          /* current-input-port (nil = stdin) */
  s7_pointer inport_stack;    /*   input port stack (load and read internally) */
  s7_pointer outport;         /* current-output-port (nil = stderr) */

  bool gc_verbose;            /* if gc_verbose is true, print gc status */
  
  #define LINESIZE 1024
  char linebuff[LINESIZE];
  char strbuf[256];
  
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

  void (*error_exiter)(void);
};


enum scheme_types {
  T_NIL_TYPE = 1,
  T_STRING = 2,
  T_NUMBER = 3,
  T_SYMBOL = 4,
  T_PROC = 5,
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
  T_LAST_TYPE = 17
};


static const char *type_names[T_LAST_TYPE + 1] = {
  "unused!", "nil", "string", "number", "symbol", "procedure", "pair", "closure", "continuation",
  "s7-function", "character", "input port", "vector", "macro", "promise", "s7-object", 
  "goto", "output port"
};



#define TYPE_BITS          16
#define T_MASKTYPE         0xffff

#define typeflag(p)        ((p)->flag)
#define type(p)            (typeflag(p) & T_MASKTYPE)
#define set_type_1(p, f)   typeflag(p) = ((f) | T_OBJECT)

#define T_SYNTAX           (1 << (TYPE_BITS + 1))
#define T_IMMUTABLE        (1 << (TYPE_BITS + 2))

#define is_immutable(p)    (typeflag(p) & T_IMMUTABLE)
#define set_immutable(p)   typeflag(p) |= T_IMMUTABLE
#define set_mutable(p)     typeflag(p) &= (~T_IMMUTABLE)

#define T_ATOM             (1 << (TYPE_BITS + 3))

#define is_atom(p)         (typeflag(p) & T_ATOM)
#define set_atom(p)        typeflag(p) |= T_ATOM

#define T_GC_MARK          (1 << (TYPE_BITS + 4))

#define is_marked(p)       (typeflag(p) &  T_GC_MARK)
#define set_mark(p)        typeflag(p)  |= T_GC_MARK
#define clear_mark(p)      typeflag(p)  &= (~T_GC_MARK)

#define T_CONSTANT         (1 << (TYPE_BITS + 5))
#define is_constant(p)     (typeflag(p) & T_CONSTANT)
#define local_protect(p)   typeflag(p) |= T_CONSTANT
#define local_unprotect(p) typeflag(p) &= (~T_CONSTANT)

#define T_UNUSED_BITS      0xff000000
#define T_OBJECT           (1 << (TYPE_BITS + 6))
#define T_FINALIZABLE      (1 << (TYPE_BITS + 7))

#define is_object(x)       (((typeflag(x) & (T_UNUSED_BITS | T_OBJECT)) == T_OBJECT) && (type(x) != 0) && (type(x) <= T_LAST_TYPE))

#define function_call(f)            (f)->object.ffptr->ff
#define function_name(f)            (f)->object.ffptr->name
#define function_documentation(f)   (f)->object.ffptr->doc
#define function_required_args(f)   (f)->object.ffptr->required_args
#define function_optional_args(f)   (f)->object.ffptr->optional_args
#define function_has_rest_arg(f)    (f)->object.ffptr->rest_arg

#define is_input_port(p) (type(p) == T_INPUT_PORT) 
#define is_output_port(p) (type(p) == T_OUTPUT_PORT)


static char *describe_type(s7_pointer p)
{
  sprintf(error_buf, "%s%s%s%s%s%s%s%s",
	  ((type(p) >= 0) && (type(p) <= T_LAST_TYPE)) ? type_names[type(p)] : "bogus type",
	  (typeflag(p) & T_SYNTAX) ? " syntax" : "",
	  (typeflag(p) & T_IMMUTABLE) ? " immutable" : "",
	  (typeflag(p) & T_ATOM) ? " atom" : "",
	  (typeflag(p) & T_GC_MARK) ? " marked" : "",
	  (typeflag(p) & T_OBJECT) ? " object" : "",
	  (typeflag(p) & T_FINALIZABLE) ? " gc free" : "",
	  (typeflag(p) & T_UNUSED_BITS) ? " and other garbage bits!" : "");
  return(error_buf);
}

#define NUM_INT 0
#define NUM_RATIO 1
#define NUM_REAL 2
#define NUM_REAL2 3
#define NUM_COMPLEX 4

/* so int = 0, exact < 2, ratio = 1, complex >= 4, real 2 or 3
 *   (I'm not going to support exact complex or inexact ratio etc)
 */

#define num_is_fixnum(n) (n.type == NUM_INT)
#define object_is_fixnum(p) (p->object.number.type == NUM_INT)

#define num_is_ratio(n) (n.type == NUM_RATIO)
#define object_is_ratio(p) (p->object.number.type == NUM_RATIO)

#define num_is_real(n) ((n.type & (NUM_REAL | NUM_COMPLEX)) == NUM_REAL)
#define object_is_real(p) ((p->object.number.type & (NUM_REAL | NUM_COMPLEX)) == NUM_REAL

#define num_is_complex(n) (n.type & NUM_COMPLEX)
#define object_is_complex(p) (p->object.number.type & NUM_COMPLEX)

#define num_type(n) (n.type)
#define object_number_type(p) (p->object.number.type)

#define numerator(n) n.value.fvalue.numerator
#define denominator(n) n.value.fvalue.denominator

#define real_part(n) n.value.cvalue.real
#define imag_part(n) n.value.cvalue.imag

#define fraction(n) (((double)numerator(n)) / ((double)denominator(n)))

#define integer(n) n.value.ivalue
#define real(n)    n.value.rvalue

#define cons(sc, a, b) cons_1(sc, a, b, false)
#define immutable_cons(sc, a, b) cons_1(sc, a, b, true)

#define car(p) ((p)->object.cons.car)
#define cdr(p) ((p)->object.cons.cdr)

#define string_value(p)  ((p)->object.string.svalue)
#define string_length(p) ((p)->object.string.length)

#define character(p) ((p)->object.cvalue)

#define loop_counter(p) ((p)->object.number.value.ivalue)

#define set_integer(p) (p)->object.number.type = NUM_INT;


/* #define procedure_index(p) ((p)->object.number.value.ivalue) */
#define procedure_index(p) ((p)->object.proc_num)

#define syntax_opcode(x) cdr(x)

#define caar(p)          car(car(p))
#define cadr(p)          car(cdr(p))
#define cdar(p)          cdr(car(p))
#define cddr(p)          cdr(cdr(p))
#define cadar(p)         car(cdr(car(p)))
#define caddr(p)         car(cdr(cdr(p)))
#define cadaar(p)        car(cdr(car(car(p))))
#define cadddr(p)        car(cdr(cdr(cdr(p))))
#define cddddr(p)        cdr(cdr(cdr(cdr(p))))

#define vector_length(p)          ((p)->object.vector.length)
#define vector_element(p, i)      ((p)->object.vector.elements[i])
#define small_int(Sc, Val)        vector_element(Sc->small_ints, Val)
#define small_int_as_num(Sc, Val) small_int(Sc, Val)->object.number

#define is_true(p)           ((p) != sc->F)
#define is_false(p)          ((p) == sc->F)
#define to_s7_bool(sc, Val)  ((Val) ? sc->T : sc->F)

#define continuation_cc_stack_size(p) (p)->object.cc->cc_stack_size
#define continuation_cc_stack_top(p)  (p)->object.cc->cc_stack_top
#define continuation_cc_stack(p)      (p)->object.cc->cc_stack

#define is_goto(p)  (type(p) == T_GOTO)

#define port_needs_close(p) (p)->object.port->needs_close


static void set_type(s7_pointer p, int f)
{
  if (is_immutable(p))
    {
      fprintf(stderr, "set type wants to clobber immutable element");
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

static int safe_strlen(const char *str)
{
  if ((str) && (*str))
    return(strlen(str));
  return(0);
}

static bool is_one_of(char *s, int c);
static void s7_mark_embedded_objects(s7_pointer a); /* called by gc, calls fobj's mark func */
static void eval(s7_scheme *sc, opcode_t first_op);
static const char *procedure_name(s7_pointer x);
static s7_pointer g_stacktrace(s7_scheme *sc, s7_pointer args);


static void gp(s7_scheme *sc) {g_stacktrace(sc, sc->args);} /* for gdb */


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
      if ((s7_is_string(a)) ||
	  (s7_is_symbol(a)))
	{
	  FREE(string_value(a)); /* malloc'd in store_string */
	} 
      else 
	{
	  if (is_input_port(a)) 
	    {
	      if (port_needs_close(a))
		s7_close_input_port(sc, a);
	      FREE(a->object.port);
	    }
	  else
	    {
	      if (is_output_port(a))
		{
		  s7_close_output_port(sc, a);
		  FREE(a->object.port);
		}
	      else
		{
		  if (s7_is_object(a))
		    s7_free_object(a);
		  else
		    {
		      if ((s7_is_vector(a)) &&
			  (vector_length(a) > 0))
			FREE(a->object.vector.elements);
		      else
			{
			  if ((s7_is_continuation(a)) &&
			      (a->object.cc))
			    FREE(a->object.cc);
			}
		    }
		}
	    }
	}
    }
  memset((void *)a, 0, sizeof(s7_cell));
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
  if (is_marked(p)) return; 

  set_mark(p);
  if (is_atom(p))
    return;

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
  sc->free_heap[sc->free_heap_top++] = loc;
}

static s7_pointer alloc_s7_cell(s7_scheme *sc)
{
  if (sc->free_heap_top > 0)
    return(sc->heap[sc->free_heap[--(sc->free_heap_top)]]);
  return(NULL);
}


static int gc(s7_scheme *sc) 
{
  s7_pointer p;
  int i, freed_heap = 0;

#if TIMING
  clock_t start;
  start = clock();
  sc->gc_sweeps++;
#endif
  
  if ((sc->gc_verbose) &&
      (sc->outport == sc->NIL))
    fprintf(stderr, "\ngc...");
  
  /* mark system globals */
  s7_mark_object(sc->symbol_table);
  s7_mark_object(sc->global_env);
  
  /* mark current registers */
  s7_mark_object(sc->args);
  s7_mark_object(sc->envir);
  s7_mark_object(sc->code);
  mark_vector(sc->stack, sc->stack_top);
  /* s7_mark_object(sc->stack); */

  s7_mark_object(sc->value);
  s7_mark_object(sc->inport);
  s7_mark_object(sc->inport_stack);
  s7_mark_object(sc->outport);

  s7_mark_object(sc->protected_objects);
  s7_mark_object(sc->x);
  s7_mark_object(sc->y);

  /* garbage collect */
  clear_mark(sc->NIL);

  for (i = 0; i < sc->heap_size; i++)
    {
      p = sc->heap[i];
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
      (sc->outport == sc->NIL))
    fprintf(stderr, "done: %d heap were recovered, total heap: %d\n", freed_heap, sc->heap_size);
  return(freed_heap);
}


static s7_pointer new_cell(s7_scheme *sc)
{
  s7_pointer p;
  p = alloc_s7_cell(sc);
  if (!p)
    {
      /* no free heap */
      int k, old_size, freed_heap = 0;

      if (!(sc->gc_off)) 
	freed_heap = gc(sc);

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

  return(p);
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
  if (car(obj))
    car(nobj) = copy_object(sc, car(obj));
  if (cdr(obj))
    cdr(nobj) = copy_object(sc, cdr(obj));
  local_unprotect(nobj);

  return(nobj);
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
#if 0
  if (sc->stack_top == 0)
    return(sc->NIL); 
#endif

  top = sc->stack_top;

  sc->op =         (opcode_t)integer(vector_element(sc->stack, top - 1)->object.number);
  sc->args =       vector_element(sc->stack, top - 2);
  sc->envir =      vector_element(sc->stack, top - 3);
  sc->code =       vector_element(sc->stack, top - 4);
#if 0
  {
    int i;
    for (i = 1; i < 5; i++)
      vector_element(sc->stack, top - i) = sc->NIL;
  }
#endif
  sc->stack_top -= 4;
} 

static void push_stack(s7_scheme *sc, opcode_t op, s7_pointer args, s7_pointer code) 
{ 
  int top;

  top = sc->stack_top;
  sc->stack_top += 4;

  if (sc->stack_top >= sc->stack_size)
    {
      int i, new_size;
      new_size = sc->stack_size * 2;

      if (new_size > 2000) 
	{
	  fprintf(stderr, "stack is growing too big");
	  abort();
	}

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


/* -------------------------------------------------------------------------------- */



bool s7_is_string(s7_pointer p)
{
  return((type(p) == T_STRING)); 
}

bool s7_is_vector(s7_pointer p)    
{ 
  return(type(p) == T_VECTOR);
}

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


bool s7_is_character(s7_pointer p) 
{ 
  return(type(p) == T_CHARACTER);
}

char *s7_string(s7_pointer p) 
{ 
  return(string_value(p));
}

static num nvalue(s7_pointer p)       
{ 
  return((p)->object.number);
}


char s7_character(s7_pointer p)  
{ 
  return(character(p));
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
  if (is_immutable(car(p)))
    {
      fprintf(stderr, "set-car! wants to clobber immutable element");
      abort();
    }

  car(p) = q;
  return(q);
}

s7_pointer s7_set_cdr(s7_pointer p, s7_pointer q) 
{ 
  if (is_immutable(car(p)))
    {
      fprintf(stderr, "set-cdr! wants to clobber immutable element");
      abort();
    }

  cdr(p) = q;
  return(q);
}

bool s7_is_symbol(s7_pointer p)   
{ 
  return(type(p) == T_SYMBOL);
}

char *s7_symbol_name(s7_pointer p)   
{ 
  return(string_value(car(p)));
}

static bool s7_is_syntax(s7_pointer p)   
{ 
  return(typeflag(p) & T_SYNTAX);
}

s7_pointer s7_load_file_with_path(s7_scheme *sc, const char *file)
{
  return(s7_load(sc, file));
}

bool s7_is_procedure(s7_pointer p)     
{ 
  return(type(p) == T_PROC);
}

bool s7_is_function(s7_pointer p)  
{ 
  return(type(p) == T_S7_FUNCTION);
}

bool s7_is_object(s7_pointer p) 
{ 
  return(type(p) == T_S7_OBJECT);
}

bool s7_is_closure(s7_pointer p)  
{ 
  return(type(p) == T_CLOSURE);
}

static bool is_macro(s7_pointer p)    
{ 
  return(type(p) == T_MACRO);
}

static bool is_promise(s7_pointer p)  
{ 
  return(type(p) == T_PROMISE);
}

static s7_pointer procedure_source(s7_pointer p)   
{ 
  return(car(p));
}
      

static s7_pointer g_function_source(s7_scheme *sc, s7_pointer args)
{
  #define H_function_source "(function-source func) tries to return the definition of func"

  sc->y = s7_symbol_value(sc, car(args));
  if (sc->y != sc->NIL)
    return(procedure_source(sc->y));
  return(sc->NIL);
}


s7_pointer s7_procedure_environment(s7_pointer p)    
{ 
  return(cdr(p));
}

bool s7_is_continuation(s7_pointer p)    
{ 
  return(type(p) == T_CONTINUATION);
}



/* To do: promise should be forced ONCE only */
bool s7_is_immutable(s7_pointer p) 
{ 
  return(typeflag(p) & T_IMMUTABLE);
}

void s7_set_immutable(s7_pointer p) 
{ 
  typeflag(p) |= T_IMMUTABLE;
}

static int Cisalpha(int c) 
{ 
  return(isascii(c) && isalpha(c));
}

static int Cisdigit(int c) 
{ 
  return(isascii(c) && isdigit(c));
}

static int Cisspace(int c) 
{ 
  return(isascii(c) && isspace(c));
}

static int Cisupper(int c) 
{ 
  return(isascii(c) && isupper(c));
}

static int Cislower(int c) 
{ 
  return(isascii(c) && islower(c));
}




/* -------------------------------- numbers -------------------------------- */

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


s7_Int s7_numerator(s7_pointer x)
{
  return(numerator(x->object.number));
}

s7_Int s7_denominator(s7_pointer x)
{
  return(denominator(x->object.number));
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


static num num_quotient(num a, num b) 
{
  num ret;
  ret.type = a.type | b.type;
  if (num_is_fixnum(ret)) 
    integer(ret) = integer(a) / integer(b);
  else real(ret) = num_to_real(a) / num_to_real(b);
  return(ret);
}

static num num_rem(num a, num b) 
{
  num ret;
  ret.type = NUM_INT;
  integer(ret) = integer(a) % integer(b);
  return(ret);
}

static num num_mod(num a, num b) 
{
  num ret;
  ret.type = NUM_INT;
  integer(ret) = c_mod(integer(a), integer(b));
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
  
  while(*s != 0 && (*s =='1' || *s =='0')) 
    {
      x <<= 1;
      x += *s - '0';
      s++;
    }
  
  return x;
}

/* static off_t conses = 0; */

static s7_pointer cons_1(s7_scheme *sc, s7_pointer a, s7_pointer b, bool immutable) 
{
  s7_pointer x;

  local_protect(a);
  local_protect(b);
#if 0
  if (!is_object(a))
    {
      fprintf(stderr, "cons car is not an object!");
      abort();
    }
  if (!is_object(b))
    {
      fprintf(stderr, "cons cdr is not an object!");
      abort();
    }
#endif
  x = new_cell(sc);
  /* conses++; */

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



/* ========== symbol_table implementation  ========== */ 


static int hash_fn(const char *key, int table_size) 
{ 
  /* I tried several other hash functions, but they gave about the same incidence of collisions */
  /*    the first is from the original tinyScheme, the second from Guile */
  unsigned int hashed = 0; 
  const char *c; 
  
#if 0
  int bits_per_int = sizeof(unsigned int) * 8; 
  for (c = key; *c; c++) 
    { 
      /* letters have about 5 bits in them */ 
      hashed = (hashed << 5) | (hashed >> (bits_per_int - 5)); 
      hashed ^= *c; 
    } 
#else
  for (c = key; *c; c++) 
    hashed = *c + hashed * 37;
#endif
  return(hashed % table_size); 
} 


/* returns the new symbol */ 

/* r4rs: hashes: 2346, collisions: 513, bigger table: hashes: 2338, collisions: 276, 2nd (simpler) form: 2338, collisions: 267
 */

static s7_pointer symbol_table_add_by_name(s7_scheme *sc, const char *name) 
{ 
  s7_pointer x; 
  int location;
  
  x = cons(sc, s7_make_string(sc, name), sc->NIL); 
  set_type(x, (T_SYMBOL | T_FINALIZABLE));
  local_protect(x);
  s7_set_immutable(car(x)); 

  location = hash_fn(name, vector_length(sc->symbol_table)); 
  vector_element(sc->symbol_table, location) = s7_immutable_cons(sc, x, vector_element(sc->symbol_table, location)); 

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
      /* case-insensitive, per R5RS section 2. */ 
      if (strcasecmp(name, s) == 0) 
	return(car(x)); 
    } 
  return sc->NIL; 
} 

static s7_pointer symbol_table_all_symbols(s7_scheme *sc) 
{ 
  int i; 
  s7_pointer x; 
  s7_pointer ob_list = sc->NIL; 
  
  for (i = 0; i < vector_length(sc->symbol_table); i++) 
    for (x  = vector_element(sc->symbol_table, i); x != sc->NIL; x = cdr(x)) 
      ob_list = cons(sc, x, ob_list); 

  return ob_list; 
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

s7_pointer s7_make_character(s7_scheme *sc, int c) 
{
  s7_pointer x = new_cell(sc);
  set_type(x, T_CHARACTER | T_ATOM);
  character(x) = c;
  return(x);
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



/* allocate name to string area */
static char *store_string(s7_scheme *sc, int len_str, const char *str, char fill) 
{
  char *q;
  
  q = (char *)MALLOC(len_str + 1);
  if (q == 0) 
    {
      fprintf(stderr, "failed in store_string\n");
      return(sc->strbuf);
    }
  if (str != 0) 
    strcpy(q, str);
  else 
    {
      memset(q, fill, len_str);
      q[len_str]= 0;
    }
  return(q);
}

/* get new string */
s7_pointer s7_make_string(s7_scheme *sc, const char *str) 
{
  return(s7_make_counted_string(sc, str, safe_strlen(str)));
}

s7_pointer s7_make_counted_string(s7_scheme *sc, const char *str, int len) 
{
  s7_pointer x;

  x = new_cell(sc);
  set_type(x, T_STRING | T_ATOM | T_FINALIZABLE);
  local_protect(x);
  string_value(x) = store_string(sc, len, str, 0);
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
  string_value(x) = store_string(sc,len, 0,fill);
  string_length(x) = len;
  local_unprotect(x);
  return(x);
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
      s7_fill_vector(x, sc->NIL);
    }
  return(x);
}

int s7_vector_length(s7_pointer vec)
{
  return(vector_length(vec));
}

void s7_fill_vector(s7_pointer vec, s7_pointer obj) 
{
  int i, len;
  len = vector_length(vec);
  for(i = 0; i < len; i++) 
    vector_element(vec, i) = obj;
}

s7_pointer s7_vector_ref(s7_pointer vec, int elem) 
{
  if (elem >= vector_length(vec))
    fprintf(stderr, "vector-ref past end of vector: %d %d\n", elem, vector_length(vec));
  return(vector_element(vec, elem));
}

s7_pointer s7_vector_set(s7_pointer vec, int elem, s7_pointer a) 
{
  if (!is_object(a))
    {
      fprintf(stderr, "vector-set! value is not an object!");
      abort();
    }

  if (elem >= vector_length(vec))
    fprintf(stderr, "vector-set past end of vector: %d %d\n", elem, vector_length(vec));

  vector_element(vec, elem) = a;
  return(a);
}

/* get new symbol */
s7_pointer s7_make_symbol(s7_scheme *sc, const char *name) 
{ 
  s7_pointer x; 
  /*
  fprintf(stderr, "make_symbol %s\n", name);
  */
  /* first check symbol_table */ 
  x = symbol_table_find_by_name(sc, name); 
  if (x != sc->NIL) 
    {
      /*
      fprintf(stderr, "already exists: %p\n", x);
      */
      return(x); 
    }

  x = symbol_table_add_by_name(sc, name); 
  /*
  fprintf(stderr, "new loc: %p\n", x);
  */
  return(x); 
} 

s7_pointer s7_gensym(s7_scheme *sc) 
{ 
  s7_pointer x; 
  char name[40]; 
  
  for(; sc->gensym_cnt < LONG_MAX; sc->gensym_cnt++) 
    { 
      sprintf(name, "gensym-%ld", sc->gensym_cnt); 
      
      /* first check symbol_table */ 
      x = symbol_table_find_by_name(sc, name); 
      
      if (x != sc->NIL) 
	continue; 

      x = symbol_table_add_by_name(sc, name); 
      return(x); 
    } 
  
  return sc->NIL; 
} 


/* make symbol or number atom from string */

static s7_pointer make_atom(s7_scheme *sc, char *q) 
{
  char c, *p, *slash, *plus;
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
      q[len - 1] = '\0'; /* remove 'i' */
      
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


/* make constant */
static s7_pointer make_sharp_const(s7_scheme *sc, char *name) 
{
  long x;
  char tmp[256];

  if (!strcmp(name, "t"))
    return(sc->T);

  if (!strcmp(name, "f"))
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





/* -------------------------------- ports -------------------------------- */

#define port_paren_depth(p) (p)->object.port->paren_depth
#define is_string_port(p) (!((p)->object.port->is_file))
#define is_file_port(p) (p)->object.port->is_file
#define port_line_number(p) (p)->object.port->rep.stdio.line_number
#define port_filename(p) (p)->object.port->rep.stdio.filename
#define port_file(p) (p)->object.port->rep.stdio.file
#define port_is_closed(p) (p)->object.port->is_closed

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
  return(sc->inport);
}

static s7_pointer g_current_input_port(s7_scheme *sc, s7_pointer args)
{
  #define H_current_input_port "(current-input-port) returns the current input port"
  return(sc->inport);
}


/* -------- current-output-port -------- */

s7_pointer s7_current_output_port(s7_scheme *sc)
{
  return(sc->outport);
}

static s7_pointer g_current_output_port(s7_scheme *sc, s7_pointer args)
{
  #define H_current_output_port "(current-output-port) returns the current output port"
  return(sc->outport);
}


/* -------- char-ready? -------- */

static s7_pointer g_is_char_ready(s7_scheme *sc, s7_pointer args)
{
  #define H_is_char_ready "(char-ready? :optional port) returns #t if a character is ready for input on the given port"
  if (s7_is_pair(args))
    {
      s7_pointer pt = car(args);
      if (!s7_is_input_port(sc, pt))
	return(s7_error(sc, s7_make_string(sc, "char-ready? argument should be an input port"), pt));

      return(to_s7_bool(sc, is_string_port(pt)));
    }
  return(to_s7_bool(sc, (is_input_port(sc->inport)) && (is_string_port(sc->inport))));  /* TODO: need to check for waiting input on files */
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
    return(s7_error(sc, s7_make_string(sc, "close-input-port argument should be an input port"), pt));
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
	  port_needs_close(p) = false;
	}
    }
  else
    {
      if (p->object.port->rep.string.start)
	{
	  FREE(p->object.port->rep.string.start);
	  p->object.port->rep.string.start = NULL;
	}
    }
  port_is_closed(p) = true;
}

static s7_pointer g_close_output_port(s7_scheme *sc, s7_pointer args)
{
  #define H_close_output_port "(close-output-port port) closes the port"
  s7_pointer pt = car(args);
  if (!is_output_port(pt))
    return(s7_error(sc, s7_make_string(sc, "close-output-port argument should be an output port"), pt));
  s7_close_output_port(sc, pt);
  return(sc->UNSPECIFIED);
}


/* -------- open-input-file -------- */

s7_pointer s7_open_input_file(s7_scheme *sc, const char *name)
{
  FILE *fp;
  /* see if we can open this file before allocating a port */

  fp = fopen(name, "r");
  if (!fp)
    return(s7_error(sc, s7_make_string(sc, "open-input-file can't open file"), s7_make_string(sc, name)));

  sc->x = new_cell(sc);
  set_type(sc->x, T_INPUT_PORT | T_ATOM | T_FINALIZABLE);

  /* set up the port struct */
  sc->x->object.port = (port *)MALLOC(sizeof(port));
  port_file(sc->x) = fp;
  is_file_port(sc->x) = true;
  port_is_closed(sc->x) = false;
  port_filename(sc->x) = copy_string(name);  /* TODO: gc the name */
  port_line_number(sc->x) = 0;
  port_paren_depth(sc->x) = 0;
  port_needs_close(sc->x) = false;

  return(sc->x);
}

static s7_pointer g_open_input_file(s7_scheme *sc, s7_pointer args)
{
  #define H_open_input_file "(open-input-file filename) opens filename for reading"
  s7_pointer name = car(args);
  if (!s7_is_string(name))
    return(s7_error(sc, s7_make_string(sc, "open-input-file argument should be a string (a filename)"), name));
  
  return(s7_open_input_file(sc, s7_string(name)));
}


/* -------- open-output-file -------- */

s7_pointer s7_open_output_file(s7_scheme *sc, const char *name)
{
  FILE *fp;
  /* see if we can open this file before allocating a port */

  fp = fopen(name, "w");
  if (!fp)
    return(s7_error(sc, s7_make_string(sc, "open-output-file can't open file"), s7_make_string(sc, name)));

  sc->x = new_cell(sc);
  set_type(sc->x, T_OUTPUT_PORT | T_ATOM | T_FINALIZABLE);

  /* set up the port struct */
  sc->x->object.port = (port *)MALLOC(sizeof(port));
  is_file_port(sc->x) = true;
  port_is_closed(sc->x) = false;
  port_filename(sc->x) = copy_string(name);  /* TODO: gc the name */
  port_line_number(sc->x) = 0;
  port_paren_depth(sc->x) = 0;
  port_needs_close(sc->x) = false;
  port_file(sc->x) = fp;

  return(sc->x);
}

static s7_pointer g_open_output_file(s7_scheme *sc, s7_pointer args)
{
  #define H_open_output_file "(open-output-file filename) opens filename for writing"
  s7_pointer name = car(args);
  if (!s7_is_string(name))
    return(s7_error(sc, s7_make_string(sc, "open-output-file argument should be a string (a filename)"), name));
  
  return(s7_open_output_file(sc, s7_string(name)));
}


/* -------- open-input-string -------- */

s7_pointer s7_open_input_string(s7_scheme *sc, const char *input_string)
{
  sc->x = new_cell(sc);
  set_type(sc->x, T_INPUT_PORT | T_ATOM | T_FINALIZABLE);

  /* set up the port struct */
  sc->x->object.port = (port *)MALLOC(sizeof(port));
  is_file_port(sc->x) = false;
  port_is_closed(sc->x) = false;
  port_paren_depth(sc->x) = 0;
  sc->x->object.port->rep.string.start = (char *)input_string;
  sc->x->object.port->rep.string.curr = (char *)input_string;
  sc->x->object.port->rep.string.past_the_end = (char *)(input_string + safe_strlen(input_string));

  return(sc->x);
}

static s7_pointer g_open_input_string(s7_scheme *sc, s7_pointer args)
{
  #define H_open_input_string "(open-input-string str) opens an input port reading str"
  s7_pointer input_string = car(args);
  if (!s7_is_string(input_string))
    return(s7_error(sc, s7_make_string(sc, "open-input-string argument should be a string"), input_string));
  
  return(s7_open_input_string(sc, s7_string(input_string))); /* presumably the caller is protecting the input string?? */
}


/* -------- open-output-string -------- */

s7_pointer s7_open_output_string(s7_scheme *sc)
{
  char *temp;
  sc->x = new_cell(sc);
  set_type(sc->x, T_OUTPUT_PORT | T_ATOM | T_FINALIZABLE);

  /* set up the port struct */
  sc->x->object.port = (port *)MALLOC(sizeof(port));
  is_file_port(sc->x) = false;
  port_is_closed(sc->x) = false;
  temp = (char *)CALLOC(128, sizeof(char));
  sc->x->object.port->rep.string.start = temp;
  sc->x->object.port->rep.string.curr = temp;
  sc->x->object.port->rep.string.past_the_end = (char *)(temp + 128);

  return(sc->x);
}

static s7_pointer g_open_output_string(s7_scheme *sc, s7_pointer args)
{
  #define H_open_output_string "(open-output-string) opens an output string port"
  return(s7_open_output_string(sc));
}


/* -------- get-output-string -------- */

char *s7_get_output_string(s7_scheme *sc, s7_pointer p)
{
  return(p->object.port->rep.string.start);
}

static s7_pointer g_get_output_string(s7_scheme *sc, s7_pointer args)
{
  #define H_get_output_string "(get-output-string port) returns the output accumulated in port"
  s7_pointer p = car(args);
  if ((!is_output_port(p)) ||
      (!is_string_port(p)))
    return(s7_error(sc, s7_make_string(sc, "get-output-string argument should be an output string port"), p));
  return(s7_make_string(sc, s7_get_output_string(sc, p)));
}


static bool push_input_port(s7_scheme *sc, s7_pointer new_port)
{
  sc->inport_stack = cons(sc, sc->inport, sc->inport_stack);
  sc->inport = new_port;
  return(sc->inport != sc->NIL);
}

static s7_pointer pop_input_port(s7_scheme *sc)
{
  if (s7_is_pair(sc->inport_stack))
    {
      sc->inport = car(sc->inport_stack);
      sc->inport_stack = cdr(sc->inport_stack);
    }
  else sc->inport = sc->NIL;
  return(sc->inport);
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
      if ((*pt->object.port->rep.string.curr == 0) || 
	  (pt->object.port->rep.string.curr == pt->object.port->rep.string.past_the_end))
	return(EOF);
      else return(*pt->object.port->rep.string.curr++);
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
      if (pt->object.port->rep.string.curr != pt->object.port->rep.string.start) 
	--pt->object.port->rep.string.curr;
    }
}


/* -------- read token or expression -------- */

/* read characters up to delimiter, but cater to character constants */

static char *read_string_upto(s7_scheme *sc, char *delim, s7_pointer pt) 
{
  char *p = sc->strbuf;
  
  while (!is_one_of(delim, (*p++ = inchar(sc, pt))));
  if ((p == sc->strbuf + 2) && 
      (p[-2] == '\\'))
    *p = 0;
  else 
    {
      backchar(sc, p[-1], pt);
      *--p = '\0';
    }

  return(sc->strbuf);
}


/* read string expression "xxx...xxx" */

static s7_pointer read_string_expression(s7_scheme *sc, s7_pointer pt) 
{
  char *p = sc->strbuf;
  int c;
  int c1 = 0;
  enum {ST_OK, ST_BSL, ST_X1, ST_X2, ST_OCT1, ST_OCT2, ST_OCT3} state = ST_OK; /* BSL=backslash? X1 or X2 = hex (ctrs) as in oct case */
  
  for (;;) 
    {
      c = inchar(sc, pt);
      if ((c == EOF) || 
	  (p - sc->strbuf > sizeof(sc->strbuf) - 1))
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
	      *p = 0;
	      return(s7_make_counted_string(sc, sc->strbuf, p - sc->strbuf));

	    default:
	      *p++ = c;
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
	      *p++ = '\n';
	      state = ST_OK;
	      break;

	    case 't':
	      *p++ = '\t';
	      state = ST_OK;
	      break;

	    case 'r':
	      *p++ = '\r';
	      state = ST_OK;
	      break;

	    case '"':
	      *p++ = '"';
	      state = ST_OK;
	      break;

	    default:
	      *p++ = c;
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
		  *p++ = c1;
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
	      
	      *p++ = c1;
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
		  *p++ = c1;
		  state = ST_OK;
		  break;
		}
	    }
	  break;
	}
    }
}


/* check c is in chars */

static  bool is_one_of(char *s, int c) 
{
  if (c == EOF) return(true);
  while (*s)
    if (*s++ == c)
      return(true);
  return(false);
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

static int token(s7_scheme *sc, s7_pointer pt) 
{
  int c;
  skipspace(sc, pt);
  switch (c = inchar(sc, pt)) 
    {
    case EOF:
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
      if (is_one_of(" tfodxb\\", c)) 
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
  else c = s7_read_char_1(sc, sc->inport, peek);
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
      push_input_port(sc, port);
      push_stack(sc, OP_READ_RETURN_EXPRESSION, port, sc->NIL);
      eval(sc, OP_READ_INTERNAL);
      pop_input_port(sc);
      return(sc->value);
    }
  return(s7_error(sc, s7_make_string(sc, "read's argument should be an input port"), port));  
}


static s7_pointer g_read(s7_scheme *sc, s7_pointer args)
{
  #define H_read "(read :optional port) returns the next object in the input port"
  s7_pointer port;

  if (s7_is_pair(args))
    port = car(args);
  else port = sc->inport;

  if (!s7_is_input_port(sc, port))
    return(s7_error(sc, s7_make_string(sc, "read needs an input port"), port));

  push_input_port(sc, port);
  push_stack(sc, OP_READ_POP_AND_RETURN_EXPRESSION, sc->NIL, sc->NIL); /* this stops the internal read process so we only get one form */
  push_stack(sc, OP_READ_INTERNAL, sc->NIL, sc->NIL);
  return(port);
}


/* -------- load -------- */

s7_pointer s7_load(s7_scheme *sc, const char *filename)
{
  s7_pointer port;

  port = s7_open_input_file(sc, filename);
  if (port == sc->ERROR)
    return(port);
  port_needs_close(port) = true;

  push_input_port(sc, port);
  push_stack(sc, OP_LOAD_RETURN_IF_EOF, port, sc->NIL);
  eval(sc, OP_READ_INTERNAL);
  /* TODO: check paren count and below */
  
  pop_input_port(sc);
  s7_close_input_port(sc, port);

  return(sc->UNSPECIFIED);
}

static s7_pointer g_load(s7_scheme *sc, s7_pointer args)
{
  s7_pointer name, port;

  name = car(args);
  if (!s7_is_string(name))
    return(s7_error(sc, s7_make_string(sc, "load argument should be a string (a filename)"), name));

  port = s7_open_input_file(sc, s7_string(name));
  if (port == sc->ERROR)
    return(port);
  port_needs_close(port) = true;

  push_input_port(sc, port);
  push_stack(sc, OP_LOAD_CLOSE_AND_POP_IF_EOF, port, sc->NIL);
  push_stack(sc, OP_READ_INTERNAL, sc->NIL, sc->NIL);

  return(sc->UNSPECIFIED);
}


/* -------- eval string -------- */

s7_pointer s7_eval_c_string(s7_scheme *sc, const char *str)
{
  s7_pointer port;
  stack_reset(sc); 
  sc->envir = sc->global_env;
  port = s7_open_input_string(sc, str);
  push_input_port(sc, port);
  push_stack(sc, OP_EVAL_STRING, port, sc->NIL);
  eval(sc, OP_READ_INTERNAL);
  pop_input_port(sc);
  s7_close_input_port(sc, port);
  return(sc->value);
}


s7_pointer s7_eval_form(s7_scheme *sc, s7_pointer form)
{
  /* TODO: eval form */
  return(sc->F);
}



/* -------- output -------- */

static void char_to_string_port(char c, port *pt)
{
  if (pt->rep.string.curr >= pt->rep.string.past_the_end)
    {
      int loc;
      loc = (int)(pt->rep.string.curr - pt->rep.string.start);
      pt->rep.string.start = (char *)REALLOC(pt->rep.string.start, loc * 2);
      pt->rep.string.curr = (char *)(pt->rep.string.start + loc);
      pt->rep.string.past_the_end = (char *)(pt->rep.string.start + (loc * 2));
    }
  *pt->rep.string.curr++ = c;
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
      else char_to_string_port(c, pt->object.port);
    }
}


static void write_string(s7_scheme *sc, const char *s, s7_pointer pt) 
{
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
	    char_to_string_port(*s, pt->object.port);
	}
    }
}


static char *slashify_string(s7_scheme *sc, const char *p, int len) 
{
  int i, j = 0;
  char *s;
  s = (char *)CALLOC(safe_strlen(p) + 256, sizeof(char));

  s[j++] = '"';
  for (i = 0; i < len; i++) 
    {
      if (p[i] == 0xff || p[i] == '"' || p[i] < ' ' || p[i] == '\\') 
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

  if (obj == sc->ERROR) 
    return(copy_string("#<error>"));

  if ((is_input_port(obj)) || (is_output_port(obj)))
    return(describe_port(sc, obj));

  if (s7_is_number(obj)) 
    {
      char *p;
      p = (char *)CALLOC(256, sizeof(char));

      switch (object_number_type(obj))
	{
	case NUM_INT:
	  snprintf(p, 256, s7_Int_d, s7_integer(obj));
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

	case NUM_RATIO:
	  snprintf(p, 256, s7_Int_d "/" s7_Int_d, s7_numerator(obj), s7_denominator(obj));
	  break;

	default:
	  if (s7_imag_part(obj) >= 0.0)
	    snprintf(p, 256, "%.14g+%.14gi", s7_real_part(obj), s7_imag_part(obj));
	  else snprintf(p, 256, "%.14g-%.14gi", s7_real_part(obj), fabs(s7_imag_part(obj)));
	  break;

	}
      return(p);
    }

  if (s7_is_string(obj)) 
    {
      if (!use_write) 
	return(copy_string(string_value(obj)));
      return(slashify_string(sc, string_value(obj), string_length(obj)));
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
	      sprintf(p, "#\\space"); break;
	    case '\n':
	      sprintf(p, "#\\newline"); break;
	    case '\r':
	      sprintf(p, "#\\return"); break;
	    case '\t':
	      sprintf(p, "#\\tab"); break;
	    default:
	      if (c < 32) 
		sprintf(p, "#\\x%x",c); break;
	      sprintf(p, "#\\%c",c); break;
	    }
	}
      return(p);
    }

  if (s7_is_symbol(obj))
    return(copy_string(s7_symbol_name(obj)));

  if (s7_is_procedure(obj)) 
    {
      char *p;
      p = (char *)CALLOC(2 + safe_strlen(procedure_name(obj)), sizeof(char));
      sprintf(p, "%s", procedure_name(obj));
      return(p);
    }

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
      sprintf(p, "<%s>", function_name(obj));
      return(p);
    } 
  
  if (s7_is_continuation(obj)) 
    return(copy_string("#<continuation>"));

  if (is_goto(obj)) 
    return(copy_string("#<goto>"));

  if (s7_is_object(obj)) 
    return(copy_string(s7_describe_object(obj)));

  return(copy_string("#<unknown object!>"));
}


static char *s7_vector_to_c_string(s7_scheme *sc, s7_pointer vect)
{
  int i, len, bufsize = 0;
  bool too_long = false;
  char **elements = NULL;
  char *buf;

  len = vector_length(vect);
  if (len == 0)
    return(copy_string("#()"));

  if (len > 8) /* TODO: vector-print-length */
    {
      too_long = true;
      len = 8;
    }
  elements = (char **)MALLOC(len * sizeof(char *));
  for (i = 0; i < len; i++)
    {
      elements[i] = copy_string(string_value(s7_object_to_string(sc, vector_element(vect, i))));
      bufsize += safe_strlen(elements[i]);
    }
  bufsize += 128;
  buf = (char *)CALLOC(bufsize, sizeof(char));
  sprintf(buf, "#(");
  for (i = 0; i < len - 1; i++)
    {
      strcat(buf, elements[i]);
      FREE(elements[i]);
      strcat(buf, " ");
    }
  strcat(buf, elements[len - 1]);
  FREE(elements[len - 1]);
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
      elements[i] = copy_string(string_value(s7_object_to_string(sc, car(x))));
      bufsize += safe_strlen(elements[i]);
    }
  if (dotted)
    {
      elements[i] = copy_string(string_value(s7_object_to_string(sc, x)));
      bufsize += safe_strlen(elements[i]);
    }

  bufsize += (128 + len); /* len spaces */
  buf = (char *)CALLOC(bufsize, sizeof(char));
  sprintf(buf, "(");
  for (i = 0; i < len - 1; i++)
    {
      strcat(buf, elements[i]);
      strcat(buf, " ");
    }
  if (dotted) strcat(buf, ". ");
  strcat(buf, elements[len - 1]);
  strcat(buf, ")");

  for (i = 0; i < len; i++)
    FREE(elements[i]);
  FREE(elements);
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
  if (s7_is_vector(obj))
    return(s7_vector_to_string(sc, obj));
  if (s7_is_pair(obj))
    return(s7_list_to_string(sc, obj));
  return(s7_make_string(sc, s7_atom_to_c_string(sc, obj, true)));
}


static void print_atom(s7_scheme *sc, s7_pointer obj)
{
  /* this is used in the eval loop, always assuming current-output-port (eval|error output, etc) */
  char *p;
  p = s7_atom_to_c_string(sc, obj, true);
  if (p)
    {
      write_string(sc, p, sc->outport);
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
	return(s7_error(sc, s7_make_string(sc, "newline port argument should be an output port"), port));
    }
  else port = sc->outport;

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
    return(s7_error(sc, s7_make_string(sc, "write-char 1st argument should be a character"), args));

  if (s7_is_pair(cdr(args)))
    {
      port = cadr(args);
      if (!is_output_port(port))
	return(s7_error(sc, s7_make_string(sc, "write-char port argument should be an output port"), port));
    }
  else port = sc->outport;
  s7_write_char(sc, s7_character(car(args)), port);
  return(sc->UNSPECIFIED);
}


/* -------- write/display -------- */

static void write_or_display(s7_scheme *sc, s7_pointer obj, s7_pointer port, bool use_write)
{
  char *val;
  val = s7_object_to_c_string_1(sc, obj, use_write);
  write_string(sc, val, port);
  FREE(val);
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
	return(s7_error(sc, s7_make_string(sc, "write port argument should be an output port"), port));
    }
  else port = sc->outport;
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
	return(s7_error(sc, s7_make_string(sc, "display port argument should be an output port"), port));
    }
  else port = sc->outport;
  write_or_display(sc, car(args), port, false);
  return(sc->UNSPECIFIED);
}


/* -------- stacktrace -------- */


/* TODO: use scheme output funcs here */

static void print_stack_entry(s7_scheme *sc, opcode_t op, s7_pointer code, s7_pointer args)
{
  if (op != OP_APPLY)
    {
      char *temp;
      temp = copy_string(opcode_names[op]);
      fprintf(stderr, "\n%s: args: %s, code: %s", 
	      string_downcase(temp),
	      s7_object_to_c_string(sc, args),
	      s7_object_to_c_string(sc, code));
      FREE(temp);
    }
  else
    {
      if (s7_is_function(code))
	fprintf(stderr, "\n(%s %s)", function_name(code), s7_object_to_c_string(sc, args));
      else
	{
	  if (s7_is_procedure(code))
	    fprintf(stderr, "\n(%s %s)", procedure_name(code), s7_object_to_c_string(sc, args));
	  else fprintf(stderr, "\n(%s %s)", s7_object_to_c_string(sc, code), s7_object_to_c_string(sc, args));
	}
    }
}

static s7_pointer g_stacktrace(s7_scheme *sc, s7_pointer args)
{
  #define H_stacktrace "(stacktrace) prints out the current stack contents"
  int i;
  sc->gc_off = true;

  if (sc->args == NULL) fprintf(stderr, "trace null args");

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





#if 0
/* TODO: eval internal */
      case OP_PEVAL: /* eval */
	if (cdr(sc->args)!= sc->NIL) 
	  {
	    sc->envir= cadr(sc->args);
	  }
	sc->code = car(sc->args);
	goto EVAL;
	
#endif




/* make closure. c is code. e is environment */
s7_pointer s7_make_closure(s7_scheme *sc, s7_pointer c, s7_pointer e) 
{
  s7_pointer x = new_cell(sc);
  set_type(x, T_CLOSURE);

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

  car(x) = c;
  cdr(x) = e;
  return(x);
}


static s7_pointer copy_vector(s7_scheme *sc, s7_pointer old_v, int top)
{
  int i, len;
  s7_pointer new_v;
  len = vector_length(old_v);
  new_v = s7_make_vector(sc, len);
  local_protect(new_v);

  sc->gc_off = true;
  for (i = 0; i < top; i++)
    vector_element(new_v, i) = copy_object(sc, vector_element(old_v, i));
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
  c->cc_stack = copy_vector(sc, sc->stack, sc->stack_top);

  x->object.cc = c;
  local_unprotect(x);

  return(x);
}

static s7_pointer s7_call_continuation(s7_scheme *sc, s7_pointer p)
{
  continuation *c;
  c = p->object.cc;

  sc->stack = copy_vector(sc, c->cc_stack, c->cc_stack_top);
  sc->stack_size = c->cc_stack_size;
  sc->stack_top = c->cc_stack_top;
  return(sc->value);
}

static s7_pointer list_star(s7_scheme *sc, s7_pointer d) 
{
  s7_pointer p, q;
  if (cdr(d) == sc->NIL) 
    return(car(d));

  p = cons(sc, car(d), cdr(d));
  q = p;
  while(cdr(cdr(p)) != sc->NIL) 
    {
      d = cons(sc, car(p), cdr(p));
      if (cdr(cdr(p)) != sc->NIL) 
	{
	  p = cdr(d);
	}
    }
  cdr(p) = car(cdr(p));
  return(q);
}

/* reverse list -- produce new list */
s7_pointer s7_reverse(s7_scheme *sc, s7_pointer a) 
{
  /* a must be checked by gc */
  s7_pointer p = sc->NIL;
  
  for ( ; s7_is_pair(a); a = cdr(a)) 
    {
      p = cons(sc, car(a), p);
    }
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

/* equivalence of atoms */
bool s7_eqv_p(s7_pointer a, s7_pointer b) 
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
  
  if (s7_is_procedure(a))
    return(procedure_index(a) == procedure_index(b));
  
  return(false);
}



/* ========== Environment implementation  ========== */ 
/* 
 * In this implementation, each frame of the environment may be 
 * a hash table: a vector of alists hashed by variable name. 
 * In practice, we use a vector only for the initial frame; 
 * subsequent frames are too small and transient for the lookup 
 * speed to out-weigh the cost of making a new vector. 
 */ 

static void new_frame_in_env(s7_scheme *sc, s7_pointer old_env) 
{ 
  s7_pointer new_frame; 

  /* The interaction-environment has about 300 variables in it. */ 
  if (old_env == sc->NIL) 
    new_frame = s7_make_vector(sc, SYMBOL_TABLE_SIZE); 
  else new_frame = sc->NIL; 
  local_protect(new_frame);
   
  sc->envir = immutable_cons(sc, new_frame, old_env); 
  local_unprotect(new_frame);
} 


static void s7_new_slot_spec_in_env(s7_scheme *sc, s7_pointer env, s7_pointer variable, s7_pointer value) 
{ 
  s7_pointer slot;
  slot = s7_immutable_cons(sc, variable, value); 
  local_protect(slot);
  if (s7_is_vector(car(env))) 
    { 
      int location = hash_fn(s7_symbol_name(variable), vector_length(car(env))); 
      vector_element(car(env), location) = s7_immutable_cons(sc, slot, vector_element(car(env), location)); 
    } 
  else car(env) = s7_immutable_cons(sc, slot, car(env));
  local_unprotect(slot);
} 

static s7_pointer s7_find_slot_in_env(s7_scheme *sc, s7_pointer env, s7_pointer hdl, int all) 
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





bool s7_is_equal(s7_scheme *sc, s7_pointer x, s7_pointer y);

static bool vectors_equal(s7_scheme *sc, s7_pointer x, s7_pointer y)
{
  int i, len;
  len = vector_length(x);
  if (len != vector_length(y)) return(false);
  for (i = 0; i < len; i++)
    if (!(s7_is_equal(sc, vector_element(x, i), vector_element(y, i))))
      return(false);
  return(true);
}

bool s7_is_equal(s7_scheme *sc, s7_pointer x, s7_pointer y)
{
  if (x == y) 
    return(true);
  
  if (type(x) != type(y)) 
    return(false);
  
  if (s7_is_pair(x))
    return((s7_is_equal(sc, car(x), car(y))) &&
	   (s7_is_equal(sc, cdr(x), cdr(y))));
  
  if (s7_is_string(x))
    return((string_length(x) == string_length(y)) &&
	   ((string_length(x) == 0) ||
	    (strcmp(string_value(x), string_value(y)) == 0)));
  
  if (s7_is_object(x))
    return(s7_equalp_objects(x, y));
  
  if (s7_is_procedure(x))
    return(procedure_index(x) == procedure_index(y));
  
  if (s7_is_vector(x))
    return(vectors_equal(sc, x, y));
  
  if (s7_is_character(x)) 
    return(s7_character(x) == s7_character(y));
  
  if (s7_is_number(x))
    return(numbers_are_eqv(x, y));
  
  return(false); /* we already checked that x != y (port etc) */
}

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


typedef s7_pointer (*dispatch_func)(s7_scheme *sc, opcode_t op);

typedef bool (*test_predicate)(s7_pointer);

static bool is_any(s7_pointer p) 
{ 
  return(true);
}

static bool s7_is_inport(s7_pointer p)
{
  return(is_input_port(p));
  /* TODO: get rid of this and the following 2 */
}

static bool s7_is_outport(s7_pointer p)
{
  return(is_output_port(p));
}

static bool s7_is_port(s7_pointer p)
{
  return(is_output_port(p) || is_input_port(p));
}


static bool is_not_negative(s7_pointer p) 
{
  return(s7_is_integer(p) && (s7_integer((p)) >= 0));
}

static s7_pointer tst_NIL = 0;

static bool s7_is_pair_or_nil(s7_pointer p)
{
  return((p == tst_NIL) ||
	 (s7_is_pair(p)));
}


#define ok_abbrev(x) (s7_is_pair(x) && cdr(x) == sc->NIL)


/* Correspond carefully with following defines! */
static struct {
  test_predicate fct;
  const char *kind;
} tests[] = {
  {0, 0}, /* unused */
  {is_any, 0},
  {s7_is_string, "string"},
  {s7_is_symbol, "symbol"},
  {s7_is_port, "port"},
  {s7_is_inport, "input port"},
  {s7_is_outport, "output_port"},
  {s7_is_pair, "pair"},
  {s7_is_pair_or_nil, "pair or '()"},
  {s7_is_character, "character"},
  {s7_is_vector, "vector"},
  {s7_is_number, "number"},
  {s7_is_integer, "integer"},
  {is_not_negative, "non-negative integer"},
  {s7_is_real, "non-complex number"},
  {s7_is_rational, "rational number"},
};

/* there's a hidden assumption that there are no gaps in this list */
#define TST_NONE 0
#define TST_ANY "\001"
#define TST_STRING "\002"
#define TST_SYMBOL "\003"
#define TST_PORT "\004"
#define TST_INPORT "\005"
#define TST_OUTPORT "\006"
#define TST_PAIR "\007"
#define TST_LIST "\010"
#define TST_CHAR "\011"
#define TST_VECTOR "\012"
#define TST_NUMBER "\013"
#define TST_INTEGER "\014"
#define TST_NATURAL "\015"
#define TST_REAL "\016"
#define TST_RATIONAL "\017"


typedef struct {
  char *name;
  int min_arity;
  int max_arity;
  char *arg_tests_encoding;
} op_code_info;

#define INF_ARG 0xffff



static op_code_info dispatch_table[]= { 
#define _OP_DEF(B, C, D, E, OP) {B, C, D, E}, 
#include "s7-ops.h" 
  { 0, 0, 0, 0 } 
}; 

static const char *procedure_name(s7_pointer x) 
{
  int n = procedure_index(x);
  const char *name = dispatch_table[n].name;
  if (name == 0) 
    name ="ILLEGAL!";
  return(name);
}


/* -------------------------------- eval -------------------------------- */

/* all explicit write-* in eval assume current-output-port -- tracing, error fallback handling, etc */
/*   internal reads assume sc->inport is the input port */

static void eval(s7_scheme *sc, opcode_t first_op) 
{
  #define s_pop(sc, a)    {pop_stack(sc, a); goto START;}
  #define s_retbool(tf)   {pop_stack(sc, (tf) ? sc->T : sc->F); goto START;}
  /* TODO: get rid of s_pop|s_retbool */

  sc->op = first_op;

  START:
  {
    if (sc->op >= OP_DEFP)
      {
	op_code_info *pcd;
	pcd = dispatch_table + sc->op;
	
	if (pcd->name != 0) 
	  { /* if built-in function, check arguments */
	    char msg[512];
	    bool ok = true;
	    int n = s7_list_length(sc, sc->args);
	    
	    /* Check number of arguments */
	    if (n < pcd->min_arity) 
	      {
		ok = false;
		sprintf(msg, "%s: needs%s %d argument%s",
			pcd->name,
			pcd->min_arity == pcd->max_arity?"":" at least",
			pcd->min_arity,
			(pcd->min_arity != 1) ? "s" : "");
	      }
	    if (ok && n > pcd->max_arity) 
	      {
		ok = false;
		sprintf(msg, "%s: needs%s %d argument%s",
			pcd->name,
			pcd->min_arity== pcd->max_arity?"":" at most",
			pcd->max_arity,
			(pcd->max_arity != 1) ? "s" : "");
	      }
	    if (ok) 
	      {
		if (pcd->arg_tests_encoding != 0) 
		  {
		    s7_pointer arg;
		    int i = 0;
		    int j;
		    const char *t = pcd->arg_tests_encoding;
		    s7_pointer arglist = sc->args;
		    
		    do 
		      {
			arg = car(arglist);
			j = (int)t[0];
			if (!tests[j].fct(arg)) break;
			if (t[1] != 0) 
			  {/* last test is replicated as necessary */
			    t++;
			  }
			arglist = cdr(arglist);
			i++;
		      } 
		    while(i < n);
		    
		    if (i < n) 
		      {
			ok = false;
			sprintf(msg, "%s: argument %d must be: %s (%s)",
				pcd->name,
				i + 1,
				tests[j].kind,
				s7_object_to_c_string(sc, arg));
		      }
		  }
	      }
	    if (!ok) 
	      {
		sc->value = s7_make_string(sc, msg);
		goto ERROR;
	      }
	  }
      }
  }

  /* g_stacktrace(sc, sc->NIL); */

  switch (sc->op) 
    {

    case OP_PEVAL: /* eval */
      if (cdr(sc->args) != sc->NIL) 
	sc->envir = cadr(sc->args);
      sc->code = car(sc->args);
      goto EVAL;

    case OP_TOP_LEVEL: /* top level */
      if (is_input_port(sc->inport))
	port_paren_depth(sc->inport) = 0;
      
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
    case OP_READ_INTERNAL:       /* internal read */
      sc->tok = token(sc, sc->inport);
      if (sc->tok == TOK_EOF) 
	s_pop(sc, sc->value);
      goto READ_EXPRESSION;
      
      
      /* g_read(p) from C 
       *   read one expr, return it, let caller deal with input port setup 
       */
    case OP_READ_RETURN_EXPRESSION:
      return;


      /* (read p) from scheme
       *    "p" becomes current input port for eval's duration, then pops back before returning value into calling expr
       */
    case OP_READ_POP_AND_RETURN_EXPRESSION:
      pop_input_port(sc);
      if (sc->tok == TOK_EOF)
	s_pop(sc, sc->EOF_OBJECT);
      s_pop(sc, sc->value);
      

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
      s7_close_input_port(sc, sc->inport);
      pop_input_port(sc);
      s_pop(sc, sc->UNSPECIFIED);
      

      /* read and evaluate string expression(s?)
       *    assume caller (C via g_eval_c_string) is dealing with the string port
       */
    case OP_EVAL_STRING:
      push_stack(sc, OP_EVAL_STRING_DONE, sc->NIL, sc->NIL); /* TODO: multiple exprs in string?? */
      sc->code = sc->value;
      goto EVAL;

    case OP_EVAL_STRING_DONE:
      return;
      
    case OP_GENSYM:
      s_pop(sc, s7_gensym(sc));
      
      
    case OP_VALUEPRINT: /* print evaluation result */
      if (sc->tracing) 
	write_string(sc, "\nGives: ", sc->outport);
      s_pop(sc, sc->value);
      
      
    BEGIN:
    case OP_BEGIN:      /* begin */
      if (!s7_is_pair(sc->code)) 
	s_pop(sc, sc->code);
      
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
	  write_string(sc, "\nEval: ", sc->outport);
	  goto P0LIST;
	}
      /* fall through */
      
      
    case OP_REAL_EVAL:
      
      if (s7_is_symbol(sc->code)) 
	{
	  sc->x = s7_find_slot_in_env(sc, sc->envir, sc->code, 1);
	  if (sc->x != sc->NIL) 
	    s_pop(sc, s7_slot_value_in_env(sc->x)); 
	  
	  fprintf(stderr, "unbound: %s\n", s7_symbol_name(sc->code));
	  
	  sc->value = s7_make_string(sc, "eval: unbound variable:");
	  sc->args = sc->code;
	  goto ERROR;
	} 
      else 
	if (s7_is_pair(sc->code)) 
	  {
	    if (s7_is_syntax(sc->x = car(sc->code))) 
	      {     
		sc->code = cdr(sc->code);
		sc->op = integer(syntax_opcode(sc->x)->object.number);
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
	    s_pop(sc, sc->code);
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
	  write_string(sc, "\nApply to: ", sc->outport);
	  goto P0LIST;
	}
      /* fall through */
      
    case OP_REAL_APPLY:
      
      if (s7_is_procedure(sc->code)) 
	{
	  sc->op = procedure_index(sc->code);
	  goto START;
	} 
      else 
	{
	  if (s7_is_function(sc->code))
	    {
	      int len;
	      len = s7_list_length(sc, sc->args);
	      if (len < function_required_args(sc->code))
		{
		  sc->value = s7_make_string(sc, "not enough args");
		  goto ERROR;
		}
	      if ((!function_has_rest_arg(sc->code)) &&
		  ((function_required_args(sc->code) + function_optional_args(sc->code)) < len))
		{
		  sc->value = s7_make_string(sc, "too many args");
		  goto ERROR;
		}
	      sc ->x = function_call(sc->code)(sc, sc->args);
	      
	      if (sc->x == sc->ERROR)
		goto ERROR;
	      s_pop(sc, sc->x);
	      
	      /* so to throw an error from a function,
	       *   set sc->value to the error string, sc->args to the error args, return sc->ERROR
	       */
	    }
	  
	  /* TODO: if object getter */
	  
	  else 
	    {
	      if (s7_is_closure(sc->code) || is_macro(sc->code) || is_promise(sc->code)) 
		{ /* CLOSURE */
		  /* Should not accept promise */
		  /* make environment */
		  /*
		    fprintf(stderr, "proc env: %s\n", s7_object_to_c_string(sc, s7_procedure_environment(sc->code)));
		  */
		  new_frame_in_env(sc, s7_procedure_environment(sc->code)); 
		  
		  /* s7_is_pair(sc->x) here assumes the func has more than 1 arg? */
		  

		  /* load up the current args into the ((args) (lambda)) layout [via the current environment] */
		  for (sc->x = car(procedure_source(sc->code)), sc->y = sc->args; s7_is_pair(sc->x); sc->x = cdr(sc->x), sc->y = cdr(sc->y)) 
		    {
		      if (sc->y == sc->NIL)
			{
			  sc->value = s7_make_string(sc, "not enough arguments");
			  goto ERROR;
			}
		      else new_slot_in_env(sc, car(sc->x), car(sc->y));
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
			  sc->args = sc->x;
			  sc->value = s7_make_string(sc, "syntax error in closure: not a symbol:");
			  goto ERROR;
			}
		    }
		  sc->code = cdr(procedure_source(sc->code));
		  sc->args = sc->NIL;
		  goto BEGIN;
		}
	      else 
		{
		  if (s7_is_continuation(sc->code)) 
		    { /* continuation */
		      s7_call_continuation(sc, sc->code);
		      s_pop(sc, sc->args != sc->NIL ? car(sc->args) : sc->NIL);
		    } 
		  else 
		    {
		      if (is_goto(sc->code))
			{
			  sc->stack_top = (sc->code)->object.goto_loc;
			  s_pop(sc, sc->args != sc->NIL ? car(sc->args) : sc->NIL);
			}
		      else
			{
			  fprintf(stderr, "apply %s?", s7_object_to_c_string(sc, sc->code));
			  sc->args = sc->code;
			  sc->value = s7_make_string(sc, "illegal function");
			  goto ERROR;
			}
		    }
		}
	    }
	}
      
    case OP_DOMACRO:    /* do macro */
      sc->code = sc->value;
      goto EVAL;
      
    case OP_LAMBDA:     /* lambda */
      s_pop(sc, s7_make_closure(sc, sc->code, sc->envir));
      
    case OP_MKCLOSURE: /* make-closure */
      sc->x = car(sc->args);
      if (car(sc->x) == sc->LAMBDA) 
	sc->x = cdr(sc->x);
      
      if (cdr(sc->args) == sc->NIL) 
	sc->y = sc->envir;
      else sc->y = cadr(sc->args);
      
      s_pop(sc, s7_make_closure(sc, sc->x, sc->y));
      
    case OP_QUOTE:      /* quote */
      s_pop(sc, car(sc->code));
      
    case OP_DEF0:  /* define */
      if (s7_is_immutable(car(sc->code)))
	{
	  sc->value = s7_make_string(sc, "define: unable to alter immutable");
	  sc->args = car(sc->code);
	  goto ERROR;
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
	  sc->value = s7_make_string(sc, "define of a non-symbol?");
	  sc->args = sc->x;
	  goto ERROR;
	}
      
      push_stack(sc, OP_DEF1, sc->NIL, sc->x);
      goto EVAL;
      
    case OP_DEF1:  /* define */
      sc->x = s7_find_slot_in_env(sc, sc->envir, sc->code, 0);
      if (sc->x != sc->NIL) 
	set_slot_in_env(sc, sc->x, sc->value); 
      else new_slot_in_env(sc, sc->code, sc->value); 
      s_pop(sc, sc->code);
      
    case OP_DEFP:  /* defined? */
      sc->x = sc->envir;
      if (cdr(sc->args) != sc->NIL) 
	sc->x = cadr(sc->args);
      s_retbool(s7_find_slot_in_env(sc, sc->x, car(sc->args), 1) != sc->NIL);
      
    case OP_SET0:       /* set! */
      if (s7_is_immutable(car(sc->code)))
	{
	  sc->value = s7_make_string(sc, "set!: unable to alter immutable variable");
	  sc->args = car(sc->code);
	  goto ERROR;
	}
      
      if (s7_is_pair(car(sc->code))) /* has accessor */
	{
	  char *sym, *name;
	  name = s7_symbol_name(caar(sc->code));
	  sym = (char *)CALLOC(safe_strlen(name) + 6, sizeof(char));
	  sprintf(sym, "set-%s", name);
	  caar(sc->code) = s7_make_symbol(sc, sym);               /* [set!] ((x a b...) y) -> ((set-x a b..) y) */
	  sc->code = s7_append(sc, car(sc->code), cdr(sc->code)); /* -> (set-x a b ... y) */
	  FREE(sym); /* I think s7_make_symbol copies the name */
	}
      else 
	{
	  push_stack(sc, OP_SET1, sc->NIL, car(sc->code));
	  sc->code = cadr(sc->code);
	}
      goto EVAL;
      
    case OP_SET1:      
      sc->y = s7_find_slot_in_env(sc, sc->envir, sc->code, 1);
      if (sc->y != sc->NIL) 
	{
	  set_slot_in_env(sc, sc->y, sc->value); 
	  s_pop(sc, sc->value);
	}
      else 
	{
	  sc->value = s7_make_string(sc, "set!: unbound variable:");
	  sc->args = sc->code;
	  goto ERROR;
	}
      
    case OP_IF0:        /* if */
      
      push_stack(sc, OP_IF1, sc->NIL, cdr(sc->code));
      sc->code = car(sc->code);
      goto EVAL;
      
    case OP_IF1:        /* if */
      
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
	  sc->value = s7_make_string(sc, "syntax error in cond");
	  goto ERROR;
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
	      s_pop(sc, sc->value);
	    }
	  if (car(sc->code) == sc->FEED_TO) 
	    {
	      if (!s7_is_pair(cdr(sc->code))) 
		{
		  sc->value = s7_make_string(sc, "syntax error in cond");
		  goto ERROR;
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
	      s_pop(sc, sc->NIL);
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
      s_pop(sc, sc->x);
      
    case OP_AND0:       /* and */
      if (sc->code == sc->NIL) 
	s_pop(sc, sc->T);
      
      push_stack(sc, OP_AND1, sc->NIL, cdr(sc->code));
      sc->code = car(sc->code);
      goto EVAL;
      
    case OP_AND1:       /* and */
      if (is_false(sc->value)) 
	s_pop(sc, sc->value);
      
      if (sc->code == sc->NIL) 
	s_pop(sc, sc->value);
      
      push_stack(sc, OP_AND1, sc->NIL, cdr(sc->code));
      sc->code = car(sc->code);
      goto EVAL;
      
    case OP_OR0:        /* or */
      if (sc->code == sc->NIL) 
	s_pop(sc, sc->F);
      
      push_stack(sc, OP_OR1, sc->NIL, cdr(sc->code));
      sc->code = car(sc->code);
      goto EVAL;
      
    case OP_OR1:        /* or */
      if (is_true(sc->value)) 
	s_pop(sc, sc->value);
      
      if (sc->code == sc->NIL) 
	s_pop(sc, sc->value);
      
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
      s_pop(sc,cons(sc, sc->args, sc->x));
      

    case OP_MACRO0:     /* macro */
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
	  sc->value = s7_make_string(sc, "variable is not a symbol");
	  sc->args = sc->x;
	  goto ERROR;
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
      /*
      fprintf(stderr, "macro def: %s %s\n", 
	      s7_object_to_c_string(sc, sc->code), 
	      s7_object_to_c_string(sc, car(sc->value)));
      */
      set_type(sc->value, T_MACRO);
      
      /* find name in environment, and define it */
      sc->x = s7_find_slot_in_env(sc, sc->envir, sc->code, 0); 
      if (sc->x != sc->NIL) 
	set_slot_in_env(sc, sc->x, sc->value); 
      else new_slot_in_env(sc, sc->code, sc->value); 
      
      /* pop back to wherever the macro call was */
      s_pop(sc, sc->code);


    case OP_DEFMACRO0:
      
      /* (defmacro name (args) body) ->
       *
       *    (macro (defmacro dform)
       *      (let ((form (gensym)))            
       *        `(macro (,(cadr dform) ,form)   
       *          (apply
       *            (lambda ,(caddr dform)      
       *             ,@(cdddr dform))          
       *            (cdr ,form)))))             
       *    
       *    end up with name as sc->x going to OP_MACRO1, ((gensym) (lambda (args) body) going to eval
       */
      
      /*
      fprintf(stderr, "macro start: %s %s\n", 
	      s7_object_to_c_string(sc, car(sc->code)), 
	      s7_object_to_c_string(sc, cddr(sc->value)));
      */

      sc->y = s7_gensym(sc);
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
      /* what could be simpler? */

      /*
      fprintf(stderr, "macro mid: %s\n", 
	      s7_object_to_c_string(sc, sc->code));
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
	    {
	      break;
	    }
	  for ( ; sc->y != sc->NIL; sc->y = cdr(sc->y)) 
	    {
	      if (s7_eqv_p(car(sc->y), sc->value)) 
		{
		  break;
		}
	    }
	  if (sc->y != sc->NIL) 
	    {
	      break;
	    }
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
	  s_pop(sc, sc->NIL);
	}
      
    case OP_CASE2:      /* case */
      if (is_true(sc->value)) 
	{
	  goto BEGIN;
	} 
      else 
	{
	  s_pop(sc, sc->NIL);
	}
      
    case OP_PAPPLY:     /* apply */
      sc->code = car(sc->args);
      sc->args = list_star(sc, cdr(sc->args));
      goto APPLY;
      
    case OP_CONTINUATION:    /* call-with-current-continuation */
      sc->code = car(sc->args);
      sc->args = cons(sc, s7_make_continuation(sc), sc->NIL);
      /*
	fprintf(stderr, "call/cc... %p %p %p ", sc->code, sc->stack, car(sc->args));
      */
      goto APPLY;
      
    case OP_GOTO:    /* call-with-exit */
      sc->code = car(sc->args);
      sc->args = cons(sc, s7_make_goto(sc), sc->NIL);
      goto APPLY;
      
      
    case OP_REAL_PART:     /* real-part */
      s_pop(sc, s7_make_real(sc, s7_real_part(car(sc->args))));
      
    case OP_IMAG_PART:     /* imag-part */
      s_pop(sc, s7_make_real(sc, s7_imag_part(car(sc->args))));
      
    case OP_NUMERATOR:     /* numerator */
      s_pop(sc, s7_make_integer(sc, num_to_numerator((car(sc->args))->object.number)));
      
    case OP_DENOMINATOR:     /* denominator */
      s_pop(sc, s7_make_integer(sc, num_to_denominator((car(sc->args))->object.number)));
      
    case OP_MAKE_RECTANGULAR:     /* make-rectangular */
      s_pop(sc, s7_make_complex(sc, num_to_real((car(sc->args))->object.number), 
				num_to_real((cadr(sc->args))->object.number)));
      
    case OP_MAKE_POLAR:
      {
	double s, c, ang, mag;
	mag = num_to_real((car(sc->args))->object.number);
	ang = num_to_real((cadr(sc->args))->object.number);
	s = sin(ang);
	c = cos(ang);
	s_pop(sc, s7_make_complex(sc, mag * c, mag * s));
      }
      
    case OP_ANGLE:
      sc->x = car(sc->args);
      
      if (!s7_is_real(sc->x))
	s_pop(sc, s7_make_real(sc, atan2(s7_imag_part(sc->x), s7_real_part(sc->x))));
      if (num_to_real(sc->x->object.number) < 0.0)
	s_pop(sc, s7_make_real(sc, atan2(0.0, -1.0)));
      
      s_pop(sc, s7_make_real(sc, 0.0));
      
    case OP_MAGNITUDE:
      sc->v = nvalue(car(sc->args));
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
      s_pop(sc, make_number(sc, sc->v));
      
    case OP_EXACTP:     /* exact? */
      s_retbool(s7_is_exact(car(sc->args)));
      
    case OP_INEXACTP:     /* exact? */
      s_retbool(s7_is_inexact(car(sc->args)));
      
    case OP_EVENP:     /* even? */
      s_retbool((s7_integer(car(sc->args)) & 1) == 0);
      
    case OP_ODDP:     /* odd? */
      s_retbool((s7_integer(car(sc->args)) & 1) == 1);
      
    case OP_POSITIVEP:     /* positive? */
      sc->x = car(sc->args);
      switch (object_number_type(sc->x))
	{
	case NUM_INT:   s_retbool(s7_integer(sc->x) > 0);
	case NUM_RATIO: s_retbool(s7_numerator(sc->x) > 0);
	default:        s_retbool(s7_real(sc->x) > 0);
	}
      
    case OP_NEGATIVEP:     /* negative? */
      sc->x = car(sc->args);
      switch (object_number_type(sc->x))
	{
	case NUM_INT:   s_retbool(s7_integer(sc->x) < 0);
	case NUM_RATIO: s_retbool(s7_numerator(sc->x) < 0);
	default:        s_retbool(s7_real(sc->x) < 0);
	}
      
    case OP_ZEROP:     /* zero? */
      sc->x = car(sc->args);
      switch (object_number_type(sc->x))
	{
	case NUM_INT:   s_retbool(s7_integer(sc->x) == 0);
	case NUM_REAL2:
	case NUM_REAL:  s_retbool(s7_real(sc->x) == 0.0);
	case NUM_RATIO: s_retbool(s7_numerator(sc->x) == 0);
	default:        s_retbool((s7_real_part(sc->x) == 0.0) &&
				  (s7_imag_part(sc->x) == 0.0));
	}
      
    case OP_RATIONALIZE:    /* rationalize */
      {
	double err;
	s7_Int numer = 0, denom = 1;
	
	sc->x = car(sc->args);
	if (s7_is_exact(sc->x)) 
	  s_pop(sc, sc->x);
	
	err = s7_real(cadr(sc->args));
	
	if (c_rationalize(s7_real(sc->x), err, &numer, &denom))
	  s_pop(sc, s7_make_ratio(sc, numer, denom))
	  else 
	    {
	      sc->value = s7_make_string(sc, "rationalize did not converge?!?");
	      sc->args = sc->x;
	      goto ERROR;
	    }
      }
      
    case OP_INEX2EX:    /* inexact->exact */
      sc->x = car(sc->args);
      if (s7_is_exact(sc->x)) 
	s_pop(sc, sc->x);
      {
	s7_Int numer = 0, denom = 1;
	if (c_rationalize(s7_real(sc->x), DEFAULT_RATIONALIZE_ERROR, &numer, &denom))
	  s_pop(sc, s7_make_ratio(sc, numer, denom))
	  else s_pop(sc, s7_make_integer(sc, (s7_Int)s7_real(sc->x)));
      }
      
    case OP_EX2INEX:    /* exact->inexact */
      sc->x = car(sc->args);
      if (s7_is_inexact(sc->x)) 
	s_pop(sc, sc->x);
      
      if (s7_is_integer(sc->x))
	s_pop(sc, s7_make_real(sc, (double)s7_integer(sc->x)))
	else s_pop(sc, s7_make_real(sc, fraction(sc->x->object.number)));
      
      
    case OP_ABS:
      sc->v = nvalue(car(sc->args));
      if (num_type(sc->v) >= NUM_REAL)
	real(sc->v) = fabs(real(sc->v));
      else
	{
	  if (num_type(sc->v) == NUM_INT)
	    integer(sc->v) = abs(integer(sc->v));
	  else numerator(sc->v) = abs(numerator(sc->v));
	}
      s_pop(sc, make_number(sc, sc->v));      
      
    case OP_EXP:
      sc->x = car(sc->args);
      if (s7_is_real(sc->x))
	s_pop(sc, s7_make_real(sc, exp(num_to_real(sc->x->object.number))));
      s_pop(sc, s7_from_c_complex(sc, cexp(s7_complex(sc->x))));
      
    case OP_LOG:
      
      /* TODO: cadr can be base */
      
      sc->x = car(sc->args);
      if ((s7_is_real(sc->x)) &&
	  (num_to_real(sc->x->object.number) > 0.0))
	s_pop(sc, s7_make_real(sc, log(num_to_real(sc->x->object.number))));
      /* if < 0 use log(-x) + pi*i */
      s_pop(sc, s7_from_c_complex(sc, clog(s7_complex(sc->x))));
      
    case OP_SIN:
      sc->x = car(sc->args);
      if (s7_is_real(sc->x))
	s_pop(sc, s7_make_real(sc, sin(num_to_real(sc->x->object.number))));
      s_pop(sc, s7_from_c_complex(sc, csin(s7_complex(sc->x))));
      
    case OP_COS:
      sc->x = car(sc->args);
      if (s7_is_real(sc->x))
	s_pop(sc, s7_make_real(sc, cos(num_to_real(sc->x->object.number))));
      s_pop(sc, s7_from_c_complex(sc, ccos(s7_complex(sc->x))));
      
    case OP_TAN:
      sc->x = car(sc->args);
      if (s7_is_real(sc->x))
	s_pop(sc, s7_make_real(sc, tan(num_to_real(sc->x->object.number))));
      s_pop(sc, s7_from_c_complex(sc, ctan(s7_complex(sc->x))));
      
    case OP_ASIN:
      sc->x = car(sc->args);
      if ((s7_is_real(sc->x)) &&
	  (fabs(num_to_real(sc->x->object.number)) <= 1.0))
	s_pop(sc, s7_make_real(sc, asin(num_to_real(sc->x->object.number))));
      s_pop(sc, s7_from_c_complex(sc, casin(s7_complex(sc->x))));
      
    case OP_ACOS:
      sc->x = car(sc->args);
      if ((s7_is_real(sc->x)) &&
	  (fabs(num_to_real(sc->x->object.number)) <= 1.0))
	s_pop(sc, s7_make_real(sc, acos(num_to_real(sc->x->object.number))));
      s_pop(sc, s7_from_c_complex(sc, cacos(s7_complex(sc->x))));
      
    case OP_ATAN:
      sc->x = car(sc->args);
      if (cdr(sc->args) == sc->NIL) 
	{
	  if (s7_is_real(sc->x))
	    s_pop(sc, s7_make_real(sc, atan(num_to_real(sc->x->object.number))));
	  s_pop(sc, s7_from_c_complex(sc, catan(s7_complex(sc->x))));
	} 
      else 
	{
	  sc->y = cadr(sc->args);
	  s_pop(sc, s7_make_real(sc, atan2(num_to_real(sc->x->object.number), num_to_real(sc->y->object.number))));
	  /* atan2 args should be real */
	}
      
    case OP_SINH:
      sc->x = car(sc->args);
      if (s7_is_real(sc->x))
	s_pop(sc, s7_make_real(sc, sinh(num_to_real(sc->x->object.number))));
      s_pop(sc, s7_from_c_complex(sc, csinh(s7_complex(sc->x))));
      
    case OP_COSH:
      sc->x = car(sc->args);
      if (s7_is_real(sc->x))
	s_pop(sc, s7_make_real(sc, cosh(num_to_real(sc->x->object.number))));
      s_pop(sc, s7_from_c_complex(sc, ccosh(s7_complex(sc->x))));
      
    case OP_TANH:
      sc->x = car(sc->args);
      if (s7_is_real(sc->x))
	s_pop(sc, s7_make_real(sc, tanh(num_to_real(sc->x->object.number))));
      s_pop(sc, s7_from_c_complex(sc, ctanh(s7_complex(sc->x))));
      
    case OP_ASINH:
      sc->x = car(sc->args);
      if (s7_is_real(sc->x))
	s_pop(sc, s7_make_real(sc, asinh(num_to_real(sc->x->object.number))));
      s_pop(sc, s7_from_c_complex(sc, casinh(s7_complex(sc->x))));
      
    case OP_ACOSH:
      sc->x = car(sc->args);
      if ((s7_is_real(sc->x)) &&
	  (num_to_real(sc->x->object.number) >= 1.0))
	s_pop(sc, s7_make_real(sc, acosh(num_to_real(sc->x->object.number))));
      s_pop(sc, s7_from_c_complex(sc, cacosh(s7_complex(sc->x))));
      
    case OP_ATANH:
      sc->x = car(sc->args);
      if ((s7_is_real(sc->x)) &&
	  (fabs(num_to_real(sc->x->object.number)) < 1.0))
	s_pop(sc, s7_make_real(sc, atanh(num_to_real(sc->x->object.number))));
      s_pop(sc, s7_from_c_complex(sc, catanh(s7_complex(sc->x))));
      
    case OP_SQRT:
      sc->x = car(sc->args);
      if ((s7_is_real(sc->x)) &&
	  (num_to_real(sc->x->object.number) >= 0.0))
	s_pop(sc, s7_make_real(sc, sqrt(num_to_real(sc->x->object.number))));
      /* if < 0 use sqrt(-num)*i */
      s_pop(sc, s7_from_c_complex(sc, csqrt(s7_complex(sc->x))));
      
    case OP_EXPT:
      {
	sc->x = car(sc->args);
	sc->y = cadr(sc->args);
	
	if ((s7_is_integer(sc->x)) &&
	    (s7_is_integer(sc->y)) &&
	    ((s7_integer(sc->y) >= 0) || 
	     (abs(s7_integer(sc->x)) == 1)))
	  s_pop(sc, s7_make_integer(sc, (s7_Int)pow(s7_integer(sc->x), s7_integer(sc->y))));
	
	if ((s7_is_real(sc->x)) &&
	    (s7_is_real(sc->y)) &&
	    (num_to_real(sc->x->object.number) > 0.0) &&
	    (num_to_real(sc->y->object.number) >= 0.0))
	  s_pop(sc, s7_make_real(sc, pow(num_to_real(sc->x->object.number), num_to_real(sc->y->object.number))));
	
	s_pop(sc, s7_from_c_complex(sc, cpow(s7_complex(sc->x), s7_complex(sc->y))));
      }
      
    case OP_FLOOR:
      sc->x = car(sc->args);
      s_pop(sc, s7_make_integer(sc, (s7_Int)floor(num_to_real(sc->x->object.number)))); /* used to be real result */
      
    case OP_CEILING:
      sc->x = car(sc->args);
      s_pop(sc, s7_make_integer(sc, (s7_Int)ceil(num_to_real(sc->x->object.number))));
      
    case OP_ROUND: 
      sc->x = car(sc->args);
      s_pop(sc, s7_make_integer(sc, (s7_Int)round_per_R5RS(num_to_real(sc->x->object.number))));
      
    case OP_TRUNCATE: 
      {
	double xf;
	sc->x = car(sc->args);
	xf = num_to_real(sc->x->object.number);
	if (xf > 0) 
	  s_pop(sc, s7_make_integer(sc, (s7_Int)floor(xf)));
	s_pop(sc, s7_make_integer(sc, (s7_Int)ceil(xf)));
      }
      
    case OP_LCM:
      { /* TODO: fix (and gcd) for new case */
	s7_Int val = 1;
	for (sc->x = sc->args; sc->x != sc->NIL; sc->x = cdr(sc->x)) 
	  {
	    val = c_lcm(val, s7_integer(car(sc->x)));
	    if (val == 0)
	      s_pop(sc, s7_make_integer(sc, 0));
	  }
	s_pop(sc, s7_make_integer(sc, val));
      }
      
    case OP_GCD:
      {
	s7_Int val = 0;
	for (sc->x = sc->args; sc->x != sc->NIL; sc->x = cdr(sc->x)) 
	  {
	    val = c_gcd(val, s7_integer(car(sc->x)));
	    if (val == 1)
	      s_pop(sc, s7_make_integer(sc, 1));
	  }
	s_pop(sc, s7_make_integer(sc, val));
      }
      
    case OP_MAX:        /* max */
      sc->v = nvalue(car(sc->args));
      for (sc->x = cdr(sc->args); sc->x != sc->NIL; sc->x = cdr(sc->x)) 
	sc->v = num_max(sc, sc->v, nvalue(car(sc->x)));
      s_pop(sc, make_number(sc, sc->v));
      
    case OP_MIN:        /* min */
      sc->v = nvalue(car(sc->args));
      for (sc->x = cdr(sc->args); sc->x != sc->NIL; sc->x = cdr(sc->x)) 
	sc->v = num_min(sc, sc->v, nvalue(car(sc->x)));
      s_pop(sc, make_number(sc, sc->v));
      
    case OP_ADD:        /* + */
      sc->v = small_int_as_num(sc, 0);
      
      for (sc->x = sc->args; sc->x != sc->NIL; sc->x = cdr(sc->x)) 
	sc->v = num_add(sc, sc->v, nvalue(car(sc->x)));
      s_pop(sc, make_number(sc, sc->v));
      
    case OP_MUL:        /* * */
      sc->v = small_int_as_num(sc, 1);
      
      for (sc->x = sc->args; sc->x != sc->NIL; sc->x = cdr(sc->x)) 
	sc->v = num_mul(sc, sc->v, nvalue(car(sc->x)));
      s_pop(sc, make_number(sc, sc->v));
      
    case OP_SUB:        /* - */
      if (cdr(sc->args) == sc->NIL) 
	{
	  sc->x = sc->args;
	  sc->v = small_int_as_num(sc, 0);
	} 
      else 
	{
	  sc->x = cdr(sc->args);
	  sc->v = nvalue(car(sc->args));
	}
      for (; sc->x != sc->NIL; sc->x = cdr(sc->x)) 
	sc->v = num_sub(sc, sc->v, nvalue(car(sc->x)));
      s_pop(sc, make_number(sc, sc->v));
      
    case OP_DIV:        /* / */
      if (cdr(sc->args) == sc->NIL) 
	{
	  sc->x = sc->args;
	  sc->v = small_int_as_num(sc, 1);
	} 
      else 
	{
	  sc->x = cdr(sc->args);
	  sc->v = nvalue(car(sc->args));
	}
      for (; sc->x != sc->NIL; sc->x = cdr(sc->x)) 
	sc->v = num_div(sc, sc->v, nvalue(car(sc->x)));
      s_pop(sc, make_number(sc, sc->v));
      
    case OP_QUOTIENT:        /* quotient */
      if (cdr(sc->args) == sc->NIL) 
	{
	  sc->x = sc->args;
	  sc->v = small_int_as_num(sc, 1);
	} 
      else 
	{
	  sc->x = cdr(sc->args);
	  sc->v = nvalue(car(sc->args));
	}
      for (; sc->x != sc->NIL; sc->x = cdr(sc->x)) 
	{
	  if (s7_integer(car(sc->x)) != 0)
	    sc->v = num_quotient(sc->v, nvalue(car(sc->x)));
	  else 
	    {
	      sc->value = s7_make_string(sc, "quotient: division by zero");
	      goto ERROR;
	    }
	}
      s_pop(sc,make_number(sc, sc->v));
      
    case OP_REM:        /* remainder */
      sc->v = nvalue(car(sc->args));
      if (s7_integer(cadr(sc->args)) != 0)
	sc->v = num_rem(sc->v, nvalue(cadr(sc->args)));
      else 
	{
	  sc->value = s7_make_string(sc, "remainder: division by zero");
	  goto ERROR;
	}
      
      s_pop(sc,make_number(sc, sc->v));
      
    case OP_MOD:        /* modulo */
      sc->v = nvalue(car(sc->args));
      if (s7_integer(cadr(sc->args)) != 0)
	sc->v = num_mod(sc->v, nvalue(cadr(sc->args)));
      else 
	{
	  sc->value = s7_make_string(sc, "modulo: division by zero");
	  goto ERROR;
	}
      s_pop(sc, make_number(sc, sc->v));
      
    case OP_CAR:        /* car */
      s_pop(sc, caar(sc->args));
      
    case OP_CDR:        /* cdr */
      s_pop(sc, cdar(sc->args));
      
    case OP_CONS:       /* cons */
      cdr(sc->args) = cadr(sc->args);
      s_pop(sc, sc->args);
      
    case OP_SETCAR:     /* set-car! */
      if (!s7_is_immutable(car(sc->args))) 
	{
	  caar(sc->args) = cadr(sc->args);
	  s_pop(sc, car(sc->args));
	} 
      else 
	{
	  sc->value = s7_make_string(sc, "set-car!: unable to alter immutable pair");
	  goto ERROR;
	}
      
    case OP_SETCDR:     /* set-cdr! */
      if (!s7_is_immutable(car(sc->args))) 
	{
	  cdar(sc->args) = cadr(sc->args);
	  s_pop(sc, car(sc->args));
	} 
      else 
	{
	  sc->value = s7_make_string(sc, "set-cdr!: unable to alter immutable pair");
	  goto ERROR;
	}
      
    case OP_CHAR2INT:  /* char->integer */
      { 
	char c;
	c = (char)character(car(sc->args));
	s_pop(sc, s7_make_integer(sc, (unsigned char)c));
      }
      
    case OP_INT2CHAR: /* integer->char */
      { 
	unsigned char c;
	c = (unsigned char)s7_integer(car(sc->args)); /* int here -> char */
	s_pop(sc, s7_make_character(sc, (char)c));
      }
      
    case OP_CHARUPCASE: 
      {
	unsigned char c;
	c = (unsigned char)character(car(sc->args));
	c = toupper(c);
	s_pop(sc, s7_make_character(sc, (char)c));
      }
      
    case OP_CHARDNCASE: 
      {
	unsigned char c;
	c = (unsigned char)character(car(sc->args));
	c = tolower(c);
	s_pop(sc, s7_make_character(sc, (char)c));
      }
      
    case OP_STR2SYM:  /* string->symbol */
      s_pop(sc, s7_make_symbol(sc, string_value(car(sc->args))));
      
    case OP_STRING_TO_NUMBER:
      {
	char *s = string_value(car(sc->args));
	sc->x = make_atom(sc, s);
	if (s7_is_number(sc->x))
	  s_pop(sc, sc->x)
	  else s_pop(sc, sc->F);
      }
      
    case OP_SYM2STR: /* symbol->string */
      sc->x = s7_make_string(sc, s7_symbol_name(car(sc->args)));
      s7_set_immutable(sc->x);
      s_pop(sc, sc->x);
      
    case OP_NUMBER_TO_STRING:
      {
	/* TODO if cdr not nil, we've got a radix */
	s_pop(sc, s7_make_string(sc, s7_atom_to_c_string(sc, car(sc->args), false)));
      } 
      
    case OP_MKSTRING: /* make-string */
      { 
	char fill = ' ';
	int len;
	
	len = s7_integer(car(sc->args));
	if (cdr(sc->args) != sc->NIL) 
	  fill = s7_character(cadr(sc->args));
	
	s_pop(sc, make_empty_string(sc, len, (char)fill));
      }
      
    case OP_STRLEN:  /* string-length */
      s_pop(sc, s7_make_integer(sc, string_length(car(sc->args))));
      
    case OP_STRREF:  /* string-ref */
      {
	char *str;
	int index;
	
	str = string_value(car(sc->args));
	index = s7_integer(cadr(sc->args));
	if (index >= string_length(car(sc->args))) 
	  {
	    sc->value = s7_make_string(sc, "string-ref: out of bounds:");
	    goto ERROR;
	  }
	
	s_pop(sc, s7_make_character(sc, ((unsigned char*)str)[index]));
      }
      
    case OP_STRSET: /* string-set! */
      { 
	char *str;
	int index;
	char c;
	
	if (s7_is_immutable(car(sc->args))) 
	  {
	    sc->value = s7_make_string(sc, "string-set!: unable to alter immutable string:");
	    goto ERROR;
	  }
	
	str = string_value(car(sc->args));
	index = s7_integer(cadr(sc->args));
	if (index >= string_length(car(sc->args))) 
	  {
	    sc->value = s7_make_string(sc, "string-set!: out of bounds:");
	    goto ERROR;
	  }
	
	c = s7_character(caddr(sc->args));
	str[index] = (char)c;
	s_pop(sc, car(sc->args));
      }
      
    case OP_STRAPPEND: /* string-append */
      { 
	/* in 1.29 string-append was in Scheme in init.scm but was too slow */
	int len = 0;
	s7_pointer newstr;
	char *pos;
	
	/* compute needed length for new string */
	for (sc->x = sc->args; sc->x != sc->NIL; sc->x = cdr(sc->x)) 
	  len += string_length(car(sc->x));
	
	newstr = make_empty_string(sc, len, ' ');
	/* store the contents of the argument strings into the new string */
	for (pos = string_value(newstr), sc->x = sc->args; sc->x != sc->NIL;
	     pos += string_length(car(sc->x)), sc->x = cdr(sc->x)) 
	  memcpy(pos, string_value(car(sc->x)), string_length(car(sc->x)));
	
	s_pop(sc, newstr);
      }
      
    case OP_SUBSTR: { /* substring */
      char *str;
      int index0;
      int index1;
      int len;
      
      str = string_value(car(sc->args));
      
      index0 = s7_integer(cadr(sc->args));
      
      if (index0 > string_length(car(sc->args))) 
	{
	  sc->value = s7_make_string(sc, "substring: start out of bounds:");
	  goto ERROR;
	}
      
      if (cddr(sc->args)!= sc->NIL) 
	{
	  index1 = s7_integer(caddr(sc->args));
	  if (index1 > string_length(car(sc->args)) || index1 < index0) 
	    {
	      sc->value = s7_make_string(sc, "substring: end out of bounds:");
	      goto ERROR;
	    }
	} 
      else 
	{
	  index1 = string_length(car(sc->args));
	}
      
      len=index1 - index0;
      sc->x = make_empty_string(sc, len,' ');
      memcpy(string_value(sc->x), str + index0, len);
      string_value(sc->x)[len] = 0;
      
      s_pop(sc, sc->x);
    }
      
    VECTOR:
    case OP_VECTOR: /* vector */
      {   
	int i;
	s7_pointer vec;
	int len = s7_list_length(sc, sc->args);
	if (len < 0) 
	  {
	    sc->value = s7_make_string(sc, "vector: not a proper list:");
	    goto ERROR;
	  }
	
	vec = s7_make_vector(sc, len);
	for (sc->x = sc->args, i = 0; s7_is_pair(sc->x); sc->x = cdr(sc->x), i++) 
	  vector_element(vec, i) =  car(sc->x);
	
	s_pop(sc,vec);
      }
      
    case OP_MKVECTOR: /* make-vector */
      { 
	s7_pointer fill= sc->NIL;
	int len;
	s7_pointer vec;
	
	len = s7_integer(car(sc->args));
	
	if (cdr(sc->args) != sc->NIL) 
	  fill = cadr(sc->args);
	
	vec = s7_make_vector(sc, len);
	if (fill != sc->NIL)
	  s7_fill_vector(vec, fill);
	
	s_pop(sc,vec);
      }
      
    case OP_VECLEN:  /* vector-length */
      s_pop(sc, s7_make_integer(sc, vector_length(car(sc->args))));
      
    case OP_VECREF: /* vector-ref */
      { 
	int index;
	index = s7_integer(cadr(sc->args));
	
	if (index >= vector_length(car(sc->args))) 
	  {
	    sc->value = s7_make_string(sc, "vector-ref: out of bounds:");
	    goto ERROR;
	  }
	
	s_pop(sc, vector_element(car(sc->args), index));
      }
      
    case OP_VECSET:/* vector-set! */
      {   
	int index;
	
	if (s7_is_immutable(car(sc->args))) 
	  {
	    sc->value = s7_make_string(sc, "vector-set!: unable to alter immutable vector:");
	    goto ERROR;
	  }
	
	index = s7_integer(cadr(sc->args));
	if (index >= vector_length(car(sc->args))) 
	  {
	    sc->value = s7_make_string(sc, "vector-set!: out of bounds:");
	    goto ERROR;
	  }
	
	vector_element(car(sc->args), index) = caddr(sc->args);
	s_pop(sc, car(sc->args));
      }
      
      
    case OP_NOT:        /* not */
      s_retbool(is_false(car(sc->args)));
      
    case OP_BOOLP:       /* boolean? */
      s_retbool(car(sc->args) == sc->F || car(sc->args) == sc->T);
      
    case OP_NULLP:       /* null? */
      s_retbool(car(sc->args) == sc->NIL);
      
    case OP_NUMEQ:      /* = */
    case OP_LESS:       /* < */
    case OP_GRE:        /* > */
    case OP_LEQ:        /* <= */
    case OP_GEQ:        /* >= */
      {
	bool (*comp_func)(num a, num b);
	switch(sc->op) 
	  {
	  case OP_NUMEQ: comp_func = num_eq; break;
	  case OP_LESS:  comp_func = num_lt; break;
	  case OP_GRE:   comp_func = num_gt; break;
	  case OP_LEQ:   comp_func = num_le; break;
	  case OP_GEQ:   comp_func = num_ge; break;
	  default: fprintf(stderr, "oops");  break;
	  }
	sc->x = sc->args;
	sc->v = nvalue(car(sc->x));
	sc->x = cdr(sc->x);
	
	for (; sc->x != sc->NIL; sc->x = cdr(sc->x)) 
	  {
	    if (!comp_func(sc->v, nvalue(car(sc->x)))) 
	      s_retbool(false);
	    sc->v = nvalue(car(sc->x));
	  }
	
	s_retbool(true);
      }
      
    case OP_SYMBOLP:     /* symbol? */
      s_retbool(s7_is_symbol(car(sc->args)));
      
    case OP_NUMBERP:     /* number? */
      s_retbool(s7_is_number(car(sc->args)));
      
    case OP_STRINGP:     /* string? */
      s_retbool(s7_is_string(car(sc->args)));
      
    case OP_INTEGERP:     /* integer? */
      s_retbool(s7_is_integer(car(sc->args)));
      
    case OP_REALP:     /* real? */
      s_retbool(s7_is_real(car(sc->args))); 
      
    case OP_RATIONALP:     /* rational? */
      s_retbool(s7_is_rational(car(sc->args)));
      
    case OP_COMPLEXP:     /* complex? */
      s_retbool(s7_is_complex(car(sc->args)));
      
    case OP_CHARP:     /* char? */
      s_retbool(s7_is_character(car(sc->args)));
      
    case OP_CHARAP:     /* char-alphabetic? */
      s_retbool(Cisalpha(character(car(sc->args))));
      
    case OP_CHARNP:     /* char-numeric? */
      s_retbool(Cisdigit(character(car(sc->args))));
      
    case OP_CHARWP:     /* char-whitespace? */
      s_retbool(Cisspace(character(car(sc->args))));
      
    case OP_CHARUP:     /* char-upper-case? */
      s_retbool(Cisupper(character(car(sc->args))));
      
    case OP_CHARLP:     /* char-lower-case? */
      s_retbool(Cislower(character(car(sc->args))));
      
    case OP_PROCP:       /* procedure? */
      /*--
       * continuation should be procedure by the example
       * (call-with-current-continuation procedure?) ==> #t
       * in R^3 report sec. 6.9
       */
      s_retbool(s7_is_procedure(car(sc->args)) || 
		s7_is_closure(car(sc->args)) || 
		(is_goto(car(sc->args))) || 
		s7_is_continuation(car(sc->args)) || 
		s7_is_function(car(sc->args)));
      
    case OP_PAIRP:       /* pair? */
      s_retbool(s7_is_pair(car(sc->args)));
      
    case OP_LISTP:  /* list? */
      {    
	s7_pointer slow, fast;
	slow = fast = car(sc->args);
	while (1) 
	  {
	    if (!s7_is_pair(fast)) s_retbool(fast == sc->NIL);
	    fast = cdr(fast);
	    if (!s7_is_pair(fast)) s_retbool(fast == sc->NIL);
	    fast = cdr(fast);
	    slow = cdr(slow);
	    if (fast == slow) 
	      {
		/* the fast s7_pointer has looped back around and caught up
		   with the slow s7_pointer, hence the structure is circular,
		   not of finite length, and therefore not a list */
		s_retbool(false);
	      }
	  }
      }
      
    case OP_VECTORP:     /* vector? */
      s_retbool(s7_is_vector(car(sc->args)));
      
    case OP_EQ:         /* eq? */
      s_retbool(car(sc->args) == cadr(sc->args));
      
    case OP_EQV:        /* eqv? */
      s_retbool(s7_eqv_p(car(sc->args), cadr(sc->args)));
      
    case OP_EQUAL:        /* equal? */
      s_retbool(s7_is_equal(sc, car(sc->args), cadr(sc->args)));
      
      
    case OP_FORCE:      /* force */
      sc->code = car(sc->args);
      if (is_promise(sc->code)) 
	{
	  /* Should change type to closure here */
	  push_stack(sc, OP_SAVE_FORCED, sc->NIL, sc->code);
	  sc->args = sc->NIL;
	  goto APPLY;
	} 
      else 
	{
	  s_pop(sc, sc->code);
	}
      
    case OP_SAVE_FORCED:     /* Save forced value replacing promise */
      memcpy(sc->code, sc->value,sizeof(struct s7_cell));
      s_pop(sc, sc->value);
      
      
    ERROR:
    case OP_ERROR:
      {
	/* first look for the symbol "error", presumably caller-defined 
	 *   if none, try to jump back to the top level
	 * sc->value is the string
	 */
	sc->x = sc->NIL;
	sc->y = symbol_table_find_by_name(sc, "error"); 
	if (sc->y != sc->NIL)
	  sc->x = s7_find_slot_in_env(sc, sc->envir, sc->y, 1);
	if (sc->x != sc->NIL)
	  {
	    if (sc->args != sc->NIL)
	      sc->code = cons(sc, cons(sc, sc->QUOTE, cons(sc, sc->args, sc->NIL)), sc->NIL);
	    else sc->code = sc->NIL;
	    
	    sc->code = cons(sc, sc->value, sc->code);
	    set_immutable(car(sc->code));
	    sc->code = cons(sc, s7_slot_value_in_env(sc->x), sc->code); 
	    goto EVAL;
	  }
	else fprintf(stderr, "can't find 'error?");
	
	/* fallback */
	sc->args = cons(sc, sc->value, sc->args);
	s7_set_immutable(car(sc->args));
	
	/* (old ERR0) */
	write_string(sc, "error: ", sc->outport);
	if (s7_is_string(sc->value))
	  write_string(sc, string_value(sc->value), sc->outport);
      }
      
    case OP_ERR1:  /* error */
      write_string(sc, " ", sc->outport);
      if (sc->args != sc->NIL) 
	{
	  push_stack(sc, OP_ERR1, cdr(sc->args), sc->NIL);
	  sc->args = car(sc->args);
	  goto P0LIST;
	} 
      else 
	{
	  write_string(sc, "\n", sc->outport);
	  sc->x = g_port_filename(sc, sc->inport);
	  if (sc->x != sc->F)
	    {
	      char linebuf[64];
	      write_string(sc, s7_string(sc->x), sc->outport);
	      write_string(sc, ": ", sc->outport);
	      write_string(sc, "line ", sc->outport);
	      snprintf(linebuf, 64, "%d", port_line_number(sc->inport));
	      write_string(sc, linebuf, sc->outport);
	      write_string(sc, "\n", sc->outport);
	    }
	  return;
	}
      
    case OP_REVERSE:    /* reverse */
      s_pop(sc, s7_reverse(sc, car(sc->args)));
      
    case OP_LIST_STAR: /* list* */
      s_pop(sc, list_star(sc, sc->args));
      
    case OP_APPEND:     /* append */
      if (sc->args == sc->NIL) 
	s_pop(sc, sc->NIL);
      
      sc->x = car(sc->args);
      if (cdr(sc->args) == sc->NIL) 
	s_pop(sc, sc->args);
      
      for (sc->y = cdr(sc->args); sc->y != sc->NIL; sc->y = cdr(sc->y)) 
	sc->x = s7_append(sc, sc->x, car(sc->y));
      s_pop(sc, sc->x);
      
    case OP_QUIT:       /* quit */
      return;
      break;
      
    case OP_SYMBOL_TABLE: /* symbol_table */
      s_pop(sc, symbol_table_all_symbols(sc)); 
      
      
    case OP_GLOBAL_ENVIRONMENT: /* global-environment */
      s_pop(sc, sc->global_env);
      
    case OP_CURRENT_ENVIRONMENT: /* current-environment */
      s_pop(sc, sc->envir);
      
      
      
      /* ========== reading part ========== */
      
    READ_EXPRESSION:
    case OP_READ_EXPRESSION:
      
      switch (sc->tok) 
	{
	case TOK_EOF:
	  s_pop(sc, sc->EOF_OBJECT);
	  
	case TOK_VEC:
	  push_stack(sc, OP_RDVEC, sc->NIL, sc->NIL);
	  /* fall through */
	  
	case TOK_LPAREN:
	  sc->tok = token(sc, sc->inport);
	  if (sc->tok == TOK_RPAREN) 
	    s_pop(sc, sc->NIL)
	    else 
	      {
		if (sc->tok == TOK_DOT) 
		  {
		    sc->value = s7_make_string(sc, "syntax error: illegal dot expression");
		    goto ERROR;
		  }
		else 
		  {
		    if (is_input_port(sc->inport))
		      port_paren_depth(sc->inport)++;
		    push_stack(sc, OP_RDLIST, sc->NIL, sc->NIL);
		    goto READ_EXPRESSION;
		  }
	      }
	  
	case TOK_QUOTE:
	  push_stack(sc, OP_RDQUOTE, sc->NIL, sc->NIL);
	  sc->tok = token(sc, sc->inport);
	  goto READ_EXPRESSION;
	  
	case TOK_BQUOTE:
	  sc->tok = token(sc, sc->inport);
	  if (sc->tok== TOK_VEC) 
	    {
	      push_stack(sc, OP_RDQQUOTEVEC, sc->NIL, sc->NIL);
	      sc->tok= TOK_LPAREN;
	      goto READ_EXPRESSION;
	    } 
	  else push_stack(sc, OP_RDQQUOTE, sc->NIL, sc->NIL);
	  goto READ_EXPRESSION;
	  
	case TOK_COMMA:
	  push_stack(sc, OP_RDUNQUOTE, sc->NIL, sc->NIL);
	  sc->tok = token(sc, sc->inport);
	  goto READ_EXPRESSION;
	  
	case TOK_ATMARK:
	  push_stack(sc, OP_RDUQTSP, sc->NIL, sc->NIL);
	  sc->tok = token(sc, sc->inport);
	  goto READ_EXPRESSION;
	  
	case TOK_ATOM:
	  s_pop(sc, make_atom(sc, read_string_upto(sc, "();\t\n\r ", sc->inport))); /* if reading list (from lparen), this will finally get us to rdlist */
	  
	case TOK_DQUOTE:
	  sc->x = read_string_expression(sc, sc->inport);
	  if (sc->x == sc->F) 
	    {
	      sc->value = s7_make_string(sc, "error reading string");
	      sc->args = sc->x;
	      goto ERROR;
	    }
	  s7_set_immutable(sc->x);
	  s_pop(sc, sc->x);
	  
	case TOK_SHARP_CONST:
	  {
	    char *expr;
	    expr = read_string_upto(sc, "();\t\n\r ", sc->inport);
	    if ((sc->x = make_sharp_const(sc, expr)) == sc->NIL)
	      {
		sc->value = s7_make_string(sc, "undefined sharp expression");
		sc->args = s7_make_string(sc, expr);
		goto ERROR;
	      }
	    else s_pop(sc, sc->x);
	  }
	default:
	  {
	    if (sc->tok == TOK_RPAREN)
	      sc->value = s7_make_string(sc, "too many close parens");
	    else sc->value = s7_make_string(sc, "syntax error: illegal token");
	    goto ERROR;
	  }
	}
      break;
      
    case OP_RDLIST: 
      {
	sc->args = cons(sc, sc->value, sc->args);
	
	sc->tok = token(sc, sc->inport);
	if (sc->tok == TOK_RPAREN) 
	  {
	    int c;
	    if (is_input_port(sc->inport))
	      port_paren_depth(sc->inport)--;
	    
	    c = inchar(sc, sc->inport);
	    if (c != '\n') backchar(sc, c, sc->inport);
	    
	    s_pop(sc, s7_reverse_in_place(sc, sc->NIL, sc->args));
	  } 
	else if (sc->tok == TOK_DOT) 
	  {
	    push_stack(sc, OP_RDDOT, sc->args, sc->NIL);
	    sc->tok = token(sc, sc->inport);
	    goto READ_EXPRESSION;
	  } 
	else 
	  {
	    push_stack(sc, OP_RDLIST, sc->args, sc->NIL);;
	    goto READ_EXPRESSION;
	  }
      }
      
    case OP_RDDOT:
      if (token(sc, sc->inport) != TOK_RPAREN) 
	{
	  sc->value = s7_make_string(sc, "syntax error: illegal dot expression");
	  goto ERROR;
	}
      else 
	{
	  if (is_input_port(sc->inport))
	    port_paren_depth(sc->inport)--;
	  
	  s_pop(sc, s7_reverse_in_place(sc, sc->value, sc->args));
	}
      
    case OP_RDQUOTE:
      s_pop(sc, cons(sc, sc->QUOTE, cons(sc, sc->value, sc->NIL)));
      
    case OP_RDQQUOTE:
      s_pop(sc, cons(sc, sc->QQUOTE, cons(sc, sc->value, sc->NIL)));
      
    case OP_RDQQUOTEVEC:
      s_pop(sc, cons(sc, s7_make_symbol(sc, "apply"),
		     cons(sc, s7_make_symbol(sc, "vector"), 
			  cons(sc,cons(sc, sc->QQUOTE, 
				       cons(sc, sc->value, sc->NIL)),
			       sc->NIL))));
      
    case OP_RDUNQUOTE:
      s_pop(sc, cons(sc, sc->UNQUOTE, cons(sc, sc->value, sc->NIL)));
      
    case OP_RDUQTSP:
      s_pop(sc, cons(sc, sc->UNQUOTESP, cons(sc, sc->value, sc->NIL)));
      
    case OP_RDVEC:
      sc->args = sc->value;
      goto VECTOR;
      
      /* ========== printing part ========== */
      
    P0LIST:
    case OP_P0LIST:
      if (s7_is_vector(sc->args)) 
	{
	  write_string(sc, "#(", sc->outport);
	  sc->args = cons(sc, sc->args, s7_make_integer(sc, 0)); /* loop_counter? */
	  goto PVECFROM;
	} 
      else if (!s7_is_pair(sc->args)) 
	{
	  print_atom(sc, sc->args);
	  s_pop(sc, sc->T);
	} 
      else if (car(sc->args) == sc->QUOTE && ok_abbrev(cdr(sc->args))) 
	{
	  write_string(sc, "'", sc->outport);
	  sc->args = cadr(sc->args);
	  goto P0LIST;
	} 
      else if (car(sc->args) == sc->QQUOTE && ok_abbrev(cdr(sc->args))) 
	{
	  write_string(sc, "`", sc->outport);
	  sc->args = cadr(sc->args);
	  goto P0LIST;
	} 
      else if (car(sc->args) == sc->UNQUOTE && ok_abbrev(cdr(sc->args))) 
	{
	  write_string(sc, ", ", sc->outport);
	  sc->args = cadr(sc->args);
	  goto P0LIST;
	} 
      else if (car(sc->args) == sc->UNQUOTESP && ok_abbrev(cdr(sc->args))) 
	{
	  write_string(sc, ",@", sc->outport);
	  sc->args = cadr(sc->args);
	  goto P0LIST;
	} 
      else 
	{
	  write_string(sc, "(", sc->outport);
	  push_stack(sc, OP_P1LIST, cdr(sc->args), sc->NIL);
	  sc->args = car(sc->args);
	  goto P0LIST;
	}
      
    case OP_P1LIST:
      if (s7_is_pair(sc->args)) 
	{
	  push_stack(sc, OP_P1LIST, cdr(sc->args), sc->NIL);
	  write_string(sc, " ", sc->outport);
	  sc->args = car(sc->args);
	  goto P0LIST;
	} 
      else if (s7_is_vector(sc->args)) 
	{
	  push_stack(sc, OP_P1LIST, sc->NIL, sc->NIL);
	  write_string(sc, " . ", sc->outport);
	  goto P0LIST;
	} 
      else 
	{
	  if (sc->args != sc->NIL) 
	    {
	      write_string(sc, " . ", sc->outport);
	      print_atom(sc, sc->args);
	    }
	  write_string(sc, ")", sc->outport);
	  s_pop(sc, sc->T);
	}
      
    PVECFROM:
    case OP_PVECFROM: 
      {
	int i = loop_counter(cdr(sc->args));
	s7_pointer vec = car(sc->args);
	int len = vector_length(vec);
	if (i == len) 
	  {
	    write_string(sc, ")", sc->outport);
	    s_pop(sc, sc->T);
	  } 
	else 
	  {
	    s7_pointer elem = vector_element(vec, i);
	    loop_counter(cdr(sc->args)) = i + 1;
	    push_stack(sc, OP_PVECFROM, sc->args, sc->NIL);
	    sc->args = elem;
	    write_string(sc, " ", sc->outport);
	    goto P0LIST;
	  }
      }
      
    case OP_OBJECT_TO_STRING:
      s_pop(sc, s7_object_to_string(sc, car(sc->args)));
      
    case OP_LIST_LENGTH:     /* length */   /* a.k */
      {
	int len;
	len = s7_list_length(sc, car(sc->args));
	if (len < 0) 
	  {
	    sc->value = s7_make_string(sc, "length: not a proper list:");
	    goto ERROR;
	  }
	s_pop(sc, s7_make_integer(sc, len));
      }
      
    case OP_ASSQ:       /* assq */     /* a.k */
      sc->x = car(sc->args);
      for (sc->y = cadr(sc->args); s7_is_pair(sc->y); sc->y = cdr(sc->y)) 
	{
	  if (!s7_is_pair(car(sc->y))) 
	    {
	      sc->value = s7_make_string(sc, "unable to handle non pair element");
	      goto ERROR;
	    }
	  
	  if (sc->x == caar(sc->y))
	    break;
	}
      if (s7_is_pair(sc->y)) 
	s_pop(sc, car(sc->y))
	else s_pop(sc, sc->F);
      
      
    case OP_CLOSUREP:        /* closure? */
      /*
       * Note, macro object is also a closure.
       * Therefore, (closure? <#MACRO>) ==> #t
       */
      s_retbool(s7_is_closure(car(sc->args)));
      
    case OP_MACROP:          /* macro? */
      s_retbool(is_macro(car(sc->args)));
      
    default:
      sprintf(sc->strbuf, "%d: illegal operator", sc->op);
      sc->value = s7_make_string(sc, sc->strbuf);
      goto ERROR;
    }
}


static void assign_syntax(s7_scheme *sc, char *name, opcode_t op) 
{
  s7_pointer x;
  x = symbol_table_add_by_name(sc, name); 
  typeflag(x) |= (T_SYNTAX | T_IMMUTABLE | T_CONSTANT); 
  syntax_opcode(x) = small_int(sc, (int)op);
}

static s7_pointer make_proc(s7_scheme *sc, opcode_t op) 
{
  s7_pointer y;
  y = new_cell(sc);
  set_type(y, T_PROC | T_ATOM | T_IMMUTABLE | T_CONSTANT);
  procedure_index(y) = op;
  return(y);
}

static void assign_proc(s7_scheme *sc, opcode_t op, char *name) 
{
  s7_pointer x, y;
  
  x = s7_make_symbol(sc, name);
  local_protect(x);
  y = make_proc(sc, op);
  local_protect(y);
  new_slot_in_env(sc, x, y); 
  local_unprotect(x);
  local_unprotect(y);
}



void s7_kill(s7_scheme *sc) 
{
  sc->symbol_table = sc->NIL;
  sc->global_env = sc->NIL;
  sc->stack = sc->NIL;
  sc->envir= sc->NIL;
  sc->code = sc->NIL;
  sc->args = sc->NIL;
  sc->value = sc->NIL;
  sc->inport = sc->NIL;
  sc->inport_stack = sc->NIL;
  sc->outport = sc->NIL;
  sc->gc_verbose = false;
  gc(sc);
}

void s7_define(s7_scheme *sc, s7_pointer envir, s7_pointer symbol, s7_pointer value) 
{
  s7_pointer x;
  x = s7_find_slot_in_env(sc, envir, symbol, 0);
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
  s7_define(sc, s7_global_env(sc), sym, value);
  local_unprotect(sym);
  local_unprotect(value);
}



s7_pointer s7_apply0(s7_scheme *sc, const char *proc_name) 
{
  s7_pointer carx = s7_make_symbol(sc, proc_name);
  s7_pointer cdrx = sc->NIL;
  
  stack_reset(sc); 
  sc->envir = sc->global_env;
  sc->code = cons(sc, carx, cdrx);
  eval(sc, OP_EVAL);
  return(sc->value);
}

s7_pointer s7_call(s7_scheme *sc, s7_pointer func, s7_pointer args) 
{ 
  stack_reset(sc); 
  sc->envir = sc->global_env; 
  sc->args = args; 
  sc->code = func; 
  eval(sc, OP_APPLY);
  return(sc->value);
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
  x->object.ffptr->doc = doc;
  x->object.ffptr->required_args = required_args;
  x->object.ffptr->optional_args = optional_args;
  x->object.ffptr->rest_arg = rest_arg;
  return(x);
}

const char *s7_function_documentation(s7_pointer x)
{
  if (s7_is_function(x))
    return(x->object.ffptr->doc);
  return("no help");
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

typedef struct {
  int type;
  const char *name;
  char *(*print)(void *value);
  void (*free)(void *value);
  bool (*equal)(void *val1, void *val2);
  void (*gc_mark)(void *val);
  /* TODO: getter? setter? */
} fobject;

static fobject *object_types = NULL;
static int object_types_size = 0;
static int num_types = 0;

int s7_new_type(const char *name, 
		char *(*print)(void *value), 
		void (*free)(void *value), 
		bool (*equal)(void *val1, void *val2),
		void (*gc_mark)(void *val))
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

void *s7_object_value(s7_pointer obj)
{
  return(obj->object.fobj.value);
}


int s7_object_type(s7_pointer obj)
{
  return(obj->object.fobj.type);
}

void s7_define_function(s7_scheme *sc, const char *name, s7_function fnc, int required_args, int optional_args, bool rest_arg, const char *doc)
{
  s7_pointer func, sym;
  
  func = s7_make_function(sc, name, fnc, required_args, optional_args, rest_arg, doc);
  local_protect(func);
  sym = s7_make_symbol(sc, name);
  local_protect(sym);
  s7_define(sc, s7_global_env(sc), sym, func);
  local_unprotect(func);
  local_unprotect(sym);
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


s7_pointer s7_load_path(s7_scheme *sc)
{
  return(s7_eval_c_string(sc, "*load-path*"));
}


s7_pointer s7_add_to_load_path(s7_scheme *sc, const char *file)
{
  /* TODO: load path stuff */
  return(sc->F);
}

s7_pointer s7_load_with_path(s7_scheme *sc, const char *file)
{
  return(s7_load(sc, file));
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



bool s7_is_keyword(s7_pointer obj)
{
  return((s7_is_symbol(obj)) &&
	 (s7_symbol_name(obj)[0] == ':'));
}

bool s7_keyword_eq_p(s7_pointer k1, s7_pointer k2)
{
  return(k1 == k2);
}

s7_pointer s7_make_keyword(s7_scheme *sc, const char *key)
{
  s7_pointer sym;
  char *name;
  name = (char *)CALLOC(safe_strlen(key) + 2, sizeof(char));
  sprintf(name, ":%s", key);                     /* prepend ":" */
  sym = s7_make_symbol(sc, name);
  FREE(name);
  s7_new_slot_spec_in_env(sc, s7_global_env(sc), sym, sym); /* GC protect (?) */
  return(sym);
}



s7_pointer s7_make_and_fill_vector(s7_scheme *sc, int len, s7_pointer fill)
{
  s7_pointer vect;
  vect = s7_make_vector(sc, len);
  if (fill != sc->NIL)
    s7_fill_vector(vect, fill);
  return(vect);
}


s7_pointer s7_list_ref(s7_scheme *sc, s7_pointer lst, int num)
{
  if (num == 0)
    return(s7_car(lst));
  return(s7_list_ref(sc, s7_cdr(lst), num - 1));
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


s7_pointer s7_assoc(s7_scheme *sc, s7_pointer sym, s7_pointer lst)
{
  s7_pointer x;
  int v;
  for (x = lst, v = 0; s7_is_pair(x); x = s7_cdr(x))
    if ((s7_is_pair(s7_car(x))) &&
	(sym == s7_car(s7_car(x))))
      return(s7_car(x));
  return(sc->F);
}


s7_pointer s7_member(s7_scheme *sc, s7_pointer sym, s7_pointer lst)
{
  s7_pointer x;
  int v;
  for (x = lst, v = 0; s7_is_pair(x); x = s7_cdr(x))
    if (sym == s7_car(x))
      return(s7_car(x));
  return(sc->F);
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


bool s7_eq_p(s7_pointer obj1, s7_pointer obj2)
{
  /* TODO: eqv?? */
  return(obj1 == obj2);
}



s7_pointer s7_symbol_value(s7_scheme *sc, s7_pointer sym)
{
  s7_pointer x;
  x = s7_find_slot_in_env(sc, s7_global_env(sc), sym, 0);
  if (x != s7_NIL(sc))
    return(s7_slot_value_in_env(x));
  return(sc->F);
}


s7_pointer s7_symbol_set_value(s7_scheme *sc, s7_pointer sym, s7_pointer val)
{
  s7_pointer x;
  x = s7_find_slot_in_env(sc, s7_global_env(sc), sym, 0);
  if (x != s7_NIL(sc))
    set_slot_in_env(sc, x, val);
  return(val);
}


s7_pointer s7_name_to_value(s7_scheme *sc, const char *name)
{
  return(s7_symbol_value(sc, s7_make_symbol(sc, name)));
}


s7_pointer s7_string_to_form(s7_scheme *sc, const char *str)
{
  /* TODO?: string-to-form */
  return(sc->F);
}

s7_pointer s7_throw(s7_scheme *sc, s7_pointer type, s7_pointer info)
{
  /* TODO: throw and error handling in C */
  return(sc->F);
}

void s7_wrong_type_arg_error(s7_scheme *sc, const char *caller, int arg_n, s7_pointer arg, const char *descr)
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
  FREE(argstr);
  s7_error_and_exit(sc, sc->x, arg);
}

void s7_out_of_range_error(s7_scheme *sc, const char *caller, int arg_n, s7_pointer arg, const char *descr)
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
  FREE(argstr);
  s7_error_and_exit(sc, sc->x, arg);
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

s7_pointer s7_global_env(s7_scheme *sc) 
{
  return(sc->global_env);
}


s7_pointer s7_error(s7_scheme *sc, s7_pointer type, s7_pointer info)
{
  sc->value = type;
  sc->args = info;
  return(sc->ERROR);
}

void s7_error_and_exit(s7_scheme *sc, s7_pointer type, s7_pointer info)
{
  s7_error(sc, type, info);
  eval(sc, OP_ERROR);
  if (sc->error_exiter)
    (*(sc->error_exiter))();
}



/* TODO: arg checks... */
static s7_pointer g_tracing(s7_scheme *sc, s7_pointer a)
{
#define H_tracing "(tracing bool) turns tracing on or off"
  s7_pointer old_val;
  old_val = (sc->tracing) ? sc->T : sc->F;
  sc->tracing = (car(a) != sc->F);
  return(old_val);
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
  gc(sc);
  return(sc->UNSPECIFIED);
}

static s7_pointer g_cadr(s7_scheme *sc, s7_pointer a)
{
#define H_cadr "(cadr lst) returns (car (cdr lst))"
  s7_pointer lst = car(a);
  if ((s7_is_pair(lst)) && (s7_is_pair(cdr(lst))))
    return(cadr(lst));
  return(s7_error(sc, s7_make_string(sc, "cadr bad argument: "), a));
}

static s7_pointer g_function_documentation(s7_scheme *sc, s7_pointer x)
{
#define H_function_documentation "(function-documentation func) returns func's documentation string"
  if (s7_is_function(x))
    return(s7_make_string(sc, s7_function_documentation(x)));
  return(sc->F);
}

s7_pointer s7_function_arity(s7_scheme *sc, s7_pointer x)
{
#define H_function_arity "(function-arity func) returns a list '(required optional rest)"
  s7_pointer lst = sc->NIL;
  if (s7_is_function(x))
    {
      lst = s7_cons(sc, (x->object.ffptr->rest_arg) ? sc->T : sc->F, lst);
      local_protect(lst);
      lst = s7_cons(sc, s7_make_integer(sc, x->object.ffptr->optional_args), lst);
      lst = s7_cons(sc, s7_make_integer(sc, x->object.ffptr->required_args), lst);
      local_unprotect(lst);
    }
  return(lst);
}


void s7_set_error_exiter(s7_scheme *sc, void (*error_exiter)(void))
{
  sc->error_exiter = error_exiter;
}

/*
  (define (caar x) (car (car x)))
  ;(define (cadr x) (car (cdr x)))
  (define (cdar x) (cdr (car x)))
  (define (cddr x) (cdr (cdr x)))
  (define (caaar x) (car (car (car x))))
  (define (caadr x) (car (car (cdr x))))
  (define (cadar x) (car (cdr (car x))))
  (define (caddr x) (car (cdr (cdr x))))
  (define (cdaar x) (cdr (car (car x))))
  (define (cdadr x) (cdr (car (cdr x))))
  (define (cddar x) (cdr (cdr (car x))))
  (define (cdddr x) (cdr (cdr (cdr x))))
  (define (caaaar x) (car (car (car (car x)))))
  (define (caaadr x) (car (car (car (cdr x)))))
  (define (caadar x) (car (car (cdr (car x)))))
  (define (caaddr x) (car (car (cdr (cdr x)))))
  (define (cadaar x) (car (cdr (car (car x)))))
  (define (cadadr x) (car (cdr (car (cdr x)))))
  (define (caddar x) (car (cdr (cdr (car x)))))
  (define (cadddr x) (car (cdr (cdr (cdr x)))))
  (define (cdaaar x) (cdr (car (car (car x)))))
  (define (cdaadr x) (cdr (car (car (cdr x)))))
  (define (cdadar x) (cdr (car (cdr (car x)))))
  (define (cdaddr x) (cdr (car (cdr (cdr x)))))
  (define (cddaar x) (cdr (cdr (car (car x)))))
  (define (cddadr x) (cdr (cdr (car (cdr x)))))
  (define (cdddar x) (cdr (cdr (cdr (car x)))))
  (define (cddddr x) (cdr (cdr (cdr (cdr x)))))
*/


s7_scheme *s7_init(void) 
{
  int i, n;
  s7_pointer x;
  
  s7_scheme *sc;
  sc = (s7_scheme *)MALLOC(sizeof(s7_scheme));
  
  sc->gc_off = true; /* sc->args and so on are not set yet, so a gc during init -> segfault */
  sc->gensym_cnt = 0;
  
  sc->NIL = &sc->_NIL;
  sc->T = &sc->_HASHT;
  sc->F = &sc->_HASHF;
  sc->EOF_OBJECT = &sc->_EOF_OBJECT;
  sc->UNDEFINED = &sc->_UNDEFINED;
  sc->UNSPECIFIED = &sc->_UNSPECIFIED;
  sc->ERROR = &sc->_ERROR;
  
  set_type(sc->UNSPECIFIED, T_NIL_TYPE | T_ATOM | T_GC_MARK | T_IMMUTABLE | T_CONSTANT);
  car(sc->UNSPECIFIED) = cdr(sc->UNSPECIFIED) = sc->UNSPECIFIED;
  
  set_type(sc->NIL, T_NIL_TYPE | T_ATOM | T_GC_MARK | T_IMMUTABLE | T_CONSTANT);
  car(sc->NIL) = cdr(sc->NIL) = sc->UNSPECIFIED;
  tst_NIL = sc->NIL;
  
  set_type(sc->T, T_NIL_TYPE | T_ATOM | T_GC_MARK | T_IMMUTABLE | T_CONSTANT);
  car(sc->T) = cdr(sc->T) = sc->UNSPECIFIED;
  
  set_type(sc->F, T_NIL_TYPE | T_ATOM | T_GC_MARK | T_IMMUTABLE | T_CONSTANT);
  car(sc->F) = cdr(sc->F) = sc->UNSPECIFIED;
  
  set_type(sc->EOF_OBJECT, T_NIL_TYPE | T_ATOM | T_GC_MARK | T_IMMUTABLE | T_CONSTANT);
  car(sc->EOF_OBJECT) = cdr(sc->EOF_OBJECT) = sc->UNSPECIFIED;
  
  set_type(sc->UNDEFINED, T_NIL_TYPE | T_ATOM | T_GC_MARK | T_IMMUTABLE | T_CONSTANT);
  car(sc->UNDEFINED) = cdr(sc->UNDEFINED) = sc->UNSPECIFIED;
  
  set_type(sc->ERROR, T_NIL_TYPE | T_ATOM | T_GC_MARK | T_IMMUTABLE | T_CONSTANT);
  car(sc->ERROR) = cdr(sc->ERROR) = sc->UNSPECIFIED;
  
  sc->inport = sc->NIL;
  sc->inport_stack = sc->NIL;
  sc->outport = sc->NIL;
  
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
  
  new_frame_in_env(sc, sc->NIL); 
  
  sc->global_env = sc->envir; 
  set_immutable(sc->global_env);
  
  x = s7_make_symbol(sc, "else");
  new_slot_in_env(sc, x, sc->T); 
  
  n = sizeof(dispatch_table) / sizeof(dispatch_table[0]);
  sc->small_ints = s7_make_vector(sc, n + 1);
  typeflag(sc->small_ints) |= T_CONSTANT;
  
  for(i = 0; i < n; i++) 
    {
      s7_pointer p;
      if (dispatch_table[i].name != 0) 
	assign_proc(sc, (opcode_t)i, dispatch_table[i].name);
      
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
  assign_syntax(sc, "delay",       OP_DELAY);
  assign_syntax(sc, "and",         OP_AND0);
  assign_syntax(sc, "or",          OP_OR0);
  assign_syntax(sc, "cons-stream", OP_C0STREAM); /* what is this?? -- something to do with "promise"? */
  assign_syntax(sc, "macro",       OP_MACRO0);
  assign_syntax(sc, "case",        OP_CASE0);
  assign_syntax(sc, "defmacro",    OP_DEFMACRO0);

  
  sc->LAMBDA = s7_make_symbol(sc, "lambda");
  typeflag(sc->LAMBDA) |= (T_IMMUTABLE | T_CONSTANT); 
  sc->QUOTE = s7_make_symbol(sc, "quote");
  typeflag(sc->QUOTE) |= (T_IMMUTABLE | T_CONSTANT); 
  sc->QQUOTE = s7_make_symbol(sc, "quasiquote");
  typeflag(sc->QQUOTE) |= (T_IMMUTABLE | T_CONSTANT); 
  sc->UNQUOTE = s7_make_symbol(sc, "unquote");
  typeflag(sc->UNQUOTE) |= (T_IMMUTABLE | T_CONSTANT); 
  sc->UNQUOTESP = s7_make_symbol(sc, "unquote-splicing");
  typeflag(sc->UNQUOTESP) |= (T_IMMUTABLE | T_CONSTANT); 
  sc->FEED_TO = s7_make_symbol(sc, "=>");
  typeflag(sc->FEED_TO) |= (T_IMMUTABLE | T_CONSTANT); 
  
  s7_define_function(sc, "function-documentation", g_function_documentation,    1, 0, false, H_function_documentation);
  s7_define_function(sc, "function-arity",      s7_function_arity,    1, 0, false, H_function_arity);
  s7_define_function(sc, "function-source",     g_function_source, 1, 0, false, H_function_source);
  
  /* ports */
  s7_define_function(sc, "port-line-number",    g_port_line_number,    1, 0, false, H_port_line_number);
  s7_define_function(sc, "port-filename",       g_port_filename,       1, 0, false, H_port_filename);
  s7_define_function(sc, "input-port?",         g_is_input_port,       1, 0, false, H_is_input_port);
  s7_define_function(sc, "output-port?",        g_is_output_port,      1, 0, false, H_is_output_port);
  s7_define_function(sc, "char-ready?",         g_is_char_ready,       0, 1, false, H_is_char_ready);
  s7_define_function(sc, "eof-object?",         g_is_eof_object,       1, 0, false, H_is_eof_object);
  s7_define_function(sc, "current-input-port",  g_current_input_port,  0, 0, false, H_current_input_port);
  s7_define_function(sc, "current-output-port", g_current_output_port, 0, 0, false, H_current_output_port);
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
  
  s7_define_function(sc, "load",                g_load,                1, 0, false, H_display);
  
  s7_define_function(sc, "tracing",    g_tracing,    1, 0, false, H_tracing);
  s7_define_function(sc, "gc-verbose", g_gc_verbose, 1, 0, false, H_gc_verbose);
  s7_define_function(sc, "stacktrace", g_stacktrace, 0, 0, false, H_stacktrace);
  s7_define_function(sc, "gc",         g_gc,         0, 0, false, H_gc);
  
  
  s7_define_function(sc, "cadr",       g_cadr,       1, 0, false, H_cadr);
  
  sc->gc_off = false;
  return(sc);
}

