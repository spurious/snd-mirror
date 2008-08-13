/* S7
 *    derived from:
 */

#define USE_SND 1

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
 *     made several procedures global (eqv, list_length, append, reverse, reverse_in_place, atom2str and others)
 *     scheme_call returns sc->value
 *     merged scheme-private.h into scheme.h and changed names to s7.[ch], init.scm -> s7.scm, opdefines.h -> s7-ops.h
 *     defined *load-path*, defmacro, a stub for make-procedure-with-setter (s7.scm)
 *     changed final initializer at dispatch_table preload to make C happier
 *     added T_FOREIGN_OBJECT (and renamed old T_FOREIGN to T_FOREIGN_FUNCTION, is_foreign to is_foreign_function)
 *       and various code to support it (is_foreign_object, _fobj struct in s7.h etc, mk_foreign_object)
 *       added OP_EQUAL and is_equal for equal? (needs to be in C, not s7.scm because foreign objects have private equality tests)
 *     sc->UNDEFINED for unset (optional) args
 *     no inexact ints, so print 440.0 as 440.0 (not 440)
 *     added OP_SET1 code for generalized set!
 *     changed succ to 1+ and pred to 1- in s7.scm.  added log10 (s7.scm).
 *     added atan asin acos atanh asinh acosh
 *     moved lcm and gcd from s7.scm to c to handle argnum!= 2 cases
 *     added case for expt of ints with 2nd arg >= 0 or 1st arg +/-1 (return an int)
 *     (eqv? 0 0.0) should return #f I believe
 *       so leaving aside a few quibbles, this passes all of Jaffer's r4rstest.scm tests
 *     decided (sigh) to clean up the names throughout -- exported functions start with "s7_"
 *       also, I've tried to hide all struct details in this file -- only s7.h stuff is exported.
 *       changed mk_* to make_*
 *     added doc field for foreign funcs
 *     floor/ceiling/truncate return ints! (This is not RnRS)
 *     guile-style catch/throw in s7.scm
 *     added get-output-string for open-string-output (useless otherwise), and current-error-port
 *     open-output-string takes 0 args, reallocs internal buffer as needed
 *     started reformatting the text so I don't have to squint all the time
 *     #\page in sharp reader so that gauche-format.scm will load.
 *     added complex and ratio numbers
 *        make-polar make-rectangular real-part imag-part magnitude angle
 *        rationalize numerator denominator
 *        ints are off_t's in this case
 *        had to move all the numeric stuff in s7.scm into s7-ops.h and s7.c
 *     split the hidden ivalue uses off into separate structs
 *     changed the layout of vectors (found the GC bug related to it: (make-vector 0) in r4rstest.scm was deadly to preceding version)
 *     removed the old memory allocation junk (at no cost in speed)
 *
 *
 * there's no arg number mismatch check for caller-defined functions!
 * dies if undefined func? (asb 1) as typo
 *
 * need dynamic-wind tested, define* et al (gauche-optargs loads but doesn't work yet).
 *      applicable types
 *      block comment #| |#
 *      procedure arity, source
 *      file sys etc in snd-xen.c: tmpnam and time stuff 
 *      completions (oblist?)
 *      s7_write so strings are quoted in object->string
 *      soft port for Snd (need to trap error reports somehow)
 *      doc strings for scheme funcs
 *      <CLOSURE> should give the name [s7_define could a backpointer to the symbol in the value]
 *      list or list* for xen connection
 *      continuations might be fixable by invoking a new interpreter on each one, copying the state of its parent
 *      (keyword? :asd) segfaults! -- need the names make-keyword, keyword? 
 *
 * [number->string has no "radix" arg]
 *
 * slib logical.scm has logior etc
 * slib dynwind.scm has dynamic-wind
 * guile's optargs.scm has define*
 */

/* if not defined, gc segfaults... */
#define USE_SCHEME_STACK 1

#ifndef USE_TRACING
  #define USE_TRACING 1
#endif

/* To force system errors through user-defined error handling (see *error-hook*) */
#ifndef USE_ERROR_HOOK
  #define USE_ERROR_HOOK 1
#endif

#ifndef USE_COLON_HOOK   /* Enable qualified qualifier -- what?? */
  #define USE_COLON_HOOK 1
#endif

/*
#define USE_OBJECT_LIST 1
#define USE_ALIST_ENV 1
*/


#define _FILE_OFFSET_BITS 64

#include <unistd.h>
#include <math.h>
#include <limits.h>
#include <float.h>
#include <ctype.h>
#include <strings.h>
#include <string.h>
#include <stdlib.h>
#include <sys/types.h>

#if USE_SND
#define MUS_DEBUGGING 1
#define copy_string(Str) copy_string_1(Str, __FUNCTION__, __FILE__, __LINE__)
char *copy_string_1(const char *str, const char *func, const char *file, int line);
#include "_sndlib.h"
#endif

#include "s7.h"


/* operator code */
enum scheme_opcodes { 
#define _OP_DEF(A, B, C, D, E, OP) OP,
#include "s7-ops.h" 
  OP_MAXDEFINED 
}; 


static char *opcode_names[] = {
#define _OP_DEF(A,B,C,D,E,OP) #OP,
#include "s7-ops.h"
};


static char error_buf[1024];

#include <complex.h>


/* num, for generic arithmetic */
typedef struct num {
  char type;
  union {

    Int ivalue;  /* is this the same as long long? */

    double rvalue; /* or possibly long double */

    struct {
      Int numerator;
      Int denominator;
    } fvalue;

    struct {
      double real;
      double imag;
    } cvalue;

  } value;
} num;


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


enum scheme_port_kind { 
  port_free = 0, 
  port_file = 1, 
  port_string = 2, 
  port_input = 16, 
  port_output = 32 
};


typedef struct port {
  unsigned char kind;
  union {
    struct {
      FILE *file;
      bool close_it;
    } stdio;
    struct {
      char *start;
      char *past_the_end;
      char *curr;
    } string;
  } rep;
} port;


/* cell structure */
typedef struct cell {
  unsigned int flag;
  union {
    
    struct {
      char *svalue;
      int  length;
    } string;
    
    num number;
    
    port *port;

    char cvalue;

    enum scheme_opcodes proc_num;

    struct {
     int length;
      pointer *elements;
    } vector;
    
    struct {
      foreign_func ff;
      const char *doc;
    } ffunc;
    
    struct {
      struct cell *car;
      struct cell *cdr;
    } cons;
    
    struct {
      int type;
      void *value;
    } fobj;
    
  } object;
} cell;

#define cons(sc, a,b) _cons(sc, a, b, 0)
#define immutable_cons(sc, a,b) _cons(sc, a, b, 1)

#define car(p) ((p)->object.cons.car)
#define cdr(p) ((p)->object.cons.cdr)

#define string_value(p)  ((p)->object.string.svalue)
#define string_length(p) ((p)->object.string.length)

#define character(p) ((p)->object.cvalue)

#define loop_counter(p) ((p)->object.number.value.ivalue)

#define set_integer(p) (p)->object.number.type = NUM_INT;

#define is_inport(p) (type(p) == T_PORT && p->object.port->kind & port_input)
#define is_outport(p) (type(p) == T_PORT && p->object.port->kind & port_output)

/* #define procedure_index(p) ((p)->object.number.value.ivalue) */
#define procedure_index(p) ((p)->object.proc_num)

#define cont_dump(p)     cdr(p)

#define setenvironment(p)    typeflag(p) = T_ENVIRONMENT

#define is_atom(p)       (typeflag(p)&T_ATOM)
#define setatom(p)       typeflag(p) |= T_ATOM
#define clratom(p)       typeflag(p) &= CLRATOM

#define is_mark(p)       (typeflag(p) & MARK)
#define setmark(p)       typeflag(p)  |= MARK
#define clrmark(p)       typeflag(p)  &= UNMARK

#define caar(p)          car(car(p))
#define cadr(p)          car(cdr(p))
#define cdar(p)          cdr(car(p))
#define cddr(p)          cdr(cdr(p))
#define cadar(p)         car(cdr(car(p)))
#define caddr(p)         car(cdr(cdr(p)))
#define cadaar(p)        car(cdr(car(car(p))))
#define cadddr(p)        car(cdr(cdr(cdr(p))))
#define cddddr(p)        cdr(cdr(cdr(cdr(p))))

#define vector_length(p)     ((p)->object.vector.length)
#define vector_element(p, i) ((p)->object.vector.elements[i])




struct scheme {
  /* return code */
  int retcode;
  bool tracing;
  
  pointer *cells;
  int cells_top, cells_size;
  
  /* We use 4 registers. */
  pointer args;            /* register for arguments of function */
  pointer envir;           /* stack register for current environment */
  pointer code;            /* register for current code */
  pointer dump;            /* stack register for next evaluation */
  
  bool interactive_repl;   /* are we in an interactive REPL? */
  
  struct cell _NIL;
  pointer NIL;             /* special cell representing empty cell */
  struct cell _HASHT;
  pointer T;               /* special cell representing #t */
  struct cell _HASHF;
  pointer F;               /* special cell representing #f */
  struct cell _EOF_OBJ;
  pointer EOF_OBJ;         /* special cell representing end-of-file object */
  
  struct cell _UNDEFINED;  
  pointer UNDEFINED;       /* special cell representing unset or undefined object */
  
  pointer oblist;          /* pointer to symbol table */
  pointer global_env;      /* pointer to global environment */
  
  /* global pointers to special symbols */
  pointer LAMBDA;               /* pointer to syntax lambda */
  pointer QUOTE;           /* pointer to syntax quote */
  
  pointer QQUOTE;               /* pointer to symbol quasiquote */
  pointer UNQUOTE;         /* pointer to symbol unquote */
  pointer UNQUOTESP;       /* pointer to symbol unquote-splicing */
  pointer FEED_TO;         /* => */
  pointer COLON_HOOK;      /* *colon-hook* */
  pointer ERROR_HOOK;      /* *error-hook* */
  pointer SHARP_HOOK;      /* *sharp-hook* */
  
  pointer inport;
  pointer outport;
  pointer errport;
  pointer save_inport;
  pointer loadport;
  
#define MAXFIL 64
  port load_stack[MAXFIL];     /* Stack of open files for port -1 (LOADing) */
  int nesting_stack[MAXFIL];
  int file_i;
  int nesting;
  
  bool gc_verbose;      /* if gc_verbose is not zero, print gc status */
  
  #define LINESIZE 1024
  char linebuff[LINESIZE];
  char strbuff[256];
  
  int tok;
  int print_flag;
  pointer value;

  enum scheme_opcodes op;

  void *ext_data;     /* For the benefit of foreign functions */
  long gensym_cnt;
  
  void *dump_base;	 /* pointer to base of allocated dump stack */
  int dump_size;		 /* number of frames allocated for dump stack */
};

#define is_true(p)       ((p) != sc->F)
#define is_false(p)      ((p) == sc->F)


#define HISTORY_SIZE 1000000
static pointer history_ptr[HISTORY_SIZE];
static int history_line[HISTORY_SIZE];
static const char *history_func[HISTORY_SIZE];
static int history_ctr = 0;

static pointer set_sc_code_1(scheme *sc, pointer p, int line, const char *func)
{
  history_ptr[history_ctr] = p;
  history_line[history_ctr] = line;
  history_func[history_ctr] = func;
  sc->code = p;
  history_ctr++;
  if (history_ctr >= HISTORY_SIZE) history_ctr = 0;
  return(p);
}

static void gc(scheme *sc, pointer a, pointer b);

static pointer new_cell_protected(scheme *sc, pointer a, pointer b)
{
  pointer p;
  p = (cell *)CALLOC(1, sizeof(cell));
  if (!sc->cells[sc->cells_top])
    sc->cells[sc->cells_top++] = p;
  else
    {
      gc(sc, a, b);
      if (sc->cells_top >= sc->cells_size)
	{
	  for (sc->cells_top = 0; sc->cells_top < sc->cells_size; sc->cells_top++)
	    if (!sc->cells[sc->cells_top])
	      {
		sc->cells[sc->cells_top++] = p;
		return(p);
	      }
	  sc->cells_size *= 2;
	  sc->cells = (cell **)REALLOC(sc->cells, sc->cells_size * sizeof(cell *));
	  sc->cells[sc->cells_top++] = p;
	}
    }
  return(p);
}

static pointer new_cell(scheme *sc)
{
  return(new_cell_protected(sc, sc->NIL, sc->NIL));
}


#define set_sc_code(sc, p) set_sc_code_1(sc, p, __LINE__, __FUNCTION__)

pointer s7_F(scheme *sc) 
{
  return(sc->F);
}

pointer s7_T(scheme *sc) 
{
  return(sc->T);
}

pointer s7_NIL(scheme *sc) 
{
  return(sc->NIL);
}

pointer s7_UNDEFINED(scheme *sc) 
{
  return(sc->UNDEFINED);
}

pointer s7_EOF_OBJ(scheme *sc) 
{
  return(sc->EOF_OBJ);
}

pointer s7_global_env(scheme *sc) 
{
  return(sc->global_env);
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
#define TOK_SHARP   10
#define TOK_SHARP_CONST 11
#define TOK_VEC     12

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


#ifndef prompt
#define prompt "> "
#endif

#ifndef FIRST_CELLSEGS
#define FIRST_CELLSEGS 3
#endif

/* T_BOOL = 0 */

enum scheme_types {
  T_STRING = 1,
  T_NUMBER = 2,
  T_SYMBOL = 3,
  T_PROC = 4,
  T_PAIR = 5,
  T_CLOSURE = 6,
  T_CONTINUATION = 7,
  T_FOREIGN_FUNCTION = 8,
  T_CHARACTER = 9,
  T_PORT = 10,
  T_VECTOR = 11,
  T_MACRO = 12,
  T_PROMISE = 13,
  T_ENVIRONMENT = 14,
  T_FOREIGN_OBJECT = 15,
  T_LAST_SYSTEM_TYPE = 15
};


static const char *type_names[16] = {
  "none", "string", "number", "symbol", "proc", "pair", "closure", "continuation",
  "foreign-function", "character", "port", "vector", "macro", "promise", "environment",
  "foreign-object"
};


/* 5 bits here so only 31 types allowed currently */

#define TYPE_BITS 5
#define T_MASKTYPE      31    /* 0000000000011111 */
#define T_SYNTAX      4096    /* 0001000000000000 */
#define T_IMMUTABLE   8192    /* 0010000000000000 */
#define T_ATOM       16384    /* 0100000000000000 */   /* only for gc */
#define CLRATOM      49151    /* 1011111111111111 */   /* only for gc */
#define MARK         32768    /* 1000000000000000 */
#define UNMARK       32767    /* 0111111111111111 */


static pointer Error_1(scheme *sc, const char *s, pointer a);
#define Error_0(sc, s) return Error_1(sc, s, 0)

static num num_zero;
static num num_one;

/* macros for cell operations */
#define typeflag(p) ((p)->flag)
#define type(p)     (typeflag(p) & T_MASKTYPE)



static void dump_object(scheme *sc, pointer x)
{
  int i;
  for (i = 0; i < HISTORY_SIZE; i++)
    if (x == history_ptr[i])
      fprintf(stderr, "history[%d]: %s[%d]\n", i, history_func[i], history_line[i]);

  fprintf(stderr, "dump %p:\n", x);
  if (x == sc->NIL)
    fprintf(stderr, "nil");
  else
    {
      if ((x) &&
	  (type(x) >= 0) &&
	  (type(x) < T_LAST_SYSTEM_TYPE))
	{
	  fprintf(stderr, "type: %s (%x)\n", type_names[type(x)], typeflag(x));
	}
      else fprintf(stderr, "bogus: %x\n", typeflag(x));
    }
}


bool s7_is_string(pointer p)
{
  return((type(p) == T_STRING)); 
}

bool s7_is_vector(pointer p)    
{ 
  return(type(p) == T_VECTOR);
}

bool s7_is_number(pointer p)
{
  return(type(p) == T_NUMBER);
}

bool s7_is_integer(pointer p) 
{ 
  if (!(s7_is_number(p)))
    return(false);

  return(object_number_type(p) == NUM_INT);
}

bool s7_is_real(pointer p) 
{ 
  if (!(s7_is_number(p)))
    return(false);

  return(object_number_type(p) < NUM_COMPLEX);
}

bool s7_is_rational(pointer p)
{
  if (!(s7_is_number(p)))
    return(false);

  return(object_number_type(p) <= NUM_RATIO);
}

bool s7_is_ratio(pointer p)
{
  if (!(s7_is_number(p)))
    return(false);

  return(object_number_type(p) == NUM_RATIO);
}

bool s7_is_complex(pointer p)
{
  return(s7_is_number(p));
}

bool s7_is_exact(pointer p)
{
  return(s7_is_rational(p));
}

bool s7_is_inexact(pointer p)
{
  return(!s7_is_rational(p));
}


bool s7_is_character(pointer p) 
{ 
  return(type(p) == T_CHARACTER);
}

char *s7_string(pointer p) 
{ 
  return(string_value(p));
}

static num nvalue(pointer p)       
{ 
  return((p)->object.number);
}


char s7_character(pointer p)  
{ 
  return(character(p));
}

bool s7_is_port(pointer p)     
{ 
  return(type(p) == T_PORT);
}

bool s7_is_pair(pointer p)     
{ 
  return (type(p) == T_PAIR);
}

pointer s7_car(pointer p)           
{
  return((p)->object.cons.car);
}

pointer s7_cdr(pointer p)           
{
  return((p)->object.cons.cdr);
}

pointer s7_pair_car(pointer p)   
{ 
  return car(p);
}

pointer s7_pair_cdr(pointer p)   
{ 
  return cdr(p);
}

pointer s7_set_car(pointer p, pointer q) 
{ 
  return car(p) =q;
}

pointer s7_set_cdr(pointer p, pointer q) 
{ 
  return cdr(p) =q;
}

bool s7_is_symbol(pointer p)   
{ 
  return (type(p) == T_SYMBOL);
}

char *s7_symbol_name(pointer p)   
{ 
  return(string_value(car(p)));
}

static bool s7_is_syntax(pointer p)   
{ 
  return (typeflag(p) & T_SYNTAX);
}

bool s7_is_proc(pointer p)     
{ 
  return (type(p) == T_PROC);
}

bool s7_is_foreign_function(pointer p)  
{ 
  return (type(p) == T_FOREIGN_FUNCTION);
}

bool s7_is_foreign_object(pointer p) 
{ 
  return(type(p) == T_FOREIGN_OBJECT);
}

static const char *procname(pointer x);

bool s7_is_closure(pointer p)  
{ 
  return (type(p) == T_CLOSURE);
}

static bool is_macro(pointer p)    
{ 
  return (type(p) == T_MACRO);
}

pointer s7_closure_code(pointer p)   
{ 
  return(car(p));
}

pointer s7_closure_env(pointer p)    
{ 
  return(cdr(p));
}

bool s7_is_continuation(pointer p)    
{ 
  return (type(p) == T_CONTINUATION);
}

/* To do: promise should be forced ONCE only */
static bool is_promise(pointer p)  
{ 
  return (type(p) == T_PROMISE);
}

bool s7_is_environment(pointer p) 
{ 
  return (type(p) == T_ENVIRONMENT);
}

bool s7_is_immutable(pointer p) 
{ 
  return (typeflag(p) & T_IMMUTABLE);
}

void s7_setimmutable(pointer p) 
{ 
  typeflag(p) |= T_IMMUTABLE;
}

static  int Cisalpha(int c) 
{ 
  return isascii(c) && isalpha(c);
}

static  int Cisdigit(int c) 
{ 
  return isascii(c) && isdigit(c);
}

static  int Cisspace(int c) 
{ 
  return isascii(c) && isspace(c);
}

static  int Cisupper(int c) 
{ 
  return isascii(c) && isupper(c);
}

static  int Cislower(int c) 
{ 
  return isascii(c) && islower(c);
}



static pointer _get_cell(scheme *sc, pointer a, pointer b);
static pointer find_consecutive_cells(scheme *sc, int n);
static port *port_rep_from_file(scheme *sc, FILE *, int prop);
static void dump_stack_mark(scheme *);
static int syntaxnum(pointer p);
static void finalize_cell(scheme *sc, pointer a);
static bool file_interactive(scheme *sc);
static int basic_inchar(port *pt);
static void port_close(scheme *sc, pointer p, int flag);
static bool is_one_of(char *s, int c);
static void s7_mark_embedded_foreign_objects(pointer a);



/* -------------------------------- numbers -------------------------------- */

#define DEFAULT_RATIONALIZE_ERROR 1.0e-12

static Int c_mod(Int x, Int y)
{
  Int z;
  if (y == 0) return(x); /* else arithmetic exception */
  z = x % y;
  if (((y < 0) && (z > 0)) ||
      ((y > 0) && (z < 0)))
    return(z + y);
  return(z);
}

static Int c_gcd(Int u, Int v)
{
  Int a, b, temp;

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


static Int c_lcm(Int a, Int b)
{
  if ((a == 0) || (b == 0)) return(0);
  if (a < 0) a = -a;
  if (b < 0) b = -b;
  return((a * b) / c_gcd(a, b));
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

pointer s7_rationalize(scheme *sc, double x, double error)
{
  Int numer = 0, denom = 1;
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

static Int num_to_numerator(num n)
{
  if (n.type == NUM_RATIO)
    return(numerator(n));
  return(integer(n));
}

static Int num_to_denominator(num n)
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

static num make_ratio(scheme *sc, Int numer, Int denom)
{
  num ret;
  Int divisor;

  if (denom == 0)
    Error_1(sc, "/: division by 0", 0);

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


Int s7_numerator(pointer x)
{
  return(numerator(x->object.number));
}

Int s7_denominator(pointer x)
{
  return(denominator(x->object.number));
}

double s7_real_part(pointer x)
{
  return(real_part(x->object.number));
}

double s7_imag_part(pointer x)
{
  return(imag_part(x->object.number));
}

Int s7_integer(pointer p)
{
  return(integer(p->object.number));
}

double s7_double(pointer p)
{
  return(real(p->object.number));
}

static double complex s7_complex(pointer p)
{
  return(num_to_real_part(p->object.number) + num_to_imag_part(p->object.number) * _Complex_I);
}

static pointer s7_from_c_complex(scheme *sc, double complex z)
{
  return(s7_make_complex(sc, creal(z), cimag(z)));
}


static num num_max(scheme *sc, num a, num b) 
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


static num num_min(scheme *sc, num a, num b) 
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


static num num_add(scheme *sc, num a, num b) 
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


static num num_sub(scheme *sc, num a, num b) 
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


static num num_mul(scheme *sc, num a, num b) 
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

/* error checks in op_exe are wrong now */
static num num_div(scheme *sc, num a, num b) 
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
	if (rb == 0.0)
	  Error_1(sc, "/: division by 0.0", 0);
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
	if (den == 0.0)
	  Error_1(sc, "/: complex division by 0.0", 0);
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
  if (num_type(a) != num_type(b)) return(false);
  switch (num_type(a))
    {
    case NUM_INT:     return(integer(a) == integer(b));
    case NUM_RATIO:   return((numerator(a) == numerator(b)) &&
			     (denominator(a) == denominator(b)));
    case NUM_REAL2:
    case NUM_REAL:    return(real(a) == real(b));
    default:          return((real_part(a) == real_part(b)) &&
			     (imag_part(a) == imag_part(b)));
    }
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
  
  while(*s!= 0 && (*s =='1' || *s =='0')) 
    {
      x<<= 1;
      x+=*s-'0';
      s++;
    }
  
  return x;
}

/* get new cons cell */
static pointer _cons(scheme *sc, pointer a, pointer b, int immutable) 
{
  pointer x;
  x = new_cell_protected(sc, a, b);
  
  typeflag(x) = T_PAIR;
  if (immutable) 
    s7_setimmutable(x);
  car(x) = a;
  cdr(x) = b;
  return(x);
}

pointer s7_immutable_cons(scheme *sc, pointer a, pointer b) 
{
  return(_cons(sc, a, b, 1));
}

pointer s7_cons(scheme *sc, pointer a, pointer b) 
{
  return(_cons(sc, a, b, 0));
}



/* ========== oblist implementation  ========== */ 

#ifndef USE_OBJECT_LIST 

static int hash_fn(const char *key, int table_size); 

static pointer oblist_initial_value(scheme *sc) 
{ 
  return(s7_make_vector(sc, 461)); /* probably should be bigger */ 
} 

/* returns the new symbol */ 
static pointer oblist_add_by_name(scheme *sc, const char *name) 
{ 
  pointer x; 
  int location; 
  
  x = s7_immutable_cons(sc, s7_make_string(sc, name), sc->NIL); 
  typeflag(x) = T_SYMBOL; 
  s7_setimmutable(car(x)); 
  
  location = hash_fn(name, vector_length(sc->oblist)); 
  s7_vector_set(sc->oblist, 
		location, 
		s7_immutable_cons(sc, x, s7_vector_ref(sc->oblist, location))); 
  return x; 
} 

static  pointer oblist_find_by_name(scheme *sc, const char *name) 
{ 
  int location; 
  pointer x; 
  char *s; 
  
  location = hash_fn(name, vector_length(sc->oblist)); 
  for (x = s7_vector_ref(sc->oblist, location); x != sc->NIL; x = cdr(x)) 
    { 
      s = s7_symbol_name(car(x)); 
      /* case-insensitive, per R5RS section 2. */ 
      if (strcasecmp(name, s) == 0) 
	{ 
	  return car(x); 
	} 
    } 
  return sc->NIL; 
} 

static pointer oblist_all_symbols(scheme *sc) 
{ 
  int i; 
  pointer x; 
  pointer ob_list = sc->NIL; 
  
  for (i = 0; i < vector_length(sc->oblist); i++) 
    for (x  = s7_vector_ref(sc->oblist, i); x != sc->NIL; x = cdr(x)) 
      ob_list = cons(sc, x, ob_list); 

  return ob_list; 
} 

#else 

static pointer oblist_initial_value(scheme *sc) 
{ 
  return sc->NIL; 
} 

static  pointer oblist_find_by_name(scheme *sc, const char *name) 
{ 
  pointer x; 
  char    *s; 
  
  for (x = sc->oblist; x != sc->NIL; x = cdr(x)) 
    { 
      s = s7_symbol_name(car(x)); 
      /* case-insensitive, per R5RS section 2. */ 
      if (strcasecmp(name, s) == 0) 
	{ 
	  return car(x); 
	} 
    } 
  return sc->NIL; 
} 

/* returns the new symbol */ 
static pointer oblist_add_by_name(scheme *sc, const char *name) 
{ 
  pointer x; 
  
  x = immutable_cons(sc, s7_make_string(sc, name), sc->NIL); 
  typeflag(x) = T_SYMBOL; 
  s7_setimmutable(car(x)); 
  sc->oblist = immutable_cons(sc, x, sc->oblist); 
  return x; 
} 
static pointer oblist_all_symbols(scheme *sc) 
{ 
  return sc->oblist; 
} 

#endif 




static pointer make_port(scheme *sc, port *p) 
{
  pointer x = new_cell(sc);
  
  typeflag(x) = T_PORT|T_ATOM;
  x->object.port = p;
  return (x);
}

pointer s7_make_foreign_function(scheme *sc, foreign_func f, const char *doc) 
{
  pointer x = new_cell(sc);
  typeflag(x) = (T_FOREIGN_FUNCTION | T_ATOM);
  x->object.ffunc.ff = f;
  x->object.ffunc.doc = doc;
  return(x);
}

const char *s7_foreign_function_doc(pointer x)
{
  if (s7_is_foreign_function(x))
    return(x->object.ffunc.doc);
  return("no help");
}

pointer s7_make_foreign_object(scheme *sc, int type, void *value)
{
  pointer x;
  x = new_cell(sc);
  typeflag(x) = (T_FOREIGN_OBJECT | T_ATOM);
  x->object.fobj.type = type;
  x->object.fobj.value = value;
  return (x);
}

pointer s7_make_character(scheme *sc, int c) 
{
  pointer x = new_cell(sc);
  typeflag(x) = (T_CHARACTER | T_ATOM);
  character(x) = c;
  return (x);
}

pointer s7_make_integer(scheme *sc, Int n) 
{
  pointer x = new_cell(sc);
  typeflag(x) = (T_NUMBER | T_ATOM);

  x->object.number.type = NUM_INT;
  integer(x->object.number) = n;

  return(x);
}

pointer s7_make_real(scheme *sc, double n) 
{
  pointer x = new_cell(sc);
  typeflag(x) = (T_NUMBER | T_ATOM);

  x->object.number.type = NUM_REAL;
  real(x->object.number) = n;

  return(x);
}

pointer s7_make_complex(scheme *sc, double a, double b)
{
  num ret;
  pointer x = new_cell(sc);
  typeflag(x) = (T_NUMBER | T_ATOM);
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

pointer s7_make_ratio(scheme *sc, Int a, Int b)
{
  /* make_number calls us, so we can't call it as a convenience! */

  num ret;
  pointer x = new_cell(sc);
  typeflag(x) = (T_NUMBER | T_ATOM);
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

static pointer make_number(scheme *sc, num n) 
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
static char *store_string(scheme *sc, int len_str, const char *str, char fill) 
{
  char *q;
  
  q = (char*)MALLOC(len_str + 1);
  if (q == 0) 
    {
      fprintf(stderr, "failed in store_string\n");
      return(sc->strbuff);
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
pointer s7_make_string(scheme *sc, const char *str) 
{
  return(s7_make_counted_string(sc, str, strlen(str)));
}

pointer s7_make_counted_string(scheme *sc, const char *str, int len) 
{
  pointer x = new_cell(sc);
  
  string_value(x) = store_string(sc, len, str, 0);
  typeflag(x) = (T_STRING | T_ATOM);
  string_length(x) = len;
  return (x);
}

static pointer mk_empty_string(scheme *sc, int len, char fill) 
{
  pointer x = new_cell(sc);
  
  string_value(x) = store_string(sc,len, 0,fill);
  typeflag(x) = (T_STRING | T_ATOM);
  string_length(x) = len;
  return (x);
}

pointer s7_make_vector(scheme *sc, int len) 
{
  pointer x;
  x = new_cell(sc);
  typeflag(x) = (T_VECTOR | T_ATOM);
  vector_length(x) = len;
  if (len > 0)
    {
      x->object.vector.elements = (pointer *)CALLOC(len, sizeof(pointer));
      s7_fill_vector(x, sc->NIL);
    }
  return x;
}

int s7_vector_length(pointer vec)
{
  return(vector_length(vec));
}

void s7_fill_vector(pointer vec, pointer obj) 
{
  int i, len;
  len = vector_length(vec);
  for(i = 0; i < len; i++) 
    vector_element(vec, i) = obj;
}

pointer s7_vector_ref(pointer vec, int elem) 
{
  if (elem >= vector_length(vec))
    fprintf(stderr, "vector-ref past end of vector: %d %d\n", elem, vector_length(vec));
  return(vector_element(vec, elem));
}

pointer s7_vector_set(pointer vec, int elem, pointer a) 
{
  if (elem >= vector_length(vec))
    fprintf(stderr, "vector-set past end of vector: %d %d\n", elem, vector_length(vec));
  vector_element(vec, elem) = a;
}

/* get new symbol */
pointer s7_make_symbol(scheme *sc, const char *name) 
{ 
  pointer x; 
  
  /* first check oblist */ 
  x = oblist_find_by_name(sc, name); 
  if (x != sc->NIL) 
    { 
      return (x); 
    } 
  else 
    { 
      x = oblist_add_by_name(sc, name); 
      return (x); 
    } 
} 

pointer s7_gensym(scheme *sc) 
{ 
  pointer x; 
  char name[40]; 
  
  for(; sc->gensym_cnt<LONG_MAX; sc->gensym_cnt++) 
    { 
      sprintf(name, "gensym-%ld", sc->gensym_cnt); 
      
      /* first check oblist */ 
      x = oblist_find_by_name(sc, name); 
      
      if (x != sc->NIL) 
	{ 
	  continue; 
	} 
      else 
	{ 
	  x = oblist_add_by_name(sc, name); 
	  return (x); 
	} 
    } 
  
  return sc->NIL; 
} 


/* make symbol or number atom from string */

static pointer make_atom(scheme *sc, char *q) 
{
  char c, *p, *slash, *plus;
  bool has_dec_point = false, has_slash = false, has_i = false, has_previous_dec_point = false; 
  int has_plus_or_minus = 0;
  bool has_fp_exp = false;

#if USE_COLON_HOOK
  if ((p = strstr(q, "::")) != 0) 
    {
      *p = 0;
      return(cons(sc, sc->COLON_HOOK,
		  cons(sc,
		       cons(sc,
			    sc->QUOTE,
			    cons(sc, make_atom(sc, p + 2), sc->NIL)),
		       cons(sc, s7_make_symbol(sc, string_downcase(q)), sc->NIL))));
    }
#endif
  
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
      len = strlen(q);

      if (q[len - 1] != 'i')
	return(s7_make_symbol(sc, string_downcase(q)));
      q[len - 1] = '\0'; /* remove 'i' */
      
      if (has_previous_dec_point)
	{
	  if (has_dec_point)
	    {
	      /* both are floats */
	      if (has_plus_or_minus == 1)
		return(sc, s7_make_complex(sc, atof(q), atof(plus)));
	      return(sc, s7_make_complex(sc, atof(q), -atof(plus)));
	    }
	  else
	    {
	      /* first was float */
	      if (has_plus_or_minus == 1)
		return(sc, s7_make_complex(sc, atof(q), atoll(plus)));
	      return(sc, s7_make_complex(sc, atof(q), -atoll(plus)));
	    }
	}
      else
	{
	  if (has_dec_point)
	    {
	      /* second is float */
	      if (has_plus_or_minus == 1)
		return(sc, s7_make_complex(sc, atoll(q), atof(plus)));
	      return(sc, s7_make_complex(sc, atoll(q), -atof(plus)));
	    }
	  else
	    {
	      /* both are ints */
	      if (has_plus_or_minus == 1)
		return(sc, s7_make_complex(sc, atoll(q), atoll(plus)));
	      return(sc, s7_make_complex(sc, atoll(q), -atoll(plus)));
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
static pointer make_sharp_const(scheme *sc, char *name) 
{
  long x;
  char tmp[256];

  if (!strcmp(name, "t"))
    return (sc->T);

  if (!strcmp(name, "f"))
    return (sc->F);

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
      else if ((strcasecmp(name + 1, "newline") == 0) ||
	       (strcasecmp(name + 1, "page") == 0))
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



/* ========== garbage collector ========== */

/*--
 *  We use algorithm E (Knuth, The Art of Computer Programming Vol.1,
 *  sec. 2.3.5), the Schorr-Deutsch-Waite link-inversion algorithm, 
 *  for marking. 
 */
static void mark(pointer a) 
{
  pointer t, q, p;
  
  t = (pointer) 0;
  p = a;
 E2:  setmark(p);
  if (s7_is_vector(p)) 
    {
      int i, len;
      len = vector_length(p);
      for(i = 0; i < len; i++) 
	mark(vector_element(p, i));
    }
  else
    {
      if (s7_is_foreign_object(p))
	s7_mark_embedded_foreign_objects(p);
    }

  if (is_atom(p))
    goto E6;
  /* E4: down car */
  q = car(p);
  if (q && !is_mark(q)) 
    {
      setatom(p);  /* a note that we have moved car */ 
      car(p) = t;
      t = p;
      p = q;
      goto E2;
    }
 E5:  q = cdr(p); /* down cdr */
  if (q && !is_mark(q)) 
    {
      cdr(p) = t;
      t = p;
      p = q;
      goto E2;
    }
 E6:   /* up.  Undo the link switching from steps E4 and E5. */ 
  if (!t)
    return;
  q = t;
  if (is_atom(q)) 
    {
      clratom(q);
      t = car(q);
      car(q) = p;
      p = q;
      goto E5;
    } 
  else 
    {
      t = cdr(q);
      cdr(q) = p;
      p = q;
      goto E6;
    }
}

void s7_mark_object(pointer a)
{
  mark(a);
}


static void gc(scheme *sc, pointer a, pointer b) 
{
  pointer p;
  int i, freed_cells = 0;
  
  if (sc->gc_verbose) 
    {
      s7_putstr(sc, "gc...");
    }
  
  /* mark system globals */
  mark(sc->oblist);
  mark(sc->global_env);
  
  /* mark current registers */
  mark(sc->args);
  mark(sc->envir);
  mark(sc->code);
  dump_stack_mark(sc); 
  mark(sc->value);
  mark(sc->inport);
  mark(sc->save_inport);
  mark(sc->outport);
  mark(sc->errport);
  mark(sc->loadport);
  mark(a);
  mark(b);
  mark(sc->ext_data); /* s7 */
  
  /* garbage collect */
  clrmark(sc->NIL);

  for (i = 0; i < sc->cells_size; i++)
    {
      p = sc->cells[i];
      if (p)
	{
	  if (is_mark(p)) 
	    clrmark(p);
	  else 
	    {
	      /* reclaim cell */
	      if (typeflag(p) != 0) 
		{ 
		  finalize_cell(sc, p); 
		  typeflag(p) = 0; 
		  car(p) = sc->NIL; 
		  FREE(p);
		  sc->cells[i] = NULL;
		} 
	      freed_cells++;
	    }
	}
    }
  
  if (sc->gc_verbose) 
    {
      char msg[80];
      sprintf(msg, "done: %ld cells were recovered.\n", freed_cells);
      s7_putstr(sc,msg);
    }
}


static void finalize_cell(scheme *sc, pointer a) 
{
  if (s7_is_string(a)) 
    {
      FREE(string_value(a)); /* malloc'd in store_string */
    } 
  else 
    {
      if (s7_is_port(a)) 
	{
	  if ((a->object.port->kind & port_file) &&
	      (a->object.port->rep.stdio.close_it))
	    {
	      port_close(sc, a, port_input | port_output);
	    }
	  if ((a->object.port->kind & port_string) &&
	      (a->object.port->kind & port_output) &&
	      (a->object.port->rep.string.start))
	    {
	      FREE(a->object.port->rep.string.start);
	      a->object.port->rep.string.start = NULL;
	    }

	  FREE(a->object.port);
	}
      else
	{
	  if (s7_is_foreign_object(a))
	    s7_free_foreign_object(a);
	}
    }
}


/* ========== Routines for Reading ========== */

static int file_push(scheme *sc, const char *fname) 
{
  FILE *fin= fopen(fname, "r");
  if (fin!= 0) 
    {
      sc->file_i++;
      sc->load_stack[sc->file_i].kind = port_file|port_input;
      sc->load_stack[sc->file_i].rep.stdio.file = fin;
      sc->load_stack[sc->file_i].rep.stdio.close_it = true;
      sc->nesting_stack[sc->file_i] = 0;
      sc->loadport->object.port = sc->load_stack+sc->file_i;
    }
  return fin!= 0;
}

static void file_pop(scheme *sc) 
{
  sc->nesting = sc->nesting_stack[sc->file_i];
  if (sc->file_i!= 0) 
    {
      port_close(sc, sc->loadport, port_input);
      sc->file_i--;
      sc->loadport->object.port = sc->load_stack+sc->file_i;
      if (file_interactive(sc)) 
	{
	  s7_putstr(sc, prompt);
	}
    }
}

static bool file_interactive(scheme *sc) 
{
  return((sc->file_i == 0) && 
	 (sc->load_stack[0].rep.stdio.file == stdin) && 
	 (sc->inport->object.port->kind & port_file));
}


static port *port_rep_from_filename(scheme *sc, const char *fn, int prop) 
{
  FILE *f;
  char *rw;
  port *pt;
  if (prop == (port_input | port_output)) 
    {
      rw = "a+";
    } 
  else if (prop == port_output) 
    {
      rw = "w";
    } 
  else 
    {
      rw = "r";
    }
  f = fopen(fn, rw);
  if (f == 0) 
    {
      return(0);
    }
  pt = port_rep_from_file(sc, f, prop);
  pt->rep.stdio.close_it = true;
  return(pt);
}


static pointer port_from_filename(scheme *sc, const char *fn, int prop) 
{
  port *pt;
  pt = port_rep_from_filename(sc,fn, prop);
  if (pt == 0) 
    {
      return sc->NIL;
    }
  return make_port(sc, pt);
}

static port *port_rep_from_file(scheme *sc, FILE *f, int prop) 
{
  char *rw;
  port *pt;
  pt = (port*)MALLOC(sizeof(port));
  if (pt == 0) 
    {
      return(NULL);
    }
  if (prop == (port_input | port_output)) 
    {
      rw = "a+";
    } 
  else if (prop == port_output) 
    {
      rw = "w";
    } 
  else 
    {
      rw = "r";
    }
  pt->kind = port_file | prop;
  pt->rep.stdio.file = f;
  pt->rep.stdio.close_it = false;
  return(pt);
}

static pointer port_from_file(scheme *sc, FILE *f, int prop) 
{
  port *pt;
  pt = port_rep_from_file(sc,f, prop);
  if (pt == 0) 
    {
      return sc->NIL;
    }
  return make_port(sc, pt);
}

static port *port_rep_from_string(scheme *sc, char *start, char *past_the_end, int prop) 
{
  port *pt;
  pt = (port*)MALLOC(sizeof(port));
  if (pt == 0) 
    {
      return(NULL);
    }
  pt->kind = port_string | prop;
  pt->rep.string.start = start;
  pt->rep.string.curr= start;
  pt->rep.string.past_the_end = past_the_end;
  return pt;
}


static pointer port_from_string(scheme *sc, char *start, char *past_the_end, int prop) 
{
  port *pt;
  pt = port_rep_from_string(sc,start, past_the_end, prop);
  if (pt == 0) 
    {
      return sc->NIL;
    }
  return make_port(sc, pt);
}


static void port_close(scheme *sc, pointer p, int flag) 
{
  port *pt;
  bool output_string;

  pt = p->object.port;
  output_string = ((pt->kind & port_output) && (pt->kind & port_string));

  pt->kind &= ~flag; /* signal that this port is no longer open (not in or out port) */

  if ((pt->kind & (port_input | port_output)) == 0) 
    {
      if (pt->kind & port_file)
	fclose(pt->rep.stdio.file);
      /* string port buffer is freed in GC */
      pt->kind = port_free;
    }
}

static char *describe_port(scheme *sc, pointer p)
{
  port *pt;
  char *desc;

  pt = p->object.port;
  desc = sc->strbuff;
  sprintf(desc, "<port");

  if (pt->kind & port_file)
    strcat(desc, " file");
  else
    {
      if (pt->kind & port_string)
	strcat(desc, " string");
      else strcat(desc, "closed");
    }

  if (pt->kind & port_input)
    strcat(desc, " input");
  else strcat(desc, " output");

  if (pt->kind & port_file)
    {
      if ((pt->kind & port_input) &&
	  (pt->rep.stdio.file == stdin))
	strcat(desc, " [stdin]");
      if (pt->kind & port_output)
	{
	  if (pt->rep.stdio.file == stdout)
	    strcat(desc, " [stdout]");
	  else
	    {
	      if (pt->rep.stdio.file == stderr)
		strcat(desc, " [stderr]");
	    }
	}
    }
  strcat(desc, ">");
  return(desc);

}


/* get new character from input file */
static int inchar(scheme *sc) 
{
  int c;
  port *pt;
 again:
  pt = sc->inport->object.port;

  if (pt->kind & port_free) return(EOF);

  c = basic_inchar(pt);
  if ((c == EOF) && 
      (sc->inport == sc->loadport) && 
      (sc->file_i != 0))
    {
      file_pop(sc);
      if (sc->nesting!= 0) 
	return(EOF);
      else return('\n');
      goto again;
    }
  return c;
}

static int basic_inchar(port *pt) 
{
  if (pt->kind & port_free) return(EOF);

  if (pt->kind&port_file) 
    return fgetc(pt->rep.stdio.file);
  else 
    {
      if ((*pt->rep.string.curr == 0) || 
	  (pt->rep.string.curr == pt->rep.string.past_the_end))
	return (EOF);
      else return(*pt->rep.string.curr++);
    }
}

/* back character to input buffer */
static void backchar(scheme *sc, int c) 
{
  port *pt;
  if (c == EOF) return;

  pt = sc->inport->object.port;
  if (pt->kind & port_free) return;

  if (pt->kind & port_file) 
    ungetc(c, pt->rep.stdio.file);
  else 
    {
      if (pt->rep.string.curr != pt->rep.string.start) 
	--pt->rep.string.curr;
    }
}


/* output */

static void put_char(port *pt, char c)
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


void s7_putstr(scheme *sc, const char *s) 
{
  port *pt = sc->outport->object.port;
  if (pt->kind & port_free) return;

  if (pt->kind & port_file) 
    fputs(s, pt->rep.stdio.file);
  else 
    {
      for(; *s; s++)
	put_char(pt, *s);
    }
}


void s7_putchars(scheme *sc, const char *s, int len) 
{
  port *pt = sc->outport->object.port;
  if (pt->kind & port_free) return;

  if (pt->kind & port_file)
    fwrite(s, 1, len, pt->rep.stdio.file);
  else 
    {
      for(; len; len--)
	put_char(pt, *s++);
    }
}


static void s7_putcharacter(scheme *sc, int c) 
{
  port *pt = sc->outport->object.port;
  if (pt->kind & port_free) return;

  if (pt->kind & port_file)
    fputc(c, pt->rep.stdio.file);
  else put_char(pt, c);
}


pointer s7_open_output_string(scheme *sc)
{
  char *tmp;
  tmp = (char *)CALLOC(128, sizeof(char));
  return(port_from_string(sc, tmp, (char *)(tmp + 128), port_output));
}


const char *s7_get_output_string(scheme *sc, pointer p)
{
  port *pt = p->object.port;
  return(pt->rep.string.start);
}


/* read characters up to delimiter, but cater to character constants */
static char *readstr_upto(scheme *sc, char *delim) 
{
  char *p = sc->strbuff;
  
  while (!is_one_of(delim, (*p++ = inchar(sc))));
  if ((p == sc->strbuff + 2) && 
      (p[-2] == '\\'))
    *p = 0;
  else 
    {
      backchar(sc, p[-1]);
      *--p = '\0';
    }

  return(sc->strbuff);
}

/* read string expression "xxx...xxx" */
static pointer readstrexp(scheme *sc) 
{
  char *p = sc->strbuff;
  int c;
  int c1= 0;
  enum {ST_OK, ST_BSL, ST_X1, ST_X2, ST_OCT1, ST_OCT2, ST_OCT3} state = ST_OK; /* BSL=backslash? X1 or X2 = hex (ctrs) as in oct case */
  
  for (;;) 
    {
      c = inchar(sc);
      if ((c == EOF) || 
	  (p - sc->strbuff > sizeof(sc->strbuff) - 1))
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
	      return(s7_make_counted_string(sc, sc->strbuff, p - sc->strbuff));

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
	      c1 = c-'0';
	      break;

	    case 'x':
	    case 'X':
	      state = ST_X1;
	      c1 = 0;
	      break;

	    case 'n':
	      *p++ ='\n';
	      state = ST_OK;
	      break;

	    case 't':
	      *p++ ='\t';
	      state = ST_OK;
	      break;

	    case 'r':
	      *p++ ='\r';
	      state = ST_OK;
	      break;

	    case '"':
	      *p++ ='"';
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
	      backchar(sc, c);
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
static  void skipspace(scheme *sc) 
{
  int c;
  while (isspace(c = inchar(sc)))
    ;
  if (c != EOF) 
    backchar(sc, c);
}

/* get token */
static int token(scheme *sc) 
{
  int c;
  skipspace(sc);
  switch (c = inchar(sc)) 
    {
    case EOF:
      return (TOK_EOF);

    case '(':
      return (TOK_LPAREN);

    case ')':
      return (TOK_RPAREN);

    case '.':
      c = inchar(sc);
      if (is_one_of(" \n\t", c)) 
	return (TOK_DOT);

      backchar(sc, c);
      backchar(sc, '.');
      return TOK_ATOM;

    case '\'':
      return (TOK_QUOTE);

    case ';':
      while ((c = inchar(sc)) != '\n' && c != EOF)
	;
      return (token(sc));

    case '"':
      return (TOK_DQUOTE);

    case BACKQUOTE:
      return (TOK_BQUOTE);

    case ',':
      if ((c = inchar(sc)) == '@') 
	return (TOK_ATMARK);

      backchar(sc, c);
      return (TOK_COMMA);

    case '#':
      c = inchar(sc);
      if (c == '(') 
	return (TOK_VEC);

      if (c == '!') 
	{
	  /* TODO: block comment, this is wrong! */
	  while ((c =inchar(sc)) != '\n' && c!=EOF)
	    ;
	  return (token(sc));
	}

      backchar(sc,c);
      if (is_one_of(" tfodxb\\", c)) 
	return TOK_SHARP_CONST;
      return (TOK_SHARP);

    default:
      backchar(sc,c);
      return (TOK_ATOM);
    }
}

/* ========== Routines for Printing ========== */

#define   ok_abbrev(x)   (s7_is_pair(x) && cdr(x) == sc->NIL)

static void printslashstring(scheme *sc, char *p, int len) 
{
  int i;
  unsigned char *s = (unsigned char*)p;
  s7_putcharacter(sc, '"');
  for ( i = 0; i<len; i++) 
    {
      if (*s == 0xff || *s == '"' || *s < ' ' || *s == '\\') 
	{
	  s7_putcharacter(sc, '\\');
	  switch(*s) 
	    {
	    case '"':
	      s7_putcharacter(sc, '"');
	      break;
	    case '\n':
	      s7_putcharacter(sc, 'n');
	      break;
	    case '\t':
	      s7_putcharacter(sc, 't');
	      break;
	    case '\r':
	      s7_putcharacter(sc, 'r');
	      break;
	    case '\\':
	      s7_putcharacter(sc, '\\');
	      break;
	    default: { 
	      int d =*s/16;
	      s7_putcharacter(sc, 'x');
	      if (d<10) 
		{
		  s7_putcharacter(sc, d + '0');
		} 
	      else 
		{
		  s7_putcharacter(sc, d - 10 + 'A');
		}
	      d = *s % 16;
	      if (d < 10) 
		{
		  s7_putcharacter(sc, d + '0');
		} 
	      else 
		{
		  s7_putcharacter(sc, d - 10 + 'A');
		}
	    }
	    }
	} 
      else 
	{
	  s7_putcharacter(sc, *s);
	}
      s++; 
    }
  s7_putcharacter(sc, '"');
}


/* print atoms */

/* Uses internal buffer unless string pointer is already available */
void s7_atom2str(scheme *sc, pointer l, int f, char **pp, int *plen) 
{
  char *p;
  
  if (l == sc->NIL) 
    {
      p = "()";
    } 
  else if (l == sc->T) 
    {
      p = "#t";
    } 
  else if (l == sc->F) 
    {
      p = "#f";
    } 
  else if (l == sc->EOF_OBJ) 
    {
      p = "#<EOF>";
    } 
  else if (l == sc->UNDEFINED) 
    {
      p = "#<undefined>";
    } 
  else if (s7_is_port(l)) 
    {
      p = describe_port(sc, l);
    } 
  else if (s7_is_number(l)) 
    {
      p = sc->strbuff;

      switch (object_number_type(l))
	{
	case NUM_INT:
	  sprintf(p, Int_d, s7_integer(l));
	  break;

	case NUM_REAL2:
	case NUM_REAL:
	  {
	    int i, len;
	    sprintf(p, "%.14g", s7_double(l));
	    len = strlen(p);
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
	  sprintf(p, Int_d "/" Int_d, s7_numerator(l), s7_denominator(l));
	  break;

	default:
	  if (s7_imag_part(l) >= 0.0)
	    sprintf(p, "%.14g+%.14gi", s7_real_part(l), s7_imag_part(l));
	  else sprintf(p, "%.14g-%.14gi", s7_real_part(l), fabs(s7_imag_part(l)));
	  break;

	}
    } 
  else if (s7_is_string(l)) 
    {
      if (!f) 
	{
	  p = string_value(l);
	} 
      else 
	{ /* Hack, uses the fact that printing is needed */
	  *pp = sc->strbuff;
	  *plen = 0;
	  printslashstring(sc, string_value(l), string_length(l));
	  return;
	}
    } 
  else if (s7_is_character(l)) 
    {
      char c = s7_character(l);
      p = sc->strbuff;
      if (!f) 
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
	      if (c<32) 
		{
		  sprintf(p, "#\\x%x",c); break;
		}
	      sprintf(p, "#\\%c",c); break;
	    }
	}
    } 
  else if (s7_is_symbol(l)) 
    {
      p = s7_symbol_name(l);
    } 
  else if (s7_is_proc(l)) 
    {
      p = sc->strbuff;
      sprintf(p, "%s", procname(l));
    } 
  else if (is_macro(l)) 
    {
      p = "#<MACRO>";
    } 
  else if (s7_is_closure(l)) 
    {
      p = "#<CLOSURE>";
    } 
  else if (is_promise(l)) 
    {
      p = "#<PROMISE>";
    } 
  else if (s7_is_foreign_function(l)) 
    {
      p = sc->strbuff;
      sprintf(p, "#<FOREIGN PROCEDURE %d>", procedure_index(l));
    } 
  else if (s7_is_continuation(l)) 
    {
      p = "#<CONTINUATION>";
    } 
  else if (s7_is_foreign_object(l)) 
    {
      p = s7_describe_foreign_object(l);
    } 
  else 
    {
      p = "<error in atom->string!>";
    }
  *pp = p;
  *plen = strlen(p);
}


static void printatom(scheme *sc, pointer l, int f) 
{
  char *p;
  int len;
  s7_atom2str(sc, l, f, &p, &len);
  s7_putchars(sc, p, len);
}


static pointer vector_to_string(scheme *sc, pointer vect)
{
  pointer result;
  int i, len, bufsize = 0;
  bool too_long = false;
  char **elements = NULL;
  char *buf;
  len = vector_length(vect);
  if (len > 8) /* TODO: vector-print-length */
    {
      too_long = true;
      len = 8;
    }
  elements = (char **)MALLOC(len * sizeof(char *));
  for (i = 0; i < len; i++)
    {
      elements[i] = copy_string(string_value(s7_object_to_string(sc, s7_vector_ref(vect, i))));
      bufsize += strlen(elements[i]);
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
  result = s7_make_string(sc, buf);
  FREE(buf);
  return(result);
}

static pointer list_to_string(scheme *sc, pointer lst)
{
  pointer x, result;
  int i, len, bufsize = 0;
  char **elements = NULL;
  char *buf;
  len = s7_list_length(sc, lst);
  if (len <= 0)
    return(s7_make_string(sc, "unknown"));

  elements = (char **)MALLOC(len * sizeof(char *));
  for (x = lst, i = 0; s7_is_pair(x); i++, x = s7_cdr(x))
    {
      elements[i] = copy_string(string_value(s7_object_to_string(sc, car(x))));
      bufsize += strlen(elements[i]);
    }
  if (i != len)
    fprintf(stderr, "list->string list len: %d, but got %d elements?", len, i);

  bufsize += (128 + len); /* len spaces */
  buf = (char *)CALLOC(bufsize, sizeof(char));
  sprintf(buf, "(");
  for (i = 0; i < len - 1; i++)
    {
      strcat(buf, elements[i]);
      strcat(buf, " ");
    }
  strcat(buf, elements[len - 1]);
  strcat(buf, ")");

  for (i = 0; i < len; i++)
    FREE(elements[i]);
  FREE(elements);

  result = s7_make_string(sc, buf);
  FREE(buf);
  return(result);
}


/* ========== Routines for Evaluation Cycle ========== */

/* make closure. c is code. e is environment */
pointer s7_make_closure(scheme *sc, pointer c, pointer e) 
{
  pointer x = new_cell(sc);
  
  typeflag(x) = T_CLOSURE;
  car(x) = c;
  cdr(x) = e;
  return (x);
}

/* make continuation. */
pointer s7_make_continuation(scheme *sc, pointer d) 
{
  pointer x = new_cell(sc);
  /*
  fprintf(stderr,"make continuation %p %p ", x, d);
  */
  typeflag(x) = T_CONTINUATION;
  cont_dump(x) = d;
  return (x);
}

static pointer list_star(scheme *sc, pointer d) 
{
  pointer p, q;
  if (cdr(d) == sc->NIL) 
    return(car(d));

  p = cons(sc, car(d), cdr(d));
  q = p;
  while(cdr(cdr(p))!= sc->NIL) 
    {
      d = cons(sc, car(p), cdr(p));
      if (cdr(cdr(p)) != sc->NIL) 
	{
	  p = cdr(d);
	}
    }
  cdr(p) = car(cdr(p));
  return q;
}

/* reverse list -- produce new list */
pointer s7_reverse(scheme *sc, pointer a) 
{
  /* a must be checked by gc */
  pointer p = sc->NIL;
  
  for ( ; s7_is_pair(a); a = cdr(a)) 
    {
      p = cons(sc, car(a), p);
    }
  return (p);
}

/* reverse list --- in-place */
pointer s7_reverse_in_place(scheme *sc, pointer term, pointer list) 
{
  pointer p = list, result = term, q;
  
  while (p != sc->NIL) 
    {
      q = cdr(p);
      cdr(p) = result;
      result = p;
      p = q;
    }
  return (result);
}

/* append list -- produce new list */
pointer s7_append(scheme *sc, pointer a, pointer b) 
{
  pointer p = b, q;
  
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
  return (p);
}

static bool numbers_are_eqv(pointer a, pointer b)
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
bool s7_eqv_p(pointer a, pointer b) 
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
  
  if (s7_is_proc(a))
    return(procedure_index(a) == procedure_index(b));
  
  return(false);
}


/* true or false value macro */
/* () is #t in R5RS */

/* ========== Environment implementation  ========== */ 

#if !defined(USE_ALIST_ENV) || !defined(USE_OBJECT_LIST) 

static int hash_fn(const char *key, int table_size) 
{ 
  unsigned int hashed = 0; 
  const char *c; 
  int bits_per_int = sizeof(unsigned int)*8; 
  
  for (c = key; *c; c++) 
    { 
      /* letters have about 5 bits in them */ 
      hashed = (hashed<<5) | (hashed>>(bits_per_int-5)); 
      hashed ^= *c; 
    } 
  return hashed % table_size; 
} 
#endif 

#ifndef USE_ALIST_ENV 

/* 
 * In this implementation, each frame of the environment may be 
 * a hash table: a vector of alists hashed by variable name. 
 * In practice, we use a vector only for the initial frame; 
 * subsequent frames are too small and transient for the lookup 
 * speed to out-weigh the cost of making a new vector. 
 */ 

static void new_frame_in_env(scheme *sc, pointer old_env) 
{ 
  pointer new_frame; 
  
  /* The interaction-environment has about 300 variables in it. */ 
  if (old_env == sc->NIL) 
    new_frame = s7_make_vector(sc, 461); 
  else new_frame = sc->NIL; 
  
  sc->envir = immutable_cons(sc, new_frame, old_env); 
  setenvironment(sc->envir); 
} 


void s7_new_slot_spec_in_env(scheme *sc, pointer env, 
			     pointer variable, pointer value) 
{ 
  pointer slot = s7_immutable_cons(sc, variable, value); 
  
  if (s7_is_vector(car(env))) 
    { 
      int location = hash_fn(s7_symbol_name(variable), vector_length(car(env))); 
      
      s7_vector_set(car(env), 
		    location, 
		    s7_immutable_cons(sc, slot, s7_vector_ref(car(env), location))); 
    } 
  else 
    { 
      car(env) = s7_immutable_cons(sc, slot, car(env)); 
    } 
} 

pointer s7_find_slot_in_env(scheme *sc, pointer env, pointer hdl, int all) 
{ 
  pointer x,y; 
  int location; 
  
  for (x = env; x != sc->NIL; x = cdr(x)) 
    { 
      if (s7_is_vector(car(x))) 
	{ 
	  location = hash_fn(s7_symbol_name(hdl), vector_length(car(x))); 
	  y = s7_vector_ref(car(x), location); 
	} 
      else 
	{ 
	  y = car(x); 
	} 
      for ( ; y != sc->NIL; y = cdr(y)) 
	{ 
	  if (caar(y) == hdl) 
	    { 
	      break; 
	    } 
	} 
      if (y != sc->NIL) 
	{ 
	  break; 
	} 
      if (!all) 
	{ 
	  return sc->NIL; 
	} 
    } 
  if (x != sc->NIL) 
    { 
      return car(y); 
    } 
  return sc->NIL; 
} 


#else /* USE_ALIST_ENV */ 

static  void new_frame_in_env(scheme *sc, pointer old_env) 
{ 
  sc->envir = s7_immutable_cons(sc, sc->NIL, old_env); 
  setenvironment(sc->envir); 
} 

void s7_new_slot_spec_in_env(scheme *sc, pointer env, 
				  pointer variable, pointer value) 
{ 
  car(env) = s7_immutable_cons(sc, s7_immutable_cons(sc, variable, value), car(env)); 
} 

pointer s7_find_slot_in_env(scheme *sc, pointer env, pointer hdl, int all) 
{ 
  pointer x,y; 
  for (x = env; x != sc->NIL; x = cdr(x)) 
    { 
      for (y = car(x); y != sc->NIL; y = cdr(y)) 
	{ 
	  if (caar(y) == hdl) 
	    { 
	      break; 
	    } 
	} 
      if (y != sc->NIL) 
	{ 
	  break; 
	} 
      if (!all) 
	{ 
	  return sc->NIL; 
	} 
    } 
  if (x != sc->NIL) 
    { 
      return car(y); 
    } 
  return sc->NIL; 
} 

#endif /* USE_ALIST_ENV else */ 

static  void new_slot_in_env(scheme *sc, pointer variable, pointer value) 
{ 
  s7_new_slot_spec_in_env(sc, sc->envir, variable, value); 
} 

static  void set_slot_in_env(scheme *sc, pointer slot, pointer value) 
{ 
  cdr(slot) = value; 
} 

pointer s7_slot_value_in_env(pointer slot) 
{ 
  return cdr(slot); 
} 



/* ========== Evaluation Cycle ========== */


static pointer Error_1(scheme *sc, const char *s, pointer a) 
{
#if USE_ERROR_HOOK
  pointer x;
  pointer hdl = sc->ERROR_HOOK;
  /*
  fprintf(stderr, "error: %s ", s);
  fprintf(stderr, "\n");
  */
  x = s7_find_slot_in_env(sc, sc->envir, hdl, 1);
  if ((x != sc->NIL) &&
      (x != sc->F))
    {
      if (a != 0) 
	set_sc_code(sc, cons(sc, cons(sc, sc->QUOTE, cons(sc, a, sc->NIL)), sc->NIL));
      else set_sc_code(sc, sc->NIL);

      set_sc_code(sc, cons(sc, s7_make_string(sc, s), sc->code));
      s7_setimmutable(car(sc->code));
      set_sc_code(sc, cons(sc, s7_slot_value_in_env(x), sc->code)); 
      sc->op = OP_EVAL;
      return(sc->T);
    }
#endif
  
  if (a != 0) 
    {
      sc->args = cons(sc, a, sc->NIL);
    } 
  else 
    {
      sc->args = sc->NIL;
    }
  sc->args = cons(sc, s7_make_string(sc, (s)), sc->args);
  s7_setimmutable(car(sc->args));
  sc->op = OP_ERR0;
  return(sc->T);
}


#define s_goto(sc, a) do {sc->op = a; return(sc->T);} while (0)
#define s_return(sc, a) return _s_return(sc, a) 

#ifndef USE_SCHEME_STACK 

/* this structure holds all the interpreter's registers */ 
struct dump_stack_frame { 
  enum scheme_opcodes op; 
  pointer args; 
  pointer envir; 
  pointer code; 
}; 

#define STACK_GROWTH 3 

static void s_save(scheme *sc, enum scheme_opcodes op, pointer args, pointer code) 
{ 
  int nframes = (int)sc->dump; 
  struct dump_stack_frame *next_frame; 
  
  /* enough room for the next frame? */ 
  if (nframes >= sc->dump_size) 
    { 
      sc->dump_size += STACK_GROWTH; 
      sc->dump_base = REALLOC(sc->dump_base, 
			      sizeof(struct dump_stack_frame) * sc->dump_size); 
    } 
  next_frame = (struct dump_stack_frame *)sc->dump_base + nframes; 
  next_frame->op = op; 
  next_frame->args = args; 
  next_frame->envir = sc->envir; 
  next_frame->code = code; 
  sc->dump = (pointer)(nframes+1); 
} 

static pointer _s_return(scheme *sc, pointer a) 
{ 
  int nframes = (int)sc->dump; 
  struct dump_stack_frame *frame; 
  
  sc->value = (a); 
  if (nframes <= 0) 
    { 
      return sc->NIL; 
    } 
  nframes--; 
  frame = (struct dump_stack_frame *)sc->dump_base + nframes; 
  sc->op = frame->op; 
  sc->args = frame->args; 
  sc->envir = frame->envir; 
  set_sc_code(sc, frame->code); 
  sc->dump = (pointer)nframes; 
  return sc->T; 
} 

static  void dump_stack_reset(scheme *sc) 
{ 
  /* in this implementation, sc->dump is the number of frames on the stack */ 
  sc->dump = (pointer)0; 
} 

static  void dump_stack_initialize(scheme *sc) 
{ 
  sc->dump_size = 0; 
  sc->dump_base = NULL; 
  dump_stack_reset(sc); 
} 

static void dump_stack_free(scheme *sc) 
{ 
  FREE(sc->dump_base); 
  sc->dump_base = NULL; 
  sc->dump = (pointer)0; 
  sc->dump_size = 0; 
} 

static  void dump_stack_mark(scheme *sc) 
{ 
  int nframes = (int)sc->dump;
  int i;
  for(i = 0; i<nframes; i++) 
    {
      struct dump_stack_frame *frame;
      frame = (struct dump_stack_frame *)sc->dump_base + i;
      mark(frame->args);
      mark(frame->envir);
      mark(frame->code);
    } 
} 

#else 

static  void dump_stack_reset(scheme *sc) 
{ 
  sc->dump = sc->NIL; 
} 

static  void dump_stack_initialize(scheme *sc) 
{ 
  dump_stack_reset(sc); 
} 

static void dump_stack_free(scheme *sc) 
{ 
  sc->dump = sc->NIL; 
} 

static pointer _s_return(scheme *sc, pointer a) 
{ 
  sc->value = a; 
  if (sc->dump == sc->NIL) return sc->NIL; 
  sc->op = (enum scheme_opcodes)s7_integer(car(sc->dump)); /* passed in as int from s_save below */
  sc->args = cadr(sc->dump); 
  sc->envir = caddr(sc->dump); 
  set_sc_code(sc, cadddr(sc->dump)); 
  sc->dump = cddddr(sc->dump); 
  return sc->T; 
} 

static void s_save(scheme *sc, enum scheme_opcodes op, pointer args, pointer code) 
{ 
  sc->dump = cons(sc, sc->envir, cons(sc, (code), sc->dump)); 
  sc->dump = cons(sc, (args), sc->dump); 
  sc->dump = cons(sc, s7_make_integer(sc, (int)op), sc->dump); 
} 

static  void dump_stack_mark(scheme *sc) 
{ 
  mark(sc->dump); 
} 
#endif 

#define s_retbool(tf)    s_return(sc, (tf) ? sc->T : sc->F)

bool s7_is_equal(scheme *sc, pointer x, pointer y);

static bool vectors_equal(scheme *sc, pointer x, pointer y)
{
  int i, len;
  len = vector_length(x);
  if (len != vector_length(y)) return(false);
  for (i = 0; i < len; i++)
    if (!(s7_is_equal(sc, vector_element(x, i), vector_element(y, i))))
      return(false);
  return(true);
}

bool s7_is_equal(scheme *sc, pointer x, pointer y)
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
  
  if (s7_is_foreign_object(x))
    return(s7_equalp_foreign_objects(x, y));
  
  if (s7_is_proc(x))
    return(procedure_index(x) == procedure_index(y));
  
  if (s7_is_vector(x))
    return(vectors_equal(sc, x, y));
  
  if (s7_is_character(x)) 
    return(s7_character(x) == s7_character(y));
  
  if (s7_is_number(x))
    return(numbers_are_eqv(x, y));
  
  return(false); /* we already checked that x != y (port etc) */
}

static pointer opexe_0(scheme *sc, enum scheme_opcodes op) 
{
  pointer x, y;
  
  switch (op) 
    {
    case OP_LOAD:       /* load */
      if (file_interactive(sc)) 
	fprintf(sc->outport->object.port->rep.stdio.file, "Loading %s\n", string_value(car(sc->args)));

      if (!file_push(sc,string_value(car(sc->args)))) 
	Error_1(sc, "unable to open", car(sc->args));
      s_goto(sc, OP_T0LVL);
      
    case OP_T0LVL: /* top level */
      if (file_interactive(sc)) 
	s7_putstr(sc, "\n");

      sc->nesting = 0;
      dump_stack_reset(sc); 
      sc->envir = sc->global_env;
      sc->save_inport = sc->inport;
      sc->inport = sc->loadport;
      s_save(sc, OP_T0LVL, sc->NIL, sc->NIL);
      s_save(sc, OP_VALUEPRINT, sc->NIL, sc->NIL);
      s_save(sc, OP_T1LVL, sc->NIL, sc->NIL);

      if (file_interactive(sc)) 
	s7_putstr(sc, prompt);

      s_goto(sc, OP_READ_INTERNAL);
      
    case OP_T1LVL: /* top level */
      set_sc_code(sc, sc->value);
      sc->inport = sc->save_inport;
      s_goto(sc, OP_EVAL);
      
    case OP_READ_INTERNAL:       /* internal read */
      sc->tok = token(sc);
      if (sc->tok == TOK_EOF) 
	{
	  if (sc->inport == sc->loadport) 
	    {
	      sc->args = sc->NIL;
	      s_goto(sc, OP_QUIT);
	    } 
	  else s_return(sc, sc->EOF_OBJ);
	}
      s_goto(sc, OP_RDSEXPR);
      
    case OP_GENSYM:
      s_return(sc, s7_gensym(sc));
      
    case OP_VALUEPRINT: /* print evaluation result */
      /* OP_VALUEPRINT is always pushed, because when changing from
	 non-interactive to interactive mode, it needs to be
	 already on the stack */
      if (sc->tracing) 
	{
	  s7_putstr(sc, "\nGives: ");
	}
      if (file_interactive(sc)) 
	{
	  sc->print_flag = 1;
	  sc->args = sc->value;
	  s_goto(sc, OP_P0LIST);
	} 
      else 
	{
	  s_return(sc, sc->value);
	}
      
    case OP_EVAL:       /* main part of evaluation */
#if USE_TRACING
      if (sc->tracing) 
	{
	  /*s_save(sc, OP_VALUEPRINT, sc->NIL, sc->NIL);*/
	  s_save(sc, OP_REAL_EVAL, sc->args, sc->code);
	  sc->args = sc->code;
	  s7_putstr(sc, "\nEval: ");
	  s_goto(sc, OP_P0LIST);
	}
      /* fall through */
    case OP_REAL_EVAL:
#endif
      if (s7_is_symbol(sc->code)) 
	{    /* symbol */
	  x = s7_find_slot_in_env(sc, sc->envir, sc->code, 1);
	  if (x != sc->NIL) 
	    {
	      s_return(sc,s7_slot_value_in_env(x)); 
	    } 
	  else 
	    {
	      Error_1(sc, "eval: unbound variable:", sc->code);
	    }
	} 
      else if (s7_is_pair(sc->code)) 
	{
	  if (s7_is_syntax(x = car(sc->code))) 
	    {     /* SYNTAX */
	      set_sc_code(sc, cdr(sc->code));
	      s_goto(sc, syntaxnum(x));
	    } 
	  else 
	    {/* first, eval top element and eval arguments */
	      s_save(sc, OP_E0ARGS, sc->NIL, sc->code);
	      /* If no macros => s_save(sc, OP_E1ARGS, sc->NIL, cdr(sc->code));*/
	      set_sc_code(sc, car(sc->code));
	      s_goto(sc, OP_EVAL);
	    }
	} 
      else 
	{
	  s_return(sc, sc->code);
	}
      
    case OP_E0ARGS:     /* eval arguments */
      if (is_macro(sc->value)) 
	{    /* macro expansion */
	  s_save(sc, OP_DOMACRO, sc->NIL, sc->NIL);
	  sc->args = cons(sc, sc->code, sc->NIL);
	  set_sc_code(sc, sc->value);
	  s_goto(sc, OP_APPLY);
	} 
      else 
	{
	  set_sc_code(sc, cdr(sc->code));
	  s_goto(sc, OP_E1ARGS);
	}
      
    case OP_E1ARGS:     /* eval arguments */
      sc->args = cons(sc, sc->value, sc->args);
      if (s7_is_pair(sc->code)) 
	{ /* continue */
	  s_save(sc, OP_E1ARGS, sc->args, cdr(sc->code));
	  set_sc_code(sc, car(sc->code));
	  sc->args = sc->NIL;
	  s_goto(sc, OP_EVAL);
	} 
      else 
	{  /* end */
	  sc->args = s7_reverse_in_place(sc, sc->NIL, sc->args); 
	  set_sc_code(sc, car(sc->args));
	  sc->args = cdr(sc->args);
	  s_goto(sc, OP_APPLY);
	}
      
      
#if USE_TRACING
    case OP_TRACING: 
      {
	bool tr = sc->tracing;
	sc->tracing = (bool)s7_integer(car(sc->args));
	s_return(sc, s7_make_integer(sc, tr));
      }
#endif
      
    case OP_APPLY:      /* apply 'code' to 'args' */

#if USE_TRACING
      if (sc->tracing) 
	{
	  s_save(sc, OP_REAL_APPLY, sc->args, sc->code);
	  sc->print_flag = 1;
	  /*	 sc->args = cons(sc, sc->code, sc->args);*/
	  s7_putstr(sc, "\nApply to: ");
	  s_goto(sc, OP_P0LIST);
	}
      /* fall through */
    case OP_REAL_APPLY:
#endif
      if (s7_is_proc(sc->code)) 
	{
	  s_goto(sc, procedure_index(sc->code));   /* PROCEDURE */
	} 
      else 
	{
	  if (s7_is_foreign_function(sc->code)) 
	    {
	      x = sc->code->object.ffunc.ff(sc, sc->args);
	      s_return(sc, x);
	    } 
      
      /* TODO: if foreign_object getter */
      
	  else 
	    {
	      if (s7_is_closure(sc->code) || is_macro(sc->code) || is_promise(sc->code)) 
		{ /* CLOSURE */
		  /* Should not accept promise */
		  /* make environment */
		  new_frame_in_env(sc, s7_closure_env(sc->code)); 

		  /* s7_is_pair(x) here assumes the func has more than 1 arg? */

		  for (x = car(s7_closure_code(sc->code)), y = sc->args; s7_is_pair(x); x = cdr(x), y = cdr(y)) 
		    {
		      if (y == sc->NIL) 
			{
			  Error_0(sc, "not enough arguments");
			} 
		      else 
			{
			  new_slot_in_env(sc, car(x), car(y)); 
			}
		    }
		  if (x == sc->NIL) 
		    {
		      /* why commented out? */

		      /*--
		       * if (y != sc->NIL) 
		       {
		       *   Error_0(sc, "too many arguments");
		       * }
		       */
		    } 
		  else 
		    {
		      if (s7_is_symbol(x))
			new_slot_in_env(sc, x, y); 
		      else 
			{
			  Error_1(sc, "syntax error in closure: not a symbol:", x); 
			}
		    }
		  set_sc_code(sc, cdr(s7_closure_code(sc->code)));
		  sc->args = sc->NIL;
		  s_goto(sc, OP_BEGIN);
		}
	      else 
		{
		  if (s7_is_continuation(sc->code)) 
		    { /* CONTINUATION */

		      sc->dump = cont_dump(sc->code);
		      /*
		      fprintf(stderr,"in apply of continuation %p %p ", sc->dump, sc->code);
		      */
		      s_return(sc, sc->args != sc->NIL ? car(sc->args) : sc->NIL);
		    } 
		  else 
		    {
		      fprintf(stderr, "bad entry, type: %d, obj: ", type(sc->code));
		      dump_object(sc, sc->code);

		      Error_0(sc, "illegal function");
		    }
		}
	    }
	}
      
    case OP_DOMACRO:    /* do macro */
      set_sc_code(sc, sc->value);
      s_goto(sc, OP_EVAL);
      
    case OP_LAMBDA:     /* lambda */
      s_return(sc, s7_make_closure(sc, sc->code, sc->envir));
      
    case OP_MKCLOSURE: /* make-closure */
      x = car(sc->args);
      if (car(x) == sc->LAMBDA) 
	x = cdr(x);

      if (cdr(sc->args) == sc->NIL) 
	y = sc->envir;
      else y = cadr(sc->args);

      s_return(sc, s7_make_closure(sc, x, y));
      
    case OP_QUOTE:      /* quote */
      x = car(sc->code); /* ??? */
      s_return(sc, car(sc->code));
      
    case OP_DEF0:  /* define */
      if (s7_is_immutable(car(sc->code)))
	Error_1(sc, "define: unable to alter immutable", car(sc->code));
      
      if (s7_is_pair(car(sc->code))) 
	{
	  x = caar(sc->code);
	  set_sc_code(sc, cons(sc, sc->LAMBDA, cons(sc, cdar(sc->code), cdr(sc->code))));
	} 
      else 
	{
	  x = car(sc->code);
	  set_sc_code(sc, cadr(sc->code));
	}
      if (!s7_is_symbol(x)) 
	Error_0(sc, "define of a non-symbol?");

      s_save(sc, OP_DEF1, sc->NIL, x);
      s_goto(sc, OP_EVAL);
      
    case OP_DEF1:  /* define */
      x = s7_find_slot_in_env(sc, sc->envir, sc->code, 0);
      if (x != sc->NIL) 
	set_slot_in_env(sc, x, sc->value); 
      else new_slot_in_env(sc, sc->code, sc->value); 
      s_return(sc, sc->code);
      
    case OP_DEFP:  /* defined? */
      x = sc->envir;
      if (cdr(sc->args) != sc->NIL) 
	x = cadr(sc->args);
      s_retbool(s7_find_slot_in_env(sc, x, car(sc->args), 1) != sc->NIL);
      
    case OP_SET0:       /* set! */
      if (s7_is_immutable(car(sc->code)))
	Error_1(sc, "set!: unable to alter immutable variable", car(sc->code));
      
      if (s7_is_pair(car(sc->code))) /* has accessor */
	{
	  char *sym, *name;
	  name = s7_symbol_name(caar(sc->code));
	  sym = (char *)CALLOC(strlen(name) + 6, sizeof(char));
	  sprintf(sym, "set-%s", name);
	  caar(sc->code) = s7_make_symbol(sc, sym);               /* [set!] ((x a b...) y) -> ((set-x a b..) y) */
	  set_sc_code(sc, s7_append(sc, car(sc->code), cdr(sc->code))); /* -> (set-x a b ... y) */
	}
      else 
	{
	  s_save(sc, OP_SET1, sc->NIL, car(sc->code));
	  set_sc_code(sc, cadr(sc->code));
	}
      s_goto(sc, OP_EVAL);
      
    case OP_SET1:      
      y = s7_find_slot_in_env(sc, sc->envir, sc->code, 1);
      if (y != sc->NIL) 
	{
	  set_slot_in_env(sc, y, sc->value); 
	  s_return(sc, sc->value);
	}
      else Error_1(sc, "set!: unbound variable:", sc->code); 
      
    case OP_BEGIN:      /* begin */
      if (!s7_is_pair(sc->code)) 
	s_return(sc, sc->code);

      if (cdr(sc->code) != sc->NIL) 
	s_save(sc, OP_BEGIN, sc->NIL, cdr(sc->code));

      set_sc_code(sc, car(sc->code));
      s_goto(sc, OP_EVAL);
      
    case OP_IF0:        /* if */
      s_save(sc, OP_IF1, sc->NIL, cdr(sc->code));
      set_sc_code(sc, car(sc->code));
      s_goto(sc, OP_EVAL);
      
    case OP_IF1:        /* if */
      if (is_true(sc->value))
	set_sc_code(sc, car(sc->code));
      else
	set_sc_code(sc, cadr(sc->code));  /* (if #f 1) ==> () because
				     * car(sc->NIL) = sc->NIL */
      s_goto(sc, OP_EVAL);
      
    case OP_LET0:       /* let */
      sc->args = sc->NIL;
      sc->value = sc->code;
      set_sc_code(sc, s7_is_symbol(car(sc->code)) ? cadr(sc->code) : car(sc->code));
      s_goto(sc, OP_LET1);
      
    case OP_LET1:       /* let (calculate parameters) */
      sc->args = cons(sc, sc->value, sc->args);
      if (s7_is_pair(sc->code)) 
	{ /* continue */
	  s_save(sc, OP_LET1, sc->args, cdr(sc->code));
	  set_sc_code(sc, cadar(sc->code));
	  sc->args = sc->NIL;
	  s_goto(sc, OP_EVAL);
	} 
      else 
	{  /* end */
	  sc->args = s7_reverse_in_place(sc, sc->NIL, sc->args);
	  set_sc_code(sc, car(sc->args));
	  sc->args = cdr(sc->args);
	  s_goto(sc, OP_LET2);
	}
      
    case OP_LET2:       /* let */
      new_frame_in_env(sc, sc->envir); 
      for (x = s7_is_symbol(car(sc->code)) ? cadr(sc->code) : car(sc->code), y = sc->args;
	   y != sc->NIL; x = cdr(x), y = cdr(y)) 
	{
	  new_slot_in_env(sc, caar(x), car(y)); 
	}
      if (s7_is_symbol(car(sc->code))) 
	{    /* named let */
	  for (x = cadr(sc->code), sc->args = sc->NIL; x != sc->NIL; x = cdr(x)) 
	    sc->args = cons(sc, caar(x), sc->args);

	  x = s7_make_closure(sc, cons(sc, s7_reverse_in_place(sc, sc->NIL, sc->args), cddr(sc->code)), sc->envir); 
	  new_slot_in_env(sc, car(sc->code), x); 
	  set_sc_code(sc, cddr(sc->code));
	  sc->args = sc->NIL;
	} 
      else 
	{
	  set_sc_code(sc, cdr(sc->code));
	  sc->args = sc->NIL;
	}
      s_goto(sc, OP_BEGIN);
      
    case OP_LET0AST:    /* let* */
      if (car(sc->code) == sc->NIL) 
	{
	  new_frame_in_env(sc, sc->envir); 
	  set_sc_code(sc, cdr(sc->code));
	  s_goto(sc, OP_BEGIN);
	}
      s_save(sc, OP_LET1AST, cdr(sc->code), car(sc->code));
      set_sc_code(sc, cadaar(sc->code));
      s_goto(sc, OP_EVAL);
      
    case OP_LET1AST:    /* let* (make new frame) */
      new_frame_in_env(sc, sc->envir); 
      s_goto(sc, OP_LET2AST);
      
    case OP_LET2AST:    /* let* (calculate parameters) */
      new_slot_in_env(sc, caar(sc->code), sc->value); 
      set_sc_code(sc, cdr(sc->code));
      if (s7_is_pair(sc->code)) 
	{ /* continue */
	  s_save(sc, OP_LET2AST, sc->args, sc->code);
	  set_sc_code(sc, cadar(sc->code));
	  sc->args = sc->NIL;
	  s_goto(sc, OP_EVAL);
	} 
      else 
	{  /* end */
	  set_sc_code(sc, sc->args);
	  sc->args = sc->NIL;
	  s_goto(sc, OP_BEGIN);
	}
    default:
      sprintf(sc->strbuff, "%d: illegal operator", sc->op);
      Error_0(sc, sc->strbuff);
    }
  return sc->T;
}

static pointer opexe_1(scheme *sc, enum scheme_opcodes op) 
{
  pointer x, y;
  
  switch (op) 
    {
    case OP_LET0REC:    /* letrec */
      new_frame_in_env(sc, sc->envir); 
      sc->args = sc->NIL;
      sc->value = sc->code;
      set_sc_code(sc, car(sc->code));
      s_goto(sc, OP_LET1REC);
      
    case OP_LET1REC:    /* letrec (calculate parameters) */
      sc->args = cons(sc, sc->value, sc->args);
      if (s7_is_pair(sc->code)) 
	{ /* continue */
	  s_save(sc, OP_LET1REC, sc->args, cdr(sc->code));
	  set_sc_code(sc, cadar(sc->code));
	  sc->args = sc->NIL;
	  s_goto(sc, OP_EVAL);
	} 
      else 
	{  /* end */
	  sc->args = s7_reverse_in_place(sc, sc->NIL, sc->args); 
	  set_sc_code(sc, car(sc->args));
	  sc->args = cdr(sc->args);
	  s_goto(sc, OP_LET2REC);
	}
      
    case OP_LET2REC:    /* letrec */
      for (x = car(sc->code), y = sc->args; y != sc->NIL; x = cdr(x), y = cdr(y)) 
	{
	  new_slot_in_env(sc, caar(x), car(y)); 
	}
      set_sc_code(sc, cdr(sc->code));
      sc->args = sc->NIL;
      s_goto(sc, OP_BEGIN);
      
    case OP_COND0:      /* cond */
      if (!s7_is_pair(sc->code)) 
	{
	  Error_0(sc, "syntax error in cond");
	}
      s_save(sc, OP_COND1, sc->NIL, sc->code);
      set_sc_code(sc, caar(sc->code));
      s_goto(sc, OP_EVAL);
      
    case OP_COND1:      /* cond */
      if (is_true(sc->value)) 
	{
	  if (set_sc_code(sc, cdar(sc->code)) == sc->NIL)
	    {
	      s_return(sc, sc->value);
	    }
	  if (car(sc->code) == sc->FEED_TO) 
	    {
	      if (!s7_is_pair(cdr(sc->code))) 
		{
		  Error_0(sc, "syntax error in cond");
		}
	      x = cons(sc, sc->QUOTE, cons(sc, sc->value, sc->NIL));
	      set_sc_code(sc, cons(sc,cadr(sc->code),cons(sc,x, sc->NIL)));
	      s_goto(sc, OP_EVAL);
	    }
	  s_goto(sc, OP_BEGIN);
	} 
      else 
	{
	  if (set_sc_code(sc, cdr(sc->code)) == sc->NIL)
	    {
	      s_return(sc, sc->NIL);
	    } 
	  else 
	    {
	      s_save(sc, OP_COND1, sc->NIL, sc->code);
	      set_sc_code(sc, caar(sc->code));
	      s_goto(sc, OP_EVAL);
	    }
	}
      
    case OP_DELAY:      /* delay */
      x = s7_make_closure(sc, cons(sc, sc->NIL, sc->code), sc->envir);
      typeflag(x) = T_PROMISE;
      s_return(sc,x);
      
    case OP_AND0:       /* and */
      if (sc->code == sc->NIL) 
	s_return(sc, sc->T);

      s_save(sc, OP_AND1, sc->NIL, cdr(sc->code));
      set_sc_code(sc, car(sc->code));
      s_goto(sc, OP_EVAL);
      
    case OP_AND1:       /* and */
      if (is_false(sc->value)) 
	s_return(sc, sc->value);

      if (sc->code == sc->NIL) 
	s_return(sc, sc->value);

      s_save(sc, OP_AND1, sc->NIL, cdr(sc->code));
      set_sc_code(sc, car(sc->code));
      s_goto(sc, OP_EVAL);
      
    case OP_OR0:        /* or */
      if (sc->code == sc->NIL) 
	s_return(sc, sc->F);

      s_save(sc, OP_OR1, sc->NIL, cdr(sc->code));
      set_sc_code(sc, car(sc->code));
      s_goto(sc, OP_EVAL);
      
    case OP_OR1:        /* or */
      if (is_true(sc->value)) 
	s_return(sc, sc->value);

      if (sc->code == sc->NIL) 
	s_return(sc, sc->value);

      s_save(sc, OP_OR1, sc->NIL, cdr(sc->code));
      set_sc_code(sc, car(sc->code));
      s_goto(sc, OP_EVAL);
      
    case OP_C0STREAM:   /* cons-stream */
      s_save(sc, OP_C1STREAM, sc->NIL, cdr(sc->code));
      set_sc_code(sc, car(sc->code));
      s_goto(sc, OP_EVAL);
      
    case OP_C1STREAM:   /* cons-stream */
      sc->args = sc->value;  /* save sc->value to register sc->args for gc */
      x = s7_make_closure(sc, cons(sc, sc->NIL, sc->code), sc->envir);
      typeflag(x) = T_PROMISE;
      s_return(sc,cons(sc, sc->args, x));
      
    case OP_MACRO0:     /* macro */
      if (s7_is_pair(car(sc->code))) 
	{
	  x = caar(sc->code);
	  set_sc_code(sc, cons(sc, sc->LAMBDA, cons(sc, cdar(sc->code), cdr(sc->code))));
	} 
      else 
	{
	  x = car(sc->code);
	  set_sc_code(sc, cadr(sc->code));
	}
      if (!s7_is_symbol(x)) 
	Error_0(sc, "variable is not a symbol");

      s_save(sc, OP_MACRO1, sc->NIL, x);
      s_goto(sc, OP_EVAL);
      
    case OP_MACRO1:     /* macro */
      typeflag(sc->value) = T_MACRO;
      x = s7_find_slot_in_env(sc, sc->envir, sc->code, 0); 
      if (x != sc->NIL) 
	set_slot_in_env(sc, x, sc->value); 
      else new_slot_in_env(sc, sc->code, sc->value); 
      s_return(sc, sc->code);
      
    case OP_CASE0:      /* case */
      s_save(sc, OP_CASE1, sc->NIL, cdr(sc->code));
      set_sc_code(sc, car(sc->code));
      s_goto(sc, OP_EVAL);
      
    case OP_CASE1:      /* case */
      for (x = sc->code; x != sc->NIL; x = cdr(x)) 
	{
	  if (!s7_is_pair(y = caar(x))) 
	    {
	      break;
	    }
	  for ( ; y != sc->NIL; y = cdr(y)) 
	    {
	      if (s7_eqv_p(car(y), sc->value)) 
		{
		  break;
		}
	    }
	  if (y != sc->NIL) 
	    {
	      break;
	    }
	}
      if (x != sc->NIL) 
	{
	  if (s7_is_pair(caar(x))) 
	    {
	      set_sc_code(sc, cdar(x));
	      s_goto(sc, OP_BEGIN);
	    } 
	  else 
	    {/* else */
	      s_save(sc, OP_CASE2, sc->NIL, cdar(x));
	      set_sc_code(sc, caar(x));
	      s_goto(sc, OP_EVAL);
	    }
	} 
      else 
	{
	  s_return(sc, sc->NIL);
	}
      
    case OP_CASE2:      /* case */
      if (is_true(sc->value)) 
	{
	  s_goto(sc, OP_BEGIN);
	} 
      else 
	{
	  s_return(sc, sc->NIL);
	}
      
    case OP_PAPPLY:     /* apply */
      set_sc_code(sc, car(sc->args));
      sc->args = list_star(sc, cdr(sc->args));
      s_goto(sc, OP_APPLY);
      
    case OP_PEVAL: /* eval */
      if (cdr(sc->args)!= sc->NIL) 
	{
	  sc->envir= cadr(sc->args);
	}
      set_sc_code(sc, car(sc->args));
      s_goto(sc, OP_EVAL);
      
    case OP_CONTINUATION:    /* call-with-current-continuation */
      set_sc_code(sc, car(sc->args));
      sc->args = cons(sc, s7_make_continuation(sc, sc->dump), sc->NIL);
      /* fprintf(stderr, "call/cc... %p %p %p ", sc->code, sc->dump, car(sc->args)); */
      s_goto(sc, OP_APPLY);
      
    default:
      sprintf(sc->strbuff, "opexe_1 %d: illegal operator", sc->op);
      Error_0(sc, sc->strbuff);
    }
  return sc->T;
}

static pointer opexe_2(scheme *sc, enum scheme_opcodes op) 
{
  pointer x;
  num v;
  double dd;
  
  switch (op) 
    {
    case OP_REAL_PART:     /* real-part */
      x = car(sc->args);
      s_return(sc, s7_make_real(sc, s7_real_part(x)));
      
    case OP_IMAG_PART:     /* imag-part */
      x = car(sc->args);
      s_return(sc, s7_make_real(sc, s7_imag_part(x)));
      
    case OP_NUMERATOR:     /* numerator */
      x = car(sc->args);
      s_return(sc, s7_make_integer(sc, num_to_numerator(x->object.number)));

    case OP_DENOMINATOR:     /* denominator */
      x = car(sc->args);
      s_return(sc, s7_make_integer(sc, num_to_denominator(x->object.number)));

    case OP_MAKE_RECTANGULAR:     /* make-rectangular */
      s_return(sc, s7_make_complex(sc, 
				   num_to_real((car(sc->args))->object.number), 
				   num_to_real((cadr(sc->args))->object.number)));

    case OP_MAKE_POLAR:
      {
	double s, c, ang, mag;
	mag = num_to_real((car(sc->args))->object.number);
	ang = num_to_real((cadr(sc->args))->object.number);
	s = sin(ang);
	c = cos(ang);
	s_return(sc, s7_make_complex(sc, mag * c, mag * s));
      }

    case OP_ANGLE:
      x = car(sc->args);

      if (!s7_is_real(x))
	s_return(sc, s7_make_real(sc, atan2(s7_imag_part(x), s7_real_part(x))));
      if (num_to_real(x->object.number) < 0.0)
	s_return(sc, s7_make_real(sc, atan2(0.0, -1.0)));

      s_return(sc, s7_make_real(sc, 0.0));

    case OP_MAGNITUDE:
      v = nvalue(car(sc->args));
      switch (num_type(v))
	{
	case NUM_REAL2: 
	case NUM_REAL: 
	  real(v) = fabs(real(v)); 
	  break;
	  
	case NUM_INT:
	  integer(v) = abs(integer(v));
	  break;
	case NUM_RATIO:
	  numerator(v) = abs(numerator(v));
	  break;

	default:
	  {
	    double a, b;
	    a = imag_part(v);
	    b = real_part(v);
	    v.type = NUM_REAL;
	    real(v) = hypot(a, b);
	  }
	  break;
	}
	  
      s_return(sc, make_number(sc, v));

    case OP_EXACTP:     /* exact? */
      s_retbool(s7_is_exact(car(sc->args)));

    case OP_INEXACTP:     /* exact? */
      s_retbool(s7_is_inexact(car(sc->args)));

    case OP_EVENP:     /* even? */
      x = car(sc->args);
      s_retbool((s7_integer(x) & 1) == 0);

    case OP_ODDP:     /* odd? */
      x = car(sc->args);
      s_retbool((s7_integer(x) & 1) == 1);

    case OP_POSITIVEP:     /* positive? */
      x = car(sc->args);
      switch (object_number_type(x))
	{
	case NUM_INT:   s_retbool(s7_integer(x) > 0);
	case NUM_RATIO: s_retbool(s7_numerator(x) > 0);
	default:        s_retbool(s7_double(x) > 0);
	}

    case OP_NEGATIVEP:     /* negative? */
      x = car(sc->args);
      switch (object_number_type(x))
	{
	case NUM_INT:   s_retbool(s7_integer(x) < 0);
	case NUM_RATIO: s_retbool(s7_numerator(x) < 0);
	default:        s_retbool(s7_double(x) < 0);
	}

    case OP_ZEROP:     /* zero? */
      x = car(sc->args);
      switch (object_number_type(x))
	{
	case NUM_INT:   s_retbool(s7_integer(x) == 0);
	case NUM_REAL2:
	case NUM_REAL:  s_retbool(s7_double(x) == 0.0);
	case NUM_RATIO: s_retbool(s7_numerator(x) == 0);
	default:        s_retbool((s7_real_part(x) == 0.0) &&
				  (s7_imag_part(x) == 0.0));
	}

    case OP_RATIONALIZE:    /* rationalize */
      {
	double error = DEFAULT_RATIONALIZE_ERROR;
	Int numer = 0, denom = 1;
	x = car(sc->args);
	if (s7_is_exact(x)) 
	  s_return(sc, x);

	if (cdr(sc->args) != sc->NIL)
	  error = real((cdr(sc->args))->object.number);
	if (c_rationalize(s7_double(x), error, &numer, &denom))
	  s_return(sc, s7_make_ratio(sc, numer, denom));
	else Error_1(sc, "rationalize did not converge?!?", x);
      }

    case OP_INEX2EX:    /* inexact->exact */
      x = car(sc->args);
      if (s7_is_exact(x)) 
	s_return(sc, x);
      {
	Int numer = 0, denom = 1;
	if (c_rationalize(s7_double(x), DEFAULT_RATIONALIZE_ERROR, &numer, &denom))
	  s_return(sc, s7_make_ratio(sc, numer, denom));
	else s_return(sc, s7_make_integer(sc, (Int)s7_double(x)));
      }
      
    case OP_EX2INEX:    /* exact->inexact */
      x = car(sc->args);
      if (s7_is_inexact(x)) 
	s_return(sc, x);

      if (s7_is_integer(x))
	s_return(sc, s7_make_real(sc, (double)s7_integer(x)));
      else s_return(sc, s7_make_real(sc, fraction(x->object.number)));

      
    case OP_ABS:
      v = nvalue(car(sc->args));
      if (num_type(v) >= NUM_REAL)
	real(v) = fabs(real(v));
      else
	{
	  if (num_type(v) == NUM_INT)
	    integer(v) = abs(integer(v));
	  else numerator(v) = abs(numerator(v));
	}
      s_return(sc, make_number(sc, v));      

    case OP_EXP:
      x = car(sc->args);
      if (s7_is_real(x))
	s_return(sc, s7_make_real(sc, exp(num_to_real(x->object.number))));
      s_return(sc, s7_from_c_complex(sc, cexp(s7_complex(x))));
      
    case OP_LOG:
      x = car(sc->args);
      if ((s7_is_real(x)) &&
	  (num_to_real(x->object.number) > 0.0))
	s_return(sc, s7_make_real(sc, log(num_to_real(x->object.number))));
      /* if < 0 use log(-x) + pi*i */
      s_return(sc, s7_from_c_complex(sc, clog(s7_complex(x))));
      
    case OP_SIN:
      x = car(sc->args);
      if (s7_is_real(x))
	s_return(sc, s7_make_real(sc, sin(num_to_real(x->object.number))));
      s_return(sc, s7_from_c_complex(sc, csin(s7_complex(x))));
      
    case OP_COS:
      x = car(sc->args);
      if (s7_is_real(x))
	s_return(sc, s7_make_real(sc, cos(num_to_real(x->object.number))));
      s_return(sc, s7_from_c_complex(sc, ccos(s7_complex(x))));
      
    case OP_TAN:
      x = car(sc->args);
      if (s7_is_real(x))
	s_return(sc, s7_make_real(sc, tan(num_to_real(x->object.number))));
      s_return(sc, s7_from_c_complex(sc, ctan(s7_complex(x))));
      
    case OP_ASIN:
      x = car(sc->args);
      if ((s7_is_real(x)) &&
	  (fabs(num_to_real(x->object.number) <= 1.0)))
	s_return(sc, s7_make_real(sc, asin(num_to_real(x->object.number))));
      s_return(sc, s7_from_c_complex(sc, casin(s7_complex(x))));
      
    case OP_ACOS:
      x = car(sc->args);
      if ((s7_is_real(x)) &&
	  (fabs(num_to_real(x->object.number) <= 1.0)))
	s_return(sc, s7_make_real(sc, acos(num_to_real(x->object.number))));
      s_return(sc, s7_from_c_complex(sc, cacos(s7_complex(x))));
      
    case OP_ATAN:
      x = car(sc->args);
      if (cdr(sc->args) == sc->NIL) 
	{
	  if (s7_is_real(x))
	    s_return(sc, s7_make_real(sc, atan(num_to_real(x->object.number))));
      s_return(sc, s7_from_c_complex(sc, catan(s7_complex(x))));
	} 
      else 
	{
	  pointer y = cadr(sc->args);
	  s_return(sc, s7_make_real(sc, atan2(num_to_real(x->object.number), num_to_real(y->object.number))));
	  /* atan2 args should be real */
	}
      
    case OP_SINH:
      x = car(sc->args);
      if (s7_is_real(x))
	s_return(sc, s7_make_real(sc, sinh(num_to_real(x->object.number))));
      s_return(sc, s7_from_c_complex(sc, csinh(s7_complex(x))));
      
    case OP_COSH:
      x = car(sc->args);
      if (s7_is_real(x))
	s_return(sc, s7_make_real(sc, cosh(num_to_real(x->object.number))));
      s_return(sc, s7_from_c_complex(sc, ccosh(s7_complex(x))));
      
    case OP_TANH:
      x = car(sc->args);
      if (s7_is_real(x))
	s_return(sc, s7_make_real(sc, tanh(num_to_real(x->object.number))));
      s_return(sc, s7_from_c_complex(sc, ctanh(s7_complex(x))));
      
    case OP_ASINH:
      x = car(sc->args);
      if (s7_is_real(x))
	s_return(sc, s7_make_real(sc, asinh(num_to_real(x->object.number))));
      s_return(sc, s7_from_c_complex(sc, casinh(s7_complex(x))));
      
    case OP_ACOSH:
      x = car(sc->args);
      if ((s7_is_real(x)) &&
	  (num_to_real(x->object.number) >= 1.0))
	s_return(sc, s7_make_real(sc, acosh(num_to_real(x->object.number))));
      s_return(sc, s7_from_c_complex(sc, cacosh(s7_complex(x))));
      
    case OP_ATANH:
      x = car(sc->args);
      if ((s7_is_real(x)) &&
	  (fabs(num_to_real(x->object.number) < 1.0)))
	s_return(sc, s7_make_real(sc, atanh(num_to_real(x->object.number))));
      s_return(sc, s7_from_c_complex(sc, catanh(s7_complex(x))));
      
    case OP_SQRT:
      x = car(sc->args);
      if ((s7_is_real(x)) &&
	  (num_to_real(x->object.number) >= 0.0))
	s_return(sc, s7_make_real(sc, sqrt(num_to_real(x->object.number))));
      /* if < 0 use sqrt(-num)*i */
      s_return(sc, s7_from_c_complex(sc, csqrt(s7_complex(x))));
      
    case OP_EXPT:
      {
	pointer y;
	x = car(sc->args);
	y = cadr(sc->args);

	if ((s7_is_integer(x)) &&
	    (s7_is_integer(y)) &&
	    ((s7_integer(y) >= 0) || 
	     (abs(s7_integer(x)) == 1)))
	  s_return(sc, s7_make_integer(sc, (Int)pow(s7_integer(x), s7_integer(y))));

	if ((s7_is_real(x)) &&
	    (s7_is_real(y)) &&
	    (num_to_real(y->object.number) >= 0.0))
	  s_return(sc, s7_make_real(sc, pow(num_to_real(x->object.number), num_to_real(y->object.number))));

	s_return(sc, s7_from_c_complex(sc, cpow(s7_complex(x), s7_complex(y))));
      }
      
    case OP_FLOOR:
      x = car(sc->args);
      s_return(sc, s7_make_integer(sc, (Int)floor(num_to_real(x->object.number)))); /* used to be real result */
      
    case OP_CEILING:
      x = car(sc->args);
      s_return(sc, s7_make_integer(sc, (Int)ceil(num_to_real(x->object.number))));
      
    case OP_TRUNCATE: 
      {
	double xf;
	x = car(sc->args);
	xf = num_to_real(x->object.number);
	if (xf > 0) 
	  s_return(sc, s7_make_integer(sc, (Int)floor(xf)));
	s_return(sc, s7_make_integer(sc, (Int)ceil(xf)));
      }
      
    case OP_LCM:
      { /* TODO: fix (and gcd) for new case */
	Int val = 1;
	for (x = sc->args; x != sc->NIL; x = cdr(x)) 
	  {
	    val = c_lcm(val, s7_integer(car(x)));
	    if (val == 0)
	      s_return(sc, s7_make_integer(sc, 0));
	  }
	s_return(sc, s7_make_integer(sc, val));
      }
      
    case OP_GCD:
      {
	Int val = 0;
	for (x = sc->args; x != sc->NIL; x = cdr(x)) 
	  {
	    val = c_gcd(val, s7_integer(car(x)));
	    if (val == 1)
	      s_return(sc, s7_make_integer(sc, 1));
	  }
	s_return(sc, s7_make_integer(sc, val));
      }
      
    case OP_ROUND:     /* round */ /* TODO: int here */
      x = car(sc->args);
      s_return(sc, s7_make_real(sc, round_per_R5RS(num_to_real(x->object.number))));
      
    case OP_MAX:        /* max */
      v = nvalue(car(sc->args));
      for (x = cdr(sc->args); x != sc->NIL; x = cdr(x)) 
	v = num_max(sc, v, nvalue(car(x)));
      s_return(sc, make_number(sc, v));
      
    case OP_MIN:        /* min */
      v = nvalue(car(sc->args));
      for (x = cdr(sc->args); x != sc->NIL; x = cdr(x)) 
	v = num_min(sc, v, nvalue(car(x)));
      s_return(sc, make_number(sc, v));
      
    case OP_ADD:        /* + */
      v = num_zero;
      for (x = sc->args; x != sc->NIL; x = cdr(x)) 
	v = num_add(sc, v, nvalue(car(x)));
      s_return(sc, make_number(sc, v));
      
    case OP_MUL:        /* * */
      v = num_one;
      for (x = sc->args; x != sc->NIL; x = cdr(x)) 
	v = num_mul(sc, v, nvalue(car(x)));
      s_return(sc, make_number(sc, v));
      
    case OP_SUB:        /* - */
      if (cdr(sc->args) == sc->NIL) 
	{
	  x = sc->args;
	  v = num_zero;
	} 
      else 
	{
	  x = cdr(sc->args);
	  v = nvalue(car(sc->args));
	}
      for (; x != sc->NIL; x = cdr(x)) 
	v = num_sub(sc, v, nvalue(car(x)));
      s_return(sc, make_number(sc, v));
      
    case OP_DIV:        /* / */
      if (cdr(sc->args) == sc->NIL) 
	{
	  x = sc->args;
	  v = num_one;
	} 
      else 
	{
	  x = cdr(sc->args);
	  v = nvalue(car(sc->args));
	}
      for (; x != sc->NIL; x = cdr(x)) 
	v = num_div(sc, v, nvalue(car(x)));
      s_return(sc, make_number(sc, v));
      
    case OP_QUOTIENT:        /* quotient */
      if (cdr(sc->args) == sc->NIL) 
	{
	  x = sc->args;
	  v = num_one;
	} 
      else 
	{
	  x = cdr(sc->args);
	  v = nvalue(car(sc->args));
	}
      for (; x != sc->NIL; x = cdr(x)) 
	{
	  if (s7_integer(car(x)) != 0)
	    v = num_quotient(v, nvalue(car(x)));
	  else {
	    Error_0(sc, "quotient: division by zero");
	  }
	}
      s_return(sc,make_number(sc, v));
      
    case OP_REM:        /* remainder */
      v = nvalue(car(sc->args));
      if (s7_integer(cadr(sc->args)) != 0)
	v = num_rem(v, nvalue(cadr(sc->args)));
      else Error_0(sc, "remainder: division by zero");

      s_return(sc,make_number(sc, v));
      
    case OP_MOD:        /* modulo */
      v = nvalue(car(sc->args));
      if (s7_integer(cadr(sc->args)) != 0)
	v = num_mod(v, nvalue(cadr(sc->args)));
      else {
	Error_0(sc, "modulo: division by zero");
      }
      s_return(sc, make_number(sc, v));
      
    case OP_CAR:        /* car */
      s_return(sc, caar(sc->args));
      
    case OP_CDR:        /* cdr */
      s_return(sc, cdar(sc->args));
      
    case OP_CONS:       /* cons */
      cdr(sc->args) = cadr(sc->args);
      s_return(sc, sc->args);
      
    case OP_SETCAR:     /* set-car! */
      if (!s7_is_immutable(car(sc->args))) 
	{
	  caar(sc->args) = cadr(sc->args);
	  s_return(sc, car(sc->args));
	} 
      else Error_0(sc, "set-car!: unable to alter immutable pair");
      
    case OP_SETCDR:     /* set-cdr! */
      if (!s7_is_immutable(car(sc->args))) 
	{
	  cdar(sc->args) = cadr(sc->args);
	  s_return(sc, car(sc->args));
	} 
      else Error_0(sc, "set-cdr!: unable to alter immutable pair");
      
    case OP_CHAR2INT:  /* char->integer */
      { 
	char c;
	c = (char)character(car(sc->args));
	s_return(sc, s7_make_integer(sc, (unsigned char)c));
      }
      
    case OP_INT2CHAR: /* integer->char */
      { 
	unsigned char c;
	c = (unsigned char)s7_integer(car(sc->args)); /* int here -> char */
	s_return(sc, s7_make_character(sc, (char)c));
      }
      
    case OP_CHARUPCASE: 
      {
	unsigned char c;
	c = (unsigned char)character(car(sc->args));
	c = toupper(c);
	s_return(sc, s7_make_character(sc, (char)c));
      }
      
    case OP_CHARDNCASE: 
      {
	unsigned char c;
	c = (unsigned char)character(car(sc->args));
	c = tolower(c);
	s_return(sc, s7_make_character(sc, (char)c));
      }
      
    case OP_STR2SYM:  /* string->symbol */
      s_return(sc, s7_make_symbol(sc, string_value(car(sc->args))));
      
    case OP_STR2ATOM: /* string->atom */ 
      {
	char *s = string_value(car(sc->args));
	if (*s == '#') 
	  s_return(sc, make_sharp_const(sc, s + 1));
	else s_return(sc, make_atom(sc, s));
      }
      
    case OP_SYM2STR: /* symbol->string */
      x = s7_make_string(sc, s7_symbol_name(car(sc->args)));
      s7_setimmutable(x);
      s_return(sc,x);

    case OP_ATOM2STR: /* atom->string */
      x = car(sc->args);
      if (s7_is_number(x) || s7_is_character(x) || s7_is_string(x) || s7_is_symbol(x)) 
	{
	  char *p;
	  int len;
	  s7_atom2str(sc, x, 0, &p, &len);
	  s_return(sc, s7_make_counted_string(sc, p, len));
	} 
      else Error_1(sc, "atom->string: not an atom:", x);
      
    case OP_MKSTRING: /* make-string */
      { 
	char fill = ' ';
	int len;

	len = s7_integer(car(sc->args));
	if (cdr(sc->args) != sc->NIL) 
	  fill = s7_character(cadr(sc->args));

	s_return(sc, mk_empty_string(sc, len, (char)fill));
      }
      
    case OP_STRLEN:  /* string-length */
      s_return(sc, s7_make_integer(sc, string_length(car(sc->args))));
      
    case OP_STRREF:  /* string-ref */
      {
	char *str;
	int index;
      
	str = string_value(car(sc->args));
	index = s7_integer(cadr(sc->args));
	if (index >= string_length(car(sc->args))) 
	  Error_1(sc, "string-ref: out of bounds:", cadr(sc->args));
      
	s_return(sc, s7_make_character(sc, ((unsigned char*)str)[index]));
      }
      
    case OP_STRSET: /* string-set! */
      { 
	char *str;
	int index;
	char c;
      
	if (s7_is_immutable(car(sc->args))) 
	  {
	    Error_1(sc, "string-set!: unable to alter immutable string:", car(sc->args));
	  }

	str = string_value(car(sc->args));
	index = s7_integer(cadr(sc->args));
	if (index >= string_length(car(sc->args))) 
	  {
	    Error_1(sc, "string-set!: out of bounds:",cadr(sc->args));
	  }
      
	c = s7_character(caddr(sc->args));
	str[index] = (char)c;
	s_return(sc, car(sc->args));
      }
      
    case OP_STRAPPEND: /* string-append */
      { 
	/* in 1.29 string-append was in Scheme in init.scm but was too slow */
	int len = 0;
	pointer newstr;
	char *pos;
      
	/* compute needed length for new string */
	for (x = sc->args; x != sc->NIL; x = cdr(x)) 
	  len += string_length(car(x));

	newstr = mk_empty_string(sc, len, ' ');
	/* store the contents of the argument strings into the new string */
	for (pos = string_value(newstr), x = sc->args; x != sc->NIL;
	     pos += string_length(car(x)), x = cdr(x)) 
	  memcpy(pos, string_value(car(x)), string_length(car(x)));

	s_return(sc, newstr);
      }
      
    case OP_SUBSTR: { /* substring */
      char *str;
      int index0;
      int index1;
      int len;
      
      str = string_value(car(sc->args));
      
      index0 = s7_integer(cadr(sc->args));
      
      if (index0>string_length(car(sc->args))) 
	{
	  Error_1(sc, "substring: start out of bounds:",cadr(sc->args));
	}
      
      if (cddr(sc->args)!= sc->NIL) 
	{
	  index1 = s7_integer(caddr(sc->args));
	  if (index1>string_length(car(sc->args)) || index1<index0) 
	    {
	      Error_1(sc, "substring: end out of bounds:",caddr(sc->args));
	    }
	} 
      else 
	{
	  index1= string_length(car(sc->args));
	}
      
      len=index1 - index0;
      x = mk_empty_string(sc,len,' ');
      memcpy(string_value(x),str+index0,len);
      string_value(x)[len]= 0;
      
      s_return(sc,x);
    }
      
    case OP_VECTOR: /* vector */
      {   
	int i;
	pointer vec;
	int len = s7_list_length(sc, sc->args);
	if (len < 0) 
	  Error_1(sc, "vector: not a proper list:", sc->args);

	vec = s7_make_vector(sc, len);
	for (x = sc->args, i = 0; s7_is_pair(x); x = cdr(x), i++) 
	  vector_element(vec, i) =  car(x);

	s_return(sc,vec);
      }
      
    case OP_MKVECTOR: /* make-vector */
      { 
	pointer fill= sc->NIL;
	int len;
	pointer vec;
      
	len = s7_integer(car(sc->args));
      
	if (cdr(sc->args) != sc->NIL) 
	  fill = cadr(sc->args);

	vec = s7_make_vector(sc, len);
	if (fill != sc->NIL)
	  s7_fill_vector(vec, fill);

	s_return(sc,vec);
      }
      
    case OP_VECLEN:  /* vector-length */
      s_return(sc, s7_make_integer(sc, vector_length(car(sc->args))));
      
    case OP_VECREF: /* vector-ref */
      { 
	int index;
	index = s7_integer(cadr(sc->args));
      
	if (index >= vector_length(car(sc->args))) 
	  Error_1(sc, "vector-ref: out of bounds:", cadr(sc->args));
      
	s_return(sc, vector_element(car(sc->args), index));
      }
      
    case OP_VECSET:/* vector-set! */
      {   
	int index;
      
	if (s7_is_immutable(car(sc->args))) 
	  Error_1(sc, "vector-set!: unable to alter immutable vector:", car(sc->args));
      
	index = s7_integer(cadr(sc->args));
	if (index >= vector_length(car(sc->args))) 
	  Error_1(sc, "vector-set!: out of bounds:",cadr(sc->args));
      
	vector_element(car(sc->args), index) = caddr(sc->args);
	s_return(sc, car(sc->args));
      }
      
    default:
      sprintf(sc->strbuff, "opexe_2 %d: illegal operator", sc->op);
      Error_0(sc, sc->strbuff);
    }
  return sc->T;
}

int s7_list_length(scheme *sc, pointer a) 
{
  int v = 0;
  pointer x;
  for (x = a, v = 0; s7_is_pair(x); x = cdr(x)) 
    ++v;
  if (x == sc->NIL) 
    return(v);
  return(-1);
}

static pointer opexe_3(scheme *sc, enum scheme_opcodes op) 
{
  pointer x;
  num v;
  
  switch (op) 
    {
    case OP_NOT:        /* not */
      s_retbool(is_false(car(sc->args)));

    case OP_BOOLP:       /* boolean? */
      s_retbool(car(sc->args) == sc->F || car(sc->args) == sc->T);

    case OP_EOFOBJP:       /* boolean? */
      s_retbool(car(sc->args) == sc->EOF_OBJ);

    case OP_NULLP:       /* null? */
      s_retbool(car(sc->args) == sc->NIL);

    case OP_NUMEQ:      /* = */
    case OP_LESS:       /* < */
    case OP_GRE:        /* > */
    case OP_LEQ:        /* <= */
    case OP_GEQ:        /* >= */
      {
	bool (*comp_func)(num, num);
	switch(op) 
	  {
	  case OP_NUMEQ: comp_func = num_eq; break;
	  case OP_LESS:  comp_func = num_lt; break;
	  case OP_GRE:   comp_func = num_gt; break;
	  case OP_LEQ:   comp_func = num_le; break;
	  case OP_GEQ:   comp_func = num_ge; break;
	  }
	x = sc->args;
	v = nvalue(car(x));
	x = cdr(x);
      
	for (; x != sc->NIL; x = cdr(x)) 
	  {
	    if (!comp_func(v, nvalue(car(x)))) 
	      s_retbool(false);
	    v = nvalue(car(x));
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

    case OP_PORTP:     /* port? */
      s_retbool(s7_is_port(car(sc->args)));

    case OP_INPORTP:     /* input-port? */
      s_retbool(is_inport(car(sc->args)));

    case OP_OUTPORTP:     /* output-port? */
      s_retbool(is_outport(car(sc->args)));

    case OP_PROCP:       /* procedure? */
      /*--
       * continuation should be procedure by the example
       * (call-with-current-continuation procedure?) ==> #t
       * in R^3 report sec. 6.9
       */
      s_retbool(s7_is_proc(car(sc->args)) || s7_is_closure(car(sc->args))
		|| s7_is_continuation(car(sc->args)) || s7_is_foreign_function(car(sc->args)));

    case OP_PAIRP:       /* pair? */
      s_retbool(s7_is_pair(car(sc->args)));

    case OP_LISTP:  /* list? */
      {    
      pointer slow, fast;
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
	      /* the fast pointer has looped back around and caught up
		 with the slow pointer, hence the structure is circular,
		 not of finite length, and therefore not a list */
	      s_retbool(false);
	    }
	}
    }
    case OP_ENVP:        /* environment? */
      s_retbool(s7_is_environment(car(sc->args)));

    case OP_VECTORP:     /* vector? */
      s_retbool(s7_is_vector(car(sc->args)));

    case OP_EQ:         /* eq? */
      s_retbool(car(sc->args) == cadr(sc->args));

    case OP_EQV:        /* eqv? */
      s_retbool(s7_eqv_p(car(sc->args), cadr(sc->args)));

    case OP_EQUAL:        /* equal? */
      s_retbool(s7_is_equal(sc, car(sc->args), cadr(sc->args)));

    default:
      sprintf(sc->strbuff, "opexe_3 %d: illegal operator", sc->op);
      Error_0(sc, sc->strbuff);
    }
  return sc->T;
}

static pointer opexe_4(scheme *sc, enum scheme_opcodes op) 
{
  pointer x, y;
  
  switch (op) 
    {
    case OP_FORCE:      /* force */
      set_sc_code(sc, car(sc->args));
      if (is_promise(sc->code)) 
	{
	  /* Should change type to closure here */
	  s_save(sc, OP_SAVE_FORCED, sc->NIL, sc->code);
	  sc->args = sc->NIL;
	  s_goto(sc, OP_APPLY);
	} 
      else 
	{
	  s_return(sc, sc->code);
	}
      
    case OP_SAVE_FORCED:     /* Save forced value replacing promise */
      memcpy(sc->code, sc->value,sizeof(struct cell));
      s_return(sc, sc->value);
      
    case OP_WRITE:      /* write */
    case OP_DISPLAY:    /* display */
    case OP_WRITE_CHAR: /* write-char */
      if (s7_is_pair(cdr(sc->args))) 
	{
	  if (cadr(sc->args) != sc->outport) 
	    {
#if 0
	      fprintf(stderr, "2nd arg %p (%s) is not current output %p (%s)\n", 
		      cadr(sc->args), s7_object_to_string(sc, cadr(sc->args)),
		      sc->outport, s7_object_to_string(sc, sc->outport));
#endif
	      x = cons(sc, sc->outport, sc->NIL);
	      s_save(sc, OP_SET_OUTPORT, x, sc->NIL);
	      sc->outport = cadr(sc->args);
	    }
	}
      sc->args = car(sc->args);
      if (op ==OP_WRITE) 
	sc->print_flag = 1;
      else sc->print_flag = 0;
      s_goto(sc, OP_P0LIST);
      
    case OP_NEWLINE:    /* newline */
      if (s7_is_pair(sc->args)) 
	{
	  if (car(sc->args)!= sc->outport) 
	    {
	      x = cons(sc, sc->outport, sc->NIL);
	      s_save(sc, OP_SET_OUTPORT, x, sc->NIL);
	      sc->outport = car(sc->args);
	    }
	}
      s7_putstr(sc, "\n");
      s_return(sc, sc->T);
      
    case OP_ERR0:  /* error */
      sc->retcode =-1;
      if (!s7_is_string(car(sc->args))) 
	{
	  sc->args = cons(sc, s7_make_string(sc, " -- "), sc->args);
	  s7_setimmutable(car(sc->args));
	}
      s7_putstr(sc, "Error: ");
      s7_putstr(sc, string_value(car(sc->args)));
      sc->args = cdr(sc->args);
      s_goto(sc, OP_ERR1);
      
    case OP_ERR1:  /* error */
      s7_putstr(sc, " ");
      if (sc->args != sc->NIL) 
	{
	  s_save(sc, OP_ERR1, cdr(sc->args), sc->NIL);
	  sc->args = car(sc->args);
	  sc->print_flag = 1;
	  s_goto(sc, OP_P0LIST);
	} 
      else 
	{
	  s7_putstr(sc, "\n");
	  if (sc->interactive_repl) 
	    s_goto(sc, OP_T0LVL);
	  else return sc->NIL;
	}
      
    case OP_REVERSE:    /* reverse */
      s_return(sc,s7_reverse(sc, car(sc->args)));
      
    case OP_LIST_STAR: /* list* */
      s_return(sc, list_star(sc, sc->args));
      
    case OP_APPEND:     /* append */
      if (sc->args == sc->NIL) 
	s_return(sc, sc->NIL);
	
      x = car(sc->args);
      if (cdr(sc->args) == sc->NIL) 
	s_return(sc, sc->args);

      for (y = cdr(sc->args); y != sc->NIL; y = cdr(y)) 
	x = s7_append(sc,x, car(y));
      s_return(sc,x);
      
    case OP_QUIT:       /* quit */
      if (s7_is_pair(sc->args)) 
	sc->retcode = s7_integer(car(sc->args));
      return (sc->NIL);
      
    case OP_GC:         /* gc */
      gc(sc, sc->NIL, sc->NIL);
      s_return(sc, sc->T);
      
    case OP_GCVERB:          /* gc-verbose */
      {   
	int  was = sc->gc_verbose;
	
	sc->gc_verbose = (car(sc->args) != sc->F);
	s_retbool(was);
      }
      
    case OP_OBLIST: /* oblist */
      s_return(sc, oblist_all_symbols(sc)); 
      
    case OP_CURR_INPORT: /* current-input-port */
      s_return(sc, sc->inport);
      
    case OP_CURR_OUTPORT: /* current-output-port */
      s_return(sc, sc->outport);
      
    case OP_CURR_ERRPORT: /* current-error-port */
      s_return(sc, sc->errport);
      
    case OP_OPEN_INFILE: /* open-input-file */
    case OP_OPEN_OUTFILE: /* open-output-file */
    case OP_OPEN_INOUTFILE: /* open-input-output-file */ {
      int prop = 0;
      pointer p;
      switch(op) 
	{
	case OP_OPEN_INFILE:     prop = port_input; break;
	case OP_OPEN_OUTFILE:    prop = port_output; break;
	case OP_OPEN_INOUTFILE: prop = port_input|port_output; break;
	}
      p = port_from_filename(sc,string_value(car(sc->args)), prop);
      if (p == sc->NIL) 
	{
	  s_return(sc, sc->F);
	}
      s_return(sc, p);
    }
      
    case OP_GET_OUTSTRING:
      {
	const char *tmp;
	tmp = s7_get_output_string(sc, car(sc->args));
	s_return(sc, s7_make_string(sc, tmp));
      }
      
    case OP_OPEN_OUTPUT_STRING: 
      s_return(sc, s7_open_output_string(sc));
      
    case OP_OPEN_INSTRING: /* open-input-string */
    case OP_OPEN_INOUTSTRING: /* open-input-output-string */ 
      {
      int prop = 0;
      pointer p;
      switch(op) 
	{
	case OP_OPEN_INSTRING:     prop = port_input; break;
	case OP_OPEN_INOUTSTRING:  prop = port_input|port_output; break;
	}
      p = port_from_string(sc, string_value(car(sc->args)),
			  string_value(car(sc->args))+string_length(car(sc->args)), prop);
      if (p == sc->NIL) 
	{
	  s_return(sc, sc->F);
	}
      s_return(sc, p);
      }
      
    case OP_CLOSE_INPORT: /* close-input-port */
      port_close(sc, car(sc->args), port_input);
      s_return(sc, sc->T);
      
    case OP_CLOSE_OUTPORT: /* close-output-port */
      port_close(sc, car(sc->args), port_output);
      s_return(sc, sc->T);
      
    case OP_INT_ENV: /* interaction-environment */
      s_return(sc, sc->global_env);
      
    case OP_CURR_ENV: /* current-environment */
      s_return(sc, sc->envir);
      
    default:
      sprintf(sc->strbuff, "opexe_4 %d: illegal operator", sc->op);
      Error_0(sc, sc->strbuff);

    }
  return sc->T;
}

static pointer opexe_5(scheme *sc, enum scheme_opcodes op) 
{
  pointer x;
  
  if (sc->nesting!= 0) 
    {
      int n= sc->nesting;
      sc->nesting = 0;
      sc->retcode =-1;
      Error_1(sc, "unmatched parentheses:", s7_make_integer(sc, n));
    }
  
  switch (op) 
    {
      /* ========== reading part ========== */
    case OP_READ:
      if (!s7_is_pair(sc->args)) 
	{
	  s_goto(sc, OP_READ_INTERNAL);
	}
      if (!is_inport(car(sc->args))) 
	{
	  Error_1(sc, "read: not an input port:", car(sc->args));
	}
      if (car(sc->args) == sc->inport) 
	{
	  s_goto(sc, OP_READ_INTERNAL);
	}
      x = sc->inport;
      sc->inport = car(sc->args);
      x = cons(sc,x, sc->NIL);
      s_save(sc, OP_SET_INPORT, x, sc->NIL);
      s_goto(sc, OP_READ_INTERNAL);
      
    case OP_READ_CHAR: /* read-char */
    case OP_PEEK_CHAR: /* peek-char */ {
      int c;
      if (s7_is_pair(sc->args)) 
	{
	  if (car(sc->args)!= sc->inport) 
	    {
	      x = sc->inport;
	      x = cons(sc,x, sc->NIL);
	      s_save(sc, OP_SET_INPORT, x, sc->NIL);
	      sc->inport = car(sc->args);
	    }
	}
      c =inchar(sc);
      if (c == EOF) 
	{
	  s_return(sc, sc->EOF_OBJ);
	}
      if (sc->op == OP_PEEK_CHAR) 
	{
	  backchar(sc, c);
	}
      s_return(sc, s7_make_character(sc, c));
    }
      
    case OP_CHAR_READY: /* char-ready? */ {
      pointer p = sc->inport;
      int res;
      if (s7_is_pair(sc->args)) 
	p = car(sc->args);
      res = p->object.port->kind & port_string;
      s_retbool(res);
    }
      
    case OP_SET_INPORT: /* set-input-port */
      sc->inport = car(sc->args);
      s_return(sc, sc->value);
      
    case OP_SET_OUTPORT: /* set-output-port */
      sc->outport = car(sc->args);
      s_return(sc, sc->value);
      
    case OP_RDSEXPR:
      switch (sc->tok) 
	{
	case TOK_EOF:
	  if (sc->inport == sc->loadport) 
	    {
	      sc->args = sc->NIL;
	      s_goto(sc, OP_QUIT);
	    } 
	  else s_return(sc, sc->EOF_OBJ);

	case TOK_VEC:
	  s_save(sc, OP_RDVEC, sc->NIL, sc->NIL);
	  /* fall through */

	case TOK_LPAREN:
	  sc->tok = token(sc);
	  if (sc->tok == TOK_RPAREN) 
	    s_return(sc, sc->NIL);
	  else 
	    {
	      if (sc->tok == TOK_DOT) 
		Error_0(sc, "syntax error: illegal dot expression");
	      else 
		{
		  sc->nesting_stack[sc->file_i]++;
		  s_save(sc, OP_RDLIST, sc->NIL, sc->NIL);
		  s_goto(sc, OP_RDSEXPR);
		}
	    }

	case TOK_QUOTE:
	  s_save(sc, OP_RDQUOTE, sc->NIL, sc->NIL);
	  sc->tok = token(sc);
	  s_goto(sc, OP_RDSEXPR);

	case TOK_BQUOTE:
	  sc->tok = token(sc);
	  if (sc->tok== TOK_VEC) 
	    {
	      s_save(sc, OP_RDQQUOTEVEC, sc->NIL, sc->NIL);
	      sc->tok= TOK_LPAREN;
	      s_goto(sc, OP_RDSEXPR);
	    } 
	  else s_save(sc, OP_RDQQUOTE, sc->NIL, sc->NIL);
	  s_goto(sc, OP_RDSEXPR);

	case TOK_COMMA:
	  s_save(sc, OP_RDUNQUOTE, sc->NIL, sc->NIL);
	  sc->tok = token(sc);
	  s_goto(sc, OP_RDSEXPR);

	case TOK_ATMARK:
	  s_save(sc, OP_RDUQTSP, sc->NIL, sc->NIL);
	  sc->tok = token(sc);
	  s_goto(sc, OP_RDSEXPR);

	case TOK_ATOM:
	  s_return(sc, make_atom(sc, readstr_upto(sc, "();\t\n\r ")));

	case TOK_DQUOTE:
	  x =readstrexp(sc);
	  if (x == sc->F) 
	    {
	      Error_0(sc, "Error reading string");
	    }
	  s7_setimmutable(x);
	  s_return(sc, x);

	case TOK_SHARP: 
	  {
	  pointer f= s7_find_slot_in_env(sc, sc->envir, sc->SHARP_HOOK, 1);
	  if (f == sc->NIL) 
	    Error_0(sc, "undefined sharp expression");
	  else 
	    {
	      set_sc_code(sc, cons(sc, s7_slot_value_in_env(f), sc->NIL)); 
	      s_goto(sc, OP_EVAL);
	    }
	  }

	case TOK_SHARP_CONST:
	  {
	    char *expr;
	    expr = readstr_upto(sc, "();\t\n\r ");
	    if ((x = make_sharp_const(sc, expr)) == sc->NIL)
	      Error_1(sc, "undefined sharp expression", s7_make_string(sc, expr));
	    else s_return(sc, x);
	  }
	default:
	  Error_0(sc, "syntax error: illegal token");
	}
      break;
      
    case OP_RDLIST: 
      {
      sc->args = cons(sc, sc->value, sc->args);
      sc->tok = token(sc);
      /* We now skip comments in the scanner
	 
	 while (sc->tok == TOK_COMMENT) 
	 {
	 int c;
	 while ((c =inchar(sc)) != '\n' && c!=EOF)
	 ;
	 sc->tok = token(sc);
	 }
      */
      if (sc->tok == TOK_RPAREN) 
	{
	  int c = inchar(sc);
	  if (c != '\n') backchar(sc,c);
	  sc->nesting_stack[sc->file_i]--;
	  s_return(sc, s7_reverse_in_place(sc, sc->NIL, sc->args));
	} 
      else if (sc->tok == TOK_DOT) 
	{
	  s_save(sc, OP_RDDOT, sc->args, sc->NIL);
	  sc->tok = token(sc);
	  s_goto(sc, OP_RDSEXPR);
	} 
      else 
	{
	  s_save(sc, OP_RDLIST, sc->args, sc->NIL);;
	  s_goto(sc, OP_RDSEXPR);
	}
    }
      
    case OP_RDDOT:
      if (token(sc) != TOK_RPAREN) 
	Error_0(sc, "syntax error: illegal dot expression");
      else 
	{
	  sc->nesting_stack[sc->file_i]--;
	  s_return(sc, s7_reverse_in_place(sc, sc->value, sc->args));
	}
      
    case OP_RDQUOTE:
      s_return(sc, cons(sc, sc->QUOTE, cons(sc, sc->value, sc->NIL)));
      
    case OP_RDQQUOTE:
      s_return(sc, cons(sc, sc->QQUOTE, cons(sc, sc->value, sc->NIL)));
      
    case OP_RDQQUOTEVEC:
      s_return(sc, cons(sc, s7_make_symbol(sc, "apply"),
			cons(sc, s7_make_symbol(sc, "vector"), 
			     cons(sc,cons(sc, sc->QQUOTE, 
					  cons(sc, sc->value, sc->NIL)),
				  sc->NIL))));
      
    case OP_RDUNQUOTE:
      s_return(sc, cons(sc, sc->UNQUOTE, cons(sc, sc->value, sc->NIL)));
      
    case OP_RDUQTSP:
      s_return(sc, cons(sc, sc->UNQUOTESP, cons(sc, sc->value, sc->NIL)));
      
    case OP_RDVEC:
      sc->args = sc->value;
      s_goto(sc, OP_VECTOR);
      
      /* ========== printing part ========== */
    case OP_P0LIST:
      if (s7_is_vector(sc->args)) 
	{
	  s7_putstr(sc, "#(");
	  sc->args = cons(sc, sc->args, s7_make_integer(sc, 0)); /* loop_counter? */
	  s_goto(sc, OP_PVECFROM);
	} 
      else if (s7_is_environment(sc->args)) 
	{
	  s7_putstr(sc, "#<ENVIRONMENT>");
	  s_return(sc, sc->T);
	} 
      else if (!s7_is_pair(sc->args)) 
	{
	  printatom(sc, sc->args, sc->print_flag);
	  s_return(sc, sc->T);
	} 
      else if (car(sc->args) == sc->QUOTE && ok_abbrev(cdr(sc->args))) 
	{
	  s7_putstr(sc, "'");
	  sc->args = cadr(sc->args);
	  s_goto(sc, OP_P0LIST);
	} 
      else if (car(sc->args) == sc->QQUOTE && ok_abbrev(cdr(sc->args))) 
	{
	  s7_putstr(sc, "`");
	  sc->args = cadr(sc->args);
	  s_goto(sc, OP_P0LIST);
	} 
      else if (car(sc->args) == sc->UNQUOTE && ok_abbrev(cdr(sc->args))) 
	{
	  s7_putstr(sc, ", ");
	  sc->args = cadr(sc->args);
	  s_goto(sc, OP_P0LIST);
	} 
      else if (car(sc->args) == sc->UNQUOTESP && ok_abbrev(cdr(sc->args))) 
	{
	  s7_putstr(sc, ",@");
	  sc->args = cadr(sc->args);
	  s_goto(sc, OP_P0LIST);
	} 
      else 
	{
	  s7_putstr(sc, "(");
	  s_save(sc, OP_P1LIST, cdr(sc->args), sc->NIL);
	  sc->args = car(sc->args);
	  s_goto(sc, OP_P0LIST);
	}
      
    case OP_P1LIST:
      if (s7_is_pair(sc->args)) 
	{
	  s_save(sc, OP_P1LIST, cdr(sc->args), sc->NIL);
	  s7_putstr(sc, " ");
	  sc->args = car(sc->args);
	  s_goto(sc, OP_P0LIST);
	} 
      else if (s7_is_vector(sc->args)) 
	{
	  s_save(sc, OP_P1LIST, sc->NIL, sc->NIL);
	  s7_putstr(sc, " . ");
	  s_goto(sc, OP_P0LIST);
	} 
      else 
	{
	  if (sc->args != sc->NIL) 
	    {
	      s7_putstr(sc, " . ");
	      printatom(sc, sc->args, sc->print_flag);
	    }
	  s7_putstr(sc, ")");
	  s_return(sc, sc->T);
	}
    case OP_PVECFROM: 
      {
      int i = loop_counter(cdr(sc->args));
      pointer vec = car(sc->args);
      int len = vector_length(vec);
      if (i == len) 
	{
	  s7_putstr(sc, ")");
	  s_return(sc, sc->T);
	} 
      else 
	{
	  pointer elem = vector_element(vec, i);
	  loop_counter(cdr(sc->args)) = i + 1;
	  s_save(sc, OP_PVECFROM, sc->args, sc->NIL);
	  sc->args = elem;
	  s7_putstr(sc, " ");
	  s_goto(sc, OP_P0LIST);
	}
      }
      
    default:
      sprintf(sc->strbuff, "opexe_5 %d: illegal operator", sc->op);
      Error_0(sc, sc->strbuff);
      
    }
  return sc->T;
}

static pointer opexe_6(scheme *sc, enum scheme_opcodes op) 
{
  pointer x, y;
  long v;
  
  switch (op) 
    {
      
    case OP_OBJECT_TO_STRING:
      {
	pointer p;
	x = car(sc->args);
	s_return(sc, s7_object_to_string(sc, x));
      } 
      
    case OP_LIST_LENGTH:     /* length */   /* a.k */
      v = s7_list_length(sc, car(sc->args));
      if (v < 0) 
	Error_1(sc, "length: not a list:", car(sc->args));
      s_return(sc, s7_make_integer(sc, v));
      
    case OP_ASSQ:       /* assq */     /* a.k */
      x = car(sc->args);
      for (y = cadr(sc->args); s7_is_pair(y); y = cdr(y)) 
	{
	  if (!s7_is_pair(car(y))) 
	    Error_0(sc, "unable to handle non pair element");

	  if (x == caar(y))
	    break;
	}
      if (s7_is_pair(y)) 
	s_return(sc, car(y));
      else s_return(sc, sc->F);
      
      
    case OP_GET_CLOSURE:     /* get-closure-code */   /* a.k */
      sc->args = car(sc->args);
      if (sc->args == sc->NIL) 
	s_return(sc, sc->F);
      else 
	{
	  if (s7_is_closure(sc->args)) 
	    s_return(sc,cons(sc, sc->LAMBDA, s7_closure_code(sc->value)));
	  else 
	    {
	      if (is_macro(sc->args)) 
		s_return(sc,cons(sc, sc->LAMBDA, s7_closure_code(sc->value)));
	      else s_return(sc, sc->F);
	    }
	}

    case OP_CLOSUREP:        /* closure? */
      /*
       * Note, macro object is also a closure.
       * Therefore, (closure? <#MACRO>) ==> #t
       */
      s_retbool(s7_is_closure(car(sc->args)));

    case OP_MACROP:          /* macro? */
      s_retbool(is_macro(car(sc->args)));

    default:
      sprintf(sc->strbuff, "opexe_6 %d: illegal operator", sc->op);
      Error_0(sc, sc->strbuff);
    }
  return sc->T; /* NOTREACHED */
}

typedef pointer (*dispatch_func)(scheme *, enum scheme_opcodes);



typedef bool (*test_predicate)(pointer);

static bool is_any(pointer p) 
{ 
  return(true);
}


static bool is_not_negative(pointer p) 
{
  return(s7_is_integer(p) && (s7_integer((p)) >= 0));
}

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
  {0, "input port"},
  {0, "output_port"},
  {s7_is_environment, "environment"},
  {s7_is_pair, "pair"},
  {0, "pair or '()"},
  {s7_is_character, "character"},
  {s7_is_vector, "vector"},
  {s7_is_number, "number"},
  {s7_is_integer, "integer"},
  {is_not_negative, "non-negative integer"},
  {s7_is_real, "non-complex number"},
  {s7_is_rational, "rational number"},
};

#define TST_NONE 0
#define TST_ANY "\001"
#define TST_STRING "\002"
#define TST_SYMBOL "\003"
#define TST_PORT "\004"
#define TST_INPORT "\005"
#define TST_OUTPORT "\006"
#define TST_ENVIRONMENT "\007"
#define TST_PAIR "\010"
#define TST_LIST "\011"
#define TST_CHAR "\012"
#define TST_VECTOR "\013"
#define TST_NUMBER "\014"
#define TST_INTEGER "\015"
#define TST_NATURAL "\016"
#define TST_REAL "\017"
#define TST_RATIONAL "\018"


typedef struct {
  dispatch_func func;
  char *name;
  int min_arity;
  int max_arity;
  char *arg_tests_encoding;
} op_code_info;

#define INF_ARG 0xffff

static op_code_info dispatch_table[]= { 
#define _OP_DEF(A, B, C, D, E, OP) {A, B, C, D, E}, 
#include "s7-ops.h" 
  { 0, 0, 0, 0, 0 } 
}; 


static const char *procname(pointer x) 
{
  int n = procedure_index(x);
  const char *name = dispatch_table[n].name;
  if (name == 0) 
    name ="ILLEGAL!";
  return name;
}

/* kernel of this interpreter */
static void Eval_Cycle(scheme *sc, enum scheme_opcodes op) 
{
  int count = 0;
  enum scheme_opcodes old_op;
  
  sc->op = op;

  for (;;) 
    {
      op_code_info *pcd = dispatch_table + sc->op;

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
	      fprintf(stderr, msg);
	    }
	  if (ok && n > pcd->max_arity) 
	    {
	      ok = false;
	      sprintf(msg, "%s: needs%s %d argument%s",
		      pcd->name,
		      pcd->min_arity== pcd->max_arity?"":" at most",
		      pcd->max_arity,
		      (pcd->max_arity != 1) ? "s" : "");
	      fprintf(stderr, msg);
	    }
	  if (ok) 
	    {
	      if (pcd->arg_tests_encoding != 0) 
		{
		  int i = 0;
		  int j;
		  const char *t = pcd->arg_tests_encoding;
		  pointer arglist = sc->args;
		  do {
		    pointer arg = car(arglist);
		    j = (int)t[0];
		    if (j == TST_INPORT[0]) 
		      {
			if (!is_inport(arg)) break;
		      } 
		    else 
		      if (j == TST_OUTPORT[0]) 
			{
			  if (!is_outport(arg)) break;
			} 
		      else 
			if (j == TST_LIST[0]) 
			  {
			    if (arg!= sc->NIL && !s7_is_pair(arg)) break; 	      
			  } 
			else 
			  {
			    if (!tests[j].fct(arg)) break;
			  }
		    
		    if (t[1] != 0) 
		      {/* last test is replicated as necessary */
			t++;
		      }
		    arglist = cdr(arglist);
		    i++;
		  } while(i < n);
		  if (i < n) 
		    {
		      ok = false;
		      sprintf(msg, "%s: argument %d must be: %s",
			      pcd->name,
			      i + 1,
			      tests[j].kind);
		    }
		}
	    }
	  if (!ok) 
	    {
	      if (Error_1(sc, msg, 0) == sc->NIL) 
		return;

	      pcd = dispatch_table + sc->op;
	    }
	}
      old_op = sc->op;
      if (pcd->func(sc, sc->op) == sc->NIL) /* here we call the op -- func is one of the opexe_n's */
	return;

      count++;
    }
}

/* ========== Initialization of internal keywords ========== */

static void assign_syntax(scheme *sc, char *name) 
{
  pointer x;
  x = oblist_add_by_name(sc, name); 
  typeflag(x) |= T_SYNTAX; 
}

static pointer make_proc(scheme *sc, enum scheme_opcodes op) 
{
  pointer y;
  y = new_cell(sc);
  typeflag(y) = (T_PROC | T_ATOM);
  procedure_index(y) = op;
  return(y);
}

static void assign_proc(scheme *sc, enum scheme_opcodes op, char *name) 
{
  pointer x, y;
  
  x = s7_make_symbol(sc, name);
  y = make_proc(sc, op);
  new_slot_in_env(sc, x, y); 
}

/* Hard-coded for the given keywords. Remember to rewrite if more are added! */
static int syntaxnum(pointer p) 
{
  const char *s = string_value(car(p));
  switch(string_length(car(p))) 
    {
    case 2:
      if (s[0]=='i') return OP_IF0;        /* if */
      else return OP_OR0;                 /* or */ 
    case 3:
      if (s[0]=='a') return OP_AND0;      /* and */
      else return OP_LET0;               /* let */
    case 4:
      switch(s[3]) 
	{
	case 'e': return OP_CASE0;         /* case */
	case 'd': return OP_COND0;         /* cond */
	case '*': return OP_LET0AST;       /* let* */
	default: return OP_SET0;           /* set! */          
	}
    case 5:
      switch(s[2]) 
	{
	case 'g': return OP_BEGIN;         /* begin */
	case 'l': return OP_DELAY;         /* delay */
	case 'c': return OP_MACRO0;        /* macro */
	default: return OP_QUOTE;          /* quote */
	}
    case 6:
      switch(s[2]) 
	{
	case 'm': return OP_LAMBDA;        /* lambda */
	case 'f': return OP_DEF0;          /* define */
	default: return OP_LET0REC;        /* letrec */
	}
    default:
      return OP_C0STREAM;                /* cons-stream */
    }
}


scheme *s7_init_new(void) 
{
  scheme *sc = (scheme*)MALLOC(sizeof(scheme));
  if (!s7_init(sc)) 
    {
      FREE(sc);
      return(NULL);
    } 
  else 
    {
      return(sc);
    }
}

scheme *s7_init_new_custom_alloc(void)
{
  scheme *sc;
  sc = (scheme *)MALLOC(sizeof(scheme));
  if (!s7_init_custom_alloc(sc))
    {
      FREE(sc);
      return(NULL);
    } 
  return(sc);
}


int s7_init(scheme *sc) 
{
  return(s7_init_custom_alloc(sc));
}

int s7_init_custom_alloc(scheme *sc)
{
  int i, n = sizeof(dispatch_table) / sizeof(dispatch_table[0]);
  pointer x;
  
  num_zero.type = NUM_INT;
  integer(num_zero) = 0;
  num_one.type = NUM_INT;
  integer(num_one) = 1;
  
  sc->gensym_cnt = 0;

  sc->NIL = &sc->_NIL;
  sc->T = &sc->_HASHT;
  sc->F = &sc->_HASHF;
  sc->EOF_OBJ = &sc->_EOF_OBJ;
  sc->UNDEFINED = &sc->_UNDEFINED;

  sc->inport = sc->NIL;
  sc->outport = sc->NIL;
  sc->errport = sc->NIL;
  sc->save_inport = sc->NIL;
  sc->loadport = sc->NIL;
  sc->nesting = 0;
  sc->interactive_repl= false;

  sc->cells_size = 500000;
  sc->cells = (pointer *)CALLOC(sc->cells_size, sizeof(pointer));
  sc->cells_top = 0;

  sc->gc_verbose = true;
  dump_stack_initialize(sc); 
  set_sc_code(sc, sc->NIL);
  sc->tracing = false;
  
  /* init sc->NIL */
  typeflag(sc->NIL) = (T_ATOM | MARK);
  car(sc->NIL) = cdr(sc->NIL) = sc->NIL;
  /* init T */
  typeflag(sc->T) = (T_ATOM | MARK);
  car(sc->T) = cdr(sc->T) = sc->T;
  /* init F */
  typeflag(sc->F) = (T_ATOM | MARK);
  car(sc->F) = cdr(sc->F) = sc->F;

  sc->oblist = oblist_initial_value(sc); 
  /* init global_env */
  new_frame_in_env(sc, sc->NIL); 
  sc->global_env = sc->envir; 
  /* init else */
  x = s7_make_symbol(sc, "else");
  new_slot_in_env(sc, x, sc->T); 
  
  assign_syntax(sc, "lambda");
  assign_syntax(sc, "quote");
  assign_syntax(sc, "define");
  assign_syntax(sc, "if");
  assign_syntax(sc, "begin");
  assign_syntax(sc, "set!");
  assign_syntax(sc, "let");
  assign_syntax(sc, "let*");
  assign_syntax(sc, "letrec");
  assign_syntax(sc, "cond");
  assign_syntax(sc, "delay");
  assign_syntax(sc, "and");
  assign_syntax(sc, "or");
  assign_syntax(sc, "cons-stream");
  assign_syntax(sc, "macro");
  assign_syntax(sc, "case");
  
  for(i = 0; i < n; i++) 
    if (dispatch_table[i].name != 0) 
      assign_proc(sc, (enum scheme_opcodes)i, dispatch_table[i].name);
  
  /* initialization of global pointers to special symbols */
  sc->LAMBDA = s7_make_symbol(sc, "lambda");
  sc->QUOTE = s7_make_symbol(sc, "quote");
  sc->QQUOTE = s7_make_symbol(sc, "quasiquote");
  sc->UNQUOTE = s7_make_symbol(sc, "unquote");
  sc->UNQUOTESP = s7_make_symbol(sc, "unquote-splicing");
  sc->FEED_TO = s7_make_symbol(sc, "=>");
  sc->COLON_HOOK = s7_make_symbol(sc, "*colon-hook*");
  sc->ERROR_HOOK = s7_make_symbol(sc, "*error-hook*");
  sc->SHARP_HOOK = s7_make_symbol(sc, "*sharp-hook*");

  sc->errport = sc->outport;
  return(1);
}


void s7_set_input_port_file(scheme *sc, FILE *fin) 
{
  sc->inport = port_from_file(sc, fin, port_input);
}


void s7_set_input_port_string(scheme *sc, char *start, char *past_the_end) 
{
  sc->inport = port_from_string(sc,start, past_the_end, port_input);
}


void s7_set_output_port_file(scheme *sc, FILE *fout) 
{
  sc->outport = port_from_file(sc, fout, port_output);
}


void s7_set_output_port_string(scheme *sc, char *start, char *past_the_end) 
{
  sc->outport = port_from_string(sc,start, past_the_end, port_output);
}


void s7_set_external_data(scheme *sc, void *p) 
{
  sc->ext_data = p;
}


void s7_deinit(scheme *sc) 
{
  int i;
  
  sc->oblist = sc->NIL;
  sc->global_env = sc->NIL;
  dump_stack_free(sc); 
  sc->envir= sc->NIL;
  set_sc_code(sc, sc->NIL);
  sc->args = sc->NIL;
  sc->value = sc->NIL;
  if (s7_is_port(sc->inport)) 
    {
      typeflag(sc->inport) = T_ATOM;
    }
  sc->inport = sc->NIL;
  sc->outport = sc->NIL;
  if (s7_is_port(sc->save_inport)) 
    {
      typeflag(sc->save_inport) = T_ATOM;
    }
  sc->save_inport = sc->NIL;
  if (s7_is_port(sc->loadport)) 
    {
      typeflag(sc->loadport) = T_ATOM;
    }
  sc->loadport = sc->NIL;
  sc->gc_verbose = 0;
  gc(sc, sc->NIL, sc->NIL);
}

void s7_load_open_file(scheme *sc, FILE *fin) 
{
  dump_stack_reset(sc); 
  sc->envir = sc->global_env;
  sc->file_i = 0;
  sc->load_stack[0].kind = port_input | port_file;
  sc->load_stack[0].rep.stdio.file = fin;
  sc->loadport = make_port(sc, sc->load_stack);
  sc->retcode = 0;
  if (fin == stdin) 
    sc->interactive_repl = true;
  sc->inport = sc->loadport;
  Eval_Cycle(sc, OP_T0LVL);
  typeflag(sc->loadport) = T_ATOM;
  if (sc->retcode == 0) 
    sc->retcode = (sc->nesting != 0);
}

void s7_load_string(scheme *sc, const char *cmd) 
{
  dump_stack_reset(sc); 
  sc->envir = sc->global_env;
  sc->file_i = 0;
  sc->load_stack[0].kind = port_input | port_string;
  sc->load_stack[0].rep.string.start = (char*)cmd; /* This func respects const */
  sc->load_stack[0].rep.string.past_the_end = (char*)cmd + strlen(cmd);
  sc->load_stack[0].rep.string.curr= (char*)cmd;
  sc->loadport = make_port(sc, sc->load_stack);
  sc->retcode = 0;
  sc->interactive_repl = false;
  sc->inport = sc->loadport;
  Eval_Cycle(sc, OP_T0LVL);
  typeflag(sc->loadport) = T_ATOM;
  if (sc->retcode == 0) 
    sc->retcode = (sc->nesting != 0);
}

void s7_define(scheme *sc, pointer envir, pointer symbol, pointer value) 
{
  pointer x;
  
  x = s7_find_slot_in_env(sc,envir,symbol, 0);
  if (x != sc->NIL) 
    { 
      set_slot_in_env(sc, x, value); 
    } 
  else 
    { 
      s7_new_slot_spec_in_env(sc, envir, symbol, value); 
    } 
}


pointer s7_apply0(scheme *sc, const char *procname) 
{
  pointer carx = s7_make_symbol(sc, procname);
  pointer cdrx = sc->NIL;
  
  dump_stack_reset(sc); 
  sc->envir = sc->global_env;
  set_sc_code(sc, cons(sc, carx, cdrx));
  sc->interactive_repl = false;
  sc->retcode = 0;
  Eval_Cycle(sc, OP_EVAL);
  return(sc->value);
}

pointer s7_call(scheme *sc, pointer func, pointer args) 
{ 
  dump_stack_reset(sc); 
  sc->envir = sc->global_env; 
  sc->args = args; 
  set_sc_code(sc, func); 
  sc->interactive_repl = false; 
  sc->retcode = 0; 
  Eval_Cycle(sc, OP_APPLY);
  return(sc->value);
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

static fobject *foreign_types = NULL;
static int foreign_types_size = 0;
static int num_foreign_types = 0;

int s7_new_foreign_type(const char *name, 
			char *(*print)(void *value), 
			void (*free)(void *value), 
			bool (*equal)(void *val1, void *val2),
			void (*gc_mark)(void *val))
{
  int tag;
  tag = num_foreign_types++;
  if (tag >= foreign_types_size)
    {
      if (foreign_types_size == 0)
	{
	  foreign_types_size = 8;
	  foreign_types = (fobject *)CALLOC(foreign_types_size, sizeof(fobject));
	}
      else
	{
	  foreign_types_size = tag + 8;
	  foreign_types = (fobject *)REALLOC((void *)foreign_types, foreign_types_size * sizeof(fobject));
	}
    }
  foreign_types[tag].type = tag;
  foreign_types[tag].name = copy_string(name);
  foreign_types[tag].free = free;
  foreign_types[tag].print = print;
  foreign_types[tag].equal = equal;
  foreign_types[tag].gc_mark = gc_mark;
  return(tag);
}

char *s7_describe_foreign_object(pointer a)
{
  int tag;
  tag = a->object.fobj.type;
  if (foreign_types[tag].print)
    return((*(foreign_types[tag].print))(a->object.fobj.value));
  return(copy_string(foreign_types[tag].name));
}

void s7_free_foreign_object(pointer a)
{
  int tag;
  tag = a->object.fobj.type;
  if (foreign_types[tag].free)
    (*(foreign_types[tag].free))(a->object.fobj.value);
}

bool s7_equalp_foreign_objects(pointer a, pointer b)
{
  if ((s7_is_foreign_object(a)) &&
      (s7_is_foreign_object(b)) &&
      (a->object.fobj.type == b->object.fobj.type))
    {
      int tag;
      tag = a->object.fobj.type;
      if (foreign_types[tag].equal)
	return((*(foreign_types[tag].equal))(a->object.fobj.value, b->object.fobj.value));
      return(a == b);
    }
  return(false);
}

static void s7_mark_embedded_foreign_objects(pointer a) /* called by gc, calls fobj's mark func */
{
  int tag;
  tag = a->object.fobj.type;
  if (tag < num_foreign_types)
    {
      if (foreign_types[tag].gc_mark)
	(*(foreign_types[tag].gc_mark))(a->object.fobj.value);
    }
  else fprintf(stderr, "%p, found bad type: %x %d %x\n", a, tag, tag, a->flag);
}


void *s7_foreign_object_value(pointer obj)
{
  return(obj->object.fobj.value);
}


int s7_foreign_object_type(pointer obj)
{
  return(obj->object.fobj.type);
}


void s7_provide(scheme *sc, const char *feature)
{
  char *expr;
  int len;
  len = strlen(feature) + 64;
  expr = (char *)CALLOC(len, sizeof(char));
  snprintf(expr, len, "(set! *features* (cons '%s *features*))", feature);
  s7_eval_string(sc, expr);
  FREE(expr);
}


pointer s7_load_file(scheme *sc, const char *file)
{
  FILE *fp;
  fp = fopen(file, "r");
  if (fp)
    {
      s7_load_open_file(sc, fp);
      fclose(fp);
      return(s7_T(sc));
    }
  return(s7_F(sc));
}


pointer s7_load_path(scheme *sc)
{
  return(s7_eval_string(sc, "*load-path*"));
}


pointer s7_add_to_load_path(scheme *sc, const char *file)
{
  return(s7_F(sc));
}

pointer s7_load_file_with_path(scheme *sc, const char *file)
{
  return(s7_load_file(sc, file));
}


bool s7_is_ulong(pointer arg)
{
  return(s7_is_integer(arg));
}

unsigned long s7_ulong(pointer num)
{
  return((unsigned long)s7_integer(num));
}

pointer s7_make_ulong(scheme *sc, unsigned long num)
{
  return(s7_make_integer(sc, num));
}



bool s7_is_keyword(pointer obj)
{
  return((s7_is_symbol(obj)) &&
	 (s7_symbol_name(obj)[0] == ':'));
}

bool s7_keyword_eq_p(pointer k1, pointer k2)
{
  return(k1 == k2);
}

pointer s7_make_keyword(scheme *sc, const char *key)
{
  pointer sym;
  char *name;
  name = (char *)CALLOC(strlen(key) + 2, sizeof(char));
  sprintf(name, ":%s", key);                     /* prepend ":" */
  sym = s7_make_symbol(sc, name);
  FREE(name);
  s7_new_slot_spec_in_env(sc, s7_global_env(sc), sym, sym); /* GC protect (?) */
  return(sym);
}



pointer s7_make_and_fill_vector(scheme *sc, int len, pointer fill)
{
  pointer vect;
  vect = s7_make_vector(sc, len);
  if (fill != sc->NIL)
    s7_fill_vector(vect, fill);
  return(vect);
}


pointer s7_list_ref(scheme *sc, pointer lst, int num)
{
  if (num == 0)
    return(s7_car(lst));
  return(s7_list_ref(sc, s7_cdr(lst), num - 1));
}

pointer s7_list_set(scheme *sc, pointer lst, int num, pointer val)
{
  return(s7_F(sc));
}


pointer s7_assoc(scheme *sc, pointer sym, pointer lst)
{
  pointer x;
  int v;
  for (x = lst, v = 0; s7_is_pair(x); x = s7_cdr(x))
    if ((s7_is_pair(s7_car(x))) &&
	(sym == s7_car(s7_car(x))))
      return(s7_car(x));
  return(s7_F(sc));
}


pointer s7_member(scheme *sc, pointer sym, pointer lst)
{
  pointer x;
  int v;
  for (x = lst, v = 0; s7_is_pair(x); x = s7_cdr(x))
    if (sym == s7_car(x))
      return(s7_car(x));
  return(s7_F(sc));
}

pointer s7_vector_to_list(scheme *sc, pointer vect)
{
  return(s7_NIL(sc));
}


bool s7_eq_p(pointer obj1, pointer obj2)
{
  return(obj1 == obj2);
}


pointer s7_eval_string(scheme *sc, const char *str)
{
  s7_load_string(sc, str);
  return(sc->value);
}


pointer s7_eval_form(scheme *sc, pointer form)
{
  return(s7_F(sc));
}


pointer s7_symbol_value(scheme *sc, pointer sym)
{
  pointer x;
  x = s7_find_slot_in_env(sc, s7_global_env(sc), sym, 0);
  if (x != s7_NIL(sc))
    return(s7_slot_value_in_env(x));
  return(s7_F(sc));
}


pointer s7_symbol_set_value(scheme *sc, pointer sym, pointer val)
{
  pointer x;
  x = s7_find_slot_in_env(sc, s7_global_env(sc), sym, 0);
  if (x != s7_NIL(sc))
    set_slot_in_env(sc, x, val);
  return(val);
}


pointer s7_name_to_value(scheme *sc, const char *name)
{
  return(s7_symbol_value(sc, s7_make_symbol(sc, name)));
}


pointer s7_string_to_form(scheme *sc, const char *str)
{
  return(s7_F(sc));
}


pointer s7_object_to_string(scheme *sc, pointer obj)
{
  char *p;
  int len;
  if (s7_is_vector(obj))
    return(vector_to_string(sc, obj));
  if (s7_is_pair(obj))
    return(list_to_string(sc, obj));
  s7_atom2str(sc, obj, 0, &p, &len);
  return(s7_make_counted_string(sc, p, len));
}


pointer s7_error(scheme *sc, pointer type, pointer info)
{
  return(s7_F(sc));
}

pointer s7_throw(scheme *sc, pointer type, pointer info)
{
  return(s7_F(sc));
}

pointer s7_wrong_type_arg_error(scheme *sc, const char *caller, int arg_n, pointer arg, const char *descr)
{
  return(s7_F(sc));
}

pointer s7_out_of_range_error(scheme *sc, const char *caller, int arg_n, pointer arg, const char *descr)
{
  return(s7_F(sc));
}

/* remv -- produce new list */
pointer s7_remv(scheme *sc, pointer a, pointer obj) 
{
  pointer p = sc->NIL;
  for ( ; s7_is_pair(a); a = cdr(a))
    if (car(a) != obj)
      p = cons(sc, car(a), p);
  return(s7_reverse(sc, p));
}
