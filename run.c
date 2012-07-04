/* run macro
 *   
 *   Rather than write/compile (via gcc) a C source file, as in CL/CLM, this
 *   produces the intermediate "triples" on the fly, packaging them into
 *   a "program" (a list of triples), and precomputing all function, program,
 *   and data addresses.
 *
 * The evaluator is eval_ptree.  The code walker is walk.  A program
 *   is a list of triples. Each triple has a function pointer and a
 *   pointer to addresses of arguments and data.  There are two special addresses:
 *   the program counter (PC) and the termination flag (pt->all_done). 
 *
 * Snd optimization flag determines how safe we try to be:
 *   0: no use of ptrees at all (fallback on Scheme)
 *   6: all opts
 *
 *
 * currently handled, at least partially:
 *
 *   types: float int char string boolean symbol keyword vct snd_fd mus_any vector function list
 *
 *   lambda (use 'declare' to set arg types)
 *   call-with-current-continuation call/cc
 *   if begin or and not let let* set! cond do define case[int keys]
 *   * + - / > < >= <= = max min 1+ 1-
 *   sin cos tan abs log exp expt acos asin atan sqrt cosh sinh tanh j0 j1 jn y0 y1 yn i0 erf erfc lgamma asinh acosh atanh
 *   boolean? exact? inexact? integer? real? number? quote
 *   odd? even? zero? positive? negative? eq? eqv? equal? symbol? symbol->string
 *   round truncate floor ceiling exact->inexact inexact->exact
 *   gcd lcm logand logior logxor lognot ash modulo remainder quotient random
 *   char? char=? char<? char>? char<=? char>=? and char-ci*
 *   char-alphabetic? char-numeric? char-lower-case? char-upper-case? char-whitespace? 
 *   char-upcase char-downcase char->integer integer->char
 *   string? string string-length string-copy string-fill! string-ref string-set!
 *   make-string substring string-append string=? string<=? string>=? string<? string>? and string-ci*
 *   display number->string format
 *   make-vector if 2nd arg exists and is float
 *   list ops that reference constant lists and return something we can handle (like a number)
 *   throw with just the 1st arg (experimental...)
 *
 *   various sndlib, clm, snd, and s7 functions
 *
 * limitations: 
 *      variables can have only one type, the type has to be ascertainable somehow at run time (similarly for vector elements)
 *      some variables (imported from outside our context) cannot be set
 *      no recursion (could be added with some pain)
 *      no complex, ratio, bignum (but we use 64-bit ints)
 *      no pointer aliasing (i.e. vct var set to alias another vct var etc -- GC confusion otherwise)
 *      no apply or eval (we need to know at parse time what we are trying to do -- actually these might be doable)
 *      no map or for-each (these need lists, but for-each's lists aren't changing...)
 *
 * whenever the code-walker or tree initializer finds something it is unhappy about,
 *  it returns an error indication, and the caller should fallback on Scheme's evaluator.
 *
 * so where does the speed-up come from? We're not getting/freeing any Scheme memory so the gc is (well, almost) never
 *   triggered, we're doing math ops direct, no function args are cons'd, no run-time types are checked, no values are boxed/unboxed,
 *   no symbols are looked-up in the current environment, wherever possible we pre-convert
 *   args to same type (i.e. int->float done just once, if possible)
 *
 * (add-hook! optimization-hook (lambda (n) (snd-display "opt: ~A~%" n)))
 */

/* make-env needs vct args because '(...) and (list...) return #f (') or a parser complaint (list).
 *   currently make-rand|-interp distribution arg (an env) can't be a vct
 */

/*
 * this doesn't get optimized yet: (set! ((mus-data gen) 123) .1)
 *   but the ref side does work: (let ((fr (frame .1 .2 .3))) (run (lambda () ((mus-data fr) 0))))
 *   set! needs to look down a level if caar is a list
 *   an example is sample-pvoc3 in clm23.scm: (vct-set! (phase-vocoder-amps sr) k ...)
 *
 * run doesn't always warn about a closure (explicit gen basically) -- if it's used directly,
 *     there's no warning, but it doesn't handle the closed-over variables correctly
 */


/* some timings (I keep losing these stats, so I'll put them here for safekeeping, "*"=not optimizable)
 *     valgrind --tool=callgrind snd ws.scm [etc -- no stats, not to snd, *clm-output-safety*=1]
 *
 *      test                                           10.4   (10.7?)   11.4   11.10  12.2  unrun[28-Jun-11] [13-Aug][6-Dec]
 * 
 * () (fm-violin 0 20 440 .1)                          1068     642     561     528    479    3692   7.7       921    511 -- 4897 in snd 12.2, 7392 in 11.10, 370 in C
 * (:channels 2) (fm-violin 0 20 440 .1 :degree 45)    1228     764     687     570    505    3722   7.4       951    581 ;(538)
 * (:reverb jc-reverb) (fm-violin 0 20 440 .1)         2577    1455    1335    1153    948    8747   9.2      5336   1668 ;(1483)  
 * (:reverb nrev) (fm-violin 0 20 440 .1)              2983    1812    1685    1503   1203   10443   8.7      6572   1637 ;(1588)
 * () (p 0 3)                                         91020*   3011    2828    2817   1965   43241  22.0     38751  36788 ;(38331)
 * () (expandn 0 10 "oboe.snd" 1 :expand 4)            1228     526     464     456    301    3526  11.7      2508   1949 ;(2098)
 * (calling-all-animals)                              16359   11684   10306    9841  10529   55207   5.2     42210  28851 ;(31635)
 * () (pins 0 3 "oboe.snd" 1.0 :max-peaks 8)           1207     783     660     700    544    5955  10.9      4943   3992 ;(4464)
 * (load "popi.scm")                                  11042    6391    5923    4756   4154   34163   8.2     16286   6254 ;(6051)
 * () (singer 0 .1                                     1015     641     562     674    363    7089  19.5      6474   4895 ;(5752)
 *     (list (list .4 ehh.shp test.glt 523.0 .8 0.0 .01) (list .6 oo.shp test.glt 523.0 .7 .1 .01))))
 * (:channels 2)                                        206     139      93     107     72     240   3.4       193    180 ;(191)
 *   (let ((file "oboe.snd")) (grani 0 1 .5 "oboe.snd" :grain-envelope '(0 0 0.2 0.2 0.5 1 0.8 0.2 1 0))))
 * () (do ((i 0 (+ i 1))) ((= i 10000))                7120    5069    4064    3996   3560   21380   6.0      4883   2794 ;(2813)
 *     (fm-violin (* i .001) .01 440 .001)))
 * (:channels 2)                                        283     220     158     167    101     286   2.8       247    216 ;(220)
 *   (fullmix "pistol.snd" 0 2 0 #f .5)  
 *   (fullmix "oboe.snd" 1 2 0 (list (list .1 (make-env '(0 0 1 1) :duration 2 :scaler .5)))))
 *                         1st case in clm-ins.scm:   12201    1138    1043     968    707    3025   4.3      2517   1928 ;(2079)
 */

#include <mus-config.h>

#if (defined(__GNUC__)) && (!(defined(__cplusplus)))
  #define _GNU_SOURCE
  /* this is needed to get the vasprintf declaration */
#endif

#if HAVE_SCHEME
/* (almost) entire file is on this switch */

#define WITH_COUNTERS 0

#if USE_SND
  #include "snd.h"
#endif

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>

#if (defined(HAVE_LIBC_H) && (!defined(HAVE_UNISTD_H)))
  #include <libc.h>
#else
  #if (!(defined(_MSC_VER)))
    #include <unistd.h>
  #endif
#endif
#if HAVE_STRING_H
  #include <string.h>
#endif
#include <stdarg.h>

#include "_sndlib.h"
#include "xen.h"
#include "clm.h"
#include "sndlib2xen.h"
#include "vct.h"
#include "clm2xen.h"
#include "clm-strings.h"
#include "sndlib-strings.h"

#define S_make_vct       "make-vct"
#define S_vct_addB       "vct-add!"
#define S_vct_subtractB  "vct-subtract!"
#define S_vct_copy       "vct-copy"
#define S_vct_length     "vct-length"
#define S_vct_multiplyB  "vct-multiply!"
#define S_vct_offsetB    "vct-offset!"
#define S_vct_ref        "vct-ref"
#define S_vct_scaleB     "vct-scale!"
#define S_vct_fillB      "vct-fill!"
#define S_vct_setB       "vct-set!"
#define S_vct_mapB       "vct-map!"
#define S_vct_peak       "vct-peak"
#define S_vct_p          "vct?"
#define S_list_to_vct    "list->vct"
#define S_vct_to_list    "vct->list"
#define S_vector_to_vct  "vector->vct"
#define S_vct_to_vector  "vct->vector"
#define S_vct_moveB      "vct-move!"
#define S_vct_subseq     "vct-subseq"
#define S_vct            "vct"
#define S_vct_reverse    "vct-reverse!"
#define S_vct_to_string  "vct->string"
#if HAVE_RUBY
  #define S_vct_times    "vct_multiply"
  #define S_vct_plus     "vct_add"
#else
  #define S_vct_times    "vct*"
  #define S_vct_plus     "vct+"
#endif

#define NOT_A_GC_LOC -1

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


static int safe_strcasecmp(const char *s1, const char *s2)
{
  int len1, len2, len;
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
  len = len1;
  if (len1 > len2)
    len = len2;

  for (i = 0; i < len; i++)
    if (toupper(s1[i]) < toupper(s2[i]))
      return(-1);
    else
      {
	if (toupper(s1[i]) > toupper(s2[i]))
	  return(1);
      }

  if (len1 < len2) 
    return(-1);
  if (len1 > len2)
    return(1);

  return(0);
}


static bool optimizing = true;
static s7_pointer optimization_hook;
#define S_optimization_hook "optimization-hook"


static s7_pointer scheme_false, scheme_true, scheme_nil, scheme_undefined, scheme_zero;

#define scheme_to_c_bool(Arg) ((Arg == scheme_true) ? true : false)
#define scheme_cadr(Arg)      s7_cadr(Arg)
#define scheme_caddr(Arg)     s7_caddr(Arg)
#define scheme_cadddr(Arg)    s7_cadddr(Arg)
#define scheme_cddr(Arg)      s7_cddr(Arg)
#define scheme_cdddr(Arg)     s7_cdddr(Arg)
#define scheme_caar(a)        s7_caar(a)
#define scheme_cdar(a)        s7_cdar(a)
#define scheme_cdadr(a)       s7_cdadr(a)
#define scheme_list_1(Arg)    s7_cons(s7, Arg, scheme_nil)
#define scheme_list_2(Arg1, Arg2) s7_cons(s7, Arg1, s7_cons(s7, Arg2, scheme_nil))

static s7_pointer scheme_make_string(const char *str)
{
  return((str) ? s7_make_string(s7, str) : scheme_false);
}

#define Int mus_long_t
#define Double double

#define INTEGER_TO_STRING(a)                s7_number_to_string(s7, s7_make_integer(s7, a), 10)
#define INTEGER_TO_STRING_WITH_RADIX(a, b)  s7_number_to_string(s7, s7_make_integer(s7, a), b)
#define DOUBLE_TO_STRING(a)                 s7_number_to_string(s7, s7_make_real(s7, a), 10)
#define DOUBLE_TO_STRING_WITH_RADIX(a, b)   s7_number_to_string(s7, s7_make_real(s7, a), b)

static s7_pointer walker_hash_table;
#define scheme_walker(Obj)                     s7_hash_table_ref(s7, walker_hash_table, Obj)
#define scheme_set_walker(Obj, Val)            s7_hash_table_set(s7, walker_hash_table, Obj, Val)


#define UNLIMITED_ARGS -1
static s7_pointer walk_sym;

/* find and set (Scheme) variable values */
static void xen_symbol_name_set_value(const char *a, s7_pointer b)
{
  s7_pointer var;
  var = s7_make_symbol(s7, a);
  if (var != scheme_false)
    s7_symbol_set_value(s7, var, b);
}

/* (define ho (let ((b 2)) (lambda (a) (+ a b))))
 * (run (lambda () (+ (ho 1) 2)))
 */

static s7_pointer symbol_to_value(s7_pointer code, s7_pointer sym)
{
  /*
  fprintf(stderr, "look for %s: %s, env: %s, %p\n", 
	  s7_object_to_c_string(s7, sym), 
	  s7_object_to_c_string(s7, s7_symbol_local_value(s7, sym, s7_cdr(code))),
	  s7_object_to_c_string(s7, s7_cdr(code)),
	  s7_cdr(code));
  */
  if (code != scheme_false)
    return(s7_symbol_local_value(s7, sym, s7_cdr(code)));
  return(s7_symbol_value(s7, sym));
}

static void symbol_set_value(s7_pointer code, s7_pointer sym, s7_pointer new_val)
{
  s7_symbol_set_value(s7, sym, new_val);
}


/* -------------------------------------------------------------------------------- */


enum {R_UNSPECIFIED, R_INT, R_FLOAT, R_BOOL, R_CHAR, R_STRING, R_LIST,
      R_SYMBOL, R_KEYWORD, R_FUNCTION, R_GOTO, R_VCT, 
#if USE_SND
      R_SAMPLER, R_SOUND, R_MIX, R_MARK, R_REGION,
#endif 
      R_SOUND_DATA, R_CLM, 
      R_FLOAT_VECTOR, R_INT_VECTOR, R_VCT_VECTOR, R_LIST_VECTOR, R_CLM_VECTOR,

      /* next 8 are for walker arg checks, generator=built-in or defgenerator */
      R_NUMBER, R_VECTOR, R_XEN, R_NUMBER_CLM, R_NUMBER_VCT, 
      R_NUMBER_SOUND_DATA, R_GENERATOR, 
      R_ANY}; 

#if USE_SND
#define BUILT_IN_TYPES 33
#else
#define BUILT_IN_TYPES 27
#endif

static int last_type = R_ANY;
static int type_names_size = BUILT_IN_TYPES;
static const char **type_names = NULL;
static const char *basic_type_names[BUILT_IN_TYPES] = {"unspecified", "int", "float", "boolean", "char", "string", "list",
						       "symbol", "keyword", "function", "continuation", "vct", 
#if USE_SND
						       "sampler", "sound", "mix", "mark", "region",
#endif
						       "sound-data", "clm", 
						       "float-vector", "int-vector", "vct-vector", "list-vector", "clm-vector",
						       "number", "vector", "xen", "number or clm", "number or vct", 
						       "number or sound-data", "generator", "any"};

static void init_type_names(void)
{
  int i;
  type_names = (const char **)malloc(BUILT_IN_TYPES * sizeof(char *));
  for (i = 0; i < BUILT_IN_TYPES; i++)
    type_names[i] = basic_type_names[i];
}


static const char *type_name(int id) 
{
  if ((id >= R_UNSPECIFIED) && (id <= last_type)) 
    return(type_names[id]); 
  return("unknown");
}

static int add_new_type(const char *new_type)
{
  if (last_type == (type_names_size - 1))
    {
      int i;
      type_names_size += 8;
      type_names = (const char **)realloc(type_names, type_names_size * sizeof(char *));
      for (i = last_type + 1; i < type_names_size; i++) type_names[i] = NULL;
    }
  last_type++;
  type_names[last_type] = mus_strdup(new_type);

  return(last_type);
}


static int name_to_type(const char *name)
{
  int i;
  for (i = 0; i <= last_type; i++)
    if (mus_strcmp(name, type_names[i]))
      return(i);
  return(R_UNSPECIFIED);
}


#define POINTER_P(Type) (((Type) >= R_VCT) && ((Type) <= R_CLM_VECTOR)) /* exclude R_ANY */
#define VECTOR_P(Type) (((Type) >= R_FLOAT_VECTOR) && ((Type) <= R_CLM_VECTOR))
#define CLM_STRUCT_P(Type) ((Type) > R_ANY)

typedef enum {R_VARIABLE, R_CONSTANT, R_TEMPORARY} xen_value_constant_t;
typedef enum {DONT_NEED_RESULT, NEED_ANY_RESULT, NEED_INT_RESULT} walk_result_t;

static bool run_warned = false;

typedef struct {
  int type;
  int addr;
  xen_value_constant_t constant;
  bool gc;
} xen_value;

typedef struct {
  const char *name;
  xen_value *v;
  bool global, unclean, unsettable;
} xen_var;

typedef struct {
  char *name;
  xen_value *result, *jump;
  int loc;
} continuation;

typedef struct {
  int len, type; /* type is either clm-struct ref or a single type */
  xen_value **vals;
} list;

typedef struct {
  int length;
  union {
    Int *ints;
    mus_any **gens;
    vct **vcts;
    list **lists;
  } data;
} vect;


typedef struct ptree {
  bool all_done;
  struct triple **pc;
  struct triple **program;
  Int *ints; 
  Double *dbls;
  int program_size, ints_size, dbls_size, triple_ctr, int_ctr, dbl_ctr;
  xen_var **vars;
  int vars_size, var_ctr;
  xen_var **global_vars;
  int global_vars_size, global_var_ctr;
  xen_value *result;
  int *args; 
  int *default_args; 
  int *arg_types;
  int arity;
  continuation **gotos;
  int goto_ctr, gotos_size;
  xen_value **gcs;
  int gc_ctr, gcs_size;
  int *gc_protected;
  int gc_protected_ctr, gc_protected_size;
  struct triple **initial_pc;
  s7_pointer code, form;
  int form_loc;
  int str_ctr, strs_size;
  char **strs;
  int vct_ctr, vcts_size;
  vct **vcts;
  int sd_ctr, sds_size;
  sound_data **sds;
  int clm_ctr, clms_size;
  mus_any **clms;
  int *clm_locs;
  int vect_ctr, vects_size;
  vect **vects;
  int list_ctr, lists_size;
  list **lists;
#if USE_SND
  int sampler_ctr, samplers_size;
  snd_fd **samplers;
#endif
  int fnc_ctr, fncs_size;
  struct ptree **fncs;
  int xen_ctr, xens_size;
  s7_pointer *xens;
  int xen_vars_size, xen_var_ctr;
  xen_value ***xen_vars;
  struct ptree *outer_tree;
  /* next needed by tree builders (temps) */
  int constants;
  bool float_result;
  walk_result_t walk_result;
  int *lambda_arg_types;
  int lambda_args;
  bool got_lambda; /* a temporary kludge?? */
} ptree;

/*
 * if we know in advance the arg types, it is cleaner to pass that as an automatic declare list to lambda_form,
 *   rather than insisting on a declare form, but rewriting the lambda form to insert the declare seems clumsy.
 *   Adding two or three extra args to walk is even clumsier, since this is a very rare special case, and using
 *   global variables is prone to trouble if we have nested lambdas, so... we use the ptree fields lambda_arg_types
 *   and lambda_args to pass this info in during the arg parsing in walk.  An embedded lambda will have its
 *   own embedded ptree, so we get the local variable effect from the ptree structure.
 */

typedef struct triple {
  void (*function)(int *arg_addrs, ptree *pt);
  int *args, *types;
  bool no_opt;
  const char *op_name;
  int num_args;
#if WITH_COUNTERS
  int func_loc;
#endif
} triple;


typedef struct {
  void (*func)(int *args, ptree *pt);
  const char *func_name;
  void (*mult_func)(int *args, ptree *pt);
  const char *mult_func_name;
  void (*env_func)(int *args, ptree *pt);
  const char *env_func_name;
} opt_ops;



static int allocate_xen_vars(ptree *pt, int size)
{
  int cur;
  cur = pt->xen_var_ctr;
  if (cur >= pt->xen_vars_size)
    {
      pt->xen_vars_size += 4;
      if (pt->xen_vars)
	{
	  int i;
	  pt->xen_vars = (xen_value ***)realloc(pt->xen_vars, pt->xen_vars_size * sizeof(xen_value **));
	  for (i = cur; i < pt->xen_vars_size; i++)
	    pt->xen_vars[i] = NULL;
	}
      else pt->xen_vars = (xen_value ***)calloc(pt->xen_vars_size, sizeof(xen_value **));
    }
  pt->xen_vars[pt->xen_var_ctr++] = (xen_value **)calloc(size, sizeof(xen_value *));
  return(cur);
}


static triple *free_triple(triple *trp)
{
  if (trp->args) free(trp->args);
  trp->args = NULL;
  if (trp->types) free(trp->types);
  trp->types = NULL;
  free(trp);
  return(NULL);
}

static xen_value *make_xen_value(int typ, int address, xen_value_constant_t constant)
{
  xen_value *v;
  v = (xen_value *)malloc(sizeof(xen_value));
  v->type = typ;
  v->addr = address;
  v->constant = constant;
  v->gc = false;
  return(v);
}


#if (!USE_SND)

/* copy run_hook from snd-xen.c */
XEN run_hook(s7_pointer hook, s7_pointer args, const char *caller)
{
  int gc_loc;
  s7_pointer procs = XEN_HOOK_PROCEDURES(hook);
  gc_loc = s7_gc_protect(s7, args);

  while (procs != scheme_nil)
    {
      if (!(s7_is_eq(args, scheme_nil)))
	s7_call(s7, s7_car(procs), args);
      else s7_call(s7, s7_car(procs), scheme_nil);
      procs = s7_cdr(procs);
    }

  s7_gc_unprotect_at(s7, gc_loc);
  return(scheme_false);
}

#endif


#if (!HAVE_VASPRINTF)
  #define OPTIMIZER_WARNING_BUFFER_SIZE 1024
  static char *optimizer_warning_buffer = NULL;
#endif

#ifdef __GNUC__
static xen_value *run_warn(const char *format, ...) __attribute ((format (printf, 1, 2)));
#endif

static xen_value *run_warn(const char *format, ...)
{
  run_warned = true;
  if (XEN_HOOKED(optimization_hook))
    {
      va_list ap;
      char *result;

      va_start(ap, format);

#if HAVE_VASPRINTF
      if (vasprintf(&result, format, ap) == -1)
	result = NULL;
#else

      if (!optimizer_warning_buffer)
	optimizer_warning_buffer = (char *)calloc(OPTIMIZER_WARNING_BUFFER_SIZE, sizeof(char));
#if HAVE_VSNPRINTF
      vsnprintf(optimizer_warning_buffer, OPTIMIZER_WARNING_BUFFER_SIZE, format, ap);
#else
      vsprintf(optimizer_warning_buffer, format, ap);
#endif

      result = optimizer_warning_buffer;
#endif

      va_end(ap);

      run_hook(optimization_hook, 
	       scheme_list_1(scheme_make_string(result)),
	       S_optimization_hook);

#if HAVE_VASPRINTF
      free(result);
#endif
    }
  return(NULL); /* this is so we can insert the call into the error return call chain */
}


static xen_value *run_warn_with_free(char *str)
{
  s7_pointer msg;
  run_warned = true;
  msg = scheme_make_string(str);
  free(str);

  if (XEN_HOOKED(optimization_hook))
    run_hook(optimization_hook, 
	     scheme_list_1(msg),
	     S_optimization_hook);

  return(NULL);
}


static xen_value *copy_xen_value(xen_value *v)
{
  return(make_xen_value(v->type, v->addr, v->constant));
}


static char *describe_xen_value(xen_value *v, ptree *pt);
static char *describe_xen_value_1(int type, int addr, ptree *pt);
static char *describe_xen_value_with_var_name(int type, int addr, ptree *pt);

static char *describe_xen_var(xen_var *var, ptree *pt)
{
  char *buf, *temp;
  if (var == NULL) return(mus_strdup("#<xen_var: null>"));
  temp = describe_xen_value_1(var->v->type, var->v->addr, pt);
  if (temp)
    {
      buf = (char *)calloc(strlen(var->name) + strlen(temp) + 32, sizeof(char));
      sprintf(buf, "%s: %s (%s%s%s)", 
	      var->name, temp,
	      (var->global) ? "global" : "local",
	      (var->unclean) ? " set" : "",
	      (var->unsettable) ? " unsettable" : "");
      free(temp);
    }
  else buf = mus_strdup(var->name);
  return(buf);
}


static continuation *free_goto(continuation *c)
{
  if (c)
    {
      if (c->name) free(c->name);
      if (c->result) free(c->result);
      if (c->jump) free(c->jump);
      free(c);
    }
  return(NULL);
}


static xen_var *find_var_in_ptree(ptree *pt, const char *name)
{
  /* search backwards for shadowing */
  int i;
  for (i = pt->var_ctr - 1; i >= 0; i--)
    if ((pt->vars[i]) &&
	(mus_strcmp(pt->vars[i]->name, name)))
      return(pt->vars[i]);
  for (i = 0; i < pt->global_var_ctr; i++)
    if ((pt->global_vars[i]) &&
	(mus_strcmp(pt->global_vars[i]->name, name)))
      return(pt->global_vars[i]);
  return(NULL);
}


static xen_var *find_var_in_ptree_via_addr(ptree *pt, int type, int addr)
{
  /* search backwards for shadowing */
  int i;
  for (i = pt->var_ctr - 1; i >= 0; i--)
    if ((pt->vars[i]) &&
	(pt->vars[i]->v->addr == addr) &&
	(pt->vars[i]->v->type == type))
      return(pt->vars[i]);
  for (i = 0; i < pt->global_var_ctr; i++)
    if ((pt->global_vars[i]) &&
	(pt->global_vars[i]->v->addr == addr) &&
	(pt->global_vars[i]->v->type == type))
      return(pt->global_vars[i]);
  return(NULL);
}


static char *str_append(char *oldstr, int *oldsize, char *newstr)
{
  oldstr = mus_strcat(oldstr, newstr, oldsize);
  free(newstr);
  return(oldstr);
}


#define INT_PT  "i%d(%lld)"
#define INT_STR "%lld"
#define B2S(Arg) ((Arg) ? "#t" : "#f")
#define GO_PT   "cont%d(continuation)"


static char *describe_triple(triple *trp, ptree *pt)
{
  char *str = NULL;
  if (trp)
    {
      if (trp->args)
	{
	  int i, size = 0;
	  char **descrs;
	  descrs = (char **)malloc(trp->num_args * sizeof(char *));
	  for (i = 0; i < trp->num_args; i++)
	    {
	      descrs[i] = describe_xen_value_with_var_name(trp->types[i], trp->args[i], pt);
	      size += (mus_strlen(descrs[i]) + 2);
	    }
	  str = (char *)calloc(size + mus_strlen(trp->op_name) + 8, sizeof(char));
	  str = strcat(str, descrs[0]);
	  str = strcat(str, " = ");
	  str = strcat(str, trp->op_name);
	  str = strcat(str, "(");
	  for (i = 1; i < (trp->num_args - 1); i++)
	    {
	      str = strcat(str, descrs[i]);
	      str = strcat(str, ", ");
	    }
	  if (trp->num_args > 1)
	    str = strcat(str, descrs[trp->num_args - 1]);
	  str = strcat(str, ")");
	  for (i = 0; i < trp->num_args; i++)
	    free(descrs[i]);
	  free(descrs);
	}
      else
	{
	  str = mus_strdup(trp->op_name);
	}
    }
  return(str);
}


static char *describe_ptree(ptree *pt, const char *space)
{
  #define INT_STR_SIZE 32
  int i, size;
  char *temp = NULL;
  char *buf;
  Int *inner_ints;
  Double *inner_dbls;
  char **inner_strs;
  vct **inner_vcts;
  sound_data **inner_sds;
  mus_any **inner_clms;
  vect **inner_vects;
  list **inner_lists;
#if USE_SND
  snd_fd **inner_samplers;
#endif
  ptree **inner_fncs;
  s7_pointer *inner_xens;
  xen_value ***inner_xen_vars;

  size = 1024;
  buf = (char *)calloc(size, sizeof(char));

  inner_ints = pt->ints;
  inner_dbls = pt->dbls;
  inner_xen_vars = pt->xen_vars;
  inner_strs = pt->strs;
  inner_vcts = pt->vcts;
  inner_sds = pt->sds;
  inner_clms = pt->clms;
  inner_vects = pt->vects;
  inner_lists = pt->lists;
  inner_fncs = (ptree **)(pt->fncs);
  inner_xens = pt->xens;
#if USE_SND
  inner_samplers = pt->samplers;
#endif
  if (pt->outer_tree)
    {
      pt->ints = pt->outer_tree->ints;
      pt->dbls = pt->outer_tree->dbls;
      pt->strs = pt->outer_tree->strs;
      pt->vcts = pt->outer_tree->vcts;
      pt->sds = pt->outer_tree->sds;
      pt->clms = pt->outer_tree->clms;
      pt->vects = pt->outer_tree->vects;
      pt->lists = pt->outer_tree->lists;
      pt->fncs = pt->outer_tree->fncs;
      pt->xens = pt->outer_tree->xens;
#if USE_SND
      pt->samplers = pt->outer_tree->samplers;
#endif
      pt->xen_vars = pt->outer_tree->xen_vars;
    }

  buf = str_append(buf, &size, mus_format("\n%sints: %d, dbls: %d, triples: %d, vars: %d", space, pt->int_ctr, pt->dbl_ctr, pt->triple_ctr, pt->var_ctr));
  if (pt->strs_size > 0) buf = str_append(buf, &size, mus_format(", strs: %d", pt->str_ctr));
  if (pt->vcts_size > 0) buf = str_append(buf, &size, mus_format(", vcts: %d", pt->vct_ctr));
  if (pt->sds_size > 0) buf = str_append(buf, &size, mus_format(", sds: %d", pt->sd_ctr));
  if (pt->clms_size > 0) buf = str_append(buf, &size, mus_format(", clms: %d", pt->clm_ctr));
  if (pt->vects_size > 0) buf = str_append(buf, &size, mus_format(", vects: %d", pt->vect_ctr));
  if (pt->lists_size > 0) buf = str_append(buf, &size, mus_format(", lists: %d", pt->list_ctr));
  if (pt->fncs_size > 0) buf = str_append(buf, &size, mus_format(", fncs: %d", pt->fnc_ctr));
  if (pt->xens_size > 0) buf = str_append(buf, &size, mus_format(", xens: %d", pt->xen_ctr));
#if USE_SND
  if (pt->samplers_size > 0) buf = str_append(buf, &size, mus_format(", rds: %d", pt->sampler_ctr));
#endif
  if (pt->xen_vars_size > 0) buf = str_append(buf, &size, mus_format(", xenvars: %d", pt->xen_var_ctr));

  buf = str_append(buf, &size, mus_format("\n%s[", space));

  if (pt->int_ctr > 0) 
    {
      for (i = 0; i < pt->int_ctr - 1; i++)
	{
	  temp = (char *)calloc(INT_STR_SIZE, sizeof(char));
	  mus_snprintf(temp, INT_STR_SIZE, INT_STR ", ", pt->ints[i]);
	  buf = str_append(buf, &size, temp);
	}
      temp = (char *)calloc(INT_STR_SIZE, sizeof(char));
      mus_snprintf(temp, INT_STR_SIZE, INT_STR "]%s", 
		   pt->ints[pt->int_ctr - 1],
		   (pt->dbl_ctr > 0) ? ", [" : "\n");
      buf = str_append(buf, &size, temp);
    }

  if (pt->dbl_ctr > 0)
    {
      for (i = 0; i < pt->dbl_ctr - 1; i++)
	{
	  temp = (char *)calloc(INT_STR_SIZE, sizeof(char));
	  mus_snprintf(temp, INT_STR_SIZE, "%.4f, ", pt->dbls[i]);
	  buf = str_append(buf, &size, temp);
	}
      temp = (char *)calloc(INT_STR_SIZE, sizeof(char));
      mus_snprintf(temp, INT_STR_SIZE, "%.4f]\n", pt->dbls[pt->dbl_ctr - 1]);
      buf = str_append(buf, &size, temp);
    }

  buf = str_append(buf, &size, mus_strdup("\n"));

  for (i = 0; i < pt->var_ctr; i++)
    {
      temp = describe_xen_var(pt->vars[i], pt);
      buf = str_append(buf, &size, mus_format("%s[var %d]: %s\n", space, i, temp));
      free(temp);
    }

  for (i = 0; i < pt->global_var_ctr; i++)
    {
      temp = describe_xen_var(pt->global_vars[i], pt);
      buf = str_append(buf, &size, mus_format("%s%s\n", space, temp));
      free(temp);
    }

  if (pt->result)
    {
      temp = describe_xen_value_with_var_name(pt->result->type, pt->result->addr, pt);
      if (temp)
	{
	  buf = str_append(buf, &size, mus_format("%sresult: %s\n\n", space, temp));
	  free(temp);
	}
    }

  for (i = 0; i < pt->triple_ctr; i++)
    {
      temp = describe_triple(((triple **)(pt->program))[i], pt);
      if (temp)
	{
	  buf = str_append(buf, &size, mus_format("%s%d: %s\n", space, i, temp));
	  free(temp);
	}
    }

  pt->ints = inner_ints;
  pt->dbls = inner_dbls;
  pt->xen_vars = inner_xen_vars;
  pt->strs = inner_strs;
  pt->vcts = inner_vcts;
  pt->sds = inner_sds;
  pt->clms = inner_clms;
  pt->vects = inner_vects;
  pt->lists = inner_lists;
  pt->fncs = (struct ptree **)(inner_fncs);
  pt->xens = inner_xens;
#if USE_SND
  pt->samplers = inner_samplers;
#endif
  return(buf);
}


static char *describe_xen_value_1(int type, int addr, ptree *pt)
{
  switch (type)
    {
    case R_BOOL:    return(mus_format("b%d(%s)", addr, B2S(pt->ints[addr])));           break;
    case R_INT:     return(mus_format(INT_PT , addr, pt->ints[addr]));                  break;
    case R_CHAR:    return(mus_format("chr%d(#\\%c)", addr, (char)(pt->ints[addr])));   break;
    case R_STRING:  return(mus_format("s%d(\"%s\")", addr, pt->strs[addr]));            break;
    case R_FLOAT:   return(mus_format("d%d(%.4f)", addr, pt->dbls[addr]));              break;
    case R_GOTO:    return(mus_format("continuation: " INT_PT , addr, pt->ints[addr])); break;
    case R_LIST:    return(mus_format("list%d(%p)", addr, pt->lists[addr]));            break;

    case R_SYMBOL:
    case R_KEYWORD: 
      {
	char *temp = NULL, *str;
	if ((pt->xens) && 
	    (pt->xens[addr] != scheme_undefined))
	  str = mus_format("%s%d(%s)", (type == R_SYMBOL) ? "sym" : "key", addr, temp = s7_object_to_c_string(s7, pt->xens[addr])); 
	else str = mus_format("%s%d=0", (type == R_SYMBOL) ? "sym" : "key", addr);
	if (temp) free(temp);
	return(str);
      }

      break;

    case R_FLOAT_VECTOR:
    case R_VCT:
      if ((pt->vcts) && (pt->vcts[addr]))
	{
	  char *buf = NULL, *vstr = NULL;
	  vstr = mus_vct_to_string(pt->vcts[addr]);
	  buf = mus_format("vct%d(%p)", addr, vstr);
	  if (vstr) free(vstr);
	  return(buf);
	}
      else return(mus_format("vct%d(null)", addr));
      break;

    case R_SOUND_DATA:
      if ((pt->sds) && (pt->sds[addr]))
	{
	  char *buf = NULL, *vstr = NULL;
	  vstr = sound_data_to_string(pt->sds[addr]);
	  buf = mus_format("sd%d(%p)", addr, vstr);
	  if (vstr) free(vstr);
	  return(buf);
	}
      else return(mus_format("sd%d(null)", addr));
      break;

#if USE_SND
    case R_SAMPLER:
      if ((pt->samplers) && (pt->samplers[addr]))
	{
	  char *buf = NULL, *vstr = NULL;
	  vstr = sampler_to_string(pt->samplers[addr]);
	  buf = mus_format("rd%d(%s)", addr, vstr);
	  if (vstr) free(vstr);
	  return(buf);
	}
      else return(mus_strdup("null"));
      break;
#endif

    case R_CLM:
      if ((pt->clms) && (pt->clms[addr]))
	{
	  char *str1, *str2;
	  str1 = mus_describe(pt->clms[addr]);
	  str2 = mus_format("clm%d(%s)", addr, str1);
	  if (str1) free(str1);
	  return(str2);
	}
      else return(mus_format("clm%d(null)", addr));
      break;

    case R_FUNCTION: 
      if ((pt->fncs) && (pt->fncs[addr]))
	return(describe_ptree(((ptree **)(pt->fncs))[addr], "      "));
      else return(mus_strdup("internal lambda?"));
      break;
      
    case R_LIST_VECTOR:
    case R_INT_VECTOR:  
    case R_VCT_VECTOR:
    case R_CLM_VECTOR:  
      return(mus_format("vect%d(%p)", addr, pt->vects[addr])); 
      break;

    case R_UNSPECIFIED: 
      return(mus_strdup("#<unspecified>")); 
      break;

    case R_XEN: 
      return(mus_strdup("#<xen (scheme) value>"));
      break;

    default:
      if (CLM_STRUCT_P(type))
	return(mus_format("clm-struct%d(%s: %p)", addr, type_name(type), (pt->lists) ? pt->lists[addr] : NULL));
      else return(mus_format("?%d(unknown type: %d)", addr, type));            
      break;

    }
  return(NULL);
}


static char *describe_xen_value_with_var_name(int type, int addr, ptree *pt)
{
  xen_var *xv;
  xv = find_var_in_ptree_via_addr(pt, type, addr);
  if (xv)
    {
      char *temp, *res;
      temp = describe_xen_value_1(type, addr, pt);
      res = mus_format("[%s] %s", xv->name, temp);
      free(temp);
      return(res);
    }

  return(describe_xen_value_1(type, addr, pt));
}


static char *describe_xen_value(xen_value *v, ptree *pt)
{
  return(describe_xen_value_1(v->type, v->addr, pt));
}


static ptree *make_ptree(int initial_data_size)
{
  ptree *pt;
  pt = (ptree *)calloc(1, sizeof(ptree));
  pt->got_lambda = false;
  if (initial_data_size > 0)
    {
      pt->ints = (Int *)calloc(initial_data_size, sizeof(Int));
      pt->dbls = (Double *)calloc(initial_data_size, sizeof(Double));
    }
  pt->ints_size = initial_data_size;
  pt->dbls_size = initial_data_size;
  pt->int_ctr = 0;
  pt->code = scheme_false;
  pt->form = scheme_undefined;
  pt->form_loc = NOT_A_GC_LOC;
  return(pt);
}


static ptree *attach_to_ptree(ptree *pt)
{
  /* share all environment tables -- memcpy? */
  ptree *new_tree;
  new_tree = (ptree *)calloc(1, sizeof(ptree));
  memcpy((void *)new_tree, (void *)pt, sizeof(ptree));
  new_tree->program_size = 0;
  new_tree->triple_ctr = 0;
  new_tree->result = NULL;
  new_tree->arity = 0;
  new_tree->code = scheme_false;
  new_tree->form = scheme_undefined;
  new_tree->form_loc = NOT_A_GC_LOC;
  if (pt->outer_tree)
    new_tree->outer_tree = pt->outer_tree;
  else new_tree->outer_tree = pt;
  new_tree->program = NULL;
  new_tree->constants = 0;
  new_tree->args = NULL;
  new_tree->default_args = NULL;
  new_tree->arg_types = NULL;
  new_tree->initial_pc = NULL;
  return(new_tree);
}


static void unattach_ptree(ptree *inner, ptree *outer)
{
  /* these may have been created while in the embedded lambda */
  outer->ints = inner->ints;
  outer->dbls = inner->dbls;
  outer->vars = inner->vars;
  outer->global_vars = inner->global_vars;
  outer->gcs = inner->gcs;
  outer->gotos = inner->gotos;
  outer->strs = inner->strs;
  outer->vcts = inner->vcts;
  outer->sds = inner->sds;
  outer->clms = inner->clms;
  outer->clm_locs = inner->clm_locs;
  outer->vects = inner->vects;
  outer->lists = inner->lists;
  outer->fncs = inner->fncs;
  outer->xens = inner->xens;
#if USE_SND
  outer->samplers = inner->samplers;
#endif
  outer->xen_vars = inner->xen_vars;

  /* outer->gc_protected = inner->gc_protected; */
  /* not this one -- we always work back to the outermost tree in add_loc_to_protected_list, and
   *   if we happen to need a reallocation of gc_protected during the inner ptree evaluation,
   *   the outermost gc_protected is realloc'd, leaving the inner ptree's pointer invalid.
   */

  inner->ints = NULL;
  inner->dbls = NULL;
  inner->vars = NULL;
  inner->global_vars = NULL;
  inner->gcs = NULL;
  inner->gotos = NULL;
  inner->strs = NULL;
  inner->vcts = NULL;
  inner->sds = NULL;
  inner->clms = NULL;
  inner->clm_locs = NULL;
  inner->vects = NULL;
  inner->lists = NULL;
  inner->fncs = NULL;
  inner->xens = NULL;
#if USE_SND
  inner->samplers = NULL;
#endif
  inner->xen_vars = NULL;
  inner->gc_protected = NULL;

  outer->ints_size = inner->ints_size;
  outer->dbls_size = inner->dbls_size;
  outer->int_ctr = inner->int_ctr;
  outer->dbl_ctr = inner->dbl_ctr;
  outer->vars_size = inner->vars_size;
  outer->var_ctr = inner->var_ctr;
  outer->global_vars_size = inner->global_vars_size;
  outer->global_var_ctr = inner->global_var_ctr;
  outer->goto_ctr = inner->goto_ctr;
  outer->gotos_size = inner->gotos_size;
  outer->gc_ctr = inner->gc_ctr;
  outer->gcs_size = inner->gcs_size;
  outer->str_ctr = inner->str_ctr;
  outer->vct_ctr = inner->vct_ctr;
  outer->sd_ctr = inner->sd_ctr;
  outer->clm_ctr = inner->clm_ctr;
  outer->list_ctr = inner->list_ctr;
  outer->vect_ctr = inner->vect_ctr;
  outer->fnc_ctr = inner->fnc_ctr;
  outer->xen_ctr = inner->xen_ctr;
#if USE_SND
  outer->sampler_ctr = inner->sampler_ctr;
#endif
  outer->strs_size = inner->strs_size;
  outer->vcts_size = inner->vcts_size;
  outer->sds_size = inner->sds_size;
  outer->clms_size = inner->clms_size;
  outer->vects_size = inner->vects_size;
  outer->lists_size = inner->lists_size;
  outer->fncs_size = inner->fncs_size;
  outer->xens_size = inner->xens_size;
#if USE_SND
  outer->samplers_size = inner->samplers_size;
#endif
  outer->xen_var_ctr = inner->xen_var_ctr;
  outer->xen_vars_size = inner->xen_vars_size;

  inner->ints_size = 0;
  inner->dbls_size = 0;
  inner->int_ctr = 0;
  inner->dbl_ctr = 0;
  inner->vars_size = 0;
  inner->var_ctr = 0;
  inner->global_vars_size = 0;
  inner->global_var_ctr = 0;
  inner->goto_ctr = 0;
  inner->gotos_size = 0;
  inner->gc_ctr = 0;
  inner->gcs_size = 0;
  inner->str_ctr = 0;
  inner->vct_ctr = 0;
  inner->sd_ctr = 0;
  inner->clm_ctr = 0;
  inner->list_ctr = 0;
  inner->vect_ctr = 0;
  inner->fnc_ctr = 0;
  inner->xen_ctr = 0;
#if USE_SND
  inner->sampler_ctr = 0;
#endif
  inner->strs_size = 0;
  inner->vcts_size = 0;
  inner->sds_size = 0;
  inner->clms_size = 0;
  inner->vects_size = 0;
  inner->lists_size = 0;
  inner->fncs_size = 0;
  inner->xens_size = 0;
#if USE_SND
  inner->samplers_size = 0;
#endif
  inner->xen_var_ctr = 0;
  inner->xen_vars_size = 0;
}

static xen_value *add_empty_var_to_ptree(ptree *prog, int type);
static xen_value *add_temporary_var_to_ptree(ptree *prog, int type);
static s7_pointer xen_value_to_xen(ptree *pt, xen_value *v);
static int def_clm_struct_field_type(int def_clm_struct_type, int field);
static xen_value *add_value_to_ptree(ptree *prog, s7_pointer val, int type);


static void list_into_list(ptree *pt, list *xl, s7_pointer lst)
{
  /* load list from ptree back into its xen original */
  int i;

  /* fprintf(stderr,"set global %d to %s\n", xl->len, s7_object_to_c_string(s7, lst)); */

  for (i = 0; i < xl->len; i++)
    if (xl->vals[i])
      s7_list_set(s7, lst, i, xen_value_to_xen(pt, xl->vals[i]));
}


static void free_list(list *xl)
{
  int i;
  if (xl->len > 0)
    {
      if (xl->vals)
	{
	  for (i = 0; i < xl->len; i++)
	    if (xl->vals[i]) 
	      free(xl->vals[i]);
	  free(xl->vals);
	  xl->vals = NULL;
	}
    }
  xl->len = 0;
  free(xl);
}

static list *xen_to_list_with_type(ptree *pt, s7_pointer lst, int type) /* list element type or clm-struct */
{
  list *xl = NULL;

  /* fprintf(stderr,"type: %s\n", type_name(type)); */

  xl = (list *)calloc(1, sizeof(list));
  xl->len = s7_list_length(s7, lst);
  xl->type = type;

  if (xl->len > 0)
    {
      int i;
      xl->vals = (xen_value **)calloc(xl->len, sizeof(xen_value *));
      for (i = 0; i < xl->len; i++)
	{
	  s7_pointer val;
	  val = s7_list_ref(s7, lst, i);
	  /* fprintf(stderr, "load %s at %d\n", s7_object_to_c_string(s7, val), i); */

	  if (s7_is_list(s7, val))
	    {
	      xl->len = i;
	      /* fprintf(stderr, "reset len to %d\n", i); */
	      break;
	    }

	  if (CLM_STRUCT_P(type))
	    {
	      int local_type;
	      if (i == 0)
		local_type = R_SYMBOL;
	      else local_type = def_clm_struct_field_type(type, i);
	      if ((val != scheme_false) ||
		  (local_type == R_BOOL))
		xl->vals[i] = add_value_to_ptree(pt, val, local_type);
	      else xl->vals[i] = add_empty_var_to_ptree(pt, local_type);
	    }
	  else
	    {
	      if ((val != scheme_false) ||
		  (type == R_BOOL))
		xl->vals[i] = add_value_to_ptree(pt, val, type);
	      else xl->vals[i] = add_empty_var_to_ptree(pt, type);
	    }
	}
    }

  /* fprintf(stderr,"return list "); */

  return(xl);
}


static int mus_run_xen_to_run_type(s7_pointer val);

static list *xen_to_list(ptree *pt, s7_pointer lst)
{
  /* load xen original into its list shadow */
  /* if first element of list is a clm-struct ref, use those types, else make sure all types are the same */

  int i, len, type = R_UNSPECIFIED; /* possible null list */

  len = s7_list_length(s7, lst);

  /* fprintf(stderr,"got list: %s %d\n", s7_object_to_c_string(s7, lst), len);  */

  if (len > 0)
    {
      if ((s7_is_symbol(s7_car(lst))) &&
	  (!(s7_is_keyword(s7_car(lst))))) /* keywords are symbols in s7 */
	{
	  type = name_to_type(s7_symbol_name(s7_car(lst)));

	  if (type == R_UNSPECIFIED)
	    type = R_SYMBOL;
	}
      else type = mus_run_xen_to_run_type(s7_car(lst));

      if ((type == R_UNSPECIFIED) ||
	  (type == R_LIST))
	{
	  /* fprintf(stderr, "null because %s\n", type_name(type)); */
	  return(NULL);
	}

      if (!(CLM_STRUCT_P(type)))
	{
	  for (i = 1; i < len; i++)
	    if (type != mus_run_xen_to_run_type(s7_list_ref(s7, lst, i)))
	      return(NULL); /* we have to be able to predict each element type */
	}
    }

  return(xen_to_list_with_type(pt, lst, type));
}


static void list_vect_into_vector(ptree *prog, vect *v, s7_pointer vectr)
{
  int len, i;
  len = s7_vector_length(vectr);
  for (i = 0; i < len; i++) 
    list_into_list(prog, v->data.lists[i], s7_vector_ref(s7, vectr, i));
}


static void int_vect_into_vector(vect *v, s7_pointer vectr)
{
  int len, i;
  len = s7_vector_length(vectr);
  for (i = 0; i < len; i++) 
    s7_vector_set(s7, vectr, i, s7_make_integer(s7, v->data.ints[i]));
}


static void free_vect(vect *v, int type)
{
  if (v)
    {
      switch (type)
	{
	case R_INT_VECTOR:        if (v->data.ints) free(v->data.ints); break;
	case R_CLM_VECTOR:        if (v->data.gens) free(v->data.gens); break;
	case R_VCT_VECTOR:        if (v->data.vcts) free(v->data.vcts); break;
	case R_LIST_VECTOR:       if (v->data.lists) free(v->data.lists); break;
	}
      free(v);
    }
}


static vct *vector_to_vct(s7_pointer vectr)
{
  int len, i;
  vct *v;
  len = s7_vector_length(vectr);
  if (len == 0) return(NULL);
  v = mus_vct_make(len);
  for (i = 0; i < len; i++) 
    if (s7_is_real(s7_vector_ref(s7, vectr, i)))
      v->data[i] = (mus_float_t)s7_number_to_real(s7_vector_ref(s7, vectr, i));
    else return(mus_vct_free(v));
  return(v);
}


static void vct_into_vector(vct *v, s7_pointer vectr)
{
  int len, i;
  len = s7_vector_length(vectr);
  for (i = 0; i < len; i++) 
    s7_vector_set(s7, vectr, i, s7_make_real(s7, v->data[i]));
}


static xen_var *free_xen_var(ptree *prog, xen_var *var)
{
  s7_pointer val;

  if (var)
    {
      /* fprintf(stderr, "free var %s %s (global: %d, unclean: %d, opt: %d)\n", var->name, type_name(var->v->type), var->global, var->unclean, optimizing); */

      /* if var->global, reflect new value into outer level version of the variable upon quit */
      if ((var->global) &&
	  (var->unclean))
	{
	  if (!optimizing)
	    {
	      switch (var->v->type)
		{
		case R_FLOAT:  xen_symbol_name_set_value(var->name, s7_make_real(s7, prog->dbls[var->v->addr]));              break;
		case R_INT:    xen_symbol_name_set_value(var->name, s7_make_integer(s7, prog->ints[var->v->addr]));           break;
		case R_BOOL:   xen_symbol_name_set_value(var->name, s7_make_boolean(s7, prog->ints[var->v->addr]));           break;
		case R_STRING: xen_symbol_name_set_value(var->name, scheme_make_string(prog->strs[var->v->addr]));            break;
		case R_CHAR:   xen_symbol_name_set_value(var->name, s7_make_character(s7, (char)(prog->ints[var->v->addr]))); break;

		case R_KEYWORD:
		case R_SYMBOL: xen_symbol_name_set_value(var->name, prog->xens[var->v->addr]);                                break;
		  
		case R_FLOAT_VECTOR:
		  val = symbol_to_value(prog->code, s7_make_symbol(s7, var->name));
		  if (s7_is_vector(val))
		    vct_into_vector(prog->vcts[var->v->addr], val);
		  break;
		  
		case R_INT_VECTOR:
		  val = symbol_to_value(prog->code, s7_make_symbol(s7, var->name));
		  if (s7_is_vector(val))
		    int_vect_into_vector(prog->vects[var->v->addr], val);
		  break;
		  
		default:
		  /* if no global set, surely we shouldn't do anything? */
		  break;
	      }
	    }
	  else
	    {
	      switch (var->v->type)
		{
		case R_FLOAT:  symbol_set_value(prog->code, s7_make_symbol(s7, var->name), s7_make_real(s7, prog->dbls[var->v->addr]));              break;
		case R_INT:    symbol_set_value(prog->code, s7_make_symbol(s7, var->name), s7_make_integer(s7, prog->ints[var->v->addr]));           break;
		case R_BOOL:   symbol_set_value(prog->code, s7_make_symbol(s7, var->name), s7_make_boolean(s7, prog->ints[var->v->addr]));           break;
		case R_STRING: symbol_set_value(prog->code, s7_make_symbol(s7, var->name), scheme_make_string(prog->strs[var->v->addr]));            break;
		case R_CHAR:   symbol_set_value(prog->code, s7_make_symbol(s7, var->name), s7_make_character(s7, (char)(prog->ints[var->v->addr]))); break;

		case R_KEYWORD:
		case R_SYMBOL: symbol_set_value(prog->code, s7_make_symbol(s7, var->name), prog->xens[var->v->addr]);                                break;
		  
		case R_FLOAT_VECTOR:
		  val = symbol_to_value(prog->code, s7_make_symbol(s7, var->name));
		  if (s7_is_vector(val))
		    vct_into_vector(prog->vcts[var->v->addr], val);
		  break;
		  
		case R_INT_VECTOR:
		  val = symbol_to_value(prog->code, s7_make_symbol(s7, var->name));
		  if (s7_is_vector(val))
		    int_vect_into_vector(prog->vects[var->v->addr], val);
		  break;

		  /* not clm_vector because it is already accessing the generators directly */

		case R_LIST_VECTOR:
		  /* not yet functional because the unclean flag is not set (we're setting elements of lists within the vector) */
		  val = symbol_to_value(prog->code, s7_make_symbol(s7, var->name));
		  if (s7_is_vector(val))
		    list_vect_into_vector(prog, prog->vects[var->v->addr], val);
		  break;
		  
		default:
		  if ((var->v->type == R_LIST) ||
		      (CLM_STRUCT_P(var->v->type)))
		    {
		      val = symbol_to_value(prog->code, s7_make_symbol(s7, var->name));
		      if (s7_is_list(s7, val))
			list_into_list(prog, prog->lists[var->v->addr], val);
		    }
		  break;
		  
		}
	    }
	}
      if (var->v) free(var->v);
      free(var);
    }
  return(NULL);
}


static ptree *free_embedded_ptree(ptree *pt)
{
  if (pt)
    {
      if (pt->program)
	{
	  int i;
	  for (i = 0; i < pt->program_size; i++)
	    if (pt->program[i])
	      ((triple **)(pt->program))[i] = free_triple(((triple **)(pt->program))[i]);
	  free(pt->program);
	}
      if (pt->args) free(pt->args);
      if (pt->default_args) free(pt->default_args);
      if (pt->arg_types) free(pt->arg_types);
      if (pt->result) free(pt->result);
      free(pt);
    }
  return(NULL);
}


static void mus_run_free_ptree(struct ptree *pt)
{
  if (pt)
    {
      int i;

      /* free_xen_var depends (in vector case) on finding current (unfreed) data */
      if (pt->form != scheme_undefined) s7_gc_unprotect_at(s7, pt->form_loc);
      if (pt->vars)
	{
	  for (i = 0; i < pt->var_ctr; i++)
	    free_xen_var(pt, pt->vars[i]);
	  free(pt->vars);
	}
      if (pt->global_vars)
	{
	  for (i = 0; i < pt->global_var_ctr; i++)
	    free_xen_var(pt, pt->global_vars[i]);
	  free(pt->global_vars);
	}
      /* now it's safe to free stuff */
      if (pt->gc_ctr > 0)
	{
	  /* if we allocated it, we free it */
	  for (i = 0; i < pt->gc_ctr; i++)
	    {
	      xen_value *v;
	      v = pt->gcs[i];
	      if (v)
		{
		  if (v->gc)
		    {
		      switch (v->type)
			{
			case R_FLOAT_VECTOR:
			case R_VCT: 
			  if (pt->vcts[v->addr])
			    {
			      int k;
			      for (k = 0; k < pt->vct_ctr; k++)
				if ((k != v->addr) && (pt->vcts[k] == pt->vcts[v->addr]))
				  pt->vcts[k] = NULL;
			      pt->vcts[v->addr] = mus_vct_free(pt->vcts[v->addr]); 
			    }
			  break;

			case R_SOUND_DATA: 
			  if (pt->sds[v->addr])
			    {
			      int k;
			      for (k = 0; k < pt->sd_ctr; k++)
				if ((k != v->addr) && (pt->sds[k] == pt->sds[v->addr]))
				  pt->sds[k] = NULL;
			      sound_data_free(pt->sds[v->addr]); 
			      pt->sds[v->addr] = NULL;   
			    }
			  break;
#if USE_SND
			case R_SAMPLER: 
			  if (pt->samplers[v->addr])
			    {
			      int k;
			      for (k = 0; k < pt->sampler_ctr; k++)
				if ((k != v->addr) && (pt->samplers[k] == pt->samplers[v->addr]))
				  pt->samplers[k] = NULL;
			      pt->samplers[v->addr] = free_snd_fd(pt->samplers[v->addr]); 
			    }
			  break;
#endif
			case R_FUNCTION:     
			  if (pt->fncs[v->addr])
			    {
			      int k;
			      for (k = 0; k < pt->fnc_ctr; k++)
				if ((k != v->addr) && (pt->fncs[k] == pt->fncs[v->addr]))
				  pt->fncs[k] = NULL;
			      free_embedded_ptree(((ptree **)(pt->fncs))[v->addr]); 
			      pt->fncs[v->addr] = NULL;   
			    }
			  break;

			case R_CLM:
			  /* this currently can't happen -- all locally created gens are handled via xen */
#if 0
			  if (pt->clms[v->addr])
			    {
			      int k;
			      for (k = 0; k < pt->clm_ctr; k++)
				if ((k != v->addr) && (pt->clms[k] == pt->clms[v->addr]))
				  pt->clms[k] = NULL;
			      mus_free(pt->clms[v->addr]); 
			      pt->clms[v->addr] = NULL;   
			    }
#endif
			  break;

			case R_LIST:
			  if (pt->lists[v->addr]) 
			    {
			      int k;
			      for (k = 0; k < pt->list_ctr; k++)
				if ((k != v->addr) && (pt->lists[k] == pt->lists[v->addr]))
				  pt->lists[k] = NULL;
			      free_list(pt->lists[v->addr]); 
			      pt->lists[v->addr] = NULL;   
			    }
			  break;

			case R_LIST_VECTOR:
			  if (pt->vects[v->addr]) 
			    {
			      int k;
			      vect *vt;
			      vt = pt->vects[v->addr];
			      for (k = 0; k < pt->vect_ctr; k++)
				if ((k != v->addr) && 
				    (pt->vects[k] == vt))
				  pt->vects[k] = NULL;
			      for (k = 0; k < vt->length; k++)
				if (vt->data.lists[k])
				  free_list(vt->data.lists[k]);
			      free_vect(vt, v->type); 
			      pt->vects[v->addr] = NULL;   
			    }
			  break;

			case R_INT_VECTOR:   
			case R_CLM_VECTOR:   
			case R_VCT_VECTOR:   
			  if (pt->vects[v->addr]) 
			    {
			      int k;
			      for (k = 0; k < pt->vect_ctr; k++)
				if ((k != v->addr) && (pt->vects[k] == pt->vects[v->addr]))
				  pt->vects[k] = NULL;
			      free_vect(pt->vects[v->addr], v->type); 
			      pt->vects[v->addr] = NULL;   
			    }
			  break;

			case R_STRING:
			  /* I don' think this currently can happen */
			  if (pt->strs[v->addr]) 
			    {
			      free(pt->strs[v->addr]);
			      pt->strs[v->addr] = NULL;
			    }
			  break;
			  
			default:
			  if (CLM_STRUCT_P(v->type))
			    {
			      if (pt->lists[v->addr]) 
				{
				  int k;
				  for (k = 0; k < pt->list_ctr; k++)
				    if ((k != v->addr) && (pt->lists[k] == pt->lists[v->addr]))
				      pt->lists[k] = NULL;
				  free_list(pt->lists[v->addr]); 
				  pt->lists[v->addr] = NULL;   
				}
			    }
			  break;

			}
		      v->gc = false;
		    }
		  free(v);
		  pt->gcs[i] = NULL;
		}
	    }
	}
      if (pt->strs)
	{
	  for (i = 0; i < pt->str_ctr; i++)
	    if (pt->strs[i])
	      {
		free(pt->strs[i]);
		pt->strs[i] = NULL;
	      }
	  free(pt->strs);
	  pt->strs = NULL;
	}
      if (pt->xen_vars)
	{
	  for (i = 0; i < pt->xen_var_ctr; i++)
	    if (pt->xen_vars[i])
	      {
		free(pt->xen_vars[i]); /* free the containing array */
		pt->xen_vars[i] = NULL;
	      }
	  free(pt->xen_vars);
	}
      if (pt->vcts) 
	{
	  free(pt->vcts);
	  pt->vcts = NULL;
	}
      if (pt->sds) 
	{
	  free(pt->sds);
	  pt->sds = NULL;
	}
      if (pt->clms) 
	{
	  free(pt->clms);
	  pt->clms = NULL;
	}
      if (pt->clm_locs)
	{
	  free(pt->clm_locs);
	  pt->clm_locs = NULL;
	}
      if (pt->vects) 
	{
	  free(pt->vects);
	  pt->vects = NULL;
	}
      if (pt->lists) 
	{
	  free(pt->lists);
	  pt->lists = NULL;
	}
      if (pt->fncs) 
	{
	  free(pt->fncs);
	  pt->fncs = NULL;
	}
      if (pt->xens) 
	{
	  free(pt->xens);
	  pt->xens = NULL;
	}
#if USE_SND
      if (pt->samplers) 
	{
	  free(pt->samplers);
	  pt->samplers = NULL;
	}
#endif
      if (pt->gc_protected)
	{
	  for (i = 0; i < pt->gc_protected_ctr; i++)
	    s7_gc_unprotect_at(s7, pt->gc_protected[i]);
	  free(pt->gc_protected);
	  pt->gc_protected = NULL;
	}
      if (pt->program)
	{
	  for (i = 0; i < pt->program_size; i++)
	    if (pt->program[i])
	      ((triple **)(pt->program))[i] = free_triple(((triple **)(pt->program))[i]);
	  free(pt->program);
	}
      if (pt->gcs) free(pt->gcs);
      if (pt->args) free(pt->args);
      if (pt->default_args) free(pt->default_args);
      if (pt->arg_types) free(pt->arg_types);
      if (pt->gotos) 
	{
	  for (i = 0; i < pt->gotos_size; i++)
	    if (pt->gotos[i])
	      free_goto(pt->gotos[i]);
	  free(pt->gotos);
	}
      if (pt->result) free(pt->result);
      if (pt->ints) free(pt->ints);
      if (pt->dbls) free(pt->dbls);
      free(pt);
    }
}


static triple *add_triple_to_ptree(ptree *pt, triple *trp)
{
  if (pt->triple_ctr >= pt->program_size)
    {
      if (pt->program_size == 0)
	{
	  pt->program = (triple **)calloc(8, sizeof(triple *));
	  pt->program_size = 8;
	}
      else
	{
	  int i, old_size;
	  old_size = pt->program_size;
	  pt->program_size += 8;
	  pt->program = (triple **)realloc(pt->program, pt->program_size * sizeof(triple *));
	  for (i = old_size; i < pt->program_size; i++) pt->program[i] = NULL;
	}
      pt->pc = pt->program;
      pt->initial_pc = pt->program;
    }
  pt->program[pt->triple_ctr++] = trp;
  return(trp);
}


static int add_int_to_ptree(ptree *pt, Int value)
{
  int cur;
  cur = pt->int_ctr++;
  if (cur >= pt->ints_size)
    {
      pt->ints_size += 8;
      pt->ints = (Int *)realloc(pt->ints, pt->ints_size * sizeof(Int));
    }
  pt->ints[cur] = value;
  return(cur);
}


static int add_dbl_to_ptree(ptree *pt, Double value)
{
  int cur;
  cur = pt->dbl_ctr++;
  if (cur >= pt->dbls_size)
    {
      pt->dbls_size += 8;
      pt->dbls = (Double *)realloc(pt->dbls, pt->dbls_size * sizeof(Double));
    }
  pt->dbls[cur] = value;
  return(cur);
}


static int add_vct_to_ptree(ptree *pt, vct *value)
{
  int cur;
  cur = pt->vct_ctr++;
  if (cur >= pt->vcts_size)
    {
      pt->vcts_size += 8;
      if (pt->vcts)
	{
	  int i;
	  pt->vcts = (vct **)realloc(pt->vcts, pt->vcts_size * sizeof(vct *));
	  for (i = cur; i < pt->vcts_size; i++) pt->vcts[i] = NULL;
	}
      else pt->vcts = (vct **)calloc(pt->vcts_size, sizeof(vct *));
    }
  pt->vcts[cur] = value;
  return(cur);
}


static int add_sound_data_to_ptree(ptree *pt, sound_data *value)
{
  int cur;
  cur = pt->sd_ctr++;
  if (cur >= pt->sds_size)
    {
      pt->sds_size += 8;
      if (pt->sds)
	{
	  int i;
	  pt->sds = (sound_data **)realloc(pt->sds, pt->sds_size * sizeof(sound_data *));
	  for (i = cur; i < pt->sds_size; i++) pt->sds[i] = NULL;
	}
      else pt->sds = (sound_data **)calloc(pt->sds_size, sizeof(sound_data *));
    }
  pt->sds[cur] = value;
  return(cur);
}


static int add_loc_to_protected_list(ptree *pt, int loc);

static int add_clm_to_ptree(ptree *pt, mus_any *value, s7_pointer orig)
{
  int cur;
  cur = pt->clm_ctr++;
  if (cur >= pt->clms_size)
    {
      int i;
      pt->clms_size += 8;
      if (pt->clms)
	{
	  pt->clms = (mus_any **)realloc(pt->clms, pt->clms_size * sizeof(mus_any *));
	  pt->clm_locs = (int *)realloc(pt->clm_locs, pt->clms_size * sizeof(int));
	  for (i = cur; i < pt->clms_size; i++) {pt->clms[i] = NULL; pt->clm_locs[i] = -1;}
	}
      else 
	{
	  pt->clms = (mus_any **)calloc(pt->clms_size, sizeof(mus_any *));
	  pt->clm_locs = (int *)calloc(pt->clms_size, sizeof(int));
	  for (i = 0; i < pt->clms_size; i++) pt->clm_locs[i] = -1;
	}
    }
  pt->clms[cur] = value;
  if (mus_xen_p(orig))
    {
      if (pt->clm_locs[cur] >= 0) s7_gc_unprotect_at(s7, pt->clm_locs[cur]);
      pt->clm_locs[cur] = add_loc_to_protected_list(pt, s7_gc_protect(s7, orig));
    }
  return(cur);
}


static int add_list_to_ptree(ptree *pt, list *value)
{
  int cur;

  cur = pt->list_ctr++;

  /* fprintf(stderr, "add list %p at %d\n", value, cur); */

  if (cur >= pt->lists_size)
    {
      pt->lists_size += 8;
      if (pt->lists)
	{
	  int i;
	  pt->lists = (list **)realloc(pt->lists, pt->lists_size * sizeof(list *));
	  for (i = cur; i < pt->lists_size; i++) pt->lists[i] = NULL;
	}
      else pt->lists = (list **)calloc(pt->lists_size, sizeof(list *));
    }
  pt->lists[cur] = value;
  return(cur);
}


static int add_vect_to_ptree(ptree *pt, vect *value)
{
  int cur;
  cur = pt->vect_ctr++;
  if (cur >= pt->vects_size)
    {
      pt->vects_size += 8;
      if (pt->vects)
	{
	  int i;
	  pt->vects = (vect **)realloc(pt->vects, pt->vects_size * sizeof(vect *));
	  for (i = cur; i < pt->vects_size; i++) pt->vects[i] = NULL;
	}
      else pt->vects = (vect **)calloc(pt->vects_size, sizeof(vect *));
    }
  pt->vects[cur] = value;
  return(cur);
}


static int add_fnc_to_ptree(ptree *pt, ptree *value)
{
  int cur;
  cur = pt->fnc_ctr++;
  if (cur >= pt->fncs_size)
    {
      pt->fncs_size += 8;
      if (pt->fncs)
	{
	  int i;
	  pt->fncs = (struct ptree **)realloc(pt->fncs, pt->fncs_size * sizeof(ptree *));
	  for (i = cur; i < pt->fncs_size; i++) pt->fncs[i] = NULL;
	}
      else pt->fncs = (struct ptree **)calloc(pt->fncs_size, sizeof(ptree *));
    }
  ((ptree **)(pt->fncs))[cur] = value;
  return(cur);
}


static int add_xen_to_ptree(ptree *pt, s7_pointer value)
{
  int cur;
  cur = pt->xen_ctr++;
  if (cur >= pt->xens_size)
    {
      int i;
      pt->xens_size += 8;
      if (pt->xens)
	pt->xens = (s7_pointer *)realloc(pt->xens, pt->xens_size * sizeof(s7_pointer));
      else pt->xens = (s7_pointer *)calloc(pt->xens_size, sizeof(s7_pointer));
      for (i = cur; i < pt->xens_size; i++) pt->xens[i] = scheme_undefined;
    }
  pt->xens[cur] = value;
  return(cur);
}

#if USE_SND
static int add_sampler_to_ptree(ptree *pt, snd_fd *value)
{
  int cur;
  cur = pt->sampler_ctr++;
  if (cur >= pt->samplers_size)
    {
      pt->samplers_size += 8;
      if (pt->samplers)
	{
	  int i;
	  pt->samplers = (snd_fd **)realloc(pt->samplers, pt->samplers_size * sizeof(snd_fd *));
	  for (i = cur; i < pt->samplers_size; i++) pt->samplers[i] = NULL;
	}
      else pt->samplers = (snd_fd **)calloc(pt->samplers_size, sizeof(snd_fd *));
    }
  pt->samplers[cur] = value;
  return(cur);
}
#endif


static xen_var *new_xen_var(const char *name, xen_value *v)
{
  xen_var *var;
  var = (xen_var *)calloc(1, sizeof(xen_var));
  var->name = name; /* this s7_symbol_name isn't going to be deallocated during a run */
  var->v = copy_xen_value(v);
  var->unclean = false;
  var->global = false;
  var->unsettable = false;
  return(var);
}


static void add_var_to_ptree(ptree *pt, const char *name, xen_value *v)
{
  int cur;
  cur = pt->var_ctr;
  if (cur >= pt->vars_size)
    {
      pt->vars_size += 8;
      if (pt->vars)
	{
	  int i;
	  pt->vars = (xen_var **)realloc(pt->vars, pt->vars_size * sizeof(xen_var *));
	  for (i = cur; i < pt->vars_size; i++) pt->vars[i] = NULL;
	}
      else pt->vars = (xen_var **)calloc(pt->vars_size, sizeof(xen_var *));
    }
  pt->vars[pt->var_ctr++] = new_xen_var(name, v);
}


static int add_outer_var_to_ptree(ptree *pt, const char *name, xen_value *v)
{
  int cur;
  xen_var *var;
  cur = pt->global_var_ctr++;
  if (cur >= pt->global_vars_size)
    {
      pt->global_vars_size += 8;
      if (pt->global_vars)
	{
	  int i;
	  pt->global_vars = (xen_var **)realloc(pt->global_vars, pt->global_vars_size * sizeof(xen_var *));
	  for (i = cur; i < pt->global_vars_size; i++) pt->global_vars[i] = NULL;
	}
      else pt->global_vars = (xen_var **)calloc(pt->global_vars_size, sizeof(xen_var *));
    }
  var = new_xen_var(name, v);
  var->global = true;
  pt->global_vars[cur] = var;
  return(cur);
}


static int add_string_to_ptree(ptree *pt, char *str)
{
  int cur;
  cur = pt->str_ctr++;
  if (cur >= pt->strs_size)
    {
      pt->strs_size += 8;
      if (pt->strs)
	{
	  int i;
	  pt->strs = (char **)realloc(pt->strs, pt->strs_size * sizeof(char *));
	  for (i = cur; i < pt->strs_size; i++)
	    pt->strs[i] = NULL;
	}
      else pt->strs = (char **)calloc(pt->strs_size, sizeof(char *));
    }
  pt->strs[cur] = str;
  return(cur);
}


static xen_value *add_some_var_to_ptree(ptree *prog, int type, xen_value_constant_t ctype)
{
  switch (type)
    {
    case R_BOOL:         return(make_xen_value(type, add_int_to_ptree(prog, false), ctype));           break;
    case R_FLOAT:        return(make_xen_value(type, add_dbl_to_ptree(prog, 0.0), ctype));             break;
    case R_STRING:       return(make_xen_value(type, add_string_to_ptree(prog, NULL), ctype));         break;
    case R_CLM:          return(make_xen_value(type, add_clm_to_ptree(prog, NULL, scheme_false), ctype)); break;
    case R_FUNCTION:     return(make_xen_value(type, add_fnc_to_ptree(prog, NULL), ctype));            break;
#if USE_SND
    case R_SAMPLER:      return(make_xen_value(type, add_sampler_to_ptree(prog, NULL), ctype));        break;
    case R_SOUND:
    case R_MIX:
    case R_MARK:
    case R_REGION:       return(make_xen_value(type, add_int_to_ptree(prog, 0), ctype));               break;
#endif
    case R_SOUND_DATA:   return(make_xen_value(type, add_sound_data_to_ptree(prog, NULL), ctype));     break;
    case R_FLOAT_VECTOR:
    case R_VCT:          return(make_xen_value(type, add_vct_to_ptree(prog, NULL), ctype));            break;
    case R_SYMBOL: 
    case R_KEYWORD:      return(make_xen_value(type, add_xen_to_ptree(prog, scheme_false), ctype));       break;
    case R_LIST_VECTOR:
    case R_CLM_VECTOR:
    case R_INT_VECTOR:
    case R_VCT_VECTOR:   return(make_xen_value(type, add_vect_to_ptree(prog, NULL), ctype));           break;
    default:
      if ((type == R_LIST) ||
	  (CLM_STRUCT_P(type)))   /* this can happen if we're declaring a function (or outer lambda) arg */
	return(make_xen_value(type, add_list_to_ptree(prog, NULL), ctype));
      break;
    }
  return(make_xen_value(type, add_int_to_ptree(prog, 0), ctype)); 
}


static xen_value *add_empty_var_to_ptree(ptree *prog, int type)
{
  return(add_some_var_to_ptree(prog, type, R_VARIABLE));
}


static xen_value *add_temporary_var_to_ptree(ptree *prog, int type)
{
  return(add_some_var_to_ptree(prog, type, R_TEMPORARY));
}


static xen_value *transfer_value(ptree *prog, xen_value *v)
{
  /* if ((v->type == R_LIST) || (CLM_STRUCT_P(v->type))) fprintf(stderr,"transfer %s\n", type_name(v->type)); */

  switch (v->type)
    {
    case R_FLOAT: 
      return(make_xen_value(v->type, add_dbl_to_ptree(prog, prog->dbls[v->addr]), R_VARIABLE)); 
      break;

    case R_STRING: 
      return(make_xen_value(v->type, add_string_to_ptree(prog, mus_strdup(prog->strs[v->addr])), R_VARIABLE)); 
      break;

    case R_INT:
    case R_BOOL:
    case R_CHAR:
    case R_GOTO:
      return(make_xen_value(v->type, add_int_to_ptree(prog, prog->ints[v->addr]), R_VARIABLE));      
      break;

    default:
      return(make_xen_value(v->type, v->addr, R_VARIABLE)); 
      break;
    }
  /* can't get here? */
  return(make_xen_value(v->type, add_int_to_ptree(prog, prog->ints[v->addr]), R_VARIABLE));
}


static void add_xen_value_to_gcs(ptree *pt, xen_value *v)
{
  if (pt->gc_ctr >= pt->gcs_size)
    {
      int old_size;
      old_size = pt->gcs_size;
      pt->gcs_size += 4;
      if (old_size == 0)
	pt->gcs = (xen_value **)calloc(pt->gcs_size, sizeof(xen_value *));
      else
	{
	  int i;
	  pt->gcs = (xen_value **)realloc(pt->gcs, pt->gcs_size * sizeof(xen_value *));
	  for (i = old_size; i < pt->gcs_size; i++) pt->gcs[i] = NULL;
	}
    }
  pt->gcs[pt->gc_ctr++] = v;
}


static void add_obj_to_gcs(ptree *pt, int type, int addr)
{
  xen_value *v;
  v = make_xen_value(type, addr, R_VARIABLE);
  v->gc = true;
  add_xen_value_to_gcs(pt, v);
}


static int add_loc_to_protected_list(ptree *pt, int loc)
{
  /* here the protected entities (clm gens and fft windows created within run) are created at eval time.
   *   since we don't have an explicit list of such trees in the outer tree, we need to put the
   *   protected loc in the outer-most tree.
   */
  int i;
  if (loc == NOT_A_GC_LOC) return(-1);
  while (pt->outer_tree) pt = pt->outer_tree;
  if (pt->gc_protected_ctr >= pt->gc_protected_size)
    {
      int old_size;
      old_size = pt->gc_protected_size;
      pt->gc_protected_size += 8;
      if (old_size == 0)
	pt->gc_protected = (int *)calloc(pt->gc_protected_size, sizeof(int));
      else pt->gc_protected = (int *)realloc(pt->gc_protected, pt->gc_protected_size * sizeof(int));
      for (i = old_size; i < pt->gc_protected_size; i++) pt->gc_protected[i] = NOT_A_GC_LOC;
    }
  pt->gc_protected[pt->gc_protected_ctr++] = loc;
  return(loc);
}


static vect *read_int_vector(s7_pointer vectr)
{
  int len, i;
  vect *v;
  len = s7_vector_length(vectr);
  if (len == 0) return(NULL);
  v = (vect *)calloc(1, sizeof(vect));
  v->length = len;
  v->data.ints = (Int *)calloc(len, sizeof(Int));
  for (i = 0; i < len; i++) 
    {
      s7_pointer datum;
      datum = s7_vector_ref(s7, vectr, i);
      if (s7_is_integer(datum))
	v->data.ints[i] = s7_number_to_integer(datum);
      else
	{
	  char *temp = NULL;
	  run_warn("initial int vector value at %d is %s: will try to abort ptree evaluation...", i, temp = s7_object_to_c_string(s7, datum));
	  if (temp) free(temp);
	  free(v->data.ints);
	  free(v);
	  return(NULL);
	}
    }
  return(v);
}


static vect *read_vct_vector(s7_pointer vectr)
{
  int len, i;
  vect *v;
  len = s7_vector_length(vectr);
  if (len == 0) return(NULL);
  v = (vect *)calloc(1, sizeof(vect));
  v->length = len;
  v->data.vcts = (vct **)calloc(len, sizeof(vct *));
  for (i = 0; i < len; i++) 
    {
      s7_pointer datum;
      datum = s7_vector_ref(s7, vectr, i);
      if (MUS_VCT_P(datum))
	v->data.vcts[i] = xen_to_vct(datum);
      else
	{
	  char *temp = NULL;
	  run_warn("initial vct vector value at %d is %s: will try to abort ptree evaluation...", i, temp = s7_object_to_c_string(s7, datum));
	  if (temp) free(temp);
	  free(v->data.vcts);
	  free(v);
	  return(NULL);
	}
    }
  return(v);
}


static vect *read_clm_vector(s7_pointer vectr)
{
  int len, i;
  vect *v;
  len = s7_vector_length(vectr);
  if (len == 0) return(NULL);
  v = (vect *)calloc(1, sizeof(vect));
  v->length = len;
  v->data.gens = (mus_any **)calloc(len, sizeof(mus_any *));
  for (i = 0; i < len; i++) 
    {
      s7_pointer datum;
      datum = s7_vector_ref(s7, vectr, i);
      if (mus_xen_p(datum))
	v->data.gens[i] = XEN_TO_MUS_ANY(datum);
      else
	{
	  if (datum != scheme_false)
	    {
	      char *temp = NULL;
	      run_warn("initial clm vector value at %d is %s: will try to abort ptree evaluation...", i, temp = s7_object_to_c_string(s7, datum));
	      if (temp) free(temp);
	      free(v->data.gens);
	      free(v);
	      return(NULL);
	    }
	}
    }
  return(v);
}


static vect *read_list_vector(ptree *pt, s7_pointer vectr)
{
  int len, i;
  vect *v;
  len = s7_vector_length(vectr);
  if (len == 0) return(NULL);
  v = (vect *)calloc(1, sizeof(vect));
  v->length = len;
  v->data.lists = (list **)calloc(len, sizeof(list *));
  for (i = 0; i < len; i++) 
    {
      s7_pointer datum;
      datum = s7_vector_ref(s7, vectr, i);
      if (s7_is_list(s7, datum))
	{
	  v->data.lists[i] = xen_to_list(pt, datum);
	  add_list_to_ptree(pt, v->data.lists[i]);
	}
      else
	{
	  char *temp = NULL;
	  run_warn("initial list vector value at %d is %s: will try to abort ptree evaluation...", i, temp = s7_object_to_c_string(s7, datum));
	  if (temp) free(temp);
	  free(v->data.lists);
	  free(v);
	  return(NULL);
	}
    }
  return(v);
}


static vect *read_vector(ptree *pt, s7_pointer vector, int type)
{
  switch (type)
    {
    case R_LIST_VECTOR: return(read_list_vector(pt, vector)); break;
    case R_INT_VECTOR:  return(read_int_vector(vector));      break;
    case R_CLM_VECTOR:  return(read_clm_vector(vector));      break;
    case R_VCT_VECTOR:  return(read_vct_vector(vector));      break;
    }
  return(NULL);
}


static int mus_run_xen_to_run_type(s7_pointer val)
{
  /* fprintf(stderr, "get type of %s\n", s7_object_to_c_string(s7, val)); */

  if (s7_is_real(val))
    {
      if ((s7_is_exact(val)) && (s7_is_integer(val)))
	return(R_INT);
      return(R_FLOAT);
    }

  if (s7_is_boolean(val)) return(R_BOOL);
  if (MUS_VCT_P(val)) return(R_VCT);
  if (sound_data_p(val)) return(R_SOUND_DATA);
  if (mus_xen_p(val)) return(R_CLM); 
  if (s7_is_character(val)) return(R_CHAR); 
  if (s7_is_string(val)) return(R_STRING);
  if (s7_is_keyword(val)) return(R_KEYWORD); 
  if (s7_is_symbol(val)) return(R_SYMBOL);

  if (s7_is_list(s7, val)) 
    {
      if ((val != scheme_nil) &&
	  (s7_is_symbol(s7_car(val))))
	{
	  int type;
	  type = name_to_type(s7_symbol_name(s7_car(val)));
	  if (CLM_STRUCT_P(type))
	    return(type); /* might be a list of symbols?? */
	}
      if (s7_list_length(s7, val) >= 0) /* no dotted lists here */
	return(R_LIST); 
    }

  if (s7_is_vector(val))
    {
      s7_pointer val0;
      val0 = s7_vector_ref(s7, val, 0);
      if (s7_is_real(val0))
	{
	  if (s7_is_exact(val0))
	    return(R_INT_VECTOR);
	  return(R_FLOAT_VECTOR);
	}
      if (MUS_VCT_P(val0)) return(R_VCT_VECTOR); 
      if ((mus_xen_p(val0)) || (val0 == scheme_false)) return(R_CLM_VECTOR);
      if (s7_is_list(s7, val0)) return(R_LIST_VECTOR);
    }

  if ((s7_is_output_port(s7, val)) || (s7_is_input_port(s7, val))) /* format arg? */
    return(R_XEN);

#if USE_SND
  if (sampler_p(val)) return(R_SAMPLER); 
  if (xen_sound_p(val)) return(R_SOUND);
  if (xen_region_p(val)) return(R_REGION);
  if (xen_mix_p(val)) return(R_MIX);
  if (xen_mark_p(val)) return(R_MARK);
#endif

  return(R_UNSPECIFIED);
}


static xen_value *add_value_to_ptree(ptree *prog, s7_pointer val, int type)
{
  xen_value *v = NULL;
  /* fprintf(stderr, "add %s as %s\n", s7_object_to_c_string(s7, val), type_name(type)); */

  switch (type)
    {
    case R_INT:        v = make_xen_value(R_INT, add_int_to_ptree(prog, s7_number_to_integer(val)), R_VARIABLE);                             break;
    case R_FLOAT:      v = make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, s7_number_to_real(val)), R_VARIABLE);                          break;
    case R_BOOL:       v = make_xen_value(R_BOOL, add_int_to_ptree(prog, (Int)scheme_to_c_bool(val)), R_VARIABLE);                     break;
    case R_VCT:        v = make_xen_value(R_VCT, add_vct_to_ptree(prog, xen_to_vct(val)), R_VARIABLE);                                 break;
    case R_SOUND_DATA: v = make_xen_value(R_SOUND_DATA, add_sound_data_to_ptree(prog, XEN_TO_SOUND_DATA(val)), R_VARIABLE);            break;
#if USE_SND
    case R_SAMPLER:    v = make_xen_value(R_SAMPLER, add_sampler_to_ptree(prog, xen_to_sampler(val)), R_VARIABLE);                     break;
    case R_MIX:        v = make_xen_value(R_MIX, add_int_to_ptree(prog, XEN_MIX_TO_C_INT(val)), R_VARIABLE);                           break;
    case R_MARK:       v = make_xen_value(R_MARK, add_int_to_ptree(prog, XEN_MARK_TO_C_INT(val)), R_VARIABLE);                         break;
    case R_REGION:     v = make_xen_value(R_REGION, add_int_to_ptree(prog, XEN_REGION_TO_C_INT(val)), R_VARIABLE);                     break;
    case R_SOUND:      v = make_xen_value(R_SOUND, add_int_to_ptree(prog, XEN_SOUND_TO_C_INT(val)), R_VARIABLE);                       break;
#endif
    case R_CHAR:       v = make_xen_value(R_CHAR, add_int_to_ptree(prog, (Int)(s7_character(val))), R_VARIABLE);                      break;
    case R_STRING:     v = make_xen_value(R_STRING, add_string_to_ptree(prog, mus_strdup(s7_string(val))), R_VARIABLE);          break;
    case R_SYMBOL:     v = make_xen_value(R_SYMBOL, add_xen_to_ptree(prog, val), R_VARIABLE);                                          break;
    case R_KEYWORD:    v = make_xen_value(R_KEYWORD, add_xen_to_ptree(prog, val), R_VARIABLE);                                         break;
    case R_CLM:        v = make_xen_value(R_CLM, add_clm_to_ptree(prog, XEN_TO_MUS_ANY(val), val), R_VARIABLE);                        break;

    case R_LIST:    
      {
	list *lst;
	lst = xen_to_list(prog, val);
	/* fprintf(stderr, "add %p\n", lst); */
	if (!lst)
	  {
	    xen_value *v1;
	    v1 = add_empty_var_to_ptree(prog, R_LIST);
	    v = make_xen_value(R_LIST, v1->addr, R_VARIABLE);
	    free(v1);
	  }
	else v = make_xen_value(R_LIST, add_list_to_ptree(prog, lst), R_VARIABLE);
	if (v) add_obj_to_gcs(prog, R_LIST, v->addr);
      }
      break;

    case R_FLOAT_VECTOR:
      {
	vct *vc;
	vc = vector_to_vct(val);
	if (vc == NULL) 
	  return(run_warn("non-number in float vector -- will try to abort ptree evaluation"));
	v = make_xen_value(R_FLOAT_VECTOR, add_vct_to_ptree(prog, vc), R_VARIABLE);
	if (v) add_obj_to_gcs(prog, R_FLOAT_VECTOR, v->addr);
      }
      break;

    case R_LIST_VECTOR:
    case R_VCT_VECTOR:
    case R_CLM_VECTOR:
    case R_INT_VECTOR: 
      {
	vect *iv;
	iv = read_vector(prog, val, type);
	if (iv == NULL) return(NULL);
	v = make_xen_value(type, add_vect_to_ptree(prog, iv), R_VARIABLE);
	if (v) add_obj_to_gcs(prog, type, v->addr); /* this makes its own xen_value with true gc field */
      }
      break;
    default:
      if (CLM_STRUCT_P(type))
	{
	  list *lst;
	  lst = xen_to_list_with_type(prog, val, type);
	  v = make_xen_value(type, add_list_to_ptree(prog, lst), R_VARIABLE);
	  /* fprintf(stderr, "add value to ptree: %s\n", describe_xen_value(v, prog)); */
	  if (v) add_obj_to_gcs(prog, type, v->addr);
	}
      break;
    }

  return(v);
}


static xen_value *add_global_var_to_ptree(ptree *prog, s7_pointer form, s7_pointer *rtn)
{
  s7_pointer val;
  xen_var *var;
  int type;
  xen_value *v = NULL;
  const char *varname;

  varname = s7_symbol_name(form);
  var = find_var_in_ptree(prog, varname);
  if (var) 
    return(copy_xen_value(var->v));

  if (!optimizing) return(NULL);

  val = symbol_to_value(prog->code, form);
  (*rtn) = val;
  if (val == scheme_undefined)
    {
      ptree *upper = NULL;
      upper = prog;
      while ((val == scheme_undefined) && (upper->outer_tree))
	{
	  upper = upper->outer_tree;
	  val = symbol_to_value(upper->code, form);
	}
      if (val == scheme_undefined)	
	return(run_warn("can't find %s", varname));
    }
  type = mus_run_xen_to_run_type(val);
  if (type == R_UNSPECIFIED)
    return(NULL);

  /* fprintf(stderr, "add global %s %s %s\n",varname, type_name(type), s7_object_to_c_string(s7, val)); */
  
  v = add_value_to_ptree(prog, val, type);

  if (v)
    {
      int var_loc;
      var_loc = add_outer_var_to_ptree(prog, varname, v);
      if (!optimizing)
	prog->global_vars[var_loc]->unsettable = true;
    }
  return(v);
}


static continuation *add_goto_to_ptree(ptree *pt, const char *name)
{
  continuation *c;
  c = (continuation *)calloc(1, sizeof(continuation));
  c->name = (char *)calloc(256, sizeof(char));                      /* c->name is used within the call/cc to identify the continuation */
  mus_snprintf(c->name, 256, "%s", name);
  c->jump = make_xen_value(R_GOTO, add_int_to_ptree(pt, 0), R_VARIABLE); /* c->jump->addr will be the continuation PC */
  c->result = NULL;                                                 /* c->result is the returned value address */
  if (pt->gotos_size <= pt->goto_ctr)
    {
      int old_size;
      old_size = pt->gotos_size;
      pt->gotos_size += 4;
      if (old_size == 0)
	pt->gotos = (continuation **)calloc(pt->gotos_size, sizeof(continuation *));
      else
	{
	  int i;
	  pt->gotos = (continuation **)realloc(pt->gotos, pt->gotos_size * sizeof(continuation *));
	  for (i = old_size; i < pt->gotos_size; i++) pt->gotos[i] = NULL;
	}
    }
  c->loc = pt->goto_ctr;
  pt->gotos[pt->goto_ctr++] = c;
  add_var_to_ptree(pt, name, c->jump);
  return(c);
}


static void erase_goto(ptree *prog, const char *name)
{
  int i;
  for (i = prog->goto_ctr - 1; i >= 0; i--)
    {
      continuation *c = NULL;
      c = prog->gotos[i];
      if ((c) && 
	  (mus_strcmp(c->name, name)))
	{
	  free(c->name);
	  c->name = NULL;
	  break;
	}
    }
}

#if WITH_COUNTERS

#define COUNTER_NUM 1024
static int top_counter = 1;
static mus_long_t counts[COUNTER_NUM][COUNTER_NUM];

typedef void (*trip_func)(int *args, ptree *pt);
static trip_func funcs[COUNTER_NUM];
static const char *func_names[COUNTER_NUM];

static void quit(int *args, ptree *pt);

static void clear_counts(void) 
{
  int i, j; 
  for (i = 0; i < COUNTER_NUM; i++) 
    for (j = 0; j < COUNTER_NUM; j++) 
      counts[i][j] = 0;
  func_names[0] = "start";
  funcs[0] = NULL;
}

static int get_func_loc(trip_func func, const char *name)
{
  int i;
  for (i = 0; i < top_counter; i++)
    if (funcs[i] == func)
      return(i);
  func_names[top_counter] = name;
  funcs[top_counter++] = func;
  return(top_counter - 1);
}

static s7_pointer g_report_counts(void)
{
  int i, j, rpt, imax, jmax;
  mus_long_t cmax;
  mus_long_t *totals;

  fprintf(stderr, "most used:\n");
  totals = (mus_long_t *)calloc(top_counter, sizeof(mus_long_t));
  for (i = 0; i < top_counter; i++)
    for (j = 0; j < top_counter; j++)
      totals[j] += counts[i][j];
  for (rpt = 0; rpt < 200; rpt++)
    {
      cmax = 0;
      for (i = 0; i < top_counter; i++)
	if (totals[i] > cmax)
	  {
	    cmax = totals[i];
	    imax = i;
	  }
      if (totals[imax] == 0) break;
      fprintf(stderr, "    %s %lld\n", func_names[imax], totals[imax]);
      totals[imax] = 0;
    }
  free(totals);

  fprintf(stderr, "funcs: %d\n", top_counter - 1);
  for (rpt = 0; rpt < 200; rpt++)
    {
      cmax = 0;
      for (i = 0; i < top_counter; i++) 
	for (j = 0; j < top_counter; j++) 
	  if (counts[i][j] > cmax)
	    {
	      cmax = counts[i][j];
	      imax = i;
	      jmax = j;
	    }
      if (cmax == 0) break;
      if ((imax != 0) &&
	  (funcs[jmax] != quit))
	fprintf(stderr, "%lld: %s\n         %s\n", cmax, func_names[imax], func_names[jmax]);
      counts[imax][jmax] = 0;
    }
  return(scheme_true);
}

static s7_pointer g_clear_counts(void)
{
  clear_counts();
  return(scheme_true);
}
#endif


static void eval_ptree(ptree *pt)
{
  /* evaluates program, result in prog->result, 
   *   assumes args already passed in addrs given by prog->addrs
   */
#if WITH_COUNTERS
  int last_loc = 0, this_loc = 0;
#endif

  pt->all_done = false;
  while (!(pt->all_done))
    {
      triple *cpc;
      cpc = (*pt->pc++);
#if WITH_COUNTERS
      this_loc = cpc->func_loc;
      counts[last_loc][this_loc]++;
      last_loc = this_loc;
#endif
      (*(cpc->function))(cpc->args, pt);
    }
  pt->pc = pt->initial_pc; /* don't reset closure junk after initial evaluation */
}


static void eval_embedded_ptree(ptree *prog, ptree *pt)
{
  triple **old_pc;
  old_pc = pt->pc;

  prog->ints = pt->ints;
  prog->dbls = pt->dbls;
  prog->xen_vars = pt->xen_vars;
  prog->strs = pt->strs;
  prog->vcts = pt->vcts;
  prog->sds = pt->sds;
  prog->clms = pt->clms;
  prog->clm_locs = pt->clm_locs;
  prog->vects = pt->vects;
  prog->lists = pt->lists;
  prog->fncs = pt->fncs;
  prog->xens = pt->xens;
#if USE_SND
  prog->samplers = pt->samplers;
#endif

  eval_ptree(prog);

  pt->pc = old_pc;
  pt->all_done = false;
}


static triple *make_triple(void (*function)(int *arg_addrs, ptree *pt),
			   const char *op_name,
			   xen_value **typed_args, int args)
{
  triple *trp;
  int *addrs = NULL, *types = NULL;
  if (args > 0)
    {
      int i;
      addrs = (int *)calloc(args, sizeof(int));
      types = (int *)calloc(args, sizeof(int));
      for (i = 0; i < args; i++) 
	{
	  addrs[i] = typed_args[i]->addr;
	  types[i] = typed_args[i]->type;
	}
    }
  trp = (triple *)calloc(1, sizeof(triple));
  trp->function = function;
  trp->args = addrs;
  trp->types = types;
  trp->num_args = args;
  trp->op_name = op_name;
#if WITH_COUNTERS
  trp->func_loc = get_func_loc(function, op_name);
#endif
  return(trp);
}


static triple *va_make_triple(void (*function)(int *arg_addrs, ptree *pt), 
			      const char *op_name,
			      int args, ...)
{
  triple *trp;
  int *addrs = NULL, *types = NULL;
  if (args > 0)
    {
      va_list ap;
      int i;
      va_start(ap, args);
      addrs = (int *)calloc(args, sizeof(int));
      types = (int *)calloc(args, sizeof(int));
      for (i = 0; i < args; i++) 
	{
	  xen_value *v;
	  v = va_arg(ap, xen_value *);
	  if (v) 
	    {
	      addrs[i] = v->addr;
	      types[i] = v->type;
	    }
	}
      va_end(ap);
    }
  trp = (triple *)calloc(1, sizeof(triple));
  trp->function = function;
  trp->args = addrs;
  trp->types = types;
  trp->num_args = args;
  trp->op_name = op_name;
#if WITH_COUNTERS
  trp->func_loc = get_func_loc(function, op_name);
#endif
  return(trp);
}


#define BOOL_RESULT pt->ints[args[0]]
#define BOOL_ARG_1 ((bool)(pt->ints[args[1]]))
#define BOOL_ARG_3 ((bool)(pt->ints[args[3]]))

#define INT_RESULT pt->ints[args[0]]
#define INT_ARG_1 pt->ints[args[1]]
#define INT_ARG_2 pt->ints[args[2]]
#define INT_ARG_3 pt->ints[args[3]]
#define INT_ARG_4 pt->ints[args[4]]
#define INT_ARG_5 pt->ints[args[5]]
#define INT_ARG_6 pt->ints[args[6]]
#define INT_ARG_9 pt->ints[args[9]]

#define FLOAT_RESULT pt->dbls[args[0]]
#define FLOAT_ARG_1 pt->dbls[args[1]]
#define FLOAT_ARG_2 pt->dbls[args[2]]
#define FLOAT_ARG_3 pt->dbls[args[3]]
#define FLOAT_ARG_4 pt->dbls[args[4]]
#define FLOAT_ARG_5 pt->dbls[args[5]]
#define FLOAT_ARG_6 pt->dbls[args[6]]
#define FLOAT_ARG_7 pt->dbls[args[7]]
#define FLOAT_ARG_8 pt->dbls[args[8]]
#define FLOAT_ARG_9 pt->dbls[args[9]]

#define VCT_RESULT pt->vcts[args[0]]
#define VCT_ARG_1 pt->vcts[args[1]]
#define VCT_ARG_2 pt->vcts[args[2]]
#define VCT_ARG_3 pt->vcts[args[3]]
#define VCT_ARG_4 pt->vcts[args[4]]

#define SOUND_DATA_RESULT pt->sds[args[0]]
#define SOUND_DATA_ARG_1 pt->sds[args[1]]
#define SOUND_DATA_ARG_2 pt->sds[args[2]]
#define SOUND_DATA_ARG_3 pt->sds[args[3]]
#define SOUND_DATA_ARG_4 pt->sds[args[4]]

#define STRING_RESULT pt->strs[args[0]]
#define STRING_ARG_1 pt->strs[args[1]]
#define STRING_ARG_2 pt->strs[args[2]]
#define STRING_ARG_3 pt->strs[args[3]]

#define CHAR_RESULT pt->ints[args[0]]
#define CHAR_ARG_1 ((char)(INT_ARG_1))
#define CHAR_ARG_2 ((char)(INT_ARG_2))
#define CHAR_ARG_3 ((char)(INT_ARG_3))

#define CLM_RESULT pt->clms[args[0]]
#define CLM_LOC pt->clm_locs[args[0]]
#define CLM_ARG_1 pt->clms[args[1]]
#define CLM_ARG_2 pt->clms[args[2]]
#define CLM_ARG_3 pt->clms[args[3]]
#define CLM_ARG_4 pt->clms[args[4]]
#define CLM_ARG_5 pt->clms[args[5]]
#define CLM_ARG_6 pt->clms[args[6]]
#define CLM_ARG_7 pt->clms[args[7]]
#define CLM_ARG_8 pt->clms[args[8]]

#if USE_SND
#define SAMPLER_RESULT pt->samplers[args[0]]
#define SAMPLER_ARG_1 pt->samplers[args[1]]
#define SAMPLER_ARG_2 pt->samplers[args[2]]
#define SAMPLER_ARG_3 pt->samplers[args[3]]

#define SOUND_ARG_1 pt->ints[args[1]]
#define MIX_ARG_1 pt->ints[args[1]]
#define REGION_ARG_1 pt->ints[args[1]]
#endif

#define FNC_RESULT ((ptree **)(pt->fncs))[args[0]]
#define FNC_ARG_1 ((ptree **)(pt->fncs))[args[1]]
#define FNC_ARG_2 ((ptree **)(pt->fncs))[args[2]]
#define FNC_ARG_3 ((ptree **)(pt->fncs))[args[3]]
#define FNC_ARG_4 ((ptree **)(pt->fncs))[args[4]]
#define FNC_ARG_5 ((ptree **)(pt->fncs))[args[5]]
#define FNC_ARG_6 ((ptree **)(pt->fncs))[args[6]]

#define SCHEME_RESULT pt->xens[args[0]]
#define SCHEME_ARG_1 pt->xens[args[1]]
#define SCHEME_ARG_2 pt->xens[args[2]]
#define SCHEME_ARG_3 pt->xens[args[3]]

#define VECT_RESULT pt->vects[args[0]]
#define VECT_ARG_1 pt->vects[args[1]]
#define VECT_ARG_2 pt->vects[args[2]]
#define VECT_ARG_3 pt->vects[args[3]]

#define LIST_RESULT pt->lists[args[0]]
#define LIST_ARG_1 pt->lists[args[1]]
#define LIST_ARG_2 pt->lists[args[2]]
#define LIST_ARG_3 pt->lists[args[3]]


static void quit(int *args, ptree *pt) {pt->all_done = true;}

static void jump(int *args, ptree *pt) {pt->pc += pt->ints[args[0]];}

static void jump_abs(int *args, ptree *pt) {pt->pc = pt->program + pt->ints[args[0]];}
static void jump_indirect(int *args, ptree *pt) {pt->pc = pt->program + pt->ints[pt->ints[args[0]]];}
static void jump_if_abs(int *args, ptree *pt) {if (INT_ARG_1 != 0) pt->pc = pt->program + pt->ints[args[0]];}
static void jump_if_not_abs(int *args, ptree *pt) {if (INT_ARG_1 == 0) pt->pc = pt->program + pt->ints[args[0]];}

static void jump_if_equal(int *args, ptree *pt) {if (INT_ARG_1 == INT_ARG_2) pt->pc = pt->program + pt->ints[args[0]];}
static void jump_if_not_equal(int *args, ptree *pt) {if (INT_ARG_1 != INT_ARG_2) pt->pc = pt->program + pt->ints[args[0]];}

static void inc_and_jump_if_not_equal(int *args, ptree *pt) 
{
  (INT_ARG_1)++;                              /* i++ */
  if (INT_ARG_1 != INT_ARG_2)                 /* (= i lim) */
    pt->pc = pt->program + pt->ints[args[0]]; /* jump back to start of loop body */
  else pt->pc++;                              /* else skip redundant end test */
}

static void jump_if(int *args, ptree *pt) {if (INT_ARG_1 != 0) pt->pc += pt->ints[args[0]];}
static void jump_if_not(int *args, ptree *pt) {if (INT_ARG_1 == 0) pt->pc += pt->ints[args[0]];}

static void jump_if_equal_i2_rel(int *args, ptree *pt)     {if (INT_ARG_1 == INT_ARG_2) pt->pc += pt->ints[args[0]];}
static void jump_if_not_equal_i2_rel(int *args, ptree *pt) {if (INT_ARG_1 != INT_ARG_2) pt->pc += pt->ints[args[0]];}
static void jump_if_gt_i2_rel(int *args, ptree *pt)        {if (INT_ARG_1 > INT_ARG_2) pt->pc += pt->ints[args[0]];}
static void jump_if_not_gt_i2_rel(int *args, ptree *pt)    {if (INT_ARG_1 <= INT_ARG_2) pt->pc += pt->ints[args[0]];}
static void jump_if_lt_i2_rel(int *args, ptree *pt)        {if (INT_ARG_1 < INT_ARG_2) pt->pc += pt->ints[args[0]];}
static void jump_if_not_lt_i2_rel(int *args, ptree *pt)    {if (INT_ARG_1 >= INT_ARG_2) pt->pc += pt->ints[args[0]];}
static void jump_if_geq_i2_rel(int *args, ptree *pt)       {if (INT_ARG_1 >= INT_ARG_2) pt->pc += pt->ints[args[0]];}
static void jump_if_not_geq_i2_rel(int *args, ptree *pt)   {if (INT_ARG_1 < INT_ARG_2) pt->pc += pt->ints[args[0]];}
static void jump_if_leq_i2_rel(int *args, ptree *pt)       {if (INT_ARG_1 <= INT_ARG_2) pt->pc += pt->ints[args[0]];}
static void jump_if_not_leq_i2_rel(int *args, ptree *pt)   {if (INT_ARG_1 > INT_ARG_2) pt->pc += pt->ints[args[0]];}

static void jump_if_equal_f2_rel(int *args, ptree *pt)     {if (FLOAT_ARG_1 == FLOAT_ARG_2) pt->pc += pt->ints[args[0]];}
static void jump_if_not_equal_f2_rel(int *args, ptree *pt) {if (FLOAT_ARG_1 != FLOAT_ARG_2) pt->pc += pt->ints[args[0]];}
static void jump_if_gt_f2_rel(int *args, ptree *pt)        {if (FLOAT_ARG_1 > FLOAT_ARG_2) pt->pc += pt->ints[args[0]];}
static void jump_if_not_gt_f2_rel(int *args, ptree *pt)    {if (FLOAT_ARG_1 <= FLOAT_ARG_2) pt->pc += pt->ints[args[0]];}
static void jump_if_lt_f2_rel(int *args, ptree *pt)        {if (FLOAT_ARG_1 < FLOAT_ARG_2) pt->pc += pt->ints[args[0]];}
static void jump_if_not_lt_f2_rel(int *args, ptree *pt)    {if (FLOAT_ARG_1 >= FLOAT_ARG_2) pt->pc += pt->ints[args[0]];}
static void jump_if_geq_f2_rel(int *args, ptree *pt)       {if (FLOAT_ARG_1 >= FLOAT_ARG_2) pt->pc += pt->ints[args[0]];}
static void jump_if_not_geq_f2_rel(int *args, ptree *pt)   {if (FLOAT_ARG_1 < FLOAT_ARG_2) pt->pc += pt->ints[args[0]];}
static void jump_if_leq_f2_rel(int *args, ptree *pt)       {if (FLOAT_ARG_1 <= FLOAT_ARG_2) pt->pc += pt->ints[args[0]];}
static void jump_if_not_leq_f2_rel(int *args, ptree *pt)   {if (FLOAT_ARG_1 > FLOAT_ARG_2) pt->pc += pt->ints[args[0]];}

static void jump_if_equal_i3_rel(int *args, ptree *pt)     {if ((INT_ARG_1 == INT_ARG_2) && (INT_ARG_2 == INT_ARG_3)) pt->pc += pt->ints[args[0]];}
static void jump_if_not_equal_i3_rel(int *args, ptree *pt) {if (!((INT_ARG_1 == INT_ARG_2) && (INT_ARG_2 == INT_ARG_3))) pt->pc += pt->ints[args[0]];}
static void jump_if_gt_i3_rel(int *args, ptree *pt)        {if ((INT_ARG_1 > INT_ARG_2) && (INT_ARG_2 > INT_ARG_3)) pt->pc += pt->ints[args[0]];}
static void jump_if_not_gt_i3_rel(int *args, ptree *pt)    {if (!((INT_ARG_1 > INT_ARG_2) && (INT_ARG_2 > INT_ARG_3))) pt->pc += pt->ints[args[0]];}
static void jump_if_lt_i3_rel(int *args, ptree *pt)        {if ((INT_ARG_1 < INT_ARG_2) && (INT_ARG_2 < INT_ARG_3)) pt->pc += pt->ints[args[0]];}
static void jump_if_not_lt_i3_rel(int *args, ptree *pt)    {if (!((INT_ARG_1 < INT_ARG_2) && (INT_ARG_2 < INT_ARG_3))) pt->pc += pt->ints[args[0]];}
static void jump_if_geq_i3_rel(int *args, ptree *pt)       {if ((INT_ARG_1 >= INT_ARG_2) && (INT_ARG_2 >= INT_ARG_3)) pt->pc += pt->ints[args[0]];}
static void jump_if_not_geq_i3_rel(int *args, ptree *pt)   {if (!((INT_ARG_1 >= INT_ARG_2) && (INT_ARG_2 >= INT_ARG_3))) pt->pc += pt->ints[args[0]];}
static void jump_if_leq_i3_rel(int *args, ptree *pt)       {if ((INT_ARG_1 <= INT_ARG_2) && (INT_ARG_2 <= INT_ARG_3)) pt->pc += pt->ints[args[0]];}
static void jump_if_not_leq_i3_rel(int *args, ptree *pt)   {if (!((INT_ARG_1 <= INT_ARG_2) && (INT_ARG_2 <= INT_ARG_3))) pt->pc += pt->ints[args[0]];}

static void jump_if_equal_f3_rel(int *args, ptree *pt)     {if ((FLOAT_ARG_1 == FLOAT_ARG_2) && (FLOAT_ARG_2 == FLOAT_ARG_3)) pt->pc += pt->ints[args[0]];}
static void jump_if_not_equal_f3_rel(int *args, ptree *pt) {if (!((FLOAT_ARG_1 == FLOAT_ARG_2) && (FLOAT_ARG_2 == FLOAT_ARG_3))) pt->pc += pt->ints[args[0]];}
static void jump_if_gt_f3_rel(int *args, ptree *pt)        {if ((FLOAT_ARG_1 > FLOAT_ARG_2) && (FLOAT_ARG_2 > FLOAT_ARG_3)) pt->pc += pt->ints[args[0]];}
static void jump_if_not_gt_f3_rel(int *args, ptree *pt)    {if (!((FLOAT_ARG_1 > FLOAT_ARG_2) && (FLOAT_ARG_2 > FLOAT_ARG_3))) pt->pc += pt->ints[args[0]];}
static void jump_if_lt_f3_rel(int *args, ptree *pt)        {if ((FLOAT_ARG_1 < FLOAT_ARG_2) && (FLOAT_ARG_2 < FLOAT_ARG_3)) pt->pc += pt->ints[args[0]];}
static void jump_if_not_lt_f3_rel(int *args, ptree *pt)    {if (!((FLOAT_ARG_1 < FLOAT_ARG_2) && (FLOAT_ARG_2 < FLOAT_ARG_3))) pt->pc += pt->ints[args[0]];}
static void jump_if_geq_f3_rel(int *args, ptree *pt)       {if ((FLOAT_ARG_1 >= FLOAT_ARG_2) && (FLOAT_ARG_2 >= FLOAT_ARG_3)) pt->pc += pt->ints[args[0]];}
static void jump_if_not_geq_f3_rel(int *args, ptree *pt)   {if (!((FLOAT_ARG_1 >= FLOAT_ARG_2) && (FLOAT_ARG_2 >= FLOAT_ARG_3))) pt->pc += pt->ints[args[0]];}
static void jump_if_leq_f3_rel(int *args, ptree *pt)       {if ((FLOAT_ARG_1 <= FLOAT_ARG_2) && (FLOAT_ARG_2 <= FLOAT_ARG_3)) pt->pc += pt->ints[args[0]];}
static void jump_if_not_leq_f3_rel(int *args, ptree *pt)   {if (!((FLOAT_ARG_1 <= FLOAT_ARG_2) && (FLOAT_ARG_2 <= FLOAT_ARG_3))) pt->pc += pt->ints[args[0]];}

static void jump_if_odd_i_rel(int *args, ptree *pt)        {if (INT_ARG_1 & 1) pt->pc += pt->ints[args[0]];}
static void jump_if_even_i_rel(int *args, ptree *pt)       {if (!(INT_ARG_1 & 1)) pt->pc += pt->ints[args[0]];}

static void jump_if_zero_i_rel(int *args, ptree *pt)           {if (INT_ARG_1 == 0) pt->pc += pt->ints[args[0]];}
static void jump_if_not_zero_i_rel(int *args, ptree *pt)       {if (INT_ARG_1 != 0) pt->pc += pt->ints[args[0]];}

static void jump_if_negative_i_rel(int *args, ptree *pt)       {if (INT_ARG_1 < 0) pt->pc += pt->ints[args[0]];}
static void jump_if_not_negative_i_rel(int *args, ptree *pt)   {if (INT_ARG_1 >= 0) pt->pc += pt->ints[args[0]];}

static void jump_if_positive_i_rel(int *args, ptree *pt)       {if (INT_ARG_1 > 0) pt->pc += pt->ints[args[0]];}
static void jump_if_not_positive_i_rel(int *args, ptree *pt)   {if (INT_ARG_1 <= 0) pt->pc += pt->ints[args[0]];}

static void jump_if_zero_f_rel(int *args, ptree *pt)           {if (FLOAT_ARG_1 == 0.0) pt->pc += pt->ints[args[0]];}
static void jump_if_not_zero_f_rel(int *args, ptree *pt)       {if (FLOAT_ARG_1 != 0.0) pt->pc += pt->ints[args[0]];}

static void jump_if_negative_f_rel(int *args, ptree *pt)       {if (FLOAT_ARG_1 < 0.0) pt->pc += pt->ints[args[0]];}
static void jump_if_not_negative_f_rel(int *args, ptree *pt)   {if (FLOAT_ARG_1 >= 0.0) pt->pc += pt->ints[args[0]];}

static void jump_if_positive_f_rel(int *args, ptree *pt)       {if (FLOAT_ARG_1 > 0.0) pt->pc += pt->ints[args[0]];}
static void jump_if_not_positive_f_rel(int *args, ptree *pt)   {if (FLOAT_ARG_1 <= 0.0) pt->pc += pt->ints[args[0]];}


static void store_i(int *args, ptree *pt) {INT_RESULT = INT_ARG_1;}


static void store_f(int *args, ptree *pt) {FLOAT_RESULT = FLOAT_ARG_1;}


static void store_f_i(int *args, ptree *pt) {INT_RESULT = (Int)FLOAT_ARG_1;}


static void store_i_f(int *args, ptree *pt) {FLOAT_RESULT = (Double)INT_ARG_1;}


static void store_false(int *args, ptree *pt) {BOOL_RESULT = (Int)false;}


static void store_true(int *args, ptree *pt) {BOOL_RESULT = (Int)true;}


static void store_b_b(int *args, ptree *pt) {BOOL_RESULT = BOOL_ARG_1;}


static void store_b_clm(int *args, ptree *pt) {BOOL_RESULT = (bool)(CLM_ARG_1 != NULL);} /* != NULLs are for MS C++ */


static void store_b_vct(int *args, ptree *pt) {BOOL_RESULT = (bool)(VCT_ARG_1 != NULL);} 

#if USE_SND
static void store_b_sampler(int *args, ptree *pt) {BOOL_RESULT = (bool)(SAMPLER_ARG_1);}
#endif

static void store_b_sd(int *args, ptree *pt) {BOOL_RESULT = (bool)(SOUND_DATA_ARG_1 != NULL);}


static void store_c(int *args, ptree *pt) {CHAR_RESULT = (int)CHAR_ARG_1;}


static void store_x(int *args, ptree *pt) {SCHEME_RESULT = SCHEME_ARG_1;}


static void store_s(int *args, ptree *pt) 
{
  if (STRING_RESULT) free(STRING_RESULT);
  STRING_RESULT = mus_strdup(STRING_ARG_1);
}

static void inc_i_1(int *args, ptree *pt);
static void add_i2(int *args, ptree *pt);
static void equal_i2(int *args, ptree *pt);
static void gt_i2(int *args, ptree *pt);
static void lt_i2(int *args, ptree *pt);
static void geq_i2(int *args, ptree *pt);
static void leq_i2(int *args, ptree *pt);

static void equal_f2(int *args, ptree *pt);
static void gt_f2(int *args, ptree *pt);
static void lt_f2(int *args, ptree *pt);
static void geq_f2(int *args, ptree *pt);
static void leq_f2(int *args, ptree *pt);

static void equal_i3(int *args, ptree *pt);
static void gt_i3(int *args, ptree *pt);
static void lt_i3(int *args, ptree *pt);
static void geq_i3(int *args, ptree *pt);
static void leq_i3(int *args, ptree *pt);

static void equal_f3(int *args, ptree *pt);
static void gt_f3(int *args, ptree *pt);
static void lt_f3(int *args, ptree *pt);
static void geq_f3(int *args, ptree *pt);
static void leq_f3(int *args, ptree *pt);

static void not_b(int *args, ptree *pt);

static void odd_i(int *args, ptree *pt);
static void even_i(int *args, ptree *pt);
static void zero_i(int *args, ptree *pt);
static void negative_i(int *args, ptree *pt);
static void positive_i(int *args, ptree *pt);
static void zero_f(int *args, ptree *pt);
static void negative_f(int *args, ptree *pt);
static void positive_f(int *args, ptree *pt);

static void add_f2_mult(int *args, ptree *pt);
static void add_f2(int *args, ptree *pt);
static void add_f3_mult(int *args, ptree *pt);
static void add_f3(int *args, ptree *pt);
static void subtract_f2(int *args, ptree *pt);
static void subtract_f2_mult(int *args, ptree *pt);
static void subtract_f3(int *args, ptree *pt);
static void subtract_f3_mult(int *args, ptree *pt);
static void multiply_add_f2(int *args, ptree *pt);
static void multiply_add_f2_mult(int *args, ptree *pt);
static void oscil_0f_1(int *args, ptree *pt);
static void oscil_0f_1_env(int *args, ptree *pt);
static void oscil_0f_1_mult(int *args, ptree *pt);
static void oscil_1f_1(int *args, ptree *pt);
static void oscil_1f_1_env(int *args, ptree *pt);
static void oscil_1f_1_mult(int *args, ptree *pt);
static void oscil_1f_2(int *args, ptree *pt);
static void oscil_1f_2_env(int *args, ptree *pt);
static void oscil_1f_2_mult(int *args, ptree *pt);
static void oscil_1f_2m(int *args, ptree *pt);
static void oscil_1f_2m_env(int *args, ptree *pt);
static void oscil_1f_2m_mult(int *args, ptree *pt);
static void oscil_1f_3ma(int *args, ptree *pt);
static void oscil_1f_3ma_mult(int *args, ptree *pt);
static void oscil_1f_3ma_env(int *args, ptree *pt);
static void oscil_1f_3(int *args, ptree *pt);
static void oscil_1f_3_mult(int *args, ptree *pt);
static void oscil_1f_3_env(int *args, ptree *pt);
static void polywave_0f_env(int *args, ptree *pt);
static void polywave_0f_mult(int *args, ptree *pt);
static void polywave_0f(int *args, ptree *pt);
static void polywave_1f_env(int *args, ptree *pt);
static void polywave_1f_mult(int *args, ptree *pt);
static void polywave_1f(int *args, ptree *pt);
static void polywave_1f_2_env(int *args, ptree *pt);
static void polywave_1f_2_mult(int *args, ptree *pt);
static void polywave_1f_2(int *args, ptree *pt);
static void random_f(int *args, ptree *pt);
static void random_f_mult(int *args, ptree *pt);
static void sin_f(int *args, ptree *pt);
static void sin_f_mult(int *args, ptree *pt);
static void cos_f(int *args, ptree *pt);
static void cos_f_mult(int *args, ptree *pt);
static void abs_f(int *args, ptree *pt);
static void abs_f_mult(int *args, ptree *pt);
static void sqrt_f(int *args, ptree *pt);
static void sqrt_f_mult(int *args, ptree *pt);
static void env_linear_0f_mult(int *args, ptree *pt);
static void env_linear_0f_env(int *args, ptree *pt);
static void env_linear_0f(int *args, ptree *pt);
static void rand_0f(int *args, ptree *pt);
static void rand_0f_env(int *args, ptree *pt);
static void rand_0f_mult(int *args, ptree *pt);
static void rand_interp_0f(int *args, ptree *pt);
static void rand_interp_0f_env(int *args, ptree *pt);
static void rand_interp_0f_mult(int *args, ptree *pt);
static void vct_constant_ref_0(int *args, ptree *pt);
static void vct_constant_ref_0_mult(int *args, ptree *pt);
static void vct_ref_f(int *args, ptree *pt);
static void vct_ref_f_mult(int *args, ptree *pt);
static void vector_ref_f(int *args, ptree *pt);
static void delay_1f(int *args, ptree *pt);
static void delay_1f_env(int *args, ptree *pt);
static void delay_1f_mult(int *args, ptree *pt);
static void delay_1f_noz(int *args, ptree *pt);
static void delay_1f_noz_env(int *args, ptree *pt);
static void delay_1f_noz_mult(int *args, ptree *pt);
static void granulate_0f_env(int *args, ptree *pt);
static void granulate_0f_mult(int *args, ptree *pt);
static void granulate_0f(int *args, ptree *pt);
static void formant_1f(int *args, ptree *pt);
static void formant_1f_mult(int *args, ptree *pt);
static void formant_1f_env(int *args, ptree *pt);
static void firmant_1f(int *args, ptree *pt);
static void firmant_1f_mult(int *args, ptree *pt);
static void firmant_1f_env(int *args, ptree *pt);
static void firmant_2f(int *args, ptree *pt);
static void firmant_2f_mult(int *args, ptree *pt);
static void firmant_2f_env(int *args, ptree *pt);
static void polyshape_1fn(int *args, ptree *pt);
static void polyshape_1fn_mult(int *args, ptree *pt);
static void polyshape_1fn_env(int *args, ptree *pt);
static void oscil_0f_vect(int *args, ptree *pt);
static void oscil_0f_vect_mult(int *args, ptree *pt);
static void oscil_0f_vect_env(int *args, ptree *pt);
static void oscil_1f_vect(int *args, ptree *pt);
static void oscil_1f_vect_mult(int *args, ptree *pt);
static void oscil_1f_vect_env(int *args, ptree *pt);
static void formant_1f_vect(int *args, ptree *pt);
static void formant_1f_vect_mult(int *args, ptree *pt);
static void formant_1f_vect_env(int *args, ptree *pt);
static void firmant_1f_vect(int *args, ptree *pt);
static void firmant_1f_vect_mult(int *args, ptree *pt);
static void firmant_1f_vect_env(int *args, ptree *pt);
static void vct_set_f_add(int *args, ptree *pt);
static void vct_set_f_mult(int *args, ptree *pt);

static void rand_interp_0f_add(int *args, ptree *pt);
static void formant_1f_add(int *args, ptree *pt);
static void abs_f_add(int *args, ptree *pt);
static void sin_f_add(int *args, ptree *pt);
static void cos_f_add(int *args, ptree *pt);
static void abs_f_mult_add(int *args, ptree *pt);
static void mus_random_f_add(int *args, ptree *pt);
static void mus_random_f(int *args, ptree *pt);
static void random_f_add(int *args, ptree *pt);
static void subtract_f2_add(int *args, ptree *pt);
static void sin_f_mult_add(int *args, ptree *pt);
static void cos_f_mult_add(int *args, ptree *pt);
static void subtract_f2_mult_add(int *args, ptree *pt);
static void vct_ref_f_add(int *args, ptree *pt);

static void locsig_3f_mono_no_rev(int *args, ptree *pt);
static void locsig_v_mono_no_rev(int *args, ptree *pt);
static void locsig_v_stereo(int *args, ptree *pt);
static void locsig_v_mono(int *args, ptree *pt);
static void outa_3f(int *args, ptree *pt);
static void outa_multiply_f2(int *args, ptree *pt);
static void outa_multiply_f3(int *args, ptree *pt);
static void frame_set_0r(int *args, ptree *pt);
static void set_scaler_f(int *args, ptree *pt);
static void vct_set_f(int *args, ptree *pt);
static void outa_polywave_1_mult(int *args, ptree *pt);
static void outa_oscil_1_mult(int *args, ptree *pt);
static void outa_add_f2_mult(int *args, ptree *pt);
static void outa_add_f3_mult(int *args, ptree *pt);
static void sound_data_set_f(int *args, ptree *pt);

static xen_value *convert_int_to_dbl(ptree *prog, xen_value *i)
{
  xen_value *val;
  val = make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, 0.0), i->constant);
  if (i->constant == R_CONSTANT)
    prog->dbls[val->addr] = (Double)(prog->ints[i->addr]);
  else add_triple_to_ptree(prog, va_make_triple(store_i_f, "int_to_double", 2, val, i));
  return(val);
}


static xen_value *convert_dbl_to_int(ptree *prog, xen_value *i, bool exact)
{
  xen_value *v;
  Double val;
  val = prog->dbls[i->addr];
  if ((exact) && ((int)(floor(val)) != (int)val)) return(NULL);
  v = make_xen_value(R_INT, add_int_to_ptree(prog, 0), i->constant);
  if (i->constant == R_CONSTANT)
    prog->ints[v->addr] = (Int)val;
  else add_triple_to_ptree(prog, va_make_triple(store_f_i, "dbl_to_int", 2, v, i));
  return(v);
}


static triple *set_var(ptree *pt, xen_value *var, xen_value *init_val)
{
  /* this also affects return types of things like "if" */
  switch (var->type)
    {
    case R_FLOAT:
      return(add_triple_to_ptree(pt, va_make_triple(store_f, "store_f", 2, var, init_val)));
      break;

    case R_STRING:
      return(add_triple_to_ptree(pt, va_make_triple(store_s, "store_s", 2, var, init_val)));
      break;

    case R_INT:
      return(add_triple_to_ptree(pt, va_make_triple(store_i, "store_i", 2, var, init_val)));
      break;

    case R_BOOL:
      {
	/* null pointers can only come as explicitly type-declared args in run/run-eval */
	switch (init_val->type)
	  {
	  case R_BOOL:         return(add_triple_to_ptree(pt, va_make_triple(store_b_b, "set_bool", 2, var, init_val))); break;
	  case R_VCT:          return(add_triple_to_ptree(pt, va_make_triple(store_b_vct, "set_vct", 2, var, init_val))); break;
	  case R_SOUND_DATA:   return(add_triple_to_ptree(pt, va_make_triple(store_b_sd, "set_sd", 2, var, init_val))); break;
	  case R_CLM:          return(add_triple_to_ptree(pt, va_make_triple(store_b_clm, "set_clm", 2, var, init_val))); break;
#if USE_SND
	  case R_SAMPLER:      return(add_triple_to_ptree(pt, va_make_triple(store_b_sampler, "set_rd", 2, var, init_val))); break;
#endif
	  default:
	    {
	      xen_value *temp_v = NULL;
	      triple *trp;
	      /* nearly everything is true in Scheme -- these are not pointers, or can't be created within run */
	      temp_v = make_xen_value(R_BOOL, add_int_to_ptree(pt, (Int)true), R_CONSTANT); 
	      trp = add_triple_to_ptree(pt, va_make_triple(store_b_b, "set_b", 2, var, temp_v));
	      /* temp_v is used only as a way to pass in an address, so it should be freed */
	      if (temp_v) free(temp_v);
	      return(trp);
	    }
	    break;
	  }
      }
      break;

    case R_CHAR:
      return(add_triple_to_ptree(pt, va_make_triple(store_c, "set_chr", 2, var, init_val)));
      break;

    case R_SYMBOL: case R_KEYWORD:
      return(add_triple_to_ptree(pt, va_make_triple(store_x, "set_xen", 2, var, init_val)));
      break;
    }
  /* this is not necessarily an error as long as we don't actually allow
   *   explicit set! of the unhandled types -- a let binding simply
   *   passes the reference through, which should be ok since it's read-only.
   */
  /*
  fprintf(stderr, "set: %s(%s. %d) %s(%s, %d) failed\n", 
	  describe_xen_value(var, pt), type_name(var->type), var->type,
	  describe_xen_value(init_val, pt), type_name(init_val->type), init_val->type);
  */
  return(NULL);
}


static triple *set_var_no_opt(ptree *pt, xen_value *var, xen_value *init_val)
{
  triple *trp;
  trp = set_var(pt, var, init_val);
  if (trp) trp->no_opt = true;
  return(trp);
}


static xen_value *walk(ptree *prog, s7_pointer form, walk_result_t walk_result);

static xen_value *walk_sequence(ptree *prog, s7_pointer body, walk_result_t need_result, const char *name)
{
  xen_value *v = NULL;
  int i, body_forms;
  s7_pointer lbody;

  if (body == scheme_undefined) return(NULL);

  body_forms = s7_list_length(s7, body);
  if (body_forms == 0) 
    return(make_xen_value(R_BOOL, add_int_to_ptree(prog, (Int)false), R_CONSTANT));

  lbody = body;
  for (i = 0; i < body_forms; i++, lbody = s7_cdr(lbody))
    {
      if (v) free(v);
      v = walk(prog, 
	       s7_car(lbody), 
	       ((need_result != DONT_NEED_RESULT) && 
		(i == (body_forms - 1))) ? 
	       NEED_ANY_RESULT : DONT_NEED_RESULT);
      if (v == NULL) 
	{
	  char *temp = NULL;
	  xen_value *v1;
	  v1 = run_warn("%s: can't handle %s", name, temp = s7_object_to_c_string(s7, s7_car(lbody)));
	  if (temp) free(temp);
	  return(v1);
	}
    }

  return(v);
}


static xen_value *lambda_form(ptree *prog, s7_pointer form, bool separate, xen_value **args, int num_args, s7_pointer local_proc);

static char *define_form(ptree *prog, s7_pointer form)
{
  /* behaves here just like a sequential bind (let*) */
  s7_pointer var, val;
  xen_value *v, *vs;

  if ((!(s7_is_list(s7, form))) || (form == scheme_nil)) return(NULL);

  var = scheme_cadr(form);
  if (!(s7_is_symbol(var)))
    {
      if (s7_is_list(s7, var))
	{
	  v = lambda_form(prog, form, false, NULL, 0, scheme_false);
	  if (v == NULL) 
	    {
	      char *temp = NULL, *str;
	      str = mus_format("can't handle this define: %s", temp = s7_object_to_c_string(s7, form));
	      if (temp) free(temp);
	      return(str);
	    }
	  add_var_to_ptree(prog, s7_symbol_name(s7_car(var)), v);
	  free(v);
	  return(NULL);
	}
      else 
	{
	  char *temp = NULL, *str;
	  str = mus_format("can't handle this definition: %s", temp = s7_object_to_c_string(s7, var));
	  if (temp) free(temp);
	  return(str);
	}
    }

  val = scheme_caddr(form);
  v = walk(prog, val, NEED_ANY_RESULT);
  if (v == NULL) 
    {
      char *temp = NULL, *str;
      str = mus_format("can't handle this define value: %s", temp = s7_object_to_c_string(s7, val));
      if (temp) free(temp);
      return(str);
    }

  if (v->constant == R_TEMPORARY)
    {
      v->constant = R_VARIABLE;
      add_var_to_ptree(prog, s7_symbol_name(var), v);
    }
  else
    {
      vs = transfer_value(prog, v);
      add_var_to_ptree(prog, s7_symbol_name(var), vs);
      set_var(prog, vs, v);
      free(vs);
    }
  free(v);
  return(NULL);
}


static s7_pointer handle_defines(ptree *prog, s7_pointer forms)
{
  if (!(s7_is_list(s7, forms))) return(forms);
  while (forms != scheme_nil)
    {
      s7_pointer form;
      char *temp = NULL;
      form = s7_car(forms);
      if ((s7_is_list(s7, form)) && 
	  (form != scheme_nil) &&
	  (safe_strcmp("define", temp = s7_object_to_c_string(s7, s7_car(form))) == 0))
	{
	  char *err;
	  if (temp) free(temp);
	  err = define_form(prog, form);
	  if (err != NULL) 
	    {
	      run_warn("%s", err);
	      free(err);
	      return(scheme_undefined);
	    }
	  forms = s7_cdr(forms);
	}
      else 
	{
	  if (temp) free(temp);
	  return(forms);
	}
    }
  return(forms);
}


static char *parallel_binds(ptree *prog, s7_pointer old_lets, const char *name)
{
  s7_pointer lets;
  int vars;

  lets = old_lets;
  vars = s7_list_length(s7, lets);
  if (vars > 0)
    {
      s7_pointer var;
      xen_value *v = NULL;
      xen_value **vs, **old_vs;
      int i;

      vs = (xen_value **)calloc(vars, sizeof(xen_value *));
      old_vs = (xen_value **)calloc(vars, sizeof(xen_value *));

      for (i = 0; i < vars; i++, lets = s7_cdr(lets))
	{
	  var = s7_car(lets);
	  v = walk(prog, scheme_cadr(var), NEED_ANY_RESULT);
	  if (v == NULL)
	    {
	      int j;
	      char *temp = NULL, *str;
	      for (j = 0; j < i; j++)
		{
		  if (old_vs[j]) free(old_vs[j]);
		  if (vs[j]) free(vs[j]);
		}
	      free(vs);
	      free(old_vs);
	      str = mus_format("can't handle %s var: %s", name, temp = s7_object_to_c_string(s7, lets));
	      if (temp) free(temp);
	      return(str);
	    }
	  old_vs[i] = v;

	  /* fprintf(stderr, "%s walk in let: %s %d\n", s7_object_to_c_string(s7, scheme_cadr(var)), describe_xen_value(v, prog), v->constant); */

	  if (v->constant != R_TEMPORARY)
	    vs[i] = transfer_value(prog, v);
	}

      lets = old_lets;
      for (i = 0; i < vars; i++, lets = s7_cdr(lets))
	{
	  var = s7_car(lets);
	  if (old_vs[i]->constant == R_TEMPORARY)
	    {
	      old_vs[i]->constant = R_VARIABLE;
	      add_var_to_ptree(prog, s7_symbol_name(s7_car(var)), old_vs[i]);
	    }
	  else
	    {
	      add_var_to_ptree(prog, s7_symbol_name(s7_car(var)), vs[i]);
	      set_var(prog, vs[i], old_vs[i]); /* establish let binding */
	      /* (run-eval '(do ((i 0 (1+ i))) ((= i 3)) (let ((a 1)) (set! a (+ a 1)) (display a)))) */
	      free(vs[i]);
	    }
	  free(old_vs[i]);
	}
      free(old_vs);
      free(vs);
    }
  return(NULL);
}


static char *sequential_binds(ptree *prog, s7_pointer old_lets, const char *name)
{
  s7_pointer lets;
  int vars;

  lets = old_lets;
  vars = s7_list_length(s7, lets);
  if (vars > 0)
    {
      int i;
      for (i = 0; i < vars; i++, lets = s7_cdr(lets))
	{
	  xen_value *v = NULL, *vs;
	  s7_pointer var;
	  var = s7_car(lets);
	  v = walk(prog, scheme_cadr(var), NEED_ANY_RESULT);

	  /* fprintf(stderr, "walk in let*: %s %d\n", describe_xen_value(v, prog), v->constant); */

	  if (v == NULL)
	    {
	      char *temp = NULL, *str;
	      str = mus_format("can't handle %s var: %s", name, temp = s7_object_to_c_string(s7, lets));
	      if (temp) free(temp);
	      return(str);
	    }

	  if (v->constant == R_TEMPORARY)
	    {
	      v->constant = R_VARIABLE;
	      add_var_to_ptree(prog, s7_symbol_name(s7_car(var)), v);
	    }
	  else
	    {
	      vs = transfer_value(prog, v);
	      add_var_to_ptree(prog, s7_symbol_name(s7_car(var)), vs);
	      set_var(prog, vs, v);
	      free(vs);
	    }
	  free(v);
	}
    }
  return(NULL);
}


static char *declare_args(ptree *prog, s7_pointer form, int default_arg_type, bool separate, xen_value **cur_args, int num_passed_args)
{
  /* need function arg types for funcall.  We have 3 sources of info:
   *   the passed args, the possible "(declare ...)" form, and
   *   the default values in the closure's arg list (if optional args)
   *   The latter is called the "template" here, "passed" = what we 
   *   see in the current call, "declared" = what declare tells us.
   *   All this needs to be merged, and default values set.
   * The declare form can be (declare (arg1 type1)...) or (snd-declare (quote ((arg1 type1...))))
   *   -- see snd-xen.c for an explanation of snd-declare.
   * The template might contain noise entries like ":optional, so
   *   its length is not the same as the number of template arguments.
   *
   * num_passed_args can also be off -- it includes the argument keywords if any
   */

  s7_pointer template_args, declarations = scheme_false;
  int i, num_template_args = 0, template_args_len = 0;

  /* !separate apparently means an embedded define: (lambda (y) (define (a) 3) (+ y (a))) */
  if (separate)
    template_args = scheme_cadr(form);
  else template_args = scheme_cdadr(form);
  /* template_args is the arglist as found in the function's closure */

  /* fprintf(stderr, "template_args: %s, list?: %s\n", s7_object_to_c_string(s7, template_args), (s7_is_list(s7, template_args)) ? "#t" : "#f"); */

  if (!(s7_is_list(s7, template_args))) 
    return(mus_format("can't handle this argument declaration: %s", s7_object_to_c_string(s7, template_args)));


  /* get number of actual template args (ignoring :key and so on) */
  template_args_len = s7_list_length(s7, template_args);
  num_template_args = template_args_len;
  for (i = 0; i < template_args_len; i++)
    if ((s7_is_symbol(s7_list_ref(s7, template_args, i))) &&
	((safe_strcmp(s7_symbol_name(s7_list_ref(s7, template_args, i)), ":optional") == 0) ||
	 (safe_strcmp(s7_symbol_name(s7_list_ref(s7, template_args, i)), ":key") == 0) ||
	 (safe_strcmp(s7_symbol_name(s7_list_ref(s7, template_args, i)), ":rest") == 0)))
      num_template_args--;

  if (num_passed_args > num_template_args)
    {
      /* in this context, we're not trying to handle :rest args */
      bool got_keyword = false;
      for (i = 1; i <= num_passed_args; i++)
	if (cur_args[i]->type == R_KEYWORD)
	  {
	    got_keyword = true;
	    break;
	  }
      if (!got_keyword)
	{
	  char *temp = NULL, *str;
	  str = mus_format("got too many args: %d passed, but %s declared", num_passed_args, temp = s7_object_to_c_string(s7, template_args));
	  if (temp) free(temp);
	  return(str);
	}
    }

  prog->arity = num_template_args;
  if (num_template_args == 0)
    return(NULL);                                         /* no args expected, none passed, nothing to check */

  if (num_passed_args < num_template_args)
    {
      declarations = scheme_caddr(form);                     /* either declare or snd-declare */
      if ((s7_is_string(declarations)) &&                 /* possible doc string */
	  (scheme_cdddr(form) != scheme_nil))
	declarations = scheme_cadddr(form);

      if ((s7_is_list(s7, declarations)) && 
	  (declarations != scheme_nil) &&
	  (s7_is_symbol(s7_car(declarations))) &&
	  ((safe_strcmp(s7_symbol_name(s7_car(declarations)), "declare") == 0) ||
	   (safe_strcmp(s7_symbol_name(s7_car(declarations)), "snd-declare") == 0)))
	{
	  if (safe_strcmp(s7_symbol_name(s7_car(declarations)), "declare") == 0)
	    declarations = s7_cdr(declarations);
	  else declarations = scheme_cadr(scheme_cadr(declarations));
	}
      else declarations = scheme_false;                      /* not a "declare" form after all */
    }

  /* fprintf(stderr, "num_args: %d, template_args: %s, declarations: %s\n", num_template_args, s7_object_to_c_string(s7, template_args), s7_object_to_c_string(s7, declarations)); */

  /* prepare the prog's local arg list */
  prog->args = (int *)calloc(num_template_args, sizeof(int));             /* value address */
  prog->default_args = (int *)calloc(num_template_args, sizeof(int));     /* default value address */
  prog->arg_types = (int *)calloc(num_template_args, sizeof(int));        /* value type */

  for (i = 0; i < num_template_args; i++, template_args = s7_cdr(template_args))
    {
      s7_pointer template_arg, template_arg_symbol;
      int arg_type;
      xen_value *v = NULL;

      template_arg = s7_car(template_args);
      if ((s7_is_keyword(template_arg)) &&
	  ((safe_strcmp(s7_symbol_name(template_arg), ":optional") == 0) ||
	   (safe_strcmp(s7_symbol_name(template_arg), ":key") == 0) ||
	   (safe_strcmp(s7_symbol_name(template_arg), ":rest") == 0)))
	{
	  template_args = s7_cdr(template_args);                        /* skip this one */
	  template_arg = s7_car(template_args);
	}
	  
      /* fprintf(stderr, "template arg: %s\n", s7_object_to_c_string(s7, template_arg)); */
      if (s7_is_list(s7, template_arg))
	template_arg_symbol = s7_car(template_arg);
      else template_arg_symbol = template_arg;
      
      arg_type = default_arg_type;
      /* first look for a declared type */
      
      if ((s7_is_list(s7, declarations)) &&
	  (declarations != scheme_nil))
	{
	  s7_pointer declaration;
	  /* (declare (x real) (a integer) (b string) ... */
	  
	  declaration = s7_car(declarations);
	  declarations = s7_cdr(declarations);
	  
	  if ((s7_is_list(s7, declaration)) &&
	      (s7_is_eq(s7_car(declaration), template_arg_symbol)) &&
	      (s7_is_symbol(scheme_cadr(declaration))))
	    {
	      const char *type;
	      type = s7_symbol_name(scheme_cadr(declaration));
	      arg_type = name_to_type(type);
	      
	      if (arg_type == R_UNSPECIFIED)                 /* not a predefined type name like "float" */
		{
		  if (safe_strcmp(type, "integer") == 0) 
		    arg_type = R_INT; 
		  else
		    {
		      if (safe_strcmp(type, "real") == 0) 
			arg_type = R_FLOAT; 
		      else 
			{
			  char *temp = NULL, *str;
			  str = mus_format("unknown type in declare: %s in %s", type, temp = s7_object_to_c_string(s7, form));
			  if (temp) free(temp);
			  return(str);
			}
		    }
		}
	    }
	  else 
	    {
	      /* declaration doesn't match template? */
	      char *temp1 = NULL, *temp2 = NULL, *temp3 = NULL, *str;
	      str = mus_format("unrecognized arg in declare: %s (for %s?) in %s", 
			       temp1 = s7_object_to_c_string(s7, declaration), 
			       temp2 = s7_object_to_c_string(s7, template_arg), 
			       temp3 = s7_object_to_c_string(s7, form));
	      if (temp1) free(temp1);
	      if (temp2) free(temp2);
	      if (temp3) free(temp3);
	      return(str);
	    }
	}
      else
	{
	  /* no applicable declaration -- look at passed arg (if any) first */
	  
	  if ((i < num_passed_args) && 
	      (cur_args) && 
	      (cur_args[i + 1]))
	    arg_type = cur_args[i + 1]->type;
	  else
	    {
	      if (prog->lambda_args > i)
		arg_type = prog->lambda_arg_types[i];
	      else
		{
		  /* look at template default, if any */
		  if (s7_is_list(s7, template_arg))
		    {
		      s7_pointer template_default;
		      template_default = scheme_cadr(template_arg);
		      arg_type = mus_run_xen_to_run_type(template_default);
		    }
		}
	    }
	}

      /* make a ptree slot for default arg value (copied later if needed) */
      if (s7_is_list(s7, template_arg))
	{
	  s7_pointer template_default;
	  template_default = scheme_cadr(template_arg);
	  
	  switch (arg_type)
	    {
	    case R_INT:     
	      v = make_xen_value(arg_type, add_int_to_ptree(prog, s7_number_to_integer(template_default)), R_CONSTANT);                 
	      break;
			  
	    case R_FLOAT:   
	      v = make_xen_value(arg_type, add_dbl_to_ptree(prog, s7_number_to_real(template_default)), R_CONSTANT);                
	      break;
	      
	    case R_STRING:  
	      v = make_xen_value(arg_type, add_string_to_ptree(prog, mus_strdup(s7_string(template_default))), R_CONSTANT); 
	      break;
			  
	    case R_CHAR:    
	      v = make_xen_value(arg_type, add_int_to_ptree(prog, (Int)(s7_character(template_default))), R_CONSTANT);           
	      break;
			  
	    case R_BOOL:    
	      v = make_xen_value(arg_type, add_int_to_ptree(prog, (template_default == scheme_false) ? 0 : 1), R_CONSTANT);          
	      break;
			  
	    default:        
	      return(mus_format("unimplemented argument default value: %s\n", s7_object_to_c_string(s7, template_default)));
	    }

	  /* fprintf(stderr, "add default arg %d: %s\n", i, describe_xen_value(v, prog)); */
	  prog->default_args[i] = v->addr;
	  free(v);
	  v = NULL;
	}
      
      add_var_to_ptree(prog, 
		       s7_symbol_name(template_arg_symbol), 
		       v = add_empty_var_to_ptree(prog, arg_type));
      prog->args[i] = v->addr;
      prog->arg_types[i] = v->type;
      free(v);
    }

  return(NULL);
}


static void undefine_locals(ptree *prog, int locals_loc)
{
  int i;
  for (i = locals_loc; i < prog->var_ctr; i++)
    if (prog->vars[i]) prog->vars[i] = free_xen_var(prog, prog->vars[i]);
  prog->var_ctr = locals_loc;
}


static xen_value *walk_then_undefine(ptree *prog, s7_pointer form, walk_result_t need_result, const char *name, int locals_loc)
{
  xen_value *v;
  v = walk_sequence(prog, handle_defines(prog, form), need_result, name);
  undefine_locals(prog, locals_loc);
  return(v);
}


static xen_value *lambda_form(ptree *prog, s7_pointer form, bool separate, xen_value **args, int num_args, s7_pointer local_proc)
{
  /* (lambda (args...) | args etc followed by forms */
  /* as args are declared as vars, put addrs in prog->args list */
  char *err = NULL;
  int locals_loc;

  /* fprintf(stderr, "lambda: %d, %s, %s\n", prog->got_lambda, s7_object_to_c_string(s7, form), s7_object_to_c_string(s7, local_proc)); */

  if (prog->got_lambda)
    {
      /* start a new ptree, walk body, return ptree as R_FUNCTION variable */
      ptree *new_tree;
      int outer_locals;
      new_tree = attach_to_ptree(prog);

      new_tree->code = local_proc;
      outer_locals = prog->var_ctr;
      /* inner level default arg type is int(?) */
      err = declare_args(new_tree, form, R_INT, separate, args, num_args);
      if (err)
	{
	  unattach_ptree(new_tree, prog); 
	  free_embedded_ptree(new_tree);
	  return(run_warn_with_free(err));
	}
      else
	{
	  new_tree->result = walk_then_undefine(new_tree, scheme_cddr(form), NEED_ANY_RESULT, "lambda", prog->var_ctr);
	  if (new_tree->result)
	    {
	      int i;
	      xen_value *v;
	      add_triple_to_ptree(new_tree, make_triple(quit, "quit", NULL, 0));
	      unattach_ptree(new_tree, prog);

	      for (i = outer_locals; i < prog->var_ctr; i++)
		if (prog->vars[i]) prog->vars[i] = free_xen_var(prog, prog->vars[i]);

	      prog->var_ctr = outer_locals;
	      v = make_xen_value(R_FUNCTION, add_fnc_to_ptree(prog, new_tree), R_VARIABLE);
	      add_obj_to_gcs(prog, R_FUNCTION, v->addr);
	      return(v);
	    }
	}
      /* we get here if walk failed, so clean up */
      unattach_ptree(new_tree, prog); /* needed in case we realloc'd during failed tree walk */
      free_embedded_ptree(new_tree);
      {
	xen_value *rv;
	char *temp = NULL;
	rv = run_warn("can't handle this embedded lambda: %s", temp = s7_object_to_c_string(s7, form));
	if (temp) free(temp);
	return(rv);
      }
    }

  /* outer level default arg type is float(?) */
  prog->got_lambda = true;
  locals_loc = prog->var_ctr;
  err = declare_args(prog, form, R_FLOAT, separate, args, num_args);
  if (err) return(run_warn_with_free(err));
  return(walk_then_undefine(prog, scheme_cddr(form), NEED_ANY_RESULT, "lambda", locals_loc));
}


static xen_value *begin_form(ptree *prog, s7_pointer form, walk_result_t need_result)
{
  return(walk_then_undefine(prog, s7_cdr(form), need_result, "begin", prog->var_ctr));
}


static xen_value *let_star_form(ptree *prog, s7_pointer form, walk_result_t need_result)
{
  int locals_loc;
  char *err = NULL;
  locals_loc = prog->var_ctr; /* lets can be nested */
  err = sequential_binds(prog, scheme_cadr(form), "let*");
  if (err) return(run_warn_with_free(err));
  if (!(prog->got_lambda)) prog->initial_pc = prog->program + prog->triple_ctr;
  return(walk_then_undefine(prog, scheme_cddr(form), need_result, "let*", locals_loc));
}


static xen_value *let_form(ptree *prog, s7_pointer form, walk_result_t need_result)
{
  /* keep vars until end of var section */
  s7_pointer lets;
  char *trouble = NULL;
  int locals_loc;
  lets = scheme_cadr(form);
  locals_loc = prog->var_ctr; /* lets can be nested */
  trouble = parallel_binds(prog, lets, "let");
  if (trouble) return(run_warn_with_free(trouble));
  if (!(prog->got_lambda)) prog->initial_pc = prog->program + prog->triple_ctr;
  return(walk_then_undefine(prog, scheme_cddr(form), need_result, "let", locals_loc));
}


static xen_value *coerce_to_boolean(ptree *prog, xen_value *v)
{
  xen_value *temp;
  temp = add_temporary_var_to_ptree(prog, R_BOOL);
  set_var_no_opt(prog, temp, v);
  free(v);
  return(temp);
}


#define NUM_IF_OPS 29

static opt_ops if_ops[NUM_IF_OPS] = {
  {not_b, "not_b", jump_if, "jump_if", jump_if_not, "jump_if_not"},

  {equal_i2, "equal_i2", jump_if_not_equal_i2_rel, "jump_if_not_equal_i2_rel", jump_if_equal_i2_rel, "jump_if_equal_i2_rel"},
  {gt_i2, "gt_i2", jump_if_not_gt_i2_rel, "jump_if_not_gt_i2_rel", jump_if_gt_i2_rel, "jump_if_gt_i2_rel"},
  {lt_i2, "lt_i2", jump_if_not_lt_i2_rel, "jump_if_not_lt_i2_rel", jump_if_lt_i2_rel, "jump_if_lt_i2_rel"},
  {geq_i2, "geq_i2", jump_if_not_geq_i2_rel, "jump_if_not_geq_i2_rel", jump_if_geq_i2_rel, "jump_if_geq_i2_rel"},
  {leq_i2, "leq_i2", jump_if_not_leq_i2_rel, "jump_if_not_leq_i2_rel", jump_if_leq_i2_rel, "jump_if_leq_i2_rel"},

  {equal_f2, "equal_f2", jump_if_not_equal_f2_rel, "jump_if_not_equal_f2_rel", jump_if_equal_f2_rel, "jump_if_equal_f2_rel"},
  {gt_f2, "gt_f2", jump_if_not_gt_f2_rel, "jump_if_not_gt_f2_rel", jump_if_gt_f2_rel, "jump_if_gt_f2_rel"},
  {lt_f2, "lt_f2", jump_if_not_lt_f2_rel, "jump_if_not_lt_f2_rel", jump_if_lt_f2_rel, "jump_if_lt_f2_rel"},
  {geq_f2, "geq_f2", jump_if_not_geq_f2_rel, "jump_if_not_geq_f2_rel", jump_if_geq_f2_rel, "jump_if_geq_f2_rel"},
  {leq_f2, "leq_f2", jump_if_not_leq_f2_rel, "jump_if_not_leq_f2_rel", jump_if_leq_f2_rel, "jump_if_leq_f2_rel"},

  {equal_i3, "equal_i3", jump_if_not_equal_i3_rel, "jump_if_not_equal_i3_rel", jump_if_equal_i3_rel, "jump_if_equal_i3_rel"},
  {gt_i3, "gt_i3", jump_if_not_gt_i3_rel, "jump_if_not_gt_i3_rel", jump_if_gt_i3_rel, "jump_if_gt_i3_rel"},
  {lt_i3, "lt_i3", jump_if_not_lt_i3_rel, "jump_if_not_lt_i3_rel", jump_if_lt_i3_rel, "jump_if_lt_i3_rel"},
  {geq_i3, "geq_i3", jump_if_not_geq_i3_rel, "jump_if_not_geq_i3_rel", jump_if_geq_i3_rel, "jump_if_geq_i3_rel"},
  {leq_i3, "leq_i3", jump_if_not_leq_i3_rel, "jump_if_not_leq_i3_rel", jump_if_leq_i3_rel, "jump_if_leq_i3_rel"},

  {equal_f3, "equal_f3", jump_if_not_equal_f3_rel, "jump_if_not_equal_f3_rel", jump_if_equal_f3_rel, "jump_if_equal_f3_rel"},
  {gt_f3, "gt_f3", jump_if_not_gt_f3_rel, "jump_if_not_gt_f3_rel", jump_if_gt_f3_rel, "jump_if_gt_f3_rel"},
  {lt_f3, "lt_f3", jump_if_not_lt_f3_rel, "jump_if_not_lt_f3_rel", jump_if_lt_f3_rel, "jump_if_lt_f3_rel"},
  {geq_f3, "geq_f3", jump_if_not_geq_f3_rel, "jump_if_not_geq_f3_rel", jump_if_geq_f3_rel, "jump_if_geq_f3_rel"},
  {leq_f3, "leq_f3", jump_if_not_leq_f3_rel, "jump_if_not_leq_f3_rel", jump_if_leq_f3_rel, "jump_if_leq_f3_rel"},

  {odd_i, "odd_i", jump_if_even_i_rel, "jump_if_even_i_rel", jump_if_odd_i_rel, "jump_if_odd_i_rel"},
  {even_i, "even_i", jump_if_odd_i_rel, "jump_if_odd_i_rel", jump_if_even_i_rel, "jump_if_even_i_rel"},

  {zero_i, "zero_i", jump_if_not_zero_i_rel, "jump_if_not_zero_i_rel", jump_if_zero_i_rel, "jump_if_zero_i_rel"},
  {zero_f, "zero_f", jump_if_not_zero_f_rel, "jump_if_not_zero_f_rel", jump_if_zero_f_rel, "jump_if_zero_f_rel"},
  {negative_i, "negative_i", jump_if_not_negative_i_rel, "jump_if_not_negative_i_rel", jump_if_negative_i_rel, "jump_if_negative_i_rel"},
  {negative_f, "negative_f", jump_if_not_negative_f_rel, "jump_if_not_negative_f_rel", jump_if_negative_f_rel, "jump_if_negative_f_rel"},
  {positive_i, "positive_i", jump_if_not_positive_i_rel, "jump_if_not_positive_i_rel", jump_if_positive_i_rel, "jump_if_positive_i_rel"},
  {positive_f, "positive_f", jump_if_not_positive_f_rel, "jump_if_not_positive_f_rel", jump_if_positive_f_rel, "jump_if_positive_f_rel"},
};


static int find_if_op(triple *prev_op)
{
  int i;
  for (i = 0; i < NUM_IF_OPS; i++)
    if (prev_op->function == if_ops[i].func)
      return(i);
  return(-1);
}

#define JUMP_IF_FALSE false
#define JUMP_IF_TRUE true

static int if_jump_opt(ptree *prog, xen_value *jump_to_false, xen_value *if_value, bool jump_choice)
{
  bool jumped = false;
  int current_pc;
  current_pc = prog->triple_ctr;

  if (prog->triple_ctr > 0)
    {
      triple *prev_op;
      int loc;
      prev_op = prog->program[prog->triple_ctr - 1];
      if (prev_op->args[0] == if_value->addr)
	{
	  loc = find_if_op(prev_op);
	  if (loc >= 0)
	    {
	      /* if it's "not", look at preceding op as well */
	      if ((prev_op->function == not_b) &&
		  (prog->triple_ctr > 1))
		{
		  triple *p2_op;
		  int not_loc;
		  p2_op = prog->program[prog->triple_ctr - 2];
		  
		  if (p2_op->args[0] == prev_op->args[1])
		    {
		      not_loc = find_if_op(p2_op);
		      if ((not_loc >= 0) &&
			  (p2_op->function != not_b) &&
			  (p2_op->args[0] == prev_op->args[1]))
			{
			  if (jump_choice == JUMP_IF_FALSE)
			    {
			      p2_op->function = if_ops[not_loc].env_func;
			      p2_op->op_name = if_ops[not_loc].env_func_name;
			    }
			  else
			    {
			      p2_op->function = if_ops[not_loc].mult_func;
			      p2_op->op_name = if_ops[not_loc].mult_func_name;
			    }
			  p2_op->args[0] = jump_to_false->addr;
			  p2_op->types[0] = R_INT;
#if WITH_COUNTERS
			  p2_op->func_loc = get_func_loc(p2_op->function, p2_op->op_name);
#endif				  
			  free_triple(prev_op);
			  prog->triple_ctr--;
			  prog->program[prog->triple_ctr] = NULL;
			  current_pc = prog->triple_ctr - 1;
			  jumped = true;
			}
		    }
		}
	      
	      if (!jumped)
		{
		  if (jump_choice == JUMP_IF_FALSE)
		    {
		      prev_op->function = if_ops[loc].mult_func;
		      prev_op->op_name = if_ops[loc].mult_func_name;
		    }
		  else
		    {
		      prev_op->function = if_ops[loc].env_func;
		      prev_op->op_name = if_ops[loc].env_func_name;
		    }
		  prev_op->args[0] = jump_to_false->addr;
		  prev_op->types[0] = R_INT;
#if WITH_COUNTERS
		  prev_op->func_loc = get_func_loc(prev_op->function, prev_op->op_name);
#endif
		  current_pc = prog->triple_ctr - 1;
		  jumped = true;
		}
	    }
	}
    }

  if (!jumped)
    {
      current_pc = prog->triple_ctr;

      if (jump_choice == JUMP_IF_FALSE)
	add_triple_to_ptree(prog, va_make_triple(jump_if_not, "jump_if_not", 2, jump_to_false, if_value));
      else add_triple_to_ptree(prog, va_make_triple(jump_if, "jump_if", 2, jump_to_false, if_value));
    }

  return(current_pc);
}


static xen_value *if_form(ptree *prog, s7_pointer form, walk_result_t need_result)
{
  /* form: (if selector true [false]) */
  xen_value *result = NULL, *true_result = NULL, *false_result = NULL;
  xen_value *jump_to_end = NULL, *jump_to_false, *if_value;
  int current_pc, false_pc = 0;
  bool has_false;

  has_false = (s7_list_length(s7, form) == 4);
  if_value = walk(prog, scheme_cadr(form), NEED_ANY_RESULT);                                      /* walk selector */

  if (if_value == NULL) 
    {
      if (run_warned) /* must have been some error in walk, passing it back to us */
	{
	  xen_value *rv;
	  char *temp = NULL;
	  rv = run_warn("if: bad selector? %s", temp = s7_object_to_c_string(s7, scheme_cadr(form)));
	  if (temp) free(temp);
	  return(rv);
	}
      /* else whatever it is, it has to be true? */
      /*      (let ((pr (cons 1 2))) (run (lambda () (if pr 1 2)))) */
      if_value = make_xen_value(R_BOOL, add_int_to_ptree(prog, true), R_CONSTANT);
    }

  if (if_value->type != R_BOOL) /* all ints are true */
    if_value = coerce_to_boolean(prog, if_value);
  if (if_value->constant == R_CONSTANT)
    {
      /* just ignore branch that can't be evaluated anyway */
      if (prog->ints[if_value->addr])
	{
	  free(if_value);
	  return(walk(prog, scheme_caddr(form), need_result));
	}
      if (has_false)
	{
	  free(if_value);
	  return(walk(prog, scheme_cadddr(form), need_result));
	}
      return(if_value);
    }

  jump_to_false = make_xen_value(R_INT, add_int_to_ptree(prog, 0), R_VARIABLE);
  current_pc = if_jump_opt(prog, jump_to_false, if_value, JUMP_IF_FALSE);
  free(if_value);

  true_result = walk(prog, scheme_caddr(form), need_result);                           /* walk true branch */
  if (true_result == NULL) 
    {
      xen_value *rv;
      char *temp = NULL;
      free(jump_to_false);
      rv = run_warn("if: can't handle true branch %s", temp = s7_object_to_c_string(s7, scheme_caddr(form)));
      if (temp) free(temp);
      return(rv);
    }
  if (need_result != DONT_NEED_RESULT)
    {
      result = add_temporary_var_to_ptree(prog, true_result->type);
      set_var_no_opt(prog, result, true_result);
    }
  if (has_false) 
    {
      false_pc = prog->triple_ctr;
      jump_to_end = make_xen_value(R_INT, add_int_to_ptree(prog, 0), R_VARIABLE);
      add_triple_to_ptree(prog, va_make_triple(jump, "jump", 1, jump_to_end));  /* jump to end (past false) */
    }
  prog->ints[jump_to_false->addr] = prog->triple_ctr - current_pc - 1;              /* fixup jump-to-false addr */
  free(jump_to_false);

  if (has_false)
    {
      false_result = walk(prog, scheme_cadddr(form), need_result);                     /* walk false branch */
      /* if false_result is null, that is an error even if need_result is DONT_NEED_RESULT (since the false branch does exist) */
      if (false_result == NULL)
	{
	  char *temp = NULL;
	  run_warn("if: can't handle false branch %s", temp = s7_object_to_c_string(s7, scheme_cadddr(form)));
	  if (temp) free(temp);
	  if (result) free(result);
	  if (jump_to_end) free(jump_to_end);
	  if (true_result) free(true_result);
	  return(NULL);
	}

      if (need_result != DONT_NEED_RESULT)
	{
	  if (false_result->type != true_result->type)
	    {
	      /* #f is ok as null pointer so fixup if needed */
#if USE_SND
	      if ((false_result->type == R_BOOL) &&
		  ((true_result->type == R_CLM) || 
		   (true_result->type == R_SAMPLER)))
#else
	      if ((false_result->type == R_BOOL) &&
		  (true_result->type == R_CLM))
#endif
		false_result->type = true_result->type;
	      else
		{
#if USE_SND
		  if ((true_result->type == R_BOOL) &&
		      ((false_result->type == R_CLM) || 
		       (false_result->type == R_SAMPLER)))
#else
		  if ((true_result->type == R_BOOL) &&
		      (false_result->type == R_CLM))
#endif
		    true_result->type = false_result->type;
		}
	    }

	  if (false_result->type != true_result->type)
	    {
	      run_warn("if branch types differ incompatibly: %s and %s", type_name(true_result->type), type_name(false_result->type));
	      if (result) free(result);
	      if (false_result) free(false_result);
	      if (jump_to_end) free(jump_to_end);
	      if (true_result) free(true_result);
	      return(NULL);
	    }
	  if (result) set_var_no_opt(prog, result, false_result);
	}

      prog->ints[jump_to_end->addr] = prog->triple_ctr - false_pc - 1;              /* fixup jump-past-false addr */
      free(jump_to_end);
    }
  else 
    {
      if (jump_to_end) free(jump_to_end);
      if (result) free(result);
      return(true_result);
    }
  if (true_result) free(true_result);
  if (false_result) free(false_result);
  if (result)
    return(result);
  return(make_xen_value(R_UNSPECIFIED, -1, R_CONSTANT));
}


static xen_value *cond_form(ptree *prog, s7_pointer form, walk_result_t need_result)
{
  xen_value *result = NULL, *test_value = NULL, *clause_value = NULL, *jump_to_next_clause = NULL;
  xen_value **fixups = NULL;
  int current_pc = 0, len, clause_ctr, i = 0, local_len;
  s7_pointer clauses, clause, local_clauses;

  clauses = s7_cdr(form);
  len = s7_list_length(s7, clauses);
  if (len == 0) return(run_warn("empty cond?"));
  fixups = (xen_value **)calloc(len, sizeof(xen_value *));

  for (clause_ctr = 0; clause_ctr < len; clause_ctr++, clauses = s7_cdr(clauses))
    {
      clause = s7_car(clauses);
      /* check car -- if #t evaluate rest */
      if ((s7_is_symbol(s7_car(clause))) &&
	  (safe_strcmp("else", s7_symbol_name(s7_car(clause))) == 0))
	test_value = make_xen_value(R_BOOL, add_int_to_ptree(prog, (Int)true), R_CONSTANT);
      else test_value = walk(prog, s7_car(clause), NEED_ANY_RESULT);
      if (test_value == NULL)
	{
	  xen_value *rv;
	  char *temp = NULL;
	  for (i = 0; i < clause_ctr; i++) 
	    if (fixups[i]) free(fixups[i]);
	  free(fixups);
	  rv = run_warn("cond test: %s", temp = s7_object_to_c_string(s7, s7_car(clause)));
	  if (temp) free(temp);
	  return(rv);
	}

      /* this coercion clobbers the result if there are no clauses:
      if (test_value->type != R_BOOL)
	test_value = coerce_to_boolean(prog, test_value);
      */

      /* test was not #f */
      local_clauses = s7_cdr(clause); /* can be null */
      local_len = s7_list_length(s7, local_clauses);
      if (local_len > 0)
	{
	  /* this coercion added 18-Mar-11 for (from clm23.scm or3 test):
           *   (let ((e1 0))
           *     (run
           *       (do ((i 0 (+ i 1)))
           *   	       ((= i 1))
           *         (cond (e1 (clm-print ";ok~%"))
           *   	           (#t (clm-print ";or3 1~%"))))))
	   *
	   * if_jump_opt assumes the test_value is a bool -- it does not coerce 
	   */
	  if (test_value->type != R_BOOL)
	    test_value = coerce_to_boolean(prog, test_value);

	  jump_to_next_clause = make_xen_value(R_INT, add_int_to_ptree(prog, 0), R_VARIABLE);
	  current_pc = if_jump_opt(prog, jump_to_next_clause, test_value, JUMP_IF_FALSE);

	  clause_value = walk_sequence(prog, local_clauses, need_result, "cond");
	  if (clause_value == NULL)
	    {
	      free(test_value);
	      free(jump_to_next_clause);
	      for (i = 0; i < clause_ctr; i++) 
		if (fixups[i]) free(fixups[i]);
	      free(fixups);
	      return(NULL);
	    }
	}
      else clause_value = copy_xen_value(test_value);
      /* now at end of this cond clause block -- fixup the jump if false above, add jump past rest if true */
      if (need_result != DONT_NEED_RESULT)
	{
	  if (result == NULL)
	    result = add_temporary_var_to_ptree(prog, clause_value->type);
	  else 
	    {
	      if (result->type != clause_value->type)
		{
		  run_warn("cond clause types differ: %s %s", type_name(clause_value->type), type_name(result->type));
		  free(clause_value);
		  free(result);
		  if (jump_to_next_clause) free(jump_to_next_clause);
		  for (i = 0; i < clause_ctr; i++) 
		    if (fixups[i]) free(fixups[i]);
		  free(fixups);
		  return(NULL);
		}
	    }
	  set_var(prog, result, clause_value);
	}
      fixups[clause_ctr] = make_xen_value(R_INT, add_int_to_ptree(prog, 0), R_VARIABLE);
      add_triple_to_ptree(prog, va_make_triple(jump_abs, "jump_abs", 1, fixups[clause_ctr])); 
      /* we jump to here is test was false */
      if (jump_to_next_clause)
	{
	  prog->ints[jump_to_next_clause->addr] = prog->triple_ctr - current_pc - 1;
	  free(jump_to_next_clause);
	  jump_to_next_clause = NULL;
	}
      free(clause_value);
      clause_value = NULL;
      free(test_value);
      test_value = NULL;
    }

  /* all the fixups are absolute jumps */
  for (i = 0; i < len; i++)
    if (fixups[i])
      {
	prog->ints[fixups[i]->addr] = prog->triple_ctr;
	free(fixups[i]);
      }
  if (fixups) free(fixups);
  if (need_result != DONT_NEED_RESULT)
    return(result);
  if (result) free(result);
  return(make_xen_value(R_UNSPECIFIED, -1, R_CONSTANT));
}


static xen_value *case_form(ptree *prog, s7_pointer form, walk_result_t need_result)
{
  /* only int (constant) selectors here */
  /* (case selector ((...) exp ...) ...) with possible 'else' */
  s7_pointer selector, body, keys, key;
  int i, j, body_len, num_keys, cur_key;
  xen_value *selval = NULL, *jump_to_selection = NULL, *v = NULL, *result = NULL, *keyval = NULL, *locval = NULL, *elseval = NULL;
  int *locations = NULL;
  xen_value **fixups = NULL;

  selector = scheme_cadr(form);
  selval = walk(prog, selector, NEED_ANY_RESULT);

  if (selval == NULL) 
    {
      xen_value *rv;
      char *temp = NULL;
      rv = run_warn("can't handle case selector: %s", temp = s7_object_to_c_string(s7, selector));
      if (temp) free(temp);
      return(rv);
    }

  if (selval->type != R_INT) 
    {
      xen_value *rv;
      char *temp = NULL;
      rv = run_warn("case only with ints: %s", temp = s7_object_to_c_string(s7, selector));
      if (temp) free(temp);
      return(rv);
    }

  jump_to_selection = make_xen_value(R_INT, add_int_to_ptree(prog, 0), R_VARIABLE);
  add_triple_to_ptree(prog, va_make_triple(jump_abs, "jump_abs", 1, jump_to_selection));
  body = scheme_cddr(form);
  body_len = s7_list_length(s7, body);
  fixups = (xen_value **)calloc(body_len, sizeof(xen_value *));
  locations = (int *)calloc(body_len, sizeof(int));

  for (i = 0; i < body_len; i++, body = s7_cdr(body))
    {
      /* ignore keys for now */
      locations[i] = prog->triple_ctr;
      if (!s7_is_pair(s7_car(body))) goto CASE_ERROR;
      v = walk_sequence(prog, scheme_cdar(body), need_result, "case");
      if (v == NULL) goto CASE_ERROR;
      if (need_result != DONT_NEED_RESULT)
	{
	  if (result == NULL)
	    result = add_temporary_var_to_ptree(prog, v->type);
	  else 
	    if (result->type != v->type)
	      {
		run_warn("case clause types differ: %s %s", type_name(v->type), type_name(result->type));
		free(v); v = NULL;
	      }
	  if (v) set_var(prog, result, v);
	}
      if (v == NULL) goto CASE_ERROR;
      free(v); v = NULL;
      fixups[i] = make_xen_value(R_INT, add_int_to_ptree(prog, 0), R_VARIABLE);
      add_triple_to_ptree(prog, va_make_triple(jump_abs, "jump_abs", 1, fixups[i])); 
    }

  /* fixup jump from selector to table of keys (here) */
  prog->ints[jump_to_selection->addr] = prog->triple_ctr;
  free(jump_to_selection); jump_to_selection = NULL;
  /* now make the selection */
  body = scheme_cddr(form);
  for (i = 0; i < body_len; i++, body = s7_cdr(body))
    {
      keys = scheme_caar(body);
      if (s7_is_symbol(keys))
	{
	  if ((s7_is_symbol(keys)) &&
	      (safe_strcmp(s7_symbol_name(keys), "else") == 0))
	    elseval = make_xen_value(R_INT, i, R_CONSTANT);
	  else 
	    {
	      char *temp = NULL;
	      run_warn("bad case key: %s", temp = s7_object_to_c_string(s7, keys));
	      if (temp) free(temp);
	      goto CASE_ERROR;
	    }
	}
      else
	{
	  num_keys = s7_list_length(s7, keys);
	  for (j = 0; j < num_keys; j++, keys = s7_cdr(keys))
	    {
	      key = s7_car(keys);
	      if (!(s7_is_integer(key)))
		{
		  char *temp = NULL;
		  run_warn("case only accepts integer selectors: %s", temp = s7_object_to_c_string(s7, key));
		  if (temp) free(temp);
		  goto CASE_ERROR;
		}
	      cur_key = s7_number_to_integer(key);
	      keyval = make_xen_value(R_INT, add_int_to_ptree(prog, cur_key), R_CONSTANT);
	      locval = make_xen_value(R_INT, add_int_to_ptree(prog, locations[i]), R_CONSTANT);
	      add_triple_to_ptree(prog, va_make_triple(jump_if_equal, "jump_if_equal", 3, locval, selval, keyval));
	      free(keyval); keyval = NULL;
	      free(locval); locval = NULL;
	    }
	}
    }

  /* now check for else clause */
  if (elseval)
    {
      locval = make_xen_value(R_INT, add_int_to_ptree(prog, locations[elseval->addr]), R_CONSTANT);
      add_triple_to_ptree(prog, va_make_triple(jump_abs, "jump_abs", 1, locval));
      free(locval);
      free(elseval);
    }
  free(locations);
  if (fixups)
    {
      for (i = 0; i < body_len; i++)
	if (fixups[i])
	  {
	    prog->ints[fixups[i]->addr] = prog->triple_ctr;
	    free(fixups[i]);
	  }
      free(fixups);
    }
  if (selval) free(selval);
  if (need_result != DONT_NEED_RESULT)
    return(result);
  return(make_xen_value(R_UNSPECIFIED, -1, R_CONSTANT));

 CASE_ERROR:
  /* try to avoid endless repetition of cleanup code */
  if (selval) free(selval);
  if (fixups)
    {
      for (j = 0; j < body_len; j++) 
	if (fixups[j])
	  free(fixups[j]);
      free(fixups);
    }
  if (locations) free(locations);
  if (result) free(result);
  if (jump_to_selection) free(jump_to_selection);
  return(NULL);
}


static bool list_member(s7_pointer symb, s7_pointer varlst)
{
  s7_pointer lst;
  for (lst = varlst; lst != scheme_nil; lst = s7_cdr(lst))
    if (s7_is_eq(symb, s7_car(lst)))
      return(true);
  return(false);
}


static bool tree_member(s7_pointer varlst, s7_pointer expr)
{
  /* is any member of varlst found in expr */
  /* search expr (assumed to be a list here) for reference to any member of varlst */
  s7_pointer symb;
  if (expr == scheme_nil) return(false);
  symb = s7_car(expr);
  if (s7_is_symbol(symb))
    {
      if (list_member(symb, varlst))
	return(true);
    }
  else
    {
      if ((s7_is_list(s7, symb)) && (tree_member(varlst, symb)))
	return(true);
    }
  return(tree_member(varlst, s7_cdr(expr)));
}


static xen_value *do_warn_of_type_trouble(int var_type, int expr_type, s7_pointer form)
{
  /* 
   * do (or parallel_bind) var that's initialized to int, then treated as float truncates
   *       so (do ((i 0 (+ i 0.5)))...) i is always 0
   *          (run (lambda () (do ((i 0 (+ i 0.5)) (j 0 (1+ j))) ((>= j 3)) (display i)))) -> 000
   */
  xen_value *rv;
  char *temp = NULL;
  rv = run_warn("do variable init and step types differ: %s, init = %s, step = %s",
		temp = s7_object_to_c_string(s7, form),
		type_name(var_type),
		type_name(expr_type));
  if (temp) free(temp);
  return(rv);
}

#define INC_AND_JUMP_MAYBE() \
  (INT_ARG_1)++; \
  if (INT_ARG_1 != INT_ARG_2) \
    pt->pc = pt->program + pt->ints[args[0]]; \
  else pt->pc += 2; 


static void add_f2_inc_and_jump_maybe(int *args, ptree *pt) 
{
  FLOAT_ARG_3 = (FLOAT_ARG_4 + FLOAT_ARG_5);  /* all add 3 */
  INC_AND_JUMP_MAYBE();
}


static void locsig_3f_mono_no_rev_inc_and_jump_maybe(int *args, ptree *pt)        
{
  mus_locsig_mono_no_reverb(CLM_ARG_4, INT_ARG_5, FLOAT_ARG_6);
  INC_AND_JUMP_MAYBE();
}


static void locsig_v_mono_no_rev_inc_and_jump_maybe(int *args, ptree *pt) 
{
  mus_locsig_mono_no_reverb(CLM_ARG_8, INT_ARG_9, FLOAT_ARG_7 * mus_oscil_fm(CLM_ARG_4, FLOAT_ARG_5 + FLOAT_ARG_6));
  INC_AND_JUMP_MAYBE();
}


static void sin_f_add_inc_and_jump_maybe(int *args, ptree *pt) 
{
  FLOAT_ARG_3 = FLOAT_ARG_5 + sin(FLOAT_ARG_4);
  INC_AND_JUMP_MAYBE();
}


static void locsig_v_stereo_inc_and_jump_maybe(int *args, ptree *pt) 
{
  mus_locsig_stereo(CLM_ARG_8, INT_ARG_9, FLOAT_ARG_7 * mus_oscil_fm(CLM_ARG_4, FLOAT_ARG_5 + FLOAT_ARG_6));
  INC_AND_JUMP_MAYBE();
}


static void outa_3f_inc_and_jump_maybe(int *args, ptree *pt)              
{
  mus_out_any_to_file(CLM_ARG_6, INT_ARG_4, 0, FLOAT_ARG_5);
  INC_AND_JUMP_MAYBE();
}


static void outa_multiply_f3_inc_and_jump_maybe(int *args, ptree *pt)     
{
  mus_out_any_to_file(CLM_ARG_6, INT_ARG_4, 0, FLOAT_ARG_5 * FLOAT_ARG_7 * FLOAT_ARG_8);
  INC_AND_JUMP_MAYBE();
}


static void frame_set_0r_inc_and_jump_maybe(int *args, ptree *pt) 
{
  mus_frame_set(CLM_ARG_4, INT_ARG_5, FLOAT_ARG_6);
  INC_AND_JUMP_MAYBE();
}


static void vct_set_f_inc_and_jump_maybe(int *args, ptree *pt) 
{
  VCT_ARG_4->data[INT_ARG_5] = FLOAT_ARG_6; FLOAT_ARG_3 = FLOAT_ARG_6;
  INC_AND_JUMP_MAYBE();
}


static void set_scaler_f_inc_and_jump_maybe(int *args, ptree *pt) 
{
  mus_set_scaler(CLM_ARG_3, FLOAT_ARG_4);
  INC_AND_JUMP_MAYBE();
}


static void outa_multiply_f2_inc_and_jump_maybe(int *args, ptree *pt)     
{
  mus_out_any_to_file(CLM_ARG_6, INT_ARG_4, 0, FLOAT_ARG_5 * FLOAT_ARG_7);
  INC_AND_JUMP_MAYBE();
}


static void outa_polywave_1_mult_inc_and_jump_maybe(int *args, ptree *pt) 
{
  mus_out_any_to_file(CLM_ARG_6, INT_ARG_4, 0, FLOAT_ARG_8 * mus_polywave(CLM_ARG_7, FLOAT_ARG_5));
  INC_AND_JUMP_MAYBE();
}


static void outa_oscil_1_mult_inc_and_jump_maybe(int *args, ptree *pt)    
{
  mus_out_any_to_file(CLM_ARG_6, INT_ARG_4, 0, FLOAT_ARG_8 * mus_oscil_fm(CLM_ARG_7, FLOAT_ARG_5));
  INC_AND_JUMP_MAYBE();
}


static void outa_add_f2_mult_inc_and_jump_maybe(int *args, ptree *pt)     
{
  mus_out_any_to_file(CLM_ARG_6, INT_ARG_4, 0, FLOAT_ARG_8 * (FLOAT_ARG_5 + FLOAT_ARG_7));
  INC_AND_JUMP_MAYBE();
}


static void sound_data_set_f_inc_and_jump_maybe(int *args, ptree *pt) 
{
  SOUND_DATA_ARG_4->data[INT_ARG_5][INT_ARG_6] = FLOAT_ARG_7;
  FLOAT_ARG_3 = FLOAT_ARG_7;
  INC_AND_JUMP_MAYBE();
}


static void outa_add_f3_mult_inc_and_jump_maybe(int *args, ptree *pt)     
{
  mus_out_any_to_file(CLM_ARG_6, INT_ARG_4, 0, FLOAT_ARG_7 * (FLOAT_ARG_5 + FLOAT_ARG_8 + FLOAT_ARG_9));
  INC_AND_JUMP_MAYBE();
}


static void locsig_v_mono_inc_and_jump_maybe(int *args, ptree *pt) 
{
  mus_locsig_mono(CLM_ARG_8, INT_ARG_9, FLOAT_ARG_7 * mus_oscil_fm(CLM_ARG_4, FLOAT_ARG_5 + FLOAT_ARG_6));
}



#define NUM_DO_OPS 17

static opt_ops do_ops[NUM_DO_OPS] = {
  {locsig_v_mono_no_rev, "locsig_v_mono_no_rev", locsig_v_mono_no_rev_inc_and_jump_maybe, "locsig_v_mono_no_rev_inc_and_jump_maybe", NULL, NULL},
  {add_f2, "add_f2", add_f2_inc_and_jump_maybe, "add_f2_inc_and_jump_maybe", NULL, NULL},
  {locsig_3f_mono_no_rev, "locsig_3f_mono_no_rev", locsig_3f_mono_no_rev_inc_and_jump_maybe, "locsig_3f_mono_no_rev_inc_and_jump_maybe", NULL, NULL},
  {sin_f_add, "sin_f_add", sin_f_add_inc_and_jump_maybe, "sin_f_add_inc_and_jump_maybe", NULL, NULL},
  {locsig_v_stereo, "locsig_v_stereo", locsig_v_stereo_inc_and_jump_maybe, "locsig_v_stereo_inc_and_jump_maybe", NULL, NULL},
  {locsig_v_mono, "locsig_v_mono", locsig_v_mono_inc_and_jump_maybe, "locsig_v_mono_inc_and_jump_maybe", NULL, NULL},
  {outa_3f, "outa_3f", outa_3f_inc_and_jump_maybe, "outa_3f_inc_and_jump_maybe", NULL, NULL},
  {outa_multiply_f3, "outa_multiply_f3", outa_multiply_f3_inc_and_jump_maybe, "outa_multiply_f3_inc_and_jump_maybe", NULL, NULL},
  {outa_multiply_f2, "outa_multiply_f2", outa_multiply_f2_inc_and_jump_maybe, "outa_multiply_f2_inc_and_jump_maybe", NULL, NULL},
  {frame_set_0r, "frame_set_0r", frame_set_0r_inc_and_jump_maybe, "frame_set_0r_inc_and_jump_maybe", NULL, NULL}, 
  {vct_set_f, "vct_set_f(1)", vct_set_f_inc_and_jump_maybe, "vct_set_f_inc_and_jump_maybe", NULL, NULL}, 
  {set_scaler_f, "set_scaler_f", set_scaler_f_inc_and_jump_maybe, "set_scaler_f_inc_and_jump_maybe", NULL, NULL},
  {outa_polywave_1_mult, "outa_polywave_1_mult", outa_polywave_1_mult_inc_and_jump_maybe, "outa_polywave_1_mult_inc_and_jump_maybe", NULL, NULL},
  {outa_oscil_1_mult, "outa_oscil_1_mult", outa_oscil_1_mult_inc_and_jump_maybe, "outa_oscil_1_mult_inc_and_jump_maybe", NULL, NULL},
  {outa_add_f2_mult, "outa_add_f2_mult", outa_add_f2_mult_inc_and_jump_maybe, "outa_add_f2_mult_inc_and_jump_maybe", NULL, NULL},
  {outa_add_f3_mult, "outa_add_f3_mult", outa_add_f3_mult_inc_and_jump_maybe, "outa_add_f3_mult_inc_and_jump_maybe", NULL, NULL},
  {sound_data_set_f, "sound_data_set_f", sound_data_set_f_inc_and_jump_maybe, "sound_data_set_f_inc_and_jump_maybe", NULL, NULL},
};


static int find_do_op(triple *prev_op)
{
  int i;
  for (i = 0; i < NUM_DO_OPS; i++)
    if (prev_op->function == do_ops[i].func)
      return(i);
  return(-1);
}


static xen_value *do_form(ptree *prog, s7_pointer form, walk_result_t need_result)
{
  /* (do ([(var val [up])...]) (test [res ...]) [exp ...]): (do () (#t))  */

  xen_value *result = NULL, *test, *expr, *jump_to_test, *jump_to_body, **exprs = NULL;
  s7_pointer var, vars, done, body, results, test_form;
  xen_var *vr;
  int i, locals_loc, varlen, body_loc;
  char *trouble;

  vars = scheme_cadr(form);
  done = scheme_caddr(form);
  if (done == scheme_nil) return(run_warn("do: null test")); 
  body = scheme_cdddr(form);
  test_form = s7_car(done);
  results = s7_cdr(done);

  /* init step vars */
  locals_loc = prog->var_ctr; /* lets can be nested */
  trouble = parallel_binds(prog, vars, "do");
  if (trouble) return(run_warn_with_free(trouble));

  /* jump to end test (location fixed up later) */
  jump_to_test = make_xen_value(R_INT, add_int_to_ptree(prog, 0), R_VARIABLE);
  add_triple_to_ptree(prog, va_make_triple(jump_abs, "jump_abs", 1, jump_to_test));

  /* walk the do body */
  body_loc = prog->triple_ctr;
  expr = walk_sequence(prog, body, DONT_NEED_RESULT, "do");
  if (expr == NULL) /* if do body is empty, expr => #f (not NULL) -- shouldn't NULL trigger run_warn? */
    {
      free(jump_to_test);
      return(NULL);
    }
  free(expr);
  expr = NULL;

  /* now increment the vars (if step-val exists) -- increments are done first (the norm in Scheme) */
  varlen = s7_list_length(s7, vars);
  if (varlen > 0)
    {
      bool sequential = true;
      if (varlen > 1)    /* 0=doesn't matter, 1=no possible non-sequential ref */
	/* the step expr can refer to a previous do local var, but it is to the previous value -- very weird semantics */
	{
	  int loc;
	  s7_pointer varlst = scheme_nil, update = scheme_false;
	  loc = s7_gc_protect(s7, varlst);
	  vars = scheme_cadr(form);
	  varlst = s7_cons(s7, scheme_caar(vars), varlst);
	  for (vars = s7_cdr(vars), i = 1; i < varlen; i++, vars = s7_cdr(vars))
	    {
	      var = s7_car(vars);
	      /* current var is CAR(var), init can be ignored (it would refer to outer var), update is CADDR(var) */
	      /*   we'll scan CADDR for any member of varlst */
	      if ((scheme_cddr(var) != scheme_nil) && 
		  (scheme_caddr(var) != scheme_nil))
		{
		  /* if update null, can't be ref */
		  update = scheme_caddr(var);
		  if (((s7_is_list(s7, update)) && (tree_member(varlst, update))) ||
		      ((s7_is_symbol(update)) && (list_member(update, varlst))))
		    {
		      /* fprintf(stderr, "found seq ref %s\n", s7_object_to_c_string(s7, vars)); */
		      sequential = false;
		      break;
		    }
		}
	      varlst = s7_cons(s7, s7_car(var), varlst);
	    }
	  s7_gc_unprotect_at(s7, loc);
	  if (!sequential)
	    exprs = (xen_value **)calloc(varlen, sizeof(xen_value *));
	}
      for (vars = scheme_cadr(form), i = 0; i < varlen; i++, vars = s7_cdr(vars))
	{
	  var = s7_car(vars);
	  if ((scheme_cddr(var) != scheme_nil) && 
	      (scheme_caddr(var) != scheme_nil))
	    {
	      if ((sequential) && (expr)) free(expr);
	      expr = walk(prog, scheme_caddr(var), NEED_ANY_RESULT);
	      /* (run-eval '(do ((i 0 (1+ i)) (j 0 (1+ i)) (k 0 (hiho k))) ((= i 3)) 0)) */
	      if (expr == NULL)
		{
		  if (exprs) 
		    {
		      int k;
		      for (k = 0; k < i; k++) if (exprs[k]) free(exprs[k]);
		      free(exprs);
		    }
		  free(jump_to_test);
		  return(NULL);
		}

	      if (sequential)
		{
		  vr = find_var_in_ptree(prog, s7_symbol_name(s7_car(var)));

		  if (vr->v->type != expr->type)
		    {
		      if (exprs) 
			{
			  int k;
			  for (k = 0; k < i; k++) if (exprs[k]) free(exprs[k]);
			  free(exprs);
			}
		      free(jump_to_test);
		      return(do_warn_of_type_trouble(vr->v->type, expr->type, var));
		    }

		  if (((expr->type == R_FLOAT) || (expr->type == R_INT)) &&
		      (s7_is_list(s7, scheme_caddr(var))) && /* otherwise no intermediate was generated */
		      (prog->triple_ctr > 0))
		    {
		      /* as in set!, if possible, simply redirect the previous store to us (implicit set) */
		      triple *prev_op;
		      int *addrs;
		      prev_op = prog->program[prog->triple_ctr - 1];
		      addrs = prev_op->args;
		      if ((addrs) && 
			  (addrs[0] == expr->addr))
			{
			  addrs[0] = vr->v->addr; /* redirect the store to us */
			  /* look for simple increment cases */
			  if ((prev_op->function == add_i2) &&
			      (((addrs[1] != addrs[0]) && (prev_op->types[1] == R_CONSTANT) && (prog->ints[addrs[1]] == 1)) ||
			       ((addrs[2] != addrs[0]) && (prev_op->types[2] == R_CONSTANT) && (prog->ints[addrs[2]] == 1))))
			    {
			      prev_op->function = inc_i_1;
			      prev_op->op_name = "inc_i_1";
#if WITH_COUNTERS
			      prev_op->func_loc = get_func_loc(prev_op->function, prev_op->op_name);
#endif
			    }
			}
		      else set_var(prog, vr->v, expr);
		    }
		  else set_var(prog, vr->v, expr); 
		}
	      else 
		{
		  /* here if expr is a variable reference (not a temp or a constant), we need to sequester it,
		   * since the variable in question may be updated before we re-reference it
		   */
		  if (s7_is_symbol(scheme_caddr(var)))
		    {
		      xen_value *temp;
		      vr = find_var_in_ptree(prog, s7_symbol_name(scheme_caddr(var)));
		      temp = add_empty_var_to_ptree(prog, vr->v->type);

		      if (temp->type != expr->type) 
			{
			  free(jump_to_test);
			  return(do_warn_of_type_trouble(temp->type, expr->type, var));
			}
		      set_var(prog, temp, expr);
		      free(expr);
		      expr = NULL;
		      exprs[i] = temp;
		    }
		  else exprs[i] = expr;
		}
	    }
	}
      if ((sequential) && (expr)) free(expr);
      if (!sequential)
	{
	  for (vars = scheme_cadr(form), i = 0; i < varlen; i++, vars = s7_cdr(vars))
	    if (exprs[i])
	      {
		var = s7_car(vars);
		vr = find_var_in_ptree(prog, s7_symbol_name(s7_car(var)));

		if (vr->v->type != exprs[i]->type) 
		  {
		    free(jump_to_test);
		    return(do_warn_of_type_trouble(vr->v->type, exprs[i]->type, var));
		  }
		set_var(prog, vr->v, exprs[i]);
		free(exprs[i]);
	      }
	  free(exprs);
	}
    }

  /* test for repeat */
  prog->ints[jump_to_test->addr] = prog->triple_ctr;
  free(jump_to_test);
  jump_to_test = NULL;

  test = walk(prog, test_form, NEED_ANY_RESULT);
  if (test == NULL) 
    return(NULL);

  if (test->type != R_BOOL) 
    {
      xen_value *rv;
      char *temp = NULL;
      free(test);
      rv = run_warn("do test must be boolean: %s", temp = s7_object_to_c_string(s7, test_form));
      if (temp) free(temp);
      return(rv);
    }

  jump_to_body = make_xen_value(R_INT, add_int_to_ptree(prog, 0), R_VARIABLE);
  prog->ints[jump_to_body->addr] = body_loc;

  /* the other cases, such as geq_i2, don't happen very often in current instruments */

  {
    triple *prev_op;
    prev_op = prog->program[prog->triple_ctr - 1];
    if (prev_op->function == equal_i2)                      /* BOOL_RESULT = (INT_ARG_1 == INT_ARG_2) */
      {
	if (prog->triple_ctr > 1)
	  {
	    triple *p_op;
	    p_op = prog->program[prog->triple_ctr - 2];
	    if ((p_op->function == inc_i_1) &&              /* INT_RESULT++ (op->args[0]) */
		(p_op->args[0] == prev_op->args[1]))
	      {
		/* p_op is inc i0++, prev_op is equal i1 i2, where one of the args is the same as the inc result
		 *   we want the inc'd arg in i1, the other in i2, jump loc in arg0
		 */
		p_op->args[1] = prev_op->args[1];
		p_op->args[2] = prev_op->args[2];
		p_op->args[0] = jump_to_body->addr; 
		p_op->types[0] = R_INT;

		p_op->function = inc_and_jump_if_not_equal;
		p_op->op_name = "inc_and_jump_if_not_equal";
#if WITH_COUNTERS
		p_op->func_loc = get_func_loc(p_op->function, p_op->op_name);
#endif
		/* prev_op has to be left in place -- it is the target of the original (pre increment) jump */

		if (prog->triple_ctr > 1)
		  {
		    triple *p2_op;
		    int loc;
		    p2_op = prog->program[prog->triple_ctr - 3];
		    loc = find_do_op(p2_op);
		    if (loc >= 0)
		      {
			int k, p2_args;
			p2_args = p2_op->num_args;

			p2_op->types = (int *)realloc(p2_op->types, (p2_args + 3) * sizeof(int));
			p2_op->args = (int *)realloc(p2_op->args, (p2_args + 3) * sizeof(int));
			for (k = p2_args - 1; k >= 0; k--)
			  {
			    p2_op->args[k + 3] = p2_op->args[k];
			    p2_op->types[k + 3] = p2_op->types[k];
			  }
			for (k = 0; k < 3; k++)
			  {
			    p2_op->args[k] = p_op->args[k];
			    p2_op->types[k] = p_op->types[k];
			  }
			p2_op->num_args = p2_args + 3;
			p2_op->function = do_ops[loc].mult_func;
			p2_op->op_name = do_ops[loc].mult_func_name;
#if WITH_COUNTERS
			p2_op->func_loc = get_func_loc(p2_op->function, p2_op->op_name);
#endif				  
		      }
		  }
	      }
	  }
	/* change previous instruction to jump_if_not_equal */
	prev_op->function = jump_if_not_equal;              /* if (INT_ARG_1 != INT_ARG_2) goto pt->ints[args[0]] */
	prev_op->args[0] = jump_to_body->addr;
	prev_op->types[0] = R_INT;
	prev_op->op_name = "jump_if_not_equal";
#if WITH_COUNTERS
	prev_op->func_loc = get_func_loc(prev_op->function, prev_op->op_name);
#endif
      }
    else
      {
	if (prev_op->function == not_b)
	  {
	    /* change previous instruction to jump_if_abs */
	    prev_op->function = jump_if_abs;
	    prev_op->args[0] = jump_to_body->addr;
	    prev_op->types[0] = R_INT;
	    prev_op->op_name = "jump_if_abs";
#if WITH_COUNTERS
	    prev_op->func_loc = get_func_loc(prev_op->function, prev_op->op_name);
#endif
	  }
	else
	  {
	    add_triple_to_ptree(prog, va_make_triple(jump_if_not_abs, "jump_if_not_abs", 2, jump_to_body, test));
	  }
      }
  }
  free(jump_to_body);
  free(test);

  /* now the result block */
  if (results != scheme_nil)
    result = walk_sequence(prog, results, NEED_ANY_RESULT, "do");
  else result = make_xen_value(R_BOOL, add_int_to_ptree(prog, (Int)false), R_CONSTANT);
  undefine_locals(prog, locals_loc);
  return(result);
}


static xen_value *callcc_form(ptree *prog, s7_pointer form, walk_result_t need_result)
{
  s7_pointer func_form, continuation_name;
  continuation *c;
  xen_value *v = NULL;

  func_form = scheme_cadr(form);
  continuation_name = s7_car(scheme_cadr(func_form));
  c = add_goto_to_ptree(prog, s7_symbol_name(continuation_name));

  v = walk_sequence(prog, scheme_cddr(func_form), need_result, "call/cc");
  if (v == NULL) return(NULL);

  if (c->result)
    {
      if (v->type != c->result->type)
	{
	  free(v);
	  return(run_warn("call/cc: types differ"));
	}
      if (need_result != DONT_NEED_RESULT)
	set_var(prog, c->result, v);
    }
  if (v) free(v);

  /* fixup the continuation jump, etc */
  prog->ints[c->jump->addr] = prog->triple_ctr;
  if (c->result)
    v = copy_xen_value(c->result);
  else v = make_xen_value(R_BOOL, add_int_to_ptree(prog, (Int)false), R_CONSTANT);
  erase_goto(prog, c->name); /* now access to this depends on a set! somewhere */
  return(v);
}


static xen_value *or_form(ptree *prog, s7_pointer form, walk_result_t ignored)
{
  /* (or ...) returning as soon as not #f seen */
  s7_pointer body;
  xen_value *v = NULL, *result = NULL, *jump_to_end;
  xen_value **fixups;
  int i, j, body_forms;

  body = s7_cdr(form);
  body_forms = s7_list_length(s7, body);
  if (body_forms == 0)                  /* (or) -> #f */
    return(make_xen_value(R_BOOL, add_int_to_ptree(prog, 0), R_CONSTANT));
  fixups = (xen_value **)calloc(body_forms, sizeof(xen_value *));

  for (i = 0; i < body_forms; i++, body = s7_cdr(body))
    {
      v = walk(prog, s7_car(body), NEED_ANY_RESULT);
      if (v == NULL)
	{
	  xen_value *rv;
	  char *temp = NULL;
	  for (j = 0; j < i; j++)
	    if (fixups[j]) free(fixups[j]);
	  free(fixups);
	  rv = run_warn("or: can't handle %s", temp = s7_object_to_c_string(s7, s7_car(body)));
	  if (temp) free(temp);
	  return(rv);
	}

      if ((i == 0) &&
	  (v->constant == R_CONSTANT) &&
	  ((v->type != R_BOOL) || (prog->ints[v->addr] != 0)))
	{
	  free(fixups);
	  return(v);
	}

      if (v->type != R_BOOL)
	v = coerce_to_boolean(prog, v);

      fixups[i] = make_xen_value(R_INT, add_int_to_ptree(prog, 0), R_VARIABLE);
      prog->ints[fixups[i]->addr] = if_jump_opt(prog, fixups[i], v, JUMP_IF_TRUE);

      free(v);
    }

  /* if we fall through, return #f */
  result = make_xen_value(R_BOOL, add_int_to_ptree(prog, 0), R_VARIABLE);
  add_triple_to_ptree(prog, va_make_triple(store_false, "store_false", 1, result));
  jump_to_end = make_xen_value(R_INT, add_int_to_ptree(prog, prog->triple_ctr), R_VARIABLE);
  add_triple_to_ptree(prog, va_make_triple(jump, "jump", 1, jump_to_end));

  /* now fixup all the jumps to end up here */
  for (i = 0; i < body_forms; i++)
    {
      prog->ints[fixups[i]->addr] = prog->triple_ctr - prog->ints[fixups[i]->addr] - 1;
      free(fixups[i]);
    }

  add_triple_to_ptree(prog, va_make_triple(store_true, "store_true", 1, result));
  prog->ints[jump_to_end->addr] = prog->triple_ctr - prog->ints[jump_to_end->addr] - 1;
  free(jump_to_end);
  free(fixups);
  return(result);
}


static xen_value *and_form(ptree *prog, s7_pointer form, walk_result_t ignored)
{
  /* (and ...) returning as soon as #f seen */
  s7_pointer body;
  xen_value *v = NULL, *result = NULL, *jump_to_end;
  xen_value **fixups;
  int i, j, body_forms;

  body = s7_cdr(form);
  body_forms = s7_list_length(s7, body);
  if (body_forms == 0)                  /* (and) -> #t */
    return(make_xen_value(R_BOOL, add_int_to_ptree(prog, 1), R_CONSTANT));
  fixups = (xen_value **)calloc(body_forms, sizeof(xen_value *));

  for (i = 0; i < body_forms; i++, body = s7_cdr(body))
    {
      v = walk(prog, s7_car(body), NEED_ANY_RESULT);
      if (v == NULL)
	{
	  xen_value *rv;
	  char *temp = NULL;
	  for (j = 0; j < i; j++)
	    if (fixups[j]) free(fixups[j]);
	  free(fixups);
	  rv = run_warn("and: can't handle %s", temp = s7_object_to_c_string(s7, s7_car(body)));
	  if (temp) free(temp);
	  return(rv);
	}

      if ((i == 0) &&
	  (v->constant == R_CONSTANT) &&
	  (v->type == R_BOOL) && 
	  (prog->ints[v->addr] == 0))
	{
	  free(fixups);
	  return(v);
	}

      if (v->type != R_BOOL)
	v = coerce_to_boolean(prog, v);
      fixups[i] = make_xen_value(R_INT, add_int_to_ptree(prog, 0), R_VARIABLE);
      prog->ints[fixups[i]->addr] = if_jump_opt(prog, fixups[i], v, JUMP_IF_FALSE);

      free(v);
    }

  result = make_xen_value(R_BOOL, add_int_to_ptree(prog, 1), R_VARIABLE);
  add_triple_to_ptree(prog, va_make_triple(store_true, "store_true", 1, result));
  jump_to_end = make_xen_value(R_INT, add_int_to_ptree(prog, prog->triple_ctr), R_VARIABLE);
  add_triple_to_ptree(prog, va_make_triple(jump, "jump", 1, jump_to_end));

  /* now fixup all the jumps to end up here */
  for (i = 0; i < body_forms; i++)
    {
      prog->ints[fixups[i]->addr] = prog->triple_ctr - prog->ints[fixups[i]->addr] - 1;
      free(fixups[i]);
    }

  add_triple_to_ptree(prog, va_make_triple(store_false, "store_false", 1, result));
  prog->ints[jump_to_end->addr] = prog->triple_ctr - prog->ints[jump_to_end->addr] - 1;
  free(jump_to_end);
  free(fixups);
  return(result);
}


static xen_value *lookup_generalized_set(ptree *prog, s7_pointer accessor, xen_value *arg0, xen_value *arg1, xen_value *arg2, xen_value *new_value);

static xen_value *generalized_set_form(ptree *prog, s7_pointer form)
{
  /* (set! (mus-phase gen) 0.0) */
  s7_pointer settee, setval;
  s7_pointer in_settee;
  xen_value *in_v0 = NULL, *in_v1 = NULL, *in_v2 = NULL, *v = NULL;

  settee = scheme_cadr(form);
  setval = scheme_caddr(form);

  if ((s7_is_list(s7, settee)) &&
      (s7_is_symbol(s7_car(settee))) &&
      (s7_list_length(s7, settee) <= 4))
    {
      v = walk(prog, setval, NEED_ANY_RESULT);
      if (v == NULL)
	{
	  xen_value *rv;
	  char *temp = NULL;
	  rv = run_warn("set!: can't handle: %s", temp = s7_object_to_c_string(s7, setval));
	  if (temp) free(temp);
	  return(rv);
	}

      in_settee = s7_car(settee);    
      if (s7_cdr(settee) != scheme_nil)
	{
	  in_v0 = walk(prog, scheme_cadr(settee), NEED_ANY_RESULT);
	  if ((in_v0) &&
	      (in_v0->type != R_UNSPECIFIED))
	    {
	      if (scheme_cddr(settee) != scheme_nil)
		{
		  in_v1 = walk(prog, scheme_caddr(settee), NEED_ANY_RESULT);
		  if ((in_v1) &&
		      (in_v1->type != R_UNSPECIFIED))
		    {
		      if (scheme_cdddr(settee) != scheme_nil)
			{
			  in_v2 = walk(prog, scheme_cadddr(settee), NEED_ANY_RESULT);
			  if ((in_v2) &&
			      (in_v2->type != R_UNSPECIFIED))
			    return(lookup_generalized_set(prog, in_settee, in_v0, in_v1, in_v2, v));
			}
		      else return(lookup_generalized_set(prog, in_settee, in_v0, in_v1, NULL, v));
		    }
		  else return(lookup_generalized_set(prog, in_settee, in_v0, NULL, NULL, v));
		}
	      else return(lookup_generalized_set(prog, in_settee, in_v0, NULL, NULL, v));
	    }
	}
      else return(lookup_generalized_set(prog, in_settee, NULL, NULL, NULL, v));

      if (v) free(v);
      if (in_v0) free(in_v0);
      if (in_v1) free(in_v1);
      if (in_v2) free(in_v2);
    }

  {
    xen_value *rv;
    char *temp = NULL;
    rv = run_warn("generalized set! for %s not implemented yet", temp = s7_object_to_c_string(s7, settee));
    if (temp) free(temp);
    return(rv);
  }
}


static xen_value *set_form(ptree *prog, s7_pointer form, walk_result_t ignore)
{
  const char *varname = NULL;
  xen_var *var;
  xen_value *v;
  s7_pointer settee, setval, rtnval;

  settee = scheme_cadr(form);
  setval = scheme_caddr(form);

  if (!(s7_is_symbol(settee)))
    return(generalized_set_form(prog, form));
  varname = s7_symbol_name(settee);
  var = find_var_in_ptree(prog, varname);
  if (var == NULL)
    {
      v = add_global_var_to_ptree(prog, settee, &rtnval);
      if (v) 
	{
	  var = find_var_in_ptree(prog, varname);
	  free(v);
	  v = NULL;
	}
    }

  if ((var) && (!(var->unsettable)))
    {
      int val_type, var_type;
      v = walk(prog, setval, NEED_ANY_RESULT);
      if (v == NULL) 
	{
	  xen_value *rv;
	  char *temp = NULL;
	  rv = run_warn("set!: can't handle: %s", temp = s7_object_to_c_string(s7, setval));
	  if (temp) free(temp);
	  return(rv);
	}

      if ((var->v->addr == v->addr) && 
	  (var->v->type == v->type))
	return(v); /* a no-op: (set! a a) */
      val_type = v->type;
      var_type = var->v->type;

      /* two problematic cases: types differ and aren't compatible, or pointer aliasing */
      if (POINTER_P(val_type))
	{
	  free(v);
	  return(run_warn("can't set pointer var (%s) to alias other such var", varname));
	}
      if (val_type != var_type)
	{
	  char *str = NULL, *temp = NULL;
	  /* here #f is ok for pointer val */
 	  if ((val_type == R_GOTO) && 
 	      (var_type == R_BOOL))
	    {
	      var->v->type = R_GOTO;
 	      add_triple_to_ptree(prog, va_make_triple(store_i, "store_i", 2, var->v, v));
	      return(v);
	    }
	  if ((val_type == R_BOOL) &&
	      (POINTER_P(var_type)))
	    {
 	      add_triple_to_ptree(prog, va_make_triple(store_i, "store_i", 2, var->v, v));
	      var->unclean = true;
	      return(v);
	    }

	  /* variables have only one type in this context */
	  run_warn("set! can't change var's type (%s (%s) = %s (%s)): %s", 
		   var->name, 
		   type_name(var->v->type), 
		   str = describe_xen_value(v, prog), 
		   type_name(v->type), 
		   temp = s7_object_to_c_string(s7, form));
	  if (str) free(str);
	  if (temp) free(temp);
	  free(v);
	  return(NULL);
	  /* this limitation could be removed, but is it worth the bother? */
	}

      /* optimize out redundant sets (can be cancelled via trp->no_opt) */
      if (((v->type == R_FLOAT) || (v->type == R_INT)) &&
	  (s7_is_list(s7, setval)) && /* this by itself assumes that any list represents an expression (i.e. a temp in effect)
				   *   but clm-def-struct field references are lists that can be the target of a set
				   *   so we set the no_opt flag when producing the set
				   */
	  (prog->triple_ctr > 0) &&
	  (((triple **)(prog->program))[prog->triple_ctr - 1]->no_opt == false))
	{
	  /* if possible, simply redirect the previous store to us (implicit set) */
	  /* (run '(let ((a 2) (b 1)) (set! a (+ b 1)))) */
	  /* but don't try to optimize increments here: */
	  /* (let ((a 0)) (run (lambda () (and (let ((b 32)) (if (< a 0) (set! a b) (set! a (+ 1 b))) #t) #f))) a) */
	  triple *prev_op;
	  int *addrs;
	  prev_op = prog->program[prog->triple_ctr - 1];
	  addrs = prev_op->args;
	  if ((addrs) && 
	      (addrs[0] == v->addr))
	    {
	      addrs[0] = var->v->addr; /* redirect the store to us */
	      var->unclean = true;
	      free(v);
	      return(copy_xen_value(var->v));

	    }
	}
      set_var(prog, var->v, v);
      var->unclean = true;
      return(v);
    }

  {
    xen_value *rv;
    char *temp = NULL;
    if ((var) && 
	(var->unsettable))
      rv = run_warn("set!: can't set: %s", temp = s7_object_to_c_string(s7, settee));
    else rv = run_warn("set! variable problem: %s", temp = s7_object_to_c_string(s7, form));
    if (temp) free(temp);
    return(rv);
  }
}


static xen_value *package(ptree *prog,
			  int type, 
			  void (*function)(int *arg_addrs, ptree *pt),
			  const char *descr,
			  xen_value **args,
			  int num_args)
{
  args[0] = add_temporary_var_to_ptree(prog, type);
  add_triple_to_ptree(prog, make_triple(function, descr, args, num_args + 1));
  return(args[0]);
}


static xen_value *package_n(ptree *prog,
			    int type, 
			    void (*function)(int *arg_addrs, ptree *pt),
			    const char *descr,
			    xen_value **args,
			    int num_args)
{
  int i;
  xen_value **new_args;
  new_args = (xen_value **)calloc(num_args + 2, sizeof(xen_value *));
  for (i = 1; i <= num_args; i++)
    new_args[i + 1] = args[i];
  new_args[1] = make_xen_value(R_INT, add_int_to_ptree(prog, num_args), R_CONSTANT);
  new_args[0] = add_temporary_var_to_ptree(prog, type);
  args[0] = new_args[0];
  add_triple_to_ptree(prog, make_triple(function, descr, new_args, num_args + 2));
  free(new_args[1]);
  free(new_args);
  return(args[0]);
}


static int float_all_args(ptree *prog, int num_args, xen_value **args, bool float_result)
{
  int i, j;
  for (i = 1, j = 1; i <= num_args; i++)
    if (args[i])
      {
	if ((float_result) && (args[i]->type == R_INT))
	  {
	    xen_value *old_loc;
	    old_loc = args[i];
	    args[i] = NULL;
	    args[j] = make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, 0.0), R_VARIABLE);
	    add_triple_to_ptree(prog, va_make_triple(store_i_f, "store_i_f", 2, args[j], old_loc));
	    free(old_loc);
	    j++;
	  }
	else 
	  {
	    args[j] = args[i];
	    if (j != i) args[i] = NULL;
	    j++;
	  }
      }
  return(j - 1);
}


/* ---------------- multiply ---------------- */

static void multiply_f2(int *args, ptree *pt) {FLOAT_RESULT = (FLOAT_ARG_1 * FLOAT_ARG_2);}


static void multiply_f2_i(int *args, ptree *pt) {INT_RESULT = (Int)(FLOAT_ARG_1 * FLOAT_ARG_2);}


static void multiply_f3(int *args, ptree *pt) {FLOAT_RESULT = (FLOAT_ARG_1 * FLOAT_ARG_2 * FLOAT_ARG_3);}


static void multiply_f4(int *args, ptree *pt) {FLOAT_RESULT = (FLOAT_ARG_1 * FLOAT_ARG_2 * FLOAT_ARG_3 * FLOAT_ARG_4);}
static void multiply_f5(int *args, ptree *pt) {FLOAT_RESULT = (FLOAT_ARG_1 * FLOAT_ARG_2 * FLOAT_ARG_3 * FLOAT_ARG_4 * FLOAT_ARG_5);}


static void multiply_fn(int *args, ptree *pt) 
{
  int i, n;
  n = INT_ARG_1;
  FLOAT_RESULT = FLOAT_ARG_2;
  for (i = 1; i < n; i++) FLOAT_RESULT *= pt->dbls[args[i + 2]];
}


static void multiply_i2(int *args, ptree *pt) {INT_RESULT = (INT_ARG_1 * INT_ARG_2);}


static void multiply_i3(int *args, ptree *pt) {INT_RESULT = (INT_ARG_1 * INT_ARG_2 * INT_ARG_3);}


static void multiply_i4(int *args, ptree *pt) {INT_RESULT = (INT_ARG_1 * INT_ARG_2 * INT_ARG_3 * INT_ARG_4);}
static void multiply_i5(int *args, ptree *pt) {INT_RESULT = (INT_ARG_1 * INT_ARG_2 * INT_ARG_3 * INT_ARG_4 * INT_ARG_5);}


static void multiply_in(int *args, ptree *pt)
{
  int i, n;
  n = INT_ARG_1;
  INT_RESULT = INT_ARG_2;
  for (i = 1; i < n; i++) INT_RESULT *= pt->ints[args[i + 2]];
}


static void multiply_i_f(int *args, ptree *pt) {FLOAT_RESULT = (INT_ARG_1 * FLOAT_ARG_2);}


static void multiply_f_i(int *args, ptree *pt) {FLOAT_RESULT = (FLOAT_ARG_1 * INT_ARG_2);}


static void multiply_f_i_i(int *args, ptree *pt) {INT_RESULT = (Int)(FLOAT_ARG_1 * INT_ARG_2);}




/* 2 arg ops that can be combined into a multiply */
#define NUM_M2_OPS 12

static opt_ops m2_ops[NUM_M2_OPS] = {
  {oscil_0f_1, "oscil_0f", oscil_0f_1_mult, "oscil_0f_mult", oscil_0f_1_env, "oscil_0f_env"},
  {polywave_0f, "polywave_0f", polywave_0f_mult, "polywave_0f_mult", polywave_0f_env, "polywave_0f_env"},
  {env_linear_0f, "env_linear_0f", env_linear_0f_mult, "env_linear_0f_mult", env_linear_0f_env, "env_linear_0f_env"},
  {sin_f, "sin_f", sin_f_mult, "sin_f_mult", NULL, NULL},
  {cos_f, "cos_f", cos_f_mult, "cos_f_mult", NULL, NULL},
  {rand_0f, "rand_0f", rand_0f_mult, "rand_0f_mult", rand_0f_env, "rand_0f_env"},
  {rand_interp_0f, "rand_interp_0f", rand_interp_0f_mult, "rand_interp_0f_mult", rand_interp_0f_env, "rand_interp_0f_env"},
  {abs_f, "abs_f", abs_f_mult, "abs_f_mult", NULL, NULL},
  {sqrt_f, "sqrt_f", sqrt_f_mult, "sqrt_f_mult", NULL, NULL},
  {random_f, "random_f", random_f_mult, "random_f_mult", NULL, NULL},
  {vct_constant_ref_0, "vct_constant_ref_0", vct_constant_ref_0_mult, "vct_constant_ref_0_mult", NULL, NULL},
  {granulate_0f, "granulate_0f", granulate_0f_mult, "granulate_0f_mult", granulate_0f_env, "granulate_0f_env"},
};


static int find_m2_op(triple *prev_op)
{
  int i;
  for (i = 0; i < NUM_M2_OPS; i++)
    if (prev_op->function == m2_ops[i].func)
      return(i);
  return(-1);
}


/* (let ((v (vector (make-oscil 330)))) (run (* 0.5 (oscil (vector-ref v 0))))) */

/* 3 arg ops that can be combined into a multiply */
#define NUM_M3_OPS 12

static opt_ops m3_ops[NUM_M3_OPS] = {
  {oscil_1f_1, "oscil_1f_1", oscil_1f_1_mult, "oscil_1f_1_mult", oscil_1f_1_env, "oscil_1f_1_env"},
  {polywave_1f, "polywave_1f", polywave_1f_mult, "polywave_1f_mult", polywave_1f_env, "polywave_1f_env"},
  {delay_1f, "delay_1f", delay_1f_mult, "delay_1f_mult", delay_1f_env, "delay_1f_env"},
  {delay_1f_noz, "delay_1f_noz", delay_1f_noz_mult, "delay_1f_noz_mult", delay_1f_noz_env, "delay_1f_noz_env"},
  {add_f2, "add_f2", add_f2_mult, "add_f2_mult", NULL, NULL},
  {subtract_f2, "subtract_f2", subtract_f2_mult, "subtract_f2_mult", NULL, NULL},
  {vct_ref_f, "vct_ref_f", vct_ref_f_mult, "vct_ref_f_mult", NULL, NULL},
  {vector_ref_f, "vector_ref_f", vct_ref_f_mult, "vct_ref_f_mult", NULL, NULL},
  {formant_1f, "formant_1f", formant_1f_mult, "formant_1f_mult", formant_1f_env, "formant_1f_env"},
  {firmant_1f, "firmant_1f", firmant_1f_mult, "firmant_1f_mult", firmant_1f_env, "firmant_1f_env"},
  {multiply_f2, "multiply_f2", multiply_f3, "multiply_f3", NULL, NULL},
  {oscil_0f_vect, "oscil_0f_vect", oscil_0f_vect_mult, "oscil_0f_vect_mult", oscil_0f_vect_env, "oscil_0f_vect_env"},
};


static int find_m3_op(triple *prev_op)
{
  int i;
  for (i = 0; i < NUM_M3_OPS; i++)
    if (prev_op->function == m3_ops[i].func)
      return(i);
  return(-1);
}


/* 4 arg ops that can be combined into a multiply */
#define NUM_M4_OPS 12

static opt_ops m4_ops[NUM_M4_OPS] = {
  {multiply_add_f2, "multiply_add_f2", multiply_add_f2_mult, "multiply_add_f2_mult", NULL, NULL},
  {add_f3, "add_f3", add_f3_mult, "add_f3_mult", NULL, NULL},
  {oscil_1f_2, "oscil_1f_2", oscil_1f_2_mult, "oscil_1f_2_mult", oscil_1f_2_env, "oscil_1f_2_env"},
  {oscil_1f_2m, "oscil_1f_2m", oscil_1f_2m_mult, "oscil_1f_2m_mult", oscil_1f_2m_env, "oscil_1f_2m_env"},
  {polywave_1f_2, "polywave_1f_2", polywave_1f_2_mult, "polywave_1f_2_mult", polywave_1f_2_env, "polywave_1f_2_env"},
  {polyshape_1fn, "polyshape_1fn", polyshape_1fn_mult, "polyshape_1fn_mult", polyshape_1fn_env, "polyshape_1fn_env"},
  {subtract_f3, "subtract_f3", subtract_f3_mult, "subtract_f3_mult", NULL, NULL},
  {multiply_f3, "multiply_f3", multiply_f4, "multiply_f4", NULL, NULL},
  {firmant_2f, "firmant_2f", firmant_2f_mult, "firmant_2f_mult", firmant_2f_env, "firmant_2f_env"},
  {oscil_1f_vect, "oscil_1f_vect", oscil_1f_vect_mult, "oscil_1f_vect_mult", oscil_1f_vect_env, "oscil_1f_vect_env"},
  {formant_1f_vect, "formant_1f_vect", formant_1f_vect_mult, "formant_1f_vect_mult", formant_1f_vect_env, "formant_1f_vect_env"},
  {firmant_1f_vect, "firmant_1f_vect", firmant_1f_vect_mult, "firmant_1f_vect_mult", firmant_1f_vect_env, "firmant_1f_vect_env"},
};


static int find_m4_op(triple *prev_op)
{
  int i;
  for (i = 0; i < NUM_M4_OPS; i++)
    if (prev_op->function == m4_ops[i].func)
      return(i);
  return(-1);
}



/* 5 arg ops that can be combined into a multiply */
#define NUM_M5_OPS 3

static opt_ops m5_ops[NUM_M5_OPS] = {
  {oscil_1f_3ma, "oscil_1f_3ma", oscil_1f_3ma_mult, "oscil_1f_3ma_mult", oscil_1f_3ma_env, "oscil_1f_3ma_env"},
  {oscil_1f_3, "oscil_1f_3", oscil_1f_3_mult, "oscil_1f_3_mult", oscil_1f_3_env, "oscil_1f_3_env"},
  {multiply_f4, "multiply_f4", multiply_f5, "multiply_f5", NULL, NULL},
  
};

/* (let ((gen (make-oscil 100)) (e (make-env '(0 0 1 1) :end 10)) (x 2.0) (y 3.0) (z 4.0)) (run (lambda () (* (env e) (oscil gen (+ x y z))))) gen) */

static int find_m5_op(triple *prev_op)
{
  int i;
  for (i = 0; i < NUM_M5_OPS; i++)
    if (prev_op->function == m5_ops[i].func)
      return(i);
  return(-1);
}



static xen_value *multiply(ptree *prog, xen_value **args, int num_args)
{
  if (num_args == 0) return(make_xen_value(R_INT, add_int_to_ptree(prog, 1), R_CONSTANT));
  if (num_args == 1) return(copy_xen_value(args[1]));

  if (prog->constants > 0)
    {
      int i, cons_loc = 0;
      Int iscl = 1;
      Double fscl = 1.0;

      for (i = 1; i <= num_args; i++)
	if (args[i]->constant == R_CONSTANT)
	  {
	    cons_loc = i;
	    if (args[i]->type == R_INT)
	      iscl *= prog->ints[args[i]->addr];
	    else fscl *= prog->dbls[args[i]->addr];
	    free(args[i]);
	    args[i] = NULL;
	  }

      if ((iscl != 1) || (fscl != 1.0))
	{
	  if (prog->float_result)
	    args[cons_loc] = make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, fscl * iscl), R_CONSTANT);
	  else args[cons_loc] = make_xen_value(R_INT, add_int_to_ptree(prog, iscl), R_CONSTANT);
	  if ((iscl == 0) || (fscl == 0.0))
	    return(copy_xen_value(args[cons_loc]));
	}

      if (prog->constants == num_args) 
	{
	  if (args[cons_loc])
	    return(copy_xen_value(args[cons_loc]));
	  else return(make_xen_value(R_INT, add_int_to_ptree(prog, 1), R_CONSTANT));
	}
    }
  else
    {
      if ((num_args == 2) &&
	  (prog->float_result))
	{
	  if (args[1]->type == R_INT)
	    return(package(prog, R_FLOAT, multiply_i_f, "multiply_i_f", args, num_args));
	  if (args[2]->type == R_INT)
	    return(package(prog, R_FLOAT, multiply_f_i, "multiply_f_i", args, num_args));
	}
    }

  num_args = float_all_args(prog, num_args, args, prog->float_result);
  if (num_args == 1) return(copy_xen_value(args[1]));

  if (prog->float_result)
    {
      if (num_args == 2) 
	{
	  /* in instruments, this is the most common operation, coupled with things like envelopes, so it's worth some optimization */
	  if (prog->triple_ctr > 0)
	    {
	      triple *prev_op;
	      prev_op = prog->program[prog->triple_ctr - 1];
	      if (((prev_op->args[0] == args[1]->addr) ||
		   (prev_op->args[0] == args[2]->addr)) &&
		  ((find_var_in_ptree_via_addr(prog, R_FLOAT, prev_op->args[0])) == NULL))
		{
		  int loc;
		  switch (prev_op->num_args)
		    {

		      /* -------- 2 args -------- */
		    case 2:
		      loc = find_m2_op(prev_op);
		      if (loc >= 0)
			{
			  /* its output is either arg1 or arg2, we can use its output loc */
			  if ((prog->triple_ctr > 1) &&
			      (m2_ops[loc].env_func)) /* env is a possibility */
			    {
			      triple *p2_op;
			      p2_op = prog->program[prog->triple_ctr - 2];
			      if ((p2_op->function == env_linear_0f) &&
				  (((p2_op->args[0] == args[1]->addr) && (prev_op->args[0] == args[2]->addr)) ||
				   ((p2_op->args[0] == args[2]->addr) && (prev_op->args[0] == args[1]->addr))) &&
				  ((find_var_in_ptree_via_addr(prog, R_FLOAT, p2_op->args[0])) == NULL))
				{
				  p2_op->types = (int *)realloc(p2_op->types, 3 * sizeof(int));
				  p2_op->args = (int *)realloc(p2_op->args, 3 * sizeof(int));
				  /* env becomes arg 2 */
				  p2_op->args[2] = p2_op->args[1];
				  p2_op->types[2] = p2_op->types[1];
				  p2_op->args[1] = prev_op->args[1];
				  p2_op->types[1] = prev_op->types[1];
				  p2_op->num_args = 3;
				  p2_op->function = m2_ops[loc].env_func;
				  p2_op->op_name = m2_ops[loc].env_func_name;
#if WITH_COUNTERS
				  p2_op->func_loc = get_func_loc(p2_op->function, p2_op->op_name);
#endif				  
				  free_triple(prev_op);
				  prog->triple_ctr--;
				  prog->program[prog->triple_ctr] = NULL;
				  return(make_xen_value(R_FLOAT, p2_op->args[0], R_TEMPORARY));
				}
			    }
			  prev_op->types = (int *)realloc(prev_op->types, 3 * sizeof(int));
			  prev_op->args = (int *)realloc(prev_op->args, 3 * sizeof(int));
			  if (prev_op->args[0] == args[1]->addr)
			    prev_op->args[2] = args[2]->addr;
			  else prev_op->args[2] = args[1]->addr;
			  prev_op->num_args = 3;
			  prev_op->function = m2_ops[loc].mult_func;
			  prev_op->op_name = m2_ops[loc].mult_func_name;
#if WITH_COUNTERS
			  prev_op->func_loc = get_func_loc(prev_op->function, prev_op->op_name);
#endif				  
			  prev_op->types[2] = R_FLOAT;
			  return(make_xen_value(R_FLOAT, prev_op->args[0], R_TEMPORARY));
			}
		      break;

		      /* -------- 3 args -------- */
		    case 3:
		      loc = find_m3_op(prev_op);
		      if (loc >= 0)
			{
			  if ((prog->triple_ctr > 1) &&
			      (m3_ops[loc].env_func)) /* env is a possibility */
			    {
			      triple *p2_op;
			      p2_op = prog->program[prog->triple_ctr - 2];
			      if ((p2_op->function == env_linear_0f) &&
				  (((p2_op->args[0] == args[1]->addr) && (prev_op->args[0] == args[2]->addr)) ||
				   ((p2_op->args[0] == args[2]->addr) && (prev_op->args[0] == args[1]->addr))) &&
				  ((find_var_in_ptree_via_addr(prog, R_FLOAT, p2_op->args[0])) == NULL))
				{
				  p2_op->types = (int *)realloc(p2_op->types, 4 * sizeof(int));
				  p2_op->args = (int *)realloc(p2_op->args, 4 * sizeof(int));
				  /* env becomes arg 3 */
				  p2_op->args[3] = p2_op->args[1];
				  p2_op->types[3] = p2_op->types[1];
				  p2_op->args[1] = prev_op->args[1];
				  p2_op->types[1] = prev_op->types[1];
				  p2_op->args[2] = prev_op->args[2];
				  p2_op->types[2] = prev_op->types[2];
				  p2_op->num_args = 4;
				  p2_op->function = m3_ops[loc].env_func;
				  p2_op->op_name = m3_ops[loc].env_func_name;
#if WITH_COUNTERS
				  p2_op->func_loc = get_func_loc(p2_op->function, p2_op->op_name);
#endif				  
				  free_triple(prev_op);
				  prog->triple_ctr--;
				  prog->program[prog->triple_ctr] = NULL;
				  return(make_xen_value(R_FLOAT, p2_op->args[0], R_TEMPORARY));
				}
			    }
			  prev_op->types = (int *)realloc(prev_op->types, 4 * sizeof(int));
			  prev_op->args = (int *)realloc(prev_op->args, 4 * sizeof(int));
			  if (prev_op->args[0] == args[1]->addr)
			    prev_op->args[3] = args[2]->addr;
			  else prev_op->args[3] = args[1]->addr;
			  prev_op->num_args = 4;
			  prev_op->function = m3_ops[loc].mult_func;
			  prev_op->op_name = m3_ops[loc].mult_func_name;
#if WITH_COUNTERS
			  prev_op->func_loc = get_func_loc(prev_op->function, prev_op->op_name);
#endif				  
			  prev_op->types[3] = R_FLOAT;
			  return(make_xen_value(R_FLOAT, prev_op->args[0], R_TEMPORARY));
			}
		      break;

		      /* -------- 4 args -------- */
		    case 4:
		      loc = find_m4_op(prev_op);
		      if (loc >= 0)
			{
			  if ((prog->triple_ctr > 1) &&
			      (m4_ops[loc].env_func)) /* env is a possibility */
			    {
			      triple *p2_op;
			      p2_op = prog->program[prog->triple_ctr - 2];
			      if ((p2_op->function == env_linear_0f) &&
				  (((p2_op->args[0] == args[1]->addr) && (prev_op->args[0] == args[2]->addr)) ||
				   ((p2_op->args[0] == args[2]->addr) && (prev_op->args[0] == args[1]->addr))) &&
				  ((find_var_in_ptree_via_addr(prog, R_FLOAT, p2_op->args[0])) == NULL))
				{
				  int k;
				  p2_op->types = (int *)realloc(p2_op->types, 5 * sizeof(int));
				  p2_op->args = (int *)realloc(p2_op->args, 5 * sizeof(int));
				  /* env becomes arg 4 */
				  p2_op->args[4] = p2_op->args[1];
				  p2_op->types[4] = p2_op->types[1];
				  for (k = 1; k < 4; k++)
				    {
				      p2_op->args[k] = prev_op->args[k];
				      p2_op->types[k] = prev_op->types[k];
				    }
				  p2_op->num_args = 5;
				  p2_op->function = m4_ops[loc].env_func;
				  p2_op->op_name = m4_ops[loc].env_func_name;
#if WITH_COUNTERS
				  p2_op->func_loc = get_func_loc(p2_op->function, p2_op->op_name);
#endif				  
				  free_triple(prev_op);
				  prog->triple_ctr--;
				  prog->program[prog->triple_ctr] = NULL;
				  return(make_xen_value(R_FLOAT, p2_op->args[0], R_TEMPORARY));
				}
			      /* (let ((gen (make-polyshape)) (e (make-env '(0 0 1 1)))) (run (lambda () (* (env e) (polyshape gen 1.0 1.5))))) */
			    }
			  prev_op->types = (int *)realloc(prev_op->types, 5 * sizeof(int));
			  prev_op->args = (int *)realloc(prev_op->args, 5 * sizeof(int));
			  if (prev_op->args[0] == args[1]->addr)
			    prev_op->args[4] = args[2]->addr;
			  else prev_op->args[4] = args[1]->addr;
			  prev_op->num_args = 5;
			  prev_op->function = m4_ops[loc].mult_func;
			  prev_op->op_name = m4_ops[loc].mult_func_name;
#if WITH_COUNTERS
			  prev_op->func_loc = get_func_loc(prev_op->function, prev_op->op_name);
#endif				  
			  prev_op->types[4] = R_FLOAT;
			  return(make_xen_value(R_FLOAT, prev_op->args[0], R_TEMPORARY));
			}
		      break;

		      /* -------- 5 args -------- */
		    case 5:
		      loc = find_m5_op(prev_op);
		      if (loc >= 0)
			{
			  if ((prog->triple_ctr > 1) &&
			      (m5_ops[loc].env_func)) /* env is a possibility */
			    {
			      triple *p2_op;
			      p2_op = prog->program[prog->triple_ctr - 2];
			      if ((p2_op->function == env_linear_0f) &&
				  (((p2_op->args[0] == args[1]->addr) && (prev_op->args[0] == args[2]->addr)) ||
				   ((p2_op->args[0] == args[2]->addr) && (prev_op->args[0] == args[1]->addr))) &&
				  ((find_var_in_ptree_via_addr(prog, R_FLOAT, p2_op->args[0])) == NULL))
				{
				  int k;
				  p2_op->types = (int *)realloc(p2_op->types, 6 * sizeof(int));
				  p2_op->args = (int *)realloc(p2_op->args, 6 * sizeof(int));
				  /* env becomes arg 5 */
				  p2_op->args[5] = p2_op->args[1];
				  p2_op->types[5] = p2_op->types[1];
				  for (k = 1; k < 5; k++)
				    {
				      p2_op->args[k] = prev_op->args[k];
				      p2_op->types[k] = prev_op->types[k];
				    }
				  p2_op->num_args = 6;
				  p2_op->function = m5_ops[loc].env_func;
				  p2_op->op_name = m5_ops[loc].env_func_name;
#if WITH_COUNTERS
				  p2_op->func_loc = get_func_loc(p2_op->function, p2_op->op_name);
#endif				  
				  free_triple(prev_op);
				  prog->triple_ctr--;
				  prog->program[prog->triple_ctr] = NULL;
				  return(make_xen_value(R_FLOAT, p2_op->args[0], R_TEMPORARY));
				}
			      /* (let ((gen (make-polyshape)) (e (make-env '(0 0 1 1)))) (run (lambda () (* (env e) (polyshape gen 1.0 1.5))))) */
			    }


			  prev_op->types = (int *)realloc(prev_op->types, 6 * sizeof(int));
			  prev_op->args = (int *)realloc(prev_op->args, 6 * sizeof(int));
			  if (prev_op->args[0] == args[1]->addr)
			    prev_op->args[5] = args[2]->addr;
			  else prev_op->args[5] = args[1]->addr;
			  prev_op->num_args = 6;
			  prev_op->function = m5_ops[loc].mult_func;
			  prev_op->op_name = m5_ops[loc].mult_func_name;
#if WITH_COUNTERS
			  prev_op->func_loc = get_func_loc(prev_op->function, prev_op->op_name);
#endif				  
			  prev_op->types[5] = R_FLOAT;
			  return(make_xen_value(R_FLOAT, prev_op->args[0], R_TEMPORARY));
			}
		      break;
		    }
		}
	    }
	  return(package(prog, R_FLOAT, multiply_f2, "multiply_f2", args, num_args));
	}
      if (num_args == 3) return(package(prog, R_FLOAT, multiply_f3, "multiply_f3", args, num_args));
      if (num_args == 4) return(package(prog, R_FLOAT, multiply_f4, "multiply_f4", args, num_args));
      if (num_args == 5) return(package(prog, R_FLOAT, multiply_f5, "multiply_f5", args, num_args));
      return(package_n(prog, R_FLOAT, multiply_fn, "multiply_fn", args, num_args));
    }

  if (num_args == 2) return(package(prog, R_INT, multiply_i2, "multiply_i2", args, num_args)); 
  if (num_args == 3) return(package(prog, R_INT, multiply_i3, "multiply_i3", args, num_args));
  if (num_args == 4) return(package(prog, R_INT, multiply_i4, "multiply_i4", args, num_args));
  if (num_args == 5) return(package(prog, R_INT, multiply_i5, "multiply_i5", args, num_args));
  return(package_n(prog, R_INT, multiply_in, "multiply_in", args, num_args));
}


/* ---------------- add ---------------- */

static void add_f2(int *args, ptree *pt) {FLOAT_RESULT = (FLOAT_ARG_1 + FLOAT_ARG_2);}
static void add_f2_mult(int *args, ptree *pt) {FLOAT_RESULT = FLOAT_ARG_3 * (FLOAT_ARG_1 + FLOAT_ARG_2);}


static void add_f2_i(int *args, ptree *pt) {INT_RESULT = (Int)(FLOAT_ARG_1 + FLOAT_ARG_2);}


static void add_f3(int *args, ptree *pt) {FLOAT_RESULT = (FLOAT_ARG_1 + FLOAT_ARG_2 + FLOAT_ARG_3);}
static void add_f3_mult(int *args, ptree *pt) {FLOAT_RESULT = FLOAT_ARG_4 * (FLOAT_ARG_1 + FLOAT_ARG_2 + FLOAT_ARG_3);}


static void add_f4(int *args, ptree *pt) {FLOAT_RESULT = (FLOAT_ARG_1 + FLOAT_ARG_2 + FLOAT_ARG_3 + FLOAT_ARG_4);}


static void add_f5(int *args, ptree *pt) {FLOAT_RESULT = (FLOAT_ARG_1 + FLOAT_ARG_2 + FLOAT_ARG_3 + FLOAT_ARG_4 + FLOAT_ARG_5);}


static void multiply_add_f2(int *args, ptree *pt) {FLOAT_RESULT = (FLOAT_ARG_1 * FLOAT_ARG_2) + FLOAT_ARG_3;}
static void multiply_add_f2_mult(int *args, ptree *pt) {FLOAT_RESULT = FLOAT_ARG_4 * ((FLOAT_ARG_1 * FLOAT_ARG_2) + FLOAT_ARG_3);}


static void add_fn(int *args, ptree *pt) 
{
  int i, n;
  n = INT_ARG_1;
  FLOAT_RESULT = FLOAT_ARG_2;
  for (i = 1; i < n; i++) FLOAT_RESULT += pt->dbls[args[i + 2]];
}


static void add_i2(int *args, ptree *pt) {INT_RESULT = (INT_ARG_1 + INT_ARG_2);}
static void add_multiply_i2(int *args, ptree *pt) {INT_RESULT = INT_ARG_3 + (INT_ARG_1 * INT_ARG_2);}

static void inc_i_1(int *args, ptree *pt) {(INT_RESULT)++;}


static void add_i3(int *args, ptree *pt) {INT_RESULT = (INT_ARG_1 + INT_ARG_2 + INT_ARG_3);}


static void add_i4(int *args, ptree *pt) {INT_RESULT = (INT_ARG_1 + INT_ARG_2 + INT_ARG_3 + INT_ARG_4);}


static void add_in(int *args, ptree *pt)
{
  int i, n;
  n = INT_ARG_1;
  INT_RESULT = INT_ARG_2;
  for (i = 1; i < n; i++) INT_RESULT += pt->ints[args[i + 2]];
}


static void add_i_f(int *args, ptree *pt) {FLOAT_RESULT = (INT_ARG_1 + FLOAT_ARG_2);}


static void add_f_i(int *args, ptree *pt) {FLOAT_RESULT = (FLOAT_ARG_1 + INT_ARG_2);}


#define NUM_A2_OPS 7

static opt_ops a2_ops[NUM_A2_OPS] = {
  {rand_interp_0f, "rand_interp_0f", rand_interp_0f_add, "rand_interp_0f_add", NULL, NULL},
  {abs_f, "abs_f", abs_f_add, "abs_f_add", NULL, NULL},
  {sin_f, "sin_f", sin_f_add, "sin_f_add", NULL, NULL},
  {cos_f, "cos_f", cos_f_add, "cos_f_add", NULL, NULL},
  {random_f, "random_f", random_f_add, "random_f_add", NULL, NULL},
  {mus_random_f, "mus_random_f", mus_random_f_add, "mus_random_f_add", NULL, NULL},
};


static int find_a2_op(triple *prev_op)
{
  int i;
  for (i = 0; i < NUM_A2_OPS; i++)
    if (prev_op->function == a2_ops[i].func)
      return(i);
  return(-1);
}


#define NUM_A3_OPS 9

static opt_ops a3_ops[NUM_A3_OPS] = {
  {multiply_f2, "multiply_f2", multiply_add_f2, "multiply_add_f2", NULL, NULL},
  {subtract_f2, "subtract_f2", subtract_f2_add, "subtract_f2_add", NULL, NULL},
  {add_f2, "add_f2", add_f3, "add_f3", NULL, NULL},
  {sin_f_mult, "sin_f_mult", sin_f_mult_add, "sin_f_mult_add", NULL, NULL},
  {cos_f_mult, "cos_f_mult", cos_f_mult_add, "cos_f_mult_add", NULL, NULL},
  {abs_f_mult, "abs_f_mult", abs_f_mult_add, "abs_f_mult_add", NULL, NULL},
  {formant_1f, "formant_1f", formant_1f_add, "formant_1f_add", NULL, NULL},
  {vct_ref_f, "vct_ref_f", vct_ref_f_add, "vct_ref_f_add", NULL, NULL},
  {vector_ref_f, "vector_ref_f", vct_ref_f_add, "vct_ref_f_add", NULL, NULL},
};


static int find_a3_op(triple *prev_op)
{
  int i;
  for (i = 0; i < NUM_A3_OPS; i++)
    if (prev_op->function == a3_ops[i].func)
      return(i);
  return(-1);
}


#define NUM_A4_OPS 2

static opt_ops a4_ops[NUM_A4_OPS] = {
  {subtract_f2_mult, "subtract_f2_mult", subtract_f2_mult_add, "subtract_f2_mult_add", NULL, NULL},
  {add_f3, "add_f3", add_f4, "add_f4", NULL, NULL},
};


static int find_a4_op(triple *prev_op)
{
  int i;
  for (i = 0; i < NUM_A4_OPS; i++)
    if (prev_op->function == a4_ops[i].func)
      return(i);
  return(-1);
}


static xen_value *add(ptree *prog, xen_value **args, int num_args)
{
  if (num_args == 0) return(make_xen_value(R_INT, add_int_to_ptree(prog, 0), R_CONSTANT));
  if (num_args == 1) return(copy_xen_value(args[1]));

  if (prog->constants > 0)
    {
      Int iscl = 0;
      Double fscl = 0.0;
      int i, cons_loc = 0;

      for (i = 1; i <= num_args; i++)
	if (args[i]->constant == R_CONSTANT)
	  {
	    cons_loc = i;
	    if (args[i]->type == R_INT)
	      iscl += prog->ints[args[i]->addr];
	    else fscl += prog->dbls[args[i]->addr];
	    free(args[i]);
	    args[i] = NULL;
	  }

      if ((iscl != 0) || (fscl != 0.0))
	{
	  if (prog->float_result)
	    args[cons_loc] = make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, fscl + iscl), R_CONSTANT);
	  else args[cons_loc] = make_xen_value(R_INT, add_int_to_ptree(prog, iscl), R_CONSTANT);
	}

      if (prog->constants == num_args) 
	{
	  if (args[cons_loc])
	    return(copy_xen_value(args[cons_loc]));
	  else return(make_xen_value(R_INT, add_int_to_ptree(prog, 0), R_CONSTANT));
	}
    }
  else
    {
      if ((num_args == 2) &&
	  (prog->float_result))
	{
	  if (args[1]->type == R_INT)
	    return(package(prog, R_FLOAT, add_i_f, "add_i_f", args, num_args));
	  if (args[2]->type == R_INT)
	    return(package(prog, R_FLOAT, add_f_i, "add_f_i", args, num_args));
	}
    }

  num_args = float_all_args(prog, num_args, args, prog->float_result);
  if (num_args == 1) return(copy_xen_value(args[1]));

  if (prog->float_result)
    {
      if (num_args == 2) 
	{
	  /* (run (lambda () (let ((y 1.5) (z 2.3)) (let ((x (* y z))) (+ x 1.0)))))
	   * (run (lambda () (let ((y 1.5) (z 2.3)) (let ((x (* y z))) (+ (* x y) z)))))
	   */
	  if (prog->triple_ctr > 0)
	    {
	      triple *prev_op;
	      prev_op = prog->program[prog->triple_ctr - 1];
	      if (((prev_op->args[0] == args[1]->addr) ||
		   (prev_op->args[0] == args[2]->addr)) &&
		  ((find_var_in_ptree_via_addr(prog, R_FLOAT, prev_op->args[0])) == NULL))
		{
		  int loc;
		  switch (prev_op->num_args)
		    {
		      /* -------- 2 args -------- */
		    case 2:
		      loc = find_a2_op(prev_op);
		      if (loc >= 0)
			{
			  prev_op->types = (int *)realloc(prev_op->types, 3 * sizeof(int));
			  prev_op->args = (int *)realloc(prev_op->args, 3 * sizeof(int));
			  if (prev_op->args[0] == args[1]->addr)
			    prev_op->args[2] = args[2]->addr;
			  else prev_op->args[2] = args[1]->addr;
			  prev_op->num_args = 3;
			  prev_op->function = a2_ops[loc].mult_func; /* " mult" means "add" in this context */
			  prev_op->op_name = a2_ops[loc].mult_func_name;
#if WITH_COUNTERS
			  prev_op->func_loc = get_func_loc(prev_op->function, prev_op->op_name);
#endif				  
			  prev_op->types[2] = R_FLOAT;
			  return(make_xen_value(R_FLOAT, prev_op->args[0], R_TEMPORARY));
			}
		      break;

		      /* -------- 3 args -------- */
		    case 3:
		      loc = find_a3_op(prev_op);
		      if (loc >= 0)
			{
			  prev_op->types = (int *)realloc(prev_op->types, 4 * sizeof(int));
			  prev_op->args = (int *)realloc(prev_op->args, 4 * sizeof(int));
			  if (prev_op->args[0] == args[1]->addr)
			    prev_op->args[3] = args[2]->addr;
			  else prev_op->args[3] = args[1]->addr;
			  prev_op->num_args = 4;
			  prev_op->function = a3_ops[loc].mult_func;
			  prev_op->op_name = a3_ops[loc].mult_func_name;
#if WITH_COUNTERS
			  prev_op->func_loc = get_func_loc(prev_op->function, prev_op->op_name);
#endif				  
			  prev_op->types[3] = R_FLOAT;
			  return(make_xen_value(R_FLOAT, prev_op->args[0], R_TEMPORARY));
			}
		      break;

		      /* -------- 4 args -------- */
		    case 4:
		      loc = find_a4_op(prev_op);
		      if (loc >= 0)
			{
			  prev_op->types = (int *)realloc(prev_op->types, 5 * sizeof(int));
			  prev_op->args = (int *)realloc(prev_op->args, 5 * sizeof(int));
			  if (prev_op->args[0] == args[1]->addr)
			    prev_op->args[4] = args[2]->addr;
			  else prev_op->args[4] = args[1]->addr;
			  prev_op->num_args = 5;
			  prev_op->function = a4_ops[loc].mult_func;
			  prev_op->op_name = a4_ops[loc].mult_func_name;
#if WITH_COUNTERS
			  prev_op->func_loc = get_func_loc(prev_op->function, prev_op->op_name);
#endif				  
			  prev_op->types[4] = R_FLOAT;
			  return(make_xen_value(R_FLOAT, prev_op->args[0], R_TEMPORARY));
			}

		      break;
		    }
		}
#if 0
	      if (((prev_op->args[0] == args[1]->addr) ||
		   (prev_op->args[0] == args[2]->addr)) &&
		  ((find_var_in_ptree_via_addr(prog, R_FLOAT, prev_op->args[0])) == NULL))
		fprintf(stderr, "add skip %s\n", prev_op->op_name);
#endif
	    }
	  return(package(prog, R_FLOAT, add_f2, "add_f2", args, num_args));
	}
      if (num_args == 3) return(package(prog, R_FLOAT, add_f3, "add_f3", args, num_args));
      if (num_args == 4) return(package(prog, R_FLOAT, add_f4, "add_f4", args, num_args));
      if (num_args == 5) return(package(prog, R_FLOAT, add_f5, "add_f5", args, num_args));
      return(package_n(prog, R_FLOAT, add_fn, "add_fn", args, num_args));
    }

  if (num_args == 2) 
    {
      if (prog->triple_ctr > 0)
	{
	  triple *prev_op;
	  prev_op = prog->program[prog->triple_ctr - 1];
	  if ((prev_op->function == multiply_i2) &&
	      ((prev_op->args[0] == args[1]->addr) ||
	       (prev_op->args[0] == args[2]->addr)) &&
	      ((find_var_in_ptree_via_addr(prog, R_INT, prev_op->args[0])) == NULL))
	    {
	      prev_op->types = (int *)realloc(prev_op->types, 4 * sizeof(int));
	      prev_op->args = (int *)realloc(prev_op->args, 4 * sizeof(int));
	      if (prev_op->args[0] == args[1]->addr)
		prev_op->args[3] = args[2]->addr;
	      else prev_op->args[3] = args[1]->addr;
	      prev_op->num_args = 4;
	      prev_op->function = add_multiply_i2;
	      prev_op->op_name = "add_multiply_i2";
#if WITH_COUNTERS
	      prev_op->func_loc = get_func_loc(prev_op->function, prev_op->op_name);
#endif				  
	      prev_op->types[3] = R_INT;
	      return(make_xen_value(R_INT, prev_op->args[0], R_TEMPORARY));
	    }
	}

      return(package(prog, R_INT, add_i2, "add_i2", args, num_args));
    }
  if (num_args == 3) return(package(prog, R_INT, add_i3, "add_i3", args, num_args));
  if (num_args == 4) return(package(prog, R_INT, add_i4, "add_i4", args, num_args));
  return(package_n(prog, R_INT, add_in, "add_in", args, num_args));
}


/* ---------------- subtract ---------------- */

static void subtract_f1(int *args, ptree *pt) {FLOAT_RESULT = -(FLOAT_ARG_1);}


static void subtract_f2(int *args, ptree *pt) {FLOAT_RESULT = (FLOAT_ARG_1 - FLOAT_ARG_2);}
static void subtract_f2_mult(int *args, ptree *pt) {FLOAT_RESULT = FLOAT_ARG_3 * (FLOAT_ARG_1 - FLOAT_ARG_2);}
static void subtract_f2_add(int *args, ptree *pt) {FLOAT_RESULT = FLOAT_ARG_3 + (FLOAT_ARG_1 - FLOAT_ARG_2);}

static void subtract_f2_mult_add(int *args, ptree *pt) {FLOAT_RESULT = FLOAT_ARG_4 + FLOAT_ARG_3 * (FLOAT_ARG_1 - FLOAT_ARG_2);}

static void subtract_f2_i(int *args, ptree *pt) {INT_RESULT = (Int)(FLOAT_ARG_1 - FLOAT_ARG_2);}


static void subtract_f3(int *args, ptree *pt) {FLOAT_RESULT = (FLOAT_ARG_1 - FLOAT_ARG_2 - FLOAT_ARG_3);}
static void subtract_f3_mult(int *args, ptree *pt) {FLOAT_RESULT = FLOAT_ARG_4 * (FLOAT_ARG_1 - FLOAT_ARG_2 - FLOAT_ARG_3);}

static void subtract_f4(int *args, ptree *pt) {FLOAT_RESULT = (FLOAT_ARG_1 - FLOAT_ARG_2 - FLOAT_ARG_3 - FLOAT_ARG_4);}

static void subtract_fn(int *args, ptree *pt) 
{
  int i, n;
  n = INT_ARG_1;
  FLOAT_RESULT = FLOAT_ARG_2;
  for (i = 1; i < n; i++) FLOAT_RESULT -= pt->dbls[args[i + 2]];
}


static void subtract_i1(int *args, ptree *pt) {INT_RESULT = -(INT_ARG_1);}


static void subtract_i2(int *args, ptree *pt) {INT_RESULT = (INT_ARG_1 - INT_ARG_2);}


static void subtract_i3(int *args, ptree *pt) {INT_RESULT = (INT_ARG_1 - INT_ARG_2 - INT_ARG_3);}
static void subtract_i4(int *args, ptree *pt) {INT_RESULT = (INT_ARG_1 - INT_ARG_2 - INT_ARG_3 - INT_ARG_4);}


static void subtract_in(int *args, ptree *pt)
{
  int i, n;
  n = INT_ARG_1;
  INT_RESULT = INT_ARG_2;
  for (i = 1; i < n; i++) INT_RESULT -= pt->ints[args[i + 2]];
}


static void subtract_i_f(int *args, ptree *pt) {FLOAT_RESULT = (INT_ARG_1 - FLOAT_ARG_2);}


static void subtract_f_i(int *args, ptree *pt) {FLOAT_RESULT = (FLOAT_ARG_1 - INT_ARG_2);}

static void subtract_mult_1f(int *args, ptree *pt) {FLOAT_RESULT = (FLOAT_ARG_1 * FLOAT_ARG_2) - FLOAT_ARG_3;}
/* (let ((x 1.0) (y 2.0) (z 3.0)) (run (lambda () (- (* y z) x)))) */
static void subtract_mult_2f(int *args, ptree *pt) {FLOAT_RESULT = FLOAT_ARG_3 - (FLOAT_ARG_1 * FLOAT_ARG_2);}
/* (let ((x 1.0) (y 2.0) (z 3.0)) (run (lambda () (- x (* y z))))) */


static xen_value *subtract(ptree *prog, xen_value **args, int num_args)
{
  if ((num_args == 1) && (args[1]->constant == R_CONSTANT))
    {
      if (args[1]->type == R_INT)
	return(make_xen_value(R_INT, add_int_to_ptree(prog, -(prog->ints[args[1]->addr])), R_CONSTANT));
      else return(make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, -(prog->dbls[args[1]->addr])), R_CONSTANT));
    }

  if (prog->constants > 0)
    {
      Int iscl = 0;
      Double fscl = 0.0;
      int i, cons_loc = 0;

      for (i = 2; i <= num_args; i++)
	if (args[i]->constant == R_CONSTANT)
	  {
	    cons_loc = i;
	    if (args[i]->type == R_INT)
	      iscl += prog->ints[args[i]->addr];
	    else fscl += prog->dbls[args[i]->addr];
	    free(args[i]);
	    args[i] = NULL;
	  }

      if ((iscl != 0) || (fscl != 0.0))
	{
	  if (prog->float_result)
	    args[cons_loc] = make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, fscl + iscl), R_CONSTANT);
	  else args[cons_loc] = make_xen_value(R_INT, add_int_to_ptree(prog, iscl), R_CONSTANT);
	}
      else
	{
	  if ((num_args == 2) && (args[1]->constant != R_CONSTANT)) /* (- arg 0) */
	    return(copy_xen_value(args[1]));
	}

      if (prog->constants == num_args) 
	{
	  if (prog->float_result)
	    {
	      if (args[1]->type == R_INT)
		return(make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, prog->ints[args[1]->addr] - (fscl + iscl)), R_CONSTANT));
	      else return(make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, prog->dbls[args[1]->addr] - (fscl + iscl)), R_CONSTANT));
	    }
	  else return(make_xen_value(R_INT, add_int_to_ptree(prog, prog->ints[args[1]->addr] - iscl), R_CONSTANT));
	}
    }
  else
    {
      if ((num_args == 2) &&
	  (prog->float_result))
	{
	  if (args[1]->type == R_INT)
	    return(package(prog, R_FLOAT, subtract_i_f, "subtract_i_f", args, num_args));
	  if (args[2]->type == R_INT)
	    return(package(prog, R_FLOAT, subtract_f_i, "subtract_f_i", args, num_args));
	}
    }

  num_args = float_all_args(prog, num_args, args, prog->float_result);
  if (prog->float_result)
    {
      if (num_args == 1) return(package(prog, R_FLOAT, subtract_f1, "subtract_f1", args, num_args));
      if (num_args == 2) 
	{
	  if (prog->triple_ctr > 0)
	    {
	      triple *prev_op;
	      prev_op = prog->program[prog->triple_ctr - 1];
	      if ((prev_op->function == multiply_f2) &&
		  ((prev_op->args[0] == args[1]->addr) || (prev_op->args[0] == args[2]->addr)) &&
		  ((find_var_in_ptree_via_addr(prog, R_FLOAT, prev_op->args[0])) == NULL))
		{
		  prev_op->types = (int *)realloc(prev_op->types, 4 * sizeof(int));
		  prev_op->args = (int *)realloc(prev_op->args, 4 * sizeof(int));
		  prev_op->num_args = 4;
		  if (prev_op->args[0] == args[1]->addr)
		    {
		      prev_op->function = subtract_mult_1f;
		      prev_op->op_name = "subtract_mult_1f";
		      prev_op->args[3] = args[2]->addr;
		    }
		  else
		    {
		      prev_op->function = subtract_mult_2f;
		      prev_op->op_name = "subtract_mult_2f";
		      prev_op->args[3] = args[1]->addr;
		    }
		  prev_op->types[3] = R_FLOAT;
#if WITH_COUNTERS
		  prev_op->func_loc = get_func_loc(prev_op->function, prev_op->op_name);
#endif				  
		  return(make_xen_value(R_FLOAT, prev_op->args[0], R_TEMPORARY));
		}
	    }

	return(package(prog, R_FLOAT, subtract_f2, "subtract_f2", args, num_args));
	}
      if (num_args == 3) return(package(prog, R_FLOAT, subtract_f3, "subtract_f3", args, num_args));
      if (num_args == 4) return(package(prog, R_FLOAT, subtract_f4, "subtract_f4", args, num_args));
      return(package_n(prog, R_FLOAT, subtract_fn, "subtract_fn", args, num_args));
    }

  if (num_args == 1) return(package(prog, R_INT, subtract_i1, "subtract_i1", args, num_args));
  if (num_args == 2) return(package(prog, R_INT, subtract_i2, "subtract_i2", args, num_args));
  if (num_args == 3) return(package(prog, R_INT, subtract_i3, "subtract_i3", args, num_args));
  if (num_args == 4) return(package(prog, R_INT, subtract_i4, "subtract_i4", args, num_args));
  return(package_n(prog, R_INT, subtract_in, "subtract_in", args, num_args));
}


/* ---------------- 1+ 1- ---------------- */

static void one_minus_f(int *args, ptree *pt) {FLOAT_RESULT = FLOAT_ARG_1 - 1.0;}


static void one_minus_i(int *args, ptree *pt) {INT_RESULT = INT_ARG_1 - 1;}


static xen_value *one_minus(ptree *prog, xen_value **args, int num_args)
{
  if (prog->constants == 1)
    {
      if (args[1]->type == R_INT)
	return(make_xen_value(R_INT, add_int_to_ptree(prog, prog->ints[args[1]->addr] - 1), R_CONSTANT));
      else return(make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, prog->dbls[args[1]->addr] - 1.0), R_CONSTANT));
    }
  if (args[1]->type == R_INT)
    return(package(prog, R_INT, one_minus_i, "one_minus_i", args, 1));
  else return(package(prog, R_FLOAT, one_minus_f, "one_minus_f", args, 1));
}


static void one_plus_f(int *args, ptree *pt) {FLOAT_RESULT = FLOAT_ARG_1 + 1.0;}


static void one_plus_i(int *args, ptree *pt) {INT_RESULT = INT_ARG_1 + 1;}


static xen_value *one_plus(ptree *prog, xen_value **args, int num_args)
{
  if (prog->constants == 1)
    {
      if (args[1]->type == R_INT)
	return(make_xen_value(R_INT, add_int_to_ptree(prog, prog->ints[args[1]->addr] + 1), R_CONSTANT));
      else return(make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, prog->dbls[args[1]->addr] + 1.0), R_CONSTANT));
    }
  if (args[1]->type == R_INT)
    return(package(prog, R_INT, one_plus_i, "one_plus_i", args, 1));
  else return(package(prog, R_FLOAT, one_plus_f, "one_plus_f", args, 1));
}


/* ---------------- divide ---------------- */

static void divide_f1(int *args, ptree *pt) {FLOAT_RESULT = (1.0 / FLOAT_ARG_1);}


static void divide_if1(int *args, ptree *pt) {INT_RESULT = (Int)(1.0 / FLOAT_ARG_1);}


static void divide_f2(int *args, ptree *pt) {FLOAT_RESULT = (FLOAT_ARG_1 / FLOAT_ARG_2);}


static void divide_f2_i(int *args, ptree *pt) {INT_RESULT = (Int)(FLOAT_ARG_1 / FLOAT_ARG_2);}


static void divide_if2(int *args, ptree *pt) {INT_RESULT = (Int)(FLOAT_ARG_1 / FLOAT_ARG_2);}
	

static void divide_f3(int *args, ptree *pt) {FLOAT_RESULT = (FLOAT_ARG_1 / (FLOAT_ARG_2 * FLOAT_ARG_3));}


static void divide_fn(int *args, ptree *pt) 
{
  int i, n;
  Double divisor = 1.0;
  n = INT_ARG_1;
  for (i = 1; i < n; i++) divisor *= pt->dbls[args[i + 2]];
  FLOAT_RESULT = FLOAT_ARG_2 / divisor;
}


static void divide_i_f(int *args, ptree *pt) {FLOAT_RESULT = (INT_ARG_1 / FLOAT_ARG_2);}


static void divide_f_i(int *args, ptree *pt) {FLOAT_RESULT = (FLOAT_ARG_1 / INT_ARG_2);}


static xen_value *divide(ptree *prog, xen_value **args, int num_args)
{
  if ((num_args == 1) && (args[1]->constant == R_CONSTANT))
    {
      if (prog->walk_result == NEED_INT_RESULT)
	{
	  if (args[1]->type == R_INT)
	    return(make_xen_value(R_INT, add_int_to_ptree(prog, 0), R_CONSTANT));
	  else return(make_xen_value(R_INT, add_int_to_ptree(prog, (Int)((double)1.0 / (prog->dbls[args[1]->addr]))), R_CONSTANT));
	}
      else
	{
	  if (args[1]->type == R_INT)
	    return(make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, (1.0 / (Double)(prog->ints[args[1]->addr]))), R_CONSTANT));
	  else return(make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, (1.0 / (prog->dbls[args[1]->addr]))), R_CONSTANT));
	}
    }

  if (prog->constants > 0)
    {
      int cons_loc = 0;
      Double fscl = 1.0;
      int i;
      for (i = 2; i <= num_args; i++)
	if (args[i]->constant == R_CONSTANT)
	  {
	    cons_loc = i;
	    if (args[i]->type == R_INT)
	      fscl *= (Double)(prog->ints[args[i]->addr]);
	    else fscl *= prog->dbls[args[i]->addr];
	    free(args[i]);
	    args[i] = NULL;
	  }

      if (fscl != 1.0)
	args[cons_loc] = make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, fscl), R_CONSTANT);
      if (prog->constants == num_args) 
	{
	  if (prog->walk_result == NEED_INT_RESULT)
	    {
	      if (args[1]->type == R_INT)
		return(make_xen_value(R_INT, add_int_to_ptree(prog, (Int)((Double)(prog->ints[args[1]->addr]) / fscl)), R_CONSTANT));
	      else return(make_xen_value(R_INT, add_int_to_ptree(prog, (Int)(prog->dbls[args[1]->addr] / fscl)), R_CONSTANT));
	    }
	  else
	    {
	      if (args[1]->type == R_INT)
		return(make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, (Double)(prog->ints[args[1]->addr]) / fscl), R_CONSTANT));
	      else return(make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, prog->dbls[args[1]->addr] / fscl), R_CONSTANT));
	    }
	}

      if ((prog->constants == (num_args - 1)) && (args[1]->constant != R_CONSTANT))
	{
	  /* divisor is a constant */
	  if (fscl == 1.0) return(copy_xen_value(args[1]));
	  if (fscl == 0.0) return(run_warn("division by zero"));
	  if (prog->walk_result == NEED_ANY_RESULT)
	    {
	      /* invert here and use multiply */
	      if (args[cons_loc]) free(args[cons_loc]);
	      args[2] = make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, (Double)(1.0 / fscl)), R_CONSTANT);
	      if (args[1]->type == R_INT) float_all_args(prog, num_args, args, true);
	      return(package(prog, R_FLOAT, multiply_f2, "multiply_f2", args, 2));
	    }
	}
    }
    {
      if ((num_args == 2) &&
	  (prog->float_result))
	{
	  if (args[1]->type == R_INT)
	    return(package(prog, R_FLOAT, divide_i_f, "divide_i_f", args, num_args));
	  if (args[2]->type == R_INT)
	    return(package(prog, R_FLOAT, divide_f_i, "divide_f_i", args, num_args));
	}
    }

  num_args = float_all_args(prog, num_args, args, true);
  if (num_args == 1) 
    {
      if (prog->walk_result == NEED_INT_RESULT)
	return(package(prog, R_INT, divide_if1, "divide_if1", args, num_args));
      else return(package(prog, R_FLOAT, divide_f1, "divide_f1", args, num_args));
    }

  if (num_args == 2) 
    {
      if (prog->walk_result == NEED_INT_RESULT)
	return(package(prog, R_INT, divide_if2, "divide_if2", args, num_args));
      else return(package(prog, R_FLOAT, divide_f2, "divide_f2", args, num_args));
    }

  if (num_args == 3) return(package(prog, R_FLOAT, divide_f3, "divide_f3", args, num_args));
  return(package_n(prog, R_FLOAT, divide_fn, "divide_fn", args, num_args));
}


/* ---------------- rel ops ---------------- */

static void float_rel_constant_args(ptree *prog, int num_args, xen_value **args)
{
  int i;
  for (i = 1; i <= num_args; i++)
    if ((args[i]->constant == R_CONSTANT) && (args[i]->type == R_INT))
      {
	xen_value *old_loc;
	old_loc = args[i];
	args[i] = make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, (Double)(prog->ints[args[i]->addr])), R_CONSTANT);
	free(old_loc);
      }
}


static void float_rel_args(ptree *prog, int num_args, xen_value **args)
{
  int i;
  for (i = 1; i <= num_args; i++)
    if (args[i]->type == R_INT)
      {
	xen_value *old_loc;
	old_loc = args[i];
	args[i] = make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, 0.0), R_VARIABLE);
	add_triple_to_ptree(prog, va_make_triple(store_i_f, "store_i_f", 2, args[i], old_loc));
	free(old_loc);
      }
}


#define REL_OP(CName, SName, COp, FOp) \
static void CName ## _f2(int *args, ptree *pt) {BOOL_RESULT = (Int)(FLOAT_ARG_1 COp FLOAT_ARG_2);} \
static void CName ## _f3(int *args, ptree *pt) {BOOL_RESULT = (Int)((FLOAT_ARG_1 COp FLOAT_ARG_2) && (FLOAT_ARG_2 COp FLOAT_ARG_3));} \
static void CName ## _f4(int *args, ptree *pt) {BOOL_RESULT = (Int)((FLOAT_ARG_1 COp FLOAT_ARG_2) && (FLOAT_ARG_2 COp FLOAT_ARG_3) && (FLOAT_ARG_3 COp FLOAT_ARG_4));} \
static void CName ## _fn(int *args, ptree *pt) \
{ \
  int i, n; \
  n = INT_ARG_1; \
  for (i = 2; i <= n; i++) \
    { \
      BOOL_RESULT = (pt->dbls[args[i]] COp pt->dbls[args[i + 1]]); \
      if (!BOOL_RESULT) break; \
    } \
} \
static void CName ## _i2(int *args, ptree *pt) {BOOL_RESULT = (Int)(INT_ARG_1 COp INT_ARG_2);} \
static void CName ## _i3(int *args, ptree *pt) {BOOL_RESULT = (Int)((INT_ARG_1 COp INT_ARG_2) && (INT_ARG_2 COp INT_ARG_3));} \
static void CName ## _i4(int *args, ptree *pt) {BOOL_RESULT = (Int)((INT_ARG_1 COp INT_ARG_2) && (INT_ARG_2 COp INT_ARG_3) && (INT_ARG_3 COp INT_ARG_4));} \
static void CName ## _in(int *args, ptree *pt) \
{ \
  int i, n; \
  n = INT_ARG_1; \
  for (i = 2; i <= n; i++) \
    { \
      BOOL_RESULT = (Int)(pt->ints[args[i]] COp pt->ints[args[i + 1]]); \
      if (!BOOL_RESULT) break; \
    } \
} \
static xen_value * SName(ptree *prog, bool float_result, xen_value **args, int num_args) \
{ \
 if ((prog->walk_result == DONT_NEED_RESULT) || (num_args <= 1)) \
    return(make_xen_value(R_BOOL, add_int_to_ptree(prog, (Int)true), R_CONSTANT)); \
  if ((prog->constants > 0) && (float_result)) \
    float_rel_constant_args(prog, num_args, args); \
  if (prog->constants > 1) \
    { \
      int i; \
      Int lasti = 0; \
      Double lastf = 0.0; \
      for (i = 1; i < num_args; i++) \
	if (args[i]->constant == R_CONSTANT) \
	  { \
	    if (float_result) \
	      lastf = prog->dbls[args[i]->addr]; \
	    else lasti = prog->ints[args[i]->addr]; \
	    break; \
	  } \
      for (i = i + 1; i <= num_args; i++) \
	if (args[i]->constant == R_CONSTANT) \
	  { \
	    if (float_result) \
	      { \
		if (lastf FOp prog->dbls[args[i]->addr]) \
		  return(make_xen_value(R_BOOL, add_int_to_ptree(prog, (Int)false), R_CONSTANT)); \
		lastf = prog->dbls[args[i]->addr]; \
	      } \
	    else  \
	      { \
		if (lasti FOp prog->ints[args[i]->addr]) \
		  return(make_xen_value(R_BOOL, add_int_to_ptree(prog, (Int)false), R_CONSTANT)); \
		lasti = prog->ints[args[i]->addr]; \
	      } \
	  } \
      if (prog->constants == num_args) \
	return(make_xen_value(R_BOOL, add_int_to_ptree(prog, (Int)true), R_CONSTANT)); \
    } \
  if (float_result) \
    { \
      float_rel_args(prog, num_args, args); \
      if (num_args == 2) return(package(prog, R_BOOL, CName ## _f2, #CName "_f2", args, num_args)); \
      if (num_args == 3) return(package(prog, R_BOOL, CName ## _f3, #CName "_f3", args, num_args)); \
      if (num_args == 4) return(package(prog, R_BOOL, CName ## _f4, #CName "_f4", args, num_args)); \
      return(package_n(prog, R_BOOL, CName ## _fn, #CName "_fn", args, num_args)); \
    } \
  else \
    { \
      if (num_args == 2) return(package(prog, R_BOOL, CName ## _i2, #CName "_i2", args, num_args)); \
      if (num_args == 3) return(package(prog, R_BOOL, CName ## _i3, #CName "_i3", args, num_args)); \
      if (num_args == 4) return(package(prog, R_BOOL, CName ## _i4, #CName "_i4", args, num_args)); \
      return(package_n(prog, R_BOOL, CName ## _in, #CName "_in", args, num_args)); \
    } \
  return(run_warn(#COp " trouble")); \
}

REL_OP(gt, greater_than, >, <=)
REL_OP(geq, greater_than_or_equal, >=, <)
REL_OP(lt, less_than, <, >=)
REL_OP(leq, less_than_or_equal, <=, >)
REL_OP(equal, numbers_equal, ==, !=)


/* ---------------- max ---------------- */

static void max_f2(int *args, ptree *pt) {FLOAT_RESULT = (FLOAT_ARG_1 > FLOAT_ARG_2) ? FLOAT_ARG_1 : FLOAT_ARG_2;}

static void max_f3(int *args, ptree *pt) 
{
  FLOAT_RESULT = (FLOAT_ARG_1 > FLOAT_ARG_2) ? FLOAT_ARG_1 : FLOAT_ARG_2;
  if (FLOAT_ARG_3 > FLOAT_RESULT) FLOAT_RESULT = FLOAT_ARG_3;
}


static void min_f2(int *args, ptree *pt);


static void min_max_f2(int *args, ptree *pt) 
{
  FLOAT_RESULT = (FLOAT_ARG_1 > FLOAT_ARG_2) ? FLOAT_ARG_1 : FLOAT_ARG_2;
  if (FLOAT_ARG_3 < FLOAT_RESULT) FLOAT_RESULT = FLOAT_ARG_3;

  /* (let ((x -2.0) (y 1.0)) (run (lambda () (max (- x) (min x y))))) */
}


static void max_min_f2(int *args, ptree *pt) 
{
  FLOAT_RESULT = (FLOAT_ARG_1 > FLOAT_ARG_2) ? FLOAT_ARG_2 : FLOAT_ARG_1;
  if (FLOAT_ARG_3 > FLOAT_RESULT) FLOAT_RESULT = FLOAT_ARG_3;

  /* (let ((x -2.0) (y 1.0)) (run (lambda () (min (- x) (max x y))))) */
}



static void max_fn(int *args, ptree *pt)
{
  int i, n;
  Double mx;
  n = INT_ARG_1;
  mx = FLOAT_ARG_2;
  for (i = 2; i <= n; i++)
    if (pt->dbls[args[i + 1]] > mx) mx = pt->dbls[args[i + 1]];
  FLOAT_RESULT = mx;
}


static void max_i2(int *args, ptree *pt) {INT_RESULT = (INT_ARG_1 > INT_ARG_2) ? INT_ARG_1 : INT_ARG_2;}

static void max_i3(int *args, ptree *pt) 
{
  INT_RESULT = (INT_ARG_1 > INT_ARG_2) ? INT_ARG_1 : INT_ARG_2;
  if (INT_ARG_3 > INT_RESULT) INT_RESULT = INT_ARG_3;
}


static void max_in(int *args, ptree *pt)
{
  int i, n;
  Int mx;
  n = INT_ARG_1;
  mx = INT_ARG_2;
  for (i = 2; i <= n; i++)
    if (pt->ints[args[i + 1]] > mx) mx = pt->ints[args[i + 1]];
  INT_RESULT = mx;
}


static xen_value *max_1(ptree *prog, xen_value **args, int num_args)
{
  if (num_args == 1) return(copy_xen_value(args[1]));
  if ((prog->constants > 0) && (prog->float_result)) float_rel_constant_args(prog, num_args, args);
  if (prog->constants == num_args)
    {
      int i;
      Double fmx;
      Int imx;
      if (prog->float_result)
	{
	  fmx = prog->dbls[args[1]->addr];
	  for (i = 2; i <= num_args; i++)
	    if (prog->dbls[args[i]->addr] > fmx) fmx = prog->dbls[args[i]->addr];
	  return(make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, fmx), R_CONSTANT));
	}
      else
	{
	  imx = prog->ints[args[1]->addr];
	  for (i = 2; i <= num_args; i++)
	    if (prog->ints[args[i]->addr] > imx) imx = prog->ints[args[i]->addr];
	  return(make_xen_value(R_INT, add_int_to_ptree(prog, imx), R_CONSTANT));
	}
    }
  if (prog->float_result)
    {
      float_rel_args(prog, num_args, args);
      if (num_args == 2) 
	{
	  if (prog->triple_ctr > 0)
	    {
	      triple *prev_op;
	      prev_op = prog->program[prog->triple_ctr - 1];
	      if ((prev_op->function == min_f2) &&
		  ((prev_op->args[0] == args[1]->addr) ||
		   (prev_op->args[0] == args[2]->addr)) &&
		  ((find_var_in_ptree_via_addr(prog, R_FLOAT, prev_op->args[0])) == NULL))
		{
		  prev_op->types = (int *)realloc(prev_op->types, 4 * sizeof(int));
		  prev_op->args = (int *)realloc(prev_op->args, 4 * sizeof(int));
		  if (prev_op->args[0] == args[1]->addr)
		    prev_op->args[3] = args[2]->addr;
		  else prev_op->args[3] = args[1]->addr;
		  prev_op->num_args = 4;
		  prev_op->function = max_min_f2;
		  prev_op->op_name = "max_min_f2";
#if WITH_COUNTERS
		  prev_op->func_loc = get_func_loc(prev_op->function, prev_op->op_name);
#endif				  
		  prev_op->types[3] = R_FLOAT;
		  return(make_xen_value(R_FLOAT, prev_op->args[0], R_TEMPORARY));
		}
	    }
	  return(package(prog, R_FLOAT, max_f2, "max_f2", args, num_args));
	}
      if (num_args == 3) return(package(prog, R_FLOAT, max_f3, "max_f3", args, num_args));
      return(package_n(prog, R_FLOAT, max_fn, "max_fn", args, num_args));
    }
  if (num_args == 2) return(package(prog, R_INT, max_i2, "max_i2", args, num_args));
  if (num_args == 3) return(package(prog, R_INT, max_i3, "max_i3", args, num_args));
  return(package_n(prog, R_INT, max_in, "max_in", args, num_args));
}


/* ---------------- min ---------------- */

static void min_f2(int *args, ptree *pt) {FLOAT_RESULT = (FLOAT_ARG_1 > FLOAT_ARG_2) ? FLOAT_ARG_2 : FLOAT_ARG_1;}

static void min_f3(int *args, ptree *pt) 
{
  FLOAT_RESULT = (FLOAT_ARG_1 > FLOAT_ARG_2) ? FLOAT_ARG_2 : FLOAT_ARG_1;
  if (FLOAT_ARG_3 < FLOAT_RESULT) FLOAT_RESULT = FLOAT_ARG_3;
}


static void min_fn(int *args, ptree *pt)
{
  int i, n;
  Double mx;
  n = INT_ARG_1;
  mx = FLOAT_ARG_2;
  for (i = 2; i <= n; i++)
    if (pt->dbls[args[i + 1]] < mx) mx = pt->dbls[args[i + 1]];
  FLOAT_RESULT = mx;
}


static void min_i2(int *args, ptree *pt) {INT_RESULT = (INT_ARG_1 > INT_ARG_2) ? INT_ARG_2 : INT_ARG_1;}

static void min_i3(int *args, ptree *pt) 
{
  INT_RESULT = (INT_ARG_1 > INT_ARG_2) ? INT_ARG_2 : INT_ARG_1;
  if (INT_ARG_3 < INT_RESULT) INT_RESULT = INT_ARG_3;
}


static void min_in(int *args, ptree *pt)
{
  int i, n;
  Int mx;
  n = INT_ARG_1;
  mx = INT_ARG_2;
  for (i = 2; i <= n; i++)
    if (pt->ints[args[i + 1]] < mx) mx = pt->ints[args[i + 1]];
  INT_RESULT = mx;
}


static xen_value *min_1(ptree *prog, xen_value **args, int num_args)
{
  if (num_args == 1) return(copy_xen_value(args[1]));
  if ((prog->constants > 0) && (prog->float_result)) float_rel_constant_args(prog, num_args, args);
  if (prog->constants == num_args)
    {
      int i;
      Double fmx;
      Int imx;
      if (prog->float_result)
	{
	  fmx = prog->dbls[args[1]->addr];
	  for (i = 2; i <= num_args; i++)
	    if (prog->dbls[args[i]->addr] < fmx) fmx = prog->dbls[args[i]->addr];
	  return(make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, fmx), R_CONSTANT));
	}
      else
	{
	  imx = prog->ints[args[1]->addr];
	  for (i = 2; i <= num_args; i++)
	    if (prog->ints[args[i]->addr] < imx) imx = prog->ints[args[i]->addr];
	  return(make_xen_value(R_INT, add_int_to_ptree(prog, imx), R_CONSTANT));
	}
    }
  if (prog->float_result)
    {
      float_rel_args(prog, num_args, args);
      if (num_args == 2) 
	{
	  if (prog->triple_ctr > 0)
	    {
	      triple *prev_op;
	      prev_op = prog->program[prog->triple_ctr - 1];
	      if ((prev_op->function == max_f2) &&
		  ((prev_op->args[0] == args[1]->addr) ||
		   (prev_op->args[0] == args[2]->addr)) &&
		  ((find_var_in_ptree_via_addr(prog, R_FLOAT, prev_op->args[0])) == NULL))
		{
		  prev_op->types = (int *)realloc(prev_op->types, 4 * sizeof(int));
		  prev_op->args = (int *)realloc(prev_op->args, 4 * sizeof(int));
		  if (prev_op->args[0] == args[1]->addr)
		    prev_op->args[3] = args[2]->addr;
		  else prev_op->args[3] = args[1]->addr;
		  prev_op->num_args = 4;
		  prev_op->function = min_max_f2;
		  prev_op->op_name = "min_max_f2";
#if WITH_COUNTERS
		  prev_op->func_loc = get_func_loc(prev_op->function, prev_op->op_name);
#endif				  
		  prev_op->types[3] = R_FLOAT;
		  return(make_xen_value(R_FLOAT, prev_op->args[0], R_TEMPORARY));
		}
	    }
	  return(package(prog, R_FLOAT, min_f2, "min_f2", args, num_args));
	}
      if (num_args == 3) return(package(prog, R_FLOAT, min_f3, "min_f3", args, num_args));
      return(package_n(prog, R_FLOAT, min_fn, "min_fn", args, num_args));
    }
  if (num_args == 2) return(package(prog, R_INT, min_i2, "min_i2", args, num_args));
  if (num_args == 3) return(package(prog, R_INT, min_i3, "min_i3", args, num_args));
  return(package_n(prog, R_INT, min_in, "min_in", args, num_args));
}


/* ---------------- not ---------------- */

static void not_b(int *args, ptree *pt) {BOOL_RESULT = (Int)(!(INT_ARG_1));}


static xen_value *not_p(ptree *prog, xen_value **args, int num_args)
{
  if (args[1]->type != R_BOOL)
    return(make_xen_value(R_BOOL, add_int_to_ptree(prog, (Int)false), R_CONSTANT)); /* only #f is false so (not anything)->false */
  if (prog->constants == 1)
    return(make_xen_value(R_BOOL, add_int_to_ptree(prog, !(prog->ints[args[1]->addr])), R_CONSTANT));
  return(package(prog, R_BOOL, not_b, "not_b", args, 1));
}


/* ---------------- eq?, eqv?, equal? ---------------- */

static void eq_b(int *args, ptree *pt) {BOOL_RESULT = (Int)(INT_ARG_1 == INT_ARG_2);} /* safe because float arg -> #f below */

static void vct_eq_b(int *args, ptree *pt) {BOOL_RESULT = (Int)(VCT_ARG_1 == VCT_ARG_2);} 

static void list_eq_b(int *args, ptree *pt) {BOOL_RESULT = (Int)(LIST_ARG_1 == LIST_ARG_2);} 

static void sd_eq_b(int *args, ptree *pt) {BOOL_RESULT = (Int)(SOUND_DATA_ARG_1 == SOUND_DATA_ARG_2);} 

static void xen_eq_b(int *args, ptree *pt) {BOOL_RESULT = (Int)s7_is_eq(SCHEME_ARG_1, SCHEME_ARG_2);}

static void clm_eq_b(int *args, ptree *pt) {BOOL_RESULT = (Int)(CLM_ARG_1 == CLM_ARG_2);}

#if USE_SND
static void sampler_eq_b(int *args, ptree *pt) {BOOL_RESULT = (Int)(SAMPLER_ARG_1 == SAMPLER_ARG_2);} /* safe because float arg -> #f below */
#endif

static xen_value *eq_p(ptree *prog, xen_value **args, int num_args)
{
  if ((args[1]->type != args[2]->type) || 
      ((args[1]->type == R_FLOAT) && (prog->constants > 0)) ||
      (args[1]->type == R_STRING) ||
      (args[1]->type == R_FUNCTION))
    return(make_xen_value(R_BOOL, add_int_to_ptree(prog, (Int)false), R_CONSTANT));
  if ((args[1]->type == args[2]->type) &&
      (args[1]->addr == args[2]->addr))
    return(make_xen_value(R_BOOL, add_int_to_ptree(prog, (Int)true), R_CONSTANT)); /* addr same if (eq? var var) -> always #t */
  if (prog->constants == 2) /* this can include xen vals */
    {
      if ((args[1]->type == R_INT) || (args[1]->type == R_BOOL))
	return(make_xen_value(R_BOOL, add_int_to_ptree(prog, prog->ints[args[1]->addr] == prog->ints[args[2]->addr]), R_CONSTANT));
    }
  switch (args[1]->type)
    {
    case R_VCT:          return(package(prog, R_BOOL, vct_eq_b, "vct_eq_b", args, 2));                break;
    case R_SOUND_DATA:   return(package(prog, R_BOOL, sd_eq_b, "sd_eq_b", args, 2));                  break;
    case R_CLM:          return(package(prog, R_BOOL, clm_eq_b, "clm_eq_b", args, 2));                break;
#if USE_SND
    case R_SAMPLER:      return(package(prog, R_BOOL, sampler_eq_b, "sampler_eq_b", args, 2));        break;
#endif
    case R_KEYWORD:
    case R_SYMBOL:       return(package(prog, R_BOOL, xen_eq_b, "xen_eq_b", args, 2));                break;
      /* R_FLOAT and R_FUNCTION -> false above */
      
    default:
      if ((args[1]->type == R_LIST) ||
	  (CLM_STRUCT_P(args[1]->type)))
	return(package(prog, R_BOOL, list_eq_b, "list_eq_b", args, 2));
      break;
    }
  return(package(prog, R_BOOL, eq_b, "eq_b", args, 2));
}


/* -------- eqv/equal -------- */

static void vct_eqv_b(int *args, ptree *pt) {BOOL_RESULT = (Int)(mus_vct_equalp(VCT_ARG_1, VCT_ARG_2));} 

static void sd_eqv_b(int *args, ptree *pt) {BOOL_RESULT = (Int)(sound_data_equalp(SOUND_DATA_ARG_1, SOUND_DATA_ARG_2));} 

static void xen_eqv_b(int *args, ptree *pt) {BOOL_RESULT = (Int)s7_is_eqv(SCHEME_ARG_1, SCHEME_ARG_2);}

static void eqv_fb(int *args, ptree *pt) {BOOL_RESULT = (Int)(FLOAT_ARG_1 == FLOAT_ARG_2);}

static void eqv_clm(int *args, ptree *pt) {BOOL_RESULT = (Int)mus_equalp(CLM_ARG_1, CLM_ARG_2);}


static xen_value *eqv_p(ptree *prog, xen_value **args, int num_args)
{
  if ((args[1]->type != args[2]->type) || 
      (args[1]->type == R_STRING) ||
      (args[1]->type == R_FUNCTION))
    return(make_xen_value(R_BOOL, add_int_to_ptree(prog, (Int)false), R_CONSTANT));
  if (prog->constants == 2)
    {
      if (args[1]->type == R_FLOAT)
	return(make_xen_value(R_BOOL, add_int_to_ptree(prog, prog->dbls[args[1]->addr] == prog->dbls[args[2]->addr]), R_CONSTANT));
      else 
	if ((args[1]->type == R_INT) || (args[1]->type == R_BOOL))
	  return(make_xen_value(R_BOOL, add_int_to_ptree(prog, prog->ints[args[1]->addr] == prog->ints[args[2]->addr]), R_CONSTANT));
    }
  switch (args[1]->type)
    {
    case R_FLOAT:        return(package(prog, R_BOOL, eqv_fb, "eqv_fb", args, 2));                   break;
    case R_CLM:          return(package(prog, R_BOOL, eqv_clm, "eqv_clm", args, 2));                 break;
#if USE_SND
    case R_SAMPLER:      return(package(prog, R_BOOL, sampler_eq_b, "sampler_eq_b", args, 2));       break;
#endif
    case R_VCT:          return(package(prog, R_BOOL, vct_eqv_b, "vct_eqv_b", args, 2));             break;
    case R_SOUND_DATA:   return(package(prog, R_BOOL, sd_eqv_b, "sd_eqv_b", args, 2));               break;
    case R_KEYWORD:
    case R_SYMBOL:       return(package(prog, R_BOOL, xen_eqv_b, "xen_eqv_b", args, 2));             break;
    default:
      if ((args[1]->type == R_LIST) ||
	  (CLM_STRUCT_P(args[1]->type)))
	return(package(prog, R_BOOL, list_eq_b, "list_eqv_b", args, 2));
      break;
    }
  return(package(prog, R_BOOL, eq_b, "eq_b", args, 2));
}


static void xen_equal_b(int *args, ptree *pt) {BOOL_RESULT = (Int)s7_is_equal(s7, SCHEME_ARG_1, SCHEME_ARG_2);}

static void xen_equal_s(int *args, ptree *pt) {BOOL_RESULT = mus_strcmp(STRING_ARG_1, STRING_ARG_2);}


static void list_equal_p(int *args, ptree *pt) 
{
  BOOL_RESULT = (Int)(LIST_ARG_1 == LIST_ARG_2);

  if ((!(BOOL_RESULT)) &&
      (LIST_ARG_1->len == LIST_ARG_2->len) &&
      (LIST_ARG_1->type == LIST_ARG_2->type))
    {
      int i;
      if (LIST_ARG_1->len == 0)
	BOOL_RESULT = true;
      else
	{
	  for (i = 0; i < LIST_ARG_1->len; i++)
	    {
	      int addr1, addr2;
	      addr1 = LIST_ARG_1->vals[i]->addr;
	      addr2 = LIST_ARG_2->vals[i]->addr;

	      switch (LIST_ARG_1->type)
		{
		case R_INT:
		case R_CHAR:
		case R_BOOL:
		  if (pt->ints[addr1] != pt->ints[addr2])
		    return;
		  break;

		case R_FLOAT:
		  if (pt->dbls[addr1] != pt->dbls[addr2])
		    return;
		  break;

		case R_STRING:
		  if (!(mus_strcmp(pt->strs[addr1], pt->strs[addr2])))
		    return;
		  break;

		case R_SYMBOL:
		case R_KEYWORD:
		  if (!(s7_is_equal(s7, pt->xens[addr1], pt->xens[addr2])))
		    return;
		  break;

		default:
		  return;
		  break;
		}
	    }
	  BOOL_RESULT = true;
	}
    }
}


static xen_value *equal_p(ptree *prog, xen_value **args, int num_args)
{
  if (args[1]->type == args[2]->type)
    switch (args[1]->type)
      {
      case R_KEYWORD:
      case R_SYMBOL: 
	return(package(prog, R_BOOL, xen_equal_b, "xen_equal_b", args, 2));
	break;

      case R_STRING:
	return(package(prog, R_BOOL, xen_equal_s, "xen_equal_s", args, 2));
	break;

      case R_LIST:
	/* eqv? == eq? here, but equal? is different, apparently -- this part of Scheme is really ugly! */
	return(package(prog, R_BOOL, list_equal_p, "list_equal?", args, 2));
	break;
	
      }
  return(eqv_p(prog, args, num_args));
}


/* ---------------- odd?, even?, zero?, positive?, negative? ---------------- */

static xen_value *convert_to_int(ptree *pt, xen_value *v)
{
  xen_value *newv;
  if (v->type == R_FLOAT) 
    {
      newv = convert_dbl_to_int(pt, v, true);
      free(v);
      return(newv);
    }
  return(v);
}

static void odd_i(int *args, ptree *pt) {BOOL_RESULT = (Int)(INT_ARG_1 & 1);}


static xen_value *odd_p(ptree *prog, xen_value **args, int num_args)
{
  args[1] = convert_to_int(prog, args[1]);
  if (args[1] == NULL) return(run_warn("odd? can't convert arg"));
  if (prog->constants == 1)
    return(make_xen_value(R_BOOL, add_int_to_ptree(prog, prog->ints[args[1]->addr] & 1), R_CONSTANT));
  return(package(prog, R_BOOL, odd_i, "odd_i", args, 1));
}


static void even_i(int *args, ptree *pt) {BOOL_RESULT = (Int)(!(INT_ARG_1 & 1));}


static xen_value *even_p(ptree *prog, xen_value **args, int num_args)
{
  args[1] = convert_to_int(prog, args[1]);
  if (args[1] == NULL) return(run_warn("even? can't convert arg"));
  if (prog->constants == 1)
    return(make_xen_value(R_BOOL, add_int_to_ptree(prog, (!(prog->ints[args[1]->addr] & 1))), R_CONSTANT));
  return(package(prog, R_BOOL, even_i, "even_i", args, 1));
}


#define INT_POS_P(CName, SName, COp) \
static void CName ## _i(int *args, ptree *pt) {BOOL_RESULT = (Int)(INT_ARG_1 COp 0);} \
static void CName ## _f(int *args, ptree *pt) {BOOL_RESULT = (Int)(FLOAT_ARG_1 COp 0.0);} \
static xen_value *CName ## _p(ptree *prog, xen_value **args, int num_args) \
{ \
  if (prog->constants == 1) \
    { \
      if (args[1]->type == R_INT) \
	return(make_xen_value(R_BOOL, add_int_to_ptree(prog, (prog->ints[args[1]->addr] COp 0)), R_CONSTANT)); \
      else \
      { \
	if (args[1]->type == R_FLOAT) \
	  return(make_xen_value(R_BOOL, add_int_to_ptree(prog, (prog->dbls[args[1]->addr] COp 0.0)), R_CONSTANT)); \
      } \
    } \
  if (args[1]->type == R_FLOAT) \
    return(package(prog, R_BOOL, CName ## _f, #CName "_f", args, 1)); \
  return(package(prog, R_BOOL, CName ## _i, #CName "_i", args, 1)); \
}

INT_POS_P(zero, zero?, ==)
INT_POS_P(positive, positive?, >)
INT_POS_P(negative, negative?, <)


static void single_to_float(ptree *prog, xen_value **args, int num)
{
  xen_value *old_loc;
  old_loc = args[num];
  args[num] = make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, 0.0), R_VARIABLE);
  add_triple_to_ptree(prog, va_make_triple(store_i_f, "store_i_f", 2, args[num], old_loc));
  free(old_loc);
}


/* ---------------- sin, cos, tan ---------------- */

#define FL_OP(CName) \
static void CName ## _f(int *args, ptree *pt) {FLOAT_RESULT = CName(FLOAT_ARG_1);} \
static void CName ## _f_i(int *args, ptree *pt) {FLOAT_RESULT = CName((double)(INT_ARG_1));} \
static xen_value * CName ## _1(ptree *prog, xen_value **args, int num_args) \
{ \
  if (prog->constants == 1) \
    { \
      if (args[1]->type == R_INT) \
	return(make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, CName((Double)(prog->ints[args[1]->addr]))), R_CONSTANT)); \
      else return(make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, CName(prog->dbls[args[1]->addr])), R_CONSTANT)); \
    } \
  if (args[1]->type == R_INT) return(package(prog, R_FLOAT, CName ## _f_i, #CName "_f_i", args, 1)); \
  return(package(prog, R_FLOAT, CName ## _f, #CName "_f", args, 1)); \
}

/* only collapsible funcs here -- if constant arg, constant val (not random!) */

#ifdef _MSC_VER
double asinh(double x);
double acosh(double x);
double atanh(double x);
#endif

FL_OP(mus_radians_to_hz)
FL_OP(mus_hz_to_radians)
FL_OP(mus_degrees_to_radians)
FL_OP(mus_radians_to_degrees)
FL_OP(mus_db_to_linear)
FL_OP(mus_linear_to_db)
FL_OP(sin)
FL_OP(cos)
FL_OP(tan)
FL_OP(atan)
FL_OP(asin)
FL_OP(acos)
FL_OP(sqrt)
FL_OP(log)
FL_OP(exp)
FL_OP(cosh)
FL_OP(sinh)
FL_OP(tanh)
FL_OP(acosh)
FL_OP(asinh)
FL_OP(atanh)

#if HAVE_SPECIAL_FUNCTIONS
  FL_OP(erf)
  FL_OP(lgamma)
  FL_OP(erfc)
  FL_OP(j0)
  FL_OP(j1)
  FL_OP(y0)
  FL_OP(y1)
#endif
FL_OP(mus_bessi0)

static void sin_f_mult(int *args, ptree *pt) {FLOAT_RESULT = FLOAT_ARG_2 * sin(FLOAT_ARG_1);}
static void cos_f_mult(int *args, ptree *pt) {FLOAT_RESULT = FLOAT_ARG_2 * cos(FLOAT_ARG_1);}
static void sqrt_f_mult(int *args, ptree *pt) {FLOAT_RESULT = FLOAT_ARG_2 * sqrt(FLOAT_ARG_1);}

static void sin_f_mult_add(int *args, ptree *pt) {FLOAT_RESULT = FLOAT_ARG_3 + FLOAT_ARG_2 * sin(FLOAT_ARG_1);}
static void cos_f_mult_add(int *args, ptree *pt) {FLOAT_RESULT = FLOAT_ARG_3 + FLOAT_ARG_2 * cos(FLOAT_ARG_1);}

static void sin_f_add(int *args, ptree *pt) {FLOAT_RESULT = FLOAT_ARG_2 + sin(FLOAT_ARG_1);}
static void cos_f_add(int *args, ptree *pt) {FLOAT_RESULT = FLOAT_ARG_2 + cos(FLOAT_ARG_1);}

static void atan2_f(int *args, ptree *pt) {FLOAT_RESULT = atan2(FLOAT_ARG_1, FLOAT_ARG_2);}


static xen_value *atan2_1(ptree *prog, xen_value **args, int num_args)
{
  xen_value *temp;
  if (args[1]->type == R_INT)
    {
      temp = args[1];
      args[1] = convert_int_to_dbl(prog, args[1]);
      free(temp);
    }
  if (args[2]->type == R_INT)
    {
      temp = args[2];
      args[2] = convert_int_to_dbl(prog, args[2]);
      free(temp);
    }
  if (prog->constants == 2)
    return(make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, atan2(prog->dbls[args[1]->addr], prog->dbls[args[2]->addr])), R_CONSTANT));
  return(package(prog, R_FLOAT, atan2_f, "atan2_f", args, 2));
}


static void fmod_f(int *args, ptree *pt) 
{
  FLOAT_RESULT = fmod(FLOAT_ARG_1, FLOAT_ARG_2);
  if (((FLOAT_ARG_2 > 0.0) && (FLOAT_RESULT < 0.0)) ||
      ((FLOAT_ARG_2 < 0.0) && (FLOAT_RESULT > 0.0)))
    FLOAT_RESULT += FLOAT_ARG_2;
}


static xen_value *fmod_1(ptree *prog, xen_value **args, int num_args)
{
  xen_value *temp;
  if (args[1]->type == R_INT)
    {
      temp = args[1];
      args[1] = convert_int_to_dbl(prog, args[1]);
      free(temp);
    }
  if (args[2]->type == R_INT)
    {
      temp = args[2];
      args[2] = convert_int_to_dbl(prog, args[2]);
      free(temp);
    }
  if (prog->constants == 2)
    {
      double x, y, val;
      x = prog->dbls[args[1]->addr];
      y = prog->dbls[args[2]->addr];
      val = fmod(x, y);
      if (((y > 0.0) && (val < 0.0)) ||
	  ((y < 0.0) && (val > 0.0)))
	val += y;
      return(make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, val), R_CONSTANT));
    }
  return(package(prog, R_FLOAT, fmod_f, "fmod_f", args, 2));
}


#if HAVE_SPECIAL_FUNCTIONS
static void jn_f(int *args, ptree *pt) {FLOAT_RESULT = jn(INT_ARG_1, FLOAT_ARG_2);}


static xen_value *jn_1(ptree *prog, xen_value **args, int num_args)
{
  xen_value *temp;
  if (args[2]->type == R_INT)
    {
      temp = args[2];
      args[2] = convert_int_to_dbl(prog, args[2]);
      free(temp);
    }
  if (prog->constants == 2)
    return(make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, jn(prog->ints[args[1]->addr], prog->dbls[args[2]->addr])), R_CONSTANT));
  return(package(prog, R_FLOAT, jn_f, "jn_f", args, 2));
}


static void yn_f(int *args, ptree *pt) {FLOAT_RESULT = yn(INT_ARG_1, FLOAT_ARG_2);}


static xen_value *yn_1(ptree *prog, xen_value **args, int num_args)
{
  xen_value *temp;
  if (args[2]->type == R_INT)
    {
      temp = args[2];
      args[2] = convert_int_to_dbl(prog, args[2]);
      free(temp);
    }
  if (prog->constants == 2)
    return(make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, yn(prog->ints[args[1]->addr], prog->dbls[args[2]->addr])), R_CONSTANT));
  return(package(prog, R_FLOAT, yn_f, "yn_f", args, 2));
}
#endif



static void seconds_to_samples_i(int *args, ptree *pt) {INT_RESULT = mus_seconds_to_samples(FLOAT_ARG_1);}


static xen_value *seconds_to_samples_1(ptree *prog, xen_value **args, int num_args)
{
  xen_value *temp;
  if (prog->constants == 1)
    {
      if (args[1]->type == R_INT)
	return(make_xen_value(R_INT, add_int_to_ptree(prog, mus_seconds_to_samples((mus_float_t)(prog->ints[args[1]->addr]))), R_CONSTANT));
      return(make_xen_value(R_INT, add_int_to_ptree(prog, mus_seconds_to_samples(prog->dbls[args[1]->addr])), R_CONSTANT));
    }

  if (args[1]->type == R_INT)
    {
      temp = args[1];
      args[1] = convert_int_to_dbl(prog, args[1]);
      free(temp);
    }
  return(package(prog, R_INT, seconds_to_samples_i, "seconds_to_samples_i", args, 1));
}




/* ---------------- round ---------------- */

static Double f_round(Double x)
{
  /* tricky here -- if .5 diff, need to round to nearest even int */
  double plus_half = x + 0.5;
  double result = floor(plus_half);
  return((plus_half == result) && ((plus_half / 2) != floor(plus_half / 2)) ? result - 1 : result);
}

static void round_i(int *args, ptree *pt) {INT_RESULT = (Int)f_round(FLOAT_ARG_1);}

static xen_value *round_1(ptree *prog, xen_value **args, int num_args)
{
  /* (round 1) -> 1.0! */
  if (args[1]->type == R_INT)
    return(copy_xen_value(args[1]));

  if (prog->constants == 1)
    return(make_xen_value(R_INT, add_int_to_ptree(prog, (Int)f_round(prog->dbls[args[1]->addr])), R_CONSTANT));
  return(package(prog, R_INT, round_i, "round_i", args, 1));
}


/* ---------------- truncate ---------------- */

static Double f_truncate(Double x)
{
  if (x < 0.0)
    return(-floor(-x));
  return(floor(x));
}

static void truncate_i(int *args, ptree *pt) {INT_RESULT = (Int)f_truncate(FLOAT_ARG_1);}


static xen_value *truncate_1(ptree *prog, xen_value **args, int num_args)
{
  if (args[1]->type == R_INT)
    return(copy_xen_value(args[1]));
  if (prog->constants == 1)
    return(make_xen_value(R_INT, add_int_to_ptree(prog, (Int)f_truncate(prog->dbls[args[1]->addr])), R_CONSTANT));
  return(package(prog, R_INT, truncate_i, "truncate_i", args, 1));
}

/* ---------------- floor ---------------- */

static void floor_i(int *args, ptree *pt) {INT_RESULT = (Int)floor(FLOAT_ARG_1);}


static xen_value *floor_1(ptree *prog, xen_value **args, int num_args)
{
  if (args[1]->type == R_INT)
    return(copy_xen_value(args[1]));

  if (prog->constants == 1)
    return(make_xen_value(R_INT, add_int_to_ptree(prog, (Int)floor(prog->dbls[args[1]->addr])), R_CONSTANT));

  if (prog->triple_ctr > 0)
    {
      triple *prev_op;
      bool happy = false;
      prev_op = prog->program[prog->triple_ctr - 1];
      if ((prev_op->args[0] == args[1]->addr) &&
	  (prev_op->types[0] == R_FLOAT) &&
	  (find_var_in_ptree_via_addr(prog, R_FLOAT, prev_op->args[0])) == NULL)
	{
	  if (prev_op->function == multiply_f2)
	    {
	      prev_op->function = multiply_f2_i;
	      prev_op->op_name = "multiply_f2_i";
	      happy = true;
	    }
	  else
	    {
	      if (prev_op->function == multiply_f_i)
		{
		  prev_op->function = multiply_f_i_i;
		  prev_op->op_name = "multiply_f_i_i";
		  happy = true;
		}
	      else
		{
		  if (prev_op->function == divide_f2)
		    {
		      prev_op->function = divide_f2_i;
		      prev_op->op_name = "divide_f2_i";
		      happy = true;
		    }
		  else
		    {
		      if (prev_op->function == subtract_f2)
			{
			  prev_op->function = subtract_f2_i;
			  prev_op->op_name = "subtract_f2_i";
			  happy = true;
			}
		      else
			{
			  if (prev_op->function == add_f2)
			    {
			      prev_op->function = add_f2_i;
			      prev_op->op_name = "add_f2_i";
			      happy = true;
			    }}}}}
	  if (happy)
	    {
	      args[0] = add_temporary_var_to_ptree(prog, R_INT);
	      prev_op->types[0] = R_INT;
	      prev_op->args[0] = args[0]->addr;
#if WITH_COUNTERS
	      prev_op->func_loc = get_func_loc(prev_op->function, prev_op->op_name);
#endif				  
	      return(args[0]);
	    }
	}
    }

  return(package(prog, R_INT, floor_i, "floor_i", args, 1));
}


/* ---------------- ceiling ---------------- */

static void ceiling_i(int *args, ptree *pt) {INT_RESULT = (Int)ceil(FLOAT_ARG_1);}


static xen_value *ceiling_1(ptree *prog, xen_value **args, int num_args)
{
  if (args[1]->type == R_INT)
    return(copy_xen_value(args[1]));
  if (prog->constants == 1)
    return(make_xen_value(R_INT, add_int_to_ptree(prog, (Int)ceil(prog->dbls[args[1]->addr])), R_CONSTANT));
  return(package(prog, R_INT, ceiling_i, "ceiling_i", args, 1));
}


/* ---------------- exact->inexact ---------------- */

static xen_value *exact2inexact_1(ptree *prog, xen_value **args, int num_args)
{
  if (args[1]->type == R_FLOAT)
    return(copy_xen_value(args[1]));
  if (prog->constants == 1)
    return(make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, (Double)(prog->ints[args[1]->addr])), R_CONSTANT));
  return(package(prog, R_FLOAT, store_i_f, "store_i_f", args, 1));
}


static void i2e_i(int *args, ptree *pt) {INT_RESULT = (Int)floor(FLOAT_ARG_1 + 0.5);}


static xen_value *inexact2exact_1(ptree *prog, xen_value **args, int num_args)
{
  if (args[1]->type == R_INT)
    return(copy_xen_value(args[1]));
  if (prog->constants == 1)
    return(make_xen_value(R_INT, add_int_to_ptree(prog, (Int)floor(prog->dbls[args[1]->addr] + 0.5)), R_CONSTANT));
  return(package(prog, R_INT, i2e_i, "i2e_i", args, 1));
}


/* ---------------- gcd, lcm ---------------- */

static Int c_mod(Int x, Int y)
{
  mus_long_t z;
  if (y == 0) return(x); /* else arithmetic exception */
  z = x % y;
  if (((y < 0) && (z > 0)) ||
      ((y > 0) && (z < 0)))
    return(z + y);
  return(z);
}

static mus_long_t c_gcd(mus_long_t a, mus_long_t b)
{
  while (b != 0)
    {
      mus_long_t temp;
      temp = b;
      b = a % b;
      a = temp;
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


static void gcd_in(int *args, ptree *pt)
{
  int i, n;
  Int mx;
  n = INT_ARG_1;
  mx = c_gcd(INT_ARG_2, INT_ARG_3);
  /* this could be optimized... */
  for (i = 3; i <= n; i++)
    {
      if (mx == 1) break;
      mx = c_gcd(mx, pt->ints[args[i + 1]]);
    }
  INT_RESULT = mx;
}


static void gcd_i(int *args, ptree *pt) {INT_RESULT = c_gcd(INT_ARG_1, INT_ARG_2);}


static xen_value *gcd_1(ptree *prog, xen_value **args, int num_args)
{
  int i;
  if (num_args == 0)
    return(make_xen_value(R_INT, add_int_to_ptree(prog, 0), R_CONSTANT)); /* (gcd) -> 0 */

  for (i = 1; i <= num_args; i++)
    {
      args[i] = convert_to_int(prog, args[i]);
      if (args[i] == NULL) return(run_warn("gcd can't convert to int"));
    }

  if (num_args == 1)
    {
      if (args[1]->constant == R_CONSTANT)
	return(make_xen_value(R_INT, add_int_to_ptree(prog, abs(prog->ints[args[1]->addr])), R_CONSTANT)); /* (gcd n) -> (abs n) */
      else 
	{
	  args[0] = make_xen_value(R_INT, add_int_to_ptree(prog, 0), R_VARIABLE);
	  add_triple_to_ptree(prog, make_triple(store_i, "store_i", args, 2));
	  return(args[0]);
	}
    }

  if (prog->constants == num_args)
    {
      Int mx;
      mx = c_gcd(prog->ints[args[1]->addr], prog->ints[args[2]->addr]);
      for (i = 3; i <= num_args; i++)
	{
	  if (mx == 1) break;
	  mx = c_gcd(mx, prog->ints[args[i]->addr]);
	}
      return(make_xen_value(R_INT, add_int_to_ptree(prog, mx), R_CONSTANT));
    }

  if (num_args == 2)
    return(package(prog, R_INT, gcd_i, "gcd_i", args, 2));
  return(package_n(prog, R_INT, gcd_in, "gcd_in", args, num_args));
}


static void lcm_i(int *args, ptree *pt) {INT_RESULT = c_lcm(INT_ARG_1, INT_ARG_2);}


static void lcm_in(int *args, ptree *pt)
{
  int i, n;
  Int mx;
  n = INT_ARG_1;
  mx = c_lcm(INT_ARG_2, INT_ARG_3);
  /* this could be optimized... */
  for (i = 3; i <= n; i++)
    {
      if (mx == 0) break;
      mx = c_lcm(mx, pt->ints[args[i + 1]]);
    }
  INT_RESULT = mx;
}


static xen_value *lcm_1(ptree *prog, xen_value **args, int num_args)
{
  int i;
  Int mx;
  if (num_args == 0)
    return(make_xen_value(R_INT, add_int_to_ptree(prog, 1), R_CONSTANT)); /* (lcm) -> 1 */

  for (i = 1; i <= num_args; i++)
    {
      args[i] = convert_to_int(prog, args[i]);
      if (args[i] == NULL) return(run_warn("lcm can't convert to int"));
    }

  if (num_args == 1)
    {
      if (args[1]->constant == R_CONSTANT)
	return(make_xen_value(R_INT, add_int_to_ptree(prog, abs(prog->ints[args[1]->addr])), R_CONSTANT)); /* (lcm n) -> (abs n) */
      else 
	{
	  args[0] = make_xen_value(R_INT, add_int_to_ptree(prog, 0), R_VARIABLE);
	  add_triple_to_ptree(prog, make_triple(store_i, "store_i", args, 2));
	  return(args[0]);
	}
    }

  if (prog->constants == num_args)
    {
      mx = c_lcm(prog->ints[args[1]->addr], prog->ints[args[2]->addr]);
      for (i = 3; i <= num_args; i++)
	{
	  if (mx == 0) break;
	  mx = c_lcm(mx, prog->ints[args[i]->addr]);
	}
      return(make_xen_value(R_INT, add_int_to_ptree(prog, mx), R_CONSTANT));
    }

  if (num_args == 2)
    return(package(prog, R_INT, lcm_i, "lcm_i", args, 2));
  return(package_n(prog, R_INT, lcm_in, "lcm_in", args, num_args));
}


/* ---------------- remainder, quotient, modulo ---------------- */

static void modulo_i(int *args, ptree *pt) {INT_RESULT = c_mod(INT_ARG_1, INT_ARG_2);}


static xen_value *modulo_1(ptree *prog, xen_value **args, int num_args)
{
  if ((args[1]->type == R_FLOAT) ||
      (args[2]->type == R_FLOAT))
    return(fmod_1(prog, args, num_args));

  args[1] = convert_to_int(prog, args[1]);
  if (args[1] == NULL) return(run_warn("modulo arg1 can't convert to int"));
  args[2] = convert_to_int(prog, args[2]);
  if (args[2] == NULL) return(run_warn("modulo arg2 can't convert to int"));
  if (prog->constants == 2)
    return(make_xen_value(R_INT, add_int_to_ptree(prog, c_mod(prog->ints[args[1]->addr], prog->ints[args[2]->addr])), R_CONSTANT));
  return(package(prog, R_INT, modulo_i, "modulo_i", args, 2));
}


static void remainder_i(int *args, ptree *pt) {INT_RESULT = (INT_ARG_1 % INT_ARG_2);}


static xen_value *remainder_1(ptree *prog, xen_value **args, int num_args)
{
  args[1] = convert_to_int(prog, args[1]);
  if (args[1] == NULL) return(run_warn("remainder arg1 can't convert to int"));
  args[2] = convert_to_int(prog, args[2]);
  if (args[2] == NULL) return(run_warn("remainder arg2 can't convert to int"));
  if (prog->constants == 2)
    return(make_xen_value(R_INT, add_int_to_ptree(prog, (prog->ints[args[1]->addr] % prog->ints[args[2]->addr])), R_CONSTANT));
  return(package(prog, R_INT, remainder_i, "remainder_i", args, 2));
}


static void quotient_i(int *args, ptree *pt) {INT_RESULT = (INT_ARG_1 / INT_ARG_2);}


static xen_value *quotient_1(ptree *prog, xen_value **args, int num_args)
{
  args[1] = convert_to_int(prog, args[1]);
  if (args[1] == NULL) return(run_warn("quotient arg1 can't convert to int"));
  args[2] = convert_to_int(prog, args[2]);
  if (args[2] == NULL) return(run_warn("quotient arg2 can't convert to int"));
  if (prog->constants == 2)
    return(make_xen_value(R_INT, add_int_to_ptree(prog, (prog->ints[args[1]->addr] / prog->ints[args[2]->addr])), R_CONSTANT));
  return(package(prog, R_INT, quotient_i, "quotient_i", args, 2));
}



/* ---------------- logand, logior, logxor, lognot, ash ---------------- */

static void logand_i(int *args, ptree *pt) {INT_RESULT = (INT_ARG_1 & INT_ARG_2);}


static xen_value *logand_1(ptree *prog, xen_value **args, int num_args)
{
  if (prog->constants == 2)
    return(make_xen_value(R_INT, add_int_to_ptree(prog, (prog->ints[args[1]->addr] & prog->ints[args[2]->addr])), R_CONSTANT));
  return(package(prog, R_INT, logand_i, "logand_i", args, 2));
}


static void logior_i(int *args, ptree *pt) {INT_RESULT = (INT_ARG_1 | INT_ARG_2);}


static xen_value *logior_1(ptree *prog, xen_value **args, int num_args)
{
  if (prog->constants == 2)
    return(make_xen_value(R_INT, add_int_to_ptree(prog, (prog->ints[args[1]->addr] | prog->ints[args[2]->addr])), R_CONSTANT));
  return(package(prog, R_INT, logior_i, "logior_i", args, 2));
}


#ifndef XOR
  #define XOR(a, b) ((~((a) & (b))) & ((a) | (b)))
#endif

static void logxor_i(int *args, ptree *pt) {INT_RESULT = XOR(INT_ARG_1, INT_ARG_2);}


static xen_value *logxor_1(ptree *prog, xen_value **args, int num_args)
{
  if (prog->constants == 2)
    return(make_xen_value(R_INT, add_int_to_ptree(prog, XOR(prog->ints[args[1]->addr], prog->ints[args[2]->addr])), R_CONSTANT));
  return(package(prog, R_INT, logxor_i, "logxor_i", args, 2));
}


static void lognot_i(int *args, ptree *pt) {INT_RESULT = ~INT_ARG_1;}


static xen_value *lognot_1(ptree *prog, xen_value **args, int num_args)
{
  if (prog->constants == 1)
    return(make_xen_value(R_INT, add_int_to_ptree(prog, ~(prog->ints[args[1]->addr])), R_CONSTANT));
  return(package(prog, R_INT, lognot_i, "lognot_i", args, 1));
}


static Int c_ash(Int arg1, Int arg2)
{
  if (arg2 >= 0)
    return(arg1 << arg2);
  return(arg1 >> -arg2);
}

static void ash_i(int *args, ptree *pt) {INT_RESULT = c_ash(INT_ARG_1, INT_ARG_2);}


static xen_value *ash_1(ptree *prog, xen_value **args, int num_args)
{
  if (prog->constants == 2)
    return(make_xen_value(R_INT, add_int_to_ptree(prog, c_ash(prog->ints[args[1]->addr], prog->ints[args[2]->addr])), R_CONSTANT));
  return(package(prog, R_INT, ash_i, "ash_i", args, 2));
}


/* ---------------- expt ---------------- */

static void expt_f(int *args, ptree *pt) {FLOAT_RESULT = pow(FLOAT_ARG_1, FLOAT_ARG_2);}
static void expt_f_i(int *args, ptree *pt) {FLOAT_RESULT = pow(FLOAT_ARG_1, (double)(INT_ARG_2));}
static void expt_i_f(int *args, ptree *pt) {FLOAT_RESULT = pow((double)(INT_ARG_1), FLOAT_ARG_2);}
static void expt_i_i(int *args, ptree *pt) {FLOAT_RESULT = pow((double)(INT_ARG_1), (double)(INT_ARG_2));}

static xen_value *expt_1(ptree *prog, xen_value **args, int num_args)
{
  if (!optimizing) return(NULL);
  if (prog->constants == 2)
    {
      Double f1, f2;
      if (args[1]->type == R_INT) f1 = (Double)(prog->ints[args[1]->addr]); else f1 = prog->dbls[args[1]->addr];
      if (args[2]->type == R_INT) f2 = (Double)(prog->ints[args[2]->addr]); else f2 = prog->dbls[args[2]->addr];
      return(make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, pow(f1, f2)), R_CONSTANT));
    }
  if (args[1]->type == R_INT) 
    {
      if (args[2]->type == R_INT) 
	return(package(prog, R_FLOAT, expt_i_i, "expt_i_i", args, 2));
      return(package(prog, R_FLOAT, expt_i_f, "expt_i_f", args, 2));
    }
  else
    {
      if (args[2]->type == R_INT)
	return(package(prog, R_FLOAT, expt_f_i, "expt_f_i", args, 2));
    }
  return(package(prog, R_FLOAT, expt_f, "expt_f", args, 2));
}


/* ---------------- abs ---------------- */

static void abs_f(int *args, ptree *pt) {FLOAT_RESULT = fabs(FLOAT_ARG_1);}
static void abs_f_mult(int *args, ptree *pt) {FLOAT_RESULT = FLOAT_ARG_2 * fabs(FLOAT_ARG_1);}
static void abs_f_add(int *args, ptree *pt) {FLOAT_RESULT = FLOAT_ARG_2 + fabs(FLOAT_ARG_1);}
static void abs_f_mult_add(int *args, ptree *pt) {FLOAT_RESULT = FLOAT_ARG_3 + FLOAT_ARG_2 * fabs(FLOAT_ARG_1);}
static void abs_subtract_f2(int *args, ptree *pt) {FLOAT_RESULT = fabs(FLOAT_ARG_1 - FLOAT_ARG_2);}

static void abs_i(int *args, ptree *pt) {INT_RESULT = ((INT_ARG_1 >= 0) ? INT_ARG_1 : (-INT_ARG_1));} /* not abs=32 bit truncation */


static xen_value *abs_1(ptree *prog, xen_value **args, int num_args)
{
  if (prog->constants == 1)
    {
      if (args[1]->type == R_INT)
	{
	  Int val;
	  val = prog->ints[args[1]->addr];
	  return(make_xen_value(R_INT, add_int_to_ptree(prog, ((val >= 0) ? val : (-val))), R_CONSTANT));
	}
      else return(make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, fabs(prog->dbls[args[1]->addr])), R_CONSTANT));
    }
  if (args[1]->type == R_INT)
    return(package(prog, R_INT, abs_i, "abs_i", args, 1));

  if (prog->triple_ctr > 0)
    {
      triple *prev_op;
      prev_op = prog->program[prog->triple_ctr - 1];
      if ((prev_op->args[0] == args[1]->addr) &&
	  (prev_op->function == subtract_f2) &&
	  ((find_var_in_ptree_via_addr(prog, R_INT, prev_op->args[0])) == NULL))
	{
	  prev_op->function = abs_subtract_f2;
	  prev_op->op_name = "abs_subtract_f2";
#if WITH_COUNTERS
	  prev_op->func_loc = get_func_loc(prev_op->function, prev_op->op_name);
#endif				  
	  return(make_xen_value(R_FLOAT, prev_op->args[0], R_TEMPORARY));
	}
    }
  return(package(prog, R_FLOAT, abs_f, "abs_f", args, 1));
}



/* ---------------- random ---------------- */

/* random is 0..arg, mus_random is -arg..arg, 
 * random arg determines return type, mus_random returns float 
 */

static void mus_random_f(int *args, ptree *pt) {FLOAT_RESULT = mus_random(FLOAT_ARG_1);}
static void mus_random_f_add(int *args, ptree *pt) {FLOAT_RESULT = FLOAT_ARG_2 + mus_random(FLOAT_ARG_1);}
static void mus_random_f_1(int *args, ptree *pt) {FLOAT_RESULT = mus_random_no_input();}

/* (run (lambda () (+ (mus-random 1.0) 1.0))) */

static xen_value *mus_random_1(ptree *prog, xen_value **args, int num_args)
{
  if (args[1]->type == R_INT) single_to_float(prog, args, 1);
  if ((prog->constants == 1) &&
      (prog->dbls[args[1]->addr] == 1.0))
    return(package(prog, R_FLOAT, mus_random_f_1, "mus_random_f_1", args, 0));
  return(package(prog, R_FLOAT, mus_random_f, "mus_random_f", args, 1));
}


static void random_f(int *args, ptree *pt) {FLOAT_RESULT = s7_random(s7, NULL) * FLOAT_ARG_1;}
static void random_f_add(int *args, ptree *pt) {FLOAT_RESULT = FLOAT_ARG_2 + s7_random(s7, NULL) * FLOAT_ARG_1;}
static void random_f_mult(int *args, ptree *pt) {FLOAT_RESULT = FLOAT_ARG_2 * s7_random(s7, NULL) * FLOAT_ARG_1;}
static void random_i(int *args, ptree *pt) {INT_RESULT = (Int)(s7_random(s7, NULL) * INT_ARG_1);}

static xen_value *random_1(ptree *prog, xen_value **args, int num_args)
{
  if (args[1]->type == R_INT)
    return(package(prog, R_INT, random_i, "random_i", args, 1));
  return(package(prog, R_FLOAT, random_f, "random_f", args, 1));
}



/* ---------------- chars ---------------- */

#define CHARP(SName, CName) \
static void char_ ## CName ## _c(int *args, ptree *pt) {BOOL_RESULT = (Int)CName((int)(INT_ARG_1));} \
static xen_value * SName(ptree *pt, xen_value **args, int num_args) \
{ \
  if (pt->constants == 1) \
    return(make_xen_value(R_BOOL, add_int_to_ptree(pt, CName((int)(pt->ints[args[1]->addr]))), R_CONSTANT)); \
  return(package(pt, R_BOOL, char_## CName ## _c, #CName "_c", args, 1)); \
}

CHARP(char_alphabetic_p, isalpha)
CHARP(char_numeric_p, isdigit)
CHARP(char_lower_case_p, islower)
CHARP(char_upper_case_p, isupper)
CHARP(char_whitespace_p, isspace)

#define CHARC(SName, CName) \
static void char_ ## CName ## _c(int *args, ptree *pt) {INT_RESULT = (int)CName(CHAR_ARG_1);} \
static xen_value * SName(ptree *pt, xen_value **args, int num_args) \
{ \
  if (pt->constants == 1) \
    return(make_xen_value(R_CHAR, add_int_to_ptree(pt, CName((char)(pt->ints[args[1]->addr]))), R_CONSTANT)); \
  return(package(pt, R_CHAR, char_## CName ## _c, #CName "_c", args, 1)); \
}

CHARC(char_upcase, toupper)
CHARC(char_downcase, tolower)


static xen_value *char_to_integer(ptree *pt, xen_value **args, int num_args)
{
  xen_value *newv;
  newv = copy_xen_value(args[1]);
  newv->type = R_INT;
  return(newv);
}


static xen_value **upcase_args(ptree *pt, xen_value **args, int num_args)
{
  int i;
  for (i = 1; i <= num_args; i++)
    if (args[i])
      {
	xen_value *old_loc;
	old_loc = args[i];
	if (args[i]->constant == R_CONSTANT)
	  args[i] = make_xen_value(R_CHAR, add_int_to_ptree(pt, toupper((char)(pt->ints[args[i]->addr]))), R_CONSTANT);
	else 
	  {
	    args[i] = make_xen_value(R_CHAR, add_int_to_ptree(pt, 0), R_VARIABLE);
	    add_triple_to_ptree(pt, va_make_triple(char_toupper_c, "toupper_c", 2, args[i], old_loc));
	  }
	free(old_loc);
      }
  return(args);
}


/* ---------------- strings ---------------- */

static void string_n(int *args, ptree *pt)
{
  int i, n;
  n = INT_ARG_1;
  if (STRING_RESULT) free(STRING_RESULT);
  STRING_RESULT = (char *)calloc(n + 1, sizeof(char));
  for (i = 1; i <= n; i++) STRING_RESULT[i - 1] = (char)(pt->ints[args[i + 1]]);
}

static xen_value *string_1(ptree *pt, xen_value **args, int num_args)
{
  if (pt->constants == num_args)
    {
      int i;
      char *str;
      str = (char *)calloc(num_args + 1, sizeof(char));
      for (i = 1; i <= num_args; i++)
	str[i - 1] = (char)(pt->ints[args[i]->addr]);
      return(make_xen_value(R_STRING, add_string_to_ptree(pt, str), R_CONSTANT));
    }
  return(package_n(pt, R_STRING, string_n, "string_n", args, num_args));
}


static void strlen_1(int *args, ptree *pt) {INT_RESULT = mus_strlen(STRING_ARG_1);}


static xen_value *string_length_1(ptree *pt, xen_value **args, int num_args)
{
  if (args[1]->constant == R_CONSTANT)
    return(make_xen_value(R_INT, add_int_to_ptree(pt, mus_strlen(pt->strs[args[1]->addr])), R_CONSTANT));
  return(package(pt, R_INT, strlen_1, "strlen_1", args, 1));
}


static void strcpy_1(int *args, ptree *pt) 
{
  if (STRING_RESULT) free(STRING_RESULT);
  STRING_RESULT = mus_strdup(STRING_ARG_1);
}


static xen_value *string_copy_1(ptree *pt, xen_value **args, int num_args)
{
  if (args[1]->constant == R_CONSTANT)
    return(make_xen_value(R_STRING, add_string_to_ptree(pt, mus_strdup(pt->strs[args[1]->addr])), R_CONSTANT));
  return(package(pt, R_STRING, strcpy_1, "strcpy_1", args, 1));
}


static void strfill_1(int *args, ptree *pt) 
{
  int len;
  len = mus_strlen(STRING_RESULT);
  if (len > 0)
    memset((void *)(STRING_RESULT), (char)(CHAR_ARG_1), len);
}


static xen_value *string_fill_1(ptree *pt, xen_value **args, int num_args)
{
  if (args[1]->constant == R_CONSTANT)
    return(run_warn("constant 1st arg to string-fill!"));
  add_triple_to_ptree(pt, va_make_triple(strfill_1, "strfill_1", 2, args[1], args[2])); /* shifts back one */
  return(make_xen_value(R_UNSPECIFIED, -1, R_CONSTANT));
}


static void strset_1(int *args, ptree *pt) {STRING_RESULT[INT_ARG_1] = (char)(CHAR_ARG_2);}


static xen_value *string_set_1(ptree *pt, xen_value **args, int num_args)
{
  if (args[1]->constant == R_CONSTANT)
    return(run_warn("constant 1st arg to string-set!"));
  add_triple_to_ptree(pt, va_make_triple(strset_1, "strset_1", 3, args[1], args[2], args[3])); /* shifts back one */
  return(make_xen_value(R_UNSPECIFIED, -1, R_CONSTANT));
}


static void strref_1(int *args, ptree *pt) {CHAR_RESULT = (char)(STRING_ARG_1[INT_ARG_2]);}


static xen_value *r_string_ref_1(ptree *pt, xen_value **args, int num_args)
{
  if ((args[1]->constant == R_CONSTANT) && (args[2]->constant == R_CONSTANT))
    return(make_xen_value(R_CHAR, add_int_to_ptree(pt, (int)(pt->strs[args[1]->addr][pt->ints[args[2]->addr]])), R_CONSTANT));
  return(package(pt, R_CHAR, strref_1, "strref_1", args, 2));
}


static char *vect_to_string(vect *v, int type)
{
  #define VECT_STRING_SIZE 32
  int len, slen;
  char *buf;

  if (v == NULL) 
    return(mus_strdup("#<vect: null>"));

  len = 8;
  if (len > v->length) len = v->length;
  slen = 64 + len * VECT_STRING_SIZE;
  buf = (char *)calloc(slen, sizeof(char));
  sprintf(buf, "#<vect[len=%d]:", v->length);

  if (len > 0)
    {
      int i;
      char flt[VECT_STRING_SIZE];
      for (i = 0; i < len; i++)
	{
	  switch (type)
	    {
	    case R_INT_VECTOR: 
	      mus_snprintf(flt, VECT_STRING_SIZE, " " INT_STR, v->data.ints[i]); 
	      break;

	    case R_VCT_VECTOR:	  
	      if (v->data.vcts[i])
		mus_snprintf(flt, VECT_STRING_SIZE, " #<vct[len=%lld]>", (v->data.vcts[i])->length);
	      else mus_snprintf(flt, VECT_STRING_SIZE, " %s", "#f");
	      break;

	    case R_CLM_VECTOR:
	      if (v->data.gens[i])
		mus_snprintf(flt, VECT_STRING_SIZE, " #<%s>", mus_name(v->data.gens[i]));
	      else mus_snprintf(flt, VECT_STRING_SIZE, " %s", "#f");
	      break;

	    case R_LIST_VECTOR:
	      if (v->data.lists[i])
		mus_snprintf(flt, VECT_STRING_SIZE, " #<list[len=%d]>", v->data.lists[i]->len);
	      else mus_snprintf(flt, VECT_STRING_SIZE, " '()");
	      break;
	    }
	  buf = mus_strcat(buf, flt, &slen);
	}

      if (v->length > 8)
	buf = mus_strcat(buf, " ...", &slen);
    }

  buf = mus_strcat(buf, ">", &slen);
  return(buf);
}


static char *int_vect_to_string(vect *v) {return(vect_to_string(v, R_INT_VECTOR));}


static char *clm_vect_to_string(vect *v) {return(vect_to_string(v, R_CLM_VECTOR));}


static char *list_vect_to_string(vect *v) {return(vect_to_string(v, R_LIST_VECTOR));}


static char *vct_vect_to_string(vect *v) {return(vect_to_string(v, R_VCT_VECTOR));}


static void display_str(int *args, ptree *pt) {fprintf(stdout, "%s", STRING_ARG_1);}


static void display_int(int *args, ptree *pt) {fprintf(stdout, INT_STR, INT_ARG_1);}


static void display_flt(int *args, ptree *pt) {fprintf(stdout, "%.6f", FLOAT_ARG_1);}


static void display_symbol(int *args, ptree *pt) 
{
  char *temp = NULL;
  fprintf(stdout, "%s", temp = s7_object_to_c_string(s7, SCHEME_ARG_1));
  if (temp) free(temp);
}


static void display_key(int *args, ptree *pt) 
{
  char *temp = NULL;
  fprintf(stdout, "%s", temp = s7_object_to_c_string(s7, SCHEME_ARG_1));
  if (temp) free(temp);
}


static void display_clm(int *args, ptree *pt) 
{
  char *str;
  fprintf(stdout, "%s", str = mus_describe(CLM_ARG_1));
  if (str) free(str);
}


static void display_vct(int *args, ptree *pt) 
{
  char *v = NULL;
  v = mus_vct_to_string(VCT_ARG_1);
  if (v)
    {
      fprintf(stdout, "%s", v);
      free(v);
    }
}



static void display_int_vect(int *args, ptree *pt) 
{
  char *buf = NULL;
  buf = int_vect_to_string(VECT_ARG_1);
  if (buf) {fprintf(stdout, "%s", buf); free(buf); }
}



static void display_clm_vect(int *args, ptree *pt) 
{
  char *buf = NULL;
  buf = clm_vect_to_string(VECT_ARG_1);
  if (buf) {fprintf(stdout, "%s", buf); free(buf); }
}


static void display_list_vect(int *args, ptree *pt) 
{
  char *buf = NULL;
  buf = list_vect_to_string(VECT_ARG_1);
  if (buf) {fprintf(stdout, "%s", buf); free(buf); }
}


static void display_vct_vect(int *args, ptree *pt) 
{
  char *buf = NULL;
  buf = vct_vect_to_string(VECT_ARG_1);
  if (buf) {fprintf(stdout, "%s", buf); free(buf); }
}


#if USE_SND
static void display_rd(int *args, ptree *pt) {char *buf = NULL; fprintf(stdout, "%s", buf = sampler_to_string(SAMPLER_ARG_1)); free(buf);}
#endif

static void display_sd(int *args, ptree *pt) {char *buf = NULL; fprintf(stdout, "%s", buf = sound_data_to_string(SOUND_DATA_ARG_1)); free(buf);}


static void display_chr(int *args, ptree *pt) {fprintf(stdout, "%c", (char)(INT_ARG_1));}


static void display_bool(int *args, ptree *pt) {fprintf(stdout, "%s", B2S(INT_ARG_1));}


static void display_con(int *args, ptree *pt) {fprintf(stdout, GO_PT, args[1]);}


static void display_func(int *args, ptree *pt) 
{
  char *p = NULL;
  p = describe_ptree(FNC_ARG_1, "");
  if (p)
    {
      fprintf(stdout, "%s", p);
      free(p);
    }
}


static xen_value *display_1(ptree *pt, xen_value **args, int num_args)
{
  switch (args[1]->type)
    {
    case R_STRING:       return(package(pt, R_BOOL, display_str, "display_str", args, 1));        break;
    case R_INT:          return(package(pt, R_BOOL, display_int, "display_int", args, 1));        break;
    case R_FLOAT:        return(package(pt, R_BOOL, display_flt, "display_flt", args, 1));        break;
    case R_CLM:          return(package(pt, R_BOOL, display_clm, "display_clm", args, 1));        break;
    case R_SYMBOL:       return(package(pt, R_BOOL, display_symbol, "display_symbol", args, 1));  break;
    case R_KEYWORD:      return(package(pt, R_BOOL, display_key, "display_key", args, 1));        break;
#if USE_SND
    case R_SAMPLER:      return(package(pt, R_BOOL, display_rd, "display_rd", args, 1));          break;
#endif
    case R_FLOAT_VECTOR:
    case R_VCT:          return(package(pt, R_BOOL, display_vct, "display_vct", args, 1));        break;
    case R_SOUND_DATA:   return(package(pt, R_BOOL, display_sd, "display_sd", args, 1));          break;
    case R_BOOL:         return(package(pt, R_BOOL, display_bool, "display_bool", args, 1));      break;
    case R_CHAR:         return(package(pt, R_BOOL, display_chr, "display_chr", args, 1));        break;
    case R_GOTO:         return(package(pt, R_BOOL, display_con, "display_con", args, 1));        break;
    case R_FUNCTION:     return(package(pt, R_BOOL, display_func, "display_func", args, 1));      break;
    case R_VCT_VECTOR:   return(package(pt, R_BOOL, display_vct_vect, "display_vect", args, 1));  break;
    case R_INT_VECTOR:   return(package(pt, R_BOOL, display_int_vect, "display_vect", args, 1));  break;
    case R_CLM_VECTOR:   return(package(pt, R_BOOL, display_clm_vect, "display_vect", args, 1));  break;
    case R_LIST_VECTOR:  return(package(pt, R_BOOL, display_list_vect, "display_vect", args, 1)); break;
    }
  return(NULL);
}


static xen_value *symbol2string_1(ptree *pt, xen_value **args, int num_args)
{
  return(make_xen_value(R_STRING, 
			add_string_to_ptree(pt, mus_strdup(s7_symbol_name(pt->xens[args[1]->addr]))),
			R_CONSTANT));
}

#if USE_SND
static void snd_print_s(int *args, ptree *pt) {fprintf(stderr, "%s", STRING_ARG_1);}


static xen_value *snd_print_1(ptree *pt, xen_value **args, int num_args) {return(package(pt, R_BOOL, snd_print_s, "snd_print_s", args, 1));}


static void snd_warning_s(int *args, ptree *pt) {snd_warning("%s", STRING_ARG_1);}


static xen_value *snd_warning_1(ptree *pt, xen_value **args, int num_args) {return(package(pt, R_BOOL, snd_warning_s, "snd_warning_s", args, 1));}
#endif

static void strmake_c(int *args, ptree *pt, char c) 
{
  int i, n;
  n = INT_ARG_1;
  if (STRING_RESULT) free(STRING_RESULT);
  /* this should be safe because we don't allow (set! str str1) aliasing */
  STRING_RESULT = (char *)calloc(n + 1, sizeof(char));
  for (i = 1; i <= n; i++) STRING_RESULT[i - 1] = c;
}


static void strmake_1(int *args, ptree *pt) {strmake_c(args, pt, ' ');}


static void strmake_2(int *args, ptree *pt) {strmake_c(args, pt, CHAR_ARG_2);}


static xen_value *make_string_1(ptree *pt, xen_value **args, int num_args)
{
  if (num_args == 1)
    return(package(pt, R_STRING, strmake_1, "strmake_1", args, 1));
  return(package(pt, R_STRING, strmake_2, "strmake_2", args, 2));
}


static char *substring(const char *str, int start, int end)
{
  int i, len;
  char *newstr;
  len = end - start;
  if (len <= 0) return((char *)calloc(1, sizeof(char)));
  newstr = (char *)calloc(len + 1, sizeof(char));
  for (i = 0; i < len; i++) newstr[i] = str[i + start];
  return(newstr);
}

static void substr_1(int *args, ptree *pt) 
{
  int start, end, len, arg_len;
  start = INT_ARG_2;
  end = INT_ARG_3;
  arg_len = mus_strlen(STRING_ARG_1);
  if (arg_len < end) end = arg_len; /* should throw run-time error technically */
  len = end - start;
  if (STRING_RESULT) free(STRING_RESULT);
  if (len <= 0) 
    STRING_RESULT = (char *)calloc(1, sizeof(char));
  else
    {
      int i;
      STRING_RESULT = (char *)calloc(len + 1, sizeof(char));
      for (i = 0; i < len; i++)
	STRING_RESULT[i] = STRING_ARG_1[i + start];
    }
}


static xen_value *substring_1(ptree *pt, xen_value **args, int num_args)
{
  if (pt->constants == 3)
    {
      /* parse time substring -- can check bounds here */
      int beg, end, len;
      len = mus_strlen(pt->strs[args[1]->addr]);
      beg = pt->ints[args[2]->addr];
      end = pt->ints[args[3]->addr];
      if ((beg <= len) && (end <= len))
	return(make_xen_value(R_STRING, 
			      add_string_to_ptree(pt, substring(pt->strs[args[1]->addr], beg, end)),
			      R_CONSTANT));
      return(run_warn("substring: args out of range"));
    }
  return(package(pt, R_STRING, substr_1, "substr_1", args, 3));
}


static char *string_append(ptree *pt, xen_value **args, int num_args)
{
  int i, len = 0;
  char *str;
  for (i = 0; i < num_args; i++)
    len += mus_strlen(pt->strs[args[i + 1]->addr]);
  str = (char *)calloc(len + 1, sizeof(char));
  for (i = 0; i < num_args; i++)
    if (pt->strs[args[i + 1]->addr])
      strcat(str, pt->strs[args[i + 1]->addr]);
  return(str);
}

static void appendstr_n(int *args, ptree *pt) 
{
  int i, n, len = 0;
  if (STRING_RESULT) free(STRING_RESULT);
  n = INT_ARG_1;
  for (i = 1; i <= n; i++) len += mus_strlen(pt->strs[args[i + 1]]);
  STRING_RESULT = (char *)calloc(len + 1, sizeof(char));
  for (i = 1; i <= n; i++) 
    if (pt->strs[args[i + 1]])
      strcat(STRING_RESULT, pt->strs[args[i + 1]]);
}


static xen_value *string_append_1(ptree *pt, xen_value **args, int num_args)
{
  if (num_args == 0)
    return(make_xen_value(R_STRING, add_string_to_ptree(pt, (char *)calloc(1, sizeof(char))), R_CONSTANT));
  if (num_args == pt->constants)
    return(make_xen_value(R_STRING, add_string_to_ptree(pt, string_append(pt, args, num_args)), R_CONSTANT));
  if (num_args == 1)
    return(copy_xen_value(args[1]));
  return(package_n(pt, R_STRING, appendstr_n, "appendstr_n", args, num_args));
}


#define STR_REL_OP(CName, SName, COp) \
static bool str_ ## CName ## _n(ptree *pt, xen_value **args, int num_args) \
{ \
  int i; \
  for (i = 2; i <= num_args; i++) \
    if (safe_strcmp(pt->strs[args[i - 1]->addr], pt->strs[args[i]->addr]) COp 0) \
      return(false); \
  return(true); \
} \
static void string_ ## CName ## _2(int *args, ptree *pt) {BOOL_RESULT = (!(safe_strcmp(STRING_ARG_1, STRING_ARG_2) COp 0));} \
static void string_ ## CName ## _n(int *args, ptree *pt) \
{ \
  int i, n; \
  n = INT_ARG_1; \
  BOOL_RESULT = (Int)true; \
  for (i = 2; i <= n; i++) \
    if (safe_strcmp(pt->strs[args[i]], pt->strs[args[i + 1]]) COp 0) \
      { \
	BOOL_RESULT = (Int)false; \
	break; \
      } \
} \
static xen_value *string_ ## CName ## _1(ptree *pt, xen_value **args, int num_args) \
{ \
  int i; \
  char *lasts = NULL; \
  if (num_args <= 1) return(make_xen_value(R_BOOL, add_int_to_ptree(pt, (Int)true), R_CONSTANT)); \
  if (num_args == pt->constants) return(make_xen_value(R_BOOL, add_int_to_ptree(pt, str_ ## CName ## _n(pt, args, num_args)), R_CONSTANT)); \
  if (pt->constants > 1) \
    for (i = 1; i <= num_args; i++) \
      if (args[i]->constant == R_CONSTANT) \
	{ \
	  if ((lasts) && (safe_strcmp(lasts, pt->strs[args[i]->addr]) COp 0)) \
	    return(make_xen_value(R_BOOL, add_int_to_ptree(pt, (Int)false), R_CONSTANT)); \
	  lasts = pt->strs[args[i]->addr]; \
	} \
  if (num_args == 2) return(package(pt, R_BOOL, string_ ## CName ## _2, "string_" #CName "_2", args, num_args)); \
  return(package_n(pt, R_BOOL, string_ ## CName ## _n, "string_" #CName "_n", args, num_args)); \
}

STR_REL_OP(eq, string=?, !=)
STR_REL_OP(geq, string>=?, <)
STR_REL_OP(leq, string<=?, >)
STR_REL_OP(gt, string>?, <=)
STR_REL_OP(lt, string<?, >=)


#define STR_CI_REL_OP(CName, SName, COp) \
static bool str_ci_ ## CName ## _n(ptree *pt, xen_value **args, int num_args) \
{ \
  int i; \
  for (i = 2; i <= num_args; i++) \
    if (safe_strcasecmp(pt->strs[args[i - 1]->addr], pt->strs[args[i]->addr]) COp 0) \
      return(false); \
  return(true); \
} \
static void string_ci_ ## CName ## _n(int *args, ptree *pt) \
{ \
  int i, n; \
  n = INT_ARG_1; \
  BOOL_RESULT = (Int)true; \
  for (i = 2; i <= n; i++) \
    if (safe_strcasecmp(pt->strs[args[i]], pt->strs[args[i + 1]]) COp 0) \
      { \
	BOOL_RESULT = (Int)false; \
	break; \
      } \
} \
static xen_value *string_ci_ ## CName ## _1(ptree *pt, xen_value **args, int num_args) \
{ \
  if (num_args <= 1) return(make_xen_value(R_BOOL, add_int_to_ptree(pt, (Int)true), R_CONSTANT)); \
  if (num_args == pt->constants) return(make_xen_value(R_BOOL, add_int_to_ptree(pt, str_ci_ ## CName ## _n(pt, args, num_args)), R_CONSTANT)); \
  return(package_n(pt, R_BOOL, string_ci_ ## CName ## _n, "string_ci_" #CName "_n", args, num_args)); \
}

STR_CI_REL_OP(eq, string=?, !=)
STR_CI_REL_OP(geq, string>=?, <)
STR_CI_REL_OP(leq, string<=?, >)
STR_CI_REL_OP(gt, string>?, <=)
STR_CI_REL_OP(lt, string<?, >=)


/* ---------------- number->string ---------------- */

static char *f2s_1(Double n) {return(DOUBLE_TO_STRING(n));}
static char *f2s_2(Double n, int rad) {return(DOUBLE_TO_STRING_WITH_RADIX(n, rad));}
static char *i2s_1(Int n) {return(INTEGER_TO_STRING(n));}
static char *i2s_2(Int n, int rad) {return(INTEGER_TO_STRING_WITH_RADIX(n, rad));}

static void number2string_f1(int *args, ptree *pt) {if (STRING_RESULT) free(STRING_RESULT); STRING_RESULT = f2s_1(FLOAT_ARG_1);}

static void number2string_f2(int *args, ptree *pt) {if (STRING_RESULT) free(STRING_RESULT); STRING_RESULT = f2s_2(FLOAT_ARG_1, INT_ARG_2);}

static void number2string_i1(int *args, ptree *pt) {if (STRING_RESULT) free(STRING_RESULT); STRING_RESULT = i2s_1(INT_ARG_1);}

static void number2string_i2(int *args, ptree *pt) {if (STRING_RESULT) free(STRING_RESULT); STRING_RESULT = i2s_2(INT_ARG_1, INT_ARG_2);}

static xen_value *number2string_1(ptree *prog, xen_value **args, int num_args)
{
  if ((args[1]->type == R_INT) || (args[1]->type == R_FLOAT))
    {
      if (num_args == 1)
	{
	  if (prog->constants == 1)
	    {
	      if (args[1]->type == R_INT)
		return(make_xen_value(R_STRING, add_string_to_ptree(prog, i2s_1(prog->ints[args[1]->addr])), R_CONSTANT));
	      else return(make_xen_value(R_STRING, add_string_to_ptree(prog, f2s_1(prog->dbls[args[1]->addr])), R_CONSTANT));
	    }
	  if (args[1]->type == R_INT)
	    return(package(prog, R_STRING, number2string_i1, "number2string_i1", args, 1));
	  return(package(prog, R_STRING, number2string_f1, "number2string_f1", args, 1));
	}
      else
	{
	  if (args[2]->type == R_INT)
	    {
	      if (prog->constants == 2)
		{
		  if (args[1]->type == R_INT)
		    return(make_xen_value(R_STRING, add_string_to_ptree(prog, i2s_2(prog->ints[args[1]->addr], prog->ints[args[2]->addr])), R_CONSTANT));
		  else return(make_xen_value(R_STRING, add_string_to_ptree(prog, f2s_2(prog->dbls[args[1]->addr], prog->ints[args[2]->addr])), R_CONSTANT));
		}
	      if (args[1]->type == R_INT)
		return(package(prog, R_STRING, number2string_i2, "number2string_i2", args, 2));
	      return(package(prog, R_STRING, number2string_f2, "number2string_f2", args, 2));
	    }
	}
    }
  return(NULL);
}

/* ---------------- user-defined function call ---------------- */

static void funcall_i1(int *args, ptree *pt)
{
  /* 1 int arg, int result */
  ptree *func;
  func = ((ptree **)(pt->fncs))[args[1]];
  pt->ints[func->args[0]] = pt->ints[args[2]]; 
  eval_embedded_ptree(func, pt);  
  INT_RESULT = pt->ints[func->result->addr]; 
}


static void funcall_f1(int *args, ptree *pt)
{
  /* 1 float arg, float result */
  ptree *func;
  func = ((ptree **)(pt->fncs))[args[1]];
  pt->dbls[func->args[0]] = pt->dbls[args[2]]; 
  eval_embedded_ptree(func, pt);  
  FLOAT_RESULT = pt->dbls[func->result->addr]; 
}


static void funcall_f2(int *args, ptree *pt)
{
  /* 2 float args, float result */
  ptree *func;
  func = ((ptree **)(pt->fncs))[args[1]];
  pt->dbls[func->args[0]] = pt->dbls[args[2]]; 
  pt->dbls[func->args[1]] = pt->dbls[args[3]]; 
  eval_embedded_ptree(func, pt);  
  FLOAT_RESULT = pt->dbls[func->result->addr]; 
}


static void funcall_f2b(int *args, ptree *pt)
{
  /* 2 float args, boolean result */
  ptree *func;
  func = ((ptree **)(pt->fncs))[args[1]];
  pt->dbls[func->args[0]] = pt->dbls[args[2]]; 
  pt->dbls[func->args[1]] = pt->dbls[args[3]]; 
  eval_embedded_ptree(func, pt);  
  BOOL_RESULT = pt->ints[func->result->addr]; 
}


static void funcall_cf2(int *args, ptree *pt)
{
  /* clm struct and float arg, float result -- very common case! */
  ptree *func;
  func = ((ptree **)(pt->fncs))[args[1]];
  pt->lists[func->args[0]] = pt->lists[args[2]]; 
  pt->dbls[func->args[1]] = pt->dbls[args[3]]; 
  eval_embedded_ptree(func, pt);  
  FLOAT_RESULT = pt->dbls[func->result->addr]; 
}


static void outa_funcall_cf2(int *args, ptree *pt)
{
  ptree *func;
  func = ((ptree **)(pt->fncs))[args[4]];
  pt->lists[func->args[0]] = pt->lists[args[2]]; 
  pt->dbls[func->args[1]] = pt->dbls[args[5]]; 
  eval_embedded_ptree(func, pt);  
  mus_out_any(INT_ARG_1, pt->dbls[func->result->addr], 0, CLM_ARG_3);
}


static void outb_funcall_cf2(int *args, ptree *pt)
{
  ptree *func;
  func = ((ptree **)(pt->fncs))[args[4]];
  pt->lists[func->args[0]] = pt->lists[args[2]]; 
  pt->dbls[func->args[1]] = pt->dbls[args[5]]; 
  eval_embedded_ptree(func, pt);  
  mus_out_any(INT_ARG_1, pt->dbls[func->result->addr], 1, CLM_ARG_3);
}


static void funcall_nf(int *args, ptree *pt) 
{
  int i;
  ptree *func;
  xen_value *fres;
  func = ((ptree **)(pt->fncs))[args[1]];
  fres = func->result;

  /* we might be passed fewer args than func->arity has room for, but we'll fix up in funcall_n */

  /* fprintf(stderr, "funcall %d args\n", func->arity); */

  for (i = 0; i < func->arity; i++)
    switch (func->arg_types[i])
      {
      case R_BOOL:
      case R_CHAR:
      case R_INT:
	pt->ints[func->args[i]] = pt->ints[args[i + 2]]; 
	break;

      case R_FLOAT: 
	/* fprintf(stderr, "float arg %d from %d at %d: %f\n", i, args[i + 2], func->args[1 + 2], pt->dbls[args[i + 2]]); */
	pt->dbls[func->args[i]] = pt->dbls[args[i + 2]]; 
	break;

      case R_STRING: 
	if (pt->strs[func->args[i]]) free(pt->strs[func->args[i]]);
	pt->strs[func->args[i]] = mus_strdup(pt->strs[args[i + 2]]); 
	break;

      case R_FLOAT_VECTOR:
      case R_VCT: 
 	pt->vcts[func->args[i]] = pt->vcts[args[i + 2]]; 
 	break;

      case R_SOUND_DATA: 
 	pt->sds[func->args[i]] = pt->sds[args[i + 2]]; 
 	break;

      case R_CLM: 
 	pt->clms[func->args[i]] = pt->clms[args[i + 2]]; 
 	break;

      case R_LIST_VECTOR:
      case R_CLM_VECTOR: 
      case R_INT_VECTOR: 
      case R_VCT_VECTOR: 
 	pt->vects[func->args[i]] = pt->vects[args[i + 2]]; 
 	break;

      case R_LIST:
	/* fprintf(stderr,"got list %d as arg %d\n", args[i + 2], i); */
 	pt->lists[func->args[i]] = pt->lists[args[i + 2]]; 
 	break;

      case R_FUNCTION: 
 	pt->fncs[func->args[i]] = pt->fncs[args[i + 2]]; 
 	break;

      case R_SYMBOL: 
      case R_KEYWORD:
 	pt->xens[func->args[i]] = pt->xens[args[i + 2]]; 
 	break;

#if USE_SND
      case R_SAMPLER: 
 	pt->samplers[func->args[i]] = pt->samplers[args[i + 2]]; 
 	break;
#endif

      default:
	if (CLM_STRUCT_P(func->arg_types[i]))
	  pt->lists[func->args[i]] = pt->lists[args[i + 2]]; 
	break;
      }

  eval_embedded_ptree(func, pt);

  switch (fres->type)
    {
    case R_BOOL:
    case R_CHAR:
    case R_INT:   
      INT_RESULT = pt->ints[fres->addr];   
      break;

    case R_FLOAT: 
      FLOAT_RESULT = pt->dbls[fres->addr]; 
      break;

    case R_STRING: 
      if (STRING_RESULT) free(STRING_RESULT);
      STRING_RESULT = pt->strs[fres->addr];
      pt->strs[fres->addr] = NULL;
      break; 

    case R_FLOAT_VECTOR:
    case R_VCT:   
      VCT_RESULT = pt->vcts[fres->addr];   
      break;

    case R_SOUND_DATA:   
      SOUND_DATA_RESULT = pt->sds[fres->addr];   
      break;

    case R_CLM:   
      CLM_RESULT = pt->clms[fres->addr];   
      break;

    case R_LIST:
      LIST_RESULT = pt->lists[fres->addr];   
      break;

    case R_LIST_VECTOR:   
    case R_CLM_VECTOR:   
    case R_INT_VECTOR:   
    case R_VCT_VECTOR:   
      VECT_RESULT = pt->vects[fres->addr];   
      break;

    case R_SYMBOL: 
    case R_KEYWORD:
      SCHEME_RESULT = pt->xens[fres->addr];
      break;

    case R_FUNCTION:   
      FNC_RESULT = ((ptree **)(pt->fncs))[fres->addr];   
      break;

#if USE_SND
    case R_SAMPLER:   
      SAMPLER_RESULT = pt->samplers[fres->addr];   
      break;
#endif

    default:
      if (CLM_STRUCT_P(fres->type))
	LIST_RESULT = pt->lists[fres->addr];
      break;
    }
}


static xen_value *funcall_n(ptree *prog, xen_value **args, int num_args, xen_value *sf)
{
  ptree *func;
  int i, total_args;
  xen_value **new_args;
  xen_value *fres;

  func = ((ptree **)(prog->fncs))[sf->addr];
  if (!func) 
    return(run_warn("inner lambda lost!"));

  if (func->arity < num_args) 
    return(run_warn("not enough args for func (got %d, wants %d)", num_args, func->arity));

  /* if func_arity > num_args, pass in the extras explicitly */

  if (func->arity > num_args)
    total_args = func->arity;
  else total_args = num_args;

  fres = func->result;
  new_args = (xen_value **)calloc(total_args + 2, sizeof(xen_value *));

  for (i = 1; i <= num_args; i++)
    {
      new_args[i + 1] = args[i];

      if (CLM_STRUCT_P(args[i]->type))
	{
	  xen_var *var;
	  var = find_var_in_ptree_via_addr(prog, args[i]->type, args[i]->addr);
	  if (var) var->unclean = true;
	}
    }

  if (total_args > num_args)
    {
      for (i = num_args + 1; i <= total_args; i++)
	{
	  /* pick up (copy) default values */
	  xen_value *v = NULL;

	  switch (func->arg_types[i - 1])
	    {
	    case R_INT:
	    case R_CHAR:    
	    case R_BOOL:
	      v = make_xen_value(func->arg_types[i - 1], add_int_to_ptree(prog, prog->ints[func->default_args[i - 1]]), R_CONSTANT);                 
	      break;
	      
	    case R_FLOAT:   
	      v = make_xen_value(func->arg_types[i - 1], add_dbl_to_ptree(prog, prog->dbls[func->default_args[i - 1]]), R_CONSTANT);                
	      break;
	      
	    case R_STRING:  
	      v = make_xen_value(func->arg_types[i - 1], add_string_to_ptree(prog, mus_strdup(prog->strs[func->default_args[i - 1]])), R_CONSTANT); 
	      break;
	    }

	  /* fprintf(stderr, "default arg %d from %d: %s\n", i - 1, func->default_args[i - 1], describe_xen_value(v, prog)); */
	  new_args[i + 1] = v;
	}
    }

  new_args[1] = sf;
  new_args[0] = add_temporary_var_to_ptree(prog, fres->type);
  args[0] = new_args[0];
  
  if (total_args == 1)
    {
      if ((func->arg_types[0] == R_FLOAT) &&
	  (fres->type == R_FLOAT))
	add_triple_to_ptree(prog, make_triple(funcall_f1, "funcall_f1", new_args, total_args + 2));
      else
	{
	  if ((func->arg_types[0] == R_INT) &&
	      (fres->type == R_INT))
	    add_triple_to_ptree(prog, make_triple(funcall_i1, "funcall_i1", new_args, total_args + 2));
	  else add_triple_to_ptree(prog, make_triple(funcall_nf, "funcall_nf", new_args, total_args + 2));
	}
    }
  else
    {
      if (total_args == 2)
	{
	  if ((func->arg_types[0] == R_FLOAT) &&
	      (func->arg_types[1] == R_FLOAT))
	    {
	      if (fres->type == R_FLOAT)
		add_triple_to_ptree(prog, make_triple(funcall_f2, "funcall_f2", new_args, total_args + 2));
	      else
		{
		  if (fres->type == R_BOOL)
		    add_triple_to_ptree(prog, make_triple(funcall_f2b, "funcall_f2b", new_args, total_args + 2));
		  else add_triple_to_ptree(prog, make_triple(funcall_nf, "funcall_nf", new_args, total_args + 2));
		}
	    }
	  else
	    {
	      if ((CLM_STRUCT_P(func->arg_types[0])) &&
		  (func->arg_types[1] == R_FLOAT) &&
		  (fres->type == R_FLOAT))
		add_triple_to_ptree(prog, make_triple(funcall_cf2, "funcall_cf2", new_args, total_args + 2));
	      else add_triple_to_ptree(prog, make_triple(funcall_nf, "funcall_nf", new_args, total_args + 2));
	    }
	}
      else add_triple_to_ptree(prog, make_triple(funcall_nf, "funcall_nf", new_args, total_args + 2));
    }

  free(new_args);
  return(args[0]);
}


/* ---------------- non-local goto via continuation ---------------- */

static xen_value *goto_0(ptree *prog, xen_value **args, xen_value *sf)
{
  if (args[0]) free(args[0]);
  args[0] = make_xen_value(R_BOOL, add_int_to_ptree(prog, sf->addr), R_VARIABLE);
  add_triple_to_ptree(prog, va_make_triple(jump_indirect, "jump_indirect", 1, args[0]));
  return(args[0]);
}


#define INT_INT_OP(CName) \
static void CName ## _i(int *args, ptree *pt) {INT_RESULT = CName(INT_ARG_1);} \
static xen_value * CName ## _1(ptree *prog, xen_value **args, int num_args) {return(package(prog, R_INT, CName ## _i, #CName "_i", args, 1));}


INT_INT_OP(mus_bytes_per_sample)



static xen_value *vct_length_1(ptree *prog, xen_value **args, int num_args);
static xen_value *mus_length_0(ptree *prog, xen_value **args, int num_args);
static xen_value *sound_data_length_1(ptree *prog, xen_value **args, int num_args);

#if USE_SND
static void sound_length_0(int *args, ptree *pt) {INT_RESULT = CURRENT_SAMPLES(ss->sounds[SOUND_ARG_1]->chans[0]);}
static xen_value *sound_length_1(ptree *prog, xen_value **args, int num_args) {return(package(prog, R_INT, sound_length_0, "length", args, 1));}

static void mix_length_0(int *args, ptree *pt) {INT_RESULT = mix_length_from_id(MIX_ARG_1);}
static xen_value *mix_length_1(ptree *prog, xen_value **args, int num_args) {return(package(prog, R_INT, mix_length_0, "length", args, 1));}

static void region_length_0(int *args, ptree *pt) {INT_RESULT = region_len(REGION_ARG_1);}
static xen_value *region_length_1(ptree *prog, xen_value **args, int num_args) {return(package(prog, R_INT, region_length_0, "length", args, 1));}
#endif



#if USE_SND
/* ---------------- simple snd ops ---------------- */

#define BOOL_INT_OP(CName) \
static void CName ## _i(int *args, ptree *pt) {BOOL_RESULT = CName(INT_ARG_1);} \
static xen_value * CName ## _1(ptree *prog, xen_value **args, int num_args) {return(package(prog, R_BOOL, CName ## _i, #CName "_i", args, 1));}


#define INT_VOID_OP(CName) \
static void CName ## _i(int *args, ptree *pt) {INT_RESULT = CName();} \
static xen_value * CName ## _1(ptree *prog, xen_value **args, int num_args) {return(package(prog, R_INT, CName ## _i, #CName "_i", args, 0));}


/* must copy string here because mus_run_free_ptree will free all temp strings */
#define STR_VOID_OP(CName) \
static void CName ## _i(int *args, ptree *pt) {if (STRING_RESULT) free(STRING_RESULT); STRING_RESULT = mus_strdup(CName());} \
static xen_value * CName ## _1(ptree *prog, xen_value **args, int num_args) {return(package(prog, R_STRING, CName ## _i, #CName "_i", args, 0));}


bool r_sound_p(int i);
BOOL_INT_OP(r_sound_p);
INT_VOID_OP(selection_chans);
static char *r_temp_dir(void) {return(temp_dir(ss));}
static char *r_save_dir(void) {return(save_dir(ss));}
STR_VOID_OP(r_temp_dir);
STR_VOID_OP(r_save_dir);



/* ---------------- snd utils ---------------- */

static snd_info *run_get_sp(int offset, int *args, Int *ints)
{
  int spint;
  spint = ints[args[offset]];
  if (spint == -1)
    return(any_selected_sound());
  else
    if ((spint < ss->max_sounds) && 
	(snd_ok(ss->sounds[spint])))
      return(ss->sounds[spint]);
  fprintf(stderr, "no such sound: %d\n", spint);
  return(NULL);
}


static chan_info *run_get_cp(int offset, int *args, Int *ints)
{
  snd_info *sp;
  sp = run_get_sp(offset, args, ints);
  if (sp)
    {
      int cpint;
      cpint = ints[args[offset + 1]];
      if (cpint == -1)
	return(any_selected_channel(sp));
      else
	if (cpint < sp->nchans)
	  return(sp->chans[cpint]);
      fprintf(stderr, "no such channel: %d\n", cpint);
    }
  return(NULL);
}


static void run_opt_arg(ptree *pt, xen_value **args, int num_args, int i, xen_value **true_args)
{
  if (num_args < i)
    true_args[i] = make_xen_value(R_INT, add_int_to_ptree(pt, -1), R_CONSTANT);
  else
    {
      if (args[i]->type == R_BOOL)
	pt->ints[args[i]->addr] = -1;
      true_args[i] = args[i];
    }
}


/* ---------------- edit-position ---------------- */

static void edit_position_i(int *args, ptree *pt) 
{
  chan_info *cp; 
  cp = run_get_cp(1, args, pt->ints);
  if (cp) INT_RESULT = cp->edit_ctr;
}


static xen_value *edit_position_1(ptree *pt, xen_value **args, int num_args)
{
  xen_value *true_args[3];
  xen_value *rtn;
  int k;
  run_opt_arg(pt, args, num_args, 1, true_args);
  run_opt_arg(pt, args, num_args, 2, true_args);
  true_args[0] = args[0];
  rtn = package(pt, R_INT, edit_position_i, "edit_position_i", true_args, 2);
  for (k = num_args + 1; k <= 2; k++) free(true_args[k]);
  return(rtn);
}


/* ---------------- cursor ---------------- */

static void cursor_i(int *args, ptree *pt) 
{
  chan_info *cp; 
  cp = run_get_cp(1, args, pt->ints);
  if (cp) INT_RESULT = CURSOR(cp);
}


static xen_value *cursor_1(ptree *pt, xen_value **args, int num_args)
{
  xen_value *true_args[3];
  xen_value *rtn;
  int k;
  run_opt_arg(pt, args, num_args, 1, true_args);
  run_opt_arg(pt, args, num_args, 2, true_args);
  true_args[0] = args[0];
  rtn = package(pt, R_INT, cursor_i, "cursor_i", true_args, 2);
  for (k = num_args + 1; k <= 2; k++) free(true_args[k]);
  return(rtn);
}


/* ---------------- maxamp ---------------- */

static void maxamp_f(int *args, ptree *pt) 
{
  chan_info *cp; 
  cp = run_get_cp(1, args, pt->ints);
  if (cp) FLOAT_RESULT = channel_maxamp(cp, INT_ARG_3);
}

static void region_maxamp_f(int *args, ptree *pt) {FLOAT_RESULT = region_maxamp(REGION_ARG_1);}
static void mix_maxamp_f(int *args, ptree *pt) {FLOAT_RESULT = mix_maxamp(MIX_ARG_1);}
static xen_value *vct_peak_1(ptree *prog, xen_value **args, int num_args);

static xen_value *maxamp_1(ptree *pt, xen_value **args, int num_args)
{
  xen_value *true_args[4];
  xen_value *rtn;
  int k;

  if (num_args == 1)
    {
      switch (args[1]->type)
	{
	case R_FLOAT_VECTOR:
	case R_VCT:        return(vct_peak_1(pt, args, num_args));
	case R_MIX:        return(package(pt, R_FLOAT, mix_maxamp_f, "mix_maxamp", args, 1));
	case R_REGION:     return(package(pt, R_FLOAT, region_maxamp_f, "region_maxamp", args, 1));
	}
    }

  run_opt_arg(pt, args, num_args, 1, true_args);
  run_opt_arg(pt, args, num_args, 2, true_args);
  run_opt_arg(pt, args, num_args, 3, true_args);
  true_args[0] = args[0];

  rtn = package(pt, R_FLOAT, maxamp_f, "maxamp_f", true_args, 3);

  for (k = num_args + 1; k <= 3; k++) free(true_args[k]);
  return(rtn);
}


/* ---------------- sample ---------------- */

static void sample_f(int *args, ptree *pt) 
{
  chan_info *cp; 
  cp = run_get_cp(2, args, pt->ints);
  if (cp) 
    FLOAT_RESULT = chn_sample(INT_ARG_1, cp, (INT_ARG_4 == AT_CURRENT_EDIT_POSITION) ? cp->edit_ctr : INT_ARG_4);
}


static xen_value *sample_1(ptree *pt, xen_value **args, int num_args) 
{
  xen_value *true_args[5];
  xen_value *rtn;
  int k;
  run_opt_arg(pt, args, num_args, 2, true_args);
  run_opt_arg(pt, args, num_args, 3, true_args);
  run_opt_arg(pt, args, num_args, 4, true_args);
  true_args[0] = args[0];
  true_args[1] = args[1];
  rtn = package(pt, R_FLOAT, sample_f, "sample_f", true_args, 4);
  for (k = num_args + 1; k <= 4; k++) free(true_args[k]);
  return(rtn);
}


/* ---------------- samples ---------------- */

static void samples_f(int *args, ptree *pt) 
{
  chan_info *cp; 
  cp = run_get_cp(3, args, pt->ints);
  if (cp) 
    {
      if (VCT_RESULT) mus_vct_free(VCT_RESULT);
      VCT_RESULT = run_samples_to_vct(INT_ARG_1, INT_ARG_2, cp, (INT_ARG_5 == AT_CURRENT_EDIT_POSITION) ? cp->edit_ctr : INT_ARG_5);
    }
}


static xen_value *samples_1(ptree *pt, xen_value **args, int num_args) 
{
  xen_value *true_args[6];
  xen_value *rtn;
  int k;

  args[0] = make_xen_value(R_VCT, add_vct_to_ptree(pt, NULL), R_VARIABLE);
  add_obj_to_gcs(pt, R_VCT, args[0]->addr);

  run_opt_arg(pt, args, num_args, 3, true_args);
  run_opt_arg(pt, args, num_args, 4, true_args);
  run_opt_arg(pt, args, num_args, 5, true_args);
  true_args[0] = args[0];
  true_args[1] = args[1];
  true_args[2] = args[2];

  rtn = package(pt, R_VCT, samples_f, "samples_f", true_args, 5);
  for (k = num_args + 1; k <= 5; k++) free(true_args[k]);

  return(rtn);
}


/* ---------------- srate ---------------- */

static void sound_srate_i(int *args, ptree *pt) 
{
  snd_info *sp;
  sp = run_get_sp(1, args, pt->ints);
  if (sp) INT_RESULT = SND_SRATE(sp);
}


static void file_srate_i(int *args, ptree *pt) 
{
  INT_RESULT = mus_sound_srate(STRING_ARG_1);
}


static void region_srate_i(int *args, ptree *pt) 
{
  INT_RESULT = region_srate(REGION_ARG_1);
}


static xen_value *srate_1(ptree *pt, xen_value **args, int num_args)
{
  xen_value *true_args[2];
  xen_value *rtn = NULL;
  int k;

  run_opt_arg(pt, args, num_args, 1, true_args);
  /* this adds an arg (of -1) if 0 passed, then run_get_sp treats the -1 as "use current sound" */
  true_args[0] = args[0];

  switch (true_args[1]->type)
    {
    case R_STRING: 
      rtn = package(pt, R_INT, file_srate_i, "file_srate_i", true_args, 1);
      break;

    case R_SOUND:      
      rtn = package(pt, R_INT, sound_srate_i, "sound_srate_i", true_args, 1);
      break;

    case R_REGION:
      rtn = package(pt, R_INT, region_srate_i, "region_srate_i", true_args, 1);
      break;

    case R_INT:
    case R_BOOL:
      rtn = package(pt, R_INT, sound_srate_i, "sound_srate_i", true_args, 1);
      break;

    default:
      run_warn("unsupported arg type for srate");
      break;
    }
  
  for (k = num_args + 1; k <= 1; k++) free(true_args[k]);
  return(rtn);
}


/* ---------------- channels ---------------- */

static void sound_channels_i(int *args, ptree *pt) 
{
  snd_info *sp;
  sp = run_get_sp(1, args, pt->ints);
  if (sp) INT_RESULT = sp->nchans;
}


static void file_channels_i(int *args, ptree *pt) {INT_RESULT = mus_sound_chans(STRING_ARG_1);}
static void region_channels_i(int *args, ptree *pt) {INT_RESULT = region_chans(REGION_ARG_1);}
static void sound_data_channels_i(int *args, ptree *pt) {INT_RESULT = SOUND_DATA_ARG_1->chans;}
static xen_value *mus_channels_0(ptree *prog, xen_value **args, int num_args);


static xen_value *channels_1(ptree *pt, xen_value **args, int num_args)
{
  xen_value *true_args[2];
  xen_value *rtn = NULL;
  int k;
  run_opt_arg(pt, args, num_args, 1, true_args);
  true_args[0] = args[0];

  switch (true_args[1]->type)
    {

    case R_STRING: 
      rtn = package(pt, R_INT, file_channels_i, "file_channels_i", true_args, 1);
      break;

    case R_SOUND:      
      rtn = package(pt, R_INT, sound_channels_i, "sound_channels_i", true_args, 1);
      break;

    case R_REGION:
      rtn = package(pt, R_INT, region_channels_i, "region_channels_i", true_args, 1);
      break;

    case R_INT:
    case R_BOOL:
      rtn = package(pt, R_INT, sound_channels_i, "sound_channels_i", true_args, 1);
      break;

    case R_SOUND_DATA:
      rtn = package(pt, R_INT, sound_data_channels_i, "sound_data_channels_i", true_args, 1);
      break;
      
    case R_CLM:
      rtn = mus_channels_0(pt, true_args, 1);
      break;

    default:
      if (CLM_STRUCT_P(args[1]->type))
	rtn = mus_channels_0(pt, true_args, 1);
      else rtn = make_xen_value(R_INT, add_int_to_ptree(pt, 1), R_CONSTANT);
      break;
    }

  for (k = num_args + 1; k <= 1; k++) free(true_args[k]);
  return(rtn);
}


/* --------------- file-name ---------------- */

static void sound_file_name_s(int *args, ptree *pt) 
{
  snd_info *sp;
  sp = run_get_sp(1, args, pt->ints);
  if (sp) 
    {
      if (STRING_RESULT) free(STRING_RESULT);
      STRING_RESULT = mus_strdup(sp->filename);
    }
}


static void string_file_name_s(int *args, ptree *pt) 
{
  if (STRING_RESULT) free(STRING_RESULT);
  STRING_RESULT = mus_expand_filename(STRING_ARG_1);
}


static void region_file_name_s(int *args, ptree *pt) 
{
  if (STRING_RESULT) free(STRING_RESULT);
  STRING_RESULT = mus_strdup(region_file_name(REGION_ARG_1));
}


static void mix_file_name_s(int *args, ptree *pt) 
{
  if (STRING_RESULT) free(STRING_RESULT);
  STRING_RESULT = mus_strdup(mix_file_name(REGION_ARG_1));
}

static xen_value *mus_file_name_0(ptree *prog, xen_value **args, int num_args);

static xen_value *file_name_1(ptree *pt, xen_value **args, int num_args)
{
  xen_value *true_args[2];
  xen_value *rtn = NULL;
  int k;

  run_opt_arg(pt, args, num_args, 1, true_args);
  /* this adds an arg (of -1) if 0 passed, then run_get_sp treats the -1 as "use current sound" */
  true_args[0] = args[0];

  switch (true_args[1]->type)
    {
    case R_STRING: 
      rtn = package(pt, R_STRING, string_file_name_s, "file_name", true_args, 1);
      break;

    case R_SOUND:      
      rtn = package(pt, R_STRING, sound_file_name_s, "file_name", true_args, 1);
      break;

    case R_REGION:
      rtn = package(pt, R_STRING, region_file_name_s, "file_name", true_args, 1);
      break;

    case R_MIX:
      rtn = package(pt, R_STRING, mix_file_name_s, "file_name", true_args, 1);
      break;

    case R_INT:
    case R_BOOL:
      rtn = package(pt, R_STRING, sound_file_name_s, "file_name", true_args, 1);
      break;

    case R_CLM:
      rtn = mus_file_name_0(pt, true_args, 1);
      break;

    default:
      if (CLM_STRUCT_P(args[1]->type))
	rtn = mus_file_name_0(pt, true_args, 1);
      else run_warn("unsupported arg type for file-name");
      break;
    }
  
  for (k = num_args + 1; k <= 1; k++) free(true_args[k]);
  return(rtn);
}



/* ---------------- exit ---------------- */

static void exit_0(int *args, ptree *pt) 
{
  if (snd_exit_cleanly(EXIT_NOT_FORCED))
    snd_exit(1);
}

static xen_value *exit_1(ptree *pt, xen_value **args, int num_args)
{
  return(package(pt, R_BOOL, exit_0, "exit_0", args, 0));
}



/* ---------------- frames ---------------- */

static void frames_i(int *args, ptree *pt) 
{
  chan_info *cp; 
  cp = run_get_cp(1, args, pt->ints);
  if (cp)
    {
      int pos;
      if (INT_ARG_3 == AT_CURRENT_EDIT_POSITION)
	pos = cp->edit_ctr;
      else pos = INT_ARG_3;
      INT_RESULT = cp->edits[pos]->samples;
    }
}


static xen_value *mus_sound_frames_1(ptree *pt, xen_value **args, int num_args);

static xen_value *frames_1(ptree *pt, xen_value **args, int num_args)
{
  xen_value *true_args[4];
  xen_value *rtn;
  int k;

  if (num_args == 1)
    {
      switch (args[1]->type)
	{
	case R_STRING:     return(mus_sound_frames_1(pt, args, num_args));
	case R_CLM:        return(mus_length_0(pt, args, num_args));
	case R_VCT:        return(vct_length_1(pt, args, num_args));
	case R_SOUND_DATA: return(sound_data_length_1(pt, args, num_args));

	case R_SOUND:      return(sound_length_1(pt, args, num_args));    /* (let ((snd (integer->sound 0))) (run (lambda () (frames snd)))) */
	case R_MIX:        return(mix_length_1(pt, args, num_args));
	case R_REGION:     return(region_length_1(pt, args, num_args));
	  /* else drop into regular frames code */
	}
    }

  if (num_args < 3)
    true_args[3] = make_xen_value(R_INT, add_int_to_ptree(pt, AT_CURRENT_EDIT_POSITION), R_CONSTANT);
  else true_args[3] = args[3];

  run_opt_arg(pt, args, num_args, 1, true_args);
  run_opt_arg(pt, args, num_args, 2, true_args);
  true_args[0] = args[0];

  rtn = package(pt, R_INT, frames_i, "frames_i", true_args, 3);
  for (k = num_args + 1; k <= 3; k++) free(true_args[k]);
  return(rtn);
}



/* ---------------- sampler stuff ---------------- */

static void sampler_f(int *args, ptree *pt) {FLOAT_RESULT = read_sample(SAMPLER_ARG_1);}


static xen_value *sampler_0(ptree *prog, xen_value **args, xen_value *sf)
{
  if (args[0]) free(args[0]);
  args[0] = make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, 0.0), R_VARIABLE);
  add_triple_to_ptree(prog, va_make_triple(sampler_f, "sampler_f", 2, args[0], sf));
  return(args[0]);
}

static xen_value *sampler_1(ptree *prog, xen_value **args, int num_args) {return(package(prog, R_FLOAT, sampler_f, "sampler_f", args, 1));}


static void next_sampler_f(int *args, ptree *pt) {FLOAT_RESULT = protected_next_sample(SAMPLER_ARG_1);}

static xen_value *next_sample_1(ptree *prog, xen_value **args, int num_args) {return(package(prog, R_FLOAT, next_sampler_f, "next_sampler_f", args, 1));}


static void previous_sampler_f(int *args, ptree *pt) {FLOAT_RESULT = protected_previous_sample(SAMPLER_ARG_1);}

static xen_value *previous_sample_1(ptree *prog, xen_value **args, int num_args) {return(package(prog, R_FLOAT, previous_sampler_f, "previous_sampler_f", args, 1));}


static void sampler_at_end_b_s(int *args, ptree *pt) {BOOL_RESULT = SAMPLER_ARG_1->at_eof;}

static xen_value *sampler_at_end_p_1(ptree *prog, xen_value **args, int num_args) 
{
  if (args[1]->type == R_SAMPLER)
    return(package(prog, R_BOOL, sampler_at_end_b_s, "sampler_at_end_b_s", args, 1));
  return(NULL);
}


static void make_sampler_r(int *args, ptree *pt) 
{
  chan_info *cp = NULL;
  cp = run_get_cp(2, args, pt->ints);
  if (cp)
    {
      int pos;
      read_direction_t direction = READ_FORWARD;
      if (INT_ARG_5 == -1)
	pos = cp->edit_ctr;
      else pos = INT_ARG_5;
      free_snd_fd(SAMPLER_RESULT);
      if (INT_ARG_4 == -1) direction = READ_BACKWARD;
      SAMPLER_RESULT = init_sample_read_any(INT_ARG_1, cp, direction, pos);
    }
}

static xen_value *make_sampler_1(ptree *pt, xen_value **args, int num_args)
{
  xen_value *true_args[6];
  int k;
  xen_value *rtn;
  bool free_true_args_1 = false;

  if (num_args < 5) 
    true_args[5] = make_xen_value(R_INT, add_int_to_ptree(pt, AT_CURRENT_EDIT_POSITION), R_CONSTANT);
  else true_args[5] = args[5];
  if (num_args < 4) 
    true_args[4] = make_xen_value(R_INT, add_int_to_ptree(pt, 1), R_CONSTANT);
  else true_args[4] = args[4];
  run_opt_arg(pt, args, num_args, 2, true_args);
  run_opt_arg(pt, args, num_args, 3, true_args);
  if (num_args == 0)
    true_args[1] = make_xen_value(R_INT, add_int_to_ptree(pt, 0), R_CONSTANT);
  else
    {
      if (args[1]->type == R_FLOAT)
	{
	  free_true_args_1 = true;
	  true_args[1] = convert_dbl_to_int(pt, args[1], true);
	}
      else true_args[1] = args[1];
    }
  true_args[0] = args[0];
  rtn = package(pt, R_SAMPLER, make_sampler_r, "make_sampler_r", true_args, 5);
  add_obj_to_gcs(pt, R_SAMPLER, rtn->addr);
  for (k = num_args + 1; k <= 5; k++) free(true_args[k]);
  if (free_true_args_1) free(true_args[1]);
  return(rtn);
}
#endif
/* end USE_SND */



/* ---------------- autocorrelate ---------------- */

static void autocorrelate_0(int *args, ptree *pt) 
{
  mus_autocorrelate(VCT_ARG_1->data, VCT_ARG_1->length);
  VCT_RESULT = VCT_ARG_1;
}


static xen_value *autocorrelate_1(ptree *prog, xen_value **args, int num_args) 
{
  return(package(prog, R_VCT, autocorrelate_0, "autocorrelate_0", args, 1));
}



/* ---------------- correlate ---------------- */

static void correlate_0(int *args, ptree *pt) 
{
  mus_long_t size;
  if (VCT_ARG_1->length < VCT_ARG_2->length)
    size = VCT_ARG_1->length;
  else size = VCT_ARG_2->length;
  mus_correlate(VCT_ARG_1->data, VCT_ARG_2->data, size);
  VCT_RESULT = VCT_ARG_1;
}


static xen_value *correlate_1(ptree *prog, xen_value **args, int num_args) 
{
  return(package(prog, R_VCT, correlate_0, "correlate_0", args, 2));
}



/* ---------------- mus-audio stuff ---------------- */

INT_INT_OP(mus_audio_close)


static char *audio_write_obuf = NULL;
static int audio_write_obuf_size = 0;
int mus_audio_io_format(int line);

static void mus_audio_write_0(int *args, ptree *pt) 
{
  #define audio_io_write_format(Line) (mus_audio_io_format(Line) >> 16)

  sound_data *sd;
  int outbytes, frms, fmt, fd;

  sd = SOUND_DATA_ARG_2;
  frms = INT_ARG_3;
  fd = INT_ARG_1;

  fmt = audio_io_write_format(fd);
  outbytes = frms * sd->chans * mus_bytes_per_sample(fmt);

  if (outbytes > audio_write_obuf_size)
    {
      if (audio_write_obuf) free(audio_write_obuf);
      audio_write_obuf_size = outbytes;
      audio_write_obuf = (char *)calloc(outbytes, sizeof(char));
    }

#if SNDLIB_USE_FLOATS
  mus_file_write_buffer(fmt, 0, frms - 1, sd->chans, sd->data, audio_write_obuf, true); /* true -> clipped */
#else
  {
    mus_sample_t **sdata;
    int i;
    mus_long_t j;
    sdata = (mus_sample_t **)calloc(sd->chans, sizeof(mus_sample_t *));
    for (i = 0; i < sd->chans; i++) 
      sdata[i] = (mus_sample_t *)calloc(sd->length, sizeof(mus_sample_t));
    for (i = 0; i < sd->chans; i++)
      for (j = 0; j < sd->length; j++)
	sdata[i][j] = MUS_DOUBLE_TO_SAMPLE(sd->data[i][j]);
    mus_file_write_buffer(fmt, 0, frms - 1, sd->chans, sdata, audio_write_obuf, true);
    for (i = 0; i < sd->chans; i++)
      free(sdata[i]);
    free(sdata);
  }
#endif
  INT_RESULT = mus_audio_write(fd, audio_write_obuf, outbytes);

}


static xen_value *mus_audio_write_1(ptree *prog, xen_value **args, int num_args) 
{
  return(package(prog, R_INT, mus_audio_write_0, "mus_audio_write_0", args, 3));
}




/* ---------------- list ---------------- */

static void float_list_ref(int *args, ptree *pt) {FLOAT_RESULT = pt->dbls[LIST_ARG_1->vals[INT_ARG_2]->addr];}


static void float_list_ref_0(int *args, ptree *pt) {FLOAT_RESULT = pt->dbls[LIST_ARG_1->vals[0]->addr];}
static void float_list_ref_1(int *args, ptree *pt) {FLOAT_RESULT = pt->dbls[LIST_ARG_1->vals[1]->addr];}
static void float_list_ref_2(int *args, ptree *pt) {FLOAT_RESULT = pt->dbls[LIST_ARG_1->vals[2]->addr];}
static void float_list_ref_3(int *args, ptree *pt) {FLOAT_RESULT = pt->dbls[LIST_ARG_1->vals[3]->addr];}
static void float_list_ref_4(int *args, ptree *pt) {FLOAT_RESULT = pt->dbls[LIST_ARG_1->vals[4]->addr];}


static void int_list_ref(int *args, ptree *pt) {INT_RESULT = pt->ints[LIST_ARG_1->vals[INT_ARG_2]->addr];}


static void list_ref(int *args, ptree *pt) 
{
  xen_value *v;
  int addr;

  v = LIST_ARG_1->vals[INT_ARG_2];
  addr = v->addr;

  switch (v->type)
    {
    case R_INT:   
      INT_RESULT = pt->ints[addr];   
      break;

    case R_BOOL:   
      BOOL_RESULT = pt->ints[addr];   
      break;

    case R_CHAR:   
      CHAR_RESULT = (char)(pt->ints[addr]);   
      break;

    case R_FLOAT: 
      FLOAT_RESULT = pt->dbls[addr]; 
      break;

    case R_STRING:
      if (STRING_RESULT) free(STRING_RESULT);
      /* (let ((val (list "l1" "l2" "l3"))) (run (lambda () (let ((str (list-ref val 1))) (do ((i 0 (1+ i))) ((= i 2)) (set! str (list-ref val i))))))) */
      STRING_RESULT = mus_strdup(pt->strs[addr]);
      break; 

    case R_FLOAT_VECTOR:
    case R_VCT:   
      VCT_RESULT = pt->vcts[addr];   
      break;

    case R_SOUND_DATA:   
      SOUND_DATA_RESULT = pt->sds[addr];   
      break;

    case R_CLM:   
      CLM_RESULT = pt->clms[addr];   
      break;

    case R_LIST_VECTOR:   
    case R_CLM_VECTOR:   
    case R_INT_VECTOR:   
    case R_VCT_VECTOR:   
      VECT_RESULT = pt->vects[addr];   
      break;

    case R_SYMBOL: 
    case R_KEYWORD:
      SCHEME_RESULT = pt->xens[addr];
      break;

    case R_FUNCTION:   
      FNC_RESULT = ((ptree **)(pt->fncs))[addr];   
      break;

#if USE_SND
    case R_SAMPLER:   
      SAMPLER_RESULT = pt->samplers[addr];   
      break;
#endif
    }
}


static char *def_clm_struct_field_name(int def_clm_struct_type, int field);

static xen_value *package_clm_struct(ptree *prog, int struct_type, xen_value **args, int num_args)
{
  int type;
  char *name;
  type = def_clm_struct_field_type(struct_type, prog->ints[args[2]->addr]);
  name = def_clm_struct_field_name(struct_type, prog->ints[args[2]->addr]);
  if (type == R_FLOAT)
    {
      switch (prog->ints[args[2]->addr])
	{
	case 0: return(package(prog, type, float_list_ref_0, name, args, num_args)); 
	case 1: return(package(prog, type, float_list_ref_1, name, args, num_args)); 
	case 2: return(package(prog, type, float_list_ref_2, name, args, num_args)); 
	case 3: return(package(prog, type, float_list_ref_3, name, args, num_args)); 
	case 4: return(package(prog, type, float_list_ref_4, name, args, num_args)); 
	default:  return(package(prog, type, float_list_ref, name, args, num_args)); 
	}
    }
  if (type == R_INT)
    return(package(prog, type, int_list_ref, name, args, num_args)); 
  return(package(prog, type, list_ref, name, args, num_args)); 
}


static xen_value *r_list_ref_1(ptree *prog, xen_value **args, int num_args)
{
  list *xl;
  /* these are list constants, we know in advance what all element types are */

  if (CLM_STRUCT_P(args[1]->type))
    return(package_clm_struct(prog, args[1]->type, args, num_args));

  xl = prog->lists[args[1]->addr];
  if (xl)
    {
      if (CLM_STRUCT_P(xl->type))
	return(package_clm_struct(prog, xl->type, args, num_args));

      if (xl->type == R_FLOAT)
	return(package(prog, xl->type, float_list_ref, "float_list_ref", args, 2));
      if (xl->type == R_INT)
	return(package(prog, xl->type, int_list_ref, "int_list_ref", args, 2));

      return(package(prog, xl->type, list_ref, "list_ref", args, 2));
    }

  return(package(prog, R_FLOAT, float_list_ref, "float_list_ref", args, 2));  
}


static xen_value *cxr_1(ptree *prog, xen_value **args, int num_args, int loc)
{
  xen_value *true_args[3];
  xen_value *rtn;
  true_args[0] = args[0];
  true_args[1] = args[1];
  true_args[2] = make_xen_value(R_INT, add_int_to_ptree(prog, loc), R_CONSTANT);
  rtn = r_list_ref_1(prog, true_args, 2);
  free(true_args[2]);
  return(rtn);
}

static xen_value *car_1(ptree *prog, xen_value **args, int num_args) {return(cxr_1(prog, args, num_args, 0));}

static xen_value *cadr_1(ptree *prog, xen_value **args, int num_args) {return(cxr_1(prog, args, num_args, 1));}

static xen_value *caddr_1(ptree *prog, xen_value **args, int num_args) {return(cxr_1(prog, args, num_args, 2));}

static xen_value *cadddr_1(ptree *prog, xen_value **args, int num_args) {return(cxr_1(prog, args, num_args, 3));}


static void float_list_set(int *args, ptree *pt) {pt->dbls[LIST_ARG_1->vals[INT_ARG_2]->addr] = FLOAT_ARG_3;}


static void float_list_set_0(int *args, ptree *pt) {pt->dbls[LIST_ARG_1->vals[0]->addr] = FLOAT_ARG_3;}
static void float_list_set_1(int *args, ptree *pt) {pt->dbls[LIST_ARG_1->vals[1]->addr] = FLOAT_ARG_3;}
static void float_list_set_2(int *args, ptree *pt) {pt->dbls[LIST_ARG_1->vals[2]->addr] = FLOAT_ARG_3;}
static void float_list_set_3(int *args, ptree *pt) {pt->dbls[LIST_ARG_1->vals[3]->addr] = FLOAT_ARG_3;}
static void float_list_set_4(int *args, ptree *pt) {pt->dbls[LIST_ARG_1->vals[4]->addr] = FLOAT_ARG_3;}


static void int_list_set(int *args, ptree *pt) {pt->ints[LIST_ARG_1->vals[INT_ARG_2]->addr] = INT_ARG_3;}


static void list_set(int *args, ptree *pt) 
{
  xen_value *v;
  int addr;

  v = LIST_ARG_1->vals[INT_ARG_2];
  addr = v->addr;

  /* fprintf(stderr,"list set %s (%d %f)\n", describe_xen_value(v, pt), (int)(INT_ARG_2), FLOAT_ARG_3); */

  switch (v->type)
    {
    case R_INT:          pt->ints[addr] = INT_ARG_3;                 break;
    case R_BOOL:         pt->ints[addr] = BOOL_ARG_3;                break;
    case R_CHAR:         pt->ints[addr] = (Int)(CHAR_ARG_3);         break;
    case R_FLOAT:        pt->dbls[addr] = FLOAT_ARG_3;               break;
    case R_CLM:          pt->clms[addr] = CLM_ARG_3;                 break;
    case R_STRING:       pt->strs[addr] = mus_strdup(STRING_ARG_3);  break;
    case R_KEYWORD:
    case R_SYMBOL:       pt->xens[addr] = SCHEME_ARG_3;                break;
    case R_FLOAT_VECTOR:
    case R_VCT:          pt->vcts[addr] = VCT_ARG_3;                 break;
    case R_SOUND_DATA:   pt->sds[addr] = SOUND_DATA_ARG_3;           break;
#if USE_SND
    case R_SAMPLER:      pt->samplers[addr] = SAMPLER_ARG_3;         break;
#endif
    case R_LIST:         pt->lists[addr] = LIST_ARG_3;               break;
    case R_LIST_VECTOR:
    case R_INT_VECTOR: 
    case R_VCT_VECTOR:
    case R_CLM_VECTOR:   pt->vects[addr] = VECT_ARG_3;               break;
    }
}


static xen_value *package_clm_struct_set(ptree *prog, int struct_type, xen_value **args, int num_args)
{
  int type;
  char *name;
  type = def_clm_struct_field_type(struct_type, prog->ints[args[2]->addr]);
  name = def_clm_struct_field_name(struct_type, prog->ints[args[2]->addr]);
  if (type == R_FLOAT)
    {
      switch (prog->ints[args[2]->addr])
	{
	case 0: return(package(prog, type, float_list_set_0, name, args, num_args)); 
	case 1: return(package(prog, type, float_list_set_1, name, args, num_args)); 
	case 2: return(package(prog, type, float_list_set_2, name, args, num_args)); 
	case 3: return(package(prog, type, float_list_set_3, name, args, num_args)); 
	case 4: return(package(prog, type, float_list_set_4, name, args, num_args)); 
	default:  return(package(prog, type, float_list_set, name, args, num_args)); 
	}
    }
  if (type == R_INT)
    return(package(prog, type, int_list_set, name, args, num_args)); 
  return(package(prog, type, list_set, name, args, num_args)); 
}


static xen_value *list_set_1(ptree *prog, xen_value **args, int num_args)
{
  list *xl;
  xen_var *var;
  var = find_var_in_ptree_via_addr(prog, args[1]->type, args[1]->addr);
  if (var) 
    var->unclean = true;

  if (CLM_STRUCT_P(args[1]->type))
    return(package_clm_struct_set(prog, args[1]->type, args, num_args));

  xl = prog->lists[args[1]->addr];
  if (xl)
    {
      if (CLM_STRUCT_P(xl->type))  /* INT_ARG_2 is a constant in this case */
	return(package_clm_struct_set(prog, xl->type, args, num_args));

      if (xl->type == R_FLOAT)
	return(package(prog, xl->type, float_list_set, "float_list_set", args, num_args));
      if (xl->type == R_INT)
	return(package(prog, xl->type, int_list_set, "int_list_set", args, num_args));
      return(package(prog, xl->type, list_set, "list_set", args, num_args));
    }

  /* else assume float */
  return(package(prog, R_FLOAT, float_list_set, "float_list_set", args, num_args));
}


static xen_value *set_car_1(ptree *prog, xen_value **args, int num_args)
{
  xen_value *true_args[4];
  xen_value *rtn;
  true_args[0] = args[0];
  true_args[1] = args[1];
  true_args[2] = make_xen_value(R_INT, add_int_to_ptree(prog, 0), R_CONSTANT);
  true_args[3] = args[2];
  rtn = list_set_1(prog, true_args, 3);
  free(true_args[2]);
  return(rtn);
}


/* ---------------------------------------- length ---------------------------------------- */

static void length_0(int *args, ptree *pt) 
{
  INT_RESULT = LIST_ARG_1->len;
}

static xen_value *vector_length_1(ptree *prog, xen_value **args, int num_args);

static xen_value *length_1(ptree *prog, xen_value **args, int num_args)
{
  switch (args[1]->type)
    {
    case R_LIST:       return(package(prog, R_INT, length_0, "length", args, 1));
    case R_VCT:        return(vct_length_1(prog, args, num_args));
    case R_STRING:     return(string_length_1(prog, args, num_args));
    case R_CLM:        return(mus_length_0(prog, args, num_args));
    case R_SOUND_DATA: return(sound_data_length_1(prog, args, num_args));

    case R_FLOAT_VECTOR: 
    case R_LIST_VECTOR:  
    case R_INT_VECTOR:  
    case R_VCT_VECTOR:  
    case R_CLM_VECTOR:   
      return(vector_length_1(prog, args, num_args)); /* (let ((v (make-vector 32 0.0))) (run-eval `(length v))) -> 32 -- the 0.0 is needed */

#if USE_SND
    case R_SOUND:      return(sound_length_1(prog, args, num_args));    /* (let ((snd (integer->sound 0))) (run (lambda () (length snd)))) */
    case R_MIX:        return(mix_length_1(prog, args, num_args));
    case R_REGION:     return(region_length_1(prog, args, num_args));
      /* not R_SAMPLER because it has no clear length (read dir can change etc) */
#endif
    }
  return(run_warn("unsupported arg type for length"));
}


/* fill and copy as generics?
   static xen_value *string_fill_1(ptree *pt, xen_value **args, int num_args)
   static xen_value *vct_fill_1(ptree *pt, xen_value **args, int num_args)
   static xen_value *vector_fill_1(ptree *prog, xen_value **args, int num_args)
   but no list_fill and clm cases are handled specially

   string_copy_1 vct_copy_v sound_data_copy_f perhaps xen_value_to_xen?
*/


/* -------------------------------------------------------------------------------- */

static void null_0(int *args, ptree *pt) 
{
  BOOL_RESULT = ((!(LIST_ARG_1)) || (LIST_ARG_1->len == 0));
}


static xen_value *null_1(ptree *prog, xen_value **args, int num_args)
{
  return(package(prog, R_BOOL, null_0, "null?", args, 1));
}


static void clm_struct_field_set_1(ptree *prog, xen_value *in_v, xen_value *in_v1, xen_value *in_v2, xen_value *v)
{
  /* in_v = args[1], new_v has int_arg[2] for addr, v is new_val */
  xen_value *args[4];
  xen_value *rtn;    
  args[0] = NULL;
  args[1] = in_v;
  args[2] = make_xen_value(R_INT, add_int_to_ptree(prog, in_v2->addr), R_CONSTANT);
  args[3] = v;
  rtn = list_set_1(prog, args, 3);
  free(rtn);
  free(args[2]);
}



/* ---------------- vector stuff ---------------- */

/* float vectors are handled as vcts
 */

/* length */

static void vector_length_f(int *args, ptree *pt) {INT_RESULT = (VCT_ARG_1) ? VCT_ARG_1->length : 0;}

static void vector_length_i(int *args, ptree *pt) {INT_RESULT = (VECT_ARG_1) ? VECT_ARG_1->length : 0;}


static xen_value *vector_length_1(ptree *prog, xen_value **args, int num_args)
{
  switch (args[1]->type)
    {
    case R_FLOAT_VECTOR: 
      return(package(prog, R_INT, vector_length_f, "vector_length_f", args, 1));
      break;

    case R_LIST_VECTOR:  
    case R_INT_VECTOR:  
    case R_VCT_VECTOR:  
    case R_CLM_VECTOR:   
      return(package(prog, R_INT, vector_length_i, "vector_length", args, 1));
      break;
    }
  return(NULL);
}


/* ref */

static void vector_ref_f(int *args, ptree *pt) {FLOAT_RESULT = VCT_ARG_1->data[INT_ARG_2];}
static void vector_ref_f0(int *args, ptree *pt) {FLOAT_RESULT = VCT_ARG_1->data[0];}
static void vector_ref_f1(int *args, ptree *pt) {FLOAT_RESULT = VCT_ARG_1->data[1];}
static void vector_ref_f2(int *args, ptree *pt) {FLOAT_RESULT = VCT_ARG_1->data[2];}

static void vector_ref_i(int *args, ptree *pt) {INT_RESULT = VECT_ARG_1->data.ints[INT_ARG_2];}

static void vector_ref_v(int *args, ptree *pt) {VCT_RESULT = VECT_ARG_1->data.vcts[INT_ARG_2];}

static void vector_ref_c(int *args, ptree *pt) {CLM_RESULT = VECT_ARG_1->data.gens[INT_ARG_2];}

static void vector_ref_l(int *args, ptree *pt) {LIST_RESULT = VECT_ARG_1->data.lists[INT_ARG_2];}

static void vector_add_i2(int *args, ptree *pt) {FLOAT_RESULT = VCT_ARG_1->data[INT_ARG_2 + INT_ARG_3];}
static void vector_subtract_i2(int *args, ptree *pt) {FLOAT_RESULT = VCT_ARG_1->data[INT_ARG_3 - INT_ARG_2];}

static xen_value *r_vector_ref_1(ptree *prog, xen_value **args, int num_args)
{
  switch (args[1]->type)
    {
    case R_FLOAT_VECTOR: 
      if (args[2]->constant == R_CONSTANT)
	{
	  if (prog->ints[args[2]->addr] == 0)
	    return(package(prog, R_FLOAT, vector_ref_f0, "vector_ref_f0", args, 2));
	  if (prog->ints[args[2]->addr] == 1)
	    return(package(prog, R_FLOAT, vector_ref_f1, "vector_ref_f1", args, 2));
	  if (prog->ints[args[2]->addr] == 2)
	    return(package(prog, R_FLOAT, vector_ref_f2, "vector_ref_f2", args, 2));
	}
      if (prog->triple_ctr > 0)
	{
	  triple *prev_op;
	  prev_op = prog->program[prog->triple_ctr - 1];
	  if ((prev_op->args[0] == args[2]->addr) &&
	      ((prev_op->function == add_i2) || (prev_op->function == subtract_i2)) &&
	      ((find_var_in_ptree_via_addr(prog, R_INT, prev_op->args[0])) == NULL))
	    {
	      xen_value *new0;
	      new0 = add_temporary_var_to_ptree(prog, R_FLOAT);  
	      prev_op->types = (int *)realloc(prev_op->types, 4 * sizeof(int));
	      prev_op->args = (int *)realloc(prev_op->args, 4 * sizeof(int));
	      prev_op->args[3] = prev_op->args[1];
	      prev_op->types[3] = prev_op->types[1];
	      prev_op->args[1] = args[1]->addr;
	      prev_op->types[1] = R_FLOAT_VECTOR;
	      prev_op->num_args = 4;
	      prev_op->args[0] = new0->addr;
	      prev_op->types[0] = R_FLOAT;
	      if (prev_op->function == add_i2)
		{
		  prev_op->function = vector_add_i2;
		  prev_op->op_name = "vector_add_i2";
		}
	      else
		{
		  prev_op->function = vector_subtract_i2;
		  prev_op->op_name = "vector_subtract_i2";
		}
#if WITH_COUNTERS
	      prev_op->func_loc = get_func_loc(prev_op->function, prev_op->op_name);
#endif				  
	      return(new0);
	    }
	}
      return(package(prog, R_FLOAT, vector_ref_f, "vector_ref_f", args, 2)); 
      break;

    case R_INT_VECTOR:   return(package(prog, R_INT, vector_ref_i, "vector_ref_i", args, 2));   break;
    case R_VCT_VECTOR:   return(package(prog, R_VCT, vector_ref_v, "vector_ref_v", args, 2));   break;
    case R_CLM_VECTOR:   return(package(prog, R_CLM, vector_ref_c, "vector_ref_c", args, 2));   break;
    case R_LIST_VECTOR:  
      {
	vect *v;
	v = prog->vects[args[1]->addr]; /* has to exist, I think */
	/* vector of lists other than generators will not work because the element type isn't passed with the list */
	/*   also currently I think the changes to gen fields are not reflected externally */
	return(package(prog, (CLM_STRUCT_P(v->data.lists[0]->type)) ? v->data.lists[0]->type : R_LIST, vector_ref_l, "vector_ref_l", args, 2)); 
      }
      break;
    }
  return(NULL);
}


/* set */

static void vector_set_f(int *args, ptree *pt) {VCT_ARG_1->data[INT_ARG_2] = FLOAT_ARG_3;}
static void vector_set_f0(int *args, ptree *pt) {VCT_ARG_1->data[0] = FLOAT_ARG_3;}
static void vector_set_f1(int *args, ptree *pt) {VCT_ARG_1->data[1] = FLOAT_ARG_3;}
static void vector_set_f2(int *args, ptree *pt) {VCT_ARG_1->data[2] = FLOAT_ARG_3;}

static void vector_set_i(int *args, ptree *pt) {VECT_ARG_1->data.ints[INT_ARG_2] = INT_ARG_3;}

static void vector_set_v(int *args, ptree *pt) {VECT_ARG_1->data.vcts[INT_ARG_2] = VCT_ARG_3;}

static void vector_set_c(int *args, ptree *pt) {VECT_ARG_1->data.gens[INT_ARG_2] = CLM_ARG_3;}

static void int_vector_set_1(ptree *prog, xen_value *in_v, xen_value *in_v1, xen_value *in_v2, xen_value *v)
{
  xen_var *var;
  var = find_var_in_ptree_via_addr(prog, in_v->type, in_v->addr);
  if (var) var->unclean = true;
  add_triple_to_ptree(prog, va_make_triple(vector_set_i, "vector_set_i", 4, NULL, in_v, in_v1, v));
}

static xen_value *r_vector_set_1(ptree *prog, xen_value **args, int num_args)
{
  xen_var *var;
  var = find_var_in_ptree_via_addr(prog, args[1]->type, args[1]->addr);
  if (var) var->unclean = true;
  switch (args[1]->type)
    {
    case R_FLOAT_VECTOR: 
      if (args[3]->type != R_FLOAT) return(run_warn("wrong new val type for float vector set"));
      if (args[2]->constant == R_CONSTANT)
	{
	  if (prog->ints[args[2]->addr] == 0)
	    return(package(prog, R_FLOAT, vector_set_f0, "vector_set_f0", args, 3));
	  if (prog->ints[args[2]->addr] == 1)
	    return(package(prog, R_FLOAT, vector_set_f1, "vector_set_f1", args, 3));
	  if (prog->ints[args[2]->addr] == 2)
	    return(package(prog, R_FLOAT, vector_set_f2, "vector_set_f2", args, 3));
	}

      if (prog->triple_ctr > 0)
	{
	  triple *prev_op;
	  prev_op = prog->program[prog->triple_ctr - 1];
	  if (((prev_op->function == vct_ref_f_add) || (prev_op->function == vct_ref_f_mult)) &&
	      (prev_op->args[1] == args[1]->addr) &&
	      (prev_op->args[2] == args[2]->addr) &&
	      ((find_var_in_ptree_via_addr(prog, R_FLOAT, prev_op->args[0])) == NULL))
	    {
	      /* (let ((v (vct 1 2 3)) (i 2)) (run (vct-set! v i (+ (vct-ref v i) 3.0)))) */
	      if (prev_op->function == vct_ref_f_add)
		{
		  prev_op->function = vct_set_f_add;
		  prev_op->op_name = "vct_set_f_add";
		}
	      else
		{
		  prev_op->function = vct_set_f_mult;
		  prev_op->op_name = "vct_set_f_mult";
		}
	      return(make_xen_value(R_FLOAT, prev_op->args[0], R_TEMPORARY));
	    }
	}

      return(package(prog, R_FLOAT, vector_set_f, "vector_set_f", args, 3)); 
      break;

    case R_INT_VECTOR: 
      if (args[3]->type != R_INT) return(run_warn("wrong new val type for int vector set"));
      return(package(prog, R_INT, vector_set_i, "vector_set_i", args, 3)); 
      break;

    case R_VCT_VECTOR: 
      if (args[3]->type != R_VCT) return(run_warn("wrong new val type for vct vector set"));
      return(package(prog, R_VCT, vector_set_v, "vector_set_v", args, 3)); 
      break;

    case R_CLM_VECTOR: 
      if (args[3]->type != R_CLM) return(run_warn("wrong new val type for clm vector set"));
      return(package(prog, R_CLM, vector_set_c, "vector_set_c", args, 3)); 
      break;
    }
  return(NULL);
}


/* fill */

static void vector_fill_f(int *args, ptree *pt) 
{
  int i; for (i = 0; i < VCT_ARG_1->length; i++) VCT_ARG_1->data[i] = FLOAT_ARG_2;
}

static void vector_fill_i(int *args, ptree *pt) 
{
  int i; for (i = 0; i < VECT_ARG_1->length; i++) VECT_ARG_1->data.ints[i] = INT_ARG_2;
}

static void vector_fill_v(int *args, ptree *pt) 
{
  int i; for (i = 0; i < VECT_ARG_1->length; i++) VECT_ARG_1->data.vcts[i] = VCT_ARG_2;
}


static void vector_fill_c(int *args, ptree *pt)
{
  int i; for (i = 0; i < VECT_ARG_1->length; i++) VECT_ARG_1->data.gens[i] = CLM_ARG_2;
}


static xen_value *vector_fill_1(ptree *prog, xen_value **args, int num_args)
{
  switch (args[1]->type)
    {
    case R_FLOAT_VECTOR: 
      if (args[2]->type != R_FLOAT) return(run_warn("wrong new val type for float vector fill"));
      return(package(prog, R_BOOL, vector_fill_f, "vector_fill_f", args, 2)); 
      break;

    case R_INT_VECTOR: 
      if (args[2]->type != R_INT) return(run_warn("wrong new val type for int vector fill"));
      return(package(prog, R_BOOL, vector_fill_i, "vector_fill_i", args, 2)); 
      break;

    case R_VCT_VECTOR:
      if (args[2]->type != R_VCT) return(run_warn("wrong new val type for vct vector fill"));
      return(package(prog, R_BOOL, vector_fill_v, "vector_fill_v", args, 2)); 
      break;

    case R_CLM_VECTOR: 
      if (args[2]->type != R_CLM) return(run_warn("wrong new val type for clm vector fill"));
      return(package(prog, R_BOOL, vector_fill_c, "vector_fill_c", args, 2)); 
      break;
    }
  return(NULL);
}



/* ---------------- vct stuff ---------------- */

static void vct_length_i(int *args, ptree *pt) {INT_RESULT = VCT_ARG_1->length;}


static xen_value *vct_length_1(ptree *prog, xen_value **args, int num_args)
{
  return(package(prog, R_INT, vct_length_i, "vct_length_i", args, 1));
}


static void vct_constant_ref_0(int *args, ptree *pt) {FLOAT_RESULT = VCT_ARG_1->data[0];}
static void vct_constant_ref_0_mult(int *args, ptree *pt) {FLOAT_RESULT = FLOAT_ARG_2 * VCT_ARG_1->data[0];}
static void vct_constant_ref_1(int *args, ptree *pt) {FLOAT_RESULT = VCT_ARG_1->data[1];}
static void vct_constant_ref_2(int *args, ptree *pt) {FLOAT_RESULT = VCT_ARG_1->data[2];}
static void vct_constant_ref_3(int *args, ptree *pt) {FLOAT_RESULT = VCT_ARG_1->data[3];}


static void vct_ref_f(int *args, ptree *pt) {FLOAT_RESULT = VCT_ARG_1->data[INT_ARG_2];}
static void vct_ref_f_mult(int *args, ptree *pt) {FLOAT_RESULT = FLOAT_ARG_3 * VCT_ARG_1->data[INT_ARG_2];}
static void vct_ref_f_add(int *args, ptree *pt) {FLOAT_RESULT = FLOAT_ARG_3 + VCT_ARG_1->data[INT_ARG_2];}

static void vct_add_i2(int *args, ptree *pt) {FLOAT_RESULT = VCT_ARG_1->data[INT_ARG_2 + INT_ARG_3];}
static void vct_subtract_i2(int *args, ptree *pt) {FLOAT_RESULT = VCT_ARG_1->data[INT_ARG_3 - INT_ARG_2];}

static xen_value *vct_ref_1(ptree *prog, xen_value **args, int num_args)
{
  if (args[2]->constant == R_CONSTANT)
    {
      if (prog->ints[args[2]->addr] == 0)
	return(package(prog, R_FLOAT, vct_constant_ref_0, "vct_constant_ref_0", args, 1));
      if (prog->ints[args[2]->addr] == 1)
	return(package(prog, R_FLOAT, vct_constant_ref_1, "vct_constant_ref_1", args, 1));
      if (prog->ints[args[2]->addr] == 2)
	return(package(prog, R_FLOAT, vct_constant_ref_2, "vct_constant_ref_2", args, 1));
      if (prog->ints[args[2]->addr] == 3)
	return(package(prog, R_FLOAT, vct_constant_ref_3, "vct_constant_ref_3", args, 1));
    }
  if (prog->triple_ctr > 0)
    {
      triple *prev_op;
      prev_op = prog->program[prog->triple_ctr - 1];
      if ((prev_op->args[0] == args[2]->addr) &&
	  ((prev_op->function == add_i2) || (prev_op->function == subtract_i2)) &&
	  ((find_var_in_ptree_via_addr(prog, R_INT, prev_op->args[0])) == NULL))
	{
	  xen_value *new0;
	  new0 = add_temporary_var_to_ptree(prog, R_FLOAT);  
	  prev_op->types = (int *)realloc(prev_op->types, 4 * sizeof(int));
	  prev_op->args = (int *)realloc(prev_op->args, 4 * sizeof(int));
	  prev_op->args[3] = prev_op->args[1];
	  prev_op->types[3] = prev_op->types[1];
	  prev_op->args[1] = args[1]->addr;
	  prev_op->types[1] = R_VCT;
	  prev_op->args[0] = new0->addr;
	  prev_op->types[0] = R_FLOAT;
	  prev_op->num_args = 4;
	  if (prev_op->function == add_i2)
	    {
	      prev_op->function = vct_add_i2;
	      prev_op->op_name = "vct_add_i2";
	    }
	  else
	    {
	      prev_op->function = vct_subtract_i2;
	      prev_op->op_name = "vct_subtract_i2";
	    }
#if WITH_COUNTERS
	  prev_op->func_loc = get_func_loc(prev_op->function, prev_op->op_name);
#endif				  
	  return(new0);
	}
    }
  return(package(prog, R_FLOAT, vct_ref_f, "vct_ref_f", args, 2));
}


static void vct_constant_set_0(int *args, ptree *pt) {VCT_ARG_1->data[0] = FLOAT_ARG_3; /* FLOAT_RESULT = FLOAT_ARG_3; */}
static void vct_constant_set_1(int *args, ptree *pt) {VCT_ARG_1->data[1] = FLOAT_ARG_3; /* FLOAT_RESULT = FLOAT_ARG_3; */}
static void vct_constant_set_2(int *args, ptree *pt) {VCT_ARG_1->data[2] = FLOAT_ARG_3; /* FLOAT_RESULT = FLOAT_ARG_3; */}
static void vct_constant_set_3(int *args, ptree *pt) {VCT_ARG_1->data[3] = FLOAT_ARG_3; /* FLOAT_RESULT = FLOAT_ARG_3; */}

static void vct_set_f(int *args, ptree *pt) {VCT_ARG_1->data[INT_ARG_2] = FLOAT_ARG_3; /* FLOAT_RESULT = FLOAT_ARG_3; */}
static void vct_set_f_add(int *args, ptree *pt) {VCT_ARG_1->data[INT_ARG_2] += FLOAT_ARG_3; /* FLOAT_RESULT = FLOAT_ARG_3; */}
static void vct_set_f_mult(int *args, ptree *pt) {VCT_ARG_1->data[INT_ARG_2] *= FLOAT_ARG_3; /* FLOAT_RESULT = FLOAT_ARG_3; */}

static void vct_set_i(int *args, ptree *pt) {VCT_ARG_1->data[INT_ARG_2] = (mus_float_t)INT_ARG_3; /* FLOAT_RESULT = (Double)INT_ARG_3; */}


static void vct_set_1(ptree *prog, xen_value *in_v, xen_value *in_v1, xen_value *in_v2, xen_value *v)
{
  /* set! vct-ref */
  xen_var *var;
  xen_value *arg0 = NULL;
  arg0 = add_temporary_var_to_ptree(prog, R_FLOAT);

  var = find_var_in_ptree_via_addr(prog, in_v->type, in_v->addr);
  if (var) var->unclean = true;

  if (v->type == R_INT)
    {
      add_triple_to_ptree(prog, va_make_triple(vct_set_i, "vct_set_i(2)", 4, arg0, in_v, in_v1, v));
      return;
    }

  if (in_v1->constant == R_CONSTANT)
    {
      if (prog->ints[in_v1->addr] == 0)
	add_triple_to_ptree(prog, va_make_triple(vct_constant_set_0, "vct_constant_set_0(1)", 4, arg0, in_v, in_v1, v));
      else if (prog->ints[in_v1->addr] == 1)
	add_triple_to_ptree(prog, va_make_triple(vct_constant_set_1, "vct_constant_set_1(1)", 4, arg0, in_v, in_v1, v));
      else if (prog->ints[in_v1->addr] == 2)
	add_triple_to_ptree(prog, va_make_triple(vct_constant_set_2, "vct_constant_set_2(1)", 4, arg0, in_v, in_v1, v));
      else if (prog->ints[in_v1->addr] == 3)
	add_triple_to_ptree(prog, va_make_triple(vct_constant_set_3, "vct_constant_set_3(1)", 4, arg0, in_v, in_v1, v));
      else add_triple_to_ptree(prog, va_make_triple(vct_set_f, "vct_set_f(2)", 4, arg0, in_v, in_v1, v));
    }
  else add_triple_to_ptree(prog, va_make_triple(vct_set_f, "vct_set_f(3)", 4, arg0, in_v, in_v1, v));
}


static xen_value *vct_set_2(ptree *prog, xen_value **args, int num_args)
{
  /* vct-set! */
  xen_var *var;
  var = find_var_in_ptree_via_addr(prog, args[1]->type, args[1]->addr);
  if (var) var->unclean = true;
  if (args[3]->type == R_FLOAT)
    {
      if (args[2]->constant == R_CONSTANT)
	{
	  if (prog->ints[args[2]->addr] == 0)
	    return(package(prog, R_FLOAT, vct_constant_set_0, "vct_constant_set_0(0)", args, 3));
	  if (prog->ints[args[2]->addr] == 1)
	    return(package(prog, R_FLOAT, vct_constant_set_1, "vct_constant_set_1(0)", args, 3));
	  if (prog->ints[args[2]->addr] == 2)
	    return(package(prog, R_FLOAT, vct_constant_set_2, "vct_constant_set_2(0)", args, 3));
	  if (prog->ints[args[2]->addr] == 3)
	    return(package(prog, R_FLOAT, vct_constant_set_3, "vct_constant_set_3(0)", args, 3));
	}

      if (prog->triple_ctr > 0)
	{
	  triple *prev_op;
	  prev_op = prog->program[prog->triple_ctr - 1];
	  if (((prev_op->function == vct_ref_f_add) || (prev_op->function == vct_ref_f_mult)) &&
	      (prev_op->args[1] == args[1]->addr) &&
	      (prev_op->args[2] == args[2]->addr) &&
	      ((find_var_in_ptree_via_addr(prog, R_FLOAT, prev_op->args[0])) == NULL))
	    {
	      /* (let ((v (vct 1 2 3)) (i 2)) (run (vct-set! v i (+ (vct-ref v i) 3.0)))) */
	      if (prev_op->function == vct_ref_f_add)
		{
		  prev_op->function = vct_set_f_add;
		  prev_op->op_name = "vct_set_f_add";
		}
	      else
		{
		  prev_op->function = vct_set_f_mult;
		  prev_op->op_name = "vct_set_f_mult";
		}
	      return(make_xen_value(R_FLOAT, prev_op->args[0], R_TEMPORARY));
	    }
	}

      return(package(prog, R_FLOAT, vct_set_f, "vct_set_f(4)", args, 3));
    }
  return(package(prog, R_FLOAT, vct_set_i, "vct_set_i", args, 3));
}


static void make_vct_v(int *args, ptree *pt) 
{
  if (VCT_RESULT) mus_vct_free(VCT_RESULT);
  VCT_RESULT = mus_vct_make(INT_ARG_1);
}


static void make_vct_v2(int *args, ptree *pt) 
{
  vct *v;
  mus_long_t i;
  if (VCT_RESULT) mus_vct_free(VCT_RESULT);
  v = mus_vct_make(INT_ARG_1);
  VCT_RESULT = v;
  for (i = 0; i < v->length; i++) v->data[i] = FLOAT_ARG_2;
}


static void make_int_vector(int *args, ptree *pt) 
{
  vect *v = NULL;
  if (VECT_RESULT) free_vect(VECT_RESULT, R_INT_VECTOR);
  if (INT_ARG_1 > 0)
    {
      mus_long_t i;
      v = (vect *)calloc(1, sizeof(vect));
      v->length = INT_ARG_1;
      v->data.ints = (Int *)calloc(v->length, sizeof(Int));
      for (i = 0; i < v->length; i++) v->data.ints[i] = INT_ARG_2;
    }
  VECT_RESULT = v;
}


static xen_value *make_vct_1(ptree *prog, xen_value **args, int num_args)
{
  args[0] = make_xen_value(R_VCT, add_vct_to_ptree(prog, NULL), R_VARIABLE);
  add_obj_to_gcs(prog, R_VCT, args[0]->addr);
  if (num_args == 1)
    add_triple_to_ptree(prog, va_make_triple(make_vct_v, "make_vct_v", 2, args[0], args[1]));
  else add_triple_to_ptree(prog, va_make_triple(make_vct_v2, "make_vct_v2", 3, args[0], args[1], args[2]));
  return(args[0]);
}


static xen_value *r_make_vector_1(ptree *prog, xen_value **args, int num_args)
{
  if ((num_args == 2) && (args[2]->type == R_INT))
    {
      args[0] = make_xen_value(R_INT_VECTOR, add_vect_to_ptree(prog, NULL), R_VARIABLE);
      add_obj_to_gcs(prog, R_INT_VECTOR, args[0]->addr);
      add_triple_to_ptree(prog, va_make_triple(make_int_vector, "make_int_vector", 3, args[0], args[1], args[2]));
    }
  else
    {
      args[0] = make_xen_value(R_FLOAT_VECTOR, add_vct_to_ptree(prog, NULL), R_VARIABLE);
      add_obj_to_gcs(prog, R_FLOAT_VECTOR, args[0]->addr);
      if (num_args == 1)
	add_triple_to_ptree(prog, va_make_triple(make_vct_v, "make_vct_v", 2, args[0], args[1]));
      else add_triple_to_ptree(prog, va_make_triple(make_vct_v2, "make_vct_v2", 3, args[0], args[1], args[2]));
    }
  return(args[0]);
}


static void vct_v(int *args, ptree *pt) 
{
  mus_long_t i;
  vct *v;
  if (VCT_RESULT) VCT_RESULT = mus_vct_free(VCT_RESULT);
  v = mus_vct_make(INT_ARG_1);
  for (i = 0; i < v->length; i++)
    v->data[i] = pt->dbls[args[i + 2]];
  VCT_RESULT = v;
}


static xen_value *vct_1(ptree *prog, xen_value **args, int num_args)
{
  xen_value *rtn;
  float_all_args(prog, num_args, args, true);
  rtn = package_n(prog, R_VCT, vct_v, "vct_v", args, num_args);
  /* rtn is args[0] */
  add_obj_to_gcs(prog, R_VCT, rtn->addr);
  return(rtn);
}


static void vct_copy_v(int *args, ptree *pt) 
{
  if (VCT_RESULT) mus_vct_free(VCT_RESULT);
  VCT_RESULT = mus_vct_copy(VCT_ARG_1);
}


static xen_value *vct_copy_1(ptree *prog, xen_value **args, int num_args)
{
  args[0] = make_xen_value(R_VCT, add_vct_to_ptree(prog, NULL), R_VARIABLE);
  add_obj_to_gcs(prog, R_VCT, args[0]->addr);
  add_triple_to_ptree(prog, va_make_triple(vct_copy_v, "vct_copy_v", 2, args[0], args[1]));
  return(args[0]);
}


#define VCT_OP_1(SName, CName, COp) \
static void vct_ ## CName ## _f(int *args, ptree *pt) \
{ \
  mus_long_t i; \
  vct *v = VCT_ARG_1; \
  if (v) for (i = 0; i < v->length; i++) v->data[i] COp FLOAT_ARG_2; \
  VCT_RESULT = VCT_ARG_1; \
} \
static xen_value *vct_ ## CName ## _1(ptree *prog, xen_value **args, int num_args) \
{ \
  xen_value *temp; \
  if (args[2]->type == R_INT) \
    { \
      temp = args[2]; \
      args[2] = convert_int_to_dbl(prog, args[2]); \
      free(temp); \
    } \
  return(package(prog, R_VCT, vct_ ## CName ## _f, "vct_" #CName "_f", args, 2)); \
}

VCT_OP_1(fill!, fill, =)
VCT_OP_1(scale!, scale, *=)
VCT_OP_1(offset!, offset, +=)


#define VCT_OP_2(SName, CName, COp) \
static void vct_ ## CName ## _f(int *args, ptree *pt) \
{ \
  mus_long_t i, len; \
  vct *v0 = VCT_ARG_1; \
  vct *v1 = VCT_ARG_2; \
  if ((v0) && (v1)) \
    { \
      len = v0->length; \
      if (v1->length < len) len = v1->length; \
      for (i = 0; i < len; i++) v0->data[i] COp v1->data[i]; \
    } \
  VCT_RESULT = v0; \
} \
static xen_value *vct_ ## CName ## _1(ptree *prog, xen_value **args, int num_args) \
{ \
  return(package(prog, R_VCT, vct_ ## CName ## _f, "vct_" #CName "_f", args, 2)); \
}

VCT_OP_2(add!, add, +=)
VCT_OP_2(multiply!, multiply, *=)
VCT_OP_2(subtract!, subtract, -=)


static void vct_reverse_v(vct *v, mus_long_t len)
{
  mus_long_t i, j;
  if ((v) && (len > 1))
    {
      for (i = 0, j = len - 1; i < j; i++, j--)
	{
	  mus_float_t temp;
	  temp = v->data[i];
	  v->data[i] = v->data[j];
	  v->data[j] = temp;
	}
    }
}

static void vct_reverse_0(int *args, ptree *pt) 
{
  vct *v;
  v = VCT_ARG_1;
  vct_reverse_v(v, v->length);
  VCT_RESULT = v;
}


static void vct_reverse_1(int *args, ptree *pt) 
{
  vct_reverse_v(VCT_ARG_1, INT_ARG_2);
  VCT_RESULT = VCT_ARG_1;
}


static xen_value *vct_reverse_2(ptree *prog, xen_value **args, int num_args)
{
  if (num_args == 1)
    return(package(prog, R_VCT, vct_reverse_0, "vct_reverse_0", args, 1));
  return(package(prog, R_VCT, vct_reverse_1, "vct_reverse_1", args, 2));
}


static xen_value *vct_times_1(ptree *prog, xen_value **args, int num_args)
{
  xen_value *temp;
  if (args[1]->type == R_VCT)
    {
      if (args[2]->type == R_VCT)
	return(vct_multiply_1(prog, args, num_args));
      return(vct_scale_1(prog, args, num_args));
    }
  /* reorder args for vct_scale_1 */
  temp = args[1];
  args[1] = args[2];
  args[2] = temp;
  return(vct_scale_1(prog, args, num_args));
}


static xen_value *vct_plus_1(ptree *prog, xen_value **args, int num_args)
{
  xen_value *temp;
  if (args[1]->type == R_VCT)
    {
      if (args[2]->type == R_VCT)
	return(vct_add_1(prog, args, num_args));
      return(vct_offset_1(prog, args, num_args));
    }
  /* reorder args for vct_offset_1 */
  temp = args[1];
  args[1] = args[2];
  args[2] = temp;
  return(vct_offset_1(prog, args, num_args));
}


/* ---------------- vct-move! ---------------- */

static void vct_move_0(int *args, ptree *pt) 
{
  vct *v;
  mus_long_t i, j;
  v = VCT_ARG_1;
  for (i = INT_ARG_2, j = INT_ARG_3; (j < v->length) && (i < v->length); i++, j++) 
    v->data[i] = v->data[j];
  VCT_RESULT = v;
}


static xen_value *vct_move_3(ptree *prog, xen_value **args, int num_args)
{
  return(package(prog, R_VCT, vct_move_0, "vct_move_0", args, 3));
}


/* ---------------- vct-peak ---------------- */


static void vct_peak_v(int *args, ptree *pt) 
{
  mus_long_t i;
  Double val = 0.0;
  vct *v;
  v = VCT_ARG_1;
  val = fabs(v->data[0]); 
  for (i = 1; i < v->length; i++) 
    {
      mus_float_t absv;
      absv = fabs(v->data[i]); 
      if (absv > val) val = absv;
    }
  FLOAT_RESULT = val;
}

static xen_value *vct_peak_1(ptree *prog, xen_value **args, int num_args) 
{
  return(package(prog, R_FLOAT, vct_peak_v, "vct_peak_v", args, 1));
}


#if USE_SND
/* ---------------- vct->channel ---------------- */

static void vct_to_channel_v(int *args, ptree *pt) 
{
  chan_info *cp; 
  cp = run_get_cp(4, args, pt->ints);
  if (cp)
    {
      vct *v;
      mus_long_t beg, dur;
      v = VCT_ARG_1;
      beg = INT_ARG_2;
      dur = INT_ARG_3;
#if SNDLIB_USE_FLOATS
      change_samples(beg, dur, v->data, cp, S_vct_to_channel, cp->edit_ctr);
#else
      {
	mus_long_t i;
	mus_sample_t *data;
	data = (mus_sample_t *)calloc(dur, sizeof(mus_sample_t));
	for (i = 0; i < dur; i++) data[i] = MUS_FLOAT_TO_SAMPLE(v->data[i]);
	change_samples(beg, dur, data, cp, S_vct_to_channel, cp->edit_ctr);
	free(data);
      }
#endif
    }
}


static xen_value *vct_to_channel_1(ptree *pt, xen_value **args, int num_args)
{
  xen_value *true_args[6];
  xen_value *rtn;
  int k;
  for (k = 0; k < 4; k++) true_args[k] = args[k];
  run_opt_arg(pt, args, num_args, 4, true_args);
  run_opt_arg(pt, args, num_args, 5, true_args);
  rtn = package(pt, R_BOOL, vct_to_channel_v, "vct_to_channel_v", true_args, 5);
  for (k = num_args + 1; k <= 5; k++) free(true_args[k]);
  return(rtn);
}
#endif



/* ---------------- sound-data ---------------- */

static void sound_data_length_i(int *args, ptree *pt) {INT_RESULT = SOUND_DATA_ARG_1->length;}


static xen_value *sound_data_length_1(ptree *prog, xen_value **args, int num_args)
{
  return(package(prog, R_INT, sound_data_length_i, "sound_data_length_i", args, 1));
}


static void sound_data_chans_i(int *args, ptree *pt) {INT_RESULT = SOUND_DATA_ARG_1->chans;}


static xen_value *sound_data_chans_1(ptree *prog, xen_value **args, int num_args)
{
  return(package(prog, R_INT, sound_data_chans_i, "sound_data_chans_i", args, 1));
}


static void sound_data_peak_f(int *args, ptree *pt) {FLOAT_RESULT = sound_data_peak(SOUND_DATA_ARG_1);}


static xen_value *sound_data_peak_1(ptree *prog, xen_value **args, int num_args)
{
  return(package(prog, R_FLOAT, sound_data_peak_f, "sound_data_peak_f", args, 1));
}


static void sound_data_copy_f(int *args, ptree *pt) 
{
  if (SOUND_DATA_RESULT) sound_data_free(SOUND_DATA_RESULT); /* this is how make works, so I guess we should be consistent */
  SOUND_DATA_RESULT = sound_data_copy(SOUND_DATA_ARG_1);
}


static xen_value *sound_data_copy_1(ptree *prog, xen_value **args, int num_args)
{
  args[0] = make_xen_value(R_SOUND_DATA, add_sound_data_to_ptree(prog, NULL), R_VARIABLE);
  add_obj_to_gcs(prog, R_SOUND_DATA, args[0]->addr);
  add_triple_to_ptree(prog, va_make_triple(sound_data_copy_f, "sound_data_copy_f", 2, args[0], args[1]));
  return(args[0]);
}


static void sound_data_reverse_f(int *args, ptree *pt) {SOUND_DATA_RESULT = sound_data_reverse(SOUND_DATA_ARG_1);}


static xen_value *sound_data_reverse_1(ptree *prog, xen_value **args, int num_args)
{
  return(package(prog, R_SOUND_DATA, sound_data_reverse_f, "sound_data_reverse_f", args, 1));
}


static void sound_data_ref_f(int *args, ptree *pt) {FLOAT_RESULT = SOUND_DATA_ARG_1->data[INT_ARG_2][INT_ARG_3];}


static xen_value *sound_data_ref_1(ptree *prog, xen_value **args, int num_args)
{
  return(package(prog, R_FLOAT, sound_data_ref_f, "sound_data_ref_f", args, 3));
}


static void sound_data_nf(int *args, ptree *pt) {FLOAT_RESULT = SOUND_DATA_ARG_1->data[INT_ARG_2][INT_ARG_3];}


static xen_value *sound_data_n(ptree *prog, xen_value **args, int num_args, xen_value *sf)
{
  /* this is handling the sound-data-as-applicable-func stuff */
  /* (let ((sd (make-sound-data 2 2))) (run (lambda () (sd 0 0)))) */
  if (args[0]) free(args[0]);
  args[0] = make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, 0.0), R_VARIABLE);
  add_triple_to_ptree(prog, va_make_triple(sound_data_nf, "sound_data_nf", 4, args[0], sf, args[1], args[2]));
  return(args[0]);
}


static void sound_data_set_f(int *args, ptree *pt) 
{
  SOUND_DATA_ARG_1->data[INT_ARG_2][INT_ARG_3] = FLOAT_ARG_4;
  /* FLOAT_RESULT = FLOAT_ARG_4; */
}

static void sound_data_set_i(int *args, ptree *pt) 
{
  SOUND_DATA_ARG_1->data[INT_ARG_2][INT_ARG_3] = (mus_float_t)INT_ARG_4;
  /* FLOAT_RESULT = (Double)INT_ARG_4; */
}


static void sound_data_set_1(ptree *prog, xen_value *in_v, xen_value *in_v1, xen_value *in_v2, xen_value *v)
{
  xen_var *var;
  var = find_var_in_ptree_via_addr(prog, in_v->type, in_v->addr);
  if (var) var->unclean = true;
  /* v->type known to be R_FLOAT (generalized set insists) */
  add_triple_to_ptree(prog, va_make_triple(sound_data_set_f, "sound_data_set_f", 5, NULL, in_v, in_v1, in_v2, v));
}


static xen_value *sound_data_set_2(ptree *prog, xen_value **args, int num_args)
{
  xen_var *var;
  var = find_var_in_ptree_via_addr(prog, args[1]->type, args[1]->addr);
  if (var) var->unclean = true;
  if (args[4]->type == R_FLOAT)
    return(package(prog, R_FLOAT, sound_data_set_f, "sound_data_set_f", args, 4));
  return(package(prog, R_FLOAT, sound_data_set_i, "sound_data_set_i", args, 4));
}


static void make_sound_data_v(int *args, ptree *pt) 
{
  if (SOUND_DATA_RESULT) sound_data_free(SOUND_DATA_RESULT);
  SOUND_DATA_RESULT = c_make_sound_data(INT_ARG_1, INT_ARG_2);
}


static xen_value *make_sound_data_1(ptree *prog, xen_value **args, int num_args)
{
  args[0] = make_xen_value(R_SOUND_DATA, add_sound_data_to_ptree(prog, NULL), R_VARIABLE);
  add_obj_to_gcs(prog, R_SOUND_DATA, args[0]->addr);
  add_triple_to_ptree(prog, va_make_triple(make_sound_data_v, "make_sound_data_v", 3, args[0], args[1], args[2]));
  return(args[0]);
}


static void sound_data_scale_f(int *args, ptree *pt) {SOUND_DATA_RESULT = sound_data_scale(SOUND_DATA_ARG_1, FLOAT_ARG_2);}


static xen_value *sound_data_scale_1(ptree *prog, xen_value **args, int num_args)
{
  xen_value *temp;
  if (args[2]->type == R_INT)
    {
      temp = args[2];
      args[2] = convert_int_to_dbl(prog, args[2]);
      free(temp);
    }
  return(package(prog, R_SOUND_DATA, sound_data_scale_f, "sound_data_scale_f", args, 2));
}


static void sound_data_offset_f(int *args, ptree *pt) {SOUND_DATA_RESULT = sound_data_offset(SOUND_DATA_ARG_1, FLOAT_ARG_2);}


static xen_value *sound_data_offset_1(ptree *prog, xen_value **args, int num_args)
{
  xen_value *temp;
  if (args[2]->type == R_INT)
    {
      temp = args[2];
      args[2] = convert_int_to_dbl(prog, args[2]);
      free(temp);
    }
  return(package(prog, R_SOUND_DATA, sound_data_offset_f, "sound_data_offset_f", args, 2));
}


static void sound_data_fill_f(int *args, ptree *pt) {SOUND_DATA_RESULT = sound_data_fill(SOUND_DATA_ARG_1, FLOAT_ARG_2);}


static xen_value *sound_data_fill_1(ptree *prog, xen_value **args, int num_args)
{
  xen_value *temp;
  if (args[2]->type == R_INT)
    {
      temp = args[2];
      args[2] = convert_int_to_dbl(prog, args[2]);
      free(temp);
    }
  return(package(prog, R_SOUND_DATA, sound_data_fill_f, "sound_data_fill_f", args, 2));
}


static void sound_data_add_f(int *args, ptree *pt) {SOUND_DATA_RESULT = sound_data_add(SOUND_DATA_ARG_1, SOUND_DATA_ARG_2);}


static xen_value *sound_data_add_1(ptree *prog, xen_value **args, int num_args)
{
  return(package(prog, R_SOUND_DATA, sound_data_add_f, "sound_data_add_f", args, 2));
}


static void sound_data_multiply_f(int *args, ptree *pt) {SOUND_DATA_RESULT = sound_data_multiply(SOUND_DATA_ARG_1, SOUND_DATA_ARG_2);}

static xen_value *sound_data_multiply_1(ptree *prog, xen_value **args, int num_args)
{
  return(package(prog, R_SOUND_DATA, sound_data_multiply_f, "sound_data_multiply_f", args, 2));
}


static xen_value *sound_data_times_1(ptree *prog, xen_value **args, int num_args)
{
  xen_value *temp;
  if (args[1]->type == R_SOUND_DATA)
    {
      if (args[2]->type == R_SOUND_DATA)
	return(sound_data_multiply_1(prog, args, num_args));
      return(sound_data_scale_1(prog, args, num_args));
    }
  /* reorder args for sound_data_scale_1 */
  temp = args[1];
  args[1] = args[2];
  args[2] = temp;
  return(sound_data_scale_1(prog, args, num_args));
}


static xen_value *sound_data_plus_1(ptree *prog, xen_value **args, int num_args)
{
  xen_value *temp;
  if (args[1]->type == R_SOUND_DATA)
    {
      if (args[2]->type == R_SOUND_DATA)
	return(sound_data_add_1(prog, args, num_args));
      return(sound_data_offset_1(prog, args, num_args));
    }
  /* reorder args for sound_data_offset_1 */
  temp = args[1];
  args[1] = args[2];
  args[2] = temp;
  return(sound_data_offset_1(prog, args, num_args));
}


static void sound_data_to_vct_v(int *args, ptree *pt)
{
  vct *v;
  sound_data *sd;
  int chan;
  mus_long_t len;
  v = VCT_ARG_3;
  sd = SOUND_DATA_ARG_1;
  chan = INT_ARG_2;
  if (sd->length < v->length) 
    len = sd->length; 
  else len = v->length;
  memcpy((void *)(v->data), (void *)(sd->data[chan]), len * sizeof(mus_float_t));
  VCT_RESULT = v;
}


static xen_value *sound_data_to_vct_1(ptree *prog, xen_value **args, int num_args)
{
  return(package(prog, R_VCT, sound_data_to_vct_v, "sound_data_to_vct_v", args, 3));
}

static void vct_to_sound_data_v(int *args, ptree *pt)
{
  vct *v;
  sound_data *sd;
  int chan;
  mus_long_t len;
  v = VCT_ARG_1;
  sd = SOUND_DATA_ARG_2;
  chan = INT_ARG_3;
  if (sd->length < v->length) 
    len = sd->length; 
  else len = v->length;
  memcpy((void *)(sd->data[chan]), (void *)(v->data), len * sizeof(mus_float_t));
  SOUND_DATA_RESULT = sd;
}


static xen_value *vct_to_sound_data_1(ptree *prog, xen_value **args, int num_args)
{
  return(package(prog, R_SOUND_DATA, vct_to_sound_data_v, "vct_to_sound_data_v", args, 3));
}



/* ---------------- CLM stuff ---------------- */


static xen_value *mus_generator_p_1(ptree *prog, xen_value **args, int num_args)
{
  return(make_xen_value(R_BOOL, add_int_to_ptree(prog, (args[1]->type == R_CLM) || (CLM_STRUCT_P(args[1]->type))), R_CONSTANT));
}


#define GEN_P(Name) \
  static void Name ## _0p(int *args, ptree *pt) {BOOL_RESULT = (Int)mus_ ## Name ## _p(CLM_ARG_1);} \
  static xen_value * Name ## _p(ptree *prog, xen_value **args, int num_args) \
  { \
    if (args[1]->type == R_CLM) \
      { \
        if (mus_ ## Name ## _p(prog->clms[args[1]->addr])) \
          return(make_xen_value(R_BOOL, add_int_to_ptree(prog, (Int)true), R_CONSTANT)); \
        return(package(prog, R_BOOL, Name ## _0p, #Name "_0p", args, 1)); \
      } \
    return(make_xen_value(R_BOOL, add_int_to_ptree(prog, (Int)false), R_CONSTANT)); \
  }


#define GEN2_0(Name) \
  static void Name ## _0f(int *args, ptree *pt) {FLOAT_RESULT = mus_ ## Name (CLM_ARG_1, 0.0);}  

#define GEN1_0(Name) \
  static void Name ## _0f(int *args, ptree *pt) {FLOAT_RESULT = mus_ ## Name (CLM_ARG_1);}  

#define GEN2_1(Name) \
  static void Name ## _1f(int *args, ptree *pt) {FLOAT_RESULT = mus_ ## Name (CLM_ARG_1, FLOAT_ARG_2);}

#define mus_filtered_comb_no_input(Ptr) mus_filtered_comb_unmodulated(Ptr, 0.0)
#define mus_ssb_am_no_input(Ptr) mus_ssb_am_unmodulated(Ptr, 0.0)

#define GEN3(Name) \
  static void Name ## _0f(int *args, ptree *pt) {FLOAT_RESULT = mus_ ## Name ## _no_input(CLM_ARG_1);} \
  static void Name ## _1f(int *args, ptree *pt) {FLOAT_RESULT = mus_ ## Name ## _unmodulated(CLM_ARG_1, FLOAT_ARG_2);} \
  static void Name ## _2f(int *args, ptree *pt) {FLOAT_RESULT = mus_ ## Name (CLM_ARG_1, FLOAT_ARG_2, FLOAT_ARG_3);} \
  GEN_P(Name) \
  static xen_value * Name ## _1(ptree *prog, xen_value **args, int num_args) \
  { \
    if ((num_args > 1) && (args[2]->type == R_INT)) single_to_float(prog, args, 2); \
    if ((num_args > 2) && (args[3]->type == R_INT)) single_to_float(prog, args, 3); \
    if (num_args == 1) return(package(prog, R_FLOAT, Name ## _0f, #Name "_0f", args, 1)); \
    if (num_args == 2) return(package(prog, R_FLOAT, Name ## _1f, #Name "_1f", args, 2)); \
    return(package(prog, R_FLOAT, Name ## _2f, #Name "_2f", args, 3)); \
  }



static void polyshape_0f(int *args, ptree *pt) {FLOAT_RESULT = mus_polyshape_no_input(CLM_ARG_1);}
static void polyshape_1f(int *args, ptree *pt) {FLOAT_RESULT = mus_polyshape_unmodulated(CLM_ARG_1, FLOAT_ARG_2);}
static void polyshape_1fn(int *args, ptree *pt) {FLOAT_RESULT = mus_polyshape_fm(CLM_ARG_1, FLOAT_ARG_3);}
static void polyshape_2f(int *args, ptree *pt) {FLOAT_RESULT = mus_polyshape(CLM_ARG_1, FLOAT_ARG_2, FLOAT_ARG_3);}

static void polyshape_1fn_mult(int *args, ptree *pt) {FLOAT_RESULT = FLOAT_ARG_4 * mus_polyshape_fm(CLM_ARG_1, FLOAT_ARG_3);}
static void polyshape_1fn_env(int *args, ptree *pt) {FLOAT_RESULT = mus_env_linear(CLM_ARG_4) * mus_polyshape_fm(CLM_ARG_1, FLOAT_ARG_3);}

GEN_P(polyshape)

static xen_value *polyshape_1(ptree *prog, xen_value **args, int num_args)
{
  if ((num_args > 1) && (args[2]->type == R_INT)) single_to_float(prog, args, 2);
  if ((num_args > 2) && (args[3]->type == R_INT)) single_to_float(prog, args, 3);
  if (num_args == 1) return(package(prog, R_FLOAT, polyshape_0f, "polyshape_0f", args, 1));
  if (num_args == 2) return(package(prog, R_FLOAT, polyshape_1f, "polyshape_1f", args, 2));
  if ((args[2]->constant == R_CONSTANT) && (prog->dbls[args[2]->addr] == 1.0)) return(package(prog, R_FLOAT, polyshape_1fn, "polyshape_1fn", args, 3));
  return(package(prog, R_FLOAT, polyshape_2f, "polyshape_2f", args, 3));
}


static void oscil_0f_1(int *args, ptree *pt) {FLOAT_RESULT = mus_oscil_unmodulated(CLM_ARG_1);}

static void oscil_0f_1_env(int *args, ptree *pt) {FLOAT_RESULT = mus_env_linear(CLM_ARG_2) * mus_oscil_unmodulated(CLM_ARG_1);}

static void oscil_0f_1_mult(int *args, ptree *pt) {FLOAT_RESULT = FLOAT_ARG_2 * mus_oscil_unmodulated(CLM_ARG_1);}

static void oscil_1f_1(int *args, ptree *pt) {FLOAT_RESULT = mus_oscil_fm(CLM_ARG_1, FLOAT_ARG_2);}

static void oscil_1f_1_env(int *args, ptree *pt) {FLOAT_RESULT = mus_env_linear(CLM_ARG_3) * mus_oscil_fm(CLM_ARG_1, FLOAT_ARG_2);}

static void oscil_1f_1_mult(int *args, ptree *pt) {FLOAT_RESULT = FLOAT_ARG_3 * mus_oscil_fm(CLM_ARG_1, FLOAT_ARG_2);}

static void oscil_1f_2(int *args, ptree *pt) {FLOAT_RESULT = mus_oscil_fm(CLM_ARG_1, FLOAT_ARG_2 + FLOAT_ARG_3);}
static void oscil_1f_2m(int *args, ptree *pt) {FLOAT_RESULT = mus_oscil_fm(CLM_ARG_1, FLOAT_ARG_2 * FLOAT_ARG_3);}

static void oscil_1f_2_env(int *args, ptree *pt) {FLOAT_RESULT = mus_env_linear(CLM_ARG_4) * mus_oscil_fm(CLM_ARG_1, FLOAT_ARG_2 + FLOAT_ARG_3);}
static void oscil_1f_2m_env(int *args, ptree *pt) {FLOAT_RESULT = mus_env_linear(CLM_ARG_4) * mus_oscil_fm(CLM_ARG_1, FLOAT_ARG_2 * FLOAT_ARG_3);}

static void oscil_1f_2_mult(int *args, ptree *pt) {FLOAT_RESULT = FLOAT_ARG_4 * mus_oscil_fm(CLM_ARG_1, FLOAT_ARG_2 + FLOAT_ARG_3);}
static void oscil_1f_2m_mult(int *args, ptree *pt) {FLOAT_RESULT = FLOAT_ARG_4 * mus_oscil_fm(CLM_ARG_1, FLOAT_ARG_2 * FLOAT_ARG_3);}

static void oscil_1f_3(int *args, ptree *pt) {FLOAT_RESULT = mus_oscil_fm(CLM_ARG_1, FLOAT_ARG_2 + FLOAT_ARG_3 + FLOAT_ARG_4);}
static void oscil_1f_3ma(int *args, ptree *pt) {FLOAT_RESULT = mus_oscil_fm(CLM_ARG_1, (FLOAT_ARG_2 * FLOAT_ARG_3) + FLOAT_ARG_4);}
static void oscil_1f_3ma_mult(int *args, ptree *pt) {FLOAT_RESULT = FLOAT_ARG_5 * mus_oscil_fm(CLM_ARG_1, (FLOAT_ARG_2 * FLOAT_ARG_3) + FLOAT_ARG_4);}
static void oscil_1f_3ma_env(int *args, ptree *pt) {FLOAT_RESULT = mus_env_linear(CLM_ARG_5) * mus_oscil_fm(CLM_ARG_1, (FLOAT_ARG_2 * FLOAT_ARG_3) + FLOAT_ARG_4);}
/* (let ((gen (make-oscil 100)) (x 2.0) (y 3.0) (z 4.0)) (run (lambda () (oscil gen (+ x (* y z))))) gen) */
static void oscil_1f_3_mult(int *args, ptree *pt) {FLOAT_RESULT = FLOAT_ARG_5 * mus_oscil_fm(CLM_ARG_1, FLOAT_ARG_2 + FLOAT_ARG_3 + FLOAT_ARG_4);}
static void oscil_1f_3_env(int *args, ptree *pt) {FLOAT_RESULT = mus_env_linear(CLM_ARG_5) * mus_oscil_fm(CLM_ARG_1, FLOAT_ARG_2 + FLOAT_ARG_3 + FLOAT_ARG_4);}

static void oscil_2f_1(int *args, ptree *pt) {FLOAT_RESULT = mus_oscil_pm(CLM_ARG_1, FLOAT_ARG_3);}

static void oscil_2f(int *args, ptree *pt) {FLOAT_RESULT = mus_oscil(CLM_ARG_1, FLOAT_ARG_2, FLOAT_ARG_3);}

static void oscil_0f_vect(int *args, ptree *pt) {FLOAT_RESULT = mus_oscil_unmodulated(VECT_ARG_1->data.gens[INT_ARG_2]);}
static void oscil_0f_vect_mult(int *args, ptree *pt) {FLOAT_RESULT = FLOAT_ARG_3 * mus_oscil_unmodulated(VECT_ARG_1->data.gens[INT_ARG_2]);}
static void oscil_0f_vect_env(int *args, ptree *pt) {FLOAT_RESULT = mus_env_linear(CLM_ARG_3) * mus_oscil_unmodulated(VECT_ARG_1->data.gens[INT_ARG_2]);}

static void oscil_1f_vect(int *args, ptree *pt) {FLOAT_RESULT = mus_oscil_fm(VECT_ARG_1->data.gens[INT_ARG_2], FLOAT_ARG_3);}
static void oscil_1f_vect_mult(int *args, ptree *pt) {FLOAT_RESULT = FLOAT_ARG_4 * mus_oscil_fm(VECT_ARG_1->data.gens[INT_ARG_2], FLOAT_ARG_3);}
static void oscil_1f_vect_env(int *args, ptree *pt) {FLOAT_RESULT = mus_env_linear(CLM_ARG_4) * mus_oscil_fm(VECT_ARG_1->data.gens[INT_ARG_2], FLOAT_ARG_3);}

GEN_P(oscil)

static xen_value *oscil_1(ptree *prog, xen_value **args, int num_args)
{
  if ((num_args > 1) && (args[2]->type == R_INT)) single_to_float(prog, args, 2);
  if ((num_args > 2) && (args[3]->type == R_INT)) single_to_float(prog, args, 3);

  if ((num_args == 1) || 
      ((num_args == 2) && 
       (args[2]->constant == R_CONSTANT) && 
       (prog->dbls[args[2]->addr] == 0.0)))
    {
      if (prog->triple_ctr > 0)
	{
	  triple *prev_op;
	  prev_op = prog->program[prog->triple_ctr - 1];
	  if ((prev_op->function == vector_ref_c) &&
	      (prev_op->types[0] == R_CLM) &&
	      (prev_op->args[0] == args[1]->addr) &&
	      ((find_var_in_ptree_via_addr(prog, R_CLM, prev_op->args[0])) == NULL))
	    {
	      xen_value *new0;
	      new0 = add_temporary_var_to_ptree(prog, R_FLOAT);
	      prev_op->args[0] = new0->addr;
	      prev_op->types[0] = R_FLOAT;
	      prev_op->function = oscil_0f_vect;
	      prev_op->op_name = "oscil_0f_vect";
	      return(new0);
	    }
	}
      return(package(prog, R_FLOAT, oscil_0f_1, "oscil_0f_1", args, 1));
    }
 
  if ((num_args == 2) || 
      ((num_args == 3) && 
       (args[3]->constant == R_CONSTANT) && 
       (prog->dbls[args[3]->addr] == 0.0)))
    {
      if (prog->triple_ctr > 0)
	{
	  triple *prev_op;
	  prev_op = prog->program[prog->triple_ctr - 1];
	  if (((prev_op->function == add_f2) ||
	       (prev_op->function == multiply_f2) ||
	       (prev_op->function == multiply_add_f2) ||
	       (prev_op->function == add_f3)) &&
	      (prev_op->args[0] == args[2]->addr) &&
	      (find_var_in_ptree_via_addr(prog, R_FLOAT, prev_op->args[0])) == NULL)
	    {
	      if ((prev_op->function == add_f2) || 
		  (prev_op->function == multiply_f2))
		{
		  prev_op->types = (int *)realloc(prev_op->types, 4 * sizeof(int));
		  prev_op->args = (int *)realloc(prev_op->args, 4 * sizeof(int));
		  prev_op->args[3] = prev_op->args[2];
		  prev_op->args[2] = prev_op->args[1];
		  prev_op->types[3] = R_FLOAT;
		  prev_op->args[1] = args[1]->addr;
		  prev_op->types[1] = R_CLM;
		  prev_op->num_args = 4;
		  if (prev_op->function == add_f2)
		    {
		      prev_op->function = oscil_1f_2;
		      prev_op->op_name = "oscil_1f_2";
		    }
		  else
		    {
		      prev_op->function = oscil_1f_2m;
		      prev_op->op_name = "oscil_1f_2m";
		    }
#if WITH_COUNTERS
		  prev_op->func_loc = get_func_loc(prev_op->function, prev_op->op_name);
#endif				  
		  return(make_xen_value(R_FLOAT, prev_op->args[0], R_TEMPORARY));
		}
	      /* else it's add_f3 or multiply_add_f2 */
	      prev_op->types = (int *)realloc(prev_op->types, 5 * sizeof(int));
	      prev_op->args = (int *)realloc(prev_op->args, 5 * sizeof(int));
	      prev_op->args[4] = prev_op->args[3];
	      prev_op->args[3] = prev_op->args[2];
	      prev_op->args[2] = prev_op->args[1];
	      prev_op->types[4] = R_FLOAT;
	      prev_op->args[1] = args[1]->addr;
	      prev_op->types[1] = R_CLM;
	      prev_op->num_args = 5;
	      if (prev_op->function == add_f3)
		{
		  prev_op->function = oscil_1f_3;
		  prev_op->op_name = "oscil_1f_3";
		}
	      else
		{
		  prev_op->function = oscil_1f_3ma;
		  prev_op->op_name = "oscil_1f_3ma";
		}
#if WITH_COUNTERS
	      prev_op->func_loc = get_func_loc(prev_op->function, prev_op->op_name);
#endif				  
	      return(make_xen_value(R_FLOAT, prev_op->args[0], R_TEMPORARY));
	    }

	  if ((prev_op->function == vector_ref_c) &&
	      (prev_op->types[0] == R_CLM) &&
	      (prev_op->args[0] == args[1]->addr) &&
	      ((find_var_in_ptree_via_addr(prog, R_CLM, prev_op->args[0])) == NULL))
	    {
	      xen_value *new0;
	      new0 = add_temporary_var_to_ptree(prog, R_FLOAT);
	      prev_op->types = (int *)realloc(prev_op->types, 4 * sizeof(int));
	      prev_op->args = (int *)realloc(prev_op->args, 4 * sizeof(int));
	      prev_op->num_args = 4;
	      prev_op->args[0] = new0->addr;
	      prev_op->types[0] = R_FLOAT;
	      prev_op->function = oscil_1f_vect;
	      prev_op->op_name = "oscil_1f_vect";
	      prev_op->args[3] = args[2]->addr;
	      prev_op->types[3] = R_FLOAT;
	      return(new0);
	    }

	}
      return(package(prog, R_FLOAT, oscil_1f_1, "oscil_1f_1", args, 2));
    }

  if ((num_args == 3) && (args[2]->constant == R_CONSTANT) && (prog->dbls[args[2]->addr] == 0.0))
    return(package(prog, R_FLOAT, oscil_2f_1, "oscil_2f_1", args, 3));

  return(package(prog, R_FLOAT, oscil_2f, "oscil_2f", args, 3));
}


static void formant_0f(int *args, ptree *pt) {FLOAT_RESULT = mus_formant(CLM_ARG_1, 0.0);}

static void formant_1f(int *args, ptree *pt) {FLOAT_RESULT = mus_formant(CLM_ARG_1, FLOAT_ARG_2);}
static void formant_1f_add(int *args, ptree *pt) {FLOAT_RESULT = FLOAT_ARG_3 + mus_formant(CLM_ARG_1, FLOAT_ARG_2);}
static void formant_1f_mult(int *args, ptree *pt) {FLOAT_RESULT = FLOAT_ARG_3 * mus_formant(CLM_ARG_1, FLOAT_ARG_2);}
static void formant_1f_env(int *args, ptree *pt) {FLOAT_RESULT = mus_env_linear(CLM_ARG_3) * mus_formant(CLM_ARG_1, FLOAT_ARG_2);}

static void formant_1f_vect(int *args, ptree *pt) {FLOAT_RESULT = mus_formant(VECT_ARG_1->data.gens[INT_ARG_2], FLOAT_ARG_3);}
static void formant_1f_vect_mult(int *args, ptree *pt) {FLOAT_RESULT = FLOAT_ARG_4 * mus_formant(VECT_ARG_1->data.gens[INT_ARG_2], FLOAT_ARG_3);}
static void formant_1f_vect_env(int *args, ptree *pt) {FLOAT_RESULT = mus_env_linear(CLM_ARG_4) * mus_formant(VECT_ARG_1->data.gens[INT_ARG_2], FLOAT_ARG_3);}

static void formant_1f_sma(int *args, ptree *pt) {FLOAT_RESULT = mus_formant(CLM_ARG_5, FLOAT_ARG_4 + FLOAT_ARG_3 * (FLOAT_ARG_1 - FLOAT_ARG_2));}

static void formant_2f(int *args, ptree *pt) {FLOAT_RESULT = mus_formant_with_frequency(CLM_ARG_1, FLOAT_ARG_2, FLOAT_ARG_3);}

GEN_P(formant)

static xen_value *formant_1(ptree *prog, xen_value **args, int num_args)
{
  if ((num_args > 1) && (args[2]->type == R_INT)) single_to_float(prog, args, 2);
  if ((num_args > 2) && (args[3]->type == R_INT)) single_to_float(prog, args, 3);
  if (num_args == 1)
    return(package(prog, R_FLOAT, formant_0f, "formant_0f", args, 1));

  if (num_args == 2)
    {
      if (prog->triple_ctr > 0)
	{
	  triple *prev_op;
	  prev_op = prog->program[prog->triple_ctr - 1];
	  if ((prev_op->function == subtract_f2_mult_add) &&
	      (prev_op->args[0] == args[2]->addr) &&
	      ((find_var_in_ptree_via_addr(prog, R_FLOAT, prev_op->args[0])) == NULL))
	    {
	      prev_op->types = (int *)realloc(prev_op->types, 6 * sizeof(int));
	      prev_op->args = (int *)realloc(prev_op->args, 6 * sizeof(int));
	      prev_op->num_args = 6;
	      prev_op->function = formant_1f_sma;
	      prev_op->op_name = "formant_1f_sma";
#if WITH_COUNTERS
	      prev_op->func_loc = get_func_loc(prev_op->function, prev_op->op_name);
#endif				  
	      prev_op->types[5] = R_CLM;
	      prev_op->args[5] = args[1]->addr;
	      return(make_xen_value(R_FLOAT, prev_op->args[0], R_TEMPORARY));
	    }

	  if ((prev_op->function == vector_ref_c) &&
	      (prev_op->types[0] == R_CLM) &&
	      (prev_op->args[0] == args[1]->addr) &&
	      ((find_var_in_ptree_via_addr(prog, R_CLM, prev_op->args[0])) == NULL))
	    {
	      /* (let ((v (vector (make-formant 330)))) (run (* 0.5 (formant (vector-ref v 0) .1)))) */
	      xen_value *new0;
	      new0 = add_temporary_var_to_ptree(prog, R_FLOAT);
	      prev_op->types = (int *)realloc(prev_op->types, 4 * sizeof(int));
	      prev_op->args = (int *)realloc(prev_op->args, 4 * sizeof(int));
	      prev_op->num_args = 4;
	      prev_op->args[0] = new0->addr;
	      prev_op->types[0] = R_FLOAT;
	      prev_op->function = formant_1f_vect;
	      prev_op->op_name = "formant_1f_vect";
	      prev_op->args[3] = args[2]->addr;
	      prev_op->types[3] = R_FLOAT;
	      return(new0);
	    }
	}

      return(package(prog, R_FLOAT, formant_1f, "formant_1f", args, 2));
    }
  return(package(prog, R_FLOAT, formant_2f, "formant_2f", args, 3));
}


static void firmant_0f(int *args, ptree *pt) {FLOAT_RESULT = mus_firmant(CLM_ARG_1, 0.0);}

static void firmant_1f(int *args, ptree *pt) {FLOAT_RESULT = mus_firmant(CLM_ARG_1, FLOAT_ARG_2);}
static void firmant_1f_mult(int *args, ptree *pt) {FLOAT_RESULT = FLOAT_ARG_3 * mus_firmant(CLM_ARG_1, FLOAT_ARG_2);}
static void firmant_1f_env(int *args, ptree *pt) {FLOAT_RESULT = mus_env_linear(CLM_ARG_3) * mus_firmant(CLM_ARG_1, FLOAT_ARG_2);}

static void firmant_2f(int *args, ptree *pt) {FLOAT_RESULT = mus_firmant_with_frequency(CLM_ARG_1, FLOAT_ARG_2, FLOAT_ARG_3);}
static void firmant_2f_mult(int *args, ptree *pt) {FLOAT_RESULT = FLOAT_ARG_4 * mus_firmant_with_frequency(CLM_ARG_1, FLOAT_ARG_2, FLOAT_ARG_3);}
static void firmant_2f_env(int *args, ptree *pt) {FLOAT_RESULT = mus_env_linear(CLM_ARG_4) * mus_firmant_with_frequency(CLM_ARG_1, FLOAT_ARG_2, FLOAT_ARG_3);}

static void firmant_1f_vect(int *args, ptree *pt) {FLOAT_RESULT = mus_firmant(VECT_ARG_1->data.gens[INT_ARG_2], FLOAT_ARG_3);}
static void firmant_1f_vect_mult(int *args, ptree *pt) {FLOAT_RESULT = FLOAT_ARG_4 * mus_firmant(VECT_ARG_1->data.gens[INT_ARG_2], FLOAT_ARG_3);}
static void firmant_1f_vect_env(int *args, ptree *pt) {FLOAT_RESULT = mus_env_linear(CLM_ARG_4) * mus_firmant(VECT_ARG_1->data.gens[INT_ARG_2], FLOAT_ARG_3);}

GEN_P(firmant)

static xen_value *firmant_1(ptree *prog, xen_value **args, int num_args)
{
  if ((num_args > 1) && (args[2]->type == R_INT)) single_to_float(prog, args, 2);
  if ((num_args > 2) && (args[3]->type == R_INT)) single_to_float(prog, args, 3);

  if (num_args == 1)
    return(package(prog, R_FLOAT, firmant_0f, "firmant_0f", args, 1));

  if (num_args == 2)
    {
      if (prog->triple_ctr > 0)
	{
	  triple *prev_op;
	  prev_op = prog->program[prog->triple_ctr - 1];

	  if ((prev_op->function == vector_ref_c) &&
	      (prev_op->types[0] == R_CLM) &&
	      (prev_op->args[0] == args[1]->addr) &&
	      ((find_var_in_ptree_via_addr(prog, R_CLM, prev_op->args[0])) == NULL))
	    {
	      /* (let ((v (vector (make-firmant 330)))) (run (* 0.5 (formant (vector-ref v 0) .1)))) */
	      xen_value *new0;
	      new0 = add_temporary_var_to_ptree(prog, R_FLOAT);
	      prev_op->types = (int *)realloc(prev_op->types, 4 * sizeof(int));
	      prev_op->args = (int *)realloc(prev_op->args, 4 * sizeof(int));
	      prev_op->num_args = 4;
	      prev_op->args[0] = new0->addr;
	      prev_op->types[0] = R_FLOAT;
	      prev_op->function = firmant_1f_vect;
	      prev_op->op_name = "firmant_1f_vect";
	      prev_op->args[3] = args[2]->addr;
	      prev_op->types[3] = R_FLOAT;
	      return(new0);
	    }
	}
      return(package(prog, R_FLOAT, firmant_1f, "firmant_1f", args, 2));
    }
  return(package(prog, R_FLOAT, firmant_2f, "firmant_2f", args, 3));
}


#define GEN2(Name) \
  GEN2_0(Name) \
  GEN2_1(Name) \
  GEN_P(Name) \
  static xen_value * Name ## _1(ptree *prog, xen_value **args, int num_args) \
  { \
    if ((num_args > 1) && (args[2]->type == R_INT)) single_to_float(prog, args, 2); \
    if (num_args == 1) return(package(prog, R_FLOAT, Name ## _0f, #Name "_0f", args, 1)); \
    return(package(prog, R_FLOAT, Name ## _1f, #Name "_1f", args, 2)); \
  }

#define GEN2_OPT(Name) \
  static void Name ## _0f(int *args, ptree *pt) {FLOAT_RESULT = mus_ ## Name ## _unmodulated (CLM_ARG_1);} \
  GEN2_1(Name) \
  GEN_P(Name) \
  static xen_value * Name ## _1(ptree *prog, xen_value **args, int num_args) \
  { \
    if ((num_args > 1) && (args[2]->type == R_INT)) single_to_float(prog, args, 2); \
    if (num_args == 1) return(package(prog, R_FLOAT, Name ## _0f, #Name "_0f", args, 1)); \
    return(package(prog, R_FLOAT, Name ## _1f, #Name "_1f", args, 2)); \
  }

#define GEN1(Name) \
  GEN1_0(Name) \
  GEN_P(Name) \
  static xen_value * Name ## _1(ptree *prog, xen_value **args, int num_args) \
  { \
    return(package(prog, R_FLOAT, Name ## _0f, #Name "_0f", args, 1)); \
  }


/* GEN1(env) */

static bool env_is_constant(mus_any *e)
{
  mus_float_t *data;
  if (mus_env_breakpoints(e) == 1) return(true);
  if (mus_env_breakpoints(e) > 2) return(false);
  data = mus_data(e);
  return(data[1] == data[3]);
}


GEN_P(env)

static void env_0f(int *args, ptree *pt) {FLOAT_RESULT = mus_env(CLM_ARG_1);}  
static void env_linear_0f(int *args, ptree *pt) {FLOAT_RESULT = mus_env_linear(CLM_ARG_1);}  
static void env_linear_0f_mult(int *args, ptree *pt) {FLOAT_RESULT = FLOAT_ARG_2 * mus_env_linear(CLM_ARG_1);}  
static void env_linear_0f_env(int *args, ptree *pt) {FLOAT_RESULT = mus_env_linear(CLM_ARG_2) * mus_env_linear(CLM_ARG_1);}  
static void env_exponential_0f(int *args, ptree *pt) {FLOAT_RESULT = mus_env_exponential(CLM_ARG_1);}  

static void env_linear_2f(int *args, ptree *pt) 
{
  FLOAT_RESULT = mus_env_linear(CLM_ARG_1); /* the original (prev_op) */
  FLOAT_ARG_2 = mus_env_linear(CLM_ARG_3);  /* 2nd (independent, arg[0] -> arg[2], arg[1] -> arg[3]) */
}  


static xen_value *env_1(ptree *prog, xen_value **args, int num_args)
{
  if ((args[1]->type == R_CLM) &&
      (mus_env_p(prog->clms[args[1]->addr])))
    {
      /* this sort of optimization assumes the ptree is not saved,
       *   and that run-time make-env values are not already set?!?
       */
      mus_env_t style;
      mus_any *e;
      e = prog->clms[args[1]->addr];
      style = mus_env_type(e);

      /* if it's a constant, return its value as in (+ 1 2) */
      if (env_is_constant(e))
	return(make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, mus_env(e)), R_CONSTANT)); /* this assumes no set scaler/offset etc */

      if (style == MUS_ENV_LINEAR)
	{
	  if (prog->triple_ctr > 0)
	    {
	      triple *prev_op;
	      prev_op = prog->program[prog->triple_ctr - 1];
	      if (prev_op->function == env_linear_0f)
		{
		  xen_value *new0;
		  prev_op->types = (int *)realloc(prev_op->types, 4 * sizeof(int));
		  prev_op->args = (int *)realloc(prev_op->args, 4 * sizeof(int));
		  prev_op->num_args = 4;
		  prev_op->function = env_linear_2f;
		  prev_op->op_name = "env_linear_2f";
#if WITH_COUNTERS
		  prev_op->func_loc = get_func_loc(prev_op->function, prev_op->op_name);
#endif				  
		  new0 = add_temporary_var_to_ptree(prog, R_FLOAT);
		  prev_op->args[2] = new0->addr;
		  prev_op->types[2] = R_FLOAT;
		  prev_op->args[3] = args[1]->addr;
		  prev_op->types[3] = R_CLM;
		  return(new0);
		}
	    }

	  return(package(prog, R_FLOAT, env_linear_0f, "env_linear_0f", args, 1));
	}

      if (style == MUS_ENV_EXPONENTIAL)
	return(package(prog, R_FLOAT, env_exponential_0f, "env_exponential_0f", args, 1));
    }

  return(package(prog, R_FLOAT, env_0f, "env_0f", args, 1));
}


GEN1(readin)
GEN3(filtered_comb)


static void delay_0f(int *args, ptree *pt) {FLOAT_RESULT = mus_delay_unmodulated(CLM_ARG_1, 0.0);}
static void delay_1f(int *args, ptree *pt) {FLOAT_RESULT = mus_delay_unmodulated(CLM_ARG_1, FLOAT_ARG_2);}
static void delay_2f(int *args, ptree *pt) {FLOAT_RESULT = mus_delay(CLM_ARG_1, FLOAT_ARG_2, FLOAT_ARG_3);}
static void delay_1f_env(int *args, ptree *pt) {FLOAT_RESULT = mus_env_linear(CLM_ARG_3) * mus_delay_unmodulated(CLM_ARG_1, FLOAT_ARG_2);}
static void delay_1f_mult(int *args, ptree *pt) {FLOAT_RESULT = FLOAT_ARG_3 * mus_delay_unmodulated(CLM_ARG_1, FLOAT_ARG_2);}

static void delay_0f_noz(int *args, ptree *pt) {FLOAT_RESULT = mus_delay_unmodulated_noz(CLM_ARG_1, 0.0);}
static void delay_1f_noz(int *args, ptree *pt) {FLOAT_RESULT = mus_delay_unmodulated_noz(CLM_ARG_1, FLOAT_ARG_2);}
static void delay_1f_noz_mult(int *args, ptree *pt) {FLOAT_RESULT = FLOAT_ARG_3 * mus_delay_unmodulated_noz(CLM_ARG_1, FLOAT_ARG_2);}
static void delay_1f_noz_env(int *args, ptree *pt) {FLOAT_RESULT = mus_env_linear(CLM_ARG_3) * mus_delay_unmodulated_noz(CLM_ARG_1, FLOAT_ARG_2);}

GEN_P(delay)

static xen_value *delay_1(ptree *prog, xen_value **args, int num_args)
{
  if ((num_args > 1) && (args[2]->type == R_INT)) single_to_float(prog, args, 2);
  if ((num_args > 2) && (args[3]->type == R_INT)) single_to_float(prog, args, 3);

  if ((args[1]->type == R_CLM) &&
      (mus_delay_p(prog->clms[args[1]->addr])) &&
      (mus_safety(prog->clms[args[1]->addr]) == 0))
    {
      if (num_args == 1) return(package(prog, R_FLOAT, delay_0f_noz, "delay_0f_noz", args, 1));
      if (num_args == 2) return(package(prog, R_FLOAT, delay_1f_noz, "delay_1f_noz", args, 2));
    }

  if (num_args == 1) return(package(prog, R_FLOAT, delay_0f, "delay_0f", args, 1));
  if (num_args == 2) return(package(prog, R_FLOAT, delay_1f, "delay_1f", args, 2));
  return(package(prog, R_FLOAT, delay_2f, "delay_2f", args, 3)); 
}


static void comb_0f(int *args, ptree *pt) {FLOAT_RESULT = mus_comb_unmodulated(CLM_ARG_1, 0.0);}
static void comb_1f(int *args, ptree *pt) {FLOAT_RESULT = mus_comb_unmodulated(CLM_ARG_1, FLOAT_ARG_2);}
static void comb_2f(int *args, ptree *pt) {FLOAT_RESULT = mus_comb(CLM_ARG_1, FLOAT_ARG_2, FLOAT_ARG_3);}

static void comb_0f_noz(int *args, ptree *pt) {FLOAT_RESULT = mus_comb_unmodulated_noz(CLM_ARG_1, 0.0);}
static void comb_1f_noz(int *args, ptree *pt) {FLOAT_RESULT = mus_comb_unmodulated_noz(CLM_ARG_1, FLOAT_ARG_2);}

static void comb_2_noz(int *args, ptree *pt) 
{
  FLOAT_RESULT = mus_comb_unmodulated_noz(CLM_ARG_1, FLOAT_ARG_2);
  FLOAT_ARG_3 = mus_comb_unmodulated_noz(CLM_ARG_4, FLOAT_ARG_5);
}

GEN_P(comb)

static xen_value *comb_1(ptree *prog, xen_value **args, int num_args)
{
  if ((num_args > 1) && (args[2]->type == R_INT)) single_to_float(prog, args, 2);
  if ((num_args > 2) && (args[3]->type == R_INT)) single_to_float(prog, args, 3);

  if ((args[1]->type == R_CLM) &&
      (mus_comb_p(prog->clms[args[1]->addr])) &&
      (mus_safety(prog->clms[args[1]->addr]) == 0))
    {
      if (num_args == 1) return(package(prog, R_FLOAT, comb_0f_noz, "comb_0f_noz", args, 1));
      if (num_args == 2) 
	{
	  if (prog->triple_ctr > 0)
	    {
	      triple *prev_op;
	      xen_value *new0;
	      prev_op = prog->program[prog->triple_ctr - 1];
	      if (prev_op->function == comb_1f_noz)
		{
		  prev_op->types = (int *)realloc(prev_op->types, 6 * sizeof(int));
		  prev_op->args = (int *)realloc(prev_op->args, 6 * sizeof(int));
		  prev_op->num_args = 6;
		  prev_op->function = comb_2_noz;
		  prev_op->op_name = "comb_2_noz";
#if WITH_COUNTERS
		  prev_op->func_loc = get_func_loc(prev_op->function, prev_op->op_name);
#endif				  
		  new0 = add_temporary_var_to_ptree(prog, R_FLOAT);
		  prev_op->args[3] = new0->addr;
		  prev_op->types[3] = R_FLOAT;
		  prev_op->args[4] = args[1]->addr;
		  prev_op->types[4] = R_CLM;
		  prev_op->args[5] = args[2]->addr;
		  prev_op->types[5] = R_FLOAT;
		  return(new0);
		}
	    }
	  return(package(prog, R_FLOAT, comb_1f_noz, "comb_1f_noz", args, 2));
	}
    }
  if (num_args == 1) return(package(prog, R_FLOAT, comb_0f, "comb_0f", args, 1));
  if (num_args == 2) return(package(prog, R_FLOAT, comb_1f, "comb_1f", args, 2));
  return(package(prog, R_FLOAT, comb_2f, "comb_2f", args, 3)); 
}


static void notch_0f(int *args, ptree *pt) {FLOAT_RESULT = mus_notch_unmodulated(CLM_ARG_1, 0.0);}
static void notch_1f(int *args, ptree *pt) {FLOAT_RESULT = mus_notch_unmodulated(CLM_ARG_1, FLOAT_ARG_2);}
static void notch_2f(int *args, ptree *pt) {FLOAT_RESULT = mus_notch(CLM_ARG_1, FLOAT_ARG_2, FLOAT_ARG_3);}

static void notch_0f_noz(int *args, ptree *pt) {FLOAT_RESULT = mus_notch_unmodulated_noz(CLM_ARG_1, 0.0);}
static void notch_1f_noz(int *args, ptree *pt) {FLOAT_RESULT = mus_notch_unmodulated_noz(CLM_ARG_1, FLOAT_ARG_2);}

GEN_P(notch)

static xen_value *notch_1(ptree *prog, xen_value **args, int num_args)
{
  if ((num_args > 1) && (args[2]->type == R_INT)) single_to_float(prog, args, 2);
  if ((num_args > 2) && (args[3]->type == R_INT)) single_to_float(prog, args, 3);

  if ((args[1]->type == R_CLM) &&
      (mus_notch_p(prog->clms[args[1]->addr])) &&
      (mus_safety(prog->clms[args[1]->addr]) == 0))
    {
      if (num_args == 1) return(package(prog, R_FLOAT, notch_0f_noz, "notch_0f_noz", args, 1));
      if (num_args == 2) return(package(prog, R_FLOAT, notch_1f_noz, "notch_1f_noz", args, 2));
    }

  if (num_args == 1) return(package(prog, R_FLOAT, notch_0f, "notch_0f", args, 1));
  if (num_args == 2) return(package(prog, R_FLOAT, notch_1f, "notch_1f", args, 2));
  return(package(prog, R_FLOAT, notch_2f, "notch_2f", args, 3)); 
}


static void all_pass_0f(int *args, ptree *pt) {FLOAT_RESULT = mus_all_pass_unmodulated(CLM_ARG_1, 0.0);}
static void all_pass_1f(int *args, ptree *pt) {FLOAT_RESULT = mus_all_pass_unmodulated(CLM_ARG_1, FLOAT_ARG_2);}
static void all_pass_2f(int *args, ptree *pt) {FLOAT_RESULT = mus_all_pass(CLM_ARG_1, FLOAT_ARG_2, FLOAT_ARG_3);}

static void all_pass_0f_noz(int *args, ptree *pt) {FLOAT_RESULT = mus_all_pass_unmodulated_noz(CLM_ARG_1, 0.0);}
static void all_pass_1f_noz(int *args, ptree *pt) {FLOAT_RESULT = mus_all_pass_unmodulated_noz(CLM_ARG_1, FLOAT_ARG_2);}

static void all_pass_2_noz(int *args, ptree *pt) 
{
  FLOAT_RESULT = mus_all_pass_unmodulated_noz(CLM_ARG_1, FLOAT_ARG_2);
  FLOAT_ARG_3 = mus_all_pass_unmodulated_noz(CLM_ARG_4, FLOAT_ARG_5);
}

GEN_P(all_pass)

static xen_value *all_pass_1(ptree *prog, xen_value **args, int num_args)
{
  if ((num_args > 1) && (args[2]->type == R_INT)) single_to_float(prog, args, 2);
  if ((num_args > 2) && (args[3]->type == R_INT)) single_to_float(prog, args, 3);

  if ((args[1]->type == R_CLM) &&
      (mus_all_pass_p(prog->clms[args[1]->addr])) &&
      (mus_safety(prog->clms[args[1]->addr]) == 0))
    {
      if (num_args == 1) return(package(prog, R_FLOAT, all_pass_0f_noz, "all_pass_0f_noz", args, 1));
      if (num_args == 2) 
	{
	  if (prog->triple_ctr > 0)
	    {
	      triple *prev_op;
	      xen_value *new0;
	      prev_op = prog->program[prog->triple_ctr - 1];
	      if (prev_op->function == all_pass_1f_noz)
		{
		  prev_op->types = (int *)realloc(prev_op->types, 6 * sizeof(int));
		  prev_op->args = (int *)realloc(prev_op->args, 6 * sizeof(int));
		  prev_op->num_args = 6;
		  prev_op->function = all_pass_2_noz;
		  prev_op->op_name = "all_pass_2_noz";
#if WITH_COUNTERS
		  prev_op->func_loc = get_func_loc(prev_op->function, prev_op->op_name);
#endif				  
		  new0 = add_temporary_var_to_ptree(prog, R_FLOAT);
		  prev_op->args[3] = new0->addr;
		  prev_op->types[3] = R_FLOAT;
		  prev_op->args[4] = args[1]->addr;
		  prev_op->types[4] = R_CLM;
		  prev_op->args[5] = args[2]->addr;
		  prev_op->types[5] = R_FLOAT;
		  return(new0);
		}
	    }
	  return(package(prog, R_FLOAT, all_pass_1f_noz, "all_pass_1f_noz", args, 2));
	}
    }

  if (num_args == 1) return(package(prog, R_FLOAT, all_pass_0f, "all_pass_0f", args, 1));
  if (num_args == 2) return(package(prog, R_FLOAT, all_pass_1f, "all_pass_1f", args, 2));
  return(package(prog, R_FLOAT, all_pass_2f, "all_pass_2f", args, 3)); 
}


static void rand_0f_env(int *args, ptree *pt) {FLOAT_RESULT = mus_env_linear(CLM_ARG_2) * mus_rand(CLM_ARG_1, 0.0);}
static void rand_0f_mult(int *args, ptree *pt) {FLOAT_RESULT = FLOAT_ARG_2 * mus_rand(CLM_ARG_1, 0.0);}
static void rand_interp_0f_env(int *args, ptree *pt) {FLOAT_RESULT = mus_env_linear(CLM_ARG_2) * mus_rand_interp(CLM_ARG_1, 0.0);}
static void rand_interp_0f_mult(int *args, ptree *pt) {FLOAT_RESULT = FLOAT_ARG_2 * mus_rand_interp(CLM_ARG_1, 0.0);}
static void rand_interp_0f_add(int *args, ptree *pt) {FLOAT_RESULT = FLOAT_ARG_2 + mus_rand_interp(CLM_ARG_1, 0.0);}

/* mus_safety = zdly setting internally, if 0 we could call simplified delay and friends */

GEN3(ssb_am)
GEN3(asymmetric_fm)

GEN2(moving_average)
GEN2(rand)
GEN2(rand_interp)
GEN2(ncos)
GEN2(nsin)
GEN2(sawtooth_wave)
GEN2(pulse_train)
GEN2(square_wave)
GEN2(triangle_wave)
GEN2(nrxysin)
GEN2(nrxycos)
GEN2(one_zero)
GEN2(one_pole)
GEN2(two_zero)
GEN2(two_pole)
GEN2(filter)
GEN2(fir_filter)
GEN2(iir_filter)

GEN2_OPT(wave_train)
GEN2_OPT(table_lookup)


GEN_P(frame)
GEN_P(mixer)
GEN_P(file_to_sample)
GEN_P(sample_to_file)
GEN_P(file_to_frame)
GEN_P(frame_to_file)
GEN_P(input)
GEN_P(output)
GEN_P(locsig)
GEN_P(move_sound)


static void polywave_0f(int *args, ptree *pt) {FLOAT_RESULT = mus_polywave_unmodulated(CLM_ARG_1);}
static void polywave_1f(int *args, ptree *pt) {FLOAT_RESULT = mus_polywave(CLM_ARG_1, FLOAT_ARG_2);}
static void polywave_1f_2(int *args, ptree *pt) {FLOAT_RESULT = mus_polywave(CLM_ARG_1, FLOAT_ARG_2 + FLOAT_ARG_3);}
static void polywave_1f_3(int *args, ptree *pt) {FLOAT_RESULT = mus_polywave(CLM_ARG_1, FLOAT_ARG_2 + FLOAT_ARG_3 + FLOAT_ARG_4);}

GEN_P(polywave) 

static xen_value *polywave_1(ptree *prog, xen_value **args, int num_args) 
{ 
  bool one_arg;
  one_arg = ((num_args == 1) || ((num_args == 2) && (args[2]->constant == R_CONSTANT) && (prog->dbls[args[2]->addr] == 0.0)));

  if ((num_args > 1) && (args[2]->type == R_INT)) single_to_float(prog, args, 2);

  if ((!one_arg) &&
      (prog->triple_ctr > 0))
    {
      triple *prev_op;
      prev_op = prog->program[prog->triple_ctr - 1];
      if (((prev_op->function == add_f2) ||
	   (prev_op->function == add_f3)) &&
	  (prev_op->args[0] == args[2]->addr) &&
	  (find_var_in_ptree_via_addr(prog, R_FLOAT, prev_op->args[0])) == NULL)
	{
	  if (prev_op->function == add_f2)
	    {
	      prev_op->types = (int *)realloc(prev_op->types, 4 * sizeof(int));
	      prev_op->args = (int *)realloc(prev_op->args, 4 * sizeof(int));
	      prev_op->args[3] = prev_op->args[2];
	      prev_op->args[2] = prev_op->args[1];
	      prev_op->types[3] = R_FLOAT;
	      prev_op->args[1] = args[1]->addr;
	      prev_op->types[1] = R_CLM;
	      prev_op->num_args = 4;
	      prev_op->function = polywave_1f_2;
	      prev_op->op_name = "polywave_1f_2";
#if WITH_COUNTERS
	      prev_op->func_loc = get_func_loc(prev_op->function, prev_op->op_name);
#endif				  
	      return(make_xen_value(R_FLOAT, prev_op->args[0], R_TEMPORARY));
	    }
	  /* else it's add_f3 */
	  prev_op->types = (int *)realloc(prev_op->types, 5 * sizeof(int));
	  prev_op->args = (int *)realloc(prev_op->args, 5 * sizeof(int));
	  prev_op->args[4] = prev_op->args[3];
	  prev_op->args[3] = prev_op->args[2];
	  prev_op->args[2] = prev_op->args[1];
	  prev_op->types[4] = R_FLOAT;
	  prev_op->args[1] = args[1]->addr;
	  prev_op->types[1] = R_CLM;
	  prev_op->num_args = 5;
	  prev_op->function = polywave_1f_3;
	  prev_op->op_name = "polywave_1f_3";
#if WITH_COUNTERS
	  prev_op->func_loc = get_func_loc(prev_op->function, prev_op->op_name);
#endif				  
	  return(make_xen_value(R_FLOAT, prev_op->args[0], R_TEMPORARY));
	}
    }
  
  /* splitting out the "kind" check does not help (I tried this three times and still can't believe it) */
  if (one_arg)
    return(package(prog, R_FLOAT, polywave_0f, "polywave_0f", args, 1));
  return(package(prog, R_FLOAT, polywave_1f, "polywave_1f", args, 2));
}

static void polywave_0f_env(int *args, ptree *pt) {FLOAT_RESULT = mus_env_linear(CLM_ARG_2) * mus_polywave_unmodulated(CLM_ARG_1);}
static void polywave_0f_mult(int *args, ptree *pt) {FLOAT_RESULT = FLOAT_ARG_2 * mus_polywave_unmodulated(CLM_ARG_1);}
static void polywave_1f_env(int *args, ptree *pt) {FLOAT_RESULT = mus_env_linear(CLM_ARG_3) * mus_polywave(CLM_ARG_1, FLOAT_ARG_2);}
static void polywave_1f_mult(int *args, ptree *pt) {FLOAT_RESULT = FLOAT_ARG_3 * mus_polywave(CLM_ARG_1, FLOAT_ARG_2);}
static void polywave_1f_2_env(int *args, ptree *pt) {FLOAT_RESULT = mus_env_linear(CLM_ARG_4) * mus_polywave(CLM_ARG_1, FLOAT_ARG_2 + FLOAT_ARG_3);}
static void polywave_1f_2_mult(int *args, ptree *pt) {FLOAT_RESULT = FLOAT_ARG_4 * mus_polywave(CLM_ARG_1, FLOAT_ARG_2 + FLOAT_ARG_3);}



static void tap_0f(int *args, ptree *pt) {FLOAT_RESULT = mus_tap_unmodulated(CLM_ARG_1);}  

static void tap_1f(int *args, ptree *pt) {FLOAT_RESULT = mus_tap(CLM_ARG_1, FLOAT_ARG_2);}

static xen_value *tap_1(ptree *prog, xen_value **args, int num_args)
{
  if (num_args == 1)
    return(package(prog, R_FLOAT, tap_0f, "tap_0f", args, 1));
  if (args[2]->type == R_INT) single_to_float(prog, args, 2);
  return(package(prog, R_FLOAT, tap_1f, "tap_1f", args, 2));
}


static void delay_tick_1f(int *args, ptree *pt) {FLOAT_RESULT = mus_delay_tick(CLM_ARG_1, FLOAT_ARG_2);}
static void delay_tick_noz_1f(int *args, ptree *pt) {FLOAT_RESULT = mus_delay_tick_noz(CLM_ARG_1, FLOAT_ARG_2);}

static xen_value *delay_tick_1(ptree *prog, xen_value **args, int num_args)
{
  if (args[2]->type == R_INT) single_to_float(prog, args, 2);

  if ((args[1]->type == R_CLM) &&
      (mus_delay_p(prog->clms[args[1]->addr])) &&
      (mus_safety(prog->clms[args[1]->addr]) == 0))
    return(package(prog, R_FLOAT, delay_tick_noz_1f, "delay_tick_noz_1f", args, 2));

  return(package(prog, R_FLOAT, delay_tick_1f, "delay_tick_1f", args, 2));
}

static void move_locsig_2f(int *args, ptree *pt) {mus_move_locsig(CLM_ARG_1, FLOAT_ARG_2, FLOAT_ARG_3);} 
 
static xen_value *move_locsig_1(ptree *prog, xen_value **args, int num_args)
{ 
  return(package(prog, R_BOOL, move_locsig_2f, "move_locsig_2f", args, 3));
}


static void mixer_ref_0(int *args, ptree *pt) {FLOAT_RESULT = mus_mixer_ref(CLM_ARG_1, INT_ARG_2, INT_ARG_3);}

static xen_value *mixer_ref_1(ptree *prog, xen_value **args, int num_args)
{
  return(package(prog, R_FLOAT, mixer_ref_0, "mixer_ref_0", args, 3));
}


static void mixer_set_0(int *args, ptree *pt) 
{
  mus_mixer_set(CLM_ARG_1, INT_ARG_2, INT_ARG_3, FLOAT_ARG_4);
}

static void mixer_set_i(int *args, ptree *pt) 
{
  mus_mixer_set(CLM_ARG_1, INT_ARG_2, INT_ARG_3, (mus_float_t)INT_ARG_4);
}

static xen_value *mixer_set_2(ptree *prog, xen_value **args, int num_args)
{
  /* mixer-set! */
  if (args[4]->type == R_FLOAT)
    return(package(prog, R_FLOAT, mixer_set_0, "mixer_set_0", args, 4));
  return(package(prog, R_FLOAT, mixer_set_i, "mixer_set_i", args, 4));
}

static void mixer_set_1(ptree *prog, xen_value *in_v, xen_value *in_v1, xen_value *in_v2, xen_value *v)
{
  /* set! mixer-ref */
  add_triple_to_ptree(prog, va_make_triple(mixer_set_0, "mixer_set_0", 5, NULL, in_v, in_v1, in_v2, v));
}

#define REF_GEN0(Name, SName) \
  static void Name ## _0r(int *args, ptree *pt) {FLOAT_RESULT = mus_ ## Name (CLM_ARG_1, INT_ARG_2);} \
  static xen_value * Name ## _0(ptree *prog, xen_value **args, int num_args) \
  { \
    return(package(prog, R_FLOAT, Name ## _0r, #Name "_0r", args, 2)); \
  }

REF_GEN0(frame_ref, frame)
REF_GEN0(locsig_ref, locsig)
REF_GEN0(locsig_reverb_ref, locsig)


#define SET_GEN0(Name, SName) \
  static void Name ## _0r(int *args, ptree *pt) {mus_ ## Name (CLM_ARG_1, INT_ARG_2, FLOAT_ARG_3);} \
  static void Name ## _ir(int *args, ptree *pt) {mus_ ## Name (CLM_ARG_1, INT_ARG_2, (mus_float_t)(INT_ARG_3));} \
  static xen_value * Name ## _2(ptree *prog, xen_value **args, int num_args) \
  { \
    if (args[3]->type == R_FLOAT) \
      return(package(prog, R_FLOAT, Name ## _0r, #Name "_0r", args, 3)); \
    return(package(prog, R_FLOAT, Name ## _ir, #Name "_ir", args, 3)); \
  } \
  static void Name ## _1(ptree *prog, xen_value *in_v, xen_value *in_v1, xen_value *in_v2, xen_value *v) \
  { \
    add_triple_to_ptree(prog, va_make_triple( Name ## _0r, #Name "_0r", 4, NULL, in_v, in_v1, v)); \
  }

SET_GEN0(frame_set, frame)
/* often preceded by (set:)add_f2 all_pass_1f (ref):add_multiply_i2 vector_ref_c */
SET_GEN0(locsig_set, locsig)
SET_GEN0(locsig_reverb_set, locsig)



/* ---------------------------------------- generic funcs ---------------------------------------- */

/* formerly, the local methods for defgenerator generators were only in the generator list,
 *   but if we have a function taking such args, we might need to expand the method
 *   before any such gen has been created.  To call the method in Scheme (via call_get_method
 *   in clm2xen.c) requires that the original gen list be saved alongside the list as read
 *   by xen_to_list_with_type, then if another arg exists, xenify it and pass into Scheme.
 *   But this will surely involve the GC and requires elaborate error checking here.
 *   So, the local methods are now in the defgenerator declaration, and are
 *   accessible through <name>-methods. We can assume that we have the type;
 *   the method expansion (via lambda_form) uses the passed-in arg types.
 */

typedef enum {USE_SET_METHOD, USE_GET_METHOD} method_choice_t;

static xen_value *clean_up(xen_value *result, xen_value **args, int args_size);

static xen_value *splice_in_function_body(ptree *prog, s7_pointer proc, xen_value **args, int num_args, const char *funcname)
{
  s7_pointer pform, func_form;

  pform = s7_procedure_source(s7, proc);
  if (!s7_is_pair(pform))
    return(NULL);

  func_form = s7_car(pform);
  s7_unoptimize(s7, func_form);
#if 0
  fprintf(stderr,"splice in %s\n", s7_object_to_c_string(s7, func_form));
  fprintf(stderr, "list: %d, sym: %d, name: %s, env: %s\n",
	  s7_is_list(s7, func_form),
	  s7_is_symbol(s7_car(func_form)),
	  s7_symbol_name(s7_car(func_form)),
	  s7_object_to_c_string(s7, s7_cdr(proc)));
#endif
  if ((s7_is_list(s7, func_form)) &&
      (s7_is_symbol(s7_car(func_form))) &&
      (strncmp("lambda", s7_symbol_name(s7_car(func_form)), 6) == 0)) /* might be lambda* */
    {
      /* look for procedure source, use current arg types as auto-declaration */
      bool old_got_lambda;
      xen_value *v;
      old_got_lambda = prog->got_lambda;
      prog->got_lambda = true;
      v = lambda_form(prog, func_form, true, args, num_args, pform);
      prog->got_lambda = old_got_lambda;
      if (v) 
	{
	  xen_value *result;
	  if (funcname) add_var_to_ptree(prog, funcname, v);
	  result = funcall_n(prog, args, num_args, v);
	  free(v);
	  /* return(clean_up(result, args, num_args)); */
	  return(result);
	}
    }
  return(NULL); /* not an error */
}


static xen_value *splice_in_method(ptree *prog, xen_value **args, int num_args, const char *method_name, method_choice_t use_getter)
{
  s7_pointer methods, pair;
  int methods_loc;
  char *method_str;
  xen_value *result = NULL;

  /* fprintf(stderr, "splice in method %s (%s)\n", method_name, (use_getter == USE_GET_METHOD) ? "getter" : "setter"); */
  
  method_str = mus_format("%s-methods", type_name(args[1]->type));
  methods = s7_call(s7, s7_name_to_value(s7, method_str), xen_nil);
  methods_loc = s7_gc_protect(s7, methods);
  free(method_str);

  /* fprintf(stderr, "methods: %s\n", s7_object_to_c_string(s7, methods)); */

  if (s7_is_list(s7, methods))
    {
      pair = s7_assoc(s7, s7_make_symbol(s7, method_name), 
		       methods);
      if (s7_is_list(s7, pair))
	{
	  if (use_getter == USE_GET_METHOD)
	    {
	      result = splice_in_function_body(prog, scheme_cadr(pair), args, num_args, NULL);
	    }
	  else
	    {
	      if (s7_is_procedure_with_setter(scheme_cadr(pair)))
		result = splice_in_function_body(prog, s7_procedure_setter(s7, scheme_cadr(pair)), args, num_args, NULL);
	      else result = splice_in_function_body(prog, scheme_caddr(pair), args, num_args, NULL);
	    }
	}
    }

  s7_gc_unprotect_at(s7, methods_loc);
  return(result);
}


static void splice_in_set_method(ptree *prog, xen_value *in_v, xen_value *in_v1, xen_value *in_v2, xen_value *v, const char *method_name)
{
  xen_value *args[3];
  xen_value *tmp;
  args[1] = in_v;
  args[2] = v;
  tmp = splice_in_method(prog, args, 2, method_name, USE_SET_METHOD);
  if (tmp) free(tmp);
}


/* (let ((hi (make-rxycos 440.0))) (run (lambda () (mus-scaler hi)))) */
/* (let ((gen (make-nssb 440)) (x 0.0)) (run (lambda () (set! x (mus-frequency gen)) x))) */

#define GEN0(Name) \
  static void Name ## _0f(int *args, ptree *pt) {FLOAT_RESULT = mus_ ## Name (CLM_ARG_1);}  \
  static xen_value * mus_ ## Name ## _0(ptree *prog, xen_value **args, int num_args) \
  { \
    if (args[1]->type == R_CLM) return(package(prog, R_FLOAT, Name ## _0f, #Name "_0f", args, 1)); \
    if (CLM_STRUCT_P(args[1]->type)) {return(splice_in_method(prog, args, num_args, "mus-" #Name, USE_GET_METHOD));} \
    return(make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, 0.0), R_CONSTANT)); \
  }

GEN0(increment)
GEN0(frequency)
GEN0(phase)
GEN0(scaler)
GEN0(width)
GEN0(offset)
GEN0(feedforward)
GEN0(feedback)


#define INT_GEN0(Name) \
  static void Name ## _0i(int *args, ptree *pt) {INT_RESULT = mus_ ## Name (CLM_ARG_1);} \
  static xen_value * mus_ ## Name ## _0(ptree *prog, xen_value **args, int num_args) \
  { \
    if (args[1]->type == R_CLM) return(package(prog, R_INT, Name ## _0i, #Name "_0i", args, 1)); \
    if (CLM_STRUCT_P(args[1]->type)) {return(splice_in_method(prog, args, num_args, "mus-" #Name, USE_GET_METHOD));} \
    return(make_xen_value(R_INT, add_int_to_ptree(prog, 0), R_CONSTANT)); \
  }

INT_GEN0(hop)
INT_GEN0(location)
INT_GEN0(ramp)
INT_GEN0(order)
INT_GEN0(length)
INT_GEN0(channel)
INT_GEN0(safety)


/* set methods have two (3?) args: (lambda (g val)...) so splice in method with args built on the spot */

#define SET_INT_GEN0(Name) \
  static void set_ ## Name ## _i(int *args, ptree *pt) {mus_set_ ## Name (CLM_RESULT, INT_ARG_1);} \
  static void mus_set_ ## Name ## _1(ptree *prog, xen_value *in_v, xen_value *in_v1, xen_value *in_v2, xen_value *v) \
  { \
    if (in_v->type == R_CLM) \
      add_triple_to_ptree(prog, va_make_triple(set_ ## Name ## _i, "set_" #Name "_i", 2, in_v, v)); \
    else \
      { \
	if (CLM_STRUCT_P(in_v->type)) \
	  splice_in_set_method(prog, in_v, in_v1, in_v2, v, "mus-" #Name); \
      } \
  }

SET_INT_GEN0(location)
SET_INT_GEN0(ramp)
SET_INT_GEN0(hop)
SET_INT_GEN0(length)
SET_INT_GEN0(safety)


#define SET_DBL_GEN0(Name) \
  static void set_ ## Name ## _f(int *args, ptree *pt) {mus_set_ ## Name (CLM_RESULT, FLOAT_ARG_1);} \
  static void mus_set_ ## Name ## _1(ptree *prog, xen_value *in_v, xen_value *in_v1, xen_value *in_v2, xen_value *v) \
  { \
    if (in_v->type == R_CLM) \
      add_triple_to_ptree(prog, va_make_triple(set_ ## Name ## _f, "set_" #Name "_f", 2, in_v, v)); \
    else \
      { \
	if (CLM_STRUCT_P(in_v->type)) \
	  splice_in_set_method(prog, in_v, in_v1, in_v2, v, "mus-" #Name); \
      } \
  }

SET_DBL_GEN0(increment)
SET_DBL_GEN0(scaler)
SET_DBL_GEN0(offset)
SET_DBL_GEN0(width)
SET_DBL_GEN0(feedback)
SET_DBL_GEN0(feedforward)
SET_DBL_GEN0(phase)
SET_DBL_GEN0(frequency)


#define STR_GEN0(Name) \
  static void Name ## _0s(int *args, ptree *pt) \
    { \
      if (STRING_RESULT) free(STRING_RESULT); \
      STRING_RESULT = mus_strdup(mus_ ## Name (CLM_ARG_1)); \
    } \
  static xen_value * mus_ ## Name ## _0(ptree *prog, xen_value **args, int num_args) \
  { \
    if (args[1]->type == R_CLM) return(package(prog, R_STRING, Name ## _0s, #Name "_0s", args, 1)); \
    if (CLM_STRUCT_P(args[1]->type)) {return(splice_in_method(prog, args, num_args, "mus-" #Name, USE_GET_METHOD));} \
    return(make_xen_value(R_STRING, add_string_to_ptree(prog, NULL), R_CONSTANT)); \
  }

STR_GEN0(name)
STR_GEN0(describe)
STR_GEN0(file_name)


static void set_name_s(int *args, ptree *pt) {mus_set_name(CLM_RESULT, STRING_ARG_1);} 

static void mus_set_name_1(ptree *prog, xen_value *in_v, xen_value *in_v1, xen_value *in_v2, xen_value *v)
{
  if (in_v->type == R_CLM)
    add_triple_to_ptree(prog, va_make_triple(set_name_s, "set_name_s", 2, in_v, v));
  else
    { 
      /* currently this doesn't work because the defgenerator set method uses stuff like set-car! */
      if (CLM_STRUCT_P(in_v->type))
	splice_in_set_method(prog, in_v, in_v1, in_v2, v, "mus-name");
    }
}



/* -------- mus-channels --------
 *
 * this is special because *output* can be a vct or sound-data object 
 */

static void mus_channels_i(int *args, ptree *pt) {INT_RESULT = mus_channels(CLM_ARG_1);}

static void mus_channels_v(int *args, ptree *pt) {INT_RESULT = 1;}

static void mus_channels_f(int *args, ptree *pt) {INT_RESULT = 0;}

static void mus_channels_s(int *args, ptree *pt) {INT_RESULT = SOUND_DATA_ARG_1->chans;}


static xen_value *mus_channels_0(ptree *prog, xen_value **args, int num_args)
{
  if (args[1]->type == R_CLM)
    return(package(prog, R_INT, mus_channels_i, "mus_channels", args, 1));
  if (args[1]->type == R_VCT)
    return(package(prog, R_INT, mus_channels_v, "mus_channels_vct", args, 1));
  if (args[1]->type == R_SOUND_DATA)
    return(package(prog, R_INT, mus_channels_s, "mus_channels_sound_data", args, 1));
  if (CLM_STRUCT_P(args[1]->type)) 
    return(splice_in_method(prog, args, num_args, "mus-channels", USE_GET_METHOD));
  return(package(prog, R_INT, mus_channels_f, "mus_channels_f", args, 1));
}


static void close_0(int *args, ptree *pt) {INT_RESULT = mus_close_file(CLM_ARG_1);}

static void close_0_noop(int *args, ptree *pt) {INT_RESULT = 0;}

static xen_value *mus_close_0(ptree *prog, xen_value **args, int num_args)
{
  if (args[1]->type == R_CLM)
    return(package(prog, R_INT, close_0, "mus_close_0", args, 1));
  if (CLM_STRUCT_P(args[1]->type)) 
    return(splice_in_method(prog, args, num_args, "mus-close", USE_GET_METHOD));
  return(package(prog, R_INT, close_0_noop, "mus_close_0_noop", args, 1));
}


static void reset_0(int *args, ptree *pt) {mus_reset(CLM_ARG_1);}

static xen_value *mus_reset_0(ptree *prog, xen_value **args, int num_args)
{
  if (args[1]->type == R_CLM)
    return(package(prog, R_CLM, reset_0, "mus_reset_0", args, 1));
  if (CLM_STRUCT_P(args[1]->type)) 
    return(splice_in_method(prog, args, num_args, "mus-reset", USE_GET_METHOD));
  return(make_xen_value(R_INT, add_int_to_ptree(prog, 0), R_CONSTANT));
}


static void set_formant_radius_and_frequency_2f(int *args, ptree *pt) 
{
  mus_set_formant_radius_and_frequency(CLM_ARG_1, FLOAT_ARG_2, FLOAT_ARG_3);
}  

static xen_value *set_formant_radius_and_frequency_1(ptree *prog, xen_value **args, int num_args)
{
  if (args[2]->type == R_INT) single_to_float(prog, args, 2);
  if (args[3]->type == R_INT) single_to_float(prog, args, 3);
  return(package(prog, R_BOOL, set_formant_radius_and_frequency_2f, "mus_set_formant_radius_and_frequency_2f", args, 3));
}


static void mus_run_0f(int *args, ptree *pt) {FLOAT_RESULT = mus_run(CLM_ARG_1, 0.0, 0.0);}
static void mus_run_1f(int *args, ptree *pt) {FLOAT_RESULT = mus_run(CLM_ARG_1, FLOAT_ARG_2, 0.0);}  
static void mus_run_2f(int *args, ptree *pt) {FLOAT_RESULT = mus_run(CLM_ARG_1, FLOAT_ARG_2, FLOAT_ARG_3);}  

static xen_value *mus_run_1(ptree *prog, xen_value **args, int num_args)
{
  if (args[1]->type == R_CLM)
    {
      if (num_args == 1)
	return(package(prog, R_FLOAT, mus_run_0f, "mus_run_0f", args, 1));
      if (args[2]->type == R_INT) single_to_float(prog, args, 2);
      if (num_args == 2)
	return(package(prog, R_FLOAT, mus_run_1f, "mus_run_1f", args, 2));
      if (args[3]->type == R_INT) single_to_float(prog, args, 3);
      return(package(prog, R_FLOAT, mus_run_2f, "mus_run_2f", args, 3));
    }
  return(splice_in_method(prog, args, num_args, S_mus_run, USE_GET_METHOD));
}


/* ---------------- xcoeff/ycoeff ---------------- */


static void xcoeff_0(int *args, ptree *pt) {FLOAT_RESULT = mus_xcoeff(CLM_ARG_1, INT_ARG_2);}

static xen_value *mus_xcoeff_1(ptree *prog, xen_value **args, int num_args) 
{
  if (args[1]->type == R_CLM)
    return(package(prog, R_FLOAT, xcoeff_0, "xcoeff_0", args, 2));
  if (CLM_STRUCT_P(args[1]->type)) 
    return(splice_in_method(prog, args, num_args, "mus-xcoeff", USE_GET_METHOD));
  return(make_xen_value(R_INT, add_int_to_ptree(prog, 0), R_CONSTANT));
}


static void set_xcoeff_0(int *args, ptree *pt) {mus_set_xcoeff(CLM_ARG_1, INT_ARG_2, FLOAT_ARG_3);}

static void mus_set_xcoeff_1(ptree *prog, xen_value *in_v, xen_value *in_v1, xen_value *in_v2, xen_value *v)
{
  add_triple_to_ptree(prog, va_make_triple(set_xcoeff_0, "set_xcoeff_0", 4, NULL, in_v, in_v1, v));
}


static void ycoeff_0(int *args, ptree *pt) {FLOAT_RESULT = mus_ycoeff(CLM_ARG_1, INT_ARG_2);}

static xen_value *mus_ycoeff_1(ptree *prog, xen_value **args, int num_args) 
{
  if (args[1]->type == R_CLM)
    return(package(prog, R_FLOAT, ycoeff_0, "ycoeff_0", args, 2));
  if (CLM_STRUCT_P(args[1]->type)) 
    return(splice_in_method(prog, args, num_args, "mus-ycoeff", USE_GET_METHOD));
  return(make_xen_value(R_INT, add_int_to_ptree(prog, 0), R_CONSTANT));
}


static void set_ycoeff_0(int *args, ptree *pt) {mus_set_ycoeff(CLM_ARG_1, INT_ARG_2, FLOAT_ARG_3);}

static void mus_set_ycoeff_1(ptree *prog, xen_value *in_v, xen_value *in_v1, xen_value *in_v2, xen_value *v)
{
  add_triple_to_ptree(prog, va_make_triple(set_ycoeff_0, "set_ycoeff_0", 4, NULL, in_v, in_v1, v));
}


/* ---------------- mus-data, xcoeffs, ycoeffs ---------------- */

#define MUS_VCT_1(Name, Position) \
static void Name ## _1(int *args, ptree *pt) \
{ \
  if (!VCT_RESULT) \
    { \
      s7_pointer res; \
      res = xen_make_vct_wrapper(mus_length(CLM_ARG_1), mus_ ## Name (CLM_ARG_1)); \
      add_loc_to_protected_list(pt, s7_gc_protect(s7, res)); \
      VCT_RESULT = xen_to_vct(res); \
    } \
} \
static xen_value *mus_ ## Name ## _1(ptree *prog, xen_value **args, int num_args) \
{ \
  if (args[1]->type == R_CLM) \
    return(package(prog, R_VCT, Name ## _1, #Name "_1", args, 1)); \
  if (CLM_STRUCT_P(args[1]->type)) \
    return(splice_in_method(prog, args, num_args, "mus-" #Name, USE_GET_METHOD)); \
  return(run_warn("wrong type arg (%s) to mus-" #Name , type_name(args[1]->type))); \
}

/* (let ((gen (make-polyoid 100.0 (vct 1 1 0.0)))) (run (lambda () (mus-ycoeffs gen)))) */

MUS_VCT_1(data, 0)
MUS_VCT_1(xcoeffs, 1)
MUS_VCT_1(ycoeffs, 2)



/* ---------------- polynomial ---------------- */


static void polynomial_1f(int *args, ptree *pt) 
{
  FLOAT_RESULT = mus_polynomial(VCT_ARG_1->data, FLOAT_ARG_2, VCT_ARG_1->length);
}

static xen_value *polynomial_1(ptree *prog, xen_value **args, int num_args) 
{
  return(package(prog, R_FLOAT, polynomial_1f, "polynomial_1f", args, 2));
}



/* ---------------- mus-fft ---------------- */


static void mus_fft_2v(int *args, ptree *pt) 
{
  mus_fft(VCT_ARG_1->data, VCT_ARG_2->data, VCT_ARG_1->length, 1);
  VCT_RESULT = VCT_ARG_1;
}


static void mus_fft_2v_1(int *args, ptree *pt) 
{
  mus_fft(VCT_ARG_1->data, VCT_ARG_2->data, INT_ARG_3, 1);
  VCT_RESULT = VCT_ARG_1;
}


static void mus_fft_2v_2(int *args, ptree *pt) 
{
  mus_fft(VCT_ARG_1->data, VCT_ARG_2->data, INT_ARG_3, INT_ARG_4);
  VCT_RESULT = VCT_ARG_1;
}

static xen_value *mus_fft_1(ptree *prog, xen_value **args, int num_args) 
{
  if (num_args == 2) return(package(prog, R_VCT, mus_fft_2v, "mus_fft_2v", args, 2));
  if (num_args == 3) return(package(prog, R_VCT, mus_fft_2v_1, "mus_fft_2v_1", args, 3));
  return(package(prog, R_VCT, mus_fft_2v_2, "mus_fft_2v_2", args, 4));
}


/* ----------------fft ---------------- */


static void fft_2v(int *args, ptree *pt) 
{
  mus_fft(VCT_ARG_1->data, VCT_ARG_2->data, VCT_ARG_1->length, 1);
  VCT_RESULT = VCT_ARG_1;
}

static void fft_2v_1(int *args, ptree *pt) 
{
  mus_fft(VCT_ARG_1->data, VCT_ARG_2->data, VCT_ARG_1->length, INT_ARG_3);
  VCT_RESULT = VCT_ARG_1;
}

static xen_value *fft_1(ptree *prog, xen_value **args, int num_args) 
{
  if (num_args == 2) return(package(prog, R_VCT, fft_2v, "fft_2v", args, 2));
  return(package(prog, R_VCT, fft_2v_1, "fft_2v_1", args, 3));
}


/* ---------------- file->sample ---------------- */

static void file_to_sample_1f(int *args, ptree *pt) {FLOAT_RESULT = mus_file_to_sample(CLM_ARG_1, INT_ARG_2, 0);}

static void file_to_sample_2f(int *args, ptree *pt) {FLOAT_RESULT = mus_file_to_sample(CLM_ARG_1, INT_ARG_2, INT_ARG_3);}

static xen_value *file_to_sample_1(ptree *prog, xen_value **args, int num_args) 
{
  if (num_args == 2) return(package(prog, R_FLOAT, file_to_sample_1f, "file_to_sample_1f", args, 2));
  return(package(prog, R_FLOAT, file_to_sample_2f, "file_to_sample_2f", args, 3));
}


#if USE_SND
/* ---------------- snd->sample ---------------- */

static void snd_to_sample_1f(int *args, ptree *pt) {FLOAT_RESULT = snd_to_sample_read(CLM_ARG_1, INT_ARG_2, 0);}

static void snd_to_sample_2f(int *args, ptree *pt) {FLOAT_RESULT = snd_to_sample_read(CLM_ARG_1, INT_ARG_2, INT_ARG_3);}

static xen_value *snd_to_sample_1(ptree *prog, xen_value **args, int num_args) 
{
  if (num_args == 2) return(package(prog, R_FLOAT, snd_to_sample_1f, "snd_to_sample_1f", args, 2));
  return(package(prog, R_FLOAT, snd_to_sample_2f, "snd_to_sample_2f", args, 3));
}

 
static void snd_to_sample_0p(int *args, ptree *pt) {BOOL_RESULT = (Int)snd_to_sample_p(CLM_ARG_1);}

static xen_value *snd_to_sample_1p(ptree *prog, xen_value **args, int num_args)
{
  return(package(prog, R_BOOL, snd_to_sample_0p, "snd_to_sample_0p", args, 1));
}
#endif



/* ---------------- sample->file ---------------- */

static void sample_to_file_4(int *args, ptree *pt) {FLOAT_RESULT = mus_sample_to_file(CLM_ARG_1, INT_ARG_2, INT_ARG_3, FLOAT_ARG_4);}

static xen_value *sample_to_file_1(ptree *prog, xen_value **args, int num_args) 
{
  return(package(prog, R_FLOAT, sample_to_file_4, "sample_to_file_4", args, 4));
}


/* ---------------- locsig ---------------- */

static void locsig_3(int *args, ptree *pt) 
{
  mus_xen *gn;
  gn = (mus_xen *)mus_locsig_closure(CLM_ARG_1); /* skip the gen check in mus_environ */
  if (!gn)
    {
      mus_locsig(CLM_ARG_1, INT_ARG_2, FLOAT_ARG_3);
      FLOAT_RESULT = FLOAT_ARG_3;
    }
  else
    {
      mus_locsig(CLM_ARG_1, INT_ARG_2, FLOAT_ARG_3);
      /* now parcel out results based on gn->vcts state and mus_locsig_outf|revf */
      FLOAT_RESULT = mus_locsig_or_move_sound_to_vct_or_sound_data(gn, CLM_ARG_1, INT_ARG_2, FLOAT_ARG_3, true);
    }
}


static void locsig_3f(int *args, ptree *pt) {mus_locsig(CLM_ARG_1, INT_ARG_2, FLOAT_ARG_3);}

static void locsig_3f_mono_no_rev(int *args, ptree *pt)        {mus_locsig_mono_no_reverb       (CLM_ARG_1, INT_ARG_2, FLOAT_ARG_3);}
static void locsig_3f_mono(int *args, ptree *pt)               {mus_locsig_mono                 (CLM_ARG_1, INT_ARG_2, FLOAT_ARG_3);}
static void locsig_3f_stereo_no_rev(int *args, ptree *pt)      {mus_locsig_stereo_no_reverb     (CLM_ARG_1, INT_ARG_2, FLOAT_ARG_3);}
static void locsig_3f_stereo(int *args, ptree *pt)             {mus_locsig_stereo               (CLM_ARG_1, INT_ARG_2, FLOAT_ARG_3);}
static void locsig_3f_safe_mono_no_rev(int *args, ptree *pt)   {mus_locsig_safe_mono_no_reverb  (CLM_ARG_1, INT_ARG_2, FLOAT_ARG_3);}
static void locsig_3f_safe_mono(int *args, ptree *pt)          {mus_locsig_safe_mono            (CLM_ARG_1, INT_ARG_2, FLOAT_ARG_3);}
static void locsig_3f_safe_stereo_no_rev(int *args, ptree *pt) {mus_locsig_safe_stereo_no_reverb(CLM_ARG_1, INT_ARG_2, FLOAT_ARG_3);}
static void locsig_3f_safe_stereo(int *args, ptree *pt)        {mus_locsig_safe_stereo          (CLM_ARG_1, INT_ARG_2, FLOAT_ARG_3);}

static void locsig_v_mono_no_rev(int *args, ptree *pt) 
{
  mus_locsig_mono_no_reverb(CLM_ARG_5, INT_ARG_6, FLOAT_ARG_4 * mus_oscil_fm(CLM_ARG_1, FLOAT_ARG_2 + FLOAT_ARG_3));
}

static void locsig_v_mono(int *args, ptree *pt) 
{
  mus_locsig_mono(CLM_ARG_5, INT_ARG_6, FLOAT_ARG_4 * mus_oscil_fm(CLM_ARG_1, FLOAT_ARG_2 + FLOAT_ARG_3));
}

static void locsig_v_stereo_no_rev(int *args, ptree *pt) 
{
  mus_locsig_stereo_no_reverb(CLM_ARG_5, INT_ARG_6, FLOAT_ARG_4 * mus_oscil_fm(CLM_ARG_1, FLOAT_ARG_2 + FLOAT_ARG_3));
}

static void locsig_v_stereo(int *args, ptree *pt) 
{
  mus_locsig_stereo(CLM_ARG_5, INT_ARG_6, FLOAT_ARG_4 * mus_oscil_fm(CLM_ARG_1, FLOAT_ARG_2 + FLOAT_ARG_3));
}

static void locsig_v_safe_mono_no_rev(int *args, ptree *pt) 
{
  mus_locsig_safe_mono_no_reverb(CLM_ARG_5, INT_ARG_6, FLOAT_ARG_4 * mus_oscil_fm(CLM_ARG_1, FLOAT_ARG_2 + FLOAT_ARG_3));
}

static void locsig_v_safe_mono(int *args, ptree *pt) 
{
  mus_locsig_safe_mono(CLM_ARG_5, INT_ARG_6, FLOAT_ARG_4 * mus_oscil_fm(CLM_ARG_1, FLOAT_ARG_2 + FLOAT_ARG_3));
}

static void locsig_v_safe_stereo_no_rev(int *args, ptree *pt) 
{
  mus_locsig_safe_stereo_no_reverb(CLM_ARG_5, INT_ARG_6, FLOAT_ARG_4 * mus_oscil_fm(CLM_ARG_1, FLOAT_ARG_2 + FLOAT_ARG_3));
}

static void locsig_v_safe_stereo(int *args, ptree *pt) 
{
  mus_locsig_safe_stereo(CLM_ARG_5, INT_ARG_6, FLOAT_ARG_4 * mus_oscil_fm(CLM_ARG_1, FLOAT_ARG_2 + FLOAT_ARG_3));
}


static xen_value *locsig_1(ptree *prog, xen_value **args, int num_args) 
{
  /* choose output func based on the locsig output and reverb fields */

  if ((args[1]->type == R_CLM) &&
      (mus_locsig_p(prog->clms[args[1]->addr])) &&
      (prog->walk_result == DONT_NEED_RESULT))
    {
      mus_any *g;
      mus_xen *gn;
      g = prog->clms[args[1]->addr];
      gn = (mus_xen *)mus_locsig_closure(g); /* same as mus_environ(g) but skip the gen check in mus_environ */
      if (!gn)
	{
	  bool direct;
	  /* locsig "environ" not set (clm2xen.c) so there's nothing special about the outputs */
	  direct = (mus_locsig_safety(g) == 1);

	  /* oscil_1f_2_mult|env (5) (fmv),  multiply_f3(4) f2(3), add_f4(5) f3(4) f2(3), = 48 procs! */
	  if (prog->triple_ctr > 0)
	    {
	      triple *prev_op;
	      prev_op = prog->program[prog->triple_ctr - 1];
	      if ((prev_op->args[0] == args[3]->addr) &&
		  (prev_op->function == oscil_1f_2_mult) &&
		  (mus_locsig_channels(g) <= 2) &&
		  (mus_locsig_reverb_channels(g) <= 1) &&
		  ((find_var_in_ptree_via_addr(prog, R_FLOAT, prev_op->args[0])) == NULL))
		{
		  /* an experiment */

		  prev_op->types = (int *)realloc(prev_op->types, 7 * sizeof(int));
		  prev_op->args = (int *)realloc(prev_op->args, 7 * sizeof(int));
		  prev_op->num_args = 7;
		  prev_op->types[5] = R_CLM;
		  prev_op->args[5] = args[1]->addr;
		  prev_op->types[6] = R_INT;
		  prev_op->args[6] = args[2]->addr;

		  if (mus_locsig_channels(g) == 1)
		    {
		      if (mus_locsig_reverb_channels(g) == 0)
			{
			  prev_op->function = (direct) ? locsig_v_safe_mono_no_rev : locsig_v_mono_no_rev;
			  prev_op->op_name = (direct) ? "locsig_v_safe_mono_no_rev" : "locsig_v_mono_no_rev";
			}
		      else
			{
			  prev_op->function = (direct) ? locsig_v_safe_mono : locsig_v_mono;
			  prev_op->op_name = (direct) ? "locsig_v_safe_mono" : "locsig_v_mono";
			}
		    }
		  else
		    {
		      if (mus_locsig_reverb_channels(g) == 0)
			{
			  prev_op->function = (direct) ? locsig_v_safe_stereo_no_rev : locsig_v_stereo_no_rev;
			  prev_op->op_name = (direct) ? "locsig_v_safe_stereo_no_rev" : "locsig_v_stereo_no_rev";
			}
		      else
			{
			  prev_op->function = (direct) ? locsig_v_safe_stereo : locsig_v_stereo;
			  prev_op->op_name = (direct) ? "locsig_v_safe_stereo" : "locsig_v_stereo";
			}
		    }

#if WITH_COUNTERS
		  prev_op->func_loc = get_func_loc(prev_op->function, prev_op->op_name);
#endif				  
		  return(make_xen_value(R_FLOAT, prev_op->args[0], R_TEMPORARY));
		}
	    }

	  if (mus_locsig_channels(g) == 1)
	    {
	      if (mus_locsig_reverb_channels(g) == 0)
		return(package(prog, R_FLOAT, 
			       (direct) ? locsig_3f_safe_mono_no_rev : locsig_3f_mono_no_rev, 
			       (direct) ? "locsig_3f_safe_mono_no_rev" : "locsig_3f_mono_no_rev", args, 3));
	      if (mus_locsig_reverb_channels(g) == 1)
		return(package(prog, R_FLOAT, 
			       (direct) ? locsig_3f_safe_mono : locsig_3f_mono, 
			       (direct) ? "locsig_3f_safe_mono" : "locsig_3f_mono", args, 3));
	    }
	  else
	    {
	      if (mus_locsig_channels(g) == 2)
		{
		  if (mus_locsig_reverb_channels(g) == 0)
		    return(package(prog, R_FLOAT, 
				   (direct) ? locsig_3f_safe_stereo_no_rev : locsig_3f_stereo_no_rev, 
				   (direct) ? "locsig_3f_safe_stereo_no_rev" : "locsig_3f_stereo_no_rev", args, 3));
		  if (mus_locsig_reverb_channels(g) == 1)
		    return(package(prog, R_FLOAT, 
				   (direct) ? locsig_3f_safe_stereo : locsig_3f_stereo, 
				   (direct) ? "locsig_3f_safe_stereo" : "locsig_3f_stereo", args, 3));
		}
	    }
	  return(package(prog, R_FLOAT, locsig_3f, "locsig_3f", args, 3));
	}
    }
  return(package(prog, R_FLOAT, locsig_3, "locsig_3", args, 3));
}


/* ---------------- move_sound ---------------- */

static void move_sound_3(int *args, ptree *pt) 
{
  mus_xen *gn;
  gn = (mus_xen *)mus_move_sound_closure(CLM_ARG_1); /* skip the gen check in mus_environ */
  if (!gn)
    FLOAT_RESULT = mus_move_sound(CLM_ARG_1, INT_ARG_2, FLOAT_ARG_3);
  else
    {
      mus_move_sound(CLM_ARG_1, INT_ARG_2, FLOAT_ARG_3);
      FLOAT_RESULT = mus_locsig_or_move_sound_to_vct_or_sound_data(gn, CLM_ARG_1, INT_ARG_2, FLOAT_ARG_3, false);
    }
}

static xen_value *move_sound_1(ptree *prog, xen_value **args, int num_args) 
{
  return(package(prog, R_FLOAT, move_sound_3, "move_sound_3", args, 3));
}


/* ---------------- env-interp ---------------- */

static void env_interp_2(int *args, ptree *pt) {FLOAT_RESULT = mus_env_interp(FLOAT_ARG_1, CLM_ARG_2);}

static xen_value *env_interp_1(ptree *prog, xen_value **args, int num_args) 
{
  return(package(prog, R_FLOAT, env_interp_2, "env_interp_2", args, 2));
}


/* ---------------- env-any ---------------- */

#if (!HAVE_NESTED_FUNCTIONS) || __cplusplus
static ptree *env_any_outer_pt, *env_any_connect_pt;

static mus_float_t env_any_connect(mus_float_t y)
{
  ptree *outer, *inner;
  mus_float_t result;

  outer = env_any_outer_pt;
  inner = env_any_connect_pt;

  outer->dbls[inner->args[0]] = y;
  eval_embedded_ptree(inner, outer);

  env_any_outer_pt = outer;
  env_any_connect_pt = inner;

  result = outer->dbls[inner->result->addr];

  return(result);
}


static void env_any_2(int *args, ptree *pt) 
{
  env_any_outer_pt = pt;
  env_any_connect_pt = FNC_ARG_2;
  FLOAT_RESULT = mus_env_any(CLM_ARG_1, env_any_connect);
}

#else

static void env_any_2(int *args, ptree *pt) 
{
  auto mus_float_t env_any_connect(mus_float_t y); /* see comment in clm2xen */
  mus_float_t env_any_connect(mus_float_t y)
  {
    pt->dbls[FNC_ARG_2->args[0]] = y;
    eval_embedded_ptree(FNC_ARG_2, pt);
    return(pt->dbls[FNC_ARG_2->result->addr]);
  }

  FLOAT_RESULT = mus_env_any(CLM_ARG_1, env_any_connect);
}

#endif


static xen_value *env_any_1(ptree *prog, xen_value **args, int num_args) 
{
  return(package(prog, R_FLOAT, env_any_2, "env_any_2", args, 2));
}


/* ---------------- frame+ etc ---------------- */

#define FRAME_OP(CName, SName, cfName) \
static void CName ## _2cc(int *args, ptree *pt) {if (CLM_RESULT) mus_free(CLM_RESULT); CLM_RESULT = CName(CLM_ARG_1, CLM_ARG_2, NULL);} \
static void CName ## _2cf(int *args, ptree *pt) {if (CLM_RESULT) mus_free(CLM_RESULT); CLM_RESULT = cfName(CLM_ARG_1, FLOAT_ARG_2, NULL);} \
static void CName ## _2fc(int *args, ptree *pt) {if (CLM_RESULT) mus_free(CLM_RESULT); CLM_RESULT = cfName(CLM_ARG_2, FLOAT_ARG_1, NULL);} \
static void CName ## _3cc(int *args, ptree *pt) {CLM_RESULT = CName(CLM_ARG_1, CLM_ARG_2, CLM_ARG_3);} \
static void CName ## _3cf(int *args, ptree *pt) {CLM_RESULT = cfName(CLM_ARG_1, FLOAT_ARG_2, CLM_ARG_3);} \
static void CName ## _3fc(int *args, ptree *pt) {CLM_RESULT = cfName(CLM_ARG_2, FLOAT_ARG_1, CLM_ARG_3);} \
static xen_value * CName ## _1(ptree *prog, xen_value **args, int num_args) \
{ \
  xen_value *temp; \
  if (args[1]->type == R_INT) {temp = args[1]; args[1] = convert_int_to_dbl(prog, args[1]); free(temp);} \
  if (args[2]->type == R_INT) {temp = args[2]; args[2] = convert_int_to_dbl(prog, args[2]); free(temp);} \
  if (args[1]->type == R_FLOAT) \
    { \
      if (args[2]->type == R_FLOAT) return(run_warn("no mixer or frame passed to " #SName)); \
      if (num_args == 2) \
	return(package(prog, R_CLM, CName ## _2fc, #CName "_2fc", args, 2)); \
      return(package(prog, R_CLM, CName ## _3fc, #CName "_3fc", args, 3)); \
    } \
  if (args[2]->type == R_FLOAT) \
    { \
      if (num_args == 2) \
	return(package(prog, R_CLM, CName ## _2cf, #CName "_2cf", args, 2)); \
      return(package(prog, R_CLM, CName ## _3cf, #CName "_3cf", args, 3)); \
    } \
  if (num_args == 2) \
    return(package(prog, R_CLM, CName ## _2cc, #CName "_2cc", args, 2)); \
  return(package(prog, R_CLM, CName ## _3cc, #CName "_3cc", args, 3)); \
}

FRAME_OP(mus_frame_add, frame+, mus_frame_offset)
FRAME_OP(mus_frame_multiply, frame*, mus_frame_scale)
FRAME_OP(mus_mixer_multiply, mixer*, mus_mixer_scale)
FRAME_OP(mus_mixer_add, mixer+, mus_mixer_offset)


static void frame_v(int *args, ptree *pt) 
{
  int i;
  mus_any *v;
  if (CLM_RESULT) {mus_free(CLM_RESULT); CLM_RESULT = NULL;}
  v = mus_make_frame(INT_ARG_1);
  for (i = 0; i < INT_ARG_1; i++)
    mus_frame_set(v, i, pt->dbls[args[i + 2]]);
  CLM_RESULT = v;
}


static xen_value *frame_1(ptree *prog, xen_value **args, int num_args)
{
  xen_value *rtn;
  float_all_args(prog, num_args, args, true);
  rtn = package_n(prog, R_CLM, frame_v, "frame_v", args, num_args);
  add_obj_to_gcs(prog, R_CLM, rtn->addr);
  return(rtn);
}


static void mixer_v(int *args, ptree *pt) 
{
  int len;
  mus_any *v;
  if (CLM_RESULT) {mus_free(CLM_RESULT); CLM_RESULT = NULL;}
  len = (int)ceil(sqrt(INT_ARG_1));
  v = mus_make_empty_mixer((len == 0) ? 1 : len);
  if (len > 0)
    {
      int i, j = 0, k = 0;
      for (i = 0; i < INT_ARG_1; i++)
	{
	  mus_mixer_set(v, j, k, pt->dbls[args[i + 2]]);
	  k++;
	  if (k == len)
	    {
	      j++;
	      k = 0;
	    }
	}
    }
  CLM_RESULT = v;
}


static xen_value *mixer_1(ptree *prog, xen_value **args, int num_args)
{
  xen_value *rtn;
  float_all_args(prog, num_args, args, true);
  rtn = package_n(prog, R_CLM, mixer_v, "mixer_v", args, num_args);
  add_obj_to_gcs(prog, R_CLM, rtn->addr);
  return(rtn);
}




/* ---------------- frame->frame ---------------- */

static void frame_to_frame_2(int *args, ptree *pt)
{
  if (CLM_RESULT) mus_free(CLM_RESULT);
  CLM_RESULT = mus_frame_to_frame(CLM_ARG_1, CLM_ARG_2, NULL);
}

static void frame_to_frame_3(int *args, ptree *pt)
{
  CLM_RESULT = mus_frame_to_frame(CLM_ARG_1, CLM_ARG_2, CLM_ARG_3);
}

static void frame_to_frame_mono(int *args, ptree *pt)
{
  CLM_RESULT = mus_frame_to_frame_mono(CLM_ARG_1, CLM_ARG_2, CLM_ARG_3);
}

static void frame_to_frame_mono_to_stereo(int *args, ptree *pt)
{
  CLM_RESULT = mus_frame_to_frame_mono_to_stereo(CLM_ARG_1, CLM_ARG_2, CLM_ARG_3);
}

static void frame_to_frame_stereo(int *args, ptree *pt)
{
  CLM_RESULT = mus_frame_to_frame_stereo(CLM_ARG_1, CLM_ARG_2, CLM_ARG_3);
}

static xen_value *frame_to_frame_1(ptree *prog, xen_value **args, int num_args)
{
  if (num_args == 2) return(package(prog, R_CLM, frame_to_frame_2, "frame_to_frame_2", args, 2));

  if ((args[1]->type == R_CLM) &&
      (mus_frame_p(prog->clms[args[1]->addr])) &&
      (args[2]->type == R_CLM) &&
      (mus_mixer_p(prog->clms[args[2]->addr])) &&
      (args[3]->type == R_CLM) &&
      (mus_frame_p(prog->clms[args[3]->addr])))
    {
      if ((mus_channels(prog->clms[args[1]->addr]) == 1) &&
	  (mus_channels(prog->clms[args[2]->addr]) >= 1) &&
	  (mus_channels(prog->clms[args[3]->addr]) == 1))
	return(package(prog, R_CLM, frame_to_frame_mono, "frame_to_frame_mono", args, 3));

      if ((mus_channels(prog->clms[args[1]->addr]) == 2) &&
	  (mus_channels(prog->clms[args[2]->addr]) >= 2) &&
	  (mus_channels(prog->clms[args[3]->addr]) == 2))
	return(package(prog, R_CLM, frame_to_frame_stereo, "frame_to_frame_stereo", args, 3));

      if ((mus_channels(prog->clms[args[1]->addr]) == 1) &&
	  (mus_channels(prog->clms[args[2]->addr]) >= 2) &&
	  (mus_channels(prog->clms[args[3]->addr]) == 2))
	return(package(prog, R_CLM, frame_to_frame_mono_to_stereo, "frame_to_frame_mono_to_stereo", args, 3));

    }

  return(package(prog, R_CLM, frame_to_frame_3, "frame_to_frame_3", args, 3));
}

/* frame_to_frame_via_mixer chans ok, out already allocated, if 1 chan just mult etc, arg1=frame, arg2=mixer, arg3=frame */


/* ---------------- frame->sample ---------------- */

static void frame_to_sample_2(int *args, ptree *pt) 
{
  FLOAT_RESULT = mus_frame_to_sample(CLM_ARG_1, CLM_ARG_2);
}

static xen_value *frame_to_sample_1(ptree *prog, xen_value **args, int num_args) 
{
  return(package(prog, R_FLOAT, frame_to_sample_2, "frame_to_sample_2", args, 2));
}


/* ---------------- sample->frame ---------------- */

static void sample_to_frame_2(int *args, ptree *pt) {CLM_RESULT = mus_sample_to_frame(CLM_ARG_1, FLOAT_ARG_2, CLM_RESULT);}

/* there may be a memory leak here -- how does the possibly new frame get gc'd? */

static void sample_to_frame_3(int *args, ptree *pt) 
{
  CLM_RESULT = mus_sample_to_frame(CLM_ARG_1, FLOAT_ARG_2, CLM_ARG_3);
}

static xen_value *sample_to_frame_1(ptree *prog, xen_value **args, int num_args) 
{
  if (num_args == 2) return(package(prog, R_CLM, sample_to_frame_2, "sample_to_frame_2", args, 2));
  return(package(prog, R_CLM, sample_to_frame_3, "sample_to_frame_3", args, 3));
}


/* ---------------- frame->file ---------------- */

static void frame_to_file_3(int *args, ptree *pt) 
{
  CLM_RESULT = mus_frame_to_file(CLM_ARG_1, INT_ARG_2, CLM_ARG_3);
}

static xen_value *frame_to_file_1(ptree *prog, xen_value **args, int num_args) 
{
  return(package(prog, R_CLM, frame_to_file_3, "frame_to_file_3", args, 3));
}


/* ---------------- file->frame ---------------- */

static void file_to_frame_3(int *args, ptree *pt) 
{
  CLM_RESULT = mus_file_to_frame(CLM_ARG_1, INT_ARG_2, CLM_ARG_3);
}

static void file_to_frame_2(int *args, ptree *pt) 
{
  if (CLM_RESULT) mus_free(CLM_RESULT);
  CLM_RESULT = mus_file_to_frame(CLM_ARG_1, INT_ARG_2, NULL);
}

static xen_value *file_to_frame_1(ptree *prog, xen_value **args, int num_args) 
{
  if (num_args == 2)
    return(package(prog, R_CLM, file_to_frame_2, "file_to_frame_2", args, 2));
  return(package(prog, R_CLM, file_to_frame_3, "file_to_frame_3", args, 3));
}


/* ---------------- out-any ---------------- */

/* see env_any for R_FUNCTION as arg */

static void outa_3(int *args, ptree *pt)               {FLOAT_RESULT = mus_out_any(INT_ARG_1, FLOAT_ARG_2, 0, CLM_ARG_3);}
static void outa_3f(int *args, ptree *pt)              {mus_out_any_to_file(CLM_ARG_3, INT_ARG_1, 0, FLOAT_ARG_2);}
static void outa_oscil_0_mult(int *args, ptree *pt)    {mus_out_any_to_file(CLM_ARG_3, INT_ARG_1, 0, FLOAT_ARG_2 * mus_oscil_unmodulated(CLM_ARG_4));}
static void outa_add_f2(int *args, ptree *pt)          {mus_out_any_to_file(CLM_ARG_3, INT_ARG_1, 0, FLOAT_ARG_2 + FLOAT_ARG_4);}
static void outa_add_f3(int *args, ptree *pt)          {mus_out_any_to_file(CLM_ARG_3, INT_ARG_1, 0, FLOAT_ARG_2 + FLOAT_ARG_4 + FLOAT_ARG_5);}
static void outa_add_f4(int *args, ptree *pt)          {mus_out_any_to_file(CLM_ARG_3, INT_ARG_1, 0, FLOAT_ARG_2 + FLOAT_ARG_4 + FLOAT_ARG_5 + FLOAT_ARG_6);}
static void outa_multiply_f2(int *args, ptree *pt)     {mus_out_any_to_file(CLM_ARG_3, INT_ARG_1, 0, FLOAT_ARG_2 * FLOAT_ARG_4);}
static void outa_multiply_f3(int *args, ptree *pt)     {mus_out_any_to_file(CLM_ARG_3, INT_ARG_1, 0, FLOAT_ARG_2 * FLOAT_ARG_4 * FLOAT_ARG_5);}
static void outa_multiply_f4(int *args, ptree *pt)     {mus_out_any_to_file(CLM_ARG_3, INT_ARG_1, 0, FLOAT_ARG_2 * FLOAT_ARG_4 * FLOAT_ARG_5 * FLOAT_ARG_6);}
static void outa_oscil_1_mult(int *args, ptree *pt)    {mus_out_any_to_file(CLM_ARG_3, INT_ARG_1, 0, FLOAT_ARG_5 * mus_oscil_fm(CLM_ARG_4, FLOAT_ARG_2));}
static void outa_polywave_1_mult(int *args, ptree *pt) {mus_out_any_to_file(CLM_ARG_3, INT_ARG_1, 0, FLOAT_ARG_5 * mus_polywave(CLM_ARG_4, FLOAT_ARG_2));}
static void outa_delay_1_mult(int *args, ptree *pt)    {mus_out_any_to_file(CLM_ARG_3, INT_ARG_1, 0, FLOAT_ARG_5 * mus_delay_unmodulated_noz(CLM_ARG_4, FLOAT_ARG_2));}
static void outa_add_f2_mult(int *args, ptree *pt)     {mus_out_any_to_file(CLM_ARG_3, INT_ARG_1, 0, FLOAT_ARG_5 * (FLOAT_ARG_2 + FLOAT_ARG_4));}
static void outa_add_f3_mult(int *args, ptree *pt)     {mus_out_any_to_file(CLM_ARG_3, INT_ARG_1, 0, FLOAT_ARG_4 * (FLOAT_ARG_2 + FLOAT_ARG_5 + FLOAT_ARG_6));}
static void outa_formant_1_mult(int *args, ptree *pt)  {mus_out_any_to_file(CLM_ARG_3, INT_ARG_1, 0, FLOAT_ARG_5 * mus_formant(CLM_ARG_4, FLOAT_ARG_2));}
static void outa_firmant_1_mult(int *args, ptree *pt)  {mus_out_any_to_file(CLM_ARG_3, INT_ARG_1, 0, FLOAT_ARG_5 * mus_firmant(CLM_ARG_4, FLOAT_ARG_2));}

static void outb_3(int *args, ptree *pt)               {FLOAT_RESULT = mus_out_any(INT_ARG_1, FLOAT_ARG_2, 1, CLM_ARG_3);}
static void outb_3f(int *args, ptree *pt)              {mus_out_any_to_file(CLM_ARG_3, INT_ARG_1, 1, FLOAT_ARG_2);}
static void outb_oscil_0_mult(int *args, ptree *pt)    {mus_out_any_to_file(CLM_ARG_3, INT_ARG_1, 1, FLOAT_ARG_2 * mus_oscil_unmodulated(CLM_ARG_4));}
static void outb_add_f2(int *args, ptree *pt)          {mus_out_any_to_file(CLM_ARG_3, INT_ARG_1, 1, FLOAT_ARG_2 + FLOAT_ARG_4);}
static void outb_add_f3(int *args, ptree *pt)          {mus_out_any_to_file(CLM_ARG_3, INT_ARG_1, 1, FLOAT_ARG_2 + FLOAT_ARG_4 + FLOAT_ARG_5);}
static void outb_add_f4(int *args, ptree *pt)          {mus_out_any_to_file(CLM_ARG_3, INT_ARG_1, 1, FLOAT_ARG_2 + FLOAT_ARG_4 + FLOAT_ARG_5 + FLOAT_ARG_6);}
static void outb_multiply_f2(int *args, ptree *pt)     {mus_out_any_to_file(CLM_ARG_3, INT_ARG_1, 1, FLOAT_ARG_2 * FLOAT_ARG_4);}
static void outb_multiply_f3(int *args, ptree *pt)     {mus_out_any_to_file(CLM_ARG_3, INT_ARG_1, 1, FLOAT_ARG_2 * FLOAT_ARG_4 * FLOAT_ARG_5);}
static void outb_multiply_f4(int *args, ptree *pt)     {mus_out_any_to_file(CLM_ARG_3, INT_ARG_1, 1, FLOAT_ARG_2 * FLOAT_ARG_4 * FLOAT_ARG_5 * FLOAT_ARG_6);}
static void outb_oscil_1_mult(int *args, ptree *pt)    {mus_out_any_to_file(CLM_ARG_3, INT_ARG_1, 1, FLOAT_ARG_5 * mus_oscil_fm(CLM_ARG_4, FLOAT_ARG_2));}
static void outb_polywave_1_mult(int *args, ptree *pt) {mus_out_any_to_file(CLM_ARG_3, INT_ARG_1, 1, FLOAT_ARG_5 * mus_polywave(CLM_ARG_4, FLOAT_ARG_2));}
static void outb_delay_1_mult(int *args, ptree *pt)    {mus_out_any_to_file(CLM_ARG_3, INT_ARG_1, 1, FLOAT_ARG_5 * mus_delay_unmodulated_noz(CLM_ARG_4, FLOAT_ARG_2));}
static void outb_add_f2_mult(int *args, ptree *pt)     {mus_out_any_to_file(CLM_ARG_3, INT_ARG_1, 1, FLOAT_ARG_5 * (FLOAT_ARG_2 + FLOAT_ARG_4));}
static void outb_add_f3_mult(int *args, ptree *pt)     {mus_out_any_to_file(CLM_ARG_3, INT_ARG_1, 1, FLOAT_ARG_4 * (FLOAT_ARG_2 + FLOAT_ARG_5 + FLOAT_ARG_6));}
static void outb_formant_1_mult(int *args, ptree *pt)  {mus_out_any_to_file(CLM_ARG_3, INT_ARG_1, 1, FLOAT_ARG_5 * mus_formant(CLM_ARG_4, FLOAT_ARG_2));}
static void outb_firmant_1_mult(int *args, ptree *pt)  {mus_out_any_to_file(CLM_ARG_3, INT_ARG_1, 1, FLOAT_ARG_5 * mus_firmant(CLM_ARG_4, FLOAT_ARG_2));}

static void outc_3(int *args, ptree *pt) {FLOAT_RESULT = mus_out_any(INT_ARG_1, FLOAT_ARG_2, 2, CLM_ARG_3);}
static void outc_3f(int *args, ptree *pt) {mus_out_any_to_file(CLM_ARG_3, INT_ARG_1, 2, FLOAT_ARG_2);}

static void outd_3(int *args, ptree *pt) {FLOAT_RESULT = mus_out_any(INT_ARG_1, FLOAT_ARG_2, 3, CLM_ARG_3);}
static void outd_3f(int *args, ptree *pt) {mus_out_any_to_file(CLM_ARG_3, INT_ARG_1, 3, FLOAT_ARG_2);}

static void out_any_4(int *args, ptree *pt) {FLOAT_RESULT = mus_out_any(INT_ARG_1, FLOAT_ARG_2, INT_ARG_3, CLM_ARG_4);} 
static void out_any_4f(int *args, ptree *pt) {mus_out_any_to_file(CLM_ARG_4, INT_ARG_1, INT_ARG_3, FLOAT_ARG_2);}


static void out_f_3(int *args, ptree *pt) {FLOAT_RESULT = FLOAT_ARG_2;}

static void out_any_f_4(int *args, ptree *pt) {FLOAT_RESULT = FLOAT_ARG_2;}


static mus_float_t call_out_any_function(ptree *pt, Int loc, mus_float_t val, Int chan, ptree *function)
{
  pt->ints[function->args[0]] = loc;
  pt->dbls[function->args[1]] = val;
  pt->ints[function->args[2]] = chan; 
  eval_embedded_ptree(function, pt);
  return(pt->dbls[function->result->addr]);
}

static void outa_function_3(int *args, ptree *pt) 
{
  FLOAT_RESULT = call_out_any_function(pt, INT_ARG_1, FLOAT_ARG_2, 0, FNC_ARG_3);
}

static void outb_function_3(int *args, ptree *pt) 
{
  FLOAT_RESULT = call_out_any_function(pt, INT_ARG_1, FLOAT_ARG_2, 1, FNC_ARG_3);
}

static void outc_function_3(int *args, ptree *pt) 
{
  FLOAT_RESULT = call_out_any_function(pt, INT_ARG_1, FLOAT_ARG_2, 2, FNC_ARG_3);
}

static void outd_function_3(int *args, ptree *pt) 
{
  FLOAT_RESULT = call_out_any_function(pt, INT_ARG_1, FLOAT_ARG_2, 3, FNC_ARG_3);
}

static void out_any_function_4(int *args, ptree *pt) 
{
  FLOAT_RESULT = call_out_any_function(pt, INT_ARG_1, FLOAT_ARG_2, INT_ARG_3, FNC_ARG_4);
}


static void out_vct_3(int *args, ptree *pt) 
{
  if ((INT_ARG_1 < VCT_ARG_3->length) && (INT_ARG_1 >= 0))
    VCT_ARG_3->data[INT_ARG_1] += FLOAT_ARG_2; 
  FLOAT_RESULT = FLOAT_ARG_2;
}

static void out_any_vct_4(int *args, ptree *pt) 
{
  if ((INT_ARG_1 < VCT_ARG_4->length) && (INT_ARG_1 >= 0))
    VCT_ARG_4->data[INT_ARG_1] += FLOAT_ARG_2; 
  FLOAT_RESULT = FLOAT_ARG_2;
}

static void outa_sound_data_3(int *args, ptree *pt) 
{
  if ((INT_ARG_1 < SOUND_DATA_ARG_3->length) && (INT_ARG_1 >= 0))
    SOUND_DATA_ARG_3->data[0][INT_ARG_1] += FLOAT_ARG_2; 
  FLOAT_RESULT = FLOAT_ARG_2;
}

static void outb_sound_data_3(int *args, ptree *pt) 
{
  if ((INT_ARG_1 < SOUND_DATA_ARG_3->length) && (SOUND_DATA_ARG_3->chans > 1) && (INT_ARG_1 >= 0))
    SOUND_DATA_ARG_3->data[1][INT_ARG_1] += FLOAT_ARG_2; 
  FLOAT_RESULT = FLOAT_ARG_2;
}

static void outc_sound_data_3(int *args, ptree *pt) 
{
  if ((INT_ARG_1 < SOUND_DATA_ARG_3->length) && (SOUND_DATA_ARG_3->chans > 2) && (INT_ARG_1 >= 0))
    SOUND_DATA_ARG_3->data[2][INT_ARG_1] += FLOAT_ARG_2; 
  FLOAT_RESULT = FLOAT_ARG_2;
}

static void outd_sound_data_3(int *args, ptree *pt) 
{  
  if ((INT_ARG_1 < SOUND_DATA_ARG_3->length) && (SOUND_DATA_ARG_3->chans > 3) && (INT_ARG_1 >= 0))
    SOUND_DATA_ARG_3->data[3][INT_ARG_1] += FLOAT_ARG_2; 
  FLOAT_RESULT = FLOAT_ARG_2;
}

static void out_any_sound_data_4(int *args, ptree *pt) 
{
  if ((INT_ARG_1 < SOUND_DATA_ARG_4->length) && (SOUND_DATA_ARG_4->chans > INT_ARG_3) && (INT_ARG_1 >= 0))
    SOUND_DATA_ARG_4->data[INT_ARG_3][INT_ARG_1] += FLOAT_ARG_2; 
  FLOAT_RESULT = FLOAT_ARG_2;
}


/* outa optimizations */

#define NUM_O3_OPS 3

static opt_ops o3_ops[NUM_O3_OPS] = {
  {add_f2, "add_f2", outa_add_f2, "outa_add_f2", outb_add_f2, "outb_add_f2"},
  {multiply_f2, "multiply_f2", outa_multiply_f2, "outa_multiply_f2", outb_multiply_f2, "outb_multiply_f2"},
  {oscil_0f_1_mult, "oscil_0f_mult", outa_oscil_0_mult, "outa_oscil_0_mult", outb_oscil_0_mult, "outb_oscil_0_mult"},
};


static int find_o3_op(triple *prev_op)
{
  int i;
  for (i = 0; i < NUM_O3_OPS; i++)
    if (prev_op->function == o3_ops[i].func)
      return(i);
  return(-1);
}


#define NUM_O4_OPS 9

static opt_ops o4_ops[NUM_O4_OPS] = {
  {add_f3, "add_f3", outa_add_f3, "outa_add_f3", outb_add_f3, "outb_add_f3"},
  {multiply_f3, "multiply_f3", outa_multiply_f3, "outa_multiply_f3", outb_multiply_f3, "outb_multiply_f3"},
  {add_f2_mult, "add_f2_mult", outa_add_f2_mult, "outa_add_f2_mult", outb_add_f2_mult, "outb_add_f2_mult"},
  {oscil_1f_1_mult, "oscil_1f_1_mult", outa_oscil_1_mult, "outa_oscil_1_mult", outb_oscil_1_mult, "outb_oscil_1_mult"},
  {polywave_1f_mult, "polywave_1f_mult", outa_polywave_1_mult, "outa_polywave_1_mult", outb_polywave_1_mult, "outb_polywave_1_mult"},
  {delay_1f_noz_mult, "delay_1f_noz_mult", outa_delay_1_mult, "outa_delay_1_mult", outb_delay_1_mult, "outb_delay_1_mult"},
  {funcall_cf2, "funcall_cf2", outa_funcall_cf2, "outa_funcall_cf2", outb_funcall_cf2, "outb_funcall_cf2"},
  {formant_1f_mult, "formant_1f_mult", outa_formant_1_mult, "outa_formant_1_mult", outb_formant_1_mult, "outb_formant_1_mult"},
  {firmant_1f_mult, "firmant_1f_mult", outa_firmant_1_mult, "outa_firmant_1_mult", outb_firmant_1_mult, "outb_firmant_1_mult"},
};


static int find_o4_op(triple *prev_op)
{
  int i;
  for (i = 0; i < NUM_O4_OPS; i++)
    if (prev_op->function == o4_ops[i].func)
      return(i);
  return(-1);
}

#define NUM_O5_OPS 3

static opt_ops o5_ops[NUM_O5_OPS] = {
  {add_f4, "add_f4", outa_add_f4, "outa_add_f4", outb_add_f4, "outb_add_f4"},
  {multiply_f4, "multiply_f4", outa_multiply_f4, "outa_multiply_f4", outb_multiply_f4, "outb_multiply_f4"},
  {add_f3_mult, "add_f3_mult", outa_add_f3_mult, "outa_add_f3_mult", outb_add_f3_mult, "outb_add_f3_mult"},
};


/* polywave_1f_env */


static int find_o5_op(triple *prev_op)
{
  int i;
  for (i = 0; i < NUM_O5_OPS; i++)
    if (prev_op->function == o5_ops[i].func)
      return(i);
  return(-1);
}


static xen_value *out_opt(ptree *prog, xen_value **args, int num_args, int channel)
{
  if ((prog->walk_result == DONT_NEED_RESULT) &&
      (mus_output_p(prog->clms[args[3]->addr])))   
    {
      if (prog->triple_ctr > 0)
	{
	  triple *prev_op;
	  prev_op = prog->program[prog->triple_ctr - 1];
	  if ((prev_op->args[0] == args[2]->addr) &&
	      ((find_var_in_ptree_via_addr(prog, R_FLOAT, prev_op->args[0])) == NULL))
	    {
	      int loc;
	      switch (prev_op->num_args)
		{
		  
		  /* -------- 3 args -------- */
		case 3:
		  loc = find_o3_op(prev_op);
		  /* (with-sound () (let ((x 1.0) (y 2.0)) (run (lambda () (do ((i 0 (+ i 1))) ((= i 2)) (outa i (+ x y) )))))) */
		  if (loc >= 0)
		    {
		      /* 1 -> 4 */
		      prev_op->types = (int *)realloc(prev_op->types, 5 * sizeof(int));
		      prev_op->args = (int *)realloc(prev_op->args, 5 * sizeof(int));
		      prev_op->num_args = 5;
		      /* reshuffle args in prev_op so that arg1 -> arg4, (arg2 can stay), then arg1 <- cur1, arg3 <- cur3 */

		      prev_op->types[4] = prev_op->types[1]; /* transfer 1 -> 4 */
		      prev_op->args[4] = prev_op->args[1];   /* new arg1 is the sample loc */
		      prev_op->types[1] = R_INT;             /* sample location */
		      prev_op->args[1] = args[1]->addr;

		      /* args[2]->addr is the old output which we ignore, using the old arg2 as it is */

		      prev_op->types[3] = R_CLM;             /* output writer */
		      prev_op->args[3] = args[3]->addr;
		      
		      if (channel == 0)
			{
			  prev_op->function = o3_ops[loc].mult_func;
			  prev_op->op_name = o3_ops[loc].mult_func_name;
			}
		      else
			{
			  prev_op->function = o3_ops[loc].env_func;
			  prev_op->op_name = o3_ops[loc].env_func_name;
			}
#if WITH_COUNTERS
		      prev_op->func_loc = get_func_loc(prev_op->function, prev_op->op_name);
#endif				  
		      return(make_xen_value(R_FLOAT, prev_op->args[0], R_TEMPORARY));
		    }
		  break;

		  /* -------- 4 args -------- */
		case 4:
		  loc = find_o4_op(prev_op);
		  if (loc >= 0)
		    {
		      /* move old 1 to 4, old 3 to 5, leave 2, fixup old 1 as loc, old 3 as output gen */
		      /* (with-sound () (let ((gen (make-oscil 100.0)) (x 0.5) (y 0.25)) (run (lambda () (do ((i 0 (+ i 1))) ((= i 2)) (outa i (* y (oscil gen x)))))))) */
		      
		      prev_op->types = (int *)realloc(prev_op->types, 6 * sizeof(int));
		      prev_op->args = (int *)realloc(prev_op->args, 6 * sizeof(int));
		      prev_op->num_args = 6;
		      
		      prev_op->types[4] = prev_op->types[1]; /* transfer 1 -> 4 */
		      prev_op->args[4] = prev_op->args[1];   /* new arg1 is the sample loc */
		      prev_op->types[1] = R_INT;             /* sample location */
		      prev_op->args[1] = args[1]->addr;
		      
		      /* args[2]->addr is the old output which we ignore, using the old arg2 as it is */
		      
		      prev_op->types[5] = prev_op->types[3]; /* transfer 3 -> 5 */
		      prev_op->args[5] = prev_op->args[3];   /* new arg3 is the writer gen */
		      prev_op->types[3] = R_CLM;             /* output writer */
		      prev_op->args[3] = args[3]->addr;
		      
		      if (channel == 0)
			{
			  prev_op->function = o4_ops[loc].mult_func;
			  prev_op->op_name = o4_ops[loc].mult_func_name;
			}
		      else
			{
			  prev_op->function = o4_ops[loc].env_func;
			  prev_op->op_name = o4_ops[loc].env_func_name;
			}
#if WITH_COUNTERS
		      prev_op->func_loc = get_func_loc(prev_op->function, prev_op->op_name);
#endif				  
		      return(make_xen_value(R_FLOAT, prev_op->args[0], R_TEMPORARY));
		    }
		  break;
		  
		  /* -------- 5 args -------- */
		case 5:
		  loc = find_o5_op(prev_op);
		  if (loc >= 0)
		    {
		      /* move old 1 to 5, old 3 to 6, leave 2 and 4, fixup old 1 as loc, old 3 as output gen */
		      prev_op->types = (int *)realloc(prev_op->types, 7 * sizeof(int));
		      prev_op->args = (int *)realloc(prev_op->args, 7 * sizeof(int));
		      prev_op->num_args = 7;
		      
		      prev_op->types[5] = prev_op->types[1]; /* transfer 1 -> 5 */
		      prev_op->args[5] = prev_op->args[1];   /* new arg1 is the sample loc */
		      prev_op->types[1] = R_INT;             /* sample location */
		      prev_op->args[1] = args[1]->addr;
		      
		      /* 2 and 4 can stay */
		      
		      prev_op->types[6] = prev_op->types[3]; /* transfer 3 -> 6 */
		      prev_op->args[6] = prev_op->args[3];   /* new arg3 is the writer gen */
		      prev_op->types[3] = R_CLM;             /* output writer */
		      prev_op->args[3] = args[3]->addr;
		      
		      if (channel == 0)
			{
			  prev_op->function = o5_ops[loc].mult_func;
			  prev_op->op_name = o5_ops[loc].mult_func_name;
			}
		      else
			{
			  prev_op->function = o5_ops[loc].env_func;
			  prev_op->op_name = o5_ops[loc].env_func_name;
			}
#if WITH_COUNTERS
		      prev_op->func_loc = get_func_loc(prev_op->function, prev_op->op_name);
#endif				  
		      return(make_xen_value(R_FLOAT, prev_op->args[0], R_TEMPORARY));
		    }
		  break;
		}
	    }
	}
      if (channel == 0)
	return(package(prog, R_FLOAT, outa_3f, "outa_3f", args, 3));
      return(package(prog, R_FLOAT, outb_3f, "outb_3f", args, 3));
    }
  if (channel == 0)
    return(package(prog, R_FLOAT, outa_3, "outa_3", args, 3));
  return(package(prog, R_FLOAT, outb_3, "outb_3", args, 3));
}


static xen_value *outa_2(ptree *prog, xen_value **args, int num_args) 
{
  if (args[3]->type == R_CLM)
    return(out_opt(prog, args, num_args, 0));
  if (args[3]->type == R_VCT)
    return(package(prog, R_FLOAT, out_vct_3, "outa_vct_3", args, 3));
  if (args[3]->type == R_SOUND_DATA)
    return(package(prog, R_FLOAT, outa_sound_data_3, "outa_sound_data_3", args, 3));
  if (args[3]->type == R_FUNCTION)
    return(package(prog, R_FLOAT, outa_function_3, "outa_function_3", args, 3));
  return(package(prog, R_FLOAT, out_f_3, "out_f_3", args, 3));  
}

/* output to a vct can only be mono, so outb to vct doesn't make sense */

static xen_value *outb_2(ptree *prog, xen_value **args, int num_args) 
{
  if (args[3]->type == R_CLM)
    return(out_opt(prog, args, num_args, 1));
  if (args[3]->type == R_SOUND_DATA)
    return(package(prog, R_FLOAT, outb_sound_data_3, "outb_sound_data_3", args, 3));
  if (args[3]->type == R_FUNCTION)
    return(package(prog, R_FLOAT, outb_function_3, "outb_function_3", args, 3));
  return(package(prog, R_FLOAT, out_f_3, "out_f_3", args, 3));  
}

static xen_value *outc_2(ptree *prog, xen_value **args, int num_args) 
{
  if (args[3]->type == R_CLM)
    {
      if ((prog->walk_result == DONT_NEED_RESULT) &&
	  (mus_output_p(prog->clms[args[3]->addr])))   
	return(package(prog, R_FLOAT, outc_3f, "outc_3f", args, 3));
      return(package(prog, R_FLOAT, outc_3, "outc_3", args, 3));
    }
  if (args[3]->type == R_SOUND_DATA)
    return(package(prog, R_FLOAT, outc_sound_data_3, "outc_sound_data_3", args, 3));
  if (args[3]->type == R_FUNCTION)
    return(package(prog, R_FLOAT, outc_function_3, "outc_function_3", args, 3));
  return(package(prog, R_FLOAT, out_f_3, "out_f_3", args, 3));  
}


static xen_value *outd_2(ptree *prog, xen_value **args, int num_args) 
{
  if (args[3]->type == R_CLM)
    {
      if ((prog->walk_result == DONT_NEED_RESULT) &&
	  (mus_output_p(prog->clms[args[3]->addr])))   
	return(package(prog, R_FLOAT, outd_3f, "outd_3f", args, 3));
      return(package(prog, R_FLOAT, outd_3, "outd_3", args, 3));
    }
  if (args[3]->type == R_SOUND_DATA)
    return(package(prog, R_FLOAT, outd_sound_data_3, "outd_sound_data_3", args, 3));
  if (args[3]->type == R_FUNCTION)
    return(package(prog, R_FLOAT, outd_function_3, "outd_function_3", args, 3));
  return(package(prog, R_FLOAT, out_f_3, "out_f_3", args, 3));  
}


static xen_value *out_any_2(ptree *prog, xen_value **args, int num_args)
{
  if (args[4]->type == R_CLM)
    {
      /* if arg[3] is a constant, go to the explicit cases that are more highly optimized */
      if ((args[3]->constant == R_CONSTANT) &&
	  (args[3]->type == R_INT) &&
	  (prog->ints[args[3]->addr] < 2))
	{
	  int chan;
	  chan = prog->ints[args[3]->addr];
	  args[3]->type = R_CLM;
	  args[3]->addr = args[4]->addr;
	  return(out_opt(prog, args, num_args - 1, chan));
	}

      if ((prog->walk_result == DONT_NEED_RESULT) &&
	  (mus_output_p(prog->clms[args[4]->addr])))   
	return(package(prog, R_FLOAT, out_any_4f, "out_any_4f", args, 4));
      return(package(prog, R_FLOAT, out_any_4, "out_any_4", args, 4));
    }

  if (args[4]->type == R_VCT)
    return(package(prog, R_FLOAT, out_any_vct_4, "out_any_vct_4", args, 4));
  if (args[4]->type == R_SOUND_DATA)
    return(package(prog, R_FLOAT, out_any_sound_data_4, "out_any_sound_data_4", args, 4));
  if (args[4]->type == R_FUNCTION)
    return(package(prog, R_FLOAT, out_any_function_4, "out_any_function_4", args, 4));
  return(package(prog, R_FLOAT, out_any_f_4, "out_any_f_4", args, 4));  
}


static xen_value *out_any_function_body(ptree *prog, s7_pointer proc, xen_value **args, int num_args, const char *funcname)
{
  s7_pointer func_form, pform;
  pform = s7_procedure_source(s7, proc);
  func_form = s7_car(pform);
  s7_unoptimize(s7, func_form);

  if ((s7_is_list(s7, func_form)) &&
      (s7_is_symbol(s7_car(func_form))) &&
      (strncmp("lambda", s7_symbol_name(s7_car(func_form)), 6) == 0))
    {
      bool old_got_lambda;
      xen_value *v;
      old_got_lambda = prog->got_lambda;
      prog->got_lambda = true;
      /* normally when we're plugging in a separate function body the args are already
       *   processed, and we're looking at the name at the start of that list, so we
       *   can pass the current args and let lambda_form use them in lieu of the declare
       *   list to get the arg types.  In this case we might be outside any such call,
       *   (with-sound (:output (lambda ...))), or some of the args might be defaulted (outa),
       *   so we first get the lambda form parsed insisting on a declare (hence the 0 num_args),
       *   then pass in the actual args so that the funcall works.
       */
      v = lambda_form(prog, func_form, true, args, 0, pform); /* must have "declare" here */
      prog->got_lambda = old_got_lambda;
      if (v) 
	{
	  xen_value *result;
	  if (funcname) add_var_to_ptree(prog, funcname, v);
	  result = funcall_n(prog, args, num_args, v);
	  free(v);
	  return(result);
	}
    }
  return(NULL); /* not an error */
}


/* here if num_args == 2 *output* is used by default */

static xen_value *outn_1(ptree *prog, int chan, xen_value **args, int num_args, xen_value *(*out_func)(ptree *prog, xen_value **args, int num_args))
{
  if (num_args == 2)
    {
      xen_value *true_args[4];
      xen_value *rtn;
      int k;
      s7_pointer output;
      bool protect_ptree = false;

      output = mus_clm_output();

      if (mus_xen_p(output))
	true_args[3] = make_xen_value(R_CLM, add_clm_to_ptree(prog, XEN_TO_MUS_ANY(output), scheme_false), R_VARIABLE);
      else
	{
	  if (mus_vct_p(output))
	    true_args[3] = make_xen_value(R_VCT, add_vct_to_ptree(prog, XEN_TO_VCT(output)), R_VARIABLE);
	  else
	    {
	      if (sound_data_p(output))
		true_args[3] = make_xen_value(R_SOUND_DATA, add_sound_data_to_ptree(prog, XEN_TO_SOUND_DATA(output)), R_VARIABLE);
	      else
		{
		  if (s7_is_procedure(output))
		    {
		      xen_value *func_args[5];
		      for (k = 0; k < 3; k++) func_args[k] = args[k];
		      func_args[3] = make_xen_value(R_INT, add_int_to_ptree(prog, chan), R_VARIABLE);
		      true_args[3] = out_any_function_body(prog, output, func_args, 3, NULL);
		      protect_ptree = true;
		      free(func_args[3]);
		      if (true_args[3] == NULL) return(NULL);
		    }
		  else true_args[3] = make_xen_value(R_XEN, add_xen_to_ptree(prog, output), R_VARIABLE);
		}
	    }
	}

      for (k = 0; k < 3; k++) true_args[k] = args[k];
      rtn = out_func(prog, true_args, 3);
      if (!protect_ptree) free(true_args[3]); /* otherwise the embedded ptree is gc'd twice */
      return(rtn);
    }
  return(out_func(prog, args, num_args));
}

static xen_value *outa_1(ptree *prog, xen_value **args, int num_args) 
{
  return(outn_1(prog, 0, args, num_args, outa_2));
}

static xen_value *outb_1(ptree *prog, xen_value **args, int num_args) 
{
  return(outn_1(prog, 1, args, num_args, outb_2));
}

static xen_value *outc_1(ptree *prog, xen_value **args, int num_args) 
{
  return(outn_1(prog, 2, args, num_args, outc_2));
}

static xen_value *outd_1(ptree *prog, xen_value **args, int num_args) 
{
  return(outn_1(prog, 3, args, num_args, outd_2));
}

static xen_value *out_any_1(ptree *prog, xen_value **args, int num_args)
{
  if (num_args == 3)
    {
      xen_value *true_args[5];
      xen_value *rtn;
      int k;
      s7_pointer output;
      bool protect_ptree = false;

      output = mus_clm_output();
      if (mus_xen_p(output))
	true_args[4] = make_xen_value(R_CLM, add_clm_to_ptree(prog, XEN_TO_MUS_ANY(output), scheme_false), R_VARIABLE);
      else
	{
	  if (mus_vct_p(output))
	    true_args[4] = make_xen_value(R_VCT, add_vct_to_ptree(prog, XEN_TO_VCT(output)), R_VARIABLE);
	  else
	    {
	      if (sound_data_p(output))
		true_args[4] = make_xen_value(R_SOUND_DATA, add_sound_data_to_ptree(prog, XEN_TO_SOUND_DATA(output)), R_VARIABLE);
	      else
		{
		  if (s7_is_procedure(output))
		    {
		      for (k = 0; k < 4; k++) true_args[k] = args[k];
		      true_args[4] = out_any_function_body(prog, output, true_args, 3, NULL);
		      if (true_args[4] == NULL) return(NULL);
		      protect_ptree = true;
		    }
		  else true_args[4] = make_xen_value(R_XEN, add_xen_to_ptree(prog, output), R_VARIABLE);
		}
	    }
	}
      for (k = 0; k < 4; k++) true_args[k] = args[k];
      rtn = out_any_2(prog, true_args, 4);
      if (!protect_ptree) free(true_args[4]);
      return(rtn);
    }
  return(out_any_2(prog, args, num_args));
}


/* ---------------- in-any ---------------- */

static void ina_2(int *args, ptree *pt) {FLOAT_RESULT = mus_in_any(INT_ARG_1, 0, CLM_ARG_2);}
static void ina_2f(int *args, ptree *pt) {FLOAT_RESULT = mus_in_any_from_file(CLM_ARG_2, INT_ARG_1, 0);}

static void inb_2(int *args, ptree *pt) {FLOAT_RESULT = mus_in_any(INT_ARG_1, 1, CLM_ARG_2);}
static void inb_2f(int *args, ptree *pt) {FLOAT_RESULT = mus_in_any_from_file(CLM_ARG_2, INT_ARG_1, 1);}

static void in_any_3(int *args, ptree *pt) {FLOAT_RESULT = mus_in_any(INT_ARG_1, INT_ARG_2, CLM_ARG_3);}
static void in_any_3f(int *args, ptree *pt) {FLOAT_RESULT = mus_in_any_from_file(CLM_ARG_3, INT_ARG_1, INT_ARG_2);}


static void in_f_2(int *args, ptree *pt) {FLOAT_RESULT = 0.0;}

static void in_any_f_3(int *args, ptree *pt) {FLOAT_RESULT = 0.0;}


static void in_vct_2(int *args, ptree *pt) 
{
  if ((VCT_ARG_2->length > INT_ARG_1) && (INT_ARG_1 >= 0))
    FLOAT_RESULT = VCT_ARG_2->data[INT_ARG_1];
  else FLOAT_RESULT = 0.0;
}

static void in_any_vct_3(int *args, ptree *pt) 
{
  if ((VCT_ARG_3->length > INT_ARG_1) && (INT_ARG_1 >= 0))
    FLOAT_RESULT = VCT_ARG_3->data[INT_ARG_1];
  else FLOAT_RESULT = 0.0;
}


static void ina_sound_data_2(int *args, ptree *pt) 
{
  if ((SOUND_DATA_ARG_2->length > INT_ARG_1) && (INT_ARG_1 >= 0))
    FLOAT_RESULT = SOUND_DATA_ARG_2->data[0][INT_ARG_1];
  else FLOAT_RESULT = 0.0;
}

static void inb_sound_data_2(int *args, ptree *pt) 
{
  if ((SOUND_DATA_ARG_2->length > INT_ARG_1) && 
      (SOUND_DATA_ARG_2->chans > 1) &&
      (INT_ARG_1 >= 0))
    FLOAT_RESULT = SOUND_DATA_ARG_2->data[1][INT_ARG_1];
  else FLOAT_RESULT = 0.0;
}

static void in_any_sound_data_3(int *args, ptree *pt) 
{
  if ((SOUND_DATA_ARG_3->length > INT_ARG_1) && 
      (SOUND_DATA_ARG_3->chans > INT_ARG_2) &&
      (INT_ARG_1 >= 0))
    FLOAT_RESULT = SOUND_DATA_ARG_3->data[INT_ARG_2][INT_ARG_1];
  else FLOAT_RESULT = 0.0;
}


static mus_float_t call_in_any_function(ptree *pt, Int loc, Int chan, ptree *function)
{
  pt->ints[function->args[0]] = loc;
  pt->ints[function->args[1]] = chan; 
  eval_embedded_ptree(function, pt);
  return(pt->dbls[function->result->addr]);
}

static void ina_function_2(int *args, ptree *pt) 
{
  FLOAT_RESULT = call_in_any_function(pt, INT_ARG_1, 0, FNC_ARG_2);
}

static void inb_function_2(int *args, ptree *pt) 
{
  FLOAT_RESULT = call_in_any_function(pt, INT_ARG_1, 1, FNC_ARG_2);
}

static void in_any_function_3(int *args, ptree *pt) 
{
  FLOAT_RESULT = call_in_any_function(pt, INT_ARG_1, INT_ARG_2, FNC_ARG_3);
}


static xen_value *ina_1(ptree *prog, xen_value **args, int num_args) 
{
  if (args[2]->type == R_CLM)
    {
      if (mus_file_to_sample_p(prog->clms[args[2]->addr]))
	return(package(prog, R_FLOAT, ina_2f, "ina_2f", args, 2));
      return(package(prog, R_FLOAT, ina_2, "ina_2", args, 2));
    }
  if (args[2]->type == R_VCT)
    return(package(prog, R_FLOAT, in_vct_2, "ina_vct_2", args, 2));
  if (args[2]->type == R_SOUND_DATA)
    return(package(prog, R_FLOAT, ina_sound_data_2, "ina_sound_data_2", args, 2));
  if (args[2]->type == R_FUNCTION)
    return(package(prog, R_FLOAT, ina_function_2, "ina_function_2", args, 2));
  return(package(prog, R_FLOAT, in_f_2, "ina_f_2", args, 2));  
}

static xen_value *inb_1(ptree *prog, xen_value **args, int num_args) 
{
  if (args[2]->type == R_CLM)
    {
      if (mus_file_to_sample_p(prog->clms[args[2]->addr]))
	return(package(prog, R_FLOAT, inb_2f, "inb_2f", args, 2));
      return(package(prog, R_FLOAT, inb_2, "inb_2", args, 2));
    }
  if (args[2]->type == R_SOUND_DATA)
    return(package(prog, R_FLOAT, inb_sound_data_2, "inb_sound_data_2", args, 2));
  if (args[2]->type == R_FUNCTION)
    return(package(prog, R_FLOAT, inb_function_2, "inb_function_2", args, 2));
  return(package(prog, R_FLOAT, in_f_2, "inb_f_2", args, 2));  
}

static xen_value *in_any_1(ptree *prog, xen_value **args, int num_args)
{
  if (args[3]->type == R_CLM)
    {
      if (mus_file_to_sample_p(prog->clms[args[3]->addr]))
	return(package(prog, R_FLOAT, in_any_3f, "in_any_3f", args, 3));
      return(package(prog, R_FLOAT, in_any_3, "in_any_3", args, 3));
    }
  if (args[3]->type == R_VCT)
    return(package(prog, R_FLOAT, in_any_vct_3, "in_any_vct_3", args, 3));
  if (args[3]->type == R_SOUND_DATA)
    return(package(prog, R_FLOAT, in_any_sound_data_3, "in_any_sound_data_3", args, 3));
  if (args[3]->type == R_FUNCTION)
    return(package(prog, R_FLOAT, in_any_function_3, "in_any_function_3", args, 3));
  return(package(prog, R_FLOAT, in_any_f_3, "in_any_f_3", args, 3));  
}




/* ---------------- array-interp ---------------- */

static void array_interp_1f(int *args, ptree *pt) 
{
  FLOAT_RESULT = mus_array_interp(VCT_ARG_1->data, FLOAT_ARG_2, VCT_ARG_1->length);
}


static void array_interp_2f(int *args, ptree *pt) {FLOAT_RESULT = mus_array_interp(VCT_ARG_1->data, FLOAT_ARG_2, INT_ARG_3);}

static xen_value *array_interp_1(ptree *prog, xen_value **args, int num_args) 
{
  if (num_args == 2) return(package(prog, R_FLOAT, array_interp_1f, "array_interp_1f", args, 2));
  return(package(prog, R_FLOAT, array_interp_2f, "array_interp_2f", args, 3));
}


/* ---------------- mus-interpolate ---------------- */


static void mus_interpolate_3f(int *args, ptree *pt) 
{
  FLOAT_RESULT = mus_interpolate((mus_interp_t)(INT_ARG_1), FLOAT_ARG_2, VCT_ARG_3->data, VCT_ARG_3->length, 0.0);
}

static void mus_interpolate_4f(int *args, ptree *pt) 
{
  FLOAT_RESULT = mus_interpolate((mus_interp_t)(INT_ARG_1), FLOAT_ARG_2, VCT_ARG_3->data, INT_ARG_4, 0.0);
}

static void mus_interpolate_5f(int *args, ptree *pt) 
{
  FLOAT_RESULT = mus_interpolate((mus_interp_t)(INT_ARG_1), FLOAT_ARG_2, VCT_ARG_3->data, INT_ARG_4, FLOAT_ARG_5);
}

static xen_value *mus_interpolate_1(ptree *prog, xen_value **args, int num_args) 
{
  if (num_args == 3) return(package(prog, R_FLOAT, mus_interpolate_3f, "mus_interpolate_3f", args, 3));
  if (num_args == 4) return(package(prog, R_FLOAT, mus_interpolate_4f, "mus_interpolate_4f", args, 4));
  return(package(prog, R_FLOAT, mus_interpolate_5f, "mus_interpolate_5f", args, 5));
}


/* ---------------- clear_array ---------------- */

static void clear_array_1f(int *args, ptree *pt) 
{
  mus_clear_array(VCT_ARG_1->data, VCT_ARG_1->length);
  VCT_RESULT = VCT_ARG_1;
}

static xen_value *clear_array_1(ptree *prog, xen_value **args, int num_args) 
{
  return(package(prog, R_VCT, clear_array_1f, "clear_array_1f", args, 1));
}


/* ---------------- dot-product etc ---------------- */

#define VCT_2_I(CName, SName) \
static void CName ## _2f(int *args, ptree *pt)  \
{ \
  mus_ ## CName(VCT_ARG_1->data, VCT_ARG_2->data, VCT_ARG_1->length); \
  VCT_RESULT = VCT_ARG_1; \
} \
static void CName ## _3f(int *args, ptree *pt)  \
{ \
  mus_ ## CName(VCT_ARG_1->data, VCT_ARG_2->data, INT_ARG_3); \
  VCT_RESULT = VCT_ARG_1; \
} \
static xen_value * CName ## _1(ptree *prog, xen_value **args, int num_args)  \
{ \
  if (num_args == 2) \
    return(package(prog, R_VCT, CName ## _2f, #CName "_2f", args, 2)); \
  else return(package(prog, R_VCT, CName ## _3f, #CName "_3f", args, 3)); \
}

VCT_2_I(rectangular_to_magnitudes, rectangular->magnitudes)
VCT_2_I(rectangular_to_polar, rectangular->polar)
VCT_2_I(polar_to_rectangular, polar->rectangular)
VCT_2_I(multiply_arrays, multiply-arrays)
VCT_2_I(convolution, convolution)


#define VCT_2_F(CName, SName) \
static void CName ## _2f(int *args, ptree *pt)  \
{ \
  FLOAT_RESULT = mus_ ## CName(VCT_ARG_1->data, VCT_ARG_2->data, VCT_ARG_1->length); \
} \
static void CName ## _3f(int *args, ptree *pt)  \
{ \
  FLOAT_RESULT = mus_ ## CName(VCT_ARG_1->data, VCT_ARG_2->data, INT_ARG_3); \
} \
static xen_value * CName ## _1(ptree *prog, xen_value **args, int num_args)  \
{ \
  if (num_args == 2) \
    return(package(prog, R_FLOAT, CName ## _2f, #CName "_2f", args, 2)); \
  else return(package(prog, R_FLOAT, CName ## _3f, #CName "_3f", args, 3)); \
}

VCT_2_F(dot_product, dot-product)


static void clm_0f(int *args, ptree *pt) {if (CLM_ARG_1) FLOAT_RESULT = MUS_RUN(CLM_ARG_1, 0.0, 0.0);}

static void clm_1f(int *args, ptree *pt) {if (CLM_ARG_1) FLOAT_RESULT = MUS_RUN(CLM_ARG_1, FLOAT_ARG_2, 0.0);}

static void clm_2f(int *args, ptree *pt) {if (CLM_ARG_1) FLOAT_RESULT = MUS_RUN(CLM_ARG_1, FLOAT_ARG_2, FLOAT_ARG_3);}

static xen_value *clm_n(ptree *prog, xen_value **args, int num_args, xen_value *sf)
{
  /* this is handling the gen-as-applicable-func stuff (gen fm) = (oscil gen fm) etc */
  xen_value *temp;
  if (args[0]) free(args[0]);
  args[0] = make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, 0.0), R_VARIABLE);
  if ((num_args > 0) && (args[1]->type == R_INT))
    {
      temp = args[1];
      args[1] = convert_int_to_dbl(prog, args[1]);
      free(temp);
    }
  if ((num_args > 1) && (args[2]->type == R_INT))
    {
      temp = args[2];
      args[2] = convert_int_to_dbl(prog, args[2]);
      free(temp);
    }
  switch (num_args)
    {
    case 0: add_triple_to_ptree(prog, va_make_triple(clm_0f, "clm_0f", 2, args[0], sf)); break;
    case 1: add_triple_to_ptree(prog, va_make_triple(clm_1f, "clm_1f", 3, args[0], sf, args[1])); break;
    case 2: add_triple_to_ptree(prog, va_make_triple(clm_2f, "clm_2f", 4, args[0], sf, args[1], args[2])); break;
    default: if (sf) free(sf); return(NULL); break;
    }
  return(args[0]);
}


static void clm_set_f(int *args, ptree *pt) 
{
  if (CLM_ARG_1)
    {
      if (mus_frame_p(CLM_ARG_1))
	mus_frame_set(CLM_ARG_1, INT_ARG_2, FLOAT_ARG_4);
      else
	{
	  if (mus_mixer_p(CLM_ARG_1))
	    mus_mixer_set(CLM_ARG_1, INT_ARG_2, INT_ARG_3, FLOAT_ARG_4);
	}
    }
  /* FLOAT_RESULT = FLOAT_ARG_4; */
}

static void clm_set_1(ptree *prog, xen_value *in_v, xen_value *in_v1, xen_value *in_v2, xen_value *v)
{
  xen_var *var;
  var = find_var_in_ptree_via_addr(prog, in_v->type, in_v->addr);
  if (var) var->unclean = true;
  /* v->type known to be R_FLOAT (generalized set insists) */
  add_triple_to_ptree(prog, va_make_triple(clm_set_f, "clm_set_f", 5, NULL, in_v, in_v1, in_v2, v));
}





/* ---------------- spectrum ---------------- */

static void mus_spectrum_3v_f(int *args, ptree *pt) 
{
  mus_spectrum(VCT_ARG_1->data, VCT_ARG_2->data, NULL, VCT_ARG_1->length, MUS_SPECTRUM_NORMALIZED);
  VCT_RESULT = VCT_ARG_1;
}


static void mus_spectrum_3v(int *args, ptree *pt) 
{
  mus_spectrum(VCT_ARG_1->data, VCT_ARG_2->data, (VCT_ARG_3) ? (VCT_ARG_3->data) : NULL, VCT_ARG_1->length, MUS_SPECTRUM_NORMALIZED);
  VCT_RESULT = VCT_ARG_1;
}


static void mus_spectrum_4v(int *args, ptree *pt) 
{
  mus_spectrum(VCT_ARG_1->data, VCT_ARG_2->data, (VCT_ARG_3) ? (VCT_ARG_3->data) : NULL, VCT_ARG_1->length, (mus_spectrum_t)INT_ARG_4);
  VCT_RESULT = VCT_ARG_1;
}


static void mus_spectrum_4v_f(int *args, ptree *pt) 
{
  mus_spectrum(VCT_ARG_1->data, VCT_ARG_2->data, NULL, VCT_ARG_1->length, (mus_spectrum_t)INT_ARG_4);
  VCT_RESULT = VCT_ARG_1;
}


static xen_value *mus_spectrum_1(ptree *prog, xen_value **args, int num_args) 
{
  if (num_args == 3) 
    {
      if (args[3]->type == R_VCT)
	return(package(prog, R_VCT, mus_spectrum_3v, "mus_spectrum_3v", args, 3));
      return(package(prog, R_VCT, mus_spectrum_3v_f, "mus_spectrum_3v_f", args, 3));
    }
  if (args[3]->type == R_VCT)
    return(package(prog, R_VCT, mus_spectrum_4v, "mus_spectrum_4v", args, 4));
  return(package(prog, R_VCT, mus_spectrum_4v_f, "mus_spectrum_4v_f", args, 4));
}


/* ---------------- partials->polynomial ---------------- */

/* (partials->polynomial amps kind) which doesn't match the C level mus_partials_to_polynomial(int npartials, mus_float_t *partials, mus_polynomial_t kind)
 */

static void partials_to_polynomial_n(int *args, ptree *pt, mus_polynomial_t kind) 
{
  int npartials = 0, error = 0;
  mus_float_t *partials = NULL;
  vct *v;
  if (VCT_RESULT) mus_vct_free(VCT_RESULT);
  partials = mus_vct_to_partials(VCT_ARG_1, &npartials, &error);
  mus_partials_to_polynomial(npartials, partials, kind);
  v = (vct *)malloc(sizeof(vct));
  v->length = npartials;
  v->data = partials;
  v->dont_free = false;
  VCT_RESULT = v;
}

static void partials_to_polynomial_no_kind(int *args, ptree *pt)
{
  partials_to_polynomial_n(args, pt, MUS_CHEBYSHEV_FIRST_KIND);
}

static void partials_to_polynomial_with_kind(int *args, ptree *pt)
{
  partials_to_polynomial_n(args, pt, (mus_polynomial_t)INT_ARG_2);
}


static xen_value *partials_to_polynomial_1(ptree *prog, xen_value **args, int num_args) 
{
  args[0] = make_xen_value(R_VCT, add_vct_to_ptree(prog, NULL), R_VARIABLE);
  add_obj_to_gcs(prog, R_VCT, args[0]->addr);
  if (num_args == 1)
    add_triple_to_ptree(prog, va_make_triple(partials_to_polynomial_no_kind, "partials_to_polynomial_0", 2, args[0], args[1]));
  else add_triple_to_ptree(prog, va_make_triple(partials_to_polynomial_with_kind, "partials_to_polynomial_1", 3, args[0], args[1], args[2]));
  return(args[0]);
}



/* ---------------- normalize-partials ---------------- */

static void normalize_partials_0(int *args, ptree *pt) 
{
  vct *v;
  v = VCT_ARG_1;
  mus_normalize_partials(v->length / 2, v->data);
  VCT_RESULT = v;
}


static xen_value *normalize_partials_1(ptree *prog, xen_value **args, int num_args)
{
  return(package(prog, R_VCT, normalize_partials_0, "normalize_partials_0", args, 1));
}



/* ---------------- mus-chebyshev-tu-sum ---------------- */

static void mus_chebyshev_tu_sum_0(int *args, ptree *pt) 
{
  FLOAT_RESULT = mus_chebyshev_tu_sum(FLOAT_ARG_1, VCT_ARG_2->length, VCT_ARG_2->data, VCT_ARG_3->data);
}


static xen_value *mus_chebyshev_tu_sum_1(ptree *prog, xen_value **args, int num_args)
{
  return(package(prog, R_FLOAT, mus_chebyshev_tu_sum_0, "mus-chebyshev-tu-sum-0", args, 3));
}


static void mus_chebyshev_t_sum_0(int *args, ptree *pt) 
{
  FLOAT_RESULT = mus_chebyshev_t_sum(FLOAT_ARG_1, VCT_ARG_2->length, VCT_ARG_2->data);
}


static xen_value *mus_chebyshev_t_sum_1(ptree *prog, xen_value **args, int num_args)
{
  return(package(prog, R_FLOAT, mus_chebyshev_t_sum_0, "mus-chebyshev-t-sum-0", args, 2));
}


static void mus_chebyshev_u_sum_0(int *args, ptree *pt) 
{
  FLOAT_RESULT = mus_chebyshev_u_sum(FLOAT_ARG_1, VCT_ARG_2->length, VCT_ARG_2->data);
}


static xen_value *mus_chebyshev_u_sum_1(ptree *prog, xen_value **args, int num_args)
{
  return(package(prog, R_FLOAT, mus_chebyshev_u_sum_0, "mus-chebyshev-u-sum-0", args, 2));
}



/* ---------------- src ---------------- */

GEN_P(src)
GEN_P(convolve)
GEN_P(granulate)
GEN_P(phase_vocoder)

static mus_float_t src_input(void *arg, int direction)
{
  mus_xen *gn = (mus_xen *)arg;
  ptree *pt, *outer;
  pt = mus_xen_input(gn);
  outer = pt->outer_tree;
  outer->ints[pt->args[0]] = direction;
  eval_embedded_ptree(pt, outer);
  return(outer->dbls[pt->result->addr]);
}


static void src_2f(int *args, ptree *pt) 
{
  mus_xen_set_input((mus_xen *)mus_environ(CLM_ARG_1), FNC_ARG_3);
  FLOAT_RESULT = mus_src(CLM_ARG_1, FLOAT_ARG_2, src_input);
}


static void src_1f(int *args, ptree *pt) {FLOAT_RESULT = mus_src(CLM_ARG_1, FLOAT_ARG_2, NULL);}


static void src_0f(int *args, ptree *pt) {FLOAT_RESULT = mus_src(CLM_ARG_1, 0.0, NULL);}

static xen_value *src_1(ptree *prog, xen_value **args, int num_args) 
{
  /* input function arg may be #f */
  if ((num_args > 1) && (args[2]->type == R_INT)) single_to_float(prog, args, 2);
  if (num_args == 3)
    {
      if (args[3]->type == R_FUNCTION)
	{
	  ptree *pt;
	  pt = ((ptree **)(prog->fncs))[args[3]->addr];
	  if (pt->arity != 1) return(run_warn("src: wrong number of args to input function"));
	}
      else num_args = 2;
    }
  switch (num_args)
    {
    case 1: return(package(prog, R_FLOAT, src_0f, "src_0f", args, 1)); break;
    case 2: return(package(prog, R_FLOAT, src_1f, "src_1f", args, 2)); break;
    default: return(package(prog, R_FLOAT, src_2f, "src_2f", args, 3)); break;
    }
}


/* ---------------- convolve ---------------- */


static void convolve_1f(int *args, ptree *pt)
{
  mus_xen_set_input((mus_xen *)mus_environ(CLM_ARG_1), FNC_ARG_2);
  FLOAT_RESULT = mus_convolve(CLM_ARG_1, src_input);
}


static void convolve_0f(int *args, ptree *pt) {FLOAT_RESULT = mus_convolve(CLM_ARG_1, NULL);}

static xen_value *convolve_1(ptree *prog, xen_value **args, int num_args)
{
  if (num_args == 2)
    {
      if (args[2]->type == R_FUNCTION)
	{
	  ptree *pt;
	  pt = ((ptree **)(prog->fncs))[args[2]->addr];
	  if (pt->arity != 1) return(run_warn("convolve: wrong number of args to input function"));
	}
      else num_args = 1;
    }
  if (num_args == 1)
    return(package(prog, R_FLOAT, convolve_0f, "convolve_0f", args, 1));
  return(package(prog, R_FLOAT, convolve_1f, "convolve_1f", args, 2));
}


/* ---------------- granulate ---------------- */

static int grn_edit(void *arg)
{
  mus_xen *gn = (mus_xen *)arg;
  ptree *pt, *outer;
  pt = mus_xen_edit(gn);
  outer = pt->outer_tree;
  outer->clms[pt->args[0]] = mus_xen_gen(gn);
  eval_embedded_ptree(pt, outer);
  return(outer->ints[pt->result->addr]);
}


static void granulate_2f(int *args, ptree *pt)
{
  mus_xen *gn = (mus_xen *)mus_environ(CLM_ARG_1);
  mus_xen_set_input(gn, FNC_ARG_2);
  mus_xen_set_edit(gn, FNC_ARG_3);
  FLOAT_RESULT = mus_granulate_with_editor(CLM_ARG_1, src_input, grn_edit);
}


static void granulate_2f_split(int *args, ptree *pt)
{
  mus_xen *gn = (mus_xen *)mus_environ(CLM_ARG_1);
  mus_xen_set_edit(gn, FNC_ARG_3);
  FLOAT_RESULT = mus_granulate_with_editor(CLM_ARG_1, NULL, grn_edit);
}


static void granulate_1f(int *args, ptree *pt)
{
  mus_xen_set_input((mus_xen *)mus_environ(CLM_ARG_1), FNC_ARG_2);
  FLOAT_RESULT = mus_granulate_with_editor(CLM_ARG_1, src_input, NULL);
}


static void granulate_0f(int *args, ptree *pt) {FLOAT_RESULT = mus_granulate_with_editor(CLM_ARG_1, NULL, NULL);}
static void granulate_0f_mult(int *args, ptree *pt) {FLOAT_RESULT = FLOAT_ARG_2 * mus_granulate_with_editor(CLM_ARG_1, NULL, NULL);}
static void granulate_0f_env(int *args, ptree *pt) {FLOAT_RESULT = mus_env_linear(CLM_ARG_2) * mus_granulate_with_editor(CLM_ARG_1, NULL, NULL);}


static xen_value *granulate_1(ptree *prog, xen_value **args, int num_args)
{
  bool got_input = false;

  if (num_args >= 2)
    {
      if (num_args == 3)
	{
	  if (args[3]->type == R_FUNCTION)
	    {
	      ptree *pt;
	      pt = ((ptree **)(prog->fncs))[args[3]->addr];
	      if (pt->arity != 1) return(run_warn("granulate: wrong number of args to edit function"));
	    }
	  else num_args = 2;
	}

      if (args[2]->type == R_FUNCTION)
	{
	  ptree *pt;
	  pt = ((ptree **)(prog->fncs))[args[2]->addr];
	  if (pt->arity != 1) return(run_warn("granulate: wrong number of args to input function"));
	  got_input = true;
	}
      else
	{
	  if (num_args == 2) 
	    num_args = 1;
	}
    }

  if (num_args == 1)
    return(package(prog, R_FLOAT, granulate_0f, "granulate_0f", args, 1));

  if (num_args == 2)
    return(package(prog, R_FLOAT, granulate_1f, "granulate_1f", args, 2));

  if (got_input)
    return(package(prog, R_FLOAT, granulate_2f, "granulate_2f", args, 3));
  return(package(prog, R_FLOAT, granulate_2f_split, "granulate_2f_split", args, 3));
}



/* ---------------- phase-vocoder ---------------- */

static bool pv_analyze(void *arg, mus_float_t (*input)(void *arg1, int direction))
{
  mus_xen *gn = (mus_xen *)arg;
  ptree *pt, *outer;
  pt = mus_xen_analyze(gn);
  outer = pt->outer_tree;
  outer->clms[pt->args[0]] = mus_xen_gen(gn);
  /* I think the input function is handled by mus_phase_vocoder */
  eval_embedded_ptree(pt, outer);
  return(outer->ints[pt->result->addr]);
}

static mus_float_t pv_synthesize(void *arg)
{
  mus_xen *gn = (mus_xen *)arg;
  ptree *pt, *outer;
  pt = mus_xen_synthesize(gn);
  outer = pt->outer_tree;
  outer->clms[pt->args[0]] = mus_xen_gen(gn);
  eval_embedded_ptree(pt, outer);
  return(outer->dbls[pt->result->addr]);
}


static void phase_vocoder_5f(int *args, ptree *pt)
{
  /* in-coming arg1 is descr of which funcs are active -- all arg ctrs are shifted over one to make room */
  mus_xen *gn = (mus_xen *)mus_environ(CLM_ARG_2);

  if (INT_ARG_1 & 1) mus_xen_set_input(gn, FNC_ARG_3);
  if (INT_ARG_1 & 2) mus_xen_set_analyze(gn, FNC_ARG_4);
  if (INT_ARG_1 & 4) mus_xen_set_edit(gn, FNC_ARG_5);
  if (INT_ARG_1 & 8) mus_xen_set_synthesize(gn, FNC_ARG_6);
  FLOAT_RESULT = mus_phase_vocoder_with_editors(CLM_ARG_2, 
						(INT_ARG_1 & 1) ? src_input : NULL, 
						(INT_ARG_1 & 2) ? pv_analyze : NULL, 
						(INT_ARG_1 & 4) ? grn_edit : NULL, 
						(INT_ARG_1 & 8) ? pv_synthesize : NULL);
}


static void phase_vocoder_1f(int *args, ptree *pt)
{
  mus_xen_set_input((mus_xen *)mus_environ(CLM_ARG_1), FNC_ARG_2);
  FLOAT_RESULT = mus_phase_vocoder(CLM_ARG_1, src_input);
}


static void phase_vocoder_0f(int *args, ptree *pt) 
{
  FLOAT_RESULT = mus_phase_vocoder(CLM_ARG_1, NULL);
}


static xen_value *phase_vocoder_1(ptree *prog, xen_value **args, int num_args)
{
  bool got_input = false, got_edit = false, got_analyze = false, got_synthesize = false;

  if (num_args == 5)
    {
      if (args[5]->type == R_FUNCTION)
	{
	  ptree *pt;
	  pt = ((ptree **)(prog->fncs))[args[5]->addr];
	  if (pt->arity != 1) return(run_warn("phase-vocoder: wrong number of args to synthesize function"));
	  got_synthesize = true;
	}
      else num_args = 4;
    }

  if (num_args >= 4)
    {
      if (args[4]->type == R_FUNCTION)
	{
	  ptree *pt;
	  pt = ((ptree **)(prog->fncs))[args[4]->addr];
	  if (pt->arity != 1) return(run_warn("phase-vocoder: wrong number of args to edit function"));
	  got_edit = true;
	}
      else 
	{
	  if (num_args == 4)
	    num_args = 3;
	}
    }

  if (num_args >= 3)
    {
      if (args[3]->type == R_FUNCTION)
	{
	  ptree *pt;
	  pt = ((ptree **)(prog->fncs))[args[3]->addr];
	  if (pt->arity != 2) return(run_warn("phase-vocoder: wrong number of args to analyze function"));
	  got_analyze = true;
	}
      else 
	{
	  if (num_args == 3)
	    num_args = 2;
	}
    }

  if (num_args >= 2)
    {
      if (args[2]->type == R_FUNCTION)
	{
	  ptree *pt;
	  pt = ((ptree **)(prog->fncs))[args[2]->addr];
	  if (pt->arity != 1) return(run_warn("phase-vocoder: wrong number of args to input function"));
	  got_input = true;
	}
      else 
	{
	  if (num_args == 2)
	    num_args = 1;
	}
    }

  if (num_args == 1)
    return(package(prog, R_FLOAT, phase_vocoder_0f, "phase_vocoder_0f", args, 1));

  if (num_args == 2)
    return(package(prog, R_FLOAT, phase_vocoder_1f, "phase_vocoder_1f", args, 2));

  /* rather than make 16 different callers, I'll pass along which args are functions in an inserted arg1 */
  {
    int i;
    xen_value **new_args;
    new_args = (xen_value **)calloc(num_args + 2, sizeof(xen_value *));
    for (i = 1; i <= num_args; i++)
      new_args[i + 1] = args[i];
    new_args[1] = make_xen_value(R_INT, 
				 add_int_to_ptree(prog, 
						  ((got_input) ? 1 : 0) +
						  ((got_analyze) ? 2: 0) + 
						  ((got_edit) ? 4 : 0) + 
						  ((got_synthesize) ? 8 : 0)), 
				 R_CONSTANT);
    new_args[0] = add_temporary_var_to_ptree(prog, R_FLOAT);
    args[0] = new_args[0];
    add_triple_to_ptree(prog, make_triple(phase_vocoder_5f, "phase_vocoder_5f", new_args, num_args + 2));
    free(new_args[1]);
    free(new_args);
    return(args[0]);
  }
}


#define PV_VCT_1(Name) \
static void pv_ ## Name ## _1(int *args, ptree *pt) \
{ \
  if (!VCT_RESULT) \
    { \
      s7_pointer res; \
      res = xen_make_vct_wrapper(mus_length(CLM_ARG_1), mus_phase_vocoder_ ## Name (CLM_ARG_1)); \
      add_loc_to_protected_list(pt, s7_gc_protect(s7, res)); \
      VCT_RESULT = xen_to_vct(res); \
    } \
} \
static xen_value *phase_vocoder_ ## Name ## _1(ptree *prog, xen_value **args, int num_args) \
{ \
  if (args[1]->type == R_CLM) \
    return(package(prog, R_VCT, pv_ ## Name ## _1, "pv_" #Name "_1", args, 1)); \
  return(run_warn("wrong type arg (%s) to mus_phase_vocoder_ " #Name , type_name(args[1]->type))); \
}


PV_VCT_1(amps)
PV_VCT_1(amp_increments)
PV_VCT_1(freqs)
PV_VCT_1(phases)
PV_VCT_1(phase_increments)




/* ---------------- contrast_enhancement ---------------- */

static void contrast_enhancement_f(int *args, ptree *pt) {FLOAT_RESULT = mus_contrast_enhancement(FLOAT_ARG_1, FLOAT_ARG_2);}


static xen_value *contrast_enhancement_1(ptree *prog, xen_value **args, int num_args)
{
  if (args[1]->type == R_INT) single_to_float(prog, args, 1);
  if (args[2]->type == R_INT) single_to_float(prog, args, 2);
  return(package(prog, R_FLOAT, contrast_enhancement_f, "contrast_enhancement_f", args, 2));
}


/* ---------------- ring_modulate ---------------- */

static void ring_modulate_f(int *args, ptree *pt) {FLOAT_RESULT = mus_ring_modulate(FLOAT_ARG_1, FLOAT_ARG_2);}


static xen_value *ring_modulate_1(ptree *prog, xen_value **args, int num_args)
{
  if (args[1]->type == R_INT) single_to_float(prog, args, 1);
  if (args[2]->type == R_INT) single_to_float(prog, args, 2);
  return(package(prog, R_FLOAT, ring_modulate_f, "ring_modulate_f", args, 2));
}


/* ---------------- amplitude_modulate ---------------- */

static void amplitude_modulate_f(int *args, ptree *pt) {FLOAT_RESULT = mus_amplitude_modulate(FLOAT_ARG_1, FLOAT_ARG_2, FLOAT_ARG_3);}


static xen_value *amplitude_modulate_1(ptree *prog, xen_value **args, int num_args)
{
  if (args[1]->type == R_INT) single_to_float(prog, args, 1);
  if (args[2]->type == R_INT) single_to_float(prog, args, 2);
  if (args[3]->type == R_INT) single_to_float(prog, args, 3);
  return(package(prog, R_FLOAT, amplitude_modulate_f, "amplitude_modulate_f", args, 3));
}


#define SND_STR_OP(CName) \
static void CName ## _f(int *args, ptree *pt) {INT_RESULT = (Int)CName(STRING_ARG_1);} \
static xen_value * CName ## _1(ptree *prog, xen_value **args, int num_args) \
{ \
  return(package(prog, R_INT, CName ## _f, #CName "_f", args, 1)); \
}

SND_STR_OP(mus_sound_samples)
SND_STR_OP(mus_sound_frames)
SND_STR_OP(mus_sound_datum_size)
SND_STR_OP(mus_sound_data_location)
SND_STR_OP(mus_sound_chans)
SND_STR_OP(mus_sound_srate)
SND_STR_OP(mus_sound_header_type)
SND_STR_OP(mus_sound_data_format)
SND_STR_OP(mus_sound_length)
SND_STR_OP(mus_sound_type_specifier)
SND_STR_OP(mus_sound_forget)


static void mus_sound_duration_f(int *args, ptree *pt) {FLOAT_RESULT = mus_sound_duration(STRING_ARG_1);}


static xen_value *mus_sound_duration_1(ptree *prog, xen_value **args, int num_args)
{
  return(package(prog, R_FLOAT, mus_sound_duration_f, "mus_sound_duration_f", args, 1));
}


static void mus_sound_comment_f(int *args, ptree *pt) 
{
  if (STRING_RESULT) free(STRING_RESULT);
  STRING_RESULT = mus_sound_comment(STRING_ARG_1);
}


static xen_value *mus_sound_comment_1(ptree *prog, xen_value **args, int num_args)
{
  return(package(prog, R_STRING, mus_sound_comment_f, "mus_sound_comment_f", args, 1));
}


static void mus_header_type_name_f(int *args, ptree *pt) 
{
  if (STRING_RESULT) free(STRING_RESULT);
  STRING_RESULT = mus_strdup(mus_header_type_name(INT_ARG_1));
}


static xen_value *mus_header_type_name_1(ptree *prog, xen_value **args, int num_args)
{
  return(package(prog, R_STRING, mus_header_type_name_f, "mus_header_type_name_f", args, 1));
}


static void mus_data_format_name_f(int *args, ptree *pt) 
{
  if (STRING_RESULT) free(STRING_RESULT);
  STRING_RESULT = mus_strdup(mus_data_format_name(INT_ARG_1));
}


static xen_value *mus_data_format_name_1(ptree *prog, xen_value **args, int num_args)
{
  return(package(prog, R_STRING, mus_data_format_name_f, "mus_data_format_name_f", args, 1));
}


static void mus_header_type_to_string_f(int *args, ptree *pt) 
{
  if (STRING_RESULT) free(STRING_RESULT);
  STRING_RESULT = mus_strdup(mus_header_type_to_string(INT_ARG_1));
}


static xen_value *mus_header_type_to_string_1(ptree *prog, xen_value **args, int num_args)
{
  return(package(prog, R_STRING, mus_header_type_to_string_f, "mus_header_type_to_string_f", args, 1));
}


static void mus_data_format_to_string_f(int *args, ptree *pt) 
{
  if (STRING_RESULT) free(STRING_RESULT);
  STRING_RESULT = mus_strdup(mus_data_format_to_string(INT_ARG_1));
}


static xen_value *mus_data_format_to_string_1(ptree *prog, xen_value **args, int num_args)
{
  return(package(prog, R_STRING, mus_data_format_to_string_f, "mus_data_format_to_string_f", args, 1));
}


/* ---------------- formant-bank ---------------- */


static void formant_bank_f(int *args, ptree *pt) 
{
  FLOAT_RESULT = mus_formant_bank(VCT_ARG_1->data, 
				  VECT_ARG_2->data.gens, 
				  FLOAT_ARG_3, 
				  VCT_ARG_1->length);
}

static xen_value *formant_bank_1(ptree *prog, xen_value **args, int num_args) 
{
  if (args[3]->type == R_INT) single_to_float(prog, args, 3);
  return(package(prog, R_FLOAT, formant_bank_f, "formant_bank_f", args, 3));
}


/* ---------------- mus-srate ---------------- */

static xen_value *mus_srate_1(ptree *prog, xen_value **args, int num_args) 
{
  return(make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, mus_srate()), R_CONSTANT));
}




static xen_value *boolean_p_1(ptree *prog, xen_value **args, int num_args)
{
  return(make_xen_value(R_BOOL, add_int_to_ptree(prog, args[1]->type == R_BOOL), R_CONSTANT));
}


static xen_value *number_p_1(ptree *prog, xen_value **args, int num_args)
{
  return(make_xen_value(R_BOOL, add_int_to_ptree(prog, (args[1]->type == R_INT) || (args[1]->type == R_FLOAT)), R_CONSTANT));
}


static xen_value *integer_p_1(ptree *prog, xen_value **args, int num_args)
{
  return(make_xen_value(R_BOOL, add_int_to_ptree(prog, args[1]->type == R_INT), R_CONSTANT));
}


static xen_value *inexact_p_1(ptree *prog, xen_value **args, int num_args)
{
  return(make_xen_value(R_BOOL, add_int_to_ptree(prog, args[1]->type == R_FLOAT), R_CONSTANT));
}


static xen_value *exact_p_1(ptree *prog, xen_value **args, int num_args)
{
  return(make_xen_value(R_BOOL, add_int_to_ptree(prog, args[1]->type == R_INT), R_CONSTANT));
}


static xen_value *real_p_1(ptree *prog, xen_value **args, int num_args)
{
  return(make_xen_value(R_BOOL, add_int_to_ptree(prog, (args[1]->type == R_INT) || (args[1]->type == R_FLOAT)), R_CONSTANT));
}


static xen_value *char_p_1(ptree *prog, xen_value **args, int num_args)
{
  return(make_xen_value(R_BOOL, add_int_to_ptree(prog, args[1]->type == R_CHAR), R_CONSTANT));
}


static xen_value *string_p_1(ptree *prog, xen_value **args, int num_args)
{
  return(make_xen_value(R_BOOL, add_int_to_ptree(prog, args[1]->type == R_STRING), R_CONSTANT));
}

#if USE_SND
static xen_value *sampler_p_1(ptree *prog, xen_value **args, int num_args)
{
  return(make_xen_value(R_BOOL, add_int_to_ptree(prog, args[1]->type == R_SAMPLER), R_CONSTANT));
}
#endif


static xen_value *keyword_p_1(ptree *prog, xen_value **args, int num_args)
{
  return(make_xen_value(R_BOOL, add_int_to_ptree(prog, args[1]->type == R_KEYWORD), R_CONSTANT));
}


static xen_value *symbol_p_1(ptree *prog, xen_value **args, int num_args)
{
  return(make_xen_value(R_BOOL, add_int_to_ptree(prog, args[1]->type == R_SYMBOL), R_CONSTANT));
}


static xen_value *vct_p_1(ptree *prog, xen_value **args, int num_args)
{
  return(make_xen_value(R_BOOL, add_int_to_ptree(prog, args[1]->type == R_VCT), R_CONSTANT));
}


static xen_value *sound_data_p_1(ptree *prog, xen_value **args, int num_args)
{
  return(make_xen_value(R_BOOL, add_int_to_ptree(prog, args[1]->type == R_SOUND_DATA), R_CONSTANT));
}


static xen_value *vector_p_1(ptree *prog, xen_value **args, int num_args)
{
  return(make_xen_value(R_BOOL, add_int_to_ptree(prog, VECTOR_P(args[1]->type)), R_CONSTANT));
}


static xen_value *list_p_1(ptree *prog, xen_value **args, int num_args)
{
  return(make_xen_value(R_BOOL, add_int_to_ptree(prog, args[1]->type == R_LIST), R_CONSTANT));
}


static xen_value *char_ge_1(ptree *prog, xen_value **args, int num_args) 
{
  return(greater_than_or_equal(prog, false, args, num_args));
}


static xen_value *char_gt_1(ptree *prog, xen_value **args, int num_args) 
{
  return(greater_than(prog, false, args, num_args));
}


static xen_value *char_le_1(ptree *prog, xen_value **args, int num_args) 
{
  return(less_than_or_equal(prog, false, args, num_args));
}


static xen_value *char_lt_1(ptree *prog, xen_value **args, int num_args) 
{
  return(less_than(prog, false, args, num_args));
}


static xen_value *char_eq_1(ptree *prog, xen_value **args, int num_args) 
{
  return(numbers_equal(prog, false, args, num_args));
}


static xen_value *char_ci_ge_1(ptree *prog, xen_value **args, int num_args) 
{
  return(greater_than_or_equal(prog, false, upcase_args(prog, args, num_args), num_args));
}


static xen_value *char_ci_gt_1(ptree *prog, xen_value **args, int num_args) 
{
  return(greater_than(prog, false, upcase_args(prog, args, num_args), num_args));
}


static xen_value *char_ci_le_1(ptree *prog, xen_value **args, int num_args) 
{
  return(less_than_or_equal(prog, false, upcase_args(prog, args, num_args), num_args));
}


static xen_value *char_ci_lt_1(ptree *prog, xen_value **args, int num_args) 
{
  return(less_than(prog, false, upcase_args(prog, args, num_args), num_args));
}


static xen_value *char_ci_eq_1(ptree *prog, xen_value **args, int num_args) 
{
  return(numbers_equal(prog, false, upcase_args(prog, args, num_args), num_args));
}


static xen_value *ge_1(ptree *prog, xen_value **args, int num_args) 
{
  return(greater_than_or_equal(prog, prog->float_result, args, num_args));
}


static xen_value *gt_1(ptree *prog, xen_value **args, int num_args) 
{
  return(greater_than(prog, prog->float_result, args, num_args));
}


static xen_value *le_1(ptree *prog, xen_value **args, int num_args) 
{
  return(less_than_or_equal(prog, prog->float_result, args, num_args));
}


static xen_value *lt_1(ptree *prog, xen_value **args, int num_args) 
{
  return(less_than(prog, prog->float_result, args, num_args));
}


static xen_value *eq_1(ptree *prog, xen_value **args, int num_args) 
{
  return(numbers_equal(prog, prog->float_result, args, num_args));
}


static xen_value *atanx_1(ptree *prog, xen_value **args, int num_args) 
{
  if (num_args == 1)
    return(atan_1(prog, args, num_args));
  return(atan2_1(prog, args, num_args));
}


static xen_value *integer_to_char_1(ptree *prog, xen_value **args, int num_args) 
{
  xen_value *newv;
  newv = copy_xen_value(args[1]);
  newv->type = R_CHAR;
  return(newv);
}


static xen_value *declare_1(ptree *prog, s7_pointer form, walk_result_t ignore) 
{
  return(make_xen_value(R_UNSPECIFIED, -1, R_VARIABLE));
}

static xen_value *lambda_preform(ptree *prog, s7_pointer form, walk_result_t ignore) 
{
  return(lambda_form(prog, form, true, NULL, 0, scheme_false));
}



/* clm-print and xen args support */

static bool xenable(xen_value *v) /* see xen_value_to_xen */
{
  switch (v->type)
    {
    case R_FLOAT: case R_INT: case R_CHAR: case R_STRING: case R_BOOL: case R_XEN:
    case R_FLOAT_VECTOR: case R_VCT: case R_SOUND_DATA: 
    case R_INT_VECTOR:
    case R_KEYWORD: case R_SYMBOL:
    case R_CLM: case R_LIST: 
      return(true);
      break;
    default:
      return(CLM_STRUCT_P(v->type));
      break;
    }
  return(false);
}


static s7_pointer wrap_generator(ptree *pt, int addr)
{
  mus_any *gen;
  gen = pt->clms[addr];

#if USE_SND
  if (pt->clm_locs[addr] >= 0)
    {
      s7_pointer val;
      val = snd_protected_at(pt->clm_locs[addr]);  /* try to use the original s7_pointer value holding this generator */
      if ((mus_xen_p(val)) &&
	  (gen == XEN_TO_MUS_ANY(val)))
	return(val);
    }
#else
  if (pt->clm_locs[addr] >= 0)
    {
      s7_pointer val;
      val = s7_gc_protected_at(s7, pt->clm_locs[addr]);
      if ((mus_xen_p(val)) &&
	  (gen == XEN_TO_MUS_ANY(val)))
	return(val);
    }
#endif

  /* desperate fallback */
  {
    mus_xen *gn;
    gn = mus_any_to_mus_xen(gen);
    mus_xen_set_dont_free(gn, true);
    return(mus_xen_to_object(gn));
  }
}


static s7_pointer xen_value_to_xen(ptree *pt, xen_value *v)
{
  s7_pointer val = scheme_false;

  /* fprintf(stderr, "xen_value_to_xen: %s %s\n", type_name(v->type), describe_xen_value(v, pt)); */

  switch (v->type)
    {
    case R_FLOAT:   return(s7_make_real(s7, pt->dbls[v->addr]));       break;
    case R_INT:     return(s7_make_integer(s7, pt->ints[v->addr]));        break;
    case R_CHAR:    return(s7_make_character(s7, (char)(pt->ints[v->addr]))); break;
    case R_STRING:  return(scheme_make_string(pt->strs[v->addr]));       break;
    case R_BOOL:    return(s7_make_boolean(s7, pt->ints[v->addr]));      break;
    case R_XEN:     return(pt->xens[v->addr]);                        break;

    case R_SYMBOL:
    case R_KEYWORD: return(pt->xens[v->addr]);                        break;

    case R_FLOAT_VECTOR:
    case R_VCT:
      if ((pt->vcts) && (pt->vcts[v->addr]))
	{
	  vct *vtemp;
	  /* it is necessary to copy the data here to protect it from mus_run_free_ptree:
	   *
	   * (defgenerator hiho2 (v #f :type vct))
	   * (define val (make-hiho2))
	   * (run (lambda () (set! (hiho2-v val) (make-vct 32 .1))))
	   */
	  vtemp = mus_vct_copy(pt->vcts[v->addr]);
	  val = xen_make_vct(vtemp->length, vtemp->data);
	  free(vtemp);
	}
      break;

    case R_INT_VECTOR:
      if (pt->vects[v->addr])
	{
	  /* (let ((hi (vector 1 2 3))) (run (lambda () (vector-set! hi 2 4) hi))) */
	  /* (run (lambda () (let ((v (make-vector 3 0))) (vector-set! v 1 2) v))) */
	  val = s7_make_and_fill_vector(s7, pt->vects[v->addr]->length, scheme_false);
	  int_vect_into_vector(pt->vects[v->addr], val);
	}
      break;

      /* if xen_value_to_xen for R_CLM|VCT|LIST_VECTOR SAMPLER also in free_xen_var */
      /*  clm_vector data.gens=mus_any direct from scheme obj: is it safe to simply repackage? */

    case R_SOUND_DATA:
      if ((pt->sds) && (pt->sds[v->addr]))
	{
	  sound_data *sd;
	  sd = sound_data_copy(pt->sds[v->addr]);
	  val = wrap_sound_data(sd->chans, sd->length, sd->data);
	  free(sd);
	  sd = XEN_TO_SOUND_DATA(val);
	  sd->wrapped = false;
	}
      break;

    case R_CLM:
      if ((pt->clms) && (pt->clms[v->addr]))
	return(wrap_generator(pt, v->addr)); /* here a mus_copy might be safer */
      break;

    default:
      if ((pt->lists) &&
	  ((v->type == R_LIST) || 
	   (CLM_STRUCT_P(v->type))) &&
	  (pt->lists[v->addr]))
	{
	  list *xl;
	  int i, loc;
	  val = scheme_nil;
	  loc = s7_gc_protect(s7, val);
	  xl = pt->lists[v->addr];
	  for (i = 0; i < xl->len; i++)
	    val = s7_cons(s7, xen_value_to_xen(pt, xl->vals[i]), val);
	  val = s7_reverse(s7, val);
	  s7_gc_unprotect_at(s7, loc);
	}
    }
  return(val);
}


static triple *make_xen_arg_triple(ptree *pt,
				   void (*function)(int *arg_addrs, ptree *pt),
				   const char *op_name,
				   xen_value **typed_args, int args)
{
  triple *trp;
  int *addrs = NULL, *types = NULL;
  addrs = (int *)calloc((args > 3) ? args : 3, sizeof(int));
  types = (int *)calloc((args > 3) ? args : 3, sizeof(int));
  if (args > 0)
    {
      xen_value **xaddrs = NULL;
      int i, xloc = -1;
      xloc = allocate_xen_vars(pt, args);
      xaddrs = pt->xen_vars[xloc];
      addrs[0] = typed_args[0]->addr; /* string result */
      addrs[1] = typed_args[1]->addr; /* num args */
      addrs[2] = xloc;                /* addr of indirect arg list */

      types[0] = typed_args[0]->type;
      types[1] = typed_args[1]->type;

      for (i = 2; i < args; i++) 
	{
	  xaddrs[i] = typed_args[i];
	  types[i] = typed_args[i]->type;
	}
    }
  else addrs[2] = -1;
  trp = (triple *)calloc(1, sizeof(triple));
  trp->function = function;
  trp->args = addrs;
  trp->types = types;
  trp->num_args = args;
  trp->op_name = op_name;
#if WITH_COUNTERS
  trp->func_loc = get_func_loc(function, op_name);
#endif
  return(trp);
}


static xen_value *package_n_xen_args(ptree *prog,
				     int type, 
				     void (*function)(int *arg_addrs, ptree *pt),
				     const char *descr,
				     xen_value **args,
				     int num_args)
{
  int i;
  xen_value **new_args;
  xen_value *rtn;
  new_args = (xen_value **)calloc(num_args + 2, sizeof(xen_value *));
  for (i = 1; i <= num_args; i++)
    {
      new_args[i + 1] = copy_xen_value(args[i]); /* copy so that w->walker can be used (and cleanup args) */
      add_xen_value_to_gcs(prog, new_args[i + 1]);
    }
  new_args[1] = make_xen_value(R_INT, add_int_to_ptree(prog, num_args), R_CONSTANT);
  new_args[0] = add_temporary_var_to_ptree(prog, type);
  add_triple_to_ptree(prog, make_xen_arg_triple(prog, function, descr, new_args, num_args + 2));
  rtn = new_args[0];
  free(new_args[1]);
  free(new_args);
  return(rtn);
}


static s7_pointer xen_values_to_list(ptree *pt, int *args)
{
  s7_pointer lst = scheme_nil;
  if (args[2] >= 0) 
    {
      int i;
      xen_value **xargs = NULL;
      xargs = pt->xen_vars[args[2]];
      for (i = (int)(INT_ARG_1 + 1); i >= 2; i--)
	lst = s7_cons(s7, xen_value_to_xen(pt, xargs[i]), lst);
    }
  return(lst);
}


static void format_s(int *args, ptree *pt) 
{
  if (STRING_RESULT) free(STRING_RESULT);
  STRING_RESULT = mus_strdup(s7_format(s7, xen_values_to_list(pt, args)));
}


static void clm_print_s(int *args, ptree *pt) 
{
  if (STRING_RESULT) free(STRING_RESULT);
  STRING_RESULT = mus_strdup(s7_format(s7, xen_values_to_list(pt, args)));
  fprintf(stderr, "%s", STRING_RESULT);
}


static void s7_version_s(int *args, ptree *pt)
{
  if (STRING_RESULT) free(STRING_RESULT);
  STRING_RESULT = mus_strdup(S7_VERSION);
}

static xen_value *s7_version_1(ptree *prog, xen_value **args, int num_args)
{
  return(package(prog, R_STRING, s7_version_s, "s7_version_s", args, 0));
}


static void open_output_file_s2(int *args, ptree *pt)
{
  SCHEME_RESULT = s7_open_output_file(s7, STRING_ARG_1, STRING_ARG_2);
}

static void open_output_file_s1(int *args, ptree *pt)
{
  SCHEME_RESULT = s7_open_output_file(s7, STRING_ARG_1, "w");
}

static xen_value *open_output_file_1(ptree *prog, xen_value **args, int num_args)
{
  args[0] = make_xen_value(R_XEN, add_xen_to_ptree(prog, s7_nil(s7)), R_VARIABLE);
  if (num_args == 1)
    add_triple_to_ptree(prog, va_make_triple(open_output_file_s1, "open_output_file_s1", 2, args[0], args[1]));
  else add_triple_to_ptree(prog, va_make_triple(open_output_file_s2, "open_output_file_s2", 3, args[0], args[1], args[2]));
  return(args[0]);
}


static void close_output_port_s(int *args, ptree *pt)
{
  s7_close_output_port(s7, SCHEME_ARG_1);
  BOOL_RESULT = false;
}

static xen_value *close_output_port_1(ptree *prog, xen_value **args, int num_args)
{
  return(package(prog, R_BOOL, close_output_port_s, "close_output_port_s", args, 1));
}


static void close_input_port_s(int *args, ptree *pt)
{
  s7_close_input_port(s7, SCHEME_ARG_1);
  BOOL_RESULT = false;
}

static xen_value *close_input_port_1(ptree *prog, xen_value **args, int num_args)
{
  return(package(prog, R_BOOL, close_input_port_s, "close_input_port_s", args, 1));
}


static void is_output_port_s(int *args, ptree *pt)
{
  BOOL_RESULT = s7_is_output_port(s7, SCHEME_ARG_1);
}

static xen_value *is_output_port_1(ptree *prog, xen_value **args, int num_args)
{
  return(package(prog, R_BOOL, is_output_port_s, "is_output_port_s", args, 1));
}


static void is_input_port_s(int *args, ptree *pt)
{
  BOOL_RESULT = s7_is_input_port(s7, SCHEME_ARG_1);
}

static xen_value *is_input_port_1(ptree *prog, xen_value **args, int num_args)
{
  return(package(prog, R_BOOL, is_input_port_s, "is_input_port_s", args, 1));
}


static void current_output_port_x(int *args, ptree *pt)
{
  SCHEME_RESULT = s7_current_output_port(s7);
}

static xen_value *current_output_port_1(ptree *prog, xen_value **args, int num_args)
{
  args[0] = make_xen_value(R_XEN, add_xen_to_ptree(prog, s7_nil(s7)), R_VARIABLE);
  add_triple_to_ptree(prog, va_make_triple(current_output_port_x, "current_output_port_x", 1, args[0]));
  return(args[0]);
}


static void current_input_port_x(int *args, ptree *pt)
{
  SCHEME_RESULT = s7_current_input_port(s7);
}

static xen_value *current_input_port_1(ptree *prog, xen_value **args, int num_args)
{
  args[0] = make_xen_value(R_XEN, add_xen_to_ptree(prog, s7_nil(s7)), R_VARIABLE);
  add_triple_to_ptree(prog, va_make_triple(current_input_port_x, "current_input_port_x", 1, args[0]));
  return(args[0]);
}


static void current_error_port_x(int *args, ptree *pt)
{
  SCHEME_RESULT = s7_current_error_port(s7);
}

static xen_value *current_error_port_1(ptree *prog, xen_value **args, int num_args)
{
  args[0] = make_xen_value(R_XEN, add_xen_to_ptree(prog, s7_nil(s7)), R_VARIABLE);
  add_triple_to_ptree(prog, va_make_triple(current_error_port_x, "current_error_port_x", 1, args[0]));
  return(args[0]);
}


static void file_exists_p(int *args, ptree *pt)
{
  BOOL_RESULT = mus_file_probe(STRING_ARG_1);
}

static xen_value *file_exists_p_1(ptree *prog, xen_value **args, int num_args)
{
  return(package(prog, R_BOOL, file_exists_p, "file_exists_p", args, 1));
}


static void getpid_i(int *args, ptree *pt)
{
  INT_RESULT = getpid();
}

static xen_value *getpid_1(ptree *prog, xen_value **args, int num_args)
{
  return(package(prog, R_INT, getpid_i, "getpid_i", args, 0));
}


static xen_value *format_1(ptree *prog, xen_value **args, int num_args)
{
  return(package_n_xen_args(prog, R_STRING, format_s, "format_s", args, num_args));
}


static xen_value *clm_print_1(ptree *prog, xen_value **args, int num_args)
{
  return(package_n_xen_args(prog, R_STRING, clm_print_s, "clm_print_s", args, num_args));
}



static int saw_mus_error = 0;

static void throw_s_1(int *args, ptree *pt) 
{
  s7_pointer res;
  res = pt->xens[args[1]];
  mus_run_free_ptree(pt);
  /* mus_run_free_ptree handles cleanup/global resets -- can we safely call it here? (i.e. no possible catch within run itself),
   */
  saw_mus_error = 0;
  pt = NULL;
  s7_error(s7, res, scheme_false);
}

static xen_value *throw_1(ptree *prog, xen_value **args, int num_args)
{
  return(package(prog, R_BOOL, throw_s_1, "throw_s_1", args, 1));
}


typedef struct {
  int *lambda_arg_types;
  int lambda_args;
} lambda_arg_info;

typedef struct {
  int args;
  lambda_arg_info **arg_data;
} lambda_info;
  
typedef struct {
  xen_value *(*walker)(ptree *prog, xen_value **args, int num_args);
  xen_value *(*special_walker)(ptree *prog, s7_pointer form, walk_result_t need_result);
  void (*set_walker)(ptree *prog, xen_value *in_v, xen_value *in_v1, xen_value *in_v2, xen_value *v);
  int required_args, max_args, result_type, num_arg_types;
  bool need_int_result;
  int *arg_types;
  int data;
  lambda_info *linfo;
} walk_info;

static walk_info *make_walker(xen_value *(*walker)(ptree *prog, xen_value **args, int num_args),
			      xen_value *(*special_walker)(ptree *prog, s7_pointer form, walk_result_t need_result),
			      void (*set_walker)(ptree *prog, xen_value *in_v, xen_value *in_v1, xen_value *in_v2, xen_value *v),
			      int required_args, 
			      int max_args, 
			      int result_type, 
			      bool need_int_result,
			      int num_arg_types,
			      ...) /* arg type list, R_NUMBER=int or float, R_ANY=unchecked */
{
  walk_info *w;
  w = (walk_info *)malloc(sizeof(walk_info));
  w->walker = walker;
  w->special_walker = special_walker;
  w->set_walker = set_walker;
  w->required_args = required_args;
  w->max_args = max_args;
  w->result_type = result_type;
  w->need_int_result = need_int_result;
  w->num_arg_types = num_arg_types;
  w->data = 0;
  w->linfo = NULL;
  if (num_arg_types > 0)
    {
      va_list ap;
      int i;
      va_start(ap, num_arg_types);
      w->arg_types = (int *)calloc(num_arg_types, sizeof(int));
      for (i = 0; i < num_arg_types; i++)
	w->arg_types[i] = (int)(va_arg(ap, int));
      va_end(ap);
    }
  return(w);
}

static walk_info *walker_with_declare(walk_info *w, int arg, int args, ...)
{
  lambda_info *li;
  lambda_arg_info *larg;

  if (!(w->linfo))
    w->linfo = (lambda_info *)calloc(1, sizeof(lambda_info));
  li = w->linfo;

  if (li->args <= arg)
    {
      if (li->args == 0)
	li->arg_data = (lambda_arg_info **)calloc(arg + 1, sizeof(lambda_arg_info *));
      else li->arg_data = (lambda_arg_info **)realloc(li->arg_data, (arg + 1) * sizeof(lambda_arg_info *));
      li->args = arg + 1;
    }
  li->arg_data[arg] = (lambda_arg_info *)calloc(1, sizeof(lambda_arg_info));

  larg = li->arg_data[arg];
  larg->lambda_args = args;
  if (args > 0)
    {
      int i;
      va_list ap;
      larg->lambda_arg_types = (int *)calloc(args, sizeof(int));
      va_start(ap, args);
      for (i = 0; i < args; i++)
	larg->lambda_arg_types[i] = (int)(va_arg(ap, int));
      va_end(ap);
    }

  return(w);
}



/* -------- CLM make functions -------- */

#define CLM_MAKE_FUNC(Name) \
static void make_ ## Name ## _0(int *args, ptree *pt) \
{ \
  s7_pointer res; \
  res = s7_call(s7, s7_symbol_value(s7, s7_make_symbol(s7, S_make_ ## Name)), xen_values_to_list(pt, args)); \
  if (mus_xen_p(res)) \
    { \
      if (CLM_LOC >= 0) s7_gc_unprotect_at(s7, CLM_LOC); \
      add_loc_to_protected_list(pt, CLM_LOC = s7_gc_protect(s7, res)); \
      CLM_RESULT = XEN_TO_MUS_ANY(res); \
    } \
  else CLM_RESULT = NULL; \
} \
static xen_value *make_ ## Name ## _1(ptree *prog, xen_value **args, int num_args) \
{ \
  return(package_n_xen_args(prog, R_CLM, make_ ## Name ## _0, "make_" #Name "_0", args, num_args)); \
}

CLM_MAKE_FUNC(all_pass)
CLM_MAKE_FUNC(moving_average)
CLM_MAKE_FUNC(asymmetric_fm)
CLM_MAKE_FUNC(comb)
CLM_MAKE_FUNC(filtered_comb)
CLM_MAKE_FUNC(convolve)
CLM_MAKE_FUNC(delay)
CLM_MAKE_FUNC(env)
CLM_MAKE_FUNC(file_to_frame)
CLM_MAKE_FUNC(file_to_sample)
CLM_MAKE_FUNC(filter)
CLM_MAKE_FUNC(fir_filter)
CLM_MAKE_FUNC(firmant)
CLM_MAKE_FUNC(formant)
CLM_MAKE_FUNC(frame)
CLM_MAKE_FUNC(frame_to_file)
CLM_MAKE_FUNC(granulate)
CLM_MAKE_FUNC(iir_filter)
CLM_MAKE_FUNC(locsig)
CLM_MAKE_FUNC(mixer)
CLM_MAKE_FUNC(notch)
CLM_MAKE_FUNC(one_pole)
CLM_MAKE_FUNC(one_zero)
CLM_MAKE_FUNC(oscil)
CLM_MAKE_FUNC(phase_vocoder)
CLM_MAKE_FUNC(pulse_train)
CLM_MAKE_FUNC(rand)
CLM_MAKE_FUNC(rand_interp)
CLM_MAKE_FUNC(readin)
CLM_MAKE_FUNC(sample_to_file)
CLM_MAKE_FUNC(sawtooth_wave)
CLM_MAKE_FUNC(nrxysin)
CLM_MAKE_FUNC(nrxycos)
CLM_MAKE_FUNC(square_wave)
CLM_MAKE_FUNC(src)
CLM_MAKE_FUNC(ncos)
CLM_MAKE_FUNC(nsin)
CLM_MAKE_FUNC(ssb_am)
CLM_MAKE_FUNC(table_lookup)
CLM_MAKE_FUNC(triangle_wave)
CLM_MAKE_FUNC(two_pole)
CLM_MAKE_FUNC(two_zero)
CLM_MAKE_FUNC(wave_train)
CLM_MAKE_FUNC(polyshape)
CLM_MAKE_FUNC(polywave)


static void make_fft_window_0(int *args, ptree *pt)
{
  s7_pointer res;
  res = s7_call(s7, s7_symbol_value(s7, s7_make_symbol(s7, S_make_fft_window)), xen_values_to_list(pt, args));
  add_loc_to_protected_list(pt, s7_gc_protect(s7, res));
  VCT_RESULT = xen_to_vct(res);
}


static xen_value *make_fft_window_1(ptree *prog, xen_value **args, int num_args)
{
  return(package_n_xen_args(prog, R_VCT, make_fft_window_0, "make_fft_window_0", args, num_args));
}


#define XEN_TO_ADDR_ERROR -1

static int xen_to_addr(ptree *pt, s7_pointer arg, int type, int addr)
{
  /* this includes handling args to the outer lambda */

  /* fprintf(stderr, "xen to addr: %s %d (%s) %d\n", s7_object_to_c_string(s7, arg), type, type_name(type), addr); */

  if ((mus_run_xen_to_run_type(arg) != type) &&
      ((!(s7_is_real(arg))) || ((type != R_FLOAT) && (type != R_INT))))
    {
      /* fprintf(stderr, "types differ "); */

      if ((arg == scheme_false) &&
	  (POINTER_P(type)))
	{
	  /* passed null pointer -- probably won't be what caller intends */
	  switch (type)
	    {
	      /* these special cases we handle in set_var (line 2474) */
	    case R_FLOAT_VECTOR:
	    case R_VCT:          pt->vcts[addr] = NULL;          break;
	    case R_SOUND_DATA:   pt->sds[addr] = NULL;           break;
	    case R_CLM:          pt->clms[addr] = NULL;          break;
#if USE_SND
	    case R_SAMPLER:      pt->samplers[addr] = NULL;      break;
#endif
	    case R_LIST:         pt->lists[addr] = NULL;         break;
	    case R_LIST_VECTOR:
	    case R_INT_VECTOR: 
	    case R_VCT_VECTOR:
	    case R_CLM_VECTOR:   pt->vects[addr] = NULL;         break;
	    default: 
	      {
		char *temp = NULL;
		run_warn("run: xen_to_addr: %s %s", temp = s7_object_to_c_string(s7, arg), type_name(type)); 
		if (temp) free(temp);
		return(XEN_TO_ADDR_ERROR);
	      }
	      break;
	    }
	  return(addr);
	}
      {
	char *temp = NULL;
	run_warn("run: xen_to_addr 2: %s %s", temp = s7_object_to_c_string(s7, arg), type_name(type));
	if (temp) free(temp);
      }
      return(XEN_TO_ADDR_ERROR);
    }
  switch (type)
    {
    case R_FLOAT:        pt->dbls[addr] = (Double)s7_number_to_real(arg);     break;
    case R_INT:          pt->ints[addr] = s7_number_to_integer(arg);              break;
    case R_CHAR:         pt->ints[addr] = (Int)s7_character(arg);          break;
    case R_BOOL:         pt->ints[addr] = (Int)scheme_to_c_bool(arg);       break;
    case R_VCT:          pt->vcts[addr] = xen_to_vct(arg);                  break;
    case R_SOUND_DATA:   pt->sds[addr] = XEN_TO_SOUND_DATA(arg);            break;
    case R_CLM:          pt->clms[addr] = XEN_TO_MUS_ANY(arg);              break;
#if USE_SND
    case R_SAMPLER:      pt->samplers[addr] = xen_to_sampler(arg);          break;
    case R_MARK:         pt->ints[addr] = XEN_MARK_TO_C_INT(arg);           break;
    case R_MIX:          pt->ints[addr] = XEN_MIX_TO_C_INT(arg);            break;
    case R_REGION:       pt->ints[addr] = XEN_REGION_TO_C_INT(arg);         break;
    case R_SOUND:        pt->ints[addr] = XEN_SOUND_TO_C_INT(arg);          break;
#endif
    case R_SYMBOL:
    case R_KEYWORD:      pt->xens[addr] = arg;                              break;

    case R_STRING:
      if (!(pt->strs[addr]))
	pt->strs[addr] = mus_strdup(s7_string(arg)); 
      break;

    case R_FLOAT_VECTOR:
      if (!(pt->vcts[addr]))
	{
	  pt->vcts[addr] = vector_to_vct(arg);
	  add_obj_to_gcs(pt, R_FLOAT_VECTOR, addr);
	}
      break;

    case R_LIST_VECTOR: 
    case R_INT_VECTOR: 
    case R_VCT_VECTOR:
    case R_CLM_VECTOR:
      if (!(pt->vects[addr]))
	{
	  pt->vects[addr] = read_vector(pt, arg, type);
	  add_obj_to_gcs(pt, type, addr);
	}
      break;

    default:
      if ((type == R_LIST) ||
	  (CLM_STRUCT_P(type)))
	{
	  /* fprintf(stderr,"adding arg list..."); */
	  pt->lists[addr] = xen_to_list_with_type(pt, arg, (type == R_LIST) ? R_FLOAT : type);
	  add_obj_to_gcs(pt, type, addr);
	  /* fprintf(stderr,"!\n"); */
	}
      break;      
    }
  return(addr);
}


static xen_value *quote_form(ptree *prog, s7_pointer form, walk_result_t ignore) 
{
  xen_value *v;

  v = add_value_to_ptree(prog, scheme_cadr(form), mus_run_xen_to_run_type(scheme_cadr(form)));
  if (v == NULL)
    {
      xen_value *rv;
      char *temp = NULL;
      rv = run_warn("quote can't handle %s", temp = s7_object_to_c_string(s7, form));
      if (temp) free(temp);
      return(rv);
    }
  v->constant = R_CONSTANT;
  return(v);
}



/* defgenerator support */

static void clm_struct_p(int *args, ptree *pt) 
{
  BOOL_RESULT = (INT_ARG_2 == LIST_ARG_1->type);
}


static xen_value *clm_struct_p_1(ptree *prog, xen_value **args, int num_args)
{
  return(package(prog, R_BOOL, clm_struct_p, "clm_struct_p", args, num_args));
}


static int add_clm_type(s7_pointer name)
{
  char *type_predicate_name;
  int run_type;
  walk_info *w;

  run_type = add_new_type(s7_string(name));
  type_predicate_name = (char *)calloc(strlen(type_name(run_type)) + 2, sizeof(char));
  sprintf(type_predicate_name, "%s?", type_name(run_type));

  w = make_walker(clm_struct_p_1, NULL, NULL, 1, 1, R_BOOL, false, 1, R_ANY);
  w->data = run_type;

  scheme_set_walker((s7_pointer)(s7_make_symbol(s7, type_predicate_name)), 
		    s7_make_c_pointer(s7, (void *)w));

  return(run_type);
}


typedef struct {
  int fields;
  int *field_types;
  char **field_names;
} dcs;

static dcs **def_clm_structs = NULL;
static int def_clm_structs_size = 0;


#define S_add_clm_field "add-clm-field"

static s7_pointer g_add_clm_field(s7_pointer struct_name, s7_pointer name, s7_pointer offset, s7_pointer type)
{
  #define H_add_clm_field "defgenerator tie-in to run optimizer"
  const char *field_name;
  int field_offset, clm_struct_type, field_type;
  dcs *d;
  walk_info *w;

  XEN_ASSERT_TYPE(s7_is_string(struct_name), struct_name, 1, S_add_clm_field, "string");
  XEN_ASSERT_TYPE(s7_is_string(name), name, 2, S_add_clm_field, "string");
  XEN_ASSERT_TYPE(s7_is_integer(offset), offset, 3, S_add_clm_field, "int");
  XEN_ASSERT_TYPE(s7_is_symbol(type), type, 4, S_add_clm_field, "symbol");

  clm_struct_type = name_to_type(s7_string(struct_name));
  if (clm_struct_type == R_UNSPECIFIED)
    clm_struct_type = add_clm_type(struct_name);

  field_name = s7_string(name);
  field_type = name_to_type(s7_symbol_name(type));
  field_offset = s7_number_to_integer(offset);

  /* fprintf(stderr, "add %s to %s (%d) at %d type %s\n", field_name, s7_string(struct_name), clm_struct_type, field_offset, type_name(field_type)); */

  if (clm_struct_type >= def_clm_structs_size)
    {
      def_clm_structs_size = clm_struct_type + 1;
      if (!def_clm_structs)
	def_clm_structs = (dcs **)calloc(def_clm_structs_size, sizeof(dcs *));
      else def_clm_structs = (dcs **)realloc(def_clm_structs, def_clm_structs_size * sizeof(dcs *));
      def_clm_structs[clm_struct_type] = (dcs *)calloc(1, sizeof(dcs));
    }

  d = def_clm_structs[clm_struct_type];
  if (field_offset >= d->fields)
    {
      if (d->fields == 0)
	{
	  d->fields = 8;
	  d->field_types = (int *)calloc(d->fields, sizeof(int));
	  d->field_names = (char **)calloc(d->fields, sizeof(char *));
	}
      else
	{
	  d->fields *= 2;
	  d->field_types = (int *)realloc(d->field_types, d->fields * sizeof(int));
	  d->field_names = (char **)realloc(d->field_names, d->fields * sizeof(char *));
	}
    }
  d->field_types[field_offset] = field_type;
  d->field_names[field_offset] = mus_strdup(field_name);

  /* now declare the get and set walkers */

  w = make_walker(r_list_ref_1, NULL, clm_struct_field_set_1, 1, 1, field_type, false, 1, R_LIST);
  w->data = field_offset;

  scheme_set_walker((s7_pointer)(s7_make_symbol(s7, field_name)), 
		 s7_make_c_pointer(s7, (void *)w));

  return(name);
}


static int def_clm_struct_field_type(int def_clm_struct_type, int field)
{
  dcs *d;
  d = def_clm_structs[def_clm_struct_type];
  if ((d) &&
      (field < d->fields))
    return(d->field_types[field]); /* there are trailing empty field slots, so this just guards against reading off the end of the array */
  return(R_UNSPECIFIED);
}


static char *def_clm_struct_field_name(int def_clm_struct_type, int field)
{
  dcs *d;
  d = def_clm_structs[def_clm_struct_type];
  if ((d) &&
      (field < d->fields))
    return(d->field_names[field]);
  return(mus_strdup("unknown clm-struct field"));
}





/* ---------------- the code walker ---------------- */

static xen_value *clean_up(xen_value *result, xen_value **args, int args_size)
{
  int i;
  /* args[0] is special */
  if ((args[0]) && (args[0] != result)) free(args[0]);
  for (i = 1; i <= args_size; i++)
    if (args[i]) 
      {
	free(args[i]);
	args[i] = NULL;
      }
  free(args);
  return(result);
}


static bool vowel_p(char b)
{
  return((b == 'a') || (b == 'e') || (b == 'i') || (b == 'o') || (b == 'u'));
}


static xen_value *arg_warn(ptree *prog, const char *funcname, int arg_num, xen_value **args, const char *correct_type)
{
  char *xb;
  const char *tb;
  xb = describe_xen_value_with_var_name(args[arg_num]->type, args[arg_num]->addr, prog);
  tb = type_name(args[arg_num]->type);
  if (xb)
    {
      run_warn("%s argument %d (%s) is a%s %s, not a%s %s?", 
	       funcname, arg_num, xb, 
	       (vowel_p(tb[0])) ? "n" : "", tb,
	       (vowel_p(correct_type[0])) ? "n" : "", correct_type);
      free(xb);
    }
  else 
    run_warn("%s argument %d is a%s %s, not a%s %s?", 
	     funcname, arg_num, 
	     (vowel_p(tb[0])) ? "n" : "", tb,
	     (vowel_p(correct_type[0])) ? "n" : "", correct_type);
  return(NULL);
}


static xen_value *walk(ptree *prog, s7_pointer form, walk_result_t walk_result)
{
  /* walk form, storing vars, making program entries for operators etc */
  s7_pointer rtnval = scheme_false;

  /* fprintf(stderr, "walk %s\n", s7_object_to_c_string(s7, form)); */

  if (!optimizing) return(NULL);

  if (s7_is_list(s7, form))
    {
      s7_pointer function, all_args;
      const char *funcname = "not-a-function";
      xen_value **args = NULL;
      int i, num_args, constants = 0;
      bool float_result = false;
      xen_var *var;
      xen_value *v = NULL;
      s7_pointer walker;
      walk_info *w = NULL;

      function = s7_car(form);
      all_args = s7_cdr(form);
      num_args = s7_list_length(s7, all_args);
      if (s7_is_symbol(function))
	{
#if 0
	  /* this is slow for some reason -- perhaps move it down? */
	  if (s7_is_macro(s7, function))
	    {
	      /* if it's a macro we want to apply its procedure-source to the form, then walk the result
	       */
	      s7_pointer new_form;
	      new_form = s7_apply_function(s7, function, form);
	      /* fprintf(stderr, "macro-> %s\n", s7_object_to_c_string(s7, new_form)); */
	      return(walk(prog, new_form, walk_result));
	    }
#endif	  
	  walker = scheme_walker(function);
	  if (s7_is_c_pointer(walker))
	    {
	      w = (walk_info *)(s7_c_pointer(walker));
	      if ((w) && (w->special_walker))
		{
		  if (num_args < w->required_args)
		    {
		      xen_value *rv;
		      char *temp = NULL;
		      rv = run_warn("missing expression for %s: %s", s7_symbol_name(function), temp = s7_object_to_c_string(s7, form));
		      if (temp) free(temp);
		      return(rv);
		    }

		  if ((w->max_args != UNLIMITED_ARGS) && 
		      (num_args > w->max_args))
		    {
		      xen_value *rv;
		      char *temp = NULL;
		      rv = run_warn("too many expressions for %s: %s", s7_symbol_name(function), temp = s7_object_to_c_string(s7, form));
		      if (temp) free(temp);
		      return(rv);
		    }

		  return((*(w->special_walker))(prog, form, walk_result));
		}
	    }
	  funcname = s7_symbol_name(function);
	}
      /* check for ((setter ...) ...) before messing up the args */
      if (s7_is_list(s7, function))
	{
	  s7_pointer caar;
	  const char *caar_name;
	  caar = s7_car(function);
	  if (s7_is_symbol(caar))
	    {
	      caar_name = s7_symbol_name(caar);
	      if (safe_strcmp(caar_name, "setter") == 0)
		{
		  /* should be clm-struct ref: ((setter moog-y) gen .1) for example */
		  /* transform (back) to (set! (moog-y gen) .1) */
		  /* fprintf(stderr, "got setter: %s\n", s7_object_to_c_string(s7, form)); */
		  return(generalized_set_form(prog,
					      s7_append(s7, scheme_list_2(s7_make_symbol(s7, "set!"),
								    scheme_list_2(scheme_cadr(function),
									       s7_car(all_args))),
							 s7_cdr(all_args))));
		  /* should this be all but last in accessor, then last as set value? */
		}
	    }
	}

      args = (xen_value **)calloc(num_args + 1, sizeof(xen_value *));
      if (num_args > 0)
	{
	  walk_result_t arg_result = NEED_ANY_RESULT;
	  for (i = 0; i < num_args; i++, all_args = s7_cdr(all_args))
	    {
	      arg_result = NEED_ANY_RESULT;
	      if (w)
		{
		  if ((w->need_int_result) ||
		      ((i < w->num_arg_types) && (w->arg_types[i] == R_INT)))
		    arg_result = NEED_INT_RESULT;

		  /* if arg might be a function, check for auto-declare */
		  if ((w->linfo) &&
		      (w->linfo->args > i) &&
		      (w->linfo->arg_data[i]) &&
		      (i < w->num_arg_types) &&
		      ((w->arg_types[i] == R_FUNCTION) || (w->arg_types[i] == R_ANY)))
		    {
		      prog->lambda_args = w->linfo->arg_data[i]->lambda_args;
		      prog->lambda_arg_types = w->linfo->arg_data[i]->lambda_arg_types;
		    }
		}

	      args[i + 1] = walk(prog, s7_car(all_args), arg_result);

	      if (prog->lambda_args > 0)
		{
		  prog->lambda_args = 0;
		  prog->lambda_arg_types = NULL;
		}
	      
	      /*
	      fprintf(stderr,"%s ->", s7_object_to_c_string(s7, s7_car(all_args)));
	      if (args[i+1])
		fprintf(stderr," %d %d\n", args[i+1]->type, args[i+1]->addr);
	      else fprintf(stderr,"null\n");
	      */

	      if (args[i + 1] == NULL) 
		return(clean_up(NULL, args, num_args)); /* no run_warn here so that reported error includes enclosing form */
	      if (args[i + 1]->constant == R_CONSTANT) constants++;
	      if (args[i + 1]->type == R_FLOAT) float_result = true; /* for "*" et al */
	    }
	}

      if (w == NULL) /* we're in a list, looking at car, and we don't optimize locally defined functions, so this should be ok */
	{
	  /* check user-defined stuff */
	  var = find_var_in_ptree(prog, funcname); /* can be "not-a-function" if function is not a symbol (an expression normally) */
	  if (var == NULL)
	    {
	      /* add_global_var will find things like *, but ignores functions and returns null */
	      if (s7_is_symbol(function))
		{
		  /* fprintf(stderr,"no walker, add %s\n", s7_object_to_c_string(s7, function)); */

		  v = add_global_var_to_ptree(prog, function, &rtnval);
		  /* if all else fails, we'll check rtnval later for externally defined functions */
		}
	      /* (let ((gen (make-oscil 440))) (vct-map! v (lambda () (gen 0.0)))) */
	      else 
		{
		  if ((s7_is_list(s7, function)) ||
		      (s7_is_string(function)) ||          /* ("asdf" 2) */
		      (s7_is_vector(function))) 
		    v = walk(prog, function, NEED_ANY_RESULT);
		}
	      /* trying to support stuff like ((vector-ref gens 0) 0.0) here */
	    }
	  else v = var->v;

	  if (v) 
	    {
	      xen_value *res = NULL;
	      switch (v->type)
		{
#if USE_SND
		case R_SAMPLER:       
		  if (num_args == 0) res = sampler_0(prog, args, v);       
		  break;
#endif
		case R_CLM:          
		  res = clm_n(prog, args, num_args, v); 
		  /* this calls mus_run so it also handles (in a kludgey way) frame and mixer refs */
		  break;

		case R_BOOL:
		case R_GOTO:         
		  if (num_args == 0) res = goto_0(prog, args, v);         
		  break;

		case R_LIST:
		  {
		    /* this implements the "implicit-indexing" of lists: (lst 1) -> (list-ref lst 1) 
		     *    the constant case is above
		     */
		    xen_value **new_args;
		    if (num_args == 0)
		      return(run_warn("not enough args"));
		    if (args[1]->type != R_INT)
		      return(clean_up(arg_warn(prog, "implicit list-ref", 1, args, "integer"), args, num_args));		      

		    new_args = (xen_value **)calloc(3, sizeof(xen_value *));
		    new_args[1] = v;
		    new_args[2] = args[1];
		    res = r_list_ref_1(prog, new_args, 2);
		    new_args[1] = NULL;
		    if (args[0]) free(args[0]);
		    return(clean_up(res, new_args, 2));
		  }
		  break;
		  
		case R_STRING:
		  {
		    xen_value **new_args;
		    if (num_args == 0)
		      return(run_warn("not enough args"));
		    if (args[1]->type != R_INT)
		      return(clean_up(arg_warn(prog, "implicit string-ref", 1, args, "integer"), args, num_args));		      

		    new_args = (xen_value **)calloc(3, sizeof(xen_value *));
		    new_args[1] = v;
		    new_args[2] = args[1];
		    res = r_string_ref_1(prog, new_args, 2);
		    new_args[1] = NULL;
		    if (args[0]) free(args[0]);
		    return(clean_up(res, new_args, 2));
		  }
		  break;

		case R_FUNCTION:     
		  res = funcall_n(prog, args, num_args, v);
		  break;

		case R_VCT:
		case R_FLOAT_VECTOR:
		  {
		    xen_value **new_args;
		    if (num_args == 0)
		      return(run_warn("not enough args"));
		    if (args[1]->type != R_INT)
		      return(clean_up(arg_warn(prog, "implicit vct-ref", 1, args, "integer"), args, num_args));		      

		    new_args = (xen_value **)calloc(3, sizeof(xen_value *));
		    new_args[1] = v;
		    new_args[2] = args[1];
		    res = vct_ref_1(prog, new_args, 2);
		    new_args[1] = NULL;
		    if (args[0]) free(args[0]);
		    return(clean_up(res, new_args, 2));
		  }
		  break;

		case R_CLM_VECTOR:
		  args[0] = make_xen_value(R_CLM, add_clm_to_ptree(prog, NULL, scheme_false), R_VARIABLE);
		  add_triple_to_ptree(prog, va_make_triple(vector_ref_c, "clm_vector_ref", 3, args[0], v, args[1]));
		  res = args[0];
		  break;

		case R_INT_VECTOR:
		  args[0] = make_xen_value(R_INT, add_int_to_ptree(prog, 0), R_VARIABLE);
		  add_triple_to_ptree(prog, va_make_triple(vector_ref_i, "int_vector_ref", 3, args[0], v, args[1]));
		  res = args[0];
		  break;

		case R_VCT_VECTOR:
		  args[0] = make_xen_value(R_VCT, add_vct_to_ptree(prog, NULL), R_VARIABLE);
		  add_triple_to_ptree(prog, va_make_triple(vector_ref_v, "vct_vector_ref", 3, args[0], v, args[1]));
		  res = args[0];
		  break;

		case R_SOUND_DATA:
		  res = sound_data_n(prog, args, num_args, v);
		  break;

		  /* fall through here if unknown function encountered (will give up later) */
		}
	      if (var == NULL) {free(v); v = NULL;}
	      if (res) return(clean_up(res, args, num_args)); 
	    }

	  if (prog->goto_ctr > 0)
	    {
	      /* possibly continuation procedure */
	      continuation *c = NULL;
	      for (i = prog->goto_ctr - 1; i >= 0; i--)
		{
		  c = prog->gotos[i];
		  if ((c) && 
		      (mus_strcmp(c->name, funcname)))
		    {
		      if (num_args > 0)
			{
			  if (c->result)
			    {
			      if (c->result->type != args[1]->type) 
				return(clean_up(run_warn("continuation types differ"), args, num_args));
			    }
			  else c->result = add_empty_var_to_ptree(prog, args[1]->type);
			  set_var(prog, c->result, args[1]);
			}
		      add_triple_to_ptree(prog, va_make_triple(jump_abs, "jump_abs", 1, c->jump));
		      return(clean_up(copy_xen_value((c->result) ? c->result : c->jump), args, num_args));
		    }
		}
	    }
	}
      
      if (w)
	{
	  int i, args_to_check, true_type;
	  if (num_args < w->required_args) 
	    return(clean_up(run_warn("not enough args (%d) for %s", num_args, funcname), args, num_args));

	  if ((w->max_args != UNLIMITED_ARGS) && 
	      (num_args > w->max_args)) 
	    return(clean_up(run_warn("too many args (%d) for %s", num_args, funcname), args, num_args));

	  args_to_check = num_args;
	  /* neg type = all args are this type */

	  if ((w->num_arg_types == 1) &&
	      (w->arg_types[0] < 0))
	    {
	      /* check all :rest args */
	      true_type = -(w->arg_types[0]);

	      for (i = 1; i <= num_args; i++)
		if ((args[i]->type != true_type) &&
		    (((true_type != R_NUMBER) ||
		      ((args[i]->type != R_INT) && 
		       (args[i]->type != R_FLOAT))) &&
		     ((true_type != R_XEN) ||
		      (!(xenable(args[i]))))))
		  return(clean_up(arg_warn(prog, funcname, i, args, type_name(true_type)), args, num_args));
	    }
	  else
	    {
	      if (w->num_arg_types < args_to_check) args_to_check = w->num_arg_types;
	      for (i = 0; i < args_to_check; i++)
		{
		  /* fprintf(stderr, "check %d: %s %s\n", i + 1, type_name(w->arg_types[i]), type_name(args[i + 1]->type)); */

		  if ((w->arg_types[i] != args[i + 1]->type) &&
		      (w->arg_types[i] != R_ANY) &&
		      (!(((POINTER_P(w->arg_types[i])) || (w->arg_types[i] == R_FUNCTION)) && (args[i + 1]->type == R_BOOL))))
		    {
		      switch (w->arg_types[i])
			{
			case R_NUMBER:
			  if ((args[i + 1]->type != R_INT) &&
			      (args[i + 1]->type != R_FLOAT))
			    return(clean_up(arg_warn(prog, funcname, i + 1, args, "number"), args, num_args));
			  break;

			case R_VECTOR:
			  if (!(VECTOR_P(args[i + 1]->type)))
			    return(clean_up(arg_warn(prog, funcname, i + 1, args, "vector"), args, num_args));
			  break;

			case R_XEN:
			  if (!(xenable(args[i + 1])))
			    return(clean_up(arg_warn(prog, funcname, i + 1, args, "indirect"), args, num_args));
			  break;

			case R_NUMBER_CLM:
			  if ((args[i + 1]->type != R_INT) &&
			      (args[i + 1]->type != R_FLOAT) &&
			      (args[i + 1]->type != R_CLM))
			    return(clean_up(arg_warn(prog, funcname, i + 1, args, "clm or number"), args, num_args));
			  break;
			  
			case R_NUMBER_VCT:
			  if ((args[i + 1]->type != R_INT) &&
			      (args[i + 1]->type != R_FLOAT) &&
			      (args[i + 1]->type != R_VCT))
			    return(clean_up(arg_warn(prog, funcname, i + 1, args, "vct or number"), args, num_args));
			  break;

			case R_NUMBER_SOUND_DATA:
			  if ((args[i + 1]->type != R_INT) &&
			      (args[i + 1]->type != R_FLOAT) &&
			      (args[i + 1]->type != R_SOUND_DATA))
			    return(clean_up(arg_warn(prog, funcname, i + 1, args, "sound_data or number"), args, num_args));
			  break;

			case R_LIST:
			  if ((args[i + 1]->type != R_LIST) &&
			      (!(CLM_STRUCT_P(args[i + 1]->type))))
			    return(clean_up(arg_warn(prog, funcname, i + 1, args, "list"), args, num_args));
			  break;

			case R_GENERATOR:
			  if ((args[i + 1]->type != R_CLM) &&
			      (!(CLM_STRUCT_P(args[i + 1]->type))) &&
			      (args[i + 1]->type != R_BOOL))
			    return(clean_up(arg_warn(prog, funcname, i + 1, args, "generator"), args, num_args));
			  break;

			default:
			  return(clean_up(arg_warn(prog, funcname, i + 1, args, type_name(w->arg_types[i])), args, num_args));
			}
		    }
		}
	    }

	  if (w->walker)
	    {
	      prog->constants = constants;
	      prog->float_result = float_result;
	      prog->walk_result = walk_result;
	      if (w->data != 0)
		{
		  args = (xen_value **)realloc(args, (num_args + 2) * sizeof(xen_value *));
		  args[num_args + 1] = make_xen_value(R_INT, add_int_to_ptree(prog, w->data), R_CONSTANT);
		  num_args++;
		}
	      return(clean_up((*(w->walker))(prog, args, num_args), args, num_args));
	    }
	}

      /* check for function defined elsewhere, get source, splice in if possible */
      if ((v == NULL) && 
	  (optimizing) &&
	  (s7_is_procedure(rtnval)) &&
	  (!(s7_is_procedure_with_setter(rtnval)))
	  )
	{
	  /* fprintf(stderr, "look for %s\n", s7_object_to_c_string(s7, rtnval)); */
	  
	  v = splice_in_function_body(prog, rtnval, args, num_args, funcname);
	  if (v) 
	    return(clean_up(v, args, num_args));
	}

      if ((walk_result) || (s7_is_list(s7, form)))
	/* need the list check as well because the called function might have side-effects */
	return(clean_up(NULL, args, num_args));

      return(clean_up(make_xen_value(R_UNSPECIFIED, -1, R_CONSTANT), args, num_args));
    }
  else /* not a list */
    {
      int type;

      if ((s7_is_symbol(form)) &&
	  (s7_is_constant(form)))
	form = symbol_to_value(prog->code, form);

      type = mus_run_xen_to_run_type(form);
      /* fprintf(stderr, "line 15924 %s %s\n", s7_object_to_c_string(s7, form), type_name(type)); */

      switch (type)
	{
	case R_INT:     return(make_xen_value(R_INT,     add_int_to_ptree(prog, s7_number_to_integer(form)), R_CONSTANT));        break;
	case R_FLOAT:   return(make_xen_value(R_FLOAT,   add_dbl_to_ptree(prog, s7_number_to_real(form)), R_CONSTANT));           break;
	case R_STRING:  return(make_xen_value(R_STRING,  add_string_to_ptree(prog, mus_strdup(s7_string(form))), R_CONSTANT));    break;
	case R_CHAR:    return(make_xen_value(R_CHAR,    add_int_to_ptree(prog, (Int)(s7_character(form))), R_CONSTANT));         break;
	case R_BOOL:    return(make_xen_value(R_BOOL,    add_int_to_ptree(prog, (form == scheme_false) ? 0 : 1), R_CONSTANT));    break;
	case R_KEYWORD: return(make_xen_value(R_KEYWORD, add_xen_to_ptree(prog, form), R_CONSTANT));                              break;
	case R_XEN:     return(make_xen_value(R_XEN,     add_xen_to_ptree(prog, form), R_CONSTANT));                              break;
#if USE_SND
	case R_SOUND:   return(make_xen_value(R_SOUND,   add_int_to_ptree(prog, XEN_SOUND_TO_C_INT(form)), R_CONSTANT));          break;
	case R_MIX:     return(make_xen_value(R_MIX,     add_int_to_ptree(prog, XEN_MIX_TO_C_INT(form)), R_CONSTANT));            break;
	case R_MARK:    return(make_xen_value(R_MARK,    add_int_to_ptree(prog, XEN_MARK_TO_C_INT(form)), R_CONSTANT));           break;
	case R_REGION:  return(make_xen_value(R_REGION,  add_int_to_ptree(prog, XEN_REGION_TO_C_INT(form)), R_CONSTANT));         break;
#endif
	}

      if (s7_is_symbol(form))
	{
	  s7_pointer ignore = scheme_false;
	  prog->walk_result = walk_result;
	  return(add_global_var_to_ptree(prog, form, &ignore)); /* misleading name -- this handles any variable reference */
	}
      /* things like complex numbers fall through here */
    }

  if (!run_warned)
    {
      xen_value *rv;
      char *temp1 = NULL, *temp2 = NULL;
      /* fprintf(stderr, "walker can't handle %s\n", s7_object_to_c_string(s7, form)); */
      rv = run_warn("can't handle: %s (%s)", temp1 = s7_object_to_c_string(s7, form), temp2 = s7_object_to_c_string(s7, s7_procedure_source(s7, prog->code)));
      if (temp1) free(temp1);
      if (temp2) free(temp2);
      return(rv);
    }

  return(NULL);
}


static xen_value *lookup_generalized_set(ptree *prog, s7_pointer acc_form, xen_value *in_v, xen_value *in_v1, xen_value *in_v2, xen_value *v)
{
  int happy = 0;
  s7_pointer walker;
  walk_info *w = NULL;

  walker = scheme_walker(acc_form);
  if (s7_is_c_pointer(walker))
    {
      w = (walk_info *)(s7_c_pointer(walker));
      if (w)
	{
	  if ((w->set_walker) &&
	      ((v->type == w->result_type) ||
	       ((w->result_type == R_NUMBER) &&
		((v->type == R_FLOAT) || (v->type == R_INT)))))
	    {
	      if (w->data != 0)
		in_v2 = make_xen_value(R_INT, w->data, R_CONSTANT);
	      (*(w->set_walker))(prog, in_v, in_v1, in_v2, v);
	      happy = 1;
	    }
	  else 
	    {
	      char *xb;
	      const char *tb, *vb;
	      xb = describe_xen_value(v, prog);
	      tb = type_name(w->result_type);
	      vb = type_name(v->type);
	      run_warn("can't set %s (a%s %s) to %s (a%s %s)",
		       s7_symbol_name(acc_form),
		       (vowel_p(tb[0])) ? "n" : "", tb,
		       xb,
		       (vowel_p(vb[0])) ? "n" : "", vb);
	      if (xb) free(xb);
	      happy = 2;
	    }
	}
    }

  { 
    xen_value *val;
    val = walk(prog, acc_form, DONT_NEED_RESULT);
    /*
    fprintf(stderr, "val: %s, v: %s, in_v: %s, in_v1: %s, in_v2: %s\n", 
	    (val) ? describe_xen_value(val, prog) : "null", 
	    (v) ? describe_xen_value(v, prog) : "null", 
	    (in_v) ? describe_xen_value(in_v, prog) : "null", 
	    (in_v1) ? describe_xen_value(in_v1, prog) : "null", 
	    (in_v2) ? describe_xen_value(in_v2, prog) : "null");
    */
    /* val is the gen, v is the new value, in_v is the 1st index, in_v1 is 2nd index if it exists */
    if (val)
      {
	switch (val->type)
	  {
	  case R_VCT:
	  case R_FLOAT_VECTOR:
	    /* (let ((v (vct 1.0 2.0 3.0))) (run (lambda () (set! (v 1) 0.5)))) */
	    if ((!in_v) || (!v) ||
		(in_v->type != R_INT))
	      happy = 0;
	    else
	      {
		vct_set_1(prog, val, in_v, NULL, v);
		happy = 1;
	      }
	    break;

	  case R_SOUND_DATA:
	    /* (let ((sd (make-sound-data 2 2))) (run (lambda () (set! (sd 1 0) 1.0))) sd) */
	    if ((!in_v) || (!v) || (!in_v1) ||
		(in_v->type != R_INT) ||
		(in_v1->type != R_INT))
	      happy = 0;
	    else
	      {
		sound_data_set_1(prog, val, in_v, in_v1, v);
		happy = 1;
	      }
	    break;

	  case R_CLM:
	    /* (let ((mx (make-mixer 2))) (run (lambda () (set! (mx 0 1) 1.0))) mx) */
	    /* (let ((fr (make-frame 2))) (run (lambda () (set! (fr 1) 1.0))) fr) */
	    clm_set_1(prog, val, in_v, in_v1, v);
	    happy = 1;
	    break;

	  case R_INT_VECTOR:
	    /* (let ((v (vector 1 2 3))) (run (set! (v 1) 32) (v 1))) */
	    if ((!in_v) || (!v) ||
		(in_v->type != R_INT))
	      happy = 0;
	    else
	      {
		int_vector_set_1(prog, val, in_v, NULL, v);
		happy = 1;
	      }
	    break;

	  case R_LIST:
	    {
	      /* (let ((lst '(1 2 3))) (run (lambda () (set! (lst 1) 123) lst)))
	       */
	      if ((!in_v) || (!v) ||
		  (in_v->type != R_INT))
		happy = 0;
	      else
		{
		  xen_value **new_args;
		  new_args = (xen_value **)calloc(4, sizeof(xen_value *));
		  new_args[1] = val;
		  new_args[2] = in_v;
		  new_args[3] = v;
		  list_set_1(prog, new_args, 3);
		  free(new_args);
		  happy = 1;
		}
	    }
	    break;

	  case R_STRING:
	    {
	      /*
		(define str "12345")
		(run-eval `(set! (str 2) #\x))
		but this leaves the global untouched because we don't copy back!
		perhaps it's time to use s7 values directly (list-set! does get copied out)
	      */
	      if ((!in_v) || (!v) ||
		  (in_v->type != R_INT))
		happy = 0;
	      else
		{
		  xen_value **new_args;
		  new_args = (xen_value **)calloc(4, sizeof(xen_value *));
		  new_args[1] = val;
		  new_args[2] = in_v;
		  new_args[3] = v;
		  string_set_1(prog, new_args, 3);
		  free(new_args);
		  happy = 1;
		}
	    }
	    break;
	  }
	free(val);
      }
  }

  if (in_v) free(in_v);
  if (in_v1) free(in_v1);
  if (in_v2) free(in_v2);
  if (happy == 1) return(v);
  if (v) free(v);
  if (happy == 0) run_warn("can't set %s", s7_symbol_name(acc_form));

  return(NULL);
}


typedef enum {NO_PTREE_DISPLAY, STDERR_PTREE_DISPLAY, LISTENER_PTREE_DISPLAY, GCAT_PTREE_WITHOUT_DISPLAY} ptree_display_t;

#ifndef DESCRIBE_PTREE_INIT
  static ptree_display_t ptree_on = NO_PTREE_DISPLAY;
#else
  static ptree_display_t ptree_on = DESCRIBE_PTREE_INIT;
#endif


static s7_pointer g_show_ptree(s7_pointer on)
{
  #define S_show_ptree "show-ptree"
  #define H_show_ptree "(show-ptree arg): if arg is not 0, the optimizer's parse tree is displayed. \
arg = 1 sends it to stderr, 2 to the listener, 3 is for gcat's use. "

  ptree_on = (ptree_display_t)s7_number_to_integer(on);
  return(on);
}

static struct ptree *form_to_ptree_1(s7_pointer code, int decls, int *types)
{
  ptree *prog;
  s7_pointer form;
  run_warned = false;

#if USE_SND
  optimizing = (optimization(ss) > 0);
#endif
  if (!optimizing) return(NULL);
  if (code == scheme_nil) return(NULL);

  form = s7_car(code);
  prog = make_ptree(8);

  prog->lambda_args = decls;
  prog->lambda_arg_types = types;
  prog->code = code;
  if (s7_is_symbol(form))
    {
      mus_run_free_ptree(prog);
      return(NULL);
    }

  /* fprintf(stderr, "form->ptree: %s\n", s7_object_to_c_string(s7, form)); */

  prog->result = walk(prog, form, NEED_ANY_RESULT);
  if (prog->result)
    {
      add_triple_to_ptree(prog, make_triple(quit, "quit", NULL, 0));

      if (ptree_on != NO_PTREE_DISPLAY)
	{
	  char *msg;
	  msg = describe_ptree(prog, "  ");
	  if (ptree_on == STDERR_PTREE_DISPLAY) fprintf(stderr, "%s", msg);
#if USE_SND
	  else if (ptree_on == LISTENER_PTREE_DISPLAY) listener_append(msg);
#endif
	  free(msg);
	}

      prog->form = form;
      prog->form_loc = s7_gc_protect(s7, prog->form);
      return(prog);
    }

  mus_run_free_ptree(prog);
  if (!run_warned)
    {
      char *temp = NULL;
      run_warn("can't optimize: %s\n", temp = s7_object_to_c_string(s7, form));
      if (temp) free(temp);
    }
  return(NULL);
}


static struct ptree *form_to_ptree(s7_pointer code)
{
  return(form_to_ptree_1(code, 0, NULL));
}


static ptree *last_ptree = NULL, *last_error_ptree = NULL;

#if USE_SND
static XEN watch_for_mus_error_in_run(s7_scheme *sc, XEN args)
{
  /* called from snd.c if mus_error called, saw_mus_error set to 0 if throw (see above) */
  if (saw_mus_error == 1) 
    {
      saw_mus_error = 2;
      last_error_ptree = last_ptree;
    }
  return(XEN_FALSE); /* don't report the error at the hook level */
}
#endif


static s7_pointer eval_ptree_to_xen(ptree *pt)
{
  s7_pointer result = scheme_false;
  int gc_loc;

  if ((saw_mus_error == 2) && (last_ptree) && (last_ptree == last_error_ptree))
    mus_run_free_ptree(last_ptree);
  last_ptree = pt;
  last_error_ptree = NULL;
  saw_mus_error = 1;
#if USE_SND
  ss->C_g_typed = false;
#endif
  eval_ptree(pt);
#if USE_SND
  ss->C_g_typed = false;
#endif
  result = xen_value_to_xen(pt, pt->result);
  gc_loc = s7_gc_protect(s7, result);

  mus_run_free_ptree(pt);

  last_ptree = NULL;
  saw_mus_error = 0;

  s7_gc_unprotect_at(s7, gc_loc);
  return(result);
}


static void init_walkers(void)
{
  #define INIT_WALKER(Name, Val) scheme_set_walker((s7_pointer)(s7_make_symbol(s7, Name)), s7_make_c_pointer(s7, (void *)Val))

  walk_sym = s7_make_symbol(s7, "snd-walk");
  s7_gc_protect(s7, walk_sym);

  /* make_walker: walker, special walker, set walker, req args, max args, result type, need int, num arg types, ... */

  /* -------- special forms */
  INIT_WALKER("let",         make_walker(NULL, let_form, NULL, 2, UNLIMITED_ARGS, R_ANY, false, 0));
  INIT_WALKER("let*",        make_walker(NULL, let_star_form, NULL, 2, UNLIMITED_ARGS, R_ANY, false, 0));
  INIT_WALKER("do",          make_walker(NULL, do_form, NULL, 2, UNLIMITED_ARGS, R_ANY, false, 0));
  INIT_WALKER("begin",       make_walker(NULL, begin_form, NULL, 0, UNLIMITED_ARGS, R_ANY, false, 0));
  INIT_WALKER("if",          make_walker(NULL, if_form, NULL, 2, 3, R_ANY, false, 0));
  INIT_WALKER("cond",        make_walker(NULL, cond_form, NULL, 1, UNLIMITED_ARGS, R_ANY, false, 0));
  INIT_WALKER("case",        make_walker(NULL, case_form, NULL, 2, UNLIMITED_ARGS, R_ANY, false, 0));
  INIT_WALKER("call-with-current-continuation", make_walker(NULL, callcc_form, NULL, 1, 1, R_ANY, false, 0));
  INIT_WALKER("call-with-exit", make_walker(NULL, callcc_form, NULL, 1, 1, R_ANY, false, 0));
  INIT_WALKER("or",          make_walker(NULL, or_form, NULL, 0, UNLIMITED_ARGS, R_ANY, false, 0));
  INIT_WALKER("and",         make_walker(NULL, and_form, NULL, 0, UNLIMITED_ARGS, R_ANY, false, 0));
  INIT_WALKER("set!",        make_walker(NULL, set_form, NULL, 2, 2, R_ANY, false, 0)); /* Scheme set! does not take &rest args */
  INIT_WALKER("call/cc",     make_walker(NULL, callcc_form, NULL, 1, 1, R_ANY, false, 0));
  INIT_WALKER("lambda",      make_walker(NULL, lambda_preform, NULL, 1, UNLIMITED_ARGS, R_ANY, false, 0));
  INIT_WALKER("declare",     make_walker(NULL, declare_1, NULL, 0, UNLIMITED_ARGS, R_ANY, false, 0));
  INIT_WALKER("snd-declare", make_walker(NULL, declare_1, NULL, 0, UNLIMITED_ARGS, R_ANY, false, 0));

  /* -------- basic stuff */
  INIT_WALKER("eq?",      make_walker(eq_p, NULL, NULL, 2, 2, R_BOOL, false, 0));
  INIT_WALKER("eqv?",     make_walker(eqv_p, NULL, NULL, 2, 2, R_BOOL, false, 0));
  INIT_WALKER("equal?",   make_walker(equal_p, NULL, NULL, 2, 2, R_BOOL, false, 0));
  INIT_WALKER("boolean?", make_walker(boolean_p_1, NULL, NULL, 1, 1, R_BOOL, false, 0));
  INIT_WALKER("number?",  make_walker(number_p_1, NULL, NULL, 1, 1, R_BOOL, false, 0));
  INIT_WALKER("integer?", make_walker(integer_p_1, NULL, NULL, 1, 1, R_BOOL, false, 0));
  INIT_WALKER("inexact?", make_walker(inexact_p_1, NULL, NULL, 1, 1, R_BOOL, false, 0));
  INIT_WALKER("exact?",   make_walker(exact_p_1, NULL, NULL, 1, 1, R_BOOL, false, 0));
  INIT_WALKER("real?",    make_walker(real_p_1, NULL, NULL, 1, 1, R_BOOL, false, 0));
  INIT_WALKER("char?",    make_walker(char_p_1, NULL, NULL, 1, 1, R_BOOL, false, 0));
  INIT_WALKER("string?",  make_walker(string_p_1, NULL, NULL, 1, 1, R_BOOL, false, 0));
  INIT_WALKER("keyword?", make_walker(keyword_p_1, NULL, NULL, 1, 1, R_BOOL, false, 0));
  INIT_WALKER("symbol?",  make_walker(symbol_p_1, NULL, NULL, 1, 1, R_BOOL, false, 0));
  INIT_WALKER("vector?",  make_walker(vector_p_1, NULL, NULL, 1, 1, R_BOOL, false, 0));
  INIT_WALKER("list?",    make_walker(list_p_1, NULL, NULL, 1, 1, R_BOOL, false, 0));
  INIT_WALKER(">=",       make_walker(ge_1, NULL, NULL, 0, UNLIMITED_ARGS, R_BOOL, false, 1, -R_NUMBER));
  INIT_WALKER(">",        make_walker(gt_1, NULL, NULL, 0, UNLIMITED_ARGS, R_BOOL, false, 1, -R_NUMBER));
  INIT_WALKER("<=",       make_walker(le_1, NULL, NULL, 0, UNLIMITED_ARGS, R_BOOL, false, 1, -R_NUMBER));
  INIT_WALKER("<",        make_walker(lt_1, NULL, NULL, 0, UNLIMITED_ARGS, R_BOOL, false, 1, -R_NUMBER));
  INIT_WALKER("=",        make_walker(eq_1, NULL, NULL, 0, UNLIMITED_ARGS, R_BOOL, false, 1, -R_NUMBER));

  INIT_WALKER("*",        make_walker(multiply, NULL, NULL, 0, UNLIMITED_ARGS, R_ANY, false, 1, -R_NUMBER));
  INIT_WALKER("+",        make_walker(add, NULL, NULL, 0, UNLIMITED_ARGS, R_NUMBER, false, 1, -R_NUMBER));
  INIT_WALKER("-",        make_walker(subtract, NULL, NULL, 1, UNLIMITED_ARGS, R_NUMBER, false, 1, -R_NUMBER));
  INIT_WALKER("/",        make_walker(divide, NULL, NULL, 1, UNLIMITED_ARGS, R_NUMBER, false, 1, -R_NUMBER));
  INIT_WALKER("max",      make_walker(max_1, NULL, NULL, 1, UNLIMITED_ARGS, R_NUMBER, false, 1, -R_NUMBER));
  INIT_WALKER("min",      make_walker(min_1, NULL, NULL, 1, UNLIMITED_ARGS, R_NUMBER, false, 1, -R_NUMBER));
  INIT_WALKER("1+",       make_walker(one_plus, NULL, NULL, 1, 1, R_NUMBER, false, 1, R_NUMBER));
  INIT_WALKER("1-",       make_walker(one_minus, NULL, NULL, 1, 1, R_NUMBER, false, 1, R_NUMBER));

  INIT_WALKER("inexact->exact", make_walker(inexact2exact_1, NULL, NULL, 1, 1, R_INT, true, 1, R_NUMBER));
  INIT_WALKER("exact->inexact", make_walker(exact2inexact_1, NULL, NULL, 1, 1, R_FLOAT, false, 1, R_NUMBER));
  INIT_WALKER("modulo",         make_walker(modulo_1, NULL, NULL, 2, 2, R_INT, true, 2, R_NUMBER, R_NUMBER));
  INIT_WALKER("remainder",      make_walker(remainder_1, NULL, NULL, 2, 2, R_INT, true, 2, R_NUMBER, R_NUMBER));
  INIT_WALKER("quotient",       make_walker(quotient_1, NULL, NULL, 2, 2, R_INT, true, 2, R_NUMBER, R_NUMBER));
  INIT_WALKER("logand",         make_walker(logand_1, NULL, NULL, 2, 2, R_INT, true, 2, R_INT, R_INT));
  INIT_WALKER("logxor",         make_walker(logxor_1, NULL, NULL, 2, 2, R_INT, true, 2, R_INT, R_INT));
  INIT_WALKER("logior",         make_walker(logior_1, NULL, NULL, 2, 2, R_INT, true, 2, R_INT, R_INT));
  INIT_WALKER("lognot",         make_walker(lognot_1, NULL, NULL, 1, 1, R_INT, true, 1, R_INT));
  INIT_WALKER("ash",            make_walker(ash_1, NULL, NULL, 2, 2, R_INT, true, 2, R_INT, R_INT));
  INIT_WALKER("integer->char",  make_walker(integer_to_char_1, NULL, NULL, 1, 1, R_CHAR, true, 1, R_INT));
  INIT_WALKER("gcd",            make_walker(gcd_1, NULL, NULL, 0, UNLIMITED_ARGS, R_INT, true, 1, -R_NUMBER));
  INIT_WALKER("lcm",            make_walker(lcm_1, NULL, NULL, 0, UNLIMITED_ARGS, R_INT, true, 1, -R_NUMBER));
  INIT_WALKER("expt",           make_walker(expt_1, NULL, NULL, 2, 2, R_NUMBER, false, 2, R_NUMBER, R_NUMBER));
  INIT_WALKER("atan",           make_walker(atanx_1, NULL, NULL, 1, 2, R_NUMBER, false, 2, R_NUMBER, R_NUMBER));
  INIT_WALKER("sin",            make_walker(sin_1, NULL, NULL, 1, 1, R_FLOAT, false, 1, R_NUMBER));
  INIT_WALKER("cos",            make_walker(cos_1, NULL, NULL, 1, 1, R_FLOAT, false, 1, R_NUMBER));
  INIT_WALKER("tan",            make_walker(tan_1, NULL, NULL, 1, 1, R_FLOAT, false, 1, R_NUMBER));
  INIT_WALKER("abs",            make_walker(abs_1, NULL, NULL, 1, 1, R_NUMBER, false, 1, R_NUMBER));
  INIT_WALKER("random",         make_walker(random_1, NULL, NULL, 1, 1, R_NUMBER, false, 1, R_NUMBER));
  INIT_WALKER("log",            make_walker(log_1, NULL, NULL, 1, 1, R_FLOAT, false, 1, R_NUMBER));
  INIT_WALKER("exp",            make_walker(exp_1, NULL, NULL, 1, 1, R_FLOAT, false, 1, R_NUMBER));
  INIT_WALKER("asin",           make_walker(asin_1, NULL, NULL, 1, 1, R_FLOAT, false, 1, R_NUMBER));
  INIT_WALKER("acos",           make_walker(acos_1, NULL, NULL, 1, 1, R_FLOAT, false, 1, R_NUMBER));
  INIT_WALKER("sqrt",           make_walker(sqrt_1, NULL, NULL, 1, 1, R_FLOAT, false, 1, R_NUMBER));
  INIT_WALKER("cosh",           make_walker(cosh_1, NULL, NULL, 1, 1, R_FLOAT, false, 1, R_NUMBER));
  INIT_WALKER("sinh",           make_walker(sinh_1, NULL, NULL, 1, 1, R_FLOAT, false, 1, R_NUMBER));
  INIT_WALKER("tanh",           make_walker(tanh_1, NULL, NULL, 1, 1, R_FLOAT, false, 1, R_NUMBER));
  INIT_WALKER("acosh",          make_walker(acosh_1, NULL, NULL, 1, 1, R_FLOAT, false, 1, R_NUMBER));
  INIT_WALKER("asinh",          make_walker(asinh_1, NULL, NULL, 1, 1, R_FLOAT, false, 1, R_NUMBER));
  INIT_WALKER("atanh",          make_walker(atanh_1, NULL, NULL, 1, 1, R_FLOAT, false, 1, R_NUMBER));
  INIT_WALKER("bes-i0",         make_walker(mus_bessi0_1, NULL, NULL, 1, 1, R_FLOAT, false, 1, R_NUMBER));
  INIT_WALKER("fmod",           make_walker(fmod_1, NULL, NULL, 2, 2, R_FLOAT, false, 2, R_NUMBER, R_NUMBER));
#if HAVE_SPECIAL_FUNCTIONS
  INIT_WALKER("bes-j0",         make_walker(j0_1, NULL, NULL, 1, 1, R_FLOAT, false, 1, R_NUMBER));
  INIT_WALKER("bes-j1",         make_walker(j1_1, NULL, NULL, 1, 1, R_FLOAT, false, 1, R_NUMBER));
  INIT_WALKER("bes-jn",         make_walker(jn_1, NULL, NULL, 2, 2, R_FLOAT, false, 2, R_INT, R_NUMBER));
  INIT_WALKER("bes-y0",         make_walker(y0_1, NULL, NULL, 1, 1, R_FLOAT, false, 1, R_NUMBER));
  INIT_WALKER("bes-y1",         make_walker(y1_1, NULL, NULL, 1, 1, R_FLOAT, false, 1, R_NUMBER));
  INIT_WALKER("bes-yn",         make_walker(yn_1, NULL, NULL, 2, 2, R_FLOAT, false, 2, R_INT, R_NUMBER));
  INIT_WALKER("erf",            make_walker(erf_1, NULL, NULL, 1, 1, R_FLOAT, false, 1, R_NUMBER));
  INIT_WALKER("erfc",           make_walker(erfc_1, NULL, NULL, 1, 1, R_FLOAT, false, 1, R_NUMBER));
  INIT_WALKER("lgamma",         make_walker(lgamma_1, NULL, NULL, 1, 1, R_FLOAT, false, 1, R_NUMBER));
#endif

  INIT_WALKER("round",          make_walker(round_1, NULL, NULL, 1, 1, R_INT, false, 1, R_NUMBER));
  INIT_WALKER("truncate",       make_walker(truncate_1, NULL, NULL, 1, 1, R_INT, false, 1, R_NUMBER));
  INIT_WALKER("floor",          make_walker(floor_1, NULL, NULL, 1, 1, R_INT, false, 1, R_NUMBER));
  INIT_WALKER("ceiling",        make_walker(ceiling_1, NULL, NULL, 1, 1, R_INT, false, 1, R_NUMBER));
  INIT_WALKER("odd?",           make_walker(odd_p, NULL, NULL, 1, 1, R_BOOL, false, 1, R_NUMBER));
  INIT_WALKER("even?",          make_walker(even_p, NULL, NULL, 1, 1, R_BOOL, false, 1, R_NUMBER));
  INIT_WALKER("zero?",          make_walker(zero_p, NULL, NULL, 1, 1, R_BOOL, false, 1, R_NUMBER));
  INIT_WALKER("positive?",      make_walker(positive_p, NULL, NULL, 1, 1, R_BOOL, false, 1, R_NUMBER));
  INIT_WALKER("negative?",      make_walker(negative_p, NULL, NULL, 1, 1, R_BOOL, false, 1, R_NUMBER));
  INIT_WALKER("not",            make_walker(not_p, NULL, NULL, 1, 1, R_BOOL, false, 0)); /* ?? */

  INIT_WALKER("throw",          make_walker(throw_1, NULL, NULL, 1, 1, R_BOOL, false, 1, R_SYMBOL));

  /* -------- char funcs */
  INIT_WALKER("char-alphabetic?", make_walker(char_alphabetic_p, NULL, NULL, 1, 1, R_BOOL, false, 1, R_CHAR));
  INIT_WALKER("char-numeric?",    make_walker(char_numeric_p, NULL, NULL, 1, 1, R_BOOL, false, 1, R_CHAR));
  INIT_WALKER("char-lower-case?", make_walker(char_lower_case_p, NULL, NULL, 1, 1, R_BOOL, false, 1, R_CHAR));
  INIT_WALKER("char-upper-case?", make_walker(char_upper_case_p, NULL, NULL, 1, 1, R_BOOL, false, 1, R_CHAR));
  INIT_WALKER("char-whitespace?", make_walker(char_whitespace_p, NULL, NULL, 1, 1, R_BOOL, false, 1, R_CHAR));
  INIT_WALKER("char-upcase",      make_walker(char_upcase, NULL, NULL, 1, 1, R_CHAR, false, 1, R_CHAR));
  INIT_WALKER("char-downcase",    make_walker(char_downcase, NULL, NULL, 1, 1, R_CHAR, false, 1, R_CHAR));
  INIT_WALKER("char->integer",    make_walker(char_to_integer, NULL, NULL, 1, 1, R_INT, false, 1, R_CHAR));
  INIT_WALKER("char>=?",          make_walker(char_ge_1, NULL, NULL, 0, UNLIMITED_ARGS, R_BOOL, false, 1, -R_CHAR));
  INIT_WALKER("char>?",           make_walker(char_gt_1, NULL, NULL, 0, UNLIMITED_ARGS, R_BOOL, false, 1, -R_CHAR));
  INIT_WALKER("char<=?",          make_walker(char_le_1, NULL, NULL, 0, UNLIMITED_ARGS, R_BOOL, false, 1, -R_CHAR));
  INIT_WALKER("char<?",           make_walker(char_lt_1, NULL, NULL, 0, UNLIMITED_ARGS, R_BOOL, false, 1, -R_CHAR));
  INIT_WALKER("char=?",           make_walker(char_eq_1, NULL, NULL, 0, UNLIMITED_ARGS, R_BOOL, false, 1, -R_CHAR));
  INIT_WALKER("char-ci>=?",       make_walker(char_ci_ge_1, NULL, NULL, 0, UNLIMITED_ARGS, R_BOOL, false, 1, -R_CHAR));
  INIT_WALKER("char-ci>?",        make_walker(char_ci_gt_1, NULL, NULL, 0, UNLIMITED_ARGS, R_BOOL, false, 1, -R_CHAR));
  INIT_WALKER("char-ci<=?",       make_walker(char_ci_le_1, NULL, NULL, 0, UNLIMITED_ARGS, R_BOOL, false, 1, -R_CHAR));
  INIT_WALKER("char-ci<?",        make_walker(char_ci_lt_1, NULL, NULL, 0, UNLIMITED_ARGS, R_BOOL, false, 1, -R_CHAR));
  INIT_WALKER("char-ci=?",        make_walker(char_ci_eq_1, NULL, NULL, 0, UNLIMITED_ARGS, R_BOOL, false, 1, -R_CHAR));
  INIT_WALKER("string",           make_walker(string_1, NULL, NULL, 0, UNLIMITED_ARGS, R_STRING, false, 1, -R_CHAR));

  /* -------- string funcs */
  INIT_WALKER("string=?",      make_walker(string_eq_1, NULL, NULL, 0, UNLIMITED_ARGS, R_BOOL, false, 1, -R_STRING));
  INIT_WALKER("string>=?",     make_walker(string_geq_1, NULL, NULL, 0, UNLIMITED_ARGS, R_BOOL, false, 1, -R_STRING));
  INIT_WALKER("string<=?",     make_walker(string_leq_1, NULL, NULL, 0, UNLIMITED_ARGS, R_BOOL, false, 1, -R_STRING));
  INIT_WALKER("string>?",      make_walker(string_gt_1, NULL, NULL, 0, UNLIMITED_ARGS, R_BOOL, false, 1, -R_STRING));
  INIT_WALKER("string<?",      make_walker(string_lt_1, NULL, NULL, 0, UNLIMITED_ARGS, R_BOOL, false, 1, -R_STRING));
  INIT_WALKER("string-ci=?",   make_walker(string_ci_eq_1, NULL, NULL, 0, UNLIMITED_ARGS, R_BOOL, false, 1, -R_STRING));
  INIT_WALKER("string-ci>=?",  make_walker(string_ci_geq_1, NULL, NULL, 0, UNLIMITED_ARGS, R_BOOL, false, 1, -R_STRING));
  INIT_WALKER("string-ci<=?",  make_walker(string_ci_leq_1, NULL, NULL, 0, UNLIMITED_ARGS, R_BOOL, false, 1, -R_STRING));
  INIT_WALKER("string-ci>?",   make_walker(string_ci_gt_1, NULL, NULL, 0, UNLIMITED_ARGS, R_BOOL, false, 1, -R_STRING));
  INIT_WALKER("string-ci<?",   make_walker(string_ci_lt_1, NULL, NULL, 0, UNLIMITED_ARGS, R_BOOL, false, 1, -R_STRING));
  INIT_WALKER("string-length", make_walker(string_length_1, NULL, NULL, 1, 1, R_INT, false, 1, R_STRING));
  INIT_WALKER("string-copy",   make_walker(string_copy_1, NULL, NULL, 1, 1, R_STRING, false, 1, R_STRING));
  INIT_WALKER("string-ref",    make_walker(r_string_ref_1, NULL, NULL, 2, 2, R_CHAR, false, 2, R_STRING, R_INT));
  INIT_WALKER("substring",     make_walker(substring_1, NULL, NULL, 3, 3, R_STRING, false, 3, R_STRING, R_INT, R_INT));
  INIT_WALKER("string-fill!",  make_walker(string_fill_1, NULL, NULL, 2, 2, R_STRING, false, 2, R_STRING, R_CHAR));
  INIT_WALKER("string-set!",   make_walker(string_set_1, NULL, NULL, 3, 3, R_STRING, false, 3, R_STRING, R_INT, R_CHAR));
  INIT_WALKER("string-append", make_walker(string_append_1, NULL, NULL, 0, UNLIMITED_ARGS, R_STRING, false, 1, -R_STRING));

  INIT_WALKER("display",        make_walker(display_1, NULL, NULL, 1, 1, R_STRING, false, 0));
  INIT_WALKER("make-string",    make_walker(make_string_1, NULL, NULL, 1, 2, R_STRING, false, 2, R_INT, R_CHAR));
  INIT_WALKER("number->string", make_walker(number2string_1, NULL, NULL, 1, 2, R_STRING, false, 2, R_NUMBER, R_INT));
  INIT_WALKER("symbol->string", make_walker(symbol2string_1, NULL, NULL, 1, 1, R_STRING, false, 1, R_SYMBOL));

  /* -------- vector funcs */
  INIT_WALKER("vector-ref",    make_walker(r_vector_ref_1, NULL, NULL, 2, 2, R_ANY, false, 2, R_VECTOR, R_INT));
  INIT_WALKER("vector-length", make_walker(vector_length_1, NULL, NULL, 1, 1, R_INT, false, 1, R_VECTOR));
  INIT_WALKER("vector-fill!",  make_walker(vector_fill_1, NULL, NULL, 2, 2, R_INT, false, 1, R_VECTOR));
  INIT_WALKER("vector-set!",   make_walker(r_vector_set_1, NULL, NULL, 3, 3, R_ANY, false, 2, R_VECTOR, R_INT));
  INIT_WALKER("make-vector",   make_walker(r_make_vector_1, NULL, NULL, 1, 2, R_ANY, false, 2, R_INT, R_NUMBER));
  INIT_WALKER(S_vct_to_vector, make_walker(vct_copy_1, NULL, NULL, 1, 1, R_VCT, false, 1, R_VCT));
  INIT_WALKER(S_vector_to_vct, make_walker(vct_copy_1, NULL, NULL, 1, 1, R_VCT, false, 1, R_VECTOR));

  /* -------- list funcs */
  INIT_WALKER("list-ref",  make_walker(r_list_ref_1, NULL, NULL, 2, 2, R_ANY, false, 2, R_LIST, R_INT));
  INIT_WALKER("list-set!", make_walker(list_set_1, NULL, NULL, 3, 3, R_ANY, false, 2, R_LIST, R_INT));
  INIT_WALKER("car",       make_walker(car_1, NULL, NULL, 1, 1, R_ANY, false, 1, R_LIST));
  INIT_WALKER("cadr",      make_walker(cadr_1, NULL, NULL, 1, 1, R_ANY, false, 1, R_LIST));
  INIT_WALKER("caddr",     make_walker(caddr_1, NULL, NULL, 1, 1, R_ANY, false, 1, R_LIST));
  INIT_WALKER("cadddr",    make_walker(cadddr_1, NULL, NULL, 1, 1, R_ANY, false, 1, R_LIST));
  INIT_WALKER("set-car!",  make_walker(set_car_1, NULL, NULL, 2, 2, R_ANY, false, 1, R_LIST));
  INIT_WALKER("length",    make_walker(length_1, NULL, NULL, 1, 1, R_INT, false, 1, R_ANY));
  INIT_WALKER("null?",     make_walker(null_1, NULL, NULL, 1, 1, R_BOOL, false, 1, R_LIST));
  INIT_WALKER("quote",     make_walker(NULL, quote_form, NULL, 1, 1, R_ANY, false, 0));

  INIT_WALKER("format",    make_walker(format_1, NULL, NULL, 0, UNLIMITED_ARGS, R_STRING, false, 1, -R_XEN));
  INIT_WALKER("clm-print", make_walker(clm_print_1, NULL, NULL, 0, UNLIMITED_ARGS, R_STRING, false, 1, -R_XEN));

  INIT_WALKER("open-output-file",  make_walker(open_output_file_1, NULL, NULL, 1, 2, R_XEN, false, 1, -R_STRING));
  INIT_WALKER("close-output-port", make_walker(close_output_port_1, NULL, NULL, 1, 1, R_BOOL, false, 1, R_XEN));
  INIT_WALKER("close-input-port",  make_walker(close_input_port_1, NULL, NULL, 1, 1, R_BOOL, false, 1, R_XEN));
  INIT_WALKER("output-port?",      make_walker(is_output_port_1, NULL, NULL, 1, 1, R_BOOL, false, 1, R_XEN));
  INIT_WALKER("input-port?",       make_walker(is_input_port_1, NULL, NULL, 1, 1, R_BOOL, false, 1, R_XEN));
  INIT_WALKER("file-exists?",      make_walker(file_exists_p_1, NULL, NULL, 1, 1, R_BOOL, false, 1, R_STRING));
  INIT_WALKER("getpid",            make_walker(getpid_1, NULL, NULL, 0, 0, R_INT, false, 0));

  INIT_WALKER("current-output-port", make_walker(current_output_port_1, NULL, NULL, 0, 0, R_XEN, false, 0));
  INIT_WALKER("current-input-port",  make_walker(current_input_port_1, NULL, NULL, 0, 0, R_XEN, false, 0));
  INIT_WALKER("current-error-port",  make_walker(current_error_port_1, NULL, NULL, 0, 0, R_XEN, false, 0));

  INIT_WALKER("s7-version", make_walker(s7_version_1, NULL, NULL, 0, 0, R_STRING, false, 0));


  /* -------- clm funcs */
  INIT_WALKER(S_mus_generator_p,  make_walker(mus_generator_p_1, NULL, NULL, 1, 1, R_BOOL, false, 1, R_ANY));
  INIT_WALKER(S_oscil_p,          make_walker(oscil_p, NULL, NULL, 1, 1, R_BOOL, false, 1, R_ANY));
  INIT_WALKER(S_env_p,            make_walker(env_p, NULL, NULL, 1, 1, R_BOOL, false, 1, R_ANY));
  INIT_WALKER(S_notch_p,          make_walker(notch_p, NULL, NULL, 1, 1, R_BOOL, false, 1, R_ANY));
  INIT_WALKER(S_comb_p,           make_walker(comb_p, NULL, NULL, 1, 1, R_BOOL, false, 1, R_ANY));
  INIT_WALKER(S_filtered_comb_p,  make_walker(filtered_comb_p, NULL, NULL, 1, 1, R_BOOL, false, 1, R_ANY));
  INIT_WALKER(S_delay_p,          make_walker(delay_p, NULL, NULL, 1, 1, R_BOOL, false, 1, R_ANY));
  INIT_WALKER(S_all_pass_p,       make_walker(all_pass_p, NULL, NULL, 1, 1, R_BOOL, false, 1, R_ANY));
  INIT_WALKER(S_moving_average_p, make_walker(moving_average_p, NULL, NULL, 1, 1, R_BOOL, false, 1, R_ANY));
  INIT_WALKER(S_rand_p,           make_walker(rand_p, NULL, NULL, 1, 1, R_BOOL, false, 1, R_ANY));
  INIT_WALKER(S_rand_interp_p,    make_walker(rand_interp_p, NULL, NULL, 1, 1, R_BOOL, false, 1, R_ANY));
  INIT_WALKER(S_ncos_p,           make_walker(ncos_p, NULL, NULL, 1, 1, R_BOOL, false, 1, R_ANY));
  INIT_WALKER(S_ssb_am_p,         make_walker(ssb_am_p, NULL, NULL, 1, 1, R_BOOL, false, 1, R_ANY));
  INIT_WALKER(S_nsin_p,           make_walker(nsin_p, NULL, NULL, 1, 1, R_BOOL, false, 1, R_ANY));
  INIT_WALKER(S_table_lookup_p,   make_walker(table_lookup_p, NULL, NULL, 1, 1, R_BOOL, false, 1, R_ANY));
  INIT_WALKER(S_sawtooth_wave_p,  make_walker(sawtooth_wave_p, NULL, NULL, 1, 1, R_BOOL, false, 1, R_ANY));
  INIT_WALKER(S_pulse_train_p,    make_walker(pulse_train_p, NULL, NULL, 1, 1, R_BOOL, false, 1, R_ANY));
  INIT_WALKER(S_square_wave_p,    make_walker(square_wave_p, NULL, NULL, 1, 1, R_BOOL, false, 1, R_ANY));
  INIT_WALKER(S_triangle_wave_p,  make_walker(triangle_wave_p, NULL, NULL, 1, 1, R_BOOL, false, 1, R_ANY));
  INIT_WALKER(S_asymmetric_fm_p,  make_walker(asymmetric_fm_p, NULL, NULL, 1, 1, R_BOOL, false, 1, R_ANY));
  INIT_WALKER(S_nrxysin_p,        make_walker(nrxysin_p, NULL, NULL, 1, 1, R_BOOL, false, 1, R_ANY));
  INIT_WALKER(S_nrxycos_p,        make_walker(nrxycos_p, NULL, NULL, 1, 1, R_BOOL, false, 1, R_ANY));
  INIT_WALKER(S_one_zero_p,       make_walker(one_zero_p, NULL, NULL, 1, 1, R_BOOL, false, 1, R_ANY));
  INIT_WALKER(S_one_pole_p,       make_walker(one_pole_p, NULL, NULL, 1, 1, R_BOOL, false, 1, R_ANY));
  INIT_WALKER(S_two_zero_p,       make_walker(two_zero_p, NULL, NULL, 1, 1, R_BOOL, false, 1, R_ANY));
  INIT_WALKER(S_two_pole_p,       make_walker(two_pole_p, NULL, NULL, 1, 1, R_BOOL, false, 1, R_ANY));
  INIT_WALKER(S_firmant_p,        make_walker(firmant_p, NULL, NULL, 1, 1, R_BOOL, false, 1, R_ANY));
  INIT_WALKER(S_formant_p,        make_walker(formant_p, NULL, NULL, 1, 1, R_BOOL, false, 1, R_ANY));
  INIT_WALKER(S_wave_train_p,     make_walker(wave_train_p, NULL, NULL, 1, 1, R_BOOL, false, 1, R_ANY));
  INIT_WALKER(S_polyshape_p,      make_walker(polyshape_p, NULL, NULL, 1, 1, R_BOOL, false, 1, R_ANY));
  INIT_WALKER(S_polywave_p,       make_walker(polywave_p, NULL, NULL, 1, 1, R_BOOL, false, 1, R_ANY));
  INIT_WALKER(S_filter_p,         make_walker(filter_p, NULL, NULL, 1, 1, R_BOOL, false, 1, R_ANY));
  INIT_WALKER(S_fir_filter_p,     make_walker(fir_filter_p, NULL, NULL, 1, 1, R_BOOL, false, 1, R_ANY));
  INIT_WALKER(S_iir_filter_p,     make_walker(iir_filter_p, NULL, NULL, 1, 1, R_BOOL, false, 1, R_ANY));
  INIT_WALKER(S_readin_p,         make_walker(readin_p, NULL, NULL, 1, 1, R_BOOL, false, 1, R_ANY));
  INIT_WALKER(S_src_p,            make_walker(src_p, NULL, NULL, 1, 1, R_BOOL, false, 1, R_ANY));
  INIT_WALKER(S_granulate_p,      make_walker(granulate_p, NULL, NULL, 1, 1, R_BOOL, false, 1, R_ANY));
  INIT_WALKER(S_phase_vocoder_p,  make_walker(phase_vocoder_p, NULL, NULL, 1, 1, R_BOOL, false, 1, R_ANY));
  INIT_WALKER(S_convolve_p,       make_walker(convolve_p, NULL, NULL, 1, 1, R_BOOL, false, 1, R_ANY));
  INIT_WALKER(S_frame_p,          make_walker(frame_p, NULL, NULL, 1, 1, R_BOOL, false, 1, R_ANY));
  INIT_WALKER(S_mixer_p,          make_walker(mixer_p, NULL, NULL, 1, 1, R_BOOL, false, 1, R_ANY));
  INIT_WALKER(S_file_to_sample_p, make_walker(file_to_sample_p, NULL, NULL, 1, 1, R_BOOL, false, 1, R_ANY));
  INIT_WALKER(S_sample_to_file_p, make_walker(sample_to_file_p, NULL, NULL, 1, 1, R_BOOL, false, 1, R_ANY));
  INIT_WALKER(S_file_to_frame_p,  make_walker(file_to_frame_p, NULL, NULL, 1, 1, R_BOOL, false, 1, R_ANY));
  INIT_WALKER(S_frame_to_file_p,  make_walker(frame_to_file_p, NULL, NULL, 1, 1, R_BOOL, false, 1, R_ANY));
  INIT_WALKER(S_locsig_p,         make_walker(locsig_p, NULL, NULL, 1, 1, R_BOOL, false, 1, R_ANY));
  INIT_WALKER(S_move_sound_p,     make_walker(move_sound_p, NULL, NULL, 1, 1, R_BOOL, false, 1, R_ANY));
  INIT_WALKER(S_mus_input_p,      make_walker(input_p, NULL, NULL, 1, 1, R_BOOL, false, 1, R_ANY));
  INIT_WALKER(S_mus_output_p,     make_walker(output_p, NULL, NULL, 1, 1, R_BOOL, false, 1, R_ANY));
#if USE_SND
  INIT_WALKER(S_snd_to_sample_p,  make_walker(snd_to_sample_1p, NULL, NULL, 1, 1, R_BOOL, false, 1, R_ANY));
#endif

  INIT_WALKER(S_mus_increment,      make_walker(mus_increment_0, NULL, mus_set_increment_1, 1, 1, R_FLOAT, false, 1, R_GENERATOR));
  INIT_WALKER(S_mus_frequency,      make_walker(mus_frequency_0, NULL, mus_set_frequency_1, 1, 1, R_FLOAT, false, 1, R_GENERATOR));
  INIT_WALKER(S_mus_phase,          make_walker(mus_phase_0, NULL, mus_set_phase_1, 1, 1, R_FLOAT, false, 1, R_GENERATOR));
  INIT_WALKER(S_mus_width,          make_walker(mus_width_0, NULL, mus_set_width_1, 1, 1, R_FLOAT, false, 1, R_GENERATOR));
  INIT_WALKER(S_mus_scaler,         make_walker(mus_scaler_0, NULL, mus_set_scaler_1, 1, 1, R_FLOAT, false, 1, R_GENERATOR));
  INIT_WALKER(S_mus_safety,         make_walker(mus_safety_0, NULL, mus_set_safety_1, 1, 1, R_INT, false, 1, R_GENERATOR));
  INIT_WALKER(S_mus_reset,          make_walker(mus_reset_0, NULL, NULL, 1, 1, R_CLM, false, 1, R_GENERATOR));
  INIT_WALKER(S_mus_offset,         make_walker(mus_offset_0, NULL, mus_set_offset_1, 1, 1, R_FLOAT, false, 1, R_GENERATOR));
  INIT_WALKER("mus-formant-radius", make_walker(mus_scaler_0, NULL, mus_set_scaler_1, 1, 1, R_FLOAT, false, 1, R_GENERATOR)); /* backwards compatibility... */
  INIT_WALKER(S_mus_data,           make_walker(mus_data_1, NULL, NULL, 1, 1, R_VCT, false, 1, R_GENERATOR));
  INIT_WALKER(S_mus_xcoeffs,        make_walker(mus_xcoeffs_1, NULL, NULL, 1, 1, R_VCT, false, 1, R_GENERATOR)); 
  INIT_WALKER(S_mus_ycoeffs,        make_walker(mus_ycoeffs_1, NULL, NULL, 1, 1, R_VCT, false, 1, R_GENERATOR));
  INIT_WALKER(S_mus_xcoeff,         make_walker(mus_xcoeff_1, NULL, mus_set_xcoeff_1, 2, 2, R_FLOAT, false, 2, R_GENERATOR, R_INT));
  INIT_WALKER(S_mus_ycoeff,         make_walker(mus_ycoeff_1, NULL, mus_set_ycoeff_1, 2, 2, R_FLOAT, false, 2, R_GENERATOR, R_INT));
  INIT_WALKER(S_mus_feedforward,    make_walker(mus_feedforward_0, NULL, mus_set_feedforward_1, 1, 1, R_FLOAT, false, 1, R_GENERATOR));
  INIT_WALKER(S_mus_feedback,       make_walker(mus_feedback_0, NULL, mus_set_feedback_1, 1, 1, R_FLOAT, false, 1, R_GENERATOR));
  INIT_WALKER(S_mus_hop,            make_walker(mus_hop_0, NULL, mus_set_hop_1, 1, 1, R_INT, false, 1, R_GENERATOR));
  INIT_WALKER(S_mus_channels,       make_walker(mus_channels_0, NULL, NULL, 1, 1, R_INT, false, 1, R_ANY));        /* *output* can be a vct or sound-data object */
  INIT_WALKER(S_mus_channel,        make_walker(mus_channel_0, NULL, NULL, 1, 1, R_INT, false, 1, R_GENERATOR));
  INIT_WALKER(S_mus_location,       make_walker(mus_location_0, NULL, mus_set_location_1, 1, 1, R_INT, false, 1, R_GENERATOR));
  INIT_WALKER(S_mus_ramp,           make_walker(mus_ramp_0, NULL, mus_set_ramp_1, 1, 1, R_INT, false, 1, R_GENERATOR));
  INIT_WALKER(S_mus_order,          make_walker(mus_order_0, NULL, NULL, 1, 1, R_INT, false, 1, R_GENERATOR));
  INIT_WALKER(S_mus_length,         make_walker(mus_length_0, NULL, mus_set_length_1, 1, 1, R_INT, false, 1, R_GENERATOR));
  INIT_WALKER(S_mus_name,           make_walker(mus_name_0, NULL, mus_set_name_1, 1, 1, R_STRING, false, 1, R_GENERATOR));
  INIT_WALKER(S_mus_file_name,      make_walker(mus_file_name_0, NULL, NULL, 1, 1, R_STRING, false, 1, R_GENERATOR));
  INIT_WALKER(S_mus_describe,       make_walker(mus_describe_0, NULL, NULL, 1, 1, R_STRING, false, 1, R_GENERATOR));
  INIT_WALKER(S_mus_close,          make_walker(mus_close_0, NULL, NULL, 1, 1, R_INT, false, 1, R_ANY));              /* *output* again */
  INIT_WALKER(S_mus_run,            make_walker(mus_run_1, NULL, NULL, 1, 3, R_FLOAT, false, 3, R_GENERATOR, R_NUMBER, R_NUMBER));

  INIT_WALKER(S_oscil,          make_walker(oscil_1, NULL, NULL, 1, 3, R_FLOAT, false, 3, R_CLM, R_NUMBER, R_NUMBER));
  INIT_WALKER(S_one_zero,       make_walker(one_zero_1, NULL, NULL, 1, 2, R_FLOAT, false, 2, R_CLM, R_NUMBER));
  INIT_WALKER(S_one_pole,       make_walker(one_pole_1, NULL, NULL, 1, 2, R_FLOAT, false, 2, R_CLM, R_NUMBER));
  INIT_WALKER(S_out_any,        walker_with_declare(make_walker(out_any_1, NULL, NULL, 3, 4, R_FLOAT, false, 4, R_NUMBER, R_NUMBER, R_INT, R_ANY), 3, 3, R_INT, R_FLOAT, R_INT));
  INIT_WALKER(S_outa,           walker_with_declare(make_walker(outa_1, NULL, NULL, 2, 3, R_FLOAT, false, 3, R_NUMBER, R_NUMBER, R_ANY), 2, 3, R_INT, R_FLOAT, R_INT));
  INIT_WALKER(S_outb,           walker_with_declare(make_walker(outb_1, NULL, NULL, 2, 3, R_FLOAT, false, 3, R_NUMBER, R_NUMBER, R_ANY), 2, 3, R_INT, R_FLOAT, R_INT));
  INIT_WALKER(S_outc,           walker_with_declare(make_walker(outc_1, NULL, NULL, 2, 3, R_FLOAT, false, 3, R_NUMBER, R_NUMBER, R_ANY), 2, 3, R_INT, R_FLOAT, R_INT));
  INIT_WALKER(S_outd,           walker_with_declare(make_walker(outd_1, NULL, NULL, 2, 3, R_FLOAT, false, 3, R_NUMBER, R_NUMBER, R_ANY), 2, 3, R_INT, R_FLOAT, R_INT));
  INIT_WALKER(S_env,            make_walker(env_1, NULL, NULL, 1, 1, R_FLOAT, false, 1, R_CLM));
  INIT_WALKER(S_env_interp,     make_walker(env_interp_1, NULL, NULL, 2, 2, R_FLOAT, false, 2, R_FLOAT, R_CLM));
  INIT_WALKER(S_env_any,        walker_with_declare(make_walker(env_any_1, NULL, NULL, 2, 2, R_FLOAT, false, 2, R_CLM, R_FUNCTION), 1, 1, R_FLOAT));
  INIT_WALKER(S_notch,          make_walker(notch_1, NULL, NULL, 1, 3, R_FLOAT, false, 3, R_CLM, R_NUMBER, R_NUMBER));
  INIT_WALKER(S_comb,           make_walker(comb_1, NULL, NULL, 1, 3, R_FLOAT, false, 3, R_CLM, R_NUMBER, R_NUMBER));
  INIT_WALKER(S_filtered_comb,  make_walker(filtered_comb_1, NULL, NULL, 1, 3, R_FLOAT, false, 3, R_CLM, R_NUMBER, R_NUMBER));
  INIT_WALKER(S_convolve,       walker_with_declare(make_walker(convolve_1, NULL, NULL, 1, 2, R_FLOAT, false, 2, R_CLM, R_FUNCTION), 2, 1, R_INT));
  INIT_WALKER(S_delay,          make_walker(delay_1, NULL, NULL, 1, 3, R_FLOAT, false, 3, R_CLM, R_NUMBER, R_NUMBER));
  INIT_WALKER(S_all_pass,       make_walker(all_pass_1, NULL, NULL, 1, 3, R_FLOAT, false, 3, R_CLM, R_NUMBER, R_NUMBER));
  INIT_WALKER(S_moving_average, make_walker(moving_average_1, NULL, NULL, 1, 2, R_FLOAT, false, 2, R_CLM, R_NUMBER));
  INIT_WALKER(S_asymmetric_fm,  make_walker(asymmetric_fm_1, NULL, NULL, 1, 3, R_FLOAT, false, 3, R_CLM, R_NUMBER, R_NUMBER));
  INIT_WALKER(S_rand,           make_walker(rand_1, NULL, NULL, 1, 2, R_FLOAT, false, 2, R_CLM, R_NUMBER));
  INIT_WALKER(S_rand_interp,    make_walker(rand_interp_1, NULL, NULL, 1, 2, R_FLOAT, false, 2, R_CLM, R_NUMBER));
  INIT_WALKER(S_readin,         make_walker(readin_1, NULL, NULL, 1, 1, R_FLOAT, false, 1, R_CLM));
  INIT_WALKER(S_src,            walker_with_declare(make_walker(src_1, NULL, NULL, 1, 3, R_FLOAT, false, 3, R_CLM, R_NUMBER, R_FUNCTION), 2, 1, R_INT));
  INIT_WALKER(S_ncos,           make_walker(ncos_1, NULL, NULL, 1, 2, R_FLOAT, false, 2, R_CLM, R_NUMBER));
  INIT_WALKER(S_nsin,           make_walker(nsin_1, NULL, NULL, 1, 2, R_FLOAT, false, 2, R_CLM, R_NUMBER));
  INIT_WALKER(S_ssb_am,         make_walker(ssb_am_1, NULL, NULL, 1, 3, R_FLOAT, false, 3, R_CLM, R_NUMBER, R_NUMBER));
  INIT_WALKER(S_sawtooth_wave,  make_walker(sawtooth_wave_1, NULL, NULL, 1, 2, R_FLOAT, false, 2, R_CLM, R_NUMBER));
  INIT_WALKER(S_square_wave,    make_walker(square_wave_1, NULL, NULL, 1, 2, R_FLOAT, false, 2, R_CLM, R_NUMBER));
  INIT_WALKER(S_nrxysin,        make_walker(nrxysin_1, NULL, NULL, 1, 2, R_FLOAT, false, 2, R_CLM, R_NUMBER));
  INIT_WALKER(S_nrxycos,        make_walker(nrxycos_1, NULL, NULL, 1, 2, R_FLOAT, false, 2, R_CLM, R_NUMBER));
  INIT_WALKER(S_sample_to_file, make_walker(sample_to_file_1, NULL, NULL, 4, 4, R_FLOAT, false, 4, R_CLM, R_NUMBER, R_INT, R_NUMBER));
  INIT_WALKER(S_table_lookup,   make_walker(table_lookup_1, NULL, NULL, 1, 2, R_FLOAT, false, 2, R_CLM, R_NUMBER));
  INIT_WALKER(S_triangle_wave,  make_walker(triangle_wave_1, NULL, NULL, 1, 2, R_FLOAT, false, 2, R_CLM, R_NUMBER));
  INIT_WALKER(S_two_zero,       make_walker(two_zero_1, NULL, NULL, 1, 2, R_FLOAT, false, 2, R_CLM, R_NUMBER));
  INIT_WALKER(S_two_pole,       make_walker(two_pole_1, NULL, NULL, 1, 2, R_FLOAT, false, 2, R_CLM, R_NUMBER));
  INIT_WALKER(S_tap,            make_walker(tap_1, NULL, NULL, 1, 2, R_FLOAT, false, 2, R_CLM, R_NUMBER));
  INIT_WALKER(S_delay_tick,     make_walker(delay_tick_1, NULL, NULL, 2, 2, R_FLOAT, false, 2, R_CLM, R_NUMBER));
  INIT_WALKER(S_pulse_train,    make_walker(pulse_train_1, NULL, NULL, 1, 2, R_FLOAT, false, 2, R_CLM, R_NUMBER));
  INIT_WALKER(S_phase_vocoder, 
	      walker_with_declare(
	        walker_with_declare(
	          walker_with_declare(
		    walker_with_declare(
                      make_walker(phase_vocoder_1, NULL, NULL, 1, 5, R_FLOAT, false, 5, R_CLM, R_FUNCTION, R_FUNCTION, R_FUNCTION, R_FUNCTION), 
		      4, 1, R_CLM),
		    3, 1, R_CLM),
		  2, 2, R_CLM, R_FUNCTION),
		1, 1, R_INT));
  INIT_WALKER(S_phase_vocoder_amps,             make_walker(phase_vocoder_amps_1, NULL, NULL, 1, 1, R_VCT, false, 1, R_CLM)); 
  INIT_WALKER(S_phase_vocoder_amp_increments,   make_walker(phase_vocoder_amp_increments_1, NULL, NULL, 1, 1, R_VCT, false, 1, R_CLM));
  INIT_WALKER(S_phase_vocoder_freqs,            make_walker(phase_vocoder_freqs_1, NULL, NULL, 1, 1, R_VCT, false, 1, R_CLM));
  INIT_WALKER(S_phase_vocoder_phases,           make_walker(phase_vocoder_phases_1, NULL, NULL, 1, 1, R_VCT, false, 1, R_CLM));
  INIT_WALKER(S_phase_vocoder_phase_increments, make_walker(phase_vocoder_phase_increments_1, NULL, NULL, 1, 1, R_VCT, false, 1, R_CLM));

  INIT_WALKER(S_firmant,        make_walker(firmant_1, NULL, NULL, 1, 3, R_FLOAT, false, 3, R_CLM, R_NUMBER, R_NUMBER));
  INIT_WALKER(S_formant,        make_walker(formant_1, NULL, NULL, 1, 3, R_FLOAT, false, 3, R_CLM, R_NUMBER, R_NUMBER));
  INIT_WALKER(S_filter,         make_walker(filter_1, NULL, NULL, 1, 2, R_FLOAT, false, 2, R_CLM, R_NUMBER));
  INIT_WALKER(S_fir_filter,     make_walker(fir_filter_1, NULL, NULL, 1, 2, R_FLOAT, false, 2, R_CLM, R_NUMBER));
  INIT_WALKER(S_file_to_sample, make_walker(file_to_sample_1, NULL, NULL, 2, 3, R_FLOAT, false, 3, R_CLM, R_NUMBER, R_INT));
#if USE_SND
  INIT_WALKER(S_snd_to_sample,  make_walker(snd_to_sample_1, NULL, NULL, 2, 3, R_FLOAT, false, 3, R_CLM, R_NUMBER, R_INT));
#endif
  INIT_WALKER(S_frame_ref,      make_walker(frame_ref_0, NULL, frame_set_1, 2, 2, R_FLOAT, false, 2, R_CLM, R_INT));
  INIT_WALKER(S_frame_set,      make_walker(frame_set_2, NULL, NULL, 3, 3, R_FLOAT, false, 3, R_CLM, R_INT, R_NUMBER));
  INIT_WALKER(S_frame,          make_walker(frame_1, NULL, NULL, 1, UNLIMITED_ARGS, R_CLM, false, 1, -R_NUMBER));
  INIT_WALKER(S_wave_train,     make_walker(wave_train_1, NULL, NULL, 1, 2, R_FLOAT, false, 2, R_CLM, R_NUMBER));
  INIT_WALKER(S_polyshape,      make_walker(polyshape_1, NULL, NULL, 1, 3, R_FLOAT, false, 3, R_CLM, R_NUMBER, R_NUMBER));
  INIT_WALKER(S_polywave,       make_walker(polywave_1, NULL, NULL, 1, 2, R_FLOAT, false, 2, R_CLM, R_NUMBER));
  INIT_WALKER(S_iir_filter,     make_walker(iir_filter_1, NULL, NULL, 1, 2, R_FLOAT, false, 2, R_CLM, R_NUMBER));
  INIT_WALKER(S_ina,            walker_with_declare(make_walker(ina_1, NULL, NULL, 2, 2, R_FLOAT, false, 2, R_NUMBER, R_ANY), 1, 2, R_INT, R_INT));
  INIT_WALKER(S_inb,            walker_with_declare(make_walker(inb_1, NULL, NULL, 2, 2, R_FLOAT, false, 2, R_NUMBER, R_ANY), 1, 2, R_INT, R_INT));
  INIT_WALKER(S_in_any,         walker_with_declare(make_walker(in_any_1, NULL, NULL, 3, 3, R_FLOAT, false, 2, R_NUMBER, R_INT, R_ANY), 2, 2, R_INT, R_INT));
  INIT_WALKER(S_granulate, 
	      walker_with_declare(
                walker_with_declare(
  	          make_walker(granulate_1, NULL, NULL, 1, 3, R_FLOAT, false, 3, R_CLM, R_FUNCTION, R_FUNCTION),
		  2, 1, R_CLM),
		1, 1, R_INT));
  INIT_WALKER(S_move_locsig,          make_walker(move_locsig_1, NULL, NULL, 3, 3, R_FLOAT, false, 3, R_CLM, R_NUMBER, R_NUMBER));
  INIT_WALKER(S_mus_set_formant_radius_and_frequency, make_walker(set_formant_radius_and_frequency_1, NULL, NULL, 3, 3, R_FLOAT, false, 3, R_CLM, R_NUMBER, R_NUMBER));
  INIT_WALKER(S_mixer_set,            make_walker(mixer_set_2, NULL, NULL, 4, 4, R_FLOAT, false, 4, R_CLM, R_INT, R_INT, R_NUMBER));
  INIT_WALKER(S_mixer_ref,            make_walker(mixer_ref_1, NULL, mixer_set_1, 3, 3, R_FLOAT, false, 3, R_CLM, R_INT, R_INT));
  INIT_WALKER(S_mixer,                make_walker(mixer_1, NULL, NULL, 1, UNLIMITED_ARGS, R_CLM, false, 1, -R_NUMBER));
  INIT_WALKER(S_locsig_ref,           make_walker(locsig_ref_0, NULL, locsig_set_1, 2, 2, R_FLOAT, false, 2, R_CLM, R_INT));
  INIT_WALKER(S_locsig_reverb_ref,    make_walker(locsig_reverb_ref_0, NULL, locsig_reverb_set_1, 2, 2, R_FLOAT, false, 2, R_CLM, R_INT));
  INIT_WALKER(S_locsig_set,           make_walker(locsig_set_2, NULL, NULL, 3, 3, R_FLOAT, false, 3, R_CLM, R_INT, R_NUMBER));
  INIT_WALKER(S_locsig_reverb_set,    make_walker(locsig_reverb_set_2, NULL, NULL, 3, 3, R_FLOAT, false, 3, R_CLM, R_INT, R_NUMBER));
  INIT_WALKER(S_polynomial,           make_walker(polynomial_1, NULL, NULL, 2, 2, R_FLOAT, false, 2, R_VCT, R_NUMBER));
  INIT_WALKER(S_clear_array,          make_walker(clear_array_1, NULL, NULL, 1, 1, R_VCT, false, 1, R_VCT));
  INIT_WALKER(S_array_interp,         make_walker(array_interp_1, NULL, NULL, 2, 3, R_FLOAT, false, 3, R_VCT, R_FLOAT, R_INT));
  INIT_WALKER(S_mus_interpolate,      make_walker(mus_interpolate_1, NULL, NULL, 3, 5, R_FLOAT, false, 3, R_INT, R_FLOAT, R_VCT, R_INT, R_FLOAT));
  INIT_WALKER(S_mus_srate,            make_walker(mus_srate_1, NULL, NULL, 0, 0, R_NUMBER, false, 0));
  INIT_WALKER(S_ring_modulate,        make_walker(ring_modulate_1, NULL, NULL, 2, 2, R_FLOAT, false, 2, R_NUMBER, R_NUMBER));
  INIT_WALKER(S_amplitude_modulate,   make_walker(amplitude_modulate_1, NULL, NULL, 3, 3, R_FLOAT, false, 3, R_NUMBER, R_NUMBER, R_NUMBER));
  INIT_WALKER(S_contrast_enhancement, make_walker(contrast_enhancement_1, NULL, NULL, 2, 2, R_FLOAT, false, 2, R_NUMBER, R_NUMBER));
  INIT_WALKER(S_dot_product,          make_walker(dot_product_1, NULL, NULL, 2, 3, R_FLOAT, false, 3, R_VCT, R_VCT, R_INT));
  INIT_WALKER(S_polar_to_rectangular, make_walker(polar_to_rectangular_1, NULL, NULL, 2, 2, R_VCT, false, 2, R_VCT, R_VCT));
  INIT_WALKER(S_rectangular_to_magnitudes, make_walker(rectangular_to_magnitudes_1, NULL, NULL, 2, 2, R_VCT, false, 2, R_VCT, R_VCT));
  INIT_WALKER(S_rectangular_to_polar, make_walker(rectangular_to_polar_1, NULL, NULL, 2, 2, R_VCT, false, 2, R_VCT, R_VCT));
  INIT_WALKER(S_multiply_arrays,      make_walker(multiply_arrays_1, NULL, NULL, 2, 3, R_VCT, false, 3, R_VCT, R_VCT, R_INT));
  INIT_WALKER(S_mus_fft,              make_walker(mus_fft_1, NULL, NULL, 2, 4, R_VCT, false, 4, R_VCT, R_VCT, R_INT, R_INT));
  INIT_WALKER(S_spectrum,             make_walker(mus_spectrum_1, NULL, NULL, 3, 4, R_VCT, false, 4, R_VCT, R_VCT, R_VCT, R_INT));
  INIT_WALKER(S_convolution,          make_walker(convolution_1, NULL, NULL, 2, 3, R_VCT, false, 3, R_VCT, R_VCT, R_INT));
  INIT_WALKER(S_formant_bank,         make_walker(formant_bank_1,NULL, NULL, 3, 3, R_FLOAT, false, 2, R_VCT, R_CLM_VECTOR));
  INIT_WALKER(S_frame_add,            make_walker(mus_frame_add_1, NULL, NULL, 2, 3, R_CLM, false, 3, R_NUMBER_CLM, R_NUMBER_CLM, R_CLM));
  INIT_WALKER(S_frame_multiply,       make_walker(mus_frame_multiply_1, NULL, NULL, 2, 3, R_CLM, false, 3, R_NUMBER_CLM, R_NUMBER_CLM, R_CLM));
  INIT_WALKER(S_mixer_multiply,       make_walker(mus_mixer_multiply_1, NULL, NULL, 2, 3, R_CLM, false, 3, R_NUMBER_CLM, R_NUMBER_CLM, R_CLM));
  INIT_WALKER(S_mixer_add,            make_walker(mus_mixer_add_1, NULL, NULL, 2, 3, R_CLM, false, 3, R_NUMBER_CLM, R_NUMBER_CLM, R_CLM));
  INIT_WALKER(S_frame_to_frame,       make_walker(frame_to_frame_1, NULL, NULL, 2, 3, R_CLM, false, 3, R_CLM, R_CLM, R_CLM));
  INIT_WALKER(S_frame_to_sample,      make_walker(frame_to_sample_1, NULL, NULL, 2, 2, R_FLOAT, false, 2, R_CLM, R_CLM));
  INIT_WALKER(S_sample_to_frame,      make_walker(sample_to_frame_1, NULL, NULL, 2, 3, R_FLOAT, false, 3, R_CLM, R_FLOAT, R_CLM));
  INIT_WALKER(S_locsig,               make_walker(locsig_1, NULL, NULL, 3, 3, R_FLOAT, false, 3, R_CLM, R_NUMBER, R_NUMBER));
  INIT_WALKER(S_move_sound,           make_walker(move_sound_1, NULL, NULL, 3, 3, R_FLOAT, false, 3, R_CLM, R_NUMBER, R_NUMBER));
  INIT_WALKER(S_frame_to_file,        make_walker(frame_to_file_1, NULL, NULL, 3, 3, R_CLM, false, 3, R_CLM, R_NUMBER, R_CLM));
  INIT_WALKER(S_file_to_frame,        make_walker(file_to_frame_1, NULL, NULL, 2, 3, R_CLM, false, 3, R_CLM, R_NUMBER, R_CLM));

  INIT_WALKER(S_radians_to_hz,      make_walker(mus_radians_to_hz_1, NULL, NULL, 1, 1, R_FLOAT, false, 1, R_NUMBER));
  INIT_WALKER(S_hz_to_radians,      make_walker(mus_hz_to_radians_1, NULL, NULL, 1, 1, R_FLOAT, false, 1, R_NUMBER));
  INIT_WALKER(S_degrees_to_radians, make_walker(mus_degrees_to_radians_1, NULL, NULL, 1, 1, R_FLOAT, false, 1, R_NUMBER));
  INIT_WALKER(S_radians_to_degrees, make_walker(mus_radians_to_degrees_1, NULL, NULL, 1, 1, R_FLOAT, false, 1, R_NUMBER));
  INIT_WALKER(S_db_to_linear,       make_walker(mus_db_to_linear_1, NULL, NULL, 1, 1, R_FLOAT, false, 1, R_NUMBER));
  INIT_WALKER(S_linear_to_db,       make_walker(mus_linear_to_db_1, NULL, NULL, 1, 1, R_FLOAT, false, 1, R_NUMBER));
  INIT_WALKER(S_seconds_to_samples, make_walker(seconds_to_samples_1, NULL, NULL, 1, 1, R_INT, false, 1, R_NUMBER));
  INIT_WALKER(S_mus_random,         make_walker(mus_random_1, NULL, NULL, 1, 1, R_FLOAT, false, 1, R_NUMBER));

  INIT_WALKER(S_make_all_pass,       make_walker(make_all_pass_1, NULL, NULL, 0, UNLIMITED_ARGS, R_CLM, false, 1, -R_XEN));
  INIT_WALKER(S_make_moving_average, make_walker(make_moving_average_1, NULL, NULL, 0, UNLIMITED_ARGS, R_CLM, false, 1, -R_XEN));
  INIT_WALKER(S_make_asymmetric_fm,  make_walker(make_asymmetric_fm_1, NULL, NULL, 0, 8, R_CLM, false, 1, -R_XEN));
  INIT_WALKER(S_make_comb,           make_walker(make_comb_1, NULL, NULL, 0, UNLIMITED_ARGS, R_CLM, false, 1, -R_XEN));
  INIT_WALKER(S_make_filtered_comb,  make_walker(make_filtered_comb_1, NULL, NULL, 0, UNLIMITED_ARGS, R_CLM, false, 1, -R_XEN));
  INIT_WALKER(S_make_convolve,       make_walker(make_convolve_1, NULL, NULL, 0, UNLIMITED_ARGS, R_CLM, false, 1, -R_XEN));
  INIT_WALKER(S_make_delay,          make_walker(make_delay_1, NULL, NULL, 0, UNLIMITED_ARGS, R_CLM, false, 1, -R_XEN));
  INIT_WALKER(S_make_env,            make_walker(make_env_1, NULL, NULL, 0, UNLIMITED_ARGS, R_CLM, false, 1, -R_XEN));
  INIT_WALKER(S_make_fft_window,     make_walker(make_fft_window_1, NULL, NULL, 2, 3, R_VCT, false, 1, -R_XEN));
  INIT_WALKER(S_make_file_to_frame,  make_walker(make_file_to_frame_1, NULL, NULL, 0, 1, R_CLM, false, 1, -R_XEN));
  INIT_WALKER(S_make_file_to_sample, make_walker(make_file_to_sample_1, NULL, NULL, 0, 1, R_CLM, false, 1, -R_XEN));
  INIT_WALKER(S_make_filter,         make_walker(make_filter_1, NULL, NULL, 0, 6, R_CLM, false, 1, -R_XEN));
  INIT_WALKER(S_make_fir_filter,     make_walker(make_fir_filter_1, NULL, NULL, 0, 4, R_CLM, false, 1, -R_XEN));
  INIT_WALKER(S_make_firmant,        make_walker(make_firmant_1, NULL, NULL, 0, 4, R_CLM, false, 1, -R_XEN));
  INIT_WALKER(S_make_formant,        make_walker(make_formant_1, NULL, NULL, 0, 4, R_CLM, false, 1, -R_XEN));
  INIT_WALKER(S_make_frame,          make_walker(make_frame_1, NULL, NULL, 0, UNLIMITED_ARGS, R_CLM, false, 1, -R_XEN));
  INIT_WALKER(S_make_frame_to_file,  make_walker(make_frame_to_file_1, NULL, NULL, 0, 4, R_CLM, false, 1, -R_XEN));
  INIT_WALKER(S_make_granulate,      make_walker(make_granulate_1, NULL, NULL, 0, UNLIMITED_ARGS, R_CLM, false, 1, -R_XEN));
  INIT_WALKER(S_make_iir_filter,     make_walker(make_iir_filter_1, NULL, NULL, 0, 4, R_CLM, false, 1, -R_XEN));
  INIT_WALKER(S_make_locsig,         make_walker(make_locsig_1, NULL, NULL, 0, UNLIMITED_ARGS, R_CLM, false, 1, -R_XEN)); /* no move-sound -- way too complex */
  INIT_WALKER(S_make_mixer,          make_walker(make_mixer_1, NULL, NULL, 0, UNLIMITED_ARGS, R_CLM, false, 1, -R_XEN));
  INIT_WALKER(S_make_notch,          make_walker(make_notch_1, NULL, NULL, 0, UNLIMITED_ARGS, R_CLM, false, 1, -R_XEN));
  INIT_WALKER(S_make_one_pole,       make_walker(make_one_pole_1, NULL, NULL, 0, 4, R_CLM, false, 1, -R_XEN));
  INIT_WALKER(S_make_one_zero,       make_walker(make_one_zero_1, NULL, NULL, 0, 4, R_CLM, false, 1, -R_XEN));
  INIT_WALKER(S_make_oscil,          make_walker(make_oscil_1, NULL, NULL, 0, 4, R_CLM, false, 1, -R_XEN));
  INIT_WALKER(S_make_phase_vocoder,  make_walker(make_phase_vocoder_1, NULL, NULL, 0, UNLIMITED_ARGS, R_CLM, false, 1, -R_XEN));
  INIT_WALKER(S_make_pulse_train,    make_walker(make_pulse_train_1, NULL, NULL, 0, 6, R_CLM, false, 1, -R_XEN));
  INIT_WALKER(S_make_rand,           make_walker(make_rand_1, NULL, NULL, 0, 6, R_CLM, false, 1, -R_XEN));
  INIT_WALKER(S_make_rand_interp,    make_walker(make_rand_interp_1, NULL, NULL, 0, 6, R_CLM, false, 1, -R_XEN));
  INIT_WALKER(S_make_readin,         make_walker(make_readin_1, NULL, NULL, 0, 8, R_CLM, false, 1, -R_XEN));
  INIT_WALKER(S_make_sample_to_file, make_walker(make_sample_to_file_1, NULL, NULL, 4, 5, R_CLM, false, 1, -R_XEN));
  INIT_WALKER(S_make_sawtooth_wave,  make_walker(make_sawtooth_wave_1, NULL, NULL, 0, 6, R_CLM, false, 1, -R_XEN));
  INIT_WALKER(S_make_nrxysin,        make_walker(make_nrxysin_1, NULL, NULL, 0, UNLIMITED_ARGS, R_CLM, false, 1, -R_XEN));
  INIT_WALKER(S_make_nrxycos,        make_walker(make_nrxycos_1, NULL, NULL, 0, UNLIMITED_ARGS, R_CLM, false, 1, -R_XEN));
  INIT_WALKER(S_make_square_wave,    make_walker(make_square_wave_1, NULL, NULL, 0, 6, R_CLM, false, 1, -R_XEN));
  INIT_WALKER(S_make_src,            make_walker(make_src_1, NULL, NULL, 0, 6, R_CLM, false, 1, -R_XEN));
  INIT_WALKER(S_make_ncos,           make_walker(make_ncos_1, NULL, NULL, 0, 4, R_CLM, false, 1, -R_XEN));
  INIT_WALKER(S_make_nsin,           make_walker(make_nsin_1, NULL, NULL, 0, 4, R_CLM, false, 1, -R_XEN));
  INIT_WALKER(S_make_ssb_am,         make_walker(make_ssb_am_1, NULL, NULL, 0, 4, R_CLM, false, 1, -R_XEN));
  INIT_WALKER(S_make_table_lookup,   make_walker(make_table_lookup_1, NULL, NULL, 0, 6, R_CLM, false, 1, -R_XEN));
  INIT_WALKER(S_make_triangle_wave,  make_walker(make_triangle_wave_1, NULL, NULL, 0, 6, R_CLM, false, 1, -R_XEN));
  INIT_WALKER(S_make_two_pole,       make_walker(make_two_pole_1, NULL, NULL, 0, 6, R_CLM, false, 1, -R_XEN));
  INIT_WALKER(S_make_two_zero,       make_walker(make_two_zero_1, NULL, NULL, 0, 6, R_CLM, false, 1, -R_XEN));
  INIT_WALKER(S_make_wave_train,     make_walker(make_wave_train_1, NULL, NULL, 0, 6, R_CLM, false, 1, -R_XEN));
  INIT_WALKER(S_make_polyshape,      make_walker(make_polyshape_1, NULL, NULL, 0, 8, R_CLM, false, 1, -R_XEN));
  INIT_WALKER(S_make_polywave,       make_walker(make_polywave_1, NULL, NULL, 0, 6, R_CLM, false, 1, -R_XEN));

  INIT_WALKER(S_partials_to_polynomial, make_walker(partials_to_polynomial_1, NULL, NULL, 1, 2, R_VCT, false, 2, R_VCT, R_INT));
  INIT_WALKER(S_normalize_partials,  make_walker(normalize_partials_1, NULL, NULL, 1, 1, R_VCT, false, 1, R_VCT));
  INIT_WALKER(S_mus_chebyshev_t_sum, make_walker(mus_chebyshev_t_sum_1, NULL, NULL, 2, 2, R_FLOAT, false, 2, R_FLOAT, R_VCT));
  INIT_WALKER(S_mus_chebyshev_u_sum, make_walker(mus_chebyshev_u_sum_1, NULL, NULL, 2, 2, R_FLOAT, false, 2, R_FLOAT, R_VCT));
  INIT_WALKER(S_mus_chebyshev_tu_sum, make_walker(mus_chebyshev_tu_sum_1, NULL, NULL, 3, 3, R_FLOAT, false, 3, R_FLOAT, R_VCT, R_VCT));


  /* -------- sndlib funcs */
  INIT_WALKER(S_mus_sound_samples,        make_walker(mus_sound_samples_1, NULL, NULL, 1, 1, R_INT, false, 1, R_STRING));
  INIT_WALKER(S_mus_sound_frames,         make_walker(mus_sound_frames_1, NULL, NULL, 1, 1, R_INT, false, 1, R_STRING));
  INIT_WALKER(S_mus_sound_datum_size,     make_walker(mus_sound_datum_size_1, NULL, NULL, 1, 1, R_INT, false, 1, R_STRING));
  INIT_WALKER(S_mus_sound_data_location,  make_walker(mus_sound_data_location_1, NULL, NULL, 1, 1, R_INT, false, 1, R_STRING));
  INIT_WALKER(S_mus_sound_chans,          make_walker(mus_sound_chans_1, NULL, NULL, 1, 1, R_INT, false, 1, R_STRING));
  INIT_WALKER(S_mus_sound_srate,          make_walker(mus_sound_srate_1, NULL, NULL, 1, 1, R_INT, false, 1, R_STRING));
  INIT_WALKER(S_mus_sound_header_type,    make_walker(mus_sound_header_type_1, NULL, NULL, 1, 1, R_INT, false, 1, R_STRING));
  INIT_WALKER(S_mus_sound_data_format,    make_walker(mus_sound_data_format_1, NULL, NULL, 1, 1, R_INT, false, 1, R_STRING));
  INIT_WALKER(S_mus_sound_length,         make_walker(mus_sound_length_1, NULL, NULL, 1, 1, R_INT, false, 1, R_STRING));
  INIT_WALKER(S_mus_sound_duration,       make_walker(mus_sound_duration_1, NULL, NULL, 1, 1, R_FLOAT, false, 1, R_STRING));
  INIT_WALKER(S_mus_sound_comment,        make_walker(mus_sound_comment_1, NULL, NULL, 1, 1, R_STRING, false, 1, R_STRING));
  INIT_WALKER(S_mus_sound_forget,         make_walker(mus_sound_forget_1, NULL, NULL, 1, 1, R_INT, false, 1, R_STRING));
  INIT_WALKER(S_mus_sound_type_specifier, make_walker(mus_sound_type_specifier_1, NULL, NULL, 1, 1, R_INT, false, 1, R_STRING));

  INIT_WALKER(S_mus_header_type_name,      make_walker(mus_header_type_name_1, NULL, NULL, 1, 1, R_STRING, false, 1, R_INT));
  INIT_WALKER(S_mus_data_format_name,      make_walker(mus_data_format_name_1, NULL, NULL, 1, 1, R_STRING, false, 1, R_INT));
  INIT_WALKER(S_mus_header_type_to_string, make_walker(mus_header_type_to_string_1, NULL, NULL, 1, 1, R_STRING, false, 1, R_INT));
  INIT_WALKER(S_mus_data_format_to_string, make_walker(mus_data_format_to_string_1, NULL, NULL, 1, 1, R_STRING, false, 1, R_INT));
  INIT_WALKER(S_mus_bytes_per_sample,      make_walker(mus_bytes_per_sample_1, NULL, NULL, 1, 1, R_INT, false, 1, R_INT));

  INIT_WALKER(S_mus_audio_close, make_walker(mus_audio_close_1, NULL, NULL, 1, 1, R_INT, false, 1, R_INT));
  INIT_WALKER(S_mus_audio_write, make_walker(mus_audio_write_1, NULL, NULL, 3, 3, R_INT, false, 3, R_INT, R_SOUND_DATA, R_INT));

  INIT_WALKER(S_vct_ref,       make_walker(vct_ref_1, NULL, vct_set_1, 2, 2, R_FLOAT, false, 2, R_VCT, R_INT));
  INIT_WALKER(S_vct_length,    make_walker(vct_length_1, NULL, NULL, 1, 1, R_INT, false, 1, R_VCT));
  INIT_WALKER(S_vct_fillB,     make_walker(vct_fill_1, NULL, NULL, 2, 2, R_VCT, false, 2, R_VCT, R_NUMBER));
  INIT_WALKER(S_vct_scaleB,    make_walker(vct_scale_1, NULL, NULL, 2, 2, R_VCT, false, 2, R_VCT, R_NUMBER));
  INIT_WALKER(S_vct_offsetB,   make_walker(vct_offset_1, NULL, NULL, 2, 2, R_VCT, false, 2, R_VCT, R_NUMBER));
  INIT_WALKER(S_vct_addB,      make_walker(vct_add_1, NULL, NULL, 2, 2, R_VCT, false, 2, R_VCT, R_VCT));
  INIT_WALKER(S_vct_subtractB, make_walker(vct_subtract_1, NULL, NULL, 2, 2, R_VCT, false, 2, R_VCT, R_VCT));
  INIT_WALKER(S_vct_multiplyB, make_walker(vct_multiply_1, NULL, NULL, 2, 2, R_VCT, false, 2, R_VCT, R_VCT));
  INIT_WALKER(S_vct_copy,      make_walker(vct_copy_1, NULL, NULL, 1, 1, R_VCT, false, 1, R_VCT));
  INIT_WALKER(S_vct_peak,      make_walker(vct_peak_1, NULL, NULL, 1, 1, R_FLOAT, false, 1, R_VCT));
  INIT_WALKER(S_vct_setB,      make_walker(vct_set_2, NULL, NULL, 3, 3, R_FLOAT, false, 3, R_VCT, R_INT, R_NUMBER));
  INIT_WALKER(S_make_vct,      make_walker(make_vct_1, NULL, NULL, 1, 2, R_VCT, false, 2, R_INT, R_FLOAT));
  INIT_WALKER(S_vct,           make_walker(vct_1, NULL, NULL, 1, UNLIMITED_ARGS, R_VCT, false, 1, -R_NUMBER));
  INIT_WALKER(S_vct_p,         make_walker(vct_p_1, NULL, NULL, 1, 1, R_BOOL, false, 0));
  INIT_WALKER(S_vct_reverse,   make_walker(vct_reverse_2, NULL, NULL, 1, 2, R_VCT, false, 2, R_VCT, R_INT));
  INIT_WALKER(S_vct_moveB,     make_walker(vct_move_3, NULL, NULL, 3, 3, R_VCT, false, 3, R_VCT, R_INT, R_INT));
  INIT_WALKER(S_vct_times,     make_walker(vct_times_1, NULL, NULL, 2, 2, R_VCT, false, 2, R_NUMBER_VCT, R_NUMBER_VCT));
  INIT_WALKER(S_vct_plus,      make_walker(vct_plus_1, NULL, NULL, 2, 2, R_VCT, false, 2, R_NUMBER_VCT, R_NUMBER_VCT));

  INIT_WALKER(S_sound_data_length,    make_walker(sound_data_length_1, NULL, NULL, 1, 1, R_INT, false, 1, R_SOUND_DATA));
  INIT_WALKER(S_sound_data_chans,     make_walker(sound_data_chans_1, NULL, NULL, 1, 1, R_INT, false, 1, R_SOUND_DATA));
  INIT_WALKER(S_sound_data_peak,      make_walker(sound_data_peak_1, NULL, NULL, 1, 1, R_FLOAT, false, 1, R_SOUND_DATA));
  INIT_WALKER(S_sound_data_copy,      make_walker(sound_data_copy_1, NULL, NULL, 1, 1, R_SOUND_DATA, false, 1, R_SOUND_DATA));
  INIT_WALKER(S_sound_data_reverseB,  make_walker(sound_data_reverse_1, NULL, NULL, 1, 1, R_SOUND_DATA, false, 1, R_SOUND_DATA));
  INIT_WALKER(S_sound_data_p,         make_walker(sound_data_p_1, NULL, NULL, 1, 1, R_BOOL, false, 0));
  INIT_WALKER(S_sound_data_ref,       make_walker(sound_data_ref_1, NULL, sound_data_set_1, 3, 3, R_FLOAT, false, 3, R_SOUND_DATA, R_INT, R_INT));
  INIT_WALKER(S_sound_data_setB,      make_walker(sound_data_set_2, NULL, NULL, 4, 4, R_FLOAT, false, 4, R_SOUND_DATA, R_INT, R_INT, R_NUMBER));
  INIT_WALKER(S_make_sound_data,      make_walker(make_sound_data_1, NULL, NULL, 2, 2, R_SOUND_DATA, false, 2, R_INT, R_INT));
  INIT_WALKER(S_sound_data_scaleB,    make_walker(sound_data_scale_1, NULL, NULL, 2, 2, R_SOUND_DATA, false, 2, R_SOUND_DATA, R_NUMBER));
  INIT_WALKER(S_sound_data_offsetB,   make_walker(sound_data_offset_1, NULL, NULL, 2, 2, R_SOUND_DATA, false, 2, R_SOUND_DATA, R_NUMBER));
  INIT_WALKER(S_sound_data_fillB,     make_walker(sound_data_fill_1, NULL, NULL, 2, 2, R_SOUND_DATA, false, 2, R_SOUND_DATA, R_NUMBER));
  INIT_WALKER(S_sound_data_addB,      make_walker(sound_data_add_1, NULL, NULL, 2, 2, R_SOUND_DATA, false, 2, R_SOUND_DATA, R_SOUND_DATA));
  INIT_WALKER(S_sound_data_multiplyB, make_walker(sound_data_multiply_1, NULL, NULL, 2, 2, R_SOUND_DATA, false, 2, R_SOUND_DATA, R_SOUND_DATA));
  INIT_WALKER(S_sound_data_multiply,  make_walker(sound_data_times_1, NULL, NULL, 2, 2, R_SOUND_DATA, false, 2, R_NUMBER_SOUND_DATA, R_NUMBER_SOUND_DATA));
  INIT_WALKER(S_sound_data_add,       make_walker(sound_data_plus_1, NULL, NULL, 2, 2, R_SOUND_DATA, false, 2, R_NUMBER_SOUND_DATA, R_NUMBER_SOUND_DATA));
  INIT_WALKER(S_sound_data_to_vct,    make_walker(sound_data_to_vct_1, NULL, NULL, 3, 3, R_VCT, false, 3, R_SOUND_DATA, R_INT, R_VCT));
  INIT_WALKER(S_vct_to_sound_data,    make_walker(vct_to_sound_data_1, NULL, NULL, 3, 3, R_SOUND_DATA, false, 3, R_VCT, R_SOUND_DATA, R_INT));

  INIT_WALKER(S_autocorrelate,  make_walker(autocorrelate_1, NULL, NULL, 1, 1, R_VCT, false, 1, R_VCT));
  INIT_WALKER(S_correlate,      make_walker(correlate_1, NULL, NULL, 2, 2, R_VCT, false, 2, R_VCT, R_VCT));

#if USE_SND
  /* -------- snd funcs */
  INIT_WALKER(S_next_sample,            make_walker(next_sample_1, NULL, NULL, 1, 1, R_FLOAT, false, 1, R_SAMPLER));
  INIT_WALKER(S_previous_sample,        make_walker(previous_sample_1, NULL, NULL, 1, 1, R_FLOAT, false, 1, R_SAMPLER));
  INIT_WALKER(S_read_sample,            make_walker(sampler_1, NULL, NULL, 1, 1, R_FLOAT, false, 1, R_SAMPLER));
  INIT_WALKER(S_make_sampler,           make_walker(make_sampler_1, NULL, NULL, 0, 5, R_SAMPLER, false, 1, R_NUMBER));
  INIT_WALKER(S_sampler_p,              make_walker(sampler_p_1, NULL, NULL, 1, 1, R_BOOL, false, 0));
  INIT_WALKER(S_sampler_at_end_p,       make_walker(sampler_at_end_p_1, NULL, NULL, 1, 1, R_BOOL, false, 0));

  INIT_WALKER(S_edit_position,          make_walker(edit_position_1, NULL, NULL, 0, 2, R_INT, false, 0));
  INIT_WALKER(S_frames,                 make_walker(frames_1, NULL, NULL, 0, 3, R_INT, false, 0));
  INIT_WALKER(S_cursor,                 make_walker(cursor_1, NULL, NULL, 0, 2, R_INT, false, 0));
  INIT_WALKER(S_maxamp,                 make_walker(maxamp_1, NULL, NULL, 0, 3, R_FLOAT, false, 0));
  INIT_WALKER(S_sample,                 make_walker(sample_1, NULL, NULL, 1, 4, R_FLOAT, false, 1, R_INT));
  INIT_WALKER(S_samples,                make_walker(samples_1, NULL, NULL, 2, 5, R_VCT, false, 2, R_INT, R_INT));
  INIT_WALKER(S_channel_to_vct,         make_walker(samples_1, NULL, NULL, 2, 5, R_VCT, false, 2, R_INT, R_INT));
  INIT_WALKER(S_srate,                  make_walker(srate_1, NULL, NULL, 0, 1, R_INT, false, 0));
  INIT_WALKER(S_channels,               make_walker(channels_1, NULL, NULL, 0, 1, R_INT, false, 0));
  INIT_WALKER(S_file_name,              make_walker(file_name_1, NULL, NULL, 0, 1, R_STRING, false, 0));
  INIT_WALKER(S_vct_to_channel,         make_walker(vct_to_channel_1, NULL, NULL, 3, 5, R_BOOL, false, 3, R_VCT, R_INT, R_INT));
  INIT_WALKER(S_exit,                   make_walker(exit_1, NULL, NULL, 0, 0, R_BOOL, false, 0));

  INIT_WALKER(S_snd_print,              make_walker(snd_print_1, NULL, NULL, 1, 1, R_BOOL, false, 1, R_STRING));
  INIT_WALKER(S_snd_warning,            make_walker(snd_warning_1, NULL, NULL, 1, 1, R_BOOL, false, 1, R_STRING));

  INIT_WALKER(S_sound_p,                make_walker(r_sound_p_1, NULL, NULL, 1, 1, R_BOOL, false, 0));
  INIT_WALKER(S_selection_chans,        make_walker(selection_chans_1, NULL, NULL, 0, 0, R_INT, false, 0));
  INIT_WALKER(S_temp_dir,               make_walker(r_temp_dir_1, NULL, NULL, 0, 0, R_STRING, false, 0));
  INIT_WALKER(S_save_dir,               make_walker(r_save_dir_1, NULL, NULL, 0, 0, R_STRING, false, 0));
  INIT_WALKER(S_fft,                    make_walker(fft_1, NULL, NULL, 2, 3, R_VCT, false, 3, R_VCT, R_VCT, R_INT));
#endif
}


static s7_pointer g_run_eval(s7_pointer code, s7_pointer arg, s7_pointer arg1, s7_pointer arg2)
{
  ptree *pt;
  s7_pointer cl;
  int gc_loc;

#if USE_SND
  optimizing = true;
#endif

  if (!s7_is_pair(code))
    return(code);

  /* cl = s7_make_closure(s7, code, s7_current_environment(s7)); */
  cl = s7_cons(s7, code, s7_current_environment(s7));

  gc_loc = s7_gc_protect(s7, cl);
  s7_unoptimize(s7, code);

  pt = make_ptree(8);
  pt->code = cl;
  pt->result = walk(pt, code, NEED_ANY_RESULT);

  s7_gc_unprotect_at(s7, gc_loc);

  /* fprintf(stderr, "run-eval: %s: %s -> %s\n", s7_object_to_c_string(s7, code), s7_object_to_c_string(s7, arg), (pt->result) ? describe_xen_value(pt->result, pt) : "no result"); */

  if (pt->result)
    {
      add_triple_to_ptree(pt, make_triple(quit, "quit", NULL, 0));

      if (ptree_on != NO_PTREE_DISPLAY)
	{
	  char *msg;
	  fprintf(stderr, "\n-------- optimize %s\n", s7_object_to_c_string(s7, code));
	  msg = describe_ptree(pt, "  ");
	  if (ptree_on == STDERR_PTREE_DISPLAY) fprintf(stderr, "%s", msg);
#if USE_SND
	  else if (ptree_on == LISTENER_PTREE_DISPLAY) listener_append(msg);
#endif
	  free(msg);
	  fprintf(stderr, "%s", "--------\n");
	}

      if (pt->args)
	{
	  bool arity_err = false;
	  int err = 0;
	  if (pt->arity > 3)
	    arity_err = true;
	  else
	    {
	      if (pt->arity > 0)
		{
		  if (arg != scheme_undefined)
		    {
		      /* fprintf(stderr, "arg: %s (%s %d)\n", s7_object_to_c_string(s7, arg), type_name(pt->arg_types[0]), pt->args[0]); */

		      err = xen_to_addr(pt, arg, pt->arg_types[0], pt->args[0]);
		      if ((err != XEN_TO_ADDR_ERROR) && (pt->arity > 1))
			{
			  if (arg1 != scheme_undefined)
			    {
			      err = xen_to_addr(pt, arg1, pt->arg_types[1], pt->args[1]);
			      if ((err != XEN_TO_ADDR_ERROR) && (pt->arity > 2))
				{
				  if (arg2 != scheme_undefined)
				    err = xen_to_addr(pt, arg2, pt->arg_types[2], pt->args[2]);
				  else arity_err = true;
				}
			    }
			  else arity_err = true;
			}
		    }
		  else arity_err = true;
		}
	    }
	  if (err == XEN_TO_ADDR_ERROR)
	    {
	      mus_run_free_ptree(pt); 
	      return(scheme_false); /* already warned, I think */
	    }
	  if (arity_err) 
	    {
	      mus_run_free_ptree(pt); 
	      s7_error(s7, s7_make_symbol(s7, "wrong-number-of-args"), 
			scheme_list_2(scheme_make_string("run-eval: wrong number of args for function: ~A"),
				   code)); 
	      return(scheme_false);
	    }
	}
      return(eval_ptree_to_xen(pt));
    }
  if (pt) mus_run_free_ptree(pt);

  s7_error(s7, s7_make_symbol(s7, "cannot-parse"),
	    scheme_list_2(scheme_make_string("run-eval: can't parse ~A"),
		       code));

  return(scheme_false);
}

#ifndef S_run
  #define S_run "run"
#endif

static s7_pointer g_run(s7_pointer proc_and_code)
{
  #define H_run "(" S_run " thunk): try to optimize the procedure passed as its argument, \
then evaluate it; if the optimizer can't handle something in the procedure, it is passed \
to Scheme and is equivalent to (thunk)."

  s7_pointer code;
  ptree *pt = NULL;
  s7_pointer result;
  int gc_loc;

  XEN_ASSERT_TYPE(s7_is_procedure(proc_and_code) && (XEN_REQUIRED_ARGS_OK(proc_and_code, 0)), proc_and_code, 1, S_run, "a thunk");
  
  /* fprintf(stderr, "start: %s\n", s7_object_to_c_string(s7, s7_car(proc_and_code))); */

  s7_unoptimize(s7, s7_cdr(proc_and_code));
  code = s7_cons(s7, s7_append(s7, 
			       s7_cons(s7, s7_make_symbol(s7, "lambda"), scheme_nil),
			       s7_cons(s7, 
				       s7_car(proc_and_code),
				       s7_cdr(proc_and_code))),
		 s7_current_environment(s7));
  gc_loc = s7_gc_protect(s7, code);

  pt = form_to_ptree(code);
  if (pt)
    result = eval_ptree_to_xen(pt);
  else result = s7_call(s7, proc_and_code, scheme_nil);
    
  s7_gc_unprotect_at(s7, gc_loc);
  return(result);
}


#if USE_SND
static s7_pointer g_optimization(void) {return(s7_make_integer(s7, optimization(ss)));}

static s7_pointer g_set_optimization(s7_pointer val) 
{
  #define H_optimization "(" S_optimization "): the current 'run' optimization level (default 6 is the max, 0 = no optimization)"
  XEN_ASSERT_TYPE(s7_is_integer(val), val, 1, S_setB S_optimization, "an integer");
  set_optimization(mus_iclamp(0, (int)s7_number_to_integer(val), 6)); 
  /* set_optimization(0); */
  optimizing = (optimization(ss) > 0);
  return(val);
}

#else

#define S_optimization "optimization"

static s7_pointer g_optimization(void) {return(s7_make_integer(s7, (optimizing) ? 6 : 0));} /* 6 here for backwards compatibility */

static s7_pointer g_set_optimization(s7_pointer val) 
{
  int ival;
  #define H_optimization "(" S_optimization "): the current 'run' optimization level (default 6 is the max, 0 = no optimization)"
  XEN_ASSERT_TYPE(s7_is_integer(val), val, 1, S_setB S_optimization, "an integer");
  ival = mus_iclamp(0, (int)s7_number_to_integer(val), 6);
  optimizing = (ival > 0);
  return(val);
}

#endif


#define S_snd_declare "snd-declare"

static s7_pointer g_snd_declare(s7_pointer args)
{
  #define H_snd_declare "this is an internal kludge aimed mainly at backwards compatibility"
  return(scheme_false);
}


#ifdef XEN_ARGIFY_1
XEN_NARGIFY_0(g_optimization_w, g_optimization)
XEN_NARGIFY_1(g_set_optimization_w, g_set_optimization)
XEN_NARGIFY_1(g_snd_declare_w, g_snd_declare)
XEN_NARGIFY_1(g_run_w, g_run)
XEN_NARGIFY_1(g_show_ptree_w, g_show_ptree)
XEN_NARGIFY_4(g_add_clm_field_w, g_add_clm_field)
XEN_ARGIFY_4(g_run_eval_w, g_run_eval)
#if WITH_COUNTERS
XEN_NARGIFY_0(g_report_counts_w, g_report_counts)
XEN_NARGIFY_0(g_clear_counts_w, g_clear_counts)
#endif
#else
#define g_optimization_w g_optimization
#define g_set_optimization_w g_set_optimization
#define g_snd_declare_w g_snd_declare
#define g_run_w g_run
#define g_show_ptree_w g_show_ptree
#define g_add_clm_field_w g_add_clm_field
#define g_run_eval_w g_run_eval
#if WITH_COUNTERS
#define g_report_counts_w, g_report_counts
#define g_clear_counts_w, g_clear_counts
#endif
#endif


void mus_init_run(void);
void mus_init_run(void)
{
  static bool run_inited = false;
  if (run_inited) fprintf(stderr, "redundant run initialization?");
  run_inited = true;

  scheme_false = s7_f(s7);
  scheme_true = s7_t(s7);
  scheme_nil = s7_nil(s7);
  scheme_undefined = s7_undefined(s7);
  scheme_zero = s7_make_integer(s7, 0);

  s7_define_function(s7, "__run__",       g_run_w,           1, 0, 0, H_run);
  s7_define_function(s7, "run-eval",      g_run_eval_w,      1, 3, 0, H_run);

  s7_eval_c_string(s7, "(define-macro (run . code)        \n\
                       (if (and (symbol? (caar code)) (string=? (symbol->string (caar code)) \"lambda\"))   \n\
                          `(__run__ ,@code)            \n\
                          `(__run__ (lambda () ,@code))))");

  walker_hash_table = s7_make_hash_table(s7, 1031);
  s7_gc_protect(s7, walker_hash_table);

  s7_define_function(s7, S_add_clm_field, g_add_clm_field_w, 4, 0, 0, H_add_clm_field);
  s7_define_function(s7, S_show_ptree,    g_show_ptree_w,    1, 0, 0, H_show_ptree);

#if WITH_COUNTERS
  s7_define_function(s7, "run-report-counts", g_report_counts_w, 0, 0, 0, "run stats");
  s7_define_function(s7, "run-clear-counts", g_clear_counts_w, 0, 0, 0, "clear run stats");
#endif
		       
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_optimization, g_optimization_w, H_optimization, S_setB S_optimization, g_set_optimization_w,  0, 0, 1, 0);
  s7_define_function(s7, S_snd_declare, g_snd_declare_w, 1, 0, 0, H_snd_declare);

  s7_eval_c_string(s7, "(defmacro declare args `(snd-declare ',args))");
  /* declare has to be a macro, else it evaluates its arguments in the fallback-on-scheme case
   *   however, we want it to still be findable on an external function (when using procedure-source to splice in the body)
   *   so in the optimizer case, it sees the original "declare" form, but in the scheme case, the snd-declare (a no-op),
   *   and in the optimizer-splicing-source case, the snd-declare, I hope.
   */

#if USE_SND
  XEN_ADD_HOOK(ss->mus_error_hook, watch_for_mus_error_in_run, "run-mus-error-handler", "run macro's mus-error handler");
#endif

  #define H_optimization_hook S_optimization_hook " (message): called if the run macro encounters \
something it can't optimize.  'msg' is a string description of the offending form:\n\
  (hook-push " S_optimization_hook " (lambda (hook) (format #t \"run trouble: ~A~%\" (hook 'message))))\n\
You can often slightly rewrite the form to make run happy."

  optimization_hook = XEN_DEFINE_HOOK(S_optimization_hook, "(make-hook 'message)", 1, H_optimization_hook);

#if (!USE_SND)
  optimizing = true;
#endif

  init_walkers();
  init_type_names();

  {
    int i;
    s7_pointer *vals;
    vals = s7_vector_elements(walker_hash_table);
    for (i = 0; i < s7_vector_length(walker_hash_table); i++)
      s7_remove_from_heap(s7, vals[i]);
    /* removing the hash table itself is problematic because (for example) add_clm_field adds new entries */
  }

}


/* -------------------------------------------------------------------------------- */
/* not s7, make some no-ops so other languages have some hope of compiling */

#else

#include "_sndlib.h"
#include "xen.h"
#include "vct.h"

#define S_run "run"

/* XEN here, not s7_pointer in case no s7 */

#define S_optimization "optimization"
static XEN g_optimization(void) {return(XEN_ZERO);}
static XEN g_set_optimization(XEN val) {return(XEN_ZERO);}

static XEN optimization_hook;

#ifdef XEN_ARGIFY_1
XEN_NARGIFY_0(g_optimization_w, g_optimization)
XEN_NARGIFY_1(g_set_optimization_w, g_set_optimization)
#else
#define g_optimization_w g_optimization
#define g_set_optimization_w g_set_optimization
#endif

void mus_init_run(void)
{
  XEN_DEFINE_PROCEDURE_WITH_SETTER("optimization", g_optimization_w, "a no-op", S_setB S_optimization, g_set_optimization_w,  0, 0, 1, 0);
  optimization_hook = XEN_DEFINE_HOOK("optimization-hook", "(make-hook 'message)", 1, "a no-op");
}

#endif
