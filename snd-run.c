/* run macro
 *   initial timing tests indicate that this is 10 times as fast as Guile.
 *   Rather than write/compile (via gcc) a C source file, as in CLM, this
 *   produces the intermediate "triples" on the fly, packaging them into
 *   a "program" (a list of triples), and precomputing all function, program,
 *   and data addresses.
 *
 * The evaluator is eval_ptree.  The code walker is walk.  A program
 *   is a list of triples. Each triple has a function pointer and a
 *   pointer to addresses of arguments and data.  There are two special addresses:
 *   the program counter (PC) and the termination flag (ALL_DONE). 
 *
 * Snd optimization flag determines how safe we try to be:
 *   0: no use of ptrees at all (fallback on Guile)
 *   1: allow simple ops (if complex result possible, give up)
 *   2: assume nothing will return a complex number (i.e. user says acos args are between -1 and 1 and so on)
 *   3: if undefined global variable encountered, try to determine eventual type from context;
 *      this is dangerous -- the tree may not recognize trouble until evaluation time.
 *   4: make more assumptions about non-local variables -- lots of errors will be unnoticed until eval time.
 *   5: try to set variable value in outer environment
 *   6: try to splice in function source
 *
 *
 * exported:
 *      (static void *form_to_ptree(XEN code) parse code, returning pointer to tree (a list) or null if code has something we can't handle)
 *   void *form_to_ptree_1_f(XEN code) -- (1 arg) adds type check that result is Float
 *   void *form_to_ptree_0_f(XEN code) -- (no args) adds type check that result is Float
 *   void *form_to_ptree_1_b(XEN code) -- (1 arg) adds type check that result is boolean
 *   Float evaluate_ptree_1f2f(void *tree, Float arg)
 *     evaluate ptree passing it the single Float arg, returning a Float result
 *   Float evaluate_ptree_0f2f(void *tree, Float arg)
 *     evaluate ptree (no args), returning a Float result
 *   Float evaluate_ptree_1f2b(void *tree, Float arg)
 *     evaluate ptree passing it the single Float arg, returning a boolean result
 *   void *free_ptree(void *pt)
 *     release resources allocated to ptree
 *
 *
 * currently handled, at least partially:
 *
 *   types: float int char string boolean symbol keyword vct snd_fd mus_any vector function [and list constants]
 *
 *   lambda (use 'declare' to set arg types)
 *   call-with-current-continuation call/cc
 *   if begin or and not let let* set! cond do define case[int keys]
 *   * + - / > < >= <= = max min 1+ 1-
 *   sin cos tan abs log exp expt acos asin atan sqrt
 *   boolean? exact? inexact? integer? real? number? quote
 *   odd? even? zero? positive? negative? eq? eqv? equal? symbol? symbol->string
 *   round truncate floor ceiling exact->inexact inexact->exact
 *   gcd lcm logand logior logxor lognot ash modulo remainder quotient random
 *   char? char=? char<? char>? char<=? char>=? and char-ci*
 *   char-alphabetic? char-numeric? char-lower-case? char-upper-case? char-whitespace? 
 *   char-upcase char-downcase char->integer integer->char
 *   string? string string-length string-copy string-fill! string-ref string-set!
 *   make-string substring string-append string=? string<=? string>=? string<? string>? and string-ci*
 *   display number->string format[as a callback into Guile]
 *   make-vector if 2nd arg exists and is float
 *   list|pair ops that reference constant lists and return something we can handle (like a number)
 *   throw with just the 1st arg (experimental...)
 *
 *   various sndlib, clm, and snd functions
 *
 * LIMITATIONS: <insert anxious lucubration here about DSP context and so on>
 *      variables can have only one type, the type has to be ascertainable somehow (similarly for vector elements)
 *      some variables (imported from outside our context) cannot be set, in some cases they can't even be found (args to define* for example)
 *      no recursion (could be added with some pain)
 *      no [variable] lists or pairs
 *      no macro expansion (not sure how to handle this in Guile)
 *      no complex, ratio, bignum (but we use 64-bit ints)
 *      no pointer aliasing (i.e. vct var set to alias another vct var etc -- GC confusion otherwise)
 *      no apply or eval (we need to know at parse time what we are trying to do -- actually these might be doable)
 *      no "delay/force", no syntax-case fanciness
 *      no map or for-each (these need lists)
 *
 * whenever the code-walker or tree initializer finds something it is unhappy about,
 *  it returns an error indication, and the caller should fallback on Guile's evaluator.
 *
 * so where does the speed-up come from? We're not getting/freeing any Guile memory so the gc is never
 *   triggered, we're doing math ops direct, no function args are cons'd, no run-time types are checked, no values are boxed/unboxed,
 *   no symbols are looked-up in the current environment, wherever possible we pre-convert
 *   args to same type (i.e. int->float done just once, if possible)
 */

#include "snd.h"
#include "sndlib2xen.h"
#include "clm2xen.h"
#include "clm-strings.h"
#include "sndlib-strings.h"

static XEN optimization_hook;

#define S_run_safety "run-safety"
enum {RUN_UNSAFE, RUN_SAFE};
static int run_safety = RUN_UNSAFE;

#if HAVE_GUILE && WITH_RUN && HAVE_STRINGIZE

#define Int off_t
#define INT_PT  "i%d(%lld)"
#define INT_STR "%lld"
#define R_C_TO_XEN_INT C_TO_XEN_OFF_T
#define R_XEN_TO_C_INT XEN_TO_C_OFF_T
#define Double double

#define DONT_OPTIMIZE 0
#define OMIT_COMPLEX 1
#define COMPLEX_OK 2
#define GLOBAL_OK 3
#define GLOBAL_SET_OK 5
#define SOURCE_OK 6

#define XEN_CDDDR(a)                        SCM_CDDDR(a)
#define XEN_CAAR(a)                         XEN_CAR(XEN_CAR(a))
#define XEN_CDAR(a)                         XEN_CDR(XEN_CAR(a))
#define XEN_CDADR(a)                        XEN_CDR(XEN_CADR(a))
#define XEN_CAAAR(a)                        SCM_CAAAR(a)
#define XEN_CAADR(a)                        SCM_CAADR(a)
#define XEN_CADAR(a)                        SCM_CADAR(a)
#define XEN_CADDR(a)                        SCM_CADDR(a)
#define XEN_CAAAAR(a)                       SCM_CAAAAR(a)
#define XEN_CAAADR(a)                       SCM_CAAADR(a)
#define XEN_CAADAR(a)                       SCM_CAADAR(a)
#define XEN_CAADDR(a)                       SCM_CAADDR(a)
#define XEN_CADAAR(a)                       SCM_CADAAR(a)
#define XEN_CADADR(a)                       SCM_CADADR(a)
#define XEN_CADDAR(a)                       SCM_CADDAR(a)
#define XEN_CADDDR(a)                       SCM_CADDDR(a)
#define XEN_APPLICABLE_SMOB_P(a)            (SCM_TYP7(a) == scm_tc7_smob)
#define XEN_ENV(a)                          SCM_ENV(a)
#define XEN_SET_CDR(pair, new_val)          scm_set_cdr_x(pair, new_val)
#define XEN_FRANDOM(a)                      mus_frandom(a)
#define XEN_IRANDOM(a)                      mus_irandom(a)
#define INTEGER_TO_STRING(a)                XEN_TO_C_STRING(scm_number_to_string(R_C_TO_XEN_INT(a), XEN_UNDEFINED))
#define INTEGER_TO_STRING_WITH_RADIX(a, b)  XEN_TO_C_STRING(scm_number_to_string(R_C_TO_XEN_INT(a), R_C_TO_XEN_INT(b)))
#define DOUBLE_TO_STRING(a)                 XEN_TO_C_STRING(scm_number_to_string(C_TO_XEN_DOUBLE(a), XEN_UNDEFINED))
#define DOUBLE_TO_STRING_WITH_RADIX(a, b)   XEN_TO_C_STRING(scm_number_to_string(C_TO_XEN_DOUBLE(a), R_C_TO_XEN_INT(b)))
#define XEN_LIST_REF_WRAPPED(a, b)          scm_list_ref(a, b)
#define XEN_OBJECT_PROPERTY(Obj, Prop)      scm_object_property(Obj, Prop)
#define XEN_SET_OBJECT_PROPERTY(Obj, Prop, Val) scm_set_object_property_x(Obj, Prop, Val)
#define XEN_PROCEDURE_PROPERTY(Obj, Prop)   scm_procedure_property(Obj, Prop)
#define XEN_SET_PROCEDURE_PROPERTY(Obj, Prop, Val) scm_set_procedure_property_x(Obj, Prop, Val)
#define XEN_PROCEDURE_WITH_SETTER_P(Proc)   scm_procedure_with_setter_p(Proc)

#define FLT_PT  "d%d(%.4f)"
#define PTR_PT  "i%d(%p)"
#define STR_PT  "s%d(\"%s\")"
#define CHR_PT  "i%d(#\\%c)"
#define XEN_PT  "x%d(%p)"
#define LST_PT  "x%d(%p)"
#define KEY_PT  "x%d(%p)"
#define RD_PT   "r%d(%p)"
#define MF_PT   "m%d(%p)"
#define TF_PT   "t%d(%p)"
#define CLM_PT  "c%d(%p)"
#define FNC_PT  "f%d(%p)"
#define VCT_PT  "v%d(%p)"
#define SD_PT   "a%d(%p)"
#define GO_PT   "i%d(continuation)"
#define BOOL_PT "i%d(%s)"
#define VECT_PT "m%d(%p)"
#define B2S(Arg) ((Arg) ? "#t" : "#f")

#define UNLIMITED_ARGS -1
static XEN walk_sym = XEN_FALSE;

/* find and set (Guile) variable values */

static void xen_symbol_name_set_value(const char *a, XEN b)
{
  XEN var = XEN_FALSE;
  var = XEN_NAME_AS_C_STRING_TO_VARIABLE(a);
  if (!(XEN_FALSE_P(var)))
    XEN_VARIABLE_SET(var, b);
}

static XEN symbol_to_value(XEN code, XEN sym, bool *local)
{
  XEN new_val = XEN_UNDEFINED;
  XEN pair = XEN_FALSE;
  XEN code_env = XEN_FALSE;
  XEN val;
  XEN names, values;
  int i, len;
  if (XEN_PROCEDURE_P(code))
    {
      /* scrounge around in the "eval" environment looking for local version of sym */
      code_env = XEN_ENV(code);
      /* fprintf(stderr, "look for %s in %s\n", XEN_AS_STRING(sym), XEN_AS_STRING(code_env)); */
      if (XEN_LIST_P(code_env))
	{
	  (*local) = true;
	  while (XEN_NOT_NULL_P(code_env))
	    {
	      pair = XEN_CAR(code_env);
	      if ((XEN_LIST_P(pair)) && 
		  (XEN_LIST_P(XEN_CAR(pair))))
		{
		  names = XEN_CAR(pair);
		  values = XEN_CDR(pair);
		  len = XEN_LIST_LENGTH(names);
		  for (i = 0; i < len; i++)
		    if (XEN_EQ_P(XEN_LIST_REF(names, i), sym))
		      return(XEN_LIST_REF(values, i));
		}
	      else
		{
		  if ((XEN_PAIR_P(pair)) && 
		      (XEN_EQ_P(XEN_CAR(pair), sym)))
		    return(XEN_CDR(pair));
		}
	      code_env = XEN_CDR(code_env);
	    }
	}
    }
  val = XEN_SYMBOL_TO_VARIABLE(sym);
  if (!(XEN_FALSE_P(val))) 
    {
      new_val = XEN_VARIABLE_REF(val);
      if (XEN_BOUND_P(new_val))
	(*local) = false;
    }
  return(new_val);
}

static XEN symbol_set_value(XEN code, XEN sym, XEN new_val)
{
  XEN pair = XEN_FALSE;
  XEN code_env = XEN_FALSE;
  XEN names, values;
  XEN var = XEN_FALSE;
  int i, len;
  /* fprintf(stderr, "set %s to %s\n", XEN_AS_STRING(sym), XEN_AS_STRING(new_val)); */
  if (XEN_PROCEDURE_P(code))
    {
      code_env = XEN_ENV(code);
      if (XEN_LIST_P(code_env))
	{
	  while (XEN_NOT_NULL_P(code_env))
	    {
	      pair = XEN_CAR(code_env);
	      if ((XEN_LIST_P(pair)) && (XEN_LIST_P(XEN_CAR(pair))))
		{
		  names = XEN_CAR(pair);
		  values = XEN_CDR(pair);
		  len = XEN_LIST_LENGTH(names);
		  for (i = 0; i < len; i++)
		    if (XEN_EQ_P(XEN_LIST_REF(names, i), sym))
		      {
			XEN_LIST_SET(values, i, new_val);
			return(new_val);
		      }
		}
	      if ((XEN_PAIR_P(pair)) && (XEN_EQ_P(XEN_CAR(pair), sym)))
		{
		  XEN_SET_CDR(pair, new_val);
		  return(new_val);
		}
	      code_env = XEN_CDR(code_env);
	    }
	}
    }
  var = XEN_SYMBOL_TO_VARIABLE(sym);
  if (!(XEN_FALSE_P(var)))
    XEN_VARIABLE_SET(var, new_val);
  return(new_val);
}

enum {R_UNSPECIFIED, R_INT, R_FLOAT, R_BOOL, R_CHAR, R_STRING, R_LIST, R_PAIR, 
      R_SYMBOL, R_KEYWORD, R_FUNCTION, R_GOTO, R_VCT, 
      R_READER, R_MIX_READER, R_TRACK_READER, R_SOUND_DATA,
      R_CLM, R_FLOAT_VECTOR, R_INT_VECTOR, R_VCT_VECTOR, R_CLM_VECTOR, 
      R_NUMBER, R_CONS, R_VECTOR, R_XEN, R_ANY}; /* last 5 for walker arg checks */


#define BUILT_IN_TYPES 27
static int last_type = R_ANY;
static int type_names_size = BUILT_IN_TYPES;
static char **type_names = NULL;
static char *basic_type_names[BUILT_IN_TYPES] = {"unspecified", "int", "float", "boolean", "char", "string", "list", "pair", 
						 "symbol", "keyword", "function", "continuation", "vct", 
						 "sample-reader", "mix-sample-reader", "track-sample-reader",
						 "sound-data", "clm", 
						 "float-vector", "int-vector", "vct-vector", "clm-vector", 
						 "number", "cons", "vector", "xen", "any"};
static void init_type_names(void)
{
  int i;
  type_names = (char **)CALLOC(BUILT_IN_TYPES, sizeof(char *));
  for (i = 0; i < BUILT_IN_TYPES; i++)
    type_names[i] = basic_type_names[i];
}
static char* type_name(int id) {if ((id >= R_UNSPECIFIED) && (id <= last_type)) return(type_names[id]); return("unknown");}
static int add_new_type(const char *new_type)
{
  int i;
  if (last_type == (type_names_size - 1))
    {
      type_names_size += 8;
      type_names = (char **)REALLOC(type_names, type_names_size * sizeof(char *));
      for (i = last_type + 1; i < type_names_size; i++) type_names[i] = NULL;
    }
  last_type++;
  type_names[last_type] = copy_string(new_type);
  return(last_type);
}
static int name_to_type(const char *name)
{
  int i;
  for (i = 0; i <= last_type; i++)
    if (strcmp(name, type_names[i]) == 0)
      return(i);
  return(R_UNSPECIFIED);
}

#define POINTER_P(Type) (((Type) >= R_VCT) && ((Type) <= R_CLM_VECTOR)) /* exclude R_ANY */
#define POINTER_OR_GOTO_P(Type) ((Type) > R_FUNCTION)
#define VECTOR_P(Type) (((Type) >= R_FLOAT_VECTOR) && ((Type) <= R_CLM_VECTOR))

typedef enum {R_VARIABLE, R_CONSTANT} xen_value_constant_t;
typedef enum {DONT_NEED_RESULT, NEED_ANY_RESULT, NEED_INT_RESULT} walk_result_t;

static int current_optimization = DONT_OPTIMIZE;
static bool run_warned = false;

typedef struct {
  int type;
  int addr;
  xen_value_constant_t constant;
  bool gc;
} xen_value;

typedef struct {
  char *name;
  xen_value *v;
  bool global, unclean, unsettable;
} xen_var;

typedef struct {
  char *name;
  xen_value *result, *jump;
  int loc;
} continuation;

typedef struct {
  int length;
  union {
    Int *ints;
    mus_any **gens;
    vct **vcts;
  } data;
} vect;


#ifdef __cplusplus
/* need struct with forward reference fields... */
struct ptree;
struct triple;

struct ptree {
  triple **program;
  Int *ints; 
  Double *dbls;
  int program_size, ints_size, dbls_size, triple_ctr, int_ctr, dbl_ctr;
  xen_var **vars;
  int vars_size, var_ctr;
  xen_var **global_vars;
  int global_vars_size, global_var_ctr;
  xen_value *result;
  int *args; 
  int *arg_types;
  int arity;
  continuation **gotos;
  int goto_ctr, gotos_size;
  xen_value **gcs;
  int gc_ctr, gcs_size;
  int *gc_protected;
  int gc_protected_ctr, gc_protected_size;
  int initial_pc;
  XEN code, form;
  int form_loc;
  int str_ctr, strs_size;
  char **strs;
  int vct_ctr, vcts_size;
  vct **vcts;
  int sd_ctr, sds_size;
  sound_data **sds;
  int clm_ctr, clms_size;
  mus_any **clms;
  int vect_ctr, vects_size;
  vect **vects;
  int reader_ctr, readers_size;
  snd_fd **readers;
  int mix_reader_ctr, mix_readers_size;
  void **mix_readers;
  int track_reader_ctr, track_readers_size;
  void **track_readers;
  int fnc_ctr, fncs_size;
  struct ptree **fncs;
  int xen_ctr, xens_size;
  XEN *xens;
  int xen_vars_size, xen_var_ctr;
  xen_value ***xen_vars;
  ptree *outer_tree;
  /* next needed by tree builders (temps) */
  int constants;
  bool float_result;
  walk_result_t walk_result;
};

struct triple {
  void (*function)(int *arg_addrs, ptree *pt);
  int *args;
  char *(*descr)(int *arg_addrs, ptree *pt); /* for debugging */
  bool no_opt;
};

#else

typedef struct ptree {
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
  int *arg_types;
  int arity;
  continuation **gotos;
  int goto_ctr, gotos_size;
  xen_value **gcs;
  int gc_ctr, gcs_size;
  int *gc_protected;
  int gc_protected_ctr, gc_protected_size;
  int initial_pc;
  XEN code, form;
  int form_loc;
  int str_ctr, strs_size;
  char **strs;
  int vct_ctr, vcts_size;
  vct **vcts;
  int sd_ctr, sds_size;
  sound_data **sds;
  int clm_ctr, clms_size;
  mus_any **clms;
  int vect_ctr, vects_size;
  vect **vects;
  int reader_ctr, readers_size;
  snd_fd **readers;
  int mix_reader_ctr, mix_readers_size;
  void **mix_readers;
  int track_reader_ctr, track_readers_size;
  void **track_readers;
  int fnc_ctr, fncs_size;
  struct ptree **fncs;
  int xen_ctr, xens_size;
  XEN *xens;
  int xen_vars_size, xen_var_ctr;
  xen_value ***xen_vars;
  struct ptree *outer_tree;
  /* next needed by tree builders (temps) */
  int constants;
  bool float_result;
  walk_result_t walk_result;
} ptree;

typedef struct {
  void (*function)(int *arg_addrs, ptree *pt);
  int *args;
  char *(*descr)(int *arg_addrs, ptree *pt); /* for debugging */
  bool no_opt;
} triple;

#endif

static int allocate_xen_vars(ptree *pt, int size)
{
  int i, cur;
  cur = pt->xen_var_ctr;
  if (cur >= pt->xen_vars_size)
    {
      pt->xen_vars_size += 4;
      if (pt->xen_vars)
	{
	  pt->xen_vars = (xen_value ***)REALLOC(pt->xen_vars, pt->xen_vars_size * sizeof(xen_value **));
	  for (i = cur; i < pt->xen_vars_size; i++)
	    pt->xen_vars[i] = NULL;
	}
      else pt->xen_vars = (xen_value ***)CALLOC(pt->xen_vars_size, sizeof(xen_value **));
    }
  pt->xen_vars[pt->xen_var_ctr++] = (xen_value **)CALLOC(size, sizeof(xen_value *));
  return(cur);
}

static triple *free_triple(triple *trp)
{
  if (trp->args) FREE(trp->args);
  trp->args = NULL;
  FREE(trp);
  return(NULL);
}

static char *describe_triple(triple *trp, ptree *pt)
{
  if (trp->descr)
    return((*(trp->descr))(trp->args, pt));
  return(NULL);
}

static xen_value *make_xen_value(int typ, int address, xen_value_constant_t constant)
{
  xen_value *v;
  v = (xen_value *)CALLOC(1, sizeof(xen_value));
  v->type = typ;
  v->addr = address;
  v->constant = constant;
  v->gc = false;
  return(v);
}

#define OPTIMIZER_WARNING_BUFFER_SIZE 1024
static char optimizer_warning_buffer[OPTIMIZER_WARNING_BUFFER_SIZE];

#ifdef __GNUC__
static xen_value *run_warn(const char *format, ...) __attribute ((format (printf, 1, 2)));
#endif

static xen_value *run_warn(const char *format, ...)
{
  va_list ap;
  run_warned = true;
#if HAVE_VPRINTF
  va_start(ap, format);
#if HAVE_VSNPRINTF
  vsnprintf(optimizer_warning_buffer, OPTIMIZER_WARNING_BUFFER_SIZE, format, ap);
#else
  vsprintf(optimizer_warning_buffer, format, ap);
#endif
  va_end(ap);
  if (XEN_HOOKED(optimization_hook))
    run_hook(optimization_hook, 
	     XEN_LIST_1(C_TO_XEN_STRING(optimizer_warning_buffer)),
	     S_optimization_hook);
#endif
  return(NULL); /* this is so we can insert the call into the error return call chain */
}

static xen_value *run_warn_with_free(char *str)
{
  XEN msg;
  run_warned = true;
  msg = C_TO_XEN_STRING(str);
  FREE(str);
  if (XEN_HOOKED(optimization_hook))
    run_hook(optimization_hook, 
	     XEN_LIST_1(msg),
	     S_optimization_hook);
  return(NULL);
}

static xen_value *copy_xen_value(xen_value *v)
{
  return(make_xen_value(v->type, v->addr, v->constant));
}

static char *describe_xen_value(xen_value *v, ptree *pt);

static char *describe_xen_var(xen_var *var, ptree *pt)
{
  char *buf, *temp;
  if (var == NULL) return(copy_string("#<xen_var: null>"));
  temp = describe_xen_value(var->v, pt);
  if (temp)
    {
      buf = (char *)CALLOC(strlen(var->name) + strlen(temp) + 32, sizeof(char));
      sprintf(buf, "%s: %s (%s%s%s)", 
	      var->name, temp,
	      (var->global) ? "global" : "local",
	      (var->unclean) ? " set" : "",
	      (var->unsettable) ? " unsettable" : "");
      FREE(temp);
    }
  else buf = copy_string(var->name);
  return(buf);
}

static continuation *free_goto(continuation *c)
{
  if (c)
    {
      if (c->name) FREE(c->name);
      if (c->result) FREE(c->result);
      if (c->jump) FREE(c->jump);
      FREE(c);
    }
  return(NULL);
}

#define INITIAL_INT_CTR 2

static xen_var *find_var_in_ptree(ptree *pt, const char *name)
{
  /* search backwards for shadowing */
  int i;
  for (i = pt->var_ctr - 1; i >= 0; i--)
    if ((pt->vars[i]) &&
	(strcmp(pt->vars[i]->name, name) == 0))
      return(pt->vars[i]);
  for (i = 0; i < pt->global_var_ctr; i++)
    if ((pt->global_vars[i]) &&
	(strcmp(pt->global_vars[i]->name, name) == 0))
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

static char *add_comments(ptree *pt, char *str)
{
  xen_var *var;
  int addr, i, j, len;
  char *new_buf;
  char name[64];
  bool name_pending = false;
  if (str == NULL) return(NULL);
  /* look for i%n(...) or d%n(...), if found, look for addr in pt tables, if found add name to buf */
  len = snd_strlen(str);
  new_buf = (char *)CALLOC(len + 256, sizeof(char));
  for (i = 0, j = 0; i < len; i++)
    {
      if (((str[i] != 'i') && (str[i] != 'd')) ||
	  (!(isdigit((int)(str[i + 1])))))
	{
	  new_buf[j++] = str[i];
	  if ((name_pending) && (str[i] == ')'))
	    {
	      int k, name_len;
	      name_len = snd_strlen(name);
	      new_buf[j++] = '[';
	      for (k = 0; k < name_len; k++)
		new_buf[j++] = name[k];
	      new_buf[j++] = ']';
	      name_pending = false;
	      name[0] = '\0';
	    }
	}
      else
	{
	  int k;
	  addr = 0;
	  k = i + 1;
	  do
	    {
	      addr = 10 * addr + (str[k++] - '0');
	    }
	  while (isdigit((int)(str[k])));
	  if (str[k] == '(')
	    {
	      var = find_var_in_ptree_via_addr(pt, 
					       ((str[i] == 'd') ? R_FLOAT : 
						((str[i] == 's') ? R_STRING : 
						 ((str[i] == 'v') ? R_VCT :
						  ((str[i] == 'c') ? R_CLM :
						   ((str[i] == 'r') ? R_READER :
						    ((str[i] == 'm') ? R_MIX_READER :
						     ((str[i] == 't') ? R_TRACK_READER :
						      ((str[i] == 'a') ? R_SOUND_DATA :
						       ((str[i] == 'f') ? R_FUNCTION :
							((str[i] == 'x') ? R_LIST :
							 R_INT)))))))))),
					       addr);
	      if (var)
		{
		  strcpy(name, var->name);
		  name_pending = true;
		}
	    }
	  new_buf[j++] = str[i];
	}
    }
  FREE(str);
  return(new_buf);
}

static char *str_append(char *oldstr, int *oldsize, char *newstr)
{
  oldstr = snd_strcat(oldstr, newstr, oldsize);
  FREE(newstr);
  return(oldstr);
}

XEN ptree_code(void *pt)
{
  return(((ptree *)pt)->form);
}

static char *describe_ptree(ptree *pt)
{
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
  snd_fd **inner_readers;
  void **inner_mix_readers;
  void **inner_track_readers;
  ptree **inner_fncs;
  XEN *inner_xens;
  xen_value ***inner_xen_vars;
  size = 1024;
  buf = (char *)CALLOC(size, sizeof(char));
  inner_ints = pt->ints;
  inner_dbls = pt->dbls;
  inner_xen_vars = pt->xen_vars;
  inner_strs = pt->strs;
  inner_vcts = pt->vcts;
  inner_sds = pt->sds;
  inner_clms = pt->clms;
  inner_vects = pt->vects;
  inner_fncs = (ptree **)(pt->fncs);
  inner_xens = pt->xens;
  inner_readers = pt->readers;
  inner_mix_readers = pt->mix_readers;
  inner_track_readers = pt->track_readers;
  if (pt->outer_tree)
    {
      pt->ints = ((ptree *)(pt->outer_tree))->ints;
      pt->dbls = ((ptree *)(pt->outer_tree))->dbls;
      pt->strs = ((ptree *)(pt->outer_tree))->strs;
      pt->vcts = ((ptree *)(pt->outer_tree))->vcts;
      pt->sds = ((ptree *)(pt->outer_tree))->sds;
      pt->clms = ((ptree *)(pt->outer_tree))->clms;
      pt->vects = ((ptree *)(pt->outer_tree))->vects;
      pt->fncs = ((ptree *)(pt->outer_tree))->fncs;
      pt->xens = ((ptree *)(pt->outer_tree))->xens;
      pt->readers = ((ptree *)(pt->outer_tree))->readers;
      pt->mix_readers = ((ptree *)(pt->outer_tree))->mix_readers;
      pt->track_readers = ((ptree *)(pt->outer_tree))->track_readers;
      pt->xen_vars = ((ptree *)(pt->outer_tree))->xen_vars;
    }
  buf = str_append(buf, &size, mus_format("ints: %d, dbls: %d, triples: %d, vars: %d\n  [",
					  pt->int_ctr, pt->dbl_ctr, pt->triple_ctr, pt->var_ctr));
  if (pt->int_ctr > 0) 
    {
      for (i = 0; i < pt->int_ctr - 1; i++)
	{
	  temp = (char *)CALLOC(16, sizeof(char));
	  mus_snprintf(temp, 16, INT_STR ", ", pt->ints[i]);
	  buf = str_append(buf, &size, temp);
	}
      temp = (char *)CALLOC(16, sizeof(char));
      mus_snprintf(temp, 16, INT_STR "]%s", 
		   pt->ints[pt->int_ctr - 1],
		   (pt->dbl_ctr > 0) ? ", [" : "\n");
      buf = str_append(buf, &size, temp);
    }
  if (pt->dbl_ctr > 0)
    {
      for (i = 0; i < pt->dbl_ctr - 1; i++)
	{
	  temp = (char *)CALLOC(16, sizeof(char));
	  mus_snprintf(temp, 16, "%.4f, ", pt->dbls[i]);
	  buf = str_append(buf, &size, temp);
	}
      temp = (char *)CALLOC(16, sizeof(char));
      mus_snprintf(temp, 16, "%.4f]\n", pt->dbls[pt->dbl_ctr - 1]);
      buf = str_append(buf, &size, temp);
    }
  for (i = 0; i < pt->triple_ctr; i++)
    {
      temp = add_comments(pt, describe_triple(((triple **)(pt->program))[i], pt));
      if (temp)
	{
	  buf = str_append(buf, &size, mus_format("  %d: %s\n", i, temp));
	  FREE(temp);
	}
    }
  strcat(buf, "\n");
  for (i = 0; i < pt->var_ctr; i++)
    {
      temp = describe_xen_var(pt->vars[i], pt);
      buf = str_append(buf, &size, mus_format("[var %d]: %s\n", i, temp));
      FREE(temp);
    }
  for (i = 0; i < pt->global_var_ctr; i++)
    {
      temp = describe_xen_var(pt->global_vars[i], pt);
      buf = str_append(buf, &size, mus_format("[global_var %d]: %s\n", i, temp));
      FREE(temp);
    }
  if (pt->result)
    {
      temp = describe_xen_value(pt->result, pt);
      if (temp)
	{
	  buf = str_append(buf, &size, mus_format("\nresult: %s\n", temp));
	  FREE(temp);
	}
    }
  buf = str_append(buf, &size, mus_format("PC: " INT_STR " (%d)\n", pt->ints[0], pt->initial_pc));
  pt->ints = inner_ints;
  pt->dbls = inner_dbls;
  pt->xen_vars = inner_xen_vars;
  pt->strs = inner_strs;
  pt->vcts = inner_vcts;
  pt->sds = inner_sds;
  pt->clms = inner_clms;
  pt->vects = inner_vects;
  pt->fncs = (struct ptree **)(inner_fncs);
  pt->xens = inner_xens;
  pt->readers = inner_readers;
  pt->mix_readers = inner_mix_readers;
  pt->track_readers = inner_track_readers;
  return(buf);
}

static char *describe_xen_value_1(int type, int addr, ptree *pt)
{
  switch (type)
    {
    case R_BOOL:    return(mus_format(BOOL_PT, addr, B2S(pt->ints[addr])));             break;
    case R_INT:     return(mus_format(INT_PT , addr, pt->ints[addr]));                  break;
    case R_CHAR:    return(mus_format(CHR_PT , addr, (char)(pt->ints[addr])));          break;
    case R_STRING:  return(mus_format(STR_PT , addr, pt->strs[addr]));                  break;
    case R_FLOAT:   return(mus_format(FLT_PT , addr, pt->dbls[addr]));                  break;
    case R_GOTO:    return(mus_format("continuation: " INT_PT , addr, pt->ints[addr])); break;
    case R_LIST:
    case R_PAIR:
    case R_SYMBOL:
    case R_KEYWORD: 
      if ((pt->xens) && (XEN_BOUND_P(pt->xens[addr])))
	return(mus_format("x%d(%s)", addr, XEN_AS_STRING(pt->xens[addr]))); 
      else return(mus_format("x(%d)=0", addr));
      break;
    case R_FLOAT_VECTOR:
    case R_VCT:
      if ((pt->vcts) && (pt->vcts[addr]))
	{
	  char *buf = NULL, *vstr = NULL;
	  vstr = vct_to_string(pt->vcts[addr]);
	  buf = mus_format(VCT_PT, addr, vstr);
	  if (vstr) FREE(vstr);
	  return(buf);
	}
      else return(mus_format("v%d(null)", addr));
      break;
    case R_SOUND_DATA:
      if ((pt->sds) && (pt->sds[addr]))
	{
	  char *buf = NULL, *vstr = NULL;
	  vstr = sound_data_to_string(pt->sds[addr]);
	  buf = mus_format(SD_PT, addr, vstr);
	  if (vstr) FREE(vstr);
	  return(buf);
	}
      else return(mus_format("a%d(null)", addr));
      break;
    case R_READER:
      if ((pt->readers) && (pt->readers[addr]))
	{
	  char *buf = NULL, *vstr = NULL;
	  vstr = sf_to_string(pt->readers[addr]);
	  buf = mus_format("r%d(%s)", addr, vstr);
	  if (vstr) FREE(vstr);
	  return(buf);
	}
      else return(copy_string("null"));
      break;
    case R_MIX_READER:
      if ((pt->mix_readers) && (pt->mix_readers[addr]))
	{
	  char *buf = NULL, *vstr = NULL;
	  vstr = run_mix_reader_to_string(pt->mix_readers[addr]);
	  buf = mus_format("m%d(%s)", addr, vstr);
	  if (vstr) FREE(vstr);
	  return(buf);
	}
      else return(copy_string("null"));
      break;
    case R_TRACK_READER:
      if ((pt->track_readers) && (pt->track_readers[addr]))
	{
	  char *buf = NULL, *vstr = NULL;
	  vstr = run_track_reader_to_string(pt->track_readers[addr]);
	  buf = mus_format("t%d(%s)", addr, vstr);
	  if (vstr) FREE(vstr);
	  return(buf);
	}
      else return(copy_string("null"));
      break;
    case R_CLM:
      if ((pt->clms) && (pt->clms[addr]))
	return(mus_format("c%d(%s)", addr, mus_describe(pt->clms[addr])));  
      else return(mus_format("c%d(null)", addr));
      break;
    case R_FUNCTION: 
      if ((pt->fncs) && (pt->fncs[addr]))
	return(describe_ptree(((ptree **)(pt->fncs))[addr]));
      else return(copy_string("internal lambda?"));
      break;
    case R_INT_VECTOR:  
    case R_VCT_VECTOR:  
    case R_CLM_VECTOR:  return(mus_format("vect " PTR_PT , addr, pt->vects[addr])); break;
    case R_UNSPECIFIED: return(copy_string("#<unspecified>")); break;
    default:
      if (type > R_ANY)
	return(mus_format("x%d(%s: clm-struct %p)", addr, type_name(type), pt->xens[addr]));
      else return(mus_format("i%d(unknown type: %d)", addr, type));            
      break;
    }
  return(NULL);
}

static char *describe_xen_value(xen_value *v, ptree *pt)
{
  return(describe_xen_value_1(v->type, v->addr, pt));
}

static bool got_lambda = false; /* a temporary kludge?? */

static ptree *make_ptree(int initial_data_size)
{
  ptree *pt;
  got_lambda = false;
  pt = (ptree *)CALLOC(1, sizeof(ptree));
  if (initial_data_size > 0)
    {
      pt->ints = (Int *)CALLOC(initial_data_size, sizeof(Int));
      pt->dbls = (Double *)CALLOC(initial_data_size, sizeof(Double));
    }
  pt->program_size = 0;
  pt->ints_size = initial_data_size;
  pt->dbls_size = initial_data_size;
  pt->triple_ctr = 0;
  pt->int_ctr = INITIAL_INT_CTR;
  pt->dbl_ctr = 0;
  pt->vars_size = 0;
  pt->var_ctr = 0;
  pt->vars = NULL;
  pt->global_vars_size = 0;
  pt->global_var_ctr = 0;
  pt->global_vars = NULL;
  pt->gcs = NULL;
  pt->gc_ctr = 0;
  pt->gcs_size = 0;
  pt->gc_protected = NULL;
  pt->gc_protected_ctr = 0;
  pt->gc_protected_size = 0;
  pt->initial_pc = 0;
  pt->code = XEN_FALSE;
  pt->form = XEN_UNDEFINED;
  pt->form_loc = -1;
  pt->strs_size = 0;
  pt->strs = NULL;
  pt->str_ctr = 0;
  pt->vcts_size = 0;
  pt->vcts = NULL;
  pt->vct_ctr = 0;
  pt->sds_size = 0;
  pt->sds = NULL;
  pt->sd_ctr = 0;
  pt->clms_size = 0;
  pt->clms = NULL;
  pt->clm_ctr = 0;
  pt->vects_size = 0;
  pt->vects = NULL;
  pt->vect_ctr = 0;
  pt->fncs_size = 0;
  pt->fncs = NULL;
  pt->fnc_ctr = 0;
  pt->xens_size = 0;
  pt->xens = NULL;
  pt->xen_ctr = 0;
  pt->readers_size = 0;
  pt->readers = NULL;
  pt->reader_ctr = 0;
  pt->mix_readers_size = 0;
  pt->mix_readers = NULL;
  pt->mix_reader_ctr = 0;
  pt->track_readers_size = 0;
  pt->track_readers = NULL;
  pt->track_reader_ctr = 0;
  pt->xen_vars_size = 0;
  pt->xen_vars = NULL;
  pt->xen_var_ctr = 0;
  pt->arity = 0;
  return(pt);
}

static ptree *attach_to_ptree(ptree *pt)
{
  /* share all environment tables */
  ptree *new_tree;
  new_tree = (ptree *)CALLOC(1, sizeof(ptree));
  new_tree->program_size = 0;
  new_tree->triple_ctr = 0;
  new_tree->result = NULL;
  new_tree->arity = 0;
  new_tree->code = XEN_FALSE;
  new_tree->form = XEN_UNDEFINED;
  new_tree->form_loc = -1;
  if (pt->outer_tree)
    new_tree->outer_tree = pt->outer_tree;
  else new_tree->outer_tree = pt;
  new_tree->ints = pt->ints;
  new_tree->dbls = pt->dbls;
  new_tree->vars = pt->vars;
  new_tree->global_vars = pt->global_vars;
  new_tree->gcs = pt->gcs;
  new_tree->gotos = pt->gotos;
  new_tree->strs = pt->strs;
  new_tree->vcts = pt->vcts;
  new_tree->sds = pt->sds;
  new_tree->clms = pt->clms;
  new_tree->vects = pt->vects;
  new_tree->fncs = pt->fncs;
  new_tree->xens = pt->xens;
  new_tree->readers = pt->readers;
  new_tree->mix_readers = pt->mix_readers;
  new_tree->track_readers = pt->track_readers;
  new_tree->xen_vars = pt->xen_vars;
  new_tree->ints_size = pt->ints_size;
  new_tree->dbls_size = pt->dbls_size;
  new_tree->int_ctr = pt->int_ctr;
  new_tree->dbl_ctr = pt->dbl_ctr;
  new_tree->vars_size = pt->vars_size;
  new_tree->var_ctr = pt->var_ctr;
  new_tree->global_vars_size = pt->global_vars_size;
  new_tree->global_var_ctr = pt->global_var_ctr;
  new_tree->goto_ctr = pt->goto_ctr;
  new_tree->gotos_size = pt->gotos_size;
  new_tree->gc_ctr = pt->gc_ctr;
  new_tree->gcs_size = pt->gcs_size;
  new_tree->str_ctr = pt->str_ctr;
  new_tree->vct_ctr = pt->vct_ctr;
  new_tree->sd_ctr = pt->sd_ctr;
  new_tree->clm_ctr = pt->clm_ctr;
  new_tree->vect_ctr = pt->vect_ctr;
  new_tree->fnc_ctr = pt->fnc_ctr;
  new_tree->xen_ctr = pt->xen_ctr;
  new_tree->reader_ctr = pt->reader_ctr;
  new_tree->mix_reader_ctr = pt->mix_reader_ctr;
  new_tree->track_reader_ctr = pt->track_reader_ctr;
  new_tree->strs_size = pt->strs_size;
  new_tree->vcts_size = pt->vcts_size;
  new_tree->sds_size = pt->sds_size;
  new_tree->clms_size = pt->clms_size;
  new_tree->vects_size = pt->vects_size;
  new_tree->fncs_size = pt->fncs_size;
  new_tree->xens_size = pt->xens_size;
  new_tree->readers_size = pt->readers_size;
  new_tree->mix_readers_size = pt->mix_readers_size;
  new_tree->track_readers_size = pt->track_readers_size;
  new_tree->xen_var_ctr = pt->xen_var_ctr;
  new_tree->xen_vars_size = pt->xen_vars_size;
  new_tree->gc_protected = pt->gc_protected;
  new_tree->gc_protected_ctr = pt->gc_protected_ctr;
  new_tree->gc_protected_size = pt->gc_protected_size;
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
  outer->vects = inner->vects;
  outer->fncs = inner->fncs;
  outer->xens = inner->xens;
  outer->readers = inner->readers;
  outer->mix_readers = inner->mix_readers;
  outer->track_readers = inner->track_readers;
  outer->xen_vars = inner->xen_vars;
  outer->gc_protected = inner->gc_protected;

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
  inner->vects = NULL;
  inner->fncs = NULL;
  inner->xens = NULL;
  inner->readers = NULL;
  inner->mix_readers = NULL;
  inner->track_readers = NULL;
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
  outer->vect_ctr = inner->vect_ctr;
  outer->fnc_ctr = inner->fnc_ctr;
  outer->xen_ctr = inner->xen_ctr;
  outer->reader_ctr = inner->reader_ctr;
  outer->mix_reader_ctr = inner->mix_reader_ctr;
  outer->track_reader_ctr = inner->track_reader_ctr;
  outer->strs_size = inner->strs_size;
  outer->vcts_size = inner->vcts_size;
  outer->sds_size = inner->sds_size;
  outer->clms_size = inner->clms_size;
  outer->vects_size = inner->vects_size;
  outer->fncs_size = inner->fncs_size;
  outer->xens_size = inner->xens_size;
  outer->readers_size = inner->readers_size;
  outer->mix_readers_size = inner->mix_readers_size;
  outer->track_readers_size = inner->track_readers_size;
  outer->xen_var_ctr = inner->xen_var_ctr;
  outer->xen_vars_size = inner->xen_vars_size;
  outer->gc_protected_ctr = inner->gc_protected_ctr;
  outer->gc_protected_size = inner->gc_protected_size;

  inner->ints_size = 0;
  inner->dbls_size = 0;
  inner->int_ctr = INITIAL_INT_CTR;
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
  inner->vect_ctr = 0;
  inner->fnc_ctr = 0;
  inner->xen_ctr = 0;
  inner->reader_ctr = 0;
  inner->mix_reader_ctr = 0;
  inner->track_reader_ctr = 0;
  inner->strs_size = 0;
  inner->vcts_size = 0;
  inner->sds_size = 0;
  inner->clms_size = 0;
  inner->vects_size = 0;
  inner->fncs_size = 0;
  inner->xens_size = 0;
  inner->readers_size = 0;
  inner->mix_readers_size = 0;
  inner->track_readers_size = 0;
  inner->xen_var_ctr = 0;
  inner->xen_vars_size = 0;
  inner->gc_protected_size = 0;
  inner->gc_protected_ctr = 0;
}


static void int_vect_into_vector(vect *v, XEN vectr)
{
  int len, i;
  XEN *vdata;
  len = XEN_VECTOR_LENGTH(vectr);
  vdata = XEN_VECTOR_ELEMENTS(vectr);
  for (i = 0; i < len; i++) 
    vdata[i] = R_C_TO_XEN_INT(v->data.ints[i]);
}

static void free_vect(vect *v, int type)
{
  if (v)
    {
      switch (type)
	{
	case R_INT_VECTOR: if (v->data.ints) FREE(v->data.ints); break;
	case R_CLM_VECTOR: if (v->data.gens) FREE(v->data.gens); break;
	case R_VCT_VECTOR: if (v->data.vcts) FREE(v->data.vcts); break;
	}
      FREE(v);
    }
}

static void clm_struct_restore(ptree *prog, xen_var *var);

static vct *vector_to_vct(XEN vectr)
{
  int len, i;
  vct *v;
  XEN *vdata;
  len = XEN_VECTOR_LENGTH(vectr);
  if (len == 0) return(NULL);
  v = c_make_vct(len);
  vdata = XEN_VECTOR_ELEMENTS(vectr);
  for (i = 0; i < len; i++) 
    if (XEN_DOUBLE_P(vdata[i]))
      v->data[i] = (Float)XEN_TO_C_DOUBLE(vdata[i]);
    else
      {
	c_free_vct(v);
	return(NULL);
      }
  return(v);
}

static void vct_into_vector(vct *v, XEN vectr)
{
  int len, i;
  XEN *vdata;
  len = XEN_VECTOR_LENGTH(vectr);
  vdata = XEN_VECTOR_ELEMENTS(vectr);
  /* VECTOR_SET here */
  for (i = 0; i < len; i++) 
    vdata[i] = C_TO_XEN_DOUBLE(v->data[i]);
}

static xen_var *free_xen_var(ptree *prog, xen_var *var)
{
  XEN val;
  bool local_var;
  if (var)
    {
      /* fprintf(stderr, "free var %s %s (global: %d, unclean: %d, opt: %d)\n", 
	 var->name, type_name(var->v->type), var->global, var->unclean, current_optimization);
      */
      /* if var->global, reflect new value into outer level version of the variable upon quit */
      if ((var->global) &&
	  (var->unclean))
	{
	  if (current_optimization < GLOBAL_SET_OK)
	    switch (var->v->type)
	      {
	      case R_FLOAT:  xen_symbol_name_set_value(var->name, C_TO_XEN_DOUBLE(prog->dbls[var->v->addr]));       break;
	      case R_INT:    xen_symbol_name_set_value(var->name, R_C_TO_XEN_INT(prog->ints[var->v->addr]));        break;
	      case R_BOOL:   xen_symbol_name_set_value(var->name, C_TO_XEN_BOOLEAN(prog->ints[var->v->addr]));      break;
	      case R_STRING: xen_symbol_name_set_value(var->name, C_TO_XEN_STRING(prog->strs[var->v->addr]));       break;
	      case R_CHAR:   xen_symbol_name_set_value(var->name, C_TO_XEN_CHAR((char)(prog->ints[var->v->addr]))); break;
	      case R_FLOAT_VECTOR:
		val = symbol_to_value(prog->code, C_STRING_TO_XEN_SYMBOL(var->name), &local_var);
		if (XEN_VECTOR_P(val))
		  vct_into_vector(prog->vcts[var->v->addr], val);
		break;
	      case R_INT_VECTOR:
		val = symbol_to_value(prog->code, C_STRING_TO_XEN_SYMBOL(var->name), &local_var);
		if (XEN_VECTOR_P(val))
		  int_vect_into_vector(prog->vects[var->v->addr], val);
		break;
	      }
	  else
	    switch (var->v->type)
	      {
	      case R_FLOAT:  symbol_set_value(prog->code, C_STRING_TO_XEN_SYMBOL(var->name), C_TO_XEN_DOUBLE(prog->dbls[var->v->addr]));       break;
	      case R_INT:    symbol_set_value(prog->code, C_STRING_TO_XEN_SYMBOL(var->name), R_C_TO_XEN_INT(prog->ints[var->v->addr]));        break;
	      case R_BOOL:   symbol_set_value(prog->code, C_STRING_TO_XEN_SYMBOL(var->name), C_TO_XEN_BOOLEAN(prog->ints[var->v->addr]));      break;
	      case R_STRING: symbol_set_value(prog->code, C_STRING_TO_XEN_SYMBOL(var->name), C_TO_XEN_STRING(prog->strs[var->v->addr]));       break;
	      case R_CHAR:   symbol_set_value(prog->code, C_STRING_TO_XEN_SYMBOL(var->name), C_TO_XEN_CHAR((char)(prog->ints[var->v->addr]))); break;
	      case R_FLOAT_VECTOR:
		val = symbol_to_value(prog->code, C_STRING_TO_XEN_SYMBOL(var->name), &local_var);
		if (XEN_VECTOR_P(val))
		  vct_into_vector(prog->vcts[var->v->addr], val);
		break;
	      case R_INT_VECTOR:
		val = symbol_to_value(prog->code, C_STRING_TO_XEN_SYMBOL(var->name), &local_var);
		if (XEN_VECTOR_P(val))
		  int_vect_into_vector(prog->vects[var->v->addr], val);
		break;
	      default:
		if (var->v->type > R_ANY)
		  clm_struct_restore(prog, var);
		break;
	      }
	}
      if (var->name) FREE(var->name);
      if (var->v) FREE(var->v);
      FREE(var);
    }
  return(NULL);
}

static ptree *free_embedded_ptree(ptree *pt)
{
  int i;
  if (pt)
    {
      if (pt->program)
	{
	  for (i = 0; i < pt->program_size; i++)
	    if (pt->program[i])
	      ((triple **)(pt->program))[i] = free_triple(((triple **)(pt->program))[i]);
	  FREE(pt->program);
	}
      if (pt->args) FREE(pt->args);
      if (pt->arg_types) FREE(pt->arg_types);
      if (pt->result) FREE(pt->result);
      FREE(pt);
    }
  return(NULL);
}

void *free_ptree(void *upt)
{
  int i;
  ptree *pt = (ptree *)upt;
  if (pt)
    {
      /* free_xen_var depends (in vector case) on finding current (unfreed) data */
      if (XEN_BOUND_P(pt->form)) snd_unprotect_at(pt->form_loc);
      if (pt->vars)
	{
	  for (i = 0; i < pt->var_ctr; i++)
	    free_xen_var(pt, pt->vars[i]);
	  FREE(pt->vars);
	}
      if (pt->global_vars)
	{
	  for (i = 0; i < pt->global_var_ctr; i++)
	    free_xen_var(pt, pt->global_vars[i]);
	  FREE(pt->global_vars);
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
			      c_free_vct(pt->vcts[v->addr]); 
			      pt->vcts[v->addr] = NULL;   
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
			case R_READER: 
			  if (pt->readers[v->addr])
			    {
			      int k;
			      for (k = 0; k < pt->reader_ctr; k++)
				if ((k != v->addr) && (pt->readers[k] == pt->readers[v->addr]))
				  pt->readers[k] = NULL;
			      free_snd_fd(pt->readers[v->addr]); 
			      pt->readers[v->addr] = NULL;   
			    }
			  break;
			case R_MIX_READER: 
			  if (pt->mix_readers[v->addr])
			    {
			      int k;
			      for (k = 0; k < pt->mix_reader_ctr; k++)
				if ((k != v->addr) && (pt->mix_readers[k] == pt->mix_readers[v->addr]))
				  pt->mix_readers[k] = NULL;
			      run_free_mix_fd(pt->mix_readers[v->addr]); 
			      pt->mix_readers[v->addr] = NULL;   
			    }
			  break;
			case R_TRACK_READER: 
			  if (pt->track_readers[v->addr])
			    {
			      int k;
			      for (k = 0; k < pt->track_reader_ctr; k++)
				if ((k != v->addr) && (pt->track_readers[k] == pt->track_readers[v->addr]))
				  pt->track_readers[k] = NULL;
			      run_free_track_fd(pt->track_readers[v->addr]); 
			      pt->track_readers[v->addr] = NULL;   
			    }
			  break;
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
#if DEBUGGING
			  fprintf(stderr,"attempt to free locally created gen");
#endif
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
			      FREE(pt->strs[v->addr]);
			      pt->strs[v->addr] = NULL;
			    }
			  break;
			}
		      v->gc = false;
		    }
		  FREE(v);
		  pt->gcs[i] = NULL;
		}
	    }
	}
      if (pt->strs)
	{
	  for (i = 0; i < pt->str_ctr; i++)
	    if (pt->strs[i])
	      {
		FREE(pt->strs[i]);
		pt->strs[i] = NULL;
	      }
	  FREE(pt->strs);
	  pt->strs = NULL;
	}
      if (pt->xen_vars)
	{
	  for (i = 0; i < pt->xen_var_ctr; i++)
	    if (pt->xen_vars[i])
	      {
		FREE(pt->xen_vars[i]); /* free the containing array */
		pt->xen_vars[i] = NULL;
	      }
	  FREE(pt->xen_vars);
	}
      if (pt->vcts) 
	{
	  FREE(pt->vcts);
	  pt->vcts = NULL;
	}
      if (pt->sds) 
	{
	  FREE(pt->sds);
	  pt->sds = NULL;
	}
      if (pt->clms) 
	{
	  FREE(pt->clms);
	  pt->clms = NULL;
	}
      if (pt->vects) 
	{
	  FREE(pt->vects);
	  pt->vects = NULL;
	}
      if (pt->fncs) 
	{
	  FREE(pt->fncs);
	  pt->fncs = NULL;
	}
      if (pt->xens) 
	{
	  FREE(pt->xens);
	  pt->xens = NULL;
	}
      if (pt->readers) 
	{
	  FREE(pt->readers);
	  pt->readers = NULL;
	}
      if (pt->mix_readers) 
	{
	  FREE(pt->mix_readers);
	  pt->mix_readers = NULL;
	}
      if (pt->track_readers) 
	{
	  FREE(pt->track_readers);
	  pt->track_readers = NULL;
	}
      if (pt->gc_protected)
	{
	  for (i = 0; i < pt->gc_protected_ctr; i++)
	    snd_unprotect_at(pt->gc_protected[i]);
	  FREE(pt->gc_protected);
	}
      if (pt->program)
	{
	  for (i = 0; i < pt->program_size; i++)
	    if (pt->program[i])
	      ((triple **)(pt->program))[i] = free_triple(((triple **)(pt->program))[i]);
	  FREE(pt->program);
	}
      if (pt->gcs) FREE(pt->gcs);
      if (pt->args) FREE(pt->args);
      if (pt->arg_types) FREE(pt->arg_types);
      if (pt->gotos) 
	{
	  for (i = 0; i < pt->gotos_size; i++)
	    if (pt->gotos[i])
	      free_goto(pt->gotos[i]);
	  FREE(pt->gotos);
	}
      if (pt->result) FREE(pt->result);
      if (pt->ints) FREE(pt->ints);
      if (pt->dbls) FREE(pt->dbls);
      FREE(pt);
    }
  return(NULL);
}

static triple *add_triple_to_ptree(ptree *pt, triple *trp)
{
  int i, old_size;
  if (pt->triple_ctr >= pt->program_size)
    {
      if (pt->program_size == 0)
	{
#ifdef __cplusplus
	  pt->program = (triple **)CALLOC(8, sizeof(triple *));
#else
	  pt->program = (struct triple **)CALLOC(8, sizeof(triple *));
#endif
	  pt->program_size = 8;
	}
      else
	{
	  old_size = pt->program_size;
	  pt->program_size += 8;
#ifdef __cplusplus
	  pt->program = (triple **)REALLOC(pt->program, pt->program_size * sizeof(triple *));
#else
	  pt->program = (struct triple **)REALLOC(pt->program, pt->program_size * sizeof(triple *));
#endif
	  for (i = old_size; i < pt->program_size; i++) pt->program[i] = NULL;
	}
    }
#ifdef __cplusplus
  pt->program[pt->triple_ctr++] = trp;
#else
  ((triple **)(pt->program))[pt->triple_ctr++] = trp;
#endif
  return(trp);
}

static int add_int_to_ptree(ptree *pt, Int value)
{
  int cur;
  cur = pt->int_ctr++;
  if (cur >= pt->ints_size)
    {
      pt->ints_size += 8;
      pt->ints = (Int *)REALLOC(pt->ints, pt->ints_size * sizeof(Int));
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
      pt->dbls = (Double *)REALLOC(pt->dbls, pt->dbls_size * sizeof(Double));
    }
  pt->dbls[cur] = value;
  return(cur);
}

static int add_vct_to_ptree(ptree *pt, vct *value)
{
  int i, cur;
  cur = pt->vct_ctr++;
  if (cur >= pt->vcts_size)
    {
      pt->vcts_size += 8;
      if (pt->vcts)
	{
	  pt->vcts = (vct **)REALLOC(pt->vcts, pt->vcts_size * sizeof(vct *));
	  for (i = cur; i < pt->vcts_size; i++) pt->vcts[i] = NULL;
	}
      else pt->vcts = (vct **)CALLOC(pt->vcts_size, sizeof(vct *));
    }
  pt->vcts[cur] = value;
  return(cur);
}

static int add_sound_data_to_ptree(ptree *pt, sound_data *value)
{
  int i, cur;
  cur = pt->sd_ctr++;
  if (cur >= pt->sds_size)
    {
      pt->sds_size += 8;
      if (pt->sds)
	{
	  pt->sds = (sound_data **)REALLOC(pt->sds, pt->sds_size * sizeof(sound_data *));
	  for (i = cur; i < pt->sds_size; i++) pt->sds[i] = NULL;
	}
      else pt->sds = (sound_data **)CALLOC(pt->sds_size, sizeof(sound_data *));
    }
  pt->sds[cur] = value;
  return(cur);
}

static int add_clm_to_ptree(ptree *pt, mus_any *value)
{
  int i, cur;
  cur = pt->clm_ctr++;
  if (cur >= pt->clms_size)
    {
      pt->clms_size += 8;
      if (pt->clms)
	{
	  pt->clms = (mus_any **)REALLOC(pt->clms, pt->clms_size * sizeof(mus_any *));
	  for (i = cur; i < pt->clms_size; i++) pt->clms[i] = NULL;
	}
      else pt->clms = (mus_any **)CALLOC(pt->clms_size, sizeof(mus_any *));
    }
  pt->clms[cur] = value;
  return(cur);
}

static int add_vect_to_ptree(ptree *pt, vect *value)
{
  int i, cur;
  cur = pt->vect_ctr++;
  if (cur >= pt->vects_size)
    {
      pt->vects_size += 8;
      if (pt->vects)
	{
	  pt->vects = (vect **)REALLOC(pt->vects, pt->vects_size * sizeof(vect *));
	  for (i = cur; i < pt->vects_size; i++) pt->vects[i] = NULL;
	}
      else pt->vects = (vect **)CALLOC(pt->vects_size, sizeof(vect *));
    }
  pt->vects[cur] = value;
  return(cur);
}

static int add_fnc_to_ptree(ptree *pt, ptree *value)
{
  int i, cur;
  cur = pt->fnc_ctr++;
  if (cur >= pt->fncs_size)
    {
      pt->fncs_size += 8;
      if (pt->fncs)
	{
	  pt->fncs = (struct ptree **)REALLOC(pt->fncs, pt->fncs_size * sizeof(ptree *));
	  for (i = cur; i < pt->fncs_size; i++) pt->fncs[i] = NULL;
	}
      else pt->fncs = (struct ptree **)CALLOC(pt->fncs_size, sizeof(ptree *));
    }
  ((ptree **)(pt->fncs))[cur] = value;
  return(cur);
}

static int add_xen_to_ptree(ptree *pt, XEN value)
{
  int i, cur;
  cur = pt->xen_ctr++;
  if (cur >= pt->xens_size)
    {
      pt->xens_size += 8;
      if (pt->xens)
	pt->xens = (XEN *)REALLOC(pt->xens, pt->xens_size * sizeof(XEN));
      else pt->xens = (XEN *)CALLOC(pt->xens_size, sizeof(XEN));
      for (i = cur; i < pt->xens_size; i++) pt->xens[i] = XEN_UNDEFINED;
    }
  pt->xens[cur] = value;
  return(cur);
}

static int add_reader_to_ptree(ptree *pt, snd_fd *value)
{
  int i, cur;
  cur = pt->reader_ctr++;
  if (cur >= pt->readers_size)
    {
      pt->readers_size += 8;
      if (pt->readers)
	{
	  pt->readers = (snd_fd **)REALLOC(pt->readers, pt->readers_size * sizeof(snd_fd *));
	  for (i = cur; i < pt->readers_size; i++) pt->readers[i] = NULL;
	}
      else pt->readers = (snd_fd **)CALLOC(pt->readers_size, sizeof(snd_fd *));
    }
  pt->readers[cur] = value;
  return(cur);
}

static int add_mix_reader_to_ptree(ptree *pt, void *value)
{
  int i, cur;
  cur = pt->mix_reader_ctr++;
  if (cur >= pt->mix_readers_size)
    {
      pt->mix_readers_size += 8;
      if (pt->mix_readers)
	{
	  pt->mix_readers = (void **)REALLOC(pt->mix_readers, pt->mix_readers_size * sizeof(void *));
	  for (i = cur; i < pt->mix_readers_size; i++) pt->mix_readers[i] = NULL;
	}
      else pt->mix_readers = (void **)CALLOC(pt->mix_readers_size, sizeof(void *));
    }
  pt->mix_readers[cur] = value;
  return(cur);
}

static int add_track_reader_to_ptree(ptree *pt, void *value)
{
  int i, cur;
  cur = pt->track_reader_ctr++;
  if (cur >= pt->track_readers_size)
    {
      pt->track_readers_size += 8;
      if (pt->track_readers)
	{
	  pt->track_readers = (void **)REALLOC(pt->track_readers, pt->track_readers_size * sizeof(void *));
	  for (i = cur; i < pt->track_readers_size; i++) pt->track_readers[i] = NULL;
	}
      else pt->track_readers = (void **)CALLOC(pt->track_readers_size, sizeof(void *));
    }
  pt->track_readers[cur] = value;
  return(cur);
}

static xen_var *new_xen_var(const char *name, xen_value *v)
{
  xen_var *var;
  var = (xen_var *)CALLOC(1, sizeof(xen_var));
  var->name = copy_string(name);
  var->v = copy_xen_value(v);
  var->unclean = false;
  var->global = false;
  var->unsettable = false;
  return(var);
}

static int add_var_to_ptree(ptree *pt, const char *name, xen_value *v)
{
  int cur;
  /*
  fprintf(stderr, "add var: %s %s\n", name, describe_xen_value(v, pt));
  */
  cur = pt->var_ctr;
  if (cur >= pt->vars_size)
    {
      pt->vars_size += 8;
      if (pt->vars)
	{
	  int i;
	  pt->vars = (xen_var **)REALLOC(pt->vars, pt->vars_size * sizeof(xen_var *));
	  for (i = cur; i < pt->vars_size; i++) pt->vars[i] = NULL;
	}
      else pt->vars = (xen_var **)CALLOC(pt->vars_size, sizeof(xen_var *));
    }
  pt->vars[pt->var_ctr++] = new_xen_var(name, v);
  return(cur);
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
	  pt->global_vars = (xen_var **)REALLOC(pt->global_vars, pt->global_vars_size * sizeof(xen_var *));
	  for (i = cur; i < pt->global_vars_size; i++) pt->global_vars[i] = NULL;
	}
      else pt->global_vars = (xen_var **)CALLOC(pt->global_vars_size, sizeof(xen_var *));
    }
  var = new_xen_var(name, v);
  var->global = true;
  pt->global_vars[cur] = var;
  return(cur);
}

static int add_string_to_ptree(ptree *pt, char *str)
{
  int i, cur;
  cur = pt->str_ctr++;
  if (cur >= pt->strs_size)
    {
      pt->strs_size += 8;
      if (pt->strs)
	{
	  pt->strs = (char **)REALLOC(pt->strs, pt->strs_size * sizeof(char *));
	  for (i = cur; i < pt->strs_size; i++)
	    pt->strs[i] = NULL;
	}
      else pt->strs = (char **)CALLOC(pt->strs_size, sizeof(char *));
    }
  pt->strs[cur] = str;
  return(cur);
}

static xen_value *add_empty_var_to_ptree(ptree *prog, int type)
{
  switch (type)
    {
    case R_FLOAT: return(make_xen_value(type, add_dbl_to_ptree(prog, 0.0), R_VARIABLE));      break;
    case R_STRING: return(make_xen_value(type, add_string_to_ptree(prog, NULL), R_VARIABLE)); break;
    case R_CLM: return(make_xen_value(type, add_clm_to_ptree(prog, NULL), R_VARIABLE));       break;
    case R_FUNCTION: return(make_xen_value(type, add_fnc_to_ptree(prog, NULL), R_VARIABLE));  break;
    case R_READER: return(make_xen_value(type, add_reader_to_ptree(prog, NULL), R_VARIABLE)); break;
    case R_MIX_READER: return(make_xen_value(type, add_mix_reader_to_ptree(prog, NULL), R_VARIABLE)); break;
    case R_TRACK_READER: return(make_xen_value(type, add_track_reader_to_ptree(prog, NULL), R_VARIABLE)); break;
    case R_SOUND_DATA: return(make_xen_value(type, add_sound_data_to_ptree(prog, NULL), R_VARIABLE)); break;
    case R_FLOAT_VECTOR:
    case R_VCT: 
      return(make_xen_value(type, add_vct_to_ptree(prog, NULL), R_VARIABLE)); 
      break;
    case R_SYMBOL: 
    case R_KEYWORD:
      return(make_xen_value(type, add_xen_to_ptree(prog, XEN_FALSE), R_VARIABLE)); 
      break;
    case R_LIST:
    case R_PAIR:
      return(make_xen_value(type, add_xen_to_ptree(prog, XEN_UNDEFINED), R_VARIABLE)); 
      /* "undefined" for later check in walk for lists as args to embedded funcs */
      break;
    case R_CLM_VECTOR:
    case R_INT_VECTOR:
    case R_VCT_VECTOR:
      return(make_xen_value(type, add_vect_to_ptree(prog, NULL), R_VARIABLE)); 
      break;
    default:
      if (type > R_ANY)
	return(make_xen_value(type, add_xen_to_ptree(prog, XEN_FALSE), R_VARIABLE)); 
      break;
    }
  return(make_xen_value(type, add_int_to_ptree(prog, 0), R_VARIABLE)); 
}

static xen_value *transfer_value(ptree *prog, xen_value *v)
{
  switch (v->type)
    {
    case R_FLOAT: 
      return(make_xen_value(v->type, add_dbl_to_ptree(prog, prog->dbls[v->addr]), R_VARIABLE)); 
      break;
    case R_STRING: 
      return(make_xen_value(v->type, add_string_to_ptree(prog, copy_string(prog->strs[v->addr])), R_VARIABLE)); 
      break;
    case R_CLM_VECTOR:
    case R_INT_VECTOR:
    case R_VCT_VECTOR:
    case R_FLOAT_VECTOR:
    case R_VCT: 
    case R_CLM: 
    case R_FUNCTION: 
    case R_READER: 
    case R_MIX_READER: 
    case R_TRACK_READER: 
    case R_SOUND_DATA:
    case R_SYMBOL: 
    case R_KEYWORD:
    case R_LIST:
    case R_PAIR:
      return(make_xen_value(v->type, v->addr, R_VARIABLE)); 
      break;
    default: 
      if (v->type > R_ANY)
	return(make_xen_value(v->type, add_xen_to_ptree(prog, prog->xens[v->addr]), R_VARIABLE));
      break;
    }
  return(make_xen_value(v->type, add_int_to_ptree(prog, prog->ints[v->addr]), R_VARIABLE));
}

static void add_xen_value_to_gcs(ptree *pt, xen_value *v)
{
  int old_size, i;
  if (pt->gc_ctr >= pt->gcs_size)
    {
      old_size = pt->gcs_size;
      pt->gcs_size += 4;
      if (old_size == 0)
	pt->gcs = (xen_value **)CALLOC(pt->gcs_size, sizeof(xen_value *));
      else
	{
	  pt->gcs = (xen_value **)REALLOC(pt->gcs, pt->gcs_size * sizeof(xen_value *));
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

static void add_loc_to_protected_list(ptree *pt, int loc)
{
  int old_size, i;
  if (loc < 0) return;
  if (pt->gc_protected_ctr >= pt->gc_protected_size)
    {
      old_size = pt->gc_protected_size;
      pt->gc_protected_size += 8;
      if (old_size == 0)
	pt->gc_protected = (int *)CALLOC(pt->gc_protected_size, sizeof(int));
      else
	{
	  pt->gc_protected = (int *)REALLOC(pt->gc_protected, pt->gc_protected_size * sizeof(int));
	  for (i = old_size; i < pt->gc_protected_size; i++) pt->gc_protected[i] = -1;
	}
    }
  pt->gc_protected[pt->gc_protected_ctr++] = loc;
}

static vect *read_int_vector(XEN vectr)
{
  int len, i;
  vect *v;
  XEN *vdata;
  len = XEN_VECTOR_LENGTH(vectr);
  if (len == 0) return(NULL);
  v = (vect *)CALLOC(1, sizeof(vect));
  v->length = len;
  v->data.ints = (Int *)CALLOC(len, sizeof(Int));
  vdata = XEN_VECTOR_ELEMENTS(vectr);
  for (i = 0; i < len; i++) 
    if (XEN_OFF_T_P(vdata[i]))
      v->data.ints[i] = R_XEN_TO_C_INT(vdata[i]);
    else
      {
	run_warn("initial int vector value at %d is %s: will try to abort ptree evaluation...", i, XEN_AS_STRING(vdata[i]));
	FREE(v->data.ints);
	FREE(v);
	return(NULL);
      }
  return(v);
}

static vect *read_vct_vector(XEN vectr)
{
  int len, i;
  vect *v;
  XEN *vdata;
  len = XEN_VECTOR_LENGTH(vectr);
  if (len == 0) return(NULL);
  v = (vect *)CALLOC(1, sizeof(vect));
  v->length = len;
  v->data.vcts = (vct **)CALLOC(len, sizeof(vct *));
  vdata = XEN_VECTOR_ELEMENTS(vectr);
  for (i = 0; i < len; i++) 
    if (VCT_P(vdata[i]))
      v->data.vcts[i] = get_vct(vdata[i]);
    else
      {
	run_warn("initial vct vector value at %d is %s: will try to abort ptree evaluation...", i, XEN_AS_STRING(vdata[i]));
	FREE(v->data.vcts);
	FREE(v);
	return(NULL);
      }
  return(v);
}

static vect *read_clm_vector(XEN vectr)
{
  int len, i;
  vect *v;
  XEN *vdata;
  len = XEN_VECTOR_LENGTH(vectr);
  if (len == 0) return(NULL);
  v = (vect *)CALLOC(1, sizeof(vect));
  v->length = len;
  v->data.gens = (mus_any **)CALLOC(len, sizeof(mus_any *));
  vdata = XEN_VECTOR_ELEMENTS(vectr);
  for (i = 0; i < len; i++) 
    if (mus_xen_p(vdata[i]))
      v->data.gens[i] = XEN_TO_MUS_ANY(vdata[i]);
    else
      {
	if (!(XEN_FALSE_P(vdata[i])))
	  {
	    run_warn("initial clm vector value at %d is %s: will try to abort ptree evaluation...", i, XEN_AS_STRING(vdata[i]));
	    FREE(v->data.gens);
	    FREE(v);
	    return(NULL);
	  }
      }
  return(v);
}

static vect *read_vector(XEN vector, int type)
{
  switch (type)
    {
    case R_INT_VECTOR: return(read_int_vector(vector)); 
    case R_CLM_VECTOR: return(read_clm_vector(vector)); 
    case R_VCT_VECTOR: return(read_vct_vector(vector)); 
    }
  return(NULL);
}

static int xen_to_run_type(XEN val)
{
  if (XEN_NUMBER_P(val))
    {
      if ((XEN_EXACT_P(val)) && (XEN_OFF_T_P(val)))
	return(R_INT);
      else return(R_FLOAT);
    }
  else
    {
      if (XEN_BOOLEAN_P(val)) return(R_BOOL); else
	if (VCT_P(val)) return(R_VCT); else
	  if (sf_p(val)) return(R_READER); else
	    if (mf_p(val)) return(R_MIX_READER); else
	      if (tf_p(val)) return(R_TRACK_READER); else
		if (sound_data_p(val)) return(R_SOUND_DATA); else
		  if (mus_xen_p(val)) return(R_CLM); else
		    if (XEN_CHAR_P(val)) return(R_CHAR); else
		      if (XEN_STRING_P(val)) return(R_STRING); else
			if (XEN_KEYWORD_P(val)) return(R_KEYWORD); else
			  if (XEN_SYMBOL_P(val)) return(R_SYMBOL); else
			    if (XEN_VECTOR_P(val))
			      {
				XEN val0;
				val0 = XEN_VECTOR_REF(val, 0);
				if (XEN_NUMBER_P(val0))
				  {
				    if (XEN_EXACT_P(val0))
				      return(R_INT_VECTOR);
				    else return(R_FLOAT_VECTOR);
				  }
				else
				  if (VCT_P(val0)) return(R_VCT_VECTOR); else
				    if ((mus_xen_p(val0)) || (XEN_BOOLEAN_P(val0))) return(R_CLM_VECTOR);
			      }
			    else
			      /* order matters here (list is subset of pair) */
			      if (XEN_LIST_P(val))
				{
				  if ((!(XEN_NULL_P(val))) &&
				      (XEN_SYMBOL_P(XEN_CAR(val))))
				    {
				      int type;
				      type = name_to_type(XEN_SYMBOL_TO_C_STRING(XEN_CAR(val)));
				      if (type > R_ANY)
					return(type);
				    }
				  return(R_LIST); 
				}
			      else
				if (XEN_PAIR_P(val)) return(R_PAIR);
    }
  return(R_UNSPECIFIED);
}

static xen_value *add_global_var_to_ptree(ptree *prog, XEN form, XEN *rtn)
{
  XEN val = XEN_UNDEFINED;
  xen_var *var;
  int var_loc = 0, type;
  bool local_var = false;
  xen_value *v = NULL;
  ptree *upper = NULL;
  char *varname;
  varname = XEN_SYMBOL_TO_C_STRING(form);
  var = find_var_in_ptree(prog, varname);
  if (var) return(copy_xen_value(var->v));
  if (current_optimization < GLOBAL_OK) return(NULL);
  val = symbol_to_value(prog->code, form, &local_var);
  (*rtn) = val;
  if (XEN_NOT_BOUND_P(val))
    {
      upper = prog;
      while ((XEN_NOT_BOUND_P(val)) && (upper->outer_tree))
	{
	  upper = (ptree *)(upper->outer_tree);
	  val = symbol_to_value(upper->code, form, &local_var);
	}
      if (XEN_NOT_BOUND_P(val))	
	return(run_warn("can't find %s", varname));
    }
  type = xen_to_run_type(val);
  /* fprintf(stderr, "add global %s %s %s\n",varname, type_name(type), XEN_AS_STRING(val)); */
  switch (type)
    {
    case R_INT:        v = make_xen_value(R_INT, add_int_to_ptree(prog, R_XEN_TO_C_INT(val)), R_VARIABLE);                 break;
    case R_FLOAT:      v = make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, XEN_TO_C_DOUBLE(val)), R_VARIABLE);              break;
    case R_BOOL:       v = make_xen_value(R_BOOL, add_int_to_ptree(prog, (Int)XEN_TO_C_BOOLEAN(val)), R_VARIABLE);         break;
    case R_VCT:        v = make_xen_value(R_VCT, add_vct_to_ptree(prog, get_vct(val)), R_VARIABLE);                        break;
    case R_SOUND_DATA: v = make_xen_value(R_SOUND_DATA, add_sound_data_to_ptree(prog, (sound_data *)XEN_OBJECT_REF(val)), R_VARIABLE); break;
    case R_READER:     v = make_xen_value(R_READER, add_reader_to_ptree(prog, get_sf(val)), R_VARIABLE);                   break;
    case R_MIX_READER: v = make_xen_value(R_MIX_READER, add_mix_reader_to_ptree(prog, get_mf(val)), R_VARIABLE);           break;
    case R_TRACK_READER: v = make_xen_value(R_TRACK_READER, add_track_reader_to_ptree(prog, get_tf(val)), R_VARIABLE);     break;
    case R_CHAR:       v = make_xen_value(R_CHAR, add_int_to_ptree(prog, (Int)(XEN_TO_C_CHAR(val))), R_VARIABLE);          break;
    case R_STRING:     v = make_xen_value(R_STRING, add_string_to_ptree(prog, copy_string(XEN_TO_C_STRING(val))), R_VARIABLE); break;
    case R_LIST:       v = make_xen_value(R_LIST, add_xen_to_ptree(prog, val), R_VARIABLE);                                break;
    case R_PAIR:       v = make_xen_value(R_PAIR, add_xen_to_ptree(prog, val), R_VARIABLE);                                break;
    case R_SYMBOL:     v = make_xen_value(R_SYMBOL, add_xen_to_ptree(prog, val), R_VARIABLE);                              break;
    case R_KEYWORD:    v = make_xen_value(R_KEYWORD, add_xen_to_ptree(prog, val), R_VARIABLE);                             break;
    case R_CLM:        v = make_xen_value(R_CLM, add_clm_to_ptree(prog, XEN_TO_MUS_ANY(val)), R_VARIABLE);                 break;
    case R_FLOAT_VECTOR:
      {
	vct *vc;
	vc = vector_to_vct(val);
	if (vc == NULL) 
	  return(run_warn("trouble in float vector -- will try to abort ptree evaluation"));
	v = make_xen_value(R_FLOAT_VECTOR, add_vct_to_ptree(prog, vc), R_VARIABLE);
	if (v) add_obj_to_gcs(prog, R_FLOAT_VECTOR, v->addr);
      }
      break;
    case R_VCT_VECTOR:
    case R_CLM_VECTOR:
    case R_INT_VECTOR: 
      {
	vect *iv;
	iv = read_vector(val, type);
	if (iv == NULL) return(NULL);
	v = make_xen_value(type, add_vect_to_ptree(prog, iv), R_VARIABLE);
	if (v) add_obj_to_gcs(prog, type, v->addr); /* this makes its own xen_value with true gc field */
      }
      break;
    default:
      if (type > R_ANY)
	v = make_xen_value(type, add_xen_to_ptree(prog, val), R_VARIABLE); 
      break;
    }
  if (v)
    {
      var_loc = add_outer_var_to_ptree(prog, varname, v);
      if ((v->type == R_LIST) || (v->type == R_PAIR)) /* lists aren't settable yet */
	prog->global_vars[var_loc]->unsettable = true;
      else
	{
	  if (current_optimization < GLOBAL_SET_OK)
	    prog->global_vars[var_loc]->unsettable = local_var;
	}
    }
  return(v);
}

static continuation *add_goto_to_ptree(ptree *pt, const char *name)
{
  continuation *c;
  int old_size, i;
  c = (continuation *)CALLOC(1, sizeof(continuation));
  c->name = (char *)CALLOC(256, sizeof(char));                      /* c->name is used within the call/cc to identify the continuation */
  mus_snprintf(c->name, 256, "%s", name);
  c->jump = make_xen_value(R_GOTO, add_int_to_ptree(pt, 0), R_VARIABLE); /* c->jump->addr will be the continuation PC */
  c->result = NULL;                                                 /* c->result is the returned value address */
  if (pt->gotos_size <= pt->goto_ctr)
    {
      old_size = pt->gotos_size;
      pt->gotos_size += 4;
      if (old_size == 0)
	pt->gotos = (continuation **)CALLOC(pt->gotos_size, sizeof(continuation *));
      else
	{
	  pt->gotos = (continuation **)REALLOC(pt->gotos, pt->gotos_size * sizeof(continuation *));
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
  continuation *c = NULL;
  for (i = prog->goto_ctr - 1; i >= 0; i--)
    {
      c = prog->gotos[i];
      if ((c) && (strcmp(c->name, name) == 0))
	{
	  FREE(c->name);
	  c->name = NULL;
	  break;
	}
    }
}

#define PC pt->ints[0]
#define ALL_DONE pt->ints[1]

static void eval_ptree(ptree *pt)
{
  /* evaluates program, result in prog->result, 
   *   assumes args already passed in addrs given by prog->addrs
   */
  triple *curfunc;
  ALL_DONE = (Int)false;
  /* evaluate the parse tree */
  while (!ALL_DONE)
    {
#if 0
      {
	char *buf;
	buf = describe_ptree(pt);
	fprintf(stderr, buf);
	FREE(buf);
      }
#endif
      curfunc = ((triple **)(pt->program))[PC++];
      (*(curfunc->function))(curfunc->args, pt);
#if 0
      {
	char *buf;
	buf = (*(curfunc->descr))(curfunc->args, pt);
	fprintf(stderr, "%d: %s\n", PC, buf);
	FREE(buf);
      }
#endif
    }
  PC = pt->initial_pc; /* don't reset closure junk after initial evaluation */
}

static void eval_embedded_ptree(ptree *prog, ptree *pt)
{
  Int old_pc, old_all_done;
  old_pc = pt->ints[0];
  old_all_done = pt->ints[1];
  pt->ints[0] = 0;
  pt->ints[1] = (Int)false;
  prog->ints = pt->ints;
  prog->dbls = pt->dbls;
  prog->xen_vars = pt->xen_vars;
  prog->strs = pt->strs;
  prog->vcts = pt->vcts;
  prog->sds = pt->sds;
  prog->clms = pt->clms;
  prog->vects = pt->vects;
  prog->fncs = pt->fncs;
  prog->xens = pt->xens;
  prog->readers = pt->readers;
  prog->mix_readers = pt->mix_readers;
  prog->track_readers = pt->track_readers;
  eval_ptree(prog);
  prog->ints = NULL;
  prog->dbls = NULL;
  prog->xen_vars = NULL;
  prog->strs = NULL;
  prog->vcts = NULL;
  prog->sds = NULL;
  prog->clms = NULL;
  prog->vects = NULL;
  prog->fncs = NULL;
  prog->xens = NULL;
  prog->readers = NULL;
  prog->mix_readers = NULL;
  prog->track_readers = NULL;
  pt->ints[0] = old_pc;
  pt->ints[1] = old_all_done;
}

static triple *make_triple(void (*function)(int *arg_addrs, ptree *pt),
			   char *(*descr)(int *arg_addrs, ptree *pt), 
			   xen_value **typed_args, int args)
{
  triple *trp;
  int *addrs = NULL;
  int i;
  if (args > 0)
    {
      addrs = (int *)CALLOC(args, sizeof(int));
      for (i = 0; i < args; i++) 
	addrs[i] = typed_args[i]->addr;
    }
  trp = (triple *)CALLOC(1, sizeof(triple));
  trp->function = function;
  trp->args = addrs;
  trp->descr = descr;
  return(trp);
}

static triple *va_make_triple(void (*function)(int *arg_addrs, ptree *pt), 
			      char *(*descr)(int *arg_addrs, ptree *pt), 
			      int args, ...)
{
  va_list ap;
  triple *trp;
  xen_value *v;
  int *addrs = NULL;
  int i;
  if (args > 0)
    {
      va_start(ap, args);
      addrs = (int *)CALLOC(args, sizeof(int));
      for (i = 0; i < args; i++) 
	{
	  v = va_arg(ap, xen_value *);
	  if (v) addrs[i] = v->addr;
	}
    }
  va_end(ap);
  trp = (triple *)CALLOC(1, sizeof(triple));
  trp->function = function;
  trp->args = addrs;
  trp->descr = descr;
  return(trp);
}

#define BOOL_RESULT pt->ints[args[0]]
#define BOOL_ARG_1 ((bool)(pt->ints[args[1]]))
#define BOOL_ARG_2 ((bool)(pt->ints[args[2]]))
#define BOOL_ARG_3 ((bool)(pt->ints[args[3]]))

#define INT_RESULT pt->ints[args[0]]
#define INT_ARG_1 pt->ints[args[1]]
#define INT_ARG_2 pt->ints[args[2]]
#define INT_ARG_3 pt->ints[args[3]]
#define INT_ARG_4 pt->ints[args[4]]
#define INT_ARG_5 pt->ints[args[5]]
#define INT_ARG_6 pt->ints[args[6]]

#define FLOAT_RESULT pt->dbls[args[0]]
#define FLOAT_ARG_1 pt->dbls[args[1]]
#define FLOAT_ARG_2 pt->dbls[args[2]]
#define FLOAT_ARG_3 pt->dbls[args[3]]
#define FLOAT_ARG_4 pt->dbls[args[4]]

#define VCT_RESULT pt->vcts[args[0]]
#define DESC_VCT_RESULT ((args[0] < pt->vct_ctr) ? pt->vcts[args[0]] : NULL)
#define VCT_ARG_1 pt->vcts[args[1]]
#define VCT_ARG_2 pt->vcts[args[2]]
#define VCT_ARG_3 pt->vcts[args[3]]
#define VCT_ARG_4 pt->vcts[args[4]]
#define VCT_ARG_5 pt->vcts[args[5]]
#define DESC_VCT_ARG_1 ((args[1] < pt->vct_ctr) ? pt->vcts[args[1]] : NULL)
#define DESC_VCT_ARG_2 ((args[2] < pt->vct_ctr) ? pt->vcts[args[2]] : NULL)
#define DESC_VCT_ARG_3 ((args[3] < pt->vct_ctr) ? pt->vcts[args[3]] : NULL)
#define DESC_VCT_ARG_4 ((args[4] < pt->vct_ctr) ? pt->vcts[args[4]] : NULL)
#define DESC_VCT_ARG_5 ((args[5] < pt->vct_ctr) ? pt->vcts[args[5]] : NULL)

#define SOUND_DATA_RESULT pt->sds[args[0]]
#define DESC_SOUND_DATA_RESULT ((args[0] < pt->sd_ctr) ? pt->sds[args[0]] : NULL)
#define SOUND_DATA_ARG_1 pt->sds[args[1]]
#define SOUND_DATA_ARG_2 pt->sds[args[2]]
#define DESC_SOUND_DATA_ARG_1 ((args[1] < pt->sd_ctr) ? pt->sds[args[1]] : NULL)
#define DESC_SOUND_DATA_ARG_2 ((args[2] < pt->sd_ctr) ? pt->sds[args[2]] : NULL)

#define STRING_RESULT pt->strs[args[0]]
#define STRING_ARG_1 pt->strs[args[1]]
#define STRING_ARG_2 pt->strs[args[2]]

#define CHAR_RESULT pt->ints[args[0]]
#define CHAR_ARG_1 ((char)(pt->ints[args[1]]))
#define CHAR_ARG_2 ((char)(pt->ints[args[2]]))

#define CLM_RESULT pt->clms[args[0]]
#define DESC_CLM_RESULT ((args[0] < pt->clm_ctr) ? pt->clms[args[0]] : NULL)
#define CLM_ARG_1 pt->clms[args[1]]
#define CLM_ARG_2 pt->clms[args[2]]
#define CLM_ARG_3 pt->clms[args[3]]
#define CLM_ARG_4 pt->clms[args[4]]
#define DESC_CLM_ARG_1 ((args[1] < pt->clm_ctr) ? pt->clms[args[1]] : NULL)
#define DESC_CLM_ARG_2 ((args[2] < pt->clm_ctr) ? pt->clms[args[2]] : NULL)
#define DESC_CLM_ARG_3 ((args[3] < pt->clm_ctr) ? pt->clms[args[3]] : NULL)
#define DESC_CLM_ARG_4 ((args[4] < pt->clm_ctr) ? pt->clms[args[4]] : NULL)

#define READER_RESULT pt->readers[args[0]]
#define DESC_READER_RESULT ((args[0] < pt->reader_ctr) ? pt->readers[args[0]] : NULL)
#define READER_ARG_1 pt->readers[args[1]]
#define READER_ARG_2 pt->readers[args[2]]
#define DESC_READER_ARG_1 ((args[1] < pt->reader_ctr) ? pt->readers[args[1]] : NULL)
#define DESC_READER_ARG_2 ((args[2] < pt->reader_ctr) ? pt->readers[args[2]] : NULL)

#define MIX_READER_RESULT pt->mix_readers[args[0]]
#define DESC_MIX_READER_RESULT ((args[0] < pt->mix_reader_ctr) ? pt->mix_readers[args[0]] : NULL)
#define MIX_READER_ARG_1 pt->mix_readers[args[1]]
#define MIX_READER_ARG_2 pt->mix_readers[args[2]]
#define DESC_MIX_READER_ARG_1 ((args[1] < pt->mix_reader_ctr) ? pt->mix_readers[args[1]] : NULL)
#define DESC_MIX_READER_ARG_2 ((args[2] < pt->mix_reader_ctr) ? pt->mix_readers[args[2]] : NULL)

#define TRACK_READER_RESULT pt->track_readers[args[0]]
#define DESC_TRACK_READER_RESULT ((args[0] < pt->track_reader_ctr) ? pt->track_readers[args[0]] : NULL)
#define TRACK_READER_ARG_1 pt->track_readers[args[1]]
#define TRACK_READER_ARG_2 pt->track_readers[args[2]]
#define DESC_TRACK_READER_ARG_1 ((args[1] < pt->track_reader_ctr) ? pt->track_readers[args[1]] : NULL)
#define DESC_TRACK_READER_ARG_2 ((args[2] < pt->track_reader_ctr) ? pt->track_readers[args[2]] : NULL)

#define FNC_RESULT ((ptree **)(pt->fncs))[args[0]]
#define DESC_FNC_RESULT ((args[0] < pt->fnc_ctr) ? pt->fncs[args[0]] : NULL)
#define FNC_ARG_1 ((ptree **)(pt->fncs))[args[1]]
#define FNC_ARG_2 ((ptree **)(pt->fncs))[args[2]]
#define FNC_ARG_3 ((ptree **)(pt->fncs))[args[3]]
#define FNC_ARG_4 ((ptree **)(pt->fncs))[args[4]]
#define FNC_ARG_5 ((ptree **)(pt->fncs))[args[5]]
#define FNC_ARG_6 ((ptree **)(pt->fncs))[args[6]]
#define DESC_FNC_ARG_1 ((args[1] < pt->fnc_ctr) ? pt->fncs[args[1]] : NULL)
#define DESC_FNC_ARG_2 ((args[2] < pt->fnc_ctr) ? pt->fncs[args[2]] : NULL)
#define DESC_FNC_ARG_3 ((args[3] < pt->fnc_ctr) ? pt->fncs[args[3]] : NULL)
#define DESC_FNC_ARG_4 ((args[4] < pt->fnc_ctr) ? pt->fncs[args[4]] : NULL)
#define DESC_FNC_ARG_5 ((args[5] < pt->fnc_ctr) ? pt->fncs[args[5]] : NULL)
#define DESC_FNC_ARG_6 ((args[6] < pt->fnc_ctr) ? pt->fncs[args[6]] : NULL)

#define XEN_RESULT pt->xens[args[0]]
#define DESC_XEN_RESULT ((args[0] < pt->xen_ctr) ? pt->xens[args[0]] : NULL)
#define RXEN_ARG_1 pt->xens[args[1]]
#define RXEN_ARG_2 pt->xens[args[2]]
#define DESC_RXEN_ARG_1 ((args[1] < pt->xen_ctr) ? pt->xens[args[1]] : NULL)
#define DESC_RXEN_ARG_2 ((args[2] < pt->xen_ctr) ? pt->xens[args[2]] : NULL)

#define VECT_RESULT pt->vects[args[0]]
#define DESC_VECT_RESULT ((args[0] < pt->vect_ctr) ? pt->vects[args[0]] : NULL)
#define VECT_ARG_1 pt->vects[args[1]]
#define VECT_ARG_2 pt->vects[args[2]]
#define DESC_VECT_ARG_1 ((args[1] < pt->vect_ctr) ? pt->vects[args[1]] : NULL)
#define DESC_VECT_ARG_2 ((args[2] < pt->vect_ctr) ? pt->vects[args[2]] : NULL)


static void quit(int *args, ptree *pt) {ALL_DONE = (Int)true;}
static char *descr_quit(int *args, ptree *pt) {return(copy_string("quit"));}

static void jump(int *args, ptree *pt) {PC += pt->ints[args[0]];}
static char *descr_jump(int *args, ptree *pt) {return(mus_format("jump " INT_PT , args[0], INT_RESULT));}

static void jump_abs(int *args, ptree *pt) {PC = pt->ints[args[0]];}
static char *descr_jump_abs(int *args, ptree *pt) {return(mus_format("goto " INT_PT , args[0], INT_RESULT));}

static void jump_indirect(int *args, ptree *pt) {PC = pt->ints[pt->ints[args[0]]];}
static char *descr_jump_indirect(int *args, ptree *pt) {return(mus_format("goto [" INT_PT "=" INT_STR, args[0], INT_RESULT, pt->ints[pt->ints[args[0]]]));}

static void jump_if(int *args, ptree *pt) {if (pt->ints[args[1]] != 0) PC += pt->ints[args[0]];}
static char *descr_jump_if(int *args, ptree *pt) 
{
  return(mus_format("if (" BOOL_PT ") jump " INT_PT , args[1], B2S(INT_ARG_1), args[0], INT_RESULT));
}

static void jump_if_equal(int *args, ptree *pt) {if (pt->ints[args[1]] == pt->ints[args[2]]) PC = pt->ints[args[0]];}
static char *descr_jump_if_equal(int *args, ptree *pt) 
{
  return(mus_format("if (" INT_PT " == " INT_PT ") goto " INT_PT , args[1], INT_ARG_1, args[2], INT_ARG_2, args[0], INT_RESULT));
}

static void jump_if_not(int *args, ptree *pt) {if (pt->ints[args[1]] == 0) PC += pt->ints[args[0]];}
static char *descr_jump_if_not(int *args, ptree *pt) 
{
  return(mus_format("if (!" BOOL_PT ") jump " INT_PT , args[1], B2S(INT_ARG_1), args[0], INT_RESULT));
}

static void jump_if_not_clm(int *args, ptree *pt) {if (CLM_ARG_1 == 0) PC += pt->ints[args[0]];}
static char *descr_jump_if_not_clm(int *args, ptree *pt) 
{
  return(mus_format("if (!" CLM_PT ") jump " INT_PT , args[1], DESC_CLM_ARG_1, args[0], INT_RESULT));
}

static void store_i(int *args, ptree *pt) {INT_RESULT = INT_ARG_1;}
static char *descr_store_i(int *args, ptree *pt) {return(mus_format( INT_PT " = " INT_PT , args[0], INT_RESULT, args[1], INT_ARG_1));}

static void store_f(int *args, ptree *pt) {FLOAT_RESULT = FLOAT_ARG_1;}
static char *descr_store_f(int *args, ptree *pt) {return(mus_format( FLT_PT " = " FLT_PT , args[0], FLOAT_RESULT, args[1], FLOAT_ARG_1));}

static void store_f_i(int *args, ptree *pt) {INT_RESULT = (Int)FLOAT_ARG_1;}
static char *descr_store_f_i(int *args, ptree *pt) {return(mus_format( INT_PT " = " FLT_PT , args[0], INT_RESULT, args[1], FLOAT_ARG_1));}

static void store_i_f(int *args, ptree *pt) {FLOAT_RESULT = (Double)INT_ARG_1;}
static char *descr_store_i_f(int *args, ptree *pt) {return(mus_format( FLT_PT " = " INT_PT , args[0], FLOAT_RESULT, args[1], INT_ARG_1));}

static void store_false(int *args, ptree *pt) {BOOL_RESULT = (Int)false;}
static char *descr_store_false(int *args, ptree *pt) {return(mus_format( BOOL_PT " = 0", args[0], B2S(BOOL_RESULT)));}

static void store_true(int *args, ptree *pt) {BOOL_RESULT = (Int)true;}
static char *descr_store_true(int *args, ptree *pt) {return(mus_format( BOOL_PT " = 1", args[0], B2S(BOOL_RESULT)));}

static void store_b(int *args, ptree *pt) {BOOL_RESULT = BOOL_ARG_1;}
static char *descr_store_b(int *args, ptree *pt) {return(mus_format( BOOL_PT " = " BOOL_PT , args[0], B2S(BOOL_RESULT), args[1], B2S(BOOL_ARG_1)));}

static void store_c(int *args, ptree *pt) {CHAR_RESULT = (int)CHAR_ARG_1;}
static char *descr_store_c(int *args, ptree *pt) {return(mus_format( CHR_PT " = " CHR_PT , args[0], (char)(CHAR_RESULT), args[1], CHAR_ARG_1));}

static void store_x(int *args, ptree *pt) {XEN_RESULT = RXEN_ARG_1;}
static char *descr_store_x(int *args, ptree *pt) {return(mus_format( XEN_PT " = " XEN_PT , args[0], DESC_XEN_RESULT, args[1], DESC_RXEN_ARG_1));}

static void store_s(int *args, ptree *pt) 
{
  if (STRING_RESULT) FREE(STRING_RESULT);
  STRING_RESULT = copy_string(STRING_ARG_1);
}
static char *descr_store_s(int *args, ptree *pt) 
{
  return(mus_format( STR_PT " = " STR_PT , args[0], STRING_RESULT, args[1], STRING_ARG_1));
}

#if 0
static void store_v(int *args, ptree *pt) {VCT_RESULT = VCT_ARG_1;}
static char *descr_store_v(int *args, ptree *pt) 
{
  return(mus_format( VCT_PT " = " VCT_PT ")", args[0], DESC_VCT_RESULT, args[1], DESC_VCT_ARG_1));
}
static void store_g(int *args, ptree *pt) {CLM_RESULT = CLM_ARG_1;}
static char *descr_store_g(int *args, ptree *pt) 
{
  return(mus_format( CLM_PT " = " CLM_PT ")", args[0], DESC_CLM_RESULT, args[1], DESC_CLM_ARG_1));
}
#endif

static xen_value *convert_int_to_dbl(ptree *prog, xen_value *i)
{
  xen_value *val;
  val = make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, 0.0), i->constant);
  if (i->constant == R_CONSTANT)
    prog->dbls[val->addr] = (Double)(prog->ints[i->addr]);
  else add_triple_to_ptree(prog, va_make_triple(store_i_f, descr_store_i_f, 2, val, i));
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
  else add_triple_to_ptree(prog, va_make_triple(store_f_i, descr_store_f_i, 2, v, i));
  return(v);
}

static triple *set_var(ptree *pt, xen_value *var, xen_value *init_val)
{
  /* this also affects return types of things like "if" */
  switch (var->type)
    {
    case R_FLOAT:
      return(add_triple_to_ptree(pt, va_make_triple(store_f, descr_store_f, 2, var, init_val)));
      break;
    case R_STRING:
      return(add_triple_to_ptree(pt, va_make_triple(store_s, descr_store_s, 2, var, init_val)));
      break;
    case R_INT:
      return(add_triple_to_ptree(pt, va_make_triple(store_i, descr_store_i, 2, var, init_val)));
      break;
    case R_BOOL:
      return(add_triple_to_ptree(pt, va_make_triple(store_b, descr_store_b, 2, var, init_val)));
      break;
    case R_CHAR:
      return(add_triple_to_ptree(pt, va_make_triple(store_c, descr_store_c, 2, var, init_val)));
      break;
    case R_LIST: case R_PAIR: case R_SYMBOL: case R_KEYWORD:
      return(add_triple_to_ptree(pt, va_make_triple(store_x, descr_store_x, 2, var, init_val)));
      break;
#if 0
      /* next two are direct -- no copy, no free */
    case R_VCT:
      return(add_triple_to_ptree(pt, va_make_triple(store_v, descr_store_v, 2, var, init_val)));
      break;
    case R_CLM:
      return(add_triple_to_ptree(pt, va_make_triple(store_g, descr_store_g, 2, var, init_val)));
      break;
#endif
      /* case R_SOUND_DATA: free_sound_data in sndlib2xen.c and sound_data_dup in the RUBY section */
      /* case R_READER: case R_MIX_READER: case R_TRACK_READER: free for each + copy_reader? */
      /* case R_FLOAT_VECTOR: case R_INT_VECTOR: case R_VCT_VECTOR: case R_CLM_VECTOR: need free/copy */
    }
  /* this is not necessarily an error as long as we don't actually allow
   *   explicit set! of the unhandled types -- a let binding simply
   *   passes the reference through, which should be ok since it's read-only.
   */
  /*
  fprintf(stderr,"set: %s(%s. %d) %s(%s, %d) failed\n", 
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

static xen_value *walk(ptree *prog, XEN form, walk_result_t walk_result);

static xen_value *walk_sequence(ptree *prog, XEN body, walk_result_t need_result, const char *name)
{
  xen_value *v;
  int i, body_forms;
  XEN lbody;
  v = NULL;
  if (XEN_NOT_BOUND_P(body)) return(NULL);
  body_forms = XEN_LIST_LENGTH(body);
  if (body_forms == 0) 
    return(make_xen_value(R_BOOL, add_int_to_ptree(prog, (Int)false), R_CONSTANT));
  lbody = body;
  for (i = 0; i < body_forms; i++, lbody = XEN_CDR(lbody))
    {
      if (v) FREE(v);
      v = walk(prog, 
	       XEN_CAR(lbody), 
	       ((need_result != DONT_NEED_RESULT) && 
		(i == (body_forms - 1))) ? 
	       NEED_ANY_RESULT : DONT_NEED_RESULT);
      if (v == NULL) 
	return(run_warn("%s: can't handle %s", name, XEN_AS_STRING(XEN_CAR(lbody))));
    }
  return(v);
}

static xen_value *lambda_form(ptree *prog, XEN form, bool separate, xen_value **args, int num_args, XEN local_proc);

static char *define_form(ptree *prog, XEN form)
{
  /* behaves here just like a sequential bind (let*) */
  XEN var, val;
  xen_value *v, *vs;
  if ((!(XEN_LIST_P(form))) || (XEN_NULL_P(form))) return(NULL);
  var = XEN_CADR(form);
  if (!(XEN_SYMBOL_P(var)))
    {
      if (XEN_LIST_P(var))
	{
	  v = lambda_form(prog, form, false, NULL, 0, XEN_FALSE);
	  if (v == NULL) return(mus_format("can't handle this define: %s", XEN_AS_STRING(form)));
	  add_var_to_ptree(prog, XEN_SYMBOL_TO_C_STRING(XEN_CAR(var)), v);
	  FREE(v);
	  return(NULL);
	}
      else return(mus_format("can't handle this definition: %s", XEN_AS_STRING(var)));
    }
  val = XEN_CADDR(form);
  v = walk(prog, val, NEED_ANY_RESULT);
  if (v == NULL) return(mus_format("can't handle this define value: %s", XEN_AS_STRING(val)));
  vs = transfer_value(prog, v);
  add_var_to_ptree(prog, XEN_SYMBOL_TO_C_STRING(var), vs);
  set_var(prog, vs, v);
  FREE(vs);
  FREE(v);
  return(NULL);
}

static XEN handle_defines(ptree *prog, XEN forms)
{
  XEN form;
  char *err;
  if (!(XEN_LIST_P(forms))) return(forms);
  while (!(XEN_NULL_P(forms)))
    {
      form = XEN_CAR(forms);
      if ((XEN_LIST_P(form)) && 
	  (XEN_NOT_NULL_P(form)) &&
	  (strcmp("define", XEN_AS_STRING(XEN_CAR(form))) == 0))
	{
	  err = define_form(prog, form);
	  if (err != NULL) 
	    {
	      run_warn(err);
	      FREE(err);
	      return(XEN_UNDEFINED);
	    }
	  forms = XEN_CDR(forms);
	}
      else return(forms);
    }
  return(forms);
}

static char *parallel_binds(ptree *prog, XEN old_lets, const char *name)
{
  XEN var, lets;
  xen_value *v = NULL;
  xen_value **vs, **old_vs;
  int i, vars;
  lets = old_lets;
  vars = XEN_LIST_LENGTH(lets);
  if (vars > 0)
    {
      vs = (xen_value **)CALLOC(vars, sizeof(xen_value *));
      old_vs = (xen_value **)CALLOC(vars, sizeof(xen_value *));
      for (i = 0; i < vars; i++, lets = XEN_CDR(lets))
	{
	  var = XEN_CAR(lets);
	  v = walk(prog, XEN_CADR(var), NEED_ANY_RESULT);
	  if (v == NULL)
	    {
	      int j;
	      for (j = 0; j < i; j++)
		{
		  if (old_vs[j]) FREE(old_vs[j]);
		  if (vs[j]) FREE(vs[j]);
		}
	      FREE(vs);
	      FREE(old_vs);
	      return(mus_format("can't handle %s var: %s", name, XEN_AS_STRING(lets)));
	    }
	  old_vs[i] = v;
	  vs[i] = transfer_value(prog, v);
	}
      lets = old_lets;
      for (i = 0; i < vars; i++, lets = XEN_CDR(lets))
	{
	  var = XEN_CAR(lets);
	  add_var_to_ptree(prog, XEN_SYMBOL_TO_C_STRING(XEN_CAR(var)), vs[i]);
	  set_var(prog, vs[i], old_vs[i]); /* establish let binding */
	  /* (run-eval '(do ((i 0 (1+ i))) ((= i 3)) (let ((a 1)) (set! a (+ a 1)) (display a)))) */
	  FREE(vs[i]);
	  FREE(old_vs[i]);
	}
      FREE(old_vs);
      FREE(vs);
    }
  return(NULL);
}

static char *sequential_binds(ptree *prog, XEN old_lets, const char *name)
{
  XEN var, lets;
  xen_value *v = NULL, *vs;
  int i, vars;
  lets = old_lets;
  vars = XEN_LIST_LENGTH(lets);
  if (vars > 0)
    {
      for (i = 0; i < vars; i++, lets = XEN_CDR(lets))
	{
	  var = XEN_CAR(lets);
	  v = walk(prog, XEN_CADR(var), NEED_ANY_RESULT);
	  if (v == NULL)
	    return(mus_format("can't handle %s var: %s", name, XEN_AS_STRING(lets)));
	  vs = transfer_value(prog, v);
	  add_var_to_ptree(prog, XEN_SYMBOL_TO_C_STRING(XEN_CAR(var)), vs);
	  set_var(prog, vs, v);
	  FREE(vs);
	  FREE(v);
	}
    }
  return(NULL);
}

static char *declare_args(ptree *prog, XEN form, int default_arg_type, bool separate, xen_value **cur_args, int num_args)
{
  XEN arg, args, declarations = XEN_FALSE, declaration;
  xen_value *v = NULL;
  int i, arg_num, arg_type;
  char *type;
  if (separate)
    args = XEN_CADR(form);
  else args = XEN_CDADR(form);
  if (!(XEN_LIST_P(args))) return(mus_format("can't handle non-explicit lambda args: %s", XEN_AS_STRING(args)));
  if (num_args > XEN_LIST_LENGTH(args))
    return(mus_format("too many args: %s", XEN_AS_STRING(args)));
  if (num_args == 0)
    {
      declarations = XEN_CADDR(form);
      if ((XEN_LIST_P(declarations)) && 
	  (XEN_NOT_NULL_P(declarations)) &&
	  (XEN_SYMBOL_P(XEN_CAR(declarations))) &&
	  (strcmp(XEN_SYMBOL_TO_C_STRING(XEN_CAR(declarations)), "declare") == 0))
	declarations = XEN_CDR(declarations);
      else declarations = XEN_FALSE;
      arg_num = XEN_LIST_LENGTH(args);
    }
  else arg_num = num_args;
  prog->arity = arg_num;
  if (arg_num > 0)
    {
      prog->args = (int *)CALLOC(arg_num, sizeof(int));
      prog->arg_types = (int *)CALLOC(arg_num, sizeof(int));
      for (i = 0; i < arg_num; i++, args = XEN_CDR(args))
	{
	  arg = XEN_CAR(args);
	  arg_type = default_arg_type;
	  /* (declare (x real) (a integer) (b string) ... */
	  if ((XEN_LIST_P(declarations)) && (XEN_NOT_NULL_P(declarations)))
	    {
	      declaration = XEN_CAR(declarations);
	      declarations = XEN_CDR(declarations);
	      if ((XEN_EQ_P(XEN_CAR(declaration), arg)) &&
		  (XEN_SYMBOL_P(XEN_CADR(declaration))))
		{
		  type = XEN_SYMBOL_TO_C_STRING(XEN_CADR(declaration));
		  arg_type = name_to_type(type);
		  /* fprintf(stderr, "arg %d: %s %s (%s)\n", i, XEN_AS_STRING(arg), type_name(arg_type), type); */
		  if (arg_type == R_UNSPECIFIED) /* not a predefined type name like "float" */
		    {
		      if (strcmp(type, "integer") == 0) 
			arg_type = R_INT; 
		      else
			{
			  if (strcmp(type, "real") == 0) 
			    arg_type = R_FLOAT; 
			  else /* arg_type = default_arg_type; */
			    return(mus_format("unknown type in declare: %s in %s", type, XEN_AS_STRING(XEN_CADDR(form))));
			}
		    }
		}
	      else return(mus_format("unrecognized arg in declare: %s (for %s?) in %s", 
				     XEN_AS_STRING(XEN_CAR(declaration)), 
				     XEN_AS_STRING(arg), 
				     XEN_AS_STRING(XEN_CADDR(form))));
	    }
	  else
	    {
	      if ((cur_args) && (cur_args[i + 1]))
		arg_type = cur_args[i + 1]->type;
	    }
	  add_var_to_ptree(prog, 
			   XEN_SYMBOL_TO_C_STRING(arg), 
			   v = add_empty_var_to_ptree(prog, arg_type));
	  prog->args[i] = v->addr;
	  prog->arg_types[i] = v->type;
	  FREE(v);
	}
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

static xen_value *walk_then_undefine(ptree *prog, XEN form, walk_result_t need_result, const char *name, int locals_loc)
{
  xen_value *v;
  v = walk_sequence(prog, handle_defines(prog, form), need_result, name);
  undefine_locals(prog, locals_loc);
  return(v);
}

static xen_value *lambda_form(ptree *prog, XEN form, bool separate, xen_value **args, int num_args, XEN local_proc)
{
  /* (lambda (args...) | args etc followed by forms */
  /* as args are declared as vars, put addrs in prog->args list */
  char *err;
  int i, locals_loc;
  if (got_lambda)
    {
      /* start a new ptree, walk body, return ptree as R_FUNCTION variable */
      ptree *new_tree;
      int outer_locals;
      new_tree = attach_to_ptree(prog);
      new_tree->code = local_proc;
      outer_locals = prog->var_ctr;
      err = declare_args(new_tree, form, R_INT, separate, args, num_args);
      if (err)
	{
	  run_warn(err);
	  FREE(err);
	}
      else
	{
	  new_tree->result = walk_then_undefine(new_tree, XEN_CDDR(form), NEED_ANY_RESULT, "lambda", prog->var_ctr);
	  if (new_tree->result)
	    {
	      xen_value *v;
	      add_triple_to_ptree(new_tree, make_triple(quit, descr_quit, NULL, 0));
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
      return(run_warn("can't handle this embedded lambda: %s", XEN_AS_STRING(form)));
    }
  got_lambda = true;
  locals_loc = prog->var_ctr;
  err = declare_args(prog, form, R_FLOAT, separate, args, num_args);
  if (err) return(run_warn_with_free(err));
  return(walk_then_undefine(prog, XEN_CDDR(form), NEED_ANY_RESULT, "lambda", locals_loc));
}

static xen_value *begin_form(ptree *prog, XEN form, walk_result_t need_result)
{
  return(walk_then_undefine(prog, XEN_CDR(form), need_result, "begin", prog->var_ctr));
}

static xen_value *let_star_form(ptree *prog, XEN form, walk_result_t need_result)
{
  int locals_loc;
  char *err = NULL;
  locals_loc = prog->var_ctr; /* lets can be nested */
  err = sequential_binds(prog, XEN_CADR(form), "let*");
  if (err) return(run_warn_with_free(err));
  if (!got_lambda) prog->initial_pc = prog->triple_ctr;
  return(walk_then_undefine(prog, XEN_CDDR(form), need_result, "let*", locals_loc));
}

static xen_value *let_form(ptree *prog, XEN form, walk_result_t need_result)
{
  /* keep vars until end of var section */
  XEN lets;
  char *trouble = NULL;
  int locals_loc;
  lets = XEN_CADR(form);
  locals_loc = prog->var_ctr; /* lets can be nested */
  trouble = parallel_binds(prog, lets, "let");
  if (trouble) return(run_warn_with_free(trouble));
  if (!got_lambda) prog->initial_pc = prog->triple_ctr;
  return(walk_then_undefine(prog, XEN_CDDR(form), need_result, "let", locals_loc));
}

static xen_value *if_form(ptree *prog, XEN form, walk_result_t need_result)
{
  /* form: (if selector true [false]) */
  xen_value *result = NULL, *true_result = NULL, *false_result = NULL;
  xen_value *jump_to_end = NULL, *jump_to_false, *if_value;
  int current_pc, false_pc = 0;
  bool has_false;
  has_false = (XEN_LIST_LENGTH(form) == 4);
  if_value = walk(prog, XEN_CADR(form), NEED_ANY_RESULT);                                      /* walk selector */
  if (if_value == NULL) return(run_warn("if: bad selector? %s", XEN_AS_STRING(XEN_CADR(form))));
  if ((if_value->type != R_BOOL) &&
      (if_value->type != R_CLM))
    return(run_warn("if: selector type not boolean or clm gen: %s %s", 
		    XEN_AS_STRING(XEN_CADR(form)), 
		    type_name(if_value->type)));
  if (if_value->constant == R_CONSTANT)
    {
      /* just ignore branch that can't be evaluated anyway */
      if (prog->ints[if_value->addr])
	{
	  FREE(if_value);
	  return(walk(prog, XEN_CADDR(form), need_result));
	}
      if (has_false)
	{
	  FREE(if_value);
	  return(walk(prog, XEN_CADDDR(form), need_result));
	}
      return(if_value);
    }
  current_pc = prog->triple_ctr;
  jump_to_false = make_xen_value(R_INT, add_int_to_ptree(prog, 0), R_VARIABLE);
  switch (if_value->type)
    {
    case R_BOOL: add_triple_to_ptree(prog, va_make_triple(jump_if_not, descr_jump_if_not, 2, jump_to_false, if_value)); break;
    case R_CLM: add_triple_to_ptree(prog, va_make_triple(jump_if_not_clm, descr_jump_if_not_clm, 2, jump_to_false, if_value)); break;
    }
  FREE(if_value);
  true_result = walk(prog, XEN_CADDR(form), need_result);                           /* walk true branch */
  if (true_result == NULL) 
    {
      FREE(jump_to_false);
      return(run_warn("if: can't handle true branch %s", XEN_AS_STRING(XEN_CADDR(form))));
    }
  if (need_result != DONT_NEED_RESULT)
    {
      result = add_empty_var_to_ptree(prog, true_result->type);
      set_var_no_opt(prog, result, true_result);
    }
  if (has_false) 
    {
      false_pc = prog->triple_ctr;
      jump_to_end = make_xen_value(R_INT, add_int_to_ptree(prog, 0), R_VARIABLE);
      add_triple_to_ptree(prog, va_make_triple(jump, descr_jump, 1, jump_to_end));  /* jump to end (past false) */
    }
  prog->ints[jump_to_false->addr] = prog->triple_ctr - current_pc - 1;              /* fixup jump-to-false addr */
  FREE(jump_to_false);
  if (has_false)
    {
      false_result = walk(prog, XEN_CADDDR(form), need_result);                     /* walk false branch */
      if (need_result != DONT_NEED_RESULT)
	{
	  if ((false_result) && (false_result->type != true_result->type))
	    {
	      /* #f is ok as null pointer so fixup if needed */
	      if ((false_result->type == R_BOOL) &&
		  ((true_result->type == R_CLM) || 
		   (true_result->type == R_READER) ||
		   (true_result->type == R_MIX_READER) ||
		   (true_result->type == R_TRACK_READER)))
		false_result->type = true_result->type;
	      else
		if ((true_result->type == R_BOOL) &&
		    ((false_result->type == R_CLM) || 
		     (false_result->type == R_READER) ||
		     (false_result->type == R_MIX_READER) ||
		     (false_result->type == R_TRACK_READER)))
		  true_result->type = false_result->type;
	    }
	  if ((false_result == NULL) ||
	      (false_result->type != true_result->type))
	    {
	      if (false_result == NULL)
		run_warn("if: can't handle false branch %s", XEN_AS_STRING(XEN_CADDDR(form)));
	      else run_warn("if branch types differ incompatibly: %s and %s", type_name(true_result->type), type_name(false_result->type));
	      if (result) FREE(result);
	      if (false_result) FREE(false_result);
	      if (jump_to_end) FREE(jump_to_end);
	      if (true_result) FREE(true_result);
	      return(NULL);
	    }
	  set_var_no_opt(prog, result, false_result);
	}
      prog->ints[jump_to_end->addr] = prog->triple_ctr - false_pc - 1;              /* fixup jump-past-false addr */
      FREE(jump_to_end);
    }
  else 
    {
      if (jump_to_end) FREE(jump_to_end);
      if (result) FREE(result);
      return(true_result);
    }
  if (true_result) FREE(true_result);
  if (false_result) FREE(false_result);
  if (result)
    return(result);
  return(make_xen_value(R_UNSPECIFIED, -1, R_CONSTANT));
}

static xen_value *cond_form(ptree *prog, XEN form, walk_result_t need_result)
{
  xen_value *result = NULL, *test_value = NULL, *clause_value = NULL, *jump_to_next_clause = NULL;
  xen_value **fixups = NULL;
  int current_pc = 0, len, clause_ctr, i = 0, local_len;
  XEN clauses, clause, local_clauses;
  clauses = XEN_CDR(form);
  len = XEN_LIST_LENGTH(clauses);
  if (len == 0) return(run_warn("empty cond?"));
  fixups = (xen_value **)CALLOC(len, sizeof(xen_value *));
  for (clause_ctr = 0; clause_ctr < len; clause_ctr++, clauses = XEN_CDR(clauses))
    {
      clause = XEN_CAR(clauses);
      /* check car -- if #t evaluate rest */
      if ((XEN_SYMBOL_P(XEN_CAR(clause))) &&
	  (strcmp("else", XEN_SYMBOL_TO_C_STRING(XEN_CAR(clause))) == 0))
	test_value = make_xen_value(R_BOOL, add_int_to_ptree(prog, (Int)true), R_CONSTANT);
      else test_value = walk(prog, XEN_CAR(clause), NEED_ANY_RESULT);
      if ((test_value == NULL) || (test_value->type != R_BOOL))
	return(run_warn("cond test: %s", XEN_AS_STRING(XEN_CAR(clause))));
      /* test was #t */
      local_clauses = XEN_CDR(clause); /* can be null */
      local_len = XEN_LIST_LENGTH(local_clauses);
      if (local_len > 0)
	{
	  current_pc = prog->triple_ctr;
	  jump_to_next_clause = make_xen_value(R_INT, add_int_to_ptree(prog, 0), R_VARIABLE);
	  add_triple_to_ptree(prog, va_make_triple(jump_if_not, descr_jump_if_not, 2, jump_to_next_clause, test_value));
	  clause_value = walk_sequence(prog, local_clauses, need_result, "cond");
	  if (clause_value == NULL)
	    {
	      FREE(test_value);
	      FREE(jump_to_next_clause);
	      for (i = 0; i < clause_ctr; i++) 
		if (fixups[i]) FREE(fixups[i]);
	      FREE(fixups);
	      return(NULL);
	    }
	}
      else clause_value = copy_xen_value(test_value);
      /* now at end of this cond clause block -- fixup the jump if false above, add jump past rest if true */
      if (need_result != DONT_NEED_RESULT)
	{
	  if (result == NULL)
	    result = add_empty_var_to_ptree(prog, clause_value->type);
	  else 
	    {
	      if (result->type != clause_value->type)
		{
		  run_warn("cond clause types differ: %s %s", type_name(clause_value->type), type_name(result->type));
		  FREE(clause_value);
		  FREE(result);
		  if (jump_to_next_clause) FREE(jump_to_next_clause);
		  for (i = 0; i < clause_ctr; i++) 
		    if (fixups[i]) FREE(fixups[i]);
		  FREE(fixups);
		  return(NULL);
		}
	    }
	  set_var(prog, result, clause_value);
	}
      fixups[clause_ctr] = make_xen_value(R_INT, add_int_to_ptree(prog, 0), R_VARIABLE);
      add_triple_to_ptree(prog, va_make_triple(jump_abs, descr_jump_abs, 1, fixups[clause_ctr])); 
      /* we jump to here is test was false */
      if (jump_to_next_clause)
	{
	  prog->ints[jump_to_next_clause->addr] = prog->triple_ctr - current_pc - 1;
	  FREE(jump_to_next_clause);
	  jump_to_next_clause = NULL;
	}
      FREE(clause_value);
      clause_value = NULL;
      FREE(test_value);
      test_value = NULL;
    }
  /* all the fixups are absolute jumps */
  for (i = 0; i < len; i++)
    if (fixups[i])
      {
	prog->ints[fixups[i]->addr] = prog->triple_ctr;
	FREE(fixups[i]);
      }
  if (fixups) FREE(fixups);
  if (need_result != DONT_NEED_RESULT)
    return(result);
  return(make_xen_value(R_UNSPECIFIED, -1, R_CONSTANT));
}

static xen_value *case_form(ptree *prog, XEN form, walk_result_t need_result)
{
  /* only int (constant) selectors here */
  /* (case selector ((...) exp ...) ...) with possible 'else' */
  XEN selector, body, keys, key;
  int i, j, body_len, num_keys, cur_key;
  xen_value *selval = NULL, *jump_to_selection = NULL, *v = NULL, *result = NULL, *keyval = NULL, *locval = NULL, *elseval = NULL;
  int *locations = NULL;
  xen_value **fixups = NULL;
  selector = XEN_CADR(form);
  selval = walk(prog, selector, NEED_ANY_RESULT);
  if (selval == NULL) return(run_warn("can't handle case selector: %s", XEN_AS_STRING(selector)));
  if (selval->type != R_INT) return(run_warn("case only with ints: %s", XEN_AS_STRING(selector)));
  jump_to_selection = make_xen_value(R_INT, add_int_to_ptree(prog, 0), R_VARIABLE);
  add_triple_to_ptree(prog, va_make_triple(jump_abs, descr_jump_abs, 1, jump_to_selection));
  body = XEN_CDDR(form);
  body_len = XEN_LIST_LENGTH(body);
  fixups = (xen_value **)CALLOC(body_len, sizeof(xen_value *));
  locations = (int *)CALLOC(body_len, sizeof(int));
  for (i = 0; i < body_len; i++, body = XEN_CDR(body))
    {
      /* ignore keys for now */
      locations[i] = prog->triple_ctr;
      v = walk_sequence(prog, XEN_CDAR(body), need_result, "case");
      if (v == NULL) goto CASE_ERROR;
      if (need_result != DONT_NEED_RESULT)
	{
	  if (result == NULL)
	    result = add_empty_var_to_ptree(prog, v->type);
	  else 
	    if (result->type != v->type)
	      {
		run_warn("case clause types differ: %s %s", type_name(v->type), type_name(result->type));
		FREE(v); v = NULL;
	      }
	  if (v) set_var(prog, result, v);
	}
      if (v == NULL) goto CASE_ERROR;
      FREE(v); v = NULL;
      fixups[i] = make_xen_value(R_INT, add_int_to_ptree(prog, 0), R_VARIABLE);
      add_triple_to_ptree(prog, va_make_triple(jump_abs, descr_jump_abs, 1, fixups[i])); 
    }
  /* fixup jump from selector to table of keys (here) */
  prog->ints[jump_to_selection->addr] = prog->triple_ctr;
  FREE(jump_to_selection); jump_to_selection = NULL;
  /* now make the selection */
  body = XEN_CDDR(form);
  for (i = 0; i < body_len; i++, body = XEN_CDR(body))
    {
      keys = XEN_CAAR(body);
      if (XEN_SYMBOL_P(keys))
	{
	  if ((XEN_SYMBOL_P(keys)) &&
	      (strcmp(XEN_SYMBOL_TO_C_STRING(keys), "else") == 0))
	    elseval = make_xen_value(R_INT, i, R_CONSTANT);
	  else 
	    {
	      run_warn("bad case key: %s", XEN_AS_STRING(keys));
	      goto CASE_ERROR;
	    }
	}
      else
	{
	  num_keys = XEN_LIST_LENGTH(keys);
	  for (j = 0; j < num_keys; j++, keys = XEN_CDR(keys))
	    {
	      key = XEN_CAR(keys);
	      if (!(XEN_OFF_T_P(key)))
		{
		  run_warn("case only accepts integer selectors: %s", XEN_AS_STRING(key));
		  goto CASE_ERROR;
		}
	      cur_key = XEN_TO_C_INT(key);
	      keyval = make_xen_value(R_INT, add_int_to_ptree(prog, cur_key), R_CONSTANT);
	      locval = make_xen_value(R_INT, add_int_to_ptree(prog, locations[i]), R_CONSTANT);
	      add_triple_to_ptree(prog, va_make_triple(jump_if_equal, descr_jump_if_equal, 3, locval, selval, keyval));
	      FREE(keyval); keyval = NULL;
	      FREE(locval); locval = NULL;
	    }
	}
    }
  /* now check for else clause */
  if (elseval)
    {
      locval = make_xen_value(R_INT, add_int_to_ptree(prog, locations[elseval->addr]), R_CONSTANT);
      add_triple_to_ptree(prog, va_make_triple(jump_abs, descr_jump_abs, 1, locval));
      FREE(locval);
      FREE(elseval);
    }
  FREE(locations);
  if (fixups)
    {
      for (i = 0; i < body_len; i++)
	if (fixups[i])
	  {
	    prog->ints[fixups[i]->addr] = prog->triple_ctr;
	    FREE(fixups[i]);
	  }
      FREE(fixups);
    }
  if (selval) FREE(selval);
  if (need_result != DONT_NEED_RESULT)
    return(result);
  return(make_xen_value(R_UNSPECIFIED, -1, R_CONSTANT));

 CASE_ERROR:
  /* try to avoid endless repetition of cleanup code */
  if (selval) FREE(selval);
  if (fixups)
    {
      for (j = 0; j < body_len; j++) 
	if (fixups[j])
	  FREE(fixups[j]);
      FREE(fixups);
    }
  if (locations) FREE(locations);
  if (result) FREE(result);
  if (jump_to_selection) FREE(jump_to_selection);
  return(NULL);
}

static bool list_member(XEN symb, XEN varlst)
{
  XEN lst;
  for (lst = varlst; (XEN_NOT_NULL_P(lst)); lst = XEN_CDR(lst))
    if (XEN_EQ_P(symb, XEN_CAR(lst)))
      return(true);
  return(false);
}

static bool tree_member(XEN varlst, XEN expr)
{
  /* is any member of varlst found in expr */
  /* search expr (assumed to be a list here) for reference to any member of varlst */
  XEN symb;
  if (XEN_NULL_P(expr)) return(false);
  symb = XEN_CAR(expr);
  if (XEN_SYMBOL_P(symb))
    {
      if (list_member(symb, varlst))
	return(true);
    }
  else
    {
      if ((XEN_LIST_P(symb)) && (tree_member(varlst, symb)))
	return(true);
    }
  return(tree_member(varlst, XEN_CDR(expr)));
}

static xen_value *do_form(ptree *prog, XEN form, walk_result_t need_result)
{
  /* (do ([(var val [up])...]) (test [res ...]) [exp ...]): (do () (#t))  */

  xen_value *result = NULL, *test, *expr, *jump_to_result, *jump_to_test, **exprs = NULL;
  XEN var, vars, done, body, results, test_form;
  xen_var *vr;
  int i, locals_loc, test_loc, varlen, jump_loc;
  char *trouble;
  vars = XEN_CADR(form);
  done = XEN_CADDR(form);
  if (XEN_NULL_P(done)) return(run_warn("do: null test")); /* invalid in Scheme */
  body = XEN_CDDDR(form);
  test_form = XEN_CAR(done);
  results = XEN_CDR(done);

  /* let to set init */
  /* test ctr */
  /* test if t goto res */
  /*      if f goto body, then steps, jump to test ctr */
  /*   for res, return last if any */

  locals_loc = prog->var_ctr; /* lets can be nested */
  trouble = parallel_binds(prog, vars, "do");
  if (trouble) return(run_warn_with_free(trouble));

  test_loc = prog->triple_ctr;
  test = walk(prog, test_form, NEED_ANY_RESULT);
  if (test == NULL) return(NULL);
  if (test->type != R_BOOL) 
    {
      FREE(test);
      return(run_warn("do test must be boolean: %s", XEN_AS_STRING(test_form)));
    }
  /* if test true, jump to result section */
  jump_loc = prog->triple_ctr;
  jump_to_result = make_xen_value(R_INT, add_int_to_ptree(prog, 0), R_VARIABLE);
  add_triple_to_ptree(prog, va_make_triple(jump_if, descr_jump_if, 2, jump_to_result, test));
  FREE(test);
  /* now walk the do body */
  expr = walk_sequence(prog, body, DONT_NEED_RESULT, "do");
  if (expr == NULL)
    {
      FREE(jump_to_result);
      return(NULL);
    }
  FREE(expr);
  expr = NULL;
  /* now increment the vars (if step-val exists) -- increments are done first (the norm in Scheme) */
  varlen = XEN_LIST_LENGTH(vars);
  if (varlen > 0)
    {
      bool sequential = true;
      if (varlen > 1)    /* 0=doesn't matter, 1=no possible non-sequential ref */
	/* the step expr can refer to a previous do local var, but it is to the previous value -- very weird semantics */
	{
	  int loc;
	  XEN varlst = XEN_EMPTY_LIST, update = XEN_FALSE;
	  loc = snd_protect(varlst);
	  vars = XEN_CADR(form);
	  varlst = XEN_CONS(XEN_CAAR(vars), varlst);
	  for (vars = XEN_CDR(vars), i = 1; i < varlen; i++, vars = XEN_CDR(vars))
	    {
	      var = XEN_CAR(vars);
	      /* current var is CAR(var), init can be ignored (it would refer to outer var), update is CADDR(var) */
	      /*   we'll scan CADDR for any member of varlst */
	      if ((XEN_NOT_NULL_P(XEN_CDDR(var))) && (XEN_NOT_NULL_P(XEN_CADDR(var))))
		{
		  /* if update null, can't be ref */
		  update = XEN_CADDR(var);
		  if (((XEN_LIST_P(update)) && (tree_member(varlst, update))) ||
		      ((XEN_SYMBOL_P(update)) && (list_member(update, varlst))))
		    {
		      /* fprintf(stderr, "found seq ref %s\n", XEN_AS_STRING(vars)); */
		      sequential = false;
		      break;
		    }
		}
	      varlst = XEN_CONS(XEN_CAR(var), varlst);
	    }
	  snd_unprotect_at(loc);
	  if (!sequential)
	    exprs = (xen_value **)CALLOC(varlen, sizeof(xen_value *));
	}
      for (vars = XEN_CADR(form), i = 0; i < varlen; i++, vars = XEN_CDR(vars))
	{
	  var = XEN_CAR(vars);
	  if ((XEN_NOT_NULL_P(XEN_CDDR(var))) && (XEN_NOT_NULL_P(XEN_CADDR(var))))
	    {
	      if ((sequential) && (expr)) FREE(expr);
	      expr = walk(prog, XEN_CADDR(var), NEED_ANY_RESULT);
	      /* (run-eval '(do ((i 0 (1+ i)) (j 0 (1+ i)) (k 0 (hiho k))) ((= i 3)) 0)) */
	      if (expr == NULL)
		{
		  if (exprs) 
		    {
		      int k;
		      for (k = 0; k < i; k++) if (exprs[k]) FREE(exprs[k]);
		      FREE(exprs);
		    }
		  FREE(jump_to_result);
		  return(NULL);
		}
	      if (sequential)
		{
		  vr = find_var_in_ptree(prog, XEN_SYMBOL_TO_C_STRING(XEN_CAR(var)));
		  if (((expr->type == R_FLOAT) || (expr->type == R_INT)) &&
		      (XEN_LIST_P(XEN_CADDR(var))) && /* otherwise no intermediate was generated */
		      (prog->triple_ctr > 0))
		    {
		      /* as in set!, if possible, simply redirect the previous store to us (implicit set) */
		      int *addrs;
		      addrs = ((triple **)(prog->program))[prog->triple_ctr - 1]->args;
		      if ((addrs) && 
			  (addrs[0] == expr->addr))
			addrs[0] = vr->v->addr; /* redirect the store to us */
		      else set_var(prog, vr->v, expr);
		    }
		  else set_var(prog, vr->v, expr);
		}
	      else 
		{
		  /* here if expr is a variable reference (not a temp or a constant), we need to sequester it,
		   * since the variable in question may be updated before we re-reference it
		   */
		  if (XEN_SYMBOL_P(XEN_CADDR(var)))
		    {
		      xen_value *temp;
		      vr = find_var_in_ptree(prog, XEN_SYMBOL_TO_C_STRING(XEN_CADDR(var)));
		      temp = add_empty_var_to_ptree(prog, vr->v->type);
		      set_var(prog, temp, expr);
		      FREE(expr);
		      expr = NULL;
		      exprs[i] = temp;
		    }
		  else exprs[i] = expr;
		}
	    }
	}
      if ((sequential) && (expr)) FREE(expr);
      if (!sequential)
	{
	  for (vars = XEN_CADR(form), i = 0; i < varlen; i++, vars = XEN_CDR(vars))
	    if (exprs[i])
	      {
		var = XEN_CAR(vars);
		vr = find_var_in_ptree(prog, XEN_SYMBOL_TO_C_STRING(XEN_CAR(var)));
		set_var(prog, vr->v, exprs[i]);
		FREE(exprs[i]);
	      }
	  FREE(exprs);
	}
    }
  /* jump back to test */
  jump_to_test = make_xen_value(R_INT, add_int_to_ptree(prog, 0), R_VARIABLE);
  add_triple_to_ptree(prog, va_make_triple(jump_abs, descr_jump_abs, 1, jump_to_test));
  prog->ints[jump_to_test->addr] = test_loc;
  FREE(jump_to_test);
  /* fixup jump from true test above */
  prog->ints[jump_to_result->addr] = prog->triple_ctr - jump_loc - 1;
  FREE(jump_to_result);
  /* now the result block */
  if (XEN_NOT_NULL_P(results))
    result = walk_sequence(prog, results, NEED_ANY_RESULT, "do");
  else result = make_xen_value(R_BOOL, add_int_to_ptree(prog, (Int)false), R_CONSTANT);
  undefine_locals(prog, locals_loc);
  return(result);
}

static xen_value *callcc_form(ptree *prog, XEN form, walk_result_t need_result)
{
  XEN func_form, continuation_name;
  continuation *c;
  xen_value *v = NULL;
  func_form = XEN_CADR(form);
  continuation_name = XEN_CAR(XEN_CADR(func_form));
  c = add_goto_to_ptree(prog, XEN_SYMBOL_TO_C_STRING(continuation_name));
  v = walk_sequence(prog, XEN_CDDR(func_form), need_result, "call/cc");
  if (v == NULL) return(NULL);
  if (c->result)
    {
      if (v->type != c->result->type)
	{
	  FREE(v);
	  return(run_warn("call/cc: types differ"));
	}
      if (need_result != DONT_NEED_RESULT)
	set_var(prog, c->result, v);
    }
  if (v) FREE(v);
  /* fixup the continuation jump, etc */
  prog->ints[c->jump->addr] = prog->triple_ctr;
  if (c->result)
    v = copy_xen_value(c->result);
  else v = make_xen_value(R_BOOL, add_int_to_ptree(prog, (Int)false), R_CONSTANT);
  erase_goto(prog, c->name); /* now access to this depends on a set! somewhere */
  return(v);
}

static xen_value *or_form(ptree *prog, XEN form, walk_result_t ignored)
{
  /* (or ...) returning as soon as #t seen -- assume booleans only here */
  XEN body;
  xen_value *v = NULL, *result = NULL, *jump_to_end;
  xen_value **fixups;
  int i, j, body_forms;
  body = XEN_CDR(form);
  body_forms = XEN_LIST_LENGTH(body);
  if (body_forms == 0)                  /* (or) -> #f */
    return(make_xen_value(R_BOOL, add_int_to_ptree(prog, 0), R_CONSTANT));
  fixups = (xen_value **)CALLOC(body_forms, sizeof(xen_value *));
  for (i = 0; i < body_forms; i++, body = XEN_CDR(body))
    {
      v = walk(prog, XEN_CAR(body), NEED_ANY_RESULT);
      if ((v == NULL) || ((v->type != R_BOOL) && (!(POINTER_OR_GOTO_P(v->type)))))
	{
	  bool was_null;
	  was_null = (v == NULL);
	  for (j = 0; j < i; j++)
	    if (fixups[j]) FREE(fixups[j]);
	  FREE(fixups);
	  if (v) FREE(v);
	  if (was_null)
	    return(run_warn("or: can't handle %s", XEN_AS_STRING(XEN_CAR(body))));
	  else return(run_warn("or: type not boolean: %s", XEN_AS_STRING(XEN_CAR(body))));
	}
      fixups[i] = make_xen_value(R_INT, add_int_to_ptree(prog, prog->triple_ctr), R_VARIABLE);
      add_triple_to_ptree(prog, va_make_triple(jump_if, descr_jump_if, 2, fixups[i], v));
      FREE(v);
    }
  /* if we fall through, return #f */
  result = make_xen_value(R_BOOL, add_int_to_ptree(prog, 0), R_VARIABLE);
  add_triple_to_ptree(prog, va_make_triple(store_false, descr_store_false, 1, result));
  jump_to_end = make_xen_value(R_INT, add_int_to_ptree(prog, prog->triple_ctr), R_VARIABLE);
  add_triple_to_ptree(prog, va_make_triple(jump, descr_jump, 1, jump_to_end));
  /* now fixup all the jumps to end up here */
  for (i = 0; i < body_forms; i++)
    {
      prog->ints[fixups[i]->addr] = prog->triple_ctr - prog->ints[fixups[i]->addr] - 1;
      FREE(fixups[i]);
    }
  add_triple_to_ptree(prog, va_make_triple(store_true, descr_store_true, 1, result));
  prog->ints[jump_to_end->addr] = prog->triple_ctr - prog->ints[jump_to_end->addr] - 1;
  FREE(jump_to_end);
  FREE(fixups);
  return(result);
}

static xen_value *and_form(ptree *prog, XEN form, walk_result_t ignored)
{
  /* (and ...) returning as soon as #f seen -- assume booleans only here */
  XEN body;
  xen_value *v = NULL, *result = NULL, *jump_to_end;
  xen_value **fixups;
  int i, j, body_forms;
  body = XEN_CDR(form);
  body_forms = XEN_LIST_LENGTH(body);
  if (body_forms == 0)                  /* (and) -> #t */
    return(make_xen_value(R_BOOL, add_int_to_ptree(prog, 1), R_CONSTANT));
  fixups = (xen_value **)CALLOC(body_forms, sizeof(xen_value *));
  for (i = 0; i < body_forms; i++, body = XEN_CDR(body))
    {
      v = walk(prog, XEN_CAR(body), NEED_ANY_RESULT);
      if ((v == NULL) || ((v->type != R_BOOL) && (!(POINTER_OR_GOTO_P(v->type)))))
	{
	  bool was_null;
	  was_null = (v == NULL);
	  for (j = 0; j < i; j++)
	    if (fixups[j]) FREE(fixups[j]);
	  FREE(fixups);
	  if (v) FREE(v);
	  if (was_null)
	    return(run_warn("and: can't handle %s", XEN_AS_STRING(XEN_CAR(body))));
	  else return(run_warn("and: type not boolean: %s", XEN_AS_STRING(XEN_CAR(body))));
	}
      fixups[i] = make_xen_value(R_INT, add_int_to_ptree(prog, prog->triple_ctr), R_VARIABLE);
      add_triple_to_ptree(prog, va_make_triple(jump_if_not, descr_jump_if_not, 2, fixups[i], v));
      FREE(v);
    }
  /* if we fall through, return #t */
  result = make_xen_value(R_BOOL, add_int_to_ptree(prog, 1), R_VARIABLE);
  add_triple_to_ptree(prog, va_make_triple(store_true, descr_store_true, 1, result));
  jump_to_end = make_xen_value(R_INT, add_int_to_ptree(prog, prog->triple_ctr), R_VARIABLE);
  add_triple_to_ptree(prog, va_make_triple(jump, descr_jump, 1, jump_to_end));
  /* now fixup all the jumps to end up here */
  for (i = 0; i < body_forms; i++)
    {
      prog->ints[fixups[i]->addr] = prog->triple_ctr - prog->ints[fixups[i]->addr] - 1;
      FREE(fixups[i]);
    }
  add_triple_to_ptree(prog, va_make_triple(store_false, descr_store_false, 1, result));
  prog->ints[jump_to_end->addr] = prog->triple_ctr - prog->ints[jump_to_end->addr] - 1;
  FREE(jump_to_end);
  FREE(fixups);
  return(result);
}

static xen_value *lookup_generalized_set(ptree *prog, XEN accessor, xen_value *arg0, xen_value *arg1, xen_value *arg2, xen_value *new_value);

static xen_value *generalized_set_form(ptree *prog, XEN form, walk_result_t need_result)
{
  /* (set! (mus-phase gen) 0.0) */
  XEN settee, setval;
  XEN in_settee;
  xen_value *in_v0 = NULL, *in_v1 = NULL, *in_v2 = NULL, *v = NULL;
  settee = XEN_CADR(form);
  setval = XEN_CADDR(form);
  if ((XEN_LIST_P(settee)) && (XEN_SYMBOL_P(XEN_CAR(settee))) && (XEN_LIST_LENGTH(settee) <= 4))
    {
      v = walk(prog, setval, NEED_ANY_RESULT);
      if (v == NULL)
	return(run_warn("set!: can't handle: %s", XEN_AS_STRING(setval)));
      in_settee = XEN_CAR(settee);
      if (XEN_NOT_NULL_P(XEN_CDR(settee)))
	{
	  in_v0 = walk(prog, XEN_CADR(settee), NEED_ANY_RESULT);
	  if ((in_v0) &&
	      (in_v0->type != R_UNSPECIFIED) &&
	      (!(in_v0->constant)))
	    {
	      if (XEN_NOT_NULL_P(XEN_CDDR(settee)))
		{
		  in_v1 = walk(prog, XEN_CADDR(settee), NEED_ANY_RESULT);
		  if ((in_v1) &&
		      (in_v1->type != R_UNSPECIFIED))
		    {
		      if (XEN_NOT_NULL_P(XEN_CDDDR(settee)))
			{
			  in_v2 = walk(prog, XEN_CADDDR(settee), NEED_ANY_RESULT);
			  if ((in_v2) &&
			      (in_v2->type != R_UNSPECIFIED))
			    return(lookup_generalized_set(prog, in_settee, in_v0, in_v1, in_v2, v));
			}
		      else return(lookup_generalized_set(prog, in_settee, in_v0, in_v1, NULL, v));
		    }
		}
	      else return(lookup_generalized_set(prog, in_settee, in_v0, NULL, NULL, v));
	    }
	}
      else return(lookup_generalized_set(prog, in_settee, NULL, NULL, NULL, v));
      if (v) FREE(v);
      if (in_v0) FREE(in_v0);
      if (in_v1) FREE(in_v1);
      if (in_v2) FREE(in_v2);
    }
  return(run_warn("generalized set! for %s not implemented yet", XEN_AS_STRING(settee)));
}

static xen_value *set_form(ptree *prog, XEN form, walk_result_t need_result)
{
  char *varname = NULL;
  xen_var *var;
  xen_value *v;
  XEN settee, setval, rtnval;
  settee = XEN_CADR(form);
  setval = XEN_CADDR(form);
  if (!(XEN_SYMBOL_P(settee)))
    return(generalized_set_form(prog, form, need_result));
  varname = XEN_SYMBOL_TO_C_STRING(settee);
  var = find_var_in_ptree(prog, varname);
  if (var == NULL)
    {
      v = add_global_var_to_ptree(prog, settee, &rtnval);
      if (v) 
	{
	  var = find_var_in_ptree(prog, varname);
	  FREE(v);
	  v = NULL;
	}
    }
  if ((var) && (!(var->unsettable)))
    {
      int val_type, var_type;
      v = walk(prog, setval, NEED_ANY_RESULT);
      if (v == NULL) 
	return(run_warn("set!: can't handle: %s", XEN_AS_STRING(setval)));
      if ((var->v->addr == v->addr) && 
	  (var->v->type == v->type))
	return(v); /* a no-op: (set! a a) */
      val_type = v->type;
      var_type = var->v->type;

      /* two problematic cases: types differ and aren't compatible, or pointer aliasing */
      if (POINTER_P(val_type))
	{
	  FREE(v);
	  return(run_warn("can't set pointer var (%s) to alias other such var", varname));
	}
      if (val_type != var_type)
	{
	  /* here #f is ok for pointer val */
 	  if ((val_type == R_GOTO) && 
 	      (var_type == R_BOOL))
	    {
	      var->v->type = R_GOTO;
 	      add_triple_to_ptree(prog, va_make_triple(store_i, descr_store_i, 2, var->v, v));
	      return(v);
	    }
	  if ((val_type == R_BOOL) &&
	      (POINTER_P(var_type)))
	    {
 	      add_triple_to_ptree(prog, va_make_triple(store_i, descr_store_i, 2, var->v, v));
	      var->unclean = true;
	      return(v);
	    }
	  /* variables have only one type in this context */
	  run_warn("set! can't change var's type (%s -> %s): %s", 
		   type_name(var->v->type), 
		   type_name(v->type), 
		   XEN_AS_STRING(form));
	  FREE(v);
	  return(NULL);
	  /* this limitation could be removed, but is it worth the bother? */
	}
      /* optimize out redundant sets (can be cancelled via trp->no_opt) */
      if (((v->type == R_FLOAT) || (v->type == R_INT)) &&
	  (XEN_LIST_P(setval)) && /* this by itself assumes that any list represents an expression (i.e. a temp in effect)
				   *   but clm-def-struct field references are lists that can be the target of a set
				   *   so we set the no_opt flag when producing the set
				   */
	  (prog->triple_ctr > 0) &&
	  (((triple **)(prog->program))[prog->triple_ctr - 1]->no_opt == false))
	{
	  /* if possible, simply redirect the previous store to us (implicit set) */
	  /* (run '(let ((a 2) (b 1)) (set! a (+ b 1)))) */
	  int *addrs;
	  addrs = ((triple **)(prog->program))[prog->triple_ctr - 1]->args;
	  if ((addrs) && 
	      (addrs[0] == v->addr))
	    {
	      addrs[0] = var->v->addr; /* redirect the store to us */
	      var->unclean = true;
	      FREE(v);
	      return(copy_xen_value(var->v));
	    }
	}
      set_var(prog, var->v, v);
      var->unclean = true;
      return(v);
    }
  if ((var) && (var->unsettable))
    return(run_warn("set!: can't set local or list vars: %s", XEN_AS_STRING(settee)));
  return(run_warn("set! variable problem: %s", XEN_AS_STRING(form)));
}

static xen_value *package(ptree *prog,
			  int type, 
			  void (*function)(int *arg_addrs, ptree *pt),
			  char *(*descr)(int *arg_addrs, ptree *pt),
			  xen_value **args,
			  int num_args)
{
  args[0] = add_empty_var_to_ptree(prog, type);
  add_triple_to_ptree(prog, make_triple(function, descr, args, num_args + 1));
  return(args[0]);
}

static void dbl_check_1(int *args, ptree *pt)
{
#if HAVE_DECL_ISNAN
  if (isnan(FLOAT_ARG_1)) mus_error(MUS_ARG_OUT_OF_RANGE, "float arg 1 is NaN");
#endif
}
static char *descr_dbl_check_1(int *args, ptree *pt)
{
  return(mus_format("if (isnan(" FLT_PT ") mus-error", args[1], FLOAT_ARG_1));
}

static void dbl_check_2(int *args, ptree *pt)
{
#if HAVE_DECL_ISNAN
  if (isnan(FLOAT_ARG_2)) mus_error(MUS_ARG_OUT_OF_RANGE, "float arg 2 is NaN");
#endif
}
static char *descr_dbl_check_2(int *args, ptree *pt)
{
  return(mus_format("if (isnan(" FLT_PT ") mus-error", args[2], FLOAT_ARG_2));
}

static void dbl_check_3(int *args, ptree *pt)
{
#if HAVE_DECL_ISNAN
  if (isnan(FLOAT_ARG_3)) mus_error(MUS_ARG_OUT_OF_RANGE, "float arg 3 is NaN");
#endif
}
static char *descr_dbl_check_3(int *args, ptree *pt)
{
  return(mus_format("if (isnan(" FLT_PT ") mus-error", args[3], FLOAT_ARG_3));
}

static xen_value *safe_package(ptree *prog,
			       int type, 
			       void (*function)(int *arg_addrs, ptree *pt),
			       char *(*descr)(int *arg_addrs, ptree *pt),
			       xen_value **args,
			       int num_args)
{
  /* package with added float arg checks for NaN; run_safety already checked */
  xen_value *result;
  result = package(prog, type, function, descr, args, num_args);
#if HAVE_DECL_ISNAN
  if (num_args >= 1)
    {
      if (args[1]->type == R_FLOAT) package(prog, R_BOOL, dbl_check_1, descr_dbl_check_1, args, num_args);
      if (num_args >= 2)
	{
	  if (args[2]->type == R_FLOAT) package(prog, R_BOOL, dbl_check_2, descr_dbl_check_2, args, num_args);
	  if (num_args >= 3)
	    {
	      if (args[3]->type == R_FLOAT) package(prog, R_BOOL, dbl_check_3, descr_dbl_check_3, args, num_args);
	    }
	}
    }
#endif
  return(result);
}

static xen_value *package_n(ptree *prog,
			    int type, 
			    void (*function)(int *arg_addrs, ptree *pt),
			    char *(*descr)(int *arg_addrs, ptree *pt),
			    xen_value **args,
			    int num_args)
{
  int i;
  xen_value **new_args;
  new_args = (xen_value **)CALLOC(num_args + 2, sizeof(xen_value *));
  for (i = 1; i <= num_args; i++)
    new_args[i + 1] = args[i];
  new_args[1] = make_xen_value(R_INT, add_int_to_ptree(prog, num_args), R_CONSTANT);
  new_args[0] = add_empty_var_to_ptree(prog, type);
  args[0] = new_args[0];
  add_triple_to_ptree(prog, make_triple(function, descr, new_args, num_args + 2));
  FREE(new_args[1]);
  FREE(new_args);
  return(args[0]);
}

static char *describe_dbl_args(const char *func, int num_args, int *args, Double *dbls, int start)
{
  char *buf, *str;
  int i, len;
  len = 256;
  buf = (char *)CALLOC(len, sizeof(char));
  str = (char *)CALLOC(32, sizeof(char));
  sprintf(buf, FLT_PT " =", args[0], dbls[args[0]]);
  for (i = start; i < num_args; i++)
    {
      mus_snprintf(str, 32, " " FLT_PT " %s", args[i], dbls[args[i]], func);
      buf = snd_strcat(buf, str, &len);
    }
  mus_snprintf(str, 32, " " FLT_PT ")", args[num_args], dbls[args[num_args]]);
  buf = snd_strcat(buf, str, &len);
  FREE(str);
  return(buf);
}

static char *describe_int_args(const char *func, int num_args, int *args, Int *ints, int start)
{
  char *buf, *str;
  int i, len;
  len = 256;
  buf = (char *)CALLOC(len, sizeof(char));
  str = (char *)CALLOC(32, sizeof(char));
  sprintf(buf, INT_PT " =", args[0], ints[args[0]]);
  for (i = start; i < num_args; i++)
    {
      mus_snprintf(str, 32, " " INT_PT " %s", args[i], ints[args[i]], func);
      buf = snd_strcat(buf, str, &len);
    }
  mus_snprintf(str, 32, " " INT_PT ")", args[num_args], ints[args[num_args]]);
  buf = snd_strcat(buf, str, &len);
  FREE(str);
  return(buf);
}

static int float_all_args(ptree *prog, int num_args, xen_value **args, bool float_result)
{
  int i, j;
  xen_value *old_loc;
  for (i = 1, j = 1; i <= num_args; i++)
    if (args[i])
      {
	if ((float_result) && (args[i]->type == R_INT))
	  {
	    old_loc = args[i];
	    args[i] = NULL;
	    args[j] = make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, 0.0), R_VARIABLE);
	    add_triple_to_ptree(prog, va_make_triple(store_i_f, descr_store_i_f, 2, args[j], old_loc));
	    FREE(old_loc);
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
static char *descr_multiply_f2(int *args, ptree *pt) {return(describe_dbl_args("*", 2, args, pt->dbls, 1));}

static void multiply_f3(int *args, ptree *pt) {FLOAT_RESULT = (FLOAT_ARG_1 * FLOAT_ARG_2 * FLOAT_ARG_3);}
static char *descr_multiply_f3(int *args, ptree *pt) {return(describe_dbl_args("*", 3, args, pt->dbls, 1));}

static void multiply_f4(int *args, ptree *pt) {FLOAT_RESULT = (FLOAT_ARG_1 * FLOAT_ARG_2 * FLOAT_ARG_3 * FLOAT_ARG_4);}
static char *descr_multiply_f4(int *args, ptree *pt) {return(describe_dbl_args("*", 4, args, pt->dbls, 1));}

static void multiply_fn(int *args, ptree *pt) 
{
  int i, n;
  n = pt->ints[args[1]];
  FLOAT_RESULT = FLOAT_ARG_2;
  for (i = 1; i < n; i++) FLOAT_RESULT *= pt->dbls[args[i + 2]];
}
static char *descr_multiply_fn(int *args, ptree *pt) {return(describe_dbl_args("*", pt->ints[args[1]] + 1, args, pt->dbls, 2));}

static void multiply_i2(int *args, ptree *pt) {INT_RESULT = (INT_ARG_1 * INT_ARG_2);}
static char *descr_multiply_i2(int *args, ptree *pt) {return(describe_int_args("*", 2, args, pt->ints, 1));}

static void multiply_i3(int *args, ptree *pt) {INT_RESULT = (INT_ARG_1 * INT_ARG_2 * INT_ARG_3);}
static char *descr_multiply_i3(int *args, ptree *pt) {return(describe_int_args("*", 3, args, pt->ints, 1));}

static void multiply_i4(int *args, ptree *pt) {INT_RESULT = (INT_ARG_1 * INT_ARG_2 * INT_ARG_3 * INT_ARG_4);}
static char *descr_multiply_i4(int *args, ptree *pt) {return(describe_int_args("*", 4, args, pt->ints, 1));}

static void multiply_in(int *args, ptree *pt)
{
  int i, n;
  n = pt->ints[args[1]];
  INT_RESULT = pt->ints[args[2]];
  for (i = 1; i < n; i++) INT_RESULT *= pt->ints[args[i + 2]];
}

static char *descr_multiply_in(int *args, ptree *pt) {return(describe_int_args("*", pt->ints[args[1]] + 1, args, pt->ints, 2));}

static xen_value *multiply(ptree *prog, xen_value **args, int num_args)
{
  Int iscl = 1;
  Double fscl = 1.0;
  int i, cons_loc = 0;
  if (num_args == 0) return(make_xen_value(R_INT, add_int_to_ptree(prog, 1), R_CONSTANT));
  if (num_args == 1) return(copy_xen_value(args[1]));
  if (prog->constants > 0)
    {
      for (i = 1; i <= num_args; i++)
	if (args[i]->constant == R_CONSTANT)
	  {
	    cons_loc = i;
	    if (args[i]->type == R_INT)
	      iscl *= prog->ints[args[i]->addr];
	    else fscl *= prog->dbls[args[i]->addr];
	    FREE(args[i]);
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
  num_args = float_all_args(prog, num_args, args, prog->float_result);
  if (num_args == 1) return(copy_xen_value(args[1]));
  if (prog->float_result)
    {
      if (num_args == 2) return(package(prog, R_FLOAT, multiply_f2, descr_multiply_f2, args, num_args));
      if (num_args == 3) return(package(prog, R_FLOAT, multiply_f3, descr_multiply_f3, args, num_args));
      if (num_args == 4) return(package(prog, R_FLOAT, multiply_f4, descr_multiply_f4, args, num_args));
      return(package_n(prog, R_FLOAT, multiply_fn, descr_multiply_fn, args, num_args));
    }
  if (num_args == 2) return(package(prog, R_INT, multiply_i2, descr_multiply_i2, args, num_args)); 
  if (num_args == 3) return(package(prog, R_INT, multiply_i3, descr_multiply_i3, args, num_args));
  if (num_args == 4) return(package(prog, R_INT, multiply_i4, descr_multiply_i4, args, num_args));
  return(package_n(prog, R_INT, multiply_in, descr_multiply_in, args, num_args));
}


/* ---------------- add ---------------- */

static void add_f2(int *args, ptree *pt) {FLOAT_RESULT = (FLOAT_ARG_1 + FLOAT_ARG_2);}
static char *descr_add_f2(int *args, ptree *pt) {return(describe_dbl_args("+", 2, args, pt->dbls, 1));}

static void add_f3(int *args, ptree *pt) {FLOAT_RESULT = (FLOAT_ARG_1 + FLOAT_ARG_2 + FLOAT_ARG_3);}
static char *descr_add_f3(int *args, ptree *pt) {return(describe_dbl_args("+", 3, args, pt->dbls, 1));}

static void add_f4(int *args, ptree *pt) {FLOAT_RESULT = (FLOAT_ARG_1 + FLOAT_ARG_2 + FLOAT_ARG_3 + FLOAT_ARG_4);}
static char *descr_add_f4(int *args, ptree *pt) {return(describe_dbl_args("+", 4, args, pt->dbls, 1));}

static void add_fn(int *args, ptree *pt) 
{
  int i, n;
  n = pt->ints[args[1]];
  FLOAT_RESULT = FLOAT_ARG_2;
  for (i = 1; i < n; i++) FLOAT_RESULT += pt->dbls[args[i + 2]];
}
static char *descr_add_fn(int *args, ptree *pt) {return(describe_dbl_args("+", pt->ints[args[1]] + 1, args, pt->dbls, 2));}

static void add_i2(int *args, ptree *pt) {INT_RESULT = (INT_ARG_1 + INT_ARG_2);}
static char *descr_add_i2(int *args, ptree *pt) {return(describe_int_args("+", 2, args, pt->ints, 1));}

static void add_i3(int *args, ptree *pt) {INT_RESULT = (INT_ARG_1 + INT_ARG_2 + INT_ARG_3);}
static char *descr_add_i3(int *args, ptree *pt) {return(describe_int_args("+", 3, args, pt->ints, 1));}

static void add_i4(int *args, ptree *pt) {INT_RESULT = (INT_ARG_1 + INT_ARG_2 + INT_ARG_3 + INT_ARG_4);}
static char *descr_add_i4(int *args, ptree *pt) {return(describe_int_args("+", 4, args, pt->ints, 1));}

static void add_in(int *args, ptree *pt)
{
  int i, n;
  n = pt->ints[args[1]];
  INT_RESULT = pt->ints[args[2]];
  for (i = 1; i < n; i++) INT_RESULT += pt->ints[args[i + 2]];
}

static char *descr_add_in(int *args, ptree *pt) {return(describe_int_args("+", pt->ints[args[1]] + 1, args, pt->ints, 2));}

static xen_value *add(ptree *prog, xen_value **args, int num_args)
{
  Int iscl = 0;
  Double fscl = 0.0;
  int i, cons_loc = 0;
  if (num_args == 0) return(make_xen_value(R_INT, add_int_to_ptree(prog, 0), R_CONSTANT));
  if (num_args == 1) return(copy_xen_value(args[1]));
  if (prog->constants > 0)
    {
      for (i = 1; i <= num_args; i++)
	if (args[i]->constant == R_CONSTANT)
	  {
	    cons_loc = i;
	    if (args[i]->type == R_INT)
	      iscl += prog->ints[args[i]->addr];
	    else fscl += prog->dbls[args[i]->addr];
	    FREE(args[i]);
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
  num_args = float_all_args(prog, num_args, args, prog->float_result);
  if (num_args == 1) return(copy_xen_value(args[1]));
  if (prog->float_result)
    {
      if (num_args == 2) return(package(prog, R_FLOAT, add_f2, descr_add_f2, args, num_args));
      if (num_args == 3) return(package(prog, R_FLOAT, add_f3, descr_add_f3, args, num_args));
      if (num_args == 4) return(package(prog, R_FLOAT, add_f4, descr_add_f4, args, num_args));
      return(package_n(prog, R_FLOAT, add_fn, descr_add_fn, args, num_args));
    }
  if (num_args == 2) return(package(prog, R_INT, add_i2, descr_add_i2, args, num_args));
  if (num_args == 3) return(package(prog, R_INT, add_i3, descr_add_i3, args, num_args));
  if (num_args == 4) return(package(prog, R_INT, add_i4, descr_add_i4, args, num_args));
  return(package_n(prog, R_INT, add_in, descr_add_in, args, num_args));
}

/* ---------------- subtract ---------------- */

static void subtract_f1(int *args, ptree *pt) {FLOAT_RESULT = -(FLOAT_ARG_1);}
static char *descr_subtract_f1(int *args, ptree *pt) {return(mus_format( FLT_PT " = -" FLT_PT , args[0], FLOAT_RESULT, args[1], FLOAT_ARG_1));}

static void subtract_f2(int *args, ptree *pt) {FLOAT_RESULT = (FLOAT_ARG_1 - FLOAT_ARG_2);}
static char *descr_subtract_f2(int *args, ptree *pt) {return(describe_dbl_args("-", 2, args, pt->dbls, 1));}

static void subtract_f3(int *args, ptree *pt) {FLOAT_RESULT = (FLOAT_ARG_1 - FLOAT_ARG_2 - FLOAT_ARG_3);}
static char *descr_subtract_f3(int *args, ptree *pt) {return(describe_dbl_args("-", 3, args, pt->dbls, 1));}

static void subtract_fn(int *args, ptree *pt) 
{
  int i, n;
  n = pt->ints[args[1]];
  FLOAT_RESULT = FLOAT_ARG_2;
  for (i = 1; i < n; i++) FLOAT_RESULT -= pt->dbls[args[i + 2]];
}
static char *descr_subtract_fn(int *args, ptree *pt) {return(describe_dbl_args("-", pt->ints[args[1]] + 1, args, pt->dbls, 2));}

static void subtract_i1(int *args, ptree *pt) {INT_RESULT = -(INT_ARG_1);}
static char *descr_subtract_i1(int *args, ptree *pt) {return(mus_format( INT_PT " = -" INT_PT , args[0], INT_RESULT, args[1], INT_ARG_1));}

static void subtract_i2(int *args, ptree *pt) {INT_RESULT = (INT_ARG_1 - INT_ARG_2);}
static char *descr_subtract_i2(int *args, ptree *pt) {return(describe_int_args("-", 2, args, pt->ints, 1));}

static void subtract_i3(int *args, ptree *pt) {INT_RESULT = (INT_ARG_1 - INT_ARG_2 - INT_ARG_3);}
static char *descr_subtract_i3(int *args, ptree *pt) {return(describe_int_args("-", 3, args, pt->ints, 1));}

static void subtract_in(int *args, ptree *pt)
{
  int i, n;
  n = pt->ints[args[1]];
  INT_RESULT = pt->ints[args[2]];
  for (i = 1; i < n; i++) INT_RESULT -= pt->ints[args[i + 2]];
}

static char *descr_subtract_in(int *args, ptree *pt) {return(describe_int_args("-", pt->ints[args[1]] + 1, args, pt->ints, 2));}

static xen_value *subtract(ptree *prog, xen_value **args, int num_args)
{
  Int iscl = 0;
  Double fscl = 0.0;
  int i, cons_loc = 0;
  if ((num_args == 1) && (args[1]->constant == R_CONSTANT))
    {
      if (args[1]->type == R_INT)
	return(make_xen_value(R_INT, add_int_to_ptree(prog, -(prog->ints[args[1]->addr])), R_CONSTANT));
      else return(make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, -(prog->dbls[args[1]->addr])), R_CONSTANT));
    }
  if (prog->constants > 0)
    {
      for (i = 2; i <= num_args; i++)
	if (args[i]->constant == R_CONSTANT)
	  {
	    cons_loc = i;
	    if (args[i]->type == R_INT)
	      iscl += prog->ints[args[i]->addr];
	    else fscl += prog->dbls[args[i]->addr];
	    FREE(args[i]);
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
	  if ((num_args == 2) && (args[1]->constant == R_VARIABLE))
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
  num_args = float_all_args(prog, num_args, args, prog->float_result);
  if (prog->float_result)
    {
      if (num_args == 1) return(package(prog, R_FLOAT, subtract_f1, descr_subtract_f1, args, num_args));
      if (num_args == 2) return(package(prog, R_FLOAT, subtract_f2, descr_subtract_f2, args, num_args));
      if (num_args == 3) return(package(prog, R_FLOAT, subtract_f3, descr_subtract_f3, args, num_args));
      return(package_n(prog, R_FLOAT, subtract_fn, descr_subtract_fn, args, num_args));
    }
  if (num_args == 1) return(package(prog, R_INT, subtract_i1, descr_subtract_i1, args, num_args));
  if (num_args == 2) return(package(prog, R_INT, subtract_i2, descr_subtract_i2, args, num_args));
  if (num_args == 3) return(package(prog, R_INT, subtract_i3, descr_subtract_i3, args, num_args));
  return(package_n(prog, R_INT, subtract_in, descr_subtract_in, args, num_args));
}


/* ---------------- 1+ 1- ---------------- */

static void one_minus_f(int *args, ptree *pt) {FLOAT_RESULT = FLOAT_ARG_1 - 1.0;}
static char *descr_one_minus_f(int *args, ptree *pt) {return(mus_format( FLT_PT " = " FLT_PT " - 1.0", args[0], FLOAT_RESULT, args[1], FLOAT_ARG_1));}
static void one_minus_i(int *args, ptree *pt) {INT_RESULT = INT_ARG_1 - 1;}
static char *descr_one_minus_i(int *args, ptree *pt) {return(mus_format( INT_PT " = " INT_PT " - 1", args[0], INT_RESULT, args[1], INT_ARG_1));}
static xen_value *one_minus(ptree *prog, xen_value **args, int num_args)
{
  if (prog->constants == 1)
    {
      if (args[1]->type == R_INT)
	return(make_xen_value(R_INT, add_int_to_ptree(prog, prog->ints[args[1]->addr] - 1), R_CONSTANT));
      else return(make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, prog->dbls[args[1]->addr] - 1.0), R_CONSTANT));
    }
  if (args[1]->type == R_INT)
    return(package(prog, R_INT, one_minus_i, descr_one_minus_i, args, 1));
  else return(package(prog, R_FLOAT, one_minus_f, descr_one_minus_f, args, 1));
}

static void one_plus_f(int *args, ptree *pt) {FLOAT_RESULT = FLOAT_ARG_1 + 1.0;}
static char *descr_one_plus_f(int *args, ptree *pt) {return(mus_format( FLT_PT " = " FLT_PT " + 1.0", args[0], FLOAT_RESULT, args[1], FLOAT_ARG_1));}
static void one_plus_i(int *args, ptree *pt) {INT_RESULT = INT_ARG_1 + 1;}
static char *descr_one_plus_i(int *args, ptree *pt) {return(mus_format( INT_PT " = " INT_PT " + 1", args[0], INT_RESULT, args[1], INT_ARG_1));}
static xen_value *one_plus(ptree *prog, xen_value **args, int num_args)
{
  if (prog->constants == 1)
    {
      if (args[1]->type == R_INT)
	return(make_xen_value(R_INT, add_int_to_ptree(prog, prog->ints[args[1]->addr] + 1), R_CONSTANT));
      else return(make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, prog->dbls[args[1]->addr] + 1.0), R_CONSTANT));
    }
  if (args[1]->type == R_INT)
    return(package(prog, R_INT, one_plus_i, descr_one_plus_i, args, 1));
  else return(package(prog, R_FLOAT, one_plus_f, descr_one_plus_f, args, 1));
}


/* ---------------- divide ---------------- */

static void divide_f1(int *args, ptree *pt) {FLOAT_RESULT = (1.0 / FLOAT_ARG_1);}
static char *descr_divide_f1(int *args, ptree *pt) 
{
  return(mus_format( FLT_PT " = 1.0 / " FLT_PT , args[0], FLOAT_RESULT, args[1], FLOAT_ARG_1));
}

static void divide_if1(int *args, ptree *pt) {INT_RESULT = (Int)(1.0 / FLOAT_ARG_1);}
static char *descr_divide_if1(int *args, ptree *pt) 
{
  return(mus_format( INT_PT " = 1.0 / " FLT_PT , args[0], INT_RESULT, args[1], FLOAT_ARG_1));
}

static void divide_f2(int *args, ptree *pt) {FLOAT_RESULT = (FLOAT_ARG_1 / FLOAT_ARG_2);}
static char *descr_divide_f2(int *args, ptree *pt) {return(describe_dbl_args("/", 2, args, pt->dbls, 1));}

static void divide_if2(int *args, ptree *pt) {INT_RESULT = (Int)(FLOAT_ARG_1 / FLOAT_ARG_2);}
static char *descr_divide_if2(int *args, ptree *pt)
{
  return(mus_format( INT_PT " = " FLT_PT " / " FLT_PT , args[0], INT_RESULT, args[1], FLOAT_ARG_1, args[2], FLOAT_ARG_2));
}
	
static void divide_f3(int *args, ptree *pt) {FLOAT_RESULT = (FLOAT_ARG_1 / (FLOAT_ARG_2 * FLOAT_ARG_3));}
static char *descr_divide_f3(int *args, ptree *pt) 
{
  return(mus_format( FLT_PT " = " FLT_PT " / (" FLT_PT " * " FLT_PT ")", 
		     args[0], FLOAT_RESULT, args[1], FLOAT_ARG_1, args[2], FLOAT_ARG_2, args[3], FLOAT_ARG_3));
}

static void divide_fn(int *args, ptree *pt) 
{
  int i, n;
  Double divisor = 1.0;
  n = pt->ints[args[1]];
  for (i = 1; i < n; i++) divisor *= pt->dbls[args[i + 2]];
  FLOAT_RESULT = FLOAT_ARG_2 / divisor;
}
static char *descr_divide_fn(int *args, ptree *pt)
{
  char *buf, *str;
  int i, num_args, len;
  num_args = pt->ints[args[1]] + 1;
  len = 256;
  buf = (char *)CALLOC(len, sizeof(char));
  str = (char *)CALLOC(32, sizeof(char));
  sprintf(buf, FLT_PT " = " FLT_PT " / (", args[0], FLOAT_RESULT, args[2], FLOAT_ARG_2);
  for (i = 3; i < num_args; i++)
    {
      mus_snprintf(str, 32, " " FLT_PT " *", args[i], pt->dbls[args[i]]);
      buf = snd_strcat(buf, str, &len);
    }
  mus_snprintf(str, 32, " " FLT_PT ")", args[num_args], pt->dbls[args[num_args]]);
  buf = snd_strcat(buf, str, &len);
  FREE(str);
  return(buf);
}

static xen_value *divide(ptree *prog, xen_value **args, int num_args)
{
  int cons_loc = 0;
  Double fscl = 1.0;
  int i;
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
      for (i = 2; i <= num_args; i++)
	if (args[i]->constant == R_CONSTANT)
	  {
	    cons_loc = i;
	    if (args[i]->type == R_INT)
	      fscl *= (Double)(prog->ints[args[i]->addr]);
	    else fscl *= prog->dbls[args[i]->addr];
	    FREE(args[i]);
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
	      if (args[cons_loc]) FREE(args[cons_loc]);
	      args[2] = make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, (Double)(1.0 / fscl)), R_CONSTANT);
	      if (args[1]->type == R_INT) float_all_args(prog, num_args, args, true);
	      return(package(prog, R_FLOAT, multiply_f2, descr_multiply_f2, args, 2));
	    }
	}
    }
  num_args = float_all_args(prog, num_args, args, true);
  if (num_args == 1) 
    {
      if (prog->walk_result == NEED_INT_RESULT)
	return(package(prog, R_INT, divide_if1, descr_divide_if1, args, num_args));
      else return(package(prog, R_FLOAT, divide_f1, descr_divide_f1, args, num_args));
    }
  if (num_args == 2) 
    {
      if (prog->walk_result == NEED_INT_RESULT)
	return(package(prog, R_INT, divide_if2, descr_divide_if2, args, num_args));
      else return(package(prog, R_FLOAT, divide_f2, descr_divide_f2, args, num_args));
    }
  if (num_args == 3) return(package(prog, R_FLOAT, divide_f3, descr_divide_f3, args, num_args));
  return(package_n(prog, R_FLOAT, divide_fn, descr_divide_fn, args, num_args));
}


static char *describe_rel_f_args(const char *func, int num_args, int *args, ptree *pt, int start)
{
  char *buf, *str;
  int i, len;
  len = 256;
  buf = (char *)CALLOC(len, sizeof(char));
  str = (char *)CALLOC(32, sizeof(char));
  sprintf(buf, BOOL_PT " =", args[0], B2S(BOOL_RESULT));
  for (i = start; i < num_args; i++)
    {
      mus_snprintf(str, 32, " " FLT_PT " %s", args[i], pt->dbls[args[i]], func);
      buf = snd_strcat(buf, str, &len);
    }
  mus_snprintf(str, 32, " " FLT_PT ")", args[num_args], pt->dbls[args[num_args]]);
  buf = snd_strcat(buf, str, &len);
  FREE(str);
  return(buf);
}

static char *describe_rel_i_args(const char *func, int num_args, int *args, Int *ints, int start)
{
  return(describe_int_args(func, num_args, args, ints, start));
}

static void float_rel_constant_args(ptree *prog, int num_args, xen_value **args)
{
  int i;
  xen_value *old_loc;
  for (i = 1; i <= num_args; i++)
    if ((args[i]->constant == R_CONSTANT) && (args[i]->type == R_INT))
      {
	old_loc = args[i];
	args[i] = make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, (Double)(prog->ints[args[i]->addr])), R_CONSTANT);
	FREE(old_loc);
      }
}

static void float_rel_args(ptree *prog, int num_args, xen_value **args)
{
  int i;
  xen_value *old_loc;
  for (i = 1; i <= num_args; i++)
    if (args[i]->type == R_INT)
      {
	old_loc = args[i];
	args[i] = make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, 0.0), R_VARIABLE);
	add_triple_to_ptree(prog, va_make_triple(store_i_f, descr_store_i_f, 2, args[i], old_loc));
	FREE(old_loc);
      }
}


/* ---------------- rel ops ---------------- */

#define REL_OP(CName, SName, COp, FOp) \
static void CName ## _f2(int *args, ptree *pt) {BOOL_RESULT = (Int)(FLOAT_ARG_1 COp FLOAT_ARG_2);} \
static char *descr_ ## CName ## _f2(int *args, ptree *pt) {return(describe_rel_f_args(#COp, 2, args, pt, 1));} \
static void CName ## _fn(int *args, ptree *pt) \
{ \
  int i, n; \
  n = pt->ints[args[1]]; \
  for (i = 2; i <= n; i++) \
    { \
      BOOL_RESULT = (pt->dbls[args[i]] COp pt->dbls[args[i + 1]]); \
      if (!BOOL_RESULT) break; \
    } \
} \
static char *descr_ ## CName ## _fn(int *args, ptree *pt) {return(describe_rel_f_args(#COp, pt->ints[args[1]] + 1, args, pt, 2));} \
static void CName ## _i2(int *args, ptree *pt) {BOOL_RESULT = (Int)(INT_ARG_1 COp INT_ARG_2);} \
static char *descr_ ## CName ## _i2(int *args, ptree *pt) {return(describe_rel_i_args(#COp, 2, args, pt->ints, 1));} \
static void CName ## _in(int *args, ptree *pt) \
{ \
  int i, n; \
  n = pt->ints[args[1]]; \
  for (i = 2; i <= n; i++) \
    { \
      BOOL_RESULT = (Int)(pt->ints[args[i]] COp pt->ints[args[i + 1]]); \
      if (!BOOL_RESULT) break; \
    } \
} \
static char *descr_ ## CName ## _in(int *args, ptree *pt) {return(describe_rel_i_args(#COp, pt->ints[args[1]] + 1, args, pt->ints, 2));} \
static xen_value * SName(ptree *prog, bool float_result, xen_value **args, int num_args) \
{ \
  int i; \
  Int lasti = 0; \
  Double lastf = 0.0; \
  if (num_args <= 1) return(make_xen_value(R_BOOL, add_int_to_ptree(prog, (Int)true), R_CONSTANT)); \
  if ((prog->constants > 0) && (float_result)) float_rel_constant_args(prog, num_args, args); \
  if (prog->constants > 1) \
    { \
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
      if (num_args == 2) return(package(prog, R_BOOL, CName ## _f2, descr_ ## CName ## _f2, args, num_args)); \
      return(package_n(prog, R_BOOL, CName ## _fn, descr_ ## CName ## _fn, args, num_args)); \
    } \
  else \
    { \
      if (num_args == 2) return(package(prog, R_BOOL, CName ## _i2, descr_ ## CName ## _i2, args, num_args)); \
      return(package_n(prog, R_BOOL, CName ## _in, descr_ ## CName ## _in, args, num_args)); \
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
static char *descr_max_f2(int *args, ptree *pt)
{
  return(mus_format( FLT_PT " = max(" FLT_PT ", " FLT_PT ")", args[0], FLOAT_RESULT, args[1], FLOAT_ARG_1, args[2], FLOAT_ARG_2));
}
static void max_fn(int *args, ptree *pt)
{
  int i, n;
  Double mx;
  n = pt->ints[args[1]];
  mx = FLOAT_ARG_2;
  for (i = 2; i <= n; i++)
    if (pt->dbls[args[i + 1]] > mx) mx = pt->dbls[args[i + 1]];
  FLOAT_RESULT = mx;
}
static char *descr_max_min_fn(int *args, ptree *pt, const char *which) 
{
  char *buf, *str;
  int i, n, len;
  len = 256;
  buf = (char *)CALLOC(len, sizeof(char));
  str = (char *)CALLOC(32, sizeof(char));
  n = pt->ints[args[1]];
  sprintf(buf, INT_PT " = %s(", args[0], INT_RESULT, which);
  for (i = 2; i <= n; i++)
    {
      mus_snprintf(str, 32, FLT_PT " ", args[i], pt->dbls[args[i]]);
      buf = snd_strcat(buf, str, &len);
    }
  mus_snprintf(str, 32, FLT_PT ")", args[n + 1], pt->dbls[args[n + 1]]);
  buf = snd_strcat(buf, str, &len);
  FREE(str);
  return(buf);
}
static char *descr_max_fn(int *args, ptree *pt) {return(descr_max_min_fn(args, pt, "max"));}
static void max_i2(int *args, ptree *pt) {INT_RESULT = (INT_ARG_1 > INT_ARG_2) ? INT_ARG_1 : INT_ARG_2;}
static char *descr_max_i2(int *args, ptree *pt)
{
  return(mus_format( INT_PT " = max(" INT_PT ", " INT_PT ")", args[0], INT_RESULT, args[1], INT_ARG_1, args[2], INT_ARG_2));
}
static void max_in(int *args, ptree *pt)
{
  int i, n;
  Int mx;
  n = pt->ints[args[1]];
  mx = pt->ints[args[2]];
  for (i = 2; i <= n; i++)
    if (pt->ints[args[i + 1]] > mx) mx = pt->ints[args[i + 1]];
  INT_RESULT = mx;
}
static char *descr_max_min_in(int *args, ptree *pt, const char *which)
{
  char *buf, *str;
  int i, n, len;
  len = 256;
  buf = (char *)CALLOC(len, sizeof(char));
  str = (char *)CALLOC(32, sizeof(char));
  n = pt->ints[args[1]];
  sprintf(buf, INT_PT " = %s(", args[0], INT_RESULT, which);
  for (i = 2; i <= n; i++)
    {
      mus_snprintf(str, 32, INT_PT " ", args[i], pt->ints[args[i]]);
      buf = snd_strcat(buf, str, &len);
    }
  mus_snprintf(str, 32, INT_PT ")", args[n + 1], pt->ints[args[n + 1]]);
  buf = snd_strcat(buf, str, &len);
  FREE(str);
  return(buf);
}
static char *descr_max_in(int *args, ptree *pt) {return(descr_max_min_in(args, pt, "max"));}
static xen_value *max_1(ptree *prog, xen_value **args, int num_args)
{
  int i;
  Double fmx;
  Int imx;
  if (num_args == 1) return(copy_xen_value(args[1]));
  if ((prog->constants > 0) && (prog->float_result)) float_rel_constant_args(prog, num_args, args);
  if (prog->constants == num_args)
    {
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
      if (num_args == 2) return(package(prog, R_FLOAT, max_f2, descr_max_f2, args, num_args));
      return(package_n(prog, R_FLOAT, max_fn, descr_max_fn, args, num_args));
    }
  if (num_args == 2) return(package(prog, R_INT, max_i2, descr_max_i2, args, num_args));
  return(package_n(prog, R_INT, max_in, descr_max_in, args, num_args));
}

/* ---------------- min ---------------- */

static void min_f2(int *args, ptree *pt) {FLOAT_RESULT = (FLOAT_ARG_1 > FLOAT_ARG_2) ? FLOAT_ARG_2 : FLOAT_ARG_1;}
static char *descr_min_f2(int *args, ptree *pt)
{
  return(mus_format( FLT_PT " = min(" FLT_PT ", " FLT_PT ")", args[0], FLOAT_RESULT, args[1], FLOAT_ARG_1, args[2], FLOAT_ARG_2));
}
static void min_fn(int *args, ptree *pt)
{
  int i, n;
  Double mx;
  n = pt->ints[args[1]];
  mx = FLOAT_ARG_2;
  for (i = 2; i <= n; i++)
    if (pt->dbls[args[i + 1]] < mx) mx = pt->dbls[args[i + 1]];
  FLOAT_RESULT = mx;
}
static char *descr_min_fn(int *args, ptree *pt) {return(descr_max_min_fn(args, pt, "min"));}
static void min_i2(int *args, ptree *pt) {INT_RESULT = (INT_ARG_1 > INT_ARG_2) ? INT_ARG_2 : INT_ARG_1;}
static char *descr_min_i2(int *args, ptree *pt)
{
  return(mus_format( INT_PT " = min(" INT_PT ", " INT_PT ")", args[0], INT_RESULT, args[1], INT_ARG_1, args[2], INT_ARG_2));
}
static void min_in(int *args, ptree *pt)
{
  int i, n;
  Int mx;
  n = pt->ints[args[1]];
  mx = pt->ints[args[2]];
  for (i = 2; i <= n; i++)
    if (pt->ints[args[i + 1]] < mx) mx = pt->ints[args[i + 1]];
  INT_RESULT = mx;
}
static char *descr_min_in(int *args, ptree *pt) {return(descr_max_min_in(args, pt, "min"));}
static xen_value *min_1(ptree *prog, xen_value **args, int num_args)
{
  int i;
  Double fmx;
  Int imx;
  if (num_args == 1) return(copy_xen_value(args[1]));
  if ((prog->constants > 0) && (prog->float_result)) float_rel_constant_args(prog, num_args, args);
  if (prog->constants == num_args)
    {
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
      if (num_args == 2) return(package(prog, R_FLOAT, min_f2, descr_min_f2, args, num_args));
      return(package_n(prog, R_FLOAT, min_fn, descr_min_fn, args, num_args));
    }
  if (num_args == 2) return(package(prog, R_INT, min_i2, descr_min_i2, args, num_args));
  return(package_n(prog, R_INT, min_in, descr_min_in, args, num_args));
}


/* ---------------- not ---------------- */

static void not_b(int *args, ptree *pt) {BOOL_RESULT = (Int)(!(INT_ARG_1));}
static char *descr_not_b(int *args, ptree *pt) 
{
  return(mus_format( BOOL_PT " = (!(" INT_PT "))", args[0], B2S(BOOL_RESULT), args[1], INT_ARG_1));
}
static xen_value *not_p(ptree *prog, xen_value **args, int num_args)
{
  if (args[1]->type != R_BOOL)
    return(make_xen_value(R_BOOL, add_int_to_ptree(prog, (Int)false), R_CONSTANT)); /* only #f is false so (not anything)->false */
  if (prog->constants == 1)
    return(make_xen_value(R_BOOL, add_int_to_ptree(prog, !(prog->ints[args[1]->addr])), R_CONSTANT));
  return(package(prog, R_BOOL, not_b, descr_not_b, args, 1));
}


/* ---------------- eq?, eqv?, equal? ---------------- */

static void eq_b(int *args, ptree *pt) {BOOL_RESULT = (Int)(INT_ARG_1 == INT_ARG_2);} /* safe because float arg -> #f below */
static void vct_eq_b(int *args, ptree *pt) {BOOL_RESULT = (Int)(VCT_ARG_1 == VCT_ARG_2);} 
static void sd_eq_b(int *args, ptree *pt) {BOOL_RESULT = (Int)(SOUND_DATA_ARG_1 == SOUND_DATA_ARG_2);} 
static void xen_eq_b(int *args, ptree *pt) {BOOL_RESULT = (Int)XEN_EQ_P(RXEN_ARG_1, RXEN_ARG_2);}
static void clm_eq_b(int *args, ptree *pt) {BOOL_RESULT = (Int)(CLM_ARG_1 == CLM_ARG_2);}
static void reader_eq_b(int *args, ptree *pt) {BOOL_RESULT = (Int)(READER_ARG_1 == READER_ARG_2);} /* safe because float arg -> #f below */
static void mix_reader_eq_b(int *args, ptree *pt) {BOOL_RESULT = (Int)(MIX_READER_ARG_1 == MIX_READER_ARG_2);} /* safe because float arg -> #f below */
static void track_reader_eq_b(int *args, ptree *pt) {BOOL_RESULT = (Int)(TRACK_READER_ARG_1 == TRACK_READER_ARG_2);} /* safe because float arg -> #f below */

static char *descr_eq_b(int *args, ptree *pt) 
{
  return(mus_format( BOOL_PT " = eq?(" INT_PT ", " INT_PT ")", args[0], B2S(BOOL_RESULT), args[1], INT_ARG_1, args[2], INT_ARG_2));
}
static char *descr_vct_eq_b(int *args, ptree *pt) 
{
  return(mus_format( BOOL_PT " = eq?(" VCT_PT ", " VCT_PT ")", args[0], B2S(BOOL_RESULT), args[1], DESC_VCT_ARG_1, args[2], DESC_VCT_ARG_2));
}
static char *descr_sd_eq_b(int *args, ptree *pt) 
{
  return(mus_format( BOOL_PT " = eq?(" SD_PT ", " SD_PT ")", args[0], B2S(BOOL_RESULT), args[1], DESC_SOUND_DATA_ARG_1, args[2], DESC_SOUND_DATA_ARG_2));
}
static char *descr_xen_eq_b(int *args, ptree *pt) 
{
  return(mus_format( BOOL_PT " = eq?(" LST_PT ", " LST_PT ")", args[0], B2S(BOOL_RESULT), args[1], DESC_RXEN_ARG_1, args[2], DESC_RXEN_ARG_2));
}
static char *descr_clm_eq_b(int *args, ptree *pt) 
{
  return(mus_format( BOOL_PT " = eq?(" CLM_PT ", " CLM_PT ")", args[0], B2S(BOOL_RESULT), args[1], DESC_CLM_ARG_1, args[2], DESC_CLM_ARG_2));
}
static char *descr_reader_eq_b(int *args, ptree *pt) 
{
  return(mus_format( BOOL_PT " = eq?(" RD_PT ", " RD_PT ")", args[0], B2S(BOOL_RESULT), args[1], DESC_READER_ARG_1, args[2], DESC_READER_ARG_2));
}
static char *descr_mix_reader_eq_b(int *args, ptree *pt) 
{
  return(mus_format( BOOL_PT " = eq?(" MF_PT ", " MF_PT ")", args[0], B2S(BOOL_RESULT), args[1], DESC_MIX_READER_ARG_1, args[2], DESC_MIX_READER_ARG_2));
}
static char *descr_track_reader_eq_b(int *args, ptree *pt) 
{
  return(mus_format( BOOL_PT " = eq?(" TF_PT ", " TF_PT ")", args[0], B2S(BOOL_RESULT), args[1], DESC_TRACK_READER_ARG_1, args[2], DESC_TRACK_READER_ARG_2));
}

static xen_value *eq_p(ptree *prog, xen_value **args, int num_args)
{
  if ((args[1]->type != args[2]->type) || (args[1]->type == R_FLOAT) || (args[1]->type == R_FUNCTION))
    return(make_xen_value(R_BOOL, add_int_to_ptree(prog, (Int)false), R_CONSTANT));
  if (prog->constants == 2) /* this can include xen vals */
    {
      if (args[1]->addr == args[2]->addr) /* types are same, checked above */
	return(make_xen_value(R_BOOL, add_int_to_ptree(prog, (Int)true), R_CONSTANT));
      if ((args[1]->type == R_INT) || (args[1]->type == R_BOOL))
	return(make_xen_value(R_BOOL, add_int_to_ptree(prog, prog->ints[args[1]->addr] == prog->ints[args[2]->addr]), R_CONSTANT));
    }
  switch (args[1]->type)
    {
    case R_VCT: return(package(prog, R_BOOL, vct_eq_b, descr_vct_eq_b, args, 2));
    case R_SOUND_DATA: return(package(prog, R_BOOL, sd_eq_b, descr_sd_eq_b, args, 2));
    case R_CLM: return(package(prog, R_BOOL, clm_eq_b, descr_clm_eq_b, args, 2));
    case R_READER: return(package(prog, R_BOOL, reader_eq_b, descr_reader_eq_b, args, 2));
    case R_MIX_READER: return(package(prog, R_BOOL, mix_reader_eq_b, descr_mix_reader_eq_b, args, 2));
    case R_TRACK_READER: return(package(prog, R_BOOL, track_reader_eq_b, descr_track_reader_eq_b, args, 2));
    case R_KEYWORD:
    case R_LIST:
    case R_PAIR:
    case R_SYMBOL: return(package(prog, R_BOOL, xen_eq_b, descr_xen_eq_b, args, 2));
      /* R_FLOAT and R_FUNCTION -> false above */
    }
  return(package(prog, R_BOOL, eq_b, descr_eq_b, args, 2));
}

/* -------- eqv/equal -------- */

static void vct_eqv_b(int *args, ptree *pt) {BOOL_RESULT = (Int)(vct_equalp(VCT_ARG_1, VCT_ARG_2));} 
static void sd_eqv_b(int *args, ptree *pt) {BOOL_RESULT = (Int)(sound_data_equalp(SOUND_DATA_ARG_1, SOUND_DATA_ARG_2));} 
static void xen_eqv_b(int *args, ptree *pt) {BOOL_RESULT = (Int)XEN_EQV_P(RXEN_ARG_1, RXEN_ARG_2);}
static void eqv_fb(int *args, ptree *pt) {BOOL_RESULT = (Int)(FLOAT_ARG_1 == FLOAT_ARG_2);}
static void eqv_clm(int *args, ptree *pt) {BOOL_RESULT = (Int)mus_equalp(CLM_ARG_1, CLM_ARG_2);}

static char *descr_eqv_fb(int *args, ptree *pt) 
{
  return(mus_format( BOOL_PT " = eqv?(" FLT_PT ", " FLT_PT ")", args[0], B2S(BOOL_RESULT), args[1], FLOAT_ARG_1, args[2], FLOAT_ARG_2));
}
static char *descr_vct_eqv_b(int *args, ptree *pt) 
{
  return(mus_format( BOOL_PT " = eqv?(" VCT_PT ", " VCT_PT ")", args[0], B2S(BOOL_RESULT), args[1], DESC_VCT_ARG_1, args[2], DESC_VCT_ARG_2));
}
static char *descr_sd_eqv_b(int *args, ptree *pt) 
{
  return(mus_format( BOOL_PT " = eqv?(" SD_PT ", " SD_PT ")", args[0], B2S(BOOL_RESULT), args[1], DESC_SOUND_DATA_ARG_1, args[2], DESC_SOUND_DATA_ARG_2));
}
static char *descr_xen_eqv_b(int *args, ptree *pt) 
{
  return(mus_format( BOOL_PT " = eqv?(" LST_PT ", " LST_PT ")", args[0], B2S(BOOL_RESULT), args[1], DESC_RXEN_ARG_1, args[2], DESC_RXEN_ARG_2));
}
static char *descr_eqv_clm(int *args, ptree *pt) 
{
  return(mus_format( BOOL_PT " = equal?(" CLM_PT ", " CLM_PT ")", 
		     args[0], B2S(BOOL_RESULT), args[1], DESC_CLM_ARG_1, args[2], DESC_CLM_ARG_2));
}

static xen_value *eqv_p(ptree *prog, xen_value **args, int num_args)
{
  if ((args[1]->type != args[2]->type) || (args[1]->type == R_FUNCTION))
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
    case R_FLOAT: return(package(prog, R_BOOL, eqv_fb, descr_eqv_fb, args, 2)); 
    case R_CLM:   return(package(prog, R_BOOL, eqv_clm, descr_eqv_clm, args, 2)); 
    case R_READER: return(package(prog, R_BOOL, reader_eq_b, descr_reader_eq_b, args, 2));
    case R_MIX_READER: return(package(prog, R_BOOL, mix_reader_eq_b, descr_mix_reader_eq_b, args, 2));
    case R_TRACK_READER: return(package(prog, R_BOOL, track_reader_eq_b, descr_track_reader_eq_b, args, 2));
    case R_VCT: return(package(prog, R_BOOL, vct_eqv_b, descr_vct_eqv_b, args, 2));
    case R_SOUND_DATA: return(package(prog, R_BOOL, sd_eqv_b, descr_sd_eqv_b, args, 2));
    case R_KEYWORD:
    case R_LIST:
    case R_PAIR:
    case R_SYMBOL: return(package(prog, R_BOOL, xen_eqv_b, descr_xen_eqv_b, args, 2));
    }
  return(package(prog, R_BOOL, eq_b, descr_eq_b, args, 2));
}

static void xen_equal_b(int *args, ptree *pt) {BOOL_RESULT = (Int)XEN_EQUAL_P(RXEN_ARG_1, RXEN_ARG_2);}
static char *descr_xen_equal_b(int *args, ptree *pt) 
{
  return(mus_format( BOOL_PT " = equal?(" LST_PT ", " LST_PT ")", args[0], B2S(BOOL_RESULT), args[1], DESC_RXEN_ARG_1, args[2], DESC_RXEN_ARG_2));
}
static xen_value *equal_p(ptree *prog, xen_value **args, int num_args)
{
  if (args[1]->type == args[2]->type)
    switch (args[1]->type)
      {
      case R_KEYWORD:
      case R_LIST:
      case R_PAIR:
      case R_SYMBOL: return(package(prog, R_BOOL, xen_equal_b, descr_xen_equal_b, args, 2));
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
      FREE(v);
      return(newv);
    }
  return(v);
}
static void odd_i(int *args, ptree *pt) {BOOL_RESULT = (Int)(INT_ARG_1 & 1);}
static char *descr_odd_i(int *args, ptree *pt) 
{
  return(mus_format( BOOL_PT " = odd?(" INT_PT ")", args[0], B2S(BOOL_RESULT), args[1], INT_ARG_1));
}
static xen_value *odd_p(ptree *prog, xen_value **args, int num_args)
{
  args[1] = convert_to_int(prog, args[1]);
  if (args[1] == NULL) return(run_warn("odd? can't convert arg"));
  if (prog->constants == 1)
    return(make_xen_value(R_BOOL, add_int_to_ptree(prog, prog->ints[args[1]->addr] & 1), R_CONSTANT));
  return(package(prog, R_BOOL, odd_i, descr_odd_i, args, 1));
}

static void even_i(int *args, ptree *pt) {BOOL_RESULT = (Int)(!(INT_ARG_1 & 1));}
static char *descr_even_i(int *args, ptree *pt) 
{
  return(mus_format( BOOL_PT " = even?(" INT_PT ")", args[0], B2S(BOOL_RESULT), args[1], INT_ARG_1));
}
static xen_value *even_p(ptree *prog, xen_value **args, int num_args)
{
  args[1] = convert_to_int(prog, args[1]);
  if (args[1] == NULL) return(run_warn("even? can't convert arg"));
  if (prog->constants == 1)
    return(make_xen_value(R_BOOL, add_int_to_ptree(prog, (!(prog->ints[args[1]->addr] & 1))), R_CONSTANT));
  return(package(prog, R_BOOL, even_i, descr_even_i, args, 1));
}

#define INT_POS_P(CName, SName, COp) \
static void CName ## _i(int *args, ptree *pt) {BOOL_RESULT = (Int)(INT_ARG_1 COp 0);} \
static char *descr_ ## CName ## _i(int *args, ptree *pt) \
{ \
  return(mus_format( BOOL_PT " = (" INT_PT #COp " 0)", args[0], B2S(BOOL_RESULT), args[1], INT_ARG_1)); \
} \
static void CName ## _f(int *args, ptree *pt) {BOOL_RESULT = (Int)(FLOAT_ARG_1 COp 0.0);} \
static char *descr_ ## CName ## _f(int *args, ptree *pt) \
{ \
  return(mus_format( BOOL_PT " = (" FLT_PT " " #COp " 0.0)", args[0], B2S(BOOL_RESULT), args[1], FLOAT_ARG_1)); \
} \
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
    return(package(prog, R_BOOL, CName ## _f, descr_ ## CName ## _f, args, 1)); \
  return(package(prog, R_BOOL, CName ## _i, descr_ ## CName ## _i, args, 1)); \
}

INT_POS_P(zero, zero?, ==)
INT_POS_P(positive, positive?, >)
INT_POS_P(negative, negative?, <)


static void single_to_float(ptree *prog, xen_value **args, int num)
{
  xen_value *old_loc;
  old_loc = args[num];
  args[num] = make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, 0.0), R_VARIABLE);
  add_triple_to_ptree(prog, va_make_triple(store_i_f, descr_store_i_f, 2, args[num], old_loc));
  FREE(old_loc);
}

/* ---------------- sin, cos, tan ---------------- */

#define FL_OP(CName) \
static void CName ## _f(int *args, ptree *pt) {FLOAT_RESULT = CName(FLOAT_ARG_1);} \
static char *descr_ ## CName ## _f(int *args, ptree *pt)  \
{ \
  return(mus_format( FLT_PT " = " #CName "(" FLT_PT ")", args[0], FLOAT_RESULT, args[1], FLOAT_ARG_1)); \
} \
static xen_value * CName ## _1(ptree *prog, xen_value **args, int num_args) \
{ \
  if (prog->constants == 1) \
    { \
      if (args[1]->type == R_INT) \
	return(make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, CName((Double)(prog->ints[args[1]->addr]))), R_CONSTANT)); \
      else return(make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, CName(prog->dbls[args[1]->addr])), R_CONSTANT)); \
    } \
  if (args[1]->type == R_INT) single_to_float(prog, args, 1); \
  return(package(prog, R_FLOAT, CName ## _f, descr_ ##CName ## _f, args, 1)); \
}

FL_OP(mus_radians_to_hz)
FL_OP(mus_hz_to_radians)
FL_OP(mus_degrees_to_radians)
FL_OP(mus_radians_to_degrees)
FL_OP(mus_db_to_linear)
FL_OP(mus_linear_to_db)
FL_OP(mus_random)

FL_OP(sin)
FL_OP(cos)
FL_OP(tan)

#define FL_OP_C(CName) \
static void CName ## _f(int *args, ptree *pt) {FLOAT_RESULT = CName(FLOAT_ARG_1);} \
static char *descr_ ## CName ## _f(int *args, ptree *pt)  \
{ \
  return(mus_format( FLT_PT " = " #CName "(" FLT_PT ")", args[0], FLOAT_RESULT, args[1], FLOAT_ARG_1)); \
} \
static xen_value * CName ## _1(ptree *prog, xen_value **args, int num_args) \
{ \
  if (current_optimization < COMPLEX_OK) return(NULL); \
  if (prog->constants == 1) \
    { \
      if (args[1]->type == R_INT) \
	return(make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, CName((Double)(prog->ints[args[1]->addr]))), R_CONSTANT)); \
      else return(make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, CName(prog->dbls[args[1]->addr])), R_CONSTANT)); \
    } \
  if (args[1]->type == R_INT) single_to_float(prog, args, 1); \
  return(package(prog, R_FLOAT, CName ## _f, descr_ ##CName ## _f, args, 1)); \
}

FL_OP_C(asin)
FL_OP_C(acos)
FL_OP_C(sqrt)
FL_OP_C(log)
FL_OP_C(exp)

static void atan1_f(int *args, ptree *pt) {FLOAT_RESULT = atan(FLOAT_ARG_1);}
static char *descr_atan1_f(int *args, ptree *pt) {return(mus_format( FLT_PT " = atan(" FLT_PT ")", args[0], FLOAT_RESULT, args[1], FLOAT_ARG_1));}
static xen_value *atan1_1(ptree *prog, xen_value **args, int num_args)
{
  if (current_optimization < COMPLEX_OK) return(NULL);
  if (prog->constants == 1)
    {
      if (args[1]->type == R_INT)
	return(make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, atan((Double)(prog->ints[args[1]->addr]))), R_CONSTANT));
      else return(make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, atan(prog->dbls[args[1]->addr])), R_CONSTANT));
    }
  if (args[1]->type == R_INT) single_to_float(prog, args, 1);
  return(package(prog, R_FLOAT, atan1_f, descr_atan1_f, args, 1));
}

static void atan2_f(int *args, ptree *pt) {FLOAT_RESULT = atan2(FLOAT_ARG_1, FLOAT_ARG_2);}
static char *descr_atan2_f(int *args, ptree *pt) 
{
  return(mus_format( FLT_PT " = atan2(" FLT_PT ", " FLT_PT ")", args[0], FLOAT_RESULT, args[1], FLOAT_ARG_1, args[2], FLOAT_ARG_2));
}
static xen_value *atan2_1(ptree *prog, xen_value **args, int num_args)
{
  xen_value *temp;
  if (current_optimization < COMPLEX_OK) return(NULL);
  if (args[1]->type == R_INT)
    {
      temp = args[1];
      args[1] = convert_int_to_dbl(prog, args[1]);
      FREE(temp);
    }
  if (args[2]->type == R_INT)
    {
      temp = args[2];
      args[2] = convert_int_to_dbl(prog, args[2]);
      FREE(temp);
    }
  if (prog->constants == 2)
    return(make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, atan2(prog->dbls[args[1]->addr], prog->dbls[args[2]->addr])), R_CONSTANT));
  return(package(prog, R_FLOAT, atan2_f, descr_atan2_f, args, 2));
}



/* ---------------- round ---------------- */

static Double f_round(Double x)
{
  /* tricky here -- if .5 diff, need to round to nearest even int */
  /* this code from Guile numbers.c */
  double plus_half = x + 0.5;
  double result = floor(plus_half);
  return((plus_half == result) && ((plus_half / 2) != floor(plus_half / 2)) ? result - 1 : result);
}

static void round_f(int *args, ptree *pt) {FLOAT_RESULT = f_round(FLOAT_ARG_1);}
static char *descr_round_f(int *args, ptree *pt) {return(mus_format( FLT_PT " = round(" FLT_PT ")", args[0], FLOAT_RESULT, args[1], FLOAT_ARG_1));}
static void round_i(int *args, ptree *pt) {INT_RESULT = (Int)f_round(FLOAT_ARG_1);}
static char *descr_round_i(int *args, ptree *pt) {return(mus_format( INT_PT " = (Int)round(" FLT_PT ")", args[0], INT_RESULT, args[1], FLOAT_ARG_1));}
static xen_value *round_1(ptree *prog, xen_value **args, int num_args)
{
  /* (round 1) -> 1.0! */
  if (args[1]->type == R_INT)
    return(copy_xen_value(args[1]));
  if (prog->constants == 1)
    {
      if (prog->walk_result == NEED_INT_RESULT)
	return(make_xen_value(R_INT, add_int_to_ptree(prog, (Int)f_round(prog->dbls[args[1]->addr])), R_CONSTANT));
      return(make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, f_round(prog->dbls[args[1]->addr])), R_CONSTANT));
    }
  if (prog->walk_result == NEED_INT_RESULT)
    return(package(prog, R_INT, round_i, descr_round_i, args, 1));
  return(package(prog, R_FLOAT, round_f, descr_round_f, args, 1));
}


/* ---------------- truncate ---------------- */

static Double f_truncate(Double x)
{
  if (x < 0.0)
    return(-floor(-x));
  return(floor(x));
}

static void truncate_f(int *args, ptree *pt) {FLOAT_RESULT = f_truncate(FLOAT_ARG_1);}
static char *descr_truncate_f(int *args, ptree *pt) 
{
  return(mus_format( FLT_PT " = truncate(" FLT_PT ")", args[0], FLOAT_RESULT, args[1], FLOAT_ARG_1));
}
static void truncate_i(int *args, ptree *pt) {INT_RESULT = (Int)f_truncate(FLOAT_ARG_1);}
static char *descr_truncate_i(int *args, ptree *pt) 
{
  return(mus_format( INT_PT " = truncate(" FLT_PT ")", args[0], INT_RESULT, args[1], FLOAT_ARG_1));
}
static xen_value *truncate_1(ptree *prog, xen_value **args, int num_args)
{
  if (args[1]->type == R_INT)
    return(copy_xen_value(args[1]));
  if (prog->constants == 1)
    {
      if (prog->walk_result == NEED_INT_RESULT)
	return(make_xen_value(R_INT, add_int_to_ptree(prog, (Int)f_truncate(prog->dbls[args[1]->addr])), R_CONSTANT));
      return(make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, f_truncate(prog->dbls[args[1]->addr])), R_CONSTANT));
    }
  if (prog->walk_result == NEED_INT_RESULT)
    return(package(prog, R_INT, truncate_i, descr_truncate_i, args, 1));
  return(package(prog, R_FLOAT, truncate_f, descr_truncate_f, args, 1));
}

/* ---------------- floor ---------------- */

static void floor_f(int *args, ptree *pt) {FLOAT_RESULT = floor(FLOAT_ARG_1);}
static char *descr_floor_f(int *args, ptree *pt) {return(mus_format( FLT_PT " = floor(" FLT_PT ")", args[0], FLOAT_RESULT, args[1], FLOAT_ARG_1));}
static void floor_i(int *args, ptree *pt) {INT_RESULT = (Int)floor(FLOAT_ARG_1);}
static char *descr_floor_i(int *args, ptree *pt) {return(mus_format( INT_PT " = (Int)floor(" FLT_PT ")", args[0], INT_RESULT, args[1], FLOAT_ARG_1));}
static xen_value *floor_1(ptree *prog, xen_value **args, int num_args)
{
  if (args[1]->type == R_INT)
    return(copy_xen_value(args[1]));
  if (prog->constants == 1)
    {
      if (prog->walk_result == NEED_INT_RESULT)
	return(make_xen_value(R_INT, add_int_to_ptree(prog, (Int)floor(prog->dbls[args[1]->addr])), R_CONSTANT));
      return(make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, floor(prog->dbls[args[1]->addr])), R_CONSTANT));
    }
  if (prog->walk_result == NEED_INT_RESULT)
    return(package(prog, R_INT, floor_i, descr_floor_i, args, 1));
  return(package(prog, R_FLOAT, floor_f, descr_floor_f, args, 1));
}

/* ---------------- ceiling ---------------- */

static void ceiling_f(int *args, ptree *pt) {FLOAT_RESULT = ceil(FLOAT_ARG_1);}
static char *descr_ceiling_f(int *args, ptree *pt) {return(mus_format( FLT_PT " = ceil(" FLT_PT ")", args[0], FLOAT_RESULT, args[1], FLOAT_ARG_1));}
static void ceiling_i(int *args, ptree *pt) {INT_RESULT = (Int)ceil(FLOAT_ARG_1);}
static char *descr_ceiling_i(int *args, ptree *pt) {return(mus_format( INT_PT " = (Int)ceil(" FLT_PT ")", args[0], INT_RESULT, args[1], FLOAT_ARG_1));}
static xen_value *ceiling_1(ptree *prog, xen_value **args, int num_args)
{
  if (args[1]->type == R_INT)
    return(copy_xen_value(args[1]));
  if (prog->constants == 1)
    {
      if (prog->walk_result == NEED_INT_RESULT)
	return(make_xen_value(R_INT, add_int_to_ptree(prog, (Int)ceil(prog->dbls[args[1]->addr])), R_CONSTANT));
      return(make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, ceil(prog->dbls[args[1]->addr])), R_CONSTANT));
    }
  if (prog->walk_result == NEED_INT_RESULT)
    return(package(prog, R_INT, ceiling_i, descr_ceiling_i, args, 1));
  return(package(prog, R_FLOAT, ceiling_f, descr_ceiling_f, args, 1));
}

/* ---------------- exact->inexact ---------------- */

static xen_value *exact2inexact_1(ptree *prog, xen_value **args, int num_args)
{
  if (args[1]->type == R_FLOAT)
    return(copy_xen_value(args[1]));
  if (prog->constants == 1)
    return(make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, (Double)(prog->ints[args[1]->addr])), R_CONSTANT));
  return(package(prog, R_FLOAT, store_i_f, descr_store_i_f, args, 1));
}

static void i2e_f(int *args, ptree *pt) {INT_RESULT = (Int)floor(FLOAT_ARG_1 + 0.5);}
static char *descr_i2e_f(int *args, ptree *pt) {return(mus_format( INT_PT " = floor(" FLT_PT " + 0.5)", args[0], INT_RESULT, args[1], FLOAT_ARG_1));}
static xen_value *inexact2exact_1(ptree *prog, xen_value **args, int num_args)
{
  if (args[1]->type == R_INT)
    return(copy_xen_value(args[1]));
  if (prog->constants == 1)
    return(make_xen_value(R_INT, add_int_to_ptree(prog, (Int)floor(prog->dbls[args[1]->addr] + 0.5)), R_CONSTANT));
  return(package(prog, R_INT, i2e_f, descr_i2e_f, args, 1));
}


/* ---------------- gcd, lcm ---------------- */

static Int c_gcd_1(Int a, Int b)
{
  Int rem;
  if (a == b) return(a);
  else
    {
      if (a > b)
	{
	  rem = (Int)(fmod(a, b));
	  if (rem == 0.0) return(b);
	  else return(c_gcd_1(b, rem));
	}
      else return(c_gcd_1(b, a));
    }
}

static Int c_gcd(Int a, Int b)
{
  if (a < 0) a = -a;
  if (b < 0) b = -b;
  return(c_gcd_1(a, b));
}

static Int c_lcm(Int a, Int b)
{
  if ((a == 0) || (b == 0)) return(0);
  if (a < 0) a = -a;
  if (b < 0) b = -b;
  return((a * b) / c_gcd_1(a, b));
}

static void gcd_in(int *args, ptree *pt)
{
  int i, n;
  Int mx;
  n = pt->ints[args[1]];
  mx = c_gcd(pt->ints[args[2]], pt->ints[args[3]]);
  /* this could be optimized... */
  for (i = 3; i <= n; i++)
    {
      if (mx == 1) break;
      mx = c_gcd(mx, pt->ints[args[i + 1]]);
    }
  INT_RESULT = mx;
}
static char *descr_gcd_in(int *args, ptree *pt) {return(descr_max_min_in(args, pt, "gcd"));}

static void gcd_i(int *args, ptree *pt) {INT_RESULT = c_gcd(INT_ARG_1, INT_ARG_2);}
static char *descr_gcd_i(int *args, ptree *pt) 
{
  return(mus_format( INT_PT " = gcd(" INT_PT ", " INT_PT ")", args[0], INT_RESULT, args[1], INT_ARG_1, args[2], INT_ARG_2));
}
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
	return(make_xen_value(R_INT, add_int_to_ptree(prog, prog->ints[args[1]->addr]), R_CONSTANT)); /* (gcd n) -> n */
      else 
	{
	  args[0] = make_xen_value(R_INT, add_int_to_ptree(prog, 0), R_VARIABLE);
	  add_triple_to_ptree(prog, make_triple(store_i, descr_store_i, args, 2));
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
    return(package(prog, R_INT, gcd_i, descr_gcd_i, args, 2));
  return(package_n(prog, R_INT, gcd_in, descr_gcd_in, args, num_args));
}

static void lcm_i(int *args, ptree *pt) {INT_RESULT = c_lcm(INT_ARG_1, INT_ARG_2);}
static char *descr_lcm_i(int *args, ptree *pt) 
{
  return(mus_format( INT_PT " = lcm(" INT_PT ", " INT_PT ")", args[0], INT_RESULT, args[1], INT_ARG_1, args[2], INT_ARG_2));
}
static void lcm_in(int *args, ptree *pt)
{
  int i, n;
  Int mx;
  n = pt->ints[args[1]];
  mx = c_lcm(pt->ints[args[2]], pt->ints[args[3]]);
  /* this could be optimized... */
  for (i = 3; i <= n; i++)
    {
      if (mx == 0) break;
      mx = c_lcm(mx, pt->ints[args[i + 1]]);
    }
  INT_RESULT = mx;
}
static char *descr_lcm_in(int *args, ptree *pt) {return(descr_max_min_in(args, pt, "lcm"));}

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
	return(make_xen_value(R_INT, add_int_to_ptree(prog, prog->ints[args[1]->addr]), R_CONSTANT)); /* (lcm n) -> n */
      else 
	{
	  args[0] = make_xen_value(R_INT, add_int_to_ptree(prog, 0), R_VARIABLE);
	  add_triple_to_ptree(prog, make_triple(store_i, descr_store_i, args, 2));
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
    return(package(prog, R_INT, lcm_i, descr_lcm_i, args, 2));
  return(package_n(prog, R_INT, lcm_in, descr_lcm_in, args, num_args));
}


/* ---------------- remainder, quotient, modulo ---------------- */

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

static void modulo_i(int *args, ptree *pt) {INT_RESULT = c_mod(INT_ARG_1, INT_ARG_2);}
static char *descr_modulo_i(int *args, ptree *pt) 
{
  return(mus_format( INT_PT " = modulo(" INT_PT ", " INT_PT ")", args[0], INT_RESULT, args[1], INT_ARG_1, args[2], INT_ARG_2));
}
static xen_value *modulo_1(ptree *prog, xen_value **args, int num_args)
{
  args[1] = convert_to_int(prog, args[1]);
  if (args[1] == NULL) return(run_warn("modulo arg1 can't convert to int"));
  args[2] = convert_to_int(prog, args[2]);
  if (args[2] == NULL) return(run_warn("modulo arg2 can't convert to int"));
  if (prog->constants == 2)
    return(make_xen_value(R_INT, add_int_to_ptree(prog, c_mod(prog->ints[args[1]->addr], prog->ints[args[2]->addr])), R_CONSTANT));
  return(package(prog, R_INT, modulo_i, descr_modulo_i, args, 2));
}

static void remainder_i(int *args, ptree *pt) {INT_RESULT = (INT_ARG_1 % INT_ARG_2);}
static char *descr_remainder_i(int *args, ptree *pt) 
{
  return(mus_format( INT_PT " = (" INT_PT " %% " INT_PT ")", args[0], INT_RESULT, args[1], INT_ARG_1, args[2], INT_ARG_2));
}
static xen_value *remainder_1(ptree *prog, xen_value **args, int num_args)
{
  args[1] = convert_to_int(prog, args[1]);
  if (args[1] == NULL) return(run_warn("remainder arg1 can't convert to int"));
  args[2] = convert_to_int(prog, args[2]);
  if (args[2] == NULL) return(run_warn("remainder arg2 can't convert to int"));
  if (prog->constants == 2)
    return(make_xen_value(R_INT, add_int_to_ptree(prog, (prog->ints[args[1]->addr] % prog->ints[args[2]->addr])), R_CONSTANT));
  return(package(prog, R_INT, remainder_i, descr_remainder_i, args, 2));
}

static void quotient_i(int *args, ptree *pt) {INT_RESULT = (INT_ARG_1 / INT_ARG_2);}
static char *descr_quotient_i(int *args, ptree *pt) 
{
  return(mus_format( INT_PT " = (" INT_PT " / " INT_PT ")", args[0], INT_RESULT, args[1], INT_ARG_1, args[2], INT_ARG_2));
}
static xen_value *quotient_1(ptree *prog, xen_value **args, int num_args)
{
  args[1] = convert_to_int(prog, args[1]);
  if (args[1] == NULL) return(run_warn("quotient arg1 can't convert to int"));
  args[2] = convert_to_int(prog, args[2]);
  if (args[2] == NULL) return(run_warn("quotient arg2 can't convert to int"));
  if (prog->constants == 2)
    return(make_xen_value(R_INT, add_int_to_ptree(prog, (prog->ints[args[1]->addr] / prog->ints[args[2]->addr])), R_CONSTANT));
  return(package(prog, R_INT, quotient_i, descr_quotient_i, args, 2));
}



/* ---------------- logand, logior, logxor, lognot, ash ---------------- */

static void logand_i(int *args, ptree *pt) {INT_RESULT = (INT_ARG_1 & INT_ARG_2);}
static char *descr_logand_i(int *args, ptree *pt) 
{
  return(mus_format( INT_PT " = (" INT_PT " & " INT_PT ")", args[0], INT_RESULT, args[1], INT_ARG_1, args[2], INT_ARG_2));
}
static xen_value *logand_1(ptree *prog, xen_value **args, int num_args)
{
  if (prog->constants == 2)
    return(make_xen_value(R_INT, add_int_to_ptree(prog, (prog->ints[args[1]->addr] & prog->ints[args[2]->addr])), R_CONSTANT));
  return(package(prog, R_INT, logand_i, descr_logand_i, args, 2));
}

static void logior_i(int *args, ptree *pt) {INT_RESULT = (INT_ARG_1 | INT_ARG_2);}
static char *descr_logior_i(int *args, ptree *pt) 
{
  return(mus_format( INT_PT " = (" INT_PT " | " INT_PT ")", args[0], INT_RESULT, args[1], INT_ARG_1, args[2], INT_ARG_2));
}
static xen_value *logior_1(ptree *prog, xen_value **args, int num_args)
{
  if (prog->constants == 2)
    return(make_xen_value(R_INT, add_int_to_ptree(prog, (prog->ints[args[1]->addr] | prog->ints[args[2]->addr])), R_CONSTANT));
  return(package(prog, R_INT, logior_i, descr_logior_i, args, 2));
}

#ifndef XOR
  #define XOR(a, b) ((~((a) & (b))) & ((a) | (b)))
#endif
static void logxor_i(int *args, ptree *pt) {INT_RESULT = XOR(INT_ARG_1, INT_ARG_2);}
static char *descr_logxor_i(int *args, ptree *pt) 
{
  return(mus_format( INT_PT " = logxor(" INT_PT ", " INT_PT ")", args[0], INT_RESULT, args[1], INT_ARG_1, args[2], INT_ARG_2));
}
static xen_value *logxor_1(ptree *prog, xen_value **args, int num_args)
{
  if (prog->constants == 2)
    return(make_xen_value(R_INT, add_int_to_ptree(prog, XOR(prog->ints[args[1]->addr], prog->ints[args[2]->addr])), R_CONSTANT));
  return(package(prog, R_INT, logxor_i, descr_logxor_i, args, 2));
}

static void lognot_i(int *args, ptree *pt) {INT_RESULT = ~INT_ARG_1;}
static char *descr_lognot_i(int *args, ptree *pt) {return(mus_format( INT_PT " = ~" INT_PT , args[0], INT_RESULT, args[1], INT_ARG_1));}
static xen_value *lognot_1(ptree *prog, xen_value **args, int num_args)
{
  if (prog->constants == 1)
    return(make_xen_value(R_INT, add_int_to_ptree(prog, ~(prog->ints[args[1]->addr])), R_CONSTANT));
  return(package(prog, R_INT, lognot_i, descr_lognot_i, args, 1));
}

static Int c_ash(Int arg1, Int arg2)
{
  if (arg2 >= 0)
    return(arg1 << arg2);
  return(arg1 >> -arg2);
}

static void ash_i(int *args, ptree *pt) {INT_RESULT = c_ash(INT_ARG_1, INT_ARG_2);}
static char *descr_ash_i(int *args, ptree *pt) 
{
  return(mus_format( INT_PT " = ash(" INT_PT ", " INT_PT ")", args[0], INT_RESULT, args[1], INT_ARG_1, args[2], INT_ARG_2));
}
static xen_value *ash_1(ptree *prog, xen_value **args, int num_args)
{
  if (prog->constants == 2)
    return(make_xen_value(R_INT, add_int_to_ptree(prog, c_ash(prog->ints[args[1]->addr], prog->ints[args[2]->addr])), R_CONSTANT));
  return(package(prog, R_INT, ash_i, descr_ash_i, args, 2));
}


/* ---------------- expt ---------------- */

static void expt_f(int *args, ptree *pt) {FLOAT_RESULT = pow(FLOAT_ARG_1, FLOAT_ARG_2);}
static char *descr_expt_f(int *args, ptree *pt) 
{
  return(mus_format( FLT_PT " = pow(" FLT_PT ", " FLT_PT ")", args[0], FLOAT_RESULT, args[1], FLOAT_ARG_1, args[2], FLOAT_ARG_2));
}
static xen_value *expt_1(ptree *prog, xen_value **args, int num_args)
{
  Double f1, f2;
  if (current_optimization < COMPLEX_OK) return(NULL);
  if (prog->constants == 2)
    {
      if (args[1]->type == R_INT) f1 = (Double)(prog->ints[args[1]->addr]); else f1 = prog->dbls[args[1]->addr];
      if (args[2]->type == R_INT) f2 = (Double)(prog->ints[args[2]->addr]); else f2 = prog->dbls[args[2]->addr];
      return(make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, pow(f1, f2)), R_CONSTANT));
    }
  if (args[1]->type == R_INT) single_to_float(prog, args, 1);
  if (args[2]->type == R_INT) single_to_float(prog, args, 2);
  return(package(prog, R_FLOAT, expt_f, descr_expt_f, args, 2));
}


/* ---------------- abs ---------------- */

static void abs_f(int *args, ptree *pt) {FLOAT_RESULT = fabs(FLOAT_ARG_1);}
static char *descr_abs_f(int *args, ptree *pt) {return(mus_format( FLT_PT " = fabs(" FLT_PT ")", args[0], FLOAT_RESULT, args[1], FLOAT_ARG_1));}
static void abs_i(int *args, ptree *pt) {INT_RESULT = ((INT_ARG_1 >= 0) ? INT_ARG_1 : (-INT_ARG_1));} /* not abs=32 bit truncation */
static char *descr_abs_i(int *args, ptree *pt) {return(mus_format( INT_PT " = abs(" INT_PT ")", args[0], INT_RESULT, args[1], INT_ARG_1));}
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
    return(package(prog, R_INT, abs_i, descr_abs_i, args, 1));
  return(package(prog, R_FLOAT, abs_f, descr_abs_f, args, 1));
}


/* ---------------- random ---------------- */

static void random_f(int *args, ptree *pt) {FLOAT_RESULT = XEN_FRANDOM(FLOAT_ARG_1);}
static char *descr_random_f(int *args, ptree *pt) 
{
  return(mus_format( FLT_PT " = random(" FLT_PT ")", args[0], FLOAT_RESULT, args[1], FLOAT_ARG_1));
}
static void random_i(int *args, ptree *pt) {INT_RESULT = XEN_IRANDOM(INT_ARG_1);}
static char *descr_random_i(int *args, ptree *pt) 
{
  return(mus_format( INT_PT " = random(" INT_PT ")", args[0], INT_RESULT, args[1], INT_ARG_1));
}
static xen_value *random_1(ptree *prog, xen_value **args, int num_args)
{
  if (args[1]->type == R_INT)
    return(package(prog, R_INT, random_i, descr_random_i, args, 1));
  return(package(prog, R_FLOAT, random_f, descr_random_f, args, 1));
}


/* ---------------- chars ---------------- */

#define CHARP(SName, CName) \
static void char_ ## CName ## _c(int *args, ptree *pt) {BOOL_RESULT = (Int)CName((int)(INT_ARG_1));} \
static char *descr_ ## CName ## _c(int *args, ptree *pt) \
{ \
  return(mus_format( BOOL_PT " = " #SName "(" CHR_PT ")", args[0], B2S(BOOL_RESULT), args[1], CHAR_ARG_1)); \
} \
static xen_value * SName(ptree *pt, xen_value **args, int num_args) \
{ \
  if (pt->constants == 1) \
    return(make_xen_value(R_BOOL, add_int_to_ptree(pt, CName((int)(pt->ints[args[1]->addr]))), R_CONSTANT)); \
  return(package(pt, R_BOOL, char_## CName ## _c, descr_ ## CName ## _c, args, 1)); \
}

CHARP(char_alphabetic_p, isalpha)
CHARP(char_numeric_p, isdigit)
CHARP(char_lower_case_p, islower)
CHARP(char_upper_case_p, isupper)
CHARP(char_whitespace_p, isspace)

#define CHARC(SName, CName) \
static void char_ ## CName ## _c(int *args, ptree *pt) {INT_RESULT = (int)CName(CHAR_ARG_1);} \
static char *descr_ ## CName ## _c(int *args, ptree *pt) \
{ \
  return(mus_format( CHR_PT " = " #SName "(" CHR_PT ")", args[0], (char)(CHAR_RESULT), args[1], CHAR_ARG_1)); \
} \
static xen_value * SName(ptree *pt, xen_value **args, int num_args) \
{ \
  if (pt->constants == 1) \
    return(make_xen_value(R_CHAR, add_int_to_ptree(pt, CName((char)(pt->ints[args[1]->addr]))), R_CONSTANT)); \
  return(package(pt, R_CHAR, char_## CName ## _c, descr_ ## CName ## _c, args, 1)); \
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
  xen_value *old_loc;
  for (i = 1; i <= num_args; i++)
    if (args[i])
      {
	old_loc = args[i];
	if (args[i]->constant == R_CONSTANT)
	  args[i] = make_xen_value(R_CHAR, add_int_to_ptree(pt, toupper((char)(pt->ints[args[i]->addr]))), R_CONSTANT);
	else 
	  {
	    args[i] = make_xen_value(R_CHAR, add_int_to_ptree(pt, 0), R_VARIABLE);
	    add_triple_to_ptree(pt, va_make_triple(char_toupper_c, descr_toupper_c, 2, args[i], old_loc));
	  }
	FREE(old_loc);
      }
  return(args);
}


/* ---------------- strings ---------------- */

static void string_n(int *args, ptree *pt)
{
  int i, n;
  n = pt->ints[args[1]];
  if (STRING_RESULT) FREE(STRING_RESULT);
  STRING_RESULT = (char *)CALLOC(n + 1, sizeof(char));
  for (i = 1; i <= n; i++) STRING_RESULT[i - 1] = (char)(pt->ints[args[i + 1]]);
}
static char *descr_string_n(int *args, ptree *pt)
{
  int i, n, len;
  char *buf, *temp;
  n = pt->ints[args[1]];
  len = 32 + 16 * n;
  buf = (char *)CALLOC(len, sizeof(char));
  temp = (char *)CALLOC(16, sizeof(char));
  sprintf(buf, STR_PT " = string(" CHR_PT , args[0], STRING_RESULT, args[2], (char)(pt->ints[args[2]]));
  for (i = 2; i <= n; i++)
    {
      sprintf(temp, ", " CHR_PT , args[i + 1], (char)(pt->ints[args[i + 1]]));
      buf = snd_strcat(buf, temp, &len);
    }
  FREE(temp);
  buf = snd_strcat(buf, ")", &len);
  return(buf);
}
static xen_value *string_1(ptree *pt, xen_value **args, int num_args)
{
  if (pt->constants == num_args)
    {
      int i;
      char *str;
      str = (char *)CALLOC(num_args + 1, sizeof(char));
      for (i = 1; i <= num_args; i++)
	str[i - 1] = (char)(pt->ints[args[i]->addr]);
      return(make_xen_value(R_STRING, add_string_to_ptree(pt, str), R_CONSTANT));
    }
  return(package_n(pt, R_STRING, string_n, descr_string_n, args, num_args));
}

static void strlen_1(int *args, ptree *pt) {INT_RESULT = snd_strlen(STRING_ARG_1);}
static char *descr_strlen_1(int *args, ptree *pt)
{
  return(mus_format( INT_PT " = string-length(" STR_PT ")", args[0], INT_RESULT, args[1], STRING_ARG_1));
}
static xen_value *string_length_1(ptree *pt, xen_value **args, int num_args)
{
  if (args[1]->constant == R_CONSTANT)
    return(make_xen_value(R_INT, add_int_to_ptree(pt, snd_strlen(pt->strs[args[1]->addr])), R_CONSTANT));
  return(package(pt, R_INT, strlen_1, descr_strlen_1, args, 1));
}

static void strcpy_1(int *args, ptree *pt) 
{
  if (STRING_RESULT) FREE(STRING_RESULT);
  STRING_RESULT = copy_string(STRING_ARG_1);
}
static char *descr_strcpy_1(int *args, ptree *pt) 
{
  return(mus_format( STR_PT " = string-copy(" STR_PT ")", args[0], STRING_RESULT, args[1], STRING_ARG_1));
}
static xen_value *string_copy_1(ptree *pt, xen_value **args, int num_args)
{
  if (args[1]->constant == R_CONSTANT)
    return(make_xen_value(R_STRING, add_string_to_ptree(pt, copy_string(pt->strs[args[1]->addr])), R_CONSTANT));
  return(package(pt, R_STRING, strcpy_1, descr_strcpy_1, args, 1));
}

static void strfill_1(int *args, ptree *pt) 
{
  int len;
  len = snd_strlen(STRING_RESULT);
  if (len > 0)
    memset((void *)(STRING_RESULT), (char)(CHAR_ARG_1), len);
}
static char *descr_strfill_1(int *args, ptree *pt) 
{
  return(mus_format( STR_PT " = string-fill!(" CHR_PT ")", args[0], STRING_RESULT, args[1], CHAR_ARG_1));
}
static xen_value *string_fill_1(ptree *pt, xen_value **args, int num_args)
{
  if (args[1]->constant == R_CONSTANT)
    return(run_warn("constant 1st arg to string-fill!"));
  add_triple_to_ptree(pt, va_make_triple(strfill_1, descr_strfill_1, 2, args[1], args[2])); /* shifts back one */
  return(make_xen_value(R_UNSPECIFIED, -1, R_CONSTANT));
}

static void strset_1(int *args, ptree *pt) {STRING_RESULT[INT_ARG_1] = (char)(CHAR_ARG_2);}
static char *descr_strset_1(int *args, ptree *pt) 
{
  return(mus_format( STR_PT " = string-set!(" INT_PT ", " CHR_PT ")", 
		    args[0], STRING_RESULT, args[1], INT_ARG_1, args[2], CHAR_ARG_2));
}
static xen_value *string_set_1(ptree *pt, xen_value **args, int num_args)
{
  if (args[1]->constant == R_CONSTANT)
    return(run_warn("constant 1st arg to string-set!"));
  add_triple_to_ptree(pt, va_make_triple(strset_1, descr_strset_1, 3, args[1], args[2], args[3])); /* shifts back one */
  return(make_xen_value(R_UNSPECIFIED, -1, R_CONSTANT));
}

static void strref_1(int *args, ptree *pt) {CHAR_RESULT = (char)(STRING_ARG_1[INT_ARG_2]);}
static char *descr_strref_1(int *args, ptree *pt) 
{
  return(mus_format( CHR_PT " = string-ref(" STR_PT ", " INT_PT ")", 
		    args[0], (INT_RESULT == 0) ? '@' : ((char)(CHAR_RESULT)), 
		    args[1], STRING_ARG_1, args[2], INT_ARG_2));
}
static xen_value *string_ref_1(ptree *pt, xen_value **args, int num_args)
{
  if ((args[1]->constant == R_CONSTANT) && (args[2]->constant == R_CONSTANT))
    return(make_xen_value(R_CHAR, add_int_to_ptree(pt, (int)(pt->strs[args[1]->addr][pt->ints[args[2]->addr]])), R_CONSTANT));
  return(package(pt, R_CHAR, strref_1, descr_strref_1, args, 2));
}


static char *int_vect_to_string(vect *v)
{
  int len, i, slen;
  char *buf;
  char flt[16];
  if (v == NULL) return(copy_string("#<vect: null>"));
  len = 8;
  if (len > v->length) len = v->length;
  slen = 64 + len * 8;
  buf = (char *)CALLOC(slen, sizeof(char));
  sprintf(buf, "#<vect[len=%d]:", v->length);
  if (len > 0)
    {
      for (i = 0; i < len; i++)
	{
	  mus_snprintf(flt, 16, " " INT_STR, v->data.ints[i]);
	  buf = snd_strcat(buf, flt, &slen);
	}
      if (v->length > 8)
	buf = snd_strcat(buf, " ...", &slen);
    }
  buf = snd_strcat(buf, ">", &slen);
  return(buf);
}

static char *clm_vect_to_string(vect *v)
{
  int len, i, slen;
  char *buf;
  char flt[16];
  if (v == NULL) return(copy_string("#<vect: null>"));
  len = 8;
  if (len > v->length) len = v->length;
  slen = 64 + len * 8;
  buf = (char *)CALLOC(slen, sizeof(char));
  sprintf(buf, "#<vect[len=%d]:", v->length);
  if (len > 0)
    {
      for (i = 0; i < len; i++)
	{
	  mus_snprintf(flt, 16, " #<%s>", (v->data.gens[i]) ? mus_name(v->data.gens[i]) : "#f");
	  buf = snd_strcat(buf, flt, &slen);
	}
      if (v->length > 8)
	buf = snd_strcat(buf, " ...", &slen);
    }
  buf = snd_strcat(buf, ">", &slen);
  return(buf);
}

static char *vct_vect_to_string(vect *v)
{
  int len, i, slen;
  char *buf;
  char flt[32];
  if (v == NULL) return(copy_string("#<vect: null>"));
  len = 8;
  if (len > v->length) len = v->length;
  slen = 64 + len * 8;
  buf = (char *)CALLOC(slen, sizeof(char));
  sprintf(buf, "#<vect[len=%d]:", v->length);
  if (len > 0)
    {
      for (i = 0; i < len; i++)
	{
	  if (v->data.vcts[i])
	    mus_snprintf(flt, 32, " #<vct[len=%d]>", (v->data.vcts[i])->length);
	  else mus_snprintf(flt, 32, " #f");
	  buf = snd_strcat(buf, flt, &slen);
	}
      if (v->length > 8)
	buf = snd_strcat(buf, " ...", &slen);
    }
  buf = snd_strcat(buf, ">", &slen);
  return(buf);
}

static void display_str(int *args, ptree *pt) {fprintf(stderr, "%s", STRING_ARG_1);}
static char *descr_display_str(int *args, ptree *pt) {return(mus_format("display(" STR_PT ")", args[1], STRING_ARG_1));}
static void display_int(int *args, ptree *pt) {fprintf(stderr, INT_STR, INT_ARG_1);}
static char *descr_display_int(int *args, ptree *pt) {return(mus_format("display(" INT_PT ")", args[1], INT_ARG_1));}
static void display_flt(int *args, ptree *pt) {fprintf(stderr, "%.6f", FLOAT_ARG_1);}
static char *descr_display_flt(int *args, ptree *pt) {return(mus_format("display(" FLT_PT ")", args[1], FLOAT_ARG_1));}
static void display_lst(int *args, ptree *pt) {fprintf(stderr, "%s", XEN_AS_STRING(RXEN_ARG_1));}
static char *descr_display_lst(int *args, ptree *pt) {return(mus_format("display(" LST_PT ")", args[1], DESC_RXEN_ARG_1));}
static void display_symbol(int *args, ptree *pt) {fprintf(stderr, "%s", XEN_AS_STRING(RXEN_ARG_1));}
static char *descr_display_symbol(int *args, ptree *pt) {return(mus_format("display(" KEY_PT ")", args[1], DESC_RXEN_ARG_1));}
static void display_key(int *args, ptree *pt) {fprintf(stderr, "%s", XEN_AS_STRING(RXEN_ARG_1));}
static char *descr_display_key(int *args, ptree *pt) {return(mus_format("display(" KEY_PT ")", args[1], DESC_RXEN_ARG_1));}
static void display_clm(int *args, ptree *pt) {fprintf(stderr, "%s", mus_describe(CLM_ARG_1));}
static char *descr_display_clm(int *args, ptree *pt) {return(mus_format("display(" CLM_PT ")", args[1], DESC_CLM_ARG_1));}
static void display_vct(int *args, ptree *pt) 
{
  char *v = NULL;
  v = vct_to_string(VCT_ARG_1);
  if (v)
    {
      fprintf(stderr, "%s", v);
      FREE(v);
    }
}
static char *descr_display_vct(int *args, ptree *pt) {return(mus_format("display(" VCT_PT ")", args[1], DESC_VCT_ARG_1));}

static void display_int_vect(int *args, ptree *pt) 
{
  char *buf = NULL;
  buf = int_vect_to_string(VECT_ARG_1);
  if (buf) {fprintf(stderr, "%s", buf); FREE(buf); }
}
static char *descr_display_vect(int *args, ptree *pt) {return(mus_format("display(" VECT_PT ")", args[1], DESC_VECT_ARG_1));}
static void display_clm_vect(int *args, ptree *pt) 
{
  char *buf = NULL;
  buf = clm_vect_to_string(VECT_ARG_1);
  if (buf) {fprintf(stderr, "%s", buf); FREE(buf); }
}
static void display_vct_vect(int *args, ptree *pt) 
{
  char *buf = NULL;
  buf = vct_vect_to_string(VECT_ARG_1);
  if (buf) {fprintf(stderr, "%s", buf); FREE(buf); }
}

static void display_rd(int *args, ptree *pt) {char *buf = NULL; fprintf(stderr, "%s", buf = sf_to_string(READER_ARG_1)); FREE(buf);}
static char *descr_display_rd(int *args, ptree *pt) {return(mus_format("display(" RD_PT ")", args[1], DESC_READER_ARG_1));}
static void display_mf(int *args, ptree *pt) {char *buf = NULL; fprintf(stderr, "%s", buf = run_mix_reader_to_string(MIX_READER_ARG_1)); FREE(buf);}
static char *descr_display_mf(int *args, ptree *pt) {return(mus_format("display(" MF_PT ")", args[1], DESC_MIX_READER_ARG_1));}
static void display_tf(int *args, ptree *pt) {char *buf = NULL; fprintf(stderr, "%s", buf = run_track_reader_to_string(TRACK_READER_ARG_1)); FREE(buf);}
static char *descr_display_tf(int *args, ptree *pt) {return(mus_format("display(" TF_PT ")", args[1], DESC_TRACK_READER_ARG_1));}
static void display_sd(int *args, ptree *pt) {char *buf = NULL; fprintf(stderr, "%s", buf = sound_data_to_string(SOUND_DATA_ARG_1)); FREE(buf);}
static char *descr_display_sd(int *args, ptree *pt) {return(mus_format("display(" SD_PT ")", args[1], DESC_SOUND_DATA_ARG_1));}
static void display_chr(int *args, ptree *pt) {fprintf(stderr, "%c", (char)(INT_ARG_1));}
static char *descr_display_chr(int *args, ptree *pt) {return(mus_format("display(" CHR_PT ")", args[1], CHAR_ARG_1));}
static void display_bool(int *args, ptree *pt) {fprintf(stderr, "%s", B2S(INT_ARG_1));}
static char *descr_display_bool(int *args, ptree *pt) {return(mus_format("display(" BOOL_PT ")", args[1], B2S(INT_ARG_1)));}
static void display_con(int *args, ptree *pt) {fprintf(stderr, GO_PT, args[1]);}
static char *descr_display_con(int *args, ptree *pt) {return(mus_format("display(" GO_PT ")", args[1]));}
static void display_func(int *args, ptree *pt) 
{
  char *p = NULL;
  p = describe_ptree(FNC_ARG_1);
  if (p)
    {
      fprintf(stderr, "%s", p);
      FREE(p);
    }
}
static char *descr_display_func(int *args, ptree *pt) {return(mus_format("display(" FNC_PT ")", args[1], DESC_FNC_ARG_1));}
static xen_value *display_1(ptree *pt, xen_value **args, int num_args)
{
  switch (args[1]->type)
    {
    case R_STRING:     return(package(pt, R_BOOL, display_str, descr_display_str, args, 1));   break;
    case R_INT:        return(package(pt, R_BOOL, display_int, descr_display_int, args, 1));   break;
    case R_FLOAT:      return(package(pt, R_BOOL, display_flt, descr_display_flt, args, 1));   break;
    case R_CLM:        return(package(pt, R_BOOL, display_clm, descr_display_clm, args, 1));   break;
    case R_PAIR:
    case R_LIST:       return(package(pt, R_BOOL, display_lst, descr_display_lst, args, 1));   break;
    case R_SYMBOL:     return(package(pt, R_BOOL, display_symbol, descr_display_symbol, args, 1));break;
    case R_KEYWORD:    return(package(pt, R_BOOL, display_key, descr_display_key, args, 1));   break;
    case R_READER:     return(package(pt, R_BOOL, display_rd, descr_display_rd, args, 1));     break;
    case R_MIX_READER: return(package(pt, R_BOOL, display_mf, descr_display_mf, args, 1));     break;
    case R_TRACK_READER: return(package(pt, R_BOOL, display_tf, descr_display_tf, args, 1));   break;
    case R_FLOAT_VECTOR:
    case R_VCT:        return(package(pt, R_BOOL, display_vct, descr_display_vct, args, 1));   break;
    case R_SOUND_DATA: return(package(pt, R_BOOL, display_sd, descr_display_sd, args, 1));     break;
    case R_BOOL:       return(package(pt, R_BOOL, display_bool, descr_display_bool, args, 1)); break;
    case R_CHAR:       return(package(pt, R_BOOL, display_chr, descr_display_chr, args, 1));   break;
    case R_GOTO:       return(package(pt, R_BOOL, display_con, descr_display_con, args, 1));   break;
    case R_FUNCTION:   return(package(pt, R_BOOL, display_func, descr_display_func, args, 1)); break;
    case R_CLM_VECTOR: return(package(pt, R_BOOL, display_clm_vect, descr_display_vect, args, 1)); break;
    case R_VCT_VECTOR: return(package(pt, R_BOOL, display_vct_vect, descr_display_vect, args, 1)); break;
    case R_INT_VECTOR: return(package(pt, R_BOOL, display_int_vect, descr_display_vect, args, 1)); break;
    default:
      if (args[1]->type > R_ANY)
	return(package(pt, R_BOOL, display_lst, descr_display_lst, args, 1));
      break;
    }
  return(NULL);
}

static xen_value *symbol2string_1(ptree *pt, xen_value **args, int num_args)
{
  return(make_xen_value(R_STRING, 
			add_string_to_ptree(pt, copy_string(XEN_SYMBOL_TO_C_STRING(pt->xens[args[1]->addr]))),
			R_CONSTANT));
}

static void snd_print_s(int *args, ptree *pt) {listener_append(STRING_ARG_1);}
static char *descr_snd_print_s(int *args, ptree *pt) {return(mus_format("snd_print(" STR_PT ")", args[1], STRING_ARG_1));}
static xen_value *snd_print_1(ptree *pt, xen_value **args, int num_args) {return(package(pt, R_BOOL, snd_print_s, descr_snd_print_s, args, 1));}

static void snd_warning_s(int *args, ptree *pt) {snd_warning(STRING_ARG_1);}
static char *descr_snd_warning_s(int *args, ptree *pt) {return(mus_format("snd_warning(" STR_PT ")", args[1], STRING_ARG_1));}
static xen_value *snd_warning_1(ptree *pt, xen_value **args, int num_args) {return(package(pt, R_BOOL, snd_warning_s, descr_snd_warning_s, args, 1));}

static void strmake_c(int *args, ptree *pt, char c) 
{
  int i, n;
  n = INT_ARG_1;
  if (STRING_RESULT) FREE(STRING_RESULT);
  /* this should be safe because we don't allow (set! str str1) aliasing */
  STRING_RESULT = (char *)CALLOC(n + 1, sizeof(char));
  for (i = 1; i <= n; i++) STRING_RESULT[i - 1] = c;
}
static void strmake_1(int *args, ptree *pt) {strmake_c(args, pt, ' ');}
static void strmake_2(int *args, ptree *pt) {strmake_c(args, pt, CHAR_ARG_2);}
static char *descr_strmake_1(int *args, ptree *pt) 
{
  return(mus_format( STR_PT " = make-string(" INT_PT ")", args[0], STRING_RESULT, args[1], INT_ARG_1));
}
static char *descr_strmake_2(int *args, ptree *pt) 
{
  return(mus_format( STR_PT " = make-string(" INT_PT ", " CHR_PT ")", args[0], STRING_RESULT, args[1], INT_ARG_1, args[1], CHAR_ARG_2));
}
static xen_value *make_string_1(ptree *pt, xen_value **args, int num_args)
{
  if (num_args == 1)
    return(package(pt, R_STRING, strmake_1, descr_strmake_1, args, 1));
  return(package(pt, R_STRING, strmake_2, descr_strmake_2, args, 2));
}

static char *substring(const char *str, int start, int end)
{
  int i, len;
  char *newstr;
  len = end - start;
  if (len <= 0) return((char *)CALLOC(1, sizeof(char)));
  newstr = (char *)CALLOC(len + 1, sizeof(char));
  for (i = 0; i < len; i++) newstr[i] = str[i + start];
  return(newstr);
}
static void substr_1(int *args, ptree *pt) 
{
  int i, start, end, len, arg_len;
  start = pt->ints[args[2]];
  end = pt->ints[args[3]];
  arg_len = snd_strlen(STRING_ARG_1);
  if (arg_len < end) end = arg_len; /* should throw run-time error technically */
  len = end - start;
  if (STRING_RESULT) FREE(STRING_RESULT);
  if (len <= 0) 
    STRING_RESULT = (char *)CALLOC(1, sizeof(char));
  else
    {
      STRING_RESULT = (char *)CALLOC(len + 1, sizeof(char));
      for (i = 0; i < len; i++)
	STRING_RESULT[i] = STRING_ARG_1[i + start];
    }
}
static char *descr_substr_1(int *args, ptree *pt) 
{
  return(mus_format( STR_PT " = substring(" STR_PT ", " INT_PT ", " INT_PT ")", 
		    args[0], STRING_RESULT, args[1], STRING_ARG_1,
		    args[2], INT_ARG_2, args[3], INT_ARG_3));
}
static xen_value *substring_1(ptree *pt, xen_value **args, int num_args)
{
  if (pt->constants == 3)
    {
      /* parse time substring -- can check bounds here */
      int beg, end, len;
      len = snd_strlen(pt->strs[args[1]->addr]);
      beg = pt->ints[args[2]->addr];
      end = pt->ints[args[3]->addr];
      if ((beg <= len) && (end <= len))
	return(make_xen_value(R_STRING, 
			      add_string_to_ptree(pt, substring(pt->strs[args[1]->addr], beg, end)),
			      R_CONSTANT));
      return(run_warn("substring: args out of range"));
    }
  return(package(pt, R_STRING, substr_1, descr_substr_1, args, 3));
}


static char *descr_strn(int *args, ptree *pt, const char *which, bool bool_result)
{
  int i, n, len;
  char *buf, *temp;
  n = pt->ints[args[1]];
  len = 1024;
  buf = (char *)CALLOC(len, sizeof(char));
  temp = (char *)CALLOC(1024, sizeof(char));
  if (bool_result)
    mus_snprintf(buf, 1024, BOOL_PT " = %s(" STR_PT , args[0], B2S(BOOL_RESULT), which, args[2], pt->strs[args[2]]);
  else mus_snprintf(buf, 1024, STR_PT " = %s(" STR_PT , args[0], STRING_RESULT, which, args[2], pt->strs[args[2]]);
  for (i = 2; i <= n; i++)
    {
      mus_snprintf(temp, 1024, ", " STR_PT , args[i + 1], pt->strs[args[i + 1]]);
      buf = snd_strcat(buf, temp, &len);
    }
  FREE(temp);
  buf = snd_strcat(buf, ")", &len);
  return(buf);
}

static char *string_append(ptree *pt, xen_value **args, int num_args)
{
  int i, len = 0;
  char *str;
  for (i = 0; i < num_args; i++)
    len += snd_strlen(pt->strs[args[i + 1]->addr]);
  str = (char *)CALLOC(len + 1, sizeof(char));
  for (i = 0; i < num_args; i++)
    if (pt->strs[args[i + 1]->addr])
      strcat(str, pt->strs[args[i + 1]->addr]);
  return(str);
}
static void appendstr_n(int *args, ptree *pt) 
{
  int i, n, len = 0;
  if (STRING_RESULT) FREE(STRING_RESULT);
  n = pt->ints[args[1]];
  for (i = 1; i <= n; i++) len += snd_strlen(pt->strs[args[i + 1]]);
  STRING_RESULT = (char *)CALLOC(len + 1, sizeof(char));
  for (i = 1; i <= n; i++) 
    if (pt->strs[args[i + 1]])
      strcat(STRING_RESULT, pt->strs[args[i + 1]]);
}
static char *descr_appendstr_n(int *args, ptree *pt)  {return(descr_strn(args, pt, "string-append", false));}
static xen_value *string_append_1(ptree *pt, xen_value **args, int num_args)
{
  if (num_args == 0)
    return(make_xen_value(R_STRING, add_string_to_ptree(pt, (char *)CALLOC(1, sizeof(char))), R_CONSTANT));
  if (num_args == pt->constants)
    return(make_xen_value(R_STRING, add_string_to_ptree(pt, string_append(pt, args, num_args)), R_CONSTANT));
  if (num_args == 1)
    return(copy_xen_value(args[1]));
  return(package_n(pt, R_STRING, appendstr_n, descr_appendstr_n, args, num_args));
}

#define STR_REL_OP(CName, SName, COp) \
static bool str_ ## CName ## _n(ptree *pt, xen_value **args, int num_args) \
{ \
  int i; \
  for (i = 2; i <= num_args; i++) \
    if (strcmp(pt->strs[args[i - 1]->addr], pt->strs[args[i]->addr]) COp 0) \
      return(false); \
  return(true); \
} \
static char *descr_string_ ## CName ## _n(int *args, ptree *pt) {return(descr_strn(args, pt, #SName, true));} \
static void string_ ## CName ## _n(int *args, ptree *pt) \
{ \
  int i, n; \
  n = pt->ints[args[1]]; \
  BOOL_RESULT = (Int)true; \
  for (i = 2; i <= n; i++) \
    if (strcmp(pt->strs[args[i]], pt->strs[args[i + 1]]) COp 0) \
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
	  if ((lasts) && (strcmp(lasts, pt->strs[args[i]->addr]) COp 0)) \
	    return(make_xen_value(R_BOOL, add_int_to_ptree(pt, (Int)false), R_CONSTANT)); \
	  lasts = pt->strs[args[i]->addr]; \
	} \
  return(package_n(pt, R_BOOL, string_ ## CName ## _n, descr_string_ ## CName ## _n, args, num_args)); \
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
    if (strcasecmp(pt->strs[args[i - 1]->addr], pt->strs[args[i]->addr]) COp 0) \
      return(false); \
  return(true); \
} \
static char *descr_string_ci_ ## CName ## _n(int *args, ptree *pt) {return(descr_strn(args, pt, #SName, true));} \
static void string_ci_ ## CName ## _n(int *args, ptree *pt) \
{ \
  int i, n; \
  n = pt->ints[args[1]]; \
  BOOL_RESULT = (Int)true; \
  for (i = 2; i <= n; i++) \
    if (strcasecmp(pt->strs[args[i]], pt->strs[args[i + 1]]) COp 0) \
      { \
	BOOL_RESULT = (Int)false; \
	break; \
      } \
} \
static xen_value *string_ci_ ## CName ## _1(ptree *pt, xen_value **args, int num_args) \
{ \
  if (num_args <= 1) return(make_xen_value(R_BOOL, add_int_to_ptree(pt, (Int)true), R_CONSTANT)); \
  if (num_args == pt->constants) return(make_xen_value(R_BOOL, add_int_to_ptree(pt, str_ci_ ## CName ## _n(pt, args, num_args)), R_CONSTANT)); \
  return(package_n(pt, R_BOOL, string_ci_ ## CName ## _n, descr_string_ci_ ## CName ## _n, args, num_args)); \
}

STR_CI_REL_OP(eq, string=?, !=)
STR_CI_REL_OP(geq, string>=?, <)
STR_CI_REL_OP(leq, string<=?, >)
STR_CI_REL_OP(gt, string>?, <=)
STR_CI_REL_OP(lt, string<?, >=)


/* ---------------- number->string ---------------- */

/* fallback on Guile's number->string */
static char *f2s_1(Double n) {return(copy_string(DOUBLE_TO_STRING(n)));}
static char *f2s_2(Double n, int rad) {return(copy_string(DOUBLE_TO_STRING_WITH_RADIX(n, rad)));}
static char *i2s_1(Int n) {return(copy_string(INTEGER_TO_STRING(n)));}
static char *i2s_2(Int n, int rad) {return(copy_string(INTEGER_TO_STRING_WITH_RADIX(n, rad)));}
static void number2string_f1(int *args, ptree *pt) {if (STRING_RESULT) FREE(STRING_RESULT); STRING_RESULT = f2s_1(FLOAT_ARG_1);}
static void number2string_f2(int *args, ptree *pt) {if (STRING_RESULT) FREE(STRING_RESULT); STRING_RESULT = f2s_2(FLOAT_ARG_1, INT_ARG_2);}
static void number2string_i1(int *args, ptree *pt) {if (STRING_RESULT) FREE(STRING_RESULT); STRING_RESULT = i2s_1(INT_ARG_1);}
static void number2string_i2(int *args, ptree *pt) {if (STRING_RESULT) FREE(STRING_RESULT); STRING_RESULT = i2s_2(INT_ARG_1, INT_ARG_2);}
static char *descr_number2string_f1(int *args, ptree *pt) 
{
  return(mus_format( STR_PT " = number->string(" FLT_PT ")", args[0], STRING_RESULT, args[1], FLOAT_ARG_1));
}
static char *descr_number2string_f2(int *args, ptree *pt) 
{
  return(mus_format( STR_PT " = number->string(" FLT_PT ", " INT_PT ")", args[0], STRING_RESULT, args[1], FLOAT_ARG_1, args[2], INT_ARG_2));
}
static char *descr_number2string_i1(int *args, ptree *pt) 
{
  return(mus_format( STR_PT " = number->string(" INT_PT ")", args[0], STRING_RESULT, args[1], INT_ARG_1));
}
static char *descr_number2string_i2(int *args, ptree *pt) 
{
  return(mus_format( STR_PT " = number->string(" INT_PT ", " INT_PT ")", args[0], STRING_RESULT, args[1], INT_ARG_1, args[2], INT_ARG_2));
}
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
	    return(package(prog, R_STRING, number2string_i1, descr_number2string_i1, args, 1));
	  return(package(prog, R_STRING, number2string_f1, descr_number2string_f1, args, 1));
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
		return(package(prog, R_STRING, number2string_i2, descr_number2string_i2, args, 2));
	      return(package(prog, R_STRING, number2string_f2, descr_number2string_f2, args, 2));
	    }
	}
    }
  return(NULL);
}

/* ---------------- user-defined function call ---------------- */

static void funcall_nf(int *args, ptree *pt) 
{
  int i;
  ptree *func;
  xen_value *fres;
  func = ((ptree **)(pt->fncs))[args[1]];
  fres = func->result;
  for (i = 0; i < func->arity; i++)
    switch (func->arg_types[i])
      {
      case R_FLOAT: 
	pt->dbls[func->args[i]] = pt->dbls[args[i + 2]]; 
	break;
      case R_STRING: 
	if (pt->strs[func->args[i]]) FREE(pt->strs[func->args[i]]);
	pt->strs[func->args[i]] = copy_string(pt->strs[args[i + 2]]); 
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
      case R_CLM_VECTOR: 
      case R_INT_VECTOR: 
      case R_VCT_VECTOR: 
 	pt->vects[func->args[i]] = pt->vects[args[i + 2]]; 
 	break;
      case R_FUNCTION: 
 	pt->fncs[func->args[i]] = pt->fncs[args[i + 2]]; 
 	break;
      case R_SYMBOL: 
      case R_KEYWORD:
      case R_LIST:
      case R_PAIR:
	/* fprintf(stderr, "func set arg %d to %s\n", args[i], XEN_AS_STRING(pt->xens[args[i + 2]])); */
 	pt->xens[func->args[i]] = pt->xens[args[i + 2]]; 
 	break;
      case R_READER: 
 	pt->readers[func->args[i]] = pt->readers[args[i + 2]]; 
 	break;
      case R_MIX_READER: 
 	pt->mix_readers[func->args[i]] = pt->mix_readers[args[i + 2]]; 
 	break;
      case R_TRACK_READER: 
 	pt->track_readers[func->args[i]] = pt->track_readers[args[i + 2]]; 
 	break;
      default:      
	pt->ints[func->args[i]] = pt->ints[args[i + 2]]; 
	break;
      }
  eval_embedded_ptree(func, pt);
  switch (fres->type)
    {
    case R_INT:   
      INT_RESULT = pt->ints[fres->addr];   
      break;
    case R_FLOAT: 
      FLOAT_RESULT = pt->dbls[fres->addr]; 
      break;
    case R_STRING: 
      if (STRING_RESULT) FREE(STRING_RESULT);
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
    case R_CLM_VECTOR:   
    case R_INT_VECTOR:   
    case R_VCT_VECTOR:   
      VECT_RESULT = pt->vects[fres->addr];   
      break;
    case R_SYMBOL: 
    case R_KEYWORD:
    case R_LIST:
    case R_PAIR:
      XEN_RESULT = pt->xens[fres->addr];
      break;
    case R_FUNCTION:   
      FNC_RESULT = ((ptree **)(pt->fncs))[fres->addr];   
      break;
    case R_READER:   
      READER_RESULT = pt->readers[fres->addr];   
      break;
    case R_MIX_READER:   
      MIX_READER_RESULT = pt->mix_readers[fres->addr];   
      break;
    case R_TRACK_READER:   
      TRACK_READER_RESULT = pt->track_readers[fres->addr];   
      break;
    default:      
      INT_RESULT = pt->ints[fres->addr];   
      break;
    }
}
static char *descr_funcall_nf(int *args, ptree *pt) 
{
  return(mus_format("%d = funcall(" FNC_PT " ...)", args[0], args[1], DESC_FNC_ARG_1));
}
static xen_value *funcall_n(ptree *prog, xen_value **args, int num_args, xen_value *sf)
{
  ptree *func;
  int i;
  xen_value **new_args;
  xen_value *fres;
  func = ((ptree **)(prog->fncs))[sf->addr];
  if (func->arity != num_args) return(run_warn("wrong number of args (%d) for func", num_args));
  fres = func->result;
  new_args = (xen_value **)CALLOC(num_args + 2, sizeof(xen_value *));
  for (i = 1; i <= num_args; i++)
    new_args[i + 1] = args[i];
  new_args[1] = sf;
  new_args[0] = add_empty_var_to_ptree(prog, fres->type);
  args[0] = new_args[0];
  add_triple_to_ptree(prog, make_triple(funcall_nf, descr_funcall_nf, new_args, num_args + 2));
  FREE(new_args);
  return(args[0]);
}


/* ---------------- non-local goto via continuation ---------------- */

static xen_value *goto_0(ptree *prog, xen_value **args, xen_value *sf)
{
  if (args[0]) FREE(args[0]);
  args[0] = make_xen_value(R_BOOL, add_int_to_ptree(prog, sf->addr), R_VARIABLE);
  add_triple_to_ptree(prog, va_make_triple(jump_indirect, descr_jump_indirect, 1, args[0]));
  return(args[0]);
}

/* ---------------- simple snd ops ---------------- */
#define INT_INT_OP(CName) \
static void CName ## _i(int *args, ptree *pt) {INT_RESULT = CName(INT_ARG_1);} \
static char *descr_ ## CName ## _i(int *args, ptree *pt) {return(mus_format( INT_PT " = " #CName "(" INT_PT ")", args[0], INT_RESULT, args[1], INT_ARG_1));} \
static xen_value * CName ## _1(ptree *prog, xen_value **args, int num_args) {return(package(prog, R_INT, CName ## _i, descr_ ##CName ## _i, args, 1));}

#define FLOAT_INT_OP(CName) \
static void CName ## _i(int *args, ptree *pt) {FLOAT_RESULT = CName(INT_ARG_1);} \
static char *descr_ ## CName ## _i(int *args, ptree *pt) {return(mus_format( FLT_PT " = " #CName "(" INT_PT ")", args[0], FLOAT_RESULT, args[1], INT_ARG_1));} \
static xen_value * CName ## _1(ptree *prog, xen_value **args, int num_args) {return(package(prog, R_FLOAT, CName ## _i, descr_ ##CName ## _i, args, 1));}

#define BOOL_INT_OP(CName) \
static void CName ## _i(int *args, ptree *pt) {BOOL_RESULT = CName(INT_ARG_1);} \
static char *descr_ ## CName ## _i(int *args, ptree *pt) {return(mus_format( BOOL_PT " = " #CName "(" INT_PT ")", args[0], B2S(BOOL_RESULT), args[1], INT_ARG_1));} \
static xen_value * CName ## _1(ptree *prog, xen_value **args, int num_args) {return(package(prog, R_BOOL, CName ## _i, descr_ ##CName ## _i, args, 1));}

#define INT_VOID_OP(CName) \
static void CName ## _i(int *args, ptree *pt) {INT_RESULT = CName();} \
static char *descr_ ## CName ## _i(int *args, ptree *pt) {return(mus_format( INT_PT " = " #CName "()", args[0], INT_RESULT));} \
static xen_value * CName ## _1(ptree *prog, xen_value **args, int num_args) {return(package(prog, R_INT, CName ## _i, descr_ ##CName ## _i, args, 0));}

/* must copy string here because free_ptree will free all temp strings */
#define STR_VOID_OP(CName) \
static void CName ## _i(int *args, ptree *pt) {if (STRING_RESULT) FREE(STRING_RESULT); STRING_RESULT = copy_string(CName());} \
static char *descr_ ## CName ## _i(int *args, ptree *pt) {return(mus_format( STR_PT " = " #CName "()", args[0], STRING_RESULT));} \
static xen_value * CName ## _1(ptree *prog, xen_value **args, int num_args) {return(package(prog, R_STRING, CName ## _i, descr_ ##CName ## _i, args, 0));}

INT_INT_OP(mus_bytes_per_sample)
bool r_sound_p(int i);
BOOL_INT_OP(r_sound_p);
BOOL_INT_OP(mix_ok);
BOOL_INT_OP(track_p);
BOOL_INT_OP(region_ok);
bool r_mark_p(int n);
BOOL_INT_OP(r_mark_p);
INT_VOID_OP(mark_sync_max);
INT_VOID_OP(selection_chans);
static char *r_temp_dir(void) {return(temp_dir(ss));}
static char *r_save_dir(void) {return(save_dir(ss));}
STR_VOID_OP(r_temp_dir);
STR_VOID_OP(r_save_dir);
off_t r_mark_sync(int n);
off_t r_mark_sample(int n);
INT_INT_OP(r_mark_sync);
INT_INT_OP(r_mark_sample);
off_t r_mix_position(int n);
int r_mix_chans(int n);
off_t r_mix_frames(int n);
bool r_mix_locked(int n);
bool r_mix_inverted(int n);
int r_mix_track(int n);
INT_INT_OP(r_mix_chans);
INT_INT_OP(r_mix_position);
INT_INT_OP(r_mix_frames);
INT_INT_OP(r_mix_track);
BOOL_INT_OP(r_mix_inverted);
BOOL_INT_OP(r_mix_locked);
INT_INT_OP(track_chans);

Float r_mix_speed(int n);
#define r_track_speed(Trk) track_dialog_track_speed(Trk)
#define r_track_tempo(Trk) track_dialog_track_tempo(Trk)
#define r_track_amp(Trk) track_dialog_track_amp(Trk)
#define r_track_track(Trk) track_dialog_track_track(Trk)

INT_INT_OP(r_track_track);
FLOAT_INT_OP(r_mix_speed);
FLOAT_INT_OP(r_track_speed);
FLOAT_INT_OP(r_track_tempo);
FLOAT_INT_OP(r_track_amp);
INT_INT_OP(region_chans);
INT_INT_OP(region_srate);
INT_INT_OP(region_len);
FLOAT_INT_OP(region_maxamp);

char *r_mark_name(int n);


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
  return(NULL);
}

static chan_info *run_get_cp(int offset, int *args, Int *ints)
{
  snd_info *sp;
  int cpint;
  sp = run_get_sp(offset, args, ints);
  if (sp)
    {
      cpint = ints[args[offset + 1]];
      if (cpint == -1)
	return(any_selected_channel(sp));
      else
	if (cpint < sp->nchans)
	  return(sp->chans[cpint]);
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

static char *descr_edit_position_i(int *args, ptree *pt) 
{
  return(mus_format( INT_PT " = edit_position(" INT_PT ", " INT_PT ")", args[0], INT_RESULT, args[1], INT_ARG_1, args[2], INT_ARG_2));
}

static xen_value *edit_position_1(ptree *pt, xen_value **args, int num_args)
{
  xen_value *true_args[3];
  xen_value *rtn;
  int k;
  run_opt_arg(pt, args, num_args, 1, true_args);
  run_opt_arg(pt, args, num_args, 2, true_args);
  true_args[0] = args[0];
  rtn = package(pt, R_INT, edit_position_i, descr_edit_position_i, true_args, 2);
  for (k = num_args + 1; k <= 2; k++) FREE(true_args[k]);
  return(rtn);
}

/* ---------------- cursor ---------------- */

static void cursor_i(int *args, ptree *pt) 
{
  chan_info *cp; 
  cp = run_get_cp(1, args, pt->ints);
  if (cp) INT_RESULT = CURSOR(cp);
}

static char *descr_cursor_i(int *args, ptree *pt) 
{
  return(mus_format( INT_PT " = cursor(" INT_PT ", " INT_PT ")", args[0], INT_RESULT, args[1], INT_ARG_1, args[2], INT_ARG_2));
}

static xen_value *cursor_1(ptree *pt, xen_value **args, int num_args)
{
  xen_value *true_args[3];
  xen_value *rtn;
  int k;
  run_opt_arg(pt, args, num_args, 1, true_args);
  run_opt_arg(pt, args, num_args, 2, true_args);
  true_args[0] = args[0];
  rtn = package(pt, R_INT, cursor_i, descr_cursor_i, true_args, 2);
  for (k = num_args + 1; k <= 2; k++) FREE(true_args[k]);
  return(rtn);
}


/* ---------------- add_mark ---------------- */

static void add_mark_i(int *args, ptree *pt) 
{
  chan_info *cp; 
  mark *m = NULL;
  cp = run_get_cp(2, args, pt->ints);
  if (cp) 
    {
      m = add_mark(INT_ARG_1, NULL, cp);
      if (m) INT_RESULT = mark_id(m);
    }
}

static char *descr_add_mark_i(int *args, ptree *pt) 
{
  return(mus_format( INT_PT " = add-mark(" INT_PT ", " INT_PT ", " INT_PT ")", 
		     args[0], INT_RESULT, args[1], INT_ARG_1, args[2], INT_ARG_2, args[3], INT_ARG_3));
}

static xen_value *add_mark_1(ptree *pt, xen_value **args, int num_args)
{
  xen_value *true_args[4];
  xen_value *rtn;
  int k;
  run_opt_arg(pt, args, num_args, 2, true_args);
  run_opt_arg(pt, args, num_args, 3, true_args);
  true_args[0] = args[0];
  true_args[1] = args[1];
  rtn = package(pt, R_INT, add_mark_i, descr_add_mark_i, true_args, 3);
  for (k = num_args + 1; k <= 3; k++) FREE(true_args[k]);
  return(rtn);
}


/* ---------------- maxamp ---------------- */

static void maxamp_f(int *args, ptree *pt) 
{
  chan_info *cp; 
  cp = run_get_cp(1, args, pt->ints);
  if (cp) FLOAT_RESULT = get_maxamp(cp->sound, cp, AT_CURRENT_EDIT_POSITION);
}

static char *descr_maxamp_f(int *args, ptree *pt) 
{
  return(mus_format( FLT_PT " = maxamp(" INT_PT ", " INT_PT ")", args[0], FLOAT_RESULT, args[1], INT_ARG_1, args[2], INT_ARG_2));
}

static xen_value *maxamp_1(ptree *pt, xen_value **args, int num_args)
{
  xen_value *true_args[3];
  xen_value *rtn;
  int k;
  run_opt_arg(pt, args, num_args, 1, true_args);
  run_opt_arg(pt, args, num_args, 2, true_args);
  true_args[0] = args[0];
  rtn = package(pt, R_FLOAT, maxamp_f, descr_maxamp_f, true_args, 2);
  for (k = num_args + 1; k <= 2; k++) FREE(true_args[k]);
  return(rtn);
}

/* ---------------- srate ---------------- */

static void srate_i(int *args, ptree *pt) 
{
  snd_info *sp;
  sp = run_get_sp(1, args, pt->ints);
  if (sp) INT_RESULT = SND_SRATE(sp);
}

static char *descr_srate_i(int *args, ptree *pt) 
{
  return(mus_format( INT_PT " = srate(" INT_PT ")", args[0], INT_RESULT, args[1], INT_ARG_1));
}

static xen_value *srate_1(ptree *pt, xen_value **args, int num_args)
{
  xen_value *true_args[2];
  xen_value *rtn;
  int k;
  run_opt_arg(pt, args, num_args, 1, true_args);
  true_args[0] = args[0];
  rtn = package(pt, R_INT, srate_i, descr_srate_i, true_args, 1);
  for (k = num_args + 1; k <= 1; k++) FREE(true_args[k]);
  return(rtn);
}


/* ---------------- channels ---------------- */

static void channels_i(int *args, ptree *pt) 
{
  snd_info *sp;
  sp = run_get_sp(1, args, pt->ints);
  if (sp) INT_RESULT = sp->nchans;
}

static char *descr_channels_i(int *args, ptree *pt) 
{
  return(mus_format( INT_PT " = channels(" INT_PT ")", args[0], INT_RESULT, args[1], INT_ARG_1));
}

static xen_value *channels_1(ptree *pt, xen_value **args, int num_args)
{
  xen_value *true_args[2];
  xen_value *rtn;
  int k;
  run_opt_arg(pt, args, num_args, 1, true_args);
  true_args[0] = args[0];
  rtn = package(pt, R_INT, channels_i, descr_channels_i, true_args, 1);
  for (k = num_args + 1; k <= 1; k++) FREE(true_args[k]);
  return(rtn);
}


/* ---------------- c-g? ---------------- */

static void c_g_p(int *args, ptree *pt) 
{
  /* explicit check here speeds this up by about a factor of 3! but it's still slow */
#if USE_GTK
  if (gtk_events_pending())
#else
  #if USE_MOTIF
  if ((XtPending()) & (XtIMXEvent | XtIMAlternateInput))
  #endif
#endif
    {
      check_for_event();
      if (ss->stopped_explicitly)
	{
	  ss->stopped_explicitly = false;
	  BOOL_RESULT = (Int)true;
	}
      else BOOL_RESULT = (Int)false;
    }
#if USE_GTK || USE_MOTIF
  else BOOL_RESULT = (Int)false;
#endif
}

static char *descr_c_g_p(int *args, ptree *pt)
{
  return(mus_format( BOOL_PT " = c-g?()", args[0], B2S(BOOL_RESULT)));
}

static xen_value *c_g_p_1(ptree *pt, xen_value **args, int num_args)
{
  return(package(pt, R_BOOL, c_g_p, descr_c_g_p, args, 0));
}


static void vct_check_1(int *args, ptree *pt) {if (!(VCT_ARG_1)) mus_error(MUS_NO_DATA, "arg 1 (vct) is null");}
static char *descr_vct_check_1(int *args, ptree *pt) {return(mus_format("if (!(" VCT_PT ")) mus-error", args[1], DESC_VCT_ARG_1));}
static void vct_check_2(int *args, ptree *pt) {if (!(VCT_ARG_2)) mus_error(MUS_NO_DATA, "arg 2 (vct) is null");}
static char *descr_vct_check_2(int *args, ptree *pt) {return(mus_format("if (!(" VCT_PT ")) mus-error", args[2], DESC_VCT_ARG_2));}


/* ---------------- autocorrelate ---------------- */
static char *descr_autocorrelate_0(int *args, ptree *pt) 
{
  return(mus_format("autocorrelate(" VCT_PT ")", args[1], DESC_VCT_ARG_1));
}
static void autocorrelate_0(int *args, ptree *pt) 
{
  autocorrelation(VCT_ARG_1->data, VCT_ARG_1->length);
  VCT_RESULT = VCT_ARG_1;
}
static xen_value *autocorrelate_1(ptree *prog, xen_value **args, int num_args) 
{
  if (run_safety == RUN_SAFE) package(prog, R_BOOL, vct_check_1, descr_vct_check_1, args, 1);
  return(package(prog, R_VCT, autocorrelate_0, descr_autocorrelate_0, args, 1));
}


/* ---------------- frames ---------------- */

static void frames_i(int *args, ptree *pt) 
{
  chan_info *cp; 
  int pos;
  cp = run_get_cp(1, args, pt->ints);
  if (cp)
    {
      if (INT_ARG_3 == AT_CURRENT_EDIT_POSITION)
	pos = cp->edit_ctr;
      else pos = INT_ARG_3;
      INT_RESULT = cp->samples[pos];
    }
}

static char *descr_frames_i(int *args, ptree *pt) 
{
  return(mus_format( INT_PT " = frames(" INT_PT ", " INT_PT ", " INT_PT ")", 
		     args[0], INT_RESULT, args[1], INT_ARG_1, args[2], INT_ARG_2, args[3], INT_ARG_3));
}

static xen_value *frames_1(ptree *pt, xen_value **args, int num_args)
{
  xen_value *true_args[4];
  xen_value *rtn;
  int k;
  if (num_args < 3)
    true_args[3] = make_xen_value(R_INT, add_int_to_ptree(pt, AT_CURRENT_EDIT_POSITION), R_CONSTANT);
  else true_args[3] = args[3];
  run_opt_arg(pt, args, num_args, 1, true_args);
  run_opt_arg(pt, args, num_args, 2, true_args);
  true_args[0] = args[0];
  rtn = package(pt, R_INT, frames_i, descr_frames_i, true_args, 3);
  for (k = num_args + 1; k <= 3; k++) FREE(true_args[k]);
  return(rtn);
}


/* ---------------- samples->vct ---------------- */
/* samples->vct samp samps snd chn v edpos */

static void samples_to_vct_v(int *args, ptree *pt) 
{
  chan_info *cp; 
  int pos, i, len;
  snd_fd *sf;
  Float *fvals;
  vct *v;
  v = pt->vcts[args[5]];
  if (v)
    {
      fvals = v->data;
      cp = run_get_cp(3, args, pt->ints);
      if (cp)
	{
	  if (INT_ARG_6 == AT_CURRENT_EDIT_POSITION)
	    pos = cp->edit_ctr;
	  else pos = INT_ARG_6;
	  sf = init_sample_read_any(INT_ARG_1, cp, READ_FORWARD, pos);
	  if (sf)
	    {
	      len = INT_ARG_2;
	      for (i = 0; i < len; i++) 
		fvals[i] = read_sample_to_float(sf);
	      free_snd_fd(sf);
	    }
	}
    }
  VCT_RESULT = v;
}

static char *descr_samples_to_vct_v(int *args, ptree *pt) 
{
  return(mus_format( VCT_PT " = samples->vct(" INT_PT ", " INT_PT ", " INT_PT ", " INT_PT ", " VCT_PT ", " INT_PT ")",
		     args[0], DESC_VCT_RESULT, args[1], INT_ARG_1, args[2], INT_ARG_2, 
		     args[3], INT_ARG_3, args[4], INT_ARG_4, args[5], DESC_VCT_ARG_5,
		     args[6], INT_ARG_6));
}

static xen_value *samples_to_vct_1(ptree *pt, xen_value **args, int num_args)
{
  xen_value *true_args[7];
  xen_value *rtn;
  int k;
  true_args[0] = args[0];
  true_args[1] = args[1];
  true_args[2] = args[2];
  run_opt_arg(pt, args, num_args, 3, true_args);
  run_opt_arg(pt, args, num_args, 4, true_args);
  true_args[5] = args[5]; /* create? */
  if (num_args < 6)
    true_args[6] = make_xen_value(R_INT, add_int_to_ptree(pt, AT_CURRENT_EDIT_POSITION), R_CONSTANT);
  else true_args[6] = args[6];
  rtn = package(pt, R_VCT, samples_to_vct_v, descr_samples_to_vct_v, true_args, 6);
  for (k = num_args + 1; k <= 6; k++) FREE(true_args[k]);
  return(rtn);
}


/* ---------------- report-in-minibuffer ---------------- */

static void report_in_minibuffer_s(int *args, ptree *pt) 
{
  snd_info *sp;
  sp = run_get_sp(2, args, pt->ints);
  if (sp)
    {
      set_minibuffer_string(sp, STRING_ARG_1, true);
      sp->minibuffer_on = MINI_REPORT;
    }
}

static char *descr_report_in_minibuffer_s(int *args, ptree *pt) 
{
  return(mus_format("report_in_minibuffer(" STR_PT ", " INT_PT ")", args[1], STRING_ARG_1, args[2], INT_ARG_2));
}

static xen_value *report_in_minibuffer_1(ptree *pt, xen_value **args, int num_args)
{
  xen_value *true_args[3];
  xen_value *rtn;
  int k;
  run_opt_arg(pt, args, num_args, 2, true_args);
  true_args[1] = args[1];
  true_args[0] = args[0];
  rtn = package(pt, R_BOOL, report_in_minibuffer_s, descr_report_in_minibuffer_s, true_args, 2);
  for (k = num_args + 1; k <= 2; k++) FREE(true_args[k]);
  return(rtn);
}


/* ---------------- sample-reader stuff ---------------- */

static void reader_f(int *args, ptree *pt) {FLOAT_RESULT = read_sample_to_float(READER_ARG_1);}
static char *descr_reader(int *args, ptree *pt, const char *which) 
{
  return(mus_format( FLT_PT " = %s(" RD_PT ")", args[0], FLOAT_RESULT, which, args[1], DESC_READER_ARG_1));
}
static char *descr_reader_f(int *args, ptree *pt) {return(descr_reader(args, pt, "read-sample"));}
static xen_value *reader_0(ptree *prog, xen_value **args, xen_value *sf)
{
  if (args[0]) FREE(args[0]);
  args[0] = make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, 0.0), R_VARIABLE);
  add_triple_to_ptree(prog, va_make_triple(reader_f, descr_reader_f, 2, args[0], sf));
  return(args[0]);
}
static xen_value *reader_1(ptree *prog, xen_value **args, int num_args) {return(package(prog, R_FLOAT, reader_f, descr_reader_f, args, 1));}

static char *descr_next_reader_f(int *args, ptree *pt) {return(descr_reader(args, pt, "next-sample"));}
static void next_reader_f(int *args, ptree *pt) {FLOAT_RESULT = protected_next_sample_to_float(READER_ARG_1);}
static xen_value *next_sample_1(ptree *prog, xen_value **args, int num_args) {return(package(prog, R_FLOAT, next_reader_f, descr_next_reader_f, args, 1));}

static char *descr_previous_reader_f(int *args, ptree *pt) {return(descr_reader(args, pt, "previous-sample"));}
static void previous_reader_f(int *args, ptree *pt) {FLOAT_RESULT = protected_previous_sample_to_float(READER_ARG_1);}
static xen_value *previous_sample_1(ptree *prog, xen_value **args, int num_args) {return(package(prog, R_FLOAT, previous_reader_f, descr_previous_reader_f, args, 1));}


static char *descr_reader_at_end_b_s(int *args, ptree *pt)
{
  return(mus_format( BOOL_PT " = sample-reader-at-end?(" RD_PT ")", args[0], B2S(BOOL_RESULT), args[1], DESC_READER_ARG_1));
}
static char *descr_reader_at_end_b_m(int *args, ptree *pt)
{
  return(mus_format( BOOL_PT " = (mix-)sample-reader-at-end?(" RD_PT ")", args[0], B2S(BOOL_RESULT), args[1], DESC_MIX_READER_ARG_1));
}
static char *descr_reader_at_end_b_t(int *args, ptree *pt)
{
  return(mus_format( BOOL_PT " = (track-)sample-reader-at-end?(" RD_PT ")", args[0], B2S(BOOL_RESULT), args[1], DESC_TRACK_READER_ARG_1));
}

static void reader_at_end_b_s(int *args, ptree *pt) {BOOL_RESULT = READER_ARG_1->at_eof;}
static void reader_at_end_b_m(int *args, ptree *pt) {BOOL_RESULT = mix_sample_reader_at_end_p(MIX_READER_ARG_1);}
static void reader_at_end_b_t(int *args, ptree *pt) {BOOL_RESULT = track_sample_reader_at_end_p(TRACK_READER_ARG_1);}

static xen_value *sample_reader_at_end_p_1(ptree *prog, xen_value **args, int num_args) 
{
  if (args[1]->type == R_READER)
    return(package(prog, R_BOOL, reader_at_end_b_s, descr_reader_at_end_b_s, args, 1));
  if (args[1]->type == R_MIX_READER)
    return(package(prog, R_BOOL, reader_at_end_b_m, descr_reader_at_end_b_m, args, 1));
  if (args[1]->type == R_TRACK_READER)
    return(package(prog, R_BOOL, reader_at_end_b_t, descr_reader_at_end_b_t, args, 1));
  return(NULL);
}


static char *descr_make_sample_reader_r(int *args, ptree *pt)
{
  return(mus_format( RD_PT " = make-sample-reader(" INT_PT ", " INT_PT ", " INT_PT ", " INT_PT ", " INT_PT ")",
		    args[0], DESC_READER_RESULT, 
		    args[1], INT_ARG_1, args[2], INT_ARG_2, args[3], INT_ARG_3, args[4], INT_ARG_4, args[5], INT_ARG_5));
}
static void make_sample_reader_r(int *args, ptree *pt) 
{
  chan_info *cp = NULL;
  int pos;
  read_direction_t direction = READ_FORWARD;
  cp = run_get_cp(2, args, pt->ints);
  if (cp)
    {
      if (INT_ARG_5 == -1)
	pos = cp->edit_ctr;
      else pos = INT_ARG_5;
      if (READER_RESULT) free_snd_fd(READER_RESULT);
      if (INT_ARG_4 == -1) direction = READ_BACKWARD;
      READER_RESULT = init_sample_read_any(INT_ARG_1, cp, direction, pos);
    }
}
static xen_value *make_sample_reader_1(ptree *pt, xen_value **args, int num_args)
{
  xen_value *true_args[6];
  int k;
  xen_value *rtn;
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
	true_args[1] = convert_dbl_to_int(pt, args[1], true);
      else true_args[1] = args[1];
    }
  true_args[0] = args[0];
  rtn = package(pt, R_READER, make_sample_reader_r, descr_make_sample_reader_r, true_args, 5);
  add_obj_to_gcs(pt, R_READER, rtn->addr);
  for (k = num_args + 1; k <= 5; k++) FREE(true_args[k]);
  return(rtn);
}


static char *descr_make_region_sample_reader_r(int *args, ptree *pt)
{
  return(mus_format( RD_PT " = make-region-sample-reader(" INT_PT ", " INT_PT ", " INT_PT ")",
		    args[0], DESC_READER_RESULT, args[1], INT_ARG_1, args[2], INT_ARG_2, args[3], INT_ARG_3));
}
static void make_region_sample_reader_r(int *args, ptree *pt) 
{
  if (READER_RESULT) free_snd_fd(READER_RESULT);
  READER_RESULT = init_region_read(INT_ARG_1, INT_ARG_2, INT_ARG_3, READ_FORWARD); /* beg reg chn */
}
static xen_value *make_region_sample_reader_1(ptree *pt, xen_value **args, int num_args)
{
  xen_value *rtn;
  rtn = package(pt, R_READER, make_region_sample_reader_r, descr_make_region_sample_reader_r, args, 3);
  add_obj_to_gcs(pt, R_READER, rtn->addr);
  return(rtn);
}


/* -------- mix -------- */

static void mix_reader_f(int *args, ptree *pt) {FLOAT_RESULT = mix_read_sample_to_float(MIX_READER_ARG_1);}
static char *descr_mix_reader(int *args, ptree *pt, const char *which) 
{
  return(mus_format( FLT_PT " = %s(" MF_PT ")", args[0], FLOAT_RESULT, which, args[1], DESC_MIX_READER_ARG_1));
}
static char *descr_mix_reader_f(int *args, ptree *pt) {return(descr_mix_reader(args, pt, "read-mix-sample"));}
static xen_value *mix_reader_0(ptree *prog, xen_value **args, xen_value *sf)
{
  if (args[0]) FREE(args[0]);
  args[0] = make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, 0.0), R_VARIABLE);
  add_triple_to_ptree(prog, va_make_triple(mix_reader_f, descr_mix_reader_f, 2, args[0], sf));
  return(args[0]);
}
static xen_value *mix_reader_1(ptree *prog, xen_value **args, int num_args) 
{
  return(package(prog, R_FLOAT, mix_reader_f, descr_mix_reader_f, args, 1));
}

static char *descr_make_mix_sample_reader_r2(int *args, ptree *pt)
{
  return(mus_format( MF_PT " = make-mix-sample-reader(" INT_PT ", " INT_PT ")",
		    args[0], DESC_MIX_READER_RESULT, args[1], INT_ARG_1, args[2], INT_ARG_2));
}
static char *descr_make_mix_sample_reader_r1(int *args, ptree *pt)
{
  return(mus_format( MF_PT " = make-mix-sample-reader(" INT_PT ", 0)", args[0], DESC_MIX_READER_RESULT, args[1], INT_ARG_1));
}
static void make_mix_sample_reader_r2(int *args, ptree *pt) {MIX_READER_RESULT = run_make_mix_sample_reader(INT_ARG_1, INT_ARG_2);}
static void make_mix_sample_reader_r1(int *args, ptree *pt) {MIX_READER_RESULT = run_make_mix_sample_reader(INT_ARG_1, 0);}
static xen_value *make_mix_sample_reader_1(ptree *pt, xen_value **args, int num_args)
{
  xen_value *rtn;
  if (num_args == 2)
    rtn = package(pt, R_MIX_READER, make_mix_sample_reader_r2, descr_make_mix_sample_reader_r2, args, num_args);
  else rtn = package(pt, R_MIX_READER, make_mix_sample_reader_r1, descr_make_mix_sample_reader_r1, args, num_args);
  add_obj_to_gcs(pt, R_MIX_READER, rtn->addr);
  return(rtn);
}

/* -------- track -------- */

static void track_reader_f(int *args, ptree *pt) {FLOAT_RESULT = track_read_sample_to_float(TRACK_READER_ARG_1);}
static char *descr_track_reader(int *args, ptree *pt, const char *which) 
{
  return(mus_format( FLT_PT " = %s(" TF_PT ")", args[0], FLOAT_RESULT, which, args[1], DESC_TRACK_READER_ARG_1));
}
static char *descr_track_reader_f(int *args, ptree *pt) {return(descr_track_reader(args, pt, "read-track-sample"));}
static xen_value *track_reader_0(ptree *prog, xen_value **args, xen_value *sf)
{
  if (args[0]) FREE(args[0]);
  args[0] = make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, 0.0), R_VARIABLE);
  add_triple_to_ptree(prog, va_make_triple(track_reader_f, descr_track_reader_f, 2, args[0], sf));
  return(args[0]);
}
static xen_value *track_reader_1(ptree *prog, xen_value **args, int num_args) {return(package(prog, R_FLOAT, track_reader_f, descr_track_reader_f, args, 1));}

static char *descr_make_track_sample_reader_r3(int *args, ptree *pt)
{
  return(mus_format( TF_PT " = make-track-sample-reader(" INT_PT ", " INT_PT ", " INT_PT ")",
		     args[0], DESC_TRACK_READER_RESULT, args[1], INT_ARG_1, args[2], INT_ARG_2, args[3], INT_ARG_3));
}
static char *descr_make_track_sample_reader_r2(int *args, ptree *pt)
{
  return(mus_format( TF_PT " = make-track-sample-reader(" INT_PT ", " INT_PT ", 0)",
		    args[0], DESC_TRACK_READER_RESULT, args[1], INT_ARG_1, args[2], INT_ARG_2));
}
static char *descr_make_track_sample_reader_r1(int *args, ptree *pt)
{
  return(mus_format( TF_PT " = make-track-sample-reader(" INT_PT ", 0, 0)", args[0], DESC_TRACK_READER_RESULT, args[1], INT_ARG_1));
}
static void make_track_sample_reader_r3(int *args, ptree *pt) {TRACK_READER_RESULT = run_make_track_sample_reader(INT_ARG_1, INT_ARG_2, INT_ARG_3);}
static void make_track_sample_reader_r2(int *args, ptree *pt) {TRACK_READER_RESULT = run_make_track_sample_reader(INT_ARG_1, INT_ARG_2, 0);}
static void make_track_sample_reader_r1(int *args, ptree *pt) {TRACK_READER_RESULT = run_make_track_sample_reader(INT_ARG_1, 0, 0);}
static xen_value *make_track_sample_reader_1(ptree *pt, xen_value **args, int num_args)
{
  xen_value *rtn;
  if (num_args == 3)
    rtn = package(pt, R_TRACK_READER, make_track_sample_reader_r3, descr_make_track_sample_reader_r3, args, num_args);
  else
    {
      if (num_args == 2)
	rtn = package(pt, R_TRACK_READER, make_track_sample_reader_r2, descr_make_track_sample_reader_r2, args, num_args);
      else rtn = package(pt, R_TRACK_READER, make_track_sample_reader_r1, descr_make_track_sample_reader_r1, args, num_args);
    }
  add_obj_to_gcs(pt, R_TRACK_READER, rtn->addr);
  return(rtn);
}



/* ---------------- vector stuff ---------------- */

/* float vectors are handled as vcts
 */

/* length */
static void vector_length_f(int *args, ptree *pt) {INT_RESULT = VCT_ARG_1->length;}
static void vector_length_i(int *args, ptree *pt) {INT_RESULT = VECT_ARG_1->length;}
static void vector_length_c(int *args, ptree *pt) {INT_RESULT = VECT_ARG_1->length;}
static void vector_length_v(int *args, ptree *pt) {INT_RESULT = VECT_ARG_1->length;}
static char *descr_vector_length_f(int *args, ptree *pt) 
{
  return(mus_format( INT_PT " = vector_length(" VCT_PT ")", args[0], INT_RESULT, args[1], DESC_VCT_ARG_1));
}
static char *descr_vector_length(int *args, ptree *pt) 
{
  return(mus_format( INT_PT " = vector_length(" VECT_PT ")", args[0], INT_RESULT, args[1], DESC_VECT_ARG_1));
}
static xen_value *vector_length_1(ptree *prog, xen_value **args, int num_args)
{
  switch (args[1]->type)
    {
    case R_FLOAT_VECTOR: return(package(prog, R_INT, vector_length_f, descr_vector_length_f, args, 1));
    case R_INT_VECTOR: return(package(prog, R_INT, vector_length_i, descr_vector_length, args, 1));
    case R_CLM_VECTOR: return(package(prog, R_INT, vector_length_c, descr_vector_length, args, 1));
    case R_VCT_VECTOR: return(package(prog, R_INT, vector_length_v, descr_vector_length, args, 1));
    }
  return(NULL);
}

/* ref */
static void vector_ref_f(int *args, ptree *pt) {FLOAT_RESULT = VCT_ARG_1->data[INT_ARG_2];}
static void vector_ref_i(int *args, ptree *pt) {INT_RESULT = VECT_ARG_1->data.ints[INT_ARG_2];}
static void vector_ref_v(int *args, ptree *pt) {VCT_RESULT = VECT_ARG_1->data.vcts[INT_ARG_2];}
static void vector_ref_c(int *args, ptree *pt) {CLM_RESULT = VECT_ARG_1->data.gens[INT_ARG_2];}
static char *descr_vector_ref_f(int *args, ptree *pt) 
{
  return(mus_format( FLT_PT " = vector_ref(" VCT_PT ", " INT_PT ")", args[0], FLOAT_RESULT, args[1], DESC_VCT_ARG_1, args[2], INT_ARG_2));
}
static char *descr_vector_ref_i(int *args, ptree *pt) 
{
  return(mus_format( INT_PT " = vector_ref(" VECT_PT ", " INT_PT ")", args[0], INT_RESULT, args[1], DESC_VECT_ARG_1, args[2], INT_ARG_2));
}
static char *descr_vector_ref_v(int *args, ptree *pt) 
{
  return(mus_format( VCT_PT " = vector_ref(" VECT_PT ", " INT_PT ")", args[0], DESC_VCT_RESULT, args[1], DESC_VECT_ARG_1, args[2], INT_ARG_2));
}
static char *descr_vector_ref_c(int *args, ptree *pt) 
{
  return(mus_format( CLM_PT " = vector_ref(" VECT_PT ", " INT_PT ")", args[0], DESC_CLM_RESULT, args[1], DESC_VECT_ARG_1, args[2], INT_ARG_2));
}
static xen_value *vector_ref_1(ptree *prog, xen_value **args, int num_args)
{
  switch (args[1]->type)
    {
    case R_FLOAT_VECTOR: return(package(prog, R_FLOAT, vector_ref_f, descr_vector_ref_f, args, 2)); break;
    case R_INT_VECTOR: return(package(prog, R_INT, vector_ref_i, descr_vector_ref_i, args, 2)); break;
    case R_VCT_VECTOR: return(package(prog, R_VCT, vector_ref_v, descr_vector_ref_v, args, 2)); break;
    case R_CLM_VECTOR: return(package(prog, R_CLM, vector_ref_c, descr_vector_ref_c, args, 2)); break;
    }
  return(NULL);
}


/* set */
static void vector_set_f(int *args, ptree *pt) {VCT_ARG_1->data[INT_ARG_2] = FLOAT_ARG_3;}
static void vector_set_i(int *args, ptree *pt) {VECT_ARG_1->data.ints[INT_ARG_2] = INT_ARG_3;}
static void vector_set_v(int *args, ptree *pt) {VECT_ARG_1->data.vcts[INT_ARG_2] = VCT_ARG_3;}
static void vector_set_c(int *args, ptree *pt) {VECT_ARG_1->data.gens[INT_ARG_2] = CLM_ARG_3;}
static char *descr_vector_set_f(int *args, ptree *pt) 
{
  return(mus_format("vector_set(" VCT_PT ", " INT_PT ", " FLT_PT ")", args[1], DESC_VCT_ARG_1, args[2], INT_ARG_2, args[3], FLOAT_ARG_3));
}
static char *descr_vector_set_i(int *args, ptree *pt) 
{
  return(mus_format("vector_set(" VECT_PT ", " INT_PT ", " INT_PT ")", args[1], DESC_VECT_ARG_1, args[2], INT_ARG_2, args[3], INT_ARG_3));
}
static char *descr_vector_set_v(int *args, ptree *pt) 
{
  return(mus_format("vector_set(" VECT_PT ", " INT_PT ", " VCT_PT ")", args[1], DESC_VECT_ARG_1, args[2], INT_ARG_2, args[3], DESC_VCT_ARG_3));
}
static char *descr_vector_set_c(int *args, ptree *pt) 
{
  return(mus_format("vector_set(" VECT_PT ", " INT_PT ", " CLM_PT ")", args[1], DESC_VECT_ARG_1, args[2], INT_ARG_2, args[3], DESC_CLM_ARG_3));
}
static xen_value *vector_set_1(ptree *prog, xen_value **args, int num_args)
{
  xen_var *var;
  var = find_var_in_ptree_via_addr(prog, args[1]->type, args[1]->addr);
  if (var) var->unclean = true;
  switch (args[1]->type)
    {
    case R_FLOAT_VECTOR: 
      if (args[3]->type != R_FLOAT) return(run_warn("wrong new val type for float vector set"));
      return(package(prog, R_FLOAT, vector_set_f, descr_vector_set_f, args, 3)); 
      break;
    case R_INT_VECTOR: 
      if (args[3]->type != R_INT) return(run_warn("wrong new val type for int vector set"));
      return(package(prog, R_INT, vector_set_i, descr_vector_set_i, args, 3)); 
      break;
    case R_VCT_VECTOR: 
      if (args[3]->type != R_VCT) return(run_warn("wrong new val type for vct vector set"));
      return(package(prog, R_VCT, vector_set_v, descr_vector_set_v, args, 3)); 
      break;
    case R_CLM_VECTOR: 
      if (args[3]->type != R_CLM) return(run_warn("wrong new val type for clm vector set"));
      return(package(prog, R_CLM, vector_set_c, descr_vector_set_c, args, 3)); 
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
static char *descr_vector_fill_f(int *args, ptree *pt) 
{
  return(mus_format("vector_fill(" VCT_PT ", " FLT_PT ")", args[1], DESC_VCT_ARG_1, args[2], FLOAT_ARG_2));
}
static char *descr_vector_fill_i(int *args, ptree *pt) 
{
  return(mus_format("vector_fill(" VECT_PT ", " INT_PT ")", args[1], DESC_VECT_ARG_1, args[2], INT_ARG_2));
}
static char *descr_vector_fill_v(int *args, ptree *pt) 
{
  return(mus_format("vector_fill(" VECT_PT ", " VCT_PT ")", args[1], DESC_VECT_ARG_1, args[2], DESC_VCT_ARG_2));
}
static char *descr_vector_fill_c(int *args, ptree *pt) 
{
  return(mus_format("vector_fill(" VECT_PT ", " CLM_PT ")", args[1], DESC_VECT_ARG_1, args[2], DESC_CLM_ARG_2));
}
static xen_value *vector_fill_1(ptree *prog, xen_value **args, int num_args)
{
  switch (args[1]->type)
    {
    case R_FLOAT_VECTOR: 
      if (args[2]->type != R_FLOAT) return(run_warn("wrong new val type for float vector fill"));
      return(package(prog, R_BOOL, vector_fill_f, descr_vector_fill_f, args, 2)); 
      break;
    case R_INT_VECTOR: 
      if (args[2]->type != R_INT) return(run_warn("wrong new val type for int vector fill"));
      return(package(prog, R_BOOL, vector_fill_i, descr_vector_fill_i, args, 2)); 
      break;
    case R_VCT_VECTOR:
      if (args[2]->type != R_VCT) return(run_warn("wrong new val type for vct vector fill"));
      return(package(prog, R_BOOL, vector_fill_v, descr_vector_fill_v, args, 2)); 
      break;
    case R_CLM_VECTOR: 
      if (args[2]->type != R_CLM) return(run_warn("wrong new val type for clm vector fill"));
      return(package(prog, R_BOOL, vector_fill_c, descr_vector_fill_c, args, 2)); 
      break;
    }
  return(NULL);
}



/* ---------------- vct stuff ---------------- */

static void vct_length_i(int *args, ptree *pt) {INT_RESULT = VCT_ARG_1->length;}
static char *descr_vct_length_i(int *args, ptree *pt) 
{
  return(mus_format( INT_PT " = vct_length(" VCT_PT ")", args[0], INT_RESULT, args[1], DESC_VCT_ARG_1));
}
static xen_value *vct_length_1(ptree *prog, xen_value **args, int num_args)
{
  return(package(prog, R_INT, vct_length_i, descr_vct_length_i, args, 1));
}

static void vct_constant_ref_0(int *args, ptree *pt) {FLOAT_RESULT = VCT_ARG_1->data[0];}
static void vct_constant_ref_1(int *args, ptree *pt) {FLOAT_RESULT = VCT_ARG_1->data[1];}
static void vct_constant_ref_2(int *args, ptree *pt) {FLOAT_RESULT = VCT_ARG_1->data[2];}
static char *descr_vct_constant_ref_0(int *args, ptree *pt) 
{
  return(mus_format( FLT_PT " = vct_ref(" VCT_PT ", 0)", args[0], FLOAT_RESULT, args[1], DESC_VCT_ARG_1));
}
static char *descr_vct_constant_ref_1(int *args, ptree *pt) 
{
  return(mus_format( FLT_PT " = vct_ref(" VCT_PT ", 1)", args[0], FLOAT_RESULT, args[1], DESC_VCT_ARG_1));
}
static char *descr_vct_constant_ref_2(int *args, ptree *pt) 
{
  return(mus_format( FLT_PT " = vct_ref(" VCT_PT ", 2)", args[0], FLOAT_RESULT, args[1], DESC_VCT_ARG_1));
}

static void vct_ref_f(int *args, ptree *pt) {FLOAT_RESULT = VCT_ARG_1->data[INT_ARG_2];}
static char *descr_vct_ref_f(int *args, ptree *pt) 
{
  return(mus_format( FLT_PT " = vct_ref(" VCT_PT ", " INT_PT ")", args[0], FLOAT_RESULT, args[1], DESC_VCT_ARG_1, args[2], INT_ARG_2));
}
static xen_value *vct_ref_1(ptree *prog, xen_value **args, int num_args)
{
  if (args[2]->constant == R_CONSTANT)
    {
      if (prog->ints[args[2]->addr] == 0)
	return(package(prog, R_FLOAT, vct_constant_ref_0, descr_vct_constant_ref_0, args, 1));
      if (prog->ints[args[2]->addr] == 1)
	return(package(prog, R_FLOAT, vct_constant_ref_1, descr_vct_constant_ref_1, args, 1));
      if (prog->ints[args[2]->addr] == 2)
	return(package(prog, R_FLOAT, vct_constant_ref_2, descr_vct_constant_ref_2, args, 1));
    }
  return(package(prog, R_FLOAT, vct_ref_f, descr_vct_ref_f, args, 2));
}

static void vct_constant_set_0(int *args, ptree *pt) {VCT_ARG_1->data[0] = FLOAT_ARG_3; FLOAT_RESULT = FLOAT_ARG_3;}
static void vct_constant_set_1(int *args, ptree *pt) {VCT_ARG_1->data[1] = FLOAT_ARG_3; FLOAT_RESULT = FLOAT_ARG_3;}
static void vct_constant_set_2(int *args, ptree *pt) {VCT_ARG_1->data[2] = FLOAT_ARG_3; FLOAT_RESULT = FLOAT_ARG_3;}
static void vct_set_f(int *args, ptree *pt) {VCT_ARG_1->data[INT_ARG_2] = FLOAT_ARG_3; FLOAT_RESULT = FLOAT_ARG_3;}

static char *descr_vct_constant_set_0(int *args, ptree *pt) 
{
  return(mus_format("vct_set!(" VCT_PT ", 0, " FLT_PT ")",  args[1], DESC_VCT_ARG_1, args[3], FLOAT_ARG_3));
}
static char *descr_vct_constant_set_1(int *args, ptree *pt) 
{
  return(mus_format("vct_set!(" VCT_PT ", 1, " FLT_PT ")",  args[1], DESC_VCT_ARG_1, args[3], FLOAT_ARG_3));
}
static char *descr_vct_constant_set_2(int *args, ptree *pt) 
{
  return(mus_format("vct_set!(" VCT_PT ", 2, " FLT_PT ")",  args[1], DESC_VCT_ARG_1, args[3], FLOAT_ARG_3));
}
static char *descr_vct_set_f(int *args, ptree *pt) 
{
  return(mus_format("vct_set!(" VCT_PT ", " INT_PT ", " FLT_PT ")", args[1], DESC_VCT_ARG_1, args[2], INT_ARG_2, args[3], FLOAT_ARG_3));
}
static void vct_set_i(int *args, ptree *pt) {VCT_ARG_1->data[INT_ARG_2] = (Float)INT_ARG_3; FLOAT_RESULT = (Double)INT_ARG_3;}
static char *descr_vct_set_i(int *args, ptree *pt) 
{
  return(mus_format("vct_set!(" VCT_PT ", " INT_PT ", " INT_PT ")", args[1], DESC_VCT_ARG_1, args[2], INT_ARG_2, args[3], INT_ARG_3));
}
static void vct_set_1(ptree *prog, xen_value *in_v, xen_value *in_v1, xen_value *in_v2, xen_value *v)
{
  /* set! vct-ref */
  xen_var *var;
  var = find_var_in_ptree_via_addr(prog, in_v->type, in_v->addr);
  if (var) var->unclean = true;
  /* v->type is guaranteed float in this case (generalized set will insist on it) */
  if (in_v1->constant == R_CONSTANT)
    {
      if (prog->ints[in_v1->addr] == 0)
	add_triple_to_ptree(prog, va_make_triple(vct_constant_set_0, descr_vct_constant_set_0, 4, NULL, in_v, in_v1, v));
      else if (prog->ints[in_v1->addr] == 1)
	add_triple_to_ptree(prog, va_make_triple(vct_constant_set_1, descr_vct_constant_set_1, 4, NULL, in_v, in_v1, v));
      else if (prog->ints[in_v1->addr] == 2)
	add_triple_to_ptree(prog, va_make_triple(vct_constant_set_2, descr_vct_constant_set_2, 4, NULL, in_v, in_v1, v));
    }
  else add_triple_to_ptree(prog, va_make_triple(vct_set_f, descr_vct_set_f, 4, NULL, in_v, in_v1, v));
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
	    return(package(prog, R_FLOAT, vct_constant_set_0, descr_vct_constant_set_0, args, 3));
	  if (prog->ints[args[2]->addr] == 1)
	    return(package(prog, R_FLOAT, vct_constant_set_1, descr_vct_constant_set_1, args, 3));
	  if (prog->ints[args[2]->addr] == 2)
	    return(package(prog, R_FLOAT, vct_constant_set_2, descr_vct_constant_set_2, args, 3));
	}
      return(package(prog, R_FLOAT, vct_set_f, descr_vct_set_f, args, 3));
    }
  return(package(prog, R_FLOAT, vct_set_i, descr_vct_set_i, args, 3));
}

static void make_vct_v(int *args, ptree *pt) 
{
  if (VCT_RESULT) c_free_vct(VCT_RESULT);
  VCT_RESULT = c_make_vct(INT_ARG_1);
}
static char *descr_make_vct_v(int *args, ptree *pt) 
{
  return(mus_format( VCT_PT " = make_vct(" INT_PT ")", args[0], DESC_VCT_RESULT, args[1], INT_ARG_1));
}
static void make_vct_v2(int *args, ptree *pt) 
{
  vct *v;
  int i;
  if (VCT_RESULT) c_free_vct(VCT_RESULT);
  v = c_make_vct(INT_ARG_1);
  VCT_RESULT = v;
  for (i = 0; i < v->length; i++) v->data[i] = FLOAT_ARG_2;
}
static char *descr_make_vct_v2(int *args, ptree *pt) 
{
  return(mus_format( VCT_PT " = make_vct(" INT_PT ", " FLT_PT ")", args[0], DESC_VCT_RESULT, args[1], INT_ARG_1, args[2], FLOAT_ARG_2));
}
static xen_value *make_vct_1(ptree *prog, xen_value **args, int num_args)
{
  args[0] = make_xen_value(R_VCT, add_vct_to_ptree(prog, NULL), R_VARIABLE);
  add_obj_to_gcs(prog, R_VCT, args[0]->addr);
  if (num_args == 1)
    add_triple_to_ptree(prog, va_make_triple(make_vct_v, descr_make_vct_v, 2, args[0], args[1]));
  else add_triple_to_ptree(prog, va_make_triple(make_vct_v2, descr_make_vct_v2, 3, args[0], args[1], args[2]));
  return(args[0]);
}

static xen_value *make_vector_1(ptree *prog, xen_value **args, int num_args)
{
  args[0] = make_xen_value(R_FLOAT_VECTOR, add_vct_to_ptree(prog, NULL), R_VARIABLE);
  add_obj_to_gcs(prog, R_FLOAT_VECTOR, args[0]->addr);
  if (num_args == 1)
    add_triple_to_ptree(prog, va_make_triple(make_vct_v, descr_make_vct_v, 2, args[0], args[1]));
  else add_triple_to_ptree(prog, va_make_triple(make_vct_v2, descr_make_vct_v2, 3, args[0], args[1], args[2]));
  return(args[0]);
}

static void vct_v(int *args, ptree *pt) 
{
  int i;
  vct *v;
  if (VCT_RESULT) c_free_vct(VCT_RESULT);
  v = c_make_vct(pt->ints[args[1]]);
  for (i = 0; i < v->length; i++)
    v->data[i] = pt->dbls[args[i + 2]];
  VCT_RESULT = v;
}
static char *descr_vct_v(int *args, ptree *pt) 
{
  return(mus_format( VCT_PT " = vct(" INT_PT " ...)", args[0], DESC_VCT_RESULT, args[1], INT_ARG_1));
}
static xen_value *vct_1(ptree *prog, xen_value **args, int num_args)
{
  xen_value *rtn;
  rtn = package_n(prog, R_VCT, vct_v, descr_vct_v, args, num_args);
  /* rtn is args[0] */
  add_obj_to_gcs(prog, R_VCT, rtn->addr);
  return(rtn);
}

static void vct_copy_v(int *args, ptree *pt) 
{
  if (VCT_RESULT) c_free_vct(VCT_RESULT);
  VCT_RESULT = c_vct_copy(VCT_ARG_1);
}
static char *descr_vct_copy_v(int *args, ptree *pt) 
{
  return(mus_format( VCT_PT " = vct_copy(" VCT_PT ")", args[0], DESC_VCT_RESULT, args[1], DESC_VCT_ARG_1));
}
static xen_value *vct_copy_1(ptree *prog, xen_value **args, int num_args)
{
  if (run_safety == RUN_SAFE) package(prog, R_BOOL, vct_check_1, descr_vct_check_1, args, 1);
  args[0] = make_xen_value(R_VCT, add_vct_to_ptree(prog, NULL), R_VARIABLE);
  add_obj_to_gcs(prog, R_VCT, args[0]->addr);
  add_triple_to_ptree(prog, va_make_triple(vct_copy_v, descr_vct_copy_v, 2, args[0], args[1]));
  return(args[0]);
}

#define VCT_OP_1(SName, CName, COp) \
static void vct_ ## CName ## _f(int *args, ptree *pt) \
{ \
  int i; \
  vct *v = VCT_ARG_1; \
  for (i = 0; i < v->length; i++) v->data[i] COp FLOAT_ARG_2; \
  VCT_RESULT = VCT_ARG_1; \
} \
static char *descr_vct_ ## CName ## _f(int *args, ptree *pt)  \
{ \
  return(mus_format("vct_" #SName "(" VCT_PT ", " FLT_PT ")", args[1], DESC_VCT_ARG_1, args[2], FLOAT_ARG_2)); \
} \
static xen_value *vct_ ## CName ## _1(ptree *prog, xen_value **args, int num_args) \
{ \
  xen_value *temp; \
  if (args[2]->type == R_INT) \
    { \
      temp = args[2]; \
      args[2] = convert_int_to_dbl(prog, args[2]); \
      FREE(temp); \
    } \
  if (run_safety == RUN_SAFE) package(prog, R_BOOL, vct_check_1, descr_vct_check_1, args, 1); \
  return(package(prog, R_VCT, vct_ ## CName ## _f, descr_vct_ ## CName ## _f, args, 2)); \
}

VCT_OP_1(fill!, fill, =)
VCT_OP_1(scale!, scale, *=)
VCT_OP_1(offset!, offset, +=)

#define VCT_OP_2(SName, CName, COp) \
static void vct_ ## CName ## _f(int *args, ptree *pt) \
{ \
  int i, len; \
  vct *v0 = VCT_ARG_1; \
  vct *v1 = VCT_ARG_2; \
  len = v0->length; \
  if (v1->length < len) len = v1->length; \
  for (i = 0; i < len; i++) v0->data[i] COp v1->data[i]; \
  VCT_RESULT = v0; \
} \
static char *descr_vct_ ## CName ## _f(int *args, ptree *pt)  \
{ \
  return(mus_format("vct_" #SName "(" VCT_PT ", " VCT_PT ")", args[1], DESC_VCT_ARG_1, args[2], DESC_VCT_ARG_2)); \
} \
static xen_value *vct_ ## CName ## _1(ptree *prog, xen_value **args, int num_args) \
{ \
  if (run_safety == RUN_SAFE) package(prog, R_BOOL, vct_check_1, descr_vct_check_1, args, 1); \
  if (run_safety == RUN_SAFE) package(prog, R_BOOL, vct_check_2, descr_vct_check_2, args, 2); \
  return(package(prog, R_VCT, vct_ ## CName ## _f, descr_vct_ ## CName ## _f, args, 2)); \
}

VCT_OP_2(add!, add, +=)
VCT_OP_2(multiply!, multiply, *=)
VCT_OP_2(subtract!, subtract, -=)


static void vct_convolve_0(int *args, ptree *pt)
{
  vct *v0 = VCT_ARG_1;
  vct *v1 = VCT_ARG_2;
  mus_convolution(v0->data, v1->data, v0->length); /* assuming power of 2 len here and v1 exists and v0 is big enough */
  VCT_RESULT = v0;
}
static char *descr_vct_convolve_0(int *args, ptree *pt)
{
  return(mus_format("vct_convolve!(" VCT_PT ", " VCT_PT ")", args[1], DESC_VCT_ARG_1, args[2], DESC_VCT_ARG_2));
}
static xen_value *vct_convolve_1(ptree *prog, xen_value **args, int num_args)
{
  if (run_safety == RUN_SAFE) package(prog, R_BOOL, vct_check_1, descr_vct_check_1, args, 1);
  if (run_safety == RUN_SAFE) package(prog, R_BOOL, vct_check_2, descr_vct_check_2, args, 2);
  return(package(prog, R_VCT, vct_convolve_0, descr_vct_convolve_0, args, 2));
}



/* ---------------- sound-data ---------------- */

static void sound_data_length_i(int *args, ptree *pt) {INT_RESULT = SOUND_DATA_ARG_1->length;}
static char *descr_sound_data_length_i(int *args, ptree *pt) 
{
  return(mus_format( INT_PT " = sound-data-length(" SD_PT ")", args[0], INT_RESULT, args[1], DESC_SOUND_DATA_ARG_1));
}
static xen_value *sound_data_length_1(ptree *prog, xen_value **args, int num_args)
{
  return(package(prog, R_INT, sound_data_length_i, descr_sound_data_length_i, args, 1));
}

static void sound_data_chans_i(int *args, ptree *pt) {INT_RESULT = SOUND_DATA_ARG_1->chans;}
static char *descr_sound_data_chans_i(int *args, ptree *pt) 
{
  return(mus_format( INT_PT " = sound-data-chans(" SD_PT ")", args[0], INT_RESULT, args[1], DESC_SOUND_DATA_ARG_1));
}
static xen_value *sound_data_chans_1(ptree *prog, xen_value **args, int num_args)
{
  return(package(prog, R_INT, sound_data_chans_i, descr_sound_data_chans_i, args, 1));
}

static void sound_data_ref_f(int *args, ptree *pt) {FLOAT_RESULT = (Double)MUS_SAMPLE_TO_DOUBLE(SOUND_DATA_ARG_1->data[INT_ARG_2][INT_ARG_3]);}
static char *descr_sound_data_ref_f(int *args, ptree *pt) 
{
  return(mus_format( FLT_PT " = sound_data_ref(" SD_PT ", " INT_PT ", " INT_PT ")", 
		     args[0], FLOAT_RESULT, args[1], DESC_SOUND_DATA_ARG_1, args[2], INT_ARG_2, args[3], INT_ARG_3));
}
static xen_value *sound_data_ref_1(ptree *prog, xen_value **args, int num_args)
{
  return(package(prog, R_FLOAT, sound_data_ref_f, descr_sound_data_ref_f, args, 3));
}

static void sound_data_set_f(int *args, ptree *pt) 
{
  SOUND_DATA_ARG_1->data[INT_ARG_2][INT_ARG_3] = MUS_DOUBLE_TO_SAMPLE(FLOAT_ARG_4);
  FLOAT_RESULT = FLOAT_ARG_4;
}
static void sound_data_set_i(int *args, ptree *pt) 
{
  SOUND_DATA_ARG_1->data[INT_ARG_2][INT_ARG_3] = MUS_DOUBLE_TO_SAMPLE((Float)INT_ARG_4);
  FLOAT_RESULT = (Double)INT_ARG_4;
}

static char *descr_sound_data_set_i(int *args, ptree *pt) 
{
  return(mus_format("sound_data_set!(" SD_PT ", " INT_PT ", " INT_PT ", " INT_PT ")", 
		    args[1], DESC_SOUND_DATA_ARG_1, args[2], INT_ARG_2, args[3], INT_ARG_3, args[4], INT_ARG_4));
}
static char *descr_sound_data_set_f(int *args, ptree *pt) 
{
  return(mus_format("sound_data_set!(" SD_PT ", " INT_PT ", " INT_PT ", " FLT_PT ")", 
		    args[1], DESC_SOUND_DATA_ARG_1, args[2], INT_ARG_2, args[3], INT_ARG_3, args[4], FLOAT_ARG_4));
}

static void sound_data_set_1(ptree *prog, xen_value *in_v, xen_value *in_v1, xen_value *in_v2, xen_value *v)
{
  xen_var *var;
  var = find_var_in_ptree_via_addr(prog, in_v->type, in_v->addr);
  if (var) var->unclean = true;
  /* v->type known to be R_FLOAT (generalized set insists) */
  add_triple_to_ptree(prog, va_make_triple(sound_data_set_f, descr_sound_data_set_f, 5, NULL, in_v, in_v1, in_v2, v));
}

static xen_value *sound_data_set_2(ptree *prog, xen_value **args, int num_args)
{
  xen_var *var;
  var = find_var_in_ptree_via_addr(prog, args[1]->type, args[1]->addr);
  if (var) var->unclean = true;
  if (args[4]->type == R_FLOAT)
    return(package(prog, R_FLOAT, sound_data_set_f, descr_sound_data_set_f, args, 4));
  return(package(prog, R_FLOAT, sound_data_set_i, descr_sound_data_set_i, args, 4));
}

static void make_sound_data_v(int *args, ptree *pt) 
{
  if (SOUND_DATA_RESULT) sound_data_free(SOUND_DATA_RESULT);
  SOUND_DATA_RESULT = c_make_sound_data(INT_ARG_1, INT_ARG_2);
}
static char *descr_make_sound_data_v(int *args, ptree *pt) 
{
  return(mus_format( SD_PT " = make_sound_data(" INT_PT ", " INT_PT ")", args[0], DESC_SOUND_DATA_RESULT, args[1], INT_ARG_1, args[2], INT_ARG_2));
}
static xen_value *make_sound_data_1(ptree *prog, xen_value **args, int num_args)
{
  args[0] = make_xen_value(R_SOUND_DATA, add_sound_data_to_ptree(prog, NULL), R_VARIABLE);
  add_obj_to_gcs(prog, R_SOUND_DATA, args[0]->addr);
  add_triple_to_ptree(prog, va_make_triple(make_sound_data_v, descr_make_sound_data_v, 3, args[0], args[1], args[2]));
  return(args[0]);
}





/* ---------------- CLM stuff ---------------- */

static char *descr_gen(int *args, ptree *pt, const char *which, int num_args) 
{
  char *buf, *str;
  int i, len;
  len = 256;
  buf = (char *)CALLOC(len, sizeof(char));
  str = (char *)CALLOC(32, sizeof(char));
  sprintf(buf, FLT_PT " = %s(" CLM_PT , args[0], FLOAT_RESULT, which, args[1], DESC_CLM_ARG_1);
  for (i = 1; i <= num_args; i++)
    {
      mus_snprintf(str, 32, ", " FLT_PT , args[i + 1], pt->dbls[args[i + 1]]);
      buf = snd_strcat(buf, str, &len);
    }
  buf = snd_strcat(buf, ")", &len);
  FREE(str);
  return(buf);
}

#define GEN_P(Name) \
  static char *descr_ ## Name ## _0p(int *args, ptree *pt) { \
    return(mus_format(BOOL_PT " = " #Name "?(" CLM_PT ")", args[0], B2S(BOOL_RESULT), args[1], DESC_CLM_ARG_1));} \
  static void Name ## _0p(int *args, ptree *pt) {BOOL_RESULT = (Int)mus_ ## Name ## _p(CLM_ARG_1);} \
  static xen_value * Name ## _p(ptree *prog, xen_value **args, int num_args) { \
    return(package(prog, R_BOOL, Name ## _0p, descr_ ## Name ## _0p, args, 1));} \
  static void Name ## _check(int *args, ptree *pt) { \
    if (!mus_ ## Name ## _p(CLM_ARG_1)) mus_error(MUS_NO_GEN, #Name ": bad arg: %s", (CLM_ARG_1) ? mus_name(CLM_ARG_1) : "null");} \
  static char *descr_ ## Name ## _check(int *args, ptree *pt) {return(mus_format("if (!" #Name "?(" CLM_PT ")) mus-error", args[1], DESC_CLM_ARG_1));}

#define GEN_P_1(Name) \
  static char *descr_ ## Name ## _0p(int *args, ptree *pt) { \
    return(mus_format(BOOL_PT " = " #Name "?(" CLM_PT ")", args[0], B2S(BOOL_RESULT), args[1], DESC_CLM_ARG_1));} \
  static void Name ## _0p(int *args, ptree *pt) {BOOL_RESULT = (Int)mus_ ## Name ## _p(CLM_ARG_1);} \
  static xen_value * Name ## _p(ptree *prog, xen_value **args, int num_args) { \
    return(package(prog, R_BOOL, Name ## _0p, descr_ ## Name ## _0p, args, 1));}

#define GEN2_0(Name) \
  static char *descr_ ## Name ## _0f(int *args, ptree *pt) {return(descr_gen(args, pt, #Name, 0));} \
  static void Name ## _0f(int *args, ptree *pt) {FLOAT_RESULT = mus_ ## Name (CLM_ARG_1, 0.0);}  
#define GEN2_0_OPT(Name) \
  static char *descr_ ## Name ## _0f(int *args, ptree *pt) {return(descr_gen(args, pt, #Name, 0));} \
  static void Name ## _0f(int *args, ptree *pt) {FLOAT_RESULT = mus_ ## Name ## _1 (CLM_ARG_1);}  
#define GEN1_0(Name) \
  static char *descr_ ## Name ## _0f(int *args, ptree *pt) {return(descr_gen(args, pt, #Name, 0));} \
  static void Name ## _0f(int *args, ptree *pt) {FLOAT_RESULT = mus_ ## Name (CLM_ARG_1);}  
#define GEN2_1(Name) \
  static char *descr_ ## Name ## _1f(int *args, ptree *pt) {return(descr_gen(args, pt, #Name, 1));} \
  static void Name ## _1f(int *args, ptree *pt) {FLOAT_RESULT = mus_ ## Name (CLM_ARG_1, FLOAT_ARG_2);}

#define GEN3(Name) \
  static char *descr_ ## Name ## _0f(int *args, ptree *pt) {return(descr_gen(args, pt, #Name, 0));} \
  static void Name ## _0f(int *args, ptree *pt) {FLOAT_RESULT = mus_ ## Name ## _1(CLM_ARG_1, 0.0);} \
  static char *descr_ ## Name ## _1f(int *args, ptree *pt) {return(descr_gen(args, pt, #Name, 1));} \
  static void Name ## _1f(int *args, ptree *pt) {FLOAT_RESULT = mus_ ## Name ## _1(CLM_ARG_1, FLOAT_ARG_2);} \
  static char *descr_ ## Name ## _2f(int *args, ptree *pt) {return(descr_gen(args, pt, #Name, 2));} \
  static void Name ## _2f(int *args, ptree *pt) {FLOAT_RESULT = mus_ ## Name (CLM_ARG_1, FLOAT_ARG_2, FLOAT_ARG_3);} \
  GEN_P(Name) \
  static xen_value * Name ## _1(ptree *prog, xen_value **args, int num_args) \
  { \
    if (run_safety == RUN_SAFE) safe_package(prog, R_BOOL, Name ## _check, descr_ ## Name ## _check, args, num_args); \
    if ((num_args > 1) && (args[2]->type == R_INT)) single_to_float(prog, args, 2); \
    if ((num_args > 2) && (args[3]->type == R_INT)) single_to_float(prog, args, 3); \
    if (num_args == 1) return(package(prog, R_FLOAT, Name ## _0f, descr_ ## Name ## _0f, args, 1)); \
    if (num_args == 2) return(package(prog, R_FLOAT, Name ## _1f, descr_ ## Name ## _1f, args, 2)); \
    return(package(prog, R_FLOAT, Name ## _2f, descr_ ## Name ## _2f, args, 3)); \
  }

static char *descr_oscil_0f_1(int *args, ptree *pt) {return(descr_gen(args, pt, "oscil", 0));}
static void oscil_0f_1(int *args, ptree *pt) {FLOAT_RESULT = mus_oscil_0(CLM_ARG_1);}
static char *descr_oscil_1f_1(int *args, ptree *pt) {return(descr_gen(args, pt, "oscil", 1));}
static void oscil_1f_1(int *args, ptree *pt) {FLOAT_RESULT = mus_oscil_1(CLM_ARG_1, FLOAT_ARG_2);}
static char *descr_oscil_2f(int *args, ptree *pt) {return(descr_gen(args, pt, "oscil", 2));}
static void oscil_2f(int *args, ptree *pt) {FLOAT_RESULT = mus_oscil(CLM_ARG_1, FLOAT_ARG_2, FLOAT_ARG_3);}
GEN_P(oscil)

static xen_value *oscil_1(ptree *prog, xen_value **args, int num_args)
{
  if (run_safety == RUN_SAFE) safe_package(prog, R_BOOL, oscil_check, descr_oscil_check, args, num_args);
  if ((num_args > 1) && (args[2]->type == R_INT)) single_to_float(prog, args, 2);
  if ((num_args > 2) && (args[3]->type == R_INT)) single_to_float(prog, args, 3);
  if ((num_args == 1) || ((num_args == 2) && (args[2]->constant == R_CONSTANT) && (prog->dbls[args[2]->addr] == 0.0)))
    return(package(prog, R_FLOAT, oscil_0f_1, descr_oscil_0f_1, args, 1));
  if ((num_args == 2) || ((num_args == 3) && (args[3]->constant == R_CONSTANT) && (prog->dbls[args[3]->addr] == 0.0)))
    return(package(prog, R_FLOAT, oscil_1f_1, descr_oscil_1f_1, args, 2));
  return(package(prog, R_FLOAT, oscil_2f, descr_oscil_2f, args, 3));
}

#define GEN2(Name) \
  GEN2_0(Name) \
  GEN2_1(Name) \
  GEN_P(Name) \
  static xen_value * Name ## _1(ptree *prog, xen_value **args, int num_args) \
  { \
    if (run_safety == RUN_SAFE) safe_package(prog, R_BOOL, Name ## _check, descr_ ## Name ## _check, args, 1); \
    if ((num_args > 1) && (args[2]->type == R_INT)) single_to_float(prog, args, 2); \
    if (num_args == 1) return(package(prog, R_FLOAT, Name ## _0f, descr_ ## Name ## _0f, args, 1)); \
    return(package(prog, R_FLOAT, Name ## _1f, descr_ ## Name ## _1f, args, 2)); \
  }
#define GEN2_OPT(Name) \
  GEN2_0_OPT(Name) \
  GEN2_1(Name) \
  GEN_P(Name) \
  static xen_value * Name ## _1(ptree *prog, xen_value **args, int num_args) \
  { \
    if (run_safety == RUN_SAFE) safe_package(prog, R_BOOL, Name ## _check, descr_ ## Name ## _check, args, 1); \
    if ((num_args > 1) && (args[2]->type == R_INT)) single_to_float(prog, args, 2); \
    if (num_args == 1) return(package(prog, R_FLOAT, Name ## _0f, descr_ ## Name ## _0f, args, 1)); \
    return(package(prog, R_FLOAT, Name ## _1f, descr_ ## Name ## _1f, args, 2)); \
  }

#define GEN1(Name) \
  GEN1_0(Name) \
  GEN_P(Name) \
  static xen_value * Name ## _1(ptree *prog, xen_value **args, int num_args) \
  { \
    if (run_safety == RUN_SAFE) safe_package(prog, R_BOOL, Name ## _check, descr_ ## Name ## _check, args, 1); \
    return(package(prog, R_FLOAT, Name ## _0f, descr_ ## Name ## _0f, args, 1)); \
  }

#define GEN0(Name) \
  GEN1_0(Name) \
  static xen_value * mus_ ## Name ## _0(ptree *prog, xen_value **args, int num_args) \
  { \
    return(package(prog, R_FLOAT, Name ## _0f, descr_ ## Name ## _0f, args, 1)); \
  }

static char *descr_int_gen0(int *args, ptree *pt, const char *which)
{
  return(mus_format( INT_PT " = %s(" CLM_PT , args[0], INT_RESULT, which, args[1], DESC_CLM_ARG_1));
}
#define INT_GEN0(Name) \
  static char *descr_ ## Name ## _0i(int *args, ptree *pt) {return(descr_int_gen0(args, pt, #Name));} \
  static void Name ## _0i(int *args, ptree *pt) {INT_RESULT = mus_ ## Name (CLM_ARG_1);} \
  static xen_value * mus_ ## Name ## _0(ptree *prog, xen_value **args, int num_args) \
  { \
    return(package(prog, R_INT, Name ## _0i, descr_ ## Name ## _0i, args, 1)); \
  }

GEN1(env)
GEN3(notch)
GEN3(comb)
GEN3(delay)
GEN3(all_pass)
GEN2(average)
GEN2(rand)
GEN2(rand_interp)
GEN2(sum_of_cosines)
GEN2(sum_of_sines)
GEN3(ssb_am)
GEN2(sawtooth_wave)
GEN2(pulse_train)
GEN2(square_wave)
GEN2(triangle_wave)
GEN3(asymmetric_fm)
GEN2(sine_summation)
GEN2(one_zero)
GEN2(one_pole)
GEN2(two_zero)
GEN2(two_pole)
GEN2(formant)
GEN3(waveshape)
GEN2(filter)
GEN2(fir_filter)
GEN2(iir_filter)
GEN1(readin)

GEN2_OPT(wave_train)
GEN2_OPT(table_lookup)

static char *descr_tap_0f(int *args, ptree *pt) {return(descr_gen(args, pt, "tap", 0));}
static void tap_0f(int *args, ptree *pt) {FLOAT_RESULT = mus_tap_1(CLM_ARG_1);}  
static char *descr_tap_1f(int *args, ptree *pt) {return(descr_gen(args, pt, "tap", 1));}
static void tap_1f(int *args, ptree *pt) {FLOAT_RESULT = mus_tap(CLM_ARG_1, FLOAT_ARG_2);}

static xen_value *tap_1(ptree *prog, xen_value **args, int num_args)
{
  if (run_safety == RUN_SAFE) safe_package(prog, R_BOOL, delay_check, descr_delay_check, args, 1);
  if (num_args == 1)
    return(package(prog, R_FLOAT, tap_0f, descr_tap_0f, args, 1));
  if (args[2]->type == R_INT) single_to_float(prog, args, 2);
  return(package(prog, R_FLOAT, tap_1f, descr_tap_1f, args, 2));
}

GEN0(buffer_to_sample)
GEN0(increment)
GEN0(frequency)
GEN0(phase)
GEN0(scaler)
GEN0(width)
GEN0(offset)
GEN0(formant_radius)
GEN0(feedforward)
GEN0(feedback)

INT_GEN0(hop)
INT_GEN0(channels)
INT_GEN0(location)
INT_GEN0(ramp)
INT_GEN0(order)
INT_GEN0(length)
INT_GEN0(cosines)
INT_GEN0(channel)

GEN_P(buffer)
GEN_P_1(buffer_empty)
GEN_P_1(buffer_full)
GEN_P(frame)
GEN_P(mixer)
GEN_P(file_to_sample)
GEN_P(sample_to_file)
GEN_P(file_to_frame)
GEN_P(frame_to_file)
GEN_P(locsig)
GEN_P_1(input)
GEN_P_1(output)

GEN2_1(sample_to_buffer)
static xen_value *sample_to_buffer_1(ptree *prog, xen_value **args, int num_args)
{
  if (run_safety == RUN_SAFE) safe_package(prog, R_BOOL, buffer_check, descr_buffer_check, args, 1);
  if (args[2]->type == R_INT) single_to_float(prog, args, 2);
  return(package(prog, R_FLOAT, sample_to_buffer_1f, descr_sample_to_buffer_1f, args, 2));
}


static char *descr_close_0(int *args, ptree *pt)
{
  return(mus_format( INT_PT " = mus-close(" CLM_PT , args[0], INT_RESULT, args[1], DESC_CLM_ARG_1));
}
static void close_0(int *args, ptree *pt) {INT_RESULT = mus_close_file(CLM_ARG_1);}
static xen_value *mus_close_0(ptree *prog, xen_value **args, int num_args)
{
  return(package(prog, R_INT, close_0, descr_close_0, args, 1));
}

static char *descr_set_formant_radius_and_frequency_2f(int *args, ptree *pt) 
{
  return(descr_gen(args, pt, S_mus_set_formant_radius_and_frequency, 2));
}
static void set_formant_radius_and_frequency_2f(int *args, ptree *pt) 
{
  mus_set_formant_radius_and_frequency(CLM_ARG_1, FLOAT_ARG_2, FLOAT_ARG_3);
}  
static xen_value *set_formant_radius_and_frequency_1(ptree *prog, xen_value **args, int num_args)
{
  if (run_safety == RUN_SAFE) safe_package(prog, R_BOOL, formant_check, descr_formant_check, args, 1);
  if (args[2]->type == R_INT) single_to_float(prog, args, 2);
  if (args[3]->type == R_INT) single_to_float(prog, args, 3);
  return(package(prog, R_BOOL, set_formant_radius_and_frequency_2f, descr_set_formant_radius_and_frequency_2f, args, 3));
}


static char *descr_move_locsig_2f(int *args, ptree *pt) {return(descr_gen(args, pt, "move-locsig", 2));}
static void move_locsig_2f(int *args, ptree *pt) {mus_move_locsig(CLM_ARG_1, FLOAT_ARG_2, FLOAT_ARG_3);}  
static xen_value *move_locsig_1(ptree *prog, xen_value **args, int num_args)
{ 
  if (run_safety == RUN_SAFE) safe_package(prog, R_BOOL, locsig_check, descr_locsig_check, args, 1);
  return(package(prog, R_BOOL, move_locsig_2f, descr_move_locsig_2f, args, 3));
}


static char *descr_str_gen0(int *args, ptree *pt, const char *which)
{
  return(mus_format( STR_PT " = %s(" CLM_PT ")", args[0], STRING_RESULT, which, args[1], DESC_CLM_ARG_1));
}
#define STR_GEN0(Name) \
  static char *descr_ ## Name ## _0s(int *args, ptree *pt) {return(descr_str_gen0(args, pt, #Name));} \
  static void Name ## _0s(int *args, ptree *pt) \
    { \
      if (STRING_RESULT) FREE(STRING_RESULT); \
      STRING_RESULT = copy_string(mus_ ## Name (CLM_ARG_1)); \
    } \
  static xen_value * mus_ ## Name ## _0(ptree *prog, xen_value **args, int num_args) \
  { \
    return(package(prog, R_STRING, Name ## _0s, descr_ ## Name ## _0s, args, 1)); \
  }

STR_GEN0(name)
STR_GEN0(describe)
STR_GEN0(file_name)


static char *descr_ref_gen0(int *args, ptree *pt, const char *which)
{
  return(mus_format( FLT_PT " = %s(" CLM_PT ", " INT_PT ")", args[0], FLOAT_RESULT, which, args[1], DESC_CLM_ARG_1, args[2], INT_ARG_2));
}
#define REF_GEN0(Name, SName) \
  static char *descr_ ## Name ## _0r(int *args, ptree *pt) {return(descr_ref_gen0(args, pt, #Name));} \
  static void Name ## _0r(int *args, ptree *pt) {FLOAT_RESULT = mus_ ## Name (CLM_ARG_1, INT_ARG_2);} \
  static xen_value * Name ## _0(ptree *prog, xen_value **args, int num_args) \
  { \
    if (run_safety == RUN_SAFE) package(prog, R_BOOL, SName ## _check, descr_ ## SName ## _check, args, 1); \
    return(package(prog, R_FLOAT, Name ## _0r, descr_ ## Name ## _0r, args, 2)); \
  }

REF_GEN0(frame_ref, frame)
REF_GEN0(locsig_ref, locsig)
REF_GEN0(locsig_reverb_ref, locsig)


static char *descr_set_gen0(int *args, ptree *pt, const char *which)
{
  return(mus_format("%s(" CLM_PT ", " INT_PT ", " FLT_PT ")", which, args[1], DESC_CLM_ARG_1, args[2], INT_ARG_2, args[3], FLOAT_ARG_3));
}
static char *descr_set_gen0_i(int *args, ptree *pt, const char *which)
{
  return(mus_format("%s(" CLM_PT ", " INT_PT ", " INT_PT ")", which, args[1], DESC_CLM_ARG_1, args[2], INT_ARG_2, args[3], INT_ARG_3));
}
#define SET_GEN0(Name, SName) \
  static char *descr_ ## Name ## _0r(int *args, ptree *pt) {return(descr_set_gen0(args, pt, #Name));} \
  static void Name ## _0r(int *args, ptree *pt) {mus_ ## Name (CLM_ARG_1, INT_ARG_2, FLOAT_ARG_3);} \
  static char *descr_ ## Name ## _ir(int *args, ptree *pt) {return(descr_set_gen0_i(args, pt, #Name));} \
  static void Name ## _ir(int *args, ptree *pt) {mus_ ## Name (CLM_ARG_1, INT_ARG_2, (Float)(INT_ARG_3));} \
  static xen_value * Name ## _2(ptree *prog, xen_value **args, int num_args) \
  { \
    if (run_safety == RUN_SAFE) safe_package(prog, R_BOOL, SName ## _check, descr_ ## SName ## _check, args, 1); \
    if (args[3]->type == R_FLOAT) \
      return(package(prog, R_FLOAT, Name ## _0r, descr_ ## Name ## _0r, args, 3)); \
    return(package(prog, R_FLOAT, Name ## _ir, descr_ ## Name ## _ir, args, 3)); \
  } \
  static void Name ## _1(ptree *prog, xen_value *in_v, xen_value *in_v1, xen_value *in_v2, xen_value *v) \
  { \
    add_triple_to_ptree(prog, va_make_triple( Name ## _0r, descr_ ## Name ## _0r, 4, NULL, in_v, in_v1, v)); \
  }

SET_GEN0(frame_set, frame)
SET_GEN0(locsig_set, locsig)
SET_GEN0(locsig_reverb_set, locsig)


static char *descr_mixer_ref_0(int *args, ptree *pt)
{
  return(mus_format( FLT_PT " = mixer-ref(" CLM_PT ", " INT_PT ", " INT_PT ")", 
		    args[0], FLOAT_RESULT, args[1], DESC_CLM_ARG_1, args[2], INT_ARG_2, args[3], INT_ARG_3));
}
static void mixer_ref_0(int *args, ptree *pt) {FLOAT_RESULT = mus_mixer_ref(CLM_ARG_1, INT_ARG_2, INT_ARG_3);}
static xen_value *mixer_ref_1(ptree *prog, xen_value **args, int num_args)
{
  if (run_safety == RUN_SAFE) package(prog, R_BOOL, mixer_check, descr_mixer_check, args, 1);
  return(package(prog, R_FLOAT, mixer_ref_0, descr_mixer_ref_0, args, 3));
}
static char *descr_mixer_set_0(int *args, ptree *pt)
{
  return(mus_format("mixer-set!(" CLM_PT ", " INT_PT ", " INT_PT ", " FLT_PT ")", 
		    args[1], DESC_CLM_ARG_1, args[2], INT_ARG_2, args[3], INT_ARG_3, args[4], FLOAT_ARG_4));
}
static void mixer_set_0(int *args, ptree *pt) 
{
  mus_mixer_set(CLM_ARG_1, INT_ARG_2, INT_ARG_3, FLOAT_ARG_4);
}
static char *descr_mixer_set_i(int *args, ptree *pt)
{
  return(mus_format("mixer-set!(" CLM_PT ", " INT_PT ", " INT_PT ", " INT_PT ")", 
		    args[1], DESC_CLM_ARG_1, args[2], INT_ARG_2, args[3], INT_ARG_3, args[4], INT_ARG_4));
}
static void mixer_set_i(int *args, ptree *pt) 
{
  mus_mixer_set(CLM_ARG_1, INT_ARG_2, INT_ARG_3, (Float)INT_ARG_4);
}
static xen_value *mixer_set_2(ptree *prog, xen_value **args, int num_args)
{
  /* mixer-set! */
  if (run_safety == RUN_SAFE) safe_package(prog, R_BOOL, mixer_check, descr_mixer_check, args, 1);
  if (args[4]->type == R_FLOAT)
    return(package(prog, R_FLOAT, mixer_set_0, descr_mixer_set_0, args, 4));
  return(package(prog, R_FLOAT, mixer_set_i, descr_mixer_set_i, args, 4));
}
static void mixer_set_1(ptree *prog, xen_value *in_v, xen_value *in_v1, xen_value *in_v2, xen_value *v)
{
  /* set! mixer-ref */
  add_triple_to_ptree(prog, va_make_triple(mixer_set_0, descr_mixer_set_0, 5, NULL, in_v, in_v1, in_v2, v));
}

/* ---------------- xcoeff/ycoeff ---------------- */

static char *descr_xcoeff_0(int *args, ptree *pt)
{
  return(mus_format( FLT_PT " = mus-xcoeff(" CLM_PT ", " INT_PT ")", args[0], FLOAT_RESULT, args[1], DESC_CLM_ARG_1, args[2], INT_ARG_2));
}
static void xcoeff_0(int *args, ptree *pt) {FLOAT_RESULT = mus_xcoeff(CLM_ARG_1, INT_ARG_2);}
static xen_value *mus_xcoeff_1(ptree *prog, xen_value **args, int num_args) {return(package(prog, R_FLOAT, xcoeff_0, descr_xcoeff_0, args, 2));}

static char *descr_set_xcoeff_0(int *args, ptree *pt)
{
  return(mus_format("mus-set-xcoeff(" CLM_PT ", " INT_PT ", " FLT_PT ")", args[1], DESC_CLM_ARG_1, args[2], INT_ARG_2, args[3], FLOAT_ARG_3));
}
static void set_xcoeff_0(int *args, ptree *pt) {mus_set_xcoeff(CLM_ARG_1, INT_ARG_2, FLOAT_ARG_3);}
static void mus_set_xcoeff_1(ptree *prog, xen_value *in_v, xen_value *in_v1, xen_value *in_v2, xen_value *v)
{
  add_triple_to_ptree(prog, va_make_triple(set_xcoeff_0, descr_set_xcoeff_0, 4, NULL, in_v, in_v1, v));
}


static char *descr_ycoeff_0(int *args, ptree *pt)
{
  return(mus_format( FLT_PT " = mus-ycoeff(" CLM_PT ", " INT_PT ")", args[0], FLOAT_RESULT, args[1], DESC_CLM_ARG_1, args[2], INT_ARG_2));
}
static void ycoeff_0(int *args, ptree *pt) {FLOAT_RESULT = mus_ycoeff(CLM_ARG_1, INT_ARG_2);}
static xen_value *mus_ycoeff_1(ptree *prog, xen_value **args, int num_args) {return(package(prog, R_FLOAT, ycoeff_0, descr_ycoeff_0, args, 2));}

static char *descr_set_ycoeff_0(int *args, ptree *pt)
{
  return(mus_format("mus-set-ycoeff(" CLM_PT ", " INT_PT ", " FLT_PT ")", args[1], DESC_CLM_ARG_1, args[2], INT_ARG_2, args[3], FLOAT_ARG_3));
}
static void set_ycoeff_0(int *args, ptree *pt) {mus_set_ycoeff(CLM_ARG_1, INT_ARG_2, FLOAT_ARG_3);}
static void mus_set_ycoeff_1(ptree *prog, xen_value *in_v, xen_value *in_v1, xen_value *in_v2, xen_value *v)
{
  add_triple_to_ptree(prog, va_make_triple(set_ycoeff_0, descr_set_ycoeff_0, 4, NULL, in_v, in_v1, v));
}


static char *descr_set_int_gen0(int *args, ptree *pt, const char *which)
{
  return(mus_format("mus-%s(" CLM_PT ") = " INT_PT , which, args[0], DESC_CLM_RESULT, args[1], INT_ARG_1));
}
#define SET_INT_GEN0(Name) \
  static char *descr_set_ ## Name ## _i(int *args, ptree *pt) {return(descr_set_int_gen0(args, pt, #Name));} \
  static void set_ ## Name ## _i(int *args, ptree *pt) {mus_set_ ## Name (CLM_RESULT, INT_ARG_1);} \
  static void mus_set_ ## Name ## _1(ptree *prog, xen_value *in_v, xen_value *in_v1, xen_value *in_v2, xen_value *v) \
  { \
    add_triple_to_ptree(prog, va_make_triple(set_ ## Name ## _i, descr_set_ ## Name ## _i, 2, in_v, v)); \
  }

SET_INT_GEN0(location)
SET_INT_GEN0(ramp)
SET_INT_GEN0(hop)
SET_INT_GEN0(length)
SET_INT_GEN0(cosines)

static char *descr_set_dbl_gen0(int *args, ptree *pt, const char *which)
{
  return(mus_format("mus-%s(" CLM_PT ") = " FLT_PT , which, args[0], DESC_CLM_RESULT, args[1], FLOAT_ARG_1));
}
#define SET_DBL_GEN0(Name) \
  static char *descr_set_ ## Name ## _f(int *args, ptree *pt) {return(descr_set_dbl_gen0(args, pt, #Name));} \
  static void set_ ## Name ## _f(int *args, ptree *pt) {mus_set_ ## Name (CLM_RESULT, FLOAT_ARG_1);} \
  static void mus_set_ ## Name ## _1(ptree *prog, xen_value *in_v, xen_value *in_v1, xen_value *in_v2, xen_value *v) \
  { \
    add_triple_to_ptree(prog, va_make_triple(set_ ## Name ## _f, descr_set_ ## Name ## _f, 2, in_v, v)); \
  }

SET_DBL_GEN0(increment)
SET_DBL_GEN0(scaler)
SET_DBL_GEN0(width)
SET_DBL_GEN0(feedback)
SET_DBL_GEN0(feedforward)
SET_DBL_GEN0(phase)
SET_DBL_GEN0(frequency)
SET_DBL_GEN0(formant_radius)


/* ---------------- mus-bank ---------------- */

static char *descr_mus_bank_1f(int *args, ptree *pt) 
{
  return(mus_format( FLT_PT " = mus-bank(" VECT_PT ", " VCT_PT ")", args[0], FLOAT_RESULT, args[1], DESC_VECT_ARG_1, args[2], DESC_VCT_ARG_2));
}
static void mus_bank_1f(int *args, ptree *pt) 
{
  FLOAT_RESULT = mus_bank(VECT_ARG_1->data.gens, VCT_ARG_2->data, NULL, NULL, VCT_ARG_2->length);
}

static char *descr_mus_bank_2f(int *args, ptree *pt) 
{
  return(mus_format( FLT_PT " = mus-bank(" VECT_PT ", " VCT_PT ", " VCT_PT ")", 
		     args[0], FLOAT_RESULT, args[1], DESC_VECT_ARG_1, args[2], DESC_VCT_ARG_2, args[3], DESC_VCT_ARG_3));
}
static void mus_bank_2f(int *args, ptree *pt) 
{
  FLOAT_RESULT = mus_bank(VECT_ARG_1->data.gens, VCT_ARG_2->data, VCT_ARG_3->data, NULL, VCT_ARG_2->length);
}

static char *descr_mus_bank_3f(int *args, ptree *pt) 
{
  return(mus_format( FLT_PT " = mus-bank(" VECT_PT ", " VCT_PT ", " VCT_PT ", " VCT_PT ")", 
		     args[0], FLOAT_RESULT, args[1], DESC_VECT_ARG_1, args[2], DESC_VCT_ARG_2, 
		     args[3], DESC_VCT_ARG_3, args[4], DESC_VCT_ARG_4));
}
static void mus_bank_3f(int *args, ptree *pt) 
{
  FLOAT_RESULT = mus_bank(VECT_ARG_1->data.gens, VCT_ARG_2->data, 
			  VCT_ARG_3->data, VCT_ARG_4->data, VCT_ARG_2->length);
}

static xen_value *mus_bank_1(ptree *prog, xen_value **args, int num_args) 
{
  if (num_args == 2) return(package(prog, R_FLOAT, mus_bank_1f, descr_mus_bank_1f, args, 2));
  if (num_args == 3) return(package(prog, R_FLOAT, mus_bank_2f, descr_mus_bank_2f, args, 3));
  return(package(prog, R_FLOAT, mus_bank_3f, descr_mus_bank_3f, args, 4));
}


/* ---------------- oscil-bank ---------------- */

static char *descr_oscil_bank_2f(int *args, ptree *pt) 
{
  return(mus_format( FLT_PT " = oscil-bank(" VCT_PT ", " VECT_PT ", " VCT_PT ")", 
		     args[0], FLOAT_RESULT, args[1], DESC_VCT_ARG_1, args[2], DESC_VECT_ARG_2, args[3], DESC_VCT_ARG_3));
}
static void oscil_bank_2f(int *args, ptree *pt) 
{
  FLOAT_RESULT = mus_oscil_bank(VCT_ARG_1->data, VECT_ARG_2->data.gens, VCT_ARG_3->data, VCT_ARG_2->length);
}

static char *descr_oscil_bank_3f(int *args, ptree *pt) 
{
  return(mus_format( FLT_PT " = oscil-bank(" VCT_PT ", " VECT_PT ", " VCT_PT ", " INT_PT ")", 
		     args[0], FLOAT_RESULT, args[1], DESC_VCT_ARG_1, args[2], DESC_VECT_ARG_2, args[3], DESC_VCT_ARG_3, args[4], INT_ARG_4));
}
static void oscil_bank_3f(int *args, ptree *pt) 
{
  FLOAT_RESULT = mus_oscil_bank(VCT_ARG_1->data, VECT_ARG_2->data.gens,	VCT_ARG_3->data, INT_ARG_4);
}

static xen_value *oscil_bank_1(ptree *prog, xen_value **args, int num_args) 
{
  if (num_args == 3) return(package(prog, R_FLOAT, oscil_bank_2f, descr_oscil_bank_2f, args, 3));
  return(package(prog, R_FLOAT, oscil_bank_3f, descr_oscil_bank_3f, args, 4));
}


/* ---------------- polynomial ---------------- */
static char *descr_polynomial_1f(int *args, ptree *pt) 
{
  return(mus_format( FLT_PT " = polynomial(" VCT_PT ", " FLT_PT ")", args[0], FLOAT_RESULT, args[1], DESC_VCT_ARG_1, args[2], FLOAT_ARG_2));
}
static void polynomial_1f(int *args, ptree *pt) 
{
  FLOAT_RESULT = mus_polynomial(VCT_ARG_1->data, FLOAT_ARG_2, VCT_ARG_1->length);
}
static xen_value *polynomial_1(ptree *prog, xen_value **args, int num_args) {return(package(prog, R_FLOAT, polynomial_1f, descr_polynomial_1f, args, 2));}

/* ---------------- mus-fft ---------------- */
static char *descr_mus_fft_2v(int *args, ptree *pt) 
{
  return(mus_format("mus-fft(" VCT_PT ", " VCT_PT ")", args[1], DESC_VCT_ARG_1, args[2], DESC_VCT_ARG_2));
}
static void mus_fft_2v(int *args, ptree *pt) 
{
  mus_fft(VCT_ARG_1->data, VCT_ARG_2->data, VCT_ARG_1->length, 1);
  VCT_RESULT = VCT_ARG_1;
}
static char *descr_mus_fft_2v_1(int *args, ptree *pt) 
{
  return(mus_format("mus-fft(" VCT_PT ", " VCT_PT ", " INT_PT ")", args[1], DESC_VCT_ARG_1, args[2], DESC_VCT_ARG_2, args[3], INT_ARG_3));
}
static void mus_fft_2v_1(int *args, ptree *pt) 
{
  mus_fft(VCT_ARG_1->data, VCT_ARG_2->data, INT_ARG_3, 1);
  VCT_RESULT = VCT_ARG_1;
}
static char *descr_mus_fft_2v_2(int *args, ptree *pt) 
{
  return(mus_format("mus-fft(" VCT_PT ", " VCT_PT ", " INT_PT ", " INT_PT ")", 
		    args[1], DESC_VCT_ARG_1, args[2], DESC_VCT_ARG_2, args[3], INT_ARG_3, args[4], INT_ARG_4));
}
static void mus_fft_2v_2(int *args, ptree *pt) 
{
  mus_fft(VCT_ARG_1->data, VCT_ARG_2->data, INT_ARG_3, INT_ARG_4);
  VCT_RESULT = VCT_ARG_1;
}
static xen_value *mus_fft_1(ptree *prog, xen_value **args, int num_args) 
{
  if (run_safety == RUN_SAFE) package(prog, R_BOOL, vct_check_1, descr_vct_check_1, args, 1);
  if (run_safety == RUN_SAFE) package(prog, R_BOOL, vct_check_2, descr_vct_check_2, args, 2);
  if (num_args == 2) return(package(prog, R_VCT, mus_fft_2v, descr_mus_fft_2v, args, 2));
  if (num_args == 3) return(package(prog, R_VCT, mus_fft_2v_1, descr_mus_fft_2v_1, args, 3));
  return(package(prog, R_VCT, mus_fft_2v_2, descr_mus_fft_2v_2, args, 4));
}

/* ----------------fft ---------------- */
static char *descr_fft_2v(int *args, ptree *pt) 
{
  return(mus_format("fft(" VCT_PT ", " VCT_PT ")", args[1], DESC_VCT_ARG_1, args[2], DESC_VCT_ARG_2));
}
static void fft_2v(int *args, ptree *pt) 
{
  mus_fft(VCT_ARG_1->data, VCT_ARG_2->data, VCT_ARG_1->length, 1);
  VCT_RESULT = VCT_ARG_1;
}
static char *descr_fft_2v_1(int *args, ptree *pt) 
{
  return(mus_format("fft(" VCT_PT ", " VCT_PT ", len, " INT_PT ")", args[1], DESC_VCT_ARG_1, args[2], DESC_VCT_ARG_2, args[3], INT_ARG_3));
}
static void fft_2v_1(int *args, ptree *pt) 
{
  mus_fft(VCT_ARG_1->data, VCT_ARG_2->data, VCT_ARG_1->length, INT_ARG_3);
  VCT_RESULT = VCT_ARG_1;
}
static xen_value *fft_1(ptree *prog, xen_value **args, int num_args) 
{
  if (run_safety == RUN_SAFE) package(prog, R_BOOL, vct_check_1, descr_vct_check_1, args, 1);
  if (run_safety == RUN_SAFE) package(prog, R_BOOL, vct_check_2, descr_vct_check_2, args, 2);
  if (num_args == 2) return(package(prog, R_VCT, fft_2v, descr_fft_2v, args, 2));
  return(package(prog, R_VCT, fft_2v_1, descr_fft_2v_1, args, 3));
}

/* ---------------- file->sample ---------------- */
static char *descr_file_to_sample_1f(int *args, ptree *pt) 
{
  return(mus_format( FLT_PT " = file->sample(" CLM_PT ", " INT_PT ")", args[0], FLOAT_RESULT, args[1], DESC_CLM_ARG_1, args[2], INT_ARG_2));
}
static char *descr_file_to_sample_2f(int *args, ptree *pt) 
{
  return(mus_format( FLT_PT " = file->sample(" CLM_PT ", " INT_PT ", " INT_PT ")", 
		    args[0], FLOAT_RESULT, args[1], DESC_CLM_ARG_1, args[2], INT_ARG_2, args[3], INT_ARG_3));
}
static void file_to_sample_1f(int *args, ptree *pt) {FLOAT_RESULT = mus_file_to_sample(CLM_ARG_1, INT_ARG_2, 0);}
static void file_to_sample_2f(int *args, ptree *pt) {FLOAT_RESULT = mus_file_to_sample(CLM_ARG_1, INT_ARG_2, INT_ARG_3);}
static xen_value *file_to_sample_1(ptree *prog, xen_value **args, int num_args) 
{
  if (run_safety == RUN_SAFE) package(prog, R_BOOL, file_to_sample_check, descr_file_to_sample_check, args, 1);
  if (num_args == 2) return(package(prog, R_FLOAT, file_to_sample_1f, descr_file_to_sample_1f, args, 2));
  return(package(prog, R_FLOAT, file_to_sample_2f, descr_file_to_sample_2f, args, 3));
}

/* ---------------- snd->sample ---------------- */
static char *descr_snd_to_sample_1f(int *args, ptree *pt) 
{
  return(mus_format( FLT_PT " = snd->sample(" CLM_PT ", " INT_PT ")", args[0], FLOAT_RESULT, args[1], DESC_CLM_ARG_1, args[2], INT_ARG_2));
}
static char *descr_snd_to_sample_2f(int *args, ptree *pt) 
{
  return(mus_format( FLT_PT " = snd->sample(" CLM_PT ", " INT_PT ", " INT_PT ")", 
		    args[0], FLOAT_RESULT, args[1], DESC_CLM_ARG_1, args[2], INT_ARG_2, args[3], INT_ARG_3));
}
static void snd_to_sample_1f(int *args, ptree *pt) {FLOAT_RESULT = snd_to_sample_read(CLM_ARG_1, INT_ARG_2, 0);}
static void snd_to_sample_2f(int *args, ptree *pt) {FLOAT_RESULT = snd_to_sample_read(CLM_ARG_1, INT_ARG_2, INT_ARG_3);}
static xen_value *snd_to_sample_1(ptree *prog, xen_value **args, int num_args) 
{
  if (num_args == 2) return(package(prog, R_FLOAT, snd_to_sample_1f, descr_snd_to_sample_1f, args, 2));
  return(package(prog, R_FLOAT, snd_to_sample_2f, descr_snd_to_sample_2f, args, 3));
}

static char *descr_snd_to_sample_0p(int *args, ptree *pt)
{
  return(mus_format(BOOL_PT " = snd_to_sample?(" CLM_PT ")", args[0], B2S(BOOL_RESULT), args[1], DESC_CLM_ARG_1));
}
 
static void snd_to_sample_0p(int *args, ptree *pt) {BOOL_RESULT = (Int)snd_to_sample_p(CLM_ARG_1);}
static xen_value *snd_to_sample_1p(ptree *prog, xen_value **args, int num_args)
{
  return(package(prog, R_BOOL, snd_to_sample_0p, descr_snd_to_sample_0p, args, 1));
}



/* ---------------- sample->file ---------------- */
static char *descr_sample_to_file_4(int *args, ptree *pt) 
{
  return(mus_format( FLT_PT " = sample->file(" CLM_PT ", " INT_PT ", " INT_PT ", " FLT_PT ")", 
		    args[0], FLOAT_RESULT, args[1], DESC_CLM_ARG_1, args[2], INT_ARG_2, args[3], INT_ARG_3, args[4], FLOAT_ARG_4));
}
static void sample_to_file_4(int *args, ptree *pt) {FLOAT_RESULT = mus_sample_to_file(CLM_ARG_1, INT_ARG_2, INT_ARG_3, FLOAT_ARG_4);}
static xen_value *sample_to_file_1(ptree *prog, xen_value **args, int num_args) 
{
  if (run_safety == RUN_SAFE) safe_package(prog, R_BOOL, sample_to_file_check, descr_sample_to_file_check, args, 1);
  return(package(prog, R_FLOAT, sample_to_file_4, descr_sample_to_file_4, args, 4));
}


/* ---------------- locsig ---------------- */
static char *descr_locsig_3(int *args, ptree *pt) 
{
  return(mus_format( CLM_PT " = locsig((" CLM_PT ", " INT_PT ", " FLT_PT ")", 
		    args[0], DESC_CLM_RESULT, args[1], DESC_CLM_ARG_1, args[2], INT_ARG_2, args[3], FLOAT_ARG_3));
}
static void locsig_3(int *args, ptree *pt) 
{
  /* frame result should not be freed */
  CLM_RESULT = mus_locsig(CLM_ARG_1, INT_ARG_2, FLOAT_ARG_3);
}
static xen_value *locsig_1(ptree *prog, xen_value **args, int num_args) 
{
  if (run_safety == RUN_SAFE) safe_package(prog, R_BOOL, locsig_check, descr_locsig_check, args, 1);
  return(package(prog, R_CLM, locsig_3, descr_locsig_3, args, 3));
}


/* ---------------- env-interp ---------------- */
static char *descr_env_interp_2(int *args, ptree *pt) 
{
  return(mus_format( FLT_PT " = env-interp(" FLT_PT ", " CLM_PT ")", args[0], FLOAT_RESULT, args[1], FLOAT_ARG_1, args[2], DESC_CLM_ARG_2));
}
static void env_interp_2(int *args, ptree *pt) {FLOAT_RESULT = mus_env_interp(FLOAT_ARG_1, CLM_ARG_2);}
static xen_value *env_interp_1(ptree *prog, xen_value **args, int num_args) 
{
  return(package(prog, R_FLOAT, env_interp_2, descr_env_interp_2, args, 2));
}


/* ---------------- frame+ etc ---------------- */

#define FRAME_OP(CName, SName) \
static char *descr_ ## CName ## _2(int *args, ptree *pt) \
{ \
  return(mus_format( CLM_PT " = " #SName "(" CLM_PT ", " CLM_PT ")", \
		    args[0], DESC_CLM_RESULT, args[1], DESC_CLM_ARG_1, args[2], DESC_CLM_ARG_2)); \
} \
static char *descr_ ## CName ## _3(int *args, ptree *pt) \
{ \
  return(mus_format( CLM_PT " = " #SName "(" CLM_PT ", " CLM_PT ", " CLM_PT ")", \
		    args[0], DESC_CLM_RESULT, args[1], DESC_CLM_ARG_1, args[2], DESC_CLM_ARG_2, args[3], DESC_CLM_ARG_3)); \
} \
static void CName ## _2(int *args, ptree *pt) \
{ \
  if (CLM_RESULT) mus_free(CLM_RESULT); \
  CLM_RESULT = CName(CLM_ARG_1, CLM_ARG_2, NULL); \
} \
static void CName ## _3(int *args, ptree *pt) \
{ \
  CLM_RESULT = CName(CLM_ARG_1, CLM_ARG_2, CLM_ARG_3); \
} \
static xen_value * CName ## _1(ptree *prog, xen_value **args, int num_args) \
{ \
  if (num_args == 2) return(package(prog, R_CLM, CName ## _2, descr_ ## CName ## _2, args, 2)); \
  return(package(prog, R_CLM, CName ## _3, descr_ ## CName ## _3, args, 3)); \
}

FRAME_OP(mus_frame_add, frame+)
FRAME_OP(mus_frame_multiply, frame*)
FRAME_OP(mus_frame_to_frame, frame->frame)
FRAME_OP(mus_mixer_multiply, mixer*)
FRAME_OP(mus_mixer_add, mixer+)


/* ---------------- mixer-scale ---------------- */
static char *descr_mixer_scale_2(int *args, ptree *pt) 
{
  return(mus_format( CLM_PT " = mixer-scale(" CLM_PT ", " FLT_PT ")", args[0], DESC_CLM_RESULT, args[1], DESC_CLM_ARG_1, args[2], FLOAT_ARG_2));
}
static void mixer_scale_2(int *args, ptree *pt) {CLM_RESULT = mus_mixer_scale(CLM_ARG_1, FLOAT_ARG_2, NULL);}
static char *descr_mixer_scale_3(int *args, ptree *pt) 
{
  return(mus_format( CLM_PT " = mixer-scale(" CLM_PT ", " FLT_PT ", " CLM_PT ")", 
		     args[0], DESC_CLM_RESULT, args[1], DESC_CLM_ARG_1, args[2], FLOAT_ARG_2, args[3], DESC_CLM_ARG_3));
}
static void mixer_scale_3(int *args, ptree *pt) {CLM_RESULT = mus_mixer_scale(CLM_ARG_1, FLOAT_ARG_2, CLM_ARG_3);}
static xen_value *mixer_scale_1(ptree *prog, xen_value **args, int num_args) 
{
  if (run_safety == RUN_SAFE) safe_package(prog, R_BOOL, mixer_check, descr_mixer_check, args, 1);
  if (num_args == 2)
    return(package(prog, R_CLM, mixer_scale_2, descr_mixer_scale_2, args, 2));
  return(package(prog, R_CLM, mixer_scale_3, descr_mixer_scale_3, args, 3));
}

/* ---------------- frame->sample ---------------- */
static char *descr_frame_to_sample_2(int *args, ptree *pt) 
{
  return(mus_format( FLT_PT " = frame->sample(" CLM_PT ", " CLM_PT ")", args[0], FLOAT_RESULT, args[1], DESC_CLM_ARG_1, args[2], DESC_CLM_ARG_2));
}
static void frame_to_sample_2(int *args, ptree *pt) 
{
  FLOAT_RESULT = mus_frame_to_sample(CLM_ARG_1, CLM_ARG_2);
}
static xen_value *frame_to_sample_1(ptree *prog, xen_value **args, int num_args) 
{
  return(package(prog, R_FLOAT, frame_to_sample_2, descr_frame_to_sample_2, args, 2));
}

/* ---------------- sample->frame ---------------- */
static char *descr_sample_to_frame_2(int *args, ptree *pt) 
{
  return(mus_format( CLM_PT " = sample->frame(" CLM_PT ", " FLT_PT ")", 
		     args[0], DESC_CLM_RESULT, args[1], DESC_CLM_ARG_1, args[2], FLOAT_ARG_2));
}
static void sample_to_frame_2(int *args, ptree *pt) {CLM_RESULT = mus_sample_to_frame(CLM_ARG_1, FLOAT_ARG_2, NULL);}
static char *descr_sample_to_frame_3(int *args, ptree *pt) 
{
  return(mus_format( CLM_PT " = sample->frame(" CLM_PT ", " FLT_PT ", " CLM_PT ")", 
		     args[0], DESC_CLM_RESULT, args[1], DESC_CLM_ARG_1, args[2], FLOAT_ARG_2, args[3], DESC_CLM_ARG_3));
}
static void sample_to_frame_3(int *args, ptree *pt) 
{
  CLM_RESULT = mus_sample_to_frame(CLM_ARG_1, FLOAT_ARG_2, CLM_ARG_3);
}
static xen_value *sample_to_frame_1(ptree *prog, xen_value **args, int num_args) 
{
  if (num_args == 2) return(package(prog, R_CLM, sample_to_frame_2, descr_sample_to_frame_2, args, 2));
  return(package(prog, R_CLM, sample_to_frame_3, descr_sample_to_frame_3, args, 3));
}

/* ---------------- frame->buffer ---------------- */
static char *descr_frame_to_buffer_2(int *args, ptree *pt) 
{
  return(mus_format( CLM_PT " = frame->buffer(" CLM_PT ", " CLM_PT ")", 
		    args[0], DESC_CLM_RESULT, args[1], DESC_CLM_ARG_1, args[2], DESC_CLM_ARG_2));
}
static void frame_to_buffer_2(int *args, ptree *pt) 
{
  CLM_RESULT = mus_frame_to_buffer(CLM_ARG_1, CLM_ARG_2);
}
static xen_value *frame_to_buffer_1(ptree *prog, xen_value **args, int num_args) 
{
  if (run_safety == RUN_SAFE) package(prog, R_BOOL, buffer_check, descr_buffer_check, args, 1);
  return(package(prog, R_CLM, frame_to_buffer_2, descr_frame_to_buffer_2, args, 2));
}


/* ---------------- buffer->frame ---------------- */
static char *descr_buffer_to_frame_1b(int *args, ptree *pt) 
{
  return(mus_format( CLM_PT " = buffer->frame(" CLM_PT ")", args[0], DESC_CLM_RESULT, args[1], DESC_CLM_ARG_1));
}
static void buffer_to_frame_1b(int *args, ptree *pt) {CLM_RESULT = mus_buffer_to_frame(CLM_ARG_1, mus_make_frame(1));}
static char *descr_buffer_to_frame_2b(int *args, ptree *pt) 
{
  return(mus_format( CLM_PT " = buffer->frame(" CLM_PT ", " CLM_PT ")", 
		    args[0], DESC_CLM_RESULT, args[1], DESC_CLM_ARG_1, args[2], DESC_CLM_ARG_2));
}
static void buffer_to_frame_2b(int *args, ptree *pt) {CLM_RESULT = mus_buffer_to_frame(CLM_ARG_1, CLM_ARG_2);}
static xen_value *buffer_to_frame_1(ptree *prog, xen_value **args, int num_args) 
{
  if (run_safety == RUN_SAFE) package(prog, R_BOOL, buffer_check, descr_buffer_check, args, 1);
  if (num_args == 1) return(package(prog, R_CLM, buffer_to_frame_1b, descr_buffer_to_frame_1b, args, 1));
  return(package(prog, R_CLM, buffer_to_frame_2b, descr_buffer_to_frame_2b, args, 2));
}


/* ---------------- frame->file ---------------- */
static char *descr_frame_to_file_3(int *args, ptree *pt) 
{
  return(mus_format( CLM_PT " = frame->file((" CLM_PT ", " INT_PT ", " CLM_PT ")", 
		    args[0], DESC_CLM_RESULT, args[1], DESC_CLM_ARG_1, args[2], INT_ARG_2, args[3], DESC_CLM_ARG_3));
}
static void frame_to_file_3(int *args, ptree *pt) 
{
  CLM_RESULT = mus_frame_to_file(CLM_ARG_1, INT_ARG_2, CLM_ARG_3);
}
static xen_value *frame_to_file_1(ptree *prog, xen_value **args, int num_args) 
{
  if (run_safety == RUN_SAFE) package(prog, R_BOOL, frame_to_file_check, descr_frame_to_file_check, args, 1);
  return(package(prog, R_CLM, frame_to_file_3, descr_frame_to_file_3, args, 3));
}

/* ---------------- file->frame ---------------- */
static char *descr_file_to_frame_3(int *args, ptree *pt) 
{
  return(mus_format( CLM_PT " = file->frame((" CLM_PT ", " INT_PT ", " CLM_PT ")", 
		    args[0], DESC_CLM_RESULT, args[1], DESC_CLM_ARG_1, args[2], INT_ARG_2, args[3], DESC_CLM_ARG_3));
}
static void file_to_frame_3(int *args, ptree *pt) 
{
  CLM_RESULT = mus_file_to_frame(CLM_ARG_1, INT_ARG_2, CLM_ARG_3);
}

static char *descr_file_to_frame_2(int *args, ptree *pt) 
{
  return(mus_format( CLM_PT " = file->frame((" CLM_PT ", " INT_PT ")", 
		     args[0], DESC_CLM_RESULT, args[1], DESC_CLM_ARG_1, args[2], INT_ARG_2));
}
static void file_to_frame_2(int *args, ptree *pt) 
{
  if (CLM_RESULT) mus_free(CLM_RESULT);
  CLM_RESULT = mus_file_to_frame(CLM_ARG_1, INT_ARG_2, NULL);
}
static xen_value *file_to_frame_1(ptree *prog, xen_value **args, int num_args) 
{
  if (run_safety == RUN_SAFE) package(prog, R_BOOL, file_to_frame_check, descr_file_to_frame_check, args, 1);
  if (num_args == 2)
    return(package(prog, R_CLM, file_to_frame_2, descr_file_to_frame_2, args, 2));
  return(package(prog, R_CLM, file_to_frame_3, descr_file_to_frame_3, args, 3));
}


/* ---------------- out-any ---------------- */
/* if *output* is null these become no-ops (there is no error message as there would be in the standard case) */


static char *descr_outa_3(int *args, ptree *pt)
{
  return(mus_format( FLT_PT " = outa(" INT_PT ", " FLT_PT ", " CLM_PT ")",
		    args[0], FLOAT_RESULT, args[1], INT_ARG_1, args[2], FLOAT_ARG_2, args[3], DESC_CLM_ARG_3));
}
static void outa_3(int *args, ptree *pt) {FLOAT_RESULT = mus_out_any(INT_ARG_1, FLOAT_ARG_2, 0, CLM_ARG_3);}
static xen_value *outa_1(ptree *prog, xen_value **args, int num_args) {return(package(prog, R_FLOAT, outa_3, descr_outa_3, args, 3));}

static char *descr_outb_3(int *args, ptree *pt)
{
  return(mus_format( FLT_PT " = outb(" INT_PT ", " FLT_PT ", " CLM_PT ")",
		    args[0], FLOAT_RESULT, args[1], INT_ARG_1, args[2], FLOAT_ARG_2, args[3], DESC_CLM_ARG_3));
}
static void outb_3(int *args, ptree *pt) {FLOAT_RESULT = mus_out_any(INT_ARG_1, FLOAT_ARG_2, 1, CLM_ARG_3);}
static xen_value *outb_1(ptree *prog, xen_value **args, int num_args) {return(package(prog, R_FLOAT, outb_3, descr_outb_3, args, 3));}

static char *descr_outc_3(int *args, ptree *pt)
{
  return(mus_format( FLT_PT " = outc(" INT_PT ", " FLT_PT ", " CLM_PT ")",
		    args[0], FLOAT_RESULT, args[1], INT_ARG_1, args[2], FLOAT_ARG_2, args[3], DESC_CLM_ARG_3));
}
static void outc_3(int *args, ptree *pt) {FLOAT_RESULT = mus_out_any(INT_ARG_1, FLOAT_ARG_2, 2, CLM_ARG_3);}
static xen_value *outc_1(ptree *prog, xen_value **args, int num_args) {return(package(prog, R_FLOAT, outc_3, descr_outc_3, args, 3));}

static char *descr_outd_3(int *args, ptree *pt)
{
  return(mus_format( FLT_PT " = outd(" INT_PT ", " FLT_PT ", " CLM_PT ")",
		    args[0], FLOAT_RESULT, args[1], INT_ARG_1, args[2], FLOAT_ARG_2, args[3], DESC_CLM_ARG_3));
}
static void outd_3(int *args, ptree *pt) {FLOAT_RESULT = mus_out_any(INT_ARG_1, FLOAT_ARG_2, 3, CLM_ARG_3);}
static xen_value *outd_1(ptree *prog, xen_value **args, int num_args) {return(package(prog, R_FLOAT, outd_3, descr_outd_3, args, 3));}

static char *descr_out_any_4(int *args, ptree *pt)
{
  return(mus_format( FLT_PT " = out-any(" INT_PT ", " FLT_PT ", " INT_PT ", " CLM_PT ")",
		    args[0], FLOAT_RESULT, args[1], INT_ARG_1, args[2], FLOAT_ARG_2, args[3], INT_ARG_3, args[4], DESC_CLM_ARG_4));
}
static void out_any_4(int *args, ptree *pt) {FLOAT_RESULT = mus_out_any(INT_ARG_1, FLOAT_ARG_2, INT_ARG_3, CLM_ARG_4);} 
static xen_value *out_any_1(ptree *prog, xen_value **args, int num_args)
{
  return(package(prog, R_FLOAT, out_any_4, descr_out_any_4, args, 4));
}


/* ---------------- in-any ---------------- */

static char *descr_ina_2(int *args, ptree *pt)
{
  return(mus_format( FLT_PT " = ina(" INT_PT ", " CLM_PT ")", args[0], FLOAT_RESULT, args[1], INT_ARG_1, args[2], DESC_CLM_ARG_2));
}
static void ina_2(int *args, ptree *pt) {FLOAT_RESULT = mus_in_any(INT_ARG_1, 0, CLM_ARG_2);}
static xen_value *ina_1(ptree *prog, xen_value **args, int num_args) {return(package(prog, R_FLOAT, ina_2, descr_ina_2, args, 2));}

static char *descr_inb_2(int *args, ptree *pt)
{
  return(mus_format( FLT_PT " = inb(" INT_PT ", " CLM_PT ")", args[0], FLOAT_RESULT, args[1], INT_ARG_1, args[2], DESC_CLM_ARG_2));
}
static void inb_2(int *args, ptree *pt) {FLOAT_RESULT = mus_in_any(INT_ARG_1, 1, CLM_ARG_2);}
static xen_value *inb_1(ptree *prog, xen_value **args, int num_args) {return(package(prog, R_FLOAT, inb_2, descr_inb_2, args, 2));}

static char *descr_in_any_3(int *args, ptree *pt)
{
  return(mus_format( FLT_PT " = in-any(" INT_PT ", " INT_PT ", " CLM_PT ")",
		    args[0], FLOAT_RESULT, args[1], INT_ARG_1, args[2], INT_ARG_2, args[3], DESC_CLM_ARG_3));
}
static void in_any_3(int *args, ptree *pt) {FLOAT_RESULT = mus_in_any(INT_ARG_1, INT_ARG_2, CLM_ARG_3);}
static xen_value *in_any_1(ptree *prog, xen_value **args, int num_args)
{
  return(package(prog, R_FLOAT, in_any_3, descr_in_any_3, args, 3));
}




/* ---------------- array-interp ---------------- */
static char *descr_array_interp_1f(int *args, ptree *pt) 
{
  return(mus_format( FLT_PT " = array-interp(" VCT_PT ", " FLT_PT ")", args[0], FLOAT_RESULT, args[1], DESC_VCT_ARG_1, args[2], FLOAT_ARG_2));
}
static void array_interp_1f(int *args, ptree *pt) 
{
  FLOAT_RESULT = mus_array_interp(VCT_ARG_1->data, FLOAT_ARG_2, VCT_ARG_1->length);
}
static char *descr_array_interp_2f(int *args, ptree *pt) 
{
  return(mus_format( FLT_PT " = array-interp(" VCT_PT ", " FLT_PT ", " INT_PT ")", 
		     args[0], FLOAT_RESULT, args[1], DESC_VCT_ARG_1, args[2], FLOAT_ARG_2, args[3], INT_ARG_3));
}
static void array_interp_2f(int *args, ptree *pt) {FLOAT_RESULT = mus_array_interp(VCT_ARG_1->data, FLOAT_ARG_2, INT_ARG_3);}
static xen_value *array_interp_1(ptree *prog, xen_value **args, int num_args) 
{
  if (num_args == 2) return(package(prog, R_FLOAT, array_interp_1f, descr_array_interp_1f, args, 2));
  return(package(prog, R_FLOAT, array_interp_2f, descr_array_interp_2f, args, 3));
}

/* ---------------- vct-peak ---------------- */
static char *descr_vct_peak_v(int *args, ptree *pt) 
{
  return(mus_format( FLT_PT " = " S_vct_peak "(" VCT_PT ")", args[0], FLOAT_RESULT, args[1], DESC_VCT_ARG_1));
}
static void vct_peak_v(int *args, ptree *pt) 
{
  int i;
  Double val = 0.0, absv;
  vct *v;
  v = VCT_ARG_1;
  val = fabs(v->data[0]); 
  for (i = 1; i < v->length; i++) 
    {
      absv = fabs(v->data[i]); 
      if (absv > val) val = absv;
    }
  FLOAT_RESULT = val;
}
static xen_value *vct_peak_1(ptree *prog, xen_value **args, int num_args) 
{
  return(package(prog, R_FLOAT, vct_peak_v, descr_vct_peak_v, args, 1));
}


/* ---------------- clear_array ---------------- */
static char *descr_clear_array_1f(int *args, ptree *pt) {return(mus_format("clear_array(" VCT_PT ")", args[1], DESC_VCT_ARG_1));}
static void clear_array_1f(int *args, ptree *pt) 
{
  mus_clear_array(VCT_ARG_1->data, VCT_ARG_1->length);
  VCT_RESULT = VCT_ARG_1;
}
static xen_value *clear_array_1(ptree *prog, xen_value **args, int num_args) 
{
  if (run_safety == RUN_SAFE) package(prog, R_BOOL, vct_check_1, descr_vct_check_1, args, 1);
  return(package(prog, R_VCT, clear_array_1f, descr_clear_array_1f, args, 1));
}


/* ---------------- restart_env ---------------- */
static char *descr_restart_env_1f(int *args, ptree *pt) {return(mus_format("restart-env(" CLM_PT ")", args[1], DESC_CLM_ARG_1));}
static void restart_env_1f(int *args, ptree *pt) {mus_restart_env(CLM_ARG_1); CLM_RESULT = CLM_ARG_1;}
static xen_value *restart_env_1(ptree *prog, xen_value **args, int num_args) 
{
  if (run_safety == RUN_SAFE) package(prog, R_BOOL, env_check, descr_env_check, args, 1);
  return(package(prog, R_CLM, restart_env_1f, descr_restart_env_1f, args, 1));
}


/* ---------------- dot-product etc ---------------- */

#define VCT_2_I(CName, SName) \
static char *descr_ ## CName ## _2f(int *args, ptree *pt)  \
{ \
  return(mus_format(#SName "(" VCT_PT ", " VCT_PT ")", args[1], DESC_VCT_ARG_1, args[2], DESC_VCT_ARG_2)); \
} \
static void CName ## _2f(int *args, ptree *pt)  \
{ \
  mus_ ## CName(VCT_ARG_1->data, VCT_ARG_2->data, VCT_ARG_1->length); \
  VCT_RESULT = VCT_ARG_1; \
} \
static char *descr_ ## CName ## _3f(int *args, ptree *pt)  \
{ \
  return(mus_format(#SName "(" VCT_PT ", " VCT_PT ", " INT_PT ")",  \
                    args[1], DESC_VCT_ARG_1, args[2], DESC_VCT_ARG_2, args[3], INT_ARG_3)); \
} \
static void CName ## _3f(int *args, ptree *pt)  \
{ \
  mus_ ## CName(VCT_ARG_1->data, VCT_ARG_2->data, INT_ARG_3); \
  VCT_RESULT = VCT_ARG_1; \
} \
static xen_value * CName ## _1(ptree *prog, xen_value **args, int num_args)  \
{ \
  if (run_safety == RUN_SAFE) package(prog, R_BOOL, vct_check_1, descr_vct_check_1, args, 1); \
  if (run_safety == RUN_SAFE) package(prog, R_BOOL, vct_check_2, descr_vct_check_2, args, 2); \
  if (num_args == 2) \
    return(package(prog, R_VCT, CName ## _2f, descr_ ## CName ## _2f, args, 2)); \
  else return(package(prog, R_VCT, CName ## _3f, descr_ ## CName ## _3f, args, 3)); \
}

VCT_2_I(rectangular_to_polar, rectangular->polar)
VCT_2_I(polar_to_rectangular, polar->rectangular)
VCT_2_I(multiply_arrays, multiply-arrays)
VCT_2_I(convolution, convolution)


#define VCT_2_F(CName, SName) \
static char *descr_ ## CName ## _2f(int *args, ptree *pt)  \
{ \
  return(mus_format( FLT_PT " = " #SName "(" VCT_PT ", " VCT_PT ")", args[0], FLOAT_RESULT, args[1], DESC_VCT_ARG_1, args[2], DESC_VCT_ARG_2)); \
} \
static void CName ## _2f(int *args, ptree *pt)  \
{ \
  FLOAT_RESULT = mus_ ## CName(VCT_ARG_1->data, VCT_ARG_2->data, VCT_ARG_1->length); \
} \
static char *descr_ ## CName ## _3f(int *args, ptree *pt)  \
{ \
  return(mus_format( FLT_PT " = " #SName "(" VCT_PT ", " VCT_PT ", " INT_PT ")",  \
                    args[0], FLOAT_RESULT, args[1], DESC_VCT_ARG_1, args[2], DESC_VCT_ARG_2, args[3], INT_ARG_3)); \
} \
static void CName ## _3f(int *args, ptree *pt)  \
{ \
  FLOAT_RESULT = mus_ ## CName(VCT_ARG_1->data, VCT_ARG_2->data, INT_ARG_3); \
} \
static xen_value * CName ## _1(ptree *prog, xen_value **args, int num_args)  \
{ \
  if (run_safety == RUN_SAFE) package(prog, R_BOOL, vct_check_1, descr_vct_check_1, args, 1); \
  if (run_safety == RUN_SAFE) package(prog, R_BOOL, vct_check_2, descr_vct_check_2, args, 2); \
  if (num_args == 2) \
    return(package(prog, R_FLOAT, CName ## _2f, descr_ ## CName ## _2f, args, 2)); \
  else return(package(prog, R_FLOAT, CName ## _3f, descr_ ## CName ## _3f, args, 3)); \
}

VCT_2_F(sine_bank, sine-bank)
VCT_2_F(dot_product, dot-product)

static void clm_0f(int *args, ptree *pt) {if (CLM_ARG_1) FLOAT_RESULT = MUS_RUN(CLM_ARG_1, 0.0, 0.0);}
static char *descr_clm_0f(int *args, ptree *pt) {return(descr_gen(args, pt, mus_name(CLM_ARG_1), 0));} 
static void clm_1f(int *args, ptree *pt) {if (CLM_ARG_1) FLOAT_RESULT = MUS_RUN(CLM_ARG_1, FLOAT_ARG_2, 0.0);}
static char *descr_clm_1f(int *args, ptree *pt) {return(descr_gen(args, pt, mus_name(CLM_ARG_1), 1));} 
static void clm_2f(int *args, ptree *pt) {if (CLM_ARG_1) FLOAT_RESULT = MUS_RUN(CLM_ARG_1, FLOAT_ARG_2, FLOAT_ARG_3);}
static char *descr_clm_2f(int *args, ptree *pt) {return(descr_gen(args, pt, mus_name(CLM_ARG_1), 2));} 
static xen_value *clm_n(ptree *prog, xen_value **args, int num_args, xen_value *sf)
{
  xen_value *temp;
  if (args[0]) FREE(args[0]);
  args[0] = make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, 0.0), R_VARIABLE);
  if ((num_args > 0) && (args[1]->type == R_INT))
    {
      temp = args[1];
      args[1] = convert_int_to_dbl(prog, args[1]);
      FREE(temp);
    }
  if ((num_args > 1) && (args[2]->type == R_INT))
    {
      temp = args[2];
      args[2] = convert_int_to_dbl(prog, args[2]);
      FREE(temp);
    }
  switch (num_args)
    {
    case 0: add_triple_to_ptree(prog, va_make_triple(clm_0f, descr_clm_0f, 2, args[0], sf)); break;
    case 1: add_triple_to_ptree(prog, va_make_triple(clm_1f, descr_clm_1f, 3, args[0], sf, args[1])); break;
    case 2: add_triple_to_ptree(prog, va_make_triple(clm_2f, descr_clm_2f, 4, args[0], sf, args[1], args[2])); break;
    default: return(NULL); break;
    }
  return(args[0]);
}

/* ---------------- spectrum ---------------- */
static char *descr_mus_spectrum_2v(int *args, ptree *pt) 
{
  return(mus_format("spectrum(" VCT_PT ", " VCT_PT ")", args[1], DESC_VCT_ARG_1, args[2], DESC_VCT_ARG_2));
}
static void mus_spectrum_2v(int *args, ptree *pt) 
{
  mus_spectrum(VCT_ARG_1->data, VCT_ARG_2->data, NULL, VCT_ARG_1->length, 1);
  VCT_RESULT = VCT_ARG_1;
}
static char *descr_mus_spectrum_3v(int *args, ptree *pt) 
{
  if (VCT_ARG_3)
    return(mus_format("spectrum(" VCT_PT ", " VCT_PT ", " VCT_PT ")", args[1], DESC_VCT_ARG_1, args[2], DESC_VCT_ARG_2, args[3], DESC_VCT_ARG_3));
  return(mus_format("spectrum(" VCT_PT ", " VCT_PT ", #f)", args[1], DESC_VCT_ARG_1, args[2], DESC_VCT_ARG_2));
}
static void mus_spectrum_3v(int *args, ptree *pt) 
{
  mus_spectrum(VCT_ARG_1->data, VCT_ARG_2->data, (VCT_ARG_3) ? (VCT_ARG_3->data) : NULL, VCT_ARG_1->length, 1);
  VCT_RESULT = VCT_ARG_1;
}
static char *descr_mus_spectrum_4v(int *args, ptree *pt) 
{
  if (VCT_ARG_3)
    return(mus_format("spectrum(" VCT_PT ", " VCT_PT ", " VCT_PT ", " INT_PT ")", 
		      args[1], DESC_VCT_ARG_1, args[2], DESC_VCT_ARG_2, args[3], DESC_VCT_ARG_3, args[4], INT_ARG_4));
  return(mus_format("spectrum(" VCT_PT ", " VCT_PT ", #f, " INT_PT ")", 
		    args[1], DESC_VCT_ARG_1, args[2], DESC_VCT_ARG_2, args[4], INT_ARG_4));
}
static void mus_spectrum_4v(int *args, ptree *pt) 
{
  mus_spectrum(VCT_ARG_1->data, 
	       VCT_ARG_2->data, 
	       (VCT_ARG_3) ? (VCT_ARG_3->data) : NULL,
	       VCT_ARG_1->length,
	       INT_ARG_4);
  VCT_RESULT = VCT_ARG_1;
}
static xen_value *mus_spectrum_1(ptree *prog, xen_value **args, int num_args) 
{
  if (run_safety == RUN_SAFE) package(prog, R_BOOL, vct_check_1, descr_vct_check_1, args, 1);
  if (run_safety == RUN_SAFE) package(prog, R_BOOL, vct_check_2, descr_vct_check_2, args, 2);
  if (num_args == 2) return(package(prog, R_VCT, mus_spectrum_2v, descr_mus_spectrum_2v, args, 2));
  if (num_args == 3) return(package(prog, R_VCT, mus_spectrum_3v, descr_mus_spectrum_3v, args, 3));
  return(package(prog, R_VCT, mus_spectrum_4v, descr_mus_spectrum_4v, args, 4));
}


/* ---------------- src ---------------- */

GEN_P(src)
GEN_P(convolve)
GEN_P(granulate)
GEN_P(phase_vocoder)

static Float src_input(void *arg, int direction)
{
  mus_xen *gn = (mus_xen *)arg;
  ptree *pt, *outer;
  pt = (ptree *)(gn->input_ptree);
  outer = (ptree *)(pt->outer_tree);
  outer->ints[pt->args[0]] = direction;
  eval_embedded_ptree(pt, outer);
  return(outer->dbls[pt->result->addr]);
}
static char *descr_src_2f(int *args, ptree *pt) 
{
  return(mus_format( FLT_PT " = src(" CLM_PT ", " FLT_PT ", " FNC_PT ")",
		    args[0], FLOAT_RESULT, args[1], DESC_CLM_ARG_1, args[2], FLOAT_ARG_2, args[3], DESC_FNC_ARG_3));
}
static void src_2f(int *args, ptree *pt) 
{
  ((mus_xen *)mus_environ(CLM_ARG_1))->input_ptree = (void *)(FNC_ARG_3);
  FLOAT_RESULT = mus_src(CLM_ARG_1, FLOAT_ARG_2, src_input);
}
static char *descr_src_1f(int *args, ptree *pt) 
{
  return(mus_format( FLT_PT " = src(" CLM_PT ", " FLT_PT ")", args[0], FLOAT_RESULT, args[1], DESC_CLM_ARG_1, args[2], FLOAT_ARG_2));
}
static void src_1f(int *args, ptree *pt) {FLOAT_RESULT = mus_src(CLM_ARG_1, FLOAT_ARG_2, NULL);}
static char *descr_src_0f(int *args, ptree *pt) 
{
  return(mus_format( FLT_PT " = src(" CLM_PT ")", args[0], FLOAT_RESULT, args[1], DESC_CLM_ARG_1));
}
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
  if (run_safety == RUN_SAFE) package(prog, R_BOOL, src_check, descr_src_check, args, 1);
  switch (num_args)
    {
    case 1: return(package(prog, R_FLOAT, src_0f, descr_src_0f, args, 1)); break;
    case 2: return(package(prog, R_FLOAT, src_1f, descr_src_1f, args, 2)); break;
    default: return(package(prog, R_FLOAT, src_2f, descr_src_2f, args, 3)); break;
    }
}


/* ---------------- convolve ---------------- */

static char *descr_convolve_1f(int *args, ptree *pt)
{
  return(mus_format( FLT_PT " = convolve(" CLM_PT ", " FNC_PT ")", args[0], FLOAT_RESULT, args[1], DESC_CLM_ARG_1, args[2], DESC_FNC_ARG_2));
}
static void convolve_1f(int *args, ptree *pt)
{
  ((mus_xen *)mus_environ(CLM_ARG_1))->input_ptree = (void *)(FNC_ARG_2);
  FLOAT_RESULT = mus_convolve(CLM_ARG_1, src_input);
}
static char *descr_convolve_0f(int *args, ptree *pt)
{
  return(mus_format( FLT_PT " = convolve(" CLM_PT ")", args[0], FLOAT_RESULT, args[1], DESC_CLM_ARG_1));
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
  if (run_safety == RUN_SAFE) package(prog, R_BOOL, convolve_check, descr_convolve_check, args, 1);
  if (num_args == 1)
    return(package(prog, R_FLOAT, convolve_0f, descr_convolve_0f, args, 1));
  return(package(prog, R_FLOAT, convolve_1f, descr_convolve_1f, args, 2));
}


/* ---------------- granulate ---------------- */

static int grn_edit(void *arg)
{
  mus_xen *gn = (mus_xen *)arg;
  ptree *pt, *outer;
  pt = (ptree *)(gn->edit_ptree);
  outer = (ptree *)(pt->outer_tree);
#if DEBUGGING
  if (outer->clms_size <= pt->args[0])
    {
      fprintf(stderr,"outer: %p[%d] (%d)\n", outer->clms, pt->args[0], outer->clms_size);
      abort();
    }
#endif
  outer->clms[pt->args[0]] = gn->gen;
  eval_embedded_ptree(pt, outer);
  return(outer->ints[pt->result->addr]);
}

static char *descr_granulate_2f(int *args, ptree *pt)
{
  return(mus_format( FLT_PT " = granulate(" CLM_PT ", " FNC_PT ", " FNC_PT ")",
          args[0], FLOAT_RESULT, args[1], DESC_CLM_ARG_1, args[2], DESC_FNC_ARG_2, args[3], DESC_FNC_ARG_3));
}
static void granulate_2f(int *args, ptree *pt)
{
  mus_xen *gn = (mus_xen *)mus_environ(CLM_ARG_1);
  gn->input_ptree = (void *)(FNC_ARG_2);
  gn->edit_ptree = (void *)(FNC_ARG_3);
  FLOAT_RESULT = mus_granulate_with_editor(CLM_ARG_1, src_input, grn_edit);
}
static char *descr_granulate_2f_split(int *args, ptree *pt)
{
  return(mus_format( FLT_PT " = granulate(" CLM_PT ", null, " FNC_PT ")", args[0], FLOAT_RESULT, args[1], DESC_CLM_ARG_1, args[3], DESC_FNC_ARG_3));
}
static void granulate_2f_split(int *args, ptree *pt)
{
  mus_xen *gn = (mus_xen *)mus_environ(CLM_ARG_1);
  gn->edit_ptree = (void *)(FNC_ARG_3);
  FLOAT_RESULT = mus_granulate_with_editor(CLM_ARG_1, NULL, grn_edit);
}
static char *descr_granulate_1f(int *args, ptree *pt)
{
  return(mus_format( FLT_PT " = granulate(" CLM_PT ", " FNC_PT ")", args[0], FLOAT_RESULT, args[1], DESC_CLM_ARG_1, args[2], DESC_FNC_ARG_2));
}
static void granulate_1f(int *args, ptree *pt)
{
  ((mus_xen *)mus_environ(CLM_ARG_1))->input_ptree = (void *)(FNC_ARG_2);
  FLOAT_RESULT = mus_granulate(CLM_ARG_1, src_input);
}
static char *descr_granulate_0f(int *args, ptree *pt)
{
  return(mus_format( FLT_PT " = granulate(" CLM_PT ")", args[0], FLOAT_RESULT, args[1], DESC_CLM_ARG_1));
}
static void granulate_0f(int *args, ptree *pt) {FLOAT_RESULT = mus_granulate(CLM_ARG_1, NULL);}
static xen_value *granulate_1(ptree *prog, xen_value **args, int num_args)
{
  bool got_edit = false, got_input = false;
  if (num_args >= 2)
    {
      if (num_args == 3)
	{
	  if (args[3]->type == R_FUNCTION)
	    {
	      ptree *pt;
	      pt = ((ptree **)(prog->fncs))[args[3]->addr];
	      if (pt->arity != 1) return(run_warn("granulate: wrong number of args to edit function"));
	      got_edit = true;
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
  if (run_safety == RUN_SAFE) package(prog, R_BOOL, granulate_check, descr_granulate_check, args, 1);
  if (num_args == 1)
    return(package(prog, R_FLOAT, granulate_0f, descr_granulate_0f, args, 1));
  if (num_args == 2)
    return(package(prog, R_FLOAT, granulate_1f, descr_granulate_1f, args, 2));
  if (got_input)
    return(package(prog, R_FLOAT, granulate_2f, descr_granulate_2f, args, 3));
  return(package(prog, R_FLOAT, granulate_2f_split, descr_granulate_2f_split, args, 3));
}



/* ---------------- phase-vocoder ---------------- */

static bool pv_analyze(void *arg, Float (*input)(void *arg1, int direction))
{
  mus_xen *gn = (mus_xen *)arg;
  ptree *pt, *outer;
  pt = (ptree *)(gn->analyze_ptree);
  outer = (ptree *)(pt->outer_tree);
#if DEBUGGING
  if (outer->clms_size <= pt->args[0])
    {
      fprintf(stderr,"analyze outer: %p[%d] (%d)\n", outer->clms, pt->args[0], outer->clms_size);
      abort();
    }
#endif
  outer->clms[pt->args[0]] = gn->gen;
  /* I think the input function is handled by mus_phase_vocoder */
  eval_embedded_ptree(pt, outer);
  return(outer->ints[pt->result->addr]);
}

static Float pv_synthesize(void *arg)
{
  mus_xen *gn = (mus_xen *)arg;
  ptree *pt, *outer;
  pt = (ptree *)(gn->synthesize_ptree);
  outer = (ptree *)(pt->outer_tree);
#if DEBUGGING
  if (outer->clms_size <= pt->args[0])
    {
      fprintf(stderr,"synthesize outer: %p[%d] (%d)\n", outer->clms, pt->args[0], outer->clms_size);
      abort();
    }
#endif
  outer->clms[pt->args[0]] = gn->gen;
  eval_embedded_ptree(pt, outer);
  return(outer->dbls[pt->result->addr]);
}

static char *descr_phase_vocoder_5f(int *args, ptree *pt)
{
  return(mus_format( FLT_PT " = phase-vocoder(" CLM_PT ", " FNC_PT ", " FNC_PT ", " FNC_PT ", " FNC_PT ")",
		     args[0], FLOAT_RESULT, 
		     args[2], DESC_CLM_ARG_2, 
		     (INT_ARG_1 & 1) ? args[3] : 0, (INT_ARG_1 & 1) ? DESC_FNC_ARG_3 : 0,
		     (INT_ARG_1 & 2) ? args[4] : 0, (INT_ARG_1 & 2) ? DESC_FNC_ARG_4 : 0,
		     (INT_ARG_1 & 4) ? args[5] : 0, (INT_ARG_1 & 4) ? DESC_FNC_ARG_5 : 0,
		     (INT_ARG_1 & 8) ? args[6] : 0, (INT_ARG_1 & 8) ? DESC_FNC_ARG_6 : 0));
}
static void phase_vocoder_5f(int *args, ptree *pt)
{
  /* in-coming arg1 is descr of which funcs are active -- all arg ctrs are shifted over one to make room */
  mus_xen *gn = (mus_xen *)mus_environ(CLM_ARG_2);
  /*
  fprintf(stderr,"funcs: %lld\n", INT_ARG_1);
  */
  if (INT_ARG_1 & 1) gn->input_ptree = (void *)(FNC_ARG_3);
  if (INT_ARG_1 & 2) gn->analyze_ptree = (void *)(FNC_ARG_4);
  if (INT_ARG_1 & 4) gn->edit_ptree = (void *)(FNC_ARG_5);
  if (INT_ARG_1 & 8) gn->synthesize_ptree = (void *)(FNC_ARG_6);
  FLOAT_RESULT = mus_phase_vocoder_with_editors(CLM_ARG_2, 
						(INT_ARG_1 & 1) ? src_input : NULL, 
						(INT_ARG_1 & 2) ? pv_analyze : NULL, 
						(INT_ARG_1 & 4) ? grn_edit : NULL, 
						(INT_ARG_1 & 8) ? pv_synthesize : NULL);
}
static char *descr_phase_vocoder_1f(int *args, ptree *pt)
{
  return(mus_format( FLT_PT " = phase-vocoder(" CLM_PT ", " FNC_PT ")", args[0], FLOAT_RESULT, args[1], DESC_CLM_ARG_1, args[2], DESC_FNC_ARG_2));
}
static void phase_vocoder_1f(int *args, ptree *pt)
{
  ((mus_xen *)mus_environ(CLM_ARG_1))->input_ptree = (void *)(FNC_ARG_2);
  FLOAT_RESULT = mus_phase_vocoder(CLM_ARG_1, src_input);
}
static char *descr_phase_vocoder_0f(int *args, ptree *pt)
{
  return(mus_format( FLT_PT " = phase-vocoder(" CLM_PT ")", args[0], FLOAT_RESULT, args[1], DESC_CLM_ARG_1));
}
static void phase_vocoder_0f(int *args, ptree *pt) {FLOAT_RESULT = mus_phase_vocoder(CLM_ARG_1, NULL);}
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
  if (run_safety == RUN_SAFE) package(prog, R_BOOL, phase_vocoder_check, descr_phase_vocoder_check, args, 1);
  if (num_args == 1)
    return(package(prog, R_FLOAT, phase_vocoder_0f, descr_phase_vocoder_0f, args, 1));
  if (num_args == 2)
    return(package(prog, R_FLOAT, phase_vocoder_1f, descr_phase_vocoder_1f, args, 2));
  /* rather than make 16 different callers, I'll pass along which args are functions in an inserted arg1 */
  {
    int i;
    xen_value **new_args;
    new_args = (xen_value **)CALLOC(num_args + 2, sizeof(xen_value *));
    for (i = 1; i <= num_args; i++)
      new_args[i + 1] = args[i];
    new_args[1] = make_xen_value(R_INT, 
				 add_int_to_ptree(prog, 
						  ((got_input) ? 1 : 0) +
						  ((got_analyze) ? 2: 0) + 
						  ((got_edit) ? 4 : 0) + 
						  ((got_synthesize) ? 8 : 0)), 
				 R_CONSTANT);
    new_args[0] = add_empty_var_to_ptree(prog, R_FLOAT);
    args[0] = new_args[0];
    add_triple_to_ptree(prog, make_triple(phase_vocoder_5f, descr_phase_vocoder_5f, new_args, num_args + 2));
    FREE(new_args[1]);
    FREE(new_args);
    return(args[0]);
  }
}

#define PV_VCT_1(Name) \
static char *descr_pv_ ## Name ## _1(int *args, ptree *pt) \
{ \
  return(mus_format( VCT_PT " = phase-vocoder-" #Name "(" LST_PT ")", args[0], DESC_VCT_RESULT, args[1], DESC_CLM_ARG_1)); \
} \
static void pv_ ## Name ## _1(int *args, ptree *pt) \
{ \
  if (!VCT_RESULT) \
    { \
      VCT_RESULT = (vct *)malloc(sizeof(vct)); \
      VCT_RESULT->length = mus_length(CLM_ARG_1); \
      VCT_RESULT->data = mus_phase_vocoder_ ## Name (CLM_ARG_1); \
      VCT_RESULT->dont_free = true; \
    } \
} \
static xen_value *phase_vocoder_ ## Name ## _1(ptree *prog, xen_value **args, int num_args) \
{ \
  if (run_safety == RUN_SAFE) package(prog, R_BOOL, phase_vocoder_check, descr_phase_vocoder_check, args, 1); \
  if (args[1]->type == R_CLM) \
    return(package(prog, R_VCT, pv_ ## Name ## _1, descr_pv_ ## Name ## _1, args, 1)); \
  return(run_warn("wrong type arg (%s) to mus_phase_vocoder_ " #Name , type_name(args[1]->type))); \
}

PV_VCT_1(amps)
PV_VCT_1(amp_increments)
PV_VCT_1(freqs)
PV_VCT_1(phases)
PV_VCT_1(phase_increments)

INT_GEN0(phase_vocoder_outctr)

static char *descr_pv_set_outctr(int *args, ptree *pt)
{
  return(mus_format("phase-vocoder-set-outctr(" CLM_PT ", " INT_PT ")" , args[0], DESC_CLM_RESULT, args[1], INT_ARG_1));
}
static void pv_set_outctr(int *args, ptree *pt) {mus_phase_vocoder_set_outctr(CLM_RESULT, INT_ARG_1);}
static xen_value *mus_phase_vocoder_set_outctr_1(ptree *prog, xen_value **args, int num_args)
{
  if (run_safety == RUN_SAFE) package(prog, R_BOOL, phase_vocoder_check, descr_phase_vocoder_check, args, 1);
  return(package(prog, R_INT, pv_set_outctr, descr_pv_set_outctr, args, 2));
}


/* ---------------- contrast_enhancement ---------------- */

static void contrast_enhancement_f(int *args, ptree *pt) {FLOAT_RESULT = mus_contrast_enhancement(FLOAT_ARG_1, FLOAT_ARG_2);}
static char *descr_contrast_enhancement_f(int *args, ptree *pt) 
{
  return(mus_format( FLT_PT " = contrast-enhancement(" FLT_PT ", " FLT_PT ")", args[0], FLOAT_RESULT, args[1], FLOAT_ARG_1, args[2], FLOAT_ARG_2));
}
static xen_value *contrast_enhancement_1(ptree *prog, xen_value **args, int num_args)
{
  if (args[1]->type == R_INT) single_to_float(prog, args, 1);
  if (args[2]->type == R_INT) single_to_float(prog, args, 2);
  return(package(prog, R_FLOAT, contrast_enhancement_f, descr_contrast_enhancement_f, args, 2));
}

/* ---------------- ring_modulate ---------------- */

static void ring_modulate_f(int *args, ptree *pt) {FLOAT_RESULT = mus_ring_modulate(FLOAT_ARG_1, FLOAT_ARG_2);}
static char *descr_ring_modulate_f(int *args, ptree *pt) 
{
  return(mus_format( FLT_PT " = ring-modulate(" FLT_PT ", " FLT_PT ")", args[0], FLOAT_RESULT, args[1], FLOAT_ARG_1, args[2], FLOAT_ARG_2));
}
static xen_value *ring_modulate_1(ptree *prog, xen_value **args, int num_args)
{
  if (args[1]->type == R_INT) single_to_float(prog, args, 1);
  if (args[2]->type == R_INT) single_to_float(prog, args, 2);
  return(package(prog, R_FLOAT, ring_modulate_f, descr_ring_modulate_f, args, 2));
}

/* ---------------- amplitude_modulate ---------------- */

static void amplitude_modulate_f(int *args, ptree *pt) {FLOAT_RESULT = mus_amplitude_modulate(FLOAT_ARG_1, FLOAT_ARG_2, FLOAT_ARG_3);}
static char *descr_amplitude_modulate_f(int *args, ptree *pt) 
{
  return(mus_format( FLT_PT " = amplitude-modulate(" FLT_PT ", " FLT_PT ", " FLT_PT ")", 
		    args[0], FLOAT_RESULT, args[1], FLOAT_ARG_1, args[2], FLOAT_ARG_2, args[3], FLOAT_ARG_3));
}
static xen_value *amplitude_modulate_1(ptree *prog, xen_value **args, int num_args)
{
  if (args[1]->type == R_INT) single_to_float(prog, args, 1);
  if (args[2]->type == R_INT) single_to_float(prog, args, 2);
  if (args[3]->type == R_INT) single_to_float(prog, args, 3);
  return(package(prog, R_FLOAT, amplitude_modulate_f, descr_amplitude_modulate_f, args, 3));
}


#define SND_STR_OP(CName) \
static void CName ## _f(int *args, ptree *pt) {INT_RESULT = (Int)CName(STRING_ARG_1);} \
static char *descr_ ## CName ## _f(int *args, ptree *pt)  \
{ \
  return(mus_format( INT_PT " = " #CName "(" STR_PT ")", args[0], INT_RESULT, args[1], STRING_ARG_1)); \
} \
static xen_value * CName ## _1(ptree *prog, xen_value **args, int num_args) \
{ \
  return(package(prog, R_INT, CName ## _f, descr_ ##CName ## _f, args, 1)); \
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

static void mus_sound_duration_f(int *args, ptree *pt) {FLOAT_RESULT = mus_sound_duration(STRING_ARG_1);}
static char *descr_mus_sound_duration_f(int *args, ptree *pt)
{
  return(mus_format( FLT_PT " = sound-duration(" STR_PT ")", args[0], FLOAT_RESULT, args[1], STRING_ARG_1));
}
static xen_value *mus_sound_duration_1(ptree *prog, xen_value **args, int num_args)
{
  return(package(prog, R_FLOAT, mus_sound_duration_f, descr_mus_sound_duration_f, args, 1));
}

static void mus_sound_comment_f(int *args, ptree *pt) 
{
  if (STRING_RESULT) FREE(STRING_RESULT);
  STRING_RESULT = mus_sound_comment(STRING_ARG_1);
}
static char *descr_mus_sound_comment_f(int *args, ptree *pt)
{
  return(mus_format( STR_PT " = sound-comment(" STR_PT ")", args[0], STRING_RESULT, args[1], STRING_ARG_1));
}
static xen_value *mus_sound_comment_1(ptree *prog, xen_value **args, int num_args)
{
  return(package(prog, R_STRING, mus_sound_comment_f, descr_mus_sound_comment_f, args, 1));
}

static void mus_header_type_name_f(int *args, ptree *pt) 
{
  if (STRING_RESULT) FREE(STRING_RESULT);
  STRING_RESULT = copy_string(mus_header_type_name(INT_ARG_1));
}
static char *descr_mus_header_type_name_f(int *args, ptree *pt)
{
  return(mus_format( STR_PT " = header-type-name(" INT_PT ")", args[0], STRING_RESULT, args[1], INT_ARG_1));
}
static xen_value *mus_header_type_name_1(ptree *prog, xen_value **args, int num_args)
{
  return(package(prog, R_STRING, mus_header_type_name_f, descr_mus_header_type_name_f, args, 1));
}

static void mus_data_format_name_f(int *args, ptree *pt) 
{
  if (STRING_RESULT) FREE(STRING_RESULT);
  STRING_RESULT = copy_string(mus_data_format_name(INT_ARG_1));
}
static char *descr_mus_data_format_name_f(int *args, ptree *pt)
{
  return(mus_format( STR_PT " = data-format-name(" INT_PT ")", args[0], STRING_RESULT, args[1], INT_ARG_1));
}
static xen_value *mus_data_format_name_1(ptree *prog, xen_value **args, int num_args)
{
  return(package(prog, R_STRING, mus_data_format_name_f, descr_mus_data_format_name_f, args, 1));
}


/* ---------------- formant-bank ---------------- */
static char *descr_formant_bank_f(int *args, ptree *pt) 
{
  return(mus_format( FLT_PT " = formant-bank(" VCT_PT ", " VECT_PT ", " FLT_PT ")", 
		    args[0], FLOAT_RESULT, args[1], DESC_VCT_ARG_1, args[2], DESC_VECT_ARG_2, args[3], FLOAT_ARG_3));
}
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
  return(package(prog, R_FLOAT, formant_bank_f, descr_formant_bank_f, args, 3));
}


/* ---------------- mus-srate ---------------- */
static char *descr_srate_f(int *args, ptree *pt) {return(mus_format( FLT_PT " = mus-srate", args[0], FLOAT_RESULT));}
static void srate_f(int *args, ptree *pt) {FLOAT_RESULT = mus_srate();}
static xen_value *mus_srate_1(ptree *prog, xen_value **args, int num_args) 
{
  return(package(prog, R_FLOAT, srate_f, descr_srate_f, args, 0));
}

static char *descr_set_srate_f(int *args, ptree *pt) {return(mus_format("mus-srate = " FLT_PT , args[1], FLOAT_ARG_1));}
static void set_srate_f(int *args, ptree *pt) {mus_set_srate(FLOAT_ARG_1); FLOAT_RESULT = mus_srate();}
static char *descr_set_srate_i(int *args, ptree *pt) {return(mus_format("mus-srate = " INT_PT , args[1], INT_ARG_1));}
static void set_srate_i(int *args, ptree *pt) {mus_set_srate((Float)INT_ARG_1); FLOAT_RESULT = mus_srate();}
static void mus_set_srate_1(ptree *prog, xen_value *in_v, xen_value *in_v1, xen_value *in_v2, xen_value *v)
{
  if (v->type == R_FLOAT)
    add_triple_to_ptree(prog, va_make_triple(set_srate_f, descr_set_srate_f, 2, NULL, v));
  else add_triple_to_ptree(prog, va_make_triple(set_srate_i, descr_set_srate_i, 2, NULL, v));
}


/* ---------------- mus-data, xcoeffs, ycoeffs ---------------- */

#define MUS_VCT_1(Name, Position) \
static char *descr_ ## Name ## _1(int *args, ptree *pt) \
{ \
  return(mus_format( VCT_PT " = mus-" #Name "(" LST_PT ")", args[0], DESC_VCT_RESULT, args[1], DESC_CLM_ARG_1)); \
} \
static void Name ## _1(int *args, ptree *pt) \
{ \
  if (!VCT_RESULT) \
    { \
      VCT_RESULT = (vct *)malloc(sizeof(vct)); \
      VCT_RESULT->length = mus_length(CLM_ARG_1); \
      VCT_RESULT->data = mus_ ## Name (CLM_ARG_1); \
      VCT_RESULT->dont_free = true; \
    } \
} \
static xen_value *mus_ ## Name ## _1(ptree *prog, xen_value **args, int num_args) \
{ \
  if (args[1]->type == R_CLM) \
    return(package(prog, R_VCT, Name ## _1, descr_ ## Name ## _1, args, 1)); \
  return(run_warn("wrong type arg (%s) to mus_ " #Name , type_name(args[1]->type))); \
}

MUS_VCT_1(data, 0)
MUS_VCT_1(xcoeffs, 1)
MUS_VCT_1(ycoeffs, 2)


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

static xen_value *sample_reader_p_1(ptree *prog, xen_value **args, int num_args)
{
  return(make_xen_value(R_BOOL, add_int_to_ptree(prog, args[1]->type == R_READER), R_CONSTANT));
}

static xen_value *mix_sample_reader_p_1(ptree *prog, xen_value **args, int num_args)
{
  return(make_xen_value(R_BOOL, add_int_to_ptree(prog, args[1]->type == R_MIX_READER), R_CONSTANT));
}

static xen_value *track_sample_reader_p_1(ptree *prog, xen_value **args, int num_args)
{
  return(make_xen_value(R_BOOL, add_int_to_ptree(prog, args[1]->type == R_TRACK_READER), R_CONSTANT));
}

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

static xen_value *pair_p_1(ptree *prog, xen_value **args, int num_args)
{
  return(make_xen_value(R_BOOL, add_int_to_ptree(prog, args[1]->type == R_PAIR), R_CONSTANT));
}

static xen_value *char_ge_1(ptree *prog, xen_value **args, int num_args) {return(greater_than_or_equal(prog, false, args, num_args));}
static xen_value *char_gt_1(ptree *prog, xen_value **args, int num_args) {return(greater_than(prog, false, args, num_args));}
static xen_value *char_le_1(ptree *prog, xen_value **args, int num_args) {return(less_than_or_equal(prog, false, args, num_args));}
static xen_value *char_lt_1(ptree *prog, xen_value **args, int num_args) {return(less_than(prog, false, args, num_args));}
static xen_value *char_eq_1(ptree *prog, xen_value **args, int num_args) {return(numbers_equal(prog, false, args, num_args));}

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

static xen_value *ge_1(ptree *prog, xen_value **args, int num_args) {return(greater_than_or_equal(prog, prog->float_result, args, num_args));}
static xen_value *gt_1(ptree *prog, xen_value **args, int num_args) {return(greater_than(prog, prog->float_result, args, num_args));}
static xen_value *le_1(ptree *prog, xen_value **args, int num_args) {return(less_than_or_equal(prog, prog->float_result, args, num_args));}
static xen_value *lt_1(ptree *prog, xen_value **args, int num_args) {return(less_than(prog, prog->float_result, args, num_args));}
static xen_value *eq_1(ptree *prog, xen_value **args, int num_args) {return(numbers_equal(prog, prog->float_result, args, num_args));}

static xen_value *unwrap_xen_object_1(ptree *prog, XEN form, const char *origin, bool constant)
{
  /* fprintf(stderr, "unwrap %s (%s) (%d)\n", XEN_AS_STRING(form), type_name(xen_to_run_type(form)), constant); */
  int type;
  type = xen_to_run_type(form);
  switch (type)
    {
    case R_BOOL:    return(make_xen_value(R_BOOL, add_int_to_ptree(prog, (XEN_FALSE_P(form)) ? 0 : 1), R_CONSTANT)); break;
    case R_INT:     return(make_xen_value(R_INT, add_int_to_ptree(prog, R_XEN_TO_C_INT(form)), R_CONSTANT)); break;
    case R_FLOAT:   return(make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, XEN_TO_C_DOUBLE(form)), R_CONSTANT)); break;
    case R_CHAR:    return(make_xen_value(R_CHAR, add_int_to_ptree(prog, (Int)(XEN_TO_C_CHAR(form))), R_CONSTANT)); break;
    case R_STRING:  return(make_xen_value(R_STRING, add_string_to_ptree(prog, copy_string(XEN_TO_C_STRING(form))), R_CONSTANT)); break;
    case R_VCT:     return(make_xen_value(R_VCT, add_vct_to_ptree(prog, get_vct(form)), R_CONSTANT)); break;
    case R_SOUND_DATA: return(make_xen_value(R_SOUND_DATA, add_sound_data_to_ptree(prog, (sound_data *)XEN_OBJECT_REF(form)), R_CONSTANT)); break;
    case R_PAIR:    if (constant) return(make_xen_value(R_PAIR, add_xen_to_ptree(prog, form), R_CONSTANT)); break;
    case R_LIST:    if (constant) return(make_xen_value(R_LIST, add_xen_to_ptree(prog, form), R_CONSTANT)); break;
    case R_SYMBOL:  return(make_xen_value(R_SYMBOL, add_xen_to_ptree(prog, form), R_CONSTANT)); break;
    case R_KEYWORD: return(make_xen_value(R_KEYWORD, add_xen_to_ptree(prog, form), R_CONSTANT)); break;
    case R_FLOAT_VECTOR:
      if (constant)
	{
	  vct *vc;
	  xen_value *v;
	  vc = vector_to_vct(form);
	  v = make_xen_value(R_FLOAT_VECTOR, add_vct_to_ptree(prog, vc), R_CONSTANT);
	  add_obj_to_gcs(prog, R_FLOAT_VECTOR, v->addr);
	  return(v);
	}
      break;
    case R_INT_VECTOR: 
    case R_CLM_VECTOR:
    case R_VCT_VECTOR:
      if (constant)
	{
	  vect *iv;
	  xen_value *v;
	  iv = read_vector(form, type);
	  v = make_xen_value(type, add_vect_to_ptree(prog, iv), R_CONSTANT);
	  add_obj_to_gcs(prog, type, v->addr);
	  return(v);
	}
      break;
    }
  return(run_warn("%s: non-simple arg: %s", origin, XEN_AS_STRING(form)));
}

static xen_value *unwrap_xen_object(ptree *prog, XEN form, const char *origin)
{
  return(unwrap_xen_object_1(prog, form, origin, false));
}

static xen_value *unwrap_constant_xen_object(ptree *prog, XEN form, const char *origin)
{
  return(unwrap_xen_object_1(prog, form, origin, true));
}

static XEN get_lst(ptree *prog, xen_value **args) 
{
  if ((prog->xens) &&
      (args[1]) &&
      (args[1]->addr < prog->xen_ctr))
    return(prog->xens[args[1]->addr]);
  return(XEN_UNDEFINED);
}

static xen_value *car_1(ptree *prog, xen_value **args, int num_args) {return(unwrap_xen_object(prog, XEN_CAR(get_lst(prog, args)), "car"));}
static xen_value *caar_1(ptree *prog, xen_value **args, int num_args) {return(unwrap_xen_object(prog, XEN_CAAR(get_lst(prog, args)), "caar"));}
static xen_value *cadr_1(ptree *prog, xen_value **args, int num_args) {return(unwrap_xen_object(prog, XEN_CADR(get_lst(prog, args)), "cadr"));}
static xen_value *caaar_1(ptree *prog, xen_value **args, int num_args) {return(unwrap_xen_object(prog, XEN_CAAAR(get_lst(prog, args)), "caaar"));}
static xen_value *caadr_1(ptree *prog, xen_value **args, int num_args) {return(unwrap_xen_object(prog, XEN_CAADR(get_lst(prog, args)), "caadr"));}
static xen_value *cadar_1(ptree *prog, xen_value **args, int num_args) {return(unwrap_xen_object(prog, XEN_CADAR(get_lst(prog, args)), "cadar"));}
static xen_value *caddr_1(ptree *prog, xen_value **args, int num_args) {return(unwrap_xen_object(prog, XEN_CADDR(get_lst(prog, args)), "caddr"));}
static xen_value *caaaar_1(ptree *prog, xen_value **args, int num_args) {return(unwrap_xen_object(prog, XEN_CAAAAR(get_lst(prog, args)), "caaaar"));}
static xen_value *caaadr_1(ptree *prog, xen_value **args, int num_args) {return(unwrap_xen_object(prog, XEN_CAAADR(get_lst(prog, args)), "caaadr"));}
static xen_value *caadar_1(ptree *prog, xen_value **args, int num_args) {return(unwrap_xen_object(prog, XEN_CAADAR(get_lst(prog, args)), "caadar"));}
static xen_value *caaddr_1(ptree *prog, xen_value **args, int num_args) {return(unwrap_xen_object(prog, XEN_CAADDR(get_lst(prog, args)), "caaddr"));}
static xen_value *cadaar_1(ptree *prog, xen_value **args, int num_args) {return(unwrap_xen_object(prog, XEN_CADAAR(get_lst(prog, args)), "cadaar"));}
static xen_value *cadadr_1(ptree *prog, xen_value **args, int num_args) {return(unwrap_xen_object(prog, XEN_CADADR(get_lst(prog, args)), "cadadr"));}
static xen_value *caddar_1(ptree *prog, xen_value **args, int num_args) {return(unwrap_xen_object(prog, XEN_CADDAR(get_lst(prog, args)), "caddar"));}
static xen_value *cadddr_1(ptree *prog, xen_value **args, int num_args) {return(unwrap_xen_object(prog, XEN_CADDDR(get_lst(prog, args)), "cadddr"));}

static xen_value *cdr_1(ptree *prog, xen_value **args, int num_args) {return(unwrap_xen_object(prog, XEN_CDR(get_lst(prog, args)), "cdr"));}

static xen_value *list_length_1(ptree *prog, xen_value **args, int num_args) 
{
  return(make_xen_value(R_INT, add_int_to_ptree(prog, XEN_LIST_LENGTH(get_lst(prog, args))), R_CONSTANT));
}

static xen_value *null_p_1(ptree *prog, xen_value **args, int num_args) 
{
  return(make_xen_value(R_BOOL, add_int_to_ptree(prog, XEN_NULL_P(get_lst(prog, args))), R_CONSTANT));
}

static xen_value *atan_1(ptree *prog, xen_value **args, int num_args) 
{
  if (num_args == 1)
    return(atan1_1(prog, args, num_args));
  return(atan2_1(prog, args, num_args));
}

static xen_value *integer_to_char_1(ptree *prog, xen_value **args, int num_args) 
{
  xen_value *newv;
  newv = copy_xen_value(args[1]);
  newv->type = R_CHAR;
  return(newv);
}

static xen_value *declare_1(ptree *prog, XEN form, walk_result_t need_result) {return(make_xen_value(R_UNSPECIFIED, -1, R_VARIABLE));}
static xen_value *lambda_preform(ptree *prog, XEN form, walk_result_t need_result) {return(lambda_form(prog, form, true, NULL, 0, XEN_FALSE));}
static xen_value *quote_form(ptree *prog, XEN form, walk_result_t need_result) 
{
  return(unwrap_constant_xen_object(prog, XEN_CADR(form), "quote"));
}

/* clm-print and xen args support */

static bool xenable(xen_value *v)
{
  switch (v->type)
    {
    case R_FLOAT: case R_INT: case R_CHAR: case R_STRING: case R_BOOL:
    case R_LIST: case R_PAIR: case R_FLOAT_VECTOR: case R_VCT: case R_SOUND_DATA: case R_KEYWORD: case R_SYMBOL:
    case R_CLM:
      return(true);
      break;
    default:
      return(v->type > R_ANY);
      break;
    }
  return(false);
}

static XEN xen_value_to_xen(ptree *pt, xen_value *v)
{
  XEN val = XEN_UNDEFINED;
  /*
  fprintf(stderr,"xen_value_to_xen: %s %s\n", type_name(v->type), describe_xen_value(v, pt));
  */
  switch (v->type)
    {
    case R_FLOAT:   val = C_TO_XEN_DOUBLE(pt->dbls[v->addr]); break;
    case R_INT:     val = R_C_TO_XEN_INT(pt->ints[v->addr]); break;
    case R_CHAR:    val = C_TO_XEN_CHAR((char)(pt->ints[v->addr])); break;
    case R_STRING:  val = C_TO_XEN_STRING(pt->strs[v->addr]); break;
    case R_BOOL:    return(C_TO_XEN_BOOLEAN(pt->ints[v->addr])); break;
    case R_SYMBOL:
    case R_KEYWORD:
    case R_LIST:    
    case R_PAIR:    val = pt->xens[v->addr]; break;
    case R_FLOAT_VECTOR:
    case R_VCT:
      {
	vct *vc;
	vc = pt->vcts[v->addr];
	val = make_vct_wrapper(vc->length, vc->data);
      }
      break;
    case R_SOUND_DATA:
      {
	sound_data *sd;
	sd = pt->sds[v->addr];
	val = wrap_sound_data(sd->chans, sd->length, sd->data);
      }
      break;
    case R_CLM:
      val = mus_wrap_generator(pt->clms[v->addr]);
      break;
    default:
      if (v->type > R_ANY)
	val = pt->xens[v->addr];
      break;
    }
  if (XEN_BOUND_P(val))
    {
      add_loc_to_protected_list(pt, snd_protect(val));
      return(val);
    }
  return(XEN_FALSE);
}

static triple *make_xen_arg_triple(ptree *pt,
				   void (*function)(int *arg_addrs, ptree *pt),
				   char *(*descr)(int *arg_addrs, ptree *pt), 
				   xen_value **typed_args, int args)
{
  triple *trp;
  int *addrs = NULL;
  xen_value **xaddrs = NULL;
  int i, xloc = -1;
  addrs = (int *)CALLOC((args > 3) ? args : 3, sizeof(int));
  if (args > 0)
    {
      xloc = allocate_xen_vars(pt, args);
      xaddrs = pt->xen_vars[xloc];
      addrs[0] = typed_args[0]->addr; /* string result */
      addrs[1] = typed_args[1]->addr; /* num args */
      addrs[2] = xloc;                /* addr of indirect arg list */
      for (i = 2; i < args; i++) 
	xaddrs[i] = typed_args[i];
    }
  else addrs[2] = -1;
  trp = (triple *)CALLOC(1, sizeof(triple));
  trp->function = function;
  trp->args = addrs;
  trp->descr = descr;
  return(trp);
}

static xen_value *package_n_xen_args(ptree *prog,
				     int type, 
				     void (*function)(int *arg_addrs, ptree *pt),
				     char *(*descr)(int *arg_addrs, ptree *pt),
				     xen_value **args,
				     int num_args)
{
  int i;
  xen_value **new_args;
  xen_value *rtn;
  new_args = (xen_value **)CALLOC(num_args + 2, sizeof(xen_value *));
  for (i = 1; i <= num_args; i++)
    {
      new_args[i + 1] = copy_xen_value(args[i]); /* copy so that w->walker can be used (and cleanup args) */
      add_xen_value_to_gcs(prog, new_args[i + 1]);
    }
  new_args[1] = make_xen_value(R_INT, add_int_to_ptree(prog, num_args), R_CONSTANT);
  new_args[0] = add_empty_var_to_ptree(prog, type);
  add_triple_to_ptree(prog, make_xen_arg_triple(prog, function, descr, new_args, num_args + 2));
  rtn = new_args[0];
  FREE(new_args[1]);
  FREE(new_args);
  return(rtn);
}

static char *describe_xen_args(const char *caller, int result_type, int *args, ptree *pt)
{
  int num_args, i, len = 0;
  char **descrs = NULL;
  char *buf;
  xen_value *res;
  xen_value **xargs = NULL;
  res = make_xen_value(result_type, args[0], R_VARIABLE);
  num_args = pt->ints[args[1]];
  descrs = (char **)CALLOC(num_args + 2, sizeof(char *));
  descrs[0] = describe_xen_value(res, pt);
  len = (2 + strlen(caller));
  if (descrs[0]) len += strlen(descrs[0]);
  FREE(res);
  res = NULL;
  if (args[2] >= 0) xargs = pt->xen_vars[args[2]];
  if (xargs)
    {
      for (i = 2; i <= num_args + 1; i++)
	{
	  descrs[i] = describe_xen_value(xargs[i], pt);
	  if (descrs[i]) len += (2 + strlen(descrs[i]));
	}
    }
  buf = (char *)CALLOC(len + 16, sizeof(char));
  if (descrs[0])
    sprintf(buf, "%s = %s(", descrs[0], caller);
  else sprintf(buf, "%s(", caller);
  for (i = 2; i <= num_args + 1; i++)
    if (descrs[i])
      {
	if (i > 2) strcat(buf, ", ");
	strcat(buf, descrs[i]);
      }
  strcat(buf, ")");
  for (i = 0; i <= num_args + 1; i++)
    if (descrs[i]) FREE(descrs[i]);
  FREE(descrs);
  return(buf);
}

static XEN xen_values_to_list(ptree *pt, int *args)
{
  XEN lst = XEN_EMPTY_LIST;
  int i;
  xen_value **xargs = NULL;
  if (args[2] >= 0) 
    {
      xargs = pt->xen_vars[args[2]];
      for (i = (int)(pt->ints[args[1]] + 1); i >= 2; i--)
	lst = XEN_CONS(xen_value_to_xen(pt, xargs[i]), lst);
    }
  return(lst);
}

static XEN format_func = XEN_FALSE;

static void format_s(int *args, ptree *pt) 
{
  if (STRING_RESULT) FREE(STRING_RESULT);
  /* fprintf(stderr, "apply: %s\n", XEN_AS_STRING(xen_values_to_list(pt, args))); */
  STRING_RESULT = copy_string(XEN_TO_C_STRING(XEN_APPLY(format_func, xen_values_to_list(pt, args), "format")));
}

static char *descr_format_s(int *args, ptree *pt) {return(describe_xen_args("format", R_STRING, args, pt));}

static xen_value *format_1(ptree *prog, xen_value **args, int num_args)
{
  return(package_n_xen_args(prog, R_STRING, format_s, descr_format_s, args, num_args));
}

static char *descr_throw_s_1(int *args, ptree *pt) 
{
  return(mus_format("throw('%s)", (XEN_BOUND_P(pt->xens[args[1]])) ? XEN_AS_STRING(pt->xens[args[1]]) : "#f"));
}
static void throw_s_1(int *args, ptree *pt) 
{
  XEN res;
  res = pt->xens[args[1]];
  free_ptree(pt);
  /* free_ptree handles cleanup/global resets -- can we safely call it here? (i.e. no possible catch within run itself) */
  pt = NULL;
  XEN_THROW(res, XEN_FALSE);
}
static xen_value *throw_1(ptree *prog, xen_value **args, int num_args)
{
  return(package(prog, R_BOOL, throw_s_1, descr_throw_s_1, args, 1));
}

typedef struct {
  xen_value *(*walker)(ptree *prog, xen_value **args, int num_args);
  xen_value *(*special_walker)(ptree *prog, XEN form, walk_result_t need_result);
  void (*set_walker)(ptree *prog, xen_value *in_v, xen_value *in_v1, xen_value *in_v2, xen_value *v);
  int required_args, max_args, result_type, num_arg_types;
  bool need_int_result;
  int *arg_types;
} walk_info;

#if DEBUGGING
static XEN g_describe_walk_info(XEN obj)
{
  XEN walker;
  walk_info *w = NULL;
  walker = XEN_OBJECT_PROPERTY(obj, walk_sym);
  if (XEN_ULONG_P(walker))
    {
      w = (walk_info *)(XEN_TO_C_ULONG(walker));
      if (w)
	{
	  char *buf;
	  int i, len;
	  len = 1024;
	  buf = (char *)CALLOC(len, sizeof(char));
	  sprintf(buf, "%p %p %p: req: %d, max: %d, result: %s, need_int: %s, num_arg_types: %d: ",
		  w->walker, w->special_walker, w->set_walker, 
		  w->required_args, w->max_args, type_name(w->result_type), (w->need_int_result) ? "#t" : "#f", w->num_arg_types);
	  for (i = 0; i < w->num_arg_types; i++)
	    {
	      buf = snd_strcat(buf, type_name(w->arg_types[i]), &len);
	      buf = snd_strcat(buf, " ", &len);
	    }
	  walker = C_TO_XEN_STRING(buf);
	  FREE(buf);
	  return(walker);
	}
    }
  return(XEN_FALSE);
}
#endif

static walk_info *make_walker(xen_value *(*walker)(ptree *prog, xen_value **args, int num_args),
			      xen_value *(*special_walker)(ptree *prog, XEN form, walk_result_t need_result),
			      void (*set_walker)(ptree *prog, xen_value *in_v, xen_value *in_v1, xen_value *in_v2, xen_value *v),
			      int required_args, 
			      int max_args, 
			      int result_type, 
			      bool need_int_result,
			      int num_arg_types,
			      ...) /* arg type list, R_NUMBER=int or float, R_ANY=unchecked */
{
  walk_info *w;
  va_list ap;
  int i;
  w = (walk_info *)malloc(sizeof(walk_info));
  w->walker = walker;
  w->special_walker = special_walker;
  w->set_walker = set_walker;
  w->required_args = required_args;
  w->max_args = max_args;
  w->result_type = result_type;
  w->need_int_result = need_int_result;
  w->num_arg_types = num_arg_types;
  if (num_arg_types > 0)
    {
      va_start(ap, num_arg_types);
      w->arg_types = (int *)calloc(num_arg_types, sizeof(int));
      for (i = 0; i < num_arg_types; i++)
	w->arg_types[i] = (int)(va_arg(ap, int));
      va_end(ap);
    }
  return(w);
}

static void clm_print_s(int *args, ptree *pt) 
{
  if (STRING_RESULT) FREE(STRING_RESULT);
  STRING_RESULT = copy_string(XEN_TO_C_STRING(XEN_APPLY(format_func, xen_values_to_list(pt, args), "clm-print")));
  listener_append(STRING_RESULT);
}

static char *descr_clm_print_s(int *args, ptree *pt) {return(describe_xen_args("clm_print", R_STRING, args, pt));}

static xen_value *clm_print_1(ptree *prog, xen_value **args, int num_args)
{
  XEN_SET_OBJECT_PROPERTY(XEN_NAME_AS_C_STRING_TO_VARIABLE("clm-print"),
			  walk_sym,
			  C_TO_XEN_ULONG(make_walker(clm_print_1, NULL, NULL, 1, UNLIMITED_ARGS, R_STRING, false, 1, -R_XEN)));
  return(package_n_xen_args(prog, R_STRING, clm_print_s, descr_clm_print_s, args, num_args));
}

static bool format_walk_property_set = false;

static xen_value *set_up_format(ptree *prog, xen_value **args, int num_args, bool is_format)
{
  int i;
  XEN format_var = XEN_FALSE;
  format_var = XEN_NAME_AS_C_STRING_TO_VARIABLE("format");
  if ((!(XEN_FALSE_P(format_var))) &&
      (XEN_PROCEDURE_P(XEN_VARIABLE_REF(format_var))))
    {
      /* define a walker for format */
      if (!format_walk_property_set)
	{
	  format_walk_property_set = true; /* odd -- why is this being called more than once? */
	  XEN_SET_OBJECT_PROPERTY(format_var, 
				  walk_sym,
				  C_TO_XEN_ULONG(make_walker(format_1, NULL, NULL, 1, UNLIMITED_ARGS, R_STRING, false, 1, -R_XEN)));
	}
      format_func = XEN_VARIABLE_REF(format_var);
      /* any further formats will be checked in walk, but this one needs explicit check */
      for (i = 1; i <= num_args; i++)
	if (!(xenable(args[i])))
	  {
	    char *xv;
	    xv = describe_xen_value(args[i], prog);
	    run_warn("can't handle %s as arg %d to %s", xv, i, (is_format) ? "format" : "clm-print");
	    FREE(xv);
	    return(NULL);
	  }
      /* no cleanup because it is handled in walk */
      if (is_format) 
	return(format_1(prog, args, num_args));
      else return(clm_print_1(prog, args, num_args));
    }
  return(run_warn("format not defined"));
}

/* -------- CLM make functions -------- */

#define CLM_MAKE_FUNC(Name) \
static void make_ ## Name ## _0(int *args, ptree *pt) \
{ \
  XEN res; \
  res = XEN_APPLY(XEN_VARIABLE_REF(XEN_NAME_AS_C_STRING_TO_VARIABLE(S_make_ ## Name)), xen_values_to_list(pt, args), S_make_ ## Name); \
  if (mus_xen_p(res)) \
    { \
      add_loc_to_protected_list(pt, snd_protect(res)); \
      CLM_RESULT = XEN_TO_MUS_ANY(res); \
    } \
  else CLM_RESULT = NULL; \
} \
static char *descr_make_ ## Name ## _0(int *args, ptree *pt) {return(describe_xen_args(S_make_ ## Name, R_CLM, args, pt));} \
static xen_value *make_ ## Name ## _1(ptree *prog, xen_value **args, int num_args) \
{ \
  return(package_n_xen_args(prog, R_CLM, make_ ## Name ## _0, descr_make_ ## Name ## _0, args, num_args)); \
}

CLM_MAKE_FUNC(all_pass)
CLM_MAKE_FUNC(average)
CLM_MAKE_FUNC(asymmetric_fm)
CLM_MAKE_FUNC(buffer)
CLM_MAKE_FUNC(comb)
CLM_MAKE_FUNC(convolve)
CLM_MAKE_FUNC(delay)
CLM_MAKE_FUNC(env)
CLM_MAKE_FUNC(file_to_frame)
CLM_MAKE_FUNC(file_to_sample)
CLM_MAKE_FUNC(filter)
CLM_MAKE_FUNC(fir_filter)
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
CLM_MAKE_FUNC(ppolar)
CLM_MAKE_FUNC(pulse_train)
CLM_MAKE_FUNC(rand)
CLM_MAKE_FUNC(rand_interp)
CLM_MAKE_FUNC(readin)
CLM_MAKE_FUNC(sample_to_file)
CLM_MAKE_FUNC(sawtooth_wave)
CLM_MAKE_FUNC(sine_summation)
CLM_MAKE_FUNC(square_wave)
CLM_MAKE_FUNC(src)
CLM_MAKE_FUNC(sum_of_cosines)
CLM_MAKE_FUNC(sum_of_sines)
CLM_MAKE_FUNC(ssb_am)
CLM_MAKE_FUNC(table_lookup)
CLM_MAKE_FUNC(triangle_wave)
CLM_MAKE_FUNC(two_pole)
CLM_MAKE_FUNC(two_zero)
CLM_MAKE_FUNC(wave_train)
CLM_MAKE_FUNC(waveshape)
CLM_MAKE_FUNC(zpolar)

static void make_fft_window_0(int *args, ptree *pt)
{
  XEN res;
  res = XEN_APPLY(XEN_VARIABLE_REF(XEN_NAME_AS_C_STRING_TO_VARIABLE(S_make_fft_window)), xen_values_to_list(pt, args), S_make_fft_window);
  add_loc_to_protected_list(pt, snd_protect(res));
  VCT_RESULT = get_vct(res);
}
static char *descr_make_fft_window_0(int *args, ptree *pt) {return(describe_xen_args(S_make_fft_window, R_VCT, args, pt));}
static xen_value *make_fft_window_1(ptree *prog, xen_value **args, int num_args)
{
  return(package_n_xen_args(prog, R_VCT, make_fft_window_0, descr_make_fft_window_0, args, num_args));
}


static int xen_to_addr(ptree *pt, XEN arg, int type, int addr)
{
  /*
  fprintf(stderr, "xen to addr: %s %d %d\n", XEN_AS_STRING(arg), type, addr);
  */
  if ((xen_to_run_type(arg) != type) &&
      ((!(XEN_NUMBER_P(arg))) || ((type != R_FLOAT) && (type != R_INT))))
    XEN_WRONG_TYPE_ARG_ERROR("run", 0, arg, type_name(type));
  switch (type)
    {
    case R_FLOAT:        pt->dbls[addr] = (Double)XEN_TO_C_DOUBLE(arg);     break;
    case R_INT:          pt->ints[addr] = R_XEN_TO_C_INT(arg);              break;
    case R_CHAR:         pt->ints[addr] = (Int)XEN_TO_C_CHAR(arg);          break;
    case R_BOOL:         pt->ints[addr] = (Int)XEN_TO_C_BOOLEAN(arg);       break;
    case R_VCT:          pt->vcts[addr] = get_vct(arg);                     break;
    case R_SOUND_DATA:   pt->sds[addr] = (sound_data *)XEN_OBJECT_REF(arg); break;
    case R_CLM:          pt->clms[addr] = XEN_TO_MUS_ANY(arg);              break;
    case R_READER:       pt->readers[addr] = get_sf(arg);                   break;
    case R_MIX_READER:   pt->mix_readers[addr] = get_mf(arg);               break;
    case R_TRACK_READER: pt->track_readers[addr] = get_tf(arg);             break;
    case R_SYMBOL:
    case R_KEYWORD:
    case R_LIST:
    case R_PAIR:         pt->xens[addr] = arg;                               break;
    case R_STRING:       pt->strs[addr] = copy_string(XEN_TO_C_STRING(arg)); break;
    case R_FLOAT_VECTOR:
      pt->vcts[addr] = vector_to_vct(arg);
      add_obj_to_gcs(pt, R_FLOAT_VECTOR, addr);
      break;
    case R_INT_VECTOR: 
    case R_VCT_VECTOR:
    case R_CLM_VECTOR:
      pt->vects[addr] = read_vector(arg, type);
      add_obj_to_gcs(pt, type, addr);
      break;
    default:
      if (type > R_ANY)
	pt->xens[addr] = arg;
      break;
    }
  return(addr);
}


/* def-clm-struct support */

static int clm_structs = 0;
static int clm_struct_top = 0;
static int *clm_struct_offsets = NULL;
static int *clm_struct_types = NULL;
static char **clm_struct_names = NULL;

#define S_add_clm_field "add-clm-field"
static XEN g_add_clm_field(XEN name, XEN offset, XEN type) /* type=field type (a symbol or #f) */
{
  #define H_add_clm_field "def-clm-struct tie-in to run optimizer"
  char *field_name;
  int i, loc = -1;
  XEN_ASSERT_TYPE(XEN_STRING_P(name), name, XEN_ARG_1, S_add_clm_field, "string");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(offset), offset, XEN_ARG_2, S_add_clm_field, "int");
  /* if def-clm-struct is called twice on the same struct, we need to recognize
   *    existing fields and update them, not tack a copy onto the end of this list.
   */
  field_name = copy_string(XEN_TO_C_STRING(name));
  if (clm_structs == 0)
    {
      loc = 0;
      clm_structs = 8;
      clm_struct_offsets = (int *)CALLOC(clm_structs, sizeof(int));
      clm_struct_types = (int *)CALLOC(clm_structs, sizeof(int));
      clm_struct_names = (char **)CALLOC(clm_structs, sizeof(char *));
      clm_struct_top++;
    }
  else
    {
      for (i = 0; i < clm_struct_top; i++)
	{
	  if (strcmp(field_name, clm_struct_names[i]) == 0)
	    {
	      loc = i;
	      FREE(clm_struct_names[i]);
	      break;
	    }
	}
      if (loc == -1)
	{
	  loc = clm_struct_top;
	  if (clm_struct_top >= clm_structs)
	    {
	      clm_structs = clm_struct_top + 8;
	      clm_struct_offsets = (int *)REALLOC(clm_struct_offsets, clm_structs * sizeof(int));
	      clm_struct_types = (int *)REALLOC(clm_struct_types, clm_structs * sizeof(int));
	      clm_struct_names = (char **)REALLOC(clm_struct_names, clm_structs * sizeof(char *));
	    }
	  clm_struct_top++;
	}
    }
  clm_struct_offsets[loc] = XEN_TO_C_INT(offset);
  clm_struct_types[loc] = R_UNSPECIFIED;
  if (XEN_SYMBOL_P(type))
    clm_struct_types[loc] = name_to_type(XEN_SYMBOL_TO_C_STRING(type));
  clm_struct_names[loc] = field_name;
  return(name);
}

static char **clm_qtypes = NULL;
static int clm_types_size = 0;
static int clm_types_top = 0;

#define S_add_clm_type "add-clm-type"
static XEN g_add_clm_type(XEN name)
{
  #define H_add_clm_type "def-clm-struct tie-in to run optimizer"
  int loc;
  XEN_ASSERT_TYPE(XEN_STRING_P(name), name, XEN_ONLY_ARG, S_add_clm_type, "string");
  if (clm_types_size == 0)
    {
      clm_types_size = 4;
      clm_qtypes = (char **)CALLOC(clm_types_size, sizeof(char *));
    }
  else
    {
      if (clm_types_top >= clm_types_size)
	{
	  clm_types_size = clm_types_top + 4;
	  clm_qtypes = (char **)REALLOC(clm_qtypes, clm_types_size * sizeof(char *));
	}
    }
  loc = add_new_type(XEN_TO_C_STRING(name));
  clm_qtypes[clm_types_top] = (char *)CALLOC(strlen(type_name(loc)) + 2, sizeof(char));
  sprintf(clm_qtypes[clm_types_top], "%s?", type_name(loc));
  clm_types_top++;
  return(name);
}

static bool check_clm_type(XEN sym, const char *type)
{
  return(strcmp(type, XEN_SYMBOL_TO_C_STRING(sym)) == 0);
}

/* to avoid GC complications and take advantage of the type decisions,
 *   def_clm_struct lists are allocated space as local variable of the
 *   initial type, and refs/sets become local variable refs/sets.
 *   local addr is saved/hashed/restored by XEN var/offset.
 */

static int *clm_ref_offsets = NULL, *clm_ref_types = NULL, *clm_ref_addrs = NULL;
static XEN *clm_ref_vars = NULL;
static int clm_ref_size = 0, clm_ref_top = 0;

static int find_clm_var(ptree *prog, XEN lst, XEN lst_ref, int offset, int run_type)
{
  int i, addr = -1;
  for (i = 0; i < clm_ref_top; i++)
    if ((clm_ref_offsets[i] == offset) && (XEN_EQ_P(lst, clm_ref_vars[i])))
      {
	if (run_type != clm_ref_types[i]) return(-1);
	addr = clm_ref_addrs[i];
	break;
      }
  if (addr == -1)
    {
      xen_value *tmp;
      /* we get here if this ref hasn't been registered yet */
      if (clm_ref_top == clm_ref_size)
	{
	  /* the usual list (re)allocation junk... */
	  if (clm_ref_size == 0)
	    {
	      clm_ref_size = 8;
	      clm_ref_offsets = (int *)CALLOC(clm_ref_size, sizeof(int));
	      clm_ref_types = (int *)CALLOC(clm_ref_size, sizeof(int));
	      clm_ref_addrs = (int *)CALLOC(clm_ref_size, sizeof(int));
	      clm_ref_vars = (XEN *)CALLOC(clm_ref_size, sizeof(XEN));
	      for (i = 0; i < clm_ref_size; i++) clm_ref_vars[i] = XEN_UNDEFINED;
	    }
	  else
	    {
	      clm_ref_size += 8;
	      clm_ref_offsets = (int *)REALLOC(clm_ref_offsets, clm_ref_size * sizeof(int));
	      clm_ref_types = (int *)REALLOC(clm_ref_types, clm_ref_size * sizeof(int));
	      clm_ref_addrs = (int *)REALLOC(clm_ref_addrs, clm_ref_size * sizeof(int));
	      clm_ref_vars = (XEN *)REALLOC(clm_ref_vars, clm_ref_size * sizeof(XEN));
	      for (i = clm_ref_size - 8; i < clm_ref_size; i++) clm_ref_vars[i] = XEN_UNDEFINED;
	    }
	}
      /* first decide if we can handle this variable */
      tmp = add_empty_var_to_ptree(prog, run_type);
      addr = tmp->addr;
      FREE(tmp);
      if (addr >= 0)
	{
	  i = clm_ref_top++;
	  clm_ref_offsets[i] = offset;
	  clm_ref_types[i] = run_type;
	  clm_ref_vars[i] = lst;
	  clm_ref_addrs[i] = addr;
	  /* fprintf(stderr, "%s: %d %s %s %d\n", XEN_AS_STRING(lst), offset, type_name(run_type), XEN_AS_STRING(lst), addr); */
	}
    }
  /* now set the (initial) value */
  if (addr >= 0) xen_to_addr(prog, lst_ref, run_type, addr);
  return(addr);
}


static char *descr_clm_struct_ref_r(int *args, ptree *pt)
{
  return(mus_format("%s = %s(%s)",
		    describe_xen_value_1(pt->ints[args[3]], args[0], pt),
		    clm_struct_names[pt->ints[args[4]]],
		    (pt->ints[args[1]] == 0) ? "null" : XEN_AS_STRING(pt->xens[args[1]])));
}

static void clm_struct_ref_r(int *args, ptree *pt)
{
  XEN lst;
  lst = pt->xens[args[1]];
  if ((XEN_BOUND_P(lst)) && (XEN_LIST_P(lst)))
    {
      xen_to_addr(pt, 
		  XEN_LIST_REF(lst, 
			       pt->ints[args[2]]), /* struct field offset into list */
		  pt->ints[args[3]],               /* result type */
		  args[0]);                    /* result address */
    }
}

static char *descr_clm_struct_set_r(int *args, ptree *pt)
{
  return(mus_format("%s(%s) = %s",
		    clm_struct_names[pt->ints[args[4]]],
		    (XEN_BOUND_P(pt->xens[args[1]])) ? XEN_AS_STRING(pt->xens[args[1]]) : "null",
		    describe_xen_value_1(pt->ints[args[3]], pt->ints[args[5]], pt)));
}

static void clm_struct_set_r(int *args, ptree *pt)
{
  XEN lst;
  xen_value *v;
  lst = pt->xens[args[1]];
  if ((XEN_BOUND_P(lst)) && (XEN_LIST_P(lst)))
    {
      v = make_xen_value(pt->ints[args[3]], pt->ints[args[5]], R_CONSTANT);
      XEN_LIST_SET(lst, pt->ints[args[2]], xen_value_to_xen(pt, v));
      FREE(v);
    }
}

static xen_value *clm_struct_ref(ptree *prog, xen_value *v, int struct_loc, xen_value *set_v)
{
  /* types can't change within run */
  int run_type, addr, offset;
  XEN lst, lst_ref;
  if (v == NULL) return(run_warn("clm-struct-ref confused"));
  offset = clm_struct_offsets[struct_loc];
  lst = prog->xens[v->addr];
  if (!(XEN_LIST_P(lst)))
    {
      /* run-time list-ref here */
      run_type = clm_struct_types[struct_loc];
      if (run_type != R_UNSPECIFIED)
	{
	  xen_value **new_args;
	  xen_value *rtn;
	  int i;
	  new_args = (xen_value **)CALLOC((set_v) ? 6 : 5, sizeof(xen_value *));
	  rtn = add_empty_var_to_ptree(prog, run_type);
	  new_args[0] = rtn;
	  new_args[1] = v;
	  new_args[2] = make_xen_value(R_INT, add_int_to_ptree(prog, offset), R_CONSTANT);
	  new_args[3] = make_xen_value(R_INT, add_int_to_ptree(prog, run_type), R_CONSTANT);
	  new_args[4] = make_xen_value(R_INT, add_int_to_ptree(prog, struct_loc), R_CONSTANT);
	  if (set_v)
	    {
	      new_args[5] = make_xen_value(R_INT, add_int_to_ptree(prog, set_v->addr), R_CONSTANT);
	      add_triple_to_ptree(prog, make_triple(clm_struct_set_r, descr_clm_struct_set_r, new_args, 6));
	    }
	  else add_triple_to_ptree(prog, make_triple(clm_struct_ref_r, descr_clm_struct_ref_r, new_args, 5));
	  for (i = 2; i <= 4; i++) FREE(new_args[i]);
	  if (set_v) FREE(new_args[5]);
	  FREE(new_args);
	  return(rtn);
	}
      return(run_warn("%s struct field needs type declaration", clm_struct_names[struct_loc]));
    }
  lst_ref = XEN_LIST_REF(lst, offset); /* get type at parse time */
  run_type = xen_to_run_type(lst_ref);
  if ((clm_struct_types[struct_loc] != R_UNSPECIFIED) &&
      (run_type != clm_struct_types[struct_loc]))
    return(run_warn("%s declared type, %s, does not match current type, %s: %s",
		    clm_struct_names[struct_loc],
		    type_name(clm_struct_types[struct_loc]),
		    type_name(run_type),
		    XEN_AS_STRING(lst_ref)));
  addr = find_clm_var(prog, lst, lst_ref, offset, run_type); /* if doesn't exist add to tables, else return holder */
  if (addr < 0) 
    return(run_warn("problem with %s (def-clm-struct) value: %s from %s", 
		    clm_struct_names[struct_loc], 
		    XEN_AS_STRING(lst_ref),
		    XEN_AS_STRING(lst)));
  return(make_xen_value(run_type, addr, R_VARIABLE));
}

static void clm_struct_restore(ptree *prog, xen_var *var)
{
  XEN lst;
  int i, loc, addr, run_type;
  lst = prog->xens[var->v->addr];
  for (i = 0; i < clm_ref_top; i++)
    if (XEN_EQ_P(clm_ref_vars[i], lst))
      {
	/* restore this field */
	loc = clm_ref_offsets[i];    /* list-set index */
	addr = clm_ref_addrs[i];     /* ptree val addr */
	run_type = clm_ref_types[i]; /* val type */
	switch (run_type)
	  {
	  case R_BOOL:   XEN_LIST_SET(lst, loc, C_TO_XEN_BOOLEAN(prog->ints[addr])); break;
	  case R_INT:    XEN_LIST_SET(lst, loc, R_C_TO_XEN_INT(prog->ints[addr])); break;
	  case R_FLOAT:  XEN_LIST_SET(lst, loc, C_TO_XEN_DOUBLE(prog->dbls[addr])); break;
	  case R_CHAR:   XEN_LIST_SET(lst, loc, C_TO_XEN_CHAR(prog->ints[addr])); break;
	  case R_STRING: XEN_LIST_SET(lst, loc, C_TO_XEN_STRING(prog->strs[addr])); break;
	  case R_FLOAT_VECTOR:
	    if (XEN_VECTOR_P(XEN_LIST_REF(lst, loc)))
	      vct_into_vector(prog->vcts[var->v->addr], XEN_LIST_REF(lst, loc));
	    break;
	  case R_INT_VECTOR: 
	    if (XEN_VECTOR_P(XEN_LIST_REF(lst, loc)))
	      int_vect_into_vector(prog->vects[var->v->addr], XEN_LIST_REF(lst, loc));
	    break;
	  }
      }
}





/* ---------------- the code walker ---------------- */

static xen_value *clean_up(xen_value *result, xen_value **args, int args_size)
{
  int i;
  /* args[0] is special */
  if ((args[0]) && (args[0] != result)) FREE(args[0]);
  for (i = 1; i <= args_size; i++)
    if (args[i]) 
      {
	FREE(args[i]);
	args[i] = NULL;
      }
  FREE(args);
  return(result);
}

static bool vowel_p(char b)
{
  return((b == 'a') || (b == 'e') || (b == 'i') || (b == 'o') || (b == 'u'));
}

static xen_value *arg_warn(ptree *prog, char *funcname, int arg_num, xen_value **args, const char *correct_type)
{
  char *xb, *tb;
  xb = describe_xen_value(args[arg_num], prog);
  tb = type_name(args[arg_num]->type);
  if (xb)
    {
      run_warn("%s argument %d (%s) is a%s %s, not a%s %s?", 
	       funcname, arg_num, xb, 
	       (vowel_p(tb[0])) ? "n" : "",
		tb,
	       (vowel_p(correct_type[0])) ? "n" : "",
	       correct_type);
      FREE(xb);
    }
  else 
    run_warn("%s argument %d is a%s %s, not a%s %s?", 
	     funcname, arg_num, 
	     (vowel_p(tb[0])) ? "n" : "",
	     type_name(args[arg_num]->type),
	     (vowel_p(correct_type[0])) ? "n" : "", 
	     correct_type);
  return(NULL);
}

#define WITH_PROCPROP 0
/* aimed at eventual apply support -- places walker property on function so it can be
 *   found even if name has changed -- (define hi min) for example (not very useful by itself)
 */

static xen_value *walk(ptree *prog, XEN form, walk_result_t walk_result)
{
  /* walk form, storing vars, making program entries for operators etc */
  XEN rtnval = XEN_FALSE;
  /* fprintf(stderr, "walk %s (needed: %d)\n", XEN_AS_STRING(form), (int)walk_result); */
  if (current_optimization == DONT_OPTIMIZE) return(NULL);

  if (XEN_LIST_P(form))
    {
      XEN function, all_args;
      char *funcname = "not-a-function";
      xen_value **args = NULL;
      int i, k, num_args, constants = 0;
      bool float_result = false;
      xen_var *var;
      xen_value *v = NULL;
      XEN walker;
      walk_info *w = NULL;
      function = XEN_CAR(form);
      all_args = XEN_CDR(form);
      num_args = XEN_LIST_LENGTH(all_args);
      if (XEN_SYMBOL_P(function))
	{
	  walker = XEN_OBJECT_PROPERTY(function, walk_sym);
#if WITH_PROCPROP
	  if ((XEN_FALSE_P(walker)) && (XEN_PROCEDURE_P(XEN_VARIABLE_REF(XEN_SYMBOL_TO_VARIABLE(function)))))
	    walker = XEN_PROCEDURE_PROPERTY(XEN_VARIABLE_REF(XEN_SYMBOL_TO_VARIABLE(function)), walk_sym);
#endif
	  if (XEN_ULONG_P(walker))
	    {
	      w = (walk_info *)(XEN_TO_C_ULONG(walker));
	      if ((w) && (w->special_walker))
		{
		  if (num_args < w->required_args) 
		    return(run_warn("missing expression for %s: %s", XEN_SYMBOL_TO_C_STRING(function), XEN_AS_STRING(form)));
		  if ((w->max_args != UNLIMITED_ARGS) && 
		      (num_args > w->max_args)) 
		    return(run_warn("too many expressions for %s: %s", XEN_SYMBOL_TO_C_STRING(function), XEN_AS_STRING(form)));
		  return((*(w->special_walker))(prog, form, walk_result));
		}
	    }
	  funcname = XEN_SYMBOL_TO_C_STRING(function);
	}
      args = (xen_value **)CALLOC(num_args + 1, sizeof(xen_value *));
      if (num_args > 0)
	{
	  walk_result_t arg_result = NEED_ANY_RESULT;
	  for (i = 0; i < num_args; i++, all_args = XEN_CDR(all_args))
	    {
	      if (w)
		{
		  if ((w->need_int_result) ||
		      ((i < w->num_arg_types) && (w->arg_types[i] == R_INT)))
		    arg_result = NEED_INT_RESULT;
		}
	      args[i + 1] = walk(prog, XEN_CAR(all_args), arg_result);
	      if ((args[i + 1] == NULL) ||
		  (((args[i + 1]->type == R_LIST) || 
		    (args[i + 1]->type == R_PAIR)) && 
		   (XEN_EQ_P(prog->xens[args[i + 1]->addr], XEN_UNDEFINED))))
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
	      if (XEN_SYMBOL_P(function))
		{
		  v = add_global_var_to_ptree(prog, function, &rtnval);
		  /* if all else fails, we'll check rtnval later for externally defined functions */
		}
	      /* (let ((gen (make-oscil 440))) (vct-map! v (lambda () (gen 0.0)))) */
	      else 
		if (XEN_LIST_P(function))
		  v = walk(prog, function, NEED_ANY_RESULT);
	      /* trying to support stuff like ((vector-ref gens 0) 0.0) here */
	    }
	  else v = var->v;
	  if (v) 
	    {
	      switch (v->type)
		{
		case R_READER:       if (num_args == 0) return(clean_up(reader_0(prog, args, v), args, num_args));       break;
		case R_MIX_READER:   if (num_args == 0) return(clean_up(mix_reader_0(prog, args, v), args, num_args));   break;
		case R_TRACK_READER: if (num_args == 0) return(clean_up(track_reader_0(prog, args, v), args, num_args)); break;
		case R_CLM:          return(clean_up(clm_n(prog, args, num_args, v), args, num_args));                   break;
		case R_BOOL:
		case R_GOTO:         if (num_args == 0) return(clean_up(goto_0(prog, args, v), args, num_args));         break;
		case R_FUNCTION:     return(clean_up(funcall_n(prog, args, num_args, v), args, num_args));               break;
#if DEBUGGING
		default:             
		  fprintf(stderr, "run got %s as function in %s\n", describe_xen_value(v, prog), XEN_AS_STRING(form));
		  break;
#endif
		}
	      if (var == NULL) FREE(v);
	    }
	  if (prog->goto_ctr > 0)
	    {
	      /* possibly continuation procedure */
	      continuation *c = NULL;
	      for (i = prog->goto_ctr - 1; i >= 0; i--)
		{
		  c = prog->gotos[i];
		  if ((c) && 
		      (c->name) &&
		      (strcmp(c->name, funcname) == 0))
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
		      add_triple_to_ptree(prog, va_make_triple(jump_abs, descr_jump_abs, 1, c->jump));
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
		  if ((w->arg_types[i] != args[i + 1]->type) &&
		      (w->arg_types[i] != R_ANY) &&
		      (!(((POINTER_P(w->arg_types[i])) || (w->arg_types[i] == R_FUNCTION)) && (args[i + 1]->type == R_BOOL))))
		    {
		      if (w->arg_types[i] == R_NUMBER)
			{
			  if ((args[i + 1]->type != R_INT) &&
			      (args[i + 1]->type != R_FLOAT))
			    return(clean_up(arg_warn(prog, funcname, i + 1, args, "number"), args, num_args));
			}
		      else
			{
			  if (w->arg_types[i] == R_VECTOR)
			    {
			      if (!(VECTOR_P(args[i + 1]->type)))
				return(clean_up(arg_warn(prog, funcname, i + 1, args, "vector"), args, num_args));
			    }
			  else
			    {
			      if (w->arg_types[i] == R_XEN)
				{
				  if (!(xenable(args[i + 1])))
				    return(clean_up(arg_warn(prog, funcname, i + 1, args, "indirect"), args, num_args));
				}
			      else
				{
				  if (w->arg_types[i] == R_CONS)
				    {
				      if ((args[i + 1]->type != R_LIST) &&
					  (args[i + 1]->type != R_PAIR))
					return(clean_up(arg_warn(prog, funcname, i + 1, args, "cons"), args, num_args));
				    }
				  else 
				    return(clean_up(arg_warn(prog, funcname, i + 1, args, type_name(w->arg_types[i])), args, num_args));
				}
			    }
			}
		    }
		}
	    }
	  if (w->walker)
	    {
	      prog->constants = constants;
	      prog->float_result = float_result;
	      prog->walk_result = walk_result;
	      return(clean_up((*(w->walker))(prog, args, num_args), args, num_args));
	    }
	}
      if ((num_args == 2) && (strcmp(funcname, "list-ref") == 0) && (XEN_EXACT_P(XEN_CADDR(form))))
	{
	  XEN lst;
	  lst = get_lst(prog, args);
	  if ((XEN_BOUND_P(lst)) && (XEN_LIST_P(lst)))
	    return(clean_up(unwrap_xen_object(prog, XEN_LIST_REF_WRAPPED(get_lst(prog, args), XEN_CADDR(form)), funcname), args, num_args));
	  else return(clean_up(run_warn("can't handle this list: %s", XEN_AS_STRING(form)), args, num_args));
	}
      /*
      fprintf(stderr, "arg 1 %p: addr: %d type: %s\n", args[1], args[1]->addr, type_name(args[1]->type));
      */
      for (k = 0; k < clm_struct_top; k++)
	if (strcmp(funcname, clm_struct_names[k]) == 0)
	  return(clean_up(clm_struct_ref(prog, args[1], k, NULL), args, num_args));
      for (k = 0; k < clm_types_top; k++)
	if (strcmp(funcname, clm_qtypes[k]) == 0)
	  return(clean_up(make_xen_value(R_BOOL, 
					 add_int_to_ptree(prog, check_clm_type(XEN_CAR(get_lst(prog, args)), type_name(R_ANY + k + 1))),
					 R_CONSTANT), 
			  args, num_args));

      if (strcmp(funcname, "format") == 0)
	return(clean_up(set_up_format(prog, args, num_args, true), args, num_args));
      if (strcmp(funcname, S_clm_print) == 0)
	return(clean_up(set_up_format(prog, args, num_args, false), args, num_args));

      /* check for function defined elsewhere, get source, splice in if possible */
      if ((v == NULL) && 
	  (current_optimization >= SOURCE_OK) &&
	  (XEN_PROCEDURE_P(rtnval)) &&
	  (XEN_FALSE_P(XEN_PROCEDURE_WITH_SETTER_P(rtnval))))
	{
	  XEN func_form;
	  /*
	  fprintf(stderr, "try to splice in %s ", funcname);
	  */
	  func_form = XEN_PROCEDURE_SOURCE(rtnval);
	  if ((XEN_LIST_P(func_form)) &&
	      (XEN_SYMBOL_P(XEN_CAR(func_form))) &&
	      (strcmp("lambda", XEN_SYMBOL_TO_C_STRING(XEN_CAR(func_form))) == 0))
	    {
	      /* look for procedure source, use current arg types as auto-declaration */
	      bool old_got_lambda;
	      old_got_lambda = got_lambda;
	      got_lambda = true;
	      v = lambda_form(prog, func_form, true, args, num_args, rtnval);
	      got_lambda = old_got_lambda;
	      if (v) 
		{
		  xen_value *result;
		  add_var_to_ptree(prog, funcname, v);
		  result = funcall_n(prog, args, num_args, v);
		  FREE(v);
		  return(clean_up(result, args, num_args));
		}
	    }
	}

      if ((walk_result) || (XEN_LIST_P(form)))
	/* need the list check as well because the called function might have side-effects */
	return(clean_up(NULL, args, num_args));

      return(clean_up(make_xen_value(R_UNSPECIFIED, -1, R_CONSTANT), args, num_args));
    }
  else /* not a list */
    {
      int type;
      type = xen_to_run_type(form);
      switch (type)
	{
	case R_INT:     return(make_xen_value(R_INT, add_int_to_ptree(prog, R_XEN_TO_C_INT(form)), R_CONSTANT)); break;
	case R_FLOAT:   return(make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, XEN_TO_C_DOUBLE(form)), R_CONSTANT)); break;
	case R_STRING:  return(make_xen_value(R_STRING, add_string_to_ptree(prog, copy_string(XEN_TO_C_STRING(form))), R_CONSTANT)); break;
	case R_CHAR:    return(make_xen_value(R_CHAR, add_int_to_ptree(prog, (Int)(XEN_TO_C_CHAR(form))), R_CONSTANT)); break;
	case R_BOOL:    return(make_xen_value(R_BOOL, add_int_to_ptree(prog, (XEN_FALSE_P(form)) ? 0 : 1), R_CONSTANT)); break;
	  /* case R_LIST:    can't happen */
	case R_PAIR:    return(make_xen_value(R_PAIR, add_xen_to_ptree(prog, form), R_CONSTANT)); break;
	case R_KEYWORD: return(make_xen_value(R_KEYWORD, add_xen_to_ptree(prog, form), R_CONSTANT)); break;

#if (HAVE_SCM_MAKE_RATIO || HAVE_SCM_C_MAKE_RECTANGULAR)
	case R_UNSPECIFIED:
	  if (strcmp(XEN_AS_STRING(form), "#@lambda") == 0)
	    return(make_xen_value(R_INT, add_int_to_ptree(prog, 0), R_CONSTANT)); break;
	  break;
#endif

	default:
	  if (type > R_ANY)
	    return(make_xen_value(type, add_xen_to_ptree(prog, form), R_CONSTANT));
	  break;
	}
      if (XEN_SYMBOL_P(form))
	{
	  XEN ignore = XEN_FALSE;
	  prog->walk_result = walk_result;
	  return(add_global_var_to_ptree(prog, form, &ignore));
	}
      /* things like complex numbers fall through here */
    }
  if (!run_warned)
    return(run_warn("can't handle: %s", XEN_AS_STRING(form)));
  return(NULL);
}

static xen_value *lookup_generalized_set(ptree *prog, XEN acc_form, xen_value *in_v, xen_value *in_v1, xen_value *in_v2, xen_value *v)
{
  char *accessor;
  int k, happy = 0;
  XEN walker;
  walk_info *w = NULL;
  walker = XEN_OBJECT_PROPERTY(acc_form, walk_sym);
  if (XEN_ULONG_P(walker))
    {
      w = (walk_info *)(XEN_TO_C_ULONG(walker));
      if (w)
	{
	  if ((w->set_walker) &&
	      ((v->type == w->result_type) ||
	       ((w->result_type == R_NUMBER) &&
		((v->type == R_FLOAT) || (v->type == R_INT)))))
	    {
	      (*(w->set_walker))(prog, in_v, in_v1, in_v2, v);
	      happy = 1;
	    }
	  else 
	    {
	      char *xb, *tb, *vb;
	      xb = describe_xen_value(v, prog);
	      tb = type_name(w->result_type);
	      vb = type_name(v->type);
	      run_warn("can't set %s (a%s %s) to %s (a%s %s)",
		       XEN_SYMBOL_TO_C_STRING(acc_form),
		       tb, (vowel_p(tb[0])) ? "n" : "",
		       xb,
		       vb, (vowel_p(vb[0])) ? "n" : "");
	      if (xb) FREE(xb);
	      happy = 2;
	    }
	}
    }
  else
    {
      xen_value *sv = NULL;
      accessor = XEN_SYMBOL_TO_C_STRING(acc_form);
      for (k = 0; k < clm_struct_top; k++)
	if (strcmp(accessor, clm_struct_names[k]) == 0)
	  {
	    sv = clm_struct_ref(prog, in_v, k, v);
	    if (sv)
	      {
		if (v->type == sv->type)
		  {
		    xen_var *lst;
		    triple *trp;
		    trp = set_var(prog, sv, v);
		    if (trp == NULL) {happy = 0; break;}
		    trp->no_opt = true;
		    lst = find_var_in_ptree_via_addr(prog, in_v->type, in_v->addr);
		    if (lst) lst->unclean = true;
		    happy = 1;
		  }
		else 
		  {
		    char *xb;
		    xb = describe_xen_value(v, prog);
		    run_warn("can't set %s (%s) to %s (%s)",
			     accessor,
			     type_name(sv->type),
			     xb,
			     type_name(v->type));
		    if (xb) FREE(xb);
		    happy = 2;
		  }
		FREE(sv);
		sv = NULL;
	      }
	    break;
	  }
    }
  if (in_v) FREE(in_v);
  if (in_v1) FREE(in_v1);
  if (in_v2) FREE(in_v2);
  if (happy == 1) return(v);
  if (happy == 0) run_warn("can't set %s", XEN_SYMBOL_TO_C_STRING(acc_form));
  return(NULL);
}

typedef enum {NO_PTREE_DISPLAY, STDERR_PTREE_DISPLAY, LISTENER_PTREE_DISPLAY} ptree_display_t;
static ptree_display_t ptree_on = NO_PTREE_DISPLAY;
static XEN g_show_ptree(XEN on)
{
  ptree_on = (ptree_display_t)XEN_TO_C_INT(on);
  return(on);
}

static void *form_to_ptree(XEN code)
{
  ptree *prog;
  XEN form;
  run_warned = false;
  current_optimization = optimization(ss);
  if (current_optimization == DONT_OPTIMIZE) return(NULL);
  form = XEN_CAR(code);
  prog = make_ptree(8);
  if ((XEN_PROCEDURE_P(XEN_CADR(code))) && 
      (!(XEN_APPLICABLE_SMOB_P(XEN_CADR(code))))) /* applicable smobs cause confusion here */
    prog->code = XEN_CADR(code);                  /* need env before starting to walk the code */
  else prog->code = XEN_FALSE;                    /* many confusing cases here -- we'll just give up */
  if (XEN_SYMBOL_P(form))
    return(free_ptree((void *)prog));
  prog->result = walk(prog, form, NEED_ANY_RESULT);
  if (prog->result)
    {
      add_triple_to_ptree(prog, make_triple(quit, descr_quit, NULL, 0));
      /* now check that we were able to nail down all global variable types */
#ifdef DESCRIBE_PTREE
      {
	char *msg;
	msg = describe_ptree(prog);
	if (ptree_on == STDERR_PTREE_DISPLAY) fprintf(stderr, msg);
	else if (ptree_on == LISTENER_PTREE_DISPLAY) listener_append(msg);
	FREE(msg);
      }
#endif
      prog->form = form;
      prog->form_loc = snd_protect(prog->form);
      return((void *)prog);
    }
  if (!run_warned)
    run_warn("can't optimize: %s\n", XEN_AS_STRING(form));
  return(free_ptree((void *)prog));
}

/* ---------------- various tree-building wrappers ---------------- */

void *form_to_ptree_1_f(XEN code)
{
  ptree *pt;
  pt = (ptree *)form_to_ptree(code);
  if (pt)
    {
      if ((pt->result->type == R_FLOAT) && (pt->arity == 1))
	return(pt);
      free_ptree(pt);
    }
  return(NULL);
}

void *form_to_ptree_3_f(XEN code)
{
  ptree *pt;
  pt = (ptree *)form_to_ptree(code);
  if (pt)
    {
      if ((pt->result->type == R_FLOAT) && (pt->arity == 3))
	return(pt);
      free_ptree(pt);
    }
  return(NULL);
}

void *form_to_ptree_0_f(XEN code)
{
  ptree *pt;
  pt = (ptree *)form_to_ptree(code);
  if (pt)
    {
      if ((pt->result->type == R_FLOAT) && (pt->arity == 0))
	return(pt);
      free_ptree(pt);
    }
  return(NULL);
}

void *form_to_ptree_1_b(XEN code)
{
  ptree *pt;
  pt = (ptree *)form_to_ptree(code);
  if (pt)
    {
      if ((pt->result->type == R_BOOL) && (pt->arity == 1))
	return(pt);
      free_ptree(pt);
    }
  return(NULL);
}

void *form_to_ptree_1_b_without_env(XEN code)
{
  return(form_to_ptree_1_b(XEN_LIST_2(code, XEN_FALSE)));
}


/* ---------------- various evaluator wrappers ---------------- */

Float evaluate_ptree_1f2f(void *upt, Float arg)
{
  ptree *pt = (ptree *)upt;
  pt->dbls[pt->args[0]] = arg;
  eval_ptree(pt);
  return(pt->dbls[pt->result->addr]);
}

Float evaluate_ptree_1f1v1b2f(void *upt, Float arg, vct *v, bool dir)
{
  ptree *pt = (ptree *)upt;
  pt->dbls[pt->args[0]] = arg;
  pt->vcts[pt->args[1]] = v;
  pt->ints[pt->args[2]] = (Int)dir;
  eval_ptree(pt);
  return(pt->dbls[pt->result->addr]);
}

Float evaluate_ptree_0f2f(void *upt)
{
  ptree *pt = (ptree *)upt;
  eval_ptree(pt);
  return(pt->dbls[pt->result->addr]);
}

int evaluate_ptree_1f2b(void *upt, Float arg)
{
  ptree *pt = (ptree *)upt;
  pt->dbls[pt->args[0]] = arg;
  eval_ptree(pt);
  return(pt->ints[pt->result->addr]);
}

Float evaluate_ptreec(void *upt, Float arg, vct *v, bool dir)
{
  ptree *pt = (ptree *)upt;
  pt->dbls[pt->args[0]] = arg;
  if (pt->arity > 1)
    {
      if (pt->vcts)
	pt->vcts[pt->args[1]] = v;
      pt->ints[pt->args[2]] = (Int)dir;
    }
  eval_ptree(pt);
  return(pt->dbls[pt->result->addr]);
}

static XEN eval_ptree_to_xen(ptree *pt)
{
  XEN result = XEN_FALSE;
  switch (pt->result->type)
    {
    case R_FLOAT:   result = C_TO_XEN_DOUBLE(pt->dbls[pt->result->addr]);       break;
    case R_INT:     result = R_C_TO_XEN_INT(pt->ints[pt->result->addr]);        break;
    case R_CHAR:    result = C_TO_XEN_CHAR((char)(pt->ints[pt->result->addr])); break;
    case R_STRING:  result = C_TO_XEN_STRING(pt->strs[pt->result->addr]);       break;
    case R_BOOL:    result = C_TO_XEN_BOOLEAN(pt->ints[pt->result->addr]);      break;
    case R_LIST:
    case R_PAIR:
    case R_SYMBOL:
    case R_KEYWORD: result = pt->xens[pt->result->addr];                        break;
    case R_CLM:     result = mus_wrap_generator(pt->clms[pt->result->addr]);    break;
    case R_VCT:
      {
	vct *v;
	v = c_vct_copy(pt->vcts[pt->result->addr]);
	result = make_vct(v->length, v->data);
	FREE(v);
      }
      break;
    default:
      if (pt->result->type > R_ANY)
	result = pt->xens[pt->result->addr]; 
      break;
    }
  return(result);
}

/* ---------------- internal testing stuff ---------------- */

static XEN g_run_eval(XEN code, XEN arg, XEN arg1, XEN arg2)
{
  ptree *pt;
  XEN result = XEN_FALSE;
  current_optimization = SOURCE_OK;
  pt = make_ptree(8);
  pt->result = walk(pt, code, NEED_ANY_RESULT);
  if (pt->result)
    {
      add_triple_to_ptree(pt, make_triple(quit, descr_quit, NULL, 0));
#ifdef DESCRIBE_PTREE
      {
	char *msg;
	msg = describe_ptree(pt);
	if (ptree_on == STDERR_PTREE_DISPLAY) fprintf(stderr, msg);
	else if (ptree_on == LISTENER_PTREE_DISPLAY) listener_append(msg);
	FREE(msg);
      }
#endif
      if (pt->args)
	{
	  if (pt->arity > 0)
	    {
	      if (!(XEN_BOUND_P(arg))) XEN_ERROR(XEN_ERROR_TYPE("wrong-number-of-args"), code);
	      xen_to_addr(pt, arg, pt->arg_types[0], pt->args[0]);
	      if (pt->arity > 1)
		{
		  if (!(XEN_BOUND_P(arg1))) XEN_ERROR(XEN_ERROR_TYPE("wrong-number-of-args"), code);
		  xen_to_addr(pt, arg1, pt->arg_types[1], pt->args[1]);
		  if (pt->arity > 2)
		    {
		      if (!(XEN_BOUND_P(arg2))) XEN_ERROR(XEN_ERROR_TYPE("wrong-number-of-args"), code);
		      xen_to_addr(pt, arg2, pt->arg_types[2], pt->args[2]);
		      if (pt->arity > 3) XEN_ERROR(XEN_ERROR_TYPE("wrong-number-of-args"), code);
		    }
		}
	    }
	}
      eval_ptree(pt);
      result = eval_ptree_to_xen(pt);
      free_ptree((void *)pt);
      return(result);
    }
  if (pt) free_ptree((void *)pt);
  XEN_ERROR(CANNOT_PARSE,
	    code);
  return(XEN_FALSE);
}
#else
void *form_to_ptree_1_b(XEN code) {return(NULL);}
void *form_to_ptree_3_f(XEN code) {return(NULL);}
void *form_to_ptree_1_b_without_env(XEN code) {return(NULL);}
void *form_to_ptree_1_f(XEN code) {return(NULL);}
Float evaluate_ptree_1f1v1b2f(void *upt, Float arg, vct *v, bool dir) {return(0.0);}
Float evaluate_ptree_0f2f(void *upt) {return(0.0);}
void *form_to_ptree_0_f(XEN code) {return(NULL);}
Float evaluate_ptree_1f2f(void *upt, Float arg) {return(0.0);}
int evaluate_ptree_1f2b(void *upt, Float arg) {return(0);}
void *free_ptree(void *upt) {return(NULL);}
XEN ptree_code(void *p) {return(XEN_FALSE);}
Float evaluate_ptreec(void *upt, Float arg, vct *v, bool dir) {return(0.0);}
#endif
/* end with_run && have_guile and so on */


#if 0
/*
;;; -------- vct-map

 (define (vct-map . args)
  "(vct-map thunk v0 ...) calls 'thunk' within vct-map! applying the frame result to the vcts passed. \
This is intended for use with locsig in multi-channel situations where you want the optimization that \
vct-map! provides."
  (let* ((func (car args))
	 (vcts (cdr args))
	 (len (length vcts))
	 (vect (make-vector len)))
    (let ((i 0))
      (for-each
       (lambda (v)
	 (vector-set! vect i v)
	 (set! i (1+ i)))
       vcts))
    (vct-map! 
     (vector-ref vect 0)
     (let ((ctr 0))
       (lambda ()
	 (let* ((fr (func))
		(chans (mus-channels fr)))
	   (do ((i 1 (1+ i)))
	       ((= i chans))
	     (vct-set! (vector-ref vect i) ctr (frame-ref fr i)))
	   (set! ctr (1+ ctr))
	   (frame-ref fr 0)))))))
	   */
#endif
/* that Scheme code would have worked except I don't currently optimize "(func)",
   so, we need (at least for now), a C version:
*/

static XEN g_vct_map(XEN proc_and_code, XEN arglist)
{
  #define H_vct_map "(" S_vct_map " thunk v ...): call 'thunk' which should return a frame; the frame result \
is then parcelled out to the vcts passed as the trailing arguments.  This is intended for use with locsig \
in multi-channel situations where you want the optimization that vct-map! provides."
  int i, j, len, min_len = 0;
  vct **vs = NULL;
  XEN proc, obj, code;
  Float *vals;
  mus_any *f;
  if (XEN_LIST_P(proc_and_code))
    {
      proc = XEN_CAR(proc_and_code);
      code = XEN_CADR(proc_and_code);
    }
  else
    {
      proc = proc_and_code;
      code = proc_and_code;
    }
  XEN_ASSERT_TYPE(XEN_PROCEDURE_P(code) && (XEN_REQUIRED_ARGS_OK(code, 0)), code, XEN_ARG_1, S_vct_map, "a thunk");
  len = XEN_LIST_LENGTH(arglist);
  if (len == 0)
    mus_misc_error(S_vct_map, "no vcts passed to vct-map", arglist);
  vs = (vct **)CALLOC(len, sizeof(vct *));
  for (i = 0; i < len; i++, arglist = XEN_CDR(arglist))
    {
      obj = XEN_CAR(arglist);
      XEN_ASSERT_TYPE(VCT_P(obj), obj, i + 2, S_vct_map, "a vct");
      vs[i] = TO_VCT(obj);
      if (min_len == 0)
	min_len = vs[i]->length;
      else
	{
	  if (min_len > vs[i]->length)
	    min_len = vs[i]->length;
	}
    }

#if WITH_RUN
  if (optimization(ss) != DONT_OPTIMIZE)
    {
      ptree *pt = NULL;
      pt = (ptree *)form_to_ptree(proc_and_code);
      if (pt)
	{
	  for (i = 0; i < min_len; i++) 
	    {
	      eval_ptree(pt);
	      vals = mus_data(pt->clms[pt->result->addr]);
	      for (j = 0; j < len; j++)
		vs[j]->data[i] = vals[j];
	    }
	  if (pt) free_ptree(pt);
	  if (vs) FREE(vs);
	  return(proc);
	}
      /* else fall through to guile */
      if (pt) free_ptree(pt);
    }
#endif

  for (i = 0; i < min_len; i++) 
    {
      obj = XEN_CALL_0_NO_CATCH(code, S_vct_map);
      if (mus_xen_p(obj))
	{
	  f = XEN_TO_MUS_ANY(obj);
	  if (mus_frame_p(f))
	    {
	      vals = mus_data(f);
	      if (vals)
		for (j = 0; j < len; j++)
		  vs[j]->data[i] = vals[j];
	    }
	}
    }
  if (vs) FREE(vs);
  return(proc);
}

static XEN g_optimization(void) {return(C_TO_XEN_INT(optimization(ss)));}
static XEN g_set_optimization(XEN val) 
{
  #define H_optimization "(" S_optimization "): the current 'run' optimization level (default 0 = off, max is 6)"
  XEN_ASSERT_TYPE(XEN_INTEGER_P(val), val, XEN_ONLY_ARG, "set! " S_optimization, "an integer");
  set_optimization(XEN_TO_C_INT(val));
  return(C_TO_XEN_INT(optimization(ss)));
}

static XEN g_run_safety(void) {return(C_TO_XEN_INT(run_safety));}
static XEN g_set_run_safety(XEN val) 
{
  #define H_run_safety "(" S_run_safety "): the current 'run' safety level (default 0 = off)"
  XEN_ASSERT_TYPE(XEN_INTEGER_P(val), val, XEN_ONLY_ARG, "set! " S_run_safety, "an integer");
  run_safety = XEN_TO_C_INT(val);
  if ((run_safety != RUN_SAFE) && (run_safety != RUN_UNSAFE))
    XEN_OUT_OF_RANGE_ERROR(S_setB S_run_safety, XEN_ONLY_ARG, val, "must be 0 (no checks) or 1 (with checks)");
  return(C_TO_XEN_INT(run_safety));
}

#if WITH_RUN
static XEN g_run(XEN proc_and_code)
{
  #define H_run "(" S_run " thunk): try to optimize the procedure passed as its argument, \
then evaluate it; if the optimizer can't handle something in the procedure, it is passed \
to Guile and is equivalent to (thunk)."

  XEN code, result = XEN_FALSE;
  ptree *pt = NULL;
  code = XEN_CADR(proc_and_code);
  XEN_ASSERT_TYPE(XEN_PROCEDURE_P(code) && (XEN_REQUIRED_ARGS_OK(code, 0)), code, XEN_ONLY_ARG, S_run, "a thunk");
  pt = (ptree *)form_to_ptree(proc_and_code);
  if (pt)
    {
      eval_ptree(pt);
      result = eval_ptree_to_xen(pt);
      free_ptree(pt);
    }
  else result = XEN_CALL_0(code, S_run);
  return(result);
}
#endif

#ifdef XEN_ARGIFY_1
XEN_NARGIFY_0(g_optimization_w, g_optimization)
XEN_NARGIFY_1(g_set_optimization_w, g_set_optimization)
XEN_NARGIFY_0(g_run_safety_w, g_run_safety)
XEN_NARGIFY_1(g_set_run_safety_w, g_set_run_safety)
XEN_NARGIFY_2(g_vct_map_w, g_vct_map)
#else
#define g_optimization_w g_optimization
#define g_set_optimization_w g_set_optimization
#define g_run_safety_w g_run_safety
#define g_set_run_safety_w g_set_run_safety
#define g_vct_map_w g_vct_map
#endif

#if HAVE_GUILE && WITH_RUN

/* make_walker: walker, special walker, set walker, req args, max args, result type, need int, num arg types, ... */
#if WITH_PROCPROP
#define INIT_WALKER(Name, Val) \
  XEN_SET_OBJECT_PROPERTY(C_STRING_TO_XEN_SYMBOL(Name), walk_sym, C_TO_XEN_ULONG(Val)); \
  XEN_SET_PROCEDURE_PROPERTY(XEN_VARIABLE_REF(XEN_NAME_AS_C_STRING_TO_VARIABLE(Name)), walk_sym, C_TO_XEN_ULONG(Val))
#else
#define INIT_WALKER(Name, Val) XEN_SET_OBJECT_PROPERTY(C_STRING_TO_XEN_SYMBOL(Name), walk_sym, C_TO_XEN_ULONG(Val))
#endif

static void init_walkers(void)
{
  XEN declare;
#if (!HAVE_GUILE_CALL_CC)
  XEN call_cc;
  XEN_DEFINE_VARIABLE("call/cc", call_cc, XEN_FALSE);
#endif
  XEN_DEFINE_VARIABLE("declare", declare, XEN_FALSE);
  walk_sym = C_STRING_TO_XEN_SYMBOL("snd-walk");
  XEN_PROTECT_FROM_GC(walk_sym);

  /* -------- special forms */
  INIT_WALKER("let", make_walker(NULL, let_form, NULL, 2, UNLIMITED_ARGS, R_ANY, false, 0));
  INIT_WALKER("let*", make_walker(NULL, let_star_form, NULL, 2, UNLIMITED_ARGS, R_ANY, false, 0));
  INIT_WALKER("do", make_walker(NULL, do_form, NULL, 2, UNLIMITED_ARGS, R_ANY, false, 0));
  INIT_WALKER("begin", make_walker(NULL, begin_form, NULL, 0, UNLIMITED_ARGS, R_ANY, false, 0));
  INIT_WALKER("if", make_walker(NULL, if_form, NULL, 2, 3, R_ANY, false, 0));
  INIT_WALKER("cond", make_walker(NULL, cond_form, NULL, 1, UNLIMITED_ARGS, R_ANY, false, 0));
  INIT_WALKER("case", make_walker(NULL, case_form, NULL, 2, UNLIMITED_ARGS, R_ANY, false, 0));
  INIT_WALKER("call-with-current-continuation", make_walker(NULL, callcc_form, NULL, 1, 1, R_ANY, false, 0));
  INIT_WALKER("or", make_walker(NULL, or_form, NULL, 0, UNLIMITED_ARGS, R_ANY, false, 0));
  INIT_WALKER("and", make_walker(NULL, and_form, NULL, 0, UNLIMITED_ARGS, R_ANY, false, 0));
  INIT_WALKER("set!", make_walker(NULL, set_form, NULL, 2, 2, R_ANY, false, 0)); /* Scheme set! does not take &rest args */
  INIT_WALKER("call/cc", make_walker(NULL, callcc_form, NULL, 1, 1, R_ANY, false, 0));
  INIT_WALKER("lambda", make_walker(NULL, lambda_preform, NULL, 1, UNLIMITED_ARGS, R_ANY, false, 0));
  INIT_WALKER("quote", make_walker(NULL, quote_form, NULL, 1, 1, R_ANY, false, 0));
  INIT_WALKER("declare", make_walker(NULL, declare_1, NULL, 0, UNLIMITED_ARGS, R_ANY, false, 0));

  /* -------- basic stuff */
  INIT_WALKER("eq?", make_walker(eq_p, NULL, NULL, 2, 2, R_BOOL, false, 0));
  INIT_WALKER("eqv?", make_walker(eqv_p, NULL, NULL, 2, 2, R_BOOL, false, 0));
  INIT_WALKER("equal?", make_walker(equal_p, NULL, NULL, 2, 2, R_BOOL, false, 0));
  INIT_WALKER("boolean?", make_walker(boolean_p_1, NULL, NULL, 1, 1, R_BOOL, false, 0));
  INIT_WALKER("number?", make_walker(number_p_1, NULL, NULL, 1, 1, R_BOOL, false, 0));
  INIT_WALKER("integer?", make_walker(integer_p_1, NULL, NULL, 1, 1, R_BOOL, false, 0));
  INIT_WALKER("inexact?", make_walker(inexact_p_1, NULL, NULL, 1, 1, R_BOOL, false, 0));
  INIT_WALKER("exact?", make_walker(exact_p_1, NULL, NULL, 1, 1, R_BOOL, false, 0));
  INIT_WALKER("real?", make_walker(real_p_1, NULL, NULL, 1, 1, R_BOOL, false, 0));
  INIT_WALKER("char?", make_walker(char_p_1, NULL, NULL, 1, 1, R_BOOL, false, 0));
  INIT_WALKER("string?", make_walker(string_p_1, NULL, NULL, 1, 1, R_BOOL, false, 0));
  INIT_WALKER("keyword?", make_walker(keyword_p_1, NULL, NULL, 1, 1, R_BOOL, false, 0));
  INIT_WALKER("symbol?", make_walker(symbol_p_1, NULL, NULL, 1, 1, R_BOOL, false, 0));
  INIT_WALKER("vector?", make_walker(vector_p_1, NULL, NULL, 1, 1, R_BOOL, false, 0));
  INIT_WALKER("list?", make_walker(list_p_1, NULL, NULL, 1, 1, R_BOOL, false, 0));
  INIT_WALKER("pair?", make_walker(pair_p_1, NULL, NULL, 1, 1, R_BOOL, false, 0));
  INIT_WALKER(">=", make_walker(ge_1, NULL, NULL, 0, UNLIMITED_ARGS, R_BOOL, false, 1, -R_NUMBER));
  INIT_WALKER(">", make_walker(gt_1, NULL, NULL, 0, UNLIMITED_ARGS, R_BOOL, false, 1, -R_NUMBER));
  INIT_WALKER("<=", make_walker(le_1, NULL, NULL, 0, UNLIMITED_ARGS, R_BOOL, false, 1, -R_NUMBER));
  INIT_WALKER("<", make_walker(lt_1, NULL, NULL, 0, UNLIMITED_ARGS, R_BOOL, false, 1, -R_NUMBER));
  INIT_WALKER("=", make_walker(eq_1, NULL, NULL, 0, UNLIMITED_ARGS, R_BOOL, false, 1, -R_NUMBER));

  INIT_WALKER("*", make_walker(multiply, NULL, NULL, 0, UNLIMITED_ARGS, R_ANY, false, 1, -R_NUMBER));
  INIT_WALKER("+", make_walker(add, NULL, NULL, 0, UNLIMITED_ARGS, R_NUMBER, false, 1, -R_NUMBER));
  INIT_WALKER("-", make_walker(subtract, NULL, NULL, 1, UNLIMITED_ARGS, R_NUMBER, false, 1, -R_NUMBER));
  INIT_WALKER("/", make_walker(divide, NULL, NULL, 1, UNLIMITED_ARGS, R_NUMBER, false, 1, -R_NUMBER));
  INIT_WALKER("max", make_walker(max_1, NULL, NULL, 1, UNLIMITED_ARGS, R_NUMBER, false, 1, -R_NUMBER));
  INIT_WALKER("min", make_walker(min_1, NULL, NULL, 1, UNLIMITED_ARGS, R_NUMBER, false, 1, -R_NUMBER));
  INIT_WALKER("1+", make_walker(one_plus, NULL, NULL, 1, 1, R_NUMBER, false, 1, R_NUMBER));
  INIT_WALKER("1-", make_walker(one_minus, NULL, NULL, 1, 1, R_NUMBER, false, 1, R_NUMBER));

  INIT_WALKER("inexact->exact", make_walker(inexact2exact_1, NULL, NULL, 1, 1, R_INT, true, 1, R_NUMBER));
  INIT_WALKER("exact->inexact", make_walker(exact2inexact_1, NULL, NULL, 1, 1, R_FLOAT, false, 1, R_NUMBER));
  INIT_WALKER("modulo", make_walker(modulo_1, NULL, NULL, 2, 2, R_INT, true, 2, R_NUMBER, R_NUMBER));
  INIT_WALKER("remainder", make_walker(remainder_1, NULL, NULL, 2, 2, R_INT, true, 2, R_NUMBER, R_NUMBER));
  INIT_WALKER("quotient", make_walker(quotient_1, NULL, NULL, 2, 2, R_INT, true, 2, R_NUMBER, R_NUMBER));
  INIT_WALKER("logand", make_walker(logand_1, NULL, NULL, 2, 2, R_INT, true, 2, R_INT, R_INT));
  INIT_WALKER("logxor", make_walker(logxor_1, NULL, NULL, 2, 2, R_INT, true, 2, R_INT, R_INT));
  INIT_WALKER("logior", make_walker(logior_1, NULL, NULL, 2, 2, R_INT, true, 2, R_INT, R_INT));
  INIT_WALKER("lognot", make_walker(lognot_1, NULL, NULL, 1, 1, R_INT, true, 1, R_INT));
  INIT_WALKER("ash", make_walker(ash_1, NULL, NULL, 2, 2, R_INT, true, 2, R_INT, R_INT));
  INIT_WALKER("integer->char", make_walker(integer_to_char_1, NULL, NULL, 1, 1, R_CHAR, true, 1, R_INT));
  INIT_WALKER("gcd", make_walker(gcd_1, NULL, NULL, 0, UNLIMITED_ARGS, R_INT, true, 1, -R_NUMBER));
  INIT_WALKER("lcm", make_walker(lcm_1, NULL, NULL, 0, UNLIMITED_ARGS, R_INT, true, 1, -R_NUMBER));
  INIT_WALKER("expt", make_walker(expt_1, NULL, NULL, 2, 2, R_NUMBER, false, 2, R_NUMBER, R_NUMBER));
  INIT_WALKER("atan", make_walker(atan_1, NULL, NULL, 1, 2, R_NUMBER, false, 2, R_NUMBER, R_NUMBER));
  INIT_WALKER("sin", make_walker(sin_1, NULL, NULL, 1, 1, R_FLOAT, false, 1, R_NUMBER));
  INIT_WALKER("cos", make_walker(cos_1, NULL, NULL, 1, 1, R_FLOAT, false, 1, R_NUMBER));
  INIT_WALKER("tan", make_walker(tan_1, NULL, NULL, 1, 1, R_FLOAT, false, 1, R_NUMBER));
  INIT_WALKER("abs", make_walker(abs_1, NULL, NULL, 1, 1, R_NUMBER, false, 1, R_NUMBER));
  INIT_WALKER("random", make_walker(random_1, NULL, NULL, 1, 1, R_NUMBER, false, 1, R_NUMBER));
  INIT_WALKER("log", make_walker(log_1, NULL, NULL, 1, 1, R_FLOAT, false, 1, R_NUMBER));
  INIT_WALKER("exp", make_walker(exp_1, NULL, NULL, 1, 1, R_FLOAT, false, 1, R_NUMBER));
  INIT_WALKER("asin", make_walker(asin_1, NULL, NULL, 1, 1, R_FLOAT, false, 1, R_NUMBER));
  INIT_WALKER("acos", make_walker(acos_1, NULL, NULL, 1, 1, R_FLOAT, false, 1, R_NUMBER));
  INIT_WALKER("sqrt", make_walker(sqrt_1, NULL, NULL, 1, 1, R_FLOAT, false, 1, R_NUMBER));
  INIT_WALKER("round", make_walker(round_1, NULL, NULL, 1, 1, R_FLOAT, false, 1, R_NUMBER));
  INIT_WALKER("truncate", make_walker(truncate_1, NULL, NULL, 1, 1, R_FLOAT, false, 1, R_NUMBER));
  INIT_WALKER("floor", make_walker(floor_1, NULL, NULL, 1, 1, R_FLOAT, false, 1, R_NUMBER));
  INIT_WALKER("ceiling", make_walker(ceiling_1, NULL, NULL, 1, 1, R_FLOAT, false, 1, R_NUMBER));
  INIT_WALKER("odd?", make_walker(odd_p, NULL, NULL, 1, 1, R_BOOL, false, 1, R_NUMBER));
  INIT_WALKER("even?", make_walker(even_p, NULL, NULL, 1, 1, R_BOOL, false, 1, R_NUMBER));
  INIT_WALKER("zero?", make_walker(zero_p, NULL, NULL, 1, 1, R_BOOL, false, 1, R_NUMBER));
  INIT_WALKER("positive?", make_walker(positive_p, NULL, NULL, 1, 1, R_BOOL, false, 1, R_NUMBER));
  INIT_WALKER("negative?", make_walker(negative_p, NULL, NULL, 1, 1, R_BOOL, false, 1, R_NUMBER));
  INIT_WALKER("not", make_walker(not_p, NULL, NULL, 1, 1, R_BOOL, false, 0)); /* ?? */

  INIT_WALKER("throw", make_walker(throw_1, NULL, NULL, 1, 1, R_BOOL, false, 1, R_SYMBOL));

  /* -------- char funcs */
  INIT_WALKER("char-alphabetic?", make_walker(char_alphabetic_p, NULL, NULL, 1, 1, R_BOOL, false, 1, R_CHAR));
  INIT_WALKER("char-numeric?", make_walker(char_numeric_p, NULL, NULL, 1, 1, R_BOOL, false, 1, R_CHAR));
  INIT_WALKER("char-lower-case?", make_walker(char_lower_case_p, NULL, NULL, 1, 1, R_BOOL, false, 1, R_CHAR));
  INIT_WALKER("char-upper-case?", make_walker(char_upper_case_p, NULL, NULL, 1, 1, R_BOOL, false, 1, R_CHAR));
  INIT_WALKER("char-whitespace?", make_walker(char_whitespace_p, NULL, NULL, 1, 1, R_BOOL, false, 1, R_CHAR));
  INIT_WALKER("char-upcase", make_walker(char_upcase, NULL, NULL, 1, 1, R_CHAR, false, 1, R_CHAR));
  INIT_WALKER("char-downcase", make_walker(char_downcase, NULL, NULL, 1, 1, R_CHAR, false, 1, R_CHAR));
  INIT_WALKER("char->integer", make_walker(char_to_integer, NULL, NULL, 1, 1, R_INT, false, 1, R_CHAR));
  INIT_WALKER("char>=?", make_walker(char_ge_1, NULL, NULL, 0, UNLIMITED_ARGS, R_BOOL, false, 1, -R_CHAR));
  INIT_WALKER("char>?", make_walker(char_gt_1, NULL, NULL, 0, UNLIMITED_ARGS, R_BOOL, false, 1, -R_CHAR));
  INIT_WALKER("char<=?", make_walker(char_le_1, NULL, NULL, 0, UNLIMITED_ARGS, R_BOOL, false, 1, -R_CHAR));
  INIT_WALKER("char<?", make_walker(char_lt_1, NULL, NULL, 0, UNLIMITED_ARGS, R_BOOL, false, 1, -R_CHAR));
  INIT_WALKER("char=?", make_walker(char_eq_1, NULL, NULL, 0, UNLIMITED_ARGS, R_BOOL, false, 1, -R_CHAR));
  INIT_WALKER("char-ci>=?", make_walker(char_ci_ge_1, NULL, NULL, 0, UNLIMITED_ARGS, R_BOOL, false, 1, -R_CHAR));
  INIT_WALKER("char-ci>?", make_walker(char_ci_gt_1, NULL, NULL, 0, UNLIMITED_ARGS, R_BOOL, false, 1, -R_CHAR));
  INIT_WALKER("char-ci<=?", make_walker(char_ci_le_1, NULL, NULL, 0, UNLIMITED_ARGS, R_BOOL, false, 1, -R_CHAR));
  INIT_WALKER("char-ci<?", make_walker(char_ci_lt_1, NULL, NULL, 0, UNLIMITED_ARGS, R_BOOL, false, 1, -R_CHAR));
  INIT_WALKER("char-ci=?", make_walker(char_ci_eq_1, NULL, NULL, 0, UNLIMITED_ARGS, R_BOOL, false, 1, -R_CHAR));
  INIT_WALKER("string", make_walker(string_1, NULL, NULL, 0, UNLIMITED_ARGS, R_STRING, false, 1, -R_CHAR));

  /* -------- string funcs */
  INIT_WALKER("string=?", make_walker(string_eq_1, NULL, NULL, 0, UNLIMITED_ARGS, R_BOOL, false, 1, -R_STRING));
  INIT_WALKER("string>=?", make_walker(string_geq_1, NULL, NULL, 0, UNLIMITED_ARGS, R_BOOL, false, 1, -R_STRING));
  INIT_WALKER("string<=?", make_walker(string_leq_1, NULL, NULL, 0, UNLIMITED_ARGS, R_BOOL, false, 1, -R_STRING));
  INIT_WALKER("string>?", make_walker(string_gt_1, NULL, NULL, 0, UNLIMITED_ARGS, R_BOOL, false, 1, -R_STRING));
  INIT_WALKER("string<?", make_walker(string_lt_1, NULL, NULL, 0, UNLIMITED_ARGS, R_BOOL, false, 1, -R_STRING));
  INIT_WALKER("string-ci=?", make_walker(string_ci_eq_1, NULL, NULL, 0, UNLIMITED_ARGS, R_BOOL, false, 1, -R_STRING));
  INIT_WALKER("string-ci>=?", make_walker(string_ci_geq_1, NULL, NULL, 0, UNLIMITED_ARGS, R_BOOL, false, 1, -R_STRING));
  INIT_WALKER("string-ci<=?", make_walker(string_ci_leq_1, NULL, NULL, 0, UNLIMITED_ARGS, R_BOOL, false, 1, -R_STRING));
  INIT_WALKER("string-ci>?", make_walker(string_ci_gt_1, NULL, NULL, 0, UNLIMITED_ARGS, R_BOOL, false, 1, -R_STRING));
  INIT_WALKER("string-ci<?", make_walker(string_ci_lt_1, NULL, NULL, 0, UNLIMITED_ARGS, R_BOOL, false, 1, -R_STRING));
  INIT_WALKER("string-length", make_walker(string_length_1, NULL, NULL, 1, 1, R_INT, false, 1, R_STRING));
  INIT_WALKER("string-copy", make_walker(string_copy_1, NULL, NULL, 1, 1, R_STRING, false, 1, R_STRING));
  INIT_WALKER("string-ref", make_walker(string_ref_1, NULL, NULL, 2, 2, R_CHAR, false, 2, R_STRING, R_INT));
  INIT_WALKER("substring", make_walker(substring_1, NULL, NULL, 3, 3, R_STRING, false, 3, R_STRING, R_INT, R_INT));
  INIT_WALKER("string-fill!", make_walker(string_fill_1, NULL, NULL, 2, 2, R_STRING, false, 2, R_STRING, R_CHAR));
  INIT_WALKER("string-set!", make_walker(string_set_1, NULL, NULL, 3, 3, R_STRING, false, 3, R_STRING, R_INT, R_CHAR));
  INIT_WALKER("string-append", make_walker(string_append_1, NULL, NULL, 0, UNLIMITED_ARGS, R_STRING, false, 1, -R_STRING));

  INIT_WALKER("display", make_walker(display_1, NULL, NULL, 1, 1, R_STRING, false, 0));
  INIT_WALKER("make-string", make_walker(make_string_1, NULL, NULL, 1, 2, R_STRING, false, 2, R_INT, R_CHAR));
  INIT_WALKER("number->string", make_walker(number2string_1, NULL, NULL, 1, 2, R_STRING, false, 2, R_NUMBER, R_INT));
  INIT_WALKER("symbol->string", make_walker(symbol2string_1, NULL, NULL, 1, 1, R_STRING, false, 1, R_SYMBOL));

  /* -------- vector funcs */
  INIT_WALKER("vector-ref", make_walker(vector_ref_1, NULL, NULL, 2, 2, R_ANY, false, 2, R_VECTOR, R_INT));
  INIT_WALKER("vector-length", make_walker(vector_length_1, NULL, NULL, 1, 1, R_INT, false, 1, R_VECTOR));
  INIT_WALKER("vector-fill!", make_walker(vector_fill_1, NULL, NULL, 2, 2, R_INT, false, 1, R_VECTOR));
  INIT_WALKER("vector-set!", make_walker(vector_set_1, NULL, NULL, 3, 3, R_ANY, false, 2, R_VECTOR, R_INT));
  INIT_WALKER("make-vector", make_walker(make_vector_1, NULL, NULL, 1, 2, R_FLOAT_VECTOR, false, 2, R_INT, R_FLOAT));

  /* -------- list funcs */
  INIT_WALKER("car", make_walker(car_1, NULL, NULL, 1, 1, R_ANY, false, 1, R_CONS));
  INIT_WALKER("cdr", make_walker(cdr_1, NULL, NULL, 1, 1, R_ANY, false, 1, R_PAIR));
  INIT_WALKER("caar", make_walker(caar_1, NULL, NULL, 1, 1, R_ANY, false, 1, R_LIST));
  INIT_WALKER("cadr", make_walker(cadr_1, NULL, NULL, 1, 1, R_ANY, false, 1, R_LIST));
  INIT_WALKER("caaar", make_walker(caaar_1, NULL, NULL, 1, 1, R_ANY, false, 1, R_LIST));
  INIT_WALKER("caadr", make_walker(caadr_1, NULL, NULL, 1, 1, R_ANY, false, 1, R_LIST));
  INIT_WALKER("cadar", make_walker(cadar_1, NULL, NULL, 1, 1, R_ANY, false, 1, R_LIST));
  INIT_WALKER("caddr", make_walker(caddr_1, NULL, NULL, 1, 1, R_ANY, false, 1, R_LIST));
  INIT_WALKER("caaaar", make_walker(caaaar_1, NULL, NULL, 1, 1, R_ANY, false, 1, R_LIST));
  INIT_WALKER("caaadr", make_walker(caaadr_1, NULL, NULL, 1, 1, R_ANY, false, 1, R_LIST));
  INIT_WALKER("caadar", make_walker(caadar_1, NULL, NULL, 1, 1, R_ANY, false, 1, R_LIST));
  INIT_WALKER("caaddr", make_walker(caaddr_1, NULL, NULL, 1, 1, R_ANY, false, 1, R_LIST));
  INIT_WALKER("cadaar", make_walker(cadaar_1, NULL, NULL, 1, 1, R_ANY, false, 1, R_LIST));
  INIT_WALKER("cadadr", make_walker(cadadr_1, NULL, NULL, 1, 1, R_ANY, false, 1, R_LIST));
  INIT_WALKER("caddar", make_walker(caddar_1, NULL, NULL, 1, 1, R_ANY, false, 1, R_LIST));
  INIT_WALKER("cadddr", make_walker(cadddr_1, NULL, NULL, 1, 1, R_ANY, false, 1, R_LIST));
  INIT_WALKER("null?", make_walker(null_p_1, NULL, NULL, 1, 1, R_BOOL, false, 1, R_LIST));
  INIT_WALKER("length", make_walker(list_length_1, NULL, NULL, 1, 1, R_INT, false, 1, R_LIST));


  /* -------- clm funcs */
  INIT_WALKER(S_buffer_to_sample, make_walker(mus_buffer_to_sample_0, NULL, NULL, 1, 1, R_BOOL, false, 1, R_CLM));
  INIT_WALKER(S_oscil_p, make_walker(oscil_p, NULL, NULL, 1, 1, R_BOOL, false, 1, R_CLM));
  INIT_WALKER(S_env_p, make_walker(env_p, NULL, NULL, 1, 1, R_BOOL, false, 1, R_CLM));
  INIT_WALKER(S_notch_p, make_walker(notch_p, NULL, NULL, 1, 1, R_BOOL, false, 1, R_CLM));
  INIT_WALKER(S_comb_p, make_walker(comb_p, NULL, NULL, 1, 1, R_BOOL, false, 1, R_CLM));
  INIT_WALKER(S_delay_p, make_walker(delay_p, NULL, NULL, 1, 1, R_BOOL, false, 1, R_CLM));
  INIT_WALKER(S_all_pass_p, make_walker(all_pass_p, NULL, NULL, 1, 1, R_BOOL, false, 1, R_CLM));
  INIT_WALKER(S_average_p, make_walker(average_p, NULL, NULL, 1, 1, R_BOOL, false, 1, R_CLM));
  INIT_WALKER(S_rand_p, make_walker(rand_p, NULL, NULL, 1, 1, R_BOOL, false, 1, R_CLM));
  INIT_WALKER(S_rand_interp_p, make_walker(rand_interp_p, NULL, NULL, 1, 1, R_BOOL, false, 1, R_CLM));
  INIT_WALKER(S_sum_of_cosines_p, make_walker(sum_of_cosines_p, NULL, NULL, 1, 1, R_BOOL, false, 1, R_CLM));
  INIT_WALKER(S_ssb_am_p, make_walker(ssb_am_p, NULL, NULL, 1, 1, R_BOOL, false, 1, R_CLM));
  INIT_WALKER(S_sum_of_sines_p, make_walker(sum_of_sines_p, NULL, NULL, 1, 1, R_BOOL, false, 1, R_CLM));
  INIT_WALKER(S_table_lookup_p, make_walker(table_lookup_p, NULL, NULL, 1, 1, R_BOOL, false, 1, R_CLM));
  INIT_WALKER(S_sawtooth_wave_p, make_walker(sawtooth_wave_p, NULL, NULL, 1, 1, R_BOOL, false, 1, R_CLM));
  INIT_WALKER(S_pulse_train_p, make_walker(pulse_train_p, NULL, NULL, 1, 1, R_BOOL, false, 1, R_CLM));
  INIT_WALKER(S_square_wave_p, make_walker(square_wave_p, NULL, NULL, 1, 1, R_BOOL, false, 1, R_CLM));
  INIT_WALKER(S_triangle_wave_p, make_walker(triangle_wave_p, NULL, NULL, 1, 1, R_BOOL, false, 1, R_CLM));
  INIT_WALKER(S_asymmetric_fm_p, make_walker(asymmetric_fm_p, NULL, NULL, 1, 1, R_BOOL, false, 1, R_CLM));
  INIT_WALKER(S_sine_summation_p, make_walker(sine_summation_p, NULL, NULL, 1, 1, R_BOOL, false, 1, R_CLM));
  INIT_WALKER(S_one_zero_p, make_walker(one_zero_p, NULL, NULL, 1, 1, R_BOOL, false, 1, R_CLM));
  INIT_WALKER(S_one_pole_p, make_walker(one_pole_p, NULL, NULL, 1, 1, R_BOOL, false, 1, R_CLM));
  INIT_WALKER(S_two_zero_p, make_walker(two_zero_p, NULL, NULL, 1, 1, R_BOOL, false, 1, R_CLM));
  INIT_WALKER(S_two_pole_p, make_walker(two_pole_p, NULL, NULL, 1, 1, R_BOOL, false, 1, R_CLM));
  INIT_WALKER(S_formant_p, make_walker(formant_p, NULL, NULL, 1, 1, R_BOOL, false, 1, R_CLM));
  INIT_WALKER(S_wave_train_p, make_walker(wave_train_p, NULL, NULL, 1, 1, R_BOOL, false, 1, R_CLM));
  INIT_WALKER(S_waveshape_p, make_walker(waveshape_p, NULL, NULL, 1, 1, R_BOOL, false, 1, R_CLM));
  INIT_WALKER(S_filter_p, make_walker(filter_p, NULL, NULL, 1, 1, R_BOOL, false, 1, R_CLM));
  INIT_WALKER(S_fir_filter_p, make_walker(fir_filter_p, NULL, NULL, 1, 1, R_BOOL, false, 1, R_CLM));
  INIT_WALKER(S_iir_filter_p, make_walker(iir_filter_p, NULL, NULL, 1, 1, R_BOOL, false, 1, R_CLM));
  INIT_WALKER(S_readin_p, make_walker(readin_p, NULL, NULL, 1, 1, R_BOOL, false, 1, R_CLM));
  INIT_WALKER(S_src_p, make_walker(src_p, NULL, NULL, 1, 1, R_BOOL, false, 1, R_CLM));
  INIT_WALKER(S_granulate_p, make_walker(granulate_p, NULL, NULL, 1, 1, R_BOOL, false, 1, R_CLM));
  INIT_WALKER(S_phase_vocoder_p, make_walker(phase_vocoder_p, NULL, NULL, 1, 1, R_BOOL, false, 1, R_CLM));
  INIT_WALKER(S_convolve_p, make_walker(convolve_p, NULL, NULL, 1, 1, R_BOOL, false, 1, R_CLM));
  INIT_WALKER(S_buffer_p, make_walker(buffer_p, NULL, NULL, 1, 1, R_BOOL, false, 1, R_CLM));
  INIT_WALKER(S_buffer_empty_p, make_walker(buffer_empty_p, NULL, NULL, 1, 1, R_BOOL, false, 1, R_CLM));
  INIT_WALKER(S_buffer_full_p, make_walker(buffer_full_p, NULL, NULL, 1, 1, R_BOOL, false, 1, R_CLM));
  INIT_WALKER(S_frame_p, make_walker(frame_p, NULL, NULL, 1, 1, R_BOOL, false, 1, R_CLM));
  INIT_WALKER(S_mixer_p, make_walker(mixer_p, NULL, NULL, 1, 1, R_BOOL, false, 1, R_CLM));
  INIT_WALKER(S_file_to_sample_p, make_walker(file_to_sample_p, NULL, NULL, 1, 1, R_BOOL, false, 1, R_CLM));
  INIT_WALKER(S_sample_to_file_p, make_walker(sample_to_file_p, NULL, NULL, 1, 1, R_BOOL, false, 1, R_CLM));
  INIT_WALKER(S_file_to_frame_p, make_walker(file_to_frame_p, NULL, NULL, 1, 1, R_BOOL, false, 1, R_CLM));
  INIT_WALKER(S_frame_to_file_p, make_walker(frame_to_file_p, NULL, NULL, 1, 1, R_BOOL, false, 1, R_CLM));
  INIT_WALKER(S_locsig_p, make_walker(locsig_p, NULL, NULL, 1, 1, R_BOOL, false, 1, R_CLM));
  INIT_WALKER(S_mus_input_p, make_walker(input_p, NULL, NULL, 1, 1, R_BOOL, false, 1, R_CLM));
  INIT_WALKER(S_mus_output_p, make_walker(output_p, NULL, NULL, 1, 1, R_BOOL, false, 1, R_CLM));
  INIT_WALKER(S_snd_to_sample_p, make_walker(snd_to_sample_1p, NULL, NULL, 1, 1, R_BOOL, false, 1, R_CLM));

  INIT_WALKER(S_restart_env, make_walker(restart_env_1, NULL, NULL, 1, 1, R_CLM, false, 1, R_CLM));
  INIT_WALKER(S_mus_increment, make_walker(mus_increment_0, NULL, mus_set_increment_1, 1, 1, R_FLOAT, false, 1, R_CLM));
  INIT_WALKER(S_mus_frequency, make_walker(mus_frequency_0, NULL, mus_set_frequency_1, 1, 1, R_FLOAT, false, 1, R_CLM));
  INIT_WALKER(S_mus_phase, make_walker(mus_phase_0, NULL, mus_set_phase_1, 1, 1, R_FLOAT, false, 1, R_CLM));
  INIT_WALKER(S_mus_width, make_walker(mus_width_0, NULL, mus_set_width_1, 1, 1, R_FLOAT, false, 1, R_CLM));
  INIT_WALKER(S_mus_scaler, make_walker(mus_scaler_0, NULL, mus_set_scaler_1, 1, 1, R_FLOAT, false, 1, R_CLM));
  INIT_WALKER(S_mus_offset, make_walker(mus_offset_0, NULL, NULL, 1, 1, R_FLOAT, false, 1, R_CLM));
  INIT_WALKER(S_mus_formant_radius, make_walker(mus_formant_radius_0, NULL, mus_set_formant_radius_1, 1, 1, R_FLOAT, false, 1, R_CLM));
  INIT_WALKER(S_mus_data, make_walker(mus_data_1, NULL, NULL, 1, 1, R_VCT, false, 0));
  INIT_WALKER(S_mus_xcoeffs, make_walker(mus_xcoeffs_1, NULL, NULL, 1, 1, R_VCT, false, 0));
  INIT_WALKER(S_mus_ycoeffs, make_walker(mus_ycoeffs_1, NULL, NULL, 1, 1, R_VCT, false, 0));
  INIT_WALKER(S_mus_xcoeff, make_walker(mus_xcoeff_1, NULL, mus_set_xcoeff_1, 2, 2, R_FLOAT, false, 2, R_CLM, R_INT));
  INIT_WALKER(S_mus_ycoeff, make_walker(mus_ycoeff_1, NULL, mus_set_ycoeff_1, 2, 2, R_FLOAT, false, 2, R_CLM, R_INT));
  INIT_WALKER(S_mus_feedforward, make_walker(mus_feedforward_0, NULL, mus_set_feedforward_1, 1, 1, R_FLOAT, false, 1, R_CLM));
  INIT_WALKER(S_mus_feedback, make_walker(mus_feedback_0, NULL, mus_set_feedback_1, 1, 1, R_FLOAT, false, 1, R_CLM));
  INIT_WALKER(S_mus_hop, make_walker(mus_hop_0, NULL, mus_set_hop_1, 1, 1, R_INT, false, 1, R_CLM));
  INIT_WALKER(S_mus_channels, make_walker(mus_channels_0, NULL, NULL, 1, 1, R_INT, false, 1, R_CLM));
  INIT_WALKER(S_mus_channel, make_walker(mus_channel_0, NULL, NULL, 1, 1, R_INT, false, 1, R_CLM));
  INIT_WALKER(S_mus_location, make_walker(mus_location_0, NULL, mus_set_location_1, 1, 1, R_INT, false, 1, R_CLM));
  INIT_WALKER(S_mus_ramp, make_walker(mus_ramp_0, NULL, mus_set_ramp_1, 1, 1, R_INT, false, 1, R_CLM));
  INIT_WALKER(S_mus_order, make_walker(mus_order_0, NULL, NULL, 1, 1, R_INT, false, 1, R_CLM));
  INIT_WALKER(S_mus_length, make_walker(mus_length_0, NULL, mus_set_length_1, 1, 1, R_INT, false, 1, R_CLM));
  INIT_WALKER(S_mus_cosines, make_walker(mus_cosines_0, NULL, mus_set_cosines_1, 1, 1, R_INT, false, 1, R_CLM));
  INIT_WALKER(S_mus_name, make_walker(mus_name_0, NULL, NULL, 1, 1, R_INT, false, 1, R_CLM));
  INIT_WALKER(S_mus_file_name, make_walker(mus_file_name_0, NULL, NULL, 1, 1, R_STRING, false, 1, R_CLM));
  INIT_WALKER(S_mus_describe, make_walker(mus_describe_0, NULL, NULL, 1, 1, R_STRING, false, 1, R_CLM));
  INIT_WALKER(S_mus_close, make_walker(mus_close_0, NULL, NULL, 1, 1, R_INT, false, 1, R_CLM));

  INIT_WALKER(S_oscil, make_walker(oscil_1, NULL, NULL, 1, 3, R_FLOAT, false, 3, R_CLM, R_NUMBER, R_NUMBER));
  INIT_WALKER(S_one_zero, make_walker(one_zero_1, NULL, NULL, 1, 2, R_FLOAT, false, 2, R_CLM, R_NUMBER));
  INIT_WALKER(S_one_pole, make_walker(one_pole_1, NULL, NULL, 1, 2, R_FLOAT, false, 2, R_CLM, R_NUMBER));
  INIT_WALKER(S_out_any, make_walker(out_any_1, NULL, NULL, 4, 4, R_FLOAT, false, 4, R_NUMBER, R_NUMBER, R_INT, R_CLM));
  INIT_WALKER(S_outa, make_walker(outa_1, NULL, NULL, 3, 3, R_FLOAT, false, 3, R_NUMBER, R_NUMBER, R_CLM));
  INIT_WALKER(S_outb, make_walker(outb_1, NULL, NULL, 3, 3, R_FLOAT, false, 3, R_NUMBER, R_NUMBER, R_CLM));
  INIT_WALKER(S_outc, make_walker(outc_1, NULL, NULL, 3, 3, R_FLOAT, false, 3, R_NUMBER, R_NUMBER, R_CLM));
  INIT_WALKER(S_outd, make_walker(outd_1, NULL, NULL, 3, 3, R_FLOAT, false, 3, R_NUMBER, R_NUMBER, R_CLM));
  INIT_WALKER(S_env, make_walker(env_1, NULL, NULL, 1, 1, R_FLOAT, false, 1, R_CLM));
  INIT_WALKER(S_env_interp, make_walker(env_interp_1, NULL, NULL, 2, 2, R_FLOAT, false, 2, R_FLOAT, R_CLM));
  INIT_WALKER(S_notch, make_walker(notch_1, NULL, NULL, 1, 3, R_FLOAT, false, 3, R_CLM, R_NUMBER, R_NUMBER));
  INIT_WALKER(S_comb, make_walker(comb_1, NULL, NULL, 1, 3, R_FLOAT, false, 3, R_CLM, R_NUMBER, R_NUMBER));
  INIT_WALKER(S_convolve, make_walker(convolve_1, NULL, NULL, 1, 2, R_FLOAT, false, 2, R_CLM, R_FUNCTION));
  INIT_WALKER(S_delay, make_walker(delay_1, NULL, NULL, 1, 3, R_FLOAT, false, 3, R_CLM, R_NUMBER, R_NUMBER));
  INIT_WALKER(S_all_pass, make_walker(all_pass_1, NULL, NULL, 1, 3, R_FLOAT, false, 3, R_CLM, R_NUMBER, R_NUMBER));
  INIT_WALKER(S_average, make_walker(average_1, NULL, NULL, 1, 2, R_FLOAT, false, 2, R_CLM, R_NUMBER));
  INIT_WALKER(S_asymmetric_fm, make_walker(asymmetric_fm_1, NULL, NULL, 1, 3, R_FLOAT, false, 3, R_CLM, R_NUMBER, R_NUMBER));
  INIT_WALKER(S_rand, make_walker(rand_1, NULL, NULL, 1, 2, R_FLOAT, false, 2, R_CLM, R_NUMBER));
  INIT_WALKER(S_rand_interp, make_walker(rand_interp_1, NULL, NULL, 1, 2, R_FLOAT, false, 2, R_CLM, R_NUMBER));
  INIT_WALKER(S_readin, make_walker(readin_1, NULL, NULL, 1, 1, R_FLOAT, false, 1, R_CLM));
  INIT_WALKER(S_src, make_walker(src_1, NULL, NULL, 1, 3, R_FLOAT, false, 3, R_CLM, R_NUMBER, R_FUNCTION));
  INIT_WALKER(S_sum_of_cosines, make_walker(sum_of_cosines_1, NULL, NULL, 1, 2, R_FLOAT, false, 2, R_CLM, R_NUMBER));
  INIT_WALKER(S_sum_of_sines, make_walker(sum_of_sines_1, NULL, NULL, 1, 2, R_FLOAT, false, 2, R_CLM, R_NUMBER));
  INIT_WALKER(S_ssb_am, make_walker(ssb_am_1, NULL, NULL, 1, 3, R_FLOAT, false, 3, R_CLM, R_NUMBER, R_NUMBER));
  INIT_WALKER(S_sawtooth_wave, make_walker(sawtooth_wave_1, NULL, NULL, 1, 2, R_FLOAT, false, 2, R_CLM, R_NUMBER));
  INIT_WALKER(S_square_wave, make_walker(square_wave_1, NULL, NULL, 1, 2, R_FLOAT, false, 2, R_CLM, R_NUMBER));
  INIT_WALKER(S_sine_summation, make_walker(sine_summation_1, NULL, NULL, 1, 2, R_FLOAT, false, 2, R_CLM, R_NUMBER));
  INIT_WALKER(S_sample_to_file, make_walker(sample_to_file_1, NULL, NULL, 4, 4, R_FLOAT, false, 4, R_CLM, R_NUMBER, R_INT, R_NUMBER));
  INIT_WALKER(S_sample_to_buffer, make_walker(sample_to_buffer_1, NULL, NULL, 2, 2, R_FLOAT, false, 2, R_CLM, R_NUMBER));
  INIT_WALKER(S_table_lookup, make_walker(table_lookup_1, NULL, NULL, 1, 2, R_FLOAT, false, 2, R_CLM, R_NUMBER));
  INIT_WALKER(S_triangle_wave, make_walker(triangle_wave_1, NULL, NULL, 1, 2, R_FLOAT, false, 2, R_CLM, R_NUMBER));
  INIT_WALKER(S_two_zero, make_walker(two_zero_1, NULL, NULL, 1, 2, R_FLOAT, false, 2, R_CLM, R_NUMBER));
  INIT_WALKER(S_two_pole, make_walker(two_pole_1, NULL, NULL, 1, 2, R_FLOAT, false, 2, R_CLM, R_NUMBER));
  INIT_WALKER(S_tap, make_walker(tap_1, NULL, NULL, 1, 2, R_FLOAT, false, 2, R_CLM, R_NUMBER));
  INIT_WALKER(S_pulse_train, make_walker(pulse_train_1, NULL, NULL, 1, 2, R_FLOAT, false, 2, R_CLM, R_NUMBER));
  INIT_WALKER(S_phase_vocoder, make_walker(phase_vocoder_1, NULL, NULL, 1, 5, R_FLOAT, false, 5, R_CLM, R_FUNCTION, R_FUNCTION, R_FUNCTION, R_FUNCTION));

  INIT_WALKER(S_phase_vocoder_amps, make_walker(phase_vocoder_amps_1, NULL, NULL, 1, 1, R_VCT, false, 0));
  INIT_WALKER(S_phase_vocoder_amp_increments, make_walker(phase_vocoder_amp_increments_1, NULL, NULL, 1, 1, R_VCT, false, 0));
  INIT_WALKER(S_phase_vocoder_freqs, make_walker(phase_vocoder_freqs_1, NULL, NULL, 1, 1, R_VCT, false, 0));
  INIT_WALKER(S_phase_vocoder_phases, make_walker(phase_vocoder_phases_1, NULL, NULL, 1, 1, R_VCT, false, 0));
  INIT_WALKER(S_phase_vocoder_phase_increments, make_walker(phase_vocoder_phase_increments_1, NULL, NULL, 1, 1, R_VCT, false, 0));
  INIT_WALKER(S_phase_vocoder_outctr, make_walker(mus_phase_vocoder_outctr_0, NULL, NULL, 1, 1, R_INT, false, 1, R_CLM));
  INIT_WALKER(S_phase_vocoder_set_outctr, make_walker(mus_phase_vocoder_set_outctr_1, NULL, NULL, 2, 2, R_INT, false, 2, R_CLM, R_INT));

  INIT_WALKER(S_formant, make_walker(formant_1, NULL, NULL, 1, 2, R_FLOAT, false, 2, R_CLM, R_NUMBER));
  INIT_WALKER(S_filter, make_walker(filter_1, NULL, NULL, 1, 2, R_FLOAT, false, 2, R_CLM, R_NUMBER));
  INIT_WALKER(S_fir_filter, make_walker(fir_filter_1, NULL, NULL, 1, 2, R_FLOAT, false, 2, R_CLM, R_NUMBER));
  INIT_WALKER(S_file_to_sample, make_walker(file_to_sample_1, NULL, NULL, 2, 3, R_FLOAT, false, 3, R_CLM, R_NUMBER, R_INT));
  INIT_WALKER(S_snd_to_sample, make_walker(snd_to_sample_1, NULL, NULL, 2, 3, R_FLOAT, false, 3, R_CLM, R_NUMBER, R_INT));
  INIT_WALKER(S_frame_ref, make_walker(frame_ref_0, NULL, frame_set_1, 2, 2, R_FLOAT, false, 2, R_CLM, R_INT));
  INIT_WALKER(S_frame_set, make_walker(frame_set_2, NULL, NULL, 3, 3, R_FLOAT, false, 3, R_CLM, R_INT, R_NUMBER));
  INIT_WALKER(S_wave_train, make_walker(wave_train_1, NULL, NULL, 1, 2, R_FLOAT, false, 2, R_CLM, R_NUMBER));
  INIT_WALKER(S_waveshape, make_walker(waveshape_1, NULL, NULL, 1, 3, R_FLOAT, false, 3, R_CLM, R_NUMBER, R_NUMBER));
  INIT_WALKER(S_iir_filter, make_walker(iir_filter_1, NULL, NULL, 1, 2, R_FLOAT, false, 2, R_CLM, R_NUMBER));
  INIT_WALKER(S_ina, make_walker(ina_1, NULL, NULL, 2, 2, R_FLOAT, false, 2, R_NUMBER, R_CLM));
  INIT_WALKER(S_inb, make_walker(inb_1, NULL, NULL, 2, 2, R_FLOAT, false, 2, R_NUMBER, R_CLM));
  INIT_WALKER(S_in_any, make_walker(in_any_1, NULL, NULL, 3, 3, R_FLOAT, false, 2, R_NUMBER, R_INT, R_CLM));
  INIT_WALKER(S_granulate, make_walker(granulate_1, NULL, NULL, 1, 3, R_FLOAT, false, 3, R_CLM, R_FUNCTION, R_FUNCTION));
  INIT_WALKER(S_move_locsig, make_walker(move_locsig_1, NULL, NULL, 3, 3, R_FLOAT, false, 3, R_CLM, R_NUMBER, R_NUMBER));
  INIT_WALKER(S_mus_set_formant_radius_and_frequency, make_walker(set_formant_radius_and_frequency_1, NULL, NULL, 3, 3, R_FLOAT, false, 3, R_CLM, R_NUMBER, R_NUMBER));
  INIT_WALKER(S_mixer_set, make_walker(mixer_set_2, NULL, NULL, 4, 4, R_FLOAT, false, 4, R_CLM, R_INT, R_INT, R_NUMBER));
  INIT_WALKER(S_mixer_ref, make_walker(mixer_ref_1, NULL, mixer_set_1, 3, 3, R_FLOAT, false, 3, R_CLM, R_INT, R_INT));
  INIT_WALKER(S_locsig_ref, make_walker(locsig_ref_0, NULL, locsig_set_1, 2, 2, R_FLOAT, false, 2, R_CLM, R_INT));
  INIT_WALKER(S_locsig_reverb_ref, make_walker(locsig_reverb_ref_0, NULL, locsig_reverb_set_1, 2, 2, R_FLOAT, false, 2, R_CLM, R_INT));
  INIT_WALKER(S_locsig_set, make_walker(locsig_set_2, NULL, NULL, 3, 3, R_FLOAT, false, 3, R_CLM, R_INT, R_NUMBER));
  INIT_WALKER(S_locsig_reverb_set, make_walker(locsig_reverb_set_2, NULL, NULL, 3, 3, R_FLOAT, false, 3, R_CLM, R_INT, R_NUMBER));
  INIT_WALKER(S_polynomial, make_walker(polynomial_1, NULL, NULL, 2, 2, R_FLOAT, false, 2, R_VCT, R_NUMBER));
  INIT_WALKER(S_clear_array, make_walker(clear_array_1, NULL, NULL, 1, 1, R_VCT, false, 1, R_VCT));
  INIT_WALKER(S_array_interp, make_walker(array_interp_1, NULL, NULL, 2, 3, R_FLOAT, false, 3, R_VCT, R_FLOAT, R_INT));
  INIT_WALKER(S_mus_srate, make_walker(mus_srate_1, NULL, mus_set_srate_1, 0, 0, R_NUMBER, false, 0));
  INIT_WALKER(S_ring_modulate, make_walker(ring_modulate_1, NULL, NULL, 2, 2, R_FLOAT, false, 2, R_NUMBER, R_NUMBER));
  INIT_WALKER(S_amplitude_modulate, make_walker(amplitude_modulate_1, NULL, NULL, 3, 3, R_FLOAT, false, 3, R_NUMBER, R_NUMBER, R_NUMBER));
  INIT_WALKER(S_contrast_enhancement, make_walker(contrast_enhancement_1, NULL, NULL, 2, 2, R_FLOAT, false, 2, R_NUMBER, R_NUMBER));
  INIT_WALKER(S_dot_product, make_walker(dot_product_1, NULL, NULL, 2, 3, R_FLOAT, false, 3, R_VCT, R_VCT, R_INT));
  INIT_WALKER(S_sine_bank, make_walker(sine_bank_1, NULL, NULL, 2, 3, R_FLOAT, false, 3, R_VCT, R_VCT, R_INT));
  INIT_WALKER(S_polar_to_rectangular, make_walker(polar_to_rectangular_1, NULL, NULL, 2, 2, R_VCT, false, 2, R_VCT, R_VCT));
  INIT_WALKER(S_rectangular_to_polar, make_walker(rectangular_to_polar_1, NULL, NULL, 2, 2, R_VCT, false, 2, R_VCT, R_VCT));
  INIT_WALKER(S_multiply_arrays, make_walker(multiply_arrays_1, NULL, NULL, 2, 3, R_VCT, false, 3, R_VCT, R_VCT, R_INT));
  INIT_WALKER(S_mus_fft, make_walker(mus_fft_1, NULL, NULL, 2, 4, R_VCT, false, 4, R_VCT, R_VCT, R_INT, R_INT));
  INIT_WALKER(S_spectrum, make_walker(mus_spectrum_1, NULL, NULL, 2, 4, R_VCT, false, 4, R_VCT, R_VCT, R_ANY, R_INT));
  INIT_WALKER(S_convolution, make_walker(convolution_1, NULL, NULL, 2, 3, R_VCT, false, 3, R_VCT, R_VCT, R_INT));
  INIT_WALKER(S_formant_bank, make_walker(formant_bank_1,NULL, NULL, 3, 3, R_FLOAT, false, 1, R_VCT));
  INIT_WALKER(S_frame_add, make_walker(mus_frame_add_1, NULL, NULL, 2, 3, R_CLM, false, 3, R_CLM, R_CLM, R_CLM));
  INIT_WALKER(S_frame_multiply, make_walker(mus_frame_multiply_1, NULL, NULL, 2, 3, R_CLM, false, 3, R_CLM, R_CLM, R_CLM));
  INIT_WALKER(S_mixer_multiply, make_walker(mus_mixer_multiply_1, NULL, NULL, 2, 3, R_CLM, false, 3, R_CLM, R_CLM, R_CLM));
  INIT_WALKER(S_mixer_add, make_walker(mus_mixer_add_1, NULL, NULL, 2, 3, R_CLM, false, 3, R_CLM, R_CLM, R_CLM));
  INIT_WALKER(S_mixer_scale, make_walker(mixer_scale_1, NULL, NULL, 2, 3, R_CLM, false, 3, R_CLM, R_FLOAT, R_CLM)); 
  INIT_WALKER(S_frame_to_frame, make_walker(mus_frame_to_frame_1, NULL, NULL, 2, 3, R_CLM, false, 3, R_CLM, R_CLM, R_CLM));
  INIT_WALKER(S_frame_to_sample, make_walker(frame_to_sample_1, NULL, NULL, 2, 2, R_FLOAT, false, 2, R_CLM, R_CLM));
  INIT_WALKER(S_sample_to_frame, make_walker(sample_to_frame_1, NULL, NULL, 2, 3, R_FLOAT, false, 3, R_CLM, R_FLOAT, R_CLM));
  INIT_WALKER(S_locsig, make_walker(locsig_1, NULL, NULL, 3, 3, R_CLM, false, 3, R_CLM, R_NUMBER, R_NUMBER));
  INIT_WALKER(S_frame_to_buffer, make_walker(frame_to_buffer_1, NULL, NULL, 2, 2, R_CLM, false, 2, R_CLM, R_CLM));
  INIT_WALKER(S_buffer_to_frame, make_walker(buffer_to_frame_1, NULL, NULL, 1, 2, R_CLM, false, 2, R_CLM, R_CLM));
  INIT_WALKER(S_frame_to_file, make_walker(frame_to_file_1, NULL, NULL, 3, 3, R_CLM, false, 3, R_CLM, R_NUMBER, R_CLM));
  INIT_WALKER(S_file_to_frame, make_walker(file_to_frame_1, NULL, NULL, 2, 3, R_CLM, false, 3, R_CLM, R_NUMBER, R_CLM));
  INIT_WALKER(S_mus_bank, make_walker(mus_bank_1, NULL, NULL, 2, 4, R_FLOAT, false, 4, R_CLM_VECTOR, R_VCT, R_VCT, R_VCT));
  INIT_WALKER(S_oscil_bank, make_walker(oscil_bank_1, NULL, NULL, 3, 4, R_FLOAT, false, 4, R_VCT, R_VECTOR, R_VCT, R_INT));

  INIT_WALKER(S_radians_to_hz, make_walker(mus_radians_to_hz_1, NULL, NULL, 1, 1, R_FLOAT, false, 1, R_NUMBER));
  INIT_WALKER(S_hz_to_radians, make_walker(mus_hz_to_radians_1, NULL, NULL, 1, 1, R_FLOAT, false, 1, R_NUMBER));
  INIT_WALKER(S_in_hz, make_walker(mus_hz_to_radians_1, NULL, NULL, 1, 1, R_FLOAT, false, 1, R_NUMBER));
  INIT_WALKER(S_degrees_to_radians, make_walker(mus_degrees_to_radians_1, NULL, NULL, 1, 1, R_FLOAT, false, 1, R_NUMBER));
  INIT_WALKER(S_radians_to_degrees, make_walker(mus_radians_to_degrees_1, NULL, NULL, 1, 1, R_FLOAT, false, 1, R_NUMBER));
  INIT_WALKER(S_db_to_linear, make_walker(mus_db_to_linear_1, NULL, NULL, 1, 1, R_FLOAT, false, 1, R_NUMBER));
  INIT_WALKER(S_linear_to_db, make_walker(mus_linear_to_db_1, NULL, NULL, 1, 1, R_FLOAT, false, 1, R_NUMBER));
  INIT_WALKER(S_mus_random, make_walker(mus_random_1, NULL, NULL, 1, 1, R_FLOAT, false, 1, R_NUMBER));

  INIT_WALKER(S_make_all_pass, make_walker(make_all_pass_1, NULL, NULL, 0, UNLIMITED_ARGS, R_CLM, false, 1, -R_XEN));
  INIT_WALKER(S_make_average, make_walker(make_average_1, NULL, NULL, 0, UNLIMITED_ARGS, R_CLM, false, 1, -R_XEN));
  INIT_WALKER(S_make_asymmetric_fm, make_walker(make_asymmetric_fm_1, NULL, NULL, 0, 8, R_CLM, false, 1, -R_XEN));
  INIT_WALKER(S_make_buffer, make_walker(make_buffer_1, NULL, NULL, 0, 4, R_CLM, false, 1, -R_XEN));
  INIT_WALKER(S_make_comb, make_walker(make_comb_1, NULL, NULL, 0, UNLIMITED_ARGS, R_CLM, false, 1, -R_XEN));
  INIT_WALKER(S_make_convolve, make_walker(make_convolve_1, NULL, NULL, 0, UNLIMITED_ARGS, R_CLM, false, 1, -R_XEN));
  INIT_WALKER(S_make_delay, make_walker(make_delay_1, NULL, NULL, 0, UNLIMITED_ARGS, R_CLM, false, 1, -R_XEN));
  INIT_WALKER(S_make_env, make_walker(make_env_1, NULL, NULL, 0, UNLIMITED_ARGS, R_CLM, false, 1, -R_XEN));
  INIT_WALKER(S_make_fft_window, make_walker(make_fft_window_1, NULL, NULL, 2, 3, R_VCT, false, 1, -R_XEN));
  INIT_WALKER(S_make_file_to_frame, make_walker(make_file_to_frame_1, NULL, NULL, 0, 1, R_CLM, false, 1, -R_XEN));
  INIT_WALKER(S_make_file_to_sample, make_walker(make_file_to_sample_1, NULL, NULL, 0, 1, R_CLM, false, 1, -R_XEN));
  INIT_WALKER(S_make_filter, make_walker(make_filter_1, NULL, NULL, 0, 6, R_CLM, false, 1, -R_XEN));
  INIT_WALKER(S_make_fir_filter, make_walker(make_fir_filter_1, NULL, NULL, 0, 4, R_CLM, false, 1, -R_XEN));
  INIT_WALKER(S_make_formant, make_walker(make_formant_1, NULL, NULL, 0, 6, R_CLM, false, 1, -R_XEN));
  INIT_WALKER(S_make_frame, make_walker(make_frame_1, NULL, NULL, 0, UNLIMITED_ARGS, R_CLM, false, 1, -R_XEN));
  INIT_WALKER(S_make_frame_to_file, make_walker(make_frame_to_file_1, NULL, NULL, 0, 4, R_CLM, false, 1, -R_XEN));
  INIT_WALKER(S_make_granulate, make_walker(make_granulate_1, NULL, NULL, 0, UNLIMITED_ARGS, R_CLM, false, 1, -R_XEN));
  INIT_WALKER(S_make_iir_filter, make_walker(make_iir_filter_1, NULL, NULL, 0, 4, R_CLM, false, 1, -R_XEN));
  INIT_WALKER(S_make_locsig, make_walker(make_locsig_1, NULL, NULL, 0, UNLIMITED_ARGS, R_CLM, false, 1, -R_XEN));
  INIT_WALKER(S_make_mixer, make_walker(make_mixer_1, NULL, NULL, 0, UNLIMITED_ARGS, R_CLM, false, 1, -R_XEN));
  INIT_WALKER(S_make_notch, make_walker(make_notch_1, NULL, NULL, 0, UNLIMITED_ARGS, R_CLM, false, 1, -R_XEN));
  INIT_WALKER(S_make_one_pole, make_walker(make_one_pole_1, NULL, NULL, 0, 4, R_CLM, false, 1, -R_XEN));
  INIT_WALKER(S_make_one_zero, make_walker(make_one_zero_1, NULL, NULL, 0, 4, R_CLM, false, 1, -R_XEN));
  INIT_WALKER(S_make_oscil, make_walker(make_oscil_1, NULL, NULL, 0, 4, R_CLM, false, 1, -R_XEN));
  INIT_WALKER(S_make_phase_vocoder, make_walker(make_phase_vocoder_1, NULL, NULL, 0, UNLIMITED_ARGS, R_CLM, false, 1, -R_XEN));
  INIT_WALKER(S_make_ppolar, make_walker(make_ppolar_1, NULL, NULL, 0, 4, R_CLM, false, 1, -R_XEN));
  INIT_WALKER(S_make_pulse_train, make_walker(make_pulse_train_1, NULL, NULL, 0, 6, R_CLM, false, 1, -R_XEN));
  INIT_WALKER(S_make_rand, make_walker(make_rand_1, NULL, NULL, 0, 6, R_CLM, false, 1, -R_XEN));
  INIT_WALKER(S_make_rand_interp, make_walker(make_rand_interp_1, NULL, NULL, 0, 6, R_CLM, false, 1, -R_XEN));
  INIT_WALKER(S_make_readin, make_walker(make_readin_1, NULL, NULL, 0, 8, R_CLM, false, 1, -R_XEN));
  INIT_WALKER(S_make_sample_to_file, make_walker(make_sample_to_file_1, NULL, NULL, 4, 5, R_CLM, false, 1, -R_XEN));
  INIT_WALKER(S_make_sawtooth_wave, make_walker(make_sawtooth_wave_1, NULL, NULL, 0, 6, R_CLM, false, 1, -R_XEN));
  INIT_WALKER(S_make_sine_summation, make_walker(make_sine_summation_1, NULL, NULL, 0, UNLIMITED_ARGS, R_CLM, false, 1, -R_XEN));
  INIT_WALKER(S_make_square_wave, make_walker(make_square_wave_1, NULL, NULL, 0, 6, R_CLM, false, 1, -R_XEN));
  INIT_WALKER(S_make_src, make_walker(make_src_1, NULL, NULL, 0, 6, R_CLM, false, 1, -R_XEN));
  INIT_WALKER(S_make_sum_of_cosines, make_walker(make_sum_of_cosines_1, NULL, NULL, 0, 6, R_CLM, false, 1, -R_XEN));
  INIT_WALKER(S_make_sum_of_sines, make_walker(make_sum_of_sines_1, NULL, NULL, 0, 6, R_CLM, false, 1, -R_XEN));
  INIT_WALKER(S_make_ssb_am, make_walker(make_ssb_am_1, NULL, NULL, 0, 4, R_CLM, false, 1, -R_XEN));
  INIT_WALKER(S_make_table_lookup, make_walker(make_table_lookup_1, NULL, NULL, 0, 6, R_CLM, false, 1, -R_XEN));
  INIT_WALKER(S_make_triangle_wave, make_walker(make_triangle_wave_1, NULL, NULL, 0, 6, R_CLM, false, 1, -R_XEN));
  INIT_WALKER(S_make_two_pole, make_walker(make_two_pole_1, NULL, NULL, 0, 6, R_CLM, false, 1, -R_XEN));
  INIT_WALKER(S_make_two_zero, make_walker(make_two_zero_1, NULL, NULL, 0, 6, R_CLM, false, 1, -R_XEN));
  INIT_WALKER(S_make_wave_train, make_walker(make_wave_train_1, NULL, NULL, 0, 6, R_CLM, false, 1, -R_XEN));
  INIT_WALKER(S_make_waveshape, make_walker(make_waveshape_1, NULL, NULL, 0, 8, R_CLM, false, 1, -R_XEN));
  INIT_WALKER(S_make_zpolar, make_walker(make_zpolar_1, NULL, NULL, 0, 4, R_CLM, false, 1, -R_XEN));


  /* -------- sndlib funcs */
  INIT_WALKER(S_mus_sound_samples, make_walker(mus_sound_samples_1, NULL, NULL, 1, 1, R_INT, false, 1, R_STRING));
  INIT_WALKER(S_mus_sound_frames, make_walker(mus_sound_frames_1, NULL, NULL, 1, 1, R_INT, false, 1, R_STRING));
  INIT_WALKER(S_mus_sound_datum_size, make_walker(mus_sound_datum_size_1, NULL, NULL, 1, 1, R_INT, false, 1, R_STRING));
  INIT_WALKER(S_mus_sound_data_location, make_walker(mus_sound_data_location_1, NULL, NULL, 1, 1, R_INT, false, 1, R_STRING));
  INIT_WALKER(S_mus_sound_chans, make_walker(mus_sound_chans_1, NULL, NULL, 1, 1, R_INT, false, 1, R_STRING));
  INIT_WALKER(S_mus_sound_srate, make_walker(mus_sound_srate_1, NULL, NULL, 1, 1, R_INT, false, 1, R_STRING));
  INIT_WALKER(S_mus_sound_header_type, make_walker(mus_sound_header_type_1, NULL, NULL, 1, 1, R_INT, false, 1, R_STRING));
  INIT_WALKER(S_mus_sound_data_format, make_walker(mus_sound_data_format_1, NULL, NULL, 1, 1, R_INT, false, 1, R_STRING));
  INIT_WALKER(S_mus_sound_length, make_walker(mus_sound_length_1, NULL, NULL, 1, 1, R_INT, false, 1, R_STRING));
  INIT_WALKER(S_mus_sound_duration, make_walker(mus_sound_duration_1, NULL, NULL, 1, 1, R_FLOAT, false, 1, R_STRING));
  INIT_WALKER(S_mus_sound_comment, make_walker(mus_sound_comment_1, NULL, NULL, 1, 1, R_STRING, false, 1, R_STRING));

  INIT_WALKER(S_mus_header_type_name, make_walker(mus_header_type_name_1, NULL, NULL, 1, 1, R_STRING, false, 1, R_INT));
  INIT_WALKER(S_mus_data_format_name, make_walker(mus_data_format_name_1, NULL, NULL, 1, 1, R_STRING, false, 1, R_INT));
  INIT_WALKER(S_mus_bytes_per_sample, make_walker(mus_bytes_per_sample_1, NULL, NULL, 1, 1, R_INT, false, 1, R_INT));

  INIT_WALKER(S_vct_ref, make_walker(vct_ref_1, NULL, vct_set_1, 2, 2, R_FLOAT, false, 2, R_VCT, R_INT));
  INIT_WALKER(S_vct_length, make_walker(vct_length_1, NULL, NULL, 1, 1, R_INT, false, 1, R_VCT));
  INIT_WALKER(S_vct_fillB, make_walker(vct_fill_1, NULL, NULL, 2, 2, R_VCT, false, 2, R_VCT, R_NUMBER));
  INIT_WALKER(S_vct_scaleB, make_walker(vct_scale_1, NULL, NULL, 2, 2, R_VCT, false, 2, R_VCT, R_NUMBER));
  INIT_WALKER(S_vct_offsetB, make_walker(vct_offset_1, NULL, NULL, 2, 2, R_VCT, false, 2, R_VCT, R_NUMBER));
  INIT_WALKER(S_vct_addB, make_walker(vct_add_1, NULL, NULL, 2, 2, R_VCT, false, 2, R_VCT, R_VCT));
  INIT_WALKER(S_vct_subtractB, make_walker(vct_subtract_1, NULL, NULL, 2, 2, R_VCT, false, 2, R_VCT, R_VCT));
  INIT_WALKER(S_vct_multiplyB, make_walker(vct_multiply_1, NULL, NULL, 2, 2, R_VCT, false, 2, R_VCT, R_VCT));
  INIT_WALKER(S_vct_copy, make_walker(vct_copy_1, NULL, NULL, 1, 1, R_VCT, false, 1, R_VCT));
  INIT_WALKER(S_vct_peak, make_walker(vct_peak_1, NULL, NULL, 1, 1, R_FLOAT, false, 1, R_VCT));
  INIT_WALKER(S_vct_setB, make_walker(vct_set_2, NULL, NULL, 3, 3, R_FLOAT, false, 3, R_VCT, R_INT, R_NUMBER));
  INIT_WALKER(S_make_vct, make_walker(make_vct_1, NULL, NULL, 1, 2, R_VCT, false, 2, R_INT, R_FLOAT));
  INIT_WALKER(S_vct_convolve, make_walker(vct_convolve_1, NULL, NULL, 2, 2, R_VCT, false, 2, R_VCT, R_VCT));
  INIT_WALKER(S_vct, make_walker(vct_1, NULL, NULL, 1, UNLIMITED_ARGS, R_VCT, false, 1, -R_FLOAT));
  INIT_WALKER(S_vct_p, make_walker(vct_p_1, NULL, NULL, 1, 1, R_BOOL, false, 0));

  INIT_WALKER(S_sound_data_length, make_walker(sound_data_length_1, NULL, NULL, 1, 1, R_INT, false, 1, R_SOUND_DATA));
  INIT_WALKER(S_sound_data_chans, make_walker(sound_data_chans_1, NULL, NULL, 1, 1, R_INT, false, 1, R_SOUND_DATA));
  INIT_WALKER(S_sound_data_p, make_walker(sound_data_p_1, NULL, NULL, 1, 1, R_BOOL, false, 0));
  INIT_WALKER(S_sound_data_ref, make_walker(sound_data_ref_1, NULL, sound_data_set_1, 3, 3, R_FLOAT, false, 3, R_SOUND_DATA, R_INT, R_INT));
  INIT_WALKER(S_sound_data_setB, make_walker(sound_data_set_2, NULL, NULL, 4, 4, R_FLOAT, false, 4, R_SOUND_DATA, R_INT, R_INT, R_NUMBER));
  INIT_WALKER(S_make_sound_data, make_walker(make_sound_data_1, NULL, NULL, 2, 2, R_SOUND_DATA, false, 2, R_INT, R_INT));

  /* -------- snd funcs */
  INIT_WALKER(S_next_sample, make_walker(next_sample_1, NULL, NULL, 1, 1, R_FLOAT, false, 1, R_READER));
  INIT_WALKER(S_previous_sample, make_walker(previous_sample_1, NULL, NULL, 1, 1, R_FLOAT, false, 1, R_READER));
  INIT_WALKER(S_read_sample, make_walker(reader_1, NULL, NULL, 1, 1, R_FLOAT, false, 1, R_READER));
  INIT_WALKER(S_make_sample_reader, make_walker(make_sample_reader_1, NULL, NULL, 0, 5, R_READER, false, 1, R_NUMBER));
  INIT_WALKER(S_sample_reader_p, make_walker(sample_reader_p_1, NULL, NULL, 1, 1, R_BOOL, false, 0));
  INIT_WALKER(S_sample_reader_at_end_p, make_walker(sample_reader_at_end_p_1, NULL, NULL, 1, 1, R_BOOL, false, 0));

  INIT_WALKER(S_make_region_sample_reader, make_walker(make_region_sample_reader_1, NULL, NULL, 3, 3, R_READER, false, 3, R_INT, R_INT, R_INT));
  INIT_WALKER(S_read_region_sample, make_walker(reader_1, NULL, NULL, 1, 1, R_FLOAT, false, 1, R_READER));

  INIT_WALKER(S_read_mix_sample, make_walker(mix_reader_1, NULL, NULL, 1, 1, R_FLOAT, false, 1, R_MIX_READER));
  INIT_WALKER(S_make_mix_sample_reader, make_walker(make_mix_sample_reader_1, NULL, NULL, 1, 2, R_MIX_READER, false, 1, R_NUMBER));
  INIT_WALKER(S_mix_sample_reader_p, make_walker(mix_sample_reader_p_1, NULL, NULL, 1, 1, R_BOOL, false, 0));

  INIT_WALKER(S_read_track_sample, make_walker(track_reader_1, NULL, NULL, 1, 1, R_FLOAT, false, 1, R_TRACK_READER));
  INIT_WALKER(S_make_track_sample_reader, make_walker(make_track_sample_reader_1, NULL, NULL, 1, 3, R_TRACK_READER, false, 2, R_NUMBER, R_NUMBER));
  INIT_WALKER(S_track_sample_reader_p, make_walker(track_sample_reader_p_1, NULL, NULL, 1, 1, R_BOOL, false, 0));

  INIT_WALKER(S_edit_position, make_walker(edit_position_1, NULL, NULL, 0, 2, R_INT, false, 0));
  INIT_WALKER(S_frames, make_walker(frames_1, NULL, NULL, 0, 3, R_INT, false, 0));
  INIT_WALKER(S_cursor, make_walker(cursor_1, NULL, NULL, 0, 2, R_INT, false, 0));
  INIT_WALKER(S_add_mark, make_walker(add_mark_1, NULL, NULL, 1, 3, R_INT, false, 0));
  INIT_WALKER(S_maxamp, make_walker(maxamp_1, NULL, NULL, 0, 2, R_FLOAT, false, 0));
  INIT_WALKER(S_srate, make_walker(srate_1, NULL, NULL, 0, 1, R_INT, false, 0));
  INIT_WALKER(S_channels, make_walker(channels_1, NULL, NULL, 0, 1, R_INT, false, 0));
  INIT_WALKER(S_c_g, make_walker(c_g_p_1, NULL, NULL, 0, 0, R_BOOL, false, 0));
  INIT_WALKER(S_autocorrelate, make_walker(autocorrelate_1, NULL, NULL, 1, 1, R_VCT, false, 1, R_VCT));
  INIT_WALKER(S_samples_to_vct, make_walker(samples_to_vct_1, NULL, NULL, 5, 6, R_VCT, false, 5, R_INT, R_INT, R_ANY, R_ANY, R_VCT));

  INIT_WALKER(S_snd_print, make_walker(snd_print_1, NULL, NULL, 1, 1, R_BOOL, false, 1, R_STRING));
  INIT_WALKER(S_snd_warning, make_walker(snd_warning_1, NULL, NULL, 1, 1, R_BOOL, false, 1, R_STRING));
  INIT_WALKER(S_report_in_minibuffer, make_walker(report_in_minibuffer_1, NULL, NULL, 1, 2, R_BOOL, false, 1, R_STRING));

  INIT_WALKER(S_sound_p, make_walker(r_sound_p_1, NULL, NULL, 1, 1, R_BOOL, false, 1, R_INT));
  INIT_WALKER(S_mix_p, make_walker(mix_ok_1, NULL, NULL, 1, 1, R_BOOL, false, 1, R_INT));
  INIT_WALKER(S_track_p, make_walker(track_p_1, NULL, NULL, 1, 1, R_BOOL, false, 1, R_INT));
  INIT_WALKER(S_region_p, make_walker(region_ok_1, NULL, NULL, 1, 1, R_BOOL, false, 1, R_INT));
  INIT_WALKER(S_mark_p, make_walker(r_mark_p_1, NULL, NULL, 1, 1, R_BOOL, false, 1, R_INT));
  INIT_WALKER(S_mark_sync, make_walker(r_mark_sync_1, NULL, NULL, 1, 1, R_INT, false, 1, R_INT));
  INIT_WALKER(S_mark_sample, make_walker(r_mark_sample_1, NULL, NULL, 1, 1, R_INT, false, 1, R_INT));
  INIT_WALKER(S_mark_sync_max, make_walker(mark_sync_max_1, NULL, NULL, 0, 0, R_INT, false, 0));
  INIT_WALKER(S_selection_chans, make_walker(selection_chans_1, NULL, NULL, 0, 0, R_INT, false, 0));
  INIT_WALKER(S_temp_dir, make_walker(r_temp_dir_1, NULL, NULL, 0, 0, R_STRING, false, 0));
  INIT_WALKER(S_save_dir, make_walker(r_save_dir_1, NULL, NULL, 0, 0, R_STRING, false, 0));
  INIT_WALKER(S_mix_chans, make_walker(r_mix_chans_1, NULL, NULL, 1, 1, R_INT, false, 1, R_INT));
  INIT_WALKER(S_mix_track, make_walker(r_mix_track_1, NULL, NULL, 1, 1, R_INT, false, 1, R_INT));
  INIT_WALKER(S_mix_position, make_walker(r_mix_position_1, NULL, NULL, 1, 1, R_INT, false, 1, R_INT));
  INIT_WALKER(S_mix_frames, make_walker(r_mix_frames_1, NULL, NULL, 1, 1, R_INT, false, 1, R_INT));
  INIT_WALKER(S_mix_inverted_p, make_walker(r_mix_inverted_1, NULL, NULL, 1, 1, R_BOOL, false, 1, R_INT));
  INIT_WALKER(S_mix_locked_p, make_walker(r_mix_locked_1, NULL, NULL, 1, 1, R_BOOL, false, 1, R_INT));
  INIT_WALKER(S_track_chans, make_walker(track_chans_1, NULL, NULL, 1, 1, R_INT, false, 1, R_INT));
  INIT_WALKER(S_track_track, make_walker(r_track_track_1, NULL, NULL, 1, 1, R_INT, false, 1, R_INT));
  INIT_WALKER(S_region_chans, make_walker(region_chans_1, NULL, NULL, 1, 1, R_INT, false, 1, R_INT));
  INIT_WALKER(S_region_srate, make_walker(region_srate_1, NULL, NULL, 1, 1, R_INT, false, 1, R_INT));
  INIT_WALKER(S_region_frames, make_walker(region_len_1, NULL, NULL, 1, 1, R_INT, false, 1, R_INT));

  INIT_WALKER(S_mix_speed, make_walker(r_mix_speed_1, NULL, NULL, 1, 1, R_FLOAT, false, 1, R_INT));
  INIT_WALKER(S_track_speed, make_walker(r_track_speed_1, NULL, NULL, 1, 1, R_FLOAT, false, 1, R_INT));
  INIT_WALKER(S_track_tempo, make_walker(r_track_tempo_1, NULL, NULL, 1, 1, R_FLOAT, false, 1, R_INT));
  INIT_WALKER(S_track_amp, make_walker(r_track_amp_1, NULL, NULL, 1, 1, R_FLOAT, false, 1, R_INT));
  INIT_WALKER(S_region_maxamp, make_walker(region_maxamp_1, NULL, NULL, 1, 1, R_FLOAT, false, 1, R_INT));

  INIT_WALKER(S_fft, make_walker(fft_1, NULL, NULL, 2, 3, R_VCT, false, 3, R_VCT, R_VCT, R_INT));
}
#endif

void g_init_run(void)
{
#if WITH_RUN
  XEN_DEFINE_PROCEDURE("run-internal", g_run, 1, 0, 0, "run macro testing...");
  XEN_EVAL_C_STRING("(defmacro " S_run " (thunk) `(run-internal (list ',thunk ,thunk)))");
  XEN_SET_DOCUMENTATION(S_run, H_run);
  XEN_DEFINE_PROCEDURE("run-eval", g_run_eval, 1, 3, 0, "run macro testing...");
  XEN_DEFINE_PROCEDURE("vct-map-2",     g_vct_map, 2, 0, 0,      H_vct_map);
  XEN_EVAL_C_STRING("(defmacro* " S_vct_map " (thunk #:rest args) `(vct-map-2 (list ',thunk ,thunk) (list ,@args)))");
  XEN_SET_DOCUMENTATION(S_vct_map, H_vct_map);
  XEN_DEFINE_PROCEDURE(S_add_clm_field, g_add_clm_field, 2, 1, 0, H_add_clm_field);
  XEN_DEFINE_PROCEDURE(S_add_clm_type, g_add_clm_type, 1, 0, 0, H_add_clm_type);
#if DEBUGGING
  XEN_DEFINE_PROCEDURE("describe-walk-info", g_describe_walk_info, 1, 0, 0, "internal debugging aid");
#endif
  XEN_DEFINE_PROCEDURE("show-ptree", g_show_ptree, 1, 0, 0, "internal debugging stuff");
#else
  XEN_DEFINE_PROCEDURE(S_vct_map, g_vct_map_w, 2, 0, 0, H_vct_map);
#endif

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_optimization, g_optimization_w, H_optimization,
				   S_setB S_optimization, g_set_optimization_w,  0, 0, 1, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_run_safety, g_run_safety_w, H_run_safety,
				   S_setB S_run_safety, g_set_run_safety_w,  0, 0, 1, 0);

#if HAVE_GUILE
  #define H_optimization_hook S_optimization_hook " (msg): called if the run macro encounters \
something it can't optimize.  'msg' is a string description of the offending form:\n\
  (add-hook! optimization-hook (lambda (msg) (snd-print msg)))\n\
You can often slightly rewrite the form to make run happy."
#else
#define H_optimization_hook S_optimization_hook " (msg): has no effect since 'run' is Guile-specific."
#endif

  XEN_DEFINE_HOOK(optimization_hook, S_optimization_hook, 1, H_optimization_hook);      /* arg = message */

#if HAVE_GUILE && WITH_RUN 
  init_walkers();
  init_type_names();
#endif
}
