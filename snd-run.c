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
 *   the program counter (PC) and the termination flag (ALL_DONE).  There is
 *   also a "self" pointer PTREE.
 *
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
 *   if begin or and not let let* set! cond do do* define case[int keys]
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
 *
 *   various sndlib, clm, and snd functions
 *
 * tests in snd-test.scm, test 22
 *
 * SOMEDAY: split Scheme from Snd/Clm here and do the latter via an FFI of some sort
 * TODO: file->array and array->file, comment? delete-mark? fft? file-name? partial->poly etc? 
 *        these should be named file->vct etc
 * TODO: left|right-sample? mark-name? mark? mark-sample mark-sync find-mark
 * TODO: mix/track/region sample readers? region info? selection info? mix info?
 * TODO: samples->vct? access to sound|channel-properties? sound?[g_sound_p in snd-snd.c]
 * TODO: def-clm-struct make-funcs?
 * SOMEDAY: save ptree somehow (local runs make this problematic) -- perhaps definstrument here
 *
 * LIMITATIONS: <insert anxious lucubration here about DSP context and so on>
 *      variables can have only one type, the type has to be ascertainable somehow (similarly for vector elements)
 *      some variables (imported from outside our context) cannot be set, in some cases they can't even be found (args to define* for example)
 *      no recursion (could be added with some pain)
 *      no [variable] lists or pairs
 *      no macro expansion (not sure how to handle this in Guile)
 *      no complex, ratio, bignum
 *      no pointer aliasing (i.e. vct var set to alias another vct var etc -- GC confusion otherwise)
 *      no apply or eval (we need to know at parse time what we are trying to do -- actually these might be doable)
 *      no "delay/force", no syntax-case fanciness
 *      no map or for-each (these need lists)
 *
 * whenever the code-walker or tree initializer finds something it is unhappy about,
 *  it returns an error indication, and the caller should fallback on Guile's evaluator.
 *
 * so where does the speed-up come from? We're not getting/freeing any Guile memory so the gc is never
 *   triggered, we're doing math ops direct and normally using float (not double),
 *   no function args are cons'd, no run-time types are checked, no values are boxed/unboxed,
 *   no symbols are looked-up in the current environment, wherever possible we pre-convert
 *   args to same type (i.e. int->float done just once, if possible)
 */

#include "snd.h"
#include "clm2xen.h"
#include "clm-strings.h"
#include "sndlib-strings.h"

static XEN optimization_hook = XEN_FALSE;

/* this code assumes a void* is the same size as int */
#if HAVE_GUILE && WITH_RUN && HAVE_STRINGIZE

#define DONT_OPTIMIZE 0
#define OMIT_COMPLEX 1
#define COMPLEX_OK 2
#define GLOBAL_OK 3
#define GLOBAL_SET_OK 5
#define SOURCE_OK 6

#define C_TO_XEN_CHAR(c)                    SCM_MAKE_CHAR(c)
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
#define XEN_PAIR_P(a)                       XEN_TRUE_P(scm_pair_p(a))
#define XEN_APPLICABLE_SMOB_P(a)            (SCM_TYP7(a) == scm_tc7_smob)
#define XEN_ENV(a)                          SCM_ENV(a)
#define XEN_VAR_NAME_TO_VAR(a)              scm_sym2var(scm_str2symbol(a), scm_current_module_lookup_closure(), XEN_FALSE)
#define XEN_SYMBOL_TO_VAR(a)                scm_sym2var(a, scm_current_module_lookup_closure(), XEN_FALSE)
#define XEN_SET_CDR(pair, new_val)          scm_set_cdr_x(pair, new_val)
#if 0
#define XEN_FRANDOM(a)                      (a * scm_c_uniform01(scm_c_default_rstate()))
#define XEN_IRANDOM(a)                      scm_c_random(scm_c_default_rstate(), a)
#else
/* those are getting some complaint from Guile about the default_rstate */
#define XEN_FRANDOM(a)                      mus_frandom(a)
#define XEN_IRANDOM(a)                      mus_irandom(a)
#endif
#define INTEGER_TO_STRING(a)                XEN_TO_C_STRING(scm_number_to_string(C_TO_XEN_INT(a), XEN_UNDEFINED))
#define INTEGER_TO_STRING_WITH_RADIX(a, b)  XEN_TO_C_STRING(scm_number_to_string(C_TO_XEN_INT(a), C_TO_XEN_INT(b)))
#define DOUBLE_TO_STRING(a)                 XEN_TO_C_STRING(scm_number_to_string(C_TO_XEN_DOUBLE(a), XEN_UNDEFINED))
#define DOUBLE_TO_STRING_WITH_RADIX(a, b)   XEN_TO_C_STRING(scm_number_to_string(C_TO_XEN_DOUBLE(a), C_TO_XEN_INT(b)))
#define XEN_PROCEDURE_SOURCE_TO_C_STRING(a) XEN_AS_STRING(scm_procedure_source(a))
#define XEN_LIST_REF_WRAPPED(a, b)          scm_list_ref(a, b)
#define XEN_OBJECT_PROPERTY(Obj, Prop)      scm_object_property(Obj, Prop)
#define XEN_SET_OBJECT_PROPERTY(Obj, Prop, Val) scm_set_object_property_x(Obj, Prop, Val)
#define XEN_PROCEDURE_PROPERTY(Obj, Prop)   scm_procedure_property(Obj, Prop)
#define XEN_SET_PROCEDURE_PROPERTY(Obj, Prop, Val) scm_set_procedure_property_x(Obj, Prop, Val)
#define XEN_PROCEDURE_SOURCE(Proc)          scm_procedure_source(Proc)
#define XEN_PROCEDURE_WITH_SETTER_P(Proc)   scm_procedure_with_setter_p(Proc)

#define INT_PT  "i%d(%d)"
#define FLT_PT  "d%d(%.4f)"
#define PTR_PT  "i%d(%p)"
#define STR_PT  "i%d(\"%s\")"
#define CHR_PT  "i%d(#\\%c)"
#define LST_PT  "i%d(%s)"
#define KEY_PT  "i%d(%s)"
#define RD_PT   "i%d(%s)"
#define CLM_PT  "i%d(%s)"
#define VCT_PT  "i%d(%s)"
#define GO_PT   "i%d(continuation)"
#define BOOL_PT "i%d(%s)"
#define B2S(Arg) ((Arg) ? "#t" : "#f")

#define UNLIMITED_ARGS -1
static XEN walk_sym = XEN_FALSE;

/* find and set (Guile) variable values */

static void xen_symbol_name_set_value(char *a, XEN b)
{
  XEN var = XEN_FALSE;
  var = XEN_VAR_NAME_TO_VAR(a);
  if (!(XEN_FALSE_P(var)))
    XEN_VARIABLE_SET(var, b);
}

static XEN symbol_to_value(XEN code, XEN sym, int *local)
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
      /* fprintf(stderr,"look for %s in %s\n", XEN_AS_STRING(sym), XEN_AS_STRING(code_env)); */
      if (XEN_LIST_P(code_env))
	{
	  (*local) = 1;
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
  val = XEN_SYMBOL_TO_VAR(sym);
  if (!(XEN_FALSE_P(val))) 
    {
      new_val = XEN_VARIABLE_REF(val);
      if (XEN_BOUND_P(new_val))
	(*local) = 0;
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
  /* fprintf(stderr,"set %s to %s\n", XEN_AS_STRING(sym), XEN_AS_STRING(new_val)); */
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
  var = XEN_SYMBOL_TO_VAR(sym);
  if (!(XEN_FALSE_P(var)))
    XEN_VARIABLE_SET(var, new_val);
  return(new_val);
}

enum {R_UNSPECIFIED, R_INT, R_FLOAT, R_BOOL, R_CHAR, R_STRING, R_LIST, R_PAIR, 
      R_SYMBOL, R_KEYWORD, R_FUNCTION, R_GOTO, R_VCT, R_READER, R_CLM, R_XCLM,
      R_FLOAT_VECTOR, R_INT_VECTOR, R_VCT_VECTOR, R_CLM_VECTOR, 
      R_NUMBER, R_CONS, R_VECTOR, R_XEN, R_ANY}; /* last 5 for walker arg checks */

#define BUILT_IN_TYPES 25
static int last_type = R_ANY;
static int type_names_size = BUILT_IN_TYPES;
static char **type_names = NULL;
static char *basic_type_names[BUILT_IN_TYPES] = {"unspecified", "int", "float", "boolean", "char", "string", "list", "pair", 
						 "symbol", "keyword", "function", "continuation", "vct", "reader", "clm", "x-clm", 
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
static int add_new_type(char *new_type)
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
static int name_to_type(char *name)
{
  int i;
  for (i = 0; i <= last_type; i++)
    if (strcmp(name, type_names[i]) == 0)
      return(i);
  return(R_UNSPECIFIED);
}

#define POINTER_P(Type) ((Type) > R_GOTO)
#define POINTER_OR_GOTO_P(Type) ((Type) > R_FUNCTION)
#define VECTOR_P(Type) (((Type) >= R_FLOAT_VECTOR) && ((Type) <= R_CLM_VECTOR))

enum {R_VARIABLE, R_CONSTANT};
#define NEED_ANY_RESULT 1
#define NEED_INT_RESULT 2
#define NEED_XCLM_RESULT 3

static int current_optimization = DONT_OPTIMIZE;
static int run_warned = FALSE;

typedef struct {
  void (*function)(int *arg_addrs, int *ints, Float *dbls);
  int *args;
  char *(*descr)(int *arg_addrs, int *ints, Float *dbls); /* for debugging */
  int no_opt;
} triple;

static triple *free_triple(triple *trp)
{
  if (trp->args) FREE(trp->args);
  trp->args = NULL;
  FREE(trp);
  return(NULL);
}

static char *describe_triple(triple *trp, int *ints, Float *dbls)
{
  if (trp->descr)
    return((*(trp->descr))(trp->args, ints, dbls));
  return(NULL);
}

typedef struct {
  int type;
  int addr;
  int constant;
  int gc;
} xen_value;

#if (!DEBUG_MEMORY)
static xen_value *make_xen_value(int typ, int address, int constant)
{
  xen_value *v;
  v = (xen_value *)CALLOC(1, sizeof(xen_value));
  v->type = typ;
  v->addr = address;
  v->constant = constant;
  v->gc = FALSE;
  return(v);
}

#else

static xen_value *make_xen_value_1(int typ, int address, int constant, const char *func, int line)
{
  xen_value *v;
  char *buf;
  buf = (char *)malloc(64);
  sprintf(buf, "%s: %d", func, line);
  set_encloser(buf);
  v = (xen_value *)CALLOC(1, sizeof(xen_value));
  v->type = typ;
  v->addr = address;
  v->constant = constant;
  v->gc = FALSE;
  set_encloser(NULL);
  return(v);
}

#define make_xen_value(a,b,c) make_xen_value_1(a, b, c, __FUNCTION__, __LINE__)
#endif

#define OPTIMIZER_WARNING_BUFFER_SIZE 1024
static char optimizer_warning_buffer[OPTIMIZER_WARNING_BUFFER_SIZE];
static xen_value *run_warn(char *format, ...)
{
  va_list ap;
  run_warned = TRUE;
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
  run_warned = TRUE;
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

static char *describe_xen_value(xen_value *v, int *ints, Float *dbls);

typedef struct {
  char *name;
  xen_value *v;
  int global, unclean, unsettable;
} xen_var;

static char *describe_xen_var(xen_var *var, int *ints, Float *dbls)
{
  char *buf, *temp;
  if (var == NULL) return(copy_string("#<xen_var: null>"));
  temp = describe_xen_value(var->v, ints, dbls);
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

typedef struct {
  char *name;
  xen_value *result, *jump;
  int loc;
} continuation;

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

#define INITIAL_INT_CTR 3

typedef struct {
  triple **program;
  int *ints; 
  Float *dbls;
  /* making these global throughout simplified the code, but did not speed up evaluation at all (when optimized),
   *   also, ptrees can be saved and called at any time (interleaved eval_ptrees), and the ints/dbls arrays
   *   constitute the "closure" of that ptree, so we can't use global arrays in any case.
   */
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
  int need_init;
  XEN code, form;
  int str_ctr, strs_size;
  int *strs;
  void *outer_tree;

  /* next needed by tree builders (temps) */
  int constants, float_result, need_result;
} ptree;

static xen_var *find_var_in_ptree(ptree *pt, char *name)
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

static xen_var *find_var_in_ptree_via_addr(ptree *pt, int dbl, int addr)
{
  /* search backwards for shadowing */
  int i;
  for (i = pt->var_ctr - 1; i >= 0; i--)
    if ((pt->vars[i]) &&
	(pt->vars[i]->v->addr == addr) &&
	(dbl == (pt->vars[i]->v->type == R_FLOAT)))
      return(pt->vars[i]);
  for (i = 0; i < pt->global_var_ctr; i++)
    if ((pt->global_vars[i]) &&
	(pt->global_vars[i]->v->addr == addr) &&
	(dbl == (pt->global_vars[i]->v->type == R_FLOAT)))
      return(pt->global_vars[i]);
  return(NULL);
}

#if DEBUGGING
static char **saved_names = NULL;
static int *saved_int_addrs = NULL;
static int *saved_dbl_addrs = NULL;
static int saved_vars = 0, saved_var_ctr = 0;

static void save_local_var_info(xen_var *var)
{
  int i, loc = -1;
  if (saved_vars == 0)
    {
      saved_vars = 8;
      saved_dbl_addrs = (int *)CALLOC(saved_vars, sizeof(int));
      saved_int_addrs = (int *)CALLOC(saved_vars, sizeof(int));
      saved_names = (char **)CALLOC(saved_vars, sizeof(char *));
    }
  else
    {
      if (saved_var_ctr == saved_vars)
	{
	  loc = saved_vars;
	  saved_vars += 8;
	  saved_dbl_addrs = (int *)REALLOC(saved_dbl_addrs, saved_vars * sizeof(int));
	  saved_int_addrs = (int *)REALLOC(saved_int_addrs, saved_vars * sizeof(int));
	  saved_names = (char **)REALLOC(saved_names, saved_vars * sizeof(char *));
	  for (i = loc; i < saved_vars; i++) saved_names[i] = NULL;
	}
    }
  loc = saved_var_ctr++;
  if (var->v->type == R_FLOAT)
    saved_dbl_addrs[loc] = var->v->addr;
  else saved_int_addrs[loc] = var->v->addr;
  saved_names[loc] = copy_string(var->name);
}

static char *find_local_var_via_addr(int is_float, int addr)
{
  int i;
  for (i = 0; i < saved_var_ctr; i++)
    {
      if ((is_float) && (saved_dbl_addrs[i] == addr))
	return(saved_names[i]);
      if ((!is_float) && (saved_int_addrs[i] == addr))
	return(saved_names[i]);
    }
  return(NULL);
}

static void free_saved_locals(void)
{
  int i;
  for (i = 0; i < saved_vars; i++)
    if (saved_names[i]) FREE(saved_names[i]);
  if (saved_names) FREE(saved_names);
  if (saved_int_addrs) FREE(saved_int_addrs);
  if (saved_dbl_addrs) FREE(saved_dbl_addrs);
  saved_var_ctr = 0;
  saved_vars = 0;
  saved_names = NULL;
  saved_int_addrs = NULL;
  saved_dbl_addrs = NULL;
}
#endif

static char *add_comments(ptree *pt, char *str)
{
  xen_var *var;
  int addr, i, j, len;
  char *new_buf;
  char name[64];
  int name_pending = FALSE;
  if (str == NULL) return(NULL);
  /* look for i%n(...) or d%n(...), if found, look for addr in pt tables, if found add name to buf */
  len = snd_strlen(str);
  new_buf = (char *)CALLOC(len + 256, sizeof(char));
  for (i = 0, j = 0; i < len; i++)
    {
      if (((str[i] != 'i') && (str[i] != 'd')) ||
	  (!(isdigit(str[i + 1]))))
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
	      name_pending = FALSE;
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
	  while (isdigit(str[k]));
	  if (str[k] == '(')
	    {
	      var = find_var_in_ptree_via_addr(pt, str[i] == 'd', addr);
	      if (var)
		{
		  strcpy(name, var->name);
		  name_pending = TRUE;
		}
#if DEBUGGING
	      else
		{
		  char *local_name;
		  local_name = find_local_var_via_addr(str[i] == 'd', addr);
		  if (local_name)
		    {
		      strcpy(name, local_name);
		      name_pending = TRUE;
		    }
		}
#endif
	    }
	  new_buf[j++] = str[i];
	}
    }
  FREE(str);
  return(new_buf);
}

static char *str_append(char *oldstr, int *oldsize, char *newstr)
{
  int size;
  size = strlen(oldstr) + strlen(newstr);
  if (size >= (*oldsize))
    {
      oldstr = (char *)REALLOC(oldstr, size * 2 * sizeof(char));
      (*oldsize) = size * 2;
    }
  strcat(oldstr, newstr);
  FREE(newstr);
  return(oldstr);
}

XEN ptree_code(void *p)
{
  return(((ptree *)p)->form);
}

static char *describe_ptree(ptree *p)
{
  int i, size;
  char *temp = NULL;
  char *buf;
  int *ints;
  Float *dbls;
  size = 1024;
  buf = (char *)CALLOC(size, sizeof(char));
  
  if (p->outer_tree)
    {
      ints = ((ptree *)(p->outer_tree))->ints;
      dbls = ((ptree *)(p->outer_tree))->dbls;
    }
  else
    {
      ints = p->ints;
      dbls = p->dbls;
    }
  buf = str_append(buf, &size, mus_format("ints: %d, dbls: %d, triples: %d, vars: %d\n  [",
					  p->int_ctr, p->dbl_ctr, p->triple_ctr, p->var_ctr));
  
  for (i = 0; i < p->int_ctr - 1; i++)
    {
      temp = (char *)CALLOC(16, sizeof(char));
      mus_snprintf(temp, 16, "%d, ", ints[i]);
      buf = str_append(buf, &size, temp);
    }
  temp = (char *)CALLOC(16, sizeof(char));
  mus_snprintf(temp, 16, "%d], [", ints[p->int_ctr - 1]);
  buf = str_append(buf, &size, temp);
  for (i = 0; i < p->dbl_ctr - 1; i++)
    {
      temp = (char *)CALLOC(16, sizeof(char));
      mus_snprintf(temp, 16, "%.4f, ", dbls[i]);
      buf = str_append(buf, &size, temp);
    }
  temp = (char *)CALLOC(16, sizeof(char));
  mus_snprintf(temp, 16, "%.4f]\n", dbls[p->dbl_ctr - 1]);
  buf = str_append(buf, &size, temp);
  for (i = 0; i < p->triple_ctr; i++)
    {
      temp = add_comments(p, describe_triple(p->program[i], ints, dbls));
      if (temp)
	{
	  buf = str_append(buf, &size, mus_format("  %d: %s\n", i, temp));
	  FREE(temp);
	}
    }
  strcat(buf, "\n");
  for (i = 0; i < p->var_ctr; i++)
    {
      temp = describe_xen_var(p->vars[i], ints, dbls);
      buf = str_append(buf, &size, mus_format("[var %d]: %s\n", i, temp));
      FREE(temp);
    }
  for (i = 0; i < p->global_var_ctr; i++)
    {
      temp = describe_xen_var(p->global_vars[i], ints, dbls);
      buf = str_append(buf, &size, mus_format("[global_var %d]: %s\n", i, temp));
      FREE(temp);
    }
  if (p->result)
    {
      temp = describe_xen_value(p->result, ints, dbls);
      if (temp)
	{
	  buf = str_append(buf, &size, mus_format("\nresult: %s\n", temp));
	  FREE(temp);
	}
    }
  buf = str_append(buf, &size, mus_format("PC: %d (%d)\n", ints[0], p->initial_pc));
  return(buf);
}

typedef struct {
  int length;
  int *data;
} int_vct;

typedef struct {
  int length;
  mus_any **data;
} clm_vct;

typedef struct {
  int length;
  vct **data;
} vct_vct;

static char *describe_xen_value_1(int type, int addr, int *ints, Float *dbls)
{
  switch (type)
    {
    case R_BOOL:    return(mus_format(BOOL_PT, addr, B2S(ints[addr])));                  break;
    case R_INT:     return(mus_format(INT_PT , addr, ints[addr]));                       break;
    case R_CHAR:    return(mus_format(CHR_PT , addr, (char)(ints[addr])));               break;
    case R_STRING:  return(mus_format(STR_PT , addr, (char *)(ints[addr])));             break;
    case R_FLOAT:   return(mus_format(FLT_PT , addr, dbls[addr]));                       break;
    case R_SYMBOL:
    case R_KEYWORD: return(mus_format(KEY_PT , addr, XEN_AS_STRING((XEN)(ints[addr])))); break;
    case R_GOTO:    return(mus_format("continuation: " INT_PT , addr, ints[addr]));      break;
    case R_LIST:
    case R_PAIR:
      if (ints[addr] != 0)
	return(mus_format(LST_PT, addr, XEN_AS_STRING((XEN)(ints[addr])))); 
      else return(mus_format("i(%d)=0: non-constant cons", addr));
      break;
    case R_FLOAT_VECTOR:
    case R_VCT:
      if (ints[addr])
	{
	  char *buf = NULL, *vstr = NULL;
	  vstr = vct_to_string((vct *)(ints[addr]));
	  buf = mus_format(VCT_PT, addr, vstr);
	  if (vstr) FREE(vstr);
	  return(buf);
	}
      else return(copy_string("null"));
      break;
    case R_READER:
      if (ints[addr])
	{
	  char *buf = NULL, *vstr = NULL;
	  vstr = sf_to_string((snd_fd *)(ints[addr]));
	  buf = mus_format(RD_PT, addr, vstr);
	  if (vstr) FREE(vstr);
	  return(buf);
	}
      else return(copy_string("null"));
      break;
    case R_CLM:
      if (ints[addr]) 
	return(mus_format(CLM_PT, addr, mus_describe((mus_any *)(ints[addr]))));  
      else return(copy_string("null"));
      break;
    case R_XCLM:
      if (ints[addr]) 
	return(mus_format(CLM_PT, addr, mus_describe(MUS_XEN_TO_MUS_ANY(ints[addr]))));
      else return(copy_string("null"));
      break;
    case R_FUNCTION: 
      if (ints[addr])
	return(describe_ptree((ptree *)(ints[addr])));
      else return(copy_string("internal lambda?"));
      break;
    case R_INT_VECTOR:  return(mus_format("int vector " PTR_PT , addr, (int_vct *)(ints[addr]))); break;
    case R_VCT_VECTOR:  return(mus_format("vct vector " PTR_PT , addr, (vct_vct *)(ints[addr]))); break;
    case R_CLM_VECTOR:  return(mus_format("clm vector " PTR_PT , addr, (clm_vct *)(ints[addr]))); break;
    case R_UNSPECIFIED: return(copy_string("#<unspecified>")); break;
    default:
      if (type > R_ANY)
	return(mus_format("i%d(%s: clm-struct %x)", addr, type_name(type), ints[addr]));
      else return(mus_format("i%d(unknown type: %d)", addr, type));            
      break;
    }
  return(NULL);
}

static char *describe_xen_value(xen_value *v, int *ints, Float *dbls)
{
  if (v == NULL) return(copy_string("null xen_value"));
  if (ints == NULL) return(copy_string("null ints???"));
  return(describe_xen_value_1(v->type, v->addr, ints, dbls));
}

static int got_lambda = FALSE; /* a temporary kludge?? */

static ptree *make_ptree(int initial_data_size)
{
  ptree *pt;

  got_lambda = FALSE;

  pt = (ptree *)CALLOC(1, sizeof(ptree));
  if (initial_data_size > 0)
    {
      pt->ints = (int *)CALLOC(initial_data_size, sizeof(int));
      pt->ints[2] = (int)pt;
      pt->dbls = (Float *)CALLOC(initial_data_size, sizeof(Float));
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
  pt->strs_size = 0;
  pt->strs = NULL;
  pt->str_ctr = 0;
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
  new_tree->need_init = FALSE;
  new_tree->arity = 0;
  if (pt->outer_tree)
    new_tree->outer_tree = pt->outer_tree;
  else new_tree->outer_tree = (void *)pt;

  new_tree->ints = pt->ints;
  new_tree->dbls = pt->dbls;
  new_tree->vars = pt->vars;
  new_tree->global_vars = pt->global_vars;
  new_tree->gcs = pt->gcs;
  new_tree->gotos = pt->gotos;
  new_tree->strs = pt->strs;
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
  new_tree->strs_size = pt->strs_size;
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
  outer->gc_protected = inner->gc_protected;

  inner->ints = NULL;
  inner->dbls = NULL;
  inner->vars = NULL;
  inner->global_vars = NULL;
  inner->gcs = NULL;
  inner->gotos = NULL;
  inner->strs = NULL;
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
  outer->strs_size = inner->strs_size;
  outer->need_init = (outer->need_init || inner->need_init);
  outer->gc_protected_ctr = inner->gc_protected_ctr;
  outer->gc_protected_size = inner->gc_protected_size;

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
  inner->strs_size = 0;
  inner->need_init = 0;
  inner->gc_protected_size = 0;
  inner->gc_protected_ctr = 0;
}


static void int_vct_into_vector(int_vct *v, XEN vect)
{
  int len, i;
  XEN *vdata;
  len = XEN_VECTOR_LENGTH(vect);
  vdata = XEN_VECTOR_ELEMENTS(vect);
  for (i = 0; i < len; i++) 
    vdata[i] = C_TO_XEN_INT(v->data[i]);
}

static void free_int_vct(int_vct *v)
{
  if (v)
    {
      if (v->data) FREE(v->data);
      FREE(v);
    }
}

static void free_clm_vct(clm_vct *v)
{
  if (v)
    {
      if (v->data) FREE(v->data);
      FREE(v);
    }
}

static void free_vct_vct(vct_vct *v)
{
  if (v)
    {
      if (v->data) FREE(v->data);
      FREE(v);
    }
}

static void clm_struct_restore(ptree *prog, xen_var *var);

static xen_var *free_xen_var(ptree *prog, xen_var *var)
{
  XEN val;
  int local_var;
  if (var)
    {
#if DEBUGGING
      save_local_var_info(var);
#endif
      /* fprintf(stderr, "free var %s %s (global: %d, unclean: %d)\n", var->name, type_name(var->v->type), var->global, var->unclean); */
      /* if var->global, reflect new value into outer level version of the variable upon quit */
      if ((var->global) &&
	  (var->unclean))
	{
	  if (current_optimization < GLOBAL_SET_OK)
	    switch (var->v->type)
	      {
	      case R_FLOAT:  xen_symbol_name_set_value(var->name, C_TO_XEN_DOUBLE(prog->dbls[var->v->addr]));           break;
	      case R_INT:    xen_symbol_name_set_value(var->name, C_TO_XEN_INT(prog->ints[var->v->addr]));              break;
	      case R_BOOL:   xen_symbol_name_set_value(var->name, C_TO_XEN_BOOLEAN(prog->ints[var->v->addr]));          break;
	      case R_STRING: xen_symbol_name_set_value(var->name, C_TO_XEN_STRING((char *)(prog->ints[var->v->addr]))); break;
	      case R_CHAR:   xen_symbol_name_set_value(var->name, C_TO_XEN_CHAR((char)(prog->ints[var->v->addr])));     break;
	      case R_FLOAT_VECTOR:
		val = symbol_to_value(prog->code, C_STRING_TO_XEN_SYMBOL(var->name), &local_var);
		if (XEN_VECTOR_P(val))
		  vct_into_vector((vct *)(prog->ints[var->v->addr]), val);
		break;
	      case R_INT_VECTOR:
		val = symbol_to_value(prog->code, C_STRING_TO_XEN_SYMBOL(var->name), &local_var);
		if (XEN_VECTOR_P(val))
		  int_vct_into_vector((int_vct *)(prog->ints[var->v->addr]), val);
		break;
	      }
	  else
	    switch (var->v->type)
	      {
	      case R_FLOAT:  symbol_set_value(prog->code, C_STRING_TO_XEN_SYMBOL(var->name), C_TO_XEN_DOUBLE(prog->dbls[var->v->addr]));           break;
	      case R_INT:    symbol_set_value(prog->code, C_STRING_TO_XEN_SYMBOL(var->name), C_TO_XEN_INT(prog->ints[var->v->addr]));              break;
	      case R_BOOL:   symbol_set_value(prog->code, C_STRING_TO_XEN_SYMBOL(var->name), C_TO_XEN_BOOLEAN(prog->ints[var->v->addr]));          break;
	      case R_STRING: symbol_set_value(prog->code, C_STRING_TO_XEN_SYMBOL(var->name), C_TO_XEN_STRING((char *)(prog->ints[var->v->addr]))); break;
	      case R_CHAR:   symbol_set_value(prog->code, C_STRING_TO_XEN_SYMBOL(var->name), C_TO_XEN_CHAR((char)(prog->ints[var->v->addr])));     break;
	      case R_FLOAT_VECTOR:
		val = symbol_to_value(prog->code, C_STRING_TO_XEN_SYMBOL(var->name), &local_var);
		if (XEN_VECTOR_P(val))
		  vct_into_vector((vct *)(prog->ints[var->v->addr]), val);
		break;
	      case R_INT_VECTOR:
		val = symbol_to_value(prog->code, C_STRING_TO_XEN_SYMBOL(var->name), &local_var);
		if (XEN_VECTOR_P(val))
		  int_vct_into_vector((int_vct *)(prog->ints[var->v->addr]), val);
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
	      pt->program[i] = free_triple(pt->program[i]);
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
      if (pt->form) snd_unprotect(pt->form);
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
		      if (pt->ints[v->addr])
			{
			  switch (v->type)
			    {
			    case R_VCT:          c_free_vct((vct *)(pt->ints[v->addr]));            break;
			    case R_READER:       free_snd_fd((snd_fd *)(pt->ints[v->addr]));        break;
			    case R_FUNCTION:     free_embedded_ptree((ptree *)(pt->ints[v->addr])); break;
			    case R_CLM:          mus_free((mus_any *)(pt->ints[v->addr]));          break;
			    case R_FLOAT_VECTOR: c_free_vct((vct *)(pt->ints[v->addr]));            break;
			    case R_INT_VECTOR:   free_int_vct((int_vct *)(pt->ints[v->addr]));      break;
			    case R_CLM_VECTOR:   free_clm_vct((clm_vct *)(pt->ints[v->addr]));      break;
			    case R_VCT_VECTOR:   free_vct_vct((vct_vct *)(pt->ints[v->addr]));      break;
			    case R_STRING:       
			      if (pt->ints[v->addr]) 
				{
				  FREE((char *)(pt->ints[v->addr]));
				  pt->ints[v->addr] = 0;
				}
			      break;
			    }
			  pt->ints[v->addr] = 0;
			}
		      v->gc = FALSE;
		    }
		  FREE(v);
		  pt->gcs[i] = NULL;
		}
	    }
	}
      if (pt->strs)
	{
	  for (i = 0; i < pt->str_ctr; i++)
	    if ((pt->strs[i] > 2) && 
		(pt->ints[pt->strs[i]]))
	      {
		FREE((char *)(pt->ints[pt->strs[i]]));
		pt->ints[pt->strs[i]] = 0;
	      }
	  FREE(pt->strs);
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
	      pt->program[i] = free_triple(pt->program[i]);
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
	  pt->program = (triple **)CALLOC(8, sizeof(triple *));
	  pt->program_size = 8;
	}
      else
	{
	  old_size = pt->program_size;
	  pt->program_size += 8;
	  pt->program = (triple **)REALLOC(pt->program, pt->program_size * sizeof(triple *));
	  for (i = old_size; i < pt->program_size; i++) pt->program[i] = NULL;
	}
    }
  pt->program[pt->triple_ctr++] = trp;
  return(trp);
}

static int add_int_to_ptree(ptree *pt, int value)
{
  int cur;
  cur = pt->int_ctr;
  if (cur >= pt->ints_size)
    {
      pt->ints_size += 8;
      pt->ints = (int *)REALLOC(pt->ints, pt->ints_size * sizeof(int));
    }
  pt->ints[pt->int_ctr++] = value;
  return(cur);
}

static int add_dbl_to_ptree(ptree *pt, Float value)
{
  int cur;
  cur = pt->dbl_ctr;
  if (cur >= pt->dbls_size)
    {
      pt->dbls_size += 8;
      pt->dbls = (Float *)REALLOC(pt->dbls, pt->dbls_size * sizeof(Float));
    }
  pt->dbls[pt->dbl_ctr++] = value;
  return(cur);
}

static xen_var *new_xen_var(char *name, xen_value *v)
{
  xen_var *var;
  var = (xen_var *)CALLOC(1, sizeof(xen_var));
  var->name = copy_string(name);
  var->v = copy_xen_value(v);
  var->unclean = FALSE;
  var->global = FALSE;
  var->unsettable = FALSE;
  return(var);
}

static int add_var_to_ptree(ptree *pt, char *name, xen_value *v)
{
  int cur;
  /*
  fprintf(stderr,"add var: %s %s\n", name, describe_xen_value(v, pt->ints, pt->dbls));
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

static int add_outer_var_to_ptree(ptree *pt, char *name, xen_value *v)
{
  int cur;
  xen_var *var;
  cur = pt->global_var_ctr;
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
  var->global = TRUE;
  pt->global_vars[pt->global_var_ctr++] = var;
  return(cur);
}

static int remember_string(ptree *pt, int addr)
{
  int i, cur;
  cur = pt->str_ctr;
  if (cur >= pt->strs_size)
    {
      pt->strs_size += 8;
      if (pt->strs)
	{
	  pt->strs = (int *)REALLOC(pt->strs, pt->strs_size * sizeof(int));
	  for (i = cur; i < pt->strs_size; i++)
	    pt->strs[i] = 0;
	}
      else pt->strs = (int *)CALLOC(pt->strs_size, sizeof(int));
    }
  pt->strs[pt->str_ctr++] = addr;
  return(addr);
}

static int add_string_to_ptree(ptree *pt, char *str)
{
  return(remember_string(pt, add_int_to_ptree(pt, (int)(str))));
}

static xen_value *add_empty_var_to_ptree(ptree *prog, int type)
{
  switch (type)
    {
    case R_FLOAT:  
      return(make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, 0.0), R_VARIABLE)); 
      break;
    case R_STRING: 
      return(make_xen_value(R_STRING, add_string_to_ptree(prog, NULL), R_VARIABLE)); 
      break;
    default:       
      return(make_xen_value(type, add_int_to_ptree(prog, 0), R_VARIABLE)); 
      break;
    }
  return(NULL);
}

static xen_value *transfer_value(ptree *prog, xen_value *v)
{
  switch (v->type)
    {
    case R_FLOAT: 
      return(make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, prog->dbls[v->addr]), R_VARIABLE)); 
      break;
    case R_STRING: 
      return(make_xen_value(R_STRING, add_string_to_ptree(prog, copy_string((char *)(prog->ints[v->addr]))), R_VARIABLE)); 
      break;
    default: 
      return(make_xen_value(v->type, add_int_to_ptree(prog, prog->ints[v->addr]), R_VARIABLE));
      break;
    }
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
  v->gc = TRUE;
  add_xen_value_to_gcs(pt, v);
}

static void add_loc_to_protected_list(ptree *pt, int loc)
{
  int old_size, i;
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

static int_vct *read_int_vector(XEN vect)
{
  int len, i;
  int_vct *v;
  XEN *vdata;
  len = XEN_VECTOR_LENGTH(vect);
  if (len == 0) return(NULL);
  v = (int_vct *)CALLOC(1, sizeof(int_vct));
  v->length = len;
  v->data = (int *)CALLOC(len, sizeof(int));
  vdata = XEN_VECTOR_ELEMENTS(vect);
  for (i = 0; i < len; i++) 
    if (XEN_INTEGER_P(vdata[i]))
      v->data[i] = XEN_TO_C_INT(vdata[i]);
    else
      {
	run_warn("initial int vector value at %d is %s: will try to abort ptree evaluation...", i, XEN_AS_STRING(vdata[i]));
	FREE(v->data);
	FREE(v);
	return(NULL);
      }
  return(v);
}

static vct_vct *read_vct_vector(XEN vect)
{
  int len, i;
  vct_vct *v;
  XEN *vdata;
  len = XEN_VECTOR_LENGTH(vect);
  if (len == 0) return(NULL);
  v = (vct_vct *)CALLOC(1, sizeof(vct_vct));
  v->length = len;
  v->data = (vct **)CALLOC(len, sizeof(vct *));
  vdata = XEN_VECTOR_ELEMENTS(vect);
  for (i = 0; i < len; i++) 
    if (VCT_P(vdata[i]))
      v->data[i] = get_vct(vdata[i]);
    else
      {
	run_warn("initial vct vector value at %d is %s: will try to abort ptree evaluation...", i, XEN_AS_STRING(vdata[i]));
	FREE(v->data);
	FREE(v);
	return(NULL);
      }
  return(v);
}

static clm_vct *read_clm_vector(XEN vect)
{
  int len, i;
  clm_vct *v;
  XEN *vdata;
  len = XEN_VECTOR_LENGTH(vect);
  if (len == 0) return(NULL);
  v = (clm_vct *)CALLOC(1, sizeof(clm_vct));
  v->length = len;
  v->data = (mus_any **)CALLOC(len, sizeof(mus_any *));
  vdata = XEN_VECTOR_ELEMENTS(vect);
  for (i = 0; i < len; i++) 
    if (mus_xen_p(vdata[i]))
      v->data[i] = XEN_TO_MUS_ANY(vdata[i]);
    else
      {
	if (!(XEN_FALSE_P(vdata[i])))
	  {
	    run_warn("initial clm vector value at %d is %s: will try to abort ptree evaluation...", i, XEN_AS_STRING(vdata[i]));
	    FREE(v->data);
	    FREE(v);
	    return(NULL);
	  }
      }
  return(v);
}

static int xen_to_run_type(XEN val)
{
  if (XEN_NUMBER_P(val))
    {
      if ((XEN_EXACT_P(val)) && (XEN_INTEGER_P(val)))
	return(R_INT);
      else return(R_FLOAT);
    }
  else
    {
      if (XEN_BOOLEAN_P(val)) return(R_BOOL); else
	if (VCT_P(val)) return(R_VCT); else
	  if (sf_p(val)) return(R_READER); else
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
  int var_loc = 0, local_var = FALSE, type;
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
  /* fprintf(stderr,"add global %s %s %s\n",varname, type_name(type), XEN_AS_STRING(val)); */
  switch (type)
    {
    case R_INT:    v = make_xen_value(R_INT, add_int_to_ptree(prog, XEN_TO_C_INT(val)), R_VARIABLE); break;
    case R_FLOAT:  v = make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, XEN_TO_C_DOUBLE(val)), R_VARIABLE); break;
    case R_BOOL:   v = make_xen_value(R_BOOL, add_int_to_ptree(prog, XEN_TO_C_BOOLEAN(val)), R_VARIABLE); break;
    case R_VCT:    v = make_xen_value(R_VCT, add_int_to_ptree(prog, (int)(get_vct(val))), R_VARIABLE); break;
    case R_READER: v = make_xen_value(R_READER, add_int_to_ptree(prog, (int)(get_sf(val))), R_VARIABLE); break;
    case R_CLM:
      if (prog->need_result == NEED_XCLM_RESULT)
	v = make_xen_value(R_XCLM, add_int_to_ptree(prog, (int)(XEN_TO_MUS_XEN(val))), R_VARIABLE);
      else v = make_xen_value(R_CLM, add_int_to_ptree(prog, (int)(XEN_TO_MUS_ANY(val))), R_VARIABLE);
      break;
    case R_CHAR:   v = make_xen_value(R_CHAR, add_int_to_ptree(prog, (int)(XEN_TO_C_CHAR(val))), R_VARIABLE); break;
    case R_STRING: v = make_xen_value(R_STRING, add_string_to_ptree(prog, copy_string(XEN_TO_C_STRING(val))), R_VARIABLE); break;
    case R_LIST:   v = make_xen_value(R_LIST, add_int_to_ptree(prog, (int)val), R_VARIABLE); break;
    case R_PAIR:   v = make_xen_value(R_PAIR, add_int_to_ptree(prog, (int)val), R_VARIABLE); break;
    case R_INT_VECTOR: 
      {
	int_vct *iv;
	iv = read_int_vector(val);
	if (iv == NULL) return(NULL);
	v = make_xen_value(R_INT_VECTOR, add_int_to_ptree(prog, (int)iv), R_VARIABLE);
	if (v) add_obj_to_gcs(prog, R_INT_VECTOR, v->addr); /* this makes its own xen_value with TRUE gc field */
      }
      break;
    case R_FLOAT_VECTOR:
      {
	vct *vc;
	vc = vector_to_vct(val);
	if (vc == NULL) 
	  return(run_warn("trouble in float vector -- will try to abort ptree evaluation"));
	v = make_xen_value(R_FLOAT_VECTOR, add_int_to_ptree(prog, (int)vc), R_VARIABLE);
	if (v) add_obj_to_gcs(prog, R_FLOAT_VECTOR, v->addr);
      }
      break;
    case R_VCT_VECTOR:
      {
	vct_vct *vv;
	vv = read_vct_vector(val);
	if (vv == NULL) return(NULL);
	v = make_xen_value(R_VCT_VECTOR, add_int_to_ptree(prog, (int)vv), R_VARIABLE);
	if (v) add_obj_to_gcs(prog, R_VCT_VECTOR, v->addr);
      }
      break;
    case R_CLM_VECTOR:
      {
	clm_vct *cv;
	cv = read_clm_vector(val);
	if (cv == NULL) return(NULL);
	v = make_xen_value(R_CLM_VECTOR, add_int_to_ptree(prog, (int)cv), R_VARIABLE);
	if (v) add_obj_to_gcs(prog, R_CLM_VECTOR, v->addr);
      }
      break;
    default:
      if (type > R_ANY)
	v = make_xen_value(type, add_int_to_ptree(prog, (int)val), R_VARIABLE); 
      break;
    }
  if (v)
    {
      var_loc = add_outer_var_to_ptree(prog, varname, v);
      if ((v->type == R_LIST) || (v->type == R_PAIR)) /* lists aren't settable yet */
	prog->global_vars[var_loc]->unsettable = TRUE;
      else
	{
	  if (current_optimization < GLOBAL_SET_OK)
	    prog->global_vars[var_loc]->unsettable = local_var;
	}
    }
  return(v);
}

static continuation *add_goto_to_ptree(ptree *pt, char *name)
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

static void erase_goto(ptree *prog, char *name)
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

#define PC ints[0]
#define ALL_DONE ints[1]
#define PTREE ((ptree *)(ints[2]))

static void eval_ptree(ptree *prog)
{
  /* evaluates program, result in prog->result, 
   *   assumes args already passed in addrs given by prog->addrs
   */
  triple *curfunc;
  int *ints;
  Float *dbls;
  ints = prog->ints;
  dbls = prog->dbls;
  ALL_DONE = FALSE;
  /* evaluate the parse tree */
  while (!ALL_DONE)
    {
#if 0
      {
	char *buf;
	buf = describe_ptree(prog);
	fprintf(stderr, buf);
	FREE(buf);
      }
#endif
      curfunc = prog->program[PC++];
      (*(curfunc->function))(curfunc->args, ints, dbls);
#if 0
      {
	char *buf;
	buf = (*(curfunc->descr))(curfunc->args, ints, dbls);
	fprintf(stderr, "%d: %s\n", PC, buf);
	FREE(buf);
      }
#endif
    }
  PC = prog->initial_pc; /* don't reset closure junk after initial evaluation */
}

static void eval_embedded_ptree(ptree *prog, int *ints, Float *dbls)
{
  int old_pc, old_all_done, old_ptree;
  old_pc = ints[0];
  old_all_done = ints[1];
  old_ptree = ints[2];
  ints[0] = 0;
  ints[1] = FALSE;
  ints[2] = (int)prog;
  prog->ints = ints;
  prog->dbls = dbls;
  eval_ptree(prog);
  prog->ints = NULL;
  prog->dbls = NULL;
  ints[0] = old_pc;
  ints[1] = old_all_done;
  ints[2] = old_ptree;
}

static triple *make_triple(void (*function)(int *arg_addrs, int *ints, Float *dbls),
			   char *(*descr)(int *arg_addrs, int *ints, Float *dbls), 
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

static triple *va_make_triple(void (*function)(int *arg_addrs, int *ints, Float *dbls), 
			      char *(*descr)(int *arg_addrs, int *ints, Float *dbls), 
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

#define BOOL_RESULT ints[args[0]]
#define FLOAT_RESULT dbls[args[0]]
#define INT_RESULT ints[args[0]]
#if (!(defined(__GNUC__))) || (defined(__cplusplus))
  #define STRING_RESULT (*((char **)(&(ints[args[0]]))))
  #define CHAR_RESULT (*((char *)(&(ints[args[0]]))))
  #define CLM_RESULT (*((mus_any **)(&(ints[args[0]]))))
  #define VCT_RESULT (*((vct **)(&(ints[args[0]]))))
#else
  #define STRING_RESULT ((char *)(ints[args[0]]))
  #define CHAR_RESULT ((char)(ints[args[0]]))
  #define CLM_RESULT ((mus_any *)(ints[args[0]]))
  #define VCT_RESULT ((vct *)(ints[args[0]]))
#endif
#define BOOL_ARG_1 ints[args[1]]
#define BOOL_ARG_2 ints[args[2]]
#define BOOL_ARG_3 ints[args[3]]
#define INT_ARG_1 ints[args[1]]
#define INT_ARG_2 ints[args[2]]
#define INT_ARG_3 ints[args[3]]
#define INT_ARG_4 ints[args[4]]
#define INT_ARG_5 ints[args[5]]
#define FLOAT_ARG_1 dbls[args[1]]
#define FLOAT_ARG_2 dbls[args[2]]
#define FLOAT_ARG_3 dbls[args[3]]
#define FLOAT_ARG_4 dbls[args[4]]
#define VCT_ARG_1 (vct *)(ints[args[1]])
#define VCT_ARG_2 (vct *)(ints[args[2]])
#define STRING_ARG_1 ((char *)(ints[args[1]]))
#define STRING_ARG_2 ((char *)(ints[args[2]]))
#define CHAR_ARG_1 ((char)(ints[args[1]]))
#define CHAR_ARG_2 ((char)(ints[args[2]]))

static void quit(int *args, int *ints, Float *dbls) {ALL_DONE = TRUE;}
static char *descr_quit(int *args, int *ints, Float *dbls) {return(copy_string("quit"));}

static void jump(int *args, int *ints, Float *dbls) {PC += ints[args[0]];}
static char *descr_jump(int *args, int *ints, Float *dbls) {return(mus_format("jump " INT_PT , args[0], INT_RESULT));}

static void jump_abs(int *args, int *ints, Float *dbls) {PC = ints[args[0]];}
static char *descr_jump_abs(int *args, int *ints, Float *dbls) {return(mus_format("goto " INT_PT , args[0], INT_RESULT));}

static void jump_indirect(int *args, int *ints, Float *dbls) {PC = ints[ints[args[0]]];}
static char *descr_jump_indirect(int *args, int *ints, Float *dbls) {return(mus_format("goto [" INT_PT "=%d]", args[0], INT_RESULT, ints[ints[args[0]]]));}

static void jump_if(int *args, int *ints, Float *dbls) {if (ints[args[1]] != 0) PC += ints[args[0]];}
static char *descr_jump_if(int *args, int *ints, Float *dbls) 
{
  return(mus_format("if (" BOOL_PT ") jump " INT_PT , args[1], B2S(INT_ARG_1), args[0], INT_RESULT));
}

static void jump_if_equal(int *args, int *ints, Float *dbls) {if (ints[args[1]] == ints[args[2]]) PC = ints[args[0]];}
static char *descr_jump_if_equal(int *args, int *ints, Float *dbls) 
{
  return(mus_format("if (" INT_PT " == " INT_PT ") goto " INT_PT , args[1], INT_ARG_1, args[2], INT_ARG_2, args[0], INT_RESULT));
}

static void jump_if_not(int *args, int *ints, Float *dbls) {if (ints[args[1]] == 0) PC += ints[args[0]];}
static char *descr_jump_if_not(int *args, int *ints, Float *dbls) 
{
  return(mus_format("if (!" BOOL_PT ") jump " INT_PT , args[1], B2S(INT_ARG_1), args[0], INT_RESULT));
}

static void store_i(int *args, int *ints, Float *dbls) {INT_RESULT = INT_ARG_1;}
static char *descr_store_i(int *args, int *ints, Float *dbls) {return(mus_format( INT_PT " = " INT_PT , args[0], INT_RESULT, args[1], INT_ARG_1));}

static void store_f(int *args, int *ints, Float *dbls) {FLOAT_RESULT = FLOAT_ARG_1;}
static char *descr_store_f(int *args, int *ints, Float *dbls) {return(mus_format( FLT_PT " = " FLT_PT , args[0], FLOAT_RESULT, args[1], FLOAT_ARG_1));}

static void store_f_i(int *args, int *ints, Float *dbls) {INT_RESULT = (int)FLOAT_ARG_1;}
static char *descr_store_f_i(int *args, int *ints, Float *dbls) {return(mus_format( INT_PT " = " FLT_PT , args[0], INT_RESULT, args[1], FLOAT_ARG_1));}

static void store_i_f(int *args, int *ints, Float *dbls) {FLOAT_RESULT = (Float)INT_ARG_1;}
static char *descr_store_i_f(int *args, int *ints, Float *dbls) {return(mus_format( FLT_PT " = " INT_PT , args[0], FLOAT_RESULT, args[1], INT_ARG_1));}

static void store_false(int *args, int *ints, Float *dbls) {BOOL_RESULT = FALSE;}
static char *descr_store_false(int *args, int *ints, Float *dbls) {return(mus_format( BOOL_PT " = 0", args[0], B2S(BOOL_RESULT)));}

static void store_true(int *args, int *ints, Float *dbls) {BOOL_RESULT = TRUE;}
static char *descr_store_true(int *args, int *ints, Float *dbls) {return(mus_format( BOOL_PT " = 1", args[0], B2S(BOOL_RESULT)));}

static void store_s(int *args, int *ints, Float *dbls) 
{
  if (STRING_RESULT) FREE(STRING_RESULT);
  STRING_RESULT = copy_string(STRING_ARG_1);
}
static char *descr_store_s(int *args, int *ints, Float *dbls) 
{
  return(mus_format( STR_PT " = " STR_PT , args[0], STRING_RESULT, args[1], STRING_ARG_1));
}

static xen_value *convert_int_to_dbl(ptree *prog, xen_value *i)
{
  xen_value *val;
  val = make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, 0.0), i->constant);
  if (i->constant == R_CONSTANT)
    prog->dbls[val->addr] = (Float)(prog->ints[i->addr]);
  else add_triple_to_ptree(prog, va_make_triple(store_i_f, descr_store_i_f, 2, val, i));
  return(val);
}

static xen_value *convert_dbl_to_int(ptree *prog, xen_value *i, int exact)
{
  xen_value *v;
  Float val;
  val = prog->dbls[i->addr];
  if ((exact) && ((int)(floor(val)) != (int)val)) return(NULL);
  v = make_xen_value(R_INT, add_int_to_ptree(prog, 0), i->constant);
  if (i->constant == R_CONSTANT)
    prog->ints[v->addr] = (int)val;
  else add_triple_to_ptree(prog, va_make_triple(store_f_i, descr_store_f_i, 2, v, i));
  return(v);
}

static triple *set_var(ptree *pt, xen_value *var, xen_value *init_val)
{
  if (var->type == R_FLOAT)
    return(add_triple_to_ptree(pt, va_make_triple(store_f, descr_store_f, 2, var, init_val)));
  else
    {
      if (var->type == R_STRING)
	return(add_triple_to_ptree(pt, va_make_triple(store_s, descr_store_s, 2, var, init_val)));
      else return(add_triple_to_ptree(pt, va_make_triple(store_i, descr_store_i, 2, var, init_val)));
    }
  return(NULL);
}

static xen_value *walk(ptree *prog, XEN form, int need_result);

static xen_value *walk_sequence(ptree *prog, XEN body, int need_result, char *name)
{
  xen_value *v;
  int i, body_forms;
  XEN lbody;
  v = NULL;
  if (XEN_NOT_BOUND_P(body)) return(NULL);
  body_forms = XEN_LIST_LENGTH(body);
  if (body_forms == 0) 
    return(make_xen_value(R_BOOL, add_int_to_ptree(prog, FALSE), R_CONSTANT));
  lbody = body;
  for (i = 0; i < body_forms; i++, lbody = XEN_CDR(lbody))
    {
      if (v) FREE(v);
      v = walk(prog, XEN_CAR(lbody), ((need_result) && (i == (body_forms - 1))));
      if (v == NULL) return(run_warn("%s: can't handle %s", name, XEN_AS_STRING(XEN_CAR(lbody))));
    }
  return(v);
}

static xen_value *lambda_form(ptree *prog, XEN form, int separate, xen_value **args, int num_args);

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
	  v = lambda_form(prog, form, FALSE, NULL, 0);
	  if (v == NULL) return(mus_format("can't handle this define: %s", XEN_AS_STRING(form)));
	  add_var_to_ptree(prog, XEN_SYMBOL_TO_C_STRING(XEN_CAR(var)), v);
	  FREE(v);
	  return(NULL);
	}
      else return(mus_format("can't handle this definition: %s", XEN_AS_STRING(var)));
    }
  val = XEN_CADDR(form);
  v = walk(prog, val, TRUE);
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
	  v = walk(prog, XEN_CADR(var), TRUE);
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
	  /* in case called in loop with set! on locals, need to restore upon re-entry */
	  set_var(prog, vs[i], old_vs[i]);
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
	  v = walk(prog, XEN_CADR(var), TRUE);
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

static char *declare_args(ptree *prog, XEN form, int default_arg_type, int separate, xen_value **cur_args, int num_args)
{
  XEN arg, args, declarations = XEN_FALSE, declaration;
  xen_value *v = NULL;
  int i, arg_num, arg_type;
  char *type;
  if (separate)
    args = XEN_CADR(form);
  else args = XEN_CDADR(form);
  if (!(XEN_LIST_P(args))) return(mus_format("can't handle non-explicit lambda args: %s", XEN_AS_STRING(args)));
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
		  if (arg_type == R_UNSPECIFIED)
		    if (strcmp(type, "integer") == 0) arg_type = R_INT; else
		      if (strcmp(type, "real") == 0) arg_type = R_FLOAT; else
			arg_type = default_arg_type;
		}
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

static xen_value *walk_then_undefine(ptree *prog, XEN form, int need_result, char *name, int locals_loc)
{
  xen_value *v;
  v = walk_sequence(prog, handle_defines(prog, form), need_result, name);
  undefine_locals(prog, locals_loc);
  return(v);
}

static xen_value *lambda_form(ptree *prog, XEN form, int separate, xen_value **args, int num_args)
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
      new_tree->code = XEN_FALSE;
      outer_locals = prog->var_ctr;
      err = declare_args(new_tree, form, R_INT, separate, args, num_args);
      if (err)
	{
	  run_warn(err);
	  FREE(err);
	}
      else
	{
	  new_tree->result = walk_then_undefine(new_tree, XEN_CDDR(form), TRUE, "lambda", prog->var_ctr);
	  if (new_tree->result)
	    {
	      xen_value *v;
	      add_triple_to_ptree(new_tree, make_triple(quit, descr_quit, NULL, 0));
	      unattach_ptree(new_tree, prog);
	      for (i = outer_locals; i < prog->var_ctr; i++)
		if (prog->vars[i]) prog->vars[i] = free_xen_var(prog, prog->vars[i]);
	      prog->var_ctr = outer_locals;
	      v = make_xen_value(R_FUNCTION, add_int_to_ptree(prog, (int)new_tree), R_VARIABLE);
	      add_obj_to_gcs(prog, R_FUNCTION, v->addr);
	      return(v);
	    }
	}
      /* we get here if walk failed, so clean up */
      unattach_ptree(new_tree, prog); /* needed in case we realloc'd during failed tree walk */
      free_embedded_ptree(new_tree);
      return(run_warn("can't handle this embedded lambda: %s", XEN_AS_STRING(form)));
    }
  got_lambda = TRUE;
  locals_loc = prog->var_ctr;
  err = declare_args(prog, form, R_FLOAT, separate, args, num_args);
  if (err) return(run_warn_with_free(err));
  return(walk_then_undefine(prog, XEN_CDDR(form), TRUE, "lambda", locals_loc));
}

static xen_value *begin_form(ptree *prog, XEN form, int need_result)
{
  return(walk_then_undefine(prog, XEN_CDR(form), need_result, "begin", prog->var_ctr));
}

static xen_value *let_star_form(ptree *prog, XEN form, int need_result)
{
  int locals_loc;
  char *err = NULL;
  locals_loc = prog->var_ctr; /* lets can be nested */
  err = sequential_binds(prog, XEN_CADR(form), "let*");
  if (err) return(run_warn_with_free(err));
  if (!got_lambda) prog->initial_pc = prog->triple_ctr;
  return(walk_then_undefine(prog, XEN_CDDR(form), need_result, "let*", locals_loc));
}

static xen_value *let_form(ptree *prog, XEN form, int need_result)
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

static xen_value *if_form(ptree *prog, XEN form, int need_result)
{
  /* form: (if selector true [false]) */
  xen_value *result = NULL, *true_result = NULL, *false_result = NULL;
  xen_value *jump_to_end = NULL, *jump_to_false, *if_value;
  int current_pc, false_pc = 0, has_false;
  has_false = (XEN_LIST_LENGTH(form) == 4);
  if_value = walk(prog, XEN_CADR(form), TRUE);                                      /* walk selector */
  if (if_value == NULL) return(run_warn("if: bad selector? %s", XEN_AS_STRING(XEN_CADR(form))));
  if (if_value->type != R_BOOL) 
    return(run_warn("if: selector type not boolean: %s %s", 
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
  add_triple_to_ptree(prog, va_make_triple(jump_if_not, descr_jump_if_not, 2, jump_to_false, if_value));
  FREE(if_value);
  true_result = walk(prog, XEN_CADDR(form), need_result);                           /* walk true branch */
  if (true_result == NULL) 
    {
      FREE(jump_to_false);
      return(run_warn("if: can't handle true branch %s", XEN_AS_STRING(XEN_CADDR(form))));
    }
  if (need_result)
    {
      result = add_empty_var_to_ptree(prog, true_result->type);
      set_var(prog, result, true_result);
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
      if (need_result)
	{
	  if ((false_result) && (false_result->type != true_result->type))
	    {
	      /* #f is ok as null pointer so fixup if needed */
	      if ((false_result->type == R_BOOL) &&
		  ((true_result->type == R_CLM) || (true_result->type == R_READER)))
		false_result->type = true_result->type;
	      else
		if ((true_result->type == R_BOOL) &&
		    ((false_result->type == R_CLM) || (false_result->type == R_READER)))
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
	  set_var(prog, result, false_result);
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

static xen_value *cond_form(ptree *prog, XEN form, int need_result)
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
	test_value = make_xen_value(R_BOOL, add_int_to_ptree(prog, TRUE), R_CONSTANT);
      else test_value = walk(prog, XEN_CAR(clause), TRUE);
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
      if (need_result)
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
  if (need_result)
    return(result);
  return(make_xen_value(R_UNSPECIFIED, -1, R_CONSTANT));
}

static xen_value *case_form(ptree *prog, XEN form, int need_result)
{
  /* only int (constant) selectors here */
  /* (case selector ((...) exp ...) ...) with possible 'else' */
  XEN selector, body, keys, key;
  int i, j, body_len, num_keys, cur_key;
  xen_value *selval = NULL, *jump_to_selection = NULL, *v = NULL, *result = NULL, *keyval = NULL, *locval = NULL, *elseval = NULL;
  int *locations = NULL;
  xen_value **fixups = NULL;
  selector = XEN_CADR(form);
  selval = walk(prog, selector, TRUE);
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
      if (need_result)
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
	      if (!(XEN_INTEGER_P(key)))
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
  if (need_result)
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

static int list_member(XEN symb, XEN varlst)
{
  XEN lst;
  for (lst = varlst; (XEN_NOT_NULL_P(lst)); lst = XEN_CDR(lst))
    if (XEN_EQ_P(symb, XEN_CAR(lst)))
      return(TRUE);
  return(FALSE);
}

static int tree_member(XEN varlst, XEN expr)
{
  /* is any member of varlst found in expr */
  /* search expr (assumed to be a list here) for reference to any member of varlst */
  XEN symb;
  if (XEN_NULL_P(expr)) return(FALSE);
  symb = XEN_CAR(expr);
  if (XEN_SYMBOL_P(symb))
    {
      if (list_member(symb, varlst))
	return(TRUE);
    }
  else
    {
      if ((XEN_LIST_P(symb)) && (tree_member(varlst, symb)))
	return(TRUE);
    }
  return(tree_member(varlst, XEN_CDR(expr)));
}

static xen_value *do_form_1(ptree *prog, XEN form, int need_result, int sequential)
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
  if (sequential)
    trouble = sequential_binds(prog, vars, "do*");
  else trouble = parallel_binds(prog, vars, "do");
  if (trouble) return(run_warn_with_free(trouble));

  test_loc = prog->triple_ctr;
  test = walk(prog, test_form, TRUE);
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
  expr = walk_sequence(prog, body, FALSE, "do");
  if (expr == NULL)
    {
      FREE(jump_to_result);
      return(NULL);
    }
  FREE(expr);
  expr = NULL;
  /* now increment the vars (if step-val exists) -- increments are done first if not sequential (the norm in Scheme) */
  varlen = XEN_LIST_LENGTH(vars);
  if (varlen > 0)
    {
      if (!sequential)
	{
	  /* here we can optimize better if sequential is true, so look for such cases */
	  sequential = TRUE; /* assume success */
	  if (varlen > 1)    /* 0=doesn't matter, 1=no possible non-sequential ref */
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
			  /* fprintf(stderr,"found seq ref %s\n", XEN_AS_STRING(vars)); */
			  sequential = FALSE;
			  break;
			}
		    }
		  varlst = XEN_CONS(XEN_CAR(var), varlst);
		}
	      snd_unprotect_at(loc);
	      if (!sequential)
		exprs = (xen_value **)CALLOC(varlen, sizeof(xen_value *));
	    }
	}
      for (vars = XEN_CADR(form), i = 0; i < varlen; i++, vars = XEN_CDR(vars))
	{
	  var = XEN_CAR(vars);
	  if ((XEN_NOT_NULL_P(XEN_CDDR(var))) && (XEN_NOT_NULL_P(XEN_CADDR(var))))
	    {
	      if ((sequential) && (expr)) FREE(expr);
	      expr = walk(prog, XEN_CADDR(var), TRUE);
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
		      addrs = prog->program[prog->triple_ctr - 1]->args;
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
    result = walk_sequence(prog, results, TRUE, "do");
  else result = make_xen_value(R_BOOL, add_int_to_ptree(prog, FALSE), R_CONSTANT);
  undefine_locals(prog, locals_loc);
  return(result);
}

static xen_value *do_form(ptree *prog, XEN form, int need_result) {return(do_form_1(prog, form, need_result, FALSE));}
static xen_value *do_star_form(ptree *prog, XEN form, int need_result) {return(do_form_1(prog, form, need_result, TRUE));}

static xen_value *callcc_form(ptree *prog, XEN form, int need_result)
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
      if (need_result)
	set_var(prog, c->result, v);
    }
  if (v) FREE(v);
  /* fixup the continuation jump, etc */
  prog->ints[c->jump->addr] = prog->triple_ctr;
  if (c->result)
    v = copy_xen_value(c->result);
  else v = make_xen_value(R_BOOL, add_int_to_ptree(prog, FALSE), R_CONSTANT);
  erase_goto(prog, c->name); /* now access to this depends on a set! somewhere */
  return(v);
}

static xen_value *or_form(ptree *prog, XEN form, int ignored)
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
      v = walk(prog, XEN_CAR(body), TRUE);
      if ((v == NULL) || ((v->type != R_BOOL) && (!(POINTER_OR_GOTO_P(v->type)))))
	{
	  int was_null;
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

static xen_value *and_form(ptree *prog, XEN form, int ignored)
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
      v = walk(prog, XEN_CAR(body), TRUE);
      if ((v == NULL) || ((v->type != R_BOOL) && (!(POINTER_OR_GOTO_P(v->type)))))
	{
	  int was_null;
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

static xen_value *generalized_set_form(ptree *prog, XEN form, int need_result)
{
  /* (set! (mus-phase gen) 0.0) */
  XEN settee, setval;
  XEN in_settee;
  xen_value *in_v0 = NULL, *in_v1 = NULL, *in_v2 = NULL, *v = NULL;
  settee = XEN_CADR(form);
  setval = XEN_CADDR(form);
  if ((XEN_LIST_P(settee)) && (XEN_SYMBOL_P(XEN_CAR(settee))) && (XEN_LIST_LENGTH(settee) <= 4))
    {
      v = walk(prog, setval, TRUE);
      if (v == NULL)
	return(run_warn("set!: can't handle: %s", XEN_AS_STRING(setval)));
      in_settee = XEN_CAR(settee);
      if (XEN_NOT_NULL_P(XEN_CDR(settee)))
	{
	  in_v0 = walk(prog, XEN_CADR(settee), TRUE);
	  if ((in_v0) &&
	      (in_v0->type != R_UNSPECIFIED) &&
	      (!(in_v0->constant)))
	    {
	      if (XEN_NOT_NULL_P(XEN_CDDR(settee)))
		{
		  in_v1 = walk(prog, XEN_CADDR(settee), TRUE);
		  if ((in_v1) &&
		      (in_v1->type != R_UNSPECIFIED))
		    {
		      if (XEN_NOT_NULL_P(XEN_CDDDR(settee)))
			{
			  in_v2 = walk(prog, XEN_CADDDR(settee), TRUE);
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

static xen_value *set_form(ptree *prog, XEN form, int need_result)
{
  char *varname;
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
      v = walk(prog, setval, TRUE);
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
	      var->unclean = TRUE;
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
      if (((v->type == R_FLOAT) || (v->type == R_INT)) &&
	  (XEN_LIST_P(setval)) && /* this by itself assumes that any list represents an expression (i.e. a temp in effect)
				   *   but clm-def-struct field references are lists that can be the target of a set
				   *   so we set the no_opt flag when producing the set
				   */
	  (prog->triple_ctr > 0) &&
	  (prog->program[prog->triple_ctr - 1]->no_opt == FALSE))
	{
	  /* if possible, simply redirect the previous store to us (implicit set) */
	  /* (run '(let ((a 2) (b 1)) (set! a (+ b 1)))) */
	  int *addrs;
	  addrs = prog->program[prog->triple_ctr - 1]->args;
	  if ((addrs) && 
	      (addrs[0] == v->addr))
	    {
	      addrs[0] = var->v->addr; /* redirect the store to us */
	      var->unclean = TRUE;
	      FREE(v);
	      return(copy_xen_value(var->v));
	    }
	}
      set_var(prog, var->v, v);
      var->unclean = TRUE;
      return(v);
    }
  if ((var) && (var->unsettable))
    return(run_warn("set!: can't set local or list vars: %s", XEN_AS_STRING(settee)));
  return(run_warn("set! variable problem: %s", XEN_AS_STRING(form)));
}

static xen_value *package(ptree *prog,
			  int type, 
			  void (*function)(int *arg_addrs, int *ints, Float *dbls),
			  char *(*descr)(int *arg_addrs, int *ints, Float *dbls),
			  xen_value **args,
			  int num_args)
{
  args[0] = add_empty_var_to_ptree(prog, type);
  add_triple_to_ptree(prog, make_triple(function, descr, args, num_args + 1));
  return(args[0]);
}

static xen_value *package_n(ptree *prog,
			    int type, 
			    void (*function)(int *arg_addrs, int *ints, Float *dbls),
			    char *(*descr)(int *arg_addrs, int *ints, Float *dbls),
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

static char *describe_dbl_args(char *func, int num_args, int *args, Float *dbls, int start)
{
  char *buf, *str;
  int i;
  buf = (char *)CALLOC(256, sizeof(char));
  str = (char *)CALLOC(32, sizeof(char));
  sprintf(buf, FLT_PT " =", args[0], FLOAT_RESULT);
  for (i = start; i < num_args; i++)
    {
      mus_snprintf(str, 32, " " FLT_PT " %s", args[i], dbls[args[i]], func);
      strcat(buf, str);
    }
  mus_snprintf(str, 32, " " FLT_PT ")", args[num_args], dbls[args[num_args]]);
  strcat(buf, str);
  FREE(str);
  return(buf);
}

static char *describe_int_args(char *func, int num_args, int *args, int *ints, int start)
{
  char *buf, *str;
  int i;
  buf = (char *)CALLOC(256, sizeof(char));
  str = (char *)CALLOC(32, sizeof(char));
  sprintf(buf, INT_PT " =", args[0], INT_RESULT);
  for (i = start; i < num_args; i++)
    {
      mus_snprintf(str, 32, " " INT_PT " %s", args[i], ints[args[i]], func);
      strcat(buf, str);
    }
  mus_snprintf(str, 32, " " INT_PT ")", args[num_args], ints[args[num_args]]);
  strcat(buf, str);
  FREE(str);
  return(buf);
}

static int float_all_args(ptree *prog, int num_args, xen_value **args, int float_result)
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

static void multiply_f2(int *args, int *ints, Float *dbls) {FLOAT_RESULT = (FLOAT_ARG_1 * FLOAT_ARG_2);}
static char *descr_multiply_f2(int *args, int *ints, Float *dbls) {return(describe_dbl_args("*", 2, args, dbls, 1));}

static void multiply_f3(int *args, int *ints, Float *dbls) {FLOAT_RESULT = (FLOAT_ARG_1 * FLOAT_ARG_2 * FLOAT_ARG_3);}
static char *descr_multiply_f3(int *args, int *ints, Float *dbls) {return(describe_dbl_args("*", 3, args, dbls, 1));}

static void multiply_f4(int *args, int *ints, Float *dbls) {FLOAT_RESULT = (FLOAT_ARG_1 * FLOAT_ARG_2 * FLOAT_ARG_3 * FLOAT_ARG_4);}
static char *descr_multiply_f4(int *args, int *ints, Float *dbls) {return(describe_dbl_args("*", 4, args, dbls, 1));}

static void multiply_fn(int *args, int *ints, Float *dbls) 
{
  int i, n;
  n = ints[args[1]];
  FLOAT_RESULT = FLOAT_ARG_2;
  for (i = 1; i < n; i++) FLOAT_RESULT *= dbls[args[i + 2]];
}
static char *descr_multiply_fn(int *args, int *ints, Float *dbls) {return(describe_dbl_args("*", ints[args[1]] + 1, args, dbls, 2));}

static void multiply_i2(int *args, int *ints, Float *dbls) {INT_RESULT = (INT_ARG_1 * INT_ARG_2);}
static char *descr_multiply_i2(int *args, int *ints, Float *dbls) {return(describe_int_args("*", 2, args, ints, 1));}

static void multiply_i3(int *args, int *ints, Float *dbls) {INT_RESULT = (INT_ARG_1 * INT_ARG_2 * INT_ARG_3);}
static char *descr_multiply_i3(int *args, int *ints, Float *dbls) {return(describe_int_args("*", 3, args, ints, 1));}

static void multiply_i4(int *args, int *ints, Float *dbls) {INT_RESULT = (INT_ARG_1 * INT_ARG_2 * INT_ARG_3 * INT_ARG_4);}
static char *descr_multiply_i4(int *args, int *ints, Float *dbls) {return(describe_int_args("*", 4, args, ints, 1));}

static void multiply_in(int *args, int *ints, Float *dbls)
{
  int i, n;
  n = ints[args[1]];
  INT_RESULT = ints[args[2]];
  for (i = 1; i < n; i++) INT_RESULT *= ints[args[i + 2]];
}

static char *descr_multiply_in(int *args, int *ints, Float *dbls) {return(describe_int_args("*", ints[args[1]] + 1, args, ints, 2));}

static xen_value *multiply(ptree *prog, xen_value **args, int num_args)
{
  int iscl = 1, cons_loc = 0;
  Float fscl = 1.0;
  int i;
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
  else
    {
      if (num_args == 2) return(package(prog, R_INT, multiply_i2, descr_multiply_i2, args, num_args)); 
      if (num_args == 3) return(package(prog, R_INT, multiply_i3, descr_multiply_i3, args, num_args));
      if (num_args == 4) return(package(prog, R_INT, multiply_i4, descr_multiply_i4, args, num_args));
      return(package_n(prog, R_INT, multiply_in, descr_multiply_in, args, num_args));
    }
  return(run_warn("* trouble"));
}


/* ---------------- add ---------------- */

static void add_f2(int *args, int *ints, Float *dbls) {FLOAT_RESULT = (FLOAT_ARG_1 + FLOAT_ARG_2);}
static char *descr_add_f2(int *args, int *ints, Float *dbls) {return(describe_dbl_args("+", 2, args, dbls, 1));}

static void add_f3(int *args, int *ints, Float *dbls) {FLOAT_RESULT = (FLOAT_ARG_1 + FLOAT_ARG_2 + FLOAT_ARG_3);}
static char *descr_add_f3(int *args, int *ints, Float *dbls) {return(describe_dbl_args("+", 3, args, dbls, 1));}

static void add_f4(int *args, int *ints, Float *dbls) {FLOAT_RESULT = (FLOAT_ARG_1 + FLOAT_ARG_2 + FLOAT_ARG_3 + FLOAT_ARG_4);}
static char *descr_add_f4(int *args, int *ints, Float *dbls) {return(describe_dbl_args("+", 4, args, dbls, 1));}

static void add_fn(int *args, int *ints, Float *dbls) 
{
  int i, n;
  n = ints[args[1]];
  FLOAT_RESULT = FLOAT_ARG_2;
  for (i = 1; i < n; i++) FLOAT_RESULT += dbls[args[i + 2]];
}
static char *descr_add_fn(int *args, int *ints, Float *dbls) {return(describe_dbl_args("+", ints[args[1]] + 1, args, dbls, 2));}

static void add_i2(int *args, int *ints, Float *dbls) {INT_RESULT = (INT_ARG_1 + INT_ARG_2);}
static char *descr_add_i2(int *args, int *ints, Float *dbls) {return(describe_int_args("+", 2, args, ints, 1));}

static void add_i3(int *args, int *ints, Float *dbls) {INT_RESULT = (INT_ARG_1 + INT_ARG_2 + INT_ARG_3);}
static char *descr_add_i3(int *args, int *ints, Float *dbls) {return(describe_int_args("+", 3, args, ints, 1));}

static void add_i4(int *args, int *ints, Float *dbls) {INT_RESULT = (INT_ARG_1 + INT_ARG_2 + INT_ARG_3 + INT_ARG_4);}
static char *descr_add_i4(int *args, int *ints, Float *dbls) {return(describe_int_args("+", 4, args, ints, 1));}

static void add_in(int *args, int *ints, Float *dbls)
{
  int i, n;
  n = ints[args[1]];
  INT_RESULT = ints[args[2]];
  for (i = 1; i < n; i++) INT_RESULT += ints[args[i + 2]];
}

static char *descr_add_in(int *args, int *ints, Float *dbls) {return(describe_int_args("+", ints[args[1]] + 1, args, ints, 2));}

static xen_value *add(ptree *prog, xen_value **args, int num_args)
{
  int iscl = 0, cons_loc = 0;
  Float fscl = 0.0;
  int i;
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
  else
    {
      if (num_args == 2) return(package(prog, R_INT, add_i2, descr_add_i2, args, num_args));
      if (num_args == 3) return(package(prog, R_INT, add_i3, descr_add_i3, args, num_args));
      if (num_args == 4) return(package(prog, R_INT, add_i4, descr_add_i4, args, num_args));
      return(package_n(prog, R_INT, add_in, descr_add_in, args, num_args));
    }
  return(run_warn("+ trouble"));
}

/* ---------------- subtract ---------------- */

static void subtract_f1(int *args, int *ints, Float *dbls) {FLOAT_RESULT = -(FLOAT_ARG_1);}
static char *descr_subtract_f1(int *args, int *ints, Float *dbls) {return(mus_format( FLT_PT " = -" FLT_PT , args[0], FLOAT_RESULT, args[1], FLOAT_ARG_1));}

static void subtract_f2(int *args, int *ints, Float *dbls) {FLOAT_RESULT = (FLOAT_ARG_1 - FLOAT_ARG_2);}
static char *descr_subtract_f2(int *args, int *ints, Float *dbls) {return(describe_dbl_args("-", 2, args, dbls, 1));}

static void subtract_f3(int *args, int *ints, Float *dbls) {FLOAT_RESULT = (FLOAT_ARG_1 - FLOAT_ARG_2 - FLOAT_ARG_3);}
static char *descr_subtract_f3(int *args, int *ints, Float *dbls) {return(describe_dbl_args("-", 3, args, dbls, 1));}

static void subtract_fn(int *args, int *ints, Float *dbls) 
{
  int i, n;
  n = ints[args[1]];
  FLOAT_RESULT = FLOAT_ARG_2;
  for (i = 1; i < n; i++) FLOAT_RESULT -= dbls[args[i + 2]];
}
static char *descr_subtract_fn(int *args, int *ints, Float *dbls) {return(describe_dbl_args("-", ints[args[1]] + 1, args, dbls, 2));}

static void subtract_i1(int *args, int *ints, Float *dbls) {INT_RESULT = -(INT_ARG_1);}
static char *descr_subtract_i1(int *args, int *ints, Float *dbls) {return(mus_format( INT_PT " = -" INT_PT , args[0], INT_RESULT, args[1], INT_ARG_1));}

static void subtract_i2(int *args, int *ints, Float *dbls) {INT_RESULT = (INT_ARG_1 - INT_ARG_2);}
static char *descr_subtract_i2(int *args, int *ints, Float *dbls) {return(describe_int_args("-", 2, args, ints, 1));}

static void subtract_i3(int *args, int *ints, Float *dbls) {INT_RESULT = (INT_ARG_1 - INT_ARG_2 - INT_ARG_3);}
static char *descr_subtract_i3(int *args, int *ints, Float *dbls) {return(describe_int_args("-", 3, args, ints, 1));}

static void subtract_in(int *args, int *ints, Float *dbls)
{
  int i, n;
  n = ints[args[1]];
  INT_RESULT = ints[args[2]];
  for (i = 1; i < n; i++) INT_RESULT -= ints[args[i + 2]];
}

static char *descr_subtract_in(int *args, int *ints, Float *dbls) {return(describe_int_args("-", ints[args[1]] + 1, args, ints, 2));}

static xen_value *subtract(ptree *prog, xen_value **args, int num_args)
{
  int iscl = 0, cons_loc = 0;
  Float fscl = 0.0;
  int i;
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
  else
    {
      if (num_args == 1) return(package(prog, R_INT, subtract_i1, descr_subtract_i1, args, num_args));
      if (num_args == 2) return(package(prog, R_INT, subtract_i2, descr_subtract_i2, args, num_args));
      if (num_args == 3) return(package(prog, R_INT, subtract_i3, descr_subtract_i3, args, num_args));
      return(package_n(prog, R_INT, subtract_in, descr_subtract_in, args, num_args));
    }
  return(run_warn("- trouble"));
}


/* ---------------- 1+ 1- ---------------- */

static void one_minus_f(int *args, int *ints, Float *dbls) {FLOAT_RESULT = FLOAT_ARG_1 - 1.0;}
static char *descr_one_minus_f(int *args, int *ints, Float *dbls) {return(mus_format( FLT_PT " = " FLT_PT " - 1.0", args[0], FLOAT_RESULT, args[1], FLOAT_ARG_1));}
static void one_minus_i(int *args, int *ints, Float *dbls) {INT_RESULT = INT_ARG_1 - 1;}
static char *descr_one_minus_i(int *args, int *ints, Float *dbls) {return(mus_format( INT_PT " = " INT_PT " - 1", args[0], INT_RESULT, args[1], INT_ARG_1));}
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

static void one_plus_f(int *args, int *ints, Float *dbls) {FLOAT_RESULT = FLOAT_ARG_1 + 1.0;}
static char *descr_one_plus_f(int *args, int *ints, Float *dbls) {return(mus_format( FLT_PT " = " FLT_PT " + 1.0", args[0], FLOAT_RESULT, args[1], FLOAT_ARG_1));}
static void one_plus_i(int *args, int *ints, Float *dbls) {INT_RESULT = INT_ARG_1 + 1;}
static char *descr_one_plus_i(int *args, int *ints, Float *dbls) {return(mus_format( INT_PT " = " INT_PT " + 1", args[0], INT_RESULT, args[1], INT_ARG_1));}
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

static void divide_f1(int *args, int *ints, Float *dbls) {FLOAT_RESULT = (1.0 / FLOAT_ARG_1);}
static char *descr_divide_f1(int *args, int *ints, Float *dbls) 
{
  return(mus_format( FLT_PT " = 1.0 / " FLT_PT , args[0], FLOAT_RESULT, args[1], FLOAT_ARG_1));
}

static void divide_if1(int *args, int *ints, Float *dbls) {INT_RESULT = (int)(1.0 / FLOAT_ARG_1);}
static char *descr_divide_if1(int *args, int *ints, Float *dbls) 
{
  return(mus_format( INT_PT " = 1.0 / " FLT_PT , args[0], INT_RESULT, args[1], FLOAT_ARG_1));
}

static void divide_f2(int *args, int *ints, Float *dbls) {FLOAT_RESULT = (FLOAT_ARG_1 / FLOAT_ARG_2);}
static char *descr_divide_f2(int *args, int *ints, Float *dbls) {return(describe_dbl_args("/", 2, args, dbls, 1));}

static void divide_if2(int *args, int *ints, Float *dbls) {INT_RESULT = (int)(FLOAT_ARG_1 / FLOAT_ARG_2);}
static char *descr_divide_if2(int *args, int *ints, Float *dbls)
{
  return(mus_format( INT_PT " = " FLT_PT " / " FLT_PT , args[0], INT_RESULT, args[1], FLOAT_ARG_1, args[2], FLOAT_ARG_2));
}
	
static void divide_f3(int *args, int *ints, Float *dbls) {FLOAT_RESULT = (FLOAT_ARG_1 / (FLOAT_ARG_2 * FLOAT_ARG_3));}
static char *descr_divide_f3(int *args, int *ints, Float *dbls) 
{
  return(mus_format( FLT_PT " = " FLT_PT " / (" FLT_PT " * " FLT_PT ")", 
		     args[0], FLOAT_RESULT, args[1], FLOAT_ARG_1, args[2], FLOAT_ARG_2, args[3], FLOAT_ARG_3));
}

static void divide_fn(int *args, int *ints, Float *dbls) 
{
  int i, n;
  Float divisor = 1.0;
  n = ints[args[1]];
  for (i = 1; i < n; i++) divisor *= dbls[args[i + 2]];
  FLOAT_RESULT = FLOAT_ARG_2 / divisor;
}
static char *descr_divide_fn(int *args, int *ints, Float *dbls)
{
  char *buf, *str;
  int i, num_args;
  num_args = ints[args[1]] + 1;
  buf = (char *)CALLOC(256, sizeof(char));
  str = (char *)CALLOC(32, sizeof(char));
  sprintf(buf, FLT_PT " = " FLT_PT " / (", args[0], FLOAT_RESULT, args[2], FLOAT_ARG_2);
  for (i = 3; i < num_args; i++)
    {
      mus_snprintf(str, 32, " " FLT_PT " *", args[i], dbls[args[i]]);
      strcat(buf, str);
    }
  mus_snprintf(str, 32, " " FLT_PT ")", args[num_args], dbls[args[num_args]]);
  strcat(buf, str);
  FREE(str);
  return(buf);
}

static xen_value *divide(ptree *prog, xen_value **args, int num_args)
{
  int cons_loc = 0;
  Float fscl = 1.0;
  int i;
  if ((num_args == 1) && (args[1]->constant == R_CONSTANT))
    {
      if (prog->need_result == NEED_INT_RESULT)
	{
	  if (args[1]->type == R_INT)
	    return(make_xen_value(R_INT, add_int_to_ptree(prog, 0), R_CONSTANT));
	  else return(make_xen_value(R_INT, add_int_to_ptree(prog, (int)(1.0 / (prog->dbls[args[1]->addr]))), R_CONSTANT));
	}
      else
	{
	  if (args[1]->type == R_INT)
	    return(make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, (1.0 / (Float)(prog->ints[args[1]->addr]))), R_CONSTANT));
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
	      fscl *= (Float)(prog->ints[args[i]->addr]);
	    else fscl *= prog->dbls[args[i]->addr];
	    FREE(args[i]);
	    args[i] = NULL;
	  }
      if (fscl != 1.0)
	args[cons_loc] = make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, fscl), R_CONSTANT);
      if (prog->constants == num_args) 
	{
	  if (prog->need_result == NEED_INT_RESULT)
	    {
	      if (args[1]->type == R_INT)
		return(make_xen_value(R_INT, add_int_to_ptree(prog, (int)((Float)(prog->ints[args[1]->addr]) / fscl)), R_CONSTANT));
	      else return(make_xen_value(R_INT, add_int_to_ptree(prog, (int)(prog->dbls[args[1]->addr] / fscl)), R_CONSTANT));
	    }
	  else
	    {
	      if (args[1]->type == R_INT)
		return(make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, (Float)(prog->ints[args[1]->addr]) / fscl), R_CONSTANT));
	      else return(make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, prog->dbls[args[1]->addr] / fscl), R_CONSTANT));
	    }
	}
      if ((prog->constants == (num_args - 1)) && (args[1]->constant != R_CONSTANT))
	{
	  /* divisor is a constant */
	  if (fscl == 1.0) return(copy_xen_value(args[1]));
	  if (fscl == 0.0) return(run_warn("division by zero"));
	  if (prog->need_result == NEED_ANY_RESULT)
	    {
	      /* invert here and use multiply */
	      if (args[cons_loc]) FREE(args[cons_loc]);
	      args[2] = make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, (Float)(1.0 / fscl)), R_CONSTANT);
	      if (args[1]->type == R_INT) float_all_args(prog, num_args, args, TRUE);
	      return(package(prog, R_FLOAT, multiply_f2, descr_multiply_f2, args, 2));
	    }
	}
    }
  num_args = float_all_args(prog, num_args, args, TRUE);
  if (num_args == 1) 
    {
      if (prog->need_result == NEED_INT_RESULT)
	return(package(prog, R_INT, divide_if1, descr_divide_if1, args, num_args));
      else return(package(prog, R_FLOAT, divide_f1, descr_divide_f1, args, num_args));
    }
  if (num_args == 2) 
    {
      if (prog->need_result == NEED_INT_RESULT)
	return(package(prog, R_INT, divide_if2, descr_divide_if2, args, num_args));
      else return(package(prog, R_FLOAT, divide_f2, descr_divide_f2, args, num_args));
    }
  if (num_args == 3) return(package(prog, R_FLOAT, divide_f3, descr_divide_f3, args, num_args));
  return(package_n(prog, R_FLOAT, divide_fn, descr_divide_fn, args, num_args));
}


static char *describe_rel_f_args(char *func, int num_args, int *args, int *ints, Float *dbls, int start)
{
  char *buf, *str;
  int i;
  buf = (char *)CALLOC(256, sizeof(char));
  str = (char *)CALLOC(32, sizeof(char));
  sprintf(buf, BOOL_PT " =", args[0], B2S(BOOL_RESULT));
  for (i = start; i < num_args; i++)
    {
      mus_snprintf(str, 32, " " FLT_PT " %s", args[i], dbls[args[i]], func);
      strcat(buf, str);
    }
  mus_snprintf(str, 32, " " FLT_PT ")", args[num_args], dbls[args[num_args]]);
  strcat(buf, str);
  FREE(str);
  return(buf);
}

static char *describe_rel_i_args(char *func, int num_args, int *args, int *ints, int start)
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
	args[i] = make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, (Float)(prog->ints[args[i]->addr])), R_CONSTANT);
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
static void CName ## _f2(int *args, int *ints, Float *dbls) {BOOL_RESULT = (FLOAT_ARG_1 COp FLOAT_ARG_2);} \
static char *descr_ ## CName ## _f2(int *args, int *ints, Float *dbls) {return(describe_rel_f_args(#COp, 2, args, ints, dbls, 1));} \
static void CName ## _fn(int *args, int *ints, Float *dbls) \
{ \
  int i, n; \
  n = ints[args[1]]; \
  for (i = 2; i <= n; i++) \
    { \
      BOOL_RESULT = (dbls[args[i]] COp dbls[args[i + 1]]); \
      if (!BOOL_RESULT) break; \
    } \
} \
static char *descr_ ## CName ## _fn(int *args, int *ints, Float *dbls) {return(describe_rel_f_args(#COp, ints[args[1]] + 1, args, ints, dbls, 2));} \
static void CName ## _i2(int *args, int *ints, Float *dbls) {BOOL_RESULT = (INT_ARG_1 COp INT_ARG_2);} \
static char *descr_ ## CName ## _i2(int *args, int *ints, Float *dbls) {return(describe_rel_i_args(#COp, 2, args, ints, 1));} \
static void CName ## _in(int *args, int *ints, Float *dbls) \
{ \
  int i, n; \
  n = ints[args[1]]; \
  for (i = 2; i <= n; i++) \
    { \
      BOOL_RESULT = (ints[args[i]] COp ints[args[i + 1]]); \
      if (!BOOL_RESULT) break; \
    } \
} \
static char *descr_ ## CName ## _in(int *args, int *ints, Float *dbls) {return(describe_rel_i_args(#COp, ints[args[1]] + 1, args, ints, 2));} \
static xen_value * SName(ptree *prog, int float_result, xen_value **args, int num_args) \
{ \
  int i, lasti = 0; \
  Float lastf = 0.0; \
  if (num_args <= 1) return(make_xen_value(R_BOOL, add_int_to_ptree(prog, TRUE), R_CONSTANT)); \
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
		  return(make_xen_value(R_BOOL, add_int_to_ptree(prog, FALSE), R_CONSTANT)); \
		lastf = prog->dbls[args[i]->addr]; \
	      } \
	    else  \
	      { \
		if (lasti FOp prog->ints[args[i]->addr]) \
		  return(make_xen_value(R_BOOL, add_int_to_ptree(prog, FALSE), R_CONSTANT)); \
		lasti = prog->ints[args[i]->addr]; \
	      } \
	  } \
      if (prog->constants == num_args) \
	return(make_xen_value(R_BOOL, add_int_to_ptree(prog, TRUE), R_CONSTANT)); \
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

static void max_f2(int *args, int *ints, Float *dbls) {FLOAT_RESULT = (FLOAT_ARG_1 > FLOAT_ARG_2) ? FLOAT_ARG_1 : FLOAT_ARG_2;}
static char *descr_max_f2(int *args, int *ints, Float *dbls)
{
  return(mus_format( FLT_PT " = max(" FLT_PT ", " FLT_PT ")", args[0], FLOAT_RESULT, args[1], FLOAT_ARG_1, args[2], FLOAT_ARG_2));
}
static void max_fn(int *args, int *ints, Float *dbls)
{
  int i, n;
  Float mx;
  n = ints[args[1]];
  mx = FLOAT_ARG_2;
  for (i = 2; i <= n; i++)
    if (dbls[args[i + 1]] > mx) mx = dbls[args[i + 1]];
  FLOAT_RESULT = mx;
}
static char *descr_max_min_fn(int *args, int *ints, Float *dbls, char *which) 
{
  char *buf, *str;
  int i, n;
  buf = (char *)CALLOC(256, sizeof(char));
  str = (char *)CALLOC(32, sizeof(char));
  n = ints[args[1]];
  sprintf(buf, INT_PT " = %s(", args[0], INT_RESULT, which);
  for (i = 2; i <= n; i++)
    {
      mus_snprintf(str, 32, FLT_PT " ", args[i], dbls[args[i]]);
      strcat(buf, str);
    }
  mus_snprintf(str, 32, FLT_PT ")", args[n + 1], dbls[args[n + 1]]);
  strcat(buf, str);
  FREE(str);
  return(buf);
}
static char *descr_max_fn(int *args, int *ints, Float *dbls) {return(descr_max_min_fn(args, ints, dbls, "max"));}
static void max_i2(int *args, int *ints, Float *dbls) {INT_RESULT = (INT_ARG_1 > INT_ARG_2) ? INT_ARG_1 : INT_ARG_2;}
static char *descr_max_i2(int *args, int *ints, Float *dbls)
{
  return(mus_format( INT_PT " = max(" INT_PT ", " INT_PT ")", args[0], INT_RESULT, args[1], INT_ARG_1, args[2], INT_ARG_2));
}
static void max_in(int *args, int *ints, Float *dbls)
{
  int i, n, mx;
  n = ints[args[1]];
  mx = ints[args[2]];
  for (i = 2; i <= n; i++)
    if (ints[args[i + 1]] > mx) mx = ints[args[i + 1]];
  INT_RESULT = mx;
}
static char *descr_max_min_in(int *args, int *ints, Float *dbls, char *which)
{
  char *buf, *str;
  int i, n;
  buf = (char *)CALLOC(256, sizeof(char));
  str = (char *)CALLOC(32, sizeof(char));
  n = ints[args[1]];
  sprintf(buf, INT_PT " = %s(", args[0], INT_RESULT, which);
  for (i = 2; i <= n; i++)
    {
      mus_snprintf(str, 32, INT_PT " ", args[i], ints[args[i]]);
      strcat(buf, str);
    }
  mus_snprintf(str, 32, INT_PT ")", args[n + 1], ints[args[n + 1]]);
  strcat(buf, str);
  FREE(str);
  return(buf);
}
static char *descr_max_in(int *args, int *ints, Float *dbls) {return(descr_max_min_in(args, ints, dbls, "max"));}
static xen_value *max_1(ptree *prog, xen_value **args, int num_args)
{
  int i;
  Float fmx;
  int imx;
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
  else
    {
      if (num_args == 2) return(package(prog, R_INT, max_i2, descr_max_i2, args, num_args));
      return(package_n(prog, R_INT, max_in, descr_max_in, args, num_args));
    }
  return(run_warn("max trouble"));
}

/* ---------------- min ---------------- */

static void min_f2(int *args, int *ints, Float *dbls) {FLOAT_RESULT = (FLOAT_ARG_1 > FLOAT_ARG_2) ? FLOAT_ARG_2 : FLOAT_ARG_1;}
static char *descr_min_f2(int *args, int *ints, Float *dbls)
{
  return(mus_format( FLT_PT " = min(" FLT_PT ", " FLT_PT ")", args[0], FLOAT_RESULT, args[1], FLOAT_ARG_1, args[2], FLOAT_ARG_2));
}
static void min_fn(int *args, int *ints, Float *dbls)
{
  int i, n;
  Float mx;
  n = ints[args[1]];
  mx = FLOAT_ARG_2;
  for (i = 2; i <= n; i++)
    if (dbls[args[i + 1]] < mx) mx = dbls[args[i + 1]];
  FLOAT_RESULT = mx;
}
static char *descr_min_fn(int *args, int *ints, Float *dbls) {return(descr_max_min_fn(args, ints, dbls, "min"));}
static void min_i2(int *args, int *ints, Float *dbls) {INT_RESULT = (INT_ARG_1 > INT_ARG_2) ? INT_ARG_2 : INT_ARG_1;}
static char *descr_min_i2(int *args, int *ints, Float *dbls)
{
  return(mus_format( INT_PT " = min(" INT_PT ", " INT_PT ")", args[0], INT_RESULT, args[1], INT_ARG_1, args[2], INT_ARG_2));
}
static void min_in(int *args, int *ints, Float *dbls)
{
  int i, n, mx;
  n = ints[args[1]];
  mx = ints[args[2]];
  for (i = 2; i <= n; i++)
    if (ints[args[i + 1]] < mx) mx = ints[args[i + 1]];
  INT_RESULT = mx;
}
static char *descr_min_in(int *args, int *ints, Float *dbls) {return(descr_max_min_in(args, ints, dbls, "min"));}
static xen_value *min_1(ptree *prog, xen_value **args, int num_args)
{
  int i;
  Float fmx;
  int imx;
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
  else
    {
      if (num_args == 2) return(package(prog, R_INT, min_i2, descr_min_i2, args, num_args));
      return(package_n(prog, R_INT, min_in, descr_min_in, args, num_args));
    }
  return(run_warn("min trouble"));
}


/* ---------------- not ---------------- */

static void not_b(int *args, int *ints, Float *dbls) {BOOL_RESULT = (!(INT_ARG_1));}
static char *descr_not_b(int *args, int *ints, Float *dbls) 
{
  return(mus_format( BOOL_PT " = (!(" INT_PT "))", args[0], B2S(BOOL_RESULT), args[1], INT_ARG_1));
}
static xen_value *not_p(ptree *prog, xen_value **args, int num_args)
{
  if (args[1]->type != R_BOOL)
    return(make_xen_value(R_BOOL, add_int_to_ptree(prog, FALSE), R_CONSTANT)); /* only #f is false so (not anything)->false */
  if (prog->constants == 1)
    return(make_xen_value(R_BOOL, add_int_to_ptree(prog, !(prog->ints[args[1]->addr])), R_CONSTANT));
  return(package(prog, R_BOOL, not_b, descr_not_b, args, 1));
}


/* ---------------- eq?, eqv?, equal? ---------------- */

static void eq_b(int *args, int *ints, Float *dbls) {BOOL_RESULT = (INT_ARG_1 == INT_ARG_2);}
static char *descr_eq_b(int *args, int *ints, Float *dbls) 
{
  return(mus_format( BOOL_PT " = eq?(" INT_PT ", " INT_PT ")", args[0], B2S(BOOL_RESULT), args[1], INT_ARG_1, args[2], INT_ARG_2));
}
static xen_value *eq_p(ptree *prog, xen_value **args, int num_args)
{
  if ((args[1]->type != args[2]->type) || (args[1]->type == R_FLOAT))
    return(make_xen_value(R_BOOL, add_int_to_ptree(prog, FALSE), R_CONSTANT));
  if (prog->constants == 2)
    return(make_xen_value(R_BOOL, add_int_to_ptree(prog, prog->ints[args[1]->addr] == prog->ints[args[2]->addr]), R_CONSTANT));
  return(package(prog, R_BOOL, eq_b, descr_eq_b, args, 2));
}

static void eqv_fb(int *args, int *ints, Float *dbls) {BOOL_RESULT = (FLOAT_ARG_1 == FLOAT_ARG_2);}
static char *descr_eqv_fb(int *args, int *ints, Float *dbls) 
{
  return(mus_format( BOOL_PT " = eqv?(" FLT_PT ", " FLT_PT ")", args[0], B2S(BOOL_RESULT), args[1], FLOAT_ARG_1, args[2], FLOAT_ARG_2));
}

static void eq_clm(int *args, int *ints, Float *dbls) {BOOL_RESULT = mus_equalp((mus_any *)(INT_ARG_1), (mus_any *)(INT_ARG_2));}
static char *descr_eq_clm(int *args, int *ints, Float *dbls) 
{
  return(mus_format( BOOL_PT " = equal?(" PTR_PT ", " PTR_PT ")", 
		     args[0], B2S(BOOL_RESULT), args[1], (mus_any *)(INT_ARG_1), args[2], (mus_any *)(INT_ARG_2)));
}

static xen_value *eqv_p(ptree *prog, xen_value **args, int num_args)
{
  if (args[1]->type != args[2]->type)
    return(make_xen_value(R_BOOL, add_int_to_ptree(prog, FALSE), R_CONSTANT));
  if (prog->constants == 2)
    {
      if (args[1]->type == R_FLOAT)
	return(make_xen_value(R_BOOL, add_int_to_ptree(prog, prog->dbls[args[1]->addr] == prog->dbls[args[2]->addr]), R_CONSTANT));
      else return(make_xen_value(R_BOOL, add_int_to_ptree(prog, prog->ints[args[1]->addr] == prog->ints[args[2]->addr]), R_CONSTANT));
    }
  switch (args[1]->type)
    {
    case R_FLOAT: return(package(prog, R_BOOL, eqv_fb, descr_eqv_fb, args, 2)); break;
    case R_CLM:   return(package(prog, R_BOOL, eq_clm, descr_eq_clm, args, 2)); break;
    default:      return(package(prog, R_BOOL, eq_b, descr_eq_b, args, 2)); break;
    }
}

static xen_value *equal_p(ptree *prog, xen_value **args, int num_args)
{
  return(eqv_p(prog, args, num_args));
}


/* ---------------- odd?, even?, zero?, positive?, negative? ---------------- */

static xen_value *convert_to_int(ptree *pt, xen_value *v)
{
  xen_value *newv;
  if (v->type == R_FLOAT) 
    {
      newv = convert_dbl_to_int(pt, v, TRUE);
      FREE(v);
      return(newv);
    }
  return(v);
}
static void odd_i(int *args, int *ints, Float *dbls) {BOOL_RESULT = (INT_ARG_1 & 1);}
static char *descr_odd_i(int *args, int *ints, Float *dbls) 
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

static void even_i(int *args, int *ints, Float *dbls) {BOOL_RESULT = (!(INT_ARG_1 & 1));}
static char *descr_even_i(int *args, int *ints, Float *dbls) 
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
static void CName ## _i(int *args, int *ints, Float *dbls) {BOOL_RESULT = (INT_ARG_1 COp 0);} \
static char *descr_ ## CName ## _i(int *args, int *ints, Float *dbls) \
{ \
  return(mus_format( BOOL_PT " = (" INT_PT #COp " 0)", args[0], B2S(BOOL_RESULT), args[1], INT_ARG_1)); \
} \
static void CName ## _f(int *args, int *ints, Float *dbls) {BOOL_RESULT = (FLOAT_ARG_1 COp 0.0);} \
static char *descr_ ## CName ## _f(int *args, int *ints, Float *dbls) \
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
static void CName ## _f(int *args, int *ints, Float *dbls) {FLOAT_RESULT = CName(FLOAT_ARG_1);} \
static char *descr_ ## CName ## _f(int *args, int *ints, Float *dbls)  \
{ \
  return(mus_format( FLT_PT " = " #CName "(" FLT_PT ")", args[0], FLOAT_RESULT, args[1], FLOAT_ARG_1)); \
} \
static xen_value * CName ## _1(ptree *prog, xen_value **args, int num_args) \
{ \
  if (prog->constants == 1) \
    { \
      if (args[1]->type == R_INT) \
	return(make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, CName((Float)(prog->ints[args[1]->addr]))), R_CONSTANT)); \
      else return(make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, CName(prog->dbls[args[1]->addr])), R_CONSTANT)); \
    } \
  if (args[1]->type == R_INT) single_to_float(prog, args, 1); \
  return(package(prog, R_FLOAT, CName ## _f, descr_ ##CName ## _f, args, 1)); \
}

FL_OP(mus_radians2hz)
FL_OP(mus_hz2radians)
FL_OP(mus_degrees2radians)
FL_OP(mus_radians2degrees)
FL_OP(mus_db2linear)
FL_OP(mus_linear2db)
FL_OP(mus_random)

FL_OP(sin)
FL_OP(cos)
FL_OP(tan)

#define FL_OP_C(CName) \
static void CName ## _f(int *args, int *ints, Float *dbls) {FLOAT_RESULT = CName(FLOAT_ARG_1);} \
static char *descr_ ## CName ## _f(int *args, int *ints, Float *dbls)  \
{ \
  return(mus_format( FLT_PT " = " #CName "(" FLT_PT ")", args[0], FLOAT_RESULT, args[1], FLOAT_ARG_1)); \
} \
static xen_value * CName ## _1(ptree *prog, xen_value **args, int num_args) \
{ \
  if (current_optimization < COMPLEX_OK) return(NULL); \
  if (prog->constants == 1) \
    { \
      if (args[1]->type == R_INT) \
	return(make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, CName((Float)(prog->ints[args[1]->addr]))), R_CONSTANT)); \
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

static void atan1_f(int *args, int *ints, Float *dbls) {FLOAT_RESULT = atan(FLOAT_ARG_1);}
static char *descr_atan1_f(int *args, int *ints, Float *dbls) {return(mus_format( FLT_PT " = atan(" FLT_PT ")", args[0], FLOAT_RESULT, args[1], FLOAT_ARG_1));}
static xen_value *atan1_1(ptree *prog, xen_value **args, int num_args)
{
  if (current_optimization < COMPLEX_OK) return(NULL);
  if (prog->constants == 1)
    {
      if (args[1]->type == R_INT)
	return(make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, atan((Float)(prog->ints[args[1]->addr]))), R_CONSTANT));
      else return(make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, atan(prog->dbls[args[1]->addr])), R_CONSTANT));
    }
  if (args[1]->type == R_INT) single_to_float(prog, args, 1);
  return(package(prog, R_FLOAT, atan1_f, descr_atan1_f, args, 1));
}

static void atan2_f(int *args, int *ints, Float *dbls) {FLOAT_RESULT = atan2(FLOAT_ARG_1, FLOAT_ARG_2);}
static char *descr_atan2_f(int *args, int *ints, Float *dbls) 
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

static Float f_round(Float x)
{
  /* tricky here -- if .5 diff, need to round to nearest even int */
  /* this code from Guile numbers.c */
  double plus_half = x + 0.5;
  double result = floor(plus_half);
  return((plus_half == result) && ((plus_half / 2) != floor(plus_half / 2)) ? result - 1 : result);
}

static void round_f(int *args, int *ints, Float *dbls) {FLOAT_RESULT = f_round(FLOAT_ARG_1);}
static char *descr_round_f(int *args, int *ints, Float *dbls) {return(mus_format( FLT_PT " = round(" FLT_PT ")", args[0], FLOAT_RESULT, args[1], FLOAT_ARG_1));}
static void round_i(int *args, int *ints, Float *dbls) {INT_RESULT = (int)f_round(FLOAT_ARG_1);}
static char *descr_round_i(int *args, int *ints, Float *dbls) {return(mus_format( INT_PT " = (int)round(" FLT_PT ")", args[0], INT_RESULT, args[1], FLOAT_ARG_1));}
static xen_value *round_1(ptree *prog, xen_value **args, int num_args)
{
  /* (round 1) -> 1.0! */
  if (prog->constants == 1)
    {
      if (args[1]->type == R_INT) 
	return(make_xen_value(R_INT, add_int_to_ptree(prog, prog->ints[args[1]->addr]), R_CONSTANT)); /* r5rs spec says return int here */
      else 
	{
	  if (prog->need_result == NEED_INT_RESULT)
	    return(make_xen_value(R_INT, add_int_to_ptree(prog, (int)f_round(prog->dbls[args[1]->addr])), R_CONSTANT));
	  return(make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, f_round(prog->dbls[args[1]->addr])), R_CONSTANT));
	}
    }
  if (prog->need_result == NEED_INT_RESULT)
    {
      if (args[1]->type == R_INT)
	return(copy_xen_value(args[1]));
      return(package(prog, R_INT, round_i, descr_round_i, args, 1));
    }
  if (args[1]->type == R_INT)
    return(package(prog, R_FLOAT, store_i_f, descr_store_i_f, args,1));
  return(package(prog, R_FLOAT, round_f, descr_round_f, args, 1));
}


/* ---------------- truncate ---------------- */

static Float f_truncate(Float x)
{
  if (x < 0.0)
    return(-floor(-x));
  return(floor(x));
}

static void truncate_f(int *args, int *ints, Float *dbls) {FLOAT_RESULT = f_truncate(FLOAT_ARG_1);}
static char *descr_truncate_f(int *args, int *ints, Float *dbls) 
{
  return(mus_format( FLT_PT " = truncate(" FLT_PT ")", args[0], FLOAT_RESULT, args[1], FLOAT_ARG_1));
}
static void truncate_i(int *args, int *ints, Float *dbls) {INT_RESULT = (int)f_truncate(FLOAT_ARG_1);}
static char *descr_truncate_i(int *args, int *ints, Float *dbls) 
{
  return(mus_format( INT_PT " = truncate(" FLT_PT ")", args[0], INT_RESULT, args[1], FLOAT_ARG_1));
}
static xen_value *truncate_1(ptree *prog, xen_value **args, int num_args)
{
  if (prog->constants == 1)
    {
      if (args[1]->type == R_INT) 
	return(make_xen_value(R_INT, add_int_to_ptree(prog, prog->ints[args[1]->addr]), R_CONSTANT));
      else 
	{
	  if (prog->need_result == NEED_INT_RESULT)
	    return(make_xen_value(R_INT, add_int_to_ptree(prog, (int)f_truncate(prog->dbls[args[1]->addr])), R_CONSTANT));
	  return(make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, f_truncate(prog->dbls[args[1]->addr])), R_CONSTANT));
	}
    }
  if (prog->need_result == NEED_INT_RESULT)
    {
      if (args[1]->type == R_INT)
	return(copy_xen_value(args[1]));
      return(package(prog, R_INT, truncate_i, descr_truncate_i, args, 1));
    }
  if (args[1]->type == R_INT)
    return(package(prog, R_FLOAT, store_i_f, descr_store_i_f, args,1));
  return(package(prog, R_FLOAT, truncate_f, descr_truncate_f, args, 1));
}

/* ---------------- floor ---------------- */

static void floor_f(int *args, int *ints, Float *dbls) {FLOAT_RESULT = floor(FLOAT_ARG_1);}
static char *descr_floor_f(int *args, int *ints, Float *dbls) {return(mus_format( FLT_PT " = floor(" FLT_PT ")", args[0], FLOAT_RESULT, args[1], FLOAT_ARG_1));}
static void floor_i(int *args, int *ints, Float *dbls) {INT_RESULT = (int)floor(FLOAT_ARG_1);}
static char *descr_floor_i(int *args, int *ints, Float *dbls) {return(mus_format( INT_PT " = (int)floor(" FLT_PT ")", args[0], INT_RESULT, args[1], FLOAT_ARG_1));}
static xen_value *floor_1(ptree *prog, xen_value **args, int num_args)
{
  if (prog->constants == 1)
    {
      if (args[1]->type == R_INT) 
	return(make_xen_value(R_INT, add_int_to_ptree(prog, prog->ints[args[1]->addr]), R_CONSTANT));
      else 
	{
	  if (prog->need_result == NEED_INT_RESULT)
	    return(make_xen_value(R_INT, add_int_to_ptree(prog, (int)floor(prog->dbls[args[1]->addr])), R_CONSTANT));
	  return(make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, floor(prog->dbls[args[1]->addr])), R_CONSTANT));
	}
    }
  if (prog->need_result == NEED_INT_RESULT)
    {
      if (args[1]->type == R_INT)
	return(copy_xen_value(args[1]));
      return(package(prog, R_INT, floor_i, descr_floor_i, args, 1));
    }
  if (args[1]->type == R_INT)
    return(package(prog, R_FLOAT, store_i_f, descr_store_i_f, args,1));
  return(package(prog, R_FLOAT, floor_f, descr_floor_f, args, 1));
}

/* ---------------- ceiling ---------------- */

static void ceiling_f(int *args, int *ints, Float *dbls) {FLOAT_RESULT = ceil(FLOAT_ARG_1);}
static char *descr_ceiling_f(int *args, int *ints, Float *dbls) {return(mus_format( FLT_PT " = ceil(" FLT_PT ")", args[0], FLOAT_RESULT, args[1], FLOAT_ARG_1));}
static void ceiling_i(int *args, int *ints, Float *dbls) {INT_RESULT = (int)ceil(FLOAT_ARG_1);}
static char *descr_ceiling_i(int *args, int *ints, Float *dbls) {return(mus_format( INT_PT " = (int)ceil(" FLT_PT ")", args[0], INT_RESULT, args[1], FLOAT_ARG_1));}
static xen_value *ceiling_1(ptree *prog, xen_value **args, int num_args)
{
  if (prog->constants == 1)
    {
      if (args[1]->type == R_INT) 
	return(make_xen_value(R_INT, add_int_to_ptree(prog, prog->ints[args[1]->addr]), R_CONSTANT));
      else 
	{
	  if (prog->need_result == NEED_INT_RESULT)
	    return(make_xen_value(R_INT, add_int_to_ptree(prog, (int)ceil(prog->dbls[args[1]->addr])), R_CONSTANT));
	  return(make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, ceil(prog->dbls[args[1]->addr])), R_CONSTANT));
	}
    }
  if (prog->need_result == NEED_INT_RESULT)
    {
      if (args[1]->type == R_INT)
	return(copy_xen_value(args[1]));
      return(package(prog, R_INT, ceiling_i, descr_ceiling_i, args, 1));
    }
  if (args[1]->type == R_INT)
    return(package(prog, R_FLOAT, store_i_f, descr_store_i_f, args,1));
  return(package(prog, R_FLOAT, ceiling_f, descr_ceiling_f, args, 1));
}

/* ---------------- exact->inexact ---------------- */

static xen_value *exact2inexact_1(ptree *prog, xen_value **args, int num_args)
{
  if (args[1]->type == R_FLOAT)
    return(copy_xen_value(args[1]));
  if (prog->constants == 1)
    return(make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, (Float)(prog->ints[args[1]->addr])), R_CONSTANT));
  return(package(prog, R_FLOAT, store_i_f, descr_store_i_f, args,1));
}

static void i2e_f(int *args, int *ints, Float *dbls) {INT_RESULT = (int)floor(FLOAT_ARG_1 + 0.5);}
static char *descr_i2e_f(int *args, int *ints, Float *dbls) {return(mus_format( INT_PT " = floor(" FLT_PT " + 0.5)", args[0], INT_RESULT, args[1], FLOAT_ARG_1));}
static xen_value *inexact2exact_1(ptree *prog, xen_value **args, int num_args)
{
  if (args[1]->type == R_INT)
    return(copy_xen_value(args[1]));
  if (prog->constants == 1)
    return(make_xen_value(R_INT, add_int_to_ptree(prog, (int)floor(prog->dbls[args[1]->addr] + 0.5)), R_CONSTANT));
  return(package(prog, R_INT, i2e_f, descr_i2e_f, args, 1));
}


/* ---------------- gcd, lcm ---------------- */

static int c_gcd_1(int a, int b)
{
  int rem;
  if (a == b) return(a);
  else
    {
      if (a > b)
	{
	  rem = (int)(fmod(a, b));
	  if (rem == 0.0) return(b);
	  else return(c_gcd_1(b, rem));
	}
      else return(c_gcd_1(b, a));
    }
}

static int c_gcd(int a, int b)
{
  if (a < 0) a = -a;
  if (b < 0) b = -b;
  return(c_gcd_1(a, b));
}

static int c_lcm(int a, int b)
{
  if ((a == 0) || (b == 0)) return(0);
  if (a < 0) a = -a;
  if (b < 0) b = -b;
  return((a * b) / c_gcd_1(a, b));
}

static void gcd_in(int *args, int *ints, Float *dbls)
{
  int i, n, mx;
  n = ints[args[1]];
  mx = c_gcd(ints[args[2]], ints[args[3]]);
  /* this could be optimized... */
  for (i = 3; i <= n; i++)
    {
      if (mx == 1) break;
      mx = c_gcd(mx, ints[args[i + 1]]);
    }
  INT_RESULT = mx;
}
static char *descr_gcd_in(int *args, int *ints, Float *dbls) {return(descr_max_min_in(args, ints, dbls, "gcd"));}

static void gcd_i(int *args, int *ints, Float *dbls) {INT_RESULT = c_gcd(INT_ARG_1, INT_ARG_2);}
static char *descr_gcd_i(int *args, int *ints, Float *dbls) 
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
      int i, mx;
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

static void lcm_i(int *args, int *ints, Float *dbls) {INT_RESULT = c_lcm(INT_ARG_1, INT_ARG_2);}
static char *descr_lcm_i(int *args, int *ints, Float *dbls) 
{
  return(mus_format( INT_PT " = lcm(" INT_PT ", " INT_PT ")", args[0], INT_RESULT, args[1], INT_ARG_1, args[2], INT_ARG_2));
}
static void lcm_in(int *args, int *ints, Float *dbls)
{
  int i, n, mx;
  n = ints[args[1]];
  mx = c_lcm(ints[args[2]], ints[args[3]]);
  /* this could be optimized... */
  for (i = 3; i <= n; i++)
    {
      if (mx == 0) break;
      mx = c_lcm(mx, ints[args[i + 1]]);
    }
  INT_RESULT = mx;
}
static char *descr_lcm_in(int *args, int *ints, Float *dbls) {return(descr_max_min_in(args, ints, dbls, "lcm"));}

static xen_value *lcm_1(ptree *prog, xen_value **args, int num_args)
{
  int i, mx;
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

static int c_mod(int x, int y)
{
  int z;
  if (y == 0) return(x); /* else arithmetic exception */
  z = x % y;
  if (((y < 0) && (z > 0)) ||
      ((y > 0) && (z < 0)))
    return(z + y);
  return(z);
}

static void modulo_i(int *args, int *ints, Float *dbls) {INT_RESULT = c_mod(INT_ARG_1, INT_ARG_2);}
static char *descr_modulo_i(int *args, int *ints, Float *dbls) 
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

static void remainder_i(int *args, int *ints, Float *dbls) {INT_RESULT = (INT_ARG_1 % INT_ARG_2);}
static char *descr_remainder_i(int *args, int *ints, Float *dbls) 
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

static void quotient_i(int *args, int *ints, Float *dbls) {INT_RESULT = (INT_ARG_1 / INT_ARG_2);}
static char *descr_quotient_i(int *args, int *ints, Float *dbls) 
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

static void logand_i(int *args, int *ints, Float *dbls) {INT_RESULT = (INT_ARG_1 & INT_ARG_2);}
static char *descr_logand_i(int *args, int *ints, Float *dbls) 
{
  return(mus_format( INT_PT " = (" INT_PT " & " INT_PT ")", args[0], INT_RESULT, args[1], INT_ARG_1, args[2], INT_ARG_2));
}
static xen_value *logand_1(ptree *prog, xen_value **args, int num_args)
{
  if (prog->constants == 2)
    return(make_xen_value(R_INT, add_int_to_ptree(prog, (prog->ints[args[1]->addr] & prog->ints[args[2]->addr])), R_CONSTANT));
  return(package(prog, R_INT, logand_i, descr_logand_i, args, 2));
}

static void logior_i(int *args, int *ints, Float *dbls) {INT_RESULT = (INT_ARG_1 | INT_ARG_2);}
static char *descr_logior_i(int *args, int *ints, Float *dbls) 
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
static void logxor_i(int *args, int *ints, Float *dbls) {INT_RESULT = XOR(INT_ARG_1, INT_ARG_2);}
static char *descr_logxor_i(int *args, int *ints, Float *dbls) 
{
  return(mus_format( INT_PT " = logxor(" INT_PT ", " INT_PT ")", args[0], INT_RESULT, args[1], INT_ARG_1, args[2], INT_ARG_2));
}
static xen_value *logxor_1(ptree *prog, xen_value **args, int num_args)
{
  if (prog->constants == 2)
    return(make_xen_value(R_INT, add_int_to_ptree(prog, XOR(prog->ints[args[1]->addr], prog->ints[args[2]->addr])), R_CONSTANT));
  return(package(prog, R_INT, logxor_i, descr_logxor_i, args, 2));
}

static void lognot_i(int *args, int *ints, Float *dbls) {INT_RESULT = ~INT_ARG_1;}
static char *descr_lognot_i(int *args, int *ints, Float *dbls) {return(mus_format( INT_PT " = ~" INT_PT , args[0], INT_RESULT, args[1], INT_ARG_1));}
static xen_value *lognot_1(ptree *prog, xen_value **args, int num_args)
{
  if (prog->constants == 1)
    return(make_xen_value(R_INT, add_int_to_ptree(prog, ~(prog->ints[args[1]->addr])), R_CONSTANT));
  return(package(prog, R_INT, lognot_i, descr_lognot_i, args, 1));
}

static int c_ash(int arg1, int arg2)
{
  if (arg2 >= 0)
    return(arg1 << arg2);
  return(arg1 >> -arg2);
}

static void ash_i(int *args, int *ints, Float *dbls) {INT_RESULT = c_ash(INT_ARG_1, INT_ARG_2);}
static char *descr_ash_i(int *args, int *ints, Float *dbls) 
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

static void expt_f(int *args, int *ints, Float *dbls) {FLOAT_RESULT = pow(FLOAT_ARG_1, FLOAT_ARG_2);}
static char *descr_expt_f(int *args, int *ints, Float *dbls) 
{
  return(mus_format( FLT_PT " = pow(" FLT_PT ", " FLT_PT ")", args[0], FLOAT_RESULT, args[1], FLOAT_ARG_1, args[2], FLOAT_ARG_2));
}
static xen_value *expt_1(ptree *prog, xen_value **args, int num_args)
{
  Float f1, f2;
  if (current_optimization < COMPLEX_OK) return(NULL);
  if (prog->constants == 2)
    {
      if (args[1]->type == R_INT) f1 = (Float)(prog->ints[args[1]->addr]); else f1 = prog->dbls[args[1]->addr];
      if (args[2]->type == R_INT) f2 = (Float)(prog->ints[args[2]->addr]); else f2 = prog->dbls[args[2]->addr];
      return(make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, pow(f1, f2)), R_CONSTANT));
    }
  if (args[1]->type == R_INT) single_to_float(prog, args, 1);
  if (args[2]->type == R_INT) single_to_float(prog, args, 2);
  return(package(prog, R_FLOAT, expt_f, descr_expt_f, args, 2));
}


/* ---------------- abs ---------------- */

static void abs_f(int *args, int *ints, Float *dbls) {FLOAT_RESULT = fabs(FLOAT_ARG_1);}
static char *descr_abs_f(int *args, int *ints, Float *dbls) {return(mus_format( FLT_PT " = fabs(" FLT_PT ")", args[0], FLOAT_RESULT, args[1], FLOAT_ARG_1));}
static void abs_i(int *args, int *ints, Float *dbls) {INT_RESULT = abs(INT_ARG_1);}
static char *descr_abs_i(int *args, int *ints, Float *dbls) {return(mus_format( INT_PT " = abs(" INT_PT ")", args[0], INT_RESULT, args[1], INT_ARG_1));}
static xen_value *abs_1(ptree *prog, xen_value **args, int num_args)
{
  if (prog->constants == 1)
    {
      if (args[1]->type == R_INT)
	return(make_xen_value(R_INT, add_int_to_ptree(prog, abs(prog->ints[args[1]->addr])), R_CONSTANT));
      else return(make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, fabs(prog->dbls[args[1]->addr])), R_CONSTANT));
    }
  if (args[1]->type == R_INT)
    return(package(prog, R_INT, abs_i, descr_abs_i, args, 1));
  return(package(prog, R_FLOAT, abs_f, descr_abs_f, args, 1));
}


/* ---------------- random ---------------- */

static void random_f(int *args, int *ints, Float *dbls) {FLOAT_RESULT = XEN_FRANDOM(FLOAT_ARG_1);}
static char *descr_random_f(int *args, int *ints, Float *dbls) 
{
  return(mus_format( FLT_PT " = random(" FLT_PT ")", args[0], FLOAT_RESULT, args[1], FLOAT_ARG_1));
}
static void random_i(int *args, int *ints, Float *dbls) {INT_RESULT = XEN_IRANDOM(INT_ARG_1);}
static char *descr_random_i(int *args, int *ints, Float *dbls) 
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
static void char_ ## CName ## _c(int *args, int *ints, Float *dbls) {BOOL_RESULT = CName((char)(INT_ARG_1));} \
static char *descr_ ## CName ## _c(int *args, int *ints, Float *dbls) \
{ \
  return(mus_format( BOOL_PT " = " #SName "(" CHR_PT ")", args[0], B2S(BOOL_RESULT), args[1], (char)(INT_ARG_1))); \
} \
static xen_value * SName(ptree *pt, xen_value **args, int num_args) \
{ \
  if (pt->constants == 1) \
    return(make_xen_value(R_BOOL, add_int_to_ptree(pt, CName((char)(pt->ints[args[1]->addr]))), R_CONSTANT)); \
  return(package(pt, R_BOOL, char_## CName ## _c, descr_ ## CName ## _c, args, 1)); \
}

CHARP(char_alphabetic_p, isalpha)
CHARP(char_numeric_p, isdigit)
CHARP(char_lower_case_p, islower)
CHARP(char_upper_case_p, isupper)
CHARP(char_whitespace_p, isspace)

#define CHARC(SName, CName) \
static void char_ ## CName ## _c(int *args, int *ints, Float *dbls) {INT_RESULT = (int)CName((char)(INT_ARG_1));} \
static char *descr_ ## CName ## _c(int *args, int *ints, Float *dbls) \
{ \
  return(mus_format( CHR_PT " = " #SName "(" CHR_PT ")", args[0], (char)(INT_RESULT), args[1], (char)(INT_ARG_1))); \
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

static void string_n(int *args, int *ints, Float *dbls)
{
  int i, n;
  n = ints[args[1]];
  if (STRING_RESULT) FREE(STRING_RESULT);
  INT_RESULT = (int)CALLOC(n + 1, sizeof(char));
  for (i = 1; i <= n; i++) STRING_RESULT[i - 1] = (char)(ints[args[i + 1]]);
}
static char *descr_string_n(int *args, int *ints, Float *dbls)
{
  int i, n;
  char *buf, *temp;
  n = ints[args[1]];
  buf = (char *)CALLOC(32 + 16 * n, sizeof(char));
  temp = (char *)CALLOC(16, sizeof(char));
  sprintf(buf, STR_PT " = string(" CHR_PT , args[0], STRING_RESULT, args[2], (char)(ints[args[2]]));
  for (i = 2; i <= n; i++)
    {
      sprintf(temp, ", " CHR_PT , args[i + 1], (char)(ints[args[i + 1]]));
      strcat(buf, temp);
    }
  FREE(temp);
  strcat(buf, ")");
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

static void strlen_1(int *args, int *ints, Float *dbls) {INT_RESULT = snd_strlen(STRING_ARG_1);}
static char *descr_strlen_1(int *args, int *ints, Float *dbls)
{
  return(mus_format( INT_PT " = string-length(" STR_PT ")", args[0], INT_RESULT, args[1], STRING_ARG_1));
}
static xen_value *string_length_1(ptree *pt, xen_value **args, int num_args)
{
  if (args[1]->constant == R_CONSTANT)
    return(make_xen_value(R_INT, add_int_to_ptree(pt, snd_strlen((char *)(pt->ints[args[1]->addr]))), R_CONSTANT));
  return(package(pt, R_INT, strlen_1, descr_strlen_1, args, 1));
}

static void strcpy_1(int *args, int *ints, Float *dbls) 
{
  if (STRING_RESULT) FREE(STRING_RESULT);
  STRING_RESULT = copy_string(STRING_ARG_1);
}
static char *descr_strcpy_1(int *args, int *ints, Float *dbls) 
{
  return(mus_format( STR_PT " = string-copy(" STR_PT ")", args[0], STRING_RESULT, args[1], STRING_ARG_1));
}
static xen_value *string_copy_1(ptree *pt, xen_value **args, int num_args)
{
  if (args[1]->constant == R_CONSTANT)
    return(make_xen_value(R_STRING, add_string_to_ptree(pt, copy_string((char *)(pt->ints[args[1]->addr]))), R_CONSTANT));
  return(package(pt, R_STRING, strcpy_1, descr_strcpy_1, args, 1));
}

static void strfill_1(int *args, int *ints, Float *dbls) 
{
  int len;
  len = snd_strlen(STRING_RESULT);
  if (len > 0)
    memset((void *)(STRING_RESULT), (char)(CHAR_ARG_1), len);
}
static char *descr_strfill_1(int *args, int *ints, Float *dbls) 
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

static void strset_1(int *args, int *ints, Float *dbls) {STRING_RESULT[INT_ARG_1] = (char)(CHAR_ARG_2);}
static char *descr_strset_1(int *args, int *ints, Float *dbls) 
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

static void strref_1(int *args, int *ints, Float *dbls) {CHAR_RESULT = (char)(STRING_ARG_1[INT_ARG_2]);}
static char *descr_strref_1(int *args, int *ints, Float *dbls) 
{
  return(mus_format( CHR_PT " = string-ref(" STR_PT ", " INT_PT ")", 
		    args[0], (INT_RESULT == 0) ? '@' : CHAR_RESULT, 
		    args[1], STRING_ARG_1, args[2], INT_ARG_2));
}
static xen_value *string_ref_1(ptree *pt, xen_value **args, int num_args)
{
  if ((args[1]->constant == R_CONSTANT) && (args[2]->constant == R_CONSTANT))
    return(make_xen_value(R_CHAR, add_int_to_ptree(pt, (int)(((char *)(pt->ints[args[1]->addr]))[pt->ints[args[2]->addr]])), R_CONSTANT));
  return(package(pt, R_CHAR, strref_1, descr_strref_1, args, 2));
}


static char *int_vct_to_string(int_vct *v)
{
  int len, i;
  char *buf;
  char flt[16];
  if (v == NULL) return(copy_string("#<int_vct: null>"));
  len = 8;
  if (len > v->length) len = v->length;
  buf = (char *)CALLOC(64 + len * 8, sizeof(char));
  sprintf(buf, "#<int_vct[len=%d]:", v->length);
  if (len > 0)
    {
      for (i = 0; i < len; i++)
	{
	  mus_snprintf(flt, 16, " %d", v->data[i]);
	  strcat(buf, flt);
	}
      if (v->length > 8)
	strcat(buf, " ...");
    }
  strcat(buf, ">");
  return(buf);
}

static void display_str(int *args, int *ints, Float *dbls) {fprintf(stderr, "%s", STRING_ARG_1);}
static char *descr_display_str(int *args, int *ints, Float *dbls) {return(mus_format("display(" STR_PT ")", args[1], STRING_ARG_1));}
static void display_int(int *args, int *ints, Float *dbls) {fprintf(stderr, "%d", INT_ARG_1);}
static char *descr_display_int(int *args, int *ints, Float *dbls) {return(mus_format("display(" INT_PT ")", args[1], INT_ARG_1));}
static void display_flt(int *args, int *ints, Float *dbls) {fprintf(stderr, "%.6f", FLOAT_ARG_1);}
static char *descr_display_flt(int *args, int *ints, Float *dbls) {return(mus_format("display(" FLT_PT ")", args[1], FLOAT_ARG_1));}
static void display_lst(int *args, int *ints, Float *dbls) {fprintf(stderr, "%s", (INT_ARG_1) ? (XEN_AS_STRING((XEN)(INT_ARG_1))) : "null");}
static char *descr_display_lst(int *args, int *ints, Float *dbls) {return(mus_format("display(" LST_PT ")", args[1], (INT_ARG_1) ? (XEN_AS_STRING(((XEN)(INT_ARG_1)))) : "null"));}
static void display_symbol(int *args, int *ints, Float *dbls) {fprintf(stderr, "%s", (INT_ARG_1) ? (XEN_AS_STRING((XEN)(INT_ARG_1))) : "null");}
static char *descr_display_symbol(int *args, int *ints, Float *dbls) {return(mus_format("display(" KEY_PT ")", args[1], (INT_ARG_1) ? (XEN_AS_STRING(((XEN)(INT_ARG_1)))) : "null"));}
static void display_key(int *args, int *ints, Float *dbls) {fprintf(stderr, "%s", (INT_ARG_1) ? (XEN_AS_STRING((XEN)(INT_ARG_1))) : "null");}
static char *descr_display_key(int *args, int *ints, Float *dbls) {return(mus_format("display(" KEY_PT ")", args[1], (INT_ARG_1) ? (XEN_AS_STRING(((XEN)(INT_ARG_1)))) : "null"));}
static void display_clm(int *args, int *ints, Float *dbls) {fprintf(stderr, "%s", mus_describe((mus_any *)(INT_ARG_1)));}
static char *descr_display_clm(int *args, int *ints, Float *dbls) {return(mus_format("display(" PTR_PT ")", args[1], ((mus_any *)(INT_ARG_1))));}
static void display_vct(int *args, int *ints, Float *dbls) 
{
  char *v = NULL;
  v = vct_to_string((vct *)(INT_ARG_1));
  if (v)
    {
      fprintf(stderr, "%s", v);
      FREE(v);
    }
}
static char *descr_display_vct(int *args, int *ints, Float *dbls) {return(mus_format("display(" PTR_PT ")", args[1], ((vct *)(INT_ARG_1))));}
static void display_int_vct(int *args, int *ints, Float *dbls) 
{
  char *buf = NULL;
  buf = int_vct_to_string((int_vct *)(INT_ARG_1));
  if (buf)
    {
      fprintf(stderr, "%s", buf);
      FREE(buf);
    }
}
static char *descr_display_int_vct(int *args, int *ints, Float *dbls) {return(mus_format("display(" PTR_PT ")", args[1], ((int_vct *)(INT_ARG_1))));}
static void display_rd(int *args, int *ints, Float *dbls) {char *buf = NULL; fprintf(stderr, "%s", buf = sf_to_string((snd_fd *)(INT_ARG_1))); FREE(buf);}
static char *descr_display_rd(int *args, int *ints, Float *dbls) {return(mus_format("display(" PTR_PT ")", args[1], ((snd_fd *)(INT_ARG_1))));}
static void display_chr(int *args, int *ints, Float *dbls) {fprintf(stderr, "%c", (char)(INT_ARG_1));}
static char *descr_display_chr(int *args, int *ints, Float *dbls) {return(mus_format("display(" CHR_PT ")", args[1], (char)(INT_ARG_1)));}
static void display_bool(int *args, int *ints, Float *dbls) {fprintf(stderr, "%s", B2S(INT_ARG_1));}
static char *descr_display_bool(int *args, int *ints, Float *dbls) {return(mus_format("display(" BOOL_PT ")", args[1], B2S(INT_ARG_1)));}
static void display_con(int *args, int *ints, Float *dbls) {fprintf(stderr, GO_PT, args[1]);}
static char *descr_display_con(int *args, int *ints, Float *dbls) {return(mus_format("display(" GO_PT ")", args[1]));}
static void display_func(int *args, int *ints, Float *dbls) 
{
  char *p = NULL;
  p = describe_ptree((ptree *)(INT_ARG_1));
  if (p)
    {
      fprintf(stderr, "%s", p);
      FREE(p);
    }
}
static char *descr_display_func(int *args, int *ints, Float *dbls) {return(mus_format("display(" PTR_PT ")", args[1], (ptree *)(INT_ARG_1)));}
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
    case R_FLOAT_VECTOR:
    case R_VCT:        return(package(pt, R_BOOL, display_vct, descr_display_vct, args, 1));   break;
    case R_BOOL:       return(package(pt, R_BOOL, display_bool, descr_display_bool, args, 1)); break;
    case R_CHAR:       return(package(pt, R_BOOL, display_chr, descr_display_chr, args, 1));   break;
    case R_GOTO:       return(package(pt, R_BOOL, display_con, descr_display_con, args, 1));   break;
    case R_FUNCTION:   return(package(pt, R_BOOL, display_func, descr_display_func, args, 1)); break;
    case R_CLM_VECTOR: /* SOMEDAY: display clm_vector and vct_vector in some pretty manner */
    case R_VCT_VECTOR:
    case R_INT_VECTOR: return(package(pt, R_BOOL, display_int_vct, descr_display_int_vct, args, 1));   break;
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
			add_string_to_ptree(pt, copy_string(XEN_SYMBOL_TO_C_STRING((XEN)(pt->ints[args[1]->addr])))),
			R_CONSTANT));
}

static void snd_print_s(int *args, int *ints, Float *dbls) {listener_append(STRING_ARG_1);}
static char *descr_snd_print_s(int *args, int *ints, Float *dbls) {return(mus_format("snd_print(" STR_PT ")", args[1], STRING_ARG_1));}
static xen_value *snd_print_1(ptree *pt, xen_value **args, int num_args) {return(package(pt, R_BOOL, snd_print_s, descr_snd_print_s, args, 1));}

static void snd_warning_s(int *args, int *ints, Float *dbls) {snd_warning(STRING_ARG_1);}
static char *descr_snd_warning_s(int *args, int *ints, Float *dbls) {return(mus_format("snd_warning(" STR_PT ")", args[1], STRING_ARG_1));}
static xen_value *snd_warning_1(ptree *pt, xen_value **args, int num_args) {return(package(pt, R_BOOL, snd_warning_s, descr_snd_warning_s, args, 1));}

static void strmake_c(int *args, int *ints, Float *dbls, char c) 
{
  int i, n;
  n = INT_ARG_1;
  if (STRING_RESULT) FREE(STRING_RESULT);
  /* this should be safe because we don't allow (set! str str1) aliasing */
  STRING_RESULT = (char *)CALLOC(n + 1, sizeof(char));
  for (i = 1; i <= n; i++) STRING_RESULT[i - 1] = c;
}
static void strmake_1(int *args, int *ints, Float *dbls) {strmake_c(args, ints, dbls, ' ');}
static void strmake_2(int *args, int *ints, Float *dbls) {strmake_c(args, ints, dbls, CHAR_ARG_2);}
static char *descr_strmake_1(int *args, int *ints, Float *dbls) 
{
  return(mus_format( STR_PT " = make-string(" INT_PT ")", args[0], STRING_RESULT, args[1], INT_ARG_1));
}
static char *descr_strmake_2(int *args, int *ints, Float *dbls) 
{
  return(mus_format( STR_PT " = make-string(" INT_PT ", " CHR_PT ")", args[0], STRING_RESULT, args[1], INT_ARG_1, args[1], CHAR_ARG_2));
}
static xen_value *make_string_1(ptree *pt, xen_value **args, int num_args)
{
  if (num_args == 1)
    return(package(pt, R_STRING, strmake_1, descr_strmake_1, args, 1));
  return(package(pt, R_STRING, strmake_2, descr_strmake_2, args, 2));
}

static char *substring(char *str, int start, int end)
{
  int i, len;
  char *newstr;
  len = end - start;
  if (len <= 0) return((char *)CALLOC(1, sizeof(char)));
  newstr = (char *)CALLOC(len + 1, sizeof(char));
  for (i = 0; i < len; i++) newstr[i] = str[i + start];
  return(newstr);
}
static void substr_1(int *args, int *ints, Float *dbls) 
{
  int i, start, end, len, arg_len;
  start = ints[args[2]];
  end = ints[args[3]];
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
static char *descr_substr_1(int *args, int *ints, Float *dbls) 
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
      len = snd_strlen((char *)(pt->ints[args[1]->addr]));
      beg = pt->ints[args[2]->addr];
      end = pt->ints[args[3]->addr];
      if ((beg <= len) && (end <= len))
	return(make_xen_value(R_STRING, 
			      add_string_to_ptree(pt, substring((char *)(pt->ints[args[1]->addr]), beg, end)),
			      R_CONSTANT));
      return(run_warn("substring: args out of range"));
    }
  return(package(pt, R_STRING, substr_1, descr_substr_1, args, 3));
}


static char *descr_strn(int *args, int *ints, Float *dbls, char *which, int bool_result)
{
  int i, n;
  char *buf, *temp;
  n = ints[args[1]];
  buf = (char *)CALLOC(1024, sizeof(char));
  temp = (char *)CALLOC(1024, sizeof(char));
  if (bool_result)
    mus_snprintf(buf, 1024, BOOL_PT " = %s(" STR_PT , args[0], B2S(BOOL_RESULT), which, args[2], (char *)(ints[args[2]]));
  else mus_snprintf(buf, 1024, STR_PT " = %s(" STR_PT , args[0], STRING_RESULT, which, args[2], (char *)(ints[args[2]]));
  for (i = 2; i <= n; i++)
    {
      mus_snprintf(temp, 1024, ", " STR_PT , args[i + 1], (char *)(ints[args[i + 1]]));
      if ((snd_strlen(buf) + snd_strlen(temp) < 1023))
	strcat(buf, temp);
    }
  FREE(temp);
  strcat(buf, ")");
  return(buf);
}

static char *string_append(ptree *pt, xen_value **args, int num_args)
{
  int i, len = 0;
  char *str;
  for (i = 0; i < num_args; i++)
    len += snd_strlen((char *)(pt->ints[args[i + 1]->addr]));
  str = (char *)CALLOC(len + 1, sizeof(char));
  for (i = 0; i < num_args; i++)
    if (pt->ints[args[i + 1]->addr])
      strcat(str, (char *)(pt->ints[args[i + 1]->addr]));
  return(str);
}
static void appendstr_n(int *args, int *ints, Float *dbls) 
{
  int i, n, len = 0;
  if (STRING_RESULT) FREE(STRING_RESULT);
  n = ints[args[1]];
  for (i = 1; i <= n; i++) len += snd_strlen((char *)(ints[args[i + 1]]));
  STRING_RESULT = (char *)CALLOC(len + 1, sizeof(char));
  for (i = 1; i <= n; i++) 
    if (ints[args[i + 1]])
      strcat(STRING_RESULT, (char *)(ints[args[i + 1]]));
}
static char *descr_appendstr_n(int *args, int *ints, Float *dbls)  {return(descr_strn(args, ints, dbls, "string-append", FALSE));}
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
static int str_ ## CName ## _n(ptree *pt, xen_value **args, int num_args) \
{ \
  int i; \
  for (i = 2; i <= num_args; i++) \
    if (strcmp((char *)(pt->ints[args[i - 1]->addr]), (char *)(pt->ints[args[i]->addr])) COp 0) \
      return(FALSE); \
  return(TRUE); \
} \
static char *descr_string_ ## CName ## _n(int *args, int *ints, Float *dbls) {return(descr_strn(args, ints, dbls, #SName, TRUE));} \
static void string_ ## CName ## _n(int *args, int *ints, Float *dbls) \
{ \
  int i, n; \
  n = ints[args[1]]; \
  BOOL_RESULT = TRUE; \
  for (i = 2; i <= n; i++) \
    if (strcmp((char *)(ints[args[i]]), (char *)(ints[args[i + 1]])) COp 0) \
      { \
	BOOL_RESULT = FALSE; \
	break; \
      } \
} \
static xen_value *string_ ## CName ## _1(ptree *pt, xen_value **args, int num_args) \
{ \
  int i; \
  char *lasts = NULL; \
  if (num_args <= 1) return(make_xen_value(R_BOOL, add_int_to_ptree(pt, TRUE), R_CONSTANT)); \
  if (num_args == pt->constants) return(make_xen_value(R_BOOL, add_int_to_ptree(pt, str_ ## CName ## _n(pt, args, num_args)), R_CONSTANT)); \
  if (pt->constants > 1) \
    for (i = 1; i <= num_args; i++) \
      if (args[i]->constant == R_CONSTANT) \
	{ \
	  if ((lasts) && (strcmp(lasts, (char *)(pt->ints[args[i]->addr])) COp 0)) \
	    return(make_xen_value(R_BOOL, add_int_to_ptree(pt, FALSE), R_CONSTANT)); \
	  lasts = (char *)(pt->ints[args[i]->addr]); \
	} \
  return(package_n(pt, R_BOOL, string_ ## CName ## _n, descr_string_ ## CName ## _n, args, num_args)); \
}

STR_REL_OP(eq, string=?, !=)
STR_REL_OP(geq, string>=?, <)
STR_REL_OP(leq, string<=?, >)
STR_REL_OP(gt, string>?, <=)
STR_REL_OP(lt, string<?, >=)

#define TOLOWER(Ch) (isupper (Ch) ? tolower (Ch) : (Ch))
static int upper_strcmp(char *s1, char *s2)
{
  /* taken from libit 0.7 with changes */
  unsigned char c1, c2;
  char *p1 = (char *)s1;
  char *p2 = (char *)s2;
  if (p1 == p2) return 0;
  do
    {
      c1 = TOLOWER(*p1);
      c2 = TOLOWER(*p2);
      if (c1 == '\0') break;
      ++p1;
      ++p2;
    }
  while (c1 == c2);
  return(c1 - c2);
}

#define STR_CI_REL_OP(CName, SName, COp) \
static int str_ci_ ## CName ## _n(ptree *pt, xen_value **args, int num_args) \
{ \
  int i; \
  for (i = 2; i <= num_args; i++) \
    if (upper_strcmp((char *)(pt->ints[args[i - 1]->addr]), (char *)(pt->ints[args[i]->addr])) COp 0) \
      return(FALSE); \
  return(TRUE); \
} \
static char *descr_string_ci_ ## CName ## _n(int *args, int *ints, Float *dbls) {return(descr_strn(args, ints, dbls, #SName, TRUE));} \
static void string_ci_ ## CName ## _n(int *args, int *ints, Float *dbls) \
{ \
  int i, n; \
  n = ints[args[1]]; \
  BOOL_RESULT = TRUE; \
  for (i = 2; i <= n; i++) \
    if (upper_strcmp((char *)(ints[args[i]]), (char *)(ints[args[i + 1]])) COp 0) \
      { \
	BOOL_RESULT = FALSE; \
	break; \
      } \
} \
static xen_value *string_ci_ ## CName ## _1(ptree *pt, xen_value **args, int num_args) \
{ \
  if (num_args <= 1) return(make_xen_value(R_BOOL, add_int_to_ptree(pt, TRUE), R_CONSTANT)); \
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
static char *f2s_1(Float n) {return(copy_string(DOUBLE_TO_STRING(n)));}
static char *f2s_2(Float n, int rad) {return(copy_string(DOUBLE_TO_STRING_WITH_RADIX(n, rad)));}
static char *i2s_1(int n) {return(copy_string(INTEGER_TO_STRING(n)));}
static char *i2s_2(int n, int rad) {return(copy_string(INTEGER_TO_STRING_WITH_RADIX(n, rad)));}
static void number2string_f1(int *args, int *ints, Float *dbls) {if (STRING_RESULT) FREE(STRING_RESULT); STRING_RESULT = f2s_1(FLOAT_ARG_1);}
static void number2string_f2(int *args, int *ints, Float *dbls) {if (STRING_RESULT) FREE(STRING_RESULT); STRING_RESULT = f2s_2(FLOAT_ARG_1, INT_ARG_2);}
static void number2string_i1(int *args, int *ints, Float *dbls) {if (STRING_RESULT) FREE(STRING_RESULT); STRING_RESULT = i2s_1(INT_ARG_1);}
static void number2string_i2(int *args, int *ints, Float *dbls) {if (STRING_RESULT) FREE(STRING_RESULT); STRING_RESULT = i2s_2(INT_ARG_1, INT_ARG_2);}
static char *descr_number2string_f1(int *args, int *ints, Float *dbls) 
{
  return(mus_format( STR_PT " = number->string(" FLT_PT ")", args[0], STRING_RESULT, args[1], FLOAT_ARG_1));
}
static char *descr_number2string_f2(int *args, int *ints, Float *dbls) 
{
  return(mus_format( STR_PT " = number->string(" FLT_PT ", " INT_PT ")", args[0], STRING_RESULT, args[1], FLOAT_ARG_1, args[2], INT_ARG_2));
}
static char *descr_number2string_i1(int *args, int *ints, Float *dbls) 
{
  return(mus_format( STR_PT " = number->string(" INT_PT ")", args[0], STRING_RESULT, args[1], INT_ARG_1));
}
static char *descr_number2string_i2(int *args, int *ints, Float *dbls) 
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

static void funcall_nf(int *args, int *ints, Float *dbls) 
{
  int i;
  ptree *func;
  xen_value *fres;
  func = (ptree *)(ints[args[1]]);
  fres = func->result;
  for (i = 0; i < func->arity; i++)
    switch (func->arg_types[i])
      {
      case R_FLOAT: 
	dbls[func->args[i]] = dbls[args[i + 2]]; 
	break;
      case R_STRING: 
	if (ints[func->args[i]]) FREE((char *)(ints[func->args[i]]));
	ints[func->args[i]] = (int)copy_string((char *)(ints[args[i + 2]])); 
	break;
      default:      
	ints[func->args[i]] = ints[args[i + 2]]; 
	break;
      }
  eval_embedded_ptree(func, ints, dbls);
  switch (fres->type)
    {
    case R_INT:   
      INT_RESULT = ints[fres->addr];   
      break;
    case R_FLOAT: 
      FLOAT_RESULT = dbls[fres->addr]; 
      break;
    case R_STRING: 
      if (STRING_RESULT) FREE(STRING_RESULT);
      STRING_RESULT = (char *)(ints[fres->addr]);
      ints[fres->addr] = 0;
      break; 
    default:      
      INT_RESULT = ints[fres->addr];   
      break;
    }
}
static char *descr_funcall_nf(int *args, int *ints, Float *dbls) 
{
  return(mus_format("%d = funcall(" PTR_PT " ...)", args[0], args[1], (ptree *)(INT_ARG_1)));
}
static xen_value *funcall_n(ptree *prog, xen_value **args, int num_args, xen_value *sf)
{
  ptree *func;
  int i;
  xen_value **new_args;
  xen_value *fres;
  func = (ptree *)(prog->ints[sf->addr]);
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


/* ---------------- snd utils ---------------- */

static snd_info *run_get_sp(int offset, int *args, int *ints)
{
  snd_state *ss;
  int spint;
  ss = get_global_state();
  spint = ints[args[offset]];
  if (spint == -1)
    return(any_selected_sound(ss));
  else
    if ((spint < ss->max_sounds) && 
	(snd_ok(ss->sounds[spint])))
      return(ss->sounds[spint]);
  return(NULL);
}

static chan_info *run_get_cp(int offset, int *args, int *ints)
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

static void edit_position_i(int *args, int *ints, Float *dbls) 
{
  chan_info *cp; 
  cp = run_get_cp(1, args, ints);
  if (cp) INT_RESULT = cp->edit_ctr;
}

static char *descr_edit_position_i(int *args, int *ints, Float *dbls) 
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

static void cursor_i(int *args, int *ints, Float *dbls) 
{
  chan_info *cp; 
  cp = run_get_cp(1, args, ints);
  if (cp) INT_RESULT = CURSOR(cp);
}

static char *descr_cursor_i(int *args, int *ints, Float *dbls) 
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

static void add_mark_i(int *args, int *ints, Float *dbls) 
{
  chan_info *cp; 
  mark *m = NULL;
  cp = run_get_cp(2, args, ints);
  if (cp) 
    {
      m = add_mark(INT_ARG_1, NULL, cp);
      if (m) INT_RESULT = mark_id(m);
    }
}

static char *descr_add_mark_i(int *args, int *ints, Float *dbls) 
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

static void maxamp_f(int *args, int *ints, Float *dbls) 
{
  chan_info *cp; 
  cp = run_get_cp(1, args, ints);
  if (cp) FLOAT_RESULT = get_maxamp(cp->sound, cp, AT_CURRENT_EDIT_POSITION);
}

static char *descr_maxamp_f(int *args, int *ints, Float *dbls) 
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

static void srate_i(int *args, int *ints, Float *dbls) 
{
  snd_info *sp;
  sp = run_get_sp(1, args, ints);
  if (sp) INT_RESULT = SND_SRATE(sp);
}

static char *descr_srate_i(int *args, int *ints, Float *dbls) 
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

static void channels_i(int *args, int *ints, Float *dbls) 
{
  snd_info *sp;
  sp = run_get_sp(1, args, ints);
  if (sp) INT_RESULT = sp->nchans;
}

static char *descr_channels_i(int *args, int *ints, Float *dbls) 
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

static void c_g_p(int *args, int *ints, Float *dbls) 
{
  snd_state *ss;
  ss = get_global_state();
  check_for_event(ss);
  if (ss->stopped_explicitly)
    {
      ss->stopped_explicitly = FALSE;
      BOOL_RESULT = TRUE;
    }
  else BOOL_RESULT = FALSE;
}

static char *descr_c_g_p(int *args, int *ints, Float *dbls)
{
  return(mus_format( BOOL_PT " = c-g?()", args[0], B2S(BOOL_RESULT)));
}

static xen_value *c_g_p_1(ptree *pt, xen_value **args, int num_args)
{
  return(package(pt, R_BOOL, c_g_p, descr_c_g_p, args, 0));
}


/* ---------------- autocorrelate ---------------- */
static char *descr_autocorrelate_0(int *args, int *ints, Float *dbls) 
{
  return(mus_format("autocorrelate(" PTR_PT ")", args[1], (vct *)(INT_ARG_1)));
}
static void autocorrelate_0(int *args, int *ints, Float *dbls) 
{
  autocorrelation(((vct *)(INT_ARG_1))->data, ((vct *)(INT_ARG_1))->length);
  INT_RESULT = INT_ARG_1;
}
static xen_value *autocorrelate_1(ptree *prog, xen_value **args, int num_args) 
{
  return(package(prog, R_VCT, autocorrelate_0, descr_autocorrelate_0, args, 1));
}


/* ---------------- frames ---------------- */

static void frames_i(int *args, int *ints, Float *dbls) 
{
  chan_info *cp; 
  int pos;
  cp = run_get_cp(1, args, ints);
  if (cp)
    {
      if (INT_ARG_3 == AT_CURRENT_EDIT_POSITION)
	pos = cp->edit_ctr;
      else pos = INT_ARG_3;
      INT_RESULT = cp->samples[pos];
    }
}

static char *descr_frames_i(int *args, int *ints, Float *dbls) 
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

/* ---------------- report-in-minibuffer ---------------- */

static void report_in_minibuffer_s(int *args, int *ints, Float *dbls) 
{
  snd_info *sp;
  sp = run_get_sp(2, args, ints);
  if (sp)
    {
      set_minibuffer_string(sp, STRING_ARG_1);
      sp->minibuffer_on = MINI_REPORT;
    }
}

static char *descr_report_in_minibuffer_s(int *args, int *ints, Float *dbls) 
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

static void reader_f(int *args, int *ints, Float *dbls) {FLOAT_RESULT = read_sample_to_float(((snd_fd *)(INT_ARG_1)));}
static char *descr_reader(int *args, int *ints, Float *dbls, char *which) 
{
  return(mus_format( FLT_PT " = %s(" PTR_PT ")", args[0], FLOAT_RESULT, which, args[1], ((snd_fd *)(INT_ARG_1))));
}
static char *descr_reader_f(int *args, int *ints, Float *dbls) {return(descr_reader(args, ints, dbls, "read-sample"));}
static xen_value *reader_0(ptree *prog, xen_value **args, xen_value *sf)
{
  if (args[0]) FREE(args[0]);
  args[0] = make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, 0.0), R_VARIABLE);
  add_triple_to_ptree(prog, va_make_triple(reader_f, descr_reader_f, 2, args[0], sf));
  return(args[0]);
}
static xen_value *reader_1(ptree *prog, xen_value **args, int num_args) {return(package(prog, R_FLOAT, reader_f, descr_reader_f, args, 1));}

static char *descr_next_reader_f(int *args, int *ints, Float *dbls) {return(descr_reader(args, ints, dbls, "next-sample"));}
static void next_reader_f(int *args, int *ints, Float *dbls) {FLOAT_RESULT = protected_next_sample_to_float(((snd_fd *)(INT_ARG_1)));}
static xen_value *next_sample_1(ptree *prog, xen_value **args, int num_args) {return(package(prog, R_FLOAT, next_reader_f, descr_next_reader_f, args, 1));}

static char *descr_previous_reader_f(int *args, int *ints, Float *dbls) {return(descr_reader(args, ints, dbls, "previous-sample"));}
static void previous_reader_f(int *args, int *ints, Float *dbls) {FLOAT_RESULT = protected_previous_sample_to_float(((snd_fd *)(INT_ARG_1)));}
static xen_value *previous_sample_1(ptree *prog, xen_value **args, int num_args) {return(package(prog, R_FLOAT, previous_reader_f, descr_previous_reader_f, args, 1));}

static char *descr_make_sample_reader_r(int *args, int *ints, Float *dbls)
{
  return(mus_format( PTR_PT " = make-sample-reader(" INT_PT ", " INT_PT ", " INT_PT ", " INT_PT ", " INT_PT ")",
		    args[0], (snd_fd *)(INT_RESULT), 
		    args[1], INT_ARG_1, args[2], INT_ARG_2, args[3], INT_ARG_3, args[4], INT_ARG_4, args[5], INT_ARG_5));
}
static void make_sample_reader_r(int *args, int *ints, Float *dbls) 
{
  chan_info *cp = NULL;
  int pos;
  cp = run_get_cp(2, args, ints);
  if (cp)
    {
      if (INT_ARG_5 == -1)
	pos = cp->edit_ctr;
      else pos = INT_ARG_5;
      if (INT_RESULT) free_snd_fd((snd_fd *)(INT_RESULT));
      INT_RESULT = (int)(init_sample_read_any(INT_ARG_1, cp, INT_ARG_4, pos));
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
    true_args[4] = make_xen_value(R_INT, add_int_to_ptree(pt, READ_FORWARD), R_CONSTANT);
  else true_args[4] = args[4];
  run_opt_arg(pt, args, num_args, 2, true_args);
  run_opt_arg(pt, args, num_args, 3, true_args);
  if (num_args == 0)
    true_args[1] = make_xen_value(R_INT, add_int_to_ptree(pt, 0), R_CONSTANT);
  else
    {
      if (args[1]->type == R_FLOAT)
	true_args[1] = convert_dbl_to_int(pt, args[1], TRUE);
      else true_args[1] = args[1];
    }
  true_args[0] = args[0];
  rtn = package(pt, R_READER, make_sample_reader_r, descr_make_sample_reader_r, true_args, 5);
  add_obj_to_gcs(pt, R_READER, rtn->addr);
  for (k = num_args + 1; k <= 5; k++) FREE(true_args[k]);
  return(rtn);
}

/* ---------------- vector stuff ---------------- */

/* float vectors are handled as vcts
 */

/* length */
static void vector_length_i(int *args, int *ints, Float *dbls) {INT_RESULT = ((vct *)(INT_ARG_1))->length;}
static char *descr_vector_length_i(int *args, int *ints, Float *dbls) 
{
  return(mus_format( INT_PT " = vector_length(" PTR_PT ")", args[0], INT_RESULT, args[1], (vct *)(INT_ARG_1)));
}
static xen_value *vector_length_1(ptree *prog, xen_value **args, int num_args)
{
  return(package(prog, R_INT, vector_length_i, descr_vector_length_i, args, 1));
}

/* ref */
static void vector_ref_f(int *args, int *ints, Float *dbls) {FLOAT_RESULT = ((vct *)(INT_ARG_1))->data[INT_ARG_2];}
static void vector_ref_i(int *args, int *ints, Float *dbls) {INT_RESULT = ((int_vct *)(INT_ARG_1))->data[INT_ARG_2];}
static void vector_ref_v(int *args, int *ints, Float *dbls) {INT_RESULT = (int)((vct_vct *)(INT_ARG_1))->data[INT_ARG_2];}
static void vector_ref_c(int *args, int *ints, Float *dbls) {INT_RESULT = (int)((clm_vct *)(INT_ARG_1))->data[INT_ARG_2];}
static char *descr_vector_ref_f(int *args, int *ints, Float *dbls) 
{
  return(mus_format( FLT_PT " = vector_ref(" PTR_PT ", " INT_PT ")", args[0], FLOAT_RESULT, args[1], (vct *)(INT_ARG_1), args[2], INT_ARG_2));
}
static char *descr_vector_ref_i(int *args, int *ints, Float *dbls) 
{
  return(mus_format( INT_PT " = vector_ref(" PTR_PT ", " INT_PT ")", args[0], INT_RESULT, args[1], (int_vct *)(INT_ARG_1), args[2], INT_ARG_2));
}
static char *descr_vector_ref_v(int *args, int *ints, Float *dbls) 
{
  return(mus_format( PTR_PT " = vector_ref(" PTR_PT ", " INT_PT ")", args[0], (vct *)(INT_RESULT), args[1], (vct_vct *)(INT_ARG_1), args[2], INT_ARG_2));
}
static char *descr_vector_ref_c(int *args, int *ints, Float *dbls) 
{
  return(mus_format( PTR_PT " = vector_ref(" PTR_PT ", " INT_PT ")", args[0], (mus_any *)(INT_RESULT), args[1], (clm_vct *)(INT_ARG_1), args[2], INT_ARG_2));
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
static void vector_set_f(int *args, int *ints, Float *dbls) {((vct *)(INT_ARG_1))->data[INT_ARG_2] = FLOAT_ARG_3;}
static void vector_set_i(int *args, int *ints, Float *dbls) {((int_vct *)(INT_ARG_1))->data[INT_ARG_2] = INT_ARG_3;}
static void vector_set_v(int *args, int *ints, Float *dbls) {((vct_vct *)(INT_ARG_1))->data[INT_ARG_2] = (vct *)(INT_ARG_3);}
static void vector_set_c(int *args, int *ints, Float *dbls) {((clm_vct *)(INT_ARG_1))->data[INT_ARG_2] = (mus_any *)(INT_ARG_3);}
static char *descr_vector_set_f(int *args, int *ints, Float *dbls) 
{
  return(mus_format("vector_set(" PTR_PT ", " INT_PT ", " FLT_PT ")", args[1], (vct *)(INT_ARG_1), args[2], INT_ARG_2, args[3], FLOAT_ARG_3));
}
static char *descr_vector_set_i(int *args, int *ints, Float *dbls) 
{
  return(mus_format("vector_set(" PTR_PT ", " INT_PT ", " INT_PT ")", args[1], (int_vct *)(INT_ARG_1), args[2], INT_ARG_2, args[3], INT_ARG_3));
}
static char *descr_vector_set_v(int *args, int *ints, Float *dbls) 
{
  return(mus_format("vector_set(" PTR_PT ", " INT_PT ", " PTR_PT ")", args[1], (vct_vct *)(INT_ARG_1), args[2], INT_ARG_2, args[3], (vct *)(INT_ARG_3)));
}
static char *descr_vector_set_c(int *args, int *ints, Float *dbls) 
{
  return(mus_format("vector_set(" PTR_PT ", " INT_PT ", " PTR_PT ")", args[1], (clm_vct *)(INT_ARG_1), args[2], INT_ARG_2, args[3], (mus_any *)(INT_ARG_3)));
}
static xen_value *vector_set_1(ptree *prog, xen_value **args, int num_args)
{
  xen_var *var;
  var = find_var_in_ptree_via_addr(prog, FALSE, args[1]->addr);
  if (var) var->unclean = TRUE;
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
static void vector_fill_f(int *args, int *ints, Float *dbls) 
{
  int i; for (i = 0; i < ((vct *)(INT_ARG_1))->length; i++) ((vct *)(INT_ARG_1))->data[i] = FLOAT_ARG_2;
}
static void vector_fill_i(int *args, int *ints, Float *dbls) 
{
  int i; for (i = 0; i < ((vct *)(INT_ARG_1))->length; i++) ((int_vct *)(INT_ARG_1))->data[i] = INT_ARG_2;
}
static void vector_fill_v(int *args, int *ints, Float *dbls) 
{
  int i; for (i = 0; i < ((vct *)(INT_ARG_1))->length; i++) ((vct_vct *)(INT_ARG_1))->data[i] = (vct *)(INT_ARG_2);
}
static void vector_fill_c(int *args, int *ints, Float *dbls)
{
  int i; for (i = 0; i < ((vct *)(INT_ARG_1))->length; i++) ((clm_vct *)(INT_ARG_1))->data[i] = (mus_any *)(INT_ARG_2);
}
static char *descr_vector_fill_f(int *args, int *ints, Float *dbls) 
{
  return(mus_format("vector_fill(" PTR_PT ", " FLT_PT ")", args[1], (vct *)(INT_ARG_1), args[2], FLOAT_ARG_2));
}
static char *descr_vector_fill_i(int *args, int *ints, Float *dbls) 
{
  return(mus_format("vector_fill(" PTR_PT ", " INT_PT ")", args[1], (int_vct *)(INT_ARG_1), args[2], INT_ARG_2));
}
static char *descr_vector_fill_v(int *args, int *ints, Float *dbls) 
{
  return(mus_format("vector_fill(" PTR_PT ", " PTR_PT ")", args[1], (vct_vct *)(INT_ARG_1), args[2], (vct *)(INT_ARG_2)));
}
static char *descr_vector_fill_c(int *args, int *ints, Float *dbls) 
{
  return(mus_format("vector_fill(" PTR_PT ", " PTR_PT ")", args[1], (clm_vct *)(INT_ARG_1), args[2], (mus_any *)(INT_ARG_2)));
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

static void vct_length_i(int *args, int *ints, Float *dbls) {INT_RESULT = ((vct *)(INT_ARG_1))->length;}
static char *descr_vct_length_i(int *args, int *ints, Float *dbls) 
{
  return(mus_format( INT_PT " = vct_length(" PTR_PT ")", args[0], INT_RESULT, args[1], (vct *)(INT_ARG_1)));
}
static xen_value *vct_length_1(ptree *prog, xen_value **args, int num_args)
{
  return(package(prog, R_INT, vct_length_i, descr_vct_length_i, args, 1));
}

static void vct_constant_ref_0(int *args, int *ints, Float *dbls) {FLOAT_RESULT = ((vct *)(INT_ARG_1))->data[0];}
static void vct_constant_ref_1(int *args, int *ints, Float *dbls) {FLOAT_RESULT = ((vct *)(INT_ARG_1))->data[1];}
static void vct_constant_ref_2(int *args, int *ints, Float *dbls) {FLOAT_RESULT = ((vct *)(INT_ARG_1))->data[2];}
static char *descr_vct_constant_ref_0(int *args, int *ints, Float *dbls) 
{
  return(mus_format( FLT_PT " = vct_ref(" PTR_PT ", 0)", args[0], FLOAT_RESULT, args[1], (vct *)(INT_ARG_1)));
}
static char *descr_vct_constant_ref_1(int *args, int *ints, Float *dbls) 
{
  return(mus_format( FLT_PT " = vct_ref(" PTR_PT ", 1)", args[0], FLOAT_RESULT, args[1], (vct *)(INT_ARG_1)));
}
static char *descr_vct_constant_ref_2(int *args, int *ints, Float *dbls) 
{
  return(mus_format( FLT_PT " = vct_ref(" PTR_PT ", 2)", args[0], FLOAT_RESULT, args[1], (vct *)(INT_ARG_1)));
}

static void vct_ref_f(int *args, int *ints, Float *dbls) {FLOAT_RESULT = ((vct *)(INT_ARG_1))->data[INT_ARG_2];}
static char *descr_vct_ref_f(int *args, int *ints, Float *dbls) 
{
  return(mus_format( FLT_PT " = vct_ref(" PTR_PT ", " INT_PT ")", args[0], FLOAT_RESULT, args[1], (vct *)(INT_ARG_1), args[2], INT_ARG_2));
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

static void vct_constant_set_0(int *args, int *ints, Float *dbls) {((vct *)(INT_ARG_1))->data[0] = FLOAT_ARG_3;}
static void vct_constant_set_1(int *args, int *ints, Float *dbls) {((vct *)(INT_ARG_1))->data[1] = FLOAT_ARG_3;}
static void vct_constant_set_2(int *args, int *ints, Float *dbls) {((vct *)(INT_ARG_1))->data[2] = FLOAT_ARG_3;}
static void vct_set_f(int *args, int *ints, Float *dbls) {((vct *)(INT_ARG_1))->data[INT_ARG_2] = FLOAT_ARG_3;}

static char *descr_vct_constant_set_0(int *args, int *ints, Float *dbls) 
{
  return(mus_format("vct_set!(" PTR_PT ", 0, " FLT_PT ")",  args[1], (vct *)(INT_ARG_1), args[3], FLOAT_ARG_3));
}
static char *descr_vct_constant_set_1(int *args, int *ints, Float *dbls) 
{
  return(mus_format("vct_set!(" PTR_PT ", 1, " FLT_PT ")",  args[1], (vct *)(INT_ARG_1), args[3], FLOAT_ARG_3));
}
static char *descr_vct_constant_set_2(int *args, int *ints, Float *dbls) 
{
  return(mus_format("vct_set!(" PTR_PT ", 2, " FLT_PT ")",  args[1], (vct *)(INT_ARG_1), args[3], FLOAT_ARG_3));
}
static char *descr_vct_set_f(int *args, int *ints, Float *dbls) 
{
  return(mus_format("vct_set!(" PTR_PT ", " INT_PT ", " FLT_PT ")", args[1], (vct *)(INT_ARG_1), args[2], INT_ARG_2, args[3], FLOAT_ARG_3));
}
static void vct_set_i(int *args, int *ints, Float *dbls) {((vct *)(INT_ARG_1))->data[INT_ARG_2] = (Float)INT_ARG_3;}
static char *descr_vct_set_i(int *args, int *ints, Float *dbls) 
{
  return(mus_format("vct_set!(" PTR_PT ", " INT_PT ", " INT_PT ")", args[1], (vct *)(INT_ARG_1), args[2], INT_ARG_2, args[3], INT_ARG_3));
}
static void vct_set_1(ptree *prog, xen_value *in_v, xen_value *in_v1, xen_value *in_v2, xen_value *v)
{
  xen_var *var;
  var = find_var_in_ptree_via_addr(prog, FALSE, in_v->addr);
  if (var) var->unclean = TRUE;
  if (v->type == R_FLOAT)
    {
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
  else add_triple_to_ptree(prog, va_make_triple(vct_set_i, descr_vct_set_i, 4, NULL, in_v, in_v1, v));
}
static xen_value *vct_set_2(ptree *prog, xen_value **args, int num_args)
{
  xen_var *var;
  var = find_var_in_ptree_via_addr(prog, FALSE, args[1]->addr);
  if (var) var->unclean = TRUE;
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

static void make_vct_v(int *args, int *ints, Float *dbls) 
{
  if (VCT_RESULT) c_free_vct(VCT_RESULT);
  VCT_RESULT = c_make_vct(INT_ARG_1);
}
static char *descr_make_vct_v(int *args, int *ints, Float *dbls) 
{
  return(mus_format( PTR_PT " = make_vct(" INT_PT ")", args[0], (vct *)INT_RESULT, args[1], INT_ARG_1));
}
static void make_vct_v2(int *args, int *ints, Float *dbls) 
{
  vct *v;
  int i;
  if (VCT_RESULT) c_free_vct(VCT_RESULT);
  v = c_make_vct(INT_ARG_1);
  VCT_RESULT = v;
  for (i = 0; i < v->length; i++) v->data[i] = FLOAT_ARG_2;
}
static char *descr_make_vct_v2(int *args, int *ints, Float *dbls) 
{
  return(mus_format( PTR_PT " = make_vct(" INT_PT ", " FLT_PT ")", args[0], (vct *)INT_RESULT, args[1], INT_ARG_1, args[2], FLOAT_ARG_2));
}
static xen_value *make_vct_1(ptree *prog, xen_value **args, int num_args)
{
  args[0] = make_xen_value(R_VCT, add_int_to_ptree(prog, 0), R_VARIABLE);
  add_obj_to_gcs(prog, R_VCT, args[0]->addr);
  if (num_args == 1)
    add_triple_to_ptree(prog, va_make_triple(make_vct_v, descr_make_vct_v, 2, args[0], args[1]));
  else add_triple_to_ptree(prog, va_make_triple(make_vct_v2, descr_make_vct_v2, 3, args[0], args[1], args[2]));
  return(args[0]);
}

static void vct_v(int *args, int *ints, Float *dbls) 
{
  int i;
  vct *v;
  if (VCT_RESULT) c_free_vct(VCT_RESULT);
  v = c_make_vct(ints[args[1]]);
  for (i = 0; i < v->length; i++)
    v->data[i] = dbls[args[i + 2]];
  VCT_RESULT = v;
}
static char *descr_vct_v(int *args, int *ints, Float *dbls) 
{
  return(mus_format( PTR_PT " = vct(" INT_PT " ...)", args[0], (vct *)INT_RESULT, args[1], INT_ARG_1));
}
static xen_value *vct_1(ptree *prog, xen_value **args, int num_args)
{
  xen_value *rtn;
  rtn = package_n(prog, R_VCT, vct_v, descr_vct_v, args, num_args);
  /* rtn is args[0] */
  add_obj_to_gcs(prog, R_VCT, rtn->addr);
  return(rtn);
}

static void vct_copy_v(int *args, int *ints, Float *dbls) 
{
  if (VCT_RESULT) c_free_vct(VCT_RESULT);
  VCT_RESULT = c_vct_copy((vct *)(INT_ARG_1));
}
static char *descr_vct_copy_v(int *args, int *ints, Float *dbls) 
{
  return(mus_format( PTR_PT " = vct_copy(" PTR_PT ")", args[0], (vct *)INT_RESULT, args[1], (vct *)(INT_ARG_1)));
}
static xen_value *vct_copy_1(ptree *prog, xen_value **args, int num_args)
{
  args[0] = make_xen_value(R_VCT, add_int_to_ptree(prog, 0), R_VARIABLE);
  add_obj_to_gcs(prog, R_VCT, args[0]->addr);
  add_triple_to_ptree(prog, va_make_triple(vct_copy_v, descr_vct_copy_v, 2, args[0], args[1]));
  return(args[0]);
}

#define VCT_OP_1(SName, CName, COp) \
static void vct_ ## CName ## _f(int *args, int *ints, Float *dbls) \
{ \
  int i; \
  vct *v = VCT_ARG_1; \
  for (i = 0; i < v->length; i++) v->data[i] COp FLOAT_ARG_2; \
  VCT_RESULT = VCT_ARG_1; \
} \
static char *descr_vct_ ## CName ## _f(int *args, int *ints, Float *dbls)  \
{ \
  return(mus_format("vct_" #SName "(" PTR_PT ", " FLT_PT ")", args[1], (vct *)(INT_ARG_1), args[2], FLOAT_ARG_2)); \
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
  return(package(prog, R_VCT, vct_ ## CName ## _f, descr_vct_ ## CName ## _f, args, 2)); \
}

VCT_OP_1(fill!, fill, =)
VCT_OP_1(scale!, scale, *=)
VCT_OP_1(offset!, offset, +=)

#define VCT_OP_2(SName, CName, COp) \
static void vct_ ## CName ## _f(int *args, int *ints, Float *dbls) \
{ \
  int i, len; \
  vct *v0 = VCT_ARG_1; \
  vct *v1 = VCT_ARG_2; \
  len = v0->length; \
  if (v1->length < len) len = v1->length; \
  for (i = 0; i < len; i++) v0->data[i] COp v1->data[i]; \
  VCT_RESULT = v0; \
} \
static char *descr_vct_ ## CName ## _f(int *args, int *ints, Float *dbls)  \
{ \
  return(mus_format("vct_" #SName "(" PTR_PT ", " PTR_PT ")", args[1], (vct *)(INT_ARG_1), args[2], (vct *)(INT_ARG_2))); \
} \
static xen_value *vct_ ## CName ## _1(ptree *prog, xen_value **args, int num_args) \
{ \
  return(package(prog, R_VCT, vct_ ## CName ## _f, descr_vct_ ## CName ## _f, args, 2)); \
}

VCT_OP_2(add!, add, +=)
VCT_OP_2(multiply!, multiply, *=)
VCT_OP_2(subtract!, subtract, -=)


static void vct_convolve_0(int *args, int *ints, Float *dbls)
{
  vct *v0 = VCT_ARG_1;
  vct *v1 = VCT_ARG_2;
  mus_convolution(v0->data, v1->data, v0->length); /* assuming power of 2 len here and v1 exists and v0 is big enough */
  VCT_RESULT = v0;
}
static char *descr_vct_convolve_0(int *args, int *ints, Float *dbls)
{
  return(mus_format("vct_convolve!(" PTR_PT ", " PTR_PT ")", args[1], (vct *)(INT_ARG_1), args[2], (vct *)(INT_ARG_2)));
}
static xen_value *vct_convolve_1(ptree *prog, xen_value **args, int num_args)
{
  return(package(prog, R_VCT, vct_convolve_0, descr_vct_convolve_0, args, 2));
}



/* ---------------- CLM stuff ---------------- */

static char *descr_gen(int *args, int *ints, Float *dbls, char *which, int num_args) 
{
  char *buf, *str;
  int i;
  buf = (char *)CALLOC(256, sizeof(char));
  str = (char *)CALLOC(32, sizeof(char));
  sprintf(buf, FLT_PT " = %s(" PTR_PT , args[0], FLOAT_RESULT, which, args[1], (mus_any *)(INT_ARG_1));
  for (i = 1; i <= num_args; i++)
    {
      mus_snprintf(str, 32, ", " FLT_PT , args[i + 1], dbls[args[i + 1]]);
      strcat(buf, str);
    }
  strcat(buf, ")");
  FREE(str);
  return(buf);
}

#define GEN_P(Name) \
  static char *descr_ ## Name ## _0p(int *args, int *ints, Float *dbls) \
{ \
  char *buf; \
  buf = (char *)CALLOC(256, sizeof(char)); \
  sprintf(buf, BOOL_PT " = " #Name "?(" PTR_PT ")", args[0], B2S(BOOL_RESULT), args[1], (mus_any *)(INT_ARG_1)); \
  return(buf); \
} \
  static void Name ## _0p(int *args, int *ints, Float *dbls) {BOOL_RESULT = mus_ ##Name ## _p((mus_any *)(INT_ARG_1));} \
  static xen_value * Name ## _p(ptree *prog, xen_value **args, int num_args) \
  { \
    return(package(prog, R_BOOL, Name ## _0p, descr_ ## Name ## _0p, args, 1)); \
  }
#define GEN2_0(Name) \
  static char *descr_ ## Name ## _0f(int *args, int *ints, Float *dbls) {return(descr_gen(args, ints, dbls, #Name, 0));} \
  static void Name ## _0f(int *args, int *ints, Float *dbls) {FLOAT_RESULT = mus_ ## Name (((mus_any *)(INT_ARG_1)), 0.0);}  
#define GEN2_0_OPT(Name) \
  static char *descr_ ## Name ## _0f(int *args, int *ints, Float *dbls) {return(descr_gen(args, ints, dbls, #Name, 0));} \
  static void Name ## _0f(int *args, int *ints, Float *dbls) {FLOAT_RESULT = mus_ ## Name ## _1 (((mus_any *)(INT_ARG_1)));}  
#define GEN1_0(Name) \
  static char *descr_ ## Name ## _0f(int *args, int *ints, Float *dbls) {return(descr_gen(args, ints, dbls, #Name, 0));} \
  static void Name ## _0f(int *args, int *ints, Float *dbls) {FLOAT_RESULT = mus_ ## Name ((mus_any *)(INT_ARG_1));}  
#define GEN2_1(Name) \
  static char *descr_ ## Name ## _1f(int *args, int *ints, Float *dbls) {return(descr_gen(args, ints, dbls, #Name, 1));} \
  static void Name ## _1f(int *args, int *ints, Float *dbls) {FLOAT_RESULT = mus_ ## Name (((mus_any *)(INT_ARG_1)), FLOAT_ARG_2);}

#define GEN3(Name) \
  static char *descr_ ## Name ## _0f(int *args, int *ints, Float *dbls) {return(descr_gen(args, ints, dbls, #Name, 0));} \
  static void Name ## _0f(int *args, int *ints, Float *dbls) {FLOAT_RESULT = mus_ ## Name ## _1(((mus_any *)(INT_ARG_1)), 0.0);} \
  static char *descr_ ## Name ## _1f(int *args, int *ints, Float *dbls) {return(descr_gen(args, ints, dbls, #Name, 1));} \
  static void Name ## _1f(int *args, int *ints, Float *dbls) {FLOAT_RESULT = mus_ ## Name ## _1(((mus_any *)(INT_ARG_1)), FLOAT_ARG_2);} \
  static char *descr_ ## Name ## _2f(int *args, int *ints, Float *dbls) {return(descr_gen(args, ints, dbls, #Name, 2));} \
  static void Name ## _2f(int *args, int *ints, Float *dbls) {FLOAT_RESULT = mus_ ## Name (((mus_any *)(INT_ARG_1)), FLOAT_ARG_2, FLOAT_ARG_3);} \
  GEN_P(Name) \
  static xen_value * Name ## _1(ptree *prog, xen_value **args, int num_args) \
  { \
    if ((num_args > 1) && (args[2]->type == R_INT)) single_to_float(prog, args, 2); \
    if ((num_args > 2) && (args[3]->type == R_INT)) single_to_float(prog, args, 3); \
    if (num_args == 1) return(package(prog, R_FLOAT, Name ## _0f, descr_ ## Name ## _0f, args, 1)); \
    if (num_args == 2) return(package(prog, R_FLOAT, Name ## _1f, descr_ ## Name ## _1f, args, 2)); \
    return(package(prog, R_FLOAT, Name ## _2f, descr_ ## Name ## _2f, args, 3)); \
  }
#define GEN_OSCIL(Name) \
  static char *descr_ ## Name ## _0f_1(int *args, int *ints, Float *dbls) {return(descr_gen(args, ints, dbls, #Name, 0));} \
  static void Name ## _0f_1(int *args, int *ints, Float *dbls) {FLOAT_RESULT = mus_ ## Name ## _0(((mus_any *)(INT_ARG_1)));} \
  static char *descr_ ## Name ## _1f_1(int *args, int *ints, Float *dbls) {return(descr_gen(args, ints, dbls, #Name, 1));} \
  static void Name ## _1f_1(int *args, int *ints, Float *dbls) {FLOAT_RESULT = mus_ ## Name ## _1(((mus_any *)(INT_ARG_1)), FLOAT_ARG_2);} \
  static char *descr_ ## Name ## _2f(int *args, int *ints, Float *dbls) {return(descr_gen(args, ints, dbls, #Name, 2));} \
  static void Name ## _2f(int *args, int *ints, Float *dbls) {FLOAT_RESULT = mus_ ## Name (((mus_any *)(INT_ARG_1)), FLOAT_ARG_2, FLOAT_ARG_3);} \
  GEN_P(Name) \
  static xen_value * Name ## _1(ptree *prog, xen_value **args, int num_args) \
  { \
    if ((num_args > 1) && (args[2]->type == R_INT)) single_to_float(prog, args, 2); \
    if ((num_args > 2) && (args[3]->type == R_INT)) single_to_float(prog, args, 3); \
    if ((num_args == 1) || ((num_args == 2) && (args[2]->constant == R_CONSTANT) && (prog->dbls[args[2]->addr] == 0.0))) \
      return(package(prog, R_FLOAT, Name ## _0f_1, descr_ ## Name ## _0f_1, args, 1)); \
    if ((num_args == 2) || ((num_args == 3) && (args[3]->constant == R_CONSTANT) && (prog->dbls[args[3]->addr] == 0.0))) \
      return(package(prog, R_FLOAT, Name ## _1f_1, descr_ ## Name ## _1f_1, args, 2)); \
    return(package(prog, R_FLOAT, Name ## _2f, descr_ ## Name ## _2f, args, 3)); \
  }
#define GEN2(Name) \
  GEN2_0(Name) \
  GEN2_1(Name) \
  GEN_P(Name) \
  static xen_value * Name ## _1(ptree *prog, xen_value **args, int num_args) \
  { \
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
    if ((num_args > 1) && (args[2]->type == R_INT)) single_to_float(prog, args, 2); \
    if (num_args == 1) return(package(prog, R_FLOAT, Name ## _0f, descr_ ## Name ## _0f, args, 1)); \
    return(package(prog, R_FLOAT, Name ## _1f, descr_ ## Name ## _1f, args, 2)); \
  }

GEN2_1(sample2buffer)
static xen_value *sample2buffer_1(ptree *prog, xen_value **args, int num_args)
{
  if (args[2]->type == R_INT) single_to_float(prog, args, 2);
  return(package(prog, R_FLOAT, sample2buffer_1f, descr_sample2buffer_1f, args, 2));
}

static char *descr_tap_0f(int *args, int *ints, Float *dbls) {return(descr_gen(args, ints, dbls, "tap", 0));}
static void tap_0f(int *args, int *ints, Float *dbls) {FLOAT_RESULT = mus_tap_1(((mus_any *)(INT_ARG_1)));}  
static char *descr_tap_1f(int *args, int *ints, Float *dbls) {return(descr_gen(args, ints, dbls, "tap", 1));}
static void tap_1f(int *args, int *ints, Float *dbls) {FLOAT_RESULT = mus_tap(((mus_any *)(INT_ARG_1)), FLOAT_ARG_2);}

static xen_value *tap_1(ptree *prog, xen_value **args, int num_args)
{
  if (num_args == 1)
    return(package(prog, R_FLOAT, tap_0f, descr_tap_0f, args, 1));
  if (args[2]->type == R_INT) single_to_float(prog, args, 2);
  return(package(prog, R_FLOAT, tap_1f, descr_tap_1f, args, 2));
}

#define GEN1(Name) \
  GEN1_0(Name) \
  GEN_P(Name) \
  static xen_value * Name ## _1(ptree *prog, xen_value **args, int num_args) {return(package(prog, R_FLOAT, Name ## _0f, descr_ ## Name ## _0f, args, 1));}
#define GEN0(Name) \
  GEN1_0(Name) \
  static xen_value * mus_ ## Name ## _0(ptree *prog, xen_value **args, int num_args) \
  { \
    return(package(prog, R_FLOAT, Name ## _0f, descr_ ## Name ## _0f, args, 1)); \
  }

static char *descr_int_gen0(int *args, int *ints, Float *dbls, char *which)
{
  return(mus_format( INT_PT " = %s(" PTR_PT , args[0], INT_RESULT, which, args[1], (void *)(INT_ARG_1)));
}
#define INT_GEN0(Name) \
  static char *descr_ ## Name ## _0i(int *args, int *ints, Float *dbls) {return(descr_int_gen0(args, ints, dbls, #Name));} \
  static void Name ## _0i(int *args, int *ints, Float *dbls) {INT_RESULT = mus_ ## Name ((mus_any *)(INT_ARG_1));} \
  static xen_value * mus_ ## Name ## _0(ptree *prog, xen_value **args, int num_args) \
  { \
    return(package(prog, R_INT, Name ## _0i, descr_ ## Name ## _0i, args, 1)); \
  }

GEN_OSCIL(oscil)
GEN1(env)
GEN3(notch)
GEN3(comb)
GEN3(delay)
GEN3(all_pass)
GEN2(rand)
GEN2(rand_interp)
GEN2(sum_of_cosines)
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

GEN0(buffer2sample)
GEN0(increment)
GEN0(frequency)
GEN0(phase)
GEN0(scaler)
GEN0(width)
GEN0(offset)
GEN0(formant_radius)
GEN0(a0)
GEN0(a1)
GEN0(a2)
GEN0(b1)
GEN0(b2)
GEN0(x1)
GEN0(x2)
GEN0(y1)
GEN0(y2)
GEN0(feedforward)
GEN0(feedback)

INT_GEN0(hop)
INT_GEN0(channels)
INT_GEN0(location)
INT_GEN0(ramp)
INT_GEN0(position)
INT_GEN0(order)
INT_GEN0(length)
INT_GEN0(cosines)
INT_GEN0(channel)

GEN_P(buffer)
GEN_P(buffer_empty)
GEN_P(buffer_full)
GEN_P(frame)
GEN_P(mixer)
GEN_P(file2sample)
GEN_P(sample2file)
GEN_P(file2frame)
GEN_P(frame2file)
GEN_P(locsig)
GEN_P(input)
GEN_P(output)


static char *descr_set_formant_radius_and_frequency_2f(int *args, int *ints, Float *dbls) 
{
  return(descr_gen(args, ints, dbls, "mus-set-formant-radius-and-frequency", 2));
}
static void set_formant_radius_and_frequency_2f(int *args, int *ints, Float *dbls) 
{
  mus_set_formant_radius_and_frequency(((mus_any *)(INT_ARG_1)), FLOAT_ARG_2, FLOAT_ARG_3);
}  
static xen_value *set_formant_radius_and_frequency_1(ptree *prog, xen_value **args, int num_args)
{
  if (args[2]->type == R_INT) single_to_float(prog, args, 2);
  if (args[3]->type == R_INT) single_to_float(prog, args, 3);
  return(package(prog, R_BOOL, set_formant_radius_and_frequency_2f, descr_set_formant_radius_and_frequency_2f, args, 3));
}


static char *descr_move_locsig_2f(int *args, int *ints, Float *dbls) {return(descr_gen(args, ints, dbls, "move-locsig", 2));}
static void move_locsig_2f(int *args, int *ints, Float *dbls) {mus_move_locsig(((mus_any *)(INT_ARG_1)), FLOAT_ARG_2, FLOAT_ARG_3);}  
static xen_value *move_locsig_1(ptree *prog, xen_value **args, int num_args)
{ 
  return(package(prog, R_BOOL, move_locsig_2f, descr_move_locsig_2f, args, 3));
}


static char *descr_str_gen0(int *args, int *ints, Float *dbls, char *which)
{
  return(mus_format( STR_PT " = %s(" PTR_PT ")", args[0], STRING_RESULT, which, args[1], (mus_any *)(INT_ARG_1)));
}
#define STR_GEN0(Name) \
  static char *descr_ ## Name ## _0s(int *args, int *ints, Float *dbls) {return(descr_str_gen0(args, ints, dbls, #Name));} \
  static void Name ## _0s(int *args, int *ints, Float *dbls) {STRING_RESULT = copy_string(mus_ ## Name ((mus_any *)(INT_ARG_1)));} \
  static xen_value * mus_ ## Name ## _0(ptree *prog, xen_value **args, int num_args) \
  { \
    return(package(prog, R_STRING, Name ## _0s, descr_ ## Name ## _0s, args, 1)); \
  }

STR_GEN0(name)
STR_GEN0(describe)
STR_GEN0(inspect)
STR_GEN0(file_name)


static char *descr_ref_gen0(int *args, int *ints, Float *dbls, char *which)
{
  return(mus_format( FLT_PT " = %s(" PTR_PT ", " INT_PT ")", args[0], FLOAT_RESULT, which, args[1], (mus_any *)(INT_ARG_1), args[2], INT_ARG_2));
}
#define REF_GEN0(Name) \
  static char *descr_ ## Name ## _0r(int *args, int *ints, Float *dbls) {return(descr_ref_gen0(args, ints, dbls, #Name));} \
  static void Name ## _0r(int *args, int *ints, Float *dbls) {FLOAT_RESULT = mus_ ## Name ((mus_any *)(INT_ARG_1), INT_ARG_2);} \
  static xen_value * Name ## _0(ptree *prog, xen_value **args, int num_args) \
  { \
    return(package(prog, R_FLOAT, Name ## _0r, descr_ ## Name ## _0r, args, 2)); \
  }

REF_GEN0(frame_ref)
REF_GEN0(locsig_ref)
REF_GEN0(locsig_reverb_ref)


static char *descr_set_gen0(int *args, int *ints, Float *dbls, char *which)
{
  return(mus_format("%s(" PTR_PT ", " INT_PT ", " FLT_PT ")", which, args[1], (mus_any *)(INT_ARG_1), args[2], INT_ARG_2, args[3], FLOAT_ARG_3));
}
#define SET_GEN0(Name) \
  static char *descr_ ## Name ## _0r(int *args, int *ints, Float *dbls) {return(descr_set_gen0(args, ints, dbls, #Name));} \
  static void Name ## _0r(int *args, int *ints, Float *dbls) \
    { \
      mus_ ## Name ((mus_any *)(INT_ARG_1), INT_ARG_2, FLOAT_ARG_3); \
    } \
  static xen_value * Name ## _2(ptree *prog, xen_value **args, int num_args) \
  { \
    return(package(prog, R_FLOAT, Name ## _0r, descr_ ## Name ## _0r, args, 3)); \
  } \
  static void Name ## _1(ptree *prog, xen_value *in_v, xen_value *in_v1, xen_value *in_v2, xen_value *v) \
  { \
    add_triple_to_ptree(prog, va_make_triple( Name ## _0r, descr_ ## Name ## _0r, 4, NULL, in_v, in_v1, v)); \
  }

SET_GEN0(frame_set)
SET_GEN0(locsig_set)
SET_GEN0(locsig_reverb_set)


static char *descr_mixer_ref_0(int *args, int *ints, Float *dbls)
{
  return(mus_format( FLT_PT " = mixer-ref(" PTR_PT ", " INT_PT ", " INT_PT ")", 
		    args[0], FLOAT_RESULT, args[1], (mus_any *)(INT_ARG_1), args[2], INT_ARG_2, args[3], INT_ARG_3));
}
static void mixer_ref_0(int *args, int *ints, Float *dbls) {FLOAT_RESULT = mus_mixer_ref((mus_any *)(INT_ARG_1), INT_ARG_2, INT_ARG_3);}
static xen_value *mixer_ref_1(ptree *prog, xen_value **args, int num_args)
{
  return(package(prog, R_FLOAT, mixer_ref_0, descr_mixer_ref_0, args, 3));
}

static char *descr_mixer_set_0(int *args, int *ints, Float *dbls)
{
  return(mus_format("mixer-set!(" PTR_PT ", " INT_PT ", " INT_PT ", " FLT_PT ")", 
		    args[1], (mus_any *)(INT_ARG_1), args[2], INT_ARG_2, args[3], INT_ARG_3, args[4], FLOAT_ARG_4));
}
static void mixer_set_0(int *args, int *ints, Float *dbls) 
{
  mus_mixer_set((mus_any *)(INT_ARG_1), INT_ARG_2, INT_ARG_3, FLOAT_ARG_4);
}
static xen_value *mixer_set_2(ptree *prog, xen_value **args, int num_args)
{
  return(package(prog, R_FLOAT, mixer_set_0, descr_mixer_set_0, args, 4));
}
static void mixer_set_1(ptree *prog, xen_value *in_v, xen_value *in_v1, xen_value *in_v2, xen_value *v)
{
  add_triple_to_ptree(prog, va_make_triple(mixer_set_0, descr_mixer_set_0, 5, NULL, in_v, in_v1, in_v2, v));
}


static char *descr_set_int_gen0(int *args, int *ints, Float *dbls, char *which)
{
  return(mus_format("mus-%s(" PTR_PT ") = " INT_PT , which, args[0], (mus_any *)(INT_RESULT), args[1], INT_ARG_1));
}
#define SET_INT_GEN0(Name) \
  static char *descr_set_ ## Name ## _i(int *args, int *ints, Float *dbls) {return(descr_set_int_gen0(args, ints, dbls, #Name));} \
  static void set_ ## Name ## _i(int *args, int *ints, Float *dbls) {mus_set_ ## Name ((mus_any *)(INT_RESULT), INT_ARG_1);} \
  static void mus_set_ ## Name ## _1(ptree *prog, xen_value *in_v, xen_value *in_v1, xen_value *in_v2, xen_value *v) \
  { \
    add_triple_to_ptree(prog, va_make_triple(set_ ## Name ## _i, descr_set_ ## Name ## _i, 2, in_v, v)); \
  }

SET_INT_GEN0(location)
SET_INT_GEN0(ramp)
SET_INT_GEN0(hop)
SET_INT_GEN0(length)
SET_INT_GEN0(cosines)

static char *descr_set_dbl_gen0(int *args, int *ints, Float *dbls, char *which)
{
  return(mus_format("mus-%s(" PTR_PT ") = " FLT_PT , which, args[0], (mus_any *)(INT_RESULT), args[1], FLOAT_ARG_1));
}
#define SET_DBL_GEN0(Name) \
  static char *descr_set_ ## Name ## _f(int *args, int *ints, Float *dbls) {return(descr_set_dbl_gen0(args, ints, dbls, #Name));} \
  static void set_ ## Name ## _f(int *args, int *ints, Float *dbls) {mus_set_ ## Name ((mus_any *)(INT_RESULT), FLOAT_ARG_1);} \
  static void mus_set_ ## Name ## _1(ptree *prog, xen_value *in_v, xen_value *in_v1, xen_value *in_v2, xen_value *v) \
  { \
    add_triple_to_ptree(prog, va_make_triple(set_ ## Name ## _f, descr_set_ ## Name ## _f, 2, in_v, v)); \
  }

SET_DBL_GEN0(increment)
SET_DBL_GEN0(scaler)
SET_DBL_GEN0(width)
SET_DBL_GEN0(feedback)
SET_DBL_GEN0(feedforward)
SET_DBL_GEN0(a0)
SET_DBL_GEN0(a1)
SET_DBL_GEN0(a2)
SET_DBL_GEN0(b1)
SET_DBL_GEN0(b2)
SET_DBL_GEN0(x1)
SET_DBL_GEN0(x2)
SET_DBL_GEN0(y1)
SET_DBL_GEN0(y2)
SET_DBL_GEN0(phase)
SET_DBL_GEN0(frequency)
SET_DBL_GEN0(formant_radius)


/* ---------------- polynomial ---------------- */
static char *descr_polynomial_1f(int *args, int *ints, Float *dbls) 
{
  return(mus_format( FLT_PT " = polynomial(" PTR_PT ", " FLT_PT ")", args[0], FLOAT_RESULT, args[1], (vct *)(INT_ARG_1), args[2], FLOAT_ARG_2));
}
static void polynomial_1f(int *args, int *ints, Float *dbls) 
{
  FLOAT_RESULT = mus_polynomial(((vct *)(INT_ARG_1))->data, FLOAT_ARG_2, ((vct *)(INT_ARG_1))->length);
}
static xen_value *polynomial_1(ptree *prog, xen_value **args, int num_args) {return(package(prog, R_FLOAT, polynomial_1f, descr_polynomial_1f, args, 2));}

/* ---------------- mus-fft ---------------- */
static char *descr_mus_fft_2v(int *args, int *ints, Float *dbls) 
{
  return(mus_format("mus-fft(" PTR_PT ", " PTR_PT ")", args[1], (vct *)(INT_ARG_1), args[2], (vct *)(INT_ARG_2)));
}
static void mus_fft_2v(int *args, int *ints, Float *dbls) 
{
  mus_fft(((vct *)(INT_ARG_1))->data, ((vct *)(INT_ARG_2))->data, ((vct *)(INT_ARG_1))->length, 1);
  INT_RESULT = INT_ARG_1;
}
static char *descr_mus_fft_2v_1(int *args, int *ints, Float *dbls) 
{
  return(mus_format("mus-fft(" PTR_PT ", " PTR_PT ", " INT_PT ")", args[1], (vct *)(INT_ARG_1), args[2], (vct *)(INT_ARG_2), args[3], INT_ARG_3));
}
static void mus_fft_2v_1(int *args, int *ints, Float *dbls) 
{
  mus_fft(((vct *)(INT_ARG_1))->data, ((vct *)(INT_ARG_2))->data, INT_ARG_3, 1);
  INT_RESULT = INT_ARG_1;
}
static char *descr_mus_fft_2v_2(int *args, int *ints, Float *dbls) 
{
  return(mus_format("mus-fft(" PTR_PT ", " PTR_PT ", " INT_PT ", " INT_PT ")", 
		    args[1], (vct *)(INT_ARG_1), args[2], (vct *)(INT_ARG_2), args[3], INT_ARG_3, args[4], INT_ARG_4));
}
static void mus_fft_2v_2(int *args, int *ints, Float *dbls) 
{
  mus_fft(((vct *)(INT_ARG_1))->data, ((vct *)(INT_ARG_2))->data, INT_ARG_3, INT_ARG_4);
  INT_RESULT = INT_ARG_1;
}
static xen_value *mus_fft_1(ptree *prog, xen_value **args, int num_args) 
{
  if (num_args == 2) return(package(prog, R_CLM, mus_fft_2v, descr_mus_fft_2v, args, 2));
  if (num_args == 3) return(package(prog, R_CLM, mus_fft_2v_1, descr_mus_fft_2v_1, args, 3));
  return(package(prog, R_VCT, mus_fft_2v_2, descr_mus_fft_2v_2, args, 4));
}

/* ---------------- file->sample ---------------- */
static char *descr_file2sample_1f(int *args, int *ints, Float *dbls) 
{
  return(mus_format( FLT_PT " = file->sample(" PTR_PT ", " INT_PT ")", args[0], FLOAT_RESULT, args[1], (mus_any *)(INT_ARG_1), args[2], INT_ARG_2));
}
static char *descr_file2sample_2f(int *args, int *ints, Float *dbls) 
{
  return(mus_format( FLT_PT " = file->sample(" PTR_PT ", " INT_PT ", " INT_PT ")", 
		    args[0], FLOAT_RESULT, args[1], (mus_any *)(INT_ARG_1), args[2], INT_ARG_2, args[3], INT_ARG_3));
}
static void file2sample_1f(int *args, int *ints, Float *dbls) {FLOAT_RESULT = mus_file2sample((mus_any *)(INT_ARG_1), INT_ARG_2, 0);}
static void file2sample_2f(int *args, int *ints, Float *dbls) {FLOAT_RESULT = mus_file2sample((mus_any *)(INT_ARG_1), INT_ARG_2, INT_ARG_3);}
static xen_value *file2sample_1(ptree *prog, xen_value **args, int num_args) 
{
  if (num_args == 2) return(package(prog, R_FLOAT, file2sample_1f, descr_file2sample_1f, args, 2));
  return(package(prog, R_FLOAT, file2sample_2f, descr_file2sample_2f, args, 3));
}

static char *descr_sample2file_4(int *args, int *ints, Float *dbls) 
{
  return(mus_format( FLT_PT " = sample->file(" PTR_PT ", " INT_PT ", " INT_PT ", " FLT_PT ")", 
		    args[0], FLOAT_RESULT, args[1], (mus_any *)(INT_ARG_1), args[2], INT_ARG_2, args[3], INT_ARG_3, args[4], FLOAT_ARG_4));
}
static void sample2file_4(int *args, int *ints, Float *dbls) {FLOAT_RESULT = mus_sample2file((mus_any *)(INT_ARG_1), INT_ARG_2, INT_ARG_3, FLOAT_ARG_4);}
static xen_value *sample2file_1(ptree *prog, xen_value **args, int num_args) 
{
  return(package(prog, R_FLOAT, sample2file_4, descr_sample2file_4, args, 4));
}


/* ---------------- locsig ---------------- */
static char *descr_locsig_3(int *args, int *ints, Float *dbls) 
{
  return(mus_format( PTR_PT " = locsig((" PTR_PT ", " INT_PT ", " FLT_PT ")", 
		    args[0], (void *)(INT_RESULT), args[1], (mus_any *)(INT_ARG_1), args[2], INT_ARG_2, args[3], FLOAT_ARG_3));
}
static void locsig_3(int *args, int *ints, Float *dbls) 
{
  /* frame result should not be freed */
  INT_RESULT = (int)mus_locsig((mus_any *)(INT_ARG_1), INT_ARG_2, FLOAT_ARG_3);
}
static xen_value *locsig_1(ptree *prog, xen_value **args, int num_args) {return(package(prog, R_CLM, locsig_3, descr_locsig_3, args, 3));}


/* ---------------- env-interp ---------------- */
static char *descr_env_interp_2(int *args, int *ints, Float *dbls) 
{
  return(mus_format( FLT_PT " = env-interp(" FLT_PT ", " PTR_PT ")", args[0], FLOAT_RESULT, args[1], FLOAT_ARG_1, args[2], (mus_any *)(INT_ARG_2)));
}
static void env_interp_2(int *args, int *ints, Float *dbls) {FLOAT_RESULT = mus_env_interp(FLOAT_ARG_1, (mus_any *)(INT_ARG_2));}
static xen_value *env_interp_1(ptree *prog, xen_value **args, int num_args) 
{
  return(package(prog, R_FLOAT, env_interp_2, descr_env_interp_2, args, 2));
}


/* ---------------- frame+ etc ---------------- */

#define FRAME_OP(CName, SName) \
static char *descr_ ## CName ## _2(int *args, int *ints, Float *dbls) \
{ \
  return(mus_format( PTR_PT " = " #SName "(" PTR_PT ", " PTR_PT ")", \
		    args[0], (mus_any *)(INT_RESULT), args[1], (mus_any *)(INT_ARG_1), args[2], (mus_any *)(INT_ARG_2))); \
} \
static char *descr_ ## CName ## _3(int *args, int *ints, Float *dbls) \
{ \
  return(mus_format( PTR_PT " = " #SName "(" PTR_PT ", " PTR_PT ", " PTR_PT ")", \
		    args[0], (mus_any *)(INT_RESULT), args[1], (mus_any *)(INT_ARG_1), args[2], (mus_any *)(INT_ARG_2), args[3], (mus_any *)(INT_ARG_3))); \
} \
static void CName ## _2(int *args, int *ints, Float *dbls) \
{ \
  if (INT_RESULT) mus_free((mus_any *)(INT_RESULT)); \
  INT_RESULT = (int)CName((mus_any *)(INT_ARG_1), (mus_any *)(INT_ARG_2), NULL); \
} \
static void CName ## _3(int *args, int *ints, Float *dbls) \
{ \
  INT_RESULT = (int)CName((mus_any *)(INT_ARG_1), (mus_any *)(INT_ARG_2), (mus_any *)(INT_ARG_3)); \
} \
static xen_value * CName ## _1(ptree *prog, xen_value **args, int num_args) \
{ \
  if (num_args == 2) return(package(prog, R_CLM, CName ## _2, descr_ ## CName ## _2, args, 2)); \
  return(package(prog, R_CLM, CName ## _3, descr_ ## CName ## _3, args, 3)); \
}

FRAME_OP(mus_frame_add, frame+)
FRAME_OP(mus_frame_multiply, frame*)
FRAME_OP(mus_frame2frame, frame->frame)
FRAME_OP(mus_mixer_multiply, mixer*)


/* ---------------- frame->sample ---------------- */
static char *descr_frame2sample_2(int *args, int *ints, Float *dbls) 
{
  return(mus_format( FLT_PT " = frame->sample(" PTR_PT ", " PTR_PT ")", args[0], FLOAT_RESULT, args[1], (mus_any *)(INT_ARG_1), args[2], (mus_any *)(INT_ARG_2)));
}
static void frame2sample_2(int *args, int *ints, Float *dbls) 
{
  FLOAT_RESULT = mus_frame2sample((mus_any *)(INT_ARG_1), (mus_any *)(INT_ARG_2));
}
static xen_value *frame2sample_1(ptree *prog, xen_value **args, int num_args) 
{
  return(package(prog, R_FLOAT, frame2sample_2, descr_frame2sample_2, args, 2));
}

/* ---------------- sample->frame ---------------- */
static char *descr_sample2frame_2(int *args, int *ints, Float *dbls) 
{
  return(mus_format( PTR_PT " = sample->frame(" PTR_PT ", " FLT_PT ")", 
		     args[0], (mus_any *)(INT_RESULT), args[1], (mus_any *)(INT_ARG_1), args[2], FLOAT_ARG_2));
}
static void sample2frame_2(int *args, int *ints, Float *dbls) {INT_RESULT = (int)mus_sample2frame((mus_any *)(INT_ARG_1), FLOAT_ARG_2, NULL);}
static char *descr_sample2frame_3(int *args, int *ints, Float *dbls) 
{
  return(mus_format( PTR_PT " = sample->frame(" PTR_PT ", " FLT_PT ", " PTR_PT ")", 
		     args[0], (mus_any *)(INT_RESULT), args[1], (mus_any *)(INT_ARG_1), args[2], FLOAT_ARG_2, args[3], (mus_any *)(INT_ARG_3)));
}
static void sample2frame_3(int *args, int *ints, Float *dbls) 
{
  INT_RESULT = (int)mus_sample2frame((mus_any *)(INT_ARG_1), FLOAT_ARG_2, (mus_any *)(INT_ARG_3));
}
static xen_value *sample2frame_1(ptree *prog, xen_value **args, int num_args) 
{
  if (num_args == 2) return(package(prog, R_CLM, sample2frame_2, descr_sample2frame_2, args, 2));
  return(package(prog, R_CLM, sample2frame_3, descr_sample2frame_3, args, 3));
}

/* ---------------- frame->buffer ---------------- */
static char *descr_frame2buffer_2(int *args, int *ints, Float *dbls) 
{
  return(mus_format( PTR_PT " = frame->buffer(" PTR_PT ", " PTR_PT ")", 
		    args[0], (mus_any *)INT_RESULT, args[1], (mus_any *)(INT_ARG_1), args[2], (mus_any *)(INT_ARG_2)));
}
static void frame2buffer_2(int *args, int *ints, Float *dbls) 
{
  INT_RESULT = (int)mus_frame2buffer((mus_any *)(INT_ARG_1), (mus_any *)(INT_ARG_2));
}
static xen_value *frame2buffer_1(ptree *prog, xen_value **args, int num_args) 
{
  return(package(prog, R_CLM, frame2buffer_2, descr_frame2buffer_2, args, 2));
}


/* ---------------- buffer->frame ---------------- */
static char *descr_buffer2frame_1b(int *args, int *ints, Float *dbls) 
{
  return(mus_format( PTR_PT " = buffer->frame(" PTR_PT ")", args[0], (mus_any *)INT_RESULT, args[1], (mus_any *)(INT_ARG_1)));
}
static void buffer2frame_1b(int *args, int *ints, Float *dbls) {INT_RESULT = (int)mus_buffer2frame((mus_any *)(INT_ARG_1), NULL);}
static char *descr_buffer2frame_2b(int *args, int *ints, Float *dbls) 
{
  return(mus_format( PTR_PT " = buffer->frame(" PTR_PT ", " PTR_PT ")", 
		    args[0], (mus_any *)INT_RESULT, args[1], (mus_any *)(INT_ARG_1), args[2], (mus_any *)(INT_ARG_2)));
}
static void buffer2frame_2b(int *args, int *ints, Float *dbls) {INT_RESULT = (int)mus_buffer2frame((mus_any *)(INT_ARG_1), (mus_any *)(INT_ARG_2));}
static xen_value *buffer2frame_1(ptree *prog, xen_value **args, int num_args) 
{
  if (num_args == 1) return(package(prog, R_CLM, buffer2frame_1b, descr_buffer2frame_1b, args, 1));
  return(package(prog, R_CLM, buffer2frame_2b, descr_buffer2frame_2b, args, 2));
}


/* ---------------- frame->file ---------------- */
static char *descr_frame2file_3(int *args, int *ints, Float *dbls) 
{
  return(mus_format( PTR_PT " = frame->file((" PTR_PT ", " INT_PT ", " PTR_PT ")", 
		    args[0], (void *)(INT_RESULT), args[1], (mus_any *)(INT_ARG_1), args[2], INT_ARG_2, args[3], (mus_any *)(INT_ARG_3)));
}
static void frame2file_3(int *args, int *ints, Float *dbls) 
{
  INT_RESULT = (int)mus_frame2file((mus_any *)(INT_ARG_1), INT_ARG_2, (mus_any *)(INT_ARG_3));
}
static xen_value *frame2file_1(ptree *prog, xen_value **args, int num_args) 
{
  return(package(prog, R_CLM, frame2file_3, descr_frame2file_3, args, 3));
}

/* ---------------- file->frame ---------------- */
static char *descr_file2frame_3(int *args, int *ints, Float *dbls) 
{
  return(mus_format( PTR_PT " = file->frame((" PTR_PT ", " INT_PT ", " PTR_PT ")", 
		    args[0], (void *)(INT_RESULT), args[1], (mus_any *)(INT_ARG_1), args[2], INT_ARG_2, args[3], (mus_any *)(INT_ARG_3)));
}
static void file2frame_3(int *args, int *ints, Float *dbls) 
{
  INT_RESULT = (int)mus_file2frame((mus_any *)(INT_ARG_1), INT_ARG_2, (mus_any *)(INT_ARG_3));
}

static char *descr_file2frame_2(int *args, int *ints, Float *dbls) 
{
  return(mus_format( PTR_PT " = file->frame((" PTR_PT ", " INT_PT ")", 
		     args[0], (void *)(INT_RESULT), args[1], (mus_any *)(INT_ARG_1), args[2], INT_ARG_2));
}
static void file2frame_2(int *args, int *ints, Float *dbls) 
{
  if (INT_RESULT) mus_free((mus_any *)(INT_RESULT));
  INT_RESULT = (int)(mus_file2frame((mus_any *)(INT_ARG_1), INT_ARG_2, NULL));
}
static xen_value *file2frame_1(ptree *prog, xen_value **args, int num_args) 
{
  if (num_args == 2)
    return(package(prog, R_CLM, file2frame_2, descr_file2frame_2, args, 2));
  return(package(prog, R_CLM, file2frame_3, descr_file2frame_3, args, 3));
}


/* ---------------- outa ---------------- */

#define OUT_GEN(CName, SName) \
static char *descr_ ## CName ## _3(int *args, int *ints, Float *dbls) \
{ \
  return(mus_format( FLT_PT " = " #SName "(" INT_PT ", " FLT_PT ", " PTR_PT ")", \
		    args[0], FLOAT_RESULT, args[1], INT_ARG_1, args[2], FLOAT_ARG_2, args[3], (mus_any *)(INT_ARG_3))); \
} \
static void CName ## _3(int *args, int *ints, Float *dbls) {FLOAT_RESULT = mus_ ## CName (INT_ARG_1, FLOAT_ARG_2, (mus_any *)(INT_ARG_3));} \
static xen_value * CName ## _1(ptree *prog, xen_value **args, int num_args) \
{ \
  return(package(prog, R_FLOAT, CName ## _3, descr_ ## CName ## _3, args, 3)); \
}

OUT_GEN(outa, outa)
OUT_GEN(outb, outb)
OUT_GEN(outc, outc)
OUT_GEN(outd, outd)

static char *descr_out_any_4(int *args, int *ints, Float *dbls)
{
  return(mus_format( FLT_PT " = out-any(" INT_PT ", " FLT_PT ", " INT_PT ", " PTR_PT ")",
		    args[0], FLOAT_RESULT, args[1], INT_ARG_1, args[2], FLOAT_ARG_2, args[3], INT_ARG_3, args[4], (mus_any *)(INT_ARG_4)));
}
static void out_any_4(int *args, int *ints, Float *dbls) {FLOAT_RESULT = mus_out_any(INT_ARG_1, FLOAT_ARG_2, INT_ARG_3, (mus_any *)(INT_ARG_4));} 
static xen_value *out_any_1(ptree *prog, xen_value **args, int num_args)
{
  return(package(prog, R_FLOAT, out_any_4, descr_out_any_4, args, 4));
}


/* ---------------- ina ---------------- */

#define IN_GEN(CName, SName) \
static char *descr_ ## CName ## _2(int *args, int *ints, Float *dbls) \
{ \
  return(mus_format( FLT_PT " = " #SName "(" INT_PT ", " PTR_PT ")", args[0], FLOAT_RESULT, args[1], INT_ARG_1, args[2], (mus_any *)(INT_ARG_2))); \
} \
static void CName ## _2(int *args, int *ints, Float *dbls) {FLOAT_RESULT = mus_ ## CName (INT_ARG_1, (mus_any *)(INT_ARG_2));} \
static xen_value * CName ## _1(ptree *prog, xen_value **args, int num_args) \
{ \
  return(package(prog, R_FLOAT, CName ## _2, descr_ ## CName ## _2, args, 2)); \
}

IN_GEN(ina, ina)
IN_GEN(inb, inb)

static char *descr_in_any_3(int *args, int *ints, Float *dbls)
{
  return(mus_format( FLT_PT " = in-any(" INT_PT ", " INT_PT ", " PTR_PT ")",
		    args[0], FLOAT_RESULT, args[1], INT_ARG_1, args[2], INT_ARG_2, args[3], (mus_any *)(INT_ARG_3)));
}
static void in_any_3(int *args, int *ints, Float *dbls) {FLOAT_RESULT = mus_in_any(INT_ARG_1, INT_ARG_2, (mus_any *)(INT_ARG_3));}
static xen_value *in_any_1(ptree *prog, xen_value **args, int num_args)
{
  return(package(prog, R_FLOAT, in_any_3, descr_in_any_3, args, 3));
}




/* ---------------- array-interp ---------------- */
static char *descr_array_interp_1f(int *args, int *ints, Float *dbls) 
{
  return(mus_format( FLT_PT " = array-interp(" PTR_PT ", " FLT_PT ")", args[0], FLOAT_RESULT, args[1], (vct *)(INT_ARG_1), args[2], FLOAT_ARG_2));
}
static void array_interp_1f(int *args, int *ints, Float *dbls) 
{
  FLOAT_RESULT = mus_array_interp(((vct *)(INT_ARG_1))->data, FLOAT_ARG_2, ((vct *)(INT_ARG_1))->length);
}
static char *descr_array_interp_2f(int *args, int *ints, Float *dbls) 
{
  return(mus_format( FLT_PT " = array-interp(" PTR_PT ", " FLT_PT ", " INT_PT ")", 
		     args[0], FLOAT_RESULT, args[1], (vct *)(INT_ARG_1), args[2], FLOAT_ARG_2, args[3], INT_ARG_3));
}
static void array_interp_2f(int *args, int *ints, Float *dbls) {FLOAT_RESULT = mus_array_interp(((vct *)(INT_ARG_1))->data, FLOAT_ARG_2, INT_ARG_3);}
static xen_value *array_interp_1(ptree *prog, xen_value **args, int num_args) 
{
  if (num_args == 2) return(package(prog, R_FLOAT, array_interp_1f, descr_array_interp_1f, args, 2));
  return(package(prog, R_FLOAT, array_interp_2f, descr_array_interp_2f, args, 3));
}

/* ---------------- vct-peak ---------------- */
static char *descr_vct_peak_v(int *args, int *ints, Float *dbls) 
{
  return(mus_format( FLT_PT " = vct-peak(" PTR_PT ")", args[0], FLOAT_RESULT, args[1], (vct *)(INT_ARG_1)));
}
static void vct_peak_v(int *args, int *ints, Float *dbls) 
{
  int i;
  Float val = 0.0, absv;
  vct *v;
  v = (vct *)(INT_ARG_1);
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
static char *descr_clear_array_1f(int *args, int *ints, Float *dbls) {return(mus_format("clear_array(" PTR_PT ")", args[1], (vct *)(INT_ARG_1)));}
static void clear_array_1f(int *args, int *ints, Float *dbls) 
{
  mus_clear_array(((vct *)(INT_ARG_1))->data, ((vct *)(INT_ARG_1))->length);
  INT_RESULT = INT_ARG_1;
}
static xen_value *clear_array_1(ptree *prog, xen_value **args, int num_args) 
{
  return(package(prog, R_VCT, clear_array_1f, descr_clear_array_1f, args, 1));
}


/* ---------------- restart_env ---------------- */
static char *descr_restart_env_1f(int *args, int *ints, Float *dbls) {return(mus_format("restart-env(" PTR_PT ")", args[1], (mus_any *)(INT_ARG_1)));}
static void restart_env_1f(int *args, int *ints, Float *dbls) {mus_restart_env((mus_any *)(INT_ARG_1)); INT_RESULT = INT_ARG_1;}
static xen_value *restart_env_1(ptree *prog, xen_value **args, int num_args) 
{
  return(package(prog, R_CLM, restart_env_1f, descr_restart_env_1f, args, 1));
}


/* ---------------- dot-product etc ---------------- */

#define VCT_2_I(CName, SName) \
static char *descr_ ## CName ## _2f(int *args, int *ints, Float *dbls)  \
{ \
  return(mus_format(#SName "(" PTR_PT ", " PTR_PT ")", args[1], (vct *)(INT_ARG_1), args[2], (vct *)(INT_ARG_2))); \
} \
static void CName ## _2f(int *args, int *ints, Float *dbls)  \
{ \
  mus_ ## CName(((vct *)(INT_ARG_1))->data, ((vct *)(INT_ARG_2))->data, ((vct *)(INT_ARG_1))->length); \
  INT_RESULT = INT_ARG_1; \
} \
static char *descr_ ## CName ## _3f(int *args, int *ints, Float *dbls)  \
{ \
  return(mus_format(#SName "(" PTR_PT ", " PTR_PT ", " INT_PT ")",  \
                    args[1], (vct *)(INT_ARG_1), args[2], (vct *)(INT_ARG_2), args[3], INT_ARG_3)); \
} \
static void CName ## _3f(int *args, int *ints, Float *dbls)  \
{ \
  mus_ ## CName(((vct *)(INT_ARG_1))->data, ((vct *)(INT_ARG_2))->data, INT_ARG_3); \
  INT_RESULT = INT_ARG_1; \
} \
static xen_value * CName ## _1(ptree *prog, xen_value **args, int num_args)  \
{ \
  if (num_args == 2) \
    return(package(prog, R_VCT, CName ## _2f, descr_ ## CName ## _2f, args, 2)); \
  else return(package(prog, R_VCT, CName ## _3f, descr_ ## CName ## _3f, args, 3)); \
}

VCT_2_I(rectangular2polar, rectangular->polar)
VCT_2_I(polar2rectangular, polar->rectangular)
VCT_2_I(multiply_arrays, multiply-arrays)
VCT_2_I(convolution, convolution)


#define VCT_2_F(CName, SName) \
static char *descr_ ## CName ## _2f(int *args, int *ints, Float *dbls)  \
{ \
  return(mus_format( FLT_PT " = " #SName "(" PTR_PT ", " PTR_PT ")", args[0], FLOAT_RESULT, args[1], (vct *)(INT_ARG_1), args[2], (vct *)(INT_ARG_2))); \
} \
static void CName ## _2f(int *args, int *ints, Float *dbls)  \
{ \
  FLOAT_RESULT = mus_ ## CName(((vct *)(INT_ARG_1))->data, ((vct *)(INT_ARG_2))->data, ((vct *)(INT_ARG_1))->length); \
} \
static char *descr_ ## CName ## _3f(int *args, int *ints, Float *dbls)  \
{ \
  return(mus_format( FLT_PT " = " #SName "(" PTR_PT ", " PTR_PT ", " INT_PT ")",  \
                    args[0], FLOAT_RESULT, args[1], (vct *)(INT_ARG_1), args[2], (vct *)(INT_ARG_2), args[3], INT_ARG_3)); \
} \
static void CName ## _3f(int *args, int *ints, Float *dbls)  \
{ \
  FLOAT_RESULT = mus_ ## CName(((vct *)(INT_ARG_1))->data, ((vct *)(INT_ARG_2))->data, INT_ARG_3); \
} \
static xen_value * CName ## _1(ptree *prog, xen_value **args, int num_args)  \
{ \
  if (num_args == 2) \
    return(package(prog, R_FLOAT, CName ## _2f, descr_ ## CName ## _2f, args, 2)); \
  else return(package(prog, R_FLOAT, CName ## _3f, descr_ ## CName ## _3f, args, 3)); \
}

VCT_2_F(sum_of_sines, sum-of-sines)
VCT_2_F(dot_product, dot-product)

static void clm_0f(int *args, int *ints, Float *dbls) {FLOAT_RESULT = MUS_RUN(((mus_any *)(INT_ARG_1)), 0.0, 0.0);}
static char *descr_clm_0f(int *args, int *ints, Float *dbls) {return(descr_gen(args, ints, dbls, mus_name(((mus_any *)(INT_ARG_1))), 0));} 
static void clm_1f(int *args, int *ints, Float *dbls) {FLOAT_RESULT = MUS_RUN(((mus_any *)(INT_ARG_1)), FLOAT_ARG_2, 0.0);}
static char *descr_clm_1f(int *args, int *ints, Float *dbls) {return(descr_gen(args, ints, dbls, mus_name(((mus_any *)(INT_ARG_1))), 1));} 
static void clm_2f(int *args, int *ints, Float *dbls) {FLOAT_RESULT = MUS_RUN(((mus_any *)(INT_ARG_1)), FLOAT_ARG_2, FLOAT_ARG_3);}
static char *descr_clm_2f(int *args, int *ints, Float *dbls) {return(descr_gen(args, ints, dbls, mus_name(((mus_any *)(INT_ARG_1))), 2));} 
static xen_value *clm_n(ptree *prog, xen_value **args, int num_args, xen_value *sf)
{
  if (args[0]) FREE(args[0]);
  args[0] = make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, 0.0), R_VARIABLE);
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
static char *descr_mus_spectrum_3v(int *args, int *ints, Float *dbls) 
{
  return(mus_format("spectrum(" PTR_PT ", " PTR_PT ", " PTR_PT ")", 
		    args[1], (vct *)(INT_ARG_1), args[2], (vct *)(INT_ARG_2), args[3], (vct *)(INT_ARG_3)));
}
static void mus_spectrum_3v(int *args, int *ints, Float *dbls) 
{
  mus_spectrum(((vct *)(INT_ARG_1))->data, ((vct *)(INT_ARG_2))->data, ((vct *)(INT_ARG_3))->data, ((vct *)(INT_ARG_1))->length, 1);
}
static char *descr_mus_spectrum_4v(int *args, int *ints, Float *dbls) 
{
  return(mus_format("spectrum(" PTR_PT ", " PTR_PT ", " PTR_PT ", " INT_PT ")", 
		    args[1], (vct *)(INT_ARG_1), args[2], (vct *)(INT_ARG_2), args[3], (vct *)(INT_ARG_3), args[4], INT_ARG_4));
}
static void mus_spectrum_4v(int *args, int *ints, Float *dbls) 
{
  mus_spectrum(((vct *)(INT_ARG_1))->data, ((vct *)(INT_ARG_2))->data, ((vct *)(INT_ARG_3))->data, INT_ARG_4, 1);
}
static char *descr_mus_spectrum_5v(int *args, int *ints, Float *dbls) 
{
  return(mus_format("spectrum(" PTR_PT ", " PTR_PT ", " PTR_PT ", " INT_PT ", " INT_PT ")", 
		    args[1], (vct *)(INT_ARG_1), args[2], (vct *)(INT_ARG_2), args[3], (vct *)(INT_ARG_3), args[4], INT_ARG_4, args[5], INT_ARG_5));
}
static void mus_spectrum_5v(int *args, int *ints, Float *dbls) 
{
  mus_spectrum(((vct *)(INT_ARG_1))->data, ((vct *)(INT_ARG_2))->data, ((vct *)(INT_ARG_3))->data, INT_ARG_4, INT_ARG_5);
}
static xen_value *mus_spectrum_1(ptree *prog, xen_value **args, int num_args) 
{
  if (num_args == 3) return(package(prog, R_BOOL, mus_spectrum_3v, descr_mus_spectrum_3v, args, 3));
  if ((num_args == 4) && (args[4]->type == R_INT))
    return(package(prog, R_BOOL, mus_spectrum_4v, descr_mus_spectrum_4v, args, 4));
  return(package(prog, R_BOOL, mus_spectrum_5v, descr_mus_spectrum_5v, args, 5));
}


/* ---------------- src ---------------- */

static Float src_input(void *arg, int direction)
{
  mus_xen *gn = (mus_xen *)arg;
  ptree *pt, *outer;
  pt = (ptree *)(gn->input_ptree);
  outer = (ptree *)(pt->outer_tree);
  outer->ints[pt->args[0]] = direction;
  eval_embedded_ptree(pt, outer->ints, outer->dbls);
  return(outer->dbls[pt->result->addr]);
}
static char *descr_src_2f(int *args, int *ints, Float *dbls) 
{
  return(mus_format( FLT_PT " = src(" PTR_PT ", " FLT_PT ", " PTR_PT ")",
		    args[0], FLOAT_RESULT, args[1], (mus_any *)(INT_ARG_1), args[2], FLOAT_ARG_2, args[3], (ptree *)(INT_ARG_3)));
}
static void src_2f(int *args, int *ints, Float *dbls) 
{
  ((mus_xen *)mus_environ((mus_any *)(INT_ARG_1)))->input_ptree = (void *)(INT_ARG_3);
  FLOAT_RESULT = mus_src(((mus_any *)(INT_ARG_1)), FLOAT_ARG_2, src_input);
}
static char *descr_src_1f(int *args, int *ints, Float *dbls) 
{
  return(mus_format( FLT_PT " = src(" PTR_PT ", " FLT_PT ")", args[0], FLOAT_RESULT, args[1], (mus_any *)(INT_ARG_1), args[2], FLOAT_ARG_2));
}
static void src_1f(int *args, int *ints, Float *dbls) {FLOAT_RESULT = mus_src(((mus_any *)(INT_ARG_1)), FLOAT_ARG_2, NULL);}
static char *descr_src_0f(int *args, int *ints, Float *dbls) 
{
  return(mus_format( FLT_PT " = src(" PTR_PT ")", args[0], FLOAT_RESULT, args[1], (mus_any *)(INT_ARG_1)));
}
static void src_0f(int *args, int *ints, Float *dbls) {FLOAT_RESULT = mus_src(((mus_any *)(INT_ARG_1)), 0.0, NULL);}
static xen_value *src_1(ptree *prog, xen_value **args, int num_args) 
{
  if ((num_args > 1) && (args[2]->type == R_INT)) single_to_float(prog, args, 2);
  if (num_args == 3)
    {
      ptree *pt;
      pt = (ptree *)(prog->ints[args[3]->addr]);
      if (pt->arity != 1) return(run_warn("src: wrong number of args to input function"));
    }
  switch (num_args)
    {
    case 1: return(package(prog, R_FLOAT, src_0f, descr_src_0f, args, 1)); break;
    case 2: return(package(prog, R_FLOAT, src_1f, descr_src_1f, args, 2)); break;
    default: return(package(prog, R_FLOAT, src_2f, descr_src_2f, args, 3)); break;
    }
}

#define GEN_AS_NEEDED(Name) \
static char *descr_ ## Name ## _1f(int *args, int *ints, Float *dbls)  \
{ \
  return(mus_format( FLT_PT " = " #Name "(" PTR_PT ", " PTR_PT ")", args[0], FLOAT_RESULT, args[1], (mus_any *)(INT_ARG_1), args[2], (ptree *)(INT_ARG_2))); \
} \
static void Name ## _1f(int *args, int *ints, Float *dbls)  \
{ \
  ((mus_xen *)mus_environ((mus_any *)(INT_ARG_1)))->input_ptree = (void *)(INT_ARG_2); \
  FLOAT_RESULT = mus_ ## Name(((mus_any *)(INT_ARG_1)), src_input); \
} \
static char *descr_ ## Name ## _0f(int *args, int *ints, Float *dbls)  \
{ \
  return(mus_format( FLT_PT " = " #Name "(" PTR_PT ")", args[0], FLOAT_RESULT, args[1], (mus_any *)(INT_ARG_1))); \
} \
static void Name ## _0f(int *args, int *ints, Float *dbls) {FLOAT_RESULT = mus_ ## Name(((mus_any *)(INT_ARG_1)), NULL);} \
static xen_value * Name ## _1(ptree *prog, xen_value **args, int num_args)  \
{ \
  if (num_args == 2) \
    { \
      ptree *pt; \
      pt = (ptree *)(prog->ints[args[2]->addr]); \
      if (pt->arity != 1) return(run_warn(#Name ": wrong number of args to input function")); \
    } \
  if (num_args == 1) \
    return(package(prog, R_FLOAT, Name ## _0f, descr_ ## Name ## _0f, args, 1)); \
  else return(package(prog, R_FLOAT, Name ## _1f, descr_ ## Name ## _1f, args, 2)); \
}

GEN_AS_NEEDED(convolve)
GEN_AS_NEEDED(granulate)
GEN_AS_NEEDED(phase_vocoder)

GEN_P(src)
GEN_P(convolve)
GEN_P(granulate)
GEN_P(phase_vocoder)


/* ---------------- contrast_enhancement ---------------- */

static void contrast_enhancement_f(int *args, int *ints, Float *dbls) {FLOAT_RESULT = mus_contrast_enhancement(FLOAT_ARG_1, FLOAT_ARG_2);}
static char *descr_contrast_enhancement_f(int *args, int *ints, Float *dbls) 
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

static void ring_modulate_f(int *args, int *ints, Float *dbls) {FLOAT_RESULT = mus_ring_modulate(FLOAT_ARG_1, FLOAT_ARG_2);}
static char *descr_ring_modulate_f(int *args, int *ints, Float *dbls) 
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

static void amplitude_modulate_f(int *args, int *ints, Float *dbls) {FLOAT_RESULT = mus_amplitude_modulate(FLOAT_ARG_1, FLOAT_ARG_2, FLOAT_ARG_3);}
static char *descr_amplitude_modulate_f(int *args, int *ints, Float *dbls) 
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
static void CName ## _f(int *args, int *ints, Float *dbls) {INT_RESULT = (int)CName(STRING_ARG_1);} \
static char *descr_ ## CName ## _f(int *args, int *ints, Float *dbls)  \
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

static void mus_sound_duration_f(int *args, int *ints, Float *dbls) {FLOAT_RESULT = mus_sound_duration(STRING_ARG_1);}
static char *descr_mus_sound_duration_f(int *args, int *ints, Float *dbls)
{
  return(mus_format( FLT_PT " = sound-duration(" STR_PT ")", args[0], FLOAT_RESULT, args[1], STRING_ARG_1));
}
static xen_value *mus_sound_duration_1(ptree *prog, xen_value **args, int num_args)
{
  return(package(prog, R_FLOAT, mus_sound_duration_f, descr_mus_sound_duration_f, args, 1));
}

static void mus_sound_comment_f(int *args, int *ints, Float *dbls) 
{
  if (STRING_RESULT) FREE(STRING_RESULT);
  STRING_RESULT = mus_sound_comment(STRING_ARG_1);
}
static char *descr_mus_sound_comment_f(int *args, int *ints, Float *dbls)
{
  return(mus_format( STR_PT " = sound-comment(" STR_PT ")", args[0], STRING_RESULT, args[1], STRING_ARG_1));
}
static xen_value *mus_sound_comment_1(ptree *prog, xen_value **args, int num_args)
{
  return(package(prog, R_STRING, mus_sound_comment_f, descr_mus_sound_comment_f, args, 1));
}

static void mus_header_type_name_f(int *args, int *ints, Float *dbls) 
{
  if (STRING_RESULT) FREE(STRING_RESULT);
  STRING_RESULT = copy_string(mus_header_type_name(INT_ARG_1));
}
static char *descr_mus_header_type_name_f(int *args, int *ints, Float *dbls)
{
  return(mus_format( STR_PT " = header-type-name(" INT_PT ")", args[0], STRING_RESULT, args[1], INT_ARG_1));
}
static xen_value *mus_header_type_name_1(ptree *prog, xen_value **args, int num_args)
{
  return(package(prog, R_STRING, mus_header_type_name_f, descr_mus_header_type_name_f, args, 1));
}

static void mus_data_format_name_f(int *args, int *ints, Float *dbls) 
{
  if (STRING_RESULT) FREE(STRING_RESULT);
  STRING_RESULT = copy_string(mus_data_format_name(INT_ARG_1));
}
static char *descr_mus_data_format_name_f(int *args, int *ints, Float *dbls)
{
  return(mus_format( STR_PT " = data-format-name(" INT_PT ")", args[0], STRING_RESULT, args[1], INT_ARG_1));
}
static xen_value *mus_data_format_name_1(ptree *prog, xen_value **args, int num_args)
{
  return(package(prog, R_STRING, mus_data_format_name_f, descr_mus_data_format_name_f, args, 1));
}


/* ---------------- formant-bank ---------------- */
static char *descr_formant_bank_f(int *args, int *ints, Float *dbls) 
{
  return(mus_format( FLT_PT " = formant-bank(" PTR_PT ", " PTR_PT ", " FLT_PT ")", 
		    args[0], FLOAT_RESULT, args[1], (vct *)(INT_ARG_1), args[2], (mus_any *)(INT_ARG_2), args[3], FLOAT_ARG_3));
}
static void formant_bank_f(int *args, int *ints, Float *dbls) 
{
  FLOAT_RESULT = mus_formant_bank(((vct *)(INT_ARG_1))->data, 
				  ((clm_vct *)(INT_ARG_2))->data, 
				  FLOAT_ARG_3, 
				  ((vct *)(INT_ARG_1))->length);
}
static xen_value *formant_bank_1(ptree *prog, xen_value **args, int num_args) 
{
  if (args[3]->type == R_INT) single_to_float(prog, args, 3);
  return(package(prog, R_FLOAT, formant_bank_f, descr_formant_bank_f, args, 3));
}


/* ---------------- mus-srate ---------------- */
static char *descr_srate_f(int *args, int *ints, Float *dbls) {return(mus_format( FLT_PT " = mus-srate", args[0], FLOAT_RESULT));}
static void srate_f(int *args, int *ints, Float *dbls) {FLOAT_RESULT = mus_srate();}
static xen_value *mus_srate_1(ptree *prog, xen_value **args, int num_args) 
{
  return(package(prog, R_FLOAT, srate_f, descr_srate_f, args, 0));
}

static char *descr_set_srate_f(int *args, int *ints, Float *dbls) {return(mus_format("mus-srate = " FLT_PT , args[1], FLOAT_ARG_1));}
static void set_srate_f(int *args, int *ints, Float *dbls) {mus_set_srate(FLOAT_ARG_1); FLOAT_RESULT = mus_srate();}
static char *descr_set_srate_i(int *args, int *ints, Float *dbls) {return(mus_format("mus-srate = " INT_PT , args[1], INT_ARG_1));}
static void set_srate_i(int *args, int *ints, Float *dbls) {mus_set_srate((Float)INT_ARG_1); FLOAT_RESULT = mus_srate();}
static void mus_set_srate_1(ptree *prog, xen_value *in_v, xen_value *in_v1, xen_value *in_v2, xen_value *v)
{
  if (v->type == R_FLOAT)
    add_triple_to_ptree(prog, va_make_triple(set_srate_f, descr_set_srate_f, 2, NULL, v));
  else add_triple_to_ptree(prog, va_make_triple(set_srate_i, descr_set_srate_i, 2, NULL, v));
}


/* ---------------- mus-data, xcoeffs, ycoeffs ---------------- */

#define MUS_VCT_1(Name, Position) \
static char *descr_ ## Name ## _0(int *args, int *ints, Float *dbls) \
{ \
  return(mus_format( PTR_PT " = mus-" #Name "(" PTR_PT ")", args[0], VCT_RESULT, args[1], (void *)(INT_ARG_1))); \
} \
static void Name ## _0(int *args, int *ints, Float *dbls) \
{ \
  mus_xen *gn; \
  gn = (mus_xen *)(INT_ARG_1); \
  if ((gn) && (gn->vcts)) \
    VCT_RESULT = (vct *)(XEN_OBJECT_REF(gn->vcts[Position])); \
} \
static xen_value *mus_ ##Name ## _1(ptree *prog, xen_value **args, int num_args) \
{ \
  return(package(prog, R_VCT, Name ## _0, descr_ ## Name ## _0, args, 1)); \
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

static xen_value *char_ge_1(ptree *prog, xen_value **args, int num_args) {return(greater_than_or_equal(prog, FALSE, args, num_args));}
static xen_value *char_gt_1(ptree *prog, xen_value **args, int num_args) {return(greater_than(prog, FALSE, args, num_args));}
static xen_value *char_le_1(ptree *prog, xen_value **args, int num_args) {return(less_than_or_equal(prog, FALSE, args, num_args));}
static xen_value *char_lt_1(ptree *prog, xen_value **args, int num_args) {return(less_than(prog, FALSE, args, num_args));}
static xen_value *char_eq_1(ptree *prog, xen_value **args, int num_args) {return(numbers_equal(prog, FALSE, args, num_args));}

static xen_value *char_ci_ge_1(ptree *prog, xen_value **args, int num_args) 
{
  return(greater_than_or_equal(prog, FALSE, upcase_args(prog, args, num_args), num_args));
}

static xen_value *char_ci_gt_1(ptree *prog, xen_value **args, int num_args) 
{
  return(greater_than(prog, FALSE, upcase_args(prog, args, num_args), num_args));
}

static xen_value *char_ci_le_1(ptree *prog, xen_value **args, int num_args) 
{
  return(less_than_or_equal(prog, FALSE, upcase_args(prog, args, num_args), num_args));
}

static xen_value *char_ci_lt_1(ptree *prog, xen_value **args, int num_args) 
{
  return(less_than(prog, FALSE, upcase_args(prog, args, num_args), num_args));
}

static xen_value *char_ci_eq_1(ptree *prog, xen_value **args, int num_args) 
{
  return(numbers_equal(prog, FALSE, upcase_args(prog, args, num_args), num_args));
}

static xen_value *ge_1(ptree *prog, xen_value **args, int num_args) {return(greater_than_or_equal(prog, prog->float_result, args, num_args));}
static xen_value *gt_1(ptree *prog, xen_value **args, int num_args) {return(greater_than(prog, prog->float_result, args, num_args));}
static xen_value *le_1(ptree *prog, xen_value **args, int num_args) {return(less_than_or_equal(prog, prog->float_result, args, num_args));}
static xen_value *lt_1(ptree *prog, xen_value **args, int num_args) {return(less_than(prog, prog->float_result, args, num_args));}
static xen_value *eq_1(ptree *prog, xen_value **args, int num_args) {return(numbers_equal(prog, prog->float_result, args, num_args));}

static xen_value *unwrap_xen_object_1(ptree *prog, XEN form, const char *origin, int constant)
{
  switch (xen_to_run_type(form))
    {
    case R_BOOL:    return(make_xen_value(R_BOOL, add_int_to_ptree(prog, (XEN_FALSE_P(form)) ? 0 : 1), R_CONSTANT)); break;
    case R_INT:     return(make_xen_value(R_INT, add_int_to_ptree(prog, XEN_TO_C_INT(form)), R_CONSTANT)); break;
    case R_FLOAT:   return(make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, XEN_TO_C_DOUBLE(form)), R_CONSTANT)); break;
    case R_CHAR:    return(make_xen_value(R_CHAR, add_int_to_ptree(prog, (int)(XEN_TO_C_CHAR(form))), R_CONSTANT)); break;
    case R_STRING:  return(make_xen_value(R_STRING, add_string_to_ptree(prog, copy_string(XEN_TO_C_STRING(form))), R_CONSTANT)); break;
    case R_VCT:     return(make_xen_value(R_VCT, add_int_to_ptree(prog, (int)(get_vct(form))), R_CONSTANT)); break;
    case R_PAIR:    if (constant) return(make_xen_value(R_PAIR, add_int_to_ptree(prog, (int)form), R_CONSTANT)); break;
    case R_LIST:    if (constant) return(make_xen_value(R_LIST, add_int_to_ptree(prog, (int)form), R_CONSTANT)); break;
    case R_SYMBOL:  return(make_xen_value(R_SYMBOL, add_int_to_ptree(prog, (int)form), R_CONSTANT)); break;
    case R_KEYWORD: return(make_xen_value(R_KEYWORD, add_int_to_ptree(prog, (int)form), R_CONSTANT)); break;
    case R_INT_VECTOR: 
      if (constant)
	{
	  int_vct *iv;
	  xen_value *v;
	  iv = read_int_vector(form);
	  v = make_xen_value(R_INT_VECTOR, add_int_to_ptree(prog, (int)iv), R_CONSTANT);
	  add_obj_to_gcs(prog, R_INT_VECTOR, v->addr);
	  return(v);
	}
      break;
    case R_FLOAT_VECTOR:
      if (constant)
	{
	  vct *vc;
	  xen_value *v;
	  vc = vector_to_vct(form);
	  v = make_xen_value(R_FLOAT_VECTOR, add_int_to_ptree(prog, (int)vc), R_CONSTANT);
	  add_obj_to_gcs(prog, R_FLOAT_VECTOR, v->addr);
	  return(v);
	}
      break;
    case R_CLM_VECTOR:
      if (constant)
	{
	  clm_vct *cv;
	  xen_value *v;
	  cv = read_clm_vector(form);
	  v = make_xen_value(R_CLM_VECTOR, add_int_to_ptree(prog, (int)cv), R_CONSTANT);
	  add_obj_to_gcs(prog, R_CLM_VECTOR, v->addr);
	  return(v);
	}
      break;
    }
  return(run_warn("%s: non-simple arg: %s", origin, XEN_AS_STRING(form)));
}

static xen_value *unwrap_xen_object(ptree *prog, XEN form, const char *origin)
{
  return(unwrap_xen_object_1(prog, form, origin, FALSE));
}

static xen_value *unwrap_constant_xen_object(ptree *prog, XEN form, const char *origin)
{
  return(unwrap_xen_object_1(prog, form, origin, TRUE));
}

static XEN get_lst(ptree *prog, xen_value **args) {return((XEN)(prog->ints[args[1]->addr]));}

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

static xen_value *cdr_1(ptree *prog, xen_value **args, int num_args) {return(unwrap_xen_object(prog, XEN_CDR(get_lst(prog, args)), "car"));}

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

static xen_value *declare_1(ptree *prog, XEN form, int need_result) {return(make_xen_value(R_UNSPECIFIED, -1, TRUE));}
static xen_value *lambda_preform(ptree *prog, XEN form, int need_result) {return(lambda_form(prog, form, TRUE, NULL, 0));}
static xen_value *quote_form(ptree *prog, XEN form, int need_result) 
{
  return(unwrap_constant_xen_object(prog, XEN_CADR(form), "quote"));
}

/* clm-print and indirect args support */

static int xenable(xen_value *v)
{
  switch (v->type)
    {
    case R_FLOAT: case R_INT: case R_CHAR: case R_STRING: case R_BOOL:
    case R_LIST: case R_PAIR: case R_FLOAT_VECTOR: case R_VCT: case R_KEYWORD: case R_SYMBOL:
      return(TRUE);
      break;
    default:
      return(v->type > R_ANY);
      break;
    }
  return(FALSE);
}

static XEN xen_value_to_xen(ptree *pt, xen_value *v)
{
  vct *vc;
  XEN val = XEN_UNDEFINED;
  switch (v->type)
    {
    case R_FLOAT:   val = C_TO_XEN_DOUBLE(pt->dbls[v->addr]); break;
    case R_INT:     val = C_TO_XEN_INT(pt->ints[v->addr]); break;
    case R_CHAR:    val = C_TO_XEN_CHAR((char)(pt->ints[v->addr])); break;
    case R_STRING:  val = C_TO_XEN_STRING((char *)(pt->ints[v->addr])); break;
    case R_BOOL:    return(C_TO_XEN_BOOLEAN(pt->ints[v->addr])); break;
    case R_SYMBOL:
    case R_KEYWORD:
    case R_LIST:    
    case R_PAIR:    val = (XEN)(pt->ints[v->addr]); break;
    case R_FLOAT_VECTOR:
    case R_VCT:
      vc = (vct *)(pt->ints[v->addr]);
      val = make_vct_wrapper(vc->length, vc->data);
      break;
    default:
      if (v->type > R_ANY)
	val = (XEN)(pt->ints[v->addr]);
      break;
    }
  if (XEN_BOUND_P(val))
    {
      add_loc_to_protected_list(pt, snd_protect(val));
      return(val);
    }
  return(XEN_FALSE);
}

static triple *make_indirect_triple(void (*function)(int *arg_addrs, int *ints, Float *dbls),
				    char *(*descr)(int *arg_addrs, int *ints, Float *dbls), 
				    xen_value **typed_args, int args)
{
  triple *trp;
  int *addrs = NULL;
  int i;
  if (args > 0)
    {
      addrs = (int *)CALLOC(args, sizeof(int));
      addrs[0] = typed_args[0]->addr; /* string result */
      addrs[1] = typed_args[1]->addr; /* num args */
      for (i = 2; i < args; i++) 
	addrs[i] = (int)(typed_args[i]);
    }
  trp = (triple *)CALLOC(1, sizeof(triple));
  trp->function = function;
  trp->args = addrs;
  trp->descr = descr;
  return(trp);
}

static xen_value *package_n_indirect(ptree *prog,
				     int type, 
				     void (*function)(int *arg_addrs, int *ints, Float *dbls),
				     char *(*descr)(int *arg_addrs, int *ints, Float *dbls),
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
  add_triple_to_ptree(prog, make_indirect_triple(function, descr, new_args, num_args + 2));
  rtn = new_args[0];
  FREE(new_args[1]);
  FREE(new_args);
  return(rtn);
}

static char *describe_indirect(const char *caller, int result_type, int *args, int *ints, Float *dbls)
{
  int num_args, i, len = 0;
  char **descrs = NULL;
  char *buf;
  xen_value *res;
  res = make_xen_value(result_type, args[0], R_VARIABLE);
  num_args = ints[args[1]];
  descrs = (char **)CALLOC(num_args + 2, sizeof(char *));
  descrs[0] = describe_xen_value(res, ints, dbls);
  len = (2 + strlen(caller));
  if (descrs[0]) len += strlen(descrs[0]);
  FREE(res);
  res = NULL;
  for (i = 2; i <= num_args + 1; i++)
    {
      descrs[i] = describe_xen_value((xen_value *)(args[i]), ints, dbls);
      if (descrs[i]) len += (2 + strlen(descrs[i]));
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

static XEN xen_values_to_list(ptree *pt, int num_args, int *args, int *ints)
{
  XEN lst = XEN_EMPTY_LIST;
  int i;
  for (i = num_args + 1; i >= 2; i--)
    lst = XEN_CONS(xen_value_to_xen(pt, (xen_value *)(args[i])), lst);
  return(lst);
}

static XEN format_func = XEN_FALSE;

static void format_s(int *args, int *ints, Float *dbls) 
{
  if (STRING_RESULT) FREE(STRING_RESULT);
  STRING_RESULT = copy_string(XEN_TO_C_STRING(XEN_APPLY(format_func, 
							xen_values_to_list(PTREE, ints[args[1]], args, ints),
							"format")));
}

static char *descr_format_s(int *args, int *ints, Float *dbls) {return(describe_indirect("format", R_STRING, args, ints, dbls));}

static xen_value *format_1(ptree *prog, xen_value **args, int num_args)
{
  return(package_n_indirect(prog, R_STRING, format_s, descr_format_s, args, num_args));
}

typedef struct {
  xen_value *(*walker)(ptree *prog, xen_value **args, int num_args);
  xen_value *(*special_walker)(ptree *prog, XEN form, int need_result);
  void (*set_walker)(ptree *prog, xen_value *in_v, xen_value *in_v1, xen_value *in_v2, xen_value *v);
  int required_args, max_args, result_type, need_int_result, num_arg_types;
  int *arg_types;
} walk_info;

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
	  int i;
	  buf = (char *)CALLOC(1024, sizeof(char));
	  sprintf(buf, "%p %p %p: req: %d, max: %d, result: %s, need_int: %d, num_arg_types: %d: ",
		  w->walker, w->special_walker, w->set_walker, 
		  w->required_args, w->max_args, type_name(w->result_type), w->need_int_result, w->num_arg_types);
	  for (i = 0; i < w->num_arg_types; i++)
	    {
	      strcat(buf, type_name(w->arg_types[i]));
	      strcat(buf, " ");
	    }
	  walker = C_TO_XEN_STRING(buf);
	  FREE(buf);
	  return(walker);
	}
    }
  return(XEN_FALSE);
}

static walk_info *make_walker(xen_value *(*walker)(ptree *prog, xen_value **args, int num_args),
			      xen_value *(*special_walker)(ptree *prog, XEN form, int need_result),
			      void (*set_walker)(ptree *prog, xen_value *in_v, xen_value *in_v1, xen_value *in_v2, xen_value *v),
			      int required_args, 
			      int max_args, 
			      int result_type, 
			      int need_int_result,
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

static void clm_print_s(int *args, int *ints, Float *dbls) 
{
  if (STRING_RESULT) FREE(STRING_RESULT);
  STRING_RESULT = copy_string(XEN_TO_C_STRING(XEN_APPLY(format_func, 
							xen_values_to_list(PTREE, ints[args[1]], args, ints),
							"format")));
  listener_append(STRING_RESULT);
}

static char *descr_clm_print_s(int *args, int *ints, Float *dbls) {return(describe_indirect("clm_print", R_STRING, args, ints, dbls));}

static xen_value *clm_print_1(ptree *prog, xen_value **args, int num_args)
{
  XEN_SET_OBJECT_PROPERTY(XEN_VAR_NAME_TO_VAR("clm-print"),
			  walk_sym,
			  C_TO_XEN_ULONG(make_walker(clm_print_1, NULL, NULL, 1, UNLIMITED_ARGS, R_STRING, FALSE, 1, -R_XEN)));
  return(package_n_indirect(prog, R_STRING, clm_print_s, descr_clm_print_s, args, num_args));
}

static xen_value *set_up_format(ptree *prog, xen_value **args, int num_args, int is_format)
{
  int i;
  XEN format_var = XEN_FALSE;
  format_var = XEN_VAR_NAME_TO_VAR("format");
  if ((!(XEN_FALSE_P(format_var))) &&
      (XEN_PROCEDURE_P(XEN_VARIABLE_REF(format_var))))
    {
      /* define a walker for format */
      XEN_SET_OBJECT_PROPERTY(format_var, 
			      walk_sym,
			      C_TO_XEN_ULONG(make_walker(format_1, NULL, NULL, 1, UNLIMITED_ARGS, R_STRING, FALSE, 1, -R_XEN)));
      format_func = XEN_VARIABLE_REF(format_var);
      /* any further formats will be checked in walk, but this one needs explicit check */
      for (i = 1; i <= num_args; i++)
	if (!(xenable(args[i])))
	  {
	    char *xv;
	    xv = describe_xen_value(args[i + 1], prog->ints, prog->dbls);
	    run_warn("can't handle %s as arg (%d) to %s", xv, i, (is_format) ? "format" : "clm-print");
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

#define CLM_MAKE_FUNC(Name) \
static void make_ ## Name ## _0(int *args, int *ints, Float *dbls) \
{ \
  XEN res; \
  res = XEN_APPLY(XEN_VARIABLE_REF(XEN_VAR_NAME_TO_VAR(S_make_ ## Name)), xen_values_to_list(PTREE, ints[args[1]], args, ints), S_make_ ## Name); \
  if (mus_xen_p(res)) \
    { \
      add_loc_to_protected_list(PTREE, snd_protect(res)); \
      CLM_RESULT = XEN_TO_MUS_ANY(res); \
    } \
  else CLM_RESULT = NULL; \
} \
static char *descr_make_ ## Name ## _0(int *args, int *ints, Float *dbls) {return(describe_indirect(S_make_ ## Name, R_CLM, args, ints, dbls));} \
static xen_value *make_ ## Name ## _1(ptree *prog, xen_value **args, int num_args) \
{ \
  return(package_n_indirect(prog, R_CLM, make_ ## Name ## _0, descr_make_ ## Name ## _0, args, num_args)); \
}

CLM_MAKE_FUNC(all_pass)
CLM_MAKE_FUNC(asymmetric_fm)
CLM_MAKE_FUNC(buffer)
CLM_MAKE_FUNC(comb)
CLM_MAKE_FUNC(convolve)
CLM_MAKE_FUNC(delay)
CLM_MAKE_FUNC(env)
CLM_MAKE_FUNC(file2frame)
CLM_MAKE_FUNC(file2sample)
CLM_MAKE_FUNC(filter)
CLM_MAKE_FUNC(fir_filter)
CLM_MAKE_FUNC(formant)
CLM_MAKE_FUNC(frame)
CLM_MAKE_FUNC(frame2file)
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
CLM_MAKE_FUNC(sample2file)
CLM_MAKE_FUNC(sawtooth_wave)
CLM_MAKE_FUNC(sine_summation)
CLM_MAKE_FUNC(square_wave)
CLM_MAKE_FUNC(src)
CLM_MAKE_FUNC(sum_of_cosines)
CLM_MAKE_FUNC(table_lookup)
CLM_MAKE_FUNC(triangle_wave)
CLM_MAKE_FUNC(two_pole)
CLM_MAKE_FUNC(two_zero)
CLM_MAKE_FUNC(wave_train)
CLM_MAKE_FUNC(waveshape)
CLM_MAKE_FUNC(zpolar)

static void make_fft_window_0(int *args, int *ints, Float *dbls)
{
  XEN res;
  res = XEN_APPLY(XEN_VARIABLE_REF(XEN_VAR_NAME_TO_VAR(S_make_fft_window)), xen_values_to_list(PTREE, ints[args[1]], args, ints), S_make_fft_window);
  add_loc_to_protected_list(PTREE, snd_protect(res));
  VCT_RESULT = get_vct(res);
}
static char *descr_make_fft_window_0(int *args, int *ints, Float *dbls) {return(describe_indirect(S_make_fft_window, R_VCT, args, ints, dbls));}
static xen_value *make_fft_window_1(ptree *prog, xen_value **args, int num_args)
{
  return(package_n_indirect(prog, R_VCT, make_fft_window_0, descr_make_fft_window_0, args, num_args));
}


static int xen_to_addr(ptree *pt, XEN arg, int type, int addr)
{
  /*
  fprintf(stderr,"xen to addr %p (%p %p): %s %d %d\n",
	  pt, pt->ints, pt->dbls, XEN_AS_STRING(arg), type, addr);
  */
  switch (type)
    {
    case R_FLOAT:   pt->dbls[addr] = (Float)XEN_TO_C_DOUBLE(arg);            break;
    case R_INT:     pt->ints[addr] = (int)XEN_TO_C_INT(arg);                 break;
    case R_CHAR:    pt->ints[addr] = (int)XEN_TO_C_CHAR(arg);                break;
    case R_BOOL:    pt->ints[addr] = (int)XEN_TO_C_BOOLEAN(arg);             break;
    case R_VCT:     pt->ints[addr] = (int)get_vct(arg);                      break;
    case R_CLM:     pt->ints[addr] = (int)(XEN_TO_MUS_ANY(arg));             break;
    case R_READER:  pt->ints[addr] = (int)get_sf(arg);                       break;
    case R_LIST:
    case R_PAIR:    pt->ints[addr] = (int)arg;                               break;
    case R_STRING:  
      pt->ints[addr] = (int)copy_string(XEN_TO_C_STRING(arg));
      remember_string(pt, addr);
      break;
    case R_INT_VECTOR: 
      pt->ints[addr] = (int)read_int_vector(arg);
      add_obj_to_gcs(pt, R_INT_VECTOR, addr);
      break;
    case R_FLOAT_VECTOR:
      pt->ints[addr] = (int)vector_to_vct(arg);
      add_obj_to_gcs(pt, R_FLOAT_VECTOR, addr);
      break;
    case R_VCT_VECTOR:
      pt->ints[addr] = (int)read_vct_vector(arg);
      add_obj_to_gcs(pt, R_VCT_VECTOR, addr);
      break;
    case R_CLM_VECTOR:
      pt->ints[addr] = (int)read_clm_vector(arg);
      add_obj_to_gcs(pt, R_CLM_VECTOR, addr);
      break;
    default:
      if (type > R_ANY)
	pt->ints[addr] = (int)arg;
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
  XEN_ASSERT_TYPE(XEN_STRING_P(name), name, XEN_ARG_1, S_add_clm_field, "string");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(offset), offset, XEN_ARG_2, S_add_clm_field, "int");
  if (clm_structs == 0)
    {
      clm_structs = 4;
      clm_struct_offsets = (int *)CALLOC(clm_structs, sizeof(int));
      clm_struct_types = (int *)CALLOC(clm_structs, sizeof(int));
      clm_struct_names = (char **)CALLOC(clm_structs, sizeof(char *));
    }
  else
    {
      if (clm_struct_top >= clm_structs)
	{
	  clm_structs = clm_struct_top + 4;
	  clm_struct_offsets = (int *)REALLOC(clm_struct_offsets, clm_structs * sizeof(int));
	  clm_struct_types = (int *)REALLOC(clm_struct_types, clm_structs * sizeof(int));
	  clm_struct_names = (char **)REALLOC(clm_struct_names, clm_structs * sizeof(char *));
	}
    }
  clm_struct_offsets[clm_struct_top] = XEN_TO_C_INT(offset);
  clm_struct_types[clm_struct_top] = R_UNSPECIFIED;
  if (XEN_SYMBOL_P(type))
    clm_struct_types[clm_struct_top] = name_to_type(XEN_SYMBOL_TO_C_STRING(type));
  clm_struct_names[clm_struct_top++] = copy_string(XEN_TO_C_STRING(name));
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
  XEN_ASSERT_TYPE(XEN_STRING_P(name), name, XEN_ARG_1, S_add_clm_type, "string");
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

static int check_clm_type(XEN sym, char *type)
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
    if ((clm_ref_offsets[i] == offset) && (lst == clm_ref_vars[i]))
      {
	if (run_type != clm_ref_types[i]) return(-1);
	addr = clm_ref_addrs[i];
	break;
      }
  if (addr == -1)
    {
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
	    }
	  else
	    {
	      clm_ref_size += 8;
	      clm_ref_offsets = (int *)REALLOC(clm_ref_offsets, clm_ref_size * sizeof(int));
	      clm_ref_types = (int *)REALLOC(clm_ref_types, clm_ref_size * sizeof(int));
	      clm_ref_addrs = (int *)REALLOC(clm_ref_addrs, clm_ref_size * sizeof(int));
	      clm_ref_vars = (XEN *)REALLOC(clm_ref_vars, clm_ref_size * sizeof(XEN));
	    }
	}
      /* first decide if we can handle this variable */
      if (run_type == R_FLOAT)
	addr = add_dbl_to_ptree(prog, 0.0);
      else
	{
	  if (run_type == R_STRING)
	    addr = add_string_to_ptree(prog, "");
	  else addr = add_int_to_ptree(prog, 0);
	}
      if (addr >= 0)
	{
	  i = clm_ref_top++;
	  clm_ref_offsets[i] = offset;
	  clm_ref_types[i] = run_type;
	  clm_ref_vars[i] = lst;
	  clm_ref_addrs[i] = addr;
	}
    }
  /* now set the (initial) value */
  if (addr >= 0) xen_to_addr(prog, lst_ref, run_type, addr);
  return(addr);
}


static char *descr_clm_struct_ref_r(int *args, int *ints, Float *dbls)
{
  return(mus_format("%s = %s(%s)",
		    describe_xen_value_1(ints[args[3]], args[0], ints, dbls),
		    clm_struct_names[ints[args[4]]],
		    (ints[args[1]] == 0) ? "null" : XEN_AS_STRING((XEN)(ints[args[1]]))));
}

static void clm_struct_ref_r(int *args, int *ints, Float *dbls)
{
  XEN lst;
  lst = (XEN)(ints[args[1]]);
  if ((lst != 0) && (XEN_LIST_P(lst)))
    {
      /*
      fprintf(stderr,"ref...%d %d %d %d, %d %d %d %d",
	      args[0], args[1], args[2], args[3],
	      args[0], ints[args[1]], ints[args[2]], ints[args[3]]);
      */
    xen_to_addr(PTREE, 
		XEN_LIST_REF(lst, 
			     ints[args[2]]), /* struct field offset into list */
		ints[args[3]],               /* result type */
		args[0]);                    /* result address */
    /* fprintf(stderr,"done\n"); */
    }
}

/* TODO: run-time set of clm-def-struct field */
/* TODO: display snd-test case for clm-struct (and run-time cases from tmp11.scm) */
static xen_value *clm_struct_ref(ptree *prog, xen_value *v, int struct_loc)
{
  /* types can't change within run */
  int run_type, addr, offset;
  XEN lst, lst_ref;
  /*
  fprintf(stderr,"struct ref %s ",clm_struct_names[struct_loc]);
  */
  if (v == NULL) return(run_warn("clm-struct-ref confused"));
  /*
    fprintf(stderr, "arg 1 %p: addr: %d type: %s\n", v, v->addr, type_name(v->type));
  */
  offset = clm_struct_offsets[struct_loc];
  lst = (XEN)(prog->ints[v->addr]);
  if ((lst == 0) || (!(XEN_LIST_P(lst))))
    {
      /* run-time list-ref here */
      run_type = clm_struct_types[struct_loc];
      if (run_type != R_UNSPECIFIED)
	{
	  /* it is up to the caller to stick to the declared type -- no attempt here to check (TODO: why not arg type check in clm_struct_ref?) */
	  xen_value **new_args;
	  xen_value *rtn;
	  int i;
	  new_args = (xen_value **)CALLOC(5, sizeof(xen_value *));
	  rtn = add_empty_var_to_ptree(prog, run_type);
	  new_args[0] = rtn;
	  new_args[1] = v;
	  new_args[2] = make_xen_value(R_INT, add_int_to_ptree(prog, offset), R_CONSTANT);
	  new_args[3] = make_xen_value(R_INT, add_int_to_ptree(prog, run_type), R_CONSTANT);
	  new_args[4] = make_xen_value(R_INT, add_int_to_ptree(prog, struct_loc), R_CONSTANT);
	  add_triple_to_ptree(prog, make_triple(clm_struct_ref_r, descr_clm_struct_ref_r, new_args, 5));
	  /*
	  fprintf(stderr,"result is %s, arg is %s\n", 
		  describe_xen_value(new_args[0], prog->ints, prog->dbls),
		  describe_xen_value(new_args[1], prog->ints, prog->dbls));
	  */
	  for (i = 2; i <= 4; i++) FREE(new_args[i]);
	  FREE(new_args);
	  return(rtn);
	}
      return(run_warn("%s struct field needs type declaration", clm_struct_names[struct_loc]));
    }
  else
    {
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
  return(NULL);
}

static void clm_struct_restore(ptree *prog, xen_var *var)
{
  XEN lst;
  int i, loc, addr, run_type;
  lst = (XEN)(prog->ints[var->v->addr]);
  for (i = 0; i < clm_ref_top; i++)
    if (clm_ref_vars[i] == lst)
      {
	/* restore this field */
	loc = clm_ref_offsets[i];    /* list-set index */
	addr = clm_ref_addrs[i];     /* ptree val addr */
	run_type = clm_ref_types[i]; /* val type */
	switch (run_type)
	  {
	  case R_BOOL:   XEN_LIST_SET(lst, loc, C_TO_XEN_BOOLEAN(prog->ints[addr])); break;
	  case R_INT:    XEN_LIST_SET(lst, loc, C_TO_XEN_INT(prog->ints[addr])); break;
	  case R_FLOAT:  XEN_LIST_SET(lst, loc, C_TO_XEN_DOUBLE(prog->dbls[addr])); break;
	  case R_CHAR:   XEN_LIST_SET(lst, loc, C_TO_XEN_CHAR(prog->ints[addr])); break;
	  case R_STRING: XEN_LIST_SET(lst, loc, C_TO_XEN_STRING((char *)(prog->ints[addr]))); break;
	    /* TODO: restore clm_def_struct field vectors -> originals? */
	    /* 	     case R_FLOAT_VECTOR: vct_into_vector((vct *)(prog->ints[var->v->addr]), val);
	     *       case R_INT_VECTOR: int_vct_into_vector((int_vct *)(prog->ints[var->v->addr]), val);
	     */
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

static int isvowel(char b)
{
  return((b == 'a') || (b == 'e') || (b == 'i') || (b == 'o') || (b == 'u'));
}

static xen_value *arg_warn(ptree *prog, char *funcname, int arg_num, xen_value **args, char *correct_type)
{
  char *xb, *tb;
  xb = describe_xen_value(args[arg_num], prog->ints, prog->dbls);
  tb = type_name(args[arg_num]->type);
  if (xb)
    {
      run_warn("%s argument %d (%s) is a%s %s, not a%s %s?", 
	       funcname, arg_num, xb, 
	       (isvowel(tb[0])) ? "n" : "",
		tb,
	       (isvowel(correct_type[0])) ? "n" : "",
	       correct_type);
      FREE(xb);
    }
  else 
    run_warn("%s argument %d is a%s %s, not a%s %s?", 
	     funcname, arg_num, 
	     (isvowel(tb[0])) ? "n" : "",
	     type_name(args[arg_num]->type),
	     (isvowel(correct_type[0])) ? "n" : "", 
	     correct_type);
  return(NULL);
}

#define WITH_PROCPROP 0
/* aimed at eventual apply support -- places walker property on function so it can be
 *   found even if name has changed -- (define hi min) for example (not very useful by itself)
 */

static xen_value *walk(ptree *prog, XEN form, int need_result)
{
  /* walk form, storing vars, making program entries for operators etc */
  XEN rtnval = XEN_FALSE;
  /* fprintf(stderr,"walk %s (needed: %d)\n", XEN_AS_STRING(form), need_result); */
  if (current_optimization == DONT_OPTIMIZE) return(NULL);

  if (XEN_LIST_P(form))
    {
      XEN function, all_args;
      char *funcname = "not-a-function";
      xen_value **args = NULL;
      int i, k, num_args, float_result = FALSE, constants = 0;
      xen_var *var;
      xen_value *v = NULL;
      XEN walker;
      walk_info *w = NULL;
      function = XEN_CAR(form);
      if (XEN_SYMBOL_P(function))
	{
	  walker = XEN_OBJECT_PROPERTY(function, walk_sym);
#if WITH_PROCPROP
	  if ((XEN_FALSE_P(walker)) && (XEN_PROCEDURE_P(XEN_VARIABLE_REF(XEN_SYMBOL_TO_VAR(function)))))
	    walker = XEN_PROCEDURE_PROPERTY(XEN_VARIABLE_REF(XEN_SYMBOL_TO_VAR(function)), walk_sym);
#endif
	  if (XEN_ULONG_P(walker))
	    {
	      w = (walk_info *)(XEN_TO_C_ULONG(walker));
	      if ((w) && (w->special_walker))
		return((*(w->special_walker))(prog, form, need_result));
	    }
	}
      all_args = XEN_CDR(form);
      num_args = XEN_LIST_LENGTH(all_args);
      args = (xen_value **)CALLOC(num_args + 1, sizeof(xen_value *));
      if (num_args > 0)
	{
	  int arg_result = NEED_ANY_RESULT;
	  for (i = 0; i < num_args; i++, all_args = XEN_CDR(all_args))
	    {
	      if (w)
		{
		  if ((w->need_int_result) ||
		      ((i < w->num_arg_types) && (w->arg_types[i] == R_INT)))
		    arg_result = NEED_INT_RESULT;
		  else
		    {
		      if ((i == 0) && (w->num_arg_types == 1) && (w->arg_types[0] == R_XCLM))
			arg_result = NEED_XCLM_RESULT;
		    }
		}
	      args[i + 1] = walk(prog, XEN_CAR(all_args), arg_result);
	      /*
	      if (args[i + 1] != NULL)
		{
		  fprintf(stderr, "arg %d: addr: %d type: %s val: %d\n", 
			  i + 1, args[i + 1]->addr, type_name(args[i + 1]->type), prog->ints[args[i + 1]->addr]);
		}
	      else fprintf(stderr,"arg %d null", i);
	      */

	      if ((args[i + 1] == NULL) ||
		  (((args[i + 1]->type == R_LIST) || (args[i + 1]->type == R_PAIR)) && (prog->ints[args[i + 1]->addr] == 0)))
		return(clean_up(NULL, args, num_args)); /* no run_warn here so that reported error includes enclosing form */
	      if (args[i + 1]->constant == R_CONSTANT) constants++;
	      if (args[i + 1]->type == R_FLOAT) float_result = TRUE; /* for "*" et al */
	    }
	}
      funcname = XEN_SYMBOL_TO_C_STRING(function);
      if (w == NULL) /* we're in a list, looking at car, and we don't optimize locally defined functions, so this should be ok */
	{
	  /* check user-defined stuff */
	  var = find_var_in_ptree(prog, funcname);
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
		case R_READER:   if (num_args == 0) return(clean_up(reader_0(prog, args, v), args, num_args)); break;
		case R_CLM:      return(clean_up(clm_n(prog, args, num_args, v), args, num_args));             break;
		case R_BOOL:
		case R_GOTO:     if (num_args == 0) return(clean_up(goto_0(prog, args, v), args, num_args));   break;
		case R_FUNCTION: return(clean_up(funcall_n(prog, args, num_args, v), args, num_args));         break;
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
	      for (i = 0; i < num_args; i++)
		if ((args[i + 1]->type != true_type) &&
		    (((true_type != R_NUMBER) ||
		      ((args[i + 1]->type != R_INT) && 
		       (args[i + 1]->type != R_FLOAT))) &&
		     ((true_type != R_XEN) ||
		      (!(xenable(args[i + 1]))))))
		  return(clean_up(arg_warn(prog, funcname, i + 1, args, type_name(true_type)), args, num_args));
	    }
	  else
	    {
	      if (w->num_arg_types < args_to_check) args_to_check = w->num_arg_types;
	      for (i = 0; i < args_to_check; i++)
		{
		  if ((w->arg_types[i] != args[i + 1]->type) &&
		      (w->arg_types[i] != R_ANY))
		    {
		      if (w->arg_types[i] == R_NUMBER)
			{
			  if ((args[i + 1]->type != R_INT) &&
			      (args[i + 1]->type != R_FLOAT))
			    return(clean_up(arg_warn(prog, funcname, i + 1, args, "number"), args, num_args));
			}
		      else
			{
			  if ((w->arg_types[i] == R_CLM) || (w->arg_types[i] == R_XCLM))
			    {
			      if (args[i + 1]->type != R_BOOL)
				{
				  if (args[i + 1]->type == R_XCLM)
				    {
				      /* get mus_any from mus_xen */
				      xen_value *old_v, *new_v; /* goddamn C++! */
				      old_v = args[i + 1];
				      new_v = make_xen_value(R_CLM, 
							     add_int_to_ptree(prog, 
									      (int)(MUS_XEN_TO_MUS_ANY(prog->ints[old_v->addr]))), 
							     R_VARIABLE);
				      FREE(old_v);
				      args[i + 1] = new_v;
				    }
				  else 
				    {
				      if (args[i + 1]->type == R_CLM)
					{
					  /* need mus_xen from mus_any -- may not be doable */
					  /* search from name for original global var -- locally created gens not transformable */
					  xen_var *var;
					  var = find_var_in_ptree_via_addr(prog, FALSE, args[i + 1]->addr);
					  if (var)
					    {
					      /* var->name -> symbol, lookup globally -> form -> mus_xen (whew!) */
					      XEN val;
					      int local_var;
					      val = symbol_to_value(prog->code, C_STRING_TO_XEN_SYMBOL(var->name), &local_var);
					      if (mus_xen_p(val))
						{
						  xen_value *old_v, *new_v;
						  old_v = args[i + 1];
						  new_v = make_xen_value(R_XCLM, 
									 add_int_to_ptree(prog, 
											  (int)(XEN_TO_MUS_XEN(val))), 
									 R_VARIABLE);
						  FREE(old_v);
						  args[i + 1] = new_v;
						}
					      else return(clean_up(run_warn("shadowed local clm gen?"), args, num_args));
					    }
					  else return(clean_up(run_warn("local clm gen use unoptimizable"), args, num_args));
					}
				      else return(clean_up(arg_warn(prog, funcname, i + 1, args, "clm generator"), args, num_args));
				    }
				}
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
				      else return(clean_up(arg_warn(prog, funcname, i + 1, args, type_name(w->arg_types[i])), args, num_args));
				    }
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
	      prog->need_result = need_result;
	      return(clean_up((*(w->walker))(prog, args, num_args), args, num_args));
	    }
	}
      if ((num_args == 2) && (strcmp(funcname, "list-ref") == 0) && (XEN_EXACT_P(XEN_CADDR(form))))
	{
	  XEN lst;
	  lst = get_lst(prog, args);
	  if ((lst) && (XEN_LIST_P(lst)))
	    return(clean_up(unwrap_xen_object(prog, XEN_LIST_REF_WRAPPED(get_lst(prog, args), XEN_CADDR(form)), funcname), args, num_args));
	  else return(clean_up(run_warn("can't handle this list: ~A", XEN_AS_STRING(form)), args, num_args));
	}
      /*
      fprintf(stderr, "arg 1 %p: addr: %d type: %s\n", args[1], args[1]->addr, type_name(args[1]->type));
      */
      for (k = 0; k < clm_struct_top; k++)
	if (strcmp(funcname, clm_struct_names[k]) == 0)
	  return(clean_up(clm_struct_ref(prog, args[1], k), args, num_args));
      for (k = 0; k < clm_types_top; k++)
	if (strcmp(funcname, clm_qtypes[k]) == 0)
	  return(clean_up(make_xen_value(R_BOOL, 
					 add_int_to_ptree(prog, check_clm_type(XEN_CAR(get_lst(prog, args)), type_name(R_ANY + k + 1))),
					 R_CONSTANT), 
			  args, num_args));

      if (strcmp(funcname, "format") == 0)
	return(clean_up(set_up_format(prog, args, num_args, TRUE), args, num_args));
      if (strcmp(funcname, S_clm_print) == 0)
	return(clean_up(set_up_format(prog, args, num_args, FALSE), args, num_args));

      /* check for function defined elsewhere, get source, splice in if possible */
      if ((v == NULL) && 
	  (current_optimization >= SOURCE_OK) &&
	  (XEN_PROCEDURE_P(rtnval)) &&
	  (XEN_FALSE_P(XEN_PROCEDURE_WITH_SETTER_P(rtnval))))
	{
	  XEN func_form;
	  /*
	  fprintf(stderr,"try to splice in %s ", funcname);
	  */
	  func_form = XEN_PROCEDURE_SOURCE(rtnval);
	  if ((XEN_LIST_P(func_form)) &&
	      (XEN_SYMBOL_P(XEN_CAR(func_form))) &&
	      (strcmp("lambda", XEN_SYMBOL_TO_C_STRING(XEN_CAR(func_form))) == 0))
	    {
	      /* look for procedure source, use current arg types as auto-declaration */
	      int old_got_lambda;
	      old_got_lambda = got_lambda;
	      got_lambda = TRUE;
	      v = lambda_form(prog, func_form, TRUE, args, num_args);
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

      if ((need_result) || (XEN_LIST_P(form)))
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
	case R_INT:     return(make_xen_value(R_INT, add_int_to_ptree(prog, XEN_TO_C_INT(form)), R_CONSTANT)); break;
	case R_FLOAT:   return(make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, XEN_TO_C_DOUBLE(form)), R_CONSTANT)); break;
	case R_STRING:  return(make_xen_value(R_STRING, add_string_to_ptree(prog, copy_string(XEN_TO_C_STRING(form))), R_CONSTANT)); break;
	case R_CHAR:    return(make_xen_value(R_CHAR, add_int_to_ptree(prog, (int)(XEN_TO_C_CHAR(form))), R_CONSTANT)); break;
	case R_BOOL:    return(make_xen_value(R_BOOL, add_int_to_ptree(prog, (XEN_FALSE_P(form)) ? 0 : 1), R_CONSTANT)); break;
	case R_LIST:    return(make_xen_value(R_LIST, add_int_to_ptree(prog, (int)form), R_CONSTANT)); break;
	case R_PAIR:    return(make_xen_value(R_PAIR, add_int_to_ptree(prog, (int)form), R_CONSTANT)); break;
	case R_KEYWORD: return(make_xen_value(R_KEYWORD, add_int_to_ptree(prog, (int)form), R_CONSTANT)); break;
	default:
	  if (type > R_ANY)
	    return(make_xen_value(type, add_int_to_ptree(prog, (int)form), R_CONSTANT));
	  break;
	}
      if (XEN_SYMBOL_P(form))
	{
	  prog->need_result = need_result;
	  return(add_global_var_to_ptree(prog, form, &rtnval));
	}
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
	      char *xb;
	      xb = describe_xen_value(v, prog->ints, prog->dbls);
	      run_warn("can't set %s (a %s) to %s (a %s)",
		       XEN_SYMBOL_TO_C_STRING(acc_form),
		       type_name(w->result_type),
		       xb,
		       type_name(v->type));
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
	    sv = clm_struct_ref(prog, in_v, k);
	    if (sv)
	      {
		if (v->type == sv->type)
		  {
		    xen_var *lst;
		    triple *trp;
		    trp = set_var(prog, sv, v);
		    trp->no_opt = TRUE;
		    lst = find_var_in_ptree_via_addr(prog, FALSE, in_v->addr);
		    if (lst) lst->unclean = TRUE;
		    happy = 1;
		  }
		else 
		  {
		    char *xb;
		    xb = describe_xen_value(v, prog->ints, prog->dbls);
		    run_warn("can't set %s (a %s) to %s (a %s)",
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

static int ptree_on = FALSE;
static XEN g_show_ptree(XEN on)
{
  ptree_on = XEN_TO_C_BOOLEAN(on);
  return(on);
}

static void *form_to_ptree(XEN code)
{
  ptree *prog;
  XEN form;
#if DEBUGGING
  free_saved_locals();
#endif
  run_warned = FALSE;
  current_optimization = optimization(get_global_state());
  if (current_optimization == DONT_OPTIMIZE) return(NULL);
  form = XEN_CAR(code);
  prog = make_ptree(8);
  if ((XEN_PROCEDURE_P(XEN_CADR(code))) && 
      (!(XEN_APPLICABLE_SMOB_P(XEN_CADR(code))))) /* applicable smobs cause confusion here */
    prog->code = XEN_CADR(code);                  /* need env before starting to walk the code */
  else prog->code = XEN_FALSE;                    /* many confusing cases here -- we'll just give up */
  if (XEN_SYMBOL_P(form))
    return(free_ptree((void *)prog));
  prog->result = walk(prog, form, TRUE);
  if (prog->result)
    {
      add_triple_to_ptree(prog, make_triple(quit, descr_quit, NULL, 0));
      /* now check that we were able to nail down all global variable types */
#ifdef DESCRIBE_PTREE
      {
	char *msg;
	msg = describe_ptree(prog);
	if (ptree_on) fprintf(stderr, msg);
	FREE(msg);
      }
#endif
      prog->form = form;
      snd_protect(prog->form);
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
  ptree *pt;
  pt = (ptree *)form_to_ptree(XEN_LIST_2(code, XEN_FALSE));
  if (pt)
    {
      if ((pt->result->type == R_BOOL) && (pt->arity == 1))
	return(pt);
      free_ptree(pt);
    }
  return(NULL);
}


/* ---------------- various evaluator wrappers ---------------- */

Float evaluate_ptree_1f2f(void *upt, Float arg)
{
  ptree *pt = (ptree *)upt;
  pt->dbls[pt->args[0]] = arg;
  eval_ptree(pt);
  return(pt->dbls[pt->result->addr]);
}

Float evaluate_ptree_1f1v1b2f(void *upt, Float arg, vct *v, int dir)
{
  ptree *pt = (ptree *)upt;
  pt->dbls[pt->args[0]] = arg;
  pt->ints[pt->args[1]] = (int)v;
  pt->ints[pt->args[2]] = dir;
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

Float evaluate_ptreec(void *upt, Float arg, vct *v, int dir)
{
  ptree *pt = (ptree *)upt;
  pt->dbls[pt->args[0]] = arg;
  if (pt->arity > 1)
    {
      pt->ints[pt->args[1]] = (int)v;
      pt->ints[pt->args[2]] = dir;
    }
  eval_ptree(pt);
  return(pt->dbls[pt->result->addr]);
}

static XEN eval_ptree_to_xen(ptree *pt)
{
  XEN result = XEN_FALSE;
  switch (pt->result->type)
    {
    case R_FLOAT:   result = C_TO_XEN_DOUBLE(pt->dbls[pt->result->addr]); break;
    case R_INT:     result = C_TO_XEN_INT(pt->ints[pt->result->addr]); break;
    case R_CHAR:    result = C_TO_XEN_CHAR((char)(pt->ints[pt->result->addr])); break;
    case R_STRING:  result = C_TO_XEN_STRING((char *)(pt->ints[pt->result->addr])); break;
    case R_BOOL:    result = C_TO_XEN_BOOLEAN(pt->ints[pt->result->addr]); break;
    case R_LIST:
    case R_PAIR:
    case R_SYMBOL:
    case R_KEYWORD: result = (XEN)(pt->ints[pt->result->addr]); break;
    case R_VCT:
      {
	vct *v;
	v = c_vct_copy((vct *)(pt->ints[pt->result->addr]));
	result = make_vct(v->length, v->data);
	FREE(v);
      }
      break;
    case R_CLM:     run_warn("can't wrap gen?"); break;
    default:
      if (pt->result->type > R_ANY)
	result = (XEN)(pt->ints[pt->result->addr]); 
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
  pt->result = walk(pt, code, TRUE);
  if (pt->result)
    {
      add_triple_to_ptree(pt, make_triple(quit, descr_quit, NULL, 0));
#ifdef DESCRIBE_PTREE
      {
	char *msg;
	msg = describe_ptree(pt);
	if (ptree_on) fprintf(stderr, msg);
	FREE(msg);
      }
#endif
      if (pt->args)
	{
	  if ((XEN_BOUND_P(arg)) && (pt->arity > 0))
	    {
	      xen_to_addr(pt, arg, pt->arg_types[0], pt->args[0]);
	      if ((XEN_BOUND_P(arg1)) && (pt->arity > 1))
		{
		  xen_to_addr(pt, arg1, pt->arg_types[1], pt->args[1]);
		  if ((XEN_BOUND_P(arg2)) && (pt->arity > 2))
		    xen_to_addr(pt, arg2, pt->arg_types[2], pt->args[2]);
		}
	    }
	}
      eval_ptree(pt);
      result = eval_ptree_to_xen(pt);
      free_ptree((void *)pt);
      return(result);
    }
  if (pt) free_ptree((void *)pt);
  XEN_ERROR(XEN_ERROR_TYPE("cannot-parse"),
	    code);
  return(XEN_FALSE);
}
#else
void *form_to_ptree_1_b(XEN code) {return(NULL);}
void *form_to_ptree_3_f(XEN code) {return(NULL);}
void *form_to_ptree_1_b_without_env(XEN code) {return(NULL);}
void *form_to_ptree_1_f(XEN code) {return(NULL);}
Float evaluate_ptree_1f1v1b2f(void *upt, Float arg, vct *v, int dir) {return(0.0);}
Float evaluate_ptree_0f2f(void *upt) {return(0.0);}
void *form_to_ptree_0_f(XEN code) {return(NULL);}
Float evaluate_ptree_1f2f(void *upt, Float arg) {return(0.0);}
int evaluate_ptree_1f2b(void *upt, Float arg) {return(0);}
void *free_ptree(void *upt) {return(NULL);}
XEN ptree_code(void *p) {return(XEN_FALSE);}
Float evaluate_ptreec(void *upt, Float arg, vct *v, int dir) {return(0.0);}
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
  XEN_ASSERT_TYPE(XEN_PROCEDURE_P(code) && (XEN_REQUIRED_ARGS(code) == 0), code, XEN_ARG_1, S_vct_map, "a thunk");
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
  if (optimization(get_global_state()) != DONT_OPTIMIZE)
    {
      ptree *pt = NULL;
      pt = (ptree *)form_to_ptree(proc_and_code);
      if (pt)
	{
	  for (i = 0; i < min_len; i++) 
	    {
	      eval_ptree(pt);
	      vals = mus_data((mus_any *)(pt->ints[pt->result->addr]));
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

static XEN g_optimization(void) {return(C_TO_XEN_INT(optimization(get_global_state())));}
static XEN g_set_optimization(XEN val) 
{
  #define H_optimization "(" S_optimization "): the current 'run' optimization level (default 0 = off, max is 6)"
  snd_state *ss;
  ss = get_global_state();
  XEN_ASSERT_TYPE(XEN_INTEGER_P(val), val, XEN_ONLY_ARG, "set! " S_optimization, "an integer");
  set_optimization(ss, XEN_TO_C_INT(val));
  return(C_TO_XEN_INT(optimization(ss)));
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
  XEN_ASSERT_TYPE(XEN_PROCEDURE_P(code) && (XEN_REQUIRED_ARGS(code) == 0), code, XEN_ARG_1, S_run, "a thunk");
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
XEN_NARGIFY_2(g_vct_map_w, g_vct_map)
#else
#define g_optimization_w g_optimization
#define g_set_optimization_w g_set_optimization
#define g_vct_map_w g_vct_map
#endif

#if HAVE_GUILE && WITH_RUN

#if WITH_PROCPROP
#define INIT_WALKER(Name, Val) \
  XEN_SET_OBJECT_PROPERTY(C_STRING_TO_XEN_SYMBOL(Name), walk_sym, C_TO_XEN_ULONG(Val)); \
  XEN_SET_PROCEDURE_PROPERTY(XEN_VARIABLE_REF(XEN_VAR_NAME_TO_VAR(Name)), walk_sym, C_TO_XEN_ULONG(Val))
#else
#define INIT_WALKER(Name, Val) XEN_SET_OBJECT_PROPERTY(C_STRING_TO_XEN_SYMBOL(Name), walk_sym, C_TO_XEN_ULONG(Val))
#endif

static void init_walkers(void)
{
  XEN do_star, declare, call_cc, gmus_position;
  XEN_DEFINE_VARIABLE("do*", do_star, XEN_FALSE);
  XEN_DEFINE_VARIABLE("declare", declare, XEN_FALSE);
  XEN_DEFINE_VARIABLE("call/cc", call_cc, XEN_FALSE);
  XEN_DEFINE_VARIABLE("mus-position", gmus_position, XEN_FALSE);
  walk_sym = C_STRING_TO_XEN_SYMBOL("snd-walk");
  snd_protect(walk_sym);

  /* -------- special forms */
  INIT_WALKER("let", make_walker(NULL, let_form, NULL, 0, UNLIMITED_ARGS, R_ANY, FALSE, 0));
  INIT_WALKER("let*", make_walker(NULL, let_star_form, NULL, 0, UNLIMITED_ARGS, R_ANY, FALSE, 0));
  INIT_WALKER("do", make_walker(NULL, do_form, NULL, 0, UNLIMITED_ARGS, R_ANY, FALSE, 0));
  INIT_WALKER("do*", make_walker(NULL, do_star_form, NULL, 0, UNLIMITED_ARGS, R_ANY, FALSE, 0));
  INIT_WALKER("begin", make_walker(NULL, begin_form, NULL, 0, UNLIMITED_ARGS, R_ANY, FALSE, 0));
  INIT_WALKER("if", make_walker(NULL, if_form, NULL, 0, UNLIMITED_ARGS, R_ANY, FALSE, 0));
  INIT_WALKER("cond", make_walker(NULL, cond_form, NULL, 0, UNLIMITED_ARGS, R_ANY, FALSE, 0));
  INIT_WALKER("case", make_walker(NULL, case_form, NULL, 0, UNLIMITED_ARGS, R_ANY, FALSE, 0));
  INIT_WALKER("call-with-current-continuation", make_walker(NULL, callcc_form, NULL, 0, UNLIMITED_ARGS, R_ANY, FALSE, 0));
  INIT_WALKER("or", make_walker(NULL, or_form, NULL, 0, UNLIMITED_ARGS, R_ANY, FALSE, 0));
  INIT_WALKER("and", make_walker(NULL, and_form, NULL, 0, UNLIMITED_ARGS, R_ANY, FALSE, 0));
  INIT_WALKER("set!", make_walker(NULL, set_form, NULL, 0, UNLIMITED_ARGS, R_ANY, FALSE, 0));
  INIT_WALKER("call/cc", make_walker(NULL, callcc_form, NULL, 0, UNLIMITED_ARGS, R_ANY, FALSE, 0));
  INIT_WALKER("lambda", make_walker(NULL, lambda_preform, NULL, 0, UNLIMITED_ARGS, R_ANY, FALSE, 0));
  INIT_WALKER("quote", make_walker(NULL, quote_form, NULL, 0, UNLIMITED_ARGS, R_ANY, FALSE, 0));
  INIT_WALKER("declare", make_walker(NULL, declare_1, NULL, 0, UNLIMITED_ARGS, R_ANY, FALSE, 0));

  /* -------- basic stuff */
  INIT_WALKER("eq?", make_walker(eq_p, NULL, NULL, 2, 2, R_BOOL, FALSE, 0));
  INIT_WALKER("eqv?", make_walker(eqv_p, NULL, NULL, 2, 2, R_BOOL, FALSE, 0));
  INIT_WALKER("equal?", make_walker(equal_p, NULL, NULL, 2, 2, R_BOOL, FALSE, 0));
  INIT_WALKER("boolean?", make_walker(boolean_p_1, NULL, NULL, 1, 1, R_BOOL, FALSE, 0));
  INIT_WALKER("number?", make_walker(number_p_1, NULL, NULL, 1, 1, R_BOOL, FALSE, 0));
  INIT_WALKER("integer?", make_walker(integer_p_1, NULL, NULL, 1, 1, R_BOOL, FALSE, 0));
  INIT_WALKER("inexact?", make_walker(inexact_p_1, NULL, NULL, 1, 1, R_BOOL, FALSE, 0));
  INIT_WALKER("exact?", make_walker(exact_p_1, NULL, NULL, 1, 1, R_BOOL, FALSE, 0));
  INIT_WALKER("real?", make_walker(real_p_1, NULL, NULL, 1, 1, R_BOOL, FALSE, 0));
  INIT_WALKER("char?", make_walker(char_p_1, NULL, NULL, 1, 1, R_BOOL, FALSE, 0));
  INIT_WALKER("string?", make_walker(string_p_1, NULL, NULL, 1, 1, R_BOOL, FALSE, 0));
  INIT_WALKER("keyword?", make_walker(keyword_p_1, NULL, NULL, 1, 1, R_BOOL, FALSE, 0));
  INIT_WALKER("symbol?", make_walker(symbol_p_1, NULL, NULL, 1, 1, R_BOOL, FALSE, 0));
  INIT_WALKER("vector?", make_walker(vector_p_1, NULL, NULL, 1, 1, R_BOOL, FALSE, 0));
  INIT_WALKER("list?", make_walker(list_p_1, NULL, NULL, 1, 1, R_BOOL, FALSE, 0));
  INIT_WALKER("pair?", make_walker(pair_p_1, NULL, NULL, 1, 1, R_BOOL, FALSE, 0));
  INIT_WALKER(">=", make_walker(ge_1, NULL, NULL, 0, UNLIMITED_ARGS, R_BOOL, FALSE, 1, -R_NUMBER));
  INIT_WALKER(">", make_walker(gt_1, NULL, NULL, 0, UNLIMITED_ARGS, R_BOOL, FALSE, 1, -R_NUMBER));
  INIT_WALKER("<=", make_walker(le_1, NULL, NULL, 0, UNLIMITED_ARGS, R_BOOL, FALSE, 1, -R_NUMBER));
  INIT_WALKER("<", make_walker(lt_1, NULL, NULL, 0, UNLIMITED_ARGS, R_BOOL, FALSE, 1, -R_NUMBER));
  INIT_WALKER("=", make_walker(eq_1, NULL, NULL, 0, UNLIMITED_ARGS, R_BOOL, FALSE, 1, -R_NUMBER));

  INIT_WALKER("*", make_walker(multiply, NULL, NULL, 0, UNLIMITED_ARGS, R_ANY, FALSE, 1, -R_NUMBER));
  INIT_WALKER("+", make_walker(add, NULL, NULL, 0, UNLIMITED_ARGS, R_NUMBER, FALSE, 1, -R_NUMBER));
  INIT_WALKER("-", make_walker(subtract, NULL, NULL, 1, UNLIMITED_ARGS, R_NUMBER, FALSE, 1, -R_NUMBER));
  INIT_WALKER("/", make_walker(divide, NULL, NULL, 1, UNLIMITED_ARGS, R_NUMBER, FALSE, 1, -R_NUMBER));
  INIT_WALKER("max", make_walker(max_1, NULL, NULL, 1, UNLIMITED_ARGS, R_NUMBER, FALSE, 1, -R_NUMBER));
  INIT_WALKER("min", make_walker(min_1, NULL, NULL, 1, UNLIMITED_ARGS, R_NUMBER, FALSE, 1, -R_NUMBER));
  INIT_WALKER("1+", make_walker(one_plus, NULL, NULL, 1, 1, R_NUMBER, FALSE, 1, R_NUMBER));
  INIT_WALKER("1-", make_walker(one_minus, NULL, NULL, 1, 1, R_NUMBER, FALSE, 1, R_NUMBER));

  INIT_WALKER("inexact->exact", make_walker(inexact2exact_1, NULL, NULL, 1, 1, R_INT, TRUE, 1, R_NUMBER));
  INIT_WALKER("exact->inexact", make_walker(exact2inexact_1, NULL, NULL, 1, 1, R_FLOAT, FALSE, 1, R_NUMBER));
  INIT_WALKER("modulo", make_walker(modulo_1, NULL, NULL, 2, 2, R_INT, TRUE, 2, R_NUMBER, R_NUMBER));
  INIT_WALKER("remainder", make_walker(remainder_1, NULL, NULL, 2, 2, R_INT, TRUE, 2, R_NUMBER, R_NUMBER));
  INIT_WALKER("quotient", make_walker(quotient_1, NULL, NULL, 2, 2, R_INT, TRUE, 2, R_NUMBER, R_NUMBER));
  INIT_WALKER("logand", make_walker(logand_1, NULL, NULL, 2, 2, R_INT, TRUE, 2, R_INT, R_INT));
  INIT_WALKER("logxor", make_walker(logxor_1, NULL, NULL, 2, 2, R_INT, TRUE, 2, R_INT, R_INT));
  INIT_WALKER("logior", make_walker(logior_1, NULL, NULL, 2, 2, R_INT, TRUE, 2, R_INT, R_INT));
  INIT_WALKER("lognot", make_walker(lognot_1, NULL, NULL, 1, 1, R_INT, TRUE, 1, R_INT));
  INIT_WALKER("ash", make_walker(ash_1, NULL, NULL, 2, 2, R_INT, TRUE, 2, R_INT, R_INT));
  INIT_WALKER("integer->char", make_walker(integer_to_char_1, NULL, NULL, 1, 1, R_CHAR, TRUE, 1, R_INT));
  INIT_WALKER("gcd", make_walker(gcd_1, NULL, NULL, 0, UNLIMITED_ARGS, R_INT, TRUE, 1, -R_NUMBER));
  INIT_WALKER("lcm", make_walker(lcm_1, NULL, NULL, 0, UNLIMITED_ARGS, R_INT, TRUE, 1, -R_NUMBER));
  INIT_WALKER("expt", make_walker(expt_1, NULL, NULL, 2, 2, R_NUMBER, FALSE, 2, R_NUMBER, R_NUMBER));
  INIT_WALKER("atan", make_walker(atan_1, NULL, NULL, 1, 2, R_NUMBER, FALSE, 2, R_NUMBER, R_NUMBER));
  INIT_WALKER("sin", make_walker(sin_1, NULL, NULL, 1, 1, R_FLOAT, FALSE, 1, R_NUMBER));
  INIT_WALKER("cos", make_walker(cos_1, NULL, NULL, 1, 1, R_FLOAT, FALSE, 1, R_NUMBER));
  INIT_WALKER("tan", make_walker(tan_1, NULL, NULL, 1, 1, R_FLOAT, FALSE, 1, R_NUMBER));
  INIT_WALKER("abs", make_walker(abs_1, NULL, NULL, 1, 1, R_NUMBER, FALSE, 1, R_NUMBER));
  INIT_WALKER("random", make_walker(random_1, NULL, NULL, 1, 1, R_NUMBER, FALSE, 1, R_NUMBER));
  INIT_WALKER("log", make_walker(log_1, NULL, NULL, 1, 1, R_FLOAT, FALSE, 1, R_NUMBER));
  INIT_WALKER("exp", make_walker(exp_1, NULL, NULL, 1, 1, R_FLOAT, FALSE, 1, R_NUMBER));
  INIT_WALKER("asin", make_walker(asin_1, NULL, NULL, 1, 1, R_FLOAT, FALSE, 1, R_NUMBER));
  INIT_WALKER("acos", make_walker(acos_1, NULL, NULL, 1, 1, R_FLOAT, FALSE, 1, R_NUMBER));
  INIT_WALKER("sqrt", make_walker(sqrt_1, NULL, NULL, 1, 1, R_FLOAT, FALSE, 1, R_NUMBER));
  INIT_WALKER("round", make_walker(round_1, NULL, NULL, 1, 1, R_FLOAT, FALSE, 1, R_NUMBER));
  INIT_WALKER("truncate", make_walker(truncate_1, NULL, NULL, 1, 1, R_FLOAT, FALSE, 1, R_NUMBER));
  INIT_WALKER("floor", make_walker(floor_1, NULL, NULL, 1, 1, R_FLOAT, FALSE, 1, R_NUMBER));
  INIT_WALKER("ceiling", make_walker(ceiling_1, NULL, NULL, 1, 1, R_FLOAT, FALSE, 1, R_NUMBER));
  INIT_WALKER("odd?", make_walker(odd_p, NULL, NULL, 1, 1, R_BOOL, FALSE, 1, R_NUMBER));
  INIT_WALKER("even?", make_walker(even_p, NULL, NULL, 1, 1, R_BOOL, FALSE, 1, R_NUMBER));
  INIT_WALKER("zero?", make_walker(zero_p, NULL, NULL, 1, 1, R_BOOL, FALSE, 1, R_NUMBER));
  INIT_WALKER("positive?", make_walker(positive_p, NULL, NULL, 1, 1, R_BOOL, FALSE, 1, R_NUMBER));
  INIT_WALKER("negative?", make_walker(negative_p, NULL, NULL, 1, 1, R_BOOL, FALSE, 1, R_NUMBER));
  INIT_WALKER("not", make_walker(not_p, NULL, NULL, 1, 1, R_BOOL, FALSE, 0)); /* ?? */

  /* -------- char funcs */
  INIT_WALKER("char-alphabetic?", make_walker(char_alphabetic_p, NULL, NULL, 1, 1, R_BOOL, FALSE, 1, R_CHAR));
  INIT_WALKER("char-numeric?", make_walker(char_numeric_p, NULL, NULL, 1, 1, R_BOOL, FALSE, 1, R_CHAR));
  INIT_WALKER("char-lower-case?", make_walker(char_lower_case_p, NULL, NULL, 1, 1, R_BOOL, FALSE, 1, R_CHAR));
  INIT_WALKER("char-upper-case?", make_walker(char_upper_case_p, NULL, NULL, 1, 1, R_BOOL, FALSE, 1, R_CHAR));
  INIT_WALKER("char-whitespace?", make_walker(char_whitespace_p, NULL, NULL, 1, 1, R_BOOL, FALSE, 1, R_CHAR));
  INIT_WALKER("char-upcase", make_walker(char_upcase, NULL, NULL, 1, 1, R_CHAR, FALSE, 1, R_CHAR));
  INIT_WALKER("char-downcase", make_walker(char_downcase, NULL, NULL, 1, 1, R_CHAR, FALSE, 1, R_CHAR));
  INIT_WALKER("char->integer", make_walker(char_to_integer, NULL, NULL, 1, 1, R_INT, FALSE, 1, R_CHAR));
  INIT_WALKER("char>=?", make_walker(char_ge_1, NULL, NULL, 0, UNLIMITED_ARGS, R_BOOL, FALSE, 1, -R_CHAR));
  INIT_WALKER("char>?", make_walker(char_gt_1, NULL, NULL, 0, UNLIMITED_ARGS, R_BOOL, FALSE, 1, -R_CHAR));
  INIT_WALKER("char<=?", make_walker(char_le_1, NULL, NULL, 0, UNLIMITED_ARGS, R_BOOL, FALSE, 1, -R_CHAR));
  INIT_WALKER("char<?", make_walker(char_lt_1, NULL, NULL, 0, UNLIMITED_ARGS, R_BOOL, FALSE, 1, -R_CHAR));
  INIT_WALKER("char=?", make_walker(char_eq_1, NULL, NULL, 0, UNLIMITED_ARGS, R_BOOL, FALSE, 1, -R_CHAR));
  INIT_WALKER("char-ci>=?", make_walker(char_ci_ge_1, NULL, NULL, 0, UNLIMITED_ARGS, R_BOOL, FALSE, 1, -R_CHAR));
  INIT_WALKER("char-ci>?", make_walker(char_ci_gt_1, NULL, NULL, 0, UNLIMITED_ARGS, R_BOOL, FALSE, 1, -R_CHAR));
  INIT_WALKER("char-ci<=?", make_walker(char_ci_le_1, NULL, NULL, 0, UNLIMITED_ARGS, R_BOOL, FALSE, 1, -R_CHAR));
  INIT_WALKER("char-ci<?", make_walker(char_ci_lt_1, NULL, NULL, 0, UNLIMITED_ARGS, R_BOOL, FALSE, 1, -R_CHAR));
  INIT_WALKER("char-ci=?", make_walker(char_ci_eq_1, NULL, NULL, 0, UNLIMITED_ARGS, R_BOOL, FALSE, 1, -R_CHAR));
  INIT_WALKER("string", make_walker(string_1, NULL, NULL, 0, UNLIMITED_ARGS, R_STRING, FALSE, 1, -R_CHAR));

  /* -------- string funcs */
  INIT_WALKER("string=?", make_walker(string_eq_1, NULL, NULL, 0, UNLIMITED_ARGS, R_BOOL, FALSE, 1, -R_STRING));
  INIT_WALKER("string>=?", make_walker(string_geq_1, NULL, NULL, 0, UNLIMITED_ARGS, R_BOOL, FALSE, 1, -R_STRING));
  INIT_WALKER("string<=?", make_walker(string_leq_1, NULL, NULL, 0, UNLIMITED_ARGS, R_BOOL, FALSE, 1, -R_STRING));
  INIT_WALKER("string>?", make_walker(string_gt_1, NULL, NULL, 0, UNLIMITED_ARGS, R_BOOL, FALSE, 1, -R_STRING));
  INIT_WALKER("string<?", make_walker(string_lt_1, NULL, NULL, 0, UNLIMITED_ARGS, R_BOOL, FALSE, 1, -R_STRING));
  INIT_WALKER("string-ci=?", make_walker(string_ci_eq_1, NULL, NULL, 0, UNLIMITED_ARGS, R_BOOL, FALSE, 1, -R_STRING));
  INIT_WALKER("string-ci>=?", make_walker(string_ci_geq_1, NULL, NULL, 0, UNLIMITED_ARGS, R_BOOL, FALSE, 1, -R_STRING));
  INIT_WALKER("string-ci<=?", make_walker(string_ci_leq_1, NULL, NULL, 0, UNLIMITED_ARGS, R_BOOL, FALSE, 1, -R_STRING));
  INIT_WALKER("string-ci>?", make_walker(string_ci_gt_1, NULL, NULL, 0, UNLIMITED_ARGS, R_BOOL, FALSE, 1, -R_STRING));
  INIT_WALKER("string-ci<?", make_walker(string_ci_lt_1, NULL, NULL, 0, UNLIMITED_ARGS, R_BOOL, FALSE, 1, -R_STRING));
  INIT_WALKER("string-length", make_walker(string_length_1, NULL, NULL, 1, 1, R_INT, FALSE, 1, R_STRING));
  INIT_WALKER("string-copy", make_walker(string_copy_1, NULL, NULL, 1, 1, R_STRING, FALSE, 1, R_STRING));
  INIT_WALKER("string-ref", make_walker(string_ref_1, NULL, NULL, 2, 2, R_CHAR, FALSE, 2, R_STRING, R_INT));
  INIT_WALKER("substring", make_walker(substring_1, NULL, NULL, 3, 3, R_STRING, FALSE, 3, R_STRING, R_INT, R_INT));
  INIT_WALKER("string-fill!", make_walker(string_fill_1, NULL, NULL, 2, 2, R_STRING, FALSE, 2, R_STRING, R_CHAR));
  INIT_WALKER("string-set!", make_walker(string_set_1, NULL, NULL, 3, 3, R_STRING, FALSE, 3, R_STRING, R_INT, R_CHAR));
  INIT_WALKER("string-append", make_walker(string_append_1, NULL, NULL, 0, UNLIMITED_ARGS, R_STRING, FALSE, 1, -R_STRING));

  INIT_WALKER("display", make_walker(display_1, NULL, NULL, 1, 1, R_STRING, FALSE, 0));
  INIT_WALKER("make-string", make_walker(make_string_1, NULL, NULL, 1, 2, R_STRING, FALSE, 2, R_INT, R_CHAR));
  INIT_WALKER("number->string", make_walker(number2string_1, NULL, NULL, 1, 2, R_STRING, FALSE, 2, R_NUMBER, R_INT));
  INIT_WALKER("symbol->string", make_walker(symbol2string_1, NULL, NULL, 1, 1, R_STRING, FALSE, 1, R_SYMBOL));

  /* -------- vector funcs */
  INIT_WALKER("vector-ref", make_walker(vector_ref_1, NULL, NULL, 2, 2, R_ANY, FALSE, 2, R_VECTOR, R_INT));
  INIT_WALKER("vector-length", make_walker(vector_length_1, NULL, NULL, 1, 1, R_INT, FALSE, 1, R_VECTOR));
  INIT_WALKER("vector-fill!", make_walker(vector_fill_1, NULL, NULL, 2, 2, R_INT, FALSE, 1, R_VECTOR));
  INIT_WALKER("vector-set!", make_walker(vector_set_1, NULL, NULL, 3, 3, R_ANY, FALSE, 2, R_VECTOR, R_INT));
  INIT_WALKER("make-vector", make_walker(make_vct_1, NULL, NULL, 1, 2, R_VCT, FALSE, 2, R_INT, R_FLOAT));

  /* -------- list funcs */
  INIT_WALKER("car", make_walker(car_1, NULL, NULL, 1, 1, R_ANY, FALSE, 1, R_CONS));
  INIT_WALKER("cdr", make_walker(cdr_1, NULL, NULL, 1, 1, R_ANY, FALSE, 1, R_PAIR));
  INIT_WALKER("caar", make_walker(caar_1, NULL, NULL, 1, 1, R_ANY, FALSE, 1, R_LIST));
  INIT_WALKER("cadr", make_walker(cadr_1, NULL, NULL, 1, 1, R_ANY, FALSE, 1, R_LIST));
  INIT_WALKER("caaar", make_walker(caaar_1, NULL, NULL, 1, 1, R_ANY, FALSE, 1, R_LIST));
  INIT_WALKER("caadr", make_walker(caadr_1, NULL, NULL, 1, 1, R_ANY, FALSE, 1, R_LIST));
  INIT_WALKER("cadar", make_walker(cadar_1, NULL, NULL, 1, 1, R_ANY, FALSE, 1, R_LIST));
  INIT_WALKER("caddr", make_walker(caddr_1, NULL, NULL, 1, 1, R_ANY, FALSE, 1, R_LIST));
  INIT_WALKER("caaaar", make_walker(caaaar_1, NULL, NULL, 1, 1, R_ANY, FALSE, 1, R_LIST));
  INIT_WALKER("caaadr", make_walker(caaadr_1, NULL, NULL, 1, 1, R_ANY, FALSE, 1, R_LIST));
  INIT_WALKER("caadar", make_walker(caadar_1, NULL, NULL, 1, 1, R_ANY, FALSE, 1, R_LIST));
  INIT_WALKER("caaddr", make_walker(caaddr_1, NULL, NULL, 1, 1, R_ANY, FALSE, 1, R_LIST));
  INIT_WALKER("cadaar", make_walker(cadaar_1, NULL, NULL, 1, 1, R_ANY, FALSE, 1, R_LIST));
  INIT_WALKER("cadadr", make_walker(cadadr_1, NULL, NULL, 1, 1, R_ANY, FALSE, 1, R_LIST));
  INIT_WALKER("caddar", make_walker(caddar_1, NULL, NULL, 1, 1, R_ANY, FALSE, 1, R_LIST));
  INIT_WALKER("cadddr", make_walker(cadddr_1, NULL, NULL, 1, 1, R_ANY, FALSE, 1, R_LIST));
  INIT_WALKER("null?", make_walker(null_p_1, NULL, NULL, 1, 1, R_BOOL, FALSE, 1, R_LIST));
  INIT_WALKER("length", make_walker(list_length_1, NULL, NULL, 1, 1, R_INT, FALSE, 1, R_LIST));


  /* -------- clm funcs */
  INIT_WALKER(S_buffer2sample, make_walker(mus_buffer2sample_0, NULL, NULL, 1, 1, R_BOOL, FALSE, 1, R_CLM));
  INIT_WALKER(S_oscil_p, make_walker(oscil_p, NULL, NULL, 1, 1, R_BOOL, FALSE, 1, R_CLM));
  INIT_WALKER(S_env_p, make_walker(env_p, NULL, NULL, 1, 1, R_BOOL, FALSE, 1, R_CLM));
  INIT_WALKER(S_notch_p, make_walker(notch_p, NULL, NULL, 1, 1, R_BOOL, FALSE, 1, R_CLM));
  INIT_WALKER(S_comb_p, make_walker(comb_p, NULL, NULL, 1, 1, R_BOOL, FALSE, 1, R_CLM));
  INIT_WALKER(S_delay_p, make_walker(delay_p, NULL, NULL, 1, 1, R_BOOL, FALSE, 1, R_CLM));
  INIT_WALKER(S_all_pass_p, make_walker(all_pass_p, NULL, NULL, 1, 1, R_BOOL, FALSE, 1, R_CLM));
  INIT_WALKER(S_rand_p, make_walker(rand_p, NULL, NULL, 1, 1, R_BOOL, FALSE, 1, R_CLM));
  INIT_WALKER(S_rand_interp_p, make_walker(rand_interp_p, NULL, NULL, 1, 1, R_BOOL, FALSE, 1, R_CLM));
  INIT_WALKER(S_sum_of_cosines_p, make_walker(sum_of_cosines_p, NULL, NULL, 1, 1, R_BOOL, FALSE, 1, R_CLM));
  INIT_WALKER(S_table_lookup_p, make_walker(table_lookup_p, NULL, NULL, 1, 1, R_BOOL, FALSE, 1, R_CLM));
  INIT_WALKER(S_sawtooth_wave_p, make_walker(sawtooth_wave_p, NULL, NULL, 1, 1, R_BOOL, FALSE, 1, R_CLM));
  INIT_WALKER(S_pulse_train_p, make_walker(pulse_train_p, NULL, NULL, 1, 1, R_BOOL, FALSE, 1, R_CLM));
  INIT_WALKER(S_square_wave_p, make_walker(square_wave_p, NULL, NULL, 1, 1, R_BOOL, FALSE, 1, R_CLM));
  INIT_WALKER(S_triangle_wave_p, make_walker(triangle_wave_p, NULL, NULL, 1, 1, R_BOOL, FALSE, 1, R_CLM));
  INIT_WALKER(S_asymmetric_fm_p, make_walker(asymmetric_fm_p, NULL, NULL, 1, 1, R_BOOL, FALSE, 1, R_CLM));
  INIT_WALKER(S_sine_summation_p, make_walker(sine_summation_p, NULL, NULL, 1, 1, R_BOOL, FALSE, 1, R_CLM));
  INIT_WALKER(S_one_zero_p, make_walker(one_zero_p, NULL, NULL, 1, 1, R_BOOL, FALSE, 1, R_CLM));
  INIT_WALKER(S_one_pole_p, make_walker(one_pole_p, NULL, NULL, 1, 1, R_BOOL, FALSE, 1, R_CLM));
  INIT_WALKER(S_two_zero_p, make_walker(two_zero_p, NULL, NULL, 1, 1, R_BOOL, FALSE, 1, R_CLM));
  INIT_WALKER(S_two_pole_p, make_walker(two_pole_p, NULL, NULL, 1, 1, R_BOOL, FALSE, 1, R_CLM));
  INIT_WALKER(S_formant_p, make_walker(formant_p, NULL, NULL, 1, 1, R_BOOL, FALSE, 1, R_CLM));
  INIT_WALKER(S_wave_train_p, make_walker(wave_train_p, NULL, NULL, 1, 1, R_BOOL, FALSE, 1, R_CLM));
  INIT_WALKER(S_waveshape_p, make_walker(waveshape_p, NULL, NULL, 1, 1, R_BOOL, FALSE, 1, R_CLM));
  INIT_WALKER(S_filter_p, make_walker(filter_p, NULL, NULL, 1, 1, R_BOOL, FALSE, 1, R_CLM));
  INIT_WALKER(S_fir_filter_p, make_walker(fir_filter_p, NULL, NULL, 1, 1, R_BOOL, FALSE, 1, R_CLM));
  INIT_WALKER(S_iir_filter_p, make_walker(iir_filter_p, NULL, NULL, 1, 1, R_BOOL, FALSE, 1, R_CLM));
  INIT_WALKER(S_readin_p, make_walker(readin_p, NULL, NULL, 1, 1, R_BOOL, FALSE, 1, R_CLM));
  INIT_WALKER(S_src_p, make_walker(src_p, NULL, NULL, 1, 1, R_BOOL, FALSE, 1, R_CLM));
  INIT_WALKER(S_granulate_p, make_walker(granulate_p, NULL, NULL, 1, 1, R_BOOL, FALSE, 1, R_CLM));
  INIT_WALKER(S_phase_vocoder_p, make_walker(phase_vocoder_p, NULL, NULL, 1, 1, R_BOOL, FALSE, 1, R_CLM));
  INIT_WALKER(S_convolve_p, make_walker(convolve_p, NULL, NULL, 1, 1, R_BOOL, FALSE, 1, R_CLM));
  INIT_WALKER(S_buffer_p, make_walker(buffer_p, NULL, NULL, 1, 1, R_BOOL, FALSE, 1, R_CLM));
  INIT_WALKER(S_buffer_empty_p, make_walker(buffer_empty_p, NULL, NULL, 1, 1, R_BOOL, FALSE, 1, R_CLM));
  INIT_WALKER(S_buffer_full_p, make_walker(buffer_full_p, NULL, NULL, 1, 1, R_BOOL, FALSE, 1, R_CLM));
  INIT_WALKER(S_frame_p, make_walker(frame_p, NULL, NULL, 1, 1, R_BOOL, FALSE, 1, R_CLM));
  INIT_WALKER(S_mixer_p, make_walker(mixer_p, NULL, NULL, 1, 1, R_BOOL, FALSE, 1, R_CLM));
  INIT_WALKER(S_file2sample_p, make_walker(file2sample_p, NULL, NULL, 1, 1, R_BOOL, FALSE, 1, R_CLM));
  INIT_WALKER(S_sample2file_p, make_walker(sample2file_p, NULL, NULL, 1, 1, R_BOOL, FALSE, 1, R_CLM));
  INIT_WALKER(S_file2frame_p, make_walker(file2frame_p, NULL, NULL, 1, 1, R_BOOL, FALSE, 1, R_CLM));
  INIT_WALKER(S_frame2file_p, make_walker(frame2file_p, NULL, NULL, 1, 1, R_BOOL, FALSE, 1, R_CLM));
  INIT_WALKER(S_locsig_p, make_walker(locsig_p, NULL, NULL, 1, 1, R_BOOL, FALSE, 1, R_CLM));
  INIT_WALKER(S_mus_input_p, make_walker(input_p, NULL, NULL, 1, 1, R_BOOL, FALSE, 1, R_CLM));
  INIT_WALKER(S_mus_output_p, make_walker(output_p, NULL, NULL, 1, 1, R_BOOL, FALSE, 1, R_CLM));

  INIT_WALKER(S_restart_env, make_walker(restart_env_1, NULL, NULL, 1, 1, R_CLM, FALSE, 1, R_CLM));
  INIT_WALKER(S_mus_increment, make_walker(mus_increment_0, NULL, mus_set_increment_1, 1, 1, R_FLOAT, FALSE, 1, R_CLM));
  INIT_WALKER(S_mus_frequency, make_walker(mus_frequency_0, NULL, mus_set_frequency_1, 1, 1, R_FLOAT, FALSE, 1, R_CLM));
  INIT_WALKER(S_mus_phase, make_walker(mus_phase_0, NULL, mus_set_phase_1, 1, 1, R_FLOAT, FALSE, 1, R_CLM));
  INIT_WALKER(S_mus_width, make_walker(mus_width_0, NULL, mus_set_width_1, 1, 1, R_FLOAT, FALSE, 1, R_CLM));
  INIT_WALKER(S_mus_scaler, make_walker(mus_scaler_0, NULL, mus_set_scaler_1, 1, 1, R_FLOAT, FALSE, 1, R_CLM));
  INIT_WALKER(S_mus_offset, make_walker(mus_offset_0, NULL, NULL, 1, 1, R_FLOAT, FALSE, 1, R_CLM));
  INIT_WALKER(S_mus_formant_radius, make_walker(mus_formant_radius_0, NULL, mus_set_formant_radius_1, 1, 1, R_FLOAT, FALSE, 1, R_CLM));
  INIT_WALKER(S_mus_a0, make_walker(mus_a0_0, NULL, mus_set_a0_1, 1, 1, R_FLOAT, FALSE, 1, R_CLM));
  INIT_WALKER(S_mus_a1, make_walker(mus_a1_0, NULL, mus_set_a1_1, 1, 1, R_FLOAT, FALSE, 1, R_CLM));
  INIT_WALKER(S_mus_a2, make_walker(mus_a2_0, NULL, mus_set_a2_1, 1, 1, R_FLOAT, FALSE, 1, R_CLM));
  INIT_WALKER(S_mus_b1, make_walker(mus_b1_0, NULL, mus_set_b1_1, 1, 1, R_FLOAT, FALSE, 1, R_CLM));
  INIT_WALKER(S_mus_b2, make_walker(mus_b2_0, NULL, mus_set_b2_1, 1, 1, R_FLOAT, FALSE, 1, R_CLM));
  INIT_WALKER(S_mus_x1, make_walker(mus_x1_0, NULL, mus_set_x1_1, 1, 1, R_FLOAT, FALSE, 1, R_CLM));
  INIT_WALKER(S_mus_x2, make_walker(mus_x2_0, NULL, mus_set_x2_1, 1, 1, R_FLOAT, FALSE, 1, R_CLM));
  INIT_WALKER(S_mus_y1, make_walker(mus_y1_0, NULL, mus_set_y1_1, 1, 1, R_FLOAT, FALSE, 1, R_CLM));
  INIT_WALKER(S_mus_y2, make_walker(mus_y2_0, NULL, mus_set_y2_1, 1, 1, R_FLOAT, FALSE, 1, R_CLM));
  INIT_WALKER(S_mus_data, make_walker(mus_data_1, NULL, NULL, 1, 1, R_VCT, FALSE, 1, R_XCLM));
  INIT_WALKER(S_mus_xcoeffs, make_walker(mus_xcoeffs_1, NULL, NULL, 1, 1, R_VCT, FALSE, 1, R_XCLM));
  INIT_WALKER(S_mus_ycoeffs, make_walker(mus_ycoeffs_1, NULL, NULL, 1, 1, R_VCT, FALSE, 1, R_XCLM));
  INIT_WALKER(S_mus_feedforward, make_walker(mus_feedforward_0, NULL, mus_set_feedforward_1, 1, 1, R_FLOAT, FALSE, 1, R_CLM));
  INIT_WALKER(S_mus_feedback, make_walker(mus_feedback_0, NULL, mus_set_feedback_1, 1, 1, R_FLOAT, FALSE, 1, R_CLM));
  INIT_WALKER(S_mus_hop, make_walker(mus_hop_0, NULL, mus_set_hop_1, 1, 1, R_INT, FALSE, 1, R_CLM));
  INIT_WALKER(S_mus_channels, make_walker(mus_channels_0, NULL, NULL, 1, 1, R_INT, FALSE, 1, R_CLM));
  INIT_WALKER(S_mus_channel, make_walker(mus_channel_0, NULL, NULL, 1, 1, R_INT, FALSE, 1, R_CLM));
  INIT_WALKER(S_mus_location, make_walker(mus_location_0, NULL, mus_set_location_1, 1, 1, R_INT, FALSE, 1, R_CLM));
  INIT_WALKER(S_mus_ramp, make_walker(mus_ramp_0, NULL, mus_set_ramp_1, 1, 1, R_INT, FALSE, 1, R_CLM));
  INIT_WALKER(S_mus_position, make_walker(mus_position_0, NULL, NULL, 1, 1, R_INT, FALSE, 1, R_CLM));
  INIT_WALKER(S_mus_order, make_walker(mus_order_0, NULL, NULL, 1, 1, R_INT, FALSE, 1, R_CLM));
  INIT_WALKER(S_mus_length, make_walker(mus_length_0, NULL, mus_set_length_1, 1, 1, R_INT, FALSE, 1, R_CLM));
  INIT_WALKER(S_mus_cosines, make_walker(mus_cosines_0, NULL, mus_set_cosines_1, 1, 1, R_INT, FALSE, 1, R_CLM));
  INIT_WALKER(S_mus_name, make_walker(mus_name_0, NULL, NULL, 1, 1, R_INT, FALSE, 1, R_CLM));
  INIT_WALKER(S_mus_file_name, make_walker(mus_file_name_0, NULL, NULL, 1, 1, R_STRING, FALSE, 1, R_CLM));
  INIT_WALKER(S_mus_describe, make_walker(mus_describe_0, NULL, NULL, 1, 1, R_STRING, FALSE, 1, R_CLM));
  INIT_WALKER(S_mus_inspect, make_walker(mus_inspect_0, NULL, NULL, 1, 1, R_STRING, FALSE, 1, R_CLM));

  INIT_WALKER(S_oscil, make_walker(oscil_1, NULL, NULL, 1, 3, R_FLOAT, FALSE, 3, R_CLM, R_NUMBER, R_NUMBER));
  INIT_WALKER(S_one_zero, make_walker(one_zero_1, NULL, NULL, 1, 2, R_FLOAT, FALSE, 2, R_CLM, R_NUMBER));
  INIT_WALKER(S_one_pole, make_walker(one_pole_1, NULL, NULL, 1, 2, R_FLOAT, FALSE, 2, R_CLM, R_NUMBER));
  INIT_WALKER(S_out_any, make_walker(out_any_1, NULL, NULL, 4, 4, R_FLOAT, FALSE, 4, R_NUMBER, R_NUMBER, R_INT, R_CLM));
  INIT_WALKER(S_outa, make_walker(outa_1, NULL, NULL, 3, 3, R_FLOAT, FALSE, 3, R_NUMBER, R_NUMBER, R_CLM));
  INIT_WALKER(S_outb, make_walker(outb_1, NULL, NULL, 3, 3, R_FLOAT, FALSE, 3, R_NUMBER, R_NUMBER, R_CLM));
  INIT_WALKER(S_outc, make_walker(outc_1, NULL, NULL, 3, 3, R_FLOAT, FALSE, 3, R_NUMBER, R_NUMBER, R_CLM));
  INIT_WALKER(S_outd, make_walker(outd_1, NULL, NULL, 3, 3, R_FLOAT, FALSE, 3, R_NUMBER, R_NUMBER, R_CLM));
  INIT_WALKER(S_env, make_walker(env_1, NULL, NULL, 1, 1, R_FLOAT, FALSE, 1, R_CLM));
  INIT_WALKER(S_env_interp, make_walker(env_interp_1, NULL, NULL, 2, 2, R_FLOAT, FALSE, 2, R_FLOAT, R_CLM));
  INIT_WALKER(S_notch, make_walker(notch_1, NULL, NULL, 1, 3, R_FLOAT, FALSE, 3, R_CLM, R_NUMBER, R_NUMBER));
  INIT_WALKER(S_comb, make_walker(comb_1, NULL, NULL, 1, 3, R_FLOAT, FALSE, 3, R_CLM, R_NUMBER, R_NUMBER));
  INIT_WALKER(S_convolve, make_walker(convolve_1, NULL, NULL, 1, 2, R_FLOAT, FALSE, 2, R_CLM, R_FUNCTION));
  INIT_WALKER(S_delay, make_walker(delay_1, NULL, NULL, 1, 3, R_FLOAT, FALSE, 3, R_CLM, R_NUMBER, R_NUMBER));
  INIT_WALKER(S_all_pass, make_walker(all_pass_1, NULL, NULL, 1, 3, R_FLOAT, FALSE, 3, R_CLM, R_NUMBER, R_NUMBER));
  INIT_WALKER(S_asymmetric_fm, make_walker(asymmetric_fm_1, NULL, NULL, 1, 3, R_FLOAT, FALSE, 3, R_CLM, R_NUMBER, R_NUMBER));
  INIT_WALKER(S_rand, make_walker(rand_1, NULL, NULL, 1, 2, R_FLOAT, FALSE, 2, R_CLM, R_NUMBER));
  INIT_WALKER(S_rand_interp, make_walker(rand_interp_1, NULL, NULL, 1, 2, R_FLOAT, FALSE, 2, R_CLM, R_NUMBER));
  INIT_WALKER(S_readin, make_walker(readin_1, NULL, NULL, 1, 1, R_FLOAT, FALSE, 1, R_CLM));
  INIT_WALKER(S_src, make_walker(src_1, NULL, NULL, 1, 3, R_FLOAT, FALSE, 3, R_CLM, R_NUMBER, R_FUNCTION));
  INIT_WALKER(S_sum_of_cosines, make_walker(sum_of_cosines_1, NULL, NULL, 1, 2, R_FLOAT, FALSE, 2, R_CLM, R_NUMBER));
  INIT_WALKER(S_sawtooth_wave, make_walker(sawtooth_wave_1, NULL, NULL, 1, 2, R_FLOAT, FALSE, 2, R_CLM, R_NUMBER));
  INIT_WALKER(S_square_wave, make_walker(square_wave_1, NULL, NULL, 1, 2, R_FLOAT, FALSE, 2, R_CLM, R_NUMBER));
  INIT_WALKER(S_sine_summation, make_walker(sine_summation_1, NULL, NULL, 1, 2, R_FLOAT, FALSE, 2, R_CLM, R_NUMBER));
  INIT_WALKER(S_sample2file, make_walker(sample2file_1, NULL, NULL, 4, 4, R_FLOAT, FALSE, 4, R_CLM, R_NUMBER, R_INT, R_NUMBER));
  INIT_WALKER(S_sample2buffer, make_walker(sample2buffer_1, NULL, NULL, 2, 2, R_FLOAT, FALSE, 2, R_CLM, R_NUMBER));
  INIT_WALKER(S_table_lookup, make_walker(table_lookup_1, NULL, NULL, 1, 2, R_FLOAT, FALSE, 2, R_CLM, R_NUMBER));
  INIT_WALKER(S_triangle_wave, make_walker(triangle_wave_1, NULL, NULL, 1, 2, R_FLOAT, FALSE, 2, R_CLM, R_NUMBER));
  INIT_WALKER(S_two_zero, make_walker(two_zero_1, NULL, NULL, 1, 2, R_FLOAT, FALSE, 2, R_CLM, R_NUMBER));
  INIT_WALKER(S_two_pole, make_walker(two_pole_1, NULL, NULL, 1, 2, R_FLOAT, FALSE, 2, R_CLM, R_NUMBER));
  INIT_WALKER(S_tap, make_walker(tap_1, NULL, NULL, 1, 2, R_FLOAT, FALSE, 2, R_CLM, R_NUMBER));
  INIT_WALKER(S_pulse_train, make_walker(pulse_train_1, NULL, NULL, 1, 2, R_FLOAT, FALSE, 2, R_CLM, R_NUMBER));
  INIT_WALKER(S_phase_vocoder, make_walker(phase_vocoder_1, NULL, NULL, 1, 2, R_FLOAT, FALSE, 2, R_CLM, R_FUNCTION));
  INIT_WALKER(S_formant, make_walker(formant_1, NULL, NULL, 1, 2, R_FLOAT, FALSE, 2, R_CLM, R_NUMBER));
  INIT_WALKER(S_filter, make_walker(filter_1, NULL, NULL, 1, 2, R_FLOAT, FALSE, 2, R_CLM, R_NUMBER));
  INIT_WALKER(S_fir_filter, make_walker(fir_filter_1, NULL, NULL, 1, 2, R_FLOAT, FALSE, 2, R_CLM, R_NUMBER));
  INIT_WALKER(S_file2sample, make_walker(file2sample_1, NULL, NULL, 2, 3, R_FLOAT, FALSE, 3, R_CLM, R_NUMBER, R_INT));
  INIT_WALKER(S_frame_ref, make_walker(frame_ref_0, NULL, frame_set_1, 2, 2, R_FLOAT, FALSE, 2, R_CLM, R_INT));
  INIT_WALKER(S_frame_set, make_walker(frame_set_2, NULL, NULL, 3, 3, R_FLOAT, FALSE, 3, R_CLM, R_INT, R_NUMBER));
  INIT_WALKER(S_wave_train, make_walker(wave_train_1, NULL, NULL, 1, 2, R_FLOAT, FALSE, 2, R_CLM, R_NUMBER));
  INIT_WALKER(S_waveshape, make_walker(waveshape_1, NULL, NULL, 1, 3, R_FLOAT, FALSE, 3, R_CLM, R_NUMBER, R_NUMBER));
  INIT_WALKER(S_iir_filter, make_walker(iir_filter_1, NULL, NULL, 1, 2, R_FLOAT, FALSE, 2, R_CLM, R_NUMBER));
  INIT_WALKER(S_ina, make_walker(ina_1, NULL, NULL, 2, 2, R_FLOAT, FALSE, 2, R_NUMBER, R_CLM));
  INIT_WALKER(S_inb, make_walker(inb_1, NULL, NULL, 2, 2, R_FLOAT, FALSE, 2, R_NUMBER, R_CLM));
  INIT_WALKER(S_in_any, make_walker(in_any_1, NULL, NULL, 3, 3, R_FLOAT, FALSE, 2, R_NUMBER, R_INT, R_CLM));
  INIT_WALKER(S_granulate, make_walker(granulate_1, NULL, NULL, 1, 2, R_FLOAT, FALSE, 2, R_CLM, R_FUNCTION));
  INIT_WALKER(S_move_locsig, make_walker(move_locsig_1, NULL, NULL, 3, 3, R_FLOAT, FALSE, 3, R_CLM, R_NUMBER, R_NUMBER));
  INIT_WALKER(S_mus_set_formant_radius_and_frequency, make_walker(set_formant_radius_and_frequency_1, NULL, NULL, 3, 3, R_FLOAT, FALSE, 3, R_CLM, R_NUMBER, R_NUMBER));
  INIT_WALKER(S_mixer_set, make_walker(mixer_set_2, NULL, NULL, 4, 4, R_FLOAT, FALSE, 4, R_CLM, R_INT, R_INT, R_NUMBER));
  INIT_WALKER(S_mixer_ref, make_walker(mixer_ref_1, NULL, mixer_set_1, 3, 3, R_FLOAT, FALSE, 3, R_CLM, R_INT, R_INT));
  INIT_WALKER(S_locsig_ref, make_walker(locsig_ref_0, NULL, locsig_set_1, 2, 2, R_FLOAT, FALSE, 2, R_CLM, R_INT));
  INIT_WALKER(S_locsig_reverb_ref, make_walker(locsig_reverb_ref_0, NULL, locsig_reverb_set_1, 2, 2, R_FLOAT, FALSE, 2, R_CLM, R_INT));
  INIT_WALKER(S_locsig_set, make_walker(locsig_set_2, NULL, NULL, 3, 3, R_FLOAT, FALSE, 3, R_CLM, R_INT, R_NUMBER));
  INIT_WALKER(S_locsig_reverb_set, make_walker(locsig_reverb_set_2, NULL, NULL, 3, 3, R_FLOAT, FALSE, 3, R_CLM, R_INT, R_NUMBER));
  INIT_WALKER(S_polynomial, make_walker(polynomial_1, NULL, NULL, 2, 2, R_FLOAT, FALSE, 0));
  INIT_WALKER(S_clear_array, make_walker(clear_array_1, NULL, NULL, 1, 1, R_VCT, FALSE, 1, R_VCT));
  INIT_WALKER(S_array_interp, make_walker(array_interp_1, NULL, NULL, 2, 3, R_FLOAT, FALSE, 3, R_VCT, R_FLOAT, R_INT));
  INIT_WALKER(S_mus_srate, make_walker(mus_srate_1, NULL, mus_set_srate_1, 0, 0, R_NUMBER, FALSE, 0));
  INIT_WALKER(S_ring_modulate, make_walker(ring_modulate_1, NULL, NULL, 2, 2, R_FLOAT, FALSE, 2, R_NUMBER, R_NUMBER));
  INIT_WALKER(S_amplitude_modulate, make_walker(amplitude_modulate_1, NULL, NULL, 3, 3, R_FLOAT, FALSE, 3, R_NUMBER, R_NUMBER, R_NUMBER));
  INIT_WALKER(S_contrast_enhancement, make_walker(contrast_enhancement_1, NULL, NULL, 2, 2, R_FLOAT, FALSE, 2, R_NUMBER, R_NUMBER));
  INIT_WALKER(S_dot_product, make_walker(dot_product_1, NULL, NULL, 2, 2, R_FLOAT, FALSE, 2, R_VCT, R_VCT));
  INIT_WALKER(S_sum_of_sines, make_walker(sum_of_sines_1, NULL, NULL, 2, 2, R_FLOAT, FALSE, 2, R_VCT, R_VCT));
  INIT_WALKER(S_polar2rectangular, make_walker(polar2rectangular_1, NULL, NULL, 2, 2, R_VCT, FALSE, 2, R_VCT, R_VCT));
  INIT_WALKER(S_rectangular2polar, make_walker(rectangular2polar_1, NULL, NULL, 2, 2, R_VCT, FALSE, 2, R_VCT, R_VCT));
  INIT_WALKER(S_multiply_arrays, make_walker(multiply_arrays_1, NULL, NULL, 2, 3, R_VCT, FALSE, 3, R_VCT, R_VCT, R_INT));
  INIT_WALKER(S_mus_fft, make_walker(mus_fft_1, NULL, NULL, 2, 4, R_VCT, FALSE, 4, R_VCT, R_VCT, R_INT, R_INT));
  INIT_WALKER(S_spectrum, make_walker(mus_spectrum_1, NULL, NULL, 2, 5, R_VCT, FALSE, 5, R_VCT, R_VCT, R_VCT, R_INT, R_INT));
  INIT_WALKER(S_convolution, make_walker(convolution_1, NULL, NULL, 2, 3, R_VCT, FALSE, 3, R_VCT, R_VCT, R_INT));
  INIT_WALKER(S_formant_bank, make_walker(formant_bank_1,NULL, NULL, 3, 3, R_FLOAT, FALSE, 1, R_VCT));
  INIT_WALKER(S_frame_add, make_walker(mus_frame_add_1, NULL, NULL, 2, 3, R_CLM, FALSE, 3, R_CLM, R_CLM, R_CLM));
  INIT_WALKER(S_frame_multiply, make_walker(mus_frame_multiply_1, NULL, NULL, 2, 3, R_CLM, FALSE, 3, R_CLM, R_CLM, R_CLM));
  INIT_WALKER(S_mixer_multiply, make_walker(mus_mixer_multiply_1, NULL, NULL, 2, 3, R_CLM, FALSE, 3, R_CLM, R_CLM, R_CLM));
  INIT_WALKER(S_frame2frame, make_walker(mus_frame2frame_1, NULL, NULL, 2, 3, R_CLM, FALSE, 3, R_CLM, R_CLM, R_CLM));
  INIT_WALKER(S_frame2sample, make_walker(frame2sample_1, NULL, NULL, 2, 2, R_FLOAT, FALSE, 2, R_CLM, R_CLM));
  INIT_WALKER(S_sample2frame, make_walker(sample2frame_1, NULL, NULL, 2, 3, R_FLOAT, FALSE, 3, R_CLM, R_FLOAT, R_CLM));
  INIT_WALKER(S_locsig, make_walker(locsig_1, NULL, NULL, 3, 3, R_CLM, FALSE, 3, R_CLM, R_NUMBER, R_NUMBER));
  INIT_WALKER(S_frame2buffer, make_walker(frame2buffer_1, NULL, NULL, 2, 2, R_CLM, FALSE, 2, R_CLM, R_CLM));
  INIT_WALKER(S_buffer2frame, make_walker(buffer2frame_1, NULL, NULL, 1, 2, R_CLM, FALSE, 2, R_CLM, R_CLM));
  INIT_WALKER(S_frame2file, make_walker(frame2file_1, NULL, NULL, 3, 3, R_CLM, FALSE, 3, R_CLM, R_NUMBER, R_CLM));
  INIT_WALKER(S_file2frame, make_walker(file2frame_1, NULL, NULL, 2, 3, R_CLM, FALSE, 3, R_CLM, R_NUMBER, R_CLM));

  INIT_WALKER(S_radians_hz, make_walker(mus_radians2hz_1, NULL, NULL, 1, 1, R_FLOAT, FALSE, 1, R_NUMBER));
  INIT_WALKER(S_hz_radians, make_walker(mus_hz2radians_1, NULL, NULL, 1, 1, R_FLOAT, FALSE, 1, R_NUMBER));
  INIT_WALKER(S_in_hz, make_walker(mus_hz2radians_1, NULL, NULL, 1, 1, R_FLOAT, FALSE, 1, R_NUMBER));
  INIT_WALKER(S_degrees_radians, make_walker(mus_degrees2radians_1, NULL, NULL, 1, 1, R_FLOAT, FALSE, 1, R_NUMBER));
  INIT_WALKER(S_radians_degrees, make_walker(mus_radians2degrees_1, NULL, NULL, 1, 1, R_FLOAT, FALSE, 1, R_NUMBER));
  INIT_WALKER(S_db_linear, make_walker(mus_db2linear_1, NULL, NULL, 1, 1, R_FLOAT, FALSE, 1, R_NUMBER));
  INIT_WALKER(S_linear_db, make_walker(mus_linear2db_1, NULL, NULL, 1, 1, R_FLOAT, FALSE, 1, R_NUMBER));
  INIT_WALKER(S_mus_random, make_walker(mus_random_1, NULL, NULL, 1, 1, R_FLOAT, FALSE, 1, R_NUMBER));

  INIT_WALKER(S_make_all_pass, make_walker(make_all_pass_1, NULL, NULL, 0, UNLIMITED_ARGS, R_CLM, FALSE, 1, -R_XEN));
  INIT_WALKER(S_make_asymmetric_fm, make_walker(make_asymmetric_fm_1, NULL, NULL, 0, 8, R_CLM, FALSE, 1, -R_XEN));
  INIT_WALKER(S_make_buffer, make_walker(make_buffer_1, NULL, NULL, 0, 4, R_CLM, FALSE, 1, -R_XEN));
  INIT_WALKER(S_make_comb, make_walker(make_comb_1, NULL, NULL, 0, UNLIMITED_ARGS, R_CLM, FALSE, 1, -R_XEN));
  INIT_WALKER(S_make_convolve, make_walker(make_convolve_1, NULL, NULL, 0, UNLIMITED_ARGS, R_CLM, FALSE, 1, -R_XEN));
  INIT_WALKER(S_make_delay, make_walker(make_delay_1, NULL, NULL, 0, UNLIMITED_ARGS, R_CLM, FALSE, 1, -R_XEN));
  INIT_WALKER(S_make_env, make_walker(make_env_1, NULL, NULL, 0, UNLIMITED_ARGS, R_CLM, FALSE, 1, -R_XEN));
  INIT_WALKER(S_make_fft_window, make_walker(make_fft_window_1, NULL, NULL, 2, 3, R_VCT, FALSE, 1, -R_XEN));
  INIT_WALKER(S_make_file2frame, make_walker(make_file2frame_1, NULL, NULL, 0, 1, R_CLM, FALSE, 1, -R_XEN));
  INIT_WALKER(S_make_file2sample, make_walker(make_file2sample_1, NULL, NULL, 0, 1, R_CLM, FALSE, 1, -R_XEN));
  INIT_WALKER(S_make_filter, make_walker(make_filter_1, NULL, NULL, 0, 6, R_CLM, FALSE, 1, -R_XEN));
  INIT_WALKER(S_make_fir_filter, make_walker(make_fir_filter_1, NULL, NULL, 0, 4, R_CLM, FALSE, 1, -R_XEN));
  INIT_WALKER(S_make_formant, make_walker(make_formant_1, NULL, NULL, 0, 6, R_CLM, FALSE, 1, -R_XEN));
  INIT_WALKER(S_make_frame, make_walker(make_frame_1, NULL, NULL, 0, UNLIMITED_ARGS, R_CLM, FALSE, 1, -R_XEN));
  INIT_WALKER(S_make_frame2file, make_walker(make_frame2file_1, NULL, NULL, 0, 4, R_CLM, FALSE, 1, -R_XEN));
  INIT_WALKER(S_make_granulate, make_walker(make_granulate_1, NULL, NULL, 0, UNLIMITED_ARGS, R_CLM, FALSE, 1, -R_XEN));
  INIT_WALKER(S_make_iir_filter, make_walker(make_iir_filter_1, NULL, NULL, 0, 4, R_CLM, FALSE, 1, -R_XEN));
  INIT_WALKER(S_make_locsig, make_walker(make_locsig_1, NULL, NULL, 0, UNLIMITED_ARGS, R_CLM, FALSE, 1, -R_XEN));
  INIT_WALKER(S_make_mixer, make_walker(make_mixer_1, NULL, NULL, 0, UNLIMITED_ARGS, R_CLM, FALSE, 1, -R_XEN));
  INIT_WALKER(S_make_notch, make_walker(make_notch_1, NULL, NULL, 0, UNLIMITED_ARGS, R_CLM, FALSE, 1, -R_XEN));
  INIT_WALKER(S_make_one_pole, make_walker(make_one_pole_1, NULL, NULL, 0, 4, R_CLM, FALSE, 1, -R_XEN));
  INIT_WALKER(S_make_one_zero, make_walker(make_one_zero_1, NULL, NULL, 0, 4, R_CLM, FALSE, 1, -R_XEN));
  INIT_WALKER(S_make_oscil, make_walker(make_oscil_1, NULL, NULL, 0, 4, R_CLM, FALSE, 1, -R_XEN));
  INIT_WALKER(S_make_phase_vocoder, make_walker(make_phase_vocoder_1, NULL, NULL, 0, UNLIMITED_ARGS, R_CLM, FALSE, 1, -R_XEN));
  INIT_WALKER(S_make_ppolar, make_walker(make_ppolar_1, NULL, NULL, 0, 4, R_CLM, FALSE, 1, -R_XEN));
  INIT_WALKER(S_make_pulse_train, make_walker(make_pulse_train_1, NULL, NULL, 0, 6, R_CLM, FALSE, 1, -R_XEN));
  INIT_WALKER(S_make_rand, make_walker(make_rand_1, NULL, NULL, 0, 4, R_CLM, FALSE, 1, -R_XEN));
  INIT_WALKER(S_make_rand_interp, make_walker(make_rand_interp_1, NULL, NULL, 0, 4, R_CLM, FALSE, 1, -R_XEN));
  INIT_WALKER(S_make_readin, make_walker(make_readin_1, NULL, NULL, 0, 8, R_CLM, FALSE, 1, -R_XEN));
  INIT_WALKER(S_make_sample2file, make_walker(make_sample2file_1, NULL, NULL, 4, 5, R_CLM, FALSE, 1, -R_XEN));
  INIT_WALKER(S_make_sawtooth_wave, make_walker(make_sawtooth_wave_1, NULL, NULL, 0, 6, R_CLM, FALSE, 1, -R_XEN));
  INIT_WALKER(S_make_sine_summation, make_walker(make_sine_summation_1, NULL, NULL, 0, UNLIMITED_ARGS, R_CLM, FALSE, 1, -R_XEN));
  INIT_WALKER(S_make_square_wave, make_walker(make_square_wave_1, NULL, NULL, 0, 6, R_CLM, FALSE, 1, -R_XEN));
  INIT_WALKER(S_make_src, make_walker(make_src_1, NULL, NULL, 0, 6, R_CLM, FALSE, 1, -R_XEN));
  INIT_WALKER(S_make_sum_of_cosines, make_walker(make_sum_of_cosines_1, NULL, NULL, 0, 6, R_CLM, FALSE, 1, -R_XEN));
  INIT_WALKER(S_make_table_lookup, make_walker(make_table_lookup_1, NULL, NULL, 0, 6, R_CLM, FALSE, 1, -R_XEN));
  INIT_WALKER(S_make_triangle_wave, make_walker(make_triangle_wave_1, NULL, NULL, 0, 6, R_CLM, FALSE, 1, -R_XEN));
  INIT_WALKER(S_make_two_pole, make_walker(make_two_pole_1, NULL, NULL, 0, 6, R_CLM, FALSE, 1, -R_XEN));
  INIT_WALKER(S_make_two_zero, make_walker(make_two_zero_1, NULL, NULL, 0, 6, R_CLM, FALSE, 1, -R_XEN));
  INIT_WALKER(S_make_wave_train, make_walker(make_wave_train_1, NULL, NULL, 0, 6, R_CLM, FALSE, 1, -R_XEN));
  INIT_WALKER(S_make_waveshape, make_walker(make_waveshape_1, NULL, NULL, 0, 8, R_CLM, FALSE, 1, -R_XEN));
  INIT_WALKER(S_make_zpolar, make_walker(make_zpolar_1, NULL, NULL, 0, 4, R_CLM, FALSE, 1, -R_XEN));


  /* -------- sndlib funcs */
  INIT_WALKER(S_mus_sound_samples, make_walker(mus_sound_samples_1, NULL, NULL, 1, 1, R_INT, FALSE, 1, R_STRING));
  INIT_WALKER(S_mus_sound_frames, make_walker(mus_sound_frames_1, NULL, NULL, 1, 1, R_INT, FALSE, 1, R_STRING));
  INIT_WALKER(S_mus_sound_datum_size, make_walker(mus_sound_datum_size_1, NULL, NULL, 1, 1, R_INT, FALSE, 1, R_STRING));
  INIT_WALKER(S_mus_sound_data_location, make_walker(mus_sound_data_location_1, NULL, NULL, 1, 1, R_INT, FALSE, 1, R_STRING));
  INIT_WALKER(S_mus_sound_chans, make_walker(mus_sound_chans_1, NULL, NULL, 1, 1, R_INT, FALSE, 1, R_STRING));
  INIT_WALKER(S_mus_sound_srate, make_walker(mus_sound_srate_1, NULL, NULL, 1, 1, R_INT, FALSE, 1, R_STRING));
  INIT_WALKER(S_mus_sound_header_type, make_walker(mus_sound_header_type_1, NULL, NULL, 1, 1, R_INT, FALSE, 1, R_STRING));
  INIT_WALKER(S_mus_sound_data_format, make_walker(mus_sound_data_format_1, NULL, NULL, 1, 1, R_INT, FALSE, 1, R_STRING));
  INIT_WALKER(S_mus_sound_length, make_walker(mus_sound_length_1, NULL, NULL, 1, 1, R_INT, FALSE, 1, R_STRING));
  INIT_WALKER(S_mus_sound_duration, make_walker(mus_sound_duration_1, NULL, NULL, 1, 1, R_FLOAT, FALSE, 1, R_STRING));
  INIT_WALKER(S_mus_sound_comment, make_walker(mus_sound_comment_1, NULL, NULL, 1, 1, R_STRING, FALSE, 1, R_STRING));

  INIT_WALKER(S_mus_header_type_name, make_walker(mus_header_type_name_1, NULL, NULL, 1, 1, R_STRING, FALSE, 1, R_INT));
  INIT_WALKER(S_mus_data_format_name, make_walker(mus_data_format_name_1, NULL, NULL, 1, 1, R_STRING, FALSE, 1, R_INT));

  INIT_WALKER(S_vct_ref, make_walker(vct_ref_1, NULL, vct_set_1, 2, 2, R_FLOAT, FALSE, 2, R_VCT, R_INT));
  INIT_WALKER(S_vct_length, make_walker(vct_length_1, NULL, NULL, 1, 1, R_INT, FALSE, 1, R_VCT));
  INIT_WALKER(S_vct_fillB, make_walker(vct_fill_1, NULL, NULL, 2, 2, R_VCT, FALSE, 2, R_VCT, R_NUMBER));
  INIT_WALKER(S_vct_scaleB, make_walker(vct_scale_1, NULL, NULL, 2, 2, R_VCT, FALSE, 2, R_VCT, R_NUMBER));
  INIT_WALKER(S_vct_offsetB, make_walker(vct_offset_1, NULL, NULL, 2, 2, R_VCT, FALSE, 2, R_VCT, R_NUMBER));
  INIT_WALKER(S_vct_addB, make_walker(vct_add_1, NULL, NULL, 2, 2, R_VCT, FALSE, 2, R_VCT, R_VCT));
  INIT_WALKER(S_vct_subtractB, make_walker(vct_subtract_1, NULL, NULL, 2, 2, R_VCT, FALSE, 2, R_VCT, R_VCT));
  INIT_WALKER(S_vct_multiplyB, make_walker(vct_multiply_1, NULL, NULL, 2, 2, R_VCT, FALSE, 2, R_VCT, R_VCT));
  INIT_WALKER(S_vct_copy, make_walker(vct_copy_1, NULL, NULL, 1, 1, R_VCT, FALSE, 1, R_VCT));
  INIT_WALKER(S_vct_peak, make_walker(vct_peak_1, NULL, NULL, 1, 1, R_FLOAT, FALSE, 1, R_VCT));
  INIT_WALKER(S_vct_setB, make_walker(vct_set_2, NULL, NULL, 3, 3, R_FLOAT, FALSE, 3, R_VCT, R_INT, R_NUMBER));
  INIT_WALKER(S_make_vct, make_walker(make_vct_1, NULL, NULL, 1, 2, R_VCT, FALSE, 2, R_INT, R_FLOAT));
  INIT_WALKER(S_vct_convolve, make_walker(vct_convolve_1, NULL, NULL, 2, 2, R_VCT, FALSE, 2, R_VCT, R_VCT));
  INIT_WALKER(S_vct, make_walker(vct_1, NULL, NULL, 1, UNLIMITED_ARGS, R_VCT, FALSE, 1, -R_FLOAT));
  INIT_WALKER(S_vct_p, make_walker(vct_p_1, NULL, NULL, 1, 1, R_BOOL, FALSE, 0));

  /* -------- snd funcs */
  INIT_WALKER(S_next_sample, make_walker(next_sample_1, NULL, NULL, 1, 1, R_FLOAT, FALSE, 1, R_READER));
  INIT_WALKER(S_previous_sample, make_walker(previous_sample_1, NULL, NULL, 1, 1, R_FLOAT, FALSE, 1, R_READER));
  INIT_WALKER(S_read_sample, make_walker(reader_1, NULL, NULL, 1, 1, R_FLOAT, FALSE, 1, R_READER));
  INIT_WALKER(S_make_sample_reader, make_walker(make_sample_reader_1, NULL, NULL, 0, 5, R_READER, FALSE, 1, R_NUMBER));
  INIT_WALKER(S_sample_reader_p, make_walker(sample_reader_p_1, NULL, NULL, 1, 1, R_BOOL, FALSE, 0));

  INIT_WALKER(S_edit_position, make_walker(edit_position_1, NULL, NULL, 0, 2, R_INT, FALSE, 0));
  INIT_WALKER(S_frames, make_walker(frames_1, NULL, NULL, 0, 3, R_INT, FALSE, 0));
  INIT_WALKER(S_cursor, make_walker(cursor_1, NULL, NULL, 0, 2, R_INT, FALSE, 0));
  INIT_WALKER(S_add_mark, make_walker(add_mark_1, NULL, NULL, 1, 3, R_INT, FALSE, 0));
  INIT_WALKER(S_maxamp, make_walker(maxamp_1, NULL, NULL, 0, 2, R_FLOAT, FALSE, 0));
  INIT_WALKER(S_srate, make_walker(srate_1, NULL, NULL, 0, 1, R_INT, FALSE, 0));
  INIT_WALKER(S_channels, make_walker(channels_1, NULL, NULL, 0, 1, R_INT, FALSE, 0));
  INIT_WALKER(S_c_g, make_walker(c_g_p_1, NULL, NULL, 0, 0, R_BOOL, FALSE, 0));
  INIT_WALKER(S_autocorrelate, make_walker(autocorrelate_1, NULL, NULL, 1, 1, R_VCT, FALSE, 1, R_VCT));

  INIT_WALKER(S_snd_print, make_walker(snd_print_1, NULL, NULL, 1, 1, R_BOOL, FALSE, 1, R_STRING));
  INIT_WALKER(S_snd_warning, make_walker(snd_warning_1, NULL, NULL, 1, 1, R_BOOL, FALSE, 1, R_STRING));
  INIT_WALKER(S_report_in_minibuffer, make_walker(report_in_minibuffer_1, NULL, NULL, 1, 2, R_BOOL, FALSE, 1, R_STRING));
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
  XEN_DEFINE_PROCEDURE("describe-walk-info", g_describe_walk_info, 1, 0, 0, "internal debugging aid");
  XEN_DEFINE_PROCEDURE("show-ptree", g_show_ptree, 1, 0, 0, "internal debugging stuff");
#else
  XEN_DEFINE_PROCEDURE(S_vct_map, g_vct_map_w, 2, 0, 0, H_vct_map);
#endif

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_optimization, g_optimization_w, H_optimization,
				   S_setB S_optimization, g_set_optimization_w,  0, 0, 1, 0);

  #define H_optimization_hook S_optimization_hook " (msg): called if the run macro encounters \
something it can't optimize.  'msg' is a string description of the offending form:\n\
  (add-hook! optimization-hook (lambda (msg) (snd-print msg)))\n\
You can often slightly rewrite the form to make run happy."

  XEN_DEFINE_HOOK(optimization_hook, S_optimization_hook, 1, H_optimization_hook);      /* arg = message */

#if HAVE_GUILE && WITH_RUN 
  init_walkers();
  init_type_names();
#endif
}
