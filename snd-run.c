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
 *
 *
 * exported:
 *      (static void *form_to_ptree(XEN code) parse code, returning pointer to tree (a list) or null if code has something we can't handle)
 *   void *form_to_ptree_1f2f(XEN code) -- (1 arg) adds type check that result is Float
 *   void *form_to_ptree_0f2f(XEN code) -- (no args) adds type check that result is Float
 *   void *form_to_ptree_1f2b(XEN code) -- (1 arg) adds type check that result is boolean
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
 *   types: float int char string boolean vct snd_fd mus_any vector function
 *
 *   lambda (use 'declare' to set arg types)
 *   call-with-current-continuation call/cc
 *   if begin or and not let let* set! cond do do* define case[int keys]
 *   * + - / > < >= <= = max min 1+ 1-
 *   sin cos tan abs log exp expt acos asin atan sqrt
 *   boolean? exact? inexact? integer? real? number? quote
 *   odd? even? zero? positive? negative? eq? eqv? equal?
 *   round truncate floor ceiling exact->inexact inexact->exact
 *   gcd lcm logand logior logxor lognot ash modulo remainder quotient random
 *   char? char=? char<? char>? char<=? char>=? and char-ci*
 *   char-alphabetic? char-numeric? char-lower-case? char-upper-case? char-whitespace? 
 *   char-upcase char-downcase char->integer integer->char
 *   string? string string-length string-copy string-fill! string-ref string-set!
 *   make-string substring string-append string=? string<=? string>=? string<? string>? and string-ci*
 *   display number->string
 *   make-vector if 2nd arg exists and is float
 *
 *   various sndlib, clm, and snd functions
 *
 * tests in snd-test.scm, test 22
 *
 *
 * TODO: procedure property 'ptree -> saved ptree
 *
 *
 * LIMITATIONS: <insert anxious lucubration here about DSP context and so on>
 *      variables can have only one type, the type has to be ascertainable somehow (similarly for vector elements)
 *      some variables (imported from outside our context) cannot be set, in some cases they can't even be found (args to define* for example)
 *      no recursion (could be added with some pain)
 *      no lists or pairs (these could be added, perhaps)
 *      no macro expansion (not sure how to handle this in Guile)
 *      no complex, ratio, bignum
 *      no pointer aliasing (i.e. vct var set to alias another vct var etc -- GC confusion otherwise)
 *      no symbols (could be added if there were any conceivable need)
 *      no apply or eval (we need to know at parse time what we are trying to do)
 *      no "delay/force", no syntax-case fanciness
 *      no map or for-each (these need lists)
 *
 * whenever the code-walker or tree initializer finds something it is unhappy about,
 *  it returns an error indication, and the caller should fallback on Guile's evaluator.
 */

#include "snd.h"
#include "vct.h"
#include "clm2xen.h"

static XEN optimization_hook = XEN_FALSE;

/* this code assumes a void* is the same size as int */
#if HAVE_GUILE && WITH_RUN && HAVE_STRINGIZE

#define C_TO_XEN_CHAR(c) SCM_MAKE_CHAR(c)
#define XEN_CDDDR(a) SCM_CDDDR(a)
#define XEN_CAAR(a) XEN_CAR(XEN_CAR(a))
#define XEN_CDAR(a) XEN_CDR(XEN_CAR(a))
#define XEN_CDADR(a) XEN_CDR(XEN_CADR(a))

#define INT_PT "i%d(%d)"
#define FLT_PT "d%d(%.4f)"
#define PTR_PT "i%d(%p)"
#define STR_PT "i%d(\"%s\")"
#define CHR_PT "i%d(#\\%c)"

/* find and set (Guile) variable values */

static void xen_symbol_name_set_value(char *a, XEN b)
{
  XEN var = XEN_FALSE;
  var = scm_sym2var(scm_str2symbol(a), scm_current_module_lookup_closure(), XEN_FALSE);
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
      code_env = SCM_ENV(code);
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
		  if ((XEN_TRUE_P(scm_pair_p(pair))) && 
		      (XEN_EQ_P(XEN_CAR(pair), sym)))
		    return(XEN_CDR(pair));
		}
	      code_env = XEN_CDR(code_env);
	    }
	}
    }
  val = scm_sym2var(sym, scm_current_module_lookup_closure(), XEN_FALSE);
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
      code_env = SCM_ENV(code);
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
	      if ((XEN_TRUE_P(scm_pair_p(pair))) && (XEN_EQ_P(XEN_CAR(pair), sym)))
		{
		  scm_set_cdr_x(pair, new_val);
		  return(new_val);
		}
	      code_env = XEN_CDR(code_env);
	    }
	}
    }
  var = scm_sym2var(sym, scm_current_module_lookup_closure(), XEN_FALSE);
  if (!(XEN_FALSE_P(var)))
    XEN_VARIABLE_SET(var, new_val);
  return(new_val);
}


enum {R_UNSPECIFIED, R_INT, R_FLOAT, R_BOOL, R_CHAR, R_STRING, R_FUNCTION, R_GOTO, R_VCT, R_READER, R_CLM, 
      R_FLOAT_VECTOR, R_INT_VECTOR, R_VCT_VECTOR, R_CLM_VECTOR};
static char *type_names[15] = {"unspecified", "int", "float", "bool", "char", "string", "function", "continuation", "vct", "reader", "clm", 
			       "float-vector", "int-vector", "vct-vector", "clm-vector"};
static char* type_name(int id) {if ((id >= R_INT) && (id <= R_CLM_VECTOR)) return(type_names[id]); return("unknown");}

#define POINTER_P(Type) ((Type) > R_GOTO)
#define POINTER_OR_GOTO_P(Type) ((Type) > R_FUNCTION)
#define VECTOR_P(Type) ((Type) > R_CLM)

enum {R_VARIABLE, R_CONSTANT};
#define NEED_ANY_RESULT 1
#define NEED_INT_RESULT 2

static int current_optimization = 0;
static int run_warned = FALSE;

typedef struct {
  void (*function)(int *arg_addrs, int *ints, Float *dbls);
  int *args;
  char *(*descr)(int *arg_addrs, int *ints, Float *dbls); /* for debugging */
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

static xen_value *make_xen_value(int typ, int address, int constant)
{
  xen_value *v;
  v = (xen_value *)CALLOC(1, sizeof(xen_value));
  v->type = typ;
  v->addr = address;
  v->constant = constant;
  v->gc = 0;
  return(v);
}

#if 0
static xen_value *make_xen_value_1(int typ, int address, int constant, const char *func, int line)
{
  xen_value *v;
  char *buf;
  buf = (char *)malloc(64);
  sprintf(buf, "%s: %d", func, line);
  set_encloser(buf);
  v = make_xen_value_2(typ, address, constant);
  set_encloser(NULL);
  return(v);
}
#define make_xen_value(a,b,c) make_xen_value_1(a,b,c,__FUNCTION__,__LINE__)
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
    g_c_run_progn_hook(optimization_hook, 
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
    g_c_run_progn_hook(optimization_hook, 
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
  int *ints; /* making these global throughout speeds up evaluations by about 5% */
  Float *dbls;
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
  int initial_pc;
  int need_init;
  XEN code, form;
  int str_ctr, strs_size;
  int *strs;
  void *outer_tree;
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
  int name_pending = 0;
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
	      name_pending = 0;
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
		  name_pending = 1;
		}
#if DEBUGGING
	      else
		{
		  char *local_name;
		  local_name = find_local_var_via_addr(str[i] == 'd', addr);
		  if (local_name)
		    {
		      strcpy(name, local_name);
		      name_pending = 1;
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
      buf = str_append(buf, &size, mus_format("  %d: %s\n", i, temp));
      FREE(temp);
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

static char *describe_xen_value(xen_value *v, int *ints, Float *dbls)
{
  char *buf = NULL;
  if (v == NULL) return(copy_string("null xen_value"));
  switch (v->type)
    {
    case R_BOOL:    buf = (char *)CALLOC(32, sizeof(char)); mus_snprintf(buf, 32, "i%d(%s)", v->addr, (ints[v->addr] == 0) ? "#f" : "#t"); break;
    case R_INT:     buf = (char *)CALLOC(32, sizeof(char)); mus_snprintf(buf, 32, INT_PT , v->addr, ints[v->addr]);                        break;
    case R_CHAR:    buf = (char *)CALLOC(32, sizeof(char)); mus_snprintf(buf, 32, CHR_PT , v->addr, (char)(ints[v->addr]));                break;
    case R_STRING:  buf = (char *)CALLOC(256, sizeof(char)); mus_snprintf(buf, 256, STR_PT , v->addr, (char *)(ints[v->addr]));            break;
    case R_FLOAT:   buf = (char *)CALLOC(32, sizeof(char)); mus_snprintf(buf, 32, FLT_PT , v->addr, dbls[v->addr]);                        break;
    case R_FLOAT_VECTOR:
    case R_VCT:     buf = vct_to_string((vct *)(ints[v->addr]));                                                                           break;
    case R_READER:  if (ints[v->addr]) buf = sf_to_string((snd_fd *)(ints[v->addr])); else buf = copy_string("null");                      break;
    case R_CLM:     if (ints[v->addr]) buf = copy_string(mus_describe((mus_any *)(ints[v->addr])));  else buf = copy_string("null");       break;
    case R_GOTO:    return(mus_format("continuation: " INT_PT , v->addr, ints[v->addr]));                                                  break;
    case R_FUNCTION: return(describe_ptree((ptree *)(ints[v->addr])));                                                                     break;
    case R_INT_VECTOR: return(mus_format("int vector " PTR_PT , v->addr, (int_vct *)(ints[v->addr])));                                     break;
    case R_VCT_VECTOR: return(mus_format("vct vector " PTR_PT , v->addr, (vct_vct *)(ints[v->addr])));                                     break;
    case R_CLM_VECTOR: return(mus_format("clm vector " PTR_PT , v->addr, (clm_vct *)(ints[v->addr])));                                     break;
    case R_UNSPECIFIED: return(copy_string("#<unspecified>"));                                                                             break;
    default:        buf = (char *)CALLOC(32, sizeof(char)); mus_snprintf(buf, 32, "unknown type: %d", v->type);                            break;
    }
  return(buf);
}

static int got_lambda = 0; /* a temporary kludge?? */

static ptree *make_ptree(int initial_data_size)
{
  ptree *pt;

  got_lambda = 0;

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

  inner->ints = NULL;
  inner->dbls = NULL;
  inner->vars = NULL;
  inner->global_vars = NULL;
  inner->gcs = NULL;
  inner->gotos = NULL;
  inner->strs = NULL;

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
}


static void int_vct_to_vector(int_vct *v, XEN vect)
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
	  if (current_optimization < 5)
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
		  vct_to_vector((vct *)(prog->ints[var->v->addr]), val);
		break;
	      case R_INT_VECTOR:
		val = symbol_to_value(prog->code, C_STRING_TO_XEN_SYMBOL(var->name), &local_var);
		if (XEN_VECTOR_P(val))
		  int_vct_to_vector((int_vct *)(prog->ints[var->v->addr]), val);
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
		  vct_to_vector((vct *)(prog->ints[var->v->addr]), val);
		break;
	      case R_INT_VECTOR:
		val = symbol_to_value(prog->code, C_STRING_TO_XEN_SYMBOL(var->name), &local_var);
		if (XEN_VECTOR_P(val))
		  int_vct_to_vector((int_vct *)(prog->ints[var->v->addr]), val);
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
	      if ((v) && (v->gc) && (pt->ints[v->addr]))
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
		    default: fprintf(stderr," free %s?\n", type_name(v->type));             break;
		    }
		  pt->ints[v->addr] = 0;
		  v->gc = 0;
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

static int add_string_to_ptree(ptree *pt, char *str)
{
  int i, cur, addr;
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
  addr = add_int_to_ptree(pt, (int)(str));
  pt->strs[pt->str_ctr++] = addr;
  return(addr);
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

static void add_obj_to_gcs(ptree *pt, int type, int addr)
{
  xen_value *v;
  int old_size, i;
  v = make_xen_value(type, addr, R_VARIABLE);
  v->gc = 1;
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
      v->data[i] = MUS_XEN_TO_CLM(vdata[i]);
    else
      {
	FREE(v->data);
	FREE(v);
	return(NULL);
      }
  return(v);
}


static xen_value *add_global_var_to_ptree(ptree *prog, XEN form)
{
  XEN val = XEN_UNDEFINED;
  xen_var *var;
  int var_loc = 0, local_var = FALSE;
  xen_value *v = NULL;
  ptree *upper = NULL;
  char varname[256];
  mus_snprintf(varname, 256, "%s", XEN_SYMBOL_TO_C_STRING(form));
  var = find_var_in_ptree(prog, varname);
  if (var) return(copy_xen_value(var->v));
  val = symbol_to_value(prog->code, form, &local_var);
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
  /* fprintf(stderr,"add global %s %d %s\n",varname, sf_p(val), XEN_AS_STRING(val)); */
  if (XEN_NUMBER_P(val))
    {
      if ((XEN_EXACT_P(val)) && (XEN_INTEGER_P(val)))
	v = make_xen_value(R_INT, add_int_to_ptree(prog, XEN_TO_C_INT(val)), R_VARIABLE);
      else v = make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, XEN_TO_C_DOUBLE(val)), R_VARIABLE);
    }
  else
    {
      if (XEN_BOOLEAN_P(val))
	v = make_xen_value(R_BOOL, add_int_to_ptree(prog, XEN_TO_C_BOOLEAN(val)), R_VARIABLE);
      else
	{
	  if (VCT_P(val))
	    v = make_xen_value(R_VCT, add_int_to_ptree(prog, (int)(get_vct(val))), R_VARIABLE);
	  else
	    {
	      if (sf_p(val))
		v = make_xen_value(R_READER, add_int_to_ptree(prog, (int)(get_sf(val))), R_VARIABLE);
	      else
		{
		  if (mus_xen_p(val))
		    v = make_xen_value(R_CLM, add_int_to_ptree(prog, (int)(MUS_XEN_TO_CLM(val))), R_VARIABLE);
		  else
		    {
		      if (XEN_CHAR_P(val))
			v = make_xen_value(R_CHAR, add_int_to_ptree(prog, (int)(XEN_TO_C_CHAR(val))), R_VARIABLE);
		      else
			{
			  if (XEN_STRING_P(val))
			    v = make_xen_value(R_STRING, add_string_to_ptree(prog, copy_string(XEN_TO_C_STRING(val))), R_VARIABLE);
			  else
			    {
			      if (XEN_VECTOR_P(val))
				{
				  XEN val0;
				  val0 = XEN_VECTOR_REF(val, 0);
				  if (XEN_NUMBER_P(val0))
				    {
				      if (XEN_EXACT_P(val0))
					{
					  v = make_xen_value(R_INT_VECTOR, add_int_to_ptree(prog, (int)read_int_vector(val)), R_VARIABLE);
					  if (v) add_obj_to_gcs(prog, R_INT_VECTOR, v->addr); /* this makes its own xen_value with TRUE gc field */
					}
				      else 
					{
					  v = make_xen_value(R_FLOAT_VECTOR, add_int_to_ptree(prog, (int)vector_to_vct(val)), R_VARIABLE);
					  if (v) add_obj_to_gcs(prog, R_FLOAT_VECTOR, v->addr);
					}
				    }
				  else
				    {
				      if (VCT_P(val0))
					{
					  v = make_xen_value(R_VCT_VECTOR, add_int_to_ptree(prog, (int)read_vct_vector(val)), R_VARIABLE);
					  if (v) add_obj_to_gcs(prog, R_VCT_VECTOR, v->addr);
					}
				      else
					{
					  if ((mus_xen_p(val0)) || (XEN_BOOLEAN_P(val0)))
					    {
					      v = make_xen_value(R_CLM_VECTOR, add_int_to_ptree(prog, (int)read_clm_vector(val)), R_VARIABLE);
					      if (v) add_obj_to_gcs(prog, R_CLM_VECTOR, v->addr);
					    }
					}
				    }
				}
			    }
			}
		    }
		}
	    }
	}
    }
  if (v)
    {
      var_loc = add_outer_var_to_ptree(prog, varname, v);
      if (current_optimization < 5)
	prog->global_vars[var_loc]->unsettable = local_var;
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
	  addrs[i] = v->addr;
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
#else
  #define STRING_RESULT ((char *)(ints[args[0]]))
  #define CHAR_RESULT ((char)(ints[args[0]]))
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
  return(mus_format("if (i%d(%s)) jump " INT_PT , args[1], (INT_ARG_1) ? "#t" : "#f", args[0], INT_RESULT));
}

static void jump_if_equal(int *args, int *ints, Float *dbls) {if (ints[args[1]] == ints[args[2]]) PC = ints[args[0]];}
static char *descr_jump_if_equal(int *args, int *ints, Float *dbls) 
{
  return(mus_format("if (" INT_PT " == " INT_PT ") goto " INT_PT , args[1], INT_ARG_1, args[2], INT_ARG_2, args[0], INT_RESULT));
}

static void jump_if_not(int *args, int *ints, Float *dbls) {if (ints[args[1]] == 0) PC += ints[args[0]];}
static char *descr_jump_if_not(int *args, int *ints, Float *dbls) 
{
  return(mus_format("if (!i%d(%s)) jump " INT_PT , args[1], (INT_ARG_1) ? "#t" : "#f", args[0], INT_RESULT));
}

static void store_i(int *args, int *ints, Float *dbls) {INT_RESULT = INT_ARG_1;}
static char *descr_store_i(int *args, int *ints, Float *dbls) {return(mus_format( INT_PT " = " INT_PT , args[0], INT_RESULT, args[1], INT_ARG_1));}

static void store_f(int *args, int *ints, Float *dbls) {FLOAT_RESULT = FLOAT_ARG_1;}
static char *descr_store_f(int *args, int *ints, Float *dbls) {return(mus_format( FLT_PT " = " FLT_PT , args[0], FLOAT_RESULT, args[1], FLOAT_ARG_1));}

static void store_f_i(int *args, int *ints, Float *dbls) {INT_RESULT = (int)FLOAT_ARG_1;}
static char *descr_store_f_i(int *args, int *ints, Float *dbls) {return(mus_format( INT_PT " = " FLT_PT , args[0], INT_RESULT, args[1], FLOAT_ARG_1));}

static void store_i_f(int *args, int *ints, Float *dbls) {FLOAT_RESULT = (Float)INT_ARG_1;}
static char *descr_store_i_f(int *args, int *ints, Float *dbls) {return(mus_format( FLT_PT " = " INT_PT , args[0], FLOAT_RESULT, args[1], INT_ARG_1));}

static void store_false(int *args, int *ints, Float *dbls) {BOOL_RESULT = 0;}
static char *descr_store_false(int *args, int *ints, Float *dbls) {return(mus_format( INT_PT " = 0", args[0], BOOL_RESULT));}

static void store_true(int *args, int *ints, Float *dbls) {BOOL_RESULT = 1;}
static char *descr_store_true(int *args, int *ints, Float *dbls) {return(mus_format( INT_PT " = 1", args[0], BOOL_RESULT));}

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
  if ((exact) && (floor(val) != (int)val)) return(NULL);
  v = make_xen_value(R_INT, add_int_to_ptree(prog, 0), i->constant);
  if (i->constant == R_CONSTANT)
    prog->ints[v->addr] = (int)val;
  else add_triple_to_ptree(prog, va_make_triple(store_f_i, descr_store_f_i, 2, v, i));
  return(v);
}

static void set_var(ptree *pt, xen_value *var, xen_value *init_val)
{
  if (var->type == R_FLOAT)
    add_triple_to_ptree(pt, va_make_triple(store_f, descr_store_f, 2, var, init_val));
  else
    {
      if (var->type == R_STRING)
	add_triple_to_ptree(pt, va_make_triple(store_s, descr_store_s, 2, var, init_val));
      else add_triple_to_ptree(pt, va_make_triple(store_i, descr_store_i, 2, var, init_val));
    }
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

static xen_value *lambda_form(ptree *prog, XEN form, int separate);

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
	  v = lambda_form(prog, form, FALSE);
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

static char *declare_args(ptree *prog, XEN form, int default_arg_type, int separate)
{
  XEN arg, args, declarations, declaration;
  xen_value *v = NULL;
  int i, arg_num, arg_type;
  char *type;
  if (separate)
    args = XEN_CADR(form);
  else args = XEN_CDADR(form);
  if (!(XEN_LIST_P(args))) return(mus_format("can't handle non-explicit lambda args: %s", XEN_AS_STRING(args)));
  declarations = XEN_CADDR(form);
  if ((XEN_LIST_P(declarations)) && 
      (XEN_NOT_NULL_P(declarations)) &&
      (strcmp("declare", XEN_AS_STRING(XEN_CAR(declarations))) == 0))
    declarations = XEN_CDR(declarations);
  else declarations = XEN_FALSE;
  arg_num = XEN_LIST_LENGTH(args);
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
	      if (XEN_EQ_P(XEN_CAR(declaration), arg))
		{
		  type = XEN_AS_STRING(XEN_CADR(declaration));
		  if (strcmp(type, "integer") == 0) arg_type = R_INT; else
		  if (strcmp(type, "string") == 0) arg_type = R_STRING; else
		  if (strcmp(type, "real") == 0) arg_type = R_FLOAT; else
		  if (strcmp(type, "number") == 0) arg_type = R_FLOAT; else
		  if (strcmp(type, "vct") == 0) arg_type = R_VCT; else
		  if (strcmp(type, "clm") == 0) arg_type = R_CLM; else
		  if (strcmp(type, "function") == 0) arg_type = R_FUNCTION; else
		  if (strcmp(type, "reader") == 0) arg_type = R_READER; else
		  if (strcmp(type, "boolean") == 0) arg_type = R_BOOL; else
		  if (strcmp(type, "char") == 0) arg_type = R_CHAR;
		}
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

static xen_value *lambda_form(ptree *prog, XEN form, int separate)
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
      declare_args(new_tree, form, R_INT, separate); /* assuming clm here */
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
      /* we get here if walk failed, so clean up */
      unattach_ptree(new_tree, prog); /* needed in case we realloc'd during failed tree walk */
      free_embedded_ptree(new_tree);
      return(run_warn("can't handle this embedded lambda: %s", XEN_AS_STRING(form)));
    }
  got_lambda = 1;
  locals_loc = prog->var_ctr;
  err = declare_args(prog, form, R_FLOAT, separate);
  if (err) return(run_warn(err));
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
  if (err) return(run_warn(err));
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
	      if (false_result->type == R_BOOL)
		false_result->type = true_result->type;
	      else
		if (true_result->type == R_BOOL)
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
      if (strcmp("else", XEN_AS_STRING(XEN_CAR(clause))) == 0)
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
		  FREE(clause_value);
		  FREE(result);
		  if (jump_to_next_clause) FREE(jump_to_next_clause);
		  for (i = 0; i < clause_ctr; i++) 
		    if (fixups[i]) FREE(fixups[i]);
		  FREE(fixups);
		  return(run_warn("cond clause types differ: %s %s", type_name(clause_value->type), type_name(result->type)));
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
  xen_value *selval, *jump_to_selection, *v, *result = NULL, *keyval, *locval, *elseval = NULL;
  int *locations;
  xen_value **fixups;
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
		FREE(v);
		v = NULL;
	      }
	  if (v) set_var(prog, result, v);
	}
      if (v == NULL)
	{
	  FREE(selval);
	  FREE(jump_to_selection);
	  for (j = 0; j < i; j++) 
	    if (fixups[j])
	      FREE(fixups[j]);
	  FREE(fixups);
	  FREE(locations);
	  return(NULL);
	}
      FREE(v);
      fixups[i] = make_xen_value(R_INT, add_int_to_ptree(prog, 0), R_VARIABLE);
      add_triple_to_ptree(prog, va_make_triple(jump_abs, descr_jump_abs, 1, fixups[i])); 
    }
  /* fixup jump from selector to table of keys (here) */
  prog->ints[jump_to_selection->addr] = prog->triple_ctr;
  FREE(jump_to_selection);
  /* now make the selection */
  body = XEN_CDDR(form);
  for (i = 0; i < body_len; i++, body = XEN_CDR(body))
    {
      keys = XEN_CAAR(body);
      if (XEN_SYMBOL_P(keys))
	{
	  if (strcmp(XEN_AS_STRING(keys), "else") == 0)
	    elseval = make_xen_value(R_INT, i, R_CONSTANT);
	  else return(run_warn("bad case key: %s", XEN_AS_STRING(keys)));
	}
      else
	{
	  num_keys = XEN_LIST_LENGTH(keys);
	  for (j = 0; j < num_keys; j++, keys = XEN_CDR(keys))
	    {
	      key = XEN_CAR(keys);
	      if (!(XEN_INTEGER_P(key)))
		{
		  FREE(selval);
		  for (j = 0; j < i; j++) 
		    if (fixups[j])
		      FREE(fixups[j]);
		  FREE(fixups);
		  FREE(locations);
		  return(run_warn("case only accepts integer selectors: %s", XEN_AS_STRING(key)));
		}
	      cur_key = XEN_TO_C_INT(key);
	      keyval = make_xen_value(R_INT, add_int_to_ptree(prog, cur_key), R_CONSTANT);
	      locval = make_xen_value(R_INT, add_int_to_ptree(prog, locations[i]), R_CONSTANT);
	      add_triple_to_ptree(prog, va_make_triple(jump_if_equal, descr_jump_if_equal, 3, locval, selval, keyval));
	      FREE(keyval);
	      FREE(locval);
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
  for (i = 0; i < body_len; i++)
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


static xen_value *do_form_1(ptree *prog, XEN form, int need_result, int sequential)
{
  /* (do ([(var val [up])...]) (test [res ...]) [exp ...]): (do () (#t))  */

  xen_value *result = NULL, *test, *expr, *jump_to_result, *jump_to_test;
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
  /* now increment the vars (if step-val exists) */
  varlen = XEN_LIST_LENGTH(vars);
  if (varlen > 0)
    {
      for (i = 0; i < varlen; i++, vars = XEN_CDR(vars))
	{
	  var = XEN_CAR(vars);
	  if ((XEN_NOT_NULL_P(XEN_CDDR(var))) && (XEN_NOT_NULL_P(XEN_CADDR(var))))
	    {
	      if (expr) FREE(expr);
	      expr = walk(prog, XEN_CADDR(var), TRUE);
	      if (expr == NULL)
		{
		  if (expr) FREE(expr);
		  FREE(jump_to_result);
		  return(NULL);
		}
	      vr = find_var_in_ptree(prog, XEN_SYMBOL_TO_C_STRING(XEN_CAR(var)));
	      set_var(prog, vr->v, expr);
	    }
	}
    }
  if (expr) FREE(expr);
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

static xen_value *do_form(ptree *prog, XEN form, int need_result)
{
  return(do_form_1(prog, form, need_result, FALSE));
}

static xen_value *do_star_form(ptree *prog, XEN form, int need_result)
{
  return(do_form_1(prog, form, need_result, TRUE));
}

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

static xen_value *or_form(ptree *prog, XEN form)
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

static xen_value *and_form(ptree *prog, XEN form)
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

static xen_value *lookup_generalized_set(ptree *prog, char *accessor, xen_value *accessor_arg, xen_value *new_value);

static xen_value *generalized_set_form(ptree *prog, XEN form, int need_result)
{
  /* (set! (mus-phase gen) 0.0) */
  XEN settee, setval;
  settee = XEN_CADR(form);
  setval = XEN_CADDR(form);
  if ((XEN_LIST_P(settee)) && (XEN_SYMBOL_P(XEN_CAR(settee))) && (XEN_LIST_LENGTH(settee) <= 2))
    {
      XEN in_settee;
      xen_value *in_v = NULL, *v = NULL;
      char accessor[256];
      in_settee = XEN_CAR(settee);
      mus_snprintf(accessor, 256, "%s", XEN_SYMBOL_TO_C_STRING(in_settee));
      v = walk(prog, setval, TRUE);
      if (v == NULL) 
	return(run_warn("set!: can't handle: %s", XEN_AS_STRING(setval)));
      if (XEN_NOT_NULL_P(XEN_CDR(settee)))
	{
	  in_v = walk(prog, XEN_CADR(settee), TRUE);
	  if (in_v == NULL)
	    {
	      FREE(v);
	      return(run_warn("set!: can't handle: %s", XEN_AS_STRING(XEN_CADR(settee))));
	    }
	}
      return(lookup_generalized_set(prog, copy_string(accessor), in_v, v));
    }
  return(run_warn("generalized set! for %s not implemented yet", XEN_AS_STRING(settee)));
}

static xen_value *set_form(ptree *prog, XEN form, int need_result)
{
  char varname[256];
  xen_var *var;
  xen_value *v;
  XEN settee, setval;
  settee = XEN_CADR(form);
  setval = XEN_CADDR(form);
  if (!(XEN_SYMBOL_P(settee)))
    return(generalized_set_form(prog, form, need_result));
  mus_snprintf(varname, 256, "%s", XEN_SYMBOL_TO_C_STRING(settee));
  var = find_var_in_ptree(prog, varname);
  if (var == NULL)
    {
      v = add_global_var_to_ptree(prog, settee);
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
	  (XEN_LIST_P(setval)) && 
	  (prog->triple_ctr > 0))
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
    return(run_warn("set!: can't set local vars: %s", XEN_AS_STRING(settee)));
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

static xen_value *multiply(ptree *prog, int float_result, xen_value **args, int num_args, int constants)
{
  int iscl = 1, cons_loc = 0;
  Float fscl = 1.0;
  int i;
  if (num_args == 0) return(make_xen_value(R_INT, add_int_to_ptree(prog, 1), R_CONSTANT));
  if (num_args == 1) return(copy_xen_value(args[1]));
  if (constants > 0)
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
	  if (float_result)
	    args[cons_loc] = make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, fscl * iscl), R_CONSTANT);
	  else args[cons_loc] = make_xen_value(R_INT, add_int_to_ptree(prog, iscl), R_CONSTANT);
	  if ((iscl == 0) || (fscl == 0.0))
	    return(copy_xen_value(args[cons_loc]));
	}
      if (constants == num_args) 
	{
	  if (args[cons_loc])
	    return(copy_xen_value(args[cons_loc]));
	  else return(make_xen_value(R_INT, add_int_to_ptree(prog, 1), R_CONSTANT));
	}
    }
  num_args = float_all_args(prog, num_args, args, float_result);
  if (num_args == 1) return(copy_xen_value(args[1]));
  if (float_result)
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

static xen_value *add(ptree *prog, int float_result, xen_value **args, int num_args, int constants)
{
  int iscl = 0, cons_loc = 0;
  Float fscl = 0.0;
  int i;
  if (num_args == 0) return(make_xen_value(R_INT, add_int_to_ptree(prog, 0), R_CONSTANT));
  if (num_args == 1) return(copy_xen_value(args[1]));
  if (constants > 0)
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
	  if (float_result)
	    args[cons_loc] = make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, fscl + iscl), R_CONSTANT);
	  else args[cons_loc] = make_xen_value(R_INT, add_int_to_ptree(prog, iscl), R_CONSTANT);
	}
      if (constants == num_args) 
	{
	  if (args[cons_loc])
	    return(copy_xen_value(args[cons_loc]));
	  else return(make_xen_value(R_INT, add_int_to_ptree(prog, 0), R_CONSTANT));
	}
    }
  num_args = float_all_args(prog, num_args, args, float_result);
  if (num_args == 1) return(copy_xen_value(args[1]));
  if (float_result)
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

static xen_value *subtract(ptree *prog, int float_result, xen_value **args, int num_args, int constants)
{
  int iscl = 0, cons_loc = 0;
  Float fscl = 0.0;
  int i;
  if (num_args == 0) return(run_warn("- with no arg?"));
  if ((num_args == 1) && (args[1]->constant == R_CONSTANT))
    {
      if (args[1]->type == R_INT)
	return(make_xen_value(R_INT, add_int_to_ptree(prog, -(prog->ints[args[1]->addr])), R_CONSTANT));
      else return(make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, -(prog->dbls[args[1]->addr])), R_CONSTANT));
    }
  if (constants > 0)
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
	  if (float_result)
	    args[cons_loc] = make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, fscl + iscl), R_CONSTANT);
	  else args[cons_loc] = make_xen_value(R_INT, add_int_to_ptree(prog, iscl), R_CONSTANT);
	}
      else
	{
	  if ((num_args == 2) && (args[1]->constant == R_VARIABLE))
	    return(copy_xen_value(args[1]));
	}
      if (constants == num_args) 
	{
	  if (float_result)
	    {
	      if (args[1]->type == R_INT)
		return(make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, prog->ints[args[1]->addr] - (fscl + iscl)), R_CONSTANT));
	      else return(make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, prog->dbls[args[1]->addr] - (fscl + iscl)), R_CONSTANT));
	    }
	  else return(make_xen_value(R_INT, add_int_to_ptree(prog, prog->ints[args[1]->addr] - iscl), R_CONSTANT));
	}
    }
  num_args = float_all_args(prog, num_args, args, float_result);
  if (float_result)
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
static xen_value *one_minus(ptree *prog, xen_value **args, int constants)
{
  if (constants == 1)
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
static xen_value *one_plus(ptree *prog, xen_value **args, int constants)
{
  if (constants == 1)
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
static char *descr_divide_f1(int *args, int *ints, Float *dbls) {return(mus_format( FLT_PT " = 1.0 / " FLT_PT , args[0], FLOAT_RESULT, args[1], FLOAT_ARG_1));}

static void divide_f2(int *args, int *ints, Float *dbls) {FLOAT_RESULT = (FLOAT_ARG_1 / FLOAT_ARG_2);}
static char *descr_divide_f2(int *args, int *ints, Float *dbls) {return(describe_dbl_args("/", 2, args, dbls, 1));}

static void divide_f3(int *args, int *ints, Float *dbls) {FLOAT_RESULT = (FLOAT_ARG_1 / (FLOAT_ARG_2 * FLOAT_ARG_3));}
static char *descr_divide_f3(int *args, int *ints, Float *dbls) 
{
  return(mus_format( FLT_PT " = " FLT_PT " / (" FLT_PT " * " FLT_PT ")", args[0], FLOAT_RESULT, args[1], FLOAT_ARG_1, args[2], FLOAT_ARG_2, args[3], FLOAT_ARG_3));
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

static xen_value *divide(ptree *prog, xen_value **args, int num_args, int constants)
{
  int cons_loc = 0;
  Float fscl = 1.0;
  int i;
  if (num_args == 0) return(run_warn("/ with no arg?"));
  if ((num_args == 1) && (args[1]->constant == R_CONSTANT))
    {
      if (args[1]->type == R_INT)
	return(make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, (1.0 / (Float)(prog->ints[args[1]->addr]))), R_CONSTANT));
      else return(make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, (1.0 / (prog->dbls[args[1]->addr]))), R_CONSTANT));
    }
  if (constants > 0)
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
      if (constants == num_args) 
	{
	  if (args[1]->type == R_INT)
	    return(make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, (Float)(prog->ints[args[1]->addr]) / fscl), R_CONSTANT));
	  else return(make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, prog->dbls[args[1]->addr] / fscl), R_CONSTANT));
	}
    }
  num_args = float_all_args(prog, num_args, args, TRUE);
  if (num_args == 1) return(package(prog, R_FLOAT, divide_f1, descr_divide_f1, args, num_args));
  if (num_args == 2) return(package(prog, R_FLOAT, divide_f2, descr_divide_f2, args, num_args));
  if (num_args == 3) return(package(prog, R_FLOAT, divide_f3, descr_divide_f3, args, num_args));
  return(package_n(prog, R_FLOAT, divide_fn, descr_divide_fn, args, num_args));
}


static char *describe_rel_f_args(char *func, int num_args, int *args, int *ints, Float *dbls, int start)
{
  char *buf, *str;
  int i;
  buf = (char *)CALLOC(256, sizeof(char));
  str = (char *)CALLOC(32, sizeof(char));
  sprintf(buf, INT_PT " =", args[0], INT_RESULT);
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
static xen_value * SName(ptree *prog, int float_result, xen_value **args, int num_args, int constants) \
{ \
  int i, lasti = 0; \
  Float lastf = 0.0; \
  if (num_args <= 1) return(make_xen_value(R_BOOL, add_int_to_ptree(prog, TRUE), R_CONSTANT)); \
  if ((constants > 0) && (float_result)) float_rel_constant_args(prog, num_args, args); \
  if (constants > 1) \
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
      if (constants == num_args) \
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
  return(mus_format( FLT_PT " = max(" FLT_PT ", " FLT_PT ")",
		    args[0], FLOAT_RESULT, args[1], FLOAT_ARG_1, args[2], FLOAT_ARG_2));
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
  return(mus_format( INT_PT " = max(" INT_PT ", " INT_PT ")",
		    args[0], INT_RESULT, args[1], INT_ARG_1, args[2], INT_ARG_2));
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
static xen_value *max_1(ptree *prog, int float_result, xen_value **args, int num_args, int constants)
{
  int i;
  Float fmx;
  int imx;
  if (num_args == 1) return(copy_xen_value(args[1]));
  if ((constants > 0) && (float_result)) float_rel_constant_args(prog, num_args, args);
  if (constants == num_args)
    {
      if (float_result)
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
  if (float_result)
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
  return(mus_format( FLT_PT " = min(" FLT_PT ", " FLT_PT ")",
		    args[0], FLOAT_RESULT, args[1], FLOAT_ARG_1, args[2], FLOAT_ARG_2));
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
  return(mus_format( INT_PT " = min(" INT_PT ", " INT_PT ")",
		    args[0], INT_RESULT, args[1], INT_ARG_1, args[2], INT_ARG_2));
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
static xen_value *min_1(ptree *prog, int float_result, xen_value **args, int num_args, int constants)
{
  int i;
  Float fmx;
  int imx;
  if (num_args == 1) return(copy_xen_value(args[1]));
  if ((constants > 0) && (float_result)) float_rel_constant_args(prog, num_args, args);
  if (constants == num_args)
    {
      if (float_result)
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
  if (float_result)
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
static char *descr_not_b(int *args, int *ints, Float *dbls) {return(mus_format( INT_PT " = (!(" INT_PT "))", args[0], BOOL_RESULT, args[1], INT_ARG_1));}
static xen_value *not_p(ptree *prog, xen_value **args, int constants)
{
  if (args[1]->type != R_BOOL)
    return(make_xen_value(R_BOOL, add_int_to_ptree(prog, FALSE), R_CONSTANT)); /* only #f is false so (not anything)->false */
  if (constants == 1)
    return(make_xen_value(R_BOOL, add_int_to_ptree(prog, !(prog->ints[args[1]->addr])), R_CONSTANT));
  return(package(prog, R_BOOL, not_b, descr_not_b, args, 1));
}


/* ---------------- eq?, eqv?, equal? ---------------- */

static void eq_b(int *args, int *ints, Float *dbls) {BOOL_RESULT = (INT_ARG_1 == INT_ARG_2);}
static char *descr_eq_b(int *args, int *ints, Float *dbls) 
{
  return(mus_format( INT_PT " = eq?(" INT_PT ", " INT_PT ")", args[0], BOOL_RESULT, args[1], INT_ARG_1, args[2], INT_ARG_2));
}
static xen_value *eq_p(ptree *prog, xen_value **args, int constants)
{
  if ((args[1]->type != args[2]->type) || (args[1]->type == R_FLOAT))
    return(make_xen_value(R_BOOL, add_int_to_ptree(prog, FALSE), R_CONSTANT));
  if (constants == 2)
    return(make_xen_value(R_BOOL, add_int_to_ptree(prog, prog->ints[args[1]->addr] == prog->ints[args[2]->addr]), R_CONSTANT));
  return(package(prog, R_BOOL, eq_b, descr_eq_b, args, 2));
}

static void eqv_fb(int *args, int *ints, Float *dbls) {BOOL_RESULT = (FLOAT_ARG_1 == FLOAT_ARG_2);}
static char *descr_eqv_fb(int *args, int *ints, Float *dbls) 
{
  return(mus_format( INT_PT " = eqv?(" FLT_PT ", " FLT_PT ")", args[0], BOOL_RESULT, args[1], FLOAT_ARG_1, args[2], FLOAT_ARG_2));
}

static void eq_clm(int *args, int *ints, Float *dbls) {BOOL_RESULT = mus_equalp((mus_any *)(INT_ARG_1), (mus_any *)(INT_ARG_2));}
static char *descr_eq_clm(int *args, int *ints, Float *dbls) 
{
  return(mus_format( INT_PT " = equal?(" PTR_PT ", " PTR_PT ")", 
		    args[0], BOOL_RESULT, args[1], (mus_any *)(INT_ARG_1), args[2], (mus_any *)(INT_ARG_2)));
}

static xen_value *eqv_p(ptree *prog, xen_value **args, int constants)
{
  if (args[1]->type != args[2]->type)
    return(make_xen_value(R_BOOL, add_int_to_ptree(prog, FALSE), R_CONSTANT));
  if (constants == 2)
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

static xen_value *equal_p(ptree *prog, xen_value **args, int constants)
{
  return(eqv_p(prog, args, constants));
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
static char *descr_odd_i(int *args, int *ints, Float *dbls) {return(mus_format( INT_PT " = odd?(" INT_PT ")", args[0], BOOL_RESULT, args[1], INT_ARG_1));}
static xen_value *odd_p(ptree *prog, xen_value **args, int constants)
{
  args[1] = convert_to_int(prog, args[1]);
  if (args[1] == NULL) return(run_warn("odd? can't convert arg"));
  if (constants == 1)
    return(make_xen_value(R_BOOL, add_int_to_ptree(prog, prog->ints[args[1]->addr] & 1), R_CONSTANT));
  return(package(prog, R_BOOL, odd_i, descr_odd_i, args, 1));
}

static void even_i(int *args, int *ints, Float *dbls) {BOOL_RESULT = (!(INT_ARG_1 & 1));}
static char *descr_even_i(int *args, int *ints, Float *dbls) {return(mus_format( INT_PT " = even?(" INT_PT ")", args[0], BOOL_RESULT, args[1], INT_ARG_1));}
static xen_value *even_p(ptree *prog, xen_value **args, int constants)
{
  args[1] = convert_to_int(prog, args[1]);
  if (args[1] == NULL) return(run_warn("even? can't convert arg"));
  if (constants == 1)
    return(make_xen_value(R_BOOL, add_int_to_ptree(prog, (!(prog->ints[args[1]->addr] & 1))), R_CONSTANT));
  return(package(prog, R_BOOL, even_i, descr_even_i, args, 1));
}

#define INT_POS_P(CName, SName, COp) \
static void CName ## _i(int *args, int *ints, Float *dbls) {BOOL_RESULT = (INT_ARG_1 COp 0);} \
static char *descr_ ## CName ## _i(int *args, int *ints, Float *dbls) {return(mus_format( INT_PT " = (" INT_PT #COp " 0)", args[0], BOOL_RESULT, args[1], INT_ARG_1));} \
static void CName ## _f(int *args, int *ints, Float *dbls) {BOOL_RESULT = (FLOAT_ARG_1 COp 0.0);} \
static char *descr_ ## CName ## _f(int *args, int *ints, Float *dbls) {return(mus_format( INT_PT " = (" FLT_PT " " #COp " 0.0)", args[0], BOOL_RESULT, args[1], FLOAT_ARG_1));} \
static xen_value *CName ## _p(ptree *prog, xen_value **args, int constants) \
{ \
  if (constants == 1) \
    { \
      if (args[1]->type == R_INT) \
	return(make_xen_value(R_BOOL, add_int_to_ptree(prog, (prog->ints[args[1]->addr] COp 0)), R_CONSTANT)); \
      else \
      { \
	if (args[1]->type == R_FLOAT) \
	  return(make_xen_value(R_BOOL, add_int_to_ptree(prog, (prog->dbls[args[1]->addr] COp 0.0)), R_CONSTANT)); \
	else return(run_warn(#SName ": bad arg")); \
      } \
    } \
  if ((args[1]->type != R_INT) && (args[1]->type != R_FLOAT)) \
    return(run_warn(#SName ": wrong type arg")); \
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
static xen_value * CName ## _1(ptree *prog, xen_value **args, int constants) \
{ \
  if (constants == 1) \
    { \
      if (args[1]->type == R_INT) \
	return(make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, CName((Float)(prog->ints[args[1]->addr]))), R_CONSTANT)); \
      else return(make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, CName(prog->dbls[args[1]->addr])), R_CONSTANT)); \
    } \
  if (args[1]->type == R_INT) single_to_float(prog, args, 1); \
  return(package(prog, R_FLOAT, CName ## _f, descr_ ##CName ## _f, args, 1)); \
}

FL_OP(sin)
FL_OP(cos)
FL_OP(tan)
FL_OP(asin)
FL_OP(acos)
FL_OP(sqrt)
FL_OP(log)
FL_OP(exp)

FL_OP(mus_radians2hz)
FL_OP(mus_hz2radians)
FL_OP(mus_degrees2radians)
FL_OP(mus_radians2degrees)
FL_OP(mus_db2linear)
FL_OP(mus_linear2db)
FL_OP(mus_random)

static void atan1_f(int *args, int *ints, Float *dbls) {FLOAT_RESULT = atan(FLOAT_ARG_1);}
static char *descr_atan1_f(int *args, int *ints, Float *dbls) {return(mus_format( FLT_PT " = atan(" FLT_PT ")", args[0], FLOAT_RESULT, args[1], FLOAT_ARG_1));}
static xen_value *atan1_1(ptree *prog, xen_value **args, int constants)
{
  if (constants == 1)
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
static xen_value *atan2_1(ptree *prog, xen_value **args, int constants)
{
  xen_value *temp;
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
  if (constants == 2)
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
static xen_value *round_1(ptree *prog, xen_value **args, int constants, int arg_result)
{
  /* (round 1) -> 1.0! */
  if (constants == 1)
    {
      if (args[1]->type == R_INT) 
	return(make_xen_value(R_INT, add_int_to_ptree(prog, prog->ints[args[1]->addr]), R_CONSTANT)); /* r5rs spec says return int here */
      else 
	{
	  if (arg_result == NEED_INT_RESULT)
	    return(make_xen_value(R_INT, add_int_to_ptree(prog, (int)f_round(prog->dbls[args[1]->addr])), R_CONSTANT));
	  return(make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, f_round(prog->dbls[args[1]->addr])), R_CONSTANT));
	}
    }
  if (arg_result == NEED_INT_RESULT)
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
static xen_value *truncate_1(ptree *prog, xen_value **args, int constants, int arg_result)
{
  if (constants == 1)
    {
      if (args[1]->type == R_INT) 
	return(make_xen_value(R_INT, add_int_to_ptree(prog, prog->ints[args[1]->addr]), R_CONSTANT));
      else 
	{
	  if (arg_result == NEED_INT_RESULT)
	    return(make_xen_value(R_INT, add_int_to_ptree(prog, (int)f_truncate(prog->dbls[args[1]->addr])), R_CONSTANT));
	  return(make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, f_truncate(prog->dbls[args[1]->addr])), R_CONSTANT));
	}
    }
  if (arg_result == NEED_INT_RESULT)
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
static xen_value *floor_1(ptree *prog, xen_value **args, int constants, int arg_result)
{
  if (constants == 1)
    {
      if (args[1]->type == R_INT) 
	return(make_xen_value(R_INT, add_int_to_ptree(prog, prog->ints[args[1]->addr]), R_CONSTANT));
      else 
	{
	  if (arg_result == NEED_INT_RESULT)
	    return(make_xen_value(R_INT, add_int_to_ptree(prog, (int)floor(prog->dbls[args[1]->addr])), R_CONSTANT));
	  return(make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, floor(prog->dbls[args[1]->addr])), R_CONSTANT));
	}
    }
  if (arg_result == NEED_INT_RESULT)
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
static xen_value *ceiling_1(ptree *prog, xen_value **args, int constants, int arg_result)
{
  if (constants == 1)
    {
      if (args[1]->type == R_INT) 
	return(make_xen_value(R_INT, add_int_to_ptree(prog, prog->ints[args[1]->addr]), R_CONSTANT));
      else 
	{
	  if (arg_result == NEED_INT_RESULT)
	    return(make_xen_value(R_INT, add_int_to_ptree(prog, (int)ceil(prog->dbls[args[1]->addr])), R_CONSTANT));
	  return(make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, ceil(prog->dbls[args[1]->addr])), R_CONSTANT));
	}
    }
  if (arg_result == NEED_INT_RESULT)
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

static xen_value *exact2inexact_1(ptree *prog, xen_value **args, int constants)
{
  if (args[1]->type == R_FLOAT)
    return(copy_xen_value(args[1]));
  if (constants == 1)
    return(make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, (Float)(prog->ints[args[1]->addr])), R_CONSTANT));
  return(package(prog, R_FLOAT, store_i_f, descr_store_i_f, args,1));
}

static void i2e_f(int *args, int *ints, Float *dbls) {INT_RESULT = (int)floor(FLOAT_ARG_1 + 0.5);}
static char *descr_i2e_f(int *args, int *ints, Float *dbls) {return(mus_format( INT_PT " = floor(" FLT_PT " + 0.5)", args[0], INT_RESULT, args[1], FLOAT_ARG_1));}
static xen_value *inexact2exact_1(ptree *prog, xen_value **args, int constants)
{
  if (args[1]->type == R_INT)
    return(copy_xen_value(args[1]));
  if (constants == 1)
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
static xen_value *gcd_1(ptree *prog, xen_value **args, int constants, int num_args)
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
  if (constants == num_args)
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

static xen_value *lcm_1(ptree *prog, xen_value **args, int constants, int num_args)
{
  int i;
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
  if (constants == num_args)
    {
      int i, mx;
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
static xen_value *modulo_1(ptree *prog, xen_value **args, int constants)
{
  args[1] = convert_to_int(prog, args[1]);
  if (args[1] == NULL) return(run_warn("modulo arg1 can't convert to int"));
  args[2] = convert_to_int(prog, args[2]);
  if (args[2] == NULL) return(run_warn("modulo arg2 can't convert to int"));
  if (constants == 2)
    return(make_xen_value(R_INT, add_int_to_ptree(prog, c_mod(prog->ints[args[1]->addr], prog->ints[args[2]->addr])), R_CONSTANT));
  return(package(prog, R_INT, modulo_i, descr_modulo_i, args, 2));
}

static void remainder_i(int *args, int *ints, Float *dbls) {INT_RESULT = (INT_ARG_1 % INT_ARG_2);}
static char *descr_remainder_i(int *args, int *ints, Float *dbls) 
{
  return(mus_format( INT_PT " = (" INT_PT " %% " INT_PT ")", args[0], INT_RESULT, args[1], INT_ARG_1, args[2], INT_ARG_2));
}
static xen_value *remainder_1(ptree *prog, xen_value **args, int constants)
{
  args[1] = convert_to_int(prog, args[1]);
  if (args[1] == NULL) return(run_warn("remainder arg1 can't convert to int"));
  args[2] = convert_to_int(prog, args[2]);
  if (args[2] == NULL) return(run_warn("remainder arg2 can't convert to int"));
  if (constants == 2)
    return(make_xen_value(R_INT, add_int_to_ptree(prog, (prog->ints[args[1]->addr] % prog->ints[args[2]->addr])), R_CONSTANT));
  return(package(prog, R_INT, remainder_i, descr_remainder_i, args, 2));
}

static void quotient_i(int *args, int *ints, Float *dbls) {INT_RESULT = (INT_ARG_1 / INT_ARG_2);}
static char *descr_quotient_i(int *args, int *ints, Float *dbls) 
{
  return(mus_format( INT_PT " = (" INT_PT " / " INT_PT ")", args[0], INT_RESULT, args[1], INT_ARG_1, args[2], INT_ARG_2));
}
static xen_value *quotient_1(ptree *prog, xen_value **args, int constants)
{
  args[1] = convert_to_int(prog, args[1]);
  if (args[1] == NULL) return(run_warn("quotient arg1 can't convert to int"));
  args[2] = convert_to_int(prog, args[2]);
  if (args[2] == NULL) return(run_warn("quotient arg2 can't convert to int"));
  if (constants == 2)
    return(make_xen_value(R_INT, add_int_to_ptree(prog, (prog->ints[args[1]->addr] / prog->ints[args[2]->addr])), R_CONSTANT));
  return(package(prog, R_INT, quotient_i, descr_quotient_i, args, 2));
}



/* ---------------- logand, logior, logxor, lognot, ash ---------------- */

static void logand_i(int *args, int *ints, Float *dbls) {INT_RESULT = (INT_ARG_1 & INT_ARG_2);}
static char *descr_logand_i(int *args, int *ints, Float *dbls) 
{
  return(mus_format( INT_PT " = (" INT_PT " & " INT_PT ")", args[0], INT_RESULT, args[1], INT_ARG_1, args[2], INT_ARG_2));
}
static xen_value *logand_1(ptree *prog, xen_value **args, int constants)
{
  if ((args[1]->type != R_INT) || (args[2]->type != R_INT)) 
    return(run_warn("logand non-int arg"));
  if (constants == 2)
    return(make_xen_value(R_INT, add_int_to_ptree(prog, (prog->ints[args[1]->addr] & prog->ints[args[2]->addr])), R_CONSTANT));
  return(package(prog, R_INT, logand_i, descr_logand_i, args, 2));
}

static void logior_i(int *args, int *ints, Float *dbls) {INT_RESULT = (INT_ARG_1 | INT_ARG_2);}
static char *descr_logior_i(int *args, int *ints, Float *dbls) 
{
  return(mus_format( INT_PT " = (" INT_PT " | " INT_PT ")", args[0], INT_RESULT, args[1], INT_ARG_1, args[2], INT_ARG_2));
}
static xen_value *logior_1(ptree *prog, xen_value **args, int constants)
{
  if ((args[1]->type != R_INT) || (args[2]->type != R_INT)) 
    return(run_warn("logior non-int arg"));
  if (constants == 2)
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
static xen_value *logxor_1(ptree *prog, xen_value **args, int constants)
{
  if ((args[1]->type != R_INT) || (args[2]->type != R_INT)) 
    return(run_warn("logxor non-int arg"));
  if (constants == 2)
    return(make_xen_value(R_INT, add_int_to_ptree(prog, XOR(prog->ints[args[1]->addr], prog->ints[args[2]->addr])), R_CONSTANT));
  return(package(prog, R_INT, logxor_i, descr_logxor_i, args, 2));
}

static void lognot_i(int *args, int *ints, Float *dbls) {INT_RESULT = ~INT_ARG_1;}
static char *descr_lognot_i(int *args, int *ints, Float *dbls) {return(mus_format( INT_PT " = ~" INT_PT , args[0], INT_RESULT, args[1], INT_ARG_1));}
static xen_value *lognot_1(ptree *prog, xen_value **args, int constants)
{
  if (args[1]->type != R_INT) 
    return(run_warn("lognot non-int arg"));
  if (constants == 1)
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
static xen_value *ash_1(ptree *prog, xen_value **args, int constants)
{
  if ((args[1]->type != R_INT) || (args[2]->type != R_INT)) 
    return(run_warn("ash non-int arg"));
  if (constants == 2)
    return(make_xen_value(R_INT, add_int_to_ptree(prog, c_ash(prog->ints[args[1]->addr], prog->ints[args[2]->addr])), R_CONSTANT));
  return(package(prog, R_INT, ash_i, descr_ash_i, args, 2));
}


/* ---------------- expt ---------------- */

static void expt_f(int *args, int *ints, Float *dbls) {FLOAT_RESULT = pow(FLOAT_ARG_1, FLOAT_ARG_2);}
static char *descr_expt_f(int *args, int *ints, Float *dbls) 
{
  return(mus_format( FLT_PT " = pow(" FLT_PT ", " FLT_PT ")", args[0], FLOAT_RESULT, args[1], FLOAT_ARG_1, args[2], FLOAT_ARG_2));
}
static xen_value *expt_1(ptree *prog, xen_value **args, int constants)
{
  Float f1, f2;
  if (constants == 2)
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
static xen_value *abs_1(ptree *prog, xen_value **args, int constants)
{
  if (constants == 1)
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

static void random_f(int *args, int *ints, Float *dbls) {FLOAT_RESULT = FLOAT_ARG_1 * scm_c_uniform01(scm_c_default_rstate());}
static char *descr_random_f(int *args, int *ints, Float *dbls) 
{
  return(mus_format( FLT_PT " = random(" FLT_PT ")", args[0], FLOAT_RESULT, args[1], FLOAT_ARG_1));
}
static void random_i(int *args, int *ints, Float *dbls) {INT_RESULT = scm_c_random(scm_c_default_rstate(), INT_ARG_1);}
static char *descr_random_i(int *args, int *ints, Float *dbls) 
{
  return(mus_format( INT_PT " = random(" INT_PT ")", args[0], INT_RESULT, args[1], INT_ARG_1));
}
static xen_value *random_1(ptree *prog, xen_value **args)
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
  return(mus_format( INT_PT " = " #SName "(" CHR_PT ")", args[0], BOOL_RESULT, args[1], (char)(INT_ARG_1))); \
} \
static xen_value * SName(ptree *pt, xen_value **args, int constants) \
{ \
  if (constants == 1) \
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
static xen_value * SName(ptree *pt, xen_value **args, int constants) \
{ \
  if (constants == 1) \
    return(make_xen_value(R_CHAR, add_int_to_ptree(pt, CName((char)(pt->ints[args[1]->addr]))), R_CONSTANT)); \
  return(package(pt, R_CHAR, char_## CName ## _c, descr_ ## CName ## _c, args, 1)); \
}

CHARC(char_upcase, toupper)
CHARC(char_downcase, tolower)

static xen_value *char_to_integer(xen_value *v)
{
  xen_value *newv;
  newv = copy_xen_value(v);
  newv->type = R_INT;
  return(newv);
}

static xen_value *integer_to_char(xen_value *v)
{
  xen_value *newv;
  newv = copy_xen_value(v);
  newv->type = R_CHAR;
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
static xen_value *string_1(ptree *pt, xen_value **args, int num_args, int constants)
{
  if (constants == num_args)
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
static xen_value *string_length_1(ptree *pt, xen_value **args)
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
static xen_value *string_copy_1(ptree *pt, xen_value **args)
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
static xen_value *string_fill_1(ptree *pt, xen_value **args)
{
  if ((args[1]->constant == R_CONSTANT) || (args[1]->type != R_STRING) || (args[2]->type != R_CHAR))
    return(run_warn("bad args to string-fill!"));
  add_triple_to_ptree(pt, va_make_triple(strfill_1, descr_strfill_1, 2, args[1], args[2])); /* shifts back one */
  return(make_xen_value(R_UNSPECIFIED, -1, R_CONSTANT));
}

static void strset_1(int *args, int *ints, Float *dbls) {STRING_RESULT[INT_ARG_1] = (char)(CHAR_ARG_2);}
static char *descr_strset_1(int *args, int *ints, Float *dbls) 
{
  return(mus_format( STR_PT " = string-set!(" INT_PT ", " CHR_PT ")", 
		    args[0], STRING_RESULT, args[1], INT_ARG_1, args[2], CHAR_ARG_2));
}
static xen_value *string_set_1(ptree *pt, xen_value **args)
{
  if ((args[1]->constant == R_CONSTANT) || (args[1]->type != R_STRING) || (args[2]->type != R_INT) || (args[3]->type != R_CHAR))
    return(run_warn("bad args to string-set!"));
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
static xen_value *string_ref_1(ptree *pt, xen_value **args)
{
  if ((args[1]->type != R_STRING) || (args[2]->type != R_INT))
    return(run_warn("bad args to string-ref"));
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

static void display_str(int *args, int *ints, Float *dbls) {fprintf(stderr, "\"%s\"", STRING_ARG_1);}
static char *descr_display_str(int *args, int *ints, Float *dbls) {return(mus_format("display(" STR_PT ")", args[1], STRING_ARG_1));}
static void display_int(int *args, int *ints, Float *dbls) {fprintf(stderr, "%d", INT_ARG_1);}
static char *descr_display_int(int *args, int *ints, Float *dbls) {return(mus_format("display(" INT_PT ")", args[1], INT_ARG_1));}
static void display_flt(int *args, int *ints, Float *dbls) {fprintf(stderr, "%.6f", FLOAT_ARG_1);}
static char *descr_display_flt(int *args, int *ints, Float *dbls) {return(mus_format("display(" FLT_PT ")", args[1], FLOAT_ARG_1));}
static void display_clm(int *args, int *ints, Float *dbls) {fprintf(stderr, "%s", mus_describe((mus_any *)(INT_ARG_1)));}
static char *descr_display_clm(int *args, int *ints, Float *dbls) {return(mus_format("display(" PTR_PT ")", args[1], ((mus_any *)(INT_ARG_1))));}
static void display_vct(int *args, int *ints, Float *dbls) {fprintf(stderr, "%s", vct_to_string((vct *)(INT_ARG_1)));}
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
static void display_rd(int *args, int *ints, Float *dbls) {fprintf(stderr, "%s", sf_to_string((snd_fd *)(INT_ARG_1)));}
static char *descr_display_rd(int *args, int *ints, Float *dbls) {return(mus_format("display(" PTR_PT ")", args[1], ((snd_fd *)(INT_ARG_1))));}
static void display_chr(int *args, int *ints, Float *dbls) {fprintf(stderr, "#\\%c", (char)(INT_ARG_1));}
static char *descr_display_chr(int *args, int *ints, Float *dbls) {return(mus_format("display(" CHR_PT ")", args[1], (char)(INT_ARG_1)));}
static void display_bool(int *args, int *ints, Float *dbls) {fprintf(stderr, "%s", (INT_ARG_1) ? "#t" : "#f");}
static char *descr_display_bool(int *args, int *ints, Float *dbls) {return(mus_format("display(i%d(%s))", args[1], (INT_ARG_1) ? "#t" : "#f"));}
static void display_con(int *args, int *ints, Float *dbls) {fprintf(stderr, "continuation");}
static char *descr_display_con(int *args, int *ints, Float *dbls) {return(mus_format("display(i%d(continuation))", args[1]));}
static void display_func(int *args, int *ints, Float *dbls) {fprintf(stderr, "%s", describe_ptree((ptree *)(INT_ARG_1)));}
static char *descr_display_func(int *args, int *ints, Float *dbls) {return(mus_format("display(" PTR_PT ")", args[1], (ptree *)(INT_ARG_1)));}
static xen_value *display_1(ptree *pt, xen_value **args, int num_args)
{
  switch (args[1]->type)
    {
    case R_STRING:     return(package(pt, R_BOOL, display_str, descr_display_str, args, 1));   break;
    case R_INT:        return(package(pt, R_BOOL, display_int, descr_display_int, args, 1));   break;
    case R_FLOAT:      return(package(pt, R_BOOL, display_flt, descr_display_flt, args, 1));   break;
    case R_CLM:        return(package(pt, R_BOOL, display_clm, descr_display_clm, args, 1));   break;
    case R_READER:     return(package(pt, R_BOOL, display_rd, descr_display_rd, args, 1));     break;
    case R_FLOAT_VECTOR:
    case R_VCT:        return(package(pt, R_BOOL, display_vct, descr_display_vct, args, 1));   break;
    case R_BOOL:       return(package(pt, R_BOOL, display_bool, descr_display_bool, args, 1)); break;
    case R_CHAR:       return(package(pt, R_BOOL, display_chr, descr_display_chr, args, 1));   break;
    case R_GOTO:       return(package(pt, R_BOOL, display_con, descr_display_con, args, 1));   break;
    case R_FUNCTION:   return(package(pt, R_BOOL, display_func, descr_display_func, args, 1)); break;
    case R_CLM_VECTOR:
    case R_VCT_VECTOR:
    case R_INT_VECTOR: return(package(pt, R_BOOL, display_int_vct, descr_display_int_vct, args, 1));   break;
    }
  return(NULL);
}

static void snd_print_s(int *args, int *ints, Float *dbls) {listener_append(get_global_state(), STRING_ARG_1);}
static char *descr_snd_print_s(int *args, int *ints, Float *dbls) {return(mus_format("snd_print(" STR_PT ")", args[1], STRING_ARG_1));}
static xen_value *snd_print_1(ptree *pt, xen_value **args, int num_args)
{
  if (args[1]->type != R_STRING) return(run_warn("bad arg to snd-print"));
  return(package(pt, R_BOOL, snd_print_s, descr_snd_print_s, args, 1));
}

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
  return(mus_format( STR_PT " = make-string(" INT_PT ", " CHR_PT ")", 
		    args[0], STRING_RESULT, args[1], INT_ARG_1, args[1], CHAR_ARG_2));
}
static xen_value *make_string_1(ptree *pt, xen_value **args, int num_args)
{
  if ((num_args == 0) || (num_args > 2) || 
      (args[1]->type != R_INT) || 
      ((num_args == 2) && (args[2]->type != R_CHAR)))
    return(run_warn("bad args to make-string"));
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
  int i, start, end, len;
  start = ints[args[2]];
  end = ints[args[3]];
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
static xen_value *substring_1(ptree *pt, xen_value **args, int num_args, int constants)
{
  if ((num_args != 3) || (args[1]->type != R_STRING) || (args[2]->type != R_INT) || (args[3]->type != R_INT))
    return(NULL);
  if (constants == 3)
    return(make_xen_value(R_STRING, 
			  add_string_to_ptree(pt, substring((char *)(pt->ints[args[1]->addr]), pt->ints[args[2]->addr], pt->ints[args[3]->addr])),
			  R_CONSTANT));
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
    mus_snprintf(buf, 1024, INT_PT " = %s(" STR_PT , args[0], BOOL_RESULT, which, args[2], (char *)(ints[args[2]]));
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
static xen_value *string_append_1(ptree *pt, xen_value **args, int num_args, int constants)
{
  if (num_args == 0)
    return(make_xen_value(R_STRING, add_string_to_ptree(pt, (char *)CALLOC(1, sizeof(char))), R_CONSTANT));
  if (num_args == constants)
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
static xen_value *string_ ## CName ## _1(ptree *pt, xen_value **args, int num_args, int constants) \
{ \
  int i; \
  char *lasts = NULL; \
  if (num_args <= 1) return(make_xen_value(R_BOOL, add_int_to_ptree(pt, TRUE), R_CONSTANT)); \
  if (num_args == constants) return(make_xen_value(R_BOOL, add_int_to_ptree(pt, str_ ## CName ## _n(pt, args, num_args)), R_CONSTANT)); \
  if (constants > 1) \
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
static xen_value *string_ci_ ## CName ## _1(ptree *pt, xen_value **args, int num_args, int constants) \
{ \
  if (num_args <= 1) return(make_xen_value(R_BOOL, add_int_to_ptree(pt, TRUE), R_CONSTANT)); \
  if (num_args == constants) return(make_xen_value(R_BOOL, add_int_to_ptree(pt, str_ci_ ## CName ## _n(pt, args, num_args)), R_CONSTANT)); \
  return(package_n(pt, R_BOOL, string_ci_ ## CName ## _n, descr_string_ci_ ## CName ## _n, args, num_args)); \
}

STR_CI_REL_OP(eq, string=?, !=)
STR_CI_REL_OP(geq, string>=?, <)
STR_CI_REL_OP(leq, string<=?, >)
STR_CI_REL_OP(gt, string>?, <=)
STR_CI_REL_OP(lt, string<?, >=)


/* ---------------- number->string ---------------- */

/* fallback on Guile's number->string */
static char *f2s_1(Float n) {return(copy_string(XEN_TO_C_STRING(scm_number_to_string(C_TO_XEN_DOUBLE(n), XEN_UNDEFINED))));}
static char *f2s_2(Float n, int rad) {return(copy_string(XEN_TO_C_STRING(scm_number_to_string(C_TO_XEN_DOUBLE(n), C_TO_XEN_INT(rad)))));}
static char *i2s_1(int n) {return(copy_string(XEN_TO_C_STRING(scm_number_to_string(C_TO_XEN_INT(n), XEN_UNDEFINED))));}
static char *i2s_2(int n, int rad) {return(copy_string(XEN_TO_C_STRING(scm_number_to_string(C_TO_XEN_INT(n), C_TO_XEN_INT(rad)))));}
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
static xen_value *number2string_1(ptree *prog, xen_value **args, int num_args, int constants)
{
  if ((args[1]->type == R_INT) || (args[1]->type == R_FLOAT))
    {
      if (num_args == 1)
	{
	  if (constants == 1)
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
	      if (constants == 2)
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


/* ---------------- edit-position ---------------- */

static void edit_position_i(int *args, int *ints, Float *dbls) 
{
  snd_state *ss;
  chan_info *cp; 
  ss = get_global_state();
  cp = ss->sounds[INT_ARG_1]->chans[INT_ARG_2];
  INT_RESULT = cp->edit_ctr;
}
static char *descr_edit_position_i(int *args, int *ints, Float *dbls) 
{
  return(mus_format( INT_PT " = edit_position(" INT_PT ", " INT_PT ")", args[0], INT_RESULT, args[1], INT_ARG_1, args[2], INT_ARG_2));
}
static xen_value *edit_position_1(ptree *pt, xen_value **args, int num_args)
{
  if ((num_args == 2) &&
      (args[1]->type == R_INT) &&
      (args[2]->type == R_INT))
    return(package(pt, R_INT, edit_position_i, descr_edit_position_i, args, 2));
  return(NULL);
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
static xen_value *reader_1(ptree *prog, xen_value **args) 
{
  return(package(prog, R_FLOAT, reader_f, descr_reader_f, args, 1));
}

static char *descr_next_reader_f(int *args, int *ints, Float *dbls) {return(descr_reader(args, ints, dbls, "next-sample"));}
static void next_reader_f(int *args, int *ints, Float *dbls) {FLOAT_RESULT = protected_next_sample_to_float(((snd_fd *)(INT_ARG_1)));}
static xen_value *next_sample_1(ptree *prog, xen_value **args) 
{
  return(package(prog, R_FLOAT, next_reader_f, descr_next_reader_f, args, 1));
}

static char *descr_previous_reader_f(int *args, int *ints, Float *dbls) {return(descr_reader(args, ints, dbls, "previous-sample"));}
static void previous_reader_f(int *args, int *ints, Float *dbls) {FLOAT_RESULT = protected_previous_sample_to_float(((snd_fd *)(INT_ARG_1)));}
static xen_value *previous_sample_1(ptree *prog, xen_value **args) 
{
  return(package(prog, R_FLOAT, previous_reader_f, descr_previous_reader_f, args, 1));
}

static char *descr_make_sample_reader_r(int *args, int *ints, Float *dbls)
{
  return(mus_format( PTR_PT " = make-sample-reader(" INT_PT ", " INT_PT ", " INT_PT ", " INT_PT ", " INT_PT ")",
		    args[0], (snd_fd *)(INT_RESULT), 
		    args[1], INT_ARG_1, args[2], INT_ARG_2, args[3], INT_ARG_3, args[4], INT_ARG_4, args[5], INT_ARG_5));
}
static void make_sample_reader_r(int *args, int *ints, Float *dbls) 
{
  snd_info *sp = NULL;
  snd_state *ss = NULL;
  chan_info *cp = NULL;
  int pos;
  ss = get_global_state();
  if (INT_ARG_2 == -1)
    sp = any_selected_sound(ss);
  else sp = ss->sounds[INT_ARG_2];
  if (INT_ARG_3 == -1)
    cp = any_selected_channel(sp);
  else cp = sp->chans[INT_ARG_3];
  if (INT_ARG_5 == -1)
    pos = cp->edit_ctr;
  else pos = INT_ARG_5;
  if (INT_RESULT) free_snd_fd((snd_fd *)(INT_RESULT));
  INT_RESULT = (int)(init_sample_read_any(INT_ARG_1, cp, INT_ARG_4, pos));
}
static xen_value *make_sample_reader_1(ptree *pt, xen_value **args, int num_args)
{
  xen_value *true_args[6];
  xen_value *rtn;
  if (num_args > 5) return(run_warn("too many args for make-sample-reader"));
  if (num_args < 5) 
    true_args[5] = make_xen_value(R_INT, add_int_to_ptree(pt, AT_CURRENT_EDIT_POSITION), R_CONSTANT);
  else true_args[5] = args[5];
  if (num_args < 4) 
    true_args[4] = make_xen_value(R_INT, add_int_to_ptree(pt, READ_FORWARD), R_CONSTANT);
  else true_args[4] = args[4];
  if (num_args < 3)
    true_args[3] = make_xen_value(R_INT, add_int_to_ptree(pt, -1), R_CONSTANT);
  else
    {
      if (args[3]->type == R_BOOL)
	pt->ints[args[3]->addr] = -1;
      else true_args[3] = args[3];
    }
  if (num_args < 2)
    true_args[2] = make_xen_value(R_INT, add_int_to_ptree(pt, -1), R_CONSTANT);
  else
    {
      if (args[2]->type == R_BOOL)
	pt->ints[args[2]->addr] = -1;
      else true_args[2] = args[2];
    }
  if (num_args == 0)
    true_args[1] = make_xen_value(R_INT, add_int_to_ptree(pt, 0), R_CONSTANT);
  else true_args[1] = args[1];
  true_args[0] = args[0];
  rtn = package(pt, R_READER, make_sample_reader_r, descr_make_sample_reader_r, true_args, 5);
  add_obj_to_gcs(pt, R_READER, rtn->addr);
  return(rtn);
}

/* ---------------- vector stuff ---------------- */

/* float vectors are handled as vcts
 */

/* length */
static void vector_length_i(int *args, int *ints, Float *dbls) {INT_RESULT = ((vct *)(INT_ARG_1))->length;}
static char *descr_vector_length_i(int *args, int *ints, Float *dbls) 
{
  return(mus_format("d%d(%d) = vector_length(" PTR_PT ")", args[0], INT_RESULT, args[1], (vct *)(INT_ARG_1)));
}
static xen_value *vector_length_1(ptree *prog, xen_value **args, int num_args)
{
  if (num_args != 1) return(NULL);
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
  if (num_args != 2) return(NULL);
  if (args[2]->type != R_INT) return(run_warn("vector-ref bad index type"));
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
static void vector_set_f(int *args, int *ints, Float *dbls) 
{
  FLOAT_RESULT = FLOAT_ARG_3; 
  ((vct *)(INT_ARG_1))->data[INT_ARG_2] = FLOAT_ARG_3;
}
static void vector_set_i(int *args, int *ints, Float *dbls) 
{
  INT_RESULT = INT_ARG_3; 
  ((int_vct *)(INT_ARG_1))->data[INT_ARG_2] = INT_ARG_3;
}
static void vector_set_v(int *args, int *ints, Float *dbls) 
{
  INT_RESULT = INT_ARG_3; 
  ((vct_vct *)(INT_ARG_1))->data[INT_ARG_2] = (vct *)(INT_ARG_3);
}
static void vector_set_c(int *args, int *ints, Float *dbls)
{
  INT_RESULT = INT_ARG_3; 
  ((clm_vct *)(INT_ARG_1))->data[INT_ARG_2] = (mus_any *)(INT_ARG_3);
}
static char *descr_vector_set_f(int *args, int *ints, Float *dbls) 
{
  return(mus_format( FLT_PT " = vector_set(" PTR_PT ", " INT_PT ", " FLT_PT ")", 
		    args[0], FLOAT_RESULT, args[1], (vct *)(INT_ARG_1), args[2], INT_ARG_2, args[3], FLOAT_ARG_3));
}
static char *descr_vector_set_i(int *args, int *ints, Float *dbls) 
{
  return(mus_format( INT_PT " = vector_set(" PTR_PT ", " INT_PT ", " INT_PT ")", 
		    args[0], INT_RESULT, args[1], (int_vct *)(INT_ARG_1), args[2], INT_ARG_2, args[3], INT_ARG_3));
}
static char *descr_vector_set_v(int *args, int *ints, Float *dbls) 
{
  return(mus_format( PTR_PT " = vector_set(" PTR_PT ", " INT_PT ", " PTR_PT ")", 
		    args[0], (vct *)(INT_RESULT), args[1], (vct_vct *)(INT_ARG_1), args[2], INT_ARG_2, args[3], (vct *)(INT_ARG_3)));
}
static char *descr_vector_set_c(int *args, int *ints, Float *dbls) 
{
  return(mus_format( PTR_PT " = vector_set(" PTR_PT ", " INT_PT ", " PTR_PT ")", 
		    args[0], (mus_any *)(INT_RESULT), args[1], (clm_vct *)(INT_ARG_1), args[2], INT_ARG_2, args[3], (mus_any *)(INT_ARG_3)));
}
static xen_value *vector_set_1(ptree *prog, xen_value **args, int num_args)
{
  xen_var *var;
  if (num_args != 3) return(NULL);
  var = find_var_in_ptree_via_addr(prog, FALSE, args[1]->addr);
  if (var) var->unclean = TRUE;
  if (args[2]->type != R_INT) return(run_warn("vector-set bad index type"));
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
  if (num_args != 2) return(NULL);
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
  return(mus_format("d%d(%d) = vct_length(" PTR_PT ")", args[0], INT_RESULT, args[1], (vct *)(INT_ARG_1)));
}
static xen_value *vct_length_1(ptree *prog, xen_value **args, int num_args)
{
  if (num_args != 1) return(NULL);
  return(package(prog, R_INT, vct_length_i, descr_vct_length_i, args, 1));
}

static void vct_ref_f(int *args, int *ints, Float *dbls) {FLOAT_RESULT = ((vct *)(INT_ARG_1))->data[INT_ARG_2];}
static char *descr_vct_ref_f(int *args, int *ints, Float *dbls) 
{
  return(mus_format( FLT_PT " = vct_ref(" PTR_PT ", " INT_PT ")", args[0], FLOAT_RESULT, args[1], (vct *)(INT_ARG_1), args[2], INT_ARG_2));
}
static xen_value *vct_ref_1(ptree *prog, xen_value **args, int num_args)
{
  if (num_args != 2) return(NULL);
  if (args[2]->type == R_INT)
    return(package(prog, R_FLOAT, vct_ref_f, descr_vct_ref_f, args, 2));
  return(run_warn("vct-ref bad index type"));
}

static void vct_set_f(int *args, int *ints, Float *dbls) {FLOAT_RESULT = FLOAT_ARG_3; ((vct *)(INT_ARG_1))->data[INT_ARG_2] = FLOAT_ARG_3;}
static char *descr_vct_set_f(int *args, int *ints, Float *dbls) 
{
  return(mus_format( FLT_PT " = vct_set!(" PTR_PT ", " INT_PT ", " FLT_PT ")",
		    args[0], FLOAT_RESULT, args[1], (vct *)(INT_ARG_1), args[2], INT_ARG_2, args[3], FLOAT_ARG_3));
}
static void vct_set_i(int *args, int *ints, Float *dbls) {FLOAT_RESULT = (Float)INT_ARG_3; ((vct *)(INT_ARG_1))->data[INT_ARG_2] = (Float)INT_ARG_3;}
static char *descr_vct_set_i(int *args, int *ints, Float *dbls) 
{
  return(mus_format( FLT_PT " = vct_set!(" PTR_PT ", " INT_PT ", " INT_PT ")",
		    args[0], FLOAT_RESULT, args[1], (vct *)(INT_ARG_1), args[2], INT_ARG_2, args[3], INT_ARG_3));
}
static xen_value *vct_set_1(ptree *prog, xen_value **args)
{
  xen_var *var;
  var = find_var_in_ptree_via_addr(prog, FALSE, args[1]->addr);
  if (var) var->unclean = TRUE;
  if (args[2]->type == R_INT)
    {
      if (args[3]->type == R_FLOAT)
	return(package(prog, R_FLOAT, vct_set_f, descr_vct_set_f, args, 3));
      else
	if (args[3]->type == R_INT)
	  return(package(prog, R_FLOAT, vct_set_i, descr_vct_set_i, args, 3));
    }
  return(run_warn("vct-set! bad index type"));
}

static void make_vct_v(int *args, int *ints, Float *dbls) 
{
  vct *v;
  v = c_make_vct(INT_ARG_1);
  if (INT_RESULT) c_free_vct((vct *)(INT_RESULT));
  INT_RESULT = (int)v;
}
static char *descr_make_vct_v(int *args, int *ints, Float *dbls) 
{
  return(mus_format( PTR_PT " = make_vct(" INT_PT ")", args[0], (vct *)INT_RESULT, args[1], INT_ARG_1));
}
static void make_vct_v2(int *args, int *ints, Float *dbls) 
{
  vct *v;
  int i;
  v = c_make_vct(INT_ARG_1);
  if (INT_RESULT) c_free_vct((vct *)(INT_RESULT));
  INT_RESULT = (int)v;
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
  else
    {
      if ((num_args == 2) && (args[2]->type == R_FLOAT))
	add_triple_to_ptree(prog, va_make_triple(make_vct_v2, descr_make_vct_v2, 3, args[0], args[1], args[2]));
      else 
	{
	  FREE(args[0]);
	  return(run_warn("make-vct: %s", (num_args == 2) ? "bad initial-element" : "wrong number of args"));
	}
    }
  return(args[0]);
}

static vct *c_vct_copy(vct *vc)
{
  vct *v;
  int len;
  len = vc->length;
  v = c_make_vct(len);
  memcpy((void *)(v->data), (void *)(vc->data), (len * sizeof(Float)));
  return(v);
}

static void vct_copy_v(int *args, int *ints, Float *dbls) 
{
  vct *v;
  v = c_vct_copy((vct *)(INT_ARG_1));
  if (INT_RESULT) c_free_vct((vct *)(INT_RESULT));
  INT_RESULT = (int)v;
}
static char *descr_vct_copy_v(int *args, int *ints, Float *dbls) 
{
  return(mus_format( PTR_PT " = vct_copy(" PTR_PT ")", args[0], (vct *)INT_RESULT, args[1], (vct *)(INT_ARG_1)));
}
static xen_value *vct_copy_1(ptree *prog, xen_value **args, int num_args)
{
  if (num_args == 1)
    {
      args[0] = make_xen_value(R_VCT, add_int_to_ptree(prog, 0), R_VARIABLE);
      add_obj_to_gcs(prog, R_VCT, args[0]->addr);
      add_triple_to_ptree(prog, va_make_triple(vct_copy_v, descr_vct_copy_v, 2, args[0], args[1]));
      return(args[0]);
    }
  return(NULL);
}

#define VCT_OP_1(SName, CName, COp) \
static void vct_ ## CName ## _f(int *args, int *ints, Float *dbls) \
{ \
  int i; \
  vct *v = (vct *)(INT_ARG_1); \
  for (i = 0; i < v->length; i++) v->data[i] COp FLOAT_ARG_2; \
  INT_RESULT = INT_ARG_1; \
} \
static char *descr_vct_ ## CName ## _f(int *args, int *ints, Float *dbls)  \
{ \
  return(mus_format("vct_" #SName "(" PTR_PT ", " FLT_PT ")", args[1], (vct *)(INT_ARG_1), args[2], FLOAT_ARG_2)); \
} \
static xen_value *vct_ ## CName ## _1(ptree *prog, xen_value **args, int num_args) \
{ \
  xen_value *temp; \
  if (num_args != 2) return(NULL); \
  if (args[2]->type == R_INT) \
    { \
      temp = args[2]; \
      args[2] = convert_int_to_dbl(prog, args[2]); \
      FREE(temp); \
    } \
  else \
    if (args[2]->type != R_FLOAT) \
      return(run_warn("bad " #CName " value for vct_" #SName )); \
  return(package(prog, R_VCT, vct_ ## CName ## _f, descr_vct_ ## CName ## _f, args, 2)); \
}

VCT_OP_1(fill!, fill, =)
VCT_OP_1(scale!, scale, *=)
VCT_OP_1(offset!, offset, +=)

#define VCT_OP_2(SName, CName, COp) \
static void vct_ ## CName ## _f(int *args, int *ints, Float *dbls) \
{ \
  int i, len; \
  vct *v0 = (vct *)(INT_ARG_1); \
  vct *v1 = (vct *)(INT_ARG_2); \
  len = v0->length; \
  if (v1->length < len) len = v1->length; \
  for (i = 0; i < len; i++) v0->data[i] COp v1->data[i]; \
  INT_RESULT = INT_ARG_1; \
} \
static char *descr_vct_ ## CName ## _f(int *args, int *ints, Float *dbls)  \
{ \
  return(mus_format("vct_" #SName "(" PTR_PT ", " PTR_PT ")", args[1], (vct *)(INT_ARG_1), args[2], (vct *)(INT_ARG_2))); \
} \
static xen_value *vct_ ## CName ## _1(ptree *prog, xen_value **args, int num_args) \
{ \
  if (num_args != 2) return(NULL); \
  return(package(prog, R_VCT, vct_ ## CName ## _f, descr_vct_ ## CName ## _f, args, 2)); \
}

VCT_OP_2(add!, add, +=)
VCT_OP_2(multiply!, multiply, *=)
VCT_OP_2(subtract!, subtract, -=)



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
  static char *descr_ ## Name ## _0p(int *args, int *ints, Float *dbls) {return(descr_gen(args, ints, dbls, #Name "?", 0));} \
  static void Name ## _0p(int *args, int *ints, Float *dbls) {BOOL_RESULT = mus_ ##Name ## _p((mus_any *)(INT_ARG_1));} \
  static xen_value * Name ## _p(ptree *prog, xen_value **args) \
  { \
    return(package(prog, R_BOOL, Name ## _0p, descr_ ## Name ## _0p, args, 1)); \
  }
#define GEN3_0(Name) \
  static char *descr_ ## Name ## _0f(int *args, int *ints, Float *dbls) {return(descr_gen(args, ints, dbls, #Name, 0));} \
  static void Name ## _0f(int *args, int *ints, Float *dbls) {FLOAT_RESULT = mus_ ## Name (((mus_any *)(INT_ARG_1)), 0.0, 0.0);}  
#define GEN2_0(Name) \
  static char *descr_ ## Name ## _0f(int *args, int *ints, Float *dbls) {return(descr_gen(args, ints, dbls, #Name, 0));} \
  static void Name ## _0f(int *args, int *ints, Float *dbls) {FLOAT_RESULT = mus_ ## Name (((mus_any *)(INT_ARG_1)), 0.0);}  
#define GEN1_0(Name) \
  static char *descr_ ## Name ## _0f(int *args, int *ints, Float *dbls) {return(descr_gen(args, ints, dbls, #Name, 0));} \
  static void Name ## _0f(int *args, int *ints, Float *dbls) {FLOAT_RESULT = mus_ ## Name ((mus_any *)(INT_ARG_1));}  
#define GEN3_1(Name) \
  static char *descr_ ## Name ## _1f(int *args, int *ints, Float *dbls) {return(descr_gen(args, ints, dbls, #Name, 1));} \
  static void Name ## _1f(int *args, int *ints, Float *dbls) {FLOAT_RESULT = mus_ ## Name (((mus_any *)(INT_ARG_1)), FLOAT_ARG_2, 0.0);}
#define GEN3_1_1(Name) \
  static char *descr_ ## Name ## _1f_1(int *args, int *ints, Float *dbls) {return(descr_gen(args, ints, dbls, #Name, 1));} \
  static void Name ## _1f_1(int *args, int *ints, Float *dbls) {FLOAT_RESULT = mus_ ## Name ## _1(((mus_any *)(INT_ARG_1)), FLOAT_ARG_2);}
#define GEN2_1(Name) \
  static char *descr_ ## Name ## _1f(int *args, int *ints, Float *dbls) {return(descr_gen(args, ints, dbls, #Name, 1));} \
  static void Name ## _1f(int *args, int *ints, Float *dbls) {FLOAT_RESULT = mus_ ## Name (((mus_any *)(INT_ARG_1)), FLOAT_ARG_2);}  
#define GEN3_2(Name) \
  static char *descr_ ## Name ## _2f(int *args, int *ints, Float *dbls) {return(descr_gen(args, ints, dbls, #Name, 2));} \
  static void Name ## _2f(int *args, int *ints, Float *dbls) {FLOAT_RESULT = mus_ ## Name (((mus_any *)(INT_ARG_1)), FLOAT_ARG_2, FLOAT_ARG_3);}  
#define GEN3(Name) \
  GEN3_0(Name) \
  GEN3_1(Name) \
  GEN3_2(Name) \
  GEN_P(Name) \
  static xen_value * Name ## _1(ptree *prog, xen_value **args, int num_args) \
  { \
    if ((num_args > 1) && (args[2]->type == R_INT)) single_to_float(prog, args, 2); \
    if ((num_args > 2) && (args[3]->type == R_INT)) single_to_float(prog, args, 3); \
    if (num_args == 1) return(package(prog, R_FLOAT, Name ## _0f, descr_ ## Name ## _0f, args, 1)); \
    if (num_args == 2) return(package(prog, R_FLOAT, Name ## _1f, descr_ ## Name ## _1f, args, 2)); \
    if (num_args == 3) return(package(prog, R_FLOAT, Name ## _2f, descr_ ## Name ## _2f, args, 3)); \
    return(run_warn(#Name ": wrong number of args")); \
  }
#define GEN3_1A(Name) \
  GEN3_0(Name) \
  GEN3_1_1(Name) \
  GEN3_2(Name) \
  GEN_P(Name) \
  static xen_value * Name ## _1(ptree *prog, xen_value **args, int num_args) \
  { \
    if ((num_args > 1) && (args[2]->type == R_INT)) single_to_float(prog, args, 2); \
    if ((num_args > 2) && (args[3]->type == R_INT)) single_to_float(prog, args, 3); \
    if (num_args == 1) return(package(prog, R_FLOAT, Name ## _0f, descr_ ## Name ## _0f, args, 1)); \
    if (num_args == 2) return(package(prog, R_FLOAT, Name ## _1f_1, descr_ ## Name ## _1f_1, args, 2)); \
    if (num_args == 3) return(package(prog, R_FLOAT, Name ## _2f, descr_ ## Name ## _2f, args, 3)); \
    return(run_warn(#Name ": wrong number of args")); \
  }
#define GEN2(Name) \
  GEN2_0(Name) \
  GEN2_1(Name) \
  GEN_P(Name) \
  static xen_value * Name ## _1(ptree *prog, xen_value **args, int num_args) \
  { \
    if ((num_args > 1) && (args[2]->type == R_INT)) single_to_float(prog, args, 2); \
    if (num_args == 1) return(package(prog, R_FLOAT, Name ## _0f, descr_ ## Name ## _0f, args, 1)); \
    if (num_args == 2) return(package(prog, R_FLOAT, Name ## _1f, descr_ ## Name ## _1f, args, 2)); \
    return(run_warn(#Name ": wrong number of args")); \
  }
#define GEN_ONLY_2(Name) \
  GEN2_1(Name) \
  static xen_value * Name ## _1(ptree *prog, xen_value **args, int num_args) \
  { \
    if (num_args == 2) \
      { \
        if (args[2]->type == R_INT) single_to_float(prog, args, 2); \
        return(package(prog, R_FLOAT, Name ## _1f, descr_ ## Name ## _1f, args, 2)); \
      } \
    return(run_warn(#Name ": wrong number of args")); \
  }
#define GEN1(Name) \
  GEN1_0(Name) \
  GEN_P(Name) \
  static xen_value * Name ## _1(ptree *prog, xen_value **args, int num_args) \
  { \
    if (num_args == 1) return(package(prog, R_FLOAT, Name ## _0f, descr_ ## Name ## _0f, args, 1)); \
    return(run_warn(#Name ": wrong number of args")); \
  }
#define GEN0(Name) \
  GEN1_0(Name) \
  static xen_value * mus_ ## Name ## _0(ptree *prog, xen_value **args) \
  { \
    return(package(prog, R_FLOAT, Name ## _0f, descr_ ## Name ## _0f, args, 1)); \
  }

static char *descr_int_gen0(int *args, int *ints, Float *dbls, char *which)
{
  return(mus_format( INT_PT " = %s(" PTR_PT , args[0], INT_RESULT, which, args[1], (void *)(INT_ARG_1)));
}
#define INT_GEN0(Name, Type) \
  static char *descr_ ## Name ## _0i(int *args, int *ints, Float *dbls) {return(descr_int_gen0(args, ints, dbls, #Name));} \
  static void Name ## _0i(int *args, int *ints, Float *dbls) {INT_RESULT = mus_ ## Name ((Type *)(INT_ARG_1));} \
  static xen_value * mus_ ## Name ## _0(ptree *prog, xen_value **args) \
  { \
    return(package(prog, R_INT, Name ## _0i, descr_ ## Name ## _0i, args, 1)); \
  }

GEN3_1A(oscil)
GEN1(env)
GEN3(notch)
GEN3(comb)
GEN3(delay)
GEN3(all_pass)
GEN2(rand)
GEN2(rand_interp)
GEN2(sum_of_cosines)
GEN2(table_lookup)
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
GEN2(wave_train)
GEN3(waveshape)
GEN2(filter)
GEN2(fir_filter)
GEN2(iir_filter)
GEN1(readin)

GEN_ONLY_2(sample2buffer)
GEN_ONLY_2(tap)

GEN0(buffer2sample)
GEN0(increment)
GEN0(frequency)
GEN0(phase)
GEN0(scaler)
GEN0(formant_radius)
GEN0(a0)
GEN0(a1)
GEN0(a2)
GEN0(b1)
GEN0(b2)
GEN0(feedforward)
GEN0(feedback)

INT_GEN0(hop, mus_any)
INT_GEN0(channels, mus_any)
INT_GEN0(location, mus_any)
INT_GEN0(ramp, mus_any)
INT_GEN0(position, mus_any)
INT_GEN0(order, mus_any)
INT_GEN0(length, mus_any)
INT_GEN0(cosines, mus_any)
INT_GEN0(channel, mus_input)

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
  if (num_args != 3) return(run_warn("mus-set-formant-radius-and-frequency: wrong number of args"));
  if (args[2]->type == R_INT) single_to_float(prog, args, 2);
  if (args[3]->type == R_INT) single_to_float(prog, args, 3);
  return(package(prog, R_BOOL, set_formant_radius_and_frequency_2f, descr_set_formant_radius_and_frequency_2f, args, 3));
}


static char *descr_move_locsig_2f(int *args, int *ints, Float *dbls) {return(descr_gen(args, ints, dbls, "move-locsig", 2));}
static void move_locsig_2f(int *args, int *ints, Float *dbls) {mus_move_locsig(((mus_any *)(INT_ARG_1)), FLOAT_ARG_2, FLOAT_ARG_3);}  
static xen_value *move_locsig_1(ptree *prog, xen_value **args, int num_args)
{ 
  if (num_args != 3) return(run_warn("move-locsig: wrong number of args"));
  return(package(prog, R_BOOL, move_locsig_2f, descr_move_locsig_2f, args, 3));
}


static char *descr_str_gen0(int *args, int *ints, Float *dbls, char *which)
{
  return(mus_format( STR_PT " = %s(" PTR_PT ")", args[0], STRING_RESULT, which, args[1], (mus_any *)(INT_ARG_1)));
}
#define STR_GEN0(Name) \
  static char *descr_ ## Name ## _0s(int *args, int *ints, Float *dbls) {return(descr_str_gen0(args, ints, dbls, #Name));} \
  static void Name ## _0s(int *args, int *ints, Float *dbls) {STRING_RESULT = copy_string(mus_ ## Name ((mus_any *)(INT_ARG_1)));} \
  static xen_value * mus_ ## Name ## _0(ptree *prog, xen_value **args) \
  { \
    return(package(prog, R_STRING, Name ## _0s, descr_ ## Name ## _0s, args, 1)); \
  }

STR_GEN0(name)
STR_GEN0(describe)
STR_GEN0(inspect)


static char *descr_ref_gen0(int *args, int *ints, Float *dbls, char *which)
{
  return(mus_format( FLT_PT " = %s(" PTR_PT ", " INT_PT ")", args[0], FLOAT_RESULT, which, args[1], (mus_any *)(INT_ARG_1), args[2], INT_ARG_2));
}
#define REF_GEN0(Name, Type) \
  static char *descr_ ## Name ## _0r(int *args, int *ints, Float *dbls) {return(descr_ref_gen0(args, ints, dbls, #Name));} \
  static void Name ## _0r(int *args, int *ints, Float *dbls) {FLOAT_RESULT = mus_ ## Name ((Type *)(INT_ARG_1), INT_ARG_2);} \
  static xen_value * Name ## _0(ptree *prog, xen_value **args, int num_args) \
  { \
    if (num_args != 2) return(run_warn(#Name ": wrong number of args")); \
    if (args[2]->type != R_INT) return(run_warn(#Name ": bad arg 2")); \
    return(package(prog, R_FLOAT, Name ## _0r, descr_ ## Name ## _0r, args, 2)); \
  }

REF_GEN0(frame_ref, mus_frame)
REF_GEN0(locsig_ref, mus_any)
REF_GEN0(locsig_reverb_ref, mus_any)


static char *descr_set_gen0(int *args, int *ints, Float *dbls, char *which)
{
  return(mus_format("%s(" PTR_PT ", " INT_PT ", " FLT_PT ")", which, args[1], (mus_any *)(INT_ARG_1), args[2], INT_ARG_2, args[3], FLOAT_ARG_3));
}
#define SET_GEN0(Name, Type) \
  static char *descr_ ## Name ## _0r(int *args, int *ints, Float *dbls) {return(descr_set_gen0(args, ints, dbls, #Name));} \
  static void Name ## _0r(int *args, int *ints, Float *dbls) \
    { \
      FLOAT_RESULT = FLOAT_ARG_3; \
      mus_ ## Name ((Type *)(INT_ARG_1), INT_ARG_2, FLOAT_ARG_3); \
    } \
  static xen_value * Name ## _0(ptree *prog, xen_value **args, int num_args) \
  { \
    if (num_args != 3) return(run_warn(#Name ": wrong number of args")); \
    if (args[2]->type != R_INT) return(run_warn(#Name ": bad arg 2")); \
    if (args[3]->type != R_FLOAT) return(run_warn(#Name ": bad arg 3")); \
    return(package(prog, R_FLOAT, Name ## _0r, descr_ ## Name ## _0r, args, 3)); \
  }

SET_GEN0(frame_set, mus_frame)
SET_GEN0(locsig_set, mus_any)
SET_GEN0(locsig_reverb_set, mus_any)


static char *descr_mixer_ref_0(int *args, int *ints, Float *dbls)
{
  return(mus_format( FLT_PT " = mixer-ref(" PTR_PT ", " INT_PT ", " INT_PT ")", 
		    args[0], FLOAT_RESULT, args[1], (mus_any *)(INT_ARG_1), args[2], INT_ARG_2, args[3], INT_ARG_3));
}
static void mixer_ref_0(int *args, int *ints, Float *dbls) {FLOAT_RESULT = mus_mixer_ref((mus_mixer *)(INT_ARG_1), INT_ARG_2, INT_ARG_3);}
static xen_value *mixer_ref_1(ptree *prog, xen_value **args, int num_args)
{
  if (num_args != 3) return(run_warn("mixer-ref: wrong number of args"));
  if ((args[2]->type != R_INT) || (args[3]->type != R_INT)) return(run_warn("mixer-ref: bad arg index"));
  return(package(prog, R_FLOAT, mixer_ref_0, descr_mixer_ref_0, args, 3));
}

static char *descr_mixer_set_0(int *args, int *ints, Float *dbls)
{
  return(mus_format("mixer-set!(" PTR_PT ", " INT_PT ", " INT_PT ", " FLT_PT ")", 
		    args[1], (mus_any *)(INT_ARG_1), args[2], INT_ARG_2, args[3], INT_ARG_3, args[4], FLOAT_ARG_4));
}
static void mixer_set_0(int *args, int *ints, Float *dbls) 
{
  FLOAT_RESULT = FLOAT_ARG_4;
  mus_mixer_set((mus_mixer *)(INT_ARG_1), INT_ARG_2, INT_ARG_3, FLOAT_ARG_4);
}
static xen_value *mixer_set_1(ptree *prog, xen_value **args, int num_args)
{
  if (num_args != 4) return(run_warn("mixer-set!: wrong number of args"));
  if ((args[2]->type != R_INT) || (args[3]->type != R_INT)) return(run_warn("mixer-set!: bad arg index"));
  if (args[4]->type != R_FLOAT) return(run_warn("mixer-set!: bad arg 4"));
  return(package(prog, R_FLOAT, mixer_set_0, descr_mixer_set_0, args, 4));
}



static char *descr_set_int_gen0(int *args, int *ints, Float *dbls, char *which)
{
  return(mus_format("mus-%s(" PTR_PT ") = " INT_PT , which, args[0], (mus_any *)(INT_RESULT), args[1], INT_ARG_1));
}
#define SET_INT_GEN0(Name) \
  static char *descr_set_ ## Name ## _i(int *args, int *ints, Float *dbls) {return(descr_set_int_gen0(args, ints, dbls, #Name));} \
  static void set_ ## Name ## _i(int *args, int *ints, Float *dbls) {mus_set_ ## Name ((mus_any *)(INT_RESULT), INT_ARG_1);} \
  static xen_value * mus_set_ ## Name ## _1(ptree *prog, xen_value *in_v, xen_value *v) \
  { \
    add_triple_to_ptree(prog, va_make_triple(set_ ## Name ## _i, descr_set_ ## Name ## _i, 2, in_v, v)); \
    return(v); \
  }

SET_INT_GEN0(location)
SET_INT_GEN0(ramp)
SET_INT_GEN0(hop)
SET_INT_GEN0(length)

static char *descr_set_dbl_gen0(int *args, int *ints, Float *dbls, char *which)
{
  return(mus_format("mus-%s(" PTR_PT ") = " FLT_PT , which, args[0], (mus_any *)(INT_RESULT), args[1], FLOAT_ARG_1));
}
#define SET_DBL_GEN0(Name) \
  static char *descr_set_ ## Name ## _f(int *args, int *ints, Float *dbls) {return(descr_set_dbl_gen0(args, ints, dbls, #Name));} \
  static void set_ ## Name ## _f(int *args, int *ints, Float *dbls) {mus_set_ ## Name ((mus_any *)(INT_RESULT), FLOAT_ARG_1);} \
  static xen_value * mus_set_ ## Name ## _1(ptree *prog, xen_value *in_v, xen_value *v) \
  { \
    add_triple_to_ptree(prog, va_make_triple(set_ ## Name ## _f, descr_set_ ## Name ## _f, 2, in_v, v)); \
    return(v); \
  }

SET_DBL_GEN0(increment)
SET_DBL_GEN0(scaler)
SET_DBL_GEN0(feedback)
SET_DBL_GEN0(feedforward)
SET_DBL_GEN0(a0)
SET_DBL_GEN0(a1)
SET_DBL_GEN0(a2)
SET_DBL_GEN0(b1)
SET_DBL_GEN0(b2)
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
static xen_value *polynomial_1(ptree *prog, xen_value **args, int num_args) 
{
  if (num_args == 2) return(package(prog, R_FLOAT, polynomial_1f, descr_polynomial_1f, args, 2));
  return(run_warn("polynomial: wrong number of args"));
}

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
  if (num_args == 4) return(package(prog, R_CLM, mus_fft_2v_2, descr_mus_fft_2v_2, args, 4));
  return(run_warn("mus-fft: wrong number of args"));
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
  if (num_args == 3) return(package(prog, R_FLOAT, file2sample_2f, descr_file2sample_2f, args, 3));
  return(run_warn("file->sample: wrong number of args"));
}

static char *descr_sample2file_4(int *args, int *ints, Float *dbls) 
{
  return(mus_format( FLT_PT " = sample->file(" PTR_PT ", " INT_PT ", " INT_PT ", " FLT_PT ")", 
		    args[0], FLOAT_RESULT, args[1], (mus_any *)(INT_ARG_1), args[2], INT_ARG_2, args[3], INT_ARG_3, args[4], FLOAT_ARG_4));
}
static void sample2file_4(int *args, int *ints, Float *dbls) {FLOAT_RESULT = mus_sample2file((mus_any *)(INT_ARG_1), INT_ARG_2, INT_ARG_3, FLOAT_ARG_4);}
static xen_value *sample2file_1(ptree *prog, xen_value **args, int num_args) 
{
  if ((args[2]->type != R_INT) || (args[3]->type != R_INT) || (args[4]->type != R_FLOAT))
    return(run_warn("sample->file: bad arg type"));
  if (num_args == 4) return(package(prog, R_FLOAT, sample2file_4, descr_sample2file_4, args, 4));
  return(run_warn("sample->file: wrong number of args"));
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
static xen_value *locsig_1(ptree *prog, xen_value **args, int num_args) 
{
  if (args[2]->type != R_INT) return(run_warn("locsig 2nd arg not int"));
  if (args[3]->type != R_FLOAT) return(run_warn("locsig 3rd arg not real"));
  if (num_args == 3) return(package(prog, R_CLM, locsig_3, descr_locsig_3, args, 3));
  return(run_warn("locsig: wrong number of args"));
}


/* ---------------- env-interp ---------------- */
static char *descr_env_interp_2(int *args, int *ints, Float *dbls) 
{
  return(mus_format( FLT_PT " = env-interp(" FLT_PT ", " PTR_PT ")", args[0], FLOAT_RESULT, args[1], FLOAT_ARG_1, args[2], (mus_any *)(INT_ARG_2)));
}
static void env_interp_2(int *args, int *ints, Float *dbls) {FLOAT_RESULT = mus_env_interp(FLOAT_ARG_1, (mus_any *)(INT_ARG_2));}
static xen_value *env_interp_1(ptree *prog, xen_value **args, int num_args) 
{
  if (args[1]->type != R_FLOAT) return(run_warn("env-interp 1st arg not real"));
  if (num_args == 2) return(package(prog, R_FLOAT, env_interp_2, descr_env_interp_2, args, 2));
  return(run_warn("env-interp: wrong number of args"));
}


/* ---------------- frame+ etc ---------------- */

#define FRAME_OP(CName, SName, OutType, Type1, Type2, Type3) \
static char *descr_ ## CName ## _2(int *args, int *ints, Float *dbls) \
{ \
  return(mus_format( PTR_PT " = " #SName "(" PTR_PT ", " PTR_PT ")", \
		    args[0], (OutType *)(INT_RESULT), args[1], (Type1 *)(INT_ARG_1), args[2], (Type2 *)(INT_ARG_2))); \
} \
static char *descr_ ## CName ## _3(int *args, int *ints, Float *dbls) \
{ \
  return(mus_format( PTR_PT " = " #SName "(" PTR_PT ", " PTR_PT ", " PTR_PT ")", \
		    args[0], (OutType *)(INT_RESULT), args[1], (Type1 *)(INT_ARG_1), args[2], (Type2 *)(INT_ARG_2), args[3], (Type3 *)(INT_ARG_3))); \
} \
static void CName ## _2(int *args, int *ints, Float *dbls) \
{ \
  if (INT_RESULT) mus_free((mus_any *)(INT_RESULT)); \
  INT_RESULT = (int)CName((Type1 *)(INT_ARG_1), (Type2 *)(INT_ARG_2), NULL); \
} \
static void CName ## _3(int *args, int *ints, Float *dbls) \
{ \
  INT_RESULT = (int)CName((Type1 *)(INT_ARG_1), (Type2 *)(INT_ARG_2), (Type3 *)(INT_ARG_3)); \
} \
static xen_value * CName ## _1(ptree *prog, xen_value **args, int num_args) \
{ \
  if (num_args == 2) return(package(prog, R_CLM, CName ## _2, descr_ ## CName ## _2, args, 2)); \
  if (num_args == 3) return(package(prog, R_CLM, CName ## _3, descr_ ## CName ## _3, args, 3)); \
  return(run_warn(#SName ": wrong number of args")); \
}

FRAME_OP(mus_frame_add, frame+, mus_frame, mus_frame, mus_frame, mus_frame)
FRAME_OP(mus_frame_multiply, frame*, mus_frame, mus_frame, mus_frame, mus_frame)
FRAME_OP(mus_frame2frame, frame->frame, mus_frame, mus_mixer, mus_frame, mus_frame)
FRAME_OP(mus_mixer_multiply, mixer*, mus_mixer, mus_mixer, mus_mixer, mus_mixer)


/* ---------------- frame->sample ---------------- */
static char *descr_frame2sample_2(int *args, int *ints, Float *dbls) 
{
  return(mus_format( FLT_PT " = frame->sample(" PTR_PT ", " PTR_PT ")", args[0], FLOAT_RESULT, args[1], (mus_any *)(INT_ARG_1), args[2], (mus_frame *)(INT_ARG_2)));
}
static void frame2sample_2(int *args, int *ints, Float *dbls) 
{
  FLOAT_RESULT = mus_frame2sample((mus_any *)(INT_ARG_1), (mus_frame *)(INT_ARG_2));
}
static xen_value *frame2sample_1(ptree *prog, xen_value **args, int num_args) 
{
  if (num_args == 2) return(package(prog, R_FLOAT, frame2sample_2, descr_frame2sample_2, args, 2));
  return(run_warn("frame->sample: wrong number of args"));
}

/* ---------------- sample->frame ---------------- */
static char *descr_sample2frame_2(int *args, int *ints, Float *dbls) 
{
  return(mus_format( PTR_PT " = sample->frame(" PTR_PT ", " FLT_PT ")", 
		     args[0], (mus_frame *)(INT_RESULT), args[1], (mus_any *)(INT_ARG_1), args[2], FLOAT_ARG_2));
}
static void sample2frame_2(int *args, int *ints, Float *dbls) {INT_RESULT = (int)mus_sample2frame((mus_any *)(INT_ARG_1), FLOAT_ARG_2, NULL);}
static char *descr_sample2frame_3(int *args, int *ints, Float *dbls) 
{
  return(mus_format( PTR_PT " = sample->frame(" PTR_PT ", " FLT_PT ", " PTR_PT ")", 
		     args[0], (mus_frame *)(INT_RESULT), args[1], (mus_any *)(INT_ARG_1), args[2], FLOAT_ARG_2, args[3], (mus_frame *)(INT_ARG_3)));
}
static void sample2frame_3(int *args, int *ints, Float *dbls) 
{
  INT_RESULT = (int)mus_sample2frame((mus_any *)(INT_ARG_1), FLOAT_ARG_2, (mus_frame *)(INT_ARG_3));
}
static xen_value *sample2frame_1(ptree *prog, xen_value **args, int num_args) 
{
  if (num_args == 2) return(package(prog, R_CLM, sample2frame_2, descr_sample2frame_2, args, 2));
  if (num_args == 3) return(package(prog, R_CLM, sample2frame_3, descr_sample2frame_3, args, 3));
  return(run_warn("sample->frame: wrong number of args"));
}

/* ---------------- frame->buffer ---------------- */
static char *descr_frame2buffer_2(int *args, int *ints, Float *dbls) 
{
  return(mus_format( PTR_PT " = frame->buffer(" PTR_PT ", " PTR_PT ")", 
		    args[0], (mus_frame *)INT_RESULT, args[1], (mus_any *)(INT_ARG_1), args[2], (mus_any *)(INT_ARG_2)));
}
static void frame2buffer_2(int *args, int *ints, Float *dbls) 
{
  INT_RESULT = (int)mus_frame2buffer((mus_any *)(INT_ARG_1), (mus_any *)(INT_ARG_2));
}
static xen_value *frame2buffer_1(ptree *prog, xen_value **args, int num_args) 
{
  if (num_args == 2) return(package(prog, R_CLM, frame2buffer_2, descr_frame2buffer_2, args, 2));
  return(run_warn("frame->buffer: wrong number of args"));
}


/* ---------------- buffer->frame ---------------- */
static char *descr_buffer2frame_1b(int *args, int *ints, Float *dbls) 
{
  return(mus_format( PTR_PT " = buffer->frame(" PTR_PT ")", args[0], (mus_frame *)INT_RESULT, args[1], (mus_any *)(INT_ARG_1)));
}
static void buffer2frame_1b(int *args, int *ints, Float *dbls) {INT_RESULT = (int)mus_buffer2frame((mus_any *)(INT_ARG_1), NULL);}
static char *descr_buffer2frame_2b(int *args, int *ints, Float *dbls) 
{
  return(mus_format( PTR_PT " = buffer->frame(" PTR_PT ", " PTR_PT ")", 
		    args[0], (mus_frame *)INT_RESULT, args[1], (mus_any *)(INT_ARG_1), args[2], (mus_any *)(INT_ARG_2)));
}
static void buffer2frame_2b(int *args, int *ints, Float *dbls) {INT_RESULT = (int)mus_buffer2frame((mus_any *)(INT_ARG_1), (mus_any *)(INT_ARG_2));}
static xen_value *buffer2frame_1(ptree *prog, xen_value **args, int num_args) 
{
  if (num_args == 1) return(package(prog, R_CLM, buffer2frame_1b, descr_buffer2frame_1b, args, 1));
  if (num_args == 2) return(package(prog, R_CLM, buffer2frame_2b, descr_buffer2frame_2b, args, 2));
  return(run_warn("buffer->frame: wrong number of args"));
}


/* ---------------- frame->file ---------------- */
static char *descr_frame2file_3(int *args, int *ints, Float *dbls) 
{
  return(mus_format( PTR_PT " = frame->file((" PTR_PT ", " INT_PT ", " PTR_PT ")", 
		    args[0], (void *)(INT_RESULT), args[1], (mus_any *)(INT_ARG_1), args[2], INT_ARG_2, args[3], (mus_frame *)(INT_ARG_3)));
}
static void frame2file_3(int *args, int *ints, Float *dbls) 
{
  INT_RESULT = (int)mus_frame2file((mus_any *)(INT_ARG_1), INT_ARG_2, (mus_frame *)(INT_ARG_3));
}
static xen_value *frame2file_1(ptree *prog, xen_value **args, int num_args) 
{
  if (args[2]->type != R_INT) return(run_warn("frame->file 2nd arg not int"));
  if (num_args == 3) return(package(prog, R_CLM, frame2file_3, descr_frame2file_3, args, 3));
  return(run_warn("frame->file: wrong number of args"));
}

static char *descr_file2frame_3(int *args, int *ints, Float *dbls) 
{
  return(mus_format( PTR_PT " = file->frame((" PTR_PT ", " INT_PT ", " PTR_PT ")", 
		    args[0], (void *)(INT_RESULT), args[1], (mus_any *)(INT_ARG_1), args[2], INT_ARG_2, args[3], (mus_frame *)(INT_ARG_3)));
}
static void file2frame_3(int *args, int *ints, Float *dbls) 
{
  INT_RESULT = (int)mus_file2frame((mus_any *)(INT_ARG_1), INT_ARG_2, (mus_frame *)(INT_ARG_3));
}
static xen_value *file2frame_1(ptree *prog, xen_value **args, int num_args) 
{
  if (args[2]->type != R_INT) return(run_warn("file->frame 2nd arg not int"));
  if (num_args == 3) return(package(prog, R_CLM, file2frame_3, descr_file2frame_3, args, 3));
  return(run_warn("file->frame: wrong number of args"));
}


/* ---------------- outa ---------------- */
#define OUT_GEN(CName, SName) \
static char *descr_ ## CName ## _3(int *args, int *ints, Float *dbls) \
{ \
  return(mus_format( FLT_PT " = " #SName "(" INT_PT ", " FLT_PT ", " PTR_PT ")", \
		    args[0], FLOAT_RESULT, args[1], INT_ARG_1, args[2], FLOAT_ARG_2, args[3], (mus_output *)(INT_ARG_3))); \
} \
static void CName ## _3(int *args, int *ints, Float *dbls) {FLOAT_RESULT = mus_ ## CName (INT_ARG_1, FLOAT_ARG_2, (mus_output *)(INT_ARG_3));} \
static xen_value * CName ## _1(ptree *prog, xen_value **args, int num_args) \
{ \
  if (num_args != 3) return(run_warn( #SName ": wrong number of args")); \
  if ((args[1]->type != R_INT) || (args[2]->type != R_FLOAT)) return(run_warn( #SName ": bad arg type")); \
  return(package(prog, R_FLOAT, CName ## _3, descr_ ## CName ## _3, args, 3)); \
}

OUT_GEN(outa, outa)
OUT_GEN(outb, outb)
OUT_GEN(outc, outc)
OUT_GEN(outd, outd)

static char *descr_out_any_4(int *args, int *ints, Float *dbls)
{
  return(mus_format( FLT_PT " = out-any(" INT_PT ", " FLT_PT ", " INT_PT ", " PTR_PT ")",
		    args[0], FLOAT_RESULT, args[1], INT_ARG_1, args[2], FLOAT_ARG_2, args[3], INT_ARG_3, args[4], (mus_output *)(INT_ARG_4)));
}
static void out_any_4(int *args, int *ints, Float *dbls) {FLOAT_RESULT = mus_out_any(INT_ARG_1, FLOAT_ARG_2, INT_ARG_3, (mus_output *)(INT_ARG_4));} 
static xen_value *out_any_1(ptree *prog, xen_value **args, int num_args)
{
  if (num_args != 4) return(run_warn("out-any: wrong number of args"));
  if ((args[1]->type != R_INT) || (args[2]->type != R_FLOAT) || (args[3]->type != R_INT)) return(run_warn("out-any: bad arg type"));
  return(package(prog, R_FLOAT, out_any_4, descr_out_any_4, args, 4));
}


/* ---------------- ina ---------------- */
#define IN_GEN(CName, SName) \
static char *descr_ ## CName ## _2(int *args, int *ints, Float *dbls) \
{ \
  return(mus_format( FLT_PT " = " #SName "(" INT_PT ", " PTR_PT ")", \
		    args[0], FLOAT_RESULT, args[1], INT_ARG_1, args[2], (mus_output *)(INT_ARG_2))); \
} \
static void CName ## _2(int *args, int *ints, Float *dbls) {FLOAT_RESULT = mus_ ## CName (INT_ARG_1, (mus_input *)(INT_ARG_2));} \
static xen_value * CName ## _1(ptree *prog, xen_value **args, int num_args) \
{ \
  if (num_args != 2) return(run_warn( #SName ": wrong number of args")); \
  if (args[1]->type != R_INT) return(run_warn( #SName ": bad arg type")); \
  return(package(prog, R_FLOAT, CName ## _2, descr_ ## CName ## _2, args, 2)); \
}

IN_GEN(ina, ina)
IN_GEN(inb, inb)

static char *descr_in_any_3(int *args, int *ints, Float *dbls)
{
  return(mus_format( FLT_PT " = in-any(" INT_PT ", " INT_PT ", " PTR_PT ")",
		    args[0], FLOAT_RESULT, args[1], INT_ARG_1, args[2], INT_ARG_2, args[3], (mus_output *)(INT_ARG_3)));
}
static void in_any_3(int *args, int *ints, Float *dbls) {FLOAT_RESULT = mus_in_any(INT_ARG_1, INT_ARG_2, (mus_input *)(INT_ARG_3));}
static xen_value *in_any_1(ptree *prog, xen_value **args, int num_args)
{
  if (num_args != 3) return(run_warn("in-any: wrong number of args"));
  if ((args[1]->type != R_INT) || (args[2]->type != R_INT)) return(run_warn("in-any: bad arg type"));
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
  if ((num_args == 3) && (args[3]->type == R_INT))
    return(package(prog, R_FLOAT, array_interp_2f, descr_array_interp_2f, args, 3));
  return(run_warn("array-interp: wrong number of args"));
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
  if (num_args != 1) return(run_warn("vct-peak: wrong number of args"));
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
static xen_value *restart_env_1(ptree *prog, xen_value **args) 
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
  if ((num_args < 2) || (num_args > 3)) return(run_warn("wrong number of args for " #SName)); \
  if (num_args == 2) \
    return(package(prog, R_VCT, CName ## _2f, descr_ ## CName ## _2f, args, 2)); \
  else \
    { \
      if (args[3]->type != R_INT) return(run_warn(#SName ": 3 arg must be int, if given")); \
      return(package(prog, R_VCT, CName ## _3f, descr_ ## CName ## _3f, args, 3)); \
    } \
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
  if ((num_args < 2) || (num_args > 3)) return(run_warn("wrong number of args for " #SName)); \
  if (num_args == 2) \
    return(package(prog, R_FLOAT, CName ## _2f, descr_ ## CName ## _2f, args, 2)); \
  else \
    { \
      if (args[3]->type != R_INT) return(run_warn(#SName ": 3 arg must be int, if given")); \
      return(package(prog, R_FLOAT, CName ## _3f, descr_ ## CName ## _3f, args, 3)); \
    } \
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
  if ((num_args < 3) || (num_args > 5)) return(run_warn("spectrum: wrong number of args"));
  if (num_args == 3) return(package(prog, R_BOOL, mus_spectrum_3v, descr_mus_spectrum_3v, args, 3));
  if ((num_args == 4) && (args[4]->type == R_INT))
    return(package(prog, R_BOOL, mus_spectrum_4v, descr_mus_spectrum_4v, args, 4));
  if ((num_args == 5) && (args[4]->type == R_INT) && (args[5]->type == R_INT))
    return(package(prog, R_BOOL, mus_spectrum_5v, descr_mus_spectrum_5v, args, 5));
  return(run_warn("spectrum: trailing arg not int"));
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
      if (args[3]->type != R_FUNCTION) return(run_warn("src: bad input function"));
      pt = (ptree *)(prog->ints[args[3]->addr]);
      if (pt->arity != 1) return(run_warn("src: wrong number of args to input function"));
    }
  switch (num_args)
    {
    case 1: return(package(prog, R_FLOAT, src_0f, descr_src_0f, args, 1)); break;
    case 2: return(package(prog, R_FLOAT, src_1f, descr_src_1f, args, 2)); break;
    case 3: return(package(prog, R_FLOAT, src_2f, descr_src_2f, args, 3)); break;
    }
  return(run_warn("wrong number of args for src"));
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
      if (args[2]->type != R_FUNCTION) return(run_warn(#Name ": bad input function")); \
      pt = (ptree *)(prog->ints[args[2]->addr]); \
      if (pt->arity != 1) return(run_warn(#Name ": wrong number of args to input function")); \
    } \
  switch (num_args) \
    { \
    case 1: return(package(prog, R_FLOAT, Name ## _0f, descr_ ## Name ## _0f, args, 1)); break; \
    case 2: return(package(prog, R_FLOAT, Name ## _1f, descr_ ## Name ## _1f, args, 2)); break; \
    } \
  return(run_warn("wrong number of args for" #Name)); \
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
  if (num_args != 2) return(run_warn("wrong number of args for contrast-enhancement"));
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
  if (num_args != 2) return(run_warn("wrong number of args for ring-modulate"));
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
  if (num_args != 3) return(run_warn("wrong number of args for amplitude-modulate"));
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
static xen_value * CName ## _1(ptree *prog, xen_value **args) \
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
static xen_value *mus_sound_duration_1(ptree *prog, xen_value **args)
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
static xen_value *mus_sound_comment_1(ptree *prog, xen_value **args)
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
static xen_value *mus_header_type_name_1(ptree *prog, xen_value **args)
{
  if (args[1]->type == R_INT) return(package(prog, R_STRING, mus_header_type_name_f, descr_mus_header_type_name_f, args, 1));
  return(NULL);
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
static xen_value *mus_data_format_name_1(ptree *prog, xen_value **args)
{
  if (args[1]->type == R_INT) return(package(prog, R_STRING, mus_data_format_name_f, descr_mus_data_format_name_f, args, 1));
  return(NULL);
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
  if (num_args != 3) return(run_warn("formant-bank: wrong number of args"));
  if (args[3]->type == R_INT) single_to_float(prog, args, 3);
  return(package(prog, R_FLOAT, formant_bank_f, descr_formant_bank_f, args, 3));
}


/* ---------------- mus-srate ---------------- */
static char *descr_srate_f(int *args, int *ints, Float *dbls) {return(mus_format( FLT_PT " = mus-srate", args[0], FLOAT_RESULT));}
static void srate_f(int *args, int *ints, Float *dbls) {FLOAT_RESULT = mus_srate();}
static xen_value *mus_srate_1(ptree *prog, xen_value **args, int num_args) 
{
  if (num_args == 0) return(package(prog, R_FLOAT, srate_f, descr_srate_f, args, 0));
  return(run_warn("mus-srate: wrong number of args"));
}

static char *descr_set_srate_f(int *args, int *ints, Float *dbls) {return(mus_format("mus-srate = " FLT_PT , args[1], FLOAT_ARG_1));}
static void set_srate_f(int *args, int *ints, Float *dbls) {mus_set_srate(FLOAT_ARG_1); FLOAT_RESULT = mus_srate();}
static char *descr_set_srate_i(int *args, int *ints, Float *dbls) {return(mus_format("mus-srate = " INT_PT , args[1], INT_ARG_1));}
static void set_srate_i(int *args, int *ints, Float *dbls) {mus_set_srate((Float)INT_ARG_1); FLOAT_RESULT = mus_srate();}
static xen_value *mus_set_srate_1(ptree *prog, xen_value *in_v, xen_value *v)
{
  xen_value *nv;
  if (v->type == R_FLOAT)
    {
      nv = add_empty_var_to_ptree(prog, R_FLOAT);
      add_triple_to_ptree(prog, va_make_triple(set_srate_f, descr_set_srate_f, 2, nv, v));
      return(nv);
    }
  if (v->type == R_INT)
    {
      nv = add_empty_var_to_ptree(prog, R_FLOAT);
      add_triple_to_ptree(prog, va_make_triple(set_srate_i, descr_set_srate_i, 2, nv, v));
      return(nv);
    }
  return(run_warn("mus-set-srate: wrong type arg"));
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

static xen_value *clean_up_if_needed(xen_value *result, xen_value **args, int args_size, int need_result)
{
  if (need_result)
    return(clean_up(result, args, args_size));
  /* ok to skip return because we walked the args, so any side effects there are handled */
  return(make_xen_value(R_UNSPECIFIED, -1, R_CONSTANT));
}

static xen_value *walk(ptree *prog, XEN form, int need_result)
{
  /* walk form, storing vars, making program entries for operators etc */
  /* fprintf(stderr,"walk %s (needed: %d)\n", XEN_AS_STRING(form), need_result); */
  /* need_result = TRUE; */
  if (current_optimization == 0) return(NULL);

  if (XEN_LIST_P(form))
    {
      XEN function, all_args;
      char funcname[256];
      xen_value **args = NULL;
      int i, num_args, float_result = FALSE, constants = 0, booleans = 0, vcts = 0;
      int readers = 0, clms = 0, chars = 0, strings = 0, arg_result = NEED_ANY_RESULT;
      xen_var *var;
      xen_value *v = NULL;
      function = XEN_CAR(form);
      mus_snprintf(funcname, 256, "%s", XEN_AS_STRING(function)); /* protect from gc... */

      if (strcmp(funcname, "lambda") == 0) return(lambda_form(prog, form, TRUE));
      if (strcmp(funcname, "declare") == 0) return(make_xen_value(R_UNSPECIFIED, -1, TRUE));
      if (strcmp(funcname, "begin") == 0) return(begin_form(prog, form, need_result));
      if (strcmp(funcname, "if") == 0) return(if_form(prog, form, need_result));
      if (strcmp(funcname, "do") == 0) return(do_form(prog, form, need_result));
      if (strcmp(funcname, "do*") == 0) return(do_star_form(prog, form, need_result));
      if (strcmp(funcname, "cond") == 0) return(cond_form(prog, form, need_result));
      if (strcmp(funcname, "case") == 0) return(case_form(prog, form, need_result));
      if (strcmp(funcname, "or") == 0) return(or_form(prog, form));
      if (strcmp(funcname, "and") == 0) return(and_form(prog, form));
      if (strcmp(funcname, "let") == 0) return(let_form(prog, form, need_result));
      if (strcmp(funcname, "let*") == 0) return(let_star_form(prog, form, need_result));
      if (strcmp(funcname, "set!") == 0) return(set_form(prog, form, need_result));
      if (strcmp(funcname, "call/cc") == 0) return(callcc_form(prog, form, need_result));
      if (strcmp(funcname, "call-with-current-continuation") == 0) return(callcc_form(prog, form, need_result));
      if (strcmp(funcname, "quote") == 0)
	{
	  form = XEN_CADR(form);
	  if (XEN_BOOLEAN_P(form))
	    return(make_xen_value(R_BOOL, add_int_to_ptree(prog, (XEN_FALSE_P(form)) ? 0 : 1), R_CONSTANT));
	  if (XEN_NUMBER_P(form))
	    {
	      if ((XEN_EXACT_P(form)) && (XEN_INTEGER_P(form)))
		return(make_xen_value(R_INT, add_int_to_ptree(prog, XEN_TO_C_INT(form)), R_CONSTANT));
	      else return(make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, XEN_TO_C_DOUBLE(form)), R_CONSTANT));
	    }
	  if (XEN_CHAR_P(form)) 
	    return(make_xen_value(R_CHAR, add_int_to_ptree(prog, (int)(XEN_TO_C_CHAR(form))), R_CONSTANT));
	  if (XEN_STRING_P(form)) 
	    return(make_xen_value(R_STRING, add_string_to_ptree(prog, copy_string(XEN_TO_C_STRING(form))), R_CONSTANT));
	  return(run_warn("quote: non-simple arg: %s", XEN_AS_STRING(form)));
	}

      all_args = XEN_CDR(form);
      num_args = XEN_LIST_LENGTH(all_args);
      args = (xen_value **)CALLOC(num_args + 1, sizeof(xen_value *));
      if (num_args > 0)
	{
	  if ((strcmp(funcname, "inexact->exact") == 0) ||
	      (strcmp(funcname, "modulo") == 0)) /* others here are remainder quotient ash logand logior logxor lognot gcd lcm integer->char */
	    arg_result = NEED_INT_RESULT;
	  for (i = 0; i < num_args; i++, all_args = XEN_CDR(all_args))
	    {
	      args[i + 1] = walk(prog, XEN_CAR(all_args), arg_result);
	      if (args[i + 1] == NULL) return(clean_up(NULL, args, num_args));
	      if (args[i + 1]->constant == R_CONSTANT) constants++;
	      switch (args[i + 1]->type)
		{
		case R_FLOAT:   float_result = TRUE; break;
		case R_BOOL:    booleans++;          break;
		case R_VCT:     vcts++;              break;
		case R_READER:  readers++;           break;
		case R_CLM:     clms++;              break;
		case R_CHAR:    chars++;             break;
		case R_STRING:  strings++;           break;
		}
	    }
	}

      /* check user-defined stuff */
      var = find_var_in_ptree(prog, funcname);
      if (var == NULL)
	{
	  XEN val = XEN_UNDEFINED;
	  XEN nval;
	  nval = scm_sym2var(function, scm_current_module_lookup_closure(), XEN_FALSE);
	  if (!(XEN_FALSE_P(nval))) val = XEN_VARIABLE_REF(nval);
	  if ((sf_p(val)) || (mus_xen_p(val)))
	    v = add_global_var_to_ptree(prog, function);

	  /* if we had a way to handle arbitrary lambda args, this might pick up user func:
	   *	   if (XEN_PROCEDURE_P(val))
	   *         fprintf(stderr,"%s: %s\n", funcname, XEN_AS_STRING(scm_procedure_source(val)));
	   */
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
      /* not user-defined (possibly implicit) function */

      /* type-independent funcs first */
      if (strcmp(funcname, "number->string") == 0) return(clean_up(number2string_1(prog, args, num_args, constants), args, num_args));
      if (strcmp(funcname, "display") == 0) return(clean_up(display_1(prog, args, num_args), args, num_args));

      if (num_args == 2)
	{
	  if (strcmp(funcname, "eq?") == 0) return(clean_up(eq_p(prog, args, constants), args, num_args));
	  if (strcmp(funcname, "eqv?") == 0) return(clean_up(eqv_p(prog, args, constants), args, num_args));
	  if (strcmp(funcname, "equal?") == 0) return(clean_up(equal_p(prog, args, constants), args, num_args));
	}
      if (num_args == 1)
	{
	  /* these are known in advance in this context */
	  if (strcmp(funcname, "boolean?") == 0) 
	    return(clean_up(make_xen_value(R_BOOL, add_int_to_ptree(prog, args[1]->type == R_BOOL), R_CONSTANT), args, num_args));
	  if (strcmp(funcname, "number?") == 0) 
	    return(clean_up(make_xen_value(R_BOOL, 
					   add_int_to_ptree(prog, (args[1]->type == R_INT) || (args[1]->type == R_FLOAT)), 
					   R_CONSTANT), 
			    args, num_args));
	  if (strcmp(funcname, "integer?") == 0) 
	    return(clean_up(make_xen_value(R_BOOL, add_int_to_ptree(prog, args[1]->type == R_INT), R_CONSTANT), args, num_args));
	  if (strcmp(funcname, "real?") == 0) 
	    return(clean_up(make_xen_value(R_BOOL, 
					   add_int_to_ptree(prog, (args[1]->type == R_INT) || (args[1]->type == R_FLOAT)), 
					   R_CONSTANT), 
			    args, num_args));
	  if (strcmp(funcname, "exact?") == 0) 
	    return(clean_up(make_xen_value(R_BOOL, add_int_to_ptree(prog, args[1]->type == R_INT), R_CONSTANT), args, num_args));
	  if (strcmp(funcname, "inexact?") == 0) 
	    return(clean_up(make_xen_value(R_BOOL, add_int_to_ptree(prog, args[1]->type == R_FLOAT), R_CONSTANT), args, num_args));
	  if (strcmp(funcname, "char?") == 0) 
	    return(clean_up(make_xen_value(R_BOOL, add_int_to_ptree(prog, args[1]->type == R_CHAR), R_CONSTANT), args, num_args));
	  if (strcmp(funcname, "string?") == 0) 
	    return(clean_up(make_xen_value(R_BOOL, add_int_to_ptree(prog, args[1]->type == R_STRING), R_CONSTANT), args, num_args));
	  
	  if (strcmp(funcname, "vct?") == 0)
	    return(clean_up(make_xen_value(R_BOOL, add_int_to_ptree(prog, args[1]->type == R_VCT), R_CONSTANT), args, num_args));
	  if (strcmp(funcname, "vector?") == 0)
	    return(clean_up(make_xen_value(R_BOOL, 
					   add_int_to_ptree(prog, VECTOR_P(args[1]->type)),
					   R_CONSTANT), 
			    args, num_args));
	  if (strcmp(funcname, "sample-reader?") == 0) 
	    return(clean_up(make_xen_value(R_BOOL, add_int_to_ptree(prog, args[1]->type == R_READER), R_CONSTANT), args, num_args));
	}
      if (strcmp(funcname, "make-string") == 0) return(clean_up(make_string_1(prog, args, num_args), args, num_args));
      if (chars > 0)
	{
	  /* char-related funcs */
	  if (num_args == 1)
	    {
	      if (strcmp(funcname, "char-alphabetic?") == 0) 
		return(clean_up(char_alphabetic_p(prog, args, constants), args, num_args));
	      if (strcmp(funcname, "char-numeric?") == 0) 
		return(clean_up(char_numeric_p(prog, args, constants), args, num_args));
	      if (strcmp(funcname, "char-lower-case?") == 0) 
		return(clean_up(char_lower_case_p(prog, args, constants), args, num_args));
	      if (strcmp(funcname, "char-upper-case?") == 0) 
		return(clean_up(char_upper_case_p(prog, args, constants), args, num_args));
	      if (strcmp(funcname, "char-whitespace?") == 0) 
		return(clean_up(char_whitespace_p(prog, args, constants), args, num_args));
	      if (strcmp(funcname, "char-upcase") == 0) 
		return(clean_up(char_upcase(prog, args, constants), args, num_args));
	      if (strcmp(funcname, "char-downcase") == 0) 
		return(clean_up(char_downcase(prog, args, constants), args, num_args));
	      if (strcmp(funcname, "char->integer") == 0) 
		return(clean_up(char_to_integer(args[1]), args, num_args));
	    }
	  if (chars == num_args)
	    {
	      if (strcmp(funcname, "char>=?") == 0) return(clean_up(greater_than_or_equal(prog, FALSE, args, num_args, constants), args, num_args));
	      if (strcmp(funcname, "char>?") == 0) return(clean_up(greater_than(prog, FALSE, args, num_args, constants), args, num_args));
	      if (strcmp(funcname, "char<=?") == 0) return(clean_up(less_than_or_equal(prog, FALSE, args, num_args, constants), args, num_args));
	      if (strcmp(funcname, "char<?") == 0) return(clean_up(less_than(prog, FALSE, args, num_args, constants), args, num_args));
	      if (strcmp(funcname, "char=?") == 0) return(clean_up(numbers_equal(prog, FALSE, args, num_args, constants), args, num_args));
	      /* ci cases convert-to-upper on all args */
	      if (strcmp(funcname, "char-ci>=?") == 0) 
		return(clean_up(greater_than_or_equal(prog, FALSE, upcase_args(prog, args, num_args), num_args, constants), args, num_args));
	      if (strcmp(funcname, "char-ci>?") == 0) 
		return(clean_up(greater_than(prog, FALSE, upcase_args(prog, args, num_args), num_args, constants), args, num_args));
	      if (strcmp(funcname, "char-ci<=?") == 0) 
		return(clean_up(less_than_or_equal(prog, FALSE, upcase_args(prog, args, num_args), num_args, constants), args, num_args));
	      if (strcmp(funcname, "char-ci<?") == 0) 
		return(clean_up(less_than(prog, FALSE, upcase_args(prog, args, num_args), num_args, constants), args, num_args));
	      if (strcmp(funcname, "char-ci=?") == 0) 
		return(clean_up(numbers_equal(prog, FALSE, upcase_args(prog, args, num_args), num_args, constants), args, num_args));
	      if (strcmp(funcname, "string") == 0) 
		return(clean_up(string_1(prog, args, num_args, constants), args, num_args));
	    }
	  if (num_args == 2)
	    {
	      if (strcmp(funcname, "string-fill!") == 0) return(clean_up(string_fill_1(prog, args), args, num_args));
	    }
	  if (num_args == 3)
	    {
	      if (strcmp(funcname, "string-set!") == 0) return(clean_up(string_set_1(prog, args), args, num_args));
	    }
	  return(clean_up(run_warn("bad character in unsavory place"), args, num_args));
	}
      /* no chars from here on */

      if (strings == num_args)
	{
	  if (strcmp(funcname, "mus-sound-samples") == 0) return(clean_up(mus_sound_samples_1(prog, args), args, num_args));
	  if (strcmp(funcname, "mus-sound-frames") == 0) return(clean_up(mus_sound_frames_1(prog, args), args, num_args));
	  if (strcmp(funcname, "mus-sound-datum-size") == 0) return(clean_up(mus_sound_datum_size_1(prog, args), args, num_args));
	  if (strcmp(funcname, "mus-sound-data-location") == 0) return(clean_up(mus_sound_data_location_1(prog, args), args, num_args));
	  if (strcmp(funcname, "mus-sound-chans") == 0) return(clean_up(mus_sound_chans_1(prog, args), args, num_args));
	  if (strcmp(funcname, "mus-sound-srate") == 0) return(clean_up(mus_sound_srate_1(prog, args), args, num_args));
	  if (strcmp(funcname, "mus-sound-header-type") == 0) return(clean_up(mus_sound_header_type_1(prog, args), args, num_args));
	  if (strcmp(funcname, "mus-sound-data-format") == 0) return(clean_up(mus_sound_data_format_1(prog, args), args, num_args));
	  if (strcmp(funcname, "mus-sound-length") == 0) return(clean_up(mus_sound_length_1(prog, args), args, num_args));
	  if (strcmp(funcname, "mus-sound-duration") == 0) return(clean_up(mus_sound_duration_1(prog, args), args, num_args));
	  if (strcmp(funcname, "mus-sound-comment") == 0) return(clean_up(mus_sound_comment_1(prog, args), args, num_args));

	  if (strcmp(funcname, "string-append") == 0) return(clean_up(string_append_1(prog, args, num_args, constants), args, num_args));
	  if (strcmp(funcname, "string=?") == 0) return(clean_up(string_eq_1(prog, args, num_args, constants), args, num_args));
	  if (strcmp(funcname, "string>=?") == 0) return(clean_up(string_geq_1(prog, args, num_args, constants), args, num_args));
	  if (strcmp(funcname, "string<=?") == 0) return(clean_up(string_leq_1(prog, args, num_args, constants), args, num_args));
	  if (strcmp(funcname, "string>?") == 0) return(clean_up(string_gt_1(prog, args, num_args, constants), args, num_args));
	  if (strcmp(funcname, "string<?") == 0) return(clean_up(string_lt_1(prog, args, num_args, constants), args, num_args));
	  if (strcmp(funcname, "string-ci=?") == 0) return(clean_up(string_ci_eq_1(prog, args, num_args, constants), args, num_args));
	  if (strcmp(funcname, "string-ci>=?") == 0) return(clean_up(string_ci_geq_1(prog, args, num_args, constants), args, num_args));
	  if (strcmp(funcname, "string-ci<=?") == 0) return(clean_up(string_ci_leq_1(prog, args, num_args, constants), args, num_args));
	  if (strcmp(funcname, "string-ci>?") == 0) return(clean_up(string_ci_gt_1(prog, args, num_args, constants), args, num_args));
	  if (strcmp(funcname, "string-ci<?") == 0) return(clean_up(string_ci_lt_1(prog, args, num_args, constants), args, num_args));
	}
      if (strings > 0)
	{
	  if (num_args == 1)
	    {
	      if (strcmp(funcname, "string-length") == 0) return(clean_up(string_length_1(prog, args), args, num_args));
	      if (strcmp(funcname, "string-copy") == 0) return(clean_up(string_copy_1(prog, args), args, num_args));
	    }
	  if (strcmp(funcname, "snd-print") == 0) return(clean_up(snd_print_1(prog, args, num_args), args, num_args));
	  if (num_args == 2)
	    {
	      if (strcmp(funcname, "string-ref") == 0) return(clean_up(string_ref_1(prog, args), args, num_args));
	    }
	  if (strcmp(funcname, "substring") == 0) return(clean_up(substring_1(prog, args, num_args, constants), args, num_args));
	  return(clean_up(run_warn("bad string arg"), args, num_args));
	}
      /* no string args from here on (except 0-arg (string)) */

      if (strcmp(funcname, "polynomial") == 0) return(clean_up(polynomial_1(prog, args, num_args), args, num_args));
      if (strcmp(funcname, "mus-fft") == 0) return(clean_up(mus_fft_1(prog, args, num_args), args, num_args));
      if (strcmp(funcname, "array-interp") == 0) return(clean_up(array_interp_1(prog, args, num_args), args, num_args));
      if (strcmp(funcname, "clear-array") == 0) return(clean_up(clear_array_1(prog, args, num_args), args, num_args));
      if (strcmp(funcname, "contrast-enhancement") == 0) return(clean_up(contrast_enhancement_1(prog, args, num_args), args, num_args));
      if (strcmp(funcname, "ring-modulate") == 0) return(clean_up(ring_modulate_1(prog, args, num_args), args, num_args));
      if (strcmp(funcname, "amplitude-modulate") == 0) return(clean_up(amplitude_modulate_1(prog, args, num_args), args, num_args));
      if (strcmp(funcname, "convolution") == 0) return(clean_up(convolution_1(prog, args, num_args), args, num_args));
      if (strcmp(funcname, "multiply-arrays") == 0) return(clean_up(multiply_arrays_1(prog, args, num_args), args, num_args));
      if (strcmp(funcname, "rectangular->polar") == 0) return(clean_up(rectangular2polar_1(prog, args, num_args), args, num_args));
      if (strcmp(funcname, "polar->rectangular") == 0) return(clean_up(polar2rectangular_1(prog, args, num_args), args, num_args));
      if (strcmp(funcname, "sum-of-sines") == 0) return(clean_up(sum_of_sines_1(prog, args, num_args), args, num_args));
      if (strcmp(funcname, "dot-product") == 0) return(clean_up(dot_product_1(prog, args, num_args), args, num_args));
      if (strcmp(funcname, "formant-bank") == 0) return(clean_up(formant_bank_1(prog, args, num_args), args, num_args));
      if (strcmp(funcname, "frame+") == 0) return(clean_up(mus_frame_add_1(prog, args, num_args), args, num_args));
      if (strcmp(funcname, "frame*") == 0) return(clean_up(mus_frame_multiply_1(prog, args, num_args), args, num_args));
      if (strcmp(funcname, "frame->frame") == 0) return(clean_up(mus_frame2frame_1(prog, args, num_args), args, num_args));
      if (strcmp(funcname, "frame->sample") == 0) return(clean_up(frame2sample_1(prog, args, num_args), args, num_args));
      if (strcmp(funcname, "frame->buffer") == 0) return(clean_up(frame2buffer_1(prog, args, num_args), args, num_args));
      if (strcmp(funcname, "buffer->frame") == 0) return(clean_up(buffer2frame_1(prog, args, num_args), args, num_args));
      if (strcmp(funcname, "mixer*") == 0) return(clean_up(mus_mixer_multiply_1(prog, args, num_args), args, num_args));
      if (strcmp(funcname, "mus-srate") == 0) return(clean_up(mus_srate_1(prog, args, num_args), args, num_args));
      if (strcmp(funcname, "sample->frame") == 0) return(clean_up(sample2frame_1(prog, args, num_args), args, num_args));
      if (strcmp(funcname, "frame->file") == 0) return(clean_up(frame2file_1(prog, args, num_args), args, num_args));
      if (strcmp(funcname, "file->frame") == 0) return(clean_up(file2frame_1(prog, args, num_args), args, num_args));
      if (strcmp(funcname, "locsig") == 0) return(clean_up(locsig_1(prog, args, num_args), args, num_args));
      if (strcmp(funcname, "spectrum") == 0) return(clean_up(mus_spectrum_1(prog, args, num_args), args, num_args));

      if ((clms == 1) || (booleans == 1))
	/* boolean for gen that is null in the current context */
	{
	  if (strcmp(funcname, "oscil") == 0) return(clean_up(oscil_1(prog, args, num_args), args, num_args));
	  if (strcmp(funcname, "notch") == 0) return(clean_up(notch_1(prog, args, num_args), args, num_args));
	  if (strcmp(funcname, "comb") == 0) return(clean_up(comb_1(prog, args, num_args), args, num_args));
	  if (strcmp(funcname, "delay") == 0) return(clean_up(delay_1(prog, args, num_args), args, num_args));
	  if (strcmp(funcname, "all-pass") == 0) return(clean_up(all_pass_1(prog, args, num_args), args, num_args));
	  if (strcmp(funcname, "rand") == 0) return(clean_up(rand_1(prog, args, num_args), args, num_args));
	  if (strcmp(funcname, "rand-interp") == 0) return(clean_up(rand_interp_1(prog, args, num_args), args, num_args));
	  if (strcmp(funcname, "sum-of-cosines") == 0) return(clean_up(sum_of_cosines_1(prog, args, num_args), args, num_args));
	  if (strcmp(funcname, "table-lookup") == 0) return(clean_up(table_lookup_1(prog, args, num_args), args, num_args));
	  if (strcmp(funcname, "sawtooth-wave") == 0) return(clean_up(sawtooth_wave_1(prog, args, num_args), args, num_args));
	  if (strcmp(funcname, "pulse-train") == 0) return(clean_up(pulse_train_1(prog, args, num_args), args, num_args));
	  if (strcmp(funcname, "square-wave") == 0) return(clean_up(square_wave_1(prog, args, num_args), args, num_args));
	  if (strcmp(funcname, "triangle-wave") == 0) return(clean_up(triangle_wave_1(prog, args, num_args), args, num_args));
	  if (strcmp(funcname, "asymmetric-fm") == 0) return(clean_up(asymmetric_fm_1(prog, args, num_args), args, num_args));
	  if (strcmp(funcname, "sine-summation") == 0) return(clean_up(sine_summation_1(prog, args, num_args), args, num_args));
	  if (strcmp(funcname, "one-zero") == 0) return(clean_up(one_zero_1(prog, args, num_args), args, num_args));
	  if (strcmp(funcname, "one-pole") == 0) return(clean_up(one_pole_1(prog, args, num_args), args, num_args));
	  if (strcmp(funcname, "two-zero") == 0) return(clean_up(two_zero_1(prog, args, num_args), args, num_args));
	  if (strcmp(funcname, "two-pole") == 0) return(clean_up(two_pole_1(prog, args, num_args), args, num_args));
	  if (strcmp(funcname, "formant") == 0) return(clean_up(formant_1(prog, args, num_args), args, num_args));
	  if (strcmp(funcname, "wave-train") == 0) return(clean_up(wave_train_1(prog, args, num_args), args, num_args));
	  if (strcmp(funcname, "waveshape") == 0) return(clean_up(waveshape_1(prog, args, num_args), args, num_args));
	  if (strcmp(funcname, "filter") == 0) return(clean_up(filter_1(prog, args, num_args), args, num_args));
	  if (strcmp(funcname, "fir-filter") == 0) return(clean_up(fir_filter_1(prog, args, num_args), args, num_args));
	  if (strcmp(funcname, "iir-filter") == 0) return(clean_up(iir_filter_1(prog, args, num_args), args, num_args));
	  if (strcmp(funcname, "readin") == 0) return(clean_up(readin_1(prog, args, num_args), args, num_args));
	  if (strcmp(funcname, "env-interp") == 0) return(clean_up(env_interp_1(prog, args, num_args), args, num_args));

	  if (strcmp(funcname, "src") == 0) return(clean_up(src_1(prog, args, num_args), args, num_args));
	  if (strcmp(funcname, "granulate") == 0) return(clean_up(granulate_1(prog, args, num_args), args, num_args));
	  if (strcmp(funcname, "phase-vocoder") == 0) return(clean_up(phase_vocoder_1(prog, args, num_args), args, num_args));
	  if (strcmp(funcname, "convolve") == 0) return(clean_up(convolve_1(prog, args, num_args), args, num_args));

	  if (strcmp(funcname, "tap") == 0) return(clean_up(tap_1(prog, args, num_args), args, num_args));
	  if (strcmp(funcname, "file->sample") == 0) return(clean_up(file2sample_1(prog, args, num_args), args, num_args));
	  if (strcmp(funcname, "sample->file") == 0) return(clean_up(sample2file_1(prog, args, num_args), args, num_args));
	  if (strcmp(funcname, "sample->buffer") == 0) return(clean_up(sample2buffer_1(prog, args, num_args), args, num_args));
	  if (strcmp(funcname, "ina") == 0) return(clean_up(ina_1(prog, args, num_args), args, num_args));
	  if (strcmp(funcname, "inb") == 0) return(clean_up(inb_1(prog, args, num_args), args, num_args));
	  if (strcmp(funcname, "in-any") == 0) return(clean_up(in_any_1(prog, args, num_args), args, num_args));
	  if (strcmp(funcname, "out-any") == 0) return(clean_up(out_any_1(prog, args, num_args), args, num_args));
	  if (strcmp(funcname, "outa") == 0) return(clean_up(outa_1(prog, args, num_args), args, num_args));
	  if (strcmp(funcname, "outb") == 0) return(clean_up(outb_1(prog, args, num_args), args, num_args));
	  if (strcmp(funcname, "outc") == 0) return(clean_up(outc_1(prog, args, num_args), args, num_args));
	  if (strcmp(funcname, "outd") == 0) return(clean_up(outd_1(prog, args, num_args), args, num_args));
	  if (strcmp(funcname, "move-locsig") == 0) return(clean_up(move_locsig_1(prog, args, num_args), args, num_args));
	  if (strcmp(funcname, "mus-set-formant-radius-and-frequency") == 0) 
	    return(clean_up(set_formant_radius_and_frequency_1(prog, args, num_args), args, num_args));

	  if (num_args == 1)
	    {
	      if (strcmp(funcname, "buffer->sample") == 0) return(clean_up(mus_buffer2sample_0(prog, args), args, num_args));
	      /* should these accept any arg? */
	      if (strcmp(funcname, "env") == 0) return(clean_up(env_1(prog, args, num_args), args, num_args));
	      if (strcmp(funcname, "oscil?") == 0) return(clean_up(oscil_p(prog, args), args, num_args));
	      if (strcmp(funcname, "env?") == 0) return(clean_up(env_p(prog, args), args, num_args));
	      if (strcmp(funcname, "notch?") == 0) return(clean_up(notch_p(prog, args), args, num_args));
	      if (strcmp(funcname, "comb?") == 0) return(clean_up(comb_p(prog, args), args, num_args));
	      if (strcmp(funcname, "delay?") == 0) return(clean_up(delay_p(prog, args), args, num_args));
	      if (strcmp(funcname, "all-pass?") == 0) return(clean_up(all_pass_p(prog, args), args, num_args));
	      if (strcmp(funcname, "rand?") == 0) return(clean_up(rand_p(prog, args), args, num_args));
	      if (strcmp(funcname, "rand-interp?") == 0) return(clean_up(rand_interp_p(prog, args), args, num_args));
	      if (strcmp(funcname, "sum-of-cosines?") == 0) return(clean_up(sum_of_cosines_p(prog, args), args, num_args));
	      if (strcmp(funcname, "table-lookup?") == 0) return(clean_up(table_lookup_p(prog, args), args, num_args));
	      if (strcmp(funcname, "sawtooth-wave?") == 0) return(clean_up(sawtooth_wave_p(prog, args), args, num_args));
	      if (strcmp(funcname, "pulse-train?") == 0) return(clean_up(pulse_train_p(prog, args), args, num_args));
	      if (strcmp(funcname, "square-wave?") == 0) return(clean_up(square_wave_p(prog, args), args, num_args));
	      if (strcmp(funcname, "triangle-wave?") == 0) return(clean_up(triangle_wave_p(prog, args), args, num_args));
	      if (strcmp(funcname, "asymmetric-fm?") == 0) return(clean_up(asymmetric_fm_p(prog, args), args, num_args));
	      if (strcmp(funcname, "sine-summation?") == 0) return(clean_up(sine_summation_p(prog, args), args, num_args));
	      if (strcmp(funcname, "one-zero?") == 0) return(clean_up(one_zero_p(prog, args), args, num_args));
	      if (strcmp(funcname, "one-pole?") == 0) return(clean_up(one_pole_p(prog, args), args, num_args));
	      if (strcmp(funcname, "two-zero?") == 0) return(clean_up(two_zero_p(prog, args), args, num_args));
	      if (strcmp(funcname, "two-pole?") == 0) return(clean_up(two_pole_p(prog, args), args, num_args));
	      if (strcmp(funcname, "formant?") == 0) return(clean_up(formant_p(prog, args), args, num_args));
	      if (strcmp(funcname, "wave-train?") == 0) return(clean_up(wave_train_p(prog, args), args, num_args));
	      if (strcmp(funcname, "waveshape?") == 0) return(clean_up(waveshape_p(prog, args), args, num_args));
	      if (strcmp(funcname, "filter?") == 0) return(clean_up(filter_p(prog, args), args, num_args));
	      if (strcmp(funcname, "fir-filter?") == 0) return(clean_up(fir_filter_p(prog, args), args, num_args));
	      if (strcmp(funcname, "iir-filter?") == 0) return(clean_up(iir_filter_p(prog, args), args, num_args));
	      if (strcmp(funcname, "readin?") == 0) return(clean_up(readin_p(prog, args), args, num_args));
	      if (strcmp(funcname, "src?") == 0) return(clean_up(src_p(prog, args), args, num_args));
	      if (strcmp(funcname, "granulate?") == 0) return(clean_up(granulate_p(prog, args), args, num_args));
	      if (strcmp(funcname, "phase-vocoder?") == 0) return(clean_up(phase_vocoder_p(prog, args), args, num_args));
	      if (strcmp(funcname, "convolve?") == 0) return(clean_up(convolve_p(prog, args), args, num_args));
	      if (strcmp(funcname, "buffer?") == 0) return(clean_up(buffer_p(prog, args), args, num_args));
	      if (strcmp(funcname, "buffer-empty?") == 0) return(clean_up(buffer_empty_p(prog, args), args, num_args));
	      if (strcmp(funcname, "buffer-full?") == 0) return(clean_up(buffer_full_p(prog, args), args, num_args));
	      if (strcmp(funcname, "frame?") == 0) return(clean_up(frame_p(prog, args), args, num_args));
	      if (strcmp(funcname, "mixer?") == 0) return(clean_up(mixer_p(prog, args), args, num_args));
	      if (strcmp(funcname, "file->sample?") == 0) return(clean_up(file2sample_p(prog, args), args, num_args));
	      if (strcmp(funcname, "sample->file?") == 0) return(clean_up(sample2file_p(prog, args), args, num_args));
	      if (strcmp(funcname, "file->frame?") == 0) return(clean_up(file2frame_p(prog, args), args, num_args));
	      if (strcmp(funcname, "frame->file?") == 0) return(clean_up(frame2file_p(prog, args), args, num_args));
	      if (strcmp(funcname, "locsig?") == 0) return(clean_up(locsig_p(prog, args), args, num_args));
	      if (strcmp(funcname, "mus-input?") == 0) return(clean_up(input_p(prog, args), args, num_args));
	      if (strcmp(funcname, "mus-output?") == 0) return(clean_up(output_p(prog, args), args, num_args));

	      if (strcmp(funcname, "restart-env") == 0) return(clean_up(restart_env_1(prog, args), args, num_args));
	      if (strcmp(funcname, "mus-increment") == 0) return(clean_up(mus_increment_0(prog, args), args, num_args));
	      if (strcmp(funcname, "mus-frequency") == 0) return(clean_up(mus_frequency_0(prog, args), args, num_args));
	      if (strcmp(funcname, "mus-phase") == 0) return(clean_up(mus_phase_0(prog, args), args, num_args));
	      if (strcmp(funcname, "mus-scaler") == 0) return(clean_up(mus_scaler_0(prog, args), args, num_args));
	      if (strcmp(funcname, "mus-formant-radius") == 0) return(clean_up(mus_formant_radius_0(prog, args), args, num_args));
	      if (strcmp(funcname, "mus-a0") == 0) return(clean_up(mus_a0_0(prog, args), args, num_args));
	      if (strcmp(funcname, "mus-a1") == 0) return(clean_up(mus_a1_0(prog, args), args, num_args));
	      if (strcmp(funcname, "mus-a2") == 0) return(clean_up(mus_a2_0(prog, args), args, num_args));
	      if (strcmp(funcname, "mus-b1") == 0) return(clean_up(mus_b1_0(prog, args), args, num_args));
	      if (strcmp(funcname, "mus-b2") == 0) return(clean_up(mus_b2_0(prog, args), args, num_args));
	      if (strcmp(funcname, "mus-feedforward") == 0) return(clean_up(mus_feedforward_0(prog, args), args, num_args));
	      if (strcmp(funcname, "mus-feedback") == 0) return(clean_up(mus_feedback_0(prog, args), args, num_args));
	      
	      if (strcmp(funcname, "mus-hop") == 0) return(clean_up(mus_hop_0(prog, args), args, num_args));
	      if (strcmp(funcname, "mus-channels") == 0) return(clean_up(mus_channels_0(prog, args), args, num_args));
	      if (strcmp(funcname, "mus-channel") == 0) return(clean_up(mus_channel_0(prog, args), args, num_args));
	      if (strcmp(funcname, "mus-location") == 0) return(clean_up(mus_location_0(prog, args), args, num_args));
	      if (strcmp(funcname, "mus-ramp") == 0) return(clean_up(mus_ramp_0(prog, args), args, num_args));
	      if (strcmp(funcname, "mus-position") == 0) return(clean_up(mus_position_0(prog, args), args, num_args));
	      if (strcmp(funcname, "mus-order") == 0) return(clean_up(mus_order_0(prog, args), args, num_args));
	      if (strcmp(funcname, "mus-length") == 0) return(clean_up(mus_length_0(prog, args), args, num_args));
	      if (strcmp(funcname, "mus-cosines") == 0) return(clean_up(mus_cosines_0(prog, args), args, num_args));
	      
	      if (strcmp(funcname, "mus-name") == 0) return(clean_up(mus_name_0(prog, args), args, num_args));
	      if (strcmp(funcname, "mus-describe") == 0) return(clean_up(mus_describe_0(prog, args), args, num_args));
	      if (strcmp(funcname, "mus-inspect") == 0) return(clean_up(mus_inspect_0(prog, args), args, num_args));

	      /* mus-xcoeffs/ycoeffs/data return the bare float array whereas we need the vct in this context */
	    }

	  if (strcmp(funcname, "locsig-ref") == 0) return(clean_up(locsig_ref_0(prog, args, num_args), args, num_args));
	  if (strcmp(funcname, "frame-ref") == 0) return(clean_up(frame_ref_0(prog, args, num_args), args, num_args));
	  if (strcmp(funcname, "locsig-reverb-ref") == 0) return(clean_up(locsig_reverb_ref_0(prog, args, num_args), args, num_args));
	  if (strcmp(funcname, "locsig-set!") == 0) return(clean_up(locsig_set_0(prog, args, num_args), args, num_args));
	  if (strcmp(funcname, "frame-set!") == 0) return(clean_up(frame_set_0(prog, args, num_args), args, num_args));
	  if (strcmp(funcname, "locsig-reverb-set!") == 0) return(clean_up(locsig_reverb_set_0(prog, args, num_args), args, num_args));
	  if (strcmp(funcname, "mixer-set!") == 0) return(clean_up(mixer_set_1(prog, args, num_args), args, num_args));
	  if (strcmp(funcname, "mixer-ref") == 0) return(clean_up(mixer_ref_1(prog, args, num_args), args, num_args));
	}
      if (num_args == 3)
	{
	  if (strcmp(funcname, "vct-set!") == 0) return(clean_up(vct_set_1(prog, args), args, num_args));
	}
      if (strcmp(funcname, "make-sample-reader") == 0) return(clean_up(make_sample_reader_1(prog, args, num_args), args, num_args));
      if ((num_args == 1) && ((readers == 1)))
	{
	  if (strcmp(funcname, "next-sample") == 0) return(clean_up(next_sample_1(prog, args), args, num_args));
	  if (strcmp(funcname, "previous-sample") == 0) return(clean_up(previous_sample_1(prog, args), args, num_args));
	  if (strcmp(funcname, "read-sample") == 0) return(clean_up(reader_1(prog, args), args, num_args));
	}
      if (readers > 0) return(clean_up(run_warn("reader bad arg"), args, num_args));
      if (clms > 0) return(clean_up(run_warn("clm gen bad arg"), args, num_args));
      /* both of these can be applicable objects, but those are not counted in the arg scan */
      /* no readers or CLM gens from here on */
      if (strcmp(funcname, "edit-position") == 0) return(clean_up(edit_position_1(prog, args, num_args), args, num_args));
      /* TODO: frames, tap with 1 arg */
      if (vcts > 0)
	{
	  if (strcmp(funcname, "vct-ref") == 0) return(clean_up(vct_ref_1(prog, args, num_args), args, num_args));
	  if (strcmp(funcname, "vct-length") == 0) return(clean_up(vct_length_1(prog, args, num_args), args, num_args));
	  if (strcmp(funcname, "vct-fill!") == 0) return(clean_up(vct_fill_1(prog, args, num_args), args, num_args));
	  if (strcmp(funcname, "vct-scale!") == 0) return(clean_up(vct_scale_1(prog, args, num_args), args, num_args));
	  if (strcmp(funcname, "vct-offset!") == 0) return(clean_up(vct_offset_1(prog, args, num_args), args, num_args));
	  if (strcmp(funcname, "vct-add!") == 0) return(clean_up(vct_add_1(prog, args, num_args), args, num_args));
	  if (strcmp(funcname, "vct-subtract!") == 0) return(clean_up(vct_subtract_1(prog, args, num_args), args, num_args));
	  if (strcmp(funcname, "vct-multiply!") == 0) return(clean_up(vct_multiply_1(prog, args, num_args), args, num_args));
	  if (strcmp(funcname, "vct-copy") == 0) return(clean_up(vct_copy_1(prog, args, num_args), args, num_args));
	  if (strcmp(funcname, "vct-peak") == 0) return(clean_up(vct_peak_1(prog, args, num_args), args, num_args));
	}
      /* no vcts from here on (except make-vct) */
      if (strcmp(funcname, "vector-ref") == 0) return(clean_up(vector_ref_1(prog, args, num_args), args, num_args));
      if (strcmp(funcname, "vector-length") == 0) return(clean_up(vector_length_1(prog, args, num_args), args, num_args));
      if (strcmp(funcname, "vector-fill!") == 0) return(clean_up(vector_fill_1(prog, args, num_args), args, num_args));
      if (strcmp(funcname, "vector-set!") == 0) return(clean_up(vector_set_1(prog, args, num_args), args, num_args));
      if ((strcmp(funcname, "make-vector") == 0) && 
	  (num_args == 2) &&
	  (args[2]->type == R_FLOAT))
	return(clean_up(make_vct_1(prog, args, num_args), args, num_args));

      if (num_args == 0)
	{
	  if (strcmp(funcname, "string") == 0)
	    return(clean_up_if_needed(make_xen_value(R_STRING, 
						     add_string_to_ptree(prog, (char *)CALLOC(1, sizeof(char))), 
						     R_CONSTANT), 
				      args, num_args, need_result));
	}
      if (booleans == 0)
	{
	  if (strcmp(funcname, "*") == 0) return(clean_up(multiply(prog, float_result, args, num_args, constants), args, num_args));
	  if (strcmp(funcname, "+") == 0) return(clean_up(add(prog, float_result, args, num_args, constants), args, num_args));
	  if (strcmp(funcname, ">") == 0) return(clean_up(greater_than(prog, float_result, args, num_args, constants), args, num_args));
	  if (strcmp(funcname, ">=") == 0) return(clean_up(greater_than_or_equal(prog, float_result, args, num_args, constants), args, num_args));
	  if (strcmp(funcname, "<") == 0) return(clean_up(less_than(prog, float_result, args, num_args, constants), args, num_args));
	  if (strcmp(funcname, "<=") == 0) return(clean_up(less_than_or_equal(prog, float_result, args, num_args, constants), args, num_args));
	  if (strcmp(funcname, "=") == 0) return(clean_up(numbers_equal(prog, float_result, args, num_args, constants), args, num_args));
	  if (strcmp(funcname, "gcd") == 0) return(clean_up(gcd_1(prog, args, constants, num_args), args, num_args));
	  if (strcmp(funcname, "lcm") == 0) return(clean_up(lcm_1(prog, args, constants, num_args), args, num_args));
	  if (num_args > 0)
	    {
	      if (strcmp(funcname, "-") == 0) return(clean_up(subtract(prog, float_result, args, num_args, constants), args, num_args));
	      if (strcmp(funcname, "/") == 0) return(clean_up(divide(prog, args, num_args, constants), args, num_args));
	      if (strcmp(funcname, "max") == 0) return(clean_up(max_1(prog, float_result, args, num_args, constants), args, num_args));
	      if (strcmp(funcname, "min") == 0) return(clean_up(min_1(prog, float_result, args, num_args, constants), args, num_args));
	    }
	  if (num_args == 2)
	    {
	      if (strcmp(funcname, "modulo") == 0) return(clean_up(modulo_1(prog, args, constants), args, num_args));
	      if (strcmp(funcname, "remainder") == 0) return(clean_up(remainder_1(prog, args, constants), args, num_args));
	      if (strcmp(funcname, "quotient") == 0) return(clean_up(quotient_1(prog, args, constants), args, num_args));
	      if (strcmp(funcname, "logand") == 0) return(clean_up(logand_1(prog, args, constants), args, num_args));
	      if (strcmp(funcname, "logxor") == 0) return(clean_up(logxor_1(prog, args, constants), args, num_args));
	      if (strcmp(funcname, "logior") == 0) return(clean_up(logior_1(prog, args, constants), args, num_args));
	      if (strcmp(funcname, "ash") == 0) return(clean_up(ash_1(prog, args, constants), args, num_args));
	      if (current_optimization > 1) 
		{
		  if (strcmp(funcname, "expt") == 0) return(clean_up(expt_1(prog, args, constants), args, num_args));
		  if (strcmp(funcname, "atan") == 0) return(clean_up(atan2_1(prog, args, constants), args, num_args));
		}
	    }
	  if (num_args == 1)
	    {
	      if (strcmp(funcname, "1+") == 0) return(clean_up(one_plus(prog, args, constants), args, num_args));
	      if (strcmp(funcname, "1-") == 0) return(clean_up(one_minus(prog, args, constants), args, num_args));
	      if (strcmp(funcname, "integer->char") == 0) return(clean_up(integer_to_char(args[1]), args, num_args));
	    }
	}
      if (num_args == 1)
	{
	  if (strcmp(funcname, "not") == 0) return(clean_up_if_needed(not_p(prog, args, constants), args, num_args, need_result));
	  if (booleans == 0)
	    {
	      if (strcmp(funcname, "lognot") == 0) return(clean_up_if_needed(lognot_1(prog, args, constants), args, num_args, need_result));
	      if (strcmp(funcname, "abs") == 0) return(clean_up_if_needed(abs_1(prog, args, constants), args, num_args, need_result));
	      if (strcmp(funcname, "sin") == 0) return(clean_up_if_needed(sin_1(prog, args, constants), args, num_args, need_result));
	      if (strcmp(funcname, "cos") == 0) return(clean_up_if_needed(cos_1(prog, args, constants), args, num_args, need_result));
	      if (strcmp(funcname, "tan") == 0) return(clean_up_if_needed(tan_1(prog, args, constants), args, num_args, need_result));
	      if (strcmp(funcname, "random") == 0) return(clean_up_if_needed(random_1(prog, args), args, num_args, need_result));
	      
	      if (strcmp(funcname, "radians->hz") == 0) return(clean_up_if_needed(mus_radians2hz_1(prog, args, constants), args, num_args, need_result));
	      if (strcmp(funcname, "hz->radians") == 0) return(clean_up_if_needed(mus_hz2radians_1(prog, args, constants), args, num_args, need_result));
	      if (strcmp(funcname, "degrees->radians") == 0) 
		return(clean_up_if_needed(mus_degrees2radians_1(prog, args, constants), args, num_args, need_result));
	      if (strcmp(funcname, "radians->degrees") == 0) 
		return(clean_up_if_needed(mus_radians2degrees_1(prog, args, constants), args, num_args, need_result));
	      if (strcmp(funcname, "db->linear") == 0) return(clean_up_if_needed(mus_db2linear_1(prog, args, constants), args, num_args, need_result));
	      if (strcmp(funcname, "linear->db") == 0) return(clean_up_if_needed(mus_linear2db_1(prog, args, constants), args, num_args, need_result));
	      if (strcmp(funcname, "mus-random") == 0) return(clean_up_if_needed(mus_random_1(prog, args, constants), args, num_args, need_result));
	      
	      if (current_optimization > 1)
		{
		  if (strcmp(funcname, "atan") == 0) return(clean_up_if_needed(atan1_1(prog, args, constants), args, num_args, need_result));
		  if (strcmp(funcname, "log") == 0) return(clean_up_if_needed(log_1(prog, args, constants), args, num_args, need_result));
		  if (strcmp(funcname, "exp") == 0) return(clean_up_if_needed(exp_1(prog, args, constants), args, num_args, need_result));
		  if (strcmp(funcname, "asin") == 0) return(clean_up_if_needed(asin_1(prog, args, constants), args, num_args, need_result));
		  if (strcmp(funcname, "acos") == 0) return(clean_up_if_needed(acos_1(prog, args, constants), args, num_args, need_result));
		  if (strcmp(funcname, "sqrt") == 0) return(clean_up_if_needed(sqrt_1(prog, args, constants), args, num_args, need_result));
		}
	      if (strcmp(funcname, "inexact->exact") == 0) return(clean_up_if_needed(inexact2exact_1(prog, args, constants), args, num_args, need_result));
	      if (strcmp(funcname, "exact->inexact") == 0) return(clean_up_if_needed(exact2inexact_1(prog, args, constants), args, num_args, need_result));
	      if (strcmp(funcname, "round") == 0) return(clean_up_if_needed(round_1(prog, args, constants, need_result), args, num_args, need_result));
	      if (strcmp(funcname, "truncate") == 0) return(clean_up_if_needed(truncate_1(prog, args, constants, need_result), args, num_args, need_result));
	      if (strcmp(funcname, "floor") == 0) return(clean_up_if_needed(floor_1(prog, args, constants, need_result), args, num_args, need_result));
	      if (strcmp(funcname, "ceiling") == 0) return(clean_up_if_needed(ceiling_1(prog, args, constants, need_result), args, num_args, need_result));
	      if (strcmp(funcname, "odd?") == 0) return(clean_up_if_needed(odd_p(prog, args, constants), args, num_args, need_result));
	      if (strcmp(funcname, "even?") == 0) return(clean_up_if_needed(even_p(prog, args, constants), args, num_args, need_result));
	      if (strcmp(funcname, "zero?") == 0) return(clean_up_if_needed(zero_p(prog, args, constants), args, num_args, need_result));
	      if (strcmp(funcname, "positive?") == 0) return(clean_up_if_needed(positive_p(prog, args, constants), args, num_args, need_result));
	      if (strcmp(funcname, "negative?") == 0) return(clean_up_if_needed(negative_p(prog, args, constants), args, num_args, need_result));
	    }
	  if (strcmp(funcname, "mus-header-type-name") == 0) return(clean_up_if_needed(mus_header_type_name_1(prog, args), args, num_args, need_result));
	  if (strcmp(funcname, "mus-data-format-name") == 0) return(clean_up_if_needed(mus_data_format_name_1(prog, args), args, num_args, need_result));
	}
      if (strcmp(funcname, "make-vct") == 0) return(clean_up_if_needed(make_vct_1(prog, args, num_args), args, num_args, need_result));
      if ((need_result) || (XEN_LIST_P(form)))
	/* need the list check as well because the called function might have side-effects */
	return(clean_up(NULL, args, num_args));
      return(make_xen_value(R_UNSPECIFIED, -1, R_CONSTANT));
    }

  if (XEN_NUMBER_P(form))
    {
      if ((XEN_EXACT_P(form)) && (XEN_INTEGER_P(form)))
	return(make_xen_value(R_INT, add_int_to_ptree(prog, XEN_TO_C_INT(form)), R_CONSTANT));
      else return(make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, XEN_TO_C_DOUBLE(form)), R_CONSTANT));
    }
  if (XEN_STRING_P(form)) 
    return(make_xen_value(R_STRING, add_string_to_ptree(prog, copy_string(XEN_TO_C_STRING(form))), R_CONSTANT));
  if (XEN_BOOLEAN_P(form))
    return(make_xen_value(R_BOOL, add_int_to_ptree(prog, (XEN_FALSE_P(form)) ? 0 : 1), R_CONSTANT));
  if (XEN_CHAR_P(form)) 
    return(make_xen_value(R_CHAR, add_int_to_ptree(prog, (int)(XEN_TO_C_CHAR(form))), R_CONSTANT));
  if (XEN_SYMBOL_P(form))
    return(add_global_var_to_ptree(prog, form));
  if (!run_warned)
    return(run_warn("can't handle: %s", XEN_AS_STRING(form)));
  return(NULL);
}

static xen_value *lookup_generalized_set(ptree *prog, char *accessor, xen_value *in_v, xen_value *v)
{
  if (v->type == R_FLOAT)
    {
      if (strcmp(accessor, "mus-phase") == 0) v = mus_set_phase_1(prog, in_v, v); else
      if (strcmp(accessor, "mus-frequency") == 0) v = mus_set_frequency_1(prog, in_v, v); else
      if (strcmp(accessor, "mus-scaler") == 0) v = mus_set_scaler_1(prog, in_v, v); else
      if (strcmp(accessor, "mus-feedback") == 0) v = mus_set_feedback_1(prog, in_v, v); else
      if (strcmp(accessor, "mus-feedforward") == 0) v = mus_set_feedforward_1(prog, in_v, v); else
      if (strcmp(accessor, "mus-increment") == 0) v = mus_set_increment_1(prog, in_v, v); else
      if (strcmp(accessor, "mus-a0") == 0) v = mus_set_a0_1(prog, in_v, v); else
      if (strcmp(accessor, "mus-a1") == 0) v = mus_set_a1_1(prog, in_v, v); else
      if (strcmp(accessor, "mus-a2") == 0) v = mus_set_a2_1(prog, in_v, v); else
      if (strcmp(accessor, "mus-b1") == 0) v = mus_set_b1_1(prog, in_v, v); else
      if (strcmp(accessor, "mus-b2") == 0) v = mus_set_b2_1(prog, in_v, v); else
      if (strcmp(accessor, "mus-formant-radius") == 0) v = mus_set_formant_radius_1(prog, in_v, v);
    }
  if (v->type == R_INT)
    {
      if (strcmp(accessor, "mus-ramp") == 0) v = mus_set_ramp_1(prog, in_v, v); else
      if (strcmp(accessor, "mus-hop") == 0) v = mus_set_hop_1(prog, in_v, v); else
      if (strcmp(accessor, "mus-location") == 0) v = mus_set_location_1(prog, in_v, v); else
      if (strcmp(accessor, "mus-length") == 0) v = mus_set_length_1(prog, in_v, v);
    }
  if (strcmp(accessor, "mus-srate") == 0) v = mus_set_srate_1(prog, in_v, v);
  if (v == NULL) run_warn("can't set! %s", accessor);
  if (in_v) FREE(in_v);
  if (accessor) FREE(accessor);
  return(v);
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
  if (current_optimization == 0) return(NULL);
  form = XEN_CAR(code);
  prog = make_ptree(8);
  if ((XEN_PROCEDURE_P(XEN_CADR(code))) && 
      (SCM_TYP7(XEN_CADR(code)) != scm_tc7_smob)) /* applicable smobs cause confusion here */
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
	fprintf(stderr, msg);
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

void *form_to_ptree_1f2f(XEN code)
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

void *form_to_ptree_0f2f(XEN code)
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

void *form_to_ptree_1f2b(XEN code)
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

void *form_to_ptree_1f2b_without_env(XEN code)
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

/* ---------------- internal testing stuff ---------------- */

static XEN g_run_eval(XEN code, XEN arg)
{
  ptree *pt;
  XEN result;
  current_optimization = 4;
  pt = make_ptree(8);
  pt->result = walk(pt, code, TRUE);
  if (pt->result)
    {
      add_triple_to_ptree(pt, make_triple(quit, descr_quit, NULL, 0));
#ifdef DESCRIBE_PTREE
      {
	char *msg;
	msg = describe_ptree(pt);
	fprintf(stderr, msg);
	FREE(msg);
      }
#endif
      if ((XEN_BOUND_P(arg)) && (pt->args))
	{
	  switch (pt->arg_types[0])
	    {
	    case R_FLOAT:  pt->dbls[pt->args[0]] = (Float)XEN_TO_C_DOUBLE(arg); break;
	    case R_INT:    pt->ints[pt->args[0]] = (int)XEN_TO_C_INT(arg); break;
	    case R_STRING: pt->ints[pt->args[0]] = (int)copy_string(XEN_TO_C_STRING(arg)); break;
	    }
	}
      eval_ptree(pt);
      if (pt->result->type == R_FLOAT) result = C_TO_XEN_DOUBLE(pt->dbls[pt->result->addr]); else
	if (pt->result->type == R_INT) result = C_TO_XEN_INT(pt->ints[pt->result->addr]); else
	  if (pt->result->type == R_CHAR) result = C_TO_XEN_CHAR((char)(pt->ints[pt->result->addr])); else
	    if (pt->result->type == R_STRING) result = C_TO_XEN_STRING((char *)(pt->ints[pt->result->addr])); else
	      if (pt->ints[pt->result->addr]) result = XEN_TRUE; else
		result = XEN_FALSE;
      free_ptree((void *)pt);
      return(result);
    }
  if (pt) free_ptree((void *)pt);
  XEN_ERROR(XEN_ERROR_TYPE("cannot-parse"),
	    code);
  return(XEN_FALSE);
}
#else
void *form_to_ptree_1f2b(XEN code) {return(NULL);}
void *form_to_ptree_1f2b_without_env(XEN code) {return(NULL);}
void *form_to_ptree_1f2f(XEN code) {return(NULL);}
Float evaluate_ptree_0f2f(void *upt) {return(0.0);}
void *form_to_ptree_0f2f(XEN code) {return(NULL);}
Float evaluate_ptree_1f2f(void *upt, Float arg) {return(0.0);}
int evaluate_ptree_1f2b(void *upt, Float arg) {return(0);}
void *free_ptree(void *upt) {return(NULL);}
XEN ptree_code(void *p) {return(XEN_FALSE);}
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
  #define H_vct_map "(" S_vct_map " thunk v ...) calls 'thunk' which should return a frame; the frame result \
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

  if (optimization(get_global_state()) > 0)
    {
#if WITH_RUN
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
#endif
    }

  for (i = 0; i < min_len; i++) 
    {
      obj = XEN_CALL_0_NO_CATCH(code, S_vct_map);
      if (mus_xen_p(obj))
	{
	  f = MUS_XEN_TO_CLM(obj);
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

void g_init_run(void)
{
#if WITH_RUN
  XEN_DEFINE_PROCEDURE("run-eval", g_run_eval, 1, 1, 0, "run macro testing...");
  XEN_DEFINE_PROCEDURE("vct-map-2",     g_vct_map, 2, 0, 0,      H_vct_map);
  XEN_EVAL_C_STRING("(defmacro* " S_vct_map " (thunk #:rest args) `(vct-map-2 (list ',thunk ,thunk) (list ,@args)))");
  scm_set_object_property_x(C_STRING_TO_XEN_SYMBOL(S_vct_map), XEN_DOCUMENTATION_SYMBOL, C_TO_XEN_STRING(H_vct_map));
#else
  XEN_DEFINE_PROCEDURE(S_vct_map, g_vct_map, 1, 0, 1, H_vct_map);
#endif

  #define H_optimization_hook S_optimization_hook " (msg) is called possibly several times \
during optimization to indicate where the optimizer ran into trouble:\n\
  (add-hook! optimization-hook (lambda (msg) (snd-print msg)))"

  XEN_DEFINE_HOOK(optimization_hook, S_optimization_hook, 1, H_optimization_hook);      /* arg = message */
}
