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
 *      try to use scm_procedure_source if procedure variable is passed
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
 *   if begin or and not let let* set! cond do do* define
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
 *
 *   various sndlib, clm, and snd functions
 *
 * tests in snd-test.scm, test 22
 *
 *
 * TODO: case[not symbols]
 * TODO: (rest of)clm snd procs
 * TODO: rest of snd-test 22, and overall test
 * TODO: ptree as fragment edit op
 * TODO: general evaluate_ptree accessor
 * TODO: (define (a ...) ) style of function decl
 * TODO: make-sample-reader (mix-sr?)
 * TODO: ports
 * TODO: timing tests: pqwvox, [fade singer?], rewrite scm examples to be optimizable
 *
 *
 * LIMITATIONS: <insert anxious lucubration here about DSP context and so on>
 *      variables can have only one type, the type has to be ascertainable somehow, 
 *      some variables (imported from outside our context) cannot be set, in some cases they can't even be found
 *      no recursion (could be added with some pain)
 *      no lists or pairs (these could be added, perhaps), vectors only of floats (could also be generalized)
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
#if HAVE_GUILE
#if WITH_RUN
#include "vct.h"
#include "clm2xen.h"

#define XEN_EXACT_P(Arg) XEN_TRUE_P(scm_exact_p(Arg))
#define XEN_SYMBOL_TO_VALUE(a) XEN_VARIABLE_REF(scm_sym2var(a, scm_current_module_lookup_closure(), XEN_TRUE))
#define XEN_SYMBOL_NAME_SET_VALUE(a, b) XEN_VARIABLE_SET(scm_sym2var(scm_str2symbol(a), scm_current_module_lookup_closure(), XEN_TRUE), b)
#define XEN_AS_STRING(form) XEN_TO_C_STRING(XEN_TO_STRING(form))
#define C_TO_XEN_CHAR(c) SCM_MAKE_CHAR(c)
#define XEN_CDDDR(a) SCM_CDDDR(a)
#define XEN_CAAR(a) XEN_CAR(XEN_CAR(a))
#define XEN_CDAR(a) XEN_CDR(XEN_CAR(a))


enum {R_INT, R_FLOAT, R_BOOL, R_VCT, R_READER, R_CLM, R_CHAR, R_STRING, R_PENDING, R_GOTO, R_FUNCTION, R_PORT};
static char *type_names[12] = {"int", "float", "bool", "vct", "reader", "clm", "char", "string", "pending", "continuation", "function", "port"};
static char* type_name(int id) {if ((id >= R_INT) && (id <= R_PORT)) return(type_names[id]); return("unknown");}

static int pointer_p(int type)
{
  return((type == R_VCT) || (type == R_CLM) || (type == R_READER));
}

static int pointer_or_goto_p(int type)
{
  return((type == R_VCT) || (type == R_CLM) || (type == R_READER) || (type == R_GOTO));
}


enum {R_VARIABLE, R_CONSTANT};

/* local variables (let vars) global to the closure we are passed are unbound at read-time,
 *   so we can't get their type from their current value.  It's possible to make plausible
 *   assumptions about the type however (vct-ref ...), so R_PENDING is used to flag some
 *   symbol whose type decision is awaiting clarification.  If a type is chosen, the actual
 *   value is retrieved at eval-time (initialize-globals), and if the type is not the
 *   one we chose, we give up -- this means in some cases, the user will have to reduce
 *   his optimization choice or change his code. (In some cases, once the type is determined
 *   triples in the existing program are edited to reflect that choice).
 */

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
static XEN optimization_hook = XEN_FALSE;
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

static xen_value *copy_xen_value(xen_value *v)
{
  return(make_xen_value(v->type, v->addr, v->constant));
}

static char *describe_xen_value(xen_value *v, int *ints, Float *dbls);

typedef struct {
  char *name;
  xen_value *v;
  int global, unclean, undefined, unsettable;
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
#define PTREE_LOC 2

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
  XEN code;
  int str_ctr, strs_size;
  int *strs;
  void *outer_tree;
} ptree;

static xen_var *find_var_in_ptree_via_addr(ptree *pt, int dbl, int addr);

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
      oldstr = REALLOC(oldstr, size * 2 * sizeof(char));
      (*oldsize) = size * 2;
    }
  strcat(oldstr, newstr);
  FREE(newstr);
  return(oldstr);
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
      snprintf(temp, 16, "%d, ", ints[i]);
      buf = str_append(buf, &size, temp);
    }
  temp = (char *)CALLOC(16, sizeof(char));
  snprintf(temp, 16, "%d], [", ints[p->int_ctr - 1]);
  buf = str_append(buf, &size, temp);
  for (i = 0; i < p->dbl_ctr - 1; i++)
    {
      temp = (char *)CALLOC(16, sizeof(char));
      snprintf(temp, 16, "%.4f, ", dbls[i]);
      buf = str_append(buf, &size, temp);
    }
  temp = (char *)CALLOC(16, sizeof(char));
  snprintf(temp, 16, "%.4f]\n", dbls[p->dbl_ctr - 1]);
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

static char *describe_xen_value(xen_value *v, int *ints, Float *dbls)
{
  char *buf = NULL;
  if (v == NULL) return(copy_string("null xen_value"));
  switch (v->type)
    {
    case R_BOOL:    buf = (char *)CALLOC(32, sizeof(char)); snprintf(buf, 32, "i%d(%s)", v->addr, (ints[v->addr] == 0) ? "#f" : "#t"); break;
    case R_INT:     buf = (char *)CALLOC(32, sizeof(char)); snprintf(buf, 32, "i%d(%d)", v->addr, ints[v->addr]);                      break;
    case R_CHAR:    buf = (char *)CALLOC(32, sizeof(char)); snprintf(buf, 32, "i%d(#\\%c)", v->addr, (char)(ints[v->addr]));           break;
    case R_STRING:  buf = (char *)CALLOC(256, sizeof(char)); snprintf(buf, 256, "i%d(\"%s\")", v->addr, (char *)(ints[v->addr]));      break;
    case R_FLOAT:   buf = (char *)CALLOC(32, sizeof(char)); snprintf(buf, 32, "d%d(%.4f)", v->addr, dbls[v->addr]);                    break;
    case R_VCT:     buf = vct_to_string((vct *)(ints[v->addr]));                                                                       break;
    case R_READER:  if (ints[v->addr]) buf = sf_to_string((snd_fd *)(ints[v->addr])); else buf = copy_string("null");                  break;
    case R_CLM:     if (ints[v->addr]) buf = copy_string(mus_describe((mus_any *)(ints[v->addr])));  else buf = copy_string("null");   break;
    case R_PENDING: return(copy_string("[value pending]"));                                                                            break;
    case R_GOTO:    return(copy_string("continuation")); /* TODO: describe goto */                                                     break;
    case R_FUNCTION: return(describe_ptree((ptree *)(ints[v->addr])));                                                                 break;
    default:        buf = (char *)CALLOC(32, sizeof(char)); snprintf(buf, 32, "unknown type: %d", v->type);                            break;
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
      pt->ints[PTREE_LOC] = (int)pt;
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
  else new_tree->outer_tree = pt;

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

static xen_var *free_xen_var(ptree *prog, xen_var *var)
{
  if (var)
    {
      /* fprintf(stderr, "free var %s %p %p %p\n", var->name, var, var->name, var->v);  */
      /* if var->global, reflect new value into outer level version of the variable upon quit */
      if ((var->global) &&
	  (var->unclean))
	{
	  /*
	  if (var->v->type == R_FLOAT)
	    fprintf(stderr,"at end %s set to %.4f from %d\n", var->name, prog->dbls[var->v->addr], var->v->addr);
	  else fprintf(stderr,"at end %s set to %d from %d\n", var->name, prog->ints[var->v->addr], var->v->addr);
	  */
	  switch (var->v->type)
	    {
	    case R_FLOAT:  XEN_SYMBOL_NAME_SET_VALUE(var->name, C_TO_XEN_DOUBLE(prog->dbls[var->v->addr]));           break;
	    case R_INT:    XEN_SYMBOL_NAME_SET_VALUE(var->name, C_TO_XEN_INT(prog->ints[var->v->addr]));              break;
	    case R_BOOL:   XEN_SYMBOL_NAME_SET_VALUE(var->name, C_TO_XEN_BOOLEAN(prog->ints[var->v->addr]));          break;
	    case R_STRING: XEN_SYMBOL_NAME_SET_VALUE(var->name, C_TO_XEN_STRING((char *)(prog->ints[var->v->addr]))); break;
	    case R_CHAR:   XEN_SYMBOL_NAME_SET_VALUE(var->name, C_TO_XEN_CHAR((char)(prog->ints[var->v->addr])));     break;
	    case R_VCT:
	      /* TODO: if global was originally a vector, try to reset its elements */
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
      if (pt->gc_ctr > 0)
	{
	  /* if we allocated it, we free it */
	  for (i = 0; i < pt->gc_ctr; i++)
	    {
	      xen_value *v;
	      v = pt->gcs[i];
	      if ((v) && (v->gc))
		{
		  switch (v->type)
		    {
		    case R_VCT:      c_free_vct((vct *)(pt->ints[v->addr]));            break;
		    case R_READER:   free_snd_fd((snd_fd *)(pt->ints[v->addr]));        break;
		    case R_FUNCTION: free_embedded_ptree((ptree *)(pt->ints[v->addr])); break;
		    case R_CLM:      mus_free((mus_any *)(pt->ints[v->addr]));          break;
		    default:         FREE((void *)(pt->ints[v->addr]));                 break;
		    }
		  v->gc = 0;
		  FREE(v);
		  pt->gcs[i] = NULL;
		}
	    }
	}
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
  /* fprintf(stderr,"put %f at %d ",value, pt->dbl_ctr); */
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

static xen_var *find_pending_var_in_ptree_via_xen_value(ptree *pt, xen_value *v)
{
  int i;
  xen_var *var;
  for (i = 0; i < pt->global_var_ctr; i++)
    {
      var = pt->global_vars[i];
      if ((var) &&
	  (var->v->addr == v->addr)) /* these are unique and unconfusable due to pending_tag above */
	return(var);
    }
  return(NULL);
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
    case R_FLOAT:  return(make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, 0.0), R_VARIABLE)); break;
    case R_STRING: return(make_xen_value(R_STRING, add_string_to_ptree(prog, NULL), R_VARIABLE)); break;
    default:       return(make_xen_value(type, add_int_to_ptree(prog, 0), R_VARIABLE)); break;
    }
  return(NULL);
}

static xen_value *transfer_value(ptree *prog, xen_value *v)
{
  switch (v->type)
    {
    case R_FLOAT: return(make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, prog->dbls[v->addr]), R_VARIABLE)); break;
    case R_STRING: return(make_xen_value(R_STRING, add_string_to_ptree(prog, copy_string((char *)(prog->ints[v->addr]))), R_VARIABLE)); break;
    default: return(make_xen_value(v->type, add_int_to_ptree(prog, prog->ints[v->addr]), R_VARIABLE));
    }
}

static int pending_tag = -1;

static XEN symbol_to_value(ptree *pt, XEN sym, int *local)
{
  /* sigh... this is a can of worms */
  XEN new_val = XEN_UNDEFINED;
  XEN pair = XEN_FALSE;
  XEN code_env = XEN_FALSE;
  XEN names, values;
  int i, len;
  int believe_it = FALSE;
  if (XEN_PROCEDURE_P(pt->code))
    {
      /* scrounge around in the "eval" environment looking for local version of sym */
      code_env = SCM_ENV(pt->code);
      /* fprintf(stderr,"look 10: %s\n", XEN_AS_STRING(code_env)); */
      if (XEN_LIST_P(code_env))
	{
	  /* now we might have an "eval environment" (an association list) or an "eval closure" (something different...) */
	  if (XEN_LIST_P(XEN_CAR(XEN_CAR(code_env))))
	    {
	      /* fprintf(stderr,"look 1: %s\n", XEN_AS_STRING(code_env)); */
	      names = XEN_CAAR(code_env);
	      values = XEN_CDAR(code_env);
	      len = XEN_LIST_LENGTH(names);
	      for (i = 0; i < len; i++)
		if (XEN_EQ_P(XEN_LIST_REF(names, i), sym))
		  {
		    new_val = XEN_LIST_REF(values, i);
		    believe_it = TRUE;
		    (*local) = TRUE;
		    break;
		  }
	      if (!believe_it)
		{
		  /* next portion of context appears to be in trailing association list */
		  if (XEN_NOT_NULL_P(XEN_CDR(code_env)))
		    {
		      pair = scm_sloppy_assoc(sym, XEN_CDR(code_env));
		      if (XEN_TRUE_P(scm_pair_p(pair)))
			{
			  new_val = XEN_CDR(pair);
			  /* whatever it is, it is the current reigning sym */
			  /* fprintf(stderr,"%s found 12: %s\n", XEN_AS_STRING(sym), XEN_AS_STRING(new_val)); */
			  believe_it = TRUE;
			  (*local) = TRUE;
			}
		    }
		  if (!believe_it)
		    {
		      /* getting desperate -- final list of code_env appears to be arg list? */
		      names = XEN_LIST_REF(code_env, XEN_LIST_LENGTH(code_env) - 1);
		      if ((XEN_LIST_P(names)) && (XEN_LIST_P(XEN_CAR(names))))
			{
			  /* fprintf(stderr,"look 14: %s\n", XEN_AS_STRING(names)); */
			  values = XEN_CDR(names);
			  names = XEN_CAR(names);
			  len = XEN_LIST_LENGTH(names);
			  for (i = 0; i < len; i++)
			    if (XEN_EQ_P(XEN_LIST_REF(names, i), sym))
			      {
				new_val = XEN_LIST_REF(values, i);
				/* fprintf(stderr,"%s found 1: %s\n", XEN_AS_STRING(sym), XEN_AS_STRING(new_val)); */
				believe_it = TRUE;
				(*local) = TRUE;
				break;
			      }
			}
		    }
		}
	    }
	  else
	    {
	      /* fprintf(stderr,"look 2: %s\n", XEN_AS_STRING(code_env));  */
	      pair = scm_sloppy_assoc(sym, code_env); /* sloppy = no goddamn error */
	      if (XEN_TRUE_P(scm_pair_p(pair)))
		{
		  new_val = XEN_CDR(pair);
		  /* whatever it is, it is the current reigning sym */
		  /* fprintf(stderr,"%s found 2: %s\n", XEN_AS_STRING(sym), XEN_AS_STRING(new_val)); */
		  believe_it = TRUE;
		  (*local) = TRUE;
		}
	    }
	}
    }
  if ((!believe_it) && (XEN_NOT_BOUND_P(new_val)))
    {
      new_val = XEN_SYMBOL_TO_VALUE(sym); /* try the outer environment */
      (*local) = FALSE;
      /* fprintf(stderr,"%s found 3: %s\n", XEN_AS_STRING(sym), XEN_AS_STRING(new_val));  */
    } 
  return(new_val);
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

static vct *vector_to_vct(XEN vect)
{
  int len, i;
  vct *v;
  XEN *vdata;
  len = XEN_VECTOR_LENGTH(vect);
  if (len == 0) return(NULL);
  v = c_make_vct(len);
  vdata = XEN_VECTOR_ELEMENTS(vect);
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

static xen_value *add_global_var_to_ptree(ptree *prog, XEN form)
{
  XEN val;
  xen_var *var;
  int var_loc = 0, local_var = FALSE;
  xen_value *v = NULL;
  char varname[256];
  snprintf(varname, 256, "%s", XEN_SYMBOL_TO_C_STRING(form));
  var = find_var_in_ptree(prog, varname);
  if (var) return(copy_xen_value(var->v));
  val = symbol_to_value(prog, form, &local_var);
  /* fprintf(stderr,"add global %s %d %s\n",varname, sf_p(val), XEN_AS_STRING(val)); */
  /* if val not (yet) available, set up a request for it at eval_ptree time
   *    if we know for sure what it's type will be (i.e. clm gen, reader, etc)
   */
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
				  vct *v0;
				  v0 = vector_to_vct(val);
				  if (v0)
				    {
				      v = make_xen_value(R_VCT, add_int_to_ptree(prog, (int)v0), R_VARIABLE);
				      add_obj_to_gcs(prog, R_VCT, v->addr); /* this makes its own xen_value with TRUE gc field */
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
      prog->global_vars[var_loc]->unsettable = local_var;
      /* actually we might be able to set this, but it's too much trouble to figure out */
    }
  else
    {
      prog->need_init = TRUE;
      /* fprintf(stderr, "%s is pending\n", varname); */
      v = make_xen_value(R_PENDING, pending_tag--, R_VARIABLE);
      var_loc = add_outer_var_to_ptree(prog, varname, v);
      prog->global_vars[var_loc]->unsettable = TRUE;
    }
  return(v);
}

static continuation *add_goto_to_ptree(ptree *pt, char *name)
{
  continuation *c;
  int old_size, i;
  c = (continuation *)CALLOC(1, sizeof(continuation));
  c->name = (char *)CALLOC(256, sizeof(char));                      /* c->name is used within the call/cc to identify the continuation */
  snprintf(c->name, 256, "%s", name);
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

static xen_var *initialize_globals(ptree *pt)
{
  /* it's possible the global value won't be defined until run-time (let var outside our form) */
  int i, local = 0;
  xen_var *var;
  xen_value *v;
  XEN val;
  for (i = 0; i < pt->global_var_ctr; i++)
    {
      var = pt->global_vars[i];
      if ((var) &&
	  (var->undefined))
	{
	  v = var->v;
	  val = symbol_to_value(pt, C_STRING_TO_XEN_SYMBOL(var->name), &local);
	  if (XEN_NOT_BOUND_P(val)) return(var);
	  /* fprintf(stderr,"init %s to %s (type: %d)\n", var->name, XEN_AS_STRING(val), v->type); */
	  switch (v->type)
	    {
	    case R_INT:
	      if (!(XEN_INTEGER_P(val))) return(var);
	      pt->ints[v->addr] = XEN_TO_C_INT(val);     
	      break;
	    case R_FLOAT:  
	      if (!(XEN_DOUBLE_P(val))) return(var);
	      pt->dbls[v->addr] = XEN_TO_C_DOUBLE(val);  
	      break;
	    case R_BOOL:   
	      if (!(XEN_BOOLEAN_P(val))) return(var);
	      pt->ints[v->addr] = XEN_TO_C_BOOLEAN(val); 
	      break;
	    case R_VCT:    
	      if (!(VCT_P(val))) return(var);
	      pt->ints[v->addr] = (int)get_vct(val);     
	      break;
	    case R_READER: 
	      if (!(sf_p(val))) return(var);
	      pt->ints[v->addr] = (int)get_sf(val);  
	      break;
	    case R_CLM: 
	      /* (let ((v (make-vct 10)) (e1 (make-env '(0 0 1 1) :end 9))) (vct-map! v (lambda () (env e1))) v) */
	      if (mus_xen_p(val)) 
		pt->ints[v->addr] = (int)MUS_XEN_TO_CLM(val);
	      else
		{
		  if (XEN_FALSE_P(val))
		    pt->ints[v->addr] = 0; /* null gen is acceptable */
		}
	      break;
	    case R_CHAR:
	      if (!(XEN_CHAR_P(val))) return(var);
	      pt->ints[v->addr] = (int)(XEN_TO_C_CHAR(val));     
	      break;
	    case R_STRING:
	      if (!(XEN_STRING_P(val))) return(var);
	      if (pt->ints[v->addr]) FREE((char *)(pt->ints[v->addr]));
	      pt->ints[v->addr] = (int)(copy_string(XEN_TO_C_STRING(val)));     
	      break;
	    }
	}
    }
  pt->need_init = FALSE;
  return(NULL);
}

char *initialize_ptree(void *upt)
{
  xen_var *trouble;
  ptree *pt = (ptree *)upt;
  if (pt->need_init)
    {
      trouble = initialize_globals(pt);
      if (trouble)
	{
	  run_warn("can't initialize %s", trouble->name);
	  return(mus_format("can't initialize %s", trouble->name));
	}
    }
  return(NULL);
}

#define PC ints[0]
#define ALL_DONE ints[1]
#define PTREE ((ptree *)(ints[PTREE_LOC]))

static void eval_ptree(ptree *prog)
{
  /* evaluates program, result in prog->result, 
   *   assumes args already passed in addrs given by prog->addrs
   *   also that globals have been initialized via initialize_ptree
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
      fprintf(stderr, describe_ptree(prog));
      /* sleep(1); */
#endif
      curfunc = prog->program[PC++];
      (*(curfunc->function))(curfunc->args, ints, dbls);
#if 0
      fprintf(stderr, "%d: %s\n", PC, (*(curfunc->descr))(curfunc->args, ints, dbls));
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
#define STRING_RESULT ((char *)(ints[args[0]]))
#define CHAR_RESULT ((char)(ints[args[0]]))
#define BOOL_ARG_1 ints[args[1]]
#define BOOL_ARG_2 ints[args[2]]
#define BOOL_ARG_3 ints[args[3]]
#define INT_ARG_1 ints[args[1]]
#define INT_ARG_2 ints[args[2]]
#define INT_ARG_3 ints[args[3]]
#define INT_ARG_4 ints[args[3]]
#define FLOAT_ARG_1 dbls[args[1]]
#define FLOAT_ARG_2 dbls[args[2]]
#define FLOAT_ARG_3 dbls[args[3]]
#define VCT_ARG_1 (vct *)(ints[args[1]])
#define VCT_ARG_2 (vct *)(ints[args[2]])
#define STRING_ARG_1 ((char *)(ints[args[1]]))
#define STRING_ARG_2 ((char *)(ints[args[2]]))
#define CHAR_ARG_1 ((char)(ints[args[1]]))
#define CHAR_ARG_2 ((char)(ints[args[2]]))

static void quit(int *args, int *ints, Float *dbls) {ALL_DONE = TRUE;}
static char *descr_quit(int *args, int *ints, Float *dbls) {return(copy_string("quit"));}

static void jump(int *args, int *ints, Float *dbls) {PC += ints[args[0]];}
static char *descr_jump(int *args, int *ints, Float *dbls) {return(mus_format("jump i%d(%d)", args[0], INT_RESULT));}

static void jump_abs(int *args, int *ints, Float *dbls) {PC = ints[args[0]];}
static char *descr_jump_abs(int *args, int *ints, Float *dbls) {return(mus_format("goto i%d(%d)", args[0], INT_RESULT));}

static void jump_indirect(int *args, int *ints, Float *dbls) {PC = ints[ints[args[0]]];}
static char *descr_jump_indirect(int *args, int *ints, Float *dbls) {return(mus_format("goto [i%d(%d)=%d]", args[0], INT_RESULT, ints[ints[args[0]]]));}

static void jump_if(int *args, int *ints, Float *dbls) {if (ints[args[1]] != 0) PC += ints[args[0]];}
static char *descr_jump_if(int *args, int *ints, Float *dbls) 
{
  return(mus_format("if (i%d(%s)) jump i%d(%d)", args[1], (INT_ARG_1) ? "#t" : "#f", args[0], INT_RESULT));
}

static void jump_if_not(int *args, int *ints, Float *dbls) {if (ints[args[1]] == 0) PC += ints[args[0]];}
static char *descr_jump_if_not(int *args, int *ints, Float *dbls) 
{
  return(mus_format("if (!i%d(%s)) jump i%d(%d)", args[1], (INT_ARG_1) ? "#t" : "#f", args[0], INT_RESULT));
}

static void store_i(int *args, int *ints, Float *dbls) {INT_RESULT = INT_ARG_1;}
static char *descr_store_i(int *args, int *ints, Float *dbls) {return(mus_format("i%d(%d) = i%d(%d)", args[0], INT_RESULT, args[1], INT_ARG_1));}

static void store_f(int *args, int *ints, Float *dbls) {FLOAT_RESULT = FLOAT_ARG_1;}
static char *descr_store_f(int *args, int *ints, Float *dbls) {return(mus_format("d%d(%.4f) = d%d(%.4f)", args[0], FLOAT_RESULT, args[1], FLOAT_ARG_1));}

static void store_f_i(int *args, int *ints, Float *dbls) {INT_RESULT = (int)FLOAT_ARG_1;}
static char *descr_store_f_i(int *args, int *ints, Float *dbls) {return(mus_format("i%d(%d) = d%d(%.4f)", args[0], INT_RESULT, args[1], FLOAT_ARG_1));}

static void store_i_f(int *args, int *ints, Float *dbls) {FLOAT_RESULT = (Float)INT_ARG_1;}
static char *descr_store_i_f(int *args, int *ints, Float *dbls) {return(mus_format("d%d(%.4f) = i%d(%d)", args[0], FLOAT_RESULT, args[1], INT_ARG_1));}

static void store_false(int *args, int *ints, Float *dbls) {BOOL_RESULT = 0;}
static char *descr_store_false(int *args, int *ints, Float *dbls) {return(mus_format("i%d(%d) = 0", args[0], BOOL_RESULT));}

static void store_true(int *args, int *ints, Float *dbls) {BOOL_RESULT = 1;}
static char *descr_store_true(int *args, int *ints, Float *dbls) {return(mus_format("i%d(%d) = 1", args[0], BOOL_RESULT));}

static void store_s(int *args, int *ints, Float *dbls) 
{
  /* fprintf(stderr,"set %s (%d) to %s (%d)\n", STRING_RESULT, args[0], STRING_ARG_1, args[1]); */
  if (STRING_RESULT) FREE(STRING_RESULT);
  STRING_RESULT = copy_string(STRING_ARG_1);
}
static char *descr_store_s(int *args, int *ints, Float *dbls) 
{
  return(mus_format("i%d(\"%s\") = i%d(\"%s\")", args[0], STRING_RESULT, args[1], STRING_ARG_1));
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

static char *define_form(ptree *prog, XEN form)
{
  /* behaves here just like a sequential bind (let*) */
  XEN var, val;
  xen_value *v, *vs;
  if ((!(XEN_LIST_P(form))) || (XEN_NULL_P(form))) return(NULL);
  var = XEN_CADR(form);
  val = XEN_CADDR(form);
  if (!(XEN_SYMBOL_P(var))) return(mus_format("can't handle this define: %s", XEN_AS_STRING(var)));
  v = walk(prog, val, TRUE);
  if (v == NULL) return(mus_format("can't handle this define value: %s", XEN_AS_STRING(val)));
  if (v->type == R_PENDING) 
    {
      FREE(v);
      return(mus_format("won't handle this define value: %s", XEN_AS_STRING(val)));
    }
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
	  if ((v == NULL) || (v->type == R_PENDING))
	    {
	      int j, was_null;
	      was_null = (v == NULL);
	      for (j = 0; j < i; j++)
		{
		  if (old_vs[j]) FREE(old_vs[j]);
		  if (vs[j]) FREE(vs[j]);
		}
	      if (v) FREE(v);
	      FREE(vs);
	      FREE(old_vs);
	      if (was_null)
		return(mus_format("can't handle %s var: %s", name, XEN_AS_STRING(lets)));
	      else return(mus_format("pending var used in %s: %s", XEN_AS_STRING(lets), name));
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
	  if ((v == NULL) || (v->type == R_PENDING)) 
	    {
	      if (v) FREE(v);
	      if (v == NULL)
		return(mus_format("can't handle %s var: %s", name, XEN_AS_STRING(lets)));
	      else return(mus_format("pending var used in %s: %s", name, XEN_AS_STRING(lets)));
	    }
	  vs = transfer_value(prog, v);
	  add_var_to_ptree(prog, XEN_SYMBOL_TO_C_STRING(XEN_CAR(var)), vs);
	  set_var(prog, vs, v);
	  FREE(vs);
	  FREE(v);
	}
    }
  return(NULL);
}

static void *form_to_ptree(XEN code);

static char *declare_args(ptree *prog, XEN form, int default_arg_type)
{
  XEN arg, args, declarations, declaration;
  xen_value *v = NULL;
  int i, arg_num, arg_type;
  char *type;
  /* TODO: (define (name...) ...) here by args = XEN_CDADR(form), rest the same */
  args = XEN_CADR(form);
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
		  /* fprintf(stderr,"found %s\n",XEN_AS_STRING(declaration)); */
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

static xen_value *lambda_form(ptree *prog, XEN form)
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
      declare_args(new_tree, form, R_INT); /* assuming clm here */
      new_tree->result = walk_sequence(new_tree, XEN_CDDR(form), TRUE, "lambda");
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
      free_embedded_ptree(new_tree);
      return(run_warn("can't handle this embedded lambda: %s", XEN_AS_STRING(form)));
    }
  got_lambda = 1;
  locals_loc = prog->var_ctr;
  err = declare_args(prog, form, R_FLOAT);
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
  if (trouble) return(run_warn(trouble));
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
  if (if_value->type != R_BOOL) return(run_warn("if: selector type not boolean: %s", XEN_AS_STRING(XEN_CADR(form))));
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
	      if ((false_result->type == R_PENDING) || (false_result->type == R_BOOL))
		false_result->type = true_result->type;
	      else
		if ((true_result->type == R_PENDING) || (true_result->type == R_BOOL))
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
  return(make_xen_value(R_BOOL, -1, R_CONSTANT));
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
      prog->ints[jump_to_next_clause->addr] = prog->triple_ctr - current_pc - 1;
      FREE(jump_to_next_clause);
      jump_to_next_clause = NULL;
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
  return(make_xen_value(R_BOOL, -1, R_CONSTANT));
}

static xen_value *do_form_1(ptree *prog, XEN form, int need_result, int sequential)
{
  /* (do ([(var val [up])...]) (test [res ...]) [exp ...]): (do () (#t))  */

  xen_value *result = NULL, *test, *expr, *jump_to_result, *jump_to_test;
  XEN var, vars, done, body, results, test_form;
  xen_var *vr;
  int i, locals_loc, test_loc, varlen;
  char *trouble;
  vars = XEN_CADR(form);
  done = XEN_CADDR(form);
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
  if (trouble) return(run_warn(trouble));

  test_loc = prog->triple_ctr;
  test = walk(prog, test_form, TRUE);
  if (test == NULL) return(NULL);
  if (test->type != R_BOOL) 
    {
      FREE(test);
      return(run_warn("do test must be boolean: %s", XEN_AS_STRING(test_form)));
    }
  /* if test true, jump to result section */
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
	      if ((expr == NULL) || (expr->type == R_PENDING)) 
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
  prog->ints[jump_to_result->addr] = prog->triple_ctr - test_loc - 2;
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
  XEN lambda_form, continuation_name;
  continuation *c;
  xen_value *v = NULL;
  lambda_form = XEN_CADR(form);
  continuation_name = XEN_CAR(XEN_CADR(lambda_form));
  c = add_goto_to_ptree(prog, XEN_SYMBOL_TO_C_STRING(continuation_name));
  v = walk_sequence(prog, XEN_CDDR(lambda_form), need_result, "call/cc");
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
  else v = add_empty_var_to_ptree(prog, R_PENDING);
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
      if ((v == NULL) || ((v->type != R_BOOL) && (!(pointer_or_goto_p(v->type)))))
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
      if ((v == NULL) || ((v->type != R_BOOL) && (!(pointer_or_goto_p(v->type)))))
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
  if ((XEN_LIST_P(settee)) && (XEN_SYMBOL_P(XEN_CAR(settee))) && (XEN_LIST_LENGTH(settee) == 2))
    {
      XEN in_settee;
      xen_value *in_v, *v;
      char accessor[256];
      in_settee = XEN_CAR(settee);
      snprintf(accessor, 256, "%s", XEN_SYMBOL_TO_C_STRING(in_settee));
      v = walk(prog, setval, TRUE);
      if (v == NULL) 
	return(run_warn("set!: can't handle: %s", XEN_AS_STRING(setval)));
      in_v = walk(prog, XEN_CADR(settee), TRUE);
      if (in_v == NULL)
	{
	  FREE(v);
	  return(run_warn("set!: can't handle: %s", XEN_AS_STRING(XEN_CADR(settee))));
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
  snprintf(varname, 256, "%s", XEN_SYMBOL_TO_C_STRING(settee));
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
      if (pointer_p(val_type))
	{
	  FREE(v);
	  return(run_warn("can't set pointer var (%s) to alias other such var", varname));
	}
      if (val_type != var_type)
	{
	  /* here #f is ok for pointer val == NULL, goto val is ok if var pending */
 	  if ((val_type == R_GOTO) && 
 	      ((var_type == R_BOOL) || (var_type == R_PENDING)))
	    {
	      var->v->type = R_GOTO;
 	      add_triple_to_ptree(prog, va_make_triple(store_i, descr_store_i, 2, var->v, v));
	      return(v);
	    }
	  if ((val_type == R_BOOL) &&
	      (pointer_p(var_type)))
	    {
 	      add_triple_to_ptree(prog, va_make_triple(store_i, descr_store_i, 2, var->v, v)); /* TODO: free here? */
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
  sprintf(buf,"d%d(%.4f) =", args[0], dbls[args[0]]);
  for (i = start; i < num_args; i++)
    {
      snprintf(str, 32, " d%d(%.4f) %s", args[i], dbls[args[i]], func);
      strcat(buf, str);
    }
  snprintf(str, 32, " d%d(%.4f))", args[num_args], dbls[args[num_args]]);
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
  sprintf(buf,"i%d(%d) =", args[0], ints[args[0]]);
  for (i = start; i < num_args; i++)
    {
      snprintf(str, 32, " i%d(%d) %s", args[i], ints[args[i]], func);
      strcat(buf, str);
    }
  snprintf(str, 32, " i%d(%d))", args[num_args], ints[args[num_args]]);
  strcat(buf, str);
  FREE(str);
  return(buf);
}

static xen_value *fixup_if_pending(ptree *pt, xen_value *sf, int new_type)
{
  xen_var *var;
  xen_value *newv;
  if (sf->type == R_PENDING)
    {
      /* fprintf(stderr,"fixup at %d (%p)...", sf->addr, (sf->addr > 0) ? (snd_fd *)(pt->ints[sf->addr]) : NULL); */
      var = find_pending_var_in_ptree_via_xen_value(pt, sf);
      /* sf->addr is a negative number as part of the pending_tag system */
      if (var == NULL) return(NULL);
      switch (new_type)
	{
	case R_FLOAT: newv = make_xen_value(R_FLOAT, add_dbl_to_ptree(pt, 0.0), R_VARIABLE); break;
	case R_STRING: newv = make_xen_value(R_STRING, add_string_to_ptree(pt, NULL), R_VARIABLE); break;
	default: newv = make_xen_value(new_type, add_int_to_ptree(pt, 0), R_VARIABLE); break;
	}
      FREE(sf);
      var->v->type = new_type;
      var->v->addr = newv->addr;
      /* fprintf(stderr,"found %s, %p (%d, %s %s)\n",var->name, var, var->v->addr, type_name(var->v->type), describe_xen_var(var, pt->ints, pt->dbls)); */
      return(newv);
    }
  return(sf);
}

static void fixup_all_pending_args(ptree *pt, xen_value **args, int num_args, int new_type)
{
  int i;
  if (current_optimization > 3)
    for (i = 1; i <= num_args; i++)
      if (args[i])
	args[i] = fixup_if_pending(pt, args[i], new_type);
}

static int float_all_args(ptree *prog, int num_args, xen_value **args, int float_result)
{
  int i, j;
  xen_value *old_loc;
  fixup_all_pending_args(prog, args, num_args, (float_result) ? R_FLOAT : R_INT);
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

static void multiply_f2(int *args, int *ints, Float *dbls) {dbls[args[0]] = (FLOAT_ARG_1 * FLOAT_ARG_2);}
static char *descr_multiply_f2(int *args, int *ints, Float *dbls) {return(describe_dbl_args("*", 2, args, dbls, 1));}

static void multiply_f3(int *args, int *ints, Float *dbls) {FLOAT_RESULT = (FLOAT_ARG_1 * FLOAT_ARG_2 * FLOAT_ARG_3);}
static char *descr_multiply_f3(int *args, int *ints, Float *dbls) {return(describe_dbl_args("*", 3, args, dbls, 1));}

static void multiply_fn(int *args, int *ints, Float *dbls) 
{
  int i, n;
  n = ints[args[1]];
  FLOAT_RESULT = dbls[args[2]];
  for (i = 1; i < n; i++) FLOAT_RESULT *= dbls[args[i + 2]];
}
static char *descr_multiply_fn(int *args, int *ints, Float *dbls) {return(describe_dbl_args("*", ints[args[1]] + 1, args, dbls, 2));}

static void multiply_i2(int *args, int *ints, Float *dbls) {INT_RESULT = (INT_ARG_1 * INT_ARG_2);}
static char *descr_multiply_i2(int *args, int *ints, Float *dbls) {return(describe_int_args("*", 2, args, ints, 1));}

static void multiply_i3(int *args, int *ints, Float *dbls) {INT_RESULT = (INT_ARG_1 * INT_ARG_2 * INT_ARG_3);}
static char *descr_multiply_i3(int *args, int *ints, Float *dbls) {return(describe_int_args("*", 3, args, ints, 1));}

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
  if (float_result)
    {
      if (num_args == 2) return(package(prog, R_FLOAT, multiply_f2, descr_multiply_f2, args, num_args));
      if (num_args == 3) return(package(prog, R_FLOAT, multiply_f3, descr_multiply_f3, args, num_args));
      return(package_n(prog, R_FLOAT, multiply_fn, descr_multiply_fn, args, num_args));
    }
  else
    {
      if (num_args == 2) return(package(prog, R_INT, multiply_i2, descr_multiply_i2, args, num_args)); 
      if (num_args == 3) return(package(prog, R_INT, multiply_i3, descr_multiply_i3, args, num_args));
      return(package_n(prog, R_INT, multiply_in, descr_multiply_in, args, num_args));
    }
  return(run_warn("* trouble"));
}


/* ---------------- add ---------------- */

static void add_f2(int *args, int *ints, Float *dbls) {dbls[args[0]] = (FLOAT_ARG_1 + FLOAT_ARG_2);}
static char *descr_add_f2(int *args, int *ints, Float *dbls) {return(describe_dbl_args("+", 2, args, dbls, 1));}

static void add_f3(int *args, int *ints, Float *dbls) {FLOAT_RESULT = (FLOAT_ARG_1 + FLOAT_ARG_2 + FLOAT_ARG_3);}
static char *descr_add_f3(int *args, int *ints, Float *dbls) {return(describe_dbl_args("+", 3, args, dbls, 1));}

static void add_fn(int *args, int *ints, Float *dbls) 
{
  int i, n;
  n = ints[args[1]];
  FLOAT_RESULT = dbls[args[2]];
  for (i = 1; i < n; i++) FLOAT_RESULT += dbls[args[i + 2]];
}
static char *descr_add_fn(int *args, int *ints, Float *dbls) {return(describe_dbl_args("+", ints[args[1]] + 1, args, dbls, 2));}

static void add_i2(int *args, int *ints, Float *dbls) {INT_RESULT = (INT_ARG_1 + INT_ARG_2);}
static char *descr_add_i2(int *args, int *ints, Float *dbls) {return(describe_int_args("+", 2, args, ints, 1));}

static void add_i3(int *args, int *ints, Float *dbls) {INT_RESULT = (INT_ARG_1 + INT_ARG_2 + INT_ARG_3);}
static char *descr_add_i3(int *args, int *ints, Float *dbls) {return(describe_int_args("+", 3, args, ints, 1));}

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
  if (float_result)
    {
      if (num_args == 2) return(package(prog, R_FLOAT, add_f2, descr_add_f2, args, num_args));
      if (num_args == 3) return(package(prog, R_FLOAT, add_f3, descr_add_f3, args, num_args));
      return(package_n(prog, R_FLOAT, add_fn, descr_add_fn, args, num_args));
    }
  else
    {
      if (num_args == 2) return(package(prog, R_INT, add_i2, descr_add_i2, args, num_args));
      if (num_args == 3) return(package(prog, R_INT, add_i3, descr_add_i3, args, num_args));
      return(package_n(prog, R_INT, add_in, descr_add_in, args, num_args));
    }
  return(run_warn("+ trouble"));
}

/* ---------------- subtract ---------------- */

static void subtract_f1(int *args, int *ints, Float *dbls) {dbls[args[0]] = -(FLOAT_ARG_1);}
static char *descr_subtract_f1(int *args, int *ints, Float *dbls) {return(mus_format("d%d(%.4f) = -d%d(%.4f)", args[0], FLOAT_RESULT, args[1], FLOAT_ARG_1));}

static void subtract_f2(int *args, int *ints, Float *dbls) {dbls[args[0]] = (FLOAT_ARG_1 - FLOAT_ARG_2);}
static char *descr_subtract_f2(int *args, int *ints, Float *dbls) {return(describe_dbl_args("-", 2, args, dbls, 1));}

static void subtract_f3(int *args, int *ints, Float *dbls) {FLOAT_RESULT = (FLOAT_ARG_1 - FLOAT_ARG_2 - FLOAT_ARG_3);}
static char *descr_subtract_f3(int *args, int *ints, Float *dbls) {return(describe_dbl_args("-", 3, args, dbls, 1));}

static void subtract_fn(int *args, int *ints, Float *dbls) 
{
  int i, n;
  n = ints[args[1]];
  FLOAT_RESULT = dbls[args[2]];
  for (i = 1; i < n; i++) FLOAT_RESULT -= dbls[args[i + 2]];
}
static char *descr_subtract_fn(int *args, int *ints, Float *dbls) {return(describe_dbl_args("-", ints[args[1]] + 1, args, dbls, 2));}

static void subtract_i1(int *args, int *ints, Float *dbls) {INT_RESULT = -(INT_ARG_1);}
static char *descr_subtract_i1(int *args, int *ints, Float *dbls) {return(mus_format("i%d(%d) = -i%d(%d)", args[0], INT_RESULT, args[1], INT_ARG_1));}

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
static char *descr_one_minus_f(int *args, int *ints, Float *dbls) {return(mus_format("d%d(%.4f) = d%d(%.4f) - 1.0", args[0], FLOAT_RESULT, args[1], FLOAT_ARG_1));}
static void one_minus_i(int *args, int *ints, Float *dbls) {INT_RESULT = INT_ARG_1 - 1;}
static char *descr_one_minus_i(int *args, int *ints, Float *dbls) {return(mus_format("i%d(%d) = i%d(%d) - 1", args[0], INT_RESULT, args[1], INT_ARG_1));}
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
static char *descr_one_plus_f(int *args, int *ints, Float *dbls) {return(mus_format("d%d(%.4f) = d%d(%.4f) + 1.0", args[0], FLOAT_RESULT, args[1], FLOAT_ARG_1));}
static void one_plus_i(int *args, int *ints, Float *dbls) {INT_RESULT = INT_ARG_1 + 1;}
static char *descr_one_plus_i(int *args, int *ints, Float *dbls) {return(mus_format("i%d(%d) = i%d(%d) + 1", args[0], INT_RESULT, args[1], INT_ARG_1));}
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

static void divide_f1(int *args, int *ints, Float *dbls) {dbls[args[0]] = (1.0 / FLOAT_ARG_1);}
static char *descr_divide_f1(int *args, int *ints, Float *dbls) {return(mus_format("d%d(%.4f) = 1.0 / d%d(%.4f)", args[0], FLOAT_RESULT, args[1], FLOAT_ARG_1));}

static void divide_f2(int *args, int *ints, Float *dbls) {dbls[args[0]] = (FLOAT_ARG_1 / FLOAT_ARG_2);}
static char *descr_divide_f2(int *args, int *ints, Float *dbls) {return(describe_dbl_args("/", 2, args, dbls, 1));}

static void divide_f3(int *args, int *ints, Float *dbls) {FLOAT_RESULT = (FLOAT_ARG_1 / (FLOAT_ARG_2 * FLOAT_ARG_3));}
static char *descr_divide_f3(int *args, int *ints, Float *dbls) 
{
  return(mus_format("d%d(%.4f) = d%d(%.4f) / (d%d(%.4f) * d%d(%.4f))", args[0], FLOAT_RESULT, args[1], FLOAT_ARG_1, args[2], FLOAT_ARG_2, args[3], FLOAT_ARG_3));
}

static void divide_fn(int *args, int *ints, Float *dbls) 
{
  int i, n;
  Float divisor = 1.0;
  n = ints[args[1]];
  for (i = 1; i < n; i++) divisor *= dbls[args[i + 2]];
  FLOAT_RESULT = dbls[args[2]] / divisor;
}
static char *descr_divide_fn(int *args, int *ints, Float *dbls)
{
  char *buf, *str;
  int i, num_args;
  num_args = ints[args[1]] + 1;
  buf = (char *)CALLOC(256, sizeof(char));
  str = (char *)CALLOC(32, sizeof(char));
  sprintf(buf,"d%d(%.4f) = d%d(%.4f) / (", args[0], dbls[args[0]], args[2], dbls[args[2]]);
  for (i = 3; i < num_args; i++)
    {
      snprintf(str, 32, " d%d(%.4f) *", args[i], dbls[args[i]]);
      strcat(buf, str);
    }
  snprintf(str, 32, " d%d(%.4f))", args[num_args], dbls[args[num_args]]);
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
  sprintf(buf,"i%d(%d) =", args[0], ints[args[0]]);
  for (i = start; i < num_args; i++)
    {
      snprintf(str, 32, " d%d(%.4f) %s", args[i], dbls[args[i]], func);
      strcat(buf, str);
    }
  snprintf(str, 32, " d%d(%.4f))", args[num_args], dbls[args[num_args]]);
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
  fixup_all_pending_args(prog, args, num_args, R_FLOAT);
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
  return(mus_format("d%d(%.4f) = max(d%d(%.4f), d%d(%.4f))",
		    args[0], FLOAT_RESULT, args[1], FLOAT_ARG_1, args[2], FLOAT_ARG_2));
}
static void max_fn(int *args, int *ints, Float *dbls)
{
  int i, n;
  Float mx;
  n = ints[args[1]];
  mx = dbls[args[2]];
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
  sprintf(buf,"i%d(%d) = %s(", args[0], ints[args[0]], which);
  for (i = 2; i <= n; i++)
    {
      snprintf(str, 32, "d%d(%.4f) ", args[i], dbls[args[i]]);
      strcat(buf, str);
    }
  snprintf(str, 32, "d%d(%.4f))", args[n + 1], dbls[args[n + 1]]);
  strcat(buf, str);
  FREE(str);
  return(buf);
}
static char *descr_max_fn(int *args, int *ints, Float *dbls) {return(descr_max_min_fn(args, ints, dbls, "max"));}
static void max_i2(int *args, int *ints, Float *dbls) {INT_RESULT = (INT_ARG_1 > INT_ARG_2) ? INT_ARG_1 : INT_ARG_2;}
static char *descr_max_i2(int *args, int *ints, Float *dbls)
{
  return(mus_format("i%d(%d) = max(i%d(%d), i%d(%d))",
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
  sprintf(buf,"i%d(%d) = %s(", args[0], ints[args[0]], which);
  for (i = 2; i <= n; i++)
    {
      snprintf(str, 32, "i%d(%d) ", args[i], ints[args[i]]);
      strcat(buf, str);
    }
  snprintf(str, 32, "i%d(%d))", args[n + 1], ints[args[n + 1]]);
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
  return(mus_format("d%d(%.4f) = min(d%d(%.4f), d%d(%.4f))",
		    args[0], FLOAT_RESULT, args[1], FLOAT_ARG_1, args[2], FLOAT_ARG_2));
}
static void min_fn(int *args, int *ints, Float *dbls)
{
  int i, n;
  Float mx;
  n = ints[args[1]];
  mx = dbls[args[2]];
  for (i = 2; i <= n; i++)
    if (dbls[args[i + 1]] < mx) mx = dbls[args[i + 1]];
  FLOAT_RESULT = mx;
}
static char *descr_min_fn(int *args, int *ints, Float *dbls) {return(descr_max_min_fn(args, ints, dbls, "min"));}
static void min_i2(int *args, int *ints, Float *dbls) {INT_RESULT = (INT_ARG_1 > INT_ARG_2) ? INT_ARG_2 : INT_ARG_1;}
static char *descr_min_i2(int *args, int *ints, Float *dbls)
{
  return(mus_format("i%d(%d) = min(i%d(%d), i%d(%d))",
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
static char *descr_not_b(int *args, int *ints, Float *dbls) {return(mus_format("i%d(%d) = (!(i%d(%d)))", args[0], BOOL_RESULT, args[1], INT_ARG_1));}
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
  return(mus_format("i%d(%d) = eq?(i%d(%d), i%d(%d))", args[0], BOOL_RESULT, args[1], INT_ARG_1, args[2], INT_ARG_2));
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
  return(mus_format("i%d(%d) = eqv?(d%d(%.4f), d%d(%.4f))", args[0], BOOL_RESULT, args[1], FLOAT_ARG_1, args[2], FLOAT_ARG_2));
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
  if (args[1]->type == R_FLOAT)
    return(package(prog, R_BOOL, eqv_fb, descr_eqv_fb, args, 2));
  return(package(prog, R_BOOL, eq_b, descr_eq_b, args, 2));
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
static char *descr_odd_i(int *args, int *ints, Float *dbls) {return(mus_format("i%d(%d) = odd?(i%d(%d))", args[0], BOOL_RESULT, args[1], INT_ARG_1));}
static xen_value *odd_p(ptree *prog, xen_value **args, int constants)
{
  args[1] = convert_to_int(prog, args[1]);
  if (args[1] == NULL) return(run_warn("odd? can't convert arg"));
  if (constants == 1)
    return(make_xen_value(R_BOOL, add_int_to_ptree(prog, prog->ints[args[1]->addr] & 1), R_CONSTANT));
  return(package(prog, R_BOOL, odd_i, descr_odd_i, args, 1));
}

static void even_i(int *args, int *ints, Float *dbls) {BOOL_RESULT = (!(INT_ARG_1 & 1));}
static char *descr_even_i(int *args, int *ints, Float *dbls) {return(mus_format("i%d(%d) = even?(i%d(%d))", args[0], BOOL_RESULT, args[1], INT_ARG_1));}
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
static char *descr_ ## CName ## _i(int *args, int *ints, Float *dbls) {return(mus_format("i%d(%d) = (i%d(%d)" #COp " 0)", args[0], BOOL_RESULT, args[1], INT_ARG_1));} \
static void CName ## _f(int *args, int *ints, Float *dbls) {BOOL_RESULT = (FLOAT_ARG_1 COp 0.0);} \
static char *descr_ ## CName ## _f(int *args, int *ints, Float *dbls) {return(mus_format("i%d(%d) = (d%d(%.4f) " #COp " 0.0)", args[0], BOOL_RESULT, args[1], FLOAT_ARG_1));} \
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
  return(mus_format("d%d(%.4f) = " #CName "(d%d(%.4f))", args[0], FLOAT_RESULT, args[1], FLOAT_ARG_1)); \
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


static void atan1_f(int *args, int *ints, Float *dbls) {FLOAT_RESULT = atan(FLOAT_ARG_1);}
static char *descr_atan1_f(int *args, int *ints, Float *dbls) {return(mus_format("d%d(%.4f) = atan(d%d(%.4f))", args[0], FLOAT_RESULT, args[1], FLOAT_ARG_1));}
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
  return(mus_format("d%d(%.4f) = atan2(d%d(%.4f), d%d(%.4f))", args[0], FLOAT_RESULT, args[1], FLOAT_ARG_1, args[2], FLOAT_ARG_2));
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
static char *descr_round_f(int *args, int *ints, Float *dbls) {return(mus_format("d%d(%.4f) = round(d%d(%.4f))", args[0], FLOAT_RESULT, args[1], FLOAT_ARG_1));}
static xen_value *round_1(ptree *prog, xen_value **args, int constants)
{
  /* (round 1) -> 1.0! */
  if (constants == 1)
    {
      if (args[1]->type == R_INT) 
	return(make_xen_value(R_INT, add_int_to_ptree(prog, prog->ints[args[1]->addr]), R_CONSTANT)); /* r5rs spec says return int here */
      else return(make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, f_round(prog->dbls[args[1]->addr])), R_CONSTANT));
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
static char *descr_truncate_f(int *args, int *ints, Float *dbls) {return(mus_format("d%d(%.4f) = truncate(d%d(%.4f))", args[0], FLOAT_RESULT, args[1], FLOAT_ARG_1));}
static xen_value *truncate_1(ptree *prog, xen_value **args, int constants)
{
  if (constants == 1)
    {
      if (args[1]->type == R_INT) 
	return(make_xen_value(R_INT, add_int_to_ptree(prog, prog->ints[args[1]->addr]), R_CONSTANT));
      else return(make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, f_truncate(prog->dbls[args[1]->addr])), R_CONSTANT));
    }
  if (args[1]->type == R_INT)
    return(package(prog, R_FLOAT, store_i_f, descr_store_i_f, args,1));
  return(package(prog, R_FLOAT, truncate_f, descr_truncate_f, args, 1));
}

/* ---------------- floor ---------------- */

static void floor_f(int *args, int *ints, Float *dbls) {FLOAT_RESULT = floor(FLOAT_ARG_1);}
static char *descr_floor_f(int *args, int *ints, Float *dbls) {return(mus_format("d%d(%.4f) = floor(d%d(%.4f))", args[0], FLOAT_RESULT, args[1], FLOAT_ARG_1));}
static xen_value *floor_1(ptree *prog, xen_value **args, int constants)
{
  if (constants == 1)
    {
      if (args[1]->type == R_INT) 
	return(make_xen_value(R_INT, add_int_to_ptree(prog, prog->ints[args[1]->addr]), R_CONSTANT));
      else return(make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, floor(prog->dbls[args[1]->addr])), R_CONSTANT));
    }
  if (args[1]->type == R_INT)
    return(package(prog, R_FLOAT, store_i_f, descr_store_i_f, args,1));
  return(package(prog, R_FLOAT, floor_f, descr_floor_f, args, 1));
}

/* ---------------- ceiling ---------------- */

static void ceiling_f(int *args, int *ints, Float *dbls) {FLOAT_RESULT = ceil(FLOAT_ARG_1);}
static char *descr_ceiling_f(int *args, int *ints, Float *dbls) {return(mus_format("d%d(%.4f) = ceil(d%d(%.4f))", args[0], FLOAT_RESULT, args[1], FLOAT_ARG_1));}
static xen_value *ceiling_1(ptree *prog, xen_value **args, int constants)
{
  if (constants == 1)
    {
      if (args[1]->type == R_INT) 
	return(make_xen_value(R_INT, add_int_to_ptree(prog, prog->ints[args[1]->addr]), R_CONSTANT));
      else return(make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, ceil(prog->dbls[args[1]->addr])), R_CONSTANT));
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
static char *descr_i2e_f(int *args, int *ints, Float *dbls) {return(mus_format("i%d(%d) = floor(d%d(%.4f) + 0.5)", args[0], INT_RESULT, args[1], FLOAT_ARG_1));}
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
  return(mus_format("i%d(%d) = gcd(i%d(%d), i%d(%d))", args[0], INT_RESULT, args[1], INT_ARG_1, args[2], INT_ARG_2));
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
  return(mus_format("i%d(%d) = lcm(i%d(%d), i%d(%d))", args[0], INT_RESULT, args[1], INT_ARG_1, args[2], INT_ARG_2));
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
  return(mus_format("i%d(%d) = modulo(i%d(%d), i%d(%d))", args[0], INT_RESULT, args[1], INT_ARG_1, args[2], INT_ARG_2));
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
  return(mus_format("i%d(%d) = (i%d(%d) %% i%d(%d))", args[0], INT_RESULT, args[1], INT_ARG_1, args[2], INT_ARG_2));
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
  return(mus_format("i%d(%d) = (i%d(%d) / i%d(%d))", args[0], INT_RESULT, args[1], INT_ARG_1, args[2], INT_ARG_2));
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
  return(mus_format("i%d(%d) = (i%d(%d) & i%d(%d))", args[0], INT_RESULT, args[1], INT_ARG_1, args[2], INT_ARG_2));
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
  return(mus_format("i%d(%d) = (i%d(%d) | i%d(%d))", args[0], INT_RESULT, args[1], INT_ARG_1, args[2], INT_ARG_2));
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
  return(mus_format("i%d(%d) = logxor(i%d(%d), i%d(%d))", args[0], INT_RESULT, args[1], INT_ARG_1, args[2], INT_ARG_2));
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
static char *descr_lognot_i(int *args, int *ints, Float *dbls) {return(mus_format("i%d(%d) = ~i%d(%d)", args[0], INT_RESULT, args[1], INT_ARG_1));}
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
  return(mus_format("i%d(%d) = ash(i%d(%d), i%d(%d))", args[0], INT_RESULT, args[1], INT_ARG_1, args[2], INT_ARG_2));
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
  return(mus_format("d%d(%.4f) = pow(d%d(%.4f), d%d(%.4f))", args[0], FLOAT_RESULT, args[1], FLOAT_ARG_1, args[2], FLOAT_ARG_2));
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
static char *descr_abs_f(int *args, int *ints, Float *dbls) {return(mus_format("d%d(%.4f) = fabs(d%d(%.4f))", args[0], FLOAT_RESULT, args[1], FLOAT_ARG_1));}
static void abs_i(int *args, int *ints, Float *dbls) {INT_RESULT = abs(INT_ARG_1);}
static char *descr_abs_i(int *args, int *ints, Float *dbls) {return(mus_format("i%d(%d) = abs(i%d(%d))", args[0], INT_RESULT, args[1], INT_ARG_1));}
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
  return(mus_format("d%d(%.4f) = random(d%d(%.4f))", args[0], FLOAT_RESULT, args[1], FLOAT_ARG_1));
}
static void random_i(int *args, int *ints, Float *dbls) {INT_RESULT = scm_c_random(scm_c_default_rstate(), INT_ARG_1);}
static char *descr_random_i(int *args, int *ints, Float *dbls) 
{
  return(mus_format("i%d(%d) = random(i%d(%d))", args[0], INT_RESULT, args[1], INT_ARG_1));
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
  return(mus_format("i%d(%d) = " #SName "(i%d(#\\%c))", args[0], BOOL_RESULT, args[1], (char)(INT_ARG_1))); \
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
  return(mus_format("i%d(#\\%c) = " #SName "(i%d(#\\%c))", args[0], (char)(INT_RESULT), args[1], (char)(INT_ARG_1))); \
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
  STRING_RESULT = (char *)CALLOC(n + 1, sizeof(char));
  for (i = 1; i <= n; i++) STRING_RESULT[i - 1] = (char)(ints[args[i + 1]]);
}
static char *descr_string_n(int *args, int *ints, Float *dbls)
{
  int i, n;
  char *buf, *temp;
  n = ints[args[1]];
  buf = (char *)CALLOC(32 + 16 * n, sizeof(char));
  temp = (char *)CALLOC(16, sizeof(char));
  sprintf(buf, "i%d(\"%s\") = string(i%d(#\\%c)", args[0], STRING_RESULT, args[2], (char)(ints[args[2]]));
  for (i = 2; i <= n; i++)
    {
      sprintf(temp, ", i%d(#\\%c)", args[i + 1], (char)(ints[args[i + 1]]));
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
  return(mus_format("i%d(%d) = string-length(i%d(\"%s\"))", args[0], INT_RESULT, args[1], STRING_ARG_1));
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
  return(mus_format("i%d(\"%s\") = string-copy(i%d(\"%s\"))", args[0], STRING_RESULT, args[1], STRING_ARG_1));
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
  /* fprintf(stderr,"%s (%d at %d) filled with %c -> ", STRING_RESULT, len, args[0], (char)(CHAR_ARG_1)); */
  if (len > 0)
    memset((void *)(STRING_RESULT), (char)(CHAR_ARG_1), len);
  /* fprintf(stderr,"%s (%d)\n", STRING_RESULT, snd_strlen(STRING_RESULT)); */
}
static char *descr_strfill_1(int *args, int *ints, Float *dbls) 
{
  return(mus_format("i%d(\"%s\") = string-fill!(i%d(#\\%c))", args[0], STRING_RESULT, args[1], CHAR_ARG_1));
}
static xen_value *string_fill_1(ptree *pt, xen_value **args)
{
  if ((args[1]->constant == R_CONSTANT) || (args[1]->type != R_STRING) || (args[2]->type != R_CHAR))
    return(run_warn("bad args to string-fill!"));
  add_triple_to_ptree(pt, va_make_triple(strfill_1, descr_strfill_1, 2, args[1], args[2])); /* shifts back one */
  return(make_xen_value(R_BOOL, -1, R_CONSTANT));
}

static void strset_1(int *args, int *ints, Float *dbls) {STRING_RESULT[INT_ARG_1] = (char)(CHAR_ARG_2);}
static char *descr_strset_1(int *args, int *ints, Float *dbls) 
{
  return(mus_format("i%d(\"%s\") = string-set!(i%d(%d), i%d(#\\%c))", 
		    args[0], STRING_RESULT, args[1], INT_ARG_1, args[2], CHAR_ARG_2));
}
static xen_value *string_set_1(ptree *pt, xen_value **args)
{
  if ((args[1]->constant == R_CONSTANT) || (args[1]->type != R_STRING) || (args[2]->type != R_INT) || (args[3]->type != R_CHAR))
    return(run_warn("bad args to string-set!"));
  add_triple_to_ptree(pt, va_make_triple(strset_1, descr_strset_1, 3, args[1], args[2], args[3])); /* shifts back one */
  return(make_xen_value(R_BOOL, -1, R_CONSTANT));
}

static void strref_1(int *args, int *ints, Float *dbls) {CHAR_RESULT = (char)(STRING_ARG_1[INT_ARG_2]);}
static char *descr_strref_1(int *args, int *ints, Float *dbls) 
{
  return(mus_format("i%d(#\\%c) = string-ref(i%d(\"%s\"), i%d(%d))", 
		    args[0], (ints[args[0]] == 0) ? '@' : CHAR_RESULT, 
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


static void display_str(int *args, int *ints, Float *dbls) {fprintf(stdout, "\"%s\"", STRING_ARG_1);}
static char *descr_display_str(int *args, int *ints, Float *dbls) {return(mus_format("display(i%d(\"%s\"))", args[1], STRING_ARG_1));}
static void display_int(int *args, int *ints, Float *dbls) {fprintf(stdout, "%d", INT_ARG_1);}
static char *descr_display_int(int *args, int *ints, Float *dbls) {return(mus_format("display(i%d(%d))", args[1], INT_ARG_1));}
static void display_flt(int *args, int *ints, Float *dbls) {fprintf(stdout, "%.6f", FLOAT_ARG_1);}
static char *descr_display_flt(int *args, int *ints, Float *dbls) {return(mus_format("display(i%d(%.4f))", args[1], FLOAT_ARG_1));}
static void display_clm(int *args, int *ints, Float *dbls) {fprintf(stdout, "%s", mus_describe((mus_any *)(INT_ARG_1)));}
static char *descr_display_clm(int *args, int *ints, Float *dbls) {return(mus_format("display(i%d(%p))", args[1], ((mus_any *)(INT_ARG_1))));}
static void display_vct(int *args, int *ints, Float *dbls) {fprintf(stdout, "%s", vct_to_string((vct *)(INT_ARG_1)));}
static char *descr_display_vct(int *args, int *ints, Float *dbls) {return(mus_format("display(i%d(%p))", args[1], ((vct *)(INT_ARG_1))));}
static void display_rd(int *args, int *ints, Float *dbls) {fprintf(stdout, "%s", sf_to_string((snd_fd *)(INT_ARG_1)));}
static char *descr_display_rd(int *args, int *ints, Float *dbls) {return(mus_format("display(i%d(%p))", args[1], ((snd_fd *)(INT_ARG_1))));}
static void display_chr(int *args, int *ints, Float *dbls) {fprintf(stdout, "#\\%c", (char)(INT_ARG_1));}
static char *descr_display_chr(int *args, int *ints, Float *dbls) {return(mus_format("display(i%d(#\\%c))", args[1], (char)(INT_ARG_1)));}
static void display_bool(int *args, int *ints, Float *dbls) {fprintf(stdout, "%s", (INT_ARG_1) ? "#t" : "#f");}
static char *descr_display_bool(int *args, int *ints, Float *dbls) {return(mus_format("display(i%d(%s))", args[1], (INT_ARG_1) ? "#t" : "#f"));}
static void display_con(int *args, int *ints, Float *dbls) {fprintf(stdout, "continuation");}
static char *descr_display_con(int *args, int *ints, Float *dbls) {return(mus_format("display(i%d(continuation))", args[1]));}
static void display_func(int *args, int *ints, Float *dbls) {fprintf(stdout, "%s", describe_ptree((ptree *)(INT_ARG_1)));}
static char *descr_display_func(int *args, int *ints, Float *dbls) {return(mus_format("display(i%d(%p))", args[1], (ptree *)(INT_ARG_1)));}
static xen_value *display_1(ptree *pt, xen_value **args, int num_args)
{
  switch (args[1]->type)
    {
    case R_STRING: return(package(pt, R_BOOL, display_str, descr_display_str, args, 1)); break;
    case R_INT: return(package(pt, R_BOOL, display_int, descr_display_int, args, 1)); break;
    case R_FLOAT: return(package(pt, R_BOOL, display_flt, descr_display_flt, args, 1)); break;
    case R_CLM: return(package(pt, R_BOOL, display_clm, descr_display_clm, args, 1)); break;
    case R_READER: return(package(pt, R_BOOL, display_rd, descr_display_rd, args, 1)); break;
    case R_VCT: return(package(pt, R_BOOL, display_vct, descr_display_vct, args, 1)); break;
    case R_BOOL: return(package(pt, R_BOOL, display_bool, descr_display_bool, args, 1)); break;
    case R_CHAR: return(package(pt, R_BOOL, display_chr, descr_display_chr, args, 1)); break;
    case R_GOTO: return(package(pt, R_BOOL, display_con, descr_display_con, args, 1)); break;
    case R_FUNCTION: return(package(pt, R_BOOL, display_func, descr_display_func, args, 1)); break;
    }
  return(NULL);
}

static void snd_print_s(int *args, int *ints, Float *dbls) {listener_append(get_global_state(), STRING_ARG_1);}
static char *descr_snd_print_s(int *args, int *ints, Float *dbls) {return(mus_format("snd_print(i%d(\"%s\"))", args[1], STRING_ARG_1));}
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
  return(mus_format("i%d(\"%s\") = make-string(i%d(%d))", args[0], STRING_RESULT, args[1], INT_ARG_1));
}
static char *descr_strmake_2(int *args, int *ints, Float *dbls) 
{
  return(mus_format("i%d(\"%s\") = make-string(i%d(%d), i%d(#\\%c))", 
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
  return(mus_format("i%d(\"%s\") = substring(i%d(\"%s\"), i%d(%d), i%d(%d))", 
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
  int i, n, len = 64;
  char *buf, *temp;
  n = ints[args[1]];
  buf = (char *)CALLOC(32 + 16 * n, sizeof(char));
  temp = (char *)CALLOC(len, sizeof(char));
  if (bool_result)
    sprintf(buf, "i%d(%d) = %s(i%d(\"%s\")", args[0], BOOL_RESULT, which, args[2], (char *)(ints[args[2]]));
  else sprintf(buf, "i%d(\"%s\") = %s(i%d(\"%s\")", args[0], STRING_RESULT, which, args[2], (char *)(ints[args[2]]));
  for (i = 2; i <= n; i++)
    {
      sprintf(temp, ", i%d(\"%s\")", args[i + 1], (char *)(ints[args[i + 1]]));
      strcat(buf, temp);
    }
  FREE(temp);
  strcat(buf, ")");
  return(buf);
}
/* TODO: collapse constants */
static char *string_append(ptree *pt, xen_value **args, int num_args)
{
  int i, len = 0;
  char *str;
  for (i = 0; i < num_args; i++)
    len += snd_strlen((char *)(pt->ints[args[i + 1]->addr]));
  str = (char *)CALLOC(len + 1, sizeof(char));
  for (i = 0; i < num_args; i++)
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
  for (i = 1; i <= n; i++) strcat(STRING_RESULT, (char *)(ints[args[i + 1]]));
}
static char *descr_appendstr_n(int *args, int *ints, Float *dbls)  {return(descr_strn(args, ints, dbls, "string-append", FALSE));}
static xen_value *string_append_1(ptree *pt, xen_value **args, int num_args, int constants)
{
  if (num_args == 0)
    return(make_xen_value(R_STRING, add_string_to_ptree(pt, (char *)CALLOC(1, sizeof(char))), R_CONSTANT));
  if (num_args == constants)
    return(make_xen_value(R_STRING, add_string_to_ptree(pt, string_append(pt, args, num_args)), R_CONSTANT));
  return(package_n(pt, R_STRING, appendstr_n, descr_appendstr_n, args, num_args));
}

/* TODO: precheck constants */
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
  if (num_args <= 1) return(make_xen_value(R_BOOL, add_int_to_ptree(pt, TRUE), R_CONSTANT)); \
  if (num_args == constants) return(make_xen_value(R_BOOL, add_int_to_ptree(pt, str_ ## CName ## _n(pt, args, num_args)), R_CONSTANT)); \
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
  return(mus_format("i%d(\"%s\") = number->string(d%d(%.4f))", args[0], STRING_RESULT, args[1], FLOAT_ARG_1));
}
static char *descr_number2string_f2(int *args, int *ints, Float *dbls) 
{
  return(mus_format("i%d(\"%s\") = number->string(d%d(%.4f), i%d(%d))", args[0], STRING_RESULT, args[1], FLOAT_ARG_1, args[2], INT_ARG_2));
}
static char *descr_number2string_i1(int *args, int *ints, Float *dbls) 
{
  return(mus_format("i%d(\"%s\") = number->string(i%d(%d))", args[0], STRING_RESULT, args[1], INT_ARG_1));
}
static char *descr_number2string_i2(int *args, int *ints, Float *dbls) 
{
  return(mus_format("i%d(\"%s\") = number->string(i%d(%d), i%d(%d))", args[0], STRING_RESULT, args[1], INT_ARG_1, args[2], INT_ARG_2));
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




/* ---------------- sample-reader stuff ---------------- */

static void reader_f(int *args, int *ints, Float *dbls) {FLOAT_RESULT = read_sample_to_float(((snd_fd *)(INT_ARG_1)));}
static char *descr_reader(int *args, int *ints, Float *dbls, char *which) 
{
  return(mus_format("d%d(%.4f) = %s(i%d(%p))", args[0], FLOAT_RESULT, which, args[1], ((snd_fd *)(INT_ARG_1))));
}
static char *descr_reader_f(int *args, int *ints, Float *dbls) {return(descr_reader(args, ints, dbls, "read-sample"));}
static xen_value *reader_0(ptree *prog, xen_value **args, xen_value *sf)
{
  if ((sf = fixup_if_pending(prog, sf, R_READER)) == NULL) return(run_warn("sample-reader bad arg"));
  if (args[0]) FREE(args[0]);
  args[0] = make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, 0.0), R_VARIABLE);
  add_triple_to_ptree(prog, va_make_triple(reader_f, descr_reader_f, 2, args[0], sf));
  return(args[0]);
}
static xen_value *reader_1(ptree *prog, xen_value **args) 
{
  if ((args[1] = fixup_if_pending(prog, args[1], R_READER)) == NULL) return(run_warn("sample-reader bad arg"));
  return(package(prog, R_FLOAT, reader_f, descr_reader_f, args, 1));
}

static char *descr_next_reader_f(int *args, int *ints, Float *dbls) {return(descr_reader(args, ints, dbls, "next-sample"));}
static void next_reader_f(int *args, int *ints, Float *dbls) {FLOAT_RESULT = protected_next_sample_to_float(((snd_fd *)(INT_ARG_1)));}
static xen_value *next_sample_1(ptree *prog, xen_value **args) 
{
  if ((args[1] = fixup_if_pending(prog, args[1], R_READER)) == NULL) return(run_warn("next-sample bad arg"));
  /* fprintf(stderr,"next sample %p at %d\n", (snd_fd *)(prog->ints[args[1]->addr]), args[1]->addr); */
  return(package(prog, R_FLOAT, next_reader_f, descr_next_reader_f, args, 1));
}

static char *descr_previous_reader_f(int *args, int *ints, Float *dbls) {return(descr_reader(args, ints, dbls, "previous-sample"));}
static void previous_reader_f(int *args, int *ints, Float *dbls) {FLOAT_RESULT = protected_previous_sample_to_float(((snd_fd *)(INT_ARG_1)));}
static xen_value *previous_sample_1(ptree *prog, xen_value **args) 
{
  if ((args[1] = fixup_if_pending(prog, args[1], R_READER)) == NULL) return(run_warn("previous-sample bad arg"));
  return(package(prog, R_FLOAT, previous_reader_f, descr_previous_reader_f, args, 1));
}

/* static XEN g_make_sample_reader(XEN samp_n, XEN snd, XEN chn, XEN dir1, XEN pos) */
/* fd = init_sample_read_any(XEN_TO_C_INT_OR_ELSE(samp_n, 0), cp, direction, edpos); */
/* need c-side get_cp */



/* ---------------- vct stuff ---------------- */

static void vct_length_i(int *args, int *ints, Float *dbls) {INT_RESULT = ((vct *)(INT_ARG_1))->length;}
static char *descr_vct_length_i(int *args, int *ints, Float *dbls) 
{
  return(mus_format("d%d(%d) = vct_length(i%d(%p))", args[0], INT_RESULT, args[1], (vct *)(INT_ARG_1)));
}
static xen_value *vct_length_1(ptree *prog, xen_value **args, int num_args)
{
  if (num_args != 1) return(NULL);
  if ((args[1] = fixup_if_pending(prog, args[1], R_VCT)) == NULL) return(run_warn("vct-length bad arg"));
  return(package(prog, R_INT, vct_length_i, descr_vct_length_i, args, 1));
}

static void vct_ref_f(int *args, int *ints, Float *dbls) {FLOAT_RESULT = ((vct *)(INT_ARG_1))->data[INT_ARG_2];}
static char *descr_vct_ref_f(int *args, int *ints, Float *dbls) 
{
  return(mus_format("d%d(%.4f) = vct_ref(i%d(%p), i%d(%d))", args[0], FLOAT_RESULT, args[1], (vct *)(INT_ARG_1), args[2], INT_ARG_2));
}
static xen_value *vct_ref_1(ptree *prog, xen_value **args, int num_args)
{
  if (num_args != 2) return(NULL);
  if ((args[1] = fixup_if_pending(prog, args[1], R_VCT)) == NULL) return(run_warn("vct-ref bad arg"));
  if (args[2]->type == R_INT)
    return(package(prog, R_FLOAT, vct_ref_f, descr_vct_ref_f, args, 2));
  return(run_warn("vct-ref bad index type"));
}

static void vct_set_f(int *args, int *ints, Float *dbls) {FLOAT_RESULT = FLOAT_ARG_3; ((vct *)(INT_ARG_1))->data[INT_ARG_2] = FLOAT_ARG_3;}
static char *descr_vct_set_f(int *args, int *ints, Float *dbls) 
{
  return(mus_format("d%d(%.4f) = vct_set!(i%d(%p), i%d(%d), d%d(%.4f))",
		    args[0], FLOAT_RESULT, args[1], (vct *)(INT_ARG_1), args[2], INT_ARG_2, args[3], FLOAT_ARG_3));
}
static xen_value *vct_set_1(ptree *prog, xen_value **args)
{
  if ((args[1] = fixup_if_pending(prog, args[1], R_VCT)) == NULL) return(run_warn("vct-set! bad arg"));
  if ((args[2]->type == R_INT) && (args[3]->type == R_FLOAT))
    return(package(prog, R_FLOAT, vct_set_f, descr_vct_set_f, args, 3));
  return(run_warn("vct-set! bad index type"));
}

static void make_vct_v(int *args, int *ints, Float *dbls) 
{
  vct *v;
  v = c_make_vct(INT_ARG_1);
  INT_RESULT = (int)v;
  add_obj_to_gcs(PTREE, R_VCT, args[0]);
}
static char *descr_make_vct_v(int *args, int *ints, Float *dbls) 
{
  return(mus_format("i%d(%p) = make_vct(i%d(%d))", args[0], (vct *)INT_RESULT, args[1], INT_ARG_1));
}
static void make_vct_v2(int *args, int *ints, Float *dbls) 
{
  vct *v;
  int i;
  v = c_make_vct(INT_ARG_1);
  /* TODO: cleanup previous? */
  INT_RESULT = (int)v;
  for (i = 0; i < v->length; i++) v->data[i] = FLOAT_ARG_2;
  add_obj_to_gcs(PTREE, R_VCT, args[0]);
}
static char *descr_make_vct_v2(int *args, int *ints, Float *dbls) 
{
  return(mus_format("i%d(%p) = make_vct(i%d(%d), d%d(%.4f))", args[0], (vct *)INT_RESULT, args[1], INT_ARG_1, args[2], FLOAT_ARG_2));
}
static xen_value *make_vct_1(ptree *prog, xen_value **args, int num_args)
{
  args[0] = make_xen_value(R_VCT, add_int_to_ptree(prog, 0), R_VARIABLE);
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
  INT_RESULT = (int)v;
  add_obj_to_gcs(PTREE, R_VCT, args[0]);
}
static char *descr_vct_copy_v(int *args, int *ints, Float *dbls) 
{
  return(mus_format("i%d(%p) = vct_copy(i%d(%p))", args[0], (vct *)INT_RESULT, args[1], (vct *)(INT_ARG_1)));
}
static xen_value *vct_copy_1(ptree *prog, xen_value **args, int num_args)
{
  if (num_args == 1)
    {
      args[0] = make_xen_value(R_VCT, add_int_to_ptree(prog, 0), R_VARIABLE);
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
  return(mus_format("vct_" #SName "(i%d(%p), d%d(%.4f))", args[1], (vct *)(INT_ARG_1), args[2], FLOAT_ARG_2)); \
} \
static xen_value *vct_ ## CName ## _1(ptree *prog, xen_value **args, int num_args) \
{ \
  xen_value *temp; \
  if (num_args != 2) return(NULL); \
  if ((args[1] = fixup_if_pending(prog, args[1], R_VCT)) == NULL) return(run_warn("vct-" #SName " bad arg")); \
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
  return(mus_format("vct_" #SName "(i%d(%p), i%d(%p))", args[1], (vct *)(INT_ARG_1), args[2], (vct *)(INT_ARG_2))); \
} \
static xen_value *vct_ ## CName ## _1(ptree *prog, xen_value **args, int num_args) \
{ \
  if (num_args != 2) return(NULL); \
  if ((args[1] = fixup_if_pending(prog, args[1], R_VCT)) == NULL) return(run_warn("vct-" #SName " bad arg")); \
  if ((args[2] = fixup_if_pending(prog, args[2], R_VCT)) == NULL) return(run_warn("vct-" #SName " bad arg")); \
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
  sprintf(buf,"d%d(%.4f) = %s(i%d(%p)", args[0], FLOAT_RESULT, which, args[1], (mus_any *)(INT_ARG_1));
  for (i = 1; i <= num_args; i++)
    {
      snprintf(str, 32, ", d%d(%.4f)", args[i + 1], dbls[args[i + 1]]);
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
    if ((args[1] = fixup_if_pending(prog, args[1], R_CLM)) == NULL) return(run_warn(#Name ": bad gen")); \
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
    if ((args[1] = fixup_if_pending(prog, args[1], R_CLM)) == NULL) return(run_warn(#Name ": bad gen")); \
    if ((num_args > 1) && (args[2]->type == R_INT)) single_to_float(prog, args, 2); \
    if ((num_args > 2) && (args[3]->type == R_INT)) single_to_float(prog, args, 3); \
    if (num_args == 1) return(package(prog, R_FLOAT, Name ## _0f, descr_ ## Name ## _0f, args, 1)); \
    if (num_args == 2) return(package(prog, R_FLOAT, Name ## _1f, descr_ ## Name ## _1f, args, 2)); \
    if (num_args == 3) return(package(prog, R_FLOAT, Name ## _2f, descr_ ## Name ## _2f, args, 3)); \
    return(run_warn(#Name ": wrong number of args")); \
  }
#define GEN2(Name) \
  GEN2_0(Name) \
  GEN2_1(Name) \
  GEN_P(Name) \
  static xen_value * Name ## _1(ptree *prog, xen_value **args, int num_args) \
  { \
    if ((args[1] = fixup_if_pending(prog, args[1], R_CLM)) == NULL) return(run_warn(#Name ": bad gen")); \
    if ((num_args > 1) && (args[2]->type == R_INT)) single_to_float(prog, args, 2); \
    if (num_args == 1) return(package(prog, R_FLOAT, Name ## _0f, descr_ ## Name ## _0f, args, 1)); \
    if (num_args == 2) return(package(prog, R_FLOAT, Name ## _1f, descr_ ## Name ## _1f, args, 2)); \
    return(run_warn(#Name ": wrong number of args")); \
  }
#define GEN_ONLY_2(Name) \
  GEN2_1(Name) \
  static xen_value * Name ## _1(ptree *prog, xen_value **args, int num_args) \
  { \
    if ((args[1] = fixup_if_pending(prog, args[1], R_CLM)) == NULL) return(run_warn(#Name ": bad gen")); \
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
    if ((args[1] = fixup_if_pending(prog, args[1], R_CLM)) == NULL) return(run_warn(#Name ": bad gen")); \
    if (num_args == 1) return(package(prog, R_FLOAT, Name ## _0f, descr_ ## Name ## _0f, args, 1)); \
    return(run_warn(#Name ": wrong number of args")); \
  }
#define GEN0(Name) \
  GEN1_0(Name) \
  static xen_value * mus_ ## Name ## _0(ptree *prog, xen_value **args, int num_args) \
  { \
    if ((args[1] = fixup_if_pending(prog, args[1], R_CLM)) == NULL) return(run_warn("mus-" #Name ": bad gen")); \
    if (num_args == 1) return(package(prog, R_FLOAT, Name ## _0f, descr_ ## Name ## _0f, args, 1)); \
    return(run_warn(#Name ": wrong number of args")); \
  }

static char *descr_int_gen0(int *args, int *ints, Float *dbls, char *which)
{
  return(mus_format("i%d(%d) = %s(i%d(%p)", args[0], INT_RESULT, which, args[1], (mus_any *)(INT_ARG_1)));
}
#define INT_GEN0(Name) \
  static char *descr_ ## Name ## _0i(int *args, int *ints, Float *dbls) {return(descr_int_gen0(args, ints, dbls, #Name));} \
  static void Name ## _0i(int *args, int *ints, Float *dbls) {INT_RESULT = mus_ ## Name ((mus_any *)(INT_ARG_1));} \
  static xen_value * mus_ ## Name ## _0(ptree *prog, xen_value **args, int num_args) \
  { \
    if ((args[1] = fixup_if_pending(prog, args[1], R_CLM)) == NULL) return(run_warn("mus-" #Name ": bad gen")); \
    if (num_args == 1) return(package(prog, R_INT, Name ## _0i, descr_ ## Name ## _0i, args, 1)); \
    return(run_warn(#Name ": wrong number of args")); \
  }

GEN3(oscil)
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

INT_GEN0(hop)
INT_GEN0(channels)
INT_GEN0(location)
INT_GEN0(ramp)
INT_GEN0(position)
INT_GEN0(order)
INT_GEN0(type)
INT_GEN0(length)
INT_GEN0(cosines)

GEN_P(buffer)
GEN_P(buffer_empty)
GEN_P(buffer_full)
GEN_P(output)
GEN_P(input)
GEN_P(frame)
GEN_P(mixer)
GEN_P(file2sample)
GEN_P(sample2file)
GEN_P(file2frame)
GEN_P(frame2file)
GEN_P(locsig)


static char *descr_str_gen0(int *args, int *ints, Float *dbls, char *which)
{
  return(mus_format("i%d(\"%s\") = %s(i%d(%p))", args[0], STRING_RESULT, which, args[1], (mus_any *)(INT_ARG_1)));
}
#define STR_GEN0(Name) \
  static char *descr_ ## Name ## _0s(int *args, int *ints, Float *dbls) {return(descr_str_gen0(args, ints, dbls, #Name));} \
  static void Name ## _0s(int *args, int *ints, Float *dbls) {STRING_RESULT = mus_ ## Name ((mus_any *)(INT_ARG_1));} \
  static xen_value * mus_ ## Name ## _0(ptree *prog, xen_value **args, int num_args) \
  { \
    if ((args[1] = fixup_if_pending(prog, args[1], R_CLM)) == NULL) return(run_warn("mus-" #Name ": bad gen")); \
    if (num_args == 1) return(package(prog, R_STRING, Name ## _0s, descr_ ## Name ## _0s, args, 1)); \
    return(run_warn(#Name ": wrong number of args")); \
  }

STR_GEN0(name)
STR_GEN0(describe)
STR_GEN0(inspect)


static char *descr_ref_gen0(int *args, int *ints, Float *dbls, char *which)
{
  return(mus_format("d%d(%.4f) = %s(i%d(%p), i%d(%d))", args[0], FLOAT_RESULT, which, args[1], (mus_any *)(INT_ARG_1), args[2], INT_ARG_2));
}
#define REF_GEN0(Name, Type) \
  static char *descr_ ## Name ## _0r(int *args, int *ints, Float *dbls) {return(descr_ref_gen0(args, ints, dbls, #Name));} \
  static void Name ## _0r(int *args, int *ints, Float *dbls) {FLOAT_RESULT = mus_ ## Name ((Type *)(INT_ARG_1), INT_ARG_2);} \
  static xen_value * Name ## _0(ptree *prog, xen_value **args, int num_args) \
  { \
    if (num_args != 2) return(run_warn(#Name ": wrong number of args")); \
    if (args[2]->type != R_INT) return(run_warn(#Name ":bad arg 2")); \
    if ((args[1] = fixup_if_pending(prog, args[1], R_CLM)) == NULL) return(run_warn("mus-" #Name ": bad gen")); \
    return(package(prog, R_FLOAT, Name ## _0r, descr_ ## Name ## _0r, args, 2)); \
  }

REF_GEN0(frame_ref, mus_frame)
REF_GEN0(locsig_ref, mus_any)
REF_GEN0(locsig_reverb_ref, mus_any)


static char *descr_vct_gen0(int *args, int *ints, Float *dbls, char *which)
{
  return(mus_format("i%d(%p) = %s(i%d(%p))", args[0], (vct *)(INT_RESULT), which, args[1], (mus_any *)(INT_ARG_1)));
}
#define VCT_GEN0(Name) \
  static char *descr_ ## Name ## _0v(int *args, int *ints, Float *dbls) {return(descr_vct_gen0(args, ints, dbls, #Name));} \
  static void Name ## _0v(int *args, int *ints, Float *dbls) {INT_RESULT = (int)mus_ ## Name ((mus_any *)(INT_ARG_1));} \
  static xen_value * mus_ ## Name ## _0(ptree *prog, xen_value **args, int num_args) \
  { \
    if ((args[1] = fixup_if_pending(prog, args[1], R_CLM)) == NULL) return(run_warn("mus-" #Name ": bad gen")); \
    if (num_args == 1) return(package(prog, R_VCT, Name ## _0v, descr_ ## Name ## _0v, args, 1)); \
    return(run_warn(#Name ": wrong number of args")); \
  }

VCT_GEN0(data)
VCT_GEN0(xcoeffs)
VCT_GEN0(ycoeffs)

static char *descr_set_int_gen0(int *args, int *ints, Float *dbls, char *which)
{
  return(mus_format("mus-%s(i%d(%p)) = i%d(%d)", which, args[0], (mus_any *)(INT_RESULT), args[1], INT_ARG_1));
}
#define SET_INT_GEN0(Name) \
  static char *descr_set_ ## Name ## _i(int *args, int *ints, Float *dbls) {return(descr_set_int_gen0(args, ints, dbls, #Name));} \
  static void set_ ## Name ## _i(int *args, int *ints, Float *dbls) {mus_set_ ## Name ((mus_any *)(INT_RESULT), INT_ARG_1);} \
  static xen_value * mus_set_ ## Name ## _1(ptree *prog, xen_value *in_v, xen_value *v) \
  { \
    if ((in_v = fixup_if_pending(prog, in_v, R_CLM)) == NULL) return(run_warn("set! mus-" #Name ": bad gen")); \
    add_triple_to_ptree(prog, va_make_triple(set_ ## Name ## _i, descr_set_ ## Name ## _i, 2, in_v, v)); \
    return(v); \
  }

SET_INT_GEN0(location)
SET_INT_GEN0(ramp)
SET_INT_GEN0(hop)
SET_INT_GEN0(length)

static char *descr_set_dbl_gen0(int *args, int *ints, Float *dbls, char *which)
{
  return(mus_format("mus-%s(i%d(%p)) = d%d(%.4f)", which, args[0], (mus_any *)(INT_RESULT), args[1], FLOAT_ARG_1));
}
#define SET_DBL_GEN0(Name) \
  static char *descr_set_ ## Name ## _f(int *args, int *ints, Float *dbls) {return(descr_set_dbl_gen0(args, ints, dbls, #Name));} \
  static void set_ ## Name ## _f(int *args, int *ints, Float *dbls) {mus_set_ ## Name ((mus_any *)(INT_RESULT), FLOAT_ARG_1);} \
  static xen_value * mus_set_ ## Name ## _1(ptree *prog, xen_value *in_v, xen_value *v) \
  { \
    if ((in_v = fixup_if_pending(prog, in_v, R_CLM)) == NULL) return(run_warn("set! mus-" #Name ": bad gen")); \
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



/* ---------------- polynomial ---------------- */
static char *descr_polynomial_1f(int *args, int *ints, Float *dbls) 
{
  return(mus_format("d%d(%.4f) = polynomial(i%d(%p), d%d(%.4f))", args[0], FLOAT_RESULT, args[1], (vct *)(INT_ARG_1), args[2], FLOAT_ARG_2));
}
static void polynomial_1f(int *args, int *ints, Float *dbls) 
{
  FLOAT_RESULT = mus_polynomial(((vct *)(INT_ARG_1))->data, FLOAT_ARG_2, ((vct *)(INT_ARG_1))->length);
}
static xen_value *polynomial_1(ptree *prog, xen_value **args, int num_args) 
{
  if ((args[1] = fixup_if_pending(prog, args[1], R_VCT)) == NULL) return(run_warn("polynomial: bad arg"));
  if (num_args == 2) return(package(prog, R_FLOAT, polynomial_1f, descr_polynomial_1f, args, 2));
  return(run_warn("polynomial: wrong number of args"));
}

/* ---------------- file->sample ---------------- */
static char *descr_file2sample_1f(int *args, int *ints, Float *dbls) 
{
  return(mus_format("d%d(%.4f) = file->sample(i%d(%p), i%d(%d))", args[0], FLOAT_RESULT, args[1], (mus_any *)(INT_ARG_1), args[2], INT_ARG_2));
}
static char *descr_file2sample_2f(int *args, int *ints, Float *dbls) 
{
  return(mus_format("d%d(%.4f) = file->sample(i%d(%p), i%d(%d), i%d(%d))", 
		    args[0], FLOAT_RESULT, args[1], (mus_any *)(INT_ARG_1), args[2], INT_ARG_2, args[3], INT_ARG_3));
}
static void file2sample_1f(int *args, int *ints, Float *dbls) {FLOAT_RESULT = mus_file2sample((mus_any *)(INT_ARG_1), INT_ARG_2, 0);}
static void file2sample_2f(int *args, int *ints, Float *dbls) {FLOAT_RESULT = mus_file2sample((mus_any *)(INT_ARG_1), INT_ARG_2, INT_ARG_3);}
static xen_value *file2sample_1(ptree *prog, xen_value **args, int num_args) 
{
  if ((args[1] = fixup_if_pending(prog, args[1], R_CLM)) == NULL) return(run_warn("file->sample: bad gen arg"));
  if (num_args == 2) return(package(prog, R_FLOAT, file2sample_1f, descr_file2sample_1f, args, 2));
  if (num_args == 3) return(package(prog, R_FLOAT, file2sample_2f, descr_file2sample_2f, args, 2));
  return(run_warn("file->sample: wrong number of args"));
}

/* ---------------- array-interp ---------------- */
static char *descr_array_interp_1f(int *args, int *ints, Float *dbls) 
{
  return(mus_format("d%d(%.4f) = array-interp(i%d(%p), d%d(%.4f))", args[0], FLOAT_RESULT, args[1], (vct *)(INT_ARG_1), args[2], FLOAT_ARG_2));
}
static void array_interp_1f(int *args, int *ints, Float *dbls) 
{
  FLOAT_RESULT = mus_array_interp(((vct *)(INT_ARG_1))->data, FLOAT_ARG_2, ((vct *)(INT_ARG_1))->length);
}
static xen_value *array_interp_1(ptree *prog, xen_value **args, int num_args) 
{
  if ((args[1] = fixup_if_pending(prog, args[1], R_VCT)) == NULL) return(run_warn("array-interp: bad arg"));
  if (num_args == 2) return(package(prog, R_FLOAT, array_interp_1f, descr_array_interp_1f, args, 2));
  return(run_warn("array-interp: wrong number of args"));
}

/* ---------------- vct-peak ---------------- */
static char *descr_vct_peak_v(int *args, int *ints, Float *dbls) 
{
  return(mus_format("d%d(%.4f) = vct-peak(i%d(%p))", args[0], FLOAT_RESULT, args[1], (vct *)(INT_ARG_1)));
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
  if ((args[1] = fixup_if_pending(prog, args[1], R_VCT)) == NULL) return(run_warn("vct-peak: bad arg"));
  return(package(prog, R_VCT, vct_peak_v, descr_vct_peak_v, args, 1));
}


/* ---------------- clear_array ---------------- */
static char *descr_clear_array_1f(int *args, int *ints, Float *dbls) {return(mus_format("clear_array(i%d(%p))", args[1], (vct *)(INT_ARG_1)));}
static void clear_array_1f(int *args, int *ints, Float *dbls) 
{
  mus_clear_array(((vct *)(INT_ARG_1))->data, ((vct *)(INT_ARG_1))->length);
  INT_RESULT = INT_ARG_1;
}
static xen_value *clear_array_1(ptree *prog, xen_value **args, int num_args) 
{
  if ((args[1] = fixup_if_pending(prog, args[1], R_VCT)) == NULL) return(run_warn("clear-array: bad arg"));
  return(package(prog, R_VCT, clear_array_1f, descr_clear_array_1f, args, 1));
}


/* ---------------- dot-product etc ---------------- */

#define VCT_2_I(CName, SName) \
static char *descr_ ## CName ## _2f(int *args, int *ints, Float *dbls)  \
{ \
  return(mus_format(#SName "(i%d(%p), i%d(%p))", args[1], (vct *)(INT_ARG_1), args[2], (vct *)(INT_ARG_2))); \
} \
static void CName ## _2f(int *args, int *ints, Float *dbls)  \
{ \
  mus_ ## CName(((vct *)(INT_ARG_1))->data, ((vct *)(INT_ARG_2))->data, ((vct *)(INT_ARG_1))->length); \
  INT_RESULT = INT_ARG_1; \
} \
static char *descr_ ## CName ## _3f(int *args, int *ints, Float *dbls)  \
{ \
  return(mus_format(#SName "(i%d(%p), i%d(%p), i%d(%d))",  \
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
  if ((args[1] = fixup_if_pending(prog, args[1], R_VCT)) == NULL) return(run_warn(#SName ": bad arg 1")); \
  if ((args[2] = fixup_if_pending(prog, args[2], R_VCT)) == NULL) return(run_warn(#SName ": bad arg 2")); \
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


static void clm_0f(int *args, int *ints, Float *dbls) {FLOAT_RESULT = MUS_RUN(((mus_any *)(INT_ARG_1)), 0.0, 0.0);}
static char *descr_clm_0f(int *args, int *ints, Float *dbls) {return(descr_gen(args, ints, dbls, mus_name(((mus_any *)(INT_ARG_1))), 0));} 
static void clm_1f(int *args, int *ints, Float *dbls) {FLOAT_RESULT = MUS_RUN(((mus_any *)(INT_ARG_1)), FLOAT_ARG_2, 0.0);}
static char *descr_clm_1f(int *args, int *ints, Float *dbls) {return(descr_gen(args, ints, dbls, mus_name(((mus_any *)(INT_ARG_1))), 1));} 
static void clm_2f(int *args, int *ints, Float *dbls) {FLOAT_RESULT = MUS_RUN(((mus_any *)(INT_ARG_1)), FLOAT_ARG_2, FLOAT_ARG_3);}
static char *descr_clm_2f(int *args, int *ints, Float *dbls) {return(descr_gen(args, ints, dbls, mus_name(((mus_any *)(INT_ARG_1))), 2));} 
static xen_value *clm_n(ptree *prog, xen_value **args, int num_args, xen_value *sf)
{
  if ((sf = fixup_if_pending(prog, sf, R_CLM)) == NULL) return(run_warn("clm gen bad arg"));
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

/* ---------------- src ---------------- */

static Float src_input(void *arg, int direction)
{
  mus_xen *gn = (mus_xen *)arg;
  ptree *pt, *outer;
  pt = (ptree *)(gn->input_ptree);
  outer = pt->outer_tree;
  outer->ints[pt->args[0]] = direction;
  eval_embedded_ptree(pt, outer->ints, outer->dbls);
  /* fprintf(stderr,"src called with %d -> %f\n", direction, pt->dbls[pt->result->addr]); */
  return(outer->dbls[pt->result->addr]);
}
static char *descr_src_2f(int *args, int *ints, Float *dbls) 
{
  return(mus_format("d%d(%.4f) = src(i%d(%p), d%d(%.4f), i%d(%p))",
		    args[0], FLOAT_RESULT, args[1], (mus_any *)(INT_ARG_1), args[2], FLOAT_ARG_2, args[3], (ptree *)(INT_ARG_3)));
}
static void src_2f(int *args, int *ints, Float *dbls) 
{
  ((mus_xen *)mus_environ((mus_any *)(INT_ARG_1)))->input_ptree = (void *)(INT_ARG_3); \
  FLOAT_RESULT = mus_src(((mus_any *)(INT_ARG_1)), FLOAT_ARG_2, src_input);
}
static char *descr_src_1f(int *args, int *ints, Float *dbls) 
{
  return(mus_format("d%d(%.4f) = src(i%d(%p), d%d(%.4f))", args[0], FLOAT_RESULT, args[1], (mus_any *)(INT_ARG_1), args[2], FLOAT_ARG_2));
}
static void src_1f(int *args, int *ints, Float *dbls) {FLOAT_RESULT = mus_src(((mus_any *)(INT_ARG_1)), FLOAT_ARG_2, NULL);}
static char *descr_src_0f(int *args, int *ints, Float *dbls) 
{
  return(mus_format("d%d(%.4f) = src(i%d(%p))", args[0], FLOAT_RESULT, args[1], (mus_any *)(INT_ARG_1)));
}
static void src_0f(int *args, int *ints, Float *dbls) {FLOAT_RESULT = mus_src(((mus_any *)(INT_ARG_1)), 0.0, NULL);}
static xen_value *src_1(ptree *prog, xen_value **args, int num_args) 
{
  if ((args[1] = fixup_if_pending(prog, args[1], R_CLM)) == NULL) return(run_warn("src: bad gen"));
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
  return(mus_format("d%d(%.4f) = " #Name "(i%d(%p), i%d(%p))", args[0], FLOAT_RESULT, args[1], (mus_any *)(INT_ARG_1), args[2], (ptree *)(INT_ARG_2))); \
} \
static void Name ## _1f(int *args, int *ints, Float *dbls)  \
{ \
  ((mus_xen *)mus_environ((mus_any *)(INT_ARG_1)))->input_ptree = (void *)(INT_ARG_2); \
  FLOAT_RESULT = mus_ ## Name(((mus_any *)(INT_ARG_1)), src_input); \
} \
static char *descr_ ## Name ## _0f(int *args, int *ints, Float *dbls)  \
{ \
  return(mus_format("d%d(%.4f) = " #Name "(i%d(%p))", args[0], FLOAT_RESULT, args[1], (mus_any *)(INT_ARG_1))); \
} \
static void Name ## _0f(int *args, int *ints, Float *dbls) {FLOAT_RESULT = mus_ ## Name(((mus_any *)(INT_ARG_1)), NULL);} \
static xen_value * Name ## _1(ptree *prog, xen_value **args, int num_args)  \
{ \
  if ((args[1] = fixup_if_pending(prog, args[1], R_CLM)) == NULL) return(run_warn(#Name ": bad gen")); \
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
  return(mus_format("d%d(%.4f) = contrast-enhancement(d%d(%.4f), d%d(%.4f))", args[0], FLOAT_RESULT, args[1], FLOAT_ARG_1, args[2], FLOAT_ARG_2));
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
  return(mus_format("d%d(%.4f) = ring-modulate(d%d(%.4f), d%d(%.4f))", args[0], FLOAT_RESULT, args[1], FLOAT_ARG_1, args[2], FLOAT_ARG_2));
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
  return(mus_format("d%d(%.4f) = amplitude-modulate(d%d(%.4f), d%d(%.4f), d%d(%.4f))", 
		    args[0], FLOAT_RESULT, args[1], FLOAT_ARG_1, args[2], FLOAT_ARG_2, args[3], FLOAT_ARG_3));
}
static xen_value *amplitude_modulate_1(ptree *prog, xen_value **args, int num_args)
{
  if (num_args != 3) return(run_warn("wrong number of args for amplitude-modulate"));
  if (args[1]->type == R_INT) single_to_float(prog, args, 1);
  if (args[2]->type == R_INT) single_to_float(prog, args, 2);
  if (args[3]->type == R_INT) single_to_float(prog, args, 3);
  return(package(prog, R_FLOAT, amplitude_modulate_f, descr_amplitude_modulate_f, args, 2));
}


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
      break; /* TODO cleanup previous? */
    }
}
static char *descr_funcall_nf(int *args, int *ints, Float *dbls) 
{
  return(mus_format("%d = funcall(i%d(%p) ...)", args[0], args[1], (ptree *)(INT_ARG_1)));
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

#define SND_STR_OP(CName) \
static void CName ## _f(int *args, int *ints, Float *dbls) {INT_RESULT = CName(STRING_ARG_1);} \
static char *descr_ ## CName ## _f(int *args, int *ints, Float *dbls)  \
{ \
  return(mus_format("i%d(%d) = " #CName "(i%d(\"%s\"))", args[0], INT_RESULT, args[1], STRING_ARG_1)); \
} \
static xen_value * CName ## _1(ptree *prog, xen_value **args) \
{ \
  if (args[1]->type == R_STRING) return(package(prog, R_INT, CName ## _f, descr_ ##CName ## _f, args, 1)); \
  return(NULL); \
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
  return(mus_format("d%d(%.4f) = sound-duration(i%d(\"%s\"))", args[0], FLOAT_RESULT, args[1], STRING_ARG_1));
}
static xen_value *mus_sound_duration_1(ptree *prog, xen_value **args)
{
  if (args[1]->type == R_STRING) return(package(prog, R_FLOAT, mus_sound_duration_f, descr_mus_sound_duration_f, args, 1));
  return(NULL);
}

static void mus_sound_comment_f(int *args, int *ints, Float *dbls) 
{
  if (STRING_RESULT) FREE(STRING_RESULT);
  STRING_RESULT = mus_sound_comment(STRING_ARG_1);
}
static char *descr_mus_sound_comment_f(int *args, int *ints, Float *dbls)
{
  return(mus_format("i%d(\"%s\") = sound-comment(i%d(\"%s\"))", args[0], STRING_RESULT, args[1], STRING_ARG_1));
}
static xen_value *mus_sound_comment_1(ptree *prog, xen_value **args)
{
  if (args[1]->type == R_STRING) return(package(prog, R_STRING, mus_sound_comment_f, descr_mus_sound_comment_f, args, 1));
  return(NULL);
}

static void mus_header_type_name_f(int *args, int *ints, Float *dbls) 
{
  if (STRING_RESULT) FREE(STRING_RESULT);
  STRING_RESULT = mus_header_type_name(INT_ARG_1);
}
static char *descr_mus_header_type_name_f(int *args, int *ints, Float *dbls)
{
  return(mus_format("i%d(\"%s\") = header-type-name(i%d(%d))", args[0], STRING_RESULT, args[1], INT_ARG_1));
}
static xen_value *mus_header_type_name_1(ptree *prog, xen_value **args)
{
  if (args[1]->type == R_INT) return(package(prog, R_STRING, mus_header_type_name_f, descr_mus_header_type_name_f, args, 1));
  return(NULL);
}

static void mus_data_format_name_f(int *args, int *ints, Float *dbls) 
{
  if (STRING_RESULT) FREE(STRING_RESULT);
  STRING_RESULT = mus_data_format_name(INT_ARG_1);
}
static char *descr_mus_data_format_name_f(int *args, int *ints, Float *dbls)
{
  return(mus_format("i%d(\"%s\") = data-format-name(i%d(%d))", args[0], STRING_RESULT, args[1], INT_ARG_1));
}
static xen_value *mus_data_format_name_1(ptree *prog, xen_value **args)
{
  if (args[1]->type == R_INT) return(package(prog, R_STRING, mus_data_format_name_f, descr_mus_data_format_name_f, args, 1));
  return(NULL);
}




static xen_value *goto_0(ptree *prog, xen_value **args, xen_value *sf)
{
  if (args[0]) FREE(args[0]);
  args[0] = make_xen_value(R_PENDING, add_int_to_ptree(prog, sf->addr), R_VARIABLE);
  add_triple_to_ptree(prog, va_make_triple(jump_indirect, descr_jump_indirect, 1, args[0]));
  return(args[0]);
}


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

static xen_value *walk(ptree *prog, XEN form, int need_result)
{
  /* walk form, storing vars, making program entries for operators etc */
  /* fprintf(stderr,"walk %s\n", XEN_AS_STRING(form)); */
  /* need_result = TRUE; */
  if (current_optimization == 0) return(NULL);

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
  if (XEN_LIST_P(form))
    {
      XEN function, all_args;
      char funcname[32];
      xen_value **args = NULL;
      int i, num_args, float_result = FALSE, constants = 0, booleans = 0, vcts = 0;
      int readers = 0, pendings = 0, clms = 0, chars = 0, strings = 0;
      xen_var *var;
      xen_value *v = NULL;
      function = XEN_CAR(form);
      snprintf(funcname, 32, "%s", XEN_AS_STRING(function)); /* protect from gc... */

      if (strcmp(funcname, "lambda") == 0) return(lambda_form(prog, form));
      if (strcmp(funcname, "declare") == 0) return(make_xen_value(R_BOOL, -1, TRUE));
      if (strcmp(funcname, "begin") == 0) return(begin_form(prog, form, need_result));
      if (strcmp(funcname, "if") == 0) return(if_form(prog, form, need_result));
      if (strcmp(funcname, "do") == 0) return(do_form(prog, form, need_result));
      if (strcmp(funcname, "do*") == 0) return(do_star_form(prog, form, need_result));
      if (strcmp(funcname, "cond") == 0) return(cond_form(prog, form, need_result));
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
	  for (i = 0; i < num_args; i++, all_args = XEN_CDR(all_args))
	    {
	      args[i + 1] = walk(prog, XEN_CAR(all_args), TRUE);
	      if (args[i + 1] == NULL) return(clean_up(NULL, args, num_args));
	      if (args[i + 1]->constant == R_CONSTANT) constants++;
	      switch (args[i + 1]->type)
		{
		case R_FLOAT:   float_result = TRUE; break;
		case R_BOOL:    booleans++;          break;
		case R_VCT:     vcts++;              break;
		case R_READER:  readers++;           break;
		case R_CLM:     clms++;              break;
		case R_PENDING: pendings++;          break;
		case R_CHAR:    chars++;             break;
		case R_STRING:  strings++;           break;
		}
	    }
	}

      /* check user-defined stuff */
      var = find_var_in_ptree(prog, funcname);
      if (var == NULL)
	{
	  XEN val;
	  val = XEN_SYMBOL_TO_VALUE(function);
	  if ((sf_p(val)) || (mus_xen_p(val)))
	    v = add_global_var_to_ptree(prog, function);
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
		  else c->result = add_empty_var_to_ptree(prog, R_PENDING);
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
      if ((num_args == 1) && (pendings == 0))
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
	  
	  if ((strcmp(funcname, "vct?") == 0) || (strcmp(funcname, "vector?") == 0))
	    return(clean_up(make_xen_value(R_BOOL, add_int_to_ptree(prog, args[1]->type == R_VCT), R_CONSTANT), args, num_args));
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
	      if (strcmp(funcname, "char?") == 0) 
		return(clean_up(make_xen_value(R_BOOL, add_int_to_ptree(prog, TRUE), R_CONSTANT), args, num_args));
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
      if (strcmp(funcname, "array-interp") == 0) return(clean_up(array_interp_1(prog, args, num_args), args, num_args));
      if (strcmp(funcname, "clear-array") == 0) return(clean_up(clear_array_1(prog, args, num_args), args, num_args));
      if (strcmp(funcname, "contrast-enhancement") == 0) return(clean_up(contrast_enhancement_1(prog, args, num_args), args, num_args));
      if (strcmp(funcname, "ring-modulate") == 0) return(clean_up(ring_modulate_1(prog, args, num_args), args, num_args));
      if (strcmp(funcname, "amplitude-modulate") == 0) return(clean_up(amplitude_modulate_1(prog, args, num_args), args, num_args));
      if (strcmp(funcname, "convolution") == 0) return(clean_up(convolution_1(prog, args, num_args), args, num_args));
      if (strcmp(funcname, "multiply-arrays") == 0) return(clean_up(multiply_arrays_1(prog, args, num_args), args, num_args));
      if (strcmp(funcname, "rectangular2polar") == 0) return(clean_up(rectangular2polar_1(prog, args, num_args), args, num_args));
      if (strcmp(funcname, "polar2rectangular") == 0) return(clean_up(polar2rectangular_1(prog, args, num_args), args, num_args));

      if ((clms == 1) || (pendings == 1) || (booleans == 1))
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

	  if (strcmp(funcname, "src") == 0) return(clean_up(src_1(prog, args, num_args), args, num_args));
	  if (strcmp(funcname, "granulate") == 0) return(clean_up(granulate_1(prog, args, num_args), args, num_args));
	  if (strcmp(funcname, "phase-vocoder") == 0) return(clean_up(phase_vocoder_1(prog, args, num_args), args, num_args));
	  if (strcmp(funcname, "convolve") == 0) return(clean_up(convolve_1(prog, args, num_args), args, num_args));

	  if (strcmp(funcname, "tap") == 0) return(clean_up(tap_1(prog, args, num_args), args, num_args));
	  if (strcmp(funcname, "file->sample") == 0) return(clean_up(file2sample_1(prog, args, num_args), args, num_args));
	  if (strcmp(funcname, "sample->buffer") == 0) return(clean_up(sample2buffer_1(prog, args, num_args), args, num_args));
	  if (strcmp(funcname, "buffer->sample") == 0) return(clean_up(mus_buffer2sample_0(prog, args, num_args), args, num_args));

	  if (num_args == 1)
	    {
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
	      if (strcmp(funcname, "output?") == 0) return(clean_up(output_p(prog, args), args, num_args));
	      if (strcmp(funcname, "input?") == 0) return(clean_up(input_p(prog, args), args, num_args));
	      if (strcmp(funcname, "frame?") == 0) return(clean_up(frame_p(prog, args), args, num_args));
	      if (strcmp(funcname, "mixer?") == 0) return(clean_up(mixer_p(prog, args), args, num_args));
	      if (strcmp(funcname, "file->sample?") == 0) return(clean_up(file2sample_p(prog, args), args, num_args));
	      if (strcmp(funcname, "sample->file?") == 0) return(clean_up(sample2file_p(prog, args), args, num_args));
	      if (strcmp(funcname, "file->frame?") == 0) return(clean_up(file2frame_p(prog, args), args, num_args));
	      if (strcmp(funcname, "frame->file?") == 0) return(clean_up(frame2file_p(prog, args), args, num_args));
	      if (strcmp(funcname, "locsig?") == 0) return(clean_up(locsig_p(prog, args), args, num_args));
	    }

	  if (strcmp(funcname, "locsig-ref") == 0) return(clean_up(locsig_ref_0(prog, args, num_args), args, num_args));
	  if (strcmp(funcname, "frame-ref") == 0) return(clean_up(frame_ref_0(prog, args, num_args), args, num_args));
	  if (strcmp(funcname, "locsig-reverb-ref") == 0) return(clean_up(locsig_reverb_ref_0(prog, args, num_args), args, num_args));
	  if (strcmp(funcname, "mus-increment") == 0) return(clean_up(mus_increment_0(prog, args, num_args), args, num_args));
	  if (strcmp(funcname, "mus-frequency") == 0) return(clean_up(mus_frequency_0(prog, args, num_args), args, num_args));
	  if (strcmp(funcname, "mus-phase") == 0) return(clean_up(mus_phase_0(prog, args, num_args), args, num_args));
	  if (strcmp(funcname, "mus-scaler") == 0) return(clean_up(mus_scaler_0(prog, args, num_args), args, num_args));
	  if (strcmp(funcname, "mus-formant_radius") == 0) return(clean_up(mus_formant_radius_0(prog, args, num_args), args, num_args));
	  if (strcmp(funcname, "mus-a0") == 0) return(clean_up(mus_a0_0(prog, args, num_args), args, num_args));
	  if (strcmp(funcname, "mus-a1") == 0) return(clean_up(mus_a1_0(prog, args, num_args), args, num_args));
	  if (strcmp(funcname, "mus-a2") == 0) return(clean_up(mus_a2_0(prog, args, num_args), args, num_args));
	  if (strcmp(funcname, "mus-b1") == 0) return(clean_up(mus_b1_0(prog, args, num_args), args, num_args));
	  if (strcmp(funcname, "mus-b2") == 0) return(clean_up(mus_b2_0(prog, args, num_args), args, num_args));
	  if (strcmp(funcname, "mus-feedforward") == 0) return(clean_up(mus_feedforward_0(prog, args, num_args), args, num_args));
	  if (strcmp(funcname, "mus-feedback") == 0) return(clean_up(mus_feedback_0(prog, args, num_args), args, num_args));

	  if (strcmp(funcname, "mus-hop") == 0) return(clean_up(mus_hop_0(prog, args, num_args), args, num_args));
	  if (strcmp(funcname, "mus-channels") == 0) return(clean_up(mus_channels_0(prog, args, num_args), args, num_args));
	  if (strcmp(funcname, "mus-location") == 0) return(clean_up(mus_location_0(prog, args, num_args), args, num_args));
	  if (strcmp(funcname, "mus-ramp") == 0) return(clean_up(mus_ramp_0(prog, args, num_args), args, num_args));
	  if (strcmp(funcname, "mus-position") == 0) return(clean_up(mus_position_0(prog, args, num_args), args, num_args));
	  if (strcmp(funcname, "mus-order") == 0) return(clean_up(mus_order_0(prog, args, num_args), args, num_args));
	  if (strcmp(funcname, "mus-type") == 0) return(clean_up(mus_type_0(prog, args, num_args), args, num_args));
	  if (strcmp(funcname, "mus-length") == 0) return(clean_up(mus_length_0(prog, args, num_args), args, num_args));
	  if (strcmp(funcname, "mus-cosines") == 0) return(clean_up(mus_cosines_0(prog, args, num_args), args, num_args));

	  if (strcmp(funcname, "mus-name") == 0) return(clean_up(mus_name_0(prog, args, num_args), args, num_args));
	  if (strcmp(funcname, "mus-describe") == 0) return(clean_up(mus_describe_0(prog, args, num_args), args, num_args));
	  if (strcmp(funcname, "mus-inspect") == 0) return(clean_up(mus_inspect_0(prog, args, num_args), args, num_args));

	  if (strcmp(funcname, "mus-data") == 0) return(clean_up(mus_data_0(prog, args, num_args), args, num_args));
	  if (strcmp(funcname, "mus-xcoeffs") == 0) return(clean_up(mus_xcoeffs_0(prog, args, num_args), args, num_args));
	  if (strcmp(funcname, "mus-ycoeffs") == 0) return(clean_up(mus_ycoeffs_0(prog, args, num_args), args, num_args));
	}
      if (num_args == 3)
	{
	  if ((strcmp(funcname, "vct-set!") == 0) || (strcmp(funcname, "vector-set!") == 0))
	    return(clean_up(vct_set_1(prog, args), args, num_args));
	}
      if ((num_args == 1) && ((readers == 1) || (pendings == 1)))
	{
	  if (strcmp(funcname, "next-sample") == 0) return(clean_up(next_sample_1(prog, args), args, num_args));
	  if (strcmp(funcname, "previous-sample") == 0) return(clean_up(previous_sample_1(prog, args), args, num_args));
	  if (strcmp(funcname, "read-sample") == 0) return(clean_up(reader_1(prog, args), args, num_args));
	}
      if (readers > 0) return(clean_up(run_warn("reader bad arg"), args, num_args));
      if (clms > 0) return(clean_up(run_warn("clm gen bad arg"), args, num_args));
      /* both of these can be applicable objects, but those are not counted in the arg scan */
      /* no readers or CLM gens from here on */

      if ((vcts > 0) || (pendings > 0))
	{
	  if ((strcmp(funcname, "vct-ref") == 0) || (strcmp(funcname, "vector-ref") == 0))
	    return(clean_up(vct_ref_1(prog, args, num_args), args, num_args));
	  if ((strcmp(funcname, "vct-length") == 0) || (strcmp(funcname, "vector-length") == 0))
	    return(clean_up(vct_length_1(prog, args, num_args), args, num_args));
	  if ((strcmp(funcname, "vct-fill!") == 0) || (strcmp(funcname, "vector-fill!") == 0))
	    return(clean_up(vct_fill_1(prog, args, num_args), args, num_args));
	  if (strcmp(funcname, "vct-scale!") == 0) return(clean_up(vct_scale_1(prog, args, num_args), args, num_args));
	  if (strcmp(funcname, "vct-offset!") == 0) return(clean_up(vct_offset_1(prog, args, num_args), args, num_args));
	  if (strcmp(funcname, "vct-add!") == 0) return(clean_up(vct_add_1(prog, args, num_args), args, num_args));
	  if (strcmp(funcname, "vct-subtract!") == 0) return(clean_up(vct_subtract_1(prog, args, num_args), args, num_args));
	  if (strcmp(funcname, "vct-multiply!") == 0) return(clean_up(vct_multiply_1(prog, args, num_args), args, num_args));
	  if (strcmp(funcname, "vct-copy") == 0) return(clean_up(vct_copy_1(prog, args, num_args), args, num_args));
	  if (strcmp(funcname, "vct-peak") == 0) return(clean_up(vct_peak_1(prog, args, num_args), args, num_args));
	}
      /* no vcts from here on (except make-vct) */

      if (need_result)
	{
	  if (num_args == 0)
	    {
	      if (strcmp(funcname, "string") == 0)
		return(clean_up(make_xen_value(R_STRING, add_string_to_ptree(prog, (char *)CALLOC(1, sizeof(char))), R_CONSTANT), args, num_args));
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
	      if (strcmp(funcname, "not") == 0) return(clean_up(not_p(prog, args, constants), args, num_args));
	      if ((booleans == 0) && (pendings == 0))
		{
		  if (strcmp(funcname, "lognot") == 0) return(clean_up(lognot_1(prog, args, constants), args, num_args));
		  if (strcmp(funcname, "abs") == 0) return(clean_up(abs_1(prog, args, constants), args, num_args));
		  if (strcmp(funcname, "sin") == 0) return(clean_up(sin_1(prog, args, constants), args, num_args));
		  if (strcmp(funcname, "cos") == 0) return(clean_up(cos_1(prog, args, constants), args, num_args));
		  if (strcmp(funcname, "tan") == 0) return(clean_up(tan_1(prog, args, constants), args, num_args));
		  if (strcmp(funcname, "random") == 0) return(clean_up(random_1(prog, args), args, num_args));

		  if (strcmp(funcname, "radians->hz") == 0) return(clean_up(mus_radians2hz_1(prog, args, constants), args, num_args));
		  if (strcmp(funcname, "hz->radians") == 0) return(clean_up(mus_hz2radians_1(prog, args, constants), args, num_args));
		  if (strcmp(funcname, "degrees->radians") == 0) return(clean_up(mus_degrees2radians_1(prog, args, constants), args, num_args));
		  if (strcmp(funcname, "radians->degrees") == 0) return(clean_up(mus_radians2degrees_1(prog, args, constants), args, num_args));
		  if (strcmp(funcname, "db->linear") == 0) return(clean_up(mus_db2linear_1(prog, args, constants), args, num_args));
		  if (strcmp(funcname, "linear->db") == 0) return(clean_up(mus_linear2db_1(prog, args, constants), args, num_args));

		  if (current_optimization > 1)
		    {
		      if (strcmp(funcname, "atan") == 0) return(clean_up(atan1_1(prog, args, constants), args, num_args));
		      if (strcmp(funcname, "log") == 0) return(clean_up(log_1(prog, args, constants), args, num_args));
		      if (strcmp(funcname, "exp") == 0) return(clean_up(exp_1(prog, args, constants), args, num_args));
		      if (strcmp(funcname, "asin") == 0) return(clean_up(asin_1(prog, args, constants), args, num_args));
		      if (strcmp(funcname, "acos") == 0) return(clean_up(acos_1(prog, args, constants), args, num_args));
		      if (strcmp(funcname, "sqrt") == 0) return(clean_up(sqrt_1(prog, args, constants), args, num_args));
		    }
		  if (strcmp(funcname, "inexact->exact") == 0) return(clean_up(inexact2exact_1(prog, args, constants), args, num_args));
		  if (strcmp(funcname, "exact->inexact") == 0) return(clean_up(exact2inexact_1(prog, args, constants), args, num_args));
		  if (strcmp(funcname, "round") == 0) return(clean_up(round_1(prog, args, constants), args, num_args));
		  if (strcmp(funcname, "truncate") == 0) return(clean_up(truncate_1(prog, args, constants), args, num_args));
		  if (strcmp(funcname, "floor") == 0) return(clean_up(floor_1(prog, args, constants), args, num_args));
		  if (strcmp(funcname, "ceiling") == 0) return(clean_up(ceiling_1(prog, args, constants), args, num_args));
		  if (strcmp(funcname, "odd?") == 0) return(clean_up(odd_p(prog, args, constants), args, num_args));
		  if (strcmp(funcname, "even?") == 0) return(clean_up(even_p(prog, args, constants), args, num_args));
		  if (strcmp(funcname, "zero?") == 0) return(clean_up(zero_p(prog, args, constants), args, num_args));
		  if (strcmp(funcname, "positive?") == 0) return(clean_up(positive_p(prog, args, constants), args, num_args));
		  if (strcmp(funcname, "negative?") == 0) return(clean_up(negative_p(prog, args, constants), args, num_args));
		}
	      if (strcmp(funcname, "mus-header-type-name") == 0) return(clean_up(mus_header_type_name_1(prog, args), args, num_args));
	      if (strcmp(funcname, "mus-data-format-name") == 0) return(clean_up(mus_data_format_name_1(prog, args), args, num_args));
	    }
	  if ((strcmp(funcname, "make-vct") == 0) || (strcmp(funcname, "make-vector") == 0))
	    return(clean_up(make_vct_1(prog, args, num_args), args, num_args));
	} /* end need_result */
      if (need_result)
	return(clean_up(NULL, args, num_args));
      return(make_xen_value(R_BOOL, -1, R_CONSTANT));
    }
  else
    {
      if (XEN_SYMBOL_P(form))
	{
	  xen_var *var;
	  xen_value *v;
	  v = add_global_var_to_ptree(prog, form);
	  if (v->type == R_PENDING)
	    {
	      if (current_optimization > 2)
		{
		  var = find_var_in_ptree(prog, XEN_SYMBOL_TO_C_STRING(form));
		  var->undefined = TRUE;
		}
	      else
		{
		  FREE(v);
		  if (!run_warned)
		    return(run_warn("can't handle %s", XEN_AS_STRING(form)));
		  return(NULL);
		}
	    }
	  return(v);
	}
    }
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
      if (strcmp(accessor, "mus-b2") == 0) v = mus_set_b2_1(prog, in_v, v);
    }
  if (v->type == R_INT)
    {
      if (strcmp(accessor, "mus-ramp") == 0) v = mus_set_ramp_1(prog, in_v, v); else
      if (strcmp(accessor, "mus-hop") == 0) v = mus_set_hop_1(prog, in_v, v); else
      if (strcmp(accessor, "mus-location") == 0) v = mus_set_location_1(prog, in_v, v); else
      if (strcmp(accessor, "mus-length") == 0) v = mus_set_length_1(prog, in_v, v);
    }
  if (v == NULL) run_warn(mus_format("can't set! %s", accessor));
  FREE(in_v);
  FREE(accessor);
  return(v);
}

static void *form_to_ptree(XEN code)
{
  int i;
  ptree *prog;
  xen_var *var;
  XEN form;
  run_warned = FALSE;
  current_optimization = optimization(get_global_state());
  if (current_optimization == 0) return(NULL);
  form = XEN_CAR(code);
  prog = make_ptree(8);
  if ((XEN_PROCEDURE_P(XEN_CADR(code))) && 
      (SCM_TYP7(XEN_CADR(code)) != scm_tc7_smob)) /* applicable smobs cause confusion here */
    prog->code = XEN_CADR(code);                  /* need env before starting to walk the code */
  else 
    {
      prog->code = XEN_FALSE;                    /* many confusing cases here -- we'll just give up */
    }

  if (XEN_SYMBOL_P(form))                         /* try to use the procedure source if it's available */
    {
      form = XEN_FALSE;
      if (current_optimization > 3)
	{
	  XEN function, source;
	  function = XEN_SYMBOL_TO_VALUE(form);
	  if (XEN_PROCEDURE_P(function))
	    {
	      source = scm_procedure_source(function);
	      /* fprintf(stderr,"use: %s\n", XEN_AS_STRING(source)); */
	      if (XEN_LIST_P(source))
		form = source;
	    }
	}
      if (XEN_FALSE_P(form))
	return(free_ptree((void *)prog));
    }
  prog->result = walk(prog, form, TRUE);
  if (prog->result)
    {
      add_triple_to_ptree(prog, make_triple(quit, descr_quit, NULL, 0));
      /* now check that we were able to nail down all global variable types */
      for (i = 0; i < prog->global_var_ctr; i++)
	{
	  var = prog->global_vars[i];
	  if ((var) &&
	      (var->v->type == R_PENDING))
	    {
	      run_warn("can't decide type for %s", var->name);
	      free_ptree((void *)prog);
	      return(NULL);
	    }
	}
      return((void *)prog);
    }
  if (!run_warned)
    run_warn("can't optimize: %s\n", XEN_AS_STRING(form));
  return(free_ptree((void *)prog));
}

void *form_to_ptree_1f2f(XEN code)
{
  ptree *pt;
  pt = form_to_ptree(code);
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
  pt = form_to_ptree(code);
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
  pt = form_to_ptree(code);
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
  pt = form_to_ptree(XEN_LIST_2(code, XEN_FALSE));
  if (pt)
    {
      if ((pt->result->type == R_BOOL) && (pt->arity == 1))
	return(pt);
      free_ptree(pt);
    }
  return(NULL);
}

Float evaluate_ptree_1f2f(void *upt, Float arg)
{
  ptree *pt = (ptree *)upt;
  if (pt)
    {
      pt->dbls[pt->args[0]] = arg;
      eval_ptree(pt);
      return(pt->dbls[pt->result->addr]);
    }
  return(0.0);
}

Float evaluate_ptree_0f2f(void *upt)
{
  ptree *pt = (ptree *)upt;
  if (pt)
    {
      eval_ptree(pt);
      return(pt->dbls[pt->result->addr]);
    }
  return(0.0);
}

int evaluate_ptree_1f2b(void *upt, Float arg)
{
  ptree *pt = (ptree *)upt;
  if (pt)
    {
      pt->dbls[pt->args[0]] = arg;
      eval_ptree(pt);
      return(pt->ints[pt->result->addr]);
    }
  return(0);
}

/* ---------------- internal testing stuff ---------------- */

static XEN g_run(XEN code)
{
  ptree *prog;
  char *msg;
  current_optimization = 4;
  prog = make_ptree(8);
  prog->result = walk(prog, code, TRUE);
  if (prog->result)
    {
      add_triple_to_ptree(prog, make_triple(quit, descr_quit, NULL, 0));
      msg = describe_ptree(prog);
      fprintf(stderr, msg);
      FREE(msg);
    }
  else fprintf(stderr,"run can't parse that");
  free_ptree(prog);
  return(XEN_FALSE);
}

static XEN g_run_eval(XEN code, XEN arg)
{
  ptree *pt;
  char *err;
  XEN result;
  current_optimization = 4;
  pt = make_ptree(8);
  pt->result = walk(pt, code, TRUE);
  if (pt->result)
    {
      add_triple_to_ptree(pt, make_triple(quit, descr_quit, NULL, 0));
      if ((XEN_BOUND_P(arg)) && (pt->args))
	{
	  switch (pt->arg_types[0])
	    {
	    case R_FLOAT:  pt->dbls[pt->args[0]] = (Float)XEN_TO_C_DOUBLE(arg); break;
	    case R_INT:    pt->ints[pt->args[0]] = (int)XEN_TO_C_INT(arg); break;
	    case R_STRING: pt->ints[pt->args[0]] = (int)copy_string(XEN_TO_C_STRING(arg)); break;
	    }
	}
      err = initialize_ptree(pt);
      if (err)
	{
	  free_ptree((void *)pt);
	  FREE(err);
	  XEN_ERROR(XEN_ERROR_TYPE("cannot-parse"),
		    code);
	  return(XEN_FALSE);
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

void g_init_run(void)
{
  XEN_DEFINE_PROCEDURE("run", g_run, 1, 0, 0, "run macro...");
  XEN_DEFINE_PROCEDURE("run-eval", g_run_eval, 1, 1, 0, "run macro...");

  #define H_optimization_hook S_optimization_hook " (msg) is called possibly several times \
during optimization to indicate where the optimizer ran into trouble"

  XEN_DEFINE_HOOK(optimization_hook, S_optimization_hook, 1, H_optimization_hook);      /* arg = message */
}

#endif
#else
/* no guile */
void g_init_run(void)
{
}
#endif

#if 0
/* not done yet for CLM:
  int mus_close_file              PROTO((mus_any *ptr));
  Float mus_srate                 PROTO((void));
  Float mus_set_srate             PROTO((Float val));

Float mus_sum_of_sines          PROTO((Float *amps, Float *phases, int size));
Float mus_dot_product           PROTO((Float *data1, Float *data2, int size));

void mus_spectrum               PROTO((Float *rdat, Float *idat, Float *window, int n, int type));

void mus_fft                    PROTO((Float *rl, Float *im, int n, int is));
Float *mus_make_fft_window      PROTO((int type, int size, Float beta));
Float *mus_make_fft_window_with_window PROTO((int type, int size, Float beta, Float *window));

 [with size arg] Float mus_array_interp          PROTO((Float *wave, Float phase, int size)); 

void mus_restart_env            PROTO((mus_any *ptr));

int mus_equalp                  PROTO((mus_any *g1, mus_any *g2));

mus_any *mus_buffer2frame       PROTO((mus_any *rb, mus_any *fr));
mus_any *mus_frame2buffer       PROTO((mus_any *rb, mus_any *fr));

Float mus_env_interp            PROTO((Float x, mus_any *env));

mus_frame *mus_frame_add        PROTO((mus_frame *f1, mus_frame *f2, mus_frame *res));
mus_frame *mus_frame_multiply   PROTO((mus_frame *f1, mus_frame *f2, mus_frame *res));

Float mus_frame_set             PROTO((mus_frame *f, int chan, Float val));
Float mus_locsig_set            PROTO((mus_any *ptr, int chan, Float val));
Float mus_locsig_reverb_set     PROTO((mus_any *ptr, int chan, Float val));

Float mus_mixer_ref             PROTO((mus_mixer *f, int in, int out));
Float mus_mixer_set             PROTO((mus_mixer *f, int in, int out, Float val));

mus_frame *mus_frame2frame      PROTO((mus_mixer *f, mus_frame *in, mus_frame *out));
mus_frame *mus_sample2frame     PROTO((mus_any *f, Float in, mus_frame *out));
Float mus_frame2sample          PROTO((mus_any *f, mus_frame *in));
mus_mixer *mus_mixer_multiply   PROTO((mus_mixer *f1, mus_mixer *f2, mus_mixer *res));
Float mus_in_any                PROTO((int frame, int chan, mus_input *IO));
Float mus_ina                   PROTO((int frame, mus_input *inp));
Float mus_inb                   PROTO((int frame, mus_input *inp));
mus_frame *mus_file2frame       PROTO((mus_any *ptr, int samp, mus_frame *f));
Float mus_sample2file           PROTO((mus_any *ptr, int samp, int chan, Float val));
Float mus_out_any               PROTO((int frame, Float val, int chan, mus_output *IO));
Float mus_outa                  PROTO((int frame, Float val, mus_output *IO));
Float mus_outb                  PROTO((int frame, Float val, mus_output *IO));
Float mus_outc                  PROTO((int frame, Float val, mus_output *IO));
Float mus_outd                  PROTO((int frame, Float val, mus_output *IO));
mus_frame *mus_frame2file       PROTO((mus_any *ptr, int samp, mus_frame *data));
mus_frame *mus_locsig           PROTO((mus_any *ptr, int loc, Float val));
void mus_move_locsig            PROTO((mus_any *ptr, Float degree, Float distance));
void mus_convolve_files         PROTO((const char *file1, const char *file2, Float maxamp, const char *output_file));
Float *mus_set_data             PROTO((mus_any *gen, Float *data));
Float mus_formant_bank          PROTO((Float *amps, mus_any **formants, Float inval, int size));
Float mus_set_formant_radius    PROTO((mus_any *ptr, Float val));
void mus_set_formant_radius_and_frequency PROTO((mus_any *ptr, Float radius, Float frequency));
void mus_mix                    PROTO((const char *outfile, const char *infile, int out_start, int out_samps, int in_start, mus_mixer *mx, mus_any ***envs));
mus_random mus_channel
int mus_file2fltarray           PROTO((const char *filename, int chan, int start, int samples, Float *array));
int mus_fltarray2file           PROTO((const char *filename, Float *ddata, int len, int srate, int channels));

  Float mus_oscil_bank            PROTO((Float *amps, mus_any **oscils, Float *inputs, int size));
  mus_any *mus_make_oscil         PROTO((Float freq, Float phase));
  mus_any *mus_make_sum_of_cosines PROTO((int cosines, Float freq, Float phase));
  mus_any *mus_make_delay         PROTO((int size, Float *line, int line_size));
  mus_any *mus_make_comb          PROTO((Float scaler, int size, Float *line, int line_size));
  mus_any *mus_make_notch         PROTO((Float scaler, int size, Float *line, int line_size));
  mus_any *mus_make_all_pass      PROTO((Float backward, Float forward, int size, Float *line, int line_size));
  mus_any *mus_make_table_lookup  PROTO((Float freq, Float phase, Float *wave, int wave_size));
  Float *mus_partials2wave        PROTO((Float *partial_data, int partials, Float *table, int table_size, int normalize));
  Float *mus_phasepartials2wave   PROTO((Float *partial_data, int partials, Float *table, int table_size, int normalize));
  mus_any *mus_make_sawtooth_wave PROTO((Float freq, Float amp, Float phase));
  mus_any *mus_make_square_wave   PROTO((Float freq, Float amp, Float phase));
  mus_any *mus_make_triangle_wave PROTO((Float freq, Float amp, Float phase));
  mus_any *mus_make_pulse_train   PROTO((Float freq, Float amp, Float phase));
  void mus_set_rand_seed          PROTO((unsigned long seed));
  unsigned long mus_rand_seed     PROTO((void));
  mus_any *mus_make_rand          PROTO((Float freq, Float base));
  mus_any *mus_make_rand_interp   PROTO((Float freq, Float base));
  mus_any *mus_make_asymmetric_fm PROTO((Float freq, Float phase, Float r, Float ratio));
  mus_any *mus_make_one_zero      PROTO((Float a0, Float a1));
  mus_any *mus_make_one_pole      PROTO((Float a0, Float b1));
  mus_any *mus_make_two_zero      PROTO((Float a0, Float a1, Float a2));
  mus_any *mus_make_zpolar        PROTO((Float radius, Float frequency));
  mus_any *mus_make_two_pole      PROTO((Float a0, Float b1, Float b2));
  mus_any *mus_make_ppolar        PROTO((Float radius, Float frequency));
  mus_any *mus_make_formant       PROTO((Float radius, Float frequency, Float gain));
  mus_any *mus_make_sine_summation PROTO((Float frequency, Float phase, int n, Float a, Float b_ratio));
  mus_any *mus_make_filter        PROTO((int order, Float *xcoeffs, Float *ycoeffs, Float *state));
  mus_any *mus_make_fir_filter    PROTO((int order, Float *xcoeffs, Float *state));
  mus_any *mus_make_iir_filter    PROTO((int order, Float *ycoeffs, Float *state));
  Float *mus_make_fir_coeffs      PROTO((int order, Float *env, Float *aa));
  mus_any *mus_make_wave_train    PROTO((Float freq, Float phase, Float *wave, int wsize));
  mus_any *mus_make_buffer        PROTO((Float *preloaded_buffer, int size, Float current_file_time));
  mus_any *mus_make_waveshape     PROTO((Float frequency, Float phase, Float *table, int size));
  Float *mus_partials2waveshape   PROTO((int npartials, Float *partials, int size, Float *table));
  Float *mus_partials2polynomial  PROTO((int npartials, Float *partials, int kind));
  mus_any *mus_make_env           PROTO((Float *brkpts, int pts, Float scaler, Float offset, Float base, Float duration, int start, int end, Float *odata));
  int *mus_env_passes             PROTO((mus_any *gen));
  double *mus_env_rates           PROTO((mus_any *gen));
  mus_frame *mus_make_empty_frame PROTO((int chans));
  mus_frame *mus_make_frame       PROTO((int chans, ...));
  mus_mixer *mus_make_empty_mixer PROTO((int chans));
  mus_mixer *mus_make_identity_mixer PROTO((int chans));
  mus_mixer *mus_make_mixer       PROTO((int chans, ...));
  Float **mus_mixer_data          PROTO((mus_mixer *f));
  mus_any *mus_make_file2sample   PROTO((const char *filename));
  mus_any *mus_make_readin        PROTO((const char *filename, int chan, int start, int direction));
  mus_any *mus_make_file2frame    PROTO((const char *filename));
  mus_any *mus_make_sample2file   PROTO((const char *filename, int chans, int out_format, int out_type));
  mus_any *mus_make_sample2file_with_comment PROTO((const char *filename, int out_chans, int out_format, int out_type, const char *comment));
  mus_any *mus_make_frame2file    PROTO((const char *filename, int chans, int out_format, int out_type));
  mus_any *mus_make_locsig        PROTO((Float degree, Float distance, Float reverb, int chans, mus_output *output, mus_output *revput, int type));
  mus_any *mus_make_src           PROTO((Float (*input)(void *arg, int direction), Float srate, int width, void *environ));
  mus_any *mus_make_convolve      PROTO((Float (*input)(void *arg, int direction), Float *filter, int fftsize, int filtersize, void *environ));
  mus_any *mus_make_granulate     PROTO((Float (*input)(void *arg, int direction), 
  Float mus_apply                 PROTO((mus_any *gen, ...));
  Float mus_bank                  PROTO((mus_any **gens, Float *scalers, Float *arg1, Float *arg2, int size));
  mus_any *mus_make_phase_vocoder PROTO((Float (*input)(void *arg, int direction), 
*/
#endif
#if 0
/* not done yet in Scheme:
case
close-input-port
close-output-port
current-input-port
current-output-port
display with port arg
eof-object?
input-port?
newline [port]
open-input-file
open-output-file
output-port?
read [port]
read-char [port]
optimize string-append et al
write obj [port]
write-char char [port]
*/
#endif
/* and from Snd? */
