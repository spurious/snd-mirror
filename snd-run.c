/* run macro
 *   initial timing tests indicate that this is 20 times as fast as Guile.
 *   Rather than write/compile (via gcc) a C source file, as in CLM, this
 *   produces the intermediate "triples" on the fly, packaging them into
 *   a "program" (a list of triples), and precomputing all function, program,
 *   and data addresses (as if a loader were at work); a kind of byte-compiler
 *   without the bytes or the compiler (that is, the function search is
 *   done at read time, not eval time).  This can work in general except
 *   for recursion, call/cc where the continuation is actually saved,
 *   and variables that have more than 1 type (not to mention delay, 
 *   define-syntax etc).  I could probably handle the latter through 
 *   some kludgery, but the whole point is to run fast,
 *   and the context is a sound editor (DSP stuff) where recursion and
 *   so forth is not a big deal.
 *
 * The evaluator is eval_ptree.  The code walker is walk.  A program
 *   is a list of triples. Each triple has a function pointer and a
 *   pointer to addresses of arguments.  There are two special addresses:
 *   the program counter (PC) and the termination flag (ALL_DONE).
 *
 * currently handled, at least partially:
 *   lambda if begin or and
 *   * + - / > < >= <= =
 *   sin cos tan abs log exp 
 *   acos asin sqrt (assuming no complex, so args are assumed to be in float result range -- on optimization switch?)
 *   boolean? exact? inexact? integer? real?
 *   odd? even? zero? positive? negative?
 *   not
 *   round truncate floor ceiling
 */

#include "snd.h"
#if WITH_RUN

#define XEN_EXACT_P(Arg) XEN_TRUE_P(scm_exact_p(Arg))
#define XEN_SYMBOL_TO_VALUE(a) XEN_VARIABLE_REF(scm_sym2var(a, scm_current_module_lookup_closure (), XEN_TRUE))

enum {R_INT, R_DBL, R_BOOL};

typedef struct {
  void (*function)(int *arg_addrs, int *ints, Float *dbls);
  int *args;
  char *(*descr)(int *arg_addrs, int *ints, Float *dbls); /* for debugging */
} triple;

static triple *free_triple(triple *trp)
{
  if (trp->args) FREE(trp->args);
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
} xen_value;

static xen_value *make_xen_value(int typ, int address, int constant)
{
  xen_value *v;
  /* fprintf(stderr,"add %d ", address); */
  v = (xen_value *)CALLOC(1, sizeof(xen_value));
  v->type = typ;
  v->addr = address;
  v->constant = constant;
  return(v);
}

static xen_value *copy_xen_value(xen_value *v)
{
  return(make_xen_value(v->type, v->addr, v->constant));
}

static char *describe_xen_value(xen_value *v, int *ints, Float *dbls)
{
  char *buf;
  buf = (char *)CALLOC(32, sizeof(char));
  if (v->type == R_BOOL)
    snprintf(buf, 32, "i%d(%s)", v->addr, (ints[v->addr] == 0) ? "#f" : "#t");
  else
    {
      if (v->type == R_INT)
	snprintf(buf, 32, "i%d(%d)", v->addr, ints[v->addr]);
      else snprintf(buf, 32, "d%d(%.4f)", v->addr, dbls[v->addr]);
    }
  return(buf);
}

typedef struct {
  char *name;
  xen_value *v;
  int global;
} xen_var;

static char *describe_xen_var(xen_var *var, int *ints, Float *dbls)
{
  char *buf, *temp;
  temp = describe_xen_value(var->v, ints, dbls);
  if (temp)
    {
      buf = (char *)CALLOC(strlen(var->name) + strlen(temp) + 4, sizeof(char));
      sprintf(buf, "%s: %s", var->name, temp);
      FREE(temp);
    }
  else buf = copy_string(var->name);
  return(buf);
}

static xen_var *free_xen_var(xen_var *var)
{
  if (var)
    {
      if (var->name) FREE(var->name);
      if (var->v) FREE(var->v);
      FREE(var);
    }
  return(NULL);
}

typedef struct {
  triple **program;
  int *ints;
  Float *dbls;
  int program_size, ints_size, dbls_size, triple_ctr, int_ctr, dbl_ctr;
  xen_var **vars;
  int vars_size, var_ctr;
  xen_value *result;
  int *args; /* ? -- need addresses of inputs (in xen_vars) */
  int arity;
} ptree;

static void describe_ptree(ptree *p)
{
  int i;
  char *temp;
  fprintf(stderr,"ints: %d, dbls: %d, triples: %d, vars: %d\n",
	  p->int_ctr, p->dbl_ctr, p->triple_ctr, p->var_ctr);
  for (i = 0; i < p->triple_ctr; i++)
    {
      temp = describe_triple(p->program[i], p->ints, p->dbls);
      fprintf(stderr, "  %d: %s\n", i, temp);
      FREE(temp);
    }
  for (i = 0; i < p->var_ctr; i++)
    {
      temp = describe_xen_var(p->vars[i], p->ints, p->dbls);
      fprintf(stderr, temp);
      FREE(temp);
    }
  temp = describe_xen_value(p->result, p->ints, p->dbls);
  fprintf(stderr,"\nresult: %s\n", temp);
  FREE(temp);
}

static XEN g_describe_ptree(XEN prog)
{
  if (!(XEN_FALSE_P(prog)))
    describe_ptree((ptree *)XEN_UNWRAP_C_POINTER(prog));
  return(XEN_FALSE);
}

static ptree *make_ptree(int initial_data_size)
{
  ptree *pt;
  pt = (ptree *)CALLOC(1, sizeof(ptree));
  if (initial_data_size > 0)
    {
      pt->ints = (int *)CALLOC(initial_data_size, sizeof(int));
      pt->dbls = (Float *)CALLOC(initial_data_size, sizeof(Float));
    }
  pt->program_size = 0;
  pt->ints_size = initial_data_size;
  pt->dbls_size = initial_data_size;
  pt->triple_ctr = 0;
  pt->int_ctr = 2;
  pt->dbl_ctr = 0;
  pt->vars_size = 0;
  pt->var_ctr = 0;
  pt->vars = NULL;
  return(pt);
}

static ptree *free_ptree(ptree *pt)
{
  int i;
  if (pt)
    {
      if (pt->ints) FREE(pt->ints);
      if (pt->dbls) FREE(pt->dbls);
      if (pt->vars)
	{
	  for (i = 0; i < pt->var_ctr; i++)
	    free_xen_var(pt->vars[i]);
	  FREE(pt->vars);
	}
      if (pt->program)
	{
	  for (i = 0; i < pt->program_size; i++)
	    if (pt->program[i])
	      pt->program[i] = free_triple(pt->program[i]);
	  FREE(pt->program);
	}
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

static int add_var_to_ptree(ptree *pt, char *name, xen_value *v)
{
  int cur;
  xen_var *var;
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
  var = (xen_var *)CALLOC(1, sizeof(xen_var));
  var->name = copy_string(name);
  var->v = copy_xen_value(v);
  pt->vars[pt->var_ctr++] = var;
  return(cur);
}

static xen_var *find_var_in_ptree(ptree *pt, char *name)
{
  /* search backwards for shadowing */
  int i;

  for (i = pt->var_ctr - 1; i >= 0; i--)
    if (strcmp(pt->vars[i]->name, name) == 0)
      return(pt->vars[i]);
  return(NULL);
}

#define PC ints[0]
#define ALL_DONE ints[1]

static void eval_ptree(ptree *prog)
{
  /* evaluates program, result in prog->result, assumes args already passed in addrs given by prog->addrs */
  triple *curfunc;
  int *ints;
  Float *dbls;
  ints = prog->ints;
  dbls = prog->dbls;
  PC = 0;
  ALL_DONE = FALSE;
  while (!ALL_DONE)
    {
      curfunc = prog->program[PC++];
      (*(curfunc->function))(curfunc->args, ints, dbls);
    }
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

static xen_value *walk(ptree *prog, XEN form, int need_result);

/* much kludgery from here on to get this working briefly in Snd to run some realistic timing tests */
/* (map-channel (lambda (y) (* y 2.5))) over storm.snd:               4.7
 * (run-channel (lambda (y) (* y 2.5))) same data same results:       0.23
 */

#define BOOL_RESULT ints[args[0]]
#define DBL_RESULT dbls[args[0]]
#define INT_RESULT ints[args[0]]
#define BOOL_ARG_1 ints[args[1]]
#define BOOL_ARG_2 ints[args[2]]
#define BOOL_ARG_3 ints[args[3]]
#define INT_ARG_1 ints[args[1]]
#define INT_ARG_2 ints[args[2]]
#define INT_ARG_3 ints[args[3]]
#define INT_ARG_4 ints[args[3]]
#define DBL_ARG_1 dbls[args[1]]
#define DBL_ARG_2 dbls[args[2]]
#define DBL_ARG_3 dbls[args[3]]
#define DBL_ARG_4 dbls[args[4]]

static void jump(int *args, int *ints, Float *dbls) {PC += ints[args[0]];}
static char *descr_jump(int *args, int *ints, Float *dbls) {return(mus_format("jump i%d(%d)", args[0], INT_RESULT));}

static void jump_if(int *args, int *ints, Float *dbls) {if (ints[args[1]] != 0) PC += ints[args[0]];}
static char *descr_jump_if(int *args, int *ints, Float *dbls) {return(mus_format("if (i%d(%d)) jump i%d(%d)", args[1], INT_ARG_1, args[0], INT_RESULT));}

static void jump_if_not(int *args, int *ints, Float *dbls) {if (ints[args[1]] == 0) PC += ints[args[0]];}
static char *descr_jump_if_not(int *args, int *ints, Float *dbls) {return(mus_format("if (!i%d(%d)) jump i%d(%d)", args[1], INT_ARG_1, args[0], INT_RESULT));}

static void store_i(int *args, int *ints, Float *dbls) {INT_RESULT = INT_ARG_1;}
static char *descr_store_i(int *args, int *ints, Float *dbls) {return(mus_format("i%d(%d) = i%d(%d)", args[0], INT_RESULT, args[1], INT_ARG_1));}

static void store_f(int *args, int *ints, Float *dbls) {DBL_RESULT = DBL_ARG_1;}
static char *descr_store_f(int *args, int *ints, Float *dbls) {return(mus_format("d%d(%.4f) = d%d(%.4f)", args[0], DBL_RESULT, args[1], DBL_ARG_1));}

static void store_f_i(int *args, int *ints, Float *dbls) {INT_RESULT = (int)DBL_ARG_1;}
static char *descr_store_f_i(int *args, int *ints, Float *dbls) {return(mus_format("i%d(%d) = d%d(%.4f)", args[0], INT_RESULT, args[1], DBL_ARG_1));}

static void store_i_f(int *args, int *ints, Float *dbls) {DBL_RESULT = (Float)INT_ARG_1;}
static char *descr_store_i_f(int *args, int *ints, Float *dbls) {return(mus_format("d%d(%.4f) = i%d(%d)", args[0], DBL_RESULT, args[1], INT_ARG_1));}

static void store_false(int *args, int *ints, Float *dbls) {BOOL_RESULT = 0;}
static char *descr_store_false(int *args, int *ints, Float *dbls) {return(mus_format("i%d(%d) = 0", args[0], BOOL_RESULT));}

static void store_true(int *args, int *ints, Float *dbls) {BOOL_RESULT = 1;}
static char *descr_store_true(int *args, int *ints, Float *dbls) {return(mus_format("i%d(%d) = 1", args[0], BOOL_RESULT));}

static xen_value *convert_int_to_dbl(ptree *prog, xen_value *i)
{
  xen_value *val;
  val = make_xen_value(R_DBL, add_dbl_to_ptree(prog, 0.0), i->constant);
  if (i->constant)
    prog->dbls[val->addr] = (Float)(prog->ints[i->addr]);
  else add_triple_to_ptree(prog, va_make_triple(store_i_f, descr_store_i_f, 2, val, i));
  return(val);
}

static xen_value *lambda_form(ptree *prog, XEN form)
{
  /* (lambda (args...) | args etc followed by forms */
  /* as args are declared as vars, put addrs in prog->args list */
  XEN args, arg, body;
  xen_value *v = NULL;
  int i, arg_num, body_forms;
  args = XEN_CADR(form);
  if (!(XEN_LIST_P(args))) return(NULL);
  arg_num = XEN_LIST_LENGTH(args);
  body = XEN_CDDR(form);
  body_forms = XEN_LIST_LENGTH(body);
  prog->arity = arg_num;
  if (arg_num > 0)
    {
      prog->args = (int *)CALLOC(arg_num, sizeof(int));
      for (i = 0; i < arg_num; i++, args = XEN_CDR(args))
	{
	  arg = XEN_CAR(args);
	  add_var_to_ptree(prog, 
			     XEN_SYMBOL_TO_C_STRING(arg), 
			     v = make_xen_value(R_DBL, add_dbl_to_ptree(prog, 0.0), FALSE));
	  prog->args[i] = v->addr;
	}
    }
  for (i = 0; i < body_forms; i++, body = XEN_CDR(body))
    v = walk(prog, XEN_CAR(body), (i == (body_forms - 1)));
  return(v);
}

static xen_value *begin_form(ptree *prog, XEN form, int need_result)
{
  /* (begin ...) returning last */
  XEN body;
  xen_value *v = NULL;
  int i, body_forms;
  body = XEN_CDR(form);
  body_forms = XEN_LIST_LENGTH(body);
  for (i = 0; i < body_forms; i++, body = XEN_CDR(body))
    v = walk(prog, XEN_CAR(body), ((need_result) && (i == (body_forms - 1))));
  return(v);
}

static xen_value *if_form(ptree *prog, XEN form, int need_result)
{
  /* form: (if selector true [false]) */
  xen_value *result = NULL, *true_result = NULL, *false_result = NULL;
  xen_value *jump_to_end = NULL, *jump_to_false, *if_value;
  int current_pc, false_pc = 0, has_false;
  current_pc = prog->triple_ctr; /* the selector actually */
  has_false = (XEN_LIST_LENGTH(form) == 4);
  if_value = walk(prog, XEN_CADR(form), TRUE);                                      /* walk selector */
  if (if_value == NULL) return(NULL);
  if (if_value->type != R_BOOL) return(NULL);
  jump_to_false = make_xen_value(R_INT, add_int_to_ptree(prog, 0), FALSE);
  add_triple_to_ptree(prog, va_make_triple(jump_if_not, descr_jump_if_not, 2, jump_to_false, if_value));
  true_result = walk(prog, XEN_CADDR(form), TRUE);                                  /* walk true branch */
  if (true_result == NULL) return(NULL);
  if (need_result)
    {
      if (true_result->type == R_INT)
	{
	  result = make_xen_value(R_INT, add_int_to_ptree(prog, 0), FALSE);
	  add_triple_to_ptree(prog, va_make_triple(store_i, descr_store_i, 2, result, true_result));
	}
      else
	{
	  result = make_xen_value(R_DBL, add_int_to_ptree(prog, 0.0), FALSE);
	  add_triple_to_ptree(prog, va_make_triple(store_f, descr_store_f, 2, result, true_result));
	}
    }
  if (has_false) 
    {
      false_pc = prog->triple_ctr;
      jump_to_end = make_xen_value(R_INT, add_int_to_ptree(prog, 0), FALSE);
      add_triple_to_ptree(prog, va_make_triple(jump, descr_jump, 1, jump_to_end));  /* jump to end (past false) */
    }
  prog->ints[jump_to_false->addr] = prog->triple_ctr - current_pc - 2;              /* fixup jump-to-false addr */
  if (has_false)
    {
      false_result = walk(prog, XEN_CADDDR(form), TRUE);                            /* walk false branch */
      if (false_result == NULL) return(NULL);
      prog->ints[jump_to_end->addr] = prog->triple_ctr - false_pc;                  /* fixup jump-past-false addr */
      if (false_result->type != true_result->type)
	return(NULL); /* TODO: cleanup */
      if (need_result)
	{
	  if (false_result->type == R_INT)
	    add_triple_to_ptree(prog, va_make_triple(store_i, descr_store_i, 2, result, false_result));
	  else add_triple_to_ptree(prog, va_make_triple(store_f, descr_store_f, 2, result, false_result));
	}
    }
  else 
    {
      if (jump_to_end) FREE(jump_to_end);
      if (jump_to_false) FREE(jump_to_false);
      if (if_value) FREE(if_value);
      return(true_result);
    }
  if (true_result) FREE(true_result);
  if (false_result) FREE(false_result);
  if (jump_to_end) FREE(jump_to_end);
  if (jump_to_false) FREE(jump_to_false);
  if (if_value) FREE(if_value);
  if (result)
    return(result);
  return(make_xen_value(R_BOOL, -1, TRUE));
}

static xen_value *or_form(ptree *prog, XEN form)
{
  /* (or ...) returning as soon as #t seen -- assume booleans only here */
  XEN body;
  xen_value *v = NULL, *result = NULL, *jump_to_end;
  xen_value **fixups;
  int i, body_forms;
  body = XEN_CDR(form);
  body_forms = XEN_LIST_LENGTH(body);
  if (body_forms == 0)                  /* (or) -> #f */
    return(make_xen_value(R_BOOL, add_int_to_ptree(prog, 0), TRUE));
  fixups = (xen_value **)CALLOC(body_forms, sizeof(xen_value *));
  for (i = 0; i < body_forms; i++, body = XEN_CDR(body))
    {
      v = walk(prog, XEN_CAR(body), TRUE);
      /* TODO: if v->type != R_BOOL return NULL after cleanup (same for and_form) */
      fixups[i] = make_xen_value(R_INT, add_int_to_ptree(prog, prog->triple_ctr), FALSE);
      add_triple_to_ptree(prog, va_make_triple(jump_if, descr_jump_if, 2, fixups[i], v));
      FREE(v);
    }
  /* if we fall through, return #f */
  result = make_xen_value(R_BOOL, add_int_to_ptree(prog, 0), FALSE);
  add_triple_to_ptree(prog, va_make_triple(store_false, descr_store_false, 1, result));
  jump_to_end = make_xen_value(R_INT, add_int_to_ptree(prog, prog->triple_ctr), FALSE);
  add_triple_to_ptree(prog, va_make_triple(jump, descr_jump, 1, jump_to_end));
  /* now fixup all the jumps to end up here */
  for (i = 0; i < body_forms; i++)
    {
      prog->ints[fixups[i]->addr] = prog->triple_ctr - prog->ints[fixups[i]->addr];
      FREE(fixups[i]);
    }
  add_triple_to_ptree(prog, va_make_triple(store_true, descr_store_true, 1, result));
  prog->ints[jump_to_end->addr] = prog->triple_ctr - prog->ints[jump_to_end->addr];
  FREE(jump_to_end);
  return(result);
}

static xen_value *and_form(ptree *prog, XEN form)
{
  /* (and ...) returning as soon as #f seen -- assume booleans only here */
  XEN body;
  xen_value *v = NULL, *result = NULL, *jump_to_end;
  xen_value **fixups;
  int i, body_forms;
  body = XEN_CDR(form);
  body_forms = XEN_LIST_LENGTH(body);
  if (body_forms == 0)                  /* (and) -> #t */
    return(make_xen_value(R_BOOL, add_int_to_ptree(prog, 1), TRUE));
  fixups = (xen_value **)CALLOC(body_forms, sizeof(xen_value *));
  for (i = 0; i < body_forms; i++, body = XEN_CDR(body))
    {
      v = walk(prog, XEN_CAR(body), TRUE);
      fixups[i] = make_xen_value(R_INT, add_int_to_ptree(prog, prog->triple_ctr), FALSE);
      add_triple_to_ptree(prog, va_make_triple(jump_if, descr_jump_if_not, 2, fixups[i], v));
      FREE(v);
    }
  /* if we fall through, return #t */
  result = make_xen_value(R_BOOL, add_int_to_ptree(prog, 1), FALSE);
  add_triple_to_ptree(prog, va_make_triple(store_true, descr_store_true, 1, result));
  jump_to_end = make_xen_value(R_INT, add_int_to_ptree(prog, prog->triple_ctr), FALSE);
  add_triple_to_ptree(prog, va_make_triple(jump, descr_jump, 1, jump_to_end));
  /* now fixup all the jumps to end up here */
  for (i = 0; i < body_forms; i++)
    {
      prog->ints[fixups[i]->addr] = prog->triple_ctr - prog->ints[fixups[i]->addr];
      FREE(fixups[i]);
    }
  add_triple_to_ptree(prog, va_make_triple(store_false, descr_store_false, 1, result));
  prog->ints[jump_to_end->addr] = prog->triple_ctr - prog->ints[jump_to_end->addr];
  FREE(jump_to_end);
  return(result);
}

static void package(ptree *prog,
		    int type, 
		    void (*function)(int *arg_addrs, int *ints, Float *dbls),
		    char *(*descr)(int *arg_addrs, int *ints, Float *dbls),
		    xen_value **args,
		    int num_args)
{
  args[0] = make_xen_value(type, 
			   (type == R_DBL) ? add_dbl_to_ptree(prog, 0.0) : add_int_to_ptree(prog, 0), 
			   FALSE);
  add_triple_to_ptree(prog, make_triple(function, descr, args, num_args + 1));
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

static xen_value *give_up(int num, xen_value **junk)
{
  int i;
  for (i = 0; i < num; i++)
    if (junk[i]) FREE(junk[i]);
  return(NULL);
}

static xen_value *clean_up(int num_args, xen_value **args)
{
  int i;
  xen_value *v;
  for (i = 1; i < num_args; i++) FREE(args[i]);
  v = args[0];
  FREE(args);
  return(v);
}


/* ---------------- multiply ---------------- */

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
	    args[j] = make_xen_value(R_DBL, add_dbl_to_ptree(prog, 0.0), FALSE);
	    add_triple_to_ptree(prog, va_make_triple(store_i_f, descr_store_i_f, 2, args[j], old_loc));
	    FREE(old_loc);
	    j++;
	  }
	else args[j++] = args[i];
      }
  return(j - 1);
}

static void multiply_f2(int *args, int *ints, Float *dbls) {dbls[args[0]] = (DBL_ARG_1 * DBL_ARG_2);}
static char *descr_multiply_f2(int *args, int *ints, Float *dbls) {return(describe_dbl_args("*", 2, args, dbls, 1));}

static void multiply_f3(int *args, int *ints, Float *dbls) {DBL_RESULT = (DBL_ARG_1 * DBL_ARG_2 * DBL_ARG_3);}
static char *descr_multiply_f3(int *args, int *ints, Float *dbls) {return(describe_dbl_args("*", 3, args, dbls, 1));}

static void multiply_fn(int *args, int *ints, Float *dbls) 
{
  int i, n;
  n = ints[args[1]];
  DBL_RESULT = dbls[args[2]];
  for (i = 1; i < n; i++) DBL_RESULT *= dbls[args[i + 2]];
}
static char *descr_multiply_fn(int *args, int *ints, Float *dbls) {return(describe_dbl_args("*", ints[args[1]], args, dbls, 2));}

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

static char *descr_multiply_in(int *args, int *ints, Float *dbls) {return(describe_int_args("*", ints[args[1]], args, ints, 2));}

static xen_value *multiply(ptree *prog, int float_result, xen_value **args, int num_args, int constants, int need_result)
{
  int iscl = 1, cons_loc = 0;
  Float fscl = 1.0;
  int i;
  if (num_args == 1) return(args[1]);
  /* if (!need_result) return(clean_up(num_args + 1, args)); */ /* can't decided about this */
  if (constants > 0)
    {
      for (i = 1; i <= num_args; i++)
	if (args[i]->constant)
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
	    args[cons_loc] = make_xen_value(R_DBL, add_dbl_to_ptree(prog, fscl * iscl), TRUE);
	  else args[cons_loc] = make_xen_value(R_INT, add_int_to_ptree(prog, iscl), TRUE);
	  if ((iscl == 0) || (fscl == 0.0))
	    return(args[cons_loc]);
	}
      if (constants == num_args) return(args[cons_loc]);
    }
  num_args = float_all_args(prog, num_args, args, float_result);
  if (float_result)
    {
      if (num_args == 2) package(prog, R_DBL, multiply_f2, descr_multiply_f2, args, num_args); else
      if (num_args == 3) package(prog, R_DBL, multiply_f3, descr_multiply_f3, args, num_args); else
      package(prog, R_DBL, multiply_fn, descr_multiply_fn, args, num_args);
    }
  else
    {
      if (num_args == 2) package(prog, R_INT, multiply_i2, descr_multiply_i2, args, num_args); else
      if (num_args == 3) package(prog, R_INT, multiply_i3, descr_multiply_i3, args, num_args); else
      package(prog, R_INT, multiply_in, descr_multiply_in, args, num_args);
    }
  return(clean_up(num_args, args));
}


/* ---------------- add ---------------- */

static void add_f2(int *args, int *ints, Float *dbls) {dbls[args[0]] = (DBL_ARG_1 + DBL_ARG_2);}
static char *descr_add_f2(int *args, int *ints, Float *dbls) {return(describe_dbl_args("+", 2, args, dbls, 1));}

static void add_f3(int *args, int *ints, Float *dbls) {DBL_RESULT = (DBL_ARG_1 + DBL_ARG_2 + DBL_ARG_3);}
static char *descr_add_f3(int *args, int *ints, Float *dbls) {return(describe_dbl_args("+", 3, args, dbls, 1));}

static void add_fn(int *args, int *ints, Float *dbls) 
{
  int i, n;
  n = ints[args[1]];
  DBL_RESULT = dbls[args[2]];
  for (i = 1; i < n; i++) DBL_RESULT += dbls[args[i + 2]];
}
static char *descr_add_fn(int *args, int *ints, Float *dbls) {return(describe_dbl_args("+", ints[args[1]], args, dbls, 2));}

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

static char *descr_add_in(int *args, int *ints, Float *dbls) {return(describe_int_args("+", ints[args[1]], args, ints, 2));}

static xen_value *add(ptree *prog, int float_result, xen_value **args, int num_args, int constants, int need_result)
{
  int iscl = 0, cons_loc = 0;
  Float fscl = 0.0;
  int i;
  if (num_args == 1) return(args[1]);
  /* if (!need_result) return(clean_up(num_args + 1, args)); */
  if (constants > 0)
    {
      for (i = 1; i <= num_args; i++)
	if (args[i]->constant)
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
	    args[cons_loc] = make_xen_value(R_DBL, add_dbl_to_ptree(prog, fscl + iscl), TRUE);
	  else args[cons_loc] = make_xen_value(R_INT, add_int_to_ptree(prog, iscl), TRUE);
	}
      if (constants == num_args) return(args[cons_loc]);
    }
  num_args = float_all_args(prog, num_args, args, float_result);
  if (float_result)
    {
      if (num_args == 2) package(prog, R_DBL, add_f2, descr_add_f2, args, num_args); else
      if (num_args == 3) package(prog, R_DBL, add_f3, descr_add_f3, args, num_args); else
      package(prog, R_DBL, add_fn, descr_add_fn, args, num_args);
    }
  else
    {
      if (num_args == 2) package(prog, R_INT, add_i2, descr_add_i2, args, num_args); else
      if (num_args == 3) package(prog, R_INT, add_i3, descr_add_i3, args, num_args); else
      package(prog, R_INT, add_in, descr_add_in, args, num_args);
    }
  return(clean_up(num_args, args));
}

/* ---------------- subtract ---------------- */

static void subtract_f1(int *args, int *ints, Float *dbls) {dbls[args[0]] = -(DBL_ARG_1);}
static char *descr_subtract_f1(int *args, int *ints, Float *dbls) {return(mus_format("d%d(%.4f) = -d%d(%.4f)", args[0], DBL_RESULT, args[1], DBL_ARG_1));}

static void subtract_f2(int *args, int *ints, Float *dbls) {dbls[args[0]] = (DBL_ARG_1 - DBL_ARG_2);}
static char *descr_subtract_f2(int *args, int *ints, Float *dbls) {return(describe_dbl_args("-", 2, args, dbls, 1));}

static void subtract_f3(int *args, int *ints, Float *dbls) {DBL_RESULT = (DBL_ARG_1 - DBL_ARG_2 - DBL_ARG_3);}
static char *descr_subtract_f3(int *args, int *ints, Float *dbls) {return(describe_dbl_args("-", 3, args, dbls, 1));}

static void subtract_fn(int *args, int *ints, Float *dbls) 
{
  int i, n;
  n = ints[args[1]];
  DBL_RESULT = dbls[args[2]];
  for (i = 1; i < n; i++) DBL_RESULT -= dbls[args[i + 2]];
}
static char *descr_subtract_fn(int *args, int *ints, Float *dbls) {return(describe_dbl_args("-", ints[args[1]], args, dbls, 2));}

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

static char *descr_subtract_in(int *args, int *ints, Float *dbls) {return(describe_int_args("-", ints[args[1]], args, ints, 2));}

static xen_value *subtract(ptree *prog, int float_result, xen_value **args, int num_args, int constants, int need_result)
{
  int iscl = 0, cons_loc = 0;
  Float fscl = 0.0;
  int i;
  if ((num_args == 1) && (args[1]->constant))
    {
      if (args[1]->type == R_INT)
	return(make_xen_value(R_INT, add_int_to_ptree(prog, -(prog->ints[args[1]->addr])), TRUE));
      else return(make_xen_value(R_DBL, add_dbl_to_ptree(prog, -(prog->dbls[args[1]->addr])), TRUE));
    }
  if (constants > 0)
    {
      for (i = 2; i <= num_args; i++)
	if (args[i]->constant)
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
	    args[cons_loc] = make_xen_value(R_DBL, add_dbl_to_ptree(prog, fscl + iscl), TRUE);
	  else args[cons_loc] = make_xen_value(R_INT, add_int_to_ptree(prog, iscl), TRUE);
	}
      if (constants == num_args) 
	{
	  if (float_result)
	    {
	      if (args[1]->type == R_INT)
		return(make_xen_value(R_DBL, add_dbl_to_ptree(prog, prog->dbls[args[1]->addr] - iscl), TRUE));
	      else return(make_xen_value(R_DBL, add_dbl_to_ptree(prog, prog->dbls[args[1]->addr] - (fscl + iscl)), TRUE));
	    }
	  else return(make_xen_value(R_INT, add_int_to_ptree(prog, prog->ints[args[1]->addr] - iscl), TRUE));
	}
    }
  num_args = float_all_args(prog, num_args, args, float_result);
  if (float_result)
    {
      if (num_args == 1) package(prog, R_DBL, subtract_f1, descr_subtract_f1, args, num_args); else
      if (num_args == 2) package(prog, R_DBL, subtract_f2, descr_subtract_f2, args, num_args); else
      if (num_args == 3) package(prog, R_DBL, subtract_f3, descr_subtract_f3, args, num_args); else
      package(prog, R_DBL, subtract_fn, descr_subtract_fn, args, num_args);
    }
  else
    {
      if (num_args == 1) package(prog, R_INT, subtract_i1, descr_subtract_i1, args, num_args); else
      if (num_args == 2) package(prog, R_INT, subtract_i2, descr_subtract_i2, args, num_args); else
      if (num_args == 3) package(prog, R_INT, subtract_i3, descr_subtract_i3, args, num_args); else
      package(prog, R_INT, subtract_in, descr_subtract_in, args, num_args);
    }
  return(clean_up(num_args, args));
}


/* ---------------- divide ---------------- */

static void divide_f1(int *args, int *ints, Float *dbls) {dbls[args[0]] = (1.0 / DBL_ARG_1);}
static char *descr_divide_f1(int *args, int *ints, Float *dbls) {return(mus_format("d%d(%.4f) = 1.0 / d%d(%.4f)", args[0], DBL_RESULT, args[1], DBL_ARG_1));}

static void divide_f2(int *args, int *ints, Float *dbls) {dbls[args[0]] = (DBL_ARG_1 / DBL_ARG_2);}
static char *descr_divide_f2(int *args, int *ints, Float *dbls) {return(describe_dbl_args("/", 2, args, dbls, 1));}

static void divide_f3(int *args, int *ints, Float *dbls) {DBL_RESULT = (DBL_ARG_1 / (DBL_ARG_2 * DBL_ARG_3));}
static char *descr_divide_f3(int *args, int *ints, Float *dbls) 
{
  return(mus_format("d%d(%.4f) = d%d(%.4f) / (d%d(%.4f) * d%d(%.4f))", args[0], DBL_RESULT, args[1], DBL_ARG_1, args[2], DBL_ARG_2, args[3], DBL_ARG_3));
}

static void divide_fn(int *args, int *ints, Float *dbls) 
{
  int i, n;
  Float divisor = 1.0;
  n = ints[args[1]];
  for (i = 1; i < n; i++) divisor *= dbls[args[i + 2]];
  DBL_RESULT = dbls[args[2]] / divisor;
}
static char *descr_divide_fn(int *args, int *ints, Float *dbls)
{
  char *buf, *str;
  int i, num_args;
  num_args = ints[args[1]];
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

static xen_value *divide(ptree *prog, xen_value **args, int num_args, int constants, int need_result)
{
  int cons_loc = 0;
  Float fscl = 1.0;
  int i;
  if ((num_args == 1) && (args[1]->constant))
    {
      if (args[1]->type == R_INT)
	return(make_xen_value(R_DBL, add_dbl_to_ptree(prog, (1.0 / (Float)(prog->ints[args[1]->addr]))), TRUE));
      else return(make_xen_value(R_DBL, add_dbl_to_ptree(prog, (1.0 / (prog->dbls[args[1]->addr]))), TRUE));
    }
  if (constants > 0)
    {
      for (i = 2; i <= num_args; i++)
	if (args[i]->constant)
	  {
	    cons_loc = i;
	    if (args[i]->type == R_INT)
	      fscl *= (Float)(prog->ints[args[i]->addr]);
	    else fscl *= prog->dbls[args[i]->addr];
	    FREE(args[i]);
	    args[i] = NULL;
	  }
      if (fscl != 1.0)
	args[cons_loc] = make_xen_value(R_DBL, add_dbl_to_ptree(prog, fscl), TRUE);
      if (constants == num_args) 
	{
	  if (args[1]->type == R_INT)
	    return(make_xen_value(R_DBL, add_dbl_to_ptree(prog, (Float)(prog->ints[args[1]->addr]) / fscl), TRUE));
	  else return(make_xen_value(R_DBL, add_dbl_to_ptree(prog, prog->dbls[args[1]->addr] / fscl), TRUE));
	}
    }
  num_args = float_all_args(prog, num_args, args, TRUE);
  if (num_args == 1) package(prog, R_DBL, divide_f1, descr_divide_f1, args, num_args); else
  if (num_args == 2) package(prog, R_DBL, divide_f2, descr_divide_f2, args, num_args); else
  if (num_args == 3) package(prog, R_DBL, divide_f3, descr_divide_f3, args, num_args); else
  package(prog, R_DBL, divide_fn, descr_divide_fn, args, num_args);
  return(clean_up(num_args, args));
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
    if ((args[i]->constant) && (args[i]->type == R_INT))
      {
	old_loc = args[i];
	args[i] = make_xen_value(R_DBL, add_dbl_to_ptree(prog, (Float)(prog->ints[args[i]->addr])), TRUE);
	FREE(old_loc);
      }
}

static void float_rel_args(ptree *prog, int num_args, xen_value **args, int float_result)
{
  int i;
  xen_value *old_loc;
  for (i = 1; i <= num_args; i++)
    if ((float_result) && (args[i]->type == R_INT))
      {
	old_loc = args[i];
	args[i] = make_xen_value(R_DBL, add_dbl_to_ptree(prog, 0.0), FALSE);
	add_triple_to_ptree(prog, va_make_triple(store_i_f, descr_store_i_f, 2, args[i], old_loc));
	FREE(old_loc);
      }
}


/* ---------------- greater-than ---------------- */

static void gt_f2(int *args, int *ints, Float *dbls) {BOOL_RESULT = (DBL_ARG_1 > DBL_ARG_2);}
static char *descr_gt_f2(int *args, int *ints, Float *dbls) {return(describe_rel_f_args(">", 2, args, ints, dbls, 1));}
static void gt_fn(int *args, int *ints, Float *dbls)
{
  int i, n;
  n = ints[args[1]];
  for (i = 2; i < n - 1; i++)
    {
      BOOL_RESULT = (dbls[args[i]] > dbls[args[i + 1]]);
      if (!BOOL_RESULT) break;
    }
}
static char *descr_gt_fn(int *args, int *ints, Float *dbls) {return(describe_rel_f_args(">", ints[args[1]], args, ints, dbls, 2));}
static void gt_i2(int *args, int *ints, Float *dbls) {BOOL_RESULT = (INT_ARG_1 > INT_ARG_2);}
static char *descr_gt_i2(int *args, int *ints, Float *dbls) {return(describe_rel_i_args(">", 2, args, ints, 1));}
static void gt_in(int *args, int *ints, Float *dbls)
{
  int i, n;
  n = ints[args[1]];
  for (i = 2; i < n - 1; i++)
    {
      BOOL_RESULT = (ints[args[i]] > ints[args[i + 1]]);
      if (!BOOL_RESULT) break;
    }
}
static char *descr_gt_in(int *args, int *ints, Float *dbls) {return(describe_rel_i_args(">", ints[args[1]], args, ints, 2));}
static xen_value *greater_than(ptree *prog, int float_result, xen_value **args, int num_args, int constants, int need_result)
{
  int i;
  if (num_args == 1) return(make_xen_value(R_BOOL, add_int_to_ptree(prog, 1), TRUE));
  if ((constants > 0) && (float_result)) float_rel_constant_args(prog, num_args, args);
  if (constants == num_args)
    {
      for (i = 1; i < num_args; i++)
	if (float_result)
	  {
	    if (prog->dbls[args[i]->addr] <= prog->dbls[args[i + 1]->addr])
	      return(make_xen_value(R_BOOL, add_int_to_ptree(prog, 0), TRUE));
	  }
	else
	  {
	    if (prog->ints[args[i]->addr] <= prog->ints[args[i + 1]->addr])
	      return(make_xen_value(R_BOOL, add_int_to_ptree(prog, 0), TRUE));
	  }
      return(make_xen_value(R_BOOL, add_int_to_ptree(prog, 1), TRUE));
    }
  float_rel_args(prog, num_args, args, float_result);
  if (float_result)
    {
      if (num_args == 2) package(prog, R_DBL, gt_f2, descr_gt_f2, args, num_args); else
      package(prog, R_DBL, gt_fn, descr_gt_fn, args, num_args);
    }
  else
    {
      if (num_args == 2) package(prog, R_INT, gt_i2, descr_gt_i2, args, num_args); else
      package(prog, R_INT, gt_in, descr_gt_in, args, num_args);
    }
  return(clean_up(num_args, args));
}

/* ---------------- greater-than-or-equal ---------------- */

static void geq_f2(int *args, int *ints, Float *dbls) {BOOL_RESULT = (DBL_ARG_1 >= DBL_ARG_2);}
static char *descr_geq_f2(int *args, int *ints, Float *dbls) {return(describe_rel_f_args(">=", 2, args, ints, dbls, 1));}
static void geq_fn(int *args, int *ints, Float *dbls)
{
  int i, n;
  n = ints[args[1]];
  for (i = 2; i < n - 1; i++)
    {
      BOOL_RESULT = (dbls[args[i]] >= dbls[args[i + 1]]);
      if (!BOOL_RESULT) break;
    }
}
static char *descr_geq_fn(int *args, int *ints, Float *dbls) {return(describe_rel_f_args(">=", ints[args[1]], args, ints, dbls, 2));}
static void geq_i2(int *args, int *ints, Float *dbls) {BOOL_RESULT = (INT_ARG_1 >= INT_ARG_2);}
static char *descr_geq_i2(int *args, int *ints, Float *dbls) {return(describe_rel_i_args(">=", 2, args, ints, 1));}

static void geq_in(int *args, int *ints, Float *dbls)
{
  int i, n;
  n = ints[args[1]];
  for (i = 2; i < n - 1; i++)
    {
      BOOL_RESULT = (ints[args[i]] >= ints[args[i + 1]]);
      if (!BOOL_RESULT) break;
    }
}
static char *descr_geq_in(int *args, int *ints, Float *dbls) {return(describe_rel_i_args(">=", ints[args[1]], args, ints, 2));}
static xen_value *greater_than_or_equal(ptree *prog, int float_result, xen_value **args, int num_args, int constants, int need_result)
{
  int i;
  if (num_args == 1) return(make_xen_value(R_BOOL, add_int_to_ptree(prog, 1), TRUE));
  if ((constants > 0) && (float_result)) float_rel_constant_args(prog, num_args, args);
  if (constants == num_args)
    {
      for (i = 1; i < num_args; i++)
	if (float_result)
	  {
	    if (prog->dbls[args[i]->addr] < prog->dbls[args[i + 1]->addr])
	      return(make_xen_value(R_BOOL, add_int_to_ptree(prog, 0), TRUE));
	  }
	else
	  {
	    if (prog->ints[args[i]->addr] < prog->ints[args[i + 1]->addr])
	      return(make_xen_value(R_BOOL, add_int_to_ptree(prog, 0), TRUE));
	  }
      return(make_xen_value(R_BOOL, add_int_to_ptree(prog, 1), TRUE));
    }
  float_rel_args(prog, num_args, args, float_result);
  if (float_result)
    {
      if (num_args == 2) package(prog, R_DBL, geq_f2, descr_geq_f2, args, num_args); else
      package(prog, R_DBL, geq_fn, descr_geq_fn, args, num_args);
    }
  else
    {
      if (num_args == 2) package(prog, R_INT, geq_i2, descr_geq_i2, args, num_args); else
      package(prog, R_INT, geq_in, descr_geq_in, args, num_args);
    }
  return(clean_up(num_args, args));
}

/* ---------------- less-than ---------------- */

static void lt_f2(int *args, int *ints, Float *dbls) {BOOL_RESULT = (DBL_ARG_1 < DBL_ARG_2);}
static char *descr_lt_f2(int *args, int *ints, Float *dbls) {return(describe_rel_f_args("<", 2, args, ints, dbls, 1));}
static void lt_fn(int *args, int *ints, Float *dbls)
{
  int i, n;
  n = ints[args[1]];
  for (i = 2; i < n - 1; i++)
    {
      BOOL_RESULT = (dbls[args[i]] < dbls[args[i + 1]]);
      if (!BOOL_RESULT) break;
    }
}
static char *descr_lt_fn(int *args, int *ints, Float *dbls) {return(describe_rel_f_args("<", ints[args[1]], args, ints, dbls, 2));}
static void lt_i2(int *args, int *ints, Float *dbls) {BOOL_RESULT = (INT_ARG_1 < INT_ARG_2);}
static char *descr_lt_i2(int *args, int *ints, Float *dbls) {return(describe_rel_i_args("<", 2, args, ints, 1));}
static void lt_in(int *args, int *ints, Float *dbls)
{
  int i, n;
  n = ints[args[1]];
  for (i = 2; i < n - 1; i++)
    {
      BOOL_RESULT = (ints[args[i]] < ints[args[i + 1]]);
      if (!BOOL_RESULT) break;
    }
}
static char *descr_lt_in(int *args, int *ints, Float *dbls) {return(describe_rel_i_args("<", ints[args[1]], args, ints, 2));}
static xen_value *less_than(ptree *prog, int float_result, xen_value **args, int num_args, int constants, int need_result)
{
  int i;
  if (num_args == 1) return(make_xen_value(R_BOOL, add_int_to_ptree(prog, 1), TRUE));
  if ((constants > 0) && (float_result)) float_rel_constant_args(prog, num_args, args);
  if (constants == num_args)
    {
      for (i = 1; i < num_args; i++)
	if (float_result)
	  {
	    if (prog->dbls[args[i]->addr] >= prog->dbls[args[i + 1]->addr])
	      return(make_xen_value(R_BOOL, add_int_to_ptree(prog, 0), TRUE));
	  }
	else
	  {
	    if (prog->ints[args[i]->addr] >= prog->ints[args[i + 1]->addr])
	      return(make_xen_value(R_BOOL, add_int_to_ptree(prog, 0), TRUE));
	  }
      return(make_xen_value(R_BOOL, add_int_to_ptree(prog, 1), TRUE));
    }
  float_rel_args(prog, num_args, args, float_result);
  if (float_result)
    {
      if (num_args == 2) package(prog, R_DBL, lt_f2, descr_lt_f2, args, num_args); else
      package(prog, R_DBL, lt_fn, descr_lt_fn, args, num_args);
    }
  else
    {
      if (num_args == 2) package(prog, R_INT, lt_i2, descr_lt_i2, args, num_args); else
      package(prog, R_INT, lt_in, descr_lt_in, args, num_args);
    }
  return(clean_up(num_args, args));
}

/* ---------------- less-than-or-equal ---------------- */

static void leq_f2(int *args, int *ints, Float *dbls) {BOOL_RESULT = (DBL_ARG_1 <= DBL_ARG_2);}
static char *descr_leq_f2(int *args, int *ints, Float *dbls) {return(describe_rel_f_args("<=", 2, args, ints, dbls, 1));}
static void leq_fn(int *args, int *ints, Float *dbls)
{
  int i, n;
  n = ints[args[1]];
  for (i = 2; i < n - 1; i++)
    {
      BOOL_RESULT = (dbls[args[i]] <= dbls[args[i + 1]]);
      if (!BOOL_RESULT) break;
    }
}
static char *descr_leq_fn(int *args, int *ints, Float *dbls) {return(describe_rel_f_args("<=", ints[args[1]], args, ints, dbls, 2));}
static void leq_i2(int *args, int *ints, Float *dbls) {BOOL_RESULT = (INT_ARG_1 <= INT_ARG_2);}
static char *descr_leq_i2(int *args, int *ints, Float *dbls) {return(describe_rel_i_args("<=", 2, args, ints, 1));}
static void leq_in(int *args, int *ints, Float *dbls)
{
  int i, n;
  n = ints[args[1]];
  for (i = 2; i < n - 1; i++)
    {
      BOOL_RESULT = (ints[args[i]] <= ints[args[i + 1]]);
      if (!BOOL_RESULT) break;
    }
}
static char *descr_leq_in(int *args, int *ints, Float *dbls) {return(describe_rel_i_args("<=", ints[args[1]], args, ints, 2));}
static xen_value *less_than_or_equal(ptree *prog, int float_result, xen_value **args, int num_args, int constants, int need_result)
{
  int i;
  if (num_args == 1) return(make_xen_value(R_BOOL, add_int_to_ptree(prog, 1), TRUE));
  if ((constants > 0) && (float_result)) float_rel_constant_args(prog, num_args, args);
  if (constants == num_args)
    {
      for (i = 1; i < num_args; i++)
	if (float_result)
	  {
	    if (prog->dbls[args[i]->addr] > prog->dbls[args[i + 1]->addr])
	      return(make_xen_value(R_BOOL, add_int_to_ptree(prog, 0), TRUE));
	  }
	else
	  {
	    if (prog->ints[args[i]->addr] > prog->ints[args[i + 1]->addr])
	      return(make_xen_value(R_BOOL, add_int_to_ptree(prog, 0), TRUE));
	  }
      return(make_xen_value(R_BOOL, add_int_to_ptree(prog, 1), TRUE));
    }
  float_rel_args(prog, num_args, args, float_result);
  if (float_result)
    {
      if (num_args == 2) package(prog, R_DBL, leq_f2, descr_leq_f2, args, num_args); else
      package(prog, R_DBL, leq_fn, descr_leq_fn, args, num_args);
    }
  else
    {
      if (num_args == 2) package(prog, R_INT, leq_i2, descr_leq_i2, args, num_args); else
      package(prog, R_INT, leq_in, descr_leq_in, args, num_args);
    }
  return(clean_up(num_args, args));
}

/* ---------------- equal (as in "=") ---------------- */

static void equal_f2(int *args, int *ints, Float *dbls) {BOOL_RESULT = (DBL_ARG_1 == DBL_ARG_2);}
static char *descr_equal_f2(int *args, int *ints, Float *dbls) {return(describe_rel_f_args("==", 2, args, ints, dbls, 1));}
static void equal_fn(int *args, int *ints, Float *dbls)
{
  int i, n;
  n = ints[args[1]];
  for (i = 2; i < n - 1; i++)
    {
      BOOL_RESULT = (dbls[args[i]] == dbls[args[i + 1]]);
      if (!BOOL_RESULT) break;
    }
}
static char *descr_equal_fn(int *args, int *ints, Float *dbls) {return(describe_rel_f_args("==", ints[args[1]], args, ints, dbls, 2));}
static void equal_i2(int *args, int *ints, Float *dbls) {BOOL_RESULT = (INT_ARG_1 == INT_ARG_2);}
static char *descr_equal_i2(int *args, int *ints, Float *dbls) {return(describe_rel_i_args("==", 2, args, ints, 1));}
static void equal_in(int *args, int *ints, Float *dbls)
{
  int i, n;
  n = ints[args[1]];
  for (i = 2; i < n - 1; i++)
    {
      BOOL_RESULT = (ints[args[i]] == ints[args[i + 1]]);
      if (!BOOL_RESULT) break;
    }
}
static char *descr_equal_in(int *args, int *ints, Float *dbls) {return(describe_rel_i_args("==", ints[args[1]], args, ints, 2));}
static xen_value *numbers_equal(ptree *prog, int float_result, xen_value **args, int num_args, int constants, int need_result)
{
  int i;
  if (num_args == 1) return(make_xen_value(R_BOOL, add_int_to_ptree(prog, 1), TRUE));
  if ((constants > 0) && (float_result)) float_rel_constant_args(prog, num_args, args);
  if (constants == num_args)
    {
      for (i = 1; i < num_args; i++)
	if (float_result)
	  {
	    if (prog->dbls[args[i]->addr] != prog->dbls[args[i + 1]->addr])
	      return(make_xen_value(R_BOOL, add_int_to_ptree(prog, 0), TRUE));
	  }
	else
	  {
	    if (prog->ints[args[i]->addr] != prog->ints[args[i + 1]->addr])
	      return(make_xen_value(R_BOOL, add_int_to_ptree(prog, 0), TRUE));
	  }
      return(make_xen_value(R_BOOL, add_int_to_ptree(prog, 1), TRUE));
    }
  float_rel_args(prog, num_args, args, float_result);
  if (float_result)
    {
      if (num_args == 2) package(prog, R_DBL, equal_f2, descr_equal_f2, args, num_args); else
      package(prog, R_DBL, equal_fn, descr_equal_fn, args, num_args);
    }
  else
    {
      if (num_args == 2) package(prog, R_INT, equal_i2, descr_equal_i2, args, num_args); else
      package(prog, R_INT, equal_in, descr_equal_in, args, num_args);
    }
  return(clean_up(num_args, args));
}


/* ---------------- not ---------------- */

static void not_b(int *args, int *ints, Float *dbls) {BOOL_RESULT = (!(INT_ARG_1));}
static char *descr_not_b(int *args, int *ints, Float *dbls) {return(mus_format("i%d(%d) = (!(i%d(%d)))", args[0], BOOL_RESULT, args[1], INT_ARG_1));}
static xen_value *not_p(ptree *prog, xen_value **args, int constants)
{
  if (args[1]->type != R_BOOL)
    return(give_up(1, args));
  if (constants == 1)
    return(make_xen_value(R_BOOL, add_int_to_ptree(prog, !(prog->ints[args[1]->addr])), TRUE));
  package(prog, R_BOOL, not_b, descr_not_b, args, 1);
  return(clean_up(1, args));
}


/* ---------------- odd?, even?, zero?, positive?, negative? ---------------- */

static void odd_i(int *args, int *ints, Float *dbls) {BOOL_RESULT = (INT_ARG_1 & 1);}
static char *descr_odd_i(int *args, int *ints, Float *dbls) {return(mus_format("i%d(%d) = odd?(i%d(%d))", args[0], BOOL_RESULT, args[1], INT_ARG_1));}
static xen_value *odd_p(ptree *prog, xen_value **args, int constants)
{
  if (args[1]->type != R_INT)
    return(give_up(1, args));
  if (constants == 1)
    return(make_xen_value(R_BOOL, add_int_to_ptree(prog, prog->ints[args[1]->addr] & 1), TRUE));
  package(prog, R_BOOL, odd_i, descr_odd_i, args, 1);
  return(clean_up(1, args));
}

static void even_i(int *args, int *ints, Float *dbls) {BOOL_RESULT = (!(INT_ARG_1 & 1));}
static char *descr_even_i(int *args, int *ints, Float *dbls) {return(mus_format("i%d(%d) = even?(i%d(%d))", args[0], BOOL_RESULT, args[1], INT_ARG_1));}
static xen_value *even_p(ptree *prog, xen_value **args, int constants)
{
  if (args[1]->type != R_INT)
    return(give_up(1, args));
  if (constants == 1)
    return(make_xen_value(R_BOOL, add_int_to_ptree(prog, (!(prog->ints[args[1]->addr] & 1))), TRUE));
  package(prog, R_BOOL, even_i, descr_even_i, args, 1);
  return(clean_up(1, args));
}

static void zero_i(int *args, int *ints, Float *dbls) {BOOL_RESULT = (INT_ARG_1 == 0);}
static char *descr_zero_i(int *args, int *ints, Float *dbls) {return(mus_format("i%d(%d) = (i%d(%d) == 0)", args[0], BOOL_RESULT, args[1], INT_ARG_1));}
static void zero_f(int *args, int *ints, Float *dbls) {BOOL_RESULT = (DBL_ARG_1 == 0.0);}
static char *descr_zero_f(int *args, int *ints, Float *dbls) {return(mus_format("i%d(%d) = (d%d(%.4f) == 0.0)", args[0], BOOL_RESULT, args[1], DBL_ARG_1));}
static xen_value *zero_p(ptree *prog, xen_value **args, int constants)
{
  if (constants == 1)
    {
      if (args[1]->type == R_INT)
	return(make_xen_value(R_BOOL, add_int_to_ptree(prog, (prog->ints[args[1]->addr] == 0)), TRUE));
      else
      {
	if (args[1]->type == R_DBL)
	  return(make_xen_value(R_BOOL, add_int_to_ptree(prog, (prog->dbls[args[1]->addr] == 0.0)), TRUE));
	else return(NULL);
      }
    }
  if ((args[1]->type != R_INT) && (args[1]->type != R_DBL))
    return(give_up(1, args));
  if (args[1]->type == R_DBL)
    package(prog, R_BOOL, zero_f, descr_zero_f, args, 1);
  else package(prog, R_BOOL, zero_i, descr_zero_i, args, 1);
  return(clean_up(1, args));
}

static void positive_i(int *args, int *ints, Float *dbls) {BOOL_RESULT = (INT_ARG_1 > 0);}
static char *descr_positive_i(int *args, int *ints, Float *dbls) {return(mus_format("i%d(%d) = (i%d(%d) > 0)", args[0], BOOL_RESULT, args[1], INT_ARG_1));}
static void positive_f(int *args, int *ints, Float *dbls) {BOOL_RESULT = (DBL_ARG_1 > 0.0);}
static char *descr_positive_f(int *args, int *ints, Float *dbls) {return(mus_format("i%d(%d) = (d%d(%.4f) > 0.0)", args[0], BOOL_RESULT, args[1], DBL_ARG_1));}
static xen_value *positive_p(ptree *prog, xen_value **args, int constants)
{
  if (constants == 1)
    {
      if (args[1]->type == R_INT)
	return(make_xen_value(R_BOOL, add_int_to_ptree(prog, (prog->ints[args[1]->addr] > 0)), TRUE));
      else
      {
	if (args[1]->type == R_DBL)
	  return(make_xen_value(R_BOOL, add_int_to_ptree(prog, (prog->dbls[args[1]->addr] > 0.0)), TRUE));
	else return(NULL);
      }
    }
  if ((args[1]->type != R_INT) && (args[1]->type != R_DBL))
    return(give_up(1, args));
  if (args[1]->type == R_DBL)
    package(prog, R_BOOL, positive_f, descr_positive_f, args, 1);
  else package(prog, R_BOOL, positive_i, descr_positive_i, args, 1);
  return(clean_up(1, args));
}

static void negative_i(int *args, int *ints, Float *dbls) {BOOL_RESULT = (INT_ARG_1 < 0);}
static char *descr_negative_i(int *args, int *ints, Float *dbls) {return(mus_format("i%d(%d) = (i%d(%d) < 0)", args[0], BOOL_RESULT, args[1], INT_ARG_1));}
static void negative_f(int *args, int *ints, Float *dbls) {BOOL_RESULT = (DBL_ARG_1 < 0.0);}
static char *descr_negative_f(int *args, int *ints, Float *dbls) {return(mus_format("i%d(%d) = (d%d(%.4f) < 0.0)", args[0], BOOL_RESULT, args[1], DBL_ARG_1));}
static xen_value *negative_p(ptree *prog, xen_value **args, int constants)
{
  if (constants == 1)
    {
      if (args[1]->type == R_INT)
	return(make_xen_value(R_BOOL, add_int_to_ptree(prog, (prog->ints[args[1]->addr] < 0)), TRUE));
      else
      {
	if (args[1]->type == R_DBL)
	  return(make_xen_value(R_BOOL, add_int_to_ptree(prog, (prog->dbls[args[1]->addr] < 0.0)), TRUE));
	else return(NULL);
      }
    }
  if ((args[1]->type != R_INT) && (args[1]->type != R_DBL))
    return(give_up(1, args));
  if (args[1]->type == R_DBL)
    package(prog, R_BOOL, negative_f, descr_negative_f, args, 1);
  else package(prog, R_BOOL, negative_i, descr_negative_i, args, 1);
  return(clean_up(1, args));
}



static void single_to_float(ptree *prog, xen_value **args)
{
  xen_value *old_loc;
  old_loc = args[1];
  args[1] = make_xen_value(R_DBL, add_dbl_to_ptree(prog, 0.0), FALSE);
  add_triple_to_ptree(prog, va_make_triple(store_i_f, descr_store_i_f, 2, args[1], old_loc));
  FREE(old_loc);
}

/* ---------------- sin, cos, tan ---------------- */

static void sin_f(int *args, int *ints, Float *dbls) {DBL_RESULT = sin(DBL_ARG_1);}
static char *descr_sin_f(int *args, int *ints, Float *dbls) {return(mus_format("d%d(%.4f) = sin(d%d(%.4f))", args[0], DBL_RESULT, args[1], DBL_ARG_1));}
static xen_value *sin_1(ptree *prog, xen_value **args, int constants)
{
  if (constants == 1)
    {
      if (args[1]->type == R_INT)
	return(make_xen_value(R_DBL, add_dbl_to_ptree(prog, sin((Float)(prog->ints[args[1]->addr]))), TRUE));
      else return(make_xen_value(R_DBL, add_dbl_to_ptree(prog, sin(prog->dbls[args[1]->addr])), TRUE));
    }
  if (args[1]->type == R_INT) single_to_float(prog, args);
  package(prog, R_DBL, sin_f, descr_sin_f, args, 1);
  return(clean_up(1, args));
}

static void cos_f(int *args, int *ints, Float *dbls) {DBL_RESULT = cos(DBL_ARG_1);}
static char *descr_cos_f(int *args, int *ints, Float *dbls) {return(mus_format("d%d(%.4f) = cos(d%d(%.4f))", args[0], DBL_RESULT, args[1], DBL_ARG_1));}
static xen_value *cos_1(ptree *prog, xen_value **args, int constants)
{
  if (constants == 1)
    {
      if (args[1]->type == R_INT)
	return(make_xen_value(R_DBL, add_dbl_to_ptree(prog, cos((Float)(prog->ints[args[1]->addr]))), TRUE));
      else return(make_xen_value(R_DBL, add_dbl_to_ptree(prog, cos(prog->dbls[args[1]->addr])), TRUE));
    }
  if (args[1]->type == R_INT) single_to_float(prog, args);
  package(prog, R_DBL, cos_f, descr_cos_f, args, 1);
  return(clean_up(1, args));
}

static void tan_f(int *args, int *ints, Float *dbls) {DBL_RESULT = tan(DBL_ARG_1);}
static char *descr_tan_f(int *args, int *ints, Float *dbls) {return(mus_format("d%d(%.4f) = tan(d%d(%.4f))", args[0], DBL_RESULT, args[1], DBL_ARG_1));}
static xen_value *tan_1(ptree *prog, xen_value **args, int constants)
{
  if (constants == 1)
    {
      if (args[1]->type == R_INT)
	return(make_xen_value(R_DBL, add_dbl_to_ptree(prog, tan((Float)(prog->ints[args[1]->addr]))), TRUE));
      else return(make_xen_value(R_DBL, add_dbl_to_ptree(prog, tan(prog->dbls[args[1]->addr])), TRUE));
    }
  if (args[1]->type == R_INT) single_to_float(prog, args);
  package(prog, R_DBL, tan_f, descr_tan_f, args, 1);
  return(clean_up(1, args));
}


/* ---------------- asin, acos, sqrt (opt?) ---------------- */

static void asin_f(int *args, int *ints, Float *dbls) {DBL_RESULT = asin(DBL_ARG_1);}
static char *descr_asin_f(int *args, int *ints, Float *dbls) {return(mus_format("d%d(%.4f) = asin(d%d(%.4f))", args[0], DBL_RESULT, args[1], DBL_ARG_1));}
static xen_value *asin_1(ptree *prog, xen_value **args, int constants)
{
  if (constants == 1)
    {
      if (args[1]->type == R_INT)
	return(make_xen_value(R_DBL, add_dbl_to_ptree(prog, asin((Float)(prog->ints[args[1]->addr]))), TRUE));
      else return(make_xen_value(R_DBL, add_dbl_to_ptree(prog, asin(prog->dbls[args[1]->addr])), TRUE));
    }
  if (args[1]->type == R_INT) single_to_float(prog, args);
  package(prog, R_DBL, asin_f, descr_asin_f, args, 1);
  return(clean_up(1, args));
}

static void acos_f(int *args, int *ints, Float *dbls) {DBL_RESULT = acos(DBL_ARG_1);}
static char *descr_acos_f(int *args, int *ints, Float *dbls) {return(mus_format("d%d(%.4f) = acos(d%d(%.4f))", args[0], DBL_RESULT, args[1], DBL_ARG_1));}
static xen_value *acos_1(ptree *prog, xen_value **args, int constants)
{
  if (constants == 1)
    {
      if (args[1]->type == R_INT)
	return(make_xen_value(R_DBL, add_dbl_to_ptree(prog, acos((Float)(prog->ints[args[1]->addr]))), TRUE));
      else return(make_xen_value(R_DBL, add_dbl_to_ptree(prog, acos(prog->dbls[args[1]->addr])), TRUE));
    }
  if (args[1]->type == R_INT) single_to_float(prog, args);
  package(prog, R_DBL, acos_f, descr_acos_f, args, 1);
  return(clean_up(1, args));
}

static void sqrt_f(int *args, int *ints, Float *dbls) {DBL_RESULT = sqrt(DBL_ARG_1);}
static char *descr_sqrt_f(int *args, int *ints, Float *dbls) {return(mus_format("d%d(%.4f) = sqrt(d%d(%.4f))", args[0], DBL_RESULT, args[1], DBL_ARG_1));}
static xen_value *sqrt_1(ptree *prog, xen_value **args, int constants)
{
  if (constants == 1)
    {
      if (args[1]->type == R_INT)
	return(make_xen_value(R_DBL, add_dbl_to_ptree(prog, sqrt((Float)(prog->ints[args[1]->addr]))), TRUE));
      else return(make_xen_value(R_DBL, add_dbl_to_ptree(prog, sqrt(prog->dbls[args[1]->addr])), TRUE));
    }
  if (args[1]->type == R_INT) single_to_float(prog, args);
  package(prog, R_DBL, sqrt_f, descr_sqrt_f, args, 1);
  return(clean_up(1, args));
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

static void round_f(int *args, int *ints, Float *dbls) {DBL_RESULT = f_round(DBL_ARG_1);}
static char *descr_round_f(int *args, int *ints, Float *dbls) {return(mus_format("d%d(%.4f) = round(d%d(%.4f))", args[0], DBL_RESULT, args[1], DBL_ARG_1));}
static xen_value *round_1(ptree *prog, xen_value **args, int constants)
{
  /* (round 1) -> 1.0! */
  if (constants == 1)
    {
      if (args[1]->type == R_INT) 
	return(make_xen_value(R_DBL, add_dbl_to_ptree(prog, (Float)(prog->ints[args[1]->addr])), TRUE));
      else return(make_xen_value(R_DBL, add_dbl_to_ptree(prog, f_round(prog->dbls[args[1]->addr])), TRUE));
    }
  if (args[1]->type == R_INT)
    package(prog, R_DBL, store_i_f, descr_store_i_f, args,1);
  else package(prog, R_DBL, round_f, descr_round_f, args, 1);
  return(clean_up(1, args));
}


/* ---------------- truncate ---------------- */

static Float f_truncate(Float x)
{
  if (x < 0.0)
    return(-floor(-x));
  return(floor(x));
}

static void truncate_f(int *args, int *ints, Float *dbls) {DBL_RESULT = f_truncate(DBL_ARG_1);}
static char *descr_truncate_f(int *args, int *ints, Float *dbls) {return(mus_format("d%d(%.4f) = truncate(d%d(%.4f))", args[0], DBL_RESULT, args[1], DBL_ARG_1));}
static xen_value *truncate_1(ptree *prog, xen_value **args, int constants)
{
  if (constants == 1)
    {
      if (args[1]->type == R_INT) 
	return(make_xen_value(R_DBL, add_dbl_to_ptree(prog, (Float)(prog->ints[args[1]->addr])), TRUE));
      else return(make_xen_value(R_DBL, add_dbl_to_ptree(prog, f_truncate(prog->dbls[args[1]->addr])), TRUE));
    }
  if (args[1]->type == R_INT)
    package(prog, R_DBL, store_i_f, descr_store_i_f, args,1);
  else package(prog, R_DBL, truncate_f, descr_truncate_f, args, 1);
  return(clean_up(1, args));
}

/* ---------------- floor ---------------- */

static void floor_f(int *args, int *ints, Float *dbls) {DBL_RESULT = floor(DBL_ARG_1);}
static char *descr_floor_f(int *args, int *ints, Float *dbls) {return(mus_format("d%d(%.4f) = floor(d%d(%.4f))", args[0], DBL_RESULT, args[1], DBL_ARG_1));}
static xen_value *floor_1(ptree *prog, xen_value **args, int constants)
{
  if (constants == 1)
    {
      if (args[1]->type == R_INT) 
	return(make_xen_value(R_DBL, add_dbl_to_ptree(prog, (Float)(prog->ints[args[1]->addr])), TRUE));
      else return(make_xen_value(R_DBL, add_dbl_to_ptree(prog, floor(prog->dbls[args[1]->addr])), TRUE));
    }
  if (args[1]->type == R_INT)
    package(prog, R_DBL, store_i_f, descr_store_i_f, args,1);
  else package(prog, R_DBL, floor_f, descr_floor_f, args, 1);
  return(clean_up(1, args));
}

/* ---------------- ceiling ---------------- */

static void ceiling_f(int *args, int *ints, Float *dbls) {DBL_RESULT = ceil(DBL_ARG_1);}
static char *descr_ceiling_f(int *args, int *ints, Float *dbls) {return(mus_format("d%d(%.4f) = ceil(d%d(%.4f))", args[0], DBL_RESULT, args[1], DBL_ARG_1));}
static xen_value *ceiling_1(ptree *prog, xen_value **args, int constants)
{
  if (constants == 1)
    {
      if (args[1]->type == R_INT) 
	return(make_xen_value(R_DBL, add_dbl_to_ptree(prog, (Float)(prog->ints[args[1]->addr])), TRUE));
      else return(make_xen_value(R_DBL, add_dbl_to_ptree(prog, ceil(prog->dbls[args[1]->addr])), TRUE));
    }
  if (args[1]->type == R_INT)
    package(prog, R_DBL, store_i_f, descr_store_i_f, args,1);
  else package(prog, R_DBL, ceiling_f, descr_ceiling_f, args, 1);
  return(clean_up(1, args));
}


/* ---------------- log, exp ---------------- */

static void log_f(int *args, int *ints, Float *dbls) {DBL_RESULT = log(DBL_ARG_1);}
static char *descr_log_f(int *args, int *ints, Float *dbls) {return(mus_format("d%d(%.4f) = log(d%d(%.4f))", args[0], DBL_RESULT, args[1], DBL_ARG_1));}
static xen_value *log_1(ptree *prog, xen_value **args, int constants)
{
  if (constants == 1)
    {
      if (args[1]->type == R_INT)
	return(make_xen_value(R_DBL, add_dbl_to_ptree(prog, log((Float)(prog->ints[args[1]->addr]))), TRUE));
      else return(make_xen_value(R_DBL, add_dbl_to_ptree(prog, log(prog->dbls[args[1]->addr])), TRUE));
    }
  if (args[1]->type == R_INT) single_to_float(prog, args);
  package(prog, R_DBL, log_f, descr_log_f, args, 1);
  return(clean_up(1, args));
}

static void exp_f(int *args, int *ints, Float *dbls) {DBL_RESULT = exp(DBL_ARG_1);}
static char *descr_exp_f(int *args, int *ints, Float *dbls) {return(mus_format("d%d(%.4f) = exp(d%d(%.4f))", args[0], DBL_RESULT, args[1], DBL_ARG_1));}
static xen_value *exp_1(ptree *prog, xen_value **args, int constants)
{
  if (constants == 1)
    {
      if (args[1]->type == R_INT)
	return(make_xen_value(R_DBL, add_dbl_to_ptree(prog, exp((Float)(prog->ints[args[1]->addr]))), TRUE));
      else return(make_xen_value(R_DBL, add_dbl_to_ptree(prog, exp(prog->dbls[args[1]->addr])), TRUE));
    }
  if (args[1]->type == R_INT) single_to_float(prog, args);
  package(prog, R_DBL, exp_f, descr_exp_f, args, 1);
  return(clean_up(1, args));
}


/* ---------------- abs ---------------- */

static void abs_f(int *args, int *ints, Float *dbls) {DBL_RESULT = fabs(DBL_ARG_1);}
static char *descr_abs_f(int *args, int *ints, Float *dbls) {return(mus_format("d%d(%.4f) = fabs(d%d(%.4f))", args[0], DBL_RESULT, args[1], DBL_ARG_1));}
static void abs_i(int *args, int *ints, Float *dbls) {DBL_RESULT = abs(DBL_ARG_1);}
static char *descr_abs_i(int *args, int *ints, Float *dbls) {return(mus_format("i%d(%d) = abs(i%d(%d))", args[0], INT_RESULT, args[1], INT_ARG_1));}
static xen_value *abs_1(ptree *prog, xen_value **args, int constants)
{
  if (constants == 1)
    {
      if (args[1]->type == R_INT)
	return(make_xen_value(R_INT, add_int_to_ptree(prog, abs(prog->ints[args[1]->addr])), TRUE));
      else return(make_xen_value(R_DBL, add_dbl_to_ptree(prog, fabs(prog->dbls[args[1]->addr])), TRUE));
    }
  if (args[1]->type == R_INT)
    package(prog, R_INT, abs_i, descr_abs_i, args, 1);
  else package(prog, R_DBL, abs_f, descr_abs_f, args, 1);
  return(clean_up(1, args));
}


static void quit(int *args, int *ints, Float *dbls) {ALL_DONE = TRUE;}
static char *descr_quit(int *args, int *ints, Float *dbls)
{return(copy_string("quit"));}

static xen_value *walk(ptree *prog, XEN form, int need_result)
{
  /* walk form, storing vars, making program entries for operators etc */
  /* fprintf(stderr,"walk %s\n", XEN_TO_C_STRING(XEN_TO_STRING(form))); */

  if (XEN_BOOLEAN_P(form))
    return(make_xen_value(R_BOOL, add_int_to_ptree(prog, (XEN_FALSE_P(form)) ? 0 : 1), TRUE));
  if (XEN_NUMBER_P(form))
    {
      if ((XEN_EXACT_P(form)) && (XEN_INTEGER_P(form)))
	return(make_xen_value(R_INT, add_int_to_ptree(prog, XEN_TO_C_INT(form)), TRUE));
      else return(make_xen_value(R_DBL, add_dbl_to_ptree(prog, XEN_TO_C_DOUBLE(form)), TRUE));
    }
  if (XEN_LIST_P(form))
    {
      XEN function, all_args;
      char *funcname;
      xen_value **args = NULL;
      int i, num_args, float_result = FALSE, constants = 0;
      function = XEN_CAR(form);
      funcname = XEN_TO_C_STRING(XEN_TO_STRING(function));

      if (strcmp(funcname, "lambda") == 0) return(lambda_form(prog, form));
      if (strcmp(funcname, "begin") == 0) return(begin_form(prog, form, need_result));
      if (strcmp(funcname, "if") == 0) return(if_form(prog, form, need_result));
      if (strcmp(funcname, "or") == 0) return(or_form(prog, form));
      if (strcmp(funcname, "and") == 0) return(and_form(prog, form));

      all_args = XEN_CDR(form);
      num_args = XEN_LIST_LENGTH(all_args);
      args = (xen_value **)CALLOC(num_args + 1, sizeof(xen_value *));

      if (num_args > 0)
	{
	  for (i = 0; i < num_args; i++, all_args = XEN_CDR(all_args))
	    {
	      args[i + 1] = walk(prog, XEN_CAR(all_args), TRUE);
	      if (args[i + 1] == NULL)
		return(give_up(i, args));
	      if (args[i + 1]->type == R_DBL) float_result = TRUE;
	      else if (args[i + 1]->type != R_INT) return(give_up(i, args));
	      if (args[i + 1]->constant) constants++;
	    }
	}

      if (strcmp(funcname, "*") == 0) return(multiply(prog, float_result, args, num_args, constants, need_result));
      if (strcmp(funcname, "+") == 0) return(add(prog, float_result, args, num_args, constants, need_result));
      if (strcmp(funcname, "-") == 0) return(subtract(prog, float_result, args, num_args, constants, need_result));
      if (strcmp(funcname, "/") == 0) return(divide(prog, args, num_args, constants, need_result));
      if (strcmp(funcname, ">") == 0) return(greater_than(prog, float_result, args, num_args, constants, need_result));
      if (strcmp(funcname, ">=") == 0) return(greater_than_or_equal(prog, float_result, args, num_args, constants, need_result));
      if (strcmp(funcname, "<") == 0) return(less_than(prog, float_result, args, num_args, constants, need_result));
      if (strcmp(funcname, "<=") == 0) return(less_than_or_equal(prog, float_result, args, num_args, constants, need_result));
      if (strcmp(funcname, "=") == 0) return(numbers_equal(prog, float_result, args, num_args, constants, need_result));


      /* single arg funcs from here */
      if (num_args > 1) return(NULL);
      if (strcmp(funcname, "abs") == 0) return(abs_1(prog, args, constants));
      if (strcmp(funcname, "sin") == 0) return(sin_1(prog, args, constants));
      if (strcmp(funcname, "cos") == 0) return(cos_1(prog, args, constants));
      if (strcmp(funcname, "tan") == 0) return(tan_1(prog, args, constants));
      if (strcmp(funcname, "log") == 0) return(log_1(prog, args, constants));
      if (strcmp(funcname, "exp") == 0) return(exp_1(prog, args, constants));
      if (strcmp(funcname, "asin") == 0) return(asin_1(prog, args, constants));
      if (strcmp(funcname, "acos") == 0) return(acos_1(prog, args, constants));
      if (strcmp(funcname, "sqrt") == 0) return(sqrt_1(prog, args, constants));
      if (strcmp(funcname, "round") == 0) return(round_1(prog, args, constants));
      if (strcmp(funcname, "truncate") == 0) return(truncate_1(prog, args, constants));
      if (strcmp(funcname, "floor") == 0) return(floor_1(prog, args, constants));
      if (strcmp(funcname, "ceiling") == 0) return(ceiling_1(prog, args, constants));
      if (strcmp(funcname, "not") == 0) return(not_p(prog, args, constants));
      if (strcmp(funcname, "odd?") == 0) return(odd_p(prog, args, constants));
      if (strcmp(funcname, "even?") == 0) return(even_p(prog, args, constants));
      if (strcmp(funcname, "zero?") == 0) return(zero_p(prog, args, constants));
      if (strcmp(funcname, "positive?") == 0) return(positive_p(prog, args, constants));
      if (strcmp(funcname, "negative?") == 0) return(negative_p(prog, args, constants));

      /* these are known in advance in this context */
      if (strcmp(funcname, "boolean?") == 0) return(make_xen_value(R_BOOL, add_int_to_ptree(prog, args[1]->type == R_BOOL), TRUE));
      if (strcmp(funcname, "exact?") == 0) return(make_xen_value(R_BOOL, add_int_to_ptree(prog, args[1]->type == R_INT), TRUE));
      if (strcmp(funcname, "integer?") == 0) return(make_xen_value(R_BOOL, add_int_to_ptree(prog, args[1]->type == R_INT), TRUE));
      if (strcmp(funcname, "inexact?") == 0) return(make_xen_value(R_BOOL, add_int_to_ptree(prog, args[1]->type == R_DBL), TRUE));
      if (strcmp(funcname, "real?") == 0) return(make_xen_value(R_BOOL, add_int_to_ptree(prog, args[1]->type == R_DBL), TRUE));
    }
  if (XEN_SYMBOL_P(form))
    {
      XEN val;
      xen_var *var;
      int addr;
      xen_value *v = NULL;
      var = find_var_in_ptree(prog, XEN_SYMBOL_TO_C_STRING(form));
      if (var) return(copy_xen_value(var->v));
      val = XEN_SYMBOL_TO_VALUE(form);

      if (XEN_NUMBER_P(val))
	{
	  if ((XEN_EXACT_P(val)) && (XEN_INTEGER_P(val)))
	    addr = add_var_to_ptree(prog,
				    XEN_SYMBOL_TO_C_STRING(form),
				    v = make_xen_value(R_INT, 
						       add_int_to_ptree(prog, XEN_TO_C_INT(val)), FALSE));
	  else 
	    addr = add_var_to_ptree(prog,
				    XEN_SYMBOL_TO_C_STRING(form),
				    v = make_xen_value(R_DBL, 
						       add_dbl_to_ptree(prog, XEN_TO_C_DOUBLE(val)), FALSE));
	  prog->vars[addr]->global = TRUE;
	  /* TRUE for global = don't allow set that changes type */
	  return(v);
	}
    }
  return(NULL);
}

static XEN g_run(XEN code)
{
  static ptree *prog;
  prog = make_ptree(8);
  prog->result = walk(prog, code, TRUE);
  if (prog->result)
    {
      add_triple_to_ptree(prog, make_triple(quit, descr_quit, NULL, 0));
      return(XEN_WRAP_C_POINTER(prog));
    }
  return(XEN_FALSE);
}

/* this is map-channel with the parse tree support */
#define S_run_channel "run-channel"
static char *run_channel(chan_info *cp, ptree *pt, int beg, int dur, int edpos)
{
  snd_state *ss;
  snd_info *sp;
  file_info *hdr = NULL;
  int j, k, ofd = 0, datumb = 0, temp_file, err = 0;
  MUS_SAMPLE_TYPE **data;
  MUS_SAMPLE_TYPE *idata;
  char *ofile = NULL;
  snd_fd *sf;
  if ((beg < 0) || (dur <= 0)) return(NULL);
  ss = cp->state;
  sp = cp->sound;
  sf = init_sample_read_any(beg, cp, READ_FORWARD, edpos);
  if (sf == NULL) return(mus_format("run-channel: can't read %s[%d] channel data!", sp->short_filename, cp->chan));
  if (dur > MAX_BUFFER_SIZE)
    {
      temp_file = 1; 
      ofile = snd_tempnam(ss);
      hdr = make_temp_header(ofile, SND_SRATE(sp), 1, dur, S_run_channel);
      ofd = open_temp_file(ofile, 1, hdr, ss);
      if (ofd == -1)
	{
	  free_snd_fd(sf); 
	  return(mus_format("can't open run-channel temp file %s: %s\n", ofile, strerror(errno)));
	}
      datumb = mus_data_format_to_bytes_per_sample(hdr->format);
    }
  else temp_file = 0;
  data = (MUS_SAMPLE_TYPE **)MALLOC(sizeof(MUS_SAMPLE_TYPE *));
  data[0] = (MUS_SAMPLE_TYPE *)CALLOC(MAX_BUFFER_SIZE, sizeof(MUS_SAMPLE_TYPE)); 
  idata = data[0];
  j = 0;
  for (k = 0; k < dur; k++)
    {
      pt->dbls[pt->args[0]] = read_sample_to_float(sf);
      eval_ptree(pt);
      idata[j++] = MUS_FLOAT_TO_SAMPLE(pt->dbls[pt->result->addr]);
      if ((temp_file) && (j == MAX_BUFFER_SIZE))
	{
	  err = mus_file_write(ofd, 0, j - 1, 1, data);
	  j = 0;
	  if (err == -1) break;
	}
    }
  if (temp_file)
    {
      if (j > 0) mus_file_write(ofd, 0, j - 1, 1, data);
      close_temp_file(ofd, hdr, dur * datumb, sp);
      hdr = free_file_info(hdr);
      file_change_samples(beg, dur, ofile, cp, 0, DELETE_ME, LOCK_MIXES, S_run_channel, cp->edit_ctr);
      if (ofile) 
	{
	  FREE(ofile); 
	  ofile = NULL;
	}
    }
  else 
    {
      if (dur > 0) 
	change_samples(beg, dur, idata, cp, LOCK_MIXES, S_run_channel, cp->edit_ctr);
    }
  update_graph(cp, NULL); 
  free_snd_fd(sf);
  FREE(data[0]);
  FREE(data);
  return(NULL);
}

XEN g_map_channel(XEN proc, XEN s_beg, XEN s_dur, XEN snd, XEN chn, XEN edpos, XEN org);

static XEN g_run_channel(XEN form, XEN samp_n, XEN samps, XEN snd_n, XEN chn_n, XEN edpos)
{
  #define H_run_channel "(" S_run_channel " form &optional beg dur snd chn edpos)\n\
evaluates form (a lambda form of one arg) on each sample of snd's channel chn starting at beg for dur samples."

  chan_info *cp;
  int beg = 0, dur = 0, pos;
  char *errmsg = NULL;
  XEN str;
  ptree *pt;
  /* "form" here is actually a list: quoted form as list and form evaluated by Guile
   *    if we can't optimize the bare list, we fallback on scm_apply with the evaluated version
   */

  pt = make_ptree(8);
  pt->result = walk(pt, XEN_CAR(form), TRUE);
  if (pt->result)
    {
      triple *trp;
      trp = add_triple_to_ptree(pt, make_triple(quit, descr_quit, NULL, 0));

      ASSERT_SAMPLE_TYPE(S_run_channel, samp_n, XEN_ARG_2);
      ASSERT_SAMPLE_TYPE(S_run_channel, samps, XEN_ARG_3);
      ASSERT_CHANNEL(S_run_channel, snd_n, chn_n, 4);
      cp = get_cp(snd_n, chn_n, S_run_channel);
      pos = to_c_edit_position(cp, edpos, S_run_channel, XEN_ARG_6);
      beg = beg_to_sample(samp_n, S_run_channel);
      dur = dur_to_samples(samps, beg, cp, pos, XEN_ARG_3, S_run_channel);
      if (dur == 0) return(XEN_FALSE);

      errmsg = run_channel(cp, pt, beg, dur, pos);
    }
  else
    {
      fprintf(stderr, "fallback on map-channel\n");
      return(g_map_channel(XEN_CADR(form), samp_n, samps, snd_n, chn_n, edpos, C_TO_XEN_STRING(S_run_channel)));
    }
  if (errmsg)
    {
      str = C_TO_XEN_STRING(errmsg);
      FREE(errmsg);
      mus_misc_error(S_run_channel, NULL, str);
    }
  return(form);
}


void g_init_run(void)
{
  XEN_EVAL_C_STRING("(use-modules (ice-9 optargs))");
  XEN_EVAL_C_STRING("(defmacro* run-channel (form #:rest args) `(apply run-channel-1 (list (list ',form ,form) ,@args)))");
  XEN_DEFINE_PROCEDURE("run", g_run, 1, 0, 0, "run macro...");
  XEN_DEFINE_PROCEDURE("describe-ptree", g_describe_ptree, 1, 0, 0, "");
  XEN_DEFINE_PROCEDURE("run-channel-1", g_run_channel, 1, 5, 0, H_run_channel);
}

#endif
