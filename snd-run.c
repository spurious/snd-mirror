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
 */

#include "snd.h"
#if WITH_RUN

#define XEN_EXACT_P(Arg) XEN_TRUE_P(scm_exact_p(Arg))
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

static char *describe_value(xen_value *v, int *ints, Float *dbls)
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
} xen_var;

static char *describe_variable(xen_var *var, int *ints, Float *dbls)
{
  char *buf, *temp;
  temp = describe_value(var->v, ints, dbls);
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

static void describe_program(ptree *p)
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
      temp = describe_variable(p->vars[i], p->ints, p->dbls);
      fprintf(stderr, temp);
      FREE(temp);
    }
  temp = describe_value(p->result, p->ints, p->dbls);
  fprintf(stderr,"\nresult: %s\n", temp);
  FREE(temp);
}

static XEN g_describe_program(XEN prog)
{
  describe_program((ptree *)XEN_UNWRAP_C_POINTER(prog));
  return(XEN_FALSE);
}

static ptree *make_program(int initial_data_size)
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

static ptree *free_program(ptree *pt)
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

static triple *add_triple_to_program(ptree *pt, triple *trp)
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

static int add_int_to_program(ptree *pt, int value)
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

static int add_dbl_to_program(ptree *pt, Float value)
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

static int add_var_to_program(ptree *pt, char *name, xen_value *v)
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

static xen_var *find_var_in_program(ptree *pt, char *name)
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

static triple *make_triple(void (*function)(int *arg_addrs, int *ints, Float *dbls), xen_value **typed_args, int args)
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
  return(trp);
}

static triple *va_make_triple(void (*function)(int *arg_addrs, int *ints, Float *dbls), int args, ...)
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
  return(trp);
}

static xen_value *walk(ptree *prog, XEN form, int need_result);

/* much kludgery from here on to get this working briefly in Snd to run some realistic timing tests */
/* (map-channel (lambda (y) (* y 2.5))) over storm.snd:               4.7
 * (c-channel test-run 0 (frames) 0 0 0 prog) same data same results: 0.23
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
static void jump_if(int *args, int *ints, Float *dbls) {if (ints[args[1]] != 0) PC += ints[args[0]];}
static void jump_if_not(int *args, int *ints, Float *dbls) {if (ints[args[1]] == 0) PC += ints[args[0]];}

static void store_i(int *args, int *ints, Float *dbls) {INT_RESULT = INT_ARG_1;}
static void store_f(int *args, int *ints, Float *dbls) {DBL_RESULT = DBL_ARG_1;}
static void store_i_f(int *args, int *ints, Float *dbls) {DBL_RESULT = (Float)INT_ARG_1;}
static char *descr_store_i_f(int *args, int *ints, Float *dbls)
{
  return(mus_format("d%d(%.4f) = i%d(%d)", args[0], dbls[args[0]], args[1], ints[args[1]]));
}
static void store_f_i(int *args, int *ints, Float *dbls) {INT_RESULT = (int)DBL_ARG_1;}
static void store_if_f(int *args, int *ints, Float *dbls) {if (ints[args[0]]) dbls[args[1]] = dbls[args[2]]; else dbls[args[1]] = dbls[args[3]];}
static void store_if_i(int *args, int *ints, Float *dbls) {if (ints[args[0]]) ints[args[1]] = ints[args[2]]; else ints[args[1]] = ints[args[3]];}

static xen_value *convert_int_to_dbl(ptree *prog, xen_value *i)
{
  xen_value *val;
  val = make_xen_value(R_DBL, add_dbl_to_program(prog, 0.0), i->constant);
  if (i->constant)
    prog->dbls[val->addr] = (Float)(prog->ints[i->addr]);
  else add_triple_to_program(prog, va_make_triple(store_i_f, 2, val, i));
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
	  add_var_to_program(prog, 
			     XEN_SYMBOL_TO_C_STRING(arg), 
			     v = make_xen_value(R_DBL, add_dbl_to_program(prog, 0.0), FALSE));
	  prog->args[i] = v->addr;
	}
    }
  for (i = 0; i < body_forms; i++, body = XEN_CDR(body))
    v = walk(prog, XEN_CAR(body), (i == (body_forms - 1)));
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
  if_value = walk(prog, XEN_CADR(form), TRUE);                             /* walk selector */
  if (if_value->type != R_BOOL)
    return(NULL);
  jump_to_false = make_xen_value(R_INT, add_int_to_program(prog, 0), FALSE);
  add_triple_to_program(prog, va_make_triple(jump_if_not, 2, jump_to_false, if_value));
  true_result = walk(prog, XEN_CADDR(form), TRUE);                         /* walk true branch */
  if (has_false) 
    {
      false_pc = prog->triple_ctr;
      jump_to_end = make_xen_value(R_INT, add_int_to_program(prog, 0), FALSE);
      add_triple_to_program(prog, va_make_triple(jump, 1, jump_to_end));   /* jump to end (past false) */
    }
  prog->ints[jump_to_false->addr] = prog->triple_ctr - current_pc;         /* fixup jump-to-false addr */
  if (has_false)
    {
      false_result = walk(prog, XEN_CADDDR(form), TRUE);                   /* walk false branch */
      prog->ints[jump_to_end->addr] = prog->triple_ctr - false_pc;         /* fixup jump-past-false addr */
    }
  else 
    {
      if (jump_to_end) FREE(jump_to_end);
      if (jump_to_false) FREE(jump_to_false);
      if (if_value) FREE(if_value);
      return(true_result);
    }
  if (need_result)                                                         /* convert result, if necessary */
    {
      if ((true_result->type == R_INT) && (false_result->type == R_INT))
	result = make_xen_value(R_INT, add_int_to_program(prog, 0), FALSE);
      else
	{
	  result = make_xen_value(R_DBL, add_dbl_to_program(prog, 0.0), FALSE);
	  if (true_result->type == R_INT)
	    true_result = convert_int_to_dbl(prog, true_result);
	  if (false_result->type == R_INT)
	    false_result = convert_int_to_dbl(prog, false_result);
	}
      add_triple_to_program(prog, va_make_triple(store_if_f, 4, jump_to_false[1], result, true_result, false_result));
    }
  else result = make_xen_value(R_BOOL, -1, TRUE); /* need non-null result to indicate success in parse */
  if (true_result) FREE(true_result);
  if (false_result) FREE(false_result);
  if (jump_to_end) FREE(jump_to_end);
  if (jump_to_false) FREE(jump_to_false);
  if (if_value) FREE(if_value);
  return(result);
}

static void package(ptree *prog,
		    int type, 
		    void (*function)(int *arg_addrs, int *ints, Float *dbls),
		    char *(*descr)(int *arg_addrs, int *ints, Float *dbls),
		    xen_value **args,
		    int num_args)
{
  triple *trp;
  args[0] = make_xen_value(type, 
			   (type == R_DBL) ? add_dbl_to_program(prog, 0.0) : add_int_to_program(prog, 0), 
			   FALSE);
  trp = add_triple_to_program(prog, 
			      make_triple(function, args, num_args + 1));
  trp->descr = descr;
}

static char *describe_dbl_args(char *func, int num_args, int *args, Float *dbls)
{
  char *buf, *str;
  int i;
  buf = (char *)CALLOC(256, sizeof(char));
  str = (char *)CALLOC(32, sizeof(char));
  sprintf(buf,"d%d(%.4f) =", args[0], dbls[args[0]]);
  for (i = 1; i < num_args; i++)
    {
      snprintf(str, 32, " d%d(%.4f) %s", args[i], dbls[args[i]], func);
      strcat(buf, str);
    }
  snprintf(str, 32, " d%d(%.4f))", args[num_args], dbls[args[num_args]]);
  strcat(buf, str);
  FREE(str);
  return(buf);
}

static char *describe_int_args(char *func, int num_args, int *args, int *ints)
{
  char *buf, *str;
  int i;
  buf = (char *)CALLOC(256, sizeof(char));
  str = (char *)CALLOC(32, sizeof(char));
  sprintf(buf,"d%d(%d) =", args[0], ints[args[0]]);
  for (i = 1; i < num_args; i++)
    {
      snprintf(str, 32, " d%d(%d) %s", args[i], ints[args[i]], func);
      strcat(buf, str);
    }
  snprintf(str, 32, " d%d(%d))", args[num_args], ints[args[num_args]]);
  strcat(buf, str);
  FREE(str);
  return(buf);
}

/* ---------------- multiply ---------------- */

static void multiply_f2(int *args, int *ints, Float *dbls) {dbls[args[0]] = (DBL_ARG_1 * DBL_ARG_2);}
static char *descr_multiply_f2(int *args, int *ints, Float *dbls) {return(describe_dbl_args("*", 2, args, dbls));}

static void multiply_f3(int *args, int *ints, Float *dbls) {DBL_RESULT = (DBL_ARG_1 * DBL_ARG_2 * DBL_ARG_3);}
static char *descr_multiply_f3(int *args, int *ints, Float *dbls) {return(describe_dbl_args("*", 3, args, dbls));}

static void multiply_f4(int *args, int *ints, Float *dbls) {DBL_RESULT = (DBL_ARG_1 * DBL_ARG_2 * DBL_ARG_3 * DBL_ARG_4);}
static char *descr_multiply_f4(int *args, int *ints, Float *dbls) {return(describe_dbl_args("*", 4, args, dbls));}

static void multiply_fn(int *args, int *ints, Float *dbls) 
{
  int i, n;
  n = ints[args[1]];
  DBL_RESULT = dbls[args[2]];
  for (i = 1; i < n; i++) DBL_RESULT *= dbls[args[i + 2]];
}
static char *descr_multiply_fn(int *args, int *ints, Float *dbls) {return(describe_dbl_args("*", ints[args[1]], args, dbls));}

static void multiply_i2(int *args, int *ints, Float *dbls) {INT_RESULT = (INT_ARG_1 * INT_ARG_2);}
static char *descr_multiply_i2(int *args, int *ints, Float *dbls) {return(describe_int_args("*", 2, args, ints));}

static void multiply_i3(int *args, int *ints, Float *dbls) {INT_RESULT = (INT_ARG_1 * INT_ARG_2 * INT_ARG_3);}
static char *descr_multiply_i3(int *args, int *ints, Float *dbls) {return(describe_int_args("*", 3, args, ints));}

static void multiply_i4(int *args, int *ints, Float *dbls) {INT_RESULT = (INT_ARG_1 * INT_ARG_2 * INT_ARG_3 * INT_ARG_4);}
static char *descr_multiply_i4(int *args, int *ints, Float *dbls) {return(describe_int_args("*", 4, args, ints));}

static void multiply_in(int *args, int *ints, Float *dbls)
{
  int i, n;
  n = ints[args[1]];
  INT_RESULT = ints[args[2]];
  for (i = 1; i < n; i++) INT_RESULT *= ints[args[i + 2]];
}

static char *descr_multiply_in(int *args, int *ints, Float *dbls) {return(describe_int_args("*", ints[args[1]], args, ints));}

static void dispatch_multiply(ptree *prog, int float_result, xen_value **args, int num_args)
{
  if (float_result)
    {
      if (num_args == 2) package(prog, R_DBL, multiply_f2, descr_multiply_f2, args, num_args); else
      if (num_args == 3) package(prog, R_DBL, multiply_f3, descr_multiply_f3, args, num_args); else
      if (num_args == 4) package(prog, R_DBL, multiply_f4, descr_multiply_f4, args, num_args); else
      package(prog, R_DBL, multiply_fn, descr_multiply_fn, args, num_args);
    }
  else
    {
      if (num_args == 2) package(prog, R_INT, multiply_i2, descr_multiply_i2, args, num_args); else
      if (num_args == 3) package(prog, R_INT, multiply_i3, descr_multiply_i3, args, num_args); else
      if (num_args == 4) package(prog, R_INT, multiply_i4, descr_multiply_i4, args, num_args); else
      package(prog, R_INT, multiply_in, descr_multiply_in, args, num_args);
    }
}


static void add_f2(int *args, int *ints, Float *dbls) {DBL_RESULT = (DBL_ARG_1 + DBL_ARG_2);}

static void gt_f2(int *args, int *ints, Float *dbls) {BOOL_RESULT = (DBL_ARG_1 > DBL_ARG_2);}
static char *descr_gt_f2(int *args, int *ints, Float *dbls) 
{
  return(mus_format("i%d(%d) = d%d(%.4f) > d%d(%.4f)", args[0], BOOL_RESULT, args[1], DBL_ARG_1, args[2], DBL_ARG_2));
}

static void quit(int *args, int *ints, Float *dbls) {ALL_DONE = TRUE;}
static char *descr_quit(int *args, int *ints, Float *dbls) {return(copy_string("quit"));}
/* (describe-program (run '(> (* 1.0 2.5) 3.0))) */

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

static xen_value *walk(ptree *prog, XEN form, int need_result)
{
  /* walk form, storing vars, making program entries for operators etc */

  xen_value *v = NULL;

  /* fprintf(stderr,"walk %s\n", XEN_TO_C_STRING(XEN_TO_STRING(form))); */

  if (XEN_BOOLEAN_P(form))
    return(make_xen_value(R_BOOL, add_int_to_program(prog, (XEN_FALSE_P(form)) ? 0 : 1), TRUE));
  if (XEN_NUMBER_P(form))
    {
      if ((XEN_EXACT_P(form)) && (XEN_INTEGER_P(form)))
	return(make_xen_value(R_INT, add_int_to_program(prog, XEN_TO_C_INT(form)), TRUE));
      else return(make_xen_value(R_DBL, add_dbl_to_program(prog, XEN_TO_C_DOUBLE(form)), TRUE));
    }
  if (XEN_LIST_P(form))
    {
      XEN function, all_args;
      char *funcname;
      triple *trp;
      xen_value **args = NULL;
      int i, num_args, float_result = FALSE, constants = 0;
      function = XEN_CAR(form);
      funcname = XEN_TO_C_STRING(XEN_TO_STRING(function));
      if (strcmp(funcname, "lambda") == 0) return(lambda_form(prog, form));
      if (strcmp(funcname, "if") == 0) return(if_form(prog, form, need_result));
	  
      all_args = XEN_CDR(form);
      num_args = XEN_LIST_LENGTH(all_args);
      /* fprintf(stderr,"[function %s with %d args] ", XEN_TO_C_STRING(XEN_TO_STRING(function)), num_args); */
      /* if let, get closure, and lambda args */

      args = (xen_value **)CALLOC(num_args + 1, sizeof(xen_value *));
      if (num_args > 0)
	{
	  for (i = 0; i < num_args; i++, all_args = XEN_CDR(all_args))
	    {
	      args[i + 1] = walk(prog, XEN_CAR(all_args), TRUE);
	      if (args[i + 1] == NULL)
		return(give_up(i, args));
	      if (args[i + 1]->type == R_DBL) float_result = TRUE;
	      if (args[i + 1]->constant) constants++;
	    }
	}

      /* if float_result, scan for int args, change to dbl if constant, else
       *   add conversion triple (in either case, fixup arg for new addr)
       */
      if (strcmp(funcname, "*") == 0) 
	{
	  /* TODO: if not need_result just clean up and return dummy */
	  int iscl = 1, cons_loc = 0;
	  Float fscl = 1.0;
	  xen_value *old_loc;
	  int j;
	  if (num_args == 1) return(args[1]);
	  if (!need_result) return(clean_up(num_args + 1, args));
	  if (constants > 1)
	    /* collapse constants (to dbl if needed) */
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
		    args[cons_loc] = make_xen_value(R_DBL, add_dbl_to_program(prog, fscl * iscl), TRUE);
		  else args[cons_loc] = make_xen_value(R_INT, add_int_to_program(prog, iscl), TRUE);
		}
	      if (constants == num_args) return(args[cons_loc]);
	    }
	  for (i = 1, j = 1; i <= num_args; i++)
	    if (args[i])
	      {
		if ((float_result) && (args[i]->type == R_INT))
		  {
		    old_loc = args[i];
		    args[i] = NULL;
		    args[j] = make_xen_value(R_DBL, add_dbl_to_program(prog, 0.0), FALSE);
		    trp = add_triple_to_program(prog, va_make_triple(store_i_f, 2, args[j], old_loc));
		    trp->descr = descr_store_i_f;
		    FREE(old_loc);
		    j++;
		  }
		else args[j++] = args[i];
	      }
	  dispatch_multiply(prog, float_result, args, j - 1);
	}
      else
	{
	  if (strcmp(funcname, ">") == 0)
	    {
	      args[0] = make_xen_value(R_BOOL, add_int_to_program(prog, FALSE), FALSE);
	      trp = add_triple_to_program(prog, make_triple(gt_f2, args, num_args + 1));
	      trp->descr = descr_gt_f2;
	    }
	}

      for (i = 1; i < num_args; i++) FREE(args[i]);
      v = args[0];
      /* fprintf(stderr,"returning value at %d\n", v->addr); */
      FREE(args);
      return(v);
    }
  if (XEN_SYMBOL_P(form))
    {
      xen_var *var;
      var = find_var_in_program(prog, XEN_SYMBOL_TO_C_STRING(form));
      if (var) return(copy_xen_value(var->v));
    }
  return(NULL);
}

static XEN g_run(XEN code)
{
  triple *trp;
  static   ptree *prog;

  prog = make_program(8);

  /* add args (i.e. (lambda ...) and info about types if known */

  prog->result = walk(prog, code, TRUE);
  if (prog->result)
    {
      /* v is output */
      trp = add_triple_to_program(prog, make_triple(quit, NULL, 0));
      trp->descr = descr_quit;

      /* also return result loc somehow */
      return(XEN_WRAP_C_POINTER(prog));
      
    }
  return(XEN_FALSE);
}

static float test_run(float b, void *environ) 
{
  ptree *pt = (ptree *)environ;
  /* set args (i.e. b) for this run */
  eval_ptree(pt);
  return(0.0); /* output addr */
}

static XEN g_get_test_run(void) {return(XEN_WRAP_C_POINTER(test_run));}



void g_init_run(void)
{
  XEN_DEFINE_PROCEDURE("run", g_run, 1, 0, 0, "run macro...");
  XEN_DEFINE_PROCEDURE("get-test-run", g_get_test_run, 0, 0, 0, "no hope"); /* try c-channel for timing info */
  XEN_DEFINE_PROCEDURE("describe-program", g_describe_program, 1, 0, 0, "");
}

#endif
