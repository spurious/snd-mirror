/* run macro
 *   initial timing tests indicate that this is 20 times as fast as Guile.
 *   Rather than write/compile (via gcc) a C source file, as in CLM, this
 *   produces the intermediate "triples" on the fly, packaging them into
 *   a "program" (a list of triples), and precomputing all function, program,
 *   and data addresses (as if a loader were at work); a kind of byte-compiler
 *   without the bytes or the compiler (that is, the function search is
 *   done at read time, not eval time).  This can work in general except
 *   for recursion, call/cc where the continuation is actually saved,
 *   and variables that have more than 1 type.  I could probably handle
 *   the latter through some kludgery, but the whole point is to run fast,
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

typedef struct {
  void (*function)(int *arg_addrs, int *ints, Float *dbls);
  int *args;
} triple;

static triple *free_triple(triple *trp)
{
  if (trp->args) FREE(trp->args);
  FREE(trp);
  return(NULL);
}

typedef struct {
  triple **program;
  int *ints;
  Float *dbls;
  int program_size, ints_size, dbls_size, triple_ctr, int_ctr, dbl_ctr;
} ptree;

static void describe_program(ptree *p)
{
  
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
  return(pt);
}

static ptree *free_program(ptree *pt)
{
  int i;
  if (pt)
    {
      if (pt->ints) FREE(pt->ints);
      if (pt->dbls) FREE(pt->dbls);
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

#define PC ints[0]
#define ALL_DONE ints[1]

static void eval_ptree(ptree *prog)
{
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

static xen_value *walk(ptree *prog, XEN form);
enum {X_INT, X_DBL};

/* much kludgery from here on to get this working briefly in Snd to run some realistic timing tests */
/* (map-channel (lambda (y) (* y 2.5))) over storm.snd:               4.7
 * (c-channel test-run 0 (frames) 0 0 0 prog) same data same results: 0.23
 */

static void jump(int *args, int *ints, Float *dbls) {PC += ints[args[0]];}
static void jump_if(int *args, int *ints, Float *dbls) {if (ints[args[1]] != 0) PC += ints[args[0]];}
static void jump_if_not(int *args, int *ints, Float *dbls) {if (ints[args[1]] == 0) PC += ints[args[0]];}

static void store_i(int *args, int *ints, Float *dbls) {ints[args[0]] = ints[args[1]];}
static void store_f(int *args, int *ints, Float *dbls) {dbls[args[0]] = dbls[args[1]];}
static void store_i_f(int *args, int *ints, Float *dbls) {dbls[args[0]] = (Float)ints[args[1]];}
static void store_f_i(int *args, int *ints, Float *dbls) {ints[args[0]] = (int)dbls[args[1]];}
static void store_if_f(int *args, int *ints, Float *dbls) {if (ints[args[0]]) dbls[args[1]] = dbls[args[2]]; else dbls[args[1]] = dbls[args[3]];}
static void store_if_i(int *args, int *ints, Float *dbls) {if (ints[args[0]]) ints[args[1]] = ints[args[2]]; else ints[args[1]] = ints[args[3]];}

static xen_value *convert_int_to_dbl(ptree *prog, xen_value *i)
{
  xen_value *val;
  val = make_xen_value(X_DBL, add_dbl_to_program(prog, 0.0), i->constant);
  if (i->constant)
    prog->dbls[val->addr] = (Float)(prog->ints[i->addr]);
  else 
    {
      xen_value *convert[2];
      convert[0] = val;
      convert[1] = i;
      add_triple_to_program(prog, make_triple(store_i_f, convert, 2));
    }
  return(val);
}

static xen_value *if_form(ptree *prog, XEN form, int need_result)
{
  /* form: (if selector true [false]) */
  xen_value *result = NULL, *true_result = NULL, *false_result = NULL;
  xen_value *jump_to_false[2];
  xen_value *jump_to_end = NULL;
  int current_pc, false_pc = 0, has_false;
  current_pc = prog->triple_ctr; /* the selector actually */
  has_false = (XEN_LIST_LENGTH(form) == 4);
  jump_to_false[0] = make_xen_value(X_INT, add_int_to_program(prog, 0), FALSE);
  jump_to_false[1] = walk(prog, XEN_CADR(form));                           /* walk selector */
  add_triple_to_program(prog, make_triple(jump_if_not, jump_to_false, 2)); /* jump if selector false */
  true_result = walk(prog, XEN_CADDR(form));                               /* walk true branch */
  if (has_false) 
    {
      false_pc = prog->triple_ctr;
      jump_to_end = make_xen_value(X_INT, add_int_to_program(prog, 0), FALSE);
      add_triple_to_program(prog, make_triple(jump, &jump_to_end, 1));     /* jump to end (past false) */
    }
  prog->ints[jump_to_false[0]->addr] = prog->triple_ctr - current_pc;      /* fixup jump-to-false addr */
  if (has_false)
    {
      false_result = walk(prog, XEN_CADDDR(form));                         /* walk false branch */
      prog->ints[jump_to_end->addr] = prog->triple_ctr - false_pc;         /* fixup jump-past-false addr */
    }
  else 
    {
      if (jump_to_end) FREE(jump_to_end);
      if (jump_to_false[0]) FREE(jump_to_false[0]);
      if (jump_to_false[1]) FREE(jump_to_false[1]);
      return(true_result);
    }
  if (need_result)                                                         /* convert result, if necessary */
    {
      xen_value *store_if_args[4];
      store_if_args[0] = jump_to_false[1];
      store_if_args[2] = true_result;
      store_if_args[3] = false_result;
      if ((true_result->type == X_INT) && (false_result->type == X_INT))
	{
	  result = make_xen_value(X_INT, add_int_to_program(prog, 0), FALSE);
	  store_if_args[1] = result;
	  add_triple_to_program(prog, make_triple(store_if_i, store_if_args, 4));
	}
      else
	{
	  result = make_xen_value(X_DBL, add_dbl_to_program(prog, 0.0), FALSE);
	  store_if_args[1] = result;
	  if (true_result->type == X_INT)
	    true_result = convert_int_to_dbl(prog, true_result);
	  if (false_result->type == X_INT)
	    false_result = convert_int_to_dbl(prog, false_result);
	  add_triple_to_program(prog, make_triple(store_if_f, store_if_args, 4));
	}
    }
  if (true_result) FREE(true_result);
  if (false_result) FREE(false_result);
  if (jump_to_end) FREE(jump_to_end);
  if (jump_to_false[0]) FREE(jump_to_false[0]);
  if (jump_to_false[1]) FREE(jump_to_false[1]);
  return(result);
}

static void multiply_f2(int *args, int *ints, Float *dbls) {dbls[args[0]] = (dbls[args[1]] * dbls[args[2]]);}
  /* precomputing the addresses here did not speed up the code, and it's unreadable code! */
static void multiply_f3(int *args, int *ints, Float *dbls) {dbls[args[0]] = (dbls[args[1]] * dbls[args[2]] * dbls[args[3]]);}
static void multiply_f4(int *args, int *ints, Float *dbls) {dbls[args[0]] = (dbls[args[1]] * dbls[args[2]] * dbls[args[3]] * dbls[args[4]]);}
static void multiply_fn(int *args, int *ints, Float *dbls) 
{
  int i, n;
  n = ints[args[1]];
  dbls[args[0]] = dbls[args[2]];
  for (i = 1; i < n; i++) dbls[args[0]] *= dbls[args[i + 2]];
}
static void multiply_i2(int *args, int *ints, Float *dbls) {ints[args[0]] = (ints[args[1]] * ints[args[2]]);}

static void add_2(int *args, int *ints, Float *dbls) {dbls[args[0]] = (dbls[args[1]] + dbls[args[2]]);}
static void gtf_2(int *args, int *ints, Float *dbls) {ints[args[0]] = (dbls[args[1]] > dbls[args[2]]);}
static void quit(int *args, int *ints, Float *dbls) {ALL_DONE = TRUE;}


static int input_addr = 0;
static xen_value *walk(ptree *prog, XEN form)
{
  xen_value *v = NULL;

  /* fprintf(stderr,"walk %s\n", XEN_TO_C_STRING(XEN_TO_STRING(form))); */

  if (XEN_NUMBER_P(form))
    {
      if (XEN_INTEGER_P(form))
	return(make_xen_value(X_INT, add_int_to_program(prog, XEN_TO_C_INT(form)), TRUE));
      else return(make_xen_value(X_DBL, add_dbl_to_program(prog, XEN_TO_C_DOUBLE(form)), TRUE));
    }
  else
    {
      if (XEN_LIST_P(form))
	{
	  XEN function, all_args;
	  xen_value **args = NULL;
	  int i, num_args, float_result = FALSE;
	  function = XEN_CAR(form);
	  all_args = XEN_CDR(form);
	  num_args = XEN_LIST_LENGTH(all_args);

	  /* fprintf(stderr,"[function %s with %d args] ", XEN_TO_C_STRING(XEN_TO_STRING(function)), num_args); */

	  args = (xen_value **)CALLOC(num_args + 1, sizeof(xen_value *));
	  if (num_args > 0)
	    {
	      for (i = 0; i < num_args; i++, all_args = XEN_CDR(all_args))
		{
		  args[i + 1] = walk(prog, XEN_CAR(all_args));
		  if (args[i + 1]->type == X_DBL) float_result = TRUE;
		}
	    }

	  /* if float_result, scan for int args, change to dbl if constant, else
	   *   add conversion triple (in either case, fixup arg for new addr)
	   */

	  if (strcmp(XEN_TO_C_STRING(XEN_TO_STRING(function)), "*") == 0)
	    {

	      /* choose func and result type based on argn and float_result */

	      args[0] = make_xen_value(X_DBL, add_dbl_to_program(prog, 0.0), FALSE);
	      add_triple_to_program(prog, make_triple(multiply_f2, args, num_args + 1));
	      for (i = 1; i < num_args; i++) FREE(args[i]);
	      v = args[0];
	      /* fprintf(stderr,"returning value at %d\n", v->addr); */
	      FREE(args);
	      return(v);
	    }
	}
      else
	{
	  /* a symbol for now */
	  v = make_xen_value(X_DBL, add_dbl_to_program(prog, 0.0), FALSE);
	  input_addr = v->addr;
	}
    }
  return(v);
}

static  xen_value *v;
static   ptree *prog;
static int output_addr = 0;

static XEN g_run(XEN code)
{
  prog = make_program(8);

  /* add args (i.e. (lambda ...) and info about types if known */

  v = walk(prog, code);
  output_addr = v->addr;
  add_triple_to_program(prog, make_triple(quit, NULL, 0));

  /* also return result loc somehow */
  return(XEN_WRAP_C_POINTER(prog));

  eval_ptree(prog);
  return(C_TO_XEN_DOUBLE(prog->dbls[v->addr]));
}

static float test_run(float b, void *environ) 
{
  ptree *pt = (ptree *)environ;
  pt->dbls[input_addr] = b;
  eval_ptree(pt);
  return(prog->dbls[output_addr]);
}

static XEN g_get_test_run(void) {return(XEN_WRAP_C_POINTER(test_run));}



void g_init_run(void)
{
  XEN_DEFINE_PROCEDURE("run", g_run, 1, 0, 0, "run macro...");
  XEN_DEFINE_PROCEDURE("get-test-run", g_get_test_run, 0, 0, 0, "no hope"); /* try c-channel for timing info */
}

#endif
