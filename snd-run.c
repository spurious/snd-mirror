/* run macro
 *   initial timing tests indicate that this is 10 times as fast as Guile.
 *   Rather than write/compile (via gcc) a C source file, as in CLM, this
 *   produces the intermediate "triples" on the fly, packaging them into
 *   a "program" (a list of triples), and precomputing all function, program,
 *   and data addresses (as if a loader were at work); a kind of byte-compiler
 *   without the bytes or the compiler (that is, the function search is
 *   done at read time, not eval time).  This can work in general except
 *   for recursion, call/cc where the continuation is actually saved,
 *   and variables that have more than 1 type (not to mention delay, 
 *   define-syntax etc).  I could handle run-time types through 
 *   some kludgery, but the whole point is to run fast in a DSP context.
 *
 * The evaluator is eval_ptree.  The code walker is walk.  A program
 *   is a list of triples. Each triple has a function pointer and a
 *   pointer to addresses of arguments.  There are two special addresses:
 *   the program counter (PC) and the termination flag (ALL_DONE).
 *
 *
 * exported:
 *   void *form_to_ptree(XEN code)
 *     parse code, returning pointer to tree (a list) or null if code has something we can't handle
 *     void *form_to_ptree_1f2f(XEN code) -- adds type check that result is Float
 *     void *form_to_ptree_1f2b(XEN code) -- adds type check that result is boolean
 *   Float evaluate_ptree_1f2f(void *tree, Float arg)
 *     evaluate ptree passing it the single Float arg, returning a Float result
 *   Float evaluate_ptree_1f2b(void *tree, Float arg)
 *     evaluate ptree passing it the single Float arg, returning a boolean result
 *   void *free_ptree(void *pt)
 *     release resources allocated to ptree
 *
 *
 * currently handled, at least partially:
 *
 *   types: float int boolean [vct]
 *
 *   lambda [assuming float arg(s) for now]["declare" for types?]
 *   call-with-current-continuation [as goto with result value][this could fully implemented]
 *   if begin or and not let let* set!
 *   * + - / > < >= <= = max min
 *   sin cos tan abs log exp expt (no complex...)
 *   acos asin atan sqrt (assuming no complex, so args are assumed to be in float result range -- on optimization switch?)
 *   boolean? exact? inexact? integer? real? number? quote
 *   odd? even? zero? positive? negative? eq? eqv? equal?
 *   round truncate floor ceiling exact->inexact inexact->exact
 *   gcd lcm logand logior logxor lognot ash modulo remainder quotient
 *   vct?
 *
 * tests in snd-test.scm, test 22
 *
 *
 * todo: cond[not => of course] case[not symbols] do rationalize[in slib] defined?
 *       vct clm sndlib
 *         R_VCT R_CLM R_SND -- optargs are main problem here (and repeated code -- everything is wrapped in XEN procs)
 *       snd procs? (need ffi or something to avoid repeating opt args handles etc) snd-print needed at least
 *       define (proc? -- each could be an independent ptree)
 *       strings/chars
 *         R_CHR is easy -- just use ints
 *         R_STR perhaps via table of strings with refcounts
 *       vectors of floats
 *         load into local vct and use that, unloading at end
 *       lists? -- this opens a huge can of worms
 *       tie into Snd on optimization switch
 *       rest of snd-test 22, and overall test
 *       ptree as fragment edit op
 *       lambda with other types?
 *         (declare...)
 *       run-time var typing?
 *       callcc with copy/restore of ptree state
 *         R_CONT type created only if continuation proc returns itself,
 *         contains copy of ints/dbls
 *         if appears in func position, restore copied state (and gc upon quit)
 *       catch/throw (or at least throw as a way to get out)
 *         could be handled like continuation is currently
 *       complex, ratio
 *       named let?
 *       display
 *       generalized set!
 */

#include "snd.h"
#if HAVE_GUILE
#if WITH_RUN
#include "vct.h"

#define XEN_EXACT_P(Arg) XEN_TRUE_P(scm_exact_p(Arg))
#define XEN_SYMBOL_TO_VALUE(a) XEN_VARIABLE_REF(scm_sym2var(a, scm_current_module_lookup_closure(), XEN_TRUE))
#define XEN_SYMBOL_NAME_SET_VALUE(a, b) XEN_VARIABLE_SET(scm_sym2var(scm_str2symbol(a), scm_current_module_lookup_closure(), XEN_TRUE), b)

enum {R_INT, R_FLOAT, R_BOOL, R_VCT, R_READER, R_CLM, R_CHAR, R_STRING, R_VECTOR};

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
  switch (v->type)
    {
    case R_BOOL:  snprintf(buf, 32, "i%d(%s)", v->addr, (ints[v->addr] == 0) ? "#f" : "#t"); break;
    case R_INT:   snprintf(buf, 32, "i%d(%d)", v->addr, ints[v->addr]); break;
    case R_FLOAT: snprintf(buf, 32, "d%d(%.4f)", v->addr, dbls[v->addr]); break;
    case R_VCT:   FREE(buf); buf = vct_to_string((vct *)(ints[v->addr])); break;
    }
  return(buf);
}

typedef struct {
  char *name;
  xen_value *v;
  int global, unclean;
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
  char *name;
  xen_value *result, *jump;
  int loc;
} continuation;

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
  continuation **gotos;
  int goto_ctr, gotos_size;
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
  fprintf(stderr,"\n");
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

static int got_lambda = 0; /* a temporary kludge?? */

static ptree *make_ptree(int initial_data_size)
{
  ptree *pt;

  got_lambda = 0;

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

void *free_ptree(void *upt)
{
  int i;
  ptree *pt = (ptree *)upt;
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
      if (pt->args) FREE(pt->args);
      if (pt->gotos) FREE(pt->gotos);
      if (pt->result) FREE(pt->result);
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
  var->global = FALSE;
  var->unclean = FALSE;
  pt->vars[pt->var_ctr++] = var;
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
  return(NULL);
}

/* (let ((count 1)) (scan-channel (lambda (y) (set! count 1) #f))) */

static xen_value *add_global_var_to_ptree(ptree *prog, XEN form)
{
  if (XEN_SYMBOL_P(form))
    {
      XEN val;
      xen_var *var;
      int addr = 0;
      xen_value *v = NULL;
      char varname[256];
      snprintf(varname, 256, "%s", XEN_SYMBOL_TO_C_STRING(form));
      var = find_var_in_ptree(prog, varname);
      if (var) return(copy_xen_value(var->v));
      val = XEN_SYMBOL_TO_VALUE(form);

      if (XEN_NUMBER_P(val))
	{
	  if ((XEN_EXACT_P(val)) && (XEN_INTEGER_P(val)))
	    addr = add_var_to_ptree(prog,
				    varname,
				    v = make_xen_value(R_INT, 
						       add_int_to_ptree(prog, 
									XEN_TO_C_INT(val)), 
						       FALSE));
	  else 
	    addr = add_var_to_ptree(prog,
				    varname,
				    v = make_xen_value(R_FLOAT, 
						       add_dbl_to_ptree(prog, 
									XEN_TO_C_DOUBLE(val)), 
						       FALSE));
	}
      else
	{
	  if (XEN_BOOLEAN_P(val))
	    {
	      addr = add_var_to_ptree(prog,
				      varname,
				      v = make_xen_value(R_BOOL, 
							 add_int_to_ptree(prog, 
									  XEN_TO_C_BOOLEAN(val)), 
							 FALSE));
	    }
	  else
	    {
	      if (VCT_P(val))
		{
		  addr = add_var_to_ptree(prog,
					  varname,
					  v = make_xen_value(R_VCT, 
							     add_int_to_ptree(prog, 
									      (int)(get_vct(val))),
							     FALSE));
		}
	    }
	}
      if (v)
	{
	  prog->vars[addr]->global = TRUE;
	  /* TRUE for global = don't allow set that changes type */
	  return(v);
	}
    }
  return(NULL);
}

static continuation *add_goto_to_ptree(ptree *pt, char *name)
{
  continuation *c;
  int old_size, i;
  c = (continuation *)CALLOC(1, sizeof(continuation));
  c->name = (char *)CALLOC(256, sizeof(char));
  snprintf(c->name, 256, "%s", name);
  c->jump = make_xen_value(R_INT, add_int_to_ptree(pt, 0), FALSE);
  c->result = NULL;
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
  return(c);
}

static continuation *find_goto_in_ptree(ptree *pt, char *name)
{
  int i;
  for (i = pt->goto_ctr - 1; i >= 0; i--)
    if ((pt->gotos[i]) &&
	(strcmp(pt->gotos[i]->name, name) == 0))
      return(pt->gotos[i]);
  return(NULL);
}

static continuation *free_goto(ptree *pt, continuation *c)
{
  if (c->name) FREE(c->name);
  if (c->jump) FREE(c->jump);
  if (c->result) FREE(c->result);
  pt->gotos[c->loc] = NULL;
  pt->goto_ctr = c->loc;
  FREE(c);
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
  int i;
  xen_var *var;
  ints = prog->ints;
  dbls = prog->dbls;
  PC = 0;
  ALL_DONE = FALSE;
  /* evaluate the parse tree */
  while (!ALL_DONE)
    {
      curfunc = prog->program[PC++];
      (*(curfunc->function))(curfunc->args, ints, dbls);
    }

  /* if var->global, reflect new value into outer level version of the variable upon quit */
  for (i = 0; i < prog->var_ctr; i++)
    {
      var = prog->vars[i];
      if ((var) &&
	  (var->global) &&
	  (var->unclean))
	{
	  switch (var->v->type)
	    {
	    case R_FLOAT: XEN_SYMBOL_NAME_SET_VALUE(var->name, C_TO_XEN_DOUBLE(prog->dbls[var->v->addr])); break;
	    case R_INT:   XEN_SYMBOL_NAME_SET_VALUE(var->name, C_TO_XEN_INT(prog->ints[var->v->addr])); break;
	    case R_BOOL:  XEN_SYMBOL_NAME_SET_VALUE(var->name, C_TO_XEN_BOOLEAN(prog->ints[var->v->addr])); break;
	    }
	}
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
#define VCT_ARG_1 (vct *)(ints[args[1]])
#define VCT_ARG_2 (vct *)(ints[args[2]])

static void jump(int *args, int *ints, Float *dbls) {PC += ints[args[0]];}
static char *descr_jump(int *args, int *ints, Float *dbls) {return(mus_format("jump i%d(%d)", args[0], INT_RESULT));}

static void jump_abs(int *args, int *ints, Float *dbls) {PC = ints[args[0]];}
static char *descr_jump_abs(int *args, int *ints, Float *dbls) {return(mus_format("goto i%d(%d)", args[0], INT_RESULT));}

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
  val = make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, 0.0), i->constant);
  if (i->constant)
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
  if (i->constant)
    prog->ints[v->addr] = (int)val;
  else add_triple_to_ptree(prog, va_make_triple(store_f_i, descr_store_f_i, 2, v, i));
  return(v);
}

static xen_value *lambda_form(ptree *prog, XEN form)
{
  /* (lambda (args...) | args etc followed by forms */
  /* as args are declared as vars, put addrs in prog->args list */
  XEN args, arg, body;
  xen_value *v = NULL;
  int i, arg_num, body_forms;

  if (got_lambda) return(NULL);
  got_lambda = 1;

  args = XEN_CADR(form);
  if (!(XEN_LIST_P(args))) return(NULL);
  arg_num = XEN_LIST_LENGTH(args);

  if (arg_num > 1) return(NULL); /* temporary?? */

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
			   v = make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, 0.0), FALSE));
	  prog->args[i] = v->addr;
	  FREE(v);
	}
    }
  v = NULL;
  for (i = 0; i < body_forms; i++, body = XEN_CDR(body))
    {
      if (v) FREE(v);
      v = walk(prog, XEN_CAR(body), (i == (body_forms - 1)));
      if (v == NULL) return(NULL);
    }
  return(v);
}

static xen_value *begin_form(ptree *prog, XEN form, int need_result)
{
  /* (begin [TODO:defines] ...) returning last */
  XEN body;
  xen_value *v = NULL;
  int i, body_forms;
  body = XEN_CDR(form);
  body_forms = XEN_LIST_LENGTH(body);
  v = NULL;
  for (i = 0; i < body_forms; i++, body = XEN_CDR(body))
    {
      if (v) FREE(v);
      v = walk(prog, XEN_CAR(body), ((need_result) && (i == (body_forms - 1))));
      if (v == NULL) return(NULL);
    }
  return(v);
}

static void init_var(ptree *pt, xen_value *var, xen_value *init_val)
{
  if (var->type == R_FLOAT)
    add_triple_to_ptree(pt, va_make_triple(store_f, descr_store_f, 2, var, init_val));
  else add_triple_to_ptree(pt, va_make_triple(store_i, descr_store_i, 2, var, init_val));
}

static xen_value *let_star_form(ptree *prog, XEN form, int need_result)
{
  /* TODO: closure gc handling */
  XEN lets, body, var;
  xen_value *v = NULL, *vs;
  int i, body_forms, vars, locals_loc;
  lets = XEN_CADR(form);
  body = XEN_CDDR(form);
  body_forms = XEN_LIST_LENGTH(body);
  if (body_forms == 0) return(NULL);
  vars = XEN_LIST_LENGTH(lets);
  locals_loc = prog->var_ctr; /* lets can be nested */
  if (vars > 0)
    {
      for (i = 0; i < vars; i++, lets = XEN_CDR(lets))
	{
	  var = XEN_CAR(lets);
	  v = walk(prog, XEN_CADR(var), TRUE);
	  if (v == NULL) return(NULL);
	  if (v->type == R_FLOAT)
	    vs = make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, prog->dbls[v->addr]), FALSE);
	  else vs = make_xen_value(v->type, add_int_to_ptree(prog, prog->ints[v->addr]), FALSE);
	  add_var_to_ptree(prog, 
			   XEN_SYMBOL_TO_C_STRING(XEN_CAR(var)),
			   vs);
	  init_var(prog, vs, v);
	  FREE(vs);
	  FREE(v);
	}
    }
  /* TODO:defines */
  v = NULL;
  for (i = 0; i < body_forms; i++, body = XEN_CDR(body))
    {
      if (v) FREE(v);
      v = walk(prog, XEN_CAR(body), ((need_result) && (i == (body_forms - 1))));
      if (v == NULL) return(NULL);
    }
  for (i = locals_loc; i < prog->var_ctr; i++)
    if (prog->vars[i]) 
      prog->vars[i] = free_xen_var(prog->vars[i]);
  prog->var_ctr = locals_loc;
  return(v);
}

static xen_value *let_form(ptree *prog, XEN form, int need_result)
{
  /* keep vars until end of var section */
  XEN lets, body, var;
  xen_value *v = NULL;
  int i, body_forms, vars, locals_loc;
  xen_value **vs, **old_vs;
  lets = XEN_CADR(form);
  body = XEN_CDDR(form);
  body_forms = XEN_LIST_LENGTH(body);
  if (body_forms == 0) return(NULL);
  vars = XEN_LIST_LENGTH(lets);
  locals_loc = prog->var_ctr; /* lets can be nested */
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
	      return(NULL);
	    }
	  old_vs[i] = v;
	  if (v->type == R_FLOAT)
	    vs[i] = make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, prog->dbls[v->addr]), FALSE);
	  else vs[i] = make_xen_value(v->type, add_int_to_ptree(prog, prog->ints[v->addr]), FALSE);
	}
      lets = XEN_CADR(form);
      for (i = 0; i < vars; i++, lets = XEN_CDR(lets))
	{
	  var = XEN_CAR(lets);
	  add_var_to_ptree(prog, 
			   XEN_SYMBOL_TO_C_STRING(XEN_CAR(var)),
			   vs[i]);
	  /* in case called in loop with set! on locals, need to restore upon re-entry */
	  init_var(prog, vs[i], old_vs[i]);
	  FREE(vs[i]);
	  FREE(old_vs[i]);
	}
      FREE(old_vs);
      FREE(vs);
    }
  /* TODO:defines */
  v = NULL;
  for (i = 0; i < body_forms; i++, body = XEN_CDR(body))
    {
      if (v) FREE(v);
      v = walk(prog, XEN_CAR(body), ((need_result) && (i == (body_forms - 1))));
      if (v == NULL) return(NULL);
    }
  for (i = locals_loc; i < prog->var_ctr; i++)
    if (prog->vars[i]) 
      prog->vars[i] = free_xen_var(prog->vars[i]);
  prog->var_ctr = locals_loc;
  return(v);
}

static xen_value *cond_form(ptree *prog, XEN form, int need_result)
{
  return(NULL);
}

static xen_value *do_form(ptree *prog, XEN form, int need_result)
{
  return(NULL);
}

static xen_value *if_form(ptree *prog, XEN form, int need_result)
{
  /* form: (if selector true [false]) */
  xen_value *result = NULL, *true_result = NULL, *false_result = NULL;
  xen_value *jump_to_end = NULL, *jump_to_false, *if_value;
  int current_pc, false_pc = 0, has_false;
  has_false = (XEN_LIST_LENGTH(form) == 4);
  if_value = walk(prog, XEN_CADR(form), TRUE);                                      /* walk selector */
  if (if_value == NULL) return(NULL);
  if (if_value->type != R_BOOL) return(NULL);
  current_pc = prog->triple_ctr;
  jump_to_false = make_xen_value(R_INT, add_int_to_ptree(prog, 0), FALSE);
  add_triple_to_ptree(prog, va_make_triple(jump_if_not, descr_jump_if_not, 2, jump_to_false, if_value));
  true_result = walk(prog, XEN_CADDR(form), TRUE);                                  /* walk true branch */
  if (true_result == NULL) return(NULL);
  if (need_result)
    {
      if (true_result->type != R_FLOAT)
	{
	  result = make_xen_value(true_result->type, add_int_to_ptree(prog, 0), FALSE);
	  add_triple_to_ptree(prog, va_make_triple(store_i, descr_store_i, 2, result, true_result));
	}
      else
	{
	  result = make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, 0.0), FALSE);
	  add_triple_to_ptree(prog, va_make_triple(store_f, descr_store_f, 2, result, true_result));
	}
    }
  if (has_false) 
    {
      false_pc = prog->triple_ctr;
      jump_to_end = make_xen_value(R_INT, add_int_to_ptree(prog, 0), FALSE);
      add_triple_to_ptree(prog, va_make_triple(jump, descr_jump, 1, jump_to_end));  /* jump to end (past false) */
    }
  prog->ints[jump_to_false->addr] = prog->triple_ctr - current_pc - 1;              /* fixup jump-to-false addr */
  if (has_false)
    {
      false_result = walk(prog, XEN_CADDDR(form), TRUE);                            /* walk false branch */
      if (false_result == NULL) return(NULL);
      prog->ints[jump_to_end->addr] = prog->triple_ctr - false_pc;                  /* fixup jump-past-false addr */
      if (false_result->type != true_result->type)
	{
	  FREE(true_result);
	  FREE(false_result);
	  if (jump_to_end) FREE(jump_to_end);
	  if (jump_to_false) FREE(jump_to_false);
	  if (result) FREE(result);
	  return(NULL);
	}
      if (need_result)
	{
	  if (false_result->type != R_FLOAT)
	    add_triple_to_ptree(prog, va_make_triple(store_i, descr_store_i, 2, result, false_result));
	  else add_triple_to_ptree(prog, va_make_triple(store_f, descr_store_f, 2, result, false_result));
	}
    }
  else 
    {
      if (jump_to_end) FREE(jump_to_end);
      if (jump_to_false) FREE(jump_to_false);
      if (if_value) FREE(if_value);
      if (result) FREE(result);
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

static xen_value *callcc_form(ptree *prog, XEN form, int need_result)
{
  /* we could also save dbls/ints state if continuation procedure itself is returned (someday) */
  XEN lambda_form, continuation_name, lambda_body;
  continuation *c;
  int i, body_forms;
  xen_value *v = NULL;
  lambda_form = XEN_CADR(form);
  continuation_name = XEN_CAR(XEN_CADR(lambda_form));
  lambda_body = XEN_CDDR(lambda_form);
  body_forms = XEN_LIST_LENGTH(lambda_body);
  if (body_forms == 0) return(make_xen_value(R_BOOL, add_int_to_ptree(prog, 0), TRUE));
  c = add_goto_to_ptree(prog, XEN_SYMBOL_TO_C_STRING(continuation_name));
  v = NULL;
  for (i = 0; i < body_forms; i++, lambda_body = XEN_CDR(lambda_body))
    {
      if (v) FREE(v);
      v = walk(prog, XEN_CAR(lambda_body), ((need_result) && (i == (body_forms - 1))));
      if (v == NULL) return(NULL);
    }
  if (c->result)
    {
      if (v->type != c->result->type)
	return(NULL);
      if (need_result)
	{
      	  if (v->type == R_FLOAT)
	    add_triple_to_ptree(prog, va_make_triple(store_f, descr_store_f, 2, c->result, v));
	  else add_triple_to_ptree(prog, va_make_triple(store_i, descr_store_i, 2, c->result, v));
	}
      FREE(v);
      /* fixup the continuation jump, etc */
      prog->ints[c->jump->addr] = prog->triple_ctr;
      v = copy_xen_value(c->result);
      free_goto(prog, c);
    }
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
    return(make_xen_value(R_BOOL, add_int_to_ptree(prog, 0), TRUE));
  fixups = (xen_value **)CALLOC(body_forms, sizeof(xen_value *));
  for (i = 0; i < body_forms; i++, body = XEN_CDR(body))
    {
      v = walk(prog, XEN_CAR(body), TRUE);
      if ((v == NULL) || (v->type != R_BOOL)) 
	{
	  for (j = 0; j < i; j++)
	    if (fixups[j]) FREE(fixups[j]);
	  FREE(fixups);
	  return(NULL);
	}
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
    return(make_xen_value(R_BOOL, add_int_to_ptree(prog, 1), TRUE));
  fixups = (xen_value **)CALLOC(body_forms, sizeof(xen_value *));
  for (i = 0; i < body_forms; i++, body = XEN_CDR(body))
    {
      v = walk(prog, XEN_CAR(body), TRUE);
      if ((v == NULL) || (v->type != R_BOOL))
	{
	  for (j = 0; j < i; j++)
	    if (fixups[j]) FREE(fixups[j]);
	  FREE(fixups);
	  return(NULL);
	}
      fixups[i] = make_xen_value(R_INT, add_int_to_ptree(prog, prog->triple_ctr), FALSE);
      add_triple_to_ptree(prog, va_make_triple(jump_if_not, descr_jump_if_not, 2, fixups[i], v));
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
      prog->ints[fixups[i]->addr] = prog->triple_ctr - prog->ints[fixups[i]->addr] - 1;
      FREE(fixups[i]);
    }
  add_triple_to_ptree(prog, va_make_triple(store_false, descr_store_false, 1, result));
  prog->ints[jump_to_end->addr] = prog->triple_ctr - prog->ints[jump_to_end->addr] - 1;
  FREE(jump_to_end);
  FREE(fixups);
  return(result);
}

static xen_value *set_form(ptree *prog, XEN form, int need_result)
{
  char varname[256];
  xen_var *var;
  xen_value *v;
  if (!(XEN_SYMBOL_P(XEN_CADR(form)))) return(NULL); /* TODO: handle generalized set! */
  snprintf(varname, 256, "%s", XEN_SYMBOL_TO_C_STRING(XEN_CADR(form)));
  var = find_var_in_ptree(prog, varname);
  if (var == NULL)
    {
      v = add_global_var_to_ptree(prog, XEN_CADR(form));
      if (v) 
	{
	  var = find_var_in_ptree(prog, varname);
	  FREE(v);
	}
    }
  if (var)
    {
      v = walk(prog, XEN_CADDR(form), TRUE);
      if (v == NULL) return(NULL);
      if (v->type != var->v->type) 
	{
	  FREE(v);
	  return(NULL); /* variables have only one type in this context */
	}
      init_var(prog, var->v, v);
      var->unclean = TRUE;
      return(v);
    }
#if DEBUGGING
  else fprintf(stderr,"run: can't find %s\n", varname);
#endif
  return(NULL);
}

static xen_value *package(ptree *prog,
			  int type, 
			  void (*function)(int *arg_addrs, int *ints, Float *dbls),
			  char *(*descr)(int *arg_addrs, int *ints, Float *dbls),
			  xen_value **args,
			  int num_args)
{
  args[0] = make_xen_value(type, 
			   (type == R_FLOAT) ? add_dbl_to_ptree(prog, 0.0) : add_int_to_ptree(prog, 0), 
			   FALSE);
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
  new_args[1] = make_xen_value(R_INT, add_int_to_ptree(prog, num_args), TRUE);
  new_args[0] = make_xen_value(type, 
			       (type == R_FLOAT) ? add_dbl_to_ptree(prog, 0.0) : add_int_to_ptree(prog, 0), 
			       FALSE);
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
	    args[j] = make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, 0.0), FALSE);
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
  if (num_args == 0) return(make_xen_value(R_INT, add_int_to_ptree(prog, 1), TRUE));
  if (num_args == 1) return(copy_xen_value(args[1]));
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
	    args[cons_loc] = make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, fscl * iscl), TRUE);
	  else args[cons_loc] = make_xen_value(R_INT, add_int_to_ptree(prog, iscl), TRUE);
	  if ((iscl == 0) || (fscl == 0.0))
	    return(copy_xen_value(args[cons_loc]));
	}
      if (constants == num_args) 
	{
	  if (args[cons_loc])
	    return(copy_xen_value(args[cons_loc]));
	  else return(make_xen_value(R_INT, add_int_to_ptree(prog, 1), TRUE));
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
  return(NULL);
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
  if (num_args == 0) return(make_xen_value(R_INT, add_int_to_ptree(prog, 0), TRUE));
  if (num_args == 1) return(copy_xen_value(args[1]));
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
	    args[cons_loc] = make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, fscl + iscl), TRUE);
	  else args[cons_loc] = make_xen_value(R_INT, add_int_to_ptree(prog, iscl), TRUE);
	}
      if (constants == num_args) 
	{
	  if (args[cons_loc])
	    return(copy_xen_value(args[cons_loc]));
	  else return(make_xen_value(R_INT, add_int_to_ptree(prog, 0), TRUE));
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
  return(NULL);
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
  if (num_args == 0) return(NULL);
  if ((num_args == 1) && (args[1]->constant))
    {
      if (args[1]->type == R_INT)
	return(make_xen_value(R_INT, add_int_to_ptree(prog, -(prog->ints[args[1]->addr])), TRUE));
      else return(make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, -(prog->dbls[args[1]->addr])), TRUE));
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
	    args[cons_loc] = make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, fscl + iscl), TRUE);
	  else args[cons_loc] = make_xen_value(R_INT, add_int_to_ptree(prog, iscl), TRUE);
	}
      if (constants == num_args) 
	{
	  if (float_result)
	    {
	      if (args[1]->type == R_INT)
		return(make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, prog->dbls[args[1]->addr] - iscl), TRUE));
	      else return(make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, prog->dbls[args[1]->addr] - (fscl + iscl)), TRUE));
	    }
	  else return(make_xen_value(R_INT, add_int_to_ptree(prog, prog->ints[args[1]->addr] - iscl), TRUE));
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
  return(NULL);
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
  if (num_args == 0) return(NULL);
  if ((num_args == 1) && (args[1]->constant))
    {
      if (args[1]->type == R_INT)
	return(make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, (1.0 / (Float)(prog->ints[args[1]->addr]))), TRUE));
      else return(make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, (1.0 / (prog->dbls[args[1]->addr]))), TRUE));
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
	args[cons_loc] = make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, fscl), TRUE);
      if (constants == num_args) 
	{
	  if (args[1]->type == R_INT)
	    return(make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, (Float)(prog->ints[args[1]->addr]) / fscl), TRUE));
	  else return(make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, prog->dbls[args[1]->addr] / fscl), TRUE));
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
    if ((args[i]->constant) && (args[i]->type == R_INT))
      {
	old_loc = args[i];
	args[i] = make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, (Float)(prog->ints[args[i]->addr])), TRUE);
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
	args[i] = make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, 0.0), FALSE);
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
  for (i = 2; i <= n; i++)
    {
      BOOL_RESULT = (dbls[args[i]] > dbls[args[i + 1]]);
      if (!BOOL_RESULT) break;
    }
}
static char *descr_gt_fn(int *args, int *ints, Float *dbls) {return(describe_rel_f_args(">", ints[args[1]] + 1, args, ints, dbls, 2));}
static void gt_i2(int *args, int *ints, Float *dbls) {BOOL_RESULT = (INT_ARG_1 > INT_ARG_2);}
static char *descr_gt_i2(int *args, int *ints, Float *dbls) {return(describe_rel_i_args(">", 2, args, ints, 1));}
static void gt_in(int *args, int *ints, Float *dbls)
{
  int i, n;
  n = ints[args[1]];
  for (i = 2; i <= n; i++)
    {
      BOOL_RESULT = (ints[args[i]] > ints[args[i + 1]]);
      if (!BOOL_RESULT) break;
    }
}
static char *descr_gt_in(int *args, int *ints, Float *dbls) {return(describe_rel_i_args(">", ints[args[1]] + 1, args, ints, 2));}
static xen_value *greater_than(ptree *prog, int float_result, xen_value **args, int num_args, int constants)
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
      if (num_args == 2) return(package(prog, R_BOOL, gt_f2, descr_gt_f2, args, num_args));
      return(package_n(prog, R_BOOL, gt_fn, descr_gt_fn, args, num_args));
    }
  else
    {
      if (num_args == 2) return(package(prog, R_BOOL, gt_i2, descr_gt_i2, args, num_args));
      return(package_n(prog, R_BOOL, gt_in, descr_gt_in, args, num_args));
    }
  return(NULL);
}

/* ---------------- greater-than-or-equal ---------------- */

static void geq_f2(int *args, int *ints, Float *dbls) {BOOL_RESULT = (DBL_ARG_1 >= DBL_ARG_2);}
static char *descr_geq_f2(int *args, int *ints, Float *dbls) {return(describe_rel_f_args(">=", 2, args, ints, dbls, 1));}
static void geq_fn(int *args, int *ints, Float *dbls)
{
  int i, n;
  n = ints[args[1]];
  for (i = 2; i <= n; i++)
    {
      BOOL_RESULT = (dbls[args[i]] >= dbls[args[i + 1]]);
      if (!BOOL_RESULT) break;
    }
}
static char *descr_geq_fn(int *args, int *ints, Float *dbls) {return(describe_rel_f_args(">=", ints[args[1]] + 1, args, ints, dbls, 2));}
static void geq_i2(int *args, int *ints, Float *dbls) {BOOL_RESULT = (INT_ARG_1 >= INT_ARG_2);}
static char *descr_geq_i2(int *args, int *ints, Float *dbls) {return(describe_rel_i_args(">=", 2, args, ints, 1));}

static void geq_in(int *args, int *ints, Float *dbls)
{
  int i, n;
  n = ints[args[1]];
  for (i = 2; i <= n; i++)
    {
      BOOL_RESULT = (ints[args[i]] >= ints[args[i + 1]]);
      if (!BOOL_RESULT) break;
    }
}
static char *descr_geq_in(int *args, int *ints, Float *dbls) {return(describe_rel_i_args(">=", ints[args[1]] + 1, args, ints, 2));}
static xen_value *greater_than_or_equal(ptree *prog, int float_result, xen_value **args, int num_args, int constants)
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
      if (num_args == 2) return(package(prog, R_BOOL, geq_f2, descr_geq_f2, args, num_args));
      return(package_n(prog, R_BOOL, geq_fn, descr_geq_fn, args, num_args));
    }
  else
    {
      if (num_args == 2) return(package(prog, R_BOOL, geq_i2, descr_geq_i2, args, num_args));
      return(package_n(prog, R_BOOL, geq_in, descr_geq_in, args, num_args));
    }
  return(NULL);
}

/* ---------------- less-than ---------------- */

static void lt_f2(int *args, int *ints, Float *dbls) {BOOL_RESULT = (DBL_ARG_1 < DBL_ARG_2);}
static char *descr_lt_f2(int *args, int *ints, Float *dbls) {return(describe_rel_f_args("<", 2, args, ints, dbls, 1));}
static void lt_fn(int *args, int *ints, Float *dbls)
{
  int i, n;
  n = ints[args[1]];
  for (i = 2; i <= n; i++)
    {
      BOOL_RESULT = (dbls[args[i]] < dbls[args[i + 1]]);
      if (!BOOL_RESULT) break;
    }
}
static char *descr_lt_fn(int *args, int *ints, Float *dbls) {return(describe_rel_f_args("<", ints[args[1]] + 1, args, ints, dbls, 2));}
static void lt_i2(int *args, int *ints, Float *dbls) {BOOL_RESULT = (INT_ARG_1 < INT_ARG_2);}
static char *descr_lt_i2(int *args, int *ints, Float *dbls) {return(describe_rel_i_args("<", 2, args, ints, 1));}
static void lt_in(int *args, int *ints, Float *dbls)
{
  int i, n;
  n = ints[args[1]];
  for (i = 2; i <= n; i++)
    {
      BOOL_RESULT = (ints[args[i]] < ints[args[i + 1]]);
      if (!BOOL_RESULT) break;
    }
}
static char *descr_lt_in(int *args, int *ints, Float *dbls) {return(describe_rel_i_args("<", ints[args[1]] + 1, args, ints, 2));}
static xen_value *less_than(ptree *prog, int float_result, xen_value **args, int num_args, int constants)
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
      if (num_args == 2) return(package(prog, R_BOOL, lt_f2, descr_lt_f2, args, num_args));
      return(package_n(prog, R_BOOL, lt_fn, descr_lt_fn, args, num_args));
    }
  else
    {
      if (num_args == 2) return(package(prog, R_BOOL, lt_i2, descr_lt_i2, args, num_args));
      return(package_n(prog, R_BOOL, lt_in, descr_lt_in, args, num_args));
    }
  return(NULL);
}

/* ---------------- less-than-or-equal ---------------- */

static void leq_f2(int *args, int *ints, Float *dbls) {BOOL_RESULT = (DBL_ARG_1 <= DBL_ARG_2);}
static char *descr_leq_f2(int *args, int *ints, Float *dbls) {return(describe_rel_f_args("<=", 2, args, ints, dbls, 1));}
static void leq_fn(int *args, int *ints, Float *dbls)
{
  int i, n;
  n = ints[args[1]];
  for (i = 2; i <= n; i++)
    {
      BOOL_RESULT = (dbls[args[i]] <= dbls[args[i + 1]]);
      if (!BOOL_RESULT) break;
    }
}
static char *descr_leq_fn(int *args, int *ints, Float *dbls) {return(describe_rel_f_args("<=", ints[args[1]] + 1, args, ints, dbls, 2));}
static void leq_i2(int *args, int *ints, Float *dbls) {BOOL_RESULT = (INT_ARG_1 <= INT_ARG_2);}
static char *descr_leq_i2(int *args, int *ints, Float *dbls) {return(describe_rel_i_args("<=", 2, args, ints, 1));}
static void leq_in(int *args, int *ints, Float *dbls)
{
  int i, n;
  n = ints[args[1]];
  for (i = 2; i <= n; i++)
    {
      BOOL_RESULT = (ints[args[i]] <= ints[args[i + 1]]);
      if (!BOOL_RESULT) break;
    }
}
static char *descr_leq_in(int *args, int *ints, Float *dbls) {return(describe_rel_i_args("<=", ints[args[1]] + 1, args, ints, 2));}
static xen_value *less_than_or_equal(ptree *prog, int float_result, xen_value **args, int num_args, int constants)
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
      if (num_args == 2) return(package(prog, R_BOOL, leq_f2, descr_leq_f2, args, num_args));
      return(package_n(prog, R_BOOL, leq_fn, descr_leq_fn, args, num_args));
    }
  else
    {
      if (num_args == 2) return(package(prog, R_BOOL, leq_i2, descr_leq_i2, args, num_args));
      return(package_n(prog, R_BOOL, leq_in, descr_leq_in, args, num_args));
    }
  return(NULL);
}

/* ---------------- equal (as in "=") ---------------- */

static void equal_f2(int *args, int *ints, Float *dbls) {BOOL_RESULT = (DBL_ARG_1 == DBL_ARG_2);}
static char *descr_equal_f2(int *args, int *ints, Float *dbls) {return(describe_rel_f_args("==", 2, args, ints, dbls, 1));}
static void equal_fn(int *args, int *ints, Float *dbls)
{
  int i, n;
  n = ints[args[1]];
  for (i = 2; i <= n; i++)
    {
      BOOL_RESULT = (dbls[args[i]] == dbls[args[i + 1]]);
      if (!BOOL_RESULT) break;
    }
}
static char *descr_equal_fn(int *args, int *ints, Float *dbls) {return(describe_rel_f_args("==", ints[args[1]] + 1, args, ints, dbls, 2));}
static void equal_i2(int *args, int *ints, Float *dbls) {BOOL_RESULT = (INT_ARG_1 == INT_ARG_2);}
static char *descr_equal_i2(int *args, int *ints, Float *dbls) {return(describe_rel_i_args("==", 2, args, ints, 1));}
static void equal_in(int *args, int *ints, Float *dbls)
{
  int i, n;
  n = ints[args[1]];
  for (i = 2; i <= n; i++)
    {
      BOOL_RESULT = (ints[args[i]] == ints[args[i + 1]]);
      if (!BOOL_RESULT) break;
    }
}
static char *descr_equal_in(int *args, int *ints, Float *dbls) {return(describe_rel_i_args("==", ints[args[1]] + 1, args, ints, 2));}
static xen_value *numbers_equal(ptree *prog, int float_result, xen_value **args, int num_args, int constants)
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
      if (num_args == 2) return(package(prog, R_BOOL, equal_f2, descr_equal_f2, args, num_args));
      return(package_n(prog, R_BOOL, equal_fn, descr_equal_fn, args, num_args));
    }
  else
    {
      if (num_args == 2) return(package(prog, R_BOOL, equal_i2, descr_equal_i2, args, num_args));
      return(package_n(prog, R_BOOL, equal_in, descr_equal_in, args, num_args));
    }
  return(NULL);
}

/* ---------------- max ---------------- */

static void max_f2(int *args, int *ints, Float *dbls) {DBL_RESULT = (DBL_ARG_1 > DBL_ARG_2) ? DBL_ARG_1 : DBL_ARG_2;}
static char *descr_max_f2(int *args, int *ints, Float *dbls)
{
  return(mus_format("d%d(%.4f) = max(d%d(%.4f), d%d(%.4f))",
		    args[0], DBL_RESULT, args[1], DBL_ARG_1, args[2], DBL_ARG_2));
}
static void max_fn(int *args, int *ints, Float *dbls)
{
  int i, n;
  Float mx;
  n = ints[args[1]];
  mx = dbls[args[2]];
  for (i = 2; i <= n; i++)
    if (dbls[args[i + 1]] > mx) mx = dbls[args[i + 1]];
  DBL_RESULT = mx;
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
	  return(make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, fmx), TRUE));
	}
      else
	{
	  imx = prog->ints[args[1]->addr];
	  for (i = 2; i <= num_args; i++)
	    if (prog->ints[args[i]->addr] > imx) imx = prog->ints[args[i]->addr];
	  return(make_xen_value(R_INT, add_int_to_ptree(prog, imx), TRUE));
	}
    }
  float_rel_args(prog, num_args, args, float_result);
  if (float_result)
    {
      if (num_args == 2) return(package(prog, R_FLOAT, max_f2, descr_max_f2, args, num_args));
      return(package_n(prog, R_FLOAT, max_fn, descr_max_fn, args, num_args));
    }
  else
    {
      if (num_args == 2) return(package(prog, R_INT, max_i2, descr_max_i2, args, num_args));
      return(package_n(prog, R_INT, max_in, descr_max_in, args, num_args));
    }
  return(NULL);
}

/* ---------------- min ---------------- */

static void min_f2(int *args, int *ints, Float *dbls) {DBL_RESULT = (DBL_ARG_1 > DBL_ARG_2) ? DBL_ARG_2 : DBL_ARG_1;}
static char *descr_min_f2(int *args, int *ints, Float *dbls)
{
  return(mus_format("d%d(%.4f) = min(d%d(%.4f), d%d(%.4f))",
		    args[0], DBL_RESULT, args[1], DBL_ARG_1, args[2], DBL_ARG_2));
}
static void min_fn(int *args, int *ints, Float *dbls)
{
  int i, n;
  Float mx;
  n = ints[args[1]];
  mx = dbls[args[2]];
  for (i = 2; i <= n; i++)
    if (dbls[args[i + 1]] < mx) mx = dbls[args[i + 1]];
  DBL_RESULT = mx;
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
	  return(make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, fmx), TRUE));
	}
      else
	{
	  imx = prog->ints[args[1]->addr];
	  for (i = 2; i <= num_args; i++)
	    if (prog->ints[args[i]->addr] < imx) imx = prog->ints[args[i]->addr];
	  return(make_xen_value(R_INT, add_int_to_ptree(prog, imx), TRUE));
	}
    }
  float_rel_args(prog, num_args, args, float_result);
  if (float_result)
    {
      if (num_args == 2) return(package(prog, R_FLOAT, min_f2, descr_min_f2, args, num_args));
      return(package_n(prog, R_FLOAT, min_fn, descr_min_fn, args, num_args));
    }
  else
    {
      if (num_args == 2) return(package(prog, R_INT, min_i2, descr_min_i2, args, num_args));
      return(package_n(prog, R_INT, min_in, descr_min_in, args, num_args));
    }
  return(NULL);
}


/* ---------------- not ---------------- */

static void not_b(int *args, int *ints, Float *dbls) {BOOL_RESULT = (!(INT_ARG_1));}
static char *descr_not_b(int *args, int *ints, Float *dbls) {return(mus_format("i%d(%d) = (!(i%d(%d)))", args[0], BOOL_RESULT, args[1], INT_ARG_1));}
static xen_value *not_p(ptree *prog, xen_value **args, int constants)
{
  if (args[1]->type != R_BOOL)
    return(make_xen_value(R_BOOL, add_int_to_ptree(prog, FALSE), TRUE)); /* only #f is false so (not anything)->false */
  if (constants == 1)
    return(make_xen_value(R_BOOL, add_int_to_ptree(prog, !(prog->ints[args[1]->addr])), TRUE));
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
    return(make_xen_value(R_BOOL, add_int_to_ptree(prog, FALSE), TRUE));
  if (constants == 2)
    return(make_xen_value(R_BOOL, add_int_to_ptree(prog, prog->ints[args[1]->addr] == prog->ints[args[1]->addr]), TRUE));
  return(package(prog, R_BOOL, eq_b, descr_eq_b, args, 2));
}

static void eqv_fb(int *args, int *ints, Float *dbls) {BOOL_RESULT = (DBL_ARG_1 == DBL_ARG_2);}
static char *descr_eqv_fb(int *args, int *ints, Float *dbls) 
{
  return(mus_format("i%d(%d) = eqv?(d%d(%.4f), d%d(%.4f))", args[0], BOOL_RESULT, args[1], DBL_ARG_1, args[2], DBL_ARG_2));
}
static xen_value *eqv_p(ptree *prog, xen_value **args, int constants)
{
  if (args[1]->type != args[2]->type)
    return(make_xen_value(R_BOOL, add_int_to_ptree(prog, FALSE), TRUE));
  if (constants == 2)
    {
      if (args[1]->type == R_FLOAT)
	return(make_xen_value(R_BOOL, add_int_to_ptree(prog, prog->dbls[args[1]->addr] == prog->dbls[args[1]->addr]), TRUE));
      else return(make_xen_value(R_BOOL, add_int_to_ptree(prog, prog->ints[args[1]->addr] == prog->ints[args[1]->addr]), TRUE));
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
  if (args[1] == NULL) return(NULL);
  if (constants == 1)
    return(make_xen_value(R_BOOL, add_int_to_ptree(prog, prog->ints[args[1]->addr] & 1), TRUE));
  return(package(prog, R_BOOL, odd_i, descr_odd_i, args, 1));
}

static void even_i(int *args, int *ints, Float *dbls) {BOOL_RESULT = (!(INT_ARG_1 & 1));}
static char *descr_even_i(int *args, int *ints, Float *dbls) {return(mus_format("i%d(%d) = even?(i%d(%d))", args[0], BOOL_RESULT, args[1], INT_ARG_1));}
static xen_value *even_p(ptree *prog, xen_value **args, int constants)
{
  args[1] = convert_to_int(prog, args[1]);
  if (args[1] == NULL) return(NULL);
  if (constants == 1)
    return(make_xen_value(R_BOOL, add_int_to_ptree(prog, (!(prog->ints[args[1]->addr] & 1))), TRUE));
  return(package(prog, R_BOOL, even_i, descr_even_i, args, 1));
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
	if (args[1]->type == R_FLOAT)
	  return(make_xen_value(R_BOOL, add_int_to_ptree(prog, (prog->dbls[args[1]->addr] == 0.0)), TRUE));
	else return(NULL);
      }
    }
  if ((args[1]->type != R_INT) && (args[1]->type != R_FLOAT))
    return(NULL);
  if (args[1]->type == R_FLOAT)
    return(package(prog, R_BOOL, zero_f, descr_zero_f, args, 1));
  return(package(prog, R_BOOL, zero_i, descr_zero_i, args, 1));
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
	if (args[1]->type == R_FLOAT)
	  return(make_xen_value(R_BOOL, add_int_to_ptree(prog, (prog->dbls[args[1]->addr] > 0.0)), TRUE));
	else return(NULL);
      }
    }
  if ((args[1]->type != R_INT) && (args[1]->type != R_FLOAT))
    return(NULL);
  if (args[1]->type == R_FLOAT)
    return(package(prog, R_BOOL, positive_f, descr_positive_f, args, 1));
  return(package(prog, R_BOOL, positive_i, descr_positive_i, args, 1));
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
	if (args[1]->type == R_FLOAT)
	  return(make_xen_value(R_BOOL, add_int_to_ptree(prog, (prog->dbls[args[1]->addr] < 0.0)), TRUE));
	else return(NULL);
      }
    }
  if ((args[1]->type != R_INT) && (args[1]->type != R_FLOAT))
    return(NULL);
  if (args[1]->type == R_FLOAT)
    return(package(prog, R_BOOL, negative_f, descr_negative_f, args, 1));
  return(package(prog, R_BOOL, negative_i, descr_negative_i, args, 1));
}

static void single_to_float(ptree *prog, xen_value **args, int num)
{
  xen_value *old_loc;
  old_loc = args[num];
  args[num] = make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, 0.0), FALSE);
  add_triple_to_ptree(prog, va_make_triple(store_i_f, descr_store_i_f, 2, args[num], old_loc));
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
	return(make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, sin((Float)(prog->ints[args[1]->addr]))), TRUE));
      else return(make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, sin(prog->dbls[args[1]->addr])), TRUE));
    }
  if (args[1]->type == R_INT) single_to_float(prog, args, 1);
  return(package(prog, R_FLOAT, sin_f, descr_sin_f, args, 1));
}

static void cos_f(int *args, int *ints, Float *dbls) {DBL_RESULT = cos(DBL_ARG_1);}
static char *descr_cos_f(int *args, int *ints, Float *dbls) {return(mus_format("d%d(%.4f) = cos(d%d(%.4f))", args[0], DBL_RESULT, args[1], DBL_ARG_1));}
static xen_value *cos_1(ptree *prog, xen_value **args, int constants)
{
  if (constants == 1)
    {
      if (args[1]->type == R_INT)
	return(make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, cos((Float)(prog->ints[args[1]->addr]))), TRUE));
      else return(make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, cos(prog->dbls[args[1]->addr])), TRUE));
    }
  if (args[1]->type == R_INT) single_to_float(prog, args, 1);
  return(package(prog, R_FLOAT, cos_f, descr_cos_f, args, 1));
}

static void tan_f(int *args, int *ints, Float *dbls) {DBL_RESULT = tan(DBL_ARG_1);}
static char *descr_tan_f(int *args, int *ints, Float *dbls) {return(mus_format("d%d(%.4f) = tan(d%d(%.4f))", args[0], DBL_RESULT, args[1], DBL_ARG_1));}
static xen_value *tan_1(ptree *prog, xen_value **args, int constants)
{
  if (constants == 1)
    {
      if (args[1]->type == R_INT)
	return(make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, tan((Float)(prog->ints[args[1]->addr]))), TRUE));
      else return(make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, tan(prog->dbls[args[1]->addr])), TRUE));
    }
  if (args[1]->type == R_INT) single_to_float(prog, args, 1);
  return(package(prog, R_FLOAT, tan_f, descr_tan_f, args, 1));
}


/* ---------------- asin, acos, sqrt (opt?) ---------------- */

static void asin_f(int *args, int *ints, Float *dbls) {DBL_RESULT = asin(DBL_ARG_1);}
static char *descr_asin_f(int *args, int *ints, Float *dbls) {return(mus_format("d%d(%.4f) = asin(d%d(%.4f))", args[0], DBL_RESULT, args[1], DBL_ARG_1));}
static xen_value *asin_1(ptree *prog, xen_value **args, int constants)
{
  if (constants == 1)
    {
      if (args[1]->type == R_INT)
	return(make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, asin((Float)(prog->ints[args[1]->addr]))), TRUE));
      else return(make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, asin(prog->dbls[args[1]->addr])), TRUE));
    }
  if (args[1]->type == R_INT) single_to_float(prog, args, 1);
  return(package(prog, R_FLOAT, asin_f, descr_asin_f, args, 1));
}

static void acos_f(int *args, int *ints, Float *dbls) {DBL_RESULT = acos(DBL_ARG_1);}
static char *descr_acos_f(int *args, int *ints, Float *dbls) {return(mus_format("d%d(%.4f) = acos(d%d(%.4f))", args[0], DBL_RESULT, args[1], DBL_ARG_1));}
static xen_value *acos_1(ptree *prog, xen_value **args, int constants)
{
  if (constants == 1)
    {
      if (args[1]->type == R_INT)
	return(make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, acos((Float)(prog->ints[args[1]->addr]))), TRUE));
      else return(make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, acos(prog->dbls[args[1]->addr])), TRUE));
    }
  if (args[1]->type == R_INT) single_to_float(prog, args, 1);
  return(package(prog, R_FLOAT, acos_f, descr_acos_f, args, 1));
}

static void atan1_f(int *args, int *ints, Float *dbls) {DBL_RESULT = atan(DBL_ARG_1);}
static char *descr_atan1_f(int *args, int *ints, Float *dbls) {return(mus_format("d%d(%.4f) = atan(d%d(%.4f))", args[0], DBL_RESULT, args[1], DBL_ARG_1));}
static xen_value *atan1_1(ptree *prog, xen_value **args, int constants)
{
  if (constants == 1)
    {
      if (args[1]->type == R_INT)
	return(make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, atan((Float)(prog->ints[args[1]->addr]))), TRUE));
      else return(make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, atan(prog->dbls[args[1]->addr])), TRUE));
    }
  if (args[1]->type == R_INT) single_to_float(prog, args, 1);
  return(package(prog, R_FLOAT, atan1_f, descr_atan1_f, args, 1));
}

static void atan2_f(int *args, int *ints, Float *dbls) {DBL_RESULT = atan2(DBL_ARG_1, DBL_ARG_2);}
static char *descr_atan2_f(int *args, int *ints, Float *dbls) 
{
  return(mus_format("d%d(%.4f) = atan2(d%d(%.4f), d%d(%.4f))", args[0], DBL_RESULT, args[1], DBL_ARG_1, args[2], DBL_ARG_2));
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
    return(make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, atan2(prog->dbls[args[1]->addr], prog->dbls[args[2]->addr])), TRUE));
  return(package(prog, R_FLOAT, atan2_f, descr_atan2_f, args, 2));
}

static void sqrt_f(int *args, int *ints, Float *dbls) {DBL_RESULT = sqrt(DBL_ARG_1);}
static char *descr_sqrt_f(int *args, int *ints, Float *dbls) {return(mus_format("d%d(%.4f) = sqrt(d%d(%.4f))", args[0], DBL_RESULT, args[1], DBL_ARG_1));}
static xen_value *sqrt_1(ptree *prog, xen_value **args, int constants)
{
  if (constants == 1)
    {
      if (args[1]->type == R_INT)
	return(make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, sqrt((Float)(prog->ints[args[1]->addr]))), TRUE));
      else return(make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, sqrt(prog->dbls[args[1]->addr])), TRUE));
    }
  if (args[1]->type == R_INT) single_to_float(prog, args, 1);
  return(package(prog, R_FLOAT, sqrt_f, descr_sqrt_f, args, 1));
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
	return(make_xen_value(R_INT, add_int_to_ptree(prog, prog->ints[args[1]->addr]), TRUE)); /* r5rs spec says return int here */
      else return(make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, f_round(prog->dbls[args[1]->addr])), TRUE));
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

static void truncate_f(int *args, int *ints, Float *dbls) {DBL_RESULT = f_truncate(DBL_ARG_1);}
static char *descr_truncate_f(int *args, int *ints, Float *dbls) {return(mus_format("d%d(%.4f) = truncate(d%d(%.4f))", args[0], DBL_RESULT, args[1], DBL_ARG_1));}
static xen_value *truncate_1(ptree *prog, xen_value **args, int constants)
{
  if (constants == 1)
    {
      if (args[1]->type == R_INT) 
	return(make_xen_value(R_INT, add_int_to_ptree(prog, prog->ints[args[1]->addr]), TRUE));
      else return(make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, f_truncate(prog->dbls[args[1]->addr])), TRUE));
    }
  if (args[1]->type == R_INT)
    return(package(prog, R_FLOAT, store_i_f, descr_store_i_f, args,1));
  return(package(prog, R_FLOAT, truncate_f, descr_truncate_f, args, 1));
}

/* ---------------- floor ---------------- */

static void floor_f(int *args, int *ints, Float *dbls) {DBL_RESULT = floor(DBL_ARG_1);}
static char *descr_floor_f(int *args, int *ints, Float *dbls) {return(mus_format("d%d(%.4f) = floor(d%d(%.4f))", args[0], DBL_RESULT, args[1], DBL_ARG_1));}
static xen_value *floor_1(ptree *prog, xen_value **args, int constants)
{
  if (constants == 1)
    {
      if (args[1]->type == R_INT) 
	return(make_xen_value(R_INT, add_int_to_ptree(prog, prog->ints[args[1]->addr]), TRUE));
      else return(make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, floor(prog->dbls[args[1]->addr])), TRUE));
    }
  if (args[1]->type == R_INT)
    return(package(prog, R_FLOAT, store_i_f, descr_store_i_f, args,1));
  return(package(prog, R_FLOAT, floor_f, descr_floor_f, args, 1));
}

/* ---------------- ceiling ---------------- */

static void ceiling_f(int *args, int *ints, Float *dbls) {DBL_RESULT = ceil(DBL_ARG_1);}
static char *descr_ceiling_f(int *args, int *ints, Float *dbls) {return(mus_format("d%d(%.4f) = ceil(d%d(%.4f))", args[0], DBL_RESULT, args[1], DBL_ARG_1));}
static xen_value *ceiling_1(ptree *prog, xen_value **args, int constants)
{
  if (constants == 1)
    {
      if (args[1]->type == R_INT) 
	return(make_xen_value(R_INT, add_int_to_ptree(prog, prog->ints[args[1]->addr]), TRUE));
      else return(make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, ceil(prog->dbls[args[1]->addr])), TRUE));
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
    return(make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, (Float)(prog->ints[args[1]->addr])), TRUE));
  return(package(prog, R_FLOAT, store_i_f, descr_store_i_f, args,1));
}

static void i2e_f(int *args, int *ints, Float *dbls) {INT_RESULT = (int)floor(DBL_ARG_1 + 0.5);}
static char *descr_i2e_f(int *args, int *ints, Float *dbls) {return(mus_format("i%d(%d) = floor(d%d(%.4f) + 0.5)", args[0], INT_RESULT, args[1], DBL_ARG_1));}
static xen_value *inexact2exact_1(ptree *prog, xen_value **args, int constants)
{
  if (args[1]->type == R_INT)
    return(copy_xen_value(args[1]));
  if (constants == 1)
    return(make_xen_value(R_INT, add_int_to_ptree(prog, (int)floor(prog->dbls[args[1]->addr] + 0.5)), TRUE));
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
    return(make_xen_value(R_INT, add_int_to_ptree(prog, 0), TRUE)); /* (gcd) -> 0 */
  for (i = 1; i <= num_args; i++)
    {
      args[i] = convert_to_int(prog, args[i]);
      if (args[i] == NULL) return(NULL);
    }
  if (num_args == 1)
    {
      if (args[1]->constant)
	return(make_xen_value(R_INT, add_int_to_ptree(prog, prog->ints[args[1]->addr]), TRUE)); /* (gcd n) -> n */
      else 
	{
	  args[0] = make_xen_value(R_INT, add_int_to_ptree(prog, 0), FALSE);
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
      return(make_xen_value(R_INT, add_int_to_ptree(prog, mx), TRUE));
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
    return(make_xen_value(R_INT, add_int_to_ptree(prog, 1), TRUE)); /* (lcm) -> 1 */
  for (i = 1; i <= num_args; i++)
    {
      args[i] = convert_to_int(prog, args[i]);
      if (args[i] == NULL) return(NULL);
    }
  if (num_args == 1)
    {
      if (args[1]->constant)
	return(make_xen_value(R_INT, add_int_to_ptree(prog, prog->ints[args[1]->addr]), TRUE)); /* (lcm n) -> n */
      else 
	{
	  args[0] = make_xen_value(R_INT, add_int_to_ptree(prog, 0), FALSE);
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
      return(make_xen_value(R_INT, add_int_to_ptree(prog, mx), TRUE));
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
  if (args[1] == NULL) return(NULL);
  args[2] = convert_to_int(prog, args[2]);
  if (args[2] == NULL) return(NULL);
  if (constants == 2)
    return(make_xen_value(R_INT, add_int_to_ptree(prog, c_mod(prog->ints[args[1]->addr], prog->ints[args[2]->addr])), TRUE));
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
  if (args[1] == NULL) return(NULL);
  args[2] = convert_to_int(prog, args[2]);
  if (args[2] == NULL) return(NULL);
  if (constants == 2)
    return(make_xen_value(R_INT, add_int_to_ptree(prog, (prog->ints[args[1]->addr] % prog->ints[args[2]->addr])), TRUE));
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
  if (args[1] == NULL) return(NULL);
  args[2] = convert_to_int(prog, args[2]);
  if (args[2] == NULL) return(NULL);
  if (constants == 2)
    return(make_xen_value(R_INT, add_int_to_ptree(prog, (prog->ints[args[1]->addr] / prog->ints[args[2]->addr])), TRUE));
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
  if ((args[1]->type != R_INT) || (args[2]->type != R_INT)) return(NULL);
  if (constants == 2)
    return(make_xen_value(R_INT, add_int_to_ptree(prog, (prog->ints[args[1]->addr] & prog->ints[args[2]->addr])), TRUE));
  return(package(prog, R_INT, logand_i, descr_logand_i, args, 2));
}

static void logior_i(int *args, int *ints, Float *dbls) {INT_RESULT = (INT_ARG_1 | INT_ARG_2);}
static char *descr_logior_i(int *args, int *ints, Float *dbls) 
{
  return(mus_format("i%d(%d) = (i%d(%d) | i%d(%d))", args[0], INT_RESULT, args[1], INT_ARG_1, args[2], INT_ARG_2));
}
static xen_value *logior_1(ptree *prog, xen_value **args, int constants)
{
  if ((args[1]->type != R_INT) || (args[2]->type != R_INT)) return(NULL);
  if (constants == 2)
    return(make_xen_value(R_INT, add_int_to_ptree(prog, (prog->ints[args[1]->addr] | prog->ints[args[2]->addr])), TRUE));
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
  if ((args[1]->type != R_INT) || (args[2]->type != R_INT)) return(NULL);
  if (constants == 2)
    return(make_xen_value(R_INT, add_int_to_ptree(prog, XOR(prog->ints[args[1]->addr], prog->ints[args[2]->addr])), TRUE));
  return(package(prog, R_INT, logxor_i, descr_logxor_i, args, 2));
}

static void lognot_i(int *args, int *ints, Float *dbls) {INT_RESULT = ~INT_ARG_1;}
static char *descr_lognot_i(int *args, int *ints, Float *dbls) {return(mus_format("i%d(%d) = ~i%d(%d)", args[0], INT_RESULT, args[1], INT_ARG_1));}
static xen_value *lognot_1(ptree *prog, xen_value **args, int constants)
{
  if (args[1]->type != R_INT) return(NULL);
  if (constants == 1)
    return(make_xen_value(R_INT, add_int_to_ptree(prog, ~(prog->ints[args[1]->addr])), TRUE));
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
  if ((args[1]->type != R_INT) || (args[2]->type != R_INT)) return(NULL);
  if (constants == 2)
    return(make_xen_value(R_INT, add_int_to_ptree(prog, c_ash(prog->ints[args[1]->addr], prog->ints[args[2]->addr])), TRUE));
  return(package(prog, R_INT, ash_i, descr_ash_i, args, 2));
}


/* ---------------- log, exp, expt ---------------- */

static void log_f(int *args, int *ints, Float *dbls) {DBL_RESULT = log(DBL_ARG_1);}
static char *descr_log_f(int *args, int *ints, Float *dbls) {return(mus_format("d%d(%.4f) = log(d%d(%.4f))", args[0], DBL_RESULT, args[1], DBL_ARG_1));}
static xen_value *log_1(ptree *prog, xen_value **args, int constants)
{
  if (constants == 1)
    {
      if (args[1]->type == R_INT)
	return(make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, log((Float)(prog->ints[args[1]->addr]))), TRUE));
      else return(make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, log(prog->dbls[args[1]->addr])), TRUE));
    }
  if (args[1]->type == R_INT) single_to_float(prog, args, 1);
  return(package(prog, R_FLOAT, log_f, descr_log_f, args, 1));
}

static void exp_f(int *args, int *ints, Float *dbls) {DBL_RESULT = exp(DBL_ARG_1);}
static char *descr_exp_f(int *args, int *ints, Float *dbls) {return(mus_format("d%d(%.4f) = exp(d%d(%.4f))", args[0], DBL_RESULT, args[1], DBL_ARG_1));}
static xen_value *exp_1(ptree *prog, xen_value **args, int constants)
{
  if (constants == 1)
    {
      if (args[1]->type == R_INT)
	return(make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, exp((Float)(prog->ints[args[1]->addr]))), TRUE));
      else return(make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, exp(prog->dbls[args[1]->addr])), TRUE));
    }
  if (args[1]->type == R_INT) single_to_float(prog, args, 1);
  return(package(prog, R_FLOAT, exp_f, descr_exp_f, args, 1));
}

static void expt_f(int *args, int *ints, Float *dbls) {DBL_RESULT = pow(DBL_ARG_1, DBL_ARG_2);}
static char *descr_expt_f(int *args, int *ints, Float *dbls) 
{
  return(mus_format("d%d(%.4f) = pow(d%d(%.4f), d%d(%.4f))", 
		    args[0], DBL_RESULT, args[1], DBL_ARG_1, args[2], DBL_ARG_2));
}
static xen_value *expt_1(ptree *prog, xen_value **args, int constants)
{
  Float f1, f2;
  if (constants == 2)
    {
      if (args[1]->type == R_INT) f1 = (Float)(prog->ints[args[1]->addr]); else f1 = prog->dbls[args[1]->addr];
      if (args[2]->type == R_INT) f2 = (Float)(prog->ints[args[2]->addr]); else f2 = prog->dbls[args[2]->addr];
      return(make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, pow(f1, f2)), TRUE));
    }
  if (args[1]->type == R_INT) single_to_float(prog, args, 1);
  if (args[2]->type == R_INT) single_to_float(prog, args, 2);
  return(package(prog, R_FLOAT, expt_f, descr_expt_f, args, 2));
}


/* ---------------- abs ---------------- */

static void abs_f(int *args, int *ints, Float *dbls) {DBL_RESULT = fabs(DBL_ARG_1);}
static char *descr_abs_f(int *args, int *ints, Float *dbls) {return(mus_format("d%d(%.4f) = fabs(d%d(%.4f))", args[0], DBL_RESULT, args[1], DBL_ARG_1));}
static void abs_i(int *args, int *ints, Float *dbls) {INT_RESULT = abs(INT_ARG_1);}
static char *descr_abs_i(int *args, int *ints, Float *dbls) {return(mus_format("i%d(%d) = abs(i%d(%d))", args[0], INT_RESULT, args[1], INT_ARG_1));}
static xen_value *abs_1(ptree *prog, xen_value **args, int constants)
{
  if (constants == 1)
    {
      if (args[1]->type == R_INT)
	return(make_xen_value(R_INT, add_int_to_ptree(prog, abs(prog->ints[args[1]->addr])), TRUE));
      else return(make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, fabs(prog->dbls[args[1]->addr])), TRUE));
    }
  if (args[1]->type == R_INT)
    return(package(prog, R_INT, abs_i, descr_abs_i, args, 1));
  return(package(prog, R_FLOAT, abs_f, descr_abs_f, args, 1));
}


static void quit(int *args, int *ints, Float *dbls) {ALL_DONE = TRUE;}
static char *descr_quit(int *args, int *ints, Float *dbls)
{return(copy_string("quit"));}


static xen_value *clean_up(xen_value *result, xen_value **args, int args_size)
{
  int i;
  /* args[0] is special */
  if ((args[0]) && (args[0] != result)) FREE(args[0]);
  for (i = 1; i < args_size + 1; i++)
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
  /* fprintf(stderr,"walk %s\n", XEN_TO_C_STRING(XEN_TO_STRING(form))); */

  if (optimization(get_global_state()) == 0) return(NULL);

  if (XEN_BOOLEAN_P(form))
    return(make_xen_value(R_BOOL, add_int_to_ptree(prog, (XEN_FALSE_P(form)) ? 0 : 1), TRUE));
  if (XEN_NUMBER_P(form))
    {
      if ((XEN_EXACT_P(form)) && (XEN_INTEGER_P(form)))
	return(make_xen_value(R_INT, add_int_to_ptree(prog, XEN_TO_C_INT(form)), TRUE));
      else return(make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, XEN_TO_C_DOUBLE(form)), TRUE));
    }
  if (XEN_LIST_P(form))
    {
      XEN function, all_args;
      char funcname[32];
      xen_value **args = NULL;
      int i, num_args, float_result = FALSE, constants = 0, booleans = 0, vcts = 0;
      function = XEN_CAR(form);
      snprintf(funcname, 32, "%s", XEN_TO_C_STRING(XEN_TO_STRING(function))); /* protect from gc... */

      if (strcmp(funcname, "lambda") == 0) return(lambda_form(prog, form));
      if (strcmp(funcname, "begin") == 0) return(begin_form(prog, form, need_result));
      if (strcmp(funcname, "if") == 0) return(if_form(prog, form, need_result));
      if (strcmp(funcname, "do") == 0) return(do_form(prog, form, need_result));
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
	    return(make_xen_value(R_BOOL, add_int_to_ptree(prog, (XEN_FALSE_P(form)) ? 0 : 1), TRUE));
	  if (XEN_NUMBER_P(form))
	    {
	      if ((XEN_EXACT_P(form)) && (XEN_INTEGER_P(form)))
		return(make_xen_value(R_INT, add_int_to_ptree(prog, XEN_TO_C_INT(form)), TRUE));
	      else return(make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, XEN_TO_C_DOUBLE(form)), TRUE));
	    }
	  return(NULL);
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
	      if (args[i + 1]->type == R_FLOAT) float_result = TRUE;
	      if (args[i + 1]->type == R_BOOL) booleans++;
	      if (args[i + 1]->constant) constants++;
	      if (args[i + 1]->type == R_VCT) vcts++;
	    }
	}

      if ((!need_result) && (prog->goto_ctr == 0)) /* continuation might return from anywhere in the body */
	return(clean_up(make_xen_value(R_BOOL, -1, TRUE), args, num_args));
      if ((booleans == 0) && (vcts == 0))
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
	      if (strcmp(funcname, "expt") == 0) return(clean_up(expt_1(prog, args, constants), args, num_args));
	      if (strcmp(funcname, "atan") == 0) return(clean_up(atan2_1(prog, args, constants), args, num_args));
	    }
	}
      if (num_args == 2)
	{
	  if (strcmp(funcname, "eq?") == 0) return(clean_up(eq_p(prog, args, constants), args, num_args));
	  if (strcmp(funcname, "eqv?") == 0) return(clean_up(eqv_p(prog, args, constants), args, num_args));
	  if (strcmp(funcname, "equal?") == 0) return(clean_up(equal_p(prog, args, constants), args, num_args));
	}
      if (num_args == 1)
	{
	  if (strcmp(funcname, "not") == 0) return(clean_up(not_p(prog, args, constants), args, num_args));
	  if ((booleans == 0) && (vcts == 0))
	    {
	      if (strcmp(funcname, "lognot") == 0) return(clean_up(lognot_1(prog, args, constants), args, num_args));
	      if (strcmp(funcname, "abs") == 0) return(clean_up(abs_1(prog, args, constants), args, num_args));
	      if (strcmp(funcname, "sin") == 0) return(clean_up(sin_1(prog, args, constants), args, num_args));
	      if (strcmp(funcname, "cos") == 0) return(clean_up(cos_1(prog, args, constants), args, num_args));
	      if (strcmp(funcname, "tan") == 0) return(clean_up(tan_1(prog, args, constants), args, num_args));
	      if (strcmp(funcname, "atan") == 0) return(clean_up(atan1_1(prog, args, constants), args, num_args));
	      if (strcmp(funcname, "log") == 0) return(clean_up(log_1(prog, args, constants), args, num_args));
	      if (strcmp(funcname, "exp") == 0) return(clean_up(exp_1(prog, args, constants), args, num_args));
	      if (strcmp(funcname, "asin") == 0) return(clean_up(asin_1(prog, args, constants), args, num_args));
	      if (strcmp(funcname, "acos") == 0) return(clean_up(acos_1(prog, args, constants), args, num_args));
	      if (strcmp(funcname, "sqrt") == 0) return(clean_up(sqrt_1(prog, args, constants), args, num_args));
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
	  /* these are known in advance in this context */
	  if (strcmp(funcname, "boolean?") == 0) 
	    return(clean_up(make_xen_value(R_BOOL, add_int_to_ptree(prog, args[1]->type == R_BOOL), TRUE), args, num_args));
	  if (strcmp(funcname, "number?") == 0) 
	    return(clean_up(make_xen_value(R_BOOL, add_int_to_ptree(prog, args[1]->type != R_BOOL), TRUE), args, num_args));
	  if (strcmp(funcname, "integer?") == 0) 
	    return(clean_up(make_xen_value(R_BOOL, add_int_to_ptree(prog, args[1]->type == R_INT), TRUE), args, num_args));
	  if (strcmp(funcname, "real?") == 0) 
	    return(clean_up(make_xen_value(R_BOOL, add_int_to_ptree(prog, args[1]->type != R_BOOL), TRUE), args, num_args));
	  if (strcmp(funcname, "exact?") == 0) 
	    return(clean_up(make_xen_value(R_BOOL, add_int_to_ptree(prog, args[1]->type == R_INT), TRUE), args, num_args));
	  if (strcmp(funcname, "inexact?") == 0) 
	    return(clean_up(make_xen_value(R_BOOL, add_int_to_ptree(prog, args[1]->type == R_FLOAT), TRUE), args, num_args));
	  if (strcmp(funcname, "vct?") == 0) 
	    return(clean_up(make_xen_value(R_BOOL, add_int_to_ptree(prog, args[1]->type == R_VCT), TRUE), args, num_args));
	}
      /* TODO: if func is vct var, it's either vct-ref or generalized vct-set! */
      if (prog->goto_ctr > 0)
	{
	  /* possibly continuation procedure */
	  continuation *c = NULL;
	  for (i = prog->goto_ctr - 1; i >= 0; i--)
	    {
	      c = prog->gotos[i];
	      if ((c) && (strcmp(c->name, funcname) == 0))
		{
		  if (c->result)
		    {
		      if (c->result->type != args[1]->type) 
			return(clean_up(NULL, args, num_args));
		    }
		  else 
		    {
		      if (args[1]->type == R_FLOAT)
			c->result = make_xen_value(R_FLOAT, add_dbl_to_ptree(prog, 0.0), FALSE);
		      else c->result = make_xen_value(args[1]->type, add_int_to_ptree(prog, 0), FALSE);
		    }
		  if (c->result->type == R_FLOAT)
		    add_triple_to_ptree(prog, va_make_triple(store_f, descr_store_f, 2, c->result, args[1]));
		  else add_triple_to_ptree(prog, va_make_triple(store_i, descr_store_i, 2, c->result, args[1]));
		  add_triple_to_ptree(prog, va_make_triple(jump_abs, descr_jump_abs, 1, c->jump));
		  return(clean_up(copy_xen_value(c->result), args, num_args));
		}
	    }
	}
      return(clean_up(NULL, args, num_args));
    }
  return(add_global_var_to_ptree(prog, form));
}

void *form_to_ptree(XEN code)
{
  ptree *prog;
  prog = make_ptree(8);
  prog->result = walk(prog, code, TRUE);
  if (prog->result)
    {
      add_triple_to_ptree(prog, make_triple(quit, descr_quit, NULL, 0));
      return((void *)prog);
    }
  else free_ptree((void *)prog);
  return(NULL);
}

void *form_to_ptree_1f2f(XEN code)
{
  ptree *pt;
  pt = form_to_ptree(code);
  if (pt)
    {
      if (pt->result->type == R_FLOAT)
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
      if (pt->result->type == R_BOOL)
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
  prog = make_ptree(8);
  prog->result = walk(prog, code, TRUE);
  if (prog->result)
    {
      add_triple_to_ptree(prog, make_triple(quit, descr_quit, NULL, 0));
      return(XEN_WRAP_C_POINTER(prog));
    }
  return(XEN_FALSE);
}

static XEN g_run_eval(XEN code, XEN arg)
{
  ptree *pt;
  XEN result;
  pt = make_ptree(8);
  pt->result = walk(pt, code, TRUE);
  if (pt->result)
    {
      add_triple_to_ptree(pt, make_triple(quit, descr_quit, NULL, 0));
      if (XEN_BOUND_P(arg))
	pt->dbls[pt->args[0]] = (Float)XEN_TO_C_DOUBLE(arg);
      eval_ptree(pt);
      if (pt->result->type == R_FLOAT) result = C_TO_XEN_DOUBLE(pt->dbls[pt->result->addr]); else
	if (pt->result->type == R_INT) result = C_TO_XEN_INT(pt->ints[pt->result->addr]); else
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

/* this is map-channel with the parse tree support */
#define S_run_channel "run-channel"
char *run_channel(chan_info *cp, void *upt, int beg, int dur, int edpos);
char *run_channel(chan_info *cp, void *upt, int beg, int dur, int edpos)
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
      idata[j++] = MUS_FLOAT_TO_SAMPLE(evaluate_ptree_1f2f(upt, read_sample_to_float(sf)));
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
  void *upt;
  /* "form" here is actually a list: quoted form as list and form evaluated by Guile
   *    if we can't optimize the bare list, we fallback on scm_apply with the evaluated version
   */

  upt = form_to_ptree(XEN_CAR(form));
  if (upt)
    {
      ASSERT_SAMPLE_TYPE(S_run_channel, samp_n, XEN_ARG_2);
      ASSERT_SAMPLE_TYPE(S_run_channel, samps, XEN_ARG_3);
      ASSERT_CHANNEL(S_run_channel, snd_n, chn_n, 4);
      cp = get_cp(snd_n, chn_n, S_run_channel);
      pos = to_c_edit_position(cp, edpos, S_run_channel, XEN_ARG_6);
      beg = beg_to_sample(samp_n, S_run_channel);
      dur = dur_to_samples(samps, beg, cp, pos, XEN_ARG_3, S_run_channel);
      if (dur == 0) return(XEN_FALSE);
      errmsg = run_channel(cp, upt, beg, dur, pos);
    }
  else
    {
#if DEBUGGING
      fprintf(stderr, "fallback on map-channel\n");
#endif
      return(g_map_channel(XEN_CADR(form), samp_n, samps, snd_n, chn_n, edpos, C_TO_XEN_STRING(S_map_channel)));
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
  XEN_DEFINE_PROCEDURE("run-eval", g_run_eval, 1, 1, 0, "run macro...");
  XEN_DEFINE_PROCEDURE("describe-ptree", g_describe_ptree, 1, 0, 0, "");
  XEN_DEFINE_PROCEDURE("run-channel-1", g_run_channel, 1, 5, 0, H_run_channel);
}

#endif
#else
/* no guile */
void g_init_run(void)
{
}
#endif
