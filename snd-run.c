/* run macro */

#include "snd.h"
#if WITH_RUN

/* this is purely an experiment. */

typedef struct {
  void (*function)(int *arg_addrs, int *ints, Float *dbls);
  int *args;
} triple;

static triple *make_triple(void (*function)(int *arg_addrs, int *ints, Float *dbls), int args)
{
  triple *trp;
  trp = (triple *)CALLOC(1, sizeof(triple));
  trp->function = function;
  trp->args = (int *)CALLOC(args, sizeof(int));
  return(trp);
}

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
} ptree;

#define PC ints[1]
#define ALL_DONE ints[2]
#define RESULT ints[3]

static void eval_ptree(triple **program, int *ints, Float *dbls)
{
  triple *curfunc;
  PC = 0;
  ALL_DONE = FALSE;
  while (!ALL_DONE)
    {
      curfunc = program[PC++];
      (*(curfunc->function))(curfunc->args, ints, dbls);
    }
}

Float eval_ptree_to_float(triple **program, int *ints, Float *dbls);
Float eval_ptree_to_float(triple **program, int *ints, Float *dbls)
{
  eval_ptree(program, ints, dbls);
  return(dbls[RESULT]);
}

int eval_ptree_to_int(triple **program, int *ints, Float *dbls);
int eval_ptree_to_int(triple **program, int *ints, Float *dbls)
{
  eval_ptree(program, ints, dbls);
  return(ints[RESULT]);
}

static void multiply_2(int *args, int *ints, Float *dbls) {dbls[args[0]] = (dbls[args[1]] * dbls[args[2]]);}
static void add_2(int *args, int *ints, Float *dbls) {dbls[args[0]] = (dbls[args[1]] + dbls[args[2]]);}
static void gtf_2(int *args, int *ints, Float *dbls) {ints[args[0]] = (dbls[args[1]] > dbls[args[2]]);}

static void quit(int *args, int *ints, Float *dbls)
{
  ALL_DONE = TRUE;
  RESULT = args[0];
}

static XEN g_run(XEN code)
{
  
  return(XEN_FALSE);
}

void g_init_run(void)
{
  XEN_DEFINE_PROCEDURE("run", g_run, 1, 0, 0, "run macro...");
}

#endif
