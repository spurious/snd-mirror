/* version, apropos and help apparently clobber (ice-9 session) or (ice-9 documentation) procedures
 * snd module, and clm?
 */

#include "snd.h"
#include "vct.h"

#if HAVE_GUILE

#include "sndlib2scm.h"
#include "sg.h"

/* entire file on HAVE_GUILE switch
 * snd-xgh.c has the Motif-specific stuff (color support, etc), snd-gscm has gtk version
 */


/* scm_protect_object has been deliberately broken, so we have to write our own... */

static SCM gc_protection;
static int gc_protection_size = 0;
#define DEFAULT_GC_VALUE SCM_MAKINUM(0)
/* can't use SCM_UNDEFINED here because both "==" and SCM_EQ_P fail with it (grind teeth...) */

void snd_protect(SCM obj)
{
  int i,old_size;
  SCM tmp,num;
  if (gc_protection_size == 0)
    {
      gc_protection_size = 128;
      /* we don't know the size in advance since each channel can have its own edit/undo hooks */
      gc_protection = scm_make_vector(SCM_MAKINUM(gc_protection_size),DEFAULT_GC_VALUE);
      scm_permanent_object(gc_protection);
      scm_vector_set_x(gc_protection,SCM_MAKINUM(0),obj);
    }
  else
    {
      for (i=0;i<gc_protection_size;i++)
	{
	  if (SCM_EQ_P(scm_vector_ref(gc_protection,SCM_MAKINUM(i)),DEFAULT_GC_VALUE))
	    {
	      scm_vector_set_x(gc_protection,SCM_MAKINUM(i),obj);
	      return;
	    }
	}
      tmp = gc_protection;
      old_size = gc_protection_size;
      gc_protection_size *= 2;
      gc_protection = scm_make_vector(SCM_MAKINUM(gc_protection_size),DEFAULT_GC_VALUE);
      scm_permanent_object(gc_protection);
      for (i=0;i<old_size;i++)
	{
	  num = SCM_MAKINUM(i);
	  scm_vector_set_x(gc_protection,num,scm_vector_ref(tmp,num));
	  scm_vector_set_x(tmp,num,DEFAULT_GC_VALUE);
	}
      scm_vector_set_x(gc_protection,SCM_MAKINUM(old_size),obj);
    }
}

void snd_unprotect(SCM obj)
{
  int i;
  for (i=0;i<gc_protection_size;i++)
    {
      if (SCM_EQ_P(scm_vector_ref(gc_protection,SCM_MAKINUM(i)),obj))
	{
	  scm_vector_set_x(gc_protection,SCM_MAKINUM(i),DEFAULT_GC_VALUE);
	  return;
	}
    }
#if DEBUGGING
  fprintf(stderr,"gc trouble");
  abort();
#endif
}


static int bool_or_arg_p(SCM a) {return((gh_number_p(a)) || (gh_boolean_p(a)) || (SCM_UNBNDP(a)));}

int g_scm2int(SCM obj)
{
  /* don't want errors here about floats with non-zero fractions etc */
  if (SCM_INUMP(obj))
    return(SCM_INUM(obj));
  else
    if (gh_number_p(obj))
      return((int)scm_num2dbl(obj,"g_scm2int"));
  return(0);
}

int g_scm2intdef(SCM obj,int fallback)
{
  /* don't want errors here about floats with non-zero fractions etc */
  if (SCM_INUMP(obj))
    return(SCM_INUM(obj));
  else
    if (gh_number_p(obj))
      return((int)scm_num2dbl(obj,"g_scm2intdef"));
  return(fallback);
}

#define ERRN4(a,b) SCM_ASSERT(SCM_NFALSEP(scm_real_p(a)),a,SCM_ARG4,b)
#define ERRS3(a,b) SCM_ASSERT((gh_string_p(a)),a,SCM_ARG3,b)
#define ERRV1(a,b) SCM_ASSERT(((vct_p(a)) || (gh_vector_p(a))),a,SCM_ARG1,b)
#define ERRV2(a,b) SCM_ASSERT(((vct_p(a)) || (gh_vector_p(a))),a,SCM_ARG2,b)
#define ERRV3(a,b) SCM_ASSERT(((vct_p(a)) || (gh_vector_p(a))),a,SCM_ARG3,b)
#define ERRVECT1(a,b) SCM_ASSERT((gh_vector_p(a)),a,SCM_ARG1,b)
#define ERRVECT2(a,b) SCM_ASSERT((gh_vector_p(a)),a,SCM_ARG2,b)
#define ERRVECT4(a,b) SCM_ASSERT((gh_vector_p(a)),a,SCM_ARG4,b)
#define ERRB3(a,b) SCM_ASSERT((bool_or_arg_p(a)),a,SCM_ARG3,b)
#define ERRB4(a,b) SCM_ASSERT((bool_or_arg_p(a)),a,SCM_ARG4,b)

static void ERRSP(char *origin, SCM snd, int off)
{
  if (!((gh_number_p(snd)) || (SCM_FALSEP(snd)) || (SCM_UNBNDP(snd)) || (gh_list_p(snd))))
    scm_wrong_type_arg(origin,off,snd);
}

void ERRCP(char *origin, SCM snd, SCM chn, int off)
{
  if (!((gh_number_p(snd)) || (SCM_FALSEP(snd)) || (SCM_UNBNDP(snd)) || (gh_list_p(snd))))
    scm_wrong_type_arg(origin,off,snd);
  if (!((gh_number_p(chn)) || (SCM_FALSEP(chn)) || (SCM_UNBNDP(chn))))
    scm_wrong_type_arg(origin,off+1,chn);
}

#define ERRSB1(a,b) SCM_ASSERT(((gh_string_p(a)) || (SCM_FALSEP(a)) || (SCM_UNBNDP(a))),a,SCM_ARG1,b)
#define ERRSB3(a,b) SCM_ASSERT(((gh_string_p(a)) || (SCM_FALSEP(a)) || (SCM_UNBNDP(a))),a,SCM_ARG3,b)



static snd_state *state = NULL;
static int g_error_occurred = 0;

#define MAX_ERROR_STRING_LENGTH 512

#include <libguile/fluids.h>

char* guile_version(void) 
{ 
  return(gh_scm2newstr(scm_version(),NULL));
}

static SCM parse_proc_handler(void *data, SCM tag, SCM throw_args)
{
  g_error_occurred = 1;
  return(SCM_BOOL_F);
}

static SCM snd_catch_scm_error(void *data, SCM tag, SCM throw_args) /* error handler */
{
  snd_info *sp;
  /* it would be nice if this would display the current file + line number when loading scm code */
  /*   there's apparently a line-number function and *load-pathname* */
  SCM port,ans,stmp;
#if HAVE_GUILE_1_4
  SCM stack;
#endif
  char *name_buf = NULL;
  port = scm_mkstrport(SCM_INUM0, scm_make_string(SCM_MAKINUM(MAX_ERROR_STRING_LENGTH), SCM_UNDEFINED),SCM_OPN | SCM_WRTNG,"snd-scm-error-handler");
  /* scm_display_error string is ugly */
  scm_display(gh_car(throw_args), port);
  scm_puts(": ", port);
  stmp = gh_cadr(throw_args);
  if (gh_string_p(stmp)) 
    scm_display_error_message(stmp, gh_caddr(throw_args), port);
  else scm_display(tag, port);
  /* scm_display(gh_car(gh_cdddr(throw_args)), port); */
  /* scm_display_error (scm_the_last_stack_fluid, port, gh_car(throw_args), SCM_CADR(throw_args), SCM_CADDR(throw_args), SCM_EOL); */
  /* this from libguile/backtrace.c */
#if HAVE_GUILE_1_4
  stack = scm_fluid_ref(SCM_CDR(scm_the_last_stack_fluid));
  if (SCM_NFALSEP(stack)) scm_display_backtrace(stack,port,SCM_UNDEFINED,SCM_UNDEFINED);
#endif
#if (!HAVE_GUILE_1_3_0)
  scm_force_output(port); /* needed to get rid of trailing garbage chars */
  ans = scm_strport_to_string(port);
#else
  SCM_DEFER_INTS;
  ans = scm_makfromstr (SCM_CHARS (SCM_CDR (SCM_STREAM (port))),SCM_INUM (SCM_CAR (SCM_STREAM (port))),0);
  SCM_ALLOW_INTS;
#endif
  name_buf = gh_scm2newstr(ans,NULL);
  if (state->mx_sp)
    {
      sp = state->mx_sp;
      clear_minibuffer_prompt(sp);
      report_in_minibuffer(sp,name_buf);
    }
  if (state->listening != LISTENER_CLOSED)
    {
      state->result_printout = MESSAGE_WITH_CARET;
      snd_append_command(state,name_buf);
    }
  else 
    if (!(state->mx_sp))
      snd_error(name_buf);
  g_error_occurred = 1;
  if (name_buf) free(name_buf);
  return(SCM_BOOL_F);
}

int procedure_ok(SCM proc, int req_args, int opt_args, char *caller, char *arg_name, int argn)
{
  SCM arity_list;
  int args,res = 0;
  if (!(gh_procedure_p(proc)))
    {
      if (SCM_NFALSEP(proc)) /* #f as explicit arg to clear */
	snd_error("%s, arg %d to %s, is not a procedure!",arg_name,argn,caller);
    }
  else
    {
#if HAVE_GUILE_1_3_0
      return(1);
#else
      arity_list = scm_i_procedure_arity(proc);
      snd_protect(arity_list);
      args = gh_scm2int(gh_car(arity_list));
      if (args != req_args)
	snd_error("%s, arg %d to %s, should take %d required argument%s, but instead takes %d",
		  arg_name,argn,caller,
		  req_args,(req_args != 1) ? "s" : "",args);
      else
	{
	  args = gh_scm2int(gh_cadr(arity_list));
	  if (args != opt_args)
	    snd_error("%s, arg %d to %s, should take %d optional argument%s, but instead takes %d",
		      arg_name,argn,caller,
		      opt_args,(opt_args != 1) ? "s" : "",args);
	  /* ignore &rest */
	  else res = 1;
	}
      snd_unprotect(arity_list);
#endif
    }
  return(res);
}

static SCM eval_str_wrapper(void *data)
{
  return(gh_eval_str((char *)data));
}

static SCM eval_file_wrapper(void *data)
{
  return(gh_eval_file((char *)data));
}

static SCM g_call0_1(void *arg)
{
  return(scm_apply((SCM)arg,SCM_EOL,SCM_EOL));
}

SCM g_call0(SCM proc) /* replacement for gh_call0 -- protect ourselves from premature exit(!$#%@$) */
{
  return(scm_internal_stack_catch(SCM_BOOL_T,g_call0_1,(void *)proc,snd_catch_scm_error,NULL));
}

static SCM g_call1_1(void *arg)
{
  return(scm_apply(((SCM *)arg)[0],((SCM *)arg)[1],scm_listofnull));
}

SCM g_call1(SCM proc,SCM arg)
{
  SCM args[2];
  args[0] = proc;
  args[1] = arg;
  return(scm_internal_stack_catch(SCM_BOOL_T,g_call1_1,(void *)args,snd_catch_scm_error,NULL));
}

static SCM g_call_any_1(void *arg)
{
  return(scm_apply(((SCM *)arg)[0],((SCM *)arg)[1],SCM_EOL));
}

static SCM g_call_any(SCM proc, SCM arglist)
{
  SCM args[2];
  args[0] = proc;
  args[1] = arglist;
  return(scm_internal_stack_catch(SCM_BOOL_T,g_call_any_1,(void *)args,snd_catch_scm_error,NULL));
}

static SCM g_call2_1(void *arg)
{
  return(scm_apply(((SCM *)arg)[0],((SCM *)arg)[1],scm_cons(((SCM *)arg)[2],scm_listofnull)));
}

SCM g_call2(SCM proc,SCM arg1,SCM arg2)
{
  SCM args[3];
  args[0] = proc;
  args[1] = arg1;
  args[2] = arg2;
  return(scm_internal_stack_catch(SCM_BOOL_T,g_call2_1,(void *)args,snd_catch_scm_error,NULL));
}

static SCM g_call3_1(void *arg)
{
  return(scm_apply(((SCM *)arg)[0],((SCM *)arg)[1],scm_cons2(((SCM *)arg)[2],((SCM *)arg)[3],scm_listofnull)));
}

SCM g_call3(SCM proc,SCM arg1,SCM arg2,SCM arg3)
{
  SCM args[4];
  args[0] = proc;
  args[1] = arg1;
  args[2] = arg2;
  args[3] = arg3;
  return(scm_internal_stack_catch(SCM_BOOL_T,g_call3_1,(void *)args,snd_catch_scm_error,NULL));
}

int bool_int_or_one(SCM n)
{
  if (SCM_FALSEP(n)) 
    return(0);
  else return(g_scm2intdef(n,1));
  /* i.e. SCM_UNDEFINED -> t (which is needed for opt #t args below) */
}

static int bool_int_or_zero(SCM n)
{
  if (SCM_TRUE_P(n)) 
    return(1);
  else return(g_scm2intdef(n,0));
}

static char *full_filename(SCM file)
{
  char *urn,*filename;
  urn = gh_scm2newstr(file,NULL);
  filename = mus_file_full_name(urn);
  free(urn);
  return(filename);
}

static char *gh_print_1(SCM obj)
{
  char *str1;
#if DEBUGGING
  char *str2;
#endif
#if (!HAVE_GUILE_1_3_0)
  SCM str,val;
  SCM port;
  str = scm_makstr (0, 0);
  port = scm_mkstrport (SCM_INUM0, str, SCM_OPN | SCM_WRTNG, "scm_strprint_obj");
  scm_prin1 (obj, port, 1);
  val = scm_strport_to_string(port);
  scm_close_port (port);
  str1 = gh_scm2newstr(val,NULL);
#else
  str1 = gh_scm2newstr(scm_strprint_obj(obj),NULL);
#endif
#if DEBUGGING
  str2 = copy_string(str1);
  free(str1);
  return(str2);
#else
  return(str1);
#endif
}

static char *gh_print(SCM result)
{
  char *newbuf = NULL,*str = NULL;
  int i,ilen,savelen,savectr,slen;
  /* specialize vectors which can be enormous in this context */
  if ((!(gh_vector_p(result))) || ((int)(gh_vector_length(result)) <= print_length(state)))
    return(gh_print_1(result));
  ilen=print_length(state); 
  newbuf = (char *)CALLOC(128,sizeof(char));
  savelen = 128;
  savectr = 3;
  sprintf(newbuf,"#("); 
  for (i=0;i<ilen;i++)
    {
      str = gh_print_1(gh_vector_ref(result,gh_int2scm(i)));
      if ((str) && (*str)) 
	{
	  slen = strlen(str);
	  if ((slen+savectr+1) >= savelen)
	    {
	      savelen += 128;
	      newbuf = (char *)REALLOC(newbuf,savelen * sizeof(char));
	    }
	  if (i != 0) {strcat(newbuf," "); savectr++;}
	  strcat(newbuf,str);
	  savectr+=slen;
	}
      if (str) free(str);
    }
  if (savectr+8 > savelen) newbuf = (char *)REALLOC(newbuf,(savectr+8) * sizeof(char));
  strcat(newbuf," ...)");
  return(newbuf);
}

env *get_env(SCM e, SCM base, char *origin) /* list or vector in e */
{
  Float *buf = NULL;
  int i,len;
  env *newenv;
  SCM_ASSERT(((gh_vector_p(e)) || (gh_list_p(e))),e,SCM_ARG1,origin);
  if (gh_vector_p(e))
    {
      len = gh_vector_length(e);
      buf = (Float *)CALLOC(len,sizeof(Float));
      for (i=0;i<len;i++) buf[i] = gh_scm2double(gh_vector_ref(e,gh_int2scm(i)));
    }
  else
    if (gh_list_p(e))
      {
	len = gh_length(e);
	buf = (Float *)CALLOC(len,sizeof(Float));
        for (i=0;i<len;i++) buf[i] = gh_scm2double(scm_list_ref(e,gh_int2scm(i)));
      }
    else return(NULL);
  newenv = make_envelope(buf,len);
  if (gh_number_p(base)) newenv->base = gh_scm2double(base); else newenv->base = 1.0;
  if (buf) FREE(buf);
  return(newenv);
}

static SCM array_to_list(Float *arr, int i, int len)
{
  if (i < (len-1))
    return(gh_cons(gh_double2scm(arr[i]),array_to_list(arr,i+1,len)));
  else return(gh_cons(gh_double2scm(arr[i]),SCM_EOL));
}

SCM env2scm (env *e)
{
  if (e) return(array_to_list(e->data,0,e->pts*2));
  return(SCM_EOL);
}

#ifdef __cplusplus
static SCM gh_new_procedure0_3 (char *proc_name,SCM (*fn)(SCM,SCM,SCM)) {return(gh_new_procedure(proc_name,(SCM (*)(...))fn,0,3,0));}
static SCM gh_new_procedure0_4 (char *proc_name,SCM (*fn)(SCM,SCM,SCM,SCM)) {return(gh_new_procedure(proc_name,(SCM (*)(...))fn,0,4,0));}
static SCM gh_new_procedure0_5 (char *proc_name,SCM (*fn)(SCM,SCM,SCM,SCM,SCM)) {return(gh_new_procedure(proc_name,(SCM (*)(...))fn,0,5,0));}
static SCM gh_new_procedure0_6 (char *proc_name,SCM (*fn)(SCM,SCM,SCM,SCM,SCM,SCM)) {return(gh_new_procedure(proc_name,(SCM (*)(...))fn,0,6,0));}
static SCM gh_new_procedure0_7 (char *proc_name,SCM (*fn)(SCM,SCM,SCM,SCM,SCM,SCM,SCM)) {return(gh_new_procedure(proc_name,(SCM (*)(...))fn,0,7,0));}
static SCM gh_new_procedure1_3 (char *proc_name,SCM (*fn)(SCM,SCM,SCM,SCM)) {return(gh_new_procedure(proc_name,(SCM (*)(...))fn,1,3,0));}
static SCM gh_new_procedure1_4 (char *proc_name,SCM (*fn)(SCM,SCM,SCM,SCM,SCM)) {return(gh_new_procedure(proc_name,(SCM (*)(...))fn,1,4,0));}
static SCM gh_new_procedure1_5 (char *proc_name,SCM (*fn)(SCM,SCM,SCM,SCM,SCM,SCM)) {return(gh_new_procedure(proc_name,(SCM (*)(...))fn,1,5,0));}
static SCM gh_new_procedure1_7 (char *proc_name,SCM (*fn)(SCM,SCM,SCM,SCM,SCM,SCM,SCM,SCM)) {return(gh_new_procedure(proc_name,(SCM (*)(...))fn,1,7,0));}
static SCM gh_new_procedure2_3 (char *proc_name,SCM (*fn)(SCM,SCM,SCM,SCM,SCM)) {return(gh_new_procedure(proc_name,(SCM (*)(...))fn,2,3,0));}
static SCM gh_new_procedure3_1 (char *proc_name,SCM (*fn)(SCM,SCM,SCM,SCM)) {return(gh_new_procedure(proc_name,(SCM (*)(...))fn,3,1,0));}
static SCM gh_new_procedure3_2 (char *proc_name,SCM (*fn)(SCM,SCM,SCM,SCM,SCM)) {return(gh_new_procedure(proc_name,(SCM (*)(...))fn,3,2,0));}
static SCM gh_new_procedure4_2 (char *proc_name,SCM (*fn)(SCM,SCM,SCM,SCM,SCM,SCM)) {return(gh_new_procedure(proc_name,(SCM (*)(...))fn,4,2,0));}
static SCM gh_new_procedure9_0 (char *proc_name,SCM (*fn)(SCM,SCM,SCM,SCM,SCM,SCM,SCM,SCM,SCM)) {return(gh_new_procedure(proc_name,(SCM (*)(...))fn,9,0,0));}
#else
static SCM gh_new_procedure0_3 (char *proc_name,SCM (*fn)()) {return(gh_new_procedure(proc_name,fn,0,3,0));} /* not provided by gh_funcs.c */
static SCM gh_new_procedure0_4 (char *proc_name,SCM (*fn)()) {return(gh_new_procedure(proc_name,fn,0,4,0));}
static SCM gh_new_procedure0_5 (char *proc_name,SCM (*fn)()) {return(gh_new_procedure(proc_name,fn,0,5,0));}
static SCM gh_new_procedure0_6 (char *proc_name,SCM (*fn)()) {return(gh_new_procedure(proc_name,fn,0,6,0));}
static SCM gh_new_procedure0_7 (char *proc_name,SCM (*fn)()) {return(gh_new_procedure(proc_name,fn,0,7,0));}
static SCM gh_new_procedure1_3 (char *proc_name,SCM (*fn)()) {return(gh_new_procedure(proc_name,fn,1,3,0));}
static SCM gh_new_procedure1_4 (char *proc_name,SCM (*fn)()) {return(gh_new_procedure(proc_name,fn,1,4,0));}
static SCM gh_new_procedure1_5 (char *proc_name,SCM (*fn)()) {return(gh_new_procedure(proc_name,fn,1,5,0));}
static SCM gh_new_procedure1_7 (char *proc_name,SCM (*fn)()) {return(gh_new_procedure(proc_name,fn,1,7,0));}
static SCM gh_new_procedure2_3 (char *proc_name,SCM (*fn)()) {return(gh_new_procedure(proc_name,fn,2,3,0));}
static SCM gh_new_procedure3_1 (char *proc_name,SCM (*fn)()) {return(gh_new_procedure(proc_name,fn,3,1,0));}
static SCM gh_new_procedure3_2 (char *proc_name,SCM (*fn)()) {return(gh_new_procedure(proc_name,fn,3,2,0));}
static SCM gh_new_procedure4_2 (char *proc_name,SCM (*fn)()) {return(gh_new_procedure(proc_name,fn,4,2,0));}
static SCM gh_new_procedure9_0 (char *proc_name,SCM (*fn)()) {return(gh_new_procedure(proc_name,fn,9,0,0));}
#endif


SCM parse_proc(char *buf)
{
  SCM result;
  result = scm_internal_stack_catch(SCM_BOOL_T,eval_str_wrapper,buf,parse_proc_handler,buf);
  if (g_error_occurred)
    {
      g_error_occurred = 0;
      return(SCM_BOOL_F);
    }
  return(result);
}

int snd_eval_str(snd_state *ss, char *buf, int count)
{
  /* scm_set_current_output_port? */
  snd_info *sp = NULL;
  SCM result = SCM_UNDEFINED;
  int ctr;
  char *str = NULL;
  for (ctr=0;ctr<count;ctr++)
    {
      result = scm_internal_stack_catch(SCM_BOOL_T,eval_str_wrapper,buf,snd_catch_scm_error,buf);
      if (g_error_occurred)
	{
	  g_error_occurred = 0;
	  /* "result" is not interesting in this case, and can erase our error message if just using minibuffer */
	  return(-1);
	}
    }
  str = gh_print(result);
  if (ss->mx_sp)
    {
      sp = ss->mx_sp;
      clear_minibuffer_prompt(sp);
      report_in_minibuffer(sp,str);
    }
  if (ss->listening != LISTENER_CLOSED)
    {
      snd_append_command(ss,buf);
      ss->result_printout = MESSAGE_WITH_CARET;
      snd_append_command(ss,str);
    }
  if (str) FREE(str);
  return(0);
}

int snd_eval_listener_str(snd_state *ss, char *buf)
{
  SCM result;
  char *str;
  if ((snd_strlen(buf) == 0) || ((snd_strlen(buf) == 1) && (buf[0] == '\n'))) return(0);
  result = scm_internal_stack_catch(SCM_BOOL_T,eval_str_wrapper,buf,snd_catch_scm_error,buf);
  if (g_error_occurred)
    {
      g_error_occurred = 0;
      return(0);
    }
  str = gh_print(result);
  ss->result_printout = MESSAGE_WITH_CARET;
  snd_append_command(ss,str);
  if (str) FREE(str);
  return(0);
}


void snd_eval_stdin_str(snd_state *ss, char *buf)
{
  SCM result;
  char *str = NULL;
  if ((snd_strlen(buf) == 0) || ((snd_strlen(buf) == 1) && (buf[0] == '\n'))) return;
  result = scm_internal_stack_catch(SCM_BOOL_T,eval_str_wrapper,buf,snd_catch_scm_error,buf);
  if (g_error_occurred)
    g_error_occurred = 0;
  else
    {
      str = gh_print(result);
      write(fileno(stdout),str,snd_strlen(str));
      FREE(str);
      str = (char *)CALLOC(8,sizeof(char));
      sprintf(str,"\n%s",listener_prompt(ss));
      write(fileno(stdout),str,snd_strlen(str));
      FREE(str);
    }
}

void snd_load_init_file(snd_state *ss, int nog, int noi)
{
  /* look for ".snd" on the home directory */
  /* called only in snd-xmain.c at initialization time */
  int fd;
  char *str = NULL;
#ifdef SND_CONF
  if (nog == 0)
    {
      fd = open(SND_CONF,O_RDONLY,0);
      if (fd != -1)
	{
	  close(fd);
	  scm_internal_stack_catch(SCM_BOOL_T,eval_file_wrapper,SND_CONF,snd_catch_scm_error,NULL);
	}
    }
#endif
  if ((ss->init_file) && (noi == 0))
    {
      str = mus_file_full_name(ss->init_file);
      fd = open(str,O_RDONLY,0);
      if (fd != -1) 
	{
	  close(fd);
	  scm_internal_stack_catch(SCM_BOOL_T,eval_file_wrapper,str,snd_catch_scm_error,NULL);
	}
      if (str) FREE(str);
    }
}

int snd_load_file(snd_state *ss,char *filename)
{
  char *str = NULL,*str1 = NULL;
  str = mus_file_full_name(filename);
  if (!mus_file_probe(str))
    {
      /* try tacking on .scm */
      str1 = (char *)CALLOC(snd_strlen(str) + 5,sizeof(char));
      sprintf(str1,"%s.scm",str);
      scm_internal_stack_catch(SCM_BOOL_T,eval_file_wrapper,str1,snd_catch_scm_error,NULL);
      FREE(str1);
    }
  else scm_internal_stack_catch(SCM_BOOL_T,eval_file_wrapper,str,snd_catch_scm_error,NULL);
  if (str) FREE(str);
  return(0);
}

static SCM snd_test = SCM_BOOL_F, full_test = SCM_BOOL_F;

static SCM g_snd_print(SCM msg)
{
  #define H_snd_print "(" S_snd_print " str) displays str in the lisp listener window"
  int fd;
  char *str=NULL,*buf;
  char time_buf[64];
  time_t ts;
  SCM val;
  ERRS1(msg,S_snd_print);
  state->result_printout = MESSAGE_WITHOUT_CARET;
  str = gh_scm2newstr(msg,NULL);
  snd_append_command(state,str);
  val = gh_cdr(snd_test);
  if ((gh_number_p(val)) && (g_scm2int(val) != -1))
    {
      if (mus_file_probe("test.errors") == 0) 
	fd = mus_file_create("test.errors");
      else
	{
	  fd = mus_file_reopen_write("test.errors");
	  lseek(fd,0L,SEEK_END);
	}
      buf = (char *)CALLOC(snd_strlen(str)+64+2,sizeof(char));
      time(&ts);
      strftime(time_buf,64,STRFTIME_FORMAT,localtime(&ts));
      sprintf(buf,"[%s]:%s\n",time_buf,str);
      write(fd,buf,snd_strlen(buf));
      close(fd);
      if (g_scm2int(val) == -2) fprintf(stderr,buf);
      FREE(buf);
    }
  if (str) free(str);
  return(msg);
}

static SCM g_snd_error(SCM msg)
{
  #define H_snd_error "(" S_snd_error " str) reports error message str"
  char *str = NULL;
  ERRS1(msg,S_snd_error);
  str = gh_scm2newstr(msg,NULL);
  snd_error(str);
  if (str) free(str);
  return(msg);
}
  
static SCM g_snd_warning(SCM msg)
{
  #define H_snd_warning "(" S_snd_warning " str) reports warning message str"
  char *str = NULL;
  ERRS1(msg,S_snd_warning);
  str = gh_scm2newstr(msg,NULL);
  snd_warning(str);
  if (str) free(str);
  return(msg);
}
  

/* global variables */

static Float fclamp(Float lo, Float val, Float hi) {if (val>hi) return(hi); else if (val<lo) return(lo); else return(val);}
static int iclamp(int lo, int val, int hi) {if (val>hi) return(hi); else if (val<lo) return(lo); else return(val);}

static SCM g_ask_before_overwrite(void) {RTNBOOL(ask_before_overwrite(state));}
static SCM g_set_ask_before_overwrite(SCM val) 
{
  #define H_ask_before_overwrite "(" S_ask_before_overwrite ") should be #t if you want Snd to ask before overwriting a file"
  #define H_set_ask_before_overwrite "(" S_set_ask_before_overwrite " &optional (val #t)) sets " S_ask_before_overwrite
  ERRB1(val,S_set_ask_before_overwrite); 
  set_ask_before_overwrite(state,bool_int_or_one(val)); 
  RTNBOOL(ask_before_overwrite(state));
}

static SCM g_audio_output_device(void) {RTNINT(audio_output_device(state));}
static SCM g_set_audio_output_device(SCM val) 
{
  #define H_audio_output_device "(" S_audio_output_device ") is the current sndlib default output device (mus-audio-default)"
  #define H_set_audio_output_device "(" S_set_audio_output_device " val) sets " S_audio_output_device
  ERRN1(val,S_set_audio_output_device); 
  set_audio_output_device(state,g_scm2int(val)); 
  RTNINT(audio_output_device(state));
}

static SCM g_dac_size(void) {RTNINT(dac_size(state));}
static SCM g_set_dac_size(SCM val) 
{
  #define H_dac_size "(dac-size) is the current DAC buffer size (256)"
  #define H_set_dac_size "(" S_set_dac_size " val) sets the DAC buffer size"
  ERRN1(val,S_set_dac_size); 
  set_dac_size(state,g_scm2int(val)); 
  RTNINT(dac_size(state));
}

static SCM g_dac_folding(void) {RTNBOOL(dac_folding(state));}
static SCM g_set_dac_folding(SCM val) 
{
  #define H_dac_folding "(" S_dac_folding ") should be #t if extra channels are to be folded into available ones during playing (#t)"
  #define H_set_dac_folding "(" S_set_dac_folding " &optional (val #t)) sets " S_dac_folding
  ERRB1(val,S_set_dac_folding); 
  set_dac_folding(state,bool_int_or_one(val)); 
  RTNBOOL(dac_folding(state));
}

static SCM g_auto_resize(void) {RTNBOOL(auto_resize(state));}
static SCM g_set_auto_resize(SCM val) 
{
  #define H_auto_resize "(" S_auto_resize ") should be #t if Snd can change its main window size as it pleases (#t)"
  #define H_set_auto_resize "(" S_set_auto_resize " &optional (val #t)) sets " S_auto_resize
  ERRB1(val,S_set_auto_resize); 
  set_auto_resize(state,bool_int_or_one(val)); 
  reflect_resize(state); 
  RTNBOOL(auto_resize(state));
}

static SCM g_auto_update(void) {RTNBOOL(auto_update(state));}
static SCM g_set_auto_update(SCM val) 
{
  #define H_auto_update "(" S_auto_update ") -> #t if Snd should automatically update a file if it changes unexpectedly (#f)"
  #define H_set_auto_update "(" S_set_auto_update " &optional (val #t)) sets " S_auto_update
  ERRB1(val,S_set_auto_update); 
  set_auto_update(state,bool_int_or_one(val)); 
  RTNBOOL(auto_update(state));
}

static SCM g_graphs_horizontal(void) {RTNBOOL(graphs_horizontal(state));}
static SCM g_set_graphs_horizontal(SCM val) 
{
  #define H_graphs_horizontal "(" S_graphs_horizontal ") -> #t if the time domain, fft, and lisp graphs are layed out horizontally (#t)"
  #define H_set_graphs_horizontal "(" S_set_graphs_horizontal " &optional (val #t)) sets " S_graphs_horizontal
  ERRB1(val,S_set_graphs_horizontal); 
  set_graphs_horizontal(state,bool_int_or_one(val)); 
  RTNBOOL(graphs_horizontal(state));
}

static SCM g_channel_style(void) {RTNINT(channel_style(state));}
static SCM g_set_channel_style(SCM style) 
{
  #define H_channel_style "(" S_channel_style ") -> how multichannel sounds layout the channels\n\
   default is channels-separate, other values are channels-combined and channels-superimposed.\n\
   this is the default setting for each sound's 'unite' button."
  #define H_set_channel_style "(" S_set_channel_style " val) sets " S_channel_style
  ERRN1(style,S_set_channel_style); 
  set_channel_style(state,iclamp(CHANNELS_SEPARATE,g_scm2int(style),CHANNELS_SUPERIMPOSED)); 
  RTNINT(channel_style(state));
}

static SCM g_color_cutoff(void) {RTNFLT(color_cutoff(state));}
static SCM g_set_color_cutoff(SCM val) 
{
  #define H_color_cutoff "(" S_color_cutoff ") -> color map cutoff point (default .003)"
  #define H_set_color_cutoff "(" S_set_color_cutoff " val) sets " S_color_cutoff
  ERRN1(val,S_set_color_cutoff);
  set_color_cutoff(state,fclamp(0.0,gh_scm2double(val),0.25)); 
  RTNFLT(color_cutoff(state));
}

static SCM g_color_inverted(void) {RTNBOOL(color_inverted(state));}
static SCM g_set_color_inverted(SCM val) 
{
  #define H_color_inverted "(" S_color_inverted ") -> whether the colormap in operation should be inverted"
  #define H_set_color_inverted "(" S_set_color_inverted " &optional (val #t)) sets " S_color_inverted
  ERRB1(val,S_set_color_inverted); 
  set_color_inverted(state,bool_int_or_one(val)); 
  RTNBOOL(color_inverted(state));
}

static SCM g_color_scale(void) {RTNFLT(color_scale(state));}
static SCM g_set_color_scale(SCM val) 
{
  #define H_color_scale "(" S_color_scale ") -> essentially a darkness setting for colormaps (0.5)"
  #define H_set_color_scale "(" S_set_color_scale " val) sets " S_color_scale
  ERRN1(val,S_set_color_scale); 
  set_color_scale(state,fclamp(0.0,gh_scm2double(val),1.0)); 
  RTNFLT(color_scale(state));
}

static SCM g_corruption_time(void) {RTNFLT(corruption_time(state));}
static SCM g_set_corruption_time(SCM val) 
{
  #define H_corruption_time "(" S_corruption_time ") -> time (seconds) between background checks for changed file on disk (60)"
  #define H_set_corruption_time "(" S_set_corruption_time " val) sets " S_corruption_time
  ERRN1(val,S_set_corruption_time); 
  set_corruption_time(state,gh_scm2double(val)); 
  RTNFLT(corruption_time(state));
}

static SCM g_default_output_chans(void) {RTNINT(default_output_chans(state));}
static SCM g_set_default_output_chans(SCM val) 
{
  #define H_default_output_chans "(" S_default_output_chans ") -> default number of channels when a new or temporary file is created (1)"
  #define H_set_default_output_chans "(" S_set_default_output_chans " val) sets " S_default_output_chans
  ERRN1(val,S_set_default_output_chans); 
  set_default_output_chans(state,g_scm2int(val));
  RTNINT(default_output_chans(state));
}

static SCM g_default_output_srate(void) {RTNINT(default_output_srate(state));}
static SCM g_set_default_output_srate(SCM val) 
{
  #define H_default_output_srate "(" S_default_output_srate ") -> default srate when a new or temporary file is created (22050)" 
  #define H_set_default_output_srate "(" S_set_default_output_srate " val) sets " S_default_output_srate
  ERRN1(val,S_set_default_output_srate); 
  set_default_output_srate(state,g_scm2int(val));
  RTNINT(default_output_srate(state));
}

static SCM g_default_output_type(void) {RTNINT(default_output_type(state));}
static SCM g_set_default_output_type(SCM val) 
{
  int typ;
  #define H_default_output_type "(" S_default_output_type ") -> default header type when a new or temporary file is created\n\
   normally this is mus-next, -1 here indicates you want Snd to use the current sound's header type, if possible\n\
   other writable headers include mus-aiff, mus-riff, mus-ircam, mus-nist, mus-aifc, mus-raw"
  #define H_set_default_output_type "(" S_set_default_output_type " val) sets " S_default_output_type
  ERRN1(val,S_set_default_output_type); 
  typ = g_scm2int(val);
  if (mus_header_writable(typ,-2))
    set_default_output_type(state,typ); 
  else snd_warning("can't write %s headers",mus_header_type_name(typ));
  RTNINT(default_output_type(state));
}

static SCM g_default_output_format(void) {RTNINT(default_output_format(state));}
static SCM g_set_default_output_format(SCM val) 
{
  int format;
  #define H_default_output_format "(" S_default_output_format ") -> default data format when a new or temporary file is created\n\
   normally mus-bshort, -1 here means try to use the current sound's data format; many other formats\n\
   are available, but not all are compatible with all header types"
  #define H_set_default_output_format "(" S_set_default_output_format " val) sets " S_default_output_format
  ERRN1(val,S_set_default_output_format); 
  format = g_scm2int(val);
  if (mus_header_writable(default_output_type(state),format))
    set_default_output_format(state,format); 
  else snd_warning("can't write %s data to %s headers",mus_data_format_name(format),mus_header_type_name(default_output_type(state)));
  RTNINT(default_output_format(state));
}

static SCM g_dot_size(void) {RTNINT(dot_size(state));}
static SCM g_set_dot_size(SCM size) 
{
  #define H_dot_size "(" S_dot_size ") -> size in pixels of dots when graphing with dots (1)"
  #define H_set_dot_size "(" S_set_dot_size " val) sets " S_dot_size
  ERRN1(size,S_set_dot_size); 
  set_dot_size(state,g_scm2int(size));
  RTNINT(dot_size(state));
}

static SCM g_enved_base(void) {RTNFLT(enved_base(state));}
static SCM g_set_enved_base(SCM val) 
{
  #define H_enved_base "(" S_enved_base ") -> envelope editor exponential base value (1.0)"
  #define H_set_enved_base "(" S_set_enved_base " val) sets " S_enved_base
  ERRN1(val,S_set_enved_base); 
  set_enved_base(state,gh_scm2double(val));
  RTNFLT(enved_base(state));
}

static SCM g_enved_power(void) {RTNFLT(enved_power(state));}
static SCM g_set_enved_power(SCM val) 
{
  #define H_enved_power "(" S_enved_power ") -> envelope editor base scale range (9.0^power)"
  #define H_set_enved_power "(" S_set_enved_power " val) sets " S_enved_power
  ERRN1(val,S_set_enved_power); 
  set_enved_power(state,gh_scm2double(val));
  RTNFLT(enved_power(state));
}

static SCM g_enved_clipping(void) {RTNBOOL(enved_clipping(state));}
static SCM g_set_enved_clipping(SCM on)
{
  #define H_enved_clipping "(" S_enved_clipping ") -> envelope editor 'clip' button setting\n\
   if clipping, the motion of the mouse is restricted to the current graph bounds."
  #define H_set_enved_clipping "(" S_set_enved_clipping " &optional (val #t)) sets " S_enved_clipping
  ERRB1(on,S_set_enved_clipping); 
  set_enved_clipping(state,bool_int_or_one(on)); 
  RTNBOOL(enved_clipping(state));
}

static SCM g_enved_exping(void) {RTNBOOL(enved_exping(state));}
static SCM g_set_enved_exping(SCM val) 
{
  #define H_enved_exping "(" S_enved_exping ") -> envelope editor 'exp' and 'lin' buttons\n\
   if enved-exping, the connecting segments use exponential curves rather than straight lines."
  #define H_set_enved_exping "(" S_set_enved_exping " &optional (val #t)) sets " S_enved_exping
  ERRB1(val,S_set_enved_exping); 
  set_enved_exping(state,bool_int_or_one(val)); 
  RTNBOOL(enved_clipping(state));
}

static SCM g_enved_target(void) {RTNINT(enved_target(state));}
static SCM g_set_enved_target(SCM val) 
{
  int n; 
  #define H_enved_target "(" S_enved_target ") determines how the envelope is applied to data in the envelope editor\n\
   choices are amplitude-env, srate-env, and spectrum-env"
  #define H_set_enved_target "(" S_set_enved_target " val) sets " S_enved_target
  ERRN1(val,S_set_enved_target); 
  n = iclamp(AMPLITUDE_ENV,g_scm2intdef(val,0),SRATE_ENV); 
  set_enved_target(state,n); 
  RTNINT(enved_target(state));
}

static SCM g_enved_waving(void) {RTNBOOL(enved_waving(state));}
static SCM g_set_enved_waving(SCM val) 
{
  #define H_enved_waving "(" S_enved_waving ") -> #t if the envelope editor is displaying the waveform to be edited"
  #define H_set_enved_waving "(" S_set_enved_waving " &optional (val #t)) sets " S_enved_waving
  ERRB1(val,S_set_enved_waving); 
  set_enved_waving(state,bool_int_or_one(val));
  RTNBOOL(enved_waving(state));
}

static SCM g_enved_dBing(void) {RTNBOOL(enved_dBing(state));}
static SCM g_set_enved_dBing(SCM val) 
{
  #define H_enved_dBing "(" S_enved_dBing ") -> #t if the envelope editor is using dB"
  #define H_set_enved_dBing "(" S_set_enved_dBing " &optional (val #t)) sets " S_enved_dBing
  ERRB1(val,S_set_enved_dBing); 
  set_enved_dBing(state,bool_int_or_one(val)); 
  RTNBOOL(enved_dBing(state));
}

static SCM g_eps_file(void) {RTNSTR(eps_file(state));}
static SCM g_set_eps_file(SCM val) 
{
  #define H_eps_file "(" S_eps_file ") -> current eps ('Print' command) file name (snd.eps)"
  #define H_set_eps_file "(" S_set_eps_file " val) sets " S_eps_file
  ERRS1(val,S_set_eps_file); 
  set_eps_file(state,gh_scm2newstr(val,0)); 
  RTNSTR(eps_file(state));
}

static SCM g_listener_prompt(void) {RTNSTR(listener_prompt(state));}
static SCM g_set_listener_prompt(SCM val) 
{
  #define H_listener_prompt "(" S_listener_prompt ") -> the current lisp listener prompt character ('>') "
  #define H_set_listener_prompt "(" S_set_listener_prompt " val) sets " S_listener_prompt
  ERRS1(val,S_set_listener_prompt); 
  set_listener_prompt(state,gh_scm2newstr(val,0));
  RTNSTR(listener_prompt(state));
}

static SCM g_audio_state_file(void) {RTNSTR(audio_state_file(state));}
static SCM g_set_audio_state_file(SCM val) 
{
  #define H_audio_state_file "(" S_audio_state_file ") -> filename for the mus-audio-save-state function (.snd-mixer)"
  #define H_set_audio_state_file "(" S_set_audio_state_file " val) sets " S_audio_state_file
  ERRS1(val,S_set_audio_state_file); 
  set_audio_state_file(state,gh_scm2newstr(val,0));
  RTNSTR(audio_state_file(state));
}

static SCM g_fft_beta(void) {RTNFLT(fft_beta(state));}
static SCM g_set_fft_beta(SCM val) 
{
  #define H_fft_beta "(" S_fft_beta ") -> 'beta' fft data window parameter value (0.0)"
  #define H_set_fft_beta "(" S_set_fft_beta " val) sets " S_fft_beta
  ERRN1(val,S_set_fft_beta); 
  set_fft_beta(state,gh_scm2double(val));
  RTNFLT(fft_beta(state));
}

static SCM g_fft_log_frequency(void) {RTNBOOL(fft_log_frequency(state));}
static SCM g_set_fft_log_frequency(SCM on) 
{
  #define H_fft_log_frequency "(" S_fft_log_frequency ") -> #t if fft displays use log on the frequency axis (#f)"
  #define H_set_fft_log_frequency "(" S_set_fft_log_frequency " &optional (val #t)) sets " S_fft_log_frequency
  ERRB1(on,S_set_fft_log_frequency); 
  set_fft_log_frequency(state,bool_int_or_one(on)); 
  RTNBOOL(fft_log_frequency(state));
}

static SCM g_fft_log_magnitude(void) {RTNBOOL(fft_log_magnitude(state));}
static SCM g_set_fft_log_magnitude(SCM on) 
{
  #define H_fft_log_magnitude "(" S_fft_log_magnitude ") -> #t if fft displays use log on the magnitude axis (#f)"
  #define H_set_fft_log_magnitude "(" S_set_fft_log_magnitude " &optional (val #t)) sets " S_fft_log_magnitude
  ERRB1(on,S_set_fft_log_magnitude); 
  set_fft_log_magnitude(state,bool_int_or_one(on)); 
  RTNBOOL(fft_log_magnitude(state));
}

static SCM g_fft_size(void) {RTNINT(fft_size(state));}
static SCM g_set_fft_size(SCM val) 
{
  #define H_fft_size "(" S_fft_size ") -> current fft size (256)"
  #define H_set_fft_size "(" S_set_fft_size " val) sets " S_fft_size
  ERRN1(val,S_set_fft_size); 
  set_fft_size(state,(int)pow(2,(ceil(log((double)(g_scm2int(val)))/log(2.0)))));
  RTNINT(fft_size(state));
}

static SCM g_fft_style(void) {RTNINT(fft_style(state));}
static SCM g_set_fft_style(SCM val) 
{
  #define H_fft_style "(" S_fft_style ") -> normal-fft, sonogram, or spectrogram"
  #define H_set_fft_style "(" S_set_fft_style " val) sets " S_fft_style
  ERRN1(val,S_set_fft_style); 
  set_fft_style(state,iclamp(NORMAL_FFT,g_scm2int(val),SPECTROGRAM));
  RTNINT(fft_style(state));
}

static SCM g_fft_window(void) {RTNINT(fft_window(state));}
static SCM g_set_fft_window(SCM val) 
{
  #define H_fft_window "(" S_fft_window ") -> current fft data window choice (e.g. blackman2-window)"
  #define H_set_fft_window "(" S_set_fft_window " val) sets " S_fft_window
  ERRN1(val,S_set_fft_window); 
  set_fft_window(state,g_scm2int(val));
  RTNINT(fft_window(state));
}

static SCM g_filter_env_order(void) {RTNINT(filter_env_order(state));}
static SCM g_set_filter_env_order(SCM val) 
{
  #define H_filter_env_order "(" S_filter_env_order ") -> envelope editor's FIR filter order (40)"
  #define H_set_filter_env_order "(" S_set_filter_env_order " val) sets " S_filter_env_order
  ERRN1(val,S_set_filter_env); 
  set_filter_env_order(state,g_scm2int(val));
  RTNINT(filter_env_order(state));
}

static SCM g_fit_data_on_open(void) {RTNBOOL(fit_data_on_open(state));}
static SCM g_set_fit_data_on_open(SCM val) 
{
  #define H_fit_data_on_open "(" S_fit_data_on_open ") -> #t if Snd should set up the initial time domain axes to show the entire sound (#f)"
  #define H_set_fit_data_on_open "(" S_set_fit_data_on_open " &optional (val #t)) sets " S_fit_data_on_open
  ERRB1(val,S_set_fit_data_on_open); 
  set_fit_data_on_open(state,bool_int_or_one(val));
  RTNBOOL(fit_data_on_open(state));
}

static SCM g_graph_style(void) {RTNINT(graph_style(state));}
static SCM g_set_graph_style(SCM style) 
{
  #define H_graph_style "(" S_graph_style ") -> how graphs are drawn: graph-lines, graph-dots, etc"
  #define H_set_graph_style "(" S_set_graph_style " val) sets " S_graph_style
  ERRN1(style,S_set_graph_style); 
  set_graph_style(state,iclamp(GRAPH_LINES,g_scm2int(style),GRAPH_LOLLIPOPS));
  RTNINT(graph_style(state));
}

static SCM g_initial_x0(void) {RTNFLT(initial_x0(state));}
static SCM g_set_initial_x0(SCM val) 
{
  #define H_initial_x0 "(" S_initial_x0 ") -> initial time domain window left edge (seconds, 0.0)"
  #define H_set_initial_x0 "(" S_set_initial_x0 " val) sets " S_initial_x0
  ERRN1(val,S_set_initial_x0); 
  set_initial_x0(state,gh_scm2double(val));
  RTNFLT(initial_x0(state));
}

static SCM g_initial_x1(void) {RTNFLT(initial_x1(state));}
static SCM g_set_initial_x1(SCM val) 
{
  #define H_initial_x1 "(" S_initial_x1 ") -> initial time domain window right edge (seconds, 0.1)"
  #define H_set_initial_x1 "(" S_set_initial_x1 " val) sets " S_initial_x1
  ERRN1(val,S_set_initial_x1); 
  set_initial_x1(state,gh_scm2double(val));
  RTNFLT(initial_x1(state));
}

static SCM g_initial_y0(void) {RTNFLT(initial_y0(state));}
static SCM g_set_initial_y0(SCM val) 
{
  #define H_initial_y0 "(" S_initial_y0 ") -> initial time domain window lower edge (-1.0)"
  #define H_set_initial_y0 "(" S_set_initial_y0 " val) sets " S_initial_y0
  ERRN1(val,S_set_initial_y0); 
  set_initial_y0(state,gh_scm2double(val));
  RTNFLT(initial_y0(state));
}

static SCM g_initial_y1(void) {RTNFLT(initial_y1(state));}
static SCM g_set_initial_y1(SCM val) 
{
  #define H_initial_y1 "(" S_initial_y1 ") -> initial time domain window upper edge (1.0)"
  #define H_set_initial_y1 "(" S_set_initial_y1 " val) sets " S_initial_y1
  ERRN1(val,S_set_initial_y1); 
  set_initial_y1(state,gh_scm2double(val));
  RTNFLT(initial_y1(state));
}

static SCM g_line_size(void) {RTNINT(line_size(state));}
static SCM g_set_line_size(SCM val) 
{
  #define H_line_size "(" S_line_size ") -> number of samples in a 'line' (C-n and C-p) (128)"
  #define H_set_line_size "(" S_set_line_size " val) sets " S_line_size
  ERRN1(val,S_set_line_size); 
  set_line_size(state,g_scm2int(val));
  RTNINT(line_size(state));
}

static SCM g_mix_console_amp_scaler(void) {RTNFLT(get_mix_console_amp_scaler());}
static SCM g_set_mix_console_amp_scaler(SCM val) 
{
  #define H_mix_console_amp_scaler "(" S_mix_console_amp_scaler ") -> multiplier on amp scales in mix consoles (1.0)"
  #define H_set_mix_console_amp_scaler "(" S_set_mix_console_amp_scaler " val) sets " S_mix_console_amp_scaler
  ERRN1(val,S_set_mix_console_amp_scaler); 
  set_mix_console_amp_scaler(gh_scm2double(val));
  RTNFLT(get_mix_console_amp_scaler());
}

static SCM g_mix_console_speed_scaler(void) {RTNFLT(get_mix_console_speed_scaler());}
static SCM g_set_mix_console_speed_scaler(SCM val) 
{
  #define H_mix_console_speed_scaler "(" S_mix_console_speed_scaler ") -> Multiplier on speed scales in mix consoles (1.0)"
  #define H_set_mix_console_speed_scaler "(" S_set_mix_console_speed_scaler " val) sets " S_mix_console_speed_scaler
  ERRN1(val,S_set_mix_console_speed_scaler); 
  set_mix_console_speed_scaler(gh_scm2double(val));
  RTNFLT(get_mix_console_speed_scaler());
}

static SCM g_movies(void) {RTNBOOL(movies(state));}
static SCM g_set_movies(SCM val) 
{
  #define H_movies "(" S_movies ") -> #t if mix graphs are update continuously as the mix is dragged (#t)"
  #define H_set_movies "(" S_set_movies " &optional (val #t)) sets " S_movies
  ERRB1(val,S_set_movies); 
  set_movies(state,bool_int_or_one(val));
  RTNBOOL(movies(state));
}

static SCM g_normalize_fft(void) {RTNBOOL(normalize_fft(state));}
static SCM g_set_normalize_fft(SCM val) 
{
  #define H_normalize_fft "(" S_normalize_fft ") -> #t is spectral data is normalized before display (#t)"
  #define H_set_normalize_fft "(" S_set_normalize_fft " &optional (val #t)) sets " S_normalize_fft
  ERRB1(val,S_set_normalize_fft); 
  set_normalize_fft(state,bool_int_or_one(val));
  RTNBOOL(normalize_fft(state));
}

static SCM g_normalize_on_open(void) {RTNBOOL(normalize_on_open(state));}
static SCM g_set_normalize_on_open(SCM val) 
{
  #define H_normalize_on_open "(" S_normalize_on_open ") -> #t if Snd should try to evenly apportion screen space when a sound is opened (#t)"
  #define H_set_normalize_on_open "(" S_set_normalize_on_open " &optional (val #t)) sets " S_normalize_on_open
  ERRB1(val,S_set_normalize_on_open); 
  set_normalize_on_open(state,bool_int_or_one(val));
  RTNBOOL(normalize_on_open(state));
}

static SCM g_prefix_arg(void) {RTNINT(prefix_arg(state));}
static SCM g_set_prefix_arg(SCM val) 
{
  #define H_prefix_arg "(" S_prefix_arg ") -> keyboard C-u argument"
  #define H_set_prefix_arg "(" S_set_prefix_arg " val) sets " S_prefix_arg
  ERRN1(val,S_set_prefix_arg); 
  set_prefix_arg(state,g_scm2int(val));
  RTNINT(prefix_arg(state));
}

static SCM g_print_length(void) {RTNINT(print_length(state));}
static SCM g_set_print_length(SCM val) 
{
  #define H_print_length "(" S_print_length ") -> number of vector elements to print in the listener (12)"
  #define H_set_print_length "(" S_set_print_length " val) sets " S_print_length
  ERRN1(val,S_set_print_length); 
  set_print_length(state,g_scm2int(val)); 
  set_vct_print_length(g_scm2int(val));
  RTNINT(print_length(state));
}

static SCM g_previous_files_sort(void) {RTNINT(previous_files_sort(state));}
static SCM g_set_previous_files_sort(SCM val) 
{
  #define H_previous_files_sort "(" S_previous_files_sort ") -> sort choice in view files (0=unsorted, 1=by name, etc)"
  #define H_set_previous_files_sort "(" S_set_previous_files_sort " val) sets " S_previous_files_sort
  ERRN1(val,S_set_previous_files_sort); 
  set_previous_files_sort(state,g_scm2int(val)); 
  update_prevfiles(state);
  RTNINT(previous_files_sort(state));
}

static SCM g_raw_chans(void) {RTNINT(raw_chans(state));}
static SCM g_set_raw_chans(SCM val) 
{
  #define H_raw_chans "(" S_raw_chans ") -> how many channels to expect in raw (headerless) files (1)"
  #define H_set_raw_chans "(" S_set_raw_chans " val) sets " S_raw_chans
  ERRN1(val,S_set_raw_chans);
  set_raw_chans(state,g_scm2int(val));
  RTNINT(raw_chans(state));
}

static SCM g_raw_format(void) {RTNINT(raw_format(state));}
static SCM g_set_raw_format(SCM val) 
{
  #define H_raw_format "(" S_raw_format ") -> data format expected in raw (headerless) files (mus-bshort)"
  #define H_set_raw_format "(" S_set_raw_format " val) sets " S_raw_format
  ERRN1(val,S_set_raw_format); 
  set_raw_format(state,g_scm2int(val));
  RTNINT(raw_format(state));
}

static SCM g_raw_srate(void) {RTNINT(raw_srate(state));}
static SCM g_set_raw_srate(SCM val) 
{
  #define H_raw_srate "(" S_raw_srate ") -> srate expected in raw (headerless) files (44100)"
  #define H_set_raw_srate "(" S_set_raw_srate " val) sets " S_raw_srate
  ERRN1(val,S_set_raw_srate); 
  set_raw_srate(state,g_scm2int(val));
  RTNINT(raw_srate(state));
}

static SCM g_recorder_autoload(void) {RTNBOOL(recorder_autoload(state));}
static SCM g_set_recorder_autoload(SCM val) 
{
  #define H_recorder_autoload "(" S_recorder_autoload ") -> #t if newly recorded sound should be loaded into Snd automatically"
  #define H_set_recorder_autoload "(" S_set_recorder_autoload " &optional (val #t)) sets " S_recorder_autoload
  ERRB1(val,S_set_recorder_autoload); 
  set_autoload(state,bool_int_or_one(val));
  RTNBOOL(recorder_autoload(state));
}

static SCM g_recorder_buffer_size(void) {RTNINT(recorder_buffer_size(state));}
static SCM g_set_recorder_buffer_size(SCM val) 
{
  #define H_recorder_buffer_size "(" S_recorder_buffer_size ") -> ADC buffer size (4096)"
  #define H_set_recorder_buffer_size "(" S_set_recorder_buffer_size " val) sets " S_recorder_buffer_size
  ERRN1(val,S_set_recorder_buffer_size); 
  in_set_recorder_buffer_size(state,g_scm2int(val));
  RTNINT(recorder_buffer_size(state));
}

static SCM g_recorder_file(void) {RTNSTR(recorder_file(state));}
static SCM g_set_recorder_file(SCM val) 
{
  #define H_recorder_file "(" S_recorder_file ") -> default recorder file name"
  #define H_set_recorder_file "(" S_set_recorder_file " val) sets " S_recorder_file
  ERRS1(val,S_set_recorder_file); 
  in_set_recorder_file(state,gh_scm2newstr(val,0));
  RTNSTR(recorder_file(state));
}

static SCM g_recorder_in_format(void) {RTNINT(recorder_in_format(state));}
static SCM g_set_recorder_in_format(SCM val) 
{
  #define H_recorder_in_format "(" S_recorder_in_format ") -> default recorder incoming data format (16 bit linear)"
  #define H_set_recorder_in_format "(" S_set_recorder_in_format " val) sets " S_recorder_in_format
  ERRN1(val,S_set_recorder_in_format); 
  in_set_recorder_in_format(state,g_scm2int(val));
  RTNINT(recorder_in_format(state));
}

static SCM g_recorder_out_chans(void) {RTNINT(recorder_out_chans(state));}
static SCM g_set_recorder_out_chans(SCM val) 
{
  #define H_recorder_out_chans "(" S_recorder_out_chans ") -> default recorder output channels (2)"
  #define H_set_recorder_out_chans "(" S_set_recorder_out_chans " val) sets " S_recorder_out_chans
  ERRN1(val,S_set_recorder_out_chans); 
  in_set_recorder_out_chans(state,g_scm2int(val));
  RTNINT(recorder_out_chans(state));
}

static SCM g_recorder_out_format(void) {RTNINT(recorder_out_format(state));}
static SCM g_set_recorder_out_format(SCM val) 
{
  #define H_recorder_out_format "(" S_recorder_out_format ") -> default recorder output data format (16-bit linear)"
  #define H_set_recorder_out_format "(" S_set_recorder_out_format " val) sets " S_recorder_out_format
  ERRN1(val,S_set_recorder_out_format); 
  in_set_recorder_out_format(state,g_scm2int(val));
  RTNINT(recorder_out_format(state));
}

static SCM g_recorder_srate(void) {RTNINT(recorder_srate(state));}
static SCM g_set_recorder_srate(SCM val) 
{
  #define H_recorder_srate "(" S_recorder_srate ") -> default recorder sampling rate (22050)"
  #define H_set_recorder_srate "(" S_set_recorder_srate " val) sets " S_recorder_srate
  ERRN1(val,S_set_recorder_srate); 
  set_recorder_srate(state,g_scm2int(val));
  RTNINT(recorder_srate(state));
}

static SCM g_recorder_trigger(void) {RTNFLT(recorder_trigger(state));}
static SCM g_set_recorder_trigger(SCM val) 
{
  #define H_recorder_trigger "(" S_recorder_trigger ") -> if doing triggered record, min amp that can trigger recording"
  #define H_set_recorder_trigger "(" S_set_recorder_trigger " val) sets " S_recorder_trigger
  ERRN1(val,S_set_recorder_trigger); 
  set_recorder_trigger(state,gh_scm2double(val));
  RTNFLT(recorder_trigger(state));
}

static SCM g_recorder_max_duration(void) {RTNFLT(recorder_max_duration(state));}
static SCM g_set_recorder_max_duration(SCM val) 
{
  #define H_recorder_max_duration "(" S_recorder_max_duration ") -> max recorder output file length"
  #define H_set_recorder_max_duration "(" S_set_recorder_max_duration " val) sets " S_recorder_max_duration
  ERRN1(val,S_set_recorder_max_duration); 
  set_recorder_max_duration(state,gh_scm2double(val));
  RTNFLT(recorder_max_duration(state));
}

static SCM g_reverb_decay(void) {RTNFLT(reverb_decay(state));}
static SCM g_set_reverb_decay(SCM val) 
{
  #define H_reverb_decay "(" S_reverb_decay ") -> 'Apply' button reverb decay time (1.0 seconds)"
  #define H_set_reverb_decay "(" S_set_reverb_decay " val) sets " S_reverb_decay
  ERRN1(val,S_set_reverb_decay); 
  set_reverb_decay(state,gh_scm2double(val));
  RTNFLT(reverb_decay(state));
}

static SCM g_save_state_on_exit(void) {RTNBOOL(save_state_on_exit(state));}
static SCM g_set_save_state_on_exit(SCM val) 
{
  #define H_save_state_on_exit "(" S_save_state_on_exit ") -> #t if Snd should save its current state upon exit"
  #define H_set_save_state_on_exit "(" S_set_save_state_on_exit " &optional (val #t)) sets " S_save_state_on_exit
  ERRB1(val,S_set_save_state_on_exit); 
  set_save_state_on_exit(state,bool_int_or_one(val));
  RTNBOOL(save_state_on_exit(state));
}

static SCM g_save_state_file(void) {RTNSTR(save_state_file(state));}
static SCM g_set_save_state_file(SCM val) 
{
  #define H_save_state_file "(" S_save_state_file ") -> name of saved state file (\"saved-snd.scm\")"
  #define H_set_save_state_file "(" S_set_save_state_file " val) sets " S_save_state_file
  ERRS1(val,S_set_save_state_file); 
  set_save_state_file(state,gh_scm2newstr(val,0));
  RTNSTR(save_state_file(state));
}

static SCM g_show_fft_peaks(void) {RTNBOOL(show_fft_peaks(state));}
static SCM g_set_show_fft_peaks(SCM val) 
{
  #define H_show_fft_peaks "(" S_show_fft_peaks ") -> #t if fft display should include peak list"
  #define H_set_show_fft_peaks "(" S_set_show_fft_peaks " &optional (val #t)) sets " S_show_fft_peaks
  ERRB1(val,S_set_show_fft_peaks); 
  set_show_fft_peaks(state,bool_int_or_one(val));
  RTNBOOL(show_fft_peaks(state));
}

static SCM g_show_marks(void) {RTNBOOL(show_marks(state));}
static SCM g_set_show_marks(SCM on) 
{
  #define H_show_marks "(" S_show_marks ") -> #t if Snd should show marks"
  #define H_set_show_marks "(" S_set_show_marks " &optional (val #t)) sets " S_show_marks
  ERRB1(on,S_set_show_marks); 
  set_show_marks(state,bool_int_or_one(on));
  RTNBOOL(show_marks(state));
}

static SCM g_show_usage_stats(void) {RTNBOOL(show_usage_stats(state));}
static SCM g_set_show_usage_stats(SCM on) 
{
  #define H_show_usage_stats "(" S_show_usage_stats ") -> #t if Snd should display memory usage stats"
  #define H_set_show_usage_stats "(" S_set_show_usage_stats " &optional (val #t)) sets " S_show_usage_stats
  ERRB1(on,S_set_show_usage_stats); 
  set_show_usage_stats(state,bool_int_or_one(on));
  RTNBOOL(show_usage_stats(state));
}

static SCM g_update_usage_stats(void) 
{
  #define H_update_usage_stats "(" S_update_usage_stats ") causes the stats display to be made current"
  update_stats(state); 
  return(SCM_BOOL_T);
}

static SCM g_show_mix_consoles(void) {RTNBOOL(show_mix_consoles(state));}
static SCM g_set_show_mix_consoles(SCM on) 
{
  #define H_show_mix_consoles "(" S_show_mix_consoles ") -> #t if Snd shoul display mix consoles"
  #define H_set_show_mix_consoles "(" S_set_show_mix_consoles " &optional (val #t)) sets " S_show_mix_consoles
  ERRB1(on,S_set_show_mix_consoles); 
  set_show_mix_consoles(state,bool_int_or_one(on));
  RTNBOOL(show_mix_consoles(state));
}

static SCM g_show_mix_waveforms(void) {RTNBOOL(show_mix_waveforms(state));}
static SCM g_set_show_mix_waveforms(SCM on) 
{
  #define H_show_mix_waveforms "(" S_show_mix_waveforms ") -> #t if Snd should display mix waveforms"
  #define H_set_show_mix_waveforms "(" S_set_show_mix_waveforms " &optional (val #t)) sets " S_show_mix_waveforms
  ERRB1(on,S_set_show_mix_waveforms); 
  set_show_mix_waveforms(state,bool_int_or_one(on));
  RTNBOOL(show_mix_waveforms(state));
}

static SCM g_show_y_zero(void) {RTNBOOL(show_y_zero(state));}
static SCM g_set_show_y_zero(SCM on) 
{
  #define H_show_y_zero "(" S_show_y_zero ") -> #t if Snd should include a line at y=0.0"
  #define H_set_show_y_zero "(" S_set_show_y_zero " &optional (val #t)) sets " S_show_y_zero
  ERRB1(on,S_set_show_y_zero); 
  set_show_y_zero(state,bool_int_or_one(on));
  RTNBOOL(show_y_zero(state));
}

static SCM g_show_axes(void) {RTNBOOL(show_axes(state));}
static SCM g_set_show_axes(SCM on) 
{
  #define H_show_axes "(" S_show_axes ") -> #t if Snd should display axes"
  #define H_set_show_axes "(" S_set_show_axes " &optional (val #t)) sets " S_show_axes
  ERRB1(on,S_set_show_axes); 
  set_show_axes(state,bool_int_or_one(on));
  RTNBOOL(show_axes(state));
}

static SCM g_sinc_width(void) {RTNINT(sinc_width(state));}
static SCM g_set_sinc_width(SCM val) 
{
  #define H_sinc_width "(" S_sinc_width ") -> sampling rate conversion sinc width (10)\n\
   The higher this number, the better the src low-pass filter, but the slower\n\
   src runs.  If you use too low a setting, you can sometimes hear high\n\
   frequency whistles leaking through."

  #define H_set_sinc_width "(" S_set_sinc_width " val) sets " S_sinc_width
  ERRN1(val,S_set_sinc_width); 
  set_sinc_width(state,g_scm2int(val));
  RTNINT(sinc_width(state));
}

static SCM g_color_map(void) {RTNINT(color_map(state));}
static SCM g_set_color_map(SCM val) 
{
  #define H_colormap "(" S_colormap ") -> current colormap choice\n\
   This should be an integer between -1 and 15.  The maps (from 0 to 15) are:\n\
   gray, hsv, hot, cool, bone, copper, pink, jet, prism, autumn, winter,\n\
   spring, summer, colorcube, flag, and lines.  -1 means black and white."

  #define H_set_colormap "(" S_set_colormap " val) sets " S_colormap
  ERRN1(val,S_set_colormap); 
  set_color_map(state,g_scm2int(val));
  RTNINT(color_map(state));
}

static SCM g_spectro_cutoff(void) {RTNFLT(spectro_cutoff(state));}
static SCM g_set_spectro_cutoff(SCM val) 
{
  #define H_spectro_cutoff "(" S_spectro_cutoff ") -> amount of frequency shown in spectra (1.0)"
  #define H_set_spectro_cutoff "(" S_set_spectro_cutoff " val) sets " S_spectro_cutoff
  ERRN1(val,S_set_spectro_cutoff); 
  set_spectro_cutoff(state,fclamp(0.0,gh_scm2double(val),1.0));
  RTNFLT(spectro_cutoff(state));
}

static SCM g_spectro_start(void) {RTNFLT(spectro_start(state));}
static SCM g_set_spectro_start(SCM val) 
{
  #define H_spectro_start "(" S_spectro_start ") -> lower bound of frequency in spectral displays (0.0)"
  #define H_set_spectro_start "(" S_set_spectro_start " val) sets " S_spectro_start
  ERRN1(val,S_set_spectro_start); 
  set_spectro_start(state,fclamp(0.0,gh_scm2double(val),1.0));
  RTNFLT(spectro_start(state));
}

static SCM g_spectro_hop(void) {RTNINT(spectro_hop(state));}
static SCM g_set_spectro_hop(SCM val) 
{
  #define H_spectro_hop "(" S_spectro_hop ") -> hop amount (pixels) in spectral displays"
  #define H_set_spectro_hop "(" S_set_spectro_hop " val) sets " S_spectro_hop
  ERRN1(val,S_set_spectro_hop); 
  set_spectro_hop(state,g_scm2int(val));
  RTNINT(spectro_hop(state));
}

static SCM g_spectro_x_angle(void) {RTNFLT(spectro_x_angle(state));}
static SCM g_set_spectro_x_angle(SCM val) 
{
  #define H_spectro_x_angle "(" S_spectro_x_angle ") -> spectrogram x-axis viewing angle (90.0)"
  #define H_set_spectro_x_angle "(" S_set_spectro_x_angle " val) sets " S_spectro_x_angle
  ERRN1(val,S_set_spectro_x_angle); 
  set_spectro_x_angle(state,gh_scm2double(val));
  RTNFLT(spectro_x_angle(state));
}

static SCM g_spectro_x_scale(void) {RTNFLT(spectro_x_scale(state));}
static SCM g_set_spectro_x_scale(SCM val) 
{
  #define H_spectro_x_scale "(" S_spectro_x_scale ") -> scaler (stretch) along the spectrogram x axis (1.0)"
  #define H_set_spectro_x_scale "(" S_set_spectro_x_scale " val) sets " S_spectro_x_scale
  ERRN1(val,S_set_spectro_x_scale); 
  set_spectro_x_scale(state,gh_scm2double(val));
  RTNFLT(spectro_x_scale(state));
}

static SCM g_spectro_y_angle(void) {RTNFLT(spectro_y_angle(state));}
static SCM g_set_spectro_y_angle(SCM val) 
{
  #define H_spectro_y_angle "(" S_spectro_y_angle ") -> spectrogram y-axis viewing angle (0.0)"
  #define H_set_spectro_y_angle "(" S_set_spectro_y_angle " val) sets " S_spectro_y_angle
  ERRN1(val,S_set_spectro_y_angle); 
  set_spectro_y_angle(state,gh_scm2double(val));
  RTNFLT(spectro_y_angle(state));
}

static SCM g_spectro_y_scale(void) {RTNFLT(spectro_y_scale(state));}
static SCM g_set_spectro_y_scale(SCM val) 
{
  #define H_spectro_y_scale "(" S_spectro_y_scale ") -> scaler (stretch) along the spectrogram y axis (1.0)"
  #define H_set_spectro_y_scale "(" S_set_spectro_y_scale " val) sets " S_spectro_y_scale
  ERRN1(val,S_set_spectro_y_scale); 
  set_spectro_y_scale(state,gh_scm2double(val));
  RTNFLT(spectro_y_scale(state));
}

static SCM g_spectro_z_angle(void) {RTNFLT(spectro_z_angle(state));}
static SCM g_set_spectro_z_angle(SCM val) 
{
  #define H_spectro_z_angle "(" S_spectro_z_angle ") -> spectrogram z-axis viewing angle (-2.0)"
  #define H_set_spectro_z_angle "(" S_set_spectro_z_angle " val) sets " S_spectro_z_angle
  ERRN1(val,S_set_spectro_z_angle); 
  set_spectro_z_angle(state,gh_scm2double(val));
  RTNFLT(spectro_z_angle(state));
}

static SCM g_spectro_z_scale(void) {RTNFLT(spectro_z_scale(state));}
static SCM g_set_spectro_z_scale(SCM val) 
{
  #define H_spectro_z_scale "(" S_spectro_z_scale ") -> scaler (stretch) along the spectrogram z axis (0.1)"
  #define H_set_spectro_z_scale "(" S_set_spectro_z_scale " val) sets " S_spectro_z_scale
  ERRN1(val,S_set_spectro_z_scale); 
  set_spectro_z_scale(state,gh_scm2double(val));
  RTNFLT(spectro_z_scale(state));
}

static SCM g_speed_style(void) {RTNINT(speed_style(state));}
static SCM g_set_speed_style(SCM speed) 
{
  #define H_speed_style "(" S_speed_style ") -> speed control panel interpretation choice (speed-as-float)"
  #define H_set_speed_style "(" S_set_speed_style " val) sets " S_speed_style
  ERRN1(speed,S_set_speed_style); 
  activate_speed_in_menu(state,iclamp(SPEED_AS_FLOAT,g_scm2int(speed),SPEED_AS_SEMITONE));
  RTNINT(speed_style(state));
}

static SCM g_speed_tones(void) {RTNINT(speed_tones(state));}
static SCM g_set_speed_tones(SCM val) 
{
  #define H_speed_tones "(" S_speed_tones ") -> if speed-style is speed-as-semitone, this chooses the octave divisions (12)"
  #define H_set_speed_tones "(" S_set_speed_tones " val) sets " S_speed_tones
  ERRN1(val,S_set_speed_tones); 
  set_speed_tones(state,g_scm2int(val));
  RTNINT(speed_tones(state));
}

static SCM g_temp_dir(void) {RTNSTR(temp_dir(state));}
static SCM g_set_temp_dir(SCM val) 
{
  #define H_temp_dir "(" S_temp_dir ") -> name of directory for temp files"
  #define H_set_temp_dir "(" S_set_temp_dir " val) sets " S_temp_dir
  ERRS1(val,S_set_temp_dir); 
  set_temp_dir(state,gh_scm2newstr(val,0));
  RTNSTR(temp_dir(state));
}

static SCM g_save_dir(void) {RTNSTR(save_dir(state));}
static SCM g_set_save_dir(SCM val) 
{
  #define H_save_dir "(" S_save_dir ") -> name of directory for saved state data"
  #define H_set_save_dir "(" S_set_save_dir " val) sets " S_save_dir
  ERRS1(val,S_set_save_dir); 
  set_save_dir(state,gh_scm2newstr(val,0));
  RTNSTR(save_dir(state));
}

static SCM g_transform_type(void) {RTNINT(transform_type(state));}
static SCM g_set_transform_type(SCM val) 
{
  #define H_transform_type "(" S_transform_type ") -> transform type, e.g. fourier-transform"
  #define H_set_transform_type "(" S_set_transform_type " val) sets " S_transform_type
  ERRN1(val,S_set_transform_type); 
  set_transform_type(state,g_scm2int(val));
  RTNINT(transform_type(state));
}

static SCM g_trap_segfault(void) {RTNBOOL(trap_segfault(state));}
static SCM g_set_trap_segfault(SCM val) 
{
  #define H_trap_segfault "(" S_trap_segfault ") -> #t if Snd should try to trap (and whine about) segfaults"
  #define H_set_trap_segfault "(" S_set_trap_segfault " &optional (val #t)) sets " S_trap_segfault
  ERRB1(val,S_set_trap_segfault); 
  set_trap_segfault(state,bool_int_or_one(val));
  RTNBOOL(trap_segfault(state));
}

static SCM g_show_selection_transform(void) {RTNBOOL(show_selection_transform(state));}
static SCM g_set_show_selection_transform(SCM val) 
{
  #define H_show_selection_transform "(" S_show_selection_transform ") -> #t if transform display reflects selection, not time-domain window"
  #define H_set_show_selection_transform "(" S_set_show_selection_transform " &optional (val #t)) sets " S_show_selection_transform
  ERRB1(val,S_set_show_selection_transform); 
  set_show_selection_transform(state,bool_int_or_one(val));
  RTNBOOL(show_selection_transform(state));
}

static SCM g_with_mix_consoles(void) {RTNBOOL(with_mix_consoles(state));}
static SCM g_set_with_mix_consoles(SCM val) 
{
  #define H_with_mix_consoles "(" S_with_mix_consoles ") -> #t if Snd should create consoles alongside mixing operations"
  #define H_set_with_mix_consoles "(" S_set_with_mix_consoles " &optional (val #t)) sets " S_with_mix_consoles
  ERRB1(val,S_set_with_mix_consoles); 
  set_with_mix_consoles(state,bool_int_or_one(val));
  RTNBOOL(with_mix_consoles(state));
}

static SCM g_use_raw_defaults(void) {RTNBOOL(use_raw_defaults(state));}
static SCM g_set_use_raw_defaults(SCM val) 
{
  #define H_use_raw_defaults "(" S_use_raw_defaults ") -> #t if Snd should simply use the raw-* defaults\n\
   when a headerless file is encountered. If #f, Snd fires up the raw file dialog."

  #define H_set_use_raw_defaults "(" S_set_use_raw_defaults " &optional (val #t)) sets " S_use_raw_defaults
  ERRB1(val,S_set_use_raw_defaults); 
  set_use_raw_defaults(state,bool_int_or_one(val));
  RTNBOOL(use_raw_defaults(state));
}

static SCM g_use_sinc_interp(void) {RTNBOOL(use_sinc_interp(state));}
static SCM g_set_use_sinc_interp(SCM val) 
{
  #define H_use_sinc_interp "(" S_use_sinc_interp ") -> #t if Snd should use convolution with sinc for sampling rate\n\
   conversion.  The other choice is (much faster) linear interpolation which can introduce distortion"

  #define H_set_use_sinc_interp "(" S_set_use_sinc_interp " &optional (val #t)) sets " S_use_sinc_interp
  ERRB1(val,S_set_use_sinc_interp); 
  set_use_sinc_interp(state,bool_int_or_one(val));
  RTNBOOL(use_sinc_interp(state));
}

static SCM g_data_clipped(void) {RTNBOOL(data_clipped(state));}
static SCM g_set_data_clipped(SCM val) 
{
  #define H_data_clipped "(" S_data_clipped ") -> #t if Snd should clip output values to the current\n\
   output data format's maximum. The default (#f) allows them to wrap-around which makes a very loud click"

  #define H_set_data_clipped "(" S_set_data_clipped " &optional (val #t)) sets " S_data_clipped
  ERRB1(val,S_set_data_clipped); 
  set_data_clipped(state,bool_int_or_one(val));
  RTNBOOL(data_clipped(state));
}

static SCM g_verbose_cursor(void) {RTNBOOL(verbose_cursor(state));}
static SCM g_set_verbose_cursor(SCM on) 
{
  #define H_verbose_cursor "(" S_verbose_cursor ") -> #t if the cursor's position and so on is displayed in the minibuffer"
  #define H_set_verbose_cursor "(" S_set_verbose_cursor " &optional (val #t)) sets " S_verbose_cursor
  ERRB1(on,S_set_verbose_cursor); 
  set_verbose_cursor(state,bool_int_or_one(on));
  RTNBOOL(verbose_cursor(state));
}

static SCM g_vu_font(void) {RTNSTR(vu_font(state));}
static SCM g_set_vu_font(SCM val) 
{
  #define H_vu_font "(" S_vu_font ") -> name of font used to make VU meter labels (courier)"
  #define H_set_vu_font "(" S_set_vu_font " val) sets " S_vu_font
  ERRS1(val,S_set_vu_font); 
  set_vu_font(state,gh_scm2newstr(val,0));
  RTNSTR(vu_font(state));
}

static SCM g_vu_font_size(void) {RTNFLT(vu_font_size(state));}
static SCM g_set_vu_font_size(SCM val) 
{
  #define H_vu_font_size "(" S_vu_font_size ") -> size of VU font meter labels (1.0)"
  #define H_set_vu_font_size "(" S_set_vu_font_size " val) sets " S_vu_font_size
  ERRN1(val,S_set_vu_font_size); 
  set_vu_font_size(state,gh_scm2double(val));
  RTNFLT(vu_font_size(state));
}

static SCM g_vu_size(void) {RTNFLT(vu_size(state));}
static SCM g_set_vu_size(SCM val) 
{
  #define H_vu_size "(" S_vu_size ") -> size of VU meters (1.0)"
  #define H_set_vu_size "(" S_set_vu_size " val) sets " S_vu_size
  ERRN1(val,S_set_vu_size); 
  set_vu_size(state,gh_scm2double(val));
  RTNFLT(vu_size(state));
}

static SCM g_wavelet_type(void) {RTNINT(wavelet_type(state));}
static SCM g_set_wavelet_type(SCM val) 
{
  #define H_wavelet_type "(" S_wavelet_type ") -> wavelet used in wavelet-transform (0)"
  #define H_set_wavelet_type "(" S_set_wavelet_type " val) sets " S_wavelet_type
  ERRN1(val,S_set_wavelet_type); 
  set_wavelet_type(state,g_scm2int(val));
  RTNINT(wavelet_type(state));
}

static SCM g_wavo(void) {RTNBOOL(wavo(state));}
static SCM g_set_wavo(SCM val) 
{
  #define H_wavo "(" S_wavo ") -> #t if Snd's time domain display is a 'wavogram'"
  #define H_set_wavo "(" S_set_wavo " &optional (val #t)) sets " S_wavo
  ERRB1(val,S_set_wavo); 
  set_wavo(state,bool_int_or_one(val));
  RTNBOOL(wavo(state));
}

static SCM g_wavo_hop(void) {RTNINT(wavo_hop(state));}
static SCM g_set_wavo_hop(SCM val) 
{
  #define H_wavo_hop "(" S_wavo_hop ") -> wavogram spacing between successive traces"
  #define H_set_wavo_hop "(" S_set_wavo_hop " val) sets " S_wavo_hop
  ERRN1(val,S_set_wavo_hop); 
  set_wavo_hop(state,g_scm2int(val));
  RTNINT(wavo_hop(state));
}

static SCM g_wavo_trace(void) {RTNINT(wavo_trace(state));}
static SCM g_set_wavo_trace(SCM val) 
{
  #define H_wavo_trace "(" S_wavo_trace ") -> length (samples) of each trace in the wavogram (64)"
  #define H_set_wavo_trace "(" S_set_wavo_trace " val) sets " S_wavo_trace
  ERRN1(val,S_set_wavo_trace); 
  set_wavo_trace(state,g_scm2int(val));
  RTNINT(wavo_trace(state));
}

static SCM g_x_axis_style(void) {RTNINT(x_axis_style(state));}
static SCM g_set_x_axis_style(SCM val) 
{
  #define H_x_axis_style "(" S_x_axis_style ") -> labelling of time domain x axis (x-in-seconds)"
  #define H_set_x_axis_style "(" S_set_x_axis_style " val) sets " S_x_axis_style
  ERRN1(val,S_set_x_axis_style); 
  set_x_axis_style(state,iclamp(X_IN_SECONDS,g_scm2int(val),X_IN_LENGTH));
  RTNINT(x_axis_style(state));
}

static SCM g_xmax(void) {RTNFLT(xmax(state));}
static SCM g_set_xmax(SCM val) 
{
  #define H_xmax "(" S_xmax ") -> x axis max (truncates sound if necessary)"
  #define H_set_xmax "(" S_set_xmax " val) sets " S_xmax
  ERRN1(val,S_set_xmax); 
  set_xmax(state,gh_scm2double(val));
  RTNFLT(xmax(state));
}

static SCM g_xmin(void) {RTNFLT(xmin(state));}
static SCM g_set_xmin(SCM val) 
{
  #define H_xmin "(" S_xmin ") -> x axis min"
  #define H_set_xmin "(" S_set_xmin " val) sets " S_xmin
  ERRN1(val,S_set_xmin); 
  set_xmin(state,gh_scm2double(val));
  RTNFLT(xmin(state));
}

static SCM g_ymax(void) {RTNFLT(ymax(state));}
static SCM g_set_ymax(SCM val)
{
  #define H_ymax "(" S_ymax ") -> y axis max (used to narrow the slider range)"
  #define H_set_ymax "(" S_set_ymax " val) sets " S_ymax
  ERRN1(val,S_set_ymax); 
  set_ymax(state,gh_scm2double(val));
  RTNFLT(ymax(state));
}

static SCM g_ymin(void) {RTNFLT(ymin(state));}
static SCM g_set_ymin(SCM val) 
{
  #define H_ymin "(" S_ymin ") -> y axis min"
  #define H_set_ymin "(" S_set_ymin " val) sets " S_ymin
  ERRN1(val,S_set_ymin); 
  set_ymin(state,gh_scm2double(val));
  RTNFLT(ymin(state));
}

static SCM g_zero_pad(void) {RTNINT(zero_pad(state));}
static SCM g_set_zero_pad(SCM val) 
{
  #define H_zero_pad "(" S_zero_pad ") -> zero padding used in fft as a multiple of fft size (0)"
  #define H_set_zero_pad "(" S_set_zero_pad " val) sets " S_zero_pad
  ERRB1(val,S_set_zero_pad); 
  set_zero_pad(state,bool_int_or_one(val)); 
  RTNINT(zero_pad(state));
}

static SCM g_zoom_focus_style(void) {RTNINT(zoom_focus_style(state));}
static SCM g_set_zoom_focus_style(SCM focus) 
{
  #define H_zoom_focus_style "(" S_zoom_focus_style ") -> what zooming centers on (focus-active)"
  #define H_set_zoom_focus_style "(" S_set_zoom_focus_style " val) sets " S_zoom_focus_style
  ERRN1(focus,S_set_zoom_focus_style); 
  activate_focus_menu(state,iclamp(FOCUS_LEFT,g_scm2int(focus),FOCUS_MIDDLE));
  RTNINT(zoom_focus_style(state));
}

static SCM g_set_just_sounds(SCM on) 
{
  #define H_set_just_sounds "(" S_set_just_sounds " &optional (val #t)) sets the 'just sounds' button in the file chooser dialog"
  int n;
  ERRB1(on,S_set_just_sounds); 
  toggle_just_sounds(n = bool_int_or_one(on)); 
  RTNBOOL(n);
}

#if ((USE_MOTIF) && (XmVERSION == 1))
  static SCM g_edit_history_width(void) {RTNINT(edit_history_width(state));}
  static SCM g_set_edit_history_width(SCM val) 
  {
    #define H_edit_history_width "(" S_edit_history_width ") -> width in pixels of each channel's edit history list"
    #define H_set_edit_history_width "(" S_set_edit_history_width " val) sets " S_edit_history_width
    ERRN1(val,S_set_edit_history_width); 
    set_edit_history_width(state,g_scm2int(val));
    RTNINT(edit_history_width(state));
  }
#endif

static SCM g_recorder_gain (SCM num) 
{
  #define H_recorder_gain "(" S_recorder_gain " gain) -> recorder input (soundcard) gain"
  RTNFLT(read_record_state(AUDIO_GAINS,g_scm2int(num),0));
}

static SCM g_recorder_in_amp (SCM in, SCM out) 
{
  #define H_recorder_in_amp "(" S_recorder_in_amp " in out) -> recorder scaler on input in to output out"
  RTNFLT(read_record_state(REC_IN_AMPS,g_scm2int(in),g_scm2int(out)));
}

static SCM g_recorder_out_amp (SCM num) 
{
  #define H_recorder_out_amp "(" S_recorder_out_amp " out) -> recorder output out scaler"
  RTNFLT(read_record_state(REC_OUT_AMPS,g_scm2int(num),0));
}

static SCM g_set_recorder_gain (SCM num, SCM amp) 
{
  #define H_set_recorder_gain "(" S_set_recorder_gain " num amp) sets recorder input gain num to amp"
  ERRN1(num,S_set_recorder_gain);
  ERRN2(amp,S_set_recorder_gain); 
  write_record_state(AUDIO_GAINS,g_scm2int(num),0,gh_scm2double(amp)); 
  return(amp);
}

static SCM g_set_recorder_in_amp (SCM in, SCM out, SCM amp) 
{
  #define H_set_recorder_in_amp "(" S_set_recorder_in_amp " in out amp) sets recorder scaler on input in to output out to amp"
  ERRN1(in,S_set_recorder_in_amp);
  ERRN2(out,S_set_recorder_in_amp);
  ERRN3(amp,S_set_recorder_in_amp);
  write_record_state(REC_IN_AMPS,g_scm2int(in),g_scm2int(out),gh_scm2double(amp)); 
  return(amp);
}

static SCM g_set_recorder_out_amp (SCM num, SCM amp) 
{
  #define H_set_recorder_out_amp "(" S_set_recorder_out_amp " num amp) sets recorder output gain num to amp"
  ERRN1(num,S_set_recorder_out_amp);
  ERRN2(amp,S_set_recorder_out_amp); 
  write_record_state(REC_OUT_AMPS,g_scm2int(num),0,gh_scm2double(amp)); 
  return(amp);
}

static SCM g_graph2ps(void) 
{
  #define H_graph2ps "(" S_graph_ps ") writes the current Snd displays to an EPS file"
  snd_print(state,eps_file(state),1); 
  RTNSTR(eps_file(state));
}

static SCM g_version(void) 
{
  #define H_version "(" S_version ") -> current Snd version"
  RTNSTR(SND_VERSION);
}

static SCM g_save_state(SCM filename) 
{
  #define H_save_state "(" S_save_state " filename) saves the current Snd state in filename; (load filename) restores it)"
  if (save_state(state,gh_scm2newstr(filename,NULL)) == -1) 
    return(SCM_BOOL_F); 
  else return(filename);
}

static SCM g_save_macros(void) 
{
  #define H_save_macros "(" S_save_macros ") saves keyboard macros in Snd's init file (.snd)"
  save_macros(state); 
  return(SCM_BOOL_F);
}

static SCM g_max_sounds(void) 
{
  #define H_max_sounds "(" S_max_sounds ") -> max sound id currently possible (grows as necessary)"
  RTNINT(state->max_sounds);
}

static SCM g_max_regions(void) 
{
  #define H_max_regions "(" S_max_regions ") -> max number of regions saved on the region list"
  RTNINT(max_regions(state));
}

static SCM g_set_max_regions(SCM n) 
{
  #define H_set_max_regions "(" S_set_max_regions " val) sets the max length of the region list"
  ERRN1(n,S_set_max_regions); 
  set_max_regions(state,g_scm2int(n));
  RTNINT(max_regions(state));
}

static SCM g_max_fft_peaks(void) 
{
  #define H_max_fft_peaks "(" S_max_fft_peaks ") -> max number of fft peaks reported in fft display"
  RTNINT(max_fft_peaks(state));
}

static SCM g_set_max_fft_peaks(SCM n) 
{
  #define H_set_max_fft_peaks "(" S_set_max_fft_peaks " val) sets " S_max_fft_peaks
  ERRN1(n,S_set_max_fft_peaks); 
  set_max_fft_peaks(state,g_scm2int(n));
  RTNINT(max_fft_peaks(state));
}

static SCM g_normalize_view(void) 
{
  #define H_normalize_view "(" S_normalize_view ") causes Snd to try to give all channels about the same screen space"
  normalize_all_sounds(state); 
  return(SCM_BOOL_F);
}

static SCM g_show_listener(void) 
{
  #define H_show_listener "(" S_show_listener ") opens the lisp listner pane"
  if (state->listening != LISTENER_OPEN) handle_listener(state,LISTENER_OPEN); 
  return(SCM_BOOL_F);
}

static SCM g_hide_listener(void) 
{
  #define H_hide_listener "(" S_hide_listener ") closes the lisp listener pane"
  if (state->listening == LISTENER_OPEN) handle_listener(state,LISTENER_LISTENING); 
  return(SCM_BOOL_F);
}

static SCM g_activate_listener(void) 
{
  #define H_activate_listener "(" S_activate_listener ") makes the listener active, if not open"
  handle_listener(state,LISTENER_LISTENING); 
  state->listening = LISTENER_LISTENING; 
  return(SCM_BOOL_F);
}

static SCM g_min_dB(void) {RTNFLT(state->min_dB);}
static SCM g_set_min_dB(SCM val) 
{
  #define H_min_dB "(" S_min_dB ") -> min dB value displayed in fft graphs using dB scales"
  #define H_set_min_dB "(" S_set_min_dB " val) sets " S_min_dB
  ERRN1(val,S_set_min_dB); 
  set_min_dB(state,gh_scm2double(val));
  RTNFLT(state->min_dB);
}

static SCM g_help_text_font(void) {RTNSTR(help_text_font(state));}
static SCM g_set_help_text_font(SCM val) 
{
  #define H_help_text_font "(" S_help_text_font ") -> font used in the Help dialog"
  #define H_set_help_text_font "(" S_set_help_text_font " val) sets " S_help_text_font
  ERRS1(val,S_set_help_text_font); 
  set_help_text_font(state,gh_scm2newstr(val,0)); 
  return(val);
}

static SCM g_tiny_font(void) {RTNSTR(tiny_font(state));}
static SCM g_set_tiny_font(SCM val) 
{
  #define H_tiny_font "(" S_tiny_font ") -> font use for some info in the graphs"
  #define H_set_tiny_font "(" S_set_tiny_font " val) sets " S_tiny_font
  ERRS1(val,S_set_tiny_font); 
  set_tiny_font(state,gh_scm2newstr(val,0)); 
  return(val);
}

static SCM g_axis_label_font(void) {RTNSTR(axis_label_font(state));}
static SCM g_set_axis_label_font(SCM val) 
{
  #define H_axis_label_font "(" S_axis_label_font ") -> font used for axis labels"
  #define H_set_axis_label_font "(" S_set_axis_label_font " val) sets " S_axis_label_font
  ERRS1(val,S_set_axis_label_font); 
  set_axis_label_font(state,gh_scm2newstr(val,0)); 
  return(val);
}

static SCM g_axis_numbers_font(void) {RTNSTR(axis_numbers_font(state));}
static SCM g_set_axis_numbers_font(SCM val) 
{
  #define H_axis_numbers_font "(" S_axis_numbers_font ") -> font used for axis numbers"
  #define H_set_axis_numbers_font "(" S_set_axis_numbers_font " val) sets " S_axis_numbers_font
  ERRS1(val,S_set_axis_numbers_font); 
  set_axis_numbers_font(state,gh_scm2newstr(val,0)); 
  return(val);
}

static SCM g_listener_font(void) {RTNSTR(listener_font(state));}
static SCM g_set_listener_font(SCM val) 
{
  #define H_listener_font "(" S_listener_font ") -> font used by the lisp listener"
  #define H_set_listener_font "(" S_set_listener_font " val) sets " S_listener_font
  ERRS1(val,S_set_listener_font);
  set_listener_font(state,gh_scm2newstr(val,0)); 
  return(val);
}

static SCM g_bold_button_font(void) {RTNSTR(bold_button_font(state));}
static SCM g_set_bold_button_font(SCM val) 
{
  #define H_bold_button_font "(" S_bold_button_font ") -> font used by some buttons"
  #define H_set_bold_button_font "(" S_set_bold_button_font " val) sets " S_bold_button_font
  ERRS1(val,S_set_bold_button_font); 
  set_bold_button_font(state,gh_scm2newstr(val,0)); 
  return(val);
}

static SCM g_button_font(void) {RTNSTR(button_font(state));}
static SCM g_set_button_font(SCM val) 
{
  #define H_button_font "(" S_button_font ") -> font used by some buttons"
  #define H_set_button_font "(" S_set_button_font " val) sets " S_button_font
  ERRS1(val,S_set_button_font); 
  set_button_font(state,gh_scm2newstr(val,0)); 
  return(val);
}

static SCM g_window_width(void) 
{
  #define H_window_width "(" S_window_width ") -> current Snd window width in pixels"
  RTNINT(widget_width(MAIN_SHELL(state)));
}

static SCM g_window_height(void) 
{
  #define H_window_height "(" S_window_height ") -> current Snd window height in pixels"
  RTNINT(widget_height(MAIN_SHELL(state)));
}

static SCM g_window_x(void) 
{
  #define H_window_x "(" S_window_x ") -> current Snd window x position in pixels"
  RTNINT(widget_x(MAIN_SHELL(state)));
}

static SCM g_window_y(void) 
{
  #define H_window_y "(" S_window_y ") -> current Snd window y position in pixels"
  RTNINT(widget_y(MAIN_SHELL(state)));
}

static SCM g_set_window_height(SCM height) 
{
  #define H_set_window_height "(" S_set_window_height " val) sets the Snd window height in pixels"
  ERRN1(height,S_set_window_height); 
  set_widget_height(MAIN_SHELL(state),g_scm2int(height));
  state->init_window_height = g_scm2int(height);
  return(height);
}

static SCM g_set_window_width(SCM width) 
{
  #define H_set_window_width "(" S_set_window_width " val) sets the Snd window width in pixels"
  ERRN1(width,S_set_window_width); 
  set_widget_width(MAIN_SHELL(state),g_scm2int(width)); 
  state->init_window_width = g_scm2int(width);
  return(width);
}

static SCM g_set_window_x(SCM val) 
{
  #define H_set_window_x "(" S_set_window_x " val) sets the Snd window x position in pixels"
  ERRN1(val,S_set_window_x); 
  set_widget_x(MAIN_SHELL(state),g_scm2int(val));
  state->init_window_x = g_scm2int(val); 
  return(val);
}

static SCM g_set_window_y(SCM val) 
{
  #define H_set_window_y "(" S_set_window_y " val) sets the Snd window y position in pixels"
  ERRN1(val,S_set_window_y); 
  set_widget_y(MAIN_SHELL(state),g_scm2int(val)); 
  state->init_window_y = g_scm2int(val); 
  return(val);
}

static SCM g_sp_scan(SCM proc, int chan_choice, SCM s_beg, SCM s_end, int series, int scan, SCM org, SCM snd, SCM chn)
{
  snd_state *ss;
  chan_info *cp;
  int beg,end;
  SCM result;
  char *origin=NULL,*ed_origin = NULL;
  if (scan)
    {
      switch (chan_choice)
	{
	case SCAN_CURRENT_CHAN: origin = S_scan_chan; break;
	case SCAN_SOUND_CHANS: if (series) origin = S_scan_sound_chans; else origin = S_scan_across_sound_chans; break;
	case SCAN_ALL_CHANS: if (series) origin = S_scan_all_chans; else origin = S_scan_across_all_chans; break;
	case SCAN_SYNCD_CHANS: if (series) origin = S_scan_chans; else origin = S_scan_across_chans; break;
	}
    }
  else
    {
      switch (chan_choice)
	{
	case SCAN_CURRENT_CHAN: origin = S_map_chan; break;
	case SCAN_SOUND_CHANS: if (series) origin = S_map_sound_chans; else origin = S_map_across_sound_chans; break;
	case SCAN_ALL_CHANS: if (series) origin = S_map_all_chans; else origin = S_map_across_all_chans; break;
	case SCAN_SYNCD_CHANS: if (series) origin = S_map_chans; else origin = S_map_across_chans; break;
	}
      if (gh_string_p(org)) ed_origin = gh_scm2newstr(org,NULL); else ed_origin = copy_string(origin);
    }
  SCM_ASSERT((gh_procedure_p(proc)),proc,SCM_ARG1,origin);
  SCM_ASSERT((bool_or_arg_p(s_beg)),s_beg,SCM_ARG2,origin);
  SCM_ASSERT((bool_or_arg_p(s_end)),s_end,SCM_ARG3,origin);
  ss = get_global_state();
  cp = get_cp(snd,chn);
  if (cp == NULL) return(NO_SUCH_CHANNEL);
  beg = g_scm2intdef(s_beg,0);
  end = g_scm2intdef(s_end,0);
  if (scan)
    {
      if (series)
	return(series_scan(ss,cp,proc,chan_choice,beg,end));
      else return(parallel_scan(ss,cp,proc,chan_choice,beg,end));
    }
  else
    {
      if (series)
	result = series_map(ss,cp,proc,chan_choice,beg,end,ed_origin);
      else result = parallel_map(ss,cp,proc,chan_choice,beg,end,ed_origin);
      if (ed_origin) FREE(ed_origin);
      return(result);
    }
}

static SCM g_scan_chan(SCM proc, SCM beg, SCM end, SCM snd, SCM chn) 
{ 
  #define H_scan_chan "(" S_scan_chan " func &optional (start 0) end snd chn)\n\
   apply func to samples in current channel (or the specified channel)\n\
   func is a function of one argument, either the current sample, or #f (to indicate end-of-data)\n\
   if func returns non-#f, the scan stops, and the value is returned to the caller with the sample number"

  ERRCP(S_scan_chan,snd,chn,4); 
  return(g_sp_scan(proc,SCAN_CURRENT_CHAN,beg,end,TRUE,TRUE,SCM_BOOL_F,snd,chn));
}

static SCM g_scan_chans(SCM proc, SCM beg, SCM end) 
{ 
  #define H_scan_chans "(" S_scan_chans " func &optional (start 0) end)\n\
   apply func to samples in all sync'd channels, one channel after another"
   
  return(g_sp_scan(proc,SCAN_SYNCD_CHANS,beg,end,TRUE,TRUE,SCM_BOOL_F,SCM_BOOL_F,SCM_BOOL_F));
}

static SCM g_scan_all_chans(SCM proc, SCM beg, SCM end) 
{ 
  #define H_scan_all_chans "(" S_scan_all_chans " func &optional (start 0) end)\n\
   apply func to samples in all channels, one after the other"

  return(g_sp_scan(proc,SCAN_ALL_CHANS,beg,end,TRUE,TRUE,SCM_BOOL_F,SCM_BOOL_F,SCM_BOOL_F));
}

static SCM g_scan_sound_chans(SCM proc, SCM beg, SCM end, SCM snd) 
{ 
  #define H_scan_sound_chans "(" S_scan_sound_chans " func &optional (start 0) end snd)\n\
   apply func to samples in all of sound snd's channels"

  ERRSP(S_scan_sound_chans,snd,4); 
  return(g_sp_scan(proc,SCAN_SOUND_CHANS,beg,end,TRUE,TRUE,SCM_BOOL_F,snd,SCM_BOOL_F));
}

static SCM g_scan_across_chans(SCM proc, SCM beg, SCM end) 
{ 
  #define H_scan_across_chans "(" S_scan_across_chans " func &optional (start 0) end)\n\
   apply func to samples in all sync'd channels in parallel"

  return(g_sp_scan(proc,SCAN_SYNCD_CHANS,beg,end,FALSE,TRUE,SCM_BOOL_F,SCM_BOOL_F,SCM_BOOL_F));
}

static SCM g_scan_across_all_chans(SCM proc, SCM beg, SCM end) 
{ 
  #define H_scan_across_all_chans "(" S_scan_across_all_chans " func &optional (start 0) end)\n\
   apply func to samples in all channels in parallel"

  return(g_sp_scan(proc,SCAN_ALL_CHANS,beg,end,FALSE,TRUE,SCM_BOOL_F,SCM_BOOL_F,SCM_BOOL_F));
}

static SCM g_scan_across_sound_chans(SCM proc, SCM beg, SCM end, SCM snd) 
{ 
  #define H_scan_across_sound_chans "(" S_scan_across_sound_chans " func &optional (start 0) end snd)\n\
   apply func to samples in sound snd's channels in parallel"

  ERRSP(S_scan_across_sound_chans,snd,4); 
  return(g_sp_scan(proc,SCAN_SOUND_CHANS,beg,end,FALSE,TRUE,SCM_BOOL_F,snd,SCM_BOOL_F));
}

static SCM g_map_chan(SCM proc, SCM beg, SCM end, SCM org, SCM snd, SCM chn) 
{ 
  #define H_map_chan "(" S_map_chan "func &optional (start 0) end edname snd chn)\n\
   apply func to samples in current channel, edname is the edit history name for this editing operation"

  ERRCP(S_map_chan,snd,chn,5); 
  return(g_sp_scan(proc,SCAN_CURRENT_CHAN,beg,end,TRUE,FALSE,org,snd,chn));
}

static SCM g_map_chans(SCM proc, SCM beg, SCM end, SCM org) 
{ 
  #define H_map_chans "(" S_map_chans "func &optional (start 0) end edname)\n\
   apply func to currently sync'd channels, edname is the edit history name for this editing operation"

  return(g_sp_scan(proc,SCAN_SYNCD_CHANS,beg,end,TRUE,FALSE,org,SCM_BOOL_F,SCM_BOOL_F));
}

static SCM g_map_all_chans(SCM proc, SCM beg, SCM end, SCM org) 
{ 
  #define H_map_all_chans "(" S_map_all_chans "func &optional (start 0) end edname)\n\
    apply func to all channels, edname is the edit history name for this editing operation"

  return(g_sp_scan(proc,SCAN_ALL_CHANS,beg,end,TRUE,FALSE,org,SCM_BOOL_F,SCM_BOOL_F));
}

static SCM g_map_sound_chans(SCM proc, SCM beg, SCM end, SCM org,SCM snd) 
{
  #define H_map_sound_chans "(" S_map_sound_chans "func &optional (start 0) end edname snd)\n\
    apply func to sound snd's channels, edname is the edit history name for this editing operation"

  ERRSP(S_map_sound_chans,snd,5); 
  return(g_sp_scan(proc,SCAN_SOUND_CHANS,beg,end,TRUE,FALSE,org,snd,SCM_BOOL_F));
}

static SCM g_map_across_chans(SCM proc, SCM beg, SCM end, SCM org) 
{
  #define H_map_across_chans "(" S_map_across_chans "func &optional (start 0) end edname)\n\
   apply func to currently sync'd channels in parallel, edname is the edit history name for this editing operation"

  return(g_sp_scan(proc,SCAN_SYNCD_CHANS,beg,end,FALSE,FALSE,org,SCM_BOOL_F,SCM_BOOL_F));
}

static SCM g_map_across_all_chans(SCM proc, SCM beg, SCM end, SCM org) 
{
  #define H_map_across_all_chans "(" S_map_across_all_chans "func &optional (start 0) end edname)\n\
   apply func to all channels in parallel, edname is the edit history name for this editing operation"

  return(g_sp_scan(proc,SCAN_ALL_CHANS,beg,end,FALSE,FALSE,org,SCM_BOOL_F,SCM_BOOL_F));
}

static SCM g_map_across_sound_chans(SCM proc, SCM beg, SCM end, SCM org, SCM snd) 
{
  #define H_map_across_sound_chans "(" S_map_across_sound_chans "func &optional (start 0) end edname snd)\n\
   apply func to sound snd's channels in parallel, edname is the edit history name for this editing operation"

  ERRSP(S_map_across_sound_chans,snd,5); 
  return(g_sp_scan(proc,SCAN_SOUND_CHANS,beg,end,FALSE,FALSE,org,snd,SCM_BOOL_F));
}

static SCM g_exit(void) 
{
  #define H_exit "(" S_exit ") exits Snd"
  if (dont_exit(state)) return(SCM_BOOL_T);
  snd_exit_cleanly(state); 
  snd_exit(1); 
  return(SCM_BOOL_F);
}

static SCM g_abort(void)
{
  #define H_abort "(" S_abort ") drops Snd into gdb, the C debugger"
  abort();
  return(SCM_BOOL_F);
}

static SCM g_dismiss_all_dialogs(void)
{
  #define H_dismiss_all_dialogs "(" S_dismiss_all_dialogs ") closes all active dialogs"
  dismiss_all_dialogs(state);
  return(SCM_BOOL_F);
}

static SCM g_abortq(void)
{
  #define H_abortQ "(" S_abortQ ") allows pending user interface events to occur, returning #t if C-g was typed"
  check_for_event(state);
  if (state->stopped_explicitly)
    {
      state->stopped_explicitly = 0;
      return(SCM_BOOL_T);
    }
  return(SCM_BOOL_F);
}

#if DEBUGGING
static SCM g_display_edits(SCM snd, SCM chn)
{
  #define H_display_edits " prints current edit tree state"
  display_edits(get_cp(snd,chn));
  return(SCM_BOOL_F);
}
#endif

snd_info *get_sp(SCM scm_snd_n)
{
  int snd_n;
  mixdata *md;
  /* if scm_snd_n is a number, it is sp->index
     if it's a list, car is mix id 
  */
  if (gh_number_p(scm_snd_n))
    {
      snd_n = g_scm2int(scm_snd_n);
      if ((snd_n >= 0) && (snd_n < state->max_sounds) && (snd_ok(state->sounds[snd_n])))
	return(state->sounds[snd_n]);
      else
	{
	  /* user asked for specific sound and it's closed or non-existent */
	  return(NULL);
	}
    }
  else
    {
      if (gh_list_p(scm_snd_n))
	{
	  /* a mix input sound */
	  snd_n = g_scm2int(gh_list_ref(scm_snd_n,gh_int2scm(0)));
	  md = md_from_int(snd_n);
	  if (md) return(make_mix_readable(md));
	  return(NULL);
	}
    }
  /* use default sound, if any */
  return(any_selected_sound(state));
}

chan_info *get_cp(SCM scm_snd_n, SCM scm_chn_n)
{
  snd_info *sp;
  int chn_n;
  sp = get_sp(scm_snd_n);
  if (sp) 
    {
      if (gh_number_p(scm_chn_n))
	chn_n = g_scm2int(scm_chn_n);
      else
	if (sp->selected_channel != NO_SELECTION) 
	  chn_n = sp->selected_channel;
	else chn_n = 0;
      if ((chn_n >= 0) && (chn_n < sp->nchans)) 
	return(sp->chans[chn_n]);
    }
  return(NULL);
}


enum {SYNCF,UNITEF,READONLYF,NCHANSF,CONTRASTINGF,EXPANDINGF,REVERBINGF,FILTERINGF,FILTERORDERF,
      SRATEF,DATAFORMATF,DATALOCATIONF,HEADERTYPEF,CONTROLPANELSAVEF,CONTROLPANELRESTOREF,SELECTEDCHANNELF,
      COMMENTF,FILENAMEF,SHORTFILENAMEF,CLOSEF,UPDATEF,SAVEF,CURSORFOLLOWSPLAYF,SHOWCONTROLSF,SAVEMARKSF,
      FILTERDBING};

static SCM sp_iread(SCM snd_n, int fld)
{
  snd_info *sp;
  sp = get_sp(snd_n);
  if (sp == NULL) return(NO_SUCH_SOUND);
  switch (fld)
    {
    case SYNCF: RTNINT(sp->syncing); break;
    case UNITEF: RTNINT(sp->combining); break;
    case READONLYF: RTNBOOL(sp->read_only); break;
    case NCHANSF: RTNINT(sp->nchans); break;
    case EXPANDINGF: RTNBOOL(sp->expanding); break;
    case CONTRASTINGF: RTNBOOL(sp->contrasting); break;
    case REVERBINGF: RTNBOOL(sp->reverbing); break;
    case FILTERINGF: RTNBOOL(sp->filtering); break;
    case FILTERDBING: RTNBOOL(sp->filter_dBing); break;
    case FILTERORDERF: RTNINT(sp->filter_order); break;
    case SRATEF: RTNINT((sp->hdr)->srate); break;
    case DATAFORMATF: return(SCM_LIST2(gh_int2scm((sp->hdr)->format),gh_str02scm(mus_data_format_name((sp->hdr)->format)))); break;
    case HEADERTYPEF: return(SCM_LIST2(gh_int2scm((sp->hdr)->type),gh_str02scm(mus_header_type_name((sp->hdr)->type)))); break;
    case DATALOCATIONF: RTNINT((sp->hdr)->data_location); break;
    case CONTROLPANELSAVEF: save_control_panel(sp); break;
    case CONTROLPANELRESTOREF: restore_control_panel(sp); break;
    case SELECTEDCHANNELF: RTNINT(sp->selected_channel); break;
    case FILENAMEF: RTNSTR(sp->fullname); break;
    case SHORTFILENAMEF: RTNSTR(sp->shortname); break;
    case COMMENTF: RTNSTR(mus_sound_comment(sp->fullname)); break;
    case CLOSEF: snd_close_file(sp,state); break;
    case SAVEF: save_edits(sp,NULL); break;
    case UPDATEF: snd_update(state,sp); break;
    case CURSORFOLLOWSPLAYF: RTNBOOL(sp->cursor_follows_play); break;
    case SHOWCONTROLSF: RTNBOOL(control_panel_open(sp)); break;
    case SAVEMARKSF: RTNSTR(save_marks(sp)); break;  /* memory leak here... */
    }
  return(SCM_BOOL_F);
}

static SCM sp_iwrite(SCM snd_n, SCM val, int fld)
{
  snd_info *sp;
  int ival=0;
  sp = get_sp(snd_n);
  if (sp == NULL) return(NO_SUCH_SOUND);
  ival = bool_int_or_one(val);
  switch (fld)
    {
    case SYNCF: syncb(sp,ival); break;
    case UNITEF: combineb(sp,ival); break;
    case READONLYF: sp->read_only = ival; snd_file_lock_icon(sp,ival); break;
    case EXPANDINGF: toggle_expand_button(sp,ival); break;
    case CONTRASTINGF: toggle_contrast_button(sp,ival); break;
    case REVERBINGF: toggle_reverb_button(sp,ival); break;
    case FILTERINGF: toggle_filter_button(sp,ival); break;
    case FILTERDBING: set_filter_dBing(sp,ival); break;
    case FILTERORDERF: set_snd_filter_order(sp,ival); break;
    case CURSORFOLLOWSPLAYF: sp->cursor_follows_play = ival; break;
    case SHOWCONTROLSF: if (ival) sound_show_ctrls(sp); else sound_hide_ctrls(sp); break;
    }
  RTNBOOL(ival);
}

static SCM g_ok(SCM snd_n)
{
  #define H_ok "(" S_okQ " &optional (index 0)) -> #t if sound associated with index is active (accessible)"
  snd_info *sp;
  sp = get_sp(snd_n);
  if ((sp) && snd_ok(sp))
    return(SCM_BOOL_T); 
  else return(SCM_BOOL_F);
}

static SCM g_bomb(SCM snd, SCM on)
{
  #define H_bomb "(" S_bomb " &optional snd (on #t)) displays (or erases if on=#f) the bomb icon"
  snd_info *sp;
  sp = get_sp(snd);
  if (sp == NULL) return(NO_SUCH_SOUND);
  x_bomb(sp,bool_int_or_one(on));
  return(on);
}


static SCM g_syncing(SCM snd_n) 
{
  #define H_syncing "(" S_syncing " &optional snd) -> whether snd is sync'd to other sounds"
  ERRSP(S_syncing,snd_n,1); 
  return(sp_iread(snd_n,SYNCF));
}

static SCM g_set_syncing(SCM on, SCM snd_n) 
{
  #define H_set_syncing "(" S_set_syncing " &optional (on #t) snd) sets whether snd's is sync'd to others"
  ERRB1(on,S_set_syncing); 
  ERRSP(S_set_syncing,snd_n,2); 
  return(sp_iwrite(snd_n,on,SYNCF));
}

static SCM g_uniting(SCM snd_n) 
{
  #define H_uniting "(" S_uniting " &optional snd) -> whether snd's channels are conbined into one graph"
  ERRSP(S_uniting,snd_n,1); 
  return(sp_iread(snd_n,UNITEF));
}

static SCM g_set_uniting(SCM on, SCM snd_n) 
{
  #define H_set_uniting "(" S_set_uniting " &optional (on #t) snd) sets whether snd's channels are combined"
  ERRB1(on,S_set_uniting); 
  ERRSP(S_set_uniting,snd_n,2); 
  return(sp_iwrite(snd_n,on,UNITEF));
}

static SCM g_read_only(SCM snd_n) 
{
  #define H_read_only "(" S_read_only " &optional snd) -> whether snd is write-protected"
  ERRSP(S_read_only,snd_n,1);
  return(sp_iread(snd_n,READONLYF));
}

static SCM g_set_read_only(SCM on, SCM snd_n) 
{
  #define H_set_read_only "(" S_set_read_only " &optional (on #t) snd) sets whether snd is write-protected"
  ERRB1(on,S_set_read_only); 
  ERRSP(S_set_read_only,snd_n,2);
  return(sp_iwrite(snd_n,on,READONLYF));
}

static SCM g_channels(SCM snd_n)
{
  #define H_channels "("  S_channels " &optional snd) how many channels snd has"
  ERRSP(S_channels,snd_n,1); 
  return(sp_iread(snd_n,NCHANSF));
}

static SCM g_srate(SCM snd_n) 
{
  #define H_srate "(" S_srate " &optional snd) -> snd's srate"
  ERRSP(S_srate,snd_n,1); 
  return(sp_iread(snd_n,SRATEF));
}

static SCM g_data_location(SCM snd_n) 
{
  #define H_data_location "(" S_data_location " &optional snd) -> snd's data location"
  ERRSP(S_data_location,snd_n,1); 
  return(sp_iread(snd_n,DATALOCATIONF));
}

static SCM g_data_format(SCM snd_n) 
{
  #define H_data_format "(" S_data_format " &optional snd) -> snd's data format (e.g. mus-bshort)"
  ERRSP(S_data_format,snd_n,1); 
  return(sp_iread(snd_n,DATAFORMATF));
}

static SCM g_header_type(SCM snd_n) 
{
  #define H_header_type "(" S_header_type " &optional snd) -> snd's header type (e.g. mus-aiff)"
  ERRSP(S_header_type,snd_n,1); 
  return(sp_iread(snd_n,HEADERTYPEF));
}

static SCM g_contrasting(SCM snd_n) 
{
  #define H_contrasting "(" S_contrasting " &optional snd) -> snd's control panel constrast button state"
  ERRSP(S_contrasting,snd_n,1); 
  return(sp_iread(snd_n,CONTRASTINGF));
}

static SCM g_set_contrasting(SCM on, SCM snd_n) 
{
  #define H_set_contrasting "(" S_set_contrasting " &optional (on #t) snd) sets snd's control panel constrast button state"
  ERRB1(on,S_set_contrasting); 
  ERRSP(S_set_contrasting,snd_n,2); 
  return(sp_iwrite(snd_n,on,CONTRASTINGF));
}

static SCM g_expanding(SCM snd_n) 
{
  #define H_expanding "(" S_expanding " &optional snd) -> snd's control panel expand button state"
  ERRSP(S_expanding,snd_n,1); 
  return(sp_iread(snd_n,EXPANDINGF));
}

static SCM g_set_expanding(SCM on, SCM snd_n) 
{
  #define H_set_expanding "(" S_set_expanding " &optional (on #t) snd) sets snd's control panel expand button state"
  ERRB1(on,S_set_expanding); 
  ERRSP(S_set_expanding,snd_n,2); 
  return(sp_iwrite(snd_n,on,EXPANDINGF));
}

static SCM g_reverbing(SCM snd_n) 
{
  #define H_reverbing "(" S_reverbing " &optional snd) -> snd's control panel reverb button state"
  ERRSP(S_reverbing,snd_n,1); 
  return(sp_iread(snd_n,REVERBINGF));
}

static SCM g_set_reverbing(SCM on, SCM snd_n) 
{
  #define H_set_reverbing "(" S_set_reverbing " &optional (on #t) snd) sets snd's control panel reverb button state"
  ERRB1(on,S_set_reverbing); 
  ERRSP(S_set_reverbing,snd_n,2); 
  return(sp_iwrite(snd_n,on,REVERBINGF));
}

static SCM g_filtering(SCM snd_n) 
{
  #define H_filtering "(" S_filtering " &optional snd) -> snd's control panel filter button state"
  ERRSP(S_filtering,snd_n,1); 
  return(sp_iread(snd_n,FILTERINGF));
}

static SCM g_set_filtering(SCM on, SCM snd_n) 
{
  #define H_set_filtering "(" S_set_filtering " &optional (on #t) snd) sets snd's control panel filter button state"
  ERRB1(on,S_set_filtering); 
  ERRSP(S_set_filtering,snd_n,2); 
  return(sp_iwrite(snd_n,on,FILTERINGF));
}

static SCM g_filter_dBing(SCM snd_n) 
{
  #define H_filter_dBing "(" S_filter_dBing " &optional snd) -> #t if snd's filter envelope is displayed in dB in control panel"
  ERRSP(S_filter_dBing,snd_n,1); 
  return(sp_iread(snd_n,FILTERDBING));
}

static SCM g_set_filter_dBing(SCM on, SCM snd_n) 
{
  #define H_set_filter_dBing "(" S_set_filter_dBing " &optional (val #t) snd) sets whether snd's filter envelope is displayed in dB in control panel"
  ERRB1(on,S_set_filter_dBing); 
  ERRSP(S_set_filter_dBing,snd_n,2); 
  return(sp_iwrite(snd_n,on,FILTERDBING));
}

static SCM g_filter_order(SCM snd_n) 
{
  #define H_filter_order "(" S_filter_order " &optional snd) -> filter order (in control panel)"
  ERRSP(S_filter_order,snd_n,1); 
  return(sp_iread(snd_n,FILTERORDERF));
}

static SCM g_set_filter_order(SCM on, SCM snd_n) 
{
  #define H_set_filter_order "(" S_set_filter_order " val &optional snd) sets snd's filter order (in control panel)"
  ERRN1(on,S_set_filter_order); 
  ERRSP(S_set_filter_order,snd_n,2); 
  return(sp_iwrite(snd_n,on,FILTERORDERF));
}

static SCM g_save_control_panel(SCM snd_n) 
{
  #define H_save_control_panel "(" S_save_control_panel " &optional snd) saves the current control panel settings for subsequent " S_restore_control_panel
  ERRSP(S_save_control_panel,snd_n,1);
  return(sp_iread(snd_n,CONTROLPANELSAVEF));
}

static SCM g_restore_control_panel(SCM snd_n) 
{
  #define H_restore_control_panel "(" S_restore_control_panel " &optional snd) restores the previously saved control panel settings"
  ERRSP(S_restore_control_panel,snd_n,1); 
  return(sp_iread(snd_n,CONTROLPANELRESTOREF));
}

static SCM g_selected_channel(SCM snd_n) 
{
  #define H_selected_channel "(" S_selected_channel " &optional snd) -> currently selected channel in snd"
  ERRSP(S_selected_channel,snd_n,1); 
  return(sp_iread(snd_n,SELECTEDCHANNELF));
}

static SCM g_file_name(SCM snd_n) 
{
  #define H_file_name "(" S_file_name " &optional snd) -> snd's full filename"
  ERRSP(S_file_name,snd_n,1);
  return(sp_iread(snd_n,FILENAMEF));
}

static SCM g_short_file_name(SCM snd_n) 
{
  #define H_short_file_name "(" S_short_file_name " &optional snd) -> short form of snd's file name (no directory)"
  ERRSP(S_short_file_name,snd_n,1);
  return(sp_iread(snd_n,SHORTFILENAMEF));
}

static SCM g_comment(SCM snd_n)
{
  #define H_comment "(" S_comment " &optional snd) -> snd's comment (in its header)"
  ERRSP(S_comment,snd_n,1); 
  return(sp_iread(snd_n,COMMENTF));
}

#if FILE_PER_CHAN
static SCM string_array_to_list(char **arr, int i, int len)
{
  if (i < (len-1))
    return(gh_cons(gh_str02scm(arr[i]),string_array_to_list(arr,i+1,len)));
  else return(gh_cons(gh_str02scm(arr[i]),SCM_EOL));
}

static SCM g_file_names(SCM snd_n) 
{
  #define H_file_names "(" S_file_names " &optional snd) -> channel file names associated with snd"
  snd_info *sp;
  ERRSP(S_file_names,snd_n,1);
  sp = get_sp(snd_n);
  if (sp == NULL) return(NO_SUCH_SOUND);
  if (sp->chan_type == FILE_PER_SOUND) return(sp_iread(snd_n,FILENAMEF));
  return(string_array_to_list(sp->channel_filenames,0,sp->nchans));
}

static SCM g_short_file_names(SCM snd_n) 
{
  #define H_short_file_names "(" S_short_file_names " &optional snd) -> channel file names (no directory) associated with snd"
  snd_info *sp;
  SCM result = SCM_EOL;
  int i;
  char **strs;
  ERRSP(S_short_file_names,snd_n,1);
  sp = get_sp(snd_n);
  if (sp == NULL) return(NO_SUCH_SOUND);
  if (sp->chan_type == FILE_PER_SOUND) return(sp_iread(snd_n,SHORTFILENAMEF));
  strs = (char **)CALLOC(sp->nchans,sizeof(char *));
  for (i=0;i<sp->nchans;i++) strs[i] = filename_without_home_directory(sp->channel_filenames[i]);
  result = string_array_to_list(strs,0,sp->nchans);
  FREE(strs);
  return(result);
}
#endif

static SCM g_close_sound(SCM snd_n) 
{
  #define H_close_sound "(" S_close_sound " snd) closes snd"
  ERRSP(S_close_sound,snd_n,1); 
  return(sp_iread(snd_n,CLOSEF));
}

static SCM g_update_sound(SCM snd_n) 
{
  #define H_update_sound "(" S_update_sound " snd) updates snd (re-reads from disk flushing pending edits)"
  ERRSP(S_update_sound,snd_n,1); 
  return(sp_iread(snd_n,UPDATEF));
}

static SCM g_save_sound(SCM snd_n) 
{
  #define H_save_sound "(" S_save_sound " &optional snd) saves snd (updates the on-disk data to match Snd's current version)"
  ERRSP(S_save_sound,snd_n,1); 
  return(sp_iread(snd_n,SAVEF));
}

static SCM g_cursor_follows_play(SCM snd_n) 
{
  #define H_cursor_follows_play "("  S_cursor_follows_play " &optional snd) -> #t if cursor moves along in waveform display as sound is played (#f)"
  ERRSP(S_cursor_follows_play,snd_n,1); 
  return(sp_iread(snd_n,CURSORFOLLOWSPLAYF));
}

static SCM g_set_cursor_follows_play(SCM on, SCM snd_n) 
{
  #define H_set_cursor_follows_play "(" S_set_cursor_follows_play " &optional (val #t)) sets " S_cursor_follows_play
  ERRB1(on,S_set_cursor_follows_play); 
  ERRSP(S_set_cursor_follows_play,snd_n,2); 
  return(sp_iwrite(snd_n,on,CURSORFOLLOWSPLAYF));
}

static SCM g_showing_controls(SCM snd_n) 
{
  #define H_showing_controls "(" S_showing_controls " &optional snd) -> #t if snd's control panel is known to be open"
  ERRSP(S_showing_controls,snd_n,1); 
  return(sp_iread(snd_n,SHOWCONTROLSF));
}

static SCM g_set_showing_controls(SCM on, SCM snd_n)
{
  #define H_set_showing_controls "(" S_set_showing_controls " &optional (on #t) snd) sets whether snd's control panel is open"
  ERRB1(on,S_set_showing_controls); 
  ERRSP(S_set_showing_controls,snd_n,2); 
  return(sp_iwrite(snd_n,on,SHOWCONTROLSF));
}

static SCM g_save_marks(SCM snd_n) 
{
  #define H_save_marks "(" S_save_marks " &optional snd) saves snd's marks in <snd's file-name>.marks"
  ERRSP(S_save_marks,snd_n,1);
  return(sp_iread(snd_n,SAVEMARKSF));
}

static SCM g_override_data_location(SCM loc, SCM snd) 
{
  #define H_override_data_location "(" S_override_data_location " loc &optional snd) overrides snd's notion of its data location"
  snd_info *sp;
  ERRN1(loc,S_override_data_location);
  ERRSP(S_override_data_location,snd,2);
  sp = get_sp(snd);
  if (sp == NULL) return(NO_SUCH_SOUND);
  mus_sound_override_header(sp->fullname,-1,-1,-1,-1,g_scm2int(loc),-1);
  snd_update(sp->state,sp);
  return(loc);
}

static SCM g_override_data_format(SCM frm, SCM snd) 
{
  #define H_override_data_format "(" S_override_data_format " format &optional snd) overrides snd's notion of its data format"
  snd_info *sp;
  ERRN1(frm,S_override_data_format);
  ERRSP(S_override_data_format,snd,2);
  sp = get_sp(snd);
  if (sp == NULL) return(NO_SUCH_SOUND);
  mus_sound_override_header(sp->fullname,-1,-1,g_scm2int(frm),-1,-1,-1);
  snd_update(sp->state,sp);
  return(frm);
}

static SCM g_override_data_size(SCM over, SCM snd) 
{
  #define H_override_data_size "(" S_override_data_size " samples &optional snd) overrides snd's notion of its data size"
  snd_info *sp;
  ERRN1(over,S_override_data_size);
  ERRSP(S_override_data_size,snd,2);
  sp = get_sp(snd);
  if (sp == NULL) return(NO_SUCH_SOUND);
  mus_sound_override_header(sp->fullname,-1,-1,-1,-1,-1,g_scm2int(over));
  snd_update(sp->state,sp);
  return(over);
}

static SCM g_set_sound_loop_info(SCM start0, SCM end0, SCM start1, SCM end1, SCM snd)
{
  #define H_set_sound_loop_info "(" S_set_sound_loop_info " start0 end0 &optional start1 end1 snd) sets loop points"
  snd_info *sp;
  char *tmp_file;
  int type;
  ERRN1(start0,S_set_sound_loop_info);
  ERRN2(end0,S_set_sound_loop_info);
  ERRB3(start1,S_set_sound_loop_info);
  ERRB4(end1,S_set_sound_loop_info);
  ERRSP(S_set_sound_loop_info,snd,5);
  sp = get_sp(snd);
  if (sp == NULL) return(NO_SUCH_SOUND);
  if ((sp->hdr)->loops == NULL)
    (sp->hdr)->loops = (int *)CALLOC(6,sizeof(int));
  (sp->hdr)->loops[0] = g_scm2int(start0);
  (sp->hdr)->loops[1] = g_scm2int(end0);
  (sp->hdr)->loops[2] = g_scm2intdef(start1,0);
  (sp->hdr)->loops[3] = g_scm2intdef(end1,0);
  mus_sound_set_loop_info(sp->fullname,(sp->hdr)->loops);
  type = (sp->hdr)->type;
  if ((type != MUS_AIFF) && (type != MUS_AIFC))
    {
      snd_warning("changing %s header from %s to aifc to accomodate loop info",sp->shortname,mus_header_type_name(type));
      type = MUS_AIFC;
    }
  tmp_file = snd_tempnam(sp->state);
  save_edits_2(sp,tmp_file,type,(sp->hdr)->format,(sp->hdr)->srate,(sp->hdr)->comment);
  snd_copy_file(sp->state,tmp_file,sp->fullname);
  remove(tmp_file);
  free(tmp_file);
  snd_update(sp->state,sp);
  return(SCM_BOOL_T);
}

static SCM g_soundfont_info(SCM snd)
{
  /* return all soundfont descriptors as list of lists: ((name start loopstart loopend)) */
  #define H_soundfont_info "(" S_soundfont_info " &optional snd) -> list of lists describing snd as a soundfont.\n\
   each inner list has the form: (name start loopstart loopend)"

  SCM inlist = SCM_EOL,outlist = SCM_EOL;
  int i,lim;
  snd_info *sp;
  ERRSP(S_soundfont_info,snd,1);
  sp = get_sp(snd);
  if (sp == NULL) return(NO_SUCH_SOUND);
  mus_header_read(sp->fullname);
  if (mus_header_type() == MUS_SOUNDFONT)
    {
      lim = mus_header_sf2_entries();
      if (lim > 0)
	{
	  for (i=lim-1;i>=0;i--)
	    {
	      inlist = SCM_LIST4(gh_str02scm(mus_header_sf2_name(i)),
				 gh_int2scm(mus_header_sf2_start(i)),
				 gh_int2scm(mus_header_sf2_loop_start(i)),
				 gh_int2scm(mus_header_sf2_end(i)));
	      outlist = gh_cons(inlist,outlist);
	    }
	}
    }
  return(outlist);
}

static file_info **temp_sound_headers = NULL;
static int *temp_sound_fds = NULL;
static int temp_sound_size = 0;

static void set_temp_fd(int fd, file_info *hdr)
{
  int i,pos;
  if (temp_sound_size == 0)
    {
      temp_sound_size = 4;
      temp_sound_fds = (int *)CALLOC(temp_sound_size,sizeof(int));
      temp_sound_headers = (file_info **)CALLOC(temp_sound_size,sizeof(file_info *));
      pos = 0;
    }
  else
    {
      pos = -1;
      for (i=0;i<temp_sound_size;i++)
	if (temp_sound_headers[i] == NULL) {pos=i; break;}
      if (pos == -1)
	{
	  pos = temp_sound_size;
	  temp_sound_size += 4;
	  temp_sound_fds = (int *)REALLOC(temp_sound_fds,temp_sound_size * sizeof(int));
	  temp_sound_headers = (file_info **)REALLOC(temp_sound_headers,temp_sound_size * sizeof(file_info *));
	  for (i=pos;i<temp_sound_size;i++) temp_sound_headers[i] = NULL;
	}
    }
  temp_sound_fds[pos] = fd;
  temp_sound_headers[pos] = hdr;
}

static file_info *get_temp_header(int fd)
{
  int i;
  for (i=0;i<temp_sound_size;i++)
    if (fd == temp_sound_fds[i]) return(temp_sound_headers[i]);
  return(NULL);
}

static void unset_temp_fd(int fd)
{
  int i;
  for (i=0;i<temp_sound_size;i++)
    if (fd == temp_sound_fds[i])
      {
	temp_sound_fds[i] = 0;
	temp_sound_headers[i] = NULL;
      }
}

static SCM g_open_sound_file(SCM g_name, SCM g_chans, SCM g_srate, SCM g_comment)
{
  #define H_open_sound_file "(" S_open_sound_file " &optional (name \"test.snd\") (chans 1) (srate 22050) comment) creates a new\n\
   sound file 'name' using either 'wave' or 'next' headers and float data, returns the file descriptor for subsequent " S_close_sound_file "\n\
   data can be written with " S_vct_sound_file

  /* assume user temp files are writing floats in native format */
  char *name = NULL,*comment = NULL;
  file_info *hdr;
  int chans = 1,srate = 22050,result;
#ifdef MUS_LITTLE_ENDIAN
  int type = MUS_RIFF;
  int format = MUS_LFLOAT; /* see comment! */
#else
  int type = MUS_NEXT;
  int format = MUS_BFLOAT;
#endif
  if (gh_string_p(g_name)) name = gh_scm2newstr(g_name,NULL); else if (!(SCM_UNBNDP(g_name))) scm_wrong_type_arg(S_open_sound_file,1,g_name);
  if (gh_number_p(g_chans)) chans = g_scm2int(g_chans); else if (!(SCM_UNBNDP(g_chans))) scm_wrong_type_arg(S_open_sound_file,2,g_chans);
  if (gh_number_p(g_srate)) srate = g_scm2int(g_srate); else if (!(SCM_UNBNDP(g_srate))) scm_wrong_type_arg(S_open_sound_file,3,g_srate);
  if (gh_string_p(g_comment)) comment = gh_scm2newstr(g_comment,NULL); else if (!(SCM_UNBNDP(g_comment))) scm_wrong_type_arg(S_open_sound_file,4,g_comment);
  if (name == NULL)
#ifdef MUS_LITTLE_ENDIAN
    name = copy_string("test.wav");
#else
    name = copy_string("test.snd");
#endif
  hdr = (file_info *)CALLOC(1,sizeof(file_info));
  hdr->name = copy_string(name);
  hdr->samples = 0;
  hdr->data_location = 28;
  hdr->srate = srate;
  hdr->chans = chans;
  hdr->format = format;
  hdr->type = type;
  hdr->comment = comment;
  result = open_temp_file(name,chans,hdr,state);
  set_temp_fd(result,hdr);
  return(gh_int2scm(result)); /* -1 for error */
}

static SCM g_close_sound_file(SCM g_fd, SCM g_bytes)
{
  #define H_close_sound_file "(" S_close_sound_file " fd bytes) closes file pointed to by fd, updating its header to report 'bytes' bytes of data"
  file_info *hdr;
  int result,fd,bytes;
  ERRN1(g_fd,S_close_sound_file);
  ERRN2(g_bytes,S_close_sound_file);
  fd = g_scm2int(g_fd);
  bytes = g_scm2int(g_bytes);
  hdr = get_temp_header(fd);
  if (hdr == NULL) 
    {
      snd_error("can't find %d's header!",fd);
      close(fd);
      return(SCM_BOOL_F);
    }
  else
    {
      result = close_temp_file(fd,hdr,bytes,any_selected_sound(state));
      unset_temp_fd(fd);
      return(gh_int2scm(result));
    }
}

static SCM g_save_envelopes(SCM filename)
{
  #define H_save_envelopes "(" S_save_envelopes " filename) saves the envelopes known to the envelope editor in filename"
  char *name = NULL;
  FILE *fd;
  ERRSB1(filename,S_save_envelopes);
  if (gh_string_p(filename)) 
    name = full_filename(filename);
  else name = copy_string("envs.save");
  fd = fopen(name,"w");
  if (name) FREE(name);
  if (fd)
    {
      save_envelope_editor_state(state,fd);
      fclose(fd);
      return(filename);
    }
  return(CANNOT_SAVE);
}

static SCM g_save_options(SCM filename)
{
  #define H_save_options "(" S_save_options " filename) saves Snd options in filename"
  char *name = NULL;
  FILE *fd;
  ERRS1(filename,S_save_options);
  name = full_filename(filename);
  fd = fopen(name,"w");
  if (name) FREE(name);
  if (fd) 
    {
      save_snd_state_options(state,fd);
      fclose(fd);
      return(filename);
    }
  return(CANNOT_SAVE);
}

static SCM g_select_sound(SCM snd_n)
{
  #define H_select_sound "(" S_select_sound " &optional snd) makes snd the selected (active) sound"
  int val;
  snd_info *osp,*sp;
  ERRSP(S_select_sound,snd_n,1);
  val = g_scm2intdef(snd_n,0);
  if ((val >= 0) && (val < state->max_sounds))
    {
      sp = state->sounds[val];
      if (snd_ok(sp))
	{
	  osp = any_selected_sound(state);
	  select_channel(sp,0);
	  normalize_sound(state,sp,osp,sp->chans[0]);
	  /* goto_graph(sp->chans[0]); */
	  map_over_chans(state,update_graph,NULL);
	  return(snd_n);
	}
    }
  return(NO_SUCH_SOUND);
}

static SCM g_select_channel(SCM chn_n)
{
  #define H_select_channel "(" S_select_channel " &optional chn) makes chn the selected (active) channel"
  snd_info *sp;
  int chan;
  ERRSP(S_select_channel,chn_n,1);
  chan = g_scm2intdef(chn_n,0);
  sp = any_selected_sound(state);
  if ((sp) && (chan < sp->nchans)) 
    {
      select_channel(sp,chan);
      return(chn_n);
    }
  return(NO_SUCH_CHANNEL);
}

static SCM g_select_mix(SCM id)
{
  #define H_select_mix "(" S_select_mix " id) makes mix is the selected mix"
  ERRN1(id,S_select_mix);
  select_mix(state,md_from_int(g_scm2int(id)));
  return(id);
}

static SCM g_selected_mix(void)
{
  #define H_selected_mix "(" S_selected_mix ") -> the id of the currently selected mix"
  return(gh_int2scm(state->selected_mix));
}


enum {AMPF,CONTRASTF,CONTRASTAMPF,EXPANDF,EXPANDLENGTHF,EXPANDRAMPF,EXPANDHOPF,
      SPEEDF,REVERBLENGTHF,REVERBFEEDBACKF,REVERBSCALEF,REVERBLOWPASSF};

static SCM sp_fread(SCM snd_n, int fld)
{
  snd_info *sp;
  sp = get_sp(snd_n);
  if (sp == NULL) return(NO_SUCH_SOUND);
  switch (fld)
    {
    case AMPF: RTNFLT(sp->amp); break;
    case CONTRASTF: RTNFLT(sp->contrast); break;
    case CONTRASTAMPF: RTNFLT(sp->contrast_amp); break;
    case EXPANDF: RTNFLT(sp->expand); break;
    case EXPANDLENGTHF: RTNFLT(sp->local_explen); break;
    case EXPANDRAMPF: RTNFLT(sp->local_exprmp); break;
    case EXPANDHOPF: RTNFLT(sp->local_exphop); break;
    case SPEEDF: if (sp->play_direction == -1) RTNFLT((-(sp->srate))); else RTNFLT(sp->srate); break;
    case REVERBLENGTHF: RTNFLT(sp->revlen); break;
    case REVERBFEEDBACKF: RTNFLT(sp->local_revfb); break;
    case REVERBSCALEF: RTNFLT(sp->revscl); break;
    case REVERBLOWPASSF: RTNFLT(sp->local_revlp); break;
    }
  return(SCM_BOOL_F);
}

static SCM sp_fwrite(SCM snd_n, SCM val, int fld)
{
  snd_info *sp;
  Float fval;
  int dir;
  sp = get_sp(snd_n);
  if (sp == NULL) return(NO_SUCH_SOUND);
  fval = gh_scm2double(val);
  switch (fld)
    {
    case AMPF: set_snd_amp(sp,fval); break;
    case CONTRASTF: set_snd_contrast(sp,fval); break;
    case CONTRASTAMPF: sp->contrast_amp = fval; break;
    case EXPANDF: set_snd_expand(sp,fval); break;
    case EXPANDLENGTHF: sp->local_explen = fval; break;
    case EXPANDRAMPF: sp->local_exprmp = fval; break;
    case EXPANDHOPF: sp->local_exphop = fval; break;
    case SPEEDF: 
      if (fval > 0.0) dir=1; else dir=-1;
      set_snd_srate(sp,dir*fval); 
      toggle_direction_arrow(sp,(dir == -1));
      break;
    case REVERBLENGTHF: set_snd_revlen(sp,fval); break;
    case REVERBFEEDBACKF: sp->local_revfb = fval; break;
    case REVERBSCALEF: set_snd_revscl(sp,fval); break;
    case REVERBLOWPASSF: sp->local_revlp = fval; break;
    }
  return(val);
}

static SCM g_amp(SCM snd_n) 
{
  #define H_amp "(" S_amp " &optional snd) -> current amp slider setting"
  ERRSP(S_amp,snd_n,1); 
  return(sp_fread(snd_n,AMPF));
}

static SCM g_set_amp(SCM on, SCM snd_n) 
{
  #define H_set_amp "(" S_set_amp " val &optional snd) sets snd's amp slider value to val"
  ERRN1(on,S_set_amp); 
  ERRSP(S_set_amp,snd_n,2); 
  return(sp_fwrite(snd_n,on,AMPF));
}

static SCM g_contrast(SCM snd_n) 
{
  #define H_contrast "(" S_contrast " &optional snd) -> current contrast slider setting"
  ERRSP(S_contrast,snd_n,1); 
  return(sp_fread(snd_n,CONTRASTF));
}

static SCM g_set_contrast(SCM on, SCM snd_n) 
{
  #define H_set_contrast " val &optional snd) sets snd's contrast slider value to val"
  ERRN1(on,S_set_contrast); 
  ERRSP(S_set_contrast,snd_n,2); 
  return(sp_fwrite(snd_n,on,CONTRASTF));
}

static SCM g_contrast_amp(SCM snd_n) 
{
  #define H_contrast_amp "(" S_contrast_amp " &optional snd) -> snd's contrast amp\n\
   (scaler on data before contrast operation in control panel, 1.0)"

  ERRSP(S_contrast_amp,snd_n,1); 
  return(sp_fread(snd_n,CONTRASTAMPF));
}

static SCM g_set_contrast_amp(SCM on, SCM snd_n) 
{
  #define H_set_contrast_amp "(" S_set_contrast_amp " val &optional snd) sets snd's contrast scaler"
  ERRN1(on,S_set_contrast_amp);
  ERRSP(S_set_contrast_amp,snd_n,2); 
  return(sp_fwrite(snd_n,on,CONTRASTAMPF));
}

static SCM g_expand(SCM snd_n) 
{
  #define H_expand "(" S_expand " &optional snd) -> current expand slider setting"
  ERRSP(S_expand,snd_n,1); 
  return(sp_fread(snd_n,EXPANDF));
}

static SCM g_set_expand(SCM on, SCM snd_n) 
{
  #define H_set_expand "(" S_set_expand " val &optional snd) sets snd's expand (granular synthesis) slider value to val"
  ERRN1(on,S_set_expand); 
  ERRSP(S_set_expand,snd_n,2); 
  return(sp_fwrite(snd_n,on,EXPANDF));
}

static SCM g_expand_length(SCM snd_n) 
{
  #define H_expand_length "(" S_expand_length " &optional snd) -> current expansion segment length in seconds (.15)"
  ERRSP(S_expand_length,snd_n,1); 
  return(sp_fread(snd_n,EXPANDLENGTHF));
}

static SCM g_set_expand_length(SCM on, SCM snd_n) 
{
  #define H_set_expand_length "(" S_set_expand_length " val &optional snd) sets snd's current expansion segment length"
  ERRN1(on,S_set_expand_length); 
  ERRSP(S_set_expand_length,snd_n,2); 
  return(sp_fwrite(snd_n,on,EXPANDLENGTHF));
}

static SCM g_expand_ramp(SCM snd_n) 
{
  #define H_expand_ramp "(" S_expand_ramp " &optional snd) -> current expansion ramp time (.4)"
  ERRSP(S_expand_ramp,snd_n,1); 
  return(sp_fread(snd_n,EXPANDRAMPF));
}

static SCM g_set_expand_ramp(SCM on, SCM snd_n) 
{
  #define H_set_expand_ramp "(" S_set_expand_ramp " val &optional snd) sets snd's current expansion ramp time (between 0.0 and 0.5)"
  ERRN1(on,S_set_expand_ramp);
  ERRSP(S_set_expand_ramp,snd_n,2); 
  return(sp_fwrite(snd_n,on,EXPANDRAMPF));
}

static SCM g_expand_hop(SCM snd_n) 
{
  #define H_expand_hop "(" S_expand_hop " &optional snd) -> current expansion output grain spacing in seconds (0.05)"
  ERRSP(S_expand_hop,snd_n,1); 
  return(sp_fread(snd_n,EXPANDHOPF));
}

static SCM g_set_expand_hop(SCM on, SCM snd_n) 
{
  #define H_set_expand_hop "(" S_set_expand_hop " val &optional snd) sets snd's current expansion output grain spacing in seconds"
  ERRN1(on,S_set_expand_hop); 
  ERRSP(S_set_expand_hop,snd_n,2);
  return(sp_fwrite(snd_n,on,EXPANDHOPF));
}

static SCM g_speed(SCM snd_n) 
{
  #define H_speed "(" S_speed " &optional snd) -> current speed (srate) slider setting"
  ERRSP(S_speed,snd_n,1); 
  return(sp_fread(snd_n,SPEEDF));
}

static SCM g_set_speed(SCM on, SCM snd_n) 
{
  #define H_set_speed "(" S_set_speed " val &optiona snd) sets snd's current speed slider value to val"
  ERRN1(on,S_set_speed); 
  ERRSP(S_set_speed,snd_n,2);
  return(sp_fwrite(snd_n,on,SPEEDF));
}

static SCM g_reverb_length(SCM snd_n) 
{
  #define H_reverb_length "(" S_reverb_length " &optional snd) -> reverb decay length scaler"
  ERRSP(S_reverb_length,snd_n,1); 
  return(sp_fread(snd_n,REVERBLENGTHF));
}

static SCM g_set_reverb_length(SCM on, SCM snd_n) 
{
  #define H_set_reverb_length "(" S_set_reverb_length " val &optional snd) sets snd's reverb decay length scaler"
  ERRN1(on,S_set_reverb_length); 
  ERRSP(S_set_reverb_length,snd_n,2); 
  return(sp_fwrite(snd_n,on,REVERBLENGTHF));
}

static SCM g_reverb_feedback(SCM snd_n) 
{
  #define H_reverb_feedback "(" S_reverb_feedback " &optional snd) -> reverb feedback scaler"
  ERRSP(S_reverb_feedback,snd_n,1); 
  return(sp_fread(snd_n,REVERBFEEDBACKF));
}

static SCM g_set_reverb_feedback(SCM on, SCM snd_n) 
{
  #define H_set_reverb_feedback "(" S_set_reverb_feedback " val &optional snd) sets snd's reverb feedback scaler"
  ERRN1(on,S_set_reverb_feedback); 
  ERRSP(S_set_reverb_feedback,snd_n,2);
  return(sp_fwrite(snd_n,on,REVERBFEEDBACKF));
}

static SCM g_reverb_scale(SCM snd_n) 
{
  #define H_reverb_scale "(" S_reverb_scale " &optional snd) -> reverb scaler (the amount of reverb)"
  ERRSP(S_reverb_scale,snd_n,1);
  return(sp_fread(snd_n,REVERBSCALEF));
}

static SCM g_set_reverb_scale(SCM on, SCM snd_n) 
{
  #define H_set_reverb_scale "(" S_set_reverb_scale " val &optional snd) sets snd's reverb amount"
  ERRN1(on,S_set_reverb_scale); 
  ERRSP(S_set_reverb_scale,snd_n,2); 
  return(sp_fwrite(snd_n,on,REVERBSCALEF));
}

static SCM g_reverb_lowpass(SCM snd_n) 
{
  #define H_reverb_lowpass "(" S_reverb_lowpass " &optional snd) -> reverb lowpass filter coefficient"
  ERRSP(S_reverb_lowpass,snd_n,1); 
  return(sp_fread(snd_n,REVERBLOWPASSF));
}

static SCM g_set_reverb_lowpass(SCM on, SCM snd_n) 
{
  #define H_set_reverb_lowpass "(" S_set_reverb_lowpass " val &optional snd) sets snd's reverb lowpass filter coefficient"
  ERRN1(on,S_set_reverb_lowpass); 
  ERRSP(S_set_reverb_lowpass,snd_n,2); 
  return(sp_fwrite(snd_n,on,REVERBLOWPASSF));
}

enum {FFTF,WAVEF,LENGTHF,CURSORF,MAXAMPF,GRAPHINGF,LOSAMPF,HISAMPF,SQUELCH_UPDATE,AP_SX,AP_SY,AP_ZX,AP_ZY,EDITF,CURSOR_STYLE,EDIT_HOOK,UNDO_HOOK};

static SCM cp_iread(SCM snd_n, SCM chn_n, int fld)
{
  chan_info *cp;
  cp = get_cp(snd_n,chn_n);
  if (cp == NULL) return(NO_SUCH_CHANNEL);
  switch(fld)
    {
    case EDITF: RTNINT(cp->edit_ctr); break;
    case FFTF: RTNBOOL(cp->ffting); break;
    case WAVEF: RTNBOOL(cp->waving); break;
    case CURSORF: RTNINT(cp->cursor); break;
    case LENGTHF: RTNINT(current_ed_samples(cp)); break;
    case MAXAMPF: RTNFLT(get_maxamp(state,cp->sound,cp)); break;
    case GRAPHINGF: RTNBOOL(cp->lisp_graphing); break;
    case LOSAMPF: if (cp->axis) RTNINT((cp->axis)->losamp); break;
    case HISAMPF: if (cp->axis) RTNINT((cp->axis)->hisamp); break;
    case SQUELCH_UPDATE: RTNBOOL(cp->squelch_update); break;
    case AP_SX: if (cp->axis) RTNFLT((cp->axis)->sx); break;
    case AP_SY: if (cp->axis) RTNFLT((cp->axis)->sy); break;
    case AP_ZX: if (cp->axis) RTNFLT((cp->axis)->zx); break;
    case AP_ZY: if (cp->axis) RTNFLT((cp->axis)->zy); break;
    case CURSOR_STYLE: RTNINT(cp->cursor_style); break;
    case EDIT_HOOK: return(cp->edit_hook); break;
    case UNDO_HOOK: return(cp->undo_hook); break;
    }
  return(SCM_BOOL_F);
}

static SCM cp_iwrite(SCM snd_n, SCM chn_n, SCM on, int fld)
{
  chan_info *cp;
  int val = 0;
  cp = get_cp(snd_n,chn_n);
  if (cp == NULL) return(NO_SUCH_CHANNEL);
  switch (fld)
    {
    case FFTF: fftb(cp,val = bool_int_or_one(on)); update_graph(cp,NULL); break;
    case WAVEF: waveb(cp,val = bool_int_or_one(on)); update_graph(cp,NULL); break;
    case CURSORF: cp->cursor_on = 1; handle_cursor(cp,cursor_moveto(cp,val = g_scm2intdef(on,1))); break;
    case GRAPHINGF: 
      cp->lisp_graphing = bool_int_or_one(on); 
      val = cp->lisp_graphing;
      update_graph(cp,NULL); 
      break;
    case LOSAMPF: set_x_axis_x0(cp,val = g_scm2intdef(on,0)); return(on); break;
    case HISAMPF: set_x_axis_x1(cp,val = g_scm2intdef(on,1)); return(on); break;
    case SQUELCH_UPDATE: cp->squelch_update = bool_int_or_one(on); break;
    case CURSOR_STYLE: cp->cursor_style = g_scm2intdef(on,0); update_graph(cp,NULL); return(on); break;
    }
  RTNBOOL(val);
}

static SCM g_edit_position(SCM snd_n, SCM chn_n) 
{
  #define H_edit_position "(" S_edit_position " &optional snd chn) -> current edit history position in snd's channel chn"
  ERRCP(S_edit_position,snd_n,chn_n,1);
  return(cp_iread(snd_n,chn_n,EDITF));
}

static SCM g_ffting(SCM snd_n, SCM chn_n) 
{
  #define H_ffting "(" S_ffting " &optional snd chn) -> #t if fft display is active in snd's channel chn"
  ERRCP(S_ffting,snd_n,chn_n,1); 
  return(cp_iread(snd_n,chn_n,FFTF));
}

static SCM g_set_ffting(SCM on, SCM snd_n, SCM chn_n) 
{
  #define H_set_ffting "(" S_set_ffting " &optional (val #t) snd chn) sets whether snd's channel chn is displaying ffts"
  ERRB1(on,S_set_ffting); 
  ERRCP(S_set_ffting,snd_n,chn_n,2);
  return(cp_iwrite(snd_n,chn_n,on,FFTF));
}

static SCM g_waving(SCM snd_n, SCM chn_n) 
{
  #define H_waving "(" S_waving " &optional snd chn) -> #t if time domain display is active in snd's channel chn"
  ERRCP(S_waving,snd_n,chn_n,1); 
  return(cp_iread(snd_n,chn_n,WAVEF));
}

static SCM g_set_waving(SCM on, SCM snd_n, SCM chn_n) 
{
  #define H_set_waving "(" S_set_waving " &optional (val #t) snd chn) sets whether snd's channel chn is displaying time domain data"
  ERRB1(on,S_set_waving); 
  ERRCP(S_set_waving,snd_n,chn_n,2); 
  return(cp_iwrite(snd_n,chn_n,on,WAVEF));
}

static SCM g_graphing(SCM snd_n, SCM chn_n) 
{
  #define H_graphing "(" S_graphing " &optional snd chn) -> #t if lisp-generated data display is active in snd's channel chn"
  ERRCP(S_graphing,snd_n,chn_n,1);
  return(cp_iread(snd_n,chn_n,GRAPHINGF));
}

static SCM g_set_graphing(SCM on, SCM snd_n, SCM chn_n) 
{
  #define H_set_graphing "(" S_set_graphing " &optional (val #t) snd chn) sets whether snd's channel chn is displaying lisp-generated data"
  ERRB1(on,S_set_graphing); 
  ERRCP(S_set_graphing,snd_n,chn_n,2); 
  return(cp_iwrite(snd_n,chn_n,on,GRAPHINGF));
}

static SCM g_cursor(SCM snd_n, SCM chn_n) 
{
  #define H_cursor "(" S_cursor " &optional snd chn) -> current cursor location in snd's channel chn"
  ERRCP(S_cursor,snd_n,chn_n,1); 
  return(cp_iread(snd_n,chn_n,CURSORF));
}

static SCM g_set_cursor(SCM on, SCM snd_n, SCM chn_n) 
{
  #define H_set_cursor "(" S_set_cursor " val &optional snd chn) sets the current cursor location in snd's channel chn"
  ERRB1(on,S_set_cursor); 
  ERRCP(S_set_cursor,snd_n,chn_n,2); 
  return(cp_iwrite(snd_n,chn_n,on,CURSORF));
}

static SCM g_cursor_style(SCM snd_n, SCM chn_n) 
{
  #define H_cursor_style "(" S_cursor_style " &optional snd chn) -> current cursor style in snd's channel chn"
  ERRCP(S_cursor_style,snd_n,chn_n,1); 
  return(cp_iread(snd_n,chn_n,CURSOR_STYLE));
}

static SCM g_set_cursor_style(SCM on, SCM snd_n, SCM chn_n) 
{
  #define H_set_cursor_style "(" S_set_cursor_style " val &optional snd chn) sets the current cursor style in snd's channel chn"
  ERRB1(on,S_set_cursor_style); 
  ERRCP(S_set_cursor_style,snd_n,chn_n,2); 
  return(cp_iwrite(snd_n,chn_n,on,CURSOR_STYLE));
}

static SCM g_frames(SCM snd_n, SCM chn_n) 
{
  #define H_frames "(" S_frames " &optional snd chn) -> number of frames of data in snd's channel chn"
  ERRCP(S_frames,snd_n,chn_n,1); 
  return(cp_iread(snd_n,chn_n,LENGTHF));
}

static SCM g_maxamp(SCM snd_n, SCM chn_n) 
{
  #define H_maxamp "(" S_maxamp " &optional snd chn) -> max amp of data in snd's channel chn"
  ERRCP(S_maxamp,snd_n,chn_n,1); 
  return(cp_iread(snd_n,chn_n,MAXAMPF));
}

static SCM g_squelch_update(SCM snd_n, SCM chn_n) 
{
  #define H_squelch_update "(" S_squelch_update " &optional snd chn) -> #t if updates (redisplays) are off in snd's channel chn"
  ERRCP(S_squelch_update,snd_n,chn_n,1); 
  return(cp_iread(snd_n,chn_n,SQUELCH_UPDATE));
}

static SCM g_set_squelch_update(SCM on, SCM snd_n, SCM chn_n) 
{
  #define H_set_squelch_update "(" S_set_squelch_update " &optional (on #t) snd chn) sets whether updates (redisplays) are off in snd's channel chn"
  ERRB1(on,S_set_squelch_update); 
  ERRCP(S_set_squelch_update,snd_n,chn_n,2); 
  return(cp_iwrite(snd_n,chn_n,on,SQUELCH_UPDATE));
}

static SCM g_ap_sx(SCM snd_n, SCM chn_n) 
{
  #define H_x_position_slider "(" S_x_position_slider " &optional snd chn) -> current x axis position slider of snd channel chn"
  ERRCP(S_x_position_slider,snd_n,chn_n,1); 
  return(cp_iread(snd_n,chn_n,AP_SX));
}

static SCM g_ap_sy(SCM snd_n, SCM chn_n) 
{
  #define H_y_position_slider "(" S_y_position_slider " &optional snd chn) -> current y axis position slider of snd channel chn"
  ERRCP(S_y_position_slider,snd_n,chn_n,1); 
  return(cp_iread(snd_n,chn_n,AP_SY));
}

static SCM g_ap_zx(SCM snd_n, SCM chn_n) 
{
  #define H_x_zoom_slider "(" S_x_zoom_slider " &optional snd chn) -> current x axis zoom slider of snd channel chn"
  ERRCP(S_x_zoom_slider,snd_n,chn_n,1); 
  return(cp_iread(snd_n,chn_n,AP_ZX));
}

static SCM g_ap_zy(SCM snd_n, SCM chn_n) 
{
  #define H_y_zoom_slider "(" S_y_zoom_slider " &optional snd chn) -> current y axis zoom slider of snd channel chn"
  ERRCP(S_y_zoom_slider,snd_n,chn_n,1); 
  return(cp_iread(snd_n,chn_n,AP_ZY));
}

static SCM g_edit_hook(SCM snd_n, SCM chn_n) 
{
  #define H_edit_hook "(" S_edit_hook " &optional snd chn) -> snd's channel chn's edit-hook"
  ERRCP(S_edit_hook,snd_n,chn_n,1); 
  return(cp_iread(snd_n,chn_n,EDIT_HOOK));
}

static SCM g_undo_hook(SCM snd_n, SCM chn_n) 
{
  #define H_undo_hook "(" S_undo_hook " &optional snd chn) -> snd's channel chn's undo-hook"
  ERRCP(S_undo_hook,snd_n,chn_n,1); 
  return(cp_iread(snd_n,chn_n,UNDO_HOOK));
}

static SCM g_peaks(SCM filename, SCM snd_n, SCM chn_n)
{
  #define H_peaks "(" S_peaks " &optional filename snd chn) writes current fft peaks data to filename, or\n\
   to the help dialog if filename is omitted"

  chan_info *cp;
  char *name = NULL;
  int err;
  ERRSB1(filename,S_peaks);
  ERRCP(S_peaks,snd_n,chn_n,2);
  cp = get_cp(snd_n,chn_n);
  if (cp == NULL) return(NO_SUCH_CHANNEL);
  if (gh_string_p(filename)) 
    name = full_filename(filename);
  else name = NULL;
  err = display_fft_peaks(cp,name);
  if (name) FREE(name);
  if ((gh_string_p(filename)) && (err == 0)) return(filename);
  return(SCM_BOOL_F);
}

static SCM g_left_sample(SCM snd_n, SCM chn_n) 
{
  #define H_left_sample "(" S_left_sample " &optional snd chn) -> left sample number in time domain window"
  ERRCP(S_left_sample,snd_n,chn_n,1); 
  return(cp_iread(snd_n,chn_n,LOSAMPF));
}

static SCM g_set_left_sample(SCM on, SCM snd_n, SCM chn_n) 
{
  #define H_set_left_sample "(" S_set_left_sample " val &optional snd chn) sets left sample number in time domain window to val"
  ERRB1(on,S_set_left_sample); 
  ERRCP(S_set_left_sample,snd_n,chn_n,2); 
  return(cp_iwrite(snd_n,chn_n,on,LOSAMPF));
}

static SCM g_right_sample(SCM snd_n, SCM chn_n) 
{
  #define H_right_sample "(" S_right_sample " &optional snd chn) -> right sample number in time domain window"
  ERRCP(S_right_sample,snd_n,chn_n,1); 
  return(cp_iread(snd_n,chn_n,HISAMPF));
}

static SCM g_set_right_sample(SCM on, SCM snd_n, SCM chn_n) 
{
  #define H_set_right_sample "(" S_set_right_sample " val &optional snd chn) sets right sample number in time domain window to val"
  ERRB1(on,S_set_right_sample); 
  ERRCP(S_set_right_sample,snd_n,chn_n,2); 
  return(cp_iwrite(snd_n,chn_n,on,HISAMPF));
}

static SCM g_edits(SCM snd_n, SCM chn_n)
{
  #define H_edits "(" S_edits " &optional snd chn) returns a vector of undoable and redoable edits in snd's channel chn"
  chan_info *cp;
  int i;
  ERRCP(S_edits,snd_n,chn_n,1);
  cp = get_cp(snd_n,chn_n);
  if (cp == NULL) return(NO_SUCH_CHANNEL);
  for (i=cp->edit_ctr+1;i<cp->edit_size;i++)
    if (!(cp->edits[i])) break;
  return(SCM_LIST2(gh_int2scm(cp->edit_ctr),gh_int2scm(i-cp->edit_ctr-1)));
}

static SCM g_find(SCM expr, SCM sample, SCM snd_n, SCM chn_n)
{
  #define H_find "(" S_find " func &optional (start-samp 0) snd chn) applyies func, a function of one argument,\n\
   the current sample, to each sample in snd's channel chn, starting at 'start-samp' until func returns #t"

  /* no free here -- it's handled as ss->search_expr in snd-find.c */
  chan_info *cp = NULL;
  SCM_ASSERT((gh_string_p(expr) || gh_procedure_p(expr)),expr,SCM_ARG1,S_find);
  ERRB2(sample,S_find);
  ERRCP(S_find,snd_n,chn_n,3);
  if (gh_string_p(expr))
    {
      cp = get_cp(snd_n,chn_n);
      if (cp) 
	RTNINT(snd_find_1(cp,gh_scm2newstr(expr,NULL),g_scm2intdef(sample,0),FALSE)); 
      else return(NO_SUCH_CHANNEL);
    }
  else return(g_scan_chan(expr,sample,SCM_BOOL_F,snd_n,chn_n));
  return(SCM_BOOL_F);
}

static SCM g_count_matches(SCM expr, SCM sample, SCM snd_n, SCM chn_n)
{
  #define H_count_matches "(" S_count_matches " func &optional (start-samp 0) snd chn) returns how many\n\
   samples satisfy func (a function of one argument, the current sample, returning #t upon match)"

  chan_info *cp = NULL;
  int samp = 0,matches,lim;
  SCM match,cursamp;
  SCM_ASSERT((gh_string_p(expr) || gh_procedure_p(expr)),expr,SCM_ARG1,S_count_matches);
  ERRB2(sample,S_count_matches);
  ERRCP(S_count_matches,snd_n,chn_n,3);
  cp = get_cp(snd_n,chn_n);
  if (cp == NULL) return(NO_SUCH_CHANNEL);
  samp = g_scm2intdef(sample,0);
  if (gh_string_p(expr))
    RTNINT(snd_find_1(cp,gh_scm2newstr(expr,NULL),g_scm2intdef(sample,0),TRUE));
  else
    {
      matches = 0;
      lim = current_ed_samples(cp);
      while (samp < lim)
	{
	  cursamp = gh_int2scm(samp);
	  match = g_scan_chan(expr,cursamp,SCM_BOOL_F,snd_n,chn_n);
	  if ((gh_list_p(match)) && (SCM_TRUE_P(scm_list_ref(match,gh_int2scm(0)))))
	    {
	      matches++;
	      samp = g_scm2int(scm_list_ref(match,gh_int2scm(1))) + 1;
	    }
	  else break;
	}
      return(gh_int2scm(matches));
    }
  return(SCM_BOOL_F);
}



static SCM g_find_sound(SCM filename)
{
  #define H_find_sound "(" S_find_sound " name) returns the id of the sound associated with file 'name'"
  char *fname = NULL;
  snd_info *sp;
  ERRS1(filename,S_find_sound);
  fname = gh_scm2newstr(filename,NULL);
  sp = find_sound(state,fname);
  if (fname) free(fname);
  if (sp) RTNINT(sp->index);
  return(SCM_BOOL_F);
}

static SCM g_set_x_bounds(SCM beg, SCM end, SCM snd_n, SCM chn_n)
{
  #define H_set_x_bounds "(" S_set_x_bounds " x0 x1 &optional snd chn) sets x axis bounds (seconds) for snd's channel chn"
  chan_info *cp;
  Float x0,x1;
  ERRN1(beg,S_set_x_bounds);
  ERRN2(end,S_set_x_bounds);
  ERRCP(S_set_x_bounds,snd_n,chn_n,3);
  cp = get_cp(snd_n,chn_n);
  if (cp == NULL) return(NO_SUCH_CHANNEL);
  x0 = gh_scm2double(beg);
  x1 = gh_scm2double(end);
  if (x1 > x0)
    set_x_axis_x0x1(cp,x0,x1);
  else return(IMPOSSIBLE_BOUNDS);
  return(SCM_BOOL_F);
}

static SCM g_set_y_bounds(SCM y0, SCM y1, SCM snd_n, SCM chn_n)
{
  #define H_set_y_bounds "(" S_set_y_bounds " &optional y0 y1 snd chn) sets y axis bounds for snd's channel chn.\n\
   if bounds are omitted, they're set to reflect current max amp"

  chan_info *cp;
  Float low,hi;
  ERRB1(y0,S_set_x_bounds);
  ERRB2(y1,S_set_x_bounds);
  ERRCP(S_set_y_bounds,snd_n,chn_n,3);
  cp = get_cp(snd_n,chn_n);
  if (cp == NULL) return(NO_SUCH_CHANNEL);
  if (gh_number_p(y0))
    {
      low = gh_scm2double(y0);
      if (gh_number_p(y1))
	hi = gh_scm2double(y1);
      else
	{
	  if (low < 0.0)
	    hi = -low;
	  else
	    {
	      hi = low;
	      low = -low;
	    }
	}
    }
  else
    {
      /* if no bounds given, use maxamp */
      hi = get_maxamp(cp->state,cp->sound,cp);
      if (hi < 0.0) hi = -hi;
      if (hi == 0.0) hi = .001;
      low = -hi;
    }
  if (hi > low)
    set_y_axis_y0y1(cp,low,hi);
  else return(IMPOSSIBLE_BOUNDS);
  return(SCM_BOOL_F);
}

static SCM g_x_bounds(SCM snd_n, SCM chn_n)
{
  #define H_x_bounds "(" S_x_bounds " &optional snd chn) returns a list (x0 x1) giving the current x axis bounds of snd channel chn"
  chan_info *cp;
  axis_info *ap;
  ERRCP(S_x_bounds,snd_n,chn_n,1);
  cp = get_cp(snd_n,chn_n);
  if (cp == NULL) return(NO_SUCH_CHANNEL);
  ap = cp->axis;
  return(SCM_LIST2(gh_double2scm(ap->x0),gh_double2scm(ap->x1)));
}

static SCM g_y_bounds(SCM snd_n, SCM chn_n)
{
  #define H_y_bounds "(" S_y_bounds " &optional snd chn) returns a list (y0 y1) giving the current y axis bounds of snd channel chn"
  chan_info *cp;
  axis_info *ap;
  ERRCP(S_y_bounds,snd_n,chn_n,1);
  cp = get_cp(snd_n,chn_n);
  if (cp == NULL) return(NO_SUCH_CHANNEL);
  ap = cp->axis;
  return(SCM_LIST2(gh_double2scm(ap->y0),gh_double2scm(ap->y1)));
}

static SCM samples2vct(SCM samp_0, SCM samps, SCM snd_n, SCM chn_n, SCM v, SCM pos)
{
  #define H_samples2vct "(" S_samples_vct " &optional (start-samp 0) samps snd chn vct-obj edit-position)\n\
   returns a vct object (vct-obj if given) containing snd channel chn's data starting at start-samp for samps,\n\
   reading edit version edit-position (defaulting to the current version)"

  chan_info *cp;
  snd_fd *sf;
  Float *fvals;
  int i,len,beg,edpos;
  MUS_SAMPLE_TYPE val;
  vct *v1 = get_vct(v);
  ERRB1(samp_0,S_samples_vct);
  ERRB2(samps,S_samples_vct);
  ERRCP(S_samples_vct,snd_n,chn_n,3);
  cp = get_cp(snd_n,chn_n);
  if (cp == NULL) return(NO_SUCH_CHANNEL);
  edpos = g_scm2intdef(pos,cp->edit_ctr);
  beg = g_scm2intdef(samp_0,0);
  len = g_scm2intdef(samps,cp->samples[edpos] - beg);
  if (v1)
    fvals = v1->data;
  else fvals = (Float *)CALLOC(len,sizeof(Float));
  sf = init_sample_read_any(beg,cp,READ_FORWARD,edpos);
  if (sf)
    {
      for (i=0;i<len;i++) 
	{
	  NEXT_SAMPLE(val,sf);
	  fvals[i] = MUS_SAMPLE_TO_FLOAT(val);
	}
      free_snd_fd(sf);
    }
  if (v1)
    return(v);
  else return(make_vct(len,fvals));
}

static SCM samples2sound_data(SCM samp_0, SCM samps, SCM snd_n, SCM chn_n, SCM sdobj, SCM pos, SCM sdchan)
{
  #define H_samples2sound_data "(" S_samples2sound_data " &optional (start-samp 0) samps snd chn sdobj edit-position (sdobj-chan 0))\n\
   returns a sound-data object (sdobj if given) containing snd channel chn's data starting at start-samp for samps,\n\
   reading edit version edit-position (defaulting to the current version)"

  chan_info *cp;
  snd_fd *sf;
  sound_data *sd;
  SCM newsd = SCM_BOOL_F;
  int i,len,beg,chn=0,edpos;
  ERRB1(samp_0,S_samples2sound_data);
  ERRB2(samps,S_samples2sound_data);
  ERRCP(S_samples2sound_data,snd_n,chn_n,3);
  cp = get_cp(snd_n,chn_n);
  if (cp == NULL) return(NO_SUCH_CHANNEL);
  edpos = g_scm2intdef(pos,cp->edit_ctr);
  if (edpos >= cp->edit_size) return(NO_SUCH_EDIT);
  beg = g_scm2intdef(samp_0,0);
  len = g_scm2intdef(samps,cp->samples[edpos] - beg);
  if (len > 0)
    {
      chn = g_scm2intdef(sdchan,0);
      if (sound_data_p(sdobj))
	sd = (sound_data *)GH_VALUE_OF(sdobj);
      else
	{
	  newsd = make_sound_data(chn+1,len);
	  sd = (sound_data *)GH_VALUE_OF(newsd);
	}
      if (chn < sd->chans)
	{
	  sf = init_sample_read_any(beg,cp,READ_FORWARD,edpos);
	  if (sf)
	    {
	      for (i=0;i<len;i++) {NEXT_SAMPLE(sd->data[chn][i],sf);}
	      free_snd_fd(sf);
	    }
	}
    }
  if (SCM_NFALSEP(newsd))
    return(newsd);
  return(sdobj);
}

static SCM transform_samples2vct(SCM snd_n, SCM chn_n, SCM v)
{
  #define H_transform_samples2vct "(" S_transform_samples_vct " &optional snd chn vct-obj) returns a vct object\n\
   (vct-obj if passed), with the current transform data from snd's channel chn"

  chan_info *cp;
  fft_info *fp;
  sono_info *si;
  int i,j,k,len,bins,slices;
  Float *fvals;
  vct *v1 = get_vct(v);
  ERRCP(S_transform_samples_vct,snd_n,chn_n,1);
  cp = get_cp(snd_n,chn_n);
  if (cp == NULL) return(NO_SUCH_CHANNEL);
  if (cp->ffting)
    {
      while (chan_fft_in_progress(cp)) {work_wait(cp->state);}
      if (fft_style(state) == NORMAL_FFT)
	{
	  fp = cp->fft;
	  len = fp->current_size;
	  if (v1)
	    fvals = v1->data;
	  else fvals = (Float *)CALLOC(len,sizeof(Float));
	  for (i=0;i<len;i++) fvals[i] = fp->data[i];
	  if (v1)
	    return(v);
	  else return(make_vct(len,fvals));
	}
      else
	{
	  si = (sono_info *)(cp->sonogram_data);
	  if (si)
	    {
	      slices = si->active_slices;
	      bins = si->target_bins;
	      len = bins * slices;
	      if (v1)
		fvals = v1->data;
	      else fvals = (Float *)CALLOC(len,sizeof(Float));
	      for (i=0,k=0;i<slices;i++)
		for (j=0;j<bins;j++,k++)
		  fvals[k] = si->data[i][j];
	      if (v1)
		return(v);
	      else return(make_vct(len,fvals));
	    }
	}
    }
  return(SCM_BOOL_F);
}  

static SCM region_samples2vct(SCM beg_n, SCM num, SCM reg_n, SCM chn_n, SCM v)
{
  #define H_region_samples2vct "(" S_region_samples_vct " &optional (beg 0) samps (region 0) (chan 0) obj) writes\n\
   region's samples starting at beg for samps in channel chan to vct obj, returning obj (or creating a new one)"

  Float *data;
  int len,reg,chn;
  vct *v1 = get_vct(v);
  finish_keyboard_selection();
  ERRB1(beg_n,S_region_samples_vct);
  ERRB2(num,S_region_samples_vct);
  ERRB3(reg_n,S_region_samples_vct);
  ERRB4(chn_n,S_region_samples_vct);
  reg = g_scm2intdef(reg_n,0);
  if (!(region_ok(reg))) return(NO_SUCH_REGION);
  chn = g_scm2intdef(chn_n,0);
  if (chn >= region_chans(reg)) return(NO_SUCH_CHANNEL);
  len = g_scm2intdef(num,0);
  if (len == 0) len = region_len(reg);
  if (len > 0)
    {
      if (v1)
	data = v1->data;
      else data = (Float *)CALLOC(len,sizeof(Float));
      region_samples(reg,chn,g_scm2intdef(beg_n,0),len,data);
      if (v1)
	return(v);
      else return(make_vct(len,data));
    }
  return(SCM_BOOL_F);
}

static SCM vct2soundfile(SCM g_fd, SCM obj, SCM g_nums)
{
  #define H_vct_sound_file "(" S_vct_sound_file " fd vct-obj samps) writes samps samples from vct-obj to the sound file controlled by fd"
  int fd,nums,i;
  float *vals;
  vct *v;
  ERRN1(g_fd,S_vct_sound_file);
  ERRVCT2(obj,S_vct_sound_file);
  ERRN3(g_nums,S_vct_sound_file);
  fd = g_scm2int(g_fd);
  nums = g_scm2int(g_nums);
  v = get_vct(obj);
  lseek(fd,0L,SEEK_END);
  if (sizeof(Float) == 4) /* Float can be either float or double */
    nums = write(fd,(char *)(v->data),nums * 4);
  else
    {
      /* v->data has doubles, but we're assuming elsewhere that these are floats in the file */
      vals = (float *)CALLOC(nums,sizeof(float));
      for (i=0;i<nums;i++)
	vals[i] = (float)(v->data[i]);
      write(fd,(char *)vals,nums * 4);
      FREE(vals);
    }
  return(scm_return_first(gh_int2scm(nums>>2),obj));
}


static SCM mix_vct(SCM obj, SCM beg, SCM in_chans, SCM snd, SCM chn, SCM with_consoles)
{
  #define H_mix_vct "(" S_mix_vct " data &optional (beg 0) (chans 1) snd chn (with-consoles #t)) mixes data\n\
   (a vct object) into snd's channel chn starting at beg; data has chans channels of interleaved data. returns\n\
   the new mix id"

  vct *v;
  int bg,chans;
  chan_info *cp[1];
  MUS_SAMPLE_TYPE **data;
  int i,j,k,len,num,mix_id=-1,with_mixers=1;
  ERRVCT1(obj,S_mix_vct);
  ERRCP(S_mix_vct,snd,chn,4);
  ERRB2(beg,S_mix_vct);
  ERRB3(in_chans,S_mix_vct);
  SCM_ASSERT((bool_or_arg_p(with_consoles)),with_consoles,SCM_ARG6,S_mix_vct);
  v = get_vct(obj);
  if (v)
    {
      len = v->length;
      cp[0] = get_cp(snd,chn);
      if (cp[0] == NULL) return(NO_SUCH_CHANNEL);
      bg = g_scm2intdef(beg,0);
      chans = g_scm2intdef(in_chans,1);
      if (chans <= 0) 
	scm_misc_error(S_mix_vct,"input channels = ~S?",in_chans);
      else
	{
	  if (bg < 0)
	    scm_misc_error(S_mix_vct,"beg = ~S?",beg);
	  else
	    {
	      if (SCM_UNBNDP(with_consoles))
		with_mixers = with_mix_consoles(state);
	      else with_mixers = bool_int_or_one(with_consoles);
	      num = len/chans;
	      data = (MUS_SAMPLE_TYPE **)CALLOC(chans,sizeof(MUS_SAMPLE_TYPE *));
	      for (i=0;i<chans;i++)
		data[i] = (MUS_SAMPLE_TYPE *)CALLOC(num,sizeof(MUS_SAMPLE_TYPE));
	      if (chans == 1)
		{
		  for (i=0;i<len;i++)
		    data[0][i] = MUS_FLOAT_TO_SAMPLE(v->data[i]);
		}
	      else
		{
		  for (i=0,k=0;i<len;i+=chans,k++)
		    for (j=0;j<chans;j++)
		      data[j][k] = MUS_FLOAT_TO_SAMPLE(v->data[i+j]);
		}
	      mix_id = mix_array(bg,num,data,cp,chans,1,SND_SRATE(cp[0]->sound),S_mix_vct,with_mixers);
	      for (i=0;i<chans;i++) FREE(data[i]);
	      FREE(data);
	    }
	}
    }
  scm_remember(&obj);
  return(gh_int2scm(mix_id));
}

MUS_SAMPLE_TYPE *g_floats_to_samples(SCM obj, int *size, char *caller, int position)
{
  MUS_SAMPLE_TYPE *vals = NULL;
  vct *v;
  int i,num = 0;
  if (gh_list_p(obj))
    {
      if ((*size) == 0) num = gh_length(obj); else num = (*size);
      vals = (MUS_SAMPLE_TYPE *)CALLOC(num,sizeof(MUS_SAMPLE_TYPE));
      for (i=0;i<num;i++) vals[i] = MUS_FLOAT_TO_SAMPLE(gh_scm2double(scm_list_ref(obj,gh_int2scm(i))));
    }
  else
    {
      if (gh_vector_p(obj))
	{
	  if ((*size) == 0) num = gh_vector_length(obj); else num = (*size);
	  vals = (MUS_SAMPLE_TYPE *)CALLOC(num,sizeof(MUS_SAMPLE_TYPE));
	  for (i=0;i<num;i++) vals[i] = MUS_FLOAT_TO_SAMPLE(gh_scm2double(gh_vector_ref(obj,gh_int2scm(i))));
	}
      else
	{
	  if (vct_p(obj))
	    {
	      v = get_vct(obj);
	      if ((*size) == 0) num = v->length; else num = (*size);
	      vals = (MUS_SAMPLE_TYPE *)CALLOC(num,sizeof(MUS_SAMPLE_TYPE));
	      for (i=0;i<num;i++) vals[i] = MUS_FLOAT_TO_SAMPLE(v->data[i]);
	    }
	  else scm_wrong_type_arg(caller,position,obj);
	}
    }
  (*size) = num;
  scm_remember(&obj);
  return(vals);
}

static SCM g_sample(SCM samp_n, SCM snd_n, SCM chn_n)
{
  #define H_sample "(" S_sample " samp &optional snd chn) -> sample samp in snd's channel chn (slow access -- use sample-readers for speed)"
  chan_info *cp;
  ERRN1(samp_n,S_sample);
  ERRCP(S_sample,snd_n,chn_n,2);
  cp = get_cp(snd_n,chn_n);
  if (cp) 
    RTNFLT(sample(g_scm2int(samp_n),cp));
  else return(NO_SUCH_CHANNEL);
}


/* ---------------- sample readers ---------------- */

static int sf_tag = 0;
static SCM mark_sf(SCM obj) {SCM_SETGC8MARK(obj); return(SCM_BOOL_F);}

int sf_p(SCM obj); /* currently for snd-ladspa.c */
int sf_p(SCM obj) {return((SCM_NIMP(obj)) && (GH_TYPE_OF(obj) == (SCM)sf_tag));}

static SCM g_sf_p(SCM obj) 
{
  #define H_sf_p "(" S_sample_readerQ " obj) -> #t if obj is a sample-reader"
  RTNBOOL(sf_p(obj));
}

snd_fd *get_sf(SCM obj); /* currently for snd-ladspa.c */
snd_fd *get_sf(SCM obj) {if (sf_p(obj)) return((snd_fd *)GH_VALUE_OF(obj)); else return(NULL);}

static int print_sf(SCM obj, SCM port, scm_print_state *pstate) 
{
  char *desc;
  chan_info *cp;
  snd_fd *fd;
  fd = get_sf(obj);
  if (fd == NULL)
    scm_puts("<null>",port);
  else
    {
      cp = fd->cp;
      desc = (char *)CALLOC(128,sizeof(char));
      sprintf(desc,"<sample-reader %p: %s from %d, at %d (%.4f)",
	      fd,
	      (cp) ? ((cp->sound)->shortname) : "unknown source?",
	      fd->initial_samp,
	      current_location(fd),
	      MUS_SAMPLE_TO_FLOAT(fd->current_value));
      scm_puts(desc,port); 
      FREE(desc);
    }
  return(1);
}

static SCM equalp_sf(SCM obj1, SCM obj2) 
{
  RTNBOOL(get_sf(obj1) == get_sf(obj2));
}

static scm_sizet free_sf(SCM obj) 
{
  snd_fd *fd = (snd_fd *)GH_VALUE_OF(obj); 
  if (fd) 
    {
#ifdef DEBUGGING
      snd_warning("Guile's GC is freeing a sample reader!");
#endif
      free_snd_fd(fd); 
    }
  return(0);
}

#if HAVE_GUILE_1_3_0
static scm_smobfuns sf_smobfuns = {
  &mark_sf,
  &free_sf,
  &print_sf,
  &equalp_sf};
#endif

static SCM g_sample_reader_at_end(SCM obj) 
{
  #define H_sample_reader_at_end "(" S_sample_reader_at_endQ " obj) -> #t if sample-reader has reached the end of its data"
  SCM_ASSERT(sf_p(obj),obj,SCM_ARG1,S_sample_reader_at_endQ);
  RTNBOOL(read_sample_eof(get_sf(obj)));
}

SCM g_c_make_sample_reader(snd_fd *fd)
{
#if (!HAVE_GUILE_1_3_0)
  SCM_RETURN_NEWSMOB(sf_tag,(SCM)fd);
#else
  SCM new_sf;
  SCM_NEWCELL(new_sf);
  SCM_SETCDR(new_sf,(SCM)fd);
  SCM_SETCAR(new_sf,sf_tag);
  return(new_sf);
#endif
}

static SCM g_make_sample_reader(SCM samp_n, SCM snd, SCM chn, SCM dir, SCM pos)
{
  #define H_make_sample_reader "(" S_make_sample_reader " &optional (start-samp 0) snd chn (dir 1) edit-position)\n\
   returns a reader ready to access snd's channel chn's data starting at 'start-samp', going in direction 'dir'\n\
   (-1 = backward), reading the version of the data indicated by 'edit-position' which defaults to the current version"

  snd_fd *fd = NULL;
  int edpos;
  chan_info *cp;
#if HAVE_GUILE_1_3_0
  SCM new_sf;
#endif
  ERRB1(samp_n,S_make_sample_reader);
  ERRCP(S_make_sample_reader,snd,chn,2);
  ERRB4(dir,S_make_sample_reader);
  cp = get_cp(snd,chn);
  if (cp == NULL) return(NO_SUCH_CHANNEL);
  edpos = g_scm2intdef(pos,cp->edit_ctr);
  fd = init_sample_read_any(g_scm2intdef(samp_n,0),cp,g_scm2intdef(dir,1),edpos);
#if (!HAVE_GUILE_1_3_0)
  SCM_RETURN_NEWSMOB(sf_tag,(SCM)fd);
#else
  SCM_NEWCELL(new_sf);
  SCM_SETCDR(new_sf,(SCM)fd);
  SCM_SETCAR(new_sf,sf_tag);
  return(new_sf);
#endif
  return(SCM_BOOL_F);
}

static SCM g_make_region_sample_reader(SCM samp_n, SCM reg, SCM chn, SCM dir)
{
  #define H_make_region_sample_reader "(" S_make_region_sample_reader " &optional (start-samp 0) (region 0) chn (dir 1))\n\
   returns a reader ready to access region's channel chn data starting at 'start-samp' going in direction 'dir'"

  snd_fd *fd = NULL;
#if HAVE_GUILE_1_3_0
  SCM new_sf;
#endif
  ERRB1(samp_n,S_make_sample_reader);
  ERRB2(reg,S_make_sample_reader);
  ERRB3(chn,S_make_sample_reader);
  ERRB4(dir,S_make_sample_reader);
  fd = init_region_read(state,g_scm2intdef(samp_n,0),g_scm2intdef(reg,0),g_scm2intdef(chn,0),g_scm2intdef(dir,1));
  if (fd)
    {
#if (!HAVE_GUILE_1_3_0)
      SCM_RETURN_NEWSMOB(sf_tag,(SCM)fd);
#else
      SCM_NEWCELL(new_sf);
      SCM_SETCDR(new_sf,(SCM)fd);
      SCM_SETCAR(new_sf,sf_tag);
      return(new_sf);
#endif
    }
  return(SCM_BOOL_F);
}

static SCM g_next_sample(SCM obj)
{
  #define H_next_sample "(" S_next_sample " reader) -> next sample from reader"
  SCM_ASSERT(sf_p(obj),obj,SCM_ARG1,S_next_sample);
  return(gh_double2scm(next_sample(get_sf(obj))));
}

static SCM g_previous_sample(SCM obj)
{
  #define H_previous_sample "(" S_previous_sample " reader) -> previous sample from reader"
  SCM_ASSERT(sf_p(obj),obj,SCM_ARG1,S_previous_sample);
  return(gh_double2scm(previous_sample(get_sf(obj))));
}

static SCM g_free_sample_reader(SCM obj)
{
  #define H_free_sample_reader "(" S_free_sample_reader " reader) frees sample reader 'reader'"
  SCM_ASSERT(sf_p(obj),obj,SCM_ARG1,S_free_sample_reader);
  free_snd_fd(get_sf(obj));
  GH_SET_VALUE_OF(obj,(SCM)NULL);
  return(SCM_BOOL_F);
}

typedef Float (*g_plug)(Float val);

static SCM g_loop_samples(SCM reader, SCM proc, SCM calls, SCM origin)
{
  #define H_loop_samples "(" S_loop_samples " reader func calls origin) calls (func (reader)) 'calls' times,\n\
   replacing current data with the function results; origin is the edit-history name for this operation"

  /* proc here is a pointer to a float procedure that takes a float arg */
  g_plug func;
  chan_info *cp;
  snd_info *sp;
  char *ofile;
  int num,i,j=0,ofd,datumb,err=0;
  MUS_SAMPLE_TYPE val;
  snd_fd *sf;
  file_info *hdr;
  MUS_SAMPLE_TYPE **data;
  MUS_SAMPLE_TYPE *idata;
  SCM_ASSERT(sf_p(reader),reader,SCM_ARG1,S_loop_samples);
  SCM_ASSERT(gh_number_p(calls),calls,SCM_ARG3,S_loop_samples);
  SCM_ASSERT(gh_string_p(origin),origin,SCM_ARG4,S_loop_samples);
  num = g_scm2int(calls);
  sf = get_sf(reader);
  cp = sf->cp;
  func = (g_plug)gh_scm2ulong(proc);
  ofile = snd_tempnam(state);
  sp = (cp->sound);
  hdr = make_temp_header(cp->state,ofile,sp->hdr,num);
  hdr->chans = 1;
  ofd = open_temp_file(ofile,1,hdr,state);
  datumb = mus_data_format_to_bytes_per_sample(hdr->format);
  data = (MUS_SAMPLE_TYPE **)CALLOC(1,sizeof(MUS_SAMPLE_TYPE *));
  data[0] = (MUS_SAMPLE_TYPE *)CALLOC(MAX_BUFFER_SIZE,sizeof(MUS_SAMPLE_TYPE)); 
  idata = data[0];
  for (i=0;i<num;i++)
    {
      NEXT_SAMPLE(val,sf);
      idata[j] = MUS_FLOAT_TO_SAMPLE((*func)(MUS_SAMPLE_TO_FLOAT(val)));
      j++;
      if (j == MAX_BUFFER_SIZE)
	{
	  err = mus_file_write(ofd,0,j-1,1,data);
	  j=0;
	  if (err == -1) break;
	  if (state->stopped_explicitly) break;
	}
    }
  if (j > 0) mus_file_write(ofd,0,j-1,1,data);
  close_temp_file(ofd,hdr,num*datumb,sp);
  hdr = free_file_info(hdr);
  file_change_samples(sf->initial_samp,num,ofile,cp,0,DELETE_ME,LOCK_MIXES,gh_scm2newstr(origin,NULL));
  update_graph(cp,NULL); /* is this needed? */
  if (ofile) free(ofile);
  FREE(data[0]);
  FREE(data);
  return(SCM_BOOL_F);
}


/* ---------------- mix sample readers ---------------- */

static int mf_tag = 0;
static SCM mark_mf(SCM obj) {SCM_SETGC8MARK(obj); return(SCM_BOOL_F);}
static int mf_p(SCM obj) {return((SCM_NIMP(obj)) && (GH_TYPE_OF(obj) == (SCM)mf_tag));}

static SCM g_mf_p(SCM obj) 
{
  #define H_mf_p "(" S_mix_sample_readerQ " obj) -> #t if obj is a mix-sample-reader"
  RTNBOOL(mf_p(obj));
}

static mix_fd *get_mf(SCM obj) {if (mf_p(obj)) return((mix_fd *)GH_VALUE_OF(obj)); else return(NULL);}
static SCM equalp_mf(SCM obj1, SCM obj2) {RTNBOOL(get_mf(obj1) == get_mf(obj2));}

static int print_mf(SCM obj, SCM port, scm_print_state *pstate) 
{
  mix_fd *fd;
  mixdata *md;
  char *desc;
  fd = get_mf(obj);
  if (fd == NULL)
    scm_puts("<null>",port);
  else
    {
      md = fd->md;
      desc = (char *)CALLOC(128,sizeof(char));
      sprintf(desc,"<mix-sample-reader %p: %s via mix %d>",
	      fd,
	      md->in_filename,
	      md->id);
      scm_puts(desc,port); 
      FREE(desc);
    }
  return(1);
}

static scm_sizet free_mf(SCM obj) 
{
  mix_fd *fd = (mix_fd *)GH_VALUE_OF(obj); 
  if (fd) 
    {
#ifdef DEBUGGING
      snd_warning("Guile's GC is freeing a mix sample reader!");
#endif
      free_mix_fd(fd); 
    }
  return(0);
}

#if HAVE_GUILE_1_3_0
static scm_smobfuns mf_smobfuns = {
  &mark_mf,
  &free_mf,
  &print_mf,
  &equalp_mf};
#endif

static SCM g_make_mix_sample_reader(SCM mix_id)
{
  #define H_make_mix_sample_reader "(" S_make_mix_sample_reader " id) returns a reader ready to access mix 'id'"
  mixdata *md = NULL;
  mix_fd *mf = NULL;
#if HAVE_GUILE_1_3_0
  SCM new_mf;
#endif
  ERRN1(mix_id,S_make_mix_sample_reader);
  md = md_from_int(g_scm2int(mix_id));
  if (md) mf = init_mix_read(md,FALSE); else return(NO_SUCH_MIX);
  if (mf)
    {
#if (!HAVE_GUILE_1_3_0)
      SCM_RETURN_NEWSMOB(mf_tag,(SCM)mf);
#else
      SCM_NEWCELL(new_mf);
      SCM_SETCDR(new_mf,(SCM)mf);
      SCM_SETCAR(new_mf,mf_tag);
      return(new_mf);
#endif
    }
  return(SCM_BOOL_F);
}

static SCM g_next_mix_sample(SCM obj)
{
  #define H_next_mix_sample "(" S_next_mix_sample " reader) -> next sample from mix reader"
  SCM_ASSERT(mf_p(obj),obj,SCM_ARG1,S_next_mix_sample);
  return(gh_double2scm(MUS_SAMPLE_TO_FLOAT(next_mix_sample(get_mf(obj)))));
}

static SCM g_free_mix_sample_reader(SCM obj)
{
  #define H_free_mix_sample_reader "(" S_free_mix_sample_reader " reader) frees mix sample reader 'reader'"
  SCM_ASSERT(mf_p(obj),obj,SCM_ARG1,S_free_mix_sample_reader);
  free_mix_fd(get_mf(obj));
  GH_SET_VALUE_OF(obj,(SCM)NULL);
  return(SCM_BOOL_F);
}



/* ---------------- track sample readers ---------------- */

static int tf_tag = 0;
static SCM mark_tf(SCM obj) {SCM_SETGC8MARK(obj); return(SCM_BOOL_F);}
static int tf_p(SCM obj) {return((SCM_NIMP(obj)) && (GH_TYPE_OF(obj) == (SCM)tf_tag));}

static SCM g_tf_p(SCM obj) 
{
  #define H_tf_p "(" S_track_sample_readerQ " obj) -> #t if obj is a track-sample-reader"
  RTNBOOL(tf_p(obj));
}

static track_fd *get_tf(SCM obj) {if (tf_p(obj)) return((track_fd *)GH_VALUE_OF(obj)); else return(NULL);}
static SCM equalp_tf(SCM obj1, SCM obj2) {RTNBOOL(get_tf(obj1) == get_tf(obj2));}

static int print_tf(SCM obj, SCM port, scm_print_state *pstate) 
{
  track_fd *fd;
  mixdata *md;
  mix_fd *mf;
  char *desc;
  int i,len;
  fd = get_tf(obj);
  if (fd == NULL)
    scm_puts("<null>",port);
  else
    {
      desc = (char *)CALLOC(128,sizeof(char));
      mf = fd->fds[0];
      md = mf->md;
      sprintf(desc,"<track-sample-reader %p: %s chan %d via mixes '(",
	      fd,
	      md->in_filename,
	      (md->cp)->chan);
      scm_puts(desc,port); 
      len = fd->mixes;
      if (len > 0)
	{
	  for (i=0;i<len-1;i++)
	    {
	      mf = fd->fds[i];
	      sprintf(desc,"%d ",(mf->md)->id);
	      scm_puts(desc,port); 
	    }
	  mf = fd->fds[len-1];
	  sprintf(desc,"%d)>",(mf->md)->id);
	}
      else sprintf(desc,")>");
      scm_puts(desc,port); 
      FREE(desc);
    }
  return(1);
}

static scm_sizet free_tf(SCM obj) 
{
  track_fd *fd = (track_fd *)GH_VALUE_OF(obj); 
  if (fd) 
    {
#ifdef DEBUGGING
      snd_warning("Guile's GC is freeing a track sample reader!");
#endif
      free_track_fd(fd); 
    }
  return(0);
}

#if HAVE_GUILE_1_3_0
static scm_smobfuns tf_smobfuns = {
  &mark_tf,
  &free_tf,
  &print_tf,
  &equalp_tf};
#endif

static SCM g_make_track_sample_reader(SCM track_id, SCM samp, SCM snd, SCM chn)
{
  #define H_make_track_sample_reader "(" S_make_track_sample_reader " track &optional (start-samp 0) snd chn)\n\
   returns a reader ready to access track's data associated with snd's channel chn starting at 'start-samp'"

  track_fd *tf = NULL;
  chan_info *cp;
#if HAVE_GUILE_1_3_0
  SCM new_tf;
#endif
  ERRN1(track_id,S_make_track_sample_reader);
  ERRCP(S_make_track_sample_reader,snd,chn,3); 
  cp = get_cp(snd,chn);
  tf = init_track_reader(cp,g_scm2int(track_id),g_scm2intdef(samp,0));
  if (tf)
    {
#if (!HAVE_GUILE_1_3_0)
      SCM_RETURN_NEWSMOB(tf_tag,(SCM)tf);
#else
      SCM_NEWCELL(new_tf);
      SCM_SETCDR(new_tf,(SCM)tf);
      SCM_SETCAR(new_tf,tf_tag);
      return(new_tf);
#endif
    }
  return(SCM_BOOL_F);
}

static SCM g_next_track_sample(SCM obj)
{
  #define H_next_track_sample "(" S_next_track_sample " reader) -> next sample from track reader"
  SCM_ASSERT(tf_p(obj),obj,SCM_ARG1,S_next_track_sample);
  return(gh_double2scm(MUS_SAMPLE_TO_FLOAT(next_track_sample(get_tf(obj)))));
}

static SCM g_free_track_sample_reader(SCM obj)
{
  #define H_free_track_sample_reader "(" S_free_track_sample_reader " reader) frees the track sample reader 'reader'"
  SCM_ASSERT(tf_p(obj),obj,SCM_ARG1,S_free_track_sample_reader);
  free_track_fd(get_tf(obj));
  GH_SET_VALUE_OF(obj,(SCM)NULL);
  return(SCM_BOOL_F);
}

static SCM g_play_track(SCM num, SCM snd, SCM chn)
{
  #define H_play_track "(" S_play_track " track &optional snd chn) plays track"
  /* just a dummy for testing */
  chan_info *cp;
  /* in this case if snd=#t, play all associated mixes in all chans */
  if (SCM_TRUE_P(snd))
    play_track(state,NULL,0,g_scm2int(num));
  else 
    {
      cp = get_cp(snd,chn);
      if (cp) play_track(state,&cp,1,g_scm2int(num)); else return(NO_SUCH_CHANNEL);
    }
  return(num);
}

static SCM g_play_mix(SCM num)
{
  #define H_play_mix "(" S_play_mix " id) plays mix"
  mixdata *md;
  md = md_from_int(g_scm2intdef(num,0));
  if (md) play_mix(state,md); else return(NO_SUCH_MIX);
  return(num);
}


static SCM g_set_sample(SCM samp_n, SCM val, SCM snd_n, SCM chn_n)
{
  #define H_set_sample "(" S_set_sample " samp new-value &optional snd chn) sets snd channel chn's sample samp to new-value"
  /* each call consitutes a separate edit from the undo/redo point-of-view */
  chan_info *cp;
  MUS_SAMPLE_TYPE ival[1];
  ERRN1(samp_n,S_set_sample);
  ERRN2(val,S_set_sample);
  ERRCP(S_set_sample,snd_n,chn_n,3);
  cp = get_cp(snd_n,chn_n);
  if (cp == NULL) return(NO_SUCH_CHANNEL);
  ival[0] = MUS_FLOAT_TO_SAMPLE(gh_scm2double(val));
  change_samples(g_scm2int(samp_n),1,ival,cp,LOCK_MIXES,S_set_sample);
  update_graph(cp,NULL);
  return(val);
}

static SCM g_samples(SCM samp_0, SCM samps, SCM snd_n, SCM chn_n, SCM pos)
{
  #define H_samples "(" S_samples " &optional (start-samp 0) samps snd chn edit-position) returns a vector\n\
   containing snd channel chn's samples starting a start-samp for samps samples; edit-position is the edit\n\
   history position to read (defaults to current position)."

  /* return the filled vector for scm to free? */
  chan_info *cp;
  snd_fd *sf;
  int i,len,beg,edpos;
  MUS_SAMPLE_TYPE val;
  SCM new_vect;
  ERRB1(samp_0,S_samples);
  ERRB2(samps,S_samples);
  ERRCP(S_samples,snd_n,chn_n,3);
  cp = get_cp(snd_n,chn_n);
  if (cp == NULL) return(NO_SUCH_CHANNEL);
  edpos = g_scm2intdef(pos,cp->edit_ctr);
  beg = g_scm2intdef(samp_0,0);
  len = g_scm2intdef(samps,cp->samples[edpos] - beg);
  new_vect = gh_make_vector(samps,gh_double2scm(0.0));
  sf = init_sample_read_any(beg,cp,READ_FORWARD,edpos);
  if (sf)
    {
      for (i=0;i<len;i++) 
	{
	  NEXT_SAMPLE(val,sf);
	  gh_vector_set_x(new_vect,gh_int2scm(i),gh_double2scm(MUS_SAMPLE_TO_FLOAT(val)));
	}
      free_snd_fd(sf);
    }
  return(new_vect);
}

static SCM g_set_samples(SCM samp_0, SCM samps, SCM vect, SCM snd_n, SCM chn_n)
{
  #define H_set_samples "(" S_set_samples " start-samp samps data &optional snd chn) sets snd's channel chn's samples\n\
   starting at start-samp for samps from data (a vct, vector, or string (filename)); start-samp can be beyond current data end"

  chan_info *cp;
  MUS_SAMPLE_TYPE *ivals;
  int len,beg,curlen;
  char *fname;
  ERRN1(samp_0,S_set_samples);
  ERRN2(samps,S_set_samples);
  ERRCP(S_set_samples,snd_n,chn_n,4);
  cp = get_cp(snd_n,chn_n);
  if (cp == NULL) return(NO_SUCH_CHANNEL);
  beg = g_scm2int(samp_0);
  len = g_scm2int(samps);
  if (gh_string_p(vect))
    {
      curlen = current_ed_samples(cp);
      fname = gh_scm2newstr(vect,NULL);
      if ((beg == 0) && (len > curlen))
	file_override_samples(len,fname,cp,0,DELETE_ME,LOCK_MIXES,S_set_samples);
      else file_change_samples(beg,len,fname,cp,0,DELETE_ME,LOCK_MIXES,S_set_samples);
      free(fname);
    }
  else
    {
      ivals = g_floats_to_samples(vect,&len,S_set_samples,3);
      change_samples(beg,len,ivals,cp,LOCK_MIXES,S_set_samples);
      FREE(ivals);
    }
  update_graph(cp,NULL);
  return(vect);
}

static SCM g_change_samples_with_origin(SCM samp_0, SCM samps, SCM origin, SCM vect, SCM snd_n, SCM chn_n)
{
  chan_info *cp;
  MUS_SAMPLE_TYPE *ivals;
  char *str,*fstr;
  int i,len,beg;
  ERRN1(samp_0,S_change_samples_with_origin);
  ERRN2(samps,S_change_samples_with_origin);
  ERRS3(origin,S_change_samples_with_origin);
  SCM_ASSERT((gh_vector_p(vect)) || (gh_string_p(vect)),vect,SCM_ARG4,S_change_samples_with_origin);
  ERRCP(S_change_samples_with_origin,snd_n,chn_n,5);
  cp = get_cp(snd_n,chn_n);
  if (cp == NULL) return(NO_SUCH_CHANNEL);
  beg = g_scm2int(samp_0);
  len = g_scm2int(samps);
  str = gh_scm2newstr(origin,NULL);
  if (gh_vector_p(vect))
    {
      ivals = (MUS_SAMPLE_TYPE *)CALLOC(len,sizeof(MUS_SAMPLE_TYPE));
#if SNDLIB_USE_FLOATS
      for (i=0;i<len;i++) ivals[i] = gh_scm2double(gh_vector_ref(vect,gh_int2scm(i)));
#else
      for (i=0;i<len;i++) ivals[i] = g_scm2int(gh_vector_ref(vect,gh_int2scm(i)));
#endif
      change_samples(beg,len,ivals,cp,LOCK_MIXES,str);
      FREE(ivals);
    }
  else
    {
      /* string = filename here */
      fstr = gh_scm2newstr(vect,NULL);
      file_change_samples(beg,len,fstr,cp,0,0,1,str);
      free(fstr);
    }
  free(str);
  update_graph(cp,NULL);
  return(vect);
}

static SCM g_delete_sample(SCM samp_n, SCM snd_n, SCM chn_n)
{
  #define H_delete_sample "(" S_delete_sample " samp &optional snd chn) deletes sample 'samp' from snd's channel chn"
  chan_info *cp;
  int samp;
  ERRN1(samp_n,S_delete_sample);
  ERRCP(S_delete_sample,snd_n,chn_n,2);
  cp = get_cp(snd_n,chn_n);
  if (cp == NULL) return(NO_SUCH_CHANNEL);
  samp = g_scm2int(samp_n);
  if ((samp >= 0) && (samp <= current_ed_samples(cp)))
    {
      delete_samples(samp,1,cp,S_delete_sample);
      update_graph(cp,NULL);
      return(SCM_BOOL_T);
    }
  return(NO_SUCH_SAMPLE);
}

static SCM g_delete_samples_1(SCM samp_n, SCM samps, SCM snd_n, SCM chn_n, char *origin)
{
  chan_info *cp;
  ERRN1(samp_n,origin);
  ERRN2(samps,origin);
  ERRCP(origin,snd_n,chn_n,3);
  cp = get_cp(snd_n,chn_n);
  if (cp == NULL) return(NO_SUCH_CHANNEL);
  delete_samples(g_scm2int(samp_n),g_scm2int(samps),cp,origin);
  update_graph(cp,NULL);
  return(SCM_BOOL_T);
}

static SCM g_delete_samples(SCM samp_n, SCM samps, SCM snd_n, SCM chn_n)
{
  #define H_delete_samples "(" S_delete_samples " start-samp samps &optional snd chn) deletes 'samps' samples\n\
   from snd's channel chn starting at 'start-samp'"

  return(g_delete_samples_1(samp_n,samps,snd_n,chn_n,S_delete_samples));
}

static SCM g_delete_samples_with_origin(SCM samp_n, SCM samps, SCM origin, SCM snd_n, SCM chn_n)
{
  char *str;
  SCM res;
  ERRS3(origin,S_delete_samples_with_origin);
  str = gh_scm2newstr(origin,NULL);
  res = g_delete_samples_1(samp_n,samps,snd_n,chn_n,str);
  free(str);
  return(res);
}

static SCM g_insert_sample(SCM samp_n, SCM val, SCM snd_n, SCM chn_n)
{
  #define H_insert_sample "(" S_insert_sample " sample value &optional snd chn) inserts 'value' at 'sample' in snd's channel chn"
  chan_info *cp;
  int beg;
  MUS_SAMPLE_TYPE ival[1];
  ERRN1(samp_n,S_insert_sample);
  ERRN2(val,S_insert_sample);
  ERRCP(S_insert_sample,snd_n,chn_n,3);
  cp = get_cp(snd_n,chn_n);
  if (cp == NULL) return(NO_SUCH_CHANNEL);
  beg = g_scm2int(samp_n);
  if (beg < 0) return(NO_SUCH_SAMPLE);
  ival[0] = MUS_FLOAT_TO_SAMPLE(gh_scm2double(val));
  insert_samples(g_scm2int(samp_n),1,ival,cp,S_insert_sample);
  update_graph(cp,NULL);
  return(val);
}

static SCM g_insert_samples(SCM samp, SCM samps, SCM vect, SCM snd_n, SCM chn_n)
{
  #define H_insert_samples "(" S_insert_samples " start-samp samps data &optional snd chn) inserts data (either\n\
   a vector, vct, or list of samples, or a filename) into snd's channel chn starting at 'start-samp' for 'samps' samples"

  chan_info *cp;
  MUS_SAMPLE_TYPE *ivals;
  char *fname;
  int beg,len;
  ERRN1(samp,S_insert_samples);
  ERRN2(samps,S_insert_samples);
  ERRCP(S_insert_samples,snd_n,chn_n,4);
  cp = get_cp(snd_n,chn_n);
  if (cp == NULL) return(NO_SUCH_CHANNEL);
  beg = g_scm2int(samp);
  len = g_scm2int(samps);
  if (gh_string_p(vect))
    {
      fname = gh_scm2newstr(vect,NULL);
      file_insert_samples(beg,len,fname,cp,0,DELETE_ME,S_insert_samples);
      free(fname);
    }
  else
    {
      ivals = g_floats_to_samples(vect,&len,S_insert_samples,3);
      insert_samples(beg,len,ivals,cp,S_insert_samples);
      FREE(ivals);
    }
  update_graph(cp,NULL);
  RTNINT(len);
}

static SCM g_insert_samples_with_origin(SCM samp, SCM samps, SCM origin, SCM vect, SCM snd_n, SCM chn_n)
{
  chan_info *cp;
  MUS_SAMPLE_TYPE *ivals;
  char *str,*fstr;
  int i,beg,len;
  ERRN1(samp,S_insert_samples_with_origin);
  ERRN2(samps,S_insert_samples_with_origin);
  ERRS3(origin,S_insert_samples_with_origin);
  SCM_ASSERT((gh_vector_p(vect)) || (gh_string_p(vect)),vect,SCM_ARG4,S_insert_samples_with_origin);
  ERRCP(S_insert_samples_with_origin,snd_n,chn_n,5);
  cp = get_cp(snd_n,chn_n);
  if (cp == NULL) return(NO_SUCH_CHANNEL);
  beg = g_scm2int(samp);
  len = g_scm2int(samps);
  str = gh_scm2newstr(origin,NULL);
  if (gh_vector_p(vect))
    {
      ivals = (MUS_SAMPLE_TYPE *)CALLOC(len,sizeof(MUS_SAMPLE_TYPE));
#if SNDLIB_USE_FLOATS
      for (i=0;i<len;i++) ivals[i] = gh_scm2double(gh_vector_ref(vect,gh_int2scm(i)));
#else
      for (i=0;i<len;i++) ivals[i] = g_scm2int(gh_vector_ref(vect,gh_int2scm(i)));
#endif
      insert_samples(beg,len,ivals,cp,str);
      FREE(ivals);
    }
  else
    {
      fstr = gh_scm2newstr(vect,NULL);
      file_insert_samples(beg,len,fstr,cp,0,0,str);
      free(fstr);
    }
  free(str);
  update_graph(cp,NULL);
  RTNINT(len);
}

static SCM g_active_sounds(void)
{
  #define H_active_sounds "(" S_active_sounds ") -> the number of sounds currently active (open and displayed)"
  int i,num;
  num = 0;
  for (i=0;i<state->max_sounds;i++) if (snd_ok(state->sounds[i])) num++;
  RTNINT(num);
}

static SCM g_undo(SCM ed_n, SCM snd_n, SCM chn_n) /* opt ed_n */
{
  #define H_undo "("  S_undo " &optional (count 1) snd chn) undoes count edits in snd's channel chn"
  chan_info *cp;
  ERRCP(S_undo,snd_n,chn_n,2);
  cp = get_cp(snd_n,chn_n);
  if (cp == NULL) return(NO_SUCH_CHANNEL);
  if (gh_number_p(ed_n))
    undo_EDIT(cp,g_scm2int(ed_n));
  else undo_EDIT(cp,1);
  update_graph(cp,NULL);
  return(SCM_BOOL_T);
}

static SCM g_redo(SCM ed_n, SCM snd_n, SCM chn_n) /* opt ed_n */
{
  #define H_redo "("  S_redo " &optional (count 1) snd chn) redoes count edits in snd's channel chn"
  chan_info *cp;
  ERRCP(S_redo,snd_n,chn_n,2);
  cp = get_cp(snd_n,chn_n);
  if (cp == NULL) return(NO_SUCH_CHANNEL);
  if (gh_number_p(ed_n))
    redo_EDIT(cp,g_scm2int(ed_n));
  else redo_EDIT(cp,1);
  update_graph(cp,NULL);
  return(SCM_BOOL_T);
}

static SCM g_insert_region(SCM samp_n, SCM reg_n, SCM snd_n, SCM chn_n) /* opt reg_n */
{
  #define H_insert_region "("  S_insert_region " &optional (start-samp 0) (region 0) snd chn) inserts region data\n\
   into snd's channel chn satrting at 'start-samp'"

  chan_info *cp;
  int rg,samp;
  ERRB1(samp_n,S_insert_region);
  ERRB2(reg_n,S_insert_region);
  ERRCP(S_insert_region,snd_n,chn_n,3);
  cp = get_cp(snd_n,chn_n);
  if (cp == NULL) return(NO_SUCH_CHANNEL);
  finish_keyboard_selection();
  rg = g_scm2intdef(reg_n,0);
  if (!(region_ok(rg))) return(NO_SUCH_REGION);
  samp = g_scm2intdef(samp_n,0);
  insert_region(rg,samp,cp,S_insert_region);
  update_graph(cp,NULL);
  return(SCM_BOOL_T);
}

static SCM g_insert_sound(SCM file, SCM file_chn, SCM snd_n, SCM chn_n)
{
  #define H_insert_sound "(" S_insert_sound " file &optional (file-chan 0) snd chn) inserts channel 'file-chan'\n\
   of 'file' into snd's channel chn at the cursor position"

  chan_info *cp;
  char *filename = NULL;
  int nc,len,fchn,beg=0;
  ERRS1(file,S_insert_sound);
  ERRB2(file_chn,S_insert_sound);
  ERRCP(S_insert_sound,snd_n,chn_n,3);
  cp = get_cp(snd_n,chn_n);
  if (cp == NULL)return(NO_SUCH_CHANNEL);
  filename = full_filename(file);
  nc = mus_sound_chans(filename);
  if (nc != -1)
    {
      len = mus_sound_samples(filename)/nc;
      fchn = g_scm2intdef(file_chn,0);
      if (fchn < mus_sound_chans(filename))
	{
	  if (cp->cursor >= 0) beg = cp->cursor;
	  file_insert_samples(beg,len,filename,cp,fchn,DONT_DELETE_ME,S_insert_sound);
	  update_graph(cp,NULL);
	  if (filename) FREE(filename);
	  RTNINT(len);
	}
    }
  else 
    {
      if (filename) FREE(filename);
      return(NO_SUCH_FILE);
    }
  if (filename) FREE(filename);
  return(SCM_BOOL_F);
}

static SCM g_cut(void)
{
  #define H_cut "(" S_cut ") cuts (deletes) the currently selected portion"
  finish_keyboard_selection();
  if (region_ok(0))
    {
      delete_selection(S_cut,UPDATE_DISPLAY);
      return(SCM_BOOL_T);
    }
  return(NO_ACTIVE_SELECTION);
}

static SCM g_selected_sound(void)
{
  #define H_selected_sound "(" S_selected_sound ") -> index of currently selected sound"
  if ((state->selected_sound != NO_SELECTION) && (snd_ok(state->sounds[state->selected_sound])))
    RTNINT(state->selected_sound);
  return(gh_int2scm(NO_SELECTION));
}

static SCM g_prompt_in_minibuffer(SCM msg, SCM callback, SCM snd_n)
{
  #define H_prompt_in_minibuffer "(" S_prompt_in_minibuffer " msg callback &optional snd) posts msg in snd's minibuffer\n\
   then when the user eventually responds, invokes the function callback with the response and snd (the index)"

  snd_info *sp;
  char *str;  
  ERRS1(msg,S_prompt_in_minibuffer);
  SCM_ASSERT((SCM_UNBNDP(callback)) || (gh_boolean_p(callback)) || gh_procedure_p(callback),callback,SCM_ARG2,S_prompt_in_minibuffer);
  ERRSP(S_prompt_in_minibuffer,snd_n,3);
  sp = get_sp(snd_n);
  if (sp == NULL) return(NO_SUCH_SOUND);
  if ((sp->prompt_callback) && (gh_procedure_p(sp->prompt_callback))) snd_unprotect(sp->prompt_callback);
  if (gh_procedure_p(callback)) 
    {
      sp->prompt_callback = callback;
      snd_protect(sp->prompt_callback);
    }
  else sp->prompt_callback = SCM_BOOL_F;
  str = gh_scm2newstr(msg,NULL);
  g_prompt(sp,str);
  free(str);
  return(SCM_BOOL_F);
}

static SCM g_report_in_minibuffer(SCM msg, SCM snd_n)
{
  #define H_report_in_minibuffer "(" S_report_in_minibuffer " msg &optional snd) displays msg in snd's minibuffer"
  snd_info *sp;
  char *str;  
  ERRS1(msg,S_report_in_minibuffer);
  ERRSP(S_report_in_minibuffer,snd_n,2);
  sp = get_sp(snd_n);
  if (sp == NULL) return(NO_SUCH_SOUND);
  str = gh_scm2newstr(msg,NULL);
  report_in_minibuffer(sp,str);
  free(str);
  return(msg);
}

static SCM g_append_to_minibuffer(SCM msg, SCM snd_n)
{
  #define H_append_to_minibuffer "(" S_append_to_minibuffer " msg &optional snd) appends msg to snd's minibuffer"
  snd_info *sp;
  char *str;
  ERRS1(msg,S_append_to_minibuffer);
  ERRSP(S_append_to_minibuffer,snd_n,2);
  sp = get_sp(snd_n);
  if (sp == NULL) return(NO_SUCH_SOUND);
  str = gh_scm2newstr(msg,NULL);
  append_to_minibuffer(sp,str);
  free(str);
  return(msg);
}

static SCM g_open_sound(SCM filename)
{ /* returns index of new sound if successful */
  #define H_open_sound "(" S_open_sound " filename) opens filename (as if opened from File:Open menu option)"
  char *fname = NULL;
  snd_info *sp;
  ERRS1(filename,S_open_sound);
  fname = full_filename(filename);
  sp = snd_open_file(fname,state);
  if (fname) FREE(fname);
  if (sp) RTNINT(sp->index);
  return(SCM_BOOL_F);
}

static SCM g_open_raw_sound(SCM filename, SCM chans, SCM srate, SCM format)
{
  #define H_open_raw_sound "(" S_open_raw_sound " filename chans srate format) opens filename assuming the data\n\
   matches the attributes indicated unless the file actually has a header"

  char *fname = NULL;
  snd_info *sp;
  int os,oc,ofr,ou,ofit;
  ERRS1(filename,S_open_raw_sound);
  ERRN2(srate,S_open_raw_sound);
  ERRN3(chans,S_open_raw_sound);
  ERRN4(format,S_open_raw_sound);
  ou=use_raw_defaults(state);
  os=raw_srate(state);
  oc=raw_chans(state);
  ofr=raw_format(state);
  ofit=fit_data_on_open(state);
  set_raw_srate(state,g_scm2int(srate));
  set_raw_chans(state,g_scm2int(chans));
  set_raw_format(state,g_scm2int(format));
  set_use_raw_defaults(state,1);
  set_fit_data_on_open(state,1);
  mus_header_set_raw_defaults(g_scm2int(srate),g_scm2int(chans),g_scm2int(format));
  fname = full_filename(filename);
  sp = snd_open_file(fname,state);
  if (fname) FREE(fname);
  set_raw_srate(state,os);
  set_raw_chans(state,oc);
  set_raw_format(state,ofr);
  set_use_raw_defaults(state,ou);
  set_fit_data_on_open(state,ofit);
  if (sp) RTNINT(sp->index);
  return(SCM_BOOL_F);
}

static SCM g_open_alternate_sound(SCM filename)
{
  #define H_open_alternate_sound "(" S_open_alternate_sound " filename) replace currently selected sound with filename"
  /* returns index of new sound if successful */
  char *fname = NULL;
  snd_info *sp;
  ERRS1(filename,S_open_alternate_sound);
  sp = any_selected_sound(state);
  if (sp) snd_close_file(sp,state); /* should we ask about saving edits here? */
  fname = full_filename(filename);
  sp = snd_open_file(fname,state);
  if (fname) FREE(fname);
  if (sp) RTNINT(sp->index);
  return(SCM_BOOL_F);
}

static SCM g_view_sound(SCM filename)
{
  #define H_view_sound "(" S_view_sound " filename) opens file name read-only"
  char *fname = NULL;
  snd_info *sp = NULL;
  ERRS1(filename,S_view_sound);
  fname = full_filename(filename);
  if (fname)
    {
      state->viewing = 1;
      sp = snd_open_file(fname,state);
      FREE(fname);
      state->viewing = 0;
      if (sp) RTNINT(sp->index);
    }
  return(SCM_BOOL_F);
}

static SCM g_save_sound_as(SCM newfile, SCM index, SCM type, SCM format, SCM srate)
{
  #define H_save_sound_as "("  S_save_sound_as " filename &optional snd header-type data-format srate)\n\
   saves snd in filename using the indicated attributes."

  snd_info *sp;
  file_info *hdr;
  int ht,df,sr;
  char *fname = NULL;
  ERRS1(newfile,S_save_sound_as);
  sp = get_sp(index);
  if (sp == NULL) return(NO_SUCH_SOUND);
  fname = full_filename(newfile);
  hdr = sp->hdr;
  ht = g_scm2intdef(type,hdr->type);
  sr = g_scm2intdef(srate,hdr->srate);
  if (gh_number_p(format)) 
    df = g_scm2int(format);
  else
    {
      if (mus_header_writable(ht,hdr->format))
	df = hdr->format;
      else df = MUS_OUT_FORMAT;
    }
  save_edits_2(sp,fname,ht,df,sr,NULL); /* last arg is comment */
  if (fname) FREE(fname);
  return(newfile);
}

static SCM g_revert_sound(SCM index)
{
  #define H_revert_sound "("  S_revert_sound " &optional snd) reverts snd to its unedited state (undo all)"
  snd_info *sp;
  int i;
  sp = get_sp(index);
  if (sp == NULL) return(NO_SUCH_SOUND);
  for (i=0;i<sp->nchans;i++) 
    {
      revert_edits(sp->chans[i],NULL); 
      update_graph(sp->chans[i],NULL);
    }
  reflect_file_revert_in_label(sp);
  reflect_file_revert_in_menu(state);
  return(SCM_BOOL_T);
}

static SCM g_new_sound(SCM name, SCM type, SCM format, SCM srate, SCM chans, SCM comment) 
{
  #define H_new_sound "(" S_new_sound " name &optional type format srate chans comment) creates a new sound file\n\
   with the indicated attributes; if any are omitted, the corresponding default-output variable is used"

  snd_info *sp; 
  int ht,df,sr,ch;
  char *str = NULL,*com = NULL;
  ERRS1(name,S_new_sound);
  str = full_filename(name);
  if ((!(gh_number_p(type))) || (g_scm2int(type) == MUS_UNSUPPORTED))
    sp = snd_new_file(state,str,MUS_UNSUPPORTED,MUS_UNSUPPORTED,0,0,NULL);
  else 
    {
      ht = g_scm2int(type);
      if (MUS_HEADER_TYPE_OK(ht))
	{
	  df = g_scm2intdef(format,MUS_OUT_FORMAT);
	  if (MUS_DATA_FORMAT_OK(df))
	    {
	      if (mus_header_writable(ht,df))
		{
		  sr = g_scm2intdef(srate,22050);
		  ch = g_scm2intdef(chans,1);
		  if (gh_string_p(comment))
		    com = gh_scm2newstr(comment,NULL);
		  sp = snd_new_file(state,str,ht,df,sr,ch,com);
		  if (com) free(com);
		}
#if HAVE_GUILE_1_3_0
	      else scm_misc_error(S_new_sound,"can't write this combination of data format (%S) and header type (%S)",SCM_LIST2(type,format));
	    }
	  else scm_misc_error(S_new_sound,"invalid data format: %S",SCM_LIST1(format));
	}
      else scm_misc_error(S_new_sound,"invalid header type: %S",SCM_LIST1(type));
#else
	      else scm_misc_error(S_new_sound,"can't write this combination of data format (~S) and header type (~S)",SCM_LIST2(type,format));
	    }
	  else scm_misc_error(S_new_sound,"invalid data format: ~S",SCM_LIST1(format));
	}
      else scm_misc_error(S_new_sound,"invalid header type: ~S",SCM_LIST1(type));
#endif
    }
  if (str) FREE(str);
  if (sp) RTNINT(sp->index);
  return(SCM_BOOL_F);
}

static SCM g_play_1(SCM samp_n, SCM snd_n, SCM chn_n, int background, int syncd) /* all chans if chn_n omitted, arbitrary file if snd_n is name */
{
  snd_info *sp;
  chan_info *cp;
  char *name = NULL;
  int samp = 0;
  if (gh_string_p(samp_n))
    {
      name = full_filename(samp_n);
      sp = make_sound_readable(state,name,FALSE);
      if (sp == NULL) {if (name) FREE(name); return(NO_SUCH_SOUND);}
      sp->shortname = filename_without_home_directory(name);
      sp->fullname = NULL;
      sp->delete_me = 1;
      if (background)
	start_playing(sp,0);
      else play_to_end(sp,0);
      if (name) FREE(name);
    }
  else
    {
      ERRB1(samp_n,S_play);
      ERRCP(S_play,snd_n,chn_n,2);
      sp = get_sp(snd_n);
      if (sp == NULL) return(NO_SUCH_SOUND);
      samp = g_scm2intdef(samp_n,0);
      if (!(gh_number_p(chn_n)))
	{
	  if (syncd)
	    start_playing_syncd(sp,samp,background);
	  else
	    {
	      if (background)
		start_playing(sp,samp);
	      else play_to_end(sp,samp);
	    }
	}
      else 
	{
	  cp = get_cp(snd_n,chn_n);
	  if (cp == NULL) return(NO_SUCH_CHANNEL);
	  if (syncd)
	    start_playing_syncd(cp->sound,samp,background);
	  else
	    {
	      if (background)
		start_playing(cp,samp);
	      play_to_end(cp,samp);
	    }
	}
    }
  return(SCM_BOOL_T);
}

static SCM g_play(SCM samp_n, SCM snd_n, SCM chn_n, SCM syncd) 
{
  #define H_play "(" S_play " &optional (start 0) snd chn sync) plays snd or snd's channel chn starting at start.\n\
   'start' can also be a filename: (" S_play " \"oboe.snd\").  If 'sync' is true, all sounds syncd to snd are played."

  return(g_play_1(samp_n,snd_n,chn_n,TRUE,bool_int_or_zero(syncd)));
}

static SCM g_play_and_wait(SCM samp_n, SCM snd_n, SCM chn_n, SCM syncd) 
{
  #define H_play_and_wait "(" S_play_and_wait " &optional (start 0) snd chn) plays snd or snd's channel chn starting at start\n\
   and waiting for the play to complete before returning.  'start' can also be a filename: (" S_play_and_wait " \"oboe.snd\")"

  return(g_play_1(samp_n,snd_n,chn_n,FALSE,bool_int_or_zero(syncd)));
}

static SCM g_stop_playing(SCM snd_n)
{
  #define H_stop_playing "(" S_stop_playing " &optional snd) stops any snd play in progress"
  snd_info *sp;
  ERRSP(S_stop_playing,snd_n,1);
  sp = get_sp(snd_n);
  if (sp) stop_playing_sound(sp); else return(NO_SUCH_SOUND);
  return(SCM_BOOL_F);
}

void add_or_edit_symbol(char *name, env *val)
{
  /* called from envelope editor -- pass new definition into scheme */
  SCM e;
  char *buf,*tmpstr=NULL;
  buf = (char *)CALLOC(256,sizeof(char));
  e = GH_LOOKUP(name);
  if ((e) && (SCM_NFALSEP(e)) && (!(SCM_UNBNDP(e))) && (gh_list_p(e)))
    sprintf(buf,"(set! %s %s)",name,tmpstr=env_to_string(val));
  else sprintf(buf,"(define %s %s)",name,tmpstr=env_to_string(val));
  scm_internal_stack_catch(SCM_BOOL_T,eval_str_wrapper,buf,snd_catch_scm_error,buf);
  FREE(buf);
  if (tmpstr) FREE(tmpstr);
}

static env *scm2env(SCM res)
{
  SCM el;
  int i,len;
  Float *data;
  env *rtn = NULL;
  if (gh_list_p(res))
    {
      len = gh_length(res);
      if (len > 0)
	{
	  data = (Float *)CALLOC(len,sizeof(Float));
	  for (i=0;i<len;i++)
	    {
	      el = scm_list_ref(res,gh_int2scm(i));
	      if (gh_number_p(el))
		data[i] = gh_scm2double(el);
	      else data[i] = 0.0;
	    }
	  rtn = make_envelope(data,len);
	  FREE(data);
	}
      return(rtn);
    }
  return(NULL);
}

static int x_increases(SCM res)
{
  int i,len;
  Float x;
  len = gh_length(res);
  x = gh_list_ref(res,gh_int2scm(0));
  for (i=2;i<len;i+=2)
    {
      if (x >= (gh_list_ref(res,gh_int2scm(i))))
	return(0);
      x = gh_list_ref(res,gh_int2scm(i));
    }
  return(1);
}

/* these make it possible for the user to type names or expressions wherever a value is possible */
env *string2env(char *str) 
{
  SCM res;
  res = scm_internal_stack_catch(SCM_BOOL_T,eval_str_wrapper,str,snd_catch_scm_error,str);
  if (gh_list_p(res))
    if ((gh_length(res)%2) == 0)
      if (x_increases(res))
	return(scm2env(res));
      else snd_error("x axis points not increasing: %s",str);
    else snd_error("odd length envelope? %s",str);
  else snd_error("%s is not a list",str);
  return(NULL);
}

#if 0
Float string2Float(char *str) 
{
  SCM res;
  res = scm_internal_stack_catch(SCM_BOOL_T,eval_str_wrapper,str,snd_catch_scm_error,str);
  if (gh_number_p(res))
    return(gh_scm2double(res));
  else snd_error("%s is not a number",str);
  return(0.0);
}
#endif

int string2int(char *str) 
{
  SCM res;
  res = scm_internal_stack_catch(SCM_BOOL_T,eval_str_wrapper,str,snd_catch_scm_error,str);
  if (gh_number_p(res))
    return(g_scm2int(res));
  else snd_error("%s is not a number",str);
  return(0);
}

#if 0
char *string2string(char *str) 
{
  SCM res;
  res = scm_internal_stack_catch(SCM_BOOL_T,eval_str_wrapper,str,snd_catch_scm_error,str);
  if (gh_string_p(res))
    return(gh_scm2newstr(res,NULL));
  else snd_error("%s is not a string",str);
  return(str);
}
#endif

env *name_to_env(char *str)
{
  /* called to see if str is a known envelope -- return its current value or nil if unknown */
  /* get str as list var and turn into env */
  return(scm2env(GH_LOOKUP(str)));
}

static SCM g_define_envelope(SCM a, SCM b)
{
  #define H_define_envelope "(" S_define_envelope " name data) defines 'name' to be the envelope 'data', a list of breakpoints"
  char *name;
  name = gh_scm2newstr(a,NULL);
  if (gh_list_p(b)) alert_envelope_editor(state,name,scm2env(b));
  return(SCM_BOOL_F);
}

static SCM g_set_env_base(SCM name, SCM val) 
{
  #define H_set_env_base "(" S_set_env_base " name base) sets the base of the envelope 'name'"
  char *urn = NULL;
  int rtnval;
  ERRS1(name,S_set_env_base);
  ERRN2(val,S_set_env_base);
  urn = gh_scm2newstr(name,NULL);
  rtnval = set_env_base(urn,gh_scm2double(val));
  free(urn);
  RTNINT(rtnval);
}

static SCM g_update_graph(SCM snd, SCM chn) 
{
  #define H_update_graph "(" S_update_graph " &optional snd chn) redraws snd channel chn's graphs"
  chan_info *cp;
  ERRCP(S_update_graph,snd,chn,1); 
  cp = get_cp(snd,chn);
  if (cp) update_graph(cp,NULL); else return(NO_SUCH_CHANNEL);
  return(SCM_BOOL_F);
}

static SCM g_update_fft(SCM snd, SCM chn) 
{
  #define H_update_fft "(" S_update_fft " &optional snd chn) recalculates snd channel chn's fft"
  chan_info *cp;
  ERRCP(S_update_fft,snd,chn,1); 
  cp = get_cp(snd,chn);
  if (cp) calculate_fft(cp,NULL); else return(NO_SUCH_CHANNEL);
  return(SCM_BOOL_F);
}

enum {REGION_LENGTH,REGION_SRATE,REGION_CHANS,REGION_MAXAMP,REGION_SELECT,REGION_DELETE,REGION_PLAY};

static SCM region_read(int field, SCM n)
{
  int rg;
  rg = g_scm2intdef(n,0);
  if (region_ok(rg))
    {
      switch (field)
	{
	case REGION_LENGTH: RTNINT(region_len(rg)); break;
	case REGION_SRATE:  RTNINT(region_srate(rg)); break;
	case REGION_CHANS:  RTNINT(region_chans(rg)); break;
	case REGION_MAXAMP: RTNFLT(region_maxamp(rg)); break;
	case REGION_SELECT: select_region_and_update_browser(state,rg); return(n); break;
	case REGION_DELETE: delete_region_and_update_browser(state,rg); return(n); break;
	}
    }
  else return(NO_SUCH_REGION);
  RTNINT(0);
}

static SCM g_region_length (SCM n) 
{
  #define H_region_length "(" S_region_length " &optional (n 0)) -> length in frames of region"
  ERRB1(n,S_region_length); 
  return(region_read(REGION_LENGTH,n));
}

static SCM g_region_srate (SCM n) 
{
  #define H_region_srate "(" S_region_srate " &optional (n 0)) -> srate of region n"
  ERRB1(n,S_region_srate); 
  return(region_read(REGION_SRATE,n));
}

static SCM g_region_chans (SCM n) 
{
  #define H_region_chans "(" S_region_chans " &optional (n 0) -> channels of data in region n"
  ERRB1(n,S_region_chans); 
  return(region_read(REGION_CHANS,n));
}

static SCM g_region_maxamp (SCM n) 
{
  #define H_region_maxamp "(" S_region_maxamp " &optional (n 0)) -> max amp of region n"
  ERRB1(n,S_region_maxamp); 
  return(region_read(REGION_MAXAMP,n));
}

static SCM g_select_region (SCM n) 
{
  #define H_select_region "(" S_select_region " &optional (n 0)) selects region n (moves it to the top of the region list)"
  ERRB1(n,S_select_region); 
  return(region_read(REGION_SELECT,n));
}

static SCM g_delete_region (SCM n) 
{
  #define H_delete_region "(" S_delete_region " &optional (n 0)) remove region n from the region list"
  ERRB1(n,S_delete_region); 
  return(region_read(REGION_DELETE,n));
}

static SCM g_play_region (SCM n, SCM wait) 
{
  #define H_play_region "(" S_play_region " &optional (n 0) (wait #f)) play region n, if wait is #t, play to end before returning"
  int rg;
  ERRB1(n,S_play_region); 
  ERRB2(wait,S_play_region);
  rg = g_scm2intdef(n,0);
  if (region_ok(rg))
    play_region(state,rg,NULL,bool_int_or_zero(wait));
  else return(NO_SUCH_REGION);
  return(n);
}

static SCM g_protect_region (SCM n, SCM protect) 
{
  #define H_protect_region "(" S_protect_region " &optional (n 0) (val #t)) if val is #t protects region n from being\n\
   pushed off the end of the region list"

  ERRN1(n,S_protect_region);
  ERRB2(protect,S_protect_region);
  set_region_protect(g_scm2int(n),bool_int_or_one(protect)); 
  return(protect);
}

static SCM g_regions(void) 
{
  #define H_regions "(" S_regions ") -> how many regions are currently in the region list"
  RTNINT(snd_regions());
}

static SCM g_make_region (SCM beg, SCM end, SCM snd_n, SCM chn_n)
{
  #define H_make_region "(" S_make_region " beg end &optional snd chn) makes a new region between beg and end in snd"
  chan_info *cp;
  ERRN1(beg,S_make_region);
  ERRN2(end,S_make_region);
  ERRCP(S_make_region,snd_n,chn_n,3);
  cp = get_cp(snd_n,chn_n);
  if (cp == NULL) return(NO_SUCH_CHANNEL);
  define_region(cp,g_scm2int(beg),g_scm2int(end),FALSE);
  return(SCM_BOOL_T);
}

static SCM g_selection_beg(void)
{
  #define H_selection_beg "(" S_selection_beg ") -> selection start samp in selected sound"
  if (selection_is_current())
    return(gh_int2scm(selection_beg(NULL)));
  return(NO_ACTIVE_SELECTION);
}

static SCM g_selection_length(void)
{
  #define H_selection_length "(" S_selection_length ") -> length (frames) of selected portion"
  if (selection_is_current())
    return(gh_int2scm(region_len(0)));
  return(NO_ACTIVE_SELECTION);
}

static SCM g_selection_member(SCM snd, SCM chn)
{
  #define H_selection_member "(" S_selection_member " &optional snd chn) -> #t if snd's channel chn is a member of the current selection"
  chan_info *cp;
  ERRCP(S_selection_member,snd,chn,1);
  cp = get_cp(snd,chn);
  if ((cp) && (selection_is_current_in_channel(cp)))
    return(SCM_BOOL_T);
  return(SCM_BOOL_F);
}

static SCM g_select_all (SCM snd_n, SCM chn_n)
{
  #define H_select_all "(" S_select_all " &optional snd chn) makes a new selection containing all of snd's channel chn"
  chan_info *cp;
  ERRCP(S_select_all,snd_n,chn_n,1);
  cp = get_cp(snd_n,chn_n);
  if (cp == NULL) return(NO_SUCH_CHANNEL);
  define_region(cp,0,current_ed_samples(cp),FALSE);
  update_graph(cp,NULL);
  return(SCM_BOOL_T);
}

static SCM g_save_region (SCM n, SCM filename, SCM format) 
{
  #define H_save_region "(" S_save_region " region filename &optional format) saves region in filename using data format (mus-bshort)"
  char *name = NULL;
  int res=SND_NO_ERROR,rg;
  ERRN1(n,S_save_region);
  ERRS2(filename,S_save_region);
  ERRB3(format,S_save_region);
  rg = g_scm2int(n);
  if (region_ok(rg))
    {
      name = full_filename(filename);
      res = save_region(state,rg,name,g_scm2intdef(format,0));
      if (name) FREE(name);
    }
  else return(NO_SUCH_REGION);
  if (res != SND_NO_ERROR)
    return(SCM_BOOL_F);
  return(CANNOT_SAVE);
}

static SCM g_mix_region(SCM chn_samp_n, SCM scaler, SCM reg_n, SCM snd_n, SCM chn_n)
{
  #define H_mix_region "(" S_mix_region " &optional (chn-samp 0) (scaler 1.0) (region 0) snd chn) mixer region\n\
   into snd's channel chn starting at chn-samp scaled by scaler; returns new mix id."

  chan_info *cp;
  int rg,id=-1;
  ERRB1(chn_samp_n,S_mix_region);
  ERRB2(scaler,S_mix_region);
  ERRB3(reg_n,S_mix_region);
  ERRCP(S_mix_region,snd_n,chn_n,4);
  rg = g_scm2intdef(reg_n,0);
  if (region_ok(rg))
    {
      cp = get_cp(snd_n,chn_n);
      if (cp)
	id = mix_region(rg,cp,
			g_scm2intdef(chn_samp_n,cp->cursor),
			(gh_number_p(scaler) ? (gh_scm2double(scaler)) : 1.0));
      else return(NO_SUCH_CHANNEL);
    }
  else return(NO_SUCH_REGION);
  return(gh_int2scm(id));
}

static SCM g_region_sample(SCM samp_n, SCM reg_n, SCM chn_n)
{
  #define H_region_sample "(" S_region_sample " &optional (samp 0) (region 0) (chan 0)) -> region's sample at samp in chan"
  ERRB1(samp_n,S_region_sample);
  ERRB2(reg_n,S_region_sample);
  ERRB3(chn_n,S_region_sample);
  finish_keyboard_selection();
  RTNFLT(region_sample(g_scm2intdef(reg_n,0),g_scm2intdef(chn_n,0),g_scm2intdef(samp_n,0)));
}

static SCM g_region_samples(SCM beg_n, SCM num, SCM reg_n, SCM chn_n)
{
  #define H_region_samples "(" S_region_samples " &optional (beg 0) samps (region 0) (chan 0)) returns a vector with\n\
   region's samples starting at samp for samps from channel chan"

  SCM new_vect;
  Float *data;
  int len,reg,i,chn;
  ERRB1(beg_n,S_region_samples);
  ERRB2(num,S_region_samples);
  ERRB3(reg_n,S_region_samples);
  ERRB4(chn_n,S_region_samples);
  finish_keyboard_selection();
  reg = g_scm2intdef(reg_n,0);
  if (!(region_ok(reg))) return(NO_SUCH_REGION);
  chn = g_scm2intdef(chn_n,0);
  if (chn < region_chans(reg))
    {
      len = g_scm2intdef(num,0);
      if (len == 0) len = region_len(reg);
      if (len > 0)
	{
	  new_vect = gh_make_vector(gh_int2scm(len),gh_double2scm(0.0));
	  data = (Float *)CALLOC(len,sizeof(Float));
	  region_samples(reg,chn,g_scm2intdef(beg_n,0),len,data);
	  for (i=0;i<len;i++) gh_vector_set_x(new_vect,gh_int2scm(i),gh_double2scm(data[i]));
	  FREE(data);
	  return(new_vect);
	}
    }
  else return(NO_SUCH_CHANNEL);
  return(SCM_BOOL_F);
}

static SCM g_transform_size(SCM snd, SCM chn)
{
  #define H_transform_size "(" S_transform_size " &optional snd chn) -> description of transform data in snd's channel chn.\n\
   If no fft, returns 0; if normal-fft, returns fft-size, else returns a list (full-size active-bins active-slices)"

  chan_info *cp;
  sono_info *si;
  cp = get_cp(snd,chn);
  if (cp == NULL) return(NO_SUCH_CHANNEL);
  if (!(cp->ffting)) return(SCM_INUM0);
  if (fft_style(state) == NORMAL_FFT) return(gh_int2scm(fft_size(state)));
  si = (sono_info *)(cp->sonogram_data);
  if (si) return(SCM_LIST3(gh_double2scm(spectro_cutoff(state)),gh_int2scm(si->active_slices),gh_int2scm(si->target_bins)));
  return(SCM_INUM0);
}

static SCM g_transform_sample(SCM bin, SCM slice, SCM snd_n, SCM chn_n)
{
  #define H_transform_sample "(" S_transform_sample " &optional (bin 0) (slice 0) snd chn) -> the current transform\n\
   sample at bin and slice in snd channel chn (assuming sonogram or spectrogram)"

  chan_info *cp;
  fft_info *fp;
  sono_info *si;
  int fbin,fslice;
  ERRB1(bin,S_transform_sample);
  ERRB2(slice,S_transform_sample);
  ERRCP(S_transform_sample,snd_n,chn_n,3);
  cp = get_cp(snd_n,chn_n);
  if (cp == NULL) return(NO_SUCH_CHANNEL);
  if (cp->ffting)
    {
      while (chan_fft_in_progress(cp)) {work_wait(cp->state);}
      fbin = g_scm2intdef(bin,0);
      fp = cp->fft;
      if ((fp) && (fbin < fp->current_size))
	{
	  if (fft_style(state) == NORMAL_FFT)
	    RTNFLT(fp->data[fbin]);
	  else 
	    {
	      fslice = g_scm2intdef(slice,0);
	      si = (sono_info *)(cp->sonogram_data);
	      if ((si) && (fbin < si->target_bins) && (fslice < si->active_slices))
		RTNFLT(si->data[fslice][fbin]);
	      else return(NO_SUCH_SAMPLE);
	    }
	}
    }
  return(SCM_BOOL_F);
}  

static SCM g_transform_samples(SCM snd_n, SCM chn_n)
{
  #define H_transform_samples "(" S_transform_samples " &optional snd chn) -> current transform data for snd channel chn"
  chan_info *cp;
  fft_info *fp;
  sono_info *si;
  int bins,slices,i,j,len;
  SCM new_vect,tmp_vect;
  ERRCP(S_transform_samples,snd_n,chn_n,1);
  cp = get_cp(snd_n,chn_n);
  if (cp == NULL)return(NO_SUCH_CHANNEL);
  if (cp->ffting)
    {
      while (chan_fft_in_progress(cp)) {work_wait(cp->state);}
      fp = cp->fft;
      if (fp)
	{
	  bins = fp->current_size;
	  if (fft_style(state) == NORMAL_FFT)
	    {
	      len = fp->current_size;
	      new_vect = gh_make_vector(gh_int2scm(len),gh_double2scm(0.0));
	      for (i=0;i<len;i++) gh_vector_set_x(new_vect,gh_int2scm(i),gh_double2scm(fp->data[i]));
	      return(new_vect);
	    }
	  else 
	    {
	      si = (sono_info *)(cp->sonogram_data);
	      if (si)
		{
		  slices = si->active_slices;
		  bins = si->target_bins;
		  new_vect = gh_make_vector(gh_int2scm(slices),gh_double2scm(0.0));
		  for (i=0;i<slices;i++)
		    {
		      tmp_vect = gh_make_vector(gh_int2scm(bins),gh_double2scm(0.0));
		      gh_vector_set_x(new_vect,gh_int2scm(i),tmp_vect);
		      for (j=0;j<bins;j++)
			gh_vector_set_x(tmp_vect,gh_int2scm(j),gh_double2scm(si->data[i][j]));
		    }
		  return(new_vect);
		}
	    }
	}
    }
  return(SCM_BOOL_F);
}  

static SCM g_add_transform(SCM name, SCM xlabel, SCM lo, SCM hi, SCM proc)
{
  #define H_add_transform "(" S_add_transform " name x-label low high func) adds the transform func\n\
   to the transform lists; func should be a function of two arguments, the length of the transform\n\
   and a sample-reader to get the data, and should return a vct object containing the transform results.\n\
   'name' is the transform's name, x-label is its x-axis label, and the relevant returned data\n\
   to be displayed goes from low to high (normally 0.0 to 1.0)"

  char *str1=NULL,*str2=NULL;
  SCM res=SCM_BOOL_F;
  ERRS1(name,S_add_transform);
  ERRS2(xlabel,S_add_transform);
  ERRN3(lo,S_add_transform);
  ERRN4(hi,S_add_transform);
  str1 = gh_scm2newstr(name,NULL);
  str2 = gh_scm2newstr(xlabel,NULL);
  SCM_ASSERT(gh_procedure_p(proc),proc,SCM_ARG5,S_add_transform);
  if (procedure_ok(proc,2,0,S_add_transform,"func",5))
    res = gh_int2scm(add_transform(str1,str2,gh_scm2double(lo),gh_scm2double(hi),proc));
  if (str1) free(str1);
  if (str2) free(str2);
  return(res);
}

static SCM g_forward_graph(SCM count, SCM snd, SCM chn) 
{
  #define H_forward_graph "(" S_forward_graph " &optional (count 1) snd chn) moves the 'selected' graph forward by count"
  int val;
  chan_info *cp;
  ERRB1(count,S_forward_graph);
  ERRCP(S_forward_graph,snd,chn,2);
  cp = get_cp(snd,chn);
  val = g_scm2intdef(count,1);
  if (cp) goto_next_graph(cp,val); else return(NO_SUCH_CHANNEL);
  RTNINT(val);
}

static SCM g_backward_graph(SCM count, SCM snd, SCM chn) 
{
  #define H_backward_graph "(" S_backward_graph " &optional (count 1) snd chn) moves the 'selected' graph back by count"
  int val;
  chan_info *cp;
  ERRB1(count,S_backward_graph);
  ERRCP(S_backward_graph,snd,chn,2);
  cp = get_cp(snd,chn);
  val = -(g_scm2intdef(count,1));
  if (cp) goto_previous_graph(cp,val); else return(NO_SUCH_CHANNEL);
  RTNINT(val);
}

static SCM g_forward_mark(SCM count, SCM snd, SCM chn) 
{
  #define H_forward_mark "(" S_forward_mark " &optional (count 1) snd chn) moves the cursor forward by count marks"
  int val; 
  chan_info *cp;
  ERRB1(count,S_forward_mark);
  ERRCP(S_forward_mark,snd,chn,2);
  cp = get_cp(snd,chn);
  val = g_scm2intdef(count,1); 
  if (cp) handle_cursor(cp,goto_mark(cp,val)); else return(NO_SUCH_CHANNEL);
  RTNINT(val);
}

static SCM g_backward_mark(SCM count, SCM snd, SCM chn) 
{
  #define H_backward_mark "(" S_backward_mark " &optional (count 1) snd chn) moves the cursor back by count marks"
  int val; 
  chan_info *cp;
  ERRB1(count,S_backward_mark);
  ERRCP(S_backward_mark,snd,chn,2);
  cp = get_cp(snd,chn);
  val = -(g_scm2intdef(count,1)); 
  if (cp) handle_cursor(cp,goto_mark(cp,val)); else return(NO_SUCH_CHANNEL);
  RTNINT(val);
}

static SCM g_forward_mix(SCM count, SCM snd, SCM chn) 
{
  #define H_forward_mix "(" S_forward_mix " &optional (count 1) snd chn) moves the cursor forward count mix consoles"
  int val;
  chan_info *cp;
  ERRB1(count,S_forward_mix); 
  ERRCP(S_forward_mix,snd,chn,2);
  cp = get_cp(snd,chn);
  val = g_scm2intdef(count,1); 
  if (cp) handle_cursor(cp,goto_mix(cp,val)); else return(NO_SUCH_CHANNEL);
  RTNINT(val);
}

static SCM g_backward_mix(SCM count, SCM snd, SCM chn) 
{
  #define H_backward_mix "(" S_backward_mix " &optional (count 1) snd chn) moves the cursor back count mix consoles"
  int val; 
  chan_info *cp;
  ERRB1(count,S_backward_mix); 
  ERRCP(S_backward_mix,snd,chn,2);
  cp = get_cp(snd,chn);
  val = -(g_scm2intdef(count,1)); 
  if (cp) handle_cursor(cp,goto_mix(cp,val)); else return(NO_SUCH_CHANNEL);
  RTNINT(val);
}

static SCM g_forward_sample(SCM count, SCM snd, SCM chn) 
{
  #define H_forward_sample "(" S_forward_sample " &optional (count 1) snd chn) moves the cursor forward count samples"
  chan_info *cp;
  ERRB1(count,S_forward_sample); 
  ERRCP(S_forward_sample,snd,chn,2);
  cp = get_cp(snd,chn);
  if (cp == NULL) return(NO_SUCH_CHANNEL);
  handle_cursor(cp,cursor_move(cp,g_scm2intdef(count,1))); 
  RTNINT(cp->cursor);
}

static SCM g_backward_sample(SCM count, SCM snd, SCM chn) 
{
  #define H_backward_sample "(" S_backward_sample " &optional (count 1) snd chn) moves the cursor back count samples"
  chan_info *cp;
  ERRB1(count,S_backward_sample); 
  ERRCP(S_backward_sample,snd,chn,2);
  cp = get_cp(snd,chn);
  if (cp == NULL) return(NO_SUCH_CHANNEL);
  handle_cursor(cp,cursor_move(cp,-(g_scm2intdef(count,1)))); 
  RTNINT(cp->cursor);
}

static Float *load_Floats(SCM scalers, int *result_len)
{
  int len,i;
  Float *scls;
  if (gh_vector_p(scalers))
    len = gh_vector_length(scalers);
  else
    if (gh_list_p(scalers))
      len = gh_length(scalers);
    else len = 1;
  if (len<=0) len=1;
  scls = (Float *)CALLOC(len,sizeof(Float));
  if (gh_vector_p(scalers))
    {
      for (i=0;i<len;i++) scls[i] = (Float)gh_scm2double(gh_vector_ref(scalers,gh_int2scm(i)));
    }
  else
    if (gh_list_p(scalers))
      {
	for (i=0;i<len;i++) scls[i] = (Float)gh_scm2double(scm_list_ref(scalers,gh_int2scm(i)));
      }
    else
      if (gh_number_p(scalers))
	scls[0] = (Float)gh_scm2double(scalers);
      else scls[0] = 1.0;
  result_len[0] = len;
  return(scls);
}

static SCM g_scale_to(SCM scalers, SCM snd_n, SCM chn_n)
{
  #define H_scale_to "(" S_scale_to " norms &optional snd chn) normalizes snd to norms (following sync)\n\
   norms can be a float or a vector of floats"

  /* chn_n irrelevant if syncing */
  snd_info *sp;
  chan_info *cp;
  int len[1];
  Float *scls;
  ERRCP(S_scale_to,snd_n,chn_n,2);
  sp = get_sp(snd_n);
  if (sp == NULL) return(NO_SUCH_SOUND);
  cp = get_cp(snd_n,chn_n);
  if (cp == NULL)return(NO_SUCH_CHANNEL);
  scls = load_Floats(scalers,len);
  scale_to(state,sp,cp,scls,len[0],FALSE); /* last arg for selection */
  FREE(scls);
  return(scalers);
}

static SCM g_scale_by(SCM scalers, SCM snd_n, SCM chn_n)
{
  #define H_scale_by "(" S_scale_by " scalers &optional snd chn) scales snd by scalers (following sync)\n\
   scalers can be a float or a vector of floats"

  /* chn_n irrelevant if syncing */
  snd_info *sp;
  chan_info *cp;
  int len[1];
  Float *scls;
  ERRCP(S_scale_by,snd_n,chn_n,2);
  sp = get_sp(snd_n);
  if (sp == NULL) return(NO_SUCH_SOUND);
  cp = get_cp(snd_n,chn_n);
  if (cp == NULL)return(NO_SUCH_CHANNEL);
  scls = load_Floats(scalers,len);
  scale_by(state,sp,cp,scls,len[0],FALSE);
  FREE(scls);
  return(scalers);
}

static SCM g_scale_selection_to(SCM scalers)
{
  #define H_scale_selection_to "(" S_scale_selection_to " norms &optional chn) normalizes current selected portion to norms"
  int len[1];
  Float *scls;
  if (region_ok(0))
    {
      scls = load_Floats(scalers,len);
      scale_to(state,NULL,NULL,scls,len[0],TRUE);
      FREE(scls);
      return(scalers);
    }
  return(NO_ACTIVE_SELECTION);
}

static SCM g_scale_selection_by(SCM scalers)
{
  #define H_scale_selection_by "(" S_scale_selection_by " scalers &optional chn) scales current selected portion by scalers"
  int len[1];
  Float *scls;
  if (region_ok(0))
    {
      scls = load_Floats(scalers,len);
      scale_by(state,NULL,NULL,scls,len[0],TRUE);
      FREE(scls);
      return(scalers);
    }
  return(NO_ACTIVE_SELECTION);
}

static SCM g_preload_directory(SCM directory) 
{
  #define H_preload_directory "(" S_preload_directory " dir) preloads (into the View:Files dialog) any sounds in dir"
  char *str;
  ERRS1(directory,S_preload_directory);
  str = gh_scm2newstr(directory,NULL);
  if (str) add_directory_to_prevlist(state,str);
  free(str);
  return(directory);
}

static SCM g_preload_file(SCM file) 
{
  #define H_preload_file "(" S_preload_file " file) preloads file (into the View:Files dialog)"
  char *name = NULL;
  ERRS1(file,S_preload_file);
  name = full_filename(file);
  remember_me(state,filename_without_home_directory(name),name);
  if (name) FREE(name);
  return(file);
}

static SCM g_sound_files_in_directory(SCM dirname)
{
  #define H_sound_files_in_directory "(" S_sound_files_in_directory " directory) returns a vector of sound files in directory"
  dir *dp = NULL;
  char *name = NULL;
  int i,numfiles;
  SCM vect = SCM_BOOL_F;
  ERRS1(dirname,S_sound_files_in_directory);
  name = gh_scm2newstr(dirname,NULL);
  if (name)
    {
      dp = find_sound_files_in_dir(name);
      free(name);
      if (dp)
	{
	  numfiles = dp->len;
	  vect = gh_make_vector(gh_int2scm(numfiles),SCM_BOOL_F);
	  for (i=0;i<numfiles;i++)
	    gh_vector_set_x(vect,gh_int2scm(i),gh_str02scm(dp->files[i]));
	  free_dir(dp);
	}
    }
  return(vect);
}




static SCM g_help_dialog(SCM subject, SCM msg)
{
  #define H_help_dialog "(" S_help_dialog " subject message) fires up the Help window with subject and message"
  char *nmsg,*nsubj;
  ERRS1(subject,S_help_dialog);
  ERRS2(msg,S_help_dialog);
  nsubj = gh_scm2newstr(subject,NULL);
  nmsg = gh_scm2newstr(msg,NULL);
  snd_help(state,nsubj,nmsg);
  free(nsubj);
  free(nmsg);
  return(SCM_BOOL_F);
}

static SCM g_enved_dialog(void) 
{
  #define H_enved_dialog "(" S_enved_dialog ") fires up the Envelope Editor"
  create_envelope_editor(state); 
  return(SCM_BOOL_F);
}

static SCM g_color_dialog(void) 
{
  #define H_color_dialog "(" S_color_dialog ") fires up the Color dialog"
  start_color_dialog(state,0,0); 
  return(SCM_BOOL_F);
}

static SCM g_orientation_dialog(void) 
{
  #define H_orientation_dialog "(" S_orientation_dialog ") fires up the Orientation dialog"
  start_orientation_dialog(state,0,0); 
  return(SCM_BOOL_F);
}

static SCM g_transform_dialog(void) 
{
  #define H_transform_dialog "(" S_transform_dialog ") fires up the Transforms dialog"
  fire_up_transform_dialog(state); 
  return(SCM_BOOL_F);
}

static SCM g_file_dialog(void) 
{
  #define H_file_dialog "(" S_file_dialog ") fires up the File dialog"
  start_file_dialog(state,0,0);
  return(SCM_BOOL_F);
}

static SCM g_edit_header_dialog(SCM snd_n) 
{
  #define H_edit_header_dialog "(" S_edit_header_dialog " snd) opens the Edit Header dialog on sound snd"
  snd_info *sp; 
  sp = get_sp(snd_n); 
  if (sp) edit_header(sp); else return(NO_SUCH_SOUND);
  return(SCM_BOOL_F);
}

static SCM g_recorder_dialog(void) 
{
  #define H_recorder_dialog "(" S_recorder_dialog ") fires up the Recorder"
  snd_record_file(state); 
  return(SCM_BOOL_F);
}

static SCM g_yes_or_no_p(SCM msg) 
{
  #define H_yes_or_no_p "(" S_yes_or_no_p " message) displays message and waits for 'y' or 'n'; returns #t if 'y'"
  RTNBOOL(snd_yes_or_no_p(state,gh_scm2newstr(msg,NULL)));
}


#if ((USE_MOTIF) && (XmVERSION == 1))
static SCM g_show_edit_history(void) 
{
  #define H_show_edit_history "(" S_show_edit_history ") -> #t if Snd should open the edit history lists"
  RTNBOOL(show_edit_history(state));
}

static SCM g_set_show_edit_history(SCM on) 
{
  #define H_set_show_edit_history "(" S_set_show_edit_history " &optional (on #t)) sets " S_show_edit_history
  ERRB1(on,S_set_show_edit_history); 
  edit_history(state,bool_int_or_one(on)); 
  return(on);
}
#endif

#if HAVE_OSS
static SCM g_clear_audio_inputs (void) 
{
  #define H_clear_audio_inputs "(" S_clear_audio_inputs ") tries to reduce soundcard noise in Linux/OSS"
  mus_audio_clear_soundcard_inputs(); 
  return(SCM_BOOL_F);
}

void set_dsp_reset(int val);
static SCM g_set_dsp_reset(SCM val) 
{
  #define H_set_dsp_reset " sets a stupid flag in OSS -- will go away someday"
  set_dsp_reset(g_scm2int(val)); 
  return(val);
}

static SCM g_dsp_devices(SCM cards, SCM dsps, SCM mixers)
{
  #define H_dsp_devices "(" S_dsp_devices " cards dsps mixers) is a horrible kludge; it will go away anyday now"
  int i,ncards;
  int *gdsps,*gmixers;
  SCM_ASSERT((gh_number_p(cards)),cards,SCM_ARG1,S_dsp_devices);
  SCM_ASSERT((gh_vector_p(dsps)),dsps,SCM_ARG2,S_dsp_devices);
  SCM_ASSERT((gh_vector_p(mixers)),mixers,SCM_ARG3,S_dsp_devices);
  ncards = g_scm2int(cards);
  gdsps = (int *)CALLOC(ncards,sizeof(int));
  gmixers = (int *)CALLOC(ncards,sizeof(int));
  mus_audio_dsp_devices(ncards,gdsps,gmixers);
  for (i=0;i<ncards;i++)
    {
      gh_vector_set_x(dsps,gh_int2scm(i),gh_int2scm(gdsps[i]));
      gh_vector_set_x(mixers,gh_int2scm(i),gh_int2scm(gmixers[i]));
    }
  FREE(gdsps);
  FREE(gmixers);
  return(cards);
}
  
static SCM g_set_dsp_devices(SCM cards, SCM dsps, SCM mixers)
{
  #define H_set_dsp_devices "(" S_set_dsp_devices " cards dsps mixers) is a horrible kludge; it will go away anyday now"
  int i,ncards;
  int *gdsps,*gmixers;
  SCM_ASSERT((gh_number_p(cards)),cards,SCM_ARG1,S_dsp_devices);
  SCM_ASSERT((gh_vector_p(dsps)),dsps,SCM_ARG2,S_dsp_devices);
  SCM_ASSERT((gh_vector_p(mixers)),mixers,SCM_ARG3,S_dsp_devices);
  ncards = g_scm2int(cards);
  gdsps = (int *)CALLOC(ncards,sizeof(int));
  gmixers = (int *)CALLOC(ncards,sizeof(int));
  for (i=0;i<ncards;i++)
    {
      gdsps[i] = gh_vector_ref(dsps,gh_int2scm(i));
      gmixers[i] = gh_vector_ref(mixers,gh_int2scm(i));
    }
  mus_audio_set_dsp_devices(ncards,gdsps,gmixers);
  FREE(gdsps);
  FREE(gmixers);
  return(cards);
}
  
#endif

static SCM g_set_filter_env(SCM edata, SCM snd_n)
{
  #define H_set_filter_env "(" S_set_filter_env " val &optional snd) sets snd's filter envelope (in the control panel)"
  snd_info *sp;
  sp = get_sp(snd_n);
  if (sp == NULL) return(NO_SUCH_SOUND);
  if (sp->filter_env) free_env(sp->filter_env);
  sp->filter_env = get_env(edata,SCM_BOOL_F,S_set_filter_env);
  filter_env_changed(sp,sp->filter_env);
  return(edata);
}

static SCM g_filter_env(SCM snd_n)
{
  #define H_filter_env "(" S_filter_env " &optional snd) -> snd's filter envelope (in the control panel)"
  snd_info *sp;
  ERRSP(S_filter_env,snd_n,1);
  sp = get_sp(snd_n);
  if (sp) return(env2scm(sp->filter_env)); 
  return(NO_SUCH_SOUND);
}

static SCM g_env_selection(SCM edata, SCM base, SCM snd_n, SCM chn_n)
{
  #define H_env_selection "(" S_env_selection " env &optional (env-base 1.0) snd chn) applies envelope 'env'\n\
   to the currently selected portion of snd's channel chn using 'env-base' to determine how breakpoints are connected"

  chan_info *cp;
  env *e;
  ERRCP(S_env_selection,snd_n,chn_n,3);
  if (selection_is_current() == 0) return(NO_ACTIVE_SELECTION);
  cp = get_cp(snd_n,chn_n);
  if (cp == NULL)return(NO_SUCH_CHANNEL);
  e = get_env(edata,base,S_env_selection);
  if (e)
    {
      apply_env(cp,e,0,0,1.0,TRUE,FALSE,S_env_selection);
      free_env(e);
      return(SCM_BOOL_T);
    }
  return(SCM_BOOL_F);
}

static SCM g_env_sound(SCM edata, SCM samp_n, SCM samps, SCM base, SCM snd_n, SCM chn_n)
{
  #define H_env_sound "(" S_env_sound " env &optional (start-samp 0) samps (env-base 1.0) snd chn) applies\n\
   amplitude envelope 'env' (a list of breakpoints) to snd's channel chn starting at start-samp, going\n\
   either to the end of the sound or for 'samps' samples, with segments interpolating according to 'env-base'"

  chan_info *cp;
  env *e;
  int dur;
  ERRB2(samp_n,S_env_sound);
  ERRB3(samps,S_env_sound);
  ERRCP(S_env_sound,snd_n,chn_n,5);
  cp = get_cp(snd_n,chn_n);
  if (cp == NULL)return(NO_SUCH_CHANNEL);
  e = get_env(edata,base,S_env_sound);
  if (e)
    {
      dur = g_scm2intdef(samps,0);
      if (dur == 0) dur = current_ed_samples(cp);
      apply_env(cp,e,g_scm2intdef(samp_n,0),dur,1.0,FALSE,FALSE,S_env_sound);
      free_env(e);
      return(SCM_BOOL_T);
    }
  return(SCM_BOOL_F);
}

static SCM g_mix(SCM file, SCM chn_samp_n, SCM file_chn, SCM snd_n, SCM chn_n, SCM console)
{
  #define H_mix "(" S_mix " file &optional (chn-start 0) (file-chan 0) snd chn with-console)) mixes file\n\
   channel file-chan into snd's channel chn starting at chn-start (or at the cursor location if chan-start\n\
   is omitted), returning the new mix's id.  if with-console is #f, the data is mixed (no console is created).\n\
   If chn is omitted, file's channels are mixed until snd runs out of channels"

  chan_info *cp = NULL;
  char *name = NULL;
  int chans,id=-1;
  int with_mixer = 1;
  mixdata *md;
  ERRS1(file,S_mix);
  ERRB2(chn_samp_n,S_mix);
  ERRB3(file_chn,S_mix);
  ERRCP(S_mix,snd_n,chn_n,4);
  SCM_ASSERT((bool_or_arg_p(console)),console,SCM_ARG6,S_mix);
  name = full_filename(file);
  if (SCM_UNBNDP(console))
    with_mixer = with_mix_consoles(state);
  else with_mixer = bool_int_or_one(console);
  if (SCM_UNBNDP(chn_samp_n))
    {
      id = mix_complete_file(any_selected_sound(state),name,S_mix,with_mixer);
    }
  else
    {
      cp = get_cp(snd_n,chn_n);
      if (cp)
	{
	  chans = mus_sound_chans(name);
	  if (chans > 0)
	    {
	      md = file_mix_samples(g_scm2intdef(chn_samp_n,0),
				    mus_sound_samples(name)/chans,name,
				    cp,g_scm2intdef(file_chn,0),
				    DONT_DELETE_ME,1,S_mix,
				    with_mixer);
	      if (md) id = md->id;
	    }
	  else return(NO_SUCH_FILE);
	}
    }
  if (name) FREE(name);
  if (cp == NULL) return(NO_SUCH_CHANNEL);
  RTNINT(id);
}

static SCM g_key(SCM kbd, SCM buckybits, SCM snd, SCM chn)
{
  #define H_key "(" S_key " key modifiers &optional snd chn) simulates typing 'key' with 'modifiers' in snd's channel chn"
  chan_info *cp;
  ERRN1(kbd,S_key);
  ERRN2(buckybits,S_key);
  cp = get_cp(snd,chn);
  if (cp == NULL) return(NO_SUCH_CHANNEL);
  RTNINT(keyboard_command(cp,g_scm2int(kbd),g_scm2int(buckybits)));
}

static SCM g_add_to_main_menu(SCM label)
{
  #define H_add_to_main_menu "(" S_add_to_main_menu " label) adds label to the main (top-level) menu, returning its index"
  char *name = NULL;
  int val;
  ERRS1(label,S_add_to_main_menu);
  name = gh_scm2newstr(label,NULL);
  val = gh_add_to_main_menu(state,name);
  free(name);
  RTNINT(val);
}

static char **menu_strings = NULL; /* backwards compatibility */
static SCM *menu_functions = NULL;
static int callbacks_size = 0;
static int callb = 0;
#define CALLBACK_INCR 16

static SCM g_add_to_menu(SCM menu, SCM label, SCM callstr)
{
  #define H_add_to_menu "(" S_add_to_menu " menu label func) adds label to menu invoking func when activated\n\
   menu is the index returned by add-to-main-menu, func should be a function of no arguments"

  char *name;
  int i,err=0;
  ERRS2(label,S_add_to_menu);
  ERRN1(menu,S_add_to_menu);
  if (callbacks_size == callb)
    {
      callbacks_size += CALLBACK_INCR;
      if (callb == 0)
	{
	  menu_strings = (char **)CALLOC(callbacks_size,sizeof(char *));
	  menu_functions = (SCM *)CALLOC(callbacks_size,sizeof(SCM));
	}
      else 
	{
	  menu_strings = (char **)REALLOC(menu_strings,callbacks_size * sizeof(char *));
	  menu_functions = (SCM *)REALLOC(menu_functions,callbacks_size * sizeof(SCM));
	  for (i=callbacks_size - CALLBACK_INCR;i<callbacks_size;i++)
	    {
	      menu_strings[i] = NULL;
	      menu_functions[i] = 0;
	    }
	}
    }
  name = gh_scm2newstr(label,NULL);
  err = gh_add_to_menu(state,g_scm2int(menu),name,callb);
  free(name);
  if (err == -1) return(NO_SUCH_MENU);
  if (gh_string_p(callstr))
    menu_strings[callb] = gh_scm2newstr(callstr,NULL);
  else 
    {
      if ((menu_functions[callb]) && (gh_procedure_p(menu_functions[callb]))) snd_unprotect(menu_functions[callb]);
      menu_functions[callb] = callstr;
      snd_protect(callstr);
    }
  callb++;
  return(label);
}

void g_snd_callback(snd_state *ss, int callb)
{
  if (menu_functions[callb])
    g_call0(menu_functions[callb]);
  else scm_internal_stack_catch(SCM_BOOL_T,eval_str_wrapper,menu_strings[callb],snd_catch_scm_error,menu_strings[callb]);
}

static SCM g_remove_from_menu(SCM menu, SCM label)
{
  #define H_remove_from_menu "(" S_remove_from_menu " menu label) removes menu item label from menu"
  char *name;
  int val;
  ERRS2(label,S_remove_from_menu);
  ERRN1(menu,S_remove_from_menu);
  name = gh_scm2newstr(label,NULL);
  val = gh_remove_from_menu(state,g_scm2int(menu),name);
  free(name);
  RTNINT(val);
}

static SCM g_change_menu_label(SCM menu, SCM old_label, SCM new_label)
{
  #define H_change_menu_label "(" S_change_menu_label " menu old-label new-label) changes menu's label"
  char *old_name,*new_name;
  int val;
  ERRS2(old_label,S_change_menu_label);
  ERRS3(new_label,S_change_menu_label);
  ERRN1(menu,S_change_menu_label);
  old_name = gh_scm2newstr(old_label,NULL);
  new_name = gh_scm2newstr(new_label,NULL);
  val = gh_change_menu_label(g_scm2int(menu),old_name,new_name);
  free(old_name);
  free(new_name);
  RTNINT(val);
}

static SCM g_set_menu_sensitive(SCM menu, SCM label, SCM on)
{
  #define H_set_menu_sensitive "(" S_set_menu_sensitive " menu label &optional (on #t)) sets whether item label in menu is sensitive"
  char *name;
  int val;
  ERRN1(menu,S_set_menu_sensitive);
  ERRS2(label,S_set_menu_sensitive);
  ERRB3(on,S_set_menu_sensitive);
  name = gh_scm2newstr(label,NULL);
  val = gh_set_menu_sensitive(g_scm2int(menu),name,bool_int_or_one(on));
  free(name);
  RTNINT(val);
}

static SCM g_autocorrelate(SCM reals)
{
  #define H_autocorrelate "(" S_autocorrelate " data) returns (in place) the autocorrelation of data (vector or vct)"
  /* assumes length is power of 2 */
  vct *v1 = NULL;
  int n,i;
  Float *rl;
  ERRV1(reals,S_autocorrelate);
  if (vct_p(reals))
    {
      v1 = (vct *)GH_VALUE_OF(reals);
      rl = v1->data;
      n = v1->length;
    }
  else
    {
      n = gh_vector_length(reals);
      rl = (Float *)CALLOC(n,sizeof(Float));
      for (i=0;i<n;i++) rl[i] = gh_scm2double(gh_vector_ref(reals,gh_int2scm(i)));
    }
  autocorrelation(rl,n);
  if (v1 == NULL) 
    {
      for (i=0;i<n;i++) gh_vector_set_x(reals,gh_int2scm(i),gh_double2scm(rl[i]));
      FREE(rl);
    }
  return(reals);
}

static SCM g_fft_1(SCM reals, SCM imag, SCM sign, int use_fft)
{
  vct *v1 = NULL,*v2 = NULL;
  int ipow,n,n2,i,isign = 1;
  Float *rl,*im;
  ERRV1(reals,((use_fft) ? S_fft : S_convolve_arrays));
  ERRV2(imag,((use_fft) ? S_fft : S_convolve_arrays));
  if ((vct_p(reals)) && (vct_p(imag)))
    {
      v1 = (vct *)GH_VALUE_OF(reals);
      v2 = (vct *)GH_VALUE_OF(imag);
      n = v1->length;
    }
  else
    n = gh_vector_length(reals);
  ipow = (int)ceil(log((Float)n)/log(2.0));
  n2 = (int)pow(2.0,(Float)ipow);
  if ((!v1) || (n != n2))
    {
      rl = (Float *)CALLOC(n2,sizeof(Float));
      im = (Float *)CALLOC(n2,sizeof(Float));
    }
  else
    {
      rl = v1->data;
      im = v2->data;
    }
  if (gh_number_p(sign)) isign = g_scm2int(sign);
  if (isign == 0) isign = 1;
  if (v1 == NULL)
    {
      for (i=0;i<n;i++)
	{
	  rl[i] = gh_scm2double(gh_vector_ref(reals,gh_int2scm(i)));
	  im[i] = gh_scm2double(gh_vector_ref(imag,gh_int2scm(i)));
	}
    }
  else
    {
      if (n != n2)
	{
	  for (i=0;i<n;i++)
	    {
	      rl[i] = v1->data[i];
	      im[i] = v2->data[i];
	    }
	}
    }
  if (use_fft) 
    {
      mus_fft(rl,im,n2,isign);
      if (v1 == NULL)
	{
	  for (i=0;i<n;i++)
	    {
	      gh_vector_set_x(reals,gh_int2scm(i),gh_double2scm(rl[i]));
	      gh_vector_set_x(imag,gh_int2scm(i),gh_double2scm(im[i]));
	    }
	}
      else
	{
	  if (n != n2)
	    {
	      for (i=0;i<n;i++)
		{
		  v1->data[i] = rl[i];
		  v2->data[i] = im[i];
		}
	    }
	}
    }
  else 
    {
      mus_convolution(rl,im,n2);
      if (v1 == NULL)
	{
	  for (i=0;i<n;i++)
	    {
	      gh_vector_set_x(reals,gh_int2scm(i),gh_double2scm(rl[i]));
	    }
	}
      else
	{
	  if (n != n2)
	    {
	      for (i=0;i<n;i++) v1->data[i] = rl[i];
	    }
	}
    }
  if ((!v1) || (n != n2))
    {
      FREE(rl);
      FREE(im);
    }
  return(reals);
}

static SCM g_fft(SCM reals, SCM imag, SCM sign)
{
  #define H_fft "(" S_fft " reals imags &optional (sign 1)) ffts the data returning the result in reals\n\
   if sign is -1, performs inverse fft"

  return(g_fft_1(reals,imag,sign,TRUE));
}

static SCM g_convolve_with(SCM file, SCM new_amp, SCM snd_n, SCM chn_n)
{
  #define H_convolve_with "(" S_convolve_with " file &optional (amp 1.0) snd chn) convolves file\n\
   with snd's channel chn (or the currently sync'd channels), amp is the resultant peak amp"

  chan_info *cp;
  Float amp;
  char *fname = NULL;
  ERRS1(file,S_convolve_with);
  ERRCP(S_convolve_with,snd_n,chn_n,3);
  cp = get_cp(snd_n,chn_n);
  if (cp == NULL) return(NO_SUCH_CHANNEL);
  if (gh_number_p(new_amp)) 
    amp = gh_scm2double(new_amp);
  else
    {
      if (SCM_FALSEP(new_amp))
	amp = 0.0;
      else amp = 1.0;
    }
  fname = full_filename(file);
  if (snd_probe_file(cp->state,fname) == FILE_EXISTS)
    convolve_with(fname,amp,cp);
  else return(NO_SUCH_FILE);
  if (fname) FREE(fname);
  return(file);
}

static SCM g_snd_spectrum(SCM data, SCM win, SCM len, SCM linear_or_dB)
{
  #define H_snd_spectrum "(" S_snd_spectrum " data window len linear-or-dB) return spectrum of data (vct) in data\n\
   using fft-window win and fft length len"

  int i,n,linear;
  Float maxa,todb,lowest,val;
  Float *idat,*rdat,*window;
  vct *v;
  SCM_ASSERT((vct_p(data)),data,SCM_ARG1,S_snd_spectrum);
  SCM_ASSERT((gh_number_p(win)),win,SCM_ARG2,S_snd_spectrum);
  SCM_ASSERT((gh_number_p(len)),len,SCM_ARG3,S_snd_spectrum);
  ERRB1(linear_or_dB,S_snd_spectrum);
  v = get_vct(data);
  rdat = v->data;
  n = g_scm2int(len);
  if (SCM_TRUE_P(linear_or_dB)) linear=1; else linear=0;
  idat = (Float *)CALLOC(n,sizeof(Float));
  window = (Float *)CALLOC(n,sizeof(Float));
  make_fft_window_1(window,n,g_scm2int(win),0.0,0);
  for (i=0;i<n;i++) rdat[i] *= window[i];
  FREE(window);
  mus_fft(rdat,idat,n,1);
  lowest = 0.00000001;
  maxa = 0.0;
  n = n/2;
  for (i=0;i<n;i++)
    {
      val = rdat[i]*rdat[i]+idat[i]*idat[i];
      if (val < lowest)
	idat[i] = .0001;
      else 
	{
	  idat[i]=sqrt(val);
	  if (idat[i] > maxa) maxa=idat[i];
	}
    }
  if (maxa>0.0)
    {
      maxa=1.0/maxa;
      if (linear == 0) /* dB */
	{
	  todb=20.0/log(10.0);
	  for (i=0;i<n;i++) idat[i]=todb*log(idat[i]*maxa);
	}
      else for (i=0;i<n;i++) idat[i] *= maxa;
    }
  return(scm_return_first(make_vct(n,idat),data));
}

static SCM g_convolve_selection_with(SCM file, SCM new_amp)
{
  #define H_convolve_selection_with "(" S_convolve_selection_with " file &optional (amp 1.0)) convolves the current selection\n\
   with file; amp is the resultant peak amp"

  Float amp;
  char *fname = NULL;
  ERRS1(file,S_convolve_selection_with);
  if (selection_is_current() == 0) return(NO_ACTIVE_SELECTION);
  if (gh_number_p(new_amp)) 
    amp = gh_scm2double(new_amp);
  else
    {
      if (SCM_FALSEP(new_amp))
	amp = 0.0;
      else amp = 1.0;
    }
  fname = full_filename(file);
  if (snd_probe_file(state,fname) == FILE_EXISTS)
    convolve_with(fname,amp,NULL);
  else return(NO_SUCH_FILE);
  if (fname) FREE(fname);
  return(file);
}

static SCM g_convolve(SCM reals, SCM imag)
{
  #define H_convolve "(" S_convolve_arrays " rl1 rl2) convolves vectors or vcts rl1 and rl2, result in rl1 (which needs to be big enough)"
  /* if reals is a string=filename and imag is a Float (or nada), assume user missppelledd convolve-with */
  if (gh_string_p(reals))
    return(g_convolve_with(reals,imag,SCM_BOOL_F,SCM_BOOL_F));
  /* result in reals (which needs to be big enough and zero padded) */
  else return(g_fft_1(reals,imag,gh_int2scm(1),FALSE));
}

static SCM g_src_sound(SCM ratio_or_env, SCM base, SCM snd_n, SCM chn_n)
{
  #define H_src_sound "(" S_src_sound " ratio-or-env &optional (base 1.0) snd chn) sampling-rate converts snd's channel chn\n\
   by ratio, or following an envelope. Negative ratio reverses the sound"

  chan_info *cp;
  ERRCP(S_src_sound,snd_n,chn_n,3);
  cp = get_cp(snd_n,chn_n);
  if (cp == NULL) return(NO_SUCH_CHANNEL);
  if (gh_number_p(ratio_or_env))
    src_env_or_num(state,cp,NULL,gh_scm2double(ratio_or_env),TRUE,FALSE,S_src_sound,FALSE);
  else src_env_or_num(state,cp,get_env(ratio_or_env,base,S_src_sound),1.0,FALSE,FALSE,S_src_sound,FALSE);
  return(ratio_or_env);
}

static SCM g_src_selection(SCM ratio_or_env, SCM base)
{
  #define H_src_selection "(" S_src_selection " ratio-or-env &optional (base 1.0)) sampling-rate converts the\n\
   currently selected data by ratio (which can be an envelope)"

  chan_info *cp;
  if (selection_is_current() == 0) return(NO_ACTIVE_SELECTION);
  cp = get_cp(SCM_BOOL_F,SCM_BOOL_F);
  if (cp == NULL) return(NO_SUCH_CHANNEL);
  if (gh_number_p(ratio_or_env))
    src_env_or_num(state,cp,NULL,gh_scm2double(ratio_or_env),TRUE,FALSE,S_src_selection,TRUE);
  else src_env_or_num(state,cp,get_env(ratio_or_env,base,S_src_selection),1.0,FALSE,FALSE,S_src_selection,TRUE);
  return(ratio_or_env);
}

static SCM g_filter_sound(SCM e, SCM order, SCM snd_n, SCM chn_n)
{
  #define H_filter_sound "(" S_filter_sound " filter order &optional snd chn) applies FIR filter to snd's channel chn\n\
   'filter' is either the frequency response envelope or a vct object with the actual coefficients"

  chan_info *cp;
  vct *v;
  int len;
  SCM_ASSERT((gh_vector_p(e)) || (gh_list_p(e)) || (vct_p(e)),e,SCM_ARG1,S_filter_sound);
  ERRN2(order,S_filter_sound);
  ERRCP(S_filter_sound,snd_n,chn_n,3);
  cp = get_cp(snd_n,chn_n);
  if (cp == NULL) return(NO_SUCH_CHANNEL);
  len = g_scm2int(order);
  SCM_ASSERT((len > 0),order,SCM_ARG2,S_filter_sound); /* not really a type error... */
  if (vct_p(e)) /* the filter coefficients direct */
    {
      v = get_vct(e);
      SCM_ASSERT((len <= v->length),e,SCM_ARG1,S_filter_sound);
      apply_filter(cp,len,NULL,FALSE,S_filter_sound,FALSE,v->data);
    }
  else apply_filter(cp,len,get_env(e,gh_double2scm(1.0),S_filter_sound),FALSE,S_filter_sound,FALSE,NULL); 
  return(scm_return_first(SCM_BOOL_T,e));
}

static SCM g_filter_selection(SCM e, SCM order)
{
  #define H_filter_selection "(" S_filter_selection " filter order) applies filter to current selection"
  chan_info *cp;
  vct *v;
  int len;
  SCM_ASSERT((gh_vector_p(e)) || (gh_list_p(e)) || (vct_p(e)),e,SCM_ARG1,S_filter_selection);
  ERRN2(order,S_filter_selection);
  if (selection_is_current() == 0) return(NO_ACTIVE_SELECTION);
  cp = get_cp(SCM_BOOL_F,SCM_BOOL_F);
  if (cp == NULL) return(NO_SUCH_CHANNEL);
  len = g_scm2int(order);
  SCM_ASSERT((len > 0),order,SCM_ARG2,S_filter_selection);
  if (vct_p(e)) /* the filter coefficients direct */
    {
      v = get_vct(e);
      SCM_ASSERT((len <= v->length),e,SCM_ARG1,S_filter_selection);
      apply_filter(cp,len,NULL,FALSE,S_filter_selection,TRUE,v->data);
    }
  else apply_filter(cp,len,get_env(e,gh_double2scm(1.0),S_filter_selection),FALSE,S_filter_selection,TRUE,NULL); 
  return(scm_return_first(SCM_BOOL_T,e));
}

static SCM g_smooth(SCM beg, SCM num, SCM snd_n, SCM chn_n)
{
  #define H_smooth "(" S_smooth " start-samp samps &optional snd chn) smooths data from start-samp for samps in snd's channel chn"
  chan_info *cp;
  ERRN1(beg,S_smooth);
  ERRN2(num,S_smooth);
  ERRCP(S_smooth,snd_n,chn_n,3);
  cp = get_cp(snd_n,chn_n);
  if (cp == NULL) return(NO_SUCH_CHANNEL);
  cos_smooth(cp,g_scm2int(beg),g_scm2int(num),FALSE,S_smooth); 
  return(SCM_BOOL_T);
}

static SCM g_smooth_selection(void)
{
  #define H_smooth_selection "(" S_smooth_selection ") smooths the data in the currently selected portion"
  chan_info *cp;
  if (selection_is_current() == 0) return(NO_ACTIVE_SELECTION);
  cp = get_cp(SCM_BOOL_F,SCM_BOOL_F);
  if (cp == NULL) return(NO_SUCH_CHANNEL);
  cos_smooth(cp,0,0,TRUE,S_smooth_selection);
  return(SCM_BOOL_T);
}

static SCM g_reverse_sound(SCM snd_n, SCM chn_n)
{
  #define H_reverse_sound "(" S_reverse_sound " &optional snd chn) reverses snd's channel chn"
  chan_info *cp;
  ERRCP(S_reverse_sound,snd_n,chn_n,1);
  cp = get_cp(snd_n,chn_n);
  if (cp) reverse_sound(cp,FALSE); else return(NO_SUCH_CHANNEL);
  return(SCM_BOOL_F);
}

static SCM g_reverse_selection(void)
{
  #define H_reverse_selection "(" S_reverse_selection ") reverses the data in the currently selected portion"
  chan_info *cp;
  if (selection_is_current() == 0) return(NO_ACTIVE_SELECTION);
  cp = get_cp(SCM_BOOL_F,SCM_BOOL_F);
  if (cp) reverse_sound(cp,TRUE); else return(NO_SUCH_CHANNEL);
  return(SCM_BOOL_F);
}

static SCM g_save_selection(SCM filename, SCM header_type, SCM data_format, SCM srate, SCM comment)
{
  #define H_save_selection "(" S_save_selection " filename &optional header-type data-format srate comment)\n\
   saves the current selection in filename using the indicated file attributes"

  snd_state *ss;
  int type,format,sr,err;
  char *com = NULL, *fname = NULL;
  ERRS1(filename,S_save_selection);
  ss = get_global_state();
  if (gh_number_p(header_type)) 
    type = g_scm2int(header_type); 
#if MUS_LITTLE_ENDIAN
  else type = MUS_RIFF;
#else
  else type = MUS_NEXT;
#endif
  format = g_scm2intdef(data_format,MUS_OUT_FORMAT);
  sr = g_scm2intdef(srate,region_srate(0));
  if (gh_string_p(comment)) com = gh_scm2newstr(comment,NULL); else com = NULL;
  fname = full_filename(filename);
  err = save_selection(ss,fname,type,format,sr,com);
  if (fname) FREE(fname);
  if (com) free(com);
  if (err == 0) return(filename);
  return(SCM_BOOL_F);
}
  
static SCM g_graph(SCM ldata, SCM xlabel, SCM x0, SCM x1, SCM y0, SCM y1, SCM snd_n, SCM chn_n)
{
  #define H_graph "(" S_graph " data &optional xlabel x0 x1 y0 y1 snd chn) displays 'data' as a graph\n\
   with x axis label 'xlabel', axis units going from x0 to x1 and y0 to y1; 'data' can be a list, vct, or vector"

  chan_info *cp;
  lisp_grf *lg;
  SCM data = SCM_UNDEFINED;
  char *label = NULL;
  vct *v = NULL;
  int i,len,graph,graphs;
  Float ymin,ymax,val,nominal_x0,nominal_x1;
  /* ldata can be a vct object, a vector, or a list of either */
  SCM_ASSERT(((vct_p(ldata)) || (gh_vector_p(ldata)) || (gh_list_p(ldata))),ldata,SCM_ARG1,S_graph);
  ERRCP(S_graph,snd_n,chn_n,7);
  cp = get_cp(snd_n,chn_n);
  ymin = 32768.0;
  ymax = -32768.0;
  if (cp == NULL)return(NO_SUCH_CHANNEL);
  if ((cp->sound_ctr == -1) || 
      (cp->sounds == NULL) || 
      (cp->sounds[cp->sound_ctr] == NULL) ||
      (cp->axis == NULL))
    return(SCM_BOOL_F);
  if (!(gh_list_p(ldata))) graphs = 1; else graphs = gh_length(ldata);
  lg = cp->lisp_info;
  if ((lg) && (graphs != lg->graphs)) cp->lisp_info = free_lisp_info(cp);
  if (!(cp->lisp_info))
    {
      cp->lisp_info = (lisp_grf *)CALLOC(graphs,sizeof(lisp_grf));
      lg = cp->lisp_info;
      lg->len = (int *)CALLOC(graphs,sizeof(int));
      lg->graphs = graphs;
      lg->data = (Float **)CALLOC(graphs,sizeof(Float *));
    }
  for (graph=0;graph<graphs;graph++)
    {
      if (gh_list_p(ldata))
	data = gh_list_ref(ldata,gh_int2scm(graph));
      else data = ldata;
      if (vct_p(data))
	{
	  v = (vct *)GH_VALUE_OF(data);
	  len = v->length;
	}
      else len = gh_vector_length(data);
      lg = cp->lisp_info;
      if (lg->len[graph] != len)
	{
	  if (lg->data[graph]) FREE(lg->data[graph]);
	  lg->data[graph] = (Float *)CALLOC(len,sizeof(Float));
	  lg->len[graph] = len;
	}
      if ((gh_number_p(y0)) && (gh_number_p(y1)))
	{
	  if (v)
	    for (i=0;i<len;i++) lg->data[graph][i] = v->data[i];
	  else for (i=0;i<len;i++) lg->data[graph][i] = gh_scm2double(gh_vector_ref(data,gh_int2scm(i)));
	}
      else
	{
	  if (v)
	    {
	      for (i=0;i<len;i++)
		{
		  val = v->data[i];
		  lg->data[graph][i] = val; 
		  if (ymin > val) ymin = val;
		  if (ymax < val) ymax = val;
		}
	    }
	  else
	    {
	      for (i=0;i<len;i++)
		{
		  val = gh_scm2double(gh_vector_ref(data,gh_int2scm(i)));
		  lg->data[graph][i] = val; 
		  if (ymin > val) ymin = val;
		  if (ymax < val) ymax = val;
		}
	    }
	}
    }
  if ((gh_number_p(y0)) && (gh_number_p(y1)))
    {
      ymin = gh_scm2double(y0);
      ymax = gh_scm2double(y1);
    }
  if (gh_string_p(xlabel)) label = gh_scm2newstr(xlabel,NULL); 
  if (gh_number_p(x0)) nominal_x0 = gh_scm2double(x0); else nominal_x0 = 0.0;
  if (gh_number_p(x1)) nominal_x1 = gh_scm2double(x1); else nominal_x1 = 1.0;
  lg->axis = make_axis_info(cp,nominal_x0,nominal_x1,ymin,ymax,label,nominal_x0,nominal_x1,ymin,ymax,lg->axis);
  if (label) free(label);
  cp->lisp_graphing = 1;
  display_channel_data(cp,cp->sound,cp->state);
  return(scm_return_first(SCM_BOOL_F,data));
}

static SCM g_bind_key(SCM key, SCM state, SCM code, SCM ignore_prefix)
{
  #define H_bind_key "(" S_bind_key " key modifiers func (ignore-prefix #f)) causes 'key' (an integer)\n\
   when typed with 'modifiers' (1:shift, 4:control, 8:meta) to invoke 'func', a function of\n\
   no arguments.  If ignore-prefix is #t, preceding C-u arguments are not handled by Snd itself.\n\
   The function should return one of the cursor choices (e.g. cursor-no-action)."

  int ip;
  ERRN1(key,S_bind_key);
  ERRN2(state,S_bind_key);
  SCM_ASSERT((SCM_FALSEP(code) || gh_procedure_p(code)),code,SCM_ARG3,S_bind_key);
  if ((SCM_FALSEP(ignore_prefix)) || (SCM_UNBNDP(ignore_prefix)) ||  
      ((gh_number_p(ignore_prefix)) && (g_scm2int(ignore_prefix) == 0)))
    ip = 0;
  else ip = 1;
  if (SCM_FALSEP(code))
    set_keymap_entry(g_scm2int(key),g_scm2int(state),ip,SCM_UNDEFINED);
  else 
    if (procedure_ok(code,0,0,S_bind_key,"func",3))
      set_keymap_entry(g_scm2int(key),g_scm2int(state),ip,code);
  return(SCM_BOOL_T);
}

static SCM g_save_edit_history(SCM filename, SCM snd, SCM chn)
{
  #define H_save_edit_history "(" S_save_edit_history " filename &optional snd chn) saves snd channel's chn edit history in filename"
  FILE *fd;
  int i,j;
  snd_info *sp;
  chan_info *cp;
  char *mcf = NULL;
  ERRS1(filename,S_save_edit_history);
  ERRCP(S_save_edit_history,snd,chn,2);
  fd = fopen(mcf = full_filename(filename),"w");
  if (mcf) FREE(mcf);
  if (fd)
    {
      if ((gh_number_p(chn)) && (gh_number_p(snd)))
	{
	  cp = get_cp(snd,chn);
	  if (cp) edit_history_to_file(fd,cp);
	}
      else
	{
	  if (gh_number_p(snd))
	    {
	      sp = get_sp(snd);
	      if (sp)
		for (i=0;i<sp->nchans;i++)
		  edit_history_to_file(fd,sp->chans[i]);
	    }
	  else
	    {
	      for (i=0;i<state->max_sounds;i++)
		{
		  if ((sp=((snd_info *)(state->sounds[i]))))
		    {
		      if (sp->inuse)
			{
			  for (j=0;j<sp->nchans;j++)
			    edit_history_to_file(fd,sp->chans[j]);
			}
		    }
		}
	    }
	}
      fclose(fd);
      return(SCM_BOOL_T);
    }
  return(CANNOT_SAVE);
}

static SCM g_set_oss_buffers(SCM num, SCM size)
{
  #define H_set_oss_buffers "(" S_set_oss_buffers " num size) sets Linux OSS 'fragment' number and size"
#if (HAVE_OSS || HAVE_ALSA)
  ERRN1(num,S_set_oss_buffers);
  ERRN2(size,S_set_oss_buffers);
  mus_audio_set_oss_buffers(g_scm2int(num),g_scm2int(size));
#endif
  return(SCM_BOOL_F);
}

static SCM g_describe_audio(void) 
{
  #define H_describe_audio "("  S_describe_audio ") posts a description of the audio hardware state in the Help dialog"
  snd_help(state,"Audio State",mus_audio_report()); 
  return(SCM_BOOL_T);
}

static SCM g_add_sound_file_extension(SCM ext)
{
  #define H_add_sound_file_extension "(" S_add_sound_file_extension " ext)  adds the file extension ext to the list of sound file extensions"
  char *name;
  ERRS1(ext,S_add_sound_file_extension);
  name = gh_scm2newstr(ext,NULL);
  add_sound_file_extension(name);
  free(name);
  return(ext);
}

static SCM g_string_length(SCM str)
{
  #define H_string_length "(" S_string_length " str) -> length of string str"
  char *val = NULL;
  if (gh_string_p(str)) val = gh_scm2newstr(str,NULL);
  if (val)
    {
      if (*val) RTNINT(strlen(val));
      free(val);
    }
  RTNINT(0);
}

static SCM g_call_apply(SCM snd)
{
  #define H_call_apply "(" S_call_apply " &optional snd) is equivalent to clicking the control panel 'Apply' button"
  snd_info *sp;
  ERRSP(S_call_apply,snd,1);
  sp = get_sp(snd);
  if (sp) run_apply_to_completion(sp); else return(NO_SUCH_SOUND);
  return(SCM_BOOL_F);
}


/* -------- EXTERNAL PROGRAMS -------- */

#define USE_FULL_FILE 0
#define USE_SELECTION 1
#define USE_ONE_FILE 1
#define USE_MANY_FILES 0

static SCM g_temp_filenames(SCM data)
{
  #define H_temp_filenames "(" S_temp_filenames " data) -> vector of temp filenames (used by sound-to-temp et al)"
  snd_exf *program_data;
  int i;
  SCM lst;
  program_data = (snd_exf *)(gh_scm2ulong(data));
  lst = gh_make_vector(gh_int2scm(program_data->files),SCM_BOOL_F);
  for (i=0;i<program_data->files;i++)
    gh_vector_set_x(lst,gh_int2scm(i),gh_str02scm(program_data->old_filenames[i]));
  return(lst);
}

static SCM g_sound_to_temp_1(SCM ht, SCM df, int selection, int one_file)
{
  snd_exf *program_data;
  chan_info *cp;
  int type,format;
  if ((selection) && (selection_is_current() == 0)) return(NO_ACTIVE_SELECTION);
  cp = current_channel(state);
  if (cp)
    {
      type = g_scm2intdef(ht,MUS_UNSUPPORTED);
      format = g_scm2intdef(df,MUS_UNSUPPORTED);
      program_data = snd_to_temp(cp,selection,one_file,type,format);
      if (program_data)
	return((SCM)(gh_ulong2scm((unsigned long)program_data)));
    }
  return(SCM_BOOL_F);
}

static SCM g_sound_to_temp(SCM ht, SCM df) 
{
  #define H_sound_to_temp "(" S_sound_to_temp " &optional header-type data-format) writes the syncd data to a temp file\n\
   with the indicated header type and data format; returns temp file name"

  return(g_sound_to_temp_1(ht,df,USE_FULL_FILE,USE_ONE_FILE));
}

static SCM g_sound_to_temps(SCM ht, SCM df) 
{
  #define H_sound_to_temps "(" S_sound_to_temps " &optional header-type data-format) writes the syncd data to mono temp files\n\
   with the indicated header type and data format; returns temp file names"

  return(g_sound_to_temp_1(ht,df,USE_FULL_FILE,USE_MANY_FILES));
}

static SCM g_selection_to_temp(SCM ht, SCM df) 
{
  #define H_selection_to_temp "(" S_selection_to_temp " &optional header-type data-format) writes the selected data to a temp file\n\
   with the indicated header type and data format; returns temp file name"

  return(g_sound_to_temp_1(ht,df,USE_SELECTION,USE_ONE_FILE));
}

static SCM g_selection_to_temps(SCM ht, SCM df) 
{
  #define H_selection_to_temps "(" S_selection_to_temps " &optional header-type data-format) writes the selected data to mono temp files\n\
   with the indicated header type and data format; returns temp file names"

  return(g_sound_to_temp_1(ht,df,USE_SELECTION,USE_MANY_FILES));
}

static SCM g_temp_to_sound(SCM data, SCM new_name, SCM origin)
{
  #define H_temp_to_sound "(" S_temp_to_sound " data new-name origin) reads new-name to complete the edit begun by " S_sound_to_temp "\n\
   using data returned by the latter and origin as the edit history entry for the edit"

  snd_exf *program_data;
  ERRS2(new_name,S_temp_to_sound);
  ERRS3(origin,S_temp_to_sound);
  program_data = (snd_exf *)(gh_scm2ulong(data));
  program_data->new_filenames[0] = gh_scm2newstr(new_name,NULL);
  temp_to_snd(state,program_data,gh_scm2newstr(origin,NULL));
  return(SCM_BOOL_T);
}

static SCM g_temps_to_sound(SCM data, SCM new_names, SCM origin)
{
  #define H_temps_to_sound "(" S_temps_to_sound " data new-names origin) reads new-names to complete the edit begun by " S_sound_to_temps "\n\
   using data returned by the latter and origin as the edit history entry for the edit"

  snd_exf *program_data;
  int i;
  ERRVECT2(new_names,S_temps_to_sound);
  ERRS3(origin,S_temps_to_sound);
  program_data = (snd_exf *)(gh_scm2ulong(data));
  for (i=0;i<(int)gh_vector_length(new_names);i++)
    program_data->new_filenames[i] = gh_scm2newstr(gh_vector_ref(new_names,gh_int2scm(i)),NULL);
  temp_to_snd(state,program_data,gh_scm2newstr(origin,NULL));
  return(SCM_BOOL_T);
}

static SCM g_start_progress_report(SCM snd)
{
  #define H_start_progress_report "(" S_start_progress_report " &optional snd) posts the hour-glass icon"
  snd_info *sp;
  ERRB1(snd,S_start_progress_report);
  ERRSP(S_start_progress_report,snd,1);
  sp = get_sp(snd);
  if (sp) start_progress_report(state,sp,NOT_FROM_ENVED); else return(NO_SUCH_SOUND);
  return(SCM_BOOL_T);
}

static SCM g_finish_progress_report(SCM snd)
{
  #define H_finish_progress_report "(" S_finish_progress_report " &optional snd) removes the hour-glass icon"
  snd_info *sp;
  ERRB1(snd,S_finish_progress_report);
  ERRSP(S_finish_progress_report,snd,1);
  sp = get_sp(snd);
  if (sp) finish_progress_report(state,sp,NOT_FROM_ENVED); else return(NO_SUCH_SOUND);
  return(SCM_BOOL_T);
}

static SCM g_progress_report(SCM pct, SCM name, SCM cur_chan, SCM chans, SCM snd)
{
  #define H_progress_report "(" S_progress_report " pct &optional name cur-chan chans snd) updates an on-going\n\
   'progress report' (e. g. an animated hour-glass icon) in snd using pct to indicate how far along we are"

  snd_info *sp;
  char *str;
  ERRN1(pct,S_progress_report);
  ERRSP(S_progress_report,snd,5);
  sp = get_sp(snd);
  if (sp) 
    {
      if (gh_string_p(name)) str = gh_scm2newstr(name,NULL); else str = copy_string("something useful");
      progress_report(state,sp,str,
		      g_scm2intdef(cur_chan,0),
		      g_scm2intdef(chans,sp->nchans),
		      gh_scm2double(pct),
		      NOT_FROM_ENVED);
      free(str);
    }
  else return(NO_SUCH_SOUND);
  return(SCM_BOOL_T);
}

static int chan_ctr=0;

static int init_as_one_edit(chan_info *cp, void *ptr) 
{
  ((int *)ptr)[chan_ctr] = cp->edit_ctr; 
  chan_ctr++; 
  return(0);
}

static int finish_as_one_edit(chan_info *cp, void *ptr) 
{
  int one_edit;
  one_edit = (((int *)ptr)[chan_ctr]+1);
  if (cp->edit_ctr > one_edit)
    {
      while (cp->edit_ctr > one_edit) backup_edit_list(cp);
      if (cp->mixes) backup_mix_list(cp,one_edit);
    }
  update_graph(cp,NULL); 
  chan_ctr++; 
  return(0);
}

static SCM g_as_one_edit(SCM proc)
{
  #define H_as_one_edit "(" S_as_one_edit " func) runs func, collecting all edits into one from the edit historys' point of view"
  int chans;
  int *cur_edits;
  SCM result = SCM_BOOL_F;
  SCM_ASSERT(gh_procedure_p(proc),proc,SCM_ARG1,S_as_one_edit);
  chans = active_channels(state,TRUE);
  if (chans > 0)
    {
      cur_edits = (int *)CALLOC(chans,sizeof(int));
      chan_ctr = 0;
      map_over_chans(state,init_as_one_edit,(void *)cur_edits);
      result = g_call0(proc);
      chan_ctr = 0;
      map_over_chans(state,finish_as_one_edit,(void *)cur_edits);
      FREE(cur_edits);
    }
  return(result);
}

static SCM g_apropos(SCM text)
{
  #define H_apropos "(apropos name) returns possible continuations of name"
  char *res=NULL,*str=NULL;
  SCM val = SCM_BOOL_F;
  SCM_ASSERT((gh_string_p(text) || gh_symbol_p(text)),text,SCM_ARG1,"apropos");
  if (gh_string_p(text))
    str = gh_scm2newstr(text,NULL);
  else str = gh_symbol2newstr(text,NULL);
  res = snd_apropos(state,str);
  if (str) {free(str); str=NULL;}
  if (res) 
    {
      val = gh_str02scm(res);
      FREE(res);
    }
  return(val);
}

static SCM g_edit_fragment(SCM ctr, SCM snd, SCM chn)
{
  #define H_edit_fragment "(" S_edit_fragment " ctr &optional snd chn) returns the edit history entry at 'ctr'\n\
   associated with snd's channel chn; this is a list (origin type start-sample samps)"

  chan_info *cp;
  ERRCP(S_edit_fragment,snd,chn,2);
  ERRN1(ctr,S_edit_fragment);
  cp = get_cp(snd,chn);
  if (cp) return(snd_edit_fragment2scm(cp,g_scm2int(ctr))); else return(NO_SUCH_CHANNEL);
  return(SCM_EOL);
}

void init_mus2scm_module(void);

static SCM fft_hook,open_hook,during_open_hook,close_hook,exit_hook,start_hook,mouse_press_hook,after_open_hook;
static SCM mouse_release_hook,mouse_drag_hook,key_press_hook,stop_playing_hook,stop_playing_region_hook;
static SCM mark_click_hook,start_playing_hook,output_comment_hook,output_name_hook,multichannel_mix_hook;
static SCM mix_console_state_changed_hook,mix_speed_changed_hook,mix_amp_changed_hook,mix_position_changed_hook;
static SCM graph_hook,after_graph_hook,mus_error_hook,snd_error_hook,snd_warning_hook;
#if FILE_PER_CHAN
  static SCM open_multifile_sound_hook,save_multifile_sound_hook;
#endif
static SCM memo_sound;

#if HAVE_LADSPA
  void g_ladspa_to_snd(SCM local_doc);
#endif

void g_initialize_gh(snd_state *ss)
{
  SCM local_doc;
  state = ss;

  local_doc = scm_permanent_object(scm_string_to_symbol(gh_str02scm("documentation")));
  mus_sndlib2scm_initialize();

#if MUS_LITTLE_ENDIAN
  gh_eval_str("(define little-endian? (lambda nil #t))");
#else
  gh_eval_str("(define little-endian? (lambda nil #f))");
#endif

  /* ---------------- CONSTANTS ---------------- */

  gh_define(S_amplitude_env,gh_int2scm(AMPLITUDE_ENV));
  gh_define(S_spectrum_env,gh_int2scm(SPECTRUM_ENV));
  gh_define(S_srate_env,gh_int2scm(SRATE_ENV));

  gh_define(S_graph_lines,gh_int2scm(GRAPH_LINES));
  gh_define(S_graph_dots,gh_int2scm(GRAPH_DOTS));
  gh_define(S_graph_filled,gh_int2scm(GRAPH_FILLED));
  gh_define(S_graph_dots_and_lines,gh_int2scm(GRAPH_DOTS_AND_LINES));
  gh_define(S_graph_lollipops,gh_int2scm(GRAPH_LOLLIPOPS));

  gh_define(S_normal_fft,gh_int2scm(NORMAL_FFT));
  gh_define(S_sonogram,gh_int2scm(SONOGRAM));
  gh_define(S_spectrogram,gh_int2scm(SPECTROGRAM));

  gh_define(S_focus_left,gh_int2scm(FOCUS_LEFT));
  gh_define(S_focus_right,gh_int2scm(FOCUS_RIGHT));
  gh_define(S_focus_active,gh_int2scm(FOCUS_ACTIVE));
  gh_define(S_focus_middle,gh_int2scm(FOCUS_MIDDLE));

  gh_define(S_x_in_seconds,gh_int2scm(X_IN_SECONDS));
  gh_define(S_x_in_samples,gh_int2scm(X_IN_SAMPLES));
  gh_define(S_x_to_one,gh_int2scm(X_TO_ONE));

  gh_define(S_speed_as_float,gh_int2scm(SPEED_AS_FLOAT));
  gh_define(S_speed_as_ratio,gh_int2scm(SPEED_AS_RATIO));
  gh_define(S_speed_as_semitone,gh_int2scm(SPEED_AS_SEMITONE));

  gh_define(S_channels_separate,gh_int2scm(CHANNELS_SEPARATE));
  gh_define(S_channels_combined,gh_int2scm(CHANNELS_COMBINED));
  gh_define(S_channels_superimposed,gh_int2scm(CHANNELS_SUPERIMPOSED));

  gh_define(S_fourier_transform,gh_int2scm(FOURIER));
  gh_define(S_wavelet_transform,gh_int2scm(WAVELET));
  gh_define(S_hankel_transform,gh_int2scm(HANKEL));
  gh_define(S_chebyshev_transform,gh_int2scm(CHEBYSHEV));
  gh_define(S_cepstrum,gh_int2scm(CEPSTRUM));
  gh_define(S_hadamard_transform,gh_int2scm(HADAMARD));
  gh_define(S_walsh_transform,gh_int2scm(WALSH));
  gh_define(S_autocorrelation,gh_int2scm(AUTOCORRELATION));

  gh_define(S_cursor_in_view,gh_int2scm(CURSOR_IN_VIEW));
  gh_define(S_cursor_on_left,gh_int2scm(CURSOR_ON_LEFT));
  gh_define(S_cursor_on_right,gh_int2scm(CURSOR_ON_RIGHT));
  gh_define(S_cursor_in_middle,gh_int2scm(CURSOR_IN_MIDDLE));
  gh_define(S_cursor_update_display,gh_int2scm(CURSOR_UPDATE_DISPLAY));
  gh_define(S_cursor_no_action,gh_int2scm(CURSOR_NO_ACTION));
  gh_define(S_cursor_claim_selection,gh_int2scm(CURSOR_CLAIM_SELECTION));
  gh_define(S_keyboard_no_action,gh_int2scm(KEYBOARD_NO_ACTION));

  gh_define(S_cursor_cross,gh_int2scm(CURSOR_CROSS));
  gh_define(S_cursor_line,gh_int2scm(CURSOR_LINE));


  /* ---------------- VARIABLES ---------------- */

  DEFINE_PROC(gh_new_procedure0_0(S_ask_before_overwrite,g_ask_before_overwrite),H_ask_before_overwrite);
  DEFINE_PROC(gh_new_procedure0_1(S_set_ask_before_overwrite,g_set_ask_before_overwrite),H_set_ask_before_overwrite);
  DEFINE_PROC(gh_new_procedure0_0(S_audio_output_device,g_audio_output_device),H_audio_output_device);
  DEFINE_PROC(gh_new_procedure1_0(S_set_audio_output_device,g_set_audio_output_device),H_set_audio_output_device);
  DEFINE_PROC(gh_new_procedure0_0(S_dac_size,g_dac_size),H_dac_size);
  DEFINE_PROC(gh_new_procedure1_0(S_set_dac_size,g_set_dac_size),H_set_dac_size);
  DEFINE_PROC(gh_new_procedure0_0(S_dac_folding,g_dac_folding),H_dac_folding);
  DEFINE_PROC(gh_new_procedure1_0(S_set_dac_folding,g_set_dac_folding),H_set_dac_folding);
  DEFINE_PROC(gh_new_procedure0_0(S_auto_resize,g_auto_resize),H_auto_resize);
  DEFINE_PROC(gh_new_procedure0_1(S_set_auto_resize,g_set_auto_resize),H_set_auto_resize);
  DEFINE_PROC(gh_new_procedure0_0(S_auto_update,g_auto_update),H_auto_update);
  DEFINE_PROC(gh_new_procedure0_1(S_set_auto_update,g_set_auto_update),H_set_auto_update);
  DEFINE_PROC(gh_new_procedure0_0(S_graphs_horizontal,g_graphs_horizontal),H_graphs_horizontal);
  DEFINE_PROC(gh_new_procedure0_1(S_set_graphs_horizontal,g_set_graphs_horizontal),H_set_graphs_horizontal);
  DEFINE_PROC(gh_new_procedure0_0(S_channel_style,g_channel_style),H_channel_style);
  DEFINE_PROC(gh_new_procedure1_0(S_set_channel_style,g_set_channel_style),H_set_channel_style);
  DEFINE_PROC(gh_new_procedure0_0(S_color_cutoff,g_color_cutoff),H_color_cutoff);
  DEFINE_PROC(gh_new_procedure1_0(S_set_color_cutoff,g_set_color_cutoff),H_set_color_cutoff);
  DEFINE_PROC(gh_new_procedure0_0(S_color_inverted,g_color_inverted),H_color_inverted);
  DEFINE_PROC(gh_new_procedure0_1(S_set_color_inverted,g_set_color_inverted),H_set_color_inverted);
  DEFINE_PROC(gh_new_procedure0_0(S_color_scale,g_color_scale),H_color_scale);
  DEFINE_PROC(gh_new_procedure1_0(S_set_color_scale,g_set_color_scale),H_set_color_scale);
  DEFINE_PROC(gh_new_procedure0_0(S_corruption_time,g_corruption_time),H_corruption_time);
  DEFINE_PROC(gh_new_procedure1_0(S_set_corruption_time,g_set_corruption_time),H_set_corruption_time);
  DEFINE_PROC(gh_new_procedure0_0(S_default_output_chans,g_default_output_chans),H_default_output_chans);
  DEFINE_PROC(gh_new_procedure1_0(S_set_default_output_chans,g_set_default_output_chans),H_set_default_output_chans);
  DEFINE_PROC(gh_new_procedure0_0(S_default_output_srate,g_default_output_srate),H_default_output_srate);
  DEFINE_PROC(gh_new_procedure1_0(S_set_default_output_srate,g_set_default_output_srate),H_set_default_output_srate);
  DEFINE_PROC(gh_new_procedure0_0(S_default_output_type,g_default_output_type),H_default_output_type);
  DEFINE_PROC(gh_new_procedure1_0(S_set_default_output_type,g_set_default_output_type),H_set_default_output_type);
  DEFINE_PROC(gh_new_procedure0_0(S_default_output_format,g_default_output_format),H_default_output_format);
  DEFINE_PROC(gh_new_procedure1_0(S_set_default_output_format,g_set_default_output_format),H_set_default_output_format);
  DEFINE_PROC(gh_new_procedure0_0(S_dot_size,g_dot_size),H_dot_size);
  DEFINE_PROC(gh_new_procedure1_0(S_set_dot_size,g_set_dot_size),H_set_dot_size);
  DEFINE_PROC(gh_new_procedure0_0(S_enved_base,g_enved_base),H_enved_base);
  DEFINE_PROC(gh_new_procedure1_0(S_set_enved_base,g_set_enved_base),H_set_enved_base);
  DEFINE_PROC(gh_new_procedure0_0(S_enved_power,g_enved_power),H_enved_power);
  DEFINE_PROC(gh_new_procedure1_0(S_set_enved_power,g_set_enved_power),H_set_enved_power);
  DEFINE_PROC(gh_new_procedure0_0(S_enved_clipping,g_enved_clipping),H_enved_clipping);
  DEFINE_PROC(gh_new_procedure0_1(S_set_enved_clipping,g_set_enved_clipping),H_set_enved_clipping);
  DEFINE_PROC(gh_new_procedure0_0(S_enved_exping,g_enved_exping),H_enved_exping);
  DEFINE_PROC(gh_new_procedure0_1(S_set_enved_exping,g_set_enved_exping),H_set_enved_exping);
  DEFINE_PROC(gh_new_procedure0_0(S_enved_target,g_enved_target),H_enved_target);
  DEFINE_PROC(gh_new_procedure1_0(S_set_enved_target,g_set_enved_target),H_set_enved_target);
  DEFINE_PROC(gh_new_procedure0_0(S_enved_waving,g_enved_waving),H_enved_waving);
  DEFINE_PROC(gh_new_procedure0_1(S_set_enved_waving,g_set_enved_waving),H_set_enved_waving);
  DEFINE_PROC(gh_new_procedure0_0(S_enved_dBing,g_enved_dBing),H_enved_dBing);
  DEFINE_PROC(gh_new_procedure0_1(S_set_enved_dBing,g_set_enved_dBing),H_set_enved_dBing);
  DEFINE_PROC(gh_new_procedure0_0(S_eps_file,g_eps_file),H_eps_file);
  DEFINE_PROC(gh_new_procedure1_0(S_set_eps_file,g_set_eps_file),H_set_eps_file);
  DEFINE_PROC(gh_new_procedure0_0(S_listener_prompt,g_listener_prompt),H_listener_prompt);
  DEFINE_PROC(gh_new_procedure1_0(S_set_listener_prompt,g_set_listener_prompt),H_set_listener_prompt);
  DEFINE_PROC(gh_new_procedure0_0(S_audio_state_file,g_audio_state_file),H_audio_state_file);
  DEFINE_PROC(gh_new_procedure1_0(S_set_audio_state_file,g_set_audio_state_file),H_set_audio_state_file);
  DEFINE_PROC(gh_new_procedure0_0(S_fft_beta,g_fft_beta),H_fft_beta);
  DEFINE_PROC(gh_new_procedure1_0(S_set_fft_beta,g_set_fft_beta),H_set_fft_beta);
  DEFINE_PROC(gh_new_procedure0_0(S_fft_log_frequency,g_fft_log_frequency),H_fft_log_frequency);
  DEFINE_PROC(gh_new_procedure0_1(S_set_fft_log_frequency,g_set_fft_log_frequency),H_set_fft_log_frequency);
  DEFINE_PROC(gh_new_procedure0_0(S_fft_log_magnitude,g_fft_log_magnitude),H_fft_log_magnitude);
  DEFINE_PROC(gh_new_procedure0_1(S_set_fft_log_magnitude,g_set_fft_log_magnitude),H_set_fft_log_magnitude);
  DEFINE_PROC(gh_new_procedure0_0(S_fft_size,g_fft_size),H_fft_size);
  DEFINE_PROC(gh_new_procedure1_0(S_set_fft_size,g_set_fft_size),H_set_fft_size);
  DEFINE_PROC(gh_new_procedure0_0(S_fft_style,g_fft_style),H_fft_style);
  DEFINE_PROC(gh_new_procedure1_0(S_set_fft_style,g_set_fft_style),H_set_fft_style);
  DEFINE_PROC(gh_new_procedure0_0(S_fft_window,g_fft_window),H_fft_window);
  DEFINE_PROC(gh_new_procedure1_0(S_set_fft_window,g_set_fft_window),H_set_fft_window);
  DEFINE_PROC(gh_new_procedure0_0(S_filter_env_order,g_filter_env_order),H_filter_env_order);
  DEFINE_PROC(gh_new_procedure1_0(S_set_filter_env_order,g_set_filter_env_order),H_set_filter_env_order);
  DEFINE_PROC(gh_new_procedure0_0(S_fit_data_on_open,g_fit_data_on_open),H_fit_data_on_open);
  DEFINE_PROC(gh_new_procedure0_1(S_set_fit_data_on_open,g_set_fit_data_on_open),H_set_fit_data_on_open);
  DEFINE_PROC(gh_new_procedure0_0(S_graph_style,g_graph_style),H_graph_style);
  DEFINE_PROC(gh_new_procedure1_0(S_set_graph_style,g_set_graph_style),H_set_graph_style);
  DEFINE_PROC(gh_new_procedure0_0(S_initial_x0,g_initial_x0),H_initial_x0);
  DEFINE_PROC(gh_new_procedure1_0(S_set_initial_x0,g_set_initial_x0),H_set_initial_x0);
  DEFINE_PROC(gh_new_procedure0_0(S_initial_x1,g_initial_x1),H_initial_x1);
  DEFINE_PROC(gh_new_procedure1_0(S_set_initial_x1,g_set_initial_x1),H_set_initial_x1);
  DEFINE_PROC(gh_new_procedure0_0(S_initial_y0,g_initial_y0),H_initial_y0);
  DEFINE_PROC(gh_new_procedure1_0(S_set_initial_y0,g_set_initial_y0),H_set_initial_y0);
  DEFINE_PROC(gh_new_procedure0_0(S_initial_y1,g_initial_y1),H_initial_y1);
  DEFINE_PROC(gh_new_procedure1_0(S_set_initial_y1,g_set_initial_y1),H_set_initial_y1);
  DEFINE_PROC(gh_new_procedure0_0(S_line_size,g_line_size),H_line_size);
  DEFINE_PROC(gh_new_procedure1_0(S_set_line_size,g_set_line_size),H_set_line_size);
  DEFINE_PROC(gh_new_procedure0_0(S_mix_console_amp_scaler,g_mix_console_amp_scaler),H_mix_console_amp_scaler);
  DEFINE_PROC(gh_new_procedure1_0(S_set_mix_console_amp_scaler,g_set_mix_console_amp_scaler),H_set_mix_console_amp_scaler);
  DEFINE_PROC(gh_new_procedure0_0(S_mix_console_speed_scaler,g_mix_console_speed_scaler),H_mix_console_speed_scaler);
  DEFINE_PROC(gh_new_procedure1_0(S_set_mix_console_speed_scaler,g_set_mix_console_speed_scaler),H_set_mix_console_speed_scaler);
  DEFINE_PROC(gh_new_procedure0_0(S_movies,g_movies),H_movies);
  DEFINE_PROC(gh_new_procedure0_1(S_set_movies,g_set_movies),H_set_movies);
  DEFINE_PROC(gh_new_procedure0_0(S_normalize_fft,g_normalize_fft),H_normalize_fft);
  DEFINE_PROC(gh_new_procedure0_1(S_set_normalize_fft,g_set_normalize_fft),H_set_normalize_fft);
  DEFINE_PROC(gh_new_procedure0_0(S_normalize_on_open,g_normalize_on_open),H_normalize_on_open);
  DEFINE_PROC(gh_new_procedure0_1(S_set_normalize_on_open,g_set_normalize_on_open),H_set_normalize_on_open);
  DEFINE_PROC(gh_new_procedure0_0(S_prefix_arg,g_prefix_arg),H_prefix_arg);
  DEFINE_PROC(gh_new_procedure1_0(S_set_prefix_arg,g_set_prefix_arg),H_set_prefix_arg);
  DEFINE_PROC(gh_new_procedure0_0(S_print_length,g_print_length),H_print_length);
  DEFINE_PROC(gh_new_procedure1_0(S_set_print_length,g_set_print_length),H_set_print_length);
  DEFINE_PROC(gh_new_procedure0_0(S_previous_files_sort,g_previous_files_sort),H_previous_files_sort);
  DEFINE_PROC(gh_new_procedure1_0(S_set_previous_files_sort,g_set_previous_files_sort),H_set_previous_files_sort);
  DEFINE_PROC(gh_new_procedure0_0(S_raw_chans,g_raw_chans),H_raw_chans);
  DEFINE_PROC(gh_new_procedure1_0(S_set_raw_chans,g_set_raw_chans),H_set_raw_chans);
  DEFINE_PROC(gh_new_procedure0_0(S_raw_format,g_raw_format),H_raw_format);
  DEFINE_PROC(gh_new_procedure1_0(S_set_raw_format,g_set_raw_format),H_set_raw_format);
  DEFINE_PROC(gh_new_procedure0_0(S_raw_srate,g_raw_srate),H_raw_srate);
  DEFINE_PROC(gh_new_procedure1_0(S_set_raw_srate,g_set_raw_srate),H_set_raw_srate);
  DEFINE_PROC(gh_new_procedure0_0(S_recorder_autoload,g_recorder_autoload),H_recorder_autoload);
  DEFINE_PROC(gh_new_procedure0_1(S_set_recorder_autoload,g_set_recorder_autoload),H_set_recorder_autoload);
  DEFINE_PROC(gh_new_procedure0_0(S_recorder_buffer_size,g_recorder_buffer_size),H_recorder_buffer_size);
  DEFINE_PROC(gh_new_procedure1_0(S_set_recorder_buffer_size,g_set_recorder_buffer_size),H_set_recorder_buffer_size);
  DEFINE_PROC(gh_new_procedure0_0(S_recorder_file,g_recorder_file),H_recorder_file);
  DEFINE_PROC(gh_new_procedure1_0(S_set_recorder_file,g_set_recorder_file),H_set_recorder_file);
  DEFINE_PROC(gh_new_procedure0_0(S_recorder_in_format,g_recorder_in_format),H_recorder_in_format);
  DEFINE_PROC(gh_new_procedure1_0(S_set_recorder_in_format,g_set_recorder_in_format),H_set_recorder_in_format);
  DEFINE_PROC(gh_new_procedure0_0(S_recorder_out_chans,g_recorder_out_chans),H_recorder_out_chans);
  DEFINE_PROC(gh_new_procedure1_0(S_set_recorder_out_chans,g_set_recorder_out_chans),H_set_recorder_out_chans);
  DEFINE_PROC(gh_new_procedure0_0(S_recorder_out_format,g_recorder_out_format),H_recorder_out_format);
  DEFINE_PROC(gh_new_procedure1_0(S_set_recorder_out_format,g_set_recorder_out_format),H_set_recorder_out_format);
  DEFINE_PROC(gh_new_procedure0_0(S_recorder_srate,g_recorder_srate),H_recorder_srate);
  DEFINE_PROC(gh_new_procedure1_0(S_set_recorder_srate,g_set_recorder_srate),H_set_recorder_srate);
  DEFINE_PROC(gh_new_procedure0_0(S_recorder_trigger,g_recorder_trigger),H_recorder_trigger);
  DEFINE_PROC(gh_new_procedure1_0(S_set_recorder_trigger,g_set_recorder_trigger),H_set_recorder_trigger);
  DEFINE_PROC(gh_new_procedure0_0(S_recorder_max_duration,g_recorder_max_duration),H_recorder_max_duration);
  DEFINE_PROC(gh_new_procedure1_0(S_set_recorder_max_duration,g_set_recorder_max_duration),H_set_recorder_max_duration);
  DEFINE_PROC(gh_new_procedure0_0(S_reverb_decay,g_reverb_decay),H_reverb_decay);
  DEFINE_PROC(gh_new_procedure1_0(S_set_reverb_decay,g_set_reverb_decay),H_set_reverb_decay);
  DEFINE_PROC(gh_new_procedure0_0(S_save_state_on_exit,g_save_state_on_exit),H_save_state_on_exit);
  DEFINE_PROC(gh_new_procedure0_1(S_set_save_state_on_exit,g_set_save_state_on_exit),H_set_save_state_on_exit);
  DEFINE_PROC(gh_new_procedure0_0(S_save_state_file,g_save_state_file),H_save_state_file);
  DEFINE_PROC(gh_new_procedure1_0(S_set_save_state_file,g_set_save_state_file),H_set_save_state_file);
  DEFINE_PROC(gh_new_procedure0_0(S_show_fft_peaks,g_show_fft_peaks),H_show_fft_peaks);
  DEFINE_PROC(gh_new_procedure0_1(S_set_show_fft_peaks,g_set_show_fft_peaks),H_set_show_fft_peaks);
  DEFINE_PROC(gh_new_procedure0_0(S_show_marks,g_show_marks),H_show_marks);
  DEFINE_PROC(gh_new_procedure0_1(S_set_show_marks,g_set_show_marks),H_set_show_marks);
  DEFINE_PROC(gh_new_procedure0_0(S_show_usage_stats,g_show_usage_stats),H_show_usage_stats);
  DEFINE_PROC(gh_new_procedure0_1(S_set_show_usage_stats,g_set_show_usage_stats),H_set_show_usage_stats);
  DEFINE_PROC(gh_new_procedure0_0(S_show_mix_consoles,g_show_mix_consoles),H_show_mix_consoles);
  DEFINE_PROC(gh_new_procedure0_1(S_set_show_mix_consoles,g_set_show_mix_consoles),H_set_show_mix_consoles);
  DEFINE_PROC(gh_new_procedure0_0(S_show_mix_waveforms,g_show_mix_waveforms),H_show_mix_waveforms);
  DEFINE_PROC(gh_new_procedure0_1(S_set_show_mix_waveforms,g_set_show_mix_waveforms),H_set_show_mix_waveforms);
  DEFINE_PROC(gh_new_procedure0_0(S_show_y_zero,g_show_y_zero),H_show_y_zero);
  DEFINE_PROC(gh_new_procedure0_1(S_set_show_y_zero,g_set_show_y_zero),H_set_show_y_zero);
  DEFINE_PROC(gh_new_procedure0_0(S_show_axes,g_show_axes),H_show_axes);
  DEFINE_PROC(gh_new_procedure0_1(S_set_show_axes,g_set_show_axes),H_set_show_axes);
  DEFINE_PROC(gh_new_procedure0_0(S_sinc_width,g_sinc_width),H_sinc_width);
  DEFINE_PROC(gh_new_procedure1_0(S_set_sinc_width,g_set_sinc_width),H_set_sinc_width);
  DEFINE_PROC(gh_new_procedure0_0(S_colormap,g_color_map),H_colormap);
  DEFINE_PROC(gh_new_procedure1_0(S_set_colormap,g_set_color_map),H_set_colormap);
  DEFINE_PROC(gh_new_procedure0_0(S_spectro_cutoff,g_spectro_cutoff),H_spectro_cutoff);
  DEFINE_PROC(gh_new_procedure1_0(S_set_spectro_cutoff,g_set_spectro_cutoff),H_set_spectro_cutoff);
  DEFINE_PROC(gh_new_procedure0_0(S_spectro_start,g_spectro_start),H_spectro_start);
  DEFINE_PROC(gh_new_procedure1_0(S_set_spectro_start,g_set_spectro_start),H_set_spectro_start);
  DEFINE_PROC(gh_new_procedure0_0(S_spectro_hop,g_spectro_hop),H_spectro_hop);
  DEFINE_PROC(gh_new_procedure1_0(S_set_spectro_hop,g_set_spectro_hop),H_set_spectro_hop);
  DEFINE_PROC(gh_new_procedure0_0(S_spectro_x_angle,g_spectro_x_angle),H_spectro_x_angle);
  DEFINE_PROC(gh_new_procedure1_0(S_set_spectro_x_angle,g_set_spectro_x_angle),H_set_spectro_x_angle);
  DEFINE_PROC(gh_new_procedure0_0(S_spectro_x_scale,g_spectro_x_scale),H_spectro_x_scale);
  DEFINE_PROC(gh_new_procedure1_0(S_set_spectro_x_scale,g_set_spectro_x_scale),H_set_spectro_x_scale);
  DEFINE_PROC(gh_new_procedure0_0(S_spectro_y_angle,g_spectro_y_angle),H_spectro_y_angle);
  DEFINE_PROC(gh_new_procedure1_0(S_set_spectro_y_angle,g_set_spectro_y_angle),H_set_spectro_y_angle);
  DEFINE_PROC(gh_new_procedure0_0(S_spectro_y_scale,g_spectro_y_scale),H_spectro_y_scale);
  DEFINE_PROC(gh_new_procedure1_0(S_set_spectro_y_scale,g_set_spectro_y_scale),H_set_spectro_y_scale);
  DEFINE_PROC(gh_new_procedure0_0(S_spectro_z_angle,g_spectro_z_angle),H_spectro_z_angle);
  DEFINE_PROC(gh_new_procedure1_0(S_set_spectro_z_angle,g_set_spectro_z_angle),H_set_spectro_z_angle);
  DEFINE_PROC(gh_new_procedure0_0(S_spectro_z_scale,g_spectro_z_scale),H_spectro_z_scale);
  DEFINE_PROC(gh_new_procedure1_0(S_set_spectro_z_scale,g_set_spectro_z_scale),H_set_spectro_z_scale);
  DEFINE_PROC(gh_new_procedure0_0(S_speed_style,g_speed_style),H_speed_style);
  DEFINE_PROC(gh_new_procedure1_0(S_set_speed_style,g_set_speed_style),H_set_speed_style);
  DEFINE_PROC(gh_new_procedure0_0(S_speed_tones,g_speed_tones),H_speed_tones);
  DEFINE_PROC(gh_new_procedure1_0(S_set_speed_tones,g_set_speed_tones),H_set_speed_tones);
  DEFINE_PROC(gh_new_procedure0_0(S_temp_dir,g_temp_dir),H_temp_dir);
  DEFINE_PROC(gh_new_procedure1_0(S_set_temp_dir,g_set_temp_dir),H_set_temp_dir);
  DEFINE_PROC(gh_new_procedure0_0(S_save_dir,g_save_dir),H_save_dir);
  DEFINE_PROC(gh_new_procedure1_0(S_set_save_dir,g_set_save_dir),H_set_save_dir);
  DEFINE_PROC(gh_new_procedure0_0(S_transform_type,g_transform_type),H_transform_type);
  DEFINE_PROC(gh_new_procedure1_0(S_set_transform_type,g_set_transform_type),H_set_transform_type);
  DEFINE_PROC(gh_new_procedure0_0(S_trap_segfault,g_trap_segfault),H_trap_segfault);
  DEFINE_PROC(gh_new_procedure0_1(S_set_trap_segfault,g_set_trap_segfault),H_set_trap_segfault);
  DEFINE_PROC(gh_new_procedure0_0(S_show_selection_transform,g_show_selection_transform),H_show_selection_transform);
  DEFINE_PROC(gh_new_procedure0_1(S_set_show_selection_transform,g_set_show_selection_transform),H_set_show_selection_transform);
  DEFINE_PROC(gh_new_procedure0_0(S_with_mix_consoles,g_with_mix_consoles),H_with_mix_consoles);
  DEFINE_PROC(gh_new_procedure0_1(S_set_with_mix_consoles,g_set_with_mix_consoles),H_set_with_mix_consoles);
  DEFINE_PROC(gh_new_procedure0_0(S_use_raw_defaults,g_use_raw_defaults),H_use_raw_defaults);
  DEFINE_PROC(gh_new_procedure0_1(S_set_use_raw_defaults,g_set_use_raw_defaults),H_set_use_raw_defaults);
  DEFINE_PROC(gh_new_procedure0_0(S_use_sinc_interp,g_use_sinc_interp),H_use_sinc_interp);
  DEFINE_PROC(gh_new_procedure0_1(S_set_use_sinc_interp,g_set_use_sinc_interp),H_set_use_sinc_interp);
  DEFINE_PROC(gh_new_procedure0_0(S_data_clipped,g_data_clipped),H_data_clipped);
  DEFINE_PROC(gh_new_procedure0_1(S_set_data_clipped,g_set_data_clipped),H_set_data_clipped);
  DEFINE_PROC(gh_new_procedure0_0(S_verbose_cursor,g_verbose_cursor),H_verbose_cursor);
  DEFINE_PROC(gh_new_procedure0_1(S_set_verbose_cursor,g_set_verbose_cursor),H_set_verbose_cursor);
  DEFINE_PROC(gh_new_procedure0_0(S_vu_font,g_vu_font),H_vu_font);
  DEFINE_PROC(gh_new_procedure1_0(S_set_vu_font,g_set_vu_font),H_set_vu_font);
  DEFINE_PROC(gh_new_procedure0_0(S_vu_font_size,g_vu_font_size),H_vu_font_size);
  DEFINE_PROC(gh_new_procedure1_0(S_set_vu_font_size,g_set_vu_font_size),H_set_vu_font_size);
  DEFINE_PROC(gh_new_procedure0_0(S_vu_size,g_vu_size),H_vu_size);
  DEFINE_PROC(gh_new_procedure1_0(S_set_vu_size,g_set_vu_size),H_set_vu_size);
  DEFINE_PROC(gh_new_procedure0_0(S_wavelet_type,g_wavelet_type),H_wavelet_type);
  DEFINE_PROC(gh_new_procedure1_0(S_set_wavelet_type,g_set_wavelet_type),H_set_wavelet_type);
  DEFINE_PROC(gh_new_procedure0_0(S_wavo,g_wavo),H_wavo);
  DEFINE_PROC(gh_new_procedure0_1(S_set_wavo,g_set_wavo),H_set_wavo);
  DEFINE_PROC(gh_new_procedure0_0(S_wavo_hop,g_wavo_hop),H_wavo_hop);
  DEFINE_PROC(gh_new_procedure1_0(S_set_wavo_hop,g_set_wavo_hop),H_set_wavo_hop);
  DEFINE_PROC(gh_new_procedure0_0(S_wavo_trace,g_wavo_trace),H_wavo_trace);
  DEFINE_PROC(gh_new_procedure1_0(S_set_wavo_trace,g_set_wavo_trace),H_set_wavo_trace);
  DEFINE_PROC(gh_new_procedure0_0(S_window_x,g_window_x),H_window_x);
  DEFINE_PROC(gh_new_procedure1_0(S_set_window_x,g_set_window_x),H_set_window_x);
  DEFINE_PROC(gh_new_procedure0_0(S_window_y,g_window_y),H_window_y);
  DEFINE_PROC(gh_new_procedure1_0(S_set_window_y,g_set_window_y),H_set_window_y);
  DEFINE_PROC(gh_new_procedure0_0(S_x_axis_style,g_x_axis_style),H_x_axis_style);
  DEFINE_PROC(gh_new_procedure1_0(S_set_x_axis_style,g_set_x_axis_style),H_set_x_axis_style);
  DEFINE_PROC(gh_new_procedure0_0(S_xmax,g_xmax),H_xmax);
  DEFINE_PROC(gh_new_procedure1_0(S_set_xmax,g_set_xmax),H_set_xmax);
  DEFINE_PROC(gh_new_procedure0_0(S_xmin,g_xmin),H_xmin);
  DEFINE_PROC(gh_new_procedure1_0(S_set_xmin,g_set_xmin),H_set_xmin);
  DEFINE_PROC(gh_new_procedure0_0(S_ymax,g_ymax),H_ymax);
  DEFINE_PROC(gh_new_procedure1_0(S_set_ymax,g_set_ymax),H_set_ymax);
  DEFINE_PROC(gh_new_procedure0_0(S_ymin,g_ymin),H_ymin);
  DEFINE_PROC(gh_new_procedure1_0(S_set_ymin,g_set_ymin),H_set_ymin);
  DEFINE_PROC(gh_new_procedure0_0(S_min_dB,g_min_dB),H_min_dB);
  DEFINE_PROC(gh_new_procedure1_0(S_set_min_dB,g_set_min_dB),H_set_min_dB);
  DEFINE_PROC(gh_new_procedure0_0(S_zero_pad,g_zero_pad),H_zero_pad);
  DEFINE_PROC(gh_new_procedure1_0(S_set_zero_pad,g_set_zero_pad),H_set_zero_pad);
  DEFINE_PROC(gh_new_procedure0_0(S_zoom_focus_style,g_zoom_focus_style),H_zoom_focus_style);
  DEFINE_PROC(gh_new_procedure1_0(S_set_zoom_focus_style,g_set_zoom_focus_style),H_set_zoom_focus_style);
  DEFINE_PROC(gh_new_procedure0_0(S_help_text_font,g_help_text_font),H_help_text_font);
  DEFINE_PROC(gh_new_procedure1_0(S_set_help_text_font,g_set_help_text_font),H_set_help_text_font);
  DEFINE_PROC(gh_new_procedure0_0(S_tiny_font,g_tiny_font),H_tiny_font);
  DEFINE_PROC(gh_new_procedure1_0(S_set_tiny_font,g_set_tiny_font),H_set_tiny_font);
  DEFINE_PROC(gh_new_procedure0_0(S_button_font,g_button_font),H_button_font);
  DEFINE_PROC(gh_new_procedure1_0(S_set_button_font,g_set_button_font),H_set_button_font);
  DEFINE_PROC(gh_new_procedure0_0(S_bold_button_font,g_bold_button_font),H_bold_button_font);
  DEFINE_PROC(gh_new_procedure1_0(S_set_bold_button_font,g_set_bold_button_font),H_set_bold_button_font);
  DEFINE_PROC(gh_new_procedure0_0(S_axis_label_font,g_axis_label_font),H_axis_label_font);
  DEFINE_PROC(gh_new_procedure1_0(S_set_axis_label_font,g_set_axis_label_font),H_set_axis_label_font);
  DEFINE_PROC(gh_new_procedure0_0(S_axis_numbers_font,g_axis_numbers_font),H_axis_numbers_font);
  DEFINE_PROC(gh_new_procedure1_0(S_set_axis_numbers_font,g_set_axis_numbers_font),H_set_axis_numbers_font);
  DEFINE_PROC(gh_new_procedure0_0(S_listener_font,g_listener_font),H_listener_font);
  DEFINE_PROC(gh_new_procedure1_0(S_set_listener_font,g_set_listener_font),H_set_listener_font);
#if ((USE_MOTIF) && (XmVERSION == 1))
  DEFINE_PROC(gh_new_procedure0_0(S_edit_history_width,g_edit_history_width),H_edit_history_width);
  DEFINE_PROC(gh_new_procedure1_0(S_set_edit_history_width,g_set_edit_history_width),H_set_edit_history_width);
  DEFINE_PROC(gh_new_procedure0_0(S_show_edit_history,g_show_edit_history),H_show_edit_history);
  DEFINE_PROC(gh_new_procedure0_1(S_set_show_edit_history,g_set_show_edit_history),H_set_show_edit_history);
#endif


  /* ---------------- FUNCTIONS ---------------- */

  DEFINE_PROC(gh_new_procedure2_0(S_set_oss_buffers,g_set_oss_buffers),H_set_oss_buffers);
  DEFINE_PROC(gh_new_procedure0_0(S_update_usage_stats,g_update_usage_stats),H_update_usage_stats);
  DEFINE_PROC(gh_new_procedure1_0(S_recorder_gain,g_recorder_gain),H_recorder_gain);
  DEFINE_PROC(gh_new_procedure2_0(S_recorder_in_amp,g_recorder_in_amp),H_recorder_in_amp);
  DEFINE_PROC(gh_new_procedure1_0(S_recorder_out_amp,g_recorder_out_amp),H_recorder_out_amp);
  DEFINE_PROC(gh_new_procedure2_0(S_set_recorder_gain,g_set_recorder_gain),H_set_recorder_gain);
  DEFINE_PROC(gh_new_procedure3_0(S_set_recorder_in_amp,g_set_recorder_in_amp),H_set_recorder_in_amp);
  DEFINE_PROC(gh_new_procedure2_0(S_set_recorder_out_amp,g_set_recorder_out_amp),H_set_recorder_out_amp);
#if HAVE_OSS
  DEFINE_PROC(gh_new_procedure0_0(S_clear_audio_inputs,g_clear_audio_inputs),H_clear_audio_inputs);
  DEFINE_PROC(gh_new_procedure1_0("set-dsp-reset",g_set_dsp_reset),H_set_dsp_reset);
  DEFINE_PROC(gh_new_procedure3_0(S_dsp_devices,g_dsp_devices),H_dsp_devices);
  DEFINE_PROC(gh_new_procedure3_0(S_set_dsp_devices,g_set_dsp_devices),H_set_dsp_devices);
#endif
  DEFINE_PROC(gh_new_procedure0_1(S_set_just_sounds,g_set_just_sounds),H_set_just_sounds);
  DEFINE_PROC(gh_new_procedure0_0(S_enved_dialog,g_enved_dialog),H_enved_dialog);
  DEFINE_PROC(gh_new_procedure0_0(S_color_dialog,g_color_dialog),H_color_dialog);
  DEFINE_PROC(gh_new_procedure0_0(S_orientation_dialog,g_orientation_dialog),H_orientation_dialog);
  DEFINE_PROC(gh_new_procedure0_0(S_transform_dialog,g_transform_dialog),H_transform_dialog);
  DEFINE_PROC(gh_new_procedure0_0(S_file_dialog,g_file_dialog),H_file_dialog);
  DEFINE_PROC(gh_new_procedure0_1(S_edit_header_dialog,g_edit_header_dialog),H_edit_header_dialog);
  DEFINE_PROC(gh_new_procedure0_0(S_recorder_dialog,g_recorder_dialog),H_recorder_dialog);
  DEFINE_PROC(gh_new_procedure2_0(S_help_dialog,g_help_dialog),H_help_dialog);
  DEFINE_PROC(gh_new_procedure1_2(S_sample,g_sample),H_sample);
  DEFINE_PROC(gh_new_procedure2_2(S_set_sample,g_set_sample),H_set_sample);
  DEFINE_PROC(gh_new_procedure2_3(S_samples,g_samples),H_samples);
  DEFINE_PROC(gh_new_procedure3_2(S_set_samples,g_set_samples),H_set_samples);
  DEFINE_PROC(gh_new_procedure3_2(S_vct_samples,g_set_samples),H_set_samples);
  DEFINE_PROC(gh_new_procedure1_2(S_delete_sample,g_delete_sample),H_delete_sample);
  DEFINE_PROC(gh_new_procedure2_2(S_delete_samples,g_delete_samples),H_delete_samples);
  DEFINE_PROC(gh_new_procedure2_2(S_insert_sample,g_insert_sample),H_insert_sample);
  DEFINE_PROC(gh_new_procedure3_2(S_insert_samples,g_insert_samples),H_insert_samples);
  DEFINE_PROC(gh_new_procedure0_2(S_cursor,g_cursor),H_cursor);
  DEFINE_PROC(gh_new_procedure1_2(S_set_cursor,g_set_cursor),H_set_cursor);
  DEFINE_PROC(gh_new_procedure0_2(S_cursor_style,g_cursor_style),H_cursor_style);
  DEFINE_PROC(gh_new_procedure1_2(S_set_cursor_style,g_set_cursor_style),H_set_cursor_style);
  DEFINE_PROC(gh_new_procedure0_2(S_left_sample,g_left_sample),H_left_sample);
  DEFINE_PROC(gh_new_procedure1_2(S_set_left_sample,g_set_left_sample),H_set_left_sample);
  DEFINE_PROC(gh_new_procedure0_2(S_right_sample,g_right_sample),H_right_sample);
  DEFINE_PROC(gh_new_procedure1_2(S_set_right_sample,g_set_right_sample),H_set_right_sample);
  DEFINE_PROC(gh_new_procedure0_1(S_channels,g_channels),H_channels);
  DEFINE_PROC(gh_new_procedure0_1(S_chans,g_channels),H_channels);
  DEFINE_PROC(gh_new_procedure0_1(S_srate,g_srate),H_srate);
  DEFINE_PROC(gh_new_procedure0_1(S_data_location,g_data_location),H_data_location);
  DEFINE_PROC(gh_new_procedure0_1(S_data_format,g_data_format),H_data_format);
  DEFINE_PROC(gh_new_procedure0_1(S_header_type,g_header_type),H_header_type);
  DEFINE_PROC(gh_new_procedure0_1(S_comment,g_comment),H_comment);
  DEFINE_PROC(gh_new_procedure0_2(S_frames,g_frames),H_frames);
  DEFINE_PROC(gh_new_procedure0_2(S_x_position_slider,g_ap_sx),H_x_position_slider);
  DEFINE_PROC(gh_new_procedure0_2(S_y_position_slider,g_ap_sy),H_y_position_slider);
  DEFINE_PROC(gh_new_procedure0_2(S_x_zoom_slider,g_ap_zx),H_x_zoom_slider);
  DEFINE_PROC(gh_new_procedure0_2(S_y_zoom_slider,g_ap_zy),H_y_zoom_slider);
  DEFINE_PROC(gh_new_procedure0_0(S_active_sounds,g_active_sounds),H_active_sounds);
  DEFINE_PROC(gh_new_procedure0_0(S_max_sounds,g_max_sounds),H_max_sounds);
  DEFINE_PROC(gh_new_procedure0_0(S_max_regions,g_max_regions),H_max_regions);
  DEFINE_PROC(gh_new_procedure1_0(S_set_max_regions,g_set_max_regions),H_set_max_regions);
  DEFINE_PROC(gh_new_procedure0_0(S_max_fft_peaks,g_max_fft_peaks),H_max_fft_peaks);
  DEFINE_PROC(gh_new_procedure1_0(S_set_max_fft_peaks,g_set_max_fft_peaks),H_set_max_fft_peaks);
  DEFINE_PROC(gh_new_procedure0_2(S_edit_position,g_edit_position),H_edit_position);
  DEFINE_PROC(gh_new_procedure0_2(S_ffting,g_ffting),H_ffting);
  DEFINE_PROC(gh_new_procedure0_3(S_set_ffting,g_set_ffting),H_set_ffting);
  DEFINE_PROC(gh_new_procedure0_2(S_waving,g_waving),H_waving);
  DEFINE_PROC(gh_new_procedure0_3(S_set_waving,g_set_waving),H_set_waving);
  DEFINE_PROC(gh_new_procedure0_2(S_graphing,g_graphing),H_graphing);
  DEFINE_PROC(gh_new_procedure0_3(S_set_graphing,g_set_graphing),H_set_graphing);
  DEFINE_PROC(gh_new_procedure0_2(S_squelch_update,g_squelch_update),H_squelch_update);
  DEFINE_PROC(gh_new_procedure0_3(S_set_squelch_update,g_set_squelch_update),H_set_squelch_update);
  DEFINE_PROC(gh_new_procedure0_2(S_edits,g_edits),H_edits);
  DEFINE_PROC(gh_new_procedure0_2(S_maxamp,g_maxamp),H_maxamp);
  DEFINE_PROC(gh_new_procedure0_3(S_peaks,g_peaks),H_peaks);
  DEFINE_PROC(gh_new_procedure0_3(S_undo,g_undo),H_undo);
  DEFINE_PROC(gh_new_procedure0_3(S_redo,g_redo),H_redo);
  DEFINE_PROC(gh_new_procedure0_4(S_insert_region,g_insert_region),H_insert_region);
  DEFINE_PROC(gh_new_procedure0_0(S_cut,g_cut),H_cut);
  DEFINE_PROC(gh_new_procedure0_0(S_selected_sound,g_selected_sound),H_selected_sound);
  DEFINE_PROC(gh_new_procedure0_1(S_selected_channel,g_selected_channel),H_selected_channel);
  DEFINE_PROC(gh_new_procedure0_1(S_select_sound,g_select_sound),H_select_sound);
  DEFINE_PROC(gh_new_procedure0_1(S_select_channel,g_select_channel),H_select_channel);
  DEFINE_PROC(gh_new_procedure1_0(S_select_mix,g_select_mix),H_select_mix);
  DEFINE_PROC(gh_new_procedure0_0(S_selected_mix,g_selected_mix),H_selected_mix);
  DEFINE_PROC(gh_new_procedure0_1(S_save_control_panel,g_save_control_panel),H_save_control_panel);
  DEFINE_PROC(gh_new_procedure0_1(S_restore_control_panel,g_restore_control_panel),H_restore_control_panel);
  DEFINE_PROC(gh_new_procedure0_2(S_x_bounds,g_x_bounds),H_x_bounds);
  DEFINE_PROC(gh_new_procedure0_2(S_y_bounds,g_y_bounds),H_y_bounds);
  DEFINE_PROC(gh_new_procedure2_2(S_set_x_bounds,g_set_x_bounds),H_set_x_bounds);
  DEFINE_PROC(gh_new_procedure0_4(S_set_y_bounds,g_set_y_bounds),H_set_y_bounds);
  DEFINE_PROC(gh_new_procedure1_3(S_insert_sound,g_insert_sound),H_insert_sound);
  DEFINE_PROC(gh_new_procedure1_0(S_open_sound,g_open_sound),H_open_sound);
  DEFINE_PROC(gh_new_procedure4_0(S_open_raw_sound,g_open_raw_sound),H_open_raw_sound);
  DEFINE_PROC(gh_new_procedure1_0(S_open_alternate_sound,g_open_alternate_sound),H_open_alternate_sound);
  DEFINE_PROC(gh_new_procedure1_0(S_view_sound,g_view_sound),H_view_sound);
  DEFINE_PROC(gh_new_procedure1_5(S_new_sound,g_new_sound),H_new_sound);
  DEFINE_PROC(gh_new_procedure0_1(S_close_sound,g_close_sound),H_close_sound);
  DEFINE_PROC(gh_new_procedure0_1(S_update_sound,g_update_sound),H_update_sound);
  DEFINE_PROC(gh_new_procedure0_1(S_revert_sound,g_revert_sound),H_revert_sound);
  DEFINE_PROC(gh_new_procedure0_1(S_save_sound,g_save_sound),H_save_sound);
  DEFINE_PROC(gh_new_procedure1_4(S_save_sound_as,g_save_sound_as),H_save_sound_as);
  DEFINE_PROC(gh_new_procedure1_0(S_preload_directory,g_preload_directory),H_preload_directory);
  DEFINE_PROC(gh_new_procedure1_0(S_preload_file,g_preload_file),H_preload_file);
  DEFINE_PROC(gh_new_procedure1_0(S_sound_files_in_directory,g_sound_files_in_directory),H_sound_files_in_directory);
  DEFINE_PROC(gh_new_procedure1_0(S_yes_or_no_p,g_yes_or_no_p),H_yes_or_no_p);
  DEFINE_PROC(gh_new_procedure0_3(S_forward_sample,g_forward_sample),H_forward_sample);
  DEFINE_PROC(gh_new_procedure0_3(S_backward_sample,g_backward_sample),H_backward_sample);
  DEFINE_PROC(gh_new_procedure0_3(S_forward_graph,g_forward_graph),H_forward_graph);
  DEFINE_PROC(gh_new_procedure0_3(S_backward_graph,g_backward_graph),H_backward_graph);
  DEFINE_PROC(gh_new_procedure0_3(S_forward_mark,g_forward_mark),H_forward_mark);
  DEFINE_PROC(gh_new_procedure0_3(S_backward_mark,g_backward_mark),H_backward_mark);
  DEFINE_PROC(gh_new_procedure0_3(S_forward_mix,g_forward_mix),H_forward_mix);
  DEFINE_PROC(gh_new_procedure0_3(S_backward_mix,g_backward_mix),H_backward_mix);
  DEFINE_PROC(gh_new_procedure0_0(S_regions,g_regions),H_regions);
  DEFINE_PROC(gh_new_procedure0_1(S_region_length,g_region_length),H_region_length);
  DEFINE_PROC(gh_new_procedure0_1(S_region_srate,g_region_srate),H_region_srate);
  DEFINE_PROC(gh_new_procedure0_1(S_region_chans,g_region_chans),H_region_chans);
  DEFINE_PROC(gh_new_procedure0_1(S_region_maxamp,g_region_maxamp),H_region_maxamp);
  DEFINE_PROC(gh_new_procedure2_1(S_save_region,g_save_region),H_save_region);
  DEFINE_PROC(gh_new_procedure0_1(S_select_region,g_select_region),H_select_region);
  DEFINE_PROC(gh_new_procedure0_1(S_delete_region,g_delete_region),H_delete_region);
  DEFINE_PROC(gh_new_procedure2_0(S_protect_region,g_protect_region),H_protect_region);
  DEFINE_PROC(gh_new_procedure0_2(S_play_region,g_play_region),H_play_region);
  DEFINE_PROC(gh_new_procedure2_2(S_make_region,g_make_region),H_make_region);
  DEFINE_PROC(gh_new_procedure0_0(S_selection_beg,g_selection_beg),H_selection_beg);
  DEFINE_PROC(gh_new_procedure0_0(S_selection_length,g_selection_length),H_selection_length);
  DEFINE_PROC(gh_new_procedure0_2(S_selection_member,g_selection_member),H_selection_member);
  DEFINE_PROC(gh_new_procedure0_2(S_select_all,g_select_all),H_select_all);
  DEFINE_PROC(gh_new_procedure9_0(S_restore_region,g_restore_region),"restores a region"); /* in snd-region.c */
  DEFINE_PROC(gh_new_procedure0_1(S_scale_selection_to,g_scale_selection_to),H_scale_selection_to);
  DEFINE_PROC(gh_new_procedure0_1(S_scale_selection_by,g_scale_selection_by),H_scale_selection_by);
  DEFINE_PROC(gh_new_procedure0_5(S_mix_region,g_mix_region),H_mix_region);
  DEFINE_PROC(gh_new_procedure0_3(S_region_sample,g_region_sample),H_region_sample);
  DEFINE_PROC(gh_new_procedure0_4(S_region_samples,g_region_samples),H_region_samples);
  DEFINE_PROC(gh_new_procedure0_2(S_update_graph,g_update_graph),H_update_graph);
  DEFINE_PROC(gh_new_procedure0_2(S_update_fft,g_update_fft),H_update_fft);
  DEFINE_PROC(gh_new_procedure0_4(S_play,g_play),H_play);
  DEFINE_PROC(gh_new_procedure0_4(S_play_and_wait,g_play_and_wait),H_play_and_wait);
  DEFINE_PROC(gh_new_procedure0_1(S_stop_playing,g_stop_playing),H_stop_playing);
  DEFINE_PROC(gh_new_procedure0_0(S_exit,g_exit),H_exit);
  DEFINE_PROC(gh_new_procedure0_0(S_abort,g_abort),H_abort);
  DEFINE_PROC(gh_new_procedure0_0(S_dismiss_all_dialogs,g_dismiss_all_dialogs),H_dismiss_all_dialogs);
  DEFINE_PROC(gh_new_procedure0_0(S_abortQ,g_abortq),H_abortQ);
  DEFINE_PROC(gh_new_procedure0_0(S_version,g_version),H_version);
  DEFINE_PROC(gh_new_procedure0_0(S_show_listener,g_show_listener),H_show_listener);
  DEFINE_PROC(gh_new_procedure0_0(S_hide_listener,g_hide_listener),H_hide_listener);
  DEFINE_PROC(gh_new_procedure0_0(S_activate_listener,g_activate_listener),H_activate_listener);
  DEFINE_PROC(gh_new_procedure0_0(S_graph_ps,g_graph2ps),H_graph2ps);
  DEFINE_PROC(gh_new_procedure1_0(S_save_state,g_save_state),H_save_state);
  DEFINE_PROC(gh_new_procedure0_0(S_save_macros,g_save_macros),H_save_macros);
  DEFINE_PROC(gh_new_procedure0_1(S_save_marks,g_save_marks),H_save_marks);
  DEFINE_PROC(gh_new_procedure1_0(S_save_options,g_save_options),H_save_options);
  DEFINE_PROC(gh_new_procedure0_1(S_save_envelopes,g_save_envelopes),H_save_envelopes);
  DEFINE_PROC(gh_new_procedure0_3(S_scale_to,g_scale_to),H_scale_to);
  DEFINE_PROC(gh_new_procedure0_3(S_scale_by,g_scale_by),H_scale_by);
  DEFINE_PROC(gh_new_procedure0_0(S_window_width,g_window_width),H_window_width);
  DEFINE_PROC(gh_new_procedure0_0(S_window_height,g_window_height),H_window_height);
  DEFINE_PROC(gh_new_procedure1_0(S_set_window_width,g_set_window_width),H_set_window_width);
  DEFINE_PROC(gh_new_procedure1_0(S_set_window_height,g_set_window_height),H_set_window_height);
  DEFINE_PROC(gh_new_procedure0_0(S_normalize_view,g_normalize_view),H_normalize_view);
  DEFINE_PROC(gh_new_procedure0_1(S_syncing,g_syncing),H_syncing);
  DEFINE_PROC(gh_new_procedure0_2(S_set_syncing,g_set_syncing),H_set_syncing);
  DEFINE_PROC(gh_new_procedure0_1(S_uniting,g_uniting),H_uniting);
  DEFINE_PROC(gh_new_procedure0_2(S_set_uniting,g_set_uniting),H_set_uniting);
  DEFINE_PROC(gh_new_procedure0_1(S_read_only,g_read_only),H_read_only);
  DEFINE_PROC(gh_new_procedure0_2(S_set_read_only,g_set_read_only),H_set_read_only);
  DEFINE_PROC(gh_new_procedure0_1(S_expanding,g_expanding),H_expanding);
  DEFINE_PROC(gh_new_procedure0_2(S_set_expanding,g_set_expanding),H_set_expanding);
  DEFINE_PROC(gh_new_procedure0_1(S_contrasting,g_contrasting),H_contrasting);
  DEFINE_PROC(gh_new_procedure0_2(S_set_contrasting,g_set_contrasting),H_set_contrasting);
  DEFINE_PROC(gh_new_procedure0_1(S_reverbing,g_reverbing),H_reverbing);
  DEFINE_PROC(gh_new_procedure0_2(S_set_reverbing,g_set_reverbing),H_set_reverbing);
  DEFINE_PROC(gh_new_procedure0_1(S_filtering,g_filtering),H_filtering);
  DEFINE_PROC(gh_new_procedure0_2(S_set_filtering,g_set_filtering),H_set_filtering);
  DEFINE_PROC(gh_new_procedure0_1(S_filter_dBing,g_filter_dBing),H_filter_dBing);
  DEFINE_PROC(gh_new_procedure1_1(S_set_filter_dBing,g_set_filter_dBing),H_set_filter_dBing);
  DEFINE_PROC(gh_new_procedure0_1(S_filter_order,g_filter_order),H_filter_order);
  DEFINE_PROC(gh_new_procedure1_1(S_set_filter_order,g_set_filter_order),H_set_filter_order);
  DEFINE_PROC(gh_new_procedure0_1(S_file_name,g_file_name),H_file_name);
  DEFINE_PROC(gh_new_procedure0_1(S_short_file_name,g_short_file_name),H_short_file_name);
#if FILE_PER_CHAN
  DEFINE_PROC(gh_new_procedure0_1(S_file_names,g_file_names),H_file_names);
  DEFINE_PROC(gh_new_procedure0_1(S_short_file_names,g_short_file_names),H_short_file_names);
#endif
  DEFINE_PROC(gh_new_procedure0_1(S_contrast,g_contrast),H_contrast);
  DEFINE_PROC(gh_new_procedure1_1(S_set_contrast,g_set_contrast),H_set_contrast);
  DEFINE_PROC(gh_new_procedure0_1(S_contrast_amp,g_contrast_amp),H_contrast_amp);
  DEFINE_PROC(gh_new_procedure1_1(S_set_contrast_amp,g_set_contrast_amp),H_set_contrast_amp);
  DEFINE_PROC(gh_new_procedure0_1(S_expand,g_expand),H_expand);
  DEFINE_PROC(gh_new_procedure1_1(S_set_expand,g_set_expand),H_set_expand);
  DEFINE_PROC(gh_new_procedure0_1(S_expand_length,g_expand_length),H_expand_length);
  DEFINE_PROC(gh_new_procedure1_1(S_set_expand_length,g_set_expand_length),H_set_expand_length);
  DEFINE_PROC(gh_new_procedure0_1(S_expand_ramp,g_expand_ramp),H_expand_ramp);
  DEFINE_PROC(gh_new_procedure1_1(S_set_expand_ramp,g_set_expand_ramp),H_set_expand_ramp);
  DEFINE_PROC(gh_new_procedure0_1(S_expand_hop,g_expand_hop),H_expand_hop);
  DEFINE_PROC(gh_new_procedure1_1(S_set_expand_hop,g_set_expand_hop),H_set_expand_hop);
  DEFINE_PROC(gh_new_procedure0_1(S_speed,g_speed),H_speed);
  DEFINE_PROC(gh_new_procedure1_1(S_set_speed,g_set_speed),H_set_speed);
  DEFINE_PROC(gh_new_procedure0_1(S_reverb_length,g_reverb_length),H_reverb_length);
  DEFINE_PROC(gh_new_procedure1_1(S_set_reverb_length,g_set_reverb_length),H_set_reverb_length);
  DEFINE_PROC(gh_new_procedure0_1(S_reverb_scale,g_reverb_scale),H_reverb_scale);
  DEFINE_PROC(gh_new_procedure1_1(S_set_reverb_scale,g_set_reverb_scale),H_set_reverb_scale);
  DEFINE_PROC(gh_new_procedure0_1(S_reverb_feedback,g_reverb_feedback),H_reverb_feedback);
  DEFINE_PROC(gh_new_procedure1_1(S_set_reverb_feedback,g_set_reverb_feedback),H_set_reverb_feedback);
  DEFINE_PROC(gh_new_procedure0_1(S_reverb_lowpass,g_reverb_lowpass),H_reverb_lowpass);
  DEFINE_PROC(gh_new_procedure1_1(S_set_reverb_lowpass,g_set_reverb_lowpass),H_set_reverb_lowpass);
  DEFINE_PROC(gh_new_procedure0_1(S_cursor_follows_play,g_cursor_follows_play),H_cursor_follows_play);
  DEFINE_PROC(gh_new_procedure0_2(S_set_cursor_follows_play,g_set_cursor_follows_play),H_set_cursor_follows_play);
  DEFINE_PROC(gh_new_procedure0_1(S_amp,g_amp),H_amp);
  DEFINE_PROC(gh_new_procedure1_1(S_set_amp,g_set_amp),H_set_amp);
  DEFINE_PROC(gh_new_procedure0_1(S_okQ,g_ok),H_ok);
  DEFINE_PROC(gh_new_procedure1_1(S_report_in_minibuffer,g_report_in_minibuffer),H_report_in_minibuffer);
  DEFINE_PROC(gh_new_procedure1_2(S_prompt_in_minibuffer,g_prompt_in_minibuffer),H_prompt_in_minibuffer);
  DEFINE_PROC(gh_new_procedure1_1(S_append_to_minibuffer,g_append_to_minibuffer),H_append_to_minibuffer);
  DEFINE_PROC(gh_new_procedure0_1(S_showing_controls,g_showing_controls),H_showing_controls);
  DEFINE_PROC(gh_new_procedure0_2(S_set_showing_controls,g_set_showing_controls),H_set_showing_controls);
  DEFINE_PROC(gh_new_procedure0_1(S_filter_env,g_filter_env),H_filter_env);
  DEFINE_PROC(gh_new_procedure1_1(S_set_filter_env,g_set_filter_env),H_set_filter_env);
  DEFINE_PROC(gh_new_procedure2_0(S_set_env_base,g_set_env_base),H_set_env_base);
  DEFINE_PROC(gh_new_procedure1_1(S_override_data_location,g_override_data_location),H_override_data_location);
  DEFINE_PROC(gh_new_procedure1_1(S_override_data_format,g_override_data_format),H_override_data_format);
  DEFINE_PROC(gh_new_procedure1_1(S_override_data_size,g_override_data_size),H_override_data_size);
  DEFINE_PROC(gh_new_procedure0_4(S_open_sound_file,g_open_sound_file),H_open_sound_file);
  DEFINE_PROC(gh_new_procedure2_0(S_close_sound_file,g_close_sound_file),H_close_sound_file);
  DEFINE_PROC(gh_new_procedure3_0(S_vct_sound_file,vct2soundfile),H_vct_sound_file);
  DEFINE_PROC(gh_new_procedure1_5(S_mix_vct,mix_vct),H_mix_vct);
  DEFINE_PROC(gh_new_procedure1_0(S_find_sound,g_find_sound),H_find_sound);
  DEFINE_PROC(gh_new_procedure0_2(S_transform_size,g_transform_size),H_transform_size);
  DEFINE_PROC(gh_new_procedure0_2(S_transform_samples,g_transform_samples),H_transform_samples);
  DEFINE_PROC(gh_new_procedure0_4(S_transform_sample,g_transform_sample),H_transform_sample);
  DEFINE_PROC(gh_new_procedure5_0(S_add_transform,g_add_transform),H_add_transform);
  DEFINE_PROC(gh_new_procedure1_3(S_env_selection,g_env_selection),H_env_selection);
  DEFINE_PROC(gh_new_procedure1_5(S_env_sound,g_env_sound),H_env_sound);
  DEFINE_PROC(gh_new_procedure1_5(S_mix,g_mix),H_mix);
  DEFINE_PROC(gh_new_procedure2_1(S_fft,g_fft),H_fft);
  DEFINE_PROC(gh_new_procedure3_1(S_snd_spectrum,g_snd_spectrum),H_snd_spectrum);
  DEFINE_PROC(gh_new_procedure1_0(S_autocorrelate,g_autocorrelate),H_autocorrelate);
  DEFINE_PROC(gh_new_procedure1_1(S_convolve_arrays,g_convolve),H_convolve);
  DEFINE_PROC(gh_new_procedure1_3(S_convolve_with,g_convolve_with),H_convolve_with);
  DEFINE_PROC(gh_new_procedure1_1(S_convolve_selection_with,g_convolve_selection_with),H_convolve_selection_with);
  DEFINE_PROC(gh_new_procedure0_0(S_reverse_selection,g_reverse_selection),H_reverse_selection);
  DEFINE_PROC(gh_new_procedure1_4(S_save_selection,g_save_selection),H_save_selection);
  DEFINE_PROC(gh_new_procedure0_2(S_reverse_sound,g_reverse_sound),H_reverse_sound);
  DEFINE_PROC(gh_new_procedure2_2(S_smooth,g_smooth),H_smooth);
  DEFINE_PROC(gh_new_procedure0_0(S_smooth_selection,g_smooth_selection),H_smooth_selection);
  DEFINE_PROC(gh_new_procedure1_3(S_src_sound,g_src_sound),H_src_sound);
  DEFINE_PROC(gh_new_procedure1_1(S_src_selection,g_src_selection),H_src_selection);
  DEFINE_PROC(gh_new_procedure2_2(S_filter_sound,g_filter_sound),H_filter_sound);
  DEFINE_PROC(gh_new_procedure2_0(S_filter_selection,g_filter_selection),H_filter_selection);
  DEFINE_PROC(gh_new_procedure0_1(S_call_apply,g_call_apply),H_call_apply);
  DEFINE_PROC(gh_new_procedure1_0(S_temp_filenames,g_temp_filenames),H_temp_filenames);
  DEFINE_PROC(gh_new_procedure0_2(S_sound_to_temp,g_sound_to_temp),H_sound_to_temp);
  DEFINE_PROC(gh_new_procedure0_2(S_sound_to_temps,g_sound_to_temps),H_sound_to_temps);
  DEFINE_PROC(gh_new_procedure0_2(S_selection_to_temp,g_selection_to_temp),H_selection_to_temp);
  DEFINE_PROC(gh_new_procedure0_2(S_selection_to_temps,g_selection_to_temps),H_selection_to_temps);
  DEFINE_PROC(gh_new_procedure3_0(S_temp_to_sound,g_temp_to_sound),H_temp_to_sound);
  DEFINE_PROC(gh_new_procedure3_0(S_temps_to_sound,g_temps_to_sound),H_temps_to_sound);
  DEFINE_PROC(gh_new_procedure3_0(S_temp_to_selection,g_temp_to_sound),H_temp_to_sound);
  DEFINE_PROC(gh_new_procedure3_0(S_temps_to_selection,g_temps_to_sound),H_temps_to_sound);
  DEFINE_PROC(gh_new_procedure2_2(S_key,g_key),H_key);
  DEFINE_PROC(gh_new_procedure1_3(S_find,g_find),H_find);
  DEFINE_PROC(gh_new_procedure1_3(S_count_matches,g_count_matches),H_count_matches);
  DEFINE_PROC(gh_new_procedure1_0(S_add_to_main_menu,g_add_to_main_menu),H_add_to_main_menu);
  DEFINE_PROC(gh_new_procedure3_0(S_add_to_menu,g_add_to_menu),H_add_to_menu);
  DEFINE_PROC(gh_new_procedure2_0(S_remove_from_menu,g_remove_from_menu),H_remove_from_menu);
  DEFINE_PROC(gh_new_procedure3_0(S_change_menu_label,g_change_menu_label),H_change_menu_label);
  DEFINE_PROC(gh_new_procedure3_0(S_set_menu_sensitive,g_set_menu_sensitive),H_set_menu_sensitive);
  DEFINE_PROC(gh_new_procedure2_0(S_define_envelope,g_define_envelope),H_define_envelope);
  DEFINE_PROC(gh_new_procedure1_2(S_save_edit_history,g_save_edit_history),H_save_edit_history);
  DEFINE_PROC(gh_new_procedure1_7(S_graph,g_graph),H_graph);
  DEFINE_PROC(gh_new_procedure3_1(S_bind_key,g_bind_key),H_bind_key);
  DEFINE_PROC(gh_new_procedure1_0(S_add_sound_file_extension,g_add_sound_file_extension),H_add_sound_file_extension);
  DEFINE_PROC(gh_new_procedure1_0(S_string_length,g_string_length),H_string_length);
  DEFINE_PROC(gh_new_procedure0_6(S_samples_vct,samples2vct),H_samples2vct);
  DEFINE_PROC(gh_new_procedure0_7(S_samples2sound_data,samples2sound_data),H_samples2sound_data);
  DEFINE_PROC(gh_new_procedure0_3(S_transform_samples_vct,transform_samples2vct),H_transform_samples2vct);
  DEFINE_PROC(gh_new_procedure0_5(S_region_samples_vct,region_samples2vct),H_region_samples2vct);
  DEFINE_PROC(gh_new_procedure0_1(S_start_progress_report,g_start_progress_report),H_start_progress_report);
  DEFINE_PROC(gh_new_procedure0_1(S_finish_progress_report,g_finish_progress_report),H_finish_progress_report);
  DEFINE_PROC(gh_new_procedure1_4(S_progress_report,g_progress_report),H_progress_report);
  DEFINE_PROC(gh_new_procedure1_4(S_scan_chan,g_scan_chan),H_scan_chan);
  DEFINE_PROC(gh_new_procedure1_2(S_scan_chans,g_scan_chans),H_scan_chans);
  DEFINE_PROC(gh_new_procedure1_2(S_scan_all_chans,g_scan_all_chans),H_scan_all_chans);
  DEFINE_PROC(gh_new_procedure1_3(S_scan_sound_chans,g_scan_sound_chans),H_scan_sound_chans);
  DEFINE_PROC(gh_new_procedure1_2(S_scan_across_chans,g_scan_across_chans),H_scan_across_chans);
  DEFINE_PROC(gh_new_procedure1_2(S_scan_across_all_chans,g_scan_across_all_chans),H_scan_across_all_chans);
  DEFINE_PROC(gh_new_procedure1_3(S_scan_across_sound_chans,g_scan_across_sound_chans),H_scan_across_sound_chans);
  DEFINE_PROC(gh_new_procedure1_5(S_map_chan,g_map_chan),H_map_chan);
  DEFINE_PROC(gh_new_procedure1_3(S_map_chans,g_map_chans),H_map_chans);
  DEFINE_PROC(gh_new_procedure1_3(S_map_all_chans,g_map_all_chans),H_map_all_chans);
  DEFINE_PROC(gh_new_procedure1_4(S_map_sound_chans,g_map_sound_chans),H_map_sound_chans);
  DEFINE_PROC(gh_new_procedure1_3(S_map_across_chans,g_map_across_chans),H_map_across_chans);
  DEFINE_PROC(gh_new_procedure1_3(S_map_across_all_chans,g_map_across_all_chans),H_map_across_all_chans);
  DEFINE_PROC(gh_new_procedure1_4(S_map_across_sound_chans,g_map_across_sound_chans),H_map_across_sound_chans);
  DEFINE_PROC(gh_new_procedure1_0(S_snd_print,g_snd_print),H_snd_print);
  DEFINE_PROC(gh_new_procedure1_0(S_snd_error,g_snd_error),H_snd_error);
  DEFINE_PROC(gh_new_procedure1_0(S_snd_warning,g_snd_warning),H_snd_warning);
  DEFINE_PROC(gh_new_procedure0_0(S_describe_audio,g_describe_audio),H_describe_audio);
  DEFINE_PROC(gh_new_procedure0_0("mus-audio-describe",g_describe_audio),H_describe_audio);
  DEFINE_PROC(gh_new_procedure0_2(S_bomb,g_bomb),H_bomb);
  DEFINE_PROC(gh_new_procedure1_0(S_as_one_edit,g_as_one_edit),H_as_one_edit);
  DEFINE_PROC(gh_new_procedure1_0("apropos",g_apropos),H_apropos);
  DEFINE_PROC(gh_new_procedure2_3(S_set_sound_loop_info,g_set_sound_loop_info),H_set_sound_loop_info);
  DEFINE_PROC(gh_new_procedure4_0(S_loop_samples,g_loop_samples),H_loop_samples);
  DEFINE_PROC(gh_new_procedure0_3(S_edit_fragment,g_edit_fragment),H_edit_fragment);
  DEFINE_PROC(gh_new_procedure0_1(S_soundfont_info,g_soundfont_info),H_soundfont_info);

  DEFINE_PROC(gh_new_procedure0_2(S_edit_hook,g_edit_hook),H_edit_hook);
  DEFINE_PROC(gh_new_procedure0_2(S_undo_hook,g_undo_hook),H_undo_hook);

  /* semi-internal functions (restore-state) */
  gh_new_procedure4_2(S_change_samples_with_origin,g_change_samples_with_origin);
  gh_new_procedure3_2(S_delete_samples_with_origin,g_delete_samples_with_origin);
  gh_new_procedure4_2(S_insert_samples_with_origin,g_insert_samples_with_origin);


  /* ---------------- SAMPLE READERS ---------------- */
#if (!HAVE_GUILE_1_3_0)
  /* sf_tag = scm_make_smob_type_mfpe("sf",sizeof(SCM),mark_sf,free_sf,print_sf,equalp_sf); */
  sf_tag = scm_make_smob_type("sf",sizeof(SCM));
  scm_set_smob_mark(sf_tag,mark_sf);
  scm_set_smob_print(sf_tag,print_sf);
  scm_set_smob_free(sf_tag,free_sf);
  scm_set_smob_equalp(sf_tag,equalp_sf);
#else
  sf_tag = scm_newsmob(&sf_smobfuns);
#endif
  DEFINE_PROC(gh_new_procedure0_5(S_make_sample_reader,g_make_sample_reader),H_make_sample_reader);
  DEFINE_PROC(gh_new_procedure0_4(S_make_region_sample_reader,g_make_region_sample_reader),H_make_region_sample_reader);
  DEFINE_PROC(gh_new_procedure1_0(S_next_sample,g_next_sample),H_next_sample);
  DEFINE_PROC(gh_new_procedure1_0(S_previous_sample,g_previous_sample),H_previous_sample);
  DEFINE_PROC(gh_new_procedure1_0(S_free_sample_reader,g_free_sample_reader),H_free_sample_reader);
  DEFINE_PROC(gh_new_procedure1_0(S_sample_readerQ,g_sf_p),H_sf_p);
  DEFINE_PROC(gh_new_procedure1_0(S_sample_reader_at_endQ,g_sample_reader_at_end),H_sample_reader_at_end);

#if (!HAVE_GUILE_1_3_0)
  /* mf_tag = scm_make_smob_type_mfpe("mf",sizeof(SCM),mark_mf,free_mf,print_mf,equalp_mf); */
  mf_tag = scm_make_smob_type("mf",sizeof(SCM));
  scm_set_smob_mark(mf_tag,mark_mf);
  scm_set_smob_print(mf_tag,print_mf);
  scm_set_smob_free(mf_tag,free_mf);
  scm_set_smob_equalp(mf_tag,equalp_mf);
#else
  mf_tag = scm_newsmob(&mf_smobfuns);
#endif
  DEFINE_PROC(gh_new_procedure1_0(S_make_mix_sample_reader,g_make_mix_sample_reader),H_make_mix_sample_reader);
  DEFINE_PROC(gh_new_procedure1_0(S_next_mix_sample,g_next_mix_sample),H_next_mix_sample);
  DEFINE_PROC(gh_new_procedure1_0(S_free_mix_sample_reader,g_free_mix_sample_reader),H_free_mix_sample_reader);
  DEFINE_PROC(gh_new_procedure1_0(S_mix_sample_readerQ,g_mf_p),H_mf_p);

#if (!HAVE_GUILE_1_3_0)
  /* tf_tag = scm_make_smob_type_mfpe("tf",sizeof(SCM),mark_tf,free_tf,print_tf,equalp_tf); */
  tf_tag = scm_make_smob_type("tf",sizeof(SCM));
  scm_set_smob_mark(tf_tag,mark_tf);
  scm_set_smob_print(tf_tag,print_tf);
  scm_set_smob_free(tf_tag,free_tf);
  scm_set_smob_equalp(tf_tag,equalp_tf);
#else
  tf_tag = scm_newsmob(&tf_smobfuns);
#endif
  DEFINE_PROC(gh_new_procedure1_3(S_make_track_sample_reader,g_make_track_sample_reader),H_make_track_sample_reader);
  DEFINE_PROC(gh_new_procedure1_0(S_next_track_sample,g_next_track_sample),H_next_track_sample);
  DEFINE_PROC(gh_new_procedure1_0(S_free_track_sample_reader,g_free_track_sample_reader),H_free_track_sample_reader);
  DEFINE_PROC(gh_new_procedure1_0(S_track_sample_readerQ,g_tf_p),H_tf_p);
  DEFINE_PROC(gh_new_procedure0_1(S_play_mix,g_play_mix),H_play_mix);
  DEFINE_PROC(gh_new_procedure1_2(S_play_track,g_play_track),H_play_track);

  /* ---------------- HOOKS ---------------- */
#if (!HAVE_GUILE_1_3_0)
  open_hook = scm_create_hook(S_open_hook,1);                     /* arg = filename */
  /* I think this is the actual hook object, not the "vcell" so it might make sense to set its documentation property */
  /*   or is this a per-hook function thing? -- then the help function could run through the hook list displaying docs? */
  during_open_hook = scm_create_hook(S_during_open_hook,3);       /* args = fd filename reason */
  after_open_hook = scm_create_hook(S_after_open_hook,1);         /* args = sound */
  close_hook = scm_create_hook(S_close_hook,1);                   /* arg = sound index */
  fft_hook = scm_create_hook(S_fft_hook,3);                       /* args = sound channel scaler */
  graph_hook = scm_create_hook(S_graph_hook,4);                   /* args = sound channel y0 y1 */
  after_graph_hook = scm_create_hook(S_after_graph_hook,2);       /* args = sound channel */
  exit_hook = scm_create_hook(S_exit_hook,0);
  start_hook = scm_create_hook(S_start_hook,1);                   /* arg = argv filename if any */
  mouse_press_hook = scm_create_hook(S_mouse_press_hook,6);       /* args = sound channel button state x y */
  mouse_release_hook = scm_create_hook(S_mouse_release_hook,6);   /* args = sound channel button state x y */
  mouse_drag_hook = scm_create_hook(S_mouse_drag_hook,6);         /* args = sound channel button state x y */
  key_press_hook = scm_create_hook(S_key_press_hook,4);           /* args = sound channel key state */
  stop_playing_hook = scm_create_hook(S_stop_playing_hook,1);     /* arg = sound */
  /* TODO: this is wrong -- it should carry along the channel argument, if any, as well */
  /*       but that means the snd-dac needs to remember what it got as well */
  stop_playing_region_hook = scm_create_hook(S_stop_playing_region_hook,1);     /* arg = region number */
  start_playing_hook = scm_create_hook(S_start_playing_hook,1);   /* arg = sound */
  mark_click_hook = scm_create_hook(S_mark_click_hook,1);         /* arg = id */
  output_comment_hook = scm_create_hook(S_output_comment_hook,1); /* arg = current mus_sound_comment(hdr) if any */
  output_name_hook = scm_create_hook(S_output_name_hook,0);
  mix_console_state_changed_hook = scm_create_hook(S_mix_console_state_changed_hook,1);
  mix_speed_changed_hook = scm_create_hook(S_mix_speed_changed_hook,1);
  mix_amp_changed_hook = scm_create_hook(S_mix_amp_changed_hook,1);
  mix_position_changed_hook = scm_create_hook(S_mix_position_changed_hook,2);
  multichannel_mix_hook = scm_create_hook(S_multichannel_mix_hook,1);
  mus_error_hook = scm_create_hook(S_mus_error_hook,2);           /* arg = error-type error-message */
  snd_error_hook = scm_create_hook(S_snd_error_hook,1);           /* arg = error-message */
  snd_warning_hook = scm_create_hook(S_snd_warning_hook,1);       /* arg = error-message */
  #if FILE_PER_CHAN
    open_multifile_sound_hook = scm_create_hook(S_open_multifile_sound_hook,1);       /* arg = filename */
    save_multifile_sound_hook = scm_create_hook(S_save_multifile_sound_hook,2);       /* args = snd chn */
  #endif
#else
  open_hook = gh_define(S_open_hook,SCM_BOOL_F);
  during_open_hook = gh_define(S_during_open_hook,SCM_BOOL_F);
  after_open_hook = gh_define(S_after_open_hook,SCM_BOOL_F);
  close_hook = gh_define(S_close_hook,SCM_BOOL_F);
  fft_hook = gh_define(S_fft_hook,SCM_BOOL_F);
  graph_hook = gh_define(S_graph_hook,SCM_BOOL_F);
  after_graph_hook = gh_define(S_after_graph_hook,SCM_BOOL_F);
  exit_hook = gh_define(S_exit_hook,SCM_BOOL_F);
  start_hook = gh_define(S_start_hook,SCM_BOOL_F);
  mouse_press_hook = gh_define(S_mouse_press_hook,SCM_BOOL_F);
  mouse_release_hook = gh_define(S_mouse_release_hook,SCM_BOOL_F);
  mouse_drag_hook = gh_define(S_mouse_drag_hook,SCM_BOOL_F);
  key_press_hook = gh_define(S_key_press_hook,SCM_BOOL_F);
  stop_playing_hook = gh_define(S_stop_playing_hook,SCM_BOOL_F);
  stop_playing_region_hook = gh_define(S_stop_playing_region_hook,SCM_BOOL_F);
  start_playing_hook = gh_define(S_start_playing_hook,SCM_BOOL_F);
  mark_click_hook = gh_define(S_mark_click_hook,SCM_BOOL_F);
  output_comment_hook = gh_define(S_output_comment_hook,SCM_BOOL_F);
  output_name_hook = gh_define(S_output_name_hook,SCM_BOOL_F);
  mix_console_state_changed_hook = gh_define(S_mix_console_state_changed_hook,SCM_BOOL_F);
  mix_speed_changed_hook = gh_define(S_mix_speed_changed_hook,SCM_BOOL_F);
  mix_amp_changed_hook = gh_define(S_mix_amp_changed_hook,SCM_BOOL_F);
  mix_position_changed_hook = gh_define(S_mix_position_changed_hook,SCM_BOOL_F);
  multichannel_mix_hook = gh_define(S_multichannel_mix_hook,SCM_BOOL_F);
  mus_error_hook = gh_define(S_mus_error_hook,SCM_BOOL_F);
  snd_error_hook = gh_define(S_snd_error_hook,SCM_BOOL_F);
  snd_warning_hook = gh_define(S_snd_warning_hook,SCM_BOOL_F);
  #if FILE_PER_CHAN
    open_multifile_sound_hook = gh_define(S_open_multifile_sound_hook,SCM_BOOL_F);
    save_multifile_sound_hook = gh_define(S_save_multifile_sound_hook,SCM_BOOL_F);
  #endif
#endif

  g_init_marks(local_doc);
  g_init_dac(local_doc);
  init_vct();
  init_mus2scm_module();
  g_initialize_xgh(state,local_doc);
  g_init_gxutils();
  g_init_mix(local_doc);

#if HAVE_LADSPA
  g_ladspa_to_snd(local_doc);
#endif

  memo_sound = gh_define(S_memo_sound,SCM_BOOL_F);
  snd_test = gh_define("snd-test",gh_int2scm(-1));
  full_test = gh_define("full-test",SCM_BOOL_T);

  gh_eval_str("(define unbind-key\
                 (lambda (key state)\
                   \"(unbind-key key state) undoes the effect of a prior bind-key call\"\
                   (bind-key key state #f)))");

  gh_eval_str("(defmacro defvar (a b)\
                 `(begin\
                    (define ,a ,b)\
                    (define-envelope (symbol->string ',a) ,b)))");
  /* this is trying to keep track of envelopes for the envelope editor */

  gh_eval_str("(define help\
                 (lambda n\
                   \"help returns the procedure documentation associated with its argument.\n\
(help make-vct) for example, prints out a brief description of make-vct.\n\
In the help descriptions, '&optional' marks optional arguments, and\n\
'&opt-key' marks CLM-style optional keyword arguments.  If you load index.scm\n\
the functions html and ? can be used in place of help to go to the HTML description\"\
                   (if (null? n)\
                       (help help)\
                     (let ((func (car n)))\
                       (if (procedure? func)\
                           (or (procedure-property func 'documentation)\
                               (procedure-documentation func))\
                           (object-property func 'documentation))))))");
  /* TODO: should we append apropos results here, or a cf list? */

#if DEBUGGING
  DEFINE_PROC(gh_new_procedure0_2("display-edits",g_display_edits),H_display_edits);
  #if HAVE_GTK
    scm_add_feature("gtk");
  #endif
#endif

  scm_add_feature("snd");
 
  /* BACKWARDS COMPATIBILTY (the -int- names confuse me) */
  gh_eval_str("(define delete-int-samples delete-samples-with-origin)");
  gh_eval_str("(define insert-int-samples insert-samples-with-origin)");
  gh_eval_str("(define set-int-samples change-samples-with-origin)");
}

#if (!HAVE_GUILE_1_3_0)

/* #define HOOKED(a) SCM_FALSEP(scm_hook_empty_p(a)) */
#define HOOKED(a) (!(SCM_NULLP(SCM_HOOK_PROCEDURES(a))))

static SCM g_c_run_progn_hook (SCM hook, SCM args)
{
  /* Guile built-in scm_c_run_hook doesn't return the value of the hook procedure(s) and exits on error */
  SCM result = SCM_BOOL_F;
  SCM procs = SCM_HOOK_PROCEDURES (hook);
  while (SCM_NIMP (procs))
    {
      if (args != SCM_LIST0)
	result = g_call_any(SCM_CAR(procs),args);
      else result = g_call0(SCM_CAR(procs));
      procs = SCM_CDR (procs);
    }
  return(scm_return_first(result,args));
}

static SCM g_c_run_or_hook (SCM hook, SCM args)
{
  /* Guile built-in scm_c_run_hook doesn't return the value of the hook procedure(s) and calls everything on the list */
  SCM result = SCM_BOOL_F;
  SCM procs = SCM_HOOK_PROCEDURES (hook);
  while (SCM_NIMP (procs))
    {
      if (args != SCM_LIST0)
	result = g_call_any(SCM_CAR(procs),args);
      else result = g_call0(SCM_CAR(procs));
      if (SCM_NFALSEP(result)) return(result);
      procs = SCM_CDR (procs);
    }
  return(scm_return_first(result,args));
}

void during_open(int fd, char *file, int reason)
{
  if (HOOKED(during_open_hook))
    g_c_run_progn_hook(during_open_hook,SCM_LIST3(gh_int2scm(fd),gh_str02scm(file),gh_int2scm(reason)));
}

void after_open(int index)
{
  if (HOOKED(after_open_hook))
    g_c_run_progn_hook(after_open_hook,SCM_LIST1(gh_int2scm(index)));
}

void after_graph(snd_state *ss, chan_info *cp)
{
  if (HOOKED(after_graph_hook))
    g_c_run_progn_hook(after_graph_hook,SCM_LIST2(gh_int2scm((cp->sound)->index),gh_int2scm(cp->chan)));
  /* (add-hook! after-graph-hook (lambda (a b) (snd-print (format #f "~A ~A" a b)))) */
}

int ignore_mus_error(int type, char *msg)
{
  SCM result = SCM_BOOL_F;
  if (HOOKED(mus_error_hook))
    result = g_c_run_or_hook(mus_error_hook,SCM_LIST2(gh_int2scm(type),gh_str02scm(msg)));
  return(SCM_NFALSEP(result));
}

int ignore_snd_error(char *msg)
{
  SCM result = SCM_BOOL_F;
  if (HOOKED(snd_error_hook))
    result = g_c_run_or_hook(snd_error_hook,SCM_LIST1(gh_str02scm(msg)));
  return(SCM_NFALSEP(result));
}

int ignore_snd_warning(char *msg)
{
  SCM result = SCM_BOOL_F;
  if (HOOKED(snd_warning_hook))
    result = g_c_run_or_hook(snd_warning_hook,SCM_LIST1(gh_str02scm(msg)));
  return(SCM_NFALSEP(result));
}

int dont_open(snd_state *ss, char *file)
{
  char *mcf = NULL;
  SCM res = SCM_BOOL_F;
  if (!(ss->open_hook_active))
    {
      if (HOOKED(open_hook))
	{
	  ss->open_hook_active = 1;
	  res = g_c_run_or_hook(open_hook,SCM_LIST1(gh_str02scm(mcf = mus_file_full_name(file))));
	  if (mcf) FREE(mcf);
	  ss->open_hook_active = 0;
	}
    }
  return(SCM_TRUE_P(res));
}

int dont_close(snd_state *ss, snd_info *sp)
{
  SCM res = SCM_BOOL_F;
  if (!(ss->close_hook_active))
    {
      if (HOOKED(close_hook))
	{
	  ss->close_hook_active = 1;
	  res = g_c_run_or_hook(close_hook,SCM_LIST1(gh_int2scm(sp->index)));
	  ss->close_hook_active = 0;
	}
    }
  return(SCM_TRUE_P(res));
}

int after_fft(snd_state *ss, chan_info *cp, Float scaler)
{
  SCM res = SCM_BOOL_F;
  if (!(ss->fft_hook_active))
    {
      if (HOOKED(fft_hook))
	{
	  ss->fft_hook_active = 1;
	  res = g_c_run_progn_hook(fft_hook,
				   SCM_LIST3(gh_int2scm((cp->sound)->index),
					     gh_int2scm(cp->chan),
					     gh_double2scm(scaler)));
	  ss->fft_hook_active = 0;
	}
    }
  return(SCM_TRUE_P(res));
}

int dont_graph(snd_state *ss, chan_info *cp)
{
  SCM res = SCM_BOOL_F;
  if (!(ss->graph_hook_active))
    {
      if (HOOKED(graph_hook))
	{
	  ss->graph_hook_active = 1;
	  res = g_c_run_progn_hook(graph_hook,
				   SCM_LIST4(gh_int2scm((cp->sound)->index),
					     gh_int2scm(cp->chan),
					     gh_double2scm((cp->axis)->y0),
					     gh_double2scm((cp->axis)->y1)));
	  /* (add-hook! graph-hook (lambda (a b c d) (snd-print (format #f "~A ~A ~A ~A" a b c d)))) */
	  ss->graph_hook_active = 0;
	}
    }
  return(SCM_TRUE_P(res));
}

int dont_exit(snd_state *ss)
{
  SCM res = SCM_BOOL_F;
  if (!(ss->exit_hook_active))
    {
      if (HOOKED(exit_hook))
	{
	  ss->exit_hook_active = 1;
	  res = g_c_run_or_hook(exit_hook,SCM_LIST0);
	  ss->exit_hook_active = 0;
	}
    }
  return(SCM_TRUE_P(res));
}
  
int dont_start(snd_state *ss, char *filename)
{
  SCM res = SCM_BOOL_F;
  if (!(ss->start_hook_active))
    {
      if (HOOKED(start_hook))
	{
	  ss->start_hook_active = 1;
	  res = g_c_run_or_hook(start_hook,SCM_LIST1(gh_str02scm(filename)));
	  ss->start_hook_active = 0;
	}
    }
  return(SCM_TRUE_P(res));
}

int handle_mark_click(snd_state *ss, int id)
{
  SCM res = SCM_BOOL_F;
  if (HOOKED(mark_click_hook))
    res = g_c_run_progn_hook(mark_click_hook,SCM_LIST1(gh_int2scm(id)));
  return(SCM_TRUE_P(res));
}
  
/* mouse/key events within lisp graph */

static void handle_mouse_event(SCM hook, snd_state *ss, snd_info *sp, chan_info *cp, Float x, Float y, int button, int state)
{
  if (HOOKED(hook))
    g_c_run_progn_hook(hook,
		       SCM_LIST6(gh_int2scm(sp->index),
				 gh_int2scm(cp->chan),
				 gh_int2scm(button),
				 gh_int2scm(state),
				 gh_double2scm(x),
				 gh_double2scm(y)));
}

void handle_mouse_release(snd_state *ss, snd_info *sp, chan_info *cp, Float x, Float y, int button, int state)
{
  handle_mouse_event(mouse_release_hook,ss,sp,cp,x,y,button,state);
}

void handle_mouse_press(snd_state *ss, snd_info *sp, chan_info *cp, Float x, Float y, int button, int state)
{
  handle_mouse_event(mouse_press_hook,ss,sp,cp,x,y,button,state);
}

void handle_mouse_drag(snd_state *ss, snd_info *sp, chan_info *cp, Float x, Float y)
{
  handle_mouse_event(mouse_drag_hook,ss,sp,cp,x,y,-1,-1);
}

int handle_key_press(snd_state *ss, snd_info *sp, chan_info *cp, int key, int state)
{
  /* return TRUE to keep this key press from being passed to keyboard_command */
  SCM res = SCM_BOOL_F;
  if (HOOKED(key_press_hook))
    res = g_c_run_or_hook(key_press_hook,
			  SCM_LIST4(gh_int2scm((cp->sound)->index),
				    gh_int2scm(cp->chan),
				    gh_int2scm(key),
				    gh_int2scm(state)));
  return(SCM_TRUE_P(res));
}

void call_stop_playing_hook(snd_info *sp)
{
  if (HOOKED(stop_playing_hook))
    g_c_run_or_hook(stop_playing_hook,SCM_LIST1(gh_int2scm(sp->index)));
}

void call_stop_playing_region_hook(int n)
{
  if (HOOKED(stop_playing_region_hook))
    g_c_run_or_hook(stop_playing_region_hook,SCM_LIST1(gh_int2scm(n)));
}

int call_start_playing_hook(snd_info *sp)
{
  SCM stop = SCM_BOOL_F;
  if (HOOKED(start_playing_hook))
    stop = g_c_run_or_hook(start_playing_hook,SCM_LIST1(gh_int2scm(sp->index)));
  return(SCM_TRUE_P(stop));
}

void call_mix_console_state_changed_hook(mixdata *md) 
{
  if ((md) && (HOOKED(mix_console_state_changed_hook)))
    g_c_run_progn_hook(mix_console_state_changed_hook,SCM_LIST1(gh_int2scm(md->id)));
}

int call_mix_speed_changed_hook(mixdata *md)
{  
  SCM res = SCM_BOOL_F;
  if ((md) && (HOOKED(mix_speed_changed_hook)))
    res = g_c_run_progn_hook(mix_speed_changed_hook,SCM_LIST1(gh_int2scm(md->id)));
  return(SCM_TRUE_P(res));
}

int call_mix_amp_changed_hook(mixdata *md)
{  
  SCM res = SCM_BOOL_F;
  if ((md) && (HOOKED(mix_amp_changed_hook)))
    res = g_c_run_progn_hook(mix_amp_changed_hook,SCM_LIST1(gh_int2scm(md->id)));
  return(SCM_TRUE_P(res));
}

int call_mix_position_changed_hook(mixdata *md, int samps)
{  
  SCM res = SCM_BOOL_F;
  if ((md) && (HOOKED(mix_position_changed_hook)))
    res = g_c_run_progn_hook(mix_position_changed_hook,SCM_LIST2(gh_int2scm(md->id),gh_int2scm(samps)));
  return(SCM_TRUE_P(res));
}

void call_multichannel_mix_hook(snd_state *ss, int *ids, int n)
{
  SCM lst = SCM_EOL;
  int i;
  /* create list from ids, pass to hook, if any */
  if (HOOKED(multichannel_mix_hook))
    {
      for (i=n-1;i>=0;i--)
	lst = scm_cons(SCM_MAKINUM(ids[i]),lst);
      g_c_run_progn_hook(multichannel_mix_hook,SCM_LIST1(lst));
    }
}

char *output_name(snd_state *ss)
{
  if (HOOKED(output_name_hook))
    {
      SCM result;
      SCM procs = SCM_HOOK_PROCEDURES (output_name_hook);
      while (SCM_NIMP (procs))
	{
	  result = g_call0(SCM_CAR(procs));
	  if (gh_string_p(result)) return(gh_scm2newstr(result,NULL));
	  procs = SCM_CDR (procs);
	}
    }
  return(NULL);
}

  #if FILE_PER_CHAN
    int multifile_channel(char *filename)
    {
      int res = -1;
      SCM hookres = SCM_BOOL_F;
      if (HOOKED(open_multifile_sound_hook))
	{
	  hookres = g_c_run_progn_hook(open_multifile_sound_hook,SCM_LIST1(gh_str02scm(filename)));
	  res = g_scm2intdef(hookres,-2);
	}
      return(res);
    }
    char *multifile_save(int snd, int chn)
    {
      SCM hookres = SCM_BOOL_F;
      if (HOOKED(save_multifile_sound_hook))
	hookres = g_c_run_progn_hook(save_multifile_sound_hook,SCM_LIST2(gh_int2scm(snd),gh_int2scm(chn)));
      if (gh_string_p(hookres))
	return(gh_scm2newstr(hookres,NULL));
      return(NULL);
    }
  #endif

int dont_edit(chan_info *cp) 
{
  SCM res = SCM_BOOL_F;
  if (HOOKED(cp->edit_hook))
    res = g_c_run_or_hook(cp->edit_hook,SCM_EOL);
  return(SCM_TRUE_P(res));
}

void call_undo_hook(chan_info *cp, int undo)
{
  if (HOOKED(cp->undo_hook))
    g_c_run_progn_hook(cp->undo_hook,SCM_EOL);
}


#else
int dont_open(snd_state *ss, char *file) {return(0);}
int dont_close(snd_state *ss, snd_info *sp) {return(0);}
int after_fft(snd_state *ss, chan_info *cp, Float scaler) {return(0);}
int dont_graph(snd_state *ss, chan_info *cp) {return(0);}
void after_graph(snd_state *ss, chan_info *cp) {}
int dont_exit(snd_state *ss) {return(0);}
int dont_start(snd_state *ss, char *filename) {return(0);}
int handle_mark_click(snd_state *ss, int id) {return(0);}
void handle_mouse_release(snd_state *ss, snd_info *sp, chan_info *cp, Float x, Float y, int button, int state) {}
void handle_mouse_press(snd_state *ss, snd_info *sp, chan_info *cp, Float x, Float y, int button, int state) {}
void handle_mouse_drag(snd_state *ss, snd_info *sp, chan_info *cp, Float x, Float y) {}
int handle_key_press(snd_state *ss, snd_info *sp, chan_info *cp, int key, int state) {return(0);}
void call_stop_playing_hook(snd_info *sp) {}
void call_stop_playing_region_hook(int n) {}
int call_start_playing_hook(snd_info *sp) {return(0);}
void call_mix_console_state_changed_hook(mixdata *md) {}
int call_mix_speed_changed_hook(mixdata *md) {return(0);}
int call_mix_amp_changed_hook(mixdata *md) {return(0);}
int call_mix_position_changed_hook(mixdata *md, int samps) {return(0);}
void call_multichannel_mix_hook(snd_state *ss, int *ids, int n) {}
void during_open(int fd, char *file, int reason) {}
void after_open(int index) {}
int ignore_mus_error(int type, char *msg) {return(0);}
int ignore_snd_error(char *msg) {return(0);}
int ignore_snd_warning(char *msg) {return(0);}
char *output_name(snd_state *ss) {return(NULL);}
  #if FILE_PER_CHAN
    int multifile_channel(char *filename) {return(-1);}
    char *multifile_save(int snd, int chn) {return(NULL);}
  #endif
int dont_edit(chan_info *cp) {return(0);}
void call_undo_hook(chan_info *cp, int undo) {return;}
#endif


void set_memo_sound(snd_info *sp)
{
  GH_INT_SET(memo_sound,sp->index);
}

int handle_keymap(chan_info *cp, SCM func) /* not a hook! */
{
  return(g_scm2intdef(g_call0(func),KEYBOARD_NO_ACTION));
}

#endif

char *output_comment(snd_state *ss, file_info *hdr)
{
  char *com = NULL;
  if (hdr) com = mus_sound_comment(hdr->name);
#if (HAVE_GUILE && (!HAVE_GUILE_1_3_0))
  if (HOOKED(output_comment_hook))
    {
      SCM result;
      SCM procs = SCM_HOOK_PROCEDURES (output_comment_hook);
      int comment_size = 0;
      char *new_comment = NULL,*tmpstr = NULL;
      while (SCM_NIMP (procs))
	{
	  result = g_call1(SCM_CAR(procs),gh_str02scm(com));
	  tmpstr = gh_scm2newstr(result,NULL);
	  if (tmpstr)
	    {
	      if ((snd_strlen(tmpstr) + snd_strlen(new_comment)) >= comment_size)
		{
		  comment_size = ((snd_strlen(tmpstr) + snd_strlen(new_comment)) * 2);
		  if (comment_size < 1024) comment_size = 1024;
		}
	      if (new_comment == NULL)
		new_comment = (char *)CALLOC(comment_size,sizeof(char));
	      else new_comment = (char *)REALLOC(new_comment,comment_size * sizeof(char));
	      strcat(new_comment,tmpstr);
	      free(tmpstr);
	    }
	  procs = SCM_CDR (procs);
	}
      return(new_comment);
    }
  else return(com);
#else
  return(com);
#endif
}

#if 0 
/* uniform arrays: (from xlib.c)

  v = SCM_ARRAY_V (arg);
  SCM_ASSERT (SCM_NIMP (v) && SCM_TYP7 (v) == scm_tc7_svect, arg, pos, func);
  vdat = (short *) SCM_CDR (v);
  SCM_ASSERT (SCM_ARRAY_NDIM (arg) == 2, arg, pos, func);
  dims = SCM_ARRAY_DIMS (arg);
  num_data             = dims[0].ubnd - dims[0].lbnd + 1;
  num_shorts_per_datum = dims[1].ubnd - dims[1].lbnd + 1;
*/
#endif
