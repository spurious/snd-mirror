#include "snd.h"
#include "vct.h"

/* TODO: perhaps fit-data-on-open should be fit-data, callable via hooks at open time
 *       mark-moved-hook? selection-creation-hook? sample-color?
 */

#if HAVE_GUILE

#include "sndlib2scm.h"
static snd_state *state = NULL;


/* if error occurs in sndlib, mus-error wants to throw to user-defined catch
 *   (or out own global catch), but if the sndlib function was not called by the user, 
 *   the attempt to throw to a non-existent catch tag exits the main program!!
 *   so, we only throw if the catch_exists flag is true.
 */
static SCM snd_internal_stack_catch (SCM tag,
				     scm_catch_body_t body,
				     void *body_data,
				     scm_catch_handler_t handler,
				     void *handler_data)
{ /* declaration from libguile/throw */
  SCM result;
  state->catch_exists = 1;
  result = scm_internal_stack_catch(tag,body,body_data,handler,handler_data);
  state->catch_exists = 0;
  return(result);
}


static SCM gc_protection;
static int gc_protection_size = 0;
#define DEFAULT_GC_VALUE SCM_MAKINUM(0)

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

void ERRSP(char *origin, SCM snd, int off)
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

static int g_error_occurred = 0;

#define MAX_ERROR_STRING_LENGTH 512

#include <libguile/fluids.h>

static SCM parse_proc_handler(void *data, SCM tag, SCM throw_args)
{
  g_error_occurred = 1;
  return(SCM_BOOL_F);
}

static void string_to_stdout(snd_state *ss, char *msg)
{
  char *str;
  write(fileno(stdout),msg,snd_strlen(msg));
  str = (char *)CALLOC(8,sizeof(char));
  sprintf(str,"\n%s",listener_prompt(ss));
  write(fileno(stdout),str,snd_strlen(str));
  FREE(str);
}

static int send_error_output_to_stdout = 0;

SCM snd_catch_scm_error(void *data, SCM tag, SCM throw_args) /* error handler */
{
  /* this is actually catching any throw not caught elsewhere, I think */
  snd_info *sp;
  /* it would be nice if this would display the current file + line number when loading scm code */
  /*   there's apparently a line-number function and *load-pathname* */
  SCM port,ans,stmp;
#if (!HAVE_GUILE_1_3)
  SCM stack;
#endif
  char *name_buf = NULL;
#ifdef SCM_MAKE_CHAR
  port = scm_mkstrport(SCM_INUM0, 
		       scm_make_string(SCM_MAKINUM(MAX_ERROR_STRING_LENGTH), SCM_MAKE_CHAR(0)),
		       SCM_OPN | SCM_WRTNG,
		       "snd-scm-error-handler");
#else
  port = scm_mkstrport(SCM_INUM0, 
		       scm_make_string(SCM_MAKINUM(MAX_ERROR_STRING_LENGTH), SCM_UNDEFINED),
		       SCM_OPN | SCM_WRTNG,
		       "snd-scm-error-handler");
#endif

  if ((gh_list_p(throw_args)) && (gh_length(throw_args) > 0))
    {
      if (SCM_EQ_P(tag,MUS_MISC_ERROR))
	{
	  scm_display(tag,port);
	  scm_puts(": ", port);
	  scm_display(throw_args,port);
	}
      else 
	{
	  scm_display(gh_car(throw_args), port);
	  scm_puts(": ", port);
	  if (gh_length(throw_args) > 1)
	    {
	      if (SCM_EQ_P(tag,NO_SUCH_FILE))
		{
		  scm_display(tag,port);
		  scm_puts(" \"",port);
		  scm_display(gh_cadr(throw_args),port);
		  scm_puts("\"",port);
		}
	      else
		{
		  if ((SCM_EQ_P(tag,NO_SUCH_SOUND)) || (SCM_EQ_P(tag,NO_SUCH_MIX)) || (SCM_EQ_P(tag,NO_SUCH_MARK)) ||
		      (SCM_EQ_P(tag,NO_SUCH_MENU)) || (SCM_EQ_P(tag,NO_SUCH_REGION)) ||
		      (SCM_EQ_P(tag,NO_SUCH_CHANNEL)) || (SCM_EQ_P(tag,NO_SUCH_EDIT)) ||
		      (SCM_EQ_P(tag,IMPOSSIBLE_BOUNDS)) || (SCM_EQ_P(tag,NO_SUCH_SAMPLE)))
		    {
		      scm_display(tag,port);
		      scm_puts(" ",port);
		      scm_display(gh_cdr(throw_args),port);
		    }
		  else
		    {
		      stmp = gh_cadr(throw_args);
		      if (gh_string_p(stmp)) 
			scm_display_error_message(stmp, gh_caddr(throw_args), port);
		      else scm_display(tag, port);
#if (!HAVE_GUILE_1_3)
		      stack = scm_fluid_ref(SCM_CDR(scm_the_last_stack_fluid));
		      if (SCM_NFALSEP(stack)) scm_display_backtrace(stack,port,SCM_UNDEFINED,SCM_UNDEFINED);
#endif
		    }
		}
	    }
	  else scm_display(tag,port);
	}
    }
  else 
    {
      scm_display(tag,port);
      scm_puts(": ", port);
      scm_display(throw_args,port);
    }
#if (!HAVE_GUILE_1_3_0)
  scm_force_output(port); /* needed to get rid of trailing garbage chars?? -- might be pointless now */
  ans = scm_strport_to_string(port);
#else
  SCM_DEFER_INTS;
  /* new version? SCM_STRING_CHARS(SCM_CDR...) */
  ans = scm_makfromstr (SCM_CHARS (SCM_CDR (SCM_STREAM (port))),SCM_INUM (SCM_CAR (SCM_STREAM (port))),0);
  SCM_ALLOW_INTS;
#endif

  name_buf = gh_scm2newstr(ans,NULL);
#if DEBUGGING
  fprintf(stderr,"%s\n",name_buf);
#endif
  if (send_error_output_to_stdout)
    string_to_stdout(state,name_buf);
  else
    {
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
    }
  g_error_occurred = 1;
  if (name_buf) free(name_buf);
  return(tag);
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

SCM eval_str_wrapper(void *data)
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
  return(snd_internal_stack_catch(SCM_BOOL_T,g_call0_1,(void *)proc,snd_catch_scm_error,NULL));
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
  return(snd_internal_stack_catch(SCM_BOOL_T,g_call1_1,(void *)args,snd_catch_scm_error,NULL));
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
  return(snd_internal_stack_catch(SCM_BOOL_T,g_call_any_1,(void *)args,snd_catch_scm_error,NULL));
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
  return(snd_internal_stack_catch(SCM_BOOL_T,g_call2_1,(void *)args,snd_catch_scm_error,NULL));
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
  return(snd_internal_stack_catch(SCM_BOOL_T,g_call3_1,(void *)args,snd_catch_scm_error,NULL));
}

int bool_int_or_one(SCM n)
{
  if (SCM_FALSEP(n)) 
    return(0);
  else return(g_scm2intdef(n,1));
  /* i.e. SCM_UNDEFINED -> t (which is needed for opt #t args below) */
}

char *full_filename(SCM file)
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
  return(str1);
}

static char *gh_print(SCM result)
{
  char *newbuf = NULL,*str = NULL;
  int i,ilen,savelen,savectr,slen;
  /* specialize vectors which can be enormous in this context */
  if ((!(gh_vector_p(result))) || ((int)(gh_vector_length(result)) <= print_length(state)))
    return(gh_print_1(result));
  ilen=print_length(state); 
  newbuf = (char *)calloc(128,sizeof(char));
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
	      newbuf = (char *)realloc(newbuf,savelen * sizeof(char));
	    }
	  if (i != 0) {strcat(newbuf," "); savectr++;}
	  strcat(newbuf,str);
	  savectr+=slen;
	}
      if (str) free(str);
    }
  if (savectr+8 > savelen) newbuf = (char *)realloc(newbuf,(savectr+8) * sizeof(char));
  strcat(newbuf," ...)");
  return(newbuf);
}

SCM array_to_list(Float *arr, int i, int len)
{
  if (i < (len-1))
    return(gh_cons(gh_double2scm(arr[i]),array_to_list(arr,i+1,len)));
  else return(gh_cons(gh_double2scm(arr[i]),SCM_EOL));
}

SCM parse_proc(char *buf)
{
  SCM result;
  result = snd_internal_stack_catch(SCM_BOOL_T,eval_str_wrapper,buf,parse_proc_handler,buf);
  if (g_error_occurred)
    {
      g_error_occurred = 0;
      return(SCM_BOOL_F);
    }
  return(result);
}

int snd_eval_str(snd_state *ss, char *buf, int count)
{
  snd_info *sp = NULL;
  SCM result = SCM_UNDEFINED;
  int ctr;
  char *str = NULL;
  for (ctr=0;ctr<count;ctr++)
    {
      result = snd_internal_stack_catch(SCM_BOOL_T,eval_str_wrapper,buf,snd_catch_scm_error,buf);
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
  if (str) free(str);
  return(0);
}

void snd_eval_listener_str(snd_state *ss, char *buf)
{
  SCM result;
  char *str;
  if ((snd_strlen(buf) == 0) || ((snd_strlen(buf) == 1) && (buf[0] == '\n'))) return;
  result = snd_internal_stack_catch(SCM_BOOL_T,eval_str_wrapper,buf,snd_catch_scm_error,buf);
  if (g_error_occurred)
    g_error_occurred = 0;
  else
    {
      str = gh_print(result);
      ss->result_printout = MESSAGE_WITH_CARET;
      snd_append_command(ss,str);
      if (str) free(str);
    }
}


void snd_eval_stdin_str(snd_state *ss, char *buf)
{
  SCM result;
  char *str = NULL;
  if ((snd_strlen(buf) == 0) || ((snd_strlen(buf) == 1) && (buf[0] == '\n'))) return;
  send_error_output_to_stdout = 1;
  result = snd_internal_stack_catch(SCM_BOOL_T,eval_str_wrapper,buf,snd_catch_scm_error,buf);
  send_error_output_to_stdout = 0;
  if (g_error_occurred)
    g_error_occurred = 0;
  else
    {
      str = gh_print(result);
      string_to_stdout(ss,str);
      if (str) free(str);
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
	  snd_internal_stack_catch(SCM_BOOL_T,eval_file_wrapper,SND_CONF,snd_catch_scm_error,NULL);
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
	  snd_internal_stack_catch(SCM_BOOL_T,eval_file_wrapper,str,snd_catch_scm_error,NULL);
	}
      if (str) FREE(str);
    }
}

void snd_load_file(char *filename)
{
  char *str = NULL,*str1 = NULL;
  str = mus_file_full_name(filename);
  if (!mus_file_probe(str))
    {
      /* try tacking on .scm */
      str1 = (char *)CALLOC(snd_strlen(str) + 5,sizeof(char));
      sprintf(str1,"%s.scm",str);
      if (!mus_file_probe(str1))
	snd_error("can't load %s: %s",filename,strerror(errno));
      else snd_internal_stack_catch(SCM_BOOL_T,eval_file_wrapper,str1,snd_catch_scm_error,NULL);
      FREE(str1);
    }
  else snd_internal_stack_catch(SCM_BOOL_T,eval_file_wrapper,str,snd_catch_scm_error,NULL);
  if (str) FREE(str);
}

static SCM g_snd_print(SCM msg)
{
  #define H_snd_print "(" S_snd_print " str) displays str in the lisp listener window"
  char *str=NULL;
  SCM_ASSERT(gh_string_p(msg),msg,SCM_ARG1,S_snd_print);
  state->result_printout = MESSAGE_WITHOUT_CARET;
  str = gh_scm2newstr(msg,NULL);
  snd_append_command(state,str);
#if DEBUGGING
  fprintf(stderr,"%s\n",str);
#endif
  if (str) free(str);
  return(msg);
}


/* global variables */

static SCM g_ask_before_overwrite(void) {RTNBOOL(ask_before_overwrite(state));}
static SCM g_set_ask_before_overwrite(SCM val) 
{
  #define H_ask_before_overwrite "(" S_ask_before_overwrite ") should be #t if you want Snd to ask before overwriting a file"
  ERRB1(val,"set-" S_ask_before_overwrite); 
  set_ask_before_overwrite(state,bool_int_or_one(val)); 
  RTNBOOL(ask_before_overwrite(state));
}

static SCM g_audio_output_device(void) {RTNINT(audio_output_device(state));}
static SCM g_set_audio_output_device(SCM val) 
{
  #define H_audio_output_device "(" S_audio_output_device ") is the current sndlib default output device (mus-audio-default)"
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(val)),val,SCM_ARG1,"set-" S_audio_output_device); 
  set_audio_output_device(state,g_scm2int(val)); 
  RTNINT(audio_output_device(state));
}

static SCM g_dac_size(void) {RTNINT(dac_size(state));}
static SCM g_set_dac_size(SCM val) 
{
  #define H_dac_size "(dac-size) is the current DAC buffer size (256)"
  int len;
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(val)),val,SCM_ARG1,"set-" S_dac_size);
  len = g_scm2int(val);
  if (len > 0)
    set_dac_size(state,len);
  RTNINT(dac_size(state));
}

static SCM g_dac_folding(void) {RTNBOOL(dac_folding(state));}
static SCM g_set_dac_folding(SCM val) 
{
  #define H_dac_folding "(" S_dac_folding ") should be #t if extra channels are to be folded into available ones during playing (#t)"
  ERRB1(val,"set-" S_dac_folding); 
  set_dac_folding(state,bool_int_or_one(val)); 
  RTNBOOL(dac_folding(state));
}

static SCM g_auto_resize(void) {RTNBOOL(auto_resize(state));}
static SCM g_set_auto_resize(SCM val) 
{
  #define H_auto_resize "(" S_auto_resize ") should be #t if Snd can change its main window size as it pleases (#t)"
  ERRB1(val,"set-" S_auto_resize); 
  set_auto_resize(state,bool_int_or_one(val)); 
  reflect_resize(state); 
  RTNBOOL(auto_resize(state));
}

static SCM g_auto_update(void) {RTNBOOL(auto_update(state));}
static SCM g_set_auto_update(SCM val) 
{
  #define H_auto_update "(" S_auto_update ") -> #t if Snd should automatically update a file if it changes unexpectedly (#f)"
  ERRB1(val,"set-" S_auto_update); 
  set_auto_update(state,bool_int_or_one(val)); 
  RTNBOOL(auto_update(state));
}

static SCM g_channel_style(void) {RTNINT(channel_style(state));}
static SCM g_set_channel_style(SCM style) 
{
  #define H_channel_style "(" S_channel_style ") -> how multichannel sounds layout the channels\n\
   default is channels-separate, other values are channels-combined and channels-superimposed.\n\
   this is the default setting for each sound's 'unite' button."
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(style)),style,SCM_ARG1,"set-" S_channel_style); 
  set_channel_style(state,iclamp(CHANNELS_SEPARATE,g_scm2int(style),CHANNELS_SUPERIMPOSED)); 
  RTNINT(channel_style(state));
}

static SCM g_color_cutoff(void) {RTNFLT(color_cutoff(state));}
static SCM g_set_color_cutoff(SCM val) 
{
  #define H_color_cutoff "(" S_color_cutoff ") -> color map cutoff point (default .003)"
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(val)),val,SCM_ARG1,"set-" S_color_cutoff);
  set_color_cutoff(state,fclamp(0.0,gh_scm2double(val),0.25)); 
  RTNFLT(color_cutoff(state));
}

static SCM g_color_inverted(void) {RTNBOOL(color_inverted(state));}
static SCM g_set_color_inverted(SCM val) 
{
  #define H_color_inverted "(" S_color_inverted ") -> whether the colormap in operation should be inverted"
  ERRB1(val,"set-" S_color_inverted); 
  set_color_inverted(state,bool_int_or_one(val)); 
  RTNBOOL(color_inverted(state));
}

static SCM g_color_scale(void) {RTNFLT(color_scale(state));}
static SCM g_set_color_scale(SCM val) 
{
  #define H_color_scale "(" S_color_scale ") -> essentially a darkness setting for colormaps (0.5)"
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(val)),val,SCM_ARG1,"set-" S_color_scale); 
  set_color_scale(state,fclamp(0.0,gh_scm2double(val),1.0)); 
  RTNFLT(color_scale(state));
}

static SCM g_corruption_time(void) {RTNFLT(corruption_time(state));}
static SCM g_set_corruption_time(SCM val) 
{
  #define H_corruption_time "(" S_corruption_time ") -> time (seconds) between background checks for changed file on disk (60)"
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(val)),val,SCM_ARG1,"set-" S_corruption_time); 
  set_corruption_time(state,gh_scm2double(val)); 
  RTNFLT(corruption_time(state));
}

static SCM g_default_output_chans(void) {RTNINT(default_output_chans(state));}
static SCM g_set_default_output_chans(SCM val) 
{
  #define H_default_output_chans "(" S_default_output_chans ") -> default number of channels when a new or temporary file is created (1)"
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(val)),val,SCM_ARG1,"set-" S_default_output_chans); 
  set_default_output_chans(state,g_scm2int(val));
  RTNINT(default_output_chans(state));
}

static SCM g_default_output_srate(void) {RTNINT(default_output_srate(state));}
static SCM g_set_default_output_srate(SCM val) 
{
  #define H_default_output_srate "(" S_default_output_srate ") -> default srate when a new or temporary file is created (22050)" 
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(val)),val,SCM_ARG1,"set-" S_default_output_srate); 
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
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(val)),val,SCM_ARG1,"set-" S_default_output_type); 
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
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(val)),val,SCM_ARG1,"set-" S_default_output_format); 
  format = g_scm2int(val);
  if (mus_header_writable(default_output_type(state),format))
    set_default_output_format(state,format); 
  else snd_warning("can't write %s data to %s headers",mus_data_format_name(format),mus_header_type_name(default_output_type(state)));
  RTNINT(default_output_format(state));
}

static SCM g_enved_base(void) {RTNFLT(enved_base(state));}
static SCM g_set_enved_base(SCM val) 
{
  #define H_enved_base "(" S_enved_base ") -> envelope editor exponential base value (1.0)"
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(val)),val,SCM_ARG1,"set-" S_enved_base); 
  set_enved_base(state,gh_scm2double(val));
  RTNFLT(enved_base(state));
}

static SCM g_enved_power(void) {RTNFLT(enved_power(state));}
static SCM g_set_enved_power(SCM val) 
{
  #define H_enved_power "(" S_enved_power ") -> envelope editor base scale range (9.0^power)"
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(val)),val,SCM_ARG1,"set-" S_enved_power); 
  set_enved_power(state,gh_scm2double(val));
  RTNFLT(enved_power(state));
}

static SCM g_enved_clipping(void) {RTNBOOL(enved_clipping(state));}
static SCM g_set_enved_clipping(SCM on)
{
  #define H_enved_clipping "(" S_enved_clipping ") -> envelope editor 'clip' button setting\n\
   if clipping, the motion of the mouse is restricted to the current graph bounds."
  ERRB1(on,"set-" S_enved_clipping); 
  set_enved_clipping(state,bool_int_or_one(on)); 
  RTNBOOL(enved_clipping(state));
}

static SCM g_enved_exping(void) {RTNBOOL(enved_exping(state));}
static SCM g_set_enved_exping(SCM val) 
{
  #define H_enved_exping "(" S_enved_exping ") -> envelope editor 'exp' and 'lin' buttons\n\
   if enved-exping, the connecting segments use exponential curves rather than straight lines."
  ERRB1(val,"set-" S_enved_exping); 
  set_enved_exping(state,bool_int_or_one(val)); 
  RTNBOOL(enved_clipping(state));
}

static SCM g_enved_target(void) {RTNINT(enved_target(state));}
static SCM g_set_enved_target(SCM val) 
{
  int n; 
  #define H_enved_target "(" S_enved_target ") determines how the envelope is applied to data in the envelope editor\n\
   choices are amplitude-env, srate-env, and spectrum-env"
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(val)),val,SCM_ARG1,"set-" S_enved_target); 
  n = iclamp(AMPLITUDE_ENV,g_scm2intdef(val,0),SRATE_ENV); 
  set_enved_target(state,n); 
  RTNINT(enved_target(state));
}

static SCM g_enved_waving(void) {RTNBOOL(enved_waving(state));}
static SCM g_set_enved_waving(SCM val) 
{
  #define H_enved_waving "(" S_enved_waving ") -> #t if the envelope editor is displaying the waveform to be edited"
  ERRB1(val,"set-" S_enved_waving); 
  set_enved_waving(state,bool_int_or_one(val));
  RTNBOOL(enved_waving(state));
}

static SCM g_enved_dBing(void) {RTNBOOL(enved_dBing(state));}
static SCM g_set_enved_dBing(SCM val) 
{
  #define H_enved_dBing "(" S_enved_dBing ") -> #t if the envelope editor is using dB"
  ERRB1(val,"set-" S_enved_dBing); 
  set_enved_dBing(state,bool_int_or_one(val)); 
  RTNBOOL(enved_dBing(state));
}

static SCM g_eps_file(void) {RTNSTR(eps_file(state));}
static SCM g_set_eps_file(SCM val) 
{
  #define H_eps_file "(" S_eps_file ") -> current eps ('Print' command) file name (snd.eps)"
  SCM_ASSERT(gh_string_p(val),val,SCM_ARG1,"set-" S_eps_file); 
  set_eps_file(state,gh_scm2newstr(val,0)); 
  RTNSTR(eps_file(state));
}

static SCM g_listener_prompt(void) {RTNSTR(listener_prompt(state));}
static SCM g_set_listener_prompt(SCM val) 
{
  #define H_listener_prompt "(" S_listener_prompt ") -> the current lisp listener prompt character ('>') "
  SCM_ASSERT(gh_string_p(val),val,SCM_ARG1,"set-" S_listener_prompt); 
  set_listener_prompt(state,gh_scm2newstr(val,0));
  RTNSTR(listener_prompt(state));
}

static SCM g_audio_state_file(void) {RTNSTR(audio_state_file(state));}
static SCM g_set_audio_state_file(SCM val) 
{
  #define H_audio_state_file "(" S_audio_state_file ") -> filename for the mus-audio-save-state function (.snd-mixer)"
  SCM_ASSERT(gh_string_p(val),val,SCM_ARG1,"set-" S_audio_state_file); 
  set_audio_state_file(state,gh_scm2newstr(val,0));
  RTNSTR(audio_state_file(state));
}

static SCM g_filter_env_order(void) {RTNINT(filter_env_order(state));}
static SCM g_set_filter_env_order(SCM val) 
{
  #define H_filter_env_order "(" S_filter_env_order ") -> envelope editor's FIR filter order (40)"
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(val)),val,SCM_ARG1,"set-" S_filter_env); 
  set_filter_env_order(state,g_scm2int(val));
  RTNINT(filter_env_order(state));
}

static SCM g_fit_data_on_open(void) {RTNBOOL(fit_data_on_open(state));}
static SCM g_set_fit_data_on_open(SCM val) 
{
  #define H_fit_data_on_open "(" S_fit_data_on_open ") -> #t if Snd should set up the initial time domain axes to show the entire sound (#f)"
  ERRB1(val,"set-" S_fit_data_on_open); 
  set_fit_data_on_open(state,bool_int_or_one(val));
  RTNBOOL(fit_data_on_open(state));
}

static SCM g_initial_x0(void) {RTNFLT(initial_x0(state));}
static SCM g_set_initial_x0(SCM val) 
{
  #define H_initial_x0 "(" S_initial_x0 ") -> initial time domain window left edge (seconds, 0.0)"
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(val)),val,SCM_ARG1,"set-" S_initial_x0); 
  set_initial_x0(state,gh_scm2double(val));
  RTNFLT(initial_x0(state));
}

static SCM g_initial_x1(void) {RTNFLT(initial_x1(state));}
static SCM g_set_initial_x1(SCM val) 
{
  #define H_initial_x1 "(" S_initial_x1 ") -> initial time domain window right edge (seconds, 0.1)"
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(val)),val,SCM_ARG1,"set-" S_initial_x1); 
  set_initial_x1(state,gh_scm2double(val));
  RTNFLT(initial_x1(state));
}

static SCM g_initial_y0(void) {RTNFLT(initial_y0(state));}
static SCM g_set_initial_y0(SCM val) 
{
  #define H_initial_y0 "(" S_initial_y0 ") -> initial time domain window lower edge (-1.0)"
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(val)),val,SCM_ARG1,"set-" S_initial_y0); 
  set_initial_y0(state,gh_scm2double(val));
  RTNFLT(initial_y0(state));
}

static SCM g_initial_y1(void) {RTNFLT(initial_y1(state));}
static SCM g_set_initial_y1(SCM val) 
{
  #define H_initial_y1 "(" S_initial_y1 ") -> initial time domain window upper edge (1.0)"
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(val)),val,SCM_ARG1,"set-" S_initial_y1); 
  set_initial_y1(state,gh_scm2double(val));
  RTNFLT(initial_y1(state));
}

static SCM g_mix_console_amp_scaler(void) {RTNFLT(get_mix_console_amp_scaler());}
static SCM g_set_mix_console_amp_scaler(SCM val) 
{
  #define H_mix_console_amp_scaler "(" S_mix_console_amp_scaler ") -> multiplier on amp scales in mix consoles (1.0)"
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(val)),val,SCM_ARG1,"set-" S_mix_console_amp_scaler); 
  set_mix_console_amp_scaler(gh_scm2double(val));
  RTNFLT(get_mix_console_amp_scaler());
}

static SCM g_mix_console_speed_scaler(void) {RTNFLT(get_mix_console_speed_scaler());}
static SCM g_set_mix_console_speed_scaler(SCM val) 
{
  #define H_mix_console_speed_scaler "(" S_mix_console_speed_scaler ") -> Multiplier on speed scales in mix consoles (1.0)"
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(val)),val,SCM_ARG1,"set-" S_mix_console_speed_scaler); 
  set_mix_console_speed_scaler(gh_scm2double(val));
  RTNFLT(get_mix_console_speed_scaler());
}

static SCM g_movies(void) {RTNBOOL(movies(state));}
static SCM g_set_movies(SCM val) 
{
  #define H_movies "(" S_movies ") -> #t if mix graphs are update continuously as the mix is dragged (#t)"
  ERRB1(val,"set-" S_movies); 
  set_movies(state,bool_int_or_one(val));
  RTNBOOL(movies(state));
}

static SCM g_normalize_on_open(void) {RTNBOOL(normalize_on_open(state));}
static SCM g_set_normalize_on_open(SCM val) 
{
  #define H_normalize_on_open "(" S_normalize_on_open ") -> #t if Snd should try to evenly apportion screen space when a sound is opened (#t)"
  ERRB1(val,"set-" S_normalize_on_open); 
  set_normalize_on_open(state,bool_int_or_one(val));
  RTNBOOL(normalize_on_open(state));
}

static SCM g_prefix_arg(void) {RTNINT(prefix_arg(state));}
static SCM g_set_prefix_arg(SCM val) 
{
  #define H_prefix_arg "(" S_prefix_arg ") -> keyboard C-u argument"
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(val)),val,SCM_ARG1,"set-" S_prefix_arg); 
  set_prefix_arg(state,g_scm2int(val));
  RTNINT(prefix_arg(state));
}

static SCM g_print_length(void) {RTNINT(print_length(state));}
static SCM g_set_print_length(SCM val) 
{
  #define H_print_length "(" S_print_length ") -> number of vector elements to print in the listener (12)"
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(val)),val,SCM_ARG1,"set-" S_print_length); 
  set_print_length(state,g_scm2int(val)); 
  set_vct_print_length(g_scm2int(val));
  RTNINT(print_length(state));
}

static SCM g_previous_files_sort(void) {RTNINT(previous_files_sort(state));}
static SCM g_set_previous_files_sort(SCM val) 
{
  #define H_previous_files_sort "(" S_previous_files_sort ") -> sort choice in view files (0=unsorted, 1=by name, etc)"
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(val)),val,SCM_ARG1,"set-" S_previous_files_sort); 
  set_previous_files_sort(state,iclamp(0,g_scm2int(val),4));
  update_prevfiles(state);
  RTNINT(previous_files_sort(state));
}

static SCM g_raw_chans(void) {RTNINT(raw_chans(state));}
static SCM g_set_raw_chans(SCM val) 
{
  #define H_raw_chans "(" S_raw_chans ") -> how many channels to expect in raw (headerless) files (1)"
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(val)),val,SCM_ARG1,"set-" S_raw_chans);
  set_raw_chans(state,g_scm2int(val));
  RTNINT(raw_chans(state));
}

static SCM g_raw_format(void) {RTNINT(raw_format(state));}
static SCM g_set_raw_format(SCM val) 
{
  #define H_raw_format "(" S_raw_format ") -> data format expected in raw (headerless) files (mus-bshort)"
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(val)),val,SCM_ARG1,"set-" S_raw_format); 
  set_raw_format(state,g_scm2int(val));
  RTNINT(raw_format(state));
}

static SCM g_raw_srate(void) {RTNINT(raw_srate(state));}
static SCM g_set_raw_srate(SCM val) 
{
  #define H_raw_srate "(" S_raw_srate ") -> srate expected in raw (headerless) files (44100)"
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(val)),val,SCM_ARG1,"set-" S_raw_srate); 
  set_raw_srate(state,g_scm2int(val));
  RTNINT(raw_srate(state));
}

static SCM g_save_state_on_exit(void) {RTNBOOL(save_state_on_exit(state));}
static SCM g_set_save_state_on_exit(SCM val) 
{
  #define H_save_state_on_exit "(" S_save_state_on_exit ") -> #t if Snd should save its current state upon exit"
  ERRB1(val,"set-" S_save_state_on_exit); 
  set_save_state_on_exit(state,bool_int_or_one(val));
  RTNBOOL(save_state_on_exit(state));
}

static SCM g_show_indices(void) {RTNBOOL(show_indices(state));}
static SCM g_set_show_indices(SCM val) 
{
  #define H_show_indices "(" S_show_indices ") -> #t if sound name should be preceded by its index"
  ERRB1(val,"set-" S_show_indices); 
  set_show_indices(state,bool_int_or_one(val));
  RTNBOOL(show_indices(state));
}

static SCM g_show_usage_stats(void) {RTNBOOL(show_usage_stats(state));}
static SCM g_set_show_usage_stats(SCM on) 
{
  #define H_show_usage_stats "(" S_show_usage_stats ") -> #t if Snd should display memory usage stats"
  ERRB1(on,"set-" S_show_usage_stats); 
  set_show_usage_stats(state,bool_int_or_one(on));
  RTNBOOL(show_usage_stats(state));
}

static SCM g_update_usage_stats(void) 
{
  #define H_update_usage_stats "(" S_update_usage_stats ") causes the stats display to be made current"
  update_stats(state); 
  return(SCM_BOOL_T);
}

static SCM g_sinc_width(void) {RTNINT(sinc_width(state));}
static SCM g_set_sinc_width(SCM val) 
{
  #define H_sinc_width "(" S_sinc_width ") -> sampling rate conversion sinc width (10)\n\
   The higher this number, the better the src low-pass filter, but the slower\n\
   src runs.  If you use too low a setting, you can sometimes hear high\n\
   frequency whistles leaking through."

  int len;
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(val)),val,SCM_ARG1,"set-" S_sinc_width); 
  len = g_scm2int(val);
  if (len >= 0)
    set_sinc_width(state,len);
  RTNINT(sinc_width(state));
}

static SCM g_color_map(void) {RTNINT(color_map(state));}
static SCM g_set_color_map(SCM val) 
{
  #define H_colormap "(" S_colormap ") -> current colormap choice\n\
   This should be an integer between -1 and 15.  The maps (from 0 to 15) are:\n\
   gray, hsv, hot, cool, bone, copper, pink, jet, prism, autumn, winter,\n\
   spring, summer, colorcube, flag, and lines.  -1 means black and white."

  SCM_ASSERT(SCM_NFALSEP(scm_real_p(val)),val,SCM_ARG1,"set-" S_colormap); 
  set_color_map(state,iclamp(0,g_scm2int(val),NUM_COLORMAPS-1));
  RTNINT(color_map(state));
}

static SCM g_temp_dir(void) {RTNSTR(temp_dir(state));}
static SCM g_set_temp_dir(SCM val) 
{
  #define H_temp_dir "(" S_temp_dir ") -> name of directory for temp files"
  SCM_ASSERT(gh_string_p(val),val,SCM_ARG1,"set-" S_temp_dir); 
  set_temp_dir(state,gh_scm2newstr(val,0));
  RTNSTR(temp_dir(state));
}

static SCM g_snd_tempnam(void) 
{
  #define H_snd_tempnam "(" S_snd_tempnam ") -> new temp file name using temp-dir"
  return(gh_str02scm(snd_tempnam(get_global_state())));
}

static SCM g_save_dir(void) {RTNSTR(save_dir(state));}
static SCM g_set_save_dir(SCM val) 
{
  #define H_save_dir "(" S_save_dir ") -> name of directory for saved state data"
  SCM_ASSERT(gh_string_p(val),val,SCM_ARG1,"set-" S_save_dir); 
  set_save_dir(state,gh_scm2newstr(val,0));
  RTNSTR(save_dir(state));
}

static SCM g_trap_segfault(void) {RTNBOOL(trap_segfault(state));}
static SCM g_set_trap_segfault(SCM val) 
{
  #define H_trap_segfault "(" S_trap_segfault ") -> #t if Snd should try to trap (and whine about) segfaults"
  ERRB1(val,"set-" S_trap_segfault); 
  set_trap_segfault(state,bool_int_or_one(val));
  RTNBOOL(trap_segfault(state));
}

static SCM g_show_selection_transform(void) {RTNBOOL(show_selection_transform(state));}
static SCM g_set_show_selection_transform(SCM val) 
{
  #define H_show_selection_transform "(" S_show_selection_transform ") -> #t if transform display reflects selection, not time-domain window"
  ERRB1(val,"set-" S_show_selection_transform); 
  set_show_selection_transform(state,bool_int_or_one(val));
  RTNBOOL(show_selection_transform(state));
}

static SCM g_with_mix_consoles(void) {RTNBOOL(with_mix_consoles(state));}
static SCM g_set_with_mix_consoles(SCM val) 
{
  #define H_with_mix_consoles "(" S_with_mix_consoles ") -> #t if Snd should create consoles alongside mixing operations"
  ERRB1(val,"set-" S_with_mix_consoles); 
  set_with_mix_consoles(state,bool_int_or_one(val));
  RTNBOOL(with_mix_consoles(state));
}

static SCM g_use_raw_defaults(void) {RTNBOOL(use_raw_defaults(state));}
static SCM g_set_use_raw_defaults(SCM val) 
{
  #define H_use_raw_defaults "(" S_use_raw_defaults ") -> #t if Snd should simply use the raw-* defaults\n\
   when a headerless file is encountered. If #f, Snd fires up the raw file dialog."

  ERRB1(val,"set-" S_use_raw_defaults); 
  set_use_raw_defaults(state,bool_int_or_one(val));
  RTNBOOL(use_raw_defaults(state));
}

static SCM g_use_sinc_interp(void) {RTNBOOL(use_sinc_interp(state));}
static SCM g_set_use_sinc_interp(SCM val) 
{
  #define H_use_sinc_interp "(" S_use_sinc_interp ") -> #t if Snd should use convolution with sinc for sampling rate\n\
   conversion.  The other choice is (much faster) linear interpolation which can introduce distortion"

  ERRB1(val,"set-" S_use_sinc_interp); 
  set_use_sinc_interp(state,bool_int_or_one(val));
  RTNBOOL(use_sinc_interp(state));
}

static SCM g_data_clipped(void) {RTNBOOL(data_clipped(state));}
static SCM g_set_data_clipped(SCM val) 
{
  #define H_data_clipped "(" S_data_clipped ") -> #t if Snd should clip output values to the current\n\
   output data format's maximum. The default (#f) allows them to wrap-around which makes a very loud click"

  ERRB1(val,"set-" S_data_clipped); 
  set_data_clipped(state,bool_int_or_one(val));
  RTNBOOL(data_clipped(state));
}

static SCM g_vu_font(void) {RTNSTR(vu_font(state));}
static SCM g_set_vu_font(SCM val) 
{
  #define H_vu_font "(" S_vu_font ") -> name of font used to make VU meter labels (courier)"
  SCM_ASSERT(gh_string_p(val),val,SCM_ARG1,"set-" S_vu_font); 
  set_vu_font(state,gh_scm2newstr(val,0));
  RTNSTR(vu_font(state));
}

static SCM g_vu_font_size(void) {RTNFLT(vu_font_size(state));}
static SCM g_set_vu_font_size(SCM val) 
{
  #define H_vu_font_size "(" S_vu_font_size ") -> size of VU font meter labels (1.0)"
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(val)),val,SCM_ARG1,"set-" S_vu_font_size); 
  set_vu_font_size(state,gh_scm2double(val));
  RTNFLT(vu_font_size(state));
}

static SCM g_vu_size(void) {RTNFLT(vu_size(state));}
static SCM g_set_vu_size(SCM val) 
{
  #define H_vu_size "(" S_vu_size ") -> size of VU meters (1.0)"
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(val)),val,SCM_ARG1,"set-" S_vu_size); 
  set_vu_size(state,gh_scm2double(val));
  RTNFLT(vu_size(state));
}

static SCM g_x_axis_style(void) {RTNINT(x_axis_style(state));}
static SCM g_set_x_axis_style(SCM val) 
{
  #define H_x_axis_style "(" S_x_axis_style ") -> labelling of time domain x axis (x-in-seconds)"
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(val)),val,SCM_ARG1,"set-" S_x_axis_style); 
  set_x_axis_style(state,iclamp(X_IN_SECONDS,g_scm2int(val),X_IN_LENGTH));
  RTNINT(x_axis_style(state));
}

static SCM g_zoom_focus_style(void) {RTNINT(zoom_focus_style(state));}
static SCM g_set_zoom_focus_style(SCM focus) 
{
  #define H_zoom_focus_style "(" S_zoom_focus_style ") -> one of '(" S_focus_left " " S_focus_right " " S_focus_middle " " S_focus_active ")\n\
  decides what zooming centers on (default: " S_focus_active ")"
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(focus)),focus,SCM_ARG1,"set-" S_zoom_focus_style); 
  activate_focus_menu(state,iclamp(FOCUS_LEFT,g_scm2int(focus),FOCUS_MIDDLE));
  RTNINT(zoom_focus_style(state));
}

static SCM g_graph2ps(void) 
{
  #define H_graph2ps "(" S_graph_ps ") writes the current Snd displays to an EPS file"
  snd_print(state,eps_file(state)); 
  RTNSTR(eps_file(state));
}

static SCM g_snd_version(void) 
{
  #define H_snd_version "(" S_snd_version ") -> current Snd version"
  RTNSTR(SND_VERSION);
}

static SCM g_save_state(SCM filename) 
{
  #define H_save_state "(" S_save_state " filename) saves the current Snd state in filename; (load filename) restores it)"
  if (save_state(state,gh_scm2newstr(filename,NULL)) == -1) 
    return(SCM_BOOL_F); 
  else return(filename);
}

static SCM g_max_sounds(void) 
{
  #define H_max_sounds "(" S_max_sounds ") -> max sound id currently possible (grows as necessary)"
  RTNINT(state->max_sounds);
}

static SCM g_sounds(void)
{
  #define H_sounds "(" S_sounds ") -> list of active sounds (ids)"
  int i;
  snd_state *ss;
  snd_info *sp;
  SCM result;
  ss = get_global_state();
  result = SCM_EOL;
  for (i=0;i<ss->max_sounds;i++)
    {
      sp=((snd_info *)(ss->sounds[i]));
      if ((sp) && (sp->inuse))
	result = gh_cons(gh_int2scm(i),result);
    }
  return(result);
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

static SCM g_help_text_font(void) {RTNSTR(help_text_font(state));}
static SCM g_set_help_text_font(SCM val) 
{
  #define H_help_text_font "(" S_help_text_font ") -> font used in the Help dialog"
  SCM_ASSERT(gh_string_p(val),val,SCM_ARG1,"set-" S_help_text_font); 
  set_help_text_font(state,gh_scm2newstr(val,0)); 
  return(val);
}

static SCM g_tiny_font(void) {RTNSTR(tiny_font(state));}
static SCM g_set_tiny_font(SCM val) 
{
  #define H_tiny_font "(" S_tiny_font ") -> font use for some info in the graphs"
  SCM_ASSERT(gh_string_p(val),val,SCM_ARG1,"set-" S_tiny_font); 
  set_tiny_font(state,gh_scm2newstr(val,0)); 
  return(val);
}

static SCM g_axis_label_font(void) {RTNSTR(axis_label_font(state));}
static SCM g_set_axis_label_font(SCM val) 
{
  #define H_axis_label_font "(" S_axis_label_font ") -> font used for axis labels"
  SCM_ASSERT(gh_string_p(val),val,SCM_ARG1,"set-" S_axis_label_font); 
  set_axis_label_font(state,gh_scm2newstr(val,0)); 
  return(val);
}

static SCM g_axis_numbers_font(void) {RTNSTR(axis_numbers_font(state));}
static SCM g_set_axis_numbers_font(SCM val) 
{
  #define H_axis_numbers_font "(" S_axis_numbers_font ") -> font used for axis numbers"
  SCM_ASSERT(gh_string_p(val),val,SCM_ARG1,"set-" S_axis_numbers_font); 
  set_axis_numbers_font(state,gh_scm2newstr(val,0)); 
  return(val);
}

static SCM g_listener_font(void) {RTNSTR(listener_font(state));}
static SCM g_set_listener_font(SCM val) 
{
  #define H_listener_font "(" S_listener_font ") -> font used by the lisp listener"
  SCM_ASSERT(gh_string_p(val),val,SCM_ARG1,"set-" S_listener_font);
  set_listener_font(state,gh_scm2newstr(val,0)); 
  return(val);
}

static SCM g_bold_button_font(void) {RTNSTR(bold_button_font(state));}
static SCM g_set_bold_button_font(SCM val) 
{
  #define H_bold_button_font "(" S_bold_button_font ") -> font used by some buttons"
  SCM_ASSERT(gh_string_p(val),val,SCM_ARG1,"set-" S_bold_button_font); 
  set_bold_button_font(state,gh_scm2newstr(val,0)); 
  return(val);
}

static SCM g_button_font(void) {RTNSTR(button_font(state));}
static SCM g_set_button_font(SCM val) 
{
  #define H_button_font "(" S_button_font ") -> font used by some buttons"
  SCM_ASSERT(gh_string_p(val),val,SCM_ARG1,"set-" S_button_font); 
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
  #define H_set_window_height "(" "set-" S_window_height " val) sets the Snd window height in pixels"
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(height)),height,SCM_ARG1,"set-" S_window_height); 
  set_widget_height(MAIN_SHELL(state),g_scm2int(height));
  state->init_window_height = g_scm2int(height);
  return(height);
}

static SCM g_set_window_width(SCM width) 
{
  #define H_set_window_width "(" "set-" S_window_width " val) sets the Snd window width in pixels"
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(width)),width,SCM_ARG1,"set-" S_window_width); 
  set_widget_width(MAIN_SHELL(state),g_scm2int(width)); 
  state->init_window_width = g_scm2int(width);
  return(width);
}

static SCM g_set_window_x(SCM val) 
{
  #define H_set_window_x "(" "set-" S_window_x " val) sets the Snd window x position in pixels"
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(val)),val,SCM_ARG1,"set-" S_window_x); 
  set_widget_x(MAIN_SHELL(state),g_scm2int(val));
  state->init_window_x = g_scm2int(val); 
  return(val);
}

static SCM g_set_window_y(SCM val) 
{
  #define H_set_window_y "(" "set-" S_window_y " val) sets the Snd window y position in pixels"
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(val)),val,SCM_ARG1,"set-" S_window_y); 
  set_widget_y(MAIN_SHELL(state),g_scm2int(val)); 
  state->init_window_y = g_scm2int(val); 
  return(val);
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
      if (snd_n >= 0)
	{
	  if ((snd_n < state->max_sounds) && (snd_ok(state->sounds[snd_n])))
	    return(state->sounds[snd_n]);
	  else return(NULL);
	}
      else return(player(snd_n));
      return(NULL);
    }
  else
    {
      if (gh_list_p(scm_snd_n))
	{
	  /* a mix input sound */
	  snd_n = g_scm2int(SCM_CAR(scm_snd_n));
	  md = md_from_int(snd_n);
	  if (md) return(make_mix_readable(md));
	  return(NULL);
	}
    }
  /* use default sound, if any */
  return(any_selected_sound(state));
}

chan_info *get_cp(SCM scm_snd_n, SCM scm_chn_n, char *caller)
{
  snd_info *sp;
  int chn_n;
  sp = get_sp(scm_snd_n);
  if (sp == NULL) {scm_throw(NO_SUCH_SOUND,SCM_LIST2(gh_str02scm(caller),scm_snd_n)); return(NULL);}
  if (gh_number_p(scm_chn_n))
    chn_n = g_scm2int(scm_chn_n);
  else
    if (sp->selected_channel != NO_SELECTION) 
      chn_n = sp->selected_channel;
    else chn_n = 0;
  if ((chn_n >= 0) && (chn_n < sp->nchans)) 
    return(sp->chans[chn_n]);
  scm_throw(NO_SUCH_CHANNEL,SCM_LIST3(gh_str02scm(caller),scm_snd_n,scm_chn_n));
  return(NULL);
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
  /* TODO: fix this so it throws an error! */
}

static SCM g_close_sound_file(SCM g_fd, SCM g_bytes)
{
  #define H_close_sound_file "(" S_close_sound_file " fd bytes) closes file pointed to by fd, updating its header to report 'bytes' bytes of data"
  file_info *hdr;
  int result,fd,bytes;
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(g_fd)),g_fd,SCM_ARG1,S_close_sound_file);
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(g_bytes)),g_bytes,SCM_ARG2,S_close_sound_file);
  fd = g_scm2int(g_fd);
  bytes = g_scm2int(g_bytes);
  hdr = get_temp_header(fd);
  if (hdr == NULL) 
    {
      snd_error("can't find %d's header!",fd);
      close(fd);
      /* TODO: fix this so it throws an error! */
      return(SCM_BOOL_F);
    }
  else
    {
      result = close_temp_file(fd,hdr,bytes,any_selected_sound(state));
      unset_temp_fd(fd);
      return(gh_int2scm(result));
    }
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
  cp = get_cp(snd_n,chn_n,S_samples_vct);
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
  cp = get_cp(snd_n,chn_n,S_samples2sound_data);
  edpos = g_scm2intdef(pos,cp->edit_ctr);
  if (edpos >= cp->edit_size) 
    return(scm_throw(NO_SUCH_EDIT,SCM_LIST4(gh_str02scm(S_samples2sound_data),snd_n,chn_n,pos)));
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
  cp = get_cp(snd_n,chn_n,S_transform_samples_vct);
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

static SCM vct2soundfile(SCM g_fd, SCM obj, SCM g_nums)
{
  #define H_vct_sound_file "(" S_vct_sound_file " fd vct-obj samps) writes samps samples from vct-obj to the sound file controlled by fd"
  int fd,nums,i;
  float *vals;
  vct *v;
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(g_fd)),g_fd,SCM_ARG1,S_vct_sound_file);
  SCM_ASSERT((vct_p(obj)),obj,SCM_ARG2,S_vct_sound_file);
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(g_nums)),g_nums,SCM_ARG3,S_vct_sound_file);
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
  SCM_ASSERT(vct_p(obj),obj,SCM_ARG1,S_mix_vct);
  ERRCP(S_mix_vct,snd,chn,4);
  ERRB2(beg,S_mix_vct);
  ERRB3(in_chans,S_mix_vct);
  SCM_ASSERT((bool_or_arg_p(with_consoles)),with_consoles,SCM_ARG6,S_mix_vct);
  v = get_vct(obj);
  if (v)
    {
      len = v->length;
      cp[0] = get_cp(snd,chn,S_mix_vct);
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
  SCM lst;
  if (gh_list_p(obj))
    {
      if ((*size) == 0) num = gh_length(obj); else num = (*size);
      vals = (MUS_SAMPLE_TYPE *)CALLOC(num,sizeof(MUS_SAMPLE_TYPE));
      for (i=0,lst=obj;i<num;i++,lst=SCM_CDR(lst)) 
	vals[i] = MUS_FLOAT_TO_SAMPLE(gh_scm2double(SCM_CAR(lst)));
    }
  else
    {
      if (gh_vector_p(obj))
	{
	  if ((*size) == 0) num = gh_vector_length(obj); else num = (*size);
	  vals = (MUS_SAMPLE_TYPE *)CALLOC(num,sizeof(MUS_SAMPLE_TYPE));
	  for (i=0;i<num;i++) 
	    vals[i] = MUS_FLOAT_TO_SAMPLE(gh_scm2double(gh_vector_ref(obj,gh_int2scm(i))));
	}
      else
	{
	  if (vct_p(obj))
	    {
	      v = get_vct(obj);
	      if ((*size) == 0) num = v->length; else num = (*size);
	      vals = (MUS_SAMPLE_TYPE *)CALLOC(num,sizeof(MUS_SAMPLE_TYPE));
	      for (i=0;i<num;i++) 
		vals[i] = MUS_FLOAT_TO_SAMPLE(v->data[i]);
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
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(samp_n)),samp_n,SCM_ARG1,S_sample);
  ERRCP(S_sample,snd_n,chn_n,2);
  cp = get_cp(snd_n,chn_n,S_sample);
  RTNFLT(sample(g_scm2int(samp_n),cp));
}

static SCM g_set_sample(SCM samp_n, SCM val, SCM snd_n, SCM chn_n)
{
  /* each call consitutes a separate edit from the undo/redo point-of-view */
  chan_info *cp;
  MUS_SAMPLE_TYPE ival[1];
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(samp_n)),samp_n,SCM_ARG1,"set-" S_sample);
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(val)),val,SCM_ARG2,"set-" S_sample);
  ERRCP("set-" S_sample,snd_n,chn_n,3);
  cp = get_cp(snd_n,chn_n,"set-" S_sample);
  ival[0] = MUS_FLOAT_TO_SAMPLE(gh_scm2double(val));
  change_samples(g_scm2int(samp_n),1,ival,cp,LOCK_MIXES,"set-" S_sample);
  update_graph(cp,NULL);
  return(val);
}

static SCM g_set_sample_reversed(SCM arg1, SCM arg2, SCM arg3, SCM arg4)
{
  if (SCM_UNBNDP(arg2))
    return(g_set_sample(SCM_UNDEFINED,arg1,SCM_UNDEFINED,SCM_UNDEFINED));
  else
    {
      if (SCM_UNBNDP(arg3))
	return(g_set_sample(arg1,arg2,SCM_UNDEFINED,SCM_UNDEFINED));
      else 
	{
	  if (SCM_UNBNDP(arg4)) 
	    return(g_set_sample(arg1,arg3,arg2,SCM_UNDEFINED)); 
	  else return(g_set_sample(arg1,arg4,arg2,arg3));
	}
    }
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
  cp = get_cp(snd_n,chn_n,S_samples);
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

static SCM g_set_samples(SCM samp_0, SCM samps, SCM vect, SCM snd_n, SCM chn_n, SCM truncate)
{
  #define H_set_samples "(" "set-" S_samples " start-samp samps data &optional snd chn truncate) sets snd's channel chn's samples\n\
   starting at start-samp for samps from data (a vct, vector, or string (filename)); start-samp can be beyond current data end\n\
   if truncate is #t and start-samp is 0, the end of the file is set to match the new data's end"

  chan_info *cp;
  MUS_SAMPLE_TYPE *ivals;
  int len,beg,curlen,override=0;
  char *fname;
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(samp_0)),samp_0,SCM_ARG1,"set-" S_samples);
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(samps)),samps,SCM_ARG2,"set-" S_samples);
  ERRCP("set-" S_samples,snd_n,chn_n,4);
  cp = get_cp(snd_n,chn_n,"set-" S_samples);
  beg = g_scm2int(samp_0);
  len = g_scm2int(samps);
  override = SCM_TRUE_P(truncate);
  if (gh_string_p(vect))
    {
      curlen = current_ed_samples(cp);
      fname = gh_scm2newstr(vect,NULL);
      if ((beg == 0) && ((len > curlen) || override))
	file_override_samples(len,fname,cp,0,DELETE_ME,LOCK_MIXES,"set-" S_samples);
      else file_change_samples(beg,len,fname,cp,0,DELETE_ME,LOCK_MIXES,"set-" S_samples);
      free(fname);
    }
  else
    {
      ivals = g_floats_to_samples(vect,&len,"set-" S_samples,3);
      change_samples(beg,len,ivals,cp,LOCK_MIXES,"set-" S_samples);
      FREE(ivals);
    }
  update_graph(cp,NULL);
  return(vect);
}

static SCM g_set_samples_reversed(SCM arg1, SCM arg2, SCM arg3, SCM arg4, SCM arg5, SCM arg6)
{
  /* (set! (samples start samps [snd chn trunc]) vect) */
  if (SCM_UNBNDP(arg4))
    return(g_set_samples(arg1,arg2,arg3,SCM_UNDEFINED,SCM_UNDEFINED,SCM_UNDEFINED));
  else
    {
      if (SCM_UNBNDP(arg5))
	return(g_set_samples(arg1,arg2,arg4,arg3,SCM_UNDEFINED,SCM_UNDEFINED));
      else 
	{
	  if (SCM_UNBNDP(arg6)) 
	    return(g_set_samples(arg1,arg2,arg5,arg3,arg4,SCM_UNDEFINED));
	  else return(g_set_samples(arg1,arg2,arg6,arg3,arg4,arg5));
	}
    }
}

static SCM g_change_samples_with_origin(SCM samp_0, SCM samps, SCM origin, SCM vect, SCM snd_n, SCM chn_n)
{
  chan_info *cp;
  MUS_SAMPLE_TYPE *ivals;
  char *str,*fstr;
  int i,len,beg;
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(samp_0)),samp_0,SCM_ARG1,S_change_samples_with_origin);
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(samps)),samps,SCM_ARG2,S_change_samples_with_origin);
  SCM_ASSERT(gh_string_p(origin),origin,SCM_ARG3,S_change_samples_with_origin);
  SCM_ASSERT((gh_vector_p(vect)) || (gh_string_p(vect)),vect,SCM_ARG4,S_change_samples_with_origin);
  ERRCP(S_change_samples_with_origin,snd_n,chn_n,5);
  cp = get_cp(snd_n,chn_n,S_change_samples_with_origin);
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
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(samp_n)),samp_n,SCM_ARG1,S_delete_sample);
  ERRCP(S_delete_sample,snd_n,chn_n,2);
  cp = get_cp(snd_n,chn_n,S_delete_sample);
  samp = g_scm2int(samp_n);
  if ((samp >= 0) && (samp <= current_ed_samples(cp)))
    {
      delete_samples(samp,1,cp,S_delete_sample);
      update_graph(cp,NULL);
      return(SCM_BOOL_T);
    }
  return(scm_throw(NO_SUCH_SAMPLE,SCM_LIST4(gh_str02scm(S_delete_sample),samp_n,snd_n,chn_n)));
}

static SCM g_delete_samples_1(SCM samp_n, SCM samps, SCM snd_n, SCM chn_n, char *origin)
{
  chan_info *cp;
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(samp_n)),samp_n,SCM_ARG1,origin);
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(samps)),samps,SCM_ARG2,origin);
  ERRCP(origin,snd_n,chn_n,3);
  cp = get_cp(snd_n,chn_n,origin);
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
  SCM_ASSERT(gh_string_p(origin),origin,SCM_ARG3,S_delete_samples_with_origin);
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
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(samp_n)),samp_n,SCM_ARG1,S_insert_sample);
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(val)),val,SCM_ARG2,S_insert_sample);
  ERRCP(S_insert_sample,snd_n,chn_n,3);
  cp = get_cp(snd_n,chn_n,S_insert_sample);
  beg = g_scm2int(samp_n);
  if (beg < 0) return(scm_throw(NO_SUCH_SAMPLE,SCM_LIST4(gh_str02scm(S_insert_sample),samp_n,snd_n,chn_n)));
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
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(samp)),samp,SCM_ARG1,S_insert_samples);
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(samps)),samps,SCM_ARG2,S_insert_samples);
  ERRCP(S_insert_samples,snd_n,chn_n,4);
  cp = get_cp(snd_n,chn_n,S_insert_samples);
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
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(samp)),samp,SCM_ARG1,S_insert_samples_with_origin);
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(samps)),samps,SCM_ARG2,S_insert_samples_with_origin);
  SCM_ASSERT(gh_string_p(origin),origin,SCM_ARG3,S_insert_samples_with_origin);
  SCM_ASSERT((gh_vector_p(vect)) || (gh_string_p(vect)),vect,SCM_ARG4,S_insert_samples_with_origin);
  ERRCP(S_insert_samples_with_origin,snd_n,chn_n,5);
  cp = get_cp(snd_n,chn_n,S_insert_samples_with_origin);
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

static SCM g_insert_sound(SCM file, SCM file_chn, SCM snd_n, SCM chn_n)
{
  #define H_insert_sound "(" S_insert_sound " file &optional (file-chan 0) snd chn) inserts channel 'file-chan'\n\
   of 'file' into snd's channel chn at the cursor position"

  chan_info *cp;
  char *filename = NULL;
  int nc,len,fchn,beg=0;
  SCM_ASSERT(gh_string_p(file),file,SCM_ARG1,S_insert_sound);
  ERRB2(file_chn,S_insert_sound);
  ERRCP(S_insert_sound,snd_n,chn_n,3);
  cp = get_cp(snd_n,chn_n,S_insert_sound);
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
      return(scm_throw(NO_SUCH_FILE,SCM_LIST2(gh_str02scm(S_insert_sound),file)));
    }
  if (filename) FREE(filename);
  return(SCM_BOOL_F);
}

#if 0
Float string2Float(char *str) 
{
  SCM res;
  res = snd_internal_stack_catch(SCM_BOOL_T,eval_str_wrapper,str,snd_catch_scm_error,str);
  if (gh_number_p(res))
    return(gh_scm2double(res));
  else snd_error("%s is not a number",str);
  return(0.0);
}
#endif

int string2int(char *str) 
{
  SCM res;
  res = snd_internal_stack_catch(SCM_BOOL_T,eval_str_wrapper,str,snd_catch_scm_error,str);
  if (gh_number_p(res))
    return(g_scm2int(res));
  else snd_error("%s is not a number",str);
  return(0);
}

#if 0
char *string2string(char *str) 
{
  SCM res;
  res = snd_internal_stack_catch(SCM_BOOL_T,eval_str_wrapper,str,snd_catch_scm_error,str);
  if (gh_string_p(res))
    return(gh_scm2newstr(res,NULL));
  else snd_error("%s is not a string",str);
  return(str);
}
#endif

static SCM g_update_graph(SCM snd, SCM chn) 
{
  #define H_update_graph "(" S_update_graph " &optional snd chn) redraws snd channel chn's graphs"
  chan_info *cp;
  ERRCP(S_update_graph,snd,chn,1); 
  cp = get_cp(snd,chn,S_update_graph);
  update_graph(cp,NULL);
  return(SCM_BOOL_F);
}

static SCM g_update_fft(SCM snd, SCM chn) 
{
  #define H_update_fft "(" S_update_fft " &optional snd chn) recalculates snd channel chn's fft"
  chan_info *cp;
  ERRCP(S_update_fft,snd,chn,1); 
  cp = get_cp(snd,chn,S_update_fft);
  calculate_fft(cp,NULL);
  return(SCM_BOOL_F);
}

static SCM g_transform_size(SCM snd, SCM chn)
{
  #define H_transform_size "(" S_transform_size " &optional snd chn) -> description of transform data in snd's channel chn.\n\
   If no fft, returns 0; if normal-fft, returns fft-size, else returns a list (full-size active-bins active-slices)"

  chan_info *cp;
  sono_info *si;
  cp = get_cp(snd,chn,S_transform_size);
  if (!(cp->ffting)) return(SCM_INUM0);
  if (fft_style(state) == NORMAL_FFT) return(gh_int2scm(fft_size(state)));
  si = (sono_info *)(cp->sonogram_data);
  if (si) return(SCM_LIST3(gh_double2scm(spectro_cutoff(state)),
			   gh_int2scm(si->active_slices),
			   gh_int2scm(si->target_bins)));
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
  cp = get_cp(snd_n,chn_n,S_transform_sample);
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
	      else  return(scm_throw(NO_SUCH_SAMPLE,SCM_LIST5(gh_str02scm(S_transform_sample),bin,slice,snd_n,chn_n)));
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
  cp = get_cp(snd_n,chn_n,S_transform_samples);
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

static Float *load_Floats(SCM scalers, int *result_len)
{
  int len,i;
  Float *scls;
  SCM lst;
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
      for (i=0;i<len;i++) 
	scls[i] = (Float)gh_scm2double(gh_vector_ref(scalers,gh_int2scm(i)));
    }
  else
    if (gh_list_p(scalers))
      {
	for (i=0,lst=scalers;i<len;i++,lst=SCM_CDR(lst)) 
	  scls[i] = (Float)gh_scm2double(SCM_CAR(lst));
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
  chan_info *cp;
  int len[1];
  Float *scls;
  ERRCP(S_scale_to,snd_n,chn_n,2);
  cp = get_cp(snd_n,chn_n,S_scale_to);
  scls = load_Floats(scalers,len);
  scale_to(state,cp->sound,cp,scls,len[0],FALSE); /* last arg for selection */
  FREE(scls);
  return(scalers);
}

static SCM g_scale_by(SCM scalers, SCM snd_n, SCM chn_n)
{
  #define H_scale_by "(" S_scale_by " scalers &optional snd chn) scales snd by scalers (following sync)\n\
   scalers can be a float or a vector of floats"

  /* chn_n irrelevant if syncing */
  chan_info *cp;
  int len[1];
  Float *scls;
  ERRCP(S_scale_by,snd_n,chn_n,2);
  cp = get_cp(snd_n,chn_n,S_scale_by);
  scls = load_Floats(scalers,len);
  scale_by(state,cp->sound,cp,scls,len[0],FALSE);
  FREE(scls);
  return(scalers);
}

static SCM g_scale_selection_to(SCM scalers)
{
  #define H_scale_selection_to "(" S_scale_selection_to " norms &optional chn) normalizes current selected portion to norms"
  int len[1];
  Float *scls;
  if (selection_is_active())
    {
      scls = load_Floats(scalers,len);
      scale_to(state,NULL,NULL,scls,len[0],TRUE);
      FREE(scls);
      return(scalers);
    }
  return(scm_throw(NO_ACTIVE_SELECTION,SCM_LIST1(gh_str02scm(S_scale_selection_to))));
}

static SCM g_scale_selection_by(SCM scalers)
{
  #define H_scale_selection_by "(" S_scale_selection_by " scalers &optional chn) scales current selected portion by scalers"
  int len[1];
  Float *scls;
  if (selection_is_active())
    {
      scls = load_Floats(scalers,len);
      scale_by(state,NULL,NULL,scls,len[0],TRUE);
      FREE(scls);
      return(scalers);
    }
  return(scm_throw(NO_ACTIVE_SELECTION,SCM_LIST1(gh_str02scm(S_scale_selection_by))));
}

static SCM g_help_dialog(SCM subject, SCM msg)
{
  #define H_help_dialog "(" S_help_dialog " subject message) fires up the Help window with subject and message"
  char *nmsg,*nsubj;
  SCM_ASSERT(gh_string_p(subject),subject,SCM_ARG1,S_help_dialog);
  SCM_ASSERT(gh_string_p(msg),msg,SCM_ARG2,S_help_dialog);
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
  if (sp) edit_header(sp); else return(scm_throw(NO_SUCH_SOUND,SCM_LIST2(gh_str02scm(S_edit_header_dialog),snd_n)));
  return(SCM_BOOL_F);
}

static SCM g_yes_or_no_p(SCM msg) 
{
  #define H_yes_or_no_p "(" S_yes_or_no_p " message) displays message and waits for 'y' or 'n'; returns #t if 'y'"
  RTNBOOL(snd_yes_or_no_p(state,gh_scm2newstr(msg,NULL)));
}


#if HAVE_OSS
static SCM g_clear_audio_inputs (void) 
{
  #define H_clear_audio_inputs "(" S_clear_audio_inputs ") tries to reduce soundcard noise in Linux/OSS"
  mus_audio_clear_soundcard_inputs(); 
  return(SCM_BOOL_F);
}
#endif

static SCM g_env_selection(SCM edata, SCM base, SCM snd_n, SCM chn_n)
{
  #define H_env_selection "(" S_env_selection " env &optional (env-base 1.0) snd chn) applies envelope 'env'\n\
   to the currently selected portion of snd's channel chn using 'env-base' to determine how breakpoints are connected"

  chan_info *cp;
  env *e;
  ERRCP(S_env_selection,snd_n,chn_n,3);
  if (selection_is_active() == 0) return(scm_throw(NO_ACTIVE_SELECTION,SCM_LIST1(gh_str02scm(S_env_selection))));
  cp = get_cp(snd_n,chn_n,S_env_selection);
  e = get_env(edata,base,S_env_selection);
  if (e)
    {
      apply_env(cp,e,0,0,1.0,TRUE,NOT_FROM_ENVED,S_env_selection);
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
  cp = get_cp(snd_n,chn_n,S_env_sound);
  e = get_env(edata,base,S_env_sound);
  if (e)
    {
      dur = g_scm2intdef(samps,0);
      if (dur == 0) dur = current_ed_samples(cp);
      apply_env(cp,e,g_scm2intdef(samp_n,0),dur,1.0,FALSE,NOT_FROM_ENVED,S_env_sound);
      free_env(e);
      return(SCM_BOOL_T);
    }
  return(SCM_BOOL_F);
}

static SCM g_fft_1(SCM reals, SCM imag, SCM sign, int use_fft)
{
  vct *v1 = NULL,*v2 = NULL;
  int ipow,n,n2,i,isign = 1;
  Float *rl,*im;
  SCM_ASSERT(((vct_p(reals)) || (gh_vector_p(reals))),reals,SCM_ARG1,((use_fft) ? S_fft : S_convolve_arrays));
  SCM_ASSERT(((vct_p(imag)) || (gh_vector_p(imag))),imag,SCM_ARG2,((use_fft) ? S_fft : S_convolve_arrays));
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
	    gh_vector_set_x(reals,gh_int2scm(i),gh_double2scm(rl[i]));
	}
      else
	{
	  if (n != n2)
	    for (i=0;i<n;i++) 
	      v1->data[i] = rl[i];
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
  SCM_ASSERT(gh_string_p(file),file,SCM_ARG1,S_convolve_with);
  ERRCP(S_convolve_with,snd_n,chn_n,3);
  cp = get_cp(snd_n,chn_n,S_convolve_with);
  if (gh_number_p(new_amp)) 
    amp = gh_scm2double(new_amp);
  else
    {
      if (SCM_FALSEP(new_amp))
	amp = 0.0;
      else amp = 1.0;
    }
  fname = full_filename(file);
  if (mus_file_probe(fname))
    convolve_with(fname,amp,cp);
  else return(scm_throw(NO_SUCH_FILE,SCM_LIST2(gh_str02scm(S_convolve_with),file)));
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
  SCM_ASSERT(gh_string_p(file),file,SCM_ARG1,S_convolve_selection_with);
  if (selection_is_active() == 0) return(scm_throw(NO_ACTIVE_SELECTION,SCM_LIST1(gh_str02scm(S_convolve_selection_with))));
  if (gh_number_p(new_amp)) 
    amp = gh_scm2double(new_amp);
  else
    {
      if (SCM_FALSEP(new_amp))
	amp = 0.0;
      else amp = 1.0;
    }
  fname = full_filename(file);
  if (mus_file_probe(fname))
    convolve_with(fname,amp,NULL);
  else return(scm_throw(NO_SUCH_FILE,SCM_LIST2(gh_str02scm(S_convolve_selection_with),file)));
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
  cp = get_cp(snd_n,chn_n,S_src_sound);
  if (gh_number_p(ratio_or_env))
    src_env_or_num(state,cp,NULL,gh_scm2double(ratio_or_env),TRUE,NOT_FROM_ENVED,S_src_sound,FALSE);
  else src_env_or_num(state,cp,get_env(ratio_or_env,base,S_src_sound),1.0,FALSE,NOT_FROM_ENVED,S_src_sound,FALSE);
  return(ratio_or_env);
}

static SCM g_src_selection(SCM ratio_or_env, SCM base)
{
  #define H_src_selection "(" S_src_selection " ratio-or-env &optional (base 1.0)) sampling-rate converts the\n\
   currently selected data by ratio (which can be an envelope)"

  chan_info *cp;
  if (selection_is_active() == 0) return(scm_throw(NO_ACTIVE_SELECTION,SCM_LIST1(gh_str02scm(S_src_selection))));
  cp = get_cp(SCM_BOOL_F,SCM_BOOL_F,S_src_selection);
  if (gh_number_p(ratio_or_env))
    src_env_or_num(state,cp,NULL,gh_scm2double(ratio_or_env),TRUE,NOT_FROM_ENVED,S_src_selection,TRUE);
  else src_env_or_num(state,cp,get_env(ratio_or_env,base,S_src_selection),1.0,FALSE,NOT_FROM_ENVED,S_src_selection,TRUE);
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
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(order)),order,SCM_ARG2,S_filter_sound);
  ERRCP(S_filter_sound,snd_n,chn_n,3);
  cp = get_cp(snd_n,chn_n,S_filter_sound);
  len = g_scm2int(order);
#if HAVE_GUILE_1_4
  if (len <= 0) scm_out_of_range_pos(S_filter_sound,order,SCM_MAKINUM(2));
#endif
  if (vct_p(e)) /* the filter coefficients direct */
    {
      v = get_vct(e);
#if HAVE_GUILE_1_4
      if (len > v->length) scm_out_of_range_pos(S_filter_sound,order,SCM_MAKINUM(2));
#endif
      apply_filter(cp,len,NULL,NOT_FROM_ENVED,S_filter_sound,FALSE,v->data);
    }
  else apply_filter(cp,len,get_env(e,gh_double2scm(1.0),S_filter_sound),NOT_FROM_ENVED,S_filter_sound,FALSE,NULL); 
  return(scm_return_first(SCM_BOOL_T,e));
}

static SCM g_filter_selection(SCM e, SCM order)
{
  #define H_filter_selection "(" S_filter_selection " filter order) applies filter to current selection"
  chan_info *cp;
  vct *v;
  int len;
  SCM_ASSERT((gh_vector_p(e)) || (gh_list_p(e)) || (vct_p(e)),e,SCM_ARG1,S_filter_selection);
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(order)),order,SCM_ARG2,S_filter_selection);
  if (selection_is_active() == 0) return(scm_throw(NO_ACTIVE_SELECTION,SCM_LIST1(gh_str02scm(S_filter_selection))));
  cp = get_cp(SCM_BOOL_F,SCM_BOOL_F,S_filter_selection);
  len = g_scm2int(order);
#if HAVE_GUILE_1_4
  if (len <= 0) scm_out_of_range_pos(S_filter_selection,order,SCM_MAKINUM(2));
#endif
  if (vct_p(e)) /* the filter coefficients direct */
    {
      v = get_vct(e);
#if HAVE_GUILE_1_4
      if (len > v->length) scm_out_of_range_pos(S_filter_sound,order,SCM_MAKINUM(2));
#endif
      apply_filter(cp,len,NULL,NOT_FROM_ENVED,S_filter_selection,TRUE,v->data);
    }
  else apply_filter(cp,len,get_env(e,gh_double2scm(1.0),S_filter_selection),NOT_FROM_ENVED,S_filter_selection,TRUE,NULL); 
  return(scm_return_first(SCM_BOOL_T,e));
}

#if 0
/* direct call like this is about twice as fast as going through Scheme */
static Float getns(void *arg, int dir) {MUS_SAMPLE_TYPE val; NEXT_SAMPLE(val,((snd_fd *)arg)); return(MUS_SAMPLE_TO_FLOAT(val));}
static SCM g_pv(void)
{
  mus_any *pv;
  snd_fd *sf;
  chan_info *cp;
  int i,len;
  MUS_SAMPLE_TYPE *data;
  cp = get_cp(SCM_UNSPECIFIED,SCM_UNSPECIFIED,"pv");
  sf = init_sample_read(0,cp,READ_FORWARD);
  pv = mus_make_phase_vocoder(NULL,512,4,128,0.5,NULL,NULL,NULL,(void *)sf);
  len = current_ed_samples(cp);
  data = (MUS_SAMPLE_TYPE *)CALLOC(len,sizeof(MUS_SAMPLE_TYPE));
  for (i=0;i<len;i++) data[i] = mus_phase_vocoder(pv,&getns);
  change_samples(0,len,data,cp,0,"pv");
  free_snd_fd(sf);
  return(SCM_BOOL_F);
}
#endif
  
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
  cp = get_cp(snd_n,chn_n,S_graph);
  ymin = 32768.0;
  ymax = -32768.0;
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
  display_channel_lisp_data(cp,cp->sound,cp->state);
  return(scm_return_first(SCM_BOOL_F,data));
}

static SCM g_set_oss_buffers(SCM num, SCM size)
{
  #define H_set_oss_buffers "(" "set-" S_oss_buffers " num size) sets Linux OSS 'fragment' number and size"
#if (HAVE_OSS || HAVE_ALSA)
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(num)),num,SCM_ARG1,"set-" S_oss_buffers);
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(size)),size,SCM_ARG2,"set-" S_oss_buffers);
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

#if HAVE_GUILE_1_3_0
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
#endif


static SCM g_start_progress_report(SCM snd)
{
  #define H_start_progress_report "(" S_start_progress_report " &optional snd) posts the hour-glass icon"
  snd_info *sp;
  ERRB1(snd,S_start_progress_report);
  ERRSP(S_start_progress_report,snd,1);
  sp = get_sp(snd);
  if (sp)
    start_progress_report(sp,NOT_FROM_ENVED); 
  else return(scm_throw(NO_SUCH_SOUND,SCM_LIST2(gh_str02scm(S_start_progress_report),snd)));
  return(SCM_BOOL_T);
}

static SCM g_finish_progress_report(SCM snd)
{
  #define H_finish_progress_report "(" S_finish_progress_report " &optional snd) removes the hour-glass icon"
  snd_info *sp;
  ERRB1(snd,S_finish_progress_report);
  ERRSP(S_finish_progress_report,snd,1);
  sp = get_sp(snd);
  if (sp) 
    finish_progress_report(sp,NOT_FROM_ENVED); 
  else return(scm_throw(NO_SUCH_SOUND,SCM_LIST2(gh_str02scm(S_finish_progress_report),snd)));
  return(SCM_BOOL_T);
}

static SCM g_progress_report(SCM pct, SCM name, SCM cur_chan, SCM chans, SCM snd)
{
  #define H_progress_report "(" S_progress_report " pct &optional name cur-chan chans snd) updates an on-going\n\
   'progress report' (e. g. an animated hour-glass icon) in snd using pct to indicate how far along we are"

  snd_info *sp;
  char *str;
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(pct)),pct,SCM_ARG1,S_progress_report);
  ERRSP(S_progress_report,snd,5);
  sp = get_sp(snd);
  if (sp) 
    {
      if (gh_string_p(name)) str = gh_scm2newstr(name,NULL); else str = copy_string("something useful");
      progress_report(sp,str,
		      g_scm2intdef(cur_chan,0),
		      g_scm2intdef(chans,sp->nchans),
		      gh_scm2double(pct),
		      NOT_FROM_ENVED);
      free(str);
    }
  else return(scm_throw(NO_SUCH_SOUND,SCM_LIST2(gh_str02scm(S_progress_report),snd)));
  return(SCM_BOOL_T);
}

void init_mus2scm_module(void);

/* GOOPS */
/* #include <goops.h> */

static SCM during_open_hook,exit_hook,start_hook,after_open_hook;
static SCM output_comment_hook;
static SCM mix_console_state_changed_hook,mix_speed_changed_hook,mix_amp_changed_hook,mix_position_changed_hook;


void define_procedure_with_setter(char *get_name, SCM (*get_func)(), char *get_help,
				  char *set_name, SCM (*set_func)(), 
				  SCM local_doc,
				  int get_req, int get_opt, int set_req, int set_opt)
{
#if HAVE_GENERALIZED_SET
  scm_set_object_property_x(
			    gh_cdr(
				   gh_define(get_name,
					     scm_make_procedure_with_setter(gh_new_procedure("",SCM_FNC get_func,get_req,get_opt,0),
									    /* is this safe? -- I want an "unnamed function" */
									    gh_new_procedure(set_name,SCM_FNC set_func,set_req,set_opt,0)
									    /* set_name for backwards compatibility */
									    ))),
			    local_doc,
			    gh_str02scm(get_help));
  /* still need to trap help output and send it to the listener */
#else
  DEFINE_PROC(gh_new_procedure(get_name,SCM_FNC get_func,get_req,get_opt,0),get_help);
  gh_new_procedure(set_name,SCM_FNC set_func,set_req,set_opt,0);
#endif
}

void define_procedure_with_reversed_setter(char *get_name, SCM (*get_func)(), char *get_help,
					   char *set_name, SCM (*set_func)(), SCM (*reversed_set_func)(), 
					   SCM local_doc,
					   int get_req, int get_opt, int set_req, int set_opt)
{
#if HAVE_GENERALIZED_SET
  scm_set_object_property_x(
			    gh_cdr(
				   gh_define(get_name,
					     scm_make_procedure_with_setter(gh_new_procedure("",SCM_FNC get_func,get_req,get_opt,0),
									    gh_new_procedure("",SCM_FNC reversed_set_func,set_req,set_opt,0)
									    ))),
			    local_doc,
			    gh_str02scm(get_help));
  /* still need to trap help output and send it to the listener */
#else
  DEFINE_PROC(gh_new_procedure(get_name,SCM_FNC get_func,get_req,get_opt,0),get_help);
#endif
  gh_new_procedure(set_name,SCM_FNC set_func,set_req,set_opt,0);
}

#if FILE_PER_CHAN
  static SCM open_multifile_sound_hook,save_multifile_sound_hook;
#endif

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

  gh_define(S_show_all_axes,gh_int2scm(SHOW_ALL_AXES));
  gh_define(S_show_no_axes,gh_int2scm(SHOW_NO_AXES));
  gh_define(S_show_x_axis,gh_int2scm(SHOW_X_AXIS));

  gh_define(S_dont_normalize,gh_int2scm(DONT_NORMALIZE));
  gh_define(S_normalize_by_channel,gh_int2scm(NORMALIZE_BY_CHANNEL));
  gh_define(S_normalize_by_sound,gh_int2scm(NORMALIZE_BY_SOUND));
  gh_define(S_normalize_globally,gh_int2scm(NORMALIZE_GLOBALLY));



  /* ---------------- VARIABLES ---------------- */
  define_procedure_with_setter(S_ask_before_overwrite,SCM_FNC g_ask_before_overwrite,H_ask_before_overwrite,
			       "set-" S_ask_before_overwrite,SCM_FNC g_set_ask_before_overwrite,local_doc,0,0,0,1);

  define_procedure_with_setter(S_audio_output_device,SCM_FNC g_audio_output_device,H_audio_output_device,
			       "set-" S_audio_output_device,SCM_FNC g_set_audio_output_device,local_doc,0,0,0,1);

  define_procedure_with_setter(S_dac_size,SCM_FNC g_dac_size,H_dac_size,
			       "set-" S_dac_size,SCM_FNC g_set_dac_size,local_doc,0,0,0,1);

  define_procedure_with_setter(S_dac_folding,SCM_FNC g_dac_folding,H_dac_folding,
			       "set-" S_dac_folding,SCM_FNC g_set_dac_folding,local_doc,0,0,0,1);

  define_procedure_with_setter(S_auto_resize,SCM_FNC g_auto_resize,H_auto_resize,
			       "set-" S_auto_resize,SCM_FNC g_set_auto_resize,local_doc,0,0,0,1);

  define_procedure_with_setter(S_auto_update,SCM_FNC g_auto_update,H_auto_update,
			       "set-" S_auto_update,SCM_FNC g_set_auto_update,local_doc,0,0,0,1);

  define_procedure_with_setter(S_channel_style,SCM_FNC g_channel_style,H_channel_style,
			       "set-" S_channel_style,SCM_FNC g_set_channel_style,local_doc,0,0,0,1);

  define_procedure_with_setter(S_color_cutoff,SCM_FNC g_color_cutoff,H_color_cutoff,
			       "set-" S_color_cutoff,SCM_FNC g_set_color_cutoff,local_doc,0,0,0,1);

  define_procedure_with_setter(S_color_inverted,SCM_FNC g_color_inverted,H_color_inverted,
			       "set-" S_color_inverted,SCM_FNC g_set_color_inverted,local_doc,0,0,0,1);

  define_procedure_with_setter(S_color_scale,SCM_FNC g_color_scale,H_color_scale,
			       "set-" S_color_scale,SCM_FNC g_set_color_scale,local_doc,0,0,0,1);

  define_procedure_with_setter(S_corruption_time,SCM_FNC g_corruption_time,H_corruption_time,
			       "set-" S_corruption_time,SCM_FNC g_set_corruption_time,local_doc,0,0,0,1);

  define_procedure_with_setter(S_default_output_chans,SCM_FNC g_default_output_chans,H_default_output_chans,
			       "set-" S_default_output_chans,SCM_FNC g_set_default_output_chans,local_doc,0,0,0,1);

  define_procedure_with_setter(S_default_output_srate,SCM_FNC g_default_output_srate,H_default_output_srate,
			       "set-" S_default_output_srate,SCM_FNC g_set_default_output_srate,local_doc,0,0,0,1);

  define_procedure_with_setter(S_default_output_type,SCM_FNC g_default_output_type,H_default_output_type,
			       "set-" S_default_output_type,SCM_FNC g_set_default_output_type,local_doc,0,0,0,1);

  define_procedure_with_setter(S_default_output_format,SCM_FNC g_default_output_format,H_default_output_format,
			       "set-" S_default_output_format,SCM_FNC g_set_default_output_format,local_doc,0,0,0,1);

  define_procedure_with_setter(S_enved_base,SCM_FNC g_enved_base,H_enved_base,
			       "set-" S_enved_base,SCM_FNC g_set_enved_base,local_doc,0,0,0,1);

  define_procedure_with_setter(S_enved_power,SCM_FNC g_enved_power,H_enved_power,
			       "set-" S_enved_power,SCM_FNC g_set_enved_power,local_doc,0,0,0,1);

  define_procedure_with_setter(S_enved_clipping,SCM_FNC g_enved_clipping,H_enved_clipping,
			       "set-" S_enved_clipping,SCM_FNC g_set_enved_clipping,local_doc,0,0,0,1);

  define_procedure_with_setter(S_enved_exping,SCM_FNC g_enved_exping,H_enved_exping,
			       "set-" S_enved_exping,SCM_FNC g_set_enved_exping,local_doc,0,0,0,1);

  define_procedure_with_setter(S_enved_target,SCM_FNC g_enved_target,H_enved_target,
			       "set-" S_enved_target,SCM_FNC g_set_enved_target,local_doc,0,0,0,1);

  define_procedure_with_setter(S_enved_waving,SCM_FNC g_enved_waving,H_enved_waving,
			       "set-" S_enved_waving,SCM_FNC g_set_enved_waving,local_doc,0,0,0,1);

  define_procedure_with_setter(S_enved_dBing,SCM_FNC g_enved_dBing,H_enved_dBing,
			       "set-" S_enved_dBing,SCM_FNC g_set_enved_dBing,local_doc,0,0,0,1);

  define_procedure_with_setter(S_eps_file,SCM_FNC g_eps_file,H_eps_file,
			       "set-" S_eps_file,SCM_FNC g_set_eps_file,local_doc,0,0,0,1);

  define_procedure_with_setter(S_listener_prompt,SCM_FNC g_listener_prompt,H_listener_prompt,
			       "set-" S_listener_prompt,SCM_FNC g_set_listener_prompt,local_doc,0,0,0,1);

  define_procedure_with_setter(S_audio_state_file,SCM_FNC g_audio_state_file,H_audio_state_file,
			       "set-" S_audio_state_file,SCM_FNC g_set_audio_state_file,local_doc,0,0,0,1);

  define_procedure_with_setter(S_filter_env_order,SCM_FNC g_filter_env_order,H_filter_env_order,
			       "set-" S_filter_env_order,SCM_FNC g_set_filter_env_order,local_doc,0,0,0,1);

  define_procedure_with_setter(S_fit_data_on_open,SCM_FNC g_fit_data_on_open,H_fit_data_on_open,
			       "set-" S_fit_data_on_open,SCM_FNC g_set_fit_data_on_open,local_doc,0,0,0,1);

  define_procedure_with_setter(S_initial_x0,SCM_FNC g_initial_x0,H_initial_x0,
			       "set-" S_initial_x0,SCM_FNC g_set_initial_x0,local_doc,0,0,0,1);

  define_procedure_with_setter(S_initial_x1,SCM_FNC g_initial_x1,H_initial_x1,
			       "set-" S_initial_x1,SCM_FNC g_set_initial_x1,local_doc,0,0,0,1);

  define_procedure_with_setter(S_initial_y0,SCM_FNC g_initial_y0,H_initial_y0,
			       "set-" S_initial_y0,SCM_FNC g_set_initial_y0,local_doc,0,0,0,1);

  define_procedure_with_setter(S_initial_y1,SCM_FNC g_initial_y1,H_initial_y1,
			       "set-" S_initial_y1,SCM_FNC g_set_initial_y1,local_doc,0,0,0,1);

  define_procedure_with_setter(S_mix_console_amp_scaler,SCM_FNC g_mix_console_amp_scaler,H_mix_console_amp_scaler,
			       "set-" S_mix_console_amp_scaler,SCM_FNC g_set_mix_console_amp_scaler,local_doc,0,0,0,1);

  define_procedure_with_setter(S_mix_console_speed_scaler,SCM_FNC g_mix_console_speed_scaler,H_mix_console_speed_scaler,
			       "set-" S_mix_console_speed_scaler,SCM_FNC g_set_mix_console_speed_scaler,local_doc,0,0,0,1);

  define_procedure_with_setter(S_movies,SCM_FNC g_movies,H_movies,
			       "set-" S_movies,SCM_FNC g_set_movies,local_doc,0,0,0,1);

  define_procedure_with_setter(S_normalize_on_open,SCM_FNC g_normalize_on_open,H_normalize_on_open,
			       "set-" S_normalize_on_open,SCM_FNC g_set_normalize_on_open,local_doc,0,0,0,1);

  define_procedure_with_setter(S_prefix_arg,SCM_FNC g_prefix_arg,H_prefix_arg,
			       "set-" S_prefix_arg,SCM_FNC g_set_prefix_arg,local_doc,0,0,0,1);

  define_procedure_with_setter(S_print_length,SCM_FNC g_print_length,H_print_length,
			       "set-" S_print_length,SCM_FNC g_set_print_length,local_doc,0,0,0,1);

  define_procedure_with_setter(S_previous_files_sort,SCM_FNC g_previous_files_sort,H_previous_files_sort,
			       "set-" S_previous_files_sort,SCM_FNC g_set_previous_files_sort,local_doc,0,0,0,1);

  define_procedure_with_setter(S_raw_chans,SCM_FNC g_raw_chans,H_raw_chans,
			       "set-" S_raw_chans,SCM_FNC g_set_raw_chans,local_doc,0,0,0,1);

  define_procedure_with_setter(S_raw_format,SCM_FNC g_raw_format,H_raw_format,
			       "set-" S_raw_format,SCM_FNC g_set_raw_format,local_doc,0,0,0,1);

  define_procedure_with_setter(S_raw_srate,SCM_FNC g_raw_srate,H_raw_srate,
			       "set-" S_raw_srate,SCM_FNC g_set_raw_srate,local_doc,0,0,0,1);

  define_procedure_with_setter(S_save_state_on_exit,SCM_FNC g_save_state_on_exit,H_save_state_on_exit,
			       "set-" S_save_state_on_exit,SCM_FNC g_set_save_state_on_exit,local_doc,0,0,0,1);

  define_procedure_with_setter(S_show_indices,SCM_FNC g_show_indices,H_show_indices,
			       "set-" S_show_indices,SCM_FNC g_set_show_indices,local_doc,0,0,0,1);

  define_procedure_with_setter(S_show_usage_stats,SCM_FNC g_show_usage_stats,H_show_usage_stats,
			       "set-" S_show_usage_stats,SCM_FNC g_set_show_usage_stats,local_doc,0,0,0,1);

  define_procedure_with_setter(S_sinc_width,SCM_FNC g_sinc_width,H_sinc_width,
			       "set-" S_sinc_width,SCM_FNC g_set_sinc_width,local_doc,0,0,0,1);

  define_procedure_with_setter(S_colormap,SCM_FNC g_color_map,H_colormap,
			       "set-" S_colormap,SCM_FNC g_set_color_map,local_doc,0,0,0,1);

  define_procedure_with_setter(S_temp_dir,SCM_FNC g_temp_dir,H_temp_dir,
			       "set-" S_temp_dir,SCM_FNC g_set_temp_dir,local_doc,0,0,0,1);

  define_procedure_with_setter(S_save_dir,SCM_FNC g_save_dir,H_save_dir,
			       "set-" S_save_dir,SCM_FNC g_set_save_dir,local_doc,0,0,0,1);

  define_procedure_with_setter(S_trap_segfault,SCM_FNC g_trap_segfault,H_trap_segfault,
			       "set-" S_trap_segfault,SCM_FNC g_set_trap_segfault,local_doc,0,0,0,1);

  define_procedure_with_setter(S_show_selection_transform,SCM_FNC g_show_selection_transform,H_show_selection_transform,
			       "set-" S_show_selection_transform,SCM_FNC g_set_show_selection_transform,local_doc,0,0,0,1);

  define_procedure_with_setter(S_with_mix_consoles,SCM_FNC g_with_mix_consoles,H_with_mix_consoles,
			       "set-" S_with_mix_consoles,SCM_FNC g_set_with_mix_consoles,local_doc,0,0,0,1);

  define_procedure_with_setter(S_use_raw_defaults,SCM_FNC g_use_raw_defaults,H_use_raw_defaults,
			       "set-" S_use_raw_defaults,SCM_FNC g_set_use_raw_defaults,local_doc,0,0,0,1);

  define_procedure_with_setter(S_use_sinc_interp,SCM_FNC g_use_sinc_interp,H_use_sinc_interp,
			       "set-" S_use_sinc_interp,SCM_FNC g_set_use_sinc_interp,local_doc,0,0,0,1);

  define_procedure_with_setter(S_data_clipped,SCM_FNC g_data_clipped,H_data_clipped,
			       "set-" S_data_clipped,SCM_FNC g_set_data_clipped,local_doc,0,0,0,1);

  define_procedure_with_setter(S_vu_font,SCM_FNC g_vu_font,H_vu_font,
			       "set-" S_vu_font,SCM_FNC g_set_vu_font,local_doc,0,0,0,1);

  define_procedure_with_setter(S_vu_font_size,SCM_FNC g_vu_font_size,H_vu_font_size,
			       "set-" S_vu_font_size,SCM_FNC g_set_vu_font_size,local_doc,0,0,0,1);

  define_procedure_with_setter(S_vu_size,SCM_FNC g_vu_size,H_vu_size,
			       "set-" S_vu_size,SCM_FNC g_set_vu_size,local_doc,0,0,0,1);

  define_procedure_with_setter(S_window_x,SCM_FNC g_window_x,H_window_x,
			       "set-" S_window_x,SCM_FNC g_set_window_x,local_doc,0,0,0,1);

  define_procedure_with_setter(S_window_y,SCM_FNC g_window_y,H_window_y,
			       "set-" S_window_y,SCM_FNC g_set_window_y,local_doc,0,0,0,1);

  define_procedure_with_setter(S_x_axis_style,SCM_FNC g_x_axis_style,H_x_axis_style,
			       "set-" S_x_axis_style,SCM_FNC g_set_x_axis_style,local_doc,0,0,0,1);

  define_procedure_with_setter(S_zoom_focus_style,SCM_FNC g_zoom_focus_style,H_zoom_focus_style,
			       "set-" S_zoom_focus_style,SCM_FNC g_set_zoom_focus_style,local_doc,0,0,0,1);

  define_procedure_with_setter(S_help_text_font,SCM_FNC g_help_text_font,H_help_text_font,
			       "set-" S_help_text_font,SCM_FNC g_set_help_text_font,local_doc,0,0,0,1);

  define_procedure_with_setter(S_tiny_font,SCM_FNC g_tiny_font,H_tiny_font,
			       "set-" S_tiny_font,SCM_FNC g_set_tiny_font,local_doc,0,0,0,1);

  define_procedure_with_setter(S_button_font,SCM_FNC g_button_font,H_button_font,
			       "set-" S_button_font,SCM_FNC g_set_button_font,local_doc,0,0,0,1);

  define_procedure_with_setter(S_bold_button_font,SCM_FNC g_bold_button_font,H_bold_button_font,
			       "set-" S_bold_button_font,SCM_FNC g_set_bold_button_font,local_doc,0,0,0,1);

  define_procedure_with_setter(S_axis_label_font,SCM_FNC g_axis_label_font,H_axis_label_font,
			       "set-" S_axis_label_font,SCM_FNC g_set_axis_label_font,local_doc,0,0,0,1);

  define_procedure_with_setter(S_axis_numbers_font,SCM_FNC g_axis_numbers_font,H_axis_numbers_font,
			       "set-" S_axis_numbers_font,SCM_FNC g_set_axis_numbers_font,local_doc,0,0,0,1);

  define_procedure_with_setter(S_listener_font,SCM_FNC g_listener_font,H_listener_font,
			       "set-" S_listener_font,SCM_FNC g_set_listener_font,local_doc,0,0,0,1);

  define_procedure_with_setter(S_window_width,SCM_FNC g_window_width,H_window_width,
			       "set-" S_window_width,SCM_FNC g_set_window_width,local_doc,0,0,0,1);  

  define_procedure_with_setter(S_window_height,SCM_FNC g_window_height,H_window_height,
			       "set-" S_window_height,SCM_FNC g_set_window_height,local_doc,0,0,0,1);



  /* ---------------- FUNCTIONS ---------------- */

  DEFINE_PROC(gh_new_procedure(S_snd_tempnam,SCM_FNC g_snd_tempnam,0,0,0),H_snd_tempnam);
  DEFINE_PROC(gh_new_procedure("set-" S_oss_buffers,SCM_FNC g_set_oss_buffers,2,0,0),H_set_oss_buffers);
  DEFINE_PROC(gh_new_procedure(S_update_usage_stats,SCM_FNC g_update_usage_stats,0,0,0),H_update_usage_stats);
#if HAVE_OSS
  DEFINE_PROC(gh_new_procedure(S_clear_audio_inputs,SCM_FNC g_clear_audio_inputs,0,0,0),H_clear_audio_inputs);
#endif
  DEFINE_PROC(gh_new_procedure(S_enved_dialog,SCM_FNC g_enved_dialog,0,0,0),H_enved_dialog);
  DEFINE_PROC(gh_new_procedure(S_color_dialog,SCM_FNC g_color_dialog,0,0,0),H_color_dialog);
  DEFINE_PROC(gh_new_procedure(S_orientation_dialog,SCM_FNC g_orientation_dialog,0,0,0),H_orientation_dialog);
  DEFINE_PROC(gh_new_procedure(S_transform_dialog,SCM_FNC g_transform_dialog,0,0,0),H_transform_dialog);
  DEFINE_PROC(gh_new_procedure(S_file_dialog,SCM_FNC g_file_dialog,0,0,0),H_file_dialog);
  DEFINE_PROC(gh_new_procedure(S_edit_header_dialog,SCM_FNC g_edit_header_dialog,0,1,0),H_edit_header_dialog);
  DEFINE_PROC(gh_new_procedure(S_help_dialog,SCM_FNC g_help_dialog,2,0,0),H_help_dialog);

  define_procedure_with_reversed_setter(S_sample,SCM_FNC g_sample,H_sample,
					"set-" S_sample,SCM_FNC g_set_sample, SCM_FNC g_set_sample_reversed,
					local_doc,1,2,1,3);

  define_procedure_with_reversed_setter(S_samples,SCM_FNC g_samples,H_samples,
					"set-" S_samples,SCM_FNC g_set_samples, SCM_FNC g_set_samples_reversed,
					local_doc,2,3,3,3);

  DEFINE_PROC(gh_new_procedure(S_vct_samples,SCM_FNC g_set_samples,3,3,0),H_set_samples);
  DEFINE_PROC(gh_new_procedure(S_delete_sample,SCM_FNC g_delete_sample,1,2,0),H_delete_sample);
  DEFINE_PROC(gh_new_procedure(S_delete_samples,SCM_FNC g_delete_samples,2,2,0),H_delete_samples);
  DEFINE_PROC(gh_new_procedure(S_insert_sample,SCM_FNC g_insert_sample,2,2,0),H_insert_sample);
  DEFINE_PROC(gh_new_procedure(S_insert_samples,SCM_FNC g_insert_samples,3,2,0),H_insert_samples);
  DEFINE_PROC(gh_new_procedure(S_max_sounds,SCM_FNC g_max_sounds,0,0,0),H_max_sounds);
  DEFINE_PROC(gh_new_procedure(S_sounds,SCM_FNC g_sounds,0,0,0),H_sounds);
  DEFINE_PROC(gh_new_procedure(S_insert_sound,SCM_FNC g_insert_sound,1,3,0),H_insert_sound);
  DEFINE_PROC(gh_new_procedure(S_yes_or_no_p,SCM_FNC g_yes_or_no_p,1,0,0),H_yes_or_no_p);
  DEFINE_PROC(gh_new_procedure(S_scale_selection_to,SCM_FNC g_scale_selection_to,0,1,0),H_scale_selection_to);
  DEFINE_PROC(gh_new_procedure(S_scale_selection_by,SCM_FNC g_scale_selection_by,0,1,0),H_scale_selection_by);
  DEFINE_PROC(gh_new_procedure(S_update_graph,SCM_FNC g_update_graph,0,2,0),H_update_graph);
  DEFINE_PROC(gh_new_procedure(S_update_fft,SCM_FNC g_update_fft,0,2,0),H_update_fft);
  DEFINE_PROC(gh_new_procedure(S_exit,SCM_FNC g_exit,0,0,0),H_exit);
  DEFINE_PROC(gh_new_procedure(S_abort,SCM_FNC g_abort,0,0,0),H_abort);
  DEFINE_PROC(gh_new_procedure(S_dismiss_all_dialogs,SCM_FNC g_dismiss_all_dialogs,0,0,0),H_dismiss_all_dialogs);
  DEFINE_PROC(gh_new_procedure(S_abortQ,SCM_FNC g_abortq,0,0,0),H_abortQ);
  DEFINE_PROC(gh_new_procedure(S_snd_version,SCM_FNC g_snd_version,0,0,0),H_snd_version);
  DEFINE_PROC(gh_new_procedure(S_show_listener,SCM_FNC g_show_listener,0,0,0),H_show_listener);
  DEFINE_PROC(gh_new_procedure(S_hide_listener,SCM_FNC g_hide_listener,0,0,0),H_hide_listener);
  DEFINE_PROC(gh_new_procedure(S_activate_listener,SCM_FNC g_activate_listener,0,0,0),H_activate_listener);
  DEFINE_PROC(gh_new_procedure(S_graph_ps,SCM_FNC g_graph2ps,0,0,0),H_graph2ps);
  DEFINE_PROC(gh_new_procedure(S_save_state,SCM_FNC g_save_state,1,0,0),H_save_state);
  DEFINE_PROC(gh_new_procedure(S_scale_to,SCM_FNC g_scale_to,0,3,0),H_scale_to);
  DEFINE_PROC(gh_new_procedure(S_scale_by,SCM_FNC g_scale_by,0,3,0),H_scale_by);
  DEFINE_PROC(gh_new_procedure(S_normalize_view,SCM_FNC g_normalize_view,0,0,0),H_normalize_view);
  DEFINE_PROC(gh_new_procedure(S_open_sound_file,SCM_FNC g_open_sound_file,0,4,0),H_open_sound_file);
  DEFINE_PROC(gh_new_procedure(S_close_sound_file,SCM_FNC g_close_sound_file,2,0,0),H_close_sound_file);
  DEFINE_PROC(gh_new_procedure(S_vct_sound_file,SCM_FNC vct2soundfile,3,0,0),H_vct_sound_file);
  DEFINE_PROC(gh_new_procedure(S_mix_vct,SCM_FNC mix_vct,1,5,0),H_mix_vct);
  DEFINE_PROC(gh_new_procedure(S_transform_size,SCM_FNC g_transform_size,0,2,0),H_transform_size);
  DEFINE_PROC(gh_new_procedure(S_transform_samples,SCM_FNC g_transform_samples,0,2,0),H_transform_samples);
  DEFINE_PROC(gh_new_procedure(S_transform_sample,SCM_FNC g_transform_sample,0,4,0),H_transform_sample);
  DEFINE_PROC(gh_new_procedure(S_env_selection,SCM_FNC g_env_selection,1,3,0),H_env_selection);
  DEFINE_PROC(gh_new_procedure(S_env_sound,SCM_FNC g_env_sound,1,5,0),H_env_sound);
  DEFINE_PROC(gh_new_procedure(S_fft,SCM_FNC g_fft,2,1,0),H_fft);
  DEFINE_PROC(gh_new_procedure(S_snd_spectrum,SCM_FNC g_snd_spectrum,3,1,0),H_snd_spectrum);
  DEFINE_PROC(gh_new_procedure(S_convolve_arrays,SCM_FNC g_convolve,1,1,0),H_convolve);
  DEFINE_PROC(gh_new_procedure(S_convolve_with,SCM_FNC g_convolve_with,1,3,0),H_convolve_with);
  DEFINE_PROC(gh_new_procedure(S_convolve_selection_with,SCM_FNC g_convolve_selection_with,1,1,0),H_convolve_selection_with);
  DEFINE_PROC(gh_new_procedure(S_src_sound,SCM_FNC g_src_sound,1,3,0),H_src_sound);
  DEFINE_PROC(gh_new_procedure(S_src_selection,SCM_FNC g_src_selection,1,1,0),H_src_selection);
  DEFINE_PROC(gh_new_procedure(S_filter_sound,SCM_FNC g_filter_sound,2,2,0),H_filter_sound);
  DEFINE_PROC(gh_new_procedure(S_filter_selection,SCM_FNC g_filter_selection,2,0,0),H_filter_selection);
  DEFINE_PROC(gh_new_procedure(S_graph,SCM_FNC g_graph,1,7,0),H_graph);
#if HAVE_GUILE_1_3_0
  DEFINE_PROC(gh_new_procedure(S_string_length,SCM_FNC g_string_length,1,0,0),H_string_length);
#endif
  DEFINE_PROC(gh_new_procedure(S_samples_vct,SCM_FNC samples2vct,0,6,0),H_samples2vct);
  DEFINE_PROC(gh_new_procedure(S_samples2sound_data,SCM_FNC samples2sound_data,0,7,0),H_samples2sound_data);
  DEFINE_PROC(gh_new_procedure(S_transform_samples_vct,SCM_FNC transform_samples2vct,0,3,0),H_transform_samples2vct);
  DEFINE_PROC(gh_new_procedure(S_start_progress_report,SCM_FNC g_start_progress_report,0,1,0),H_start_progress_report);
  DEFINE_PROC(gh_new_procedure(S_finish_progress_report,SCM_FNC g_finish_progress_report,0,1,0),H_finish_progress_report);
  DEFINE_PROC(gh_new_procedure(S_progress_report,SCM_FNC g_progress_report,1,4,0),H_progress_report);
  DEFINE_PROC(gh_new_procedure(S_snd_print,SCM_FNC g_snd_print,1,0,0),H_snd_print);
  DEFINE_PROC(gh_new_procedure(S_describe_audio,SCM_FNC g_describe_audio,0,0,0),H_describe_audio);
  DEFINE_PROC(gh_new_procedure("mus-audio-describe",SCM_FNC g_describe_audio,0,0,0),H_describe_audio);

  /* semi-internal functions (restore-state) */
  gh_new_procedure(S_change_samples_with_origin,SCM_FNC g_change_samples_with_origin,4,2,0);
  gh_new_procedure(S_delete_samples_with_origin,SCM_FNC g_delete_samples_with_origin,3,2,0);
  gh_new_procedure(S_insert_samples_with_origin,SCM_FNC g_insert_samples_with_origin,4,2,0);

  /* ---------------- HOOKS ---------------- */
#if HAVE_HOOKS
  /* I think this is the actual hook object, not the "vcell" so it might make sense to set its documentation property */
  /*   or is this a per-hook function thing? -- then the help function could run through the hook list displaying docs? */
  during_open_hook = scm_create_hook(S_during_open_hook,3);       /* args = fd filename reason */
  after_open_hook = scm_create_hook(S_after_open_hook,1);         /* args = sound */
  exit_hook = scm_create_hook(S_exit_hook,0);
  start_hook = scm_create_hook(S_start_hook,1);                   /* arg = argv filename if any */
  output_comment_hook = scm_create_hook(S_output_comment_hook,1); /* arg = current mus_sound_comment(hdr) if any */
  mix_console_state_changed_hook = scm_create_hook(S_mix_console_state_changed_hook,1);
  mix_speed_changed_hook = scm_create_hook(S_mix_speed_changed_hook,1);
  mix_amp_changed_hook = scm_create_hook(S_mix_amp_changed_hook,1);
  mix_position_changed_hook = scm_create_hook(S_mix_position_changed_hook,2);
  #if FILE_PER_CHAN
    open_multifile_sound_hook = scm_create_hook(S_open_multifile_sound_hook,1);       /* arg = filename */
    save_multifile_sound_hook = scm_create_hook(S_save_multifile_sound_hook,2);       /* args = snd chn */
  #endif
#else
  during_open_hook = gh_define(S_during_open_hook,SCM_BOOL_F);
  after_open_hook = gh_define(S_after_open_hook,SCM_BOOL_F);
  exit_hook = gh_define(S_exit_hook,SCM_BOOL_F);
  start_hook = gh_define(S_start_hook,SCM_BOOL_F);
  output_comment_hook = gh_define(S_output_comment_hook,SCM_BOOL_F);
  mix_console_state_changed_hook = gh_define(S_mix_console_state_changed_hook,SCM_BOOL_F);
  mix_speed_changed_hook = gh_define(S_mix_speed_changed_hook,SCM_BOOL_F);
  mix_amp_changed_hook = gh_define(S_mix_amp_changed_hook,SCM_BOOL_F);
  mix_position_changed_hook = gh_define(S_mix_position_changed_hook,SCM_BOOL_F);
  #if FILE_PER_CHAN
    open_multifile_sound_hook = gh_define(S_open_multifile_sound_hook,SCM_BOOL_F);
    save_multifile_sound_hook = gh_define(S_save_multifile_sound_hook,SCM_BOOL_F);
  #endif
#endif

  g_init_marks(local_doc);
  g_init_regions(local_doc);
  g_init_selection(local_doc);
  g_init_dac(local_doc);
  init_vct();
  init_mus2scm_module();
  g_initialize_xgh(state,local_doc);
  g_initialize_xgfile(state,local_doc);
  g_init_gxutils();
  g_init_mix(local_doc);
  g_init_chn(local_doc);
  g_init_errors(local_doc);
  g_init_fft(local_doc);
  g_init_edits(local_doc);
  g_init_completions(local_doc);
  g_init_menu(local_doc);
  g_init_main(local_doc);
  g_init_snd(local_doc);
  g_init_file(local_doc);
  g_init_env(local_doc);
  g_init_recorder(local_doc);

  /* GOOPS */
  /* scm_init_oop_goops_goopscore_module (); */


#if HAVE_LADSPA
  g_ladspa_to_snd(local_doc);
#endif
  gh_eval_str("(define unbind-key\
                 (lambda (key state)\
                   \"(unbind-key key state) undoes the effect of a prior bind-key call\"\
                   (bind-key key state #f)))");

  gh_eval_str("(defmacro defvar (a b)\
                 `(begin\
                    (define ,a ,b)\
                    (define-envelope (symbol->string ',a) ,b)))");
  /* this is trying to keep track of envelopes for the envelope editor */

  gh_eval_str("(define snd-help\
                 (lambda n\
                   \"snd-help returns the procedure documentation associated with its argument.\n\
(snd-help make-vct) for example, prints out a brief description of make-vct.\n\
In the help descriptions, '&optional' marks optional arguments, and\n\
'&opt-key' marks CLM-style optional keyword arguments.  If you load index.scm\n\
the functions html and ? can be used in place of help to go to the HTML description\"\
                   (if (null? n)\
                       (snd-help snd-help)\
                     (let ((func (car n)))\
                       (if (procedure? func)\
                           (or (procedure-property func 'documentation)\
                               (procedure-documentation func)\
                               (object-property func 'documentation))\
                           (object-property func 'documentation))))))");
  /* TODO: should we append apropos results here, or a cf list? */
  /* to handle (extend) apropos from session.scm, we need to set up the Snd module, I think */
  /* TODO: how to grab "display" output from scheme and put it in the listener? */

    gh_eval_str("(read-set! keywords 'prefix)");

#if USE_MOTIF
  scm_add_feature("snd-motif");
#endif
#if USE_GTK
  scm_add_feature("snd-gtk");
#endif
#if USE_NO_GUI
  scm_add_feature("snd-nogui");
#endif

  scm_add_feature("snd");
}

#if HAVE_HOOKS

SCM g_c_run_progn_hook (SCM hook, SCM args)
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

SCM g_c_run_or_hook (SCM hook, SCM args)
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


#else
int dont_exit(snd_state *ss) {return(0);}
int dont_start(snd_state *ss, char *filename) {return(0);}
void call_mix_console_state_changed_hook(mixdata *md) {}
int call_mix_speed_changed_hook(mixdata *md) {return(0);}
int call_mix_amp_changed_hook(mixdata *md) {return(0);}
int call_mix_position_changed_hook(mixdata *md, int samps) {return(0);}
void during_open(int fd, char *file, int reason) {}
void after_open(int index) {}
  #if FILE_PER_CHAN
    int multifile_channel(char *filename) {return(-1);}
    char *multifile_save(int snd, int chn) {return(NULL);}
  #endif
#endif

#endif

char *output_comment(file_info *hdr)
{
  char *com = NULL;
  if (hdr) com = mus_sound_comment(hdr->name);
#if HAVE_HOOKS
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
