#include "snd.h"

/* TODO: be more restrictive about what counts as a boolean!
 * TODO: check rest of snd_error calls for homogenization
 */

#include "vct.h"
#include "clm2scm.h"
#include "sndlib-strings.h"

static snd_state *state = NULL;


/* -------- documentation for hooks -------- */

SCM snd_set_object_property(SCM obj, SCM key, SCM val)
{
  /* a convenience in creating hooks -- return obj, not val */
  scm_set_object_property_x(obj, key, val);
  return(obj);
}


/* -------- protect SCM vars from GC -------- */

static SCM gc_protection;
static int gc_protection_size = 0;
#define DEFAULT_GC_VALUE INTEGER_ZERO

void snd_protect(SCM obj)
{
  int i, old_size;
  SCM tmp, num;
  SCM *gcdata;
  if (gc_protection_size == 0)
    {
      gc_protection_size = 128;
      /* we don't know the size in advance since each channel can have its own edit/undo hooks */
      gc_protection = MAKE_VECTOR(gc_protection_size, DEFAULT_GC_VALUE);
      scm_permanent_object(gc_protection);
      scm_vector_set_x(gc_protection, TO_SMALL_SCM_INT(0), obj);
    }
  else
    {
      gcdata = SCM_VELTS(gc_protection);
      for (i = 0; i < gc_protection_size; i++)
	if (SCM_EQ_P(gcdata[i], DEFAULT_GC_VALUE))
	  {
	    scm_vector_set_x(gc_protection, TO_SCM_INT(i), obj);
	    return;
	  }
      tmp = gc_protection;
      old_size = gc_protection_size;
      gc_protection_size *= 2;
      gc_protection = MAKE_VECTOR(gc_protection_size, DEFAULT_GC_VALUE);
      scm_permanent_object(gc_protection);
      for (i = 0; i < old_size; i++)
	{
	  num = TO_SCM_INT(i);
	  scm_vector_set_x(gc_protection, num, VECTOR_REF(tmp, i));
	  scm_vector_set_x(tmp, num, DEFAULT_GC_VALUE);
	}
      scm_vector_set_x(gc_protection, TO_SCM_INT(old_size), obj);
    }
}

void snd_unprotect(SCM obj)
{
  int i;
  SCM *gcdata;
  gcdata = SCM_VELTS(gc_protection);
  for (i = 0; i < gc_protection_size; i++)
    if (SCM_EQ_P(gcdata[i], obj))
      {
	scm_vector_set_x(gc_protection, TO_SCM_INT(i), DEFAULT_GC_VALUE);
	return;
      }
}


int to_c_int_or_else(SCM obj, int fallback, const char *origin)
{
  /* don't want errors here about floats with non-zero fractions etc */
  if (INTEGER_P(obj))
    return(TO_C_INT(obj));
  else
    if (NUMBER_P(obj))
      return((int)TO_C_DOUBLE_WITH_ORIGIN(obj, origin));
  return(fallback);
}


/* -------- error handling -------- */

#define MAX_ERROR_STRING_LENGTH 1024
/* what is this number actually affecting? I can set it to 0 with no ill effects */

#if HAVE_GUILE
  #include <libguile/fluids.h>
#endif

static int send_error_output_to_stdout = 0;
static char *last_file_loaded = NULL;

static void string_to_stdout(snd_state *ss, char *msg)
{
  char *str;
  write(fileno(stdout), msg, snd_strlen(msg));
  str = (char *)CALLOC(4 + snd_strlen(listener_prompt(ss)), sizeof(char));
  sprintf(str, "\n%s", listener_prompt(ss));
  write(fileno(stdout), str, snd_strlen(str));
  FREE(str);
}

static SCM snd_catch_scm_error(void *data, SCM tag, SCM throw_args) /* error handler */
{
#if HAVE_GUILE
  /* this is actually catching any throw not caught elsewhere, I think */
  snd_info *sp;
  char *possible_code;
  SCM port, ans, stmp;
  SCM stack;
  char *name_buf = NULL;

#ifdef SCM_MAKE_CHAR
  port = scm_mkstrport(INTEGER_ZERO, 
		       scm_make_string(TO_SCM_INT(MAX_ERROR_STRING_LENGTH), 
				       SCM_MAKE_CHAR(0)),
		       SCM_OPN | SCM_WRTNG,
		       __FUNCTION__);
#else
  port = scm_mkstrport(INTEGER_ZERO, 
		       scm_make_string(TO_SCM_INT(MAX_ERROR_STRING_LENGTH), 
				       SCM_UNDEFINED),
		       SCM_OPN | SCM_WRTNG,
		       __FUNCTION__);
#endif

#if DEBUGGING
  {
    /* force out an error before possible backtrace call */
    SCM lport;
    lport = scm_mkstrport(INTEGER_ZERO, 
			  scm_make_string(TO_SCM_INT(MAX_ERROR_STRING_LENGTH), 
					  SCM_MAKE_CHAR(0)),
			  SCM_OPN | SCM_WRTNG,
			  __FUNCTION__);
    scm_display(tag, lport);
    scm_puts(": ", lport);
    scm_display(throw_args, lport);
    scm_force_output(lport);
    ans = scm_strport_to_string(lport);
    name_buf = TO_NEW_C_STRING(ans);
    fprintf(stderr, name_buf);
    if (name_buf) free(name_buf);
    name_buf = NULL;
  }
#endif

  if ((LIST_P(throw_args)) && 
      (LIST_LENGTH(throw_args) > 0))
    {
      scm_display(SCM_CAR(throw_args), port);
      scm_puts(": ", port);
      if (LIST_LENGTH(throw_args) > 1)
	{
	  if (SCM_EQ_P(tag, NO_SUCH_FILE))
	    {
	      scm_display(tag, port);
	      scm_puts(" \"", port);
	      scm_display(SCM_CADR(throw_args), port);
	      scm_puts("\" ", port);
	      if (LIST_LENGTH(throw_args) > 2)
		scm_display(SCM_CDDR(throw_args), port);
	    }
	  else
	    {
	      if ((SCM_EQ_P(tag, NO_SUCH_SOUND)) || (SCM_EQ_P(tag, NO_SUCH_MIX)) || (SCM_EQ_P(tag, NO_SUCH_MARK)) ||
		  (SCM_EQ_P(tag, NO_SUCH_MENU)) || (SCM_EQ_P(tag, NO_SUCH_REGION)) || (SCM_EQ_P(tag, MUS_MISC_ERROR)) ||
		  (SCM_EQ_P(tag, NO_SUCH_CHANNEL)) || (SCM_EQ_P(tag, NO_SUCH_EDIT)) ||
		  (SCM_EQ_P(tag, NO_SUCH_AXIS_INFO)) || (SCM_EQ_P(tag, NO_SUCH_AXIS_CONTEXT)) ||
		  (SCM_EQ_P(tag, CANNOT_SAVE)) || (SCM_EQ_P(tag, CANNOT_PRINT)) || (SCM_EQ_P(tag, BAD_ARITY)) ||
		  (SCM_EQ_P(tag, IMPOSSIBLE_BOUNDS)) || (SCM_EQ_P(tag, NO_SUCH_SAMPLE)))
		{
		  scm_display(tag, port);
		  scm_puts(" ", port);
		  scm_display(SCM_CDR(throw_args), port);
		}
	      else
		{
		  stmp = SCM_CADR(throw_args);
		  if ((STRING_P(stmp)) && (LIST_LENGTH(throw_args) > 2))
		    scm_display_error_message(stmp, SCM_CADDR(throw_args), port);
		  else scm_display(tag, port);
		  if (show_backtrace(state))
		    {
		      /* this was buggy in 1.3.4 */
		      /* and it still is -- #@$%!#@ */
		      stack = scm_fluid_ref(SCM_CDR(scm_the_last_stack_fluid));
		      if (NOT_FALSE_P(stack)) 
			/* scm_display_backtrace(stack, port, SCM_UNDEFINED, SCM_UNDEFINED); */
			scm_display_backtrace(stack, port, SCM_BOOL_F, SCM_BOOL_F); 
		    }
		}
	    }
	}
      else scm_display(tag, port);
    }
  else 
    {
      scm_display(tag, port);
      scm_puts(": ", port);
      scm_display(throw_args, port);
    }
  possible_code = (char *)data;
  if ((possible_code) && 
      (snd_strlen(possible_code) < MAX_ERROR_STRING_LENGTH / 2))
    {
      /* not actually sure if this is always safe */
      scm_puts("\n; ", port);
      scm_puts(possible_code, port);
    }
  if (last_file_loaded)
    {
      /* sigh -- scm_current_load_port is #f so can't use scm_port_filename etc */
      scm_puts("\n(while loading \"", port);
      scm_puts(last_file_loaded, port);
      scm_puts("\")", port);
      last_file_loaded = NULL;
    }
  scm_force_output(port); /* needed to get rid of trailing garbage chars?? -- might be pointless now */
  ans = scm_strport_to_string(port);
  name_buf = TO_NEW_C_STRING(ans);
  if (send_error_output_to_stdout)
    string_to_stdout(state, name_buf);
  else
    {
      if (state->mx_sp)
	{
	  sp = state->mx_sp;
	  clear_minibuffer_prompt(sp);
	  report_in_minibuffer(sp, name_buf);
	}
      if (state->listening != LISTENER_CLOSED)
	{
	  state->result_printout = MESSAGE_WITH_CARET;
	  snd_append_command(state, name_buf);
	}
      else 
	if (!(state->mx_sp))
	  snd_error(name_buf);
    }
  if (name_buf) free(name_buf);
#endif
  return(tag);
}

/* if error occurs in sndlib, mus-error wants to throw to user-defined catch
 *   (or our own global catch), but if the sndlib function was not called by the user, 
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
  state->catch_exists++;
  /* one function can invoke, for example, a hook that will call back here setting up a nested catch */
  result = scm_internal_stack_catch(tag, body, body_data, handler, handler_data);
  state->catch_exists--;
  if (state->catch_exists < 0) 
    {
#if DEBUGGING
      fprintf(stderr," catch unwound too far?? ");
      abort();
#endif
      state->catch_exists = 0;
    }
  return(result);
}

SCM snd_catch_any(scm_catch_body_t body, void *body_data, const char *caller)
{
  return(snd_internal_stack_catch(SCM_BOOL_T, body, body_data, snd_catch_scm_error, (void *)caller));
}

char *procedure_ok(SCM proc, int req_args, int opt_args, const char *caller, const char *arg_name, int argn)
{
  /* if string returned, needs to be freed */
  SCM arity_list;
  int args;
#if TIMING
  return(NULL);
#endif
  if (!(PROCEDURE_P(proc)))
    {
      if (NOT_FALSE_P(proc)) /* #f as explicit arg to clear */
	return(mus_format("%s, arg %d to %s, is not a procedure!", arg_name, argn, caller));
    }
  else
    {
      arity_list = ARITY(proc);
      snd_protect(arity_list);
      args = TO_SMALL_C_INT(SCM_CAR(arity_list));
      if (args != req_args)
	return(mus_format("%s, arg %d to %s, should take %d required argument%s, but instead takes %d",
			  arg_name, argn, caller,
			  req_args, 
			  (req_args != 1) ? "s" : "", 
			  args));
      else
	{
	  args = TO_SMALL_C_INT(SCM_CADR(arity_list));
	  if (args != opt_args)
	    return(mus_format("%s, arg %d to %s, should take %d optional argument%s, but instead takes %d",
			      arg_name, argn, caller,
			      opt_args, 
			      (opt_args != 1) ? "s" : "", 
			      args));
	  /* ignore &rest */
	}
      snd_unprotect(arity_list);
    }
  return(NULL);
}

int procedure_ok_with_error(SCM proc, int req_args, int opt_args, const char *caller, const char *arg_name, int argn)
{
  char *errmsg;
  errmsg = procedure_ok(proc, req_args, opt_args, caller, arg_name, argn);
  if (errmsg)
    {
      snd_error(errmsg);
      FREE(errmsg);
      return(0);
    }
  return(1);
}

void snd_no_such_file_error(const char *caller, SCM filename)
{
  scm_throw(NO_SUCH_FILE,
	    SCM_LIST3(TO_SCM_STRING(caller),
		      filename,
		      TO_SCM_STRING(strerror(errno))));
}

void snd_no_such_channel_error(const char *caller, SCM snd, SCM chn)
{
  scm_throw(NO_SUCH_CHANNEL,
	    SCM_LIST3(TO_SCM_STRING(caller),
		      snd,
		      chn));
}

void snd_no_active_selection_error(const char *caller)
{
  scm_throw(NO_ACTIVE_SELECTION,
	    SCM_LIST1(TO_SCM_STRING(caller)));
}

void snd_bad_arity_error(const char *caller, SCM errstr, SCM proc)
{
  scm_throw(BAD_ARITY,
	    SCM_LIST3(TO_SCM_STRING(caller),
		      errstr,
		      proc));
}


/* -------- various evaluators (within our error handler) -------- */

SCM eval_str_wrapper(void *data)
{
  return(EVAL_STRING((char *)data));
}

static SCM eval_file_wrapper(void *data)
{
  SCM res;
  last_file_loaded = (char *)data;
  res = gh_eval_file((char *)data);
  last_file_loaded = NULL;
  return(res);
}

#ifndef USE_OPT_APPLY
  #if HAVE_GUILE
    #define USE_OPT_APPLY 1
  #else
    #define USE_OPT_APPLY 0
  #endif
#endif

static SCM g_call0_1(void *arg)
{
#if USE_OPT_APPLY
  /* taken from guile's sort.c; can speed up user-funcs */
  SCM code = (SCM)arg;
  switch (SCM_TYP7(code))
    {
    case scm_tc7_subr_0:
      return(SCM_SUBRF(code)());
    case scm_tcs_closures:
      return scm_eval_body(SCM_CDR(SCM_CODE(code)), SCM_ENV(code)); /* not sure about env here */
    default:
      return(scm_apply(code, SCM_EOL, SCM_EOL));
    }
#else
  return(scm_apply((SCM)arg, SCM_EOL, SCM_EOL));
#endif
}

SCM g_call0(SCM proc, const char *caller) /* replacement for gh_call0 -- protect ourselves from premature exit(!$#%@$) */
{
  return(snd_catch_any(g_call0_1, (void *)proc, caller));
}

static SCM g_call1_1(void *arg)
{
#if USE_OPT_APPLY
  SCM env, code, obj;
  code = ((SCM *)arg)[0]; 
  obj = ((SCM *)arg)[1]; 
  switch (SCM_TYP7(code))
    {
    case scm_tc7_subr_1:
#ifdef __cplusplus
      return(((SCM (*)(SCM a))SCM_SUBRF(code))(obj));
#else
      return SCM_SUBRF(code)(obj);
#endif
    case scm_tcs_closures:
      env = SCM_EXTEND_ENV(SCM_CAR(SCM_CODE(code)),
			   SCM_LIST1(obj),
			   SCM_ENV(code));
      return scm_eval_body(SCM_CDR (SCM_CODE(code)), env);
    default:
      return(scm_apply(code, obj, scm_listofnull)); /* scm_listofnull, not SCM_EOL!  (the latter causes segfaults here) */
    }
#else
  return(scm_apply(((SCM *)arg)[0], 
 		   ((SCM *)arg)[1], 
 		   scm_listofnull));
#endif
}

SCM g_call1(SCM proc, SCM arg, const char *caller)
{
  SCM args[2];
  args[0] = proc;
  args[1] = arg;
  return(snd_catch_any(g_call1_1, (void *)args, caller));
}

static SCM g_call_any_1(void *arg)
{
  return(scm_apply(((SCM *)arg)[0], 
		   ((SCM *)arg)[1], 
		   SCM_EOL));
}

SCM g_call_any(SCM proc, SCM arglist, const char *caller)
{
  SCM args[2];
  args[0] = proc;
  args[1] = arglist;
  return(snd_catch_any(g_call_any_1, (void *)args, caller));
}

static SCM g_call2_1(void *arg)
{
#if USE_OPT_APPLY
  SCM env, code, arg1, arg2;
  code = ((SCM *)arg)[0]; 
  arg1 = ((SCM *)arg)[1]; 
  arg2 = ((SCM *)arg)[2]; 
  switch (SCM_TYP7(code))
    {
    case scm_tc7_subr_2:
#ifdef __cplusplus
      return(((SCM (*)(SCM a, SCM b))SCM_SUBRF(code))(arg1, arg2));
#else
      return SCM_SUBRF(code)(arg1, arg2);
#endif
    case scm_tcs_closures:
      env = SCM_EXTEND_ENV(SCM_CAR(SCM_CODE(code)),
			   SCM_LIST2(arg1, arg2),
			   SCM_ENV(code));
      return scm_eval_body(SCM_CDR (SCM_CODE(code)), env);
    default:
      return(scm_apply(code, arg1, CONS(arg2, scm_listofnull)));
    }
#else
  return(scm_apply(((SCM *)arg)[0], 
 		   ((SCM *)arg)[1], 
 		   CONS(((SCM *)arg)[2], scm_listofnull)));
#endif
}

SCM g_call2(SCM proc, SCM arg1, SCM arg2, const char *caller)
{
  SCM args[3];
  args[0] = proc;
  args[1] = arg1;
  args[2] = arg2;
  return(snd_catch_any(g_call2_1, (void *)args, caller));
}

static SCM g_call3_1(void *arg)
{
#if USE_OPT_APPLY
  SCM env, code, arg1, arg2, arg3;
  code = ((SCM *)arg)[0]; 
  arg1 = ((SCM *)arg)[1]; 
  arg2 = ((SCM *)arg)[2]; 
  arg3 = ((SCM *)arg)[3]; 
  switch (SCM_TYP7(code))
    {
    case scm_tc7_subr_3:
#ifdef __cplusplus
      return(((SCM (*)(SCM a, SCM b, SCM c))SCM_SUBRF(code))(arg1, arg2, arg3));
#else
      return SCM_SUBRF(code)(arg1, arg2, arg3);
#endif
    case scm_tcs_closures:
      env = SCM_EXTEND_ENV(SCM_CAR(SCM_CODE(code)),
			   SCM_LIST3(arg1, arg2, arg3),
			   SCM_ENV(code));
      return scm_eval_body(SCM_CDR (SCM_CODE(code)), env);
    default:
      return(scm_apply(code, arg1, CONS2(arg2, arg3, scm_listofnull)));
    }
#else
  return(scm_apply(((SCM *)arg)[0], 
		   ((SCM *)arg)[1], 
		   CONS2(((SCM *)arg)[2], 
			 ((SCM *)arg)[3], 
			 scm_listofnull)));
#endif
}

SCM g_call3(SCM proc, SCM arg1, SCM arg2, SCM arg3, const char *caller)
{
  SCM args[4];
  args[0] = proc;
  args[1] = arg1;
  args[2] = arg2;
  args[3] = arg3;
  return(snd_catch_any(g_call3_1, (void *)args, caller));
}

char *g_print_1(SCM obj, const char *caller)
{
  char *str1;
#if HAVE_GUILE
#if HAVE_SCM_OBJECT_TO_STRING
  return(TO_NEW_C_STRING(scm_object_to_string(obj, SCM_UNDEFINED))); /* does the GC handle the scm_close_port? */
#else
  SCM str, val;
  SCM port;
  str = scm_makstr (0, 0);
  port = scm_mkstrport (INTEGER_ZERO, str, SCM_OPN | SCM_WRTNG, caller);
  scm_prin1 (obj, port, 1);
  val = scm_strport_to_string(port);
  scm_close_port (port);
  str1 = TO_NEW_C_STRING(val);
#endif
#endif
  return(str1);
}

static char *gl_print(SCM result, const char *caller)
{
  char *newbuf = NULL, *str = NULL;
  int i, ilen, savelen, savectr, slen;
  /* specialize vectors which can be enormous in this context */
  if ((!(VECTOR_P(result))) || 
      ((int)(VECTOR_LENGTH(result)) <= print_length(state)))
    return(g_print_1(result, caller));
  ilen = print_length(state); 
  newbuf = (char *)calloc(128, sizeof(char));
  savelen = 128;
  savectr = 3;
  sprintf(newbuf, "#("); 
  for (i = 0; i < ilen; i++)
    {
      str = g_print_1(VECTOR_REF(result, i), caller);
      if ((str) && (*str)) 
	{
	  slen = strlen(str);
	  if ((slen + savectr + 1) >= savelen)
	    {
	      savelen += (slen + 128);
	      newbuf = (char *)realloc(newbuf, savelen * sizeof(char));
	    }
	  if (i != 0) 
	    {
	      strcat(newbuf, " "); 
	      savectr++;
	    }
	  strcat(newbuf, str);
	  savectr += slen;
	}
      if (str) free(str);
    }
  if (savectr + 8 > savelen) 
    newbuf = (char *)realloc(newbuf, (savectr + 8) * sizeof(char));
  strcat(newbuf, " ...)");
  return(newbuf);
}

int snd_eval_str(snd_state *ss, char *buf, int count)
{
  snd_info *sp = NULL;
  SCM result = SCM_UNDEFINED;
  int ctr;
  char *str = NULL;
  for (ctr = 0; ctr < count; ctr++)
    result = snd_catch_any(eval_str_wrapper, buf, buf);
  str = gl_print(result, "eval-str");
  if (ss->mx_sp)
    {
      sp = ss->mx_sp;
      clear_minibuffer_prompt(sp);
      report_in_minibuffer(sp, str);
    }
  if (ss->listening != LISTENER_CLOSED)
    {
      snd_append_command(ss, buf);
      ss->result_printout = MESSAGE_WITH_CARET;
      snd_append_command(ss, str);
    }
  if (str) free(str);
  return(0);
}

void snd_eval_listener_str(snd_state *ss, char *buf)
{
  SCM result;
  char *str;
  if ((snd_strlen(buf) == 0) || ((snd_strlen(buf) == 1) && (buf[0] == '\n'))) return;
  result = snd_catch_any(eval_str_wrapper, buf, buf);
  str = gl_print(result, "eval-listener-str");
  ss->result_printout = MESSAGE_WITH_CARET;
  snd_append_command(ss, str);
  if (str) free(str);
}

static char *stdin_str = NULL;

void clear_listener(void)
{
  snd_state *ss;
  ss = get_global_state();
  if (stdin_str) FREE(stdin_str);
  stdin_str = NULL;
  ss->result_printout = MESSAGE_WITH_CARET;
  snd_append_command(ss, "");
}

static char *stdin_check_for_full_expression(char *newstr)
{
  char *str;
  int end_of_text;
  if (stdin_str)
    {
      str = stdin_str;
      stdin_str = (char *)CALLOC(snd_strlen(str) + snd_strlen(newstr) + 2, sizeof(char));
      strcat(stdin_str, str);
      strcat(stdin_str, newstr);
      FREE(str);
    }
  else stdin_str = copy_string(newstr);
  end_of_text = check_balance(stdin_str, 0, strlen(stdin_str)); /* can be strlen! */
  if (end_of_text > 0)
    {
      if (end_of_text + 1 < snd_strlen(stdin_str)) /* is this needed?  see warning above, which no longer makes any sense to me... */
	stdin_str[end_of_text + 1] = 0;
      return(stdin_str);
    }
  else return(NULL);
}

void snd_eval_stdin_str(snd_state *ss, char *buf)
{
  /* we may get incomplete expressions here */
  /*   (Ilisp always sends a complete expression, but it may be broken into two or more pieces from read's point of view) */
  SCM result;
  char *str = NULL;
  if (snd_strlen(buf) == 0) return;
  str = stdin_check_for_full_expression(buf);
  if (str)
    {
      send_error_output_to_stdout = 1;
      result = snd_catch_any(eval_str_wrapper, str, str);
      send_error_output_to_stdout = 0;
      FREE(stdin_str); /* same as str in this case */
      stdin_str = NULL;
      str = gl_print(result, "eval-stdin-str");
      string_to_stdout(ss, str);
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
      fd = open(SND_CONF, O_RDONLY, 0);
      if (fd != -1)
	{
	  close(fd);
	  snd_catch_any(eval_file_wrapper, SND_CONF, "(load " SND_CONF ")");
	}
    }
#endif
  if ((ss->init_file) && (noi == 0))
    {
      str = mus_expand_filename(ss->init_file);
      fd = open(str, O_RDONLY, 0);
      if (fd != -1) 
	{
	  close(fd);
	  snd_catch_any(eval_file_wrapper, str, "(load ~/.snd)");
	}
      if (str) FREE(str);
    }
}

void snd_load_file(char *filename)
{
  char *str = NULL, *str1 = NULL, *str2 = NULL;
  str = mus_expand_filename(filename);
  str2 = (char *)CALLOC(256, sizeof(char));
  mus_snprintf(str2, 256, "(load \"%s\")", filename);
  if (!mus_file_probe(str))
    {
      /* try tacking on .scm */
      str1 = (char *)CALLOC(snd_strlen(str) + 5, sizeof(char));
      sprintf(str1, "%s.scm", str);
      if (!mus_file_probe(str1))
	snd_error("can't load %s: %s", filename, strerror(errno));
      /* snd_error ok here because all uses of this are user-interface generated (autoload, memo-file, etc) */
      else snd_catch_any(eval_file_wrapper, str1, str2);
      FREE(str1);
    }
  else snd_catch_any(eval_file_wrapper, str, str2);
  if (str) FREE(str);
  if (str2) FREE(str2);
}

static SCM g_snd_print(SCM msg)
{
  #define H_snd_print "(" S_snd_print " str) displays str in the lisp listener window"
  char *str = NULL;
  state->result_printout = MESSAGE_WITHOUT_CARET;
  if (STRING_P(msg))
    str = TO_NEW_C_STRING(msg);
  else
    {
      if (CHAR_P(msg))
	{
	  str = (char *)CALLOC(2, sizeof(char));
	  str[0] = TO_C_CHAR(msg);
	}
      else str = gl_print(msg, S_snd_print);
    }
  snd_append_command(state, str);
  if (str) free(str);
  return(msg);
}


/* -------- global variables -------- */

static SCM g_ask_before_overwrite(void) {return(TO_SCM_BOOLEAN(ask_before_overwrite(state)));}
static SCM g_set_ask_before_overwrite(SCM val) 
{
  #define H_ask_before_overwrite "(" S_ask_before_overwrite ") should be #t if you want Snd to ask before overwriting a file"
  SCM_ASSERT(BOOLEAN_IF_BOUND_P(val), val, SCM_ARG1, "set-" S_ask_before_overwrite);
  set_ask_before_overwrite(state, TO_C_BOOLEAN_OR_T(val)); 
  return(TO_SCM_BOOLEAN(ask_before_overwrite(state)));
}

static SCM g_audio_output_device(void) {return(TO_SCM_INT(audio_output_device(state)));}
static SCM g_set_audio_output_device(SCM val) 
{
  #define H_audio_output_device "(" S_audio_output_device ") is the current sndlib default output device (mus-audio-default)"
  SCM_ASSERT(INTEGER_P(val), val, SCM_ARG1, "set-" S_audio_output_device); 
  set_audio_output_device(state, TO_C_INT(val)); 
  return(TO_SCM_INT(audio_output_device(state)));
}

static SCM g_audio_input_device(void) {return(TO_SCM_INT(audio_input_device(state)));}
static SCM g_set_audio_input_device(SCM val) 
{
  #define H_audio_input_device "(" S_audio_input_device ") is the current sndlib default input device (mus-audio-default)"
  SCM_ASSERT(INTEGER_P(val), val, SCM_ARG1, "set-" S_audio_input_device); 
  set_audio_input_device(state, TO_C_INT(val)); 
  return(TO_SCM_INT(audio_input_device(state)));
}

static SCM g_minibuffer_history_length(void) {return(TO_SCM_INT(minibuffer_history_length(state)));}
static SCM g_set_minibuffer_history_length(SCM val) 
{
  #define H_minibuffer_history_length "(" S_minibuffer_history_length ") is the minibuffer history length"
  int len;
  SCM_ASSERT(INTEGER_P(val), val, SCM_ARG1, "set-" S_minibuffer_history_length);
  len = TO_C_INT(val);
  if (len > 0)
    set_minibuffer_history_length(state, len);
  return(TO_SCM_INT(minibuffer_history_length(state)));
}

static SCM g_dac_size(void) {return(TO_SCM_INT(dac_size(state)));}
static SCM g_set_dac_size(SCM val) 
{
  #define H_dac_size "(" S_dac_size ") is the current DAC buffer size (256)"
  int len;
  SCM_ASSERT(NUMBER_P(val), val, SCM_ARG1, "set-" S_dac_size);
  len = TO_C_INT_OR_ELSE(val, 0);
  if (len > 0)
    set_dac_size(state, len);
  return(TO_SCM_INT(dac_size(state)));
}

static SCM g_dac_folding(void) {return(TO_SCM_BOOLEAN(dac_folding(state)));}
static SCM g_set_dac_folding(SCM val) 
{
  #define H_dac_folding "(" S_dac_folding ") should be #t if extra channels are to be folded into available ones during playing (#t)"
  SCM_ASSERT(BOOLEAN_IF_BOUND_P(val), val, SCM_ARG1, "set-" S_dac_folding);
  set_dac_folding(state, TO_C_BOOLEAN_OR_T(val)); 
  return(TO_SCM_BOOLEAN(dac_folding(state)));
}

static SCM g_auto_resize(void) {return(TO_SCM_BOOLEAN(auto_resize(state)));}
static SCM g_set_auto_resize(SCM val) 
{
  #define H_auto_resize "(" S_auto_resize ") should be #t if Snd can change its main window size as it pleases (#t)"
  SCM_ASSERT(BOOLEAN_IF_BOUND_P(val), val, SCM_ARG1, "set-" S_auto_resize);
  set_auto_resize(state, TO_C_BOOLEAN_OR_T(val)); 
  reflect_resize(state); 
  return(TO_SCM_BOOLEAN(auto_resize(state)));
}

static SCM g_auto_update(void) {return(TO_SCM_BOOLEAN(auto_update(state)));}
static SCM g_set_auto_update(SCM val) 
{
  #define H_auto_update "(" S_auto_update ") -> #t if Snd should automatically update a file if it changes unexpectedly (#f)"
  SCM_ASSERT(BOOLEAN_IF_BOUND_P(val), val, SCM_ARG1, "set-" S_auto_update);
  set_auto_update(state, TO_C_BOOLEAN_OR_T(val)); 
  return(TO_SCM_BOOLEAN(auto_update(state)));
}

static SCM g_filter_env_in_hz(void) {return(TO_SCM_BOOLEAN(filter_env_in_hz(state)));}
static SCM g_set_filter_env_in_hz(SCM val) 
{
  #define H_filter_env_in_hz "(" S_filter_env_in_hz ") -> #t if filter env x axis should be in Hz"
  SCM_ASSERT(BOOLEAN_IF_BOUND_P(val), val, SCM_ARG1, "set-" S_filter_env_in_hz);
  set_filter_env_in_hz(state, TO_C_BOOLEAN_OR_T(val)); 
  return(TO_SCM_BOOLEAN(filter_env_in_hz(state)));
}

static SCM g_channel_style(void) {return(TO_SCM_INT(channel_style(state)));}
static SCM g_set_channel_style(SCM style) 
{
  #define H_channel_style "(" S_channel_style ") -> how multichannel sounds layout the channels \
default is channels-separate, other values are channels-combined and channels-superimposed. \
this is the default setting for each sound's 'unite' button."

  SCM_ASSERT(INTEGER_P(style), style, SCM_ARG1, "set-" S_channel_style); 
  set_channel_style(state, iclamp(CHANNELS_SEPARATE,
				 TO_C_INT(style),
				 CHANNELS_SUPERIMPOSED)); 
  return(TO_SCM_INT(channel_style(state)));
}

static SCM g_color_cutoff(void) {return(TO_SCM_DOUBLE(color_cutoff(state)));}
static SCM g_set_color_cutoff(SCM val) 
{
  #define H_color_cutoff "(" S_color_cutoff ") -> col" STR_OR " map cutoff point (default .003)"
  SCM_ASSERT(NUMBER_P(val), val, SCM_ARG1, "set-" S_color_cutoff);
  set_color_cutoff(state, fclamp(0.0,
				TO_C_DOUBLE(val),
				0.25)); 
  return(TO_SCM_DOUBLE(color_cutoff(state)));
}

static SCM g_color_inverted(void) {return(TO_SCM_BOOLEAN(color_inverted(state)));}
static SCM g_set_color_inverted(SCM val) 
{
  #define H_color_inverted "(" S_color_inverted ") -> whether the col" STR_OR "map in operation should be inverted"
  SCM_ASSERT(BOOLEAN_IF_BOUND_P(val), val, SCM_ARG1, "set-" S_color_inverted);
  set_color_inverted(state, TO_C_BOOLEAN_OR_T(val)); 
  return(TO_SCM_BOOLEAN(color_inverted(state)));
}

static SCM g_color_scale(void) {return(TO_SCM_DOUBLE(color_scale(state)));}
static SCM g_set_color_scale(SCM val) 
{
  #define H_color_scale "(" S_color_scale ") -> essentially a darkness setting for col" STR_OR "maps (0.5)"
  SCM_ASSERT(NUMBER_P(val), val, SCM_ARG1, "set-" S_color_scale); 
  set_color_scale(state, fclamp(0.0,
			       TO_C_DOUBLE(val),
			       1.0)); 
  return(TO_SCM_DOUBLE(color_scale(state)));
}

static SCM g_corruption_time(void) {return(TO_SCM_DOUBLE(corruption_time(state)));}
static SCM g_set_corruption_time(SCM val) 
{
  Float ctime;
  #define H_corruption_time "(" S_corruption_time ") -> time (seconds) between background checks for changed file on disk (60)"
  SCM_ASSERT(NUMBER_P(val), val, SCM_ARG1, "set-" S_corruption_time); 
  ctime = TO_C_DOUBLE(val);
  if ((ctime < 0.0) || (ctime > (24 * 3600)))
    mus_misc_error("set-" S_corruption_time, "invalid time:", val);
  set_corruption_time(state, TO_C_DOUBLE(val)); 
  return(TO_SCM_DOUBLE(corruption_time(state)));
}

static SCM g_default_output_chans(void) {return(TO_SCM_INT(default_output_chans(state)));}
static SCM g_set_default_output_chans(SCM val) 
{
  #define H_default_output_chans "(" S_default_output_chans ") -> default number of channels when a new or temporary file is created (1)"
  SCM_ASSERT(INTEGER_P(val), val, SCM_ARG1, "set-" S_default_output_chans); 
  set_default_output_chans(state, TO_C_INT(val));
  return(TO_SCM_INT(default_output_chans(state)));
}

static SCM g_default_output_srate(void) {return(TO_SCM_INT(default_output_srate(state)));}
static SCM g_set_default_output_srate(SCM val) 
{
  #define H_default_output_srate "(" S_default_output_srate ") -> default srate when a new or temporary file is created (22050)" 
  SCM_ASSERT(NUMBER_P(val), val, SCM_ARG1, "set-" S_default_output_srate); 
  set_default_output_srate(state, TO_C_INT_OR_ELSE(val, 0));
  return(TO_SCM_INT(default_output_srate(state)));
}

static SCM g_default_output_type(void) {return(TO_SCM_INT(default_output_type(state)));}
static SCM g_set_default_output_type(SCM val) 
{
  int typ;
  #define H_default_output_type "(" S_default_output_type ") -> default header type when a new or temporary file is created. \
Normally this is " S_mus_next "; -1 here indicates you want Snd to use the current sound's header type, if possible. \
Other writable headers include " S_mus_aiff ", " S_mus_riff ", " S_mus_ircam ", " S_mus_nist ", " S_mus_aifc ", and " S_mus_raw "."

  SCM_ASSERT(INTEGER_P(val), val, SCM_ARG1, "set-" S_default_output_type); 
  typ = TO_C_INT(val);
  if (mus_header_writable(typ, -2))
    set_default_output_type(state, typ); 
  else mus_misc_error("set-" S_default_output_type, 
		      "can't write this header type", 
		      SCM_LIST2(val, 
				TO_SCM_STRING(mus_header_type_name(typ))));
  return(TO_SCM_INT(default_output_type(state)));
}

static SCM g_default_output_format(void) {return(TO_SCM_INT(default_output_format(state)));}
static SCM g_set_default_output_format(SCM val) 
{
  int format;
  #define H_default_output_format "(" S_default_output_format ") -> default data format when a new or temporary file is created, \
normally " S_mus_bshort "; -1 here means try to use the current sound's data format; many other formats \
are available, but not all are compatible with all header types"

  SCM_ASSERT(INTEGER_P(val), val, SCM_ARG1, "set-" S_default_output_format); 
  format = TO_C_INT(val);
  set_default_output_format(state, format); 
  return(TO_SCM_INT(default_output_format(state)));
}

static SCM g_enved_base(void) {return(TO_SCM_DOUBLE(enved_base(state)));}
static SCM g_set_enved_base(SCM val) 
{
  #define H_enved_base "(" S_enved_base ") -> envelope editor exponential base value (1.0)"
  SCM_ASSERT(NUMBER_P(val), val, SCM_ARG1, "set-" S_enved_base); 
  set_enved_base(state, TO_C_DOUBLE(val));
  return(TO_SCM_DOUBLE(enved_base(state)));
}

static SCM g_enved_power(void) {return(TO_SCM_DOUBLE(enved_power(state)));}
static SCM g_set_enved_power(SCM val) 
{
  #define H_enved_power "(" S_enved_power ") -> envelope editor base scale range (9.0^power)"
  SCM_ASSERT(NUMBER_P(val), val, SCM_ARG1, "set-" S_enved_power); 
  set_enved_power(state, TO_C_DOUBLE(val));
  return(TO_SCM_DOUBLE(enved_power(state)));
}

static SCM g_enved_clipping(void) {return(TO_SCM_BOOLEAN(enved_clipping(state)));}
static SCM g_set_enved_clipping(SCM on)
{
  #define H_enved_clipping "(" S_enved_clipping ") -> envelope editor 'clip' button setting; \
if clipping, the motion of the mouse is restricted to the current graph bounds."

  SCM_ASSERT(BOOLEAN_IF_BOUND_P(on), on, SCM_ARG1, "set-" S_enved_clipping);
  set_enved_clipping(state, TO_C_BOOLEAN_OR_T(on)); 
  return(TO_SCM_BOOLEAN(enved_clipping(state)));
}

static SCM g_enved_exping(void) {return(TO_SCM_BOOLEAN(enved_exping(state)));}
static SCM g_set_enved_exping(SCM val) 
{
  #define H_enved_exping "(" S_enved_exping ") -> envelope editor 'exp' and 'lin' buttons; \
if enved-exping, the connecting segments use exponential curves rather than straight lines."

  SCM_ASSERT(BOOLEAN_IF_BOUND_P(val), val, SCM_ARG1, "set-" S_enved_exping);
  set_enved_exping(state, TO_C_BOOLEAN_OR_T(val)); 
  return(TO_SCM_BOOLEAN(enved_clipping(state)));
}

static SCM g_enved_target(void) {return(TO_SCM_INT(enved_target(state)));}
static SCM g_set_enved_target(SCM val) 
{
  int n; 
  #define H_enved_target "(" S_enved_target ") determines how the envelope is applied to data in the envelope editor; \
choices are " S_amplitude_env ", " S_srate_env ", and " S_spectrum_env

  SCM_ASSERT(INTEGER_P(val), val, SCM_ARG1, "set-" S_enved_target); 
  n = iclamp(AMPLITUDE_ENV,
	     TO_C_INT(val),
	     SRATE_ENV); 
  set_enved_target(state, n); 
  return(TO_SCM_INT(enved_target(state)));
}

static SCM g_enved_waving(void) {return(TO_SCM_BOOLEAN(enved_waving(state)));}
static SCM g_set_enved_waving(SCM val) 
{
  #define H_enved_waving "(" S_enved_waving ") -> #t if the envelope editor is displaying the waveform to be edited"
  SCM_ASSERT(BOOLEAN_IF_BOUND_P(val), val, SCM_ARG1, "set-" S_enved_waving);
  set_enved_waving(state, TO_C_BOOLEAN_OR_T(val));
  return(TO_SCM_BOOLEAN(enved_waving(state)));
}

static SCM g_enved_dBing(void) {return(TO_SCM_BOOLEAN(enved_dBing(state)));}
static SCM g_set_enved_dBing(SCM val) 
{
  #define H_enved_dBing "(" S_enved_dBing ") -> #t if the envelope editor is using dB"
  SCM_ASSERT(BOOLEAN_IF_BOUND_P(val), val, SCM_ARG1, "set-" S_enved_dBing);
  set_enved_dBing(state, TO_C_BOOLEAN_OR_T(val)); 
  return(TO_SCM_BOOLEAN(enved_dBing(state)));
}

static SCM g_eps_file(void) {return(TO_SCM_STRING(eps_file(state)));}
static SCM g_set_eps_file(SCM val) 
{
  #define H_eps_file "(" S_eps_file ") -> current eps ('Print' command) file name (snd.eps)"
  SCM_ASSERT(STRING_P(val), val, SCM_ARG1, "set-" S_eps_file); 
  if (eps_file(state)) free(eps_file(state));
  set_eps_file(state, TO_NEW_C_STRING(val)); 
  return(TO_SCM_STRING(eps_file(state)));
}

static SCM g_eps_left_margin(void) {return(TO_SCM_DOUBLE(eps_left_margin(state)));}
static SCM g_set_eps_left_margin(SCM val) 
{
  #define H_eps_left_margin "(" S_eps_left_margin ") -> current eps ('Print' command) left margin"
  SCM_ASSERT(NUMBER_P(val), val, SCM_ARG1, "set-" S_eps_left_margin); 
  set_eps_left_margin(state, TO_C_DOUBLE(val));
  return(TO_SCM_DOUBLE(eps_left_margin(state)));
}

static SCM g_eps_bottom_margin(void) {return(TO_SCM_DOUBLE(eps_bottom_margin(state)));}
static SCM g_set_eps_bottom_margin(SCM val) 
{
  #define H_eps_bottom_margin "(" S_eps_bottom_margin ") -> current eps ('Print' command) bottom margin"
  SCM_ASSERT(NUMBER_P(val), val, SCM_ARG1, "set-" S_eps_bottom_margin); 
  set_eps_bottom_margin(state, TO_C_DOUBLE(val));
  return(TO_SCM_DOUBLE(eps_bottom_margin(state)));
}

static SCM g_listener_prompt(void) {return(TO_SCM_STRING(listener_prompt(state)));}
static SCM g_set_listener_prompt(SCM val) 
{
  #define H_listener_prompt "(" S_listener_prompt ") -> the current lisp listener prompt character ('>') "
  SCM_ASSERT(STRING_P(val), val, SCM_ARG1, "set-" S_listener_prompt); 
  if (listener_prompt(state)) free(listener_prompt(state));
  set_listener_prompt(state, TO_NEW_C_STRING(val));
#if USE_NO_GUI
  {
    char *str;
    str = (char *)CALLOC(128, sizeof(char));
    mus_snprintf(str, 128, "(set! scm-repl-prompt \"%s\")", listener_prompt(state));
    EVAL_STRING(str);
    FREE(str);
  }
#endif
  return(TO_SCM_STRING(listener_prompt(state)));
}

static SCM g_audio_state_file(void) {return(TO_SCM_STRING(audio_state_file(state)));}
static SCM g_set_audio_state_file(SCM val) 
{
  #define H_audio_state_file "(" S_audio_state_file ") -> filename for the mus-audio-save-state function (.snd-mixer)"
  SCM_ASSERT(STRING_P(val), val, SCM_ARG1, "set-" S_audio_state_file); 
  if (audio_state_file(state)) free(audio_state_file(state));
  set_audio_state_file(state, TO_NEW_C_STRING(val));
  return(TO_SCM_STRING(audio_state_file(state)));
}

static SCM g_filter_env_order(void) {return(TO_SCM_INT(filter_env_order(state)));}
static SCM g_set_filter_env_order(SCM val) 
{
  #define H_filter_env_order "(" S_filter_env_order ") -> envelope editor's FIR filter order (40)"
  SCM_ASSERT(INTEGER_P(val), val, SCM_ARG1, "set-" S_filter_env); 
  set_filter_env_order(state, TO_C_INT(val));
  return(TO_SCM_INT(filter_env_order(state)));
}

static SCM g_fit_data_on_open(void) {return(TO_SCM_BOOLEAN(fit_data_on_open(state)));}
static SCM g_set_fit_data_on_open(SCM val) 
{
  #define H_fit_data_on_open "(" S_fit_data_on_open ") -> #t if Snd should set up the initial time domain axes to show the entire sound (#f)"
  SCM_ASSERT(BOOLEAN_IF_BOUND_P(val), val, SCM_ARG1, "set-" S_fit_data_on_open);
  set_fit_data_on_open(state, TO_C_BOOLEAN_OR_T(val));
  return(TO_SCM_BOOLEAN(fit_data_on_open(state)));
}

static SCM g_movies(void) {return(TO_SCM_BOOLEAN(movies(state)));}
static SCM g_set_movies(SCM val) 
{
  #define H_movies "(" S_movies ") -> #t if mix graphs are update continuously as the mix is dragged (#t)"
  SCM_ASSERT(BOOLEAN_IF_BOUND_P(val), val, SCM_ARG1, "set-" S_movies);
  set_movies(state, TO_C_BOOLEAN_OR_T(val));
  return(TO_SCM_BOOLEAN(movies(state)));
}

static SCM g_selection_creates_region(void) {return(TO_SCM_BOOLEAN(selection_creates_region(state)));}
static SCM g_set_selection_creates_region(SCM val) 
{
  #define H_selection_creates_region "(" S_selection_creates_region ") -> #t if a region should be created each time a selection is made"
  SCM_ASSERT(BOOLEAN_IF_BOUND_P(val), val, SCM_ARG1, "set-" S_selection_creates_region);
  set_selection_creates_region(state, TO_C_BOOLEAN_OR_T(val));
  return(TO_SCM_BOOLEAN(selection_creates_region(state)));
}

static SCM g_normalize_on_open(void) {return(TO_SCM_BOOLEAN(normalize_on_open(state)));}
static SCM g_set_normalize_on_open(SCM val) 
{
  #define H_normalize_on_open "(" S_normalize_on_open ") -> #t if Snd should try to evenly apportion screen space when a sound is opened (#t)"
  SCM_ASSERT(BOOLEAN_IF_BOUND_P(val), val, SCM_ARG1, "set-" S_normalize_on_open);
  set_normalize_on_open(state, TO_C_BOOLEAN_OR_T(val));
  return(TO_SCM_BOOLEAN(normalize_on_open(state)));
}

static SCM g_prefix_arg(void) {return(TO_SCM_INT(prefix_arg(state)));}
static SCM g_set_prefix_arg(SCM val) 
{
  #define H_prefix_arg "(" S_prefix_arg ") -> keyboard C-u argument"
  SCM_ASSERT(INTEGER_P(val), val, SCM_ARG1, "set-" S_prefix_arg); 
  set_prefix_arg(state, TO_C_INT(val));
  return(TO_SCM_INT(prefix_arg(state)));
}

static SCM g_print_length(void) {return(TO_SCM_INT(print_length(state)));}
static SCM g_set_print_length(SCM val) 
{
  #define H_print_length "(" S_print_length ") -> number of vector elements to print in the listener (12)"
  SCM_ASSERT(INTEGER_P(val), val, SCM_ARG1, "set-" S_print_length); 
  set_print_length(state, TO_C_INT(val)); 
  set_vct_print_length(TO_C_INT(val));
  return(TO_SCM_INT(print_length(state)));
}

static SCM g_previous_files_sort(void) {return(TO_SCM_INT(previous_files_sort(state)));}
static SCM g_set_previous_files_sort(SCM val) 
{
  #define H_previous_files_sort "(" S_previous_files_sort ") -> sort choice in view files (0 = unsorted, 1 = by name, etc)"
  SCM_ASSERT(INTEGER_P(val), val, SCM_ARG1, "set-" S_previous_files_sort); 
  update_prevlist(state);
  set_previous_files_sort(state, iclamp(0,
				       TO_C_INT(val),
				       4));
  update_prevfiles(state);
  return(TO_SCM_INT(previous_files_sort(state)));
}

static SCM g_raw_chans(void) {return(TO_SCM_INT(raw_chans(state)));}
static SCM g_set_raw_chans(SCM val) 
{
  #define H_raw_chans "(" S_raw_chans ") -> how many channels to expect in raw (headerless) files (1) [OBSOLETE]"
  SCM_ASSERT(INTEGER_P(val), val, SCM_ARG1, "set-" S_raw_chans);
  set_raw_chans(state, TO_C_INT(val));
  return(TO_SCM_INT(raw_chans(state)));
}

static SCM g_raw_format(void) {return(TO_SCM_INT(raw_format(state)));}
static SCM g_set_raw_format(SCM val) 
{
  #define H_raw_format "(" S_raw_format ") -> data format expected in raw (headerless) files (mus-bshort) [OBSOLETE]"
  SCM_ASSERT(INTEGER_P(val), val, SCM_ARG1, "set-" S_raw_format); 
  set_raw_format(state, TO_C_INT(val));
  return(TO_SCM_INT(raw_format(state)));
}

static SCM g_raw_srate(void) {return(TO_SCM_INT(raw_srate(state)));}
static SCM g_set_raw_srate(SCM val) 
{
  #define H_raw_srate "(" S_raw_srate ") -> srate expected in raw (headerless) files (44100) [OBSOLETE]"
  SCM_ASSERT(NUMBER_P(val), val, SCM_ARG1, "set-" S_raw_srate); 
  set_raw_srate(state, TO_C_INT_OR_ELSE(val, 0));
  return(TO_SCM_INT(raw_srate(state)));
}

static SCM g_save_state_on_exit(void) {return(TO_SCM_BOOLEAN(save_state_on_exit(state)));}
static SCM g_set_save_state_on_exit(SCM val) 
{
  #define H_save_state_on_exit "(" S_save_state_on_exit ") -> #t if Snd should save its current state upon exit"
  SCM_ASSERT(BOOLEAN_IF_BOUND_P(val), val, SCM_ARG1, "set-" S_save_state_on_exit);
  set_save_state_on_exit(state, TO_C_BOOLEAN_OR_T(val));
  return(TO_SCM_BOOLEAN(save_state_on_exit(state)));
}

static SCM g_show_indices(void) {return(TO_SCM_BOOLEAN(show_indices(state)));}
static SCM g_set_show_indices(SCM val) 
{
  #define H_show_indices "(" S_show_indices ") -> #t if sound name should be preceded by its index"
  SCM_ASSERT(BOOLEAN_IF_BOUND_P(val), val, SCM_ARG1, "set-" S_show_indices);
  set_show_indices(state, TO_C_BOOLEAN_OR_T(val));
  return(TO_SCM_BOOLEAN(show_indices(state)));
}

static SCM g_show_backtrace(void) {return(TO_SCM_BOOLEAN(show_backtrace(state)));}
static SCM g_set_show_backtrace(SCM val) 
{
  #define H_show_backtrace "(" S_show_backtrace ") -> #t to show backtrace automatically upon error"
  SCM_ASSERT(BOOLEAN_IF_BOUND_P(val), val, SCM_ARG1, "set-" S_show_backtrace);
  set_show_backtrace(state, TO_C_BOOLEAN_OR_T(val));
  return(TO_SCM_BOOLEAN(show_backtrace(state)));
}

static SCM g_show_usage_stats(void) {return(TO_SCM_BOOLEAN(show_usage_stats(state)));}
static SCM g_set_show_usage_stats(SCM on) 
{
  #define H_show_usage_stats "(" S_show_usage_stats ") -> #t if Snd should display memory usage stats"
  SCM_ASSERT(BOOLEAN_IF_BOUND_P(on), on, SCM_ARG1, "set-" S_show_usage_stats);
  set_show_usage_stats(state, TO_C_BOOLEAN_OR_T(on));
  return(TO_SCM_BOOLEAN(show_usage_stats(state)));
}

static SCM g_update_usage_stats(void) 
{
  #define H_update_usage_stats "(" S_update_usage_stats ") causes the stats display to be made current"
  update_stats(state); 
  return(SCM_BOOL_T);
}

static SCM g_sinc_width(void) {return(TO_SCM_INT(sinc_width(state)));}
static SCM g_set_sinc_width(SCM val) 
{
  #define H_sinc_width "(" S_sinc_width ") -> sampling rate conversion sinc width (10). \
The higher this number, the better the src low-pass filter, but the slower \
src runs.  If you use too low a setting, you can sometimes hear high \
frequency whistles leaking through."

  int len;
  SCM_ASSERT(INTEGER_P(val), val, SCM_ARG1, "set-" S_sinc_width); 
  len = TO_C_INT(val);
  if (len >= 0)
    set_sinc_width(state, len);
  return(TO_SCM_INT(sinc_width(state)));
}

static SCM g_color_map(void) {return(TO_SCM_INT(color_map(state)));}
static SCM g_set_color_map(SCM val) 
{
  #define H_colormap "(" S_colormap ") -> current col" STR_OR "map choice. \
This should be an integer between -1 and 15.  The maps (from 0 to 15) are: \
gray, hsv, hot, cool, bone, copper, pink, jet, prism, autumn, winter, \
spring, summer, colorcube, flag, and lines.  -1 means black and white."

  SCM_ASSERT(INTEGER_P(val), val, SCM_ARG1, "set-" S_colormap); 
  set_color_map(state, iclamp(0,
			     TO_C_INT(val),
			     NUM_COLORMAPS-1));
  return(TO_SCM_INT(color_map(state)));
}

static SCM g_temp_dir(void) {return(TO_SCM_STRING(temp_dir(state)));}
static SCM g_set_temp_dir(SCM val) 
{
  #define H_temp_dir "(" S_temp_dir ") -> name of directory for temp files"
  SCM_ASSERT(STRING_P(val), val, SCM_ARG1, "set-" S_temp_dir); 
  if (temp_dir(state)) free(temp_dir(state));
  set_temp_dir(state, TO_NEW_C_STRING(val));
  return(TO_SCM_STRING(temp_dir(state)));
}

static SCM g_snd_tempnam(void) 
{
  #define H_snd_tempnam "(" S_snd_tempnam ") -> new temp file name using temp-dir"
  return(TO_SCM_STRING(snd_tempnam(get_global_state())));
}

static SCM g_save_dir(void) {return(TO_SCM_STRING(save_dir(state)));}
static SCM g_set_save_dir(SCM val) 
{
  #define H_save_dir "(" S_save_dir ") -> name of directory for saved state data"
  SCM_ASSERT(STRING_P(val), val, SCM_ARG1, "set-" S_save_dir); 
  if (save_dir(state)) free(save_dir(state));
  set_save_dir(state, TO_NEW_C_STRING(val));
  return(TO_SCM_STRING(save_dir(state)));
}

static SCM g_trap_segfault(void) {return(TO_SCM_BOOLEAN(trap_segfault(state)));}
static SCM g_set_trap_segfault(SCM val) 
{
  #define H_trap_segfault "(" S_trap_segfault ") -> #t if Snd should try to trap (and whine about) segfaults"
  SCM_ASSERT(BOOLEAN_IF_BOUND_P(val), val, SCM_ARG1, "set-" S_trap_segfault);
  set_trap_segfault(state, TO_C_BOOLEAN_OR_T(val));
  return(TO_SCM_BOOLEAN(trap_segfault(state)));
}

static SCM g_show_selection_transform(void) {return(TO_SCM_BOOLEAN(show_selection_transform(state)));}
static SCM g_set_show_selection_transform(SCM val) 
{
  #define H_show_selection_transform "(" S_show_selection_transform ") -> #t if transform display reflects selection, not time-domain window"
  SCM_ASSERT(BOOLEAN_IF_BOUND_P(val), val, SCM_ARG1, "set-" S_show_selection_transform);
  set_show_selection_transform(state, TO_C_BOOLEAN_OR_T(val));
  return(TO_SCM_BOOLEAN(show_selection_transform(state)));
}

static SCM g_with_mix_tags(void) {return(TO_SCM_BOOLEAN(with_mix_tags(state)));}
static SCM g_set_with_mix_tags(SCM val) 
{
  #define H_with_mix_tags "(" S_with_mix_tags ") -> #t if Snd should editable mixes"
  SCM_ASSERT(BOOLEAN_IF_BOUND_P(val), val, SCM_ARG1, "set-" S_with_mix_tags);
  set_with_mix_tags(state, TO_C_BOOLEAN_OR_T(val));
  return(TO_SCM_BOOLEAN(with_mix_tags(state)));
}

static SCM g_use_raw_defaults(void) {return(TO_SCM_BOOLEAN(use_raw_defaults(state)));}
static SCM g_set_use_raw_defaults(SCM val) 
{
  #define H_use_raw_defaults "(" S_use_raw_defaults ") -> #t if Snd should simply use the raw-* defaults \
when a headerless file is encountered. If #f, Snd fires up the raw file dialog. [OBSOLETE]"

  SCM_ASSERT(BOOLEAN_IF_BOUND_P(val), val, SCM_ARG1, "set-" S_use_raw_defaults);
  set_use_raw_defaults(state, TO_C_BOOLEAN_OR_T(val));
  return(TO_SCM_BOOLEAN(use_raw_defaults(state)));
}

static SCM g_use_sinc_interp(void) {return(TO_SCM_BOOLEAN(use_sinc_interp(state)));}
static SCM g_set_use_sinc_interp(SCM val) 
{
  #define H_use_sinc_interp "(" S_use_sinc_interp ") -> #t if Snd should use convolution with sinc for sampling rate \
conversion.  The other choice is (much faster) linear interpolation which can introduce distortion"

  SCM_ASSERT(BOOLEAN_IF_BOUND_P(val), val, SCM_ARG1, "set-" S_use_sinc_interp);
  set_use_sinc_interp(state, TO_C_BOOLEAN_OR_T(val));
  return(TO_SCM_BOOLEAN(use_sinc_interp(state)));
}

static SCM g_data_clipped(void) {return(TO_SCM_BOOLEAN(data_clipped(state)));}
static SCM g_set_data_clipped(SCM val) 
{
  #define H_data_clipped "(" S_data_clipped ") -> #t if Snd should clip output values to the current \
output data format's maximum. The default (#f) allows them to wrap-around which makes a very loud click"

  SCM_ASSERT(BOOLEAN_IF_BOUND_P(val), val, SCM_ARG1, "set-" S_data_clipped);
  set_data_clipped(state, TO_C_BOOLEAN_OR_T(val));
  return(TO_SCM_BOOLEAN(data_clipped(state)));
}

static SCM g_vu_font(void) {return(TO_SCM_STRING(vu_font(state)));}
static SCM g_set_vu_font(SCM val) 
{
  #define H_vu_font "(" S_vu_font ") -> name of font used to make VU meter labels (courier)"
  SCM_ASSERT(STRING_P(val), val, SCM_ARG1, "set-" S_vu_font); 
  if (vu_font(state)) free(vu_font(state));
  set_vu_font(state, TO_NEW_C_STRING(val));
  return(TO_SCM_STRING(vu_font(state)));
}

static SCM g_vu_font_size(void) {return(TO_SCM_DOUBLE(vu_font_size(state)));}
static SCM g_set_vu_font_size(SCM val) 
{
  #define H_vu_font_size "(" S_vu_font_size ") -> size of VU font meter labels (1.0)"
  SCM_ASSERT(NUMBER_P(val), val, SCM_ARG1, "set-" S_vu_font_size); 
  set_vu_font_size(state, TO_C_DOUBLE(val));
  return(TO_SCM_DOUBLE(vu_font_size(state)));
}

static SCM g_vu_size(void) {return(TO_SCM_DOUBLE(vu_size(state)));}
static SCM g_set_vu_size(SCM val) 
{
  #define H_vu_size "(" S_vu_size ") -> size of VU meters (1.0)"
  SCM_ASSERT(NUMBER_P(val), val, SCM_ARG1, "set-" S_vu_size); 
  set_vu_size(state, TO_C_DOUBLE(val));
  return(TO_SCM_DOUBLE(vu_size(state)));
}

static SCM g_x_axis_style(void) {return(TO_SCM_INT(x_axis_style(state)));}
static SCM g_set_x_axis_style(SCM val) 
{
  #define H_x_axis_style "(" S_x_axis_style ") -> labelling of time domain x axis (x-in-seconds)"
  SCM_ASSERT(INTEGER_P(val), val, SCM_ARG1, "set-" S_x_axis_style); 
  set_x_axis_style(state, iclamp(X_IN_SECONDS,
				TO_C_INT(val),
				X_IN_LENGTH));
  return(TO_SCM_INT(x_axis_style(state)));
}

static SCM g_zoom_focus_style(void) {return(TO_SCM_INT(zoom_focus_style(state)));}
static SCM g_set_zoom_focus_style(SCM focus) 
{
  #define H_zoom_focus_style "(" S_zoom_focus_style ") -> one of '(" S_focus_left " " S_focus_right " " S_focus_middle " " S_focus_active ")\n\
  decides what zooming centers on (default: " S_focus_active ")"
  SCM_ASSERT(INTEGER_P(focus), focus, SCM_ARG1, "set-" S_zoom_focus_style); 
  activate_focus_menu(state, iclamp(FOCUS_LEFT,
				   TO_C_INT(focus),
				   FOCUS_MIDDLE));
  return(TO_SCM_INT(zoom_focus_style(state)));
}

static SCM g_snd_version(void) 
{
  #define H_snd_version "(" S_snd_version ") -> current Snd version"
  return(TO_SCM_STRING(SND_VERSION));
}

static SCM g_max_sounds(void) 
{
  #define H_max_sounds "(" S_max_sounds ") -> max sound id currently possible (grows as necessary)"
  return(TO_SCM_INT(state->max_sounds));
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
  for (i = 0; i < ss->max_sounds; i++)
    {
      sp = ((snd_info *)(ss->sounds[i]));
      if ((sp) && (sp->inuse))
	result = CONS(TO_SMALL_SCM_INT(i),
		      result);
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
  if (state->listening != LISTENER_OPEN) handle_listener(state, LISTENER_OPEN); 
  return(SCM_BOOL_F);
}

static SCM g_hide_listener(void) 
{
  #define H_hide_listener "(" S_hide_listener ") closes the lisp listener pane"
  if (state->listening == LISTENER_OPEN) handle_listener(state, LISTENER_LISTENING); 
  return(SCM_BOOL_F);
}

static SCM g_activate_listener(void) 
{
  #define H_activate_listener "(" S_activate_listener ") makes the listener active, if not open"
  handle_listener(state, LISTENER_LISTENING); 
  state->listening = LISTENER_LISTENING; 
  return(SCM_BOOL_F);
}

static SCM g_help_text_font(void) {return(TO_SCM_STRING(help_text_font(state)));}
static SCM g_set_help_text_font(SCM val) 
{
  #define H_help_text_font "(" S_help_text_font ") -> font used in the Help dialog"
  SCM_ASSERT(STRING_P(val), val, SCM_ARG1, "set-" S_help_text_font); 
  set_help_text_font(state, TO_NEW_C_STRING(val)); 
  return(val);
}

static SCM g_tiny_font(void) {return(TO_SCM_STRING(tiny_font(state)));}
static SCM g_set_tiny_font(SCM val) 
{
  #define H_tiny_font "(" S_tiny_font ") -> font use for some info in the graphs"
  SCM_ASSERT(STRING_P(val), val, SCM_ARG1, "set-" S_tiny_font); 
  set_tiny_font(state, TO_NEW_C_STRING(val)); 
  return(val);
}

static SCM g_axis_label_font(void) {return(TO_SCM_STRING(axis_label_font(state)));}
static SCM g_set_axis_label_font(SCM val) 
{
  #define H_axis_label_font "(" S_axis_label_font ") -> font used for axis labels"
  SCM_ASSERT(STRING_P(val), val, SCM_ARG1, "set-" S_axis_label_font); 
  set_axis_label_font(state, TO_NEW_C_STRING(val)); 
  return(val);
}

static SCM g_axis_numbers_font(void) {return(TO_SCM_STRING(axis_numbers_font(state)));}
static SCM g_set_axis_numbers_font(SCM val) 
{
  #define H_axis_numbers_font "(" S_axis_numbers_font ") -> font used for axis numbers"
  SCM_ASSERT(STRING_P(val), val, SCM_ARG1, "set-" S_axis_numbers_font); 
  set_axis_numbers_font(state, TO_NEW_C_STRING(val)); 
  return(val);
}

static SCM g_listener_font(void) {return(TO_SCM_STRING(listener_font(state)));}
static SCM g_set_listener_font(SCM val) 
{
  #define H_listener_font "(" S_listener_font ") -> font used by the lisp listener"
  SCM_ASSERT(STRING_P(val), val, SCM_ARG1, "set-" S_listener_font);
  set_listener_font(state, TO_NEW_C_STRING(val)); 
  return(val);
}

static SCM g_bold_button_font(void) {return(TO_SCM_STRING(bold_button_font(state)));}
static SCM g_set_bold_button_font(SCM val) 
{
  #define H_bold_button_font "(" S_bold_button_font ") -> font used by some buttons"
  SCM_ASSERT(STRING_P(val), val, SCM_ARG1, "set-" S_bold_button_font); 
  set_bold_button_font(state, TO_NEW_C_STRING(val)); 
  return(val);
}

static SCM g_button_font(void) {return(TO_SCM_STRING(button_font(state)));}
static SCM g_set_button_font(SCM val) 
{
  #define H_button_font "(" S_button_font ") -> font used by some buttons"
  SCM_ASSERT(STRING_P(val), val, SCM_ARG1, "set-" S_button_font); 
  set_button_font(state, TO_NEW_C_STRING(val)); 
  return(val);
}

static SCM g_window_width(void) 
{
  #define H_window_width "(" S_window_width ") -> current Snd window width in pixels"
  return(TO_SCM_INT(widget_width(MAIN_SHELL(state))));
}

static SCM g_window_height(void) 
{
  #define H_window_height "(" S_window_height ") -> current Snd window height in pixels"
  return(TO_SCM_INT(widget_height(MAIN_SHELL(state))));
}

static SCM g_window_x(void) 
{
  #define H_window_x "(" S_window_x ") -> current Snd window x position in pixels"
  return(TO_SCM_INT(widget_x(MAIN_SHELL(state))));
}

static SCM g_window_y(void) 
{
  #define H_window_y "(" S_window_y ") -> current Snd window y position in pixels"
  return(TO_SCM_INT(widget_y(MAIN_SHELL(state))));
}

static SCM g_set_window_height(SCM height) 
{
  #define H_set_window_height "(" "set-" S_window_height " val) sets the Snd window height in pixels"
  SCM_ASSERT(NUMBER_P(height), height, SCM_ARG1, "set-" S_window_height); 
  set_widget_height(MAIN_SHELL(state), TO_C_INT_OR_ELSE(height, 0));
  state->init_window_height = TO_C_INT_OR_ELSE(height, 0);
  return(height);
}

static SCM g_set_window_width(SCM width) 
{
  #define H_set_window_width "(" "set-" S_window_width " val) sets the Snd window width in pixels"
  SCM_ASSERT(NUMBER_P(width), width, SCM_ARG1, "set-" S_window_width); 
  set_widget_width(MAIN_SHELL(state), TO_C_INT_OR_ELSE(width, 0)); 
  state->init_window_width = TO_C_INT_OR_ELSE(width, 0);
  return(width);
}

static SCM g_set_window_x(SCM val) 
{
  #define H_set_window_x "(" "set-" S_window_x " val) sets the Snd window x position in pixels"
  SCM_ASSERT(NUMBER_P(val), val, SCM_ARG1, "set-" S_window_x); 
  set_widget_x(MAIN_SHELL(state), TO_C_INT_OR_ELSE(val, 0));
  state->init_window_x = TO_C_INT_OR_ELSE(val, 0); 
  return(val);
}

static SCM g_set_window_y(SCM val) 
{
  #define H_set_window_y "(" "set-" S_window_y " val) sets the Snd window y position in pixels"
  SCM_ASSERT(NUMBER_P(val), val, SCM_ARG1, "set-" S_window_y); 
  set_widget_y(MAIN_SHELL(state), TO_C_INT_OR_ELSE(val, 0)); 
  state->init_window_y = TO_C_INT_OR_ELSE(val, 0); 
  return(val);
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
  #define H_abortQ "(" S_c_g ") allows pending user interface events to occur, returning #t if C-g was typed"
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
  int snd_n, len;
  /* if scm_snd_n is a number, it is sp->index
     if it's a (non-empty) list, car is mix id (perhaps someday treat list as track if more than one member)
  */
  if (INTEGER_P(scm_snd_n))
    {
      snd_n = TO_C_INT(scm_snd_n);
      if (snd_n >= 0)
	{
	  if ((snd_n < state->max_sounds) && 
	      (snd_ok(state->sounds[snd_n])))
	    return(state->sounds[snd_n]);
	  else return(NULL);
	}
      else return(player(snd_n));
      return(NULL);
    }
  else
    {
      if (LIST_P_WITH_LENGTH(scm_snd_n, len))
	{
	  /* a mix input sound */
	  /* make sure it's not the null list */
	  if (len > 0)
	    return(make_mix_readable_from_id(TO_C_INT_OR_ELSE(SCM_CAR(scm_snd_n), 0)));
	  return(NULL);
	}
    }
  /* use default sound, if any */
  return(any_selected_sound(state));
}

chan_info *get_cp(SCM scm_snd_n, SCM scm_chn_n, const char *caller)
{
  snd_info *sp;
  int chn_n;
  sp = get_sp(scm_snd_n);
  if (sp == NULL) 
    snd_no_such_sound_error(caller, scm_snd_n); 
  if (INTEGER_P(scm_chn_n))
    chn_n = TO_C_INT(scm_chn_n);
  else
    if (sp->selected_channel != NO_SELECTION) 
      chn_n = sp->selected_channel;
    else chn_n = 0;
  if ((chn_n >= 0) && (chn_n < sp->nchans)) 
    return(sp->chans[chn_n]);
  snd_no_such_channel_error(caller, scm_snd_n, scm_chn_n);
  return(NULL);
}

/* -------- random stuff that hasn't been moved to a more logical place -------- */

static file_info **temp_sound_headers = NULL;
static int *temp_sound_fds = NULL;
static int temp_sound_size = 0;

static void set_temp_fd(int fd, file_info *hdr)
{
  int i, pos;
  if (temp_sound_size == 0)
    {
      temp_sound_size = 4;
      temp_sound_fds = (int *)CALLOC(temp_sound_size, sizeof(int));
      temp_sound_headers = (file_info **)CALLOC(temp_sound_size, sizeof(file_info *));
      pos = 0;
    }
  else
    {
      pos = -1;
      for (i = 0; i < temp_sound_size; i++)
	if (temp_sound_headers[i] == NULL) {pos = i; break;}
      if (pos == -1)
	{
	  pos = temp_sound_size;
	  temp_sound_size += 4;
	  temp_sound_fds = (int *)REALLOC(temp_sound_fds, temp_sound_size * sizeof(int));
	  temp_sound_headers = (file_info **)REALLOC(temp_sound_headers, temp_sound_size * sizeof(file_info *));
	  for (i = pos; i < temp_sound_size; i++) temp_sound_headers[i] = NULL;
	}
    }
  temp_sound_fds[pos] = fd;
  temp_sound_headers[pos] = hdr;
}

static file_info *get_temp_header(int fd)
{
  int i;
  for (i = 0; i < temp_sound_size; i++)
    if (fd == temp_sound_fds[i]) 
      return(temp_sound_headers[i]);
  return(NULL);
}

static void unset_temp_fd(int fd)
{
  int i;
  for (i = 0; i < temp_sound_size; i++)
    if (fd == temp_sound_fds[i])
      {
	temp_sound_fds[i] = 0;
	temp_sound_headers[i] = NULL;
      }
}

static mus_error_handler_t *old_mus_error;

static void mus_local_error(int type, char *msg)
{
  mus_error_set_handler(old_mus_error);           /* make sure subsequent errors are handled by the default handler */
  scm_throw(CANNOT_SAVE,
	    SCM_LIST2(TO_SCM_STRING(S_save_sound_as),
		      TO_SCM_STRING(msg)));
}

static SCM g_open_sound_file(SCM g_name, SCM g_chans, SCM g_srate, SCM g_comment)
{
  #define H_open_sound_file "(" S_open_sound_file " &optional (name \"test.snd\")\n    (chans 1) (srate 22050) comment)\n\
creates a new sound file 'name' using either 'wave' or 'next' headers and float data, returns the file descriptor for \
subsequent " S_close_sound_file ". data can be written with " S_vct_sound_file

  /* assume user temp files are writing floats in native format */
  char *name = NULL, *comment = NULL;
  file_info *hdr;
  int chans = 1, srate = 22050, result;
#if MUS_LITTLE_ENDIAN
  int type = MUS_RIFF;
  int format = MUS_LFLOAT; /* see comment! */
#else
  int type = MUS_NEXT;
  int format = MUS_BFLOAT;
#endif
  if (STRING_P(g_name)) 
    name = TO_NEW_C_STRING(g_name); 
  else 
    if (BOUND_P(g_name))
      scm_wrong_type_arg(S_open_sound_file, 1, g_name);
  if (NUMBER_P(g_chans)) 
    chans = TO_C_INT_OR_ELSE(g_chans, 0); 
  else 
    if (BOUND_P(g_chans))
      scm_wrong_type_arg(S_open_sound_file, 2, g_chans);
  if (NUMBER_P(g_srate)) 
    srate = TO_C_INT_OR_ELSE(g_srate, 0); 
  else
    if (BOUND_P(g_srate))
      scm_wrong_type_arg(S_open_sound_file, 3, g_srate);
  if (STRING_P(g_comment)) 
    comment = TO_NEW_C_STRING(g_comment);
  else 
    if (BOUND_P(g_comment)) 
      scm_wrong_type_arg(S_open_sound_file, 4, g_comment);
  if (name == NULL)
#if MUS_LITTLE_ENDIAN
    name = copy_string("test.wav");
#else
    name = copy_string("test.snd");
#endif
  hdr = (file_info *)CALLOC(1, sizeof(file_info));
  hdr->name = copy_string(name);
  hdr->samples = 0;
  hdr->data_location = 28;
  hdr->srate = srate;
  hdr->chans = chans;
  hdr->format = format;
  hdr->type = type;
  if (comment)
    {
      hdr->comment = copy_string(comment);
      free(comment);
    }
  old_mus_error = mus_error_set_handler(mus_local_error);
  result = open_temp_file(name, chans, hdr, state);
  mus_error_set_handler(old_mus_error);
  free(name);
  if (result == -1) 
    snd_no_such_file_error(S_open_sound_file, g_name);
  set_temp_fd(result, hdr);
  return(TO_SCM_INT(result));
}

static SCM g_close_sound_file(SCM g_fd, SCM g_bytes)
{
  #define H_close_sound_file "(" S_close_sound_file " fd bytes) closes file pointed to by fd, updating its header to report 'bytes' bytes of data"
  file_info *hdr;
  int result, fd, bytes;
  SCM_ASSERT(INTEGER_P(g_fd), g_fd, SCM_ARG1, S_close_sound_file);
  SCM_ASSERT(NUMBER_P(g_bytes), g_bytes, SCM_ARG2, S_close_sound_file);
  fd = TO_C_INT(g_fd);
  bytes = TO_C_INT_OR_ELSE(g_bytes, 0);
  hdr = get_temp_header(fd);
  if (hdr == NULL) 
    {
      close(fd);
      snd_no_such_file_error(S_close_sound_file, g_fd);
    }
  result = close_temp_file(fd, hdr, bytes, any_selected_sound(state));
  unset_temp_fd(fd);
  return(TO_SCM_INT(result));
}

static SCM samples2vct(SCM samp_0, SCM samps, SCM snd_n, SCM chn_n, SCM v, SCM pos)
{
  #define H_samples2vct "(" S_samples_vct " &optional (start-samp 0)\n    samps snd chn vct-obj edit-position)\n\
returns a vct object (vct-obj if given) containing snd channel chn's data starting at start-samp for samps, \
reading edit version edit-position (defaulting to the current version)"

  chan_info *cp;
  snd_fd *sf;
  Float *fvals;
  int i, len, beg, edpos;
  vct *v1 = get_vct(v);
  SCM_ASSERT(NUMBER_IF_BOUND_P(samp_0), samp_0, SCM_ARG1, S_samples_vct);
  SCM_ASSERT(NUMBER_IF_BOUND_P(samps), samps, SCM_ARG2, S_samples_vct);
  SND_ASSERT_CHAN(S_samples_vct, snd_n, chn_n, 3);
  cp = get_cp(snd_n, chn_n, S_samples_vct);
  edpos = TO_C_INT_OR_ELSE(pos, cp->edit_ctr);
  beg = TO_C_INT_OR_ELSE(samp_0, 0);
  len = TO_C_INT_OR_ELSE(samps, cp->samples[edpos] - beg);
  if (v1)
    fvals = v1->data;
  else fvals = (Float *)CALLOC(len, sizeof(Float));
  sf = init_sample_read_any(beg, cp, READ_FORWARD, edpos);
  if (sf)
    {
      for (i = 0; i < len; i++) 
	fvals[i] = next_sample_to_float(sf);
      free_snd_fd(sf);
    }
  if (v1)
    return(v);
  else return(make_vct(len, fvals));
}

static inline MUS_SAMPLE_TYPE local_next_sample_unscaled(snd_fd *sf)
{
  if (sf->view_buffered_data > sf->last)
    return(next_sound(sf));
  else return(*sf->view_buffered_data++);
}

static SCM samples2sound_data(SCM samp_0, SCM samps, SCM snd_n, SCM chn_n, SCM sdobj, SCM pos, SCM sdchan)
{
  #define H_samples2sound_data "(" S_samples2sound_data " &optional (start-samp 0)\n    samps snd chn sdobj edit-position (sdobj-chan 0))\n\
returns a sound-data object (sdobj if given) containing snd channel chn's data starting at start-samp for samps, \
reading edit version edit-position (defaulting to the current version)"

  chan_info *cp;
  snd_fd *sf;
  sound_data *sd;
  SCM newsd = SCM_BOOL_F;
  int i, len, beg, chn = 0, edpos;
  SCM_ASSERT(NUMBER_IF_BOUND_P(samp_0), samp_0, SCM_ARG1, S_samples2sound_data);
  SCM_ASSERT(NUMBER_IF_BOUND_P(samps), samps, SCM_ARG2, S_samples2sound_data);
  SND_ASSERT_CHAN(S_samples2sound_data, snd_n, chn_n, 3);
  cp = get_cp(snd_n, chn_n, S_samples2sound_data);
  edpos = TO_C_INT_OR_ELSE(pos, cp->edit_ctr);
  if (edpos >= cp->edit_size) 
    scm_throw(NO_SUCH_EDIT,
	      SCM_LIST4(TO_SCM_STRING(S_samples2sound_data),
			snd_n, chn_n, pos));
  beg = TO_C_INT_OR_ELSE(samp_0, 0);
  len = TO_C_INT_OR_ELSE(samps, cp->samples[edpos] - beg);
  if (len > 0)
    {
      chn = TO_C_INT_OR_ELSE(sdchan, 0);
      if (sound_data_p(sdobj))
	sd = (sound_data *)SND_VALUE_OF(sdobj);
      else
	{
	  newsd = make_sound_data(chn + 1, len);
	  sd = (sound_data *)SND_VALUE_OF(newsd);
	}
      if (chn < sd->chans)
	{
	  sf = init_sample_read_any(beg, cp, READ_FORWARD, edpos);
	  if (sf)
	    {
	      if (no_ed_scalers(cp))
		for (i = 0; i < len; i++) 
		  sd->data[chn][i] = local_next_sample_unscaled(sf);
	      else 
		for (i = 0; i < len; i++) 
		  sd->data[chn][i] = next_sample(sf);
	      free_snd_fd(sf);
	    }
	}
    }
  if (NOT_FALSE_P(newsd))
    return(newsd);
  return(sdobj);
}

static SCM vct2soundfile(SCM g_fd, SCM obj, SCM g_nums)
{
  #define H_vct_sound_file "(" S_vct_sound_file " fd vct-obj samps) writes samps samples from vct-obj to the sound file controlled by fd"
  int fd, nums, i;
  float *vals;
  vct *v;
  SCM_ASSERT(INTEGER_P(g_fd), g_fd, SCM_ARG1, S_vct_sound_file);
  SCM_ASSERT((VCT_P(obj)), obj, SCM_ARG2, S_vct_sound_file);
  SCM_ASSERT(NUMBER_P(g_nums), g_nums, SCM_ARG3, S_vct_sound_file);
  fd = TO_C_INT(g_fd);
  nums = TO_C_INT_OR_ELSE(g_nums, 0);
  v = TO_VCT(obj);
  lseek(fd, 0L, SEEK_END);
  if (sizeof(Float) == 4) /* Float can be either float or double */
    nums = write(fd, (char *)(v->data), nums * 4);
  else
    {
      /* v->data has doubles, but we're assuming elsewhere that these are floats in the file */
      vals = (float *)CALLOC(nums, sizeof(float));
      for (i = 0; i < nums; i++)
	vals[i] = (float)(v->data[i]);
      write(fd, (char *)vals, nums * 4);
      FREE(vals);
    }
  return(scm_return_first(TO_SMALL_SCM_INT(nums>>2), obj));
}


Float string2Float(char *str) 
{
  SCM res;
  res = snd_catch_any(eval_str_wrapper, str, "string->float");
  if (NUMBER_P(res))
    return(TO_C_DOUBLE(res));
  else snd_error("%s is not a number", str);
  return(0.0);
}

int string2int(char *str) 
{
  SCM res;
  res = snd_catch_any(eval_str_wrapper, str, "string->int");
  if (NUMBER_P(res))
    return(TO_C_INT_OR_ELSE(res, 0));
  else snd_error("%s is not a number", str);
  return(0);
}

#if 0
char *string2string(char *str) 
{
  SCM res;
  res = snd_catch_any(eval_str_wrapper, str, "string->string");
  if (STRING_P(res))
    return(TO_NEW_C_STRING(res));
  else snd_error("%s is not a string", str);
  return(str);
}
#endif

static SCM g_update_graph(SCM snd, SCM chn) 
{
  #define H_update_graph "(" S_update_graph " &optional snd chn) redraws snd channel chn's graphs"
  chan_info *cp;
  SND_ASSERT_CHAN(S_update_graph, snd, chn, 1); 
  cp = get_cp(snd, chn, S_update_graph);
  update_graph(cp, NULL);
  return(SCM_BOOL_F);
}

static SCM g_update_fft(SCM snd, SCM chn) 
{
  #define H_update_fft "(" S_update_fft " &optional snd chn) recalculates snd channel chn's fft"
  chan_info *cp;
  SND_ASSERT_CHAN(S_update_fft, snd, chn, 1); 
  cp = get_cp(snd, chn, S_update_fft);
  calculate_fft(cp, NULL);
  return(SCM_BOOL_F);
}

static SCM g_update_lisp_graph(SCM snd, SCM chn) 
{
  #define H_update_lisp_graph "(" S_update_lisp_graph " &optional snd chn) redraws snd channel chn's lisp graph"
  chan_info *cp;
  SND_ASSERT_CHAN(S_update_lisp_graph, snd, chn, 1); 
  cp = get_cp(snd, chn, S_update_lisp_graph);
  display_channel_lisp_data(cp, cp->sound, cp->state);
  return(SCM_BOOL_F);
}

static SCM g_help_dialog(SCM subject, SCM msg)
{
  #define H_help_dialog "(" S_help_dialog " subject message) fires up the Help window with subject and message"
  SCM_ASSERT(STRING_P(subject), subject, SCM_ARG1, S_help_dialog);
  SCM_ASSERT(STRING_P(msg), msg, SCM_ARG2, S_help_dialog);
  snd_help(state, TO_C_STRING(subject), TO_C_STRING(msg));
  return(SCM_BOOL_F);
}

static SCM g_mix_panel(void)
{
  #define H_mix_panel "(" S_mix_panel ") starts the mix panel"
  make_mix_panel(get_global_state());
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
  start_color_dialog(state, 0, 0); 
  return(SCM_BOOL_F);
}

static SCM g_orientation_dialog(void) 
{
  #define H_orientation_dialog "(" S_orientation_dialog ") fires up the Orientation dialog"
  start_orientation_dialog(state, 0, 0); 
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
  start_file_dialog(state, 0, 0);
  return(SCM_BOOL_F);
}

static SCM g_edit_header_dialog(SCM snd_n) 
{
  #define H_edit_header_dialog "(" S_edit_header_dialog " snd) opens the Edit Header dialog on sound snd"
  snd_info *sp; 
  sp = get_sp(snd_n); 
  if (sp) 
    edit_header(sp); 
  else snd_no_such_sound_error(S_edit_header_dialog, snd_n);
  return(SCM_BOOL_F);
}

static SCM g_yes_or_no_p(SCM msg) 
{
  #define H_yes_or_no_p "(" S_yes_or_no_p " message) displays message and waits for 'y' or 'n'; returns #t if 'y'"
  SCM_ASSERT(STRING_P(msg), msg, SCM_ARG1, S_yes_or_no_p);
  return(TO_SCM_BOOLEAN(snd_yes_or_no_p(state, TO_C_STRING(msg))));
}

static SCM g_graph(SCM ldata, SCM xlabel, SCM x0, SCM x1, SCM y0, SCM y1, SCM snd_n, SCM chn_n, SCM force_display)
{
  #define H_graph "(" S_graph " data &optional xlabel x0 x1 y0 y1 snd chn force-display)\n\
displays 'data' as a graph with x axis label 'xlabel', axis units going from x0 to x1 and y0 to y1; 'data' can be a list, vct, or vector. \
If 'data' is a list of numbers, it is treated as an envelope."

  chan_info *cp;
  lisp_grf *lg;
  SCM data = SCM_UNDEFINED, lst;
  char *label = NULL;
  vct *v = NULL;
  SCM *vdata;
  int i, len, graph, graphs, need_update = 0;
  Float ymin, ymax, val, nominal_x0, nominal_x1;
  lisp_grf *old_lp = NULL;
  int h = 0, w = 0, o = 0, gx0 = 0, ww = 0;
  axis_info *uap = NULL;
  /* ldata can be a vct object, a vector, or a list of either */
  SCM_ASSERT(((VCT_P(ldata)) || (VECTOR_P(ldata)) || (LIST_P(ldata))), ldata, SCM_ARG1, S_graph);
  SND_ASSERT_CHAN(S_graph, snd_n, chn_n, 7);
  cp = get_cp(snd_n, chn_n, S_graph);
  ymin = 32768.0;
  ymax = -32768.0;
  if ((cp->sound_ctr == -1) || 
      (cp->sounds == NULL) || 
      (cp->sounds[cp->sound_ctr] == NULL) ||
      (cp->axis == NULL))
    return(SCM_BOOL_F);
  if (STRING_P(xlabel)) label = TO_NEW_C_STRING(xlabel); 
  if (NUMBER_P(x0)) nominal_x0 = TO_C_DOUBLE(x0); else nominal_x0 = 0.0;
  if (NUMBER_P(x1)) nominal_x1 = TO_C_DOUBLE(x1); else nominal_x1 = 1.0;
  if (NUMBER_P(y0)) ymin = TO_C_DOUBLE(y0);
  if (NUMBER_P(y1)) ymax = TO_C_DOUBLE(y1);
  if ((!(LIST_P(ldata))) || 
      (NUMBER_P(SCM_CAR(ldata))))
    graphs = 1; 
  else graphs = LIST_LENGTH(ldata);
  if (graphs == 0) return(SCM_BOOL_F);
  lg = cp->lisp_info;
  if ((lg) && (graphs != lg->graphs)) 
    {
      old_lp = (lisp_grf *)(cp->lisp_info);
      uap = old_lp->axis;
      h = uap->height;
      w = uap->width;
      ww = uap->window_width;
      o = uap->y_offset;
      gx0 = uap->graph_x0;
      cp->lisp_info = free_lisp_info(cp);
    }
  if (!(cp->lisp_info))
    {
      cp->lisp_info = (lisp_grf *)CALLOC(graphs, sizeof(lisp_grf));
      lg = cp->lisp_info;
      lg->len = (int *)CALLOC(graphs, sizeof(int));
      lg->graphs = graphs;
      lg->data = (Float **)CALLOC(graphs, sizeof(Float *));
      need_update = 1;
    }
  if ((LIST_P_WITH_LENGTH(ldata, len)) && 
      (NUMBER_P(SCM_CAR(ldata))))
    {
      lg = cp->lisp_info;
      lg->env_data = 1;
      if (lg->len[0] != len)
	{
	  if (lg->data[0]) FREE(lg->data[0]);
	  lg->data[0] = (Float *)CALLOC(len, sizeof(Float));
	  lg->len[0] = len;
	}
      for (i = 0, lst = ldata; i < len; i++, lst = SCM_CDR(lst))
	lg->data[0][i] = TO_C_DOUBLE(SCM_CAR(lst));
      if ((!NUMBER_P(y0)) || 
	  (!NUMBER_P(y1)))
	{
	  for (i = 1; i < len; i+=2)
	    {
	      val = lg->data[0][i];
	      if (ymin > val) ymin = val;
	      if (ymax < val) ymax = val;
	    }
	}
      if (!NUMBER_P(x0)) nominal_x0 = lg->data[0][0];
      if (!NUMBER_P(x1)) nominal_x1 = lg->data[0][len - 2];
    }
  else
    {
      lg = cp->lisp_info;
      lg->env_data = 0;
      for (graph = 0; graph < graphs; graph++)
	{
	  if (LIST_P(ldata))
	    data = LIST_REF(ldata, graph);
	  else data = ldata;
	  if (VCT_P(data))
	    {
	      v = (vct *)SND_VALUE_OF(data);
	      len = v->length;
	    }
	  else len = VECTOR_LENGTH(data);
	  if (lg->len[graph] != len)
	    {
	      if (lg->data[graph]) FREE(lg->data[graph]);
	      lg->data[graph] = (Float *)CALLOC(len, sizeof(Float));
	      lg->len[graph] = len;
	    }
	  if (v)
	    {
	      for (i = 0; i < len; i++) 
		lg->data[graph][i] = v->data[i];
	    }
	  else 
	    {
	      vdata = SCM_VELTS(data);
	      for (i = 0; i < len; i++) 
		lg->data[graph][i] = TO_C_DOUBLE(vdata[i]);
	    }
	  if ((!NUMBER_P(y0)) || 
	      (!NUMBER_P(y1)))
	    {
	      for (i = 0; i < len; i++)
		{
		  val = lg->data[graph][i];
		  if (ymin > val) ymin = val;
		  if (ymax < val) ymax = val;
		}
	    }
	}
    }
  lg->axis = make_axis_info(cp, nominal_x0, nominal_x1, ymin, ymax, label, nominal_x0, nominal_x1, ymin, ymax, lg->axis);
  if (need_update)
    {
      uap = lg->axis;
      uap->height = h;
      uap->window_width = ww;
      uap->y_offset = o;
      uap->width = w;
      uap->graph_x0 = gx0;
    }
  if (label) free(label);
  cp->lisp_graphing = 1;
  if ((SCM_EQ_P(force_display, SCM_UNDEFINED)) || 
      (NOT_FALSE_P(force_display)))
    {
      if (need_update)
	update_graph(cp, NULL);
      else display_channel_lisp_data(cp, cp->sound, cp->state);
    }
  return(scm_return_first(SCM_BOOL_F, data));
}


#if HAVE_OSS
static SCM g_clear_audio_inputs (void) 
{
  #define H_clear_audio_inputs "(" S_clear_audio_inputs ") tries to reduce soundcard noise in Linux/OSS"
  mus_audio_clear_soundcard_inputs(); 
  return(SCM_BOOL_F);
}
#endif

static SCM g_set_oss_buffers(SCM num, SCM size)
{
  #define H_set_oss_buffers "(" "set-" S_oss_buffers " num size) sets Linux OSS 'fragment' number and size"
#if (HAVE_OSS || HAVE_ALSA)
  SCM_ASSERT(INTEGER_P(num), num, SCM_ARG1, "set-" S_oss_buffers);
  SCM_ASSERT(INTEGER_P(size), size, SCM_ARG2, "set-" S_oss_buffers);
  mus_audio_set_oss_buffers(TO_C_INT(num),
			    TO_C_INT(size));
#endif
  return(SCM_BOOL_F);
}

static SCM g_describe_audio(void) 
{
  #define H_describe_audio "("  S_describe_audio ") posts a description of the audio hardware state in the Help dialog"
  snd_help(state, "Audio State", mus_audio_report()); 
  return(SCM_BOOL_T);
}


static SCM g_start_progress_report(SCM snd)
{
  #define H_start_progress_report "(" S_start_progress_report " &optional snd) posts the hour-glass icon"
  snd_info *sp;
  SND_ASSERT_SND(S_start_progress_report, snd, 1);
  sp = get_sp(snd);
  if (sp)
    start_progress_report(sp, NOT_FROM_ENVED); 
  else snd_no_such_sound_error(S_start_progress_report, snd);
  return(SCM_BOOL_T);
}

static SCM g_finish_progress_report(SCM snd)
{
  #define H_finish_progress_report "(" S_finish_progress_report " &optional snd) removes the hour-glass icon"
  snd_info *sp;
  SND_ASSERT_SND(S_finish_progress_report, snd, 1);
  sp = get_sp(snd);
  if (sp) 
    finish_progress_report(sp, NOT_FROM_ENVED); 
  else snd_no_such_sound_error(S_finish_progress_report, snd);
  return(SCM_BOOL_T);
}

static SCM g_progress_report(SCM pct, SCM name, SCM cur_chan, SCM chans, SCM snd)
{
  #define H_progress_report "(" S_progress_report " pct &optional name cur-chan chans snd)\n\
updates an on-going 'progress report' (e. g. an animated hour-glass icon) in snd using pct to indicate how far along we are"

  snd_info *sp;
  SCM_ASSERT(NUMBER_P(pct), pct, SCM_ARG1, S_progress_report);
  SND_ASSERT_SND(S_progress_report, snd, 5);
  sp = get_sp(snd);
  if (sp) 
    progress_report(sp,
		    (STRING_P(name)) ? TO_C_STRING(name) : "something useful",
		    TO_C_INT_OR_ELSE(cur_chan, 0),
		    TO_C_INT_OR_ELSE(chans, sp->nchans),
		    TO_C_DOUBLE(pct),
		    NOT_FROM_ENVED);
  else snd_no_such_sound_error(S_progress_report, snd);
  return(SCM_BOOL_T);
}


#if (!USE_NO_GUI)

#if HAVE_HTML
static SCM g_html_dir(void) 
{
  #define H_html_dir "(" S_html_dir ") -> location of Snd documentation"
  return(TO_SCM_STRING(html_dir(state)));
}

static SCM g_set_html_dir(SCM val) 
{
  SCM_ASSERT(STRING_P(val), val, SCM_ARG1, "set-" S_html_dir);
  set_html_dir(state, TO_NEW_C_STRING(val)); 
  return(val);
}
#endif


/* -------- shared color funcs -------- */

static SCM g_set_cursor_color (SCM color) 
{
  snd_color *v; 
  SCM_ASSERT(snd_color_p(color), color, SCM_ARG1, "set-" S_cursor_color); 
  v = get_snd_color(color); 
  if (v) 
    {
      color_cursor(state, v->color);
      map_over_chans(state, update_graph, NULL);
    }
  return(color);
}

static SCM g_cursor_color(void) 
{
  #define H_cursor_color "(" S_cursor_color ") -> cursor col" STR_OR
  return(pixel2color((state->sgx)->cursor_color));
}

static SCM g_set_highlight_color (SCM color) 
{
  snd_color *v; 
  SCM_ASSERT(snd_color_p(color), color, SCM_ARG1, "set-" S_highlight_color); 
  v = get_snd_color(color); 
  if (v) (state->sgx)->highlight_color = v->color; 
  return(color);
}

static SCM g_highlight_color(void) 
{
  #define H_highlight_color "(" S_highlight_color ") -> col" STR_OR " of highlighted text or buttons"
  return(pixel2color((state->sgx)->highlight_color));
}

static SCM g_set_mark_color (SCM color) 
{
  snd_color *v; 
  SCM_ASSERT(snd_color_p(color), color, SCM_ARG1, "set-" S_mark_color); 
  v = get_snd_color(color); 
  if (v) 
    {
      color_marks(state, v->color);
      map_over_chans(state, update_graph, NULL);
    }
  return(color);
}

static SCM g_mark_color(void) 
{
  #define H_mark_color "(" S_mark_color ") -> mark col" STR_OR
  return(pixel2color((state->sgx)->mark_color));
}

static SCM g_set_zoom_color (SCM color) 
{
  snd_color *v; 
  SCM_ASSERT(snd_color_p(color), color, SCM_ARG1, "set-" S_zoom_color); 
  v = get_snd_color(color); 
  if (v) 
    {
      (state->sgx)->zoom_color = v->color; 
      color_chan_components(v->color, COLOR_ZOOM);
    }
  return(color);
}

static SCM g_zoom_color(void) 
{
  #define H_zoom_color "(" S_zoom_color ") -> col" STR_OR " of zoom sliders"
  return(pixel2color((state->sgx)->zoom_color));
}

static SCM g_set_position_color (SCM color) 
{
  snd_color *v; 
  SCM_ASSERT(snd_color_p(color), color, SCM_ARG1, "set-" S_position_color); 
  v = get_snd_color(color); 
  if (v) 
    {
      (state->sgx)->position_color = v->color; 
      color_chan_components(v->color, COLOR_POSITION);
    }
  return(color);
}

static SCM g_position_color(void) 
{
  #define H_position_color "(" S_position_color ") -> col" STR_OR " of position sliders"
  return(pixel2color((state->sgx)->position_color));
}

static SCM g_set_listener_color (SCM color) 
{
  snd_color *v; 
  SCM_ASSERT(snd_color_p(color), color, SCM_ARG1, "set-" S_listener_color); 
  v = get_snd_color(color);
  if (v) color_listener(v->color);
  return(color);
}

static SCM g_listener_color(void) 
{
  #define H_listener_color "(" S_listener_color ") -> background col" STR_OR " of the lisp listener"
  return(pixel2color((state->sgx)->listener_color));
}

static SCM g_set_listener_text_color (SCM color) 
{
  snd_color *v; 
  SCM_ASSERT(snd_color_p(color), color, SCM_ARG1, "set-" S_listener_text_color); 
  v = get_snd_color(color);
  if (v) color_listener_text(v->color);
  return(color);
}

static SCM g_listener_text_color(void) 
{
  #define H_listener_text_color "(" S_listener_text_color ") -> text col" STR_OR " in the lisp listener"
  return(pixel2color((state->sgx)->listener_text_color));
}

static SCM g_set_enved_waveform_color (SCM color) 
{
  snd_color *v;
  SCM_ASSERT(snd_color_p(color), color, SCM_ARG1, "set-" S_enved_waveform_color); 
  v = get_snd_color(color); 
  if (v) color_enved_waveform(v->color);
  return(color);
}

static SCM g_enved_waveform_color(void) 
{
  #define H_enved_waveform_color "(" S_enved_waveform_color ") -> col" STR_OR " of the envelope editor wave display"
  return(pixel2color((state->sgx)->enved_waveform_color));
}

static SCM g_set_filter_waveform_color (SCM color) 
{
  snd_color *v; 
  SCM_ASSERT(snd_color_p(color), color, SCM_ARG1, "set-" S_filter_waveform_color);
  v = get_snd_color(color);
  if (v) color_filter_waveform(state, v->color);
  return(color);
}

static SCM g_filter_waveform_color(void) 
{
  #define H_filter_waveform_color "(" S_filter_waveform_color ") -> col" STR_OR " of the filter waveform"
  return(pixel2color((state->sgx)->filter_waveform_color));
}

static SCM g_set_selection_color (SCM color) 
{
  snd_color *v; 
  SCM_ASSERT(snd_color_p(color), color, SCM_ARG1, "set-" S_selection_color); 
  v = get_snd_color(color); 
  if (v) 
    {
      color_selection(state, v->color);
      map_over_chans(state, update_graph, NULL);
    }
  return(color);
}

static SCM g_selection_color(void) 
{
  #define H_selection_color "(" S_selection_color ") -> selection col" STR_OR
  return(pixel2color((state->sgx)->selection_color));
}

static SCM g_set_mix_color (SCM arg1, SCM arg2)
{
  snd_color *v;
  SCM color, mix_id = SCM_UNDEFINED;
  if (NOT_BOUND_P(arg2))
    color = arg1;
  else
    {
      color = arg2;
      mix_id = arg1;
    }
  SCM_ASSERT(snd_color_p(color), color, SCM_ARG1, "set-" S_mix_color); 
  v = get_snd_color(color); 
  if (v) 
    {
      if (INTEGER_P(mix_id))
	color_one_mix_from_id(TO_SMALL_C_INT(mix_id), v->color);
      else set_mix_color(state, v->color);
      map_over_chans(state, update_graph, NULL);
    }
  return(color);
}

static SCM g_mix_color(SCM mix_id) 
{
  #define H_mix_color "(" S_mix_color ") -> col" STR_OR " of mix consoles"
  if (INTEGER_P(mix_id))
    return(pixel2color(mix_to_color_from_id(TO_SMALL_C_INT(mix_id))));
  return(pixel2color((state->sgx)->mix_color));
}

static SCM g_set_selected_mix_color (SCM color) 
{
  snd_color *v; 
  SCM_ASSERT(snd_color_p(color), color, SCM_ARG1, "set-" S_selected_mix_color); 
  v = get_snd_color(color); 
  if (v) 
    {
      set_selected_mix_color(state, v->color);
      map_over_chans(state, update_graph, NULL);
    }
  return(color);
}

static SCM g_selected_mix_color(void) 
{
  #define H_selected_mix_color "(" S_selected_mix_color ") -> col" STR_OR " of the currently selected mix"
  return(pixel2color((state->sgx)->selected_mix_color));
}

static SCM g_set_text_focus_color (SCM color) 
{
  snd_color *v; 
  SCM_ASSERT(snd_color_p(color), color, SCM_ARG1, "set-" S_text_focus_color); 
  v = get_snd_color(color); 
  if (v) (state->sgx)->text_focus_color = v->color;
  return(color);
}

static SCM g_text_focus_color(void) 
{
  #define H_text_focus_color "(" S_text_focus_color ") -> col" STR_OR " used to show a text field has focus"
  return(pixel2color((state->sgx)->text_focus_color));
}

static SCM g_set_sash_color (SCM color) 
{
  snd_color *v; 
  SCM_ASSERT(snd_color_p(color), color, SCM_ARG1, "set-" S_sash_color); 
  v = get_snd_color(color); 
  if (v) (state->sgx)->sash_color = v->color;
  return(color);
}

static SCM g_sash_color(void) 
{
  #define H_sash_color "(" S_sash_color ") -> col" STR_OR " used to draw paned window sashes"
  return(pixel2color((state->sgx)->sash_color));
}

static SCM g_set_data_color (SCM color) 
{
  snd_color *v; 
  SCM_ASSERT(snd_color_p(color), color, SCM_ARG1, "set-" S_data_color); 
  v = get_snd_color(color); 
  if (v) 
    {
      color_data(state, v->color);
      map_over_chans(state, update_graph, NULL);
    }
  return(color);
}

static SCM g_data_color(void) 
{
  #define H_data_color "(" S_data_color ") -> col" STR_OR " used to draw unselected data"
  return(pixel2color((state->sgx)->data_color));
}

static SCM g_set_selected_data_color (SCM color)
{
  snd_color *v; 
  chan_info *cp;
  SCM_ASSERT(snd_color_p(color), color, SCM_ARG1, "set-" S_selected_data_color); 
  v = get_snd_color(color); 
  if (v) 
    {
      color_selected_data(state, v->color);
      cp = selected_channel(state);
      if (cp) 
	{
	  color_selected_data(state, v->color);
	  update_graph(cp, NULL);
	}
    }
  return(color);
}

static SCM g_selected_data_color(void) 
{
  #define H_selected_data_color "(" S_selected_data_color ") -> col" STR_OR " used for selected data"
  return(pixel2color((state->sgx)->selected_data_color));
}

static SCM g_set_graph_color (SCM color) 
{
  snd_color *v; 
  SCM_ASSERT(snd_color_p(color), color, SCM_ARG1, "set-" S_graph_color);
  v = get_snd_color(color);
  if (v) 
    {
      color_graph(state, v->color);
      color_unselected_graphs(v->color);
    }
  return(color);
}

static SCM g_graph_color(void) 
{
  #define H_graph_color "(" S_graph_color ") -> background col" STR_OR " used for unselected data"
  return(pixel2color((state->sgx)->graph_color));
}

static SCM g_set_selected_graph_color (SCM color) 
{
  snd_color *v; 
  chan_info *cp;
  SCM_ASSERT(snd_color_p(color), color, SCM_ARG1, "set-" S_selected_graph_color);
  v = get_snd_color(color); 
  if (v) 
    {
      color_selected_graph(state, v->color);
      cp = selected_channel(state);
      if (cp) 
	{
#if USE_MOTIF
	  XtVaSetValues(channel_graph(cp), XmNbackground, v->color, NULL);
#else
	  set_background_and_redraw(channel_graph(cp), v->color);
#endif
	}
    }
  return(color);
}

static SCM g_selected_graph_color(void) 
{
  #define H_selected_graph_color "(" S_selected_graph_color ") -> background col" STR_OR " of selected data"
  return(pixel2color((state->sgx)->selected_graph_color));
}

static SCM g_set_pushed_button_color (SCM color) 
{
  snd_color *v; 
  SCM_ASSERT(snd_color_p(color), color, SCM_ARG1, "set-" S_pushed_button_color); 
  v = get_snd_color(color); 
  if (v) 
    {
      (state->sgx)->pushed_button_color = v->color;
      map_over_children(MAIN_SHELL(state), recolor_button, NULL);
    }
  return(color);
}

static SCM g_pushed_button_color(void) 
{
  #define H_pushed_button_color "(" S_pushed_button_color ") -> col" STR_OR " of a pushed button"
  return(pixel2color((state->sgx)->pushed_button_color));
}

static SCM g_basic_color(void) 
{
  #define H_basic_color "(" S_basic_color ") -> Snd's basic col" STR_OR
  return(pixel2color((state->sgx)->basic_color));
}

static SCM g_set_basic_color (SCM color) 
{
  snd_color *v; 
  COLOR_TYPE old_color;
  SCM_ASSERT(snd_color_p(color), color, SCM_ARG1, "set-" S_basic_color); 
  v = get_snd_color(color); 
  if (v) 
    {
      old_color = (state->sgx)->basic_color;
      (state->sgx)->basic_color = v->color; 
      map_over_children(MAIN_SHELL(state), recolor_everything, (void *)old_color);
    }
  return(color);
}


#endif


static SCM during_open_hook, after_open_hook, output_comment_hook;

SCM g_c_run_progn_hook (SCM hook, SCM args, const char *caller)
{
  /* Guile built-in scm_c_run_hook doesn't return the value of the hook procedure(s) and exits on error */
  SCM result = SCM_BOOL_F;
  SCM procs = SCM_HOOK_PROCEDURES (hook);
  while (NOT_NULL_P (procs))
    {
      if (args != SCM_LIST0)
	result = APPLY(SCM_CAR(procs), args, caller);
      else result = CALL0(SCM_CAR(procs), caller);
      procs = SCM_CDR (procs);
    }
  return(scm_return_first(result, args));
}

SCM g_c_run_or_hook (SCM hook, SCM args, const char *caller)
{
  /* Guile built-in scm_c_run_hook doesn't return the value of the hook procedure(s) and calls everything on the list */
  SCM result = SCM_BOOL_F; /* (or) -> #f */
  SCM procs = SCM_HOOK_PROCEDURES (hook);
  while (NOT_NULL_P (procs))
    {
      if (args != SCM_LIST0)
	result = APPLY(SCM_CAR(procs), args, caller);
      else result = CALL0(SCM_CAR(procs), caller);
      if (NOT_FALSE_P(result)) return(result);
      procs = SCM_CDR (procs);
    }
  return(scm_return_first(result, args));
}

SCM g_c_run_and_hook (SCM hook, SCM args, const char *caller)
{
  SCM result = SCM_BOOL_T; /* (and) -> #t */
  SCM procs = SCM_HOOK_PROCEDURES (hook);
  while (NOT_NULL_P (procs))
    {
      if (args != SCM_LIST0)
	result = APPLY(SCM_CAR(procs), args, caller);
      else result = CALL0(SCM_CAR(procs), caller);
      if (FALSE_P(result)) return(result);
      procs = SCM_CDR (procs);
    }
  return(scm_return_first(result, args));
}

void during_open(int fd, char *file, int reason)
{
  if (HOOKED(during_open_hook))
    g_c_run_progn_hook(during_open_hook,
		       SCM_LIST3(TO_SCM_INT(fd),
				 TO_SCM_STRING(file),
				 TO_SCM_INT(reason)),
		       S_during_open_hook);
}

void after_open(int index)
{
  if (HOOKED(after_open_hook))
    g_c_run_progn_hook(after_open_hook,
		       SCM_LIST1(TO_SMALL_SCM_INT(index)),
		       S_after_open_hook);
}


char *output_comment(file_info *hdr)
{
  char *com = NULL;
  if (hdr) com = mus_sound_comment(hdr->name);
  if (HOOKED(output_comment_hook))
    {
      SCM result;
      SCM procs = SCM_HOOK_PROCEDURES (output_comment_hook);
      int comment_size = 0;
      char *new_comment = NULL, *tmpstr = NULL;
      while (NOT_NULL_P (procs))
	{
	  result = CALL1(SCM_CAR(procs),
			 TO_SCM_STRING(com),
			 S_output_comment_hook);
	  tmpstr = TO_NEW_C_STRING(result);
	  if (tmpstr)
	    {
	      if ((snd_strlen(tmpstr) + snd_strlen(new_comment)) >= comment_size)
		{
		  comment_size = ((snd_strlen(tmpstr) + snd_strlen(new_comment)) * 2);
		  if (comment_size < 1024) 
		    comment_size = 1024;
		}
	      if (new_comment == NULL)
		new_comment = (char *)CALLOC(comment_size, sizeof(char));
	      else new_comment = (char *)REALLOC(new_comment, comment_size * sizeof(char));
	      strcat(new_comment, tmpstr);
	      free(tmpstr);
	    }
	  procs = SCM_CDR (procs);
	}
      return(new_comment);
    }
  else return(com);
}

void define_procedure_with_setter(char *get_name, SCM (*get_func)(), char *get_help,
				  char *set_name, SCM (*set_func)(), 
				  SCM local_doc,
				  int get_req, int get_opt, int set_req, int set_opt)
{
  scm_set_object_property_x(
    SCM_CDR(
      gh_define(get_name,
	scm_make_procedure_with_setter(
          gh_new_procedure("", SCM_FNC get_func, get_req, get_opt, 0),
	  gh_new_procedure(set_name, SCM_FNC set_func, set_req, set_opt, 0)
	  ))),
    local_doc,
    TO_SCM_STRING(get_help));
  /* still need to trap help output and send it to the listener */
}

void define_procedure_with_reversed_setter(char *get_name, SCM (*get_func)(), char *get_help,
					   char *set_name, SCM (*set_func)(), SCM (*reversed_set_func)(), 
					   SCM local_doc,
					   int get_req, int get_opt, int set_req, int set_opt)
{
  scm_set_object_property_x(
    SCM_CDR(
      gh_define(get_name,
	scm_make_procedure_with_setter(
          gh_new_procedure("", SCM_FNC get_func, get_req, get_opt, 0),
	  gh_new_procedure("", SCM_FNC reversed_set_func, set_req, set_opt, 0)
	  ))),
    local_doc,
    TO_SCM_STRING(get_help));
  /* still need to trap help output and send it to the listener */
  gh_new_procedure(set_name, SCM_FNC set_func, set_req, set_opt, 0);
}

#if HAVE_LADSPA
  void g_ladspa_to_snd(SCM local_doc);
#endif

#if HAVE_DLFCN_H
#include <dlfcn.h>
/* these are included because libtool's dlopen is incredibly stupid, as is libtool in general */

static SCM g_dlopen(SCM name)
{
  void *handle;
  handle = dlopen(TO_C_STRING(name), RTLD_LAZY);
  if (handle == NULL) return(TO_SCM_STRING(dlerror()));
  return(SND_WRAP(handle));
}

static SCM g_dlclose(SCM handle)
{
  return(TO_SCM_INT(dlclose((void *)(SND_UNWRAP(handle)))));
}

static SCM g_dlerror(void)
{
  return(TO_SCM_STRING(dlerror()));
}

static SCM g_dlinit(SCM handle, SCM func)
{
  typedef void *(*snd_dl_func)(void);
  void *proc;
  proc = dlsym((void *)(SND_UNWRAP(handle)), TO_C_STRING(func));
  if (proc == NULL) return(TO_SCM_STRING(dlerror()));
  ((snd_dl_func)proc)();
  return(SCM_BOOL_T);
}

static void g_init_dl(SCM local_doc)
{
  DEFINE_PROC("dlopen", SCM_FNC g_dlopen, 1, 0 ,0, "");
  DEFINE_PROC("dlclose", SCM_FNC g_dlclose, 1, 0 ,0, "");
  DEFINE_PROC("dlerror", SCM_FNC g_dlerror, 0, 0 ,0, "");
  DEFINE_PROC("dlinit", SCM_FNC g_dlinit, 2, 0 ,0, "");
  
}
#endif

void g_initialize_gh(snd_state *ss)
{
  SCM local_doc;
  state = ss;

  local_doc = scm_permanent_object(scm_string_to_symbol(TO_SCM_STRING("documentation")));
#if TIMING
  g_init_timing(local_doc);
#endif
  mus_sndlib2scm_initialize();

#if MUS_LITTLE_ENDIAN
  EVAL_STRING("(define little-endian? (lambda nil #t))");
#else
  EVAL_STRING("(define little-endian? (lambda nil #f))");
#endif

  /* ---------------- CONSTANTS ---------------- */

  #define H_amplitude_env "The value for " S_enved_target " that sets the envelope editor 'amp' button."
  #define H_spectrum_env "The value for " S_enved_target " that sets the envelope editor 'flt' button."
  #define H_srate_env "The value for " S_enved_target " that sets the envelope editor 'src' button."

  DEFINE_VAR(S_amplitude_env,         AMPLITUDE_ENV, H_amplitude_env);
  DEFINE_VAR(S_spectrum_env,          SPECTRUM_ENV,  H_spectrum_env);
  DEFINE_VAR(S_srate_env,             SRATE_ENV,     H_srate_env);

  #define H_graph_lines "The value for " S_graph_style " that causes graphs to use line-segments"
  #define H_graph_dots "The value for " S_graph_style " that causes graphs to use dots"
  #define H_graph_filled "The value for " S_graph_style " that causes graphs to use filled polygons"
  #define H_graph_dots_and_lines "The value for " S_graph_style " that causes graphs to use dots connected by lines"
  #define H_graph_lollipops "The value for " S_graph_style " that makes DSP engineers happy"

  DEFINE_VAR(S_graph_lines,           GRAPH_LINES,          H_graph_lines);
  DEFINE_VAR(S_graph_dots,            GRAPH_DOTS,           H_graph_dots);
  DEFINE_VAR(S_graph_filled,          GRAPH_FILLED,         H_graph_filled);
  DEFINE_VAR(S_graph_dots_and_lines,  GRAPH_DOTS_AND_LINES, H_graph_dots_and_lines);
  DEFINE_VAR(S_graph_lollipops,       GRAPH_LOLLIPOPS,      H_graph_lollipops);

  #define H_focus_left "The value for " S_zoom_focus_style " that causes zooming to maintain the left edge steady"
  #define H_focus_right "The value for " S_zoom_focus_style " that causes zooming to maintain the right edge steady"
  #define H_focus_middle "The value for " S_zoom_focus_style " that causes zooming to focus on the middle sample"
  #define H_focus_active "The value for " S_zoom_focus_style " that causes zooming to focus on the currently active object"

  DEFINE_VAR(S_focus_left,            FOCUS_LEFT,   H_focus_left);
  DEFINE_VAR(S_focus_right,           FOCUS_RIGHT,  H_focus_right);
  DEFINE_VAR(S_focus_active,          FOCUS_ACTIVE, H_focus_active);
  DEFINE_VAR(S_focus_middle,          FOCUS_MIDDLE, H_focus_middle);

  #define H_x_in_seconds "The value for " S_x_axis_style " that displays the x axis using seconds"
  #define H_x_in_samples "The value for " S_x_axis_style " that displays the x axis using sample numbers"
  #define H_x_to_one "The value for " S_x_axis_style " that displays the x axis using percentages"

  DEFINE_VAR(S_x_in_seconds,          X_IN_SECONDS, H_x_in_seconds);
  DEFINE_VAR(S_x_in_samples,          X_IN_SAMPLES, H_x_in_samples);
  DEFINE_VAR(S_x_to_one,              X_TO_ONE,     H_x_to_one);

  #define H_speed_as_float "The value for " S_speed_style " that interprets the speed slider as a float"
  #define H_speed_as_ratio "The value for " S_speed_style " that interprets the speed slider as a just-intonation ratio"
  #define H_speed_as_semitone "The value for " S_speed_style " that interprets the speed slider as a microtone (via " S_speed_tones ")"

  DEFINE_VAR(S_speed_as_float,        SPEED_AS_FLOAT,    H_speed_as_float);
  DEFINE_VAR(S_speed_as_ratio,        SPEED_AS_RATIO,    H_speed_as_ratio);
  DEFINE_VAR(S_speed_as_semitone,     SPEED_AS_SEMITONE, H_speed_as_semitone);

  #define H_channels_separate "The value for " S_channel_style " that causes channel graphs to occupy separate panes"
  #define H_channels_combined "The value for " S_channel_style " that causes channel graphs to occupy one panes (the 'unite' button)"
  #define H_channels_superimposed "The value for " S_channel_style " that causes channel graphs to occupy one pane and one axis"

  DEFINE_VAR(S_channels_separate,     CHANNELS_SEPARATE,     H_channels_separate);
  DEFINE_VAR(S_channels_combined,     CHANNELS_COMBINED,     H_channels_combined);
  DEFINE_VAR(S_channels_superimposed, CHANNELS_SUPERIMPOSED, H_channels_superimposed);

  #define H_cursor_in_view "The value for an " S_bind_key " function that causes it to shift the window so that the cursor is in the view"
  #define H_cursor_on_left "The value for an " S_bind_key " function that causes it to shift the window so that the cursor is at the left edge"
  #define H_cursor_on_right "The value for an " S_bind_key " function that causes it to shift the window so that the cursor is at the right edge"
  #define H_cursor_in_middle "The value for an " S_bind_key " function that causes it to shift the window so that the cursor is in the middle"
  #define H_cursor_update_display "The value for an " S_bind_key " function that causes it to redraw the graph"
  #define H_cursor_no_action "The value for an " S_bind_key " function that causes it do nothing with the graph window"
  #define H_keyboard_no_action "The value for an " S_bind_key " function that causes it do nothing upon return"

  DEFINE_VAR(S_cursor_in_view,        CURSOR_IN_VIEW,        H_cursor_in_view);
  DEFINE_VAR(S_cursor_on_left,        CURSOR_ON_LEFT,        H_cursor_on_left);
  DEFINE_VAR(S_cursor_on_right,       CURSOR_ON_RIGHT,       H_cursor_on_right);
  DEFINE_VAR(S_cursor_in_middle,      CURSOR_IN_MIDDLE,      H_cursor_in_middle);
  DEFINE_VAR(S_cursor_update_display, CURSOR_UPDATE_DISPLAY, H_cursor_update_display);
  DEFINE_VAR(S_cursor_no_action,      CURSOR_NO_ACTION,      H_cursor_no_action);
  DEFINE_VAR(S_keyboard_no_action,    KEYBOARD_NO_ACTION,    H_keyboard_no_action);

  #define H_cursor_cross "The value for " S_cursor_style " that causes is to be a cross (the default)"
  #define H_cursor_line "The value for " S_cursor_style " that causes is to be a full vertical line"

  DEFINE_VAR(S_cursor_cross,          CURSOR_CROSS, H_cursor_cross);
  DEFINE_VAR(S_cursor_line,           CURSOR_LINE,  H_cursor_line);

  #define H_show_all_axes "The value for " S_show_axes " that causes both the x and y axes to be displayed"
  #define H_show_no_axes "The value for " S_show_axes " that causes neither the x or y axes to be displayed"
  #define H_show_x_axis "The value for " S_show_axes " that causes only the x axis to be displayed"

  DEFINE_VAR(S_show_all_axes,         SHOW_ALL_AXES, H_show_all_axes);
  DEFINE_VAR(S_show_no_axes,          SHOW_NO_AXES,  H_show_no_axes);
  DEFINE_VAR(S_show_x_axis,           SHOW_X_AXIS,   H_show_x_axis);

  #define H_dont_normalize "The value for " S_normalize_fft " that causes the fft to display raw data"
  #define H_normalize_by_channel "The value for " S_normalize_fft " that causes the fft to be normalized in each channel independently"
  #define H_normalize_by_sound "The value for " S_normalize_fft " that causes the fft to be normalized across a sound's channels"
  #define H_normalize_globally "The value for " S_normalize_fft " that causes the fft to be normalized across all sounds"

  DEFINE_VAR(S_dont_normalize,        DONT_NORMALIZE,       H_dont_normalize);
  DEFINE_VAR(S_normalize_by_channel,  NORMALIZE_BY_CHANNEL, H_normalize_by_channel);
  DEFINE_VAR(S_normalize_by_sound,    NORMALIZE_BY_SOUND,   H_normalize_by_sound);
  DEFINE_VAR(S_normalize_globally,    NORMALIZE_GLOBALLY,   H_normalize_globally);


  /* ---------------- VARIABLES ---------------- */
  define_procedure_with_setter(S_ask_before_overwrite, SCM_FNC g_ask_before_overwrite, H_ask_before_overwrite,
			       "set-" S_ask_before_overwrite, SCM_FNC g_set_ask_before_overwrite, local_doc, 0, 0, 0, 1);

  define_procedure_with_setter(S_audio_output_device, SCM_FNC g_audio_output_device, H_audio_output_device,
			       "set-" S_audio_output_device, SCM_FNC g_set_audio_output_device, local_doc, 0, 0, 0, 1);

  define_procedure_with_setter(S_audio_input_device, SCM_FNC g_audio_input_device, H_audio_input_device,
			       "set-" S_audio_input_device, SCM_FNC g_set_audio_input_device, local_doc, 0, 0, 0, 1);

  define_procedure_with_setter(S_minibuffer_history_length, SCM_FNC g_minibuffer_history_length, H_minibuffer_history_length,
			       "set-" S_minibuffer_history_length, SCM_FNC g_set_minibuffer_history_length, local_doc, 0, 0, 0, 1);

  define_procedure_with_setter(S_dac_size, SCM_FNC g_dac_size, H_dac_size,
			       "set-" S_dac_size, SCM_FNC g_set_dac_size, local_doc, 0, 0, 0, 1);

  define_procedure_with_setter(S_dac_folding, SCM_FNC g_dac_folding, H_dac_folding,
			       "set-" S_dac_folding, SCM_FNC g_set_dac_folding, local_doc, 0, 0, 0, 1);

  define_procedure_with_setter(S_auto_resize, SCM_FNC g_auto_resize, H_auto_resize,
			       "set-" S_auto_resize, SCM_FNC g_set_auto_resize, local_doc, 0, 0, 0, 1);

  define_procedure_with_setter(S_auto_update, SCM_FNC g_auto_update, H_auto_update,
			       "set-" S_auto_update, SCM_FNC g_set_auto_update, local_doc, 0, 0, 0, 1);

  define_procedure_with_setter(S_filter_env_in_hz, SCM_FNC g_filter_env_in_hz, H_filter_env_in_hz,
			       "set-" S_filter_env_in_hz, SCM_FNC g_set_filter_env_in_hz, local_doc, 0, 0, 0, 1);

  define_procedure_with_setter(S_channel_style, SCM_FNC g_channel_style, H_channel_style,
			       "set-" S_channel_style, SCM_FNC g_set_channel_style, local_doc, 0, 0, 0, 1);

  define_procedure_with_setter(S_color_cutoff, SCM_FNC g_color_cutoff, H_color_cutoff,
			       "set-" S_color_cutoff, SCM_FNC g_set_color_cutoff, local_doc, 0, 0, 0, 1);

  define_procedure_with_setter(S_color_inverted, SCM_FNC g_color_inverted, H_color_inverted,
			       "set-" S_color_inverted, SCM_FNC g_set_color_inverted, local_doc, 0, 0, 0, 1);

  define_procedure_with_setter(S_color_scale, SCM_FNC g_color_scale, H_color_scale,
			       "set-" S_color_scale, SCM_FNC g_set_color_scale, local_doc, 0, 0, 0, 1);

  define_procedure_with_setter(S_corruption_time, SCM_FNC g_corruption_time, H_corruption_time,
			       "set-" S_corruption_time, SCM_FNC g_set_corruption_time, local_doc, 0, 0, 0, 1);

  define_procedure_with_setter(S_default_output_chans, SCM_FNC g_default_output_chans, H_default_output_chans,
			       "set-" S_default_output_chans, SCM_FNC g_set_default_output_chans, local_doc, 0, 0, 0, 1);

  define_procedure_with_setter(S_default_output_srate, SCM_FNC g_default_output_srate, H_default_output_srate,
			       "set-" S_default_output_srate, SCM_FNC g_set_default_output_srate, local_doc, 0, 0, 0, 1);

  define_procedure_with_setter(S_default_output_type, SCM_FNC g_default_output_type, H_default_output_type,
			       "set-" S_default_output_type, SCM_FNC g_set_default_output_type, local_doc, 0, 0, 0, 1);

  define_procedure_with_setter(S_default_output_format, SCM_FNC g_default_output_format, H_default_output_format,
			       "set-" S_default_output_format, SCM_FNC g_set_default_output_format, local_doc, 0, 0, 0, 1);

  define_procedure_with_setter(S_enved_base, SCM_FNC g_enved_base, H_enved_base,
			       "set-" S_enved_base, SCM_FNC g_set_enved_base, local_doc, 0, 0, 0, 1);

  define_procedure_with_setter(S_enved_power, SCM_FNC g_enved_power, H_enved_power,
			       "set-" S_enved_power, SCM_FNC g_set_enved_power, local_doc, 0, 0, 0, 1);

  define_procedure_with_setter(S_enved_clipping, SCM_FNC g_enved_clipping, H_enved_clipping,
			       "set-" S_enved_clipping, SCM_FNC g_set_enved_clipping, local_doc, 0, 0, 0, 1);

  define_procedure_with_setter(S_enved_exping, SCM_FNC g_enved_exping, H_enved_exping,
			       "set-" S_enved_exping, SCM_FNC g_set_enved_exping, local_doc, 0, 0, 0, 1);

  define_procedure_with_setter(S_enved_target, SCM_FNC g_enved_target, H_enved_target,
			       "set-" S_enved_target, SCM_FNC g_set_enved_target, local_doc, 0, 0, 0, 1);

  define_procedure_with_setter(S_enved_waving, SCM_FNC g_enved_waving, H_enved_waving,
			       "set-" S_enved_waving, SCM_FNC g_set_enved_waving, local_doc, 0, 0, 0, 1);

  define_procedure_with_setter(S_enved_dBing, SCM_FNC g_enved_dBing, H_enved_dBing,
			       "set-" S_enved_dBing, SCM_FNC g_set_enved_dBing, local_doc, 0, 0, 0, 1);

  define_procedure_with_setter(S_eps_file, SCM_FNC g_eps_file, H_eps_file,
			       "set-" S_eps_file, SCM_FNC g_set_eps_file, local_doc, 0, 0, 0, 1);

  define_procedure_with_setter(S_eps_left_margin, SCM_FNC g_eps_left_margin, H_eps_left_margin,
			       "set-" S_eps_left_margin, SCM_FNC g_set_eps_left_margin, local_doc, 0, 0, 0, 1);

  define_procedure_with_setter(S_eps_bottom_margin, SCM_FNC g_eps_bottom_margin, H_eps_bottom_margin,
			       "set-" S_eps_bottom_margin, SCM_FNC g_set_eps_bottom_margin, local_doc, 0, 0, 0, 1);

  define_procedure_with_setter(S_listener_prompt, SCM_FNC g_listener_prompt, H_listener_prompt,
			       "set-" S_listener_prompt, SCM_FNC g_set_listener_prompt, local_doc, 0, 0, 0, 1);

  define_procedure_with_setter(S_audio_state_file, SCM_FNC g_audio_state_file, H_audio_state_file,
			       "set-" S_audio_state_file, SCM_FNC g_set_audio_state_file, local_doc, 0, 0, 0, 1);

  define_procedure_with_setter(S_filter_env_order, SCM_FNC g_filter_env_order, H_filter_env_order,
			       "set-" S_filter_env_order, SCM_FNC g_set_filter_env_order, local_doc, 0, 0, 0, 1);

  define_procedure_with_setter(S_fit_data_on_open, SCM_FNC g_fit_data_on_open, H_fit_data_on_open,
			       "set-" S_fit_data_on_open, SCM_FNC g_set_fit_data_on_open, local_doc, 0, 0, 0, 1);

  define_procedure_with_setter(S_movies, SCM_FNC g_movies, H_movies,
			       "set-" S_movies, SCM_FNC g_set_movies, local_doc, 0, 0, 0, 1);

  define_procedure_with_setter(S_selection_creates_region, SCM_FNC g_selection_creates_region, H_selection_creates_region,
			       "set-" S_selection_creates_region, SCM_FNC g_set_selection_creates_region, local_doc, 0, 0, 0, 1);

  define_procedure_with_setter(S_normalize_on_open, SCM_FNC g_normalize_on_open, H_normalize_on_open,
			       "set-" S_normalize_on_open, SCM_FNC g_set_normalize_on_open, local_doc, 0, 0, 0, 1);

  define_procedure_with_setter(S_prefix_arg, SCM_FNC g_prefix_arg, H_prefix_arg,
			       "set-" S_prefix_arg, SCM_FNC g_set_prefix_arg, local_doc, 0, 0, 0, 1);

  define_procedure_with_setter(S_print_length, SCM_FNC g_print_length, H_print_length,
			       "set-" S_print_length, SCM_FNC g_set_print_length, local_doc, 0, 0, 0, 1);

  define_procedure_with_setter(S_previous_files_sort, SCM_FNC g_previous_files_sort, H_previous_files_sort,
			       "set-" S_previous_files_sort, SCM_FNC g_set_previous_files_sort, local_doc, 0, 0, 0, 1);

  define_procedure_with_setter(S_raw_chans, SCM_FNC g_raw_chans, H_raw_chans,
			       "set-" S_raw_chans, SCM_FNC g_set_raw_chans, local_doc, 0, 0, 0, 1);

  define_procedure_with_setter(S_raw_format, SCM_FNC g_raw_format, H_raw_format,
			       "set-" S_raw_format, SCM_FNC g_set_raw_format, local_doc, 0, 0, 0, 1);

  define_procedure_with_setter(S_raw_srate, SCM_FNC g_raw_srate, H_raw_srate,
			       "set-" S_raw_srate, SCM_FNC g_set_raw_srate, local_doc, 0, 0, 0, 1);

  define_procedure_with_setter(S_save_state_on_exit, SCM_FNC g_save_state_on_exit, H_save_state_on_exit,
			       "set-" S_save_state_on_exit, SCM_FNC g_set_save_state_on_exit, local_doc, 0, 0, 0, 1);

  define_procedure_with_setter(S_show_indices, SCM_FNC g_show_indices, H_show_indices,
			       "set-" S_show_indices, SCM_FNC g_set_show_indices, local_doc, 0, 0, 0, 1);

  define_procedure_with_setter(S_show_backtrace, SCM_FNC g_show_backtrace, H_show_backtrace,
			       "set-" S_show_backtrace, SCM_FNC g_set_show_backtrace, local_doc, 0, 0, 0, 1);

  define_procedure_with_setter(S_show_usage_stats, SCM_FNC g_show_usage_stats, H_show_usage_stats,
			       "set-" S_show_usage_stats, SCM_FNC g_set_show_usage_stats, local_doc, 0, 0, 0, 1);

  define_procedure_with_setter(S_sinc_width, SCM_FNC g_sinc_width, H_sinc_width,
			       "set-" S_sinc_width, SCM_FNC g_set_sinc_width, local_doc, 0, 0, 0, 1);

  define_procedure_with_setter(S_colormap, SCM_FNC g_color_map, H_colormap,
			       "set-" S_colormap, SCM_FNC g_set_color_map, local_doc, 0, 0, 0, 1);

  define_procedure_with_setter(S_temp_dir, SCM_FNC g_temp_dir, H_temp_dir,
			       "set-" S_temp_dir, SCM_FNC g_set_temp_dir, local_doc, 0, 0, 0, 1);

  define_procedure_with_setter(S_save_dir, SCM_FNC g_save_dir, H_save_dir,
			       "set-" S_save_dir, SCM_FNC g_set_save_dir, local_doc, 0, 0, 0, 1);

  define_procedure_with_setter(S_trap_segfault, SCM_FNC g_trap_segfault, H_trap_segfault,
			       "set-" S_trap_segfault, SCM_FNC g_set_trap_segfault, local_doc, 0, 0, 0, 1);

  define_procedure_with_setter(S_show_selection_transform, SCM_FNC g_show_selection_transform, H_show_selection_transform,
			       "set-" S_show_selection_transform, SCM_FNC g_set_show_selection_transform, local_doc, 0, 0, 0, 1);

  define_procedure_with_setter(S_with_mix_tags, SCM_FNC g_with_mix_tags, H_with_mix_tags,
			       "set-" S_with_mix_tags, SCM_FNC g_set_with_mix_tags, local_doc, 0, 0, 0, 1);

  define_procedure_with_setter(S_use_raw_defaults, SCM_FNC g_use_raw_defaults, H_use_raw_defaults,
			       "set-" S_use_raw_defaults, SCM_FNC g_set_use_raw_defaults, local_doc, 0, 0, 0, 1);

  define_procedure_with_setter(S_use_sinc_interp, SCM_FNC g_use_sinc_interp, H_use_sinc_interp,
			       "set-" S_use_sinc_interp, SCM_FNC g_set_use_sinc_interp, local_doc, 0, 0, 0, 1);

  define_procedure_with_setter(S_data_clipped, SCM_FNC g_data_clipped, H_data_clipped,
			       "set-" S_data_clipped, SCM_FNC g_set_data_clipped, local_doc, 0, 0, 0, 1);

  define_procedure_with_setter(S_vu_font, SCM_FNC g_vu_font, H_vu_font,
			       "set-" S_vu_font, SCM_FNC g_set_vu_font, local_doc, 0, 0, 0, 1);

  define_procedure_with_setter(S_vu_font_size, SCM_FNC g_vu_font_size, H_vu_font_size,
			       "set-" S_vu_font_size, SCM_FNC g_set_vu_font_size, local_doc, 0, 0, 0, 1);

  define_procedure_with_setter(S_vu_size, SCM_FNC g_vu_size, H_vu_size,
			       "set-" S_vu_size, SCM_FNC g_set_vu_size, local_doc, 0, 0, 0, 1);

  define_procedure_with_setter(S_window_x, SCM_FNC g_window_x, H_window_x,
			       "set-" S_window_x, SCM_FNC g_set_window_x, local_doc, 0, 0, 0, 1);

  define_procedure_with_setter(S_window_y, SCM_FNC g_window_y, H_window_y,
			       "set-" S_window_y, SCM_FNC g_set_window_y, local_doc, 0, 0, 0, 1);

  define_procedure_with_setter(S_x_axis_style, SCM_FNC g_x_axis_style, H_x_axis_style,
			       "set-" S_x_axis_style, SCM_FNC g_set_x_axis_style, local_doc, 0, 0, 0, 1);

  define_procedure_with_setter(S_zoom_focus_style, SCM_FNC g_zoom_focus_style, H_zoom_focus_style,
			       "set-" S_zoom_focus_style, SCM_FNC g_set_zoom_focus_style, local_doc, 0, 0, 0, 1);

  define_procedure_with_setter(S_help_text_font, SCM_FNC g_help_text_font, H_help_text_font,
			       "set-" S_help_text_font, SCM_FNC g_set_help_text_font, local_doc, 0, 0, 0, 1);

  define_procedure_with_setter(S_tiny_font, SCM_FNC g_tiny_font, H_tiny_font,
			       "set-" S_tiny_font, SCM_FNC g_set_tiny_font, local_doc, 0, 0, 0, 1);

  define_procedure_with_setter(S_button_font, SCM_FNC g_button_font, H_button_font,
			       "set-" S_button_font, SCM_FNC g_set_button_font, local_doc, 0, 0, 0, 1);

  define_procedure_with_setter(S_bold_button_font, SCM_FNC g_bold_button_font, H_bold_button_font,
			       "set-" S_bold_button_font, SCM_FNC g_set_bold_button_font, local_doc, 0, 0, 0, 1);

  define_procedure_with_setter(S_axis_label_font, SCM_FNC g_axis_label_font, H_axis_label_font,
			       "set-" S_axis_label_font, SCM_FNC g_set_axis_label_font, local_doc, 0, 0, 0, 1);

  define_procedure_with_setter(S_axis_numbers_font, SCM_FNC g_axis_numbers_font, H_axis_numbers_font,
			       "set-" S_axis_numbers_font, SCM_FNC g_set_axis_numbers_font, local_doc, 0, 0, 0, 1);

  define_procedure_with_setter(S_listener_font, SCM_FNC g_listener_font, H_listener_font,
			       "set-" S_listener_font, SCM_FNC g_set_listener_font, local_doc, 0, 0, 0, 1);

  define_procedure_with_setter(S_window_width, SCM_FNC g_window_width, H_window_width,
			       "set-" S_window_width, SCM_FNC g_set_window_width, local_doc, 0, 0, 0, 1);  

  define_procedure_with_setter(S_window_height, SCM_FNC g_window_height, H_window_height,
			       "set-" S_window_height, SCM_FNC g_set_window_height, local_doc, 0, 0, 0, 1);


#if (!USE_NO_GUI)
  #if HAVE_HTML
  scm_add_feature("snd-html");
  define_procedure_with_setter(S_html_dir, SCM_FNC g_html_dir, H_html_dir,
			       "set-" S_html_dir, SCM_FNC g_set_html_dir, local_doc, 0, 0, 1, 0);
  #endif

  define_procedure_with_setter(S_selection_color, SCM_FNC g_selection_color, H_selection_color,
			       "set-" S_selection_color, SCM_FNC g_set_selection_color, local_doc, 0, 0, 1, 0);

  define_procedure_with_setter(S_zoom_color, SCM_FNC g_zoom_color, H_zoom_color,
			       "set-" S_zoom_color, SCM_FNC g_set_zoom_color, local_doc, 0, 0, 1, 0);

  define_procedure_with_setter(S_position_color, SCM_FNC g_position_color, H_position_color,
			       "set-" S_position_color, SCM_FNC g_set_position_color, local_doc, 0, 0, 1, 0);

  define_procedure_with_setter(S_mark_color, SCM_FNC g_mark_color, H_mark_color,
			       "set-" S_mark_color, SCM_FNC g_set_mark_color, local_doc, 0, 0, 1, 0);

  define_procedure_with_setter(S_listener_color, SCM_FNC g_listener_color, H_listener_color,
			       "set-" S_listener_color, SCM_FNC g_set_listener_color, local_doc, 0, 0, 1, 0);

  define_procedure_with_setter(S_listener_text_color, SCM_FNC g_listener_text_color, H_listener_text_color,
			       "set-" S_listener_text_color, SCM_FNC g_set_listener_text_color, local_doc, 0, 0, 1, 0);

  define_procedure_with_setter(S_enved_waveform_color, SCM_FNC g_enved_waveform_color, H_enved_waveform_color,
			       "set-" S_enved_waveform_color, SCM_FNC g_set_enved_waveform_color, local_doc, 0, 0, 1, 0);

  define_procedure_with_setter(S_filter_waveform_color, SCM_FNC g_filter_waveform_color, H_filter_waveform_color,
			       "set-" S_filter_waveform_color, SCM_FNC g_set_filter_waveform_color, local_doc, 0, 0, 1, 0);

  define_procedure_with_setter(S_highlight_color, SCM_FNC g_highlight_color, H_highlight_color,
			       "set-" S_highlight_color, SCM_FNC g_set_highlight_color, local_doc, 0, 0, 1, 0);

  define_procedure_with_setter(S_cursor_color, SCM_FNC g_cursor_color, H_cursor_color,
			       "set-" S_cursor_color, SCM_FNC g_set_cursor_color, local_doc, 0, 0, 1, 0);

  define_procedure_with_setter(S_mix_color, SCM_FNC g_mix_color, H_mix_color,
			       "set-" S_mix_color, SCM_FNC g_set_mix_color, local_doc, 0, 1, 1, 1);

  define_procedure_with_setter(S_selected_mix_color, SCM_FNC g_selected_mix_color, H_selected_mix_color,
			       "set-" S_selected_mix_color, SCM_FNC g_set_selected_mix_color, local_doc, 0, 1, 1, 1);

  define_procedure_with_setter(S_text_focus_color, SCM_FNC g_text_focus_color, H_text_focus_color,
			       "set-" S_text_focus_color, SCM_FNC g_set_text_focus_color, local_doc, 0, 0, 1, 0);

  define_procedure_with_setter(S_sash_color, SCM_FNC g_sash_color, H_sash_color,
			       "set-" S_sash_color, SCM_FNC g_set_sash_color, local_doc, 0, 0, 1, 0);

  define_procedure_with_setter(S_data_color, SCM_FNC g_data_color, H_data_color,
			       "set-" S_data_color, SCM_FNC g_set_data_color, local_doc, 0, 0, 1, 0);

  define_procedure_with_setter(S_graph_color, SCM_FNC g_graph_color, H_graph_color,
			       "set-" S_graph_color, SCM_FNC g_set_graph_color, local_doc, 0, 0, 1, 0);

  define_procedure_with_setter(S_selected_graph_color, SCM_FNC g_selected_graph_color, H_selected_graph_color,
			       "set-" S_selected_graph_color, SCM_FNC g_set_selected_graph_color, local_doc, 0, 0, 1, 0);

  define_procedure_with_setter(S_selected_data_color, SCM_FNC g_selected_data_color, H_selected_data_color,
			       "set-" S_selected_data_color, SCM_FNC g_set_selected_data_color, local_doc, 0, 0, 1, 0);

  define_procedure_with_setter(S_basic_color, SCM_FNC g_basic_color, H_basic_color,
			       "set-" S_basic_color, SCM_FNC g_set_basic_color, local_doc, 0, 0, 1, 0);

  define_procedure_with_setter(S_pushed_button_color, SCM_FNC g_pushed_button_color, H_pushed_button_color,
			       "set-" S_pushed_button_color, SCM_FNC g_set_pushed_button_color, local_doc, 0, 0, 1, 0);
#endif


  /* ---------------- FUNCTIONS ---------------- */

  DEFINE_PROC(S_snd_tempnam,         g_snd_tempnam, 0, 0, 0,         H_snd_tempnam);
  DEFINE_PROC("set-" S_oss_buffers,  g_set_oss_buffers, 2, 0, 0,     H_set_oss_buffers);
  DEFINE_PROC(S_update_usage_stats,  g_update_usage_stats, 0, 0, 0,  H_update_usage_stats);
#if HAVE_OSS
  DEFINE_PROC(S_clear_audio_inputs,  g_clear_audio_inputs, 0, 0, 0,  H_clear_audio_inputs);
#endif
  DEFINE_PROC(S_enved_dialog,        g_enved_dialog, 0, 0, 0,        H_enved_dialog);
  DEFINE_PROC(S_color_dialog,        g_color_dialog, 0, 0, 0,        H_color_dialog);
  DEFINE_PROC(S_orientation_dialog,  g_orientation_dialog, 0, 0, 0,  H_orientation_dialog);
  DEFINE_PROC(S_transform_dialog,    g_transform_dialog, 0, 0, 0,    H_transform_dialog);
  DEFINE_PROC(S_file_dialog,         g_file_dialog, 0, 0, 0,         H_file_dialog);
  DEFINE_PROC(S_edit_header_dialog,  g_edit_header_dialog, 0, 1, 0,  H_edit_header_dialog);
  DEFINE_PROC(S_help_dialog,         g_help_dialog, 2, 0, 0,         H_help_dialog);
  DEFINE_PROC(S_mix_panel,           g_mix_panel, 0, 0, 0,           H_mix_panel);

  DEFINE_PROC(S_max_sounds,          g_max_sounds, 0, 0, 0,          H_max_sounds);
  DEFINE_PROC(S_sounds,              g_sounds, 0, 0, 0,              H_sounds);
  DEFINE_PROC(S_yes_or_no_p,         g_yes_or_no_p, 1, 0, 0,         H_yes_or_no_p);
  DEFINE_PROC(S_update_graph,        g_update_graph, 0, 2, 0,        H_update_graph);
  DEFINE_PROC(S_update_lisp_graph,   g_update_lisp_graph, 0, 2, 0,   H_update_lisp_graph);
  DEFINE_PROC(S_update_fft,          g_update_fft, 0, 2, 0,          H_update_fft);
  DEFINE_PROC(S_abort,               g_abort, 0, 0, 0,               H_abort);
  DEFINE_PROC(S_dismiss_all_dialogs, g_dismiss_all_dialogs, 0, 0, 0, H_dismiss_all_dialogs);
  DEFINE_PROC(S_c_g,                 g_abortq, 0, 0, 0,              H_abortQ);
  DEFINE_PROC(S_snd_version,         g_snd_version, 0, 0, 0,         H_snd_version);
  DEFINE_PROC(S_show_listener,       g_show_listener, 0, 0, 0,       H_show_listener);
  DEFINE_PROC(S_hide_listener,       g_hide_listener, 0, 0, 0,       H_hide_listener);
  DEFINE_PROC(S_activate_listener,   g_activate_listener, 0, 0, 0,   H_activate_listener);
  DEFINE_PROC(S_normalize_view,      g_normalize_view, 0, 0, 0,      H_normalize_view);
  DEFINE_PROC(S_open_sound_file,     g_open_sound_file, 0, 4, 0,     H_open_sound_file);
  DEFINE_PROC(S_close_sound_file,    g_close_sound_file, 2, 0, 0,    H_close_sound_file);
  DEFINE_PROC(S_vct_sound_file,      vct2soundfile, 3, 0, 0,         H_vct_sound_file);
  DEFINE_PROC(S_graph,               g_graph, 1, 8, 0,               H_graph);
  DEFINE_PROC(S_samples_vct,         samples2vct, 0, 6, 0,           H_samples2vct);
  DEFINE_PROC(S_samples2sound_data,  samples2sound_data, 0, 7, 0,    H_samples2sound_data);
  DEFINE_PROC(S_start_progress_report, g_start_progress_report, 0, 1, 0, H_start_progress_report);
  DEFINE_PROC(S_finish_progress_report, g_finish_progress_report, 0, 1, 0, H_finish_progress_report);
  DEFINE_PROC(S_progress_report,     g_progress_report, 1, 4, 0,     H_progress_report);
  DEFINE_PROC(S_snd_print,           g_snd_print, 1, 0, 0,           H_snd_print);
  DEFINE_PROC(S_describe_audio,      g_describe_audio, 0, 0, 0,      H_describe_audio);
  DEFINE_PROC("mus-audio-describe",  g_describe_audio, 0, 0, 0,      H_describe_audio);


  #define H_during_open_hook S_during_open_hook " (fd name reason) is called after file is opened, but before data has been read."

  #define H_after_open_hook S_after_open_hook " (snd) is called just before the new file's window is displayed. \
This provides a way to set various sound-specific defaults."

  #define H_output_comment_hook S_output_comment_hook " (str) is called in Save-As dialog, passed current sound's comment, if any. \
If more than one hook function, results are concatenated. If none, the current comment is used.\n\
  (add-hook! output-comment-hook\n\
    (lambda (str)\n\
      (string-append \"written \"\n\
        (strftime \"%a %d-%b-%Y %H:%M %Z\"\n\
          (localtime (current-time))))))"

  during_open_hook =    MAKE_HOOK(S_during_open_hook, 3,    H_during_open_hook);    /* args = fd filename reason */
  after_open_hook =     MAKE_HOOK(S_after_open_hook, 1,     H_after_open_hook);     /* args = sound */
  output_comment_hook = MAKE_HOOK(S_output_comment_hook, 1, H_output_comment_hook); /* arg = current mus_sound_comment(hdr) if any */

  g_init_marks(local_doc);
  g_init_regions(local_doc);
  g_init_selection(local_doc);
  g_init_dac(local_doc);
  init_vct();
  init_mus2scm_module();
  g_initialize_xgh(state, local_doc);
  g_initialize_xgfile(local_doc);
  g_init_gxutils(local_doc);
  g_init_mix(local_doc);
  g_init_chn(local_doc);
  g_init_kbd(local_doc);
  g_init_sig(local_doc);
  g_init_print(local_doc);
  g_init_errors(local_doc);
  g_init_fft(local_doc);
  g_init_edits(local_doc);
  g_init_listener(local_doc);
  g_init_help(local_doc);
  g_init_menu(local_doc);
  g_init_main(local_doc);
  g_init_snd(local_doc);
  g_init_file(local_doc);
  g_init_data(local_doc);
  g_init_env(local_doc);
  g_init_recorder(local_doc);
  g_init_gxenv(local_doc);
  g_init_gxmenu(local_doc);
  g_init_find(local_doc);
#if (!USE_NO_GUI)
  g_init_axis(local_doc);
  g_init_gxmain(local_doc);
  g_init_gxlistener(local_doc);
  g_init_gxchn(local_doc);
  g_init_draw(local_doc);
  g_init_gxdrop(local_doc);
#endif
#if HAVE_DLFCN_H
  g_init_dl(local_doc);
#endif
#if HAVE_LADSPA
  g_ladspa_to_snd(local_doc);
#endif

  EVAL_STRING("(define unbind-key\
                 (lambda (key state)\
                   \"(unbind-key key state) undoes the effect of a prior bind-key call\"\
                   (bind-key key state #f)))");

  EVAL_STRING("(defmacro defvar (a b)\
                 `(begin\
                    (define , a , b)\
                    (define-envelope (symbol->string ', a) , b)))");
  /* this is trying to keep track of envelopes for the envelope editor */

  EVAL_STRING("(define (" S_snd_apropos " val) (snd-print (with-output-to-string (lambda () (apropos val)))))");
  EVAL_STRING("(read-set! keywords 'prefix)");
  EVAL_STRING("(print-enable 'source)");  /* added 13-Feb-01 */

  /* from ice-9/r4rs.scm but with output to snd listener */
  EVAL_STRING("(define snd-last-file-loaded #f)");
  EVAL_STRING("(set! %load-hook (lambda (filename)\
                                  (set! snd-last-file-loaded filename)\
                                  (if %load-verbosely\
                                    (snd-print (format #f \";;; loading ~S\" filename)))))");

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

#ifndef __GNUC__
#if (!HAVE_GUILE) && (!HAVE_LIBREP)
SCM scm_return_first(SCM a, ...)
{
  return(a);
}
#endif
#endif
