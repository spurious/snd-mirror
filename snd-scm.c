#include "snd.h"
#include "vct.h"
#include "clm2scm.h"
#include "sndlib-strings.h"

static snd_state *state = NULL;


/* -------- documentation for hooks -------- */

SCM snd_create_hook(const char *name, int args, const char *help, SCM local_doc)
{
  SCM hook;
#if HAVE_GUILE
  hook = scm_permanent_object(scm_make_hook(TO_SMALL_SCM_INT(args)));
  scm_set_object_property_x(hook, local_doc, TO_SCM_STRING(help));
  SND_DEFINE(name, hook);
#endif
  return(hook);
}


/* -------- protect SCM vars from GC -------- */

static SCM gc_protection;
static int gc_protection_size = 0;
#define DEFAULT_GC_VALUE INTEGER_ZERO
static int gc_last_cleared = -1;
static int gc_last_set = -1;

void snd_protect(SCM obj)
{
  int i, old_size;
  SCM tmp;
  SCM *gcdata;
  if (gc_protection_size == 0)
    {
      gc_protection_size = 128;
      /* we don't know the size in advance since each channel can have its own edit/undo hooks */
      gc_protection = MAKE_VECTOR(gc_protection_size, DEFAULT_GC_VALUE);
      MAKE_PERMANENT(gc_protection);
      VECTOR_SET(gc_protection, 0, obj);
      gc_last_set = 0;
    }
  else
    {
      gcdata = VECTOR_ELEMENTS(gc_protection);
      if ((gc_last_cleared >= 0) && 
	  EQ_P(gcdata[gc_last_cleared], DEFAULT_GC_VALUE))
	{
	  VECTOR_SET(gc_protection, gc_last_cleared, obj);
	  gc_last_set = gc_last_cleared;
	  gc_last_cleared = -1;
	  return;
	}
      for (i = 0; i < gc_protection_size; i++)
	if (EQ_P(gcdata[i], DEFAULT_GC_VALUE))
	  {
	    VECTOR_SET(gc_protection, i, obj);
	    gc_last_set = i;
	    return;
	  }
      tmp = gc_protection;
      old_size = gc_protection_size;
      gc_protection_size *= 2;
      gc_protection = MAKE_VECTOR(gc_protection_size, DEFAULT_GC_VALUE);
      MAKE_PERMANENT(gc_protection);
      for (i = 0; i < old_size; i++)
	{
	  VECTOR_SET(gc_protection, i, VECTOR_REF(tmp, i));
	  VECTOR_SET(tmp, i, DEFAULT_GC_VALUE);
	}
      VECTOR_SET(gc_protection, old_size, obj);
      gc_last_set = old_size;
    }
}

void snd_unprotect(SCM obj)
{
  int i;
  SCM *gcdata;
  gcdata = VECTOR_ELEMENTS(gc_protection);
  if ((gc_last_set >= 0) && 
      (EQ_P(gcdata[gc_last_set], obj)))
    {
      VECTOR_SET(gc_protection, gc_last_set, DEFAULT_GC_VALUE);
      gc_last_cleared = gc_last_set;
      gc_last_set = -1;
      return;
    }
  for (i = 0; i < gc_protection_size; i++)
    if (EQ_P(gcdata[i], obj))
      {
	VECTOR_SET(gc_protection, i, DEFAULT_GC_VALUE);
	gc_last_cleared = i;
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

#define MAX_ERROR_STRING_LENGTH 0
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
				       UNDEFINED_VALUE),
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
    WRITE_STRING(": ", lport);
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
      if (NOT_FALSE_P(CAR(throw_args)))
	{
	  scm_display(CAR(throw_args), port);
	  WRITE_STRING(": ", port);
	}
      if (LIST_LENGTH(throw_args) > 1)
	{
	  if (EQ_P(tag, NO_SUCH_FILE))
	    {
	      scm_display(tag, port);
	      WRITE_STRING(" \"", port);
	      scm_display(CADR(throw_args), port);
	      WRITE_STRING("\" ", port);
	      if (LIST_LENGTH(throw_args) > 2)
		scm_display(CDDR(throw_args), port);
	    }
	  else
	    {
	      if ((EQ_P(tag, NO_SUCH_SOUND)) || (EQ_P(tag, NO_SUCH_MIX)) || (EQ_P(tag, NO_SUCH_MARK)) ||
		  (EQ_P(tag, NO_SUCH_MENU)) || (EQ_P(tag, NO_SUCH_REGION)) || (EQ_P(tag, MUS_MISC_ERROR)) ||
		  (EQ_P(tag, NO_SUCH_CHANNEL)) || (EQ_P(tag, NO_SUCH_EDIT)) ||
		  (EQ_P(tag, NO_SUCH_AXIS_INFO)) || (EQ_P(tag, NO_SUCH_AXIS_CONTEXT)) ||
		  (EQ_P(tag, CANNOT_SAVE)) || (EQ_P(tag, CANNOT_PRINT)) || (EQ_P(tag, BAD_ARITY)) ||
		  (EQ_P(tag, IMPOSSIBLE_BOUNDS)) || (EQ_P(tag, NO_SUCH_SAMPLE)))
		{
		  scm_display(tag, port);
		  WRITE_STRING(" ", port);
		  scm_display(throw_args, port);
		}
	      else
		{
		  stmp = CADR(throw_args);
		  if ((STRING_P(stmp)) && (LIST_LENGTH(throw_args) > 2))
		    scm_display_error_message(stmp, CADDR(throw_args), port);
		  else scm_display(tag, port);
		  if (show_backtrace(state))
		    {
#if 0
		      {
			SCM oldport; /* This code from the guile mailing list, but doesn't seem to be needed here after all */
			SCM str;
			oldport = scm_current_output_port();
			scm_set_current_output_port(
			  scm_mkstrport(SCM_INUM0,
					scm_make_string(SCM_INUM0, UNDEFINED_VALUE),
					SCM_OPN | SCM_WRTNG,
					__FUNCTION__));
#endif			
#if HAVE_SCM_C_DEFINE
		      stack = scm_fluid_ref(VARIABLE_REF(scm_the_last_stack_fluid_var));
#else
		      stack = scm_fluid_ref(CDR(scm_the_last_stack_fluid));
#endif
		      if (NOT_FALSE_P(stack)) 
			scm_display_backtrace(stack, port, UNDEFINED_VALUE, UNDEFINED_VALUE);
#if 0
		      str = scm_strport_to_string(scm_current_output_port());
		      scm_set_current_output_port(oldport);
		      scm_display(str, port);
		      }
#endif
		    }
		}
	    }
	}
      else scm_display(tag, port);
    }
  else 
    {
      scm_display(tag, port);
      WRITE_STRING(": ", port);
      scm_display(throw_args, port);
    }
  possible_code = (char *)data;
  if ((possible_code) && 
      (snd_strlen(possible_code) < PRINT_BUFFER_SIZE))
    {
      /* not actually sure if this is always safe */
      WRITE_STRING("\n; ", port);
      WRITE_STRING(possible_code, port);
    }
  if (last_file_loaded)
    {
      /* sigh -- scm_current_load_port is #f so can't use scm_port_filename etc */
      WRITE_STRING("\n(while loading \"", port);
      WRITE_STRING(last_file_loaded, port);
      WRITE_STRING("\")", port);
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
      if (state->listening)
	listener_append_and_prompt(state, name_buf);
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
#if HAVE_GUILE
  result = scm_internal_stack_catch(tag, body, body_data, handler, handler_data);
#endif
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
  return(snd_internal_stack_catch(TRUE_VALUE, body, body_data, snd_catch_scm_error, (void *)caller));
}

char *procedure_ok(SCM proc, int args, const char *caller, const char *arg_name, int argn)
{
  /* if string returned, needs to be freed */
  SCM arity_list;
  int rargs, oargs, restargs;
#if TIMING
  return(NULL);
#endif
  if (!(PROCEDURE_P(proc)))
    {
      if (NOT_FALSE_P(proc)) /* #f as explicit arg to clear */
	return(mus_format("%s (%s arg %d) is not a procedure!", arg_name, caller, argn));
    }
  else
    {
      arity_list = ARITY(proc);
      snd_protect(arity_list);
      rargs = TO_SMALL_C_INT(CAR(arity_list));
      oargs = TO_SMALL_C_INT(CADR(arity_list));
      restargs = ((TRUE_P(CADDR(arity_list))) ? 1 : 0);
      snd_unprotect(arity_list);
      if (rargs > args)
	return(mus_format("%s function (%s arg %d) should take %d argument%s, but instead requires %d",
			  arg_name, caller, argn, args, (args != 1) ? "s" : "", rargs));
      if ((restargs == 0) && ((rargs + oargs) < args))
	return(mus_format("%s function (%s arg %d) should accept at least %d argument%s, but instead accepts only %d",
			  arg_name, caller, argn, args, (args != 1) ? "s" : "", rargs + oargs));
    }
  return(NULL);
}

int procedure_ok_with_error(SCM proc, int req_args, const char *caller, const char *arg_name, int argn)
{
  char *errmsg;
  errmsg = procedure_ok(proc, req_args, caller, arg_name, argn);
  if (errmsg)
    {
      snd_error(errmsg);
      FREE(errmsg);
      return(0);
    }
  return(1);
}

SCM snd_no_such_file_error(const char *caller, SCM filename)
{
  ERROR(NO_SUCH_FILE,
	LIST_3(TO_SCM_STRING(caller),
	       filename,
	       TO_SCM_STRING(strerror(errno))));
  return(FALSE_VALUE);
}

SCM snd_no_such_channel_error(const char *caller, SCM snd, SCM chn)
{
  ERROR(NO_SUCH_CHANNEL,
	LIST_3(TO_SCM_STRING(caller),
	       snd,
	       chn));
  return(FALSE_VALUE);
}

SCM snd_no_active_selection_error(const char *caller)
{
  ERROR(NO_ACTIVE_SELECTION,
	LIST_1(TO_SCM_STRING(caller)));
  return(FALSE_VALUE);
}

SCM snd_bad_arity_error(const char *caller, SCM errstr, SCM proc)
{
  ERROR(BAD_ARITY,
	LIST_3(TO_SCM_STRING(caller),
	       errstr,
	       proc));
  return(FALSE_VALUE);
}



/* -------- various evaluators (within our error handler) -------- */

SCM eval_str_wrapper(void *data)
{
  return(EVAL_STRING((char *)data));
}

SCM eval_form_wrapper(void *data)
{
#if (SCM_DEBUG_TYPING_STRICTNESS == 2)
  return(FALSE_VALUE);
#else
  return(EVAL_FORM(data));
#endif
}

static SCM eval_file_wrapper(void *data)
{
  last_file_loaded = (char *)data;
  LOAD_SCM_FILE((char *)data);
  last_file_loaded = NULL;
  return(UNDEFINED_VALUE);
}

static SCM g_call0_1(void *arg)
{
#if (SCM_DEBUG_TYPING_STRICTNESS == 2)
  return(FALSE_VALUE);
#else
  return(scm_apply((SCM)arg, EMPTY_LIST, EMPTY_LIST));
#endif
}

SCM g_call0(SCM proc, const char *caller) /* replacement for gh_call0 -- protect ourselves from premature exit(!$#%@$) */
{
#if (SCM_DEBUG_TYPING_STRICTNESS == 2)
  return(FALSE_VALUE);
#else
  return(snd_catch_any(g_call0_1, (void *)proc, caller));
#endif
}

static SCM g_call1_1(void *arg)
{
  return(scm_apply(((SCM *)arg)[0], 
 		   ((SCM *)arg)[1], 
 		   APPLY_EOL));
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
		   EMPTY_LIST));
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
  return(scm_apply(((SCM *)arg)[0], 
 		   ((SCM *)arg)[1], 
 		   CONS(((SCM *)arg)[2], APPLY_EOL)));
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
  return(scm_apply(((SCM *)arg)[0], 
		   ((SCM *)arg)[1], 
		   CONS2(((SCM *)arg)[2], 
			 ((SCM *)arg)[3], 
			 APPLY_EOL)));
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
  char *str1 = NULL;
#if HAVE_GUILE
#if HAVE_SCM_OBJECT_TO_STRING
  return(TO_NEW_C_STRING(OBJECT_TO_STRING(obj))); 
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
#if HAVE_LIBREP
  SCM stream, val;
  stream = Fmake_string_output_stream();
  rep_print_val(stream, obj);
  val = Fget_output_stream_string(stream);
  str1 = TO_C_STRING(val);
#endif
#if HAVE_RUBY
  return(TO_NEW_C_STRING(OBJECT_TO_STRING(obj)));
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

SCM snd_eval_str(snd_state *ss, char *buf)
{
  return(snd_report_result(ss, snd_catch_any(eval_str_wrapper, (void *)buf, buf), buf));
}

static SCM print_hook;

SCM snd_report_result(snd_state *ss, SCM result, char *buf)
{
  snd_info *sp = NULL;
  char *str = NULL;
  SCM res = FALSE_VALUE;
  str = gl_print(result, "eval-str");
  if (ss->mx_sp)
    {
      sp = ss->mx_sp;
      clear_minibuffer_prompt(sp);
      report_in_minibuffer(sp, str);
    }
  if (ss->listening)
    {
      if (buf) listener_append(ss, buf);
      if (HOOKED(print_hook))
	res = g_c_run_or_hook(print_hook, 
			      LIST_1(TO_SCM_STRING(str)),
			      S_print_hook);
      if (FALSE_P(res))
	listener_append_and_prompt(ss, str);
    }
  if (str) free(str);
  return(result);
}

SCM snd_report_listener_result(snd_state *ss, SCM form)
{
  char *str = NULL;
  SCM res = FALSE_VALUE, result;
  listener_append(ss, "\n");
  result = snd_catch_any(eval_form_wrapper, (void *)form, NULL);
  str = gl_print(result, "eval");
  if (ss->listening)
    {
      if (HOOKED(print_hook))
	res = g_c_run_or_hook(print_hook, 
			      LIST_1(TO_SCM_STRING(str)),
			      S_print_hook);
      if (FALSE_P(res))
	listener_append_and_prompt(ss, str);
    }
  if (str) free(str);
  return(result);
}

void snd_eval_property_str(snd_state *ss, char *buf)
{
  SCM result;
  char *str;
  if ((snd_strlen(buf) == 0) || ((snd_strlen(buf) == 1) && (buf[0] == '\n'))) return;
  result = snd_catch_any(eval_str_wrapper, (void *)buf, buf);
  str = gl_print(result, "eval-listener-str");
  listener_append_and_prompt(ss, str);
  if (str) free(str);
}

static char *stdin_str = NULL;

void clear_listener(void)
{
  snd_state *ss;
  ss = get_global_state();
  if (stdin_str) FREE(stdin_str);
  stdin_str = NULL;
  listener_append_and_prompt(ss, "");
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
#if HAVE_LIBREP || HAVE_RUBY
      result = EVAL_STRING(buf);
#else
      result = snd_catch_any(eval_str_wrapper, (void *)str, str);
#endif
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
	  snd_catch_any(eval_file_wrapper, (void *)SND_CONF, "(load " SND_CONF ")");
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
	  snd_catch_any(eval_file_wrapper, (void *)str, "(load ~/.snd)");
	}
      if (str) FREE(str);
    }
}

void snd_load_file(char *filename)
{
  char *str = NULL, *str1 = NULL, *str2 = NULL;
  str = mus_expand_filename(filename);
  str2 = (char *)CALLOC(PRINT_BUFFER_SIZE, sizeof(char));
  mus_snprintf(str2, PRINT_BUFFER_SIZE, "(load \"%s\")", filename);
  if (!mus_file_probe(str))
    {
      /* try tacking on .scm */
      str1 = (char *)CALLOC(snd_strlen(str) + 5, sizeof(char));
      sprintf(str1, "%s.scm", str);
      if (!mus_file_probe(str1))
	snd_error("can't load %s: %s", filename, strerror(errno));
      /* snd_error ok here because all uses of this are user-interface generated (autoload, memo-file, etc) */
      else snd_catch_any(eval_file_wrapper, (void *)str1, str2);
      FREE(str1);
    }
  else snd_catch_any(eval_file_wrapper, (void *)str, str2);
  if (str) FREE(str);
  if (str2) FREE(str2);
}

static SCM g_snd_print(SCM msg)
{
  #define H_snd_print "(" S_snd_print " str) displays str in the lisp listener window"
  char *str = NULL;
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
  check_for_event(state);
  listener_append(state, str);
  if (str) free(str);
  return(msg);
}


/* -------- global variables -------- */

static SCM g_ask_before_overwrite(void) {return(TO_SCM_BOOLEAN(ask_before_overwrite(state)));}
static SCM g_set_ask_before_overwrite(SCM val) 
{
  #define H_ask_before_overwrite "(" S_ask_before_overwrite ") should be #t if you want Snd to ask before overwriting a file"
  ASSERT_TYPE(BOOLEAN_IF_BOUND_P(val), val, ARGn, "set-" S_ask_before_overwrite, "a boolean");
  set_ask_before_overwrite(state, TO_C_BOOLEAN_OR_T(val)); 
  return(TO_SCM_BOOLEAN(ask_before_overwrite(state)));
}

static SCM g_audio_output_device(void) {return(TO_SCM_INT(audio_output_device(state)));}
static SCM g_set_audio_output_device(SCM val) 
{
  #define H_audio_output_device "(" S_audio_output_device ") is the current sndlib default output device (mus-audio-default)"
  ASSERT_TYPE(INTEGER_P(val), val, ARGn, "set-" S_audio_output_device, "an integer"); 
  set_audio_output_device(state, TO_C_INT(val)); 
  return(TO_SCM_INT(audio_output_device(state)));
}

static SCM g_audio_input_device(void) {return(TO_SCM_INT(audio_input_device(state)));}
static SCM g_set_audio_input_device(SCM val) 
{
  #define H_audio_input_device "(" S_audio_input_device ") is the current sndlib default input device (mus-audio-default)"
  ASSERT_TYPE(INTEGER_P(val), val, ARGn, "set-" S_audio_input_device, "an integer"); 
  set_audio_input_device(state, TO_C_INT(val)); 
  return(TO_SCM_INT(audio_input_device(state)));
}

static SCM g_minibuffer_history_length(void) {return(TO_SCM_INT(minibuffer_history_length(state)));}
static SCM g_set_minibuffer_history_length(SCM val) 
{
  #define H_minibuffer_history_length "(" S_minibuffer_history_length ") is the minibuffer history length"
  int len;
  ASSERT_TYPE(INTEGER_P(val), val, ARGn, "set-" S_minibuffer_history_length, "an integer");
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
  ASSERT_TYPE(NUMBER_P(val), val, ARGn, "set-" S_dac_size, "a number");
  len = TO_C_INT_OR_ELSE(val, 0);
  if (len > 0)
    set_dac_size(state, len);
  return(TO_SCM_INT(dac_size(state)));
}

static SCM g_dac_combines_channels(void) {return(TO_SCM_BOOLEAN(dac_combines_channels(state)));}
static SCM g_set_dac_combines_channels(SCM val) 
{
  #define H_dac_combines_channels "(" S_dac_combines_channels ") should be #t if extra channels are to be mixed into available ones during playing (#t)"
  ASSERT_TYPE(BOOLEAN_IF_BOUND_P(val), val, ARGn, "set-" S_dac_combines_channels, "a boolean");
  set_dac_combines_channels(state, TO_C_BOOLEAN_OR_T(val)); 
  return(TO_SCM_BOOLEAN(dac_combines_channels(state)));
}

static SCM g_auto_resize(void) {return(TO_SCM_BOOLEAN(auto_resize(state)));}
static SCM g_set_auto_resize(SCM val) 
{
  #define H_auto_resize "(" S_auto_resize ") should be #t if Snd can change its main window size as it pleases (#t)"
  ASSERT_TYPE(BOOLEAN_IF_BOUND_P(val), val, ARGn, "set-" S_auto_resize, "a boolean");
  set_auto_resize(state, TO_C_BOOLEAN_OR_T(val)); 
  reflect_resize(state); 
  return(TO_SCM_BOOLEAN(auto_resize(state)));
}

static SCM g_auto_update(void) {return(TO_SCM_BOOLEAN(auto_update(state)));}
static SCM g_set_auto_update(SCM val) 
{
  #define H_auto_update "(" S_auto_update ") -> #t if Snd should automatically update a file if it changes unexpectedly (#f)"
  ASSERT_TYPE(BOOLEAN_IF_BOUND_P(val), val, ARGn, "set-" S_auto_update, "a boolean");
  set_auto_update(state, TO_C_BOOLEAN_OR_T(val)); 
  return(TO_SCM_BOOLEAN(auto_update(state)));
}

static SCM g_filter_env_in_hz(void) {return(TO_SCM_BOOLEAN(filter_env_in_hz(state)));}
static SCM g_set_filter_env_in_hz(SCM val) 
{
  #define H_filter_env_in_hz "(" S_filter_env_in_hz ") -> #t if filter env x axis should be in Hz"
  ASSERT_TYPE(BOOLEAN_IF_BOUND_P(val), val, ARGn, "set-" S_filter_env_in_hz, "a boolean");
  set_filter_env_in_hz(state, TO_C_BOOLEAN_OR_T(val)); 
  return(TO_SCM_BOOLEAN(filter_env_in_hz(state)));
}

static SCM g_color_cutoff(void) {return(TO_SCM_DOUBLE(color_cutoff(state)));}
static SCM g_set_color_cutoff(SCM val) 
{
  #define H_color_cutoff "(" S_color_cutoff ") -> col" STR_OR " map cutoff point (default .003)"
  ASSERT_TYPE(NUMBER_P(val), val, ARGn, "set-" S_color_cutoff, "a number");
  set_color_cutoff(state, mus_fclamp(0.0,
				     TO_C_DOUBLE(val),
				     0.25)); 
  return(TO_SCM_DOUBLE(color_cutoff(state)));
}

static SCM g_color_inverted(void) {return(TO_SCM_BOOLEAN(color_inverted(state)));}
static SCM g_set_color_inverted(SCM val) 
{
  #define H_color_inverted "(" S_color_inverted ") -> whether the col" STR_OR "map in operation should be inverted"
  ASSERT_TYPE(BOOLEAN_IF_BOUND_P(val), val, ARGn, "set-" S_color_inverted, "a boolean");
  set_color_inverted(state, TO_C_BOOLEAN_OR_T(val)); 
  return(TO_SCM_BOOLEAN(color_inverted(state)));
}

static SCM g_color_scale(void) {return(TO_SCM_DOUBLE(color_scale(state)));}
static SCM g_set_color_scale(SCM val) 
{
  #define H_color_scale "(" S_color_scale ") -> essentially a darkness setting for col" STR_OR "maps (0.5)"
  ASSERT_TYPE(NUMBER_P(val), val, ARGn, "set-" S_color_scale, "a number"); 
  set_color_scale(state, mus_fclamp(0.0,
				    TO_C_DOUBLE(val),
				    1.0)); 
  return(TO_SCM_DOUBLE(color_scale(state)));
}

static SCM g_auto_update_interval(void) {return(TO_SCM_DOUBLE(auto_update_interval(state)));}
static SCM g_set_auto_update_interval(SCM val) 
{
  Float ctime;
  #define H_auto_update_interval "(" S_auto_update_interval ") -> time (seconds) between background checks for changed file on disk (60)"
  ASSERT_TYPE(NUMBER_P(val), val, ARGn, "set-" S_auto_update_interval, "a number"); 
  ctime = TO_C_DOUBLE(val);
  if ((ctime < 0.0) || (ctime > (24 * 3600)))
    mus_misc_error("set-" S_auto_update_interval, "invalid time:", val);
  set_auto_update_interval(state, TO_C_DOUBLE(val)); 
  return(TO_SCM_DOUBLE(auto_update_interval(state)));
}

static SCM g_default_output_chans(void) {return(TO_SCM_INT(default_output_chans(state)));}
static SCM g_set_default_output_chans(SCM val) 
{
  #define H_default_output_chans "(" S_default_output_chans ") -> default number of channels when a new or temporary file is created (1)"
  ASSERT_TYPE(INTEGER_P(val), val, ARGn, "set-" S_default_output_chans, "an integer"); 
  set_default_output_chans(state, TO_C_INT(val));
  return(TO_SCM_INT(default_output_chans(state)));
}

static SCM g_default_output_srate(void) {return(TO_SCM_INT(default_output_srate(state)));}
static SCM g_set_default_output_srate(SCM val) 
{
  #define H_default_output_srate "(" S_default_output_srate ") -> default srate when a new or temporary file is created (22050)" 
  ASSERT_TYPE(NUMBER_P(val), val, ARGn, "set-" S_default_output_srate, "a number"); 
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

  ASSERT_TYPE(INTEGER_P(val), val, ARGn, "set-" S_default_output_type, "an integer"); 
  typ = TO_C_INT(val);
  if (mus_header_writable(typ, -2))
    set_default_output_type(state, typ); 
  else mus_misc_error("set-" S_default_output_type, 
		      "can't write this header type", 
		      LIST_2(val, 
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

  ASSERT_TYPE(INTEGER_P(val), val, ARGn, "set-" S_default_output_format, "an integer"); 
  format = TO_C_INT(val);
  set_default_output_format(state, format); 
  return(TO_SCM_INT(default_output_format(state)));
}

static SCM g_eps_file(void) {return(TO_SCM_STRING(eps_file(state)));}
static SCM g_set_eps_file(SCM val) 
{
  #define H_eps_file "(" S_eps_file ") -> current eps ('Print' command) file name (snd.eps)"
  ASSERT_TYPE(STRING_P(val), val, ARGn, "set-" S_eps_file, "a string"); 
  if (eps_file(state)) free(eps_file(state));
  set_eps_file(state, TO_NEW_C_STRING(val)); 
  return(TO_SCM_STRING(eps_file(state)));
}

static SCM g_eps_left_margin(void) {return(TO_SCM_DOUBLE(eps_left_margin(state)));}
static SCM g_set_eps_left_margin(SCM val) 
{
  #define H_eps_left_margin "(" S_eps_left_margin ") -> current eps ('Print' command) left margin"
  ASSERT_TYPE(NUMBER_P(val), val, ARGn, "set-" S_eps_left_margin, "a number"); 
  set_eps_left_margin(state, TO_C_DOUBLE(val));
  return(TO_SCM_DOUBLE(eps_left_margin(state)));
}

static SCM g_eps_bottom_margin(void) {return(TO_SCM_DOUBLE(eps_bottom_margin(state)));}
static SCM g_set_eps_bottom_margin(SCM val) 
{
  #define H_eps_bottom_margin "(" S_eps_bottom_margin ") -> current eps ('Print' command) bottom margin"
  ASSERT_TYPE(NUMBER_P(val), val, ARGn, "set-" S_eps_bottom_margin, "a number"); 
  set_eps_bottom_margin(state, TO_C_DOUBLE(val));
  return(TO_SCM_DOUBLE(eps_bottom_margin(state)));
}

static SCM g_listener_prompt(void) {return(TO_SCM_STRING(listener_prompt(state)));}
static SCM g_set_listener_prompt(SCM val) 
{
  #define H_listener_prompt "(" S_listener_prompt ") -> the current lisp listener prompt character ('>') "
  ASSERT_TYPE(STRING_P(val), val, ARGn, "set-" S_listener_prompt, "a string"); 
  if (listener_prompt(state)) free(listener_prompt(state));
  set_listener_prompt(state, TO_NEW_C_STRING(val));
#if USE_NO_GUI
  {
#if HAVE_GUILE
    char *str;
    str = (char *)CALLOC(PRINT_BUFFER_SIZE, sizeof(char));
    mus_snprintf(str, PRINT_BUFFER_SIZE, "(set! scm-repl-prompt \"%s\")", listener_prompt(state));
    EVAL_STRING(str);
    FREE(str);
#endif
  }
#endif
  return(TO_SCM_STRING(listener_prompt(state)));
}

static SCM g_audio_state_file(void) {return(TO_SCM_STRING(audio_state_file(state)));}
static SCM g_set_audio_state_file(SCM val) 
{
  #define H_audio_state_file "(" S_audio_state_file ") -> filename for the mus-audio-save-state function (.snd-mixer)"
  ASSERT_TYPE(STRING_P(val), val, ARGn, "set-" S_audio_state_file, "a string"); 
  if (audio_state_file(state)) free(audio_state_file(state));
  set_audio_state_file(state, TO_NEW_C_STRING(val));
  return(TO_SCM_STRING(audio_state_file(state)));
}

static SCM g_movies(void) {return(TO_SCM_BOOLEAN(movies(state)));}
static SCM g_set_movies(SCM val) 
{
  #define H_movies "(" S_movies ") -> #t if mix graphs are update continuously as the mix is dragged (#t)"
  ASSERT_TYPE(BOOLEAN_IF_BOUND_P(val), val, ARGn, "set-" S_movies, "a boolean");
  set_movies(state, TO_C_BOOLEAN_OR_T(val));
  return(TO_SCM_BOOLEAN(movies(state)));
}

static SCM g_selection_creates_region(void) {return(TO_SCM_BOOLEAN(selection_creates_region(state)));}
static SCM g_set_selection_creates_region(SCM val) 
{
  #define H_selection_creates_region "(" S_selection_creates_region ") -> #t if a region should be created each time a selection is made"
  ASSERT_TYPE(BOOLEAN_IF_BOUND_P(val), val, ARGn, "set-" S_selection_creates_region, "a boolean");
  set_selection_creates_region(state, TO_C_BOOLEAN_OR_T(val));
  return(TO_SCM_BOOLEAN(selection_creates_region(state)));
}

static SCM g_print_length(void) {return(TO_SCM_INT(print_length(state)));}
static SCM g_set_print_length(SCM val) 
{
  #define H_print_length "(" S_print_length ") -> number of vector elements to print in the listener (12)"
  ASSERT_TYPE(INTEGER_P(val), val, ARGn, "set-" S_print_length, "an integer"); 
  set_print_length(state, TO_C_INT(val)); 
  set_vct_print_length(TO_C_INT(val));
  return(TO_SCM_INT(print_length(state)));
}

static SCM g_previous_files_sort(void) {return(TO_SCM_INT(previous_files_sort(state)));}
static SCM g_set_previous_files_sort(SCM val) 
{
  #define H_previous_files_sort "(" S_previous_files_sort ") -> sort choice in view files (0 = unsorted, 1 = by name, etc)"
  ASSERT_TYPE(INTEGER_P(val), val, ARGn, "set-" S_previous_files_sort, "an integer"); 
  update_prevlist(state);
  set_previous_files_sort(state, mus_iclamp(0,
					    TO_C_INT(val),
					    5));
  update_prevfiles(state);
  return(TO_SCM_INT(previous_files_sort(state)));
}

static SCM g_show_indices(void) {return(TO_SCM_BOOLEAN(show_indices(state)));}
static SCM g_set_show_indices(SCM val) 
{
  #define H_show_indices "(" S_show_indices ") -> #t if sound name should be preceded by its index"
  ASSERT_TYPE(BOOLEAN_IF_BOUND_P(val), val, ARGn, "set-" S_show_indices, "a boolean");
  set_show_indices(state, TO_C_BOOLEAN_OR_T(val));
  return(TO_SCM_BOOLEAN(show_indices(state)));
}

static SCM g_show_backtrace(void) {return(TO_SCM_BOOLEAN(show_backtrace(state)));}
static SCM g_set_show_backtrace(SCM val) 
{
  #define H_show_backtrace "(" S_show_backtrace ") -> #t to show backtrace automatically upon error"
  ASSERT_TYPE(BOOLEAN_IF_BOUND_P(val), val, ARGn, "set-" S_show_backtrace, "a boolean");
  set_show_backtrace(state, TO_C_BOOLEAN_OR_T(val));
  return(TO_SCM_BOOLEAN(show_backtrace(state)));
}

static SCM g_show_usage_stats(void) {return(TO_SCM_BOOLEAN(show_usage_stats(state)));}
static SCM g_set_show_usage_stats(SCM on) 
{
  #define H_show_usage_stats "(" S_show_usage_stats ") -> #t if Snd should display memory usage stats"
  ASSERT_TYPE(BOOLEAN_IF_BOUND_P(on), on, ARGn, "set-" S_show_usage_stats, "a boolean");
  set_show_usage_stats(state, TO_C_BOOLEAN_OR_T(on));
  return(TO_SCM_BOOLEAN(show_usage_stats(state)));
}

static SCM g_update_usage_stats(void) 
{
  #define H_update_usage_stats "(" S_update_usage_stats ") causes the stats display to be made current"
  update_stats(state); 
  return(TRUE_VALUE);
}

static SCM g_sinc_width(void) {return(TO_SCM_INT(sinc_width(state)));}
static SCM g_set_sinc_width(SCM val) 
{
  #define H_sinc_width "(" S_sinc_width ") -> sampling rate conversion sinc width (10). \
The higher this number, the better the src low-pass filter, but the slower \
src runs.  If you use too low a setting, you can sometimes hear high \
frequency whistles leaking through."

  int len;
  ASSERT_TYPE(INTEGER_P(val), val, ARGn, "set-" S_sinc_width, "an integer"); 
  len = TO_C_INT(val);
  if (len >= 0)
    set_sinc_width(state, len);
  return(TO_SCM_INT(sinc_width(state)));
}

static SCM g_hankel_jn(void) {return(TO_SCM_DOUBLE(state->Hankel_Jn));}
static SCM g_set_hankel_jn(SCM val) 
{
  #define H_hankel_jn "(" S_hankel_jn ") -> Bessel function used in Hankel transform."
  ASSERT_TYPE(NUMBER_P(val), val, ARGn, "set-" S_hankel_jn, "a number"); 
  state->Hankel_Jn = TO_C_DOUBLE_WITH_ORIGIN(val, S_hankel_jn);
  return(val);
}

static SCM g_color_map(void) {return(TO_SCM_INT(color_map(state)));}
static SCM g_set_color_map(SCM val) 
{
  #define H_colormap "(" S_colormap ") -> current col" STR_OR "map choice. \
This should be an integer between -1 and 15.  The maps (from 0 to 15) are: \
gray, hsv, hot, cool, bone, copper, pink, jet, prism, autumn, winter, \
spring, summer, colorcube, flag, and lines.  -1 means black and white."

  ASSERT_TYPE(INTEGER_P(val), val, ARGn, "set-" S_colormap, "an integer"); 
  set_color_map(state, mus_iclamp(0,
				  TO_C_INT(val),
				  NUM_COLORMAPS-1));
  return(TO_SCM_INT(color_map(state)));
}

static SCM snd_access(char *dir, char *caller)
{
  int err;
  char *temp;
  SCM res;
  temp = shorter_tempnam(dir, "snd_");
  err = mus_file_create(temp);
  if (err == -1)
    {
      FREE(temp);
      temp = mus_format("%s %s is not writable: %s", caller, dir, strerror(errno));
      res = TO_SCM_STRING(temp);
      FREE(temp);
      return(res);
    }
  remove(temp);
  FREE(temp);
  return(TO_SCM_STRING(dir));
}

static SCM g_temp_dir(void) {return(TO_SCM_STRING(temp_dir(state)));}
static SCM g_set_temp_dir(SCM val) 
{
  #define H_temp_dir "(" S_temp_dir ") -> name of directory for temp files"

  ASSERT_TYPE(STRING_P(val) || FALSE_P(val), val, ARGn, "set-" S_temp_dir, "a string"); 
  if (temp_dir(state)) free(temp_dir(state));
  if (FALSE_P(val))
    set_temp_dir(state, snd_strdup(DEFAULT_TEMP_DIR));
  else 
    {
      set_temp_dir(state, TO_NEW_C_STRING(val));
      return(snd_access(temp_dir(state), S_temp_dir));
    }
  return(TO_SCM_STRING(temp_dir(state)));
}

static SCM g_snd_tempnam(void) 
{
  #define H_snd_tempnam "(" S_snd_tempnam ") -> new temp file name using temp-dir"
  char *tmp;
  SCM res;
  tmp = snd_tempnam(get_global_state());
  res = TO_SCM_STRING(tmp);
  FREE(tmp);
  return(res);
}

static SCM g_save_dir(void) {return(TO_SCM_STRING(save_dir(state)));}
static SCM g_set_save_dir(SCM val) 
{
  #define H_save_dir "(" S_save_dir ") -> name of directory for saved state data"
  ASSERT_TYPE(STRING_P(val), val, ARGn, "set-" S_save_dir, "a string"); 
  if (save_dir(state)) free(save_dir(state));
  set_save_dir(state, TO_NEW_C_STRING(val));
  return(snd_access(save_dir(state), S_save_dir));
}

static SCM g_trap_segfault(void) {return(TO_SCM_BOOLEAN(trap_segfault(state)));}
static SCM g_set_trap_segfault(SCM val) 
{
  #define H_trap_segfault "(" S_trap_segfault ") -> #t if Snd should try to trap (and whine about) segfaults"
  ASSERT_TYPE(BOOLEAN_IF_BOUND_P(val), val, ARGn, "set-" S_trap_segfault, "a boolean");
  set_trap_segfault(state, TO_C_BOOLEAN_OR_T(val));
  return(TO_SCM_BOOLEAN(trap_segfault(state)));
}

static SCM g_show_selection_transform(void) {return(TO_SCM_BOOLEAN(show_selection_transform(state)));}
static SCM g_set_show_selection_transform(SCM val) 
{
  #define H_show_selection_transform "(" S_show_selection_transform ") -> #t if transform display reflects selection, not time-domain window"
  ASSERT_TYPE(BOOLEAN_IF_BOUND_P(val), val, ARGn, "set-" S_show_selection_transform, "a boolean");
  set_show_selection_transform(state, TO_C_BOOLEAN_OR_T(val));
  return(TO_SCM_BOOLEAN(show_selection_transform(state)));
}

static SCM g_with_mix_tags(void) {return(TO_SCM_BOOLEAN(with_mix_tags(state)));}
static SCM g_set_with_mix_tags(SCM val) 
{
  #define H_with_mix_tags "(" S_with_mix_tags ") -> #t if Snd should editable mixes"
  ASSERT_TYPE(BOOLEAN_IF_BOUND_P(val), val, ARGn, "set-" S_with_mix_tags, "a boolean");
  set_with_mix_tags(state, TO_C_BOOLEAN_OR_T(val));
  return(TO_SCM_BOOLEAN(with_mix_tags(state)));
}

static SCM g_use_sinc_interp(void) {return(TO_SCM_BOOLEAN(use_sinc_interp(state)));}
static SCM g_set_use_sinc_interp(SCM val) 
{
  #define H_use_sinc_interp "(" S_use_sinc_interp ") -> #t if Snd should use convolution with sinc for sampling rate \
conversion.  The other choice is (much faster) linear interpolation which can introduce distortion"

  ASSERT_TYPE(BOOLEAN_IF_BOUND_P(val), val, ARGn, "set-" S_use_sinc_interp, "a boolean");
  set_use_sinc_interp(state, TO_C_BOOLEAN_OR_T(val));
  return(TO_SCM_BOOLEAN(use_sinc_interp(state)));
}

/* data-clipped -> clip-data? -- this is from sndlib */
static SCM g_data_clipped(void) {return(TO_SCM_BOOLEAN(data_clipped(state)));}
static SCM g_set_data_clipped(SCM val) 
{
  #define H_data_clipped "(" S_data_clipped ") -> #t if Snd should clip output values to the current \
output data format's maximum. The default (#f) allows them to wrap-around which makes a very loud click"

  ASSERT_TYPE(BOOLEAN_IF_BOUND_P(val), val, ARGn, "set-" S_data_clipped, "a boolean");
  set_data_clipped(state, TO_C_BOOLEAN_OR_T(val));
  return(TO_SCM_BOOLEAN(data_clipped(state)));
}

static SCM g_vu_font(void) {return(TO_SCM_STRING(vu_font(state)));}
static SCM g_set_vu_font(SCM val) 
{
  #define H_vu_font "(" S_vu_font ") -> name of font used to make VU meter labels (courier)"
  ASSERT_TYPE(STRING_P(val) || FALSE_P(val), val, ARGn, "set-" S_vu_font, "a string"); 
  if (vu_font(state)) free(vu_font(state));
  if (FALSE_P(val))
    set_vu_font(state, DEFAULT_VU_FONT);
  else set_vu_font(state, TO_NEW_C_STRING(val));
  return(TO_SCM_STRING(vu_font(state)));
}

static SCM g_vu_font_size(void) {return(TO_SCM_DOUBLE(vu_font_size(state)));}
static SCM g_set_vu_font_size(SCM val) 
{
  #define H_vu_font_size "(" S_vu_font_size ") -> size of VU font meter labels (1.0)"
  ASSERT_TYPE(NUMBER_P(val), val, ARGn, "set-" S_vu_font_size, "a number"); 
  set_vu_font_size(state, TO_C_DOUBLE(val));
  return(TO_SCM_DOUBLE(vu_font_size(state)));
}

static SCM g_vu_size(void) {return(TO_SCM_DOUBLE(vu_size(state)));}
static SCM g_set_vu_size(SCM val) 
{
  #define H_vu_size "(" S_vu_size ") -> size of VU meters (1.0)"
  ASSERT_TYPE(NUMBER_P(val), val, ARGn, "set-" S_vu_size, "a number"); 
  set_vu_size(state, TO_C_DOUBLE(val));
  return(TO_SCM_DOUBLE(vu_size(state)));
}

static SCM g_zoom_focus_style(void) {return(TO_SCM_INT(zoom_focus_style(state)));}
static SCM g_set_zoom_focus_style(SCM focus) 
{
  #define H_zoom_focus_style "(" S_zoom_focus_style ") -> one of '(" S_zoom_focus_left " " S_zoom_focus_right " " S_zoom_focus_middle " " S_zoom_focus_active ")\n\
  decides what zooming centers on (default: " S_zoom_focus_active ")"
  ASSERT_TYPE(INTEGER_P(focus), focus, ARGn, "set-" S_zoom_focus_style, "an integer"); 
  activate_focus_menu(state, mus_iclamp(ZOOM_FOCUS_LEFT,
					TO_C_INT(focus),
					ZOOM_FOCUS_MIDDLE));
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
  result = EMPTY_LIST;
  for (i = 0; i < ss->max_sounds; i++)
    {
      sp = ((snd_info *)(ss->sounds[i]));
      if ((sp) && (sp->inuse))
	result = CONS(TO_SMALL_SCM_INT(i),
		      result);
    }
  return(result);
}

static SCM g_equalize_panes(SCM snd) 
{
  #define H_equalize_panes "(" S_equalize_panes " (&optional snd) causes Snd to try to give all channels about the same screen space"
  snd_info *sp;
  if (NOT_BOUND_P(snd))
    equalize_all_panes(state); 
  else 
    {
      sp = get_sp(snd);
      if (sp)
	equalize_sound_panes(get_global_state(), 
			     sp,
			     sp->chans[0],
			     TRUE);
    }
  return(FALSE_VALUE);
}

static SCM g_show_listener(void) 
{
  #define H_show_listener "(" S_show_listener ") opens the lisp listener pane"
  handle_listener(state, TRUE); 
  return(TO_SCM_BOOLEAN(state->listening));
}

static SCM g_set_show_listener(SCM val)
{
  ASSERT_TYPE(BOOLEAN_P(val), val, ARGn, "set-" S_show_listener, "a boolean");
  handle_listener(state, TO_C_BOOLEAN(val));
  return(TO_SCM_BOOLEAN(state->listening));
}

static SCM g_help_text_font(void) {return(TO_SCM_STRING(help_text_font(state)));}
static SCM g_set_help_text_font(SCM val) 
{
  #define H_help_text_font "(" S_help_text_font ") -> font used in the Help dialog"
  ASSERT_TYPE(STRING_P(val), val, ARGn, "set-" S_help_text_font, "a string"); 
  set_help_text_font(state, TO_NEW_C_STRING(val)); 
  return(val);
}

static SCM g_tiny_font(void) {return(TO_SCM_STRING(tiny_font(state)));}
static SCM g_set_tiny_font(SCM val) 
{
  #define H_tiny_font "(" S_tiny_font ") -> font use for some info in the graphs"
  ASSERT_TYPE(STRING_P(val), val, ARGn, "set-" S_tiny_font, "a string"); 
  set_tiny_font(state, TO_NEW_C_STRING(val)); 
  return(val);
}

static SCM g_axis_label_font(void) {return(TO_SCM_STRING(axis_label_font(state)));}
static SCM g_set_axis_label_font(SCM val) 
{
  #define H_axis_label_font "(" S_axis_label_font ") -> font used for axis labels"
  ASSERT_TYPE(STRING_P(val), val, ARGn, "set-" S_axis_label_font, "a string"); 
  set_axis_label_font(state, TO_NEW_C_STRING(val)); 
  return(val);
}

static SCM g_axis_numbers_font(void) {return(TO_SCM_STRING(axis_numbers_font(state)));}
static SCM g_set_axis_numbers_font(SCM val) 
{
  #define H_axis_numbers_font "(" S_axis_numbers_font ") -> font used for axis numbers"
  ASSERT_TYPE(STRING_P(val), val, ARGn, "set-" S_axis_numbers_font, "a string"); 
  set_axis_numbers_font(state, TO_NEW_C_STRING(val)); 
  return(val);
}

static SCM g_listener_font(void) {return(TO_SCM_STRING(listener_font(state)));}
static SCM g_set_listener_font(SCM val) 
{
  #define H_listener_font "(" S_listener_font ") -> font used by the lisp listener"
  ASSERT_TYPE(STRING_P(val), val, ARGn, "set-" S_listener_font, "a string");
  set_listener_font(state, TO_NEW_C_STRING(val)); 
  return(val);
}

static SCM g_bold_button_font(void) {return(TO_SCM_STRING(bold_button_font(state)));}
static SCM g_set_bold_button_font(SCM val) 
{
  #define H_bold_button_font "(" S_bold_button_font ") -> font used by some buttons"
  ASSERT_TYPE(STRING_P(val), val, ARGn, "set-" S_bold_button_font, "a string"); 
  set_bold_button_font(state, TO_NEW_C_STRING(val)); 
  return(val);
}

static SCM g_button_font(void) {return(TO_SCM_STRING(button_font(state)));}
static SCM g_set_button_font(SCM val) 
{
  #define H_button_font "(" S_button_font ") -> font used by some buttons"
  ASSERT_TYPE(STRING_P(val), val, ARGn, "set-" S_button_font, "a string"); 
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
  Latus val;
  ASSERT_TYPE(NUMBER_P(height), height, ARGn, "set-" S_window_height, "a number"); 
  set_widget_height(MAIN_SHELL(state), val = (Latus)TO_C_INT_OR_ELSE(height, 0));
  state->init_window_height = val;
  return(height);
}

static SCM g_set_window_width(SCM width) 
{
  #define H_set_window_width "(" "set-" S_window_width " val) sets the Snd window width in pixels"
  Latus val;
  ASSERT_TYPE(NUMBER_P(width), width, ARGn, "set-" S_window_width, "a number"); 
  set_widget_width(MAIN_SHELL(state), val = (Latus)TO_C_INT_OR_ELSE(width, 0)); 
  state->init_window_width = val;
  return(width);
}

static SCM g_set_window_x(SCM val) 
{
  #define H_set_window_x "(" "set-" S_window_x " val) sets the Snd window x position in pixels"
  Locus x;
  ASSERT_TYPE(NUMBER_P(val), val, ARGn, "set-" S_window_x, "a number"); 
  set_widget_x(MAIN_SHELL(state), x = (Locus)TO_C_INT_OR_ELSE(val, 0));
  state->init_window_x = x;
  return(val);
}

static SCM g_set_window_y(SCM val) 
{
  #define H_set_window_y "(" "set-" S_window_y " val) sets the Snd window y position in pixels"
  Locus y;
  ASSERT_TYPE(NUMBER_P(val), val, ARGn, "set-" S_window_y, "a number"); 
  set_widget_y(MAIN_SHELL(state), y = (Locus)TO_C_INT_OR_ELSE(val, 0)); 
  state->init_window_y = y;
  return(val);
}

static SCM g_abort(void)
{
  #define H_abort "(" S_abort ") exits Snd via \"abort\", presumably to land in the debugger"
  abort();
  return(FALSE_VALUE);
}

static SCM g_dismiss_all_dialogs(void)
{
  #define H_dismiss_all_dialogs "(" S_dismiss_all_dialogs ") closes all active dialogs"
  dismiss_all_dialogs(state);
  return(FALSE_VALUE);
}

static SCM g_abortq(void)
{
  #define H_abortQ "(" S_c_g ") allows pending user interface events to occur, returning #t if C-g was typed"
  check_for_event(state);
  if (state->stopped_explicitly)
    {
      state->stopped_explicitly = 0;
      return(TRUE_VALUE);
    }
  return(FALSE_VALUE);
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
	    return(make_mix_readable_from_id(TO_C_INT_OR_ELSE(CAR(scm_snd_n), 0)));
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
  if ((sp == NULL) || (sp->active != 1))
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

static SCM g_open_sound_file(SCM g_name, SCM g_chans, SCM g_srate, SCM g_comment)
{
  #define H_open_sound_file "(" S_open_sound_file " &optional (name \"test.snd\")\n    (chans 1) (srate 22050) comment)\n\
creates a new sound file 'name' using either 'wave' or 'next' headers and float data, returns the file descriptor for \
subsequent " S_close_sound_file ". data can be written with " S_vct2sound_file

  /* assume user temp files are writing floats in native format */
  char *name = NULL, *comment = NULL;
  file_info *hdr;
  snd_state *ss;
  int chans = 1, srate = 22050, result;
#if MUS_LITTLE_ENDIAN
  int type = MUS_RIFF;
  int format = MUS_LFLOAT; /* see comment! */
#else
  int type = MUS_NEXT;
  int format = MUS_BFLOAT;
#endif
  ASSERT_TYPE(STRING_IF_BOUND_P(g_name), g_name, ARG1, S_open_sound_file, "a string");
  ASSERT_TYPE(INTEGER_IF_BOUND_P(g_chans), g_chans, ARG2, S_open_sound_file, "an integer");
  ASSERT_TYPE(NUMBER_IF_BOUND_P(g_srate), g_srate, ARG3, S_open_sound_file, "a number");
  ASSERT_TYPE(STRING_IF_BOUND_P(g_comment), g_comment, ARG4, S_open_sound_file, "a string");
  if (STRING_P(g_comment)) comment = TO_C_STRING(g_comment);
  chans = TO_C_INT_OR_ELSE(g_chans, 0); 
  srate = TO_C_INT_OR_ELSE(g_srate, 0); 
  if (STRING_P(g_name)) name = TO_C_STRING(g_name); 
  if (name == NULL)
#if MUS_LITTLE_ENDIAN
    name = "test.wav";
#else
    name = "test.snd";
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
    hdr->comment = copy_string(comment);
  ss = get_global_state();
  ss->catch_message = NULL;
  result = open_temp_file(name, chans, hdr, state);
  if (result == -1) 
    {
      free_file_info(hdr);
      if (ss->catch_message)
	ERROR(MUS_MISC_ERROR,
	      LIST_2(TO_SCM_STRING(S_open_sound_file),
		     TO_SCM_STRING(ss->catch_message)));
      else
	return(snd_no_such_file_error(S_open_sound_file, g_name));
    }
  set_temp_fd(result, hdr);
  return(TO_SCM_INT(result));
}

static SCM g_close_sound_file(SCM g_fd, SCM g_bytes)
{
  #define H_close_sound_file "(" S_close_sound_file " fd bytes) closes file pointed to by fd, updating its header to report 'bytes' bytes of data"
  file_info *hdr;
  int result, fd, bytes;
  ASSERT_TYPE(INTEGER_P(g_fd), g_fd, ARG1, S_close_sound_file, "an integer");
  ASSERT_TYPE(NUMBER_P(g_bytes), g_bytes, ARG2, S_close_sound_file, "a number");
  fd = TO_C_INT(g_fd);
  if ((fd < 0) || (fd == fileno(stdin)) || (fd == fileno(stdout)) || (fd == fileno(stderr)))
    mus_misc_error(S_close_sound_file, "invalid file", g_fd);
  bytes = TO_C_INT_OR_ELSE(g_bytes, 0);
  hdr = get_temp_header(fd);
  if (hdr == NULL) 
    {
      close(fd);
      return(snd_no_such_file_error(S_close_sound_file, g_fd));
    }
  result = close_temp_file(fd, hdr, bytes, any_selected_sound(state));
  unset_temp_fd(fd);
  free_file_info(hdr);
  return(TO_SCM_INT(result));
}

static SCM samples2vct(SCM samp_0, SCM samps, SCM snd_n, SCM chn_n, SCM v, SCM edpos)
{
  #define H_samples2vct "(" S_samples2vct " &optional (start-samp 0)\n    samps snd chn vct-obj edit-position)\n\
returns a vct object (vct-obj if given) containing snd channel chn's data starting at start-samp for samps, \
reading edit version edit-position (defaulting to the current version)"

  chan_info *cp;
  snd_fd *sf;
  Float *fvals;
  int i, len, beg, pos;
  vct *v1 = get_vct(v);
  ASSERT_TYPE(NUMBER_IF_BOUND_P(samp_0), samp_0, ARG1, S_samples2vct, "a number");
  ASSERT_TYPE(NUMBER_IF_BOUND_P(samps), samps, ARG2, S_samples2vct, "a number");
  ASSERT_CHANNEL(S_samples2vct, snd_n, chn_n, 3);
  cp = get_cp(snd_n, chn_n, S_samples2vct);
  pos = to_c_edit_position(cp, edpos, S_samples2vct, 6);
  beg = TO_C_INT_OR_ELSE(samp_0, 0);
  len = TO_C_INT_OR_ELSE(samps, cp->samples[pos] - beg);
  if (v1)
    {
      fvals = v1->data;
      if (len > v1->length)
	len = v1->length;
    }
  else fvals = (Float *)MALLOC(len * sizeof(Float));
  sf = init_sample_read_any(beg, cp, READ_FORWARD, pos);
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

static MUS_SAMPLE_TYPE local_next_sample_unscaled(snd_fd *sf)
{
  if (sf->view_buffered_data > sf->last)
    return(next_sound(sf));
  else return(*sf->view_buffered_data++);
}

static SCM samples2sound_data(SCM samp_0, SCM samps, SCM snd_n, SCM chn_n, SCM sdobj, SCM edpos, SCM sdchan)
{
  #define H_samples2sound_data "(" S_samples2sound_data " &optional (start-samp 0)\n    samps snd chn sdobj edit-position (sdobj-chan 0))\n\
returns a sound-data object (sdobj if given) containing snd channel chn's data starting at start-samp for samps, \
reading edit version edit-position (defaulting to the current version)"

  chan_info *cp;
  snd_fd *sf;
  sound_data *sd;
  SCM newsd = FALSE_VALUE;
  int i, len, beg, chn = 0, pos;
  ASSERT_TYPE(NUMBER_IF_BOUND_P(samp_0), samp_0, ARG1, S_samples2sound_data, "a number");
  ASSERT_TYPE(NUMBER_IF_BOUND_P(samps), samps, ARG2, S_samples2sound_data, "a number");
  ASSERT_CHANNEL(S_samples2sound_data, snd_n, chn_n, 3);
  ASSERT_TYPE(INTEGER_IF_BOUND_P(sdchan), sdchan, ARG7, S_samples2sound_data, "an integer");
  cp = get_cp(snd_n, chn_n, S_samples2sound_data);
  pos = to_c_edit_position(cp, edpos, S_samples2sound_data, 6);
  beg = TO_C_INT_OR_ELSE(samp_0, 0);
  len = TO_C_INT_OR_ELSE(samps, cp->samples[pos] - beg);
  if (len > 0)
    {
      chn = TO_C_INT_OR_ELSE(sdchan, 0);
      if (sound_data_p(sdobj))
	sd = (sound_data *)OBJECT_REF(sdobj);
      else
	{
	  newsd = make_sound_data(chn + 1, len);
	  sd = (sound_data *)OBJECT_REF(newsd);
	}
      if (chn < sd->chans)
	{
	  sf = init_sample_read_any(beg, cp, READ_FORWARD, pos);
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
  #define H_vct2sound_file "(" S_vct2sound_file " fd vct-obj samps) writes samps samples from vct-obj to the sound file controlled by fd"
  int fd, nums, i;
  float *vals;
  vct *v;
  ASSERT_TYPE(INTEGER_P(g_fd), g_fd, ARG1, S_vct2sound_file, "an integer");
  ASSERT_TYPE((VCT_P(obj)), obj, ARG2, S_vct2sound_file, "a vct");
  ASSERT_TYPE(NUMBER_P(g_nums), g_nums, ARG3, S_vct2sound_file, "a number");
  fd = TO_C_INT(g_fd);
  if ((fd < 0) || (fd == fileno(stdin)) || (fd == fileno(stdout)) || (fd == fileno(stderr)))
    mus_misc_error(S_vct2sound_file, "invalid file", g_fd);
  nums = TO_C_INT_OR_ELSE(g_nums, 0);
  v = TO_VCT(obj);
  lseek(fd, 0L, SEEK_END);
  if (sizeof(Float) == 4) /* Float can be either float or double */
    nums = write(fd, (char *)(v->data), nums * 4);
  else
    {
      /* v->data has doubles, but we're assuming elsewhere that these are floats in the file */
      vals = (float *)MALLOC(nums * sizeof(float));
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
  res = snd_catch_any(eval_str_wrapper, (void *)str, "string->float");
  if (NUMBER_P(res))
    return(TO_C_DOUBLE(res));
  else snd_error("%s is not a number", str);
  return(0.0);
}

int string2int(char *str) 
{
  SCM res;
  res = snd_catch_any(eval_str_wrapper, (void *)str, "string->int");
  if (NUMBER_P(res))
    return(TO_C_INT_OR_ELSE(res, 0));
  else snd_error("%s is not a number", str);
  return(0);
}

#if 0
char *string2string(char *str) 
{
  SCM res;
  res = snd_catch_any(eval_str_wrapper, (void *)str, "string->string");
  if (STRING_P(res))
    return(TO_NEW_C_STRING(res));
  else snd_error("%s is not a string", str);
  return(str);
}
#endif

static SCM g_update_time_graph(SCM snd, SCM chn) 
{
  #define H_update_time_graph "(" S_update_time_graph " &optional snd chn) redraws snd channel chn's graphs"
  chan_info *cp;
  ASSERT_CHANNEL(S_update_time_graph, snd, chn, 1); 
  cp = get_cp(snd, chn, S_update_time_graph);
  update_graph(cp, NULL);
  return(FALSE_VALUE);
}

static SCM g_update_transform(SCM snd, SCM chn) 
{
  #define H_update_transform "(" S_update_transform " &optional snd chn) recalculates snd channel chn's fft (and forces it to completion)"
  chan_info *cp;
  void *val;
  ASSERT_CHANNEL(S_update_transform, snd, chn, 1); 
  cp = get_cp(snd, chn, S_update_transform);
  if (cp->graph_transform_p)
    {
      if (chan_fft_in_progress(cp)) 
	force_fft_clear(cp, NULL);

      (cp->state)->checking_explicitly = 1;  /* do not allow UI events to intervene here! */
      if (cp->transform_graph_type == GRAPH_TRANSFORM_ONCE)
	{
	  val = (void *)make_fft_state(cp, 1);
	  while (safe_fft_in_slices(val) == BACKGROUND_CONTINUE);
	}
      else
	{
	  val = (void *)make_sonogram_state(cp);
	  while (sonogram_in_slices(val) == BACKGROUND_CONTINUE);
	}
      (cp->state)->checking_explicitly = 0;
    }
  return(FALSE_VALUE);
}

static SCM g_update_lisp_graph(SCM snd, SCM chn) 
{
  #define H_update_lisp_graph "(" S_update_lisp_graph " &optional snd chn) redraws snd channel chn's lisp graph"
  chan_info *cp;
  ASSERT_CHANNEL(S_update_lisp_graph, snd, chn, 1); 
  cp = get_cp(snd, chn, S_update_lisp_graph);
  display_channel_lisp_data(cp, cp->sound, cp->state);
  return(FALSE_VALUE);
}

static SCM g_help_dialog(SCM subject, SCM msg)
{
  #define H_help_dialog "(" S_help_dialog " subject message) fires up the Help window with subject and message"
  ASSERT_TYPE(STRING_P(subject), subject, ARG1, S_help_dialog, "a string");
  ASSERT_TYPE(STRING_P(msg), msg, ARG2, S_help_dialog, "a string");
  return(SND_WRAP(snd_help(state, TO_C_STRING(subject), TO_C_STRING(msg))));
}

static SCM g_mix_panel(void)
{
  #define H_mix_panel "(" S_mix_panel ") starts (and returns) the mix panel"
  return(SND_WRAP(make_mix_panel(get_global_state())));
}

static SCM g_color_dialog(void) 
{
  #define H_color_dialog "(" S_color_dialog ") fires up the Color dialog"
  return(SND_WRAP(start_color_dialog(state, 0, 0))); 
}

static SCM g_orientation_dialog(void) 
{
  #define H_orientation_dialog "(" S_orientation_dialog ") fires up the Orientation dialog"
  return(SND_WRAP(start_orientation_dialog(state, 0, 0))); 
}

static SCM g_transform_dialog(void) 
{
  #define H_transform_dialog "(" S_transform_dialog ") fires up the Transforms dialog"
  return(SND_WRAP(fire_up_transform_dialog(state))); 
}

static SCM g_file_dialog(void) 
{
  #define H_file_dialog "(" S_file_dialog ") fires up the File dialog"
  return(SND_WRAP(start_file_dialog(state, 0, 0)));
}

static SCM g_edit_header_dialog(SCM snd_n) 
{
  #define H_edit_header_dialog "(" S_edit_header_dialog " snd) opens the Edit Header dialog on sound snd"
  snd_info *sp; 
  sp = get_sp(snd_n);
  if (sp == NULL)
    return(snd_no_such_sound_error(S_edit_header_dialog, snd_n));
  return(SND_WRAP(edit_header(sp))); 
}

static SCM g_yes_or_no_p(SCM msg) 
{
  #define H_yes_or_no_p "(" S_yes_or_no_p " message) displays message and waits for 'y' or 'n'; returns #t if 'y'"
  ASSERT_TYPE(STRING_P(msg), msg, ARGn, S_yes_or_no_p, "a string");
  return(TO_SCM_BOOLEAN(snd_yes_or_no_p(state, TO_C_STRING(msg))));
}

static SCM g_graph(SCM ldata, SCM xlabel, SCM x0, SCM x1, SCM y0, SCM y1, SCM snd_n, SCM chn_n, SCM force_display)
{
  /* TODO: need a way to create the lisp-graph axis-info and draw there even if no call on graph
   */

  #define H_graph "(" S_graph " data &optional xlabel x0 x1 y0 y1 snd chn force-display)\n\
displays 'data' as a graph with x axis label 'xlabel', axis units going from x0 to x1 and y0 to y1; 'data' can be a list, vct, or vector. \
If 'data' is a list of numbers, it is treated as an envelope."

  chan_info *cp;
  lisp_grf *lg;
  SCM data = UNDEFINED_VALUE, lst;
  char *label = NULL;
  vct *v = NULL;
  SCM *vdata;
  int i, len, graph, graphs, need_update = 0;
  Float ymin, ymax, val, nominal_x0, nominal_x1;
  lisp_grf *old_lp = NULL;
  Latus h = 0, w = 0, ww = 0;
  Locus o = 0, gx0 = 0;
  axis_info *uap = NULL;
  /* ldata can be a vct object, a vector, or a list of either */
  ASSERT_TYPE(((VCT_P(ldata)) || (VECTOR_P(ldata)) || (LIST_P(ldata))), ldata, ARG1, S_graph, "a vct, vector, or list");
  ASSERT_CHANNEL(S_graph, snd_n, chn_n, 7);
  cp = get_cp(snd_n, chn_n, S_graph);
  ymin = 32768.0;
  ymax = -32768.0;
  if ((cp->sound_ctr == -1) || 
      (cp->sounds == NULL) || 
      (cp->sounds[cp->sound_ctr] == NULL) ||
      (cp->axis == NULL))
    return(FALSE_VALUE);
  if (STRING_P(xlabel)) label = TO_C_STRING(xlabel); 
  if (NUMBER_P(x0)) nominal_x0 = TO_C_DOUBLE(x0); else nominal_x0 = 0.0;
  if (NUMBER_P(x1)) nominal_x1 = TO_C_DOUBLE(x1); else nominal_x1 = 1.0;
  if (NUMBER_P(y0)) ymin = TO_C_DOUBLE(y0);
  if (NUMBER_P(y1)) ymax = TO_C_DOUBLE(y1);
  if ((!(LIST_P(ldata))) || 
      (NUMBER_P(CAR(ldata))))
    graphs = 1; 
  else graphs = LIST_LENGTH(ldata);
  if (graphs == 0) return(FALSE_VALUE);
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
      (NUMBER_P(CAR(ldata))))
    {
      lg = cp->lisp_info;
      lg->env_data = 1;
      if (lg->len[0] != len)
	{
	  if (lg->data[0]) FREE(lg->data[0]);
	  lg->data[0] = (Float *)CALLOC(len, sizeof(Float));
	  lg->len[0] = len;
	}
      for (i = 0, lst = ldata; i < len; i++, lst = CDR(lst))
	lg->data[0][i] = TO_C_DOUBLE(CAR(lst));
      if ((!NUMBER_P(y0)) || 
	  (!NUMBER_P(y1)))
	{
	  for (i = 1; i < len; i += 2)
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
	      v = (vct *)OBJECT_REF(data);
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
	    memcpy((void *)(lg->data[graph]), (void *)(v->data), len * sizeof(Float));
	  else 
	    {
	      vdata = VECTOR_ELEMENTS(data);
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
  cp->graph_lisp_p = 1;
  if ((EQ_P(force_display, UNDEFINED_VALUE)) || 
      (NOT_FALSE_P(force_display)))
    {
      if (need_update)
	update_graph(cp, NULL);
      else display_channel_lisp_data(cp, cp->sound, cp->state);
    }
  return(scm_return_first(FALSE_VALUE, data));
}


static SCM g_clear_audio_inputs (void) 
{
  #define H_clear_audio_inputs "(" S_clear_audio_inputs ") tries to reduce soundcard noise in Linux/OSS"
#if HAVE_OSS
  mus_audio_clear_soundcard_inputs(); 
#endif
  return(FALSE_VALUE);
}

static SCM g_set_oss_buffers(SCM num, SCM size)
{
  #define H_set_oss_buffers "(" "set-" S_oss_buffers " num size) sets Linux OSS 'fragment' number and size"
#if (HAVE_OSS || HAVE_ALSA)
  ASSERT_TYPE(INTEGER_P(num), num, ARG1, "set-" S_oss_buffers, "an integer");
  ASSERT_TYPE(INTEGER_P(size), size, ARG2, "set-" S_oss_buffers, "an integer");
  mus_audio_set_oss_buffers(TO_C_INT(num),
			    TO_C_INT(size));
#endif
  return(FALSE_VALUE);
}

#define S_mus_audio_describe            "mus-audio-describe"
static SCM g_mus_audio_describe(void) 
{
  #define H_mus_audio_describe "("  S_mus_audio_describe ") posts a description of the audio hardware state in the Help dialog"
  snd_help(state, "Audio State", mus_audio_report()); 
  return(TRUE_VALUE);
}


static SCM g_start_progress_report(SCM snd)
{
  #define H_start_progress_report "(" S_start_progress_report " &optional snd) posts the hour-glass icon"
  snd_info *sp;
  ASSERT_SOUND(S_start_progress_report, snd, 1);
  sp = get_sp(snd);
  if (sp == NULL)
    return(snd_no_such_sound_error(S_start_progress_report, snd));
  start_progress_report(sp, NOT_FROM_ENVED);
  return(snd);
}

static SCM g_finish_progress_report(SCM snd)
{
  #define H_finish_progress_report "(" S_finish_progress_report " &optional snd) removes the hour-glass icon"
  snd_info *sp;
  ASSERT_SOUND(S_finish_progress_report, snd, 1);
  sp = get_sp(snd);
  if (sp == NULL)
    return(snd_no_such_sound_error(S_finish_progress_report, snd));
  finish_progress_report(sp, NOT_FROM_ENVED);
  return(snd); 
}

static SCM g_progress_report(SCM pct, SCM name, SCM cur_chan, SCM chans, SCM snd)
{
  #define H_progress_report "(" S_progress_report " pct &optional name cur-chan chans snd)\n\
updates an on-going 'progress report' (e. g. an animated hour-glass icon) in snd using pct to indicate how far along we are"

  snd_info *sp;
  ASSERT_TYPE(NUMBER_P(pct), pct, ARG1, S_progress_report, "a number");
  ASSERT_SOUND(S_progress_report, snd, 5);
  ASSERT_TYPE(STRING_IF_BOUND_P(name), name, ARG2, S_progress_report, "a string");
  ASSERT_TYPE(INTEGER_IF_BOUND_P(cur_chan), cur_chan, ARG3, S_progress_report, "an integer");
  ASSERT_TYPE(INTEGER_IF_BOUND_P(chans), chans, ARG4, S_progress_report, "an integer");
  sp = get_sp(snd);
  if (sp == NULL)
    return(snd_no_such_sound_error(S_progress_report, snd));
  progress_report(sp,
		  (STRING_P(name)) ? TO_C_STRING(name) : "something useful",
		  TO_C_INT_OR_ELSE(cur_chan, 0),
		  TO_C_INT_OR_ELSE(chans, sp->nchans),
		  TO_C_DOUBLE(pct),
		  NOT_FROM_ENVED);
  return(snd);
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
  ASSERT_TYPE(STRING_P(val), val, ARGn, "set-" S_html_dir, "a string");
  set_html_dir(state, TO_NEW_C_STRING(val)); 
  return(val);
}
#endif


/* -------- shared color funcs -------- */

Float check_color_range(const char *caller, SCM val)
{
  Float rf;
  rf = TO_C_DOUBLE(val);
  if ((rf > 1.0) || (rf < 0.0))
    ERROR(TO_SCM_SYMBOL("out-of-range"), 
	  LIST_3(TO_SCM_STRING(caller),
		 TO_SCM_STRING("value must be between 0.0 and 1.0: ~S"),
		 LIST_1(val)));
  return(rf);
}

static SCM g_set_cursor_color (SCM color) 
{
  snd_color *v; 
  ASSERT_TYPE(COLOR_P(color), color, ARGn, "set-" S_cursor_color, "a color object"); 
  v = TO_SND_COLOR(color); 
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
  ASSERT_TYPE(COLOR_P(color), color, ARGn, "set-" S_highlight_color, "a color object"); 
  v = TO_SND_COLOR(color); 
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
  ASSERT_TYPE(COLOR_P(color), color, ARGn, "set-" S_mark_color, "a color object"); 
  v = TO_SND_COLOR(color); 
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
  ASSERT_TYPE(COLOR_P(color), color, ARGn, "set-" S_zoom_color, "a color object"); 
  v = TO_SND_COLOR(color); 
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
  ASSERT_TYPE(COLOR_P(color), color, ARGn, "set-" S_position_color, "a color object"); 
  v = TO_SND_COLOR(color); 
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
  ASSERT_TYPE(COLOR_P(color), color, ARGn, "set-" S_listener_color, "a color object"); 
  v = TO_SND_COLOR(color);
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
  ASSERT_TYPE(COLOR_P(color), color, ARGn, "set-" S_listener_text_color, "a color object"); 
  v = TO_SND_COLOR(color);
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
  ASSERT_TYPE(COLOR_P(color), color, ARGn, "set-" S_enved_waveform_color, "a color object"); 
  v = TO_SND_COLOR(color); 
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
  ASSERT_TYPE(COLOR_P(color), color, ARGn, "set-" S_filter_waveform_color, "a color object");
  v = TO_SND_COLOR(color);
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
  ASSERT_TYPE(COLOR_P(color), color, ARGn, "set-" S_selection_color, "a color object"); 
  v = TO_SND_COLOR(color); 
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
  SCM color, mix_id = UNDEFINED_VALUE;
  if (NOT_BOUND_P(arg2))
    color = arg1;
  else
    {
      color = arg2;
      mix_id = arg1;
    }
  ASSERT_TYPE(COLOR_P(color), color, ARGn, "set-" S_mix_color, "a color object"); 
  v = TO_SND_COLOR(color); 
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
  ASSERT_TYPE(COLOR_P(color), color, ARGn, "set-" S_selected_mix_color, "a color object"); 
  v = TO_SND_COLOR(color); 
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
  ASSERT_TYPE(COLOR_P(color), color, ARGn, "set-" S_text_focus_color, "a color object"); 
  v = TO_SND_COLOR(color); 
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
  ASSERT_TYPE(COLOR_P(color), color, ARGn, "set-" S_sash_color, "a color object"); 
  v = TO_SND_COLOR(color); 
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
  ASSERT_TYPE(COLOR_P(color), color, ARGn, "set-" S_data_color, "a color object"); 
  v = TO_SND_COLOR(color); 
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
  ASSERT_TYPE(COLOR_P(color), color, ARGn, "set-" S_selected_data_color, "a color object"); 
  v = TO_SND_COLOR(color); 
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
  ASSERT_TYPE(COLOR_P(color), color, ARGn, "set-" S_graph_color, "a color object");
  v = TO_SND_COLOR(color);
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
  ASSERT_TYPE(COLOR_P(color), color, ARGn, "set-" S_selected_graph_color, "a color object");
  v = TO_SND_COLOR(color); 
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
  ASSERT_TYPE(COLOR_P(color), color, ARGn, "set-" S_pushed_button_color, "a color object"); 
  v = TO_SND_COLOR(color); 
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
  ASSERT_TYPE(COLOR_P(color), color, ARGn, "set-" S_basic_color, "a color object"); 
  v = TO_SND_COLOR(color); 
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
  SCM result = FALSE_VALUE;
  SCM procs = HOOK_PROCEDURES (hook);
  while (NOT_NULL_P (procs))
    {
      if (!(EQ_P(args, LIST_0)))
	result = APPLY(CAR(procs), args, caller);
      else result = CALL_0(CAR(procs), caller);
      procs = CDR (procs);
    }
  return(scm_return_first(result, args));
}

SCM g_c_run_or_hook (SCM hook, SCM args, const char *caller)
{
  /* Guile built-in scm_c_run_hook doesn't return the value of the hook procedure(s) and calls everything on the list */
  SCM result = FALSE_VALUE; /* (or) -> #f */
  SCM procs = HOOK_PROCEDURES (hook);
  while (NOT_NULL_P (procs))
    {
      if (!(EQ_P(args, LIST_0)))
	result = APPLY(CAR(procs), args, caller);
      else result = CALL_0(CAR(procs), caller);
      if (NOT_FALSE_P(result)) return(result);
      procs = CDR (procs);
    }
  return(scm_return_first(result, args));
}

SCM g_c_run_and_hook (SCM hook, SCM args, const char *caller)
{
  SCM result = TRUE_VALUE; /* (and) -> #t */
  SCM procs = HOOK_PROCEDURES (hook);
  while (NOT_NULL_P (procs))
    {
      if (!(EQ_P(args, LIST_0)))
	result = APPLY(CAR(procs), args, caller);
      else result = CALL_0(CAR(procs), caller);
      if (FALSE_P(result)) return(result);
      procs = CDR (procs);
    }
  return(scm_return_first(result, args));
}

void during_open(int fd, char *file, int reason)
{
  if (HOOKED(during_open_hook))
    g_c_run_progn_hook(during_open_hook,
		       LIST_3(TO_SCM_INT(fd),
			      TO_SCM_STRING(file),
			      TO_SCM_INT(reason)),
		       S_during_open_hook);
}

void after_open(int index)
{
  if (HOOKED(after_open_hook))
    g_c_run_progn_hook(after_open_hook,
		       LIST_1(TO_SMALL_SCM_INT(index)),
		       S_after_open_hook);
}


char *output_comment(file_info *hdr)
{
  char *com = NULL;
  if (hdr) com = mus_sound_comment(hdr->name);
  if (HOOKED(output_comment_hook))
    {
      SCM result;
      SCM procs = HOOK_PROCEDURES (output_comment_hook);
      int comment_size = 0;
      char *new_comment = NULL, *tmpstr = NULL;
      while (NOT_NULL_P (procs))
	{
	  result = CALL_1(CAR(procs),
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
	  procs = CDR (procs);
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
#if HAVE_GUILE
#if HAVE_SCM_C_DEFINE
  SCM str;
  str = TO_SCM_STRING(get_help);
  scm_permanent_object(
    scm_c_define(get_name,
      scm_make_procedure_with_setter(
        NEW_PROCEDURE("", PROCEDURE get_func, get_req, get_opt, 0),
	NEW_PROCEDURE(set_name, PROCEDURE set_func, set_req, set_opt, 0))));
  scm_set_object_property_x(TO_SCM_SYMBOL(get_name), local_doc, str);
  scm_set_procedure_property_x(SND_LOOKUP(get_name), local_doc, str);
#else
  scm_set_object_property_x(
    CDR(
      gh_define(get_name,
	scm_make_procedure_with_setter(
          NEW_PROCEDURE("", PROCEDURE get_func, get_req, get_opt, 0),
	  NEW_PROCEDURE(set_name, PROCEDURE set_func, set_req, set_opt, 0)
	  ))),
    local_doc,
    TO_SCM_STRING(get_help));
  /* still need to trap help output and send it to the listener */
#endif
#endif
#if HAVE_LIBREP || HAVE_RUBY
  DEFINE_PROC(get_name, get_func, get_req, get_opt, 0, get_help);
  DEFINE_PROC(set_name, set_func, set_req, set_opt, 0, get_help);
#endif
}

void define_procedure_with_reversed_setter(char *get_name, SCM (*get_func)(), char *get_help,
					   char *set_name, SCM (*set_func)(), SCM (*reversed_set_func)(), 
					   SCM local_doc,
					   int get_req, int get_opt, int set_req, int set_opt)
{
#if HAVE_GUILE
#if HAVE_SCM_C_DEFINE
  SCM str;
  str = TO_SCM_STRING(get_help);
  scm_permanent_object(
    scm_c_define(get_name,
      scm_make_procedure_with_setter(
        NEW_PROCEDURE("", PROCEDURE get_func, get_req, get_opt, 0),
	NEW_PROCEDURE("", PROCEDURE reversed_set_func, set_req, set_opt, 0))));
  NEW_PROCEDURE(set_name, PROCEDURE set_func, set_req, set_opt, 0);
  scm_set_object_property_x(TO_SCM_SYMBOL(get_name), local_doc, str);
  scm_set_procedure_property_x(SND_LOOKUP(get_name), local_doc, str);
#else
  scm_set_object_property_x(
    CDR(
      gh_define(get_name,
	scm_make_procedure_with_setter(
          NEW_PROCEDURE("", PROCEDURE get_func, get_req, get_opt, 0),
	  NEW_PROCEDURE("", PROCEDURE reversed_set_func, set_req, set_opt, 0)
	  ))),
    local_doc,
    TO_SCM_STRING(get_help));
  /* still need to trap help output and send it to the listener */
  NEW_PROCEDURE(set_name, PROCEDURE set_func, set_req, set_opt, 0);
#endif
#endif
#if HAVE_LIBREP || HAVE_RUBY
  DEFINE_PROC(get_name, get_func, get_req, get_opt, 0, get_help);
  DEFINE_PROC(set_name, set_func, set_req, set_opt, 0, get_help);
#endif
}

#if HAVE_LADSPA
  void g_ladspa_to_snd(SCM local_doc);
#endif

#if HAVE_GUILE && HAVE_DLFCN_H
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
  return(TRUE_VALUE);
}

#endif

static SCM g_little_endian(void)
{
#if MUS_LITTLE_ENDIAN
  return(TRUE_VALUE);
#else
  return(FALSE_VALUE);
#endif
}

static SCM g_snd_completion(SCM text)
{
  /* perhaps callable from emacs? */
  return(TO_SCM_STRING(command_completer(TO_C_STRING(text))));
}

#if HAVE_GUILE
static SCM g_gc_off(void) {++scm_block_gc; return(TO_SCM_INT(scm_block_gc));}
static SCM g_gc_on(void) {--scm_block_gc; return(TO_SCM_INT(scm_block_gc));}
#endif

#if WITH_MCHECK
#include <mcheck.h>
static SCM g_mcheck_check_all(void)
{
  mcheck_check_all();
  return(FALSE_VALUE);
}
#endif


#ifdef ARGIFY_1
#if HAVE_GUILE && HAVE_DLFCN_H
NARGIFY_1(g_dlopen_w, g_dlopen)
NARGIFY_1(g_dlclose_w, g_dlclose)
NARGIFY_0(g_dlerror_w, g_dlerror)
NARGIFY_2(g_dlinit_w, g_dlinit)
#endif
NARGIFY_0(g_ask_before_overwrite_w, g_ask_before_overwrite)
ARGIFY_1(g_set_ask_before_overwrite_w, g_set_ask_before_overwrite)
NARGIFY_0(g_audio_output_device_w, g_audio_output_device)
ARGIFY_1(g_set_audio_output_device_w, g_set_audio_output_device)
NARGIFY_0(g_audio_input_device_w, g_audio_input_device)
ARGIFY_1(g_set_audio_input_device_w, g_set_audio_input_device)
NARGIFY_0(g_minibuffer_history_length_w, g_minibuffer_history_length)
ARGIFY_1(g_set_minibuffer_history_length_w, g_set_minibuffer_history_length)
NARGIFY_0(g_dac_size_w, g_dac_size)
ARGIFY_1(g_set_dac_size_w, g_set_dac_size)
NARGIFY_0(g_dac_combines_channels_w, g_dac_combines_channels)
ARGIFY_1(g_set_dac_combines_channels_w, g_set_dac_combines_channels)
NARGIFY_0(g_auto_resize_w, g_auto_resize)
ARGIFY_1(g_set_auto_resize_w, g_set_auto_resize)
NARGIFY_0(g_auto_update_w, g_auto_update)
ARGIFY_1(g_set_auto_update_w, g_set_auto_update)
NARGIFY_0(g_filter_env_in_hz_w, g_filter_env_in_hz)
ARGIFY_1(g_set_filter_env_in_hz_w, g_set_filter_env_in_hz)
NARGIFY_0(g_color_cutoff_w, g_color_cutoff)
ARGIFY_1(g_set_color_cutoff_w, g_set_color_cutoff)
NARGIFY_0(g_color_inverted_w, g_color_inverted)
ARGIFY_1(g_set_color_inverted_w, g_set_color_inverted)
NARGIFY_0(g_color_scale_w, g_color_scale)
ARGIFY_1(g_set_color_scale_w, g_set_color_scale)
NARGIFY_0(g_auto_update_interval_w, g_auto_update_interval)
ARGIFY_1(g_set_auto_update_interval_w, g_set_auto_update_interval)
NARGIFY_0(g_default_output_chans_w, g_default_output_chans)
ARGIFY_1(g_set_default_output_chans_w, g_set_default_output_chans)
NARGIFY_0(g_default_output_srate_w, g_default_output_srate)
ARGIFY_1(g_set_default_output_srate_w, g_set_default_output_srate)
NARGIFY_0(g_default_output_type_w, g_default_output_type)
ARGIFY_1(g_set_default_output_type_w, g_set_default_output_type)
NARGIFY_0(g_default_output_format_w, g_default_output_format)
ARGIFY_1(g_set_default_output_format_w, g_set_default_output_format)
NARGIFY_0(g_eps_file_w, g_eps_file)
NARGIFY_1(g_set_eps_file_w, g_set_eps_file)
NARGIFY_0(g_eps_left_margin_w, g_eps_left_margin)
ARGIFY_1(g_set_eps_left_margin_w, g_set_eps_left_margin)
NARGIFY_0(g_eps_bottom_margin_w, g_eps_bottom_margin)
ARGIFY_1(g_set_eps_bottom_margin_w, g_set_eps_bottom_margin)
NARGIFY_0(g_listener_prompt_w, g_listener_prompt)
ARGIFY_1(g_set_listener_prompt_w, g_set_listener_prompt)
NARGIFY_0(g_audio_state_file_w, g_audio_state_file)
ARGIFY_1(g_set_audio_state_file_w, g_set_audio_state_file)
NARGIFY_0(g_movies_w, g_movies)
ARGIFY_1(g_set_movies_w, g_set_movies)
NARGIFY_0(g_selection_creates_region_w, g_selection_creates_region)
ARGIFY_1(g_set_selection_creates_region_w, g_set_selection_creates_region)
NARGIFY_0(g_print_length_w, g_print_length)
ARGIFY_1(g_set_print_length_w, g_set_print_length)
NARGIFY_0(g_previous_files_sort_w, g_previous_files_sort)
ARGIFY_1(g_set_previous_files_sort_w, g_set_previous_files_sort)
NARGIFY_0(g_show_listener_w, g_show_listener)
ARGIFY_1(g_set_show_listener_w, g_set_show_listener)
NARGIFY_0(g_show_indices_w, g_show_indices)
ARGIFY_1(g_set_show_indices_w, g_set_show_indices)
NARGIFY_0(g_show_backtrace_w, g_show_backtrace)
ARGIFY_1(g_set_show_backtrace_w, g_set_show_backtrace)
NARGIFY_0(g_show_usage_stats_w, g_show_usage_stats)
ARGIFY_1(g_set_show_usage_stats_w, g_set_show_usage_stats)
NARGIFY_0(g_sinc_width_w, g_sinc_width)
ARGIFY_1(g_set_sinc_width_w, g_set_sinc_width)
NARGIFY_0(g_hankel_jn_w, g_hankel_jn)
ARGIFY_1(g_set_hankel_jn_w, g_set_hankel_jn)
NARGIFY_0(g_color_map_w, g_color_map)
ARGIFY_1(g_set_color_map_w, g_set_color_map)
NARGIFY_0(g_temp_dir_w, g_temp_dir)
ARGIFY_1(g_set_temp_dir_w, g_set_temp_dir)
NARGIFY_0(g_save_dir_w, g_save_dir)
ARGIFY_1(g_set_save_dir_w, g_set_save_dir)
NARGIFY_0(g_trap_segfault_w, g_trap_segfault)
ARGIFY_1(g_set_trap_segfault_w, g_set_trap_segfault)
NARGIFY_0(g_show_selection_transform_w, g_show_selection_transform)
ARGIFY_1(g_set_show_selection_transform_w, g_set_show_selection_transform)
NARGIFY_0(g_with_mix_tags_w, g_with_mix_tags)
ARGIFY_1(g_set_with_mix_tags_w, g_set_with_mix_tags)
NARGIFY_0(g_use_sinc_interp_w, g_use_sinc_interp)
ARGIFY_1(g_set_use_sinc_interp_w, g_set_use_sinc_interp)
NARGIFY_0(g_data_clipped_w, g_data_clipped)
ARGIFY_1(g_set_data_clipped_w, g_set_data_clipped)
NARGIFY_0(g_vu_font_w, g_vu_font)
ARGIFY_1(g_set_vu_font_w, g_set_vu_font)
NARGIFY_0(g_vu_font_size_w, g_vu_font_size)
ARGIFY_1(g_set_vu_font_size_w, g_set_vu_font_size)
NARGIFY_0(g_vu_size_w, g_vu_size)
ARGIFY_1(g_set_vu_size_w, g_set_vu_size)
NARGIFY_0(g_window_x_w, g_window_x)
ARGIFY_1(g_set_window_x_w, g_set_window_x)
NARGIFY_0(g_window_y_w, g_window_y)
ARGIFY_1(g_set_window_y_w, g_set_window_y)
NARGIFY_0(g_zoom_focus_style_w, g_zoom_focus_style)
ARGIFY_1(g_set_zoom_focus_style_w, g_set_zoom_focus_style)
NARGIFY_0(g_help_text_font_w, g_help_text_font)
ARGIFY_1(g_set_help_text_font_w, g_set_help_text_font)
NARGIFY_0(g_tiny_font_w, g_tiny_font)
ARGIFY_1(g_set_tiny_font_w, g_set_tiny_font)
NARGIFY_0(g_button_font_w, g_button_font)
ARGIFY_1(g_set_button_font_w, g_set_button_font)
NARGIFY_0(g_bold_button_font_w, g_bold_button_font)
ARGIFY_1(g_set_bold_button_font_w, g_set_bold_button_font)
NARGIFY_0(g_axis_label_font_w, g_axis_label_font)
ARGIFY_1(g_set_axis_label_font_w, g_set_axis_label_font)
NARGIFY_0(g_axis_numbers_font_w, g_axis_numbers_font)
ARGIFY_1(g_set_axis_numbers_font_w, g_set_axis_numbers_font)
NARGIFY_0(g_listener_font_w, g_listener_font)
ARGIFY_1(g_set_listener_font_w, g_set_listener_font)
NARGIFY_0(g_window_width_w, g_window_width)
ARGIFY_1(g_set_window_width_w, g_set_window_width)
NARGIFY_0(g_window_height_w, g_window_height)
ARGIFY_1(g_set_window_height_w, g_set_window_height)
#if (!USE_NO_GUI)
#if HAVE_HTML
NARGIFY_0(g_html_dir_w, g_html_dir)
NARGIFY_1(g_set_html_dir_w, g_set_html_dir)
#endif
NARGIFY_0(g_selection_color_w, g_selection_color)
NARGIFY_1(g_set_selection_color_w, g_set_selection_color)
NARGIFY_0(g_zoom_color_w, g_zoom_color)
NARGIFY_1(g_set_zoom_color_w, g_set_zoom_color)
NARGIFY_0(g_position_color_w, g_position_color)
NARGIFY_1(g_set_position_color_w, g_set_position_color)
NARGIFY_0(g_mark_color_w, g_mark_color)
NARGIFY_1(g_set_mark_color_w, g_set_mark_color)
NARGIFY_0(g_listener_color_w, g_listener_color)
NARGIFY_1(g_set_listener_color_w, g_set_listener_color)
NARGIFY_0(g_listener_text_color_w, g_listener_text_color)
NARGIFY_1(g_set_listener_text_color_w, g_set_listener_text_color)
NARGIFY_0(g_enved_waveform_color_w, g_enved_waveform_color)
NARGIFY_1(g_set_enved_waveform_color_w, g_set_enved_waveform_color)
NARGIFY_0(g_filter_waveform_color_w, g_filter_waveform_color)
NARGIFY_1(g_set_filter_waveform_color_w, g_set_filter_waveform_color)
NARGIFY_0(g_highlight_color_w, g_highlight_color)
NARGIFY_1(g_set_highlight_color_w, g_set_highlight_color)
NARGIFY_0(g_cursor_color_w, g_cursor_color)
NARGIFY_1(g_set_cursor_color_w, g_set_cursor_color)
ARGIFY_1(g_mix_color_w, g_mix_color)
ARGIFY_2(g_set_mix_color_w, g_set_mix_color)
NARGIFY_0(g_selected_mix_color_w, g_selected_mix_color)
ARGIFY_1(g_set_selected_mix_color_w, g_set_selected_mix_color)
NARGIFY_0(g_text_focus_color_w, g_text_focus_color)
NARGIFY_1(g_set_text_focus_color_w, g_set_text_focus_color)
NARGIFY_0(g_sash_color_w, g_sash_color)
NARGIFY_1(g_set_sash_color_w, g_set_sash_color)
NARGIFY_0(g_data_color_w, g_data_color)
NARGIFY_1(g_set_data_color_w, g_set_data_color)
NARGIFY_0(g_graph_color_w, g_graph_color)
NARGIFY_1(g_set_graph_color_w, g_set_graph_color)
NARGIFY_0(g_selected_graph_color_w, g_selected_graph_color)
NARGIFY_1(g_set_selected_graph_color_w, g_set_selected_graph_color)
NARGIFY_0(g_selected_data_color_w, g_selected_data_color)
NARGIFY_1(g_set_selected_data_color_w, g_set_selected_data_color)
NARGIFY_0(g_basic_color_w, g_basic_color)
NARGIFY_1(g_set_basic_color_w, g_set_basic_color)
NARGIFY_0(g_pushed_button_color_w, g_pushed_button_color)
NARGIFY_1(g_set_pushed_button_color_w, g_set_pushed_button_color)
#endif
NARGIFY_0(g_snd_tempnam_w, g_snd_tempnam)
NARGIFY_2(g_set_oss_buffers_w, g_set_oss_buffers)
NARGIFY_0(g_update_usage_stats_w, g_update_usage_stats)
NARGIFY_0(g_clear_audio_inputs_w, g_clear_audio_inputs)
NARGIFY_0(g_color_dialog_w, g_color_dialog)
NARGIFY_0(g_orientation_dialog_w, g_orientation_dialog)
NARGIFY_0(g_transform_dialog_w, g_transform_dialog)
NARGIFY_0(g_file_dialog_w, g_file_dialog)
ARGIFY_1(g_edit_header_dialog_w, g_edit_header_dialog)
NARGIFY_2(g_help_dialog_w, g_help_dialog)
NARGIFY_0(g_mix_panel_w, g_mix_panel)
NARGIFY_0(g_max_sounds_w, g_max_sounds)
NARGIFY_0(g_sounds_w, g_sounds)
NARGIFY_1(g_yes_or_no_p_w, g_yes_or_no_p)
ARGIFY_2(g_update_time_graph_w, g_update_time_graph)
ARGIFY_2(g_update_lisp_graph_w, g_update_lisp_graph)
ARGIFY_2(g_update_transform_w, g_update_transform)
NARGIFY_0(g_abort_w, g_abort)
NARGIFY_0(g_dismiss_all_dialogs_w, g_dismiss_all_dialogs)
NARGIFY_0(g_abortq_w, g_abortq)
NARGIFY_0(g_snd_version_w, g_snd_version)
ARGIFY_1(g_equalize_panes_w, g_equalize_panes)
ARGIFY_4(g_open_sound_file_w, g_open_sound_file)
NARGIFY_2(g_close_sound_file_w, g_close_sound_file)
NARGIFY_3(vct2soundfile_w, vct2soundfile)
ARGIFY_9(g_graph_w, g_graph)
ARGIFY_6(samples2vct_w, samples2vct)
ARGIFY_7(samples2sound_data_w, samples2sound_data)
ARGIFY_1(g_start_progress_report_w, g_start_progress_report)
ARGIFY_1(g_finish_progress_report_w, g_finish_progress_report)
ARGIFY_5(g_progress_report_w, g_progress_report)
NARGIFY_1(g_snd_print_w, g_snd_print)
NARGIFY_0(g_mus_audio_describe_w, g_mus_audio_describe)
NARGIFY_0(g_little_endian_w, g_little_endian)
NARGIFY_1(g_snd_completion_w, g_snd_completion)
#if HAVE_GUILE
NARGIFY_0(g_gc_off_w, g_gc_off)
NARGIFY_0(g_gc_on_w, g_gc_on)
#endif
#else
#if HAVE_GUILE && HAVE_DLFCN_H
#define g_dlopen_w g_dlopen
#define g_dlclose_w g_dlclose
#define g_dlerror_w g_dlerror
#define g_dlinit_w g_dlinit
#endif
#define g_ask_before_overwrite_w g_ask_before_overwrite
#define g_set_ask_before_overwrite_w g_set_ask_before_overwrite
#define g_audio_output_device_w g_audio_output_device
#define g_set_audio_output_device_w g_set_audio_output_device
#define g_audio_input_device_w g_audio_input_device
#define g_set_audio_input_device_w g_set_audio_input_device
#define g_minibuffer_history_length_w g_minibuffer_history_length
#define g_set_minibuffer_history_length_w g_set_minibuffer_history_length
#define g_dac_size_w g_dac_size
#define g_set_dac_size_w g_set_dac_size
#define g_dac_combines_channels_w g_dac_combines_channels
#define g_set_dac_combines_channels_w g_set_dac_combines_channels
#define g_auto_resize_w g_auto_resize
#define g_set_auto_resize_w g_set_auto_resize
#define g_auto_update_w g_auto_update
#define g_set_auto_update_w g_set_auto_update
#define g_filter_env_in_hz_w g_filter_env_in_hz
#define g_set_filter_env_in_hz_w g_set_filter_env_in_hz
#define g_color_cutoff_w g_color_cutoff
#define g_set_color_cutoff_w g_set_color_cutoff
#define g_color_inverted_w g_color_inverted
#define g_set_color_inverted_w g_set_color_inverted
#define g_color_scale_w g_color_scale
#define g_set_color_scale_w g_set_color_scale
#define g_auto_update_interval_w g_auto_update_interval
#define g_set_auto_update_interval_w g_set_auto_update_interval
#define g_default_output_chans_w g_default_output_chans
#define g_set_default_output_chans_w g_set_default_output_chans
#define g_default_output_srate_w g_default_output_srate
#define g_set_default_output_srate_w g_set_default_output_srate
#define g_default_output_type_w g_default_output_type
#define g_set_default_output_type_w g_set_default_output_type
#define g_default_output_format_w g_default_output_format
#define g_set_default_output_format_w g_set_default_output_format
#define g_eps_file_w g_eps_file
#define g_set_eps_file_w g_set_eps_file
#define g_eps_left_margin_w g_eps_left_margin
#define g_set_eps_left_margin_w g_set_eps_left_margin
#define g_eps_bottom_margin_w g_eps_bottom_margin
#define g_set_eps_bottom_margin_w g_set_eps_bottom_margin
#define g_listener_prompt_w g_listener_prompt
#define g_set_listener_prompt_w g_set_listener_prompt
#define g_audio_state_file_w g_audio_state_file
#define g_set_audio_state_file_w g_set_audio_state_file
#define g_movies_w g_movies
#define g_set_movies_w g_set_movies
#define g_selection_creates_region_w g_selection_creates_region
#define g_set_selection_creates_region_w g_set_selection_creates_region
#define g_print_length_w g_print_length
#define g_set_print_length_w g_set_print_length
#define g_previous_files_sort_w g_previous_files_sort
#define g_set_previous_files_sort_w g_set_previous_files_sort
#define g_show_listener_w g_show_listener
#define g_set_show_listener_w g_set_show_listener
#define g_show_indices_w g_show_indices
#define g_set_show_indices_w g_set_show_indices
#define g_show_backtrace_w g_show_backtrace
#define g_set_show_backtrace_w g_set_show_backtrace
#define g_show_usage_stats_w g_show_usage_stats
#define g_set_show_usage_stats_w g_set_show_usage_stats
#define g_sinc_width_w g_sinc_width
#define g_set_sinc_width_w g_set_sinc_width
#define g_hankel_jn_w g_hankel_jn
#define g_set_hankel_jn_w g_set_hankel_jn
#define g_color_map_w g_color_map
#define g_set_color_map_w g_set_color_map
#define g_temp_dir_w g_temp_dir
#define g_set_temp_dir_w g_set_temp_dir
#define g_save_dir_w g_save_dir
#define g_set_save_dir_w g_set_save_dir
#define g_trap_segfault_w g_trap_segfault
#define g_set_trap_segfault_w g_set_trap_segfault
#define g_show_selection_transform_w g_show_selection_transform
#define g_set_show_selection_transform_w g_set_show_selection_transform
#define g_with_mix_tags_w g_with_mix_tags
#define g_set_with_mix_tags_w g_set_with_mix_tags
#define g_use_sinc_interp_w g_use_sinc_interp
#define g_set_use_sinc_interp_w g_set_use_sinc_interp
#define g_data_clipped_w g_data_clipped
#define g_set_data_clipped_w g_set_data_clipped
#define g_vu_font_w g_vu_font
#define g_set_vu_font_w g_set_vu_font
#define g_vu_font_size_w g_vu_font_size
#define g_set_vu_font_size_w g_set_vu_font_size
#define g_vu_size_w g_vu_size
#define g_set_vu_size_w g_set_vu_size
#define g_window_x_w g_window_x
#define g_set_window_x_w g_set_window_x
#define g_window_y_w g_window_y
#define g_set_window_y_w g_set_window_y
#define g_zoom_focus_style_w g_zoom_focus_style
#define g_set_zoom_focus_style_w g_set_zoom_focus_style
#define g_help_text_font_w g_help_text_font
#define g_set_help_text_font_w g_set_help_text_font
#define g_tiny_font_w g_tiny_font
#define g_set_tiny_font_w g_set_tiny_font
#define g_button_font_w g_button_font
#define g_set_button_font_w g_set_button_font
#define g_bold_button_font_w g_bold_button_font
#define g_set_bold_button_font_w g_set_bold_button_font
#define g_axis_label_font_w g_axis_label_font
#define g_set_axis_label_font_w g_set_axis_label_font
#define g_axis_numbers_font_w g_axis_numbers_font
#define g_set_axis_numbers_font_w g_set_axis_numbers_font
#define g_listener_font_w g_listener_font
#define g_set_listener_font_w g_set_listener_font
#define g_window_width_w g_window_width
#define g_set_window_width_w g_set_window_width
#define g_window_height_w g_window_height
#define g_set_window_height_w g_set_window_height
#if (!USE_NO_GUI)
#if HAVE_HTML
#define g_html_dir_w g_html_dir
#define g_set_html_dir_w g_set_html_dir
#endif
#define g_selection_color_w g_selection_color
#define g_set_selection_color_w g_set_selection_color
#define g_zoom_color_w g_zoom_color
#define g_set_zoom_color_w g_set_zoom_color
#define g_position_color_w g_position_color
#define g_set_position_color_w g_set_position_color
#define g_mark_color_w g_mark_color
#define g_set_mark_color_w g_set_mark_color
#define g_listener_color_w g_listener_color
#define g_set_listener_color_w g_set_listener_color
#define g_listener_text_color_w g_listener_text_color
#define g_set_listener_text_color_w g_set_listener_text_color
#define g_enved_waveform_color_w g_enved_waveform_color
#define g_set_enved_waveform_color_w g_set_enved_waveform_color
#define g_filter_waveform_color_w g_filter_waveform_color
#define g_set_filter_waveform_color_w g_set_filter_waveform_color
#define g_highlight_color_w g_highlight_color
#define g_set_highlight_color_w g_set_highlight_color
#define g_cursor_color_w g_cursor_color
#define g_set_cursor_color_w g_set_cursor_color
#define g_mix_color_w g_mix_color
#define g_set_mix_color_w g_set_mix_color
#define g_selected_mix_color_w g_selected_mix_color
#define g_set_selected_mix_color_w g_set_selected_mix_color
#define g_text_focus_color_w g_text_focus_color
#define g_set_text_focus_color_w g_set_text_focus_color
#define g_sash_color_w g_sash_color
#define g_set_sash_color_w g_set_sash_color
#define g_data_color_w g_data_color
#define g_set_data_color_w g_set_data_color
#define g_graph_color_w g_graph_color
#define g_set_graph_color_w g_set_graph_color
#define g_selected_graph_color_w g_selected_graph_color
#define g_set_selected_graph_color_w g_set_selected_graph_color
#define g_selected_data_color_w g_selected_data_color
#define g_set_selected_data_color_w g_set_selected_data_color
#define g_basic_color_w g_basic_color
#define g_set_basic_color_w g_set_basic_color
#define g_pushed_button_color_w g_pushed_button_color
#define g_set_pushed_button_color_w g_set_pushed_button_color
#endif
#define g_snd_tempnam_w g_snd_tempnam
#define g_set_oss_buffers_w g_set_oss_buffers
#define g_update_usage_stats_w g_update_usage_stats
#define g_clear_audio_inputs_w g_clear_audio_inputs
#define g_color_dialog_w g_color_dialog
#define g_orientation_dialog_w g_orientation_dialog
#define g_transform_dialog_w g_transform_dialog
#define g_file_dialog_w g_file_dialog
#define g_edit_header_dialog_w g_edit_header_dialog
#define g_help_dialog_w g_help_dialog
#define g_mix_panel_w g_mix_panel
#define g_max_sounds_w g_max_sounds
#define g_sounds_w g_sounds
#define g_yes_or_no_p_w g_yes_or_no_p
#define g_update_time_graph_w g_update_time_graph
#define g_update_lisp_graph_w g_update_lisp_graph
#define g_update_transform_w g_update_transform
#define g_abort_w g_abort
#define g_dismiss_all_dialogs_w g_dismiss_all_dialogs
#define g_abortq_w g_abortq
#define g_snd_version_w g_snd_version
#define g_equalize_panes_w g_equalize_panes
#define g_open_sound_file_w g_open_sound_file
#define g_close_sound_file_w g_close_sound_file
#define vct2soundfile_w vct2soundfile
#define g_graph_w g_graph
#define samples2vct_w samples2vct
#define samples2sound_data_w samples2sound_data
#define g_start_progress_report_w g_start_progress_report
#define g_finish_progress_report_w g_finish_progress_report
#define g_progress_report_w g_progress_report
#define g_snd_print_w g_snd_print
#define g_mus_audio_describe_w g_mus_audio_describe
#define g_little_endian_w g_little_endian
#define g_snd_completion_w g_snd_completion
#if HAVE_GUILE
#define g_gc_off_w g_gc_off
#define g_gc_on_w g_gc_on
#endif
#endif

void g_initialize_gh(snd_state *ss)
{
  SCM local_doc;
  state = ss;
  local_doc = MAKE_PERMANENT(DOCUMENTATION);

#if WITH_MCHECK
  NEW_PROCEDURE("mcheck-all", PROCEDURE g_mcheck_check_all_w, 0, 0, 0);
#endif

#if TIMING
  g_init_timing(local_doc);
#endif
  mus_sndlib2scm_initialize();

  /* ---------------- CONSTANTS ---------------- */

  #define H_zoom_focus_left "The value for " S_zoom_focus_style " that causes zooming to maintain the left edge steady"
  #define H_zoom_focus_right "The value for " S_zoom_focus_style " that causes zooming to maintain the right edge steady"
  #define H_zoom_focus_middle "The value for " S_zoom_focus_style " that causes zooming to focus on the middle sample"
  #define H_zoom_focus_active "The value for " S_zoom_focus_style " that causes zooming to focus on the currently active object"

  DEFINE_CONST(S_zoom_focus_left,       ZOOM_FOCUS_LEFT,   H_zoom_focus_left);
  DEFINE_CONST(S_zoom_focus_right,      ZOOM_FOCUS_RIGHT,  H_zoom_focus_right);
  DEFINE_CONST(S_zoom_focus_active,     ZOOM_FOCUS_ACTIVE, H_zoom_focus_active);
  DEFINE_CONST(S_zoom_focus_middle,     ZOOM_FOCUS_MIDDLE, H_zoom_focus_middle);

  #define H_cursor_in_view "The value for an " S_bind_key " function that causes it to shift the window so that the cursor is in the view"
  #define H_cursor_on_left "The value for an " S_bind_key " function that causes it to shift the window so that the cursor is at the left edge"
  #define H_cursor_on_right "The value for an " S_bind_key " function that causes it to shift the window so that the cursor is at the right edge"
  #define H_cursor_in_middle "The value for an " S_bind_key " function that causes it to shift the window so that the cursor is in the middle"
  #define H_cursor_update_display "The value for an " S_bind_key " function that causes it to redraw the graph"
  #define H_cursor_no_action "The value for an " S_bind_key " function that causes it do nothing with the graph window"
  #define H_keyboard_no_action "The value for an " S_bind_key " function that causes it do nothing upon return"

  DEFINE_CONST(S_cursor_in_view,        CURSOR_IN_VIEW,        H_cursor_in_view);
  DEFINE_CONST(S_cursor_on_left,        CURSOR_ON_LEFT,        H_cursor_on_left);
  DEFINE_CONST(S_cursor_on_right,       CURSOR_ON_RIGHT,       H_cursor_on_right);
  DEFINE_CONST(S_cursor_in_middle,      CURSOR_IN_MIDDLE,      H_cursor_in_middle);
  DEFINE_CONST(S_cursor_update_display, CURSOR_UPDATE_DISPLAY, H_cursor_update_display);
  DEFINE_CONST(S_cursor_no_action,      CURSOR_NO_ACTION,      H_cursor_no_action);
  DEFINE_CONST(S_keyboard_no_action,    KEYBOARD_NO_ACTION,    H_keyboard_no_action);

  DEFINE_CONST(S_time_graph,           TIME_AXIS_INFO,      "time domain graph");
  DEFINE_CONST(S_transform_graph,      TRANSFORM_AXIS_INFO, "frequency domain graph");
  DEFINE_CONST(S_lisp_graph,           LISP_AXIS_INFO,      "lisp graph");


  /* ---------------- VARIABLES ---------------- */
  define_procedure_with_setter(S_ask_before_overwrite, PROCEDURE g_ask_before_overwrite_w, H_ask_before_overwrite,
			       "set-" S_ask_before_overwrite, PROCEDURE g_set_ask_before_overwrite_w, local_doc, 0, 0, 0, 1);

  define_procedure_with_setter(S_audio_output_device, PROCEDURE g_audio_output_device_w, H_audio_output_device,
			       "set-" S_audio_output_device, PROCEDURE g_set_audio_output_device_w, local_doc, 0, 0, 0, 1);

  define_procedure_with_setter(S_audio_input_device, PROCEDURE g_audio_input_device_w, H_audio_input_device,
			       "set-" S_audio_input_device, PROCEDURE g_set_audio_input_device_w, local_doc, 0, 0, 0, 1);

  define_procedure_with_setter(S_minibuffer_history_length, PROCEDURE g_minibuffer_history_length_w, H_minibuffer_history_length,
			       "set-" S_minibuffer_history_length, PROCEDURE g_set_minibuffer_history_length_w, local_doc, 0, 0, 0, 1);

  define_procedure_with_setter(S_dac_size, PROCEDURE g_dac_size_w, H_dac_size,
			       "set-" S_dac_size, PROCEDURE g_set_dac_size_w, local_doc, 0, 0, 0, 1);

  define_procedure_with_setter(S_dac_combines_channels, PROCEDURE g_dac_combines_channels_w, H_dac_combines_channels,
			       "set-" S_dac_combines_channels, PROCEDURE g_set_dac_combines_channels_w, local_doc, 0, 0, 0, 1);

  define_procedure_with_setter(S_auto_resize, PROCEDURE g_auto_resize_w, H_auto_resize,
			       "set-" S_auto_resize, PROCEDURE g_set_auto_resize_w, local_doc, 0, 0, 0, 1);

  define_procedure_with_setter(S_auto_update, PROCEDURE g_auto_update_w, H_auto_update,
			       "set-" S_auto_update, PROCEDURE g_set_auto_update_w, local_doc, 0, 0, 0, 1);

  define_procedure_with_setter(S_filter_env_in_hz, PROCEDURE g_filter_env_in_hz_w, H_filter_env_in_hz,
			       "set-" S_filter_env_in_hz, PROCEDURE g_set_filter_env_in_hz_w, local_doc, 0, 0, 0, 1);

  define_procedure_with_setter(S_color_cutoff, PROCEDURE g_color_cutoff_w, H_color_cutoff,
			       "set-" S_color_cutoff, PROCEDURE g_set_color_cutoff_w, local_doc, 0, 0, 0, 1);

  define_procedure_with_setter(S_color_inverted, PROCEDURE g_color_inverted_w, H_color_inverted,
			       "set-" S_color_inverted, PROCEDURE g_set_color_inverted_w, local_doc, 0, 0, 0, 1);

  define_procedure_with_setter(S_color_scale, PROCEDURE g_color_scale_w, H_color_scale,
			       "set-" S_color_scale, PROCEDURE g_set_color_scale_w, local_doc, 0, 0, 0, 1);

  define_procedure_with_setter(S_auto_update_interval, PROCEDURE g_auto_update_interval_w, H_auto_update_interval,
			       "set-" S_auto_update_interval, PROCEDURE g_set_auto_update_interval_w, local_doc, 0, 0, 0, 1);

  define_procedure_with_setter(S_default_output_chans, PROCEDURE g_default_output_chans_w, H_default_output_chans,
			       "set-" S_default_output_chans, PROCEDURE g_set_default_output_chans_w, local_doc, 0, 0, 0, 1);

  define_procedure_with_setter(S_default_output_srate, PROCEDURE g_default_output_srate_w, H_default_output_srate,
			       "set-" S_default_output_srate, PROCEDURE g_set_default_output_srate_w, local_doc, 0, 0, 0, 1);

  define_procedure_with_setter(S_default_output_type, PROCEDURE g_default_output_type_w, H_default_output_type,
			       "set-" S_default_output_type, PROCEDURE g_set_default_output_type_w, local_doc, 0, 0, 0, 1);

  define_procedure_with_setter(S_default_output_format, PROCEDURE g_default_output_format_w, H_default_output_format,
			       "set-" S_default_output_format, PROCEDURE g_set_default_output_format_w, local_doc, 0, 0, 0, 1);

  define_procedure_with_setter(S_eps_file, PROCEDURE g_eps_file_w, H_eps_file,
			       "set-" S_eps_file, PROCEDURE g_set_eps_file_w, local_doc, 0, 0, 1, 0);

  define_procedure_with_setter(S_eps_left_margin, PROCEDURE g_eps_left_margin_w, H_eps_left_margin,
			       "set-" S_eps_left_margin, PROCEDURE g_set_eps_left_margin_w, local_doc, 0, 0, 0, 1);

  define_procedure_with_setter(S_eps_bottom_margin, PROCEDURE g_eps_bottom_margin_w, H_eps_bottom_margin,
			       "set-" S_eps_bottom_margin, PROCEDURE g_set_eps_bottom_margin_w, local_doc, 0, 0, 0, 1);

  define_procedure_with_setter(S_listener_prompt, PROCEDURE g_listener_prompt_w, H_listener_prompt,
			       "set-" S_listener_prompt, PROCEDURE g_set_listener_prompt_w, local_doc, 0, 0, 0, 1);

  define_procedure_with_setter(S_audio_state_file, PROCEDURE g_audio_state_file_w, H_audio_state_file,
			       "set-" S_audio_state_file, PROCEDURE g_set_audio_state_file_w, local_doc, 0, 0, 0, 1);

  define_procedure_with_setter(S_movies, PROCEDURE g_movies_w, H_movies,
			       "set-" S_movies, PROCEDURE g_set_movies_w, local_doc, 0, 0, 0, 1);

  define_procedure_with_setter(S_selection_creates_region, PROCEDURE g_selection_creates_region_w, H_selection_creates_region,
			       "set-" S_selection_creates_region, PROCEDURE g_set_selection_creates_region_w, local_doc, 0, 0, 0, 1);

  define_procedure_with_setter(S_print_length, PROCEDURE g_print_length_w, H_print_length,
			       "set-" S_print_length, PROCEDURE g_set_print_length_w, local_doc, 0, 0, 0, 1);

  define_procedure_with_setter(S_previous_files_sort, PROCEDURE g_previous_files_sort_w, H_previous_files_sort,
			       "set-" S_previous_files_sort, PROCEDURE g_set_previous_files_sort_w, local_doc, 0, 0, 0, 1);

  define_procedure_with_setter(S_show_listener, PROCEDURE g_show_listener_w, H_show_listener,
			       "set-" S_show_listener, PROCEDURE g_set_show_listener_w, local_doc, 0, 0, 1, 0);

  define_procedure_with_setter(S_show_indices, PROCEDURE g_show_indices_w, H_show_indices,
			       "set-" S_show_indices, PROCEDURE g_set_show_indices_w, local_doc, 0, 0, 0, 1);

  define_procedure_with_setter(S_show_backtrace, PROCEDURE g_show_backtrace_w, H_show_backtrace,
			       "set-" S_show_backtrace, PROCEDURE g_set_show_backtrace_w, local_doc, 0, 0, 0, 1);

  define_procedure_with_setter(S_show_usage_stats, PROCEDURE g_show_usage_stats_w, H_show_usage_stats,
			       "set-" S_show_usage_stats, PROCEDURE g_set_show_usage_stats_w, local_doc, 0, 0, 0, 1);

  define_procedure_with_setter(S_sinc_width, PROCEDURE g_sinc_width_w, H_sinc_width,
			       "set-" S_sinc_width, PROCEDURE g_set_sinc_width_w, local_doc, 0, 0, 0, 1);

  define_procedure_with_setter(S_hankel_jn, PROCEDURE g_hankel_jn_w, H_hankel_jn,
			       "set-" S_hankel_jn, PROCEDURE g_set_hankel_jn_w, local_doc, 0, 0, 1, 0);

  define_procedure_with_setter(S_colormap, PROCEDURE g_color_map_w, H_colormap,
			       "set-" S_colormap, PROCEDURE g_set_color_map_w, local_doc, 0, 0, 0, 1);

  define_procedure_with_setter(S_temp_dir, PROCEDURE g_temp_dir_w, H_temp_dir,
			       "set-" S_temp_dir, PROCEDURE g_set_temp_dir_w, local_doc, 0, 0, 0, 1);

  define_procedure_with_setter(S_save_dir, PROCEDURE g_save_dir_w, H_save_dir,
			       "set-" S_save_dir, PROCEDURE g_set_save_dir_w, local_doc, 0, 0, 0, 1);

  define_procedure_with_setter(S_trap_segfault, PROCEDURE g_trap_segfault_w, H_trap_segfault,
			       "set-" S_trap_segfault, PROCEDURE g_set_trap_segfault_w, local_doc, 0, 0, 0, 1);

  define_procedure_with_setter(S_show_selection_transform, PROCEDURE g_show_selection_transform_w, H_show_selection_transform,
			       "set-" S_show_selection_transform, PROCEDURE g_set_show_selection_transform_w, local_doc, 0, 0, 0, 1);

  define_procedure_with_setter(S_with_mix_tags, PROCEDURE g_with_mix_tags_w, H_with_mix_tags,
			       "set-" S_with_mix_tags, PROCEDURE g_set_with_mix_tags_w, local_doc, 0, 0, 0, 1);

  define_procedure_with_setter(S_use_sinc_interp, PROCEDURE g_use_sinc_interp_w, H_use_sinc_interp,
			       "set-" S_use_sinc_interp, PROCEDURE g_set_use_sinc_interp_w, local_doc, 0, 0, 0, 1);

  define_procedure_with_setter(S_data_clipped, PROCEDURE g_data_clipped_w, H_data_clipped,
			       "set-" S_data_clipped, PROCEDURE g_set_data_clipped_w, local_doc, 0, 0, 0, 1);

  define_procedure_with_setter(S_vu_font, PROCEDURE g_vu_font_w, H_vu_font,
			       "set-" S_vu_font, PROCEDURE g_set_vu_font_w, local_doc, 0, 0, 0, 1);

  define_procedure_with_setter(S_vu_font_size, PROCEDURE g_vu_font_size_w, H_vu_font_size,
			       "set-" S_vu_font_size, PROCEDURE g_set_vu_font_size_w, local_doc, 0, 0, 0, 1);

  define_procedure_with_setter(S_vu_size, PROCEDURE g_vu_size_w, H_vu_size,
			       "set-" S_vu_size, PROCEDURE g_set_vu_size_w, local_doc, 0, 0, 0, 1);

  define_procedure_with_setter(S_window_x, PROCEDURE g_window_x_w, H_window_x,
			       "set-" S_window_x, PROCEDURE g_set_window_x_w, local_doc, 0, 0, 0, 1);

  define_procedure_with_setter(S_window_y, PROCEDURE g_window_y_w, H_window_y,
			       "set-" S_window_y, PROCEDURE g_set_window_y_w, local_doc, 0, 0, 0, 1);

  define_procedure_with_setter(S_zoom_focus_style, PROCEDURE g_zoom_focus_style_w, H_zoom_focus_style,
			       "set-" S_zoom_focus_style, PROCEDURE g_set_zoom_focus_style_w, local_doc, 0, 0, 0, 1);

  define_procedure_with_setter(S_help_text_font, PROCEDURE g_help_text_font_w, H_help_text_font,
			       "set-" S_help_text_font, PROCEDURE g_set_help_text_font_w, local_doc, 0, 0, 0, 1);

  define_procedure_with_setter(S_tiny_font, PROCEDURE g_tiny_font_w, H_tiny_font,
			       "set-" S_tiny_font, PROCEDURE g_set_tiny_font_w, local_doc, 0, 0, 0, 1);

  define_procedure_with_setter(S_button_font, PROCEDURE g_button_font_w, H_button_font,
			       "set-" S_button_font, PROCEDURE g_set_button_font_w, local_doc, 0, 0, 0, 1);

  define_procedure_with_setter(S_bold_button_font, PROCEDURE g_bold_button_font_w, H_bold_button_font,
			       "set-" S_bold_button_font, PROCEDURE g_set_bold_button_font_w, local_doc, 0, 0, 0, 1);

  define_procedure_with_setter(S_axis_label_font, PROCEDURE g_axis_label_font_w, H_axis_label_font,
			       "set-" S_axis_label_font, PROCEDURE g_set_axis_label_font_w, local_doc, 0, 0, 0, 1);

  define_procedure_with_setter(S_axis_numbers_font, PROCEDURE g_axis_numbers_font_w, H_axis_numbers_font,
			       "set-" S_axis_numbers_font, PROCEDURE g_set_axis_numbers_font_w, local_doc, 0, 0, 0, 1);

  define_procedure_with_setter(S_listener_font, PROCEDURE g_listener_font_w, H_listener_font,
			       "set-" S_listener_font, PROCEDURE g_set_listener_font_w, local_doc, 0, 0, 0, 1);

  define_procedure_with_setter(S_window_width, PROCEDURE g_window_width_w, H_window_width,
			       "set-" S_window_width, PROCEDURE g_set_window_width_w, local_doc, 0, 0, 0, 1);  

  define_procedure_with_setter(S_window_height, PROCEDURE g_window_height_w, H_window_height,
			       "set-" S_window_height, PROCEDURE g_set_window_height_w, local_doc, 0, 0, 0, 1);


#if (!USE_NO_GUI)
  #if HAVE_HTML
  YES_WE_HAVE("snd-html");
  define_procedure_with_setter(S_html_dir, PROCEDURE g_html_dir_w, H_html_dir,
			       "set-" S_html_dir, PROCEDURE g_set_html_dir_w, local_doc, 0, 0, 1, 0);
  #endif

  define_procedure_with_setter(S_selection_color, PROCEDURE g_selection_color_w, H_selection_color,
			       "set-" S_selection_color, PROCEDURE g_set_selection_color_w, local_doc, 0, 0, 1, 0);

  define_procedure_with_setter(S_zoom_color, PROCEDURE g_zoom_color_w, H_zoom_color,
			       "set-" S_zoom_color, PROCEDURE g_set_zoom_color_w, local_doc, 0, 0, 1, 0);

  define_procedure_with_setter(S_position_color, PROCEDURE g_position_color_w, H_position_color,
			       "set-" S_position_color, PROCEDURE g_set_position_color_w, local_doc, 0, 0, 1, 0);

  define_procedure_with_setter(S_mark_color, PROCEDURE g_mark_color_w, H_mark_color,
			       "set-" S_mark_color, PROCEDURE g_set_mark_color_w, local_doc, 0, 0, 1, 0);

  define_procedure_with_setter(S_listener_color, PROCEDURE g_listener_color_w, H_listener_color,
			       "set-" S_listener_color, PROCEDURE g_set_listener_color_w, local_doc, 0, 0, 1, 0);

  define_procedure_with_setter(S_listener_text_color, PROCEDURE g_listener_text_color_w, H_listener_text_color,
			       "set-" S_listener_text_color, PROCEDURE g_set_listener_text_color_w, local_doc, 0, 0, 1, 0);

  define_procedure_with_setter(S_enved_waveform_color, PROCEDURE g_enved_waveform_color_w, H_enved_waveform_color,
			       "set-" S_enved_waveform_color, PROCEDURE g_set_enved_waveform_color_w, local_doc, 0, 0, 1, 0);

  define_procedure_with_setter(S_filter_waveform_color, PROCEDURE g_filter_waveform_color_w, H_filter_waveform_color,
			       "set-" S_filter_waveform_color, PROCEDURE g_set_filter_waveform_color_w, local_doc, 0, 0, 1, 0);

  define_procedure_with_setter(S_highlight_color, PROCEDURE g_highlight_color_w, H_highlight_color,
			       "set-" S_highlight_color, PROCEDURE g_set_highlight_color_w, local_doc, 0, 0, 1, 0);

  define_procedure_with_setter(S_cursor_color, PROCEDURE g_cursor_color_w, H_cursor_color,
			       "set-" S_cursor_color, PROCEDURE g_set_cursor_color_w, local_doc, 0, 0, 1, 0);

  define_procedure_with_setter(S_mix_color, PROCEDURE g_mix_color_w, H_mix_color,
			       "set-" S_mix_color, PROCEDURE g_set_mix_color_w, local_doc, 0, 1, 1, 1);

  define_procedure_with_setter(S_selected_mix_color, PROCEDURE g_selected_mix_color_w, H_selected_mix_color,
			       "set-" S_selected_mix_color, PROCEDURE g_set_selected_mix_color_w, local_doc, 0, 0, 1, 0);

  define_procedure_with_setter(S_text_focus_color, PROCEDURE g_text_focus_color_w, H_text_focus_color,
			       "set-" S_text_focus_color, PROCEDURE g_set_text_focus_color_w, local_doc, 0, 0, 1, 0);

  define_procedure_with_setter(S_sash_color, PROCEDURE g_sash_color_w, H_sash_color,
			       "set-" S_sash_color, PROCEDURE g_set_sash_color_w, local_doc, 0, 0, 1, 0);

  define_procedure_with_setter(S_data_color, PROCEDURE g_data_color_w, H_data_color,
			       "set-" S_data_color, PROCEDURE g_set_data_color_w, local_doc, 0, 0, 1, 0);

  define_procedure_with_setter(S_graph_color, PROCEDURE g_graph_color_w, H_graph_color,
			       "set-" S_graph_color, PROCEDURE g_set_graph_color_w, local_doc, 0, 0, 1, 0);

  define_procedure_with_setter(S_selected_graph_color, PROCEDURE g_selected_graph_color_w, H_selected_graph_color,
			       "set-" S_selected_graph_color, PROCEDURE g_set_selected_graph_color_w, local_doc, 0, 0, 1, 0);

  define_procedure_with_setter(S_selected_data_color, PROCEDURE g_selected_data_color_w, H_selected_data_color,
			       "set-" S_selected_data_color, PROCEDURE g_set_selected_data_color_w, local_doc, 0, 0, 1, 0);

  define_procedure_with_setter(S_basic_color, PROCEDURE g_basic_color_w, H_basic_color,
			       "set-" S_basic_color, PROCEDURE g_set_basic_color_w, local_doc, 0, 0, 1, 0);

  define_procedure_with_setter(S_pushed_button_color, PROCEDURE g_pushed_button_color_w, H_pushed_button_color,
			       "set-" S_pushed_button_color, PROCEDURE g_set_pushed_button_color_w, local_doc, 0, 0, 1, 0);
#endif


  /* ---------------- FUNCTIONS ---------------- */

  DEFINE_PROC(S_snd_tempnam,         g_snd_tempnam_w, 0, 0, 0,         H_snd_tempnam);
  DEFINE_PROC("set-" S_oss_buffers,  g_set_oss_buffers_w, 2, 0, 0,     H_set_oss_buffers);
  DEFINE_PROC(S_update_usage_stats,  g_update_usage_stats_w, 0, 0, 0,  H_update_usage_stats);
  DEFINE_PROC(S_clear_audio_inputs,  g_clear_audio_inputs_w, 0, 0, 0,  H_clear_audio_inputs);
  DEFINE_PROC(S_color_dialog,        g_color_dialog_w, 0, 0, 0,        H_color_dialog);
  DEFINE_PROC(S_orientation_dialog,  g_orientation_dialog_w, 0, 0, 0,  H_orientation_dialog);
  DEFINE_PROC(S_transform_dialog,    g_transform_dialog_w, 0, 0, 0,    H_transform_dialog);
  DEFINE_PROC(S_file_dialog,         g_file_dialog_w, 0, 0, 0,         H_file_dialog);
  DEFINE_PROC(S_edit_header_dialog,  g_edit_header_dialog_w, 0, 1, 0,  H_edit_header_dialog);
  DEFINE_PROC(S_help_dialog,         g_help_dialog_w, 2, 0, 0,         H_help_dialog);
  DEFINE_PROC(S_mix_panel,           g_mix_panel_w, 0, 0, 0,           H_mix_panel);

  DEFINE_PROC(S_max_sounds,          g_max_sounds_w, 0, 0, 0,          H_max_sounds);
  DEFINE_PROC(S_sounds,              g_sounds_w, 0, 0, 0,              H_sounds);
  DEFINE_PROC(S_yes_or_no_p,         g_yes_or_no_p_w, 1, 0, 0,         H_yes_or_no_p);
  DEFINE_PROC(S_update_time_graph,   g_update_time_graph_w, 0, 2, 0,   H_update_time_graph);
  DEFINE_PROC(S_update_lisp_graph,   g_update_lisp_graph_w, 0, 2, 0,   H_update_lisp_graph);
  DEFINE_PROC(S_update_transform,    g_update_transform_w, 0, 2, 0,    H_update_transform);
  DEFINE_PROC(S_abort,               g_abort_w, 0, 0, 0,               H_abort);
  DEFINE_PROC(S_dismiss_all_dialogs, g_dismiss_all_dialogs_w, 0, 0, 0, H_dismiss_all_dialogs);
  DEFINE_PROC(S_c_g,                 g_abortq_w, 0, 0, 0,              H_abortQ);
  DEFINE_PROC(S_snd_version,         g_snd_version_w, 0, 0, 0,         H_snd_version);
  DEFINE_PROC(S_equalize_panes,      g_equalize_panes_w, 0, 1, 0,      H_equalize_panes);
  DEFINE_PROC(S_open_sound_file,     g_open_sound_file_w, 0, 4, 0,     H_open_sound_file);
  DEFINE_PROC(S_close_sound_file,    g_close_sound_file_w, 2, 0, 0,    H_close_sound_file);
  DEFINE_PROC(S_vct2sound_file,      vct2soundfile, 3, 0, 0,         H_vct2sound_file);
  DEFINE_PROC(S_graph,               g_graph_w, 1, 8, 0,               H_graph);
  DEFINE_PROC(S_samples2vct,         samples2vct, 0, 6, 0,           H_samples2vct);
  DEFINE_PROC(S_samples2sound_data,  samples2sound_data, 0, 7, 0,    H_samples2sound_data);
  DEFINE_PROC(S_start_progress_report, g_start_progress_report_w, 0, 1, 0, H_start_progress_report);
  DEFINE_PROC(S_finish_progress_report, g_finish_progress_report_w, 0, 1, 0, H_finish_progress_report);
  DEFINE_PROC(S_progress_report,     g_progress_report_w, 1, 4, 0,     H_progress_report);
  DEFINE_PROC(S_snd_print,           g_snd_print_w, 1, 0, 0,           H_snd_print);

  DEFINE_PROC("describe-audio",      g_mus_audio_describe_w, 0, 0, 0,  H_mus_audio_describe);
  /* this (describe-audio) is going away someday */

  DEFINE_PROC(S_mus_audio_describe,  g_mus_audio_describe_w, 0, 0, 0,  H_mus_audio_describe);
  DEFINE_PROC("little-endian?",      g_little_endian_w, 0, 0, 0,       "return #t if host is little endian");
  DEFINE_PROC("snd-completion",      g_snd_completion_w, 1, 0, 0,      "return completion of arg");
#if HAVE_GUILE
  DEFINE_PROC("gc-off",              g_gc_off_w, 0, 0, 0,              "turn off GC");
  DEFINE_PROC("gc-on",               g_gc_on_w, 0, 0, 0,               "turn on GC");
#endif

  #define H_during_open_hook S_during_open_hook " (fd name reason) is called after file is opened, but before data has been read. \n\
(add-hook! during-open-hook \n\
  (lambda (fd name reason) \n\
    (if (= (mus-sound-header-type name) mus-raw) \n\
        (mus-file-set-prescaler fd 500.0))))"

  #define H_after_open_hook S_after_open_hook " (snd) is called just before the new file's window is displayed. \
This provides a way to set various sound-specific defaults. \n\
(add-hook! after-open-hook \n\
  (lambda (snd) \n\
    (if (> (channels snd) 1) \n\
        (set! (channel-style snd) channels-combined))))"

  #define H_output_comment_hook S_output_comment_hook " (str) is called in Save-As dialog, passed current sound's comment, if any. \
If more than one hook function, results are concatenated. If none, the current comment is used.\n\
  (add-hook! output-comment-hook\n\
    (lambda (str)\n\
      (string-append \"written \"\n\
        (strftime \"%a %d-%b-%Y %H:%M %Z\"\n\
          (localtime (current-time))))))"

  #define H_print_hook S_print_hook " (text) is called each time some Snd-generated response (text) is about to be appended to the listener. \
If it returns some non-#f result, Snd assumes you've sent the text out yourself, as well as any needed prompt. \n\
(add-hook! print-hook \n\
  (lambda (msg) \n\
    (snd-print \n\
      (format #f \"~A~%[~A]~%~A\" \n\
              msg \n\
              (strftime \"%d-%b %H:%M %Z\" \n\
                         (localtime (current-time))) \n\
              (listener-prompt)))))"

  during_open_hook =    MAKE_HOOK(S_during_open_hook, 3,    H_during_open_hook);    /* args = fd filename reason */
  after_open_hook =     MAKE_HOOK(S_after_open_hook, 1,     H_after_open_hook);     /* args = sound */
  output_comment_hook = MAKE_HOOK(S_output_comment_hook, 1, H_output_comment_hook); /* arg = current mus_sound_comment(hdr) if any */
  print_hook =          MAKE_HOOK(S_print_hook, 1,          H_print_hook);          /* arg = text */

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
  g_init_gxregion(local_doc);
#if DEBUGGING
  g_init_gxfind(local_doc);
#endif
#endif
#if HAVE_GUILE && HAVE_DLFCN_H
  DEFINE_PROC("dlopen", PROCEDURE g_dlopen_w, 1, 0 ,0, "");
  DEFINE_PROC("dlclose", PROCEDURE g_dlclose_w, 1, 0 ,0, "");
  DEFINE_PROC("dlerror", PROCEDURE g_dlerror_w, 0, 0 ,0, "");
  DEFINE_PROC("dlinit", PROCEDURE g_dlinit_w, 2, 0 ,0, "");
#endif
#if HAVE_LADSPA
  g_ladspa_to_snd(local_doc);
#endif

#if HAVE_GUILE
  EVAL_STRING("(defmacro defvar (a b)\
                 `(begin\
                    (define , a , b)\
                    (define-envelope (symbol->string ', a) , b)))");
  /* this is trying to keep track of envelopes for the envelope editor */

  EVAL_STRING("(define (" S_snd_apropos " val) (snd-print (with-output-to-string (lambda () (apropos (if (string? val) val (object->string val)))))))");
  EVAL_STRING("(read-set! keywords 'prefix)");
  EVAL_STRING("(print-enable 'source)");  /* added 13-Feb-01 */

  /* not sure about these two */
  EVAL_STRING("(define scale-sound-to scale-to)");
  EVAL_STRING("(define scale-sound-by scale-by)");

  /* from ice-9/r4rs.scm but with output to snd listener */
  EVAL_STRING("(define snd-last-file-loaded #f)");
  EVAL_STRING("(set! %load-hook (lambda (filename)\
                                  (set! snd-last-file-loaded filename)\
                                  (if %load-verbosely\
                                    (snd-print (format #f \";;; loading ~S\" filename)))))");
#endif

#if USE_MOTIF
  YES_WE_HAVE("snd-motif");
#endif
#if USE_GTK
  YES_WE_HAVE("snd-gtk");
#endif
#if USE_NO_GUI
  YES_WE_HAVE("snd-nogui");
#endif
#if HAVE_GUILE
  YES_WE_HAVE("snd-guile");
#endif
#if HAVE_RUBY
  YES_WE_HAVE("snd-ruby");
#endif

  YES_WE_HAVE("snd");
}

#if (!HAVE_GUILE)
SCM scm_return_first(SCM a, ...)
{
  return(a);
}
#endif

#if HAVE_LIBREP

static rep_xsubr **obarr = NULL;
static rep_string **obstr = NULL;
static int obarr_size = 0;
static int obarr_ctr = 0;

void librep_new_procedure(const char *name, SCM (*func)(), int reqargs, int optargs, int rstargs, const char *doc)
{
  rep_xsubr *ob;
  rep_string *str;
  if (obarr_ctr == obarr_size)
    {
      if (obarr == NULL)
	{
	  obarr = (rep_xsubr **)CALLOC(1024, sizeof(rep_xsubr *));
	  obstr = (rep_string **)CALLOC(1024, sizeof(rep_string *));
	}
      else 
	{
	  obarr = (rep_xsubr **)REALLOC(obarr, (obarr_size + 1024) * sizeof(rep_xsubr *));
	  obstr = (rep_string **)REALLOC(obstr, (obarr_size + 1024) * sizeof(rep_string *));
	}
      obarr_size += 1024;
    }
  obarr[obarr_ctr] = (rep_xsubr *)CALLOC(1, sizeof(rep_xsubr));
  obstr[obarr_ctr] = (rep_string *)CALLOC(1, sizeof(rep_string));
  str = obstr[obarr_ctr];
  str->car = (strlen(name) << rep_STRING_LEN_SHIFT) | rep_CELL_STATIC_BIT | rep_String;
  str->data = (u_char *)name;
  ob = obarr[obarr_ctr++];
  switch (reqargs + optargs)
    {
    case 0: ob->car = rep_Subr0; break;
    case 1: ob->car = rep_Subr1; break;
    case 2: ob->car = rep_Subr2; break;
    case 3: ob->car = rep_Subr3; break;
    case 4: ob->car = rep_Subr4; break;
    case 5: ob->car = rep_Subr5; break;
    default: ob->car = rep_SubrN; break;
    }
  ob->fun = func;
  ob->int_spec = rep_NULL;
  ob->name = rep_VAL(str);
  rep_add_subr(ob, rep_TRUE);
}

static repv **obvars = NULL;
static rep_string **obvarstrs = NULL;
static int obvars_size = 0;
static int obvars_ctr = 0;

void librep_new_variable(const char *name, int val, const char *doc)
{
  repv *q;
  rep_string *qs;
  if (obvars_size == obvars_ctr)
    {
      if (obvars == NULL)
	{
	  obvars = (repv **)CALLOC(1024, sizeof(repv *));
	  obvarstrs = (rep_string **)CALLOC(1024, sizeof(rep_string *));
	}
      else 
	{
	  obvars = (repv **)REALLOC(obvars, (obvars_size + 1024) * sizeof(repv *));
	  obvarstrs = (rep_string **)REALLOC(obvarstrs, (obvars_size + 1024) * sizeof(rep_string *));
	}
      obvars_size += 1024;
    }
  obvars[obvars_ctr] = (repv *)CALLOC(1, sizeof(repv));
  obvarstrs[obvars_ctr] = (rep_string *)CALLOC(1, sizeof(rep_string));
  qs = obvarstrs[obvars_ctr];
  q = obvars[obvars_ctr++];
  qs->car = (strlen(name) << rep_STRING_LEN_SHIFT) | rep_CELL_STATIC_BIT | rep_String;
  qs->data = (u_char *)name;
  rep_intern_static(q, rep_VAL(qs));
  Fmake_variable_special(*q);
  rep_SYM(q)->car |= rep_SF_DEFVAR;
  Fset((*q), TO_SCM_INT(val));
}

SCM librep_eval_string(char *data)
{
  int c;
  repv stream, res;
  stream = Fmake_string_input_stream(TO_SCM_STRING(data), TO_SCM_INT(0));
  c = rep_stream_getc(stream);
  res = rep_eval(rep_readl(stream, &c));
  return(res);
}

#endif

#if HAVE_MZSCHEME
  
/* need a way to turn DEFINE_PROC into a tie into a call on C proc -- see ruby case */

static Scheme_Object *snd_inner(void *closure_data, int argc, Scheme_Object **argv)
{
  PROCEDURE func = (PROCEDURE *)closure_data;
  switch (argc)
    {
    case 0: return((*func)()); break;
    case 1: return((*func)(argv[0])); break;
      /* etc */
    }
}

/*
  return scheme_make_closed_prim_w_arity(snd_inner,
					 argv[0], <-??
					 funcname,
					 0, 10);

*/
#endif

#if HAVE_RUBY

static char *Scheme_to_Ruby(char *name)
{
  /* replace any non-alphanumeric except "?" with "_". "?" -> "_p". '->" -> "2" */
  char *new_name = NULL;
  int len, i, j;
  len = snd_strlen(name);
  if (len > 0)
    {
      new_name = (char *)CALLOC(len + 2, sizeof(char)); /* +1 for possible _p, +1 for possible $ */
      for (i = 0, j = 0; i < len; i++)
	{
	  if (isalnum(name[i]))
	    new_name[j++] = name[i];
	  else 
	    {
	      if ((name[i] == '-') &&
		  (name[i + 1] == '>'))
		{
		  new_name[j++] = '2';
		  i++;
		}
	      else
		{
		  new_name[j++] = '_';
		  if (name[i] == '?')
		    new_name[j++] = 'p';
		}
	    }
	}
    }
  return(new_name);
}

char *Scheme_constant_to_Ruby(char *name)
{
  /* upcase 1st char */
  char *new_name;
  new_name = Scheme_to_Ruby(name);
  new_name[0] = toupper(new_name[0]);
  return(new_name);
}

char *Scheme_procedure_to_Ruby(char *name)
{
  return(Scheme_to_Ruby(name));
}

char *Scheme_global_variable_to_Ruby(char *name)
{
  /* prepend $ */
  char *new_name;
  int i, len;
  new_name = Scheme_to_Ruby(name);
  len = snd_strlen(new_name);
  for (i = len; i > 0; i--)
    new_name[i] = new_name[i - 1];
  new_name[0] = '$';
  return(new_name);
}

SCM snd_rb_cdr(SCM val)
{
  rb_ary_delete_at(val, 0);
  return(val);
}

#endif
