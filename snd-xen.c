#include "snd.h"
#include "vct.h"
#include "clm2xen.h"
#include "sndlib-strings.h"

static snd_state *state = NULL;


/* -------- protect XEN vars from GC -------- */

static XEN gc_protection;
static int gc_protection_size = 0;
#define DEFAULT_GC_VALUE XEN_ZERO
static int gc_last_cleared = -1;
static int gc_last_set = -1;

void snd_protect(XEN obj)
{
  int i, old_size;
  XEN tmp;
  XEN *gcdata;
  if (gc_protection_size == 0)
    {
      gc_protection_size = 128;
      /* we don't know the size in advance since each channel can have its own edit/undo hooks */
      gc_protection = XEN_MAKE_VECTOR(gc_protection_size, DEFAULT_GC_VALUE);
      XEN_PROTECT_FROM_GC(gc_protection);
      XEN_VECTOR_SET(gc_protection, 0, obj);
      gc_last_set = 0;
    }
  else
    {
      gcdata = XEN_VECTOR_ELEMENTS(gc_protection);
      if ((gc_last_cleared >= 0) && 
	  XEN_EQ_P(gcdata[gc_last_cleared], DEFAULT_GC_VALUE))
	{
	  XEN_VECTOR_SET(gc_protection, gc_last_cleared, obj);
	  gc_last_set = gc_last_cleared;
	  gc_last_cleared = -1;
	  return;
	}
      for (i = 0; i < gc_protection_size; i++)
	if (XEN_EQ_P(gcdata[i], DEFAULT_GC_VALUE))
	  {
	    XEN_VECTOR_SET(gc_protection, i, obj);
	    gc_last_set = i;
	    return;
	  }
      tmp = gc_protection;
      old_size = gc_protection_size;
      gc_protection_size *= 2;
      gc_protection = XEN_MAKE_VECTOR(gc_protection_size, DEFAULT_GC_VALUE);
      XEN_PROTECT_FROM_GC(gc_protection);
      for (i = 0; i < old_size; i++)
	{
	  XEN_VECTOR_SET(gc_protection, i, XEN_VECTOR_REF(tmp, i));
	  XEN_VECTOR_SET(tmp, i, DEFAULT_GC_VALUE);
	}
      XEN_VECTOR_SET(gc_protection, old_size, obj);
      gc_last_set = old_size;
    }
}

void snd_unprotect(XEN obj)
{
  int i;
  XEN *gcdata;
  gcdata = XEN_VECTOR_ELEMENTS(gc_protection);
  if ((gc_last_set >= 0) && 
      (XEN_EQ_P(gcdata[gc_last_set], obj)))
    {
      XEN_VECTOR_SET(gc_protection, gc_last_set, DEFAULT_GC_VALUE);
      gc_last_cleared = gc_last_set;
      gc_last_set = -1;
      return;
    }
  for (i = 0; i < gc_protection_size; i++)
    if (XEN_EQ_P(gcdata[i], obj))
      {
	XEN_VECTOR_SET(gc_protection, i, DEFAULT_GC_VALUE);
	gc_last_cleared = i;
	return;
      }
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

static XEN snd_catch_scm_error(void *data, XEN tag, XEN throw_args) /* error handler */
{
#if HAVE_GUILE
  /* this is actually catching any throw not caught elsewhere, I think */
  snd_info *sp;
  char *possible_code;
  XEN port; XEN ans; XEN stmp;
  XEN stack;
  char *name_buf = NULL;

#ifdef SCM_MAKE_CHAR
  port = scm_mkstrport(XEN_ZERO, 
		       scm_make_string(C_TO_XEN_INT(MAX_ERROR_STRING_LENGTH), 
				       SCM_MAKE_CHAR(0)),
		       SCM_OPN | SCM_WRTNG,
		       __FUNCTION__);
#else
  port = scm_mkstrport(XEN_ZERO, 
		       scm_make_string(C_TO_XEN_INT(MAX_ERROR_STRING_LENGTH), 
				       XEN_UNDEFINED),
		       SCM_OPN | SCM_WRTNG,
		       __FUNCTION__);
#endif

#if DEBUGGING
  {
    /* force out an error before possible backtrace call */
    XEN lport;
    lport = scm_mkstrport(XEN_ZERO, 
			  scm_make_string(C_TO_XEN_INT(MAX_ERROR_STRING_LENGTH), 
					  SCM_MAKE_CHAR(0)),
			  SCM_OPN | SCM_WRTNG,
			  __FUNCTION__);
    scm_display(tag, lport);
    scm_puts(": ", lport);
    scm_display(throw_args, lport);
    scm_force_output(lport);
    ans = scm_strport_to_string(lport);
    name_buf = XEN_TO_NEW_C_STRING(ans);
    fprintf(stderr, name_buf);
    if (name_buf) free(name_buf);
    name_buf = NULL;
  }
#endif

  if ((XEN_LIST_P(throw_args)) && 
      (XEN_LIST_LENGTH(throw_args) > 0))
    {
      if (XEN_NOT_FALSE_P(XEN_CAR(throw_args)))
	{
	  scm_display(XEN_CAR(throw_args), port);
	  scm_puts(": ", port);
	}
      if (XEN_LIST_LENGTH(throw_args) > 1)
	{
	  if (XEN_EQ_P(tag, NO_SUCH_FILE))
	    {
	      scm_display(tag, port);
	      scm_puts(" \"", port);
	      scm_display(XEN_CADR(throw_args), port);
	      scm_puts("\" ", port);
	      if (XEN_LIST_LENGTH(throw_args) > 2)
		scm_display(XEN_CDDR(throw_args), port);
	    }
	  else
	    {
	      if ((XEN_EQ_P(tag, NO_SUCH_SOUND)) || (XEN_EQ_P(tag, NO_SUCH_MIX)) || (XEN_EQ_P(tag, NO_SUCH_MARK)) ||
		  (XEN_EQ_P(tag, NO_SUCH_MENU)) || (XEN_EQ_P(tag, NO_SUCH_REGION)) || (XEN_EQ_P(tag, MUS_MISC_ERROR)) ||
		  (XEN_EQ_P(tag, NO_SUCH_CHANNEL)) || (XEN_EQ_P(tag, NO_SUCH_EDIT)) ||
		  (XEN_EQ_P(tag, NO_SUCH_AXIS_INFO)) || (XEN_EQ_P(tag, NO_SUCH_AXIS_CONTEXT)) ||
		  (XEN_EQ_P(tag, CANNOT_SAVE)) || (XEN_EQ_P(tag, CANNOT_PRINT)) || (XEN_EQ_P(tag, BAD_ARITY)) ||
		  (XEN_EQ_P(tag, IMPOSSIBLE_BOUNDS)) || (XEN_EQ_P(tag, NO_SUCH_SAMPLE)))
		{
		  scm_display(tag, port);
		  scm_puts(" ", port);
		  scm_display(throw_args, port);
		}
	      else
		{
		  stmp = XEN_CADR(throw_args);
		  if ((XEN_STRING_P(stmp)) && (XEN_LIST_LENGTH(throw_args) > 2))
		    scm_display_error_message(stmp, XEN_CADDR(throw_args), port);
		  else scm_display(tag, port);
		  if (show_backtrace(state))
		    {
#if HAVE_SCM_C_DEFINE
		      stack = scm_fluid_ref(VARIABLE_REF(scm_the_last_stack_fluid_var));
#else
		      stack = scm_fluid_ref(XEN_CDR(scm_the_last_stack_fluid));
#endif
		      if (XEN_NOT_FALSE_P(stack)) 
			scm_display_backtrace(stack, port, XEN_UNDEFINED, XEN_UNDEFINED);
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
      (snd_strlen(possible_code) < PRINT_BUFFER_SIZE))
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
  name_buf = XEN_TO_NEW_C_STRING(ans);
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

#if HAVE_GUILE
static XEN snd_internal_stack_catch (XEN tag,
				     XEN_CATCH_BODY_TYPE body,
				     void *body_data,
				     scm_catch_handler_t handler,
				     void *handler_data)
{ /* declaration from libguile/throw */
  XEN result;
  state->catch_exists++;
  /* one function can invoke, for example, a hook that will call back here setting up a nested catch */
  result = scm_internal_stack_catch(tag, body, body_data, handler, handler_data);
  state->catch_exists--;
  return(result);
}

XEN snd_catch_any(XEN_CATCH_BODY_TYPE body, void *body_data, const char *caller)
{
  return(snd_internal_stack_catch(XEN_TRUE, body, body_data, snd_catch_scm_error, (void *)caller));
}
#else
XEN snd_catch_any(XEN_CATCH_BODY_TYPE body, void *body_data, const char *caller)
{
  /* TODO: this needs to catch errors */
  return((*body)(body_data));
}
#endif

char *procedure_ok(XEN proc, int args, const char *caller, const char *arg_name, int argn)
{
  /* if string returned, needs to be freed */
  XEN arity;
  int rargs, oargs, restargs;
#if TIMING
  return(NULL);
#endif
  if (!(XEN_PROCEDURE_P(proc)))
    {
      if (XEN_NOT_FALSE_P(proc)) /* #f as explicit arg to clear */
	return(mus_format("%s (%s arg %d) is not a procedure!", arg_name, caller, argn));
    }
  else
    {
      arity = XEN_ARITY(proc);
#if HAVE_RUBY
      rargs = XEN_TO_C_INT(arity);
      if ((rargs > args) ||
	  ((rargs < 0) && (-rargs > args)))
	return(mus_format("%s function (%s arg %d) should take %d args, not %d", 
			  arg_name, caller, argn, args, (rargs < 0) ? (-rargs) : rargs));
#else
      snd_protect(arity);
      rargs = XEN_TO_SMALL_C_INT(XEN_CAR(arity));
      oargs = XEN_TO_SMALL_C_INT(XEN_CADR(arity));
      restargs = ((XEN_TRUE_P(XEN_CADDR(arity))) ? 1 : 0);
      snd_unprotect(arity);
      if (rargs > args)
	return(mus_format("%s function (%s arg %d) should take %d argument%s, but instead requires %d",
			  arg_name, caller, argn, args, (args != 1) ? "s" : "", rargs));
      if ((restargs == 0) && ((rargs + oargs) < args))
	return(mus_format("%s function (%s arg %d) should accept at least %d argument%s, but instead accepts only %d",
			  arg_name, caller, argn, args, (args != 1) ? "s" : "", rargs + oargs));
#endif
    }
  return(NULL);
}

int procedure_ok_with_error(XEN proc, int req_args, const char *caller, const char *arg_name, int argn)
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

XEN snd_no_such_file_error(const char *caller, XEN filename)
{
  XEN_ERROR(NO_SUCH_FILE,
	    XEN_LIST_3(C_TO_XEN_STRING(caller),
		       filename,
		       C_TO_XEN_STRING(strerror(errno))));
  return(XEN_FALSE);
}

XEN snd_no_such_channel_error(const char *caller, XEN snd, XEN chn)
{
  XEN_ERROR(NO_SUCH_CHANNEL,
	    XEN_LIST_3(C_TO_XEN_STRING(caller),
		       snd,
		       chn));
  return(XEN_FALSE);
}

XEN snd_no_active_selection_error(const char *caller)
{
  XEN_ERROR(NO_ACTIVE_SELECTION,
	    XEN_LIST_1(C_TO_XEN_STRING(caller)));
  return(XEN_FALSE);
}

XEN snd_bad_arity_error(const char *caller, XEN errstr, XEN proc)
{
  XEN_ERROR(BAD_ARITY,
	    XEN_LIST_3(C_TO_XEN_STRING(caller),
		       errstr,
		       proc));
  return(XEN_FALSE);
}



/* -------- various evaluators (within our error handler) -------- */

XEN eval_str_wrapper(void *data)
{
  return(XEN_EVAL_C_STRING((char *)data));
}

XEN eval_form_wrapper(void *data)
{
#if (SCM_DEBUG_TYPING_STRICTNESS == 2)
  return(XEN_FALSE);
#else
  return(XEN_EVAL_FORM((XEN)data));
#endif
}

static XEN eval_file_wrapper(void *data)
{
  last_file_loaded = (char *)data;
  XEN_LOAD_FILE((char *)data);
  last_file_loaded = NULL;
  return(XEN_UNDEFINED);
}

#if HAVE_GUILE
static XEN g_call0_1(void *arg)
{
#if (SCM_DEBUG_TYPING_STRICTNESS == 2)
  return(XEN_FALSE);
#else
  return(scm_apply((XEN)arg, XEN_EMPTY_LIST, XEN_EMPTY_LIST));
#endif
}
#endif

XEN g_call0(XEN proc, const char *caller) /* replacement for gh_call0 -- protect ourselves from premature exit(!$#%@$) */
{
#if HAVE_GUILE
#if (SCM_DEBUG_TYPING_STRICTNESS == 2)
  return(XEN_FALSE);
#else
  return(snd_catch_any(g_call0_1, (void *)proc, caller));
#endif
#else
  return(proc);
#endif
}

#if HAVE_GUILE
static XEN g_call1_1(void *arg)
{
  return(scm_apply(((XEN *)arg)[0], 
 		   ((XEN *)arg)[1], 
 		   XEN_APPLY_ARG_LIST_END));
}
#endif

XEN g_call1(XEN proc, XEN arg, const char *caller)
{
  XEN args[2];
#if HAVE_GUILE
  args[0] = proc;
  args[1] = arg;
  return(snd_catch_any(g_call1_1, (void *)args, caller));
#else
  return(arg);
#endif
}

#if HAVE_GUILE
static XEN g_call_any_1(void *arg)
{
  return(scm_apply(((XEN *)arg)[0], 
		   ((XEN *)arg)[1], 
		   XEN_EMPTY_LIST));
}
#endif

XEN g_call_any(XEN proc, XEN arglist, const char *caller)
{
  XEN args[2];
  args[0] = proc;
  args[1] = arglist;
#if HAVE_GUILE
  return(snd_catch_any(g_call_any_1, (void *)args, caller));
#else
  return(arglist);
#endif
}

#if HAVE_GUILE
static XEN g_call2_1(void *arg)
{
  return(scm_apply(((XEN *)arg)[0], 
 		   ((XEN *)arg)[1], 
 		   XEN_CONS(((XEN *)arg)[2], XEN_APPLY_ARG_LIST_END)));
}
#endif

XEN g_call2(XEN proc, XEN arg1, XEN arg2, const char *caller)
{
  XEN args[3];
#if HAVE_GUILE
  args[0] = proc;
  args[1] = arg1;
  args[2] = arg2;
  return(snd_catch_any(g_call2_1, (void *)args, caller));
#else
  return(arg1);
#endif
}

#if HAVE_GUILE
static XEN g_call3_1(void *arg)
{
  return(scm_apply(((XEN *)arg)[0], 
		   ((XEN *)arg)[1], 
		   XEN_CONS_2(((XEN *)arg)[2], 
			      ((XEN *)arg)[3], 
			      XEN_APPLY_ARG_LIST_END)));
}
#endif

XEN g_call3(XEN proc, XEN arg1, XEN arg2, XEN arg3, const char *caller)
{
  XEN args[4];
#if HAVE_GUILE
  args[0] = proc;
  args[1] = arg1;
  args[2] = arg2;
  args[3] = arg3;
  return(snd_catch_any(g_call3_1, (void *)args, caller));
#else
  return(arg1);
#endif
}

char *g_print_1(XEN obj, const char *caller)
{
  char *str1 = NULL;
#if HAVE_GUILE
#if HAVE_SCM_OBJECT_TO_STRING
  return(XEN_TO_NEW_C_STRING(XEN_TO_STRING(obj))); 
#else
  XEN str; XEN val;
  XEN port;
  str = scm_makstr (0, 0);
  port = scm_mkstrport (XEN_ZERO, str, SCM_OPN | SCM_WRTNG, caller);
  scm_prin1 (obj, port, 1);
  val = scm_strport_to_string(port);
  scm_close_port (port);
  str1 = XEN_TO_NEW_C_STRING(val);
#endif
#endif
#if HAVE_RUBY
  if (XEN_NULL_P(obj))
    return(copy_string("nil")); /* Ruby returns the null string in this case??? */
  return(XEN_TO_NEW_C_STRING(XEN_TO_STRING(obj)));
#endif
  return(str1);
}

char *gl_print(XEN result, const char *caller)
{
  char *newbuf = NULL, *str = NULL;
  int i, ilen, savelen, savectr, slen;
  /* specialize vectors which can be enormous in this context */
  if ((!(XEN_VECTOR_P(result))) || 
      ((int)(XEN_VECTOR_LENGTH(result)) <= print_length(state)))
    return(g_print_1(result, caller));
  ilen = print_length(state); 
  newbuf = (char *)calloc(128, sizeof(char));
  savelen = 128;
  savectr = 3;
  sprintf(newbuf, "#("); 
  for (i = 0; i < ilen; i++)
    {
      str = g_print_1(XEN_VECTOR_REF(result, i), caller);
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

XEN snd_eval_str(snd_state *ss, char *buf)
{
  return(snd_report_result(ss, snd_catch_any(eval_str_wrapper, (void *)buf, buf), buf));
}

static XEN print_hook;

XEN snd_report_result(snd_state *ss, XEN result, char *buf)
{
  snd_info *sp = NULL;
  char *str = NULL;
  XEN res = XEN_FALSE;
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
      if (XEN_HOOKED(print_hook))
	res = g_c_run_or_hook(print_hook, 
			      XEN_LIST_1(C_TO_XEN_STRING(str)),
			      S_print_hook);
      if (XEN_FALSE_P(res))
	listener_append_and_prompt(ss, str);
    }
  if (str) free(str);
  return(result);
}

XEN snd_report_listener_result(snd_state *ss, XEN form)
{
  char *str = NULL;
  XEN res = XEN_FALSE; XEN result;
  listener_append(ss, "\n");
#if HAVE_RUBY
  str = gl_print(form, "repl");
#else
  result = snd_catch_any(eval_form_wrapper, (void *)form, NULL);
  str = gl_print(result, "eval");
#endif
  if (ss->listening)
    {
      if (XEN_HOOKED(print_hook))
	res = g_c_run_or_hook(print_hook, 
			      XEN_LIST_1(C_TO_XEN_STRING(str)),
			      S_print_hook);
      if (XEN_FALSE_P(res))
	listener_append_and_prompt(ss, str);
    }
  if (str) free(str);
  return(result);
}

void snd_eval_property_str(snd_state *ss, char *buf)
{
  XEN result;
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
  XEN result;
  char *str = NULL;
  if (snd_strlen(buf) == 0) return;
  str = stdin_check_for_full_expression(buf);
  if (str)
    {
      send_error_output_to_stdout = 1;
      result = snd_catch_any(eval_str_wrapper, (void *)str, str);
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
#if (!HAVE_RUBY)
  str2 = (char *)CALLOC(PRINT_BUFFER_SIZE, sizeof(char));
  mus_snprintf(str2, PRINT_BUFFER_SIZE, "(load \"%s\")", filename);
#endif
  if (!mus_file_probe(str))
    {
      /* try tacking on extension */
      str1 = (char *)CALLOC(snd_strlen(str) + 2 + strlen(XEN_FILE_EXTENSION), sizeof(char));
      sprintf(str1, "%s.%s", str, XEN_FILE_EXTENSION);
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

static XEN g_snd_print(XEN msg)
{
  #define H_snd_print "(" S_snd_print " str) displays str in the lisp listener window"
  char *str = NULL;
  if (XEN_STRING_P(msg))
    str = XEN_TO_NEW_C_STRING(msg);
  else
    {
      if (XEN_CHAR_P(msg))
	{
	  str = (char *)CALLOC(2, sizeof(char));
	  str[0] = XEN_TO_C_CHAR(msg);
	}
      else str = gl_print(msg, S_snd_print);
    }
  check_for_event(state);
  listener_append(state, str);
  if (str) free(str);
  return(msg);
}


/* -------- global variables -------- */

static XEN g_region_graph_style(void) {return(C_TO_XEN_INT(region_graph_style(state)));}
static XEN g_set_region_graph_style(XEN val) 
{
  #define H_region_graph_style "(" S_region_graph_style ") refers to the graph-style of the region dialog graph"
  XEN_ASSERT_TYPE(XEN_INTEGER_P(val), val, XEN_ONLY_ARG, "set-" S_region_graph_style, "an integer");
  set_region_graph_style(state, XEN_TO_C_INT(val));
  reflect_region_graph_style(state);
  return(val);
}

static XEN g_ask_before_overwrite(void) {return(C_TO_XEN_BOOLEAN(ask_before_overwrite(state)));}
static XEN g_set_ask_before_overwrite(XEN val) 
{
  #define H_ask_before_overwrite "(" S_ask_before_overwrite ") should be #t if you want Snd to ask before overwriting a file"
  XEN_ASSERT_TYPE(XEN_BOOLEAN_IF_BOUND_P(val), val, XEN_ONLY_ARG, "set-" S_ask_before_overwrite, "a boolean");
  set_ask_before_overwrite(state, XEN_TO_C_BOOLEAN_OR_TRUE(val)); 
  return(C_TO_XEN_BOOLEAN(ask_before_overwrite(state)));
}

static XEN g_audio_output_device(void) {return(C_TO_XEN_INT(audio_output_device(state)));}
static XEN g_set_audio_output_device(XEN val) 
{
  #define H_audio_output_device "(" S_audio_output_device ") is the current sndlib default output device (mus-audio-default)"
  XEN_ASSERT_TYPE(XEN_INTEGER_P(val), val, XEN_ONLY_ARG, "set-" S_audio_output_device, "an integer"); 
  set_audio_output_device(state, XEN_TO_C_INT(val)); 
  return(C_TO_XEN_INT(audio_output_device(state)));
}

static XEN g_audio_input_device(void) {return(C_TO_XEN_INT(audio_input_device(state)));}
static XEN g_set_audio_input_device(XEN val) 
{
  #define H_audio_input_device "(" S_audio_input_device ") is the current sndlib default input device (mus-audio-default)"
  XEN_ASSERT_TYPE(XEN_INTEGER_P(val), val, XEN_ONLY_ARG, "set-" S_audio_input_device, "an integer"); 
  set_audio_input_device(state, XEN_TO_C_INT(val)); 
  return(C_TO_XEN_INT(audio_input_device(state)));
}

static XEN g_minibuffer_history_length(void) {return(C_TO_XEN_INT(minibuffer_history_length(state)));}
static XEN g_set_minibuffer_history_length(XEN val) 
{
  #define H_minibuffer_history_length "(" S_minibuffer_history_length ") is the minibuffer history length"
  int len;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(val), val, XEN_ONLY_ARG, "set-" S_minibuffer_history_length, "an integer");
  len = XEN_TO_C_INT(val);
  if (len > 0)
    set_minibuffer_history_length(state, len);
  return(C_TO_XEN_INT(minibuffer_history_length(state)));
}

static XEN g_dac_size(void) {return(C_TO_XEN_INT(dac_size(state)));}
static XEN g_set_dac_size(XEN val) 
{
  #define H_dac_size "(" S_dac_size ") is the current DAC buffer size (256)"
  int len;
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ONLY_ARG, "set-" S_dac_size, "a number");
  len = XEN_TO_C_INT_OR_ELSE(val, 0);
  if (len > 0)
    set_dac_size(state, len);
  return(C_TO_XEN_INT(dac_size(state)));
}

static XEN g_dac_combines_channels(void) {return(C_TO_XEN_BOOLEAN(dac_combines_channels(state)));}
static XEN g_set_dac_combines_channels(XEN val) 
{
  #define H_dac_combines_channels "(" S_dac_combines_channels ") should be #t if extra channels are to be mixed into available ones during playing (#t)"
  XEN_ASSERT_TYPE(XEN_BOOLEAN_IF_BOUND_P(val), val, XEN_ONLY_ARG, "set-" S_dac_combines_channels, "a boolean");
  set_dac_combines_channels(state, XEN_TO_C_BOOLEAN_OR_TRUE(val)); 
  return(C_TO_XEN_BOOLEAN(dac_combines_channels(state)));
}

static XEN g_auto_resize(void) {return(C_TO_XEN_BOOLEAN(auto_resize(state)));}
static XEN g_set_auto_resize(XEN val) 
{
  #define H_auto_resize "(" S_auto_resize ") should be #t if Snd can change its main window size as it pleases (#t)"
  XEN_ASSERT_TYPE(XEN_BOOLEAN_IF_BOUND_P(val), val, XEN_ONLY_ARG, "set-" S_auto_resize, "a boolean");
  set_auto_resize(state, XEN_TO_C_BOOLEAN_OR_TRUE(val)); 
  reflect_resize(state); 
  return(C_TO_XEN_BOOLEAN(auto_resize(state)));
}

static XEN g_auto_update(void) {return(C_TO_XEN_BOOLEAN(auto_update(state)));}
static XEN g_set_auto_update(XEN val) 
{
  #define H_auto_update "(" S_auto_update ") -> #t if Snd should automatically update a file if it changes unexpectedly (#f)"
  XEN_ASSERT_TYPE(XEN_BOOLEAN_IF_BOUND_P(val), val, XEN_ONLY_ARG, "set-" S_auto_update, "a boolean");
  set_auto_update(state, XEN_TO_C_BOOLEAN_OR_TRUE(val)); 
  return(C_TO_XEN_BOOLEAN(auto_update(state)));
}

static XEN g_filter_env_in_hz(void) {return(C_TO_XEN_BOOLEAN(filter_env_in_hz(state)));}
static XEN g_set_filter_env_in_hz(XEN val) 
{
  #define H_filter_env_in_hz "(" S_filter_env_in_hz ") -> #t if filter env x axis should be in Hz"
  XEN_ASSERT_TYPE(XEN_BOOLEAN_IF_BOUND_P(val), val, XEN_ONLY_ARG, "set-" S_filter_env_in_hz, "a boolean");
  set_filter_env_in_hz(state, XEN_TO_C_BOOLEAN_OR_TRUE(val)); 
  return(C_TO_XEN_BOOLEAN(filter_env_in_hz(state)));
}

static XEN g_color_cutoff(void) {return(C_TO_XEN_DOUBLE(color_cutoff(state)));}
static XEN g_set_color_cutoff(XEN val) 
{
  #define H_color_cutoff "(" S_color_cutoff ") -> col" STR_OR " map cutoff point (default .003)"
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ONLY_ARG, "set-" S_color_cutoff, "a number");
  set_color_cutoff(state, mus_fclamp(0.0,
				     XEN_TO_C_DOUBLE(val),
				     0.25)); 
  return(C_TO_XEN_DOUBLE(color_cutoff(state)));
}

static XEN g_color_inverted(void) {return(C_TO_XEN_BOOLEAN(color_inverted(state)));}
static XEN g_set_color_inverted(XEN val) 
{
  #define H_color_inverted "(" S_color_inverted ") -> whether the col" STR_OR "map in operation should be inverted"
  XEN_ASSERT_TYPE(XEN_BOOLEAN_IF_BOUND_P(val), val, XEN_ONLY_ARG, "set-" S_color_inverted, "a boolean");
  set_color_inverted(state, XEN_TO_C_BOOLEAN_OR_TRUE(val)); 
  return(C_TO_XEN_BOOLEAN(color_inverted(state)));
}

static XEN g_color_scale(void) {return(C_TO_XEN_DOUBLE(color_scale(state)));}
static XEN g_set_color_scale(XEN val) 
{
  #define H_color_scale "(" S_color_scale ") -> essentially a darkness setting for col" STR_OR "maps (0.5)"
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ONLY_ARG, "set-" S_color_scale, "a number"); 
  set_color_scale(state, mus_fclamp(0.0,
				    XEN_TO_C_DOUBLE(val),
				    1.0)); 
  return(C_TO_XEN_DOUBLE(color_scale(state)));
}

static XEN g_auto_update_interval(void) {return(C_TO_XEN_DOUBLE(auto_update_interval(state)));}
static XEN g_set_auto_update_interval(XEN val) 
{
  Float ctime;
  #define H_auto_update_interval "(" S_auto_update_interval ") -> time (seconds) between background checks for changed file on disk (60)"
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ONLY_ARG, "set-" S_auto_update_interval, "a number"); 
  ctime = XEN_TO_C_DOUBLE(val);
  if ((ctime < 0.0) || (ctime > (24 * 3600)))
    mus_misc_error("set-" S_auto_update_interval, "invalid time:", val);
  set_auto_update_interval(state, XEN_TO_C_DOUBLE(val)); 
  return(C_TO_XEN_DOUBLE(auto_update_interval(state)));
}

static XEN g_default_output_chans(void) {return(C_TO_XEN_INT(default_output_chans(state)));}
static XEN g_set_default_output_chans(XEN val) 
{
  #define H_default_output_chans "(" S_default_output_chans ") -> default number of channels when a new or temporary file is created (1)"
  XEN_ASSERT_TYPE(XEN_INTEGER_P(val), val, XEN_ONLY_ARG, "set-" S_default_output_chans, "an integer"); 
  set_default_output_chans(state, XEN_TO_C_INT(val));
  return(C_TO_XEN_INT(default_output_chans(state)));
}

static XEN g_default_output_srate(void) {return(C_TO_XEN_INT(default_output_srate(state)));}
static XEN g_set_default_output_srate(XEN val) 
{
  #define H_default_output_srate "(" S_default_output_srate ") -> default srate when a new or temporary file is created (22050)" 
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ONLY_ARG, "set-" S_default_output_srate, "a number"); 
  set_default_output_srate(state, XEN_TO_C_INT_OR_ELSE(val, 0));
  return(C_TO_XEN_INT(default_output_srate(state)));
}

static XEN g_default_output_type(void) {return(C_TO_XEN_INT(default_output_type(state)));}
static XEN g_set_default_output_type(XEN val) 
{
  int typ;
  #define H_default_output_type "(" S_default_output_type ") -> default header type when a new or temporary file is created. \
Normally this is " S_mus_next "; -1 here indicates you want Snd to use the current sound's header type, if possible. \
Other writable headers include " S_mus_aiff ", " S_mus_riff ", " S_mus_ircam ", " S_mus_nist ", " S_mus_aifc ", and " S_mus_raw "."

  XEN_ASSERT_TYPE(XEN_INTEGER_P(val), val, XEN_ONLY_ARG, "set-" S_default_output_type, "an integer"); 
  typ = XEN_TO_C_INT(val);
  if (mus_header_writable(typ, -2))
    set_default_output_type(state, typ); 
  else mus_misc_error("set-" S_default_output_type, 
		      "can't write this header type", 
		      XEN_LIST_2(val, 
				 C_TO_XEN_STRING(mus_header_type_name(typ))));
  return(C_TO_XEN_INT(default_output_type(state)));
}

static XEN g_default_output_format(void) {return(C_TO_XEN_INT(default_output_format(state)));}
static XEN g_set_default_output_format(XEN val) 
{
  int format;
  #define H_default_output_format "(" S_default_output_format ") -> default data format when a new or temporary file is created, \
normally " S_mus_bshort "; -1 here means try to use the current sound's data format; many other formats \
are available, but not all are compatible with all header types"

  XEN_ASSERT_TYPE(XEN_INTEGER_P(val), val, XEN_ONLY_ARG, "set-" S_default_output_format, "an integer"); 
  format = XEN_TO_C_INT(val);
  set_default_output_format(state, format); 
  return(C_TO_XEN_INT(default_output_format(state)));
}

static XEN g_eps_file(void) {return(C_TO_XEN_STRING(eps_file(state)));}
static XEN g_set_eps_file(XEN val) 
{
  #define H_eps_file "(" S_eps_file ") -> current eps ('Print' command) file name (snd.eps)"
  XEN_ASSERT_TYPE(XEN_STRING_P(val), val, XEN_ONLY_ARG, "set-" S_eps_file, "a string"); 
  if (eps_file(state)) free(eps_file(state));
  set_eps_file(state, XEN_TO_NEW_C_STRING(val)); 
  return(C_TO_XEN_STRING(eps_file(state)));
}

static XEN g_eps_left_margin(void) {return(C_TO_XEN_DOUBLE(eps_left_margin(state)));}
static XEN g_set_eps_left_margin(XEN val) 
{
  #define H_eps_left_margin "(" S_eps_left_margin ") -> current eps ('Print' command) left margin"
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ONLY_ARG, "set-" S_eps_left_margin, "a number"); 
  set_eps_left_margin(state, XEN_TO_C_DOUBLE(val));
  return(C_TO_XEN_DOUBLE(eps_left_margin(state)));
}

static XEN g_eps_bottom_margin(void) {return(C_TO_XEN_DOUBLE(eps_bottom_margin(state)));}
static XEN g_set_eps_bottom_margin(XEN val) 
{
  #define H_eps_bottom_margin "(" S_eps_bottom_margin ") -> current eps ('Print' command) bottom margin"
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ONLY_ARG, "set-" S_eps_bottom_margin, "a number"); 
  set_eps_bottom_margin(state, XEN_TO_C_DOUBLE(val));
  return(C_TO_XEN_DOUBLE(eps_bottom_margin(state)));
}

static XEN g_listener_prompt(void) {return(C_TO_XEN_STRING(listener_prompt(state)));}
static XEN g_set_listener_prompt(XEN val) 
{
  #define H_listener_prompt "(" S_listener_prompt ") -> the current lisp listener prompt character ('>') "
  XEN_ASSERT_TYPE(XEN_STRING_P(val), val, XEN_ONLY_ARG, "set-" S_listener_prompt, "a string"); 
  if (listener_prompt(state)) free(listener_prompt(state));
  set_listener_prompt(state, XEN_TO_NEW_C_STRING(val));
#if USE_NO_GUI
  {
#if HAVE_GUILE
    char *str;
    str = (char *)CALLOC(PRINT_BUFFER_SIZE, sizeof(char));
    mus_snprintf(str, PRINT_BUFFER_SIZE, "(set! scm-repl-prompt \"%s\")", listener_prompt(state));
    XEN_EVAL_C_STRING(str);
    FREE(str);
#endif
  }
#endif
  return(C_TO_XEN_STRING(listener_prompt(state)));
}

static XEN g_audio_state_file(void) {return(C_TO_XEN_STRING(audio_state_file(state)));}
static XEN g_set_audio_state_file(XEN val) 
{
  #define H_audio_state_file "(" S_audio_state_file ") -> filename for the mus-audio-save-state function (.snd-mixer)"
  XEN_ASSERT_TYPE(XEN_STRING_P(val), val, XEN_ONLY_ARG, "set-" S_audio_state_file, "a string"); 
  if (audio_state_file(state)) free(audio_state_file(state));
  set_audio_state_file(state, XEN_TO_NEW_C_STRING(val));
  return(C_TO_XEN_STRING(audio_state_file(state)));
}

static XEN g_movies(void) {return(C_TO_XEN_BOOLEAN(movies(state)));}
static XEN g_set_movies(XEN val) 
{
  #define H_movies "(" S_movies ") -> #t if mix graphs are update continuously as the mix is dragged (#t)"
  XEN_ASSERT_TYPE(XEN_BOOLEAN_IF_BOUND_P(val), val, XEN_ONLY_ARG, "set-" S_movies, "a boolean");
  set_movies(state, XEN_TO_C_BOOLEAN_OR_TRUE(val));
  return(C_TO_XEN_BOOLEAN(movies(state)));
}

static XEN g_selection_creates_region(void) {return(C_TO_XEN_BOOLEAN(selection_creates_region(state)));}
static XEN g_set_selection_creates_region(XEN val) 
{
  #define H_selection_creates_region "(" S_selection_creates_region ") -> #t if a region should be created each time a selection is made"
  XEN_ASSERT_TYPE(XEN_BOOLEAN_IF_BOUND_P(val), val, XEN_ONLY_ARG, "set-" S_selection_creates_region, "a boolean");
  set_selection_creates_region(state, XEN_TO_C_BOOLEAN_OR_TRUE(val));
  return(C_TO_XEN_BOOLEAN(selection_creates_region(state)));
}

static XEN g_print_length(void) {return(C_TO_XEN_INT(print_length(state)));}
static XEN g_set_print_length(XEN val) 
{
  #define H_print_length "(" S_print_length ") -> number of vector elements to print in the listener (12)"
  XEN_ASSERT_TYPE(XEN_INTEGER_P(val), val, XEN_ONLY_ARG, "set-" S_print_length, "an integer"); 
  set_print_length(state, XEN_TO_C_INT(val)); 
  set_vct_print_length(XEN_TO_C_INT(val));
  return(C_TO_XEN_INT(print_length(state)));
}

static XEN g_previous_files_sort(void) {return(C_TO_XEN_INT(previous_files_sort(state)));}
static XEN g_set_previous_files_sort(XEN val) 
{
  #define H_previous_files_sort "(" S_previous_files_sort ") -> sort choice in view files (0 = unsorted, 1 = by name, etc)"
  XEN_ASSERT_TYPE(XEN_INTEGER_P(val), val, XEN_ONLY_ARG, "set-" S_previous_files_sort, "an integer"); 
  update_prevlist(state);
  set_previous_files_sort(state, mus_iclamp(0,
					    XEN_TO_C_INT(val),
					    5));
  update_prevfiles(state);
  return(C_TO_XEN_INT(previous_files_sort(state)));
}

static XEN g_show_indices(void) {return(C_TO_XEN_BOOLEAN(show_indices(state)));}
static XEN g_set_show_indices(XEN val) 
{
  #define H_show_indices "(" S_show_indices ") -> #t if sound name should be preceded by its index"
  XEN_ASSERT_TYPE(XEN_BOOLEAN_IF_BOUND_P(val), val, XEN_ONLY_ARG, "set-" S_show_indices, "a boolean");
  set_show_indices(state, XEN_TO_C_BOOLEAN_OR_TRUE(val));
  return(C_TO_XEN_BOOLEAN(show_indices(state)));
}

static XEN g_show_backtrace(void) {return(C_TO_XEN_BOOLEAN(show_backtrace(state)));}
static XEN g_set_show_backtrace(XEN val) 
{
  #define H_show_backtrace "(" S_show_backtrace ") -> #t to show backtrace automatically upon error"
  XEN_ASSERT_TYPE(XEN_BOOLEAN_IF_BOUND_P(val), val, XEN_ONLY_ARG, "set-" S_show_backtrace, "a boolean");
  set_show_backtrace(state, XEN_TO_C_BOOLEAN_OR_TRUE(val));
  return(C_TO_XEN_BOOLEAN(show_backtrace(state)));
}

static XEN g_show_usage_stats(void) {return(C_TO_XEN_BOOLEAN(show_usage_stats(state)));}
static XEN g_set_show_usage_stats(XEN on) 
{
  #define H_show_usage_stats "(" S_show_usage_stats ") -> #t if Snd should display memory usage stats"
  XEN_ASSERT_TYPE(XEN_BOOLEAN_IF_BOUND_P(on), on, XEN_ONLY_ARG, "set-" S_show_usage_stats, "a boolean");
  set_show_usage_stats(state, XEN_TO_C_BOOLEAN_OR_TRUE(on));
  return(C_TO_XEN_BOOLEAN(show_usage_stats(state)));
}

static XEN g_update_usage_stats(void) 
{
  #define H_update_usage_stats "(" S_update_usage_stats ") causes the stats display to be made current"
  update_stats(state); 
  return(XEN_TRUE);
}

static XEN g_sinc_width(void) {return(C_TO_XEN_INT(sinc_width(state)));}
static XEN g_set_sinc_width(XEN val) 
{
  #define H_sinc_width "(" S_sinc_width ") -> sampling rate conversion sinc width (10). \
The higher this number, the better the src low-pass filter, but the slower \
src runs.  If you use too low a setting, you can sometimes hear high \
frequency whistles leaking through."

  int len;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(val), val, XEN_ONLY_ARG, "set-" S_sinc_width, "an integer"); 
  len = XEN_TO_C_INT(val);
  if (len >= 0)
    set_sinc_width(state, len);
  return(C_TO_XEN_INT(sinc_width(state)));
}

static XEN g_hankel_jn(void) {return(C_TO_XEN_DOUBLE(state->Hankel_Jn));}
static XEN g_set_hankel_jn(XEN val) 
{
  #define H_hankel_jn "(" S_hankel_jn ") -> Bessel function used in Hankel transform."
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ONLY_ARG, "set-" S_hankel_jn, "a number"); 
  state->Hankel_Jn = XEN_TO_C_DOUBLE_WITH_CALLER(val, S_hankel_jn);
  return(val);
}

static XEN g_color_map(void) {return(C_TO_XEN_INT(color_map(state)));}
static XEN g_set_color_map(XEN val) 
{
  #define H_colormap "(" S_colormap ") -> current col" STR_OR "map choice. \
This should be an integer between -1 and 15.  The maps (from 0 to 15) are: \
gray, hsv, hot, cool, bone, copper, pink, jet, prism, autumn, winter, \
spring, summer, colorcube, flag, and lines.  -1 means black and white."

  XEN_ASSERT_TYPE(XEN_INTEGER_P(val), val, XEN_ONLY_ARG, "set-" S_colormap, "an integer"); 
  set_color_map(state, mus_iclamp(0,
				  XEN_TO_C_INT(val),
				  NUM_COLORMAPS-1));
  return(C_TO_XEN_INT(color_map(state)));
}

static XEN snd_access(char *dir, char *caller)
{
  int err;
  char *temp;
  XEN res;
  temp = shorter_tempnam(dir, "snd_");
  err = mus_file_create(temp);
  if (err == -1)
    {
      FREE(temp);
      temp = mus_format("%s %s is not writable: %s", caller, dir, strerror(errno));
      res = C_TO_XEN_STRING(temp);
      FREE(temp);
      return(res);
    }
  remove(temp);
  FREE(temp);
  return(C_TO_XEN_STRING(dir));
}

static XEN g_temp_dir(void) {return(C_TO_XEN_STRING(temp_dir(state)));}
static XEN g_set_temp_dir(XEN val) 
{
  #define H_temp_dir "(" S_temp_dir ") -> name of directory for temp files"

  XEN_ASSERT_TYPE(XEN_STRING_P(val) || XEN_FALSE_P(val), val, XEN_ONLY_ARG, "set-" S_temp_dir, "a string"); 
  if (temp_dir(state)) free(temp_dir(state));
  if (XEN_FALSE_P(val))
    set_temp_dir(state, snd_strdup(DEFAULT_TEMP_DIR));
  else 
    {
      set_temp_dir(state, XEN_TO_NEW_C_STRING(val));
      return(snd_access(temp_dir(state), S_temp_dir));
    }
  return(C_TO_XEN_STRING(temp_dir(state)));
}

static XEN g_snd_tempnam(void) 
{
  #define H_snd_tempnam "(" S_snd_tempnam ") -> new temp file name using temp-dir"
  char *tmp;
  XEN res;
  tmp = snd_tempnam(get_global_state());
  res = C_TO_XEN_STRING(tmp);
  FREE(tmp);
  return(res);
}

static XEN g_save_dir(void) {return(C_TO_XEN_STRING(save_dir(state)));}
static XEN g_set_save_dir(XEN val) 
{
  #define H_save_dir "(" S_save_dir ") -> name of directory for saved state data"
  XEN_ASSERT_TYPE(XEN_STRING_P(val), val, XEN_ONLY_ARG, "set-" S_save_dir, "a string"); 
  if (save_dir(state)) free(save_dir(state));
  set_save_dir(state, XEN_TO_NEW_C_STRING(val));
  return(snd_access(save_dir(state), S_save_dir));
}

static XEN g_trap_segfault(void) {return(C_TO_XEN_BOOLEAN(trap_segfault(state)));}
static XEN g_set_trap_segfault(XEN val) 
{
  #define H_trap_segfault "(" S_trap_segfault ") -> #t if Snd should try to trap (and whine about) segfaults"
  XEN_ASSERT_TYPE(XEN_BOOLEAN_IF_BOUND_P(val), val, XEN_ONLY_ARG, "set-" S_trap_segfault, "a boolean");
  set_trap_segfault(state, XEN_TO_C_BOOLEAN_OR_TRUE(val));
  return(C_TO_XEN_BOOLEAN(trap_segfault(state)));
}

static XEN g_show_selection_transform(void) {return(C_TO_XEN_BOOLEAN(show_selection_transform(state)));}
static XEN g_set_show_selection_transform(XEN val) 
{
  #define H_show_selection_transform "(" S_show_selection_transform ") -> #t if transform display reflects selection, not time-domain window"
  XEN_ASSERT_TYPE(XEN_BOOLEAN_IF_BOUND_P(val), val, XEN_ONLY_ARG, "set-" S_show_selection_transform, "a boolean");
  set_show_selection_transform(state, XEN_TO_C_BOOLEAN_OR_TRUE(val));
  return(C_TO_XEN_BOOLEAN(show_selection_transform(state)));
}

static XEN g_with_mix_tags(void) {return(C_TO_XEN_BOOLEAN(with_mix_tags(state)));}
static XEN g_set_with_mix_tags(XEN val) 
{
  #define H_with_mix_tags "(" S_with_mix_tags ") -> #t if Snd should editable mixes"
  XEN_ASSERT_TYPE(XEN_BOOLEAN_IF_BOUND_P(val), val, XEN_ONLY_ARG, "set-" S_with_mix_tags, "a boolean");
  set_with_mix_tags(state, XEN_TO_C_BOOLEAN_OR_TRUE(val));
  return(C_TO_XEN_BOOLEAN(with_mix_tags(state)));
}

static XEN g_use_sinc_interp(void) {return(C_TO_XEN_BOOLEAN(use_sinc_interp(state)));}
static XEN g_set_use_sinc_interp(XEN val) 
{
  #define H_use_sinc_interp "(" S_use_sinc_interp ") -> #t if Snd should use convolution with sinc for sampling rate \
conversion.  The other choice is (much faster) linear interpolation which can introduce distortion"

  XEN_ASSERT_TYPE(XEN_BOOLEAN_IF_BOUND_P(val), val, XEN_ONLY_ARG, "set-" S_use_sinc_interp, "a boolean");
  set_use_sinc_interp(state, XEN_TO_C_BOOLEAN_OR_TRUE(val));
  return(C_TO_XEN_BOOLEAN(use_sinc_interp(state)));
}

/* data-clipped -> clip-data? -- this is from sndlib */
static XEN g_data_clipped(void) {return(C_TO_XEN_BOOLEAN(data_clipped(state)));}
static XEN g_set_data_clipped(XEN val) 
{
  #define H_data_clipped "(" S_data_clipped ") -> #t if Snd should clip output values to the current \
output data format's maximum. The default (#f) allows them to wrap-around which makes a very loud click"

  XEN_ASSERT_TYPE(XEN_BOOLEAN_IF_BOUND_P(val), val, XEN_ONLY_ARG, "set-" S_data_clipped, "a boolean");
  set_data_clipped(state, XEN_TO_C_BOOLEAN_OR_TRUE(val));
  return(C_TO_XEN_BOOLEAN(data_clipped(state)));
}

static XEN g_vu_font(void) {return(C_TO_XEN_STRING(vu_font(state)));}
static XEN g_set_vu_font(XEN val) 
{
  #define H_vu_font "(" S_vu_font ") -> name of font used to make VU meter labels (courier)"
  XEN_ASSERT_TYPE(XEN_STRING_P(val) || XEN_FALSE_P(val), val, XEN_ONLY_ARG, "set-" S_vu_font, "a string"); 
  if (vu_font(state)) free(vu_font(state));
  if (XEN_FALSE_P(val))
    set_vu_font(state, DEFAULT_VU_FONT);
  else set_vu_font(state, XEN_TO_NEW_C_STRING(val));
  return(C_TO_XEN_STRING(vu_font(state)));
}

static XEN g_vu_font_size(void) {return(C_TO_XEN_DOUBLE(vu_font_size(state)));}
static XEN g_set_vu_font_size(XEN val) 
{
  #define H_vu_font_size "(" S_vu_font_size ") -> size of VU font meter labels (1.0)"
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ONLY_ARG, "set-" S_vu_font_size, "a number"); 
  set_vu_font_size(state, XEN_TO_C_DOUBLE(val));
  return(C_TO_XEN_DOUBLE(vu_font_size(state)));
}

static XEN g_vu_size(void) {return(C_TO_XEN_DOUBLE(vu_size(state)));}
static XEN g_set_vu_size(XEN val) 
{
  #define H_vu_size "(" S_vu_size ") -> size of VU meters (1.0)"
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ONLY_ARG, "set-" S_vu_size, "a number"); 
  set_vu_size(state, XEN_TO_C_DOUBLE(val));
  return(C_TO_XEN_DOUBLE(vu_size(state)));
}

static XEN g_zoom_focus_style(void) {return(C_TO_XEN_INT(zoom_focus_style(state)));}
static XEN g_set_zoom_focus_style(XEN focus) 
{
  #define H_zoom_focus_style "(" S_zoom_focus_style ") -> one of '(" S_zoom_focus_left " " S_zoom_focus_right " " S_zoom_focus_middle " " S_zoom_focus_active ")\n\
  decides what zooming centers on (default: " S_zoom_focus_active ")"
  XEN_ASSERT_TYPE(XEN_INTEGER_P(focus), focus, XEN_ONLY_ARG, "set-" S_zoom_focus_style, "an integer"); 
  activate_focus_menu(state, mus_iclamp(ZOOM_FOCUS_LEFT,
					XEN_TO_C_INT(focus),
					ZOOM_FOCUS_MIDDLE));
  return(C_TO_XEN_INT(zoom_focus_style(state)));
}

static XEN g_snd_version(void) 
{
  #define H_snd_version "(" S_snd_version ") -> current Snd version"
  return(C_TO_XEN_STRING(SND_VERSION));
}

static XEN g_max_sounds(void) 
{
  #define H_max_sounds "(" S_max_sounds ") -> max sound id currently possible (grows as necessary)"
  return(C_TO_XEN_INT(state->max_sounds));
}

static XEN g_sounds(void)
{
  #define H_sounds "(" S_sounds ") -> list of active sounds (ids)"
  int i;
  snd_state *ss;
  snd_info *sp;
  XEN result;
  ss = get_global_state();
  result = XEN_EMPTY_LIST;
  for (i = 0; i < ss->max_sounds; i++)
    {
      sp = ((snd_info *)(ss->sounds[i]));
      if ((sp) && (sp->inuse))
	result = XEN_CONS(C_TO_SMALL_XEN_INT(i),
			  result);
    }
  return(result);
}

static XEN g_equalize_panes(XEN snd) 
{
  #define H_equalize_panes "(" S_equalize_panes " (&optional snd) causes Snd to try to give all channels about the same screen space"
  snd_info *sp;
  if (XEN_NOT_BOUND_P(snd))
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
  return(XEN_FALSE);
}

static XEN g_show_listener(void) 
{
  #define H_show_listener "(" S_show_listener ") opens the lisp listener pane"
  handle_listener(state, TRUE); 
  return(C_TO_XEN_BOOLEAN(state->listening));
}

static XEN g_set_show_listener(XEN val)
{
  XEN_ASSERT_TYPE(XEN_BOOLEAN_P(val), val, XEN_ONLY_ARG, "set-" S_show_listener, "a boolean");
  handle_listener(state, XEN_TO_C_BOOLEAN(val));
  return(C_TO_XEN_BOOLEAN(state->listening));
}

static XEN g_help_text_font(void) {return(C_TO_XEN_STRING(help_text_font(state)));}
static XEN g_set_help_text_font(XEN val) 
{
  #define H_help_text_font "(" S_help_text_font ") -> font used in the Help dialog"
  XEN_ASSERT_TYPE(XEN_STRING_P(val), val, XEN_ONLY_ARG, "set-" S_help_text_font, "a string"); 
  set_help_text_font(state, XEN_TO_NEW_C_STRING(val)); 
  return(val);
}

static XEN g_tiny_font(void) {return(C_TO_XEN_STRING(tiny_font(state)));}
static XEN g_set_tiny_font(XEN val) 
{
  #define H_tiny_font "(" S_tiny_font ") -> font use for some info in the graphs"
  XEN_ASSERT_TYPE(XEN_STRING_P(val), val, XEN_ONLY_ARG, "set-" S_tiny_font, "a string"); 
  set_tiny_font(state, XEN_TO_NEW_C_STRING(val)); 
  return(val);
}

static XEN g_axis_label_font(void) {return(C_TO_XEN_STRING(axis_label_font(state)));}
static XEN g_set_axis_label_font(XEN val) 
{
  #define H_axis_label_font "(" S_axis_label_font ") -> font used for axis labels"
  XEN_ASSERT_TYPE(XEN_STRING_P(val), val, XEN_ONLY_ARG, "set-" S_axis_label_font, "a string"); 
  set_axis_label_font(state, XEN_TO_NEW_C_STRING(val)); 
  return(val);
}

static XEN g_axis_numbers_font(void) {return(C_TO_XEN_STRING(axis_numbers_font(state)));}
static XEN g_set_axis_numbers_font(XEN val) 
{
  #define H_axis_numbers_font "(" S_axis_numbers_font ") -> font used for axis numbers"
  XEN_ASSERT_TYPE(XEN_STRING_P(val), val, XEN_ONLY_ARG, "set-" S_axis_numbers_font, "a string"); 
  set_axis_numbers_font(state, XEN_TO_NEW_C_STRING(val)); 
  return(val);
}

static XEN g_listener_font(void) {return(C_TO_XEN_STRING(listener_font(state)));}
static XEN g_set_listener_font(XEN val) 
{
  #define H_listener_font "(" S_listener_font ") -> font used by the lisp listener"
  XEN_ASSERT_TYPE(XEN_STRING_P(val), val, XEN_ONLY_ARG, "set-" S_listener_font, "a string");
  set_listener_font(state, XEN_TO_NEW_C_STRING(val)); 
  return(val);
}

static XEN g_bold_button_font(void) {return(C_TO_XEN_STRING(bold_button_font(state)));}
static XEN g_set_bold_button_font(XEN val) 
{
  #define H_bold_button_font "(" S_bold_button_font ") -> font used by some buttons"
  XEN_ASSERT_TYPE(XEN_STRING_P(val), val, XEN_ONLY_ARG, "set-" S_bold_button_font, "a string"); 
  set_bold_button_font(state, XEN_TO_NEW_C_STRING(val)); 
  return(val);
}

static XEN g_button_font(void) {return(C_TO_XEN_STRING(button_font(state)));}
static XEN g_set_button_font(XEN val) 
{
  #define H_button_font "(" S_button_font ") -> font used by some buttons"
  XEN_ASSERT_TYPE(XEN_STRING_P(val), val, XEN_ONLY_ARG, "set-" S_button_font, "a string"); 
  set_button_font(state, XEN_TO_NEW_C_STRING(val)); 
  return(val);
}

static XEN g_window_width(void) 
{
  #define H_window_width "(" S_window_width ") -> current Snd window width in pixels"
  return(C_TO_XEN_INT(widget_width(MAIN_SHELL(state))));
}

static XEN g_window_height(void) 
{
  #define H_window_height "(" S_window_height ") -> current Snd window height in pixels"
  return(C_TO_XEN_INT(widget_height(MAIN_SHELL(state))));
}

static XEN g_window_x(void) 
{
  #define H_window_x "(" S_window_x ") -> current Snd window x position in pixels"
  return(C_TO_XEN_INT(widget_x(MAIN_SHELL(state))));
}

static XEN g_window_y(void) 
{
  #define H_window_y "(" S_window_y ") -> current Snd window y position in pixels"
  return(C_TO_XEN_INT(widget_y(MAIN_SHELL(state))));
}

static XEN g_set_window_height(XEN height) 
{
  #define H_set_window_height "(" "set-" S_window_height " val) sets the Snd window height in pixels"
  Latus val;
  XEN_ASSERT_TYPE(XEN_NUMBER_P(height), height, XEN_ONLY_ARG, "set-" S_window_height, "a number"); 
  set_widget_height(MAIN_SHELL(state), val = (Latus)XEN_TO_C_INT_OR_ELSE(height, 0));
  state->init_window_height = val;
  return(height);
}

static XEN g_set_window_width(XEN width) 
{
  #define H_set_window_width "(" "set-" S_window_width " val) sets the Snd window width in pixels"
  Latus val;
  XEN_ASSERT_TYPE(XEN_NUMBER_P(width), width, XEN_ONLY_ARG, "set-" S_window_width, "a number"); 
  set_widget_width(MAIN_SHELL(state), val = (Latus)XEN_TO_C_INT_OR_ELSE(width, 0)); 
  state->init_window_width = val;
  return(width);
}

static XEN g_set_window_x(XEN val) 
{
  #define H_set_window_x "(" "set-" S_window_x " val) sets the Snd window x position in pixels"
  Locus x;
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ONLY_ARG, "set-" S_window_x, "a number"); 
  set_widget_x(MAIN_SHELL(state), x = (Locus)XEN_TO_C_INT_OR_ELSE(val, 0));
  state->init_window_x = x;
  return(val);
}

static XEN g_set_window_y(XEN val) 
{
  #define H_set_window_y "(" "set-" S_window_y " val) sets the Snd window y position in pixels"
  Locus y;
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ONLY_ARG, "set-" S_window_y, "a number"); 
  set_widget_y(MAIN_SHELL(state), y = (Locus)XEN_TO_C_INT_OR_ELSE(val, 0)); 
  state->init_window_y = y;
  return(val);
}

static XEN g_abort(void)
{
  #define H_abort "(" S_abort ") exits Snd via \"abort\", presumably to land in the debugger"
  abort();
  return(XEN_FALSE);
}

static XEN g_dismiss_all_dialogs(void)
{
  #define H_dismiss_all_dialogs "(" S_dismiss_all_dialogs ") closes all active dialogs"
  dismiss_all_dialogs(state);
  return(XEN_FALSE);
}

static XEN g_abortq(void)
{
  #define H_abortQ "(" S_c_g ") allows pending user interface events to occur, returning #t if C-g was typed"
  check_for_event(state);
  if (state->stopped_explicitly)
    {
      state->stopped_explicitly = 0;
      return(XEN_TRUE);
    }
  return(XEN_FALSE);
}

snd_info *get_sp(XEN x_snd_n)
{
  int snd_n, len;
  /* if x_snd_n is a number, it is sp->index
     if it's a (non-empty) list, car is mix id (perhaps someday treat list as track if more than one member)
  */
  if (XEN_INTEGER_P(x_snd_n))
    {
      snd_n = XEN_TO_C_INT(x_snd_n);
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
      if (XEN_LIST_P_WITH_LENGTH(x_snd_n, len))
	{
	  /* a mix input sound */
	  /* make sure it's not the null list */
	  if (len > 0)
	    return(make_mix_readable_from_id(XEN_TO_C_INT_OR_ELSE(XEN_CAR(x_snd_n), 0)));
	  return(NULL);
	}
    }
  /* use default sound, if any */
  return(any_selected_sound(state));
}

chan_info *get_cp(XEN x_snd_n, XEN x_chn_n, const char *caller)
{
  snd_info *sp;
  int chn_n;
  sp = get_sp(x_snd_n);
  if ((sp == NULL) || (sp->active != 1))
    snd_no_such_sound_error(caller, x_snd_n); 
  if (XEN_INTEGER_P(x_chn_n))
    chn_n = XEN_TO_C_INT(x_chn_n);
  else
    if (sp->selected_channel != NO_SELECTION) 
      chn_n = sp->selected_channel;
    else chn_n = 0;
  if ((chn_n >= 0) && (chn_n < sp->nchans)) 
    return(sp->chans[chn_n]);
  snd_no_such_channel_error(caller, x_snd_n, x_chn_n);
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

static XEN g_open_sound_file(XEN g_name, XEN g_chans, XEN g_srate, XEN g_comment)
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
  XEN_ASSERT_TYPE(XEN_STRING_IF_BOUND_P(g_name), g_name, XEN_ARG_1, S_open_sound_file, "a string");
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(g_chans), g_chans, XEN_ARG_2, S_open_sound_file, "an integer");
  XEN_ASSERT_TYPE(XEN_NUMBER_IF_BOUND_P(g_srate), g_srate, XEN_ARG_3, S_open_sound_file, "a number");
  XEN_ASSERT_TYPE(XEN_STRING_IF_BOUND_P(g_comment), g_comment, XEN_ARG_4, S_open_sound_file, "a string");
  if (XEN_STRING_P(g_comment)) comment = XEN_TO_C_STRING(g_comment);
  chans = XEN_TO_C_INT_OR_ELSE(g_chans, 0); 
  srate = XEN_TO_C_INT_OR_ELSE(g_srate, 0); 
  if (XEN_STRING_P(g_name)) name = XEN_TO_C_STRING(g_name); 
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
	XEN_ERROR(MUS_MISC_ERROR,
		  XEN_LIST_2(C_TO_XEN_STRING(S_open_sound_file),
			     C_TO_XEN_STRING(ss->catch_message)));
      else
	return(snd_no_such_file_error(S_open_sound_file, g_name));
    }
  set_temp_fd(result, hdr);
  return(C_TO_XEN_INT(result));
}

static XEN g_close_sound_file(XEN g_fd, XEN g_bytes)
{
  #define H_close_sound_file "(" S_close_sound_file " fd bytes) closes file pointed to by fd, updating its header to report 'bytes' bytes of data"
  file_info *hdr;
  int result, fd, bytes;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(g_fd), g_fd, XEN_ARG_1, S_close_sound_file, "an integer");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(g_bytes), g_bytes, XEN_ARG_2, S_close_sound_file, "a number");
  fd = XEN_TO_C_INT(g_fd);
  if ((fd < 0) || (fd == fileno(stdin)) || (fd == fileno(stdout)) || (fd == fileno(stderr)))
    mus_misc_error(S_close_sound_file, "invalid file", g_fd);
  bytes = XEN_TO_C_INT_OR_ELSE(g_bytes, 0);
  hdr = get_temp_header(fd);
  if (hdr == NULL) 
    {
      close(fd);
      return(snd_no_such_file_error(S_close_sound_file, g_fd));
    }
  result = close_temp_file(fd, hdr, bytes, any_selected_sound(state));
  unset_temp_fd(fd);
  free_file_info(hdr);
  return(C_TO_XEN_INT(result));
}

static XEN samples2vct(XEN samp_0, XEN samps, XEN snd_n, XEN chn_n, XEN v, XEN edpos)
{
  #define H_samples2vct "(" S_samples2vct " &optional (start-samp 0)\n    samps snd chn vct-obj edit-position)\n\
returns a vct object (vct-obj if given) containing snd channel chn's data starting at start-samp for samps, \
reading edit version edit-position (defaulting to the current version)"

  chan_info *cp;
  snd_fd *sf;
  Float *fvals;
  int i, len, beg, pos;
  vct *v1 = get_vct(v);
  XEN_ASSERT_TYPE(XEN_NUMBER_IF_BOUND_P(samp_0), samp_0, XEN_ARG_1, S_samples2vct, "a number");
  XEN_ASSERT_TYPE(XEN_NUMBER_IF_BOUND_P(samps), samps, XEN_ARG_2, S_samples2vct, "a number");
  ASSERT_CHANNEL(S_samples2vct, snd_n, chn_n, 3);
  cp = get_cp(snd_n, chn_n, S_samples2vct);
  pos = to_c_edit_position(cp, edpos, S_samples2vct, 6);
  beg = XEN_TO_C_INT_OR_ELSE(samp_0, 0);
  len = XEN_TO_C_INT_OR_ELSE(samps, cp->samples[pos] - beg);
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

static XEN samples2sound_data(XEN samp_0, XEN samps, XEN snd_n, XEN chn_n, XEN sdobj, XEN edpos, XEN sdchan)
{
  #define H_samples2sound_data "(" S_samples2sound_data " &optional (start-samp 0)\n    samps snd chn sdobj edit-position (sdobj-chan 0))\n\
returns a sound-data object (sdobj if given) containing snd channel chn's data starting at start-samp for samps, \
reading edit version edit-position (defaulting to the current version)"

  chan_info *cp;
  snd_fd *sf;
  sound_data *sd;
  XEN newsd = XEN_FALSE;
  int i, len, beg, chn = 0, pos;
  XEN_ASSERT_TYPE(XEN_NUMBER_IF_BOUND_P(samp_0), samp_0, XEN_ARG_1, S_samples2sound_data, "a number");
  XEN_ASSERT_TYPE(XEN_NUMBER_IF_BOUND_P(samps), samps, XEN_ARG_2, S_samples2sound_data, "a number");
  ASSERT_CHANNEL(S_samples2sound_data, snd_n, chn_n, 3);
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(sdchan), sdchan, XEN_ARG_7, S_samples2sound_data, "an integer");
  cp = get_cp(snd_n, chn_n, S_samples2sound_data);
  pos = to_c_edit_position(cp, edpos, S_samples2sound_data, 6);
  beg = XEN_TO_C_INT_OR_ELSE(samp_0, 0);
  len = XEN_TO_C_INT_OR_ELSE(samps, cp->samples[pos] - beg);
  if (len > 0)
    {
      chn = XEN_TO_C_INT_OR_ELSE(sdchan, 0);
      if (sound_data_p(sdobj))
	sd = (sound_data *)XEN_OBJECT_REF(sdobj);
      else
	{
	  newsd = make_sound_data(chn + 1, len);
	  sd = (sound_data *)XEN_OBJECT_REF(newsd);
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
  if (XEN_NOT_FALSE_P(newsd))
    return(newsd);
  return(sdobj);
}

static XEN vct2soundfile(XEN g_fd, XEN obj, XEN g_nums)
{
  #define H_vct2sound_file "(" S_vct2sound_file " fd vct-obj samps) writes samps samples from vct-obj to the sound file controlled by fd"
  int fd, nums, i;
  float *vals;
  vct *v;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(g_fd), g_fd, XEN_ARG_1, S_vct2sound_file, "an integer");
  XEN_ASSERT_TYPE((VCT_P(obj)), obj, XEN_ARG_2, S_vct2sound_file, "a vct");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(g_nums), g_nums, XEN_ARG_3, S_vct2sound_file, "a number");
  fd = XEN_TO_C_INT(g_fd);
  if ((fd < 0) || (fd == fileno(stdin)) || (fd == fileno(stdout)) || (fd == fileno(stderr)))
    mus_misc_error(S_vct2sound_file, "invalid file", g_fd);
  nums = XEN_TO_C_INT_OR_ELSE(g_nums, 0);
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
  return(xen_return_first(C_TO_SMALL_XEN_INT(nums>>2), obj));
}


Float string2Float(char *str) 
{
  XEN res;
  res = snd_catch_any(eval_str_wrapper, (void *)str, "string->float");
  if (XEN_NUMBER_P(res))
    return(XEN_TO_C_DOUBLE(res));
  else snd_error("%s is not a number", str);
  return(0.0);
}

int string2int(char *str) 
{
  XEN res;
  res = snd_catch_any(eval_str_wrapper, (void *)str, "string->int");
  if (XEN_NUMBER_P(res))
    return(XEN_TO_C_INT_OR_ELSE(res, 0));
  else snd_error("%s is not a number", str);
  return(0);
}

#if 0
char *string2string(char *str) 
{
  XEN res;
  res = snd_catch_any(eval_str_wrapper, (void *)str, "string->string");
  if (XEN_STRING_P(res))
    return(XEN_TO_NEW_C_STRING(res));
  else snd_error("%s is not a string", str);
  return(str);
}
#endif

static XEN g_help_dialog(XEN subject, XEN msg)
{
  #define H_help_dialog "(" S_help_dialog " subject message) fires up the Help window with subject and message"
  XEN_ASSERT_TYPE(XEN_STRING_P(subject), subject, XEN_ARG_1, S_help_dialog, "a string");
  XEN_ASSERT_TYPE(XEN_STRING_P(msg), msg, XEN_ARG_2, S_help_dialog, "a string");
  return(XEN_WRAP_C_POINTER(snd_help(state, XEN_TO_C_STRING(subject), XEN_TO_C_STRING(msg))));
}

static XEN g_mix_panel(void)
{
  #define H_mix_panel "(" S_mix_panel ") starts (and returns) the mix panel"
  return(XEN_WRAP_C_POINTER(make_mix_panel(get_global_state())));
}

static XEN g_color_dialog(void) 
{
  #define H_color_dialog "(" S_color_dialog ") fires up the Color dialog"
  return(XEN_WRAP_C_POINTER(start_color_dialog(state, 0, 0))); 
}

static XEN g_orientation_dialog(void) 
{
  #define H_orientation_dialog "(" S_orientation_dialog ") fires up the Orientation dialog"
  return(XEN_WRAP_C_POINTER(start_orientation_dialog(state, 0, 0))); 
}

static XEN g_transform_dialog(void) 
{
  #define H_transform_dialog "(" S_transform_dialog ") fires up the Transforms dialog"
  return(XEN_WRAP_C_POINTER(fire_up_transform_dialog(state))); 
}

static XEN g_file_dialog(void) 
{
  #define H_file_dialog "(" S_file_dialog ") fires up the File dialog"
  return(XEN_WRAP_C_POINTER(start_file_dialog(state, 0, 0)));
}

static XEN g_edit_header_dialog(XEN snd_n) 
{
  #define H_edit_header_dialog "(" S_edit_header_dialog " snd) opens the Edit Header dialog on sound snd"
  snd_info *sp; 
  sp = get_sp(snd_n);
  if (sp == NULL)
    return(snd_no_such_sound_error(S_edit_header_dialog, snd_n));
  return(XEN_WRAP_C_POINTER(edit_header(sp))); 
}

static XEN g_edit_save_as_dialog(void) 
{
  #define H_edit_save_as_dialog "(" S_edit_save_as_dialog ") opens the Selection Save-as dialog"
  make_edit_save_as_dialog(get_global_state()); 
  return(XEN_FALSE);
}

static XEN g_yes_or_no_p(XEN msg) 
{
  #define H_yes_or_no_p "(" S_yes_or_no_p " message) displays message and waits for 'y' or 'n'; returns #t if 'y'"
  XEN_ASSERT_TYPE(XEN_STRING_P(msg), msg, XEN_ONLY_ARG, S_yes_or_no_p, "a string");
  return(C_TO_XEN_BOOLEAN(snd_yes_or_no_p(state, XEN_TO_C_STRING(msg))));
}

static XEN g_graph(XEN ldata, XEN xlabel, XEN x0, XEN x1, XEN y0, XEN y1, XEN snd_n, XEN chn_n, XEN force_display)
{
  /* TODO: need a way to create the lisp-graph axis-info and draw there even if no call on graph
   */

  #define H_graph "(" S_graph " data &optional xlabel x0 x1 y0 y1 snd chn force-display)\n\
displays 'data' as a graph with x axis label 'xlabel', axis units going from x0 to x1 and y0 to y1; 'data' can be a list, vct, or vector. \
If 'data' is a list of numbers, it is treated as an envelope."

  chan_info *cp;
  lisp_grf *lg;
  XEN data = XEN_UNDEFINED; XEN lst;
  char *label = NULL;
  vct *v = NULL;
  XEN *vdata;
  int i, len, graph, graphs, need_update = 0;
  Float ymin, ymax, val, nominal_x0, nominal_x1;
  lisp_grf *old_lp = NULL;
  Latus h = 0, w = 0, ww = 0;
  Locus o = 0, gx0 = 0;
  axis_info *uap = NULL;
  /* ldata can be a vct object, a vector, or a list of either */
  XEN_ASSERT_TYPE(((VCT_P(ldata)) || (XEN_VECTOR_P(ldata)) || (XEN_LIST_P(ldata))), ldata, XEN_ARG_1, S_graph, "a vct, vector, or list");
  ASSERT_CHANNEL(S_graph, snd_n, chn_n, 7);
  cp = get_cp(snd_n, chn_n, S_graph);
  ymin = 32768.0;
  ymax = -32768.0;
  if ((cp->sound_ctr == -1) || 
      (cp->sounds == NULL) || 
      (cp->sounds[cp->sound_ctr] == NULL) ||
      (cp->axis == NULL))
    return(XEN_FALSE);
  if (XEN_STRING_P(xlabel)) label = XEN_TO_C_STRING(xlabel); 
  if (XEN_NUMBER_P(x0)) nominal_x0 = XEN_TO_C_DOUBLE(x0); else nominal_x0 = 0.0;
  if (XEN_NUMBER_P(x1)) nominal_x1 = XEN_TO_C_DOUBLE(x1); else nominal_x1 = 1.0;
  if (XEN_NUMBER_P(y0)) ymin = XEN_TO_C_DOUBLE(y0);
  if (XEN_NUMBER_P(y1)) ymax = XEN_TO_C_DOUBLE(y1);
  if ((!(XEN_LIST_P(ldata))) || 
      (XEN_NUMBER_P(XEN_CAR(ldata))))
    graphs = 1; 
  else graphs = XEN_LIST_LENGTH(ldata);
  if (graphs == 0) return(XEN_FALSE);
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
  if ((XEN_LIST_P_WITH_LENGTH(ldata, len)) && 
      (XEN_NUMBER_P(XEN_CAR(ldata))))
    {
      lg = cp->lisp_info;
      lg->env_data = 1;
      if (lg->len[0] != len)
	{
	  if (lg->data[0]) FREE(lg->data[0]);
	  lg->data[0] = (Float *)CALLOC(len, sizeof(Float));
	  lg->len[0] = len;
	}
      for (i = 0, lst = ldata; i < len; i++, lst = XEN_CDR(lst))
	lg->data[0][i] = XEN_TO_C_DOUBLE(XEN_CAR(lst));
      if ((!XEN_NUMBER_P(y0)) || 
	  (!XEN_NUMBER_P(y1)))
	{
	  for (i = 1; i < len; i += 2)
	    {
	      val = lg->data[0][i];
	      if (ymin > val) ymin = val;
	      if (ymax < val) ymax = val;
	    }
	}
      if (!XEN_NUMBER_P(x0)) nominal_x0 = lg->data[0][0];
      if (!XEN_NUMBER_P(x1)) nominal_x1 = lg->data[0][len - 2];
    }
  else
    {
      lg = cp->lisp_info;
      lg->env_data = 0;
      for (graph = 0; graph < graphs; graph++)
	{
	  if (XEN_LIST_P(ldata))
	    data = XEN_LIST_REF(ldata, graph);
	  else data = ldata;
	  if (VCT_P(data))
	    {
	      v = (vct *)XEN_OBJECT_REF(data);
	      len = v->length;
	    }
	  else len = XEN_VECTOR_LENGTH(data);
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
	      vdata = XEN_VECTOR_ELEMENTS(data);
	      for (i = 0; i < len; i++) 
		lg->data[graph][i] = XEN_TO_C_DOUBLE(vdata[i]);
	    }
	  if ((!XEN_NUMBER_P(y0)) || 
	      (!XEN_NUMBER_P(y1)))
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
  if ((XEN_NOT_BOUND_P(force_display)) || 
      (XEN_NOT_FALSE_P(force_display)))
    {
      if (need_update)
	update_graph(cp, NULL);
      else display_channel_lisp_data(cp, cp->sound, cp->state);
    }
  return(xen_return_first(XEN_FALSE, data));
}


static XEN g_clear_audio_inputs (void) 
{
  #define H_clear_audio_inputs "(" S_clear_audio_inputs ") tries to reduce soundcard noise in Linux/OSS"
#if HAVE_OSS
  mus_audio_clear_soundcard_inputs(); 
#endif
  return(XEN_FALSE);
}

static XEN g_set_oss_buffers(XEN num, XEN size)
{
  #define H_set_oss_buffers "(" "set-" S_oss_buffers " num size) sets Linux OSS 'fragment' number and size"
#if (HAVE_OSS || HAVE_ALSA)
  XEN_ASSERT_TYPE(XEN_INTEGER_P(num), num, XEN_ARG_1, "set-" S_oss_buffers, "an integer");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(size), size, XEN_ARG_2, "set-" S_oss_buffers, "an integer");
  mus_audio_set_oss_buffers(XEN_TO_C_INT(num),
			    XEN_TO_C_INT(size));
#endif
  return(XEN_FALSE);
}

#define S_mus_audio_describe            "mus-audio-describe"
static XEN g_mus_audio_describe(void) 
{
  #define H_mus_audio_describe "("  S_mus_audio_describe ") posts a description of the audio hardware state in the Help dialog"
  snd_help(state, "Audio State", mus_audio_report()); 
  return(XEN_TRUE);
}


static XEN g_start_progress_report(XEN snd)
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

static XEN g_finish_progress_report(XEN snd)
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

static XEN g_progress_report(XEN pct, XEN name, XEN cur_chan, XEN chans, XEN snd)
{
  #define H_progress_report "(" S_progress_report " pct &optional name cur-chan chans snd)\n\
updates an on-going 'progress report' (e. g. an animated hour-glass icon) in snd using pct to indicate how far along we are"

  snd_info *sp;
  XEN_ASSERT_TYPE(XEN_NUMBER_P(pct), pct, XEN_ARG_1, S_progress_report, "a number");
  ASSERT_SOUND(S_progress_report, snd, 5);
  XEN_ASSERT_TYPE(XEN_STRING_IF_BOUND_P(name), name, XEN_ARG_2, S_progress_report, "a string");
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(cur_chan), cur_chan, XEN_ARG_3, S_progress_report, "an integer");
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(chans), chans, XEN_ARG_4, S_progress_report, "an integer");
  sp = get_sp(snd);
  if (sp == NULL)
    return(snd_no_such_sound_error(S_progress_report, snd));
  progress_report(sp,
		  (XEN_STRING_P(name)) ? XEN_TO_C_STRING(name) : "something useful",
		  XEN_TO_C_INT_OR_ELSE(cur_chan, 0),
		  XEN_TO_C_INT_OR_ELSE(chans, sp->nchans),
		  XEN_TO_C_DOUBLE(pct),
		  NOT_FROM_ENVED);
  return(snd);
}


#if (!USE_NO_GUI)

#if HAVE_HTML
static XEN g_html_dir(void) 
{
  #define H_html_dir "(" S_html_dir ") -> location of Snd documentation"
  return(C_TO_XEN_STRING(html_dir(state)));
}

static XEN g_set_html_dir(XEN val) 
{
  XEN_ASSERT_TYPE(XEN_STRING_P(val), val, XEN_ONLY_ARG, "set-" S_html_dir, "a string");
  set_html_dir(state, XEN_TO_NEW_C_STRING(val)); 
  return(val);
}
#endif


/* -------- shared color funcs -------- */

Float check_color_range(const char *caller, XEN val)
{
  Float rf;
  rf = XEN_TO_C_DOUBLE(val);
  if ((rf > 1.0) || (rf < 0.0))
    XEN_ERROR(C_STRING_TO_XEN_SYMBOL("out-of-range"), 
	      XEN_LIST_3(C_TO_XEN_STRING(caller),
			 C_TO_XEN_STRING("value must be between 0.0 and 1.0: ~S"),
			 XEN_LIST_1(C_TO_XEN_DOUBLE(rf))));
  return(rf);
}

static XEN g_set_cursor_color (XEN color) 
{
  snd_color *v; 
  XEN_ASSERT_TYPE(COLOR_P(color), color, XEN_ONLY_ARG, "set-" S_cursor_color, "a color object"); 
  v = TO_SND_COLOR(color); 
  if (v) 
    {
      color_cursor(state, v->color);
      map_over_chans(state, update_graph, NULL);
    }
  return(color);
}

static XEN g_cursor_color(void) 
{
  #define H_cursor_color "(" S_cursor_color ") -> cursor col" STR_OR
  return(pixel2color((state->sgx)->cursor_color));
}

static XEN g_set_highlight_color (XEN color) 
{
  snd_color *v; 
  XEN_ASSERT_TYPE(COLOR_P(color), color, XEN_ONLY_ARG, "set-" S_highlight_color, "a color object"); 
  v = TO_SND_COLOR(color); 
  if (v) (state->sgx)->highlight_color = v->color; 
  return(color);
}

static XEN g_highlight_color(void) 
{
  #define H_highlight_color "(" S_highlight_color ") -> col" STR_OR " of highlighted text or buttons"
  return(pixel2color((state->sgx)->highlight_color));
}

static XEN g_set_mark_color (XEN color) 
{
  snd_color *v; 
  XEN_ASSERT_TYPE(COLOR_P(color), color, XEN_ONLY_ARG, "set-" S_mark_color, "a color object"); 
  v = TO_SND_COLOR(color); 
  if (v) 
    {
      color_marks(state, v->color);
      map_over_chans(state, update_graph, NULL);
    }
  return(color);
}

static XEN g_mark_color(void) 
{
  #define H_mark_color "(" S_mark_color ") -> mark col" STR_OR
  return(pixel2color((state->sgx)->mark_color));
}

static XEN g_set_zoom_color (XEN color) 
{
  snd_color *v; 
  XEN_ASSERT_TYPE(COLOR_P(color), color, XEN_ONLY_ARG, "set-" S_zoom_color, "a color object"); 
  v = TO_SND_COLOR(color); 
  if (v) 
    {
      (state->sgx)->zoom_color = v->color; 
      color_chan_components(v->color, COLOR_ZOOM);
    }
  return(color);
}

static XEN g_zoom_color(void) 
{
  #define H_zoom_color "(" S_zoom_color ") -> col" STR_OR " of zoom sliders"
  return(pixel2color((state->sgx)->zoom_color));
}

static XEN g_set_position_color (XEN color) 
{
  snd_color *v; 
  XEN_ASSERT_TYPE(COLOR_P(color), color, XEN_ONLY_ARG, "set-" S_position_color, "a color object"); 
  v = TO_SND_COLOR(color); 
  if (v) 
    {
      (state->sgx)->position_color = v->color; 
      color_chan_components(v->color, COLOR_POSITION);
    }
  return(color);
}

static XEN g_position_color(void) 
{
  #define H_position_color "(" S_position_color ") -> col" STR_OR " of position sliders"
  return(pixel2color((state->sgx)->position_color));
}

static XEN g_set_listener_color (XEN color) 
{
  snd_color *v; 
  XEN_ASSERT_TYPE(COLOR_P(color), color, XEN_ONLY_ARG, "set-" S_listener_color, "a color object"); 
  v = TO_SND_COLOR(color);
  if (v) color_listener(v->color);
  return(color);
}

static XEN g_listener_color(void) 
{
  #define H_listener_color "(" S_listener_color ") -> background col" STR_OR " of the lisp listener"
  return(pixel2color((state->sgx)->listener_color));
}

static XEN g_set_listener_text_color (XEN color) 
{
  snd_color *v; 
  XEN_ASSERT_TYPE(COLOR_P(color), color, XEN_ONLY_ARG, "set-" S_listener_text_color, "a color object"); 
  v = TO_SND_COLOR(color);
  if (v) color_listener_text(v->color);
  return(color);
}

static XEN g_listener_text_color(void) 
{
  #define H_listener_text_color "(" S_listener_text_color ") -> text col" STR_OR " in the lisp listener"
  return(pixel2color((state->sgx)->listener_text_color));
}

static XEN g_set_enved_waveform_color (XEN color) 
{
  snd_color *v;
  XEN_ASSERT_TYPE(COLOR_P(color), color, XEN_ONLY_ARG, "set-" S_enved_waveform_color, "a color object"); 
  v = TO_SND_COLOR(color); 
  if (v) color_enved_waveform(v->color);
  return(color);
}

static XEN g_enved_waveform_color(void) 
{
  #define H_enved_waveform_color "(" S_enved_waveform_color ") -> col" STR_OR " of the envelope editor wave display"
  return(pixel2color((state->sgx)->enved_waveform_color));
}

static XEN g_set_filter_waveform_color (XEN color) 
{
  snd_color *v; 
  XEN_ASSERT_TYPE(COLOR_P(color), color, XEN_ONLY_ARG, "set-" S_filter_waveform_color, "a color object");
  v = TO_SND_COLOR(color);
  if (v) color_filter_waveform(state, v->color);
  return(color);
}

static XEN g_filter_waveform_color(void) 
{
  #define H_filter_waveform_color "(" S_filter_waveform_color ") -> col" STR_OR " of the filter waveform"
  return(pixel2color((state->sgx)->filter_waveform_color));
}

static XEN g_set_selection_color (XEN color) 
{
  snd_color *v; 
  XEN_ASSERT_TYPE(COLOR_P(color), color, XEN_ONLY_ARG, "set-" S_selection_color, "a color object"); 
  v = TO_SND_COLOR(color); 
  if (v) 
    {
      color_selection(state, v->color);
      map_over_chans(state, update_graph, NULL);
    }
  return(color);
}

static XEN g_selection_color(void) 
{
  #define H_selection_color "(" S_selection_color ") -> selection col" STR_OR
  return(pixel2color((state->sgx)->selection_color));
}

static XEN g_set_mix_color (XEN arg1, XEN arg2)
{
  snd_color *v;
  XEN color; XEN mix_id = XEN_UNDEFINED;
  if (XEN_NOT_BOUND_P(arg2))
    color = arg1;
  else
    {
      color = arg2;
      mix_id = arg1;
    }
  XEN_ASSERT_TYPE(COLOR_P(color), color, XEN_ONLY_ARG, "set-" S_mix_color, "a color object"); 
  v = TO_SND_COLOR(color); 
  if (v) 
    {
      if (XEN_INTEGER_P(mix_id))
	color_one_mix_from_id(XEN_TO_SMALL_C_INT(mix_id), v->color);
      else set_mix_color(state, v->color);
      map_over_chans(state, update_graph, NULL);
    }
  return(color);
}

static XEN g_mix_color(XEN mix_id) 
{
  #define H_mix_color "(" S_mix_color ") -> col" STR_OR " of mix consoles"
  if (XEN_INTEGER_P(mix_id))
    return(pixel2color(mix_to_color_from_id(XEN_TO_SMALL_C_INT(mix_id))));
  return(pixel2color((state->sgx)->mix_color));
}

static XEN g_set_selected_mix_color (XEN color) 
{
  snd_color *v; 
  XEN_ASSERT_TYPE(COLOR_P(color), color, XEN_ONLY_ARG, "set-" S_selected_mix_color, "a color object"); 
  v = TO_SND_COLOR(color); 
  if (v) 
    {
      set_selected_mix_color(state, v->color);
      map_over_chans(state, update_graph, NULL);
    }
  return(color);
}

static XEN g_selected_mix_color(void) 
{
  #define H_selected_mix_color "(" S_selected_mix_color ") -> col" STR_OR " of the currently selected mix"
  return(pixel2color((state->sgx)->selected_mix_color));
}

static XEN g_set_text_focus_color (XEN color) 
{
  snd_color *v; 
  XEN_ASSERT_TYPE(COLOR_P(color), color, XEN_ONLY_ARG, "set-" S_text_focus_color, "a color object"); 
  v = TO_SND_COLOR(color); 
  if (v) (state->sgx)->text_focus_color = v->color;
  return(color);
}

static XEN g_text_focus_color(void) 
{
  #define H_text_focus_color "(" S_text_focus_color ") -> col" STR_OR " used to show a text field has focus"
  return(pixel2color((state->sgx)->text_focus_color));
}

static XEN g_set_sash_color (XEN color) 
{
  snd_color *v; 
  XEN_ASSERT_TYPE(COLOR_P(color), color, XEN_ONLY_ARG, "set-" S_sash_color, "a color object"); 
  v = TO_SND_COLOR(color); 
  if (v) (state->sgx)->sash_color = v->color;
  return(color);
}

static XEN g_sash_color(void) 
{
  #define H_sash_color "(" S_sash_color ") -> col" STR_OR " used to draw paned window sashes"
  return(pixel2color((state->sgx)->sash_color));
}

static XEN g_set_data_color (XEN color) 
{
  snd_color *v; 
  XEN_ASSERT_TYPE(COLOR_P(color), color, XEN_ONLY_ARG, "set-" S_data_color, "a color object"); 
  v = TO_SND_COLOR(color); 
  if (v) 
    {
      color_data(state, v->color);
      map_over_chans(state, update_graph, NULL);
    }
  return(color);
}

static XEN g_data_color(void) 
{
  #define H_data_color "(" S_data_color ") -> col" STR_OR " used to draw unselected data"
  return(pixel2color((state->sgx)->data_color));
}

static XEN g_set_selected_data_color (XEN color)
{
  snd_color *v; 
  chan_info *cp;
  XEN_ASSERT_TYPE(COLOR_P(color), color, XEN_ONLY_ARG, "set-" S_selected_data_color, "a color object"); 
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

static XEN g_selected_data_color(void) 
{
  #define H_selected_data_color "(" S_selected_data_color ") -> col" STR_OR " used for selected data"
  return(pixel2color((state->sgx)->selected_data_color));
}

static XEN g_set_graph_color (XEN color) 
{
  snd_color *v; 
  XEN_ASSERT_TYPE(COLOR_P(color), color, XEN_ONLY_ARG, "set-" S_graph_color, "a color object");
  v = TO_SND_COLOR(color);
  if (v) 
    {
      color_graph(state, v->color);
      color_unselected_graphs(v->color);
    }
  return(color);
}

static XEN g_graph_color(void) 
{
  #define H_graph_color "(" S_graph_color ") -> background col" STR_OR " used for unselected data"
  return(pixel2color((state->sgx)->graph_color));
}

static XEN g_set_selected_graph_color (XEN color) 
{
  snd_color *v; 
  chan_info *cp;
  XEN_ASSERT_TYPE(COLOR_P(color), color, XEN_ONLY_ARG, "set-" S_selected_graph_color, "a color object");
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

static XEN g_selected_graph_color(void) 
{
  #define H_selected_graph_color "(" S_selected_graph_color ") -> background col" STR_OR " of selected data"
  return(pixel2color((state->sgx)->selected_graph_color));
}

static XEN g_set_pushed_button_color (XEN color) 
{
  snd_color *v; 
  XEN_ASSERT_TYPE(COLOR_P(color), color, XEN_ONLY_ARG, "set-" S_pushed_button_color, "a color object"); 
  v = TO_SND_COLOR(color); 
  if (v) 
    {
      (state->sgx)->pushed_button_color = v->color;
      map_over_children(MAIN_SHELL(state), recolor_button, NULL);
    }
  return(color);
}

static XEN g_pushed_button_color(void) 
{
  #define H_pushed_button_color "(" S_pushed_button_color ") -> col" STR_OR " of a pushed button"
  return(pixel2color((state->sgx)->pushed_button_color));
}

static XEN g_basic_color(void) 
{
  #define H_basic_color "(" S_basic_color ") -> Snd's basic col" STR_OR
  return(pixel2color((state->sgx)->basic_color));
}

static XEN g_set_basic_color (XEN color) 
{
  snd_color *v; 
  COLOR_TYPE old_color;
  XEN_ASSERT_TYPE(COLOR_P(color), color, XEN_ONLY_ARG, "set-" S_basic_color, "a color object"); 
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


static XEN during_open_hook;
static XEN after_open_hook;
static XEN output_comment_hook;

XEN g_c_run_progn_hook (XEN hook, XEN args, const char *caller)
{
#if HAVE_GUILE
  /* Guile built-in scm_c_run_hook doesn't return the value of the hook procedure(s) and exits on error */
  XEN result = XEN_FALSE;
  XEN procs = XEN_HOOK_PROCEDURES (hook);
  while (XEN_NOT_NULL_P (procs))
    {
      if (!(XEN_EQ_P(args, XEN_EMPTY_LIST)))
	result = XEN_APPLY(XEN_CAR(procs), args, caller);
      else result = XEN_CALL_0(XEN_CAR(procs), caller);
      procs = XEN_CDR (procs);
    }
  return(xen_return_first(result, args));
#else
  if (!(XEN_EQ_P(args, XEN_EMPTY_LIST)))
    return(XEN_APPLY(XEN_VARIABLE_REF(hook), args, caller));
  else return(XEN_CALL_0(XEN_VARIABLE_REF(hook), caller));
#endif
}

XEN g_c_run_or_hook (XEN hook, XEN args, const char *caller)
{
#if HAVE_GUILE
  /* Guile built-in scm_c_run_hook doesn't return the value of the hook procedure(s) and calls everything on the list */
  XEN result = XEN_FALSE; /* (or) -> #f */
  XEN procs = XEN_HOOK_PROCEDURES (hook);
  while (XEN_NOT_NULL_P (procs))
    {
      if (!(XEN_EQ_P(args, XEN_EMPTY_LIST)))
	result = XEN_APPLY(XEN_CAR(procs), args, caller);
      else result = XEN_CALL_0(XEN_CAR(procs), caller);
      if (XEN_NOT_FALSE_P(result)) return(result);
      procs = XEN_CDR (procs);
    }
  return(xen_return_first(result, args));
#else
  if (!(XEN_EQ_P(args, XEN_EMPTY_LIST)))
    return(XEN_APPLY(XEN_VARIABLE_REF(hook), args, caller));
  else return(XEN_CALL_0(XEN_VARIABLE_REF(hook), caller));
#endif
}

XEN g_c_run_and_hook (XEN hook, XEN args, const char *caller)
{
#if HAVE_GUILE
  XEN result = XEN_TRUE; /* (and) -> #t */
  XEN procs = XEN_HOOK_PROCEDURES (hook);
  while (XEN_NOT_NULL_P (procs))
    {
      if (!(XEN_EQ_P(args, XEN_EMPTY_LIST)))
	result = XEN_APPLY(XEN_CAR(procs), args, caller);
      else result = XEN_CALL_0(XEN_CAR(procs), caller);
      if (XEN_FALSE_P(result)) return(result);
      procs = XEN_CDR (procs);
    }
  return(xen_return_first(result, args));
#else
  if (!(XEN_EQ_P(args, XEN_EMPTY_LIST)))
    return(XEN_APPLY(XEN_VARIABLE_REF(hook), args, caller));
  else return(XEN_CALL_0(XEN_VARIABLE_REF(hook), caller));
#endif
}

void during_open(int fd, char *file, int reason)
{
  if (XEN_HOOKED(during_open_hook))
    g_c_run_progn_hook(during_open_hook,
		       XEN_LIST_3(C_TO_XEN_INT(fd),
				  C_TO_XEN_STRING(file),
				  C_TO_XEN_INT(reason)),
		       S_during_open_hook);
}

void after_open(int index)
{
  if (XEN_HOOKED(after_open_hook))
    g_c_run_progn_hook(after_open_hook,
		       XEN_LIST_1(C_TO_SMALL_XEN_INT(index)),
		       S_after_open_hook);
}


char *output_comment(file_info *hdr)
{
  char *com = NULL;
  if (hdr) com = mus_sound_comment(hdr->name);
  if (XEN_HOOKED(output_comment_hook))
    {
      XEN result;
      XEN procs = XEN_HOOK_PROCEDURES (output_comment_hook);
      int comment_size = 0;
      char *new_comment = NULL, *tmpstr = NULL;
#if HAVE_GUILE
      while (XEN_NOT_NULL_P (procs))
	{
	  result = XEN_CALL_1(XEN_CAR(procs),
			      C_TO_XEN_STRING(com),
			      S_output_comment_hook);
#else
	  result = XEN_CALL_1(XEN_VARIABLE_REF(procs),
			      C_TO_XEN_STRING(com),
			      S_output_comment_hook);
#endif
	  tmpstr = XEN_TO_NEW_C_STRING(result);
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
#if HAVE_GUILE
	  procs = XEN_CDR (procs);
	}
#endif
      return(new_comment);
    }
  else return(com);
}

#if HAVE_LADSPA
  void g_ladspa_to_snd();
#endif

#if HAVE_GUILE && HAVE_DLFCN_H
#include <dlfcn.h>
/* these are included because libtool's dlopen is incredibly stupid, as is libtool in general */

static XEN g_dlopen(XEN name)
{
  void *handle;
  handle = dlopen(XEN_TO_C_STRING(name), RTLD_LAZY);
  if (handle == NULL) return(C_TO_XEN_STRING(dlerror()));
  return(XEN_WRAP_C_POINTER(handle));
}

static XEN g_dlclose(XEN handle)
{
  return(C_TO_XEN_INT(dlclose((void *)(XEN_UNWRAP_C_POINTER(handle)))));
}

static XEN g_dlerror(void)
{
  return(C_TO_XEN_STRING(dlerror()));
}

static XEN g_dlinit(XEN handle, XEN func)
{
  typedef void *(*snd_dl_func)(void);
  void *proc;
  proc = dlsym((void *)(XEN_UNWRAP_C_POINTER(handle)), XEN_TO_C_STRING(func));
  if (proc == NULL) return(C_TO_XEN_STRING(dlerror()));
  ((snd_dl_func)proc)();
  return(XEN_TRUE);
}

#endif

static XEN g_little_endian(void)
{
#if MUS_LITTLE_ENDIAN
  return(XEN_TRUE);
#else
  return(XEN_FALSE);
#endif
}

static XEN g_snd_completion(XEN text)
{
  /* perhaps callable from emacs? */
  return(C_TO_XEN_STRING(command_completer(XEN_TO_C_STRING(text))));
}

#if HAVE_GUILE
static XEN g_gc_off(void) {++scm_block_gc; return(C_TO_XEN_INT(scm_block_gc));}
static XEN g_gc_on(void) {--scm_block_gc; return(C_TO_XEN_INT(scm_block_gc));}
#endif

#if WITH_MCHECK
#include <mcheck.h>
static XEN g_mcheck_check_all(void)
{
  mcheck_check_all();
  return(XEN_FALSE);
}
#endif


#ifdef XEN_ARGIFY_1
#if HAVE_GUILE && HAVE_DLFCN_H
XEN_NARGIFY_1(g_dlopen_w, g_dlopen)
XEN_NARGIFY_1(g_dlclose_w, g_dlclose)
XEN_NARGIFY_0(g_dlerror_w, g_dlerror)
XEN_NARGIFY_2(g_dlinit_w, g_dlinit)
#endif
XEN_NARGIFY_0(g_region_graph_style_w, g_region_graph_style)
XEN_NARGIFY_1(g_set_region_graph_style_w, g_set_region_graph_style)
XEN_NARGIFY_0(g_ask_before_overwrite_w, g_ask_before_overwrite)
XEN_ARGIFY_1(g_set_ask_before_overwrite_w, g_set_ask_before_overwrite)
XEN_NARGIFY_0(g_audio_output_device_w, g_audio_output_device)
XEN_ARGIFY_1(g_set_audio_output_device_w, g_set_audio_output_device)
XEN_NARGIFY_0(g_audio_input_device_w, g_audio_input_device)
XEN_ARGIFY_1(g_set_audio_input_device_w, g_set_audio_input_device)
XEN_NARGIFY_0(g_minibuffer_history_length_w, g_minibuffer_history_length)
XEN_ARGIFY_1(g_set_minibuffer_history_length_w, g_set_minibuffer_history_length)
XEN_NARGIFY_0(g_dac_size_w, g_dac_size)
XEN_ARGIFY_1(g_set_dac_size_w, g_set_dac_size)
XEN_NARGIFY_0(g_dac_combines_channels_w, g_dac_combines_channels)
XEN_ARGIFY_1(g_set_dac_combines_channels_w, g_set_dac_combines_channels)
XEN_NARGIFY_0(g_auto_resize_w, g_auto_resize)
XEN_ARGIFY_1(g_set_auto_resize_w, g_set_auto_resize)
XEN_NARGIFY_0(g_auto_update_w, g_auto_update)
XEN_ARGIFY_1(g_set_auto_update_w, g_set_auto_update)
XEN_NARGIFY_0(g_filter_env_in_hz_w, g_filter_env_in_hz)
XEN_ARGIFY_1(g_set_filter_env_in_hz_w, g_set_filter_env_in_hz)
XEN_NARGIFY_0(g_color_cutoff_w, g_color_cutoff)
XEN_ARGIFY_1(g_set_color_cutoff_w, g_set_color_cutoff)
XEN_NARGIFY_0(g_color_inverted_w, g_color_inverted)
XEN_ARGIFY_1(g_set_color_inverted_w, g_set_color_inverted)
XEN_NARGIFY_0(g_color_scale_w, g_color_scale)
XEN_ARGIFY_1(g_set_color_scale_w, g_set_color_scale)
XEN_NARGIFY_0(g_auto_update_interval_w, g_auto_update_interval)
XEN_ARGIFY_1(g_set_auto_update_interval_w, g_set_auto_update_interval)
XEN_NARGIFY_0(g_default_output_chans_w, g_default_output_chans)
XEN_ARGIFY_1(g_set_default_output_chans_w, g_set_default_output_chans)
XEN_NARGIFY_0(g_default_output_srate_w, g_default_output_srate)
XEN_ARGIFY_1(g_set_default_output_srate_w, g_set_default_output_srate)
XEN_NARGIFY_0(g_default_output_type_w, g_default_output_type)
XEN_ARGIFY_1(g_set_default_output_type_w, g_set_default_output_type)
XEN_NARGIFY_0(g_default_output_format_w, g_default_output_format)
XEN_ARGIFY_1(g_set_default_output_format_w, g_set_default_output_format)
XEN_NARGIFY_0(g_eps_file_w, g_eps_file)
XEN_NARGIFY_1(g_set_eps_file_w, g_set_eps_file)
XEN_NARGIFY_0(g_eps_left_margin_w, g_eps_left_margin)
XEN_ARGIFY_1(g_set_eps_left_margin_w, g_set_eps_left_margin)
XEN_NARGIFY_0(g_eps_bottom_margin_w, g_eps_bottom_margin)
XEN_ARGIFY_1(g_set_eps_bottom_margin_w, g_set_eps_bottom_margin)
XEN_NARGIFY_0(g_listener_prompt_w, g_listener_prompt)
XEN_ARGIFY_1(g_set_listener_prompt_w, g_set_listener_prompt)
XEN_NARGIFY_0(g_audio_state_file_w, g_audio_state_file)
XEN_ARGIFY_1(g_set_audio_state_file_w, g_set_audio_state_file)
XEN_NARGIFY_0(g_movies_w, g_movies)
XEN_ARGIFY_1(g_set_movies_w, g_set_movies)
XEN_NARGIFY_0(g_selection_creates_region_w, g_selection_creates_region)
XEN_ARGIFY_1(g_set_selection_creates_region_w, g_set_selection_creates_region)
XEN_NARGIFY_0(g_print_length_w, g_print_length)
XEN_ARGIFY_1(g_set_print_length_w, g_set_print_length)
XEN_NARGIFY_0(g_previous_files_sort_w, g_previous_files_sort)
XEN_ARGIFY_1(g_set_previous_files_sort_w, g_set_previous_files_sort)
XEN_NARGIFY_0(g_show_listener_w, g_show_listener)
XEN_ARGIFY_1(g_set_show_listener_w, g_set_show_listener)
XEN_NARGIFY_0(g_show_indices_w, g_show_indices)
XEN_ARGIFY_1(g_set_show_indices_w, g_set_show_indices)
XEN_NARGIFY_0(g_show_backtrace_w, g_show_backtrace)
XEN_ARGIFY_1(g_set_show_backtrace_w, g_set_show_backtrace)
XEN_NARGIFY_0(g_show_usage_stats_w, g_show_usage_stats)
XEN_ARGIFY_1(g_set_show_usage_stats_w, g_set_show_usage_stats)
XEN_NARGIFY_0(g_sinc_width_w, g_sinc_width)
XEN_ARGIFY_1(g_set_sinc_width_w, g_set_sinc_width)
XEN_NARGIFY_0(g_hankel_jn_w, g_hankel_jn)
XEN_ARGIFY_1(g_set_hankel_jn_w, g_set_hankel_jn)
XEN_NARGIFY_0(g_color_map_w, g_color_map)
XEN_ARGIFY_1(g_set_color_map_w, g_set_color_map)
XEN_NARGIFY_0(g_temp_dir_w, g_temp_dir)
XEN_ARGIFY_1(g_set_temp_dir_w, g_set_temp_dir)
XEN_NARGIFY_0(g_save_dir_w, g_save_dir)
XEN_ARGIFY_1(g_set_save_dir_w, g_set_save_dir)
XEN_NARGIFY_0(g_trap_segfault_w, g_trap_segfault)
XEN_ARGIFY_1(g_set_trap_segfault_w, g_set_trap_segfault)
XEN_NARGIFY_0(g_show_selection_transform_w, g_show_selection_transform)
XEN_ARGIFY_1(g_set_show_selection_transform_w, g_set_show_selection_transform)
XEN_NARGIFY_0(g_with_mix_tags_w, g_with_mix_tags)
XEN_ARGIFY_1(g_set_with_mix_tags_w, g_set_with_mix_tags)
XEN_NARGIFY_0(g_use_sinc_interp_w, g_use_sinc_interp)
XEN_ARGIFY_1(g_set_use_sinc_interp_w, g_set_use_sinc_interp)
XEN_NARGIFY_0(g_data_clipped_w, g_data_clipped)
XEN_ARGIFY_1(g_set_data_clipped_w, g_set_data_clipped)
XEN_NARGIFY_0(g_vu_font_w, g_vu_font)
XEN_ARGIFY_1(g_set_vu_font_w, g_set_vu_font)
XEN_NARGIFY_0(g_vu_font_size_w, g_vu_font_size)
XEN_ARGIFY_1(g_set_vu_font_size_w, g_set_vu_font_size)
XEN_NARGIFY_0(g_vu_size_w, g_vu_size)
XEN_ARGIFY_1(g_set_vu_size_w, g_set_vu_size)
XEN_NARGIFY_0(g_window_x_w, g_window_x)
XEN_ARGIFY_1(g_set_window_x_w, g_set_window_x)
XEN_NARGIFY_0(g_window_y_w, g_window_y)
XEN_ARGIFY_1(g_set_window_y_w, g_set_window_y)
XEN_NARGIFY_0(g_zoom_focus_style_w, g_zoom_focus_style)
XEN_ARGIFY_1(g_set_zoom_focus_style_w, g_set_zoom_focus_style)
XEN_NARGIFY_0(g_help_text_font_w, g_help_text_font)
XEN_ARGIFY_1(g_set_help_text_font_w, g_set_help_text_font)
XEN_NARGIFY_0(g_tiny_font_w, g_tiny_font)
XEN_ARGIFY_1(g_set_tiny_font_w, g_set_tiny_font)
XEN_NARGIFY_0(g_button_font_w, g_button_font)
XEN_ARGIFY_1(g_set_button_font_w, g_set_button_font)
XEN_NARGIFY_0(g_bold_button_font_w, g_bold_button_font)
XEN_ARGIFY_1(g_set_bold_button_font_w, g_set_bold_button_font)
XEN_NARGIFY_0(g_axis_label_font_w, g_axis_label_font)
XEN_ARGIFY_1(g_set_axis_label_font_w, g_set_axis_label_font)
XEN_NARGIFY_0(g_axis_numbers_font_w, g_axis_numbers_font)
XEN_ARGIFY_1(g_set_axis_numbers_font_w, g_set_axis_numbers_font)
XEN_NARGIFY_0(g_listener_font_w, g_listener_font)
XEN_ARGIFY_1(g_set_listener_font_w, g_set_listener_font)
XEN_NARGIFY_0(g_window_width_w, g_window_width)
XEN_ARGIFY_1(g_set_window_width_w, g_set_window_width)
XEN_NARGIFY_0(g_window_height_w, g_window_height)
XEN_ARGIFY_1(g_set_window_height_w, g_set_window_height)
#if (!USE_NO_GUI)
#if HAVE_HTML
XEN_NARGIFY_0(g_html_dir_w, g_html_dir)
XEN_NARGIFY_1(g_set_html_dir_w, g_set_html_dir)
#endif
XEN_NARGIFY_0(g_selection_color_w, g_selection_color)
XEN_NARGIFY_1(g_set_selection_color_w, g_set_selection_color)
XEN_NARGIFY_0(g_zoom_color_w, g_zoom_color)
XEN_NARGIFY_1(g_set_zoom_color_w, g_set_zoom_color)
XEN_NARGIFY_0(g_position_color_w, g_position_color)
XEN_NARGIFY_1(g_set_position_color_w, g_set_position_color)
XEN_NARGIFY_0(g_mark_color_w, g_mark_color)
XEN_NARGIFY_1(g_set_mark_color_w, g_set_mark_color)
XEN_NARGIFY_0(g_listener_color_w, g_listener_color)
XEN_NARGIFY_1(g_set_listener_color_w, g_set_listener_color)
XEN_NARGIFY_0(g_listener_text_color_w, g_listener_text_color)
XEN_NARGIFY_1(g_set_listener_text_color_w, g_set_listener_text_color)
XEN_NARGIFY_0(g_enved_waveform_color_w, g_enved_waveform_color)
XEN_NARGIFY_1(g_set_enved_waveform_color_w, g_set_enved_waveform_color)
XEN_NARGIFY_0(g_filter_waveform_color_w, g_filter_waveform_color)
XEN_NARGIFY_1(g_set_filter_waveform_color_w, g_set_filter_waveform_color)
XEN_NARGIFY_0(g_highlight_color_w, g_highlight_color)
XEN_NARGIFY_1(g_set_highlight_color_w, g_set_highlight_color)
XEN_NARGIFY_0(g_cursor_color_w, g_cursor_color)
XEN_NARGIFY_1(g_set_cursor_color_w, g_set_cursor_color)
XEN_ARGIFY_1(g_mix_color_w, g_mix_color)
XEN_ARGIFY_2(g_set_mix_color_w, g_set_mix_color)
XEN_NARGIFY_0(g_selected_mix_color_w, g_selected_mix_color)
XEN_ARGIFY_1(g_set_selected_mix_color_w, g_set_selected_mix_color)
XEN_NARGIFY_0(g_text_focus_color_w, g_text_focus_color)
XEN_NARGIFY_1(g_set_text_focus_color_w, g_set_text_focus_color)
XEN_NARGIFY_0(g_sash_color_w, g_sash_color)
XEN_NARGIFY_1(g_set_sash_color_w, g_set_sash_color)
XEN_NARGIFY_0(g_data_color_w, g_data_color)
XEN_NARGIFY_1(g_set_data_color_w, g_set_data_color)
XEN_NARGIFY_0(g_graph_color_w, g_graph_color)
XEN_NARGIFY_1(g_set_graph_color_w, g_set_graph_color)
XEN_NARGIFY_0(g_selected_graph_color_w, g_selected_graph_color)
XEN_NARGIFY_1(g_set_selected_graph_color_w, g_set_selected_graph_color)
XEN_NARGIFY_0(g_selected_data_color_w, g_selected_data_color)
XEN_NARGIFY_1(g_set_selected_data_color_w, g_set_selected_data_color)
XEN_NARGIFY_0(g_basic_color_w, g_basic_color)
XEN_NARGIFY_1(g_set_basic_color_w, g_set_basic_color)
XEN_NARGIFY_0(g_pushed_button_color_w, g_pushed_button_color)
XEN_NARGIFY_1(g_set_pushed_button_color_w, g_set_pushed_button_color)
#endif
XEN_NARGIFY_0(g_snd_tempnam_w, g_snd_tempnam)
XEN_NARGIFY_2(g_set_oss_buffers_w, g_set_oss_buffers)
XEN_NARGIFY_0(g_update_usage_stats_w, g_update_usage_stats)
XEN_NARGIFY_0(g_clear_audio_inputs_w, g_clear_audio_inputs)
XEN_NARGIFY_0(g_color_dialog_w, g_color_dialog)
XEN_NARGIFY_0(g_orientation_dialog_w, g_orientation_dialog)
XEN_NARGIFY_0(g_transform_dialog_w, g_transform_dialog)
XEN_NARGIFY_0(g_file_dialog_w, g_file_dialog)
XEN_ARGIFY_1(g_edit_header_dialog_w, g_edit_header_dialog)
XEN_NARGIFY_0(g_edit_save_as_dialog_w, g_edit_save_as_dialog)
XEN_NARGIFY_2(g_help_dialog_w, g_help_dialog)
XEN_NARGIFY_0(g_mix_panel_w, g_mix_panel)
XEN_NARGIFY_0(g_max_sounds_w, g_max_sounds)
XEN_NARGIFY_0(g_sounds_w, g_sounds)
XEN_NARGIFY_1(g_yes_or_no_p_w, g_yes_or_no_p)
XEN_NARGIFY_0(g_abort_w, g_abort)
XEN_NARGIFY_0(g_dismiss_all_dialogs_w, g_dismiss_all_dialogs)
XEN_NARGIFY_0(g_abortq_w, g_abortq)
XEN_NARGIFY_0(g_snd_version_w, g_snd_version)
XEN_ARGIFY_1(g_equalize_panes_w, g_equalize_panes)
XEN_ARGIFY_4(g_open_sound_file_w, g_open_sound_file)
XEN_NARGIFY_2(g_close_sound_file_w, g_close_sound_file)
XEN_NARGIFY_3(vct2soundfile_w, vct2soundfile)
XEN_ARGIFY_9(g_graph_w, g_graph)
XEN_ARGIFY_6(samples2vct_w, samples2vct)
XEN_ARGIFY_7(samples2sound_data_w, samples2sound_data)
XEN_ARGIFY_1(g_start_progress_report_w, g_start_progress_report)
XEN_ARGIFY_1(g_finish_progress_report_w, g_finish_progress_report)
XEN_ARGIFY_5(g_progress_report_w, g_progress_report)
XEN_NARGIFY_1(g_snd_print_w, g_snd_print)
XEN_NARGIFY_0(g_mus_audio_describe_w, g_mus_audio_describe)
XEN_NARGIFY_0(g_little_endian_w, g_little_endian)
XEN_NARGIFY_1(g_snd_completion_w, g_snd_completion)
#if HAVE_GUILE
XEN_NARGIFY_0(g_gc_off_w, g_gc_off)
XEN_NARGIFY_0(g_gc_on_w, g_gc_on)
#endif
#else
#if HAVE_GUILE && HAVE_DLFCN_H
#define g_dlopen_w g_dlopen
#define g_dlclose_w g_dlclose
#define g_dlerror_w g_dlerror
#define g_dlinit_w g_dlinit
#endif
#define g_region_graph_style_w g_region_graph_style
#define g_set_region_graph_style_w g_set_region_graph_style
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
#define g_edit_save_as_dialog_w g_edit_save_as_dialog
#define g_help_dialog_w g_help_dialog
#define g_mix_panel_w g_mix_panel
#define g_max_sounds_w g_max_sounds
#define g_sounds_w g_sounds
#define g_yes_or_no_p_w g_yes_or_no_p
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
  state = ss;

  XEN_DEFINE_PROCEDURE("show-stack", show_stack, 0 ,0, 0, "show stack trace");

#if WITH_MCHECK
  XEN_NEW_PROCEDURE("mcheck-all", g_mcheck_check_all_w, 0, 0, 0);
#endif

#if TIMING
  g_init_timing();
#endif
  mus_sndlib2xen_initialize();

  /* ---------------- CONSTANTS ---------------- */

  #define H_zoom_focus_left "The value for " S_zoom_focus_style " that causes zooming to maintain the left edge steady"
  #define H_zoom_focus_right "The value for " S_zoom_focus_style " that causes zooming to maintain the right edge steady"
  #define H_zoom_focus_middle "The value for " S_zoom_focus_style " that causes zooming to focus on the middle sample"
  #define H_zoom_focus_active "The value for " S_zoom_focus_style " that causes zooming to focus on the currently active object"

  XEN_DEFINE_CONSTANT(S_zoom_focus_left,       ZOOM_FOCUS_LEFT,   H_zoom_focus_left);
  XEN_DEFINE_CONSTANT(S_zoom_focus_right,      ZOOM_FOCUS_RIGHT,  H_zoom_focus_right);
  XEN_DEFINE_CONSTANT(S_zoom_focus_active,     ZOOM_FOCUS_ACTIVE, H_zoom_focus_active);
  XEN_DEFINE_CONSTANT(S_zoom_focus_middle,     ZOOM_FOCUS_MIDDLE, H_zoom_focus_middle);

  #define H_cursor_in_view "The value for an " S_bind_key " function that causes it to shift the window so that the cursor is in the view"
  #define H_cursor_on_left "The value for an " S_bind_key " function that causes it to shift the window so that the cursor is at the left edge"
  #define H_cursor_on_right "The value for an " S_bind_key " function that causes it to shift the window so that the cursor is at the right edge"
  #define H_cursor_in_middle "The value for an " S_bind_key " function that causes it to shift the window so that the cursor is in the middle"
  #define H_cursor_update_display "The value for an " S_bind_key " function that causes it to redraw the graph"
  #define H_cursor_no_action "The value for an " S_bind_key " function that causes it do nothing with the graph window"
  #define H_keyboard_no_action "The value for an " S_bind_key " function that causes it do nothing upon return"

  XEN_DEFINE_CONSTANT(S_cursor_in_view,        CURSOR_IN_VIEW,        H_cursor_in_view);
  XEN_DEFINE_CONSTANT(S_cursor_on_left,        CURSOR_ON_LEFT,        H_cursor_on_left);
  XEN_DEFINE_CONSTANT(S_cursor_on_right,       CURSOR_ON_RIGHT,       H_cursor_on_right);
  XEN_DEFINE_CONSTANT(S_cursor_in_middle,      CURSOR_IN_MIDDLE,      H_cursor_in_middle);
  XEN_DEFINE_CONSTANT(S_cursor_update_display, CURSOR_UPDATE_DISPLAY, H_cursor_update_display);
  XEN_DEFINE_CONSTANT(S_cursor_no_action,      CURSOR_NO_ACTION,      H_cursor_no_action);
  XEN_DEFINE_CONSTANT(S_keyboard_no_action,    KEYBOARD_NO_ACTION,    H_keyboard_no_action);

  XEN_DEFINE_CONSTANT(S_time_graph,           TIME_AXIS_INFO,      "time domain graph");
  XEN_DEFINE_CONSTANT(S_transform_graph,      TRANSFORM_AXIS_INFO, "frequency domain graph");
  XEN_DEFINE_CONSTANT(S_lisp_graph,           LISP_AXIS_INFO,      "lisp graph");


  /* ---------------- VARIABLES ---------------- */
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_region_graph_style, g_region_graph_style_w, H_region_graph_style,
			       "set-" S_region_graph_style, g_set_region_graph_style_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_ask_before_overwrite, g_ask_before_overwrite_w, H_ask_before_overwrite,
			       "set-" S_ask_before_overwrite, g_set_ask_before_overwrite_w,  0, 0, 0, 1);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_audio_output_device, g_audio_output_device_w, H_audio_output_device,
			       "set-" S_audio_output_device, g_set_audio_output_device_w,  0, 0, 0, 1);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_audio_input_device, g_audio_input_device_w, H_audio_input_device,
			       "set-" S_audio_input_device, g_set_audio_input_device_w,  0, 0, 0, 1);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_minibuffer_history_length, g_minibuffer_history_length_w, H_minibuffer_history_length,
			       "set-" S_minibuffer_history_length, g_set_minibuffer_history_length_w,  0, 0, 0, 1);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_dac_size, g_dac_size_w, H_dac_size,
			       "set-" S_dac_size, g_set_dac_size_w,  0, 0, 0, 1);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_dac_combines_channels, g_dac_combines_channels_w, H_dac_combines_channels,
			       "set-" S_dac_combines_channels, g_set_dac_combines_channels_w,  0, 0, 0, 1);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_auto_resize, g_auto_resize_w, H_auto_resize,
			       "set-" S_auto_resize, g_set_auto_resize_w,  0, 0, 0, 1);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_auto_update, g_auto_update_w, H_auto_update,
			       "set-" S_auto_update, g_set_auto_update_w,  0, 0, 0, 1);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_filter_env_in_hz, g_filter_env_in_hz_w, H_filter_env_in_hz,
			       "set-" S_filter_env_in_hz, g_set_filter_env_in_hz_w,  0, 0, 0, 1);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_color_cutoff, g_color_cutoff_w, H_color_cutoff,
			       "set-" S_color_cutoff, g_set_color_cutoff_w,  0, 0, 0, 1);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_color_inverted, g_color_inverted_w, H_color_inverted,
			       "set-" S_color_inverted, g_set_color_inverted_w,  0, 0, 0, 1);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_color_scale, g_color_scale_w, H_color_scale,
			       "set-" S_color_scale, g_set_color_scale_w,  0, 0, 0, 1);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_auto_update_interval, g_auto_update_interval_w, H_auto_update_interval,
			       "set-" S_auto_update_interval, g_set_auto_update_interval_w,  0, 0, 0, 1);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_default_output_chans, g_default_output_chans_w, H_default_output_chans,
			       "set-" S_default_output_chans, g_set_default_output_chans_w,  0, 0, 0, 1);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_default_output_srate, g_default_output_srate_w, H_default_output_srate,
			       "set-" S_default_output_srate, g_set_default_output_srate_w,  0, 0, 0, 1);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_default_output_type, g_default_output_type_w, H_default_output_type,
			       "set-" S_default_output_type, g_set_default_output_type_w,  0, 0, 0, 1);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_default_output_format, g_default_output_format_w, H_default_output_format,
			       "set-" S_default_output_format, g_set_default_output_format_w,  0, 0, 0, 1);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_eps_file, g_eps_file_w, H_eps_file,
			       "set-" S_eps_file, g_set_eps_file_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_eps_left_margin, g_eps_left_margin_w, H_eps_left_margin,
			       "set-" S_eps_left_margin, g_set_eps_left_margin_w,  0, 0, 0, 1);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_eps_bottom_margin, g_eps_bottom_margin_w, H_eps_bottom_margin,
			       "set-" S_eps_bottom_margin, g_set_eps_bottom_margin_w,  0, 0, 0, 1);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_listener_prompt, g_listener_prompt_w, H_listener_prompt,
			       "set-" S_listener_prompt, g_set_listener_prompt_w,  0, 0, 0, 1);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_audio_state_file, g_audio_state_file_w, H_audio_state_file,
			       "set-" S_audio_state_file, g_set_audio_state_file_w,  0, 0, 0, 1);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_movies, g_movies_w, H_movies,
			       "set-" S_movies, g_set_movies_w,  0, 0, 0, 1);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_selection_creates_region, g_selection_creates_region_w, H_selection_creates_region,
			       "set-" S_selection_creates_region, g_set_selection_creates_region_w,  0, 0, 0, 1);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_print_length, g_print_length_w, H_print_length,
			       "set-" S_print_length, g_set_print_length_w,  0, 0, 0, 1);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_previous_files_sort, g_previous_files_sort_w, H_previous_files_sort,
			       "set-" S_previous_files_sort, g_set_previous_files_sort_w,  0, 0, 0, 1);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_show_listener, g_show_listener_w, H_show_listener,
			       "set-" S_show_listener, g_set_show_listener_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_show_indices, g_show_indices_w, H_show_indices,
			       "set-" S_show_indices, g_set_show_indices_w,  0, 0, 0, 1);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_show_backtrace, g_show_backtrace_w, H_show_backtrace,
			       "set-" S_show_backtrace, g_set_show_backtrace_w,  0, 0, 0, 1);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_show_usage_stats, g_show_usage_stats_w, H_show_usage_stats,
			       "set-" S_show_usage_stats, g_set_show_usage_stats_w,  0, 0, 0, 1);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_sinc_width, g_sinc_width_w, H_sinc_width,
			       "set-" S_sinc_width, g_set_sinc_width_w,  0, 0, 0, 1);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_hankel_jn, g_hankel_jn_w, H_hankel_jn,
			       "set-" S_hankel_jn, g_set_hankel_jn_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_colormap, g_color_map_w, H_colormap,
			       "set-" S_colormap, g_set_color_map_w,  0, 0, 0, 1);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_temp_dir, g_temp_dir_w, H_temp_dir,
			       "set-" S_temp_dir, g_set_temp_dir_w,  0, 0, 0, 1);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_save_dir, g_save_dir_w, H_save_dir,
			       "set-" S_save_dir, g_set_save_dir_w,  0, 0, 0, 1);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_trap_segfault, g_trap_segfault_w, H_trap_segfault,
			       "set-" S_trap_segfault, g_set_trap_segfault_w,  0, 0, 0, 1);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_show_selection_transform, g_show_selection_transform_w, H_show_selection_transform,
			       "set-" S_show_selection_transform, g_set_show_selection_transform_w,  0, 0, 0, 1);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_with_mix_tags, g_with_mix_tags_w, H_with_mix_tags,
			       "set-" S_with_mix_tags, g_set_with_mix_tags_w,  0, 0, 0, 1);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_use_sinc_interp, g_use_sinc_interp_w, H_use_sinc_interp,
			       "set-" S_use_sinc_interp, g_set_use_sinc_interp_w,  0, 0, 0, 1);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_data_clipped, g_data_clipped_w, H_data_clipped,
			       "set-" S_data_clipped, g_set_data_clipped_w,  0, 0, 0, 1);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_vu_font, g_vu_font_w, H_vu_font,
			       "set-" S_vu_font, g_set_vu_font_w,  0, 0, 0, 1);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_vu_font_size, g_vu_font_size_w, H_vu_font_size,
			       "set-" S_vu_font_size, g_set_vu_font_size_w,  0, 0, 0, 1);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_vu_size, g_vu_size_w, H_vu_size,
			       "set-" S_vu_size, g_set_vu_size_w,  0, 0, 0, 1);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_window_x, g_window_x_w, H_window_x,
			       "set-" S_window_x, g_set_window_x_w,  0, 0, 0, 1);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_window_y, g_window_y_w, H_window_y,
			       "set-" S_window_y, g_set_window_y_w,  0, 0, 0, 1);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_zoom_focus_style, g_zoom_focus_style_w, H_zoom_focus_style,
			       "set-" S_zoom_focus_style, g_set_zoom_focus_style_w,  0, 0, 0, 1);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_help_text_font, g_help_text_font_w, H_help_text_font,
			       "set-" S_help_text_font, g_set_help_text_font_w,  0, 0, 0, 1);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_tiny_font, g_tiny_font_w, H_tiny_font,
			       "set-" S_tiny_font, g_set_tiny_font_w,  0, 0, 0, 1);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_button_font, g_button_font_w, H_button_font,
			       "set-" S_button_font, g_set_button_font_w,  0, 0, 0, 1);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_bold_button_font, g_bold_button_font_w, H_bold_button_font,
			       "set-" S_bold_button_font, g_set_bold_button_font_w,  0, 0, 0, 1);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_axis_label_font, g_axis_label_font_w, H_axis_label_font,
			       "set-" S_axis_label_font, g_set_axis_label_font_w,  0, 0, 0, 1);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_axis_numbers_font, g_axis_numbers_font_w, H_axis_numbers_font,
			       "set-" S_axis_numbers_font, g_set_axis_numbers_font_w,  0, 0, 0, 1);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_listener_font, g_listener_font_w, H_listener_font,
			       "set-" S_listener_font, g_set_listener_font_w,  0, 0, 0, 1);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_window_width, g_window_width_w, H_window_width,
			       "set-" S_window_width, g_set_window_width_w,  0, 0, 0, 1);  

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_window_height, g_window_height_w, H_window_height,
			       "set-" S_window_height, g_set_window_height_w,  0, 0, 0, 1);


#if (!USE_NO_GUI)
  #if HAVE_HTML
  XEN_YES_WE_HAVE("snd-html");
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_html_dir, g_html_dir_w, H_html_dir,
			       "set-" S_html_dir, g_set_html_dir_w,  0, 0, 1, 0);
  #endif

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_selection_color, g_selection_color_w, H_selection_color,
			       "set-" S_selection_color, g_set_selection_color_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_zoom_color, g_zoom_color_w, H_zoom_color,
			       "set-" S_zoom_color, g_set_zoom_color_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_position_color, g_position_color_w, H_position_color,
			       "set-" S_position_color, g_set_position_color_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mark_color, g_mark_color_w, H_mark_color,
			       "set-" S_mark_color, g_set_mark_color_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_listener_color, g_listener_color_w, H_listener_color,
			       "set-" S_listener_color, g_set_listener_color_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_listener_text_color, g_listener_text_color_w, H_listener_text_color,
			       "set-" S_listener_text_color, g_set_listener_text_color_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_enved_waveform_color, g_enved_waveform_color_w, H_enved_waveform_color,
			       "set-" S_enved_waveform_color, g_set_enved_waveform_color_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_filter_waveform_color, g_filter_waveform_color_w, H_filter_waveform_color,
			       "set-" S_filter_waveform_color, g_set_filter_waveform_color_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_highlight_color, g_highlight_color_w, H_highlight_color,
			       "set-" S_highlight_color, g_set_highlight_color_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_cursor_color, g_cursor_color_w, H_cursor_color,
			       "set-" S_cursor_color, g_set_cursor_color_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mix_color, g_mix_color_w, H_mix_color,
			       "set-" S_mix_color, g_set_mix_color_w,  0, 1, 1, 1);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_selected_mix_color, g_selected_mix_color_w, H_selected_mix_color,
			       "set-" S_selected_mix_color, g_set_selected_mix_color_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_text_focus_color, g_text_focus_color_w, H_text_focus_color,
			       "set-" S_text_focus_color, g_set_text_focus_color_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_sash_color, g_sash_color_w, H_sash_color,
			       "set-" S_sash_color, g_set_sash_color_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_data_color, g_data_color_w, H_data_color,
			       "set-" S_data_color, g_set_data_color_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_graph_color, g_graph_color_w, H_graph_color,
			       "set-" S_graph_color, g_set_graph_color_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_selected_graph_color, g_selected_graph_color_w, H_selected_graph_color,
			       "set-" S_selected_graph_color, g_set_selected_graph_color_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_selected_data_color, g_selected_data_color_w, H_selected_data_color,
			       "set-" S_selected_data_color, g_set_selected_data_color_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_basic_color, g_basic_color_w, H_basic_color,
			       "set-" S_basic_color, g_set_basic_color_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_pushed_button_color, g_pushed_button_color_w, H_pushed_button_color,
			       "set-" S_pushed_button_color, g_set_pushed_button_color_w,  0, 0, 1, 0);
#endif


  /* ---------------- FUNCTIONS ---------------- */

  XEN_DEFINE_PROCEDURE(S_snd_tempnam,         g_snd_tempnam_w, 0, 0, 0,         H_snd_tempnam);
  XEN_DEFINE_PROCEDURE("set-" S_oss_buffers,  g_set_oss_buffers_w, 2, 0, 0,     H_set_oss_buffers);
  XEN_DEFINE_PROCEDURE(S_update_usage_stats,  g_update_usage_stats_w, 0, 0, 0,  H_update_usage_stats);
  XEN_DEFINE_PROCEDURE(S_clear_audio_inputs,  g_clear_audio_inputs_w, 0, 0, 0,  H_clear_audio_inputs);
  XEN_DEFINE_PROCEDURE(S_color_dialog,        g_color_dialog_w, 0, 0, 0,        H_color_dialog);
  XEN_DEFINE_PROCEDURE(S_orientation_dialog,  g_orientation_dialog_w, 0, 0, 0,  H_orientation_dialog);
  XEN_DEFINE_PROCEDURE(S_transform_dialog,    g_transform_dialog_w, 0, 0, 0,    H_transform_dialog);
  XEN_DEFINE_PROCEDURE(S_file_dialog,         g_file_dialog_w, 0, 0, 0,         H_file_dialog);
  XEN_DEFINE_PROCEDURE(S_edit_header_dialog,  g_edit_header_dialog_w, 0, 1, 0,  H_edit_header_dialog);
  XEN_DEFINE_PROCEDURE(S_edit_save_as_dialog, g_edit_save_as_dialog_w, 0, 0, 0, H_edit_save_as_dialog);
  XEN_DEFINE_PROCEDURE(S_help_dialog,         g_help_dialog_w, 2, 0, 0,         H_help_dialog);
  XEN_DEFINE_PROCEDURE(S_mix_panel,           g_mix_panel_w, 0, 0, 0,           H_mix_panel);

  XEN_DEFINE_PROCEDURE(S_max_sounds,          g_max_sounds_w, 0, 0, 0,          H_max_sounds);
  XEN_DEFINE_PROCEDURE(S_sounds,              g_sounds_w, 0, 0, 0,              H_sounds);
  XEN_DEFINE_PROCEDURE(S_yes_or_no_p,         g_yes_or_no_p_w, 1, 0, 0,         H_yes_or_no_p);
  XEN_DEFINE_PROCEDURE(S_abort,               g_abort_w, 0, 0, 0,               H_abort);
  XEN_DEFINE_PROCEDURE(S_dismiss_all_dialogs, g_dismiss_all_dialogs_w, 0, 0, 0, H_dismiss_all_dialogs);
  XEN_DEFINE_PROCEDURE(S_c_g,                 g_abortq_w, 0, 0, 0,              H_abortQ);
  XEN_DEFINE_PROCEDURE(S_snd_version,         g_snd_version_w, 0, 0, 0,         H_snd_version);
  XEN_DEFINE_PROCEDURE(S_equalize_panes,      g_equalize_panes_w, 0, 1, 0,      H_equalize_panes);
  XEN_DEFINE_PROCEDURE(S_open_sound_file,     g_open_sound_file_w, 0, 4, 0,     H_open_sound_file);
  XEN_DEFINE_PROCEDURE(S_close_sound_file,    g_close_sound_file_w, 2, 0, 0,    H_close_sound_file);
  XEN_DEFINE_PROCEDURE(S_vct2sound_file,      vct2soundfile_w, 3, 0, 0,         H_vct2sound_file);
  XEN_DEFINE_PROCEDURE(S_graph,               g_graph_w, 1, 8, 0,               H_graph);
  XEN_DEFINE_PROCEDURE(S_samples2vct,         samples2vct_w, 0, 6, 0,           H_samples2vct);
  XEN_DEFINE_PROCEDURE(S_samples2sound_data,  samples2sound_data_w, 0, 7, 0,    H_samples2sound_data);
  XEN_DEFINE_PROCEDURE(S_start_progress_report, g_start_progress_report_w, 0, 1, 0, H_start_progress_report);
  XEN_DEFINE_PROCEDURE(S_finish_progress_report, g_finish_progress_report_w, 0, 1, 0, H_finish_progress_report);
  XEN_DEFINE_PROCEDURE(S_progress_report,     g_progress_report_w, 1, 4, 0,     H_progress_report);
  XEN_DEFINE_PROCEDURE(S_snd_print,           g_snd_print_w, 1, 0, 0,           H_snd_print);

  XEN_DEFINE_PROCEDURE("describe-audio",      g_mus_audio_describe_w, 0, 0, 0,  H_mus_audio_describe);
  /* this (describe-audio) is going away someday */

  XEN_DEFINE_PROCEDURE(S_mus_audio_describe,  g_mus_audio_describe_w, 0, 0, 0,  H_mus_audio_describe);
  XEN_DEFINE_PROCEDURE("little-endian?",      g_little_endian_w, 0, 0, 0,       "return #t if host is little endian");
  XEN_DEFINE_PROCEDURE("snd-completion",      g_snd_completion_w, 1, 0, 0,      "return completion of arg");
#if HAVE_GUILE
  XEN_DEFINE_PROCEDURE("gc-off",              g_gc_off_w, 0, 0, 0,              "turn off GC");
  XEN_DEFINE_PROCEDURE("gc-on",               g_gc_on_w, 0, 0, 0,               "turn on GC");
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

  XEN_DEFINE_HOOK(during_open_hook,    S_during_open_hook, 3,    H_during_open_hook);    /* args = fd filename reason */
  XEN_DEFINE_HOOK(after_open_hook,     S_after_open_hook, 1,     H_after_open_hook);     /* args = sound */
  XEN_DEFINE_HOOK(output_comment_hook, S_output_comment_hook, 1, H_output_comment_hook); /* arg = current mus_sound_comment(hdr) if any */
  XEN_DEFINE_HOOK(print_hook,          S_print_hook, 1,          H_print_hook);          /* arg = text */

  g_init_marks();
  g_init_regions();
  g_init_selection();
  g_init_dac();
  init_vct();
  mus_xen_init();
  g_initialize_xgh(state);
  g_initialize_xgfile();
  g_init_gxutils();
  g_init_mix();
  g_init_chn();
  g_init_kbd();
  g_init_sig();
  g_init_print();
  g_init_errors();
  g_init_fft();
  g_init_edits();
  g_init_listener();
  g_init_help();
  g_init_menu();
  g_init_main();
  g_init_snd();
  g_init_file();
  g_init_data();
  g_init_env();
  g_init_recorder();
  g_init_gxenv();
  g_init_gxmenu();
  g_init_find();
#if (!USE_NO_GUI)
  g_init_axis();
  g_init_gxmain();
  g_init_gxlistener();
  g_init_gxchn();
  g_init_draw();
  g_init_gxdrop();
  g_init_gxregion();
#if DEBUGGING
  g_init_gxfind();
#endif
#endif
#if HAVE_GUILE && HAVE_DLFCN_H
  XEN_DEFINE_PROCEDURE("dlopen", g_dlopen_w, 1, 0 ,0, "");
  XEN_DEFINE_PROCEDURE("dlclose", g_dlclose_w, 1, 0 ,0, "");
  XEN_DEFINE_PROCEDURE("dlerror", g_dlerror_w, 0, 0 ,0, "");
  XEN_DEFINE_PROCEDURE("dlinit", g_dlinit_w, 2, 0 ,0, "");
#endif
#if HAVE_LADSPA
  g_ladspa_to_snd();
#endif

#if HAVE_GUILE
  XEN_EVAL_C_STRING("(defmacro defvar (a b)\
                       `(begin\
                          (define , a , b)\
                          (define-envelope (symbol->string ', a) , b)))");
  /* this is trying to keep track of envelopes for the envelope editor */

  XEN_EVAL_C_STRING("(define (" S_snd_apropos " val) (snd-print (with-output-to-string (lambda () (apropos (if (string? val) val (object->string val)))))))");
  XEN_EVAL_C_STRING("(read-set! keywords 'prefix)");
  XEN_EVAL_C_STRING("(print-enable 'source)");  /* added 13-Feb-01 */

  /* not sure about these two */
  XEN_EVAL_C_STRING("(define scale-sound-to scale-to)");
  XEN_EVAL_C_STRING("(define scale-sound-by scale-by)");

  /* from ice-9/r4rs.scm but with output to snd listener */
  XEN_EVAL_C_STRING("(define snd-last-file-loaded #f)");
  XEN_EVAL_C_STRING("(set! %load-hook (lambda (filename)\
                                        (set! snd-last-file-loaded filename)\
                                        (if %load-verbosely\
                                          (snd-print (format #f \";;; loading ~S\" filename)))))");
#endif

#if USE_MOTIF
  XEN_YES_WE_HAVE("snd-motif");
#endif
#if USE_GTK
  XEN_YES_WE_HAVE("snd-gtk");
#endif
#if USE_NO_GUI
  XEN_YES_WE_HAVE("snd-nogui");
#endif
#if HAVE_GUILE
  XEN_YES_WE_HAVE("snd-guile");
#endif
#if HAVE_RUBY
  XEN_YES_WE_HAVE("snd-ruby");
#endif

  XEN_YES_WE_HAVE("snd");
}


