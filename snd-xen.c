#include "snd.h"
#include "clm2xen.h"
#include "sndlib-strings.h"

/* Snd defines its own exit, delay, and frame? clobbering (presumably) the Guile versions,
 *   delay is protected in clm2xen.c as %delay, frame? as %frame?
 *   In Ruby, rand is protected as kernel_rand.
 */

/* -------- protect XEN vars from GC -------- */

static XEN gc_protection;
static int gc_protection_size = 0;
#define DEFAULT_GC_VALUE XEN_ZERO
static int gc_last_cleared = -1;
static int gc_last_set = -1;

#if 0
void dump_protection(void);
void dump_protection(void)
{
  XEN *gcdata;
  int i;
  gcdata = XEN_VECTOR_ELEMENTS(gc_protection);
  for (i = 0; i < gc_protection_size; i++)
    if (!(XEN_EQ_P(gcdata[i], DEFAULT_GC_VALUE)))
      fprintf(stderr,"protect[%i] %p\n",i, gcdata[i]);
}
#endif

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

#if HAVE_GUILE
  #include <libguile/fluids.h>
#endif

static int send_error_output_to_stdout = FALSE;
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

#ifndef DEBUGGING
  #define DEBUGGING 0
#endif

static XEN snd_catch_scm_error(void *data, XEN tag, XEN throw_args) /* error handler */
{
#if HAVE_GUILE
  snd_info *sp;
  char *possible_code;
  XEN port; XEN stmp;
  XEN stack;
  char *name_buf = NULL;
  snd_state *ss;
  ss = get_global_state();
#ifdef SCM_MAKE_CHAR
  port = scm_mkstrport(XEN_ZERO, 
		       scm_make_string(XEN_ZERO, SCM_MAKE_CHAR(0)),
		       SCM_OPN | SCM_WRTNG,
		       "snd error handler");
#else
  port = scm_mkstrport(XEN_ZERO, 
		       scm_make_string(XEN_ZERO, XEN_UNDEFINED),
		       SCM_OPN | SCM_WRTNG,
		       "snd error handler");
#endif

  if ((DEBUGGING) || (ss->batch_mode))
    {
      /* force out an error before possible backtrace call */
      XEN lport;
#ifdef SCM_MAKE_CHAR
      lport = scm_mkstrport(XEN_ZERO, 
			    scm_make_string(XEN_ZERO, SCM_MAKE_CHAR(0)),
			    SCM_OPN | SCM_WRTNG,
			    "snd error handler");
#else
      lport = scm_mkstrport(XEN_ZERO, 
			    scm_make_string(XEN_ZERO, XEN_UNDEFINED),
			    SCM_OPN | SCM_WRTNG,
			    "snd error handler");
#endif
      XEN_DISPLAY(tag, lport);
      XEN_PUTS(": ", lport);
      XEN_DISPLAY(throw_args, lport);
      XEN_FLUSH_PORT(lport);
      fprintf(stderr, XEN_TO_C_STRING(XEN_PORT_TO_STRING(lport)));
    }

  if ((XEN_LIST_P(throw_args)) && 
      (XEN_LIST_LENGTH(throw_args) > 0))
    {
      if (XEN_NOT_FALSE_P(XEN_CAR(throw_args)))
	{
	  XEN_DISPLAY(XEN_CAR(throw_args), port);
	  XEN_PUTS(": ", port);
	}
      if (XEN_LIST_LENGTH(throw_args) > 1)
	{
	  if (XEN_EQ_P(tag, NO_SUCH_FILE))
	    {
	      XEN_DISPLAY(tag, port);
	      XEN_PUTS(" \"", port);
	      XEN_DISPLAY(XEN_CADR(throw_args), port);
	      XEN_PUTS("\" ", port);
	      if (XEN_LIST_LENGTH(throw_args) > 2)
		XEN_DISPLAY(XEN_CDDR(throw_args), port);
	    }
	  else
	    {
	      if ((XEN_EQ_P(tag, NO_SUCH_SOUND)) || (XEN_EQ_P(tag, NO_SUCH_MIX)) || (XEN_EQ_P(tag, NO_SUCH_MARK)) ||
		  (XEN_EQ_P(tag, NO_SUCH_MENU)) || (XEN_EQ_P(tag, NO_SUCH_REGION)) || (XEN_EQ_P(tag, MUS_MISC_ERROR)) ||
		  (XEN_EQ_P(tag, NO_SUCH_CHANNEL)) || (XEN_EQ_P(tag, NO_SUCH_EDIT)) || (XEN_EQ_P(tag, NO_SUCH_DIRECTION)) ||
		  (XEN_EQ_P(tag, NO_SUCH_AXIS_INFO)) || (XEN_EQ_P(tag, NO_SUCH_AXIS_CONTEXT)) || XEN_EQ_P(tag, BAD_TYPE) ||
		  (XEN_EQ_P(tag, CANNOT_SAVE)) || (XEN_EQ_P(tag, CANNOT_PRINT)) || (XEN_EQ_P(tag, BAD_ARITY)) ||
#if HAVE_LADSPA
		  (XEN_EQ_P(tag, NO_SUCH_PLUGIN)) || (XEN_EQ_P(tag, PLUGIN_ERROR)) ||
#endif
#if HAVE_GSL
		  (XEN_EQ_P(tag, SND_GSL_ERROR)) ||
#endif
		  (XEN_EQ_P(tag, IMPOSSIBLE_BOUNDS)) || (XEN_EQ_P(tag, NO_SUCH_SAMPLE)))
		{
		  XEN_DISPLAY(tag, port);
		  XEN_PUTS(" ", port);
		  XEN_DISPLAY(throw_args, port);
		}
	      else
		{
		  stmp = XEN_CADR(throw_args);
		  if ((XEN_STRING_P(stmp)) && (XEN_LIST_LENGTH(throw_args) > 2))
		    scm_display_error_message(stmp, XEN_CADDR(throw_args), port);
		  else XEN_DISPLAY(tag, port);
		  if (show_backtrace(ss))
		    {
#if HAVE_SCM_C_DEFINE
		      stack = scm_fluid_ref(XEN_VARIABLE_REF(scm_the_last_stack_fluid_var));
#else
		      stack = scm_fluid_ref(XEN_CDR(scm_the_last_stack_fluid));
#endif
		      if (XEN_NOT_FALSE_P(stack)) 
			scm_display_backtrace(stack, port, XEN_UNDEFINED, XEN_UNDEFINED);
		    }
		}
	    }
	}
      else XEN_DISPLAY(tag, port);
    }
  else 
    {
      XEN_DISPLAY(tag, port);
      XEN_PUTS(": ", port);
      XEN_DISPLAY(throw_args, port);
    }
  possible_code = (char *)data;
  if ((possible_code) && 
      (snd_strlen(possible_code) < PRINT_BUFFER_SIZE))
    {
      /* not actually sure if this is always safe */
      XEN_PUTS("\n; ", port);
      XEN_PUTS(possible_code, port);
    }
#if HAVE_SCM_PORT_P
  if ((XEN_TRUE_P(scm_port_p(scm_current_load_port()))) &&
      (XEN_INTEGER_P(scm_port_line(scm_current_load_port()))))
    {
      /* this actually isn't very useful since it points to the end of the enclosing form */
      char *info;
      info = (char *)CALLOC(1024, sizeof(char));
      sprintf(info, " (%s: line %d)", 
	      filename_without_home_directory(XEN_TO_C_STRING(scm_port_filename(scm_current_load_port()))),
	      XEN_TO_C_INT(scm_port_line(scm_current_load_port())));
      XEN_PUTS(info, port);
      FREE(info);
    }
  else
#endif
    {
      if (last_file_loaded)
	{
	  XEN_PUTS("\n(while loading \"", port);
	  XEN_PUTS(last_file_loaded, port);
	  XEN_PUTS("\")", port);
	  last_file_loaded = NULL;
	}
    }
  XEN_FLUSH_PORT(port); /* needed to get rid of trailing garbage chars?? -- might be pointless now */
  name_buf = XEN_TO_C_STRING(XEN_PORT_TO_STRING(port));
  if (send_error_output_to_stdout)
    string_to_stdout(ss, name_buf);
  else
    {
      if (listener_height() > 5)
	listener_append_and_prompt(name_buf);
      else 
	{
	  if (ss->mx_sp)
	    {
	      sp = ss->mx_sp;
	      clear_minibuffer_prompt(sp);
	      report_in_minibuffer(sp, name_buf);
	      add_to_error_history(ss, name_buf, FALSE);
	    }
	  else snd_error(name_buf);
	}
    }
  check_for_event(ss);
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
#if HAVE_SCM_T_CATCH_BODY
				     scm_t_catch_handler handler,
#else
				     scm_catch_handler_t handler,
#endif
				     void *handler_data)
{ /* declaration from libguile/throw */
  XEN result;
  snd_state *ss;
  ss = get_global_state();
  if (ss->catch_exists < 0) ss->catch_exists = 0;
  ss->catch_exists++;
  /* one function can invoke, for example, a hook that will call back here setting up a nested catch */
  result = scm_internal_stack_catch(tag, body, body_data, handler, handler_data);
  if (ss->catch_exists > 0) ss->catch_exists--;
  return(result);
}

XEN snd_throw(XEN key, XEN args)
{
  snd_state *ss;
  ss = get_global_state();
  if (ss->catch_exists)
    return(scm_throw(key, args));
  else
    {
      snd_error("%s: %s", 
		XEN_AS_STRING(key),
		XEN_AS_STRING(args));
    }
  return(XEN_FALSE);
}

XEN snd_catch_any(XEN_CATCH_BODY_TYPE body, void *body_data, const char *caller)
{
  return(snd_internal_stack_catch(XEN_TRUE, body, body_data, snd_catch_scm_error, (void *)caller));
}
#else
XEN snd_catch_any(XEN_CATCH_BODY_TYPE body, void *body_data, const char *caller)
{
  return((*body)(body_data));
}
#endif

char *procedure_ok(XEN proc, int args, const char *caller, const char *arg_name, int argn)
{
  /* if string returned, needs to be freed */
  XEN arity;
  int rargs, oargs, restargs;
  if (!(XEN_PROCEDURE_P(proc)))
    {
      if (XEN_NOT_FALSE_P(proc)) /* #f as explicit arg to clear */
	return(mus_format(_("%s: %s (%s arg %d) is not a procedure!"), 
			  XEN_AS_STRING(proc),
			  arg_name, caller, argn));
    }
  else
    {
      arity = XEN_ARITY(proc);
#if HAVE_RUBY
      rargs = XEN_TO_C_INT(arity);
      if ((rargs > args) ||
	  ((rargs < 0) && (-rargs > args)))
	return(mus_format(_("%s function (%s arg %d) should take %d args, not %d"), 
			  arg_name, caller, argn, args, (rargs < 0) ? (-rargs) : rargs));
#else
      snd_protect(arity);
      rargs = XEN_TO_SMALL_C_INT(XEN_CAR(arity));
      oargs = XEN_TO_SMALL_C_INT(XEN_CADR(arity));
      restargs = ((XEN_TRUE_P(XEN_CADDR(arity))) ? 1 : 0);
      snd_unprotect(arity);
      if (rargs > args)
	return(mus_format(_("%s function (%s arg %d) should take %d argument%s, but instead requires %d"),
			  arg_name, caller, argn, args, (args != 1) ? "s" : "", rargs));
      if ((restargs == 0) && ((rargs + oargs) < args))
	return(mus_format(_("%s function (%s arg %d) should accept at least %d argument%s, but instead accepts only %d"),
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
  return(XEN_EVAL_FORM((XEN)data));
}

static XEN string_to_form_1(void *data)
{
  return(C_STRING_TO_XEN_FORM((char *)data));
}

XEN string_to_form(void *str)
{
  return(snd_catch_any(string_to_form_1, (void *)str, (const char *)str));  /* catch needed else #< in input (or incomplete form) exits Snd! */
}

static XEN eval_file_wrapper(void *data)
{
  XEN error;
  last_file_loaded = (char *)data;
  error = XEN_LOAD_FILE((char *)data); /* error only meaningful in Ruby */
  last_file_loaded = NULL;
  return(error);
}

#if HAVE_GUILE
static XEN g_call0_1(void *arg)
{
  return(scm_apply((XEN)arg, XEN_EMPTY_LIST, XEN_EMPTY_LIST));
}
#endif

XEN g_call0_unprotected(XEN proc)
{
#if HAVE_GUILE
  return(scm_apply(proc, XEN_EMPTY_LIST, XEN_EMPTY_LIST));
#else
  return(proc);
#endif
}

XEN g_call0(XEN proc, const char *caller) /* replacement for gh_call0 -- protect ourselves from premature exit(!$#%@$) */
{
#if HAVE_GUILE
  return(snd_catch_any(g_call0_1, (void *)proc, caller));
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

XEN g_call1_unprotected(XEN proc, XEN arg)
{
#if HAVE_GUILE
  return(scm_apply(proc, arg, XEN_APPLY_ARG_LIST_END));
#else
  return(arg);
#endif
}

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

XEN g_call_any_unprotected(XEN proc, XEN arglist)
{
#if HAVE_GUILE
  return(scm_apply(proc, arglist, XEN_EMPTY_LIST));
#else
  return(arglist);
#endif
}

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

XEN g_call2_unprotected(XEN proc, XEN arg1, XEN arg2)
{
#if HAVE_GUILE
  return(scm_apply(proc, arg1, XEN_CONS(arg2, XEN_APPLY_ARG_LIST_END)));
#else
  return(arg1);
#endif
}

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

XEN g_call3_unprotected(XEN proc, XEN arg1, XEN arg2, XEN arg3)
{
#if HAVE_GUILE
  return(scm_apply(proc, arg1, XEN_CONS_2(arg2, arg3, XEN_APPLY_ARG_LIST_END)));
#else
  return(arg1);
#endif
}

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

char *g_print_1(XEN obj) /* don't free return val */
{
  char *str1 = NULL;
#if HAVE_GUILE
#if HAVE_SCM_OBJECT_TO_STRING
  return(XEN_AS_STRING(obj)); 
#else
  XEN str; XEN val;
  XEN port;
  str = scm_makstr (0, 0);
  port = scm_mkstrport (XEN_ZERO, str, SCM_OPN | SCM_WRTNG, "snd-print");
  scm_prin1(obj, port, 1);
  val = XEN_PORT_TO_STRING(port);
  XEN_CLOSE_PORT(port);
  str1 = XEN_TO_C_STRING(val);
#endif
#endif
#if HAVE_RUBY
  if (XEN_NULL_P(obj))
    return("nil"); /* Ruby returns the null string in this case??? */
  return(XEN_AS_STRING(obj));
#endif
  return(str1);
}

static char *gl_print(XEN result)
{
  char *newbuf = NULL, *str = NULL;
  int i, ilen, savelen, savectr, slen;
  snd_state *ss;
  ss = get_global_state();
  /* specialize vectors which can be enormous in this context */
  if ((!(XEN_VECTOR_P(result))) || 
      ((int)(XEN_VECTOR_LENGTH(result)) <= print_length(ss)))
    return(copy_string(g_print_1(result)));
  ilen = print_length(ss); 
  newbuf = (char *)CALLOC(128, sizeof(char));
  savelen = 128;
  savectr = 3;
  sprintf(newbuf, "#("); 
  for (i = 0; i < ilen; i++)
    {
      str = g_print_1(XEN_VECTOR_REF(result, i));
      if ((str) && (*str)) 
	{
	  slen = strlen(str);
	  if ((slen + savectr + 1) >= savelen)
	    {
	      savelen += (slen + 128);
	      newbuf = (char *)REALLOC(newbuf, savelen * sizeof(char));
	    }
	  if (i != 0) 
	    {
	      strcat(newbuf, " "); 
	      savectr++;
	    }
	  strcat(newbuf, str);
	  savectr += slen;
	}
    }
  if (savectr + 8 > savelen) 
    newbuf = (char *)REALLOC(newbuf, (savectr + 8) * sizeof(char));
  strcat(newbuf, " ...)");
  return(newbuf);
}

XEN snd_eval_str(snd_state *ss, char *buf)
{
  return(snd_report_result(ss, snd_catch_any(eval_str_wrapper, (void *)buf, buf), buf));
}

XEN snd_report_result(snd_state *ss, XEN result, char *buf)
{
  snd_info *sp = NULL;
  char *str = NULL;
  str = gl_print(result);
  if (ss->mx_sp)
    {
      sp = ss->mx_sp;
      clear_minibuffer_prompt(sp);
      report_in_minibuffer(sp, str);
    }
  if (listener_height() > 5)
    {
      if (buf) listener_append(buf);
      listener_append_and_prompt(str);
    }
  if (str) FREE(str);
  return(result);
}

XEN snd_report_listener_result(XEN form)
{
  char *str = NULL;
  XEN result;
  listener_append("\n");
#if HAVE_RUBY
  str = gl_print(form);
  result = form;
#else
  result = snd_catch_any(eval_form_wrapper, (void *)form, NULL);
  str = gl_print(result);
#endif
  if (listener_height() > 5)
    listener_append_and_prompt(str);
  if (str) FREE(str);
  return(result);
}

void snd_eval_property_str(char *buf)
{
  XEN result;
  char *str;
  if ((snd_strlen(buf) == 0) || ((snd_strlen(buf) == 1) && (buf[0] == '\n'))) return;
  result = snd_catch_any(eval_str_wrapper, (void *)buf, buf);
  str = gl_print(result);
  listener_append_and_prompt(str);
  if (str) FREE(str);
}

static char *stdin_str = NULL;

void clear_stdin(void)
{
  if (stdin_str) FREE(stdin_str);
  stdin_str = NULL;
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
#if HAVE_GUILE
  end_of_text = check_balance(stdin_str, 0, snd_strlen(stdin_str), FALSE); /* last-arg->not in listener */
  if (end_of_text > 0)
    {
      if (end_of_text + 1 < snd_strlen(stdin_str))
	stdin_str[end_of_text + 1] = 0;
      return(stdin_str);
    }
  else return(NULL);
#else
  return(stdin_str);
#endif
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
      send_error_output_to_stdout = TRUE;
      result = snd_catch_any(eval_str_wrapper, (void *)str, str);
      send_error_output_to_stdout = FALSE;
      if (stdin_str) FREE(stdin_str);
      /* same as str here; if c-g! evaluated from stdin, clear_listener is called which frees/nullifies stdin_str */
      stdin_str = NULL;
      str = gl_print(result);
      string_to_stdout(ss, str);
      if (str) FREE(str);
    }
}

void snd_load_init_file(snd_state *ss, int no_global, int no_init)
{
  /* look for ".snd" on the home directory */
  /* called only in snd-xmain.c at initialization time */
  int fd;
  XEN result = XEN_TRUE;
  char *str = NULL;
  #define SND_CONF "/etc/snd.conf"
  if (no_global == 0)
    {
      fd = OPEN(SND_CONF, O_RDONLY, 0);
      if (fd != -1)
	{
	  snd_close(fd, SND_CONF);
	  result = snd_catch_any(eval_file_wrapper, (void *)SND_CONF, "(load " SND_CONF ")");
	}
    }
  if ((ss->init_file) && (no_init == 0))
    {
      str = mus_expand_filename(ss->init_file);
      fd = OPEN(str, O_RDONLY, 0);
      if (fd != -1) 
	{
	  snd_close(fd, str);
	  result = snd_catch_any(eval_file_wrapper, (void *)str, "(load ~/.snd)");
	}
      if (str) FREE(str);
    }
#if HAVE_RUBY
  if (!(XEN_TRUE_P(result)))
    {
      str = gl_print(result);
      if (str)
	{
	  snd_error(str);
	  FREE(str);
	}
    }
#endif
}

void snd_load_file(char *filename)
{
  char *str = NULL, *str1 = NULL, *str2 = NULL;
  XEN result = XEN_TRUE;
  str = mus_expand_filename(filename);
#if (!HAVE_RUBY)
  str2 = (char *)CALLOC(snd_strlen(filename) + 16, sizeof(char));
  sprintf(str2, "(load \"%s\")", filename);
#endif
  if (!mus_file_probe(str))
    {
      /* try tacking on extension */
      str1 = (char *)CALLOC(snd_strlen(str) + 2 + strlen(XEN_FILE_EXTENSION), sizeof(char));
      sprintf(str1, "%s.%s", str, XEN_FILE_EXTENSION);
      if (!mus_file_probe(str1))
	{
	  FREE(str);
	  str = NULL;
	  FREE(str1);
	  str1 = NULL;
	  FREE(str2);
	  str2 = NULL;
	  snd_error(_("can't load %s: %s"), filename, strerror(errno));
	}
      /* snd_error ok here because all uses of this are user-interface generated (autoload, memo-file, etc) */
      else result = snd_catch_any(eval_file_wrapper, (void *)str1, str2);
      if (str1) FREE(str1);
    }
  else result = snd_catch_any(eval_file_wrapper, (void *)str, str2);
  if (str) FREE(str);
  if (str2) FREE(str2);
#if HAVE_RUBY
  if (!(XEN_TRUE_P(result)))
    {
      str = gl_print(result);
      if (str)
	{
	  snd_error(str);
	  FREE(str);
	}
    }
#endif
}

static XEN g_snd_print(XEN msg)
{
  #define H_snd_print "(" S_snd_print " str) displays str in the lisp listener window"
  char *str = NULL;
  if (XEN_STRING_P(msg))
    str = copy_string(XEN_TO_C_STRING(msg));
  else
    {
      if (XEN_CHAR_P(msg))
	{
	  str = (char *)CALLOC(2, sizeof(char));
	  str[0] = XEN_TO_C_CHAR(msg);
	}
      else str = gl_print(msg);
    }
  listener_append(str);
  if (str) FREE(str);
#if (!USE_GTK)
  check_for_event(get_global_state());
#endif
  return(msg);
}

static XEN print_hook;
static int print_depth = 0;

int listener_print_p(char *msg)
{
  XEN res = XEN_FALSE;
  if ((msg) && (print_depth == 0) && (strlen(msg) > 0) && (XEN_HOOKED(print_hook)))
    {
      print_depth++;
      res = run_or_hook(print_hook, 
			XEN_LIST_1(C_TO_XEN_STRING(msg)),
			S_print_hook);
      print_depth--;
    }
 return(XEN_FALSE_P(res));
}


/* -------- global variables -------- */

static XEN g_region_graph_style(void) {return(C_TO_XEN_INT(region_graph_style(get_global_state())));}
static XEN g_set_region_graph_style(XEN val) 
{
  #define H_region_graph_style "(" S_region_graph_style ") refers to the graph-style of the region dialog graph. \
The region-graph-style choices are graph-lines, graph-dots, graph-filled, graph-lollipops, and graph-dots-and-lines."
  snd_state *ss;
  ss = get_global_state();
  XEN_ASSERT_TYPE(XEN_INTEGER_P(val), val, XEN_ONLY_ARG, S_setB S_region_graph_style, "an integer");
  set_region_graph_style(ss, XEN_TO_C_INT(val));
  reflect_region_graph_style(ss);
  return(val);
}

static XEN g_ask_before_overwrite(void) {return(C_TO_XEN_BOOLEAN(ask_before_overwrite(get_global_state())));}
static XEN g_set_ask_before_overwrite(XEN val) 
{
  #define H_ask_before_overwrite "(" S_ask_before_overwrite ") should be #t if you want Snd to ask before overwriting a file. \
If #f, any existing file will be overwritten when you save or save-as."
  snd_state *ss;
  ss = get_global_state();
  XEN_ASSERT_TYPE(XEN_BOOLEAN_IF_BOUND_P(val), val, XEN_ONLY_ARG, S_setB S_ask_before_overwrite, "a boolean");
  set_ask_before_overwrite(ss, XEN_TO_C_BOOLEAN_OR_TRUE(val)); 
  return(C_TO_XEN_BOOLEAN(ask_before_overwrite(ss)));
}

static XEN g_audio_output_device(void) {return(C_TO_XEN_INT(audio_output_device(get_global_state())));}
static XEN g_set_audio_output_device(XEN val) 
{
  #define H_audio_output_device "(" S_audio_output_device ") is the current sndlib default output device (mus-audio-default)"
  snd_state *ss;
  ss = get_global_state();
  XEN_ASSERT_TYPE(XEN_INTEGER_P(val), val, XEN_ONLY_ARG, S_setB S_audio_output_device, "an integer"); 
  set_audio_output_device(ss, XEN_TO_C_INT(val)); 
  return(C_TO_XEN_INT(audio_output_device(ss)));
}

static XEN g_audio_input_device(void) {return(C_TO_XEN_INT(audio_input_device(get_global_state())));}
static XEN g_set_audio_input_device(XEN val) 
{
  #define H_audio_input_device "(" S_audio_input_device ") is the current sndlib default input device (mus-audio-default)"
  snd_state *ss;
  ss = get_global_state();
  XEN_ASSERT_TYPE(XEN_INTEGER_P(val), val, XEN_ONLY_ARG, S_setB S_audio_input_device, "an integer"); 
  set_audio_input_device(ss, XEN_TO_C_INT(val)); 
  return(C_TO_XEN_INT(audio_input_device(ss)));
}

static XEN g_minibuffer_history_length(void) {return(C_TO_XEN_INT(minibuffer_history_length(get_global_state())));}
static XEN g_set_minibuffer_history_length(XEN val) 
{
  #define H_minibuffer_history_length "(" S_minibuffer_history_length ") is the minibuffer history length. \
This pertains to the M-p and M-n commands."
  int len;
  snd_state *ss;
  ss = get_global_state();
  XEN_ASSERT_TYPE(XEN_INTEGER_P(val), val, XEN_ONLY_ARG, S_setB S_minibuffer_history_length, "an integer");
  len = XEN_TO_C_INT(val);
  if (len > 0)
    set_minibuffer_history_length(ss, len);
  return(C_TO_XEN_INT(minibuffer_history_length(ss)));
}

static XEN g_emacs_style_save_as(void) {return(C_TO_XEN_BOOLEAN(emacs_style_save_as(get_global_state())));}
static XEN g_set_emacs_style_save_as(XEN val) 
{
  #define H_emacs_style_save_as "(" S_emacs_style_save_as ") #t if File:Save-as dialog option should move to the new file (default: #f)"
  snd_state *ss;
  ss = get_global_state();
  XEN_ASSERT_TYPE(XEN_BOOLEAN_IF_BOUND_P(val), val, XEN_ONLY_ARG, S_setB S_emacs_style_save_as, "a boolean");
  set_emacs_style_save_as(ss, XEN_TO_C_BOOLEAN_OR_TRUE(val)); 
  return(C_TO_XEN_BOOLEAN(emacs_style_save_as(ss)));
}

static XEN g_auto_resize(void) {return(C_TO_XEN_BOOLEAN(auto_resize(get_global_state())));}
static XEN g_set_auto_resize(XEN val) 
{
  #define H_auto_resize "(" S_auto_resize ") should be #t if Snd can change its main window size as it pleases (default: #t)"
  snd_state *ss;
  ss = get_global_state();
  XEN_ASSERT_TYPE(XEN_BOOLEAN_IF_BOUND_P(val), val, XEN_ONLY_ARG, S_setB S_auto_resize, "a boolean");
  set_auto_resize(ss, XEN_TO_C_BOOLEAN_OR_TRUE(val)); 
  reflect_resize(ss); 
  return(C_TO_XEN_BOOLEAN(auto_resize(ss)));
}

static XEN g_auto_update(void) {return(C_TO_XEN_BOOLEAN(auto_update(get_global_state())));}
static XEN g_set_auto_update(XEN val) 
{
  #define H_auto_update "(" S_auto_update ") -> #t if Snd should automatically update a file if it changes unexpectedly (default: #f). \
The number of seconds between update checks is set by auto-update-interval."
  snd_state *ss;
  ss = get_global_state();
  XEN_ASSERT_TYPE(XEN_BOOLEAN_IF_BOUND_P(val), val, XEN_ONLY_ARG, S_setB S_auto_update, "a boolean");
  set_auto_update(ss, XEN_TO_C_BOOLEAN_OR_TRUE(val)); 
  return(C_TO_XEN_BOOLEAN(auto_update(ss)));
}

static XEN g_filter_env_in_hz(void) {return(C_TO_XEN_BOOLEAN(filter_env_in_hz(get_global_state())));}
static XEN g_set_filter_env_in_hz(XEN val) 
{
  #define H_filter_env_in_hz "(" S_filter_env_in_hz ") -> #t if filter env x axis should be in Hz"
  snd_state *ss;
  ss = get_global_state();
  XEN_ASSERT_TYPE(XEN_BOOLEAN_IF_BOUND_P(val), val, XEN_ONLY_ARG, S_setB S_filter_env_in_hz, "a boolean");
  set_filter_env_in_hz(ss, XEN_TO_C_BOOLEAN_OR_TRUE(val)); 
  return(C_TO_XEN_BOOLEAN(filter_env_in_hz(ss)));
}

static XEN g_color_cutoff(void) {return(C_TO_XEN_DOUBLE(color_cutoff(get_global_state())));}
static XEN g_set_color_cutoff(XEN val) 
{
  #define H_color_cutoff "(" S_color_cutoff ") -> color map cutoff point (default .003).  Any values \
below the cutoff are displayed in the background color"
  snd_state *ss;
  ss = get_global_state();
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ONLY_ARG, S_setB S_color_cutoff, "a number");
  set_color_cutoff(ss, mus_fclamp(0.0,
				     XEN_TO_C_DOUBLE(val),
				     0.25)); 
  return(C_TO_XEN_DOUBLE(color_cutoff(ss)));
}

static XEN g_color_inverted(void) {return(C_TO_XEN_BOOLEAN(color_inverted(get_global_state())));}
static XEN g_set_color_inverted(XEN val) 
{
  #define H_color_inverted "(" S_color_inverted ") -> whether the colormap in operation should be inverted"
  snd_state *ss;
  ss = get_global_state();
  XEN_ASSERT_TYPE(XEN_BOOLEAN_IF_BOUND_P(val), val, XEN_ONLY_ARG, S_setB S_color_inverted, "a boolean");
  set_color_inverted(ss, XEN_TO_C_BOOLEAN_OR_TRUE(val)); 
  return(C_TO_XEN_BOOLEAN(color_inverted(ss)));
}

static XEN g_color_scale(void) {return(C_TO_XEN_DOUBLE(color_scale(get_global_state())));}
static XEN g_set_color_scale(XEN val) 
{
  #define H_color_scale "(" S_color_scale ") -> essentially a darkness setting for colormaps (0.5)"
  snd_state *ss;
  ss = get_global_state();
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ONLY_ARG, S_setB S_color_scale, "a number"); 
  set_color_scale(ss, mus_fclamp(0.0,
				    XEN_TO_C_DOUBLE(val),
				    1000.0)); 
  return(C_TO_XEN_DOUBLE(color_scale(ss)));
}

static XEN g_auto_update_interval(void) {return(C_TO_XEN_DOUBLE(auto_update_interval(get_global_state())));}
static XEN g_set_auto_update_interval(XEN val) 
{
  Float ctime;
  #define H_auto_update_interval "(" S_auto_update_interval ") -> time (seconds) between background checks for changed file on disk (default: 60). \
This value only matters if auto-update is #t"
  snd_state *ss;
  ss = get_global_state();
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ONLY_ARG, S_setB S_auto_update_interval, "a number"); 
  ctime = XEN_TO_C_DOUBLE(val);
  if ((ctime < 0.0) || (ctime > (24 * 3600)))
    mus_misc_error(S_setB S_auto_update_interval, "invalid time:", val);
  set_auto_update_interval(ss, XEN_TO_C_DOUBLE(val)); 
  return(C_TO_XEN_DOUBLE(auto_update_interval(ss)));
}

static XEN g_default_output_chans(void) {return(C_TO_XEN_INT(default_output_chans(get_global_state())));}
static XEN g_set_default_output_chans(XEN val) 
{
  #define H_default_output_chans "(" S_default_output_chans ") -> default number of channels when a new or temporary file is created (1)"
  snd_state *ss;
  ss = get_global_state();
  XEN_ASSERT_TYPE(XEN_INTEGER_P(val), val, XEN_ONLY_ARG, S_setB S_default_output_chans, "an integer"); 
  set_default_output_chans(ss, XEN_TO_C_INT(val));
  return(C_TO_XEN_INT(default_output_chans(ss)));
}

static XEN g_default_output_srate(void) {return(C_TO_XEN_INT(default_output_srate(get_global_state())));}
static XEN g_set_default_output_srate(XEN val) 
{
  #define H_default_output_srate "(" S_default_output_srate ") -> default srate when a new or temporary file is created (22050)" 
  snd_state *ss;
  ss = get_global_state();
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ONLY_ARG, S_setB S_default_output_srate, "a number"); 
  set_default_output_srate(ss, XEN_TO_C_INT_OR_ELSE(val, 0));
  return(C_TO_XEN_INT(default_output_srate(ss)));
}

static XEN g_default_output_type(void) {return(C_TO_XEN_INT(default_output_type(get_global_state())));}
static XEN g_set_default_output_type(XEN val) 
{
  int typ;
  #define H_default_output_type "(" S_default_output_type ") -> default header type when a new or temporary file is created. \
Normally this is " S_mus_next "; -1 here indicates you want Snd to use the current sound's header type, if possible. \
Other writable headers include " S_mus_aiff ", " S_mus_riff ", " S_mus_ircam ", " S_mus_nist ", " S_mus_aifc ", and " S_mus_raw "."

  snd_state *ss;
  ss = get_global_state();
  XEN_ASSERT_TYPE(XEN_INTEGER_P(val), val, XEN_ONLY_ARG, S_setB S_default_output_type, "an integer"); 
  typ = XEN_TO_C_INT(val);
  if (mus_header_writable(typ, -2))
    set_default_output_type(ss, typ); 
  else mus_misc_error(S_setB S_default_output_type, 
		      "can't write this header type", 
		      XEN_LIST_2(val, 
				 C_TO_XEN_STRING(mus_header_type_name(typ))));
  return(C_TO_XEN_INT(default_output_type(ss)));
}

static XEN g_default_output_format(void) {return(C_TO_XEN_INT(default_output_format(get_global_state())));}
static XEN g_set_default_output_format(XEN val) 
{
  int format;
  #define H_default_output_format "(" S_default_output_format ") -> default data format when a new or temporary file is created, \
normally " S_mus_bshort "; -1 here means try to use the current sound's data format; many other formats \
are available, but not all are compatible with all header types"

  snd_state *ss;
  ss = get_global_state();
  XEN_ASSERT_TYPE(XEN_INTEGER_P(val), val, XEN_ONLY_ARG, S_setB S_default_output_format, "an integer"); 
  format = XEN_TO_C_INT(val);
  set_default_output_format(ss, format); 
  return(C_TO_XEN_INT(default_output_format(ss)));
}

static XEN g_audio_state_file(void) {return(C_TO_XEN_STRING(audio_state_file(get_global_state())));}
static XEN g_set_audio_state_file(XEN val) 
{
  #define H_audio_state_file "(" S_audio_state_file ") -> filename for the mus-audio-save-state function (.snd-mixer)"
  snd_state *ss;
  ss = get_global_state();
  XEN_ASSERT_TYPE(XEN_STRING_P(val), val, XEN_ONLY_ARG, S_setB S_audio_state_file, "a string"); 
  if (audio_state_file(ss)) FREE(audio_state_file(ss));
  set_audio_state_file(ss, copy_string(XEN_TO_C_STRING(val)));
  return(C_TO_XEN_STRING(audio_state_file(ss)));
}

static XEN g_selection_creates_region(void) {return(C_TO_XEN_BOOLEAN(selection_creates_region(get_global_state())));}
static XEN g_set_selection_creates_region(XEN val) 
{
  #define H_selection_creates_region "(" S_selection_creates_region ") -> #t if a region should be created each time a selection is made. \
The default is currently #t, but that may change.  If you're dealing with large selections, and have no need of \
regions (saved selections), you can speed up many operations by setting this flag to #f"
  snd_state *ss;
  ss = get_global_state();
  XEN_ASSERT_TYPE(XEN_BOOLEAN_IF_BOUND_P(val), val, XEN_ONLY_ARG, S_setB S_selection_creates_region, "a boolean");
  set_selection_creates_region(ss, XEN_TO_C_BOOLEAN_OR_TRUE(val));
  return(C_TO_XEN_BOOLEAN(selection_creates_region(ss)));
}

static XEN g_print_length(void) {return(C_TO_XEN_INT(print_length(get_global_state())));}
static XEN g_set_print_length(XEN val) 
{
  #define H_print_length "(" S_print_length ") -> number of vector elements to print in the listener (default: 12)"
  snd_state *ss;
  ss = get_global_state();
  XEN_ASSERT_TYPE(XEN_INTEGER_P(val), val, XEN_ONLY_ARG, S_setB S_print_length, "an integer"); 
  set_print_length(ss, XEN_TO_C_INT(val)); 
  set_vct_print_length(XEN_TO_C_INT(val));
  return(C_TO_XEN_INT(print_length(ss)));
}

static XEN g_show_indices(void) {return(C_TO_XEN_BOOLEAN(show_indices(get_global_state())));}
static XEN g_set_show_indices(XEN val) 
{
  #define H_show_indices "(" S_show_indices ") -> #t if sound name should be preceded by its index"
  snd_state *ss;
  ss = get_global_state();
  XEN_ASSERT_TYPE(XEN_BOOLEAN_IF_BOUND_P(val), val, XEN_ONLY_ARG, S_setB S_show_indices, "a boolean");
  set_show_indices(ss, XEN_TO_C_BOOLEAN_OR_TRUE(val));
  return(C_TO_XEN_BOOLEAN(show_indices(ss)));
}

static XEN g_show_backtrace(void) {return(C_TO_XEN_BOOLEAN(show_backtrace(get_global_state())));}
static XEN g_set_show_backtrace(XEN val) 
{
  #define H_show_backtrace "(" S_show_backtrace ") -> #t to show backtrace automatically upon error"
  snd_state *ss;
  ss = get_global_state();
  XEN_ASSERT_TYPE(XEN_BOOLEAN_IF_BOUND_P(val), val, XEN_ONLY_ARG, S_setB S_show_backtrace, "a boolean");
  set_show_backtrace(ss, XEN_TO_C_BOOLEAN_OR_TRUE(val));
  return(C_TO_XEN_BOOLEAN(show_backtrace(ss)));
}

static XEN g_color_map(void) {return(C_TO_XEN_INT(color_map(get_global_state())));}
static XEN g_set_color_map(XEN val) 
{
  #define H_colormap "(" S_colormap ") -> current colormap choice. \
This should be an integer between -1 and 15.  The maps (from 0 to 15) are: \
gray, hsv, hot, cool, bone, copper, pink, jet, prism, autumn, winter, \
spring, summer, colorcube, flag, and lines.  -1 means black and white."

  snd_state *ss;
  ss = get_global_state();
  XEN_ASSERT_TYPE(XEN_INTEGER_P(val), val, XEN_ONLY_ARG, S_setB S_colormap, "an integer"); 
  set_color_map(ss, mus_iclamp(
#if USE_GTK
				  0,
#else
				  -1,
#endif
				  XEN_TO_C_INT(val),
				  NUM_COLORMAPS - 1));
  return(C_TO_XEN_INT(color_map(ss)));
}

static int snd_access(char *dir, char *caller)
{
  int err;
  char *temp;
  XEN res;
  temp = shorter_tempnam(dir, "snd_");
  err = mus_file_create(temp);
  if (err == -1)
    {
      FREE(temp);
      temp = mus_format(_("%s: directory %s is not writable: %s"), caller, dir, strerror(errno));
      res = C_TO_XEN_STRING(temp);
      FREE(temp);
      XEN_ERROR(NO_SUCH_FILE,
		XEN_LIST_1(res));
    }
  else snd_close(err, temp);
  snd_remove(temp, FALSE);
  FREE(temp);
  return(1);
}

static XEN g_temp_dir(void) {return(C_TO_XEN_STRING(temp_dir(get_global_state())));}
static XEN g_set_temp_dir(XEN val) 
{
  #define H_temp_dir "(" S_temp_dir ") -> name of directory for temp files (or #f=null)"
  char *dir = DEFAULT_TEMP_DIR;
  snd_state *ss;
  ss = get_global_state();
  XEN_ASSERT_TYPE(XEN_STRING_P(val) || XEN_FALSE_P(val), val, XEN_ONLY_ARG, S_setB S_temp_dir, "a string or #f=default (null)"); 
  if (XEN_STRING_P(val)) dir = XEN_TO_C_STRING(val);
  if (snd_access(dir, S_temp_dir))
    {
      if (temp_dir(ss)) FREE(temp_dir(ss));
      set_temp_dir(ss, copy_string(dir));
    }
  return(C_TO_XEN_STRING(temp_dir(ss)));
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

static XEN g_save_dir(void) {return(C_TO_XEN_STRING(save_dir(get_global_state())));}
static XEN g_set_save_dir(XEN val) 
{
  #define H_save_dir "(" S_save_dir ") -> name of directory for saved state data (or #f=null)"
  char *dir = DEFAULT_SAVE_DIR;
  snd_state *ss;
  ss = get_global_state();
  XEN_ASSERT_TYPE(XEN_STRING_P(val) || XEN_FALSE_P(val), val, XEN_ONLY_ARG, S_setB S_save_dir, "a string or #f=default (null)"); 
  if (XEN_STRING_P(val)) dir = XEN_TO_C_STRING(val);
  if (snd_access(dir, S_save_dir))
    {
      if (save_dir(ss)) FREE(save_dir(ss));
      set_save_dir(ss, copy_string(dir));
    }
  return(C_TO_XEN_STRING(save_dir(ss)));
}

static XEN g_ladspa_dir(void) {return(C_TO_XEN_STRING(ladspa_dir(get_global_state())));}
static XEN g_set_ladspa_dir(XEN val) 
{
  #define H_ladspa_dir "(" S_ladspa_dir ") -> name of directory for ladspa plugin libraries"
  snd_state *ss;
  ss = get_global_state();
  XEN_ASSERT_TYPE(XEN_STRING_P(val) || XEN_FALSE_P(val), val, XEN_ONLY_ARG, S_setB S_ladspa_dir, "a string or #f=default (null)"); 
  if (ladspa_dir(ss)) FREE(ladspa_dir(ss));
  if (XEN_FALSE_P(val))
    set_ladspa_dir(ss, (DEFAULT_LADSPA_DIR) ? copy_string(DEFAULT_LADSPA_DIR) : NULL);
  else set_ladspa_dir(ss, copy_string(XEN_TO_C_STRING(val)));
  return(C_TO_XEN_STRING(ladspa_dir(ss)));
}

static XEN g_trap_segfault(void) {return(C_TO_XEN_BOOLEAN(trap_segfault(get_global_state())));}
static XEN g_set_trap_segfault(XEN val) 
{
  #define H_trap_segfault "(" S_trap_segfault ") -> #t if Snd should try to trap (and whine about) segfaults"
  snd_state *ss;
  ss = get_global_state();
  XEN_ASSERT_TYPE(XEN_BOOLEAN_IF_BOUND_P(val), val, XEN_ONLY_ARG, S_setB S_trap_segfault, "a boolean");
  set_trap_segfault(ss, XEN_TO_C_BOOLEAN_OR_TRUE(val));
  return(C_TO_XEN_BOOLEAN(trap_segfault(ss)));
}

static XEN g_show_selection_transform(void) {return(C_TO_XEN_BOOLEAN(show_selection_transform(get_global_state())));}
static XEN g_set_show_selection_transform(XEN val) 
{
  #define H_show_selection_transform "(" S_show_selection_transform ") -> #t if transform display reflects selection, not time-domain window"
  snd_state *ss;
  ss = get_global_state();
  XEN_ASSERT_TYPE(XEN_BOOLEAN_IF_BOUND_P(val), val, XEN_ONLY_ARG, S_setB S_show_selection_transform, "a boolean");
  set_show_selection_transform(ss, XEN_TO_C_BOOLEAN_OR_TRUE(val));
  return(C_TO_XEN_BOOLEAN(show_selection_transform(ss)));
}

static XEN g_with_gl(void) {return(C_TO_XEN_BOOLEAN(with_gl(get_global_state())));}
static XEN g_set_with_gl(XEN val) 
{
  #define H_with_gl "(" S_with_gl ") -> #t if Snd should use GL graphics"
  snd_state *ss;
  ss = get_global_state();
  XEN_ASSERT_TYPE(XEN_BOOLEAN_IF_BOUND_P(val), val, XEN_ONLY_ARG, S_setB S_with_gl, "a boolean");
#if HAVE_GL
  set_with_gl(ss, XEN_TO_C_BOOLEAN_OR_TRUE(val));
  for_each_chan(ss, update_graph);
#endif
  return(C_TO_XEN_BOOLEAN(with_gl(ss)));
}

static XEN g_with_relative_panes(void) {return(C_TO_XEN_BOOLEAN(with_relative_panes(get_global_state())));}
static XEN g_set_with_relative_panes(XEN val) 
{
  #define H_with_relative_panes "(" S_with_relative_panes ") -> #t if multichannel sounds should try to maintain relative pane sizes"
  snd_state *ss;
  ss = get_global_state();
  XEN_ASSERT_TYPE(XEN_BOOLEAN_IF_BOUND_P(val), val, XEN_ONLY_ARG, S_setB S_with_relative_panes, "a boolean");
  set_with_relative_panes(ss, XEN_TO_C_BOOLEAN_OR_TRUE(val));
  return(C_TO_XEN_BOOLEAN(with_relative_panes(ss)));
}

static XEN g_with_background_processes(void) {return(C_TO_XEN_BOOLEAN(with_background_processes(get_global_state())));}
static XEN g_set_with_background_processes(XEN val) 
{
  #define H_with_background_processes "(" S_with_background_processes ") -> #t if Snd should use background (idle time) processing"
  snd_state *ss;
  ss = get_global_state();
  if ((XEN_INTEGER_P(val)) && (XEN_TO_C_INT(val) == DISABLE_BACKGROUND_PROCESSES))
    {
      set_with_background_processes(ss, DISABLE_BACKGROUND_PROCESSES);
      return(C_STRING_TO_XEN_SYMBOL("internal-testing"));
    }
  else
    {
      XEN_ASSERT_TYPE(XEN_BOOLEAN_IF_BOUND_P(val), val, XEN_ONLY_ARG, S_setB S_with_background_processes, "a boolean");
      set_with_background_processes(ss, XEN_TO_C_BOOLEAN_OR_TRUE(val));
      return(C_TO_XEN_BOOLEAN(with_background_processes(ss)));
    }
}

static XEN g_use_sinc_interp(void) {return(C_TO_XEN_BOOLEAN(use_sinc_interp(get_global_state())));}
static XEN g_set_use_sinc_interp(XEN val) 
{
  #define H_use_sinc_interp "(" S_use_sinc_interp ") -> #t if Snd should use convolution with sinc for sampling rate \
conversion.  The other choice is (much faster) linear interpolation which can introduce distortion"

  snd_state *ss;
  ss = get_global_state();
  XEN_ASSERT_TYPE(XEN_BOOLEAN_IF_BOUND_P(val), val, XEN_ONLY_ARG, S_setB S_use_sinc_interp, "a boolean");
  set_use_sinc_interp(ss, XEN_TO_C_BOOLEAN_OR_TRUE(val));
  return(C_TO_XEN_BOOLEAN(use_sinc_interp(ss)));
}

/* data-clipped -> clip-data? -- this is from sndlib */
static XEN g_data_clipped(void) {return(C_TO_XEN_BOOLEAN(data_clipped(get_global_state())));}
static XEN g_set_data_clipped(XEN val) 
{
  #define H_data_clipped "(" S_data_clipped ") -> #t if Snd should clip output values to the current \
output data format's maximum. The default (#f) allows them to wrap-around which makes a very loud click"

  snd_state *ss;
  ss = get_global_state();
  XEN_ASSERT_TYPE(XEN_BOOLEAN_IF_BOUND_P(val), val, XEN_ONLY_ARG, S_setB S_data_clipped, "a boolean");
  set_data_clipped(ss, XEN_TO_C_BOOLEAN_OR_TRUE(val));
  return(C_TO_XEN_BOOLEAN(data_clipped(ss)));
}

static XEN g_zoom_focus_style(void) {return(C_TO_XEN_INT(zoom_focus_style(get_global_state())));}
static XEN g_set_zoom_focus_style(XEN focus) 
{
  #define H_zoom_focus_style "(" S_zoom_focus_style ") -> one of '(" S_zoom_focus_left " " S_zoom_focus_right " " S_zoom_focus_middle " " S_zoom_focus_active ")\n\
  decides what zooming centers on (default: " S_zoom_focus_active ")"
  snd_state *ss;
  ss = get_global_state();
  XEN_ASSERT_TYPE(XEN_INTEGER_P(focus), focus, XEN_ONLY_ARG, S_setB S_zoom_focus_style, "an integer"); 
  activate_focus_menu(ss, mus_iclamp(ZOOM_FOCUS_LEFT,
					XEN_TO_C_INT(focus),
					ZOOM_FOCUS_MIDDLE));
  return(C_TO_XEN_INT(zoom_focus_style(ss)));
}

static XEN g_snd_version(void) 
{
  #define H_snd_version "(" S_snd_version ") -> current Snd version"
  return(C_TO_XEN_STRING(SND_VERSION));
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
  snd_state *ss;
  ss = get_global_state();
  if (XEN_NOT_BOUND_P(snd))
    equalize_all_panes(ss); 
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

static XEN g_help_text_font(void) {return(C_TO_XEN_STRING(help_text_font(get_global_state())));}
static XEN g_set_help_text_font(XEN val) 
{
  #define H_help_text_font "(" S_help_text_font ") -> font used in the Help dialog"
  snd_state *ss;
  ss = get_global_state();
  XEN_ASSERT_TYPE(XEN_STRING_P(val), val, XEN_ONLY_ARG, S_setB S_help_text_font, "a string"); 
  set_help_text_font(ss, XEN_TO_C_STRING(val)); 
  return(C_TO_XEN_STRING(help_text_font(ss)));
}

static XEN g_tiny_font(void) {return(C_TO_XEN_STRING(tiny_font(get_global_state())));}
static XEN g_set_tiny_font(XEN val) 
{
  #define H_tiny_font "(" S_tiny_font ") -> font use for some info in the graphs"
  snd_state *ss;
  ss = get_global_state();
  XEN_ASSERT_TYPE(XEN_STRING_P(val), val, XEN_ONLY_ARG, S_setB S_tiny_font, "a string"); 
  set_tiny_font(ss, XEN_TO_C_STRING(val)); 
  return(C_TO_XEN_STRING(tiny_font(ss)));
}

static XEN g_axis_label_font(void) {return(C_TO_XEN_STRING(axis_label_font(get_global_state())));}
static XEN g_set_axis_label_font(XEN val) 
{
  #define H_axis_label_font "(" S_axis_label_font ") -> font used for axis labels"
  snd_state *ss;
  ss = get_global_state();
  XEN_ASSERT_TYPE(XEN_STRING_P(val), val, XEN_ONLY_ARG, S_setB S_axis_label_font, "a string"); 
  set_axis_label_font(ss, XEN_TO_C_STRING(val)); 
  return(C_TO_XEN_STRING(axis_label_font(ss)));
}

static XEN g_axis_numbers_font(void) {return(C_TO_XEN_STRING(axis_numbers_font(get_global_state())));}
static XEN g_set_axis_numbers_font(XEN val) 
{
  #define H_axis_numbers_font "(" S_axis_numbers_font ") -> font used for axis numbers"
  snd_state *ss;
  ss = get_global_state();
  XEN_ASSERT_TYPE(XEN_STRING_P(val), val, XEN_ONLY_ARG, S_setB S_axis_numbers_font, "a string"); 
  set_axis_numbers_font(ss, XEN_TO_C_STRING(val)); 
  return(C_TO_XEN_STRING(axis_numbers_font(ss)));
}

static XEN g_listener_font(void) {return(C_TO_XEN_STRING(listener_font(get_global_state())));}
static XEN g_set_listener_font(XEN val) 
{
  #define H_listener_font "(" S_listener_font ") -> font used by the lisp listener"
  snd_state *ss;
  ss = get_global_state();
  XEN_ASSERT_TYPE(XEN_STRING_P(val), val, XEN_ONLY_ARG, S_setB S_listener_font, "a string");
  set_listener_font(ss, XEN_TO_C_STRING(val)); 
  return(C_TO_XEN_STRING(listener_font(ss)));
}

static XEN g_bold_button_font(void) {return(C_TO_XEN_STRING(bold_button_font(get_global_state())));}
static XEN g_set_bold_button_font(XEN val) 
{
  #define H_bold_button_font "(" S_bold_button_font ") -> font used by some buttons"
  snd_state *ss;
  ss = get_global_state();
  XEN_ASSERT_TYPE(XEN_STRING_P(val), val, XEN_ONLY_ARG, S_setB S_bold_button_font, "a string"); 
  set_bold_button_font(ss, XEN_TO_C_STRING(val)); 
  return(C_TO_XEN_STRING(bold_button_font(ss)));
}

static XEN g_button_font(void) {return(C_TO_XEN_STRING(button_font(get_global_state())));}
static XEN g_set_button_font(XEN val) 
{
  #define H_button_font "(" S_button_font ") -> font used by some buttons"
  snd_state *ss;
  ss = get_global_state();
  XEN_ASSERT_TYPE(XEN_STRING_P(val), val, XEN_ONLY_ARG, S_setB S_button_font, "a string"); 
  set_button_font(ss, XEN_TO_C_STRING(val)); 
  return(C_TO_XEN_STRING(button_font(ss)));
}

static XEN g_bold_peaks_font(void) {return(C_TO_XEN_STRING(bold_peaks_font(get_global_state())));}
static XEN g_set_bold_peaks_font(XEN val) 
{
  #define H_bold_peaks_font "(" S_bold_peaks_font ") -> font used by fft peak display"
  snd_state *ss;
  ss = get_global_state();
  XEN_ASSERT_TYPE(XEN_STRING_P(val), val, XEN_ONLY_ARG, S_setB S_bold_peaks_font, "a string"); 
  set_bold_peaks_font(ss, XEN_TO_C_STRING(val)); 
  return(C_TO_XEN_STRING(bold_peaks_font(ss)));
}

static XEN g_peaks_font(void) {return(C_TO_XEN_STRING(peaks_font(get_global_state())));}
static XEN g_set_peaks_font(XEN val) 
{
  #define H_peaks_font "(" S_peaks_font ") -> font used by fft peak display"
  snd_state *ss;
  ss = get_global_state();
  XEN_ASSERT_TYPE(XEN_STRING_P(val), val, XEN_ONLY_ARG, S_setB S_peaks_font, "a string"); 
  set_peaks_font(ss, XEN_TO_C_STRING(val)); 
  return(C_TO_XEN_STRING(peaks_font(ss)));
}

static XEN g_window_width(void) 
{
  #define H_window_width "(" S_window_width ") -> current Snd window width in pixels"
  return(C_TO_XEN_INT(widget_width(MAIN_SHELL(get_global_state()))));
}

static XEN g_window_height(void) 
{
  #define H_window_height "(" S_window_height ") -> current Snd window height in pixels"
  return(C_TO_XEN_INT(widget_height(MAIN_SHELL(get_global_state()))));
}

static XEN g_window_x(void) 
{
  #define H_window_x "(" S_window_x ") -> current Snd window x position in pixels"
  return(C_TO_XEN_INT(widget_x(MAIN_SHELL(get_global_state()))));
}

static XEN g_window_y(void) 
{
  #define H_window_y "(" S_window_y ") -> current Snd window y position in pixels"
  return(C_TO_XEN_INT(widget_y(MAIN_SHELL(get_global_state()))));
}

static int snd_screen_height(void)
{
#if USE_MOTIF
  return(HeightOfScreen(ScreenOfDisplay(MAIN_DISPLAY(get_global_state()), 0)));
#else
#if USE_GTK
  return(gdk_screen_height());
#else
  return(4000);
#endif
#endif
}

static int snd_screen_width(void)
{
#if USE_MOTIF
  return(WidthOfScreen(ScreenOfDisplay(MAIN_DISPLAY(get_global_state()), 0)));
#else
#if USE_GTK
  return(gdk_screen_width());
#else
  return(4000);
#endif
#endif
}

static XEN g_set_window_height(XEN height) 
{
  #define H_set_window_height "(" S_setB S_window_height " val) sets the Snd window height in pixels"
  Latus val;
  snd_state *ss;
  ss = get_global_state();
  XEN_ASSERT_TYPE(XEN_NUMBER_P(height), height, XEN_ONLY_ARG, S_setB S_window_height, "a number"); 
  val = (Latus)XEN_TO_C_INT_OR_ELSE(height, 0);
  if ((val > 0) && (val < snd_screen_height()))
    {
#if HAVE_MOTIF
      set_widget_height(MAIN_SHELL(ss), val);
#endif
      ss->init_window_height = val;
    }
  return(height);
}

static XEN g_set_window_width(XEN width) 
{
  #define H_set_window_width "(" S_setB S_window_width " val) sets the Snd window width in pixels"
  Latus val;
  snd_state *ss;
  ss = get_global_state();
  XEN_ASSERT_TYPE(XEN_NUMBER_P(width), width, XEN_ONLY_ARG, S_setB S_window_width, "a number"); 
  val = (Latus)XEN_TO_C_INT_OR_ELSE(width, 0);
  if ((val > 0) && (val < snd_screen_width()))
    {
#if HAVE_MOTIF
      set_widget_width(MAIN_SHELL(ss), val);
#endif
      ss->init_window_width = val;
    }
  return(width);
}

static XEN g_set_window_x(XEN val) 
{
  #define H_set_window_x "(" S_setB S_window_x " val) sets the Snd window x position in pixels"
  Locus x;
  snd_state *ss;
  ss = get_global_state();
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ONLY_ARG, S_setB S_window_x, "a number"); 
  x = (Locus)XEN_TO_C_INT_OR_ELSE(val, 0);
  if ((x >= 0) && (x < snd_screen_width()))
    {
      set_widget_x(MAIN_SHELL(ss), x);
      ss->init_window_x = x;
    }
  return(val);
}

static XEN g_set_window_y(XEN val) 
{
  #define H_set_window_y "(" S_setB S_window_y " val) sets the Snd window y position in pixels"
  Locus y;
  snd_state *ss;
  ss = get_global_state();
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ONLY_ARG, S_setB S_window_y, "a number"); 
  y = (Locus)XEN_TO_C_INT_OR_ELSE(val, 0);
  if ((y >= 0) && (y < snd_screen_height()))
    {
      set_widget_y(MAIN_SHELL(ss), y);
      ss->init_window_y = y;
    }
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
  dismiss_all_dialogs(get_global_state());
  return(XEN_FALSE);
}

static XEN g_abortq(void)
{
  #define H_abortQ "(" S_c_g ") allows pending user interface events to occur, returning #t if C-g was typed"
  snd_state *ss;
  ss = get_global_state();
  check_for_event(ss);
  if (ss->stopped_explicitly)
    {
      ss->stopped_explicitly = FALSE;
      return(XEN_TRUE);
    }
  return(XEN_FALSE);
}

snd_info *get_sp(XEN x_snd_n)
{
  int snd_n, len;
  snd_state *ss;
  ss = get_global_state();
  /* if x_snd_n is a number, it is sp->index
     if it's a (non-empty) list, car is mix id (perhaps someday treat list as track if more than one member)
  */
  if (XEN_INTEGER_P(x_snd_n))
    {
      snd_n = XEN_TO_C_INT(x_snd_n);
      if (snd_n >= 0)
	{
	  if ((snd_n < ss->max_sounds) && 
	      (snd_ok(ss->sounds[snd_n])))
	    return(ss->sounds[snd_n]);
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
	  if ((len == 1) && 
	      XEN_INTEGER_P(XEN_CAR(x_snd_n)))
	    {
	      snd_n = XEN_TO_C_INT(XEN_CAR(x_snd_n));
	      if (mix_ok(snd_n))
		return(make_mix_readable_from_id(snd_n));
	    }
	  XEN_ERROR(NO_SUCH_MIX,
		    x_snd_n);
	}
    }
  /* use default sound, if any */
  return(any_selected_sound(ss));
}

chan_info *get_cp(XEN x_snd_n, XEN x_chn_n, const char *caller)
{
  snd_info *sp;
  int chn_n;
  sp = get_sp(x_snd_n);
  if ((sp == NULL) || (!(sp->active)))
    {
      snd_no_such_sound_error(caller, x_snd_n); 
      return(NULL); /* gad -- just in case our catch has been clobbered */
    }
  if (XEN_INTEGER_P(x_chn_n))
    chn_n = XEN_TO_C_INT(x_chn_n);
  else
    if (sp->selected_channel != NO_SELECTION) 
      chn_n = sp->selected_channel;
    else chn_n = 0;
  if ((chn_n >= 0) && (chn_n < sp->nchans) && (sp->chans[chn_n]))
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
  result = open_temp_file(name, chans, hdr, ss);
  if (result == -1) 
    {
      free_file_info(hdr);
      /* this happens if the header writer hit an error -- need to delete the bogus output file */
      if (mus_file_probe(name)) snd_remove(name, TRUE);
      if (ss->catch_message)
	XEN_ERROR(MUS_MISC_ERROR,
		  XEN_LIST_2(C_TO_XEN_STRING(S_open_sound_file),
			     C_TO_XEN_STRING(ss->catch_message)));
      else
	return(snd_no_such_file_error(S_open_sound_file, g_name));
    }
  mus_file_set_data_clipped(result, data_clipped(ss));
  set_temp_fd(result, hdr);
  return(C_TO_XEN_INT(result));
}

static XEN g_close_sound_file(XEN g_fd, XEN g_bytes)
{
  #define H_close_sound_file "(" S_close_sound_file " fd bytes) closes file pointed to by fd, updating its header to report 'bytes' bytes of data"
  file_info *hdr;
  int result, fd;
  off_t bytes;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(g_fd), g_fd, XEN_ARG_1, S_close_sound_file, "an integer");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(g_bytes), g_bytes, XEN_ARG_2, S_close_sound_file, "a number");
  fd = XEN_TO_C_INT(g_fd);
  if ((fd < 0) || (fd == fileno(stdin)) || (fd == fileno(stdout)) || (fd == fileno(stderr)))
    mus_misc_error(S_close_sound_file, "invalid file", g_fd);
  bytes = XEN_TO_C_OFF_T_OR_ELSE(g_bytes, 0);
  hdr = get_temp_header(fd);
  if (hdr == NULL) 
    {
      snd_close(fd, "sound file");
      return(snd_no_such_file_error(S_close_sound_file, g_fd));
    }
  result = close_temp_file(fd, hdr, bytes, any_selected_sound(get_global_state()));
  unset_temp_fd(fd);
  free_file_info(hdr);
  return(C_TO_XEN_INT(result));
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
  int i, len, chn = 0, pos;
  off_t beg;
  XEN_ASSERT_TYPE(XEN_NUMBER_IF_BOUND_P(samp_0), samp_0, XEN_ARG_1, S_samples2sound_data, "a number");
  XEN_ASSERT_TYPE(XEN_NUMBER_IF_BOUND_P(samps), samps, XEN_ARG_2, S_samples2sound_data, "a number");
  ASSERT_CHANNEL(S_samples2sound_data, snd_n, chn_n, 3);
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(sdchan), sdchan, XEN_ARG_7, S_samples2sound_data, "an integer");
  cp = get_cp(snd_n, chn_n, S_samples2sound_data);
  pos = to_c_edit_position(cp, edpos, S_samples2sound_data, 6);
  beg = XEN_TO_C_OFF_T_OR_ELSE(samp_0, 0);
  len = XEN_TO_C_INT_OR_ELSE(samps, (int)(cp->samples[pos] - beg));
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
	      for (i = 0; i < len; i++) 
		sd->data[chn][i] = read_sample(sf);
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
#if HAVE_EXTENSION_LANGUAGE
  XEN res;
  res = snd_catch_any(eval_str_wrapper, (void *)str, "string->float");
  if (XEN_NUMBER_P(res))
    return(XEN_TO_C_DOUBLE(res));
  else snd_error(_("%s is not a number"), str);
  return(0.0);
#else
  Float res = 0.0;
  if (str) sscanf(str, "%f", &res);
  return(res);
#endif
}

int string2int(char *str) 
{
#if HAVE_EXTENSION_LANGUAGE
  XEN res;
  res = snd_catch_any(eval_str_wrapper, (void *)str, "string->int");
  if (XEN_NUMBER_P(res))
    return(XEN_TO_C_INT_OR_ELSE(res, 0));
  else snd_error(_("%s is not a number"), str);
  return(0);
#else
  int res = 0;
  if (str) sscanf(str, "%d", &res);
  return(res);
#endif
}

off_t string2off_t(char *str) 
{
#if HAVE_EXTENSION_LANGUAGE
  XEN res;
  res = snd_catch_any(eval_str_wrapper, (void *)str, "string->off_t");
  if (XEN_NUMBER_P(res))
    return(XEN_TO_C_OFF_T_OR_ELSE(res, 0));
  else snd_error(_("%s is not a number"), str);
  return(0);
#else
  off_t res = 0;
  if (str) sscanf(str, OFF_TD , &res);
  return(res);
#endif
}

static XEN g_help_dialog(XEN subject, XEN msg)
{
  #define H_help_dialog "(" S_help_dialog " subject message) fires up the Help window with subject and message"
  XEN_ASSERT_TYPE(XEN_STRING_P(subject), subject, XEN_ARG_1, S_help_dialog, "a string");
  XEN_ASSERT_TYPE(XEN_STRING_P(msg), msg, XEN_ARG_2, S_help_dialog, "a string");
  return(XEN_WRAP_WIDGET(snd_help_with_wrap(get_global_state(), XEN_TO_C_STRING(subject), XEN_TO_C_STRING(msg))));
}

static XEN g_mix_panel(void)
{
  #define H_mix_panel "(" S_mix_panel ") starts (and returns) the mix panel"
  return(XEN_WRAP_WIDGET(make_mix_panel(get_global_state())));
}

static XEN g_color_dialog(void) 
{
  #define H_color_dialog "(" S_color_dialog ") fires up the Color dialog"
  return(XEN_WRAP_WIDGET(start_color_dialog(get_global_state(), 0, 0))); 
}

static XEN g_orientation_dialog(void) 
{
  #define H_orientation_dialog "(" S_orientation_dialog ") fires up the Orientation dialog"
  return(XEN_WRAP_WIDGET(start_orientation_dialog(get_global_state(), 0, 0))); 
}

static XEN g_transform_dialog(XEN managed) 
{
  #define H_transform_dialog "(" S_transform_dialog " (managed #t)) creates and (if managed) fires up the Transforms dialog"
  XEN_ASSERT_TYPE(XEN_BOOLEAN_IF_BOUND_P(managed), managed, XEN_ONLY_ARG, S_transform_dialog, "a boolean");
  return(XEN_WRAP_WIDGET(fire_up_transform_dialog(get_global_state(), XEN_TO_C_BOOLEAN_OR_TRUE(managed))));
}

static XEN g_file_dialog(void) 
{
  #define H_file_dialog "(" S_file_dialog ") fires up the View Current/Previous File dialog"
  return(XEN_WRAP_WIDGET(start_file_dialog(get_global_state(), 0, 0)));
}

static XEN g_edit_header_dialog(XEN snd_n) 
{
  #define H_edit_header_dialog "(" S_edit_header_dialog " snd) opens the Edit Header dialog on sound snd"
  snd_info *sp; 
  sp = get_sp(snd_n);
  if (sp == NULL)
    return(snd_no_such_sound_error(S_edit_header_dialog, snd_n));
  return(XEN_WRAP_WIDGET(edit_header(sp))); 
}

static XEN g_edit_save_as_dialog(void) 
{
  #define H_edit_save_as_dialog "(" S_edit_save_as_dialog ") opens the Selection Save-as dialog"
  make_edit_save_as_dialog(get_global_state()); 
  return(XEN_FALSE);
}

static XEN g_file_save_as_dialog(void) 
{
  #define H_file_save_as_dialog "(" S_file_save_as_dialog ") opens the File Save-as dialog"
  make_file_save_as_dialog(get_global_state()); 
  return(XEN_FALSE);
}

static XEN g_yes_or_no_p(XEN msg) 
{
  #define H_yes_or_no_p "(" S_yes_or_no_p " message) displays message and waits for 'y' or 'n'; returns #t if 'y'"
  XEN_ASSERT_TYPE(XEN_STRING_P(msg), msg, XEN_ONLY_ARG, S_yes_or_no_p, "a string");
  return(C_TO_XEN_BOOLEAN(snd_yes_or_no_p(get_global_state(), XEN_TO_C_STRING(msg))));
}

static XEN g_clear_audio_inputs (void) 
{
  #define H_clear_audio_inputs "(" S_clear_audio_inputs ") tries to reduce soundcard noise in Linux/OSS"
#if HAVE_OSS
  mus_audio_clear_soundcard_inputs(); 
#endif
  return(XEN_FALSE);
}

/* this needs to be in Snd (rather than sndlib2xen.c) because it calls snd_help */
#define S_mus_audio_describe            "mus-audio-describe"
static XEN g_mus_audio_describe(void) 
{
  #define H_mus_audio_describe "("  S_mus_audio_describe ") posts a description of the audio hardware state in the Help dialog"
  snd_help(get_global_state(), "Audio State", mus_audio_report()); 
  return(XEN_TRUE);
}

#if (!USE_NO_GUI)
/* -------- shared color funcs -------- */

static XEN g_color_p(XEN obj) 
{
  #define H_color_p "(" S_color_p " obj) -> #t if obj is a color"
  return(C_TO_XEN_BOOLEAN(XEN_PIXEL_P(obj)));
}

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
  snd_state *ss;
  ss = get_global_state();
  XEN_ASSERT_TYPE(XEN_PIXEL_P(color), color, XEN_ONLY_ARG, S_setB S_cursor_color, "a color"); 
  color_cursor(ss, XEN_UNWRAP_PIXEL(color));
  for_each_chan(ss, update_graph);
  return(color);
}

static XEN g_cursor_color(void) 
{
  #define H_cursor_color "(" S_cursor_color ") -> cursor color"
  snd_state *ss;
  ss = get_global_state();
  return(XEN_WRAP_PIXEL((ss->sgx)->cursor_color));
}

static XEN g_set_highlight_color (XEN color) 
{
  snd_state *ss;
  ss = get_global_state();
  XEN_ASSERT_TYPE(XEN_PIXEL_P(color), color, XEN_ONLY_ARG, S_setB S_highlight_color, "a color"); 
  (ss->sgx)->highlight_color = XEN_UNWRAP_PIXEL(color);
  return(color);
}

static XEN g_highlight_color(void) 
{
  #define H_highlight_color "(" S_highlight_color ") -> color of highlighted text or buttons"
  snd_state *ss;
  ss = get_global_state();
  return(XEN_WRAP_PIXEL((ss->sgx)->highlight_color));
}

static XEN g_set_mark_color (XEN color) 
{
  snd_state *ss;
  ss = get_global_state();
  XEN_ASSERT_TYPE(XEN_PIXEL_P(color), color, XEN_ONLY_ARG, S_setB S_mark_color, "a color"); 
  color_marks(ss, XEN_UNWRAP_PIXEL(color));
  for_each_chan(ss, update_graph);
  return(color);
}

static XEN g_mark_color(void) 
{
  #define H_mark_color "(" S_mark_color ") -> mark color"
  snd_state *ss;
  ss = get_global_state();
  return(XEN_WRAP_PIXEL((ss->sgx)->mark_color));
}

static XEN g_set_zoom_color (XEN color) 
{
  snd_state *ss;
  ss = get_global_state();
  XEN_ASSERT_TYPE(XEN_PIXEL_P(color), color, XEN_ONLY_ARG, S_setB S_zoom_color, "a color"); 
  (ss->sgx)->zoom_color = XEN_UNWRAP_PIXEL(color); 
  color_chan_components(ss->sgx->zoom_color, COLOR_ZOOM);
  return(color);
}

static XEN g_zoom_color(void) 
{
  #define H_zoom_color "(" S_zoom_color ") -> color of zoom sliders"
  snd_state *ss;
  ss = get_global_state();
  return(XEN_WRAP_PIXEL((ss->sgx)->zoom_color));
}

static XEN g_set_position_color (XEN color) 
{
  snd_state *ss;
  ss = get_global_state();
  XEN_ASSERT_TYPE(XEN_PIXEL_P(color), color, XEN_ONLY_ARG, S_setB S_position_color, "a color"); 
  (ss->sgx)->position_color = XEN_UNWRAP_PIXEL(color); 
  color_chan_components(ss->sgx->position_color, COLOR_POSITION);
  return(color);
}

static XEN g_position_color(void) 
{
  #define H_position_color "(" S_position_color ") -> color of position sliders"
  snd_state *ss;
  ss = get_global_state();
  return(XEN_WRAP_PIXEL((ss->sgx)->position_color));
}

static XEN g_set_listener_color (XEN color) 
{
  snd_state *ss;
  ss = get_global_state();
  XEN_ASSERT_TYPE(XEN_PIXEL_P(color), color, XEN_ONLY_ARG, S_setB S_listener_color, "a color"); 
  color_listener(XEN_UNWRAP_PIXEL(color));
  return(color);
}

static XEN g_listener_color(void) 
{
  #define H_listener_color "(" S_listener_color ") -> background color of the lisp listener"
  snd_state *ss;
  ss = get_global_state();
  return(XEN_WRAP_PIXEL((ss->sgx)->listener_color));
}

static XEN g_set_listener_text_color (XEN color) 
{
  snd_state *ss;
  ss = get_global_state();
  XEN_ASSERT_TYPE(XEN_PIXEL_P(color), color, XEN_ONLY_ARG, S_setB S_listener_text_color, "a color"); 
  color_listener_text(XEN_UNWRAP_PIXEL(color));
  return(color);
}

static XEN g_listener_text_color(void) 
{
  #define H_listener_text_color "(" S_listener_text_color ") -> text color in the lisp listener"
  snd_state *ss;
  ss = get_global_state();
  return(XEN_WRAP_PIXEL((ss->sgx)->listener_text_color));
}

static XEN g_set_enved_waveform_color (XEN color) 
{
  snd_state *ss;
  ss = get_global_state();
  XEN_ASSERT_TYPE(XEN_PIXEL_P(color), color, XEN_ONLY_ARG, S_setB S_enved_waveform_color, "a color"); 
  color_enved_waveform(XEN_UNWRAP_PIXEL(color));
  return(color);
}

static XEN g_enved_waveform_color(void) 
{
  #define H_enved_waveform_color "(" S_enved_waveform_color ") -> color of the envelope editor wave display"
  snd_state *ss;
  ss = get_global_state();
  return(XEN_WRAP_PIXEL((ss->sgx)->enved_waveform_color));
}

static XEN g_set_filter_waveform_color (XEN color) 
{
  snd_state *ss;
  ss = get_global_state();
  XEN_ASSERT_TYPE(XEN_PIXEL_P(color), color, XEN_ONLY_ARG, S_setB S_filter_waveform_color, "a color");
  color_filter_waveform(ss, XEN_UNWRAP_PIXEL(color));
  return(color);
}

static XEN g_filter_waveform_color(void) 
{
  #define H_filter_waveform_color "(" S_filter_waveform_color ") -> color of the filter waveform"
  snd_state *ss;
  ss = get_global_state();
  return(XEN_WRAP_PIXEL((ss->sgx)->filter_waveform_color));
}

static XEN g_set_selection_color (XEN color) 
{
  snd_state *ss;
  ss = get_global_state();
  XEN_ASSERT_TYPE(XEN_PIXEL_P(color), color, XEN_ONLY_ARG, S_setB S_selection_color, "a color"); 
  color_selection(ss, XEN_UNWRAP_PIXEL(color));
  for_each_chan(ss, update_graph);
  return(color);
}

static XEN g_selection_color(void) 
{
  #define H_selection_color "(" S_selection_color ") -> selection color"
  snd_state *ss;
  ss = get_global_state();
  return(XEN_WRAP_PIXEL((ss->sgx)->selection_color));
}

static XEN g_set_text_focus_color (XEN color) 
{
  snd_state *ss;
  ss = get_global_state();
  XEN_ASSERT_TYPE(XEN_PIXEL_P(color), color, XEN_ONLY_ARG, S_setB S_text_focus_color, "a color"); 
  (ss->sgx)->text_focus_color = XEN_UNWRAP_PIXEL(color);
  return(color);
}

static XEN g_text_focus_color(void) 
{
  #define H_text_focus_color "(" S_text_focus_color ") -> color used to show a text field has focus"
  snd_state *ss;
  ss = get_global_state();
  return(XEN_WRAP_PIXEL((ss->sgx)->text_focus_color));
}

static XEN g_set_sash_color (XEN color) 
{
  snd_state *ss;
  ss = get_global_state();
  XEN_ASSERT_TYPE(XEN_PIXEL_P(color), color, XEN_ONLY_ARG, S_setB S_sash_color, "a color"); 
  (ss->sgx)->sash_color = XEN_UNWRAP_PIXEL(color);
  return(color);
}

static XEN g_sash_color(void) 
{
  #define H_sash_color "(" S_sash_color ") -> color used to draw paned window sashes"
  snd_state *ss;
  ss = get_global_state();
  return(XEN_WRAP_PIXEL((ss->sgx)->sash_color));
}

static XEN g_set_data_color (XEN color) 
{
  snd_state *ss;
  ss = get_global_state();
  XEN_ASSERT_TYPE(XEN_PIXEL_P(color), color, XEN_ONLY_ARG, S_setB S_data_color, "a color"); 
  color_data(ss, XEN_UNWRAP_PIXEL(color));
  for_each_chan(ss, update_graph);
  return(color);
}

static XEN g_data_color(void) 
{
  #define H_data_color "(" S_data_color ") -> color used to draw unselected data"
  snd_state *ss;
  ss = get_global_state();
  return(XEN_WRAP_PIXEL((ss->sgx)->data_color));
}

static XEN g_set_selected_data_color (XEN color)
{
  chan_info *cp;
  snd_state *ss;
  ss = get_global_state();
  XEN_ASSERT_TYPE(XEN_PIXEL_P(color), color, XEN_ONLY_ARG, S_setB S_selected_data_color, "a color"); 
  color_selected_data(ss, XEN_UNWRAP_PIXEL(color));
  cp = selected_channel(ss);
  if (cp) 
    {
      color_selected_data(ss, XEN_UNWRAP_PIXEL(color));
      update_graph(cp);
    }
  return(color);
}

static XEN g_selected_data_color(void) 
{
  #define H_selected_data_color "(" S_selected_data_color ") -> color used for selected data"
  snd_state *ss;
  ss = get_global_state();
  return(XEN_WRAP_PIXEL((ss->sgx)->selected_data_color));
}

static XEN g_set_graph_color (XEN color) 
{
  snd_state *ss;
  ss = get_global_state();
  XEN_ASSERT_TYPE(XEN_PIXEL_P(color), color, XEN_ONLY_ARG, S_setB S_graph_color, "a color");
  color_graph(ss, XEN_UNWRAP_PIXEL(color));
  color_unselected_graphs(XEN_UNWRAP_PIXEL(color));
  return(color);
}

static XEN g_graph_color(void) 
{
  #define H_graph_color "(" S_graph_color ") -> background color used for unselected data"
  snd_state *ss;
  ss = get_global_state();
  return(XEN_WRAP_PIXEL((ss->sgx)->graph_color));
}

static XEN g_set_selected_graph_color (XEN color) 
{
  chan_info *cp;
  snd_state *ss;
  ss = get_global_state();
  XEN_ASSERT_TYPE(XEN_PIXEL_P(color), color, XEN_ONLY_ARG, S_setB S_selected_graph_color, "a color");
  color_selected_graph(ss, XEN_UNWRAP_PIXEL(color));
  cp = selected_channel(ss);
  if (cp) 
    {
#if USE_MOTIF
      XtVaSetValues(channel_graph(cp), XmNbackground, XEN_UNWRAP_PIXEL(color), NULL);
#else
      set_background_and_redraw(channel_graph(cp), XEN_UNWRAP_PIXEL(color));
#endif
    }
  return(color);
}

static XEN g_selected_graph_color(void) 
{
  #define H_selected_graph_color "(" S_selected_graph_color ") -> background color of selected data"
  snd_state *ss;
  ss = get_global_state();
  return(XEN_WRAP_PIXEL((ss->sgx)->selected_graph_color));
}

static XEN g_set_pushed_button_color (XEN color) 
{
  snd_state *ss;
  ss = get_global_state();
  XEN_ASSERT_TYPE(XEN_PIXEL_P(color), color, XEN_ONLY_ARG, S_setB S_pushed_button_color, "a color"); 
  (ss->sgx)->pushed_button_color = XEN_UNWRAP_PIXEL(color);
#if USE_MOTIF
  map_over_children(MAIN_SHELL(ss), recolor_button, NULL);
#endif
  return(color);
}

static XEN g_pushed_button_color(void) 
{
  #define H_pushed_button_color "(" S_pushed_button_color ") -> color of a pushed button"
  snd_state *ss;
  ss = get_global_state();
  return(XEN_WRAP_PIXEL((ss->sgx)->pushed_button_color));
}

static XEN g_basic_color(void) 
{
  #define H_basic_color "(" S_basic_color ") -> Snd's basic color"
  snd_state *ss;
  ss = get_global_state();
  return(XEN_WRAP_PIXEL((ss->sgx)->basic_color));
}

static XEN g_set_basic_color(XEN color) 
{
  color_t old_color;
  snd_state *ss;
  ss = get_global_state();
  XEN_ASSERT_TYPE(XEN_PIXEL_P(color), color, XEN_ONLY_ARG, S_setB S_basic_color, "a color"); 
  old_color = (ss->sgx)->basic_color;
  (ss->sgx)->basic_color = XEN_UNWRAP_PIXEL(color); 
#if USE_MOTIF
  map_over_children(MAIN_SHELL(ss), recolor_everything, (void *)old_color);
#endif
  return(color);
}
#endif


static XEN during_open_hook;
static XEN after_open_hook;

XEN run_progn_hook(XEN hook, XEN args, const char *caller)
{
#if HAVE_GUILE
  /* Guile built-in scm_c_run_hook doesn't return the value of the hook procedure(s) and exits on error */
  XEN result = XEN_FALSE;
  XEN procs = XEN_HOOK_PROCEDURES(hook);
  while (XEN_NOT_NULL_P(procs))
    {
      if (!(XEN_EQ_P(args, XEN_EMPTY_LIST)))
	result = XEN_APPLY(XEN_CAR(procs), args, caller);
      else result = XEN_CALL_0(XEN_CAR(procs), caller); /* currently unused */
      procs = XEN_CDR (procs);
    }
  return(xen_return_first(result, args));
#else
  if (!(XEN_EQ_P(args, XEN_EMPTY_LIST)))
    return(XEN_APPLY(hook, args, caller));
  else return(XEN_CALL_0(hook, caller));
#endif
}

XEN run_hook(XEN hook, XEN args, const char *caller)
{
#if HAVE_GUILE
  XEN procs = XEN_HOOK_PROCEDURES(hook);
  while (XEN_NOT_NULL_P(procs))
    {
      if (!(XEN_EQ_P(args, XEN_EMPTY_LIST)))
	XEN_APPLY(XEN_CAR(procs), args, caller);
      else XEN_CALL_0(XEN_CAR(procs), caller);
      procs = XEN_CDR (procs);
    }
  return(xen_return_first(XEN_FALSE, args));
#else
  if (!(XEN_EQ_P(args, XEN_EMPTY_LIST)))
    XEN_APPLY(hook, args, caller);
  else XEN_CALL_0(hook, caller);
  return(XEN_FALSE);
#endif
}

XEN run_or_hook (XEN hook, XEN args, const char *caller)
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
    return(XEN_APPLY(hook, args, caller));
  else return(XEN_CALL_0(hook, caller));
#endif
}

XEN run_and_hook(XEN hook, XEN args, const char *caller)
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
    return(XEN_APPLY(hook, args, caller));
  else return(XEN_CALL_0(hook, caller));
#endif
}

void during_open(int fd, char *file, int reason)
{
  if (XEN_HOOKED(during_open_hook))
    run_hook(during_open_hook,
	     XEN_LIST_3(C_TO_XEN_INT(fd),
			C_TO_XEN_STRING(file),
			C_TO_XEN_INT(reason)),
	     S_during_open_hook);
}

void after_open(int index)
{
  if (XEN_HOOKED(after_open_hook))
    run_hook(after_open_hook,
	     XEN_LIST_1(C_TO_SMALL_XEN_INT(index)),
	     S_after_open_hook);
}

#if HAVE_LADSPA
  void g_ladspa_to_snd();
#endif

#if HAVE_GUILE && HAVE_DLFCN_H
#include <dlfcn.h>
/* these are included because libtool's dlopen is incredibly stupid */

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
  char *str;
  XEN res;
  str = command_completer(XEN_TO_C_STRING(text));
  res = C_TO_XEN_STRING(str);
  FREE(str);
  return(res);
}

static XEN g_snd_global_state(void)
{
  return(XEN_WRAP_C_POINTER(get_global_state()));
}

#if DEBUGGING
static XEN g_snd_sound_pointer(XEN snd)
{
  /* (XtCallCallbacks (cadr (sound-widgets 0)) XmNactivateCallback (snd-sound-pointer 0)) */
  snd_state *ss;
  int s;
  ss = get_global_state();
  s = XEN_TO_C_INT(snd);
  if ((s < ss->max_sounds) && (s >= 0) && (ss->sounds[s]))
    return(C_TO_XEN_ULONG((unsigned long)(ss->sounds[s])));
  return(XEN_FALSE);
}

#if HAVE_GUILE
static SCM g_gc_off(void) {++scm_block_gc; return(XEN_FALSE);}
static SCM g_gc_on(void) {--scm_block_gc; return(XEN_FALSE);}
#endif

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
XEN_NARGIFY_0(g_emacs_style_save_as_w, g_emacs_style_save_as)
XEN_ARGIFY_1(g_set_emacs_style_save_as_w, g_set_emacs_style_save_as)
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
XEN_NARGIFY_0(g_audio_state_file_w, g_audio_state_file)
XEN_ARGIFY_1(g_set_audio_state_file_w, g_set_audio_state_file)
XEN_NARGIFY_0(g_selection_creates_region_w, g_selection_creates_region)
XEN_ARGIFY_1(g_set_selection_creates_region_w, g_set_selection_creates_region)
XEN_NARGIFY_0(g_print_length_w, g_print_length)
XEN_ARGIFY_1(g_set_print_length_w, g_set_print_length)
XEN_NARGIFY_0(g_show_indices_w, g_show_indices)
XEN_ARGIFY_1(g_set_show_indices_w, g_set_show_indices)
XEN_NARGIFY_0(g_show_backtrace_w, g_show_backtrace)
XEN_ARGIFY_1(g_set_show_backtrace_w, g_set_show_backtrace)
XEN_NARGIFY_0(g_color_map_w, g_color_map)
XEN_ARGIFY_1(g_set_color_map_w, g_set_color_map)
XEN_NARGIFY_0(g_temp_dir_w, g_temp_dir)
XEN_ARGIFY_1(g_set_temp_dir_w, g_set_temp_dir)
XEN_NARGIFY_0(g_save_dir_w, g_save_dir)
XEN_ARGIFY_1(g_set_save_dir_w, g_set_save_dir)
XEN_NARGIFY_0(g_ladspa_dir_w, g_ladspa_dir)
XEN_ARGIFY_1(g_set_ladspa_dir_w, g_set_ladspa_dir)
XEN_NARGIFY_0(g_trap_segfault_w, g_trap_segfault)
XEN_ARGIFY_1(g_set_trap_segfault_w, g_set_trap_segfault)
XEN_NARGIFY_0(g_show_selection_transform_w, g_show_selection_transform)
XEN_ARGIFY_1(g_set_show_selection_transform_w, g_set_show_selection_transform)
XEN_NARGIFY_0(g_with_gl_w, g_with_gl)
XEN_ARGIFY_1(g_set_with_gl_w, g_set_with_gl)
XEN_NARGIFY_0(g_with_relative_panes_w, g_with_relative_panes)
XEN_ARGIFY_1(g_set_with_relative_panes_w, g_set_with_relative_panes)
XEN_NARGIFY_0(g_with_background_processes_w, g_with_background_processes)
XEN_ARGIFY_1(g_set_with_background_processes_w, g_set_with_background_processes)
XEN_NARGIFY_0(g_use_sinc_interp_w, g_use_sinc_interp)
XEN_ARGIFY_1(g_set_use_sinc_interp_w, g_set_use_sinc_interp)
XEN_NARGIFY_0(g_data_clipped_w, g_data_clipped)
XEN_ARGIFY_1(g_set_data_clipped_w, g_set_data_clipped)
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
XEN_NARGIFY_0(g_peaks_font_w, g_peaks_font)
XEN_ARGIFY_1(g_set_peaks_font_w, g_set_peaks_font)
XEN_NARGIFY_0(g_bold_peaks_font_w, g_bold_peaks_font)
XEN_ARGIFY_1(g_set_bold_peaks_font_w, g_set_bold_peaks_font)
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
XEN_NARGIFY_1(g_color_p_w, g_color_p)
#endif
XEN_NARGIFY_0(g_snd_tempnam_w, g_snd_tempnam)
XEN_NARGIFY_0(g_clear_audio_inputs_w, g_clear_audio_inputs)
XEN_NARGIFY_0(g_color_dialog_w, g_color_dialog)
XEN_NARGIFY_0(g_orientation_dialog_w, g_orientation_dialog)
XEN_ARGIFY_1(g_transform_dialog_w, g_transform_dialog)
XEN_NARGIFY_0(g_file_dialog_w, g_file_dialog)
XEN_ARGIFY_1(g_edit_header_dialog_w, g_edit_header_dialog)
XEN_NARGIFY_0(g_edit_save_as_dialog_w, g_edit_save_as_dialog)
XEN_NARGIFY_0(g_file_save_as_dialog_w, g_file_save_as_dialog)
XEN_NARGIFY_2(g_help_dialog_w, g_help_dialog)
XEN_NARGIFY_0(g_mix_panel_w, g_mix_panel)
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
XEN_ARGIFY_7(samples2sound_data_w, samples2sound_data)
XEN_NARGIFY_1(g_snd_print_w, g_snd_print)
XEN_NARGIFY_0(g_mus_audio_describe_w, g_mus_audio_describe)
XEN_NARGIFY_0(g_little_endian_w, g_little_endian)
XEN_NARGIFY_1(g_snd_completion_w, g_snd_completion)
XEN_NARGIFY_0(g_snd_global_state_w, g_snd_global_state)
#if DEBUGGING
  XEN_NARGIFY_1(g_snd_sound_pointer_w, g_snd_sound_pointer)
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
#define g_emacs_style_save_as_w g_emacs_style_save_as
#define g_set_emacs_style_save_as_w g_set_emacs_style_save_as
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
#define g_audio_state_file_w g_audio_state_file
#define g_set_audio_state_file_w g_set_audio_state_file
#define g_selection_creates_region_w g_selection_creates_region
#define g_set_selection_creates_region_w g_set_selection_creates_region
#define g_print_length_w g_print_length
#define g_set_print_length_w g_set_print_length
#define g_show_indices_w g_show_indices
#define g_set_show_indices_w g_set_show_indices
#define g_show_backtrace_w g_show_backtrace
#define g_set_show_backtrace_w g_set_show_backtrace
#define g_color_map_w g_color_map
#define g_set_color_map_w g_set_color_map
#define g_temp_dir_w g_temp_dir
#define g_set_temp_dir_w g_set_temp_dir
#define g_save_dir_w g_save_dir
#define g_set_save_dir_w g_set_save_dir
#define g_ladspa_dir_w g_ladspa_dir
#define g_set_ladspa_dir_w g_set_ladspa_dir
#define g_trap_segfault_w g_trap_segfault
#define g_set_trap_segfault_w g_set_trap_segfault
#define g_show_selection_transform_w g_show_selection_transform
#define g_set_show_selection_transform_w g_set_show_selection_transform
#define g_with_gl_w g_with_gl
#define g_set_with_gl_w g_set_with_gl
#define g_with_relative_panes_w g_with_relative_panes
#define g_set_with_relative_panes_w g_set_with_relative_panes
#define g_with_background_processes_w g_with_background_processes
#define g_set_with_background_processes_w g_set_with_background_processes
#define g_use_sinc_interp_w g_use_sinc_interp
#define g_set_use_sinc_interp_w g_set_use_sinc_interp
#define g_data_clipped_w g_data_clipped
#define g_set_data_clipped_w g_set_data_clipped
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
#define g_peaks_font_w g_peaks_font
#define g_set_peaks_font_w g_set_peaks_font
#define g_bold_peaks_font_w g_bold_peaks_font
#define g_set_bold_peaks_font_w g_set_bold_peaks_font
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
#define g_color_p_w g_color_p
#endif
#define g_snd_tempnam_w g_snd_tempnam
#define g_clear_audio_inputs_w g_clear_audio_inputs
#define g_color_dialog_w g_color_dialog
#define g_orientation_dialog_w g_orientation_dialog
#define g_transform_dialog_w g_transform_dialog
#define g_file_dialog_w g_file_dialog
#define g_edit_header_dialog_w g_edit_header_dialog
#define g_edit_save_as_dialog_w g_edit_save_as_dialog
#define g_file_save_as_dialog_w g_file_save_as_dialog
#define g_help_dialog_w g_help_dialog
#define g_mix_panel_w g_mix_panel
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
#define samples2sound_data_w samples2sound_data
#define g_snd_print_w g_snd_print
#define g_mus_audio_describe_w g_mus_audio_describe
#define g_little_endian_w g_little_endian
#define g_snd_completion_w g_snd_completion
#define g_snd_global_state_w g_snd_global_state
#if DEBUGGING
  #define g_snd_sound_pointer_w g_snd_sound_pointer
#endif
#endif

#if HAVE_STATIC_XM
#if HAVE_GUILE
 void init_xm(void);
#else
 void Init_libxm(void);
#endif
#endif

#ifndef JUST_GL
#if HAVE_GL
#if HAVE_GUILE
 void init_gl(void);
#else
 void Init_libgl(void);
#endif
#endif
#endif

void g_initialize_gh(void)
{
  XEN_DEFINE_PROCEDURE("snd-global-state", g_snd_global_state_w, 0, 0, 0, "internal testing function");
#if DEBUGGING
  XEN_DEFINE_PROCEDURE("snd-sound-pointer", g_snd_sound_pointer_w, 1, 0, 0, "internal testing function");
#if HAVE_GUILE
  XEN_DEFINE_PROCEDURE("gc-off", g_gc_off, 0, 0, 0, "turns off the garbage collector");
  XEN_DEFINE_PROCEDURE("gc-on", g_gc_on, 0, 0, 0, "turns on the garbage collector");
#endif
#endif

#if HAVE_RUBY
  Init_sndlib();
#else
  init_sndlib();
#endif

#if HAVE_EXTENSION_LANGUAGE
  mus_midi_init();
#endif

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
  #define H_keyboard_no_action "The value for an " S_bind_key " function that causes it do nothing upon return"

  XEN_DEFINE_CONSTANT(S_cursor_in_view,        CURSOR_IN_VIEW,        H_cursor_in_view);
  XEN_DEFINE_CONSTANT(S_cursor_on_left,        CURSOR_ON_LEFT,        H_cursor_on_left);
  XEN_DEFINE_CONSTANT(S_cursor_on_right,       CURSOR_ON_RIGHT,       H_cursor_on_right);
  XEN_DEFINE_CONSTANT(S_cursor_in_middle,      CURSOR_IN_MIDDLE,      H_cursor_in_middle);
  XEN_DEFINE_CONSTANT(S_keyboard_no_action,    KEYBOARD_NO_ACTION,    H_keyboard_no_action);

  XEN_DEFINE_CONSTANT(S_time_graph,            TIME_AXIS_INFO,        "time domain graph");
  XEN_DEFINE_CONSTANT(S_transform_graph,       TRANSFORM_AXIS_INFO,   "frequency domain graph");
  XEN_DEFINE_CONSTANT(S_lisp_graph,            LISP_AXIS_INFO,        "lisp graph");


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

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_emacs_style_save_as, g_emacs_style_save_as_w, H_emacs_style_save_as,
				   "set-" S_emacs_style_save_as, g_set_emacs_style_save_as_w,  0, 0, 0, 1);

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

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_audio_state_file, g_audio_state_file_w, H_audio_state_file,
				   "set-" S_audio_state_file, g_set_audio_state_file_w,  0, 0, 0, 1);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_selection_creates_region, g_selection_creates_region_w, H_selection_creates_region,
				   "set-" S_selection_creates_region, g_set_selection_creates_region_w,  0, 0, 0, 1);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_print_length, g_print_length_w, H_print_length,
				   "set-" S_print_length, g_set_print_length_w,  0, 0, 0, 1);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_show_indices, g_show_indices_w, H_show_indices,
				   "set-" S_show_indices, g_set_show_indices_w,  0, 0, 0, 1);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_show_backtrace, g_show_backtrace_w, H_show_backtrace,
				   "set-" S_show_backtrace, g_set_show_backtrace_w,  0, 0, 0, 1);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_colormap, g_color_map_w, H_colormap,
				   "set-" S_colormap, g_set_color_map_w,  0, 0, 0, 1);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_temp_dir, g_temp_dir_w, H_temp_dir,
				   "set-" S_temp_dir, g_set_temp_dir_w,  0, 0, 0, 1);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_save_dir, g_save_dir_w, H_save_dir,
				   "set-" S_save_dir, g_set_save_dir_w,  0, 0, 0, 1);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_ladspa_dir, g_ladspa_dir_w, H_ladspa_dir,
				   "set-" S_ladspa_dir, g_set_ladspa_dir_w,  0, 0, 0, 1);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_trap_segfault, g_trap_segfault_w, H_trap_segfault,
				   "set-" S_trap_segfault, g_set_trap_segfault_w,  0, 0, 0, 1);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_show_selection_transform, g_show_selection_transform_w, H_show_selection_transform,
				   "set-" S_show_selection_transform, g_set_show_selection_transform_w,  0, 0, 0, 1);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_with_gl, g_with_gl_w, H_with_gl,
				   "set-" S_with_gl, g_set_with_gl_w,  0, 0, 0, 1);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_with_relative_panes, g_with_relative_panes_w, H_with_relative_panes,
				   "set-" S_with_relative_panes, g_set_with_relative_panes_w,  0, 0, 0, 1);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_with_background_processes, g_with_background_processes_w, H_with_background_processes,
				   "set-" S_with_background_processes, g_set_with_background_processes_w,  0, 0, 0, 1);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_use_sinc_interp, g_use_sinc_interp_w, H_use_sinc_interp,
				   "set-" S_use_sinc_interp, g_set_use_sinc_interp_w,  0, 0, 0, 1);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_data_clipped, g_data_clipped_w, H_data_clipped,
				   "set-" S_data_clipped, g_set_data_clipped_w,  0, 0, 0, 1);

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

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_peaks_font, g_peaks_font_w, H_peaks_font,
				   "set-" S_peaks_font, g_set_peaks_font_w,  0, 0, 0, 1);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_bold_peaks_font, g_bold_peaks_font_w, H_bold_peaks_font,
				   "set-" S_bold_peaks_font, g_set_bold_peaks_font_w,  0, 0, 0, 1);

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

  XEN_DEFINE_PROCEDURE(S_color_p,   g_color_p_w, 1, 0, 0, H_color_p);
#endif


  XEN_DEFINE_PROCEDURE(S_snd_tempnam,         g_snd_tempnam_w, 0, 0, 0,         H_snd_tempnam);
  XEN_DEFINE_PROCEDURE(S_clear_audio_inputs,  g_clear_audio_inputs_w, 0, 0, 0,  H_clear_audio_inputs);
  XEN_DEFINE_PROCEDURE(S_color_dialog,        g_color_dialog_w, 0, 0, 0,        H_color_dialog);
  XEN_DEFINE_PROCEDURE(S_orientation_dialog,  g_orientation_dialog_w, 0, 0, 0,  H_orientation_dialog);
  XEN_DEFINE_PROCEDURE(S_transform_dialog,    g_transform_dialog_w, 0, 1, 0,    H_transform_dialog);
  XEN_DEFINE_PROCEDURE(S_file_dialog,         g_file_dialog_w, 0, 0, 0,         H_file_dialog);
  XEN_DEFINE_PROCEDURE(S_edit_header_dialog,  g_edit_header_dialog_w, 0, 1, 0,  H_edit_header_dialog);
  XEN_DEFINE_PROCEDURE(S_edit_save_as_dialog, g_edit_save_as_dialog_w, 0, 0, 0, H_edit_save_as_dialog);
  XEN_DEFINE_PROCEDURE(S_file_save_as_dialog, g_file_save_as_dialog_w, 0, 0, 0, H_file_save_as_dialog);
  XEN_DEFINE_PROCEDURE(S_help_dialog,         g_help_dialog_w, 2, 0, 0,         H_help_dialog);
  XEN_DEFINE_PROCEDURE(S_mix_panel,           g_mix_panel_w, 0, 0, 0,           H_mix_panel);
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
  XEN_DEFINE_PROCEDURE(S_samples2sound_data,  samples2sound_data_w, 0, 7, 0,    H_samples2sound_data);
  XEN_DEFINE_PROCEDURE(S_snd_print,           g_snd_print_w, 1, 0, 0,           H_snd_print);
  XEN_DEFINE_PROCEDURE(S_mus_audio_describe,  g_mus_audio_describe_w, 0, 0, 0,  H_mus_audio_describe);
  XEN_DEFINE_PROCEDURE("little-endian?",      g_little_endian_w, 0, 0, 0,       "return #t if host is little endian");
  XEN_DEFINE_PROCEDURE("snd-completion",      g_snd_completion_w, 1, 0, 0,      "return completion of arg");

  #define H_during_open_hook S_during_open_hook " (fd name reason) is called after file is opened, \
but before data has been read. \n\
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

  XEN_DEFINE_HOOK(during_open_hook,    S_during_open_hook, 3,    H_during_open_hook);    /* args = fd filename reason */
  XEN_DEFINE_HOOK(after_open_hook,     S_after_open_hook, 1,     H_after_open_hook);     /* args = sound */

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

  XEN_DEFINE_HOOK(print_hook, S_print_hook, 1, H_print_hook);          /* arg = text */

  g_init_base();
  g_init_marks();
  g_init_regions();
  g_init_selection();
  g_init_dac();
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
  g_init_find();
  g_init_run();
#if (!USE_NO_GUI)
  g_init_gxutils();
  g_init_gxen();
  g_init_gxfile();
  g_init_gxdraw();
  g_init_gxenv();
  g_init_gxmenu();
  g_init_axis();
  g_init_gxmain();
  g_init_gxlistener();
  g_init_gxchn();
  g_init_draw();
  g_init_gxdrop();
  g_init_gxregion();
  g_init_gxsnd();
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

  XEN_EVAL_C_STRING("(define (" S_snd_apropos " val)\
                       (snd-print (with-output-to-string\
                                    (lambda ()\
                                      (apropos (if (string? val) val (object->string val)))))))");
  XEN_EVAL_C_STRING("(read-set! keywords 'prefix)");
  XEN_EVAL_C_STRING("(print-enable 'source)");  /* added 13-Feb-01 -- print closures with source  */
  XEN_EVAL_C_STRING("(defmacro declare args #f)"); /* for optimizer */

  /* from ice-9/r4rs.scm but with output to snd listener */
  XEN_EVAL_C_STRING("(define snd-remember-paths #f)");
  XEN_EVAL_C_STRING("(set! %load-hook (lambda (filename)\
                                        (if %load-verbosely\
                                            (snd-print (format #f \"~%;;; loading ~S\" filename)))\
                                        (if snd-remember-paths\
                                            (let ((curfile (mus-expand-filename filename))\
                                                  (last-slash 0))\
                                              (do ((i 0 (1+ i)))\
                                                  ((= i (string-length curfile)))\
                                                (if (char=? (string-ref curfile i) #\\/)\
	                                            (set! last-slash i)))\
                                              (let ((new-path (substring curfile 0 last-slash)))\
                                                (if (not (member new-path %load-path))\
	                                            (set! %load-path (cons new-path %load-path))))))))");
#endif

#if HAVE_STATIC_XM
#if HAVE_GUILE
  init_xm();
#else
  Init_libxm();
#endif
#endif

#ifndef JUST_GL
#if HAVE_GL
#if HAVE_GUILE
  init_gl();
#else
  Init_libgl();
#endif
#endif
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
  /* we need to set up the search path so that load and require will work as in the program Ruby */
  #ifdef RUBY_SEARCH_PATH
    {
      /* this code stolen from ruby.c */
      extern VALUE rb_load_path;
      char *str;
      char buf[FILENAME_MAX];
      int i, j = 0, len;
      str = RUBY_SEARCH_PATH;
      len = snd_strlen(str);
      for (i = 0; i < len; i++)
	if (str[i] == ':')
	  {
	    buf[j] = 0;
	    if (j > 0)
	      {
		rb_ary_push(rb_load_path, rb_str_new2(buf));
	      }
	    j = 0;
	  }
	else buf[j++] = str[i];
      if (j > 0)
	{
	  buf[j] = 0;
	  rb_ary_push(rb_load_path, rb_str_new2(buf));
	}
    }
  #endif
#endif
#if DEBUGGING
  XEN_YES_WE_HAVE("snd-debug");
#endif

  XEN_YES_WE_HAVE("snd");

#if WITH_MODULES
  scm_c_use_module("snd sndlib");
  scm_c_use_module("snd clm");
#endif

}
