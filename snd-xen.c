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

#if DEBUGGING
static char **snd_protect_callers = NULL; /* static char* const *callers? no thanks... */
#endif

#if DEBUG_MEMORY
static int max_gc_index = 0;
void dump_protection(FILE *Fp);
void dump_protection(FILE *Fp)
{
  if (XEN_VECTOR_P(gc_protection))
    {
      XEN *gcdata;
      int i;
      gcdata = XEN_VECTOR_ELEMENTS(gc_protection);
      fprintf(Fp, "\n\nsnd_protect (%d table size, used: %d):\n", gc_protection_size, max_gc_index);
      for (i = 0; i < gc_protection_size; i++)
	if (!(XEN_EQ_P(gcdata[i], DEFAULT_GC_VALUE)))
	  {
#if DEBUGGING
  #if HAVE_GUILE
	    fprintf(Fp,"  %s:%d %s", snd_protect_callers[i], i, XEN_AS_STRING(gcdata[i]));
	    if (XEN_HOOK_P(gcdata[i]))
	      fprintf(Fp, " -> %s", XEN_AS_STRING(scm_hook_to_list(gcdata[i])));
  #else
	    fprintf(Fp,"  %s:%d %d %s", snd_protect_callers[i], i, (int)gcdata[i], XEN_AS_STRING(gcdata[i]));
  #endif
#else
  #if HAVE_GUILE
	    fprintf(Fp,"  %d %s", i, XEN_AS_STRING(gcdata[i]));
	    if (XEN_HOOK_P(gcdata[i]))
	      fprintf(Fp, " -> %s", XEN_AS_STRING(scm_hook_to_list(gcdata[i])));
  #else
	    fprintf(Fp,"  %d %d %s", i, (int)gcdata[i], XEN_AS_STRING(gcdata[i]));
  #endif
#endif
	    fprintf(Fp, "\n");
	  }
    }
}
#endif

#if DEBUGGING
int snd_protect_1(XEN obj, const char *caller)
#else
int snd_protect(XEN obj)
#endif
{
  int i, old_size;
  XEN tmp;
  XEN *gcdata;
  if ((XEN_NUMBER_P(obj)) || 
      (XEN_EQ_P(obj, XEN_EMPTY_LIST)) ||
      (XEN_FALSE_P(obj)) || 
      (XEN_TRUE_P(obj)))
    return(-1);
  if (gc_protection_size == 0)
    {
      gc_protection_size = 128;
      /* we don't know the size in advance since each channel can have its own edit/undo hooks */
      gc_protection = XEN_MAKE_VECTOR(gc_protection_size, DEFAULT_GC_VALUE);
      XEN_PROTECT_FROM_GC(gc_protection);
      XEN_VECTOR_SET(gc_protection, 0, obj);
      gc_last_set = 0;
#if DEBUGGING
      snd_protect_callers = (char **)calloc(gc_protection_size, sizeof(char *));
      snd_protect_callers[0] = (char *)caller;
#endif
    }
  else
    {
      gcdata = XEN_VECTOR_ELEMENTS(gc_protection);
      if ((gc_last_cleared >= 0) && 
	  XEN_EQ_P(gcdata[gc_last_cleared], DEFAULT_GC_VALUE))
	{
	  XEN_VECTOR_SET(gc_protection, gc_last_cleared, obj);
#if DEBUGGING
	  snd_protect_callers[gc_last_cleared] = (char *)caller;
#endif
	  gc_last_set = gc_last_cleared;
	  gc_last_cleared = -1;
	  return(gc_last_set);
	}
      for (i = 0; i < gc_protection_size; i++)
	if (XEN_EQ_P(gcdata[i], DEFAULT_GC_VALUE))
	  {
	    XEN_VECTOR_SET(gc_protection, i, obj);
#if DEBUGGING
	    snd_protect_callers[i] = (char *)caller;
#endif
	    gc_last_set = i;
#if DEBUG_MEMORY
	    if (i > max_gc_index) max_gc_index = i;
#endif
	    return(gc_last_set);
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
      /* it would be ideal to unprotect the old table, but it's a permanent object in Guile terms */
      /*   in Ruby, I think we can unprotect it */
#if HAVE_RUBY
      XEN_UNPROTECT_FROM_GC(tmp);
#endif

#if DEBUGGING
      snd_protect_callers = (char **)realloc(snd_protect_callers, gc_protection_size * sizeof(char *));
      snd_protect_callers[old_size] = (char *)caller;
#endif
      gc_last_set = old_size;
    }
#if DEBUG_MEMORY
  if (gc_last_set > max_gc_index) max_gc_index = gc_last_set;
#endif
  return(gc_last_set);
}

void snd_unprotect(XEN obj)
{
  int i;
  XEN *gcdata;
  if (XEN_EQ_P(obj, XEN_EMPTY_LIST)) return;
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
#if DEBUGGING
  fprintf(stderr, "can't unprotect %s", XEN_AS_STRING(obj));
#endif
}

void snd_unprotect_at(int loc)
{
  if (loc >= 0)
    {
      XEN_VECTOR_SET(gc_protection, loc, DEFAULT_GC_VALUE);
      gc_last_cleared = loc;
    }
}


#if HAVE_RUBY
static char *scheme_to_ruby(const char *name)
{
  char *new_name = NULL;
  int len, i, j, cap_loc = -1;
  len = snd_strlen(name);
  if (len > 0)
    {
      new_name = (char *)calloc(len + 8, sizeof(char));
      for (i = 0, j = 0; i < len; i++)
	{
	  if (isalnum(name[i]))
	    {
	      if (cap_loc == -1) cap_loc = j;
	      new_name[j++] = name[i];
	    }
	  else 
	    {
	      if (name[i] != '!')
		{
		  if (name[i] == '-')
		    {
		      if ((i < len - 1) && (name[i + 1] == '>'))
			{
			  new_name[j++] = '2';
			  i++;
			}
		      else
			{
			  new_name[j++] = '_';
			  if (cap_loc != -1)
			    new_name[cap_loc] = (char)toupper((int)(new_name[cap_loc]));
			}
		    }
		  else new_name[j++] = name[i];
		}
	      cap_loc = -1;
	    }
	}
    }
  return(new_name);
}

#endif

/* -------- error handling -------- */

static bool send_error_output_to_stdout = false;
static char *last_file_loaded = NULL;

static void string_to_stdout(const char *msg)
{
  char *str;
  write(fileno(stdout), msg, snd_strlen(msg));
  str = (char *)CALLOC(4 + snd_strlen(listener_prompt(ss)), sizeof(char));
  sprintf(str, "\n%s", listener_prompt(ss));
  write(fileno(stdout), str, snd_strlen(str));
  FREE(str);
}

static char *gl_print(XEN result);

static XEN snd_format_if_needed(XEN args)
{
  /* if car has formatting info, use next arg as arg list for it */
  XEN format_args = XEN_EMPTY_LIST, cur_arg, result;
  int i, start = 0, num_args, format_info_len, err_size = 8192;
  bool got_tilde = false, was_formatted = false;
  char *format_info = NULL, *errmsg = NULL;
  num_args = XEN_LIST_LENGTH(args);
  if (num_args == 1) return(XEN_CAR(args));
  format_info = copy_string(XEN_TO_C_STRING(XEN_CAR(args)));
  format_info_len = snd_strlen(format_info);
  if (XEN_LIST_P(XEN_CADR(args)))
    format_args = XEN_COPY_ARG(XEN_CADR(args)); /* protect Ruby case, a no-op in Guile */
  else format_args = XEN_CADR(args);
  errmsg = (char *)CALLOC(err_size, sizeof(char));
  for (i = 0; i < format_info_len; i++)
    {
      if (format_info[i] == '~')
	{
	  strncat(errmsg, (char *)(format_info + start), i - start);
	  start = i + 2;
	  got_tilde = true;
	}
      else
	{
	  if (got_tilde)
	    {
	      was_formatted = true;
	      got_tilde = false;
	      switch (format_info[i])
		{
		case '~': errmsg = snd_strcat(errmsg, "~", &err_size); break;
		case '%': errmsg = snd_strcat(errmsg, "\n", &err_size); break;
		case 'S': 
		case 'A':
		  if (XEN_NOT_NULL_P(format_args))
		    {
		      cur_arg = XEN_CAR(format_args);
		      format_args = XEN_CDR(format_args);
		      if (XEN_VECTOR_P(cur_arg))
			{
			  char *vstr;
			  vstr = gl_print(cur_arg);
			  errmsg = snd_strcat(errmsg, vstr, &err_size);
			  FREE(vstr);
			}
		      else
			{
#if HAVE_GUILE
			  if (XEN_PROCEDURE_P(cur_arg))
			    {
			      /* don't need the source, just the name here, I think */
			      XEN str;
			      str = scm_procedure_name(cur_arg);
			      if (!(XEN_FALSE_P(str)))
				errmsg = snd_strcat(errmsg, XEN_AS_STRING(str), &err_size);
			      else errmsg = snd_strcat(errmsg, XEN_AS_STRING(cur_arg), &err_size);
			    }
			  else 
#endif
			    errmsg = snd_strcat(errmsg, XEN_AS_STRING(cur_arg), &err_size);
			}
		    }
		  /* else ignore it */
		  break;
		default: start = i - 1; break;
		}
	    }
	}
    }
  if (i > start)
    strncat(errmsg, (char *)(format_info + start), i - start);
  if (format_info) FREE(format_info);
  if (!was_formatted)
    {
      errmsg = snd_strcat(errmsg, " ", &err_size);
      errmsg = snd_strcat(errmsg, XEN_AS_STRING(XEN_CADR(args)), &err_size);
    }
  if (num_args > 2)
    {
      if ((!was_formatted) || (!(XEN_FALSE_P(XEN_CADDR(args))))) start = 2; else start = 3;
      for (i = start; i < num_args; i++)
	{
	  errmsg = snd_strcat(errmsg, " ", &err_size);
	  errmsg = snd_strcat(errmsg, XEN_AS_STRING(XEN_LIST_REF(args, i)), &err_size);
	}
    }
#if HAVE_RUBY
  {
    char *temp;
    temp = scheme_to_ruby(errmsg);
    result = C_TO_XEN_STRING(temp);
    if (temp) free(temp); /* calloc in xen.c */
  }
#else
  result = C_TO_XEN_STRING(errmsg);
#endif
  FREE(errmsg);
  return(xen_return_first(result, args));
}

/* ---------------- GUILE error handler ---------------- */

#ifndef DEBUGGING
  #define DEBUGGING 0
#endif

#if HAVE_GUILE
static XEN snd_catch_scm_error(void *data, XEN tag, XEN throw_args) /* error handler */
{
  snd_info *sp;
  char *possible_code;
  XEN port;
  int port_gc_loc, stack_gc_loc;
  XEN stack = XEN_FALSE;
  char *name_buf = NULL;
  bool need_comma = false;
  if ((XEN_SYMBOL_P(tag)) &&
      (strcmp(XEN_SYMBOL_TO_C_STRING(tag), "snd-top-level") == 0))
    return(throw_args); /* not an error -- just a way to exit the current context */

  port = scm_mkstrport(XEN_ZERO, 
		       scm_make_string(XEN_ZERO, C_TO_XEN_CHAR(0)),
		       SCM_OPN | SCM_WRTNG,
		       "snd error handler");
  port_gc_loc = snd_protect(port);

  if ((DEBUGGING) || (ss->batch_mode))
    {
      /* force out an error before possible backtrace call */
      XEN lport;
      lport = scm_current_error_port();
      XEN_DISPLAY(tag, lport);
      XEN_PUTS(": ", lport);
      XEN_DISPLAY(throw_args, lport);
      XEN_FLUSH_PORT(lport);
    }

  /* Guile's error messages sometimes have formatting directives in the first of the throw_args,
   *   but its simple_format (called by Guile's error handlers) can itself die with an error
   *   in some cases, causing the main program to exit, so we replace the whole thing with our own.
   */
  if ((XEN_LIST_P(throw_args)) && 
      (XEN_LIST_LENGTH(throw_args) > 0))
    {
      /* normally car is string name of calling func */
      if (XEN_NOT_FALSE_P(XEN_CAR(throw_args)))
	{
	  XEN_DISPLAY(XEN_CAR(throw_args), port);
	  XEN_PUTS(": ", port);
	  XEN_DISPLAY(tag, port); /* redundant in many cases */
	  need_comma = true;
	}
      /* else it's something like unbound variable which passes #f as car */
      if (XEN_LIST_LENGTH(throw_args) > 1)
	{
	  /* here XEN_CADR can contain formatting info and XEN_CADDR is a list of args to fit in */
	  /* or it may be a list of info vars etc */
	  if (need_comma) XEN_PUTS(": ", port);
	  if (XEN_STRING_P(XEN_CADR(throw_args)))
	    XEN_DISPLAY(snd_format_if_needed(XEN_CDR(throw_args)), port);
	  else XEN_DISPLAY(XEN_CDR(throw_args), port);
	}
    }
  else 
    {
      /* 'cannot-parse can get us here */
      XEN_DISPLAY(tag, port);
      XEN_PUTS(": ", port);
      XEN_DISPLAY(throw_args, port);
    }
#if HAVE_SCM_C_DEFINE
  stack = scm_fluid_ref(XEN_VARIABLE_REF(scm_the_last_stack_fluid_var));
#else
  stack = scm_fluid_ref(XEN_CDR(scm_the_last_stack_fluid));
#endif
  if (XEN_NOT_FALSE_P(stack)) 
    {
      stack_gc_loc = snd_protect(stack);
      if (show_backtrace(ss))
	{
	  XEN_PUTS("\n", port);
	  scm_display_backtrace(stack, port, XEN_UNDEFINED, XEN_UNDEFINED);
	}
      else
	{
	  XEN current_frame, source;
	  int frame_gc_loc, source_gc_loc;
	  current_frame = scm_stack_ref(stack, XEN_ZERO);
	  frame_gc_loc = snd_protect(current_frame);
	  if (XEN_NOT_FALSE_P(current_frame))
	    {
	      source = scm_frame_source(current_frame);
	      if (XEN_NOT_FALSE_P(source))
		{
		  source_gc_loc = snd_protect(source);
		  XEN_PUTS("\n", port);
		  XEN_PUTS(XEN_AS_STRING(scm_source_property(source, scm_sym_filename)), port);
		  XEN_PUTS(": line ", port);
		  XEN_PUTS(XEN_AS_STRING(scm_source_property(source, scm_sym_line)), port);
		  snd_unprotect_at(source_gc_loc);
		}
	    }
	  snd_unprotect_at(frame_gc_loc);
	}
      snd_unprotect_at(stack_gc_loc);
    }
  else
    {
      if (last_file_loaded)
	{
	  XEN_PUTS("\n(while loading \"", port);
	  XEN_PUTS(last_file_loaded, port);
	  XEN_PUTS("\")", port);
	  last_file_loaded = NULL;
	}
    }
  possible_code = (char *)data;
  if ((possible_code) && 
      (snd_strlen(possible_code) < PRINT_BUFFER_SIZE))
    {
      /* not actually sure if this is always safe */
      XEN_PUTS("\n; ", port);
      XEN_PUTS(possible_code, port);
    }
  XEN_FLUSH_PORT(port); /* needed to get rid of trailing garbage chars?? -- might be pointless now */
  name_buf = copy_string(XEN_TO_C_STRING(XEN_PORT_TO_STRING(port)));
  if (send_error_output_to_stdout)
    string_to_stdout(name_buf);
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
	      add_to_error_history(name_buf, false);
	    }
	  else snd_error(name_buf);
	}
    }
  snd_unprotect_at(port_gc_loc);
  if (name_buf) FREE(name_buf);
  check_for_event();
  return(tag);
}
#endif
/* end HAVE_GUILE */


/* ---------------- RUBY error handler ---------------- */

#if HAVE_RUBY
static char *msg = NULL;
void snd_rb_raise(XEN tag, XEN throw_args)
{
  XEN err = rb_eStandardError;
  bool need_comma = false;
  int size = 2048;
  if (strcmp(rb_id2name(tag), "Out_of_range") == 0) err = rb_eRangeError;
  if (msg) FREE(msg);
  msg = (char *)CALLOC(size, sizeof(char));
  if ((XEN_LIST_P(throw_args)) && 
      (XEN_LIST_LENGTH(throw_args) > 0))
    {
      /* normally car is string name of calling func */
      if (XEN_NOT_FALSE_P(XEN_CAR(throw_args)))
	{
	  snprintf(msg, 2048, "%s: %s", 
		   xen_scheme_procedure_to_ruby(XEN_AS_STRING(XEN_CAR(throw_args))), 
		   rb_id2name(tag));
	  need_comma = true;
	}
      if (XEN_LIST_LENGTH(throw_args) > 1)
	{
	  /* here XEN_CADR can contain formatting info and XEN_CADDR is a list of args to fit in */
	  /* or it may be a list of info vars etc */
	  if (need_comma) msg = snd_strcat(msg, ": ", &size);
	  if (XEN_STRING_P(XEN_CADR(throw_args)))
	    msg = snd_strcat(msg, XEN_TO_C_STRING(snd_format_if_needed(XEN_CDR(throw_args))), &size);
	  else msg = snd_strcat(msg, XEN_AS_STRING(XEN_CDR(throw_args)), &size);
	}
    }
  rb_raise(err, msg);
}
#endif
/* end HAVE_RUBY */

/* if error occurs in sndlib, mus-error wants to throw to user-defined catch
 *   (or our own global catch), but if the sndlib function was not called by the user, 
 *   the attempt to throw to a non-existent catch tag exits the main program!!
 *   so, we only throw if catch_exists.
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
  if (ss->catch_exists < 0) ss->catch_exists = 0;
  ss->catch_exists++;
  /* one function can invoke, for example, a hook that will call back here setting up a nested catch */
  result = scm_internal_stack_catch(tag, body, body_data, handler, handler_data);
  if (ss->catch_exists > 0) ss->catch_exists--;
  return(result);
}

XEN snd_throw(XEN key, XEN args)
{
  if (ss->catch_exists)
    return(XEN_THROW(key, args));
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
#if HAVE_RUBY
XEN snd_catch_any(XEN_CATCH_BODY_TYPE body, void *body_data, const char *caller)
{
  return((*body)(body_data));
}
#else
/* no extension language but user managed to try to evaluate something -- one way is to
 *   activate the minibuffer (via click) and type an expression into it
 */
XEN snd_catch_any(XEN_CATCH_BODY_TYPE body, void *body_data, const char *caller)
{
  snd_error("This version of Snd has no extension language, so there's no way for %s to evaluate anything", caller);
  return(XEN_FALSE);
}
#endif
#endif


bool procedure_arity_ok(XEN proc, int args)
{
  XEN arity;
  int rargs;
#if (!HAVE_RUBY)
  int oargs, restargs, loc;
#endif
  arity = XEN_ARITY(proc);
#if HAVE_RUBY
  rargs = XEN_TO_C_INT(arity);
  if ((rargs > args) ||
      ((rargs < 0) && (-rargs > args)))
    return(false);
#else
  loc = snd_protect(arity);
  rargs = XEN_TO_C_INT(XEN_CAR(arity));
  oargs = XEN_TO_C_INT(XEN_CADR(arity));
  restargs = ((XEN_TRUE_P(XEN_CADDR(arity))) ? 1 : 0);
  snd_unprotect_at(loc);
  if (rargs > args) return(false);
  if ((restargs == 0) && ((rargs + oargs) < args)) return(false);
#endif
  return(true);
}

char *procedure_ok(XEN proc, int args, const char *caller, const char *arg_name, int argn)
{
  /* if string returned, needs to be freed */
  XEN arity;
  int rargs;
#if (!HAVE_RUBY)
  int oargs, restargs, loc;
#endif
  if (!(XEN_PROCEDURE_P(proc)))
    {
      if (XEN_NOT_FALSE_P(proc)) /* #f as explicit arg to clear */
	return(mus_format("%s: %s (%s arg %d) is not a procedure!", 
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
      loc = snd_protect(arity);
      rargs = XEN_TO_C_INT(XEN_CAR(arity));
      oargs = XEN_TO_C_INT(XEN_CADR(arity));
      restargs = ((XEN_TRUE_P(XEN_CADDR(arity))) ? 1 : 0);
      snd_unprotect_at(loc);
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

bool procedure_ok_with_error(XEN proc, int req_args, const char *caller, const char *arg_name, int argn)
{
  char *errmsg;
  errmsg = procedure_ok(proc, req_args, caller, arg_name, argn);
  if (errmsg)
    {
      snd_error(errmsg);
      FREE(errmsg);
      return(false);
    }
  return(true);
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
  int index = -1;
  snd_info *sp;
  if (XEN_INTEGER_P(snd))
    index = XEN_TO_C_INT(snd);
  if ((index >= 0) &&
      (index < ss->max_sounds) && 
      (snd_ok(ss->sounds[index]))) /* good grief... */
    {
      sp = ss->sounds[index];
      XEN_ERROR(NO_SUCH_CHANNEL,
		XEN_LIST_3(C_TO_XEN_STRING(caller),
			   C_TO_XEN_STRING("chan: ~A, sound index: ~A (~A, chans: ~A)"),
			   XEN_LIST_4(chn, 
				      snd, 
				      C_TO_XEN_STRING(sp->short_filename), 
				      C_TO_XEN_INT(sp->nchans))));
    }
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
  return(XEN_CALL_0_NO_CATCH((XEN)arg, "call0"));
}
#endif

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
  return(XEN_CALL_1_NO_CATCH(((XEN *)arg)[0], ((XEN *)arg)[1], "call1"));
}
#endif

XEN g_call1(XEN proc, XEN arg, const char *caller)
{
#if HAVE_GUILE
  XEN args[2];
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
  return(XEN_APPLY_NO_CATCH(((XEN *)arg)[0], ((XEN *)arg)[1], "call any"));
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
  return(XEN_CALL_2_NO_CATCH(((XEN *)arg)[0], ((XEN *)arg)[1], ((XEN *)arg)[2], "call2"));
}
#endif

XEN g_call2(XEN proc, XEN arg1, XEN arg2, const char *caller)
{
#if HAVE_GUILE
  XEN args[3];
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
  return(XEN_CALL_3_NO_CATCH(((XEN *)arg)[0], ((XEN *)arg)[1], ((XEN *)arg)[2], ((XEN *)arg)[3], "call3"));
}
#endif

XEN g_call3(XEN proc, XEN arg1, XEN arg2, XEN arg3, const char *caller)
{
#if HAVE_GUILE
  XEN args[4];
  args[0] = proc;
  args[1] = arg1;
  args[2] = arg2;
  args[3] = arg3;
  return(snd_catch_any(g_call3_1, (void *)args, caller));
#else
  return(arg1);
#endif
}

char *g_print_1(XEN obj) /* free return val */
{
#if HAVE_GUILE
#if HAVE_SCM_OBJECT_TO_STRING
  return(copy_string(XEN_AS_STRING(obj))); 
#else
  XEN str, val;
  XEN port;
  str = scm_makstr (0, 0);
  port = scm_mkstrport (XEN_ZERO, str, SCM_OPN | SCM_WRTNG, S_snd_print);
  scm_prin1(obj, port, 1);
  val = XEN_PORT_TO_STRING(port);
  XEN_CLOSE_PORT(port);
  return(copy_string(XEN_TO_C_STRING(val)));
#endif
#endif
#if HAVE_RUBY
  if (XEN_NULL_P(obj))
    return(copy_string("nil")); /* Ruby returns the null string in this case??? */
  return(copy_string(XEN_AS_STRING(obj)));
#endif
}

static char *gl_print(XEN result)
{
  char *newbuf = NULL, *str = NULL;
  int i, ilen, savelen;
  /* specialize vectors which can be enormous in this context */
  if ((!(XEN_VECTOR_P(result))) || 
      ((int)(XEN_VECTOR_LENGTH(result)) <= print_length(ss)))
    return(g_print_1(result));
  ilen = print_length(ss); 
  newbuf = (char *)CALLOC(128, sizeof(char));
  savelen = 128;
#if HAVE_GUILE
  sprintf(newbuf, "#("); 
#else
  sprintf(newbuf, "[");
#endif
  for (i = 0; i < ilen; i++)
    {
      str = g_print_1(XEN_VECTOR_REF(result, i));
      if ((str) && (*str)) 
	{
	  if (i != 0) 
	    {
#if HAVE_RUBY
	      newbuf = snd_strcat(newbuf, ",", &savelen);
#endif
	      newbuf = snd_strcat(newbuf, " ", &savelen); 
	    }
	  newbuf = snd_strcat(newbuf, str, &savelen);
	  FREE(str);
	}
    }
#if HAVE_GUILE
  newbuf = snd_strcat(newbuf, " ...)", &savelen);
#else
  newbuf = snd_strcat(newbuf, " ...]", &savelen);
#endif
  return(newbuf);
}

XEN snd_eval_str(char *buf)
{
  return(snd_report_result(snd_catch_any(eval_str_wrapper, (void *)buf, buf), buf));
}

XEN snd_report_result(XEN result, char *buf)
{
  char *str = NULL;
  str = gl_print(result);
  if (ss->mx_sp)
    {
      snd_info *sp = NULL;
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
  int loc;
  char *str;
  if ((snd_strlen(buf) == 0) || ((snd_strlen(buf) == 1) && (buf[0] == '\n'))) return;
  result = snd_catch_any(eval_str_wrapper, (void *)buf, buf);
  loc = snd_protect(result);
  str = gl_print(result);
  listener_append_and_prompt(str);
  if (str) FREE(str);
  snd_unprotect_at(loc);
}

static char *stdin_str = NULL;

void clear_stdin(void)
{
  if (stdin_str) FREE(stdin_str);
  stdin_str = NULL;
}

static char *stdin_check_for_full_expression(char *newstr)
{
#if HAVE_GUILE
  int end_of_text;
#endif
  if (stdin_str)
    {
      char *str;
      str = stdin_str;
      stdin_str = (char *)CALLOC(snd_strlen(str) + snd_strlen(newstr) + 2, sizeof(char));
      strcat(stdin_str, str);
      strcat(stdin_str, newstr);
      FREE(str);
    }
  else stdin_str = copy_string(newstr);
#if HAVE_GUILE
  end_of_text = check_balance(stdin_str, 0, snd_strlen(stdin_str), false); /* last-arg->not in listener */
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

void snd_eval_stdin_str(char *buf)
{
  /* we may get incomplete expressions here */
  /*   (Ilisp always sends a complete expression, but it may be broken into two or more pieces from read's point of view) */
  char *str = NULL;
  if (snd_strlen(buf) == 0) return;
  str = stdin_check_for_full_expression(buf);
  if (str)
    {
      XEN result;
      int loc;
      send_error_output_to_stdout = true;
      result = snd_catch_any(eval_str_wrapper, (void *)str, str);
      loc = snd_protect(result);
      send_error_output_to_stdout = false;
      if (stdin_str) FREE(stdin_str);
      /* same as str here; if c-g! evaluated from stdin, clear_listener is called which frees/nullifies stdin_str */
      stdin_str = NULL;
      str = gl_print(result);
      string_to_stdout(str);
      if (str) FREE(str);
      snd_unprotect_at(loc);
    }
}

void snd_load_init_file(bool no_global, bool no_init)
{
  /* look for ".snd" on the home directory */
  /* called only in snd-xmain.c at initialization time */
  int fd;
  XEN result = XEN_TRUE;
  char *str = NULL;
  #define SND_CONF "/etc/snd.conf"
  if (!no_global)
    {
      fd = OPEN(SND_CONF, O_RDONLY, 0);
      if (fd != -1)
	{
	  snd_close(fd, SND_CONF);
	  result = snd_catch_any(eval_file_wrapper, (void *)SND_CONF, "(load " SND_CONF ")");
	}
    }
  if ((ss->init_file) && (!no_init))
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
      int loc;
      loc = snd_protect(result);
      str = gl_print(result);
      if (str)
	{
	  snd_error(str);
	  FREE(str);
	}
      snd_unprotect_at(loc);
    }
#endif
}

void snd_load_file(char *filename)
{
  char *str = NULL, *str1 = NULL, *str2 = NULL;
  XEN result = XEN_TRUE;
  str = mus_expand_filename(filename);
#if (!HAVE_RUBY)
  str2 = mus_format("(load \"%s\")", filename);
#endif
  if (!mus_file_probe(str))
    {
      /* try tacking on extension */
      str1 = mus_format("%s.%s", str, XEN_FILE_EXTENSION);
      if (!mus_file_probe(str1))
	{
	  FREE(str);
	  str = NULL;
	  FREE(str1);
	  str1 = NULL;
#if (!HAVE_RUBY)
	  FREE(str2);
	  str2 = NULL;
#endif
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
      int loc;
      loc = snd_protect(result);
      str = gl_print(result);
      if (str)
	{
	  snd_error(str);
	  FREE(str);
	}
      snd_unprotect_at(loc);
    }
#endif
}

static XEN g_snd_print(XEN msg)
{
  #define H_snd_print "(" S_snd_print " str): display str in the lisp listener window"
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
  check_for_event();
#endif
  return(msg);
}

static XEN print_hook;
static int print_depth = 0;

bool listener_print_p(char *msg)
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

static XEN g_region_graph_style(void) {return(C_TO_XEN_INT(region_graph_style(ss)));}
static XEN g_set_region_graph_style(XEN val) 
{
  graph_style_t style;
  #define H_region_graph_style "(" S_region_graph_style "): graph style of the region dialog graph. \
The " S_region_graph_style " choices are " S_graph_lines ", " S_graph_dots ", " S_graph_filled ", " S_graph_lollipops ", \
and " S_graph_dots_and_lines "."
  XEN_ASSERT_TYPE(XEN_INTEGER_P(val), val, XEN_ONLY_ARG, S_setB S_region_graph_style, "an integer");
  style = (graph_style_t)XEN_TO_C_INT(val);
  if (!(GRAPH_STYLE_OK(style)))
    XEN_OUT_OF_RANGE_ERROR(S_setB S_region_graph_style, 1, val, "~A: unknown " S_lisp_graph_style);
  else
    {
      set_region_graph_style(style);
      reflect_region_graph_style();
    }
  return(val);
}

static XEN g_ask_before_overwrite(void) {return(C_TO_XEN_BOOLEAN(ask_before_overwrite(ss)));}
static XEN g_set_ask_before_overwrite(XEN val) 
{
  #define H_ask_before_overwrite "(" S_ask_before_overwrite "): #t if you want Snd to ask before overwriting a file. \
If #f, any existing file of the same name will be overwritten without warning when you save a sound."
  XEN_ASSERT_TYPE(XEN_BOOLEAN_P(val), val, XEN_ONLY_ARG, S_setB S_ask_before_overwrite, "a boolean");
  set_ask_before_overwrite(XEN_TO_C_BOOLEAN(val)); 
  return(C_TO_XEN_BOOLEAN(ask_before_overwrite(ss)));
}

static XEN g_audio_output_device(void) {return(C_TO_XEN_INT(audio_output_device(ss)));}
static XEN g_set_audio_output_device(XEN val) 
{
  #define H_audio_output_device "(" S_audio_output_device "): the current sndlib default output device (" S_mus_audio_default ")"
  XEN_ASSERT_TYPE(XEN_INTEGER_P(val), val, XEN_ONLY_ARG, S_setB S_audio_output_device, "an integer"); 
  set_audio_output_device(XEN_TO_C_INT(val)); 
  return(C_TO_XEN_INT(audio_output_device(ss)));
}

static XEN g_audio_input_device(void) {return(C_TO_XEN_INT(audio_input_device(ss)));}
static XEN g_set_audio_input_device(XEN val) 
{
  #define H_audio_input_device "(" S_audio_input_device "): the current sndlib default input device (" S_mus_audio_default ")"
  XEN_ASSERT_TYPE(XEN_INTEGER_P(val), val, XEN_ONLY_ARG, S_setB S_audio_input_device, "an integer"); 
  set_audio_input_device(XEN_TO_C_INT(val)); 
  return(C_TO_XEN_INT(audio_input_device(ss)));
}

static XEN g_minibuffer_history_length(void) {return(C_TO_XEN_INT(minibuffer_history_length(ss)));}
static XEN g_set_minibuffer_history_length(XEN val) 
{
  #define H_minibuffer_history_length "(" S_minibuffer_history_length "): the minibuffer history length. \
This pertains to the M-p and M-n commands."
  int len;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(val), val, XEN_ONLY_ARG, S_setB S_minibuffer_history_length, "an integer");
  len = XEN_TO_C_INT(val);
  if (len > 0)
    set_minibuffer_history_length(len);
  return(C_TO_XEN_INT(minibuffer_history_length(ss)));
}

static XEN g_emacs_style_save_as(void) {return(C_TO_XEN_BOOLEAN(emacs_style_save_as(ss)));}
static XEN g_set_emacs_style_save_as(XEN val) 
{
  #define H_emacs_style_save_as "(" S_emacs_style_save_as "): #t if File:Save-as dialog option should move to the new file (default: #f)"
  XEN_ASSERT_TYPE(XEN_BOOLEAN_P(val), val, XEN_ONLY_ARG, S_setB S_emacs_style_save_as, "a boolean");
  set_emacs_style_save_as(XEN_TO_C_BOOLEAN(val)); 
  return(C_TO_XEN_BOOLEAN(emacs_style_save_as(ss)));
}

static XEN g_auto_resize(void) {return(C_TO_XEN_BOOLEAN(auto_resize(ss)));}
static XEN g_set_auto_resize(XEN val) 
{
  #define H_auto_resize "(" S_auto_resize "): #t if Snd can change its main window size as it pleases (default: #t)"
  XEN_ASSERT_TYPE(XEN_BOOLEAN_P(val), val, XEN_ONLY_ARG, S_setB S_auto_resize, "a boolean");
  set_auto_resize(XEN_TO_C_BOOLEAN(val)); 
#if USE_MOTIF
  XtVaSetValues(MAIN_SHELL(ss), XmNallowShellResize, auto_resize(ss), NULL);
#endif
  return(C_TO_XEN_BOOLEAN(auto_resize(ss)));
}

static XEN g_auto_update(void) {return(C_TO_XEN_BOOLEAN(auto_update(ss)));}
static XEN g_set_auto_update(XEN val) 
{
  #define H_auto_update "(" S_auto_update "): #t if Snd should automatically update a file if it changes unexpectedly (default: #f). \
The number of seconds between update checks is set by " S_auto_update_interval "."
  XEN_ASSERT_TYPE(XEN_BOOLEAN_P(val), val, XEN_ONLY_ARG, S_setB S_auto_update, "a boolean");
  set_auto_update(XEN_TO_C_BOOLEAN(val)); 
  return(C_TO_XEN_BOOLEAN(auto_update(ss)));
}

static XEN g_color_cutoff(void) {return(C_TO_XEN_DOUBLE(color_cutoff(ss)));}
static XEN g_set_color_cutoff(XEN val) 
{
  #define H_color_cutoff "(" S_color_cutoff "): color map cutoff point (default .003).  Any values \
below the cutoff are displayed in the background color"
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ONLY_ARG, S_setB S_color_cutoff, "a number");
  set_color_cutoff(mus_fclamp(0.0,
			      XEN_TO_C_DOUBLE(val),
			      0.25)); 
  return(C_TO_XEN_DOUBLE(color_cutoff(ss)));
}

static XEN g_color_inverted(void) {return(C_TO_XEN_BOOLEAN(color_inverted(ss)));}
static XEN g_set_color_inverted(XEN val) 
{
  #define H_color_inverted "(" S_color_inverted "): whether the colormap in operation should be inverted"
  XEN_ASSERT_TYPE(XEN_BOOLEAN_P(val), val, XEN_ONLY_ARG, S_setB S_color_inverted, "a boolean");
  set_color_inverted(XEN_TO_C_BOOLEAN(val)); 
  return(C_TO_XEN_BOOLEAN(color_inverted(ss)));
}

static XEN g_color_scale(void) {return(C_TO_XEN_DOUBLE(color_scale(ss)));}
static XEN g_set_color_scale(XEN val) 
{
  #define H_color_scale "(" S_color_scale "): darkness setting for colormaps (0.5)"
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ONLY_ARG, S_setB S_color_scale, "a number"); 
  set_color_scale(mus_fclamp(0.0,
			     XEN_TO_C_DOUBLE(val),
			     1000.0)); 
  return(C_TO_XEN_DOUBLE(color_scale(ss)));
}

static XEN g_auto_update_interval(void) {return(C_TO_XEN_DOUBLE(auto_update_interval(ss)));}
static XEN g_set_auto_update_interval(XEN val) 
{
  Float ctime, old_time;
  #define H_auto_update_interval "(" S_auto_update_interval "): time (seconds) between background checks for changed file on disk (default: 60). \
This value only matters if " S_auto_update " is #t"
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ONLY_ARG, S_setB S_auto_update_interval, "a number"); 
  ctime = XEN_TO_C_DOUBLE(val);
  if ((ctime < 0.0) || (ctime > (24 * 3600)))
    XEN_OUT_OF_RANGE_ERROR(S_setB S_auto_update_interval, 1, val, "~A: invalid time");
  old_time = auto_update_interval(ss);
  set_auto_update_interval(ctime);
  /* if new value is 0.0, auto_update_check will notice that, and not run or re-start the update check */
  /* if new value is not 0.0, and old value was 0.0, we need to restart the timeout proc, unless it's still on the queue */
  if ((ctime > 0.0) && (old_time == 0.0))
    auto_update_restart();
  return(C_TO_XEN_DOUBLE(auto_update_interval(ss)));
}

static XEN g_default_output_chans(void) {return(C_TO_XEN_INT(default_output_chans(ss)));}
static XEN g_set_default_output_chans(XEN val) 
{
  #define MAX_OUTPUT_CHANS 1024
  #define H_default_output_chans "(" S_default_output_chans "): default number of channels when a new or temporary file is created (1)"
  XEN_ASSERT_TYPE(XEN_INTEGER_P(val), val, XEN_ONLY_ARG, S_setB S_default_output_chans, "an integer"); 
  set_default_output_chans(mus_iclamp(1, XEN_TO_C_INT(val), MAX_OUTPUT_CHANS));
  return(C_TO_XEN_INT(default_output_chans(ss)));
}

static XEN g_default_output_srate(void) {return(C_TO_XEN_INT(default_output_srate(ss)));}
static XEN g_set_default_output_srate(XEN val) 
{
  #define MAX_OUTPUT_SRATE 1000000000
  #define H_default_output_srate "(" S_default_output_srate "): default srate when a new or temporary file is created (22050)" 
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ONLY_ARG, S_setB S_default_output_srate, "a number"); 
  set_default_output_srate(mus_iclamp(1, XEN_TO_C_INT_OR_ELSE(val, 0), MAX_OUTPUT_SRATE));
  return(C_TO_XEN_INT(default_output_srate(ss)));
}

static XEN g_default_output_type(void) {return(C_TO_XEN_INT(default_output_type(ss)));}
static XEN g_set_default_output_type(XEN val) 
{
  int typ;
  #define H_default_output_type "(" S_default_output_type "): default header type when a new or temporary file is created. \
Normally this is " S_mus_next "; -1 here indicates you want Snd to use the current sound's header type, if possible. \
Other writable headers include " S_mus_aiff ", " S_mus_riff ", " S_mus_ircam ", " S_mus_nist ", " S_mus_aifc ", and " S_mus_raw "."
  XEN_ASSERT_TYPE(XEN_INTEGER_P(val), val, XEN_ONLY_ARG, S_setB S_default_output_type, "an integer"); 
  typ = XEN_TO_C_INT(val);
  if (mus_header_writable(typ, -2))
    set_default_output_type(typ); 
  else XEN_OUT_OF_RANGE_ERROR(S_setB S_default_output_type, 1, val, "~A: unwritable header type");
  return(C_TO_XEN_INT(default_output_type(ss)));
}

static XEN g_default_output_format(void) {return(C_TO_XEN_INT(default_output_format(ss)));}
static XEN g_set_default_output_format(XEN val) 
{
  int format;
  #define H_default_output_format "(" S_default_output_format "): default data format when a new or temporary file is created, \
normally " S_mus_bshort "; -1 here means try to use the current sound's data format; many other formats \
are available, but not all are compatible with all header types"
  XEN_ASSERT_TYPE(XEN_INTEGER_P(val), val, XEN_ONLY_ARG, S_setB S_default_output_format, "an integer"); 
  format = XEN_TO_C_INT(val);
  if (MUS_DATA_FORMAT_OK(format))
    set_default_output_format(format); 
  else XEN_OUT_OF_RANGE_ERROR(S_setB S_default_output_format, 1, val, "~A: unknown data format");
  return(C_TO_XEN_INT(default_output_format(ss)));
}

static XEN g_selection_creates_region(void) {return(C_TO_XEN_BOOLEAN(selection_creates_region(ss)));}
static XEN g_set_selection_creates_region(XEN val) 
{
  #define H_selection_creates_region "(" S_selection_creates_region "): #t if a region should be created each time a selection is made. \
The default is currently #t, but that may change.  If you're dealing with large selections, and have no need of \
regions (saved selections), you can speed up many operations by setting this flag to #f"
  XEN_ASSERT_TYPE(XEN_BOOLEAN_P(val), val, XEN_ONLY_ARG, S_setB S_selection_creates_region, "a boolean");
  set_selection_creates_region(XEN_TO_C_BOOLEAN(val));
  return(C_TO_XEN_BOOLEAN(selection_creates_region(ss)));
}

static XEN g_print_length(void) {return(C_TO_XEN_INT(print_length(ss)));}
static XEN g_set_print_length(XEN val) 
{
  int len;
  #define H_print_length "(" S_print_length "): number of vector elements to print in the listener (default: 12)"
  XEN_ASSERT_TYPE(XEN_INTEGER_P(val), val, XEN_ONLY_ARG, S_setB S_print_length, "an integer"); 
  len = XEN_TO_C_INT(val);
  if (len < 0)
    XEN_OUT_OF_RANGE_ERROR(S_setB S_print_length, XEN_ONLY_ARG, val, "must be >= 0");
  set_print_length(len);
  set_vct_print_length(len);
  return(C_TO_XEN_INT(print_length(ss)));
}

static XEN g_show_indices(void) {return(C_TO_XEN_BOOLEAN(show_indices(ss)));}
static XEN g_set_show_indices(XEN val) 
{
  #define H_show_indices "(" S_show_indices "): #t if sound name should be preceded by its index in the sound display."
  XEN_ASSERT_TYPE(XEN_BOOLEAN_P(val), val, XEN_ONLY_ARG, S_setB S_show_indices, "a boolean");
  set_show_indices(XEN_TO_C_BOOLEAN(val));
  return(C_TO_XEN_BOOLEAN(show_indices(ss)));
}

static XEN g_show_backtrace(void) {return(C_TO_XEN_BOOLEAN(show_backtrace(ss)));}
static XEN g_set_show_backtrace(XEN val) 
{
  #define H_show_backtrace "(" S_show_backtrace "): #t to show backtrace automatically upon error"
  XEN_ASSERT_TYPE(XEN_BOOLEAN_P(val), val, XEN_ONLY_ARG, S_setB S_show_backtrace, "a boolean");
  set_show_backtrace(XEN_TO_C_BOOLEAN(val));
  return(C_TO_XEN_BOOLEAN(show_backtrace(ss)));
}

static int snd_access(char *dir, char *caller)
{
  int err;
  char *temp;
  temp = shorter_tempnam(dir, "snd_");
  err = mus_file_create(temp);
  if (err == -1)
    {
      XEN res;
      FREE(temp);
      temp = mus_format(_("%s: directory %s is not writable: %s"), caller, dir, strerror(errno));
      res = C_TO_XEN_STRING(temp);
      FREE(temp);
      XEN_ERROR(NO_SUCH_FILE,
		XEN_LIST_1(res));
    }
  else snd_close(err, temp);
  snd_remove(temp, IGNORE_CACHE);
  FREE(temp);
  return(1);
}

static XEN g_temp_dir(void) {return(C_TO_XEN_STRING(temp_dir(ss)));}
static XEN g_set_temp_dir(XEN val) 
{
  #define H_temp_dir "(" S_temp_dir "): name of directory for temp files (or #f=null)"
  char *dir = DEFAULT_TEMP_DIR;
  XEN_ASSERT_TYPE(XEN_STRING_P(val) || XEN_FALSE_P(val), val, XEN_ONLY_ARG, S_setB S_temp_dir, "a string or #f=default (null)"); 
  if (XEN_STRING_P(val)) dir = XEN_TO_C_STRING(val);
  if (snd_access(dir, S_temp_dir))
    {
      if (temp_dir(ss)) FREE(temp_dir(ss));
      set_temp_dir(copy_string(dir));
    }
  return(C_TO_XEN_STRING(temp_dir(ss)));
}

static XEN g_snd_tempnam(void) 
{
  #define H_snd_tempnam "(" S_snd_tempnam "): return a new temp file name using " S_temp_dir "."
  char *tmp;
  XEN res;
  tmp = snd_tempnam();
  res = C_TO_XEN_STRING(tmp);
  FREE(tmp);
  return(res);
}

static XEN g_save_dir(void) {return(C_TO_XEN_STRING(save_dir(ss)));}
static XEN g_set_save_dir(XEN val) 
{
  #define H_save_dir "(" S_save_dir "): name of directory for saved state data (or #f=null)"
  char *dir = DEFAULT_SAVE_DIR;
  XEN_ASSERT_TYPE(XEN_STRING_P(val) || XEN_FALSE_P(val), val, XEN_ONLY_ARG, S_setB S_save_dir, "a string or #f=default (null)"); 
  if (XEN_STRING_P(val)) dir = XEN_TO_C_STRING(val);
  if (snd_access(dir, S_save_dir))
    {
      if (save_dir(ss)) FREE(save_dir(ss));
      set_save_dir(copy_string(dir));
    }
  return(C_TO_XEN_STRING(save_dir(ss)));
}

static XEN g_ladspa_dir(void) {return(C_TO_XEN_STRING(ladspa_dir(ss)));}
static XEN g_set_ladspa_dir(XEN val) 
{
  #define H_ladspa_dir "(" S_ladspa_dir "): name of directory for ladspa plugin libraries"
  XEN_ASSERT_TYPE(XEN_STRING_P(val) || XEN_FALSE_P(val), val, XEN_ONLY_ARG, S_setB S_ladspa_dir, "a string or #f=default (null)"); 
  if (ladspa_dir(ss)) FREE(ladspa_dir(ss));
  if (XEN_FALSE_P(val))
    set_ladspa_dir((DEFAULT_LADSPA_DIR) ? copy_string(DEFAULT_LADSPA_DIR) : NULL);
  else set_ladspa_dir(copy_string(XEN_TO_C_STRING(val)));
  return(C_TO_XEN_STRING(ladspa_dir(ss)));
}

static XEN g_trap_segfault(void) {return(C_TO_XEN_BOOLEAN(trap_segfault(ss)));}
static XEN g_set_trap_segfault(XEN val) 
{
  #define H_trap_segfault "(" S_trap_segfault "): #t if Snd should try to trap (and whine about) segfaults"
  XEN_ASSERT_TYPE(XEN_BOOLEAN_P(val), val, XEN_ONLY_ARG, S_setB S_trap_segfault, "a boolean");
  set_trap_segfault(XEN_TO_C_BOOLEAN(val));
  return(C_TO_XEN_BOOLEAN(trap_segfault(ss)));
}

static XEN g_show_selection_transform(void) {return(C_TO_XEN_BOOLEAN(show_selection_transform(ss)));}
static XEN g_set_show_selection_transform(XEN val) 
{
  #define H_show_selection_transform "(" S_show_selection_transform "): #t if transform display reflects selection, not time-domain window"
  XEN_ASSERT_TYPE(XEN_BOOLEAN_P(val), val, XEN_ONLY_ARG, S_setB S_show_selection_transform, "a boolean");
  set_show_selection_transform(XEN_TO_C_BOOLEAN(val));
  return(C_TO_XEN_BOOLEAN(show_selection_transform(ss)));
}

static void update_log_freq_fft_graph(chan_info *cp)
{
  if ((!(cp->active)) ||
      (cp->cgx == NULL) || 
      (cp->sounds == NULL) || 
      (cp->sounds[cp->sound_ctr] == NULL) ||
      (!(cp->graph_transform_p)) ||
      (!(cp->fft_log_frequency)) ||
      (chan_fft_in_progress(cp)))
    return;
  calculate_fft(cp);
}

void set_log_freq_start(Float base)
{
  in_set_log_freq_start(base);
  for_each_chan(update_log_freq_fft_graph);
}

static XEN g_log_freq_start(void) {return(C_TO_XEN_DOUBLE(log_freq_start(ss)));}
static XEN g_set_log_freq_start(XEN val) 
{
  Float base;
  #define H_log_freq_start "(" S_log_freq_start "): log freq base (default: 25.0)"
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ONLY_ARG, S_setB S_log_freq_start, "a number");
  base = XEN_TO_C_DOUBLE(val);
  if (base < 0.0)
    XEN_OUT_OF_RANGE_ERROR(S_log_freq_start, XEN_ONLY_ARG, val, "a number >= 0.0");
  if (base > 100000.0)
    XEN_OUT_OF_RANGE_ERROR(S_log_freq_start, XEN_ONLY_ARG, val, "a number < srate/2");
  set_log_freq_start(base);
  reflect_log_freq_start_in_transform_dialog();
  return(C_TO_XEN_DOUBLE(log_freq_start(ss)));
}

static XEN g_with_gl(void) {return(C_TO_XEN_BOOLEAN(with_gl(ss)));}
static XEN g_set_with_gl(XEN val) 
{
  #define H_with_gl "(" S_with_gl "): #t if Snd should use GL graphics"
  XEN_ASSERT_TYPE(XEN_BOOLEAN_P(val), val, XEN_ONLY_ARG, S_setB S_with_gl, "a boolean");
#if HAVE_GL
  set_with_gl(XEN_TO_C_BOOLEAN(val));
  for_each_chan(update_graph);
#endif
  return(C_TO_XEN_BOOLEAN(with_gl(ss)));
}

static XEN g_with_relative_panes(void) {return(C_TO_XEN_BOOLEAN(with_relative_panes(ss)));}
static XEN g_set_with_relative_panes(XEN val) 
{
  #define H_with_relative_panes "(" S_with_relative_panes "): #t if multichannel sounds should try to maintain relative pane sizes"
  XEN_ASSERT_TYPE(XEN_BOOLEAN_P(val), val, XEN_ONLY_ARG, S_setB S_with_relative_panes, "a boolean");
  set_with_relative_panes(XEN_TO_C_BOOLEAN(val));
  return(C_TO_XEN_BOOLEAN(with_relative_panes(ss)));
}

static XEN g_with_background_processes(void) {return(C_TO_XEN_BOOLEAN(with_background_processes(ss)));}
static XEN g_set_with_background_processes(XEN val) 
{
  #define H_with_background_processes "(" S_with_background_processes "): #t if Snd should use background (idle time) processing"
  XEN_ASSERT_TYPE(XEN_BOOLEAN_P(val), val, XEN_ONLY_ARG, S_setB S_with_background_processes, "a boolean");
  set_with_background_processes(XEN_TO_C_BOOLEAN(val));
  return(C_TO_XEN_BOOLEAN(with_background_processes(ss)));
}

/* data-clipped -> clip-data? -- this is from sndlib */
static XEN g_data_clipped(void) {return(C_TO_XEN_BOOLEAN(data_clipped(ss)));}
static XEN g_set_data_clipped(XEN val) 
{
  #define H_data_clipped "(" S_data_clipped "): #t if Snd should clip output values to the current \
output data format's maximum. The default (#f) allows them to wrap-around which makes a very loud click"
  XEN_ASSERT_TYPE(XEN_BOOLEAN_P(val), val, XEN_ONLY_ARG, S_setB S_data_clipped, "a boolean");
  set_data_clipped(XEN_TO_C_BOOLEAN(val));
  return(C_TO_XEN_BOOLEAN(data_clipped(ss)));
}

static XEN g_zoom_focus_style(void) {return(C_TO_XEN_INT((int)zoom_focus_style(ss)));}
static XEN g_set_zoom_focus_style(XEN focus) 
{
  #define H_zoom_focus_style "(" S_zoom_focus_style "): one of " S_zoom_focus_left ", " S_zoom_focus_right ", " S_zoom_focus_middle \
", or " S_zoom_focus_active ". This determines what zooming centers on (default: " S_zoom_focus_active ")"
  zoom_focus_t choice;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(focus), focus, XEN_ONLY_ARG, S_setB S_zoom_focus_style, "an integer"); 
  choice = (zoom_focus_t)XEN_TO_C_INT(focus);
  if ((choice < ZOOM_FOCUS_LEFT) || (choice > ZOOM_FOCUS_MIDDLE))
    XEN_OUT_OF_RANGE_ERROR(S_setB S_zoom_focus_style, 
			   1, focus, 
			   "~A, but must be " S_zoom_focus_left ", " S_zoom_focus_right ", " S_zoom_focus_middle ", or " S_zoom_focus_active);
  activate_focus_menu(choice);
  return(C_TO_XEN_INT((int)zoom_focus_style(ss)));
}

static XEN g_snd_version(void) 
{
  #define H_snd_version "(" S_snd_version "): current Snd version (a string)"
  return(C_TO_XEN_STRING(SND_DATE));
}

static XEN g_sounds(void)
{
  #define H_sounds "(" S_sounds "): list of active sounds (a list of indices)"
  int i;
  XEN result;
  result = XEN_EMPTY_LIST;
  for (i = 0; i < ss->max_sounds; i++)
    {
      snd_info *sp;
      sp = ss->sounds[i];
      if ((sp) && (sp->inuse == SOUND_NORMAL))
	result = XEN_CONS(C_TO_XEN_INT(i),
			  result);
    }
  return(result);
}

static XEN g_tiny_font(void) {return(C_TO_XEN_STRING(tiny_font(ss)));}
static XEN g_set_tiny_font(XEN val) 
{
  #define H_tiny_font "(" S_tiny_font "): font use for some info in the graphs"
  XEN_ASSERT_TYPE(XEN_STRING_P(val), val, XEN_ONLY_ARG, S_setB S_tiny_font, "a string"); 
  set_tiny_font(XEN_TO_C_STRING(val)); 
  return(C_TO_XEN_STRING(tiny_font(ss)));
}

static XEN g_axis_label_font(void) {return(C_TO_XEN_STRING(axis_label_font(ss)));}
static XEN g_set_axis_label_font(XEN val) 
{
  #define H_axis_label_font "(" S_axis_label_font "): font used for axis labels"
  XEN_ASSERT_TYPE(XEN_STRING_P(val), val, XEN_ONLY_ARG, S_setB S_axis_label_font, "a string"); 
  set_axis_label_font(XEN_TO_C_STRING(val)); 
  return(C_TO_XEN_STRING(axis_label_font(ss)));
}

static XEN g_axis_numbers_font(void) {return(C_TO_XEN_STRING(axis_numbers_font(ss)));}
static XEN g_set_axis_numbers_font(XEN val) 
{
  #define H_axis_numbers_font "(" S_axis_numbers_font "): font used for axis numbers"
  XEN_ASSERT_TYPE(XEN_STRING_P(val), val, XEN_ONLY_ARG, S_setB S_axis_numbers_font, "a string"); 
  set_axis_numbers_font(XEN_TO_C_STRING(val)); 
  return(C_TO_XEN_STRING(axis_numbers_font(ss)));
}

static XEN g_listener_font(void) {return(C_TO_XEN_STRING(listener_font(ss)));}
static XEN g_set_listener_font(XEN val) 
{
  #define H_listener_font "(" S_listener_font "): font used by the lisp listener"
  XEN_ASSERT_TYPE(XEN_STRING_P(val), val, XEN_ONLY_ARG, S_setB S_listener_font, "a string");
  set_listener_font(XEN_TO_C_STRING(val)); 
  return(C_TO_XEN_STRING(listener_font(ss)));
}

static XEN g_bold_peaks_font(void) {return(C_TO_XEN_STRING(bold_peaks_font(ss)));}
static XEN g_set_bold_peaks_font(XEN val) 
{
  #define H_bold_peaks_font "(" S_bold_peaks_font "): bold font used by fft peak display"
  XEN_ASSERT_TYPE(XEN_STRING_P(val), val, XEN_ONLY_ARG, S_setB S_bold_peaks_font, "a string"); 
  set_bold_peaks_font(XEN_TO_C_STRING(val)); 
  return(C_TO_XEN_STRING(bold_peaks_font(ss)));
}

static XEN g_peaks_font(void) {return(C_TO_XEN_STRING(peaks_font(ss)));}
static XEN g_set_peaks_font(XEN val) 
{
  #define H_peaks_font "(" S_peaks_font "): normal font used by fft peak display"
  XEN_ASSERT_TYPE(XEN_STRING_P(val), val, XEN_ONLY_ARG, S_setB S_peaks_font, "a string"); 
  set_peaks_font(XEN_TO_C_STRING(val)); 
  return(C_TO_XEN_STRING(peaks_font(ss)));
}

static XEN g_window_width(void) 
{
  #define H_window_width "(" S_window_width "): current Snd window width in pixels"
  return(C_TO_XEN_INT(widget_width(MAIN_SHELL(ss))));
}

static XEN g_window_height(void) 
{
  #define H_window_height "(" S_window_height "): current Snd window height in pixels"
  return(C_TO_XEN_INT(widget_height(MAIN_SHELL(ss))));
}

static XEN g_window_x(void) 
{
  #define H_window_x "(" S_window_x "): current Snd window x position in pixels"
  return(C_TO_XEN_INT(widget_x(MAIN_SHELL(ss))));
}

static XEN g_window_y(void) 
{
  #define H_window_y "(" S_window_y "): current Snd window y position in pixels"
  return(C_TO_XEN_INT(widget_y(MAIN_SHELL(ss))));
}

static int snd_screen_height(void)
{
#if USE_MOTIF
  return(HeightOfScreen(ScreenOfDisplay(MAIN_DISPLAY(ss), 0)));
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
  return(WidthOfScreen(ScreenOfDisplay(MAIN_DISPLAY(ss), 0)));
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
  XEN_ASSERT_TYPE(XEN_NUMBER_P(height), height, XEN_ONLY_ARG, S_setB S_window_height, "a number"); 
  val = (Latus)XEN_TO_C_INT_OR_ELSE(height, 0);
  if ((val > 0) && (val < snd_screen_height()))
    {
#if (!USE_NO_GUI)
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
  XEN_ASSERT_TYPE(XEN_NUMBER_P(width), width, XEN_ONLY_ARG, S_setB S_window_width, "a number"); 
  val = (Latus)XEN_TO_C_INT_OR_ELSE(width, 0);
  if ((val > 0) && (val < snd_screen_width()))
    {
#if (!USE_NO_GUI)
      set_widget_width(MAIN_SHELL(ss), val);
#endif
      ss->init_window_width = val;
    }
  return(width);
}

static XEN g_set_window_x(XEN val) 
{
  Locus x;
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
  Locus y;
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
  #define H_abort "(" S_abort "): exit Snd via \"abort\", presumably to land in the debugger"
  abort();
  return(XEN_FALSE);
}

static XEN g_abortq(void)
{
  #define H_abortQ "(" S_c_g "): allow pending user interface events to occur, returning #t if C-g was typed"
  check_for_event();
  if (ss->stopped_explicitly)
    {
      ss->stopped_explicitly = false;
      return(XEN_TRUE);
    }
  return(XEN_FALSE);
}

snd_info *get_sp(XEN x_snd_n, sp_sound_t accept_player)
{
  /* if x_snd_n is a number, it is sp->index */
  if (XEN_INTEGER_P(x_snd_n))
    {
      int snd_n;
      snd_n = XEN_TO_C_INT(x_snd_n);
      if (snd_n >= 0)
	{
	  if ((snd_n < ss->max_sounds) && 
	      (snd_ok(ss->sounds[snd_n])))
	    return(ss->sounds[snd_n]);
	}
      else
	{
	  if (accept_player == PLAYERS_OK)
	    return(player(snd_n));
	}
      return(NULL);
    }
  /* use default sound, if any */
  return(any_selected_sound());
}

chan_info *get_cp(XEN x_snd_n, XEN x_chn_n, const char *caller)
{
  snd_info *sp;
  int chn_n;
  sp = get_sp(x_snd_n, NO_PLAYERS);
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

static XEN g_samples_to_sound_data(XEN samp_0, XEN samps, XEN snd_n, XEN chn_n, XEN sdobj, XEN edpos, XEN sdchan)
{
  #define H_samples_to_sound_data "(" S_samples_to_sound_data " (start-samp 0) (samps len) (snd #f) (chn #f) (sdobj #f) (edpos #f) (sdobj-chan 0)): \
return a sound-data object (sdobj if given) containing snd channel chn's data starting at start-samp for samps, \
reading edit version edpos"

  chan_info *cp;
  XEN newsd = XEN_FALSE;
  int i, len, pos, maxlen = 0, loc = -1;
  off_t beg;
  XEN_ASSERT_TYPE(XEN_NUMBER_IF_BOUND_P(samp_0), samp_0, XEN_ARG_1, S_samples_to_sound_data, "a number");
  XEN_ASSERT_TYPE(XEN_NUMBER_IF_BOUND_P(samps), samps, XEN_ARG_2, S_samples_to_sound_data, "a number");
  ASSERT_CHANNEL(S_samples_to_sound_data, snd_n, chn_n, 3);
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(sdchan), sdchan, XEN_ARG_7, S_samples_to_sound_data, "an integer");
  cp = get_cp(snd_n, chn_n, S_samples_to_sound_data);
  pos = to_c_edit_position(cp, edpos, S_samples_to_sound_data, 6);
  beg = beg_to_sample(samp_0, S_samples_to_sound_data);
  maxlen = (int)(cp->samples[pos] - beg);
  len = XEN_TO_C_INT_OR_ELSE(samps, maxlen);
  if (len > maxlen) len = maxlen;
  if (len > 0)
    {
      int chn = 0;
      sound_data *sd;
      chn = XEN_TO_C_INT_OR_ELSE(sdchan, 0);
      if (chn < 0)
	XEN_OUT_OF_RANGE_ERROR(S_samples_to_sound_data, 7, sdchan, "sound-data channel ~A < 0?");
      if (sound_data_p(sdobj))
	sd = (sound_data *)XEN_OBJECT_REF(sdobj);
      else
	{
	  newsd = make_sound_data(chn + 1, len);
	  loc = snd_protect(newsd);
	  sd = (sound_data *)XEN_OBJECT_REF(newsd);
	  if ((sd->data == NULL) || (sd->data[chn] == NULL))
	    mus_misc_error(S_samples_to_sound_data, "sound_data memory allocation trouble", newsd);
	}
      if (chn < sd->chans)
	{
	  snd_fd *sf;
	  if (len > sd->length) len = sd->length;
	  sf = init_sample_read_any(beg, cp, READ_FORWARD, pos);
	  if (sf)
	    {
	      for (i = 0; i < len; i++) 
		sd->data[chn][i] = read_sample(sf);
	      free_snd_fd(sf);
	    }
	}
      else 
	{
	  if (loc != -1) snd_unprotect_at(loc);
	  XEN_OUT_OF_RANGE_ERROR(S_samples_to_sound_data, 7, sdchan, "sound-data channel ~A > available chans");
	}
    }
  if (loc != -1) snd_unprotect_at(loc);
  if (XEN_NOT_FALSE_P(newsd))
    return(newsd);
  return(sdobj);
}

Float string_to_Float(char *str) 
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

int string_to_int(char *str) 
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

off_t string_to_off_t(char *str) 
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

static XEN g_color_dialog(XEN managed) 
{
  widget_t w;
  #define H_color_dialog "(" S_color_dialog "): start the Color dialog"
  XEN_ASSERT_TYPE(XEN_BOOLEAN_IF_BOUND_P(managed), managed, XEN_ONLY_ARG, S_color_dialog, "a boolean");
  w = start_color_dialog(XEN_TO_C_BOOLEAN(managed));
  return(XEN_WRAP_WIDGET(w));
}

static XEN g_orientation_dialog(XEN managed) 
{
  widget_t w;
  #define H_orientation_dialog "(" S_orientation_dialog " (managed #t)): start the Orientation dialog"
  XEN_ASSERT_TYPE(XEN_BOOLEAN_IF_BOUND_P(managed), managed, XEN_ONLY_ARG, S_orientation_dialog, "a boolean");
  w = start_orientation_dialog(XEN_TO_C_BOOLEAN(managed));
  return(XEN_WRAP_WIDGET(w));
}

static XEN g_transform_dialog(XEN managed) 
{
  widget_t w;
  #define H_transform_dialog "(" S_transform_dialog " (managed #t)): start the Transforms dialog"
  XEN_ASSERT_TYPE(XEN_BOOLEAN_IF_BOUND_P(managed), managed, XEN_ONLY_ARG, S_transform_dialog, "a boolean");
  w = fire_up_transform_dialog(XEN_TO_C_BOOLEAN(managed));
  return(XEN_WRAP_WIDGET(w));
}

static XEN g_view_files_dialog(XEN managed)
{
  widget_t w;
  #define H_view_files_dialog "(" S_view_files_dialog "): start the View Files dialog"
  XEN_ASSERT_TYPE(XEN_BOOLEAN_IF_BOUND_P(managed), managed, XEN_ONLY_ARG, S_view_files_dialog, "a boolean");
  w = start_file_dialog(XEN_TO_C_BOOLEAN(managed));
  return(XEN_WRAP_WIDGET(w));
}

static XEN g_edit_header_dialog(XEN snd_n) 
{
  widget_t w;
  #define H_edit_header_dialog "(" S_edit_header_dialog " snd): start the Edit Header dialog on sound snd"
  snd_info *sp; 
  sp = get_sp(snd_n, NO_PLAYERS);
  if ((sp == NULL) || (sp->inuse != SOUND_NORMAL))
    return(snd_no_such_sound_error(S_edit_header_dialog, snd_n));
  w = edit_header(sp);
  return(XEN_WRAP_WIDGET(w));
}

static XEN g_save_selection_dialog(XEN managed)
{
  widget_t w;
  #define H_save_selection_dialog "(" S_save_selection_dialog "): start the Selection Save-as dialog"
  XEN_ASSERT_TYPE(XEN_BOOLEAN_IF_BOUND_P(managed), managed, XEN_ONLY_ARG, S_save_selection_dialog, "a boolean");
  w = make_edit_save_as_dialog(XEN_TO_C_BOOLEAN(managed));
  return(XEN_WRAP_WIDGET(w));
}

static XEN g_save_sound_dialog(XEN managed)
{
  widget_t w;
  #define H_save_sound_dialog "(" S_save_sound_dialog "): start the File Save-as dialog"
  XEN_ASSERT_TYPE(XEN_BOOLEAN_IF_BOUND_P(managed), managed, XEN_ONLY_ARG, S_save_sound_dialog, "a boolean");
  w = make_file_save_as_dialog(XEN_TO_C_BOOLEAN(managed));
  return(XEN_WRAP_WIDGET(w));
}

static XEN g_print_dialog(XEN managed) 
{
  widget_t w;
  #define H_print_dialog "(" S_print_dialog " (managed #t)): start the File Print dialog"
  XEN_ASSERT_TYPE(XEN_BOOLEAN_IF_BOUND_P(managed), managed, XEN_ONLY_ARG, S_print_dialog, "a boolean");
  w = make_file_print_dialog(XEN_TO_C_BOOLEAN(managed));
  return(XEN_WRAP_WIDGET(w));
}

static XEN g_yes_or_no_p(XEN msg) 
{
  #define H_yes_or_no_p "(" S_yes_or_no_p " message): display message and wait for 'y' or 'n'; return #t if 'y'"
  XEN_ASSERT_TYPE(XEN_STRING_P(msg), msg, XEN_ONLY_ARG, S_yes_or_no_p, "a string");
  return(C_TO_XEN_BOOLEAN(snd_yes_or_no_p(XEN_TO_C_STRING(msg))));
}

static XEN g_info_dialog(XEN subject, XEN msg)
{
  widget_t w;
  #define H_info_dialog "(" S_info_dialog " subject message): start the Info window with subject and message"
  XEN_ASSERT_TYPE(XEN_STRING_P(subject), subject, XEN_ARG_1, S_info_dialog, "a string");
  XEN_ASSERT_TYPE(XEN_STRING_P(msg), msg, XEN_ARG_2, S_info_dialog, "a string");
  w = post_it(XEN_TO_C_STRING(subject), XEN_TO_C_STRING(msg));
  return(XEN_WRAP_WIDGET(w));
}


#if (!USE_NO_GUI)
/* -------- shared color funcs -------- */

static XEN g_color_p(XEN obj) 
{
  #define H_color_p "(" S_color_p " obj): #t if obj is a color"
  return(C_TO_XEN_BOOLEAN(XEN_PIXEL_P(obj)));
}

Float check_color_range(const char *caller, XEN val)
{
  Float rf;
  rf = XEN_TO_C_DOUBLE(val);
  if ((rf > 1.0) || (rf < 0.0))
    XEN_OUT_OF_RANGE_ERROR(caller, 1, val, "value ~A must be between 0.0 and 1.0");
  return(rf);
}

static XEN g_set_cursor_color (XEN color) 
{
  XEN_ASSERT_TYPE(XEN_PIXEL_P(color), color, XEN_ONLY_ARG, S_setB S_cursor_color, "a color"); 
  color_cursor(XEN_UNWRAP_PIXEL(color));
  for_each_chan(update_graph);
  return(color);
}

static XEN g_cursor_color(void) 
{
  #define H_cursor_color "(" S_cursor_color "): cursor color"
  return(XEN_WRAP_PIXEL((ss->sgx)->cursor_color));
}

static XEN g_set_highlight_color (XEN color) 
{
  XEN_ASSERT_TYPE(XEN_PIXEL_P(color), color, XEN_ONLY_ARG, S_setB S_highlight_color, "a color"); 
  (ss->sgx)->highlight_color = XEN_UNWRAP_PIXEL(color);
  return(color);
}

static XEN g_highlight_color(void) 
{
  #define H_highlight_color "(" S_highlight_color "): color of highlighted text or buttons"
  return(XEN_WRAP_PIXEL((ss->sgx)->highlight_color));
}

static XEN g_set_mark_color (XEN color) 
{
  XEN_ASSERT_TYPE(XEN_PIXEL_P(color), color, XEN_ONLY_ARG, S_setB S_mark_color, "a color"); 
  color_marks(XEN_UNWRAP_PIXEL(color));
  for_each_chan(update_graph);
  return(color);
}

static XEN g_mark_color(void) 
{
  #define H_mark_color "(" S_mark_color "): mark color"
  return(XEN_WRAP_PIXEL((ss->sgx)->mark_color));
}

static XEN g_set_zoom_color (XEN color) 
{
  XEN_ASSERT_TYPE(XEN_PIXEL_P(color), color, XEN_ONLY_ARG, S_setB S_zoom_color, "a color"); 
  (ss->sgx)->zoom_color = XEN_UNWRAP_PIXEL(color); 
  color_chan_components(ss->sgx->zoom_color, COLOR_ZOOM);
  return(color);
}

static XEN g_zoom_color(void) 
{
  #define H_zoom_color "(" S_zoom_color "): color of zoom sliders"
  return(XEN_WRAP_PIXEL((ss->sgx)->zoom_color));
}

static XEN g_set_position_color (XEN color) 
{
  XEN_ASSERT_TYPE(XEN_PIXEL_P(color), color, XEN_ONLY_ARG, S_setB S_position_color, "a color"); 
  (ss->sgx)->position_color = XEN_UNWRAP_PIXEL(color); 
  color_chan_components(ss->sgx->position_color, COLOR_POSITION);
  return(color);
}

static XEN g_position_color(void) 
{
  #define H_position_color "(" S_position_color "): color of position sliders"
  return(XEN_WRAP_PIXEL((ss->sgx)->position_color));
}

static XEN g_set_listener_color (XEN color) 
{
  XEN_ASSERT_TYPE(XEN_PIXEL_P(color), color, XEN_ONLY_ARG, S_setB S_listener_color, "a color"); 
  color_listener(XEN_UNWRAP_PIXEL(color));
  return(color);
}

static XEN g_listener_color(void) 
{
  #define H_listener_color "(" S_listener_color "): background color of the lisp listener"
  return(XEN_WRAP_PIXEL((ss->sgx)->listener_color));
}

static XEN g_set_listener_text_color (XEN color) 
{
  XEN_ASSERT_TYPE(XEN_PIXEL_P(color), color, XEN_ONLY_ARG, S_setB S_listener_text_color, "a color"); 
  color_listener_text(XEN_UNWRAP_PIXEL(color));
  return(color);
}

static XEN g_listener_text_color(void) 
{
  #define H_listener_text_color "(" S_listener_text_color "): text color in the lisp listener"
  return(XEN_WRAP_PIXEL((ss->sgx)->listener_text_color));
}

static XEN g_set_enved_waveform_color (XEN color) 
{
  XEN_ASSERT_TYPE(XEN_PIXEL_P(color), color, XEN_ONLY_ARG, S_setB S_enved_waveform_color, "a color"); 
  color_enved_waveform(XEN_UNWRAP_PIXEL(color));
  return(color);
}

static XEN g_enved_waveform_color(void) 
{
  #define H_enved_waveform_color "(" S_enved_waveform_color "): color of the envelope editor wave display"
  return(XEN_WRAP_PIXEL((ss->sgx)->enved_waveform_color));
}

static XEN g_set_filter_control_waveform_color (XEN color) 
{
  XEN_ASSERT_TYPE(XEN_PIXEL_P(color), color, XEN_ONLY_ARG, S_setB S_filter_control_waveform_color, "a color");
  color_filter_waveform(XEN_UNWRAP_PIXEL(color));
  return(color);
}

static XEN g_filter_control_waveform_color(void) 
{
  #define H_filter_control_waveform_color "(" S_filter_control_waveform_color "): color of the filter waveform"
  return(XEN_WRAP_PIXEL((ss->sgx)->filter_control_waveform_color));
}

static XEN g_set_selection_color (XEN color) 
{
  XEN_ASSERT_TYPE(XEN_PIXEL_P(color), color, XEN_ONLY_ARG, S_setB S_selection_color, "a color"); 
  color_selection(XEN_UNWRAP_PIXEL(color));
  for_each_chan(update_graph);
  return(color);
}

static XEN g_selection_color(void) 
{
  #define H_selection_color "(" S_selection_color "): selection color"
  return(XEN_WRAP_PIXEL((ss->sgx)->selection_color));
}

static XEN g_set_text_focus_color (XEN color) 
{
  XEN_ASSERT_TYPE(XEN_PIXEL_P(color), color, XEN_ONLY_ARG, S_setB S_text_focus_color, "a color"); 
  (ss->sgx)->text_focus_color = XEN_UNWRAP_PIXEL(color);
  return(color);
}

static XEN g_text_focus_color(void) 
{
  #define H_text_focus_color "(" S_text_focus_color "): color used to show a text field has focus"
  return(XEN_WRAP_PIXEL((ss->sgx)->text_focus_color));
}

static XEN g_set_sash_color(XEN color) 
{
  XEN_ASSERT_TYPE(XEN_PIXEL_P(color), color, XEN_ONLY_ARG, S_setB S_sash_color, "a color"); 
  (ss->sgx)->sash_color = XEN_UNWRAP_PIXEL(color);
  return(color);
}

static XEN g_sash_color(void) 
{
  #define H_sash_color "(" S_sash_color "): color used to draw paned window sashes"
  return(XEN_WRAP_PIXEL((ss->sgx)->sash_color));
}

static XEN g_set_data_color(XEN color) 
{
  XEN_ASSERT_TYPE(XEN_PIXEL_P(color), color, XEN_ONLY_ARG, S_setB S_data_color, "a color"); 
  color_data(XEN_UNWRAP_PIXEL(color));
  ss->sgx->grid_color = get_in_between_color(ss->sgx->data_color, ss->sgx->graph_color);
  for_each_chan(update_graph);
  return(color);
}

static XEN g_set_help_button_color(XEN color) 
{
  XEN_ASSERT_TYPE(XEN_PIXEL_P(color), color, XEN_ONLY_ARG, S_setB S_help_button_color, "a color"); 
  (ss->sgx)->help_button_color = XEN_UNWRAP_PIXEL(color);
  return(color);
}

static XEN g_help_button_color(void) 
{
  #define H_help_button_color "(" S_help_button_color "): color used to draw help buttons"
  return(XEN_WRAP_PIXEL((ss->sgx)->help_button_color));
}

static XEN g_set_quit_button_color(XEN color) 
{
  XEN_ASSERT_TYPE(XEN_PIXEL_P(color), color, XEN_ONLY_ARG, S_setB S_quit_button_color, "a color"); 
  (ss->sgx)->quit_button_color = XEN_UNWRAP_PIXEL(color);
  return(color);
}

static XEN g_quit_button_color(void) 
{
  #define H_quit_button_color "(" S_quit_button_color "): color used to draw quit (dismiss, cancel) buttons"
  return(XEN_WRAP_PIXEL((ss->sgx)->quit_button_color));
}

static XEN g_set_doit_button_color(XEN color) 
{
  XEN_ASSERT_TYPE(XEN_PIXEL_P(color), color, XEN_ONLY_ARG, S_setB S_doit_button_color, "a color"); 
  (ss->sgx)->doit_button_color = XEN_UNWRAP_PIXEL(color);
  return(color);
}

static XEN g_doit_button_color(void) 
{
  #define H_doit_button_color "(" S_doit_button_color "): color used to draw doit (Ok, Apply) buttons"
  return(XEN_WRAP_PIXEL((ss->sgx)->doit_button_color));
}

static XEN g_set_doit_again_button_color(XEN color) 
{
  XEN_ASSERT_TYPE(XEN_PIXEL_P(color), color, XEN_ONLY_ARG, S_setB S_doit_again_button_color, "a color"); 
  (ss->sgx)->doit_again_button_color = XEN_UNWRAP_PIXEL(color);
  return(color);
}

static XEN g_doit_again_button_color(void) 
{
  #define H_doit_again_button_color "(" S_doit_again_button_color "): color used to doit again (Undo&Apply) buttons"
  return(XEN_WRAP_PIXEL((ss->sgx)->doit_again_button_color));
}

static XEN g_set_reset_button_color(XEN color) 
{
  XEN_ASSERT_TYPE(XEN_PIXEL_P(color), color, XEN_ONLY_ARG, S_setB S_reset_button_color, "a color"); 
  (ss->sgx)->reset_button_color = XEN_UNWRAP_PIXEL(color);
  return(color);
}

static XEN g_reset_button_color(void) 
{
  #define H_reset_button_color "(" S_reset_button_color "): color used to draw reset buttons"
  return(XEN_WRAP_PIXEL((ss->sgx)->reset_button_color));
}


static XEN g_data_color(void) 
{
  #define H_data_color "(" S_data_color "): color used to draw unselected data"
  return(XEN_WRAP_PIXEL((ss->sgx)->data_color));
}

static XEN g_set_selected_data_color(XEN color)
{
  chan_info *cp;
  XEN_ASSERT_TYPE(XEN_PIXEL_P(color), color, XEN_ONLY_ARG, S_setB S_selected_data_color, "a color"); 
  color_selected_data(XEN_UNWRAP_PIXEL(color));
  ss->sgx->selected_grid_color = get_in_between_color(ss->sgx->selected_data_color, ss->sgx->selected_graph_color);
  cp = selected_channel();
  if (cp) update_graph(cp);
  return(color);
}

static XEN g_selected_data_color(void) 
{
  #define H_selected_data_color "(" S_selected_data_color "): color used for selected data"
  return(XEN_WRAP_PIXEL((ss->sgx)->selected_data_color));
}

static XEN g_set_graph_color(XEN color) 
{
  XEN_ASSERT_TYPE(XEN_PIXEL_P(color), color, XEN_ONLY_ARG, S_setB S_graph_color, "a color");
  color_graph(XEN_UNWRAP_PIXEL(color));
  color_unselected_graphs(XEN_UNWRAP_PIXEL(color));
  ss->sgx->grid_color = get_in_between_color(ss->sgx->data_color, ss->sgx->graph_color);
  return(color);
}

static XEN g_graph_color(void) 
{
  #define H_graph_color "(" S_graph_color "): background color used for unselected data"
  return(XEN_WRAP_PIXEL((ss->sgx)->graph_color));
}

static XEN g_set_selected_graph_color(XEN color) 
{
  chan_info *cp;
  XEN_ASSERT_TYPE(XEN_PIXEL_P(color), color, XEN_ONLY_ARG, S_setB S_selected_graph_color, "a color");
  color_selected_graph(XEN_UNWRAP_PIXEL(color));
  ss->sgx->selected_grid_color = get_in_between_color(ss->sgx->selected_data_color, ss->sgx->selected_graph_color);
  cp = selected_channel();
  if (cp) 
    {
#if USE_MOTIF
      XtVaSetValues(channel_graph(cp), XmNbackground, ss->sgx->selected_graph_color, NULL);
#else
      gtk_widget_modify_bg(channel_graph(cp), GTK_STATE_NORMAL, ss->sgx->selected_graph_color);
#endif
    }
  return(color);
}

static XEN g_selected_graph_color(void) 
{
  #define H_selected_graph_color "(" S_selected_graph_color "): background color of selected data"
  return(XEN_WRAP_PIXEL((ss->sgx)->selected_graph_color));
}

static XEN g_set_pushed_button_color(XEN color) 
{
  XEN_ASSERT_TYPE(XEN_PIXEL_P(color), color, XEN_ONLY_ARG, S_setB S_pushed_button_color, "a color"); 
  (ss->sgx)->pushed_button_color = XEN_UNWRAP_PIXEL(color);
#if USE_MOTIF
  map_over_children(MAIN_SHELL(ss), recolor_button, NULL);
#endif
  return(color);
}

static XEN g_pushed_button_color(void) 
{
  #define H_pushed_button_color "(" S_pushed_button_color "): color of a pushed button"
  return(XEN_WRAP_PIXEL((ss->sgx)->pushed_button_color));
}

static XEN g_basic_color(void) 
{
  #define H_basic_color "(" S_basic_color "): Snd's basic color"
  return(XEN_WRAP_PIXEL((ss->sgx)->basic_color));
}

static void recolor_everything(widget_t w, void *ptr)
{
  /* yow! these are treating ptr differently */
#if USE_GTK
  if (GTK_IS_WIDGET(w)) 
    {
      gtk_widget_modify_bg(w, GTK_STATE_NORMAL, (GdkColor *)ptr);
      if (GTK_IS_CONTAINER(w))
	gtk_container_foreach(GTK_CONTAINER(w), recolor_everything, (gpointer)ptr);
    }
#endif
#if USE_MOTIF
  Pixel curcol;
  if (XtIsWidget(w))
    {
      XtVaGetValues(w, XmNbackground, &curcol, NULL);
      if (curcol == (Pixel)ptr)
	XmChangeColor(w, (ss->sgx)->basic_color);
    }
#endif
}


static XEN g_set_basic_color(XEN color) 
{
  color_t old_color;
  XEN_ASSERT_TYPE(XEN_PIXEL_P(color), color, XEN_ONLY_ARG, S_setB S_basic_color, "a color"); 
  old_color = (ss->sgx)->basic_color;
  (ss->sgx)->basic_color = XEN_UNWRAP_PIXEL(color); 
#if USE_MOTIF
  map_over_children(MAIN_SHELL(ss), recolor_everything, (void *)old_color);
#endif
#if USE_GTK
  gtk_container_foreach(GTK_CONTAINER(MAIN_SHELL(ss)), recolor_everything, (gpointer)(ss->sgx->basic_color));
#endif

#if HAVE_XPM && USE_MOTIF
  make_sound_icons_transparent_again(old_color, ss->sgx->basic_color);
  make_recorder_icons_transparent_again(old_color, ss->sgx->basic_color);
  make_mixer_icons_transparent_again(old_color, ss->sgx->basic_color);
#endif

  return(color);
}
#endif


static XEN during_open_hook;
static XEN after_open_hook;

XEN run_progn_hook(XEN hook, XEN args, const char *caller)
{
  /* Guile built-in scm_c_run_hook doesn't return the value of the hook procedure(s) and exits on error */
  XEN result = XEN_FALSE;
  XEN procs = XEN_HOOK_PROCEDURES(hook);
  while (XEN_NOT_NULL_P(procs))
    {
      result = XEN_APPLY(XEN_CAR(procs), args, caller);
      procs = XEN_CDR(procs);
    }
  return(xen_return_first(result, args));
}

XEN run_hook(XEN hook, XEN args, const char *caller)
{
  XEN procs = XEN_HOOK_PROCEDURES(hook);
  while (XEN_NOT_NULL_P(procs))
    {
      if (!(XEN_EQ_P(args, XEN_EMPTY_LIST)))
	XEN_APPLY(XEN_CAR(procs), args, caller);
      else XEN_CALL_0(XEN_CAR(procs), caller);
      procs = XEN_CDR (procs);
    }
  return(xen_return_first(XEN_FALSE, args));
}

XEN run_or_hook (XEN hook, XEN args, const char *caller)
{
  XEN result = XEN_FALSE; /* (or): #f */
  XEN hook_result = XEN_FALSE;
  XEN procs = XEN_HOOK_PROCEDURES (hook);
  while (XEN_NOT_NULL_P (procs))
    {
      if (!(XEN_EQ_P(args, XEN_EMPTY_LIST)))
	result = XEN_APPLY(XEN_CAR(procs), args, caller);
      else result = XEN_CALL_0(XEN_CAR(procs), caller);
      if (XEN_NOT_FALSE_P(result)) hook_result = result; /* return last non-#f result */
      procs = XEN_CDR (procs);
    }
  return(xen_return_first(hook_result, args));
}

void during_open(int fd, char *file, open_reason_t reason)
{
  if (XEN_HOOKED(during_open_hook))
    run_hook(during_open_hook,
	     XEN_LIST_3(C_TO_XEN_INT(fd),
			C_TO_XEN_STRING(file),
			C_TO_XEN_INT((int)reason)),
	     S_during_open_hook);
}

void after_open(int index)
{
  if (XEN_HOOKED(after_open_hook))
    run_hook(after_open_hook,
	     XEN_LIST_1(C_TO_XEN_INT(index)),
	     S_after_open_hook);
}

/* this needs to be in Snd (rather than sndlib2xen.c) because it calls post_it */
static XEN g_mus_audio_describe(void) 
{
  #define H_mus_audio_describe "("  S_mus_audio_describe "): post a description of the audio hardware state in the Help dialog"
  post_it("Audio State", mus_audio_report()); 
  return(XEN_TRUE);
}

static XEN g_just_sounds(void)
{
  #define H_just_sounds "(" S_just_sounds "): the 'just sounds' choice in the file chooser dialog"
  return(C_TO_XEN_BOOLEAN(ss->just_sounds_state));
}

static XEN g_set_just_sounds(XEN on) 
{
  XEN_ASSERT_TYPE(XEN_BOOLEAN_P(on), on, XEN_ARG_1, S_setB S_just_sounds, "a boolean");
  ss->just_sounds_state = XEN_TO_C_BOOLEAN(on);
  reflect_just_sounds_state();
  return(C_TO_XEN_BOOLEAN(ss->just_sounds_state));
}


#if HAVE_GUILE && HAVE_DLFCN_H
#include <dlfcn.h>
/* these are included because libtool's dlopen is incredibly stupid */

static XEN g_dlopen(XEN name)
{
  void *handle;
  char *cname;
  XEN_ASSERT_TYPE(XEN_STRING_P(name), name, XEN_ONLY_ARG, "dlopen", "a string (filename)");
  cname = XEN_TO_C_STRING(name);
  if (cname)
    {
      handle = dlopen(cname, RTLD_LAZY);
      if (handle == NULL)
	{
	  char *longname;
	  longname = mus_expand_filename(cname);
	  handle = dlopen(longname, RTLD_LAZY);
	  FREE(longname);
	  if (handle == NULL)
	    {
	      char *err;
	      err = dlerror();
	      if ((err) && (*err))
		return(C_TO_XEN_STRING(err));
	      return(XEN_FALSE);
	    }
	}
      return(XEN_WRAP_C_POINTER(handle));
    }
  return(XEN_FALSE);
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
  char *str, *temp;
  XEN res;
  temp = copy_string(XEN_TO_C_STRING(text));
  str = command_completer(temp);
  res = C_TO_XEN_STRING(str);
  FREE(str);
  FREE(temp);
  return(res);
}

static XEN g_snd_global_state(void)
{
  return(XEN_WRAP_C_POINTER(ss));
}

#if DEBUGGING
static XEN g_snd_sound_pointer(XEN snd)
{
  /* (XtCallCallbacks (cadr (sound-widgets 0)) XmNactivateCallback (snd-sound-pointer 0)) */
  int s;
  s = XEN_TO_C_INT(snd);
  if ((s < ss->max_sounds) && (s >= 0) && (ss->sounds[s]))
    return(C_TO_XEN_ULONG((unsigned long)(ss->sounds[s])));
  return(XEN_FALSE);
}

#if HAVE_GUILE
static XEN g_snd_stdin_test(XEN str)
{
  /* autotest stdin stuff since I can't figure out how to write stdin directly */
  snd_eval_stdin_str(XEN_TO_C_STRING(str));
  return(XEN_FALSE);
}
#endif
#endif


#define S_gc_off "gc-off"
#define S_gc_on "gc-on"

static XEN g_gc_off(void) 
{
  #define H_gc_off "(" S_gc_off ") turns off garbage collection"
#if HAVE_GUILE
  ++scm_block_gc; 
#else
#if HAVE_RUBY && HAVE_RB_GC_DISABLE
  rb_gc_disable();
#endif
#endif
  return(XEN_FALSE);
}

static XEN g_gc_on(void) 
{
  #define H_gc_on "(" S_gc_on ") turns on garbage collection"
#if HAVE_GUILE
  --scm_block_gc; 
#else
#if HAVE_RUBY && HAVE_RB_GC_DISABLE
  rb_gc_enable();
#endif
#endif
  return(XEN_FALSE);
}


#if (!HAVE_SCM_CONTINUATION_P)
#if HAVE_GUILE
static SCM g_continuation_p(XEN obj)
{
#ifdef SCM_CONTINUATIONP
  return(C_TO_XEN_BOOLEAN(SCM_NIMP(obj) && (SCM_CONTINUATIONP(obj))));
#else
  return(XEN_PROCEDURE_P(obj));
#endif
}
#endif
#endif

static XEN g_fmod(XEN a, XEN b)
{
  XEN_ASSERT_TYPE(XEN_NUMBER_P(a), a, XEN_ARG_1, "fmod", " a number");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(b), b, XEN_ARG_2, "fmod", " a number");
  return(C_TO_XEN_DOUBLE(fmod(XEN_TO_C_DOUBLE(a), XEN_TO_C_DOUBLE(b))));
}


#ifdef XEN_ARGIFY_1
#if HAVE_GUILE && HAVE_DLFCN_H
XEN_NARGIFY_1(g_dlopen_w, g_dlopen)
XEN_NARGIFY_1(g_dlclose_w, g_dlclose)
XEN_NARGIFY_0(g_dlerror_w, g_dlerror)
XEN_NARGIFY_2(g_dlinit_w, g_dlinit)
#endif
XEN_NARGIFY_0(g_just_sounds_w, g_just_sounds)
XEN_NARGIFY_1(g_set_just_sounds_w, g_set_just_sounds)
XEN_NARGIFY_0(g_region_graph_style_w, g_region_graph_style)
XEN_NARGIFY_1(g_set_region_graph_style_w, g_set_region_graph_style)
XEN_NARGIFY_0(g_ask_before_overwrite_w, g_ask_before_overwrite)
XEN_NARGIFY_1(g_set_ask_before_overwrite_w, g_set_ask_before_overwrite)
XEN_NARGIFY_0(g_audio_output_device_w, g_audio_output_device)
XEN_NARGIFY_1(g_set_audio_output_device_w, g_set_audio_output_device)
XEN_NARGIFY_0(g_audio_input_device_w, g_audio_input_device)
XEN_NARGIFY_1(g_set_audio_input_device_w, g_set_audio_input_device)
XEN_NARGIFY_0(g_minibuffer_history_length_w, g_minibuffer_history_length)
XEN_NARGIFY_1(g_set_minibuffer_history_length_w, g_set_minibuffer_history_length)
XEN_NARGIFY_0(g_emacs_style_save_as_w, g_emacs_style_save_as)
XEN_NARGIFY_1(g_set_emacs_style_save_as_w, g_set_emacs_style_save_as)
XEN_NARGIFY_0(g_auto_resize_w, g_auto_resize)
XEN_NARGIFY_1(g_set_auto_resize_w, g_set_auto_resize)
XEN_NARGIFY_0(g_auto_update_w, g_auto_update)
XEN_NARGIFY_1(g_set_auto_update_w, g_set_auto_update)
XEN_NARGIFY_0(g_color_cutoff_w, g_color_cutoff)
XEN_NARGIFY_1(g_set_color_cutoff_w, g_set_color_cutoff)
XEN_NARGIFY_0(g_color_inverted_w, g_color_inverted)
XEN_NARGIFY_1(g_set_color_inverted_w, g_set_color_inverted)
XEN_NARGIFY_0(g_color_scale_w, g_color_scale)
XEN_NARGIFY_1(g_set_color_scale_w, g_set_color_scale)
XEN_NARGIFY_0(g_auto_update_interval_w, g_auto_update_interval)
XEN_NARGIFY_1(g_set_auto_update_interval_w, g_set_auto_update_interval)
XEN_NARGIFY_0(g_default_output_chans_w, g_default_output_chans)
XEN_NARGIFY_1(g_set_default_output_chans_w, g_set_default_output_chans)
XEN_NARGIFY_0(g_default_output_srate_w, g_default_output_srate)
XEN_NARGIFY_1(g_set_default_output_srate_w, g_set_default_output_srate)
XEN_NARGIFY_0(g_default_output_type_w, g_default_output_type)
XEN_NARGIFY_1(g_set_default_output_type_w, g_set_default_output_type)
XEN_NARGIFY_0(g_default_output_format_w, g_default_output_format)
XEN_NARGIFY_1(g_set_default_output_format_w, g_set_default_output_format)
XEN_NARGIFY_0(g_selection_creates_region_w, g_selection_creates_region)
XEN_NARGIFY_1(g_set_selection_creates_region_w, g_set_selection_creates_region)
XEN_NARGIFY_0(g_print_length_w, g_print_length)
XEN_NARGIFY_1(g_set_print_length_w, g_set_print_length)
XEN_NARGIFY_0(g_show_indices_w, g_show_indices)
XEN_NARGIFY_1(g_set_show_indices_w, g_set_show_indices)
XEN_NARGIFY_0(g_show_backtrace_w, g_show_backtrace)
XEN_NARGIFY_1(g_set_show_backtrace_w, g_set_show_backtrace)
XEN_NARGIFY_0(g_temp_dir_w, g_temp_dir)
XEN_NARGIFY_1(g_set_temp_dir_w, g_set_temp_dir)
XEN_NARGIFY_0(g_save_dir_w, g_save_dir)
XEN_NARGIFY_1(g_set_save_dir_w, g_set_save_dir)
XEN_NARGIFY_0(g_ladspa_dir_w, g_ladspa_dir)
XEN_NARGIFY_1(g_set_ladspa_dir_w, g_set_ladspa_dir)
XEN_NARGIFY_0(g_trap_segfault_w, g_trap_segfault)
XEN_NARGIFY_1(g_set_trap_segfault_w, g_set_trap_segfault)
XEN_NARGIFY_0(g_log_freq_start_w, g_log_freq_start)
XEN_NARGIFY_1(g_set_log_freq_start_w, g_set_log_freq_start)
XEN_NARGIFY_0(g_show_selection_transform_w, g_show_selection_transform)
XEN_NARGIFY_1(g_set_show_selection_transform_w, g_set_show_selection_transform)
XEN_NARGIFY_0(g_with_gl_w, g_with_gl)
XEN_NARGIFY_1(g_set_with_gl_w, g_set_with_gl)
XEN_NARGIFY_0(g_with_relative_panes_w, g_with_relative_panes)
XEN_NARGIFY_1(g_set_with_relative_panes_w, g_set_with_relative_panes)
XEN_NARGIFY_0(g_with_background_processes_w, g_with_background_processes)
XEN_NARGIFY_1(g_set_with_background_processes_w, g_set_with_background_processes)
XEN_NARGIFY_0(g_data_clipped_w, g_data_clipped)
XEN_NARGIFY_1(g_set_data_clipped_w, g_set_data_clipped)
XEN_NARGIFY_0(g_window_x_w, g_window_x)
XEN_NARGIFY_1(g_set_window_x_w, g_set_window_x)
XEN_NARGIFY_0(g_window_y_w, g_window_y)
XEN_NARGIFY_1(g_set_window_y_w, g_set_window_y)
XEN_NARGIFY_0(g_zoom_focus_style_w, g_zoom_focus_style)
XEN_NARGIFY_1(g_set_zoom_focus_style_w, g_set_zoom_focus_style)
XEN_NARGIFY_0(g_tiny_font_w, g_tiny_font)
XEN_NARGIFY_1(g_set_tiny_font_w, g_set_tiny_font)
XEN_NARGIFY_0(g_peaks_font_w, g_peaks_font)
XEN_NARGIFY_1(g_set_peaks_font_w, g_set_peaks_font)
XEN_NARGIFY_0(g_bold_peaks_font_w, g_bold_peaks_font)
XEN_NARGIFY_1(g_set_bold_peaks_font_w, g_set_bold_peaks_font)
XEN_NARGIFY_0(g_axis_label_font_w, g_axis_label_font)
XEN_NARGIFY_1(g_set_axis_label_font_w, g_set_axis_label_font)
XEN_NARGIFY_0(g_axis_numbers_font_w, g_axis_numbers_font)
XEN_NARGIFY_1(g_set_axis_numbers_font_w, g_set_axis_numbers_font)
XEN_NARGIFY_0(g_listener_font_w, g_listener_font)
XEN_NARGIFY_1(g_set_listener_font_w, g_set_listener_font)
XEN_NARGIFY_0(g_window_width_w, g_window_width)
XEN_NARGIFY_1(g_set_window_width_w, g_set_window_width)
XEN_NARGIFY_0(g_window_height_w, g_window_height)
XEN_NARGIFY_1(g_set_window_height_w, g_set_window_height)
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
XEN_NARGIFY_0(g_filter_control_waveform_color_w, g_filter_control_waveform_color)
XEN_NARGIFY_1(g_set_filter_control_waveform_color_w, g_set_filter_control_waveform_color)
XEN_NARGIFY_0(g_highlight_color_w, g_highlight_color)
XEN_NARGIFY_1(g_set_highlight_color_w, g_set_highlight_color)
XEN_NARGIFY_0(g_cursor_color_w, g_cursor_color)
XEN_NARGIFY_1(g_set_cursor_color_w, g_set_cursor_color)
XEN_NARGIFY_0(g_text_focus_color_w, g_text_focus_color)
XEN_NARGIFY_1(g_set_text_focus_color_w, g_set_text_focus_color)
XEN_NARGIFY_0(g_sash_color_w, g_sash_color)
XEN_NARGIFY_1(g_set_sash_color_w, g_set_sash_color)
XEN_NARGIFY_0(g_help_button_color_w, g_help_button_color)
XEN_NARGIFY_1(g_set_help_button_color_w, g_set_help_button_color)
XEN_NARGIFY_0(g_reset_button_color_w, g_reset_button_color)
XEN_NARGIFY_1(g_set_reset_button_color_w, g_set_reset_button_color)
XEN_NARGIFY_0(g_quit_button_color_w, g_quit_button_color)
XEN_NARGIFY_1(g_set_quit_button_color_w, g_set_quit_button_color)
XEN_NARGIFY_0(g_doit_button_color_w, g_doit_button_color)
XEN_NARGIFY_1(g_set_doit_button_color_w, g_set_doit_button_color)
XEN_NARGIFY_0(g_doit_again_button_color_w, g_doit_again_button_color)
XEN_NARGIFY_1(g_set_doit_again_button_color_w, g_set_doit_again_button_color)
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
XEN_ARGIFY_1(g_color_dialog_w, g_color_dialog)
XEN_ARGIFY_1(g_orientation_dialog_w, g_orientation_dialog)
XEN_ARGIFY_1(g_transform_dialog_w, g_transform_dialog)
XEN_ARGIFY_1(g_view_files_dialog_w, g_view_files_dialog)
XEN_ARGIFY_1(g_edit_header_dialog_w, g_edit_header_dialog)
XEN_ARGIFY_1(g_save_selection_dialog_w, g_save_selection_dialog)
XEN_ARGIFY_1(g_save_sound_dialog_w, g_save_sound_dialog)
XEN_ARGIFY_1(g_print_dialog_w, g_print_dialog)
XEN_NARGIFY_2(g_info_dialog_w, g_info_dialog)
XEN_NARGIFY_0(g_sounds_w, g_sounds)
XEN_NARGIFY_1(g_yes_or_no_p_w, g_yes_or_no_p)
XEN_NARGIFY_0(g_abort_w, g_abort)
XEN_NARGIFY_0(g_abortq_w, g_abortq)
XEN_NARGIFY_0(g_snd_version_w, g_snd_version)
XEN_ARGIFY_7(g_samples_to_sound_data_w, g_samples_to_sound_data)
XEN_NARGIFY_1(g_snd_print_w, g_snd_print)
XEN_NARGIFY_0(g_little_endian_w, g_little_endian)
XEN_NARGIFY_1(g_snd_completion_w, g_snd_completion)
XEN_NARGIFY_0(g_snd_global_state_w, g_snd_global_state)
XEN_NARGIFY_0(g_mus_audio_describe_w, g_mus_audio_describe)
#if DEBUGGING
  XEN_NARGIFY_1(g_snd_sound_pointer_w, g_snd_sound_pointer)
#endif
XEN_NARGIFY_2(g_fmod_w, g_fmod)
XEN_NARGIFY_0(g_gc_off_w, g_gc_off)
XEN_NARGIFY_0(g_gc_on_w, g_gc_on)

#else
#if HAVE_GUILE && HAVE_DLFCN_H
#define g_dlopen_w g_dlopen
#define g_dlclose_w g_dlclose
#define g_dlerror_w g_dlerror
#define g_dlinit_w g_dlinit
#endif
#define g_just_sounds_w g_just_sounds
#define g_set_just_sounds_w g_set_just_sounds
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
#define g_selection_creates_region_w g_selection_creates_region
#define g_set_selection_creates_region_w g_set_selection_creates_region
#define g_print_length_w g_print_length
#define g_set_print_length_w g_set_print_length
#define g_show_indices_w g_show_indices
#define g_set_show_indices_w g_set_show_indices
#define g_show_backtrace_w g_show_backtrace
#define g_set_show_backtrace_w g_set_show_backtrace
#define g_temp_dir_w g_temp_dir
#define g_set_temp_dir_w g_set_temp_dir
#define g_save_dir_w g_save_dir
#define g_set_save_dir_w g_set_save_dir
#define g_ladspa_dir_w g_ladspa_dir
#define g_set_ladspa_dir_w g_set_ladspa_dir
#define g_trap_segfault_w g_trap_segfault
#define g_set_trap_segfault_w g_set_trap_segfault
#define g_log_freq_start_w g_log_freq_start
#define g_set_log_freq_start_w g_set_log_freq_start
#define g_show_selection_transform_w g_show_selection_transform
#define g_set_show_selection_transform_w g_set_show_selection_transform
#define g_with_gl_w g_with_gl
#define g_set_with_gl_w g_set_with_gl
#define g_with_relative_panes_w g_with_relative_panes
#define g_set_with_relative_panes_w g_set_with_relative_panes
#define g_with_background_processes_w g_with_background_processes
#define g_set_with_background_processes_w g_set_with_background_processes
#define g_data_clipped_w g_data_clipped
#define g_set_data_clipped_w g_set_data_clipped
#define g_window_x_w g_window_x
#define g_set_window_x_w g_set_window_x
#define g_window_y_w g_window_y
#define g_set_window_y_w g_set_window_y
#define g_zoom_focus_style_w g_zoom_focus_style
#define g_set_zoom_focus_style_w g_set_zoom_focus_style
#define g_tiny_font_w g_tiny_font
#define g_set_tiny_font_w g_set_tiny_font
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
#define g_filter_control_waveform_color_w g_filter_control_waveform_color
#define g_set_filter_control_waveform_color_w g_set_filter_control_waveform_color
#define g_highlight_color_w g_highlight_color
#define g_set_highlight_color_w g_set_highlight_color
#define g_cursor_color_w g_cursor_color
#define g_set_cursor_color_w g_set_cursor_color
#define g_text_focus_color_w g_text_focus_color
#define g_set_text_focus_color_w g_set_text_focus_color
#define g_sash_color_w g_sash_color
#define g_set_sash_color_w g_set_sash_color
#define g_help_button_color_w g_help_button_color
#define g_set_help_button_color_w g_set_help_button_color
#define g_doit_again_button_color_w g_doit_again_button_color
#define g_set_doit_again_button_color_w g_set_doit_again_button_color
#define g_doit_button_color_w g_doit_button_color
#define g_set_doit_button_color_w g_set_doit_button_color
#define g_quit_button_color_w g_quit_button_color
#define g_set_quit_button_color_w g_set_quit_button_color
#define g_reset_button_color_w g_reset_button_color
#define g_set_reset_button_color_w g_set_reset_button_color
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
#define g_color_dialog_w g_color_dialog
#define g_orientation_dialog_w g_orientation_dialog
#define g_transform_dialog_w g_transform_dialog
#define g_view_files_dialog_w g_view_files_dialog
#define g_edit_header_dialog_w g_edit_header_dialog
#define g_save_selection_dialog_w g_save_selection_dialog
#define g_save_sound_dialog_w g_save_sound_dialog
#define g_print_dialog_w g_print_dialog
#define g_info_dialog_w g_info_dialog
#define g_sounds_w g_sounds
#define g_yes_or_no_p_w g_yes_or_no_p
#define g_abort_w g_abort
#define g_abortq_w g_abortq
#define g_snd_version_w g_snd_version
#define g_samples_to_sound_data_w g_samples_to_sound_data
#define g_snd_print_w g_snd_print
#define g_little_endian_w g_little_endian
#define g_snd_completion_w g_snd_completion
#define g_snd_global_state_w g_snd_global_state
#define g_mus_audio_describe_w g_mus_audio_describe
#if DEBUGGING
  #define g_snd_sound_pointer_w g_snd_sound_pointer
#endif
#define g_fmod_w g_fmod
#define g_gc_off_w g_gc_off
#define g_gc_on_w g_gc_on
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
  XEN_DEFINE_PROCEDURE(S_mus_audio_describe, g_mus_audio_describe_w, 0, 0, 0, H_mus_audio_describe);

  XEN_DEFINE_PROCEDURE("snd-global-state", g_snd_global_state_w, 0, 0, 0, "internal testing function");
#if DEBUGGING
  XEN_DEFINE_PROCEDURE("snd-sound-pointer", g_snd_sound_pointer_w, 1, 0, 0, "internal testing function");
#if HAVE_GUILE
  XEN_DEFINE_PROCEDURE("snd-stdin-test", g_snd_stdin_test, 1, 0, 0, "internal testing function");
#endif
#endif

  XEN_DEFINE_PROCEDURE(S_gc_off, g_gc_off_w, 0, 0, 0, H_gc_off);
  XEN_DEFINE_PROCEDURE(S_gc_on,  g_gc_on_w,  0, 0, 0, H_gc_on);

#if (!HAVE_SCM_CONTINUATION_P)
#if HAVE_GUILE
  XEN_DEFINE_PROCEDURE("continuation?", g_continuation_p, 1, 0, 0, "#t if arg is a continuation");
#endif
#endif

#if HAVE_RUBY
  Init_sndlib();
#else
  init_sndlib();
#endif

#if WITH_MIDI && HAVE_EXTENSION_LANGUAGE
  mus_midi_init();
#endif

  gc_protection = XEN_FALSE;

  #define H_zoom_focus_left "The value for " S_zoom_focus_style " that causes zooming to maintain the left edge steady"
  #define H_zoom_focus_right "The value for " S_zoom_focus_style " that causes zooming to maintain the right edge steady"
  #define H_zoom_focus_middle "The value for " S_zoom_focus_style " that causes zooming to focus on the middle sample"
  #define H_zoom_focus_active "The value for " S_zoom_focus_style " that causes zooming to focus on the currently active object"

  XEN_DEFINE_CONSTANT(S_zoom_focus_left,       ZOOM_FOCUS_LEFT,   H_zoom_focus_left);
  XEN_DEFINE_CONSTANT(S_zoom_focus_right,      ZOOM_FOCUS_RIGHT,  H_zoom_focus_right);
  XEN_DEFINE_CONSTANT(S_zoom_focus_active,     ZOOM_FOCUS_ACTIVE, H_zoom_focus_active);
  XEN_DEFINE_CONSTANT(S_zoom_focus_middle,     ZOOM_FOCUS_MIDDLE, H_zoom_focus_middle);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_region_graph_style, g_region_graph_style_w, H_region_graph_style,
				   S_setB S_region_graph_style, g_set_region_graph_style_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_ask_before_overwrite, g_ask_before_overwrite_w, H_ask_before_overwrite,
				   S_setB S_ask_before_overwrite, g_set_ask_before_overwrite_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_audio_output_device, g_audio_output_device_w, H_audio_output_device,
				   S_setB S_audio_output_device, g_set_audio_output_device_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_audio_input_device, g_audio_input_device_w, H_audio_input_device,
				   S_setB S_audio_input_device, g_set_audio_input_device_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_minibuffer_history_length, g_minibuffer_history_length_w, H_minibuffer_history_length,
				   S_setB S_minibuffer_history_length, g_set_minibuffer_history_length_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_emacs_style_save_as, g_emacs_style_save_as_w, H_emacs_style_save_as,
				   S_setB S_emacs_style_save_as, g_set_emacs_style_save_as_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_auto_resize, g_auto_resize_w, H_auto_resize,
				   S_setB S_auto_resize, g_set_auto_resize_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_auto_update, g_auto_update_w, H_auto_update,
				   S_setB S_auto_update, g_set_auto_update_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_color_cutoff, g_color_cutoff_w, H_color_cutoff,
				   S_setB S_color_cutoff, g_set_color_cutoff_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_color_inverted, g_color_inverted_w, H_color_inverted,
				   S_setB S_color_inverted, g_set_color_inverted_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_color_scale, g_color_scale_w, H_color_scale,
				   S_setB S_color_scale, g_set_color_scale_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_auto_update_interval, g_auto_update_interval_w, H_auto_update_interval,
				   S_setB S_auto_update_interval, g_set_auto_update_interval_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_default_output_chans, g_default_output_chans_w, H_default_output_chans,
				   S_setB S_default_output_chans, g_set_default_output_chans_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_default_output_srate, g_default_output_srate_w, H_default_output_srate,
				   S_setB S_default_output_srate, g_set_default_output_srate_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_default_output_type, g_default_output_type_w, H_default_output_type,
				   S_setB S_default_output_type, g_set_default_output_type_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_default_output_format, g_default_output_format_w, H_default_output_format,
				   S_setB S_default_output_format, g_set_default_output_format_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_selection_creates_region, g_selection_creates_region_w, H_selection_creates_region,
				   S_setB S_selection_creates_region, g_set_selection_creates_region_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_print_length, g_print_length_w, H_print_length,
				   S_setB S_print_length, g_set_print_length_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_show_indices, g_show_indices_w, H_show_indices,
				   S_setB S_show_indices, g_set_show_indices_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_show_backtrace, g_show_backtrace_w, H_show_backtrace,
				   S_setB S_show_backtrace, g_set_show_backtrace_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_temp_dir, g_temp_dir_w, H_temp_dir,
				   S_setB S_temp_dir, g_set_temp_dir_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_save_dir, g_save_dir_w, H_save_dir,
				   S_setB S_save_dir, g_set_save_dir_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_ladspa_dir, g_ladspa_dir_w, H_ladspa_dir,
				   S_setB S_ladspa_dir, g_set_ladspa_dir_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_trap_segfault, g_trap_segfault_w, H_trap_segfault,
				   S_setB S_trap_segfault, g_set_trap_segfault_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_log_freq_start, g_log_freq_start_w, H_log_freq_start,
				   S_setB S_log_freq_start, g_set_log_freq_start_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_show_selection_transform, g_show_selection_transform_w, H_show_selection_transform,
				   S_setB S_show_selection_transform, g_set_show_selection_transform_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_with_gl, g_with_gl_w, H_with_gl,
				   S_setB S_with_gl, g_set_with_gl_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_with_relative_panes, g_with_relative_panes_w, H_with_relative_panes,
				   S_setB S_with_relative_panes, g_set_with_relative_panes_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_with_background_processes, g_with_background_processes_w, H_with_background_processes,
				   S_setB S_with_background_processes, g_set_with_background_processes_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_data_clipped, g_data_clipped_w, H_data_clipped,
				   S_setB S_data_clipped, g_set_data_clipped_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_window_x, g_window_x_w, H_window_x,
				   S_setB S_window_x, g_set_window_x_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_window_y, g_window_y_w, H_window_y,
				   S_setB S_window_y, g_set_window_y_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_zoom_focus_style, g_zoom_focus_style_w, H_zoom_focus_style,
				   S_setB S_zoom_focus_style, g_set_zoom_focus_style_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_tiny_font, g_tiny_font_w, H_tiny_font,
				   S_setB S_tiny_font, g_set_tiny_font_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_peaks_font, g_peaks_font_w, H_peaks_font,
				   S_setB S_peaks_font, g_set_peaks_font_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_bold_peaks_font, g_bold_peaks_font_w, H_bold_peaks_font,
				   S_setB S_bold_peaks_font, g_set_bold_peaks_font_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_axis_label_font, g_axis_label_font_w, H_axis_label_font,
				   S_setB S_axis_label_font, g_set_axis_label_font_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_axis_numbers_font, g_axis_numbers_font_w, H_axis_numbers_font,
				   S_setB S_axis_numbers_font, g_set_axis_numbers_font_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_listener_font, g_listener_font_w, H_listener_font,
				   S_setB S_listener_font, g_set_listener_font_w,  0, 0, 1, 0);
  
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_window_width, g_window_width_w, H_window_width,
				   S_setB S_window_width, g_set_window_width_w,  0, 0, 1, 0);  

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_window_height, g_window_height_w, H_window_height,
				   S_setB S_window_height, g_set_window_height_w,  0, 0, 1, 0);


#if (!USE_NO_GUI)
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_selection_color, g_selection_color_w, H_selection_color,
				   S_setB S_selection_color, g_set_selection_color_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_zoom_color, g_zoom_color_w, H_zoom_color,
				   S_setB S_zoom_color, g_set_zoom_color_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_position_color, g_position_color_w, H_position_color,
				   S_setB S_position_color, g_set_position_color_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mark_color, g_mark_color_w, H_mark_color,
				   S_setB S_mark_color, g_set_mark_color_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_listener_color, g_listener_color_w, H_listener_color,
				   S_setB S_listener_color, g_set_listener_color_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_listener_text_color, g_listener_text_color_w, H_listener_text_color,
				   S_setB S_listener_text_color, g_set_listener_text_color_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_enved_waveform_color, g_enved_waveform_color_w, H_enved_waveform_color,
				   S_setB S_enved_waveform_color, g_set_enved_waveform_color_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_filter_control_waveform_color, g_filter_control_waveform_color_w, H_filter_control_waveform_color,
				   S_setB S_filter_control_waveform_color, g_set_filter_control_waveform_color_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_highlight_color, g_highlight_color_w, H_highlight_color,
				   S_setB S_highlight_color, g_set_highlight_color_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_cursor_color, g_cursor_color_w, H_cursor_color,
				   S_setB S_cursor_color, g_set_cursor_color_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_text_focus_color, g_text_focus_color_w, H_text_focus_color,
				   S_setB S_text_focus_color, g_set_text_focus_color_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_sash_color, g_sash_color_w, H_sash_color,
				   S_setB S_sash_color, g_set_sash_color_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_help_button_color, g_help_button_color_w, H_help_button_color,
				   S_setB S_help_button_color, g_set_help_button_color_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_reset_button_color, g_reset_button_color_w, H_reset_button_color,
				   S_setB S_reset_button_color, g_set_reset_button_color_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_quit_button_color, g_quit_button_color_w, H_quit_button_color,
				   S_setB S_quit_button_color, g_set_quit_button_color_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_doit_button_color, g_doit_button_color_w, H_doit_button_color,
				   S_setB S_doit_button_color, g_set_doit_button_color_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_doit_again_button_color, g_doit_again_button_color_w, H_doit_again_button_color,
				   S_setB S_doit_again_button_color, g_set_doit_again_button_color_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_data_color, g_data_color_w, H_data_color,
				   S_setB S_data_color, g_set_data_color_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_graph_color, g_graph_color_w, H_graph_color,
				   S_setB S_graph_color, g_set_graph_color_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_selected_graph_color, g_selected_graph_color_w, H_selected_graph_color,
				   S_setB S_selected_graph_color, g_set_selected_graph_color_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_selected_data_color, g_selected_data_color_w, H_selected_data_color,
				   S_setB S_selected_data_color, g_set_selected_data_color_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_basic_color, g_basic_color_w, H_basic_color,
				   S_setB S_basic_color, g_set_basic_color_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_pushed_button_color, g_pushed_button_color_w, H_pushed_button_color,
				   S_setB S_pushed_button_color, g_set_pushed_button_color_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE(S_color_p, g_color_p_w, 1, 0, 0, H_color_p);
#endif

  XEN_DEFINE_PROCEDURE(S_snd_tempnam,           g_snd_tempnam_w,           0, 0, 0, H_snd_tempnam);
  XEN_DEFINE_PROCEDURE(S_color_dialog,          g_color_dialog_w,          0, 1, 0, H_color_dialog);
  XEN_DEFINE_PROCEDURE(S_orientation_dialog,    g_orientation_dialog_w,    0, 1, 0, H_orientation_dialog);
  XEN_DEFINE_PROCEDURE(S_transform_dialog,      g_transform_dialog_w,      0, 1, 0, H_transform_dialog);
  XEN_DEFINE_PROCEDURE(S_view_files_dialog,     g_view_files_dialog_w,     0, 1, 0, H_view_files_dialog);
  XEN_DEFINE_PROCEDURE(S_edit_header_dialog,    g_edit_header_dialog_w,    0, 1, 0, H_edit_header_dialog);
  XEN_DEFINE_PROCEDURE(S_save_selection_dialog, g_save_selection_dialog_w, 0, 1, 0, H_save_selection_dialog);
  XEN_DEFINE_PROCEDURE(S_save_sound_dialog,     g_save_sound_dialog_w,     0, 1, 0, H_save_sound_dialog);
  XEN_DEFINE_PROCEDURE(S_print_dialog,          g_print_dialog_w,          0, 1, 0, H_print_dialog);
  XEN_DEFINE_PROCEDURE(S_info_dialog,           g_info_dialog_w,           2, 0, 0, H_info_dialog);
  XEN_DEFINE_PROCEDURE(S_sounds,                g_sounds_w,                0, 0, 0, H_sounds);
  XEN_DEFINE_PROCEDURE(S_yes_or_no_p,           g_yes_or_no_p_w,           1, 0, 0, H_yes_or_no_p);
  XEN_DEFINE_PROCEDURE(S_abort,                 g_abort_w,                 0, 0, 0, H_abort);
  XEN_DEFINE_PROCEDURE(S_c_g,                   g_abortq_w,                0, 0, 0, H_abortQ);
  XEN_DEFINE_PROCEDURE(S_snd_version,           g_snd_version_w,           0, 0, 0, H_snd_version);
  XEN_DEFINE_PROCEDURE(S_samples_to_sound_data, g_samples_to_sound_data_w, 0, 7, 0, H_samples_to_sound_data);
  XEN_DEFINE_PROCEDURE(S_snd_print,             g_snd_print_w,             1, 0, 0, H_snd_print);
  XEN_DEFINE_PROCEDURE("little-endian?",        g_little_endian_w,         0, 0, 0, "return #t if host is little endian");
  XEN_DEFINE_PROCEDURE("fmod",                  g_fmod_w,                  2, 0, 0, "C's fmod");
  XEN_DEFINE_PROCEDURE("snd-completion",        g_snd_completion_w,        1, 0, 0, "return completion of arg");
  /* XEN_DEFINE_PROCEDURE(S_clm_print,          g_clm_print,               0, 0, 1, H_clm_print); */
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_just_sounds, g_just_sounds_w, H_just_sounds, S_setB S_just_sounds, g_set_just_sounds_w,  0, 0, 1, 0);

#if HAVE_GUILE
  #define H_during_open_hook S_during_open_hook " (fd name reason): called after file is opened, \
but before data has been read. \n\
  (add-hook! " S_during_open_hook "\n\
    (lambda (fd name reason) \n\
      (if (= (" S_mus_sound_header_type " name) " S_mus_raw ") \n\
          (set! (" S_mus_file_prescaler " fd) 500.0))))"

  #define H_after_open_hook S_after_open_hook " (snd): called just before the new file's window is displayed. \
This provides a way to set various sound-specific defaults. \n\
  (add-hook! " S_after_open_hook "\n\
    (lambda (snd) \n\
      (if (> (" S_channels " snd) 1) \n\
          (set! (" S_channel_style " snd) " S_channels_combined "))))"
#else
  #define H_during_open_hook "$" S_during_open_hook " lambda do |fd, name, reason| ...; called after file is opened, \
but before data has been read. \n\
  $during_open_hook.add_hook!(\"during-open-hook\") do |fd, name, reason|\n\
    if (mus_sound_header_type(name) == Mus_raw)\n\
      set_mus_file_prescaler(fd, 500.0)\n\
    end\n\
  end"

  #define H_after_open_hook S_after_open_hook " (snd): called just before the new file's window is displayed. \
This provides a way to set various sound-specific defaults. \n\
  $after_open_hook.add-hook!(\"set-channels-combined\") do |snd| \n\
    if (channels(snd) > 1) \n\
      set_channel_style(snd, Channels_combined)\n\
    end\n\
  end"
#endif

  XEN_DEFINE_HOOK(during_open_hook,    S_during_open_hook, 3,    H_during_open_hook);    /* args = fd filename reason */
  XEN_DEFINE_HOOK(after_open_hook,     S_after_open_hook, 1,     H_after_open_hook);     /* args = sound */

#if HAVE_GUILE
  #define H_print_hook S_print_hook " (text): called each time some Snd-generated response (text) is about to be appended to the listener. \
If it returns some non-#f result, Snd assumes you've sent the text out yourself, as well as any needed prompt. \n\
  (add-hook! "S_print_hook "\n\
    (lambda (msg) \n\
      (" S_snd_print "\n\
        (format #f \"~A~%[~A]~%~A\" \n\
                msg \n\
                (strftime \"%d-%b %H:%M %Z\" \n\
                           (localtime (current-time))) \n\
                (" S_listener_prompt ")))))"
#else
  #define H_print_hook S_print_hook " (text): called each time some Snd-generated response (text) is about to be appended to the listener. \
If it returns some non-false result, Snd assumes you've sent the text out yourself, as well as any needed prompt. \n\
  $print_hook.add-hook!(\"localtime\") do |msg|\n\
    $stdout.print msg\n\
  false\n\
  end"
#endif

  XEN_DEFINE_HOOK(print_hook, S_print_hook, 1, H_print_hook);          /* arg = text */

  g_init_base();
  g_init_utils();
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
  g_init_track();
#if (!USE_NO_GUI)
  g_init_gxutils();
  g_init_gxcolormaps();
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
  g_init_gxrec();
  g_init_gxfind();
#endif
#if HAVE_GUILE && HAVE_DLFCN_H
  XEN_DEFINE_PROCEDURE("dlopen", g_dlopen_w, 1, 0 ,0, "");
  XEN_DEFINE_PROCEDURE("dlclose", g_dlclose_w, 1, 0 ,0, "");
  XEN_DEFINE_PROCEDURE("dlerror", g_dlerror_w, 0, 0 ,0, "");
  XEN_DEFINE_PROCEDURE("dlinit", g_dlinit_w, 2, 0 ,0, "");
#endif
#if HAVE_LADSPA && HAVE_EXTENSION_LANGUAGE && HAVE_DLFCN_H && HAVE_DIRENT_H
  g_ladspa_to_snd();
#endif

#if HAVE_GUILE
  XEN_EVAL_C_STRING("(define (clm-print . args) (snd-print (apply format #f args)))");
  XEN_EVAL_C_STRING("(define (" S_snd_apropos " val)\
                       (with-output-to-string\
                         (lambda ()\
                           (apropos (if (string? val) val (object->string val))))))");
  XEN_EVAL_C_STRING("(read-set! keywords 'prefix)");
  XEN_EVAL_C_STRING("(print-enable 'source)");
  XEN_EVAL_C_STRING("(defmacro declare args #f)");     /* for optimizer */
  XEN_EVAL_C_STRING("(define redo-edit redo)");        /* consistency with Ruby */
  XEN_EVAL_C_STRING("(define undo-edit undo)");

  /* from ice-9/r4rs.scm but with output to snd listener */
  XEN_EVAL_C_STRING("(define snd-loaded-files '())");
  XEN_EVAL_C_STRING("(define snd-remember-paths #f)");
  XEN_EVAL_C_STRING("(set! %load-path (cons \".\" %load-path))");
  XEN_EVAL_C_STRING("(set! %load-hook \
                       (lambda (filename)\
                         (if %load-verbosely\
                             (snd-print (format #f \"~%;;; loading ~S\" filename)))\
                         (if (not (member filename snd-loaded-files))\
                             (set! snd-loaded-files (cons filename snd-loaded-files)))\
                         (if snd-remember-paths\
                             (let ((curfile (mus-expand-filename filename))\
                                   (last-slash 0))\
                               (do ((i 0 (1+ i)))\
                                   ((= i (string-length curfile)))\
                                 (if (char=? (string-ref curfile i) #\\/)\
	                             (set! last-slash i)))\
                                     (let ((new-path (substring curfile 0 last-slash)))\
                                       (if (and (not (member new-path %load-path))\
                                                (not (string=? (substring curfile (max 0 (- last-slash 5)) last-slash) \"ice-9\")))\
	                                   (set! %load-path (cons new-path %load-path))))))))");
  /* the "ice-9" business is to keep us from loading ice-9/debug.scm when we intend our own debug.scm */
  /* load-from-path can still be fooled, but the user will have to work at it. */
  /* If you load Guile's debug.scm by mistake (set! %load-verbosely #t) to see Snd's names get clobbered! */
#endif
#if HAVE_RUBY
  XEN_EVAL_C_STRING("def clm_print(str, *args)\n\
                       snd_print format(str, *args)\n\
                       end");
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

#if HAVE_ALSA
  XEN_YES_WE_HAVE("alsa");
#endif

  XEN_YES_WE_HAVE("snd");

#if WITH_MODULES
  scm_c_use_module("snd sndlib");
  scm_c_use_module("snd clm");
#endif
}
