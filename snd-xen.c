#include "snd.h"
#include "clm2xen.h"

/* Snd defines its own exit, delay, and frame? clobbering (presumably) the Guile/Gauche versions, (filter is another such collision)
 *
 *   In Scheme, delay is protected in clm2xen.c as %delay
 *              filter is defined in srfi-1 so we need protection against that
 *
 *   In Guile, frame? as %frame?
 *
 *   In Ruby, rand is protected as kernel_rand.
 *
 *   In Forth, Snd's exit is named snd-exit.
 *
 *   In Gauche, apropos is defined in lib/gauche/interactive.scm
 *              optimizer needs local variable access [this is not currently possible -- perhaps in 1.0 says Shiro]
 *              random is implemented via mus-i|frandom (clm.c).
 */


/* Other extension language possibilities:
 *
 * chicken:     (Scheme) looks clean, but no vararg functions, no bignums, gc protection looks iffy, not clear
 *                         how to call C function from Scheme.
 * ecl:         (CL)     do-able; will require that all direct refs be through xen.c (can't include its header files!)
 *                         it also uses the field "complex" which confuses C.
 * eel:         (C)      a commercial product
 * elastic:     (C)      looks dead (no change since 2001), like Lua in calling sequences
 * elk:         (Scheme) looks dead (no change since 1996) and has very severe name-space problems
 * Gambit:      (Scheme) not an extension language, complicated connection to C
 * GameMonkey:  ()       c++, windows oriented (no linux I think)
 * librep:      (CL)     looks dead, was very hard to debug a long time ago, but I still have the macros (xen.h)
 * lua:         (C)      do-able, very primitive -- push-pop-stack etc. (I'm told it is freeware, just a funny license)
 * lush:        (CL)     compilation problem, serious name-space problems (not really an extension language)
 * mzscheme:    (Scheme) support semi-exists (I have the xen.h macros for it), but I refuse to touch it
 * ocaml:       (ML)     not an extension language, as far as I can tell
 * octave:      (Matlab) c++, probably do-able; standard linkage is through octave_value_list arg list.
 *                       Sigh... I'll have to learn C++ to go very far with this.
 * pike:        (C)      not an extension language
 * python:      ()       looks like ruby to me -- why duplicate? (I have about 1/4 of xen.h for this)
 * rscheme:     (Scheme) serious name-space problems
 * s-lang:      (C)      probably doable -- would need to wrap everything in my own struct, and 7 args max is too few.
 * squirrel:    ()       c++, like lua in call sequence
 * stklos:      (Scheme) doesn't build libstklos yet, and has many non-unique names in its headers (checked 0.82)
 */

/* TODO: (gauche)   stacktrace and errors->listener (current-load-history)
 * TODO: (gauche)   error in find lambda -> exit (need better error protection)
 * TODO: (gauche)   unwind-protects around scm_apply (snd-xen g_call)
 * TODO: (guile)    still no func name in backtrace ref to procedure-with-setter setter case (getter has name) -- need a test case
 */


/* -------- protect XEN vars from GC -------- */

static XEN gc_protection;
static int gc_protection_size = 0;
#define DEFAULT_GC_VALUE XEN_UNDEFINED
static int gc_last_cleared = NOT_A_GC_LOC;
static int gc_last_set = NOT_A_GC_LOC;

#if MUS_DEBUGGING
static char **snd_protect_callers = NULL; /* static char* const *callers? no thanks... */
static int cur_gc_index = 0, max_gc_index = 0;
void dump_protection(FILE *Fp);
void dump_protection(FILE *Fp)
{
  if (XEN_VECTOR_P(gc_protection))
    {
      int i;
      fprintf(Fp, "\n\nsnd_protect (%d table size, most used: %d):\n", gc_protection_size, max_gc_index);
      for (i = 0; i < gc_protection_size; i++)
	{
	  XEN gcdat;
	  gcdat = XEN_VECTOR_REF(gc_protection, i);
	  if (!(XEN_EQ_P(gcdat, DEFAULT_GC_VALUE)))
	    {
#if HAVE_SCHEME
	      fprintf(Fp,"  %s:%d %s", snd_protect_callers[i], i, XEN_AS_STRING(gcdat));
#endif
#if HAVE_GUILE
	      if (XEN_HOOK_P(gcdat))
		fprintf(Fp, " -> %s", XEN_AS_STRING(scm_hook_to_list(gcdat)));
#endif
#if HAVE_RUBY
	      fprintf(Fp,"  %s:%d %d %s", snd_protect_callers[i], i, (int)gcdat, XEN_AS_STRING(gcdat));
#endif
#if HAVE_FORTH
	      fprintf(Fp,"  %s:%d %s", snd_protect_callers[i], i, XEN_AS_STRING(gcdat));
	      if (XEN_HOOK_P(gcdat))
		fprintf(Fp, " -> %s", XEN_AS_STRING(XEN_HOOK_PROCEDURES(gcdat)));
#endif
	      fprintf(Fp, "\n");
	    }
	}
    }
}
#endif

#if MUS_DEBUGGING
int snd_protect_1(XEN obj, const char *caller)
#else
int snd_protect(XEN obj)
#endif
{
  int i, old_size;
  XEN tmp;
#if MUS_DEBUGGING
  cur_gc_index++;
  if (cur_gc_index > max_gc_index) max_gc_index = cur_gc_index;
#endif
  if (gc_protection_size == 0)
    {
      gc_protection_size = 512;
      gc_protection = XEN_MAKE_VECTOR(gc_protection_size, DEFAULT_GC_VALUE);
      XEN_PROTECT_FROM_GC(gc_protection);
      XEN_VECTOR_SET(gc_protection, 0, obj);
      gc_last_set = 0;
#if MUS_DEBUGGING
      snd_protect_callers = (char **)calloc(gc_protection_size, sizeof(char *));
      snd_protect_callers[0] = (char *)caller;
#endif
    }
  else
    {
      if ((gc_last_cleared >= 0) && 
	  XEN_EQ_P(XEN_VECTOR_REF(gc_protection, gc_last_cleared), DEFAULT_GC_VALUE))
	{
	  /* we hit this branch about 2/3 of the time */
	  XEN_VECTOR_SET(gc_protection, gc_last_cleared, obj);
#if MUS_DEBUGGING
	  snd_protect_callers[gc_last_cleared] = (char *)caller;
#endif
	  gc_last_set = gc_last_cleared;
	  gc_last_cleared = NOT_A_GC_LOC;
	  return(gc_last_set);
	}

      for (i = gc_last_set; i < gc_protection_size; i++)
	if (XEN_EQ_P(XEN_VECTOR_REF(gc_protection, i), DEFAULT_GC_VALUE))
	  {
	    XEN_VECTOR_SET(gc_protection, i, obj);
#if MUS_DEBUGGING
	    snd_protect_callers[i] = (char *)caller;
#endif
	    gc_last_set = i;
	    return(gc_last_set);
	  }
      for (i = 0; i < gc_last_set; i++)
	if (XEN_EQ_P(XEN_VECTOR_REF(gc_protection, i), DEFAULT_GC_VALUE))
	  {
	    /* here we average 3 checks before a hit, so this isn't as bad as it looks */
	    XEN_VECTOR_SET(gc_protection, i, obj);
#if MUS_DEBUGGING
	    snd_protect_callers[i] = (char *)caller;
#endif
	    gc_last_set = i;
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
#if HAVE_RUBY || HAVE_FORTH
      XEN_UNPROTECT_FROM_GC(tmp);
#endif
#if MUS_DEBUGGING
      snd_protect_callers = (char **)realloc(snd_protect_callers, gc_protection_size * sizeof(char *));
      snd_protect_callers[old_size] = (char *)caller;
#endif
      gc_last_set = old_size;
    }
  return(gc_last_set);
}

void snd_unprotect_at(int loc)
{
#if MUS_DEBUGGING
  cur_gc_index--;
#endif
  if (loc >= 0)
    {
      XEN_VECTOR_SET(gc_protection, loc, DEFAULT_GC_VALUE);
      gc_last_cleared = loc;
    }
}

XEN snd_protected_at(int loc)
{
  if (loc >= 0)
    return(XEN_VECTOR_REF(gc_protection, loc));
  return(DEFAULT_GC_VALUE);
}


/* -------- error handling -------- */

static char *last_file_loaded = NULL;

void redirect_xen_error_to(void (*handler)(const char *msg, void *ufd), void *data) /* currently could be local */
{
  ss->xen_error_handler = handler;
  ss->xen_error_data = data;
}

static void call_xen_error_handler(const char *msg)
{
  /* make sure it doesn't call itself recursively */
  void (*old_xen_error_handler)(const char *msg, void *data);
  void *old_xen_error_data;
  old_xen_error_handler = ss->xen_error_handler;
  old_xen_error_data = ss->xen_error_data;
  ss->xen_error_handler = NULL;
  ss->xen_error_data = NULL;
  (*(old_xen_error_handler))(msg, old_xen_error_data);
  ss->xen_error_handler = old_xen_error_handler;
  ss->xen_error_data = old_xen_error_data;
}

void redirect_snd_print_to(void (*handler)(const char *msg, void *ufd), void *data)
{
  ss->snd_print_handler = handler;
  ss->snd_print_data = data;
}

void redirect_everything_to(void (*handler)(const char *msg, void *ufd), void *data)
{
  redirect_snd_error_to(handler, data);
  redirect_xen_error_to(handler, data);
  redirect_snd_warning_to(handler, data);
  redirect_snd_print_to(handler, data);
}

void redirect_errors_to(void (*handler)(const char *msg, void *ufd), void *data)
{
  redirect_snd_error_to(handler, data);
  redirect_xen_error_to(handler, data);
  redirect_snd_warning_to(handler, data);
}

static char *gl_print(XEN result);

#if (!HAVE_GAUCHE)
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
#if HAVE_GUILE || HAVE_FORTH
			  if (XEN_PROCEDURE_P(cur_arg))
			    {
			      /* don't need the source, just the name here, I think */
			      XEN str;
			      str = XEN_PROCEDURE_NAME(cur_arg);
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
  result = C_TO_XEN_STRING(errmsg);
  FREE(errmsg);
  return(xen_return_first(result, args));
}
#endif


/* ---------------- GUILE error handler ---------------- */

#ifndef MUS_DEBUGGING
  #define MUS_DEBUGGING 0
#endif

#if HAVE_GUILE
static XEN snd_catch_scm_error(void *data, XEN tag, XEN throw_args) /* error handler, data = handler_data = caller's name */
{
  char *possible_code;
  XEN port;
  int port_gc_loc, stack_gc_loc;
  XEN stack = XEN_FALSE;
  char *name_buf = NULL, *tag_name = NULL;
  bool need_comma = false;

  if (XEN_SYMBOL_P(tag)) tag_name = XEN_SYMBOL_TO_C_STRING(tag);
  if ((tag_name) && (strcmp(tag_name, "snd-top-level") == 0))
    return(throw_args); /* not an error -- just a way to exit the current context */

  port = scm_mkstrport(XEN_ZERO, 
		       scm_make_string(XEN_ZERO, C_TO_XEN_CHAR(0)),
		       SCM_OPN | SCM_WRTNG,
		       "snd error handler");
  port_gc_loc = snd_protect(port);
  XEN_PUTS("\n", port);

  if ((MUS_DEBUGGING) || (ss->batch_mode))
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
  possible_code = (char *)data; /* or is this the caller's name? */
  if ((possible_code) && 
      (snd_strlen(possible_code) < PRINT_BUFFER_SIZE))
    {
      /* not actually sure if this is always safe */
      XEN_PUTS("\n; ", port);
      XEN_PUTS(possible_code, port);
    }
  XEN_FLUSH_PORT(port); /* needed to get rid of trailing garbage chars?? -- might be pointless now */
  name_buf = copy_string(XEN_TO_C_STRING(XEN_PORT_TO_STRING(port)));

  {
    bool show_error = true;
    if ((!tag_name) || (strcmp(tag_name, "snd-error") != 0)) /* otherwise an explicit snd-error call which has run the hook already */
      show_error = (!(run_snd_error_hook(name_buf)));
    if (show_error)
      {
	if (ss->xen_error_handler)
	  call_xen_error_handler(name_buf);
	else
	  {
	    listener_append_and_prompt(name_buf);
	    if (!(listener_is_visible()))
	      snd_error_without_redirection_or_hook(name_buf);
	    /* we're in xen_error from the redirection point of view and we already checked snd-error-hook */
	  }
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
		   XEN_AS_STRING(XEN_CAR(throw_args)), 
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

  if (show_backtrace(ss)) 
    {
      XEN bt = rb_funcall(err, rb_intern("caller"), 0); 
      if (XEN_VECTOR_P(bt) && XEN_VECTOR_LENGTH(bt) > 0) 
	{
	  long i; 
	  msg = snd_strcat(msg, "\n", &size); 
	  for (i = 0; i < XEN_VECTOR_LENGTH(bt); i++) 
	    { 
	      msg = snd_strcat(msg, XEN_TO_C_STRING(XEN_VECTOR_REF(bt, i)), &size); 
	      msg = snd_strcat(msg, "\n", &size); 
	    } 
	} 
    }
  if (strcmp(rb_id2name(tag), "Snd_error") != 0)
    {
      if (!(run_snd_error_hook(msg)))
	{
	  if (ss->xen_error_handler)
	    call_xen_error_handler(msg);
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
static XEN snd_internal_stack_catch(XEN tag,
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

static XEN g_call0_1(void *arg)
{
  return(XEN_CALL_0_NO_CATCH((XEN)arg));
}

XEN g_call0(XEN proc, const char *caller) /* replacement for gh_call0 -- protect ourselves from premature exit(!$#%@$) */
{
  return(snd_catch_any(g_call0_1, (void *)proc, caller));
}

static XEN g_call1_1(void *arg)
{
  return(XEN_CALL_1_NO_CATCH(((XEN *)arg)[0], ((XEN *)arg)[1]));
}

XEN g_call1(XEN proc, XEN arg, const char *caller)
{
  XEN args[2];
  args[0] = proc;
  args[1] = arg;
  return(snd_catch_any(g_call1_1, (void *)args, caller));
}

static XEN g_call_any_1(void *arg)
{
  return(XEN_APPLY_NO_CATCH(((XEN *)arg)[0], ((XEN *)arg)[1]));
}

XEN g_call_any(XEN proc, XEN arglist, const char *caller)
{
  XEN args[2];
  args[0] = proc;
  args[1] = arglist;
  return(snd_catch_any(g_call_any_1, (void *)args, caller));
}

static XEN g_call2_1(void *arg)
{
  return(XEN_CALL_2_NO_CATCH(((XEN *)arg)[0], ((XEN *)arg)[1], ((XEN *)arg)[2]));
}

XEN g_call2(XEN proc, XEN arg1, XEN arg2, const char *caller)
{
  XEN args[3];
  args[0] = proc;
  args[1] = arg1;
  args[2] = arg2;
  return(snd_catch_any(g_call2_1, (void *)args, caller));
}

static XEN g_call3_1(void *arg)
{
  return(XEN_CALL_3_NO_CATCH(((XEN *)arg)[0], ((XEN *)arg)[1], ((XEN *)arg)[2], ((XEN *)arg)[3]));
}

XEN g_call3(XEN proc, XEN arg1, XEN arg2, XEN arg3, const char *caller)
{
  XEN args[4];
  args[0] = proc;
  args[1] = arg1;
  args[2] = arg2;
  args[3] = arg3;
  return(snd_catch_any(g_call3_1, (void *)args, caller));
}
#endif


#if HAVE_GAUCHE

#if 0
SCM_EXTERN void Scm_ShowStackTrace(ScmPort *out, ScmObj stacklite,
                                   int maxdepth, int skip, int offset,
                                   int format);

				   SCM_EXTERN void Scm_ReportError(ScmObj e); /* calls stack trace */


static ScmObj repl_error_handle(ScmObj *args, int nargs, void *data)
{
    SCM_ASSERT(nargs == 1);
    Scm_ReportError(args[0]);
    return SCM_TRUE;
}

ScmObj Scm_VMRepl(ScmObj reader, ScmObj evaluator,
                  ScmObj printer, ScmObj prompter)
{
    ScmObj ehandler, reploop;
    ScmObj *packet = SCM_NEW_ARRAY(ScmObj, 4);
    packet[0] = reader;
    packet[1] = evaluator;
    packet[2] = printer;
    packet[3] = prompter;
    ehandler = Scm_MakeSubr(repl_error_handle, packet, 1, 0, SCM_FALSE);
    reploop = Scm_MakeSubr(repl_main, packet, 0, 0, SCM_FALSE);
    Scm_VMPushCC(repl_loop_cc, (void**)packet, 4);
    return Scm_VMWithErrorHandler(ehandler, reploop);
}

#endif
/*
 * Show stack trace.
 *   stacklite - return value of Scm_GetStackLite
 *   maxdepth - maximum # of stacks to be shown.
 *              0 to use the default.  -1 for unlimited.

 *   skip     - ignore this number of frames.  Useful to call this from
 *              a Scheme error handling routine, in order to skip the
 *              frames of the handler itself.
 *   offset   - add this to the frame number.  Useful to show a middle part
 *              of frames only, by combining the skip parameter.
 *   format   - SCM_STACK_TRACE_FORMAT_* enum value.  EXPERIMENTAL.
 */

XEN snd_catch_any(XEN_CATCH_BODY_TYPE body, void *body_data, const char *caller)
{
  XEN result = XEN_FALSE;

  SCM_UNWIND_PROTECT {
    result = (*body)(body_data);
  }
  SCM_WHEN_ERROR {
    fprintf(stderr, "Error!");
    /* SCM_NEXT_HANDLER; */
  }
  SCM_END_PROTECT;

  return(result);
}
#endif

#if HAVE_RUBY || HAVE_FORTH
XEN snd_catch_any(XEN_CATCH_BODY_TYPE body, void *body_data, const char *caller)
{
  return((*body)(body_data));
}
#endif

#if (!HAVE_EXTENSION_LANGUAGE)
/* no extension language but user managed to try to evaluate something -- one way is to
 *   activate the minibuffer (via click) and type an expression into it
 */
XEN snd_catch_any(XEN_CATCH_BODY_TYPE body, void *body_data, const char *caller)
{
  snd_error("This version of Snd has no extension language, so there's no way for %s to evaluate anything", caller);
  return(XEN_FALSE);
}
#endif

bool procedure_arity_ok(XEN proc, int args)
{
  XEN arity;
  int rargs;
  arity = XEN_ARITY(proc);

#if HAVE_RUBY
  rargs = XEN_TO_C_INT(arity);
  return(xen_rb_arity_ok(rargs, args));
#endif

#if HAVE_FORTH
  rargs = XEN_TO_C_INT(arity);
  if (rargs != args)
    return(false);
#endif

#if HAVE_GUILE
  {
    int oargs, restargs, loc;
    loc = snd_protect(arity);
    rargs = XEN_TO_C_INT(XEN_CAR(arity));
    oargs = XEN_TO_C_INT(XEN_CADR(arity));
    restargs = ((XEN_TRUE_P(XEN_CADDR(arity))) ? 1 : 0);
    snd_unprotect_at(loc);
    if (rargs > args) return(false);
    if ((restargs == 0) && ((rargs + oargs) < args)) return(false);
  }
#endif

#if HAVE_GAUCHE
  {
    int oargs, loc;
    loc = snd_protect(arity);
    rargs = XEN_TO_C_INT(XEN_CAR(arity));
    oargs = XEN_TO_C_INT(XEN_CDR(arity));
    snd_unprotect_at(loc);
    if (rargs > args) return(false);
    if ((rargs + oargs) < args) return(false);
  }
#endif

  return(true);
}

char *procedure_ok(XEN proc, int args, const char *caller, const char *arg_name, int argn)
{
  /* if string returned, needs to be freed */
  /* 0 args is special => "thunk" meaning in this case that optional args are not ok (applies to as-one-edit and two menu callbacks) */
  XEN arity;
  int rargs;

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
      if (!xen_rb_arity_ok(rargs, args))
 	return(mus_format(_("%s function (%s arg %d) should take %d args, not %d"),
 			  arg_name, caller, argn, args, (rargs < 0) ? (-rargs) : rargs));
      /*
      if ((rargs > args) ||
	  ((rargs < 0) && (-rargs > args)))
	return(mus_format(_("%s function (%s arg %d) should take %d args, not %d"), 
			  arg_name, caller, argn, args, (rargs < 0) ? (-rargs) : rargs));
      if ((args == 0) && (rargs != 0))
	return(mus_format(_("%s function (%s arg %d) should take no args, not %d"), 
			  arg_name, caller, argn, (rargs < 0) ? (-rargs) : rargs));
      */
#endif

#if HAVE_GUILE
      {
	int oargs, restargs, loc;
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
	if ((args == 0) &&
	    ((rargs != 0) || (oargs != 0) || (restargs != 0)))
	  return(mus_format(_("%s function (%s arg %d) should take no args, not %d"), 
			    arg_name, caller, argn, rargs + oargs + restargs));
      }
#endif

#if HAVE_GAUCHE
      {
	int oargs, loc;
	loc = snd_protect(arity);
	rargs = XEN_TO_C_INT(XEN_CAR(arity));
	oargs = XEN_TO_C_INT(XEN_CDR(arity));
	snd_unprotect_at(loc);
	if (rargs > args)
	  return(mus_format(_("%s function (%s arg %d) should take %d argument%s, but instead requires %d"),
			    arg_name, caller, argn, args, (args != 1) ? "s" : "", rargs));
	if ((rargs + oargs) < args)
	  return(mus_format(_("%s function (%s arg %d) should accept at least %d argument%s, but instead accepts only %d"),
			    arg_name, caller, argn, args, (args != 1) ? "s" : "", rargs + oargs));
	if ((args == 0) &&
	    ((rargs != 0) || (oargs != 0)))
	  return(mus_format(_("%s function (%s arg %d) should take no args, not %d"), 
			    arg_name, caller, argn, rargs + oargs));
      }
#endif

#if HAVE_FORTH
      rargs = XEN_TO_C_INT(arity);
      if (rargs != args)
	return(mus_format(_("%s function (%s arg %d) should take %d args, not %d"),
			  arg_name, caller, argn, args, rargs));
#endif
    }
  return(NULL);
}

XEN snd_no_such_file_error(const char *caller, XEN filename)
{
  XEN_ERROR(NO_SUCH_FILE,
	    XEN_LIST_3(C_TO_XEN_STRING(caller),
		       filename,
		       C_TO_XEN_STRING(snd_open_strerror())));
  return(XEN_FALSE);
}

XEN snd_no_such_channel_error(const char *caller, XEN snd, XEN chn)
{
  int index = NOT_A_SOUND;
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
  XEN_ERROR(XEN_ERROR_TYPE("no-active-selection"),
	    XEN_LIST_1(C_TO_XEN_STRING(caller)));
  return(XEN_FALSE);
}

XEN snd_bad_arity_error(const char *caller, XEN errstr, XEN proc)
{
  XEN_ERROR(XEN_ERROR_TYPE("bad-arity"),
            XEN_LIST_3(C_TO_XEN_STRING(caller),
                       errstr,
		       proc));
  return(XEN_FALSE);
}



/* -------- various evaluators (within our error handler) -------- */

XEN eval_str_wrapper(void *data)
{
#if MUS_DEBUGGING
  if (data == NULL)
    {
      fprintf(stderr, "null string passed to eval_str_wrapper");
      abort();
    }
#endif
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

XEN string_to_form(char *str)
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


char *g_print_1(XEN obj) /* free return val */
{
#if HAVE_GAUCHE || HAVE_FORTH || HAVE_RUBY
  return(copy_string(XEN_AS_STRING(obj))); 
#endif
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
#if (!HAVE_EXTENSION_LANGUAGE)
  return(NULL);
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
#if HAVE_SCHEME || HAVE_FORTH
  sprintf(newbuf, "#("); 
#endif
#if HAVE_RUBY
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
#if HAVE_SCHEME || HAVE_FORTH
  newbuf = snd_strcat(newbuf, " ...)", &savelen);
#endif
#if HAVE_RUBY
  newbuf = snd_strcat(newbuf, " ...]", &savelen);
#endif
  return(newbuf);
}

void snd_report_result(XEN result, const char *buf)
{
  /* kbd macros, startup evalled args */
  char *str = NULL;
  str = gl_print(result);
  if (ss->snd_print_handler)
    {
      /* make sure it doesn't call itself recursively */
      void (*old_snd_print_handler)(const char *msg, void *data);
      void *old_snd_print_data;
      old_snd_print_handler = ss->snd_print_handler;
      old_snd_print_data = ss->snd_print_data;
      ss->snd_print_handler = NULL;
      ss->snd_print_data = NULL;
      (*(old_snd_print_handler))(str, old_snd_print_data);
      ss->snd_print_handler = old_snd_print_handler;
      ss->snd_print_data = old_snd_print_data;
    }
  else
    {
      if (buf) listener_append(buf);
      listener_append_and_prompt(str);
    }
  if (str) FREE(str);
}

void snd_report_listener_result(XEN form)
{
#if HAVE_RUBY || HAVE_FORTH || HAVE_GAUCHE
  snd_report_result(form, "\n");
#endif
#if HAVE_GUILE
  snd_report_result(snd_catch_any(eval_form_wrapper, (void *)form, NULL), "\n");
#endif
}

static char *stdin_str = NULL;

void clear_stdin(void)
{
  if (stdin_str) FREE(stdin_str);
  stdin_str = NULL;
}

static char *stdin_check_for_full_expression(char *newstr)
{
#if HAVE_SCHEME
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
#if HAVE_SCHEME
  end_of_text = check_balance(stdin_str, 0, snd_strlen(stdin_str), false); /* last-arg->not in listener */
  if (end_of_text > 0)
    {
      if (end_of_text + 1 < snd_strlen(stdin_str))
	stdin_str[end_of_text + 1] = 0;
      return(stdin_str);
    }
  return(NULL);
#endif
  return(stdin_str);
}

static void string_to_stdout(const char *msg, void *ignored)
{
  fprintf(stdout, "%s\n", msg);
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
      redirect_everything_to(string_to_stdout, NULL);
      result = snd_catch_any(eval_str_wrapper, (void *)str, str);
      redirect_everything_to(NULL, NULL);
      loc = snd_protect(result);
      if (stdin_str) FREE(stdin_str);
      /* same as str here; if c-g! evaluated from stdin, clear_listener is called which frees/nullifies stdin_str */
      stdin_str = NULL;
      str = gl_print(result);
      string_to_stdout(str, NULL);
      if (str) FREE(str);
      snd_unprotect_at(loc);
    }
}

static void string_to_stderr_and_listener(const char *msg, void *ignore)
{
  fprintf(stderr, "%s\n", msg);
  if (listener_exists()) /* the idea here is to save startup errors until we can post them */
    {
      listener_append((char *)msg);
      listener_append("\n");
    }
  else 
    {
      if (ss->startup_errors)
	{
	  char *temp;
	  temp = ss->startup_errors;
	  ss->startup_errors = mus_format("%s\n%s %s\n", ss->startup_errors, listener_prompt(ss), msg);
	  FREE(temp);
	}
      else ss->startup_errors = copy_string(msg); /* initial prompt is already there */
    }
}

static bool snd_load_init_file_1(const char *filename)
{
  char *expr, *fullname;
  XEN result;
  bool happy = false;
  fullname = mus_expand_filename(filename);
  if (mus_file_probe(fullname))
    {
      happy = true;
#if HAVE_SCHEME
      expr = mus_format("(load %s)", fullname);
#endif
#if HAVE_RUBY || HAVE_FORTH
      expr = mus_format("load(%s)", fullname);
#endif
      result = snd_catch_any(eval_file_wrapper, (void *)fullname, expr);
      FREE(expr);

#if HAVE_RUBY || HAVE_FORTH
      if (!(XEN_TRUE_P(result)))
	{
	  int loc;
	  char *str;
	  loc = snd_protect(result);
	  str = gl_print(result);
	  if (str)
	    {
	      expr = mus_format("%s: %s\n", filename, str);
	      snd_error_without_format(expr);
	      FREE(str);
	      FREE(expr);
	    }
	  snd_unprotect_at(loc);
	}
#endif
    }
  if (fullname) FREE(fullname);
  return(happy);
}

void snd_load_init_file(bool no_global, bool no_init)
{
  /* look for ".snd" on the home directory; return true if an error occurred (to try to get that info to the user's attention) */
  /* called only in snd-g|xmain.c at initialization time */

  /* changed Oct-05 because the Scheme/Ruby/Forth choices are becoming a hassle --
   *   now save-options has its own file ~/.snd_prefs_guile|ruby|forth which is loaded first, if present
   *     then ~/.snd_guile|ruby|forth, if present
   *     then ~/.snd for backwards compatibility
   * snd_options does not write ~/.snd anymore, but overwrites the .snd_prefs_* file
   * use set init files only change the ~/.snd choice
   *
   * there are parallel choices for the global configuration file: /etc/snd_guile|ruby|forth.conf
   */
#if HAVE_EXTENSION_LANGUAGE
#if HAVE_GUILE
  #define SND_EXT_CONF "/etc/snd_guile.conf"
  #define SND_PREFS "~/.snd_prefs_guile"
  #define SND_INIT "~/.snd_guile"
#endif

#if HAVE_GAUCHE
  #define SND_EXT_CONF "/etc/snd_gauche.conf"
  #define SND_PREFS "~/.snd_prefs_gauche"
  #define SND_INIT "~/.snd_gauche"
#endif

#if HAVE_RUBY
  #define SND_EXT_CONF "/etc/snd_ruby.conf"
  #define SND_PREFS "~/.snd_prefs_ruby"
  #define SND_INIT "~/.snd_ruby"
#endif

#if HAVE_FORTH
  #define SND_EXT_CONF "/etc/snd_forth.conf"
  #define SND_PREFS "~/.snd_prefs_forth"
  #define SND_INIT "~/.snd_forth"
#endif

#define SND_INIT_FILE_ENVIRONMENT_NAME "SND_INIT_FILE"
#ifndef MUS_WINDOZE
  #define INIT_FILE_NAME "~/.snd"
#else
  #define INIT_FILE_NAME "snd-init"
#endif

  #define SND_CONF "/etc/snd.conf"
  redirect_snd_print_to(string_to_stdout, NULL);
  redirect_errors_to(string_to_stderr_and_listener, NULL);

  /* check for global configuration files (/etc/snd*) */
  if (!no_global)
    {
      snd_load_init_file_1(SND_EXT_CONF);
      snd_load_init_file_1(SND_CONF);
    }

  /* now load local init file(s) */
  if (!no_init)
    {
      char *temp;
      snd_load_init_file_1(SND_PREFS);  /* check for possible prefs dialog output */
      snd_load_init_file_1(SND_INIT);
      temp = getenv(SND_INIT_FILE_ENVIRONMENT_NAME);
      if (temp)
	snd_load_init_file_1(temp);
      else snd_load_init_file_1(INIT_FILE_NAME);
    }

  redirect_everything_to(NULL, NULL);
#endif
}

static char *find_source_file(char *orig);

void snd_load_file(char *filename)
{
  char *str = NULL, *str2 = NULL;
  XEN result = XEN_TRUE;

  str = mus_expand_filename(filename);
  if (!(mus_file_probe(str)))
    str = find_source_file(str); /* this frees original str, returns either NULL or a new (needs-to-be-freed) filename */
  if (!str)
    {
      snd_error(_("can't load %s: %s"), filename, snd_open_strerror());
      return;
    }

  str2 = mus_format("(load \"%s\")", filename);   /* currently unused in Forth and Ruby */
  result = snd_catch_any(eval_file_wrapper, (void *)str, str2);
  if (str) FREE(str);
  if (str2) FREE(str2);

#if HAVE_RUBY || HAVE_FORTH
  if (!(XEN_TRUE_P(result)))
    {
      int loc;
      loc = snd_protect(result);
      str = gl_print(result);
      if (str)
	{
	  snd_error_without_format(str);
	  FREE(str);
	}
      snd_unprotect_at(loc);
    }
#endif
}

static XEN g_snd_print(XEN msg)
{
  #define H_snd_print "(" S_snd_print " str): display str in the listener window"
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

bool listener_print_p(const char *msg)
{
  static int print_depth = 0;
  XEN res = XEN_FALSE;
  if ((msg) && (print_depth == 0) && (snd_strlen(msg) > 0) && (XEN_HOOKED(print_hook)))
    {
      print_depth++;
      res = run_or_hook(print_hook, 
			XEN_LIST_1(C_TO_XEN_STRING(msg)),
			S_print_hook);
      print_depth--;
    }
 return(XEN_FALSE_P(res));
}

#if HAVE_FORTH
static int features_length(char *features)
{
  int i, len, num = 1;
  len = strlen(features);
  for (i = 0; i < len - 2; i++)
    if (features[i] == ' ') num++;
  return(num);
}

static void fth_local_print(ficlVm *vm, char *msg)
{
  /* can't use g_snd_print (as in xen.c) because listener doesn't exist in this context */
  fprintf(stderr, "error: %s\n", msg);
}

static void (*old_print_hook)(ficlVm *vm, char *msg);
#endif


void check_features_list(char *features)
{
  /* check for list of features, report any missing, exit (for compsnd) */
  /*  this can't be in snd.c because we haven't fully initialized the extension language and so on at that point */
  if (!features) return;

#if HAVE_GUILE
  XEN_EVAL_C_STRING(mus_format("(for-each \
                                  (lambda (f)	\
                                    (if (not (provided? f)) \
                                        (display (format #f \"~%%no ~A!~%%~%%\" f)))) \
                                  (list %s))", features));
#endif

#if HAVE_RUBY
  /* provided? is defined in examp.rb */
  XEN_EVAL_C_STRING(mus_format("[%s].each do |f|\n\
                                  unless $LOADED_FEATURES.map do |ff| File.basename(ff) end.member?(f.to_s.tr(\"_\", \"-\"))\n\
                                    $stderr.printf(\"~\\nno %%s!\\n\\n\", f.id2name)\n\
                                  end\n\
                                end\n", features));
#endif

#if HAVE_GAUCHE
  XEN_EVAL_C_STRING(mus_format("(for-each \
                                  (lambda (f)	\
                                    (if (not (provided? (symbol->string f))) \
                                        (display (string-append (string #\\newline) \"no \" (symbol->string f) \"!\" (string #\\newline))))) \
                                  (list %s))", features));
#endif

#if HAVE_FORTH
  old_print_hook = fth_print_hook;
  fth_print_hook = fth_local_print;
  XEN_EVAL_C_STRING(mus_format("%s %d >list [each] provided? not [if] \"oops\" .error [end-each]\n",
			       features, 
			       features_length(features)));
  fth_print_hook = old_print_hook;
#endif
  snd_exit(0);
}


Float string_to_Float(char *str, Float lo, const char *field_name)
{
#if HAVE_EXTENSION_LANGUAGE
  XEN res;
  Float f;
  res = snd_catch_any(eval_str_wrapper, (void *)str, "string->float");
  if (XEN_NUMBER_P(res))
    {
      f = XEN_TO_C_DOUBLE(res);
      if (f < lo)
	snd_error(_("%s: %.3f is invalid"), field_name, f);
      else return(f);
    }
  else snd_error(_("%s is not a number"), str);
  return(0.0);
#else
  Float res = 0.0;
  if (str) 
    {
      if (!(sscanf(str, "%f", &res)))
	snd_error(_("%s is not a number"), str);
      else
	{
	  if (res < lo)
	    snd_error(_("%s: %.3f is invalid"), field_name, res);
	}
    }
  return(res);
#endif
}

int string_to_int(char *str, int lo, const char *field_name) 
{
#if HAVE_EXTENSION_LANGUAGE
  XEN res;
  res = snd_catch_any(eval_str_wrapper, (void *)str, "string->int");
  if (XEN_NUMBER_P(res))
    {
      int val;
      val = XEN_TO_C_INT(res);
      if (val < lo)
	snd_error(_("%s: %d is invalid"), field_name, val);
      else return(val);
    }
  else snd_error(_("%s: %s is not a number"), field_name, str);
  return(0);
#else
  int res = 0;
  if (str) 
    {
      if (!(sscanf(str, "%d", &res)))
	snd_error(_("%s: %s is not a number"), field_name, str);
      else
	{
	  if (res < lo)
	    snd_error(_("%s: %d is invalid"), field_name, res);
	}
    }
  return(res);
#endif
}

off_t string_to_off_t(char *str, off_t lo, const char *field_name)
{
#if HAVE_EXTENSION_LANGUAGE
  XEN res;
  res = snd_catch_any(eval_str_wrapper, (void *)str, "string->off_t");
  if (XEN_NUMBER_P(res))
    {
      off_t val;
      val = XEN_TO_C_OFF_T(res);
      if (val < lo)
	snd_error(_("%s: " OFF_TD " is invalid"), field_name, val);
      else return(val);
    }
  else snd_error(_("%s: %s is not a number"), field_name, str);
  return(0);
#else
  off_t res = 0;
  if (str) 
    {
      if (!(sscanf(str, OFF_TD , &res)))
	snd_error(_("%s: %s is not a number"), field_name, str);
      else
	{
	  if (res < lo)
	    snd_error(_("%s: " OFF_TD " is invalid"), field_name, res);
	}
    }
  return(res);
#endif
}

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

XEN run_or_hook(XEN hook, XEN args, const char *caller)
{
  XEN result = XEN_FALSE; /* (or): #f */
  XEN hook_result = XEN_FALSE;
  XEN procs = XEN_HOOK_PROCEDURES (hook);
  while (XEN_NOT_NULL_P (procs))
    {
      if (!(XEN_EQ_P(args, XEN_EMPTY_LIST)))
	result = XEN_APPLY(XEN_CAR(procs), args, caller);
      else result = XEN_CALL_0(XEN_CAR(procs), caller);
      if (XEN_NOT_FALSE_P(result)) 
#if HAVE_GUILE
	{
	  if (!(XEN_EQ_P(result, SCM_UNSPECIFIED)))
	    hook_result = result; /* return last non-#f result, but not #<unspecified>! */
	}
#else
        hook_result = result;
#endif
      procs = XEN_CDR (procs);
    }
  return(xen_return_first(hook_result, args));
}



/* this needs to be in Snd (rather than sndlib2xen.c) because it calls post_it */
static XEN g_mus_audio_describe(void) 
{
  #define H_mus_audio_describe "("  S_mus_audio_describe "): post a description of the audio hardware state in the Help dialog"
  post_it("Audio State", mus_audio_report()); 
  return(XEN_TRUE);
}

#if HAVE_SCHEME && HAVE_DLFCN_H
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
	      err = (char *)dlerror();
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

static XEN g_snd_global_state(void)
{
  return(XEN_WRAP_C_POINTER(ss));
}

#if MUS_DEBUGGING
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


static XEN g_gc_off(void) 
{
#if HAVE_GUILE
  #define H_gc_off "(" S_gc_off ") is a no-op"
#else
  #define H_gc_off "(" S_gc_off ") turns off garbage collection"
#endif

#if HAVE_RUBY && HAVE_RB_GC_DISABLE
  rb_gc_disable();
#endif

#if HAVE_FORTH
  fth_gc_off();
#endif

#if HAVE_GAUCHE
  GC_disable();
#endif
  return(XEN_FALSE);
}

static XEN g_gc_on(void) 
{
#if HAVE_GUILE
  #define H_gc_on "(" S_gc_on ") is a no-op"
#else
  #define H_gc_on "(" S_gc_on ") turns on garbage collection"
#endif

#if HAVE_RUBY && HAVE_RB_GC_DISABLE
  rb_gc_enable();
#endif

#if HAVE_FORTH
  fth_gc_on();
#endif

#if HAVE_GAUCHE
  GC_enable();
#endif
  return(XEN_FALSE);
}


#if (!HAVE_SCM_CONTINUATION_P)
#if HAVE_GUILE
static XEN g_continuation_p(XEN obj)
{
#ifdef SCM_CONTINUATIONP
  return(C_TO_XEN_BOOLEAN(SCM_NIMP(obj) && SCM_CONTINUATIONP(obj)));
#else
  return(C_TO_XEN_BOOLEAN(XEN_PROCEDURE_P(obj)));
#endif
}
#endif
/* in Gauche there's SCM_CCONTP(obj), but it doesn't appear to be used or declared publically */
#if HAVE_GAUCHE
static XEN g_continuation_p(XEN obj)
{
  return(XEN_FALSE);
}
#endif
#endif

#if (!HAVE_GAUCHE)
static XEN g_fmod(XEN a, XEN b)
{
  XEN_ASSERT_TYPE(XEN_NUMBER_P(a), a, XEN_ARG_1, "fmod", " a number");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(b), b, XEN_ARG_2, "fmod", " a number");
  return(C_TO_XEN_DOUBLE(fmod(XEN_TO_C_DOUBLE(a), XEN_TO_C_DOUBLE(b))));
}
#endif

#if HAVE_SPECIAL_FUNCTIONS
static XEN g_j0(XEN x)
{
  #define H_j0 "(j0 x) returns the regular cylindrical bessel function J0(x)"
  XEN_ASSERT_TYPE(XEN_NUMBER_P(x), x, XEN_ONLY_ARG, "j0", " a number");
  return(C_TO_XEN_DOUBLE(j0(XEN_TO_C_DOUBLE(x))));
}

static XEN g_j1(XEN x)
{
  #define H_j1 "(j1 x) returns the regular cylindrical bessel function J1(x)"
  XEN_ASSERT_TYPE(XEN_NUMBER_P(x), x, XEN_ONLY_ARG, "j1", " a number");
  return(C_TO_XEN_DOUBLE(j1(XEN_TO_C_DOUBLE(x))));
}

static XEN g_jn(XEN order, XEN x)
{
  #define H_jn "(jn n x) returns the regular cylindrical bessel function Jn(x)"
  XEN_ASSERT_TYPE(XEN_INTEGER_P(order), x, XEN_ARG_1, "jn", " an int");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(x), x, XEN_ARG_2, "jn", " a number");
  return(C_TO_XEN_DOUBLE(jn(XEN_TO_C_INT(order), XEN_TO_C_DOUBLE(x))));
}

static XEN g_y0(XEN x)
{
  #define H_y0 "(y0 x) returns the irregular cylindrical bessel function Y0(x)"
  XEN_ASSERT_TYPE(XEN_NUMBER_P(x), x, XEN_ONLY_ARG, "y0", " a number");
  return(C_TO_XEN_DOUBLE(y0(XEN_TO_C_DOUBLE(x))));
}

static XEN g_y1(XEN x)
{
  #define H_y1 "(y1 x) returns the irregular cylindrical bessel function Y1(x)"
  XEN_ASSERT_TYPE(XEN_NUMBER_P(x), x, XEN_ONLY_ARG, "y1", " a number");
  return(C_TO_XEN_DOUBLE(y1(XEN_TO_C_DOUBLE(x))));
}

static XEN g_yn(XEN order, XEN x)
{
  #define H_yn "(yn n x) returns the irregular cylindrical bessel function Yn(x)"
  XEN_ASSERT_TYPE(XEN_INTEGER_P(order), x, XEN_ARG_1, "yn", " an int");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(x), x, XEN_ARG_2, "yn", " a number");
  return(C_TO_XEN_DOUBLE(yn(XEN_TO_C_INT(order), XEN_TO_C_DOUBLE(x))));
}

static XEN g_erf(XEN x)
{
  #define H_erf "(erf x) returns the error function erf(x)"
  XEN_ASSERT_TYPE(XEN_NUMBER_P(x), x, XEN_ONLY_ARG, "erf", " a number");
  return(C_TO_XEN_DOUBLE(erf(XEN_TO_C_DOUBLE(x))));
}

static XEN g_erfc(XEN x)
{
  #define H_erfc "(erfc x) returns the complementary error function erfc(x)"
  XEN_ASSERT_TYPE(XEN_NUMBER_P(x), x, XEN_ONLY_ARG, "erfc", " a number");
  return(C_TO_XEN_DOUBLE(erfc(XEN_TO_C_DOUBLE(x))));
}

static XEN g_lgamma(XEN x)
{
  #define H_lgamma "(lgamma x) returns the log of the gamma function at x"
  XEN_ASSERT_TYPE(XEN_NUMBER_P(x), x, XEN_ONLY_ARG, "lgamma", " a number");
  return(C_TO_XEN_DOUBLE(lgamma(XEN_TO_C_DOUBLE(x))));
}
#endif

static XEN g_i0(XEN x)
{
  #define H_i0 "(i0 x) returns the modified cylindrical bessel function I0(x)"
  XEN_ASSERT_TYPE(XEN_NUMBER_P(x), x, XEN_ONLY_ARG, "i0", " a number");
  return(C_TO_XEN_DOUBLE(mus_bessi0(XEN_TO_C_DOUBLE(x))));
}

#if HAVE_GSL

#include <gsl/gsl_sf_ellint.h>
static XEN g_gsl_ellipk(XEN k)
{
  #define H_gsl_ellipk "(gsl-ellipk k) returns the complete elliptic integral k"
  XEN_ASSERT_TYPE(XEN_NUMBER_P(k), k, XEN_ONLY_ARG, "gsl-ellipk", "a number");
  return(C_TO_XEN_DOUBLE(gsl_sf_ellint_Kcomp(sqrt(XEN_TO_C_DOUBLE(k)), GSL_PREC_APPROX)));
}

#include <gsl/gsl_sf_elljac.h>
static XEN g_gsl_ellipj(XEN u, XEN m)
{
  #define H_gsl_ellipj "(gsl-ellipj u m) returns the Jacobian elliptic functions sn, cn, and dn of u and m"
  double sn = 0.0, cn = 0.0, dn = 0.0;
  XEN_ASSERT_TYPE(XEN_NUMBER_P(u), u, XEN_ARG_1, "gsl-ellipj", "a number");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(m), m, XEN_ARG_2, "gsl-ellipj", "a number");
  gsl_sf_elljac_e(XEN_TO_C_DOUBLE(u),
		  XEN_TO_C_DOUBLE(m),
		  &sn, &cn, &dn);
  return(XEN_LIST_3(C_TO_XEN_DOUBLE(sn),
		    C_TO_XEN_DOUBLE(cn),
		    C_TO_XEN_DOUBLE(dn)));
}

#if MUS_DEBUGGING && HAVE_GUILE
/* use gsl gegenbauer to check our function */
#include <gsl/gsl_sf_gegenbauer.h>
static XEN g_gsl_gegenbauer(XEN n, XEN lambda, XEN x)
{
  gsl_sf_result val;
  gsl_sf_gegenpoly_n_e(XEN_TO_C_INT(n), XEN_TO_C_DOUBLE(lambda), XEN_TO_C_DOUBLE(x), &val);
  return(C_TO_XEN_DOUBLE(val.val));
}
#endif

#include <gsl/gsl_dht.h>
static XEN g_gsl_dht(XEN size, XEN data, XEN nu, XEN xmax)
{
  #define H_gsl_dht "(gsl-dht size data nu xmax) -> Hankel transform of data (a vct)"
  int n;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(size), size, XEN_ARG_1, "gsl-dht", "an integer");
  XEN_ASSERT_TYPE(MUS_VCT_P(data), data, XEN_ARG_2, "gsl-dht", "a vct");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(nu), nu, XEN_ARG_3, "gsl-dht", "a number");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(xmax), xmax, XEN_ARG_4, "gsl-dht", "a number");
  n = XEN_TO_C_INT(size);
  if (n <= 0)
    XEN_OUT_OF_RANGE_ERROR("gsl-dht", XEN_ARG_1, size, "must be > 0");
  else
    {
      double *indata, *outdata;
      int i;
      vct *v;
      gsl_dht *t = gsl_dht_new(n, XEN_TO_C_DOUBLE(nu), XEN_TO_C_DOUBLE(xmax));
      indata = (double *)CALLOC(n, sizeof(double));
      outdata = (double *)CALLOC(n, sizeof(double));
      v = XEN_TO_VCT(data);
      for (i = 0; i < n; i++)
	indata[i] = v->data[i];
      gsl_dht_apply(t, indata, outdata);
      for (i = 0; i < n; i++)
	v->data[i] = outdata[i];
      gsl_dht_free(t);
      FREE(indata);
      FREE(outdata);
    }
  return(data);
}

#if HAVE_COMPLEX_TRIG && (!HAVE_RUBY) && HAVE_SCM_MAKE_COMPLEX
#include <gsl/gsl_poly.h>
#include <complex.h>

static XEN g_gsl_roots(XEN poly)
{
  #define H_gsl_roots "(gsl-roots poly) -> roots of poly"
  int i, n, loc;
  double *p;
  double complex *z;
  gsl_poly_complex_workspace *w;
  XEN result;
  XEN_ASSERT_TYPE(XEN_VECTOR_P(poly), poly, XEN_ONLY_ARG, "gsl-roots", "a vector");
  n = XEN_VECTOR_LENGTH(poly);
  w = gsl_poly_complex_workspace_alloc(n);
  z = (double complex *)calloc(n, sizeof(double complex));
  p = (double *)calloc(n, sizeof(double));
  for (i = 0; i < n; i++)
    p[i] = XEN_TO_C_DOUBLE(XEN_VECTOR_REF(poly, i));
  gsl_poly_complex_solve(p, n, w, (gsl_complex_packed_ptr)z);
  gsl_poly_complex_workspace_free (w);
  result = XEN_MAKE_VECTOR(n - 1, XEN_ZERO);
  loc = snd_protect(result);
  for (i = 0; i < n - 1; i++)
    if (__imag__(z[i]) != 0.0)
      XEN_VECTOR_SET(result, i, C_TO_XEN_COMPLEX(z[i]));
    else XEN_VECTOR_SET(result, i, C_TO_XEN_DOUBLE(__real__(z[i])));
  free(z);
  free(p);
  snd_unprotect_at(loc);
  return(result);
}
#endif
#endif

#if HAVE_GAUCHE
static XEN g_random(XEN val)
{
  if (XEN_INTEGER_P(val))
    return(C_TO_XEN_INT(mus_irandom(XEN_TO_C_INT(val))));
  return(C_TO_XEN_DOUBLE(mus_frandom(XEN_TO_C_DOUBLE(val))));
}
#if HAVE_SYS_TIME_H
  #include <sys/time.h>
#endif

/* this number is overflowing (gauche int = 29 bits) if left as a bare int */
static XEN g_get_internal_real_time(void) {return(C_TO_XEN_INT((int)(100.0 * ((double)clock() / (double)CLOCKS_PER_SEC))));}
#endif

#if HAVE_GUILE
/* libguile/read.c */
static XEN g_skip_block_comment(XEN ch, XEN port)
{
  int bang_seen = 0;
  while (true)
    {
      int c;
      c = scm_getc (port);
      if (c == EOF)
	{
	  snd_warning("unterminated `#| ... |#' comment");
	  return(XEN_FALSE);
	}
      if (c == '|')
	bang_seen = 1;
      else 
	{
	  if ((c == '#') && (bang_seen))
	    return(XEN_FALSE);
	  else bang_seen = 0;
	}
    }
  return(XEN_FALSE);
}
#endif


/* -------- watchers -------- */

#define NOT_A_WATCHER -1
#define INITIAL_WATCHERS_SIZE 8
#define WATCHERS_SIZE_INCREMENT 8
static int *watchers = NULL;
static int watchers_size = 0;

static XEN g_delete_watcher(XEN id)
{
  int w;
  #define H_delete_watcher "(" S_delete_watcher " id) removes the watcher associated with the integer 'id'"
  XEN_ASSERT_TYPE(XEN_INTEGER_P(id), id, XEN_ONLY_ARG, S_delete_watcher, "an integer");

  w = XEN_TO_C_INT(id);
  if ((w >= 0) && 
      (w < watchers_size) &&
      (watchers[w] != NOT_A_WATCHER))
    {
      snd_unprotect_at(watchers[w]);
      watchers[w] = NOT_A_WATCHER;
    }
  return(id);
}

void run_watchers(void)
{
  if (watchers)
    {
      int i;
      for (i = 0; i < watchers_size; i++)
	if (watchers[i] != NOT_A_WATCHER)
	  XEN_CALL_0(snd_protected_at(watchers[i]), S_add_watcher);
    }
}

static XEN g_add_watcher(XEN func)
{
  int i, floc = 0;
  #define H_add_watcher "(" S_add_watcher " func) adds 'func' (a function of no arguments) to the watcher list, and \
returns its id (an integer, used by " S_delete_watcher "). "

  XEN_ASSERT_TYPE(XEN_PROCEDURE_P(func) && XEN_REQUIRED_ARGS_OK(func, 0), func, XEN_ONLY_ARG, S_add_watcher, "a function of no args");

  if (watchers_size == 0)
    {
      watchers_size = INITIAL_WATCHERS_SIZE;
      watchers = (int *)CALLOC(watchers_size, sizeof(int));
      for (i = 0; i < watchers_size; i++) watchers[i] = NOT_A_WATCHER;
    }
  else
    {
      floc = -1;
      for (i = 0; i < watchers_size; i++)
	if (watchers[i] == NOT_A_WATCHER)
	  {
	    floc = i;
	    break;
	  }
      if (floc == -1)
	{
	  floc = watchers_size;
	  watchers_size += WATCHERS_SIZE_INCREMENT;
	  watchers = (int *)REALLOC(watchers, watchers_size * sizeof(int));
	  for (i = floc; i < watchers_size; i++) watchers[i] = NOT_A_WATCHER;
	}
    }

  watchers[floc] = snd_protect(func);
  return(C_TO_XEN_INT(floc));
}

/* TODO: watcher fs / rb  */


/* -------- source file extensions list -------- */

static char **source_file_extensions = NULL;
static int source_file_extensions_size = 0;
static int source_file_extensions_end = 0;
static int default_source_file_extensions = 0;

static void add_source_file_extension(const char *ext)
{
  int i;
  for (i = 0; i < source_file_extensions_end; i++)
    if (strcmp(ext, source_file_extensions[i]) == 0)
      return;
  if (source_file_extensions_end == source_file_extensions_size)
    {
      source_file_extensions_size += 8;
      if (source_file_extensions == NULL)
	source_file_extensions = (char **)CALLOC(source_file_extensions_size, sizeof(char *));
      else source_file_extensions = (char **)REALLOC(source_file_extensions, source_file_extensions_size * sizeof(char *));
    }
  source_file_extensions[source_file_extensions_end] = copy_string(ext);
  source_file_extensions_end++;
}

bool source_file_p(const char *name)
{
  int i, dot_loc = -1, len;
  if (!name) return(false);
  if (source_file_extensions)
    {
      len = strlen(name);
      for (i = 0; i < len; i++)
	if (name[i] == '.')
	  dot_loc = i;
      /* dot_loc is last dot in the name */
      if ((dot_loc > 0) &&
	  (dot_loc < len - 1))
	{
	  const char *ext;
	  ext = (const char *)(name + dot_loc + 1);
	  for (i = 0; i < source_file_extensions_end; i++)
	    if (strcmp(ext, source_file_extensions[i]) == 0)
	      return(true);
	}
    }
  return(false);
}

void save_added_source_file_extensions(FILE *fd)
{
  int i;
  if (source_file_extensions_end > default_source_file_extensions)
    for (i = default_source_file_extensions; i < source_file_extensions_end; i++)
      {
#if HAVE_SCHEME
	fprintf(fd, "(%s \"%s\")\n", S_add_source_file_extension, source_file_extensions[i]);
#endif
#if HAVE_RUBY
	fprintf(fd, "%s(\"%s\")\n", TO_PROC_NAME(S_add_source_file_extension), source_file_extensions[i]);
#endif
#if HAVE_FORTH
	fprintf(fd, "\"%s\" %s drop\n", source_file_extensions[i], S_add_source_file_extension);
#endif
      }
}

static XEN g_add_source_file_extension(XEN ext)
{
  #define H_add_source_file_extension "(" S_add_source_file_extension " ext):  add the file extension 'ext' to the list of source file extensions"
  XEN_ASSERT_TYPE(XEN_STRING_P(ext), ext, XEN_ONLY_ARG, S_add_source_file_extension, "a string");
  add_source_file_extension(XEN_TO_C_STRING(ext));
  return(ext);
}

static char *find_source_file(char *orig)
{
  int i;
  char *str;
  /* orig is a full filename that should be freed before returning */
  for (i = 0; i < source_file_extensions_end; i++)
    {
      str = mus_format("%s.%s", orig, source_file_extensions[i]);
      if (mus_file_probe(str))
	{
	  if (orig) FREE(orig);
	  return(str);
	}
    }
  if (orig) FREE(orig);
  return(NULL);
}



#ifdef XEN_ARGIFY_1
#if HAVE_SCHEME && HAVE_DLFCN_H
  XEN_NARGIFY_1(g_dlopen_w, g_dlopen)
  XEN_NARGIFY_1(g_dlclose_w, g_dlclose)
  XEN_NARGIFY_0(g_dlerror_w, g_dlerror)
  XEN_NARGIFY_2(g_dlinit_w, g_dlinit)
#endif
#if HAVE_SCHEME && (!HAVE_SCM_CONTINUATION_P)
  XEN_NARGIFY_1(g_continuation_p_w, g_continuation_p)
#endif
XEN_NARGIFY_1(g_snd_print_w, g_snd_print)
XEN_NARGIFY_0(g_little_endian_w, g_little_endian)
XEN_NARGIFY_0(g_snd_global_state_w, g_snd_global_state)
XEN_NARGIFY_0(g_mus_audio_describe_w, g_mus_audio_describe)
XEN_NARGIFY_1(g_add_source_file_extension_w, g_add_source_file_extension)

#if MUS_DEBUGGING
  XEN_NARGIFY_1(g_snd_sound_pointer_w, g_snd_sound_pointer)
#endif
#if (!HAVE_GAUCHE)
  XEN_NARGIFY_2(g_fmod_w, g_fmod)
#endif

XEN_NARGIFY_0(g_gc_off_w, g_gc_off)
XEN_NARGIFY_0(g_gc_on_w, g_gc_on)

#if HAVE_SPECIAL_FUNCTIONS
  XEN_NARGIFY_1(g_j0_w, g_j0)
  XEN_NARGIFY_1(g_j1_w, g_j1)
  XEN_NARGIFY_2(g_jn_w, g_jn)
  XEN_NARGIFY_1(g_y0_w, g_y0)
  XEN_NARGIFY_1(g_y1_w, g_y1)
  XEN_NARGIFY_2(g_yn_w, g_yn)
  XEN_NARGIFY_1(g_erf_w, g_erf)
  XEN_NARGIFY_1(g_erfc_w, g_erfc)
  XEN_NARGIFY_1(g_lgamma_w, g_lgamma)
#endif

XEN_NARGIFY_1(g_i0_w, g_i0)

#if HAVE_GSL
  XEN_NARGIFY_1(g_gsl_ellipk_w, g_gsl_ellipk)
  XEN_NARGIFY_2(g_gsl_ellipj_w, g_gsl_ellipj)
  XEN_NARGIFY_4(g_gsl_dht_w, g_gsl_dht)

  #if HAVE_COMPLEX_TRIG && (!HAVE_RUBY) && HAVE_SCM_MAKE_COMPLEX
    XEN_NARGIFY_1(g_gsl_roots_w, g_gsl_roots)
  #endif
#endif

#if HAVE_GAUCHE
  XEN_NARGIFY_1(g_random_w, g_random)
  XEN_NARGIFY_0(g_get_internal_real_time_w, g_get_internal_real_time)
#endif

XEN_NARGIFY_1(g_delete_watcher_w, g_delete_watcher)
XEN_NARGIFY_1(g_add_watcher_w, g_add_watcher)

#else
/* not argify */

#if HAVE_SCHEME && HAVE_DLFCN_H
  #define g_dlopen_w g_dlopen
  #define g_dlclose_w g_dlclose
  #define g_dlerror_w g_dlerror
  #define g_dlinit_w g_dlinit
#endif
#if HAVE_SCHEME && (!HAVE_SCM_CONTINUATION_P)
  #define g_continuation_p_w g_continuation_p
#endif
#define g_snd_print_w g_snd_print
#define g_little_endian_w g_little_endian
#define g_snd_global_state_w g_snd_global_state
#define g_mus_audio_describe_w g_mus_audio_describe
#define g_add_source_file_extension_w g_add_source_file_extension
#if MUS_DEBUGGING
  #define g_snd_sound_pointer_w g_snd_sound_pointer
#endif
#if (!HAVE_GAUCHE)
  #define g_fmod_w g_fmod
#endif
#define g_gc_off_w g_gc_off
#define g_gc_on_w g_gc_on
#if HAVE_SPECIAL_FUNCTIONS
  #define g_j0_w g_j0
  #define g_j1_w g_j1
  #define g_jn_w g_jn
  #define g_y0_w g_y0
  #define g_y1_w g_y1
  #define g_yn_w g_yn
  #define g_erf_w g_erf
  #define g_erfc_w g_erfc
  #define g_lgamma_w g_lgamma
#endif
#define g_i0_w g_i0
#if HAVE_GSL
  #define g_gsl_ellipk_w g_gsl_ellipk
  #define g_gsl_ellipj_w g_gsl_ellipj
  #define g_gsl_dht_w g_gsl_dht
  #if HAVE_COMPLEX_TRIG && (!HAVE_RUBY) && HAVE_SCM_MAKE_COMPLEX
    #define g_gsl_roots_w g_gsl_roots
  #endif
#endif
#define g_delete_watcher_w g_delete_watcher
#define g_add_watcher_w g_add_watcher
#endif


#if HAVE_STATIC_XM
  #if USE_MOTIF
    void Init_libxm(void);
  #else
    void Init_libxg(void);
  #endif
#endif

#if HAVE_GL && (!JUST_GL)
 void Init_libgl(void);
#endif

#if MUS_DEBUGGING && HAVE_SCHEME
void g_init_xmix(void);
#endif

#if HAVE_GUILE
#define S_write_byte "write-byte"

static XEN g_write_byte(XEN byte) /* this collides with CM */
{
  #define H_write_byte "(" S_write_byte " byte) writes byte to the current output port"
  XEN port;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(byte), byte, XEN_ONLY_ARG, S_write_byte, "an integer");
  port = scm_current_output_port();
  scm_putc(XEN_TO_C_INT(byte), SCM_COERCE_OUTPORT(port));
  return(byte);
}
#endif

#if HAVE_GAUCHE
static XEN g_eval_string(XEN str)
{
  char *cstr;
  cstr = XEN_TO_C_STRING(str);
  if (cstr)
    return(XEN_EVAL_C_STRING(cstr));
  return(XEN_FALSE);
}
XEN_NARGIFY_1(g_eval_string_w, g_eval_string)

static XEN g_ftell(XEN fd)
{
  return(C_TO_XEN_OFF_T(lseek(XEN_TO_C_INT(fd), 0, SEEK_CUR)));
}
XEN_NARGIFY_1(g_ftell_w, g_ftell)
#endif


void g_xen_initialize(void)
{
  add_source_file_extension(XEN_FILE_EXTENSION);
#if HAVE_SCHEME
  add_source_file_extension("cl");
  add_source_file_extension("lisp");
#endif
#if HAVE_FORTH
  add_source_file_extension("fth");
  add_source_file_extension("fsm");
#endif
  add_source_file_extension("marks"); /* from save-marks */
  default_source_file_extensions = source_file_extensions_end;

  XEN_DEFINE_PROCEDURE(S_mus_audio_describe, g_mus_audio_describe_w, 0, 0, 0, H_mus_audio_describe);
  XEN_DEFINE_PROCEDURE("snd-global-state", g_snd_global_state_w, 0, 0, 0, "internal testing function");
  XEN_DEFINE_PROCEDURE(S_add_source_file_extension, g_add_source_file_extension_w, 1, 0, 0, H_add_source_file_extension);

#if MUS_DEBUGGING
  XEN_DEFINE_PROCEDURE("snd-sound-pointer", g_snd_sound_pointer_w, 1, 0, 0, "internal testing function");

#if HAVE_GUILE
  XEN_DEFINE_PROCEDURE("snd-stdin-test", g_snd_stdin_test, 1, 0, 0, "internal testing function");
#endif
#endif

  XEN_DEFINE_PROCEDURE(S_gc_off, g_gc_off_w, 0, 0, 0, H_gc_off);
  XEN_DEFINE_PROCEDURE(S_gc_on,  g_gc_on_w,  0, 0, 0, H_gc_on);

#if HAVE_GUILE
  XEN_DEFINE_PROCEDURE(S_write_byte, g_write_byte, 1, 0, 0, H_write_byte);
#endif

#if HAVE_SCHEME && (!HAVE_SCM_CONTINUATION_P)
  XEN_DEFINE_PROCEDURE("continuation?", g_continuation_p_w, 1, 0, 0, "#t if arg is a continuation");
#endif

#if HAVE_GAUCHE
  /* defmacro used in vct init called from sndlib init if with-run */
  XEN_EVAL_C_STRING("(define-syntax defmacro\
                       (syntax-rules ()\
                         ((_ name params . body) (define-macro (name . params) . body))))");
#endif

  Init_sndlib();

#if HAVE_FORTH
  fth_add_loaded_files("sndlib.so");
#endif

#if WITH_MIDI && HAVE_EXTENSION_LANGUAGE
  mus_midi_init();
#endif

  gc_protection = XEN_FALSE;

  XEN_DEFINE_PROCEDURE(S_snd_print,      g_snd_print_w,     1, 0, 0, H_snd_print);
  XEN_DEFINE_PROCEDURE("little-endian?", g_little_endian_w, 0, 0, 0, "return " PROC_TRUE " if host is little endian");

#if (!HAVE_GAUCHE)
  XEN_DEFINE_PROCEDURE("fmod",   g_fmod_w,   2, 0, 0, "C's fmod");
#endif

#if HAVE_SPECIAL_FUNCTIONS
  XEN_DEFINE_PROCEDURE("bes-j0", g_j0_w,     1, 0, 0, H_j0);
  XEN_DEFINE_PROCEDURE("bes-j1", g_j1_w,     1, 0, 0, H_j1);
  XEN_DEFINE_PROCEDURE("bes-jn", g_jn_w,     2, 0, 0, H_jn);
  XEN_DEFINE_PROCEDURE("bes-y0", g_y0_w,     1, 0, 0, H_y0);
  XEN_DEFINE_PROCEDURE("bes-y1", g_y1_w,     1, 0, 0, H_y1);
  XEN_DEFINE_PROCEDURE("bes-yn", g_yn_w,     2, 0, 0, H_yn);
  XEN_DEFINE_PROCEDURE("erf",    g_erf_w,    1, 0, 0, H_erf);
  XEN_DEFINE_PROCEDURE("erfc",   g_erfc_w,   1, 0, 0, H_erfc);
  XEN_DEFINE_PROCEDURE("lgamma", g_lgamma_w, 1, 0, 0, H_lgamma);
#endif
  XEN_DEFINE_PROCEDURE("bes-i0", g_i0_w,     1, 0, 0, H_i0);

#if HAVE_GSL
  XEN_DEFINE_PROCEDURE("gsl-ellipk", g_gsl_ellipk_w, 1, 0, 0, H_gsl_ellipk);
  XEN_DEFINE_PROCEDURE("gsl-ellipj", g_gsl_ellipj_w, 2, 0, 0, H_gsl_ellipj);
  XEN_DEFINE_PROCEDURE("gsl-dht",    g_gsl_dht_w,    4, 0, 0, H_gsl_dht);

#if MUS_DEBUGGING && HAVE_GUILE
  XEN_DEFINE_PROCEDURE("gsl-gegenbauer",  g_gsl_gegenbauer,  3, 0, 0, "internal test func");
#endif

#if HAVE_COMPLEX_TRIG && (!HAVE_RUBY) && HAVE_SCM_MAKE_COMPLEX
  XEN_DEFINE_PROCEDURE("gsl-roots",  g_gsl_roots_w,  1, 0, 0, H_gsl_roots);
#endif

#endif

#if HAVE_GAUCHE
  XEN_DEFINE_CONSTANT("internal-time-units-per-second", 100, "clock speed");
  XEN_DEFINE_PROCEDURE("random",                 g_random_w,                 1, 0, 0, "(random arg) -> random number between 0 and arg ");
  XEN_DEFINE_PROCEDURE("get-internal-real-time", g_get_internal_real_time_w, 0, 0, 0, "get system time");
  XEN_DEFINE_PROCEDURE("ftell",                  g_ftell_w,                  1, 0, 0, "(ftell fd) -> lseek");
  XEN_DEFINE_PROCEDURE("eval-string",            g_eval_string_w,            1, 0, 0, "eval a string");
#endif

  XEN_DEFINE_PROCEDURE(S_delete_watcher, g_delete_watcher_w, 1, 0, 0, H_delete_watcher);
  XEN_DEFINE_PROCEDURE(S_add_watcher,    g_add_watcher_w,    1, 0, 0, H_add_watcher);

#if HAVE_SCHEME
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
#endif

#if HAVE_RUBY
  #define H_print_hook S_print_hook " (text): called each time some Snd-generated response (text) is about to be appended to the listener. \
If it returns some non-false result, Snd assumes you've sent the text out yourself, as well as any needed prompt. \n\
  $print_hook.add-hook!(\"localtime\") do |msg|\n\
    $stdout.print msg\n\
  false\n\
  end"
#endif

#if HAVE_FORTH
  #define H_print_hook S_print_hook " (text): called each time some Snd-generated response (text) is about to be appended to the listener. \
If it returns some non-#f result, Snd assumes you've sent the text out yourself, as well as any needed prompt. \n\
" S_print_hook " lambda: <{ msg }>\n\
  \"%s\n[%s]\n%s\" '( msg date " S_listener_prompt " ) format " S_snd_print "\n\
; add-hook!"
#endif

  print_hook = XEN_DEFINE_HOOK(S_print_hook, 1, H_print_hook);          /* arg = text */

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
  g_init_run();

#if MUS_DEBUGGING && HAVE_SCHEME  && USE_MOTIF
  g_init_xmix();
#endif

#if HAVE_SCHEME && HAVE_DLFCN_H
  XEN_DEFINE_PROCEDURE("dlopen",  g_dlopen_w,  1, 0 ,0, "");
  XEN_DEFINE_PROCEDURE("dlclose", g_dlclose_w, 1, 0 ,0, "");
  XEN_DEFINE_PROCEDURE("dlerror", g_dlerror_w, 0, 0 ,0, "");
  XEN_DEFINE_PROCEDURE("dlinit",  g_dlinit_w,  2, 0 ,0, "");
#endif

#if HAVE_LADSPA && HAVE_EXTENSION_LANGUAGE && HAVE_DLFCN_H && HAVE_DIRENT_H
  g_ladspa_to_snd();
#endif

#ifdef SCRIPTS_DIR
  XEN_ADD_TO_LOAD_PATH(SCRIPTS_DIR);
#endif

  {
    char *pwd;
    pwd = mus_getcwd();
    XEN_ADD_TO_LOAD_PATH(pwd);
    FREE(pwd);
  }

#if HAVE_GUILE || HAVE_GAUCHE
  #if(!defined(M_PI))
    #define M_PI 3.14159265358979323846264338327
  #endif
  XEN_DEFINE("pi", C_TO_XEN_DOUBLE(M_PI)); /* not XEN_DEFINE_CONSTANT which assumes int */
#endif

#if HAVE_GUILE
  {
    /* Gauche and CL use '#| |#' for block comments, so implement them in Guile */
    /*   this is in R6RS, so presumably Guile will eventually implement them itself */
    XEN proc;
    proc = XEN_NEW_PROCEDURE("%skip-comment%", g_skip_block_comment, 2, 0, 0);
    scm_read_hash_extend(C_TO_XEN_CHAR('|'), proc);
  }

  XEN_EVAL_C_STRING("(read-set! keywords 'prefix)");
  XEN_EVAL_C_STRING("(print-enable 'source)");

  XEN_EVAL_C_STRING("(define (clm-print . args) (snd-print (apply format #f args)))");
  XEN_EVAL_C_STRING("(defmacro declare args #f)");     /* for optimizer */
  XEN_EVAL_C_STRING("(define redo-edit redo)");        /* consistency with Ruby */
  XEN_EVAL_C_STRING("(define undo-edit undo)");
  XEN_EVAL_C_STRING("(define define+ define)");        /* Gauche can't handle documentation strings */

  /* from ice-9/r4rs.scm but with output to snd listener */
  XEN_EVAL_C_STRING("(define *snd-loaded-files* '())");
  XEN_EVAL_C_STRING("(define *snd-remember-paths* #t)");
  XEN_EVAL_C_STRING("(set! %load-hook \
                       (lambda (filename)\
                         (if %load-verbosely\
                             (snd-print (format #f \"~%;;; loading ~S\" filename)))\
                         (if (not (member filename *snd-loaded-files*))\
                             (set! *snd-loaded-files* (cons filename *snd-loaded-files*)))\
                         (if *snd-remember-paths*\
                             (let ((curfile (mus-expand-filename filename))\
                                   (last-slash 0))\
                               (do ((i 0 (1+ i)))\
                                   ((= i (string-length curfile)))\
                                 (if (char=? (string-ref curfile i) #\\/)\
	                             (set! last-slash i)))\
                                     (let ((new-path (substring curfile 0 last-slash)))\
                                       (if (and (not (member new-path %load-path))\
                                                (not (string=? (substring curfile (max 0 (- last-slash 5)) last-slash) \"ice-9\")))\
	                                   (set! %load-path (append %load-path (list new-path)))))))))");
  /* the "ice-9" business is to keep us from loading ice-9/debug.scm when we intend our own debug.scm.
   *   load-from-path can still be fooled, but the user will have to work at it.
   *   If you load Guile's debug.scm by mistake (set! %load-verbosely #t) to see Snd's names get clobbered!
   * 
   * one gotcha here is that when an initialization file is loaded, and we're using cons to reset the load-path,
   *   the user's home directory gets placed on the load list, pushing the cwd to second; hence append above
   */
#endif

#if HAVE_GAUCHE
  XEN_EVAL_C_STRING("(define redo-edit redo)");        /* consistency with Ruby */
  XEN_EVAL_C_STRING("(define undo-edit undo)");
  XEN_EVAL_C_STRING("(define *snd-loaded-files* '())");
  XEN_EVAL_C_STRING("(define *snd-remember-paths* #t)");
  XEN_EVAL_C_STRING("(define (clm-print . args) (snd-print (apply format #f args)))"); /* assumes we've loaded format */

  XEN_EVAL_C_STRING("(define load-from-path load)");
  XEN_EVAL_C_STRING("(define system sys-system)");
  XEN_EVAL_C_STRING("(define getenv sys-getenv)");
  XEN_EVAL_C_STRING("(define getcwd sys-getcwd)");
  XEN_EVAL_C_STRING("(define rename-file sys-rename)");
  XEN_EVAL_C_STRING("(define delete-file sys-unlink)");
  XEN_EVAL_C_STRING("(define version gauche-version)");
  XEN_EVAL_C_STRING("(define localtime sys-localtime)");
  XEN_EVAL_C_STRING("(define current-time sys-time)");
  XEN_EVAL_C_STRING("(define strftime sys-strftime)");
  XEN_EVAL_C_STRING("(define shell sys-system)");

  /* add-load-path in Gauche is a spooky special case that is executed no matter what, and
   *   Shiro says I shouldn't use the underlying function %add-load-path; here is his suggestion:
   */
  XEN_EVAL_C_STRING("(define-macro (add-to-load-path path) (when (not (member path *load-path*)) `(add-load-path ,path)))");

  XEN_EVAL_C_STRING("(define (list-set! lis pos val)\
                       (set-car! (list-tail lis pos) val)\
                       val)");

  XEN_EVAL_C_STRING("(define (make-procedure-with-setter get set)\
                       (let ((proc (lambda x (apply get x))))\
                         (set! (setter proc) set)\
                         proc))");

  XEN_EVAL_C_STRING("(define (throw . args) (raise args))");
  XEN_EVAL_C_STRING("(define (catch tag body error-handler)                                                         \
                       (guard (err                                                                                  \
 	                        ((and (condition-has-type? err <error>)                                             \
		                      (or (equal? tag #t)                                                           \
		                          (member tag (list 'wrong-number-of-args 'wrong-type-arg 'out-of-range)))) \
	                         (let ((msg (slot-ref err 'message))                                                \
		                       (translated-error (list err)))                                               \
	                           (if (string? msg)                                                                \
		                       (let ((len (string-length msg)))                                             \
		                         (if (and (> len 26)                                                        \
			                          (string=? (substring msg 0 25) \"wrong number of arguments\"))    \
		                             (set! translated-error (list 'wrong-number-of-args msg))               \
		                             (if (and (> len 21)                                                    \
				                      (string=? (substring msg 0 21) \"argument out of range\"))    \
			                         (set! translated-error (list 'out-of-range msg))))))               \
	                           (apply error-handler translated-error)))                                         \
	                         ((or (equal? tag #t)                                                               \
	                              (and (list? err)                                                              \
		                           (eq? tag (car err))))                                                    \
	                          (apply error-handler (if (list? err) err (list err)))))                           \
	                 (body)))");

  XEN_EVAL_C_STRING("(define (symbol->keyword key) (make-keyword (symbol->string key)))");
  XEN_EVAL_C_STRING("(define (keyword->symbol key) (string->symbol (keyword->string key)))");
  XEN_EVAL_C_STRING("(define (1- val) (- val 1))");
  XEN_EVAL_C_STRING("(define (1+ val) (+ val 1))");
  
  /* these are for compatibility with Guile (rather than add hundreds of "if provided?" checks) */
  XEN_EVAL_C_STRING("(defmacro use-modules (arg . args) #f)");
  XEN_EVAL_C_STRING("(define (debug-enable . args) #f)");
  XEN_EVAL_C_STRING("(define (read-enable . args) #f)");
  XEN_EVAL_C_STRING("(define (debug-set! . args) #f)");
  XEN_EVAL_C_STRING("(define (make-soft-port . args) #f)");
  XEN_EVAL_C_STRING("(defmacro declare args #f)");     /* for optimizer */
  XEN_EVAL_C_STRING("(define procedure-source procedure-info)");
  XEN_EVAL_C_STRING("(define procedure-with-setter? has-setter?)");
  XEN_EVAL_C_STRING("(define (set-procedure-property! . args) #f)");

  /* Gauche doesn't handle documentation strings correctly */
  XEN_EVAL_C_STRING("(defmacro define+ (args . body) `(define ,args ,@(cdr body)))"); /* strip out documentation string if embedded defines */
#endif

#if 0
#if HAVE_GUILE || HAVE_GAUCHE
  /* R6RS change: define ->inexact and ->exact if not already defined */
  if (!(XEN_DEFINED_P("->exact")))
    {
      XEN_EVAL_C_STRING("(define ->inexact exact->inexact)");
      XEN_EVAL_C_STRING("(define ->exact inexact->exact)");
    }
#endif
#endif

#if HAVE_RUBY
  XEN_EVAL_C_STRING("def clm_print(str, *args)\n\
                       snd_print format(str, *args)\n\
                       end");
#endif

#if HAVE_FORTH
  XEN_EVAL_C_STRING(": clm-print ( fmt lst -- ) string-format snd-print drop ;");
  XEN_EVAL_C_STRING("' redo alias redo-edit");        /* consistency with Ruby */
  XEN_EVAL_C_STRING("' undo alias undo-edit");
#endif

#if HAVE_STATIC_XM
  #if USE_MOTIF
    Init_libxm();
#if HAVE_FORTH
    fth_add_loaded_files("libxm.so");
#endif
  #else
    Init_libxg();
#if HAVE_FORTH
    fth_add_loaded_files("libxg.so");
#endif
  #endif
#endif

#if (HAVE_GL) && (!JUST_GL)
  Init_libgl();
#endif

#if MUS_DEBUGGING
  XEN_YES_WE_HAVE("snd-debug");
#endif

#if HAVE_ALSA
  XEN_YES_WE_HAVE("alsa");
#endif

#if HAVE_GSL
  XEN_YES_WE_HAVE("gsl");
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

#if HAVE_GAUCHE
  XEN_YES_WE_HAVE("snd-gauche");
#endif

#if HAVE_FORTH
  XEN_YES_WE_HAVE("snd-forth");
#endif

#if HAVE_RUBY
  XEN_YES_WE_HAVE("snd-ruby");
  /* we need to set up the search path so that load and require will work as in the program Ruby */
  #ifdef RUBY_SEARCH_PATH
    {
      /* this code stolen from ruby.c */
      char *str, *buf;
      int i, j = 0, len;
      str = RUBY_SEARCH_PATH;
      len = snd_strlen(str);
      buf = (char *)CALLOC(len + 1, sizeof(char));
      for (i = 0; i < len; i++)
	if (str[i] == ':')
	  {
	    buf[j] = 0;
	    if (j > 0)
	      {
		XEN_ADD_TO_LOAD_PATH(buf);
	      }
	    j = 0;
	  }
	else buf[j++] = str[i];
      if (j > 0)
	{
	  buf[j] = 0;
	  XEN_ADD_TO_LOAD_PATH(buf);
	}
      FREE(buf);
    }
  #endif
#endif

  XEN_YES_WE_HAVE("snd");

#if WITH_MODULES
  scm_c_use_module("snd sndlib");
  scm_c_use_module("snd clm");
#endif
}
