#if (defined(__GNUC__))
#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif
#include <string.h>
#endif

#include "snd.h"
#include "clm2xen.h"

#define HAVE_SPECIAL_FUNCTIONS (!_MSC_VER)

/* Snd defines its own exit and delay
 *
 *   In Ruby, rand is kernel_rand.
 *
 *   In Forth, Snd's exit is named snd-exit.
 */


/* -------- protect XEN vars from GC -------- */

#if HAVE_SCHEME

int snd_protect(XEN obj) {return(s7_gc_protect(s7, obj));}
void snd_unprotect_at(int loc) {s7_gc_unprotect_at(s7, loc);}

#else
static XEN gc_protection;
static int gc_protection_size = 0;
#define DEFAULT_GC_VALUE XEN_UNDEFINED
static int gc_last_cleared = NOT_A_GC_LOC;
static int gc_last_set = NOT_A_GC_LOC;

int snd_protect(XEN obj)
{
  int i, old_size;
  XEN tmp;
  
  if (gc_protection_size == 0)
    {
      gc_protection_size = 512;
      gc_protection = XEN_MAKE_VECTOR(gc_protection_size, DEFAULT_GC_VALUE);
      XEN_PROTECT_FROM_GC(gc_protection);
      XEN_VECTOR_SET(gc_protection, 0, obj);
      gc_last_set = 0;
    }
  else
    {
      if ((gc_last_cleared >= 0) && 
	  XEN_EQ_P(XEN_VECTOR_REF(gc_protection, gc_last_cleared), DEFAULT_GC_VALUE))
	{
	  /* we hit this branch about 2/3 of the time */
	  XEN_VECTOR_SET(gc_protection, gc_last_cleared, obj);
	  gc_last_set = gc_last_cleared;
	  gc_last_cleared = NOT_A_GC_LOC;

	  return(gc_last_set);
	}

      for (i = gc_last_set; i < gc_protection_size; i++)
	if (XEN_EQ_P(XEN_VECTOR_REF(gc_protection, i), DEFAULT_GC_VALUE))
	  {
	    XEN_VECTOR_SET(gc_protection, i, obj);
	    gc_last_set = i;
	    
	    return(gc_last_set);
	  }

      for (i = 0; i < gc_last_set; i++)
	if (XEN_EQ_P(XEN_VECTOR_REF(gc_protection, i), DEFAULT_GC_VALUE))
	  {
	    /* here we average 3 checks before a hit, so this isn't as bad as it looks */
	    XEN_VECTOR_SET(gc_protection, i, obj);
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

      /*   in Ruby, I think we can unprotect it */
#if HAVE_RUBY || HAVE_FORTH
      XEN_UNPROTECT_FROM_GC(tmp);
#endif
      gc_last_set = old_size;
    }
  return(gc_last_set);
}


void snd_unprotect_at(int loc)
{
  if (loc >= 0)
    {
      XEN_VECTOR_SET(gc_protection, loc, DEFAULT_GC_VALUE);
      gc_last_cleared = loc;
    }
}
#endif


/* -------- error handling -------- */

static char *last_file_loaded = NULL;

#if HAVE_SCHEME
static XEN g_snd_s7_error_handler(XEN args)
{
  s7_pointer msg;
  msg = s7_car(args);
  XEN_ASSERT_TYPE(XEN_STRING_P(msg), msg, 1, "_snd_s7_error_handler_", "a string");

  if (ss->xen_error_handler)
    (*(ss->xen_error_handler))(s7_string(msg), (void *)any_selected_sound()); /* not NULL! */
  return(s7_f(s7));
}
#endif


void redirect_xen_error_to(void (*handler)(const char *msg, void *ufd), void *data)
{
  ss->xen_error_handler = handler;
  ss->xen_error_data = data;

#if HAVE_SCHEME
  if (handler == NULL)
    s7_eval_c_string(s7, "(set! (hook-functions *error-hook*) ())");
  else s7_eval_c_string(s7, "(set! (hook-functions *error-hook*) (list               \n\
                               (lambda (hook)                                        \n\
                                 (let ((args (hook 'data)))                          \n\
                                 (_snd_s7_error_handler_                             \n\
                                   (string-append                                    \n\
                                     (if (string? args)                              \n\
                                         args                                        \n\
                                         (if (pair? args)                            \n\
                                             (apply format #f args)                  \n\
                                             \"\"))                                  \n\
                                     (with-environment (error-environment)           \n\
                                       (if (and error-code                           \n\
                                                (string? error-file)                 \n\
                                                (number? error-line))                \n\
                                           (format #f \"~%~S[~D]: ~A~%\" error-file error-line error-code) \n\
                                           \"\"))))))))");
#endif
}


static void redirect_snd_print_to(void (*handler)(const char *msg, void *ufd), void *data)
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



/* ---------------- RUBY error handler ---------------- */ 

#if HAVE_RUBY 
static XEN snd_format_if_needed(XEN args) 
{ 
  /* if car has formatting info, use next arg as arg list for it */ 
  XEN format_args = XEN_EMPTY_LIST, cur_arg, result; 
  int i, start = 0, num_args, format_info_len, err_size = 8192; 
  bool got_tilde = false, was_formatted = false; 
  char *format_info = NULL, *errmsg = NULL; 

  num_args = XEN_LIST_LENGTH(args); 
  if (num_args == 1) return(XEN_CAR(args)); 

  format_info = mus_strdup(XEN_TO_C_STRING(XEN_CAR(args))); 
  format_info_len = mus_strlen(format_info); 

  if (XEN_CONS_P(XEN_CADR(args))) 
    format_args = XEN_COPY_ARG(XEN_CADR(args)); /* protect Ruby case */ 
  else format_args = XEN_CDR(args); 

  errmsg = (char *)calloc(err_size, sizeof(char)); 

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
         case '~': errmsg = mus_strcat(errmsg, "~", &err_size); break; 
         case '%': errmsg = mus_strcat(errmsg, "\n", &err_size); break; 
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
                errmsg = mus_strcat(errmsg, vstr, &err_size); 
                free(vstr); 
              } 
               else 
              { 
                char *temp = NULL; 
                errmsg = mus_strcat(errmsg, temp = (char *)XEN_AS_STRING(cur_arg), &err_size); 
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
  if (format_info) free(format_info); 
  if (!was_formatted) 
    { 
      char *temp = NULL; 
      errmsg = mus_strcat(errmsg, " ", &err_size); 
      errmsg = mus_strcat(errmsg, temp = (char *)XEN_AS_STRING(XEN_CADR(args)), &err_size); 

      if (num_args > 2) 
    { 
      if (!XEN_FALSE_P(XEN_CADDR(args))) start = 2; else start = 3; 
      for (i = start; i < num_args; i++) 
        { 
          char *temp = NULL; 
          errmsg = mus_strcat(errmsg, " ", &err_size); 
          errmsg = mus_strcat(errmsg, temp = (char *)XEN_AS_STRING(XEN_LIST_REF(args, i)), &err_size); 
        } 
    } 
    } 
  result = C_TO_XEN_STRING(errmsg); 
  free(errmsg); 
  return(result); 
} 

void snd_rb_raise(XEN tag, XEN throw_args) 
{ 
  static char *msg = NULL; 
  XEN err = rb_eStandardError, bt; 
  int size = 2048; 
  char *idname; 

  if (msg) free(msg); 
  msg = (char *)calloc(size, sizeof(char)); 

  idname = (char *)rb_id2name(tag); 
  if (strcmp(idname, "Out_of_range") == 0) 
    err = rb_eRangeError; 
  else 
    if (strcmp(idname, "Wrong_type_arg") == 0) 
      err = rb_eTypeError; 

  msg = mus_strcat(msg, idname, &size); 
  if (strcmp(idname, "Mus_error") == 0) 
    msg = mus_strcat(msg, ": ", &size); 
  else msg = mus_strcat(msg, " in ", &size); 
  msg = mus_strcat(msg, XEN_TO_C_STRING(snd_format_if_needed(throw_args)), &size); 

  bt = rb_funcall(err, rb_intern("caller"), 0); 

  if (XEN_VECTOR_LENGTH(bt) > 0) 
    { 
      int i; 
      msg = mus_strcat(msg, "\n", &size); 
      for (i = 0; i < XEN_VECTOR_LENGTH(bt); i++) 
    { 
      msg = mus_strcat(msg, XEN_TO_C_STRING(XEN_VECTOR_REF(bt, i)), &size); 
      msg = mus_strcat(msg, "\n", &size); 
    } 
    } 

  if (strcmp(idname, "Snd_error") != 0) 
    { 
      if (!(run_snd_error_hook(msg))) 
    { 
      if (ss->xen_error_handler) 
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
    } 
    } 

  rb_raise(err, "%s", msg); 
} 
#endif 
/* end HAVE_RUBY */ 



#if HAVE_EXTENSION_LANGUAGE

XEN snd_catch_any(XEN_CATCH_BODY_TYPE body, void *body_data, const char *caller)
{
  return((*body)(body_data));
}

#else

/* no extension language but user managed to try to evaluate something
 *   can this happen?
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

#if HAVE_SCHEME
  {
    int oargs, restargs, gc_loc;

    gc_loc = s7_gc_protect(s7, arity);
    rargs = XEN_TO_C_INT(XEN_CAR(arity));
    oargs = XEN_TO_C_INT(XEN_CADR(arity));
    restargs = ((XEN_TRUE_P(XEN_CADDR(arity))) ? 1 : 0);
    s7_gc_unprotect_at(s7, gc_loc);

    if (rargs > args) return(false);
    if ((restargs == 0) && ((rargs + oargs) < args)) return(false);
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
	{
	  char *temp = NULL, *str;
	  str = mus_format("%s: %s (%s arg %d) is not a procedure!", 
			   temp = (char *)XEN_AS_STRING(proc),
			   arg_name, caller, argn);
#if HAVE_SCHEME
	  if (temp) free(temp);
#endif
	  return(str);
	}
    }
  else
    {
      arity = XEN_ARITY(proc);

#if HAVE_RUBY
      rargs = XEN_TO_C_INT(arity);
      if (!xen_rb_arity_ok(rargs, args))
 	return(mus_format("%s function (%s arg %d) should take %d args, not %d",
 			  arg_name, caller, argn, args, (rargs < 0) ? (-rargs) : rargs));
#endif

#if HAVE_SCHEME
      {
	int oargs, restargs;
	int loc;

	loc = snd_protect(arity);
	rargs = XEN_TO_C_INT(XEN_CAR(arity));
	oargs = XEN_TO_C_INT(XEN_CADR(arity));
	restargs = ((XEN_TRUE_P(XEN_CADDR(arity))) ? 1 : 0);
	snd_unprotect_at(loc);

	if (rargs > args)
	  return(mus_format("%s function (%s arg %d) should take %d argument%s, but instead requires %d",
			    arg_name, caller, argn, args, (args != 1) ? "s" : "", rargs));

	if ((restargs == 0) && ((rargs + oargs) < args))
	  return(mus_format("%s function (%s arg %d) should accept at least %d argument%s, but instead accepts only %d",
			    arg_name, caller, argn, args, (args != 1) ? "s" : "", rargs + oargs));

	if ((args == 0) &&
	    ((rargs != 0) || (oargs != 0) || (restargs != 0)))
	  return(mus_format("%s function (%s arg %d) should take no args, not %d", 
			    arg_name, caller, argn, rargs + oargs + restargs));
      }
#endif

#if HAVE_FORTH
      rargs = XEN_TO_C_INT(arity);
      if (rargs != args)
	return(mus_format("%s function (%s arg %d) should take %d args, not %d",
			  arg_name, caller, argn, args, rargs));
#endif
    }
  return(NULL);
}


XEN snd_no_such_file_error(const char *caller, XEN filename)
{
  XEN_ERROR(NO_SUCH_FILE,
	    XEN_LIST_4(C_TO_XEN_STRING("no-such-file: ~A ~S: ~A"),
		       C_TO_XEN_STRING(caller),
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
  else
    {
      if (XEN_SOUND_P(snd))
	index = XEN_SOUND_TO_C_INT(snd);
    }

  if ((index >= 0) &&
      (index < ss->max_sounds) && 
      (snd_ok(ss->sounds[index]))) /* good grief... */
    {
      sp = ss->sounds[index];
      XEN_ERROR(NO_SUCH_CHANNEL,
		XEN_LIST_6(C_TO_XEN_STRING("no-such-channel: (~A: sound: ~A, chan: ~A) (~S, chans: ~A))"),
			   C_TO_XEN_STRING(caller),
			   snd, 
			   chn, 
			   C_TO_XEN_STRING(sp->short_filename), 
			   C_TO_XEN_INT(sp->nchans)));
    }
  XEN_ERROR(NO_SUCH_CHANNEL,
	    XEN_LIST_4(C_TO_XEN_STRING("no-such-channel: (~A: sound: ~A, chan: ~A)"),
		       C_TO_XEN_STRING(caller),
		       snd,
		       chn));
  return(XEN_FALSE);
}


XEN snd_no_active_selection_error(const char *caller)
{
  XEN_ERROR(XEN_ERROR_TYPE("no-active-selection"),
	    XEN_LIST_2(C_TO_XEN_STRING("~A: no active selection"),
		       C_TO_XEN_STRING(caller)));
  return(XEN_FALSE);
}


XEN snd_bad_arity_error(const char *caller, XEN errstr, XEN proc)
{
  XEN_ERROR(XEN_ERROR_TYPE("bad-arity"),
            XEN_LIST_3(C_TO_XEN_STRING("~A,~A"),
		       C_TO_XEN_STRING(caller),
                       errstr));
  return(XEN_FALSE);
}



/* -------- various evaluators (within our error handler) -------- */

XEN eval_str_wrapper(void *data)
{
  return(XEN_EVAL_C_STRING((char *)data));
}


static XEN eval_file_wrapper(void *data)
{
  XEN error;
  last_file_loaded = (char *)data;
  error = XEN_LOAD_FILE((char *)data); /* error only meaningful in Ruby */
  last_file_loaded = NULL;
  return(error);
}


static char *g_print_1(XEN obj) /* free return val */
{
#if HAVE_SCHEME
  return(XEN_AS_STRING(obj)); 
#endif

#if HAVE_FORTH || HAVE_RUBY
  return(mus_strdup(XEN_AS_STRING(obj))); 
#endif

#if (!HAVE_EXTENSION_LANGUAGE)
  return(NULL);
#endif
}


static char *gl_print(XEN result)
{
  char *newbuf = NULL, *str = NULL;
  int i, ilen, savelen;

#if HAVE_SCHEME
  /* expand \t first (neither gtk nor motif handles this automatically)
   *   but... "#\\t" is the character t not a tab indication!
   *   (object->string #\t) or worse #\tab
   */
  #define TAB_SPACES 4
  int tabs = 0, len, j = 0;

  newbuf = g_print_1(result);
  len = mus_strlen(newbuf);

  for (i = 0; i < len - 1; i++)
    if (((i == 0) || ((newbuf[i - 1] != '\\') && (newbuf[i - 1] != '#'))) &&
	(newbuf[i] == '\\') && 
	(newbuf[i + 1] == 't'))
      tabs++;

  if (tabs == 0)
    return(newbuf);

  ilen = len + tabs * TAB_SPACES;
  str = (char *)calloc(ilen, sizeof(char));

  for (i = 0; i < len - 1; i++)
    {
      if (((i == 0) || (newbuf[i - 1] != '\\')) && 
	  (newbuf[i] == '\\') && 
	  (newbuf[i + 1] == 't'))
	{
	  int k;
	  for (k = 0; k < TAB_SPACES; k++)
	    str[j + k] = ' ';
	  j += TAB_SPACES;
	  i++;
	}
      else str[j++] = newbuf[i];
    }
  str[j] = newbuf[len - 1];

  free(newbuf);
  return(str);
#endif

  /* specialize vectors which can be enormous in this context */
  if ((!(XEN_VECTOR_P(result))) || 
      ((int)(XEN_VECTOR_LENGTH(result)) <= print_length(ss)))
    return(g_print_1(result));

  ilen = print_length(ss); 
  newbuf = (char *)calloc(128, sizeof(char));
  savelen = 128;

#if HAVE_FORTH
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
	      newbuf = mus_strcat(newbuf, ",", &savelen);
#endif
	      newbuf = mus_strcat(newbuf, " ", &savelen); 
	    }
	  newbuf = mus_strcat(newbuf, str, &savelen);
	  free(str);
	}
    }

#if HAVE_FORTH
  newbuf = mus_strcat(newbuf, " ...)", &savelen);
#endif

#if HAVE_RUBY
  newbuf = mus_strcat(newbuf, " ...]", &savelen);
#endif

  return(newbuf);
}


void snd_display_result(const char *str, const char *endstr)
{
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
      if (endstr) listener_append(endstr);
      listener_append_and_prompt(str);
    }
}


void snd_report_result(XEN result, const char *buf)
{
  char *str = NULL;
  str = gl_print(result);
  snd_display_result(str, buf);
  if (str) free(str);
}


void snd_report_listener_result(XEN form)
{
  snd_report_result(form, "\n");
}


static char *stdin_str = NULL;

void clear_stdin(void)
{
  if (stdin_str) free(stdin_str);
  stdin_str = NULL;
}


#if HAVE_SCHEME
static int check_balance(const char *expr, int start, int end, bool in_listener) 
{
  int i;
  bool non_whitespace_p = false;
  int paren_count = 0;
  bool prev_separator = true;
  bool quote_wait = false;

  i = start;
  while (i < end) 
    {
      switch (expr[i]) 
	{
	case ';' :
	  /* skip till newline. */
	  do {
	    i++;
	  } while ((expr[i] != '\n') && (i < end));
	  break;

	case ' ':
	case '\n':
	case '\t':
	case '\r':
	  if ((non_whitespace_p) && (paren_count == 0) && (!quote_wait))
	    return(i);
	  else 
	    {
	      prev_separator = true;
	      i++;
	    }
	  break;

	case '\"' :
	  if ((non_whitespace_p) && (paren_count == 0) && (!quote_wait))
	    return(i);
	  else 
	    {
	      /* skip past ", ignoring \", some cases:
	       *  "\"\"" '("\"\"") "\\" "#\\(" "'(\"#\\\")"
	       */
	      while (i < end)
		{
		  i++;
		  if (expr[i] == '\\') 
		    i++;
		  else
		    {
		      if (expr[i] == '\"')
			break;
		    }
		}
	      i++;
	      if (paren_count == 0) 
		{
		  if (i < end) 
		    return(i);
		  else return(0);
		} 
	      else 
		{
		  prev_separator = true;
		  non_whitespace_p = true;
		  quote_wait = false;
		}
	    }
	  break;

	case '#':
	  if ((i < end - 1) &&
	      (expr[i + 1] == '|'))
	    {
	      /* (+ #| a comment |# 2 1) */
	      i++;
	      do {
		i++;
	      } while (((expr[i] != '|') || (expr[i + 1] != '#')) && (i < end));
	      i++;
	      break;
	    }
	  else
	    {
	      /* (set! *#readers* (cons (cons #\c (lambda (str) (apply make-rectangular (read)))) *#readers*))
	       */
	      if ((non_whitespace_p) && (paren_count == 0) && (!quote_wait))
		return(i);
	      else 
		{
		  bool found_it = false;
		  if (prev_separator)
		    {
		      int k, incr = 0;
		      for (k = i + 1; k < end; k++)
			{
			  if (expr[k] == '(')
			    {
			      /* should we look at the readers here? I want to support #c(1 2) for example */
			      non_whitespace_p = false;
			      prev_separator = false;
			      incr = k - i;
			      break;
			    }
			  else
			    {
			      if ((!isdigit(expr[k])) && /* #2d(...)? */
				  (!isalpha(expr[k])) && /* #c(1 2)? */
				  (expr[k] != 'D') && 
				  (expr[k] != 'd') &&
				  (expr[k] != '=') &&   /* what is this for? */
				  (expr[k] != '#'))     /* perhaps #1d(#(1 2) 3) ? */
				break;
			    }
			}
		      if (incr > 0)
			{
			  i += incr;
			  found_it = true;
			}
		    }
		  if (!found_it)
		    {
		      if ((i + 2 < end) && (expr[i + 1] == '\\') && 
			  ((expr[i + 2] == ')') || (expr[i + 2] == ';') || (expr[i + 2] == '\"') || (expr[i + 2] == '(')))
			i += 3;
		      else
			{
			  prev_separator = false;
			  quote_wait = false;
			  non_whitespace_p = true;
			  i++;
			}
		    }
		}
	    }
	  break;

	case '(' :
	  if ((non_whitespace_p) && (paren_count == 0) && (!quote_wait))
	    return(i - 1); /* 'a(...) -- ignore the (...) */
	  else 
	    {
	      i++;
	      paren_count++;
	      non_whitespace_p = true;
	      prev_separator = true;
	      quote_wait = false;
	    }
	  break;

	case ')' :
	  paren_count--;
	  if ((non_whitespace_p) && (paren_count == 0))
	    return(i + 1);
	  else 
	    {
	      i++;
	      non_whitespace_p = true;
	      prev_separator = true;
	      quote_wait = false;
	    }
	  break;

	case '\'' :
	case '`' :                  /* `(1 2) */
	  if (prev_separator) 
	    quote_wait = true;
	  non_whitespace_p = true;
	  i++;
	  break;

	case ',':                   /* `,(+ 1 2) */
	case '@':                   /* `,@(list 1 2) */
	  prev_separator = false;
	  non_whitespace_p = true;
	  i++;
	  break;

	default:
	  prev_separator = false;
	  quote_wait = false;
	  non_whitespace_p = true;
	  i++;
	  break;
	}
    }

  return(0);
}
#endif


static char *stdin_check_for_full_expression(const char *newstr)
{
#if HAVE_SCHEME
  int end_of_text;
#endif
  if (stdin_str)
    {
      char *str;
      str = stdin_str;
      stdin_str = (char *)calloc(mus_strlen(str) + mus_strlen(newstr) + 2, sizeof(char));
      strcat(stdin_str, str);
      strcat(stdin_str, newstr);
      free(str);
    }
  else stdin_str = mus_strdup(newstr);
#if HAVE_SCHEME
  end_of_text = check_balance(stdin_str, 0, mus_strlen(stdin_str), false); /* last-arg->not in listener */
  if (end_of_text > 0)
    {
      if (end_of_text + 1 < mus_strlen(stdin_str))
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


void snd_eval_stdin_str(const char *buf)
{
  /* we may get incomplete expressions here */
  /*   (Ilisp always sends a complete expression, but it may be broken into two or more pieces from read's point of view) */

  char *str = NULL;
  if (mus_strlen(buf) == 0) return;

  str = stdin_check_for_full_expression(buf);
  if (str)
    {
      XEN result;
      int loc;

      redirect_everything_to(string_to_stdout, NULL);
      result = snd_catch_any(eval_str_wrapper, (void *)str, str);
      redirect_everything_to(NULL, NULL);

      loc = snd_protect(result);
      if (stdin_str) free(stdin_str);
      /* same as str here */
      stdin_str = NULL;
      str = gl_print(result);
      string_to_stdout(str, NULL);

      if (str) free(str);
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
	  free(temp);
	}
      else ss->startup_errors = mus_strdup(msg); /* initial prompt is already there */
    }
}


static bool snd_load_init_file_1(const char *filename)
{
  char *expr, *fullname;
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
      snd_catch_any(eval_file_wrapper, (void *)fullname, expr);
      free(expr);
    }

  if (fullname) free(fullname);
  return(happy);
}


void snd_load_init_file(bool no_global, bool no_init)
{
  /* look for ".snd" on the home directory; return true if an error occurred (to try to get that info to the user's attention) */
  /* called only in snd-g|xmain.c at initialization time */

  /* changed Oct-05 because the Scheme/Ruby/Forth choices are becoming a hassle --
   *   now save-options has its own file ~/.snd_prefs_ruby|forth|s7 which is loaded first, if present
   *     then ~/.snd_ruby|forth|s7, if present
   *     then ~/.snd for backwards compatibility
   * snd_options does not write ~/.snd anymore, but overwrites the .snd_prefs_* file
   * use set init files only change the ~/.snd choice
   *
   * there are parallel choices for the global configuration file: /etc/snd_ruby|forth|s7.conf
   */

#if HAVE_EXTENSION_LANGUAGE
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

#if HAVE_SCHEME
  #define SND_EXT_CONF "/etc/snd_s7.conf"
  #define SND_PREFS "~/.snd_prefs_s7"
  #define SND_INIT "~/.snd_s7"
#endif

#define SND_INIT_FILE_ENVIRONMENT_NAME "SND_INIT_FILE"
#if (defined(_MSC_VER) || __CYGWIN__)
  #define INIT_FILE_NAME "snd-init"
#else
  #define INIT_FILE_NAME "~/.snd"
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


static char *find_source_file(const char *orig);

void snd_load_file(const char *filename)
{
  char *str = NULL, *str2 = NULL;

  str = mus_expand_filename(filename);
  if (!(mus_file_probe(str)))
    {
      char *temp;
      temp = find_source_file(str); 
      free(str);
      str = temp;
    }
  if (!str)
    {
      snd_error("can't load %s: %s", filename, snd_open_strerror());
      return;
    }

  str2 = mus_format("(load \"%s\")", filename);   /* currently unused in Forth and Ruby */
  snd_catch_any(eval_file_wrapper, (void *)str, str2);
  if (str) free(str);
  if (str2) free(str2);
}


static XEN g_snd_print(XEN msg)
{
  #define H_snd_print "(" S_snd_print " str): display str in the listener window"
  char *str = NULL;

  if (XEN_STRING_P(msg))
    str = mus_strdup(XEN_TO_C_STRING(msg));
  else
    {
      if (XEN_CHAR_P(msg))
	{
	  str = (char *)calloc(2, sizeof(char));
	  str[0] = XEN_TO_C_CHAR(msg);
	}
      else str = gl_print(msg);
    }

  if (str)
    {
#if USE_GTK
      if (ss->listener)
#endif
      listener_append(str);
      free(str);
    }
  /* used to check for event in Motif case, but that is very dangerous -- check for infinite loop C-c needs to be somewhere else */
  return(msg);
}


void check_features_list(const char *features)
{
  /* check for list of features, report any missing, exit (for compsnd) */
  /*  this can't be in snd.c because we haven't fully initialized the extension language and so on at that point */
  if (!features) return;

#if HAVE_SCHEME
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

#if HAVE_FORTH
  XEN_EVAL_C_STRING(mus_format("'( %s ) [each] dup \
                                          provided? [if] \
                                            drop \
                                          [else] \
                                            1 >list \"\\nno %%s!\\n\\n\" swap format .stderr \
                                          [then] \
                                        [end-each]\n", 
			       features)); 
#endif
  snd_exit(0);
}


mus_float_t string_to_mus_float_t(const char *str, mus_float_t lo, const char *field_name)
{
#if HAVE_EXTENSION_LANGUAGE
  XEN res;
  mus_float_t f;
  res = snd_catch_any(eval_str_wrapper, (void *)str, "string->float");
  if (XEN_NUMBER_P(res))
    {
      f = XEN_TO_C_DOUBLE(res);
      if (f < lo)
	snd_error("%s: %.3f is invalid", field_name, f);
      else return(f);
    }
  else snd_error("%s is not a number", str);
  return(0.0);
#else
  mus_float_t res = 0.0;
  if (str) 
    {
      if (!(sscanf(str, "%f", &res)))
	snd_error("%s is not a number", str);
      else
	{
	  if (res < lo)
	    snd_error("%s: %.3f is invalid", field_name, res);
	}
    }
  return(res);
#endif
}


int string_to_int(const char *str, int lo, const char *field_name) 
{
#if HAVE_EXTENSION_LANGUAGE
  XEN res;
  res = snd_catch_any(eval_str_wrapper, (void *)str, "string->int");
  if (XEN_NUMBER_P(res))
    {
      int val;
      val = XEN_TO_C_INT(res);
      if (val < lo)
	snd_error("%s: %d is invalid", field_name, val);
      else return(val);
    }
  else snd_error("%s: %s is not a number", field_name, str);
  return(0);
#else
  int res = 0;
  if (str) 
    {
      if (!(sscanf(str, "%d", &res)))
	snd_error("%s: %s is not a number", field_name, str);
      else
	{
	  if (res < lo)
	    snd_error("%s: %d is invalid", field_name, res);
	}
    }
  return(res);
#endif
}


mus_long_t string_to_mus_long_t(const char *str, mus_long_t lo, const char *field_name)
{
#if HAVE_EXTENSION_LANGUAGE
  XEN res;

  res = snd_catch_any(eval_str_wrapper, (void *)str, "string->mus_long_t");
  if (XEN_NUMBER_P(res))
    {
      mus_long_t val;
      val = XEN_TO_C_LONG_LONG(res);
      if (val < lo)
	snd_error("%s: %lld is invalid", field_name, val);
      else return(val);
    }
  else snd_error("%s: %s is not a number", field_name, str);
  return(0);
#else
  mus_long_t res = 0;
  if (str) 
    {
      if (!(sscanf(str, "%lld" , &res)))
	snd_error("%s: %s is not a number", field_name, str);
      else
	{
	  if (res < lo)
	    snd_error("%s: %lld is invalid", field_name, res);
	}
    }
  return(res);
#endif
}


XEN run_progn_hook(XEN hook, XEN args, const char *caller)
{
#if HAVE_SCHEME
  return(s7_call(s7, hook, args));
#else
  XEN result = XEN_FALSE;
  XEN procs = XEN_HOOK_PROCEDURES(hook);

  while (XEN_NOT_NULL_P(procs))
    {
      result = XEN_APPLY(XEN_CAR(procs), args, caller);
      procs = XEN_CDR(procs);
    }

  return(result);
#endif
}


XEN run_hook(XEN hook, XEN args, const char *caller)
{
#if HAVE_SCHEME
  return(s7_call(s7, hook, args));
#else
  XEN procs = XEN_HOOK_PROCEDURES(hook);

  while (XEN_NOT_NULL_P(procs))
    {
      if (!(XEN_EQ_P(args, XEN_EMPTY_LIST)))
	XEN_APPLY(XEN_CAR(procs), args, caller);
      else XEN_CALL_0(XEN_CAR(procs), caller);
      procs = XEN_CDR (procs);
    }

  return(XEN_FALSE);
#endif
}


XEN run_or_hook(XEN hook, XEN args, const char *caller)
{
#if HAVE_SCHEME
  return(s7_call(s7, hook, args));
#else
  XEN result = XEN_FALSE; /* (or): #f */
  XEN hook_result = XEN_FALSE;
  XEN procs = XEN_HOOK_PROCEDURES(hook);

  while (XEN_NOT_NULL_P(procs))
    {
      if (!(XEN_EQ_P(args, XEN_EMPTY_LIST)))
	result = XEN_APPLY(XEN_CAR(procs), args, caller);
      else result = XEN_CALL_0(XEN_CAR(procs), caller);
      if (XEN_NOT_FALSE_P(result)) 
        hook_result = result;
      procs = XEN_CDR (procs);
    }

  return(hook_result);
#endif
}



#if HAVE_SCHEME && (!_MSC_VER)
#include <dlfcn.h>
/* these are included because libtool's dlopen is incredibly stupid */

/* apparently netBSD does not have dlerror? 
    #ifdef __NetBSD__
      #define dlerror() g_strerror(errno)
    #endif

    to get symbols from current program: handle = dlopen(NULL, RTLD_GLOBAL | RTLD_LAZY);
 */

static XEN g_dlopen(XEN name, XEN flags)
{
  #define H_dlopen "(dlopen lib (flags RTLD_LAZY)) loads the dynamic library 'lib' and returns a handle for it (for dlinit and dlclose)"
  void *handle;
  const char *cname;
  XEN_ASSERT_TYPE(XEN_STRING_P(name), name, 1, "dlopen", "a string (filename)");
  cname = XEN_TO_C_STRING(name);
  if (cname)
    {
      handle = dlopen(cname, RTLD_LAZY);
      if (handle == NULL)
	{
	  char *longname;

	  longname = mus_expand_filename(cname);
	  if (XEN_INTEGER_P(flags))
	    handle = dlopen(longname, XEN_TO_C_INT(flags));
	  else handle = dlopen(longname, RTLD_LAZY);
	  free(longname);

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
  #define H_dlclose "(dlclose handle) may close the library referred to by 'handle'."
  XEN_ASSERT_TYPE(XEN_WRAPPED_C_POINTER_P(handle), handle, 1, "dlclose", "a library handle");
  return(C_TO_XEN_INT(dlclose((void *)(XEN_UNWRAP_C_POINTER(handle)))));
}


static XEN g_dlerror(void)
{
  #define H_dlerror "(dlerror) returns a string describing the last dlopen/dlinit/dlclose error"
  return(C_TO_XEN_STRING(dlerror()));
}


static XEN g_dlsym(XEN handle, XEN func)
{
  #define H_dlsym "(dlsym library function-name) returns a pointer to function in library, or #f."
  void *proc;

  XEN_ASSERT_TYPE(XEN_WRAPPED_C_POINTER_P(handle), handle, 1, "dlsym", "a library handle");
  XEN_ASSERT_TYPE(XEN_STRING_P(func), func, 2, "dlsym", "a string (function name)");

  proc = dlsym((void *)(XEN_UNWRAP_C_POINTER(handle)), XEN_TO_C_STRING(func));
  if (proc == NULL) return(XEN_FALSE);
  return(XEN_WRAP_C_POINTER(func));
}


static XEN g_dlinit(XEN handle, XEN func)
{
  #define H_dlinit "(dlinit handle func) calls 'func' from the library referred to by 'handle'."
  typedef void *(*snd_dl_func)(void);
  void *proc;

  XEN_ASSERT_TYPE(XEN_WRAPPED_C_POINTER_P(handle), handle, 1, "dlinit", "a library handle");
  XEN_ASSERT_TYPE(XEN_STRING_P(func), func, 2, "dlinit", "a string (init func name)");

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


#if (!HAVE_SCHEME)
/* fmod is the same as modulo in s7:
   (do ((i 0 (+ i 1))) 
       ((= i 100)) 
     (let ((val1 (- (random 1.0) 2.0)) 
           (val2 (- (random 1.0) 2.0)))
       (let ((f (fmod val1 val2)) 
             (m (modulo val1 val2))) 
         (if (> (abs (- f m)) 1e-9) 
             (format *stderr* "~A ~A -> ~A ~A~%" val1 val2 f m)))))
*/

static XEN g_fmod(XEN a, XEN b)
{
  double val, x, y;
  XEN_ASSERT_TYPE(XEN_NUMBER_P(a), a, 1, "fmod", " a number");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(b), b, 2, "fmod", " a number");
  x = XEN_TO_C_DOUBLE(a);
  y = XEN_TO_C_DOUBLE(b);
  val = fmod(x, y);
  if (((y > 0.0) && (val < 0.0)) ||
      ((y < 0.0) && (val > 0.0)))
    return(C_TO_XEN_DOUBLE(val + y));
  return(C_TO_XEN_DOUBLE(val));
}
#endif


#if HAVE_SPECIAL_FUNCTIONS || HAVE_GSL
#define S_bes_j0 "bes-j0"
#define S_bes_j1 "bes-j1"
#define S_bes_jn "bes-jn"
#define S_bes_y0 "bes-y0"
#define S_bes_y1 "bes-y1"
#define S_bes_yn "bes-yn"
#endif


#if HAVE_SCHEME && WITH_GMP && HAVE_SPECIAL_FUNCTIONS

#include <gmp.h>
#include <mpfr.h>
#include <mpc.h>

static XEN big_math_1(XEN x, 
		      int (*mpfr_math)(mpfr_ptr, mpfr_srcptr, mpfr_rnd_t))
{
  s7_pointer val;
  mpfr_t y;
  mpfr_init_set(y, *s7_big_real(x), GMP_RNDN);
  mpfr_math(y, y, GMP_RNDN);
  val = s7_make_big_real(s7, &y);
  mpfr_clear(y);
  return(val);
}


static XEN big_j0(XEN x) {return(big_math_1(x, mpfr_j0));}
static XEN big_j1(XEN x) {return(big_math_1(x, mpfr_j1));}
static XEN big_y0(XEN x) {return(big_math_1(x, mpfr_y0));}
static XEN big_y1(XEN x) {return(big_math_1(x, mpfr_y1));}

static XEN big_erf(XEN x) {return(big_math_1(x, mpfr_erf));}
static XEN big_erfc(XEN x) {return(big_math_1(x, mpfr_erfc));}


static XEN big_math_2(XEN n, XEN x, 
		      int (*mpfr_math)(mpfr_ptr, long, mpfr_srcptr, mpfr_rnd_t))
{
  s7_pointer val;
  mpfr_t y;
  mpfr_init_set(y, *s7_big_real(x), GMP_RNDN);
  mpfr_math(y, XEN_TO_C_INT(n), y, GMP_RNDN);
  val = s7_make_big_real(s7, &y);
  mpfr_clear(y);
  return(val);
}


static XEN big_jn(XEN n, XEN x) {return(big_math_2(n, x, mpfr_jn));}
static XEN big_yn(XEN n, XEN x) {return(big_math_2(n, x, mpfr_yn));}


/* bes-i0 from G&R 8.447, 8.451, A&S 9.6.12, 9.7.1, arprec bessel.cpp */

static XEN big_i0(XEN ux)
{
  int k;
  mpfr_t sum, x, x1, x2, eps;
  mpfr_init_set_ui(sum, 0, GMP_RNDN);
  mpfr_init_set(x, *s7_big_real(ux), GMP_RNDN);
  mpfr_init_set_ui(sum, 1, GMP_RNDN);
  mpfr_init_set_ui(x1, 1, GMP_RNDN);
  mpfr_init_set_ui(eps, 2, GMP_RNDN);
  mpfr_pow_si(eps, eps, -mpfr_get_default_prec(), GMP_RNDN);
  mpfr_init_set_ui(x2, mpfr_get_default_prec(), GMP_RNDN);
  mpfr_div_ui(x2, x2, 2, GMP_RNDN);
  if (mpfr_cmpabs(x, x2) < 0)
    {
      mpfr_mul(x, x, x, GMP_RNDN);           /* x = ux^2 */
      for (k = 1; k < 10000; k++)
	{
	  mpfr_set_ui(x2, k, GMP_RNDN);      /* x2 = k */
	  mpfr_mul(x2, x2, x2, GMP_RNDN);    /* x2 = k^2 */
	  mpfr_div(x1, x1, x2, GMP_RNDN);    /* x1 = x1/x2 */
	  mpfr_mul(x1, x1, x, GMP_RNDN);     /* x1 = x1*x */
	  mpfr_div_ui(x1, x1, 4, GMP_RNDN);  /* x1 = x1/4 */
	  if (mpfr_cmp(x1, eps) < 0)
	    break;
	  mpfr_add(sum, sum, x1, GMP_RNDN);  /* sum += x1 */
	}
      /* takes usually ca 10 to 40 iterations */
    }
  else
    {
      mpfr_t den, num;
      mpfr_init(den);
      mpfr_init(num);
      mpfr_abs(x, x, GMP_RNDN);
      for (k = 1; k < 10000; k++)
	{
	  mpfr_set(x2, x1, GMP_RNDN);
	  mpfr_set_ui(den, k, GMP_RNDN);
	  mpfr_mul_ui(den, den, 8, GMP_RNDN);
	  mpfr_mul(den, den, x, GMP_RNDN);
	  mpfr_set_ui(num, k, GMP_RNDN);
	  mpfr_mul_ui(num, num, 2, GMP_RNDN);
	  mpfr_sub_ui(num, num, 1, GMP_RNDN);
	  mpfr_mul(num, num, num, GMP_RNDN);
	  mpfr_div(num, num, den, GMP_RNDN);
	  mpfr_mul(x1, x1, num, GMP_RNDN);
	  mpfr_add(sum, sum, x1, GMP_RNDN);  
	  if (mpfr_cmp(x1, eps) < 0)
	    {
	      mpfr_const_pi(x2, GMP_RNDN);
	      mpfr_mul_ui(x2, x2, 2, GMP_RNDN);
	      mpfr_mul(x2, x2, x, GMP_RNDN);
	      mpfr_sqrt(x2, x2, GMP_RNDN);           /* sqrt(2*pi*x) */
	      mpfr_div(sum, sum, x2, GMP_RNDN);
	      mpfr_exp(x1, x, GMP_RNDN);
	      mpfr_mul(sum, sum, x1, GMP_RNDN);      /* sum * e^x / sqrt(2*pi*x) */
	      break;
	    }
	  if (mpfr_cmp(x1, x2) > 0)
	    {
	      fprintf(stderr, "bes-i0 has screwed up");
	      break;
	    }
	}
      mpfr_clear(den);
      mpfr_clear(num);
    }
  mpfr_clear(x1);
  mpfr_clear(x2);
  mpfr_clear(x);
  mpfr_clear(eps);
  return(s7_make_big_real(s7, &sum));
}


/* fft
 *     (define hi (make-vector 8))
 *     (define ho (make-vector 8))
 *     (do ((i 0 (+ i 1))) ((= i 8)) (vector-set! hi i (bignum "0.0")) (vector-set! ho i (bignum "0.0")))
 *     (vector-set! ho 1 (bignum "-1.0"))
 *     (vector-set! ho 1 (bignum "-1.0"))
 *     (bignum-fft hi ho 8)
 *
 * this is tricky -- perhaps a bad idea.  vector elements are changed in place which means
 *   they better be unique!  and there are no checks that each element actually is a bignum
 *   which means we'll segfault if a normal real leaks through.
 *
 * bignum_fft is say 200 times slower than the same size fftw call, and takes more space than
 *   I can account for: 2^20 29 secs ~.5 Gb, 2^24 11 mins ~5Gb.  I think there should be
 *   the vector element (8), the mpfr_t space (16 or 32), the s7_cell (28 or 32), and the value pointer (8),
 *   and the heap pointer loc (8) so 2^24 should be (* 2 (expt 2 24) (+ 8 8 8 8 32 32)) = 3 Gb, not 5.  2^25 25 min 10.6?
 *   I think the extra is in the free space in the heap -- it can be adding 1/4 of the total.
 */

static s7_pointer bignum_fft(s7_scheme *sc, s7_pointer args)
{
  #define H_bignum_fft "(bignum-fft rl im n (sign 1)) performs a multiprecision fft on the vectors of bigfloats rl and im"

  int n, sign = 1;
  s7_pointer *rl, *im;

  int m, j, mh, ldm, lg, i, i2, j2, imh;
  mpfr_t ur, ui, u, vr, vi, angle, c, s, temp;

  #define big_rl(n) (*(s7_big_real(rl[n])))
  #define big_im(n) (*(s7_big_real(im[n])))

  n = s7_integer(s7_list_ref(sc, args, 2));
  if (s7_list_length(sc, args) > 3)
    sign = s7_integer(s7_list_ref(sc, args, 3));

  rl = s7_vector_elements(s7_list_ref(sc, args, 0));
  im = s7_vector_elements(s7_list_ref(sc, args, 1));

  /* scramble(rl, im, n); */
  {
    int i, m, j;
    s7_pointer vr, vi;
    j = 0;
    for (i = 0; i < n; i++)
      {
	if (j > i)
	  {
	    vr = rl[j];
	    vi = im[j];
	    rl[j] = rl[i];
	    im[j] = im[i];
	    rl[i] = vr;
	    im[i] = vi;
	  }
	m = n >> 1;
	while ((m >= 2) && (j >= m))
	  {
	    j -= m;
	    m = m >> 1;
	  }
	j += m;
      }
  }

  imh = (int)(log(n + 1) / log(2.0));
  m = 2;
  ldm = 1;
  mh = n >> 1;

  mpfr_init(angle);                        /* angle = (M_PI * sign) */
  mpfr_const_pi(angle, GMP_RNDN);
  if (sign == -1)
    mpfr_neg(angle, angle, GMP_RNDN);

  mpfr_init(c);
  mpfr_init(s);
  mpfr_init(ur);
  mpfr_init(ui);
  mpfr_init(u);
  mpfr_init(vr);
  mpfr_init(vi);
  mpfr_init(temp);

  for (lg = 0; lg < imh; lg++)
    {
      mpfr_cos(c, angle, GMP_RNDN);         /* c = cos(angle) */
      mpfr_sin(s, angle, GMP_RNDN);         /* s = sin(angle) */
      mpfr_set_ui(ur, 1, GMP_RNDN);         /* ur = 1.0 */
      mpfr_set_ui(ui, 0, GMP_RNDN);         /* ui = 0.0 */
      for (i2 = 0; i2 < ldm; i2++)
	{
	  i = i2;
	  j = i2 + ldm;
	  for (j2 = 0; j2 < mh; j2++)
	    {
	      mpfr_set(temp, big_im(j), GMP_RNDN);          /* vr = ur * rl[j] - ui * im[j] */
	      mpfr_mul(temp, temp, ui, GMP_RNDN);
	      mpfr_set(vr, big_rl(j), GMP_RNDN);
	      mpfr_mul(vr, vr, ur, GMP_RNDN);
	      mpfr_sub(vr, vr, temp, GMP_RNDN);
	      
	      mpfr_set(temp, big_rl(j), GMP_RNDN);          /* vi = ur * im[j] + ui * rl[j] */
	      mpfr_mul(temp, temp, ui, GMP_RNDN);
	      mpfr_set(vi, big_im(j), GMP_RNDN);
	      mpfr_mul(vi, vi, ur, GMP_RNDN);
	      mpfr_add(vi, vi, temp, GMP_RNDN);
	      
	      mpfr_set(big_rl(j), big_rl(i), GMP_RNDN);     /* rl[j] = rl[i] - vr */
	      mpfr_sub(big_rl(j), big_rl(j), vr, GMP_RNDN);

	      mpfr_set(big_im(j), big_im(i), GMP_RNDN);     /* im[j] = im[i] - vi */
	      mpfr_sub(big_im(j), big_im(j), vi, GMP_RNDN);
	      
	      mpfr_add(big_rl(i), big_rl(i), vr, GMP_RNDN); /* rl[i] += vr */
	      mpfr_add(big_im(i), big_im(i), vi, GMP_RNDN); /* im[i] += vi */
	      
	      i += m;
	      j += m;
	    }

	  mpfr_set(u, ur, GMP_RNDN);             /* u = ur */
	  mpfr_set(temp, ui, GMP_RNDN);          /* ur = (ur * c) - (ui * s) */
	  mpfr_mul(temp, temp, s, GMP_RNDN);
	  mpfr_mul(ur, ur, c, GMP_RNDN);
	  mpfr_sub(ur, ur, temp, GMP_RNDN);
	  
	  mpfr_set(temp, u, GMP_RNDN);           /* ui = (ui * c) + (u * s) */
	  mpfr_mul(temp, temp, s, GMP_RNDN);
	  mpfr_mul(ui, ui, c, GMP_RNDN);
	  mpfr_add(ui, ui, temp, GMP_RNDN);
	}
      mh >>= 1;
      ldm = m;

      mpfr_div_ui(angle, angle, 2, GMP_RNDN);   /* angle *= 0.5 */
      m <<= 1;
    }
  return(s7_f(sc));
}

#endif


#if HAVE_SPECIAL_FUNCTIONS && (!HAVE_GSL)
static XEN g_j0(XEN x)
{
  #define H_j0 "(" S_bes_j0 " x): returns the regular cylindrical bessel function value J0(x)"
#if (!HAVE_SCHEME)
  XEN_ASSERT_TYPE(XEN_NUMBER_P(x), x, 1, S_bes_j0, " a number");
#endif

#if HAVE_SCHEME && WITH_GMP
  if ((s7_is_bignum(x)) &&
      (s7_is_real(x)) &&
      (!(s7_is_rational(x))))
    return(big_j0(x));
#endif
  return(C_TO_XEN_DOUBLE(j0(XEN_TO_C_DOUBLE(x))));
}


static XEN g_j1(XEN x)
{
  #define H_j1 "(" S_bes_j1 " x): returns the regular cylindrical bessel function value J1(x)"
#if (!HAVE_SCHEME)
  XEN_ASSERT_TYPE(XEN_NUMBER_P(x), x, 1, S_bes_j1, " a number");
#endif

#if HAVE_SCHEME && WITH_GMP
  if ((s7_is_bignum(x)) &&
      (s7_is_real(x)) &&
      (!(s7_is_rational(x))))
    return(big_j1(x));
#endif
  return(C_TO_XEN_DOUBLE(j1(XEN_TO_C_DOUBLE(x))));
}


static XEN g_jn(XEN order, XEN x)
{
  #define H_jn "(" S_bes_jn " n x): returns the regular cylindrical bessel function value Jn(x)"
  XEN_ASSERT_TYPE(XEN_INTEGER_P(order), x, 1, S_bes_jn, " an int");
#if (!HAVE_SCHEME)
  XEN_ASSERT_TYPE(XEN_NUMBER_P(x), x, 2, S_bes_jn, " a number");
#endif

#if HAVE_SCHEME && WITH_GMP
  if ((s7_is_bignum(x)) &&
      (s7_is_real(x)) &&
      (!(s7_is_rational(x))))
    return(big_jn(order, x));
#endif
  return(C_TO_XEN_DOUBLE(jn(XEN_TO_C_INT(order), XEN_TO_C_DOUBLE(x))));
}


static XEN g_y0(XEN x)
{
  #define H_y0 "(" S_bes_y0 " x): returns the irregular cylindrical bessel function value Y0(x)"
  XEN_ASSERT_TYPE(XEN_NUMBER_P(x), x, 1, S_bes_y0, " a number");
#if HAVE_SCHEME && WITH_GMP
  if ((s7_is_bignum(x)) &&
      (s7_is_real(x)) &&
      (!(s7_is_rational(x))))
    return(big_y0(x));
#endif
  return(C_TO_XEN_DOUBLE(y0(XEN_TO_C_DOUBLE(x))));
}


static XEN g_y1(XEN x)
{
  #define H_y1 "(" S_bes_y1 " x): returns the irregular cylindrical bessel function value Y1(x)"
  XEN_ASSERT_TYPE(XEN_NUMBER_P(x), x, 1, S_bes_y1, " a number");
#if HAVE_SCHEME && WITH_GMP
  if ((s7_is_bignum(x)) &&
      (s7_is_real(x)) &&
      (!(s7_is_rational(x))))
    return(big_y1(x));
#endif
  return(C_TO_XEN_DOUBLE(y1(XEN_TO_C_DOUBLE(x))));
}


static XEN g_yn(XEN order, XEN x)
{
  #define H_yn "(" S_bes_yn " n x): returns the irregular cylindrical bessel function value Yn(x)"
  XEN_ASSERT_TYPE(XEN_INTEGER_P(order), x, 1, S_bes_yn, " an int");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(x), x, 2, S_bes_yn, " a number");
#if HAVE_SCHEME && WITH_GMP
  if ((s7_is_bignum(x)) &&
      (s7_is_real(x)) &&
      (!(s7_is_rational(x))))
    return(big_yn(order, x));
#endif
  return(C_TO_XEN_DOUBLE(yn(XEN_TO_C_INT(order), XEN_TO_C_DOUBLE(x))));
}


static XEN g_erf(XEN x)
{
  #define H_erf "(erf x): returns the error function erf(x)"
  XEN_ASSERT_TYPE(XEN_NUMBER_P(x), x, 1, "erf", " a number");
#if HAVE_SCHEME && WITH_GMP
  if ((s7_is_bignum(x)) &&
      (s7_is_real(x)) &&
      (!(s7_is_rational(x))))
    return(big_erf(x));
#endif
  return(C_TO_XEN_DOUBLE(erf(XEN_TO_C_DOUBLE(x))));
}


static XEN g_erfc(XEN x)
{
  #define H_erfc "(erfc x): returns the complementary error function erfc(x)"
  XEN_ASSERT_TYPE(XEN_NUMBER_P(x), x, 1, "erfc", " a number");
#if HAVE_SCHEME && WITH_GMP
  if ((s7_is_bignum(x)) &&
      (s7_is_real(x)) &&
      (!(s7_is_rational(x))))
    return(big_erfc(x));
#endif
  return(C_TO_XEN_DOUBLE(erfc(XEN_TO_C_DOUBLE(x))));
}


static XEN g_lgamma(XEN x)
{
  #define H_lgamma "(lgamma x): returns the log of the gamma function at x"
  XEN_ASSERT_TYPE(XEN_NUMBER_P(x), x, 1, "lgamma", " a number");
  return(C_TO_XEN_DOUBLE(lgamma(XEN_TO_C_DOUBLE(x))));
}
#endif


#define S_bes_i0 "bes-i0"

static XEN g_i0(XEN x)
{
  #define H_i0 "(" S_bes_i0 " x): returns the modified cylindrical bessel function value I0(x)"
  XEN_ASSERT_TYPE(XEN_NUMBER_P(x), x, 1, S_bes_i0, " a number");
#if HAVE_SCHEME && WITH_GMP
  if ((s7_is_bignum(x)) &&
      (s7_is_real(x)) &&
      (!(s7_is_rational(x))))
    return(big_i0(x));
#endif
  return(C_TO_XEN_DOUBLE(mus_bessi0(XEN_TO_C_DOUBLE(x)))); /* uses GSL if possible */
}


/* ---------------------------------------- use GSL ---------------------------------------- */
#if HAVE_GSL

/* include all the bessel functions, etc */
#include <gsl/gsl_sf_bessel.h>

static XEN g_j0(XEN x)
{
  #define H_j0 "(" S_bes_j0 " x): returns the regular cylindrical bessel function value J0(x)"
  XEN_ASSERT_TYPE(XEN_NUMBER_P(x), x, 1, S_bes_j0, " a number");

#if HAVE_SCHEME && WITH_GMP
  if ((s7_is_bignum(x)) &&
      (s7_is_real(x)) &&
      (!(s7_is_rational(x))))
    return(big_j0(x));
#endif
  return(C_TO_XEN_DOUBLE(gsl_sf_bessel_J0(XEN_TO_C_DOUBLE(x))));
}


static XEN g_j1(XEN x)
{
  #define H_j1 "(" S_bes_j1 " x): returns the regular cylindrical bessel function value J1(x)"
  XEN_ASSERT_TYPE(XEN_NUMBER_P(x), x, 1, S_bes_j1, " a number");

#if HAVE_SCHEME && WITH_GMP
  if ((s7_is_bignum(x)) &&
      (s7_is_real(x)) &&
      (!(s7_is_rational(x))))
    return(big_j1(x));
#endif
  return(C_TO_XEN_DOUBLE(gsl_sf_bessel_J1(XEN_TO_C_DOUBLE(x))));
}


static XEN g_jn(XEN order, XEN x)
{
  #define H_jn "(" S_bes_jn " n x): returns the regular cylindrical bessel function value Jn(x)"
  XEN_ASSERT_TYPE(XEN_INTEGER_P(order), x, 1, S_bes_jn, " an int");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(x), x, 2, S_bes_jn, " a number");

#if HAVE_SCHEME && WITH_GMP
  if ((s7_is_bignum(x)) &&
      (s7_is_real(x)) &&
      (!(s7_is_rational(x))))
    return(big_jn(order, x));
#endif
  return(C_TO_XEN_DOUBLE(gsl_sf_bessel_Jn(XEN_TO_C_INT(order), XEN_TO_C_DOUBLE(x))));
}


static XEN g_y0(XEN x)
{
  #define H_y0 "(" S_bes_y0 " x): returns the irregular cylindrical bessel function value Y0(x)"
  XEN_ASSERT_TYPE(XEN_NUMBER_P(x), x, 1, S_bes_y0, " a number");
#if HAVE_SCHEME && WITH_GMP
  if ((s7_is_bignum(x)) &&
      (s7_is_real(x)) &&
      (!(s7_is_rational(x))))
    return(big_y0(x));
#endif
  return(C_TO_XEN_DOUBLE(gsl_sf_bessel_Y0(XEN_TO_C_DOUBLE(x))));
}


static XEN g_y1(XEN x)
{
  #define H_y1 "(" S_bes_y1 " x): returns the irregular cylindrical bessel function value Y1(x)"
  XEN_ASSERT_TYPE(XEN_NUMBER_P(x), x, 1, S_bes_y1, " a number");
#if HAVE_SCHEME && WITH_GMP
  if ((s7_is_bignum(x)) &&
      (s7_is_real(x)) &&
      (!(s7_is_rational(x))))
    return(big_y1(x));
#endif
  return(C_TO_XEN_DOUBLE(gsl_sf_bessel_Y1(XEN_TO_C_DOUBLE(x))));
}


static XEN g_yn(XEN order, XEN x)
{
  #define H_yn "(" S_bes_yn " n x): returns the irregular cylindrical bessel function value Yn(x)"
  XEN_ASSERT_TYPE(XEN_INTEGER_P(order), x, 1, S_bes_yn, " an int");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(x), x, 2, S_bes_yn, " a number");
#if HAVE_SCHEME && WITH_GMP
  if ((s7_is_bignum(x)) &&
      (s7_is_real(x)) &&
      (!(s7_is_rational(x))))
    return(big_yn(order, x));
#endif
  return(C_TO_XEN_DOUBLE(gsl_sf_bessel_Yn(XEN_TO_C_INT(order), XEN_TO_C_DOUBLE(x))));
}

#define S_bes_i1 "bes-i1"
#define S_bes_in "bes-in"
#define S_bes_k0 "bes-k0"
#define S_bes_k1 "bes-k1"
#define S_bes_kn "bes-kn"

static XEN g_i1(XEN x)
{
  #define H_i1 "(" S_bes_i1 " x): returns the regular cylindrical bessel function value I1(x)"
  XEN_ASSERT_TYPE(XEN_NUMBER_P(x), x, 1, S_bes_i1, " a number");
  return(C_TO_XEN_DOUBLE(gsl_sf_bessel_I1(XEN_TO_C_DOUBLE(x))));
}


static XEN g_in(XEN order, XEN x)
{
  #define H_in "(" S_bes_in " n x): returns the regular cylindrical bessel function value In(x)"
  XEN_ASSERT_TYPE(XEN_INTEGER_P(order), x, 1, S_bes_in, " an int");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(x), x, 2, S_bes_in, " a number");
  return(C_TO_XEN_DOUBLE(gsl_sf_bessel_In(XEN_TO_C_INT(order), XEN_TO_C_DOUBLE(x))));
}


static XEN g_k0(XEN x)
{
  #define H_k0 "(" S_bes_k0 " x): returns the irregular cylindrical bessel function value K0(x)"
  XEN_ASSERT_TYPE(XEN_NUMBER_P(x), x, 1, S_bes_k0, " a number");
  return(C_TO_XEN_DOUBLE(gsl_sf_bessel_K0(XEN_TO_C_DOUBLE(x))));
}


static XEN g_k1(XEN x)
{
  #define H_k1 "(" S_bes_k1 " x): returns the irregular cylindrical bessel function value K1(x)"
  XEN_ASSERT_TYPE(XEN_NUMBER_P(x), x, 1, S_bes_k1, " a number");
  return(C_TO_XEN_DOUBLE(gsl_sf_bessel_K1(XEN_TO_C_DOUBLE(x))));
}


static XEN g_kn(XEN order, XEN x)
{
  #define H_kn "(" S_bes_kn " n x): returns the irregular cylindrical bessel function value Kn(x)"
  XEN_ASSERT_TYPE(XEN_INTEGER_P(order), x, 1, S_bes_kn, " an int");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(x), x, 2, S_bes_kn, " a number");
  return(C_TO_XEN_DOUBLE(gsl_sf_bessel_Kn(XEN_TO_C_INT(order), XEN_TO_C_DOUBLE(x))));
}


#include <gsl/gsl_sf_erf.h>
static XEN g_erf(XEN x)
{
  #define H_erf "(erf x): returns the error function erf(x)"
  XEN_ASSERT_TYPE(XEN_NUMBER_P(x), x, 1, "erf", " a number");
#if HAVE_SCHEME && WITH_GMP
  if ((s7_is_bignum(x)) &&
      (s7_is_real(x)) &&
      (!(s7_is_rational(x))))
    return(big_erf(x));
#endif
  return(C_TO_XEN_DOUBLE(gsl_sf_erf(XEN_TO_C_DOUBLE(x))));
}


static XEN g_erfc(XEN x)
{
  #define H_erfc "(erfc x): returns the complementary error function value erfc(x)"
  XEN_ASSERT_TYPE(XEN_NUMBER_P(x), x, 1, "erfc", " a number");
#if HAVE_SCHEME && WITH_GMP
  if ((s7_is_bignum(x)) &&
      (s7_is_real(x)) &&
      (!(s7_is_rational(x))))
    return(big_erfc(x));
#endif
  return(C_TO_XEN_DOUBLE(gsl_sf_erfc(XEN_TO_C_DOUBLE(x))));
}


#include <gsl/gsl_sf_gamma.h>
static XEN g_lgamma(XEN x)
{
  #define H_lgamma "(lgamma x): returns the log of the gamma function at x"
  XEN_ASSERT_TYPE(XEN_NUMBER_P(x), x, 1, "lgamma", " a number");
  return(C_TO_XEN_DOUBLE(gsl_sf_lngamma(XEN_TO_C_DOUBLE(x))));
}



#include <gsl/gsl_sf_ellint.h>
static XEN g_gsl_ellipk(XEN k)
{
  double f;
  #define H_gsl_ellipk "(gsl-ellipk k): returns the complete elliptic integral k"
  XEN_ASSERT_TYPE(XEN_NUMBER_P(k), k, 1, "gsl-ellipk", "a number");
  f = XEN_TO_C_DOUBLE(k);
  XEN_ASSERT_TYPE(f >= 0.0, k, 1, "gsl-ellipk", "a non-negative number");
  return(C_TO_XEN_DOUBLE(gsl_sf_ellint_Kcomp(sqrt(XEN_TO_C_DOUBLE(k)), GSL_PREC_APPROX)));
}


#include <gsl/gsl_sf_elljac.h>
static XEN g_gsl_ellipj(XEN u, XEN m)
{
  #define H_gsl_ellipj "(gsl-ellipj u m): returns the Jacobian elliptic functions sn, cn, and dn of u and m"
  double sn = 0.0, cn = 0.0, dn = 0.0;
  XEN_ASSERT_TYPE(XEN_NUMBER_P(u), u, 1, "gsl-ellipj", "a number");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(m), m, 2, "gsl-ellipj", "a number");
  gsl_sf_elljac_e(XEN_TO_C_DOUBLE(u),
		  XEN_TO_C_DOUBLE(m),
		  &sn, &cn, &dn);
  return(XEN_LIST_3(C_TO_XEN_DOUBLE(sn),
		    C_TO_XEN_DOUBLE(cn),
		    C_TO_XEN_DOUBLE(dn)));
}


#include <gsl/gsl_version.h>
#if ((GSL_MAJOR_VERSION >= 1) && (GSL_MINOR_VERSION >= 9))
  #define HAVE_GSL_EIGEN_NONSYMMV_WORKSPACE 1
#endif

#if HAVE_GSL_EIGEN_NONSYMMV_WORKSPACE

/* eignevector/values, from gsl/doc/examples/eigen_nonsymm.c */

#include <gsl/gsl_math.h>
#include <gsl/gsl_eigen.h>

static XEN g_gsl_eigenvectors(XEN matrix)
{
  double *data;
  int i, j, len;
  XEN values = XEN_FALSE, vectors = XEN_FALSE;

#if HAVE_SCHEME
  XEN_ASSERT_TYPE(s7_is_float_vector(matrix), matrix, 1, "gsl-eigenvectors", "a float vector");
  len = (int)sqrt(s7_vector_length(matrix));
  data = (double *)s7_float_vector_elements(matrix);
#else
  mus_any *u1;
  XEN_ASSERT_TYPE(mus_xen_p(matrix), matrix, 1, "gsl-eigenvectors", "a mixer (matrix)");
  u1 = XEN_TO_MUS_ANY(matrix);
  if (!mus_mixer_p(u1)) return(XEN_FALSE);

  len = mus_length(u1);
  data = (double *)calloc(len * len, sizeof(double));
  for (i = 0; i < len; i++)
    for (j = 0; j < len; j++)
      data[i * len + j] = mus_mixer_ref(u1, i, j);
#endif

  {
    gsl_matrix_view m = gsl_matrix_view_array(data, len, len);
    gsl_vector_complex *eval = gsl_vector_complex_alloc(len);
    gsl_matrix_complex *evec = gsl_matrix_complex_alloc(len, len);
    gsl_eigen_nonsymmv_workspace *w = gsl_eigen_nonsymmv_alloc(len);
    gsl_eigen_nonsymmv(&m.matrix, eval, evec, w);
    gsl_eigen_nonsymmv_free(w);
    gsl_eigen_nonsymmv_sort(eval, evec, GSL_EIGEN_SORT_ABS_DESC);
  
    {
      int values_loc, vectors_loc;

      values = XEN_MAKE_VECTOR(len, XEN_ZERO);
      values_loc = snd_protect(values);
      vectors = XEN_MAKE_VECTOR(len, XEN_FALSE);
      vectors_loc = snd_protect(vectors);

      for (i = 0; i < len; i++)
	{
	  XEN vect;
	  gsl_complex eval_i = gsl_vector_complex_get(eval, i);
	  gsl_vector_complex_view evec_i = gsl_matrix_complex_column(evec, i);
	  XEN_VECTOR_SET(values, i, C_TO_XEN_DOUBLE(GSL_REAL(eval_i)));
	
	  vect = XEN_MAKE_VECTOR(len, XEN_ZERO);
	  XEN_VECTOR_SET(vectors, i, vect);

	  for (j = 0; j < len; j++)
	    {
	      gsl_complex z = gsl_vector_complex_get(&evec_i.vector, j);
	      XEN_VECTOR_SET(vect, j, C_TO_XEN_DOUBLE(GSL_REAL(z)));
	    }
	}
      snd_unprotect_at(values_loc);
      snd_unprotect_at(vectors_loc);
    }

    gsl_vector_complex_free(eval);
    gsl_matrix_complex_free(evec);
  }

#if (!HAVE_SCHEME)
  free(data);
#endif
  return(XEN_LIST_2(values, vectors));
}
#endif


#if HAVE_COMPLEX_TRIG && HAVE_COMPLEX_NUMBERS
#include <gsl/gsl_poly.h>
#include <complex.h>

static XEN g_gsl_roots(XEN poly)
{
  #define H_gsl_roots "(gsl-roots poly): roots of poly"
  int i, n, loc;
  double *p;
  double complex *z;
  gsl_poly_complex_workspace *w;
  XEN result;

  XEN_ASSERT_TYPE(XEN_VECTOR_P(poly), poly, 1, "gsl-roots", "a vector");

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



/* -------- source file extensions list -------- */

static char **source_file_extensions = NULL;
static int source_file_extensions_size = 0;
static int source_file_extensions_end = 0;
static int default_source_file_extensions = 0;

static void add_source_file_extension(const char *ext)
{
  int i;
  for (i = 0; i < source_file_extensions_end; i++)
    if (mus_strcmp(ext, source_file_extensions[i]))
      return;
  if (source_file_extensions_end == source_file_extensions_size)
    {
      source_file_extensions_size += 8;
      if (source_file_extensions == NULL)
	source_file_extensions = (char **)calloc(source_file_extensions_size, sizeof(char *));
      else source_file_extensions = (char **)realloc(source_file_extensions, source_file_extensions_size * sizeof(char *));
    }
  source_file_extensions[source_file_extensions_end] = mus_strdup(ext);
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
	    if (mus_strcmp(ext, source_file_extensions[i]))
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
  XEN_ASSERT_TYPE(XEN_STRING_P(ext), ext, 1, S_add_source_file_extension, "a string");
  add_source_file_extension(XEN_TO_C_STRING(ext));
  return(ext);
}


static char *find_source_file(const char *orig)
{
  int i;
  char *str;
  for (i = 0; i < source_file_extensions_end; i++)
    {
      str = mus_format("%s.%s", orig, source_file_extensions[i]);
      if (mus_file_probe(str))
	return(str);
      free(str);
    }
  return(NULL);
}


/* list-in-vector|list, vector-in-list|vector, cobj-in-vector|list obj-in-cobj
 *   string-ci-in-vector? hash-table cases?
 *   most of this could be done via for-each
 */

#if HAVE_SCHEME && (!_MSC_VER)
  XEN_ARGIFY_2(g_dlopen_w, g_dlopen)
  XEN_NARGIFY_1(g_dlclose_w, g_dlclose)
  XEN_NARGIFY_0(g_dlerror_w, g_dlerror)
  XEN_NARGIFY_2(g_dlinit_w, g_dlinit)
  XEN_NARGIFY_2(g_dlsym_w, g_dlsym)
#endif
#if HAVE_SCHEME
  XEN_VARGIFY(g_snd_s7_error_handler_w, g_snd_s7_error_handler);
#endif

XEN_NARGIFY_1(g_snd_print_w, g_snd_print)
XEN_NARGIFY_0(g_little_endian_w, g_little_endian)
XEN_NARGIFY_0(g_snd_global_state_w, g_snd_global_state)
XEN_NARGIFY_1(g_add_source_file_extension_w, g_add_source_file_extension)

#if (!HAVE_SCHEME)
XEN_NARGIFY_2(g_fmod_w, g_fmod)
#endif

#if HAVE_SPECIAL_FUNCTIONS || HAVE_GSL
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
  XEN_NARGIFY_1(g_i1_w, g_i1)
  XEN_NARGIFY_2(g_in_w, g_in)
  XEN_NARGIFY_1(g_k0_w, g_k0)
  XEN_NARGIFY_1(g_k1_w, g_k1)
  XEN_NARGIFY_2(g_kn_w, g_kn)

  XEN_NARGIFY_1(g_gsl_ellipk_w, g_gsl_ellipk)
  XEN_NARGIFY_2(g_gsl_ellipj_w, g_gsl_ellipj)
#if HAVE_GSL_EIGEN_NONSYMMV_WORKSPACE
  XEN_NARGIFY_1(g_gsl_eigenvectors_w, g_gsl_eigenvectors)
#endif

  #if HAVE_COMPLEX_TRIG && HAVE_COMPLEX_NUMBERS
    XEN_NARGIFY_1(g_gsl_roots_w, g_gsl_roots)
  #endif
#endif



#if USE_MOTIF
  void Init_libxm(void);
#else
  void Init_libxg(void);
#endif

#if HAVE_GL
 void Init_libgl(void);
#endif


static char *legalize_path(const char *in_str)
{ 
  int inlen;
  char *out_str;
  int inpos, outpos = 0; 

  inlen = mus_strlen(in_str); 
  out_str = (char *)calloc(inlen * 2, sizeof(char)); 

  for (inpos = 0; inpos < inlen; inpos++)
    { 
      if (in_str[inpos] == '\\')
	out_str[outpos++] = '\\';
      out_str[outpos++] = in_str[inpos]; 
    } 

  return(out_str); 
} 


#if HAVE_GL
static XEN g_snd_glx_context(void)
{
  return(XEN_LIST_2(C_STRING_TO_XEN_SYMBOL("GLXContext"), 
		    XEN_WRAP_C_POINTER(ss->cx)));
} 

XEN_NARGIFY_0(g_snd_glx_context_w, g_snd_glx_context)
#endif



/* -------------------------------------------------------------------------------- */

void g_xen_initialize(void)
{
#if HAVE_RUBY
  rb_gc_disable();
#endif

  add_source_file_extension(XEN_FILE_EXTENSION);
#if HAVE_SCHEME
  add_source_file_extension("cl");
  add_source_file_extension("lisp");
  add_source_file_extension("init");  /* for slib */
#endif

#if HAVE_FORTH
  add_source_file_extension("fth");
  add_source_file_extension("fsm");
#endif
  add_source_file_extension("marks"); /* from save-marks */
  default_source_file_extensions = source_file_extensions_end;

  XEN_DEFINE_PROCEDURE("snd-global-state", g_snd_global_state_w, 0, 0, 0, "internal testing function");
  XEN_DEFINE_PROCEDURE(S_add_source_file_extension, g_add_source_file_extension_w, 1, 0, 0, H_add_source_file_extension);

  ss->snd_open_file_hook = XEN_DEFINE_SIMPLE_HOOK("(make-hook 'reason)", 1);
  XEN_PROTECT_FROM_GC(ss->snd_open_file_hook);

  ss->effects_hook = XEN_DEFINE_HOOK(S_effects_hook, "(make-hook)", 0, "called when something changes that the effects dialogs care about");

  Init_sndlib();

#if HAVE_FORTH
  fth_add_loaded_files("sndlib.so");
#endif

#if (!HAVE_SCHEME)
  gc_protection = XEN_FALSE;
#endif

  XEN_DEFINE_SAFE_PROCEDURE(S_snd_print,      g_snd_print_w,     1, 0, 0, H_snd_print);
  XEN_DEFINE_SAFE_PROCEDURE("little-endian?", g_little_endian_w, 0, 0, 0, "return " PROC_TRUE " if host is little endian");

#if HAVE_SCHEME
  XEN_EVAL_C_STRING("(define fmod modulo)");
#else
  XEN_DEFINE_PROCEDURE("fmod",           g_fmod_w,          2, 0, 0, "C's fmod");
#endif

#if HAVE_SCHEME && (!HAVE_GSL)
#define XEN_DEFINE_REAL_PROCEDURE(Name, Func, ReqArg, OptArg, RstArg, Doc) \
  do { \
  s7_pointer sym, f;							\
  sym = s7_define_safe_function(s7, Name, Func, ReqArg, OptArg, RstArg, Doc); \
  f = s7_value(s7, sym); \
  s7_function_set_returns_temp(f);\
  } while (0)
#else
#define XEN_DEFINE_REAL_PROCEDURE(Name, Func, ReqArg, OptArg, RstArg, Doc) XEN_DEFINE_SAFE_PROCEDURE(Name, Func, ReqArg, OptArg, RstArg, Doc)
#endif

#if HAVE_SPECIAL_FUNCTIONS || HAVE_GSL
  XEN_DEFINE_REAL_PROCEDURE(S_bes_j0, g_j0_w,     1, 0, 0, H_j0);
  XEN_DEFINE_REAL_PROCEDURE(S_bes_j1, g_j1_w,     1, 0, 0, H_j1);
  XEN_DEFINE_REAL_PROCEDURE(S_bes_jn, g_jn_w,     2, 0, 0, H_jn);
  XEN_DEFINE_REAL_PROCEDURE(S_bes_y0, g_y0_w,     1, 0, 0, H_y0);
  XEN_DEFINE_REAL_PROCEDURE(S_bes_y1, g_y1_w,     1, 0, 0, H_y1);
  XEN_DEFINE_REAL_PROCEDURE(S_bes_yn, g_yn_w,     2, 0, 0, H_yn);
  XEN_DEFINE_REAL_PROCEDURE("erf",    g_erf_w,    1, 0, 0, H_erf);
  XEN_DEFINE_REAL_PROCEDURE("erfc",   g_erfc_w,   1, 0, 0, H_erfc);
  XEN_DEFINE_REAL_PROCEDURE("lgamma", g_lgamma_w, 1, 0, 0, H_lgamma);
#endif

  XEN_DEFINE_PROCEDURE(S_bes_i0, g_i0_w,     1, 0, 0, H_i0);

#if HAVE_GSL
  XEN_DEFINE_SAFE_PROCEDURE(S_bes_i1, g_i1_w,     1, 0, 0, H_i1);
  XEN_DEFINE_SAFE_PROCEDURE(S_bes_in, g_in_w,     2, 0, 0, H_in);
  XEN_DEFINE_SAFE_PROCEDURE(S_bes_k0, g_k0_w,     1, 0, 0, H_k0);
  XEN_DEFINE_SAFE_PROCEDURE(S_bes_k1, g_k1_w,     1, 0, 0, H_k1);
  XEN_DEFINE_SAFE_PROCEDURE(S_bes_kn, g_kn_w,     2, 0, 0, H_kn);

  XEN_DEFINE_SAFE_PROCEDURE("gsl-ellipk", g_gsl_ellipk_w, 1, 0, 0, H_gsl_ellipk);
  XEN_DEFINE_SAFE_PROCEDURE("gsl-ellipj", g_gsl_ellipj_w, 2, 0, 0, H_gsl_ellipj);

#if HAVE_GSL_EIGEN_NONSYMMV_WORKSPACE
  XEN_DEFINE_SAFE_PROCEDURE("gsl-eigenvectors", g_gsl_eigenvectors_w, 1, 0, 0, "returns eigenvalues and eigenvectors");
#endif

#if HAVE_COMPLEX_TRIG && HAVE_COMPLEX_NUMBERS
  XEN_DEFINE_SAFE_PROCEDURE("gsl-roots",  g_gsl_roots_w,  1, 0, 0, H_gsl_roots);
#endif
#endif

#if HAVE_SCHEME && WITH_GMP
  s7_define_function(s7, "bignum-fft", bignum_fft, 3, 1, false, H_bignum_fft);
#endif

  g_init_base();
  g_init_utils();
  g_init_marks();
  g_init_regions();
  g_init_selection();
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
  g_init_dac(); /* needs to follow snd and mix */
  g_init_file();
  g_init_data();
  g_init_env();
  g_init_find();
#if (!USE_NO_GUI)
  g_init_gxcolormaps();
  g_init_gxfile();
  g_init_gxdraw();
  g_init_gxenv();
  g_init_gxmenu();
  g_init_axis();
  g_init_gxlistener();
  g_init_gxchn();
  g_init_draw();
  g_init_gxregion();
  g_init_gxsnd();
  g_init_gxfind();
#endif


#if HAVE_SCHEME && (!_MSC_VER)
  XEN_DEFINE_PROCEDURE("dlopen",  g_dlopen_w,  1, 1 ,0, H_dlopen);
  XEN_DEFINE_PROCEDURE("dlclose", g_dlclose_w, 1, 0 ,0, H_dlclose);
  XEN_DEFINE_PROCEDURE("dlerror", g_dlerror_w, 0, 0 ,0, H_dlerror);
  XEN_DEFINE_PROCEDURE("dlinit",  g_dlinit_w,  2, 0 ,0, H_dlinit);
  XEN_DEFINE_PROCEDURE("dlsym",   g_dlsym_w,   2, 0 ,0, H_dlsym);

  XEN_DEFINE_CONSTANT("RTLD_LAZY", RTLD_LAZY, "dlopen flag");
  XEN_DEFINE_CONSTANT("RTLD_NOW", RTLD_NOW, "dlopen flag");
  XEN_DEFINE_CONSTANT("RTLD_GLOBAL", RTLD_GLOBAL, "dlopen flag");
#endif

#if HAVE_LADSPA && HAVE_EXTENSION_LANGUAGE
  g_ladspa_to_snd();
#endif

#ifdef SCRIPTS_DIR
  XEN_ADD_TO_LOAD_PATH((char *)SCRIPTS_DIR);
#endif

  { 
    char *pwd, *legal_pwd; 
    pwd = mus_getcwd(); 
    legal_pwd = legalize_path(pwd);
    XEN_ADD_TO_LOAD_PATH(legal_pwd); 
    free(legal_pwd); 
  } 

#if HAVE_SCHEME
  XEN_DEFINE_PROCEDURE("_snd_s7_error_handler_", g_snd_s7_error_handler_w,  0, 0, 1, "internal error redirection for snd/s7");

  XEN_EVAL_C_STRING("(define redo-edit redo)");        /* consistency with Ruby */
  XEN_EVAL_C_STRING("(define undo-edit undo)");
  
  /* XEN_EVAL_C_STRING("(define (procedure-name proc) (if (procedure? proc) (format #f \"~A\" proc) #f))"); */
  /* needed in snd-test.scm and hooks.scm */

  XEN_EVAL_C_STRING("\
        (define* (apropos name port)\
          \"(apropos name (port #f)) looks for 'name' as a part of any symbol name, and sends matches to 'port'\"\
          (define (apropos-1 e)\
            (for-each\
             (lambda (binding)\
               (if (and (pair? binding)\
                        (string-position name (symbol->string (car binding))))\
                   (let ((str (format #f \"~%~A: ~A\" \
	        	              (car binding) \
	        	              (if (procedure? (cdr binding))\
	        	                  (procedure-documentation (cdr binding))\
	        	                  (cdr binding)))))\
                     (if (not port)\
                         (snd-print str)\
	                 (display str port)))))\
             e))\
          (if (or (not (string? name))\
                  (= (length name) 0))\
              (error 'wrong-type-arg \"apropos argument should be a non-nil string\")\
              (begin \
                 (if (not (eq? (current-environment) (global-environment)))\
                     (for-each apropos-1 (environment->list (current-environment)))) \
                 (apropos-1 (global-environment)))))");

  XEN_EVAL_C_STRING("\
(define break-ok #f)\
(define break-exit #f)  ; a kludge to get 2 funcs to share a local variable\n\
(define break-enter #f)\
\
(let ((saved-listener-prompt (listener-prompt)))\
  (set! break-exit (lambda ()\
		     (hook-clear read-hook)\
		     (set! (listener-prompt) saved-listener-prompt)\
		     #f))\
  (set! break-enter (lambda ()\
		      (set! saved-listener-prompt (listener-prompt)))))\
\
(define-macro (break)\
  `(let ((__break__ (current-environment)))\
     (break-enter)\
     (set! (listener-prompt) (format #f \"~A>\" (if (defined? __func__) __func__ 'break)))\
     (call/cc\
      (lambda (return)\
	(set! break-ok return)      ; save current program loc so (break-ok) continues from the break\n\
	(hook-push read-hook        ; anything typed in the listener is evaluated in the environment of the break call\n\
		   (lambda (str)\
		     (eval-string str __break__)))\
	(error 'snd-top-level)))    ; jump back to the top level\n\
     (break-exit)))                 ; we get here if break-ok is called\n\
");

#endif

#if HAVE_SCHEME && USE_GTK && (!HAVE_GTK_ADJUSTMENT_GET_UPPER)
  /* Gtk 3 is removing direct struct accesses (which they should have done years ago), so we need compatibility functions: */
  XEN_EVAL_C_STRING("(define (gtk_widget_get_window w) (.window w))");
  XEN_EVAL_C_STRING("(define (gtk_font_selection_dialog_get_ok_button w) (.ok_button w))");
  XEN_EVAL_C_STRING("(define (gtk_font_selection_dialog_get_apply_button w) (.apply_button w))");
  XEN_EVAL_C_STRING("(define (gtk_font_selection_dialog_get_cancel_button w) (.cancel_button w))");
  XEN_EVAL_C_STRING("(define (gtk_color_selection_dialog_get_color_selection w) (.colorsel w))");
  XEN_EVAL_C_STRING("(define (gtk_dialog_get_action_area w) (.action_area w))");
  XEN_EVAL_C_STRING("(define (gtk_dialog_get_content_area w) (.vbox w))");
  /* also gtk_adjustment fields, but I think they are not in use in Snd's gtk code */
#endif

#if HAVE_FORTH
  XEN_EVAL_C_STRING("<'> redo alias redo-edit");        /* consistency with Ruby */ 
  XEN_EVAL_C_STRING("<'> undo alias undo-edit"); 
  XEN_EVAL_C_STRING(": clm-print ( fmt :optional args -- ) fth-format snd-print drop ;"); 
#endif

#if HAVE_RUBY
  XEN_EVAL_C_STRING("def clm_print(str, *args)\n\
                      snd_print format(str, *args)\n\
                      end");
#endif

#if (!DISABLE_DEPRECATED)
#if HAVE_SCHEME
  XEN_EVAL_C_STRING("(define (clm-print . args) \"(clm-print . args) applies format to args and prints the result via snd-print\" \
                       (snd-print (apply format #f args)))");
#endif
#endif

#if HAVE_GL
  XEN_DEFINE_PROCEDURE("snd-glx-context", g_snd_glx_context_w, 0, 0, 0, "OpenGL GLXContext");
#endif

#if USE_MOTIF
  Init_libxm();
  #if HAVE_FORTH
    fth_add_loaded_files("libxm.so");
  #endif
#endif

#if 1
#if USE_GTK
  Init_libxg();
  #if HAVE_FORTH
    fth_add_loaded_files("libxg.so");
  #endif
#endif
#endif

#if HAVE_GL
  Init_libgl();
#endif

#if HAVE_ALSA
  XEN_PROVIDE("alsa");
#endif

#if HAVE_OSS
  XEN_PROVIDE("oss");
#endif

#if MUS_PULSEAUDIO
  XEN_PROVIDE("pulse-audio");
#endif

#if MUS_JACK
  XEN_PROVIDE("jack");
#endif

#if HAVE_GSL
  XEN_PROVIDE("gsl");
#endif

#if USE_MOTIF
  XEN_PROVIDE("snd-motif");
#endif

#if USE_GTK
  XEN_PROVIDE("snd-gtk");
#if HAVE_GTK_3
  XEN_PROVIDE("gtk3");
#else
  XEN_PROVIDE("gtk2");
#endif
#endif

#if USE_NO_GUI
  XEN_PROVIDE("snd-nogui");
#endif

#if HAVE_FORTH
  XEN_PROVIDE("snd-forth");
#endif

#if HAVE_SCHEME
  XEN_PROVIDE("snd-s7");
#endif

#if WITH_AUDIO
  XEN_PROVIDE("audio");
#endif

#if HAVE_RUBY
  XEN_PROVIDE("snd-ruby");
  /* we need to set up the search path so that load and require will work as in the program irb */
  {
    XEN paths;
    int i, len;
    paths = rb_gv_get("$:");
    /* this is printed as 
     *   ["/home/bil/ruby-snd", "/usr/local/share/snd", "/usr/local/lib/ruby/site_ruby/2.0.0", ...]
     */
    len = XEN_VECTOR_LENGTH(paths);
    for (i = 0; i < len; i++)
      XEN_ADD_TO_LOAD_PATH(XEN_TO_C_STRING(XEN_VECTOR_REF(paths, i)));
  }
#endif

  XEN_PROVIDE("snd");
  XEN_PROVIDE("snd" SND_MAJOR_VERSION);
  XEN_PROVIDE("snd-" SND_MAJOR_VERSION "." SND_MINOR_VERSION);

#if HAVE_RUBY
  rb_gc_enable();
#endif

}

