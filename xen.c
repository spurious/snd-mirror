/* xen support procedures */

#if defined(HAVE_CONFIG_H)
  #include "config.h"
#endif

#include <ctype.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include "xen.h"


XEN xen_return_first(XEN a, ...)
{
  return(a);
}

int xen_to_c_int_or_else(XEN obj, int fallback, const char *origin)
{
  /* don't want errors about floats with non-zero fractions etc */
  if (XEN_INTEGER_P(obj))
    return(XEN_TO_C_INT(obj));
  else
    if (XEN_NUMBER_P(obj))
      return((int)XEN_TO_C_DOUBLE_WITH_CALLER(obj, origin));
  return(fallback);
}


/* ------------------------------ GUILE ------------------------------ */

#if HAVE_GUILE

char *xen_version(void)
{
  char *buf;
  buf = (char *)calloc(64, sizeof(char));
  sprintf(buf, "Xen: %s, Guile: %s", XEN_VERSION, XEN_TO_C_STRING(scm_version()));
  return(buf);
}

void *xen_malloc(int size)
{
  return((void *)(scm_must_malloc(size, __FUNCTION__)));
}

void xen_repl(int argc, char **argv)
{
  scm_shell(argc, argv); 
}

void xen_gc_mark(XEN val)
{
  scm_gc_mark(val);
}

void xen_initialize(void)
{
#if HAVE_SCM_INIT_GUILE
  scm_init_guile();
#endif
}

void xen_guile_define_procedure_with_setter(char *get_name, XEN (*get_func)(), char *get_help,
					    char *set_name, XEN (*set_func)(), 
					    XEN local_doc,
					    int get_req, int get_opt, int set_req, int set_opt)
{
#if HAVE_SCM_C_DEFINE
  XEN str;
  str = C_TO_XEN_STRING(get_help);
  scm_permanent_object(
    scm_c_define(get_name,
      scm_make_procedure_with_setter(
        XEN_NEW_PROCEDURE("", XEN_PROCEDURE_CAST get_func, get_req, get_opt, 0),
	XEN_NEW_PROCEDURE(set_name, XEN_PROCEDURE_CAST set_func, set_req, set_opt, 0))));
  scm_set_object_property_x(C_STRING_TO_XEN_SYMBOL(get_name), local_doc, str);
  scm_set_procedure_property_x(XEN_NAME_AS_C_STRING_TO_VALUE(get_name), local_doc, str);
#else
  scm_set_object_property_x(
    XEN_CDR(
      gh_define(get_name,
	scm_make_procedure_with_setter(
          XEN_NEW_PROCEDURE("", XEN_PROCEDURE_CAST get_func, get_req, get_opt, 0),
	  XEN_NEW_PROCEDURE(set_name, XEN_PROCEDURE_CAST set_func, set_req, set_opt, 0)
	  ))),
    local_doc,
    C_TO_XEN_STRING(get_help));
  /* still need to trap help output and send it to the listener */
#endif
}

void xen_guile_define_procedure_with_reversed_setter(char *get_name, XEN (*get_func)(), char *get_help,
						     char *set_name, XEN (*set_func)(), XEN (*reversed_set_func)(), 
						     XEN local_doc,
						     int get_req, int get_opt, int set_req, int set_opt)
{
#if HAVE_SCM_C_DEFINE
  XEN str;
  str = C_TO_XEN_STRING(get_help);
  scm_permanent_object(
    scm_c_define(get_name,
      scm_make_procedure_with_setter(
        XEN_NEW_PROCEDURE("", XEN_PROCEDURE_CAST get_func, get_req, get_opt, 0),
	XEN_NEW_PROCEDURE("", XEN_PROCEDURE_CAST reversed_set_func, set_req, set_opt, 0))));
  XEN_NEW_PROCEDURE(set_name, XEN_PROCEDURE_CAST set_func, set_req, set_opt, 0);
  scm_set_object_property_x(C_STRING_TO_XEN_SYMBOL(get_name), local_doc, str);
  scm_set_procedure_property_x(XEN_NAME_AS_C_STRING_TO_VALUE(get_name), local_doc, str);
#else
  scm_set_object_property_x(
    XEN_CDR(
      gh_define(get_name,
	scm_make_procedure_with_setter(
          XEN_NEW_PROCEDURE("", XEN_PROCEDURE_CAST get_func, get_req, get_opt, 0),
	  XEN_NEW_PROCEDURE("", XEN_PROCEDURE_CAST reversed_set_func, set_req, set_opt, 0)
	  ))),
    local_doc,
    C_TO_XEN_STRING(get_help));
  /* still need to trap help output and send it to the listener */
  XEN_NEW_PROCEDURE(set_name, XEN_PROCEDURE_CAST set_func, set_req, set_opt, 0);
#endif
}

XEN xen_guile_create_hook(const char *name, int args, const char *help, XEN local_doc)
{
  /* documentation for hooks */
  XEN hook;
#if HAVE_GUILE
  if ((name) && (help))
    {
      hook = scm_permanent_object(scm_make_hook(C_TO_SMALL_XEN_INT(args)));
      scm_set_object_property_x(hook, local_doc, C_TO_XEN_STRING(help));
#if HAVE_SCM_C_DEFINE
      scm_c_define(name, hook);
#else
      gh_define(name, hook);
#endif
    }
  else hook = scm_make_hook(C_TO_SMALL_XEN_INT(args));
#endif
  return(hook);
}
#endif


/* ------------------------------ RUBY ------------------------------ */

#if HAVE_RUBY

void xen_initialize(void)
{
  ruby_init();
}

void *xen_malloc(int size)
{
  return((void *)(ALLOC_N(char, size)));
}

void xen_gc_mark(XEN val)
{
  rb_gc_mark((void *)val);
}

XEN xen_rb_cdr(XEN val)
{
  rb_ary_delete_at(val, 0);
  return(val);
}

XEN xen_rb_cons(XEN arg1, XEN arg2)
{
  if (XEN_NULL_P(arg2))
    return(rb_ary_new3(1, arg1));
  return(rb_ary_unshift(arg2, arg1));
}

XEN xen_rb_cons2(XEN arg1, XEN arg2, XEN arg3)
{
  return(rb_ary_unshift(xen_rb_cons(arg2, arg3), arg1));
}

XEN xen_rb_ary_new_with_initial_element(long num, XEN element)
{
  XEN arr;
  int i;
  arr = rb_ary_new2(num);
  for (i = 0; i < num; i++)
    rb_ary_store(arr, i, element);
  return(arr);
}

static char *scheme_to_ruby(const char *name)
{
  /* replace any non-alphanumeric except "?" with "_". "?" -> "_p". '->" -> "2" drop "!" */
  char *new_name = NULL;
  int len, i, j;
  len = strlen(name);
  if (len > 0)
    {
      new_name = (char *)calloc(len + 2, sizeof(char)); /* +1 for possible _p, +1 for possible $ */
      for (i = 0, j = 0; i < len; i++)
	{
	  if (isalnum(name[i]))
	    new_name[j++] = name[i];
	  else 
	    {
	      if (name[i] != '!')
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
    }
  return(new_name);
}

char *xen_scheme_constant_to_ruby(const char *name)
{
  /* upcase 1st char */
  char *new_name;
  new_name = scheme_to_ruby(name);
  new_name[0] = toupper(new_name[0]);
  return(new_name);
}

char *xen_scheme_procedure_to_ruby(const char *name)
{
  /* return(scheme_to_ruby(name)); */
  char *new_name = NULL;
  int len, i, j;
  len = strlen(name);
  if (len > 0)
    {
      new_name = (char *)calloc(len + 1, sizeof(char));
      for (i = 0, j = 0; i < len; i++)
	{
	  if ((isalnum(name[i])) || (name[i] == '!') || (name[i] == '?'))
	    new_name[j++] = name[i];
	  else 
	    {
	      if ((name[i] == '-') &&
		  (name[i + 1] == '>'))
		{
		  new_name[j++] = '2';
		  i++;
		}
	      else new_name[j++] = '_';
	    }
	}
    }
  return(new_name);
}

char *xen_scheme_global_variable_to_ruby(const char *name)
{
  /* prepend $ */
  char *new_name;
  int i, len;
  new_name = scheme_to_ruby(name);
  len = strlen(new_name);
  for (i = len; i > 0; i--)
    new_name[i] = new_name[i - 1];
  new_name[0] = '$';
  return(new_name);
}

char *xen_version(void)
{
  char *buf;
  buf = (char *)calloc(128, sizeof(char));
  sprintf(buf, "Xen: %s, Ruby: %s (%s)", 
	  XEN_VERSION,
	  XEN_TO_C_STRING(XEN_EVAL_C_STRING("RUBY_VERSION")),
	  XEN_TO_C_STRING(XEN_EVAL_C_STRING("RUBY_RELEASE_DATE")));
  return(buf);
}

#if HAVE_READLINE
  #include <readline/readline.h>
#endif

static XEN xen_rb_report_error(XEN nada, XEN err_info)
{
  /* TODO: backtrace info? */
  /*    return rb_funcall(err_info, rb_intern("backtrace"), 0); */
  /* which can be an array of strings */

  fprintf(stderr,"error: %s\n", XEN_TO_C_STRING(XEN_TO_STRING(err_info)));
  return(XEN_FALSE);
}

static XEN xen_rb_rep(XEN ig)
{
  XEN val;
  int status = 0;
  char *str;
#if HAVE_READLINE
  char *line_read = NULL;
  line_read = readline(">");
  if ((line_read) && (*line_read))
    {
      add_history(line_read);
      val = XEN_EVAL_C_STRING(line_read);
      if (status)
	{
	  fprintf(stdout,"error: %d\n", status);
	  fprintf(stdout, XEN_TO_C_STRING(XEN_TO_STRING(val)));
	}
      else 
	{
	  str = XEN_TO_NEW_C_STRING(XEN_TO_STRING(val));
	  fprintf(stdout, "%s\n", (str) ? str : "nil");
	  if (str) free(str);
	}
      free(line_read);
      line_read = NULL;
    }
#else
  int size = 512;
  char **buffer = NULL;
  buffer = (char **)calloc(1, sizeof(char *));
  buffer[0] = (char *)calloc(size, sizeof(char));
  fprintf(stdout, ">");
  getline(buffer, &size, stdin);
  val = XEN_EVAL_C_STRING(buffer[0]);
  if (status)
    fprintf(stdout,"error: %d\n", status);
  else 
    {
      str = XEN_TO_NEW_C_STRING(XEN_TO_STRING(val));
      fprintf(stdout, "%s\n", (str) ? str : "nil");
      if (str) free(str);
    }
  free(buffer[0]);
  free(buffer);
#endif
  return(ig);
}

static XEN xen_rb_rescue(XEN val)
{
  return(rb_rescue(xen_rb_rep,
		   XEN_FALSE,
		   xen_rb_report_error,
		   XEN_FALSE));
}

void xen_repl(int argc, char **argv)
{
  int status = 0;
  while (1)
    {
      rb_protect(xen_rb_rescue,
		 XEN_FALSE,
		 &status);
      if (status != 0)
	{
	  extern VALUE ruby_errinfo;
	  fprintf(stderr, "%s\n", XEN_TO_C_STRING(XEN_TO_STRING(ruby_errinfo)));
	  status = 0;
	}
    }
}

XEN xen_rb_eval_string_with_error(char *str)
{
  int status = 0;
  XEN res;
  res = rb_eval_string_protect(str, &status);
  if (status != 0)
    {
      extern VALUE ruby_errinfo;
      return(XEN_TO_STRING(ruby_errinfo));
    }
  return(res);
}

XEN xen_rb_load_file_with_error(XEN file)
{
  int status = 0;
  rb_load_protect(file, 0, &status);
  if (status != 0)
    {
      extern VALUE ruby_errinfo;
      return(XEN_TO_STRING(ruby_errinfo));
    }
  return(XEN_TRUE);
}

static char *lstbuf = NULL;
static char *xen_rb_list_to_s(XEN lst)
{
  int i, len;
  XEN val;
  if (lstbuf == NULL) 
    lstbuf = (char *)calloc(512, sizeof(char));
  else lstbuf[0] = '\0';
  len = XEN_LIST_LENGTH(lst);
  for (i = 0; i < len; i++)
    {
      strcat(lstbuf, XEN_TO_NEW_C_STRING(XEN_TO_STRING(XEN_LIST_REF(lst, i))));
      strcat(lstbuf, " ");
    }
  return(lstbuf);
}

void xen_rb_raise(XEN type, XEN info)
{
  rb_raise(rb_eStandardError, "%s: %s\n", 
	   rb_id2name(type), 
	   xen_rb_list_to_s(info));
}

XEN xen_rb_obj_as_string(XEN obj)
{
  int status = 0;
  XEN result;
  result = rb_protect(rb_obj_as_string,
		      obj,
		      &status);
  if (status != 0)
    return(C_TO_XEN_STRING("<invalid object>"));
  return(result);
}

static XEN xen_rb_apply_1(XEN args)
{
  return(rb_apply(XEN_CAR(args), rb_intern("call"), XEN_CADR(args)));
}

XEN xen_rb_apply(XEN func, XEN args)
{
  XEN val;
  int status = 0;
  val = rb_protect(xen_rb_apply_1,
		   XEN_LIST_2(func, args),
		   &status);
  if (status != 0)
    {
      extern VALUE ruby_errinfo;
      return(XEN_TO_STRING(ruby_errinfo));
    }
  return(val);
}

static XEN xen_rb_funcall_0_inner(XEN args)
{
  return(rb_funcall(args, rb_intern("call"), 0));
}

XEN xen_rb_funcall_0(XEN func)
{
  XEN val;
  int status = 0;
  val = rb_protect(xen_rb_funcall_0_inner,
		   func,
		   &status);
  if (status != 0)
    {
      extern VALUE ruby_errinfo;
      return(XEN_TO_STRING(ruby_errinfo));
    }
  return(val);
}

#endif


/* ------------------------------ MZSCHEME ------------------------------ */

#if HAVE_MZSCHEME
int xen_mzscheme_get_unsigned_int_val(XEN obj)
{
  unsigned long val;
  scheme_get_unsigned_int_val(obj, &val);
  return((int)val);
}

char *xen_version(void)
{
  char *buf;
  buf = (char *)calloc(64, sizeof(char));
  sprintf(buf, "Xen: %s, MzScheme: %s", XEN_VERSION, scheme_version());
  return(buf);
}

void xen_repl(int argc, char **argv)
{
  scheme_rep();
}

void xen_gc_mark(XEN val)
{
}

static Scheme_Env *xen_scheme_env = NULL;

Scheme_Env *xen_get_env(void) 
{
  if (xen_scheme_env == NULL)
    xen_scheme_env = scheme_basic_env();
  return(xen_scheme_env);
}

void xen_initialize(void)
{
  xen_get_env();
}

XEN xen_mzscheme_list_ref(XEN lst, int loc)
{
  int i, len;
  len = XEN_LIST_LENGTH(lst);
  if (loc >= len)
    return(XEN_EMPTY_LIST);
  for (i = 0; i < loc; i++)
    lst = XEN_CDR(lst);
  return(XEN_CAR(lst));
}

XEN xen_mzscheme_reverse_list(XEN lst)
{
  XEN last = XEN_EMPTY_LIST;
  while (!(XEN_NULL_P(lst)))
    {
      last = XEN_CONS(XEN_CAR(lst), last);
      lst = XEN_CDR(lst);
    }
  return(last);
}

void *xen_malloc(int size)
{
  return(scheme_malloc(size));
}

#endif


/* ------------------------------ NONE OF THE ABOVE ------------------------------ */

#if (!HAVE_GUILE) && (!HAVE_MZSCHEME) && (!HAVE_RUBY)

char *xen_version(void)
{
  return("no embedded language");
}

void *xen_malloc(int size)
{
  return(malloc(size));
}

void xen_repl(int argc, char **argv)
{
}

void xen_initialize(void)
{
}

void xen_gc_mark(XEN val)
{
}

#endif
