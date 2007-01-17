/* xen support procedures */

#include <mus-config.h>
#include <ctype.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#if HAVE_STDINT_H
  #include <stdint.h>
#endif
#include <math.h>
#include "xen.h"

XEN xen_return_first(XEN a, ...)
{
  return(a);
}

/* ------------------------------ GUILE ------------------------------ */

#if HAVE_GUILE

off_t xen_to_c_off_t_or_else(XEN obj, off_t fallback)
{
  if ((XEN_NOT_FALSE_P(scm_integer_p(obj))) && XEN_EXACT_P(obj))
#if (defined(SIZEOF_OFF_T) && (SIZEOF_OFF_T > 4)) || (defined(_FILE_OFFSET_BITS) && (_FILE_OFFSET_BITS == 64))
    return(XEN_TO_C_LONG_LONG(obj));
#else
    return(XEN_TO_C_INT(obj));
#endif
  else
    if (XEN_NUMBER_P(obj))
      return((off_t)XEN_TO_C_DOUBLE(obj));
  return(fallback);
}

off_t xen_to_c_off_t(XEN obj)
{
  if (XEN_EXACT_P(obj))
#if (defined(SIZEOF_OFF_T) && (SIZEOF_OFF_T > 4)) || (defined(_FILE_OFFSET_BITS) && (_FILE_OFFSET_BITS == 64))
    return(XEN_TO_C_LONG_LONG(obj));
#else
    return(XEN_TO_C_INT(obj));
#endif
  return((off_t)XEN_TO_C_DOUBLE(obj)); /* inexact integer squeezed through somewhere */
}

XEN c_to_xen_off_t(off_t val)
{
#if (defined(SIZEOF_OFF_T) && (SIZEOF_OFF_T > 4)) || (defined(_FILE_OFFSET_BITS) && (_FILE_OFFSET_BITS == 64))
    return(C_TO_XEN_LONG_LONG(val));
#else
    return(C_TO_XEN_INT(val));
#endif
}

char *xen_version(void)
{
  char *buf;
  buf = (char *)calloc(64, sizeof(char));
#if HAVE_SNPRINTF
  snprintf(buf, 64, "Guile: %s, Xen: %s", XEN_TO_C_STRING(scm_version()), XEN_VERSION);
#else
  sprintf(buf, "Guile: %s, Xen: %s", XEN_TO_C_STRING(scm_version()), XEN_VERSION);
#endif
  return(buf);
}

void xen_repl(int argc, char **argv)
{
  scm_shell(argc, argv); 
}

void xen_gc_mark(XEN val)
{
  scm_gc_mark(val);
}


double xen_to_c_double(XEN a) 
{
  double num = 0.0;
#if HAVE_SCM_TO_SIGNED_INTEGER
  num = scm_to_double(a);
#else
  num = scm_num2dbl(a, c__FUNCTION__);
#endif
#if HAVE_DECL_ISNAN
  if (isnan(num)) return(0.0);
#endif
#if HAVE_DECL_ISINF
  if (isinf(num)) return(0.0);
#endif
  return(num);
}

double xen_to_c_double_or_else(XEN a, double b) 
{
  if (XEN_NUMBER_P(a))
    return(xen_to_c_double(a));
  return(b);
}

bool xen_integer_p(XEN a) 
{
#if (!defined(SCM_PACK)) || HAVE_SCM_TO_SIGNED_INTEGER
  return(XEN_NOT_FALSE_P(scm_integer_p(a)));
#else
  if (SCM_INUMP(a)) return(true);
  if (SCM_REALP(a))
    {
      double r;
#ifdef SCM_REAL_VALUE
      r = SCM_REAL_VALUE(a);
#else
      r = SCM_REALPART(a);
#endif
      if (r == floor(r))
	return(true);
    }
  return(false);
#endif
}

int xen_to_c_int(XEN a)
{
  /* Scheme integer (possible inexact) to C int without errors */
#if HAVE_SCM_TO_SIGNED_INTEGER
    if ((SCM_INEXACTP(a)) || (SCM_FRACTIONP(a))) /* avoid error if inexact integer! SCM_INUMP deprecated in 1.7 */
      {
	if ((scm_is_true(scm_inf_p(a))) || 
	    (scm_is_true(scm_nan_p(a)))) 
	  return(0);
	if (SCM_REALP(a)) 
	  return((int)(SCM_REAL_VALUE(a)));
	return((int)scm_to_double(a)); 
      }
    return(scm_to_int32(a)); 
#else
  #if HAVE_SCM_NUM2INT
    if (SCM_INUMP(a)) return(SCM_INUM(a));
    if (SCM_REALP(a)) return((int)(SCM_REAL_VALUE(a)));
    return((int)scm_num2dbl(a, c__FUNCTION__));
  #else
    return((int)gh_scm2int(a));  /* ah for the good old days... */
  #endif
#endif
}

int xen_to_c_int_or_else(XEN obj, int fallback)
{
  /* don't want errors about floats with non-zero fractions etc */
  if (XEN_NUMBER_P(obj))
    return(xen_to_c_int(obj));
  return(fallback);
}

void xen_initialize(void)
{
#if HAVE_SCM_INIT_GUILE
  scm_init_guile();
#endif
}

void xen_guile_define_procedure_with_setter(char *get_name, XEN (*get_func)(), 
					    char *get_help, XEN (*set_func)(), XEN local_doc,
					    int get_req, int get_opt, int set_req, int set_opt)
{
#if HAVE_SCM_C_DEFINE
  XEN str = XEN_FALSE;
#if XEN_DEBUGGING
  if (XEN_DEFINED_P(get_name)) fprintf(stderr, "%s is defined\n", get_name);
  /* if (!(snd_url(get_name))) fprintf(stderr, "%s not documented\n", get_name); */
#endif

  if (get_help) str = C_TO_XEN_STRING(get_help);
  XEN_PROTECT_FROM_GC(
    XEN_DEFINE(get_name,
      scm_make_procedure_with_setter(
        XEN_NEW_PROCEDURE(get_name, XEN_PROCEDURE_CAST get_func, get_req, get_opt, 0),
	XEN_NEW_PROCEDURE(get_name, XEN_PROCEDURE_CAST set_func, set_req, set_opt, 0))));
  if (get_help)
    {
      scm_set_object_property_x(C_STRING_TO_XEN_SYMBOL(get_name), local_doc, str);
      scm_set_procedure_property_x(XEN_NAME_AS_C_STRING_TO_VALUE(get_name), local_doc, str);
    }
#else
  scm_set_object_property_x(
    XEN_CDR(
      XEN_DEFINE(get_name,
	scm_make_procedure_with_setter(
          XEN_NEW_PROCEDURE(get_name, XEN_PROCEDURE_CAST get_func, get_req, get_opt, 0),
	  XEN_NEW_PROCEDURE(get_name, XEN_PROCEDURE_CAST set_func, set_req, set_opt, 0)
	  ))),
    local_doc,
    C_TO_XEN_STRING(get_help));
#endif
}

void xen_guile_define_procedure_with_reversed_setter(char *get_name, XEN (*get_func)(), 
						     char *get_help, XEN (*set_func)(), XEN (*reversed_set_func)(), 
						     XEN local_doc, int get_req, int get_opt, int set_req, int set_opt)
{
#if HAVE_SCM_C_DEFINE
  XEN str = XEN_FALSE;
#if XEN_DEBUGGING
  if (XEN_DEFINED_P(get_name)) fprintf(stderr, "%s is defined\n", get_name);
  /* if (!(snd_url(get_name))) fprintf(stderr, "%s not documented\n", get_name); */
#endif
  if (get_help) str = C_TO_XEN_STRING(get_help);
  XEN_PROTECT_FROM_GC(
    XEN_DEFINE(get_name,
      scm_make_procedure_with_setter(
        XEN_NEW_PROCEDURE(get_name, XEN_PROCEDURE_CAST get_func, get_req, get_opt, 0),
	XEN_NEW_PROCEDURE(get_name, XEN_PROCEDURE_CAST reversed_set_func, set_req, set_opt, 0))));
  if (get_help)
    {
      scm_set_object_property_x(C_STRING_TO_XEN_SYMBOL(get_name), local_doc, str);
      scm_set_procedure_property_x(XEN_NAME_AS_C_STRING_TO_VALUE(get_name), local_doc, str);
    }
#else
  scm_set_object_property_x(
    XEN_CDR(
      XEN_DEFINE(get_name,
	scm_make_procedure_with_setter(
          XEN_NEW_PROCEDURE(get_name, XEN_PROCEDURE_CAST get_func, get_req, get_opt, 0),
	  XEN_NEW_PROCEDURE(get_name, XEN_PROCEDURE_CAST reversed_set_func, set_req, set_opt, 0)
	  ))),
    local_doc,
    C_TO_XEN_STRING(get_help));
#endif
}

XEN xen_guile_create_hook(const char *name, int args, const char *help, XEN local_doc)
{
  /* make-hook + documentation */
  XEN hook;
  hook = XEN_DEFINE_SIMPLE_HOOK(args);
  if ((name) && (help))
    scm_set_object_property_x(XEN_PROTECT_FROM_GC(hook), local_doc, C_TO_XEN_STRING(help));
  XEN_DEFINE(name, hook);
  return(hook);
}

#if XEN_DEBUGGING && HAVE_SCM_C_DEFINE_GSUBR
XEN xen_guile_dbg_new_procedure(const char *name, XEN (*func)(), int req, int opt, int rst)
{
  /* look for name collisions */
  if ((name) && (strlen(name) > 0) && XEN_DEFINED_P(name)) fprintf(stderr, "%s is defined\n", name);
  /* if ((name) && (strlen(name) > 0) && (!(snd_url(name)))) fprintf(stderr, "%s not documented\n", name); */
  return(scm_c_define_gsubr(name, req, opt, rst, func));
}
#endif

XEN xen_guile_add_to_load_path(char *path)
{
  char *buf = NULL;
  int len;
  XEN result = XEN_FALSE;
  if (path)
    {
      len = (strlen(path) * 2) + 256;
      buf = (char *)calloc(len, sizeof(char));
#if HAVE_SNPRINTF
      snprintf(buf, len, "(if (not (member \"%s\" %%load-path)) (set! %%load-path (cons \"%s\" %%load-path)))", path, path);
#else
      sprintf(buf, "(if (not (member \"%s\" %%load-path)) (set! %%load-path (cons \"%s\" %%load-path)))", path, path);
#endif 
      result = XEN_EVAL_C_STRING(buf);
      free(buf);
    }
  return(result);
}

#if HAVE_SCM_C_MAKE_RECTANGULAR
static char **xen_temp_strings = NULL;
static int xen_temp_strings_ctr = 0;
#define XEN_TEMP_STRINGS_SIZE 512

#if XEN_DEBUGGING
static char **stored_strings = NULL;
static int stored_strings_ctr = 0;
#endif

char *xen_guile_to_c_string_with_eventual_free(XEN str)
{
  char *result;
  if (XEN_FALSE_P(str)) return(NULL);
#if XEN_DEBUGGING
  XEN_ASSERT_TYPE(XEN_STRING_P(str), str, 0, "xen->c-string", "a string");
#endif
    
  if (!xen_temp_strings)
    xen_temp_strings = (char **)calloc(XEN_TEMP_STRINGS_SIZE, sizeof(char *));
  else
    {
      if (xen_temp_strings[xen_temp_strings_ctr]) 
#if XEN_DEBUGGING
	{
	  int i, len;
	  char *str;
	  str = xen_temp_strings[xen_temp_strings_ctr];
	  len = strlen(str);
	  for (i = 0; i < len; i++) str[i] = 'X';
	  if (!stored_strings) stored_strings = (char **)calloc(1024, sizeof(char *));
	  if (stored_strings[stored_strings_ctr]) free(stored_strings[stored_strings_ctr]);
	  stored_strings[stored_strings_ctr++] = str;
	  if (stored_strings_ctr >= 1024) stored_strings_ctr = 0;
	}
#else
	free(xen_temp_strings[xen_temp_strings_ctr]);
#endif
    }
  result = scm_to_locale_string(str); /* not XEN_TO_C_STRING here -- infinite recursion */
  xen_temp_strings[xen_temp_strings_ctr++] = result;
  if (xen_temp_strings_ctr >= XEN_TEMP_STRINGS_SIZE) xen_temp_strings_ctr = 0;
  return(result);
}
#endif

#endif


/* ------------------------------ RUBY ------------------------------ */

#if HAVE_RUBY

#define S_add_help "add_help"
#define S_get_help "get_help"

XEN rb_documentation(XEN name)
{
  XEN_ASSERT_TYPE((XEN_STRING_P(name) || XEN_SYMBOL_P(name)), name, XEN_ONLY_ARG, S_get_help, "a char* or symbol");
  if (XEN_SYMBOL_P(name))
    return(rb_property(XEN_SYMBOL_TO_STRING(name), XEN_DOCUMENTATION_SYMBOL));
  else
    return(rb_property(name, XEN_DOCUMENTATION_SYMBOL));
}

XEN rb_set_documentation(XEN name, XEN help)
{
  XEN_ASSERT_TYPE((XEN_STRING_P(name) || XEN_SYMBOL_P(name)), name, XEN_ARG_1, S_add_help, "a char* or symbol");
  XEN_ASSERT_TYPE(XEN_STRING_P(help), help, XEN_ARG_2, S_add_help, "a char*");
  if (XEN_SYMBOL_P(name))
    rb_set_property(XEN_SYMBOL_TO_STRING(name), XEN_DOCUMENTATION_SYMBOL, help);
  else
    rb_set_property(name, XEN_DOCUMENTATION_SYMBOL, help);
  return(name);
}

static XEN g_add_help(XEN name, XEN help)
{
#define H_add_help S_add_help"(name, help)  add help to topic or function name (String or Symbol)"
  return(rb_set_documentation(name, help));
}

static XEN g_get_help(XEN name)
{
#define H_get_help S_get_help"([name=:"S_get_help"])  \
return help associated with name (String or Symbol) or false"
  if (XEN_NOT_BOUND_P(name))
    return(C_TO_XEN_STRING(H_get_help));
  else
    return(rb_documentation(name));
}

void xen_initialize(void)
{
  ruby_init();
  Init_Hook();
}

off_t xen_to_c_off_t_or_else(XEN obj, off_t fallback)
{
  if (XEN_OFF_T_P(obj))
#if (defined(SIZEOF_OFF_T) && (SIZEOF_OFF_T > 4)) || (defined(_FILE_OFFSET_BITS) && (_FILE_OFFSET_BITS == 64))
    return(XEN_TO_C_LONG_LONG(obj));
#else
    return(XEN_TO_C_INT(obj));
#endif
  else
    if (XEN_NUMBER_P(obj))
      return((off_t)XEN_TO_C_DOUBLE(obj));
  return(fallback);
}

off_t xen_to_c_off_t(XEN obj)
{
#if (defined(SIZEOF_OFF_T) && (SIZEOF_OFF_T > 4)) || (defined(_FILE_OFFSET_BITS) && (_FILE_OFFSET_BITS == 64))
  return(XEN_TO_C_LONG_LONG(obj));
#else
  return(XEN_TO_C_INT(obj));
#endif
}

XEN c_to_xen_off_t(off_t val)
{
#if (defined(SIZEOF_OFF_T) && (SIZEOF_OFF_T > 4)) || (defined(_FILE_OFFSET_BITS) && (_FILE_OFFSET_BITS == 64))
    return(C_TO_XEN_LONG_LONG(val));
#else
    return(C_TO_XEN_INT(val));
#endif
}

int xen_to_c_int_or_else(XEN obj, int fallback)
{
  /* don't want errors about floats with non-zero fractions etc */
  if (XEN_INTEGER_P(obj))
    return(XEN_TO_C_INT(obj));
  else
    if (XEN_NUMBER_P(obj))
      return((int)XEN_TO_C_DOUBLE(obj));
  return(fallback);
}

void xen_gc_mark(XEN val)
{
#if HAVE_REASONABLE_RB_GC_MARK
  rb_gc_mark(val);
#endif
}

XEN xen_rb_cdr(XEN val)
{
  if (XEN_CONS_P(val))
    {
      XEN new_list;
      new_list = XEN_COPY_ARG(val);
      rb_ary_delete_at(new_list, 0);
      return(new_list);
    }
  return(val);
}

XEN xen_rb_cons(XEN arg1, XEN arg2)
{
  if (XEN_NULL_P(arg2))
    return(rb_ary_new3(1, arg1));
  if (!(XEN_CONS_P(arg2)))
    return(rb_ary_new3(2, arg1, arg2));
  return(rb_ary_unshift(arg2, arg1)); /* arg2 assumed to be array here in Ruby */
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

static char *last_name1 = NULL, *last_name2 = NULL, *last_name3; /* too much of a bother to free the return values over and over */

static char *scheme_to_ruby(const char *name)
{
  /* replace any non-alphanumeric except "?" with "_". "?" -> "_p". '->" -> "2" drop "!" */
  char *new_name = NULL;
  int len, i, j;
  len = strlen(name);
  if (len > 0)
    {
      new_name = (char *)calloc(len + 3, sizeof(char)); /* +1 for possible _p, +1 for possible $ */
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
  if (last_name3) free(last_name3);
  last_name3 = last_name2;
  last_name2 = last_name1;
  last_name1 = new_name;
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
  if (last_name3) free(last_name3);
  last_name3 = last_name2;
  last_name2 = last_name1;
  last_name1 = new_name;
  return(new_name);
}

char *xen_scheme_global_variable_to_ruby(const char *name)
{
  /* prepend $ */
  char *new_name;
  new_name = scheme_to_ruby(name);
  if (new_name[0] == '_')
    new_name[0] = '$';
  else
    {
      int i, len;
      len = strlen(new_name);
      for (i = len; i > 0; i--)
	new_name[i] = new_name[i - 1];
      new_name[0] = '$';
    }
  return(new_name);
}

/* looks for global variables and constants (functions too?) */
bool xen_rb_defined_p(const char *name)
{
  char *var_name = scheme_to_ruby(name);
  char buf[128];

  if (var_name[0] == '$')
    sprintf(buf, "defined? %s", var_name);
  else sprintf(buf, "defined? $%s", var_name);

  if (XEN_EVAL_C_STRING(buf) != Qnil)
    return(true);
  else
    {
      var_name[0] = toupper(var_name[0]);
      return(rb_const_defined(rb_cObject, rb_intern(var_name)));
    }
}

#ifndef RARRAY_PTR 
  #define RB_ARRAY_PTR(Ary) RARRAY(Ary)->ptr 
  #define RB_ARRAY_LEN(Ary) RARRAY(Ary)->len 
#else 
  #define RB_ARRAY_PTR(Ary) RARRAY_PTR(Ary) 
  #define RB_ARRAY_LEN(Ary) RARRAY_LEN(Ary) 
#endif 
 
int xen_rb_list_length(XEN obj) 
{ 
  if (XEN_VECTOR_P(obj)) 
     return((int)RB_ARRAY_LEN(obj)); 
  if (obj == XEN_EMPTY_LIST) 
    return(0); 
  return(-1); 
} 

/* XEN_CAR, XEN_CADR..., XEN_LIST_REF, XEN_VECTOR_REF */
XEN xen_rb_list_ref(XEN obj, int index)
{
  if (XEN_VECTOR_P(obj))
    return(rb_ary_entry(obj, (long)index));
  return(XEN_EMPTY_LIST);
}

/* XEN_LIST_SET, XEN_VECTOR_SET */
XEN xen_rb_list_set(XEN obj, int index, XEN value)
{
  if (XEN_VECTOR_P(obj))
    rb_ary_store(obj, (long)index, value);
  return(value);
}

char *xen_version(void)
{
  char *buf;
  buf = (char *)calloc(128, sizeof(char));
#if HAVE_SNPRINTF
  snprintf(buf, 128, "Ruby: %s (%s), Xen: %s", 
#else
  sprintf(buf, "Ruby: %s (%s), Xen: %s", 
#endif
#ifdef MUS_RUBY_VERSION
	  MUS_RUBY_VERSION,
	  RUBY_RELEASE_DATE,
#else
	  XEN_TO_C_STRING(XEN_EVAL_C_STRING("RUBY_VERSION")),
	  XEN_TO_C_STRING(XEN_EVAL_C_STRING("RUBY_RELEASE_DATE")),
#endif
	  XEN_VERSION);
  return(buf);
}

#if HAVE_READLINE
  #include <readline/readline.h>
  #include <readline/history.h>
#endif

static XEN xen_rb_report_error(XEN nada, XEN err_info)
{
  /* backtrace info: */
  /*    return rb_funcall(err_info, rb_intern("backtrace"), 0); */
  /* which can be an array of strings */

  fprintf(stderr,"error: %s\n", XEN_AS_STRING(err_info));
  return(XEN_FALSE);
}

static char *rb_prompt = NULL;

static XEN xen_rb_rep(XEN ig)
{
  XEN val;
  char *str;
#if HAVE_READLINE
  char *line_read = NULL;
  line_read = readline(rb_prompt);
  if ((line_read) && (*line_read))
    {
      add_history(line_read);
      val = xen_rb_eval_string_with_error(line_read);
      str = XEN_AS_STRING(val);
      fprintf(stdout, "%s\n", (str) ? str : "nil");
      free(line_read);
      line_read = NULL;
    }
#else
  int size = 512;
  char **buffer = NULL;
  buffer = (char **)calloc(1, sizeof(char *));
  buffer[0] = (char *)calloc(size, sizeof(char));
  fprintf(stdout, rb_prompt);
#if HAVE_GETLINE
  getline(buffer, &size, stdin);
#else
  fgets(buffer[0], size, stdin);
#endif
  val = xen_rb_eval_string_with_error(buffer[0]);
  str = XEN_AS_STRING(val);
  fprintf(stdout, "%s\n", (str) ? str : "nil");
  free(buffer[0]);
  free(buffer);
#endif
  return(ig);
}

void xen_rb_repl_set_prompt(const char *prompt)
{
  if (rb_prompt) free(rb_prompt);
  rb_prompt = strdup(prompt);
}

static XEN xen_rb_rescue(XEN val)
{
  if (!rb_prompt) rb_prompt = strdup(">");
  return(rb_rescue(XEN_PROCEDURE_CAST xen_rb_rep,
		   XEN_FALSE,
		   XEN_PROCEDURE_CAST xen_rb_report_error,
		   XEN_FALSE));
}

#ifndef HAVE_RB_ERRINFO
XEN rb_errinfo(void)
{
  return ruby_errinfo;
}
#endif

void xen_repl(int argc, char **argv)
{
  while (true)
    {
      int status = 0;
      rb_protect(XEN_VALUE_ARG_PROCEDURE_CAST xen_rb_rescue,
		 XEN_FALSE,
		 &status);
      if (status != 0)
	{
	  fprintf(stderr, "%s\n", XEN_AS_STRING(rb_errinfo()));
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
    return(XEN_TO_STRING(rb_errinfo()));
  return(res);
}

XEN xen_rb_load_file_with_error(XEN file)
{
  int status = 0;
  rb_load_protect(file, 0, &status);
  if (status != 0)
    return(XEN_TO_STRING(rb_errinfo()));
  return(XEN_TRUE);
}

XEN xen_rb_add_to_load_path(char *path)
{
  extern VALUE rb_load_path;
  XEN rpath;
  rpath = rb_str_new2(path);
  if (XEN_FALSE_P(rb_ary_includes(rb_load_path, rpath)))
    rb_ary_unshift(rb_load_path, rpath);
  return(XEN_FALSE);
}

static char *lstbuf = NULL;
static char *xen_rb_list_to_s(XEN lst)
{
  int i, len;
  if (lstbuf == NULL) 
    lstbuf = (char *)calloc(512, sizeof(char));
  else lstbuf[0] = '\0';
  len = XEN_LIST_LENGTH(lst);
  for (i = 0; i < len; i++)
    {
      strcat(lstbuf, XEN_AS_STRING(XEN_LIST_REF(lst, i)));
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

int xen_rb_required_args(XEN val)
{
  int args;
  args = XEN_TO_C_INT(val);
  if (args == -1) return(1); 
  if (args < 0) return(abs(args + 1));
  return(args);
}

XEN xen_rb_obj_as_string(XEN obj)
{
  int status = 0;
  XEN result;
  result = rb_protect(XEN_VALUE_ARG_PROCEDURE_CAST rb_obj_as_string,
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
  val = rb_protect(XEN_VALUE_ARG_PROCEDURE_CAST xen_rb_apply_1,
		   XEN_LIST_2(func, args),
		   &status);
  if (status != 0)
    return(XEN_TO_STRING(rb_errinfo()));
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
  val = rb_protect(XEN_VALUE_ARG_PROCEDURE_CAST xen_rb_funcall_0_inner,
		   func,
		   &status);
  if (status != 0)
    return(XEN_TO_STRING(rb_errinfo()));
  return(val);
}

#if (!HAVE_RB_ARY_DUP)
XEN xen_rb_copy_list(XEN val) 
{ 
  /* if this is considered bad form, we could fall back on flatten (rb_ary_dup?) */ 
  long len, i; 
  VALUE collect; 
  len = RB_ARRAY_LEN(val); 
  collect = rb_ary_new2(len); 
  for (i = 0; i < len; i++) 
    RB_ARRAY_PTR(collect)[i] = RB_ARRAY_PTR(val)[i]; 
  RB_ARRAY_LEN(collect) = len; 
  return(collect); 
} 
#endif

XEN xen_rb_str_new2(char *arg)
{
  return(rb_str_new2((arg) ? arg : ""));
}

double xen_rb_to_c_double_or_else(XEN a, double b) 
{
  return(XEN_NUMBER_P(a) ? NUM2DBL(a) : b);
}

int xen_rb_to_c_int_or_else(XEN a, int b) 
{
  if (XEN_INTEGER_P(a)) return(FIX2INT(a));
  if (XEN_NUMBER_P(a)) return((int)(NUM2DBL(a)));
  return(b);
}

/* class Hook */
 
static XEN xen_rb_cHook;

static XEN hook_alloc(XEN klass)
{
  return(Data_Wrap_Struct(klass, 0, 0, 0));
}

#define XEN_CLASS_HOOK_P(Arg)              rb_obj_is_kind_of(Arg, xen_rb_cHook)

bool xen_rb_hook_p(XEN obj)
{
  return(XEN_CLASS_HOOK_P(obj));
}

bool xen_rb_hook_empty_p(XEN obj) 
{ 
  if (XEN_CLASS_HOOK_P(obj)) 
    return(RB_ARRAY_LEN(rb_iv_get(obj, "@procs")) == 0); 
  return(true); 
} 

/*
 * @name = "$name_of_hook"
 * @arity = arity of procedure(s),         default 0
 * @procs = [["named proc1", proc1], ...]
 */
static XEN xen_rb_hook_initialize(int argc, XEN *argv, XEN hook)
{
  XEN name, arity, help;
  rb_scan_args(argc, argv, "12", &name, &arity, &help);
  XEN_ASSERT_TYPE(XEN_STRING_P(name) || XEN_SYMBOL_P(name), name, XEN_ARG_1, c__FUNCTION__, "a char* or symbol");
  if (XEN_SYMBOL_P(name))
    name = XEN_SYMBOL_TO_STRING(name);
  if (arity != Qnil)
    {
      XEN_ASSERT_TYPE(XEN_INTEGER_P(arity), arity, XEN_ARG_2, c__FUNCTION__, "an integer");
    }
  else arity = INT2NUM(0);
  if (help != Qnil)
    {
      XEN_ASSERT_TYPE(XEN_STRING_P(help), help, XEN_ARG_3, c__FUNCTION__, "a char*");
      XEN_SET_OBJECT_HELP(name, help);
    }
  else help = rb_str_new2("");
  rb_iv_set(hook, "@name", name);
  rb_iv_set(hook, "@arity", arity);
  rb_iv_set(hook, "@procs", rb_ary_new());
  return(hook);
}

/*
 * To create a simple hook in C, see xen.h, XEN_DEFINE_SIMPLE_HOOK.
 * To create a global hook variables, see xen_rb_create_hook() below.
 */
XEN xen_rb_hook_c_new(char *name, int arity, char *help)
{
  XEN args[3];
  args[0] = C_TO_XEN_STRING(name);
  args[1] = C_TO_XEN_INT(arity);
  args[2] = C_TO_XEN_STRING(help);
  return(xen_rb_hook_initialize(3, args, hook_alloc(xen_rb_cHook)));
}

/*
  RUBY_RELEASE_DATE < "2004-03-18" ? old : new

  lambda do         end.arity 	    -1     0 !!!
  lambda do ||      end.arity 	     0     0
  lambda do |a|     end.arity 	    -1     1 !!!
  lambda do |*a|    end.arity 	    -1    -1
  lambda do |a, b|  end.arity 	     2     2
  lambda do |a, *b| end.arity 	    -2    -2
  etc.
*/

#ifdef MUS_RUBY_VERSION
  #define XEN_RUBY_RELEASE_DATE  RUBY_RELEASE_DATE
#else
  #define XEN_RUBY_RELEASE_DATE  XEN_TO_C_STRING(XEN_EVAL_C_STRING("RUBY_RELEASE_DATE"))
#endif

#define RUBY_NEW_ARITY_DATE   "2004-03-18"
#define OLD_RUBY_ARITY()      (strcmp(XEN_RUBY_RELEASE_DATE, RUBY_NEW_ARITY_DATE) < 0)
#define NEW_RUBY_ARITY()      (strcmp(XEN_RUBY_RELEASE_DATE, RUBY_NEW_ARITY_DATE) >= 0)

bool xen_rb_arity_ok(int rargs, int args)
{
  if (OLD_RUBY_ARITY())
    {
      if ((rargs >= 2) || (rargs == 0))
	return(rargs == args);
      else if (rargs <= -2)
	return(abs(rargs) <= args);
      else			/* rargs -1 remains (no 1 exists) */
	return((args == 1) || (args == 0) || (args == -1));
    }
  else /* NEW_RUBY_ARITY */
    return((rargs >= 0) ? (rargs == args) : (abs(rargs) <= args));
}
 
static XEN xen_rb_hook_add_hook(int argc, XEN *argv, XEN hook)
{
  XEN name, func;
  int args;
  args = XEN_TO_C_INT(rb_iv_get(hook, "@arity"));
  rb_scan_args(argc, argv, "1&", &name, &func);
  XEN_ASSERT_TYPE(XEN_STRING_P(name), name, XEN_ARG_1, c__FUNCTION__, "a char*");
  XEN_ASSERT_TYPE(XEN_PROCEDURE_P(func) && xen_rb_arity_ok(XEN_TO_C_INT(XEN_ARITY(func)), args),
		  func, XEN_ARG_2, c__FUNCTION__, "a procedure");
  rb_ary_push(rb_iv_get(hook, "@procs"), rb_ary_new3(2, name, func));
  return(hook);
}

static XEN xen_rb_hook_remove_hook(XEN hook, XEN name)
{
  XEN ary;
  ary = rb_iv_get(hook, "@procs");
  return(rb_ary_delete(ary, rb_ary_assoc(ary, name)));
}

XEN xen_rb_hook_reset_hook(XEN hook)
{
  if (XEN_CLASS_HOOK_P(hook))
    rb_ary_clear(rb_iv_get(hook, "@procs"));
  return(hook);
}

static XEN xen_rb_hook_names(XEN hook)
{
  XEN ary, ret = Qnil;
  long len;
  ary = rb_iv_get(hook, "@procs");
  len = RB_ARRAY_LEN(ary); 
  if (len > 0)
    {
      long i;
      ret = rb_ary_new2(len);
      for (i = 0; i < len; i++)
	rb_ary_store(ret, i, XEN_VECTOR_REF(XEN_VECTOR_REF(ary, i), 0));
    }
  return(ret);
}

XEN xen_rb_hook_to_a(XEN hook)
{
  XEN ret = Qnil;
  if (XEN_CLASS_HOOK_P(hook))
    {
      XEN ary;
      long len;
      ary = rb_iv_get(hook, "@procs");
      len = XEN_LIST_LENGTH(ary);
      if (len > 0)
	{
	  long i;
	  ret = rb_ary_new2(len);
	  for (i = 0; i < len; i++)
	    rb_ary_store(ret, i, XEN_VECTOR_REF(XEN_VECTOR_REF(ary, i), 1));
	}
    }
  return(ret);
}

static XEN xen_rb_hook_run_hook(XEN hook)
{
  if (RB_ARRAY_LEN(rb_iv_get(hook, "@procs"))) 
    rb_ary_each(xen_rb_hook_to_a(hook));
  return(hook);
}

/*
 * Calls all hook-procedures but returns only the last result; use
 * $var_hook.run_hook { |prc| ret << prc.call(*args) } for collecting
 * results.
 */
static XEN xen_rb_hook_call(int argc, XEN *argv, XEN hook)
{
  XEN result = Qnil, rest, procs;
  rb_scan_args(argc, argv, "*", &rest);
  procs = xen_rb_hook_to_a(hook);
  if (procs != Qnil)
    {
      long i;
      for (i = 0; i < RB_ARRAY_LEN(procs); i++) 
	result = xen_rb_apply(rb_ary_entry(procs, i), rest);
    }
  return(result);
}

static XEN xen_rb_hook_is_empty_p(XEN hook)
{
  return(C_TO_XEN_BOOLEAN(RB_ARRAY_LEN(rb_iv_get(hook, "@procs")) == 0)); 
}
 
static XEN xen_rb_hook_length(XEN hook)
{
  return(C_TO_XEN_INT(RB_ARRAY_LEN(rb_iv_get(hook, "@procs")))); 
}

static XEN xen_rb_hook_name(XEN hook)
{
  return(rb_iv_get(hook, "@name"));
}

static XEN xen_rb_hook_describe(XEN hook)
{
  return(XEN_OBJECT_HELP(xen_rb_hook_name(hook)));
}

static XEN xen_rb_hook_arity(XEN hook)
{
  return(rb_iv_get(hook, "@arity"));
}

static XEN xen_rb_hook_inspect(XEN hook)
{
  XEN str = rb_str_new2("#<Hook name: ");
  rb_str_append(str, rb_inspect(rb_iv_get(hook, "@name")));
  rb_str_cat2(str, ", arity: ");
  rb_str_append(str, rb_inspect(rb_iv_get(hook, "@arity")));
  rb_str_cat2(str, ", procs[");
  rb_str_append(str, rb_inspect(xen_rb_hook_length(hook)));
  rb_str_cat2(str, "]: ");
  rb_str_append(str, rb_inspect(xen_rb_hook_names(hook)));
  rb_str_cat2(str, ">");
  return(str);
}    

/* bil -- added xen_rb_create_hook for XEN_DEFINE_HOOK in xen.h, 13-Jun-05 --
 *   seems to work, but I'm guessing, especially the rb_gv_set line.
 *   I can't use rb_define_variable here, as in the old version, because it takes a pointer
 *   to the new variable, which in this case is a local variable => segfault.
 */
XEN xen_rb_create_hook(char *name, int arity, char *help)
{
  XEN var, hook_name;
  var = xen_rb_hook_c_new(xen_scheme_global_variable_to_ruby(name), arity, help);
  hook_name = xen_rb_hook_name(var);
  rb_gv_set(XEN_TO_C_STRING(hook_name), var);   
  return(var);
}


/*
 * make_hook(name, arity = 0, help = "", hook_name = nil, &func)
 *
 * make_hook("var_hook")
 *   == $var_hook = Hook.new("var_hook")
 * make_hook("var_hook", 1)
 *   == $var_hook = Hook.new("var_hook", 1)
 * make_hook("var_hook", 1, "help $var_hook")
 *   == $var_hook = Hook.new("var_hook", 1, "help $var_hook")
 * 
 * make_hook("var_hook", 1, "help $var_hook", "1st proc") do |a| ... end
 *   == $var_hook = Hook.new("var_hook", 1, "help $var_hook")
 *      $var_hook.add_hook!("1st proc") do |a| ... end
 */

#if USE_SND
void snd_rb_raise(XEN type, XEN info); /* XEN_ERROR */
#endif

#ifndef RSTRING_LEN 
  #define RB_STR_LEN(str)                RSTRING(str)->len 
#else 
  #define RB_STR_LEN(str)                RSTRING_LEN(str) 
#endif 

static XEN xen_rb_make_hook(int argc, XEN *argv, XEN klass)
{
  XEN hook, name;
  if (argc > 0 && argc < 4)
    {
      hook = xen_rb_hook_initialize(argc, argv, hook_alloc(xen_rb_cHook));
      if (rb_block_given_p())
	{
	  argv[0] = rb_str_new2("");
	  xen_rb_hook_add_hook(1, argv, hook);
	}
    }
  else if (argc == 4 && rb_block_given_p())
    {
      hook = xen_rb_hook_initialize(3, argv, hook_alloc(xen_rb_cHook));
      argv[0] = argv[3];
      xen_rb_hook_add_hook(1, argv, hook);
    }
  else XEN_ERROR(XEN_ERROR_TYPE("wrong-number-of-args"),
		 XEN_LIST_1(C_TO_XEN_STRING("make_hook(name, arity=0, help=\"\", hook_name=\"\", &func)")));
  name = xen_rb_hook_name(hook);
  if (XEN_TO_C_CHAR(name) != '$') 
    name = C_TO_XEN_STRING(xen_scheme_global_variable_to_ruby(XEN_TO_C_STRING(name))); 
  XEN_ASSERT_TYPE(RB_STR_LEN(name) >= 2, name, XEN_ARG_1, c__FUNCTION__, "a char*, len >= 2"); 
  return(rb_gv_set(XEN_TO_C_STRING(name), hook)); 
}

static XEN xen_rb_is_hook_p(XEN klass, XEN obj)
{
  return(C_TO_XEN_BOOLEAN(XEN_CLASS_HOOK_P(obj)));
}

/*
 * Hook.new(name, arity = 0, help = "")
 *
 * $my_hook = Hook.new("my_hook", 2, "info of my_hook")
 * $my_hook.add_hook!("1st proc") do |a, b| ... end
 *     or make_hook("my_hook", 2, "info of my_hook", "1st proc") do |a, b| ... end
 * 
 * $my_hook.add_hook!("2nd proc") do |a, b| ... end
 * $my_hook.inspect   --> #<Hook name: "$my_hook", arity: 2, procs[2]: ["1st proc", "2nd proc"]>
 *
 * ret = 0
 * $my_hook.run_hook do |prc| ret = prc.call(ret, 2) end
 *
 * $my_hook.help      --> info of my_hook
 * $my_hook.remove_hook!("1st proc")
 * $my_hook.inspect   --> #<Hook name: "$my_hook", arity: 2, procs[1]: ["2nd proc"]>
 * 
 * $my_hook.remove_hook!("2nd proc")
 * $my_hook.inspect   --> #<Hook name: "$my_hook", arity: 2, procs[0]: nil>
 */

#if (!HAVE_RB_DEFINE_ALLOC_FUNC)
static XEN xen_rb_new(int argc, XEN *argv, XEN klass)
{
  XEN hook = hook_alloc(klass);
  rb_obj_call_init(hook, argc, argv);
  return(hook);
}
#endif

static XEN rb_object_properties = XEN_FALSE;

#define S_property       "property"
#define S_set_property   "set_property"
#define S_properties     "properties"

XEN rb_property(XEN obj, XEN key)
{
#define H_property S_property "(obj, key)  \
if key exists, return obj's value (maybe nil) associated with key otherwise false"
  XEN props = XEN_FALSE;
  
  if (XEN_FALSE_P(rb_object_properties))
    return(XEN_FALSE);

  props = rb_hash_aref(rb_object_properties, obj);

  if (XEN_FALSE_P(props) || props == Qnil)
    return(XEN_FALSE);
  else
    return(rb_hash_aref(props, key));
}

XEN rb_set_property(XEN obj, XEN key, XEN value)
{
#define H_set_property S_set_property "(obj, key, value)  \
set key-value pair for obj and return value"
  XEN props = XEN_FALSE;

  if (XEN_FALSE_P(rb_object_properties))
    {
      rb_object_properties = rb_hash_new();
      XEN_PROTECT_FROM_GC(rb_object_properties);
    }
  else
    props = rb_hash_aref(rb_object_properties, obj);
  
  if (XEN_FALSE_P(props) || props == Qnil)
    props = rb_hash_new();
  
  rb_hash_aset(props, key, value);
  rb_hash_aset(rb_object_properties, obj, props);
  return(value);
}

XEN rb_properties(void)
{
#define H_properties S_properties "()  return all properties of rb_object_properties (a hash)"
  return(rb_object_properties);
}

XEN_ARGIFY_1(g_get_help_w, g_get_help);
XEN_NARGIFY_2(g_add_help_w, g_add_help);
XEN_NARGIFY_3(g_set_property_w, rb_set_property);
XEN_NARGIFY_2(g_property_w, rb_property);
XEN_NARGIFY_0(g_properties_w, rb_properties);

static bool hook_inited = false;

void Init_Hook(void)
{
  if (hook_inited) return;
  hook_inited = true;
 
  xen_rb_cHook = rb_define_class("Hook", rb_cObject);
  rb_include_module(xen_rb_cHook, rb_mEnumerable);
#if HAVE_RB_DEFINE_ALLOC_FUNC
  rb_define_alloc_func(xen_rb_cHook, hook_alloc);
#else
  rb_define_singleton_method(xen_rb_cHook, "new", XEN_PROCEDURE_CAST xen_rb_new, -1);
#endif
    
  rb_define_method(xen_rb_cHook, "initialize", XEN_PROCEDURE_CAST xen_rb_hook_initialize, -1);
  rb_define_method(xen_rb_cHook, "add_hook!", XEN_PROCEDURE_CAST xen_rb_hook_add_hook, -1);
  rb_define_method(xen_rb_cHook, "remove_hook!", XEN_PROCEDURE_CAST xen_rb_hook_remove_hook, 1);
  rb_define_method(xen_rb_cHook, "reset_hook!", XEN_PROCEDURE_CAST xen_rb_hook_reset_hook, 0);
  rb_define_alias(xen_rb_cHook, "clear", "reset_hook!");
  rb_define_method(xen_rb_cHook, "to_a", XEN_PROCEDURE_CAST xen_rb_hook_to_a, 0);
  rb_define_method(xen_rb_cHook, "run_hook", XEN_PROCEDURE_CAST xen_rb_hook_run_hook, 0);
  rb_define_alias(xen_rb_cHook, "each", "run_hook");
  rb_define_method(xen_rb_cHook, "call", XEN_PROCEDURE_CAST xen_rb_hook_call, -1);
  rb_define_method(xen_rb_cHook, "length", XEN_PROCEDURE_CAST xen_rb_hook_length, 0);
  rb_define_alias(xen_rb_cHook, "size", "length");
  rb_define_method(xen_rb_cHook, "empty?", XEN_PROCEDURE_CAST xen_rb_hook_is_empty_p, 0);
  rb_define_method(xen_rb_cHook, "name", XEN_PROCEDURE_CAST xen_rb_hook_name, 0);
  rb_define_method(xen_rb_cHook, "arity", XEN_PROCEDURE_CAST xen_rb_hook_arity, 0);
  rb_define_method(xen_rb_cHook, "describe", XEN_PROCEDURE_CAST xen_rb_hook_describe, 0);
  rb_define_alias(xen_rb_cHook, "help", "describe");
  rb_define_alias(xen_rb_cHook, "documentation", "describe");
  rb_define_method(xen_rb_cHook, "inspect", XEN_PROCEDURE_CAST xen_rb_hook_inspect, 0);
  
  rb_define_global_function("make_hook", XEN_PROCEDURE_CAST xen_rb_make_hook, -1);
  rb_define_global_function("hook?", XEN_PROCEDURE_CAST xen_rb_is_hook_p, 1);

  XEN_DEFINE_PROCEDURE(S_get_help,             g_get_help_w,             0, 1, 0, H_get_help);
  XEN_DEFINE_PROCEDURE(S_add_help,             g_add_help_w,             2, 0, 0, H_add_help);

  XEN_DEFINE_PROCEDURE(S_set_property,         g_set_property_w,         3, 0, 0, H_set_property);
  XEN_DEFINE_PROCEDURE(S_property,             g_property_w,             2, 0, 0, H_property);
  XEN_DEFINE_PROCEDURE(S_properties,           g_properties_w,           0, 0, 0, H_properties);
}

/* end of class Hook */

#endif

/* ------------------------------ FORTH ------------------------------ */

#if HAVE_FORTH

char *xen_version(void)
{
  return(fth_format("Fth: %s, Xen: " XEN_VERSION, FTH_VERSION));
}

void xen_gc_mark(XEN val)
{
  fth_gc_mark(val);
}

/*
 * A simple interpreter:
 *
 *  #include <xen.h>
 *  
 *  int main(int argc, char **argv)
 *  {
 *    xen_repl(argc, argv);
 *    return(0);
 *  }
 *
 * linking requires xen.o and -lfth -lm
 */

void xen_repl(int argc, char **argv)
{
  fth_repl(argc, argv);
}

static ficlWord *snd_print_xt;
static ficlWord *snd_exit_xt;
static bool fth_listener_p;
extern bool fth_printing_p;
extern bool fth_new_eval;

static void fth_snd_print(ficlVm *vm, char *msg)
{
  if (fth_listener_p)
    {
      if (!snd_print_xt)
	snd_print_xt = ficlSystemLookup(FTH_FICL_VM()->callback.system, "snd-print");
      push_cstring(vm, msg);
      ficlVmExecuteXT(vm, snd_print_xt);
      ficlStackDrop(vm->dataStack, 1);
    }
  else
    {
      fputs(msg, stdout);
      fflush(stdout);
    }
}

static void fth_snd_exit(int n)
{
  ficlVm *vm = FTH_FICL_VM();

  if (!snd_exit_xt)
    snd_exit_xt = ficlSystemLookup(vm->callback.system, "snd-exit");
  ficlStackPushInteger(vm->dataStack, n);
  ficlVmExecuteXT(vm, snd_exit_xt);
  ficlStackDrop(vm->dataStack, 1);
}

/* called in inf-snd.el (inf-snd-comint-snd-send) */
static void fth_emacs_eval(FTH line_in)
{
#define h_emacs_eval "( line -- )"
  int status;
  const char *old_file = fth_current_file;
  long old_line = fth_current_line;
  
  if (!FTH_STRING_P(line_in)) return;
  fth_current_file = "emacs-eval";
  fth_current_line = 1;
  fth_listener_p = false;
  fth_new_eval = false;

  if ((status = fth_catch_eval(fth_string_ref(line_in))) == FTH_BYE) 
    fth_exit(EXIT_SUCCESS);
  else
    {
      ficlVm *vm = FTH_FICL_VM();
      long i, len = FTH_STACK_DEPTH(vm);
	
      for (i = len - 1; i >= 0; i--)
	{
	  ficlStackRoll(vm->dataStack, i);
	  fth_printf(fth_to_c_string(fth_pop_ficl_cell(vm)));
	  if (i > 0) fth_printf(" ");
	}
    }

  if (fth_printing_p) fth_printf(" ");
  fth_printf("ok\n");
  fth_current_file = old_file;
  fth_current_line = old_line;
  fth_printing_p = false;
  fth_listener_p = true;
}

off_t xen_to_c_off_t_or_else(XEN obj, off_t fallback)
{
  if (XEN_NUMBER_P(obj))
    return(fth_long_long_ref(obj));
  return(fallback);
}

off_t xen_to_c_off_t(XEN obj)
{
  return(fth_long_long_ref(obj));
}

XEN c_to_xen_off_t(off_t obj)
{
  return(fth_make_long_long(obj));
}

void xen_initialize(void)
{
  fth_printing_p = false;
  fth_listener_p = true;
  fth_print_hook = fth_snd_print;
  fth_exit_hook  = fth_snd_exit;
  fth_init();
  fth_define_void_procedure("emacs-eval", fth_emacs_eval, 1, 0, 0, h_emacs_eval);
}

#endif 	/* HAVE_FORTH */


/* ------------------------------ GAUCHE ------------------------------ */

#if HAVE_GAUCHE

char *xen_version(void)
{
  char *buf;
  buf = (char *)calloc(64, sizeof(char));
#if HAVE_SNPRINTF
  snprintf(buf, 64, "Gauche: %s, Xen: %s", GAUCHE_VERSION, XEN_VERSION);
#else
  sprintf(buf, "Gauche: %s, Xen: %s", GAUCHE_VERSION, XEN_VERSION);
#endif
  return(buf);
}

off_t xen_to_c_off_t_or_else(XEN obj, off_t fallback)
{
  if (XEN_OFF_T_P(obj))
    return(Scm_GetInteger64(obj));
  if (XEN_NUMBER_P(obj))
    return((off_t)XEN_TO_C_DOUBLE(obj));
  return(fallback);
}

off_t xen_to_c_off_t(XEN obj)
{
  return(Scm_GetInteger64(obj));
}

XEN c_to_xen_off_t(off_t val)
{
  return(Scm_MakeInteger64(val));
}

int xen_to_c_int_or_else(XEN obj, int fallback)
{
  if (XEN_NUMBER_P(obj))
    return(XEN_TO_C_INT(obj));
  return(fallback);
}

double xen_to_c_double(XEN a) 
{
  double num = 0.0;
  if (SCM_REALP(a))
    return(Scm_GetDouble(a));
  if (SCM_INTP(a))
    return((double)Scm_GetInteger(a));
  if (SCM_BIGNUMP(a))
    return((double)Scm_GetInteger64(a));
  return(num);
}

void xen_repl(int argc, char **argv)
{
  XEN_EVAL_C_STRING("(if (not (defined? 'gauche-repl-prompt)) (define gauche-repl-prompt \">\"))");
  XEN_EVAL_C_STRING("(read-eval-print-loop #f #f #f (lambda () (display gauche-repl-prompt (current-output-port)) (flush (current-output-port))))");
}

static XEN g_defined_p(XEN sym)
{
  /* "defined?" is absolutely needed from the start */
  return(C_TO_XEN_BOOLEAN(Scm_FindBinding(Scm_UserModule(), SCM_SYMBOL(sym), false) != NULL));
}

void xen_gc_mark(XEN val)
{
}

void xen_gauche_load_args(XEN *args, int incoming_args, int args_size, XEN *arg_list)
{
  int i, len;
  XEN list;
  for (i = 0; i < incoming_args - 1; i++) args[i] = arg_list[i];
  if (incoming_args <= args_size)
    {
      int j;
      list = arg_list[incoming_args - 1];
      len = XEN_LIST_LENGTH(list);
      for (i = incoming_args - 1, j = 0; (j < len) && (i < args_size); i++, j++) args[i] = XEN_LIST_REF(list, j);
      for (i = incoming_args - 1 + len; i < args_size; i++) args[i] = XEN_UNDEFINED;
    }
}

static XEN help_hash_table = XEN_FALSE;

XEN xen_gauche_help(XEN sym)
{
  ScmHashEntry *e = NULL;
  if (XEN_STRING_P(sym))
    e = Scm_HashTableGet(SCM_HASH_TABLE(help_hash_table), SCM_INTERN(XEN_TO_C_STRING(sym)));
  else e = Scm_HashTableGet(SCM_HASH_TABLE(help_hash_table), sym);
  if (e) return((XEN)(e->value));
  return(XEN_FALSE);
}

void xen_gauche_set_help(XEN sym, const char *help)
{
  if (XEN_STRING_P(sym))
    Scm_HashTableAdd(SCM_HASH_TABLE(help_hash_table), SCM_INTERN(XEN_TO_C_STRING(sym)), (help) ? C_TO_XEN_STRING(help) : XEN_FALSE);
  else Scm_HashTableAdd(SCM_HASH_TABLE(help_hash_table), sym, (help) ? C_TO_XEN_STRING(help) : XEN_FALSE);
}

XEN xen_gauche_define_constant(const char *name, int value, const char *help)
{
  XEN obj, sym;
  sym = SCM_INTERN(name);
  obj = Scm_DefineConst(SCM_MODULE(Scm_UserModule()), SCM_SYMBOL(sym), C_TO_XEN_INT(value));
  xen_gauche_set_help(sym, help);
  return(obj);
}

#ifndef __cplusplus
void xen_gauche_define_procedure(char *Name, XEN (*Func)(), int ReqArg, int OptArg, int RstArg, char *Doc)
{
  XEN proc, sym;
  if (RstArg > 0)
    OptArg = 24; /* vargify but I think 24 args will handle most cases */
  proc = Scm_MakeSubr(Func, NULL, ReqArg, OptArg, SCM_MAKE_STR_COPYING(Name));
  sym = SCM_INTERN(Name);
  xen_gauche_set_help(sym, Doc);
  Scm_Define(SCM_MODULE(Scm_UserModule()), SCM_SYMBOL(sym), proc);
}

void xen_gauche_define_procedure_with_setter(char *get_name, XEN (*get_func)(), char *get_help, XEN (*set_func)(), 
					     int get_req, int get_opt, int set_req, int set_opt)
{
  XEN proc, set_proc, sym;
  proc = Scm_MakeSubr(get_func, NULL, get_req, get_opt, SCM_MAKE_STR_COPYING(get_name));
  sym = SCM_INTERN(get_name);
  xen_gauche_set_help(sym, get_help);
  Scm_Define(SCM_MODULE(Scm_UserModule()), SCM_SYMBOL(sym), proc);
  set_proc = Scm_MakeSubr(set_func, NULL, set_req, set_opt, SCM_MAKE_STR_COPYING(get_name));
  Scm_SetterSet((ScmProcedure *)proc, (ScmProcedure *)set_proc, false);
}

void xen_gauche_define_procedure_with_reversed_setter(char *get_name, XEN (*get_func)(), char *get_help, XEN (*set_func)(), XEN (*reversed_set_func)(), 
						      int get_req, int get_opt, int set_req, int set_opt)
{
  XEN proc, set_proc, sym;
  proc = Scm_MakeSubr(get_func, NULL, get_req, get_opt, SCM_MAKE_STR_COPYING(get_name));
  sym = SCM_INTERN(get_name);
  xen_gauche_set_help(sym, get_help);
  Scm_Define(SCM_MODULE(Scm_UserModule()), SCM_SYMBOL(sym), proc);
  set_proc = Scm_MakeSubr(reversed_set_func, NULL, set_req, set_opt, SCM_MAKE_STR_COPYING(get_name));
  Scm_SetterSet((ScmProcedure *)proc, (ScmProcedure *)set_proc, false);
}
#else
void xen_gauche_define_procedure(char *Name, 
				 ScmHeaderRec* (*Func)(ScmHeaderRec**, int, void*), 
				 int ReqArg, int OptArg, int RstArg, char *Doc)
{
  XEN proc, sym;
  if (RstArg > 0)
    OptArg = 24; /* vargify but I think 24 args will handle most cases */
  proc = Scm_MakeSubr(Func, NULL, ReqArg, OptArg, SCM_MAKE_STR_COPYING(Name));
  sym = SCM_INTERN(Name);
  xen_gauche_set_help(sym, Doc);
  Scm_Define(SCM_MODULE(Scm_UserModule()), SCM_SYMBOL(sym), proc);
}

void xen_gauche_define_procedure_with_reversed_setter(char *get_name, 
						      ScmHeaderRec* (*get_func)(ScmHeaderRec**, int, void*), 
						      char *get_help, 
						      ScmHeaderRec* (*set_func)(ScmHeaderRec**, int, void*), 
						      ScmHeaderRec* (*reversed_set_func)(ScmHeaderRec**, int, void*), 
						      int get_req, int get_opt, int set_req, int set_opt)
{
  XEN proc, set_proc, sym;
  proc = Scm_MakeSubr(get_func, NULL, get_req, get_opt, SCM_MAKE_STR_COPYING(get_name));
  sym = SCM_INTERN(get_name);
  xen_gauche_set_help(sym, get_help);
  Scm_Define(SCM_MODULE(Scm_UserModule()), SCM_SYMBOL(sym), proc);
  set_proc = Scm_MakeSubr(reversed_set_func, NULL, set_req, set_opt, SCM_MAKE_STR_COPYING(get_name));
  Scm_SetterSet((ScmProcedure *)proc, (ScmProcedure *)set_proc, false);
}

void xen_gauche_define_procedure_with_setter(char *get_name, 
					     ScmHeaderRec* (*get_func)(ScmHeaderRec**, int, void*), 
					     char *get_help, 
					     ScmHeaderRec* (*set_func)(ScmHeaderRec**, int, void*),
					     int get_req, int get_opt, int set_req, int set_opt)
{
  XEN proc, set_proc, sym;
  proc = Scm_MakeSubr(get_func, NULL, get_req, get_opt, SCM_MAKE_STR_COPYING(get_name));
  sym = SCM_INTERN(get_name);
  xen_gauche_set_help(sym, get_help);
  Scm_Define(SCM_MODULE(Scm_UserModule()), SCM_SYMBOL(sym), proc);
  set_proc = Scm_MakeSubr(set_func, NULL, set_req, set_opt, SCM_MAKE_STR_COPYING(get_name));
  Scm_SetterSet((ScmProcedure *)proc, (ScmProcedure *)set_proc, false);
}
#endif

void xen_gauche_list_set_x(XEN Lst, int Loc, XEN Val)
{
  /* modelled on Scm_ListRef in src/list.c */
  int k;
  if (Loc < 0) return;
  for (k = 0; k < Loc; k++)
    {
      if (!SCM_PAIRP(Lst)) return;
      Lst = SCM_CDR(Lst);
    }
  if (!SCM_PAIRP(Lst)) return;
  SCM_SET_CAR(Lst, Val);
}

XEN xen_gauche_load_file(char *file)
{
  Scm_Load(file, 0); /* returns an int, but we want (XEN) error indication */
  /* flags is or of SCM_LOAD_QUIET_NOFILE SCM_LOAD_IGNORE_CODING */
  return(XEN_FALSE);
}

XEN xen_gauche_add_to_load_path(char *path)
{
  if (XEN_FALSE_P(Scm_Member(C_TO_XEN_STRING(path), XEN_LOAD_PATH, SCM_CMP_EQUAL))) /* scheme spec says eq? and eqv? of strings is unspecified */
    Scm_AddLoadPath(path, false);
  return(XEN_FALSE);
}

XEN xen_gauche_object_to_string(XEN obj)
{
  /* return XEN string description of obj */
  ScmObj ostr;
  if (XEN_STRING_P(obj))
    {
      char *str, *newstr;
      int i, j, quotes = 0, len, oldlen;
      str = XEN_TO_C_STRING(obj);
      if (str)
	{
	  XEN result;
	  len = strlen(str);
	  oldlen = len;
	  for (i = 0; i < len; i++)
	    if (str[i] == '"') quotes++;
	  len = len + 2 + quotes;
	  newstr = (char *)calloc(len + 1, sizeof(char));
	  newstr[0] = '"';
	  newstr[len - 1] = '"';
	  for (j = 1, i = 0; i < oldlen; i++)
	    {
	      if (str[i] == '"')
		newstr[j++] = '\\';
	      newstr[j++] = str[i];
	    }
	  result = C_TO_XEN_STRING(newstr);
	  free(newstr);
	  return(result);
	}
      return(C_TO_XEN_STRING("\"\""));
    }
  ostr = Scm_MakeOutputStringPort(true);
  Scm_Write(obj, SCM_OBJ(ostr), true);
  return(Scm_GetOutputString(SCM_PORT(ostr)));
}

void xen_gauche_permanent_object(XEN obj)
{
  /* I can't see how you're supposed to protect something from the gc, so I'll try
   *   simply placing this object under a gensymmed name in the user module??
   */
  Scm_DefineConst(Scm_UserModule(), SCM_SYMBOL(Scm_Gensym(SCM_STRING(C_TO_XEN_STRING("Snd")))), obj);
}

XEN xen_gauche_eval_c_string(char *arg)
{
  XEN result = XEN_FALSE;
  SCM_UNWIND_PROTECT 
    {
#if (!HAVE_SCM_EVALREC)
      result = Scm_EvalCString(arg, SCM_OBJ(Scm_UserModule()));
#else
      result = Scm_EvalRec(Scm_ReadFromCString(arg), SCM_OBJ(Scm_UserModule()));
#endif
    }
  SCM_WHEN_ERROR 
    {
      fprintf(stderr, "Error in %s\n", arg);
      /* SCM_NEXT_HANDLER; */
      /* if this is left in, Snd exits on an error! -- exactly what we're trying to avoid */
    }
  SCM_END_PROTECT;
  return(result);
}

typedef struct {
  XEN_OBJECT_TYPE type;
  void *data;
} smob;

static XEN_OBJECT_TYPE smob_type = 0;
static ScmClass **smob_classes = NULL;
static int smob_classes_size = 0;

XEN xen_gauche_make_object(XEN_OBJECT_TYPE type, void *val, XEN_MARK_OBJECT_TYPE (*protect_func)(XEN obj))
{
  smob *s;
  XEN obj;
  s = (smob *)calloc(1, sizeof(smob));
  s->type = type;
  s->data = val;
  obj = Scm_MakeForeignPointer(smob_classes[type], (void *)s);
  if (protect_func) protect_func(obj);
  return(obj);
}

void *xen_gauche_object_ref(XEN obj)
{
  smob *s;
  s = (smob *)(((ScmForeignPointer *)obj)->ptr);
  if (s) 
    return(s->data);
  return(NULL);
}

XEN_OBJECT_TYPE xen_gauche_new_type(const char *name, ScmClassPrintProc print, ScmForeignCleanupProc cleanup)
{
  XEN_OBJECT_TYPE current_type;
  current_type = smob_type;
  smob_type++;
  if (current_type >= smob_classes_size)
    {
      if (smob_classes_size == 0)
	{
	  smob_classes_size = 8;
	  smob_classes = (ScmClass **)calloc(smob_classes_size, sizeof(ScmClass *));
	}
      else
	{
	  smob_classes_size += 8;
	  smob_classes = (ScmClass **)realloc(smob_classes, smob_classes_size * sizeof(ScmClass *));
	}
    }
  smob_classes[current_type] = Scm_MakeForeignPointerClass(Scm_UserModule(),
							   name,
							   print,
							   cleanup,
							   SCM_FOREIGN_POINTER_KEEP_IDENTITY | SCM_FOREIGN_POINTER_MAP_NULL);
  return(current_type);
}

bool xen_gauche_type_p(XEN obj, XEN_OBJECT_TYPE type)
{
  smob *s;
  if (SCM_FOREIGN_POINTER_P(obj))
    {
      s = SCM_FOREIGN_POINTER_REF(smob *, obj);
      return((s) &&
	     (s->type == type));
    }
  return(false);
}

void xen_gauche_provide(const char *feature)
{
  /* there is no *features* list built-in Gauche!! I've defined one below */
  /*   also Gauche's provide and provided? take strings! so I'll have to encapsulate them */
  char *expr;
  int len;
  Scm_Provide(C_TO_XEN_STRING(feature));
  len = strlen(feature) + 64;
  expr = (char *)calloc(len, sizeof(char));
  snprintf(expr, len, "(set! *features* (cons '%s *features*))", feature);
  XEN_EVAL_C_STRING(expr);
  free(expr);
}

const char *xen_gauche_features(void)
{
  return(XEN_AS_STRING(XEN_EVAL_C_STRING("*features*")));
}

static XEN g_xen_gauche_provide(XEN feature)
{
  if (XEN_SYMBOL_P(feature))
    xen_gauche_provide(XEN_SYMBOL_TO_C_STRING(feature));
  else xen_gauche_provide(XEN_TO_C_STRING(feature));
  return(feature);
}

static XEN g_xen_gauche_provided_p(XEN feature)
{
  if (XEN_SYMBOL_P(feature))
    return(C_TO_XEN_BOOLEAN(Scm_ProvidedP(SCM_OBJ(SCM_SYMBOL_NAME(feature)))));
  return(C_TO_XEN_BOOLEAN(Scm_ProvidedP(feature)));
}

/* hooks */

typedef struct {
  int arity;
  XEN functions;
} ghook;

static XEN_OBJECT_TYPE ghook_tag;

static ghook *make_ghook(int arity)
{
  ghook *hook;
  hook = (ghook *)calloc(1, sizeof(ghook));
  hook->arity = arity;
  hook->functions = XEN_EMPTY_LIST;
  return(hook);
}

static void free_ghook(ghook *hook)
{
  if (hook)
    {
      hook->functions = XEN_FALSE;
      free(hook);
    }
}

XEN_MAKE_OBJECT_FREE_PROCEDURE(ghook, free_hook, free_ghook)

static char *hook_to_string(ghook *hook)
{
  if (hook)
    {
      int len;
      char *functions, *str;
      functions = XEN_AS_STRING(hook->functions);
      len = 64 + strlen(functions);
      str = (char *)calloc(len, sizeof(char));
      snprintf(str, len, "<hook arity: %d, hooks: %s>", hook->arity, functions);
      return(str);
    }
  return(NULL);
}

#define FREE free
XEN_MAKE_OBJECT_PRINT_PROCEDURE(ghook, print_hook, hook_to_string)

static XEN_MARK_OBJECT_TYPE mark_ghook(XEN obj)
{
  ghook *hook;
  hook = (ghook *)XEN_OBJECT_REF(obj);
  Scm_ForeignPointerAttrSet(SCM_FOREIGN_POINTER(obj), SCM_INTERN("functions"), hook->functions);
  return(XEN_FALSE);
}

static int ghook_arity(ghook *hook) {return(hook->arity);}
static XEN ghook_functions(ghook *hook) {return(hook->functions);}
static void reset_ghook(ghook *hook) {hook->functions = XEN_EMPTY_LIST;}
bool xen_gauche_hook_p(XEN obj) {return(XEN_OBJECT_TYPE_P(obj, ghook_tag));}

static void add_ghook(ghook *hook, XEN function, bool at_end) 
{
  if (at_end)
    hook->functions = XEN_APPEND(hook->functions, XEN_LIST_1(function));
  else hook->functions = XEN_CONS(function, hook->functions);
}

static XEN g_hook_p(XEN val) 
{
  return(C_TO_XEN_BOOLEAN(xen_gauche_hook_p(val)));
}

static XEN g_hook_empty_p(XEN hook)
{
  XEN_ASSERT_TYPE(xen_gauche_hook_p(hook), hook, XEN_ONLY_ARG, "hook-empty?", "a hook");
  return(C_TO_XEN_BOOLEAN(XEN_NULL_P(ghook_functions((ghook *)XEN_OBJECT_REF(hook)))));
}

bool xen_gauche_hook_empty_p(XEN hook)
{
  return(XEN_NULL_P(ghook_functions((ghook *)XEN_OBJECT_REF(hook))));
}

static XEN g_make_hook(XEN arity, XEN help)
{
  ghook *hook;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(arity), arity, XEN_ARG_1, "make-hook", "an integer");
  XEN_ASSERT_TYPE(XEN_STRING_P(help) || XEN_NOT_BOUND_P(help), help, XEN_ARG_2, "make-hook", "a string if bound");
  hook = make_ghook(XEN_TO_C_INT(arity));
  return(xen_gauche_make_object(ghook_tag, (void *)hook, mark_ghook));
}

static XEN g_add_hook(XEN hook, XEN function, XEN position)
{
  ghook *obj;
  bool at_end = false;
  XEN_ASSERT_TYPE(xen_gauche_hook_p(hook), hook, XEN_ARG_1, "add-hook!", "a hook");
  obj = (ghook *)XEN_OBJECT_REF(hook);
  XEN_ASSERT_TYPE(XEN_PROCEDURE_P(function) && 
		  ((XEN_REQUIRED_ARGS(function) == ghook_arity(obj)) ||
		   ((SCM_PROCEDURE_REQUIRED(function) + SCM_PROCEDURE_OPTIONAL(function)) == ghook_arity(obj))),
		  function, XEN_ARG_2, "add-hook!", "a function");
  XEN_ASSERT_TYPE(XEN_BOOLEAN_IF_BOUND_P(position), position, XEN_ARG_3, "add-hook!", "boolean");
  if (XEN_BOOLEAN_P(position)) at_end = XEN_TO_C_BOOLEAN(position);
  add_ghook(obj, function, at_end);
  Scm_ForeignPointerAttrSet(SCM_FOREIGN_POINTER(hook), SCM_INTERN("functions"), obj->functions);
  return(hook);
}

XEN xen_gauche_reset_hook(XEN hook)
{
  ghook *obj;
  XEN_ASSERT_TYPE(xen_gauche_hook_p(hook), hook, XEN_ONLY_ARG, "reset-hook!", "a hook");
  obj = (ghook *)XEN_OBJECT_REF(hook);
  reset_ghook(obj);
  Scm_ForeignPointerAttrSet(SCM_FOREIGN_POINTER(hook), SCM_INTERN("functions"), obj->functions);  
  return(hook);
}

static XEN g_run_hook(XEN all_args)
{
  XEN hook, args;
  ghook *obj;
  int arglen;
  XEN functions, val = XEN_FALSE;
  hook = XEN_CAR(all_args);
  XEN_ASSERT_TYPE(xen_gauche_hook_p(hook), hook, XEN_ARG_1, "run-hook", "a hook");
  obj = (ghook *)XEN_OBJECT_REF(hook);
  args = XEN_CDR(all_args);
  arglen = XEN_LIST_LENGTH(args);
  if (ghook_arity(obj) != arglen)
    XEN_ERROR(XEN_ERROR_TYPE("wrong-number-of-args"),
	      XEN_LIST_2(C_TO_XEN_STRING("run-hook"),
			 args));
  functions = ghook_functions(obj);
  while (XEN_NOT_NULL_P(functions))
    {
      val = Scm_Apply(XEN_CAR(functions), args);
      functions = XEN_CDR(functions);
    }
  return(val);
}

static XEN g_remove_hook(XEN hook, XEN function)
{
  ghook *obj;
  XEN_ASSERT_TYPE(xen_gauche_hook_p(hook), hook, XEN_ARG_1, "remove-hook!", "a hook");
  obj = (ghook *)XEN_OBJECT_REF(hook);
  XEN_ASSERT_TYPE(XEN_PROCEDURE_P(function), function, XEN_ARG_2, "remove-hook!", "a function");
  obj->functions = Scm_DeleteX(function, obj->functions, SCM_CMP_EQ);
  Scm_ForeignPointerAttrSet(SCM_FOREIGN_POINTER(hook), SCM_INTERN("functions"), obj->functions);  
  return(hook);
}

XEN xen_gauche_hook_to_list(XEN hook)
{
  XEN_ASSERT_TYPE(xen_gauche_hook_p(hook), hook, XEN_ONLY_ARG, "reset-hook!", "a hook");
  return(ghook_functions((ghook *)XEN_OBJECT_REF(hook)));
}

XEN xen_gauche_define_hook(const char *name, int arity, const char *help)
{
  XEN sym, hook;
  hook = g_make_hook(C_TO_XEN_INT(arity), (help) ? C_TO_XEN_STRING(help) : XEN_UNDEFINED);
  if (name)
    {
      sym = SCM_INTERN(name);
      Scm_Define(SCM_MODULE(Scm_UserModule()), SCM_SYMBOL(sym), hook);
      xen_gauche_set_help(sym, help);
    }
  return(hook);
}


static XEN g_procedure_arity(XEN func)
{
  return(XEN_ARITY(func));
}

XEN_NARGIFY_1(g_defined_p_w, g_defined_p)
XEN_NARGIFY_1(g_xen_gauche_provided_p_w, g_xen_gauche_provided_p)
XEN_NARGIFY_1(g_xen_gauche_provide_w, g_xen_gauche_provide)
XEN_NARGIFY_1(g_xen_gauche_object_to_string_w, xen_gauche_object_to_string)
XEN_NARGIFY_1(g_procedure_arity_w, g_procedure_arity)

XEN_NARGIFY_1(g_hook_p_w, g_hook_p);
XEN_NARGIFY_1(g_hook_empty_p_w, g_hook_empty_p)
XEN_NARGIFY_2(g_remove_hook_w, g_remove_hook)
XEN_NARGIFY_1(g_hook_to_list_w, xen_gauche_hook_to_list)
XEN_VARGIFY(g_run_hook_w, g_run_hook)
XEN_NARGIFY_1(g_reset_hook_w, xen_gauche_reset_hook)
XEN_ARGIFY_2(g_make_hook_w, g_make_hook)
XEN_ARGIFY_3(g_add_hook_w, g_add_hook)

void xen_initialize(void)
{
  Scm_Init(GAUCHE_SIGNATURE); /* signature is apparently a version mismatch check? (core.c) */
  {
    SCM_UNWIND_PROTECT {
      Scm_Load("gauche-init.scm", 0);
    }
    SCM_WHEN_ERROR {
      fprintf(stderr, "Error in Gauche initialization file.\n");
      /* SCM_NEXT_HANDLER; */
    }
    SCM_END_PROTECT;
  }

  XEN_EVAL_C_STRING("(define *features* (list 'defmacro 'record))"); /* has to be first so *features* exists */
  help_hash_table = Scm_MakeHashTableSimple(SCM_HASH_EQ, 2048);
  xen_gauche_permanent_object(help_hash_table);

  XEN_DEFINE_PROCEDURE("defined?",        g_defined_p_w,                   1, 0, 0, "(defined? arg) -> #t if arg is defined");
  XEN_DEFINE_PROCEDURE("provided?",       g_xen_gauche_provided_p_w,       1, 0, 0, "(provided? arg) -> #t if arg is on the *features* list");
  XEN_DEFINE_PROCEDURE("provide",         g_xen_gauche_provide_w,          1, 0, 0, "(provide arg) -> add arg to *features* list");
  XEN_DEFINE_PROCEDURE("object->string",  g_xen_gauche_object_to_string_w, 1, 0, 0, "return string representation of arg");
  XEN_DEFINE_PROCEDURE("procedure-arity", g_procedure_arity_w,             1, 0, 0, "return (list required optional) args");

  ghook_tag = XEN_MAKE_OBJECT_TYPE("<hook>", sizeof(ghook), print_hook, free_hook);

  XEN_DEFINE_PROCEDURE("hook?",        g_hook_p_w,       1, 0, 0, "(hook? obj) -> #t if obj is a hook");
  XEN_DEFINE_PROCEDURE("hook-empty?",  g_hook_empty_p_w, 1, 0, 0, "(hook-empty? hook) -> #t if obj is an empty hook");
  XEN_DEFINE_PROCEDURE("remove-hook!", g_remove_hook_w,  2, 0, 0, "(remove-hook! hook func) removes func from hook obj");
  XEN_DEFINE_PROCEDURE("reset-hook!",  g_reset_hook_w,   1, 0, 0, "(reset-hook! hook) removes all funcs from hook obj");
  XEN_DEFINE_PROCEDURE("hook->list",   g_hook_to_list_w, 1, 0, 0, "(hook->list hook) -> list of functions on hook obj");
  XEN_DEFINE_PROCEDURE("run-hook",     g_run_hook_w,     0, 0, 1, "(run-hook hook . args) applies each hook function to args");
  XEN_DEFINE_PROCEDURE("make-hook",    g_make_hook_w,    1, 1, 0, "(make-hook arity :optional help) makes a new hook object");
  XEN_DEFINE_PROCEDURE("add-hook!",    g_add_hook_w,     2, 1, 0, "(add-hook! hook func :optional append) adds func to the hooks function list");
}


#endif



/* ------------------------------ NONE OF THE ABOVE ------------------------------ */

#if (!HAVE_EXTENSION_LANGUAGE)

char *xen_version(void)
{
#if HAVE_STRDUP
  return(strdup("no extension language"));
#else
  char *buf;
  buf = (char *)calloc(64, sizeof(char));
#if HAVE_SNPRINTF
  snprintf(buf, 64, "no extension language");
#else
  sprintf(buf, "no extension language");
#endif
  return(buf);
#endif
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

int xen_to_c_int_or_else(XEN obj, int fallback)
{
  return(fallback);
}

off_t xen_to_c_off_t_or_else(XEN obj, off_t fallback)
{
  return(0);
}

off_t xen_to_c_off_t(XEN obj)
{
  return(0);
}

XEN c_to_xen_off_t(off_t val)
{
  return(XEN_ZERO);
}

#endif
