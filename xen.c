/* xen support procedures */

#include <config.h>
#include <ctype.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#if MACOS
  #define off_t long
#else
  #include <sys/types.h>
#endif
#if HAVE_STDINT_H
  #include <stdint.h>
#endif
#include <math.h>
#include "xen.h"

XEN xen_return_first(XEN a, ...)
{
  return(a);
}

off_t xen_to_c_off_t_or_else(XEN obj, off_t fallback)
{
#if HAVE_GUILE
  if ((XEN_NOT_FALSE_P(scm_integer_p(obj))) && XEN_EXACT_P(obj))
#else
  if (XEN_OFF_T_P(obj))
#endif
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
#if HAVE_GUILE
  if (XEN_EXACT_P(obj))
#endif
#if (defined(SIZEOF_OFF_T) && (SIZEOF_OFF_T > 4)) || (defined(_FILE_OFFSET_BITS) && (_FILE_OFFSET_BITS == 64))
    return(XEN_TO_C_LONG_LONG(obj));
#else
    return(XEN_TO_C_INT(obj));
#endif
#if HAVE_GUILE
    return((off_t)XEN_TO_C_DOUBLE(obj)); /* inexact integer squeezed through somewhere */
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


/* ------------------------------ GUILE ------------------------------ */

#if HAVE_GUILE

char *xen_version(void)
{
  char *buf;
  buf = (char *)calloc(64, sizeof(char));
#if HAVE_SNPRINTF
  snprintf(buf, 64, "Xen: %s, Guile: %s", XEN_VERSION, XEN_TO_C_STRING(scm_version()));
#else
  sprintf(buf, "Xen: %s, Guile: %s", XEN_VERSION, XEN_TO_C_STRING(scm_version()));
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
  double num;
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
	if ((scm_is_true(scm_inf_p(a))) || (scm_is_true(scm_nan_p(a)))) return(0);
	if (SCM_REALP(a)) return((int)(SCM_REAL_VALUE(a)));
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

void xen_guile_define_procedure_with_setter(char *get_name, XEN (*get_func)(), char *get_help,
					    char *set_name, XEN (*set_func)(), 
					    XEN local_doc,
					    int get_req, int get_opt, int set_req, int set_opt)
{
#if HAVE_SCM_C_DEFINE
  XEN str = XEN_FALSE;
#if XEN_DEBUGGING
  if (XEN_DEFINED_P(get_name)) fprintf(stderr, "%s is defined\n", get_name);
  /* if (!(snd_url(get_name))) fprintf(stderr, "%s not documented\n", get_name); */
#endif
  if (get_help) str = C_TO_XEN_STRING(get_help);
  scm_permanent_object(
    scm_c_define(get_name,
      scm_make_procedure_with_setter(
        XEN_NEW_PROCEDURE("", XEN_PROCEDURE_CAST get_func, get_req, get_opt, 0),
	XEN_NEW_PROCEDURE("", XEN_PROCEDURE_CAST set_func, set_req, set_opt, 0))));
  if (get_help)
    {
      scm_set_object_property_x(C_STRING_TO_XEN_SYMBOL(get_name), local_doc, str);
      scm_set_procedure_property_x(XEN_NAME_AS_C_STRING_TO_VALUE(get_name), local_doc, str);
    }
#else
  scm_set_object_property_x(
    XEN_CDR(
      gh_define(get_name,
	scm_make_procedure_with_setter(
          XEN_NEW_PROCEDURE("", XEN_PROCEDURE_CAST get_func, get_req, get_opt, 0),
	  XEN_NEW_PROCEDURE("", XEN_PROCEDURE_CAST set_func, set_req, set_opt, 0)
	  ))),
    local_doc,
    C_TO_XEN_STRING(get_help));
#endif
}

void xen_guile_define_procedure_with_reversed_setter(char *get_name, XEN (*get_func)(), char *get_help,
						     char *set_name, XEN (*set_func)(), XEN (*reversed_set_func)(), 
						     XEN local_doc,
						     int get_req, int get_opt, int set_req, int set_opt)
{
#if HAVE_SCM_C_DEFINE
  XEN str = XEN_FALSE;
#if XEN_DEBUGGING
  if (XEN_DEFINED_P(get_name)) fprintf(stderr, "%s is defined\n", get_name);
  /* if (!(snd_url(get_name))) fprintf(stderr, "%s not documented\n", get_name); */
#endif
  if (get_help) str = C_TO_XEN_STRING(get_help);
  scm_permanent_object(
    scm_c_define(get_name,
      scm_make_procedure_with_setter(
        XEN_NEW_PROCEDURE("", XEN_PROCEDURE_CAST get_func, get_req, get_opt, 0),
	XEN_NEW_PROCEDURE("", XEN_PROCEDURE_CAST reversed_set_func, set_req, set_opt, 0))));
  if (get_help)
    {
      scm_set_object_property_x(C_STRING_TO_XEN_SYMBOL(get_name), local_doc, str);
      scm_set_procedure_property_x(XEN_NAME_AS_C_STRING_TO_VALUE(get_name), local_doc, str);
    }
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
#endif
}

XEN xen_guile_create_hook(const char *name, int args, const char *help, XEN local_doc)
{
  /* documentation for hooks */
  XEN hook;
#if HAVE_GUILE
  if ((name) && (help))
    {
      hook = scm_permanent_object(scm_make_hook(C_TO_XEN_INT(args)));
      scm_set_object_property_x(hook, local_doc, C_TO_XEN_STRING(help));
#if HAVE_SCM_C_DEFINE
#if XEN_DEBUGGING
      if (XEN_DEFINED_P(name)) fprintf(stderr, "%s is defined\n", name);
      /* if (!(snd_url(name))) fprintf(stderr, "%s not documented\n", name); */
#endif
      scm_c_define(name, hook);
#else
  #if HAVE_SCM_MAKE_REAL
      gh_define(name, hook);
  #else
      gh_define((char *)name, hook);
  #endif
#endif
    }
  else hook = scm_make_hook(C_TO_XEN_INT(args));
#endif
  return(hook);
}

#if XEN_DEBUGGING
XEN xen_guile_dbg_new_procedure(const char *name, XEN (*func)(), int req, int opt, int rst)
{
  /* look for name collisions */
  if ((name) && (strlen(name) > 0) && XEN_DEFINED_P(name)) fprintf(stderr, "%s is defined\n", name);
  /* if ((name) && (strlen(name) > 0) && (!(snd_url(name)))) fprintf(stderr, "%s not documented\n", name); */
  return(scm_c_define_gsubr(name, req, opt, rst, func));
}
#endif

#if HAVE_SCM_C_MAKE_RECTANGULAR
static char **xen_temp_strings = NULL;
static int xen_temp_strings_ctr = 0;
#define XEN_TEMP_STRINGS_SIZE 512

#if DEBUGGING
static char **stored_strings = NULL;
static int stored_strings_ctr = 0;
#endif

char *xen_guile_to_c_string_with_eventual_free(XEN str)
{
  char *result;
  if (!xen_temp_strings)
    xen_temp_strings = (char **)calloc(XEN_TEMP_STRINGS_SIZE, sizeof(char *));
  else
    {
      if (xen_temp_strings[xen_temp_strings_ctr]) 
#if DEBUGGING
	{
	  int i, len;
	  char *str;
	  str = xen_temp_strings[xen_temp_strings_ctr];
	  len = strlen(str);
	  for (i = 0; i < len; i++) str[i] = 'X';
	  if (!stored_strings) stored_strings = (char **)calloc(1024, sizeof(char **));
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

static char **xr_help_names = NULL;
static char **xr_help_data = NULL;
static int help_size = 0;
static int help_top = 0;

void xen_add_help(char *name, const char *help)
{
  if (help_top >= help_size)
    {
      if (help_size == 0)
	{
	  help_size = 1024;
	  xr_help_names = (char **)calloc(help_size, sizeof(char *));
	  xr_help_data = (char **)calloc(help_size, sizeof(char *));
	}
      else
	{
	  help_size += 1024;
	  xr_help_names = (char **)realloc(xr_help_names, help_size * sizeof(char *));
	  xr_help_data = (char **)realloc(xr_help_data, help_size * sizeof(char *));
	}
    }
  xr_help_names[help_top] = name;
  xr_help_data[help_top] = (char *)help;
  help_top++;
}

char *xen_help(char *name)
{
  int i;
  for (i = 0; i < help_top; i++)
    if (strcmp(name, xr_help_names[i]) == 0)
      return(xr_help_data[i]);
  return(NULL);
}

void xen_initialize(void)
{
  ruby_init();
  Init_Hook();
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
  rb_gc_mark(val);
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
  if (!(XEN_LIST_P(arg2)))
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
#if HAVE_SNPRINTF
  snprintf(buf, 128, "Xen: %s, Ruby: %s (%s)", 
#else
  sprintf(buf, "Xen: %s, Ruby: %s (%s)", 
#endif
	  XEN_VERSION,
#ifdef RUBY_VERSION
	  RUBY_VERSION,
	  RUBY_RELEASE_DATE
#else
	  XEN_TO_C_STRING(XEN_EVAL_C_STRING("RUBY_VERSION")),
	  XEN_TO_C_STRING(XEN_EVAL_C_STRING("RUBY_RELEASE_DATE"))
#endif
	  );
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

static XEN xen_rb_rep(XEN ig)
{
  XEN val;
  char *str;
#if HAVE_READLINE
  char *line_read = NULL;
  line_read = readline(">");
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
  fprintf(stdout, ">");
  getline(buffer, &size, stdin);
  val = xen_rb_eval_string_with_error(buffer[0]);
  str = XEN_AS_STRING(val);
  fprintf(stdout, "%s\n", (str) ? str : "nil");
  free(buffer[0]);
  free(buffer);
#endif
  return(ig);
}

static XEN xen_rb_rescue(XEN val)
{
  return(rb_rescue(XEN_PROCEDURE_CAST xen_rb_rep,
		   XEN_FALSE,
		   XEN_PROCEDURE_CAST xen_rb_report_error,
		   XEN_FALSE));
}

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
	  extern VALUE ruby_errinfo;
	  fprintf(stderr, "%s\n", XEN_AS_STRING(ruby_errinfo));
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
  val = rb_protect(XEN_VALUE_ARG_PROCEDURE_CAST xen_rb_funcall_0_inner,
		   func,
		   &status);
  if (status != 0)
    {
      extern VALUE ruby_errinfo;
      return(XEN_TO_STRING(ruby_errinfo));
    }
  return(val);
}

#if (!HAVE_RB_ARY_DUP)
XEN xen_rb_copy_list(XEN val)
{
  /* if this is considered bad form, we could fall back on flatten (rb_ary_dup?) */
  long len, i;
  VALUE collect;
  len = RARRAY(val)->len;
  collect = rb_ary_new2(len);
  for (i = 0; i < len; i++) 
    RARRAY(collect)->ptr[i] = RARRAY(val)->ptr[i];
  RARRAY(collect)->len = len;
  return(collect);
}
#endif

XEN xen_rb_str_new2(char *arg)
{
  return(rb_str_new2((arg) ? arg : ""));
}

double xen_rb_to_c_double_or_else(XEN a, double b) {return(XEN_NUMBER_P(a) ? NUM2DBL(a) : b);}
int xen_rb_to_c_int_or_else(XEN a, int b) {return(XEN_INTEGER_P(a) ? FIX2INT(a) : b);}

/* class Hook */
 
static VALUE xen_rb_cHook;

static VALUE hook_alloc(VALUE klass)
{
  return Data_Wrap_Struct(klass, 0, 0, 0);
}

VALUE xen_rb_is_hook_p(VALUE obj)
{
  return rb_obj_is_kind_of(obj, xen_rb_cHook);
}

#define XEN_CLASS_HOOK_P(Arg)              xen_rb_is_hook_p(Arg)

/*
 * @name = "$name_of_hook"
 * @arity = arity of procedure(s),         default 0
 * @help = "optional description of hook", default ""
 * @procs = [["named proc1", proc1], ...]
 */

static VALUE xen_rb_hook_initialize(int argc, VALUE *argv, VALUE hook)
{
  VALUE name, arity, help;
  rb_scan_args(argc, argv, "12", &name, &arity, &help);
  XEN_ASSERT_TYPE(XEN_STRING_P(name) || XEN_SYMBOL_P(name), name, XEN_ARG_1, c__FUNCTION__, "a char* or symbol");
  if (XEN_SYMBOL_P(name))
    name = C_TO_XEN_STRING(rb_id2name(SYM2ID(name)));
  if (arity != Qnil) {
    XEN_ASSERT_TYPE(XEN_INTEGER_P(arity), arity, XEN_ARG_2, c__FUNCTION__, "an integer");
  }
  else {
    arity = INT2NUM(0);
  }
  if (help != Qnil) {
    XEN_ASSERT_TYPE(XEN_STRING_P(help), help, XEN_ARG_3, c__FUNCTION__, "a char*");
  }
  else {
    help = rb_str_new2("");
  }
  rb_iv_set(hook, "@name", name);
  rb_iv_set(hook, "@arity", arity);
  rb_iv_set(hook, "@help", help);
  rb_iv_set(hook, "@procs", rb_ary_new());
  return hook;
}

/*
 * To create variables of class Hook in C, see xen.h, XEN_DEFINE_HOOK
 * and XEN_DEFINE_SIMPLE_HOOK
 */
VALUE xen_rb_hook_c_new(char *name, int arity, char *help)
{
  VALUE args[3];
  args[0] = C_TO_XEN_STRING(name);
  args[1] = C_TO_XEN_INT(arity);
  args[2] = C_TO_XEN_STRING(help);
  return xen_rb_hook_initialize(3, args, hook_alloc(xen_rb_cHook));
}

static VALUE xen_rb_hook_add_hook(int argc, VALUE *argv, VALUE hook)
{
  VALUE name, func;
  rb_scan_args(argc, argv, "1&", &name, &func);
  XEN_ASSERT_TYPE(XEN_STRING_P(name), name, XEN_ARG_1, c__FUNCTION__, "a char*");
  XEN_ASSERT_TYPE(XEN_PROCEDURE_P(func), func, XEN_ARG_2, c__FUNCTION__, "a procedure");
  rb_ary_push(rb_iv_get(hook, "@procs"), rb_ary_new3(2, name, func));
  return hook;
}

static VALUE xen_rb_hook_remove_hook(VALUE hook, VALUE name)
{
  VALUE ary = rb_iv_get(hook, "@procs");
  return rb_ary_delete(ary, rb_ary_assoc(ary, name));
}

VALUE xen_rb_hook_reset_hook(VALUE hook)
{
  XEN_ASSERT_TYPE(XEN_CLASS_HOOK_P(hook), hook, XEN_ARG_1, c__FUNCTION__, "class Hook");
  rb_ary_clear(rb_iv_get(hook, "@procs"));
  return hook;
}

static VALUE xen_rb_hook_names(VALUE hook)
{
  VALUE ary = rb_obj_dup(rb_iv_get(hook, "@procs"));
  if (RARRAY(ary)->len) {
    VALUE ret = rb_ary_new();
    while (RARRAY(ary)->len)
      rb_ary_push(ret, rb_ary_entry(rb_ary_shift(ary), 0));
    return ret;
  }
  else
    return Qnil;
}

VALUE xen_rb_hook_to_a(VALUE hook)
{
  VALUE ary = rb_obj_dup(rb_iv_get(hook, "@procs"));
  XEN_ASSERT_TYPE(XEN_CLASS_HOOK_P(hook), hook, XEN_ARG_1, c__FUNCTION__, "class Hook");
  if (RARRAY(ary)->len) {
    VALUE ret = rb_ary_new();
    while (RARRAY(ary)->len)
      rb_ary_push(ret, rb_ary_entry(rb_ary_shift(ary), 1));
    return ret;
  }
  else
    return Qnil;
}

static VALUE xen_rb_hook_run_hook(VALUE hook)
{
  if (RARRAY(rb_iv_get(hook, "@procs"))->len)
    rb_ary_each(xen_rb_hook_to_a(hook));
  return hook;
}

/*
 * Calls all hook-procedures but returns only the last result; use
 * $var_hook.run_hook { |prc| ret << prc.call(*args) } for collecting
 * results.
 */
static VALUE xen_rb_hook_call(int argc, VALUE *argv, VALUE hook)
{
  long i;
  VALUE result = Qnil, rest, procs = xen_rb_hook_to_a(hook);
  rb_scan_args(argc, argv, "*", &rest);
  if (procs != Qnil)
    for (i = 0; i < RARRAY(procs)->len; i++)
      result = xen_rb_apply(rb_ary_entry(procs, i), rest);
  return result;
}

static VALUE xen_rb_hook_is_empty_p(VALUE hook)
{
  return C_TO_XEN_BOOLEAN(RARRAY(rb_iv_get(hook, "@procs"))->len == 0);
}

static VALUE xen_rb_hook_length(VALUE hook)
{
  return C_TO_XEN_INT(RARRAY(rb_iv_get(hook, "@procs"))->len);
}

static VALUE xen_rb_hook_name(VALUE hook)
{
  return rb_iv_get(hook, "@name");
}

static VALUE xen_rb_hook_arity(VALUE hook)
{
  return rb_iv_get(hook, "@arity");
}

static VALUE xen_rb_hook_describe(VALUE hook)
{
  return rb_iv_get(hook, "@help");
}

static VALUE xen_rb_hook_inspect(VALUE hook)
{
  VALUE str = rb_str_new2("#<Hook name: ");
  rb_str_append(str, rb_inspect(rb_iv_get(hook, "@name")));
  rb_str_cat2(str, ", arity: ");
  rb_str_append(str, rb_inspect(rb_iv_get(hook, "@arity")));
  rb_str_cat2(str, ", procs[");
  rb_str_append(str, rb_inspect(xen_rb_hook_length(hook)));
  rb_str_cat2(str, "]: ");
  rb_str_append(str, rb_inspect(xen_rb_hook_names(hook)));
  rb_str_cat2(str, ">");
  return str;
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

static VALUE xen_rb_make_hook(int argc, VALUE *argv, VALUE klass)
{
  VALUE hook, name;
  if (argc <= 3) {
    hook = xen_rb_hook_initialize(argc, argv, hook_alloc(xen_rb_cHook));
  }
  else {
    hook = xen_rb_hook_initialize(3, argv, hook_alloc(xen_rb_cHook));
    argv[0] = argv[3];
    hook = xen_rb_hook_add_hook(1, argv, hook);
  }
  /* set global ruby variable "#{@name} = #{hook}" */
  name = xen_rb_hook_name(hook);
  if (RSTRING(name)->ptr[0] != '$')
    name = C_TO_XEN_STRING(xen_scheme_global_variable_to_ruby(RSTRING(name)->ptr));
  XEN_ASSERT_TYPE(RSTRING(name)->len >= 2, name, XEN_ARG_1, c__FUNCTION__, "a char*, len >= 2");
  return rb_gv_set(RSTRING(name)->ptr, hook);
}

static VALUE xen_rb_hook_p(VALUE klass, VALUE obj)
{
  return rb_obj_is_kind_of(obj, xen_rb_cHook);
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
static VALUE xen_rb_new(int argc, VALUE *argv, VALUE klass)
{
  VALUE hook = hook_alloc(klass);
  rb_obj_call_init(hook, argc, argv);
  return hook;
}
#endif

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
  rb_define_singleton_method(xen_rb_cHook, "new", xen_rb_new, -1);
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
  rb_define_global_function("hook?", XEN_PROCEDURE_CAST xen_rb_hook_p, 1);
}

/* end of class Hook */

#endif


/* ------------------------------ NONE OF THE ABOVE ------------------------------ */

#if (!HAVE_GUILE) && (!HAVE_RUBY)

char *xen_version(void)
{
#if HAVE_STRDUP
  return(strdup("no embedded language"));
#else
  char *buf;
  buf = (char *)calloc(64, sizeof(char));
#if HAVE_SNPRINTF
  snprintf(buf, 64, "no embedded language");
#else
  sprintf(buf, "no embedded language");
#endif
  return(buf);
#endif
}

int xen_to_c_int_or_else(XEN obj, int fallback)
{
  return(fallback);
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
