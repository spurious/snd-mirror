#ifndef XEN_H
#define XEN_H

/* macros for extension language support 
 *
 * Guile:     covers 1.3.4 to present (1.7.0) given the configuration macros in the Snd or Libxm config.h.in.
 * Ruby:      still a few gaps, tested in 1.6.*, 1.7.*, 1.8.*, 1.9.0
 * None:      covers all known versions of None
 *
 * I also have incomplete versions for SCM, MzScheme, Python, and librep if anyone is interested,
 *   and much of Elk, but the latter defines Object, False, and True which collide with Xt's Object etc.
 *   Last I looked, MzScheme's user-defined-type stuff was not very good.
 *   Librep's main problem is that any code that uses it is completely undebuggable.
 *   Python doesn't strike me as very different from Ruby.
 *   SCM, like MIT_Scheme, isn't really oriented toward extending C programs.
 *   I've also looked briefly at Haskell (FFI too primitive), Lua (looks ok, though minimal, but lots of
 *     work to fit into the Xen model), ocaml (FFI in flux, apparently, and I think it
 *     insists on being the main program).
 *
 * "xen" from Greek xenos (guest, foreigner)
 */

#define XEN_MAJOR_VERSION 1
#define XEN_MINOR_VERSION 17
#define XEN_VERSION "1.17"

/* HISTORY:
 *
 *  12-Aug-04: more Guile name changes, C_TO_XEN_STRINGN (Guile)
 *  3-Aug-04:  xen_to_c_int bugfix thanks to Kjetil S. Matheussen.
 *  29-Jul-04: deprecated XEN_TO_C_BOOLEAN_OR_TRUE.
 *  21-Jul-04: deprecated XEN_TO_SMALL_C_INT and C_TO_SMALL_XEN_INT.
 *             use new Guile 1.7 numerical function names (under flag HAVE_SCM_TO_SIGNED_INTEGER).
 *  28-Jun-04: XEN_REQUIRED_ARGS_OK to make it easier to turn off this check.
 *  9-June-04: complex number conversions (Guile) -- I don't think Ruby has complex numbers.
 *  21-May-04: plug some memory leaks in Ruby cases.
 *  23-Feb-04: changed DEBUGGING to XEN_DEBUGGING, added redefinition checks under that switch.
 *  2-Feb-04:  C_TO_XEN_CHAR, ratio support (Guile), XEN_CONS_P, XEN_PAIR_P, etc
 *  6-Jan-04:  XEN_VARIABLE_REF in Guile changed to support 1.4 and older versions.
 *  5-Jan-04:  hook support in Ruby thanks to Michael Scholz.
 *  --------
 *  1-Nov-03:  protect several macros from hidden double evaluations.
 *  29-Sep-03: fixed incorrect assumption in xen_rb_cons (xen.c) that arg2 was list.
 *  8-Sep-03:  removed xen_malloc -- can't remember now why this existed.
 *  19-Aug-03: xen_rb_str_new2 to avoid unwanted side-effects.
 *  12-Aug-03: various changes for ISO C99.
 *  30-Jul-03: use new SCM_VECTOR_REF/SET macros if they're defined.
 *  7-Apr-03:  changes to error handlers for more perspicuous error messages
 *             changed XEN_PROTECT_FROM_GC in Ruby to use rb_gc_register_address, added XEN_UNPROTECT_FROM_GC (rb_gc_unregister_address)
 *  10-Mar-03: XEN_OUT_OF_RANGE_ERROR, XEN_BAD_ARITY_ERROR
 *  17-Feb-03: XEN_HOOK_P
 *  20-Jan-03: added Windows case for auto-import loader bugfix.
 *  --------
 *  19-Dec-02: proc arg checks for Ruby (to make sure XEN_[N|V]ARGIFY|DEFINE_PROCEDURE[etc] agree)
 *  29-Jul-02: SCM_WRITABLE_VELTS for current CVS Guile
 *  28-May-02: off_t equivalents in Ruby 1.7
 *  6-May-02:  off_t (long long) macros.
 *  29-Apr-02: XEN_EXACT_P
 *  2-Jan-02:  removed TIMING and MCHECK debugging stuff, VARIABLE_REF -> XEN_VARIABLE_REF
 *  --------
 *  22-Sep-01: removed (redundant) UNSIGNED_LONG macros -- use ULONG instead
*/

#ifndef __cplusplus
#if HAVE_STDBOOL_H
  #include <stdbool.h>
#else
#ifndef true
  #define bool	int
  #define true	1
  #define false	0
#endif
#endif
#endif

#ifndef c__FUNCTION__
#if defined(__STDC__) && defined(__STDC_VERSION__) && (__STDC_VERSION__ >= 199901L)
  #define c__FUNCTION__ __func__
#else
#ifdef __GNUC__
  #define c__FUNCTION__ __FUNCTION__
#else
  #define c__FUNCTION__ ""
#endif
#endif
#endif


/* ------------------------------ GUILE ------------------------------ */

#if HAVE_GUILE
#if (!HAVE_SCM_NUM2INT)
  #include <guile/gh.h>
#else
  #include <libguile.h>
#endif

#define XEN_OK 1

#define XEN                 SCM
#define XEN_TRUE            SCM_BOOL_T
#define XEN_FALSE           SCM_BOOL_F

#define XEN_FILE_EXTENSION  "scm"
#define XEN_COMMENT_STRING  ";"
#define XEN_EMPTY_LIST      SCM_EOL

#ifdef __cplusplus
  #define XEN_PROCEDURE_CAST (XEN (*)())
#else
  #define XEN_PROCEDURE_CAST
#endif

#define XEN_UNDEFINED       SCM_UNDEFINED

#if HAVE_SCM_C_DEFINE
  #define XEN_VARIABLE_REF(Var)               SCM_VARIABLE_REF(Var)
  #define XEN_VARIABLE_SET(Var, Val)          SCM_VARIABLE_SET(Var, Val)
  #define XEN_NAME_AS_C_STRING_TO_VALUE(a)    XEN_VARIABLE_REF(scm_sym2var(scm_str2symbol(a), scm_current_module_lookup_closure (), XEN_TRUE))
      /* this is probably not the right thing -- the 3rd arg should be XEN_FALSE, else we're defining a new variable in the current module */
  #define XEN_NAME_AS_C_STRING_TO_VARIABLE(a) scm_sym2var(scm_str2symbol(a), scm_current_module_lookup_closure(), XEN_FALSE)
  #define XEN_SYMBOL_TO_VARIABLE(a)           scm_sym2var(a, scm_current_module_lookup_closure(), XEN_FALSE)

  #if HAVE_SCM_DEFINED_P
    #define XEN_DEFINED_P(Name)               XEN_TRUE_P(scm_defined_p(C_STRING_TO_XEN_SYMBOL(Name), XEN_UNDEFINED))
  #else
    #define XEN_DEFINED_P(Name)               XEN_TRUE_P(scm_definedp(C_STRING_TO_XEN_SYMBOL(Name), XEN_UNDEFINED))
  #endif

#else
  #define XEN_VARIABLE_REF(Var)               SCM_CDR(Var)
  #define XEN_VARIABLE_SET(Var, Val)          SCM_SETCDR(Var, Val)
  #define XEN_NAME_AS_C_STRING_TO_VALUE(a)    scm_symbol_value0(a)
  #define XEN_NAME_AS_C_STRING_TO_VARIABLE(a) XEN_FALSE
  #define XEN_SYMBOL_TO_VARIABLE(a)           XEN_FALSE
  #define XEN_DEFINED_P(Name)                 false
#endif

#if (SCM_DEBUG_TYPING_STRICTNESS == 2)
  #define XEN_MAKE_AND_RETURN_OBJECT(Tag, Val, Ignore1, Ignore2) SCM_RETURN_NEWSMOB(Tag, Val)
#else
  #define XEN_MAKE_AND_RETURN_OBJECT(Tag, Val, Ignore1, Ignore2) SCM_RETURN_NEWSMOB(Tag, (XEN)Val)
#endif
#define XEN_OBJECT_REF(a) SCM_SMOB_DATA(a)
/* remember to check the smob type agreement before calling XEN_OBJECT_REF! */
#define XEN_MAKE_OBJECT(Var, Tag, Val, ig1, ig2)  SCM_NEWSMOB(Var, Tag, Val)

#if HAVE_SCM_REMEMBER_UPTO_HERE
  #if HAVE_SCM_T_CATCH_BODY
    #define XEN_OBJECT_TYPE            scm_t_bits
  #else
    #define XEN_OBJECT_TYPE            scm_bits_t
  #endif
  #define XEN_OBJECT_TYPE_P(Obj, Type) ((SCM_NIMP(Obj)) && (SCM_SMOB_PREDICATE(Type, Obj)))
#else
  #define XEN_OBJECT_TYPE              long
  #define XEN_OBJECT_TYPE_P(Obj, Type) ((SCM_NIMP(Obj)) &&  ((XEN)(SCM_TYP16(Obj)) == (XEN)Type))
#endif
#define XEN_MAKE_OBJECT_TYPE(Typ, Siz) scm_make_smob_type(Typ, Siz)
#define XEN_MARK_OBJECT_TYPE           SCM

#define XEN_MAKE_OBJECT_PRINT_PROCEDURE(Type, Wrapped_Print, Original_Print) \
  static int Wrapped_Print(XEN obj, XEN port, scm_print_state *pstate) \
  { \
    char *str; \
    str = Original_Print((Type *)XEN_OBJECT_REF(obj)); \
    scm_puts(str, port); \
    FREE(str); \
    return(1); \
  }

#define XEN_MAKE_OBJECT_FREE_PROCEDURE(Type, Wrapped_Free, Original_Free) \
  static size_t Wrapped_Free(XEN obj) \
  { \
    Original_Free((Type *)XEN_OBJECT_REF(obj)); \
    return(0); \
  }

#if HAVE_SCM_TO_SIGNED_INTEGER
  #define XEN_EQ_P(a, b)             scm_is_eq(a, b)
#else
  #ifndef SCM_EQ_P
    #define SCM_EQ_P(a, b)           ((a) == (b))
  #endif
  #define XEN_EQ_P(a, b)             SCM_EQ_P(a, b)
#endif

#define XEN_TRUE_P(a)                XEN_EQ_P(a, XEN_TRUE)
#define XEN_FALSE_P(a)               XEN_EQ_P(a, XEN_FALSE)
#define XEN_NULL_P(a)                SCM_NULLP(a)
#define XEN_BOUND_P(Arg)             (!(SCM_UNBNDP(Arg)))
#define XEN_NOT_BOUND_P(Arg)         SCM_UNBNDP(Arg)
#define XEN_ZERO                     SCM_INUM0

#define XEN_CAR(a)                   SCM_CAR(a)
#define XEN_CADR(a)                  SCM_CADR(a)
#define XEN_CADDR(a)                 SCM_CADDR(a)
#define XEN_CADDDR(a)                SCM_CADDDR(a)
#define XEN_CDR(a)                   SCM_CDR(a)
#define XEN_CDDR(a)                  SCM_CDDR(a)
#define XEN_COPY_ARG(Lst)            Lst
#define XEN_EQV_P(A, B)              XEN_TO_C_BOOLEAN(scm_eqv_p(A, B))
#define XEN_EQUAL_P(A, B)            XEN_TO_C_BOOLEAN(scm_equal_p(A, B))

#define XEN_ONLY_ARG SCM_ARGn
#define XEN_ARG_1    SCM_ARG1
#define XEN_ARG_2    SCM_ARG2
#define XEN_ARG_3    SCM_ARG3
#define XEN_ARG_4    SCM_ARG4
#define XEN_ARG_5    SCM_ARG5
#define XEN_ARG_6    SCM_ARG6
#define XEN_ARG_7    SCM_ARG7
#define XEN_ARG_8    8
#define XEN_ARG_9    9


#define XEN_TO_C_INT(a)                 xen_to_c_int(a)
#define XEN_TO_C_INT_OR_ELSE(a, b)      xen_to_c_int_or_else(a, b, c__FUNCTION__)
#define XEN_TO_C_INT_OR_ELSE_WITH_CALLER(a, b, c) xen_to_c_int_or_else(a, b, c)
#define XEN_INTEGER_P(Arg)              xen_integer_p(Arg)

/* all the number handlers changed (names...) in 1.7 */
#if HAVE_SCM_TO_SIGNED_INTEGER
  #define XEN_TO_C_DOUBLE(a)            scm_to_double(a)
  #if defined(__GNUC__) && (!(defined(__cplusplus)))
    #define XEN_TO_C_DOUBLE_OR_ELSE(a, b) ({ XEN _xen_h_0_ = a; ((XEN_NUMBER_P(_xen_h_0_)) ? (scm_to_double(_xen_h_0_)) : (b)); })
  #else
    #define XEN_TO_C_DOUBLE_OR_ELSE(a, b) xen_to_c_double_or_else(a, b)
  #endif
  #define XEN_TO_C_DOUBLE_WITH_CALLER(a, b) scm_to_double(a)
  #define C_TO_XEN_DOUBLE(a)            scm_from_double(a)
  #define C_TO_XEN_INT(a)               scm_from_int(a)
  #define XEN_TO_C_ULONG(a)             scm_to_ulong(a)
  #define C_TO_XEN_ULONG(a)             scm_from_ulong((unsigned long)a)
  #define C_TO_XEN_LONG_LONG(a)         scm_from_long_long(a)
  #define XEN_TO_C_LONG_LONG(a)         scm_to_long_long(a)
  #define XEN_EXACT_P(Arg)              XEN_TRUE_P(scm_exact_p(Arg))
  #define XEN_BOOLEAN_P(Arg)            ((bool)scm_is_bool(Arg))
  #define XEN_NUMBER_P(Arg)             ((bool)scm_is_real(Arg))
  #define XEN_DOUBLE_P(Arg)             ((bool)scm_is_real(Arg))
  #define XEN_OFF_T_P(Arg)              ((bool)scm_is_integer(Arg))
  #define XEN_ULONG_P(Arg1)             (XEN_NOT_FALSE_P(scm_number_p(Arg1)))
#else
  #define XEN_TO_C_DOUBLE(a)            scm_num2dbl(a,  c__FUNCTION__)
  #if defined(__GNUC__) && (!(defined(__cplusplus)))
    #define XEN_TO_C_DOUBLE_OR_ELSE(a, b) ({ XEN _xen_h_0_ = a; ((XEN_NUMBER_P(_xen_h_0_)) ? (scm_num2dbl(_xen_h_0_,  c__FUNCTION__)) : (b)); })
  #else
    #define XEN_TO_C_DOUBLE_OR_ELSE(a, b) xen_to_c_double_or_else(a, b)
  #endif
  #define XEN_TO_C_DOUBLE_WITH_CALLER(a, b) scm_num2dbl(a, b)
  #if HAVE_SCM_MAKE_REAL
    #define C_TO_XEN_DOUBLE(a)          scm_make_real(a)
  #else
    #define C_TO_XEN_DOUBLE(a)          scm_makdbl(a, 0.0)
  #endif
  #define C_TO_XEN_INT(a)               scm_long2num((long)a)
  #define XEN_TO_C_ULONG(a)             scm_num2ulong(a, 0, c__FUNCTION__)
  #define C_TO_XEN_ULONG(a)             scm_ulong2num((unsigned long)a)
  #define XEN_ULONG_P(Arg1)             (XEN_NOT_FALSE_P(scm_number_p(Arg1)))
  #define XEN_EXACT_P(Arg)              XEN_TRUE_P(scm_exact_p(Arg))
  #if HAVE_SCM_NUM2LONG_LONG
    #define C_TO_XEN_LONG_LONG(a)       scm_long_long2num(a)
    #define XEN_TO_C_LONG_LONG(a)       scm_num2long_long(a, 0, c__FUNCTION__)
  #else
    #define C_TO_XEN_LONG_LONG(a)       scm_long2num(a)
    #define XEN_TO_C_LONG_LONG(a)       scm_num2long(a, 0, c__FUNCTION__)
  #endif
  #define XEN_BOOLEAN_P(Arg)            (SCM_BOOLP(Arg))
  #define XEN_NUMBER_P(Arg)             (XEN_NOT_FALSE_P(scm_real_p(Arg)))
  #define XEN_DOUBLE_P(Arg)             (XEN_NOT_FALSE_P(scm_real_p(Arg)))
  #define XEN_OFF_T_P(Arg)              (XEN_NOT_FALSE_P(scm_integer_p(Arg)))
#endif

#if HAVE_COMPLEX_TRIG
  #if HAVE_SCM_C_MAKE_RECTANGULAR
    #if defined(__GNUC__) && (!(defined(__cplusplus)))
      #define XEN_TO_C_COMPLEX(a)       ({ XEN _xen_h_23_ = a; (scm_c_real_part(_xen_h_23_) + scm_c_imag_part(_xen_h_23_) * 1.0fi); })    
      #define C_TO_XEN_COMPLEX(a)       ({ complex _xen_h_24_ = a; scm_c_make_rectangular(__real__ _xen_h_24_, __imag__ _xen_h_24_); })
      #define XEN_COMPLEX_P(Arg)        scm_is_complex(Arg)
    #else
      #define XEN_TO_C_COMPLEX(a)       (scm_c_real_part(a) + scm_c_imag_part(a) * 1.0fi)
      #define C_TO_XEN_COMPLEX(a)       scm_c_make_rectangular(__real__ a, __imag__ a)
      #define XEN_COMPLEX_P(Arg)        scm_is_complex(Arg)
    #endif
  #else
    #define XEN_TO_C_COMPLEX(a)         XEN_TO_C_DOUBLE(scm_real_part(a)) + (XEN_TO_C_DOUBLE(scm_imag_part(a)) * 1.0fi)
    #define C_TO_XEN_COMPLEX(a)         scm_make_complex(__real__ a, __imag__ a)
    #define XEN_COMPLEX_P(Arg)          (XEN_NOT_FALSE_P(scm_number_p(Arg)))
  #endif
#endif

/* there is SCM_CONTINUATIONP -- why doesn't scheme have continuation? */

#if HAVE_SCM_C_MAKE_RECTANGULAR
  #define XEN_TO_C_STRING(Str)        xen_guile_to_c_string_with_eventual_free(Str)
  #define C_TO_XEN_STRING(a)          ((a) ? scm_from_locale_string(a) : XEN_FALSE)
  #define C_TO_XEN_STRINGN(Str, Len)  scm_from_locale_stringn(Str, Len)
#else
  #ifndef SCM_STRING_CHARS
    #define XEN_TO_C_STRING(STR)      SCM_CHARS(STR)
  #else
    #define XEN_TO_C_STRING(STR)      SCM_STRING_CHARS(STR)
    /* this assumes its argument is a XEN string and does not allocate new space */
  #endif
  #define C_TO_XEN_STRING(a)          scm_makfrom0str((const char *)(a))
  #define C_TO_XEN_STRINGN(Str, Len)  scm_mem2string(Str, Len)
#endif

#define C_TO_XEN_BOOLEAN(a)           ((a) ? XEN_TRUE : XEN_FALSE)
#define XEN_TO_C_BOOLEAN(a)           (!(XEN_FALSE_P(a)))

#if HAVE_SCM_C_EVAL_STRING
  #define C_STRING_TO_XEN_FORM(Str)   scm_c_read_string(Str)
#else
  #define C_STRING_TO_XEN_FORM(Str)   scm_read_0str(Str)
#endif

#if HAVE_SCM_STR2SYMBOL
  #define C_STRING_TO_XEN_SYMBOL(a)   scm_str2symbol(a)
#else
  #define C_STRING_TO_XEN_SYMBOL(a)   gh_symbol2scm(a)
#endif

#ifdef SCM_SYMBOL_CHARS
  #define XEN_EVAL_FORM(Form)         scm_eval((XEN)(Form), scm_interaction_environment())
  /* was scm_eval_x but I'm not sure that's safe */
  #define XEN_SYMBOL_TO_C_STRING(a)   SCM_SYMBOL_CHARS(a)
#else
  #define XEN_EVAL_FORM(Form)         scm_eval((XEN)(Form))
  #define XEN_SYMBOL_TO_C_STRING(a)   gh_symbol2newstr(a, NULL)
#endif
#if HAVE_SCM_OBJECT_TO_STRING
  #define XEN_TO_STRING(Obj)          scm_object_to_string(Obj, XEN_UNDEFINED)
#else
  #define XEN_TO_STRING(Obj)          scm_strprint_obj(Obj)
#endif

/* (need a way to pass an uninterpreted pointer from C to XEN then back to C) */
#if (SCM_DEBUG_TYPING_STRICTNESS == 2)
  #define XEN_WRAP_C_POINTER(a)       (C_TO_XEN_ULONG((unsigned long)a))
#else
  #define XEN_WRAP_C_POINTER(a)       ((XEN)(C_TO_XEN_ULONG((unsigned long)a)))
#endif

#define XEN_UNWRAP_C_POINTER(a)       XEN_TO_C_ULONG(a)
#define XEN_WRAPPED_C_POINTER_P(a)    XEN_NOT_FALSE_P(scm_number_p(a))

#if HAVE_SCM_C_EVAL_STRING
  #define XEN_EVAL_C_STRING(Arg)      scm_c_eval_string(Arg)
#else
  #define XEN_EVAL_C_STRING(Arg)      scm_eval_0str(Arg)
#endif

#define XEN_SET_DOCUMENTATION(Func, Help) scm_set_object_property_x(C_STRING_TO_XEN_SYMBOL(Func), XEN_DOCUMENTATION_SYMBOL, C_TO_XEN_STRING(Help))

#if XEN_DEBUGGING
  #define XEN_NEW_PROCEDURE(Name, Func, Req, Opt, Rst) xen_guile_dbg_new_procedure(Name, XEN_PROCEDURE_CAST Func, Req, Opt, Rst)
#else
#if HAVE_SCM_C_DEFINE_GSUBR
  #define XEN_NEW_PROCEDURE(Name, Func, Req, Opt, Rst) scm_c_define_gsubr(Name, Req, Opt, Rst, XEN_PROCEDURE_CAST Func)
#else
  #define XEN_NEW_PROCEDURE(Name, Func, Req, Opt, Rst) gh_new_procedure(Name, XEN_PROCEDURE_CAST Func, Req, Opt, Rst)
#endif
#endif

#define XEN_DEFINE_PROCEDURE(Name, Func, ReqArg, OptArg, RstArg, Doc) \
  if (Doc != (char *)NULL) \
    scm_set_procedure_property_x(XEN_NEW_PROCEDURE(Name, Func, ReqArg, OptArg, RstArg), XEN_DOCUMENTATION_SYMBOL, C_TO_XEN_STRING(Doc)); \
  else XEN_NEW_PROCEDURE(Name, Func, ReqArg, OptArg, RstArg)

#define XEN_DEFINE_PROCEDURE_WITH_SETTER(Get_Name, Get_Func, Get_Help, Set_Name, Set_Func, Get_Req, Get_Opt, Set_Req, Set_Opt) \
  xen_guile_define_procedure_with_setter(Get_Name, XEN_PROCEDURE_CAST Get_Func, Get_Help, \
                                         Set_Name, XEN_PROCEDURE_CAST Set_Func, \
                                         XEN_DOCUMENTATION_SYMBOL, Get_Req, Get_Opt, Set_Req, Set_Opt)

#define XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(Get_Name, Get_Func, Get_Help, Set_Name, Set_Func, Rev_Func, Get_Req, Get_Opt, Set_Req, Set_Opt) \
  xen_guile_define_procedure_with_reversed_setter(Get_Name, XEN_PROCEDURE_CAST Get_Func, Get_Help, \
                                                  Set_Name, XEN_PROCEDURE_CAST Set_Func, XEN_PROCEDURE_CAST Rev_Func, \
                                                  XEN_DOCUMENTATION_SYMBOL, Get_Req, Get_Opt, Set_Req, Set_Opt)

#if HAVE_SCM_C_DEFINE
#if XEN_DEBUGGING
#define XEN_DEFINE_CONSTANT(Name, Value, Help) \
  { \
    if (XEN_DEFINED_P(Name)) fprintf(stderr, "%s is defined\n", Name); \
    scm_c_define(Name, C_TO_XEN_INT(Value)); \
    XEN_SET_DOCUMENTATION(Name, Help); \
  }
#else
#define XEN_DEFINE_CONSTANT(Name, Value, Help) \
  { \
    scm_c_define(Name, C_TO_XEN_INT(Value)); \
    XEN_SET_DOCUMENTATION(Name, Help); \
  }
#endif
#else
#define XEN_DEFINE_CONSTANT(Name, Value, Help) \
  { \
    gh_define(Name, C_TO_XEN_INT(Value)); \
    XEN_SET_DOCUMENTATION(Name, Help); \
  }
#endif

#define XEN_DOCUMENTATION_SYMBOL        scm_string_to_symbol(C_TO_XEN_STRING("documentation"))
#define XEN_OBJECT_HELP(Name)           scm_object_property(Name, XEN_DOCUMENTATION_SYMBOL)
#define XEN_PROCEDURE_HELP(Name)        scm_procedure_property(Name, XEN_DOCUMENTATION_SYMBOL)
#define XEN_PROCEDURE_SOURCE_HELP(Name) scm_procedure_documentation(Name)
#define XEN_PROCEDURE_SOURCE(Func)      scm_procedure_source(Func)

#if HAVE_SCM_C_DEFINE
  #if XEN_DEBUGGING
    #define XEN_DEFINE_VARIABLE(Name, Var, Value) \
      { \
        if (XEN_DEFINED_P(Name)) fprintf(stderr, "%s is defined\n", Name); \
        Var = scm_permanent_object(scm_c_define(Name, Value)); \
      }
  #else
    #define XEN_DEFINE_VARIABLE(Name, Var, Value) Var = scm_permanent_object(scm_c_define(Name, Value))
  #endif
#else
  #define XEN_DEFINE_VARIABLE(Name, Var, Value) Var = gh_define(Name, Value)
#endif

#define XEN_ERROR_TYPE(Typ)           C_STRING_TO_XEN_SYMBOL(Typ)
#if USE_SND
  #define XEN_ERROR(Type, Info)       snd_throw(Type, Info)
#else
  #define XEN_ERROR(Type, Info)       scm_throw(Type, Info)
#endif
#define XEN_THROW(Tag, Arg)           scm_throw(Tag, Arg)

/* these are only needed in 1.3.4, but it's hard to find the right way to distinguish it */
#ifndef SCM_PACK
  #define SCM_BOOLP(Arg)              gh_boolean_p(Arg)
  /* the next exist in 1.3.4 but are not usable in this context (need SCM_NIMP check) */
  #undef SCM_STRINGP
  #undef SCM_VECTORP
  #undef SCM_SYMBOLP
  #define SCM_STRINGP(Arg)            gh_string_p(Arg)
  #define SCM_VECTORP(Arg)            gh_vector_p(Arg)
  #define SCM_SYMBOLP(Arg)            gh_symbol_p(Arg)
#endif

#define XEN_SYMBOL_P(Arg)             (SCM_SYMBOLP(Arg))
#define XEN_PROCEDURE_P(Arg)          (XEN_NOT_FALSE_P(scm_procedure_p(Arg)))
#if HAVE_SCM_C_MAKE_RECTANGULAR
  #define XEN_STRING_P(Arg)           scm_is_string(Arg)
#else
  #define XEN_STRING_P(Arg)           (SCM_STRINGP(Arg))
#endif
#define XEN_VECTOR_P(Arg)             (SCM_VECTORP(Arg))
#define XEN_LIST_P(Arg)               (scm_ilength(Arg) >= 0)
#define XEN_LIST_P_WITH_LENGTH(Arg, Len) ((Len = ((int)scm_ilength(Arg))) >= 0)
#define XEN_HOOK_P(Arg)               (SCM_HOOKP(Arg))

#define XEN_LIST_LENGTH(Arg)          ((int)(scm_ilength(Arg)))
#define XEN_LIST_REF(Lst, Num)        scm_list_ref(Lst, C_TO_XEN_INT(Num))
#define XEN_LIST_SET(Lst, Loc, Val)   scm_list_set_x(Lst, C_TO_XEN_INT(Loc), Val)
#define XEN_CONS(Arg1, Arg2)          scm_cons(Arg1, Arg2)
#define XEN_CONS_2(Arg1, Arg2, Arg3)  scm_cons2(Arg1, Arg2, Arg3)
#if HAVE_SCM_LIST_N
  #define XEN_LIST_1(a)                   scm_list_1(a)
  #define XEN_LIST_2(a, b)                scm_list_2(a, b)
  #define XEN_LIST_3(a, b, c)             scm_list_3(a, b, c)
  #define XEN_LIST_4(a, b, c, d)          scm_list_4(a, b, c, d)
  #define XEN_LIST_5(a, b, c, d, e)       scm_list_5(a, b, c, d, e)
  #define XEN_LIST_6(a, b, c, d, e, f)    scm_list_n(a, b, c, d, e, f, XEN_UNDEFINED)
  #define XEN_LIST_7(a, b, c, d, e, f, g) scm_list_n(a, b, c, d, e, f, g, XEN_UNDEFINED)
  #define XEN_LIST_8(a, b, c, d, e, f, g, h) scm_list_n(a, b, c, d, e, f, g, h, XEN_UNDEFINED)
  #define XEN_LIST_9(a, b, c, d, e, f, g, h, i) scm_list_n(a, b, c, d, e, f, g, h, i, XEN_UNDEFINED)
#else
  #define XEN_LIST_1(a)                   SCM_LIST1(a)
  #define XEN_LIST_2(a, b)                SCM_LIST2(a, b)
  #define XEN_LIST_3(a, b, c)             SCM_LIST3(a, b, c)
  #define XEN_LIST_4(a, b, c, d)          SCM_LIST4(a, b, c, d)
  #define XEN_LIST_5(a, b, c, d, e)       SCM_LIST5(a, b, c, d, e)
  #define XEN_LIST_6(a, b, c, d, e, f)    SCM_LIST6(a, b, c, d, e, f)
  #define XEN_LIST_7(a, b, c, d, e, f, g) SCM_LIST7(a, b, c, d, e, f, g)
  #define XEN_LIST_8(a, b, c, d, e, f, g, h) SCM_LIST8(a, b, c, d, e, f, g, h)
  #define XEN_LIST_9(a, b, c, d, e, f, g, h, i) SCM_LIST9(a, b, c, d, e, f, g, h, i)
#endif
#define XEN_APPEND(a, b)                  scm_append(XEN_LIST_2(a, b))

#ifndef SCM_VECTOR_LENGTH
  #define XEN_VECTOR_LENGTH(Arg)      ((int)(gh_vector_length(Arg)))
#else
  #define XEN_VECTOR_LENGTH(Arg)      ((int)(SCM_VECTOR_LENGTH(Arg)))
#endif
#ifdef SCM_WRITABLE_VELTS
  #define XEN_VECTOR_ELEMENTS(a)      SCM_WRITABLE_VELTS(a)
#else
  #define XEN_VECTOR_ELEMENTS(a)      SCM_VELTS(a)
#endif

#ifdef SCM_VECTOR_REF
  #define XEN_VECTOR_REF(Vect, Num)      SCM_VECTOR_REF(Vect, Num)
  #define XEN_VECTOR_SET(Vect, Num, Val) SCM_VECTOR_SET(Vect, Num, Val)
#else
  #define XEN_VECTOR_REF(Vect, Num)      scm_vector_ref(Vect, C_TO_XEN_INT(Num))
  #define XEN_VECTOR_SET(Vect, Num, Val) scm_vector_set_x(Vect, C_TO_XEN_INT(Num), Val)
#endif

#define XEN_VECTOR_TO_LIST(Vect)      scm_vector_to_list(Vect)
#if HAVE_SCM_C_MAKE_VECTOR
  #define XEN_MAKE_VECTOR(Num, Fill)  scm_c_make_vector((unsigned long)(Num), Fill)
#else
  #define XEN_MAKE_VECTOR(Num, Fill)  scm_make_vector(C_TO_XEN_INT(Num), Fill)
#endif

#ifdef SCM_CHARP
  #define XEN_CHAR_P(Arg)             (SCM_CHARP(Arg))
#else
  #define XEN_CHAR_P(Arg)             gh_char_p(Arg)
#endif
#ifdef SCM_CHAR
  #define XEN_TO_C_CHAR(Arg)          SCM_CHAR(Arg)
#else
  #define XEN_TO_C_CHAR(Arg)          gh_scm2char(Arg)
#endif
#ifdef SCM_MAKE_CHAR
  #define C_TO_XEN_CHAR(c)            SCM_MAKE_CHAR(c)
#else
  #define C_TO_XEN_CHAR(c)            SCM_MAKICHR(c)
#endif

#if HAVE_SCM_MAKE_RATIO || HAVE_SCM_C_MAKE_RECTANGULAR
  #define XEN_NUMERATOR(Arg)          scm_numerator(Arg)
  #define XEN_DENOMINATOR(Arg)        scm_denominator(Arg)
  #define XEN_RATIONALIZE(Arg1, Arg2) scm_rationalize(scm_inexact_to_exact(Arg1), scm_inexact_to_exact(Arg2))
  #define XEN_RATIO_P(Arg)            SCM_FRACTIONP(Arg)
  #if HAVE_SCM_C_MAKE_RECTANGULAR
    #define XEN_MAKE_RATIO(Num, Den)  scm_divide(Num, Den)
  #else
    #define XEN_MAKE_RATIO(Num, Den)  scm_make_ratio(Num, Den)
  #endif
#endif

#define XEN_CONS_P(Arg)               SCM_CONSP(Arg)
#define XEN_PAIR_P(a)                 XEN_TRUE_P(scm_pair_p(a))

#define XEN_ARITY(Func)               scm_i_procedure_arity(Func)

#if (!WITH_HOBBIT)
#define XEN_REQUIRED_ARGS(Func)       XEN_TO_C_INT(XEN_CAR(XEN_ARITY(Func)))
#else
#define XEN_REQUIRED_ARGS(Func) \
  XEN_TO_C_INT(((!(SCM_CLOSUREP(Func))) && \
                (XEN_NOT_FALSE_P(scm_procedure_property(Func, scm_str2symbol("hobbit-numargs"))))) ? \
		 scm_procedure_property(Func,scm_str2symbol("hobbit-numargs")) : XEN_CAR(XEN_ARITY(Func)))
#endif

#define XEN_REQUIRED_ARGS_OK(Func, Args) (XEN_TO_C_INT(XEN_CAR(XEN_ARITY(Func))) == Args)
#ifdef SCM_CHARP
  #define XEN_KEYWORD_P(Obj)          (SCM_KEYWORDP(Obj))
#else
  #define XEN_KEYWORD_P(Obj)          XEN_TRUE_P(scm_keyword_p(Obj))
#endif
#define XEN_KEYWORD_EQ_P(k1, k2)      XEN_EQ_P(k1, k2)
#define XEN_MAKE_KEYWORD(Arg)         scm_c_make_keyword(Arg)
#define XEN_YES_WE_HAVE(Feature)      scm_add_feature(Feature)
#define XEN_PROTECT_FROM_GC(Obj)      scm_permanent_object(Obj)
#define XEN_LOAD_FILE(File)           scm_primitive_load(C_TO_XEN_STRING(File))

#define XEN_DEFINE_HOOK(Var, Name, Arity, Help) Var = xen_guile_create_hook(Name, Arity, Help, XEN_DOCUMENTATION_SYMBOL)
#define XEN_DEFINE_SIMPLE_HOOK(Var, Arity) Var = scm_make_hook(C_TO_XEN_INT(Arity))
#define XEN_CLEAR_HOOK(Arg)           scm_reset_hook_x(Arg)
#define XEN_HOOKED(a)                 (XEN_NOT_NULL_P(SCM_HOOK_PROCEDURES(a)))
#define XEN_HOOK_PROCEDURES(a)        SCM_HOOK_PROCEDURES(a)

/* disabling type checks saves almost no space (200k out of 12M) and no time (5% or so) */
#ifdef SCM_ASSERT_TYPE
  #define XEN_ASSERT_TYPE(Assertion, Arg, Position, Caller, Correct_Type) \
    do {SCM_ASSERT_TYPE(Assertion, Arg, Position, Caller, Correct_Type);} while (0) /* actual macro is unprotected if..then */

  #define XEN_WRONG_TYPE_ARG_ERROR(Caller, ArgN, Arg, Descr) \
    scm_wrong_type_arg_msg(Caller, ArgN, Arg, Descr)
#else
  #define XEN_ASSERT_TYPE(Assertion, Arg, Position, Caller, Correct_Type) \
    do {SCM_ASSERT(Assertion, Arg, Position, Caller);} while (0)

  #define XEN_WRONG_TYPE_ARG_ERROR(Caller, ArgN, Arg, Descr) \
    scm_wrong_type_arg(Caller, ArgN, Arg)
#endif

#define XEN_OUT_OF_RANGE_ERROR(Caller, ArgN, Arg, Descr) \
  XEN_ERROR(XEN_ERROR_TYPE("out-of-range"), \
            XEN_LIST_3(C_TO_XEN_STRING(Caller), \
                       C_TO_XEN_STRING(Descr), \
                       XEN_LIST_1(Arg)))

#ifdef WINDOZE
/* can't use scm_listofnull in Windows because the loader can't deal with it:
 *    snd-xen.o(.text+0x1456): In function `g_call1_1':
 *    snd-xen.c:482: variable '_scm_sys_protects' can't be auto-imported.
 * this definition taken from eval.c, listofnull is in root.h, set to SCM_BOOL_F in gc.c
 *
 * William Morgan says:
 *    scm_sys_protects need to be declared "__declspec (dllexport) extern" in
 *    the Guile root.h header file. [Or:]
 *    
 *    #if (defined(_WIN32) || defined(__CYGWIN__))
 *    __declspec (dllexport) extern scm_sys_protects;
 *    #endif
 */
  #define XEN_APPLY_ARG_LIST_END            scm_cons(SCM_EOL, SCM_EOL)
#else
  #define XEN_APPLY_ARG_LIST_END            scm_listofnull
#endif

#if USE_SND
  /* take advantage of Snd's elaborate error handling */
  #define XEN_CALL_0(Func, Caller)                   g_call0(Func, Caller)
  #define XEN_CALL_1(Func, Arg1, Caller)             g_call1(Func, Arg1, Caller)
  #define XEN_CALL_2(Func, Arg1, Arg2, Caller)       g_call2(Func, Arg1, Arg2, Caller)
  #define XEN_CALL_3(Func, Arg1, Arg2, Arg3, Caller) g_call3(Func, Arg1, Arg2, Arg3, Caller)
  #define XEN_APPLY(Func, Args, Caller)              g_call_any(Func, Args, Caller)
  #define XEN_CALL_4(Func, Arg1, Arg2, Arg3, Arg4, Caller) g_call_any(Func, XEN_LIST_4(Arg1, Arg2, Arg3, Arg4), Caller) 
  #define XEN_CALL_5(Func, Arg1, Arg2, Arg3, Arg4, Arg5, Caller) g_call_any(Func, XEN_LIST_5(Arg1, Arg2, Arg3, Arg4, Arg5), Caller) 
  #define XEN_CALL_6(Func, Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, Caller) g_call_any(Func, XEN_LIST_6(Arg1, Arg2, Arg3, Arg4, Arg5, Arg6), Caller) 
#else
  #define XEN_CALL_0(Func, Caller)                   scm_apply(Func, XEN_EMPTY_LIST, XEN_EMPTY_LIST)
  #define XEN_CALL_1(Func, Arg1, Caller)             scm_apply(Func, Arg1, XEN_APPLY_ARG_LIST_END)
  #define XEN_CALL_2(Func, Arg1, Arg2, Caller)       scm_apply(Func, Arg1, scm_cons(Arg2, XEN_APPLY_ARG_LIST_END))
  #define XEN_CALL_3(Func, Arg1, Arg2, Arg3, Caller) scm_apply(Func, Arg1, scm_cons2(Arg2, Arg3, XEN_APPLY_ARG_LIST_END))
  #define XEN_APPLY(Func, Args, Caller)              scm_apply(Func, Args, XEN_EMPTY_LIST)
  #define XEN_CALL_4(Func, Arg1, Arg2, Arg3, Arg4, Caller) \
     scm_apply(Func, Arg1, scm_cons2(Arg2, Arg3, scm_cons(Arg4, XEN_APPLY_ARG_LIST_END)))
  #define XEN_CALL_5(Func, Arg1, Arg2, Arg3, Arg4, Arg5, Caller) \
     scm_apply(Func, Arg1, scm_cons2(Arg2, Arg3, scm_cons2(Arg4, Arg5, XEN_APPLY_ARG_LIST_END)))
  #define XEN_CALL_6(Func, Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, Caller) \
     scm_apply(Func, Arg1, scm_cons2(Arg2, Arg3, scm_cons2(Arg4, Arg5, scm_cons(Arg6, XEN_APPLY_ARG_LIST_END))))
#endif
#define XEN_APPLY_NO_CATCH(Func, Args, Caller)              scm_apply(Func, Args, XEN_EMPTY_LIST)
#define XEN_CALL_0_NO_CATCH(Func, Caller)                   scm_apply(Func, XEN_EMPTY_LIST, XEN_EMPTY_LIST)
#define XEN_CALL_1_NO_CATCH(Func, Arg1, Caller)             scm_apply(Func, Arg1, XEN_APPLY_ARG_LIST_END)
#define XEN_CALL_2_NO_CATCH(Func, Arg1, Arg2, Caller)       scm_apply(Func, Arg1, scm_cons(Arg2, XEN_APPLY_ARG_LIST_END))
#define XEN_CALL_3_NO_CATCH(Func, Arg1, Arg2, Arg3, Caller) scm_apply(Func, Arg1, scm_cons2(Arg2, Arg3, XEN_APPLY_ARG_LIST_END))

#define XEN_PUTS(Str, Port)      scm_puts(Str, Port)
#define XEN_DISPLAY(Val, Port)   scm_display(Val, Port)
#define XEN_FLUSH_PORT(Port)     scm_force_output(Port)
#define XEN_CLOSE_PORT(Port)     scm_close_port(Port)
#define XEN_PORT_TO_STRING(Port) scm_strport_to_string(Port)

XEN xen_guile_create_hook(const char *name, int args, const char *help, XEN local_doc);
void xen_guile_define_procedure_with_setter(char *get_name, XEN (*get_func)(), char *get_help,
					    char *set_name, XEN (*set_func)(), 
					    XEN local_doc,
					    int get_req, int get_opt, int set_req, int set_opt);

void xen_guile_define_procedure_with_reversed_setter(char *get_name, XEN (*get_func)(), char *get_help,
						     char *set_name, XEN (*set_func)(), XEN (*reversed_set_func)(), 
						     XEN local_doc,
						     int get_req, int get_opt, int set_req, int set_opt);
double xen_to_c_double_or_else(XEN a, double b);
int xen_to_c_int(XEN a);
bool xen_integer_p(XEN a);
#if XEN_DEBUGGING
XEN xen_guile_dbg_new_procedure(const char *name, XEN (*func)(), int req, int opt, int rst);
#endif

#if HAVE_SCM_T_CATCH_BODY
  #define XEN_CATCH_BODY_TYPE scm_t_catch_body
#else
  #define XEN_CATCH_BODY_TYPE scm_catch_body_t
#endif

#if HAVE_SCM_C_MAKE_RECTANGULAR
char *xen_guile_to_c_string_with_eventual_free(XEN str);
#endif

#endif
/* end HAVE_GUILE */


/* ------------------------------ RUBY ------------------------------ */

#if HAVE_RUBY

#include <ruby.h>

#define XEN_OK 1

#define XEN                  VALUE
#define XEN_FALSE            Qfalse
#define XEN_TRUE             Qtrue
#define XEN_EMPTY_LIST       Qnil
#define XEN_UNDEFINED        ID2SYM(rb_intern(":undefined"))

#define XEN_FILE_EXTENSION  "rb"
#define XEN_COMMENT_STRING  "#"

#define XEN_ONLY_ARG 1
#define XEN_ARG_1 1
#define XEN_ARG_2 2
#define XEN_ARG_3 3
#define XEN_ARG_4 4
#define XEN_ARG_5 5
#define XEN_ARG_6 6
#define XEN_ARG_7 7
#define XEN_ARG_8 8
#define XEN_ARG_9 9

#ifdef __cplusplus
  #ifdef ANYARGS
    #define XEN_PROCEDURE_CAST (XEN (*)(ANYARGS))
    #define XEN_VALUE_ARG_PROCEDURE_CAST (XEN (*)(VALUE))
  #else
    #define XEN_PROCEDURE_CAST (XEN (*)())
    #define XEN_VALUE_ARG_PROCEDURE_CAST (XEN (*)())
  #endif
#else
  #define XEN_PROCEDURE_CAST
  #define XEN_VALUE_ARG_PROCEDURE_CAST
#endif

#define XEN_MARK_OBJECT_TYPE              void *
#define XEN_MAKE_AND_RETURN_OBJECT(Tag, Val, Mark, Free) return(Data_Wrap_Struct(Tag, Mark, Free, Val))
#define XEN_OBJECT_REF(a)                 DATA_PTR(a)
#define XEN_MAKE_OBJECT(Var, Tag, Val, Mark, Free) Var = Data_Wrap_Struct(Tag, Mark, Free, Val)
#define XEN_OBJECT_TYPE                   VALUE
#define XEN_OBJECT_TYPE_P(OBJ, TAG)       (XEN_BOUND_P(OBJ) && (rb_obj_is_instance_of(OBJ, TAG)))
#define XEN_MAKE_OBJECT_TYPE(Typ, Siz)    rb_define_class(xen_scheme_constant_to_ruby(Typ), rb_cObject)

#define XEN_MAKE_OBJECT_PRINT_PROCEDURE(Type, Wrapped_Print, Original_Print) \
  static XEN Wrapped_Print(XEN obj) \
  { \
    XEN val; \
    char *str; \
    str = Original_Print((Type *)XEN_OBJECT_REF(obj)); \
    val = C_TO_XEN_STRING(str); \
    FREE(str); \
    return(val); \
  }

#define XEN_MAKE_OBJECT_FREE_PROCEDURE(Type, Wrapped_Free, Original_Free) \
  static void *Wrapped_Free(XEN obj) \
  { \
    Original_Free((Type *)obj); \
    return(NULL); \
  } 

#define XEN_TRUE_P(a)                     ((a) == Qtrue)
#define XEN_FALSE_P(a)                    ((a) == Qfalse)
#define XEN_NULL_P(a)                     (((a) == Qnil) || ((a) == INT2NUM(0)) || (XEN_LIST_P(a) && XEN_LIST_LENGTH(a) == 0))
#define XEN_BOUND_P(Arg)                  ((Arg) != XEN_UNDEFINED)
#define XEN_NOT_BOUND_P(Arg)              ((Arg) == XEN_UNDEFINED)
#define XEN_ZERO                          INT2NUM(0)

#define XEN_TO_C_DOUBLE(a)                NUM2DBL(a)
#if defined(__GNUC__) && (!(defined(__cplusplus)))
  #define XEN_TO_C_DOUBLE_OR_ELSE(a, b) ({ XEN _xen_h_4_ = a; (XEN_NUMBER_P(_xen_h_4_) ? NUM2DBL(_xen_h_4_) : b); })
#else
  #define XEN_TO_C_DOUBLE_OR_ELSE(a, b) xen_rb_to_c_double_or_else(a, b)
#endif
#define XEN_TO_C_DOUBLE_WITH_CALLER(a, b) NUM2DBL(a)
#define C_TO_XEN_DOUBLE(a)                rb_float_new(a)

#define XEN_TO_C_INT(a)                   rb_num2long(a)
#if defined(__GNUC__) && (!(defined(__cplusplus)))
  #define XEN_TO_C_INT_OR_ELSE(a, b) ({ XEN _xen_h_5_ = a; (XEN_INTEGER_P(_xen_h_5_) ? FIX2INT(_xen_h_5_) : b); })
  #define XEN_TO_C_INT_OR_ELSE_WITH_CALLER(a, b, c) ({ XEN _xen_h_6_ = a; (XEN_INTEGER_P(_xen_h_6_) ? FIX2INT(_xen_h_6_) : b); })
#else
  #define XEN_TO_C_INT_OR_ELSE(a, b) xen_rb_to_c_int_or_else(a, b)
  #define XEN_TO_C_INT_OR_ELSE_WITH_CALLER(a, b, c) xen_rb_to_c_int_or_else(a, b)
#endif

#define C_TO_XEN_INT(a)                   INT2NUM(a)
#define XEN_TO_C_ULONG(a)                 NUM2ULONG(a)
#ifdef ULONG2NUM
  #define C_TO_XEN_ULONG(a)               ULONG2NUM((unsigned long)a)
#else
  #define C_TO_XEN_ULONG(a)               UINT2NUM((unsigned long)a)
#endif
#if HAVE_RB_NUM2LL
  #define C_TO_XEN_LONG_LONG(a)           C_TO_XEN_ULONG(a)
  #define XEN_TO_C_LONG_LONG(a)           rb_num2ll(a)
#else
  /* not ULONG here -- screws up negative ints */
  #define C_TO_XEN_LONG_LONG(a)           C_TO_XEN_INT(a)
  #define XEN_TO_C_LONG_LONG(a)           XEN_TO_C_INT(a)
#endif

#define C_TO_XEN_STRING(a)                xen_rb_str_new2((char *)a)
#define XEN_TO_C_STRING(Str)              RSTRING(Str)->ptr

#define C_TO_XEN_BOOLEAN(a)               ((a) ? Qtrue : Qfalse)
#define XEN_TO_C_BOOLEAN(a)               (!(XEN_FALSE_P(a)))

#define XEN_NAME_AS_C_STRING_TO_VALUE(a)  rb_gv_get(xen_scheme_global_variable_to_ruby(a))
#define C_STRING_TO_XEN_FORM(Str)         XEN_EVAL_C_STRING(Str)
#define XEN_EVAL_FORM(Form)               ((XEN)Form)
#define XEN_EVAL_C_STRING(Arg)            xen_rb_eval_string_with_error(Arg)
#define XEN_SYMBOL_TO_C_STRING(a)         rb_id2name(SYM2ID(a))
#define C_STRING_TO_XEN_SYMBOL(a)         ID2SYM(rb_intern(a))
#define XEN_TO_STRING(Obj)                xen_rb_obj_as_string(Obj)
#define XEN_LOAD_FILE(a)                  xen_rb_load_file_with_error(C_TO_XEN_STRING(a))

#define XEN_WRAP_C_POINTER(a)             Data_Wrap_Struct(rb_cData, 0, 0, (void *)a)
#define XEN_UNWRAP_C_POINTER(a)           DATA_PTR(a)
#define XEN_WRAPPED_C_POINTER_P(a)        (TYPE(a) == T_DATA)

#define XEN_PROCEDURE_SOURCE(Func)        Func

#if XEN_DEBUGGING
/* the otiose casts to int here are required by g++ */
#define XEN_DEFINE_PROCEDURE(Name, Func, ReqArg, OptArg, RstArg, Doc) \
  do { \
      if (((int)Func ## _Rst != -1) && (RstArg != 1)) fprintf(stderr,"%s rest args: %d %d?\n", Name, (int)Func ## _Rst, RstArg); \
      if (((int)Func ## _Rst == -1) && (RstArg != 0)) fprintf(stderr,"%s rest args: %d %d?\n", Name, (int)Func ## _Rst, RstArg); \
      if (((int)Func ## _Opt != 0) && (OptArg == 0)) fprintf(stderr,"%s opt args: %d %d?\n", Name, (int)Func ## _Opt, OptArg); \
      if (((int)Func ## _Opt == 0) && (OptArg != 0)) fprintf(stderr,"%s opt args: %d %d?\n", Name, (int)Func ## _Opt, OptArg); \
      if (((int)Func ## _Req != 0) && (OptArg != 0)) fprintf(stderr,"%s req/opt args: %d %d?\n", Name, (int)Func ## _Req, OptArg); \
      if (((int)Func ## _Req != 0) && (ReqArg != (int)Func ## _Req)) fprintf(stderr,"%s req args: %d %d?\n", Name, (int)Func ## _Req, ReqArg); \
      if (((int)Func ## _Req == 0) && (OptArg + ReqArg != (int)Func ## _Opt)) \
        fprintf(stderr,"%s total args: %d %d + %d?\n", Name, (int)Func ## _Opt, ReqArg, OptArg); \
      rb_define_global_function(xen_scheme_procedure_to_ruby(Name), XEN_PROCEDURE_CAST Func, ((RstArg > 0) ? -2 : ((OptArg > 0) ? -1 : ReqArg))); \
      if (Doc != NULL) xen_add_help(xen_scheme_procedure_to_ruby(Name), Doc); \
    } while (0)
#else
#define XEN_DEFINE_PROCEDURE(Name, Func, ReqArg, OptArg, RstArg, Doc) \
  do { \
      rb_define_global_function(xen_scheme_procedure_to_ruby(Name), XEN_PROCEDURE_CAST Func, ((RstArg > 0) ? -2 : (OptArg > 0) ? -1 : ReqArg)); \
      if (Doc != NULL) xen_add_help(xen_scheme_procedure_to_ruby(Name), Doc); \
    } while (0)
#endif

#define XEN_DEFINE_PROCEDURE_WITH_SETTER(Get_Name, Get_Func, Get_Help, Set_Name, Set_Func, Get_Req, Get_Opt, Set_Req, Set_Opt) \
  do { \
      XEN_DEFINE_PROCEDURE(Get_Name, XEN_PROCEDURE_CAST Get_Func, Get_Req, Get_Opt, 0, Get_Help); \
      XEN_DEFINE_PROCEDURE(Set_Name, XEN_PROCEDURE_CAST Set_Func, Set_Req, Set_Opt, 0, Get_Help); \
      if (Get_Help != NULL) xen_add_help(xen_scheme_procedure_to_ruby(Get_Name), Get_Help); \
   } while (0)

#define XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(Get_Name, Get_Func, Get_Help, Set_Name, Set_Func, Rev_Func, Get_Req, Get_Opt, Set_Req, Set_Opt) \
  do { \
      XEN_DEFINE_PROCEDURE(Get_Name, XEN_PROCEDURE_CAST Get_Func, Get_Req, Get_Opt, 0, Get_Help); \
      XEN_DEFINE_PROCEDURE(Set_Name, XEN_PROCEDURE_CAST Set_Func, Set_Req, Set_Opt, 0, Get_Help); \
      if (Get_Help != NULL) xen_add_help(xen_scheme_procedure_to_ruby(Get_Name), Get_Help); \
    } while (0)

#define XEN_DEFINE_CONSTANT(Name, Value, Help) \
  do { \
      rb_define_global_const(xen_scheme_constant_to_ruby(Name), C_TO_XEN_INT(Value)); \
      if (Help) xen_add_help(xen_scheme_constant_to_ruby(Name), Help); \
    } while (0)

#define XEN_DEFINE_VARIABLE(Name, Var, Value) \
  { \
    Var = Value; \
    rb_define_variable(xen_scheme_global_variable_to_ruby(Name), (VALUE *)(&Var)); \
  }

#define XEN_DEFINE_HOOK(Var, Name, Arity, Help) \
  { \
    Var = xen_rb_hook_c_new(xen_scheme_global_variable_to_ruby(Name), Arity, Help); \
    rb_define_variable(xen_scheme_global_variable_to_ruby(Name), (VALUE *)(&Var)); \
    if (Help != NULL) xen_add_help(xen_scheme_global_variable_to_ruby(Name), Help); \
  }
 
#define XEN_DEFINE_SIMPLE_HOOK(Var, Arity) \
  { \
    Var = xen_rb_hook_c_new("simple_hook", Arity, NULL); \
  }

#if defined(__GNUC__) && (!(defined(__cplusplus)))
  #define XEN_BOOLEAN_P(Arg)    ({ XEN _xen_h_7_ = Arg;        (XEN_TRUE_P(_xen_h_7_) || XEN_FALSE_P(_xen_h_7_)); })
  #define XEN_NUMBER_P(Arg)     ({ int _xen_h_8_ = TYPE(Arg);  ((_xen_h_8_ == T_FLOAT) || (_xen_h_8_ == T_FIXNUM) || (_xen_h_8_ == T_BIGNUM)); })
  #define XEN_INTEGER_P(Arg)    ({ int _xen_h_9_ = TYPE(Arg);  ((_xen_h_9_ == T_FIXNUM) || (_xen_h_9_ == T_BIGNUM)); })
  #define XEN_PROCEDURE_P(Arg)  ({ XEN _xen_h_10_ = Arg;       (XEN_BOUND_P(_xen_h_10_) && (rb_obj_is_kind_of(_xen_h_10_, rb_cProc))); })
  #define XEN_OFF_T_P(Arg)      ({ int _xen_h_11_ = TYPE(Arg); ((_xen_h_11_ == T_FIXNUM) || (_xen_h_11_ == T_BIGNUM)); })
  #define XEN_KEYWORD_P(Obj)    ({ XEN _xen_h_12_ = Obj;       (XEN_BOUND_P(_xen_h_12_) && SYMBOL_P(_xen_h_12_)); })
  #define XEN_LIST_P_WITH_LENGTH(Arg, Len) ({ XEN _xen_h_13_ = Arg; ((XEN_LIST_P(_xen_h_13_)) ? (Len = RARRAY(_xen_h_13_)->len) : (Len = 0)); })
#else
  #define XEN_BOOLEAN_P(Arg)    (XEN_TRUE_P(Arg) || XEN_FALSE_P(Arg))
  #define XEN_NUMBER_P(Arg)     ((TYPE(Arg) == T_FLOAT) || (TYPE(Arg) == T_FIXNUM) || (TYPE(Arg) == T_BIGNUM))
  #define XEN_INTEGER_P(Arg)    ((TYPE(Arg) == T_FIXNUM) || (TYPE(Arg) == T_BIGNUM))
  #define XEN_PROCEDURE_P(Arg)  (XEN_BOUND_P(Arg) && (rb_obj_is_kind_of(Arg, rb_cProc)))
  #define XEN_OFF_T_P(Arg)      ((TYPE(Arg) == T_FIXNUM) || (TYPE(Arg) == T_BIGNUM))
  #define XEN_KEYWORD_P(Obj)    (XEN_BOUND_P(Obj) && SYMBOL_P(Obj))
  #define XEN_LIST_P_WITH_LENGTH(Arg, Len) ((XEN_LIST_P(Arg)) ? (Len = RARRAY(Arg)->len) : (Len = 0))
#endif

#define XEN_STRING_P(Arg)       (TYPE(Arg) == T_STRING)
#define XEN_VECTOR_P(Arg)       (TYPE(Arg) == T_ARRAY)
#define XEN_SYMBOL_P(Arg)       SYMBOL_P(Arg)
#define XEN_DOUBLE_P(Arg)       XEN_NUMBER_P(Arg)
#define XEN_ULONG_P(Arg1)       XEN_INTEGER_P(Arg1)
#define XEN_EXACT_P(Arg1)       XEN_INTEGER_P(Arg1)
#define XEN_HOOK_P(Arg)         (xen_rb_is_hook_p(Arg) || XEN_PROCEDURE_P(Arg))

/* apparently no complex numbers (built-in) in Ruby? */

#define XEN_LIST_P(Arg)                 (TYPE(Arg) == T_ARRAY)
#define XEN_CONS_P(Arg)                 (TYPE(Arg) == T_ARRAY)
#define XEN_PAIR_P(Arg)                 (TYPE(Arg) == T_ARRAY)
#define XEN_LIST_LENGTH(Arg)            RARRAY(Arg)->len
#define XEN_EQ_P(a, b)                  ((a) == (b))
#define XEN_EQV_P(a, b)                 ((a) == (b))
#define XEN_EQUAL_P(a, b)               ((a) == (b))
#define XEN_LIST_1(a)                   rb_ary_new3(1, a)
#define XEN_LIST_2(a, b)                rb_ary_new3(2, a, b) 
#define XEN_LIST_3(a, b, c)             rb_ary_new3(3, a, b, c) 
#define XEN_LIST_4(a, b, c, d)          rb_ary_new3(4, a, b, c, d) 
#define XEN_LIST_5(a, b, c, d, e)       rb_ary_new3(5, a, b, c, d, e) 
#define XEN_LIST_6(a, b, c, d, e, f)    rb_ary_new3(6, a, b, c, d, e, f)
#define XEN_LIST_7(a, b, c, d, e, f, g) rb_ary_new3(7, a, b, c, d, e, f, g)
#define XEN_LIST_8(a, b, c, d, e, f, g, h) rb_ary_new3(8, a, b, c, d, e, f, g, h)
#define XEN_LIST_9(a, b, c, d, e, f, g, h, i) rb_ary_new3(9, a, b, c, d, e, f, g, h, i)
#define XEN_CAR(a)                      rb_ary_entry(a, 0)
#define XEN_CADR(a)                     rb_ary_entry(a, 1)
#define XEN_CADDR(a)                    rb_ary_entry(a, 2)
#define XEN_CADDDR(a)                   rb_ary_entry(a, 3)
#define XEN_CDR(a)                      xen_rb_cdr(a)
#define XEN_CDDR(a)                     XEN_CDR(XEN_CDR(a))
#define XEN_COPY_ARG(Lst)               xen_rb_copy_list(Lst)
#define XEN_CONS(Arg1, Arg2)            xen_rb_cons(Arg1, Arg2)
#define XEN_CONS_2(Arg1, Arg2, Arg3)    xen_rb_cons2(Arg1, Arg2, Arg3)
#define XEN_LIST_REF(Lst, Num)          rb_ary_entry(Lst, Num)
#define XEN_LIST_SET(Lst, Num, Val)     rb_ary_store(Lst, Num, Val)
#define XEN_APPEND(X, Y)                rb_ary_concat(X, Y)

#define XEN_HOOK_PROCEDURES(a)          ((xen_rb_is_hook_p(a)) ? xen_rb_hook_to_a(a) : ((XEN_NULL_P(a)) ? Qnil : XEN_LIST_1(a)))
#define XEN_CLEAR_HOOK(a)               ((xen_rb_is_hook_p(a)) ? xen_rb_hook_reset_hook(a) : (a = Qnil))
#define XEN_HOOKED(a)                   XEN_NOT_NULL_P(XEN_HOOK_PROCEDURES(a))
#define XEN_VARIABLE_SET(a, b)          rb_gv_set(xen_scheme_global_variable_to_ruby(a), b)
#define XEN_VARIABLE_REF(a)             rb_gv_get(xen_scheme_global_variable_to_ruby(a))

#define XEN_VECTOR_ELEMENTS(a)          RARRAY(a)->ptr
#define XEN_VECTOR_LENGTH(Arg)          RARRAY(Arg)->len
#define XEN_VECTOR_REF(Vect, Num)       rb_ary_entry(Vect, Num)
#define XEN_VECTOR_SET(a, b, c)         rb_ary_store(a, b, c)
#define XEN_MAKE_VECTOR(Num, Fill)      xen_rb_ary_new_with_initial_element(Num, Fill)
#define XEN_VECTOR_TO_LIST(a)           a

#define XEN_CHAR_P(Arg)                  XEN_STRING_P(Arg)
#define XEN_TO_C_CHAR(Arg)               RSTRING(Arg)->ptr[0]
#define C_TO_XEN_CHAR(Arg)               rb_str_new((char *)(&(Arg)), 1)

#define XEN_CALL_0(Func, Caller)                   xen_rb_funcall_0(Func)
#define XEN_CALL_1(Func, Arg1, Caller)             rb_funcall(Func, rb_intern("call"), 1, Arg1)
#define XEN_CALL_2(Func, Arg1, Arg2, Caller)       rb_funcall(Func, rb_intern("call"), 2, Arg1, Arg2)
#define XEN_CALL_3(Func, Arg1, Arg2, Arg3, Caller) rb_funcall(Func, rb_intern("call"), 3, Arg1, Arg2, Arg3)
#define XEN_CALL_4(Func, Arg1, Arg2, Arg3, Arg4, Caller) rb_funcall(Func, rb_intern("call"), 4, Arg1, Arg2, Arg3, Arg4)
#define XEN_CALL_5(Func, Arg1, Arg2, Arg3, Arg4, Arg5, Caller) rb_funcall(Func, rb_intern("call"), 5, Arg1, Arg2, Arg3, Arg4, Arg5)
#define XEN_CALL_6(Func, Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, Caller) rb_funcall(Func, rb_intern("call"), 6, Arg1, Arg2, Arg3, Arg4, Arg5, Arg6)
#define XEN_APPLY(Func, Args, Caller)              xen_rb_apply(Func, Args)
#define XEN_APPLY_ARG_LIST_END          Qnil
#define XEN_CALL_0_NO_CATCH(Func, Caller)                   xen_rb_funcall_0(Func)
#define XEN_CALL_1_NO_CATCH(Func, Arg1, Caller)             rb_funcall(Func, rb_intern("call"), 1, Arg1)
#define XEN_CALL_2_NO_CATCH(Func, Arg1, Arg2, Caller)       rb_funcall(Func, rb_intern("call"), 2, Arg1, Arg2)
#define XEN_CALL_3_NO_CATCH(Func, Arg1, Arg2, Arg3, Caller) rb_funcall(Func, rb_intern("call"), 3, Arg1, Arg2, Arg3)
#define XEN_APPLY_NO_CATCH(Func, Args, Caller)              xen_rb_apply(Func, Args)

#define XEN_ARITY(Func)                 rb_funcall(Func, rb_intern("arity"), 0)
#define XEN_REQUIRED_ARGS(Func)         xen_rb_required_args(XEN_ARITY(Func))
#define XEN_REQUIRED_ARGS_OK(Func, Args) (xen_rb_required_args(XEN_ARITY(Func)) == Args)
#define XEN_KEYWORD_EQ_P(k1, k2)        ((k1) == (k2))
#define XEN_MAKE_KEYWORD(Arg)           C_STRING_TO_XEN_SYMBOL(Arg)
#define XEN_YES_WE_HAVE(a)              rb_provide(a)
#define XEN_DOCUMENTATION_SYMBOL        rb_intern("documentation")
#define XEN_PROTECT_FROM_GC(Var)        rb_gc_register_address(&(Var))
#define XEN_UNPROTECT_FROM_GC(Var)      rb_gc_unregister_address(&(Var))

#define XEN_ERROR_TYPE(Name)            rb_intern(xen_scheme_constant_to_ruby(Name))

#define XEN_ASSERT_TYPE(Assertion, Arg, Position, Caller, Correct_Type) \
  if (!(Assertion)) \
    rb_raise(rb_eTypeError, "%s: wrong type arg %d, %s, wanted %s\n", \
             Caller, Position, XEN_TO_C_STRING(XEN_TO_STRING(Arg)), Correct_Type)

#define XEN_WRONG_TYPE_ARG_ERROR(Caller, ArgN, Arg, Descr) \
  rb_raise(rb_eTypeError, "%s: wrong type arg %d, %s, wanted %s\n", \
           Caller, ArgN, XEN_TO_C_STRING(XEN_TO_STRING(Arg)), Descr)

#if USE_SND
#define XEN_ERROR(Type, Info)           snd_rb_raise(Type, Info)
#define XEN_OUT_OF_RANGE_ERROR(Caller, ArgN, Arg, Descr) \
  snd_rb_raise(XEN_ERROR_TYPE("out-of-range"), \
            XEN_LIST_3(C_TO_XEN_STRING(Caller), \
                       C_TO_XEN_STRING(Descr), \
                       XEN_LIST_1(Arg)))
#else
#define XEN_ERROR(Type, Info)           xen_rb_raise(Type, Info)
#define XEN_OUT_OF_RANGE_ERROR(Caller, ArgN, Arg, Descr) \
  rb_raise(rb_eRangeError, "%s: arg %d, %s, out of range: %s\n", \
           Caller, ArgN, XEN_TO_C_STRING(XEN_TO_STRING(Arg)), Descr)
#endif
#define XEN_THROW(Type, Info)           xen_rb_raise(Type, Info)

#if XEN_DEBUGGING

/* -------------------------------- XEN_DEBUGGING (arg checks in XEN_DEFINE_PROCEDURE) -------------------------------- */
#define XEN_ARGIFY_1(OutName, InName) \
  static int OutName ## _Req = 0; \
  static int OutName ## _Opt = 1; \
  static int OutName ## _Rst = -1; \
  static XEN OutName(int argc, XEN *argv, XEN self) \
  { \
    return(InName((argc > 0) ? argv[0] : XEN_UNDEFINED)); \
  }

#define XEN_ARGIFY_2(OutName, InName) \
  static int OutName ## _Req = 0; \
  static int OutName ## _Opt = 2; \
  static int OutName ## _Rst = -1; \
  static XEN OutName(int argc, XEN *argv, XEN self) \
  { \
    return(InName((argc > 0) ? argv[0] : XEN_UNDEFINED, \
		  (argc > 1) ? argv[1] : XEN_UNDEFINED)); \
  }

#define XEN_ARGIFY_3(OutName, InName) \
  static int OutName ## _Req = 0; \
  static int OutName ## _Opt = 3; \
  static int OutName ## _Rst = -1; \
  static XEN OutName(int argc, XEN *argv, XEN self) \
  { \
    return(InName((argc > 0) ? argv[0] : XEN_UNDEFINED, \
		  (argc > 1) ? argv[1] : XEN_UNDEFINED, \
		  (argc > 2) ? argv[2] : XEN_UNDEFINED)); \
  }

#define XEN_ARGIFY_4(OutName, InName) \
  static int OutName ## _Req = 0; \
  static int OutName ## _Opt = 4; \
  static int OutName ## _Rst = -1; \
  static XEN OutName(int argc, XEN *argv, XEN self) \
  { \
    return(InName((argc > 0) ? argv[0] : XEN_UNDEFINED, \
		  (argc > 1) ? argv[1] : XEN_UNDEFINED, \
		  (argc > 2) ? argv[2] : XEN_UNDEFINED, \
		  (argc > 3) ? argv[3] : XEN_UNDEFINED)); \
  }

#define XEN_ARGIFY_5(OutName, InName) \
  static int OutName ## _Req = 0; \
  static int OutName ## _Opt = 5; \
  static int OutName ## _Rst = -1; \
  static XEN OutName(int argc, XEN *argv, XEN self) \
  { \
    return(InName((argc > 0) ? argv[0] : XEN_UNDEFINED, \
		  (argc > 1) ? argv[1] : XEN_UNDEFINED, \
		  (argc > 2) ? argv[2] : XEN_UNDEFINED, \
		  (argc > 3) ? argv[3] : XEN_UNDEFINED, \
		  (argc > 4) ? argv[4] : XEN_UNDEFINED)); \
  }

#define XEN_ARGIFY_6(OutName, InName) \
  static int OutName ## _Req = 0; \
  static int OutName ## _Opt = 6; \
  static int OutName ## _Rst = -1; \
  static XEN OutName(int argc, XEN *argv, XEN self) \
  { \
    return(InName((argc > 0) ? argv[0] : XEN_UNDEFINED, \
		  (argc > 1) ? argv[1] : XEN_UNDEFINED, \
		  (argc > 2) ? argv[2] : XEN_UNDEFINED, \
		  (argc > 3) ? argv[3] : XEN_UNDEFINED, \
		  (argc > 4) ? argv[4] : XEN_UNDEFINED, \
		  (argc > 5) ? argv[5] : XEN_UNDEFINED)); \
  }

#define XEN_ARGIFY_7(OutName, InName) \
  static int OutName ## _Req = 0; \
  static int OutName ## _Opt = 7; \
  static int OutName ## _Rst = -1; \
  static XEN OutName(int argc, XEN *argv, XEN self) \
  { \
    return(InName((argc > 0) ? argv[0] : XEN_UNDEFINED, \
		  (argc > 1) ? argv[1] : XEN_UNDEFINED, \
		  (argc > 2) ? argv[2] : XEN_UNDEFINED, \
		  (argc > 3) ? argv[3] : XEN_UNDEFINED, \
		  (argc > 4) ? argv[4] : XEN_UNDEFINED, \
		  (argc > 5) ? argv[5] : XEN_UNDEFINED, \
		  (argc > 6) ? argv[6] : XEN_UNDEFINED)); \
  }

#define XEN_ARGIFY_8(OutName, InName) \
  static int OutName ## _Req = 0; \
  static int OutName ## _Opt = 8; \
  static int OutName ## _Rst = -1; \
  static XEN OutName(int argc, XEN *argv, XEN self) \
  { \
    return(InName((argc > 0) ? argv[0] : XEN_UNDEFINED, \
		  (argc > 1) ? argv[1] : XEN_UNDEFINED, \
		  (argc > 2) ? argv[2] : XEN_UNDEFINED, \
		  (argc > 3) ? argv[3] : XEN_UNDEFINED, \
		  (argc > 4) ? argv[4] : XEN_UNDEFINED, \
		  (argc > 5) ? argv[5] : XEN_UNDEFINED, \
		  (argc > 6) ? argv[6] : XEN_UNDEFINED, \
		  (argc > 7) ? argv[7] : XEN_UNDEFINED)); \
  }

#define XEN_ARGIFY_9(OutName, InName) \
  static int OutName ## _Req = 0; \
  static int OutName ## _Opt = 9; \
  static int OutName ## _Rst = -1; \
  static XEN OutName(int argc, XEN *argv, XEN self) \
  { \
    return(InName((argc > 0) ? argv[0] : XEN_UNDEFINED, \
		  (argc > 1) ? argv[1] : XEN_UNDEFINED, \
		  (argc > 2) ? argv[2] : XEN_UNDEFINED, \
		  (argc > 3) ? argv[3] : XEN_UNDEFINED, \
		  (argc > 4) ? argv[4] : XEN_UNDEFINED, \
		  (argc > 5) ? argv[5] : XEN_UNDEFINED, \
		  (argc > 6) ? argv[6] : XEN_UNDEFINED, \
		  (argc > 7) ? argv[7] : XEN_UNDEFINED, \
		  (argc > 8) ? argv[8] : XEN_UNDEFINED)); \
  }

#define XEN_ARGIFY_10(OutName, InName) \
  static int OutName ## _Req = 0; \
  static int OutName ## _Opt = 10; \
  static int OutName ## _Rst = -1; \
  static XEN OutName(int argc, XEN *argv, XEN self) \
  { \
    return(InName((argc > 0) ? argv[0] : XEN_UNDEFINED, \
		  (argc > 1) ? argv[1] : XEN_UNDEFINED, \
		  (argc > 2) ? argv[2] : XEN_UNDEFINED, \
		  (argc > 3) ? argv[3] : XEN_UNDEFINED, \
		  (argc > 4) ? argv[4] : XEN_UNDEFINED, \
		  (argc > 5) ? argv[5] : XEN_UNDEFINED, \
		  (argc > 6) ? argv[6] : XEN_UNDEFINED, \
		  (argc > 7) ? argv[7] : XEN_UNDEFINED, \
		  (argc > 8) ? argv[8] : XEN_UNDEFINED, \
		  (argc > 9) ? argv[9] : XEN_UNDEFINED)); \
  }

#define XEN_NARGIFY_0(OutName, InName) \
  static int OutName ## _Req = 0; \
  static int OutName ## _Opt = 0; \
  static int OutName ## _Rst = -1; \
  static XEN OutName(void) {return(InName());}

#define XEN_NARGIFY_1(OutName, InName) \
  static int OutName ## _Req = 1; \
  static int OutName ## _Opt = 0; \
  static int OutName ## _Rst = -1; \
  static XEN OutName(XEN self, XEN Arg) {return(InName(Arg));}
#define XEN_NARGIFY_2(OutName, InName) \
  static int OutName ## _Req = 2; \
  static int OutName ## _Opt = 0; \
  static int OutName ## _Rst = -1; \
  static XEN OutName(XEN self, XEN Arg1, XEN Arg2) {return(InName(Arg1, Arg2));}
#define XEN_NARGIFY_3(OutName, InName) \
  static int OutName ## _Req = 3; \
  static int OutName ## _Opt = 0; \
  static int OutName ## _Rst = -1; \
  static XEN OutName(XEN self, XEN Arg1, XEN Arg2, XEN Arg3) {return(InName(Arg1, Arg2, Arg3));}
#define XEN_NARGIFY_4(OutName, InName) \
  static int OutName ## _Req = 4; \
  static int OutName ## _Opt = 0; \
  static int OutName ## _Rst = -1; \
  static XEN OutName(XEN self, XEN Arg1, XEN Arg2, XEN Arg3, XEN Arg4) {return(InName(Arg1, Arg2, Arg3, Arg4));}
#define XEN_NARGIFY_5(OutName, InName) \
  static int OutName ## _Req = 5; \
  static int OutName ## _Opt = 0; \
  static int OutName ## _Rst = -1; \
  static XEN OutName(XEN self, XEN Arg1, XEN Arg2, XEN Arg3, XEN Arg4, XEN Arg5) {return(InName(Arg1, Arg2, Arg3, Arg4, Arg5));}
#define XEN_NARGIFY_6(OutName, InName) \
  static int OutName ## _Req = 6; \
  static int OutName ## _Opt = 0; \
  static int OutName ## _Rst = -1; \
  static XEN OutName(XEN self, XEN Arg1, XEN Arg2, XEN Arg3, XEN Arg4, XEN Arg5, XEN Arg6) \
    {return(InName(Arg1, Arg2, Arg3, Arg4, Arg5, Arg6));}
#define XEN_NARGIFY_7(OutName, InName) \
  static int OutName ## _Req = 7; \
  static int OutName ## _Opt = 0; \
  static int OutName ## _Rst = -1; \
  static XEN OutName(XEN self, XEN Arg1, XEN Arg2, XEN Arg3, XEN Arg4, XEN Arg5, XEN Arg6, XEN Arg7) \
    {return(InName(Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, Arg7));}
#define XEN_NARGIFY_8(OutName, InName) \
  static int OutName ## _Req = 8; \
  static int OutName ## _Opt = 0; \
  static int OutName ## _Rst = -1; \
  static XEN OutName(XEN self, XEN Arg1, XEN Arg2, XEN Arg3, XEN Arg4, XEN Arg5, XEN Arg6, XEN Arg7, XEN Arg8) \
    {return(InName(Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, Arg7, Arg8));}
#define XEN_NARGIFY_9(OutName, InName) \
  static int OutName ## _Req = 9; \
  static int OutName ## _Opt = 0; \
  static int OutName ## _Rst = -1; \
  static XEN OutName(XEN self, XEN Arg1, XEN Arg2, XEN Arg3, XEN Arg4, XEN Arg5, XEN Arg6, XEN Arg7, XEN Arg8, XEN Arg9) \
    {return(InName(Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, Arg7, Arg8, Arg9));}

#define XEN_VARGIFY(OutName, InName) \
  static int OutName ## _Req = 0; \
  static int OutName ## _Opt = 0; \
  static int OutName ## _Rst = 1; \
  static XEN OutName(XEN self, XEN Args) {return(InName(Args));}

#else

/* -------------------------------- not XEN_DEBUGGING -------------------------------- */
#define XEN_ARGIFY_1(OutName, InName) \
  static XEN OutName(int argc, XEN *argv, XEN self) \
  { \
    return(InName((argc > 0) ? argv[0] : XEN_UNDEFINED)); \
  }

#define XEN_ARGIFY_2(OutName, InName) \
  static XEN OutName(int argc, XEN *argv, XEN self) \
  { \
    return(InName((argc > 0) ? argv[0] : XEN_UNDEFINED, \
		  (argc > 1) ? argv[1] : XEN_UNDEFINED)); \
  }

#define XEN_ARGIFY_3(OutName, InName) \
  static XEN OutName(int argc, XEN *argv, XEN self) \
  { \
    return(InName((argc > 0) ? argv[0] : XEN_UNDEFINED, \
		  (argc > 1) ? argv[1] : XEN_UNDEFINED, \
		  (argc > 2) ? argv[2] : XEN_UNDEFINED)); \
  }

#define XEN_ARGIFY_4(OutName, InName) \
  static XEN OutName(int argc, XEN *argv, XEN self) \
  { \
    return(InName((argc > 0) ? argv[0] : XEN_UNDEFINED, \
		  (argc > 1) ? argv[1] : XEN_UNDEFINED, \
		  (argc > 2) ? argv[2] : XEN_UNDEFINED, \
		  (argc > 3) ? argv[3] : XEN_UNDEFINED)); \
  }

#define XEN_ARGIFY_5(OutName, InName) \
  static XEN OutName(int argc, XEN *argv, XEN self) \
  { \
    return(InName((argc > 0) ? argv[0] : XEN_UNDEFINED, \
		  (argc > 1) ? argv[1] : XEN_UNDEFINED, \
		  (argc > 2) ? argv[2] : XEN_UNDEFINED, \
		  (argc > 3) ? argv[3] : XEN_UNDEFINED, \
		  (argc > 4) ? argv[4] : XEN_UNDEFINED)); \
  }

#define XEN_ARGIFY_6(OutName, InName) \
  static XEN OutName(int argc, XEN *argv, XEN self) \
  { \
    return(InName((argc > 0) ? argv[0] : XEN_UNDEFINED, \
		  (argc > 1) ? argv[1] : XEN_UNDEFINED, \
		  (argc > 2) ? argv[2] : XEN_UNDEFINED, \
		  (argc > 3) ? argv[3] : XEN_UNDEFINED, \
		  (argc > 4) ? argv[4] : XEN_UNDEFINED, \
		  (argc > 5) ? argv[5] : XEN_UNDEFINED)); \
  }

#define XEN_ARGIFY_7(OutName, InName) \
  static XEN OutName(int argc, XEN *argv, XEN self) \
  { \
    return(InName((argc > 0) ? argv[0] : XEN_UNDEFINED, \
		  (argc > 1) ? argv[1] : XEN_UNDEFINED, \
		  (argc > 2) ? argv[2] : XEN_UNDEFINED, \
		  (argc > 3) ? argv[3] : XEN_UNDEFINED, \
		  (argc > 4) ? argv[4] : XEN_UNDEFINED, \
		  (argc > 5) ? argv[5] : XEN_UNDEFINED, \
		  (argc > 6) ? argv[6] : XEN_UNDEFINED)); \
  }

#define XEN_ARGIFY_8(OutName, InName) \
  static XEN OutName(int argc, XEN *argv, XEN self) \
  { \
    return(InName((argc > 0) ? argv[0] : XEN_UNDEFINED, \
		  (argc > 1) ? argv[1] : XEN_UNDEFINED, \
		  (argc > 2) ? argv[2] : XEN_UNDEFINED, \
		  (argc > 3) ? argv[3] : XEN_UNDEFINED, \
		  (argc > 4) ? argv[4] : XEN_UNDEFINED, \
		  (argc > 5) ? argv[5] : XEN_UNDEFINED, \
		  (argc > 6) ? argv[6] : XEN_UNDEFINED, \
		  (argc > 7) ? argv[7] : XEN_UNDEFINED)); \
  }

#define XEN_ARGIFY_9(OutName, InName) \
  static XEN OutName(int argc, XEN *argv, XEN self) \
  { \
    return(InName((argc > 0) ? argv[0] : XEN_UNDEFINED, \
		  (argc > 1) ? argv[1] : XEN_UNDEFINED, \
		  (argc > 2) ? argv[2] : XEN_UNDEFINED, \
		  (argc > 3) ? argv[3] : XEN_UNDEFINED, \
		  (argc > 4) ? argv[4] : XEN_UNDEFINED, \
		  (argc > 5) ? argv[5] : XEN_UNDEFINED, \
		  (argc > 6) ? argv[6] : XEN_UNDEFINED, \
		  (argc > 7) ? argv[7] : XEN_UNDEFINED, \
		  (argc > 8) ? argv[8] : XEN_UNDEFINED)); \
  }

#define XEN_ARGIFY_10(OutName, InName) \
  static XEN OutName(int argc, XEN *argv, XEN self) \
  { \
    return(InName((argc > 0) ? argv[0] : XEN_UNDEFINED, \
		  (argc > 1) ? argv[1] : XEN_UNDEFINED, \
		  (argc > 2) ? argv[2] : XEN_UNDEFINED, \
		  (argc > 3) ? argv[3] : XEN_UNDEFINED, \
		  (argc > 4) ? argv[4] : XEN_UNDEFINED, \
		  (argc > 5) ? argv[5] : XEN_UNDEFINED, \
		  (argc > 6) ? argv[6] : XEN_UNDEFINED, \
		  (argc > 7) ? argv[7] : XEN_UNDEFINED, \
		  (argc > 8) ? argv[8] : XEN_UNDEFINED, \
		  (argc > 9) ? argv[9] : XEN_UNDEFINED)); \
  }

#define XEN_NARGIFY_0(OutName, InName) static XEN OutName(void) {return(InName());}
#define XEN_NARGIFY_1(OutName, InName) static XEN OutName(XEN self, XEN Arg) {return(InName(Arg));}
#define XEN_NARGIFY_2(OutName, InName) static XEN OutName(XEN self, XEN Arg1, XEN Arg2) {return(InName(Arg1, Arg2));}
#define XEN_NARGIFY_3(OutName, InName) static XEN OutName(XEN self, XEN Arg1, XEN Arg2, XEN Arg3) {return(InName(Arg1, Arg2, Arg3));}
#define XEN_NARGIFY_4(OutName, InName) static XEN OutName(XEN self, XEN Arg1, XEN Arg2, XEN Arg3, XEN Arg4) {return(InName(Arg1, Arg2, Arg3, Arg4));}
#define XEN_NARGIFY_5(OutName, InName) static XEN OutName(XEN self, XEN Arg1, XEN Arg2, XEN Arg3, XEN Arg4, XEN Arg5) {return(InName(Arg1, Arg2, Arg3, Arg4, Arg5));}
#define XEN_NARGIFY_6(OutName, InName) static XEN OutName(XEN self, XEN Arg1, XEN Arg2, XEN Arg3, XEN Arg4, XEN Arg5, XEN Arg6) \
  {return(InName(Arg1, Arg2, Arg3, Arg4, Arg5, Arg6));}
#define XEN_NARGIFY_7(OutName, InName) static XEN OutName(XEN self, XEN Arg1, XEN Arg2, XEN Arg3, XEN Arg4, XEN Arg5, XEN Arg6, XEN Arg7) \
  {return(InName(Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, Arg7));}
#define XEN_NARGIFY_8(OutName, InName) static XEN OutName(XEN self, XEN Arg1, XEN Arg2, XEN Arg3, XEN Arg4, XEN Arg5, XEN Arg6, XEN Arg7, XEN Arg8) \
  {return(InName(Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, Arg7, Arg8));}
#define XEN_NARGIFY_9(OutName, InName) static XEN OutName(XEN self, XEN Arg1, XEN Arg2, XEN Arg3, XEN Arg4, XEN Arg5, XEN Arg6, XEN Arg7, XEN Arg8, XEN Arg9) \
  {return(InName(Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, Arg7, Arg8, Arg9));}

#define XEN_VARGIFY(OutName, InName) static XEN OutName(XEN self, XEN Args) {return(InName(Args));}
#endif

XEN xen_rb_cdr(XEN val);
XEN xen_rb_cons(XEN arg1, XEN arg2);
XEN xen_rb_cons2(XEN arg1, XEN arg2, XEN arg3);
char *xen_scheme_constant_to_ruby(const char *name);
char *xen_scheme_procedure_to_ruby(const char *name);
char *xen_scheme_global_variable_to_ruby(const char *name);
void xen_rb_raise(XEN type, XEN info);
XEN xen_rb_obj_as_string(XEN obj);
XEN xen_rb_eval_string_with_error(char *str);
XEN xen_rb_load_file_with_error(XEN file);
XEN xen_rb_ary_new_with_initial_element(long num, XEN element);
XEN xen_rb_apply(XEN func, XEN args);
XEN xen_rb_funcall_0(XEN func);
int xen_rb_required_args(XEN val);
XEN xen_rb_copy_list(XEN val); /* Ruby arrays (lists) are passed by reference */
XEN xen_rb_str_new2(char *arg);
void xen_add_help(char *name, const char *help);
char *xen_help(char *name);
double xen_rb_to_c_double_or_else(XEN a, double b);
int xen_rb_to_c_int_or_else(XEN a, int b);
/* class Hook */
VALUE xen_rb_is_hook_p(VALUE hook);
VALUE xen_rb_hook_c_new(char *name, int arity, char *help);
VALUE xen_rb_hook_reset_hook(VALUE hook);
VALUE xen_rb_hook_to_a(VALUE hook);
void Init_Hook(void);
typedef XEN (*XEN_CATCH_BODY_TYPE) (void *data);
#endif
/* end HAVE_RUBY */


/* ------------------------------ NO EXTENSION LANGUAGE ------------------------------ */

#ifndef XEN_OK

#define XEN int
#define XEN_FALSE 0
#define XEN_TRUE 1
#define XEN_EMPTY_LIST 0
#define XEN_UNDEFINED 0

#define XEN_FILE_EXTENSION  "txt"
#define XEN_COMMENT_STRING  ";"

#define XEN_EQ_P(a, b) 0
#define XEN_EQV_P(a, b) 0
#define XEN_EQUAL_P(a, b) 0
#define XEN_EMPTY_LIST 0
#define XEN_LIST_1(a) 0
#define XEN_LIST_2(a, b) 0
#define XEN_LIST_3(a, b, c) 0
#define XEN_LIST_4(a, b, c, d) 0
#define XEN_LIST_5(a, b, c, d, e) 0
#define XEN_LIST_6(a, b, c, d, e, f) 0
#define XEN_LIST_7(a, b, c, d, e, f, g) 0
#define XEN_LIST_8(a, b, c, d, e, f, g, h) 0
#define XEN_LIST_9(a, b, c, d, e, f, g, h, i) 0
#define XEN_CAR(a) 0
#define XEN_CADR(a) 0
#define XEN_CADDR(a) 0
#define XEN_CADDDR(a) 0
#define XEN_CDR(a) 0
#define XEN_CDDR(a) 0
#define XEN_COPY_ARG(Lst) Lst
#define XEN_VECTOR_ELEMENTS(a) ((XEN *)a)
#define XEN_MARK_OBJECT_TYPE         XEN
#define XEN_MAKE_OBJECT(a, b, c, ig1, ig2)
#define XEN_MAKE_OBJECT_TYPE(Typ, Siz) 0
#define XEN_MAKE_OBJECT_PRINT_PROCEDURE(Type, Wrapped_Print, Original_Print) 
#define XEN_MAKE_OBJECT_FREE_PROCEDURE(Type, Wrapped_Free, Original_Free)
#define XEN_HOOK_PROCEDURES(a) 0
#define XEN_VARIABLE_SET(a, b)
#define XEN_VARIABLE_REF(a) 0
#define XEN_PROCEDURE_SOURCE(Func) 0

#define XEN_MAKE_AND_RETURN_OBJECT(Tag, Val, ig1, ig2) return(0)
#define XEN_OBJECT_REF(a) 0
#define XEN_NAME_AS_C_STRING_TO_VALUE(a) 0
#define XEN_OBJECT_TYPE int
#define XEN_OBJECT_TYPE_P(OBJ, TAG) 0
#define XEN_TRUE_P(a) ((a) == XEN_TRUE)
#define XEN_FALSE_P(a) ((a) == XEN_FALSE)
#define XEN_NULL_P(a) ((a) == XEN_EMPTY_LIST)
#define XEN_BOUND_P(Arg) 0
#define XEN_NOT_BOUND_P(Arg) 1
#define XEN_ZERO 0
#define XEN_DOUBLE_P(Arg) 0

#define XEN_TO_C_DOUBLE(a) 0.0
#define XEN_TO_C_DOUBLE_OR_ELSE(a, b) b
#define XEN_TO_C_DOUBLE_WITH_CALLER(a, b) 0.0
#define XEN_TO_C_INT(a) 0
#define XEN_TO_C_INT_OR_ELSE(a, b) b
#define XEN_TO_C_INT_OR_ELSE_WITH_CALLER(a, b, c) b
#define XEN_TO_C_STRING(STR) NULL
#define C_TO_XEN_DOUBLE(a) 0
#define C_TO_XEN_INT(a) a
#define C_TO_XEN_LONG_LONG(a) a
#define XEN_TO_C_LONG_LONG(a) a
#define C_TO_XEN_STRING(a) 0
#define C_TO_XEN_BOOLEAN(a) 0
#define C_STRING_TO_XEN_SYMBOL(a) 0
#define XEN_TO_C_BOOLEAN(a) 0
#define C_STRING_TO_XEN_FORM(Str) 0
#define XEN_EVAL_FORM(Form) 0
#define XEN_SYMBOL_TO_C_STRING(a) 0
#define XEN_WRAP_C_POINTER(a) 0
#define XEN_UNWRAP_C_POINTER(a) 0
#define XEN_WRAPPED_C_POINTER_P(a) 0
#define XEN_HOOKED(a) 0
#define XEN_DEFINE_PROCEDURE(Name, Func, ReqArg, OptArg, RstArg, Doc)
#define XEN_DEFINE_PROCEDURE_WITH_SETTER(Get_Name, Get_Func, Get_Help, Set_Name, Set_Func, Get_Req, Get_Opt, Set_Req, Set_Opt)
#define XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(Get_Name, Get_Func, Get_Help, Set_Name, Set_Func, Rev_Func, Get_Req, Get_Opt, Set_Req, Set_Opt)
#define XEN_DEFINE_CONSTANT(a, b, c)
#define XEN_DEFINE_VARIABLE(a, b, c)
#define XEN_BOOLEAN_P(Arg) 0
#define XEN_NUMBER_P(Arg) 0
#define XEN_INTEGER_P(Arg) 0
#define XEN_OFF_T_P(Arg) 0
#define XEN_SYMBOL_P(Arg) 0
#define XEN_STRING_P(Arg) 0
#define XEN_VECTOR_P(Arg) 0
#define XEN_TO_C_ULONG(a) 0
#define C_TO_XEN_ULONG(a) 0
#define XEN_ULONG_P(Arg) 0
#define XEN_HOOK_P(Arg) 0
#define XEN_EXACT_P(Arg) 0
#define XEN_LIST_P(Arg) 0
#define XEN_CONS_P(Arg) 0
#define XEN_PAIR_P(Arg) 0
#define XEN_LIST_P_WITH_LENGTH(Arg, Len) 0
#define XEN_LIST_LENGTH(Arg) 0
#define XEN_VECTOR_LENGTH(Arg) 0
#define XEN_PROCEDURE_P(Arg) 0
#define XEN_CONS(Arg1, Arg2) 0
#define XEN_CONS_2(Arg1, Arg2, Arg3) 0
#define XEN_LIST_REF(Lst, Num) 0
#define XEN_LIST_SET(Lst, Num, Val)
#define XEN_VECTOR_REF(Vect, Num) 0
#define XEN_VECTOR_SET(a, b, c)
#define XEN_EVAL_C_STRING(Arg) 0
#define XEN_MAKE_VECTOR(Num, Fill) 0
#define XEN_VECTOR_TO_LIST(Vect) 0
#define XEN_DEFINE_HOOK(Var, Name, Arity, Help)
#define XEN_DEFINE_SIMPLE_HOOK(Var, Arity)
#define XEN_CLEAR_HOOK(Arg)
#define XEN_CHAR_P(Arg) 0
#define XEN_TO_C_CHAR(Arg) 0
#define C_TO_XEN_CHAR(Arg) 0
#define XEN_CALL_0(Func, Caller) 0
#define XEN_CALL_1(Func, Arg1, Caller) 0
#define XEN_CALL_2(Func, Arg1, Arg2, Caller) 0
#define XEN_CALL_3(Func, Arg1, Arg2, Arg3, Caller) 0
#define XEN_CALL_4(Func, Arg1, Arg2, Arg3, Arg4, Caller) 0
#define XEN_CALL_5(Func, Arg1, Arg2, Arg3, Arg4, Arg5, Caller) 0
#define XEN_CALL_6(Func, Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, Caller) 0
#define XEN_APPLY(Func, Args, Caller) 0
#define XEN_APPLY_ARG_LIST_END 0
#define XEN_CALL_0_NO_CATCH(Func, Caller) 0
#define XEN_CALL_1_NO_CATCH(Func, Arg1, Caller) 0
#define XEN_CALL_2_NO_CATCH(Func, Arg1, Arg2, Caller) 0
#define XEN_CALL_3_NO_CATCH(Func, Arg1, Arg2, Arg3, Caller) 0
#define XEN_APPLY_NO_CATCH(Func, Args, Caller) 0
#define XEN_ARITY(Func) 0
#define XEN_REQUIRED_ARGS(Func) 0
#define XEN_REQUIRED_ARGS_OK(Func, Args) false
#define XEN_KEYWORD_P(Obj) 0
#define XEN_KEYWORD_EQ_P(k1, k2) 0
#define XEN_MAKE_KEYWORD(Arg) 0
#define XEN_YES_WE_HAVE(Feature)
#define XEN_DOCUMENTATION_SYMBOL 0
#define XEN_ASSERT_TYPE(Assertion, Arg, Position, Caller, Correct_Type)
#define XEN_PROTECT_FROM_GC(a) 0
#define XEN_LOAD_FILE(a) 0
#define XEN_ERROR_TYPE(Typ) XEN_FALSE
#define XEN_ERROR(Type, Info) fprintf(stderr, "error")
#define XEN_THROW(Type, Info) fprintf(stderr, "error")
#define XEN_TO_STRING(Obj) "(unknown)"
#define XEN_WRONG_TYPE_ARG_ERROR(Caller, ArgN, Arg, Descr)
#define XEN_OUT_OF_RANGE_ERROR(Caller, ArgN, Arg, Descr)
#define XEN_APPEND(X, Y) 0
#define XEN_ONLY_ARG 0
#define XEN_ARG_1    0
#define XEN_ARG_2    0
#define XEN_ARG_3    0
#define XEN_ARG_4    0
#define XEN_ARG_5    0
#define XEN_ARG_6    0
#define XEN_ARG_7    0
#define XEN_ARG_8    0
#define XEN_ARG_9    0
typedef XEN (*XEN_CATCH_BODY_TYPE) (void *data);

#endif
/* end NO EXTENSION LANGUAGE */

#define XEN_NOT_TRUE_P(a)  (!(XEN_TRUE_P(a)))
#define XEN_NOT_FALSE_P(a) (!(XEN_FALSE_P(a)))
#define XEN_NOT_NULL_P(a)  (!(XEN_NULL_P(a)))

#if defined(__GNUC__) && (!(defined(__cplusplus)))
  #define XEN_BOOLEAN_IF_BOUND_P(Arg)            ({ XEN _xen_h_14_ = Arg; ((XEN_BOOLEAN_P(_xen_h_14_))   || (XEN_NOT_BOUND_P(_xen_h_14_))); })
  #define XEN_INTEGER_IF_BOUND_P(Arg)            ({ XEN _xen_h_15_ = Arg; ((XEN_NOT_BOUND_P(_xen_h_15_)) || (XEN_INTEGER_P(_xen_h_15_))); })
  #define XEN_NUMBER_IF_BOUND_P(Arg)             ({ XEN _xen_h_16_ = Arg; ((XEN_NOT_BOUND_P(_xen_h_16_)) || (XEN_NUMBER_P(_xen_h_16_))); })
  #define XEN_STRING_IF_BOUND_P(Arg)             ({ XEN _xen_h_17_ = Arg; ((XEN_NOT_BOUND_P(_xen_h_17_)) || (XEN_STRING_P(_xen_h_17_))); })
  #define XEN_INTEGER_OR_BOOLEAN_IF_BOUND_P(Arg) ({ XEN _xen_h_18_ = Arg; ((XEN_BOOLEAN_P(_xen_h_18_)) || (XEN_NOT_BOUND_P(_xen_h_18_)) || (XEN_INTEGER_P(_xen_h_18_))); })
  #define XEN_NUMBER_OR_BOOLEAN_IF_BOUND_P(Arg)  ({ XEN _xen_h_19_ = Arg; ((XEN_BOOLEAN_P(_xen_h_19_)) || (XEN_NOT_BOUND_P(_xen_h_19_)) || (XEN_NUMBER_P(_xen_h_19_))); })
  #define XEN_NUMBER_OR_BOOLEAN_P(Arg)           ({ XEN _xen_h_20_ = Arg; ((XEN_BOOLEAN_P(_xen_h_20_))   || (XEN_NUMBER_P(_xen_h_20_))); })
  #define XEN_INTEGER_OR_BOOLEAN_P(Arg)          ({ XEN _xen_h_21_ = Arg; ((XEN_BOOLEAN_P(_xen_h_21_))   || (XEN_INTEGER_P(_xen_h_21_))); })
  #define XEN_ULONG_IF_BOUND_P(Arg)              ({ XEN _xen_h_22_ = Arg; ((XEN_NOT_BOUND_P(_xen_h_22_)) || (XEN_ULONG_P(_xen_h_22_))); })
#else
  #define XEN_BOOLEAN_IF_BOUND_P(Arg)            ((XEN_BOOLEAN_P(Arg))   || (XEN_NOT_BOUND_P(Arg)))
  #define XEN_INTEGER_IF_BOUND_P(Arg)            ((XEN_NOT_BOUND_P(Arg)) || (XEN_INTEGER_P(Arg)))
  #define XEN_NUMBER_IF_BOUND_P(Arg)             ((XEN_NOT_BOUND_P(Arg)) || (XEN_NUMBER_P(Arg)))
  #define XEN_STRING_IF_BOUND_P(Arg)             ((XEN_NOT_BOUND_P(Arg)) || (XEN_STRING_P(Arg)))
  #define XEN_INTEGER_OR_BOOLEAN_IF_BOUND_P(Arg) ((XEN_BOOLEAN_P(Arg))   || (XEN_NOT_BOUND_P(Arg)) || (XEN_INTEGER_P(Arg)))
  #define XEN_NUMBER_OR_BOOLEAN_IF_BOUND_P(Arg)  ((XEN_BOOLEAN_P(Arg))   || (XEN_NOT_BOUND_P(Arg)) || (XEN_NUMBER_P(Arg)))
  #define XEN_NUMBER_OR_BOOLEAN_P(Arg)           ((XEN_BOOLEAN_P(Arg))   || (XEN_NUMBER_P(Arg)))
  #define XEN_INTEGER_OR_BOOLEAN_P(Arg)          ((XEN_BOOLEAN_P(Arg))   || (XEN_INTEGER_P(Arg)))
  #define XEN_ULONG_IF_BOUND_P(Arg)              ((XEN_NOT_BOUND_P(Arg)) || (XEN_ULONG_P(Arg)))
#endif

#define XEN_TO_C_OFF_T_OR_ELSE(a, b)  xen_to_c_off_t_or_else(a, b, c__FUNCTION__)
#define C_TO_XEN_OFF_T(a)             c_to_xen_off_t(a)
#define XEN_TO_C_OFF_T(a)             xen_to_c_off_t(a)
#define XEN_AS_STRING(form)           XEN_TO_C_STRING(XEN_TO_STRING(form))

#define XEN_BAD_ARITY_ERROR(Caller, ArgN, Arg, Descr) \
  XEN_ERROR(XEN_ERROR_TYPE("bad-arity"), \
            XEN_LIST_3(C_TO_XEN_STRING(Caller), \
                       C_TO_XEN_STRING(Descr), \
                       Arg))

#ifndef XEN_RATIO_P
  #define XEN_NUMERATOR(Arg)          0
  #define XEN_DENOMINATOR(Arg)        1
  #define XEN_RATIONALIZE(Arg1, Arg2) 1
  #define XEN_RATIO_P(Arg)            false
  #define XEN_MAKE_RATIO(Num, Den)    1
#endif
#ifndef XEN_DEFINED_P
  #define XEN_DEFINED_P(Name) false
#endif

XEN xen_return_first(XEN a, ...);
int xen_to_c_int_or_else(XEN obj, int fallback, const char *origin);
off_t xen_to_c_off_t_or_else(XEN obj, off_t fallback, const char *origin);
off_t xen_to_c_off_t(XEN obj);
XEN c_to_xen_off_t(off_t val);
char *xen_version(void);
void xen_repl(int argc, char **argv);
void xen_initialize(void);
void xen_gc_mark(XEN val);

#ifndef XEN_DISABLE_DEPRECATED
  #define C_TO_SMALL_XEN_INT(a) C_TO_XEN_INT(a)
  #define XEN_TO_SMALL_C_INT(a) XEN_TO_C_INT(a)
  #define XEN_TO_C_BOOLEAN_OR_TRUE(a) (!(XEN_FALSE_P(a)))
#endif
#endif
