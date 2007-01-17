#ifndef XEN_H
#define XEN_H

/* macros for extension language support 
 *
 * Guile:     covers 1.3.4 to present (1.8.0)
 * Ruby:      covers 1.6 to present (1.9)
 * Forth:     covers 1.0
 * Gauche:    covers 0.8.7 and 0.8.8
 * None:      covers all known versions of None
 */

#define XEN_MAJOR_VERSION 2
#define XEN_MINOR_VERSION 6
#define XEN_VERSION "2.6"

/* HISTORY:
 *
 *  17-Jan-07: rb_errinfo changes (Mike Scholz).
 *  --------
 *  14-Nov-06: check for Scm_EvalRec (Gauche 0.8.8).
 *  9-Sep-06:  XEN_LOAD_PATH and XEN_ADD_TO_LOAD_PATH
 *  1-Sep-06:  string and array changes for Ruby (from Mike).
 *  7-Aug-06:  more careful list length handling in Ruby (from Mike).
 *  23-May-06: added xen_rb_repl_set_prompt to set (no-gui) Ruby repl prompt.
 *  12-May-06: changed HAVE_RATIOS to XEN_HAVE_RATIOS.
 *  17-Apr-06: removed XEN_MAKE_OBJECT.
 *  15-Apr-06: Gauche support.
 *  28-Mar-06: Forth support thanks to Mike Scholz.
 *  --------
 *  7-Nov-05:  xen_rb_defined_p (Mike Scholz).
 *  24-Oct-05: XEN_LOAD_FILE_WITH_PATH.
 *  16-Sep-05: removed some debugging extras that caused confusion on 64-bit machines.
 *  12-Aug-05: include guile setter procedure names for better error reporting.
 *  14-Jun-05: XEN_DEFINE (XEN value, not assumed to be int as in XEN_DEFINE_CONSTANT).
 *             XEN_ASSOC, XEN_MEMBER, and XEN_PROCEDURE_NAME for Scheme side.
 *             XEN_DEFINE_HOOK and XEN_DEFINE_SIMPLE_HOOK no longer take the "Var" arg.
 *  18-May-05: deprecate XEN_NUMBER_OR_BOOLEAN_IF_BOUND_P and XEN_NUMBER_OR_BOOLEAN_P.
 *  29-Mar-05: C_TO_XEN_STRINGN changes.
 *  24-Mar-05: Ruby properties (Mike Scholz).
 *  8-Mar-05:  Ruby improvements in keywords and hooks (Mike Scholz).
 *  7-Mar-05:  C99 complex number changes (creal, _Complex_I) (Steve Bankowitz).
 *  2-Mar-05:  Ruby support for off_t (Mike Scholz).
 *  4-Jan-05:  more guile changes, deprecated XEN_VECTOR_ELEMENTS.
 *  --------
 *  31-Dec-04: removed "caller" arg from *_NO_CATCH.
 *  10-Nov-04: scm_c_vector* (new Guile functions)
 *  21-Oct-04: XEN_LIST_REVERSE, (using rb_ary_dup available in 1.8)
 *  7-Oct-04:  keyword changes for new Guile.
 *  28-Sep-04: deprecated *_WITH_CALLER -- these no longer do anything useful in Guile.
 *             NaNs and Infs -> 0 or 0.0 in XEN_TO_C_INT|DOUBLE -- perhaps I should add another set of macros?
 *  23-Aug-04: more Guile name changes.
 *  12-Aug-04: more Guile name changes, C_TO_XEN_STRINGN (Guile)
 *  3-Aug-04:  xen_to_c_int bugfix thanks to Kjetil S. Matheussen.
 *  29-Jul-04: deprecated XEN_TO_C_BOOLEAN_OR_TRUE.
 *  21-Jul-04: deprecated XEN_TO_SMALL_C_INT and C_TO_SMALL_XEN_INT.
 *             use new Guile 1.7 numerical function names (under flag HAVE_SCM_TO_SIGNED_INTEGER).
 *  28-Jun-04: XEN_REQUIRED_ARGS_OK to make it easier to turn off this check.
 *  9-June-04: complex number conversions (Guile) -- Ruby complex numbers are an optional module?
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
#include <sys/types.h>
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
#if (HAVE_SCM_NUM2INT || HAVE_SCM_C_MAKE_RECTANGULAR)
  #include <libguile.h>
#else
  #include <guile/gh.h>
#endif

#define XEN_OK 1

#define XEN                          SCM
#define XEN_FILE_EXTENSION           "scm"
#define XEN_COMMENT_STRING           ";"
#define XEN_LANGUAGE_NAME            "Guile"

#define XEN_TRUE                     SCM_BOOL_T
#define XEN_FALSE                    SCM_BOOL_F
#define XEN_TRUE_P(a)                XEN_EQ_P(a, XEN_TRUE)
#define XEN_FALSE_P(a)               XEN_EQ_P(a, XEN_FALSE)
#define C_TO_XEN_BOOLEAN(a)          ((a) ? XEN_TRUE : XEN_FALSE)
#define XEN_TO_C_BOOLEAN(a)          (!(XEN_FALSE_P(a)))

#define XEN_UNDEFINED                SCM_UNDEFINED
#define XEN_BOUND_P(Arg)             (!(SCM_UNBNDP(Arg)))
#define XEN_NOT_BOUND_P(Arg)         SCM_UNBNDP(Arg)

#if HAVE_SCM_TO_SIGNED_INTEGER
  #define XEN_EQ_P(a, b)             scm_is_eq(a, b)
#else
  #ifndef SCM_EQ_P
    #define SCM_EQ_P(a, b)           ((a) == (b))
  #endif
  #define XEN_EQ_P(a, b)             SCM_EQ_P(a, b)
#endif
#define XEN_EQV_P(A, B)              XEN_TO_C_BOOLEAN(scm_eqv_p(A, B))
#define XEN_EQUAL_P(A, B)            XEN_TO_C_BOOLEAN(scm_equal_p(A, B))

#if HAVE_SCM_CAR
  #define XEN_NULL_P(a)              scm_is_null(a)
  #define XEN_CONS_P(Arg)            scm_is_pair(Arg)
  #define XEN_PAIR_P(Arg)            scm_is_pair(Arg)
#else
  #define XEN_NULL_P(a)              SCM_NULLP(a)
  #define XEN_CONS_P(Arg)            SCM_CONSP(Arg)
  #define XEN_PAIR_P(Arg)            XEN_TRUE_P(scm_pair_p(Arg))
#endif

#define XEN_EMPTY_LIST               SCM_EOL
#define XEN_CONS(Arg1, Arg2)         scm_cons(Arg1, Arg2)
#define XEN_CONS_2(Arg1, Arg2, Arg3) scm_cons2(Arg1, Arg2, Arg3)

/* should SCM_CAR -> scm_car? (snd-run also) -- this appears to be a pointless change */
#define XEN_CAR(a)                   SCM_CAR(a)
#define XEN_CADR(a)                  SCM_CADR(a)
#define XEN_CADDR(a)                 SCM_CADDR(a)
#define XEN_CADDDR(a)                SCM_CADDDR(a)
#define XEN_CDR(a)                   SCM_CDR(a)
#define XEN_CDDR(a)                  SCM_CDDR(a)
#define XEN_COPY_ARG(Lst)            Lst

/* ---- numbers ---- */
#define XEN_ZERO                     SCM_INUM0
#define XEN_INTEGER_P(Arg)           xen_integer_p(Arg)
#define XEN_TO_C_INT(a)              xen_to_c_int(a)
#define XEN_TO_C_INT_OR_ELSE(a, b)   xen_to_c_int_or_else(a, b)
#define XEN_TO_C_DOUBLE(a)           xen_to_c_double(a)  
#define XEN_TO_C_DOUBLE_OR_ELSE(a, b) xen_to_c_double_or_else(a, b)  

/* all the number handlers changed (names...) in 1.7 */
#if HAVE_SCM_TO_SIGNED_INTEGER
  #define C_TO_XEN_INT(a)            scm_from_int(a)
  #define XEN_DOUBLE_P(Arg)          ((bool)scm_is_real(Arg))
  #define C_TO_XEN_DOUBLE(a)         scm_from_double(a)
  #define XEN_ULONG_P(Arg1)          (XEN_NOT_FALSE_P(scm_number_p(Arg1)))
  #define XEN_TO_C_ULONG(a)          scm_to_ulong(a)
  #define C_TO_XEN_ULONG(a)          scm_from_ulong((unsigned long)a)
  #define C_TO_XEN_LONG_LONG(a)      scm_from_long_long(a)
  #define XEN_TO_C_LONG_LONG(a)      scm_to_long_long(a)
  #define XEN_EXACT_P(Arg)           XEN_TRUE_P(scm_exact_p(Arg))
  #define XEN_BOOLEAN_P(Arg)         ((bool)scm_is_bool(Arg))
  #define XEN_NUMBER_P(Arg)          ((bool)scm_is_real(Arg))
  #define XEN_OFF_T_P(Arg)           ((bool)scm_is_integer(Arg))
#else
  #define C_TO_XEN_INT(a)            scm_long2num((long)a)
  #define XEN_DOUBLE_P(Arg)          (XEN_NOT_FALSE_P(scm_real_p(Arg)))
  #if HAVE_SCM_MAKE_REAL
    #define C_TO_XEN_DOUBLE(a)       scm_make_real(a)
  #else
    #define C_TO_XEN_DOUBLE(a)       scm_makdbl(a, 0.0)
  #endif
  #define XEN_TO_C_ULONG(a)          scm_num2ulong(a, 0, c__FUNCTION__)
  #define C_TO_XEN_ULONG(a)          scm_ulong2num((unsigned long)a)
  #define XEN_ULONG_P(Arg1)          (XEN_NOT_FALSE_P(scm_number_p(Arg1)))
  #if HAVE_SCM_NUM2LONG_LONG
    #define C_TO_XEN_LONG_LONG(a)    scm_long_long2num(a)
    #define XEN_TO_C_LONG_LONG(a)    scm_num2long_long(a, 0, c__FUNCTION__)
  #else
    #define C_TO_XEN_LONG_LONG(a)    scm_long2num(a)
    #define XEN_TO_C_LONG_LONG(a)    scm_num2long(a, 0, c__FUNCTION__)
  #endif
  #define XEN_EXACT_P(Arg)           XEN_TRUE_P(scm_exact_p(Arg))
  #define XEN_BOOLEAN_P(Arg)         (SCM_BOOLP(Arg))
  #define XEN_NUMBER_P(Arg)          (XEN_NOT_FALSE_P(scm_real_p(Arg)))
  #define XEN_OFF_T_P(Arg)           (XEN_NOT_FALSE_P(scm_integer_p(Arg)))
#endif

#if HAVE_COMPLEX_TRIG
  #if HAVE_SCM_C_MAKE_RECTANGULAR
    #define XEN_COMPLEX_P(Arg)       scm_is_complex(Arg)
    #if defined(__GNUC__) && (!(defined(__cplusplus)))
      #define XEN_TO_C_COMPLEX(a)    ({ XEN _xen_h_23_ = a; (scm_c_real_part(_xen_h_23_) + scm_c_imag_part(_xen_h_23_) * _Complex_I); })    
      #define C_TO_XEN_COMPLEX(a)    ({ complex _xen_h_24_ = a; scm_c_make_rectangular(creal(_xen_h_24_), cimag(_xen_h_24_)); })
    #else
      #define XEN_TO_C_COMPLEX(a)    (scm_c_real_part(a) + scm_c_imag_part(a) * _Complex_I)
      #define C_TO_XEN_COMPLEX(a)    scm_c_make_rectangular(creal(a), cimag(a))
    #endif
  #else
    #define XEN_COMPLEX_P(Arg)       (XEN_NOT_FALSE_P(scm_number_p(Arg)))
    #define XEN_TO_C_COMPLEX(a)      XEN_TO_C_DOUBLE(scm_real_part(a)) + (XEN_TO_C_DOUBLE(scm_imag_part(a)) * _Complex_I)
    #define C_TO_XEN_COMPLEX(a)      scm_make_complex(creal(a), cimag(a))
  #endif
#endif

#if HAVE_SCM_MAKE_RATIO || HAVE_SCM_C_MAKE_RECTANGULAR
  #define XEN_HAVE_RATIOS                 1
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

/* ---- lists ---- */
#define XEN_LIST_P(Arg)               (scm_ilength(Arg) >= 0)
#define XEN_LIST_P_WITH_LENGTH(Arg, Len) ((Len = ((int)scm_ilength(Arg))) >= 0)
#define XEN_LIST_LENGTH(Arg)          ((int)(scm_ilength(Arg)))
#define XEN_LIST_REF(Lst, Num)        scm_list_ref(Lst, C_TO_XEN_INT(Num))
#define XEN_LIST_SET(Lst, Loc, Val)   scm_list_set_x(Lst, C_TO_XEN_INT(Loc), Val)
#define XEN_LIST_REVERSE(Lst)         scm_reverse(Lst)
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
/* these are only used in Scheme-side stuff, so aren't defined in other cases; in Ruby the args are reversed, I think: rb_ary_assoc */
#define XEN_ASSOC(a, b)                   scm_assoc(a, b)
#define XEN_MEMBER(a, b)                  scm_member(a, b)

/* ---- vectors ---- */
#if HAVE_SCM_IS_SIMPLE_VECTOR
  #define XEN_VECTOR_P(Arg)               scm_is_simple_vector(Arg)
  #define XEN_VECTOR_LENGTH(Arg)          SCM_SIMPLE_VECTOR_LENGTH(Arg)
  #define XEN_VECTOR_REF(Vect, Num)       SCM_SIMPLE_VECTOR_REF(Vect, Num)
  #define XEN_VECTOR_SET(Vect, Num, Val)  SCM_SIMPLE_VECTOR_SET(Vect, Num, Val)
#else
  #if HAVE_SCM_IS_VECTOR
    #define XEN_VECTOR_P(Arg)              scm_is_vector(Arg)
    #define XEN_VECTOR_LENGTH(Arg)         scm_c_vector_length(Arg)
    #define XEN_VECTOR_REF(Vect, Num)      scm_c_vector_ref(Vect, Num)
    #define XEN_VECTOR_SET(Vect, Num, Val) scm_c_vector_set_x(Vect, Num, Val)
  #else
    #define XEN_VECTOR_P(Arg)             (SCM_VECTORP(Arg))
    #ifndef SCM_VECTOR_LENGTH
      #define XEN_VECTOR_LENGTH(Arg)      ((int)(gh_vector_length(Arg)))
    #else
      #define XEN_VECTOR_LENGTH(Arg)      ((int)(SCM_VECTOR_LENGTH(Arg)))
    #endif
    #ifdef SCM_VECTOR_REF
      #define XEN_VECTOR_REF(Vect, Num)      SCM_VECTOR_REF(Vect, Num)
      #define XEN_VECTOR_SET(Vect, Num, Val) SCM_VECTOR_SET(Vect, Num, Val)
    #else
      #define XEN_VECTOR_REF(Vect, Num)      scm_vector_ref(Vect, C_TO_XEN_INT(Num))
      #define XEN_VECTOR_SET(Vect, Num, Val) scm_vector_set_x(Vect, C_TO_XEN_INT(Num), Val)
    #endif
  #endif
#endif

#define XEN_VECTOR_TO_LIST(Vect)      scm_vector_to_list(Vect)
#if HAVE_SCM_C_MAKE_VECTOR
  #define XEN_MAKE_VECTOR(Num, Fill)  scm_c_make_vector((unsigned long)(Num), Fill)
#else
  #define XEN_MAKE_VECTOR(Num, Fill)  scm_make_vector(C_TO_XEN_INT(Num), Fill)
#endif

/* ---- hooks ---- */
#define XEN_HOOK_P(Arg)               (SCM_HOOKP(Arg))
#define XEN_DEFINE_HOOK(Name, Arity, Help) xen_guile_create_hook(Name, Arity, Help, XEN_DOCUMENTATION_SYMBOL)
#define XEN_DEFINE_SIMPLE_HOOK(Arity) scm_make_hook(C_TO_XEN_INT(Arity))
#define XEN_CLEAR_HOOK(Arg)           scm_reset_hook_x(Arg)
#define XEN_HOOKED(a)                 (XEN_NOT_NULL_P(SCM_HOOK_PROCEDURES(a)))
#define XEN_HOOK_PROCEDURES(a)        SCM_HOOK_PROCEDURES(a)

/* ---- characters, strings, keywords ---- */
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

#if HAVE_SCM_FROM_LOCALE_KEYWORD
  #define XEN_KEYWORD_P(Obj)          scm_is_keyword(Obj)
  #define XEN_MAKE_KEYWORD(Arg)       scm_from_locale_keyword(Arg)
#else
  #ifdef SCM_CHARP
    #define XEN_KEYWORD_P(Obj)        (SCM_KEYWORDP(Obj))
  #else
    #define XEN_KEYWORD_P(Obj)        XEN_TRUE_P(scm_keyword_p(Obj))
  #endif
  #define XEN_MAKE_KEYWORD(Arg)       scm_c_make_keyword(Arg)
#endif
#define XEN_KEYWORD_EQ_P(k1, k2)      XEN_EQ_P(k1, k2)

/* there is SCM_CONTINUATIONP -- why doesn't scheme have continuation? */

#if HAVE_SCM_C_MAKE_RECTANGULAR
  #define XEN_STRING_P(Arg)           scm_is_string(Arg)
  #define XEN_TO_C_STRING(Str)        xen_guile_to_c_string_with_eventual_free(Str)
  #define C_TO_XEN_STRING(a)          ((a) ? scm_from_locale_string(a) : XEN_FALSE)
  #define C_TO_XEN_STRINGN(Str, Len)  scm_from_locale_stringn(Str, Len)
#else
  #define XEN_STRING_P(Arg)           (SCM_STRINGP(Arg))
  #ifndef SCM_STRING_CHARS
    #define XEN_TO_C_STRING(STR)      SCM_CHARS(STR)
  #else
    #define XEN_TO_C_STRING(STR)      SCM_STRING_CHARS(STR)
    /* this assumes its argument is a XEN string and does not allocate new space */
  #endif
  #define C_TO_XEN_STRING(a)          scm_makfrom0str((const char *)(a))
  #if HAVE_SCM_MEM2STRING
    #define C_TO_XEN_STRINGN(Str, Len)  scm_mem2string(Str, Len)
  #else
    #define C_TO_XEN_STRINGN(Str, Len)  scm_makfromstr(Str, Len, 0)
  #endif
#endif

#if HAVE_SCM_OBJECT_TO_STRING
  #define XEN_TO_STRING(Obj)          scm_object_to_string(Obj, XEN_UNDEFINED)
#else
  #define XEN_TO_STRING(Obj)          scm_strprint_obj(Obj)
#endif

/* ---- eval ---- */
#if HAVE_SCM_C_EVAL_STRING
  #define C_STRING_TO_XEN_FORM(Str)   scm_c_read_string(Str)
  #define XEN_EVAL_C_STRING(Arg)      scm_c_eval_string(Arg)
#else
  #define C_STRING_TO_XEN_FORM(Str)   scm_read_0str(Str)
  #define XEN_EVAL_C_STRING(Arg)      scm_eval_0str(Arg)
#endif

#if HAVE_SCM_C_MAKE_RECTANGULAR
  #define C_STRING_TO_XEN_SYMBOL(a)   scm_from_locale_symbol(a)
#else
  #if HAVE_SCM_STR2SYMBOL
    #define C_STRING_TO_XEN_SYMBOL(a) scm_str2symbol(a)
  #else
#define C_STRING_TO_XEN_SYMBOL(a)     gh_symbol2scm((char *)(a))
  #endif
#endif

#if HAVE_SCM_C_MAKE_RECTANGULAR
  #define XEN_SYMBOL_P(Arg)           scm_is_symbol(Arg)
  #define XEN_EVAL_FORM(Form)         scm_eval((XEN)(Form), scm_interaction_environment())
  #define XEN_SYMBOL_TO_C_STRING(a)   XEN_TO_C_STRING(scm_symbol_to_string(a))
#else
  #define XEN_SYMBOL_P(Arg)           (SCM_SYMBOLP(Arg))
  #ifdef SCM_SYMBOL_CHARS
    #define XEN_EVAL_FORM(Form)       scm_eval((XEN)(Form), scm_interaction_environment())
    /* was scm_eval_x but I'm not sure that's safe */
    #define XEN_SYMBOL_TO_C_STRING(a) SCM_SYMBOL_CHARS(a)
  #else
    #define XEN_EVAL_FORM(Form)       scm_eval((XEN)(Form))
    #define XEN_SYMBOL_TO_C_STRING(a) gh_symbol2newstr(a, NULL)
  #endif
#endif

/* ---- user-defined variables and constants ---- */
#if HAVE_SCM_C_DEFINE
  #define XEN_VARIABLE_REF(Var)               SCM_VARIABLE_REF(Var)
  #define XEN_VARIABLE_SET(Var, Val)          SCM_VARIABLE_SET(Var, Val)
  #define XEN_NAME_AS_C_STRING_TO_VALUE(a)    XEN_VARIABLE_REF(scm_sym2var(C_STRING_TO_XEN_SYMBOL(a), scm_current_module_lookup_closure (), XEN_TRUE))
      /* this is probably not the right thing -- the 3rd arg should be XEN_FALSE, else we're defining a new variable in the current module */
  #define XEN_NAME_AS_C_STRING_TO_VARIABLE(a) scm_sym2var(C_STRING_TO_XEN_SYMBOL(a), scm_current_module_lookup_closure(), XEN_FALSE)
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

#if HAVE_SCM_C_DEFINE
  #define XEN_DEFINE_VARIABLE(Name, Var, Value) Var = XEN_PROTECT_FROM_GC(scm_c_define(Name, Value))
#else
  #define XEN_DEFINE_VARIABLE(Name, Var, Value) Var = gh_define((char *)(Name), Value)
#endif

#if HAVE_SCM_C_DEFINE
  #define XEN_DEFINE(Name, Value) scm_c_define(Name, Value)
  #define XEN_DEFINE_CONSTANT(Name, Value, Help) \
    { \
      scm_c_define(Name, C_TO_XEN_INT(Value)); \
      if (Help) XEN_SET_DOCUMENTATION(Name, Help); \
    }
#else
  #define XEN_DEFINE(Name, Value) gh_define((char *)(Name), Value)
  #define XEN_DEFINE_CONSTANT(Name, Value, Help) \
    { \
      gh_define((char *)(Name), C_TO_XEN_INT(Value));	\
      if (Help) XEN_SET_DOCUMENTATION(Name, Help); \
    }
#endif

/* ---- user-defined types --- */
#if (SCM_DEBUG_TYPING_STRICTNESS == 2)
  #define XEN_MAKE_AND_RETURN_OBJECT(Tag, Val, Ignore1, Ignore2) SCM_RETURN_NEWSMOB(Tag, Val)
#else
  #define XEN_MAKE_AND_RETURN_OBJECT(Tag, Val, Ignore1, Ignore2) SCM_RETURN_NEWSMOB(Tag, (XEN)Val)
#endif
#define XEN_OBJECT_REF(a) SCM_SMOB_DATA(a)
/* remember to check the smob type agreement before calling XEN_OBJECT_REF! */

#if HAVE_SCM_REMEMBER_UPTO_HERE
  #if HAVE_SCM_T_CATCH_BODY
    #define XEN_OBJECT_TYPE            scm_t_bits
  #else
    #define XEN_OBJECT_TYPE            scm_bits_t
  #endif
  /* SCM_SMOB_PREDICATE -> scm_assert_smob_type? -- don't want an error here */
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

/* (need a way to pass an uninterpreted pointer from C to XEN then back to C) */
#if (SCM_DEBUG_TYPING_STRICTNESS == 2)
  #define XEN_WRAP_C_POINTER(a)       (C_TO_XEN_ULONG((unsigned long)a))
#else
  #define XEN_WRAP_C_POINTER(a)       ((XEN)(C_TO_XEN_ULONG((unsigned long)a)))
#endif

#define XEN_UNWRAP_C_POINTER(a)       XEN_TO_C_ULONG(a)
#define XEN_WRAPPED_C_POINTER_P(a)    XEN_NOT_FALSE_P(scm_number_p(a))

#define XEN_SET_DOCUMENTATION(Func, Help) scm_set_object_property_x(C_STRING_TO_XEN_SYMBOL(Func), XEN_DOCUMENTATION_SYMBOL, C_TO_XEN_STRING(Help))

/* ---- procedures ---- */
#ifdef __cplusplus
  #define XEN_PROCEDURE_CAST (XEN (*)())
#else
  #define XEN_PROCEDURE_CAST
#endif

/* max all args is SCM_GSUBR_MAX (gsubr.h) in all versions if (SCM_GSUBR_MAX < req + opt + rst) error
 *   apparently there is not limit in Ruby (passes an array)
 */

#if HAVE_SCM_C_DEFINE_GSUBR
  #define XEN_NEW_PROCEDURE(Name, Func, Req, Opt, Rst) scm_c_define_gsubr(Name, Req, Opt, Rst, XEN_PROCEDURE_CAST Func)
#else
  #define XEN_NEW_PROCEDURE(Name, Func, Req, Opt, Rst) gh_new_procedure(Name, XEN_PROCEDURE_CAST Func, Req, Opt, Rst)
#endif

#if XEN_DEBUGGING && HAVE_SCM_C_DEFINE_GSUBR

  #define XEN_NEW_PROCEDURE_WITH_CHECK(Name, Func, Req, Opt, Rst) xen_guile_dbg_new_procedure(Name, XEN_PROCEDURE_CAST Func, Req, Opt, Rst)
  #define XEN_DEFINE_PROCEDURE(Name, Func, ReqArg, OptArg, RstArg, Doc) \
    if ((char *)(Doc) != (char *)(NULL))					\
      scm_set_procedure_property_x(XEN_NEW_PROCEDURE_WITH_CHECK(Name, Func, ReqArg, OptArg, RstArg), XEN_DOCUMENTATION_SYMBOL, C_TO_XEN_STRING(Doc)); \
    else XEN_NEW_PROCEDURE_WITH_CHECK(Name, Func, ReqArg, OptArg, RstArg)

#else

  #define XEN_DEFINE_PROCEDURE(Name, Func, ReqArg, OptArg, RstArg, Doc) \
    if ((char *)(Doc) != (char *)(NULL))					\
      scm_set_procedure_property_x(XEN_NEW_PROCEDURE(Name, Func, ReqArg, OptArg, RstArg), XEN_DOCUMENTATION_SYMBOL, C_TO_XEN_STRING(Doc)); \
    else XEN_NEW_PROCEDURE(Name, Func, ReqArg, OptArg, RstArg)

#endif

/* Set_Name is ignored here, but is needed in Ruby */
#define XEN_DEFINE_PROCEDURE_WITH_SETTER(Get_Name, Get_Func, Get_Help, Set_Name, Set_Func, Get_Req, Get_Opt, Set_Req, Set_Opt) \
  xen_guile_define_procedure_with_setter(Get_Name, XEN_PROCEDURE_CAST Get_Func, Get_Help, \
                                         XEN_PROCEDURE_CAST Set_Func, XEN_DOCUMENTATION_SYMBOL, Get_Req, Get_Opt, Set_Req, Set_Opt)

#define XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(Get_Name, Get_Func, Get_Help, Set_Name, Set_Func, Rev_Func, Get_Req, Get_Opt, Set_Req, Set_Opt) \
  xen_guile_define_procedure_with_reversed_setter(Get_Name, XEN_PROCEDURE_CAST Get_Func, Get_Help, \
                                                  XEN_PROCEDURE_CAST Set_Func, XEN_PROCEDURE_CAST Rev_Func, \
                                                  XEN_DOCUMENTATION_SYMBOL, Get_Req, Get_Opt, Set_Req, Set_Opt)

#define XEN_PROCEDURE_P(Arg)             (XEN_NOT_FALSE_P(scm_procedure_p(Arg)))
#define XEN_DOCUMENTATION_SYMBOL         scm_string_to_symbol(C_TO_XEN_STRING("documentation"))
#define XEN_OBJECT_HELP(Name)            scm_object_property(Name, XEN_DOCUMENTATION_SYMBOL)
#define XEN_PROCEDURE_HELP(Name)         scm_procedure_property(Name, XEN_DOCUMENTATION_SYMBOL)
#define XEN_PROCEDURE_SOURCE_HELP(Name)  scm_procedure_documentation(Name)
#define XEN_PROCEDURE_SOURCE(Func)       scm_procedure_source(Func)
#define XEN_ARITY(Func)                  scm_i_procedure_arity(Func)
#define XEN_PROCEDURE_NAME(Func)         scm_procedure_name(Func)
#define XEN_REQUIRED_ARGS_OK(Func, Args) (XEN_TO_C_INT(XEN_CAR(XEN_ARITY(Func))) == Args)

#if (!WITH_HOBBIT)
#define XEN_REQUIRED_ARGS(Func)          XEN_TO_C_INT(XEN_CAR(XEN_ARITY(Func)))
#else
#define XEN_REQUIRED_ARGS(Func) \
  XEN_TO_C_INT(((!(SCM_CLOSUREP(Func))) && \
                (XEN_NOT_FALSE_P(scm_procedure_property(Func, C_STRING_TO_XEN_SYMBOL("hobbit-numargs"))))) ? \
		 scm_procedure_property(Func, C_STRING_TO_XEN_SYMBOL("hobbit-numargs")) : XEN_CAR(XEN_ARITY(Func)))
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
#define XEN_APPLY_NO_CATCH(Func, Args)              scm_apply(Func, Args, XEN_EMPTY_LIST)
#define XEN_CALL_0_NO_CATCH(Func)                   scm_apply(Func, XEN_EMPTY_LIST, XEN_EMPTY_LIST)
#define XEN_CALL_1_NO_CATCH(Func, Arg1)             scm_apply(Func, Arg1, XEN_APPLY_ARG_LIST_END)
#define XEN_CALL_2_NO_CATCH(Func, Arg1, Arg2)       scm_apply(Func, Arg1, scm_cons(Arg2, XEN_APPLY_ARG_LIST_END))
#define XEN_CALL_3_NO_CATCH(Func, Arg1, Arg2, Arg3) scm_apply(Func, Arg1, scm_cons2(Arg2, Arg3, XEN_APPLY_ARG_LIST_END))

#if defined(__MINGW32__) || defined(__CYGWIN__)
  #define XEN_APPLY_ARG_LIST_END      scm_cons(SCM_EOL, SCM_EOL)
#else
  #define XEN_APPLY_ARG_LIST_END      scm_listofnull
#endif

/* ---- errors ---- */
#define XEN_ERROR_TYPE(Typ)           C_STRING_TO_XEN_SYMBOL(Typ)
#if USE_SND
  #define XEN_ERROR(Type, Info)       snd_throw(Type, Info)
#else
  #define XEN_ERROR(Type, Info)       scm_throw(Type, Info)
#endif
#define XEN_THROW(Tag, Arg)           scm_throw(Tag, Arg)

/* disabling type checks saves almost no space (200k out of 12M) and no time (5% or so) */
#if HAVE_SCM_FROM_LOCALE_KEYWORD
  #define XEN_ASSERT_TYPE(Assertion, Arg, Position, Caller, Correct_Type) \
    SCM_ASSERT_TYPE(Assertion, Arg, Position, Caller, Correct_Type)

  #define XEN_WRONG_TYPE_ARG_ERROR(Caller, ArgN, Arg, Descr) \
    scm_wrong_type_arg_msg(Caller, ArgN, Arg, Descr)
#else
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
#endif

#define XEN_OUT_OF_RANGE_ERROR(Caller, ArgN, Arg, Descr) \
  XEN_ERROR(XEN_ERROR_TYPE("out-of-range"), \
            XEN_LIST_3(C_TO_XEN_STRING(Caller), \
                       C_TO_XEN_STRING(Descr), \
                       XEN_LIST_1(Arg)))

#if HAVE_SCM_T_CATCH_BODY
  #define XEN_CATCH_BODY_TYPE scm_t_catch_body
#else
  #define XEN_CATCH_BODY_TYPE scm_catch_body_t
#endif

#define XEN_YES_WE_HAVE(Feature)      scm_add_feature(Feature)
#define XEN_PROTECT_FROM_GC(Obj)      scm_permanent_object(Obj)
#if HAVE_SCM_C_PRIMITIVE_LOAD
  #define XEN_LOAD_FILE(File)           scm_c_primitive_load(File)
  #define XEN_LOAD_FILE_WITH_PATH(File) scm_c_primitive_load_path(File)
#else
  #define XEN_LOAD_FILE(File)           scm_primitive_load(C_TO_XEN_STRING(File))
  #define XEN_LOAD_FILE_WITH_PATH(File) scm_primitive_load_path(C_TO_XEN_STRING(File))
#endif
#define XEN_LOAD_PATH                 XEN_EVAL_C_STRING("%load-path")
#define XEN_ADD_TO_LOAD_PATH(Path)    xen_guile_add_to_load_path(Path)

#define XEN_PUTS(Str, Port)           scm_puts(Str, Port)
#define XEN_DISPLAY(Val, Port)        scm_display(Val, Port)
#define XEN_FLUSH_PORT(Port)          scm_force_output(Port)
#define XEN_CLOSE_PORT(Port)          scm_close_port(Port)
#define XEN_PORT_TO_STRING(Port)      scm_strport_to_string(Port)

XEN xen_guile_create_hook(const char *name, int args, const char *help, XEN local_doc);
void xen_guile_define_procedure_with_setter(char *get_name, XEN (*get_func)(), char *get_help, XEN (*set_func)(), 
					    XEN local_doc, int get_req, int get_opt, int set_req, int set_opt);

void xen_guile_define_procedure_with_reversed_setter(char *get_name, XEN (*get_func)(), char *get_help, XEN (*set_func)(), XEN (*reversed_set_func)(), 
						     XEN local_doc, int get_req, int get_opt, int set_req, int set_opt);
double xen_to_c_double(XEN a);
double xen_to_c_double_or_else(XEN a, double b);
int xen_to_c_int(XEN a);
bool xen_integer_p(XEN a);
XEN xen_guile_add_to_load_path(char *path);
#if XEN_DEBUGGING && HAVE_SCM_C_DEFINE_GSUBR
XEN xen_guile_dbg_new_procedure(const char *name, XEN (*func)(), int req, int opt, int rst);
#endif


#if HAVE_SCM_C_MAKE_RECTANGULAR
char *xen_guile_to_c_string_with_eventual_free(XEN str);
#endif

/* these are only needed in 1.3.4, but it's hard to find a good way to distinguish that particular version */
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

#endif
/* end HAVE_GUILE */



/* ------------------------------ RUBY ------------------------------ */

/* other possibilities: 
 *    XEN_ASSOC (args reversed from Scheme), XEN_DEFINE_METHOD, XEN_DEFINE_ALIAS, rb_ary_unsift = XEN_LIST_PREPEND?,
 *    various property macros -- in Scheme as well, rb_const_defined, rb_yield, XEN_INCLUDE_MODULE,
 *    rb_id2name (XEN_SYMBOL...), rb_raise.
 */

#if HAVE_RUBY

#include <ruby.h>
#define XEN_OK 1

#define XEN                             VALUE
#define XEN_FILE_EXTENSION              "rb"
#define XEN_COMMENT_STRING              "#"
#define XEN_LANGUAGE_NAME               "Ruby"

#define XEN_FALSE                       Qfalse
#define XEN_TRUE                        Qtrue
#define XEN_TRUE_P(a)                   ((a) == Qtrue)
#define XEN_FALSE_P(a)                  ((a) == Qfalse)
#define C_TO_XEN_BOOLEAN(a)             ((a) ? Qtrue : Qfalse)
#define XEN_TO_C_BOOLEAN(a)             (!(XEN_FALSE_P(a)))

/* #define XEN_UNDEFINED                   Qundef */
#define XEN_UNDEFINED                   ID2SYM(rb_intern("undefined"))

#define XEN_BOUND_P(Arg)                ((Arg) != XEN_UNDEFINED)
#define XEN_NOT_BOUND_P(Arg)            ((Arg) == XEN_UNDEFINED)

#if defined(__GNUC__) && (!(defined(__cplusplus)))
  #define XEN_BOOLEAN_P(Arg)            ({ XEN _xen_h_7_ = Arg;        (XEN_TRUE_P(_xen_h_7_) || XEN_FALSE_P(_xen_h_7_)); })
  #define XEN_NUMBER_P(Arg)             ({ int _xen_h_8_ = TYPE(Arg);  ((_xen_h_8_ == T_FLOAT) || (_xen_h_8_ == T_FIXNUM) || (_xen_h_8_ == T_BIGNUM)); })
  #define XEN_INTEGER_P(Arg)            ({ int _xen_h_9_ = TYPE(Arg);  ((_xen_h_9_ == T_FIXNUM) || (_xen_h_9_ == T_BIGNUM)); })
  #define XEN_PROCEDURE_P(Arg)          ({ XEN _xen_h_10_ = Arg;       (XEN_BOUND_P(_xen_h_10_) && (rb_obj_is_kind_of(_xen_h_10_, rb_cProc))); })
  #define XEN_OFF_T_P(Arg)              ({ int _xen_h_11_ = TYPE(Arg); ((_xen_h_11_ == T_FIXNUM) || (_xen_h_11_ == T_BIGNUM)); })
  #define XEN_KEYWORD_P(Obj)            ({ XEN _xen_h_12_ = Obj;       (XEN_BOUND_P(_xen_h_12_) && SYMBOL_P(_xen_h_12_)); })
#else
  #define XEN_BOOLEAN_P(Arg)            (XEN_TRUE_P(Arg) || XEN_FALSE_P(Arg))
  #define XEN_NUMBER_P(Arg)             ((TYPE(Arg) == T_FLOAT) || (TYPE(Arg) == T_FIXNUM) || (TYPE(Arg) == T_BIGNUM))
  #define XEN_INTEGER_P(Arg)            ((TYPE(Arg) == T_FIXNUM) || (TYPE(Arg) == T_BIGNUM))
  #define XEN_PROCEDURE_P(Arg)          (XEN_BOUND_P(Arg) && (rb_obj_is_kind_of(Arg, rb_cProc)))
  #define XEN_OFF_T_P(Arg)              ((TYPE(Arg) == T_FIXNUM) || (TYPE(Arg) == T_BIGNUM))
  #define XEN_KEYWORD_P(Obj)            (XEN_BOUND_P(Obj) && SYMBOL_P(Obj))
#endif

/* ---- lists ---- */
#define XEN_EMPTY_LIST                  Qnil
#define XEN_NULL_P(a)                   (XEN_LIST_LENGTH(a) == 0)

#define XEN_CONS_P(Arg)                 (TYPE(Arg) == T_ARRAY)
#define XEN_PAIR_P(Arg)                 (TYPE(Arg) == T_ARRAY)
#define XEN_CONS(Arg1, Arg2)            xen_rb_cons(Arg1, Arg2)
#define XEN_CONS_2(Arg1, Arg2, Arg3)    xen_rb_cons2(Arg1, Arg2, Arg3)
#define XEN_CAR(a)                      xen_rb_list_ref(a, 0)
#define XEN_CADR(a)                     xen_rb_list_ref(a, 1)
#define XEN_CADDR(a)                    xen_rb_list_ref(a, 2)
#define XEN_CADDDR(a)                   xen_rb_list_ref(a, 3)
#define XEN_CDR(a)                      xen_rb_cdr(a)
#define XEN_CDDR(a)                     XEN_CDR(XEN_CDR(a))

#define XEN_LIST_P(Arg)                 ((Arg) == XEN_EMPTY_LIST || XEN_CONS_P(Arg))
#define XEN_LIST_P_WITH_LENGTH(Arg, Len) ((Len = XEN_LIST_LENGTH(Arg)) >= 0)
#define XEN_LIST_LENGTH(Arg)            xen_rb_list_length(Arg)
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
#if HAVE_RB_ARY_DUP
  #define XEN_COPY_ARG(Lst)             rb_ary_dup(Lst)
#else
  #define XEN_COPY_ARG(Lst)             xen_rb_copy_list(Lst)
#endif
#define XEN_LIST_REF(Lst, Num)          xen_rb_list_ref(Lst, Num)
#define XEN_LIST_SET(Lst, Num, Val)     xen_rb_list_set(Lst, Num, Val)
#define XEN_APPEND(X, Y)                rb_ary_concat(X, Y)
#define XEN_LIST_REVERSE(Lst)           rb_ary_reverse(XEN_COPY_ARG(Lst))

/* ---- numbers ---- */
/* apparently no complex numbers (built-in) in Ruby? */

#define XEN_ZERO                        INT2NUM(0)
#define XEN_DOUBLE_P(Arg)               XEN_NUMBER_P(Arg)
#define XEN_TO_C_DOUBLE(a)              NUM2DBL(a)
#if defined(__GNUC__) && (!(defined(__cplusplus)))
  #define XEN_TO_C_DOUBLE_OR_ELSE(a, b) ({ XEN _xen_h_4_ = a; (XEN_NUMBER_P(_xen_h_4_) ? NUM2DBL(_xen_h_4_) : b); })
#else
  #define XEN_TO_C_DOUBLE_OR_ELSE(a, b) xen_rb_to_c_double_or_else(a, b)
#endif
#define C_TO_XEN_DOUBLE(a)              rb_float_new(a)
#define XEN_TO_C_INT(a)                 rb_num2long(a)
#define XEN_TO_C_INT_OR_ELSE(a, b)      xen_rb_to_c_int_or_else(a, b)

#define XEN_ULONG_P(Arg1)               XEN_INTEGER_P(Arg1)
#define XEN_EXACT_P(Arg1)               XEN_INTEGER_P(Arg1)
#define C_TO_XEN_INT(a)                 INT2NUM(a)
#define XEN_TO_C_ULONG(a)               NUM2ULONG(a)
#ifdef ULONG2NUM
  #define C_TO_XEN_ULONG(a)             ULONG2NUM((unsigned long)a)
#else
  #define C_TO_XEN_ULONG(a)             UINT2NUM((unsigned long)a)
#endif

/* off_t support added in 1.8.0, I think */
#ifndef OFFT2NUM
  #define OFFT2NUM(a)                   INT2NUM(a)
#endif
#ifndef NUM2OFFT
  #define NUM2OFFT(a)                   NUM2LONG(a)
#endif
#define C_TO_XEN_LONG_LONG(a)           OFFT2NUM(a)
#define XEN_TO_C_LONG_LONG(a)           NUM2OFFT(a)

/* ---- strings ---- */
#define XEN_STRING_P(Arg)               ((TYPE(Arg) == T_STRING) && (!SYMBOL_P(Arg)))
#define C_TO_XEN_STRING(a)              xen_rb_str_new2((char *)a)
#define C_TO_XEN_STRINGN(a, len)        rb_str_new((char *)a, len)
#ifndef RSTRING_PTR 
  #define XEN_TO_C_STRING(Str)          RSTRING(Str)->ptr 
#else 
  #define XEN_TO_C_STRING(Str)          RSTRING_PTR(Str) 
#endif 

#define XEN_CHAR_P(Arg)                 XEN_STRING_P(Arg)
#define XEN_TO_C_CHAR(Arg)              XEN_TO_C_STRING(Arg)[0] 
#define C_TO_XEN_CHAR(Arg)              rb_str_new((char *)(&(Arg)), 1)

#define XEN_NAME_AS_C_STRING_TO_VALUE(a) rb_gv_get(xen_scheme_global_variable_to_ruby(a))
#define C_STRING_TO_XEN_FORM(Str)       XEN_EVAL_C_STRING(Str)
#define XEN_EVAL_FORM(Form)             ((XEN)Form)
#define XEN_EVAL_C_STRING(Arg)          xen_rb_eval_string_with_error(Arg)
#define XEN_TO_STRING(Obj)              xen_rb_obj_as_string(Obj)
#define XEN_LOAD_FILE(a)                xen_rb_load_file_with_error(C_TO_XEN_STRING(a))
#define XEN_LOAD_FILE_WITH_PATH(a)      xen_rb_load_file_with_error(C_TO_XEN_STRING(a))
#define XEN_LOAD_PATH                   XEN_NAME_AS_C_STRING_TO_VALUE("$LOAD_PATH")
#define XEN_ADD_TO_LOAD_PATH(Path)      xen_rb_add_to_load_path(Path)

/* ---- hooks ---- */
#define XEN_HOOK_P(Arg)                 xen_rb_hook_p(Arg)
#define XEN_HOOK_PROCEDURES(a)          xen_rb_hook_to_a(a)
#define XEN_CLEAR_HOOK(a)               xen_rb_hook_reset_hook(a)
#define XEN_HOOKED(a)                   (!xen_rb_hook_empty_p(a))
#define XEN_DEFINE_HOOK(Name, Arity, Help) xen_rb_create_hook(Name, Arity, Help)
#define XEN_DEFINE_SIMPLE_HOOK(Arity)   xen_rb_hook_c_new("simple_hook", Arity, NULL);

/* ---- vectors ---- */
#define XEN_VECTOR_P(Arg)               (TYPE(Arg) == T_ARRAY)
#define XEN_VECTOR_LENGTH(Arg)          xen_rb_list_length(Arg)
#define XEN_VECTOR_REF(Vect, Num)       xen_rb_list_ref(Vect, Num)
#define XEN_VECTOR_SET(Vect, Num, Val)  xen_rb_list_set(Vect, Num, Val)
#define XEN_MAKE_VECTOR(Num, Fill)      xen_rb_ary_new_with_initial_element(Num, Fill)
#define XEN_VECTOR_TO_LIST(a)           a

/* ---- symbols ---- */
#define XEN_SYMBOL_P(Arg)               SYMBOL_P(Arg)
#define XEN_SYMBOL_TO_C_STRING(a)       ((char *)rb_id2name(SYM2ID(a)))
#define C_STRING_TO_XEN_SYMBOL(a)       ID2SYM(rb_intern(a))
#define XEN_STRING_TO_SYMBOL(Str)       C_STRING_TO_XEN_SYMBOL(XEN_TO_C_STRING(Str))
#define XEN_SYMBOL_TO_STRING(Sym)       C_TO_XEN_STRING(XEN_SYMBOL_TO_C_STRING(Sym))
#define XEN_DOCUMENTATION_SYMBOL        C_STRING_TO_XEN_SYMBOL("documentation")
#define XEN_OBJECT_HELP(Name)           rb_documentation(Name)
#define XEN_SET_OBJECT_HELP(Name, Help) rb_set_documentation(Name, Help)
#define C_SET_OBJECT_HELP(name, help)   XEN_SET_OBJECT_HELP(C_TO_XEN_STRING(name), C_TO_XEN_STRING(help))

#define XEN_VARIABLE_SET(a, b)          rb_gv_set(xen_scheme_global_variable_to_ruby(a), b)
#define XEN_VARIABLE_REF(a)             rb_gv_get(xen_scheme_global_variable_to_ruby(a))
#define XEN_DEFINE_CONSTANT(Name, Value, Help) \
  do { \
      rb_define_global_const(xen_scheme_constant_to_ruby(Name), C_TO_XEN_INT(Value)); \
      if ((Name) && (Help)) C_SET_OBJECT_HELP(xen_scheme_constant_to_ruby(Name), Help); \
    } while (0)

#define XEN_DEFINE_VARIABLE(Name, Var, Value) \
  { \
    Var = Value; \
    rb_define_variable(xen_scheme_global_variable_to_ruby(Name), (VALUE *)(&Var)); \
  }
#define XEN_DEFINE(Name, Value) rb_define_global_const(xen_scheme_constant_to_ruby(Name), Value)
#define XEN_DEFINED_P(Name)             xen_rb_defined_p(Name)

#define XEN_WRAP_C_POINTER(a)           Data_Wrap_Struct(rb_cData, 0, 0, (void *)a)
#define XEN_UNWRAP_C_POINTER(a)         DATA_PTR(a)
#define XEN_WRAPPED_C_POINTER_P(a)      (TYPE(a) == T_DATA)

/* ---- C structs ---- */
#define XEN_MARK_OBJECT_TYPE            void *
#define XEN_MAKE_AND_RETURN_OBJECT(Tag, Val, Mark, Free) return(Data_Wrap_Struct(Tag, Mark, Free, Val))
#define XEN_OBJECT_REF(a)               DATA_PTR(a)
#define XEN_OBJECT_TYPE                 VALUE
#define XEN_OBJECT_TYPE_P(OBJ, TAG)     (XEN_BOUND_P(OBJ) && (rb_obj_is_instance_of(OBJ, TAG)))
#define XEN_MAKE_OBJECT_TYPE(Typ, Siz)  rb_define_class(xen_scheme_constant_to_ruby(Typ), rb_cObject)

#define XEN_MAKE_OBJECT_FREE_PROCEDURE(Type, Wrapped_Free, Original_Free) \
  static void *Wrapped_Free(XEN obj) \
  { \
    Original_Free((Type *)obj); \
    return(NULL); \
  }

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

/* ---- procedures ---- */
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

#define XEN_PROCEDURE_SOURCE(Func)       Func
#define XEN_ARITY(Func)                  rb_funcall(Func, rb_intern("arity"), 0)
#define XEN_REQUIRED_ARGS(Func)          xen_rb_required_args(XEN_ARITY(Func))
#define XEN_REQUIRED_ARGS_OK(Func, Args) (xen_rb_required_args(XEN_ARITY(Func)) == Args)

#define XEN_DEFINE_PROCEDURE(Name, Func, ReqArg, OptArg, RstArg, Doc) \
  do { \
      rb_define_global_function(xen_scheme_procedure_to_ruby(Name), XEN_PROCEDURE_CAST Func, ((RstArg > 0) ? -2 : (OptArg > 0) ? -1 : ReqArg)); \
      if ((Name) && (Doc)) C_SET_OBJECT_HELP(xen_scheme_procedure_to_ruby(Name), Doc); \
    } while (0)

#define XEN_DEFINE_PROCEDURE_WITH_SETTER(Get_Name, Get_Func, Get_Help, Set_Name, Set_Func, Get_Req, Get_Opt, Set_Req, Set_Opt) \
  do { \
      XEN_DEFINE_PROCEDURE(Get_Name, XEN_PROCEDURE_CAST Get_Func, Get_Req, Get_Opt, 0, Get_Help); \
      XEN_DEFINE_PROCEDURE(Set_Name, XEN_PROCEDURE_CAST Set_Func, Set_Req, Set_Opt, 0, Get_Help); \
   } while (0)

#define XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(Get_Name, Get_Func, Get_Help, Set_Name, Set_Func, Rev_Func, Get_Req, Get_Opt, Set_Req, Set_Opt) \
  do { \
      XEN_DEFINE_PROCEDURE(Get_Name, XEN_PROCEDURE_CAST Get_Func, Get_Req, Get_Opt, 0, Get_Help); \
      XEN_DEFINE_PROCEDURE(Set_Name, XEN_PROCEDURE_CAST Set_Func, Set_Req, Set_Opt, 0, Get_Help); \
    } while (0)

#define XEN_CALL_0(Func, Caller)                   xen_rb_funcall_0(Func)
#define XEN_CALL_1(Func, Arg1, Caller)             rb_funcall(Func, rb_intern("call"), 1, Arg1)
#define XEN_CALL_2(Func, Arg1, Arg2, Caller)       rb_funcall(Func, rb_intern("call"), 2, Arg1, Arg2)
#define XEN_CALL_3(Func, Arg1, Arg2, Arg3, Caller) rb_funcall(Func, rb_intern("call"), 3, Arg1, Arg2, Arg3)
#define XEN_CALL_4(Func, Arg1, Arg2, Arg3, Arg4, Caller) rb_funcall(Func, rb_intern("call"), 4, Arg1, Arg2, Arg3, Arg4)
#define XEN_CALL_5(Func, Arg1, Arg2, Arg3, Arg4, Arg5, Caller) rb_funcall(Func, rb_intern("call"), 5, Arg1, Arg2, Arg3, Arg4, Arg5)
#define XEN_CALL_6(Func, Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, Caller) rb_funcall(Func, rb_intern("call"), 6, Arg1, Arg2, Arg3, Arg4, Arg5, Arg6)
#define XEN_APPLY(Func, Args, Caller)              xen_rb_apply(Func, Args)
#define XEN_APPLY_ARG_LIST_END          Qnil
#define XEN_CALL_0_NO_CATCH(Func)                   xen_rb_funcall_0(Func)
#define XEN_CALL_1_NO_CATCH(Func, Arg1)             rb_funcall(Func, rb_intern("call"), 1, Arg1)
#define XEN_CALL_2_NO_CATCH(Func, Arg1, Arg2)       rb_funcall(Func, rb_intern("call"), 2, Arg1, Arg2)
#define XEN_CALL_3_NO_CATCH(Func, Arg1, Arg2, Arg3) rb_funcall(Func, rb_intern("call"), 3, Arg1, Arg2, Arg3)
#define XEN_APPLY_NO_CATCH(Func, Args)              xen_rb_apply(Func, Args)

/* ---- keywords, etc ---- */
#define XEN_KEYWORD_EQ_P(k1, k2)        ((k1) == (k2))
#define XEN_MAKE_KEYWORD(Arg)           C_STRING_TO_XEN_SYMBOL(xen_scheme_procedure_to_ruby(Arg))
#define XEN_YES_WE_HAVE(a)              rb_provide(a)
#define XEN_PROTECT_FROM_GC(Var)        rb_gc_register_address(&(Var))
#define XEN_UNPROTECT_FROM_GC(Var)      rb_gc_unregister_address(&(Var))

/* ---- errors ---- */
#define XEN_ERROR_TYPE(Name)            rb_intern(xen_scheme_constant_to_ruby(Name))

#define XEN_ASSERT_TYPE(Assertion, Arg, Position, Caller, Correct_Type) \
  do { \
    if (!(Assertion)) \
      rb_raise(rb_eTypeError, "%s: wrong type arg %d, %s, wanted %s\n", \
               Caller, Position, XEN_TO_C_STRING(XEN_TO_STRING(Arg)), Correct_Type); \
     } while (0)

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

XEN xen_rb_cdr(XEN val);
XEN xen_rb_cons(XEN arg1, XEN arg2);
XEN xen_rb_cons2(XEN arg1, XEN arg2, XEN arg3);
char *xen_scheme_constant_to_ruby(const char *name);
char *xen_scheme_procedure_to_ruby(const char *name);
char *xen_scheme_global_variable_to_ruby(const char *name);
bool xen_rb_defined_p(const char *name);
int xen_rb_list_length(XEN obj); 
XEN xen_rb_list_ref(XEN obj, int index);
XEN xen_rb_list_set(XEN obj, int index, XEN value);
void xen_rb_raise(XEN type, XEN info);
XEN xen_rb_obj_as_string(XEN obj);
XEN xen_rb_eval_string_with_error(char *str);
XEN xen_rb_load_file_with_error(XEN file);
XEN xen_rb_ary_new_with_initial_element(long num, XEN element);
XEN xen_rb_apply(XEN func, XEN args);
XEN xen_rb_funcall_0(XEN func);
int xen_rb_required_args(XEN val);
#if (!HAVE_RB_ARY_DUP)
XEN xen_rb_copy_list(XEN val);
#endif
XEN xen_rb_str_new2(char *arg);
void xen_add_help(char *name, const char *help);
char *xen_help(char *name);
double xen_rb_to_c_double_or_else(XEN a, double b);
int xen_rb_to_c_int_or_else(XEN a, int b);
/* class Hook */
bool xen_rb_hook_p(XEN hook);
bool xen_rb_hook_empty_p(XEN hook);
XEN xen_rb_hook_c_new(char *name, int arity, char *help);
XEN xen_rb_hook_reset_hook(XEN hook);
XEN xen_rb_hook_to_a(XEN hook);
void Init_Hook(void);
XEN xen_rb_create_hook(char *name, int arity, char *help);

typedef XEN (*XEN_CATCH_BODY_TYPE) (void *data);

XEN rb_properties(void);
XEN rb_property(XEN obj, XEN prop);
XEN rb_set_property(XEN obj, XEN prop, XEN val);
XEN rb_documentation(XEN name);
XEN rb_set_documentation(XEN name, XEN help);
bool xen_rb_arity_ok(int rargs, int args);
void xen_rb_repl_set_prompt(const char *prompt);
XEN xen_rb_add_to_load_path(char *path);
#endif
/* end HAVE_RUBY */



/* ------------------------------ FORTH ------------------------------ */

#if HAVE_FORTH

#define LOCAL_SIZEOF_OFF_T SIZEOF_OFF_T
#undef SIZEOF_OFF_T
#include <fth.h>
#undef SIZEOF_OFF_T
#define SIZEOF_OFF_T LOCAL_SIZEOF_OFF_T

#if USE_SND
# undef gettext_noop
# undef _
# undef N_
#endif

#define XEN_OK                          true

#define XEN                             FTH
#define XEN_FILE_EXTENSION              FTH_FILE_EXTENSION
#define XEN_COMMENT_STRING              "\\"
#define XEN_LANGUAGE_NAME               "Forth"

#define XEN_FALSE                       FTH_FALSE
#define XEN_TRUE                        FTH_TRUE
#define XEN_EMPTY_LIST                  FTH_NIL
#define XEN_UNDEFINED                   FTH_UNDEF
#define XEN_DOCUMENTATION_SYMBOL        FTH_DOCUMENTATION_SYMBOL

#define XEN_DEFINED_P(name)             fth_defined_p((char *)name)
#define XEN_YES_WE_HAVE(feature)        fth_add_feature(feature)

/* === Boolean, Bound, Equal === */
#define XEN_BOOLEAN_P(Arg)              FTH_BOOLEAN_P(Arg)
#define XEN_TRUE_P(a)                   FTH_TRUE_P(a)
#define XEN_FALSE_P(a)                  FTH_FALSE_P(a)
#define C_TO_XEN_BOOLEAN(a)             BOOL_TO_FTH(a)
#define XEN_TO_C_BOOLEAN(a)             FTH_TO_BOOL(a)

#define XEN_BOUND_P(Arg)                FTH_BOUND_P(Arg)
#define XEN_NOT_BOUND_P(Arg)            FTH_NOT_BOUND_P(Arg)

#define XEN_EQ_P(a, b)                  ((a) == (b))
#define XEN_EQV_P(a, b)                 XEN_EQ_P(a, b)
#define XEN_EQUAL_P(a, b)               fth_object_equal_p(a, b)

/* === Number === */
#define XEN_ZERO                        FTH_ZERO
#define XEN_NUMBER_P(Arg)               FTH_NUMBER_P(Arg)
#define XEN_EXACT_P(Arg)                FTH_EXACT_P(Arg)

#define XEN_INTEGER_P(Arg)              FTH_INTEGER_P(Arg)
#define C_TO_XEN_INT(a)                 fth_make_int(a)
#define XEN_TO_C_INT(a)                 fth_int_ref(a)
#define XEN_TO_C_INT_OR_ELSE(a, b)      fth_int_ref_or_else(a, b)

#define XEN_ULONG_P(Arg)                FTH_UNSIGNED_P(Arg)
#define C_TO_XEN_ULONG(a)               fth_make_unsigned((unsigned long)(a))
#define XEN_TO_C_ULONG(a)               fth_unsigned_ref(a)

#define XEN_OFF_T_P(Arg)                FTH_LONG_LONG_P(Arg)
#define C_TO_XEN_LONG_LONG(a)           fth_make_long_long(a)
#define XEN_TO_C_LONG_LONG(a)           fth_long_long_ref(a)

#define XEN_DOUBLE_P(Arg)               FTH_FLOAT_P(Arg)
#define C_TO_XEN_DOUBLE(a)              fth_make_float(a)
#define XEN_TO_C_DOUBLE(a)              fth_float_ref(a)
#define XEN_TO_C_DOUBLE_OR_ELSE(a, b)   fth_float_ref_or_else(a, b)

#if HAVE_SCM_MAKE_COMPLEX || HAVE_SCM_C_MAKE_RECTANGULAR
# define XEN_COMPLEX_P(Arg)             FTH_NUMBER_P(Arg) 
# define C_TO_XEN_COMPLEX(a)            fth_make_complex(a)
# define XEN_TO_C_COMPLEX(a)            fth_complex_ref(a)
#else
# define XEN_COMPLEX_P(Arg)             false
# define C_TO_XEN_COMPLEX(a)            XEN_ZERO
# define XEN_TO_C_COMPLEX(a)            0.0
#endif

#if HAVE_SCM_MAKE_RATIO
# define XEN_HAVE_RATIOS                    true
# define XEN_RATIO_P(Arg)               FTH_RATIO_P(Arg)
# define XEN_MAKE_RATIO(Num, Den)       fth_make_ratio(Num, Den)
# define XEN_NUMERATOR(Arg)             fth_numerator(Arg)
# define XEN_DENOMINATOR(Arg)           fth_denominator(Arg)
# define XEN_RATIONALIZE(Arg1, Arg2)    fth_rationalize(Arg1, Arg2)
#endif

/* === String, Symbol, Keyword, Eval === */
#define XEN_CHAR_P(Arg)                 FTH_CHAR_P(Arg)
#define C_TO_XEN_CHAR(Arg)              CHAR_TO_FTH(Arg)
#define XEN_TO_C_CHAR(Arg)              FTH_TO_CHAR(Arg)

#define XEN_STRING_P(Arg)               FTH_STRING_P(Arg)
#define C_TO_XEN_STRING(str)            fth_make_string(str)
#define C_TO_XEN_STRINGN(str, len)      fth_make_string_len(str, len)
#define XEN_TO_C_STRING(Str)            fth_string_ref(Str)

#define XEN_TO_STRING(Obj)              fth_object_to_string(Obj)

#define XEN_SYMBOL_P(Arg)               FTH_SYMBOL_P(Arg)
#define C_STRING_TO_XEN_SYMBOL(a)       fth_symbol(a)
#define XEN_SYMBOL_TO_C_STRING(Sym)     fth_symbol_ref(Sym)

#define XEN_KEYWORD_P(Obj)              FTH_KEYWORD_P(Obj)
#define XEN_MAKE_KEYWORD(arg)           fth_keyword(arg)
#define XEN_KEYWORD_EQ_P(K1, K2)        XEN_EQ_P(K1, K2)

#define XEN_EVAL_C_STRING(arg)          fth_eval(arg) 
#define XEN_EVAL_FORM(Form)             XEN_EVAL_C_STRING(XEN_TO_C_STRING(Form))
#define C_STRING_TO_XEN_FORM(str)       XEN_EVAL_C_STRING(str)
#define XEN_LOAD_FILE(a)                fth_load_file(a)
#define XEN_LOAD_FILE_WITH_PATH(a)      fth_load_file(a)
#define XEN_LOAD_PATH                   XEN_NAME_AS_C_STRING_TO_VALUE("*load-path*")
#define XEN_ADD_TO_LOAD_PATH(Path)      fth_add_load_path(Path)

/* === Vector (Array) === */
#define XEN_MAKE_VECTOR(Num, Fill)      fth_make_array_with_init(Num, Fill)
#define XEN_VECTOR_P(Arg)               FTH_ARRAY_P(Arg)
#define XEN_VECTOR_LENGTH(Arg)          ((int)fth_array_length(Arg))
#define XEN_VECTOR_TO_LIST(Vect)        fth_array_to_list(Vect)
#define XEN_VECTOR_REF(Vect, Num)       fth_array_ref(Vect, Num)
#define XEN_VECTOR_SET(Vect, Num, Val)  fth_array_set(Vect, Num, Val)

/* === List === */
#define XEN_NULL_P(a)                   FTH_NIL_P(a)
#define XEN_LIST_P(Arg)                 FTH_LIST_P(Arg)
#define XEN_CONS_P(Arg)                 FTH_CONS_P(Arg)
#define XEN_PAIR_P(Arg)                 FTH_PAIR_P(Arg)
#define XEN_CONS(Arg1, Arg2)            fth_cons(Arg1, Arg2)
#define XEN_CONS_2(Arg1, Arg2, Arg3)    fth_cons_2(Arg1, Arg2, Arg3)
#define XEN_LIST_REF(Lst, Num)          fth_list_ref(Lst, Num)
#define XEN_LIST_SET(Lst, Num, Val)     fth_list_set(Lst, Num, Val)
#define XEN_LIST_REVERSE(Lst)           fth_list_reverse(Lst)
#define XEN_LIST_P_WITH_LENGTH(Arg, Len) ((Len = XEN_LIST_LENGTH(Arg)) >= 0)
#define XEN_LIST_LENGTH(Arg)            ((int)fth_list_length(Arg))
#define XEN_LIST_1(a)                   FTH_LIST_1(a)
#define XEN_LIST_2(a, b)                FTH_LIST_2(a, b)
#define XEN_LIST_3(a, b, c)             FTH_LIST_3(a, b, c)
#define XEN_LIST_4(a, b, c, d)          FTH_LIST_4(a, b, c, d)
#define XEN_LIST_5(a, b, c, d, e)       FTH_LIST_5(a, b, c, d, e)
#define XEN_LIST_6(a, b, c, d, e, f)    FTH_LIST_6(a, b, c, d, e, f)
#define XEN_LIST_7(a, b, c, d, e, f, g) FTH_LIST_7(a, b, c, d, e, f, g)
#define XEN_LIST_8(a, b, c, d, e, f, g, h)    FTH_LIST_8(a, b, c, d, e, f, g, h)
#define XEN_LIST_9(a, b, c, d, e, f, g, h, i) FTH_LIST_9(a, b, c, d, e, f, g, h, i)
#define XEN_CAR(a)                      fth_car(a)
#define XEN_CADR(a)                     FTH_CADR(a)
#define XEN_CADDR(a)                    FTH_CADDR(a)
#define XEN_CADDDR(a)                   FTH_CADDDR(a)
#define XEN_CDR(a)                      fth_cdr(a)
#define XEN_CDDR(a)                     FTH_CDDR(a)
#define XEN_COPY_ARG(Lst)               fth_list_copy(Lst)
#define XEN_APPEND(a, b)                fth_list_append(XEN_LIST_2(a, b))
#define XEN_ASSOC(Item, Lst)            fth_list_assoc_ref(Lst, Item)
#define XEN_MEMBER(Item, Lst)           fth_list_member_p(Lst, Item)

/* === Hook, Procedure === */
#define XEN_HOOK_P(Arg)                 FTH_HOOK_P(Arg)
#define XEN_HOOKED(a)                   (!fth_hook_empty_p(a))
#define XEN_DEFINE_HOOK(name, arity, help) fth_make_hook(name, arity, help)
#define XEN_DEFINE_SIMPLE_HOOK(arity)   fth_make_simple_hook(arity)
#define XEN_CLEAR_HOOK(Arg)             fth_hook_clear(Arg)
#define XEN_HOOK_PROCEDURES(Obj)        fth_hook_procedure_list(Obj)

#define XEN_PROCEDURE_P(Arg)            FTH_PROC_P(Arg)
#define XEN_PROCEDURE_NAME(Func)        C_TO_XEN_STRING(fth_proc_name(Func))
#define XEN_PROCEDURE_HELP(Name)        fth_documentation_ref(Name)
#define XEN_PROCEDURE_SOURCE_HELP(Name) XEN_PROCEDURE_HELP(Name)
#define XEN_PROCEDURE_SOURCE(Func)      fth_proc_source_ref(Func)
#define XEN_ARITY(Func)                 INT_TO_FIX(XEN_REQUIRED_ARGS(Func))
#define XEN_REQUIRED_ARGS(Func)         fth_proc_arity(Func)
#define XEN_REQUIRED_ARGS_OK(Func, args) (XEN_REQUIRED_ARGS(Func) == (args))

#define XEN_CALL_0(Func, Caller)                    fth_proc_call(Func, Caller, 0)
#define XEN_CALL_1(Func, Arg1, Caller)              fth_proc_call(Func, Caller, 1, Arg1)
#define XEN_CALL_2(Func, Arg1, Arg2, Caller)        fth_proc_call(Func, Caller, 2, Arg1, Arg2)
#define XEN_CALL_3(Func, Arg1, Arg2, Arg3, Caller)  fth_proc_call(Func, Caller, 3, Arg1, Arg2, Arg3)
#define XEN_CALL_4(Func, Arg1, Arg2, Arg3, Arg4, Caller) \
  fth_proc_call(Func, Caller, 4, Arg1, Arg2, Arg3, Arg4)
#define XEN_CALL_5(Func, Arg1, Arg2, Arg3, Arg4, Arg5, Caller) \
  fth_proc_call(Func, Caller, 5, Arg1, Arg2, Arg3, Arg4, Arg5)
#define XEN_CALL_6(Func, Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, Caller) \
  fth_proc_call(Func, Caller, 6, Arg1, Arg2, Arg3, Arg4, Arg5, Arg6)
#define XEN_APPLY(Func, Args, Caller)               fth_proc_apply(Func, Args, Caller)
#define XEN_APPLY_ARG_LIST_END                      XEN_EMPTY_LIST
#define XEN_CALL_0_NO_CATCH(Func)                   XEN_CALL_0(Func, NULL)
#define XEN_CALL_1_NO_CATCH(Func, Arg1)             XEN_CALL_1(Func, Arg1, NULL)
#define XEN_CALL_2_NO_CATCH(Func, Arg1, Arg2)       XEN_CALL_2(Func, Arg1, Arg2, NULL)
#define XEN_CALL_3_NO_CATCH(Func, Arg1, Arg2, Arg3) XEN_CALL_3(Func, Arg1, Arg2, Arg3, NULL)
#define XEN_APPLY_NO_CATCH(Func, Args)              XEN_APPLY(Func, Args, NULL)

/* === Define === */
#define XEN_DEFINE(name, Value)                fth_define(name, Value)
#define XEN_DEFINE_CONSTANT(name, Value, help) fth_define_constant(name, Value, help)
#define XEN_DEFINE_VARIABLE(name, Var, Value)  (Var = fth_define_variable(name, Value, NULL))
#define XEN_VARIABLE_SET(name, Value)          fth_variable_set((char *)(name), Value)
#define XEN_VARIABLE_REF(name)                 fth_variable_ref((char *)(name))
#define XEN_NAME_AS_C_STRING_TO_VARIABLE(name) fth_word_ref((char *)(name))
#define XEN_NAME_AS_C_STRING_TO_VALUE(name)    XEN_VARIABLE_REF(name)

#ifdef __cplusplus
# define XEN_PROCEDURE_CAST (XEN (*)())
#else
# define XEN_PROCEDURE_CAST
#endif

#define XEN_DEFINE_PROCEDURE(Name, Func, ReqArg, OptArg, RstArg, Doc) \
  do { \
    fth_current_file = __FILE__; \
    fth_current_line = __LINE__; \
    fth_define_procedure(Name, XEN_PROCEDURE_CAST Func, ReqArg, OptArg, RstArg, Doc); \
  } while (0)

#define XEN_DEFINE_PROCEDURE_WITH_SETTER(Get_Name, Get_Func, Get_Help, Set_Name, Set_Func, Get_Req, Get_Opt, Set_Req, Set_Opt) \
  do { \
    XEN_DEFINE_PROCEDURE(Get_Name, XEN_PROCEDURE_CAST Get_Func, Get_Req, Get_Opt, 0, Get_Help); \
    XEN_DEFINE_PROCEDURE(Set_Name, XEN_PROCEDURE_CAST Set_Func, Set_Req, Set_Opt, 0, Get_Help); \
  } while (0)

#define XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(Get_Name, Get_Func, Get_Help, Set_Name, Set_Func, Rev_Func, Get_Req, Get_Opt, Set_Req, Set_Opt) \
  XEN_DEFINE_PROCEDURE_WITH_SETTER(Get_Name, Get_Func, Get_Help, Set_Name, Set_Func, Get_Req, Get_Opt, Set_Req, Set_Opt)

/* === Object === */
#define XEN_OBJECT_TYPE                 FTH
#define XEN_MARK_OBJECT_TYPE            void

#define XEN_MAKE_AND_RETURN_OBJECT(Tag, Val, Mark, Free) return(fth_make_instance(Tag, Val))

#define XEN_OBJECT_TYPE_P(Obj, Tag)     fth_object_is_instance_of(Obj, Tag)
#define XEN_OBJECT_REF(Obj)             fth_instance_ref_gen(Obj)
#define XEN_MAKE_OBJECT_TYPE(Typ, Siz)  fth_make_object_type(Typ)
#define XEN_OBJECT_HELP(Name)           fth_documentation_ref(Name)

#define XEN_WRAPPED_C_POINTER_P(a)      XEN_ULONG_P(a)
#define XEN_WRAP_C_POINTER(a)           C_TO_XEN_ULONG((unsigned long)(a))
#define XEN_UNWRAP_C_POINTER(a)         XEN_TO_C_ULONG(a)
#define XEN_PROTECT_FROM_GC(Obj)        fth_gc_protect(Obj)
#define XEN_UNPROTECT_FROM_GC(Obj)      fth_gc_unprotect(Obj)

#define XEN_MAKE_OBJECT_PRINT_PROCEDURE(Type, Wrapped_Print, Original_Print) \
  static XEN Wrapped_Print(XEN obj) \
  { \
    char * str = Original_Print((Type *)XEN_OBJECT_REF(obj)); \
    XEN val = C_TO_XEN_STRING(str); \
    FREE(str); \
    return val; \
  }

#define XEN_MAKE_OBJECT_FREE_PROCEDURE(Type, Wrapped_Free, Original_Free) \
  static void Wrapped_Free(XEN obj) \
  { \
    Original_Free((Type *)XEN_OBJECT_REF(obj)); \
  } 

/* === Error === */
#define XEN_ASSERT_TYPE(Assertion, Arg, Position, Caller, Correct_Type) \
  FTH_ASSERT_TYPE(Assertion, Arg, Position, Caller, Correct_Type)
#define XEN_ERROR_TYPE(Typ)             fth_exception(Typ)

#define XEN_ERROR(Type, Info)           fth_throw_list(Type, Info)
#define XEN_THROW(Type, Info)           XEN_ERROR(Type, Info)

#define XEN_OUT_OF_RANGE_ERROR(Caller, ArgN, Arg, Descr) \
  FTH_OUT_OF_RANGE_ERROR(Caller, ArgN, Arg, Descr)
#define XEN_WRONG_TYPE_ARG_ERROR(Caller, ArgN, Arg, Descr) \
  FTH_WRONG_TYPE_ARG_ERROR(Caller, ArgN, Arg, Descr)

typedef XEN (*XEN_CATCH_BODY_TYPE) (void *data);

#endif /* end HAVE_FORTH */


/* ------------------------------ GAUCHE ------------------------------ */

#if HAVE_GAUCHE

/* gauche.h requires its config.h thereby clobbering some of our macros.  I'll try to protect the ones I notice */
#define LOCAL_SIZEOF_OFF_T SIZEOF_OFF_T
#undef SIZEOF_OFF_T
#include <gauche.h>
#undef SIZEOF_OFF_T
#define SIZEOF_OFF_T LOCAL_SIZEOF_OFF_T

#define XEN_OK 1

#define XEN_FILE_EXTENSION           "scm"
#define XEN_COMMENT_STRING           ";"
#define XEN_EMPTY_LIST               SCM_NIL
#define XEN_LANGUAGE_NAME            "Gauche"

#define XEN                          ScmObj
#define XEN_TRUE                     SCM_TRUE
#define XEN_FALSE                    SCM_FALSE
#define XEN_TRUE_P(a)                SCM_TRUEP(a)
#define XEN_FALSE_P(a)               SCM_FALSEP(a)

#define XEN_UNDEFINED                SCM_UNDEFINED
/* XEN_DEFINED_P is used in the sense of "is this identifier (a string) defined" */
/*   as a function argument, XEN_UNDEFINED is a marker that the given argument was not supplied */
#define XEN_DEFINED_P(Name)          (Scm_FindBinding(Scm_UserModule(), SCM_SYMBOL(SCM_INTERN(Name)), false) != NULL)
/* in Gauche, undefined != unbound (SCM_UNDEFINED, SCM_UNBOUND), but the distinction is blurred in other cases */
/* XEN_NOT_BOUND_P is applied to XEN objects, not strings */
#define XEN_BOUND_P(Arg)             (!(SCM_UNDEFINEDP(Arg)))
#define XEN_NOT_BOUND_P(Arg)         SCM_UNDEFINEDP(Arg)

#define XEN_EQ_P(a, b)               SCM_EQ(a, b)
#define XEN_EQV_P(A, B)              ((bool)Scm_EqvP(A, B))
#define XEN_EQUAL_P(A, B)            ((bool)Scm_EqualP(A, B))
#define XEN_NULL_P(a)                SCM_NULLP(a)

#define XEN_BOOLEAN_P(Arg)           SCM_BOOLP(Arg)
#define C_TO_XEN_BOOLEAN(a)          SCM_MAKE_BOOL(a)
#define XEN_TO_C_BOOLEAN(a)          SCM_BOOL_VALUE(a)

#define XEN_CAR(a)                   SCM_CAR(a)
#define XEN_CADR(a)                  SCM_CADR(a)
#define XEN_CADDR(a)                 SCM_CAR(SCM_CDDR(a))
#define XEN_CADDDR(a)                SCM_CAR(SCM_CDR(SCM_CDDR(a)))
#define XEN_CDR(a)                   SCM_CDR(a)
#define XEN_CDDR(a)                  SCM_CDDR(a)

#define XEN_CONS_P(Arg)              SCM_PAIRP(Arg)
#define XEN_PAIR_P(Arg)              ((SCM_PAIRP(Arg)) && (SCM_DOTTED_LIST_P(Arg)))
/* probably not PAIRP since it seems to refer to any list or even #f!! */
#define XEN_CONS(Arg1, Arg2)         Scm_Cons(Arg1, Arg2)
#define XEN_CONS_2(Arg1, Arg2, Arg3) Scm_Cons(Arg1, Scm_Cons(Arg2, Arg3))

#define XEN_LIST_P(Arg)              ((SCM_LISTP(Arg)) && (SCM_PROPER_LIST_P(Arg)))
#define XEN_LIST_P_WITH_LENGTH(Arg, Len) ((Len = ((int)Scm_Length(Arg))) >= 0)
#define XEN_LIST_LENGTH(Arg)         Scm_Length(Arg)
#define XEN_LIST_REF(Lst, Num)       Scm_ListRef(Lst, Num, XEN_FALSE)
#define XEN_LIST_SET(Lst, Loc, Val)  xen_gauche_list_set_x(Lst, Loc, Val)
#define XEN_LIST_REVERSE(Lst)        Scm_Reverse(Lst)
#define XEN_LIST_1(a)                SCM_LIST1(a)
#define XEN_LIST_2(a, b)             SCM_LIST2(a, b)
#define XEN_LIST_3(a, b, c)          SCM_LIST3(a, b, c)
#define XEN_LIST_4(a, b, c, d)       SCM_LIST4(a, b, c, d)
#define XEN_LIST_5(a, b, c, d, e)    SCM_LIST5(a, b, c, d, e)
#define XEN_LIST_6(a, b, c, d, e, f)          Scm_List(a, b, c, d, e, f, NULL)
#define XEN_LIST_7(a, b, c, d, e, f, g)       Scm_List(a, b, c, d, e, f, g, NULL)
#define XEN_LIST_8(a, b, c, d, e, f, g, h)    Scm_List(a, b, c, d, e, f, g, h, NULL)
#define XEN_LIST_9(a, b, c, d, e, f, g, h, i) Scm_List(a, b, c, d, e, f, g, h, i, NULL)
#define XEN_APPEND(a, b)             Scm_Append2(a, b)
#define XEN_COPY_ARG(Lst)            Lst
#define XEN_MEMBER(a, b)             Scm_Member(a, b, SCM_CMP_EQUAL)
#define XEN_ASSOC(a, b)              Scm_Assoc(a, b, SCM_CMP_EQUAL)

#define XEN_VECTOR_P(Arg)            SCM_VECTORP(Arg)
#define XEN_VECTOR_LENGTH(Arg)       SCM_VECTOR_SIZE(Arg)
#define XEN_VECTOR_REF(Vect, Num)    Scm_VectorRef(SCM_VECTOR(Vect), Num, XEN_FALSE)
#define XEN_VECTOR_SET(Vect, Num, Val) Scm_VectorSet(SCM_VECTOR(Vect), Num, Val)
#define XEN_VECTOR_TO_LIST(Vect)     Scm_VectorToList(SCM_VECTOR(Vect), 0, XEN_VECTOR_LENGTH(Vect) - 1)
#define XEN_MAKE_VECTOR(Num, Fill)   Scm_MakeVector(Num, Fill)

#define XEN_NUMBER_P(Arg)            SCM_REALP(Arg)
#define XEN_ZERO                     SCM_MAKE_INT(0)
#define XEN_INTEGER_P(Arg)           SCM_INTEGERP(Arg)

/* Gauche "ints" are apparently 29-bit quantities -- might have to stick with ULONG everywhere */
#if defined(__GNUC__) && (!(defined(__cplusplus)))
  #define XEN_TO_C_INT(a)               ({ XEN _xen_ga_9_ = a; (SCM_INTP(_xen_ga_9_) ? SCM_INT_VALUE(_xen_ga_9_) : Scm_GetInteger(_xen_ga_9_)); })
  #define XEN_TO_C_INT_OR_ELSE(a, b)    ({ XEN _xen_ga_1_ = a; ((XEN_INTEGER_P(_xen_ga_1_)) ? XEN_TO_C_INT(_xen_ga_1_) : b); })
  #define XEN_TO_C_DOUBLE_OR_ELSE(a, b) ({ XEN _xen_ga_2_ = a; ((XEN_NUMBER_P(_xen_ga_2_)) ? XEN_TO_C_DOUBLE(_xen_ga_2_) : b); })
  #define C_TO_XEN_INT(a)               ({ int _xen_ga_8_ = a; \
                                            (SCM_SMALL_INT_FITS(_xen_ga_8_)) ? \
                                              SCM_MAKE_INT(_xen_ga_8_) : Scm_MakeInteger((long)_xen_ga_8_); })
#else
  #define XEN_TO_C_INT(a)               (SCM_INTP(a) ? SCM_INT_VALUE(a) : Scm_GetInteger(a))
  #define XEN_TO_C_INT_OR_ELSE(a, b)    ((XEN_INTEGER_P(a)) ? XEN_TO_C_INT(a) : b)
  #define XEN_TO_C_DOUBLE_OR_ELSE(a, b) ((XEN_NUMBER_P(a)) ? XEN_TO_C_DOUBLE(a) : b)
  #define C_TO_XEN_INT(a)               Scm_MakeInteger((long)a)
#endif
#define XEN_DOUBLE_P(Arg)            SCM_REALP(Arg)
#define XEN_TO_C_DOUBLE(a)           xen_to_c_double(a)
#define C_TO_XEN_DOUBLE(a)           Scm_MakeFlonum(a)
#define XEN_TO_C_ULONG(a)            Scm_GetIntegerU(a)
#define C_TO_XEN_ULONG(a)            Scm_MakeIntegerU((unsigned long)a)
#define XEN_ULONG_P(Arg)             SCM_INTEGERP(Arg)
#define XEN_EXACT_P(Arg)             SCM_EXACTP(Arg)
#define C_TO_XEN_LONG_LONG(a)        Scm_MakeBignumFromSI(a)
#define XEN_COMPLEX_P(Arg)           SCM_COMPLEXP(Arg)

#if defined(__GNUC__) && (!(defined(__cplusplus)))
  #define XEN_OFF_T_P(Arg)           ({ XEN _xen_ga_3_ = Arg; (SCM_INTEGERP(_xen_ga_3_) || SCM_BIGNUMP(_xen_ga_3_)); })
  #define XEN_TO_C_LONG_LONG(a)      ({ XEN _xen_ga_4_ = a; \
                                         (SCM_BIGNUMP(_xen_ga_4_) ? \
                                            ((off_t)(Scm_BignumToSI64(SCM_BIGNUM(_xen_ga_4_), SCM_CLAMP_NONE, NULL))) : \
                                            ((off_t)XEN_TO_C_INT(_xen_ga_4_))); })
  #define XEN_TO_C_COMPLEX(a)        ({ XEN _xen_ga_5_ = a; (SCM_COMPLEX_REAL(_xen_ga_5_) + SCM_COMPLEX_IMAG(_xen_ga_5_) * _Complex_I); })
  #define C_TO_XEN_COMPLEX(a)        ({ complex double _xen_ga_6_ = a; Scm_MakeComplex(creal(_xen_ga_6_), cimag(_xen_ga_6_)); })
#else
  #define XEN_OFF_T_P(Arg)           (SCM_INTEGERP(Arg) || SCM_BIGNUMP(Arg))
  #define XEN_TO_C_LONG_LONG(a)      (SCM_BIGNUMP(a) ? ((off_t)(Scm_BignumToSI64(SCM_BIGNUM(a), SCM_CLAMP_NONE, NULL))) : ((off_t)XEN_TO_C_INT(a)))
  #define XEN_TO_C_COMPLEX(a)        (SCM_COMPLEX_REAL(a) + SCM_COMPLEX_IMAG(a) * _Complex_I)
  #define C_TO_XEN_COMPLEX(a)        Scm_MakeComplex(creal(a), cimag(a))
#endif

#define XEN_CHAR_P(Arg)              SCM_CHARP(Arg)
#define XEN_TO_C_CHAR(Arg)           SCM_CHAR_VALUE(Arg)
#define C_TO_XEN_CHAR(c)             SCM_MAKE_CHAR(c)

#define XEN_KEYWORD_P(Obj)           SCM_KEYWORDP(Obj)
#define XEN_MAKE_KEYWORD(Arg)        SCM_MAKE_KEYWORD(Arg)
#define XEN_KEYWORD_EQ_P(k1, k2)     XEN_EQ_P(k1, k2)

#define XEN_STRING_P(Arg)            SCM_STRINGP(Arg)
#define XEN_TO_C_STRING(Str)         Scm_GetString(SCM_STRING(Str))
#define C_TO_XEN_STRING(a)           (a) ? SCM_MAKE_STR_COPYING(a) : XEN_FALSE
#define C_TO_XEN_STRINGN(Str, Len)   Scm_MakeString(Str, Len, Len, SCM_MAKSTR_COPYING)
#define C_STRING_TO_XEN_FORM(Str)    Scm_ReadFromCString(Str)
#define XEN_EVAL_FORM(Form)          Scm_Eval(Form, SCM_OBJ(Scm_UserModule()))
#define XEN_EVAL_C_STRING(Arg)       xen_gauche_eval_c_string(Arg)
#define XEN_SYMBOL_P(Arg)            SCM_SYMBOLP(Arg)
#define XEN_SYMBOL_TO_C_STRING(a)    XEN_TO_C_STRING(SCM_SYMBOL_NAME(a))
#define XEN_TO_STRING(Obj)           xen_gauche_object_to_string(Obj)

#define XEN_WRAP_C_POINTER(a)        ((XEN)(C_TO_XEN_ULONG((unsigned long)a)))
#define XEN_UNWRAP_C_POINTER(a)      XEN_TO_C_ULONG(a)
#define XEN_WRAPPED_C_POINTER_P(a)   XEN_NUMBER_P(a)

#define XEN_DEFINE_CONSTANT(Name, Value, Help) xen_gauche_define_constant(Name, Value, Help)
#define XEN_DEFINE_VARIABLE(Name, Var, Value)  Var = SCM_DEFINE(Scm_UserModule(), Name, Value)
#define C_STRING_TO_XEN_SYMBOL(a)           SCM_INTERN(a)
#define XEN_NAME_AS_C_STRING_TO_VARIABLE(a) Scm_FindBinding(Scm_UserModule(), SCM_SYMBOL(SCM_INTERN(a)), false)
#define XEN_SYMBOL_TO_VARIABLE(a)           Scm_FindBinding(Scm_UserModule(), SCM_SYMBOL(a), false)
#define XEN_VARIABLE_REF(Var)               Scm_SymbolValue(Scm_UserModule(), SCM_SYMBOL(SCM_INTERN(Var)))
#define XEN_VARIABLE_SET(Var, Val)          SCM_GLOC_SET(SCM_GLOC(XEN_NAME_AS_C_STRING_TO_VARIABLE(Var)), Val)
#define XEN_NAME_AS_C_STRING_TO_VALUE(a)    XEN_VARIABLE_REF(a)

#define XEN_SET_DOCUMENTATION(Name, Help)   xen_gauche_set_help((XEN)(SCM_INTERN(Name)), Help)
#define XEN_DOCUMENTATION_SYMBOL            SCM_SYMBOL(SCM_INTERN("documentation"))
#define XEN_OBJECT_HELP(Name)               xen_gauche_help(Name)

#define XEN_MAKE_AND_RETURN_OBJECT(Tag, Val, Protect, Ignore2) return(xen_gauche_make_object(Tag, (void *)Val, Protect))
/* tag here is int -- needs ScmClass* for underlying call */
#define XEN_OBJECT_REF(a)                   xen_gauche_object_ref(a)
#define XEN_OBJECT_TYPE                     int
/* the "Tag" type in other calls */
#define XEN_MAKE_OBJECT_TYPE(Type, Size, Print, Cleanup) xen_gauche_new_type(Type, Print, Cleanup)
/* Type here is a string like "Vct" */
#define XEN_MARK_OBJECT_TYPE                ScmObj
/* used only in clm2xen for mark_mus_xen */
#define XEN_OBJECT_TYPE_P(OBJ, TAG)         xen_gauche_type_p(OBJ, TAG)

#define XEN_MAKE_OBJECT_PRINT_PROCEDURE(Type, Wrapped_Print, Original_Print) \
  static void Wrapped_Print(XEN obj, ScmPort *port, ScmWriteContext *pstate) \
  { \
    char *str; \
    str = Original_Print((Type *)XEN_OBJECT_REF(obj)); \
    XEN_PUTS(str, port); \
    FREE(str); \
  }

#define XEN_MAKE_OBJECT_FREE_PROCEDURE(Type, Wrapped_Free, Original_Free) \
  static void Wrapped_Free(XEN obj) \
  { \
    Original_Free((Type *)XEN_OBJECT_REF(obj)); \
  }

#define XEN_YES_WE_HAVE(Feature)      xen_gauche_provide(Feature)
#define XEN_PROTECT_FROM_GC(Obj)      xen_gauche_permanent_object(Obj)
#define XEN_LOAD_FILE(File)           xen_gauche_load_file(File)
#define XEN_LOAD_FILE_WITH_PATH(File) Scm_VMLoad(SCM_STRING(C_TO_XEN_STRING(File)), XEN_FALSE, XEN_FALSE, 0)
#define XEN_LOAD_PATH                 Scm_GetLoadPath()
#define XEN_ADD_TO_LOAD_PATH(Path)    xen_gauche_add_to_load_path(Path)

#define XEN_DEFINE(Name, Value)       SCM_DEFINE(Scm_UserModule(), Name, Value)

#define XEN_HOOK_P(Arg)                    xen_gauche_hook_p(Arg)
#define XEN_DEFINE_HOOK(Name, Arity, Help) xen_gauche_define_hook(Name, Arity, Help)
/* "simple hooks are for channel-local hooks (unnamed, accessed through the channel) */
#define XEN_DEFINE_SIMPLE_HOOK(Arity)      xen_gauche_define_hook(NULL, Arity, NULL)
#define XEN_HOOKED(Arg)                    (!xen_gauche_hook_empty_p(Arg))
#define XEN_CLEAR_HOOK(Arg)                xen_gauche_reset_hook(Arg)
#define XEN_HOOK_PROCEDURES(Arg)           xen_gauche_hook_to_list(Arg)

#define XEN_ERROR_TYPE(Typ)                 C_STRING_TO_XEN_SYMBOL(Typ)
#define XEN_ERROR(Type, Info)               Scm_Raise(XEN_CONS(Type, Info))
#define XEN_THROW(Tag, Arg)                 Scm_Raise(XEN_CONS(Tag, Arg))

#define XEN_ASSERT_TYPE(Assertion, Arg, Position, Caller, Correct_Type) \
  do {if (!(Assertion)) \
      XEN_ERROR(XEN_ERROR_TYPE("wrong-type-arg"),\
		XEN_LIST_3(C_TO_XEN_STRING(Caller),			\
			   C_TO_XEN_STRING("wrong type argument (arg %S): %S, wanted %S"), \
			   XEN_LIST_3(C_TO_XEN_INT(Position), \
				      Arg,				\
				      C_TO_XEN_STRING(Correct_Type))));} while (0)

#define XEN_WRONG_TYPE_ARG_ERROR(Caller, ArgN, Arg, Descr) XEN_ASSERT_TYPE(false, Arg, ArgN, Caller, Descr)

#define XEN_OUT_OF_RANGE_ERROR(Caller, ArgN, Arg, Descr) \
  XEN_ERROR(XEN_ERROR_TYPE("out-of-range"), \
            XEN_LIST_3(C_TO_XEN_STRING(Caller), \
                       C_TO_XEN_STRING(Descr), \
                       XEN_LIST_1(Arg)))

#define XEN_PROCEDURE_P(Arg)              SCM_PROCEDUREP(Arg)

/* unfortunately, SCM_PROCEDUREP doesn't include applicable objects, so it's not like the Guile case.
 *   we apparently have to use compute-applicable-method to find out whether an object
 *   has a matching object-apply method, but in much of the xen code, once XEN_PROCEDURE_P returns true,
 *   we assume we can check arity etc -- this is not pretty...  Here's a stab at the 1st level:
 *     ((SCM_PROCEDUREP(Arg)) || ((SCM_GENERICP(Arg)) && (!(SCM_NULLP((ScmObj)(Scm_ComputeApplicableMethods(SCM_GENERIC(Arg), NULL, 0)))))))
 *     then XEN_ARITY would be ((SCM_PROCEDUREP(Func)) ? <as is below> : -1) or something like that?
 *   but this is Ugly. So we're stuck -- Gauche users will have to wrap applicable objects in thunks.
 */

#define XEN_PROCEDURE_HELP(Sym)           XEN_OBJECT_HELP(Sym)
#define XEN_PROCEDURE_SOURCE_HELP(Name)   XEN_FALSE
#define XEN_PROCEDURE_SOURCE(Func)        XEN_FALSE
#define XEN_ARITY(Func)                   XEN_CONS(C_TO_XEN_INT(SCM_PROCEDURE_REQUIRED(Func)), C_TO_XEN_INT(SCM_PROCEDURE_OPTIONAL(Func)))
#define XEN_REQUIRED_ARGS(Func)           SCM_PROCEDURE_REQUIRED(Func)
#define XEN_REQUIRED_ARGS_OK(Func, Args)  (SCM_PROCEDURE_REQUIRED(Func) == Args)

#ifndef __cplusplus
#define XEN_PROCEDURE_CAST (XEN (*)())
#define XEN_DEFINE_PROCEDURE(Name, Func, ReqArg, OptArg, RstArg, Doc) \
  xen_gauche_define_procedure(Name, XEN_PROCEDURE_CAST Func, ReqArg, OptArg, RstArg, Doc)
#define XEN_DEFINE_PROCEDURE_WITH_SETTER(Get_Name, Get_Func, Get_Help, Set_Name, Set_Func, Get_Req, Get_Opt, Set_Req, Set_Opt) \
  xen_gauche_define_procedure_with_setter(Get_Name, Get_Func, Get_Help, Set_Func, Get_Req, Get_Opt, Set_Req, Set_Opt)
#define XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(Get_Name, Get_Func, Get_Help, Set_Name, Set_Func, Rev_Func, Get_Req, Get_Opt, Set_Req, Set_Opt) \
  xen_gauche_define_procedure_with_reversed_setter(Get_Name, Get_Func, Get_Help, Set_Func, Rev_Func, Get_Req, Get_Opt, Set_Req, Set_Opt)

void xen_gauche_define_procedure(char *Name, XEN (*Func)(), int ReqArg, int OptArg, int RstArg, char *Doc);
void xen_gauche_define_procedure_with_reversed_setter(char *get_name, XEN (*get_func)(), char *get_help, XEN (*set_func)(), XEN (*reversed_set_func)(), 
  int get_req, int get_opt, int set_req, int set_opt);
void xen_gauche_define_procedure_with_setter(char *get_name, XEN (*get_func)(), char *get_help, XEN (*set_func)(),
  int get_req, int get_opt, int set_req, int set_opt);

#else

#define XEN_PROCEDURE_CAST (ScmHeaderRec* (*)(ScmHeaderRec**, int, void*))
#define XEN_DEFINE_PROCEDURE(Name, Func, ReqArg, OptArg, RstArg, Doc) \
  xen_gauche_define_procedure(Name, XEN_PROCEDURE_CAST Func, ReqArg, OptArg, RstArg, Doc)
#define XEN_DEFINE_PROCEDURE_WITH_SETTER(Get_Name, Get_Func, Get_Help, Set_Name, Set_Func, Get_Req, Get_Opt, Set_Req, Set_Opt) \
  xen_gauche_define_procedure_with_setter(Get_Name, XEN_PROCEDURE_CAST Get_Func, Get_Help, XEN_PROCEDURE_CAST Set_Func, Get_Req, Get_Opt, Set_Req, Set_Opt)
#define XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(Get_Name, Get_Func, Get_Help, Set_Name, Set_Func, Rev_Func, Get_Req, Get_Opt, Set_Req, Set_Opt) \
  xen_gauche_define_procedure_with_reversed_setter(Get_Name, XEN_PROCEDURE_CAST Get_Func, Get_Help, XEN_PROCEDURE_CAST Set_Func, XEN_PROCEDURE_CAST Rev_Func, Get_Req, Get_Opt, Set_Req, Set_Opt)

void xen_gauche_define_procedure(char *Name, ScmHeaderRec* (*Func)(ScmHeaderRec**, int, void*), int ReqArg, int OptArg, int RstArg, char *Doc);
void xen_gauche_define_procedure_with_reversed_setter(char *get_name, ScmHeaderRec* (*get_func)(ScmHeaderRec**, int, void*), char *get_help, 
  ScmHeaderRec* (*set_func)(ScmHeaderRec**, int, void*), ScmHeaderRec* (*reversed_set_func)(ScmHeaderRec**, int, void*), 
  int get_req, int get_opt, int set_req, int set_opt);
void xen_gauche_define_procedure_with_setter(char *get_name, ScmHeaderRec* (*get_func)(ScmHeaderRec**, int, void*), char *get_help, 
  ScmHeaderRec* (*set_func)(ScmHeaderRec**, int, void*),
  int get_req, int get_opt, int set_req, int set_opt);
#endif

#define XEN_CALL_0(Func, Caller)                   Scm_Apply(Func, XEN_EMPTY_LIST)
#define XEN_CALL_1(Func, Arg1, Caller)             Scm_Apply(Func, XEN_LIST_1(Arg1))
#define XEN_CALL_2(Func, Arg1, Arg2, Caller)       Scm_Apply(Func, XEN_LIST_2(Arg1, Arg2))
#define XEN_CALL_3(Func, Arg1, Arg2, Arg3, Caller) Scm_Apply(Func, XEN_LIST_3(Arg1, Arg2, Arg3))
#define XEN_APPLY(Func, Args, Caller)              Scm_Apply(Func, Args)
#define XEN_CALL_4(Func, Arg1, Arg2, Arg3, Arg4, Caller) Scm_Apply(Func, XEN_LIST_4(Arg1, Arg2, Arg3, Arg4))
#define XEN_CALL_5(Func, Arg1, Arg2, Arg3, Arg4, Arg5, Caller) Scm_Apply(Func, XEN_LIST_5(Arg1, Arg2, Arg3, Arg4, Arg5))
#define XEN_CALL_6(Func, Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, Caller) Scm_Apply(Func, XEN_LIST_6(Arg1, Arg2, Arg3, Arg4, Arg5, Arg6))
#define XEN_APPLY_NO_CATCH(Func, Args)              Scm_Apply(Func, Args)
#define XEN_CALL_0_NO_CATCH(Func)                   Scm_Apply(Func, XEN_EMPTY_LIST)
#define XEN_CALL_1_NO_CATCH(Func, Arg1)             Scm_Apply(Func, XEN_LIST_1(Arg1))
#define XEN_CALL_2_NO_CATCH(Func, Arg1, Arg2)       Scm_Apply(Func, XEN_LIST_2(Arg1, Arg2))
#define XEN_CALL_3_NO_CATCH(Func, Arg1, Arg2, Arg3) Scm_Apply(Func, XEN_LIST_3(Arg1, Arg2, Arg3))

/* puts arg=string, display arg=obj */
#define XEN_PUTS(Str, Port)      Scm_Puts(SCM_STRING(C_TO_XEN_STRING(Str)), Port)
#define XEN_DISPLAY(Val, Port)   xen_gauche_display(Val, Port)
#define XEN_FLUSH_PORT(Port)     Scm_Flush(Port)
#define XEN_CLOSE_PORT(Port)     Scm_ClosePort(Port)
#define XEN_PORT_TO_STRING(Port) Scm_GetOutputString(SCM_PORT(Port))

typedef XEN (*XEN_CATCH_BODY_TYPE) (void *data);

#define XEN_ARGIFY_1(OutName, InName) \
  static XEN OutName(XEN *argv, int argc, void *self) \
  { \
    XEN args[1];\
    SCM_ENTER_SUBR(#InName); \
    xen_gauche_load_args(args, argc, 1, argv);	\
    return(InName(args[0])); \
  }

#define XEN_ARGIFY_2(OutName, InName) \
  static XEN OutName(XEN *argv, int argc, void *self) \
  { \
    XEN args[2];\
    SCM_ENTER_SUBR(#InName); \
    xen_gauche_load_args(args, argc, 2, argv);	\
    return(InName(args[0], args[1])); \
  }

#define XEN_ARGIFY_3(OutName, InName) \
  static XEN OutName(XEN *argv, int argc, void *self) \
  { \
    XEN args[3];\
    SCM_ENTER_SUBR(#InName); \
    xen_gauche_load_args(args, argc, 3, argv);    \
    return(InName(args[0], args[1], args[2]));	    \
  }

#define XEN_ARGIFY_4(OutName, InName) \
  static XEN OutName(XEN *argv, int argc, void *self) \
  { \
    XEN args[4];\
    SCM_ENTER_SUBR(#InName); \
    xen_gauche_load_args(args, argc, 4, argv);	\
    return(InName(args[0], args[1], args[2], args[3]));	\
  }

#define XEN_ARGIFY_5(OutName, InName) \
  static XEN OutName(XEN *argv, int argc, void *self) \
  { \
    XEN args[5];\
    SCM_ENTER_SUBR(#InName); \
    xen_gauche_load_args(args, argc, 5, argv);			\
    return(InName(args[0], args[1], args[2], args[3], args[4]));	\
  }

#define XEN_ARGIFY_6(OutName, InName) \
  static XEN OutName(XEN *argv, int argc, void *self) \
  { \
    XEN args[6];\
    SCM_ENTER_SUBR(#InName); \
    xen_gauche_load_args(args, argc, 6, argv);			\
    return(InName(args[0], args[1], args[2], args[3], args[4], args[5]));	\
  }

#define XEN_ARGIFY_7(OutName, InName) \
  static XEN OutName(XEN *argv, int argc, void *self) \
  { \
    XEN args[7];\
    SCM_ENTER_SUBR(#InName); \
    xen_gauche_load_args(args, argc, 7, argv);			\
    return(InName(args[0], args[1], args[2], args[3], args[4], args[5], args[6])); \
  }

#define XEN_ARGIFY_8(OutName, InName) \
  static XEN OutName(XEN *argv, int argc, void *self) \
  { \
    XEN args[8];\
    SCM_ENTER_SUBR(#InName); \
    xen_gauche_load_args(args, argc, 8, argv);			\
    return(InName(args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7])); \
  }

#define XEN_ARGIFY_9(OutName, InName) \
  static XEN OutName(XEN *argv, int argc, void *self) \
  { \
    XEN args[9];\
    SCM_ENTER_SUBR(#InName); \
    xen_gauche_load_args(args, argc, 9, argv);			\
    return(InName(args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7], args[8])); \
  }

#define XEN_ARGIFY_10(OutName, InName) \
  static XEN OutName(XEN *argv, int argc, void *self) \
  { \
    XEN args[10];\
    SCM_ENTER_SUBR(#InName); \
    xen_gauche_load_args(args, argc, 10, argv);			\
    return(InName(args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7], args[8], args[9])); \
  }

#define XEN_NARGIFY_0(OutName, InName) \
  static XEN OutName(XEN *argv, int argc, void *self) \
    {SCM_ENTER_SUBR(#InName); return(InName());}

#define XEN_NARGIFY_1(OutName, InName) \
  static XEN OutName(XEN *argv, int argc, void *self) \
    {SCM_ENTER_SUBR(#InName); return(InName(argv[0]));}

#define XEN_NARGIFY_2(OutName, InName) \
  static XEN OutName(XEN *argv, int argc, void *self) \
    {SCM_ENTER_SUBR(#InName); return(InName(argv[0], argv[1]));}

#define XEN_NARGIFY_3(OutName, InName) \
  static XEN OutName(XEN *argv, int argc, void *self) \
    {SCM_ENTER_SUBR(#InName); return(InName(argv[0], argv[1], argv[2]));}

#define XEN_NARGIFY_4(OutName, InName) \
  static XEN OutName(XEN *argv, int argc, void *self) \
    {SCM_ENTER_SUBR(#InName); return(InName(argv[0], argv[1], argv[2], argv[3]));}

#define XEN_NARGIFY_5(OutName, InName) \
  static XEN OutName(XEN *argv, int argc, void *self) \
    {SCM_ENTER_SUBR(#InName); return(InName(argv[0], argv[1], argv[2], argv[3], argv[4]));}

#define XEN_NARGIFY_6(OutName, InName) \
  static XEN OutName(XEN *argv, int argc, void *self) \
    {SCM_ENTER_SUBR(#InName); return(InName(argv[0], argv[1], argv[2], argv[3], argv[4], argv[5]));}

#define XEN_NARGIFY_7(OutName, InName) \
  static XEN OutName(XEN *argv, int argc, void *self) \
    {SCM_ENTER_SUBR(#InName); return(InName(argv[0], argv[1], argv[2], argv[3], argv[4], argv[5], argv[6]));}

#define XEN_NARGIFY_8(OutName, InName) \
  static XEN OutName(XEN *argv, int argc, void *self) \
    {SCM_ENTER_SUBR(#InName); return(InName(argv[0], argv[1], argv[2], argv[3], argv[4], argv[5], argv[6], argv[7]));}

#define XEN_NARGIFY_9(OutName, InName) \
  static XEN OutName(XEN *argv, int argc, void *self) \
    {SCM_ENTER_SUBR(#InName); return(InName(argv[0], argv[1], argv[2], argv[3], argv[4], argv[5], argv[6], argv[7], argv[8]));}

#define XEN_VARGIFY(OutName, InName) \
  static XEN OutName(XEN *argv, int argc, void *self) \
  { \
    SCM_ENTER_SUBR(#InName); \
    return(InName(argv[0])); \
  }

void xen_gauche_list_set_x(XEN Lst, int Loc, XEN Val);
XEN xen_gauche_load_file(char *file);
XEN xen_gauche_add_to_load_path(char *path);
XEN xen_gauche_object_to_string(XEN obj);
void xen_gauche_permanent_object(XEN obj);
double xen_to_c_double(XEN a);
void xen_gauche_load_args(XEN *args, int incoming_args, int args_size, XEN *arg_list);
XEN xen_gauche_eval_c_string(char *arg);
void xen_gauche_provide(const char *feature);
const char *xen_gauche_features(void);
XEN xen_gauche_make_object(XEN_OBJECT_TYPE type, void *val, XEN_MARK_OBJECT_TYPE (*protect_func)(XEN obj));
void *xen_gauche_object_ref(XEN obj);
XEN_OBJECT_TYPE xen_gauche_new_type(const char *name, ScmClassPrintProc print, ScmForeignCleanupProc cleanup);
bool xen_gauche_type_p(XEN obj, XEN_OBJECT_TYPE type);
XEN xen_gauche_help(XEN proc);
void xen_gauche_set_help(XEN proc, const char *help);
XEN xen_gauche_define_constant(const char *name, int value, const char *help);
XEN xen_gauche_define_hook(const char *name, int arity, const char *help);
XEN xen_gauche_hook_to_list(XEN hook);
XEN xen_gauche_reset_hook(XEN hook);
bool xen_gauche_hook_empty_p(XEN hook);
bool xen_gauche_hook_p(XEN val);
#endif



/* ------------------------------ NO EXTENSION LANGUAGE ------------------------------ */

#ifndef XEN_OK

#define XEN int
#define XEN_FILE_EXTENSION  "txt"
#define XEN_LANGUAGE_NAME "What Language?"
#define XEN_COMMENT_STRING  ";"
#define XEN_FALSE 0
#define XEN_TRUE 1
#define XEN_TRUE_P(a) ((a) == XEN_TRUE)
#define XEN_FALSE_P(a) ((a) == XEN_FALSE)
#define XEN_BOOLEAN_P(Arg) 0
#define C_TO_XEN_BOOLEAN(a) 0
#define XEN_TO_C_BOOLEAN(a) 0
#define XEN_NULL_P(a) ((a) == XEN_EMPTY_LIST)
#define XEN_BOUND_P(Arg) 0
#define XEN_NOT_BOUND_P(Arg) 1
#define XEN_EMPTY_LIST 0
#define XEN_UNDEFINED 0
#define XEN_EQ_P(a, b) 0
#define XEN_EQV_P(a, b) 0
#define XEN_EQUAL_P(a, b) 0
#define XEN_CONS_P(Arg) 0
#define XEN_CONS(Arg1, Arg2) 0
#define XEN_CONS_2(Arg1, Arg2, Arg3) 0
#define XEN_PAIR_P(Arg) 0
#define XEN_CAR(a) 0
#define XEN_CADR(a) 0
#define XEN_CADDR(a) 0
#define XEN_CADDDR(a) 0
#define XEN_CDR(a) 0
#define XEN_CDDR(a) 0
#define XEN_EMPTY_LIST 0
#define XEN_LIST_P(Arg) 0
#define XEN_LIST_P_WITH_LENGTH(Arg, Len) 0
#define XEN_LIST_LENGTH(Arg) 0
#define XEN_LIST_1(a) 0
#define XEN_LIST_2(a, b) 0
#define XEN_LIST_3(a, b, c) 0
#define XEN_LIST_4(a, b, c, d) 0
#define XEN_LIST_5(a, b, c, d, e) 0
#define XEN_LIST_6(a, b, c, d, e, f) 0
#define XEN_LIST_7(a, b, c, d, e, f, g) 0
#define XEN_LIST_8(a, b, c, d, e, f, g, h) 0
#define XEN_LIST_9(a, b, c, d, e, f, g, h, i) 0
#define XEN_LIST_REF(Lst, Num) 0
#define XEN_LIST_SET(Lst, Num, Val)
#define XEN_LIST_REVERSE(Lst) 0
#define XEN_COPY_ARG(Lst) Lst
#define XEN_APPEND(X, Y) 0
#define XEN_STRING_P(Arg) 0
#define XEN_NAME_AS_C_STRING_TO_VALUE(a) 0
#define XEN_TO_C_STRING(STR) "(not a string)"
#define C_TO_XEN_STRING(a) 0
#define C_TO_XEN_STRINGN(Str, Len) 0
#define C_STRING_TO_XEN_SYMBOL(a) 0
#define XEN_ZERO 0
#define XEN_NUMBER_P(Arg) 0
#define XEN_OFF_T_P(Arg) 0
#define XEN_DOUBLE_P(Arg) 0
#define XEN_TO_C_DOUBLE(a) 0.0
#define XEN_TO_C_DOUBLE_OR_ELSE(a, b) b
#define C_TO_XEN_DOUBLE(a) 0
#define XEN_INTEGER_P(Arg) 0
#define C_TO_XEN_INT(a) a
#define XEN_TO_C_INT(a) 0
#define XEN_TO_C_INT_OR_ELSE(a, b) b
#define XEN_ULONG_P(Arg) 0
#define XEN_TO_C_ULONG(a) 0
#define C_TO_XEN_ULONG(a) 0
#define C_TO_XEN_LONG_LONG(a) a
#define XEN_TO_C_LONG_LONG(a) a
#define XEN_EXACT_P(Arg) 0
#define C_STRING_TO_XEN_FORM(Str) 0
#define XEN_EVAL_FORM(Form) 0
#define XEN_EVAL_C_STRING(Arg) 0
#define XEN_SYMBOL_TO_C_STRING(a) "(not a symbol)"
#define XEN_TO_STRING(Obj) "(unknown)"
#define XEN_WRAP_C_POINTER(a) 0
#define XEN_UNWRAP_C_POINTER(a) 0
#define XEN_WRAPPED_C_POINTER_P(a) 0
#define XEN_PROCEDURE_P(Arg) 0
#define XEN_PROCEDURE_SOURCE(Func) 0
#define XEN_DEFINE_PROCEDURE(Name, Func, ReqArg, OptArg, RstArg, Doc)
#define XEN_DEFINE_PROCEDURE_WITH_SETTER(Get_Name, Get_Func, Get_Help, Set_Name, Set_Func, Get_Req, Get_Opt, Set_Req, Set_Opt)
#define XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(Get_Name, Get_Func, Get_Help, Set_Name, Set_Func, Rev_Func, Get_Req, Get_Opt, Set_Req, Set_Opt)
#define XEN_ARITY(Func) 0
#define XEN_REQUIRED_ARGS(Func) 0
#define XEN_REQUIRED_ARGS_OK(Func, Args) false
#define XEN_CALL_0(Func, Caller) 0
#define XEN_CALL_1(Func, Arg1, Caller) 0
#define XEN_CALL_2(Func, Arg1, Arg2, Caller) 0
#define XEN_CALL_3(Func, Arg1, Arg2, Arg3, Caller) 0
#define XEN_CALL_4(Func, Arg1, Arg2, Arg3, Arg4, Caller) 0
#define XEN_CALL_5(Func, Arg1, Arg2, Arg3, Arg4, Arg5, Caller) 0
#define XEN_CALL_6(Func, Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, Caller) 0
#define XEN_APPLY(Func, Args, Caller) 0
#define XEN_APPLY_ARG_LIST_END 0
#define XEN_CALL_0_NO_CATCH(Func) 0
#define XEN_CALL_1_NO_CATCH(Func, Arg1) 0
#define XEN_CALL_2_NO_CATCH(Func, Arg1, Arg2) 0
#define XEN_CALL_3_NO_CATCH(Func, Arg1, Arg2, Arg3) 0
#define XEN_APPLY_NO_CATCH(Func, Args) 0
#define XEN_DEFINE_CONSTANT(a, b, c)
#define XEN_DEFINE_VARIABLE(a, b, c)
#define XEN_DEFINE(Name, Value)
#define XEN_VARIABLE_SET(a, b)
#define XEN_VARIABLE_REF(a) 0
#define XEN_MARK_OBJECT_TYPE         XEN
#define XEN_MAKE_OBJECT_TYPE(Typ, Siz) 0
#define XEN_MAKE_OBJECT_PRINT_PROCEDURE(Type, Wrapped_Print, Original_Print) 
#define XEN_MAKE_OBJECT_FREE_PROCEDURE(Type, Wrapped_Free, Original_Free)
#define XEN_MAKE_AND_RETURN_OBJECT(Tag, Val, ig1, ig2) return(0)
#define XEN_OBJECT_REF(a) 0
#define XEN_OBJECT_TYPE int
#define XEN_OBJECT_TYPE_P(OBJ, TAG) 0
#define XEN_SYMBOL_P(Arg) 0
#define XEN_HOOK_P(Arg) 0
#define XEN_HOOKED(a) 0
#define XEN_DEFINE_HOOK(Name, Arity, Help) 0
#define XEN_DEFINE_SIMPLE_HOOK(Arity) 0
#define XEN_CLEAR_HOOK(Arg)
#define XEN_HOOK_PROCEDURES(a) 0
#define XEN_VECTOR_P(Arg) 0
#define XEN_VECTOR_LENGTH(Arg) 0
#define XEN_VECTOR_REF(Vect, Num) 0
#define XEN_VECTOR_SET(a, b, c)
#define XEN_MAKE_VECTOR(Num, Fill) 0
#define XEN_VECTOR_TO_LIST(Vect) 0
#define XEN_CHAR_P(Arg) 0
#define XEN_TO_C_CHAR(Arg) 0
#define C_TO_XEN_CHAR(Arg) 0
#define XEN_KEYWORD_P(Obj) 0
#define XEN_KEYWORD_EQ_P(k1, k2) 0
#define XEN_MAKE_KEYWORD(Arg) 0
#define XEN_YES_WE_HAVE(Feature)
#define XEN_DOCUMENTATION_SYMBOL 0
#define XEN_OBJECT_HELP(Name) 0
#define XEN_PROTECT_FROM_GC(a) 0
#define XEN_LOAD_FILE(a) 0
#define XEN_LOAD_FILE_WITH_PATH(a) 0
#define XEN_LOAD_PATH XEN_FALSE
#define XEN_ADD_TO_LOAD_PATH(Path) XEN_FALSE
#define XEN_ERROR_TYPE(Typ) XEN_FALSE
#define XEN_ERROR(Type, Info) fprintf(stderr, "error")
#define XEN_THROW(Type, Info) fprintf(stderr, "error")
#define XEN_ASSERT_TYPE(Assertion, Arg, Position, Caller, Correct_Type)
#define XEN_WRONG_TYPE_ARG_ERROR(Caller, ArgN, Arg, Descr)
#define XEN_OUT_OF_RANGE_ERROR(Caller, ArgN, Arg, Descr)
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
  #define XEN_INTEGER_OR_BOOLEAN_IF_BOUND_P(Arg) ({ XEN _xen_h_18_ = Arg; ((XEN_BOOLEAN_P(_xen_h_18_))   || (XEN_NOT_BOUND_P(_xen_h_18_)) || (XEN_INTEGER_P(_xen_h_18_))); })
  #define XEN_INTEGER_OR_BOOLEAN_P(Arg)          ({ XEN _xen_h_21_ = Arg; ((XEN_BOOLEAN_P(_xen_h_21_))   || (XEN_INTEGER_P(_xen_h_21_))); })
#else
  #define XEN_BOOLEAN_IF_BOUND_P(Arg)            ((XEN_BOOLEAN_P(Arg))   || (XEN_NOT_BOUND_P(Arg)))
  #define XEN_INTEGER_IF_BOUND_P(Arg)            ((XEN_NOT_BOUND_P(Arg)) || (XEN_INTEGER_P(Arg)))
  #define XEN_NUMBER_IF_BOUND_P(Arg)             ((XEN_NOT_BOUND_P(Arg)) || (XEN_NUMBER_P(Arg)))
  #define XEN_STRING_IF_BOUND_P(Arg)             ((XEN_NOT_BOUND_P(Arg)) || (XEN_STRING_P(Arg)))
  #define XEN_INTEGER_OR_BOOLEAN_IF_BOUND_P(Arg) ((XEN_BOOLEAN_P(Arg))   || (XEN_NOT_BOUND_P(Arg)) || (XEN_INTEGER_P(Arg)))
  #define XEN_INTEGER_OR_BOOLEAN_P(Arg)          ((XEN_BOOLEAN_P(Arg))   || (XEN_INTEGER_P(Arg)))
#endif

#if HAVE_GUILE
#define XEN_ONLY_ARG 0
#else
#define XEN_ONLY_ARG 1
#endif
#define XEN_ARG_1    1
#define XEN_ARG_2    2
#define XEN_ARG_3    3
#define XEN_ARG_4    4
#define XEN_ARG_5    5
#define XEN_ARG_6    6
#define XEN_ARG_7    7
#define XEN_ARG_8    8
#define XEN_ARG_9    9
#define XEN_ARG_10   10
/* 10 is the limit in Guile (SCM_GSUBR_MAX in gsubr.h), 10000 in Gauche (VM_STACK_SIZE in src/gauche/vm.h), not sure about Ruby or Forth */

#define XEN_TO_C_OFF_T_OR_ELSE(a, b)  xen_to_c_off_t_or_else(a, b)
#define C_TO_XEN_OFF_T(a)             c_to_xen_off_t(a)
#define XEN_TO_C_OFF_T(a)             xen_to_c_off_t(a)
#define XEN_AS_STRING(form)           XEN_TO_C_STRING(XEN_TO_STRING(form))

#define XEN_BAD_ARITY_ERROR(Caller, ArgN, Arg, Descr) \
  XEN_ERROR(XEN_ERROR_TYPE("bad-arity"), \
            XEN_LIST_3(C_TO_XEN_STRING(Caller), \
                       C_TO_XEN_STRING(Descr), \
                       Arg))

#ifndef XEN_HAVE_RATIOS
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
int xen_to_c_int_or_else(XEN obj, int fallback);
off_t xen_to_c_off_t_or_else(XEN obj, off_t fallback);
off_t xen_to_c_off_t(XEN obj);
XEN c_to_xen_off_t(off_t val);
char *xen_version(void);
void xen_repl(int argc, char **argv);
void xen_initialize(void);
void xen_gc_mark(XEN val);

#endif
