#ifndef XEN_H
#define XEN_H

/* macros for extended language support (Guile, MzScheme, Librep, Ruby, None)
 *
 * "xen" from Greek xenos (guest, foreigner)
 */

/* there are still lingering "scm"-isms here... */

#ifndef __GNUC__
  #ifndef __FUNCTION__
    #define __FUNCTION__ ""
  #endif
#endif


/* ------------------------------ GUILE ------------------------------ */

#if HAVE_GUILE

#define XEN_OK 1

#define XEN                 SCM
#define xen_return_first    scm_return_first
#define XEN_TRUE            SCM_BOOL_T
#define XEN_FALSE           SCM_BOOL_F

#define XEN_FILE_EXTENSION  "scm"

#if HAVE_SCM_LIST_N
  #define XEN_EMPTY_LIST    SCM_LIST0
#else
  #define XEN_EMPTY_LIST    SCM_EOL
#endif

#define XEN_UNDEFINED       SCM_UNDEFINED

#ifdef __cplusplus
  #define XEN_PROCEDURE_CAST (XEN (*)())
#else
  #define XEN_PROCEDURE_CAST
#endif

#ifndef SCM_EQ_P
  #define SCM_EQ_P(a, b)   ((a) == (b))
#endif

#if (SCM_DEBUG_TYPING_STRICTNESS == 2)
  #define XEN_MAKE_AND_RETURN_OBJECT(Tag, Val, Ignore1, Ignore2) SCM_RETURN_NEWSMOB(Tag, Val)
#else
  #define XEN_MAKE_AND_RETURN_OBJECT(Tag, Val, Ignore1, Ignore2) SCM_RETURN_NEWSMOB(Tag, (XEN)Val)
#endif
#define XEN_OBJECT_REF(a) SCM_SMOB_DATA(a)
/* remember to check the smob type agreement before calling XEN_OBJECT_REF! */
#define XEN_MAKE_OBJECT(a, b, c, ig1, ig2)  SCM_NEWSMOB(a, b, c)

#define VARIABLE_REF                   SCM_VARIABLE_REF
#if HAVE_SCM_C_DEFINE
  #define XEN_VARIABLE_SET(Var, Val)   SCM_VARIABLE_SET(Var, Val)
  #define XEN_NAME_AS_C_STRING_TO_VALUE(a) VARIABLE_REF(scm_sym2var(scm_str2symbol(a), scm_current_module_lookup_closure (), XEN_TRUE))
#else
  #define XEN_VARIABLE_SET(Var, Val)   SCM_SETCDR(Var, Val)
  #define XEN_NAME_AS_C_STRING_TO_VALUE(a) scm_symbol_value0(a)
#endif

#if HAVE_SCM_REMEMBER_UPTO_HERE
  #define XEN_OBJECT_TYPE              scm_bits_t
  #define SND_SMOB_TYPE(TAG, OBJ) SCM_SMOB_PREDICATE(TAG, OBJ)
#else
  #define XEN_OBJECT_TYPE              long
  #define SND_SMOB_TYPE(TAG, OBJ) (SCM_TYP16(OBJ) == (XEN)TAG)
#endif

#define XEN_OBJECT_TYPE_P(Obj, Type) ((SCM_NIMP(Obj)) && (SND_SMOB_TYPE(Type, Obj)))
#define XEN_FREE_OBJECT_TYPE         scm_sizet
#define XEN_MARK_OBJECT_TYPE         SCM

#define XEN_TRUE_P(a)                SCM_EQ_P(a, XEN_TRUE)
#define XEN_FALSE_P(a)               SCM_EQ_P(a, XEN_FALSE)
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
#define XEN_EQ_P(a, b)               SCM_EQ_P(a, b)

#define XEN_ONLY_ARG SCM_ARGn
#define XEN_ARG_1    SCM_ARG1
#define XEN_ARG_2    SCM_ARG2
#define XEN_ARG_3    SCM_ARG3
#define XEN_ARG_4    SCM_ARG4
#define XEN_ARG_5    SCM_ARG5
#define XEN_ARG_6    SCM_ARG6
#define XEN_ARG_7    SCM_ARG7
#define XEN_ARG_8    8


#define XEN_TO_C_DOUBLE(a)            scm_num2dbl(a,  __FUNCTION__)
#define XEN_TO_C_DOUBLE_OR_ELSE(a, b) ((XEN_NUMBER_P(a)) ? (scm_num2dbl(a,  __FUNCTION__)) : (b))
#define XEN_TO_C_DOUBLE_WITH_CALLER(a, b) scm_num2dbl(a, b)

#if HAVE_SCM_NUM2INT
  #define XEN_TO_C_INT(a)             ((int)scm_num2int(a, 0, __FUNCTION__))
#else
  #define XEN_TO_C_INT(a)             ((int)gh_scm2int(a))
#endif

#define XEN_TO_C_INT_OR_ELSE(a, b)    to_c_int_or_else(a, b, __FUNCTION__)
#define XEN_TO_C_INT_OR_ELSE_WITH_CALLER(a, b, c) to_c_int_or_else(a, b, c)

#ifndef SCM_STRING_CHARS
  #define XEN_TO_C_STRING(STR)        SCM_CHARS(STR)
#else
  #define XEN_TO_C_STRING(STR)        SCM_STRING_CHARS(STR)
  /* this assumes its argument is an XEN string and does not allocate new space */
#endif

#if HAVE_SCM_MAKE_REAL
  #define C_TO_XEN_DOUBLE(a)          scm_make_real(a)
#else
  #define C_TO_XEN_DOUBLE(a)          scm_makdbl(a, 0.0)
#endif

#define C_TO_XEN_INT(a)               scm_long2num((long)a)
#define C_TO_SMALL_XEN_INT(a)         SCM_MAKINUM(a)
#define XEN_TO_SMALL_C_INT(a)         ((int)(SCM_INUM(a)))
#define XEN_TO_C_UNSIGNED_LONG(a)     scm_num2ulong(a, 0, __FUNCTION__)
#define C_TO_XEN_UNSIGNED_LONG(a)     scm_ulong2num(a)
#define C_TO_XEN_STRING(a)            scm_makfrom0str(a)
#define XEN_TO_NEW_C_STRING(a)        gh_scm2newstr(a, NULL)
#define C_TO_XEN_BOOLEAN(a)           ((a) ? XEN_TRUE : XEN_FALSE)
#define C_STRING_TO_XEN_SYMBOL(a)     gh_symbol2scm(a)
#define XEN_TO_C_BOOLEAN_OR_TRUE(a)   ((XEN_FALSE_P(a) || ((SCM_INUMP(a)) && (SCM_INUM(a) == 0))) ? 0 : 1)
#define XEN_TO_C_BOOLEAN(a)           ((XEN_FALSE_P(a)) ? 0 : 1)
#if HAVE_SCM_C_EVAL_STRING
  #define C_STRING_TO_XEN_FORM(Str)   scm_c_read_string(Str)
#else
  #define C_STRING_TO_XEN_FORM(Str)   scm_read_0str(Str)
#endif

#ifdef SCM_SYMBOL_CHARS
  #define XEN_EVAL_FORM(Form)         scm_eval((XEN)(Form), scm_interaction_environment())
  /* was scm_eval_x but I'm not sure that's safe */
  #define XEN_SYMBOL_TO_C_STRING(a)   SCM_SYMBOL_CHARS(a)
#else
  #define XEN_EVAL_FORM(Form)         scm_eval((XEN)(Form))
  #define XEN_SYMBOL_TO_C_STRING(a)   gh_symbol2newstr(a, NULL)
#endif

/* (need a way to pass an uninterpreted pointer from C to XEN then back to C) */
#if (SCM_DEBUG_TYPING_STRICTNESS == 2)
  #define XEN_WRAP_C_POINTER(a)       (C_TO_XEN_UNSIGNED_LONG((unsigned long)a))
#else
  #define XEN_WRAP_C_POINTER(a)       ((XEN)(C_TO_XEN_UNSIGNED_LONG((unsigned long)a)))
#endif

#define XEN_UNWRAP_C_POINTER(a)       XEN_TO_C_UNSIGNED_LONG(a)
#define XEN_WRAPPED_C_POINTER_P(a)    XEN_NOT_FALSE_P(scm_number_p(a))

#if HAVE_SCM_C_EVAL_STRING
  #define XEN_EVAL_C_STRING(Arg)      scm_c_eval_string(Arg)
#else
  #define XEN_EVAL_C_STRING(Arg)      scm_eval_0str(Arg)
#endif

#if HAVE_SCM_C_DEFINE_GSUBR
  #define XEN_NEW_PROCEDURE(Name, Func, Req, Opt, Rst) scm_c_define_gsubr(Name, Req, Opt, Rst, XEN_PROCEDURE_CAST Func)
#else
  #define XEN_NEW_PROCEDURE(Name, Func, Req, Opt, Rst) gh_new_procedure(Name, XEN_PROCEDURE_CAST Func, Req, Opt, Rst)
#endif

#if (!TIMING) && (!WITH_MCHECK)

#define XEN_DEFINE_PROCEDURE(Name, Func, ReqArg, OptArg, RstArg, Doc) \
  scm_set_procedure_property_x(XEN_NEW_PROCEDURE(Name, Func, ReqArg, OptArg, RstArg), local_doc, C_TO_XEN_STRING(Doc))

#else
#if (TIMING)
/* add timing calls */
#define XEN_DEFINE_PROCEDURE(Name, Func, ReqArg, OptArg, RstArg, Doc) \
  XEN_NEW_PROCEDURE(Name "-t", XEN_PROCEDURE_CAST Func, ReqArg, OptArg, RstArg); \
  { \
    int tag; \
    tag = new_time(Name); \
    XEN_EVAL_C_STRING(mus_format("(define " Name " \
                              (lambda args \
                                (begin \
                                  (start-time %d) \
                                  (let ((res (apply " Name "-t args))) \
                                    (stop-time %d) \
                                    res))))", \
                            tag, tag)); \
  }
#else
#define XEN_DEFINE_PROCEDURE(Name, Func, ReqArg, OptArg, RstArg, Doc) \
  XEN_NEW_PROCEDURE(Name "-t", XEN_PROCEDURE_CAST Func, ReqArg, OptArg, RstArg); \
  XEN_EVAL_C_STRING("(define " Name " \
                 (lambda args \
                   (let ((res #f)) \
                     (set! res (apply " Name "-t args)) \
                     (mcheck-all) \
                     res)))");
#endif
#endif

#if 0
#define DEFINE_SETPROC(Name, GetFunc, SetFunc, Req, Opt, Help) \
  define_procedure_with_setter(Name, XEN_PROCEDURE_CAST GetFunc, Help, \
			       "set-" Name, XEN_PROCEDURE_CAST SetFunc, local_doc, Req, Opt, Req + 1, Opt)

#define DEFINE_SETREVPROC(Name, GetFunc, SetFunc, RevFunc, Req, Opt, Help) \
  define_procedure_with_reversed_setter(Name, XEN_PROCEDURE_CAST GetFunc, Help, \
			       "set-" Name, XEN_PROCEDURE_CAST SetFunc, XEN_PROCEDURE_CAST RevFunc, local_doc, Req, Opt, Req + 1, Opt)
#endif

#if HAVE_SCM_C_DEFINE
  #define SND_DEFINE(a, b)         scm_c_define(a, b)
#else
  #define SND_DEFINE(a, b)         gh_define(a, b)
#endif

#define XEN_DEFINE_CONSTANT(Name, Value, Help) \
  { \
    SND_DEFINE(Name, C_TO_SMALL_XEN_INT(Value)); \
    scm_set_object_property_x(C_STRING_TO_XEN_SYMBOL(Name), local_doc, C_TO_XEN_STRING(Help)); \
  }

#if HAVE_SCM_C_DEFINE
  #define XEN_DEFINE_VARIABLE(Name, Var, Value) Var = scm_permanent_object(scm_c_define(Name, Value))
#else
  #define XEN_DEFINE_VARIABLE(Name, Var, Value) Var = gh_define(Name, Value)
#endif

/* XEN_DEFINE_PROCEDURE sets the documentation property of procedure Func to Doc
 * XEN_DEFINE_CONSTANT sets the symbol's documentation property (gh_define returned the value in older version of Guile) 
 */

#define XEN_ERROR(Type, Info)         scm_throw(Type, Info)

#ifndef SCM_BOOLP
  #define SCM_BOOLP(Arg)          gh_boolean_p(Arg)
  /* the next exist in 1.3.4 but are not usable in this context (need SCM_NIMP check) */
  #undef SCM_STRINGP
  #undef SCM_VECTORP
  #undef SCM_SYMBOLP
  #define SCM_STRINGP(Arg)        gh_string_p(Arg)
  #define SCM_VECTORP(Arg)        gh_vector_p(Arg)
  #define SCM_SYMBOLP(Arg)        gh_symbol_p(Arg)
#endif

#ifndef SCM_VECTOR_LENGTH
  #define SCM_VECTOR_LENGTH(Arg)  ((int)(gh_vector_length(Arg)))
#endif
#define XEN_VECTOR_ELEMENTS(a)       SCM_VELTS(a)


#define XEN_BOOLEAN_P(Arg)            (SCM_BOOLP(Arg))
#define XEN_NUMBER_P(Arg)             (XEN_NOT_FALSE_P(scm_real_p(Arg)))
#define XEN_INTEGER_P(Arg)            (XEN_NOT_FALSE_P(scm_integer_p(Arg)))
#define XEN_SYMBOL_P(Arg)             (SCM_SYMBOLP(Arg))
#define XEN_PROCEDURE_P(Arg)          (XEN_NOT_FALSE_P(scm_procedure_p(Arg)))
#define XEN_STRING_P(Arg)             (SCM_STRINGP(Arg))
#define XEN_VECTOR_P(Arg)             (SCM_VECTORP(Arg))
#define XEN_LIST_P(Arg)               (scm_ilength(Arg) >= 0)
#define XEN_LIST_P_WITH_LENGTH(Arg, Len) ((Len = ((int)scm_ilength(Arg))) >= 0)
#define XEN_LIST_LENGTH(Arg)          ((int)(scm_ilength(Arg)))
#define XEN_LIST_REF(Lst, Num)        scm_list_ref(Lst, C_TO_SMALL_XEN_INT(Num))
#define XEN_CONS(Arg1, Arg2)          scm_cons(Arg1, Arg2)
#define XEN_CONS_2(Arg1, Arg2, Arg3)   scm_cons2(Arg1, Arg2, Arg3)
#define XEN_VECTOR_LENGTH(Arg)        ((int)(SCM_VECTOR_LENGTH(Arg)))
#define XEN_VECTOR_REF(Vect, Num)     scm_vector_ref(Vect, C_TO_XEN_INT(Num))
#define XEN_VECTOR_SET(Vect, Num, Val) scm_vector_set_x(Vect, C_TO_XEN_INT(Num), Val)
#define XEN_VECTOR_TO_LIST(Vect)      scm_vector_to_list(Vect)
#define XEN_REVERSE_LIST(Lst)         scm_reverse(Lst)
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
#define XEN_ARITY(Func)               scm_i_procedure_arity(Func)
#define XEN_KEYWORD_P(Obj)            (SCM_KEYWORDP(Obj))
#define XEN_MAKE_KEYWORD(Arg)         scm_c_make_keyword(Arg)
#define XEN_YES_WE_HAVE(Feature)      scm_add_feature(Feature)
#define XEN_DOCUMENTATION_SYMBOL      scm_string_to_symbol(C_TO_XEN_STRING("documentation"))
#define XEN_PROTECT_FROM_GC(Obj)      scm_permanent_object(Obj)
#define XEN_LOAD_FILE(File)           scm_primitive_load(C_TO_XEN_STRING(File))
#define XEN_DEFINE_HOOK(Var, Name, Arity, Help, Doc) Var = snd_create_hook(Name, Arity, Help, Doc)
#define XEN_CLEAR_HOOK(Arg)           scm_reset_hook_x(Arg)
#define XEN_HOOKED(a)                 (XEN_NOT_NULL_P(SCM_HOOK_PROCEDURES(a)))
#define XEN_HOOK_PROCEDURES(a)        SCM_HOOK_PROCEDURES(a)

#define XEN_TO_STRING(Obj)            scm_object_to_string(Obj, XEN_UNDEFINED)

#ifdef SCM_ASSERT_TYPE
  #define XEN_ASSERT_TYPE(Assertion, Arg, Position, Caller, Correct_Type) \
    SCM_ASSERT_TYPE(Assertion, Arg, Position, Caller, Correct_Type)

  #define XEN_WRONG_TYPE_ARG_ERROR(Caller, ArgN, Arg, Descr) \
    scm_wrong_type_arg_msg(Caller, ArgN, Arg, Descr)
#else
  #define XEN_ASSERT_TYPE(Assertion, Arg, Position, Caller, Correct_Type) \
    SCM_ASSERT(Assertion, Arg, Position, Caller)

  #define XEN_WRONG_TYPE_ARG_ERROR(Caller, ArgN, Arg, Descr) \
    scm_wrong_type_arg(Caller, ArgN, Arg)
#endif

#if USE_SND
  #define XEN_CALL_0(Func, Caller)                   g_call0(Func, Caller)
  #define XEN_CALL_1(Func, Arg1, Caller)             g_call1(Func, Arg1, Caller)
  #define XEN_CALL_2(Func, Arg1, Arg2, Caller)       g_call2(Func, Arg1, Arg2, Caller)
  #define XEN_CALL_3(Func, Arg1, Arg2, Arg3, Caller) g_call3(Func, Arg1, Arg2, Arg3, Caller)
  #define XEN_APPLY(Func, Args, Caller)              g_call_any(Func, Args, Caller)
#else
  #define XEN_CALL_0(Func, Caller)                   scm_apply(Func, XEN_EMPTY_LIST, XEN_EMPTY_LIST)
  #define XEN_CALL_1(Func, Arg1, Caller)             scm_apply(Func, Arg1, scm_listofnull)
  #define XEN_CALL_2(Func, Arg1, Arg2, Caller)       scm_apply(Func, Arg1, scm_cons(Arg2, scm_listofnull))
  #define XEN_CALL_3(Func, Arg1, Arg2, Arg3, Caller) scm_apply(Func, Arg1, scm_cons2(Arg2, Arg3, scm_listofnull))
  #define XEN_APPLY(Func, Args, Caller)              scm_apply(Func, Args, XEN_EMPTY_LIST)
#endif

#define XEN_APPLY_ARG_LIST_END                     scm_listofnull
#define XEN_WRITE_STRING(Str, Prt)        scm_puts(Str, Prt)

#if HAVE_SCM_LIST_N
  #define XEN_LIST_1(a)                   scm_list_1(a)
  #define XEN_LIST_2(a, b)                scm_list_2(a, b)
  #define XEN_LIST_3(a, b, c)             scm_list_3(a, b, c)
  #define XEN_LIST_4(a, b, c, d)          scm_list_4(a, b, c, d)
  #define XEN_LIST_5(a, b, c, d, e)       scm_list_5(a, b, c, d, e)
  #define XEN_LIST_6(a, b, c, d, e, f)    scm_list_n(a, b, c, d, e, f, XEN_UNDEFINED)
  #define XEN_LIST_7(a, b, c, d, e, f, g) scm_list_n(a, b, c, d, e, f, g, XEN_UNDEFINED)
#else
  #define XEN_LIST_1(a)                   SCM_LIST1(a)
  #define XEN_LIST_2(a, b)                SCM_LIST2(a, b)
  #define XEN_LIST_3(a, b, c)             SCM_LIST3(a, b, c)
  #define XEN_LIST_4(a, b, c, d)          SCM_LIST4(a, b, c, d)
  #define XEN_LIST_5(a, b, c, d, e)       SCM_LIST5(a, b, c, d, e)
  #define XEN_LIST_6(a, b, c, d, e, f)    SCM_LIST6(a, b, c, d, e, f)
  #define XEN_LIST_7(a, b, c, d, e, f, g) SCM_LIST7(a, b, c, d, e, f, g)
#endif
#endif
/* end GUILE */


#if HAVE_RUBY
/* ------------------------------ RUBY ------------------------------ */

#define XEN_OK 1

/* this basically is working but needs:
 *   wrapped ptrs -> objects
 *     keywords? 
 *   untested: lookup/gc protection/error handling
 *   state savers other than snd-main need ruby syntax
 */

#define XEN                  VALUE
#define XEN_FALSE            Qfalse
#define XEN_TRUE             Qtrue
#define XEN_EMPTY_LIST       Qnil
#define XEN_UNDEFINED        Qundef

#define XEN_FILE_EXTENSION  "rb"

#define scm_catch_body_t     void *
#define scm_catch_handler_t  void *
#define scm_print_state      int
#define XEN_FREE_OBJECT_TYPE void *
#define XEN_MARK_OBJECT_TYPE void *

#define scm_apply(a, b, c)                                                      Qnil
XEN xen_return_first(XEN a, ...);
#define scm_must_malloc(size, str) malloc(size)
#define scm_done_malloc(size) 

#define XEN_EQ_P(a, b)                  ((a) == (b))
#define XEN_EMPTY_LIST                  Qnil
#define XEN_LIST_1(a)                   rb_ary_new3(1, a)
#define XEN_LIST_2(a, b)                rb_ary_new3(2, a, b) 
#define XEN_LIST_3(a, b, c)             rb_ary_new3(3, a, b, c) 
#define XEN_LIST_4(a, b, c, d)          rb_ary_new3(4, a, b, c, d) 
#define XEN_LIST_5(a, b, c, d, e)       rb_ary_new3(5, a, b, c, d, e) 
#define XEN_LIST_6(a, b, c, d, e, f)    rb_ary_new3(6, a, b, c, d, e, f)
#define XEN_LIST_7(a, b, c, d, e, f, g) rb_ary_new3(7, a, b, c, d, e, f, g)
#define XEN_CAR(a)                      rb_ary_entry(a, 0)
#define XEN_CADR(a)                     rb_ary_entry(a, 1)
#define XEN_CADDR(a)                    rb_ary_entry(a, 2)
#define XEN_CADDDR(a)                   rb_ary_entry(a, 3)
#define XEN_CDR(a)                      snd_rb_cdr(a)
#define XEN_CDDR(a)                     XEN_CDR(XEN_CDR(a))


#define XEN_ONLY_ARG 1
#define XEN_ARG_1 1
#define XEN_ARG_2 2
#define XEN_ARG_3 3
#define XEN_ARG_4 4
#define XEN_ARG_5 5
#define XEN_ARG_6 6
#define XEN_ARG_7 7
#define XEN_ARG_8 8


#define XEN_VECTOR_ELEMENTS(a)              RARRAY(a)->ptr
#define XEN_HOOK_PROCEDURES(a)              a
#define XEN_VARIABLE_SET(a, b)              a = b
#define XEN_PROCEDURE_CAST

#define XEN_DEFINE_HOOK(Var, Name, Arity, Help, Doc) \
  { \
    Var = Qnil; \
    rb_define_variable(Scheme_global_variable_to_Ruby(Name), (VALUE *)(&Var)); \
  }

#define XEN_MAKE_AND_RETURN_OBJECT(Tag, Val, Mark, Free) return(Data_Wrap_Struct(Tag, Mark, Free, Val))
#define XEN_OBJECT_REF(a)                   DATA_PTR(a)
#define XEN_MAKE_OBJECT(a, b, c, Mark, Free) a = Data_Wrap_Struct(b, c, Mark, Free)
#define XEN_NAME_AS_C_STRING_TO_VALUE(a)                                                      Qnil
#define XEN_OBJECT_TYPE                    VALUE
#define XEN_OBJECT_TYPE_P(OBJ, TAG)       (XEN_BOUND_P(OBJ) && (rb_obj_is_instance_of(OBJ, TAG)))
#define XEN_TRUE_P(a)                     ((a) == Qtrue)
#define XEN_FALSE_P(a)                    ((a) == Qfalse)
#define XEN_NULL_P(a)                     ((a) == Qnil)
#define XEN_BOUND_P(Arg)                  ((Arg) != Qundef)
#define XEN_NOT_BOUND_P(Arg)              ((Arg) == Qundef)
#define XEN_ZERO                          INT2NUM(0)

#define XEN_TO_C_DOUBLE(a)                NUM2DBL(a)
#define XEN_TO_C_DOUBLE_OR_ELSE(a, b)     (XEN_NUMBER_P(a) ? NUM2DBL(a) : b)
#define XEN_TO_C_DOUBLE_WITH_CALLER(a, b) NUM2DBL(a)
#define C_TO_XEN_DOUBLE(a)                rb_float_new(a)
#define XEN_TO_C_INT(a)                   NUM2INT(a)
#define XEN_TO_C_INT_OR_ELSE(a, b)        (XEN_INTEGER_P(a) ? FIX2INT(a) : b)
#define XEN_TO_C_INT_OR_ELSE_WITH_CALLER(a, b, c) (XEN_INTEGER_P(a) ? FIX2INT(a) : b)
#define C_TO_XEN_INT(a)                   INT2NUM(a)
#define C_TO_SMALL_XEN_INT(a)             INT2FIX(a)
#define XEN_TO_SMALL_C_INT(a)             FIX2INT(a)
#define XEN_TO_C_UNSIGNED_LONG(a)         NUM2ULONG(a)
#define C_TO_XEN_UNSIGNED_LONG(a)         rb_int2inum(a)
#define C_TO_XEN_STRING(a)                rb_str_new2(a)
#define XEN_TO_C_STRING(Str)              RSTRING(Str)->ptr
#define XEN_TO_NEW_C_STRING(a)            copy_string(RSTRING(a)->ptr)
#define C_TO_XEN_BOOLEAN(a)               ((a) ? Qtrue : Qfalse)
#define XEN_TO_C_BOOLEAN_OR_TRUE(a)       ((XEN_FALSE_P(a) ? 0 : 1))
#define XEN_TO_C_BOOLEAN(a)               ((XEN_FALSE_P(a) ? 0 : 1))
#define C_STRING_TO_XEN_FORM(Str)         XEN_EVAL_C_STRING(Str)
#define XEN_EVAL_FORM(Form)               ((XEN)Form)
#define XEN_SYMBOL_TO_C_STRING(a)         rb_id2name(a)
#define C_STRING_TO_XEN_SYMBOL(a)         rb_intern(a)
#define XEN_WRAP_C_POINTER(a)             Data_Wrap_Struct(rb_cData, 0, 0, (void *)a)
#define XEN_UNWRAP_C_POINTER(a)           DATA_PTR(a)
#define XEN_WRAPPED_C_POINTER_P(a)        (TYPE(a) == T_DATA)

#define XEN_DEFINE_PROCEDURE(Name, Func, ReqArg, OptArg, RstArg, Doc) \
  rb_define_global_function(Scheme_procedure_to_Ruby(Name), Func, ((RstArg > 0) ? -2 : (OptArg > 0) ? -1 : ReqArg))

#define XEN_DEFINE_CONSTANT(a, b, c) \
  rb_define_global_const(Scheme_constant_to_Ruby(a), C_TO_XEN_INT(b))

#define XEN_DEFINE_VARIABLE(Name, Var, Value) \
  { \
    Var = Value; \
    rb_define_variable(Scheme_global_variable_to_Ruby(Name), (VALUE *)(&Var)); \
  }

#define XEN_BOOLEAN_P(Arg)               (XEN_TRUE_P(Arg) || XEN_FALSE_P(Arg))
#define XEN_NUMBER_P(Arg)                ((TYPE(Arg) == T_FLOAT) || (TYPE(Arg) == T_FIXNUM) || (TYPE(Arg) == T_BIGNUM))
#define XEN_INTEGER_P(Arg)               ((TYPE(Arg) == T_FIXNUM) || (TYPE(Arg) == T_BIGNUM))
#define XEN_SYMBOL_P(Arg)                SYMBOL_P(Arg)
#define XEN_STRING_P(Arg)                (TYPE(Arg) == T_STRING)
#define XEN_VECTOR_P(Arg)                (TYPE(Arg) == T_ARRAY)
#define XEN_LIST_P(Arg)                  (TYPE(Arg) == T_ARRAY)
#define XEN_LIST_P_WITH_LENGTH(Arg, Len) ((XEN_LIST_P(Arg)) ? (Len = RARRAY(Arg)->len) : 0)
#define XEN_LIST_LENGTH(Arg)             RARRAY(Arg)->len
#define XEN_VECTOR_LENGTH(Arg)           RARRAY(Arg)->len
#define XEN_PROCEDURE_P(Arg)             (XEN_BOUND_P(Arg) && (rb_obj_is_kind_of(Arg, rb_cProc)))
#define XEN_CONS(Arg1, Arg2)             rb_ary_new3(2, Arg1, Arg2)
#define XEN_CONS_2(Arg1, Arg2, Arg3)     rb_ary_new3(3, Arg1, Arg2, Arg3)
#define XEN_LIST_REF(Lst, Num)           rb_ary_entry(Lst, Num)
#define XEN_VECTOR_REF(Vect, Num)        rb_ary_entry(Vect, Num)
#define XEN_VECTOR_SET(a, b, c)          rb_ary_store(a, b, c)
#define XEN_MAKE_VECTOR(Num, Fill)       rb_ary_new2(Num)
#define XEN_EVAL_C_STRING(Arg)           rb_eval_string(Arg)
#define XEN_CHAR_P(Arg)                  0
#define XEN_TO_C_CHAR(Arg)               0

#define XEN_WRONG_TYPE_ARG_ERROR(Caller, ArgN, Arg, Descr) \
  fprintf(stderr, "%s: wrong type arg %d: %s", Caller, ArgN, XEN_TO_STRING(Arg))

#define XEN_CALL_0(Func, Caller)        rb_funcall(Func, rb_intern("call"), 0)
#define XEN_CALL_1(Func, Arg1, Caller)  rb_funcall(Func, rb_intern("call"), 1, Arg1)
#define XEN_CALL_2(Func, Arg1, Arg2, Caller) rb_funcall(Func, rb_intern("call"), 2, Arg1, Arg2)
#define XEN_CALL_3(Func, Arg1, Arg2, Arg3, Caller) rb_funcall(Func, rb_intern("call"), 3, Arg1, Arg2, Arg3)
#define XEN_APPLY(Func, Args, Caller)   rb_apply(Func, rb_intern("call"), Args)
#define XEN_APPLY_ARG_LIST_END          Qnil
#define XEN_ARITY(Func)                 rb_funcall(Func, rb_intern("arity"), 0)
#define XEN_KEYWORD_P(Obj)              (TYPE(Obj) == T_HASH)
#define XEN_MAKE_KEYWORD(Arg)                                                       Qnil
#define XEN_CLEAR_HOOK(a)               a = Qnil
#define XEN_HOOKED(a)                   a != Qnil
#define XEN_YES_WE_HAVE(a)              rb_provide(a)
#define XEN_DOCUMENTATION_SYMBOL        rb_intern("documentation")
#define XEN_VECTOR_TO_LIST(a)           a
#define XEN_REVERSE_LIST(a)             rb_ary_reverse(a)
#define XEN_TO_STRING(Obj)              rb_obj_as_string(Obj)

#define XEN_ASSERT_TYPE(Assertion, Arg, Position, Caller, Correct_Type) \
  if (!(Assertion)) fprintf(stderr,"%s: wrong type arg %d: %s (want %s)", Caller, Position, XEN_TO_STRING(Arg), Correct_Type)

#define XEN_PROTECT_FROM_GC(a)           a /* ? dialog-widgets */
#define XEN_LOAD_FILE(a)                 rb_load_file(a)
#define XEN_ERROR(Type, Info)           fprintf(stderr, "error ") /* ? */
#define XEN_WRITE_STRING(a, b)          fprintf(stdout, a) /* ? */


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
/* end RUBY */


/* ------------------------------ MZSCHEME ------------------------------ */

#if HAVE_MZSCHEME

#define XEN_OK 1

#define XEN                 Scheme_Object *
#define XEN_FALSE           scheme_false
#define XEN_TRUE            scheme_true
#define XEN_EMPTY_LIST      scheme_null
#define XEN_UNDEFINED       scheme_undefined

#define XEN_FILE_EXTENSION  "scm"

#ifdef __cplusplus
  #define XEN_PROCEDURE_CAST (XEN (*)())
  typedef XEN (*scm_catch_body_t) (void *data);
  typedef XEN (*scm_catch_handler_t) (void *data, XEN tag, XEN throw_args);
#else
  #define XEN_PROCEDURE_CAST
  #define scm_catch_body_t void *
  #define scm_catch_handler_t void *
#endif

#define scm_print_state int
#define XEN_FREE_OBJECT_TYPE         int
#define XEN_MARK_OBJECT_TYPE         XEN

#define scm_apply(a, b, c)
XEN xen_return_first(XEN a, ...);
#define scm_must_malloc(size, str) malloc(size)
#define scm_done_malloc(size) 

#define XEN_EQ_P(a, b)                   scheme_eq(a, b)
#define XEN_EMPTY_LIST                   scheme_null
#define XEN_LIST_1(a)                    scheme_make_pair(a, scheme_null)
#define XEN_LIST_2(a, b)                 scheme_make_pair(a, XEN_LIST_1(b))
#define XEN_LIST_3(a, b, c)              scheme_make_pair(a, XEN_LIST_2(b, c))
#define XEN_LIST_4(a, b, c, d)           scheme_make_pair(a, XEN_LIST_3(b, c, d))
#define XEN_LIST_5(a, b, c, d, e)        scheme_make_pair(a, XEN_LIST_4(b, c, d, e))
#define XEN_LIST_6(a, b, c, d, e, f)     scheme_make_pair(a, XEN_LIST_5(b, c, d, e, f))
#define XEN_LIST_7(a, b, c, d, e, f, g)  scheme_make_pair(a, XEN_LIST_6(b, c, d, e, f, g))
#define XEN_CAR(a)                       SCHEME_CAR(a)
#define XEN_CADR(a)                      SCHEME_CADR(a)
#define XEN_CADDR(a)                     SCHEME_CAR(SCHEME_CDDR(a))
#define XEN_CADDDR(a)                    SCHEME_CAR(SCHEME_CDR(SCHEME_CDDR(a)))
#define XEN_CDR(a)                       SCHEME_CDR(a)
#define XEN_CDDR(a)                      SCHEME_CDDR(a)
#define XEN_VECTOR_ELEMENTS(a)           SCHEME_VEC_ELS(a)
#define XEN_MAKE_OBJECT(a, b, c, ig1, ig2)
#define XEN_HOOK_PROCEDURES(a)
#define XEN_VARIABLE_SET(a, b)

#define XEN_MAKE_AND_RETURN_OBJECT(Tag, Val, ig1, ig2) 0
#define XEN_OBJECT_REF(a) 0
#define XEN_NAME_AS_C_STRING_TO_VALUE(a) 0
#define XEN_OBJECT_TYPE int
#define XEN_OBJECT_TYPE_P(OBJ, TAG) 0

#define XEN_TRUE_P(a)                            scheme_eq(a, XEN_TRUE)
#define XEN_FALSE_P(a)                           SCHEME_FALSEP(a)
#define XEN_NULL_P(a)                            SCHEME_NULLP(a)
#define XEN_BOUND_P(Arg)                         (!(XEN_EQ_P(Arg, XEN_UNDEFINED)))
#define XEN_NOT_BOUND_P(Arg)                     (XEN_EQ_P(Arg, XEN_UNDEFINED))
#define XEN_ZERO                                 scheme_make_integer_value(0)

#define XEN_TO_C_DOUBLE(a)                       SCHEME_DBL_VAL(a)
#define XEN_TO_C_DOUBLE_OR_ELSE(a, b)            SCHEME_DBL_VAL(a)
#define XEN_TO_C_DOUBLE_WITH_CALLER(a, b)        SCHEME_DBL_VAL(a)
#define XEN_TO_C_INT(a)                          SCHEME_INT_VAL(a)
#define XEN_TO_C_INT_OR_ELSE(a, b)               (SCHEME_INTP(a) ? SCHEME_INT_VAL(a) : (b))
#define XEN_TO_C_INT_OR_ELSE_WITH_CALLER(a, b, c) (SCHEME_INTP(a) ? SCHEME_INT_VAL(a) : (b))
#define XEN_TO_C_STRING(STR)                     SCHEME_STR_VAL(STR)
#define C_TO_XEN_DOUBLE(a)                       scheme_make_double(a)
#define C_TO_XEN_INT(a)                          scheme_make_integer_value((long)a)
#define C_TO_SMALL_XEN_INT(a)                    scheme_make_integer((long)a)
#define XEN_TO_SMALL_C_INT(a)                    SCHEME_INT_VAL(a)
#define XEN_TO_C_UNSIGNED_LONG(a)                scheme_get_int_val(a)
#define C_TO_XEN_UNSIGNED_LONG(a)                XEN_FALSE
#define C_TO_XEN_STRING(a)                       scheme_make_string(a)
#define XEN_TO_NEW_C_STRING(a)                   strdup(SCHEME_STR_VAL(a))
#define C_TO_XEN_BOOLEAN(a)                      ((a) ? scheme_true : scheme_false)
#define C_STRING_TO_XEN_SYMBOL(a)                scheme_intern_symbol(a)
#define XEN_TO_C_BOOLEAN_OR_TRUE(a)              (XEN_BOOLEAN_P(a) ? (XEN_TRUE_P(a)) : 1)
#define XEN_TO_C_BOOLEAN(a)                      ((XEN_FALSE_P(a)) ? 0 : 1)

#define C_STRING_TO_XEN_FORM(Str)                XEN_FALSE
#define XEN_EVAL_FORM(Form)                      XEN_FALSE

#define XEN_SYMBOL_TO_C_STRING(a)                SCHEME_SYM_VAL(a)
#define XEN_WRAP_C_POINTER(a)                    scheme_make_integer_value_from_unsigned((unsigned long)a)
#define XEN_UNWRAP_C_POINTER(a) 0
#define XEN_WRAPPED_C_POINTER_P(a) 0
#define XEN_HOOKED(a) 0
#define XEN_DEFINE_PROCEDURE(Name, Func, ReqArg, OptArg, RstArg, Doc)
#define XEN_DEFINE_CONSTANT(a, b, c)
#define XEN_DEFINE_VARIABLE(a, b, c)

#define XEN_BOOLEAN_P(Arg)                 SCHEME_BOOLP(Arg)
#define XEN_NUMBER_P(Arg)                  (SCHEME_REALP(Arg) && (!SCHEME_COMPLEXP(Arg)))
#define XEN_INTEGER_P(Arg)                 SCHEME_INTP(Arg)
#define XEN_SYMBOL_P(Arg)                  SCHEME_SYMBOLP(Arg)
#define XEN_STRING_P(Arg)                  SCHEME_STRINGP(Arg)
#define XEN_VECTOR_P(Arg)                  SCHEME_VECTORP(Arg)
#define XEN_LIST_P(Arg)                    SCHEME_LISTP(Arg)
#define XEN_LIST_P_WITH_LENGTH(Arg, Len)   ((SCHEME_LISTP(Arg)) && (Len = scheme_list_length(Arg)))
#define XEN_LIST_LENGTH(Arg)               scheme_list_length(Arg)
#define XEN_VECTOR_LENGTH(Arg)             SCHEME_VEC_SIZE(Arg)
#define XEN_PROCEDURE_P(Arg)               SCHEME_PROCP(Arg)
#define XEN_CONS(Arg1, Arg2)               scheme_make_pair(Arg1, Arg2)
#define XEN_CONS_2(Arg1, Arg2, Arg3)       scheme_make_pair(Arg1, scheme_make_pair(Arg2, Arg3))
#define XEN_LIST_REF(Lst, Num) XEN_ZERO
#define XEN_VECTOR_REF(Vect, Num)          SCHEME_VEC_ELS(a)[Num]
#define XEN_VECTOR_SET(a, b, c)            SCHEME_VEC_ELS(a)[b] = c
#define XEN_EVAL_C_STRING(Arg)             scheme_eval_string(Arg, get_global_env())
#define XEN_MAKE_VECTOR(Num, Fill)         scheme_make_vector(Num, Fill)
#define XEN_VECTOR_TO_LIST(Vect)           scheme_vector_to_list(Vect)
#define XEN_DEFINE_HOOK(Var, Name, Arity, Help, Doc)
#define XEN_CLEAR_HOOK(Arg)
#define XEN_CHAR_P(Arg)                     SCHEME_CHARP(Arg)
#define XEN_TO_C_CHAR(Arg)                  SCHEME_CHAR_VAL(Arg)
#define XEN_CALL_0(Func, Caller) 0
#define XEN_CALL_1(Func, Arg1, Caller) 0
#define XEN_CALL_2(Func, Arg1, Arg2, Caller) 0
#define XEN_CALL_3(Func, Arg1, Arg2, Arg3, Caller) 0
#define XEN_APPLY(Func, Args, Caller) 0
#define XEN_APPLY_ARG_LIST_END scheme_null
#define XEN_ARITY(Func) 0
#define XEN_KEYWORD_P(Obj) 0
#define XEN_MAKE_KEYWORD(Arg) 0
#define XEN_YES_WE_HAVE(Feature)
#define XEN_DOCUMENTATION_SYMBOL 0
#define XEN_ASSERT_TYPE(Assertion, Arg, Position, Caller, Correct_Type)
#define XEN_REVERSE_LIST(a) a
#define XEN_PROTECT_FROM_GC(a) a
#define XEN_LOAD_FILE(a)                  scheme_load(a)
#define XEN_ERROR(Type, Info)             scheme_signal_error(Info)
#define XEN_WRITE_STRING(a, b)            scheme_write_string(a, strlen(a), b)

#define XEN_WRONG_TYPE_ARG_ERROR(Caller, ArgN, Arg, Descr) \
  fprintf(stderr, "%s: wrong type arg %d: %s", Caller, ArgN, XEN_TO_STRING(Arg))

#endif
/* end MZSCHEME */


/* ------------------------------ LIBREP ------------------------------ */

#if HAVE_LIBREP

#define XEN_OK 1

/* Librep versions of the various extension-language entities used in Snd */
/*   this project is currently on hold; I'm finding it very difficult to
 *   debug Librep/Snd problems.  The librep_* code in snd-scm.c does work.
 */

/* these are the left-over scm/gh entities */
#define XEN            repv
#define XEN_FALSE      rep_FALSE
#define XEN_TRUE       rep_TRUE
#define XEN_EMPTY_LIST rep_NULL
#define XEN_UNDEFINED  rep_NULL

#define XEN_FILE_EXTENSION  "jl"

#define scm_catch_body_t void *
#define scm_catch_handler_t void *
#define scm_print_state int
#define XEN_FREE_OBJECT_TYPE         int
#define XEN_MARK_OBJECT_TYPE         XEN

#define scm_apply(a, b, c) rep_apply(a, b)
XEN xen_return_first(XEN a, ...);
#define scm_must_malloc(size, str) malloc(size)
#define scm_done_malloc(size) 

#define XEN_EQ_P(a, b)                           ((a) == (b))
#define XEN_EMPTY_LIST                           Qnil
#define XEN_LIST_1(Arg1)                         rep_LIST_1(Arg1)
#define XEN_LIST_2(Arg1, Arg2)                   rep_LIST_2(Arg1, Arg2)
#define XEN_LIST_3(Arg1, Arg2, Arg3)             rep_LIST_3(Arg1, Arg2, Arg3)
#define XEN_LIST_4(Arg1, Arg2, Arg3, Arg4)       rep_LIST_4(Arg1, Arg2, Arg3, Arg4)
#define XEN_LIST_5(Arg1, Arg2, Arg3, Arg4, Arg5) rep_LIST_5(Arg1, Arg2, Arg3, Arg4, Arg5)
#define XEN_LIST_6(a, b, c, d, e, f)             Fcons(a, XEN_LIST_5(b, c, d, e, f))
#define XEN_LIST_7(a, b, c, d, e, f, g)          Fcons(a, XEN_LIST_6(b, c, d, e, f, g))
#define XEN_CAR(Arg)                             rep_CAR(Arg)
#define XEN_CDR(Arg)                             rep_CDR(Arg)
#define XEN_CDDR(Arg)                            rep_CDDR(Arg)
#define XEN_CADR(Arg)                            rep_CADR(Arg)
#define XEN_CADDR(Arg)                           rep_CADDR(Arg)
#define XEN_CADDDR(Arg)                          rep_CADDDR(Arg)
#define XEN_VECTOR_ELEMENTS(a)                   (repv *)(rep_VECT(a)->array)
#define XEN_MAKE_OBJECT(a, b, c, ig1, ig2) Qnil
#define XEN_HOOK_PROCEDURES(a) Qnil
#define SET_OBJECT_REF(a, b)

#ifdef __cplusplus
  #define XEN_PROCEDURE_CAST            (repv(*)())
#else
  #define XEN_PROCEDURE_CAST
#endif

#define XEN_DEFINE_HOOK(Var, Name, Arity, Help, Doc)
#define XEN_MAKE_AND_RETURN_OBJECT(Tag, Val, ig1, ig2) Qnil
#define XEN_OBJECT_REF(a)                Qnil
#define XEN_NAME_AS_C_STRING_TO_VALUE(a) Qnil
#define XEN_OBJECT_TYPE                  int
#define XEN_OBJECT_TYPE_P(OBJ, TAG)      Qnil
#define XEN_TRUE_P(a)                    ((a) == Qt)
#define XEN_FALSE_P(a)                   ((a) == Qnil)
#define XEN_NULL_P(a)                    (((a) == Qnil) || (XEN_LIST_LENGTH(a) == 0))
#define XEN_BOUND_P(Arg)                 ((Arg) && (Fboundp(Arg)))
#define XEN_NOT_BOUND_P(Arg)             (!Fboundp(Arg))
#define XEN_ZERO                         rep_MAKE_INT(0)

#define XEN_TO_C_DOUBLE(a)               rep_get_float(a)
#define XEN_TO_C_DOUBLE_OR_ELSE(a, b)    (XEN_NUMBER_P(a) ? rep_get_float(a) : b)
#define XEN_TO_C_DOUBLE_WITH_CALLER(a, b) rep_get_float(a)
#define XEN_TO_C_INT(Arg)                rep_INT(Arg)
#define XEN_TO_C_INT_OR_ELSE(a, b)       (XEN_INTEGER_P(a) ? rep_INT(a) : b)
#define XEN_TO_C_INT_OR_ELSE_WITH_CALLER(a, b, c) (XEN_INTEGER_P(a) ? rep_INT(a) : b)
#define XEN_TO_C_STRING(Str)             ((char *)(rep_STR(Str)))
#define C_TO_XEN_DOUBLE(a)               rep_make_float(a, rep_FALSE)
#define C_TO_XEN_INT(Arg)                rep_MAKE_INT(Arg)
#define C_TO_SMALL_XEN_INT(a)            rep_MAKE_INT(a)
#define XEN_TO_SMALL_C_INT(a)            rep_INT(a)
#define XEN_TO_C_UNSIGNED_LONG(a)        rep_get_long_uint(a)
#define C_TO_XEN_UNSIGNED_LONG(a)        Qnil
#define C_TO_XEN_STRING(a)               rep_string_dupn(a, strlen(a))
#define XEN_TO_NEW_C_STRING(a)           copy_string(XEN_TO_C_STRING(a))
#define C_TO_XEN_BOOLEAN(a)              ((a) ? Qt : Qnil)
#define C_STRING_TO_XEN_SYMBOL(a)        Fintern_symbol(Fmake_symbol(rep_string_dupn(a, strlen(a))), Qnil)
#define XEN_TO_C_BOOLEAN_OR_TRUE(a)      (((a) == Qnil) ? 0 : 1)
#define XEN_TO_C_BOOLEAN(a)              (((a) == Qnil) ? 0 : 1)

#define C_STRING_TO_XEN_FORM(Str)        Qnil
#define XEN_EVAL_FORM(Form)              Qnil

#define XEN_SYMBOL_TO_C_STRING(a)        rep_STR(rep_SYM(a)->name)
#define XEN_WRAP_C_POINTER(a)            rep_make_long_uint((unsigned long)a)
#define XEN_UNWRAP_C_POINTER(a)          rep_get_long_uint(a)
#define XEN_WRAPPED_C_POINTER_P(a)       (a)
#define XEN_HOOKED(a)                    ((a) && ((a) != Qnil))

#define XEN_DEFINE_PROCEDURE(Name, Func, ReqArg, OptArg, RstArg, Doc) \
  librep_new_procedure(Name, Func, ReqArg, OptArg, RstArg, Doc);

#define XEN_DEFINE_CONSTANT(Name, Value, Documentation) \
  librep_new_variable(Name, Value, Documentation);

#define XEN_DEFINE_VARIABLE(Name, Value, Documentation) \
  librep_new_variable(Name, Value, Documentation);

#define XEN_ERROR(a, b)               Fthrow(a, b)

#define XEN_BOOLEAN_P(Arg)            (((Arg) == Qt) || ((Arg) == Qnil))
#define XEN_NUMBER_P(Arg)             ((Arg) && (rep_NUMBERP(Arg)))
#define XEN_INTEGER_P(Arg)            ((Arg) && (rep_INTEGERP(Arg)))
#define XEN_SYMBOL_P(Arg)             ((Arg) && (rep_SYMBOLP(Arg)))
#define XEN_STRING_P(Arg)             ((Arg) && (rep_STRINGP(Arg)))
#define XEN_VECTOR_P(Arg)             ((Arg) && (rep_VECTORP(Arg)))
#define XEN_LIST_P(Arg)               ((Arg) && (rep_LISTP(Arg)))
#define XEN_LIST_P_WITH_LENGTH(Arg, Len) ((Arg) && (XEN_LIST_P(Arg)) && ((Len = XEN_LIST_LENGTH(Arg)) >= 0))
#define XEN_LIST_LENGTH(Arg)          rep_list_length(Arg)
#define XEN_VECTOR_LENGTH(Arg)        rep_VECT_LEN(Arg)
#define XEN_PROCEDURE_P(Arg)          ((Arg) && ((Ffunctionp(Arg)) || (Fsubrp(Arg)) || (Fclosurep(Arg))))
#define XEN_CONS(Arg1, Arg2)          Fcons(Arg1, Arg2)
#define XEN_CONS_2(Arg1, Arg2, Arg3)  Fcons(Arg1, Fcons(Arg2, Arg3))
#define XEN_LIST_REF(Lst, Num)        Fnth(Num, Lst)
#define XEN_VECTOR_REF(Vect, Num)     Faref(Vect, Num)
#define XEN_VECTOR_SET(Vect, Num, Val) Faset(Vect, Num, Val) 
#define XEN_EVAL_C_STRING(Arg)        librep_eval_string(Arg)
#define XEN_MAKE_VECTOR(Num, Fill)    rep_make_vector(Num)
#define XEN_VECTOR_TO_LIST(a)         Qnil
#define XEN_CHAR_P(Arg)               ((Arg) && (XEN_STRING_P(Arg)))
#define XEN_TO_C_CHAR(Arg)            (XEN_TO_C_STRING(Arg))[0]
#define XEN_CALL_0(Func, Caller)       rep_call_lisp0(Func)
#define XEN_CALL_1(Func, Arg1, Caller) rep_call_lisp1(Func, Arg1)
#define XEN_CALL_2(Func, Arg1, Arg2, Caller) rep_call_lisp2(Func, Arg1, Arg2)
#define XEN_CALL_3(Func, Arg1, Arg2, Arg3, Caller) rep_call_lisp3(Func, Arg1, Arg2, Arg3)
#define XEN_APPLY(Func, Args, Caller) rep_apply(Func, Args)
#define XEN_APPLY_ARG_LIST_END        Qnil
#define XEN_ARITY(Func)               Qnil
#define XEN_KEYWORD_P(Obj)            ((Obj) && (rep_KEYWORDP(Obj)))
#define XEN_MAKE_KEYWORD(Arg)         Fmake_keyword(C_STRING_TO_XEN_SYMBOL(Arg))
#define XEN_CLEAR_HOOK(a)
#define XEN_YES_WE_HAVE(a)            Fprovide(C_STRING_TO_XEN_SYMBOL(a))
#define XEN_DOCUMENTATION_SYMBOL      Qnil
#define XEN_ASSERT_TYPE(Assertion, Arg, Position, Caller, Correct_Type)
#define XEN_REVERSE_LIST(a)           Freverse(a)
#define XEN_PROTECT_FROM_GC(a)        a
#define XEN_LOAD_FILE(a)              Fload(C_TO_XEN_STRING(a), Qnil, Qnil, Qnil, Qnil)
#define XEN_WRITE_STRING(a, b)        rep_princ_val(Qstandard_output, C_TO_XEN_STRING(a))

#define XEN_WRONG_TYPE_ARG_ERROR(Caller, ArgN, Arg, Descr) \
  fprintf(stderr, "%s: wrong type arg %d: %s", Caller, ArgN, XEN_TO_STRING(Arg))

#endif
/* end LIBREP */


/* ------------------------------ NO EXTENSION LANGUAGE ------------------------------ */

#ifndef XEN_OK

#if LONG_INT_P
  #define XEN long
#else
  #define XEN int
#endif
#define XEN_FALSE 0
#define XEN_TRUE 1
#define XEN_EMPTY_LIST 0
#define XEN_UNDEFINED 0

#define XEN_FILE_EXTENSION  "txt"

#ifdef __cplusplus
  #define XEN_PROCEDURE_CAST (XEN (*)())
  typedef XEN (*scm_catch_body_t) (void *data);
  typedef XEN (*scm_catch_handler_t) (void *data, XEN tag, XEN throw_args);
#else
  #define XEN_PROCEDURE_CAST
  #define scm_catch_body_t void *
  #define scm_catch_handler_t void *
#endif

#define scm_print_state int

#define XEN_FREE_OBJECT_TYPE         int
#define XEN_MARK_OBJECT_TYPE         XEN

#define scm_apply(a, b, c) 0
XEN xen_return_first(XEN a, ...);
#define scm_must_malloc(size, str) malloc(size)
#define scm_done_malloc(size) 

#define XEN_EQ_P(a, b) 0
#define XEN_EMPTY_LIST 0
#define XEN_LIST_1(a) 0
#define XEN_LIST_2(a, b) 0
#define XEN_LIST_3(a, b, c) 0
#define XEN_LIST_4(a, b, c, d) 0
#define XEN_LIST_5(a, b, c, d, e) 0
#define XEN_LIST_6(a, b, c, d, e, f) 0
#define XEN_LIST_7(a, b, c, d, e, f, g) 0
#define XEN_CAR(a) 0
#define XEN_CADR(a) 0
#define XEN_CADDR(a) 0
#define XEN_CADDDR(a) 0
#define XEN_CDR(a) 0
#define XEN_CDDR(a) 0
#define XEN_VECTOR_ELEMENTS(a) ((XEN *)a)
#define XEN_MAKE_OBJECT(a, b, c, ig1, ig2)
#define XEN_HOOK_PROCEDURES(a) 0
#define XEN_VARIABLE_SET(a, b)
#define VARIABLE_REF(a) 0

/* this is the sg.h replacement */

#define XEN_MAKE_AND_RETURN_OBJECT(Tag, Val, ig1, ig2) return(0)
#define XEN_OBJECT_REF(a) 0
#define XEN_NAME_AS_C_STRING_TO_VALUE(a) 0
#define XEN_OBJECT_TYPE int
#define XEN_OBJECT_TYPE_P(OBJ, TAG) 0
#define XEN_TRUE_P(a) 0
#define XEN_FALSE_P(a) 0
#define XEN_NULL_P(a) 0
#define XEN_BOUND_P(Arg) 0
#define XEN_NOT_BOUND_P(Arg) 1
#define XEN_ZERO 0

#define XEN_TO_C_DOUBLE(a) 0.0
#define XEN_TO_C_DOUBLE_OR_ELSE(a, b) 0.0
#define XEN_TO_C_DOUBLE_WITH_CALLER(a, b) 0.0
#define XEN_TO_C_INT(a) 0
#define XEN_TO_C_INT_OR_ELSE(a, b) 0
#define XEN_TO_C_INT_OR_ELSE_WITH_CALLER(a, b, c) 0
#define XEN_TO_C_STRING(STR) NULL
#define C_TO_XEN_DOUBLE(a) 0
#define C_TO_XEN_INT(a) a
#define C_TO_SMALL_XEN_INT(a) a
#define XEN_TO_SMALL_C_INT(a) 0
#define XEN_TO_C_UNSIGNED_LONG(a) 0
#define C_TO_XEN_UNSIGNED_LONG(a) 0
#define C_TO_XEN_STRING(a) 0
#define XEN_TO_NEW_C_STRING(a) NULL
#define C_TO_XEN_BOOLEAN(a) 0
#define C_STRING_TO_XEN_SYMBOL(a) 0
#define XEN_TO_C_BOOLEAN_OR_TRUE(a) 0
#define XEN_TO_C_BOOLEAN(a) 0
#define C_STRING_TO_XEN_FORM(Str) 0
#define XEN_EVAL_FORM(Form) 0
#define XEN_SYMBOL_TO_C_STRING(a) 0
#define XEN_WRAP_C_POINTER(a) 0
#define XEN_UNWRAP_C_POINTER(a) 0
#define XEN_WRAPPED_C_POINTER_P(a) 0
#define XEN_HOOKED(a) 0
#define XEN_DEFINE_PROCEDURE(Name, Func, ReqArg, OptArg, RstArg, Doc)
#define XEN_DEFINE_CONSTANT(a, b, c)
#define XEN_DEFINE_VARIABLE(a, b, c)
#define XEN_BOOLEAN_P(Arg) 0
#define XEN_NUMBER_P(Arg) 0
#define XEN_INTEGER_P(Arg) 0
#define XEN_SYMBOL_P(Arg) 0
#define XEN_STRING_P(Arg) 0
#define XEN_VECTOR_P(Arg) 0
#define XEN_LIST_P(Arg) 0
#define XEN_LIST_P_WITH_LENGTH(Arg, Len) 0
#define XEN_LIST_LENGTH(Arg) 0
#define XEN_VECTOR_LENGTH(Arg) 0
#define XEN_PROCEDURE_P(Arg) 0
#define XEN_CONS(Arg1, Arg2) 0
#define XEN_CONS_2(Arg1, Arg2, Arg3) 0
#define XEN_LIST_REF(Lst, Num) 0
#define XEN_VECTOR_REF(Vect, Num) 0
#define XEN_VECTOR_SET(a, b, c)
#define XEN_EVAL_C_STRING(Arg) 0
#define XEN_MAKE_VECTOR(Num, Fill) 0
#define XEN_VECTOR_TO_LIST(Vect) 0
#define XEN_DEFINE_HOOK(Var, Name, Arity, Help, Doc)
#define XEN_CLEAR_HOOK(Arg)
#define XEN_CHAR_P(Arg) 0
#define XEN_TO_C_CHAR(Arg) 0
#define XEN_CALL_0(Func, Caller) 0
#define XEN_CALL_1(Func, Arg1, Caller) 0
#define XEN_CALL_2(Func, Arg1, Arg2, Caller) 0
#define XEN_CALL_3(Func, Arg1, Arg2, Arg3, Caller) 0
#define XEN_APPLY(Func, Args, Caller) 0
#define XEN_APPLY_ARG_LIST_END 0
#define XEN_ARITY(Func) 0
#define XEN_KEYWORD_P(Obj) 0
#define XEN_MAKE_KEYWORD(Arg) 0
#define XEN_YES_WE_HAVE(Feature)
#define XEN_DOCUMENTATION_SYMBOL 0
#define XEN_ASSERT_TYPE(Assertion, Arg, Position, Caller, Correct_Type)
#define XEN_REVERSE_LIST(a) a
#define XEN_PROTECT_FROM_GC(a) 0
#define XEN_LOAD_FILE(a) 0
#define XEN_ERROR(Type, Info) fprintf(stderr, Info)
#define XEN_WRITE_STRING(a, b) fprintf(stdout, a)

#define XEN_WRONG_TYPE_ARG_ERROR(Caller, ArgN, Arg, Descr) \
  fprintf(stderr, "%s: wrong type arg %d: %s", Caller, ArgN, XEN_TO_STRING(Arg))

#endif
/* end NO EXTENSION LANGUAGE */


#endif
