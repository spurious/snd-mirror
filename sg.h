#ifndef SG_H
#define SG_H
/* macros useful in all the files using guile */

/* I'm slowly moving every gh_*, scm_* and SCM_* entity into this file,
 *   hoping eventually to be able to make a parallel header file that
 *   allows us to use librep, mzscheme, ruby etc as the extension language
 *   without any (or not many) internal code changes.
 *
 * Putting everything here also insulates the rest of the code from Guile name changes.
 */

#ifdef __cplusplus
  #define PROCEDURE (SCM (*)())
#else
  #define PROCEDURE
#endif

#ifndef __GNUC__
  #ifndef __FUNCTION__
    #define __FUNCTION__ ""
  #endif
#endif

#define TRUE_VALUE       SCM_BOOL_T
#define FALSE_VALUE      SCM_BOOL_F
#define EMPTY_LIST       SCM_EOL
#define UNDEFINED_VALUE  SCM_UNDEFINED
#define NEW_OBJECT       SCM_NEWSMOB
#define VARIABLE_REF     SCM_VARIABLE_REF

#ifndef SCM_EQ_P
  #define SCM_EQ_P(a, b) ((a) == (b))
  /* SCM_UNPACK used here in later Guile's */
#endif

#if (SCM_DEBUG_TYPING_STRICTNESS == 2)
  #define RETURN_NEW_OBJECT(Tag, Val) SCM_RETURN_NEWSMOB(Tag, Val)
#else
  #define RETURN_NEW_OBJECT(Tag, Val) SCM_RETURN_NEWSMOB(Tag, (SCM)Val)
#endif
#define OBJECT_REF(a) SCM_SMOB_DATA(a)
/* remember to check the smob type agreement before calling OBJECT_REF! */

#if HAVE_SCM_C_DEFINE
  #define SET_OBJECT_REF(Var, Val) SCM_VARIABLE_SET(Var, Val)
  #define SND_LOOKUP(a)            VARIABLE_REF(scm_sym2var(scm_str2symbol(a), scm_current_module_lookup_closure (), TRUE_VALUE))
  /* can't use scm_c_lookup here because it exits the goddamn program if the name is undefined */
#else
  #define SET_OBJECT_REF(Var, Val) SCM_SETCDR(Var, Val)
  #define SND_LOOKUP(a)            scm_symbol_value0(a)
#endif

#if HAVE_SCM_REMEMBER_UPTO_HERE
  #define TAG_TYPE                scm_bits_t
  #define SND_SMOB_TYPE(TAG, OBJ) SCM_SMOB_PREDICATE(TAG, OBJ)
#else
  #define TAG_TYPE                long
  #define SND_SMOB_TYPE(TAG, OBJ) (SCM_TYP16(OBJ) == (SCM)TAG)
#endif

#define OBJECT_TYPE_P(Obj, Type) ((SCM_NIMP(Obj)) && (SND_SMOB_TYPE(Type, Obj)))

#define TRUE_P(a)                SCM_EQ_P(a, TRUE_VALUE)
#define FALSE_P(a)               SCM_EQ_P(a, FALSE_VALUE)
#define NULL_P(a)                SCM_NULLP(a)
#define BOUND_P(Arg)             (!(SCM_UNBNDP(Arg)))
#define NOT_BOUND_P(Arg)         SCM_UNBNDP(Arg)
#define INTEGER_ZERO             SCM_INUM0

#define CAR(a)                   SCM_CAR(a)
#define CADR(a)                  SCM_CADR(a)
#define CADDR(a)                 SCM_CADDR(a)
#define CADDDR(a)                SCM_CADDDR(a)
#define CDR(a)                   SCM_CDR(a)
#define CDDR(a)                  SCM_CDDR(a)
#define VECTOR_ELEMENTS(a)       SCM_VELTS(a)
#define EQ_P(a, b)               SCM_EQ_P(a, b)
#define HOOK_PROCEDURES(a)       SCM_HOOK_PROCEDURES(a)

#define ARGn SCM_ARGn
#define ARG1 SCM_ARG1
#define ARG2 SCM_ARG2
#define ARG3 SCM_ARG3
#define ARG4 SCM_ARG4
#define ARG5 SCM_ARG5
#define ARG6 SCM_ARG6
#define ARG7 SCM_ARG7
#define ARG8 8


#define TO_C_DOUBLE(a)            scm_num2dbl(a,  __FUNCTION__)
#define TO_C_DOUBLE_OR_ELSE(a, b) ((NUMBER_P(a)) ? (scm_num2dbl(a,  __FUNCTION__)) : (b))
#define TO_C_DOUBLE_WITH_ORIGIN(a, b) scm_num2dbl(a, b)

#if HAVE_SCM_NUM2INT
  #define TO_C_INT(a)             ((int)scm_num2int(a, 0, __FUNCTION__))
#else
  #define TO_C_INT(a)             ((int)gh_scm2int(a))
#endif

#define TO_C_INT_OR_ELSE(a, b)    to_c_int_or_else(a, b, __FUNCTION__)
#define TO_C_INT_OR_ELSE_WITH_ORIGIN(a, b, c) to_c_int_or_else(a, b, c)

#ifndef SCM_STRING_CHARS
  #define TO_C_STRING(STR)        SCM_CHARS(STR)
#else
  #define TO_C_STRING(STR)        SCM_STRING_CHARS(STR)
  /* this assumes its argument is an SCM string and does not allocate new space */
#endif

#if HAVE_SCM_MAKE_REAL
  #define TO_SCM_DOUBLE(a)        scm_make_real(a)
#else
  #define TO_SCM_DOUBLE(a)        scm_makdbl(a, 0.0)
#endif

#define TO_SCM_INT(a)             scm_long2num((long)a)
#define TO_SMALL_SCM_INT(a)       SCM_MAKINUM(a)
#define TO_SMALL_C_INT(a)         ((int)(SCM_INUM(a)))
#define TO_C_UNSIGNED_LONG(a)     scm_num2ulong(a, 0, __FUNCTION__)
#define TO_SCM_UNSIGNED_LONG(a)   scm_ulong2num(a)
#define TO_SCM_STRING(a)          scm_makfrom0str(a)
#define TO_NEW_C_STRING(a)        gh_scm2newstr(a, NULL)
#define TO_SCM_BOOLEAN(a)         ((a) ? TRUE_VALUE : FALSE_VALUE)
#define TO_SCM_SYMBOL(a)          gh_symbol2scm(a)
#define TO_C_BOOLEAN_OR_T(a)      ((FALSE_P(a) || ((SCM_INUMP(a)) && (SCM_INUM(a) == 0))) ? 0 : 1)
#define TO_C_BOOLEAN(a)           ((FALSE_P(a)) ? 0 : 1)
#if HAVE_SCM_C_EVAL_STRING
  #define TO_SCM_FORM(Str)        scm_c_read_string(Str)
#else
  #define TO_SCM_FORM(Str)        scm_read_0str(Str)
#endif

#ifdef SCM_SYMBOL_CHARS
  #define EVAL_FORM(Form)         scm_eval((SCM)(Form), scm_interaction_environment())
  /* was scm_eval_x but I'm not sure that's safe */
  #define SYMBOL_TO_C_STRING(a)   SCM_SYMBOL_CHARS(a)
#else
  #define EVAL_FORM(Form)         scm_eval((SCM)(Form))
  #define SYMBOL_TO_C_STRING(a)   gh_symbol2newstr(a, NULL)
#endif

/* (need a way to pass an uninterpreted pointer from C to SCM then back to C) */
#if (SCM_DEBUG_TYPING_STRICTNESS == 2)
  #define SND_WRAP(a)             (TO_SCM_UNSIGNED_LONG((unsigned long)a))
#else
  #define SND_WRAP(a)             ((SCM)(TO_SCM_UNSIGNED_LONG((unsigned long)a)))
#endif

#define SND_UNWRAP(a)             TO_C_UNSIGNED_LONG(a)
#define SND_WRAPPED(a)            NOT_FALSE_P(scm_number_p(a))

#if HAVE_SCM_C_EVAL_STRING
  #define EVAL_STRING(Arg)        scm_c_eval_string(Arg)
#else
  #define EVAL_STRING(Arg)        scm_eval_0str(Arg)
#endif

#if HAVE_SCM_C_DEFINE_GSUBR
  #define NEW_PROCEDURE(Name, Func, Req, Opt, Rst) scm_c_define_gsubr(Name, Req, Opt, Rst, PROCEDURE Func)
#else
  #define NEW_PROCEDURE(Name, Func, Req, Opt, Rst) gh_new_procedure(Name, PROCEDURE Func, Req, Opt, Rst)
#endif

#if (!TIMING) && (!GCING) && (!WITH_MCHECK)

#define DEFINE_PROC(Name, Func, ReqArg, OptArg, RstArg, Doc) \
  scm_set_procedure_property_x(NEW_PROCEDURE(Name, Func, ReqArg, OptArg, RstArg), local_doc, TO_SCM_STRING(Doc))

#else
#if (TIMING)
/* add timing calls */
#define DEFINE_PROC(Name, Func, ReqArg, OptArg, RstArg, Doc) \
  NEW_PROCEDURE(Name "-t", PROCEDURE Func, ReqArg, OptArg, RstArg); \
  { \
    int tag; \
    tag = new_time(Name); \
    EVAL_STRING(mus_format("(define " Name " \
                              (lambda args \
                                (begin \
                                  (start-time %d) \
                                  (let ((res (apply " Name "-t args))) \
                                    (stop-time %d) \
                                    res))))", \
                            tag, tag)); \
  }
#else
#if WITH_MCHECK
#define DEFINE_PROC(Name, Func, ReqArg, OptArg, RstArg, Doc) \
  NEW_PROCEDURE(Name "-t", PROCEDURE Func, ReqArg, OptArg, RstArg); \
  EVAL_STRING("(define " Name " \
                 (lambda args \
                   (let ((res #f)) \
                     (set! res (apply " Name "-t args)) \
                     (mcheck-all) \
                     res)))");
#else
/* pound on gc-related bugs! */
#define DEFINE_PROC(Name, Func, ReqArg, OptArg, RstArg, Doc) \
  NEW_PROCEDURE(Name "-t", PROCEDURE Func, ReqArg, OptArg, RstArg); \
  EVAL_STRING("(define " Name " \
                 (lambda args \
                   (let ((res #f)) \
                     (set-last-proc \"" Name "\" args) \
                     (set! res (apply " Name "-t args)) \
                     (gc-1) \
                     res)))");
#endif
#endif
#endif

#if 0
#define DEFINE_SETPROC(Name, GetFunc, SetFunc, Req, Opt, Help) \
  define_procedure_with_setter(Name, PROCEDURE GetFunc, Help, \
			       "set-" Name, PROCEDURE SetFunc, local_doc, Req, Opt, Req + 1, Opt)

#define DEFINE_SETREVPROC(Name, GetFunc, SetFunc, RevFunc, Req, Opt, Help) \
  define_procedure_with_reversed_setter(Name, PROCEDURE GetFunc, Help, \
			       "set-" Name, PROCEDURE SetFunc, PROCEDURE RevFunc, local_doc, Req, Opt, Req + 1, Opt)
#endif

#if HAVE_SCM_C_DEFINE
  #define SND_DEFINE(a, b)         scm_c_define(a, b)
#else
  #define SND_DEFINE(a, b)         gh_define(a, b)
#endif

#define DEFINE_VAR(Name, Value, Help) \
  { \
    SND_DEFINE(Name, TO_SMALL_SCM_INT(Value)); \
    scm_set_object_property_x(TO_SCM_SYMBOL(Name), local_doc, TO_SCM_STRING(Help)); \
  }

/* DEFINE_PROC sets the documentation property of procedure Func to Doc
 * DEFINE_VAR sets the symbol's documentation property (gh_define returned the value in older version of Guile) 
 */

#define WITH_REVERSED_CHANNEL_ARGS(name_reversed, name) \
static SCM name_reversed(SCM arg1, SCM arg2, SCM arg3) \
{ \
  if (NOT_BOUND_P(arg2)) \
    return(name(arg1, UNDEFINED_VALUE, UNDEFINED_VALUE)); \
  else { \
    if (NOT_BOUND_P(arg3)) \
      return(name(arg2, arg1, UNDEFINED_VALUE)); \
    else return(name(arg3, arg1, arg2)); \
}}

#define ERROR(Type, Info)         scm_throw(Type, Info)

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

#define BOOLEAN_P(Arg)            (SCM_BOOLP(Arg))
#define NUMBER_P(Arg)             (NOT_FALSE_P(scm_real_p(Arg)))
#define INTEGER_P(Arg)            (NOT_FALSE_P(scm_integer_p(Arg)))
#define SYMBOL_P(Arg)             (SCM_SYMBOLP(Arg))
#define STRING_P(Arg)             (SCM_STRINGP(Arg))
#define VECTOR_P(Arg)             (SCM_VECTORP(Arg))
#define LIST_P(Arg)               (scm_ilength(Arg) >= 0)
#define LIST_P_WITH_LENGTH(Arg, Len) ((Len = ((int)scm_ilength(Arg))) >= 0)
#define LIST_LENGTH(Arg)          ((int)(scm_ilength(Arg)))
#define VECTOR_LENGTH(Arg)        ((int)(SCM_VECTOR_LENGTH(Arg)))
#define PROCEDURE_P(Arg)          (NOT_FALSE_P(scm_procedure_p(Arg)))
#define CONS(Arg1, Arg2)          scm_cons(Arg1, Arg2)
#define CONS2(Arg1, Arg2, Arg3)   scm_cons2(Arg1, Arg2, Arg3)
#define LIST_REF(Lst, Num)        scm_list_ref(Lst, TO_SMALL_SCM_INT(Num))
#define VECTOR_REF(Vect, Num)     scm_vector_ref(Vect, TO_SCM_INT(Num))
#define VECTOR_SET(Vect, Num, Val) scm_vector_set_x(Vect, TO_SCM_INT(Num), Val)
#define VECTOR_TO_LIST(Vect)      scm_vector_to_list(Vect)
#define REVERSE_LIST(Lst)         scm_reverse(Lst)
#if HAVE_SCM_C_MAKE_VECTOR
  #define MAKE_VECTOR(Num, Fill)  scm_c_make_vector((unsigned long)(Num), Fill)
#else
  #define MAKE_VECTOR(Num, Fill)  scm_make_vector(TO_SCM_INT(Num), Fill)
#endif
#ifdef SCM_CHARP
  #define CHAR_P(Arg)             (SCM_CHARP(Arg))
#else
  #define CHAR_P(Arg)             gh_char_p(Arg)
#endif
#ifdef SCM_CHAR
  #define TO_C_CHAR(Arg)          SCM_CHAR(Arg)
#else
  #define TO_C_CHAR(Arg)          gh_scm2char(Arg)
#endif
#define ARITY(Func)               scm_i_procedure_arity(Func)
#define KEYWORD_P(Obj)            (SCM_KEYWORDP(Obj))
#define MAKE_KEYWORD(Arg)         scm_c_make_keyword(Arg)
#define YES_WE_HAVE(Feature)      scm_add_feature(Feature)
#define DOCUMENTATION             scm_string_to_symbol(TO_SCM_STRING("documentation"))
#define MAKE_PERMANENT(Obj)       scm_permanent_object(Obj)
#define LOAD_SCM_FILE(File)       scm_primitive_load(TO_SCM_STRING(File))
#define MAKE_HOOK(Name, Args, Help) snd_create_hook(Name, Args, Help, local_doc)
#define MAKE_HELPLESS_HOOK(Args)  scm_make_hook(TO_SMALL_SCM_INT(Args))
#define CLEAR_HOOK(Arg)           scm_reset_hook_x(Arg)
#define HOOKED(a)                 (NOT_NULL_P(SCM_HOOK_PROCEDURES(a)))

#ifdef SCM_ASSERT_TYPE

  #define ASSERT_TYPE(Assertion, Arg, Position, Caller, Correct_Type) \
    SCM_ASSERT_TYPE(Assertion, Arg, Position, Caller, Correct_Type)

  #define ASSERT_SOUND(Origin, Snd, Offset) \
    if (!((INTEGER_P(Snd)) || (FALSE_P(Snd)) || (NOT_BOUND_P(Snd)) || (LIST_P(Snd)))) \
      scm_wrong_type_arg_msg(Origin, Offset, Snd, "an integer (sound index), boolean, or a list");

  #define ASSERT_CHANNEL(Origin, Snd, Chn, Offset) \
    if (!((INTEGER_P(Snd)) || (FALSE_P(Snd)) || (NOT_BOUND_P(Snd)) || (LIST_P(Snd)))) \
      scm_wrong_type_arg_msg(Origin, Offset, Snd, "an integer (sound index), boolean, or a list"); \
    else \
      if (!((INTEGER_P(Chn)) || (FALSE_P(Chn)) || (NOT_BOUND_P(Chn)))) \
        scm_wrong_type_arg_msg(Origin, Offset + 1, Chn, "an integer (0-based channel number) or boolean");

#else

  #define ASSERT_TYPE(Assertion, Arg, Position, Caller, Correct_Type) SCM_ASSERT(Assertion, Arg, Position, Caller)

  #define ASSERT_SOUND(Origin, Snd, Offset) \
    if (!((INTEGER_P(Snd)) || (FALSE_P(Snd)) || (NOT_BOUND_P(Snd)) || (LIST_P(Snd)))) \
      scm_wrong_type_arg(Origin, Offset, Snd);

  #define ASSERT_CHANNEL(Origin, Snd, Chn, Offset) \
    if (!((INTEGER_P(Snd)) || (FALSE_P(Snd)) || (NOT_BOUND_P(Snd)) || (LIST_P(Snd)))) \
      scm_wrong_type_arg(Origin, Offset, Snd); \
    else \
      if (!((INTEGER_P(Chn)) || (FALSE_P(Chn)) || (NOT_BOUND_P(Chn)))) \
        scm_wrong_type_arg(Origin, Offset + 1, Chn);

#endif

#if USE_SND
  #define CALL_0(Func, Caller)                   g_call0(Func, Caller)
  #define CALL_1(Func, Arg1, Caller)             g_call1(Func, Arg1, Caller)
  #define CALL_2(Func, Arg1, Arg2, Caller)       g_call2(Func, Arg1, Arg2, Caller)
  #define CALL_3(Func, Arg1, Arg2, Arg3, Caller) g_call3(Func, Arg1, Arg2, Arg3, Caller)
  #define APPLY(Func, Args, Caller)              g_call_any(Func, Args, Caller)
#else
  #define CALL_0(Func, Caller)                   scm_apply(Func, EMPTY_LIST, EMPTY_LIST)
  #define CALL_1(Func, Arg1, Caller)             scm_apply(Func, Arg1, scm_listofnull)
  #define CALL_2(Func, Arg1, Arg2, Caller)       scm_apply(Func, Arg1, scm_cons(Arg2, scm_listofnull))
  #define CALL_3(Func, Arg1, Arg2, Arg3, Caller) scm_apply(Func, Arg1, scm_cons2(Arg2, Arg3, scm_listofnull))
  #define APPLY(Func, Args, Caller)              scm_apply(Func, Args, EMPTY_LIST)
#endif

#define APPLY_EOL                     scm_listofnull
#define WRITE_STRING(Str, Prt)        scm_puts(Str, Prt)

#if HAVE_SCM_LIST_N
  #define LIST_0                      EMPTY_LIST
  #define LIST_1(a)                   scm_list_1(a)
  #define LIST_2(a, b)                scm_list_2(a, b)
  #define LIST_3(a, b, c)             scm_list_3(a, b, c)
  #define LIST_4(a, b, c, d)          scm_list_4(a, b, c, d)
  #define LIST_5(a, b, c, d, e)       scm_list_5(a, b, c, d, e)
  #define LIST_6(a, b, c, d, e, f)    scm_list_n(a, b, c, d, e, f, UNDEFINED_VALUE)
  #define LIST_7(a, b, c, d, e, f, g) scm_list_n(a, b, c, d, e, f, g, UNDEFINED_VALUE)
#else
  #define LIST_0                      SCM_LIST0
  #define LIST_1(a)                   SCM_LIST1(a)
  #define LIST_2(a, b)                SCM_LIST2(a, b)
  #define LIST_3(a, b, c)             SCM_LIST3(a, b, c)
  #define LIST_4(a, b, c, d)          SCM_LIST4(a, b, c, d)
  #define LIST_5(a, b, c, d, e)       SCM_LIST5(a, b, c, d, e)
  #define LIST_6(a, b, c, d, e, f)    SCM_LIST6(a, b, c, d, e, f)
  #define LIST_7(a, b, c, d, e, f, g) SCM_LIST7(a, b, c, d, e, f, g)
#endif

#endif
