#ifndef SZ_H
#define SZ_H

/* these are the extension-language entities called by Snd.
 *   In this case, we're making Snd with mzscheme.
 */

#define SCM                 Scheme_Object *
#define SCM_BOOL_F          scheme_false
#define SCM_BOOL_T          scheme_true
#define SCM_EOL             scheme_null
#define SCM_UNDEFINED       scheme_undefined

#ifdef __cplusplus
  #define SCM_FNC (SCM (*)())
  typedef SCM (*scm_catch_body_t) (void *data);
  typedef SCM (*scm_catch_handler_t) (void *data, SCM tag, SCM throw_args);
#else
  #define SCM_FNC
  #define scm_catch_body_t void *
  #define scm_catch_handler_t void *
#endif

#define scm_print_state int
#define scm_sizet int

#define scm_apply(a, b, c)
SCM scm_return_first(SCM a, ...);
#define scm_must_malloc(size, str) malloc(size)
#define scm_done_malloc(size) 

#define EQ_P(a, b)                  scheme_eq(a, b)
#define LIST_0                       scheme_null
#define LIST_1(a)                    scheme_make_pair(a, scheme_null)
#define LIST_2(a, b)                 scheme_make_pair(a, LIST_1(b))
#define LIST_3(a, b, c)              scheme_make_pair(a, LIST_2(b, c))
#define LIST_4(a, b, c, d)           scheme_make_pair(a, LIST_3(b, c, d))
#define LIST_5(a, b, c, d, e)        scheme_make_pair(a, LIST_4(b, c, d, e))
#define LIST_6(a, b, c, d, e, f)     scheme_make_pair(a, LIST_5(b, c, d, e, f))
#define LIST_7(a, b, c, d, e, f, g)  scheme_make_pair(a, LIST_6(b, c, d, e, f, g))
#define CAR(a)                       SCHEME_CAR(a)
#define CADR(a)                      SCHEME_CADR(a)
#define CADDR(a)                     SCHEME_CAR(SCHEME_CDDR(a))
#define CADDDR(a)                    SCHEME_CAR(SCHEME_CDR(SCHEME_CDDR(a)))
#define CDR(a)                       SCHEME_CDR(a)
#define CDDR(a)                      SCHEME_CDDR(a)
#define VECTOR_ELEMENTS(a)           SCHEME_VEC_ELS(a)
#define SCM_NEWSMOB(a, b, c)
#define HOOK_PROCEDURES(a)
#define SET_SCM_VALUE(a, b)

#define SND_RETURN_NEWSMOB(Tag, Val) 0
#define SND_VALUE_OF(a) 0
#define SND_LOOKUP(a) 0
#define SND_TAG_TYPE int
#define SND_SMOB_TYPE(TAG, OBJ) 0
#define SMOB_TYPE_P(OBJ, TAG) 0

#define TRUE_P(a)                       scheme_eq(a, SCM_BOOL_T)
#define FALSE_P(a)                      SCHEME_FALSEP(a)
#define NULL_P(a)                       SCHEME_NULLP(a)
#define BOUND_P(Arg)                    (!(SCM_EQ_P(Arg, SCM_UNDEFINED)))
#define NOT_BOUND_P(Arg)                (SCM_EQ_P(Arg, SCM_UNDEFINED)))
#define INTEGER_ZERO                    scheme_make_integer_value(0)

#ifndef __GNUC__
  #ifndef __FUNCTION__
    #define __FUNCTION__ ""
  #endif
#endif

#define TO_C_DOUBLE(a)                       SCHEME_DBL_VAL(a)
#define TO_C_DOUBLE_OR_ELSE(a, b)            SCHEME_DBL_VAL(a)
#define TO_C_DOUBLE_WITH_ORIGIN(a, b)        SCHEME_DBL_VAL(a)
#define TO_C_INT(a)                          SCHEME_INT_VAL(a)
#define TO_C_INT_OR_ELSE(a, b)               0
#define TO_C_INT_OR_ELSE_WITH_ORIGIN(a, b, c) 0
#define TO_C_STRING(STR)                     SCHEME_STR_VAL(a)
#define TO_SCM_DOUBLE(a)                     scheme_make_double(a)
#define TO_SCM_INT(a)                        scheme_make_integer_value((long)a)
#define TO_SMALL_SCM_INT(a)                  scheme_make_integer((long)a)
#define TO_SMALL_C_INT(a)                    SCHEME_INT_VAL(a)
#define TO_C_UNSIGNED_LONG(a)                scheme_get_int_val(a)
#define TO_SCM_UNSIGNED_LONG(a)              SCM_BOOL_F
#define TO_SCM_STRING(a)                     scheme_make_string(a)
#define TO_NEW_C_STRING(a)                   strdup(SCHEME_STR_VAL(a))
#define TO_SCM_BOOLEAN(a)                    ((a) ? scheme_true : scheme_false)
#define TO_SCM_SYMBOL(a)                     scheme_intern_symbol(a)
#define TO_C_BOOLEAN_OR_T(a)                 (BOOLEAN_P(a) ? (TRUE_P(a)) : 1)
#define TO_C_BOOLEAN(a)                      ((FALSE_P(a)) ? 0 : 1)

#define TO_SCM_FORM(Str)                     SCM_BOOL_F
#define EVAL_FORM(Form)                      SCM_BOOL_F

#define SYMBOL_TO_C_STRING(a)                SCHEME_SYM_VAL(a)
#define SND_WRAP(a)                          scheme_make_integer_value_from_unsigned((unsigned long)a)
#define SND_UNWRAP(a) 0
#define SND_WRAPPED(a) 0
#define HOOKED(a) 0
#define DEFINE_PROC(Name, Func, ReqArg, OptArg, RstArg, Doc)
#define DEFINE_VAR(a, b, c)
#define WITH_REVERSED_CHANNEL_ARGS(name_reversed, name)

#define BOOLEAN_P(Arg)                 SCHEME_BOOLP(Arg)
#define NUMBER_P(Arg)                  (SCHEME_REALP(Arg) && (!SCHEME_COMPLEXP(Arg)))
#define INTEGER_P(Arg)                 SCHEME_INTP(Arg)
#define SYMBOL_P(Arg)                  SCHEME_SYMBOLP(Arg)
#define STRING_P(Arg)                  SCHEME_STRINGP(Arg)
#define VECTOR_P(Arg)                  SCHEME_VECTORP(Arg)
#define LIST_P(Arg)                    SCHEME_LISTP(Arg)
#define LIST_P_WITH_LENGTH(Arg, Len)   ((SCHEME_LISTP(Arg)) && (Len = scheme_list_length(Arg)))
#define LIST_LENGTH(Arg)               scheme_list_length(Arg)
#define VECTOR_LENGTH(Arg)             SCHEME_VEC_SIZE(Arg)
#define PROCEDURE_P(Arg)               SCHEME_PROCP(Arg)
#define CONS(Arg1, Arg2)               scheme_make_pair(Arg1, Arg2)
#define CONS2(Arg1, Arg2, Arg3)        scheme_make_pair(Arg1, scheme_make_pair(Arg2, Arg3))
#define LIST_REF(Lst, Num) 0
#define VECTOR_REF(Vect, Num)          SCHEME_VEC_ELS(a)[Num]
#define VECTOR_SET(a, b, c)            SCHEME_VEC_ELS(a)[b] = c
#define EVAL_STRING(Arg)               scheme_eval_string(Arg, get_global_env())
#define MAKE_VECTOR(Num, Fill)         scheme_make_vector(Num, Fill)
#define VECTOR_TO_LIST(Vect)           scheme_vector_to_list(Vect)
#define MAKE_HOOK(Name, Args, Help) 0
#define MAKE_HELPLESS_HOOK(Args) 0
#define CLEAR_HOOK(Arg)
#define CHAR_P(Arg)                     SCHEME_CHARP(Arg)
#define TO_C_CHAR(Arg)                  SCHEME_CHAR_VAL(Arg)
#define SND_ASSERT_SND(Origin, Snd, Offset)
#define SND_ASSERT_CHAN(Origin, Snd, Chn, Offset)
#define CALL0(Func, Caller) 0
#define CALL1(Func, Arg1, Caller) 0
#define CALL2(Func, Arg1, Arg2, Caller) 0
#define CALL3(Func, Arg1, Arg2, Arg3, Caller) 0
#define APPLY(Func, Args, Caller) 0
#define APPLY_EOL scheme_null
#define ARITY(Func) 0
#define KEYWORD_P(Obj) 0
#define MAKE_KEYWORD(Arg) 0
#define YES_WE_HAVE(Feature)
#define DOCUMENTATION 0
#define ASSERT_TYPE(Assertion, Arg, Position, Caller, Correct_Type)
#define REVERSE_LIST(a) a
#define MAKE_PERMANENT(a) a
#define LOAD_SCM_FILE(a)              scheme_load(a)
#define ERROR(Type, Info)             scheme_signal_error(Info)
#define WRITE_STRING(a, b)             scheme_write_string(a, 0, strlen(a), b)

#endif
