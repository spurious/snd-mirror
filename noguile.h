#ifndef SND_NOGUILE_H
#define SND_NOGUILE_H

/* these are the extension-language entities called by Snd.
 *   In this case, since we're making Snd without any such language,
 *   they're mostly 0 and no-ops.  sl.h is an on-going Librep
 *   version, sg.h is the current Guile version, and I'm looking
 *   at Ruby since it appears to follow basically the same paradigm
 *   as the other two (i.e. one extension type (SCM, repv, VALUE), 
 *   ways to get/set/load C vars/funcs, and so on)
 */

#if LONG_INT_P
  #define SCM long
#else
  #define SCM int
#endif
#define FALSE_VALUE 0
#define TRUE_VALUE 1
#define EMPTY_LIST 0
#define UNDEFINED_VALUE 0

#ifdef __cplusplus
  #define PROCEDURE (SCM (*)())
  typedef SCM (*scm_catch_body_t) (void *data);
  typedef SCM (*scm_catch_handler_t) (void *data, SCM tag, SCM throw_args);
#else
  #define PROCEDURE
  #define scm_catch_body_t void *
  #define scm_catch_handler_t void *
#endif

#define scm_print_state int
#define scm_sizet int

#define scm_apply(a, b, c) 0
SCM scm_return_first(SCM a, ...);
#define scm_must_malloc(size, str) malloc(size)
#define scm_done_malloc(size) 

#define EQ_P(a, b) 0
#define LIST_0 0
#define LIST_1(a) 0
#define LIST_2(a, b) 0
#define LIST_3(a, b, c) 0
#define LIST_4(a, b, c, d) 0
#define LIST_5(a, b, c, d, e) 0
#define LIST_6(a, b, c, d, e, f) 0
#define LIST_7(a, b, c, d, e, f, g) 0
#define CAR(a) 0
#define CADR(a) 0
#define CADDR(a) 0
#define CADDDR(a) 0
#define CDR(a) 0
#define CDDR(a) 0
#define VECTOR_ELEMENTS(a) ((SCM *)a)
#define NEW_OBJECT(a, b, c)
#define HOOK_PROCEDURES(a) 0
#define SET_OBJECT_REF(a, b)
#define VARIABLE_REF(a) 0

/* this is the sg.h replacement */

#define RETURN_NEW_OBJECT(Tag, Val) return(0)
#define OBJECT_REF(a) 0
#define SND_LOOKUP(a) 0
#define TAG_TYPE int
#define OBJECT_TYPE_P(OBJ, TAG) 0
#define TRUE_P(a) 0
#define FALSE_P(a) 0
#define NULL_P(a) 0
#define BOUND_P(Arg) 0
#define NOT_BOUND_P(Arg) 1
#define INTEGER_ZERO 0

#ifndef __GNUC__
  #ifndef __FUNCTION__
    #define __FUNCTION__ ""
  #endif
#endif

#define TO_C_DOUBLE(a) 0.0
#define TO_C_DOUBLE_OR_ELSE(a, b) 0.0
#define TO_C_DOUBLE_WITH_ORIGIN(a, b) 0.0
#define TO_C_INT(a) 0
#define TO_C_INT_OR_ELSE(a, b) 0
#define TO_C_INT_OR_ELSE_WITH_ORIGIN(a, b, c) 0
#define TO_C_STRING(STR) NULL
#define TO_SCM_DOUBLE(a) 0
#define TO_SCM_INT(a) a
#define TO_SMALL_SCM_INT(a) a
#define TO_SMALL_C_INT(a) 0
#define TO_C_UNSIGNED_LONG(a) 0
#define TO_SCM_UNSIGNED_LONG(a) 0
#define TO_SCM_STRING(a) 0
#define TO_NEW_C_STRING(a) NULL
#define TO_SCM_BOOLEAN(a) 0
#define TO_SCM_SYMBOL(a) 0
#define TO_C_BOOLEAN_OR_T(a) 0
#define TO_C_BOOLEAN(a) 0
#define TO_SCM_FORM(Str) 0
#define EVAL_FORM(Form) 0
#define SYMBOL_TO_C_STRING(a) 0
#define SND_WRAP(a) 0
#define SND_UNWRAP(a) 0
#define SND_WRAPPED(a) 0
#define HOOKED(a) 0
#define DEFINE_PROC(Name, Func, ReqArg, OptArg, RstArg, Doc)
#define DEFINE_VAR(a, b, c)
#define WITH_REVERSED_CHANNEL_ARGS(name_reversed, name) static SCM name_reversed(SCM arg1, SCM arg2, SCM arg3) {return(0);}
#define BOOLEAN_P(Arg) 0
#define NUMBER_P(Arg) 0
#define INTEGER_P(Arg) 0
#define SYMBOL_P(Arg) 0
#define STRING_P(Arg) 0
#define VECTOR_P(Arg) 0
#define LIST_P(Arg) 0
#define LIST_P_WITH_LENGTH(Arg, Len) 0
#define LIST_LENGTH(Arg) 0
#define VECTOR_LENGTH(Arg) 0
#define PROCEDURE_P(Arg) 0
#define CONS(Arg1, Arg2) 0
#define CONS2(Arg1, Arg2, Arg3) 0
#define LIST_REF(Lst, Num) 0
#define VECTOR_REF(Vect, Num) 0
#define VECTOR_SET(a, b, c)
#define EVAL_STRING(Arg) 0
#define MAKE_VECTOR(Num, Fill) 0
#define VECTOR_TO_LIST(Vect) 0
#define MAKE_HOOK(Name, Args, Help) 0
#define MAKE_HELPLESS_HOOK(Args) 0
#define CLEAR_HOOK(Arg)
#define CHAR_P(Arg) 0
#define TO_C_CHAR(Arg) 0
#define ASSERT_SOUND(Origin, Snd, Offset)
#define ASSERT_CHANNEL(Origin, Snd, Chn, Offset)
#define CALL_0(Func, Caller) 0
#define CALL_1(Func, Arg1, Caller) 0
#define CALL_2(Func, Arg1, Arg2, Caller) 0
#define CALL_3(Func, Arg1, Arg2, Arg3, Caller) 0
#define APPLY(Func, Args, Caller) 0
#define APPLY_EOL 0
#define ARITY(Func) 0
#define KEYWORD_P(Obj) 0
#define MAKE_KEYWORD(Arg) 0
#define YES_WE_HAVE(Feature)
#define DOCUMENTATION 0
#define ASSERT_TYPE(Assertion, Arg, Position, Caller, Correct_Type)
#define REVERSE_LIST(a) a
#define MAKE_PERMANENT(a) 0
#define LOAD_SCM_FILE(a) 0
#define ERROR(Type, Info) fprintf(stderr, Info)
#define WRITE_STRING(a, b) fprintf(stdout, a)

#endif
