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

/* these are the left-over scm/gh entities */

#define SCM int
#define SCM_BOOL_F 0
#define SCM_BOOL_T 1
#define SCM_EOL 0
#define SCM_UNDEFINED 0

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

#define scm_make_hook(a) 0
#define scm_puts(a, b)
#define scm_wrong_type_arg(a, b, c) exit(0)
#define scm_wrong_num_args(a) exit(0)
#define scm_out_of_range(a, b) exit(0)
#define scm_make_smob_type(a, b) 0
#define scm_set_smob_mark(a, b)
#define scm_set_smob_print(a, b)
#define scm_set_smob_free(a, b)
#define scm_set_smob_equalp(a, b)
#define scm_permanent_object(a) 0
#define scm_string_to_symbol(a) 0
#define scm_throw(a, b) exit(a)
#define scm_set_object_property_x(a, b, c)
#define scm_add_feature(a) 
#define scm_c_make_keyword(a) 0
#define scm_reset_hook_x(a)
#define scm_apply(a, b, c) 0
#define scm_internal_stack_catch(a, b, c, d, e) 0
#define scm_vector_set_x(a, b, c)
#define scm_reverse(a) a
#define scm_object_property(a, b) 0
#define scm_vector_to_list(a) 0
#define scm_gc_mark(a)
#define scm_procedure_documentation(a) 0
#define scm_procedure_property(a, b) 0

#ifdef __GNUC__
  #define scm_return_first(a, ...) a
#else
  SCM scm_return_first(SCM a, ...);
#endif

#define gh_new_procedure(a, b, c, d, e) 0
#define gh_define(a, b) 0
#define gh_eval_file(a) 0

#define SCM_ASSERT(a, b, c, d)
#define SCM_EQ_P(a, b) 0
#define SCM_LIST0 0
#define SCM_LIST1(a) 0
#define SCM_LIST2(a, b) 0
#define SCM_LIST3(a, b, c) 0
#define SCM_LIST4(a, b, c, d) 0
#define SCM_LIST5(a, b, c, d, e) 0
#define SCM_LIST6(a, b, c, d, e, f) 0
#define SCM_CAR(a) 0
#define SCM_CADR(a) 0
#define SCM_CADDR(a) 0
#define SCM_CADDDR(a) 0
#define SCM_CDR(a) 0
#define SCM_CDDR(a) 0
#define SCM_VELTS(a) ((SCM *)a)
#define SCM_NEWSMOB(a, b, c)
#define SCM_HOOK_PROCEDURES(a) 0
#define SCM_SETCDR(a, b)


/* this is the sg.h replacement */

#define MAKE_HOOK(Name, Args, Help) 0
#define SND_RETURN_NEWSMOB(Tag, Val) 0
#define SND_VALUE_OF(a) 0
#define SND_SET_VALUE_OF(a, b) 
#define SND_LOOKUP(a) 0
#define SND_TAG_TYPE int
#define SND_SMOB_TYPE(TAG, OBJ) 0
#define SMOB_TYPE_P(OBJ, TAG) 0
#define SND_SETGCMARK(X)
#define TRUE_P(a) 0
#define FALSE_P(a) 0
#define NOT_TRUE_P(a) 0
#define NOT_FALSE_P(a) 0
#define NULL_P(a) 0
#define NOT_NULL_P(a) 0
#define BOUND_P(Arg) 0
#define NOT_BOUND_P(Arg) 0
#define INTEGER_ZERO 0

#ifndef __GNUC__
  #ifndef __FUNCTION__
    #define __FUNCTION__ ""
  #endif
#endif

#define TO_C_DOUBLE(a) 0.0
#define TO_C_DOUBLE_WITH_ORIGIN(a, b) 0.0
#define TO_C_INT(a) 0
#define TO_C_INT_OR_ELSE(a, b) 0
#define TO_C_STRING(STR) NULL
#define TO_SCM_DOUBLE(a) 0
#define TO_SCM_INT(a) 0
#define TO_SMALL_SCM_INT(a) 0
#define TO_SMALL_C_INT(a) 0
#define TO_C_UNSIGNED_LONG(a) 0
#define TO_SCM_STRING(a) 0
#define TO_NEW_C_STRING(a) NULL
#define TO_SCM_BOOLEAN(a) 0
#define TO_SCM_SYMBOL(a) 0
#define TO_C_BOOLEAN_OR_T(a) 0
#define TO_C_BOOLEAN(a) 0
#define SYMBOL_TO_C_STRING(a) 0
#define SND_WRAP(a) 0
#define SND_UNWRAP(a) 0
#define SND_WRAPPED(a) 0
#define HOOKED(a) 0
#define DEFINE_PROC(Name, Func, ReqArg, OptArg, RstArg, Doc)
#define DEFINE_VAR(a, b, c)
#define WITH_REVERSED_CHANNEL_ARGS(name_reversed, name) static SCM name_reversed(SCM arg1, SCM arg2, SCM arg3) {return(0);}
#define NO_SUCH_CHANNEL 0
#define NO_SUCH_SOUND 0
#define NO_SUCH_MARK 0
#define NO_SUCH_MIX 0
#define NO_SUCH_TRACK 0
#define NO_SUCH_MENU 0
#define NO_SUCH_FILE 0
#define NO_SUCH_REGION 0
#define NO_SUCH_SAMPLE 0
#define NO_SUCH_ENVELOPE 0
#define NO_SUCH_EDIT 0
#define CANNOT_SAVE 0
#define CANNOT_PRINT 0
#define IMPOSSIBLE_BOUNDS 0
#define NO_ACTIVE_SELECTION 0
#define MUS_MISC_ERROR 0
#define NO_SUCH_AXIS_INFO 0
#define NO_SUCH_PLAYER 0
#define NO_SUCH_AXIS_CONTEXT 0
#define BAD_ARITY 0
#define BOOLEAN_P(Arg) 0
#define NUMBER_P(Arg) 0
#define INTEGER_P(Arg) 0
#define BOOLEAN_IF_BOUND_P(Arg) 0
#define INTEGER_IF_BOUND_P(Arg) 0
#define NUMBER_IF_BOUND_P(Arg) 0
#define INTEGER_OR_BOOLEAN_IF_BOUND_P(Arg) 0
#define NUMBER_OR_BOOLEAN_IF_BOUND_P(Arg) 0
#define NUMBER_OR_BOOLEAN_P(Arg) 0
#define INTEGER_OR_BOOLEAN_P(Arg) 0
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
#define EVAL_STRING(Arg) 0
#define MAKE_VECTOR(Num, Fill) 0
#define CHAR_P(Arg) 0
#define TO_C_CHAR(Arg) 0
#define SND_ASSERT_SND(Origin, Snd, Offset)
#define SND_ASSERT_CHAN(Origin, Snd, Chn, Offset)
#define CALL0(Func, Caller) 0
#define CALL1(Func, Arg1, Caller) 0
#define CALL2(Func, Arg1, Arg2, Caller) 0
#define CALL3(Func, Arg1, Arg2, Arg3, Caller) 0
#define APPLY(Func, Args, Caller) 0
#define ARITY(Func) 0
#define KEYWORD_P(Obj) 0
#define MAKE_KEYWORD(Arg) 0

#endif
