#ifndef SL_H
#define SL_H

/* these are the left-over scm/gh entities */
#define SCM repv
#define SCM_BOOL_F rep_FALSE
#define SCM_BOOL_T rep_TRUE
#define SCM_EOL rep_NULL

#define SCM_UNDEFINED 0

#define scm_catch_body_t void *
#define scm_catch_handler_t void *
#define scm_print_state int
#define scm_sizet int

#define scm_listofnull rep_NULL
#define scm_make_hook(a) 0
#define scm_puts(a, b)
#define scm_wrong_type_arg(a, b, c) rep_signal_arg_error(c, b)
#define scm_wrong_num_args(a) rep_signal_missing_arg(a)
#define scm_out_of_range(a, b) exit(0)
#define scm_make_smob_type(a, b) 0
#define scm_set_smob_mark(a, b)
#define scm_set_smob_print(a, b)
#define scm_set_smob_free(a, b)
#define scm_set_smob_equalp(a, b)
#define scm_permanent_object(a) 0
#define scm_string_to_symbol(a) 0
#define scm_throw(a, b) Fthrow(a, b)
#define scm_set_object_property_x(a, b, c)
#define scm_add_feature(a) Fprovide(a)
#define scm_c_make_keyword(a) Fmake_keyword(a)
#define scm_reset_hook_x(a)
#define scm_apply(a, b, c) rep_apply(a, b)
#define scm_internal_stack_catch(a, b, c, d, e) 0
#define scm_vector_set_x(a, b, c) Faset(a, b, c)
#define scm_reverse(a) Freverse(a)
#define scm_object_property(a, b) 0
#define scm_vector_to_list(a) 0
#define scm_gc_mark(a)
#define scm_procedure_documentation(a) 0
#define scm_procedure_property(a, b) 0

#define scm_return_first(a, ...) a

#define gh_new_procedure(a, b, c, d, e) 0
#define gh_define(a, b) 0
#define gh_eval_file(a) 0

#define SCM_ASSERT(a, b, c, d)
#define SCM_EQ_P(a, b) Feq(a, b)
#define SCM_LIST0 rep_NULL
#define SCM_LIST1(Arg1) rep_LIST_1(Arg1)
#define SCM_LIST2(Arg1, Arg2) rep_LIST_2(Arg1, Arg2)
#define SCM_LIST3(Arg1, Arg2, Arg3) rep_LIST_3(Arg1, Arg2, Arg3)
#define SCM_LIST4(Arg1, Arg2, Arg3, Arg4) rep_LIST_4(Arg1, Arg2, Arg3, Arg4)
#define SCM_LIST5(Arg1, Arg2, Arg3, Arg4, Arg5) rep_LIST_5(Arg1, Arg2, Arg3, Arg4, Arg5)
#define SCM_LIST6(a, b, c, d, e, f) 0
#define SCM_CAR(Arg) rep_CAR(Arg)
#define SCM_CDR(Arg) rep_CDR(Arg)
#define SCM_CDDR(Arg) rep_CDDR(Arg)
#define SCM_CADR(Arg) rep_CADR(Arg)
#define SCM_CADDR(Arg) rep_CADDR(Arg)
#define SCM_CADDDR(Arg) rep_CADDDR(Arg)
#define SCM_VELTS(a) ((SCM *)a)
#define SCM_NEWSMOB(a, b, c)
#define SCM_HOOK_PROCEDURES(a) 0
#define SCM_SETCDR(a, b)

#define SCM_FNC

#define MAKE_HOOK(Name, Args, Help) 0
#define SND_RETURN_NEWSMOB(Tag, Val) 0
#define SND_VALUE_OF(a) 0
#define SND_SET_VALUE_OF(a, b) 
#define SND_LOOKUP(a) 0
#define SND_REMEMBER(OBJ)
#define SND_TAG_TYPE int
#define SND_SMOB_TYPE(TAG, OBJ) 0
#define SMOB_TYPE_P(OBJ, TAG) 0
#define SND_SETGCMARK(X)
#define TRUE_P(a) ((a) == Qt)
#define FALSE_P(a) ((a) == Qnil)
#define NOT_TRUE_P(a) ((a) != Qt))
#define NOT_FALSE_P(a) ((a) != Qnil)
#define NULL_P(a) (((a) == Qnil) || (LIST_LENGTH(a) == 0))
#define NOT_NULL_P(a) (!(NULL_P(a)))
#define BOUND_P(Arg) Fboundp(Arg)
#define NOT_BOUND_P(Arg) (!Fboundp(Arg))
#define INTEGER_ZERO rep_MAKE_INT(0)

#ifndef __GNUC__
  #ifndef __FUNCTION__
    #define __FUNCTION__ ""
  #endif
#endif

#define TO_C_DOUBLE(a) rep_get_float(a)
#define TO_C_DOUBLE_WITH_ORIGIN(a, b) rep_get_float(a)
#define TO_C_INT(Arg) rep_INT(Arg)
#define TO_C_INT_OR_ELSE(a, b) (INTEGER_P(a) ? rep_INT(a) : b)
#define TO_C_STRING(STR) NULL
#define TO_SCM_DOUBLE(a) rep_make_float(a, rep_FALSE)
#define TO_SCM_INT(Arg) rep_MAKE_INT(Arg)
#define TO_SMALL_SCM_INT(a) rep_MAKE_INT(a)
#define TO_SMALL_C_INT(a) rep_INT(a)
#define TO_C_UNSIGNED_LONG(a) rep_get_long_uint(a)
#define TO_SCM_STRING(a) 0
#define TO_NEW_C_STRING(a) NULL
#define TO_SCM_BOOLEAN(a) ((a) ? Qt : Qnil)
#define TO_SCM_SYMBOL(a) 0
#define TO_C_BOOLEAN_OR_T(a) (((a) == Qnil) ? 0 : 1)
#define TO_C_BOOLEAN(a) (((a) == Qnil) ? 0 : 1)
#define SYMBOL_TO_NEW_C_STRING(a) 0
#define SND_WRAP(a) rep_make_long_uint((unsigned long)a)
#define SND_UNWRAP(a) rep_get_long_uint(a)
#define SND_WRAPPED(a) 0
#define HOOKED(a) ((a) != Qnil)

#define DEFINE_PROC(Name, Func, ReqArg, OptArg, RstArg, Doc)
/*
    DEFSTRING(rep_CONCAT(Func, __name), Name); \
    rep_ALIGN_CELL(rep_xsubr S ## Func) = { rep_Subr1, (repv (*)()) Func, \
				       rep_VAL(&rep_CONCAT(Func, __name)), \
				       rep_NULL }; \
    rep_add_subr(&(S ## Func), rep_TRUE);

keep array of these, fill the fields explicitly 
typedef struct {
    repv car;
    repv (*fun)();
    repv name;
    repv int_spec;
} rep_xsubr;
*/
#define DEFINE_VAR(Name, Value, Documentation) \
  { \
   DEFSYM(tmp, Name, Documentation); \
   rep_INTERN(tmp); \
   Fset(Qtmp, TO_SCM_INT(Value)); \
  }

#define WITH_REVERSED_CHANNEL_ARGS(name_reversed, name) static SCM name_reversed(SCM arg1, SCM arg2, SCM arg3) {return(rep_NULL);}
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
#define BOOLEAN_P(Arg) (((Arg) == Qt) || ((Arg) == Qnil))
#define NUMBER_P(Arg) rep_NUMBERP(Arg)
#define INTEGER_P(Arg)  rep_INTEGERP(Arg)
#define BOOLEAN_IF_BOUND_P(Arg) ((BOOLEAN_P(Arg)) || (NOT_BOUND_P(Arg)))
#define INTEGER_IF_BOUND_P(Arg) ((NOT_BOUND_P(Arg)) || (INTEGER_P(Arg)))
#define NUMBER_IF_BOUND_P(Arg) ((NOT_BOUND_P(Arg)) || (NUMBER_P(Arg)))
#define INTEGER_OR_BOOLEAN_IF_BOUND_P(Arg) ((BOOLEAN_P(Arg)) || (NOT_BOUND_P(Arg)) || (INTEGER_P(Arg)))
#define NUMBER_OR_BOOLEAN_IF_BOUND_P(Arg) ((BOOLEAN_P(Arg)) || (NOT_BOUND_P(Arg)) || (NUMBER_P(Arg)))
#define NUMBER_OR_BOOLEAN_P(Arg) ((BOOLEAN_P(Arg)) || (NUMBER_P(Arg)))
#define INTEGER_OR_BOOLEAN_P(Arg) ((BOOLEAN_P(Arg)) || (INTEGER_P(Arg)))
#define SYMBOL_P(Arg) rep_SYMBOLP(Arg)
#define STRING_P(Arg) rep_STRINGP(Arg)
#define VECTOR_P(Arg) rep_VECTORP(Arg)
#define LIST_P(Arg) rep_LISTP(Arg)
#define LIST_P_WITH_LENGTH(Arg, Len) 0
#define LIST_LENGTH(Arg) rep_list_length(Arg)
#define VECTOR_LENGTH(Arg) rep_VECT_LEN(Arg)
#define PROCEDURE_P(Arg) ((Ffunctionp(Arg)) || (Fsubrp(Arg)) || (Fclosurep(Arg)))
#define CONS(Arg1, Arg2) Fcons(Arg1, Arg2)
#define CONS2(Arg1, Arg2, Arg3) Fcons(Arg1, Fcons(Arg2, Arg3))
#define LIST_REF(Lst, Num) Fnth(Num, Lst)
#define VECTOR_REF(Vect, Num) Faref(Vect, Num)
#define EVAL_STRING(Arg) 0
#define MAKE_VECTOR(Num, Fill) Fmake_vector(Num, Fill)
#define CHAR_P(Arg) STRING_P(Arg)
#define TO_C_CHAR(Arg) TO_C_STRING(Arg)
#define SND_ASSERT_SND(Origin, Snd, Offset)
#define SND_ASSERT_CHAN(Origin, Snd, Chn, Offset)
#define CALL0(Func, Caller) rep_call_lisp0(Func)
#define CALL1(Func, Arg1, Caller) rep_call_lisp1(Func, Arg1)
#define CALL2(Func, Arg1, Arg2, Caller) rep_call_lisp2(Func, Arg1, Arg2)
#define CALL3(Func, Arg1, Arg2, Arg3, Caller) rep_call_lisp3(Func, Arg1, Arg2, Arg3)
#define APPLY(Func, Args, Caller) rep_apply(Func, Args)
#define ARITY(Func) Qnil
#define KEYWORD_P(Obj) rep_KEYWORDP(Obj)
#define MAKE_KEYWORD(Arg) Qnil

#endif
