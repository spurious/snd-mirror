#ifndef SL_H
#define SL_H

/* Librep versions of the various extension-language entities used in Snd */
/*   this project is currently on hold; I'm finding it very difficult to
 *   debug Librep/Snd problems.  The librep_* code in snd-scm.c does work.
 */

/* these are the left-over scm/gh entities */
#define SCM repv
#define SCM_BOOL_F     rep_FALSE
#define SCM_BOOL_T     rep_TRUE
#define SCM_EOL        rep_NULL
#define SCM_UNDEFINED  rep_NULL

#define scm_catch_body_t void *
#define scm_catch_handler_t void *
#define scm_print_state int
#define scm_sizet int

#define scm_apply(a, b, c) rep_apply(a, b)
SCM scm_return_first(SCM a, ...);
#define scm_must_malloc(size, str) malloc(size)
#define scm_done_malloc(size) 

#define SCM_EQ_P(a, b)  ((a) == (b))
#define SCM_LIST0       Qnil
#define SCM_LIST1(Arg1) rep_LIST_1(Arg1)
#define SCM_LIST2(Arg1, Arg2) rep_LIST_2(Arg1, Arg2)
#define SCM_LIST3(Arg1, Arg2, Arg3) rep_LIST_3(Arg1, Arg2, Arg3)
#define SCM_LIST4(Arg1, Arg2, Arg3, Arg4) rep_LIST_4(Arg1, Arg2, Arg3, Arg4)
#define SCM_LIST5(Arg1, Arg2, Arg3, Arg4, Arg5) rep_LIST_5(Arg1, Arg2, Arg3, Arg4, Arg5)
#define SCM_LIST6(a, b, c, d, e, f) Fcons(a, SCM_LIST5(b, c, d, e, f))
#define SCM_CAR(Arg)    rep_CAR(Arg)
#define SCM_CDR(Arg)    rep_CDR(Arg)
#define SCM_CDDR(Arg)   rep_CDDR(Arg)
#define SCM_CADR(Arg)   rep_CADR(Arg)
#define SCM_CADDR(Arg)  rep_CADDR(Arg)
#define SCM_CADDDR(Arg) rep_CADDDR(Arg)
#define SCM_VELTS(a)    (repv *)(rep_VECT(a)->array)
#define SCM_NEWSMOB(a, b, c) Qnil
#define SCM_HOOK_PROCEDURES(a) Qnil
#define SET_SCM_VALUE(a, b)

#ifdef __cplusplus
  #define SCM_FNC (repv(*)())
#else
  #define SCM_FNC
#endif

#define MAKE_HOOK(Name, Args, Help)  Qnil
#define MAKE_HELPLESS_HOOK(a)        Qnil
#define SND_RETURN_NEWSMOB(Tag, Val) Qnil
#define SND_VALUE_OF(a)              Qnil
#define SND_SET_VALUE_OF(a, b) 
#define SND_LOOKUP(a)                Qnil
#define SND_TAG_TYPE                 int
#define SND_SMOB_TYPE(TAG, OBJ)      Qnil
#define SMOB_TYPE_P(OBJ, TAG)        Qnil
#define TRUE_P(a)                    ((a) == Qt)
#define FALSE_P(a)                   ((a) == Qnil)
#define NOT_TRUE_P(a)                ((a) != Qt))
#define NOT_FALSE_P(a)               ((a) != Qnil)
#define NULL_P(a)                    (((a) == Qnil) || (LIST_LENGTH(a) == 0))
#define NOT_NULL_P(a)                (!(NULL_P(a)))
#define BOUND_P(Arg)                 ((Arg) && (Fboundp(Arg)))
#define NOT_BOUND_P(Arg)             (!Fboundp(Arg))
#define INTEGER_ZERO                 rep_MAKE_INT(0)

#ifndef __GNUC__
  #ifndef __FUNCTION__
    #define __FUNCTION__ ""
  #endif
#endif

#define TO_C_DOUBLE(a)               rep_get_float(a)
#define TO_C_DOUBLE_WITH_ORIGIN(a, b) rep_get_float(a)
#define TO_C_INT(Arg)                rep_INT(Arg)
#define TO_C_INT_OR_ELSE(a, b)       (INTEGER_P(a) ? rep_INT(a) : b)
#define TO_C_INT_OR_ELSE_WITH_ORIGIN(a, b, c) (INTEGER_P(a) ? rep_INT(a) : b)
#define TO_C_STRING(Str)             ((char *)(rep_STR(Str)))
#define TO_SCM_DOUBLE(a)             rep_make_float(a, rep_FALSE)
#define TO_SCM_INT(Arg)              rep_MAKE_INT(Arg)
#define TO_SMALL_SCM_INT(a)          rep_MAKE_INT(a)
#define TO_SMALL_C_INT(a)            rep_INT(a)
#define TO_C_UNSIGNED_LONG(a)        rep_get_long_uint(a)
#define TO_SCM_STRING(a)             rep_string_dupn(a, strlen(a))
#define TO_NEW_C_STRING(a)           copy_string(TO_C_STRING(a))
#define TO_SCM_BOOLEAN(a)            ((a) ? Qt : Qnil)
#define TO_SCM_SYMBOL(a)             Fintern_symbol(Fmake_symbol(rep_string_dupn(a, strlen(a))), Qnil)
#define TO_C_BOOLEAN_OR_T(a)         (((a) == Qnil) ? 0 : 1)
#define TO_C_BOOLEAN(a)              (((a) == Qnil) ? 0 : 1)

#define TO_SCM_FORM(Str)             Qnil
#define EVAL_FORM(Form)              Qnil

#define SYMBOL_TO_C_STRING(a)        rep_STR(rep_SYM(a)->name)
#define SND_WRAP(a)                  rep_make_long_uint((unsigned long)a)
#define SND_UNWRAP(a)                rep_get_long_uint(a)
#define SND_WRAPPED(a)               (a)
#define HOOKED(a)                    ((a) && ((a) != Qnil))

#define DEFINE_PROC(Name, Func, ReqArg, OptArg, RstArg, Doc) \
  librep_new_procedure(Name, Func, ReqArg, OptArg, RstArg, Doc);

#define DEFINE_VAR(Name, Value, Documentation) \
  librep_new_variable(Name, Value, Documentation);

#define WITH_REVERSED_CHANNEL_ARGS(name_reversed, name) static SCM name_reversed(SCM arg1, SCM arg2, SCM arg3) {return(rep_NULL);}

#define NO_SUCH_CHANNEL      TO_SCM_SYMBOL("no-such-channel")
#define NO_SUCH_SOUND        TO_SCM_SYMBOL("no-such-sound")
#define NO_SUCH_MARK         TO_SCM_SYMBOL("no-such-mark")
#define NO_SUCH_MIX          TO_SCM_SYMBOL("no-such-mix")
#define NO_SUCH_TRACK        TO_SCM_SYMBOL("no-such-track")
#define NO_SUCH_MENU         TO_SCM_SYMBOL("no-such-menu")
#define NO_SUCH_FILE         TO_SCM_SYMBOL("no-such-file")
#define NO_SUCH_REGION       TO_SCM_SYMBOL("no-such-region")
#define NO_SUCH_SAMPLE       TO_SCM_SYMBOL("no-such-sample")
#define NO_SUCH_ENVELOPE     TO_SCM_SYMBOL("no-such-envelope")
#define NO_SUCH_EDIT         TO_SCM_SYMBOL("no-such-edit")
#define CANNOT_SAVE          TO_SCM_SYMBOL("cannot-save")
#define CANNOT_PRINT         TO_SCM_SYMBOL("cannot-print")
#define IMPOSSIBLE_BOUNDS    TO_SCM_SYMBOL("impossible-bounds")
#define NO_ACTIVE_SELECTION  TO_SCM_SYMBOL("no-active-selection")
#define MUS_MISC_ERROR       TO_SCM_SYMBOL("mus-error")
#define NO_SUCH_AXIS_INFO    TO_SCM_SYMBOL("no-such-axis")
#define NO_SUCH_PLAYER       TO_SCM_SYMBOL("no-such-player")
#define NO_SUCH_AXIS_CONTEXT TO_SCM_SYMBOL("no-such-graphics-context")
#define BAD_ARITY            TO_SCM_SYMBOL("bad-arity")

#define ERROR(a, b)               Fthrow(a, b)

#define BOOLEAN_P(Arg)            (((Arg) == Qt) || ((Arg) == Qnil))
#define NUMBER_P(Arg)             ((Arg) && (rep_NUMBERP(Arg)))
#define INTEGER_P(Arg)            ((Arg) && (rep_INTEGERP(Arg)))
#define BOOLEAN_IF_BOUND_P(Arg)   ((BOOLEAN_P(Arg)) || (NOT_BOUND_P(Arg)))
#define INTEGER_IF_BOUND_P(Arg)   ((NOT_BOUND_P(Arg)) || (INTEGER_P(Arg)))
#define NUMBER_IF_BOUND_P(Arg)    ((NOT_BOUND_P(Arg)) || (NUMBER_P(Arg)))
#define STRING_IF_BOUND_P(Arg)    ((NOT_BOUND_P(Arg)) || (STRING_P(Arg)))
#define INTEGER_OR_BOOLEAN_IF_BOUND_P(Arg) ((BOOLEAN_P(Arg)) || (NOT_BOUND_P(Arg)) || (INTEGER_P(Arg)))
#define NUMBER_OR_BOOLEAN_IF_BOUND_P(Arg) ((BOOLEAN_P(Arg)) || (NOT_BOUND_P(Arg)) || (NUMBER_P(Arg)))
#define NUMBER_OR_BOOLEAN_P(Arg)  ((BOOLEAN_P(Arg)) || (NUMBER_P(Arg)))
#define INTEGER_OR_BOOLEAN_P(Arg) ((BOOLEAN_P(Arg)) || (INTEGER_P(Arg)))
#define SYMBOL_P(Arg)             ((Arg) && (rep_SYMBOLP(Arg)))
#define STRING_P(Arg)             ((Arg) && (rep_STRINGP(Arg)))
#define VECTOR_P(Arg)             ((Arg) && (rep_VECTORP(Arg)))
#define LIST_P(Arg)               ((Arg) && (rep_LISTP(Arg)))
#define LIST_P_WITH_LENGTH(Arg, Len) ((Arg) && (LIST_P(Arg)) && ((Len = LIST_LENGTH(Arg)) >= 0))
#define LIST_LENGTH(Arg)          rep_list_length(Arg)
#define VECTOR_LENGTH(Arg)        rep_VECT_LEN(Arg)
#define PROCEDURE_P(Arg)          ((Arg) && ((Ffunctionp(Arg)) || (Fsubrp(Arg)) || (Fclosurep(Arg))))
#define CONS(Arg1, Arg2)          Fcons(Arg1, Arg2)
#define CONS2(Arg1, Arg2, Arg3)   Fcons(Arg1, Fcons(Arg2, Arg3))
#define LIST_REF(Lst, Num)        Fnth(Num, Lst)
#define VECTOR_REF(Vect, Num)     Faref(Vect, Num)
#define VECTOR_SET(Vect, Num, Val) Faset(Vect, Num, Val) 
#define EVAL_STRING(Arg)          librep_eval_string(Arg)
#define MAKE_VECTOR(Num, Fill)    rep_make_vector(Num)
#define VECTOR_TO_LIST(a)         Qnil
#define CHAR_P(Arg)               ((Arg) && (STRING_P(Arg)))
#define TO_C_CHAR(Arg)            (TO_C_STRING(Arg))[0]
#define SND_ASSERT_SND(Origin, Snd, Offset)
#define SND_ASSERT_CHAN(Origin, Snd, Chn, Offset)
#define CALL0(Func, Caller)       rep_call_lisp0(Func)
#define CALL1(Func, Arg1, Caller) rep_call_lisp1(Func, Arg1)
#define CALL2(Func, Arg1, Arg2, Caller) rep_call_lisp2(Func, Arg1, Arg2)
#define CALL3(Func, Arg1, Arg2, Arg3, Caller) rep_call_lisp3(Func, Arg1, Arg2, Arg3)
#define APPLY(Func, Args, Caller) rep_apply(Func, Args)
#define APPLY_EOL                 Qnil
#define ARITY(Func)               Qnil
#define KEYWORD_P(Obj)            ((Obj) && (rep_KEYWORDP(Obj)))
#define MAKE_KEYWORD(Arg)         Fmake_keyword(TO_SCM_SYMBOL(Arg))
#define CLEAR_HOOK(a)
#define YES_WE_HAVE(a)            Fprovide(TO_SCM_SYMBOL(a))
#define DOCUMENTATION             Qnil
#define ASSERT_TYPE(Assertion, Arg, Position, Caller, Correct_Type)
#define REVERSE_LIST(a)           Freverse(a)
#define MAKE_PERMANENT(a)         a
#define LOAD_SCM_FILE(a)          Fload(TO_SCM_STRING(a), Qnil, Qnil, Qnil, Qnil)
#define WRITE_STRING(a, b)        rep_princ_val(Qstandard_output, TO_SCM_STRING(a))

#endif
