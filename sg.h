#ifndef SG_H
#define SG_H
/* macros useful in all the files using guile */

/* I'm slowly moving every gh_*, scm_* and SCM_* entity into this file,
 *   hoping eventually to be able to make a parallel header file that
 *   allows us to use librep as the extension language without any
 *   (or not many) internal code changes.  Perhaps other Scheme extension
 *   systems could also work.
 */

#define MAKE_HOOK(Name, Args, Help) snd_set_object_property(scm_create_hook(Name, Args), local_doc, TO_SCM_STRING(Help))
#define SND_RETURN_NEWSMOB(Tag, Val) SCM_RETURN_NEWSMOB(Tag, (SCM)Val)
#define SND_VALUE_OF(a) SCM_SMOB_DATA(a)
/* remember to check the smob type agreement before calling SND_VALUE_OF! */
#define SND_SET_VALUE_OF(a, b) SCM_SET_SMOB_DATA(a, b)
#define SND_LOOKUP(a) scm_symbol_value0(a)

#if HAVE_SCM_REMEMBER_UPTO_HERE
  #define SND_REMEMBER(OBJ) scm_remember_upto_here_1(OBJ)
  #define SND_TAG_TYPE scm_bits_t
  #define SND_SMOB_TYPE(TAG, OBJ) SCM_SMOB_PREDICATE(TAG, OBJ)
  #define SND_SETGCMARK(X) SCM_SETGCMARK(X)
#else
  #define SND_REMEMBER(OBJ) scm_remember(&OBJ)
  #define SND_TAG_TYPE long
  #define SND_SMOB_TYPE(TAG, OBJ) (SCM_TYP16(OBJ) == (SCM)TAG)
  #define SND_SETGCMARK(X) SCM_SETGC8MARK(X)
#endif

#ifndef SCM_EQ_P
  #define SCM_EQ_P(a, b) ((a) == (b))
  /* SCM_UNPACK used here in later Guile's */
#endif

#ifndef SCM_TRUE_P
  #define SCM_TRUE_P(a) SCM_EQ_P(a, SCM_BOOL_T)
#endif

#ifdef __cplusplus
  #define SCM_FNC (SCM (*)())
#else
  #define SCM_FNC
#endif

#ifndef __GNUC__
  #ifndef __FUNCTION__
    #define __FUNCTION__ ""
  #endif
#endif

#define TO_C_DOUBLE(a) scm_num2dbl(a, __FUNCTION__)
#define TO_C_DOUBLE_WITH_ORIGIN(a, b) scm_num2dbl(a, b)
#define TO_C_INT(a) ((int)scm_num2long(a, (char *)SCM_ARG1, __FUNCTION__))
/* using these rather than gh_scm2double and gh_scm2int to get better error reporting */
#define TO_C_INT_OR_ELSE(a, b) to_c_int_or_else(a, b, __FUNCTION__)

#ifndef SCM_STRING_CHARS
  #define TO_C_STRING(STR) SCM_CHARS(STR)
#else
  #define TO_C_STRING(STR) SCM_STRING_CHARS(STR)
  /* this assumes its argument is an SCM string and does not allocate new space */
#endif

#if HAVE_SCM_MAKE_REAL
  #define TO_SCM_DOUBLE(a) scm_make_real(a)
#else
  #define TO_SCM_DOUBLE(a) scm_makdbl(a, 0.0)
#endif

#define TO_SCM_INT(a) scm_long2num((long)a)
#define TO_SMALL_SCM_INT(a) SCM_MAKINUM(a)
#define TO_SMALL_C_INT(a) SCM_INUM(a)
#define TO_C_UNSIGNED_LONG(a) gh_scm2ulong(a)
#define TO_SCM_STRING(a) scm_makfrom0str(a)
#define TO_NEW_C_STRING(a) gh_scm2newstr(a, NULL)
#define TO_SCM_BOOLEAN(a) ((a) ? SCM_BOOL_T : SCM_BOOL_F)
#define TO_SCM_SYMBOL(a) gh_symbol2scm(a)
#define TO_C_BOOLEAN_OR_T(a) ((SCM_FALSEP(a) || ((SCM_INUMP(a)) && (SCM_INUM(a) == 0))) ? 0 : 1)
#define TO_C_BOOLEAN(a) ((SCM_FALSEP(a)) ? 0 : 1)


#define SND_WRAP(a) ((SCM)(gh_ulong2scm((unsigned long)a)))
#define SND_UNWRAP(a) gh_scm2ulong(a)
#define SND_WRAPPED(a) gh_number_p(a)
/* these work as long as SCM is long */
/*   (need a way to pass an uninterpreted pointer from C to SCM then back to C) */

#define HOOKED(a) (!(SCM_NULLP(SCM_HOOK_PROCEDURES(a))))

#define DEFINE_PROC(Name, Func, ReqArg, OptArg, RstArg, Doc) \
  scm_set_procedure_property_x(gh_new_procedure(Name, SCM_FNC Func, ReqArg, OptArg, RstArg), local_doc, gh_str02scm(Doc))

#define DEFINE_VAR(a, b, c) \
  { \
    gh_define(a, b); \
    scm_set_object_property_x(TO_SCM_SYMBOL(a), local_doc, TO_SCM_STRING(c)); \
  }

/* DEFINE_PROC sets the documentation property of procedure Func to Doc
 * DEFINE_VAR sets the symbol's documentation property (gh_define returns the value) 
 */

#define WITH_REVERSED_CHANNEL_ARGS(name_reversed, name) \
static SCM name_reversed(SCM arg1, SCM arg2, SCM arg3) \
{ \
  if (SCM_UNBNDP(arg2)) \
    return(name(arg1, SCM_UNDEFINED, SCM_UNDEFINED)); \
  else { \
    if (SCM_UNBNDP(arg3)) \
      return(name(arg2, arg1, SCM_UNDEFINED)); \
    else return(name(arg3, arg1, arg2)); \
}}

/* error indications */

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

#define SND_ASSERT_SND(Origin, Snd, Offset) \
  if (!((SCM_NFALSEP(scm_integer_p(Snd))) || (SCM_FALSEP(Snd)) || (SCM_UNBNDP(Snd)) || (gh_list_p(Snd)))) \
    scm_wrong_type_arg(Origin, Offset, Snd);

#define SND_ASSERT_CHAN(Origin, Snd, Chn, Offset) \
  if (!((SCM_NFALSEP(scm_integer_p(Snd))) || (SCM_FALSEP(Snd)) || (SCM_UNBNDP(Snd)) || (gh_list_p(Snd)))) \
    scm_wrong_type_arg(Origin, Offset, Snd); \
  else \
    if (!((SCM_NFALSEP(scm_integer_p(Chn))) || (SCM_FALSEP(Chn)) || (SCM_UNBNDP(Chn)))) \
      scm_wrong_type_arg(Origin, Offset + 1, Chn);

#ifndef SCM_BOOLP
  #define SCM_BOOLP(Arg) gh_boolean_p(Arg)
#endif
#ifndef SCM_STRINGP
  #define SCM_STRINGP(Arg) gh_string_p(Arg)
#endif
#ifndef SCM_VECTORP
  #define SCM_VECTOR_P(Arg) gh_vector_p(Arg)
#endif

#define BOOLEAN_IF_BOUND_P(Arg) ((SCM_BOOLP(Arg)) || (SCM_UNBNDP(Arg)))
#define INTEGER_IF_BOUND_P(Arg) ((SCM_UNBNDP(Arg)) || (SCM_NFALSEP(scm_integer_p(Arg))))
#define NUMBER_IF_BOUND_P(Arg) ((SCM_UNBNDP(Arg)) || (SCM_NFALSEP(scm_real_p(Arg))))
#define INTEGER_OR_BOOLEAN_IF_BOUND_P(Arg) ((SCM_BOOLP(Arg)) || (SCM_UNBNDP(Arg)) || (SCM_NFALSEP(scm_integer_p(Arg))))
#define NUMBER_OR_BOOLEAN_IF_BOUND_P(Arg) ((SCM_BOOLP(Arg)) || (SCM_UNBNDP(Arg)) || (SCM_NFALSEP(scm_real_p(Arg))))
#define NUMBER_OR_BOOLEAN_P(Arg) ((SCM_BOOLP(Arg)) || (SCM_NFALSEP(scm_real_p(Arg))))
#define INTEGER_OR_BOOLEAN_P(Arg) ((SCM_BOOLP(Arg)) || (SCM_NFALSEP(scm_integer_p(Arg))))
#define NUMBER_P(Arg) (SCM_NFALSEP(scm_real_p(Arg)))
#define INTEGER_P(Arg) (SCM_NFALSEP(scm_integer_p(Arg)))
#define BOOLEAN_P(Arg) (SCM_BOOLP(Arg))
#define BOUND_P(Arg) (!(SCM_UNBNDP(Arg)))
#define SYMBOL_P(Arg) (SCM_SYMBOLP(Arg))
#define STRING_P(Arg) (SCM_STRINGP(Arg))
#define VECTOR_P(Arg) (SCM_VECTORP(Arg))

#endif
