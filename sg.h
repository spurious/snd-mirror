#ifndef SG_H
#define SG_H
/* macros useful in all the files using guile */

#if (HAVE_GUILE) && (!(HAVE_GUILE_1_3_0))
  #define HAVE_GENERALIZED_SET 1
  #define HAVE_NEW_SMOB 1
  #define HAVE_KEYWORDS 1
  #define HAVE_HOOKS 1
  #define MAKE_HOOK(Name, Args) scm_create_hook(Name, Args)
#else
  #define HAVE_GENERALIZED_SET 0
  #define HAVE_NEW_SMOB 0
  #define HAVE_KEYWORDS 0
  #define HAVE_HOOKS 0
  #define MAKE_HOOK(Name, Args) gh_define(Name, SCM_BOOL_F)
#endif

#if HAVE_NEW_SMOB
  #define SND_RETURN_NEWSMOB(Tag, Val) SCM_RETURN_NEWSMOB(Tag, (SCM)Val)
  #define SND_VALUE_OF(a) SCM_SMOB_DATA(a)
  #define SND_SET_VALUE_OF(a, b) SCM_SET_SMOB_DATA(a, b)
#else
  #define SND_RETURN_NEWSMOB(Tag, Val) \
    do { \
       SCM New_Cell; \
       SCM_NEWCELL(New_Cell); \
       SCM_SETCDR(New_Cell, (SCM)Val); \
       SCM_SETCAR(New_Cell, Tag); \
       return(New_Cell); \
       } while (0)
  #define SND_VALUE_OF(a) gh_cdr(a)
  #define SND_SET_VALUE_OF(a, b) SCM_SETCDR(a, b)
#endif

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

#ifndef SCM_STRING_CHARS
  #define SCM_STRING_CHARS(STR) SCM_CHARS(STR)
#endif

#ifdef __cplusplus
/* #define SCM_FNC (SCM (*)(...)) */
  #define SCM_FNC (SCM (*)())
#else
  #define SCM_FNC
#endif

#ifdef __FUNCTION__
  #define TO_C_DOUBLE(a) scm_num2dbl(a, __FUNCTION__)
  #define TO_C_DOUBLE_WITH_ORIGIN(a, b) scm_num2dbl(a, b)
  #define TO_C_INT(a) ((int)scm_num2long(a, (char *)SCM_ARG1, __FUNCTION__))
  /* using these rather than gh_scm2double and gh_scm2int to get better error reporting */
  #define TO_C_INT_OR_ELSE(a, b) to_c_int_or_else(a, b, __FUNCTION__)
#else
  #define TO_C_DOUBLE(a) gh_scm2double(a)
  #define TO_C_DOUBLE_WITH_ORIGIN(a, b) gh_scm2double(a)
  #define TO_C_INT(a) gh_scm2int(a)
  #define TO_C_INT_OR_ELSE(a, b) to_c_int_or_else(a, b, "to_c_int_or_else")
#endif

#if HAVE_GUILE_1_3_0
  #define TO_SCM_DOUBLE(a) scm_makdbl(a, 0.0)
#else
  #define TO_SCM_DOUBLE(a) scm_make_real(a)
#endif

#define TO_SCM_INT(a) scm_long2num((long)a)
#define TO_SMALL_SCM_INT(a) SCM_MAKINUM(a)
#define TO_SMALL_C_INT(a) SCM_INUM(a)
#define TO_SCM_STRING(a) scm_makfrom0str(a)
#define TO_NEW_C_STRING(a) gh_scm2newstr(a, NULL)
#define TO_SCM_BOOLEAN(a) ((a) ? SCM_BOOL_T : SCM_BOOL_F)

#define SCM_WRAP(a) ((SCM)(gh_ulong2scm((unsigned long)a)))
#define SCM_UNWRAP(a) gh_scm2ulong(a)
/* these work as long as SCM is long */
/*   (need a way to pass an uninterpreted pointer from C to SCM then back to C) */

#define HOOKED(a) (!(SCM_NULLP(SCM_HOOK_PROCEDURES(a))))

#define DEFINE_PROC(a, b) scm_set_procedure_property_x(a, local_doc, gh_str02scm(b))

/* DEFINE_PROC(proc, doc) sets the documentation property of procedure proc to the text doc
 *   the assumption is that it will be used with gh_new_procedure and scm_string_to_symbol
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

#define NO_SUCH_CHANNEL gh_symbol2scm("no-such-channel")
#define NO_SUCH_SOUND gh_symbol2scm("no-such-sound")
#define NO_SUCH_MARK gh_symbol2scm("no-such-mark")
#define NO_SUCH_MIX gh_symbol2scm("no-such-mix")
#define NO_SUCH_TRACK gh_symbol2scm("no-such-track")
#define NO_SUCH_MENU gh_symbol2scm("no-such-menu")
#define NO_SUCH_FILE gh_symbol2scm("no-such-file")
#define NO_SUCH_REGION gh_symbol2scm("no-such-region")
#define NO_SUCH_SAMPLE gh_symbol2scm("no-such-sample")
#define NO_SUCH_ENVELOPE gh_symbol2scm("no-such-envelope")
#define NO_SUCH_EDIT gh_symbol2scm("no-such-edit")
#define CANNOT_SAVE gh_symbol2scm("cannot-save")
#define IMPOSSIBLE_BOUNDS gh_symbol2scm("impossible-bounds")
#define NO_ACTIVE_SELECTION gh_symbol2scm("no-active-selection")
#define MUS_MISC_ERROR gh_symbol2scm("mus-error")

#endif
