#ifndef SG_H
#define SG_H
/* macros useful in all the files using guile */

#if (HAVE_GUILE) && (!(HAVE_GUILE_1_3_0))
  #define HAVE_GENERALIZED_SET 1
  #define HAVE_NEW_SMOB 1
  #define HAVE_KEYWORDS 1
  #define HAVE_HOOKS 1
#else
  #define HAVE_GENERALIZED_SET 0
  #define HAVE_NEW_SMOB 0
  #define HAVE_KEYWORDS 0
  #define HAVE_HOOKS 0
#endif

#if HAVE_NEW_SMOB
#define SND_RETURN_NEWSMOB(Tag,Val) SCM_RETURN_NEWSMOB(Tag,(SCM)Val)
#else
#define SND_RETURN_NEWSMOB(Tag,Val) \
  do { \
     SCM New_Cell; \
     SCM_NEWCELL(New_Cell); \
     SCM_SETCDR(New_Cell,(SCM)Val); \
     SCM_SETCAR(New_Cell,Tag); \
     return(New_Cell); \
     } while (0)
#endif

#define GH_LOOKUP(a) scm_symbol_value0(a)
#define GH_TYPE_OF(a) (SCM_TYP16(a))

#if HAVE_NEW_SMOB
  #define GH_VALUE_OF(a) SCM_SMOB_DATA(a)
  #define GH_SET_VALUE_OF(a,b) SCM_SET_SMOB_DATA(a,b)
#else
  #define GH_VALUE_OF(a) gh_cdr(a)
  #define GH_SET_VALUE_OF(a,b) SCM_SETCDR(a,b)
#endif

#ifndef SCM_EQ_P
  #define SCM_EQ_P(a,b) ((a) == (b))
  /* SCM_UNPACK used here in later Guile's */
#endif

#ifndef SCM_TRUE_P
  #define SCM_TRUE_P(a) SCM_EQ_P(a,SCM_BOOL_T)
#endif

#ifdef __cplusplus
  #define SCM_FNC (SCM (*)(...))
#else
  #define SCM_FNC
#endif

#define ERRB1(a,b) SCM_ASSERT((gh_number_p(a)) || (gh_boolean_p(a)) || (SCM_UNBNDP(a)),a,SCM_ARG1,b)
#define ERRB2(a,b) SCM_ASSERT((gh_number_p(a)) || (gh_boolean_p(a)) || (SCM_UNBNDP(a)),a,SCM_ARG2,b)
#define ERRB3(a,b) SCM_ASSERT((gh_number_p(a)) || (gh_boolean_p(a)) || (SCM_UNBNDP(a)),a,SCM_ARG3,b)
#define ERRB4(a,b) SCM_ASSERT((gh_number_p(a)) || (gh_boolean_p(a)) || (SCM_UNBNDP(a)),a,SCM_ARG4,b)

#define RTNBOOL(a) return((a) ? SCM_BOOL_T : SCM_BOOL_F)
#define RTNINT(a) return(gh_int2scm(a))
#define RTNFLT(a) return(gh_double2scm(a))
#define RTNSTR(a) return(gh_str02scm(a))

#define HOOKED(a) (!(SCM_NULLP(SCM_HOOK_PROCEDURES(a))))

#define DEFINE_PROC(a,b) scm_set_procedure_property_x(a,local_doc,gh_str02scm(b))

/* DEFINE_PROC(proc,doc) sets the documentation property of procedure proc to the text doc
 *   the assumption is that it will be used with gh_new_procedure and scm_string_to_symbol
 */

#define WITH_REVERSED_CHANNEL_ARGS(name_reversed,name) \
static SCM name_reversed(SCM arg1, SCM arg2, SCM arg3) \
{ \
  if (SCM_UNBNDP(arg2)) \
    return(name(arg1,SCM_UNDEFINED,SCM_UNDEFINED)); \
  else { \
    if (SCM_UNBNDP(arg3)) \
      return(name(arg2,arg1,SCM_UNDEFINED)); \
    else return(name(arg3,arg1,arg2)); \
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
#define NO_SUCH_EDIT gh_symbol2scm("no-such-edit")
#define CANNOT_SAVE gh_symbol2scm("cannot-save")
#define IMPOSSIBLE_BOUNDS gh_symbol2scm("impossible-bounds")
#define NO_ACTIVE_SELECTION gh_symbol2scm("no-active-selection")
#define MUS_MISC_ERROR gh_symbol2scm("mus-error")

#endif
