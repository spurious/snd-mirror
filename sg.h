#ifndef SG_H
#define SG_H
/* macros useful in all the files using guile */

#define GH_LOOKUP(a) scm_symbol_value0(a)
#define GH_TYPE_OF(a) (SCM_TYP16(a))

#if (!HAVE_GUILE_1_3_0)
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

#define ERRN1(a,b) SCM_ASSERT(SCM_NFALSEP(scm_real_p(a)),a,SCM_ARG1,b)
#define ERRN2(a,b) SCM_ASSERT(SCM_NFALSEP(scm_real_p(a)),a,SCM_ARG2,b)
#define ERRN3(a,b) SCM_ASSERT(SCM_NFALSEP(scm_real_p(a)),a,SCM_ARG3,b)
#define ERRN4(a,b) SCM_ASSERT(SCM_NFALSEP(scm_real_p(a)),a,SCM_ARG4,b)

#define ERRS1(a,b) SCM_ASSERT((gh_string_p(a)),a,SCM_ARG1,b)
#define ERRS2(a,b) SCM_ASSERT((gh_string_p(a)),a,SCM_ARG2,b)
#define ERRS3(a,b) SCM_ASSERT((gh_string_p(a)),a,SCM_ARG3,b)

#define ERRVCT1(a,b) SCM_ASSERT((vct_p(a)),a,SCM_ARG1,b)
#define ERRVCT2(a,b) SCM_ASSERT((vct_p(a)),a,SCM_ARG2,b)

#define ERRB1(a,b) SCM_ASSERT((gh_number_p(a)) || (gh_boolean_p(a)) || (SCM_UNBNDP(a)),a,SCM_ARG1,b)
#define ERRB2(a,b) SCM_ASSERT((gh_number_p(a)) || (gh_boolean_p(a)) || (SCM_UNBNDP(a)),a,SCM_ARG2,b)
#define ERRB3(a,b) SCM_ASSERT((gh_number_p(a)) || (gh_boolean_p(a)) || (SCM_UNBNDP(a)),a,SCM_ARG3,b)
#define ERRB4(a,b) SCM_ASSERT((gh_number_p(a)) || (gh_boolean_p(a)) || (SCM_UNBNDP(a)),a,SCM_ARG4,b)

#define ERRV1(a,b) SCM_ASSERT(((vct_p(a)) || (gh_vector_p(a))),a,SCM_ARG1,b)
#define ERRV2(a,b) SCM_ASSERT(((vct_p(a)) || (gh_vector_p(a))),a,SCM_ARG2,b)
#define ERRV3(a,b) SCM_ASSERT(((vct_p(a)) || (gh_vector_p(a))),a,SCM_ARG3,b)

#define ERRVECT1(a,b) SCM_ASSERT((gh_vector_p(a)),a,SCM_ARG1,b)
#define ERRVECT2(a,b) SCM_ASSERT((gh_vector_p(a)),a,SCM_ARG2,b)
#define ERRVECT4(a,b) SCM_ASSERT((gh_vector_p(a)),a,SCM_ARG4,b)

#define ERRSB1(a,b) SCM_ASSERT(((gh_string_p(a)) || (SCM_FALSEP(a)) || (SCM_UNBNDP(a))),a,SCM_ARG1,b)
#define ERRSB3(a,b) SCM_ASSERT(((gh_string_p(a)) || (SCM_FALSEP(a)) || (SCM_UNBNDP(a))),a,SCM_ARG3,b)

#define RTNBOOL(a) return((a) ? SCM_BOOL_T : SCM_BOOL_F)
#define RTNINT(a) return(gh_int2scm(a))
#define RTNFLT(a) return(gh_double2scm(a))
#define RTNSTR(a) return(gh_str02scm(a))

#define HOOKED(a) (!(SCM_NULLP(SCM_HOOK_PROCEDURES(a))))

#define DEFINE_PROC(a,b) scm_set_procedure_property_x(a,local_doc,gh_str02scm(b))

/* DEFINE_PROC(proc,doc) sets the documentation property of procedure proc to the text doc
 *   the assumption is that it will be used with gh_new_procedure and scm_string_to_symbol
 */

/*
 * this sets the documentation property of a variable (the symbol, not the variable's value):
 *   #define DEFINE_OBJ(name,obj,text) {gh_define(name,obj); scm_set_object_property_x(scm_string_to_symbol(gh_str02scm(name)),local_doc,gh_str02scm(text));}
 * but it's inconsistent with the procedure property above where the "value" has the property, not the symbol
 * I'm not sure which is "the right thing":  (help vct?) or (help 'vct?)
 * the problem with setting the symbol's property is that there apparently isn't a Scheme version of defconstant
 */

/* error indications (probably temporary) */

#define NO_SUCH_CHANNEL gh_symbol2scm("no-such-channel")
#define NO_SUCH_SOUND gh_symbol2scm("no-such-sound")
#define NO_SUCH_MARK gh_symbol2scm("no-such-mark")
#define NO_SUCH_MIX gh_symbol2scm("no-such-mix")
#define NO_SUCH_MENU gh_symbol2scm("no-such-menu")
#define NO_SUCH_FILE gh_symbol2scm("no-such-file")
#define NO_SUCH_REGION gh_symbol2scm("no-such-region")
#define NO_SUCH_SAMPLE gh_symbol2scm("no-such-sample")
#define NO_SUCH_EDIT gh_symbol2scm("no-such-edit")
#define CANNOT_SAVE gh_symbol2scm("cannot-save")
#define IMPOSSIBLE_BOUNDS gh_symbol2scm("impossible-bounds")
#define NO_ACTIVE_SELECTION gh_symbol2scm("no-active-selection")


#endif
