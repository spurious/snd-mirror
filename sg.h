#ifndef SG_H
#define SG_H
/* macros useful in all the files using guile */

#define GH_LOOKUP(a) scm_symbol_value0(a)
#define GH_TYPE_OF(a) (SCM_TYP16(a))
#define GH_INT_SET(name,val) SCM_SETCDR(name,gh_int2scm(val))

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
#define ERRS1(a,b) SCM_ASSERT((gh_string_p(a)),a,SCM_ARG1,b)
#define ERRVCT1(a,b) SCM_ASSERT((vct_p(a)),a,SCM_ARG1,b)
#define ERRVCT2(a,b) SCM_ASSERT((vct_p(a)),a,SCM_ARG2,b)

#define RTNBOOL(a) return((a) ? SCM_BOOL_T : SCM_BOOL_F)
#define RTNINT(a) return(gh_int2scm(a))
#define RTNFLT(a) return(gh_double2scm(a))
#define RTNSTR(a) return(gh_str02scm(a))

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

#endif
