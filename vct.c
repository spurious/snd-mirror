/* vct support 
 *
 * a vct is a Guile "smob" containing a Float array and its size
 * we use these in Snd because Guile's floating-point and vector operations are slow,
 * and Snd/CLM are applications where no user is willing to wait on a multiply.
 *
 * C side:
 *   void init_vct(void)                   called to declare the various functions and the vct type in Guile
 *   int vct_p(SCM obj)                    is obj a vct
 *   SCM make_vct(int len, Float *data)    make a new vct
 *   vct *get_vct(SCM arg)                 given SCM arg, return vct object
 *   void set_vct_print_length(int val)    set vct print length (default 10)
 *
 * Scheme side:
 *   (make-vct len)                        make new vct
 *   (vct? obj)                            is obj a vct
 *   (vct-ref v index)                     return v[index]
 *   (vct-set! v index val)                v[index] = val
 *   (vct-copy v)                          return a copy of v
 *   (vct-length v)                        return length of v
 *   (vct-add! v1 v2)                      v1[i] = v1[i] + v2[i] -> v1
 *   (vct-subtract! v1 v2)                 v1[i] = v1[i] - v2[i] -> v1
 *   (vct-offset! v1 scl)                  v1[i] += scl -> v1
 *   (vct-multiply! v1 v2)                 v1[i] *= v2[i] -> v1
 *   (vct-scale! v1 scl)                   v1[i] *= scl -> v1
 *   (vct-fill! v1 val)                    v1[i] = val -> v1
 *   (vct-map! v1 proc)                    set each element of v1 to value of function proc()
 *   (vct-do! v1 proc)                     set each element of v1 to value of function proc(i)
 *   (vcts-map! v1 v2 ... proc)            set each element of each vs from values of function proc(num)
 *   (vcts-do! v1 v2 ... proc)             set each element of each vs from values of function proc(num,i)
 *   (vct-peak v1)                         max val (abs) in v
 *   (list->vct lst)                       return vct with elements of list lst
 *   (vector->vct vect)                    return vct with elements of vector vect
 *
 * The intended use is a sort of latter-day array-processing system that handles huge
 * one-dimensional vectors -- fft's, etc.  Some of these functions can be found in
 * snd-scm.c in the Snd package; others can be found in the CLM package, (clm2scm.c).
 */

#if defined(HAVE_CONFIG_H)
  #include "config.h"
#endif

#ifndef HAVE_GUILE
  #define HAVE_GUILE 1
#endif

#if USE_SND
  #include "snd.h"
#endif

#include "vct.h"
#include "sg.h"

#if HAVE_GUILE

#ifdef DEBUG_MEMORY
  #include <stdlib.h>
  #include "sndlib.h"
#endif

#ifndef CALLOC
  #define CALLOC(a,b)  calloc(a,b)
  #define MALLOC(a)    malloc(a)
  #define FREE(a)      free(a)
  #define REALLOC(a,b) realloc(a,b)
#endif

#define VCT_PRINT_LENGTH 10

#ifndef MIN
  #define MIN(a,b) ((a > b) ? (b) : (a))
#endif

static int vct_tag = 0;
static int vct_print_length = VCT_PRINT_LENGTH;
void set_vct_print_length(int len) {vct_print_length = len;}

static SCM mark_vct(SCM obj)
{
  SCM_SETGC8MARK(obj);
  return(SCM_BOOL_F);
}

int vct_p(SCM obj)
{
  return((SCM_NIMP(obj)) && (GH_TYPE_OF(obj) == (SCM)vct_tag));
}

static SCM g_vct_p(SCM obj) 
{
  #define H_vct_p "(" S_vct_p " obj) -> #t if obj is a vct object, else #f"
  RTNBOOL(vct_p(obj));
}

vct *get_vct(SCM arg)
{
  if (vct_p(arg))
    return((vct *)GH_VALUE_OF(arg));
  return(NULL);
}

static scm_sizet free_vct(SCM obj)
{
  vct *v = (vct *)GH_VALUE_OF(obj);
  if (v->data) FREE(v->data);
  v->data = NULL;
  FREE(v);
  return(0);
}

static int print_vct(SCM obj, SCM port, scm_print_state *pstate)
{
  int len,i;
  char *buf;
  vct *v = (vct *)GH_VALUE_OF(obj);
  scm_puts("#<vct",port);
  len = vct_print_length;
  if (len > v->length) len = v->length;
  if (len > 0)
    {
      buf = (char *)CALLOC(32,sizeof(char));
      for (i=0;i<len;i++)
	{
	  sprintf(buf," %.3f",v->data[i]);
	  scm_puts(buf,port);
	}
      if (v->length > vct_print_length)
	scm_puts(" ...",port);
      FREE(buf);
    }
  scm_puts(">",port);
  scm_remember(&obj); /* ?? */
  return(1);
}

static SCM equalp_vct(SCM obj1, SCM obj2)
{
  vct *v1,*v2;
  int i;
  v1 = (vct *)GH_VALUE_OF(obj1);
  v2 = (vct *)GH_VALUE_OF(obj2);
  if (v1->length != v2->length) return(SCM_BOOL_F);
  for (i=0;i<v1->length;i++)
    if (v1->data[i] != v2->data[i])
      return(SCM_BOOL_F);
  return(scm_return_first(SCM_BOOL_T,obj1,obj2));
}

SCM make_vct(int len, Float *data)
{
#if HAVE_GUILE_1_3_0
  SCM ans;
#endif
  vct *new_vct;
  new_vct = (vct *)CALLOC(1,sizeof(vct));
  new_vct->length = len;
  new_vct->data = data;
#if (!HAVE_GUILE_1_3_0)
  SCM_RETURN_NEWSMOB(vct_tag,new_vct);
#else
  SCM_NEWCELL(ans);
  SCM_SETCDR(ans,(SCM)new_vct);
  SCM_SETCAR(ans,vct_tag);
  return(ans);
#endif
}

#if HAVE_GUILE_1_3_0
static scm_smobfuns vct_smobfuns = {
  &mark_vct,
  &free_vct,
  &print_vct,
  &equalp_vct};
#endif

static SCM g_make_vct(SCM len)
{
  #define H_make_vct "(" S_make_vct " len) -> a new vct object of length len"
  int size;
  ERRN1(len,S_make_vct);
  size = gh_scm2int(len);
  if (size <= 0) scm_misc_error(S_make_vct,"size: ~S?",SCM_LIST1(len));
  return(make_vct(size,(Float *)CALLOC(size,sizeof(Float))));
}

static SCM copy_vct(SCM obj)
{
  #define H_vct_copy "(" S_vct_copy " v) -> a copy of vct v"
  vct *v;
  Float *copied_data;
  int len,i;
  ERRVCT1(obj,S_vct_copy);
  v = get_vct(obj);
  if (v)
    {
      len = v->length;
      copied_data = (Float *)CALLOC(len,sizeof(Float));
      for (i=0;i<len;i++) copied_data[i] = v->data[i];
      return(make_vct(len,copied_data));
    }
  return(scm_return_first(SCM_BOOL_F,obj));
}

static SCM vct_length(SCM obj)
{
  #define H_vct_length     "(" S_vct_length " v) -> length of vct v"
  vct *v = get_vct(obj);
  ERRVCT1(obj,S_vct_length);
  if (v)
    RTNINT(v->length);
  RTNINT(0);
}

static SCM vct_ref(SCM obj, SCM pos)
{
  #define H_vct_ref "(" S_vct_ref " v n) -> element n of vct v, v[n]"
  vct *v = get_vct(obj);
  int loc;
  ERRVCT1(obj,S_vct_ref);
  ERRN2(pos,S_vct_ref);
  if (v)
    {
      loc = gh_scm2int(pos);
      if ((loc >= 0) && (loc < v->length))
	RTNFLT(v->data[loc]);
      else scm_misc_error(S_vct_ref,"invalid index: ~S[~S]",SCM_LIST2(obj,pos));
    }
  else scm_misc_error(S_vct_ref,"nil vct?",SCM_EOL);
  RTNFLT(0.0);
}

static SCM vct_set(SCM obj, SCM pos, SCM val)
{
  #define H_vct_setB "(" S_vct_setB " v n val) sets element n of vct v to val, v[n]=val"
  vct *v = get_vct(obj);
  int loc;
  ERRVCT1(obj,S_vct_setB);
  ERRN2(pos,S_vct_setB);
  ERRN3(val,S_vct_setB);
  if (v)
    {
      loc = gh_scm2int(pos);
      if ((loc >= 0) && (loc < v->length))
	v->data[loc] = gh_scm2double(val);
      else scm_misc_error(S_vct_setB,"invalid index: ~S[~S] = ~S",SCM_LIST3(obj,pos,val));
    }
  else scm_misc_error(S_vct_setB,"nil vct?",SCM_EOL);
  return(val);
}

static SCM vct_multiply(SCM obj1, SCM obj2)
{
  #define H_vct_multiplyB "(" S_vct_multiplyB " v1 v2) -> v1 with element-wise multiply of vcts v1 and v2:\n   v1[i] *= v2[i]"
  int i,lim;
  vct *v1,*v2;
  ERRVCT1(obj1,S_vct_multiplyB);
  ERRVCT2(obj2,S_vct_multiplyB);
  v1 = get_vct(obj1);
  v2 = get_vct(obj2);
  if ((v1) && (v2))
    {
      lim = MIN(v1->length,v2->length);
      for (i=0;i<lim;i++) v1->data[i] *= v2->data[i];
    }
  return(scm_return_first(obj1,obj2)); /* I wonder if this is necessary */
}

static SCM vct_add(SCM obj1, SCM obj2)
{
  #define H_vct_addB "(" S_vct_addB " v1 v2) -> v1 with element-wise add of vcts v1 and v2:\n   v1[i] += v2[i]"
  int i,lim;
  vct *v1,*v2;
  ERRVCT1(obj1,S_vct_addB);
  ERRVCT2(obj2,S_vct_addB);
  v1 = get_vct(obj1);
  v2 = get_vct(obj2);
  if ((v1) && (v2))
    {
      lim = MIN(v1->length,v2->length);
      for (i=0;i<lim;i++) v1->data[i] += v2->data[i];
    }
  return(scm_return_first(obj1,obj2));
}

static SCM vct_subtract(SCM obj1, SCM obj2)
{
  #define H_vct_subtractB "(" S_vct_subtractB " v1 v2) -> v1 with element-wise subtract of vcts v1 and v2:\n   v1[i] -= v2[i]"
  int i,lim;
  vct *v1,*v2;
  ERRVCT1(obj1,S_vct_addB);
  ERRVCT2(obj2,S_vct_addB);
  v1 = get_vct(obj1);
  v2 = get_vct(obj2);
  if ((v1) && (v2))
    {
      lim = MIN(v1->length,v2->length);
      for (i=0;i<lim;i++) v1->data[i] -= v2->data[i];
    }
  return(scm_return_first(obj1,obj2));
}

static SCM vct_scale(SCM obj1, SCM obj2)
{
  #define H_vct_scaleB "(" S_vct_scaleB " v val) -> v with each element scaled by val:\n   v[i] *= val"
  int i;
  vct *v1;
  Float scl;
  ERRVCT1(obj1,S_vct_scaleB);
  ERRN2(obj2,S_vct_scaleB);
  v1 = get_vct(obj1);
  scl = gh_scm2double(obj2);
  if (v1)
    for (i=0;i<v1->length;i++) v1->data[i] *= scl;
  return(scm_return_first(obj1,obj2));
}

static SCM vct_offset(SCM obj1, SCM obj2)
{
  #define H_vct_offsetB "(" S_vct_offsetB " v val) -> v with val added to each element:\n   v[i] += val"
  int i;
  vct *v1;
  Float scl;
  ERRVCT1(obj1,S_vct_offsetB);
  ERRN2(obj2,S_vct_offsetB);
  v1 = get_vct(obj1);
  scl = gh_scm2double(obj2);
  if (v1)
    for (i=0;i<v1->length;i++) v1->data[i] += scl;
  return(scm_return_first(obj1,obj2));
}

static SCM vct_fill(SCM obj1, SCM obj2)
{
  #define H_vct_fillB "(" S_vct_fillB " v val) -> v with each element set to val:\n   v[i] = val"
  int i;
  vct *v1;
  Float scl;
  ERRVCT1(obj1,S_vct_fillB);
  ERRN2(obj2,S_vct_fillB);
  v1 = get_vct(obj1);
  scl = gh_scm2double(obj2);
  if (v1)
    for (i=0;i<v1->length;i++) v1->data[i] = scl;
  return(scm_return_first(obj1,obj2));
}

static SCM vct_map(SCM obj, SCM proc)
{
  #define H_vct_mapB "(" S_vct_mapB " v proc) -> v with each element set to value of proc:\n   v[i] = (proc)"
  int i;
  vct *v;
  ERRVCT1(obj,S_vct_mapB);
  SCM_ASSERT((gh_procedure_p(proc)),proc,SCM_ARG2,S_vct_mapB);
  v = get_vct(obj);
#if USE_SND
  if (v) for (i=0;i<v->length;i++) v->data[i] = gh_scm2double(g_call0(proc));
#else
  if (v) for (i=0;i<v->length;i++) v->data[i] = gh_scm2double(gh_call0(proc));
#endif
  return(obj);
}

static SCM vct_do(SCM obj, SCM proc)
{
  #define H_vct_doB "(" S_vct_doB " v proc) -> v with each element set to value of proc:\n   v[i] = (proc i)"
  int i;
  vct *v;
  ERRVCT1(obj,S_vct_doB);
  SCM_ASSERT((gh_procedure_p(proc)),proc,SCM_ARG2,S_vct_doB);
  v = get_vct(obj);
#if USE_SND
  if (v) for (i=0;i<v->length;i++) v->data[i] = gh_scm2double(g_call1(proc,gh_int2scm(i)));
#else
  if (v) for (i=0;i<v->length;i++) v->data[i] = gh_scm2double(gh_call1(proc,gh_int2scm(i)));
#endif
  return(obj);
}

static SCM vcts_map(SCM args)
{
  #define H_vcts_mapB "(" S_vcts_mapB " v1 v2 ... proc) sets each element of the vct objects from\n   the list of values returned by (proc)"
  /* n vcts followed by proc, proc returns n values (list) on each call */
  int i,vi,vnum,vsize,argnum;
  vct **v;
  SCM proc,arg,svi;
  argnum = gh_length(args);
  vnum = argnum-1;
  if (vnum <= 0)
    {
      scm_misc_error(S_vcts_mapB,"not enough args: ~S",SCM_LIST1(args));
      return(gh_int2scm(0));
    }
  v = (vct **)CALLOC(vnum,sizeof(vct *));
  for (i=0;i<vnum;i++)
    {
      arg = gh_list_ref(args,gh_int2scm(i));
      if (!(vct_p(arg))) 
	{
	  scm_misc_error(S_vcts_mapB,"in ~S, non-vct argument ~S at position ~S",SCM_LIST3(args,arg,gh_int2scm(i)));
	  return(gh_int2scm(0));
	}
      v[i] = get_vct(arg);
    }
  proc = gh_list_ref(args,gh_int2scm(vnum));
  if (!(gh_procedure_p(proc)))
    {
      scm_misc_error(S_vcts_mapB,"in ~S, last argument must be a function",SCM_LIST1(args));
      FREE(v);
      return(gh_int2scm(0));
    }
  svi = gh_int2scm(vnum);
  vsize = v[0]->length;
  for (i=1;i<vnum;i++) if (vsize > v[i]->length) vsize = v[i]->length;
  for (i=0;i<vsize;i++)
    {
#if USE_SND
      arg = g_call1(proc,svi);
#else
      arg = gh_call1(proc,svi);
#endif
      if (gh_list_p(arg))
	{
	  for (vi=0;vi<vnum;vi++)
	    v[vi]->data[i] = gh_scm2double(gh_list_ref(arg,gh_int2scm(vi)));
	}
    }
  FREE(v);
  return(gh_int2scm(vnum));
}

static SCM vcts_do(SCM args)
{
  #define H_vcts_doB "(" S_vcts_doB " v1 v2 ... proc) sets each element of the vct objects from\n   the list of values returned by (proc i)"
  /* n vcts followed by proc, proc returns n values (list) on each call */
  int i,vi,vnum,vsize,argnum;
  vct **v;
  SCM proc,arg,svi;
  argnum = gh_length(args);
  vnum = argnum-1;
  if (vnum <= 0)
    {
      scm_misc_error(S_vcts_doB,"not enough args: ~S",SCM_LIST1(args));
      return(gh_int2scm(0));
    }
  v = (vct **)CALLOC(vnum,sizeof(vct *));
  for (i=0;i<vnum;i++)
    {
      arg = gh_list_ref(args,gh_int2scm(i));
      if (!(vct_p(arg))) 
	{
	  scm_misc_error(S_vcts_doB,"in ~S, non-vct argument ~S at position ~S",SCM_LIST3(args,arg,gh_int2scm(i)));
	  return(gh_int2scm(0));
	}
      v[i] = get_vct(arg);
    }
  proc = gh_list_ref(args,gh_int2scm(vnum));
  if (!(gh_procedure_p(proc)))
    {
      scm_misc_error(S_vcts_doB,"in ~S, last argument must be a function",SCM_LIST1(args));
      FREE(v);
      return(gh_int2scm(0));
    }
  vsize = v[0]->length;
  svi = gh_int2scm(vnum);
  for (i=1;i<vnum;i++) if (vsize > v[i]->length) vsize = v[i]->length;
  for (i=0;i<vsize;i++)
    {
#if USE_SND
      arg = g_call2(proc,svi,gh_int2scm(i));
#else
      arg = gh_call2(proc,svi,gh_int2scm(i));
#endif
      if (gh_list_p(arg))
	{
	  for (vi=0;vi<vnum;vi++)
	    v[vi]->data[i] = gh_scm2double(gh_list_ref(arg,gh_int2scm(vi)));
	}
    }
  FREE(v);
  return(gh_int2scm(vnum));
}

static SCM vct_peak(SCM obj)
{
  #define H_vct_peak "(" S_vct_peak " v) -> max of abs of elements of v"
  int i;
  Float val=0.0,absv;
  vct *v;
  ERRVCT1(obj,S_vct_peak);
  v = get_vct(obj);
  if (v) 
    {
      val = fabs(v->data[0]); 
      for (i=1;i<v->length;i++) 
	{
	  absv = fabs(v->data[i]); 
	  if (absv > val) val = absv;
	}
    }
  return(scm_return_first(gh_double2scm(val),obj));
}

static SCM list2vct(SCM lst)
{
  #define H_list2vct "(" S_list2vct " lst) -> a new vct object filled with elements of list lst"
  int len,i;
  vct *v;
  SCM scv;
  SCM_ASSERT(gh_list_p(lst),lst,SCM_ARG1,S_list2vct);
  len = gh_length(lst);
  scv = make_vct(len,(Float *)CALLOC(len,sizeof(Float)));
  v = get_vct(scv);
  for (i=0;i<len;i++) v->data[i] = (Float)gh_scm2double(scm_list_ref(lst,gh_int2scm(i)));
  return(scv);
}

static SCM vector2vct(SCM vect)
{
  #define H_vector2vct "(" S_vector2vct " vect) -> a new vct object with the elements of vector vect"
  int len,i;
  vct *v;
  SCM scv;
  SCM_ASSERT(gh_vector_p(vect),vect,SCM_ARG1,S_vector2vct);
  len = gh_vector_length(vect);
  scv = make_vct(len,(Float *)CALLOC(len,sizeof(Float)));
  v = get_vct(scv);
  for (i=0;i<len;i++) v->data[i] = (Float)gh_scm2double(scm_vector_ref(vect,gh_int2scm(i)));
  return(scv);
}

void init_vct(void)
{
  SCM local_doc;
#if (!HAVE_GUILE_1_3_0)
  vct_tag = scm_make_smob_type_mfpe("vct",sizeof(vct),mark_vct,free_vct,print_vct,equalp_vct);
#else
  vct_tag = scm_newsmob(&vct_smobfuns);
#endif
  local_doc = scm_permanent_object(scm_string_to_symbol(gh_str02scm("documentation")));

  DEFINE_PROC(gh_new_procedure1_0(S_make_vct,g_make_vct),H_make_vct);
  DEFINE_PROC(gh_new_procedure1_0(S_vct_copy,copy_vct),H_vct_copy);
  DEFINE_PROC(gh_new_procedure1_0(S_vct_p,g_vct_p),H_vct_p);
  DEFINE_PROC(gh_new_procedure1_0(S_list2vct,list2vct),H_list2vct);
  DEFINE_PROC(gh_new_procedure1_0(S_vector2vct,vector2vct),H_vector2vct);
  DEFINE_PROC(gh_new_procedure1_0(S_vct_length,vct_length),H_vct_length);
  DEFINE_PROC(gh_new_procedure2_0(S_vct_ref,vct_ref),H_vct_ref);
  DEFINE_PROC(gh_new_procedure3_0(S_vct_setB,vct_set),H_vct_setB);
  DEFINE_PROC(gh_new_procedure2_0(S_vct_multiplyB,vct_multiply),H_vct_multiplyB);
  DEFINE_PROC(gh_new_procedure2_0(S_vct_scaleB,vct_scale),H_vct_scaleB);
  DEFINE_PROC(gh_new_procedure2_0(S_vct_fillB,vct_fill),H_vct_fillB);
  DEFINE_PROC(gh_new_procedure2_0(S_vct_addB,vct_add),H_vct_addB);
  DEFINE_PROC(gh_new_procedure2_0(S_vct_subtractB,vct_subtract),H_vct_subtractB);
  DEFINE_PROC(gh_new_procedure2_0(S_vct_offsetB,vct_offset),H_vct_offsetB);
  DEFINE_PROC(gh_new_procedure2_0(S_vct_mapB,vct_map),H_vct_mapB);
  DEFINE_PROC(gh_new_procedure2_0(S_vct_doB,vct_do),H_vct_doB);
  DEFINE_PROC(gh_new_procedure1_0(S_vct_peak,vct_peak),H_vct_peak);
  DEFINE_PROC(gh_new_procedure(S_vcts_mapB,SCM_FNC vcts_map,0,0,1),H_vcts_mapB);
  DEFINE_PROC(gh_new_procedure(S_vcts_doB,SCM_FNC vcts_do,0,0,1),H_vcts_doB);
  scm_add_feature("vct");
}
#endif
