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
 *   SCM make_vct_wrapper(int len, Float *data) make a new vct that doesn't free data when garbage collector strikes
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
 *   (vct-add! v1 v2 &opt (offset 0))      v1[i+offset] = v1[i+offset] + v2[i] -> v1
 *   (vct-subtract! v1 v2)                 v1[i] = v1[i] - v2[i] -> v1
 *   (vct-offset! v1 scl)                  v1[i] += scl -> v1
 *   (vct-multiply! v1 v2)                 v1[i] *= v2[i] -> v1
 *   (vct-scale! v1 scl)                   v1[i] *= scl -> v1
 *   (vct-fill! v1 val)                    v1[i] = val -> v1
 *   (vct-map! v1 proc)                    set each element of v1 to value of function proc()
 *   (vct-do! v1 proc)                     set each element of v1 to value of function proc(i)
 *   (vcts-map! v1 v2 ... proc)            set each element of each vs from values of function proc(num)
 *   (vcts-do! v1 v2 ... proc)             set each element of each vs from values of function proc(num, i)
 *   (vct-peak v1)                         max val (abs) in v
 *   (list->vct lst)                       return vct with elements of list lst
 *   (vct->list v1)                        return list with elements of vct v1
 *   (vector->vct vect)                    return vct with elements of vector vect
 *   (vct-move! v new old)                 v[new++]=v[old++] -> v
 *   (vct-subseq v start end &opt vnew)    vnew = v[start..end]
 *
 * The intended use is a sort of latter-day array-processing system that handles huge
 * one-dimensional vectors -- fft's, etc.  Some of these functions can be found in
 * snd-scm.c in the Snd package; others can be found in the CLM package, (clm2scm.c).
 */

/* TODO  offsets for vct-*?
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
  #define CALLOC(a, b)  calloc(a, b)
  #define MALLOC(a)    malloc(a)
  #define FREE(a)      free(a)
  #define REALLOC(a, b) realloc(a, b)
#endif

#define VCT_PRINT_LENGTH 10

#ifndef MIN
  #define MIN(a, b) ((a > b) ? (b) : (a))
#endif

static SND_TAG_TYPE vct_tag = 0;
static int vct_print_length = VCT_PRINT_LENGTH;
void set_vct_print_length(int len) {vct_print_length = len;}

static SCM mark_vct(SCM obj)
{
  SCM_SETGC8MARK(obj);
  return(SCM_BOOL_F);
}

int vct_p(SCM obj)
{
  return((SCM_NIMP(obj)) && 
	 (SND_SMOB_TYPE(vct_tag, obj)));
}

static SCM g_vct_p(SCM obj) 
{
  #define H_vct_p "(" S_vct_p " obj) -> #t if obj is a vct object, else #f"
  return(TO_SCM_BOOLEAN(vct_p(obj)));
}

vct *get_vct(SCM arg)
{
  if (vct_p(arg))
    return((vct *)SND_VALUE_OF(arg));
  return(NULL);
}

static scm_sizet free_vct(SCM obj)
{
  vct *v = (vct *)SND_VALUE_OF(obj);
  if (v)
    {
      if ((v->dont_free == 0) && 
	  (v->data)) 
	FREE(v->data);
      v->data = NULL;
      FREE(v);
      SND_SET_VALUE_OF(obj, (SCM)NULL);
    }
  return(0);
}

static int print_vct(SCM obj, SCM port, scm_print_state *pstate)
{
  int len,i;
  char *buf;
  vct *v = (vct *)SND_VALUE_OF(obj);
  buf = (char *)CALLOC(64, sizeof(char));
  sprintf(buf, "#<vct[len=%d]:", v->length);
  scm_puts(buf, port);
  len = vct_print_length;
  if (len > v->length) len = v->length;
  if (len > 0)
    {
      for (i=0;i<len;i++)
	{
	  sprintf(buf, " %.3f", v->data[i]);
	  scm_puts(buf, port);
	}
      if (v->length > vct_print_length)
	scm_puts(" ...", port);
    }
  FREE(buf);
  scm_puts(">", port);
  SND_REMEMBER(obj); /* ?? */
  return(1);
}

static SCM equalp_vct(SCM obj1, SCM obj2)
{
  vct *v1,*v2;
  int i;
  v1 = (vct *)SND_VALUE_OF(obj1);
  v2 = (vct *)SND_VALUE_OF(obj2);
  if (v1->length != v2->length) 
    return(SCM_BOOL_F);
  for (i=0;i<v1->length;i++)
    if (v1->data[i] != v2->data[i])
      return(SCM_BOOL_F);
  return(scm_return_first(SCM_BOOL_T, obj1, obj2));
}

SCM make_vct(int len, Float *data)
{
  vct *new_vct;
  new_vct = (vct *)CALLOC(1, sizeof(vct));
  new_vct->length = len;
  new_vct->data = data;
  new_vct->dont_free = 0;
  SND_RETURN_NEWSMOB(vct_tag, new_vct);
}

SCM make_vct_wrapper(int len, Float *data)
{
  vct *new_vct;
  new_vct = (vct *)CALLOC(1, sizeof(vct));
  new_vct->length = len;
  new_vct->data = data;
  new_vct->dont_free = 1;
  SND_RETURN_NEWSMOB(vct_tag, new_vct);
}

#if (!(HAVE_NEW_SMOB))
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
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(len)), len, SCM_ARG1, S_make_vct);
  size = TO_C_INT(len);
  if (size <= 0) 
    {
      scm_out_of_range(S_make_vct, len);
      return(SCM_EOL);
    }
  return(scm_return_first(make_vct(size,
				   (Float *)CALLOC(size, sizeof(Float))),
			  len));
}

static SCM copy_vct(SCM obj)
{
  #define H_vct_copy "(" S_vct_copy " v) -> a copy of vct v"
  vct *v;
  Float *copied_data;
  int len,i;
  SCM_ASSERT(vct_p(obj), obj, SCM_ARG1, S_vct_copy);
  v = get_vct(obj);
  if (v)
    {
      len = v->length;
      copied_data = (Float *)CALLOC(len, sizeof(Float));
      for (i=0;i<len;i++) 
	copied_data[i] = v->data[i];
      return(make_vct(len, copied_data));
    }
  return(scm_return_first(SCM_BOOL_F, obj));
}

static SCM vct_move(SCM obj, SCM newi, SCM oldi, SCM backwards)
{
  #define H_vct_moveB "(" S_vct_moveB " obj new old backwards) moves obj data from old to new"
  vct *v;
  int i,j,ni,nj;
  SCM_ASSERT(vct_p(obj), obj, SCM_ARG1, S_vct_moveB);
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(newi)), newi, SCM_ARG2, S_vct_moveB);
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(oldi)), oldi, SCM_ARG3, S_vct_moveB);
  v = get_vct(obj);
  ni = SCM_INUM(newi);
  nj = SCM_INUM(oldi);
  if ((gh_boolean_p(backwards)) && 
      (SCM_NFALSEP(backwards)))
    {
      if (ni >= v->length) 
	scm_misc_error(S_vct_moveB,
		       "new-index: ~A (len: ~A)?",
		       SCM_LIST2(newi,
				 TO_SCM_INT(v->length)));
      if (nj >= v->length)
	scm_misc_error(S_vct_moveB,
		       "old-index: ~A (len: ~A)?",
		       SCM_LIST2(oldi,
				 TO_SCM_INT(v->length)));
      if (v) for (i=ni, j=nj;(j>=0) && (i>=0);i--,j--) 
	v->data[i] = v->data[j];
    }
  else
    {
      if (ni < 0)
	scm_misc_error(S_vct_moveB,
		       "new-index: ~A?",
		       SCM_LIST1(newi));
      if (nj < 0)
	scm_misc_error(S_vct_moveB,
		       "old-index: ~A?",
		       SCM_LIST1(oldi));
      if (v) for (i=ni, j=nj;(j<v->length) && (i<v->length);i++,j++) 
	v->data[i] = v->data[j];
    }
  return(obj);
}

static SCM vct_length(SCM obj)
{
  #define H_vct_length     "(" S_vct_length " v) -> length of vct v"
  vct *v = get_vct(obj);
  SCM_ASSERT(vct_p(obj), obj, SCM_ARG1, S_vct_length);
  if (v)
    return(TO_SCM_INT(v->length));
  return(TO_SMALL_SCM_INT(0));
}

static SCM vct_ref(SCM obj, SCM pos)
{
  #define H_vct_ref "(" S_vct_ref " v n) -> element n of vct v, v[n]"
  vct *v = get_vct(obj);
  int loc;
  SCM_ASSERT(vct_p(obj), obj, SCM_ARG1, S_vct_ref);
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(pos)), pos, SCM_ARG2, S_vct_ref);
  if (v)
    {
      loc = TO_C_INT(pos);
      if (loc < 0)
	scm_misc_error(S_vct_ref, "index: ~A?", SCM_LIST1(pos));
      else
	{
	  if (loc >= v->length)
	    scm_misc_error(S_vct_ref,
			   "index: ~A but vct length: ~A?",
			   SCM_LIST2(pos,
				     TO_SCM_INT(v->length)));
	  else return(TO_SCM_DOUBLE(v->data[loc]));
	}
    }
  else scm_misc_error(S_vct_ref, "~A is null?", SCM_LIST1(obj));
  return(TO_SCM_DOUBLE(0.0));
}

static SCM vct_set(SCM obj, SCM pos, SCM val)
{
  #define H_vct_setB "(" S_vct_setB " v n val) sets element n of vct v to val, v[n]=val"
  vct *v = get_vct(obj);
  int loc;
  SCM_ASSERT(vct_p(obj), obj, SCM_ARG1, S_vct_setB);
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(pos)), pos, SCM_ARG2, S_vct_setB);
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(val)), val, SCM_ARG3, S_vct_setB);
  if (v)
    {
      loc = TO_C_INT(pos);
      if (loc < 0)
	scm_misc_error(S_vct_setB, "index: ~A?", SCM_LIST1(pos));
      else
	{
	  if (loc >= v->length)
	    scm_misc_error(S_vct_setB,
			   "index: ~A but vct length: ~A?",
			   SCM_LIST2(pos,
				     TO_SCM_INT(v->length)));
	  else v->data[loc] = TO_C_DOUBLE(val);
	}
    }
  else scm_misc_error(S_vct_ref, "~A is null?", SCM_LIST1(obj));
  return(scm_return_first(val, obj, pos));
}

static SCM vct_multiply(SCM obj1, SCM obj2)
{
  #define H_vct_multiplyB "(" S_vct_multiplyB " v1 v2) -> v1 with element-wise multiply of vcts v1 and v2:\n   v1[i] *= v2[i]"
  int i,lim;
  vct *v1,*v2;
  SCM_ASSERT(vct_p(obj1), obj1, SCM_ARG1, S_vct_multiplyB);
  SCM_ASSERT(vct_p(obj2), obj2, SCM_ARG2, S_vct_multiplyB);
  v1 = get_vct(obj1);
  v2 = get_vct(obj2);
  if ((v1) && (v2))
    {
      lim = MIN(v1->length, v2->length);
      for (i=0;i<lim;i++) v1->data[i] *= v2->data[i];
    }
  return(scm_return_first(obj1, obj2)); /* I wonder if this is necessary */
}

static SCM vct_add(SCM obj1, SCM obj2, SCM offs)
{
  #define H_vct_addB "(" S_vct_addB " v1 v2 &optional (offset 0)) -> v1 with element-wise add of vcts v1 and v2:\n   v1[i+offset] += v2[i]"
  int i,lim,j;
  vct *v1,*v2;
  SCM_ASSERT(vct_p(obj1), obj1, SCM_ARG1, S_vct_addB);
  SCM_ASSERT(vct_p(obj2), obj2, SCM_ARG2, S_vct_addB);
  v1 = get_vct(obj1);
  v2 = get_vct(obj2);
  if ((v1) && (v2))
    {
      lim = MIN(v1->length, v2->length);
      if (SCM_INUMP(offs))
	for (i=0, j=SCM_INUM(offs);i<lim;i++,j++) 
	  v1->data[j] += v2->data[i];
      else
	for (i=0;i<lim;i++) 
	  v1->data[i] += v2->data[i];
    }
  return(scm_return_first(obj1, obj2));
}

static SCM vct_subtract(SCM obj1, SCM obj2)
{
  #define H_vct_subtractB "(" S_vct_subtractB " v1 v2) -> v1 with element-wise subtract of vcts v1 and v2:\n   v1[i] -= v2[i]"
  int i,lim;
  vct *v1,*v2;
  SCM_ASSERT(vct_p(obj1), obj1, SCM_ARG1, S_vct_subtractB);
  SCM_ASSERT(vct_p(obj2), obj2, SCM_ARG2, S_vct_subtractB);
  v1 = get_vct(obj1);
  v2 = get_vct(obj2);
  if ((v1) && (v2))
    {
      lim = MIN(v1->length, v2->length);
      for (i=0;i<lim;i++) v1->data[i] -= v2->data[i];
    }
  return(scm_return_first(obj1, obj2));
}

static SCM vct_scale(SCM obj1, SCM obj2)
{
  #define H_vct_scaleB "(" S_vct_scaleB " v val) -> v with each element scaled by val:\n   v[i] *= val"
  int i;
  vct *v1;
  Float scl;
  SCM_ASSERT(vct_p(obj1), obj1, SCM_ARG1, S_vct_scaleB);
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(obj2)), obj2, SCM_ARG2, S_vct_scaleB);
  v1 = get_vct(obj1);
  scl = TO_C_DOUBLE(obj2);
  if (v1)
    for (i=0;i<v1->length;i++) v1->data[i] *= scl;
  return(scm_return_first(obj1, obj2));
}

static SCM vct_offset(SCM obj1, SCM obj2)
{
  #define H_vct_offsetB "(" S_vct_offsetB " v val) -> v with val added to each element:\n   v[i] += val"
  int i;
  vct *v1;
  Float scl;
  SCM_ASSERT(vct_p(obj1), obj1, SCM_ARG1, S_vct_offsetB);
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(obj2)), obj2, SCM_ARG2, S_vct_offsetB);
  v1 = get_vct(obj1);
  scl = TO_C_DOUBLE(obj2);
  if (v1)
    for (i=0;i<v1->length;i++) v1->data[i] += scl;
  return(scm_return_first(obj1, obj2));
}

static SCM vct_fill(SCM obj1, SCM obj2)
{
  #define H_vct_fillB "(" S_vct_fillB " v val) -> v with each element set to val:\n   v[i] = val"
  int i;
  vct *v1;
  Float scl;
  SCM_ASSERT(vct_p(obj1), obj1, SCM_ARG1, S_vct_fillB);
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(obj2)), obj2, SCM_ARG2, S_vct_fillB);
  v1 = get_vct(obj1);
  scl = TO_C_DOUBLE(obj2);
  if (v1)
    for (i=0;i<v1->length;i++) v1->data[i] = scl;
  return(scm_return_first(obj1, obj2));
}

static SCM vct_map(SCM obj, SCM proc)
{
  #define H_vct_mapB "(" S_vct_mapB " v proc) -> v with each element set to value of proc:\n   v[i] = (proc)"
  int i;
  vct *v;
  SCM_ASSERT(vct_p(obj), obj, SCM_ARG1, S_vct_mapB);
  SCM_ASSERT((gh_procedure_p(proc)), proc, SCM_ARG2, S_vct_mapB);
  v = get_vct(obj);
#if USE_SND
  if (v) 
    for (i=0;i<v->length;i++) 
      v->data[i] = TO_C_DOUBLE(g_call0(proc));
#else
  if (v) 
    for (i=0;i<v->length;i++) 
      v->data[i] = TO_C_DOUBLE(gh_call0(proc));
#endif
  return(obj);
}

static SCM vct_do(SCM obj, SCM proc)
{
  #define H_vct_doB "(" S_vct_doB " v proc) -> v with each element set to value of proc:\n   v[i] = (proc i)"
  int i;
  vct *v;
  SCM_ASSERT(vct_p(obj), obj, SCM_ARG1, S_vct_doB);
  SCM_ASSERT((gh_procedure_p(proc)), proc, SCM_ARG2, S_vct_doB);
  v = get_vct(obj);
#if USE_SND
  if (v) 
    for (i=0;i<v->length;i++) 
      v->data[i] = TO_C_DOUBLE(g_call1(proc, TO_SCM_INT(i)));
#else
  if (v) 
    for (i=0;i<v->length;i++) 
      v->data[i] = TO_C_DOUBLE(gh_call1(proc, TO_SCM_INT(i)));
#endif
  return(obj);
}

static SCM vcts_map(SCM args)
{
  #define H_vcts_mapB "(" S_vcts_mapB " v1 v2 ... proc) sets each element of the vct objects from\n   the list of values returned by (proc)"
  /* n vcts followed by proc, proc returns n values (list) on each call */
  int i,vi,vnum,vsize,argnum;
  vct **v;
  SCM proc,arg,svi,lst;
  argnum = gh_length(args);
  vnum = argnum-1;
  if (vnum <= 0)
    {
      scm_wrong_num_args(TO_SCM_STRING("vcts-map!"));
      return(TO_SMALL_SCM_INT(0));
    }
  v = (vct **)CALLOC(vnum, sizeof(vct *));
  for (i=0;i<vnum;i++)
    {
      arg = gh_list_ref(args, TO_SMALL_SCM_INT(i));
      if (!(vct_p(arg))) 
	{
	  scm_wrong_type_arg(S_vcts_mapB, i, arg);
	  return(TO_SMALL_SCM_INT(0));
	}
      v[i] = get_vct(arg);
    }
  proc = gh_list_ref(args, TO_SMALL_SCM_INT(vnum));
  if (!(gh_procedure_p(proc)))
    {
      scm_misc_error(S_vcts_mapB,
		     "in ~S, last argument must be a function",
		     SCM_LIST1(args));
      FREE(v);
      return(TO_SMALL_SCM_INT(0));
    }
  svi = TO_SMALL_SCM_INT(vnum);
  vsize = v[0]->length;
  for (i=1;i<vnum;i++) 
    if (vsize > v[i]->length) 
      vsize = v[i]->length;
  for (i=0;i<vsize;i++)
    {
#if USE_SND
      arg = g_call1(proc, svi);
#else
      arg = gh_call1(proc, svi);
#endif
      if (gh_list_p(arg))
	{
	  for (vi=0, lst=arg;vi<vnum;vi++,lst=SCM_CDR(lst))
	    v[vi]->data[i] = TO_C_DOUBLE(SCM_CAR(lst));
	}
    }
  FREE(v);
  return(TO_SMALL_SCM_INT(vnum));
}

static SCM vcts_do(SCM args)
{
  #define H_vcts_doB "(" S_vcts_doB " v1 v2 ... proc) sets each element of the vct objects from\n   the list of values returned by (proc i)"
  /* n vcts followed by proc, proc returns n values (list) on each call */
  int i,vi,vnum,vsize,argnum;
  vct **v;
  SCM proc,arg,svi,lst;
  argnum = gh_length(args);
  vnum = argnum-1;
  if (vnum <= 0)
    {
      scm_wrong_num_args(TO_SCM_STRING("vcts-do!"));
      return(TO_SMALL_SCM_INT(0));
    }
  v = (vct **)CALLOC(vnum, sizeof(vct *));
  for (i=0;i<vnum;i++)
    {
      arg = gh_list_ref(args, TO_SMALL_SCM_INT(i));
      if (!(vct_p(arg))) 
	{
	  scm_wrong_type_arg(S_vcts_doB, i, arg);
	  return(TO_SMALL_SCM_INT(0));
	}
      v[i] = get_vct(arg);
    }
  proc = gh_list_ref(args, TO_SMALL_SCM_INT(vnum));
  if (!(gh_procedure_p(proc)))
    {
      scm_misc_error(S_vcts_doB,
		     "in ~S, last argument must be a function",
		     SCM_LIST1(args));
      FREE(v);
      return(TO_SMALL_SCM_INT(0));
    }
  vsize = v[0]->length;
  svi = TO_SMALL_SCM_INT(vnum);
  for (i=1;i<vnum;i++) 
    if (vsize > v[i]->length) 
      vsize = v[i]->length;
  for (i=0;i<vsize;i++)
    {
#if USE_SND
      arg = g_call2(proc, svi, TO_SCM_INT(i));
#else
      arg = gh_call2(proc, svi, TO_SCM_INT(i));
#endif
      if (gh_list_p(arg))
	{
	  for (vi=0, lst=arg;vi<vnum;vi++,lst=SCM_CDR(lst))
	    v[vi]->data[i] = TO_C_DOUBLE(SCM_CAR(lst));
	}
    }
  FREE(v);
  return(TO_SMALL_SCM_INT(vnum));
}

static SCM vct_peak(SCM obj)
{
  #define H_vct_peak "(" S_vct_peak " v) -> max of abs of elements of v"
  int i;
  Float val=0.0,absv;
  vct *v;
  SCM_ASSERT(vct_p(obj), obj, SCM_ARG1, S_vct_peak);
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
  return(scm_return_first(TO_SCM_DOUBLE(val), obj));
}

static SCM list2vct(SCM lst)
{
  #define H_list2vct "(" S_list2vct " lst) -> a new vct object filled with elements of list lst"
  int len,i;
  vct *v;
  SCM scv,lst1;
  SCM_ASSERT(gh_list_p(lst), lst, SCM_ARG1, S_list2vct);
  len = gh_length(lst);
  scv = make_vct(len, (Float *)CALLOC(len, sizeof(Float)));
  v = get_vct(scv);
  for (i=0, lst1=lst;i<len;i++,lst1=SCM_CDR(lst1)) 
    v->data[i] = (Float)TO_C_DOUBLE(SCM_CAR(lst1));
  return(scm_return_first(scv, lst));
}

static SCM g_vct(SCM args) 
{
  #define H_vct "(" S_vct " args -> vct with contents as args"
  return(list2vct(args));
}

#if (!USE_SND)
static SCM array_to_list(Float *arr, int i, int len)
{
  if (i < (len-1))
    return(gh_cons(TO_SCM_DOUBLE(arr[i]), array_to_list(arr, i+1, len)));
  else return(gh_cons(TO_SCM_DOUBLE(arr[i]), SCM_EOL));
}
#endif

static SCM vct2list(SCM vobj)
{
  #define H_vct2list "(" S_vct2list " v) -> a new list with elements of vct v"
  vct *v;
  SCM_ASSERT(vct_p(vobj), vobj, SCM_ARG1, S_vct2list);
  v = get_vct(vobj);
  return(scm_return_first(array_to_list(v->data, 0, v->length), vobj));
}

static SCM vector2vct(SCM vect)
{
  #define H_vector2vct "(" S_vector2vct " vect) -> a new vct object with the elements of vector vect"
  int len,i;
  vct *v;
  SCM *vdata;
  SCM scv;
  SCM_ASSERT(gh_vector_p(vect), vect, SCM_ARG1, S_vector2vct);
  len = gh_vector_length(vect);
  scv = make_vct(len, (Float *)CALLOC(len, sizeof(Float)));
  v = get_vct(scv);
  vdata = SCM_VELTS(vect);
  for (i=0;i<len;i++) 
    v->data[i] = (Float)TO_C_DOUBLE(vdata[i]);
  return(scm_return_first(scv, vect));
}

static SCM vct_subseq(SCM vobj, SCM start, SCM end, SCM newv)
{
  #define H_vct_subseq "(" S_vct_subseq " v start end &optional vnew) -> vnew with vals v[start..end]"
  vct *vold,*vnew;
  SCM res;
  int i,old_len,new_len,j;
  SCM_ASSERT(vct_p(vobj), vobj, SCM_ARG1, S_vct_subseq);
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(start)), start, SCM_ARG2, S_vct_subseq);
  vold = get_vct(vobj);
  old_len = vold->length;
  if (SCM_INUMP(end))
    new_len = SCM_INUM(end) - SCM_INUM(start) + 1;
  else new_len = old_len - SCM_INUM(start);
  if (new_len <= 0) 
    return(SCM_BOOL_F);
  if (vct_p(newv))
    res = newv;
  else res = make_vct(new_len, (Float *)CALLOC(new_len, sizeof(Float)));
  vnew = get_vct(res);
  if (new_len > vnew->length) 
    new_len = vnew->length;
  for (i=SCM_INUM(start), j=0;(j < new_len) && (i < old_len);i++,j++)
    vnew->data[j] = vold->data[i];
  return(scm_return_first(res, vobj, vnew));
}

void init_vct(void)
{
  SCM local_doc;
#if HAVE_NEW_SMOB
  vct_tag = scm_make_smob_type("vct", sizeof(vct));
  scm_set_smob_mark(vct_tag, mark_vct);
  scm_set_smob_print(vct_tag, print_vct);
  scm_set_smob_free(vct_tag, free_vct);
  scm_set_smob_equalp(vct_tag, equalp_vct);
#if HAVE_APPLICABLE_SMOB
  scm_set_smob_apply(vct_tag, vct_ref, 1, 0, 0);
#endif

#else
  vct_tag = scm_newsmob(&vct_smobfuns);
#endif
  local_doc = scm_permanent_object(scm_string_to_symbol(TO_SCM_STRING("documentation")));

  DEFINE_PROC(gh_new_procedure1_0(S_make_vct,      g_make_vct),    H_make_vct);
  DEFINE_PROC(gh_new_procedure1_0(S_vct_copy,      copy_vct),      H_vct_copy);
  DEFINE_PROC(gh_new_procedure1_0(S_vct_p,         g_vct_p),       H_vct_p);
  DEFINE_PROC(gh_new_procedure1_0(S_list2vct,      list2vct),      H_list2vct);
  DEFINE_PROC(gh_new_procedure1_0(S_vct2list,      vct2list),      H_vct2list);
  DEFINE_PROC(gh_new_procedure1_0(S_vector2vct,    vector2vct),    H_vector2vct);
  DEFINE_PROC(gh_new_procedure1_0(S_vct_length,    vct_length),    H_vct_length);
  DEFINE_PROC(gh_new_procedure2_0(S_vct_ref,       vct_ref),       H_vct_ref);
  DEFINE_PROC(gh_new_procedure3_0(S_vct_setB,      vct_set),       H_vct_setB);
  DEFINE_PROC(gh_new_procedure2_0(S_vct_multiplyB, vct_multiply),  H_vct_multiplyB);
  DEFINE_PROC(gh_new_procedure2_0(S_vct_scaleB,    vct_scale),     H_vct_scaleB);
  DEFINE_PROC(gh_new_procedure2_0(S_vct_fillB,     vct_fill),      H_vct_fillB);
  DEFINE_PROC(gh_new_procedure2_1(S_vct_addB,      vct_add),       H_vct_addB);
  DEFINE_PROC(gh_new_procedure2_0(S_vct_subtractB, vct_subtract),  H_vct_subtractB);
  DEFINE_PROC(gh_new_procedure2_0(S_vct_offsetB,   vct_offset),    H_vct_offsetB);
  DEFINE_PROC(gh_new_procedure2_0(S_vct_mapB,      vct_map),       H_vct_mapB);
  DEFINE_PROC(gh_new_procedure2_0(S_vct_doB,       vct_do),        H_vct_doB);
  DEFINE_PROC(gh_new_procedure1_0(S_vct_peak,      vct_peak),      H_vct_peak);
  DEFINE_PROC(gh_new_procedure(S_vcts_mapB, SCM_FNC vcts_map, 0, 0, 1), H_vcts_mapB);
  DEFINE_PROC(gh_new_procedure(S_vcts_doB, SCM_FNC vcts_do, 0, 0, 1), H_vcts_doB);
  DEFINE_PROC(gh_new_procedure(S_vct_moveB, SCM_FNC vct_move, 3, 1, 0), H_vct_moveB);
  DEFINE_PROC(gh_new_procedure2_2(S_vct_subseq,    vct_subseq),    H_vct_subseq);
  DEFINE_PROC(gh_new_procedure(S_vct,      SCM_FNC g_vct, 0, 0, 1),   H_vct);

#if USE_SND
  define_procedure_with_setter(S_vct_ref, SCM_FNC vct_ref, H_vct_ref,
			       "set-" S_vct_ref, SCM_FNC vct_set, local_doc, 2, 0, 3, 0);
#endif
}
#endif
