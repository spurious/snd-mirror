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
 *   (vct-move! v new old)                 v[new++] = v[old++] -> v
 *   (vct-subseq v start end &opt vnew)    vnew = v[start..end]
 *
 * The intended use is a sort of latter-day array-processing system that handles huge
 * one-dimensional vectors -- fft's, etc.  Some of these functions can be found in
 * snd-scm.c in the Snd package; others can be found in the CLM package, (clm2scm.c).
 */

#if defined(HAVE_CONFIG_H)
  #include "config.h"
#endif

#if USE_SND
  #include "snd.h"
#else
  #define PRINT_BUFFER_SIZE 512
  #define LABEL_BUFFER_SIZE 64
#endif

#include <math.h>
#include <stdlib.h>
#include <stdio.h>
#if (!defined(HAVE_CONFIG_H)) || (defined(HAVE_STRING_H))
  #include <string.h>
#endif

#if (!USE_SND)
#if HAVE_GUILE
  #include <guile/gh.h>
  #include "sg.h"
#endif
#if HAVE_LIBREP 
  #include <rep.h>
  #include "sl.h"
#endif
#if HAVE_MZSCHEME
  #include <scheme.h>
  #include "sz.h"
#endif
#if (!HAVE_EXTENSION_LANGUAGE)
  #include "noguile.h"
#endif
#endif

#include "sndlib.h"
#include "vct.h"
#include "sndlib2scm.h"

#ifndef CALLOC
  #define CALLOC(a, b)  calloc(a, b)
  #define MALLOC(a)     malloc(a)
  #define FREE(a)       free(a)
  #define REALLOC(a, b) realloc(a, b)
#endif

#define VCT_PRINT_LENGTH 10

#ifndef MIN
  #define MIN(a, b) ((a > b) ? (b) : (a))
#endif

static SND_TAG_TYPE vct_tag = 0;
static int vct_print_length = VCT_PRINT_LENGTH;
void set_vct_print_length(int len) {vct_print_length = len;}

int vct_p(SCM obj)
{
  return(SMOB_TYPE_P(obj, vct_tag));
}

static SCM g_vct_p(SCM obj) 
{
  #define H_vct_p "(" S_vct_p " obj) -> #t if obj is a vct object, else #f"
  return(TO_SCM_BOOLEAN(VCT_P(obj)));
}

vct *get_vct(SCM arg)
{
  if (VCT_P(arg))
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
      free(v);
      SND_SET_VALUE_OF(obj, (SCM)NULL);
    }
  return(sizeof(vct));
}

static int print_vct(SCM obj, SCM port, scm_print_state *pstate)
{
  int len, i;
  char *buf;
  vct *v = (vct *)SND_VALUE_OF(obj);
  buf = (char *)CALLOC(64, sizeof(char));
  sprintf(buf, "#<vct[len=%d]:", v->length);
  WRITE_STRING(buf, port);
  len = vct_print_length;
  if (len > v->length) len = v->length;
  if (len > 0)
    {
      for (i = 0; i < len; i++)
	{
	  sprintf(buf, " %.3f", v->data[i]);
	  WRITE_STRING(buf, port);
	}
      if (v->length > vct_print_length)
	WRITE_STRING(" ...", port);
    }
  FREE(buf);
  WRITE_STRING(">", port);
#if HAVE_SCM_REMEMBER_UPTO_HERE
  scm_remember_upto_here(obj);
#endif
  return(1);
}

static SCM equalp_vct(SCM obj1, SCM obj2)
{
  vct *v1, *v2;
  int i;
  v1 = (vct *)SND_VALUE_OF(obj1);
  v2 = (vct *)SND_VALUE_OF(obj2);
  if (v1->length != v2->length) 
    return(SCM_BOOL_F);
  for (i = 0; i < v1->length; i++)
    if (v1->data[i] != v2->data[i])
      return(SCM_BOOL_F);
  return(scm_return_first(SCM_BOOL_T, obj1, obj2));
}

SCM make_vct(int len, Float *data)
{
  vct *new_vct;
  new_vct = (vct *)scm_must_malloc(sizeof(vct), S_make_vct);
  new_vct->length = len;
  new_vct->data = data;
  new_vct->dont_free = 0;
  SND_RETURN_NEWSMOB(vct_tag, new_vct);
}

SCM make_vct_wrapper(int len, Float *data)
{
  vct *new_vct;
  new_vct = (vct *)scm_must_malloc(sizeof(vct), S_make_vct);
  new_vct->length = len;
  new_vct->data = data;
  new_vct->dont_free = 1;
  SND_RETURN_NEWSMOB(vct_tag, new_vct);
}

static SCM g_make_vct(SCM len)
{
  #define H_make_vct "(" S_make_vct " len) -> a new vct object of length len"
  int size;
  ASSERT_TYPE(INTEGER_P(len), len, SCM_ARGn, S_make_vct, "an integer");
  size = TO_C_INT(len);
  if (size <= 0) 
    mus_misc_error(S_make_vct, "len <= 0?", len);
  return(make_vct(size, (Float *)CALLOC(size, sizeof(Float))));
}

static SCM copy_vct(SCM obj)
{
  #define H_vct_copy "(" S_vct_copy " v) -> a copy of vct v"
  vct *v;
  Float *copied_data;
  int len;
  ASSERT_TYPE(VCT_P(obj), obj, SCM_ARGn, S_vct_copy, "a vct");
  v = TO_VCT(obj);
  if (v)
    {
      len = v->length;
      copied_data = (Float *)MALLOC(len * sizeof(Float));
      memcpy((void *)copied_data, (void *)(v->data), (len * sizeof(Float)));
      return(make_vct(len, copied_data));
    }
  return(scm_return_first(SCM_BOOL_F, obj));
}

static SCM vct_move(SCM obj, SCM newi, SCM oldi, SCM backwards)
{
  #define H_vct_moveB "(" S_vct_moveB " obj new old backwards) moves obj data from old to new"
  vct *v;
  int i, j, ni, nj;
  ASSERT_TYPE(VCT_P(obj), obj, SCM_ARG1, S_vct_moveB, "a vct");
  ASSERT_TYPE(INTEGER_P(newi), newi, SCM_ARG2, S_vct_moveB, "an integer");
  ASSERT_TYPE(INTEGER_P(oldi), oldi, SCM_ARG3, S_vct_moveB, "an integer");
  ASSERT_TYPE(BOOLEAN_IF_BOUND_P(backwards), backwards, SCM_ARG4, S_vct_moveB, "a boolean");
  v = TO_VCT(obj);
  ni = TO_SMALL_C_INT(newi);
  nj = TO_SMALL_C_INT(oldi);
  if ((BOOLEAN_P(backwards)) && 
      (NOT_FALSE_P(backwards)))
    {
      if (ni >= v->length) 
	mus_misc_error(S_vct_moveB,
		       "new-index too high?",
		       SCM_LIST2(newi,
				 TO_SCM_INT(v->length)));
      if (nj >= v->length)
	mus_misc_error(S_vct_moveB,
		       "old-index too high?",
		       SCM_LIST2(oldi,
				 TO_SCM_INT(v->length)));
      if (v) 
	for (i = ni, j = nj; (j >= 0) && (i >= 0); i--, j--) 
	  v->data[i] = v->data[j];
    }
  else
    {
      if (ni < 0)
	mus_misc_error(S_vct_moveB,
		       "new-index < 0?",
		       newi);
      if (nj < 0)
	mus_misc_error(S_vct_moveB,
		       "old-index < 0?",
		       oldi);
      if (v) 
	for (i = ni, j = nj; (j < v->length) && (i < v->length); i++, j++) 
	  v->data[i] = v->data[j];
    }
  return(obj);
}

static SCM vct_length(SCM obj)
{
  #define H_vct_length     "(" S_vct_length " v) -> length of vct v"
  vct *v;
  ASSERT_TYPE(VCT_P(obj), obj, SCM_ARGn, S_vct_length, "a vct");
  v = TO_VCT(obj);
  if (v)
    return(TO_SCM_INT(v->length));
  return(TO_SMALL_SCM_INT(0));
}

/* static SCM reckless_vct_ref(SCM obj, SCM pos) {return(TO_SCM_DOUBLE(((vct *)SND_VALUE_OF(obj))->data[TO_SMALL_C_INT(pos)]));} */
/* this is about 4% faster than the checked version */

static SCM vct_ref(SCM obj, SCM pos)
{
  #define H_vct_ref "(" S_vct_ref " v n) -> element n of vct v, v[n]"
  vct *v;
  int loc;
  ASSERT_TYPE(VCT_P(obj), obj, SCM_ARG1, S_vct_ref, "a vct");
  ASSERT_TYPE(INTEGER_P(pos), pos, SCM_ARG2, S_vct_ref, "an integer");
  v = TO_VCT(obj);
  if (v)
    {
      loc = TO_C_INT(pos);
      if (loc < 0)
	mus_misc_error(S_vct_ref, "index < 0?", pos);
      else
	{
	  if (loc >= v->length)
	    mus_misc_error(S_vct_ref, "index too high?", SCM_LIST2(pos, TO_SCM_INT(v->length)));
	  return(TO_SCM_DOUBLE(v->data[loc]));
	}
    }
  else mus_misc_error(S_vct_ref, "vct obj is null?", obj);
  return(TO_SCM_DOUBLE(0.0));
}

static SCM vct_set(SCM obj, SCM pos, SCM val)
{
  #define H_vct_setB "(" S_vct_setB " v n val) sets element n of vct v to val, v[n] = val"
  vct *v;
  int loc;
  ASSERT_TYPE(VCT_P(obj), obj, SCM_ARG1, S_vct_setB, "a vct");
  ASSERT_TYPE(INTEGER_P(pos), pos, SCM_ARG2, S_vct_setB, "an integer");
  ASSERT_TYPE(NUMBER_P(val), val, SCM_ARG3, S_vct_setB, "a number");
  v = TO_VCT(obj);
  if (v)
    {
      loc = TO_C_INT(pos);
      if (loc < 0)
	mus_misc_error(S_vct_setB, "index < 0?", pos); 
      if (loc >= v->length)
	mus_misc_error(S_vct_setB, "index >= vct-length?", SCM_LIST2(pos, TO_SCM_INT(v->length)));
      v->data[loc] = TO_C_DOUBLE(val);
    }
  else mus_misc_error(S_vct_ref, "vct obj is null?", obj);
  return(scm_return_first(val, obj, pos));
}

static SCM vct_multiply(SCM obj1, SCM obj2)
{
  #define H_vct_multiplyB "(" S_vct_multiplyB " v1 v2) -> v1 with element-wise multiply of vcts v1 and v2: v1[i] *= v2[i]"
  int i, lim;
  vct *v1, *v2;
  ASSERT_TYPE(VCT_P(obj1), obj1, SCM_ARG1, S_vct_multiplyB, "a vct");
  ASSERT_TYPE(VCT_P(obj2), obj2, SCM_ARG2, S_vct_multiplyB, "a vct");
  v1 = TO_VCT(obj1);
  v2 = TO_VCT(obj2);
  if ((v1) && (v2))
    {
      lim = MIN(v1->length, v2->length);
      for (i = 0; i < lim; i++) v1->data[i] *= v2->data[i];
    }
  return(scm_return_first(obj1, obj2)); /* I wonder if this is necessary */
}

static SCM vct_add(SCM obj1, SCM obj2, SCM offs)
{
  #define H_vct_addB "(" S_vct_addB " v1 v2 &optional (offset 0)) -> v1 with element-wise add of vcts v1 and v2: v1[i+offset] += v2[i]"
  int i, lim, j;
  vct *v1, *v2;
  ASSERT_TYPE(VCT_P(obj1), obj1, SCM_ARG1, S_vct_addB, "a vct");
  ASSERT_TYPE(VCT_P(obj2), obj2, SCM_ARG2, S_vct_addB, "a vct");
  v1 = TO_VCT(obj1);
  v2 = TO_VCT(obj2);
  if ((v1) && (v2))
    {
      lim = MIN(v1->length, v2->length);
      if (INTEGER_P(offs))
	for (i = 0, j = TO_C_INT(offs); i < lim; i++, j++) 
	  v1->data[j] += v2->data[i];
      else
	for (i = 0; i < lim; i++) 
	  v1->data[i] += v2->data[i];
    }
  return(scm_return_first(obj1, obj2));
}

static SCM vct_subtract(SCM obj1, SCM obj2)
{
  #define H_vct_subtractB "(" S_vct_subtractB " v1 v2) -> v1 with element-wise subtract of vcts v1 and v2: v1[i] -= v2[i]"
  int i, lim;
  vct *v1, *v2;
  ASSERT_TYPE(VCT_P(obj1), obj1, SCM_ARG1, S_vct_subtractB, "a vct");
  ASSERT_TYPE(VCT_P(obj2), obj2, SCM_ARG2, S_vct_subtractB, "a vct");
  v1 = TO_VCT(obj1);
  v2 = TO_VCT(obj2);
  if ((v1) && (v2))
    {
      lim = MIN(v1->length, v2->length);
      for (i = 0; i < lim; i++) v1->data[i] -= v2->data[i];
    }
  return(scm_return_first(obj1, obj2));
}

static SCM vct_scale(SCM obj1, SCM obj2)
{
  #define H_vct_scaleB "(" S_vct_scaleB " v val) -> v with each element scaled by val: v[i] *= val"
  int i;
  vct *v1;
  Float scl;
  ASSERT_TYPE(VCT_P(obj1), obj1, SCM_ARG1, S_vct_scaleB, "a vct");
  ASSERT_TYPE(NUMBER_P(obj2), obj2, SCM_ARG2, S_vct_scaleB, "a number");
  v1 = TO_VCT(obj1);
  scl = TO_C_DOUBLE(obj2);
  if (v1)
    for (i = 0; i < v1->length; i++) v1->data[i] *= scl;
  return(scm_return_first(obj1, obj2));
}

static SCM vct_offset(SCM obj1, SCM obj2)
{
  #define H_vct_offsetB "(" S_vct_offsetB " v val) -> v with val added to each element: v[i] += val"
  int i;
  vct *v1;
  Float scl;
  ASSERT_TYPE(VCT_P(obj1), obj1, SCM_ARG1, S_vct_offsetB, "a vct");
  ASSERT_TYPE(NUMBER_P(obj2), obj2, SCM_ARG2, S_vct_offsetB, "a number");
  v1 = TO_VCT(obj1);
  scl = TO_C_DOUBLE(obj2);
  if (v1)
    for (i = 0; i < v1->length; i++) v1->data[i] += scl;
  return(scm_return_first(obj1, obj2));
}

static SCM vct_fill(SCM obj1, SCM obj2)
{
  #define H_vct_fillB "(" S_vct_fillB " v val) -> v with each element set to val: v[i] = val"
  int i;
  vct *v1;
  Float scl;
  ASSERT_TYPE(VCT_P(obj1), obj1, SCM_ARG1, S_vct_fillB, "a vct");
  ASSERT_TYPE(NUMBER_P(obj2), obj2, SCM_ARG2, S_vct_fillB, "a number");
  v1 = TO_VCT(obj1);
  scl = TO_C_DOUBLE(obj2);
  if (v1)
    for (i = 0; i < v1->length; i++) v1->data[i] = scl;
  return(scm_return_first(obj1, obj2));
}

int procedure_fits(SCM proc, int args)
{
  SCM arity;
  if (PROCEDURE_P(proc))
    {
      arity = ARITY(proc);
      return(NOT_FALSE_P(arity) && 
	     (TO_C_INT(SCM_CAR(arity)) == args));
    }
  return(0);
}

static SCM vct_map(SCM obj, SCM proc)
{
  #define H_vct_mapB "(" S_vct_mapB " v proc) -> v with each element set to value of proc: v[i] = (proc)"
  int i;
  vct *v;
  ASSERT_TYPE(VCT_P(obj), obj, SCM_ARG1, S_vct_mapB, "a vct");
  ASSERT_TYPE((PROCEDURE_P(proc)) && (procedure_fits(proc, 0)), proc, SCM_ARG2, S_vct_mapB, "a thunk");
  v = TO_VCT(obj);
  if (v) 
    for (i = 0; i < v->length; i++) 
      v->data[i] = TO_C_DOUBLE(CALL0(proc, S_vct_mapB));
  return(scm_return_first(obj, proc));
}

static SCM vct_do(SCM obj, SCM proc)
{
  #define H_vct_doB "(" S_vct_doB " v proc) -> v with each element set to value of proc: v[i] = (proc i)"
  int i;
  vct *v;
  ASSERT_TYPE(VCT_P(obj), obj, SCM_ARG1, S_vct_doB, "a vct");
  ASSERT_TYPE((PROCEDURE_P(proc)) && (procedure_fits(proc, 1)), proc, SCM_ARG2, S_vct_doB, "a procedure");
  v = TO_VCT(obj);
  if (v) 
    for (i = 0; i < v->length; i++) 
      v->data[i] = TO_C_DOUBLE(CALL1(proc, TO_SMALL_SCM_INT(i), S_vct_doB));
  return(scm_return_first(obj, proc));
}

static SCM vcts_map(SCM args)
{
  #define H_vcts_mapB "(" S_vcts_mapB " v1 v2 ... proc) sets each element of the vct objects from the list of values returned by (proc)"
  /* n vcts followed by proc, proc returns n values (list) on each call */
  int i, vi, vnum, vsize, argnum;
  vct **v;
  SCM proc, arg, svi, lst;
  argnum = LIST_LENGTH(args);
  vnum = argnum - 1;
  if (vnum <= 1)
    mus_misc_error(S_vcts_mapB, "need at least 2 args", args);
  v = (vct **)CALLOC(vnum, sizeof(vct *));
  for (i = 0; i < vnum; i++)
    {
      arg = LIST_REF(args, i);
      if (!(VCT_P(arg))) 
	{
	  FREE(v);
	  ASSERT_TYPE(0, arg, i, S_vcts_mapB, "a vct");
	}
      v[i] = TO_VCT(arg);
    }
  proc = LIST_REF(args, vnum);
  if (!(procedure_fits(proc, 1)))
    {
      FREE(v);
      mus_misc_error(S_vcts_mapB,
		     "last argument must be a function of 1 arg",
		     args);
      return(TO_SMALL_SCM_INT(0));
    }
  svi = TO_SMALL_SCM_INT(vnum);
  vsize = v[0]->length;
  for (i = 1; i < vnum; i++) 
    if (vsize > v[i]->length) 
      vsize = v[i]->length;
  for (i = 0; i < vsize; i++)
    {
      arg = CALL1(proc, svi, S_vcts_mapB);
      if (LIST_P(arg))
	for (vi = 0, lst = arg; vi < vnum; vi++, lst = SCM_CDR(lst))
	  v[vi]->data[i] = TO_C_DOUBLE(SCM_CAR(lst));
    }
  FREE(v);
  return(scm_return_first(TO_SMALL_SCM_INT(vnum), args));
}

static SCM vcts_do(SCM args)
{
  #define H_vcts_doB "(" S_vcts_doB " v1 v2 ... proc) sets each element of the vct objects from the list of values returned by (proc vnum i)"
  /* n vcts followed by proc, proc returns n values (list) on each call */
  int i, vi, vnum, vsize, argnum;
  vct **v;
  SCM proc, arg, svi, lst;
  argnum = LIST_LENGTH(args);
  vnum = argnum - 1;
  if (vnum <= 1)
    mus_misc_error(S_vcts_doB, "need at least 2 args", args);
  v = (vct **)CALLOC(vnum, sizeof(vct *));
  for (i = 0; i < vnum; i++)
    {
      arg = LIST_REF(args, i);
      if (!(VCT_P(arg))) 
	{
	  FREE(v);
	  ASSERT_TYPE(0, arg, i, S_vcts_doB, "a vct");
	}
      v[i] = TO_VCT(arg);
    }
  proc = LIST_REF(args, vnum);
  if (!(procedure_fits(proc, 2)))
    {
      FREE(v);
      mus_misc_error(S_vcts_doB,
		     "last argument must be a function of 2 args",
		     args);
    }
  vsize = v[0]->length;
  svi = TO_SMALL_SCM_INT(vnum);
  for (i = 1; i < vnum; i++) 
    if (vsize > v[i]->length) 
      vsize = v[i]->length;
  for (i = 0; i < vsize; i++)
    {
      arg = CALL2(proc, svi, TO_SMALL_SCM_INT(i), S_vcts_doB);
      if (LIST_P(arg))
	for (vi = 0, lst = arg; vi < vnum; vi++, lst = SCM_CDR(lst))
	  v[vi]->data[i] = TO_C_DOUBLE(SCM_CAR(lst));
    }
  FREE(v);
  return(scm_return_first(TO_SMALL_SCM_INT(vnum), args));
}

static SCM vct_peak(SCM obj)
{
  #define H_vct_peak "(" S_vct_peak " v) -> max of abs of elements of v"
  int i;
  Float val = 0.0, absv;
  vct *v;
  ASSERT_TYPE(VCT_P(obj), obj, SCM_ARGn, S_vct_peak, "a vct");
  v = TO_VCT(obj);
  if (v) 
    {
      val = fabs(v->data[0]); 
      for (i = 1; i < v->length; i++) 
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
  int len, i;
  vct *v;
  SCM scv, lst1;
  ASSERT_TYPE(LIST_P_WITH_LENGTH(lst, len), lst, SCM_ARGn, S_list2vct, "a list");
  if (len == 0)
    return(SCM_BOOL_F);
  scv = make_vct(len, (Float *)CALLOC(len, sizeof(Float)));
  v = TO_VCT(scv);
  for (i = 0, lst1 = lst; i < len; i++, lst1 = SCM_CDR(lst1)) 
    v->data[i] = (Float)TO_C_DOUBLE(SCM_CAR(lst1));
  return(scm_return_first(scv, lst));
}

static SCM g_vct(SCM args) 
{
  #define H_vct "(" S_vct " args -> vct with contents as args"
  return(list2vct(args));
}

static SCM array_to_list(Float *arr, int i, int len)
{
  if (i < (len - 1))
    return(CONS(TO_SCM_DOUBLE(arr[i]), 
		array_to_list(arr, i + 1, len)));
  else return(CONS(TO_SCM_DOUBLE(arr[i]), 
		   SCM_EOL));
}

static SCM vct2list(SCM vobj)
{
  #define H_vct2list "(" S_vct2list " v) -> a new list with elements of vct v"
  vct *v;
  ASSERT_TYPE(VCT_P(vobj), vobj, SCM_ARGn, S_vct2list, "a vct");
  v = TO_VCT(vobj);
  return(scm_return_first(array_to_list(v->data, 0, v->length), vobj));
}

static SCM vector2vct(SCM vect)
{
  #define H_vector2vct "(" S_vector2vct " vect) -> a new vct object with the elements of vector vect"
  int len, i;
  vct *v;
  SCM *vdata;
  SCM scv;
  ASSERT_TYPE(VECTOR_P(vect), vect, SCM_ARGn, S_vector2vct, "a vector");
  len = VECTOR_LENGTH(vect);
  scv = make_vct(len, (Float *)CALLOC(len, sizeof(Float)));
  v = TO_VCT(scv);
  vdata = SCM_VELTS(vect);
  for (i = 0; i < len; i++) 
    v->data[i] = (Float)TO_C_DOUBLE(vdata[i]);
  return(scm_return_first(scv, vect));
}

static SCM vct_subseq(SCM vobj, SCM start, SCM end, SCM newv)
{
  #define H_vct_subseq "(" S_vct_subseq " v start end &optional vnew) -> vnew with vals v[start..end]"
  vct *vold, *vnew;
  SCM res;
  int i, old_len, new_len, j;
  ASSERT_TYPE(VCT_P(vobj), vobj, SCM_ARG1, S_vct_subseq, "a vct");
  ASSERT_TYPE(INTEGER_P(start), start, SCM_ARG2, S_vct_subseq, "an integer");
  ASSERT_TYPE(INTEGER_IF_BOUND_P(end), end, SCM_ARG3, S_vct_subseq, "an integer");
  vold = TO_VCT(vobj);
  old_len = vold->length;
  if (INTEGER_P(end))
    new_len = TO_C_INT(end) - TO_C_INT(start) + 1;
  else new_len = old_len - TO_C_INT(start);
  if (new_len <= 0) 
    return(SCM_BOOL_F);
  if (VCT_P(newv))
    res = newv;
  else res = make_vct(new_len, (Float *)CALLOC(new_len, sizeof(Float)));
  vnew = TO_VCT(res);
  if (new_len > vnew->length) 
    new_len = vnew->length;
  for (i = TO_C_INT(start), j = 0; (j < new_len) && (i < old_len); i++, j++)
    vnew->data[j] = vold->data[i];
  return(scm_return_first(res, vobj, vnew));
}

void init_vct(void)
{
  SCM local_doc;
#if HAVE_GUILE
  vct_tag = scm_make_smob_type("vct", sizeof(vct));
  scm_set_smob_print(vct_tag, print_vct);
  scm_set_smob_free(vct_tag, free_vct);
  scm_set_smob_equalp(vct_tag, equalp_vct);
#if HAVE_APPLICABLE_SMOB
  scm_set_smob_apply(vct_tag, SCM_FNC vct_ref, 1, 0, 0);
#endif
#endif

  local_doc = MAKE_PERMANENT(DOCUMENTATION);

  DEFINE_PROC(S_make_vct,      g_make_vct, 1, 0, 0,    H_make_vct);
  DEFINE_PROC(S_vct_copy,      copy_vct, 1, 0, 0,      H_vct_copy);
  DEFINE_PROC(S_vct_p,         g_vct_p, 1, 0, 0,       H_vct_p);
  DEFINE_PROC(S_list2vct,      list2vct, 1, 0, 0,      H_list2vct);
  DEFINE_PROC(S_vct2list,      vct2list, 1, 0, 0,      H_vct2list);
  DEFINE_PROC(S_vector2vct,    vector2vct, 1, 0, 0,    H_vector2vct);
  DEFINE_PROC(S_vct_length,    vct_length, 1, 0, 0,    H_vct_length);
  DEFINE_PROC(S_vct_ref,       vct_ref, 2, 0, 0,       H_vct_ref);
  DEFINE_PROC(S_vct_setB,      vct_set, 3, 0, 0,       H_vct_setB);
  DEFINE_PROC(S_vct_multiplyB, vct_multiply, 2, 0, 0,  H_vct_multiplyB);
  DEFINE_PROC(S_vct_scaleB,    vct_scale, 2, 0, 0,     H_vct_scaleB);
  DEFINE_PROC(S_vct_fillB,     vct_fill, 2, 0, 0,      H_vct_fillB);
  DEFINE_PROC(S_vct_addB,      vct_add, 2, 1, 0,       H_vct_addB);
  DEFINE_PROC(S_vct_subtractB, vct_subtract, 2, 0, 0,  H_vct_subtractB);
  DEFINE_PROC(S_vct_offsetB,   vct_offset, 2, 0, 0,    H_vct_offsetB);
  DEFINE_PROC(S_vct_mapB,      vct_map, 2, 0, 0,       H_vct_mapB);
  DEFINE_PROC(S_vct_doB,       vct_do, 2, 0, 0,        H_vct_doB);
  DEFINE_PROC(S_vct_peak,      vct_peak, 1, 0, 0,      H_vct_peak);
  DEFINE_PROC(S_vcts_mapB,     vcts_map, 0, 0, 1,      H_vcts_mapB);
  DEFINE_PROC(S_vcts_doB,      vcts_do, 0, 0, 1,       H_vcts_doB);
  DEFINE_PROC(S_vct_moveB,     vct_move, 3, 1, 0,      H_vct_moveB);
  DEFINE_PROC(S_vct_subseq,    vct_subseq, 2, 2, 0,    H_vct_subseq);
  DEFINE_PROC(S_vct,           g_vct, 0, 0, 1,         H_vct);

#if USE_SND
  define_procedure_with_setter(S_vct_ref, SCM_FNC vct_ref, H_vct_ref,
			       "set-" S_vct_ref, SCM_FNC vct_set, local_doc, 2, 0, 3, 0);
#endif
}

