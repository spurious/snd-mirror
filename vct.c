/* vct support 
 *
 * a vct is a Guile "smob" containing a Float array and its size
 * we use these in Snd because Guile's floating-point and vector operations are slow,
 * and Snd/CLM are applications where no user is willing to wait on a multiply.
 *
 * C side:
 *   void init_vct(void)                   called to declare the various functions and the vct type in Guile
 *   int vct_p(XEN obj)                    is obj a vct
 *   XEN make_vct(int len, Float *data)    make a new vct
 *   XEN make_vct_wrapper(int len, Float *data) make a new vct that doesn't free data when garbage collector strikes
 *   vct *get_vct(XEN arg)                 given XEN arg, return vct object
 *   void set_vct_print_length(int val)    set vct print length (default 10)
 *   XEN vct_to_vector(XEN vct)            return vector of vct contents
 *
 * Scheme side:
 *   (make-vct len &opt (filler 0.0))      make new vct
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
 *   (vct-peak v1)                         max val (abs) in v
 *   (list->vct lst)                       return vct with elements of list lst
 *   (vct->list v1)                        return list with elements of vct v1
 *   (vector->vct vect)                    return vct with elements of vector vect
 *   (vct->vector v)                       return vector of vct contents
 *   (vct-move! v new old)                 v[new++] = v[old++] -> v
 *   (vct-subseq v start end &opt vnew)    vnew = v[start..end]
 *
 * The intended use is a sort of latter-day array-processing system that handles huge
 * one-dimensional vectors -- fft's, etc.  Some of these functions can be found in
 * snd-xen.c in the Snd package; others can be found in the CLM package, (clm2xen.c).
 */

#if defined(HAVE_CONFIG_H)
  #include <config.h>
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

#include "sndlib.h"
#include "sndlib2xen.h"
#include "vct.h"

#ifndef CALLOC
  #define CALLOC(a, b)  calloc(a, b)
  #define MALLOC(a)     malloc(a)
  #define FREE(a)       free(a)
  #define REALLOC(a, b) realloc(a, b)
#endif

#if USE_SND
  #define VCT_PRINT_LENGTH DEFAULT_PRINT_LENGTH
#else
  #define VCT_PRINT_LENGTH 10
#endif

#ifndef MIN
  #define MIN(a, b) ((a > b) ? (b) : (a))
#endif

static XEN_OBJECT_TYPE vct_tag;
static int vct_print_length = VCT_PRINT_LENGTH;
void set_vct_print_length(int len) {vct_print_length = len;}

int vct_p(XEN obj)
{
  return(XEN_OBJECT_TYPE_P(obj, vct_tag));
}

static XEN g_vct_p(XEN obj) 
{
  #define H_vct_p "(" S_vct_p " obj) -> #t if obj is a vct object, else #f"
  return(C_TO_XEN_BOOLEAN(VCT_P(obj)));
}

vct *get_vct(XEN arg)
{
  if (VCT_P(arg))
    return((vct *)XEN_OBJECT_REF(arg));
  return(NULL);
}

static void vct_free(vct *v)
{
  if (v)
    {
      if ((v->dont_free == 0) && 
	  (v->data)) 
	FREE(v->data);
      v->data = NULL;
      FREE(v);
    }
}

XEN_MAKE_OBJECT_FREE_PROCEDURE(vct, free_vct, vct_free)

char *vct_to_string(vct *v)
{
  int len, i;
  char *buf;
  char flt[16];
  if (v == NULL) return(NULL); /* not copy_string! -- it's not a sndlib function */
  len = vct_print_length;
  if (len > v->length) len = v->length;
  buf = (char *)CALLOC(64 + len * 8, sizeof(char));
  sprintf(buf, "#<vct[len=%d]:", v->length);
  if (len > 0)
    {
      for (i = 0; i < len; i++)
	{
	  mus_snprintf(flt, 16, " %.3f", v->data[i]);
	  strcat(buf, flt);
	}
      if (v->length > vct_print_length)
	strcat(buf, " ...");
    }
  strcat(buf, ">");
  return(buf);
}

XEN_MAKE_OBJECT_PRINT_PROCEDURE(vct, print_vct, vct_to_string)

static XEN equalp_vct(XEN obj1, XEN obj2)
{
  vct *v1, *v2;
  int i;
  v1 = (vct *)XEN_OBJECT_REF(obj1);
  v2 = (vct *)XEN_OBJECT_REF(obj2);
  if (v1->length != v2->length) 
    return(XEN_FALSE);
  for (i = 0; i < v1->length; i++)
    if (v1->data[i] != v2->data[i])
      return(XEN_FALSE);
  return(xen_return_first(XEN_TRUE, obj1, obj2));
}

vct *c_make_vct(int len)
{
  vct *new_vct;
  new_vct = (vct *)MALLOC(sizeof(vct));
  new_vct->length = len;
  new_vct->data = (Float *)CALLOC(len, sizeof(Float));
  new_vct->dont_free = 0;
  return(new_vct);
}

vct *c_free_vct(vct *v) 
{
  vct_free(v);
  return(NULL);
}

XEN make_vct(int len, Float *data)
{
  vct *new_vct;
  new_vct = (vct *)MALLOC(sizeof(vct));
  new_vct->length = len;
  new_vct->data = data;
  new_vct->dont_free = 0;
  XEN_MAKE_AND_RETURN_OBJECT(vct_tag, new_vct, 0, free_vct);
}

XEN make_vct_wrapper(int len, Float *data)
{
  vct *new_vct;
  new_vct = (vct *)MALLOC(sizeof(vct));
  new_vct->length = len;
  new_vct->data = data;
  new_vct->dont_free = 1;
  XEN_MAKE_AND_RETURN_OBJECT(vct_tag, new_vct, 0, free_vct);
}

static XEN vct_fill(XEN obj1, XEN obj2);

static XEN g_make_vct(XEN len, XEN filler)
{
  #define H_make_vct "(" S_make_vct " len &optional initial-element) -> a new vct object of length len"
  int size;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(len), len, XEN_ONLY_ARG, S_make_vct, "an integer");
  size = XEN_TO_C_INT(len);
  if (size <= 0) 
    mus_misc_error(S_make_vct, "len <= 0?", len);
  if (XEN_NUMBER_P(filler))
    return(vct_fill(make_vct(size, (Float *)CALLOC(size, sizeof(Float))), filler));
  return(make_vct(size, (Float *)CALLOC(size, sizeof(Float))));
}

static XEN copy_vct(XEN obj)
{
  #define H_vct_copy "(" S_vct_copy " v) -> a copy of vct v"
  vct *v;
  Float *copied_data;
  int len;
  XEN_ASSERT_TYPE(VCT_P(obj), obj, XEN_ONLY_ARG, S_vct_copy, "a vct");
  v = TO_VCT(obj);
  len = v->length;
  copied_data = (Float *)MALLOC(len * sizeof(Float));
  memcpy((void *)copied_data, (void *)(v->data), (len * sizeof(Float)));
  return(make_vct(len, copied_data));
}

static XEN vct_move(XEN obj, XEN newi, XEN oldi, XEN backwards)
{
  #define H_vct_moveB "(" S_vct_moveB " obj new old backwards) moves obj data from old to new"
  vct *v;
  int i, j, ni, nj;
  XEN_ASSERT_TYPE(VCT_P(obj), obj, XEN_ARG_1, S_vct_moveB, "a vct");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(newi), newi, XEN_ARG_2, S_vct_moveB, "an integer");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(oldi), oldi, XEN_ARG_3, S_vct_moveB, "an integer");
  XEN_ASSERT_TYPE(XEN_BOOLEAN_IF_BOUND_P(backwards), backwards, XEN_ARG_4, S_vct_moveB, "a boolean");
  v = TO_VCT(obj);
  ni = XEN_TO_SMALL_C_INT(newi);
  nj = XEN_TO_SMALL_C_INT(oldi);
  if ((XEN_BOOLEAN_P(backwards)) && 
      (XEN_NOT_FALSE_P(backwards)))
    {
      if (ni >= v->length) 
	mus_misc_error(S_vct_moveB,
		       "new-index too high?",
		       XEN_LIST_2(newi,
				  C_TO_XEN_INT(v->length)));
      if (nj >= v->length)
	mus_misc_error(S_vct_moveB,
		       "old-index too high?",
		       XEN_LIST_2(oldi,
				  C_TO_XEN_INT(v->length)));
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
      for (i = ni, j = nj; (j < v->length) && (i < v->length); i++, j++) 
	v->data[i] = v->data[j];
    }
  return(obj);
}

static XEN vct_length(XEN obj)
{
  #define H_vct_length "(" S_vct_length " v) -> length of vct v"
  vct *v;
  XEN_ASSERT_TYPE(VCT_P(obj), obj, XEN_ONLY_ARG, S_vct_length, "a vct");
  v = TO_VCT(obj);
  return(C_TO_XEN_INT(v->length));
}

/* static XEN reckless_vct_ref(XEN obj, XEN pos) {return(C_TO_XEN_DOUBLE(((vct *)XEN_OBJECT_REF(obj))->data[XEN_TO_SMALL_C_INT(pos)]));} */
/* this is about 4% faster than the checked version */

static XEN vct_ref(XEN obj, XEN pos)
{
  #define H_vct_ref "(" S_vct_ref " v n) -> element n of vct v, v[n]"
  vct *v;
  int loc;
  XEN_ASSERT_TYPE(VCT_P(obj), obj, XEN_ARG_1, S_vct_ref, "a vct");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(pos), pos, XEN_ARG_2, S_vct_ref, "an integer");
  v = TO_VCT(obj);
  loc = XEN_TO_C_INT(pos);
  if (loc < 0)
    mus_misc_error(S_vct_ref, "index < 0?", pos);
  if (loc >= v->length)
    mus_misc_error(S_vct_ref, "index too high?", XEN_LIST_2(pos, C_TO_XEN_INT(v->length)));
  return(C_TO_XEN_DOUBLE(v->data[loc]));
}

static XEN vct_set(XEN obj, XEN pos, XEN val)
{
  #define H_vct_setB "(" S_vct_setB " v n val) sets element n of vct v to val, v[n] = val"
  vct *v;
  int loc;
  XEN_ASSERT_TYPE(VCT_P(obj), obj, XEN_ARG_1, S_vct_setB, "a vct");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(pos), pos, XEN_ARG_2, S_vct_setB, "an integer");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ARG_3, S_vct_setB, "a number");
  v = TO_VCT(obj);
  loc = XEN_TO_C_INT(pos);
  if (loc < 0)
    mus_misc_error(S_vct_setB, "index < 0?", pos); 
  if (loc >= v->length)
    mus_misc_error(S_vct_setB, "index >= vct-length?", XEN_LIST_2(pos, C_TO_XEN_INT(v->length)));
  v->data[loc] = XEN_TO_C_DOUBLE(val);
  return(xen_return_first(val, obj, pos));
}

static XEN vct_multiply(XEN obj1, XEN obj2)
{
  #define H_vct_multiplyB "(" S_vct_multiplyB " v1 v2) -> v1 with element-wise multiply of vcts v1 and v2: v1[i] *= v2[i]"
  int i, lim;
  vct *v1, *v2;
  XEN_ASSERT_TYPE(VCT_P(obj1), obj1, XEN_ARG_1, S_vct_multiplyB, "a vct");
  XEN_ASSERT_TYPE(VCT_P(obj2), obj2, XEN_ARG_2, S_vct_multiplyB, "a vct");
  v1 = TO_VCT(obj1);
  v2 = TO_VCT(obj2);
  lim = MIN(v1->length, v2->length);
  for (i = 0; i < lim; i++) v1->data[i] *= v2->data[i];
  return(xen_return_first(obj1, obj2)); /* I wonder if this is necessary */
}

static XEN vct_add(XEN obj1, XEN obj2, XEN offs)
{
  #define H_vct_addB "(" S_vct_addB " v1 v2 &optional (offset 0)) -> v1 with element-wise add of vcts v1 and v2: v1[i+offset] += v2[i]"
  int i, lim, j;
  vct *v1, *v2;
  XEN_ASSERT_TYPE(VCT_P(obj1), obj1, XEN_ARG_1, S_vct_addB, "a vct");
  XEN_ASSERT_TYPE(VCT_P(obj2), obj2, XEN_ARG_2, S_vct_addB, "a vct");
  v1 = TO_VCT(obj1);
  v2 = TO_VCT(obj2);
  lim = MIN(v1->length, v2->length);
  if (XEN_INTEGER_P(offs))
    for (i = 0, j = XEN_TO_C_INT(offs); i < lim; i++, j++) 
      v1->data[j] += v2->data[i];
  else
    for (i = 0; i < lim; i++) 
      v1->data[i] += v2->data[i];
  return(xen_return_first(obj1, obj2));
}

static XEN vct_subtract(XEN obj1, XEN obj2)
{
  #define H_vct_subtractB "(" S_vct_subtractB " v1 v2) -> v1 with element-wise subtract of vcts v1 and v2: v1[i] -= v2[i]"
  int i, lim;
  vct *v1, *v2;
  XEN_ASSERT_TYPE(VCT_P(obj1), obj1, XEN_ARG_1, S_vct_subtractB, "a vct");
  XEN_ASSERT_TYPE(VCT_P(obj2), obj2, XEN_ARG_2, S_vct_subtractB, "a vct");
  v1 = TO_VCT(obj1);
  v2 = TO_VCT(obj2);
  lim = MIN(v1->length, v2->length);
  for (i = 0; i < lim; i++) v1->data[i] -= v2->data[i];
  return(xen_return_first(obj1, obj2));
}

static XEN vct_scale(XEN obj1, XEN obj2)
{
  #define H_vct_scaleB "(" S_vct_scaleB " v val) -> v with each element scaled by val: v[i] *= val"
  int i;
  vct *v1;
  Float scl;
  XEN_ASSERT_TYPE(VCT_P(obj1), obj1, XEN_ARG_1, S_vct_scaleB, "a vct");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(obj2), obj2, XEN_ARG_2, S_vct_scaleB, "a number");
  v1 = TO_VCT(obj1);
  scl = XEN_TO_C_DOUBLE(obj2);
  for (i = 0; i < v1->length; i++) v1->data[i] *= scl;
  return(xen_return_first(obj1, obj2));
}

static XEN vct_offset(XEN obj1, XEN obj2)
{
  #define H_vct_offsetB "(" S_vct_offsetB " v val) -> v with val added to each element: v[i] += val"
  int i;
  vct *v1;
  Float scl;
  XEN_ASSERT_TYPE(VCT_P(obj1), obj1, XEN_ARG_1, S_vct_offsetB, "a vct");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(obj2), obj2, XEN_ARG_2, S_vct_offsetB, "a number");
  v1 = TO_VCT(obj1);
  scl = XEN_TO_C_DOUBLE(obj2);
  for (i = 0; i < v1->length; i++) v1->data[i] += scl;
  return(xen_return_first(obj1, obj2));
}

static XEN vct_fill(XEN obj1, XEN obj2)
{
  #define H_vct_fillB "(" S_vct_fillB " v val) -> v with each element set to val: v[i] = val"
  int i;
  vct *v1;
  Float scl;
  XEN_ASSERT_TYPE(VCT_P(obj1), obj1, XEN_ARG_1, S_vct_fillB, "a vct");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(obj2), obj2, XEN_ARG_2, S_vct_fillB, "a number");
  v1 = TO_VCT(obj1);
  scl = XEN_TO_C_DOUBLE(obj2);
  for (i = 0; i < v1->length; i++) v1->data[i] = scl;
  return(xen_return_first(obj1, obj2));
}

static XEN vct_mapB(XEN obj, XEN proc)
{
  #define H_vct_mapB "(" S_vct_mapB " v proc) -> v with each element set to value of proc: v[i] = (proc)"
  int i;
  vct *v;
  XEN_ASSERT_TYPE(VCT_P(obj), obj, XEN_ARG_1, S_vct_mapB, "a vct");
  v = TO_VCT(obj);
#if WITH_RUN
  {
    void *pt = NULL;
    if ((optimization(get_global_state())) > 0)
      {
	pt = form_to_ptree_0f2f(proc);
	if (pt)
	  {
	    for (i = 0; i < v->length; i++) 
	      v->data[i] = evaluate_ptree_0f2f(pt);
	    free_ptree(pt);
	    return(xen_return_first(obj, proc));
	  }
      }
    proc = XEN_CADR(proc);
    XEN_ASSERT_TYPE(XEN_PROCEDURE_P(proc) && (XEN_REQUIRED_ARGS(proc) == 0), proc, XEN_ARG_2, S_vct_mapB, "a thunk");
    for (i = 0; i < v->length; i++) 
      v->data[i] = XEN_TO_C_DOUBLE(XEN_CALL_0_NO_CATCH(proc, S_vct_mapB));
  }
#else
  XEN_ASSERT_TYPE(XEN_PROCEDURE_P(proc) && (XEN_REQUIRED_ARGS(proc) == 0), proc, XEN_ARG_2, S_vct_mapB, "a thunk");
  for (i = 0; i < v->length; i++) 
    v->data[i] = XEN_TO_C_DOUBLE(XEN_CALL_0_NO_CATCH(proc, S_vct_mapB));
#endif
  return(xen_return_first(obj, proc));
}

static XEN vct_peak(XEN obj)
{
  #define H_vct_peak "(" S_vct_peak " v) -> max of abs of elements of v"
  int i;
  Float val = 0.0, absv;
  vct *v;
  XEN_ASSERT_TYPE(VCT_P(obj), obj, XEN_ONLY_ARG, S_vct_peak, "a vct");
  v = TO_VCT(obj);
  val = fabs(v->data[0]); 
  for (i = 1; i < v->length; i++) 
    {
      absv = fabs(v->data[i]); 
      if (absv > val) val = absv;
    }
  return(xen_return_first(C_TO_XEN_DOUBLE(val), obj));
}

static XEN vct_subseq(XEN vobj, XEN start, XEN end, XEN newv)
{
  #define H_vct_subseq "(" S_vct_subseq " v start end &optional vnew) -> vnew with vals v[start..end]"
  vct *vold, *vnew;
  XEN res;
  int i, old_len, new_len, j;
  XEN_ASSERT_TYPE(VCT_P(vobj), vobj, XEN_ARG_1, S_vct_subseq, "a vct");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(start), start, XEN_ARG_2, S_vct_subseq, "an integer");
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(end), end, XEN_ARG_3, S_vct_subseq, "an integer");
  vold = TO_VCT(vobj);
  old_len = vold->length;
  if (XEN_INTEGER_P(end))
    new_len = XEN_TO_C_INT(end) - XEN_TO_C_INT(start) + 1;
  else new_len = old_len - XEN_TO_C_INT(start);
  if (new_len <= 0) 
    return(XEN_FALSE);
  if (VCT_P(newv))
    res = newv;
  else res = make_vct(new_len, (Float *)CALLOC(new_len, sizeof(Float)));
  vnew = TO_VCT(res);
  if (new_len > vnew->length) 
    new_len = vnew->length;
  for (i = XEN_TO_C_INT(start), j = 0; (j < new_len) && (i < old_len); i++, j++)
    vnew->data[j] = vold->data[i];
  return(xen_return_first(res, vobj, vnew));
}

static XEN list2vct(XEN lst)
{
  #define H_list2vct "(" S_list2vct " lst) -> a new vct object filled with elements of list lst"
  int len, i;
  vct *v;
  XEN scv; XEN lst1;
  XEN_ASSERT_TYPE(XEN_LIST_P_WITH_LENGTH(lst, len), lst, XEN_ONLY_ARG, S_list2vct, "a list");
  if (len == 0)
    return(XEN_FALSE);
  scv = make_vct(len, (Float *)CALLOC(len, sizeof(Float)));
  v = TO_VCT(scv);
  for (i = 0, lst1 = XEN_COPY_ARG(lst); i < len; i++, lst1 = XEN_CDR(lst1)) 
    v->data[i] = (Float)XEN_TO_C_DOUBLE(XEN_CAR(lst1));
  return(xen_return_first(scv, lst));
}

static XEN g_vct(XEN args) 
{
  #define H_vct "(" S_vct " args -> vct with contents as args"
  return(list2vct(args));
}

XEN mus_array_to_list(Float *arr, int i, int len)
{
  if (i < (len - 1))
    return(XEN_CONS(C_TO_XEN_DOUBLE(arr[i]), 
		    mus_array_to_list(arr, i + 1, len)));
  else return(XEN_CONS(C_TO_XEN_DOUBLE(arr[i]), 
		       XEN_EMPTY_LIST));
}

static XEN vct2list(XEN vobj)
{
  #define H_vct2list "(" S_vct2list " v) -> a new list with elements of vct v"
  vct *v;
  XEN_ASSERT_TYPE(VCT_P(vobj), vobj, XEN_ONLY_ARG, S_vct2list, "a vct");
  v = TO_VCT(vobj);
  return(xen_return_first(mus_array_to_list(v->data, 0, v->length), vobj));
}

static XEN vector2vct(XEN vect)
{
  #define H_vector2vct "(" S_vector2vct " vect) -> a new vct object with the elements of vector vect"
  int len, i;
  vct *v;
  XEN *vdata;
  XEN scv;
  XEN_ASSERT_TYPE(XEN_VECTOR_P(vect), vect, XEN_ONLY_ARG, S_vector2vct, "a vector");
  len = XEN_VECTOR_LENGTH(vect);
  if (len == 0) return(XEN_FALSE);
  scv = make_vct(len, (Float *)CALLOC(len, sizeof(Float)));
  v = TO_VCT(scv);
  vdata = XEN_VECTOR_ELEMENTS(vect);
  for (i = 0; i < len; i++) 
    v->data[i] = (Float)XEN_TO_C_DOUBLE(vdata[i]);
  return(xen_return_first(scv, vect));
}

XEN vct2vector(XEN vobj)
{
  #define H_vct2vector "(" S_vct2vector " vct) -> a new vector with the elements of vct"
  vct *v;
  int i, len;
  XEN new_vect;
  XEN *vdata;
  XEN_ASSERT_TYPE(VCT_P(vobj), vobj, XEN_ONLY_ARG, S_vct2vector, "a vct");
  v = TO_VCT(vobj);
  len = v->length;
  new_vect = XEN_MAKE_VECTOR(len, C_TO_XEN_DOUBLE(0.0));
  vdata = XEN_VECTOR_ELEMENTS(new_vect);
  /* VECTOR_SET here */
  for (i = 0; i < len; i++) 
    vdata[i] = C_TO_XEN_DOUBLE(v->data[i]);
  return(xen_return_first(new_vect, vobj));
}

vct *vector_to_vct(XEN vect)
{
  int len, i;
  vct *v;
  XEN *vdata;
  len = XEN_VECTOR_LENGTH(vect);
  if (len == 0) return(NULL);
  v = c_make_vct(len);
  vdata = XEN_VECTOR_ELEMENTS(vect);
  for (i = 0; i < len; i++) 
    if (XEN_DOUBLE_P(vdata[i]))
      v->data[i] = (Float)XEN_TO_C_DOUBLE(vdata[i]);
    else
      {
	c_free_vct(v);
	return(NULL);
      }
  return(v);
}

void vct_into_vector(vct *v, XEN vect)
{
  int len, i;
  XEN *vdata;
  len = XEN_VECTOR_LENGTH(vect);
  vdata = XEN_VECTOR_ELEMENTS(vect);
  /* VECTOR_SET here */
  for (i = 0; i < len; i++) 
    vdata[i] = C_TO_XEN_DOUBLE(v->data[i]);
}


#ifdef XEN_ARGIFY_1
XEN_ARGIFY_2(g_make_vct_w, g_make_vct)
XEN_NARGIFY_1(copy_vct_w, copy_vct)
XEN_NARGIFY_1(g_vct_p_w, g_vct_p)
XEN_NARGIFY_1(list2vct_w, list2vct)
XEN_NARGIFY_1(vct2list_w, vct2list)
XEN_NARGIFY_1(vector2vct_w, vector2vct)
XEN_NARGIFY_1(vct2vector_w, vct2vector)
XEN_NARGIFY_1(vct_length_w, vct_length)
XEN_NARGIFY_2(vct_ref_w, vct_ref)
XEN_NARGIFY_3(vct_set_w, vct_set)
XEN_NARGIFY_2(vct_multiply_w, vct_multiply)
XEN_NARGIFY_2(vct_scale_w, vct_scale)
XEN_NARGIFY_2(vct_fill_w, vct_fill)
XEN_ARGIFY_3(vct_add_w, vct_add)
XEN_NARGIFY_2(vct_subtract_w, vct_subtract)
XEN_NARGIFY_2(vct_offset_w, vct_offset)
XEN_NARGIFY_2(vct_mapB_w, vct_mapB)
XEN_NARGIFY_1(vct_peak_w, vct_peak)
XEN_ARGIFY_4(vct_move_w, vct_move)
XEN_ARGIFY_4(vct_subseq_w, vct_subseq)
XEN_VARGIFY(g_vct_w, g_vct)
#else
#define g_make_vct_w g_make_vct
#define copy_vct_w copy_vct
#define g_vct_p_w g_vct_p
#define list2vct_w list2vct
#define vct2list_w vct2list
#define vector2vct_w vector2vct
#define vct2vector_w vct2vector
#define vct_length_w vct_length
#define vct_ref_w vct_ref
#define vct_set_w vct_set
#define vct_multiply_w vct_multiply
#define vct_scale_w vct_scale
#define vct_fill_w vct_fill
#define vct_add_w vct_add
#define vct_subtract_w vct_subtract
#define vct_offset_w vct_offset
#define vct_mapB_w vct_mapB
#define vct_peak_w vct_peak
#define vct_move_w vct_move
#define vct_subseq_w vct_subseq
#define g_vct_w g_vct
#define vct_ref_w vct_ref
#endif

#if HAVE_RUBY
static XEN vct_each(XEN obj)
{
  long i;
  vct *v;
  v = (vct *)XEN_OBJECT_REF(obj);
  for (i = 0; i < v->length; i++)
    rb_yield(C_TO_XEN_DOUBLE(v->data[i]));
  return(obj);
}

static XEN vct_compare(XEN vr1, XEN vr2)
{
  long i, len;
  vct *v1, *v2;
  if ((VCT_P(vr1)) && (VCT_P(vr2)))
    {
      v1 = (vct *)XEN_OBJECT_REF(vr1);
      v2 = (vct *)XEN_OBJECT_REF(vr2);
      len = v1->length;
      if (len > v2->length) len = v2->length;
      for (i = 0; i < len; i++) 
	if (v1->data[i] < v2->data[i])
	  return(C_TO_XEN_INT(-1));
	else
	  if (v1->data[i] > v2->data[i])
	    return(C_TO_XEN_INT(1));
      len = v1->length - v2->length;
      if (len == 0) return(C_TO_XEN_INT(0));
      if (len > 0) return(C_TO_XEN_INT(1));
    }
  return(C_TO_XEN_INT(-1));
}

#endif

#if WITH_MODULES
static void vct_init(void *ignore)
#else
void init_vct(void)
#endif
{
  vct_tag = XEN_MAKE_OBJECT_TYPE("Vct", sizeof(vct));
#if HAVE_GUILE
  scm_set_smob_print(vct_tag, print_vct);
  scm_set_smob_free(vct_tag, free_vct);
  scm_set_smob_equalp(vct_tag, equalp_vct);
#if HAVE_APPLICABLE_SMOB
  scm_set_smob_apply(vct_tag, XEN_PROCEDURE_CAST vct_ref, 1, 0, 0);
#endif
#endif
#if HAVE_RUBY
  rb_include_module(vct_tag, rb_mComparable);
  rb_include_module(vct_tag, rb_mEnumerable);
  rb_define_method(vct_tag, "to_s", XEN_PROCEDURE_CAST print_vct, 0);
  rb_define_method(vct_tag, "eql?", XEN_PROCEDURE_CAST equalp_vct, 1);
  rb_define_method(vct_tag, "[]", XEN_PROCEDURE_CAST vct_ref, 1);
  rb_define_method(vct_tag, "[]=", XEN_PROCEDURE_CAST vct_set, 2);
  rb_define_method(vct_tag, "length", XEN_PROCEDURE_CAST vct_length, 0);
  rb_define_method(vct_tag, "each", XEN_PROCEDURE_CAST vct_each, 0);
  rb_define_method(vct_tag, "<=>", XEN_PROCEDURE_CAST vct_compare, 1);
  /* many more could be added */
#endif

  XEN_DEFINE_PROCEDURE(S_make_vct,      g_make_vct_w, 1, 1, 0,    H_make_vct);
  XEN_DEFINE_PROCEDURE(S_vct_copy,      copy_vct_w, 1, 0, 0,      H_vct_copy);
  XEN_DEFINE_PROCEDURE(S_vct_p,         g_vct_p_w, 1, 0, 0,       H_vct_p);
  XEN_DEFINE_PROCEDURE(S_list2vct,      list2vct_w, 1, 0, 0,      H_list2vct);
  XEN_DEFINE_PROCEDURE(S_vct2list,      vct2list_w, 1, 0, 0,      H_vct2list);
  XEN_DEFINE_PROCEDURE(S_vector2vct,    vector2vct_w, 1, 0, 0,    H_vector2vct);
  XEN_DEFINE_PROCEDURE(S_vct2vector,    vct2vector_w, 1, 0, 0,    H_vct2vector);
  XEN_DEFINE_PROCEDURE(S_vct_length,    vct_length_w, 1, 0, 0,    H_vct_length);
  XEN_DEFINE_PROCEDURE(S_vct_multiplyB, vct_multiply_w, 2, 0, 0,  H_vct_multiplyB);
  XEN_DEFINE_PROCEDURE(S_vct_scaleB,    vct_scale_w, 2, 0, 0,     H_vct_scaleB);
  XEN_DEFINE_PROCEDURE(S_vct_fillB,     vct_fill_w, 2, 0, 0,      H_vct_fillB);
  XEN_DEFINE_PROCEDURE(S_vct_addB,      vct_add_w, 2, 1, 0,       H_vct_addB);
  XEN_DEFINE_PROCEDURE(S_vct_subtractB, vct_subtract_w, 2, 0, 0,  H_vct_subtractB);
  XEN_DEFINE_PROCEDURE(S_vct_offsetB,   vct_offset_w, 2, 0, 0,    H_vct_offsetB);
  XEN_DEFINE_PROCEDURE(S_vct_peak,      vct_peak_w, 1, 0, 0,      H_vct_peak);
  XEN_DEFINE_PROCEDURE(S_vct_moveB,     vct_move_w, 3, 1, 0,      H_vct_moveB);
  XEN_DEFINE_PROCEDURE(S_vct_subseq,    vct_subseq_w, 2, 2, 0,    H_vct_subseq);
  XEN_DEFINE_PROCEDURE(S_vct,           g_vct_w, 0, 0, 1,         H_vct);
#if WITH_RUN
  XEN_DEFINE_PROCEDURE("vct-map-1",     vct_mapB_w, 2, 0, 0,      H_vct_mapB);
  XEN_EVAL_C_STRING("(defmacro vct-map! (v form) `(vct-map-1 ,v (list ',form ,form)))");
  XEN_SET_DOCUMENTATION(S_vct_mapB, H_vct_mapB);
#else
  XEN_DEFINE_PROCEDURE(S_vct_mapB,      vct_mapB_w, 2, 0, 0,      H_vct_mapB);
#endif

#if HAVE_GUILE
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_vct_ref, vct_ref_w, H_vct_ref, "set-" S_vct_ref, vct_set_w,  2, 0, 3, 0);
#else
  XEN_DEFINE_PROCEDURE(S_vct_ref,       vct_ref_w, 2, 0, 0,       H_vct_ref);
#endif
  XEN_DEFINE_PROCEDURE(S_vct_setB,      vct_set_w, 3, 0, 0,       H_vct_setB);

#if WITH_MODULES
  scm_c_export(S_make_vct,
	       S_vct_copy,
	       S_vct_p,
	       S_list2vct,
	       S_vct2list,
	       S_vector2vct,
	       S_vct2vector,
	       S_vct_length,
	       S_vct_multiplyB,
	       S_vct_scaleB,
	       S_vct_fillB,
	       S_vct_addB,
	       S_vct_subtractB,
	       S_vct_offsetB,
	       S_vct_peak,
	       S_vct_moveB,
	       S_vct_subseq,
	       S_vct,
	       S_vct_mapB,
	       S_vct_ref,
	       S_vct_setB,
#if WITH_RUN
	       "vct-map-1",
#endif
	       NULL);
#endif
}

#if WITH_MODULES
void init_vct(void)
{
  scm_c_define_module("snd sndlib", vct_init, NULL);
}
#endif

