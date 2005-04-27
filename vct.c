/* vct support 
 *
 * a vct is a Guile "smob" containing a Float array and its size
 * we use these in Snd because Guile's floating-point and vector operations are slow,
 * and Snd/CLM are applications where no user is willing to wait on a multiply.
 *
 * C side:
 *   void vct_init(void)                   called to declare the various functions and the vct type in Guile
 *   bool vct_p(XEN obj)                   is obj a vct
 *   XEN make_vct(int len, Float *data)    make a new vct
 *   XEN make_vct_wrapper(int len, Float *data) make a new vct that doesn't free data when garbage collector strikes
 *   vct *get_vct(XEN arg)                 given XEN arg, return vct
 *   void set_vct_print_length(int val)    set vct print length (default 10)
 *   vct *c_vct_copy(vct *vc)              return copy of vc
 *
 * Scheme side:
 *   (make-vct len (filler 0.0))      make new vct
 *   (vct? obj)                       is obj a vct
 *   (vct-ref v index)                return v[index]
 *   (vct-set! v index val)           v[index] = val
 *   (vct-copy v)                     return a copy of v
 *   (vct-length v)                   return length of v
 *   (vct-add! v1 v2 (offset 0))      v1[i+offset] = v1[i+offset] + v2[i] -> v1
 *   (vct-subtract! v1 v2)            v1[i] = v1[i] - v2[i] -> v1
 *   (vct-offset! v1 scl)             v1[i] += scl -> v1
 *   (vct-multiply! v1 v2)            v1[i] *= v2[i] -> v1
 *   (vct-scale! v1 scl)              v1[i] *= scl -> v1
 *   (vct-fill! v1 val)               v1[i] = val -> v1
 *   (vct-map! v1 proc)               set each element of v1 to value of function proc()
 *   (vct-peak v1)                    max val (abs) in v
 *   (list->vct lst)                  return vct with elements of list lst
 *   (vct->list v1)                   return list with elements of vct v1
 *   (vector->vct vect)               return vct with elements of vector vect
 *   (vct->vector v)                  return vector of vct contents
 *   (vct-move! v new old)            v[new++] = v[old++] -> v
 *   (vct-subseq v start end vnew)    vnew = v[start..end]
 *   (vct-reverse v (len #f))         reverse contents (using len as end point if given)
 *   (vct->string v)                  scheme-readable description of vct
 *
 *   (vct* obj1 obj2) combines vct-multiply and vct-scale
 *   (vct+ obj1 obj2) combines vct-add and vct-offset
 *
 * The intended use is a sort of latter-day array-processing system that handles huge
 * one-dimensional vectors -- fft's, etc.  Some of these functions can be found in
 * the Snd package; others can be found in the CLM package (clm2xen.c).
 */

#include <config.h>

#if USE_SND
  #include "snd.h"
#else
  #define PRINT_BUFFER_SIZE 512
  #define LABEL_BUFFER_SIZE 64
#endif

#include <math.h>
#include <stdlib.h>
#include <stdio.h>
#if HAVE_STRING_H
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

bool vct_p(XEN obj)
{
  return(XEN_OBJECT_TYPE_P(obj, vct_tag));
}

static XEN g_vct_p(XEN obj) 
{
  #define H_vct_p "(" S_vct_p " obj): is obj a vct"
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
      if ((!(v->dont_free)) && 
	  (v->data)) 
	FREE(v->data);
      v->data = NULL;
      FREE(v);
    }
}

XEN_MAKE_OBJECT_FREE_PROCEDURE(vct, free_vct, vct_free)

char *vct_to_string(vct *v)
{
  int len;
  char *buf;
  char flt[16];
  if (v == NULL) return(NULL);
  len = vct_print_length;
  if (len > v->length) len = v->length;
  buf = (char *)CALLOC(64 + len * 16, sizeof(char));
  sprintf(buf, "#<vct[len=%d]:", v->length);
  if (len > 0)
    {
      int i;
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

char *vct_to_readable_string(vct *v)
{
  int i, len;
  char *buf;
  char flt[16];
  if (v == NULL) return(NULL);
  len = v->length;
  buf = (char *)CALLOC(64 + len * 16, sizeof(char));
#if HAVE_GUILE
  sprintf(buf, "(vct");
#else
  sprintf(buf, "vct(");
#endif
  for (i = 0; i < len; i++)
    {
#if HAVE_GUILE
      mus_snprintf(flt, 16, " %.3f", v->data[i]);
#else
      mus_snprintf(flt, 16, "%.3f%s", v->data[i], i + 1 < len ? ", " : "");
#endif
      strcat(buf, flt);
    }
  strcat(buf, ")");
  return(buf);
}

static XEN g_vct_to_readable_string(XEN obj)
{
  char *vstr;
  XEN result;
  #define H_vct_to_string "(" S_vct_to_string " v) -> scheme readable description of v"
  XEN_ASSERT_TYPE(VCT_P(obj), obj, XEN_ONLY_ARG, S_vct_to_string, "a vct");
  vstr = vct_to_readable_string(TO_VCT(obj));
  result = C_TO_XEN_STRING(vstr);
  FREE(vstr);
  return(result);
}

bool vct_equalp(vct *v1, vct *v2)
{
  int i;
  if (v1->length != v2->length) 
    return(false);
  for (i = 0; i < v1->length; i++)
    if (v1->data[i] != v2->data[i])
      return(false);
  return(true);
}

XEN_MAKE_OBJECT_PRINT_PROCEDURE(vct, print_vct, vct_to_string)

static XEN equalp_vct(XEN obj1, XEN obj2)
{
  return(xen_return_first(C_TO_XEN_BOOLEAN(vct_equalp((vct *)XEN_OBJECT_REF(obj1), (vct *)XEN_OBJECT_REF(obj2))), obj1, obj2));
}

vct *c_make_vct(int len)
{
  vct *new_vct;
  new_vct = (vct *)MALLOC(sizeof(vct));
  new_vct->length = len;
  new_vct->data = (Float *)CALLOC(len, sizeof(Float));
  new_vct->dont_free = false;
  return(new_vct);
}

vct *c_free_vct(vct *v) 
{
  vct_free(v);
  return(NULL);
}

vct *c_vct_copy(vct *vc)
{
  vct *v = NULL;
  int len;
  if (vc)
    {
      len = vc->length;
      v = c_make_vct(len);
      memcpy((void *)(v->data), (void *)(vc->data), (len * sizeof(Float)));
    }
  return(v);
}

XEN make_vct(int len, Float *data)
{
  vct *new_vct;
  if (len <= 0) return(XEN_FALSE);
  new_vct = (vct *)MALLOC(sizeof(vct));
  new_vct->length = len;
  new_vct->data = data;
  new_vct->dont_free = false;
  XEN_MAKE_AND_RETURN_OBJECT(vct_tag, new_vct, 0, free_vct);
}

XEN make_vct_wrapper(int len, Float *data)
{
  vct *new_vct;
  new_vct = (vct *)MALLOC(sizeof(vct));
  new_vct->length = len;
  new_vct->data = data;
  new_vct->dont_free = true;
  XEN_MAKE_AND_RETURN_OBJECT(vct_tag, new_vct, 0, free_vct);
}

static XEN vct_fill(XEN obj1, XEN obj2);

static XEN g_make_vct(XEN len, XEN filler)
{
  #define H_make_vct "(" S_make_vct " len (initial-element 0)): returns a new vct of length len filled with initial-element"
  int size;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(len), len, XEN_ONLY_ARG, S_make_vct, "an integer");
  size = XEN_TO_C_INT(len);
  if (size <= 0) 
    XEN_OUT_OF_RANGE_ERROR(S_make_vct, 1, len, "len ~A <= 0?");
  if (XEN_NUMBER_P(filler))
    return(vct_fill(make_vct(size, (Float *)CALLOC(size, sizeof(Float))), filler));
  return(make_vct(size, (Float *)CALLOC(size, sizeof(Float))));
}

static XEN copy_vct(XEN obj)
{
  #define H_vct_copy "(" S_vct_copy " v): returns a copy of vct v"
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
  #define H_vct_moveB "(" S_vct_moveB " obj new old (backwards #f)): moves vct obj data from old to new: v[new++] = v[old++], or \
v[new--] = v[old--] if backwards is #t"
  vct *v;
  int i, j, ni, nj;
  XEN_ASSERT_TYPE(VCT_P(obj), obj, XEN_ARG_1, S_vct_moveB, "a vct");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(newi), newi, XEN_ARG_2, S_vct_moveB, "an integer");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(oldi), oldi, XEN_ARG_3, S_vct_moveB, "an integer");
  XEN_ASSERT_TYPE(XEN_BOOLEAN_IF_BOUND_P(backwards), backwards, XEN_ARG_4, S_vct_moveB, "a boolean");
  v = TO_VCT(obj);
  ni = XEN_TO_C_INT(newi);
  nj = XEN_TO_C_INT(oldi);
  if ((XEN_BOOLEAN_P(backwards)) && 
      (XEN_NOT_FALSE_P(backwards)))
    {
      if (ni >= v->length) 
	XEN_OUT_OF_RANGE_ERROR(S_vct_moveB, 2, newi, "new-index ~A too high");
      if (nj >= v->length)
	XEN_OUT_OF_RANGE_ERROR(S_vct_moveB, 3, oldi, "old-index ~A too high");
      for (i = ni, j = nj; (j >= 0) && (i >= 0); i--, j--) 
	v->data[i] = v->data[j];
    }
  else
    {
      if (ni < 0)
	XEN_OUT_OF_RANGE_ERROR(S_vct_moveB, 2, newi, "new-index ~A < 0?");
      if (nj < 0)
	XEN_OUT_OF_RANGE_ERROR(S_vct_moveB, 3, oldi, "old-index ~A < 0?");
      for (i = ni, j = nj; (j < v->length) && (i < v->length); i++, j++) 
	v->data[i] = v->data[j];
    }
  return(obj);
}

static XEN vct_length(XEN obj)
{
  #define H_vct_length "(" S_vct_length " v): length of vct v"
  vct *v;
  XEN_ASSERT_TYPE(VCT_P(obj), obj, XEN_ONLY_ARG, S_vct_length, "a vct");
  v = TO_VCT(obj);
  return(C_TO_XEN_INT(v->length));
}

static XEN vct_ref(XEN obj, XEN pos)
{
  #define H_vct_ref "(" S_vct_ref " v n): element n of vct v, v[n]"
  vct *v;
  int loc;
  XEN_ASSERT_TYPE(VCT_P(obj), obj, XEN_ARG_1, S_vct_ref, "a vct");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(pos), pos, XEN_ARG_2, S_vct_ref, "an integer");
  v = TO_VCT(obj);
  loc = XEN_TO_C_INT(pos);
  if (loc < 0)
    XEN_OUT_OF_RANGE_ERROR(S_vct_ref, 2, pos, "index ~A < 0?");
  if (loc >= v->length)
    XEN_OUT_OF_RANGE_ERROR(S_vct_ref, 2, pos, "index ~A too high?");
  return(C_TO_XEN_DOUBLE(v->data[loc]));
}

static XEN vct_set(XEN obj, XEN pos, XEN val)
{
  #define H_vct_setB "(" S_vct_setB " v n val): sets element of vct v to val, v[n] = val"
  vct *v;
  int loc;
  XEN_ASSERT_TYPE(VCT_P(obj), obj, XEN_ARG_1, S_vct_setB, "a vct");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(pos), pos, XEN_ARG_2, S_vct_setB, "an integer");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ARG_3, S_vct_setB, "a real number");
  v = TO_VCT(obj);
  loc = XEN_TO_C_INT(pos);
  if (loc < 0)
    XEN_OUT_OF_RANGE_ERROR(S_vct_setB, 2, pos, "index ~A < 0?"); 
  if (loc >= v->length)
    XEN_OUT_OF_RANGE_ERROR(S_vct_setB, 2, pos, "index ~A >= vct-length?");
  v->data[loc] = XEN_TO_C_DOUBLE(val);
  return(xen_return_first(val, obj, pos));
}

static XEN vct_multiply(XEN obj1, XEN obj2)
{
  #define H_vct_multiplyB "(" S_vct_multiplyB " v1 v2): element-wise multiply of vcts v1 and v2: v1[i] *= v2[i], returns v1"
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
  #define H_vct_addB "(" S_vct_addB " v1 v2 (offset 0)): element-wise add of vcts v1 and v2: v1[i + offset] += v2[i], returns v1"
  int i, lim, j;
  vct *v1, *v2;
  XEN_ASSERT_TYPE(VCT_P(obj1), obj1, XEN_ARG_1, S_vct_addB, "a vct");
  XEN_ASSERT_TYPE(VCT_P(obj2), obj2, XEN_ARG_2, S_vct_addB, "a vct");
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(offs), offs, XEN_ARG_3, S_vct_addB, "an integer");
  v1 = TO_VCT(obj1);
  v2 = TO_VCT(obj2);
  lim = MIN(v1->length, v2->length);
  if (XEN_INTEGER_P(offs))
    {
      j = XEN_TO_C_INT(offs); /* needed by g++ 3.2 -- otherwise segfault from the compiler! */
      if (j < 0) XEN_OUT_OF_RANGE_ERROR(S_vct_addB, 3, offs, "offset ~A < 0?");
      if ((j + lim) > v1->length)
	lim = (v1->length - j);
      for (i = 0; i < lim; i++, j++) 
	v1->data[j] += v2->data[i];
    }
  else
    for (i = 0; i < lim; i++) 
      v1->data[i] += v2->data[i];
  return(xen_return_first(obj1, obj2));
}

static XEN vct_subtract(XEN obj1, XEN obj2)
{
  #define H_vct_subtractB "(" S_vct_subtractB " v1 v2): element-wise subtract of vcts v1 and v2: v1[i] -= v2[i], returns v1"
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
  #define H_vct_scaleB "(" S_vct_scaleB " v val): scale each element of v by val: v[i] *= val, returns v"
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
  #define H_vct_offsetB "(" S_vct_offsetB " v val): add val to each element of v: v[i] += val, returns v"
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
  #define H_vct_fillB "(" S_vct_fillB " v val): set each element of v to val: v[i] = val, returns v"
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
  #define H_vct_mapB "(" S_vct_mapB " v proc): set each element of v to value of proc (a thunk): v[i] = (proc), returns \
v. (" S_vct_mapB " v (lambda () 3.0)) is the same as (" S_vct_fillB " v 3.0)"
  int i;
  vct *v;
  XEN_ASSERT_TYPE(VCT_P(obj), obj, XEN_ARG_1, S_vct_mapB, "a vct");
  v = TO_VCT(obj);
#if WITH_RUN && USE_SND
  {
    struct ptree *pt = NULL;
    if ((optimization(ss)) > 0)
      {
	pt = form_to_ptree_0_f(proc);
	if (pt)
	  {
	    for (i = 0; i < v->length; i++) 
	      v->data[i] = evaluate_ptree_0f2f(pt);
	    free_ptree(pt);
	    return(xen_return_first(obj, proc));
	  }
      }
    proc = XEN_CADR(proc);
    XEN_ASSERT_TYPE(XEN_PROCEDURE_P(proc) && (XEN_REQUIRED_ARGS_OK(proc, 0)), proc, XEN_ARG_2, S_vct_mapB, "a thunk");
    for (i = 0; i < v->length; i++) 
      v->data[i] = XEN_TO_C_DOUBLE(XEN_CALL_0_NO_CATCH(proc));
  }
#else
  XEN_ASSERT_TYPE(XEN_PROCEDURE_P(proc) && (XEN_REQUIRED_ARGS_OK(proc, 0)), proc, XEN_ARG_2, S_vct_mapB, "a thunk");
  for (i = 0; i < v->length; i++) 
    v->data[i] = XEN_TO_C_DOUBLE(XEN_CALL_0_NO_CATCH(proc));
#endif
  return(xen_return_first(obj, proc));
}

static XEN vct_peak(XEN obj)
{
  #define H_vct_peak "(" S_vct_peak " v): max of abs of elements of v"
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
  #define H_vct_subseq "(" S_vct_subseq " v start (end len) (vnew #f)): v[start..end], placed in vnew if given or new vct"
  vct *vold, *vnew;
  XEN res;
  int i, old_len, new_len, j, istart, iend;
  XEN_ASSERT_TYPE(VCT_P(vobj), vobj, XEN_ARG_1, S_vct_subseq, "a vct");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(start), start, XEN_ARG_2, S_vct_subseq, "an integer");
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(end), end, XEN_ARG_3, S_vct_subseq, "an integer");
  istart = XEN_TO_C_INT(start);
  if (istart < 0)
    XEN_OUT_OF_RANGE_ERROR(S_vct_subseq, 2, start, "start ~A < 0?");
  vold = TO_VCT(vobj);
  old_len = vold->length;
  if (XEN_INTEGER_P(end))
    {
      iend = XEN_TO_C_INT(end);
      if (iend < istart)
	XEN_OUT_OF_RANGE_ERROR(S_vct_subseq, 3, end, "end ~A < start?");
      new_len = iend - istart + 1;
    }
  else new_len = old_len - istart;
  if (new_len <= 0) 
    return(XEN_FALSE);
  if (VCT_P(newv))
    res = newv;
  else res = make_vct(new_len, (Float *)CALLOC(new_len, sizeof(Float)));
  vnew = TO_VCT(res);
  if (new_len > vnew->length) 
    new_len = vnew->length;
  for (i = istart, j = 0; (j < new_len) && (i < old_len); i++, j++)
    vnew->data[j] = vold->data[i];
  return(xen_return_first(res, vobj, vnew));
}

XEN list_to_vct(XEN lst)
{
  #define H_list_to_vct "(" S_list_to_vct " lst): returns a new vct filled with elements of list lst"
  int len = 0, i;
  vct *v;
  XEN scv, lst1;
  XEN_ASSERT_TYPE(XEN_LIST_P_WITH_LENGTH(lst, len), lst, XEN_ONLY_ARG, S_list_to_vct, "a list");
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
  #define H_vct "(" S_vct " args...) returns a new vct with args as contents; same as " S_list_to_vct ": (vct 1 2 3)"
  return(list_to_vct(args));
}

XEN mus_array_to_list(Float *arr, int i, int len)
{
  if (i < (len - 1))
    return(XEN_CONS(C_TO_XEN_DOUBLE(arr[i]), 
		    mus_array_to_list(arr, i + 1, len)));
  else return(XEN_CONS(C_TO_XEN_DOUBLE(arr[i]), 
		       XEN_EMPTY_LIST));
}

static XEN vct_to_list(XEN vobj)
{
  #define H_vct_to_list "(" S_vct_to_list " v): returns a new list with elements of vct v"
  vct *v;
  XEN_ASSERT_TYPE(VCT_P(vobj), vobj, XEN_ONLY_ARG, S_vct_to_list, "a vct");
  v = TO_VCT(vobj);
  return(xen_return_first(mus_array_to_list(v->data, 0, v->length), vobj));
}

static XEN vector_to_vct(XEN vect)
{
  #define H_vector_to_vct "(" S_vector_to_vct " vect): returns a new vct with the elements of vector vect"
  int len, i;
  vct *v;
  XEN scv;
  XEN_ASSERT_TYPE(XEN_VECTOR_P(vect), vect, XEN_ONLY_ARG, S_vector_to_vct, "a vector");
  len = XEN_VECTOR_LENGTH(vect);
  if (len == 0) return(XEN_FALSE);
  scv = make_vct(len, (Float *)CALLOC(len, sizeof(Float)));
  v = TO_VCT(scv);
  for (i = 0; i < len; i++) 
    v->data[i] = (Float)XEN_TO_C_DOUBLE(XEN_VECTOR_REF(vect, i));
  return(xen_return_first(scv, vect));
}

static XEN vct_to_vector(XEN vobj)
{
  #define H_vct_to_vector "(" S_vct_to_vector " vct): returns a new vector with the elements of vct"
  vct *v;
  int i, len;
  XEN new_vect;
  XEN_ASSERT_TYPE(VCT_P(vobj), vobj, XEN_ONLY_ARG, S_vct_to_vector, "a vct");
  v = TO_VCT(vobj);
  len = v->length;
  new_vect = XEN_MAKE_VECTOR(len, C_TO_XEN_DOUBLE(0.0));
#if HAVE_RUBY && HAVE_RB_GC_DISABLE
  rb_gc_disable(); 
  /* uh oh -- gc is triggered by C_TO_XEN_DOUBLE causing segfault, even if we
   *   protect (via XEN_PROTECT_FROM_GC) new_vect -- I guess the double currently
   *   being created is causing the trouble?
   */
#endif
  for (i = 0; i < len; i++) 
    XEN_VECTOR_SET(new_vect, i, C_TO_XEN_DOUBLE(v->data[i]));
#if HAVE_RUBY && HAVE_RB_GC_DISABLE
  rb_gc_enable();
#endif
  return(xen_return_first(new_vect, vobj));
}

static XEN vct_reverse(XEN vobj, XEN size)
{
  #define H_vct_reverse "(" S_vct_reverse " vct len): in-place reversal of vct contents"
  vct *v;
  int i, j, len = -1;
  XEN_ASSERT_TYPE(VCT_P(vobj), vobj, XEN_ARG_1, S_vct_to_vector, "a vct");
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(size), size, XEN_ARG_2, S_vct_to_vector, "an integer");
  v = TO_VCT(vobj);
  if (XEN_INTEGER_P(size))
    len = XEN_TO_C_INT(size);
  if ((len <= 0) || (len > v->length))
    len = v->length;
  if (len == 1) return(vobj);
  for (i = 0, j = len - 1; i < j; i++, j--)
    {
      Float temp;
      temp = v->data[i];
      v->data[i] = v->data[j];
      v->data[j] = temp;
    }
  return(vobj);
}

static XEN vct_times(XEN obj1, XEN obj2)
{
  #define H_vct_times "(" S_vct_times " obj1 obj2) is either " S_vct_multiplyB " or " S_vct_scaleB ", depending on the types of its arguments"
  if (VCT_P(obj1))
    {
      if (VCT_P(obj2))
	return(vct_multiply(obj1, obj2));
      return(vct_scale(obj1, obj2));
    }
  return(vct_scale(obj2, obj1));
}

static XEN vct_plus(XEN obj1, XEN obj2)
{
  #define H_vct_plus "(" S_vct_plus " obj1 obj2) is either " S_vct_addB " or " S_vct_offsetB ", depending on the types of its arguments"
  if (VCT_P(obj1))
    {
      if (VCT_P(obj2))
	return(vct_add(obj1, obj2, XEN_UNDEFINED));
      return(vct_offset(obj1, obj2));
    }
  return(vct_offset(obj2, obj1));
}


#ifdef XEN_ARGIFY_1
XEN_ARGIFY_2(g_make_vct_w, g_make_vct)
XEN_NARGIFY_1(copy_vct_w, copy_vct)
XEN_NARGIFY_1(g_vct_p_w, g_vct_p)
XEN_NARGIFY_1(list_to_vct_w, list_to_vct)
XEN_NARGIFY_1(vct_to_list_w, vct_to_list)
XEN_NARGIFY_1(vector_to_vct_w, vector_to_vct)
XEN_NARGIFY_1(vct_to_vector_w, vct_to_vector)
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
XEN_ARGIFY_2(vct_reverse_w, vct_reverse)
XEN_NARGIFY_1(g_vct_to_readable_string_w, g_vct_to_readable_string)
XEN_NARGIFY_2(vct_times_w, vct_times)
XEN_NARGIFY_2(vct_plus_w, vct_plus)
#else
#define g_make_vct_w g_make_vct
#define copy_vct_w copy_vct
#define g_vct_p_w g_vct_p
#define list_to_vct_w list_to_vct
#define vct_to_list_w vct_to_list
#define vector_to_vct_w vector_to_vct
#define vct_to_vector_w vct_to_vector
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
#define vct_reverse_w vct_reverse
#define g_vct_to_readable_string_w g_vct_to_readable_string
#define vct_times_w vct_times
#define vct_plus_w vct_plus
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

static XEN g_rb_make_vct(int argc, XEN *argv, XEN self)
{
  int size;
  XEN len, filler;
  rb_scan_args(argc, argv, "11", &len, &filler);
  XEN_ASSERT_TYPE(XEN_INTEGER_P(len), len, XEN_ONLY_ARG, "Vct.new", "an integer");
  size = XEN_TO_C_INT(len);
  if (size <= 0) 
    XEN_OUT_OF_RANGE_ERROR("Vct.new", 1, len, "len <= 0?");
  if (XEN_NUMBER_P(filler))
    return(vct_fill(make_vct(size, (Float *)CALLOC(size, sizeof(Float))), filler));
  if (rb_block_given_p()) {
    long i;
    Float *buffer = (Float *)CALLOC(size, sizeof(Float));
    for(i = 0; i < size; i++) {
      buffer[i] = XEN_TO_C_DOUBLE(rb_yield(C_TO_XEN_INT(i)));
    }
    return make_vct(size, buffer);
  }
  return(make_vct(size, (Float *)CALLOC(size, sizeof(Float))));
}

static XEN g_vct_map(XEN obj)
{
  if (rb_block_given_p()) {
    long i;
    vct *v = TO_VCT(obj);
    Float *buffer = (Float *)CALLOC(v->length, sizeof(Float));
    for(i = 0; i < v->length; i++)
      buffer[i] = XEN_TO_C_DOUBLE(rb_yield(C_TO_XEN_DOUBLE(v->data[i])));
    return make_vct(v->length, buffer);
  }
  return obj;
}

static XEN g_vct_map_store(XEN obj)
{
  if (rb_block_given_p()) {
    long i;
    vct *v = TO_VCT(obj);
    for(i = 0; i < v->length; i++)
      v->data[i] = XEN_TO_C_DOUBLE(rb_yield(C_TO_XEN_DOUBLE(v->data[i])));
  }
  return obj;
}

/* v1.add!(v2[,offset=0]) destructive */
static XEN rb_vct_add(int argc, XEN *argv, XEN obj1)
{
  XEN obj2, offs;
  rb_scan_args(argc, argv, "11", &obj2, &offs);
  return vct_add(obj1, obj2, (argc == 2) ? offs : XEN_UNDEFINED);
}

/* v1.add(v2[,offset=0]) returns new vct */
static XEN rb_vct_add_cp(int argc, XEN *argv, XEN obj1)
{
  XEN obj2, offs;
  rb_scan_args(argc, argv, "11", &obj2, &offs);
  return vct_add(copy_vct(obj1), obj2, (argc == 2) ? offs : XEN_UNDEFINED);
}

/* v1.subtract(v2) returns new vct */
static XEN rb_vct_subtract_cp(XEN obj1, XEN obj2)
{
  return vct_subtract(copy_vct(obj1), obj2);
}

static XEN rb_vct_offset_cp(XEN obj, XEN scl)
{
  return vct_offset(copy_vct(obj), scl);
}

static XEN rb_vct_multiply_cp(XEN obj1, XEN obj2)
{
  return vct_multiply(copy_vct(obj1), obj2);
}

static XEN rb_vct_scale_cp(XEN obj, XEN scl)
{
  return vct_scale(copy_vct(obj), scl);
}

#if 0
static XEN rb_vct_fill_cp(XEN obj, XEN scl)
{
  return vct_fill(copy_vct(obj), scl);
}
#endif

/* destructive */
static XEN rb_vct_move(int argc, XEN *argv, XEN obj)
{
  XEN vnew, old, backward;
  rb_scan_args(argc, argv, "21", &vnew, &old, &backward);
  return vct_move(obj, vnew, old, (argc == 3) ? backward : XEN_UNDEFINED);
}

/* returns new vct */
static XEN rb_vct_move_cp(int argc, XEN *argv, XEN obj)
{
  XEN vnew, old, backward;
  rb_scan_args(argc, argv, "21", &vnew, &old, &backward);
  return vct_move(copy_vct(obj), vnew, old, (argc == 3) ? backward : XEN_UNDEFINED);
}

static XEN rb_vct_subseq(int argc, XEN *argv, XEN obj)
{
  XEN start, end, vnew;
  rb_scan_args(argc, argv, "12", &start, &end, &vnew);
    return vct_subseq(obj, start, (argc > 1) ? end :XEN_UNDEFINED, (argc > 2) ? vnew : XEN_UNDEFINED);
}

/* destructive */
static XEN rb_vct_reverse(int argc, XEN *argv, XEN obj)
{
  XEN len;
  rb_scan_args(argc, argv, "01", &len);
  return vct_reverse(obj, (argc > 0) ? len : XEN_UNDEFINED);
}

/* returns new vct */
static XEN rb_vct_reverse_cp(int argc, XEN *argv, XEN obj)
{
  XEN len;
  rb_scan_args(argc, argv, "01", &len);
  return vct_reverse(copy_vct(obj), (argc > 0) ? len : XEN_UNDEFINED);
}

static XEN rb_vct_first(XEN obj)
{
  return vct_ref(obj, C_TO_XEN_INT(0));
}

static XEN rb_set_vct_first(XEN obj, XEN val)
{
  return vct_set(obj, C_TO_XEN_INT(0), val);
}

static XEN rb_vct_last(XEN obj)
{
  return vct_ref(obj, C_TO_XEN_INT(TO_VCT(obj)->length - 1));
}

static XEN rb_set_vct_last(XEN obj, XEN val)
{
  return vct_set(obj, C_TO_XEN_INT(TO_VCT(obj)->length - 1), val);
}
#endif

#if WITH_MODULES
static void vct_init_1(void *ignore)
#else
void vct_init(void)
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

  rb_define_method(vct_tag, "to_s",     XEN_PROCEDURE_CAST print_vct, 0);
  rb_define_method(vct_tag, "eql?",     XEN_PROCEDURE_CAST equalp_vct, 1);
  rb_define_method(vct_tag, "[]",       XEN_PROCEDURE_CAST vct_ref, 1);
  rb_define_method(vct_tag, "[]=",      XEN_PROCEDURE_CAST vct_set, 2);
  rb_define_method(vct_tag, "length",   XEN_PROCEDURE_CAST vct_length, 0);
  rb_define_method(vct_tag, "each",     XEN_PROCEDURE_CAST vct_each, 0);
  rb_define_method(vct_tag, "<=>",      XEN_PROCEDURE_CAST vct_compare, 1);
  rb_define_singleton_method(vct_tag, "new", XEN_PROCEDURE_CAST g_rb_make_vct, -1);
  rb_define_method(vct_tag, "map",      XEN_PROCEDURE_CAST g_vct_map, 0);
  rb_define_method(vct_tag, "map!",     XEN_PROCEDURE_CAST g_vct_map_store, 0);
  rb_define_method(vct_tag, "to_a",     XEN_PROCEDURE_CAST vct_to_vector, 0);
  rb_define_method(rb_cArray, "to_vct", XEN_PROCEDURE_CAST vector_to_vct, 0);

  rb_define_method(vct_tag, "to_str",    XEN_PROCEDURE_CAST g_vct_to_readable_string, 0);
  rb_define_method(vct_tag, "dup",       XEN_PROCEDURE_CAST copy_vct, 0);
  rb_define_method(vct_tag, "peak",      XEN_PROCEDURE_CAST vct_peak, 0);
  rb_define_method(vct_tag, "add",       XEN_PROCEDURE_CAST rb_vct_add_cp, -1);
  rb_define_method(vct_tag, "add!",      XEN_PROCEDURE_CAST rb_vct_add, -1);
  rb_define_method(vct_tag, "subtract",  XEN_PROCEDURE_CAST rb_vct_subtract_cp, 1);
  rb_define_method(vct_tag, "subtract!", XEN_PROCEDURE_CAST vct_subtract, 1);
  rb_define_method(vct_tag, "offset",    XEN_PROCEDURE_CAST rb_vct_offset_cp, 1);
  rb_define_method(vct_tag, "offset!",   XEN_PROCEDURE_CAST vct_offset, 1);
  rb_define_method(vct_tag, "multiply",  XEN_PROCEDURE_CAST rb_vct_multiply_cp, 1);
  rb_define_method(vct_tag, "multiply!", XEN_PROCEDURE_CAST vct_multiply, 1);
  rb_define_method(vct_tag, "scale",     XEN_PROCEDURE_CAST rb_vct_scale_cp, 1);
  rb_define_method(vct_tag, "scale!",    XEN_PROCEDURE_CAST vct_scale, 1);
  rb_define_method(vct_tag, "fill",      XEN_PROCEDURE_CAST vct_fill, 1);
  rb_define_method(vct_tag, "move",      XEN_PROCEDURE_CAST rb_vct_move_cp, -1);
  rb_define_method(vct_tag, "move!",     XEN_PROCEDURE_CAST rb_vct_move, -1);
  rb_define_method(vct_tag, "subseq",    XEN_PROCEDURE_CAST rb_vct_subseq, -1);
  rb_define_method(vct_tag, "reverse",   XEN_PROCEDURE_CAST rb_vct_reverse_cp, -1);
  rb_define_method(vct_tag, "reverse!",  XEN_PROCEDURE_CAST rb_vct_reverse, -1);
  rb_define_method(vct_tag, "first",     XEN_PROCEDURE_CAST rb_vct_first, 0);
  rb_define_method(vct_tag, "first=",    XEN_PROCEDURE_CAST rb_set_vct_first, 1);
  rb_define_method(vct_tag, "last",      XEN_PROCEDURE_CAST rb_vct_last, 0);
  rb_define_method(vct_tag, "last=",     XEN_PROCEDURE_CAST rb_set_vct_last, 1);

#endif

  XEN_DEFINE_PROCEDURE(S_make_vct,      g_make_vct_w,   1, 1, 0, H_make_vct);
  XEN_DEFINE_PROCEDURE(S_vct_copy,      copy_vct_w,     1, 0, 0, H_vct_copy);
  XEN_DEFINE_PROCEDURE(S_vct_p,         g_vct_p_w,      1, 0, 0, H_vct_p);
  XEN_DEFINE_PROCEDURE(S_list_to_vct,   list_to_vct_w,  1, 0, 0, H_list_to_vct);
  XEN_DEFINE_PROCEDURE(S_vct_to_list,   vct_to_list_w,  1, 0, 0, H_vct_to_list);
  XEN_DEFINE_PROCEDURE(S_vector_to_vct, vector_to_vct_w, 1, 0, 0, H_vector_to_vct);
  XEN_DEFINE_PROCEDURE(S_vct_to_vector, vct_to_vector_w, 1, 0, 0, H_vct_to_vector);
  XEN_DEFINE_PROCEDURE(S_vct_length,    vct_length_w,   1, 0, 0, H_vct_length);
  XEN_DEFINE_PROCEDURE(S_vct_multiplyB, vct_multiply_w, 2, 0, 0, H_vct_multiplyB);
  XEN_DEFINE_PROCEDURE(S_vct_scaleB,    vct_scale_w,    2, 0, 0, H_vct_scaleB);
  XEN_DEFINE_PROCEDURE(S_vct_fillB,     vct_fill_w,     2, 0, 0, H_vct_fillB);
  XEN_DEFINE_PROCEDURE(S_vct_addB,      vct_add_w,      2, 1, 0, H_vct_addB);
  XEN_DEFINE_PROCEDURE(S_vct_subtractB, vct_subtract_w, 2, 0, 0, H_vct_subtractB);
  XEN_DEFINE_PROCEDURE(S_vct_offsetB,   vct_offset_w,   2, 0, 0, H_vct_offsetB);
  XEN_DEFINE_PROCEDURE(S_vct_peak,      vct_peak_w,     1, 0, 0, H_vct_peak);
  XEN_DEFINE_PROCEDURE(S_vct_moveB,     vct_move_w,     3, 1, 0, H_vct_moveB);
  XEN_DEFINE_PROCEDURE(S_vct_subseq,    vct_subseq_w,   2, 2, 0, H_vct_subseq);
  XEN_DEFINE_PROCEDURE(S_vct,           g_vct_w,        0, 0, 1, H_vct);
  XEN_DEFINE_PROCEDURE(S_vct_reverse,   vct_reverse_w,  1, 1, 0, H_vct_reverse);
#if WITH_RUN && USE_SND
  XEN_DEFINE_PROCEDURE("vct-map-1",     vct_mapB_w,     2, 0, 0, H_vct_mapB);
  XEN_EVAL_C_STRING("(defmacro vct-map! (v form) `(vct-map-1 ,v (list ',form ,form)))");
  XEN_SET_DOCUMENTATION(S_vct_mapB, H_vct_mapB);
#else
  XEN_DEFINE_PROCEDURE(S_vct_mapB,      vct_mapB_w,     2, 0, 0, H_vct_mapB);
#endif

#if HAVE_GUILE
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_vct_ref, vct_ref_w, H_vct_ref, "set-" S_vct_ref, vct_set_w,  2, 0, 3, 0);
#else
  XEN_DEFINE_PROCEDURE(S_vct_ref,       vct_ref_w,      2, 0, 0, H_vct_ref);
#endif
  XEN_DEFINE_PROCEDURE(S_vct_to_string, g_vct_to_readable_string_w, 1, 0, 0, H_vct_to_string);
  XEN_DEFINE_PROCEDURE(S_vct_setB,      vct_set_w,      3, 0, 0, H_vct_setB);
  XEN_DEFINE_PROCEDURE(S_vct_times,     vct_times_w,    2, 0, 0, H_vct_times);
  XEN_DEFINE_PROCEDURE(S_vct_plus,      vct_plus_w,     2, 0, 0, H_vct_plus);

#if WITH_MODULES
  scm_c_export(S_make_vct,
	       S_vct_copy,
	       S_vct_p,
	       S_list_to_vct,
	       S_vct_to_list,
	       S_vector_to_vct,
	       S_vct_to_vector,
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
	       S_vct_reverse,
	       S_vct_to_string,
	       S_vct_times,
	       S_vct_plus,
#if WITH_RUN && USE_SND
	       "vct-map-1",
#endif
	       NULL);
#endif
}

#if WITH_MODULES
void vct_init(void)
{
  scm_c_define_module("snd sndlib", vct_init_1, NULL);
}
#endif

