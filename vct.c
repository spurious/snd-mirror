/* vct support 
 *
 * a vct is an object containing a mus_float_t array and its size
 *
 * C side:
 *   void mus_vct_init(void)                    called to declare the various functions and the vct type
 *   bool mus_vct_p(XEN obj)                    is obj a vct
 *   XEN xen_make_vct(int len, mus_float_t *data)     make a new vct
 *   XEN xen_make_vct_wrapper(int len, mus_float_t *data) make a new vct that doesn't free data when garbage collector strikes
 *   vct *xen_to_vct(XEN arg)                   given XEN arg, return vct
 *   void mus_vct_set_print_length(int val)     set vct print length (default 10) (also mus_vct_print_length)
 *   vct *mus_vct_copy(vct *vc)                 return copy of vc
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
 *   (vct-reverse! v (len #f))        reverse contents (using len as end point if given)
 *   (vct->string v)                  scheme-readable description of vct
 *
 *   (vct* obj1 obj2) combines vct-multiply and vct-scale
 *   (vct+ obj1 obj2) combines vct-add and vct-offset
 *
 * The intended use is a sort of latter-day array-processing system that handles huge
 * one-dimensional vectors -- fft's, etc.  Some of these functions can be found in
 * the Snd package; others can be found in the CLM package (clm2xen.c).
 */

#include <mus-config.h>

#if USE_SND
  #include "snd.h"
#endif

#include <math.h>
#include <stdlib.h>
#include <stdio.h>
#if HAVE_STRING_H
  #include <string.h>
#endif

#include "_sndlib.h"
#include "xen.h"
#include "clm.h"
#include "sndlib2xen.h"
#include "vct.h"

#define S_make_vct       "make-vct"
#define S_vct_addB       "vct-add!"
#define S_vct_subtractB  "vct-subtract!"
#define S_vct_copy       "vct-copy"
#define S_vct_length     "vct-length"
#define S_vct_multiplyB  "vct-multiply!"
#define S_vct_offsetB    "vct-offset!"
#define S_vct_ref        "vct-ref"
#define S_vct_scaleB     "vct-scale!"
#define S_vct_fillB      "vct-fill!"
#define S_vct_setB       "vct-set!"
#define S_vct_mapB       "vct-map!"
#define S_vct_peak       "vct-peak"
#define S_vct_p          "vct?"
#define S_list_to_vct    "list->vct"
#define S_vct_to_list    "vct->list"
#define S_vector_to_vct  "vector->vct"
#define S_vct_to_vector  "vct->vector"
#define S_vct_moveB      "vct-move!"
#define S_vct_subseq     "vct-subseq"
#define S_vct            "vct"
#define S_vct_reverse    "vct-reverse!"
#define S_vct_to_string  "vct->string"
#if HAVE_RUBY
  #define S_vct_times    "vct_multiply"
  #define S_vct_plus     "vct_add"
#else
  #define S_vct_times    "vct*"
  #define S_vct_plus     "vct+"
#endif

#ifndef PROC_FALSE
  #if HAVE_RUBY
    #define PROC_FALSE "false"
    #define PROC_TRUE "true"
  #else
    #define PROC_FALSE "#f"
    #define PROC_TRUE  "#t"
  #endif
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

void mus_vct_set_print_length(int len) 
{
  vct_print_length = len;
}

int mus_vct_print_length(void) 
{
  return(vct_print_length);
}


bool mus_vct_p(XEN obj)
{
  return(XEN_OBJECT_TYPE_P(obj, vct_tag));
}


static XEN g_vct_p(XEN obj) 
{
  #define H_vct_p "(" S_vct_p " obj): is obj a vct"
  return(C_TO_XEN_BOOLEAN(MUS_VCT_P(obj)));
}


vct *xen_to_vct(XEN arg)
{
  if (MUS_VCT_P(arg))
    return((vct *)XEN_OBJECT_REF(arg));
  return(NULL);
}


static void vct_free(vct *v)
{
  if (v)
    {
      if ((!(v->dont_free)) && 
	  (v->data)) 
	free(v->data);
      v->data = NULL;
      free(v);
    }
}

XEN_MAKE_OBJECT_FREE_PROCEDURE(vct, free_vct, vct_free)

#define VCT_PRINT_BUFFER_SIZE 64

char *mus_vct_to_string(vct *v)
{
  int len;
  char *buf;
  char flt[VCT_PRINT_BUFFER_SIZE];

  if (v == NULL) return(NULL);
  len = vct_print_length;
  if (len > v->length) len = v->length;

  buf = (char *)calloc((len + 1) * VCT_PRINT_BUFFER_SIZE, sizeof(char));

  sprintf(buf, "#<vct[len=%lld" "]", v->length);
  if ((len > 0) && (v->data != NULL))
    {
      int i;
      strcat(buf, ":");
      for (i = 0; i < len; i++)
	{
	  mus_snprintf(flt, VCT_PRINT_BUFFER_SIZE, " %.3f", v->data[i]);
	  strcat(buf, flt);
	}
      if (v->length > vct_print_length)
	strcat(buf, " ...");
    }
  strcat(buf, ">");
  return(buf);
}


char *mus_vct_to_readable_string(vct *v)
{
  int i, len;
  char *buf;
  char flt[VCT_PRINT_BUFFER_SIZE];

  if (v == NULL) return(NULL);
  len = (int)(v->length);
  buf = (char *)calloc((len + 1) * VCT_PRINT_BUFFER_SIZE, sizeof(char));

#if HAVE_SCHEME
  sprintf(buf, "(vct");
#endif
#if HAVE_RUBY || HAVE_FORTH
  sprintf(buf, "vct(");
#endif

  for (i = 0; i < len; i++)
    {
#if HAVE_SCHEME || HAVE_FORTH
      mus_snprintf(flt, VCT_PRINT_BUFFER_SIZE, " %.3f", v->data[i]);
#endif
#if HAVE_RUBY
      mus_snprintf(flt, VCT_PRINT_BUFFER_SIZE, "%.3f%s", v->data[i], i + 1 < len ? ", " : "");
#endif
      strcat(buf, flt);
    }

#if HAVE_FORTH
  strcat(buf, " ");
#endif
  strcat(buf, ")");

  return(buf);
}


static XEN g_vct_to_readable_string(XEN obj)
{
  char *vstr;
  XEN result;
  #define H_vct_to_string "(" S_vct_to_string " v): scheme readable description of v"

  XEN_ASSERT_TYPE(MUS_VCT_P(obj), obj, XEN_ONLY_ARG, S_vct_to_string, "a vct");

  vstr = mus_vct_to_readable_string(XEN_TO_VCT(obj));
  result = C_TO_XEN_STRING(vstr);
  free(vstr);
  return(result);
}


bool mus_vct_equalp(vct *v1, vct *v2)
{
  if (v1 == v2) return(true);
  return((v1->length == v2->length) &&
	 (mus_arrays_are_equal(v1->data, v2->data, 
			       mus_float_equal_fudge_factor(),
			       v1->length)));
}

static XEN g_vct_fill(XEN obj, XEN val);

#if (HAVE_SCHEME)
static XEN g_vct_length(XEN obj);
static XEN g_vct_copy(XEN obj);
static XEN g_vct_reverse(XEN obj, XEN len);

static bool s7_mus_vct_equalp(void *uv1, void *uv2)
{
  return(mus_vct_equalp((vct *)uv1, (vct *)uv2));
}

static XEN s7_mus_vct_apply(s7_scheme *sc, XEN obj, XEN args)
{
  vct *v;
  mus_long_t loc;
  s7_pointer pos;

  if (s7_is_null(s7, args))
    return(s7_wrong_number_of_args_error(s7, "not enough args for vct-ref: ~A", args));
  if (!s7_is_null(s7, s7_cdr(args)))
    return(s7_wrong_number_of_args_error(s7, "too many args for vct-ref: ~A", args));

  v = XEN_TO_VCT(obj);

  pos = XEN_CAR(args);
  XEN_ASSERT_TYPE(XEN_INTEGER_P(pos), pos, XEN_ARG_2, S_vct_ref, "an integer");
  loc = s7_integer(pos);

  if (loc < 0)
    XEN_OUT_OF_RANGE_ERROR(S_vct_ref, 2, pos, "index ~A < 0?");
  if (loc >= v->length)
    XEN_OUT_OF_RANGE_ERROR(S_vct_ref, 2, pos, "index ~A too high?");

  return(C_TO_XEN_DOUBLE(v->data[loc]));
}

static XEN s7_mus_vct_set(s7_scheme *sc, XEN obj, XEN args)
{
  vct *v;
  mus_long_t loc;
  double x;
  s7_pointer pos, val;

  v = XEN_TO_VCT(obj);

  if (s7_is_null(s7, args))
    return(s7_wrong_number_of_args_error(s7, "not enough args for vct-set!: ~A", args));
  if (s7_is_null(s7, s7_cdr(args)))
    return(s7_wrong_number_of_args_error(s7, "not enough args for vct-set!: ~A", args));
  if (!s7_is_null(s7, s7_cddr(args)))
    return(s7_wrong_number_of_args_error(s7, "too many args for vct-set!: ~A", args));

  pos = XEN_CAR(args);
  XEN_ASSERT_TYPE(XEN_INTEGER_P(pos), pos, XEN_ARG_2, S_vct_setB, "an integer");
  loc = s7_integer(pos); /* this was number_to_integer? */

  if (loc < 0)
    XEN_OUT_OF_RANGE_ERROR(S_vct_setB, 2, pos, "index ~A < 0?");
  if (loc >= v->length)
    XEN_OUT_OF_RANGE_ERROR(S_vct_setB, 2, pos, "index ~A too high?");

  val = XEN_CADR(args);
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ARG_3, S_vct_setB, "a real");
  x = s7_number_to_real(val);

  v->data[loc] = x;
  return(val);
}

static XEN s7_vct_ref_2(s7_scheme *sc, void *obj, s7_pointer index)
{
  vct *v = (vct *)obj;
  mus_long_t loc;

  XEN_ASSERT_TYPE(XEN_INTEGER_P(index), index, XEN_ARG_2, S_vct_ref, "an integer");
  loc = s7_integer(index);

  if (loc < 0)
    XEN_OUT_OF_RANGE_ERROR(S_vct_ref, 2, index, "index ~A < 0?");
  if (loc >= v->length)
    XEN_OUT_OF_RANGE_ERROR(S_vct_ref, 2, index, "index ~A too high?");

  return(C_TO_XEN_DOUBLE(v->data[loc]));
}

static XEN s7_vct_ref_2i(s7_scheme *sc, void *obj, s7_Int loc)
{
  vct *v = (vct *)obj;
  if (loc < 0)
    XEN_OUT_OF_RANGE_ERROR(S_vct_ref, 2, s7_make_integer(sc, loc), "index ~A < 0?");
  if (loc >= v->length)
    XEN_OUT_OF_RANGE_ERROR(S_vct_ref, 2, s7_make_integer(sc, loc), "index ~A too high?");

  return(C_TO_XEN_DOUBLE(v->data[loc]));
}

static XEN s7_vct_set_3(s7_scheme *sc, void *obj, s7_pointer index, s7_pointer value)
{
  vct *v = (vct *)obj;
  mus_long_t loc;

  XEN_ASSERT_TYPE(XEN_INTEGER_P(index), index, XEN_ARG_2, S_vct_setB, "an integer");
  loc = s7_integer(index); 

  if (loc < 0)
    XEN_OUT_OF_RANGE_ERROR(S_vct_setB, 2, index, "index ~A < 0?");
  if (loc >= v->length)
    XEN_OUT_OF_RANGE_ERROR(S_vct_setB, 2, index, "index ~A too high?");

  XEN_ASSERT_TYPE(XEN_NUMBER_P(value), value, XEN_ARG_3, S_vct_setB, "a real");

  v->data[loc] = s7_number_to_real(value);
  return(value);
}

static void s7_vct_set_3if(s7_scheme *sc, void *obj, s7_Int loc, s7_Double value)
{
  vct *v = (vct *)obj;

  if (loc < 0)
    XEN_OUT_OF_RANGE_ERROR(S_vct_setB, 2, s7_make_integer(sc, loc), "index ~A < 0?");
  if (loc >= v->length)
    XEN_OUT_OF_RANGE_ERROR(S_vct_setB, 2, s7_make_integer(sc, loc), "index ~A too high?");

  v->data[loc] = value;
}


static XEN s7_mus_vct_length(s7_scheme *sc, XEN obj)
{
  return(g_vct_length(obj));
}

static XEN s7_mus_vct_copy(s7_scheme *sc, XEN obj)
{
  return(g_vct_copy(obj));
}

static XEN s7_mus_vct_reverse(s7_scheme *sc, XEN obj)
{
  return(g_vct_reverse(obj, XEN_UNDEFINED));
}

static XEN s7_mus_vct_fill(s7_scheme *sc, XEN obj, XEN val)
{
  return(g_vct_fill(obj, val));
}
#endif



XEN_MAKE_OBJECT_PRINT_PROCEDURE(vct, print_vct, mus_vct_to_string)

#if (!HAVE_SCHEME)
static XEN equalp_vct(XEN obj1, XEN obj2)
{
  if ((!(MUS_VCT_P(obj1))) || (!(MUS_VCT_P(obj2)))) return(XEN_FALSE);
  return(C_TO_XEN_BOOLEAN(mus_vct_equalp(XEN_TO_VCT(obj1), XEN_TO_VCT(obj2))));
}
#endif

vct *mus_vct_make(mus_long_t len)
{
  vct *new_vct;
  new_vct = (vct *)malloc(sizeof(vct));
  new_vct->length = len;
  if (len > 0)
    new_vct->data = (mus_float_t *)calloc(len, sizeof(mus_float_t));
  else new_vct->data = NULL;
  new_vct->dont_free = false;
  return(new_vct);
}


vct *mus_vct_free(vct *v) 
{
  vct_free(v);
  return(NULL);
}


vct *mus_vct_copy(vct *vc)
{
  vct *v = NULL;
  mus_long_t len;
  if (vc)
    {
      len = vc->length;
      v = mus_vct_make(len);
      if (len > 0)
	memcpy((void *)(v->data), (void *)(vc->data), (len * sizeof(mus_float_t)));
    }
  return(v);
}


#if HAVE_SCHEME && 0
  static s7_pointer g_vct_methods;
#endif


XEN xen_make_vct(mus_long_t len, mus_float_t *data)
{
  vct *new_vct;

  if (len < 0) return(XEN_FALSE);
  if ((len > 0) && 
      (data == NULL))
    XEN_ERROR(XEN_ERROR_TYPE("out-of-memory"),
	      XEN_LIST_2(C_TO_XEN_STRING(S_make_vct ": can't allocate size ~A"),
			 C_TO_XEN_INT(len)));

  new_vct = (vct *)malloc(sizeof(vct));
  new_vct->length = len;
  new_vct->data = data;
  new_vct->dont_free = false;
#if (!HAVE_SCHEME)
  XEN_MAKE_AND_RETURN_OBJECT(vct_tag, new_vct, 0, free_vct);
#else
  {
    s7_pointer nv;
    nv = s7_make_object(s7, vct_tag, new_vct);
#if 0
    s7_object_set_environment(nv, g_vct_methods);
    s7_open_environment(nv);
#endif
    return(nv);
  }
#endif
}


XEN xen_make_vct_wrapper(mus_long_t len, mus_float_t *data)
{
  vct *new_vct;
  new_vct = (vct *)malloc(sizeof(vct));
  new_vct->length = len;
  new_vct->data = data;
  new_vct->dont_free = true;
#if (!HAVE_SCHEME)
  XEN_MAKE_AND_RETURN_OBJECT(vct_tag, new_vct, 0, free_vct);
#else
  {
    s7_pointer nv;
    nv = s7_make_object(s7, vct_tag, new_vct);
#if 0
    s7_object_set_environment(nv, g_vct_methods);
    s7_open_environment(nv);
#endif
    return(nv);
  }
#endif
}



XEN vct_to_xen(vct *v)
{
  XEN_MAKE_AND_RETURN_OBJECT(vct_tag, v, 0, free_vct);
}


static XEN g_make_vct(XEN len, XEN filler)
{
  #if HAVE_SCHEME
    #define vct_make_example "(define v (make-vct 32 1.0))"
  #endif
  #if HAVE_RUBY
    #define vct_make_example "v = make_vct(32, 1.0)"
  #endif
  #if HAVE_FORTH
    #define vct_make_example "32 1.0 make-vct value v"
  #endif

  #define H_make_vct "(" S_make_vct " len :optional (initial-element 0)): returns a new vct of length len filled with \
initial-element: \n  " vct_make_example

  mus_long_t size;
  XEN_ASSERT_TYPE(XEN_LONG_LONG_P(len), len, XEN_ARG_1, S_make_vct, "an integer");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(filler) || XEN_NOT_BOUND_P(filler), filler, XEN_ARG_2, S_make_vct, "a number");

  size = XEN_TO_C_LONG_LONG(len);
  if (size < 0) 
    XEN_OUT_OF_RANGE_ERROR(S_make_vct, 1, len, "len ~A < 0?");

  if ((size > mus_max_malloc()) ||
      (((mus_long_t)(size * sizeof(mus_float_t))) > mus_max_malloc()))
    XEN_OUT_OF_RANGE_ERROR(S_make_vct, 1, len, "len ~A too large (see mus-max-malloc)");

  if (XEN_NUMBER_P(filler))
    return(g_vct_fill(xen_make_vct(size, (mus_float_t *)calloc(size, sizeof(mus_float_t))), filler));

  return(xen_make_vct(size, (mus_float_t *)calloc(size, sizeof(mus_float_t))));
}


static XEN g_vct_copy(XEN obj)
{
  #define H_vct_copy "(" S_vct_copy " v): returns a copy of vct v"
  vct *v;
  mus_float_t *copied_data = NULL;
  mus_long_t len;

  XEN_ASSERT_TYPE(MUS_VCT_P(obj), obj, XEN_ONLY_ARG, S_vct_copy, "a vct");

  v = XEN_TO_VCT(obj);
  len = v->length;
  if (len > 0)
    {
      copied_data = (mus_float_t *)malloc(len * sizeof(mus_float_t));
      memcpy((void *)copied_data, (void *)(v->data), (len * sizeof(mus_float_t)));
    }
  return(xen_make_vct(len, copied_data));
}


static XEN g_vct_move(XEN obj, XEN newi, XEN oldi, XEN backwards)
{
  #define H_vct_moveB "(" S_vct_moveB " obj new old :optional backwards): moves vct obj data from old to new: v[new++] = v[old++], or \
v[new--] = v[old--] if backwards is " PROC_FALSE "."
  vct *v;
  mus_long_t i, j, ni, nj;

  XEN_ASSERT_TYPE(MUS_VCT_P(obj), obj, XEN_ARG_1, S_vct_moveB, "a vct");
  XEN_ASSERT_TYPE(XEN_LONG_LONG_P(newi), newi, XEN_ARG_2, S_vct_moveB, "an integer");
  XEN_ASSERT_TYPE(XEN_LONG_LONG_P(oldi), oldi, XEN_ARG_3, S_vct_moveB, "an integer");
  XEN_ASSERT_TYPE(XEN_BOOLEAN_IF_BOUND_P(backwards), backwards, XEN_ARG_4, S_vct_moveB, "a boolean");

  v = XEN_TO_VCT(obj);
  ni = XEN_TO_C_LONG_LONG(newi);
  nj = XEN_TO_C_LONG_LONG(oldi);

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


static XEN g_vct_length(XEN obj)
{
  #define H_vct_length "(" S_vct_length " v): length of vct v"
  vct *v;
  XEN_ASSERT_TYPE(MUS_VCT_P(obj), obj, XEN_ONLY_ARG, S_vct_length, "a vct");
  v = XEN_TO_VCT(obj);
  return(C_TO_XEN_LONG_LONG(v->length));
}


static XEN g_vct_ref(XEN obj, XEN pos)
{
  #define H_vct_ref "(" S_vct_ref " v n): element n of vct v, v[n]"
  vct *v;
  mus_long_t loc;

  XEN_ASSERT_TYPE(MUS_VCT_P(obj), obj, XEN_ARG_1, S_vct_ref, "a vct");
  XEN_ASSERT_TYPE(XEN_LONG_LONG_P(pos), pos, XEN_ARG_2, S_vct_ref, "an integer");

  v = XEN_TO_VCT(obj);
  loc = XEN_TO_C_LONG_LONG(pos);

  if (loc < 0)
    XEN_OUT_OF_RANGE_ERROR(S_vct_ref, 2, pos, "index ~A < 0?");
  if (loc >= v->length)
    XEN_OUT_OF_RANGE_ERROR(S_vct_ref, 2, pos, "index ~A too high?");

  return(C_TO_XEN_DOUBLE(v->data[loc]));
}


static XEN g_vct_set(XEN obj, XEN pos, XEN val)
{
  #define H_vct_setB "(" S_vct_setB " v n val): sets element of vct v to val, v[n] = val"
  vct *v;
  mus_long_t loc;
  double x;

  XEN_ASSERT_TYPE(MUS_VCT_P(obj), obj, XEN_ARG_1, S_vct_setB, "a vct");
  XEN_ASSERT_TYPE(XEN_LONG_LONG_P(pos), pos, XEN_ARG_2, S_vct_setB, "an integer");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ARG_3, S_vct_setB, "a real number");

  x = XEN_TO_C_DOUBLE(val);
  v = XEN_TO_VCT(obj);
  loc = XEN_TO_C_LONG_LONG(pos);

  if (loc < 0)
    XEN_OUT_OF_RANGE_ERROR(S_vct_setB, 2, pos, "index ~A < 0?"); 
  if (loc >= v->length)
    XEN_OUT_OF_RANGE_ERROR(S_vct_setB, 2, pos, "index ~A >= vct-length?");

  v->data[loc] = x;
  return(val);
}


static XEN g_vct_multiply(XEN obj1, XEN obj2)
{
  #define H_vct_multiplyB "(" S_vct_multiplyB " v1 v2): element-wise multiply of vcts v1 and v2: v1[i] *= v2[i], returns v1"
  mus_long_t i, lim;
  vct *v1, *v2;

  XEN_ASSERT_TYPE(MUS_VCT_P(obj1), obj1, XEN_ARG_1, S_vct_multiplyB, "a vct");
  XEN_ASSERT_TYPE(MUS_VCT_P(obj2), obj2, XEN_ARG_2, S_vct_multiplyB, "a vct");

  v1 = XEN_TO_VCT(obj1);
  v2 = XEN_TO_VCT(obj2);
  lim = MIN(v1->length, v2->length);
  for (i = 0; i < lim; i++) v1->data[i] *= v2->data[i];
  return(obj1);
}


static XEN g_vct_add(XEN obj1, XEN obj2, XEN offs)
{
  #define H_vct_addB "(" S_vct_addB " v1 v2 :optional (offset 0)): element-wise add of vcts v1 and v2: v1[i + offset] += v2[i], returns v1"
  mus_long_t i, lim, j;
  vct *v1, *v2;

  XEN_ASSERT_TYPE(MUS_VCT_P(obj1), obj1, XEN_ARG_1, S_vct_addB, "a vct");
  XEN_ASSERT_TYPE(MUS_VCT_P(obj2), obj2, XEN_ARG_2, S_vct_addB, "a vct");
  XEN_ASSERT_TYPE(XEN_LONG_LONG_IF_BOUND_P(offs), offs, XEN_ARG_3, S_vct_addB, "an integer");

  v1 = XEN_TO_VCT(obj1);
  v2 = XEN_TO_VCT(obj2);
  lim = MIN(v1->length, v2->length);
  if (lim == 0) return(obj1);

  if (XEN_LONG_LONG_P(offs))
    {
      j = XEN_TO_C_LONG_LONG(offs);
      if (j < 0) 
	XEN_OUT_OF_RANGE_ERROR(S_vct_addB, 3, offs, "offset ~A < 0?");

      if ((j + lim) > v1->length)
	lim = (v1->length - j);

      for (i = 0; i < lim; i++, j++) 
	v1->data[j] += v2->data[i];
    }
  else
    {
      mus_float_t *d1, *d2, *dend;
      d1 = v1->data;
      d2 = v2->data;
      dend = (mus_float_t *)(v1->data + lim);
      while (d1 != dend) (*d1++) += (*d2++);  /* is this safe? or perhaps use (*d1++) = (*d1) + (*d2++)? */

      /* for (i = 0; i < lim; i++) v1->data[i] += v2->data[i]; */
    }
  return(obj1);
}


static XEN g_vct_subtract(XEN obj1, XEN obj2)
{
  #define H_vct_subtractB "(" S_vct_subtractB " v1 v2): element-wise subtract of vcts v1 and v2: v1[i] -= v2[i], returns v1"
  mus_long_t i, lim;
  vct *v1, *v2;

  XEN_ASSERT_TYPE(MUS_VCT_P(obj1), obj1, XEN_ARG_1, S_vct_subtractB, "a vct");
  XEN_ASSERT_TYPE(MUS_VCT_P(obj2), obj2, XEN_ARG_2, S_vct_subtractB, "a vct");

  v1 = XEN_TO_VCT(obj1);
  v2 = XEN_TO_VCT(obj2);
  lim = MIN(v1->length, v2->length);
  for (i = 0; i < lim; i++) v1->data[i] -= v2->data[i];
  return(obj1);
}


static XEN g_vct_scale(XEN obj1, XEN obj2)
{
  #define H_vct_scaleB "(" S_vct_scaleB " v val): scale each element of v by val: v[i] *= val, returns v"
  mus_long_t i;
  vct *v1;
  mus_float_t scl;

  XEN_ASSERT_TYPE(MUS_VCT_P(obj1), obj1, XEN_ARG_1, S_vct_scaleB, "a vct");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(obj2), obj2, XEN_ARG_2, S_vct_scaleB, "a number");

  v1 = XEN_TO_VCT(obj1);
  if (v1->length == 0) return(obj1);

  scl = XEN_TO_C_DOUBLE(obj2);
  if (scl == 0.0)
    mus_clear_array(v1->data, v1->length);
  else
    {
      if (scl != 1.0)
	for (i = 0; i < v1->length; i++) v1->data[i] *= scl;
    }
  return(obj1);
}


static XEN g_vct_offset(XEN obj1, XEN obj2)
{
  #define H_vct_offsetB "(" S_vct_offsetB " v val): add val to each element of v: v[i] += val, returns v"
  mus_long_t i;
  vct *v1;
  mus_float_t scl;

  XEN_ASSERT_TYPE(MUS_VCT_P(obj1), obj1, XEN_ARG_1, S_vct_offsetB, "a vct");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(obj2), obj2, XEN_ARG_2, S_vct_offsetB, "a number");

  v1 = XEN_TO_VCT(obj1);
  if (v1->length == 0) return(obj1);

  scl = XEN_TO_C_DOUBLE(obj2);
  if (scl != 0.0)
    for (i = 0; i < v1->length; i++) v1->data[i] += scl;
  return(obj1);
}


static XEN g_vct_fill(XEN obj1, XEN obj2)
{
  #define H_vct_fillB "(" S_vct_fillB " v val): set each element of v to val: v[i] = val, returns v"
  mus_long_t i;
  vct *v1;
  mus_float_t scl;

  XEN_ASSERT_TYPE(MUS_VCT_P(obj1), obj1, XEN_ARG_1, S_vct_fillB, "a vct");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(obj2), obj2, XEN_ARG_2, S_vct_fillB, "a number");

  v1 = XEN_TO_VCT(obj1);
  if (v1->length == 0) return(obj1);

  scl = XEN_TO_C_DOUBLE(obj2);
  if (scl == 0.0)
    mus_clear_array(v1->data, v1->length);
  else for (i = 0; i < v1->length; i++) v1->data[i] = scl;
  return(obj1);
}


#if (!SND_DISABLE_DEPRECATED)
static XEN g_vct_mapB(XEN obj, XEN proc)
{
  #if HAVE_SCHEME
    #define vct_map_example "(vct-map! v (lambda () 3.0))"
    #define vct_fill_example  "(vct-fill! v 3.0)"
  #endif
  #if HAVE_RUBY
    #define vct_map_example "vct_map!(v, lambda do | | 3.0 end)"
    #define vct_fill_example "vct_fill!(v, 3.0)"
  #endif
  #if HAVE_FORTH
    #define vct_map_example "v lambda: <{ -- val }> 3.0 ; vct-map!"
    #define vct_fill_example "v 3.0 vct-fill!"
  #endif

  #define H_vct_mapB "(" S_vct_mapB " v proc): set each element of v to value of proc (a thunk): v[i] = (proc), returns \
v. " vct_map_example " is the same as " vct_fill_example

  mus_long_t i;
  vct *v;

  XEN_ASSERT_TYPE(MUS_VCT_P(obj), obj, XEN_ARG_1, S_vct_mapB, "a vct");

  v = XEN_TO_VCT(obj);
  XEN_ASSERT_TYPE(XEN_PROCEDURE_P(proc) && (XEN_REQUIRED_ARGS_OK(proc, 0)), proc, XEN_ARG_2, S_vct_mapB, "a thunk");
  for (i = 0; i < v->length; i++) 
    v->data[i] = XEN_TO_C_DOUBLE(XEN_CALL_0_NO_CATCH(proc));

  return(obj);
}
#endif


double mus_vct_peak(vct *v)
{
  mus_float_t val = 0.0, absv;
  mus_float_t *d1, *dend;

  if (v->length == 0) return(0.0);

  val = fabs(v->data[0]);
  d1 = (mus_float_t *)(v->data + 1);
  dend = (mus_float_t *)(v->data + v->length);
  while (d1 != dend)
    {
      absv = fabs(*d1++);
      if (absv > val) val = absv;
    }

  return(val);
}


XEN g_vct_peak(XEN obj)
{
  #define H_vct_peak "(" S_vct_peak " v): max of abs of elements of v"
  XEN_ASSERT_TYPE(MUS_VCT_P(obj), obj, XEN_ONLY_ARG, S_vct_peak, "a vct");
  return(C_TO_XEN_DOUBLE(mus_vct_peak(XEN_TO_VCT(obj))));
}


#define S_vct_peak_and_location "vct-peak-and-location"

static XEN g_vct_peak_and_location(XEN obj)
{
  #define H_vct_peak_and_location "(" S_vct_peak_and_location " v): max of abs of elements of v and its position in v"
  mus_float_t val = 0.0, absv;
  mus_long_t i, loc = 0;
  vct *v;

  XEN_ASSERT_TYPE(MUS_VCT_P(obj), obj, XEN_ONLY_ARG, S_vct_peak, "a vct");
  v = XEN_TO_VCT(obj);

  for (i = 0; i < v->length; i++)
    {
      absv = fabs(v->data[i]);
      if (absv > val) 
	{
	  val = absv;
	  loc = i;
	}
    }
  return(XEN_LIST_2(C_TO_XEN_DOUBLE(val), C_TO_XEN_INT(loc)));
}


static XEN g_vct_subseq(XEN vobj, XEN start, XEN end, XEN newv)
{
  #define H_vct_subseq "(" S_vct_subseq " v start :optional end vnew): v[start..end], placed in vnew if given or new vct"
  vct *vold, *vnew;
  XEN res;
  mus_long_t i, old_len, new_len, j, istart, iend;

  XEN_ASSERT_TYPE(MUS_VCT_P(vobj), vobj, XEN_ARG_1, S_vct_subseq, "a vct");
  XEN_ASSERT_TYPE(XEN_LONG_LONG_P(start), start, XEN_ARG_2, S_vct_subseq, "an integer");
  XEN_ASSERT_TYPE(XEN_LONG_LONG_IF_BOUND_P(end), end, XEN_ARG_3, S_vct_subseq, "an integer");

  istart = XEN_TO_C_LONG_LONG(start);
  if (istart < 0)
    XEN_OUT_OF_RANGE_ERROR(S_vct_subseq, 2, start, "start ~A < 0?");

  vold = XEN_TO_VCT(vobj);
  old_len = vold->length;

  if (XEN_LONG_LONG_P(end))
    {
      iend = XEN_TO_C_LONG_LONG(end);
      if (iend < istart)
	XEN_OUT_OF_RANGE_ERROR(S_vct_subseq, 3, end, "end ~A < start?");
      if (iend > old_len)
	XEN_OUT_OF_RANGE_ERROR(S_vct_subseq, 3, end, "end ~A > vct length?");
      new_len = iend - istart + 1;
    }
  else new_len = old_len - istart;

  if (new_len <= 0) 
    return(XEN_FALSE);

  if (MUS_VCT_P(newv))
    res = newv;
  else res = xen_make_vct(new_len, (mus_float_t *)calloc(new_len, sizeof(mus_float_t)));
  vnew = XEN_TO_VCT(res);
  if (new_len > vnew->length) 
    new_len = vnew->length;
  for (i = istart, j = 0; (j < new_len) && (i < old_len); i++, j++)
    vnew->data[j] = vold->data[i];

  return(res);
}


XEN xen_list_to_vct(XEN lst)
{
  #define H_list_to_vct "(" S_list_to_vct " lst): returns a new vct filled with elements of list lst"
  mus_long_t len = 0, i;
  vct *v;
  XEN scv, lst1;

  XEN_ASSERT_TYPE(XEN_LIST_P_WITH_LENGTH(lst, len), lst, XEN_ONLY_ARG, S_list_to_vct, "a list");

  if (len > 0)
    scv = xen_make_vct(len, (mus_float_t *)calloc(len, sizeof(mus_float_t)));
  else scv = xen_make_vct(0, NULL);

  v = XEN_TO_VCT(scv);
  for (i = 0, lst1 = XEN_COPY_ARG(lst); i < len; i++, lst1 = XEN_CDR(lst1)) 
    {
      if (XEN_NUMBER_P(XEN_CAR(lst1)))
	v->data[i] = (mus_float_t)XEN_TO_C_DOUBLE_OR_ELSE(XEN_CAR(lst1), 0.0);
      else XEN_WRONG_TYPE_ARG_ERROR(S_list_to_vct, i, XEN_CAR(lst1), "a number");
    }

  return(scv);
}


static XEN g_vct(XEN args) 
{
  #define H_vct "(" S_vct " args...): returns a new vct with args as contents; same as " S_list_to_vct ": (vct 1 2 3)"
  return(xen_list_to_vct(args));
}


XEN mus_array_to_list(mus_float_t *arr, mus_long_t i, mus_long_t len)
{
  if (i < (len - 1))
    return(XEN_CONS(C_TO_XEN_DOUBLE(arr[i]), 
		    mus_array_to_list(arr, i + 1, len)));
  else return(XEN_CONS(C_TO_XEN_DOUBLE(arr[i]), 
		       XEN_EMPTY_LIST));
}


static XEN g_vct_to_list(XEN vobj)
{
  #define H_vct_to_list "(" S_vct_to_list " v): returns a new list with elements of vct v"
  vct *v;
  XEN_ASSERT_TYPE(MUS_VCT_P(vobj), vobj, XEN_ONLY_ARG, S_vct_to_list, "a vct");

  v = XEN_TO_VCT(vobj);
  if (v->length == 0)
    return(XEN_EMPTY_LIST);

  return(mus_array_to_list(v->data, 0, v->length));
}


static XEN g_vector_to_vct(XEN vect)
{
  #define H_vector_to_vct "(" S_vector_to_vct " vect): returns a new vct with the elements of vector vect"
  mus_long_t len, i;
  vct *v;
  XEN scv;
#if HAVE_SCHEME
  int gc_loc;
#endif

  XEN_ASSERT_TYPE(XEN_VECTOR_P(vect), vect, XEN_ONLY_ARG, S_vector_to_vct, "a vector");

  len = (mus_long_t)XEN_VECTOR_LENGTH(vect);
  if (len > 0) 
    scv = xen_make_vct(len, (mus_float_t *)calloc(len, sizeof(mus_float_t)));
  else scv = xen_make_vct(0, NULL);
#if HAVE_SCHEME
  gc_loc = s7_gc_protect(s7, scv);
#endif

  v = XEN_TO_VCT(scv);
  for (i = 0; i < len; i++) 
    v->data[i] = (mus_float_t)XEN_TO_C_DOUBLE(XEN_VECTOR_REF(vect, i));

#if HAVE_SCHEME
  s7_gc_unprotect_at(s7, gc_loc);
#endif
  return(scv);
}


static XEN g_vct_to_vector(XEN vobj)
{
  #define H_vct_to_vector "(" S_vct_to_vector " vct): returns a new vector with the elements of vct"
  vct *v;
  mus_long_t i, len;
  XEN new_vect;
#if HAVE_SCHEME
  int gc_loc;
#endif

  XEN_ASSERT_TYPE(MUS_VCT_P(vobj), vobj, XEN_ONLY_ARG, S_vct_to_vector, "a vct");

  v = XEN_TO_VCT(vobj);
  len = v->length;
  new_vect = XEN_MAKE_VECTOR(len, C_TO_XEN_DOUBLE(0.0));

#if HAVE_SCHEME
  gc_loc = s7_gc_protect(s7, new_vect);
#endif
#if HAVE_RUBY && HAVE_RB_GC_DISABLE
  rb_gc_disable(); 
  /* uh oh -- gc is triggered by C_TO_XEN_DOUBLE causing segfault, even if we
   *   protect (via XEN_PROTECT_FROM_GC) new_vect -- I guess the double currently
   *   being created is causing the trouble?
   */
#endif

  for (i = 0; i < len; i++) 
    XEN_VECTOR_SET(new_vect, i, C_TO_XEN_DOUBLE(v->data[i]));

#if HAVE_SCHEME
  s7_gc_unprotect_at(s7, gc_loc);
#endif
#if HAVE_RUBY && HAVE_RB_GC_DISABLE
  rb_gc_enable();
#endif

  return(new_vect);
}


static XEN g_vct_reverse(XEN vobj, XEN size)
{
  #define H_vct_reverse "(" S_vct_reverse " vct len): in-place reversal of vct contents"
  vct *v;
  mus_long_t i, j, len = -1;

  XEN_ASSERT_TYPE(MUS_VCT_P(vobj), vobj, XEN_ARG_1, S_vct_to_vector, "a vct");
  XEN_ASSERT_TYPE(XEN_LONG_LONG_IF_BOUND_P(size), size, XEN_ARG_2, S_vct_to_vector, "an integer");

  v = XEN_TO_VCT(vobj);
  if (XEN_LONG_LONG_P(size))
    len = XEN_TO_C_LONG_LONG(size);
  if ((len <= 0) || (len > v->length))
    len = v->length;
  if (len == 1) return(vobj);

  for (i = 0, j = len - 1; i < j; i++, j--)
    {
      mus_float_t temp;
      temp = v->data[i];
      v->data[i] = v->data[j];
      v->data[j] = temp;
    }
  return(vobj);
}


static XEN g_vct_times(XEN obj1, XEN obj2)
{
  #define H_vct_times "(" S_vct_times " obj1 obj2): either " S_vct_multiplyB " or " S_vct_scaleB ", depending on the types of its arguments"
  if (MUS_VCT_P(obj1))
    {
      if (MUS_VCT_P(obj2))
	return(g_vct_multiply(obj1, obj2));
      return(g_vct_scale(obj1, obj2));
    }
  return(g_vct_scale(obj2, obj1));
}


static XEN g_vct_plus(XEN obj1, XEN obj2)
{
  #define H_vct_plus "(" S_vct_plus " obj1 obj2): either " S_vct_addB " or " S_vct_offsetB ", depending on the types of its arguments"
  if (MUS_VCT_P(obj1))
    {
      if (MUS_VCT_P(obj2))
	return(g_vct_add(obj1, obj2, XEN_UNDEFINED));
      return(g_vct_offset(obj1, obj2));
    }
  return(g_vct_offset(obj2, obj1));
}


#ifdef XEN_ARGIFY_1
XEN_ARGIFY_2(g_make_vct_w, g_make_vct)
XEN_NARGIFY_1(g_vct_copy_w, g_vct_copy)
XEN_NARGIFY_1(g_vct_p_w, g_vct_p)
XEN_NARGIFY_1(g_list_to_vct_w, xen_list_to_vct)
XEN_NARGIFY_1(g_vct_to_list_w, g_vct_to_list)
XEN_NARGIFY_1(g_vector_to_vct_w, g_vector_to_vct)
XEN_NARGIFY_1(g_vct_to_vector_w, g_vct_to_vector)
XEN_NARGIFY_1(g_vct_length_w, g_vct_length)
XEN_NARGIFY_2(g_vct_ref_w, g_vct_ref)
XEN_NARGIFY_3(g_vct_set_w, g_vct_set)
XEN_NARGIFY_2(g_vct_multiply_w, g_vct_multiply)
XEN_NARGIFY_2(g_vct_scale_w, g_vct_scale)
XEN_NARGIFY_2(g_vct_fill_w, g_vct_fill)
XEN_ARGIFY_3(g_vct_add_w, g_vct_add)
XEN_NARGIFY_2(g_vct_subtract_w, g_vct_subtract)
XEN_NARGIFY_2(g_vct_offset_w, g_vct_offset)
#if (!SND_DISABLE_DEPRECATED)
  XEN_NARGIFY_2(g_vct_mapB_w, g_vct_mapB)
#endif
XEN_NARGIFY_1(g_vct_peak_w, g_vct_peak)
XEN_NARGIFY_1(g_vct_peak_and_location_w, g_vct_peak_and_location)
XEN_ARGIFY_4(g_vct_move_w, g_vct_move)
XEN_ARGIFY_4(g_vct_subseq_w, g_vct_subseq)
XEN_VARGIFY(g_vct_w, g_vct)
XEN_ARGIFY_2(g_vct_reverse_w, g_vct_reverse)
XEN_NARGIFY_1(g_vct_to_readable_string_w, g_vct_to_readable_string)
XEN_NARGIFY_2(g_vct_times_w, g_vct_times)
XEN_NARGIFY_2(g_vct_plus_w, g_vct_plus)
#else
#define g_make_vct_w g_make_vct
#define g_vct_copy_w g_vct_copy
#define g_vct_p_w g_vct_p
#define g_list_to_vct_w xen_list_to_vct
#define g_vct_to_list_w g_vct_to_list
#define g_vector_to_vct_w g_vector_to_vct
#define g_vct_to_vector_w g_vct_to_vector
#define g_vct_length_w g_vct_length
#define g_vct_ref_w g_vct_ref
#define g_vct_set_w g_vct_set
#define g_vct_multiply_w g_vct_multiply
#define g_vct_scale_w g_vct_scale
#define g_vct_fill_w g_vct_fill
#define g_vct_add_w g_vct_add
#define g_vct_subtract_w g_vct_subtract
#define g_vct_offset_w g_vct_offset
#if (!SND_DISABLE_DEPRECATED)
  #define g_vct_mapB_w g_vct_mapB
#endif
#define g_vct_peak_w g_vct_peak
#define g_vct_peak_and_location_w g_vct_peak_and_location
#define g_vct_move_w g_vct_move
#define g_vct_subseq_w g_vct_subseq
#define g_vct_w g_vct
#define g_vct_reverse_w g_vct_reverse
#define g_vct_to_readable_string_w g_vct_to_readable_string
#define g_vct_times_w g_vct_times
#define g_vct_plus_w g_vct_plus
#endif


#if HAVE_RUBY
static XEN g_vct_each(XEN obj)
{
  mus_long_t i;
  vct *v;
  v = XEN_TO_VCT(obj);
  for (i = 0; i < v->length; i++)
    rb_yield(C_TO_XEN_DOUBLE(v->data[i]));
  return(obj);
}


static XEN g_vct_compare(XEN vr1, XEN vr2)
{
  mus_long_t i, len;
  vct *v1, *v2;
  if ((MUS_VCT_P(vr1)) && (MUS_VCT_P(vr2)))
    {
      v1 = XEN_TO_VCT(vr1);
      v2 = XEN_TO_VCT(vr2);
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
  mus_long_t size;
  XEN len, filler;
  rb_scan_args(argc, argv, "11", &len, &filler);
  XEN_ASSERT_TYPE(XEN_LONG_LONG_P(len), len, XEN_ONLY_ARG, "Vct.new", "an integer");
  size = XEN_TO_C_LONG_LONG(len);
  if (size <= 0) 
    XEN_OUT_OF_RANGE_ERROR("Vct.new", 1, len, "len <= 0?");
  if (XEN_NUMBER_P(filler))
    return(g_vct_fill(xen_make_vct(size, (mus_float_t *)calloc(size, sizeof(mus_float_t))), filler));
  if (rb_block_given_p()) {
    mus_long_t i;
    mus_float_t *buffer = (mus_float_t *)calloc(size, sizeof(mus_float_t));
    for (i = 0; i < size; i++) {
      buffer[i] = XEN_TO_C_DOUBLE(rb_yield(C_TO_XEN_INT(i)));
    }
    return xen_make_vct(size, buffer);
  }
  return(xen_make_vct(size, (mus_float_t *)calloc(size, sizeof(mus_float_t))));
}


static XEN g_vct_map(XEN obj)
{
  if (rb_block_given_p()) {
    mus_long_t i;
    vct *v = XEN_TO_VCT(obj);
    mus_float_t *buffer = (mus_float_t *)calloc(v->length, sizeof(mus_float_t));
    for (i = 0; i < v->length; i++)
      buffer[i] = XEN_TO_C_DOUBLE(rb_yield(C_TO_XEN_DOUBLE(v->data[i])));
    return xen_make_vct(v->length, buffer);
  }
  return obj;
}


static XEN g_vct_map_store(XEN obj)
{
  if (rb_block_given_p()) {
    mus_long_t i;
    vct *v = XEN_TO_VCT(obj);
    for (i = 0; i < v->length; i++)
      v->data[i] = XEN_TO_C_DOUBLE(rb_yield(C_TO_XEN_DOUBLE(v->data[i])));
  }
  return obj;
}


/* v1.add!(v2[,offset=0]) destructive */

static XEN rb_vct_add(int argc, XEN *argv, XEN obj1)
{
  XEN obj2, offs;
  rb_scan_args(argc, argv, "11", &obj2, &offs);
  return g_vct_add(obj1, obj2, (argc == 2) ? offs : XEN_UNDEFINED);
}


/* v1.add(v2[,offset=0]) returns new vct */

static XEN rb_vct_add_cp(int argc, XEN *argv, XEN obj1)
{
  XEN obj2, offs;
  rb_scan_args(argc, argv, "11", &obj2, &offs);
  return g_vct_add(g_vct_copy(obj1), obj2, (argc == 2) ? offs : XEN_UNDEFINED);
}


/* v1.subtract(v2) returns new vct */

static XEN rb_vct_subtract_cp(XEN obj1, XEN obj2)
{
  return g_vct_subtract(g_vct_copy(obj1), obj2);
}


static XEN rb_vct_offset_cp(XEN obj, XEN scl)
{
  return g_vct_offset(g_vct_copy(obj), scl);
}


static XEN rb_vct_multiply_cp(XEN obj1, XEN obj2)
{
  return g_vct_multiply(g_vct_copy(obj1), obj2);
}


static XEN rb_vct_scale_cp(XEN obj, XEN scl)
{
  return g_vct_scale(g_vct_copy(obj), scl);
}


#if 0
static XEN rb_vct_fill_cp(XEN obj, XEN scl)
{
  return g_vct_fill(g_vct_copy(obj), scl);
}
#endif


/* destructive */

static XEN rb_vct_move(int argc, XEN *argv, XEN obj)
{
  XEN vnew, old, backward;
  rb_scan_args(argc, argv, "21", &vnew, &old, &backward);
  return g_vct_move(obj, vnew, old, (argc == 3) ? backward : XEN_UNDEFINED);
}


/* returns new vct */

static XEN rb_vct_move_cp(int argc, XEN *argv, XEN obj)
{
  XEN vnew, old, backward;
  rb_scan_args(argc, argv, "21", &vnew, &old, &backward);
  return g_vct_move(g_vct_copy(obj), vnew, old, (argc == 3) ? backward : XEN_UNDEFINED);
}


static XEN rb_vct_subseq(int argc, XEN *argv, XEN obj)
{
  XEN start, end, vnew;
  rb_scan_args(argc, argv, "12", &start, &end, &vnew);
    return g_vct_subseq(obj, start, (argc > 1) ? end :XEN_UNDEFINED, (argc > 2) ? vnew : XEN_UNDEFINED);
}


/* destructive */

static XEN rb_vct_reverse(int argc, XEN *argv, XEN obj)
{
  XEN len;
  rb_scan_args(argc, argv, "01", &len);
  return g_vct_reverse(obj, (argc > 0) ? len : XEN_UNDEFINED);
}


/* returns new vct */

static XEN rb_vct_reverse_cp(int argc, XEN *argv, XEN obj)
{
  XEN len;
  rb_scan_args(argc, argv, "01", &len);
  return g_vct_reverse(g_vct_copy(obj), (argc > 0) ? len : XEN_UNDEFINED);
}


static XEN rb_vct_first(XEN obj)
{
  return g_vct_ref(obj, C_TO_XEN_INT(0));
}


static XEN rb_set_vct_first(XEN obj, XEN val)
{
  return g_vct_set(obj, C_TO_XEN_INT(0), val);
}


static XEN rb_vct_last(XEN obj)
{
  return g_vct_ref(obj, C_TO_XEN_INT(XEN_TO_VCT(obj)->length - 1));
}


static XEN rb_set_vct_last(XEN obj, XEN val)
{
  return g_vct_set(obj, C_TO_XEN_INT(XEN_TO_VCT(obj)->length - 1), val);
}
#endif


#if HAVE_FORTH
static void ficl_values_to_vct(ficlVm *vm)
{
#define h_values_to_vct "( len-floats len -- vct )  \
Returns a new vct of length LEN with len items found on stack.\n\
0.5 0.3 0.1  3  >vct  .g => #<vct[len=3]: 0.500 0.300 0.100>"
  long size;
  FICL_STACK_CHECK(vm->dataStack, 1, 0);
  size = ficlStackPopInteger(vm->dataStack);
  if (size > 0)
    {
      mus_float_t *data = (mus_float_t *)calloc(size, sizeof(mus_float_t));
      if (data)
	{
	  long i;
	  FICL_STACK_CHECK(vm->dataStack, size, 1);
	  for (i = size - 1; i >= 0; i--)
	    data[i] = ficlStackPop2Float(vm->dataStack);
	  ficlStackPushUnsigned(vm->dataStack, xen_make_vct(size, data));
	}
      else fth_throw(FTH_SYSTEM_ERROR, "cannot create Vct");
    }
  else ficlStackPushUnsigned(vm->dataStack, fth_false());
}


static void ficl_begin_vct(ficlVm *vm)
{
#define h_begin_vct "( -- )  \
Creates a vct with contents between `vct(' and closing paren `)'.\n\
vct( 0.5 0.3 0.1 ) .g => #<vct[len=3]: 0.500 0.300 0.100>"
  fth_begin_values_to_obj(vm, ">vct", FTH_FALSE);
}
#endif


void mus_vct_init(void)
{

#if HAVE_SCHEME
  vct_tag = XEN_MAKE_OBJECT_TYPE("<vct>", print_vct, free_vct, s7_mus_vct_equalp, NULL, 
				 s7_mus_vct_apply, s7_mus_vct_set, s7_mus_vct_length, 
				 s7_mus_vct_copy, s7_mus_vct_reverse, s7_mus_vct_fill);
  s7_set_object_ref_2(vct_tag, s7_vct_ref_2);
  s7_set_object_ref_2i(vct_tag, s7_vct_ref_2i);
  s7_set_object_set_3(vct_tag, s7_vct_set_3);
  s7_set_object_set_3if(vct_tag, s7_vct_set_3if);
  s7_set_object_ref_arity(vct_tag, 2, 2);
#else
  vct_tag = XEN_MAKE_OBJECT_TYPE("Vct", sizeof(vct));
#endif

#if HAVE_FORTH
  fth_set_object_inspect(vct_tag,   print_vct);
  fth_set_object_dump(vct_tag,      g_vct_to_readable_string);
  fth_set_object_to_array(vct_tag,  g_vct_to_vector);
  fth_set_object_copy(vct_tag,      g_vct_copy);
  fth_set_object_value_ref(vct_tag, g_vct_ref);
  fth_set_object_value_set(vct_tag, g_vct_set);
  fth_set_object_equal(vct_tag,     equalp_vct);
  fth_set_object_length(vct_tag,    g_vct_length);
  fth_set_object_free(vct_tag,      free_vct);
  fth_set_object_apply(vct_tag, XEN_PROCEDURE_CAST g_vct_ref, 1, 0, 0);
  FTH_PRIM(FTH_FICL_DICT(), ">vct",   ficl_values_to_vct, h_values_to_vct);
  FTH_PRIM(FTH_FICL_DICT(), "vct(",   ficl_begin_vct,     h_begin_vct);
  XEN_EVAL_C_STRING("start-prefixes : vct( vct( ; end-prefixes"); 
#endif

#if HAVE_RUBY
  rb_include_module(vct_tag, rb_mComparable);
  rb_include_module(vct_tag, rb_mEnumerable);

  rb_define_method(vct_tag, "to_s",     XEN_PROCEDURE_CAST print_vct, 0);
  rb_define_method(vct_tag, "eql?",     XEN_PROCEDURE_CAST equalp_vct, 1);
  rb_define_method(vct_tag, "[]",       XEN_PROCEDURE_CAST g_vct_ref, 1);
  rb_define_method(vct_tag, "[]=",      XEN_PROCEDURE_CAST g_vct_set, 2);
  rb_define_method(vct_tag, "length",   XEN_PROCEDURE_CAST g_vct_length, 0);
  rb_define_method(vct_tag, "each",     XEN_PROCEDURE_CAST g_vct_each, 0);
  rb_define_method(vct_tag, "<=>",      XEN_PROCEDURE_CAST g_vct_compare, 1);
  rb_define_singleton_method(vct_tag, "new", XEN_PROCEDURE_CAST g_rb_make_vct, -1);
  rb_define_method(vct_tag, "map",      XEN_PROCEDURE_CAST g_vct_map, 0);
  rb_define_method(vct_tag, "map!",     XEN_PROCEDURE_CAST g_vct_map_store, 0);
  rb_define_method(vct_tag, "to_a",     XEN_PROCEDURE_CAST g_vct_to_vector, 0);
  rb_define_method(rb_cArray, "to_vct", XEN_PROCEDURE_CAST g_vector_to_vct, 0);

  rb_define_method(vct_tag, "to_str",    XEN_PROCEDURE_CAST g_vct_to_readable_string, 0);
  rb_define_method(vct_tag, "dup",       XEN_PROCEDURE_CAST g_vct_copy, 0);
  rb_define_method(vct_tag, "peak",      XEN_PROCEDURE_CAST g_vct_peak, 0);
  rb_define_method(vct_tag, "add",       XEN_PROCEDURE_CAST rb_vct_add_cp, -1);
  rb_define_method(vct_tag, "add!",      XEN_PROCEDURE_CAST rb_vct_add, -1);
  rb_define_method(vct_tag, "subtract",  XEN_PROCEDURE_CAST rb_vct_subtract_cp, 1);
  rb_define_method(vct_tag, "subtract!", XEN_PROCEDURE_CAST g_vct_subtract, 1);
  rb_define_method(vct_tag, "offset",    XEN_PROCEDURE_CAST rb_vct_offset_cp, 1);
  rb_define_method(vct_tag, "offset!",   XEN_PROCEDURE_CAST g_vct_offset, 1);
  rb_define_method(vct_tag, "multiply",  XEN_PROCEDURE_CAST rb_vct_multiply_cp, 1);
  rb_define_method(vct_tag, "multiply!", XEN_PROCEDURE_CAST g_vct_multiply, 1);
  rb_define_method(vct_tag, "scale",     XEN_PROCEDURE_CAST rb_vct_scale_cp, 1);
  rb_define_method(vct_tag, "scale!",    XEN_PROCEDURE_CAST g_vct_scale, 1);
  rb_define_method(vct_tag, "fill",      XEN_PROCEDURE_CAST g_vct_fill, 1);
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

  XEN_DEFINE_SAFE_PROCEDURE(S_make_vct,          g_make_vct_w,      1, 1, 0, H_make_vct);
  XEN_DEFINE_SAFE_PROCEDURE(S_vct_copy,          g_vct_copy_w,      1, 0, 0, H_vct_copy);
  XEN_DEFINE_SAFE_PROCEDURE(S_vct_p,             g_vct_p_w,         1, 0, 0, H_vct_p);
  XEN_DEFINE_SAFE_PROCEDURE(S_list_to_vct,       g_list_to_vct_w,   1, 0, 0, H_list_to_vct);
  XEN_DEFINE_SAFE_PROCEDURE(S_vct_to_list,       g_vct_to_list_w,   1, 0, 0, H_vct_to_list);
  XEN_DEFINE_SAFE_PROCEDURE(S_vector_to_vct,     g_vector_to_vct_w, 1, 0, 0, H_vector_to_vct);
  XEN_DEFINE_SAFE_PROCEDURE(S_vct_to_vector,     g_vct_to_vector_w, 1, 0, 0, H_vct_to_vector);
  XEN_DEFINE_SAFE_PROCEDURE(S_vct_length,        g_vct_length_w,    1, 0, 0, H_vct_length);
  XEN_DEFINE_SAFE_PROCEDURE(S_vct_multiplyB,     g_vct_multiply_w,  2, 0, 0, H_vct_multiplyB);
  XEN_DEFINE_SAFE_PROCEDURE(S_vct_scaleB,        g_vct_scale_w,     2, 0, 0, H_vct_scaleB);
  XEN_DEFINE_SAFE_PROCEDURE(S_vct_fillB,         g_vct_fill_w,      2, 0, 0, H_vct_fillB);
  XEN_DEFINE_SAFE_PROCEDURE(S_vct_addB,          g_vct_add_w,       2, 1, 0, H_vct_addB);
  XEN_DEFINE_SAFE_PROCEDURE(S_vct_subtractB,     g_vct_subtract_w,  2, 0, 0, H_vct_subtractB);
  XEN_DEFINE_SAFE_PROCEDURE(S_vct_offsetB,       g_vct_offset_w,    2, 0, 0, H_vct_offsetB);
  XEN_DEFINE_SAFE_PROCEDURE(S_vct_peak,          g_vct_peak_w,      1, 0, 0, H_vct_peak);
  XEN_DEFINE_SAFE_PROCEDURE(S_vct_peak_and_location, g_vct_peak_and_location_w, 1, 0, 0, H_vct_peak_and_location);
  XEN_DEFINE_SAFE_PROCEDURE(S_vct_moveB,         g_vct_move_w,      3, 1, 0, H_vct_moveB);
  XEN_DEFINE_SAFE_PROCEDURE(S_vct_subseq,        g_vct_subseq_w,    2, 2, 0, H_vct_subseq);
  XEN_DEFINE_PROCEDURE(S_vct,                    g_vct_w,           0, 0, 1, H_vct);
  XEN_DEFINE_SAFE_PROCEDURE(S_vct_reverse,       g_vct_reverse_w,   1, 1, 0, H_vct_reverse);
#if (!SND_DISABLE_DEPRECATED)
  XEN_DEFINE_PROCEDURE(S_vct_mapB,               g_vct_mapB_w,      2, 0, 0, H_vct_mapB);
#endif

#if HAVE_SCHEME || HAVE_FORTH
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_vct_ref,    g_vct_ref_w, H_vct_ref, "set-" S_vct_ref, g_vct_set_w,  2, 0, 3, 0);
#else
  XEN_DEFINE_PROCEDURE(S_vct_ref,                g_vct_ref_w,       2, 0, 0, H_vct_ref);
#endif

  XEN_DEFINE_SAFE_PROCEDURE(S_vct_to_string,     g_vct_to_readable_string_w, 1, 0, 0, H_vct_to_string);
  XEN_DEFINE_SAFE_PROCEDURE(S_vct_setB,          g_vct_set_w,       3, 0, 0, H_vct_setB);
  XEN_DEFINE_SAFE_PROCEDURE(S_vct_times,         g_vct_times_w,     2, 0, 0, H_vct_times);
  XEN_DEFINE_SAFE_PROCEDURE(S_vct_plus,          g_vct_plus_w,      2, 0, 0, H_vct_plus);

#if HAVE_SCHEME && 0
  g_vct_methods = s7_eval_c_string(s7, "(augment-environment ()                                        \n\
                                          (cons 'vector? (lambda (p) #t))                              \n\
                                          (cons 'vector-length vct-length)                             \n\
                                          (cons 'vector-dimensions (lambda (p) (list (vct-length p)))) \n\
                                          (cons 'vector-ref vct-ref)                                   \n\
                                          (cons 'vector-set! vct-set!)                                 \n\
                                          (cons 'vector-fill! vct-fill!)                               \n\
                                          (cons 'vector->list vct->list))");
  s7_gc_protect(s7, g_vct_methods);
#endif
}
