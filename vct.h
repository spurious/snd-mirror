#ifndef VCT_H
#define VCT_H

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
#define S_list2vct       "list->vct"
#define S_vct2list       "vct->list"
#define S_vector2vct     "vector->vct"
#define S_vct2vector     "vct->vector"
#define S_vct_moveB      "vct-move!"
#define S_vct_subseq     "vct-subseq"
#define S_vct            "vct"

#ifndef Float
  #define Float float
#endif

typedef struct {
  int length;
  Float *data;
  int dont_free;
} vct;

void init_vct(void);
int vct_p(XEN obj);
XEN make_vct(int len, Float *data);
XEN make_vct_wrapper(int len, Float *data);
vct *get_vct(XEN arg);
void set_vct_print_length(int len);
XEN vct2vector(XEN vobj);
XEN mus_array_to_list(Float *arr, int i, int len);
char *vct_to_string(vct *v);
vct *c_make_vct(int len);
vct *c_free_vct(vct *v);
vct *vector_to_vct(XEN vect);
void vct_to_vector(vct *v, XEN vect);

#define TO_VCT(arg) ((vct *)XEN_OBJECT_REF(arg))
#define VCT_P(arg) vct_p(arg)

#endif
