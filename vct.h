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

#ifndef Float
  #define Float float
#endif

typedef struct {
  int length;
  Float *data;
  bool dont_free;
} vct;

void mus_vct_init(void);
bool mus_vct_p(XEN obj);
int mus_vct_print_length(void);
void mus_vct_set_print_length(int len);
XEN mus_array_to_list(Float *arr, int i, int len);
char *mus_vct_to_string(vct *v);
bool mus_vct_equalp(vct *v1, vct *v2);
char *mus_vct_to_readable_string(vct *v);
vct *mus_vct_make(int len);
vct *mus_vct_free(vct *v);
vct *mus_vct_copy(vct *vc);

XEN xen_list_to_vct(XEN lst);
vct *xen_to_vct(XEN arg);
XEN xen_make_vct(int len, Float *data);
XEN xen_make_vct_wrapper(int len, Float *data);

#define XEN_TO_VCT(arg) ((vct *)XEN_OBJECT_REF(arg))
#define MUS_VCT_P(arg) mus_vct_p(arg)
#endif
