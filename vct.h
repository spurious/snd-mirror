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

typedef struct {
  mus_long_t length;
  mus_float_t *data;
  bool dont_free;
} vct;

#ifdef __cplusplus
extern "C" {
#endif

MUS_EXPORT void mus_vct_init(void);
MUS_EXPORT bool mus_vct_p(XEN obj);
MUS_EXPORT int mus_vct_print_length(void);
MUS_EXPORT void mus_vct_set_print_length(int len);
MUS_EXPORT XEN mus_array_to_list(mus_float_t *arr, mus_long_t i, mus_long_t len);
MUS_EXPORT char *mus_vct_to_string(vct *v);
MUS_EXPORT bool mus_vct_equalp(vct *v1, vct *v2);
MUS_EXPORT char *mus_vct_to_readable_string(vct *v);
MUS_EXPORT vct *mus_vct_make(mus_long_t len);
MUS_EXPORT vct *mus_vct_free(vct *v);
MUS_EXPORT vct *mus_vct_copy(vct *vc);
MUS_EXPORT double mus_vct_peak(vct *v);

MUS_EXPORT XEN vct_to_xen(vct *v);
MUS_EXPORT XEN xen_list_to_vct(XEN lst);
MUS_EXPORT vct *xen_to_vct(XEN arg);
MUS_EXPORT XEN xen_make_vct(mus_long_t len, mus_float_t *data);
MUS_EXPORT XEN xen_make_vct_wrapper(mus_long_t len, mus_float_t *data);
MUS_EXPORT XEN g_vct_peak(XEN obj);

#define XEN_TO_VCT(arg) ((vct *)XEN_OBJECT_REF(arg))
#define MUS_VCT_P(arg) mus_vct_p(arg)


#ifdef __cplusplus
}
#endif

#endif
