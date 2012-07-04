#ifndef VCT_H
#define VCT_H

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
