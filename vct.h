#ifndef VCT_H
#define VCT_H

#if HAVE_SCHEME
  typedef struct s7_cell vct;
#else
typedef struct vct vct;
#endif

#ifdef __cplusplus
extern "C" {
#endif

MUS_EXPORT void mus_vct_init(void);
MUS_EXPORT bool mus_is_vct(Xen obj);
MUS_EXPORT int mus_vct_print_length(void);
MUS_EXPORT void mus_vct_set_print_length(int len);
MUS_EXPORT Xen mus_array_to_list(mus_float_t *arr, mus_long_t i, mus_long_t len);
MUS_EXPORT bool mus_vct_is_equal(vct *v1, vct *v2);
MUS_EXPORT char *mus_vct_to_readable_string(vct *v);
MUS_EXPORT vct *mus_vct_make(mus_long_t len);
MUS_EXPORT vct *mus_vct_free(vct *v);
MUS_EXPORT double mus_vct_peak(vct *v);

MUS_EXPORT Xen vct_to_xen(vct *v);
MUS_EXPORT Xen xen_list_to_vct(Xen lst);
MUS_EXPORT vct *xen_to_vct(Xen arg);
MUS_EXPORT Xen xen_make_vct(mus_long_t len, mus_float_t *data);
MUS_EXPORT Xen xen_make_vct_wrapper(mus_long_t len, mus_float_t *data);
MUS_EXPORT Xen g_vct_peak(Xen obj);

MUS_EXPORT mus_long_t mus_vct_length(vct *v);
MUS_EXPORT mus_float_t *mus_vct_data(vct *v);
MUS_EXPORT vct *mus_vct_wrap(mus_long_t len, mus_float_t *data);

#if HAVE_SCHEME
  #define Xen_to_vct(Obj) (vct *)Obj
#else
  #define Xen_to_vct(arg) ((vct *)Xen_object_ref(arg))
#endif

#if (!DISABLE_DEPRECATED)
#if HAVE_SCHEME
  #define XEN_TO_VCT(Obj) (vct *)Obj
#else
  #define XEN_TO_VCT(arg) ((vct *)Xen_object_ref(arg))
#endif
#define MUS_IS_VCT(arg) mus_is_vct(arg)
#define mus_vct_p mus_is_vct
#define mus_vct_equalp mus_vct_is_equal
#define MUS_VCT_P(arg) mus_is_vct(arg)
#endif

#ifdef __cplusplus
}
#endif

#endif
