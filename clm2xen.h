#ifndef CLM2XEN_H
#define CLM2XEN_H

#include "vct.h"

typedef struct mus_xen mus_xen;

#define XEN_TO_MUS_XEN(arg) ((mus_xen *)XEN_OBJECT_REF(arg))
#define XEN_TO_MUS_ANY(obj) mus_xen_gen(XEN_TO_MUS_XEN(obj))
#define MUS_CLM_DEFAULT_TABLE_SIZE 512
#define MUS_CLM_DEFAULT_FREQUENCY 0.0

#ifdef __cplusplus
extern "C" {
#endif

MUS_EXPORT mus_long_t clm_default_table_size_c(void);
MUS_EXPORT double clm_default_frequency_c(void);

MUS_EXPORT mus_any *mus_xen_gen(mus_xen *x);

MUS_EXPORT bool mus_xen_p(XEN obj);
MUS_EXPORT const char *mus_fft_window_xen_name(mus_fft_window_t i);
MUS_EXPORT XEN mus_xen_to_object(mus_xen *gn);
MUS_EXPORT XEN mus_xen_to_object_with_vct(mus_xen *gn, XEN v);
MUS_EXPORT mus_any *mus_optkey_to_mus_any(XEN key, const char *caller, int n, mus_any *def);
MUS_EXPORT int mus_optkey_unscramble(const char *caller, int nkeys, XEN *keys, XEN *args, int *orig);
MUS_EXPORT mus_float_t mus_optkey_to_float(XEN key, const char *caller, int n, mus_float_t def);
MUS_EXPORT int mus_optkey_to_int(XEN key, const char *caller, int n, int def);
MUS_EXPORT bool mus_optkey_to_bool(XEN key, const char *caller, int n, bool def);
MUS_EXPORT mus_long_t mus_optkey_to_mus_long_t(XEN key, const char *caller, int n, mus_long_t def);
MUS_EXPORT const char *mus_optkey_to_string(XEN key, const char *caller, int n, char *def);
MUS_EXPORT XEN mus_optkey_to_procedure(XEN key, const char *caller, int n, XEN def, int required_args, const char *err);

MUS_EXPORT mus_xen *mus_any_to_mus_xen(mus_any *ge);
MUS_EXPORT mus_xen *mus_any_to_mus_xen_with_vct(mus_any *ge, XEN v);
MUS_EXPORT mus_xen *mus_any_to_mus_xen_with_two_vcts(mus_any *ge, XEN v1, XEN v2);

MUS_EXPORT XEN g_mus_channels(XEN obj);
MUS_EXPORT XEN g_mus_length(XEN gen);
MUS_EXPORT XEN g_mus_file_name(XEN gen);
MUS_EXPORT XEN g_mus_data(XEN gen);

#if HAVE_SCHEME
typedef struct {
  void *gen, *g1, *g2, *g3, *g4;
  mus_float_t (*func_1)(void *p);
  mus_float_t (*func_2)(void *p, mus_float_t x);
  mus_float_t (*func_3)(void *p, mus_float_t x, mus_float_t y);
  mus_float_t (*func)(void *p);
  mus_float_t (*f1)(void *p);
  mus_float_t (*f2)(void *p);
  mus_float_t (*f3)(void *p);
  mus_float_t (*f4)(void *p);
  double x1, x2, x3, x4;
  s7_pointer s1, s2, s3, s4;
} gf;

MUS_EXPORT gf *find_gf(s7_scheme *sc, s7_pointer expr);
MUS_EXPORT void free_gf(void *p);
MUS_EXPORT gf *find_gf_with_locals(s7_scheme *sc, s7_pointer expr, s7_pointer locals);
MUS_EXPORT void store_choices(s7_scheme *sc, s7_pointer base_f, s7_pointer g1, s7_pointer g2, s7_pointer g3, s7_pointer isg);
MUS_EXPORT gf *find_gf3(s7_scheme *sc, s7_pointer expr);
MUS_EXPORT mus_any *s7_to_mus_any(s7_pointer p);
#endif

MUS_EXPORT void Init_sndlib(void);

#ifdef __cplusplus
}
#endif

#endif
