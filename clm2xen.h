#ifndef CLM2XEN_H
#define CLM2XEN_H

typedef struct {
  mus_any *gen;
  XEN *vcts; /* one for each accessible Float array (wrapped up here in a vct object) */
  int nvcts;
  void *input_ptree; /* added 24-Apr-02 for snd-run.c optimizer */
} mus_xen;

#define CLM_TO_MUS_XEN(arg) ((mus_xen *)XEN_OBJECT_REF(arg))
#define MUS_XEN_TO_CLM(obj) ((mus_any *)((CLM_TO_MUS_XEN(obj))->gen))

int mus_xen_p(XEN obj);
void mus_xen_init(void);
char *mus_fft_window_name(int i);
XEN mus_xen_to_object(mus_xen *gn);
XEN mus_xen_to_object_with_vct(mus_xen *gn, XEN v);

#define S_bartlett_window        "bartlett-window"
#define S_blackman2_window       "blackman2-window"
#define S_blackman3_window       "blackman3-window"
#define S_blackman4_window       "blackman4-window"
#define S_cauchy_window          "cauchy-window"
#define S_exponential_window     "exponential-window"
#define S_gaussian_window        "gaussian-window"
#define S_hamming_window         "hamming-window"
#define S_hann_window            "hann-window"
#define S_kaiser_window          "kaiser-window"
#define S_parzen_window          "parzen-window"
#define S_poisson_window         "poisson-window"
#define S_rectangular_window     "rectangular-window"
#define S_riemann_window         "riemann-window"
#define S_tukey_window           "tukey-window"
#define S_welch_window           "welch-window"
#define S_dolph_chebyshev_window "dolph-chebyshev-window"

#endif
