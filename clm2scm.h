#ifndef CLM2SCM_H
#define CLM2SCM_H

typedef struct {
  mus_any *gen;
  SCM *vcts; /* one for each accessible Float array (wrapped up here in a vct object) */
  int nvcts;
} mus_scm;

/* next two macros are "deprecated" */
#define mus_get_any(arg) (((mus_scm *)OBJECT_REF(arg))->gen)
#define mus_get_scm(arg) ((mus_scm *)OBJECT_REF(arg))

#define TO_MUS_SCM(arg) ((mus_scm *)OBJECT_REF(arg))
#define TO_CLM(obj) ((mus_any *)((TO_MUS_SCM(obj))->gen))

int mus_scm_p(SCM obj);
mus_any *mus_scm_to_clm(SCM obj);  /* "deprecated" -- use TO_CLM */
SCM mus_scm_to_smob(mus_scm *gn);
SCM mus_scm_to_smob_with_vct(mus_scm *gn, SCM v);
void init_mus2scm_module(void);
char *mus_fft_window_name(int i);

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
