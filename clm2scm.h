#ifndef CLM2SCM_H
#define CLM2SCM_H

typedef struct {
  mus_any *gen;
  SCM *vcts; /* one for each accessible Float array (wrapped up here in a vct object) */
  int nvcts;
} mus_scm;

/* next two macros are "deprecated" */
#define mus_get_any(arg) (((mus_scm *)SND_VALUE_OF(arg))->gen)
#define mus_get_scm(arg) ((mus_scm *)SND_VALUE_OF(arg))

#define TO_MUS_SCM(arg) ((mus_scm *)SND_VALUE_OF(arg))
#define TO_CLM(obj) ((mus_any *)((TO_MUS_SCM(obj))->gen))

int mus_scm_p(SCM obj);
mus_any *mus_scm_to_clm(SCM obj);  /* "deprecated" -- use TO_CLM */
SCM mus_scm_to_smob(mus_scm *gn);
SCM mus_scm_to_smob_with_vct(mus_scm *gn, SCM v);
void init_mus2scm_module(void);
char *mus_fft_window_name(int i);

#endif
