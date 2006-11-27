#ifndef CLM2XEN_H
#define CLM2XEN_H

#include "vct.h"

typedef struct mus_xen {
  mus_any *gen;
  XEN *vcts; /* one for each accessible Float array (wrapped up here in a vct) */
  int nvcts;
  bool dont_free_gen;
  struct ptree *input_ptree; /* added 24-Apr-02 for snd-run.c optimizer */
  struct ptree *edit_ptree;  /* ditto 26-Jul-04 */
  struct ptree *analyze_ptree;
  struct ptree *synthesize_ptree;
} mus_xen;

#define XEN_TO_MUS_XEN(arg) ((mus_xen *)XEN_OBJECT_REF(arg))
#define XEN_TO_MUS_ANY(obj) ((mus_any *)((XEN_TO_MUS_XEN(obj))->gen))
#define MUS_DEFAULT_CLM_TABLE_SIZE 512
int clm_table_size_c(void);

bool mus_xen_p(XEN obj);
void mus_xen_init(void);
char *mus_fft_window_name(mus_fft_window_t i);
XEN mus_xen_to_object(mus_xen *gn);
XEN mus_xen_to_object_with_vct(mus_xen *gn, XEN v);
mus_any *mus_optkey_to_mus_any(XEN key, const char *caller, int n, mus_any *def);
int mus_optkey_unscramble(const char *caller, int nkeys, XEN *keys, XEN *args, int *orig);
Float mus_optkey_to_float(XEN key, const char *caller, int n, Float def);
int mus_optkey_to_int(XEN key, const char *caller, int n, int def);
off_t mus_optkey_to_off_t(XEN key, const char *caller, int n, off_t def);
char *mus_optkey_to_string(XEN key, const char *caller, int n, char *def);
vct *mus_optkey_to_vct(XEN key, const char *caller, int n, vct *def);
XEN mus_optkey_to_procedure(XEN key, const char *caller, int n, XEN def, int required_args, const char *err);
mus_xen *mus_any_to_mus_xen(mus_any *ge);
Float mus_locsig_or_move_sound_to_vct_or_sound_data(mus_xen *ms, mus_any *loc_gen, off_t pos, Float fval, bool from_locsig);

void Init_sndlib(void);

#endif
