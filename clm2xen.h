#ifndef CLM2XEN_H
#define CLM2XEN_H

typedef struct {
  mus_any *gen;
  XEN *vcts; /* one for each accessible Float array (wrapped up here in a vct object) */
  int nvcts;
  void *input_ptree; /* added 24-Apr-02 for snd-run.c optimizer */
} mus_xen;

#define XEN_TO_MUS_XEN(arg) ((mus_xen *)XEN_OBJECT_REF(arg))
#define XEN_TO_MUS_ANY(obj) ((mus_any *)((XEN_TO_MUS_XEN(obj))->gen))
#define MUS_XEN_TO_MUS_ANY(obj) ((mus_any *)(((mus_xen *)(obj))->gen))
#define MUS_DATA_POSITION 0

int mus_xen_p(XEN obj);
void mus_xen_init(void);
char *mus_fft_window_name(int i);
XEN mus_xen_to_object(mus_xen *gn);
XEN mus_xen_to_object_with_vct(mus_xen *gn, XEN v);

#if HAVE_RUBY
void Init_sndlib(void);
#else
void init_sndlib(void);
#endif

#endif
