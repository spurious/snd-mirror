#ifndef SNDLIB2XEN_H
#define SNDLIB2XEN_H

#include "xen.h"

/* error indications */

#define NO_SUCH_CHANNEL      XEN_ERROR_TYPE("no-such-channel")
#define NO_SUCH_FILE         XEN_ERROR_TYPE("no-such-file")
#define MUS_MISC_ERROR       XEN_ERROR_TYPE("mus-error")
#define BAD_TYPE             XEN_ERROR_TYPE("bad-type")
#define NO_DATA              XEN_ERROR_TYPE("no-data")
#define BAD_HEADER           XEN_ERROR_TYPE("bad-header")

typedef struct {
  int length, chans;
  mus_sample_t **data;
  int wrapped;
} sound_data;

int sound_data_p(XEN obj);
XEN make_sound_data(int chans, int frames);
void mus_misc_error(const char *caller, char *msg, XEN val);
void mus_sndlib2xen_initialize (void);
XEN wrap_sound_data(int chans, int frames, mus_sample_t **data);

#endif
