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
  bool wrapped;
} sound_data;

bool sound_data_p(XEN obj);
char *sound_data_to_string(sound_data *v);
void sound_data_free(sound_data *v);
bool sound_data_equalp(sound_data *v1, sound_data *v2);
sound_data *c_make_sound_data(int chans, int frames);
XEN make_sound_data(int chans, int frames);
void mus_misc_error(const char *caller, char *msg, XEN val);
void mus_sndlib2xen_initialize (void);
XEN wrap_sound_data(int chans, int frames, mus_sample_t **data);
char *mus_header_type_to_constant_name(int type);
char *mus_data_format_to_constant_name(int format);

#endif
