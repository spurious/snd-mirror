#ifndef SNDLIB2XEN_H
#define SNDLIB2XEN_H

#include "xen.h"

/* error indications */

#define NO_SUCH_CHANNEL      XEN_ERROR_TYPE("no-such-channel")
#define NO_SUCH_FILE         XEN_ERROR_TYPE("no-such-file")
#define BAD_TYPE             XEN_ERROR_TYPE("bad-type")
#define NO_DATA              XEN_ERROR_TYPE("no-data")
#define BAD_HEADER           XEN_ERROR_TYPE("bad-header")

typedef struct {
  int length, chans;
  Float **data;
  bool wrapped;
} sound_data;

bool sound_data_p(XEN obj);
char *sound_data_to_string(sound_data *v);
void sound_data_free(sound_data *v);
bool sound_data_equalp(sound_data *v1, sound_data *v2);
sound_data *c_make_sound_data(int chans, int frames);
XEN make_sound_data(int chans, int frames);
void mus_sndlib_xen_initialize (void);
XEN wrap_sound_data(int chans, int frames, Float **data);
sound_data *sound_data_scale(sound_data *sd, Float scaler);
sound_data *sound_data_fill(sound_data *sd, Float scaler);
Float sound_data_peak(sound_data *sd);
sound_data *sound_data_copy(sound_data *sd);
sound_data *sound_data_reverse(sound_data *sd);
sound_data *sound_data_add(sound_data *sd1, sound_data *sd2);
sound_data *sound_data_multiply(sound_data *sd1, sound_data *sd2);
sound_data *sound_data_offset(sound_data *sd, Float off);
#endif
