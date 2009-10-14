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
  mus_long_t length;
  int chans;
  mus_float_t **data;
  bool wrapped;
} sound_data;

#ifdef __cplusplus
extern "C" {
#endif

MUS_EXPORT bool sound_data_p(XEN obj);
MUS_EXPORT char *sound_data_to_string(sound_data *v);
MUS_EXPORT void sound_data_free(sound_data *v);
MUS_EXPORT bool sound_data_equalp(sound_data *v1, sound_data *v2);
MUS_EXPORT sound_data *c_make_sound_data(int chans, mus_long_t frames);
MUS_EXPORT XEN make_sound_data(int chans, mus_long_t frames);
MUS_EXPORT void mus_sndlib_xen_initialize (void);
MUS_EXPORT XEN wrap_sound_data(int chans, mus_long_t frames, mus_float_t **data);
MUS_EXPORT sound_data *sound_data_scale(sound_data *sd, mus_float_t scaler);
MUS_EXPORT sound_data *sound_data_fill(sound_data *sd, mus_float_t scaler);
MUS_EXPORT mus_float_t sound_data_peak(sound_data *sd);
MUS_EXPORT sound_data *sound_data_copy(sound_data *sd);
MUS_EXPORT sound_data *sound_data_reverse(sound_data *sd);
MUS_EXPORT sound_data *sound_data_add(sound_data *sd1, sound_data *sd2);
MUS_EXPORT sound_data *sound_data_multiply(sound_data *sd1, sound_data *sd2);
MUS_EXPORT sound_data *sound_data_offset(sound_data *sd, mus_float_t off);

#define XEN_TO_SOUND_DATA(Obj) (sound_data *)XEN_OBJECT_REF(Obj)

MUS_EXPORT XEN g_mus_sound_srate(XEN filename);
MUS_EXPORT XEN g_mus_sound_chans(XEN filename);
MUS_EXPORT XEN g_mus_sound_frames(XEN filename);
MUS_EXPORT XEN g_mus_expand_filename(XEN file);
MUS_EXPORT XEN g_sound_data_maxamp(XEN obj);
MUS_EXPORT XEN g_mus_sound_maxamp(XEN file);

#ifdef __cplusplus
}
#endif

#endif
