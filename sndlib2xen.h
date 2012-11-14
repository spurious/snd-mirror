#ifndef SNDLIB2XEN_H
#define SNDLIB2XEN_H

#include "xen.h"

/* error indications */

#define NO_SUCH_CHANNEL      XEN_ERROR_TYPE("no-such-channel")
#define NO_SUCH_FILE         XEN_ERROR_TYPE("no-such-file")
#define BAD_TYPE             XEN_ERROR_TYPE("bad-type")
#define NO_DATA              XEN_ERROR_TYPE("no-data")
#define BAD_HEADER           XEN_ERROR_TYPE("bad-header")

typedef struct sound_data sound_data;

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
MUS_EXPORT sound_data *sound_data_fill(sound_data *sd, mus_float_t scaler);
MUS_EXPORT mus_float_t mus_sound_data_ref(sound_data *sd, int chan, mus_long_t pos);
MUS_EXPORT mus_float_t mus_sound_data_set(sound_data *sd, int chan, mus_long_t pos, mus_float_t val);
MUS_EXPORT int mus_sound_data_chans(sound_data *sd);
MUS_EXPORT mus_long_t mus_sound_data_length(sound_data *sd);
MUS_EXPORT void mus_sound_data_add_frame(sound_data *sd, mus_long_t pos, mus_float_t *data);

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
