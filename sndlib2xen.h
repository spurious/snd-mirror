#ifndef SNDLIB2XEN_H
#define SNDLIB2XEN_H

#include "xen.h"

/* error indications */

#define NO_SUCH_CHANNEL      Xen_make_error_type("no-such-channel")
#define NO_SUCH_FILE         Xen_make_error_type("no-such-file")
#define BAD_TYPE             Xen_make_error_type("bad-type")
#define NO_DATA              Xen_make_error_type("no-data")
#define BAD_HEADER           Xen_make_error_type("bad-header")

#if HAVE_SCHEME
  typedef struct s7_cell sound_data;
#else
  typedef struct sound_data sound_data;
#endif

#ifdef __cplusplus
extern "C" {
#endif

MUS_EXPORT bool xen_is_sound_data(Xen obj);
MUS_EXPORT void mus_sndlib_xen_initialize (void);
MUS_EXPORT Xen wrap_sound_data(int chans, mus_long_t frames, mus_float_t **data);
MUS_EXPORT mus_float_t mus_sound_data_ref(sound_data *sd, int chan, mus_long_t pos);
MUS_EXPORT mus_float_t mus_sound_data_set(sound_data *sd, int chan, mus_long_t pos, mus_float_t val);
MUS_EXPORT int mus_sound_data_chans(sound_data *sd);
MUS_EXPORT mus_long_t mus_sound_data_length(sound_data *sd);
MUS_EXPORT void mus_sound_data_add_frame(sound_data *sd, mus_long_t pos, mus_float_t *data);

#if HAVE_SCHEME
#define Xen_to_sound_data(Obj) (sound_data *)Obj
#else
#define Xen_to_sound_data(Obj) (sound_data *)Xen_object_ref(Obj)
#define XEN_TO_SOUND_DATA(Obj) (sound_data *)Xen_object_ref(Obj)
#endif

MUS_EXPORT Xen g_mus_sound_srate(Xen filename);
MUS_EXPORT Xen g_mus_sound_chans(Xen filename);
MUS_EXPORT Xen g_mus_sound_frames(Xen filename);
MUS_EXPORT Xen g_mus_expand_filename(Xen file);
MUS_EXPORT Xen g_sound_data_maxamp(Xen obj);
MUS_EXPORT Xen g_mus_sound_maxamp(Xen file);

#ifdef __cplusplus
}
#endif

#endif
