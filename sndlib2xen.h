#ifndef SNDLIB2XEN_H
#define SNDLIB2XEN_H

#include "xen.h"

/* error indications */

#define NO_SUCH_CHANNEL      XEN_ERROR_TYPE("no-such-channel")
#define NO_SUCH_SOUND        XEN_ERROR_TYPE("no-such-sound")
#define NO_SUCH_MARK         XEN_ERROR_TYPE("no-such-mark")
#define NO_SUCH_MIX          XEN_ERROR_TYPE("no-such-mix")
#define NO_SUCH_TRACK        XEN_ERROR_TYPE("no-such-track")
#define NO_SUCH_MENU         XEN_ERROR_TYPE("no-such-menu")
#define NO_SUCH_FILE         XEN_ERROR_TYPE("no-such-file")
#define NO_SUCH_REGION       XEN_ERROR_TYPE("no-such-region")
#define NO_SUCH_SAMPLE       XEN_ERROR_TYPE("no-such-sample")
#define NO_SUCH_ENVELOPE     XEN_ERROR_TYPE("no-such-envelope")
#define NO_SUCH_EDIT         XEN_ERROR_TYPE("no-such-edit")
#define CANNOT_SAVE          XEN_ERROR_TYPE("cannot-save")
#define CANNOT_PRINT         XEN_ERROR_TYPE("cannot-print")
#define IMPOSSIBLE_BOUNDS    XEN_ERROR_TYPE("impossible-bounds")
#define NO_ACTIVE_SELECTION  XEN_ERROR_TYPE("no-active-selection")
#define MUS_MISC_ERROR       XEN_ERROR_TYPE("mus-error")
#define NO_SUCH_AXIS_INFO    XEN_ERROR_TYPE("no-such-axis")
#define NO_SUCH_PLAYER       XEN_ERROR_TYPE("no-such-player")
#define NO_SUCH_COLOR        XEN_ERROR_TYPE("no-such-color")
#define NO_SUCH_WIDGET       XEN_ERROR_TYPE("no-such-widget")
#define NO_SUCH_AXIS_CONTEXT XEN_ERROR_TYPE("no-such-graphics-context")
#define BAD_ARITY            XEN_ERROR_TYPE("bad-arity")
#define NO_SUCH_DIRECTION    XEN_ERROR_TYPE("no-such-direction")
#define BAD_TYPE             XEN_ERROR_TYPE("bad-type")

#if HAVE_GSL
  #define SND_GSL_ERROR      XEN_ERROR_TYPE("gsl-error")
#endif

#if HAVE_GL
  #define SND_GL_ERROR       XEN_ERROR_TYPE("gl-error")
#endif

#if HAVE_LADSPA
  #define NO_SUCH_PLUGIN     XEN_ERROR_TYPE("no-such-plugin")
  #define PLUGIN_ERROR       XEN_ERROR_TYPE("plugin-error")
#endif

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
