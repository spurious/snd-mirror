#ifndef SNDLIB2SCM_H
#define SNDLIB2SCM_H

/* error indications */

#define NO_SUCH_CHANNEL      TO_SCM_SYMBOL("no-such-channel")
#define NO_SUCH_SOUND        TO_SCM_SYMBOL("no-such-sound")
#define NO_SUCH_MARK         TO_SCM_SYMBOL("no-such-mark")
#define NO_SUCH_MIX          TO_SCM_SYMBOL("no-such-mix")
#define NO_SUCH_TRACK        TO_SCM_SYMBOL("no-such-track")
#define NO_SUCH_MENU         TO_SCM_SYMBOL("no-such-menu")
#define NO_SUCH_FILE         TO_SCM_SYMBOL("no-such-file")
#define NO_SUCH_REGION       TO_SCM_SYMBOL("no-such-region")
#define NO_SUCH_SAMPLE       TO_SCM_SYMBOL("no-such-sample")
#define NO_SUCH_ENVELOPE     TO_SCM_SYMBOL("no-such-envelope")
#define NO_SUCH_EDIT         TO_SCM_SYMBOL("no-such-edit")
#define CANNOT_SAVE          TO_SCM_SYMBOL("cannot-save")
#define CANNOT_PRINT         TO_SCM_SYMBOL("cannot-print")
#define IMPOSSIBLE_BOUNDS    TO_SCM_SYMBOL("impossible-bounds")
#define NO_ACTIVE_SELECTION  TO_SCM_SYMBOL("no-active-selection")
#define MUS_MISC_ERROR       TO_SCM_SYMBOL("mus-error")
#define NO_SUCH_AXIS_INFO    TO_SCM_SYMBOL("no-such-axis")
#define NO_SUCH_PLAYER       TO_SCM_SYMBOL("no-such-player")
#define NO_SUCH_COLOR        TO_SCM_SYMBOL("no-such-color")
#define NO_SUCH_AXIS_CONTEXT TO_SCM_SYMBOL("no-such-graphics-context")
#define BAD_ARITY            TO_SCM_SYMBOL("bad-arity")

typedef struct {
  int length, chans;
  MUS_SAMPLE_TYPE **data;
} sound_data;

int sound_data_p(SCM obj);
SCM make_sound_data(int chans, int frames);
void mus_misc_error(const char *caller, char *msg, SCM val);

#endif
