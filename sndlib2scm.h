#ifndef SNDLIB2SCM_H
#define SNDLIB2SCM_H

#if HAVE_GUILE

typedef struct {
  int length, chans;
  MUS_SAMPLE_TYPE **data;
} sound_data;

int sound_data_p(SCM obj);
SCM make_sound_data(int chans, int frames);
void mus_misc_error(char *caller, char *msg, SCM val);

#endif
#endif
