#ifndef SND_REC_H
#define SND_REC_H

#ifdef SGI
  #include <audio.h>
  #ifdef AL_RESOURCE
    #define NEW_SGI_AL 1
    #define OLD_SGI_AL 0
  #else
    #define NEW_SGI_AL 0
    #define OLD_SGI_AL 1
  #endif
#else
  #define NEW_SGI_AL 0
  #define OLD_SGI_AL 0
#endif

#define MAX_OUT_CHANS 32
#define MAX_SOUNDCARDS 8

#define SMALL_FONT_CUTOFF .85
#define SMALLER_FONT_CUTOFF .7
#define SMALL_FONT "6x10"
#define SMALLER_FONT "5x7"

void recorder_set_audio_srate(snd_state *ss, int device, int srate, int system, int aud);
char *recorder_device_name(int dev);
char *recorder_system_and_device_name(int sys, int dev);
int recorder_input_device(int dev);
int recorder_output_device(int dev);
char *pane_device_name(int dev);

#if (HAVE_OSS || HAVE_ALSA)
  char *recorder_field_abbreviation(int fld);
  char *recorder_field_name(int fld);
  char *recorder_field_function(int fld);
#endif

#endif
