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
#define MAX_IN_CHANS 32
#define MAX_MIXER_GAINS 128
#define MAX_SOUNDCARDS 8

#define SMALL_FONT_CUTOFF .85
#define SMALLER_FONT_CUTOFF .7
#define SMALL_FONT "6x10"
#define SMALLER_FONT "5x7"

#define DEFAULT_RECORDER_AUTOLOAD 0
#define DEFAULT_RECORDER_FILE NULL
#define DEFAULT_RECORDER_BUFFER_SIZE 4096

#ifdef SUN
#define DEFAULT_RECORDER_IN_FORMAT MUS_MULAW
#else
#define DEFAULT_RECORDER_IN_FORMAT MUS_COMPATIBLE_FORMAT
#endif

#ifdef SUN
#define DEFAULT_RECORDER_SRATE 8000
#else
#define DEFAULT_RECORDER_SRATE 22050
#endif

#define DEFAULT_RECORDER_TRIGGER 0.0
#define DEFAULT_RECORDER_MAX_DURATION 1000000.0
#define DEFAULT_RECORDER_OUT_FORMAT MUS_COMPATIBLE_FORMAT
#define DEFAULT_RECORDER_OUT_CHANS 2

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

typedef struct {
  int *chan_in_active;        /* overall_in_chans */
  int *chan_out_active;       /* (file)_out_chans */
  Float max_duration,trigger;
  int autoload,srate,in_format,out_format,out_chans,buffer_size,triggering,triggered;
  char *output_file;
  Float **in_amps;           /* overall_in_chans X out_chans */
  Float *out_amps;           /* out_chans (independent of file write: monitor vol) */
  Float *mixer_gains;        /* audio gain values (widgets are per pane) */
  int num_mixer_gains;
  int monitor_out_chans;     /* number of channels being "monitored" -- i.e. output chans sent to the "output" pane
			      *   not chans being sent to output file; used in conjunction with monitor_fd.
			      * for example, on some SGI's you can have 4 incoming chans, 
			      *   any number of recorded chans, but only 2 speaker chans
			      *   and on some Linux setups, you can have 2 incoming chans,
			      *   but no ("full duplex") speaker chans.
			      */
  int taking_input;          /* is input (port) active -- are we running the ADC(s) */
  int output_header_type;
} recorder_info;

recorder_info *get_recorder_info(void);
void set_recorder_autoload(int val);
void set_recorder_trigger(Float val);
void set_recorder_srate(int val);
void reflect_recorder_mixer_gain(int ind, Float val);
void reflect_recorder_out_amp(int ind, Float val);
void reflect_recorder_in_amp(int in, int out, Float val);
char *channel_name(int in_chans, int out_chans, int chan);
char *out_channel_name(snd_state *ss, int chan);
char *gain_channel_name(int in_chans, int out_chans, int input, int dev_in, int out);
Float mixer_gain(int system, int device, int chan, int gain, int field);
void set_mixer_gain(int system, int device, int chan, int gain, int field, Float amp) ;

#endif
