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

#define AUDVAL_SIZE 64
#define MAX_AUDIO_FIELD (MUS_AUDIO_DIRECTION + 1)

#define DEFAULT_RECORDER_AUTOLOAD 0
#define DEFAULT_RECORDER_FILE NULL
#define DEFAULT_RECORDER_BUFFER_SIZE 4096
#define DEFAULT_RECORDER_TRIGGER 0.0
#define DEFAULT_RECORDER_MAX_DURATION 1000000.0
#define DEFAULT_RECORDER_OUT_FORMAT MUS_COMPATIBLE_FORMAT
#define DEFAULT_RECORDER_OUT_CHANS 2

#ifdef SUN
  #define DEFAULT_RECORDER_IN_FORMAT MUS_MULAW
  #define DEFAULT_RECORDER_SRATE 8000
#else
  #define DEFAULT_RECORDER_IN_FORMAT MUS_COMPATIBLE_FORMAT
  #define DEFAULT_RECORDER_SRATE 22050
#endif

int recorder_columns(int vu_meters);
int recorder_sort_mixer_device(void *wd, int i, int chan, int input, int device, int *mixflds);
void recorder_fill_wd(void *wd, int chan, int field, int device);
int recorder_check_device(int system, int device, int *mixer_gains_posted, int *tone_controls_posted, int *mixflds, int *gains, int *inp);
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
  int recording;
  int input_ports[MAX_SOUNDCARDS]; /* input (audio hardware) channel (mus_audio_read from this) */
  int possible_input_chans;        /* possible_input_chans is a count of all existing input channels, some of which may be incompatible */
  int input_srates[MAX_SOUNDCARDS];
  int input_formats[MAX_SOUNDCARDS];
  int input_buffer_sizes[MAX_SOUNDCARDS];
  int input_channels[MAX_SOUNDCARDS];
  int input_channel_active[MAX_IN_CHANS]; /* is this input channel receiving input */
  char *raw_input_bufs[MAX_SOUNDCARDS]; /* incoming data has not yet been converted to sndlib representation */
  MUS_SAMPLE_TYPE unscaled_output_bufs[MAX_OUT_CHANS]; /* per-channel (output) buffer, before final output scaling */
  MUS_SAMPLE_TYPE input_vu_maxes[MAX_IN_CHANS]; /* VU label values on input chans */
  MUS_SAMPLE_TYPE output_vu_maxes[MAX_OUT_CHANS]; /* VU label values on output chans */
  MUS_SAMPLE_TYPE *all_systems_input_buf;
  MUS_SAMPLE_TYPE *one_system_input_buf;
  int system_input_buffer_size;

  int *chan_in_active;             /* overall_in_chans */
  int *chan_out_active;            /* (file)_out_chans */
  Float max_duration, trigger;
  int autoload, srate, in_format, out_format, out_chans, buffer_size, triggering, triggered, in_device;
  Float **in_amps;                /* overall_in_chans X out_chans */
  Float *out_amps;                /* out_chans (independent of file write: monitor vol) */
  Float *mixer_gains;             /* audio gain values (widgets are per pane) */
  int num_mixer_gains;
  int monitoring;                 /* speakers active (monitor_fd open) */
  int monitor_port;               /* mus_audio_write to this */
  int monitor_chans;              /* number of channels being "monitored" -- i.e. output chans sent to the "output" pane
				   *   not chans being sent to output file; used in conjunction with monitor_fd.
				   * for example, on some SGI's you can have 4 incoming chans, 
				   *   any number of recorded chans, but only 2 speaker chans
				   *   and on some Linux setups, you can have 2 incoming chans,
				   *   but no ("full duplex") speaker chans.
				   */
  int monitor_data_format;
  char *monitor_buf;
  int taking_input;               /* is input (port) active -- are we running the ADC(s) */
  char *output_file;
  int output_file_descriptor;     /* mus_file_write to this */
  int output_header_type;
  MUS_SAMPLE_TYPE **output_bufs;  /* formatted non-interleaved output (for file and monitor outputs) */

  int duration_label_update_frames; /* frames between updates of the duration label */
  int total_output_frames;
  int systems;                    /* soundcards normally = how many independent input sources from sndlib's point of view */
  int *ordered_devices, *ordered_systems; /* soundcards in recorder dialog order with output at end */
  int ordered_devices_size;
  int mixer_settings_saved;
  int autoload_button;
  int digital_in_button;
  int microphone_button;
  int line_in_button;
  BACKGROUND_FUNCTION_TYPE recorder_reader;
} recorder_info;

recorder_info *get_recorder_info(void);
int recorder_get_devices(recorder_info *rp, int *outs);
void set_recorder_autoload(recorder_info *rp, int val);
void set_recorder_trigger(recorder_info *rp, Float val);
void set_recorder_srate(recorder_info *rp, int val);
void reflect_recorder_mixer_gain(int ind, Float val);
void reflect_recorder_out_amp(int ind, Float val);
void reflect_recorder_in_amp(int in, int out, Float val);
char *channel_name(int in_chans, int out_chans, int chan);
char *out_channel_name(int chan);
char *gain_channel_name(int in_chans, int out_chans, int input, int dev_in, int out);
Float mixer_gain(int system, int device, int chan, int gain, int field);
void set_mixer_gain(int system, int device, int chan, int gain, int field, Float amp) ;
void set_record_size (int new_size);
void reflect_record_size(int val);
void reflect_recorder_duration(Float new_dur);

int device_channels(int dev); /* audio.c */
/* for testing, it's convenient to fake up a device here */
/* static int device_channels(int dev) {return(8);} */
int device_gains(int dev);

void set_read_in_progress (snd_state *ss);
int in_chans_active(void);
int out_chans_active(void);
void recorder_characterize_devices(int devs, int output_devices);
void recorder_set_vu_in_val(int chan, MUS_SAMPLE_TYPE val);
void recorder_set_vu_out_val(int chan, MUS_SAMPLE_TYPE val);

void sensitize_control_buttons(void);
void unsensitize_control_buttons(void);
int recorder_start_output_file(snd_state *ss, char *comment);
void finish_recording(snd_state *ss, recorder_info *rp);

#if OLD_SGI_AL
  void set_line_source(snd_state *ss, int in_digital);
#endif

#if USE_MOTIF
  #define FONT_TYPE XFontStruct
  #define LOAD_FONT(Font) XLoadQueryFont(MAIN_DISPLAY(ss), Font)
#else
  #if USE_GTK
    #define FONT_TYPE SG_FONT
    #define LOAD_FONT(Font) SG_FONT_LOAD(Font)
  #else
    #define FONT_TYPE int
    #define LOAD_FONT(Font) NULL
  #endif
#endif

FONT_TYPE *get_vu_font(snd_state *ss, Float size);

#endif
