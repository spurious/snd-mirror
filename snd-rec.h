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

#define DEFAULT_RECORDER_AUTOLOAD false
#define DEFAULT_RECORDER_FILE NULL
#define DEFAULT_RECORDER_TRIGGER 0.0
#define DEFAULT_RECORDER_MAX_DURATION 1000000.0
#define DEFAULT_RECORDER_OUT_FORMAT MUS_COMPATIBLE_FORMAT
#define DEFAULT_RECORDER_OUT_CHANS 2
#define DEFAULT_RECORDER_IN_CHANS 0

#ifdef SUN
  #define DEFAULT_RECORDER_BUFFER_SIZE 4096
  #if MUS_LITTLE_ENDIAN
    #define DEFAULT_RECORDER_IN_FORMAT MUS_LSHORT
    #define DEFAULT_RECORDER_SRATE 22050
  #else
    #define DEFAULT_RECORDER_IN_FORMAT MUS_MULAW
    #define DEFAULT_RECORDER_SRATE 8000
  #endif
#else
  #define DEFAULT_RECORDER_IN_FORMAT MUS_COMPATIBLE_FORMAT
  #if MAC_OSX
    #define DEFAULT_RECORDER_BUFFER_SIZE 1024
    #define DEFAULT_RECORDER_SRATE 44100
  #else
    #define DEFAULT_RECORDER_BUFFER_SIZE 4096
    #define DEFAULT_RECORDER_SRATE 22050
  #endif
#endif

int recorder_columns(int vu_meters);
int recorder_sort_mixer_device(void *wd, int i, int chan, bool input, int device, int *mixflds);
void recorder_fill_wd(void *wd, int chan, int field, int device);
int recorder_check_device(int system, int device, int *mixer_gains_posted, int *tone_controls_posted, int *mixflds, int *gains, bool *inp);
void recorder_set_audio_srate(int device, int srate, int system, bool aud);
char *recorder_device_name(int dev);
char *recorder_system_and_device_name(int sys, int dev);
bool recorder_input_device(int dev);
bool recorder_output_device(int dev);

#if (HAVE_OSS || HAVE_ALSA)
  char *recorder_field_abbreviation(int fld);
#endif

typedef struct {
  bool recording;
  int input_ports[MAX_SOUNDCARDS]; /* input (audio hardware) channel (mus_audio_read from this) */
  int possible_input_chans;        /* possible_input_chans is a count of all existing input channels, some of which may be incompatible */
  int input_channels[MAX_SOUNDCARDS];
  bool input_channel_active[MAX_IN_CHANS];     /* is this input channel receiving input */
  bool *chan_in_active;               /* overall_in_chans */
  bool *chan_out_active;              /* (file)_out_chans */
  Float max_duration, trigger;
  int srate, in_format, output_data_format, out_chans, in_chans, buffer_size, in_device;
  bool triggered, triggering, autoload;
  Float **in_amps;                   /* overall_in_chans X out_chans */
  Float *out_amps;                   /* out_chans (independent of file write: monitor vol) */
  Float *mixer_gains;                /* audio gain values (widgets are per pane) */
  int num_mixer_gains;
  bool monitoring;                   /* speakers active (monitor_fd open) */
  int monitor_port;                  /* mus_audio_write to this */
  int hd_audio_out_chans;            /* number of channels being "monitored" -- audio output
				      *   not chans being sent to output file; used in conjunction with monitor_fd.
				      * for example, on some SGI's you can have 4 incoming chans, 
				      *   any number of recorded chans, but only 2 speaker chans
				      *   and on some Linux setups, you can have 2 incoming chans,
				      *   but no ("full duplex") speaker chans.
				      */
  bool taking_input;                 /* is input (port) active -- are we running the ADC(s) */
  char *output_file;
  int output_file_descriptor;        /* mus_file_write to this */
  int output_header_type;
  int duration_label_update_frames;  /* frames between updates of the duration label */
  off_t total_output_frames;
  int systems;                       /* soundcards normally = how many independent input sources from sndlib's point of view */
  int *ordered_devices, *ordered_systems; /* soundcards in recorder dialog order with output at end */
  int ordered_devices_size;
  int autoload_button;
  int digital_in_button;
  int microphone_button;
  int line_in_button;
} recorder_info;

recorder_info *get_recorder_info(void);
int recorder_get_devices(recorder_info *rp, int *outs);
void set_recorder_autoload(recorder_info *rp, bool val);
void set_recorder_trigger(recorder_info *rp, Float val);
void set_recorder_srate(recorder_info *rp, int val);
void reflect_recorder_mixer_gain(int ind, Float val);
void reflect_recorder_out_amp(int ind, Float val);
void reflect_recorder_in_amp(int in, int out, Float val);
char *channel_name(int in_chans, int out_chans, int chan);
char *gain_channel_name(int in_chans, int out_chans, bool input, int dev_in, int out);
Float mixer_gain(int system, int device, int chan, int gain, int field);
void set_mixer_gain(int system, int device, int chan, int gain, int field, Float amp) ;
void set_record_size (int new_size);
void reflect_record_size(int val);
void reflect_recorder_duration(Float new_dur);

int device_channels(int dev); /* audio.c */
/* for testing, it's convenient to fake up a device here */
/* static int device_channels(int dev) {return(8);} */
int device_gains(int dev);

void set_read_in_progress (void);
int in_chans_active(void);
int out_chans_active(void);
void recorder_characterize_devices(int devs, int output_devices);
void recorder_set_vu_in_val(int chan, mus_sample_t val);
void recorder_set_vu_out_val(int chan, mus_sample_t val);

void sensitize_control_buttons(void);
void unsensitize_control_buttons(void);
bool recorder_start_output_file(const char *comment);
void finish_recording(recorder_info *rp);

#if OLD_SGI_AL
  void set_line_source(bool in_digital);
#endif

#if USE_MOTIF
  #define font_t XFontStruct
  #define LOAD_FONT(Font) XLoadQueryFont(MAIN_DISPLAY(ss), Font)
#else
  #if USE_GTK
    #define font_t PangoFontDescription
    #define LOAD_FONT(Font) pango_font_description_from_string(Font)
  #else
    #define font_t int
    #define LOAD_FONT(Font) NULL
  #endif
#endif

font_t *get_vu_font(Float size);

#endif
