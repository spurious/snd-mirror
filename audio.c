/* Audio hardware handlers (SGI, OSS, ALSA, Sun, Mac, Windows, HPUX, Mac OSX, ESD) */

/*
 * layout of this file:
 *    error handlers
 *    SGI new and old audio library
 *    OSS (with Sam 9407 support)
 *    ALSA
 *    Sun (has switches for OPENBSD, but they're untested)
 *    Mac (Mac OS 8.1) -- this code will be removed 
 *    HPUX -- untested, will also be removed
 *    Windows 95/98
 *    OSX
 *    ESD
 *    audio describers
 */

/* SOMEDAY: linux jack support */

/*
 * void mus_audio_describe(void) describes the audio hardware state.
 * char *mus_audio_report(void) returns the same information as a string.
 *
 * int mus_audio_open_output(int dev, int srate, int chans, int format, int size)
 * int mus_audio_open_input(int dev, int srate, int chans, int format, int size)
 * int mus_audio_write(int line, char *buf, int bytes)
 * int mus_audio_close(int line)
 * int mus_audio_read(int line, char *buf, int bytes)
 *
 * int mus_audio_mixer_read(int dev, int field, int chan, float *val)
 * int mus_audio_mixer_write(int dev, int field, int chan, float *val)
 * void mus_audio_save(void)
 * void mus_audio_restore(void)
 * int mus_audio_initialize(void) does whatever is needed to get set up
 * int mus_audio_systems(void) returns number of separate complete audio systems (soundcards essentially)
 * AUDIO_SYSTEM(n) selects the nth card (counting from 0), AUDIO_SYSTEM(0) is always the default
 * char *mus_audio_system_name(int system) returns some user-recognizable (?) name for the given card (don't free)
 * char *mus_audio_moniker(void) returns some brief description of the overall audio setup (don't free return string).
 */

/* error handling is tricky here -- higher levels are using many calls as probes, so
 *   the "error" is a sign of non-existence, not a true error.  So, for nearly all
 *   cases, I'll use mus_print, not mus_error.
 */

#if defined(HAVE_CONFIG_H)
  #include <config.h>
#endif

#if USE_SND && MAC_OSX && USE_MOTIF
  #undef USE_MOTIF
  #define USE_NO_GUI 1
  /* Xt's Boolean collides with MacTypes.h Boolean, but we want snd.h for other stuff,
   *   so, if Motif is in use, don't load its headers at this time
   */
#endif

#if USE_SND
  #include "snd.h"
#else
  #define PRINT_BUFFER_SIZE 512
  #define LABEL_BUFFER_SIZE 64
#endif

#if USE_SND && MAC_OSX
  #define USE_MOTIF 1
  #undef USE_NO_GUI
#endif

#include <math.h>
#include <stdio.h>
#if (!defined(HAVE_CONFIG_H)) || HAVE_FCNTL_H
  #include <fcntl.h>
#endif
#include <errno.h>
#include <stdlib.h>
#if (defined(HAVE_LIBC_H) && (!defined(HAVE_UNISTD_H)))
  #include <libc.h>
#else
  #if (!(defined(_MSC_VER))) && (!(defined(MPW_C)))
    #include <unistd.h>
  #endif
#endif
#if (!defined(HAVE_CONFIG_H)) || HAVE_STRING_H
  #include <string.h>
#endif

#if (defined(HAVE_CONFIG_H)) && (!HAVE_STRERROR)
char *strerror(int errnum)
{
  char *strerrbuf;
  strerrbuf = (char *)CALLOC(LABEL_BUFFER_SIZE, sizeof(char));
  mus_snprintf(strerrbuf, LABEL_BUFFER_SIZE, "io err %d", errnum);
  return(strerrbuf);
}
#endif

#if HAVE_SAM_9407
  #include <sys/sam9407.h>
#endif

#include "sndlib.h"
#include "sndlib-strings.h"

#define MUS_STANDARD_ERROR(Error_Type, Error_Message) \
  mus_print("%s\n  [%s[%d] %s]", Error_Message, __FILE__, __LINE__, __FUNCTION__)

#define MUS_STANDARD_IO_ERROR(Error_Type, IO_Func, IO_Name) \
  mus_print("%s %s: %s\n  [%s[%d] %s]", IO_Func, IO_Name, strerror(errno), __FILE__, __LINE__, __FUNCTION__)


static char *version_name = NULL;
static int audio_initialized = FALSE;

static const char *mus_audio_device_names[] = {
  S_mus_audio_default, S_mus_audio_duplex_default, S_mus_audio_adat_in, S_mus_audio_aes_in, S_mus_audio_line_out,
  S_mus_audio_line_in, S_mus_audio_microphone, S_mus_audio_speakers, S_mus_audio_digital_in, S_mus_audio_digital_out,
  S_mus_audio_dac_out, S_mus_audio_adat_out, S_mus_audio_aes_out, S_mus_audio_dac_filter, S_mus_audio_mixer,
  S_mus_audio_line1, S_mus_audio_line2, S_mus_audio_line3, S_mus_audio_aux_input, S_mus_audio_cd,
  S_mus_audio_aux_output, S_mus_audio_spdif_in, S_mus_audio_spdif_out, S_mus_audio_amp, S_mus_audio_srate,
  S_mus_audio_channel, S_mus_audio_format, S_mus_audio_imix, S_mus_audio_igain, S_mus_audio_reclev,
  S_mus_audio_pcm, S_mus_audio_pcm2, S_mus_audio_ogain, S_mus_audio_line, S_mus_audio_synth,
  S_mus_audio_bass, S_mus_audio_treble, S_mus_audio_port, S_mus_audio_samples_per_channel,
  S_mus_audio_direction
};

static const char *mus_audio_device_name(int dev)
{
  if (MUS_AUDIO_DEVICE_OK(dev))
    return(mus_audio_device_names[dev]);
  return("invalid device");
}

#if (!HAVE_OSS) || (HAVE_ALSA)
static const char *mus_audio_format_names[] = {
  "unknown", S_mus_bshort, S_mus_mulaw, S_mus_byte, S_mus_bfloat, S_mus_bint, S_mus_alaw, S_mus_ubyte, S_mus_b24int,
  S_mus_bdouble, S_mus_lshort, S_mus_lint, S_mus_lfloat, S_mus_ldouble, S_mus_ubshort, S_mus_ulshort, S_mus_l24int,
  S_mus_bintn, S_mus_lintn
};

static const char *mus_audio_format_name(int fr)
{
  if (MUS_DATA_FORMAT_OK(fr))
    return(mus_audio_format_names[fr]);
  return("invalid format");
}
#endif

static char *audio_strbuf = NULL; /* previous name "strbuf" collides with Mac OSX global! */
static void pprint(char *str);

int device_channels(int dev);
int device_gains(int dev);

int device_channels(int dev)
{
  float val[4];
#if USE_SND && DEBUGGING
  XEN res;
  int chans;
  res = XEN_EVAL_C_STRING("(if (defined? 'debugging-device-channels) debugging-device-channels 0)");
  if (XEN_INTEGER_P(res))
    {
      chans = XEN_TO_C_INT(res);
      if (chans > 0) return(chans);
    }
#endif
  mus_audio_mixer_read(dev, MUS_AUDIO_CHANNEL, 0, val);
  return((int)val[0]);
}

int device_gains(int ur_dev)
{
  float val[4];
  int err;
  int dev;
  dev = MUS_AUDIO_DEVICE(ur_dev);
  /* to get hardware gains, read device amp_field and error = none */
  if ((dev == MUS_AUDIO_DAC_FILTER) || (dev == MUS_AUDIO_MIXER)) 
    {
      err = mus_audio_mixer_read(ur_dev, MUS_AUDIO_CHANNEL, 0, val);
#ifdef HAVE_ALSA
      if (err != MUS_NO_ERROR) return(0);
#endif
      return((int)val[0]);
    }
  err = mus_audio_mixer_read(ur_dev, MUS_AUDIO_AMP, 0, val);
  if (err != MUS_NO_ERROR) return(0);
  return(device_channels(ur_dev));
}



/* ------------------------------- SGI ----------------------------------------- */

#ifdef SGI
#define AUDIO_OK

#include <audio.h>

int mus_audio_systems(void) {return(1);} /* I think more than 1 is possible, but don't have a case to test with */

char *mus_audio_system_name(int system) {return("SGI");}

char *mus_audio_moniker(void) 
{
#ifdef AL_RESOURCE
  return("New SGI audio");
#else
  return("Old SGI audio");
#endif
}

#ifndef AL_RESOURCE
static char *alGetErrorString(int err)
{
  switch (err)
    {
    case AL_BAD_NOT_IMPLEMENTED:   return("not implemented yet");                   break;
    case AL_BAD_PORT:              return("tried to use an invalid port");          break;
    case AL_BAD_CONFIG:            return("tried to use an invalid configuration"); break;
    case AL_BAD_DEVICE:            return("tried to use an invalid device");        break;
    case AL_BAD_DEVICE_ACCESS:     return("unable to access the device");           break;
    case AL_BAD_DIRECTION:         return("illegal direction given for port");      break;
    case AL_BAD_OUT_OF_MEM:        return("operation has run out of memory");       break;
    case AL_BAD_NO_PORTS:          return("not able to allocate a port");           break;
    case AL_BAD_WIDTH:             return("invalid sample width given");            break;
    case AL_BAD_ILLEGAL_STATE:     return("an illegal state has occurred");         break;
    case AL_BAD_QSIZE:             return("attempt to set an invalid queue size");  break;
    case AL_BAD_FILLPOINT:         return("attempt to set an invalid fillpoint");   break;
    case AL_BAD_BUFFER_NULL:       return("null buffer pointer");                   break;
    case AL_BAD_COUNT_NEG:         return("negative count");                        break;
    case AL_BAD_PVBUFFER:          return("param/val buffer doesn't make sense");   break;
    case AL_BAD_BUFFERLENGTH_NEG:  return("negative buffer length");                break;
    case AL_BAD_BUFFERLENGTH_ODD:  return("odd length parameter/value buffer");     break;
    case AL_BAD_CHANNELS:          return("invalid channel specifier");             break;
    case AL_BAD_PARAM:             return("invalid parameter");                     break;
    case AL_BAD_SAMPFMT:           return("attempt to set invalid sample format");  break;
    case AL_BAD_RATE:              return("invalid sample rate token");             break;
    case AL_BAD_TRANSFER_SIZE:     return("invalid size for sample read/write");    break;
    case AL_BAD_FLOATMAX:          return("invalid size for floatmax");             break;
    case AL_BAD_PORTSTYLE:         return("invalid port style");                    break;
    default:                       return("");
    }
}
#endif

static char *sgi_err_buf = NULL;
static mus_print_handler_t *old_handler = NULL;

static void sgi_mus_print(char *msg)
{
  int oserr = oserror();
  if (oserr)
    {
      if (sgi_err_buf == NULL) sgi_err_buf = (char *)CALLOC(PRINT_BUFFER_SIZE, sizeof(char));
      mus_snprintf(sgi_err_buf, PRINT_BUFFER_SIZE, "%s [%s]", msg, alGetErrorString(oserr));
      (*old_handler)(sgi_err_buf);
    }
  else (*old_handler)(msg);
}

static void start_sgi_print(void)
{
  if (old_handler != sgi_mus_print)
    old_handler = mus_print_set_handler(sgi_mus_print);
}

static void end_sgi_print(void)
{
  if (old_handler != sgi_mus_print)
    mus_print_set_handler(old_handler);
  else mus_print_set_handler(NULL);
}


#if AL_RESOURCE
  #define al_free(Line)                  alFreeConfig(config[Line])
  #define al_newconfig()                 alNewConfig()
  #define al_setsampfmt(Line, Format)    alSetSampFmt(Line, Format)
  #define al_setchannels(Line, Chans)    alSetChannels(Line, Chans)
  #define al_setwidth(Line, Width)       alSetWidth(Line, Width)
  #define al_setqueuesize(Line, Size)    alSetQueueSize(Line, Size)
  #define al_openport(Name, Flag, Line)  alOpenPort(Name, Flag, Line)
  #define al_getfilled(Port)             alGetFilled(Port)
  #define al_closeport(Port)             alClosePort(Port)
  #define al_freeconfig(Config)          alFreeConfig(Config)
#else
  #define al_free(Line)                  ALfreeconfig(config[Line]);
  #define al_newconfig()                 ALnewconfig()
  #define al_setsampfmt(Line, Format)    ALsetsampfmt(Line, Format)
  #define al_setchannels(Line, Chans)    ALsetchannels(Line, Chans)
  #define al_setwidth(Line, Width)       ALsetwidth(Line, Width)
  #define al_setqueuesize(Line, Size)    ALsetqueuesize(Line, Size)
  #define al_openport(Name, Flag, Line)  ALopenport(Name, Flag, Line)
  #define al_getfilled(Port)             ALgetfilled(Port)
  #define al_closeport(Port)             ALcloseport(Port)
  #define al_freeconfig(Config)          ALfreeconfig(Config)
#endif


#define RETURN_ERROR_EXIT(Error_Type, Audio_Line, Ur_Error_Message) \
  do { char *Error_Message; Error_Message = Ur_Error_Message; \
    if (Audio_Line != -1) al_free(Audio_Line); \
    if (Error_Message) \
      {MUS_STANDARD_ERROR(Error_Type, Error_Message); FREE(Error_Message);} \
    else MUS_STANDARD_ERROR(Error_Type, mus_error_to_string(Error_Type)); \
    end_sgi_print(); \
    return(MUS_ERROR); \
  } while (0)


#ifdef AL_RESOURCE

static int check_queue_size (int size, int chans) 
{
  if (size > chans * 1024)
    return(size);
  else return(chans * 1024);
}

#else

#define STEREO_QUEUE_MIN_SIZE 1024
#define STEREO_QUEUE_MIN_CHOICE 1024
/* docs say 510 or 512, but they die with "File size limit exceeded" %$@#!(& */
#define MONO_QUEUE_MIN_SIZE 1019
#define MONO_QUEUE_MIN_CHOICE 1024
#define STEREO_QUEUE_MAX_SIZE 131069
#define STEREO_QUEUE_MAX_CHOICE 65536
#define MONO_QUEUE_MAX_SIZE 262139
#define MONO_QUEUE_MAX_CHOICE 131072
/* if these limits are not followed, the damned thing dumps core and dies */

static int check_queue_size (int size, int chans)
{
  if ((chans == 1) && (size > MONO_QUEUE_MAX_SIZE)) return(MONO_QUEUE_MAX_CHOICE);
  if ((chans == 1) && (size < MONO_QUEUE_MIN_SIZE)) return(MONO_QUEUE_MIN_CHOICE);
  if ((chans > 1) && (size > STEREO_QUEUE_MAX_SIZE)) return(STEREO_QUEUE_MAX_CHOICE);
  if ((chans > 1) && (size < STEREO_QUEUE_MIN_SIZE)) return(STEREO_QUEUE_MIN_CHOICE);
  return(size);
}

static void check_quad (int device, int channels)
{
  long sr[2];
  /* if quad, make sure we are set up for it, else make sure we aren't (perhaps the latter is unnecessary) */
  /* in 4 channel mode, stereo mic and line-in are 4 inputs, headphones/speakers and stereo line-out are the 4 outputs */
  sr[0] = AL_CHANNEL_MODE;
  ALgetparams(device, sr, 2);
  if ((channels == 4) && (sr[1] != AL_4CHANNEL))
    {
      sr[1] = AL_4CHANNEL;
      ALsetparams(device, sr, 2);
    }
  else
    {
      if ((channels != 4) && (sr[1] != AL_STEREO))
        {
          sr[1] = AL_STEREO;
          ALsetparams(device, sr, 2);
        }
    }
}
#endif

#define IO_LINES 8
static ALconfig *config = NULL;
static ALport *port = NULL;
static int *line_in_use = NULL;
static int *channels = NULL;
static long *device = NULL;
static int *datum_size = NULL;
static int *line_out = NULL;

int mus_audio_initialize(void)
{
  if (!audio_initialized)
    {
      audio_initialized = TRUE;
      config = (ALconfig *)CALLOC(IO_LINES, sizeof(ALconfig));
      port = (ALport *)CALLOC(IO_LINES, sizeof(ALport));
      line_in_use = (int *)CALLOC(IO_LINES, sizeof(int));
      channels = (int *)CALLOC(IO_LINES, sizeof(int));
      device = (long *)CALLOC(IO_LINES, sizeof(long));
      datum_size = (int *)CALLOC(IO_LINES, sizeof(int));
      line_out = (int *)CALLOC(IO_LINES, sizeof(int));
    }
  return(MUS_NO_ERROR);
}

#ifdef AL_RESOURCE
static int to_al_interface_or_device(int dev, int which)
{
  switch (dev)
    {
    case MUS_AUDIO_DEFAULT:
    case MUS_AUDIO_DUPLEX_DEFAULT: return(AL_DEFAULT_OUTPUT);                                 break;
    case MUS_AUDIO_DAC_OUT:
    case MUS_AUDIO_SPEAKERS:       return(alGetResourceByName(AL_SYSTEM, "Analog Out", which)); break;
    case MUS_AUDIO_MICROPHONE:     return(alGetResourceByName(AL_SYSTEM, "Microphone", which)); break;
    case MUS_AUDIO_ADAT_IN:        return(alGetResourceByName(AL_SYSTEM, "ADAT In", which));    break;
    case MUS_AUDIO_AES_IN:         return(alGetResourceByName(AL_SYSTEM, "AES In", which));     break;
    case MUS_AUDIO_ADAT_OUT:       return(alGetResourceByName(AL_SYSTEM, "ADAT Out", which));   break;
    case MUS_AUDIO_DIGITAL_OUT:
    case MUS_AUDIO_AES_OUT:        return(alGetResourceByName(AL_SYSTEM, "AES Out", which));    break;
    case MUS_AUDIO_LINE_IN:        return(alGetResourceByName(AL_SYSTEM, "Line In", which));    break;
    case MUS_AUDIO_LINE_OUT:       return(alGetResourceByName(AL_SYSTEM, "Line Out2", which));  break; /* ?? */
      /* case MUS_AUDIO_DIGITAL_IN: return(alGetResourceByName(AL_SYSTEM, "DAC2 In", which));   break; */ /* this is analog in ?? */
    }
  return(MUS_ERROR);
}

static int to_al_device(int dev) 
{
  return(to_al_interface_or_device(dev, AL_DEVICE_TYPE));
}

static int to_al_interface(int dev) 
{
  return(to_al_interface_or_device(dev, AL_INTERFACE_TYPE));
}
#endif

#include <stdio.h>

/* just a placeholder for now */
int find_audio_output(int chans)
{
#ifdef AL_RESOURCE
  ALvalue x[32];
  ALpv y;
  int n, i;
  y.param = AL_INTERFACE;
  y.value.i = AL_DIGITAL_IF_TYPE;
  n = alQueryValues(AL_SYSTEM, AL_DEFAULT_OUTPUT, x, 32, &y, 1);
  for (i = 0; i < n; i++) 
    {
      y.param = AL_CHANNELS;
      alGetParams(x[i].i, &y, 1);
      if (chans <= y.value.i) return(x[i].i);
    }
#endif
  return(MUS_ERROR);
}

static int to_sgi_format(int frm)
{
  switch (frm)
    {
    case MUS_BYTE: 
    case MUS_BSHORT: 
    case MUS_B24INT: return(AL_SAMPFMT_TWOSCOMP); break;
    case MUS_BFLOAT: return(AL_SAMPFMT_FLOAT); break;
    case MUS_BDOUBLE: return(AL_SAMPFMT_DOUBLE); break;
    }
  return(MUS_ERROR);
}

int mus_audio_open_output(int ur_dev, int srate, int chans, int format, int requested_size)
{
#ifdef AL_RESOURCE
  ALpv z[2];
#endif
  long sr[2];
  int i, line, size, width, sgi_format, dev;
  start_sgi_print();
  dev = MUS_AUDIO_DEVICE(ur_dev);
  line = -1;
  for (i = 0; i < IO_LINES; i++) 
    if (line_in_use[i] == 0) 
      {
	line = i; 
	break;
      }
  if (line == -1) 
    RETURN_ERROR_EXIT(MUS_AUDIO_NO_LINES_AVAILABLE, line,
		      mus_format("no free audio lines?"));
  channels[line] = chans;
  line_out[line] = 1;

  if (requested_size == 0) 
    size = 1024 * chans;
  else size = check_queue_size(requested_size, chans);
  /* if (chans > 2) size = 65536; */                            /* for temp adat code */

  datum_size[line] = mus_bytes_per_sample(format);
  if (datum_size[line] == 3) 
    width = AL_SAMPLE_24;
  else 
    {
      if (datum_size[line] == 1) 
        width = AL_SAMPLE_8;
      else width = AL_SAMPLE_16;
    }
  sgi_format = to_sgi_format(format);
  if (sgi_format == MUS_ERROR)
    RETURN_ERROR_EXIT(MUS_AUDIO_FORMAT_NOT_AVAILABLE, -1,
		      mus_format("format %d (%s) not supported by SGI",
				 format, 
				 mus_audio_format_name(format)));
#ifdef AL_RESOURCE
  if (dev == MUS_AUDIO_DEFAULT)
    device[line] = AL_DEFAULT_OUTPUT;
  else device[line] = to_al_device(dev);
  if (!(device[line])) 
    RETURN_ERROR_EXIT(MUS_AUDIO_DEVICE_NOT_AVAILABLE, -1,
		      mus_format("device %d (%s) not available",
				 dev,
				 mus_audio_device_name(dev)));
#if 0
  if (device_channels(dev) < chans)                     /* look for some device that can play this file */
    device[line] = find_audio_output(chans);
  if (device[line] == -1) 
    RETURN_ERROR_EXIT(MUS_AUDIO_CHANNELS_NOT_AVAILABLE, -1,
		      mus_format("can't find %d channel device",
				 chans));
#endif
  if ((chans == 4) && (dev == MUS_AUDIO_DAC_OUT))
    {                                                   /* kludge around a bug in the new audio library */
      sr[0] = AL_CHANNEL_MODE;
      sr[1] = AL_4CHANNEL;
      ALsetparams(AL_DEFAULT_DEVICE, sr, 2);
    }
  z[0].param = AL_RATE;
  z[0].value.ll = alDoubleToFixed((double)srate);
  z[1].param = AL_MASTER_CLOCK;
  /* z[1].value.i = AL_CRYSTAL_MCLK_TYPE; */
  z[1].value.i = AL_MCLK_TYPE;                          /* was AL_CRYSTAL_MCLK_TYPE -- digital I/O perhaps needs AL_VARIABLE_MCLK_TYPE */
  if (alSetParams(device[line], z, 2) == -1)
    RETURN_ERROR_EXIT(MUS_AUDIO_SRATE_NOT_AVAILABLE, -1,
		      mus_format("can't set srate of %s to %d",
				 mus_audio_device_name(dev),
				 srate));
#else
  device[line] = AL_DEFAULT_DEVICE;
  check_quad(device[line], chans);
  sr[0] = AL_OUTPUT_RATE;
  sr[1] = srate;
  if (ALsetparams(device[line], sr, 2) == -1)
    RETURN_ERROR_EXIT(MUS_AUDIO_SRATE_NOT_AVAILABLE, -1,
		      mus_format("can't set srate of %s to %d",
				 mus_audio_device_name(dev),
				 srate));
#endif

  config[line] = al_newconfig();
  if (!(config[line])) 
    RETURN_ERROR_EXIT(MUS_AUDIO_CONFIGURATION_NOT_AVAILABLE, -1,
		      mus_format("can't allocate audio configuration?"));
  if ((al_setsampfmt(config[line], sgi_format) == -1) ||
      (al_setwidth(config[line], width) == -1))           /* this is a no-op in the float and double cases */
    RETURN_ERROR_EXIT(MUS_AUDIO_FORMAT_NOT_AVAILABLE, line,
		      mus_format("audio format %d (%s, SGI: %d) not available on device %d (%s)",
				 format, mus_audio_format_name(format), sgi_format,
				 dev, 
				 mus_audio_device_name(dev)));
  if (al_setchannels(config[line], chans) == -1)
    RETURN_ERROR_EXIT(MUS_AUDIO_CHANNELS_NOT_AVAILABLE, line,
		      mus_format("can't get %d channels on device %d (%s)",
				 chans, dev, mus_audio_device_name(dev)));

  /* set queue size probably needs a check first for legal queue sizes given the current desired device */
  /*   in new AL, I'm assuming above (check_queue_size) that it needs at least 1024 per chan */
  if (al_setqueuesize(config[line], size) == -1)
    RETURN_ERROR_EXIT(MUS_AUDIO_SIZE_NOT_AVAILABLE, line,
		      mus_format("can't get queue size %d on device %d (%s) (chans: %d, requested_size: %d)",
				 size, dev, 
				 mus_audio_device_name(dev), 
				 chans, requested_size));

#ifdef AL_RESOURCE
  if (alSetDevice(config[line], device[line]) == -1)
    RETURN_ERROR_EXIT(MUS_AUDIO_DEVICE_NOT_AVAILABLE, line,
		      mus_format("can't get device %d (%s)",
				 dev, 
				 mus_audio_device_name(dev)));
#endif

  port[line] = al_openport("dac", "w", config[line]);
  if (!(port[line])) 
    RETURN_ERROR_EXIT(MUS_AUDIO_CONFIGURATION_NOT_AVAILABLE, line,
		      mus_format("can't open output port on device %d (%s)",
				 dev, 
				 mus_audio_device_name(dev)));
  line_in_use[line] = 1;
  end_sgi_print();
  return(line);
}

int mus_audio_write(int line, char *buf, int bytes)
{
  start_sgi_print();
#ifdef AL_RESOURCE
  if (alWriteFrames(port[line], (short *)buf, bytes / (channels[line] * datum_size[line])))
#else
  if (ALwritesamps(port[line], (short *)buf, bytes / datum_size[line]))
#endif
    RETURN_ERROR_EXIT(MUS_AUDIO_WRITE_ERROR, -1,
		      mus_format("write error"));
  end_sgi_print();
  return(MUS_NO_ERROR);
}

int mus_audio_close(int line)
{
  int err;
  start_sgi_print();
  if (line_in_use[line])
    {
      if (line_out[line])
	while (al_getfilled(port[line]) > 0) 
	  sginap(1);
      err = ((al_closeport(port[line])) || 
	     (al_freeconfig(config[line])));
      line_in_use[line] = 0;
      if (err) 
	RETURN_ERROR_EXIT(MUS_AUDIO_CANT_CLOSE, -1,
			  mus_format("can't close audio port %p (line %d)",
				     port[line], line));
    }
  end_sgi_print();
  return(MUS_NO_ERROR);
}

int mus_audio_open_input(int ur_dev, int srate, int chans, int format, int requested_size)
{
  int dev;
#ifdef AL_RESOURCE
  ALpv pv;
  ALpv x[2];
  int itf;
#else
  long sr[2];
  int resind;
#endif
  int i, line, sgi_format;
  start_sgi_print();
  dev = MUS_AUDIO_DEVICE(ur_dev);
  line = -1;
  for (i = 0; i < IO_LINES; i++) 
    if (line_in_use[i] == 0) 
      {
	line = i; 
	break;
      }
  if (line == -1)
    RETURN_ERROR_EXIT(MUS_AUDIO_NO_LINES_AVAILABLE, -1,
		      mus_format("no free audio lines?"));
  channels[line] = chans;
  line_out[line] = 0;
  datum_size[line] = mus_bytes_per_sample(format);
#ifdef AL_RESOURCE
  if (dev == MUS_AUDIO_DEFAULT)
    device[line] = AL_DEFAULT_INPUT;
  else 
    {
      device[line] = to_al_device(dev);
      itf = to_al_interface(dev);
      if (itf)
	{ 
	  pv.param = AL_INTERFACE;
	  pv.value.i = itf;
	  if (alSetParams(device[line], &pv, 1) == -1)
	    RETURN_ERROR_EXIT(MUS_AUDIO_CONFIGURATION_NOT_AVAILABLE, -1,
			      mus_format("can't set up device %d (%s)",
					 dev, 
					 mus_audio_device_name(dev)));
	}
    }
  if (!(device[line])) 
    RETURN_ERROR_EXIT(MUS_AUDIO_DEVICE_NOT_AVAILABLE, -1,
		      mus_format("can't get input device %d (%s)",
				 dev, mus_audio_device_name(dev)));
  x[0].param = AL_RATE;
  x[0].value.ll = alDoubleToFixed((double)srate);
  x[1].param = AL_MASTER_CLOCK;
  x[1].value.i = AL_MCLK_TYPE; /* AL_CRYSTAL_MCLK_TYPE; */
  if (alSetParams(device[line], x, 2) == -1)
    RETURN_ERROR_EXIT(MUS_AUDIO_SRATE_NOT_AVAILABLE, -1,
		      mus_format("can't set srate of %s to %d",
				 mus_audio_device_name(dev),
				 srate));
#else
  switch (dev)
    {
    case MUS_AUDIO_DEFAULT: 
    case MUS_AUDIO_DUPLEX_DEFAULT: 
    case MUS_AUDIO_MICROPHONE: resind = AL_INPUT_MIC; break;
    case MUS_AUDIO_LINE_IN: resind = AL_INPUT_LINE; break;
    case MUS_AUDIO_DIGITAL_IN: resind = AL_INPUT_DIGITAL; break;
    default: 
      RETURN_ERROR_EXIT(MUS_AUDIO_CONFIGURATION_NOT_AVAILABLE, -1,
			mus_format("audio input device %d (%s) not available",
				   dev, 
				   mus_audio_device_name(dev)));
      break;
    }
  device[line] = AL_DEFAULT_DEVICE;
  sr[0] = AL_INPUT_SOURCE;
  sr[1] = resind;
  if (ALsetparams(device[line], sr, 2) == -1)
    RETURN_ERROR_EXIT(MUS_AUDIO_CONFIGURATION_NOT_AVAILABLE, -1,
		      mus_format("can't set up input device %d (%s)",
				 dev, 
				 mus_audio_device_name(dev)));
  check_quad(device[line], chans);
  sr[0] = AL_INPUT_RATE;
  sr[1] = srate;
  if (ALsetparams(device[line], sr, 2) == -1)
    RETURN_ERROR_EXIT(MUS_AUDIO_SRATE_NOT_AVAILABLE, -1,
		      mus_format("can't set srate of %s to %d",
				 mus_audio_device_name(dev),
				 srate));
#endif

  config[line] = al_newconfig();
  if (!(config[line])) 
    RETURN_ERROR_EXIT(MUS_AUDIO_CONFIGURATION_NOT_AVAILABLE, -1,
		      mus_format("can't allocate audio configuration?"));
  sgi_format = to_sgi_format(format);
  if (sgi_format == MUS_ERROR)
    RETURN_ERROR_EXIT(MUS_AUDIO_FORMAT_NOT_AVAILABLE, -1,
		      mus_format("format %d (%s) not supported by SGI",
				 format, 
				 mus_audio_format_name(format)));
  if ((al_setsampfmt(config[line], sgi_format) == -1) ||
      (al_setwidth(config[line], (datum_size[line] == 2) ? AL_SAMPLE_16 : AL_SAMPLE_8) == -1))
    RETURN_ERROR_EXIT(MUS_AUDIO_FORMAT_NOT_AVAILABLE, line,
		      mus_format("audio format %d (%s, SGI: %d) not available on device %d (%s)",
				 format, 
				 mus_audio_format_name(format), sgi_format,
				 dev,
				 mus_audio_device_name(dev)));
  if (al_setchannels(config[line], chans) == -1)
    RETURN_ERROR_EXIT(MUS_AUDIO_CHANNELS_NOT_AVAILABLE, line,
		      mus_format("can't get %d channels on device %d (%s)",
				 chans, dev, 
				 mus_audio_device_name(dev)));

#ifdef AL_RESOURCE
  if (alSetDevice(config[line], device[line]) == -1)
    RETURN_ERROR_EXIT(MUS_AUDIO_DEVICE_NOT_AVAILABLE, line,
		      mus_format("can't get device %d (%s)",
				 dev, 
				 mus_audio_device_name(dev)));
#endif

  port[line] = al_openport("adc", "r", config[line]);
  if (!(port[line])) 
    RETURN_ERROR_EXIT(MUS_AUDIO_CONFIGURATION_NOT_AVAILABLE, line,
		      mus_format("can't open input port on device %d (%s)",
				 dev, 
				 mus_audio_device_name(dev)));
  line_in_use[line] = 1;
  end_sgi_print();
  return(line);
}

int mus_audio_read(int line, char *buf, int bytes)
{
  start_sgi_print();
#ifdef AL_RESOURCE
  if (alReadFrames(port[line], (short *)buf, bytes / (channels[line] * datum_size[line])))
#else
  if (ALreadsamps(port[line], (short *)buf, bytes / datum_size[line]))
#endif
    RETURN_ERROR_EXIT(MUS_AUDIO_READ_ERROR, -1,
		      mus_format("read error"));
  end_sgi_print();
  return(MUS_NO_ERROR);
}


#ifdef AL_RESOURCE
/* borrowed from /usr/share/src/dmedia/audio/printdevs.c with modifications */

#define MAX_CHANNELS 8

static float dB_to_linear(float val)
{
  if (val == 0.0) return(1.0);
  return(pow(10.0, val / 20.0));
}

static float dB_to_normalized(float val, float lo, float hi)
{
  float linlo;
  if (hi <= lo) return(1.0);
  linlo = dB_to_linear(lo);
  return((dB_to_linear(val) - linlo) / (dB_to_linear(hi) - linlo));
}

static float normalized_to_dB(float val_norm, float lo_dB, float hi_dB)
{
  if (hi_dB <= lo_dB) return(0.0);
  return(lo_dB + (hi_dB - lo_dB) * val_norm);
}

int mus_audio_mixer_read(int ur_dev, int field, int chan, float *val)
{
  ALpv x[4];
  ALparamInfo pinf;
  ALfixed g[MAX_CHANNELS];
  int rv = 0, i, dev;
  start_sgi_print();
  dev = MUS_AUDIO_DEVICE(ur_dev);
  if (field != MUS_AUDIO_PORT)
    {
      rv = to_al_device(dev);
      if (!rv) 
	RETURN_ERROR_EXIT(MUS_AUDIO_DEVICE_NOT_AVAILABLE, -1,
			  mus_format("can't read %s field %d (%s)",
				     mus_audio_device_name(dev),
				     field, 
				     mus_audio_device_name(field)));
    }
  switch (field)
    {
    case MUS_AUDIO_PORT:
      /* in this case, chan == length of incoming val array.  Number of devices is returned as val[0],
       * and the rest of the available area (if needed) is filled with the device ids.
       */
      i = 0;
      if (alGetResourceByName(AL_SYSTEM, "Microphone", AL_DEVICE_TYPE) != 0) {if ((i + 1) < chan) val[i + 1] = MUS_AUDIO_MICROPHONE; i++;}
      if (alGetResourceByName(AL_SYSTEM, "Analog Out", AL_DEVICE_TYPE) != 0) {if ((i + 1) < chan) val[i + 1] = MUS_AUDIO_DAC_OUT;    i++;}
      if (alGetResourceByName(AL_SYSTEM, "ADAT In", AL_DEVICE_TYPE) != 0)    {if ((i + 1) < chan) val[i + 1] = MUS_AUDIO_ADAT_IN;    i++;}
      if (alGetResourceByName(AL_SYSTEM, "AES In", AL_DEVICE_TYPE) != 0)     {if ((i + 1) < chan) val[i + 1] = MUS_AUDIO_AES_IN;     i++;}
      if (alGetResourceByName(AL_SYSTEM, "ADAT Out", AL_DEVICE_TYPE) != 0)   {if ((i + 1) < chan) val[i + 1] = MUS_AUDIO_ADAT_OUT;   i++;}
      if (alGetResourceByName(AL_SYSTEM, "AES Out", AL_DEVICE_TYPE) != 0)    {if ((i + 1) < chan) val[i + 1] = MUS_AUDIO_AES_OUT;    i++;}
      if (alGetResourceByName(AL_SYSTEM, "Line In", AL_DEVICE_TYPE) != 0)    {if ((i + 1) < chan) val[i + 1] = MUS_AUDIO_LINE_IN;    i++;}
      /* if (alGetResourceByName(AL_SYSTEM, "DAC2 In", AL_DEVICE_TYPE) != 0) {if ((i + 1) < chan) val[i + 1] = MUS_AUDIO_DIGITAL_IN; i++;} */
      val[0] = i;
      break;
    case MUS_AUDIO_FORMAT:  
      val[0] = 1; 
      if (chan > 1) val[1] = MUS_BSHORT; 
      break;
    case MUS_AUDIO_CHANNEL:
      x[0].param = AL_CHANNELS;
      if (alGetParams(rv, x, 1) == -1)
	RETURN_ERROR_EXIT(MUS_AUDIO_READ_ERROR, -1,
			  mus_format("can't read channel setting of %s",
				     mus_audio_device_name(dev)));
      val[0] = (float)(x[0].value.i);
      break;
    case MUS_AUDIO_SRATE:
      x[0].param = AL_RATE;
      if (alGetParams(rv, x, 1) == -1) 
	RETURN_ERROR_EXIT(MUS_AUDIO_READ_ERROR, -1,
			  mus_format("can't read srate setting of %s",
				     mus_audio_device_name(dev)));
      val[0] = (float)(x[0].value.i);
      break;
    case MUS_AUDIO_AMP:
      if (alGetParamInfo(rv, AL_GAIN, &pinf) == -1) 
	RETURN_ERROR_EXIT(MUS_AUDIO_READ_ERROR, -1,
			  mus_format("can't read gain settings of %s",
				     mus_audio_device_name(dev)));
      if (pinf.min.ll == pinf.max.ll) 
	RETURN_ERROR_EXIT(MUS_AUDIO_AMP_NOT_AVAILABLE, -1,
			  mus_format("%s's gain apparently can't be set",
				     mus_audio_device_name(dev)));
      /* this ridiculous thing is in dB with completely arbitrary min and max values */
      x[0].param = AL_GAIN;
      x[0].value.ptr = g;
      x[0].sizeIn = MAX_CHANNELS;
      alGetParams(rv, x, 1);
      if (x[0].sizeOut <= chan) 
	RETURN_ERROR_EXIT(MUS_AUDIO_CHANNELS_NOT_AVAILABLE, -1,
			  mus_format("can't read gain settings of %s channel %d",
				     mus_audio_device_name(dev), chan));
      val[0] = dB_to_normalized(alFixedToDouble(g[chan]),
				alFixedToDouble(pinf.min.ll),
				alFixedToDouble(pinf.max.ll));
      break;
    default: 
      RETURN_ERROR_EXIT(MUS_AUDIO_CANT_READ, -1,
			  mus_format("can't read %s setting of %s",
				     mus_audio_device_name(field),
				     mus_audio_device_name(dev)));
      break;
    }
  end_sgi_print();
  return(MUS_NO_ERROR);
}

int mus_audio_mixer_write(int ur_dev, int field, int chan, float *val)
{
  /* each field coming in assumes 0.0 to 1.0 as the range */
  ALpv x[4];
  ALparamInfo pinf;
  ALfixed g[MAX_CHANNELS];
  int rv, dev;
  start_sgi_print();
  dev = MUS_AUDIO_DEVICE(ur_dev);
  rv = to_al_device(dev);
  if (!rv) RETURN_ERROR_EXIT(MUS_AUDIO_DEVICE_NOT_AVAILABLE, -1,
			  mus_format("can't write %s field %d (%s)",
				     mus_audio_device_name(dev),
				     field, 
				     mus_audio_device_name(field)));
  switch (field)
    {
    case MUS_AUDIO_SRATE:
      x[0].param = AL_RATE;
      x[0].value.i = (int)val[0];
      x[1].param = AL_MASTER_CLOCK;
      x[1].value.i = AL_CRYSTAL_MCLK_TYPE;
      alSetParams(rv, x, 2); 
      break;
    case MUS_AUDIO_AMP:
      /* need to change normalized linear value into dB between (dB) lo and hi */
      if (alGetParamInfo(rv, AL_GAIN, &pinf) == -1) 
	RETURN_ERROR_EXIT(MUS_AUDIO_READ_ERROR, -1,
			  mus_format("can't write gain settings of %s",
				     mus_audio_device_name(dev)));
      /* I think we need to read all channels here, change the one we care about, then write all channels */
      x[0].param = AL_GAIN;
      x[0].value.ptr = g;
      x[0].sizeIn = MAX_CHANNELS;
      alGetParams(rv, x, 1);
      if (x[0].sizeOut <= chan) 
	RETURN_ERROR_EXIT(MUS_AUDIO_CHANNELS_NOT_AVAILABLE, -1,
			  mus_format("can't write gain settings of %s channel %d",
				     mus_audio_device_name(dev), 
				     chan));
      g[chan] = alDoubleToFixed(normalized_to_dB(val[0], 
						 alFixedToDouble(pinf.min.ll),
						 alFixedToDouble(pinf.max.ll)));
      if (alSetParams(rv, x, 1) == -1)
	RETURN_ERROR_EXIT(MUS_AUDIO_WRITE_ERROR, -1,
			  mus_format("can't write gain settings of %s",
				     mus_audio_device_name(dev)));
      break;
    default: 
      RETURN_ERROR_EXIT(MUS_AUDIO_CANT_READ, -1,
			  mus_format("can't write %s setting of %s",
				     mus_audio_device_name(field),
				     mus_audio_device_name(dev)));
      break;
    }
  end_sgi_print();
  return(MUS_NO_ERROR);
}

#define STRING_SIZE 32
static void dump_resources(ALvalue *x, int rv)
{
  ALpv y[4];
  ALpv yy;
  ALparamInfo pinf;
  ALfixed g[MAX_CHANNELS];
  char dn[STRING_SIZE];
  char dl[STRING_SIZE];
  int i, k;
  ALvalue z[16];
  int nres;
  for (i = 0; i < rv; i++)
    {
      y[0].param = AL_LABEL;
      y[0].value.ptr = dl;
      y[0].sizeIn = STRING_SIZE;
      y[1].param = AL_NAME;
      y[1].value.ptr = dn;
      y[1].sizeIn = STRING_SIZE;
      y[2].param = AL_CHANNELS;
      y[3].param = AL_RATE;
      alGetParams(x[i].i, y, 5);
      if (alIsSubtype(AL_DEVICE_TYPE, x[i].i)) 
        {
          alGetParamInfo(x[i].i, AL_GAIN, &pinf);
          mus_snprintf(audio_strbuf, PRINT_BUFFER_SIZE, "\nDevice: %s (%s), srate: %d, chans: %d",
                  dn, dl,
                  y[3].value.i, 
		  y[2].value.i); 
          pprint(audio_strbuf);
          if (pinf.min.ll != pinf.max.ll)
            {
              yy.param = AL_GAIN;
              yy.value.ptr = g;
              yy.sizeIn = MAX_CHANNELS;
              alGetParams(x[i].i, &yy, 1);
              pprint(" amps:[");
              for (k = 0; k < yy.sizeOut; k++)
                {
                  mus_snprintf(audio_strbuf, PRINT_BUFFER_SIZE, "%.2f",
			  dB_to_normalized(alFixedToDouble(g[k]),
					   alFixedToDouble(pinf.min.ll),
					   alFixedToDouble(pinf.max.ll)));
                  pprint(audio_strbuf);
                  if (k < (yy.sizeOut - 1)) pprint(" ");
                }
              pprint("]");
            }
          pprint("\n");
          if ((nres= alQueryValues(x[i].i, AL_INTERFACE, z, 16, 0, 0)) >= 0) 
            dump_resources(z, nres);
          else mus_snprintf(audio_strbuf, PRINT_BUFFER_SIZE, "query failed: %s\n", alGetErrorString(oserror())); 
          pprint(audio_strbuf);
        }
      else 
        {
          mus_snprintf(audio_strbuf, PRINT_BUFFER_SIZE, "      %s (%s), chans: %d\n", dn, dl, y[2].value.i); 
          pprint(audio_strbuf);
        }
    }
}

static void describe_audio_state_1(void)
{
  int rv;
  ALvalue x[16];
  pprint("Devices and Interfaces on this system:\n"); 
  rv= alQueryValues(AL_SYSTEM, AL_DEVICES, x, 16, 0, 0);
  if (rv > 0) 
    dump_resources(x, rv);
}


typedef struct {
  int dev, has_gains;
  ALfixed gains[MAX_CHANNELS];
  int srate;
} saved_info;

static saved_info **si = NULL;
static int saved_devices = 0;
static int saved_devices_size = 0;

static void save_devices(ALvalue *x, int rv)
{
  ALpv y;
  saved_info *sv;
  ALparamInfo pinf;
  int i;
  ALvalue z[16];
  int nres;
  for (i = 0; i < rv; i++)
    {
      if (saved_devices >= saved_devices_size)
	{
	  if (saved_devices_size == 0)
	    {
	      if (saved_devices >= 16) 
		saved_devices_size = 2 * saved_devices; 
	      else saved_devices_size = 16;
	      si = (saved_info **)CALLOC(saved_devices_size, sizeof(saved_info *));
	    }
	  else
	    {
	      si = (saved_info **)REALLOC(si, 2 * saved_devices * sizeof(saved_info *));
	      for (i = saved_devices_size; i < 2 * saved_devices; i++) si[i] = NULL;
	      saved_devices_size = 2 * saved_devices;
	    }
	}
      si[saved_devices] = (saved_info *)CALLOC(1, sizeof(saved_info));
      sv = si[saved_devices];
      sv->dev = x[i].i;
      y.param = AL_RATE;
      alGetParams(x[i].i, &y, 1);
      sv->srate = y.value.i;
      saved_devices++;
      if (alIsSubtype(AL_DEVICE_TYPE, x[i].i)) 
        {
          alGetParamInfo(x[i].i, AL_GAIN, &pinf);
          if (pinf.min.ll != pinf.max.ll)
            {
              y.param = AL_GAIN;
              y.value.ptr = sv->gains;
              y.sizeIn = MAX_CHANNELS;
              alGetParams(x[i].i, &y, 1);
              sv->has_gains = 1;
            }
          else sv->has_gains = 0;
          if ((nres = alQueryValues(x[i].i, AL_INTERFACE, z, 16, 0, 0)) >= 0) 
	    save_devices(z, nres);
        }
    }
}

void mus_audio_save (void) 
{
  int rv, i;
  ALvalue x[16];
  if (saved_devices > 0)
    for (i = 0; i < saved_devices; i++)
      if (si[i])
	{
	  FREE(si[i]);
	  si[i] = NULL;
	}
  saved_devices = 0;
  rv= alQueryValues(AL_SYSTEM, AL_DEVICES, x, 16, 0, 0);
  if (rv > 0) save_devices(x, rv);
}

void mus_audio_restore (void) 
{
  int i;
  ALpv x;
  if (saved_devices > 0)
    for (i = 0; i < saved_devices; i++)
      if (si[i])
	{
	  if (si[i]->has_gains)
	    {
	      x.param = AL_GAIN;
	      x.value.ptr = si[i]->gains;
	      x.sizeIn = MAX_CHANNELS;
	      alSetParams(si[i]->dev, &x, 1);
	    }
	  x.param = AL_RATE;
	  x.value.i = si[i]->srate;
	  alSetParams(si[i]->dev, &x, 1);
        }
}

#else

/* old audio library */

#define MAX_VOLUME 255

static int decode_field(int dev, int field, int chan)
{
  switch (dev)
    {
    case MUS_AUDIO_DEFAULT: 
    case MUS_AUDIO_DAC_OUT:
    case MUS_AUDIO_DUPLEX_DEFAULT:
    case MUS_AUDIO_SPEAKERS:
      switch (field)
        {
        case MUS_AUDIO_AMP:
          return((chan == 0) ? AL_LEFT_SPEAKER_GAIN : AL_RIGHT_SPEAKER_GAIN);
          break;
        case MUS_AUDIO_SRATE:
          return(AL_OUTPUT_RATE);
          break;
        }
      break;
    case MUS_AUDIO_LINE_OUT:
      switch (field)
        {
        case MUS_AUDIO_SRATE:
          return(AL_OUTPUT_RATE); /* ? */
          break;
        }
      break;
    case MUS_AUDIO_DIGITAL_OUT:
      if (field == MUS_AUDIO_SRATE)
        return(AL_OUTPUT_RATE);
      break;
    case MUS_AUDIO_DIGITAL_IN:
      if (field == MUS_AUDIO_SRATE)
        return(AL_INPUT_RATE);
      break;
    case MUS_AUDIO_LINE_IN:
      if (field == MUS_AUDIO_AMP)
        return((chan == 0) ? AL_LEFT_INPUT_ATTEN : AL_RIGHT_INPUT_ATTEN);
      else
	if (field == MUS_AUDIO_SRATE)
	  return(AL_INPUT_RATE);
      break;
    case MUS_AUDIO_MICROPHONE:
      if (field == MUS_AUDIO_AMP)
        return((chan == 0) ? AL_LEFT2_INPUT_ATTEN : AL_RIGHT2_INPUT_ATTEN);
      else
	if (field == MUS_AUDIO_SRATE)
	  return(AL_INPUT_RATE);
      break;
    }
  return(MUS_ERROR);
}

int mus_audio_mixer_read(int ur_dev, int field, int chan, float *val)
{
  long pb[4];
  long fld;
  int dev, err = MUS_NO_ERROR;
  start_sgi_print();
  dev = MUS_AUDIO_DEVICE(ur_dev);
  switch (field)
    {
    case MUS_AUDIO_CHANNEL:
      val[0] = 4;
      break;
    case MUS_AUDIO_FORMAT:  
      val[0] = 1; 
      if (chan > 1) val[1] = MUS_BSHORT; 
      break;
    case MUS_AUDIO_PORT:
                            /* how to tell which machine we're on? */
      val[0] = 4; 
      if (chan > 1) val[1] = MUS_AUDIO_LINE_IN; 
      if (chan > 2) val[2] = MUS_AUDIO_MICROPHONE; 
      if (chan > 3) val[3] = MUS_AUDIO_DIGITAL_IN; 
      if (chan > 4) val[4] = MUS_AUDIO_DAC_OUT;
      /* does this order work for digital input as well? (i.e. does it replace the microphone)? */
      break;
    case MUS_AUDIO_AMP:
      fld = decode_field(dev, field, chan);
      if (fld != MUS_ERROR)
        {
          pb[0] = fld;
          if (ALgetparams(AL_DEFAULT_DEVICE, pb, 2)) 
	    RETURN_ERROR_EXIT(MUS_AUDIO_READ_ERROR, -1,
			      mus_format("can't read gain settings of %s",
					 mus_audio_device_name(dev)));
          if ((fld == AL_LEFT_SPEAKER_GAIN) || 
	      (fld == AL_RIGHT_SPEAKER_GAIN))
            val[0] = ((float)pb[1]) / ((float)MAX_VOLUME);
          else val[0] = 1.0 - ((float)pb[1]) / ((float)MAX_VOLUME);
        }
      else err = MUS_ERROR;
      break;
    case MUS_AUDIO_SRATE:
      fld = decode_field(dev, field, chan);
      if (fld != MUS_ERROR)
        {
          pb[0] = fld;
          if (ALgetparams(AL_DEFAULT_DEVICE, pb, 2)) 
	    RETURN_ERROR_EXIT(MUS_AUDIO_READ_ERROR, -1,
			      mus_format("can't read srate setting of %s",
					 mus_audio_device_name(dev)));
          val[0] = pb[1];
        }
      else err = MUS_ERROR;
      break;
    default: 
      err = MUS_ERROR;
      break;
    }
  if (err == MUS_ERROR)
    RETURN_ERROR_EXIT(MUS_AUDIO_CANT_READ, -1,
		      mus_format("can't read %s setting of %s",
				 mus_audio_device_name(field),
				 mus_audio_device_name(dev)));
  end_sgi_print();
  return(MUS_NO_ERROR);
}

int mus_audio_mixer_write(int ur_dev, int field, int chan, float *val)
{
  long pb[4];
  long fld;
  int dev, err = MUS_NO_ERROR;
  start_sgi_print();
  dev = MUS_AUDIO_DEVICE(ur_dev);
  switch (field)
    {
    case MUS_AUDIO_PORT:
      if (dev == MUS_AUDIO_DEFAULT)
        {
	  pb[0] = AL_CHANNEL_MODE;
	  pb[1] = ((chan == MUS_AUDIO_DIGITAL_IN) ? AL_STEREO : AL_4CHANNEL);
          pb[2] = AL_INPUT_SOURCE;
          pb[3] = ((chan == MUS_AUDIO_DIGITAL_IN) ? AL_INPUT_DIGITAL : AL_INPUT_MIC);
          if (ALsetparams(AL_DEFAULT_DEVICE, pb, 4)) 
	    RETURN_ERROR_EXIT(MUS_AUDIO_WRITE_ERROR, -1,
			      mus_format("can't set mode and source of %s",
					 mus_audio_device_name(dev)));
        }
      else err = MUS_ERROR;
      break;
    case MUS_AUDIO_CHANNEL:
      if (dev == MUS_AUDIO_MICROPHONE)
        {
          pb[0] =  AL_MIC_MODE;
          pb[1] = ((chan == 2) ? AL_STEREO : AL_MONO);
          if (ALsetparams(AL_DEFAULT_DEVICE, pb, 2)) 
	    RETURN_ERROR_EXIT(MUS_AUDIO_WRITE_ERROR, -1,
			      mus_format("can't set microphone to be %s",
					 (chan == 2) ? "stereo" : "mono"));
        }
      else
        {
          if (dev == MUS_AUDIO_DEFAULT)
            {
              pb[0] = AL_CHANNEL_MODE;
              pb[1] = ((chan == 4) ? AL_4CHANNEL : AL_STEREO);
              if (ALsetparams(AL_DEFAULT_DEVICE, pb, 2)) 
		RETURN_ERROR_EXIT(MUS_AUDIO_WRITE_ERROR, -1,
				  mus_format("can't set default device to be %s",
					     (chan == 4) ? "quad" : "stereo"));
            }
	  else err = MUS_ERROR;
        }
      break;
    case MUS_AUDIO_AMP:
      fld = decode_field(dev, field, chan);
      if (fld != -1)
        {
          pb[0] = fld;
          if ((fld == AL_LEFT_SPEAKER_GAIN) || 
	      (fld == AL_RIGHT_SPEAKER_GAIN))
            pb[1] = val[0] * MAX_VOLUME;
          else pb[1] =  (1.0 - val[0]) * MAX_VOLUME;
          if (ALsetparams(AL_DEFAULT_DEVICE, pb, 2)) 
	    RETURN_ERROR_EXIT(MUS_AUDIO_WRITE_ERROR, -1,
			      mus_format("can't set gain of %s",
					 mus_audio_device_name(dev)));
        }
      else err = MUS_ERROR;
      break;
    case MUS_AUDIO_SRATE:
      fld = decode_field(dev, field, chan);
      if (fld != -1)
        {
          pb[0] = fld;
          pb[1] = val[0];
          if (ALsetparams(AL_DEFAULT_DEVICE, pb, 2)) 
	    RETURN_ERROR_EXIT(MUS_AUDIO_WRITE_ERROR, -1, NULL);
	  if (fld == AL_INPUT_RATE)
	    {
	      pb[0] = AL_OUTPUT_RATE;
	      pb[1] = val[0];
	      if (ALsetparams(AL_DEFAULT_DEVICE, pb, 2)) 
		RETURN_ERROR_EXIT(MUS_AUDIO_WRITE_ERROR, -1,
				  mus_format("can't set srate of %s",
					     mus_audio_device_name(dev)));
            }
        }
      else err = MUS_ERROR;
      break;
    default: 
      err = MUS_ERROR;
      break;
    }
    if (err == MUS_ERROR)
      RETURN_ERROR_EXIT(MUS_AUDIO_CANT_WRITE, -1,
			mus_format("can't write %s setting of %s",
				   mus_audio_device_name(field),
				   mus_audio_device_name(dev)));
  end_sgi_print();
  return(MUS_NO_ERROR);
}

static void describe_audio_state_1(void) 
{
  float amps[1];
  int err;
  err = mus_audio_mixer_read(MUS_AUDIO_SPEAKERS, MUS_AUDIO_SRATE, 0, amps);
  if (err == MUS_NO_ERROR) 
    {mus_snprintf(audio_strbuf, PRINT_BUFFER_SIZE, "srate: %.2f\n", amps[0]); pprint(audio_strbuf);} 
  else {fprintf(stdout, "err: %d!\n", err); fflush(stdout);}
  err = mus_audio_mixer_read(MUS_AUDIO_SPEAKERS, MUS_AUDIO_AMP, 0, amps);
  if (err == MUS_NO_ERROR) {mus_snprintf(audio_strbuf, PRINT_BUFFER_SIZE, "speakers: %.2f", amps[0]); pprint(audio_strbuf);}
  err = mus_audio_mixer_read(MUS_AUDIO_SPEAKERS, MUS_AUDIO_AMP, 1, amps);
  if (err == MUS_NO_ERROR) {mus_snprintf(audio_strbuf, PRINT_BUFFER_SIZE, " %.2f\n", amps[0]); pprint(audio_strbuf);}
  err = mus_audio_mixer_read(MUS_AUDIO_LINE_IN, MUS_AUDIO_AMP, 0, amps);
  if (err == MUS_NO_ERROR) {mus_snprintf(audio_strbuf, PRINT_BUFFER_SIZE, "line in: %.2f", amps[0]); pprint(audio_strbuf);}
  err = mus_audio_mixer_read(MUS_AUDIO_LINE_IN, MUS_AUDIO_AMP, 1, amps);
  if (err == MUS_NO_ERROR) {mus_snprintf(audio_strbuf, PRINT_BUFFER_SIZE, " %.2f\n", amps[0]); pprint(audio_strbuf);}
  err = mus_audio_mixer_read(MUS_AUDIO_MICROPHONE, MUS_AUDIO_AMP, 0, amps);
  if (err == MUS_NO_ERROR) {mus_snprintf(audio_strbuf, PRINT_BUFFER_SIZE, "microphone: %.2f", amps[0]); pprint(audio_strbuf);}
  err = mus_audio_mixer_read(MUS_AUDIO_MICROPHONE, MUS_AUDIO_AMP, 1, amps);
  if (err == MUS_NO_ERROR) {mus_snprintf(audio_strbuf, PRINT_BUFFER_SIZE, " %.2f\n", amps[0]); pprint(audio_strbuf);}
  err = mus_audio_mixer_read(MUS_AUDIO_LINE_OUT, MUS_AUDIO_AMP, 0, amps);
  if (err == MUS_NO_ERROR) {mus_snprintf(audio_strbuf, PRINT_BUFFER_SIZE, "line out: %.2f", amps[0]); pprint(audio_strbuf);}
  err = mus_audio_mixer_read(MUS_AUDIO_LINE_OUT, MUS_AUDIO_AMP, 1, amps);
  if (err == MUS_NO_ERROR) {mus_snprintf(audio_strbuf, PRINT_BUFFER_SIZE, " %.2f\n", amps[0]); pprint(audio_strbuf);}
  err = mus_audio_mixer_read(MUS_AUDIO_DIGITAL_OUT, MUS_AUDIO_AMP, 0, amps);
  if (err == MUS_NO_ERROR) {mus_snprintf(audio_strbuf, PRINT_BUFFER_SIZE, "digital out: %.2f", amps[0]); pprint(audio_strbuf);}
  err = mus_audio_mixer_read(MUS_AUDIO_DIGITAL_OUT, MUS_AUDIO_AMP, 1, amps);
  if (err == MUS_NO_ERROR) {mus_snprintf(audio_strbuf, PRINT_BUFFER_SIZE, " %.2f\n", amps[0]); pprint(audio_strbuf);}
}

static long *init_state = NULL;

void mus_audio_save (void)
{
  /* save channel mode, input source, left/right atten (2), speaker gain, input/output rate, mic_mode */
  start_sgi_print();
  init_state = (long *)CALLOC(22, sizeof(long));
  init_state[0] = AL_CHANNEL_MODE;
  init_state[2] = AL_INPUT_SOURCE;
  init_state[4] = AL_LEFT_INPUT_ATTEN;
  init_state[6] = AL_RIGHT_INPUT_ATTEN;
  init_state[8] = AL_LEFT2_INPUT_ATTEN;
  init_state[10] = AL_RIGHT2_INPUT_ATTEN;
  init_state[12] = AL_MIC_MODE;
  init_state[14] = AL_LEFT_SPEAKER_GAIN;
  init_state[16] = AL_RIGHT_SPEAKER_GAIN;
  init_state[18] = AL_INPUT_RATE;
  init_state[20] = AL_OUTPUT_RATE;
  if (ALgetparams(AL_DEFAULT_DEVICE, init_state, 22)) MUS_STANDARD_ERROR(MUS_AUDIO_READ_ERROR, NULL);
  end_sgi_print();
}

void mus_audio_restore (void)
{
  start_sgi_print();
  if ((init_state) &&
      (ALsetparams(AL_DEFAULT_DEVICE, init_state, 22)))
    MUS_STANDARD_ERROR(MUS_AUDIO_WRITE_ERROR, NULL);
  end_sgi_print();
}

 
#endif
/* new or old AL */

#endif
/* SGI */



/* ------------------------------- OSS ----------------------------------------- */

#if (HAVE_OSS || HAVE_ALSA)
/* actually it's not impossible that someday we'll have ALSA but not OSS... */
#define AUDIO_OK

#include <sys/ioctl.h>

/* the system version of the soundcard header file may have no relation to the current OSS actually loaded */
/* sys/soundcard.h is usually just a pointer to linux/soundcard.h */

#if (USR_LIB_OSS)
  #include "/usr/lib/oss/include/sys/soundcard.h"
#else
  #if (USR_LOCAL_LIB_OSS)
    #include "/usr/local/lib/oss/include/sys/soundcard.h"
  #else
    #if (OPT_OSS)
      #include "/opt/oss/include/sys/soundcard.h"
    #else
      #if (VAR_LIB_OSS)
        #include "/var/lib/oss/include/sys/soundcard.h"
      #else
        #if defined(HAVE_SYS_SOUNDCARD_H) || defined(LINUX) || defined(UW2)
          #include <sys/soundcard.h>
        #else
          #if defined(HAVE_MACHINE_SOUNDCARD_H)
            #include <machine/soundcard.h>
          #else
            #include <soundcard.h>
          #endif
        #endif
      #endif
    #endif
  #endif
#endif

#if ((SOUND_VERSION > 360) && (defined(OSS_SYSINFO)))
  #define NEW_OSS 1
#endif

#define MAX_VOLUME 100

#define DAC_NAME "/dev/dsp"
#define MIXER_NAME "/dev/mixer"
#define SYNTH_NAME "/dev/music"
/* some programs use /dev/audio */

/* there can be more than one sound card installed, and a card can be handled through
 * more than one /dev/dsp device, so we can't use a global dac device here.
 * The caller has to keep track of the various cards (via AUDIO_SYSTEM) --
 * I toyed with embedding all that in mus_audio_open_output and mus_audio_write, but
 * decided it's better to keep them explicit -- the caller may want entirely
 * different (non-synchronous) streams going to the various cards.  This same
 * code (AUDIO_SYSTEM(n)->devn) should work in Windoze (see below), and
 * might work on the Mac and SGI -- something for a rainy day...
 */

#define RETURN_ERROR_EXIT(Message_Type, Audio_Line, Ur_Message) \
  do { \
       char *Message; Message = Ur_Message; \
       if (Audio_Line != -1) \
          linux_audio_close(Audio_Line); \
       if ((Message) && (strlen(Message) > 0)) \
         { \
           mus_print("%s\n  [%s[%d] %s]", \
                     Message, \
                     __FILE__, __LINE__, __FUNCTION__); \
           FREE(Message); \
         } \
       else mus_print("%s\n  [%s[%d] %s]", \
                      mus_error_to_string(Message_Type), \
                      __FILE__, __LINE__, __FUNCTION__); \
       return(MUS_ERROR); \
     } while (0)

static int FRAGMENTS = 4;
static int FRAGMENT_SIZE = 12;
static int fragments_locked = 0;

/* defaults here are FRAGMENTS 16 and FRAGMENT_SIZE 12; these values however
 * cause about a .5 second delay, which is not acceptable in "real-time" situations.
 *
 * this changed 22-May-01: these are causing more trouble than they're worth
 */

static void oss_mus_audio_set_oss_buffers(int num, int size) {FRAGMENTS = num; FRAGMENT_SIZE = size; fragments_locked = 1;}
int mus_audio_oss_buffer_size(void);
int mus_audio_oss_buffer_size(void) 
{
  if (fragments_locked)
    return(FRAGMENTS * (1 << FRAGMENT_SIZE));
  else return(16 * 4096); 
}

#define MAX_SOUNDCARDS 8
#define MAX_DSPS 8
#define MAX_MIXERS 8
/* there can be (apparently) any number of mixers and dsps per soundcard, but 8 is enough! */

static int *audio_fd = NULL; 
static int *audio_open_ctr = NULL; 
static int *audio_dsp = NULL; 
static int *audio_mixer = NULL; 
static int *audio_type = NULL; 
static int *audio_mode = NULL; 
enum {NORMAL_CARD, SONORUS_STUDIO, RME_HAMMERFALL, SAM9407_DSP, DELTA_66};
/* the Sonorus Studi/o card is a special case in all regards */

static int sound_cards = 0;
static int new_oss_running = 0;
static char *dev_name = NULL;

static int oss_mus_audio_systems(void) 
{
  return(sound_cards);
}

static char *mixer_name(int sys)
{
#if HAVE_SAM_9407
  if(sys < sound_cards && audio_type[sys] == SAM9407_DSP)
    {
      mus_snprintf(dev_name, LABEL_BUFFER_SIZE, "/dev/sam%d_mixer", audio_mixer[sys]);
      return(dev_name);
    }
#endif 
  if (sys < sound_cards)
    {
      if (audio_mixer[sys] == -2)
	return(MIXER_NAME); 
      /* if we have /dev/dsp (not /dev/dsp0), I assume the corresponding mixer is /dev/mixer (not /dev/mixer0) */
      /* but in sam9407 driver, there is no /dev/mixer, and everything goes through /dev/dsp */
      else
	{
	  if (audio_mixer[sys] == -3)
	    return(DAC_NAME); 
	  else 
	    {
	      mus_snprintf(dev_name, LABEL_BUFFER_SIZE, "%s%d", MIXER_NAME, audio_mixer[sys]);
	      return(dev_name);
	    }
	}
    }
  return(DAC_NAME);
}

static char *oss_mus_audio_system_name(int system) 
{
#if HAVE_SAM_9407
  if(system < sound_cards && audio_type[system] == SAM9407_DSP)
    {
      int fd;
      fd = open(mixer_name(system), O_RDONLY, 0);
      if(fd != -1) 
	{
	  static SamDriverInfo driverInfo;
	  if(ioctl(fd, SAM_IOC_DRIVER_INFO, &driverInfo) >= 0) 
	    {
	      close(fd);
	      return(driverInfo.hardware);
	    }
	  close(fd);
	}
      return("sam9407");
    }
#endif
#ifdef NEW_OSS
  static mixer_info mixinfo;
  int status, ignored, fd;
  fd = open(mixer_name(system), O_RDONLY, 0);
  if (fd != -1)
    {
      status = ioctl(fd, OSS_GETVERSION, &ignored);
      if (status == 0)
	{
	  status = ioctl(fd, SOUND_MIXER_INFO, &mixinfo);
	  if (status == 0) 
	    {
	      close(fd);
	      return(mixinfo.name);
	    }
	}
      close(fd);
    }
#endif
  return("OSS");
}

#if HAVE_SAM_9407
static char *oss_mus_audio_moniker(void) {return("Sam 9407");}
#else
static char *oss_mus_audio_moniker(void)
{
  char version[LABEL_BUFFER_SIZE];
  if (version_name == NULL) version_name = (char *)CALLOC(LABEL_BUFFER_SIZE, sizeof(char));
  if (SOUND_VERSION < 361)
    {
      mus_snprintf(version, LABEL_BUFFER_SIZE, "%d", SOUND_VERSION);
      mus_snprintf(version_name, LABEL_BUFFER_SIZE, "OSS %c.%c.%c", version[0], version[1], version[2]);
    }
  else
    mus_snprintf(version_name, LABEL_BUFFER_SIZE, "OSS %x.%x.%x", 
		 (SOUND_VERSION >> 16) & 0xff, 
		 (SOUND_VERSION >> 8) & 0xff, 
		 SOUND_VERSION & 0xff);
  return(version_name);
}
#endif

static char *dac_name(int sys, int offset)
{
#if HAVE_SAM_9407
  if (sys < sound_cards && audio_type[sys] == SAM9407_DSP) 
    {
      mus_snprintf(dev_name, LABEL_BUFFER_SIZE, "/dev/sam%d_dsp", audio_dsp[sys]);
      return(dev_name);
    }
#endif
  if ((sys < sound_cards) && (audio_mixer[sys] >= -1))
    {
      mus_snprintf(dev_name, LABEL_BUFFER_SIZE, "%s%d", DAC_NAME, audio_dsp[sys] + offset);
      return(dev_name);
    }
  return(DAC_NAME);
}

#define MIXER_SIZE SOUND_MIXER_NRDEVICES
static int **mixer_state = NULL;
static int *init_srate = NULL, *init_chans = NULL, *init_format = NULL;

void set_dsp_reset(int val);
static int dsp_reset = 0;               /* trying to find out if DSP_RESET is ever needed */
void set_dsp_reset(int val) {dsp_reset = val;}

static int oss_mus_audio_initialize(void) 
{
  /* here we need to set up the map of /dev/dsp and /dev/mixer to a given system */
  /* since this info is not passed to us by OSS, we have to work at it... */
  /* for the time being, I'll ignore auxiliary dsp and mixer ports (each is a special case) */
  int i, fd = -1, md, err = 0;
  char dname[LABEL_BUFFER_SIZE];
  int amp, old_mixer_amp, old_dsp_amp, new_mixer_amp, responsive_field;
  int devmask;
#ifdef NEW_OSS
  int status, ignored;
  oss_sysinfo sysinfo;
  static mixer_info mixinfo;
  int sysinfo_ok = 0;
#endif
  int num_mixers, num_dsps, nmix, ndsp;
  if (!audio_initialized)
    {
      audio_initialized = TRUE;
      audio_fd = (int *)CALLOC(MAX_SOUNDCARDS, sizeof(int));
      audio_open_ctr = (int *)CALLOC(MAX_SOUNDCARDS, sizeof(int));
      audio_dsp = (int *)CALLOC(MAX_SOUNDCARDS, sizeof(int));
      audio_mixer = (int *)CALLOC(MAX_SOUNDCARDS, sizeof(int));
      audio_type = (int *)CALLOC(MAX_SOUNDCARDS, sizeof(int));
      audio_mode = (int *)CALLOC(MAX_SOUNDCARDS, sizeof(int));
      dev_name = (char *)CALLOC(LABEL_BUFFER_SIZE, sizeof(char));
      init_srate = (int *)CALLOC(MAX_SOUNDCARDS, sizeof(int));
      init_chans = (int *)CALLOC(MAX_SOUNDCARDS, sizeof(int));
      init_format = (int *)CALLOC(MAX_SOUNDCARDS, sizeof(int));
      mixer_state = (int **)CALLOC(MAX_SOUNDCARDS, sizeof(int *));
      for (i = 0; i < MAX_SOUNDCARDS; i++) mixer_state[i] = (int *)CALLOC(MIXER_SIZE, sizeof(int));
      for (i = 0; i < MAX_SOUNDCARDS; i++)
	{
	  audio_fd[i] = -1;
	  audio_open_ctr[i] = 0;
	  audio_dsp[i] = -1;
	  audio_mixer[i] = -1;
	  audio_type[i] = NORMAL_CARD;
	}
#if HAVE_SAM_9407
      {
	SamApiInfo apiInfo;
	SamDriverInfo driverInfo;
	for (i = 0; i < MAX_SOUNDCARDS; i++) 
	  {
	    mus_snprintf(dname, LABEL_BUFFER_SIZE, "/dev/sam%d_mixer", i);
	    fd = open(dname, O_WRONLY);
	    if (fd < 0)
	      break;
	    if ((ioctl(fd, SAM_IOC_API_INFO, &apiInfo) < 0) ||
		(apiInfo.apiClass!=SAM_API_CLASS_VANILLA) ||
		(ioctl(fd, SAM_IOC_DRIVER_INFO, &driverInfo) < 0) ||
		(!driverInfo.haveAudio))
	      {
		close(fd);
		continue;
	      }
	    audio_type[sound_cards] = SAM9407_DSP;
	    audio_dsp[sound_cards] = i;
	    audio_mixer[sound_cards] = i;
	    sound_cards++;
	    close(fd);
	  }
	if(sound_cards > 0)
	  return(0);
      }
#endif

      num_mixers = MAX_MIXERS;
      num_dsps = MAX_DSPS;
#ifdef NEW_OSS
      fd = open(DAC_NAME, O_WRONLY | O_NONBLOCK, 0);
      if (fd == -1) fd = open(SYNTH_NAME, O_RDONLY | O_NONBLOCK, 0);
      if (fd == -1) fd = open(MIXER_NAME, O_RDONLY | O_NONBLOCK, 0);
      if (fd != -1)
	{
	  status = ioctl(fd, OSS_GETVERSION, &ignored);
	  new_oss_running = (status == 0);
	  if (new_oss_running)
	    {
	      status = ioctl(fd, OSS_SYSINFO, &sysinfo);
	      sysinfo_ok = (status == 0);
	    }
	  if ((new_oss_running) && (sysinfo_ok))
	    {
	      num_mixers = sysinfo.nummixers;
	      num_dsps = sysinfo.numaudios;
	    }
	  close(fd);
	}
#endif

      /* need to get which /dev/dsp lines match which /dev/mixer lines,
       *   find out how many separate systems (soundcards) are available,
       *   fill the audio_dsp and audio_mixer arrays with the system-related numbers,
       * since we have no way to tell from OSS info which mixers/dsps are the
       *   main ones, we'll do some messing aound to try to deduce this info.
       * for example, SB uses two dsp ports and two mixers per card, whereas
       *  Ensoniq uses 2 dsps and 1 mixer.
       * 
       * the data we are gathering here:
       *   int audio_dsp[MAX_SOUNDCARDS] -> main_dsp_port[MUS_AUDIO_PACK_SYSTEM(n)] (-1 => no such system dsp)
       *   int audio_mixer[MAX_SOUNDCARDS] -> main_mixer_port[MUS_AUDIO_PACK_SYSTEM(n)]
       *   int sound_cards = 0 -> usable systems
       * all auxiliary ports are currently ignored (SB equalizer, etc)
       */
      sound_cards = 0;
      ndsp = 0;
      nmix = 0;
      while ((nmix < num_mixers) && 
	     (ndsp < num_dsps))
	{
	  /* for each mixer, find associated main dsp (assumed to be first in /dev/dsp ordering) */
	  /*   if mixer's dsp overlaps or we run out of dsps first, ignore it (aux mixer) */
	  /* our by-guess-or-by-gosh method here is to try to open the mixer.
	   *   if that fails, quit (if very first, try at least to get the dsp setup)
	   *   find volume field, if none, go on, else read current volume
	   *   open next unchecked dsp, try to set volume, read current, if different we found a match -- set and go on.
	   *     if no change, move to next dsp and try again, if no more dsps, quit (checking for null case as before)
	   */
	  mus_snprintf(dname, LABEL_BUFFER_SIZE, "%s%d", MIXER_NAME, nmix);
	  md = open(dname, O_RDWR, 0);
	  if (md == -1)
	    {
	      if (errno == EBUSY) 
		{
		  mus_print("%s is busy: can't access it [%s[%d] %s]", 
			    dname,
			    __FILE__, __LINE__, __FUNCTION__); 
		  nmix++;
		  continue;
		}
	      else break;
	    }
	  mus_snprintf(dname, LABEL_BUFFER_SIZE, "%s%d", DAC_NAME, ndsp);
	  fd = open(dname, O_RDWR | O_NONBLOCK, 0);
	  if (fd == -1) fd = open(dname, O_RDONLY | O_NONBLOCK, 0);
 	  if (fd == -1) fd = open(dname, O_WRONLY | O_NONBLOCK, 0); /* some output devices need this */
	  if (fd == -1)
	    {
	      close(md); 
	      if (errno == EBUSY) /* in linux /usr/include/asm/errno.h */
		{
		  fprintf(stderr, "%s is busy: can't access it\n", dname); 
		  ndsp++;
		  continue;
		}
	      else 
		{
		  if ((errno != ENXIO) && (errno != ENODEV))
		    fprintf(stderr, "%s: %s! ", dname, strerror(errno));
		  break;
		}
	    }
#ifdef NEW_OSS				  
	  /* can't change volume yet of Sonorus, so the method above won't work --
	   * try to catch this case via the mixer's name
	   */
	  status = ioctl(md, SOUND_MIXER_INFO, &mixinfo);
	  if ((status == 0) && 
	      (mixinfo.name) && 
	      (*(mixinfo.name)) && 
	      (strlen(mixinfo.name) > 6))
	    {
	      if (strncmp("STUDI/O", mixinfo.name, 7) == 0)
		{
		  /* a special case in every regard */
		  audio_type[sound_cards] = SONORUS_STUDIO;
		  audio_mixer[sound_cards] = nmix; 
		  nmix++;
		  audio_dsp[sound_cards] = ndsp; 
		  if (num_dsps >= 21)
		    {
		      ndsp += 21;
		      audio_mode[sound_cards] = 1;
		    }
		  else
		    {
		      ndsp += 9;
		      audio_mode[sound_cards] = 0;
		    }
		  sound_cards++;
		  close(fd);
		  close(md);
		  continue;
		}
	      else
		{
		  if (strncmp("RME Digi96", mixinfo.name, 10) == 0)
		    {
		      audio_type[sound_cards] = RME_HAMMERFALL;
		      audio_mixer[sound_cards] = nmix; 
		      nmix++;
		      audio_dsp[sound_cards] = ndsp; 
		      sound_cards++;
		      close(fd);
		      close(md);
		      continue;
		    }
		  else
		    {
		      if (strncmp("M Audio Delta", mixinfo.name, 13) == 0)
			{
			  audio_type[sound_cards] = DELTA_66;
			  audio_mixer[sound_cards] = nmix; 
			  nmix++;
			  ndsp += 6; /* just a guess */
			  audio_dsp[sound_cards] = ndsp; 
			  sound_cards++;
			  close(fd);
			  close(md);
			  continue;
			}
		    }
		}
	    }
#endif
	  err = ioctl(md, SOUND_MIXER_READ_DEVMASK, &devmask);
	  responsive_field = SOUND_MIXER_VOLUME;
	  for (i = 0; i < SOUND_MIXER_NRDEVICES; i++)
	    if ((1 << i) & devmask)
	      {
		responsive_field = i;
		break;
	      }
	  if (!err)
	    {
	      err = ioctl(md, MIXER_READ(responsive_field), &old_mixer_amp);
	      if (!err)
		{
		  err = ioctl(fd, MIXER_READ(responsive_field), &old_dsp_amp);
		  if ((!err) && (old_dsp_amp == old_mixer_amp))
		    {
		      if (old_mixer_amp == 0) amp = 50; else amp = 0; /* 0..100 */
		      err = ioctl(fd, MIXER_WRITE(responsive_field), &amp);
		      if (!err)
			{
			  err = ioctl(md, MIXER_READ(responsive_field), &new_mixer_amp);
			  if (!err)
			    {
			      if (new_mixer_amp == amp)
				{
				  /* found one! */
				  audio_dsp[sound_cards] = ndsp; ndsp++;
				  audio_mixer[sound_cards] = nmix; nmix++;
				    audio_type[sound_cards] = NORMAL_CARD;
				  sound_cards++;
				}
			      else ndsp++;
			      err = ioctl(fd, MIXER_WRITE(responsive_field), &old_dsp_amp);
			    }
			  else nmix++;
			}
		      else ndsp++;
		    }
		  else ndsp++;
		}
	      else nmix++;
	    }
	  else nmix++;
	  close(fd);
	  close(md);
	}
      if (sound_cards == 0)
	{
 	  fd = open(DAC_NAME, O_WRONLY | O_NONBLOCK, 0);
	  if (fd != -1)
	    {
	      sound_cards = 1;
	      audio_dsp[0] = 0;
	      audio_type[0] = NORMAL_CARD;
	      audio_mixer[0] = -2; /* hmmm -- need a way to see /dev/dsp as lonely outpost */
	      close(fd);
 	      fd = open(MIXER_NAME, O_RDONLY | O_NONBLOCK, 0);
	      if (fd == -1)
		audio_mixer[0] = -3;
	      else close(fd);
	    }
	}
    }
  return(MUS_NO_ERROR);
}

int mus_audio_reinitialize(void)
{
  /* an experiment */
  audio_initialized = FALSE;
  return(mus_audio_initialize());
}

static int linux_audio_open(const char *pathname, int flags, mode_t mode, int system)
{
  /* sometimes this is simply searching for a device (so failure is not a mus_error) */
  if (audio_fd[system] == -1) 
    {
      audio_fd[system] = open(pathname, flags, mode);
      audio_open_ctr[system] = 0;
    }
  else audio_open_ctr[system]++;
  return(audio_fd[system]);
}

static int linux_audio_open_with_error(const char *pathname, int flags, mode_t mode, int system)
{
  int fd;
  fd = linux_audio_open(pathname, flags, mode, system);
  if (fd == -1)
    MUS_STANDARD_IO_ERROR(MUS_AUDIO_CANT_OPEN,
			  ((mode == O_RDONLY) ? "open read" : 
			   (mode == O_WRONLY) ? "open write" : "open read/write"),
			  pathname);
  return(fd);
}

static int find_system(int line)
{
  int i;
  for (i = 0; i < sound_cards; i++)
    if (line == audio_fd[i])
      return(i);
  return(MUS_ERROR);
}

static int linux_audio_close(int fd)
{
  int err = 0, sys;
  if (fd != -1)
    {
      sys = find_system(fd);
      if (sys != -1)
	{
	  if (audio_open_ctr[sys] > 0) 
	    audio_open_ctr[sys]--;
	  else 
	    {
	      err = close(fd);
	      audio_open_ctr[sys] = 0;
	      audio_fd[sys] = -1;
	    }
	}
      else err = close(fd);
      if (err) RETURN_ERROR_EXIT(MUS_AUDIO_CANT_CLOSE, -1,
				 mus_format("close %d failed: %s",
					    fd, strerror(errno)));
    }
  /* is this an error? */
  return(MUS_NO_ERROR);
}

static int to_oss_format(int snd_format)
{
  switch (snd_format)
    {
    case MUS_BYTE:    return(AFMT_S8);     break;
    case MUS_BSHORT:  return(AFMT_S16_BE); break;
    case MUS_UBYTE:   return(AFMT_U8);     break;
    case MUS_MULAW:   return(AFMT_MU_LAW); break;
    case MUS_ALAW:    return(AFMT_A_LAW);  break;
    case MUS_LSHORT:  return(AFMT_S16_LE); break;
    case MUS_UBSHORT: return(AFMT_U16_BE); break;
    case MUS_ULSHORT: return(AFMT_U16_LE); break;
#ifdef NEW_OSS
    case MUS_LINT:    return(AFMT_S32_LE); break;
    case MUS_BINT:    return(AFMT_S32_BE); break;
#endif
    }
  return(MUS_ERROR);
}

static char sonorus_buf[LABEL_BUFFER_SIZE];
static char *sonorus_name(int sys, int offset)
{
  mus_snprintf(sonorus_buf, LABEL_BUFFER_SIZE, "/dev/dsp%d", offset + audio_dsp[sys]);
  return(sonorus_buf);
}

static int oss_mus_audio_open_output(int ur_dev, int srate, int chans, int format, int size)
{
  /* ur_dev is in general MUS_AUDIO_PACK_SYSTEM(n) | MUS_AUDIO_DEVICE */
  int oss_format, buffer_info, audio_out = -1, sys, dev;
  char *dev_name;
#ifndef NEW_OSS
  int stereo;
#endif
  sys = MUS_AUDIO_SYSTEM(ur_dev);
  dev = MUS_AUDIO_DEVICE(ur_dev);
  oss_format = to_oss_format(format); 
  if (oss_format == MUS_ERROR) 
    RETURN_ERROR_EXIT(MUS_AUDIO_FORMAT_NOT_AVAILABLE, -1,
		      mus_format("format %d (%s) not available",
				 format, 
				 mus_data_format_name(format)));
  if (audio_type[sys] == SONORUS_STUDIO)
    {
      /* in this case the output devices are parcelled out to the /dev/dsp locs */
      /* dev/dsp0 is always stereo */
      switch (dev)
	{
	case MUS_AUDIO_DEFAULT: 
	  if (chans > 2) 
	    audio_out = open(sonorus_name(sys, 1), O_WRONLY, 0);
	  else audio_out = open(sonorus_name(sys, 0), O_WRONLY, 0);
	  /* probably should write to both outputs */
	  if (audio_out == -1) audio_out = open("/dev/dsp", O_WRONLY, 0);
	  break;
	case MUS_AUDIO_SPEAKERS:
	  audio_out = open(sonorus_name(sys, 0), O_WRONLY, 0);
	  if (audio_out == -1) audio_out = open("/dev/dsp", O_WRONLY, 0);
	  break;
	case MUS_AUDIO_ADAT_OUT: case MUS_AUDIO_SPDIF_OUT:
	  audio_out = open(sonorus_name(sys, 1), O_WRONLY, 0);
	  break;
	case MUS_AUDIO_AES_OUT:
	  audio_out = open(sonorus_name(sys, 9), O_WRONLY, 0);
	  break;
	default:
	  RETURN_ERROR_EXIT(MUS_AUDIO_DEVICE_NOT_AVAILABLE, audio_out,
			    mus_format("Sonorus device %d (%s) not available",
				       dev, 
				       mus_audio_device_name(dev)));
	  break;
	}
      if (audio_out == -1) 
	RETURN_ERROR_EXIT(MUS_AUDIO_CANT_OPEN, audio_out,
			  mus_format("can't open Sonorus output device %d (%s): %s",
				     dev, 
				     mus_audio_device_name(dev), strerror(errno)));
#ifdef NEW_OSS
      if (ioctl(audio_out, SNDCTL_DSP_CHANNELS, &chans) == -1) 
	RETURN_ERROR_EXIT(MUS_AUDIO_CHANNELS_NOT_AVAILABLE, audio_out,
			  mus_format("can't get %d channels for Sonorus device %d (%s)",
				     chans, dev, 
				     mus_audio_device_name(dev)));
#endif
      return(audio_out);
    }

#if HAVE_SAM_9407
  if (audio_type[sys] == SAM9407_DSP)
    {
      char dname[LABEL_BUFFER_SIZE];
      mus_snprintf(dname, LABEL_BUFFER_SIZE, "/dev/sam%d_dsp", audio_dsp[sys]);
      audio_out = open(dname, O_WRONLY);
      if(audio_out == -1)
	RETURN_ERROR_EXIT(MUS_AUDIO_CANT_OPEN, audio_out,
			  mus_format("can't open %s: %s",
				     dname, 
				     strerror(errno)));
      if ((ioctl(audio_out, SNDCTL_DSP_SETFMT, &oss_format) == -1) || 
	  (oss_format != to_oss_format(format))) 
	RETURN_ERROR_EXIT(MUS_AUDIO_FORMAT_NOT_AVAILABLE, audio_out,
			  mus_format("can't set %s format to %d (%s)",
				     dname, format, 
				     mus_data_format_name(format)));
      if (ioctl(audio_out, SNDCTL_DSP_CHANNELS, &chans) == -1)
	RETURN_ERROR_EXIT(MUS_AUDIO_CHANNELS_NOT_AVAILABLE, audio_out,
			  mus_format("can't get %d channels on %s",
				     chans, dname));
      if (ioctl(audio_out, SNDCTL_DSP_SPEED, &srate) == -1)
	RETURN_ERROR_EXIT(MUS_AUDIO_SRATE_NOT_AVAILABLE, audio_out,
			  mus_format("can't set srate to %d on %s",
				     srate, dname));
      FRAGMENT_SIZE = 14;
      buffer_info = (FRAGMENTS << 16) | (FRAGMENT_SIZE);
      ioctl(audio_out, SNDCTL_DSP_SETFRAGMENT, &buffer_info);
      return(audio_out);
    }
#endif

  if (dev == MUS_AUDIO_DEFAULT)
    audio_out = linux_audio_open_with_error(dev_name = dac_name(sys, 0), 
					    O_WRONLY, 0, sys);
  else audio_out = linux_audio_open_with_error(dev_name = dac_name(sys, (dev == MUS_AUDIO_AUX_OUTPUT) ? 1 : 0), 
					       O_RDWR, 0, sys);
  if (audio_out == -1) return(MUS_ERROR);

  /* ioctl(audio_out, SNDCTL_DSP_RESET, 0); */ /* causes clicks */
  if ((fragments_locked) && ((dev == MUS_AUDIO_DUPLEX_DEFAULT) || (size != 0))) /* only set if user has previously called set_oss_buffers */
    {
      buffer_info = (FRAGMENTS << 16) | (FRAGMENT_SIZE);
      if (ioctl(audio_out, SNDCTL_DSP_SETFRAGMENT, &buffer_info) == -1)
        {
          /* older Linuces (or OSS's?) refuse to handle the fragment reset if O_RDWR used --
           * someone at OSS forgot to update the version number when this was fixed, so
           * I have no way to get around this except to try and retry...
           */
          linux_audio_close(audio_out);
          audio_out = linux_audio_open_with_error(dev_name = dac_name(sys, (dev == MUS_AUDIO_AUX_OUTPUT) ? 1 : 0), 
						  O_WRONLY, 0, sys);
	  if (audio_out == -1) return(MUS_ERROR);
          buffer_info = (FRAGMENTS << 16) | (FRAGMENT_SIZE);
          if (ioctl(audio_out, SNDCTL_DSP_SETFRAGMENT, &buffer_info) == -1) 
	    RETURN_ERROR_EXIT(MUS_AUDIO_CONFIGURATION_NOT_AVAILABLE, audio_out,
			      mus_format("can't set %s fragments to: %d x %d",
					 dev_name, FRAGMENTS, FRAGMENT_SIZE));
        }
    }
  if ((ioctl(audio_out, SNDCTL_DSP_SETFMT, &oss_format) == -1) || 
      (oss_format != to_oss_format(format)))
    RETURN_ERROR_EXIT(MUS_AUDIO_FORMAT_NOT_AVAILABLE, audio_out,
		      mus_format("data format %d (%s) not available on %s",
				 format, 
				 mus_data_format_name(format), 
				 dev_name));
#ifdef NEW_OSS
  if (ioctl(audio_out, SNDCTL_DSP_CHANNELS, &chans) == -1) 
    RETURN_ERROR_EXIT(MUS_AUDIO_CHANNELS_NOT_AVAILABLE, audio_out,
		      mus_format("can't get %d channels on %s",
				 chans, dev_name));
#else
  if (chans == 2) stereo = 1; else stereo = 0;
  if ((ioctl(audio_out, SNDCTL_DSP_STEREO, &stereo) == -1) || 
      ((chans == 2) && (stereo == 0)))
    RETURN_ERROR_EXIT(MUS_AUDIO_CHANNELS_NOT_AVAILABLE, audio_out,
		      mus_format("can't get %d channels on %s",
				 chans, dev_name));
#endif
  if (ioctl(audio_out, SNDCTL_DSP_SPEED, &srate) == -1) 
    RETURN_ERROR_EXIT(MUS_AUDIO_SRATE_NOT_AVAILABLE, audio_out,
		      mus_format("can't set srate of %s to %d",
				 dev_name, srate));
  /* http://www.4front-tech.com/pguide/audio.html says this order has to be followed */
  return(audio_out);
}

static int oss_mus_audio_write(int line, char *buf, int bytes)
{
  if (write(line, buf, bytes) != bytes) 
    RETURN_ERROR_EXIT(MUS_AUDIO_WRITE_ERROR, -1,
		      mus_format("write error: %s", strerror(errno)));
  return(MUS_NO_ERROR);
}

static int oss_mus_audio_close(int line)
{
  if (dsp_reset) ioctl(line, SNDCTL_DSP_RESET, 0);  /* is this needed? */
  return(linux_audio_close(line));
}

static int oss_mus_audio_read(int line, char *buf, int bytes)
{
  if (read(line, buf, bytes) != bytes) 
    RETURN_ERROR_EXIT(MUS_AUDIO_READ_ERROR, -1,
		      mus_format("read error: %s", strerror(errno)));
  return(MUS_NO_ERROR);
}

static char *oss_unsrc(int srcbit)
{
  char *buf;
  int need_and = 0;
  if (srcbit == 0)
    return(strdup("none"));
  else
    {
      buf = (char *)CALLOC(PRINT_BUFFER_SIZE, sizeof(char));
      if (srcbit & SOUND_MASK_MIC) {need_and = 1; strcat(buf, "mic");}
      if (srcbit & SOUND_MASK_LINE) {if (need_and) strcat(buf, " and "); need_and = 1; strcat(buf, "line in");}
      if (srcbit & SOUND_MASK_LINE1) {if (need_and) strcat(buf, " and "); need_and = 1; strcat(buf, "line1");}
      if (srcbit & SOUND_MASK_LINE2) {if (need_and) strcat(buf, " and "); need_and = 1; strcat(buf, "line2");}
      if (srcbit & SOUND_MASK_LINE3) {if (need_and) strcat(buf, " and "); need_and = 1; strcat(buf, "line3");}
      if (srcbit & SOUND_MASK_CD) {if (need_and) strcat(buf, " and "); need_and = 1; strcat(buf, "cd");}
      return(buf);
    }
}

static int oss_mus_audio_open_input(int ur_dev, int srate, int chans, int format, int requested_size)
{
  /* dev can be MUS_AUDIO_DEFAULT or MUS_AUDIO_DUPLEX_DEFAULT as well as the obvious others */
  int audio_fd = -1, oss_format, buffer_info, sys, dev, srcbit, cursrc, adat_mode = 0, err;
  char *dev_name;
#ifndef NEW_OSS
  int stereo;
#endif
  sys = MUS_AUDIO_SYSTEM(ur_dev);
  dev = MUS_AUDIO_DEVICE(ur_dev);
  oss_format = to_oss_format(format);
  if (oss_format == MUS_ERROR)
    RETURN_ERROR_EXIT(MUS_AUDIO_FORMAT_NOT_AVAILABLE, -1,
		      mus_format("format %d (%s) not available",
				 format, 
				 mus_data_format_name(format)));
  if (audio_type[sys] == SONORUS_STUDIO)
    {
      adat_mode = (audio_mode[sys] == 1);
      switch (dev)
	{
	case MUS_AUDIO_DEFAULT:
	  if (adat_mode)
	    audio_fd = open(dev_name = sonorus_name(sys, 11), O_RDONLY, 0);
	  else audio_fd = open(dev_name = sonorus_name(sys, 5), O_RDONLY, 0);
	  break;
	case MUS_AUDIO_ADAT_IN:
	  audio_fd = open(dev_name = sonorus_name(sys, 11), O_RDONLY, 0);
	  break;
	case MUS_AUDIO_AES_IN:
	  audio_fd = open(dev_name = sonorus_name(sys, 20), O_RDONLY, 0);
	  break;
	case MUS_AUDIO_SPDIF_IN:
	  audio_fd = open(dev_name = sonorus_name(sys, 5), O_RDONLY, 0);
	  break;
	default:
	  RETURN_ERROR_EXIT(MUS_AUDIO_DEVICE_NOT_AVAILABLE, -1,
			    mus_format("no %s device on Sonorus?",
				       mus_audio_device_name(dev)));
	  break;
	}
      if (audio_fd == -1) 
	RETURN_ERROR_EXIT(MUS_AUDIO_NO_INPUT_AVAILABLE, -1,
			  mus_format("can't open %s (Sonorus device %s): %s",
				     dev_name, 
				     mus_audio_device_name(dev), 
				     strerror(errno)));
#ifdef NEW_OSS
      if (ioctl(audio_fd, SNDCTL_DSP_CHANNELS, &chans) == -1) 
	RETURN_ERROR_EXIT(MUS_AUDIO_CHANNELS_NOT_AVAILABLE, audio_fd,
			  mus_format("can't get %d channels on %s (Sonorus device %s)",
				     chans, dev_name, 
				     mus_audio_device_name(dev)));
#endif
      return(audio_fd);
    }

#if HAVE_SAM_9407
  if (audio_type[sys] == SAM9407_DSP)
    {
      char dname[LABEL_BUFFER_SIZE];
      mus_snprintf(dname, LABEL_BUFFER_SIZE, "/dev/sam%d_dsp", audio_dsp[sys]);
      audio_fd = open(dname, O_RDONLY);
      if(audio_fd == -1)
	RETURN_ERROR_EXIT(MUS_AUDIO_CANT_OPEN, audio_fd,
			  mus_format("can't open input %s: %s",
				     dname, 
				     strerror(errno)));
      if ((ioctl(audio_fd, SNDCTL_DSP_SETFMT, &oss_format) == -1) || 
	  (oss_format != to_oss_format(format)))
	RETURN_ERROR_EXIT(MUS_AUDIO_FORMAT_NOT_AVAILABLE, audio_fd,
			  mus_format("can't set %s format to %d (%s)",
				     dname, format, 
				     mus_data_format_name(format)));
      if (ioctl(audio_fd, SNDCTL_DSP_CHANNELS, &chans) == -1)
	RETURN_ERROR_EXIT(MUS_AUDIO_CHANNELS_NOT_AVAILABLE, audio_fd,
			  mus_format("can't get %d channels on %s",
				     chans, dname));
      if (ioctl(audio_fd, SNDCTL_DSP_SPEED, &srate) == -1)
	RETURN_ERROR_EXIT(MUS_AUDIO_SRATE_NOT_AVAILABLE, audio_fd,
			  mus_format("can't set srate to %d on %s",
				     srate, dname));
      FRAGMENT_SIZE = 14;
      buffer_info = (FRAGMENTS << 16) | (FRAGMENT_SIZE);
      ioctl(audio_fd, SNDCTL_DSP_SETFRAGMENT, &buffer_info);
      return(audio_fd);
    }
#endif

  if (((dev == MUS_AUDIO_DEFAULT) || (dev == MUS_AUDIO_DUPLEX_DEFAULT)) && (sys == 0))
    audio_fd = linux_audio_open(dev_name = dac_name(sys, 0), 
				O_RDWR, 0, sys);
  else audio_fd = linux_audio_open(dev_name = dac_name(sys, (dev == MUS_AUDIO_AUX_INPUT) ? 1 : 0), 
				   O_RDONLY, 0, sys);
  if (audio_fd == -1)
    {
      if (dev == MUS_AUDIO_DUPLEX_DEFAULT)
	RETURN_ERROR_EXIT(MUS_AUDIO_CONFIGURATION_NOT_AVAILABLE, -1,
		       mus_format("can't open %s (device %s): %s",
				  dev_name, mus_audio_device_name(dev), strerror(errno)));
      if ((audio_fd = linux_audio_open(dev_name = dac_name(sys, (dev == MUS_AUDIO_AUX_INPUT) ? 1 : 0), 
				       O_RDONLY, 0, sys)) == -1)
        {
          if ((errno == EACCES) || (errno == ENOENT))
	    RETURN_ERROR_EXIT(MUS_AUDIO_NO_READ_PERMISSION, -1,
			      mus_format("can't open %s (device %s): %s\n  to get input in Linux, we need read permission on /dev/dsp",
					 dev_name, 
					 mus_audio_device_name(dev), 
					 strerror(errno)));
          else RETURN_ERROR_EXIT(MUS_AUDIO_NO_INPUT_AVAILABLE, -1,
				 mus_format("can't open %s (device %s): %s",
					    dev_name, 
					    mus_audio_device_name(dev), 
					    strerror(errno)));
        }
    }
#ifdef SNDCTL_DSP_SETDUPLEX
  else 
    ioctl(audio_fd, SNDCTL_DSP_SETDUPLEX, &err); /* not always a no-op! */
#endif
  if (audio_type[sys] == RME_HAMMERFALL) return(audio_fd);
  if (audio_type[sys] == DELTA_66) return(audio_fd);
  /* need to make sure the desired recording source is active -- does this actually have any effect? */
  switch (dev)
    {
    case MUS_AUDIO_MICROPHONE: srcbit = SOUND_MASK_MIC;                   break;
    case MUS_AUDIO_LINE_IN:    srcbit = SOUND_MASK_LINE;                  break;
    case MUS_AUDIO_LINE1:      srcbit = SOUND_MASK_LINE1;                 break;
    case MUS_AUDIO_LINE2:      srcbit = SOUND_MASK_LINE2;                 break;
    case MUS_AUDIO_LINE3:      srcbit = SOUND_MASK_LINE3;                 break; /* also digital1..3 */
    case MUS_AUDIO_DUPLEX_DEFAULT: 
    case MUS_AUDIO_DEFAULT:    srcbit = SOUND_MASK_LINE | SOUND_MASK_MIC; break;
    case MUS_AUDIO_CD:         srcbit = SOUND_MASK_CD;                    break;
    default:                   srcbit = 0;                                break;
      /* other possibilities: synth, radio, phonein but these apparently bypass the mixer (no gains?) */
    }
  ioctl(audio_fd, MIXER_READ(SOUND_MIXER_RECSRC), &cursrc);
  srcbit = (srcbit | cursrc);
  ioctl(audio_fd, MIXER_WRITE(SOUND_MIXER_RECSRC), &srcbit);
  ioctl(audio_fd, MIXER_READ(SOUND_MIXER_RECSRC), &cursrc);
  if (cursrc != srcbit)
    {
      char *str1, *str2;
      str1 = oss_unsrc(srcbit);
      str2 = oss_unsrc(cursrc);
      mus_print("weird: tried to set recorder source to %s, but got %s?", str1, str2);
      FREE(str1);
      FREE(str2);
    }
  if (dsp_reset) ioctl(audio_fd, SNDCTL_DSP_RESET, 0);
  if ((fragments_locked) && (requested_size != 0))
    {
      buffer_info = (FRAGMENTS << 16) | (FRAGMENT_SIZE);
      ioctl(audio_fd, SNDCTL_DSP_SETFRAGMENT, &buffer_info);
    }
  if ((ioctl(audio_fd, SNDCTL_DSP_SETFMT, &oss_format) == -1) ||
      (oss_format != to_oss_format(format)))
    RETURN_ERROR_EXIT(MUS_AUDIO_FORMAT_NOT_AVAILABLE, audio_fd,
		      mus_format("can't set %s format to %d (%s)",
				 dev_name, format, 
				 mus_data_format_name(format)));
#ifdef NEW_OSS
  if (ioctl(audio_fd, SNDCTL_DSP_CHANNELS, &chans) == -1) 
    RETURN_ERROR_EXIT(MUS_AUDIO_CHANNELS_NOT_AVAILABLE, audio_fd,
		      mus_format("can't get %d channels on %s",
				 chans, dev_name));
#else
  if (chans == 2) stereo = 1; else stereo = 0;
  if ((ioctl(audio_fd, SNDCTL_DSP_STEREO, &stereo) == -1) || 
      ((chans == 2) && (stereo == 0))) 
    RETURN_ERROR_EXIT(MUS_AUDIO_CHANNELS_NOT_AVAILABLE, audio_fd,
		      mus_format("can't get %d channels on %s (%s)",
				 chans, dev_name, 
				 mus_audio_device_name(dev)));
#endif
  if (ioctl(audio_fd, SNDCTL_DSP_SPEED, &srate) == -1) 
    RETURN_ERROR_EXIT(MUS_AUDIO_SRATE_NOT_AVAILABLE, audio_fd,
		      mus_format("can't set srate to %d on %s (%s)",
				 srate, dev_name, 
				 mus_audio_device_name(dev)));
  return(audio_fd);
}


static int oss_mus_audio_mixer_read(int ur_dev, int field, int chan, float *val)
{
  int fd, amp, channels, err = MUS_NO_ERROR, devmask, stereodevs, ind, formats, sys, dev, srate, adat_mode = 0;
  char *dev_name = NULL;
  sys = MUS_AUDIO_SYSTEM(ur_dev);
  dev = MUS_AUDIO_DEVICE(ur_dev);
  if (audio_type[sys] == SONORUS_STUDIO)
    {
      adat_mode = (audio_mode[sys] == 1);
      if (dev == MUS_AUDIO_MIXER) val[0] = 0; /* no mixer */
      else
	{
	  if (field == MUS_AUDIO_PORT)
	    {
	      if (adat_mode)
		{
		  val[0] = 5;
		  val[1] = MUS_AUDIO_ADAT_IN;
		  val[2] = MUS_AUDIO_ADAT_OUT;
		  val[3] = MUS_AUDIO_SPEAKERS;
		  val[4] = MUS_AUDIO_AES_IN;
		  val[5] = MUS_AUDIO_AES_OUT;
		}
	      else
		{
		  val[0] = 3;
		  val[1] = MUS_AUDIO_SPDIF_IN;
		  val[2] = MUS_AUDIO_SPDIF_OUT;
		  val[3] = MUS_AUDIO_SPEAKERS;
		}
	    }
	  else
	    {
	      if (field == MUS_AUDIO_FORMAT)
		{
		  val[0] = 1;
		  val[1] = MUS_LSHORT;
		}
	      else
		{
		  if (field == MUS_AUDIO_CHANNEL)
		    {
		      switch (dev)
			{
			case MUS_AUDIO_SPEAKERS:
			  channels = 2; 
			  break;
			case MUS_AUDIO_ADAT_IN: case MUS_AUDIO_ADAT_OUT:
			  channels = 8; 
			  break;
			case MUS_AUDIO_AES_IN: case MUS_AUDIO_AES_OUT:
			  channels = 2; 
			  break;
			case MUS_AUDIO_SPDIF_IN: case MUS_AUDIO_SPDIF_OUT:
			  channels = 4; 
			  break;
			case MUS_AUDIO_DEFAULT: 
			  if (adat_mode) 
			    channels = 8; 
			  else channels = 4; 
			  break;
			default:
			  channels = 0; 
			  break;
			}
		      val[0] = channels;
		    }
		  else
		    {
		      if (field == MUS_AUDIO_SRATE)
			{
			  val[0] = 44100;
			}
		    }
		}
	    }
	}
      return(MUS_NO_ERROR);
    }

#if HAVE_SAM_9407
  if (audio_type[sys] == SAM9407_DSP)
    {
      switch(field) 
	{
	case MUS_AUDIO_PORT:
	  val[0] = 2;
	  val[1] = MUS_AUDIO_SPEAKERS;
	  val[2] = MUS_AUDIO_LINE_IN;
	  break;
	case MUS_AUDIO_FORMAT:
	  val[0] = 1;
	  val[1] = MUS_LSHORT;
	  break;
	case MUS_AUDIO_CHANNEL:
	  val[0] = 2;
	  break;
	case MUS_AUDIO_AMP:
	  RETURN_ERROR_EXIT(MUS_AUDIO_DEVICE_NOT_AVAILABLE, -1,
			    mus_format("can't read %s's gains in Sam9407",
				       mus_audio_device_name(dev)));
	  break;
	case MUS_AUDIO_SRATE:
	  val[0] = 44100;
	  break;
	default:
	  RETURN_ERROR_EXIT(MUS_AUDIO_CANT_READ, -1,
			    mus_format("can't read %s's %s in Sam9407",
				       mus_audio_device_name(dev),
				       mus_audio_device_name(field)));
	  break;
	}
      return(MUS_NO_ERROR);
    }
#endif

  if (audio_type[sys] == RME_HAMMERFALL)
    {
      if (dev == MUS_AUDIO_MIXER) val[0] = 0; /* no mixer */
      else
	{
	  if (field == MUS_AUDIO_PORT)
	    {
	      val[0] = 5;
	      val[1] = MUS_AUDIO_ADAT_IN;
	      val[2] = MUS_AUDIO_ADAT_OUT;
	      val[3] = MUS_AUDIO_SPEAKERS;
	      val[4] = MUS_AUDIO_AES_IN;
	      val[5] = MUS_AUDIO_AES_OUT;
	    }
	  else
	    {
	      if (field == MUS_AUDIO_FORMAT)
		{
		  val[0] = 1;
		  val[1] = MUS_LSHORT;
		}
	      else
		{
		  if (field == MUS_AUDIO_CHANNEL)
		    {
		      switch (dev)
			{
			case MUS_AUDIO_SPEAKERS:
			  channels = 2; 
			  break;
			case MUS_AUDIO_ADAT_IN: case MUS_AUDIO_ADAT_OUT:
			  channels = 8; 
			  break;
			case MUS_AUDIO_AES_IN: case MUS_AUDIO_AES_OUT:
			  channels = 2; 
			  break;
			case MUS_AUDIO_SPDIF_IN: case MUS_AUDIO_SPDIF_OUT:
			  channels = 4; 
			  break;
			case MUS_AUDIO_DEFAULT:
			  channels = 8; 
			  break;
			default:
			  channels = 0;
			  break;
			}
		      val[0] = channels;
		    }
		  else
		    {
		      if (field == MUS_AUDIO_SRATE)
			{
			  val[0] = 44100;
			}
		    }
		}
	    }
	}
      return(MUS_NO_ERROR);
    }

  fd = linux_audio_open(dev_name = mixer_name(sys), O_RDONLY | O_NONBLOCK, 0, sys);
  if (fd == -1) 
    {
      fd = linux_audio_open(DAC_NAME, O_RDONLY, 0, sys);
      if (fd == -1)
	{
	  fd = linux_audio_open(DAC_NAME, O_WRONLY, 0, sys);
	  if (fd == -1) 
	    {
	      RETURN_ERROR_EXIT(MUS_AUDIO_CANT_OPEN, -1,
				mus_format("can't open input %s or %s: %s",
					   dev_name, DAC_NAME, 
					   strerror(errno)));
	      return(MUS_ERROR);
	    }
	  else dev_name = DAC_NAME;
	}
      else dev_name = DAC_NAME;
    }
  if (ioctl(fd, SOUND_MIXER_READ_DEVMASK, &devmask))
    RETURN_ERROR_EXIT(MUS_AUDIO_CONFIGURATION_NOT_AVAILABLE, fd,
		      mus_format("can't read device info from %s",
				 dev_name));
  err = 0;
  if ((dev == MUS_AUDIO_MIXER) || 
      (dev == MUS_AUDIO_DAC_FILTER))           /* these give access to all the on-board analog input gain controls */
    {
      amp = 0;
      ioctl(fd, SOUND_MIXER_READ_DEVMASK, &devmask);
      switch (field)
        { 
          /* also DIGITAL1..3 PHONEIN PHONEOUT VIDEO RADIO MONITOR */
	  /* the digital lines should get their own panes in the recorder */
	  /* not clear whether the phone et al lines are routed to the ADC */
	  /* also, I've never seen a card with any of these devices */
        case MUS_AUDIO_IMIX:   if (SOUND_MASK_IMIX & devmask)    err = ioctl(fd, MIXER_READ(SOUND_MIXER_IMIX), &amp);     break;
        case MUS_AUDIO_IGAIN:  if (SOUND_MASK_IGAIN & devmask)   err = ioctl(fd, MIXER_READ(SOUND_MIXER_IGAIN), &amp);    break;
        case MUS_AUDIO_RECLEV: if (SOUND_MASK_RECLEV & devmask)  err = ioctl(fd, MIXER_READ(SOUND_MIXER_RECLEV), &amp);   break;
        case MUS_AUDIO_PCM:    if (SOUND_MASK_PCM & devmask)     err = ioctl(fd, MIXER_READ(SOUND_MIXER_PCM), &amp);      break;
        case MUS_AUDIO_PCM2:   if (SOUND_MASK_ALTPCM & devmask)  err = ioctl(fd, MIXER_READ(SOUND_MIXER_ALTPCM), &amp);   break;
        case MUS_AUDIO_OGAIN:  if (SOUND_MASK_OGAIN & devmask)   err = ioctl(fd, MIXER_READ(SOUND_MIXER_OGAIN), &amp);    break;
        case MUS_AUDIO_LINE:   if (SOUND_MASK_LINE & devmask)    err = ioctl(fd, MIXER_READ(SOUND_MIXER_LINE), &amp);     break;
        case MUS_AUDIO_MICROPHONE: if (SOUND_MASK_MIC & devmask) err = ioctl(fd, MIXER_READ(SOUND_MIXER_MIC), &amp);      break;
        case MUS_AUDIO_LINE1:  if (SOUND_MASK_LINE1 & devmask)   err = ioctl(fd, MIXER_READ(SOUND_MIXER_LINE1), &amp);    break;
        case MUS_AUDIO_LINE2:  if (SOUND_MASK_LINE2 & devmask)   err = ioctl(fd, MIXER_READ(SOUND_MIXER_LINE2), &amp);    break;
        case MUS_AUDIO_LINE3:  if (SOUND_MASK_LINE3 & devmask)   err = ioctl(fd, MIXER_READ(SOUND_MIXER_LINE3), &amp);    break;
        case MUS_AUDIO_SYNTH:  if (SOUND_MASK_SYNTH & devmask)   err = ioctl(fd, MIXER_READ(SOUND_MIXER_SYNTH), &amp);    break;
        case MUS_AUDIO_BASS:   if (SOUND_MASK_BASS & devmask)    err = ioctl(fd, MIXER_READ(SOUND_MIXER_BASS), &amp);     break;
        case MUS_AUDIO_TREBLE: if (SOUND_MASK_TREBLE & devmask)  err = ioctl(fd, MIXER_READ(SOUND_MIXER_TREBLE), &amp);   break;
        case MUS_AUDIO_CD:     if (SOUND_MASK_CD & devmask)      err = ioctl(fd, MIXER_READ(SOUND_MIXER_CD), &amp);       break;
        case MUS_AUDIO_CHANNEL:
          if (dev == MUS_AUDIO_MIXER)
            {
              channels = 0;
              ioctl(fd, SOUND_MIXER_READ_STEREODEVS, &stereodevs);
	      if (SOUND_MASK_IMIX & devmask)   {if (SOUND_MASK_IMIX & stereodevs)   channels += 2; else channels += 1;}
	      if (SOUND_MASK_IGAIN & devmask)  {if (SOUND_MASK_IGAIN & stereodevs)  channels += 2; else channels += 1;}
	      if (SOUND_MASK_RECLEV & devmask) {if (SOUND_MASK_RECLEV & stereodevs) channels += 2; else channels += 1;}
	      if (SOUND_MASK_PCM & devmask)    {if (SOUND_MASK_PCM & stereodevs)    channels += 2; else channels += 1;}
	      if (SOUND_MASK_ALTPCM & devmask) {if (SOUND_MASK_ALTPCM & stereodevs) channels += 2; else channels += 1;}
	      if (SOUND_MASK_OGAIN & devmask)  {if (SOUND_MASK_OGAIN & stereodevs)  channels += 2; else channels += 1;}
	      if (SOUND_MASK_LINE & devmask)   {if (SOUND_MASK_LINE & stereodevs)   channels += 2; else channels += 1;}
	      if (SOUND_MASK_MIC & devmask)    {if (SOUND_MASK_MIC & stereodevs)    channels += 2; else channels += 1;}
	      if (SOUND_MASK_LINE1 & devmask)  {if (SOUND_MASK_LINE1 & stereodevs)  channels += 2; else channels += 1;}
	      if (SOUND_MASK_LINE2 & devmask)  {if (SOUND_MASK_LINE2 & stereodevs)  channels += 2; else channels += 1;}
	      if (SOUND_MASK_LINE3 & devmask)  {if (SOUND_MASK_LINE3 & stereodevs)  channels += 2; else channels += 1;}
	      if (SOUND_MASK_SYNTH & devmask)  {if (SOUND_MASK_SYNTH & stereodevs)  channels += 2; else channels += 1;}
	      if (SOUND_MASK_CD & devmask)     {if (SOUND_MASK_CD & stereodevs)     channels += 2; else channels += 1;}
            }
          else 
            if (SOUND_MASK_TREBLE & devmask) channels = 2; else channels = 0;
          val[0] = channels;
          linux_audio_close(fd);
          return(MUS_NO_ERROR);
          break;
        case MUS_AUDIO_FORMAT: /* this is asking for configuration info -- we return an array with per-"device" channels */
          ioctl(fd, SOUND_MIXER_READ_STEREODEVS, &stereodevs);
	  for (ind = 0; ind <= MUS_AUDIO_SYNTH; ind++) {if (chan > ind) val[ind] = 0;}
	  if (SOUND_MASK_IMIX & devmask)   {if (chan > MUS_AUDIO_IMIX)       val[MUS_AUDIO_IMIX] = ((SOUND_MASK_IMIX & stereodevs) ? 2 : 1);}
	  if (SOUND_MASK_IGAIN & devmask)  {if (chan > MUS_AUDIO_IGAIN)      val[MUS_AUDIO_IGAIN] = ((SOUND_MASK_IGAIN & stereodevs) ? 2 : 1);}
	  if (SOUND_MASK_RECLEV & devmask) {if (chan > MUS_AUDIO_RECLEV)     val[MUS_AUDIO_RECLEV] = ((SOUND_MASK_RECLEV & stereodevs) ? 2 : 1);}
	  if (SOUND_MASK_PCM & devmask)    {if (chan > MUS_AUDIO_PCM)        val[MUS_AUDIO_PCM] = ((SOUND_MASK_PCM & stereodevs) ? 2 : 1);}
	  if (SOUND_MASK_ALTPCM & devmask) {if (chan > MUS_AUDIO_PCM2)       val[MUS_AUDIO_PCM2] = ((SOUND_MASK_ALTPCM & stereodevs) ? 2 : 1);}
	  if (SOUND_MASK_OGAIN & devmask)  {if (chan > MUS_AUDIO_OGAIN)      val[MUS_AUDIO_OGAIN] = ((SOUND_MASK_OGAIN & stereodevs) ? 2 : 1);}
	  if (SOUND_MASK_LINE & devmask)   {if (chan > MUS_AUDIO_LINE)       val[MUS_AUDIO_LINE] = ((SOUND_MASK_LINE & stereodevs) ? 2 : 1);}
	  if (SOUND_MASK_MIC & devmask)    {if (chan > MUS_AUDIO_MICROPHONE) val[MUS_AUDIO_MICROPHONE] = ((SOUND_MASK_MIC & stereodevs) ? 2 : 1);}
	  if (SOUND_MASK_LINE1 & devmask)  {if (chan > MUS_AUDIO_LINE1)      val[MUS_AUDIO_LINE1] = ((SOUND_MASK_LINE1 & stereodevs) ? 2 : 1);}
	  if (SOUND_MASK_LINE2 & devmask)  {if (chan > MUS_AUDIO_LINE2)      val[MUS_AUDIO_LINE2] = ((SOUND_MASK_LINE2 & stereodevs) ? 2 : 1);}
	  if (SOUND_MASK_LINE3 & devmask)  {if (chan > MUS_AUDIO_LINE3)      val[MUS_AUDIO_LINE3] = ((SOUND_MASK_LINE3 & stereodevs) ? 2 : 1);}
 	  if (SOUND_MASK_SYNTH & devmask)  {if (chan > MUS_AUDIO_SYNTH)      val[MUS_AUDIO_SYNTH] = ((SOUND_MASK_SYNTH & stereodevs) ? 2 : 1);}
	  if (SOUND_MASK_CD & devmask)     {if (chan > MUS_AUDIO_CD)         val[MUS_AUDIO_CD] = ((SOUND_MASK_CD & stereodevs) ? 2 : 1);}
          linux_audio_close(fd);
          return(MUS_NO_ERROR);
          break;
        default: 
	  RETURN_ERROR_EXIT(MUS_AUDIO_CANT_READ, fd,
			    mus_format("can't read %s's (%s) %s",
				       mus_audio_device_name(dev), dev_name,
				       mus_audio_device_name(field)));
	  break;
        }
      if (chan == 0)
        val[0] = ((float)(amp & 0xff)) * 0.01;
      else val[0] = (((float)((amp & 0xff00) >> 8)) * 0.01);
    }
  else
    {
      switch (field)
        {
        case MUS_AUDIO_PORT:
          ind = 1;
          val[1] = MUS_AUDIO_MIXER;
          if ((SOUND_MASK_MIC | SOUND_MASK_LINE | SOUND_MASK_CD) & devmask) {ind++; if (chan > ind) val[ind] = MUS_AUDIO_LINE_IN;}
          /* problem here is that microphone and line_in are mixed before the ADC */
          if (SOUND_MASK_SPEAKER & devmask) {ind++; if (chan > ind) val[ind] = MUS_AUDIO_SPEAKERS;}
          if (SOUND_MASK_VOLUME & devmask)  {ind++; if (chan > ind) val[ind] = MUS_AUDIO_DAC_OUT;}
          if (SOUND_MASK_TREBLE & devmask)  {ind++; if (chan > ind) val[ind] = MUS_AUDIO_DAC_FILTER;}
          /* DIGITAL1..3 as RECSRC(?) => MUS_AUDIO_DIGITAL_IN */
          val[0] = ind;
          break;
#if 1
         case MUS_AUDIO_FORMAT:
	  linux_audio_close(fd);
	  fd = open(dac_name(sys, 0), O_WRONLY, 0);
	  if (fd == -1) fd = open(DAC_NAME, O_WRONLY, 0);
	  if (fd == -1) 
	    {
	      RETURN_ERROR_EXIT(MUS_AUDIO_CANT_OPEN, -1,
				mus_format("can't open %s: %s",
					   DAC_NAME, strerror(errno)));
	      return(MUS_ERROR);
	    }
           ioctl(fd, SOUND_PCM_GETFMTS, &formats);
#else
        case MUS_AUDIO_FORMAT:
          ioctl(fd, SOUND_PCM_GETFMTS, &formats);
	  /* this returns -1 and garbage?? */

	  /* from Steven Schultz:
	I did discover why, in audio.c the SOUND_PCM_GETFMTS ioctl was failing.
	That ioctl call can only be made against the /dev/dsp device and _not_
	the /dev/mixer device.  With that change things starting working real
	nice.
	  */
#endif
	  ind = 0;
	  if (formats & (to_oss_format(MUS_BYTE)))    {ind++; if (chan > ind) val[ind] = MUS_BYTE;}
 	  if (formats & (to_oss_format(MUS_BSHORT)))  {ind++; if (chan > ind) val[ind] = MUS_BSHORT;}
	  if (formats & (to_oss_format(MUS_UBYTE)))   {ind++; if (chan > ind) val[ind] = MUS_UBYTE;}
	  if (formats & (to_oss_format(MUS_MULAW)))   {ind++; if (chan > ind) val[ind] = MUS_MULAW;}
	  if (formats & (to_oss_format(MUS_ALAW)))    {ind++; if (chan > ind) val[ind] = MUS_ALAW;}
	  if (formats & (to_oss_format(MUS_LSHORT)))  {ind++; if (chan > ind) val[ind] = MUS_LSHORT;}
	  if (formats & (to_oss_format(MUS_UBSHORT))) {ind++; if (chan > ind) val[ind] = MUS_UBSHORT;}
	  if (formats & (to_oss_format(MUS_ULSHORT))) {ind++; if (chan > ind) val[ind] = MUS_ULSHORT;}
	  val[0] = ind;
          break;
        case MUS_AUDIO_CHANNEL:
	  channels = 0;
          ioctl(fd, SOUND_MIXER_READ_STEREODEVS, &stereodevs);
	  switch (dev)
	    {
	    case MUS_AUDIO_MICROPHONE: if (SOUND_MASK_MIC & devmask)     {if (SOUND_MASK_MIC & stereodevs) channels = 2; else channels = 1;}     break;
	    case MUS_AUDIO_SPEAKERS:   if (SOUND_MASK_SPEAKER & devmask) {if (SOUND_MASK_SPEAKER & stereodevs) channels = 2; else channels = 1;} break;
	    case MUS_AUDIO_LINE_IN:    if (SOUND_MASK_LINE & devmask)    {if (SOUND_MASK_LINE & stereodevs) channels = 2; else channels = 1;}    break;
	    case MUS_AUDIO_LINE1:      if (SOUND_MASK_LINE1 & devmask)   {if (SOUND_MASK_LINE1 & stereodevs) channels = 2; else channels = 1;}   break;
	    case MUS_AUDIO_LINE2:      if (SOUND_MASK_LINE2 & devmask)   {if (SOUND_MASK_LINE2 & stereodevs) channels = 2; else channels = 1;}   break;
	    case MUS_AUDIO_LINE3:      if (SOUND_MASK_LINE3 & devmask)   {if (SOUND_MASK_LINE3 & stereodevs) channels = 2; else channels = 1;}   break;
	    case MUS_AUDIO_DAC_OUT:    if (SOUND_MASK_VOLUME & devmask)  {if (SOUND_MASK_VOLUME & stereodevs) channels = 2; else channels = 1;}  break;
	    case MUS_AUDIO_DEFAULT:    if (SOUND_MASK_VOLUME & devmask)  {if (SOUND_MASK_VOLUME & stereodevs) channels = 2; else channels = 1;}  break;
	    case MUS_AUDIO_CD:         if (SOUND_MASK_CD & devmask)      {if (SOUND_MASK_CD & stereodevs) channels = 2; else channels = 1;}      break;
	    case MUS_AUDIO_DUPLEX_DEFAULT: 
	      err = ioctl(fd, SNDCTL_DSP_GETCAPS, &ind);
	      if (err != -1)
		channels = (ind & DSP_CAP_DUPLEX);
	      else channels = 0;
	      break;
	    default: 
	      RETURN_ERROR_EXIT(MUS_AUDIO_DEVICE_NOT_AVAILABLE, fd,
				mus_format("can't read channel info from %s (%s)",
					   mus_audio_device_name(dev), dev_name));
	      break;
            }
          val[0] = channels;
          break;
        case MUS_AUDIO_AMP:
          amp = 0;
          switch (dev)
            {
            case MUS_AUDIO_MICROPHONE: if (SOUND_MASK_MIC & devmask)     err = ioctl(fd, MIXER_READ(SOUND_MIXER_MIC), &amp);     break;
            case MUS_AUDIO_SPEAKERS:   if (SOUND_MASK_SPEAKER & devmask) err = ioctl(fd, MIXER_READ(SOUND_MIXER_SPEAKER), &amp); break; 
            case MUS_AUDIO_LINE_IN:    if (SOUND_MASK_LINE & devmask)    err = ioctl(fd, MIXER_READ(SOUND_MIXER_LINE), &amp);    break; 
            case MUS_AUDIO_LINE1:      if (SOUND_MASK_LINE1 & devmask)   err = ioctl(fd, MIXER_READ(SOUND_MIXER_LINE1), &amp);   break; 
            case MUS_AUDIO_LINE2:      if (SOUND_MASK_LINE2 & devmask)   err = ioctl(fd, MIXER_READ(SOUND_MIXER_LINE2), &amp);   break; 
            case MUS_AUDIO_LINE3:      if (SOUND_MASK_LINE3 & devmask)   err = ioctl(fd, MIXER_READ(SOUND_MIXER_LINE3), &amp);   break; 
            case MUS_AUDIO_DAC_OUT:    if (SOUND_MASK_VOLUME & devmask)  err = ioctl(fd, MIXER_READ(SOUND_MIXER_VOLUME), &amp);  break;
            case MUS_AUDIO_DEFAULT:    if (SOUND_MASK_VOLUME & devmask)  err = ioctl(fd, MIXER_READ(SOUND_MIXER_VOLUME), &amp);  break;
            case MUS_AUDIO_CD:         if (SOUND_MASK_CD & devmask)      err = ioctl(fd, MIXER_READ(SOUND_MIXER_CD), &amp);      break;
            default: 
	      RETURN_ERROR_EXIT(MUS_AUDIO_DEVICE_NOT_AVAILABLE, fd,
				mus_format("can't get gain info for %s (%s)",
					   mus_audio_device_name(dev), dev_name));
	      break;
            }
	  if (err) 
	    RETURN_ERROR_EXIT(MUS_AUDIO_CANT_READ, fd,
			      mus_format("can't read %s's (%s) amp info",
					   mus_audio_device_name(dev), dev_name));
          if (chan == 0)
            val[0] = ((float)(amp & 0xff)) * 0.01;
          else val[0] = (((float)((amp & 0xff00) >> 8)) * 0.01);
          break;
	case MUS_AUDIO_SRATE:
	  srate = (int)(val[0]);
	  if (ioctl(fd, SNDCTL_DSP_SPEED, &srate) == -1) 
	    {
	      linux_audio_close(fd);
	      /* see comment from Steven Schultz above */
	      fd = open(dac_name(sys, 0), O_WRONLY, 0);
	      if (fd == -1) fd = open(DAC_NAME, O_WRONLY, 0);
	      if (ioctl(fd, SNDCTL_DSP_SPEED, &srate) == -1) 
		RETURN_ERROR_EXIT(MUS_AUDIO_SRATE_NOT_AVAILABLE, fd,
				  mus_format("can't get %s's (%s) srate",
					     mus_audio_device_name(dev), dev_name));
	    }
	  val[0] = (float)srate;
	  break;
	case MUS_AUDIO_DIRECTION:
	  switch (dev)
	    {
	    case MUS_AUDIO_DIGITAL_OUT: case MUS_AUDIO_LINE_OUT: case MUS_AUDIO_DEFAULT:  case MUS_AUDIO_ADAT_OUT:
	    case MUS_AUDIO_AES_OUT: case MUS_AUDIO_SPDIF_OUT: case MUS_AUDIO_SPEAKERS: case MUS_AUDIO_MIXER:
	    case MUS_AUDIO_DAC_FILTER:  case MUS_AUDIO_AUX_OUTPUT: case MUS_AUDIO_DAC_OUT: 
	      val[0] = 0.0;
	      break;
	    default:  
	      val[0] = 1.0; 
	      break;
	    }
        default: 
	  RETURN_ERROR_EXIT(MUS_AUDIO_CANT_READ, fd,
			    mus_format("can't get %s's (%s) %s",
				       mus_audio_device_name(dev), dev_name,
				       mus_audio_device_name(field)));
	  break;
        }
    }
  return(linux_audio_close(fd));
}

static int oss_mus_audio_mixer_write(int ur_dev, int field, int chan, float *val)
{
  int fd, err = MUS_NO_ERROR, devmask, vol, sys, dev;
  char *dev_name;
  float amp[1];
  sys = MUS_AUDIO_SYSTEM(ur_dev);
  dev = MUS_AUDIO_DEVICE(ur_dev);

#if HAVE_SAM_9407
  if (audio_type[sys] == SAM9407_DSP) return(MUS_NO_ERROR); /* XXX */
#endif

  if (audio_type[sys] == SONORUS_STUDIO) return(MUS_NO_ERROR); /* there are apparently volume controls, but they're not accessible yet */
  if (audio_type[sys] == RME_HAMMERFALL) return(MUS_NO_ERROR);
  if (audio_type[sys] == DELTA_66) return(MUS_NO_ERROR);

  fd = linux_audio_open(dev_name = mixer_name(sys), O_RDWR | O_NONBLOCK, 0, sys);
  if (fd == -1) 
    {
      fd = linux_audio_open_with_error(dev_name = DAC_NAME, O_WRONLY, 0, sys);
      if (fd == -1) return(MUS_ERROR);
    }
  if ((dev == MUS_AUDIO_MIXER) || 
      (dev == MUS_AUDIO_DAC_FILTER))                          /* these give access to all the on-board analog input gain controls */
    {
      if (mus_audio_mixer_read(ur_dev, field, (chan == 0) ? 1 : 0, amp)) 
	{
	  linux_audio_close(fd); 
	  return(MUS_ERROR);
	}
      if (val[0] >= 0.99) val[0] = 0.99;  
      if (val[0] < 0.0) val[0] = 0.0;
      if (amp[0] >= 0.99) amp[0] = 0.99;
      if (chan == 0)
        vol = (((int)(amp[0] * 100)) << 8) + ((int)(val[0] * 100));
      else vol = (((int)(val[0] * 100)) << 8) + ((int)(amp[0] * 100));
      ioctl(fd, SOUND_MIXER_READ_DEVMASK, &devmask);
      switch (field)
        {
        case MUS_AUDIO_IMIX:   if (SOUND_MASK_IMIX & devmask)    err = ioctl(fd, MIXER_WRITE(SOUND_MIXER_IMIX), &vol);    break;
        case MUS_AUDIO_IGAIN:  if (SOUND_MASK_IGAIN & devmask)   err = ioctl(fd, MIXER_WRITE(SOUND_MIXER_IGAIN), &vol);   break;
        case MUS_AUDIO_RECLEV: if (SOUND_MASK_RECLEV & devmask)  err = ioctl(fd, MIXER_WRITE(SOUND_MIXER_RECLEV), &vol);  break;
        case MUS_AUDIO_PCM:    if (SOUND_MASK_PCM & devmask)     err = ioctl(fd, MIXER_WRITE(SOUND_MIXER_PCM), &vol);     break;
        case MUS_AUDIO_PCM2:   if (SOUND_MASK_ALTPCM & devmask)  err = ioctl(fd, MIXER_WRITE(SOUND_MIXER_ALTPCM), &vol);  break;
        case MUS_AUDIO_OGAIN:  if (SOUND_MASK_OGAIN & devmask)   err = ioctl(fd, MIXER_WRITE(SOUND_MIXER_OGAIN), &vol);   break;
        case MUS_AUDIO_LINE:   if (SOUND_MASK_LINE & devmask)    err = ioctl(fd, MIXER_WRITE(SOUND_MIXER_LINE), &vol);    break;
        case MUS_AUDIO_MICROPHONE: if (SOUND_MASK_MIC & devmask) err = ioctl(fd, MIXER_WRITE(SOUND_MIXER_MIC), &vol);     break;
        case MUS_AUDIO_LINE1:  if (SOUND_MASK_LINE1 & devmask)   err = ioctl(fd, MIXER_WRITE(SOUND_MIXER_LINE1), &vol);   break;
        case MUS_AUDIO_LINE2:  if (SOUND_MASK_LINE2 & devmask)   err = ioctl(fd, MIXER_WRITE(SOUND_MIXER_LINE2), &vol);   break;
        case MUS_AUDIO_LINE3:  if (SOUND_MASK_LINE3 & devmask)   err = ioctl(fd, MIXER_WRITE(SOUND_MIXER_LINE3), &vol);   break;
        case MUS_AUDIO_SYNTH:  if (SOUND_MASK_SYNTH & devmask)   err = ioctl(fd, MIXER_WRITE(SOUND_MIXER_SYNTH), &vol);   break;
        case MUS_AUDIO_BASS:   if (SOUND_MASK_BASS & devmask)    err = ioctl(fd, MIXER_WRITE(SOUND_MIXER_BASS), &vol);    break;
        case MUS_AUDIO_TREBLE: if (SOUND_MASK_TREBLE & devmask)  err = ioctl(fd, MIXER_WRITE(SOUND_MIXER_TREBLE), &vol);  break;
        case MUS_AUDIO_CD:     if (SOUND_MASK_CD & devmask)      err = ioctl(fd, MIXER_WRITE(SOUND_MIXER_CD), &vol);      break;
        default: 
	  RETURN_ERROR_EXIT(MUS_AUDIO_CANT_WRITE, fd,
			    mus_format("can't write %s's (%s) %s field",
				       mus_audio_device_name(dev), dev_name,
				       mus_audio_device_name(field)));
	  break;
        }
    }
  else
    {
      switch (field)
        {
        case MUS_AUDIO_AMP:
          /* need to read both channel amps, then change the one we're concerned with */
          mus_audio_mixer_read(ur_dev, field, (chan == 0) ? 1 : 0, amp);
          if (val[0] >= 0.99) val[0] = 0.99;  
	  if (val[0] < 0.0) val[0] = 0.0;
          if (amp[0] >= 0.99) amp[0] = 0.99;
          if (chan == 0)
            vol = (((int)(amp[0] * 100)) << 8) + ((int)(val[0] * 100));
          else vol = (((int)(val[0] * 100)) << 8) + ((int)(amp[0] * 100));
          ioctl(fd, SOUND_MIXER_READ_DEVMASK, &devmask);
          switch (dev)
            {
            case MUS_AUDIO_MICROPHONE: if (SOUND_MASK_MIC & devmask)     err = ioctl(fd, MIXER_WRITE(SOUND_MIXER_MIC), &vol);     break;
            case MUS_AUDIO_SPEAKERS:   if (SOUND_MASK_SPEAKER & devmask) err = ioctl(fd, MIXER_WRITE(SOUND_MIXER_SPEAKER), &vol); break;
            case MUS_AUDIO_LINE_IN:    if (SOUND_MASK_LINE & devmask)    err = ioctl(fd, MIXER_WRITE(SOUND_MIXER_LINE), &vol);    break;
            case MUS_AUDIO_LINE1:      if (SOUND_MASK_LINE1 & devmask)   err = ioctl(fd, MIXER_WRITE(SOUND_MIXER_LINE1), &vol);   break;
            case MUS_AUDIO_LINE2:      if (SOUND_MASK_LINE2 & devmask)   err = ioctl(fd, MIXER_WRITE(SOUND_MIXER_LINE2), &vol);   break;
            case MUS_AUDIO_LINE3:      if (SOUND_MASK_LINE3 & devmask)   err = ioctl(fd, MIXER_WRITE(SOUND_MIXER_LINE3), &vol);   break;
            case MUS_AUDIO_DAC_OUT:    if (SOUND_MASK_VOLUME & devmask)  err = ioctl(fd, MIXER_WRITE(SOUND_MIXER_VOLUME), &vol);  break;
            case MUS_AUDIO_DEFAULT:    if (SOUND_MASK_VOLUME & devmask)  err = ioctl(fd, MIXER_WRITE(SOUND_MIXER_VOLUME), &vol);  break;
            case MUS_AUDIO_CD:         if (SOUND_MASK_CD & devmask)      err = ioctl(fd, MIXER_WRITE(SOUND_MIXER_CD), &vol);      break;
            default: 
	      RETURN_ERROR_EXIT(MUS_AUDIO_DEVICE_NOT_AVAILABLE, fd,
				mus_format("device %d (%s) not available on %s",
					   dev, mus_audio_device_name(dev), dev_name));
            }
          break;
        case MUS_AUDIO_SRATE:
          vol = (int)val[0];
	  linux_audio_close(fd);
	  /* see comment from Steven Schultz above */
	  fd = open(dac_name(sys, 0), O_WRONLY, 0);
	  if (fd == -1) fd = open(DAC_NAME, O_WRONLY, 0);

          if (dsp_reset) ioctl(fd, SNDCTL_DSP_RESET, 0);  /* is this needed? */
          err = ioctl(fd, SNDCTL_DSP_SPEED, &vol);
          break;
        default: 
	  RETURN_ERROR_EXIT(MUS_AUDIO_CANT_WRITE, fd,
			    mus_format("can't write %s's (%s) %s field",
				       mus_audio_device_name(dev), dev_name,
				       mus_audio_device_name(field)));
	  break;
          /* case MUS_AUDIO_FORMAT: to force 16-bit input or give up */
          /* case MUS_AUDIO_CHANNEL: to open as stereo if possible?? */
          /* case MUS_AUDIO_PORT: to open digital out? */
        }
    }
  if (err) 
    RETURN_ERROR_EXIT(MUS_AUDIO_WRITE_ERROR, fd,
		      mus_format("possible write problem for %s's (%s) %s field",
				 mus_audio_device_name(dev), dev_name,
				 mus_audio_device_name(field)));
  return(linux_audio_close(fd));
}

static char *synth_names[] = 
  {"",
   "Adlib", "SoundBlaster", "ProAudio Spectrum", "Gravis UltraSound", "MPU 401",
   "SoundBlaster 16", "SoundBlaster 16 MIDI", "6850 UART", "Gravis UltraSound 16", "Microsoft",
   "Personal sound system", "Ensoniq Soundscape", "Personal sound system + MPU", "Personal/Microsoft",
   "Mediatrix Pro", "MAD16", "MAD16 + MPU", "CS4232", "CS4232 + MPU", "Maui",
   "Pseudo-MSS", "Gravis Ultrasound PnP", "UART 401"};

static char *synth_name(int i)
{
#ifdef SNDCARD_UART401
  if ((i > 0) && (i <= SNDCARD_UART401)) 
#else
  if ((i > 0) && (i <= 26))
#endif
    return(synth_names[i]);
  return("unknown");
}

static char *device_types[] = {"FM", "Sampling", "MIDI"};

static char *device_type(int i)
{
  if ((i >= 0) && (i <= 2))
    return(device_types[i]);
  return("unknown");
}

static void yes_no (int condition)
{
  if (condition)  
    pprint("   yes    ");
  else pprint("   no     "); 
}

static int set_dsp(int fd, int channels, int bits, int *rate)
{
  int val;
  val = channels;
  ioctl(fd, SOUND_PCM_WRITE_CHANNELS, &val);
  if (val != channels) return(MUS_ERROR);
  val = bits;
  ioctl(fd, SOUND_PCM_WRITE_BITS, &val);
  if (val != bits) return(MUS_ERROR);
  ioctl(fd, SOUND_PCM_WRITE_RATE, rate);
  return(MUS_NO_ERROR);
}

static void oss_describe_audio_state_1(void)
{
  /* this code taken largely from "Linux Multimedia Guide" by Jeff Tranter, O'Reilly & Associates, Inc 1996 */
  /* it is explicitly released under the GPL, so I think I can use it here without elaborate disguises */
  int fd;
  int status = 0, level, i, recsrc, devmask, recmask, stereodevs, caps;
  int numdevs, rate, channels, bits, blocksize, formats, deffmt, min_rate, max_rate;
  struct synth_info sinfo;
  struct midi_info minfo;
  const char *sound_device_names[] = SOUND_DEVICE_LABELS;
  char dsp_name[LABEL_BUFFER_SIZE];
  char version[LABEL_BUFFER_SIZE];
  int dsp_num = 0;
#ifdef NEW_OSS
  mixer_info mixinfo;
  oss_sysinfo sysinfo;
#endif
  
  if (sound_cards <= 0) mus_audio_initialize();

#ifdef NEW_OSS
  fd = open(DAC_NAME, O_WRONLY, 0);
  if (fd == -1) fd = open(SYNTH_NAME, O_RDONLY, 0);
  if (fd == -1) fd = open(MIXER_NAME, O_RDONLY, 0);
  if (fd != -1)
    {
      status = ioctl(fd, OSS_GETVERSION, &level);
      new_oss_running = (status == 0);
      status = ioctl(fd, OSS_SYSINFO, &sysinfo);
      close(fd);
    }
#endif

  if (new_oss_running)
    {
#ifdef NEW_OSS
      if (status == 0)
	{
	  mus_snprintf(audio_strbuf, PRINT_BUFFER_SIZE, "OSS version: %s\n", sysinfo.version);
	  pprint(audio_strbuf);
	}
      else
	{
	  mus_snprintf(audio_strbuf, PRINT_BUFFER_SIZE, "OSS version: %x.%x.%x\n", (level >> 16) & 0xff, (level >> 8) & 0xff, level & 0xff);
	  pprint(audio_strbuf);
	}
#else
      mus_snprintf(audio_strbuf, PRINT_BUFFER_SIZE, "OSS version: %x.%x.%x\n", (level >> 16) & 0xff, (level >> 8) & 0xff, level & 0xff);
      pprint(audio_strbuf);
#endif
    }
  else 
    {
      /* refers to the version upon compilation */
      mus_snprintf(version, LABEL_BUFFER_SIZE, "%d", SOUND_VERSION);
      mus_snprintf(audio_strbuf, PRINT_BUFFER_SIZE, "OSS version: %c.%c.%c\n", version[0], version[1], version[2]);
      pprint(audio_strbuf);
    }
  
  mus_snprintf(audio_strbuf, PRINT_BUFFER_SIZE, "%d card%s found", sound_cards, (sound_cards != 1) ? "s" : ""); pprint(audio_strbuf);
  if (sound_cards > 1)
    {
      pprint(": ");
      for (i = 0; i < sound_cards; i++)
	{
	  mus_snprintf(audio_strbuf, PRINT_BUFFER_SIZE, "/dev/dsp%d with /dev/mixer%d%s", 
		       audio_dsp[i], 
		       audio_mixer[i], 
		       (i < (sound_cards - 1)) ? ", " : "");
	  pprint(audio_strbuf);
	}
    }
  pprint("\n\n");

  fd = open(SYNTH_NAME, O_RDWR, 0);
  if (fd == -1) fd = open(SYNTH_NAME, O_RDONLY, 0);
  if (fd == -1) 
    {
      mus_snprintf(audio_strbuf, PRINT_BUFFER_SIZE, "%s: %s\n", SYNTH_NAME, strerror(errno)); pprint(audio_strbuf); 
      pprint("no synth found\n"); 
    }
  else
    {
      status = ioctl(fd, SNDCTL_SEQ_NRSYNTHS, &numdevs);
      if (status == -1) 
	{
	  close(fd); fd = -1;
	  pprint("no sequencer?");
	}
      else
	{
	  mus_snprintf(audio_strbuf, PRINT_BUFFER_SIZE, "/dev/sequencer: %d device%s installed\n", numdevs, (numdevs == 1) ? "" : "s"); 
	  pprint(audio_strbuf);
	  for (i = 0; i < numdevs; i++)
	    {
	      sinfo.device = i;
	      status = ioctl(fd, SNDCTL_SYNTH_INFO, &sinfo);
	      if (status != -1)
		{
		  mus_snprintf(audio_strbuf, PRINT_BUFFER_SIZE, "  device: %d: %s, %s, %d voices\n", i, sinfo.name, device_type(sinfo.synth_type), sinfo.nr_voices); 
		  pprint(audio_strbuf);
		}
	    }
	  status = ioctl(fd, SNDCTL_SEQ_NRMIDIS, &numdevs);
	  if (status == -1) 
	    {
	      close(fd); fd = -1;
	      pprint("no midi");
	    }
	  else
	    {
	      mus_snprintf(audio_strbuf, PRINT_BUFFER_SIZE, "  %d midi device%s installed\n", numdevs, (numdevs == 1) ? "" : "s"); 
	      pprint(audio_strbuf);
	      for (i = 0; i < numdevs; i++)
		{
		  minfo.device = i;
		  status = ioctl(fd, SNDCTL_MIDI_INFO, &minfo);
		  if (status != -1)
		    {
		      mus_snprintf(audio_strbuf, PRINT_BUFFER_SIZE, "  device %d: %s, %s\n", i, minfo.name, synth_name(minfo.dev_type)); 
		      pprint(audio_strbuf);
		    }
		}
	    }
	}
    }
  if (fd != -1) close(fd);
  pprint("--------------------------------\n");

MIXER_INFO:
  mus_snprintf(dsp_name, LABEL_BUFFER_SIZE, "%s%d", MIXER_NAME, dsp_num);
  fd = linux_audio_open(dsp_name, O_RDWR, 0, 0);
  if (fd == -1) 
    {
      /* maybe output only */
      fd = linux_audio_open(dsp_name, O_WRONLY, 0, 0);
      if (fd == -1)
        {
          if (dsp_num == 0) 
	    {
	      mus_snprintf(dsp_name, LABEL_BUFFER_SIZE, "%s", DAC_NAME);
	      fd = linux_audio_open(DAC_NAME, O_RDWR, 0, 0);
	      if (fd == -1) 
		{
		  /* maybe output only */
		  fd = linux_audio_open(DAC_NAME, O_WRONLY, 0, 0);
		  if (fd == -1)
		    {
		      pprint("no audio device found\n"); 
		      return;
		    }
		}
	    }
	  else goto AUDIO_INFO; /* no /dev/mixern */
        }
      else pprint("no audio input enabled\n"); 
    }
  if (fd == -1) goto AUDIO_INFO;

#ifdef NEW_OSS
  if (new_oss_running) status = ioctl(fd, SOUND_MIXER_INFO, &mixinfo);
#endif
  mus_snprintf(audio_strbuf, PRINT_BUFFER_SIZE, "%s", dsp_name);
  pprint(audio_strbuf);
#ifdef NEW_OSS
  if ((new_oss_running) && (status == 0)) 
    {
      mus_snprintf(audio_strbuf, PRINT_BUFFER_SIZE, " (%s", mixinfo.name);
      pprint(audio_strbuf);
      for (i = 0; i < sound_cards; i++)
	{
	  if ((audio_mixer[i] == dsp_num) && (audio_type[i] == SONORUS_STUDIO))
	    {
	      mus_snprintf(audio_strbuf, PRINT_BUFFER_SIZE, " in mode %d", audio_mode[i]);
	      pprint(audio_strbuf);
	      break;
	    }
	}
      pprint(")");
    }
#endif
  status = ioctl(fd, SOUND_MIXER_READ_RECSRC, &recsrc);
  if (status == -1) 
    {
      linux_audio_close(fd); 
      fd = -1;
      pprint(" no recsrc\n");
    }
  else
    {
      status = ioctl(fd, SOUND_MIXER_READ_DEVMASK, &devmask);
      if ((status == -1) || (devmask == 0))
	{
	  linux_audio_close(fd); 
	  fd = -1;
	  if (status == -1) pprint(" no devmask\n"); else pprint(" (no reported devices)");
	}
      else
	{
	  status = ioctl(fd, SOUND_MIXER_READ_RECMASK, &recmask);
	  if (status == -1) 
	    {
	      pprint(" no recmask\n");
	      recmask = 0;
	    }
	  status = ioctl(fd, SOUND_MIXER_READ_STEREODEVS, &stereodevs);
	  if (status == -1) 
	    {
	      pprint(" no stereodevs\n");
	      stereodevs = 0;
	    }
	  status = ioctl(fd, SOUND_MIXER_READ_CAPS, &caps);
	  if (status == -1) 
	    {
	      pprint(" no caps\n");
	      caps = 0;
	    }
	  pprint(":\n\n"
		 "  mixer     recording  active     stereo    current\n"
		 "  channel   source     source     device    level\n"
		 "  --------  --------   --------   --------  -------- \n"); 
	  for (i = 0; i < SOUND_MIXER_NRDEVICES; i++)
	    {
	      if ((1<<i) & devmask)
		{
		  mus_snprintf(audio_strbuf, PRINT_BUFFER_SIZE, "  %-10s", sound_device_names[i]); 
		  pprint(audio_strbuf);
		  yes_no((1 << i) & recmask);
		  yes_no((1 << i) & recsrc);
		  yes_no((1 << i) & stereodevs);
		  status = ioctl(fd, MIXER_READ(i), &level);
		  if (status != -1)
		    {
		      if ((1<<i) & stereodevs)
			mus_snprintf(audio_strbuf, PRINT_BUFFER_SIZE, "  %.2f %.2f", (float)(level & 0xff) * 0.01, (float)((level & 0xff00) >> 8) * 0.01);
		      else mus_snprintf(audio_strbuf, PRINT_BUFFER_SIZE, "  %.2f", (float)(level & 0xff) * 0.01);
		      /* can't use %% here because subsequent fprintf in pprint evaluates the %! #$@$! */
		      pprint(audio_strbuf);
		    }
		  pprint("\n"); 
		}
	    }
	  pprint("--------------------------------\n");
	}
    }

AUDIO_INFO:
  if (fd != -1) {linux_audio_close(fd); fd = -1;}
  mus_snprintf(dsp_name, LABEL_BUFFER_SIZE, "%s%d", DAC_NAME, dsp_num);
  fd = linux_audio_open(dsp_name, O_RDWR, 0, 0);
  if ((fd == -1) && (dsp_num == 0)) fd = linux_audio_open(DAC_NAME, O_WRONLY, 0, 0);
  if (fd == -1) return;
  mus_snprintf(audio_strbuf, PRINT_BUFFER_SIZE, "%s:\n\n", dsp_name); pprint(audio_strbuf);
  if ((ioctl(fd, SOUND_PCM_READ_RATE, &rate) != -1) &&
      (ioctl(fd, SOUND_PCM_READ_CHANNELS, &channels) != -1) &&
      (ioctl(fd, SOUND_PCM_READ_BITS, &bits) != -1) &&
      (ioctl(fd, SNDCTL_DSP_GETBLKSIZE, &blocksize) != -1))
    {
      mus_snprintf(audio_strbuf, PRINT_BUFFER_SIZE, 
		   "  defaults:\n    sampling rate: %d, chans: %d, sample size: %d bits, block size: %d bytes", 
		   rate, channels, bits, blocksize); 
      pprint(audio_strbuf);

#ifdef SNDCTL_DSP_GETOSPACE
      {
	audio_buf_info abi;
	ioctl(fd, SNDCTL_DSP_GETOSPACE, &abi);
	mus_snprintf(audio_strbuf, PRINT_BUFFER_SIZE, " (%d fragments)\n", abi.fragments);
	pprint(audio_strbuf);
      }
#else
      pprint("\n");
#endif
 
      deffmt = AFMT_QUERY;
      if ((ioctl(fd, SOUND_PCM_SETFMT, &deffmt) != -1) &&
	  (ioctl(fd, SOUND_PCM_GETFMTS, &formats) != -1))
	{
	  pprint("  supported formats:\n"); 
	  if (formats & AFMT_MU_LAW)    {pprint("    mulaw");                        if (deffmt == AFMT_MU_LAW)    pprint(" (default)"); pprint("\n");}
	  if (formats & AFMT_A_LAW)     {pprint("    alaw");                         if (deffmt == AFMT_A_LAW)     pprint(" (default)"); pprint("\n");}
	  if (formats & AFMT_IMA_ADPCM) {pprint("    adpcm");                        if (deffmt == AFMT_IMA_ADPCM) pprint(" (default)"); pprint("\n");}
	  if (formats & AFMT_U8)        {pprint("    unsigned byte");                if (deffmt == AFMT_U8)        pprint(" (default)"); pprint("\n");}
	  if (formats & AFMT_S16_LE)    {pprint("    signed little-endian short");   if (deffmt == AFMT_S16_LE)    pprint(" (default)"); pprint("\n");}
	  if (formats & AFMT_S16_BE)    {pprint("    signed big-endian short");      if (deffmt == AFMT_S16_BE)    pprint(" (default)"); pprint("\n");}
	  if (formats & AFMT_S8)        {pprint("    signed byte");                  if (deffmt == AFMT_S8)        pprint(" (default)"); pprint("\n");}
	  if (formats & AFMT_U16_LE)    {pprint("    unsigned little-endian short"); if (deffmt == AFMT_U16_LE)    pprint(" (default)"); pprint("\n");}
	  if (formats & AFMT_U16_BE)    {pprint("    unsigned big-endian short");    if (deffmt == AFMT_U16_BE)    pprint(" (default)"); pprint("\n");}
	  if (formats & AFMT_MPEG)      {pprint("    mpeg 2");                       if (deffmt == AFMT_MPEG)      pprint(" (default)"); pprint("\n");}
#ifdef NEW_OSS
	  if (formats & AFMT_S32_LE)    {pprint("    signed little-endian int");     if (deffmt == AFMT_S32_LE)    pprint(" (default)"); pprint("\n");}
	  if (formats & AFMT_S32_BE)    {pprint("    signed big-endian int");        if (deffmt == AFMT_S32_BE)    pprint(" (default)"); pprint("\n");}
#endif
	  status = ioctl(fd, SNDCTL_DSP_GETCAPS, &caps);
	  if (status != -1)
	    {
	      if (caps & DSP_CAP_DUPLEX) pprint("  full duplex\n");
	      pprint("            sample        srate\n  channels   size      min      max\n");
	      for (channels = 1; channels <= 2; channels++)
		{
		  for (bits = 8; bits <= 16; bits += 8)
		    {
		      min_rate = 1;
		      if (set_dsp(fd, channels, bits, &min_rate) == -1) continue;
		      max_rate = 100000;
		      if (set_dsp(fd, channels, bits, &max_rate) == -1) continue;
		      mus_snprintf(audio_strbuf, PRINT_BUFFER_SIZE, "  %4d  %8d  %8d  %8d\n", channels, bits, min_rate, max_rate); 
		      pprint(audio_strbuf);
		    }
		}
	    }
	}
    }
  pprint("--------------------------------\n");
  linux_audio_close(fd); 
  fd = -1;
  dsp_num++; 
  if (dsp_num < 16)
    {
      mus_snprintf(dsp_name, LABEL_BUFFER_SIZE, "%s%d", MIXER_NAME, dsp_num);
      goto MIXER_INFO;
    }
}

static void oss_mus_audio_save (void)
{
  int afd, i, devmask, err, level, system, systems;
  systems = mus_audio_systems();
  for (system = 0; system < systems; system++)
    {
      afd = linux_audio_open(mixer_name(system), O_RDONLY, 0, 0);
      if (afd == -1) 
	mus_print("mus_audio_save: %s: %s", mixer_name(system), strerror(errno));
      else
	{
	  ioctl(afd, SOUND_MIXER_READ_DEVMASK, &devmask);
	  for (i = 0; i < SOUND_MIXER_NRDEVICES; i++)
	    {
	      mixer_state[system][i] = 0;
	      if ((1 << i) & devmask)
		{
		  err = ioctl(afd, MIXER_READ(i), &level);
		  if (err != -1) 
		    mixer_state[system][i] = level;
		}
	    }
	  ioctl(afd, SOUND_PCM_READ_RATE, &(init_srate[system]));
	  ioctl(afd, SOUND_PCM_READ_CHANNELS, &(init_chans[system]));
	  init_format[system] = AFMT_QUERY;
	  ioctl(afd, SOUND_PCM_SETFMT, &(init_format[system]));
	  linux_audio_close(afd);
	}
    }
}

static void oss_mus_audio_restore (void)
{
  int afd, i, level, devmask, system, systems;
  systems = mus_audio_systems();
  for (system = 0; system < systems; system++)
    {
      afd = linux_audio_open(mixer_name(system), O_RDWR, 0, 0);
      if (afd == -1) 
	mus_print("mus_audio_restore: %s: %s", mixer_name(system), strerror(errno));
      else
	{
	  ioctl(afd, SOUND_PCM_WRITE_CHANNELS, &(init_chans[system]));
	  ioctl(afd, SOUND_PCM_WRITE_RATE, &(init_srate[system]));
	  ioctl(afd, SOUND_PCM_SETFMT, &(init_format[system]));
	  ioctl(afd, SOUND_MIXER_READ_DEVMASK, &devmask);
	  for (i = 0; i < SOUND_MIXER_NRDEVICES; i++) 
	    if ((1 << i) & devmask)
	      {
		level = mixer_state[system][i];
		ioctl(afd, MIXER_WRITE(i), &level);
	      }
	  linux_audio_close(afd);
	}
    }
}


/* ------------------------------- both ALSA and OSS ----------------------------------- */
/* API being used */

static int api = ALSA_API;
int mus_audio_api(void) {return(api);}

/* hopefully first call to sndlib will be this... */
static int probe_api(void);
static int (*vect_mus_audio_initialize)(void);

/* FIXME: add a suitable default for all other vectors
   so that a call happening before mus_audio_initialize
   can be detected */
/* I don't think this is necessary -- documentation discusses this
 * (mus_sound_initialize calls mus_audio_initialize)
 */

/* vectors for the rest of the sndlib api */
static void  (*vect_mus_audio_set_oss_buffers)(int num, int size);
static int   (*vect_mus_audio_systems)(void);
static char* (*vect_mus_audio_system_name)(int system);
static char* (*vect_mus_audio_moniker)(void);
static int   (*vect_mus_audio_open_output)(int ur_dev, int srate, int chans, int format, int size);
static int   (*vect_mus_audio_open_input)(int ur_dev, int srate, int chans, int format, int requested_size);
static int   (*vect_mus_audio_write)(int id, char *buf, int bytes);
static int   (*vect_mus_audio_read)(int id, char *buf, int bytes);
static int   (*vect_mus_audio_close)(int id);
static int   (*vect_mus_audio_mixer_read)(int ur_dev, int field, int chan, float *val);
static int   (*vect_mus_audio_mixer_write)(int ur_dev, int field, int chan, float *val);
static void  (*vect_mus_audio_save)(void);
static void  (*vect_mus_audio_restore)(void);
static void  (*vect_describe_audio_state_1)(void);

/* vectors for the rest of the sndlib api */
int mus_audio_initialize(void) 
{
  return(probe_api());
}

void mus_audio_set_oss_buffers(int num, int size) 
{
  vect_mus_audio_set_oss_buffers(num, size);
}

int mus_audio_systems(void) 
{
  return(vect_mus_audio_systems());
}

char* mus_audio_system_name(int system) 
{
  return(vect_mus_audio_system_name(system));
}

#if HAVE_ALSA 
static char* alsa_mus_audio_moniker(void);
#endif

char* mus_audio_moniker(void) 
{
#if (HAVE_OSS && HAVE_ALSA)
  char *both_names;
  both_names = (char *)CALLOC(PRINT_BUFFER_SIZE, sizeof(char));
  /* need to be careful here since these use the same constant buffer */
  strcpy(both_names, oss_mus_audio_moniker());
  strcat(both_names, ", ");
  strcat(both_names, alsa_mus_audio_moniker());
  return(both_names); /* tiny memory leak ... */
#else
  return(vect_mus_audio_moniker());
#endif
}

int mus_audio_open_output(int ur_dev, int srate, int chans, int format, int size) 
{
  return(vect_mus_audio_open_output(ur_dev, srate, chans, format, size));
}

int mus_audio_open_input(int ur_dev, int srate, int chans, int format, int requested_size) 
{
  return(vect_mus_audio_open_input(ur_dev, srate, chans, format, requested_size));
}

int mus_audio_write(int id, char *buf, int bytes) 
{
  return(vect_mus_audio_write(id, buf, bytes));
}

int mus_audio_read(int id, char *buf, int bytes) 
{
  return(vect_mus_audio_read(id, buf, bytes));
}

int mus_audio_close(int id) 
{
  return(vect_mus_audio_close(id));
}

int mus_audio_mixer_read(int ur_dev, int field, int chan, float *val) 
{
  return(vect_mus_audio_mixer_read(ur_dev, field, chan, val));
}

int mus_audio_mixer_write(int ur_dev, int field, int chan, float *val) 
{
  return(vect_mus_audio_mixer_write(ur_dev, field, chan, val));
}

void mus_audio_save(void) 
{
  vect_mus_audio_save();
}

void mus_audio_restore(void) 
{
  vect_mus_audio_restore();
}

static void describe_audio_state_1(void) 
{
  vect_describe_audio_state_1();
}

#if (!HAVE_ALSA)
static int probe_api(void) 
{
  /* go for the oss api */
  api = OSS_API;
  vect_mus_audio_initialize = oss_mus_audio_initialize;
  vect_mus_audio_set_oss_buffers = oss_mus_audio_set_oss_buffers;
  vect_mus_audio_systems = oss_mus_audio_systems;
  vect_mus_audio_system_name = oss_mus_audio_system_name;
  vect_mus_audio_moniker = oss_mus_audio_moniker;
  vect_mus_audio_open_output = oss_mus_audio_open_output;
  vect_mus_audio_open_input = oss_mus_audio_open_input;
  vect_mus_audio_write = oss_mus_audio_write;
  vect_mus_audio_read = oss_mus_audio_read;
  vect_mus_audio_close = oss_mus_audio_close;
  vect_mus_audio_mixer_read = oss_mus_audio_mixer_read;
  vect_mus_audio_mixer_write = oss_mus_audio_mixer_write;
  vect_mus_audio_save = oss_mus_audio_save;
  vect_mus_audio_restore = oss_mus_audio_restore;
  vect_describe_audio_state_1 = oss_describe_audio_state_1;
  return(vect_mus_audio_initialize());
}
#endif

#endif


/* ------------------------------- ALSA ----------------------------------------- */
/*
 * error handling (mus_error) changed by Bill 14-Nov-02
 * 0.5 support removed by Bill 24-Mar-02
 *
 * changed for 0.9.x api by Fernando Lopez-Lezcano <nando@ccrma.stanford.edu>
 *
 *  sndlib "exports" only one soundcard with two directions (if they are available),
 *  and only deals with the alsa library pcm's. It does not scan for available
 *  cards and devices at the hardware level. Which device it uses can be defined by:
 *
 *  - setting variables in the environment (searched for in the following order):
 *    SNDLIB_ALSA_PLAYBACK_DEVICE
 *       defines the name of the playback device
 *    SNDLIB_ALSA_CAPTURE_DEVICE
 *       defines the name of the capture device
 *    SNDLIB_ALSA_DEVICE
 *       defines the name of the playback and capture device
 *    use the first two if the playback and capture devices are different or the
 *    third if they are the same. 
 *  - if no variables are found in the environment sndlib tries to probe for a
 *    default device named "sndlib" (in alsa 0.9 devices are configured in 
 *    /usr/share/alsa/alsa.conf or in ~/.asoundrc)
 *  - if "sndlib" is not a valid device "hw:0,0" is used (which by default should
 *    point to the first device of the first card
 *
 *  Some default settings are controllable through the environment as well:
 *    SNDLIB_ALSA_BUFFER_SIZE = size of each buffer in frames
 *    SNDLIB_ALSA_BUFFERS = number of buffers
 *
 * changed 18-Sep-00 by Bill: new error handling: old mus_audio_error folded into
 *  mus_error; mus_error itself should be used only for "real" errors -- things
 *  that can cause a throw (a kind of global jump elsewhere); use mus_print for informational
 *  stuff -- in Snd, mus_print will also save everything printed in the error dialog.
 *  In a few cases, I tried to fix the code to unwind before mus_error, and in others
 *  I've changed mus_error to mus_print, but some of these may be mistaken.
 *  Look for ?? below for areas where I'm not sure I rewrote code correctly.
 *
 * changed for 0.6.x api by Paul Barton-Davis, pbd@op.net
 *
 * changed for 0.5.x api by Fernando Lopez-Lezcano, nando@ccrma.stanford.edu
 *   04-10-2000:
 *     based on original 0.4.x code by Paul Barton-Davis (not much left of it :-)
 *     also Bill's code and Jaroslav Kysela (aplay.c and friends)
 *
 * Other files changed:
 *   sndlib.h:
 *     added MUS_AUDIO_SAMPLES_PER_CHANNEL
 *   snd-scm.c and snd-noscm.c:
 *     removed HAVE_ALSA in #if's, it would appear that clear_audio_inputs,
 *     dsp_reset, dsp_devices and set_dsp_devices are OSS specific
 *   snd-dac.c:
 *     changed DAC_BUFFER_SIZE to MAX_DAC_BUFFER_SIZE and created static variable
 *     dac_buffer_size so that larger alsa fragments can be accommodated (the 
 *     alsa write function only accepts integer multiples of the fragment size, 
 *     that is, we cannot write half a buffer in one call). 
 *
 * Changes:
 * 04/25/2000: finished major rework, snd-dac now automatically decides which
 *             device or devices it uses for playback. Multiple device use is
 *             for now restricted to only two at most (more changes in Bill's
 *             needed to be able to support more). Four channel playback in 
 *             Ensoniq AudioPCI and relatives possible (with proper settings
 *             of the mixer) as well as using two separate cards. 
 * 04/11/2000: added reporting of alsa sound formats
*/

#if HAVE_ALSA

#if (!HAVE_OSS)
#define AUDIO_OK
#endif

#include <sys/ioctl.h>

#if HAVE_ALSA_ASOUNDLIB_H
  #include <alsa/asoundlib.h>
#else
  #include <sys/asoundlib.h>
#endif

#if SND_LIB_VERSION < ((0<<16)|(6<<8)|(0))
  #error ALSA version is too old -- audio.c needs 0.9 or later
#else
  #define ALSA_9 1
#endif

#ifndef SND_CARDS
  #define SND_CARDS (8)
#endif

static int alsa_mus_error(int type, char *message)
{
  if (message)
    {
      mus_print(message);
      FREE(message);
    }
  return(MUS_ERROR);
}

/* prototypes for the alsa sndlib functions */
static int   alsa_mus_audio_initialize(void);
static void  alsa_mus_audio_set_oss_buffers(int num, int size);
static int   alsa_mus_audio_systems(void);
static char* alsa_mus_audio_system_name(int system);
/* static char* alsa_mus_audio_moniker(void); */ /* moved above */
static int   alsa_mus_audio_open_output(int ur_dev, int srate, int chans, int format, int size);
static int   alsa_mus_audio_open_input(int ur_dev, int srate, int chans, int format, int requested_size);
static int   alsa_mus_audio_write(int id, char *buf, int bytes);
static int   alsa_mus_audio_read(int id, char *buf, int bytes);
static int   alsa_mus_audio_close(int id);
static int   alsa_mus_audio_mixer_read(int ur_dev, int field, int chan, float *val);
static int   alsa_mus_audio_mixer_write(int ur_dev, int field, int chan, float *val);
static void  alsa_mus_audio_save(void);
static void  alsa_mus_audio_restore(void);
static void  alsa_describe_audio_state_1(void);

/* decide which api to activate */

static int probe_api(void) 
{
    int card = -1;
    if (snd_card_next(&card) >= 0 && card >= 0) {
	/* the alsa library has detected one or more cards */
	api = ALSA_API;
	vect_mus_audio_initialize = alsa_mus_audio_initialize;
	vect_mus_audio_set_oss_buffers = alsa_mus_audio_set_oss_buffers;
	vect_mus_audio_systems = alsa_mus_audio_systems;
	vect_mus_audio_system_name = alsa_mus_audio_system_name;
	vect_mus_audio_moniker = alsa_mus_audio_moniker;
	vect_mus_audio_open_output = alsa_mus_audio_open_output;
	vect_mus_audio_open_input = alsa_mus_audio_open_input;
	vect_mus_audio_write = alsa_mus_audio_write;
	vect_mus_audio_read = alsa_mus_audio_read;
	vect_mus_audio_close = alsa_mus_audio_close;
	vect_mus_audio_mixer_read = alsa_mus_audio_mixer_read;
	vect_mus_audio_mixer_write = alsa_mus_audio_mixer_write;
	vect_mus_audio_save = alsa_mus_audio_save;
	vect_mus_audio_restore = alsa_mus_audio_restore;
	vect_describe_audio_state_1 = alsa_describe_audio_state_1;
    } else {
	/* go for the oss api */
        api = OSS_API;
	vect_mus_audio_initialize = oss_mus_audio_initialize;
	vect_mus_audio_set_oss_buffers = oss_mus_audio_set_oss_buffers;
	vect_mus_audio_systems = oss_mus_audio_systems;
	vect_mus_audio_system_name = oss_mus_audio_system_name;
	vect_mus_audio_moniker = oss_mus_audio_moniker;
	vect_mus_audio_open_output = oss_mus_audio_open_output;
	vect_mus_audio_open_input = oss_mus_audio_open_input;
	vect_mus_audio_write = oss_mus_audio_write;
	vect_mus_audio_read = oss_mus_audio_read;
	vect_mus_audio_close = oss_mus_audio_close;
	vect_mus_audio_mixer_read = oss_mus_audio_mixer_read;
	vect_mus_audio_mixer_write = oss_mus_audio_mixer_write;
	vect_mus_audio_save = oss_mus_audio_save;
	vect_mus_audio_restore = oss_mus_audio_restore;
	vect_describe_audio_state_1 = oss_describe_audio_state_1;
    }
    /* will the _real_ mus_audio_initialize please stand up? */
    return(vect_mus_audio_initialize());
}

/* size of buffer in number of samples per channel, 
 * at 44100 approximately 5.9mSecs
 */

static int alsa_samples_per_channel = 1024;

/* whether we want to trace calls 
 *
 * set to "1" to print function trace information in the 
 * snd error window
 */

static int alsa_trace = 0;

/* static char dev_name[64]; */

/* this should go away as it is oss specific */

static int fragment_size = 512; 
static int fragments = 4;

static void alsa_mus_audio_set_oss_buffers (int num, int size) {
    fragments = num; 
    fragment_size = size; 
#if DEBUGGING
    mus_print("set_oss_buffers: %d fragments or size %d", num, size);
#endif
}

/* total number of soundcards in our setup, set by initialize_audio */

/* static int sound_cards = 0; */

/* convert a sndlib sample format to an alsa sample format */

static snd_pcm_format_t to_alsa_format(int snd_format)
{
    switch (snd_format) {
    case MUS_BYTE: 
	return(SND_PCM_FORMAT_S8); 
    case MUS_UBYTE: 
	return(SND_PCM_FORMAT_U8); 
    case MUS_MULAW: 
	return(SND_PCM_FORMAT_MU_LAW); 
    case MUS_ALAW: 
	return(SND_PCM_FORMAT_A_LAW); 
    case MUS_BSHORT: 
	return(SND_PCM_FORMAT_S16_BE); 
    case MUS_LSHORT: 
	return(SND_PCM_FORMAT_S16_LE); 
    case MUS_UBSHORT: 
	return(SND_PCM_FORMAT_U16_BE); 
    case MUS_ULSHORT: 
	return(SND_PCM_FORMAT_U16_LE); 
    case MUS_B24INT: 
	return(SND_PCM_FORMAT_S24_BE); 
    case MUS_L24INT: 
	return(SND_PCM_FORMAT_S24_LE); 
    case MUS_BINT: 
	return(SND_PCM_FORMAT_S32_BE); 
    case MUS_LINT: 
	return(SND_PCM_FORMAT_S32_LE); 
    case MUS_BINTN: 
	return(SND_PCM_FORMAT_S32_BE); 
    case MUS_LINTN: 
	return(SND_PCM_FORMAT_S32_LE); 
    case MUS_BFLOAT: 
	return(SND_PCM_FORMAT_FLOAT_BE); 
    case MUS_LFLOAT: 
	return(SND_PCM_FORMAT_FLOAT_LE); 
    case MUS_BDOUBLE: 
	return(SND_PCM_FORMAT_FLOAT64_BE); 
    case MUS_LDOUBLE: 
	return(SND_PCM_FORMAT_FLOAT64_LE); 
    }
    return((snd_pcm_format_t)MUS_ERROR);
}

/* FIXME: this is not taking yet into account the 
 * number of bits that a given alsa format is actually
 * using... */

static int to_mus_format(int alsa_format) 
{
  /* alsa format definitions from asoundlib.h (0.9 cvs 6/27/2001) */
  switch (alsa_format)
    {
    case SND_PCM_FORMAT_S8:
      return(MUS_BYTE);
    case SND_PCM_FORMAT_U8:
      return(MUS_UBYTE);
    case SND_PCM_FORMAT_S16_LE:
      return(MUS_LSHORT);
    case SND_PCM_FORMAT_S16_BE:
      return(MUS_BSHORT);
    case SND_PCM_FORMAT_U16_LE:
      return(MUS_ULSHORT);
    case SND_PCM_FORMAT_U16_BE:
      return(MUS_UBSHORT);
    case SND_PCM_FORMAT_S24_LE:
      return(MUS_L24INT);
    case SND_PCM_FORMAT_S24_BE:
      return(MUS_B24INT);
    case SND_PCM_FORMAT_S32_LE:
      /* choose the 32bit normalized format 
       * plays 24bit and 16bit files with same
       * upper amplitude bound (for 24 bit cards) */
      return(MUS_LINTN);
    case SND_PCM_FORMAT_S32_BE:
      return(MUS_BINTN);
    case SND_PCM_FORMAT_FLOAT_LE:
      return(MUS_LFLOAT);
    case SND_PCM_FORMAT_FLOAT_BE:
      return(MUS_BFLOAT);
    case SND_PCM_FORMAT_FLOAT64_LE:
      return(MUS_LDOUBLE);
    case SND_PCM_FORMAT_FLOAT64_BE:
      return(MUS_BDOUBLE);
    case SND_PCM_FORMAT_MU_LAW:
      return(MUS_MULAW);
    case SND_PCM_FORMAT_A_LAW:
      return(MUS_ALAW);
    /* formats with no translation in snd */
    case SND_PCM_FORMAT_U24_LE:
    case SND_PCM_FORMAT_U24_BE:
    case SND_PCM_FORMAT_U32_LE:
    case SND_PCM_FORMAT_U32_BE:
    case SND_PCM_FORMAT_IEC958_SUBFRAME_LE:
    case SND_PCM_FORMAT_IEC958_SUBFRAME_BE:
    case SND_PCM_FORMAT_IMA_ADPCM:
    case SND_PCM_FORMAT_MPEG:
    case SND_PCM_FORMAT_GSM:
    case SND_PCM_FORMAT_SPECIAL:
    default:
      return(MUS_ERROR);
    }
}

/* convert a sndlib device into an alsa device number and channel
 * [has to be coordinated with following function!] */

/* very simplistic approach, device mapping should also depend
 * on which card we're dealing with, digital i/o devices should
 * be identified as such and so on */

/* NOTE: in the Delta1010 digital i/o is just a pair of channels
 * in the 10 channel playback frame or 12 channel capture frame,
 * how do we specify that???
 */

static int to_alsa_device(int dev, int *adev, snd_pcm_stream_t *achan)
{
    switch(dev) {
	/* default values are a problem because the concept does
	 * not imply a direction (playback or capture). This works
	 * fine as long as both directions of a device are symetric,
	 * the Midiman 1010, for example, has 10 channel frames for
	 * playback and 12 channel frames for capture and breaks 
	 * the recorder (probes the default, defaults to output, 
	 * uses the values for input). 
	 */
    case MUS_AUDIO_DEFAULT:
    case MUS_AUDIO_DUPLEX_DEFAULT:
    case MUS_AUDIO_LINE_OUT:
	/* analog output */
	(*adev) =0;
	(*achan) =SND_PCM_STREAM_PLAYBACK;
	break;
    case MUS_AUDIO_AUX_OUTPUT:
	/* extra analog output */
	(*adev) =1;
	(*achan) =SND_PCM_STREAM_PLAYBACK;
	break;
    case MUS_AUDIO_DAC_OUT:
	/* analog outputs */
	(*adev) =2;
	(*achan) =SND_PCM_STREAM_PLAYBACK;
	break;
    case MUS_AUDIO_LINE_IN:
	/* analog input */
	(*adev) =0;
	(*achan) =SND_PCM_STREAM_CAPTURE;
	break;
    case MUS_AUDIO_AUX_INPUT:
	/* extra analog input */
	(*adev) =1;
	(*achan) =SND_PCM_STREAM_CAPTURE;
	break;
    case MUS_AUDIO_DIGITAL_OUT:
    case MUS_AUDIO_SPDIF_OUT:
    case MUS_AUDIO_AES_OUT:
    case MUS_AUDIO_ADAT_OUT:
    case MUS_AUDIO_DIGITAL_IN:
    case MUS_AUDIO_SPDIF_IN:
    case MUS_AUDIO_AES_IN:
    case MUS_AUDIO_ADAT_IN:
    case MUS_AUDIO_SPEAKERS:
    case MUS_AUDIO_DAC_FILTER:
    case MUS_AUDIO_MICROPHONE:
    case MUS_AUDIO_MIXER:
    case MUS_AUDIO_LINE1:
    case MUS_AUDIO_LINE2:
    case MUS_AUDIO_LINE3:
    case MUS_AUDIO_CD:
    default:
	return(MUS_ERROR);
	break;
    }
    return(0);
}

/* convert an alsa device into a sndlib device 
 * [has to be coordinated with previous function!] 
 *
 * naming here is pretty much arbitrary. We have to have
 * a bidirectional mapping between sndlib devices and
 * alsa devices and that's just not possible (I think). 
 * This stopgap mapping ignores digital input and output
 * devices - how to differentiate them in alsa?
 */

static int to_sndlib_device(int dev, int channel) {
    switch (channel) {
    case SND_PCM_STREAM_PLAYBACK:
	switch (dev) {
	/* works only for the first three outputs */
	case 0: return(MUS_AUDIO_LINE_OUT);
	case 1: return(MUS_AUDIO_AUX_OUTPUT);
	case 2: return(MUS_AUDIO_DAC_OUT);
	default:
	    return(MUS_ERROR);
	}
    case SND_PCM_STREAM_CAPTURE:
	switch (dev) {
	case 0: return(MUS_AUDIO_LINE_IN);
	case 1: return(MUS_AUDIO_AUX_INPUT);
	default:
	    return(MUS_ERROR);
	}
	break;
    }
    return(MUS_ERROR);
}

/* set schedulling priority to SCHED_FIFO 
 * this will only work if the program that uses sndlib is run as root or is suid root */

#if (!defined(HAVE_CONFIG_H)) || HAVE_SCHED_H
#include <sched.h>

static void set_priority() 
{
    struct sched_param p;
    int min, max, priority;

    if (!getuid() || !geteuid()) {
	min = sched_get_priority_min(SCHED_FIFO);
	max = sched_get_priority_max(SCHED_FIFO);
	priority = min+(max-min)/2;
	p.sched_priority = priority;
	if (sched_setscheduler(0, SCHED_FIFO, &p) ==-1) {
	    fprintf(stderr, "\ncould not activate scheduling with priority %d\n", priority);
	}
	seteuid(getuid());
    }
}
#else
static void set_priority() {}
#endif

/* return the number of cards that are available */

static int alsa_mus_audio_systems(void) 
{
    return(sound_cards);
}

/* return the type of driver we're dealing with */

static char *alsa_mus_audio_moniker(void)
{
  if (version_name == NULL) version_name = (char *)CALLOC(LABEL_BUFFER_SIZE, sizeof(char));
  mus_snprintf(version_name, LABEL_BUFFER_SIZE, "ALSA %s", SND_LIB_VERSION_STR);
  return(version_name);
}


/* handles for both directions of the virtual device */

static snd_pcm_t* handles[2];

/* hardware and software parameter sctructure pointers */

static snd_pcm_hw_params_t *alsa_hw_params[2];
static snd_pcm_sw_params_t *alsa_sw_params[2];

/* some defaults */

static int alsa_open_mode = SND_PCM_ASYNC;
static int alsa_periods = 3;
static snd_pcm_access_t alsa_interleave = SND_PCM_ACCESS_RW_INTERLEAVED;
static int alsa_max_capture_channels = 32;

/* first default name for pcm configuration */

char *alsa_sndlib_device_name = "sndlib";

/* second default for playback and capture: hardware pcm, first card, first device */

char *alsa_default_playback_device_name = "hw:0,0";
char *alsa_default_capture_device_name = "hw:0,0";

/* pcms used by sndlib, playback and capture */

char *alsa_playback_device_name = NULL;
char *alsa_capture_device_name = NULL;

/* return the name of a given system */

static char *alsa_mus_audio_system_name(int system) 
{
    return(alsa_playback_device_name);
}

void alsa_dump_hardware_params(snd_pcm_hw_params_t *params, const char *msg) 
{
    snd_output_t *out;
    snd_output_stdio_attach(&out, stderr, 0);
    fprintf(stderr, "%s\n", msg);
    snd_pcm_hw_params_dump(params, out);
}

void alsa_dump_software_params(snd_pcm_sw_params_t *params, const char *msg) 
{
    snd_output_t *out;
    snd_output_stdio_attach(&out, stderr, 0);
    fprintf(stderr, "%s\n", msg);
    snd_pcm_sw_params_dump(params, out);
}

/* get hardware params for a pcm */

snd_pcm_hw_params_t * alsa_get_hardware_params(char *name, snd_pcm_stream_t stream, int mode)
{
    int err;
    snd_pcm_t *handle;
    if ((err = snd_pcm_open(&handle, name, stream, mode))!=0) {
	alsa_mus_error(MUS_AUDIO_CANT_OPEN, 
		       mus_format("%s: open pcm %s for stream %d: %s",
				  __FUNCTION__, name, stream, snd_strerror(err)));
    } else {
	snd_pcm_hw_params_t *params;
	params = (snd_pcm_hw_params_t *)calloc(1, snd_pcm_hw_params_sizeof());
	if (params == NULL) {
	    snd_pcm_close(handle);
	    alsa_mus_error(MUS_AUDIO_CONFIGURATION_NOT_AVAILABLE, 
			   mus_format("%s: could not allocate memory for hardware params", __FUNCTION__));
	} else {
	    err = snd_pcm_hw_params_any(handle, params);
	    if (err < 0) {
		snd_pcm_close(handle);
		alsa_mus_error(MUS_AUDIO_CONFIGURATION_NOT_AVAILABLE, 
			       mus_format("%s: snd_pcm_hw_params_any: pcm %s, stream %d, error: %s",
					  __FUNCTION__, name, stream, snd_strerror(err)));
	    } else {
		snd_pcm_close(handle);
		return(params);
	    }
	}
    }
    return(NULL);
}

/* allocate software params structure */

snd_pcm_sw_params_t * alsa_allocate_software_params(void)
{
    snd_pcm_sw_params_t *params;
    params = (snd_pcm_sw_params_t *)calloc(1, snd_pcm_sw_params_sizeof());
    if (params == NULL) {
	alsa_mus_error(MUS_AUDIO_CONFIGURATION_NOT_AVAILABLE, 
		       mus_format("%s: could not allocate memory for software params", __FUNCTION__));
    } else {
	return(params);
    }
    return(NULL);
}

/* probe a device name against the list of available pcm devices */

#ifndef SND_CONFIG_GET_ID_ARGS
  #define SND_CONFIG_GET_ID_ARGS 1
#endif

int alsa_probe_device_name(char *name)
{
    snd_config_t *conf;
    snd_config_iterator_t pos, next;
    int err;

    err = snd_config_update();
    if (err < 0) {
	mus_print("%s: snd_config_update: %s", __FUNCTION__, snd_strerror(err));
	return(MUS_ERROR);
    }
    err = snd_config_search(snd_config, "pcm", &conf);
    if (err < 0) {
	mus_print("%s: snd_config_search: %s", __FUNCTION__, snd_strerror(err));
	return(MUS_ERROR);
    }
    snd_config_for_each(pos, next, conf) {
	snd_config_t *c = snd_config_iterator_entry(pos);
#if (SND_CONFIG_GET_ID_ARGS == 2)
	const char *id;
	int err = snd_config_get_id(c, &id);
	if (err == 0) {
	    int result = strncmp(name, id, strlen(id));
	    if (result == 0 &&
		(name[strlen(id)] == '\0' || name[strlen(id)] == ':')) {
		return(MUS_NO_ERROR);
	    }}
#else
	const char *id = snd_config_get_id(c);
	int result = strncmp(name, id, strlen(id));
	if (result == 0 &&
	    (name[strlen(id)] == '\0' || name[strlen(id)] == ':')) {
	    return(MUS_NO_ERROR);
	}
#endif
    }
    return(MUS_ERROR);
}

/* check a device name against the list of available pcm devices */

int alsa_check_device_name(char *name)
{
    if (alsa_probe_device_name(name) == MUS_ERROR) {
	return(alsa_mus_error(MUS_AUDIO_CANT_READ, 
			      mus_format("%s: could not find device \"%s\" in configuration", 
					 __FUNCTION__, name)));
    } else {
	return(MUS_NO_ERROR);
    }
}

/* get a device name from the environment */

char *alsa_get_device_from_env(char *name)
{
    char *string = getenv(name);
    if (string != NULL) {
	if (alsa_check_device_name(string) == MUS_NO_ERROR) {
	    return(string);
	}
    }
    return(NULL);
}

/* get an integer from the environment */

int alsa_get_int_from_env(char *name, int *value, int min, int max)
{
    char *string = getenv(name);
    if (string != NULL) {
	char *end;
	long int result = strtol(string, &end, 10);
	if (result < min || result > max) {
	    return(alsa_mus_error(MUS_AUDIO_CANT_READ, 
				  mus_format("%s: %s ignored: out of range, value=%d, min=%d, max=%d",
					     __FUNCTION__, name, result, min, max)));
	} else if (errno == ERANGE) {
	    return(alsa_mus_error(MUS_AUDIO_CANT_READ, 
				  mus_format("%s: %s ignored: strlol conversion out of range",
					     __FUNCTION__, name)));
	} else {
	    if (*string != '\0' && *end == '\0') {
		*value = (int)result;
		return(MUS_NO_ERROR);
	    } else {
		return(alsa_mus_error(MUS_AUDIO_CANT_READ, 
				      mus_format("%s: %s ignored: value is \"%s\", not an integer",
						 __FUNCTION__, name, string)));
	    }
	}
    }
    return(MUS_ERROR);
}

/* initialize the audio subsystem */

static int alsa_mus_audio_initialize(void) 
{
    char *name;
    char *pname;
    char *cname;
    int value; 
    int dir;
    snd_pcm_uframes_t min_periods, max_periods, min_rec_periods, max_rec_periods;
    snd_pcm_uframes_t min_buffer_size, max_buffer_size, min_rec_buffer_size, max_rec_buffer_size;
    if (audio_initialized) {
	return(0);
    }
    /* set the process schedulling policy to real-time SCHED_FIFO */
    set_priority();
    /* allocate various things */
    dev_name = (char *)CALLOC(LABEL_BUFFER_SIZE, sizeof(char));
    sound_cards = 0;
    /* get trace flag from environment */
    if (alsa_get_int_from_env("SNDLIB_ALSA_TRACE", &value, 0, 1) == MUS_NO_ERROR) {
	alsa_trace = value;
    }
    /* try to get device names from environment */
    pname = alsa_get_device_from_env("SNDLIB_ALSA_PLAYBACK_DEVICE");
    if (name != NULL) {
	alsa_playback_device_name = pname;
    }
    cname = alsa_get_device_from_env("SNDLIB_ALSA_CAPTURE_DEVICE");
    if (cname != NULL) {
	alsa_capture_device_name = cname;
    }
    name = alsa_get_device_from_env("SNDLIB_ALSA_DEVICE");
    if (name != NULL) {
	if (alsa_playback_device_name == NULL) {
	    alsa_playback_device_name = name;
	}
	if (alsa_capture_device_name == NULL) {
	    alsa_capture_device_name = name;
	}
    }
    /* if no device name set yet, try for special sndlib name first */
    if (alsa_playback_device_name == NULL) {
	if (alsa_probe_device_name(alsa_sndlib_device_name) == MUS_NO_ERROR) {
	    alsa_playback_device_name = alsa_sndlib_device_name;
	}
    }
    if (alsa_capture_device_name == NULL) {
	if (alsa_probe_device_name(alsa_sndlib_device_name) == MUS_NO_ERROR) {
	    alsa_capture_device_name = alsa_sndlib_device_name;
	}
    }
    /* if no device name set yet, default to the first hardware card and device */
    if (alsa_playback_device_name == NULL) {
	alsa_playback_device_name = alsa_default_playback_device_name;
    }
    if (alsa_capture_device_name == NULL) {
	alsa_capture_device_name = alsa_default_capture_device_name;
    }
    /* playback stream parameters */
    alsa_hw_params[SND_PCM_STREAM_PLAYBACK] = 
	alsa_get_hardware_params(alsa_playback_device_name, SND_PCM_STREAM_PLAYBACK, alsa_open_mode);
    if (alsa_hw_params[SND_PCM_STREAM_PLAYBACK] != NULL) {
	alsa_sw_params[SND_PCM_STREAM_PLAYBACK] = 
	    alsa_allocate_software_params();
	sound_cards = 1;
    }
    /* capture stream parameters */
    alsa_hw_params[SND_PCM_STREAM_CAPTURE] = 
	alsa_get_hardware_params(alsa_capture_device_name, SND_PCM_STREAM_CAPTURE, alsa_open_mode);
    if (alsa_hw_params[SND_PCM_STREAM_CAPTURE] != NULL) {
	alsa_sw_params[SND_PCM_STREAM_CAPTURE] = 
	    alsa_allocate_software_params();
	sound_cards = 1;
    }
    /* check validity of default periods and buffer size, adjust if necessary 
     *
     * this might not always work because periods and buffer size are checked
     * separately, and they might interact when being set, but it is better
     * than not checking at all anything 
     */
    min_periods = snd_pcm_hw_params_get_periods_min(alsa_hw_params[SND_PCM_STREAM_PLAYBACK], &dir);
    max_periods = snd_pcm_hw_params_get_periods_max(alsa_hw_params[SND_PCM_STREAM_PLAYBACK], &dir);
    if (alsa_hw_params[SND_PCM_STREAM_CAPTURE] != NULL) {
        min_rec_periods = snd_pcm_hw_params_get_periods_min(alsa_hw_params[SND_PCM_STREAM_CAPTURE], &dir);
	max_rec_periods = snd_pcm_hw_params_get_periods_max(alsa_hw_params[SND_PCM_STREAM_CAPTURE], &dir);
	if (max_periods > max_rec_periods) {
	    max_periods = max_rec_periods;
	}
	if (min_periods < min_rec_periods) {
	    min_periods = min_rec_periods;
	}
    }
    if (alsa_periods > max_periods) {
        alsa_periods = max_periods;
    }
    if (alsa_periods < min_periods) {
        alsa_periods = min_periods;
    }
    /* get number of buffers from environment, override if possible */
    if (alsa_get_int_from_env("SNDLIB_ALSA_BUFFERS", &value, 
			      min_periods, 
			      max_periods) == MUS_NO_ERROR) {
	alsa_periods = value;
    }
    min_buffer_size = snd_pcm_hw_params_get_buffer_size_min(alsa_hw_params[SND_PCM_STREAM_PLAYBACK]);
    max_buffer_size = snd_pcm_hw_params_get_buffer_size_max(alsa_hw_params[SND_PCM_STREAM_PLAYBACK]);
    if (alsa_hw_params[SND_PCM_STREAM_CAPTURE] != NULL) {
        min_rec_buffer_size = snd_pcm_hw_params_get_buffer_size_min(alsa_hw_params[SND_PCM_STREAM_CAPTURE]);
	max_rec_buffer_size = snd_pcm_hw_params_get_buffer_size_max(alsa_hw_params[SND_PCM_STREAM_CAPTURE]);
	if (max_buffer_size > max_rec_buffer_size) {
	    max_buffer_size = max_rec_buffer_size;
	}
	if (min_buffer_size < min_rec_buffer_size) {
	    min_buffer_size = min_rec_buffer_size;
	}
    }
    if (alsa_samples_per_channel*alsa_periods > max_buffer_size) {
        alsa_samples_per_channel = max_buffer_size/alsa_periods;
    }
    if (alsa_samples_per_channel*alsa_periods < min_buffer_size) {
        alsa_samples_per_channel = min_buffer_size/alsa_periods;
    }
    /* get buffer size from environment, override if possible */
    if (alsa_get_int_from_env("SNDLIB_ALSA_BUFFER_SIZE", &value, 
			      min_buffer_size/alsa_periods, 
			      max_buffer_size/alsa_periods) == MUS_NO_ERROR) {
	alsa_samples_per_channel = value;
    }
    audio_initialized = TRUE;
    return 0;
}

/* dump current hardware and software configuration */

static void alsa_dump_configuration(char *name, snd_pcm_hw_params_t *hw_params, snd_pcm_sw_params_t *sw_params)
{
    int err; 
    char *str;
    size_t len;
    snd_output_t *buf;
    err = snd_output_buffer_open(&buf);
    if (err < 0) {
	mus_print("%s: could not open dump buffer: %s", 
		  __FUNCTION__, snd_strerror(err));
    } else {
	if (hw_params != NULL) {
	    snd_output_puts(buf, "hw_params status of ");
	    snd_output_puts(buf, name);
	    snd_output_puts(buf, "\n");
	    err = snd_pcm_hw_params_dump(hw_params, buf);
	    if (err < 0) {
		mus_print("%s: snd_pcm_hw_params_dump: %s", __FUNCTION__, snd_strerror(err));
	    }
	}
	if (sw_params != NULL) {
	    snd_output_puts(buf, "sw_params status of ");
	    snd_output_puts(buf, name);
	    snd_output_puts(buf, "\n");
	    err = snd_pcm_sw_params_dump(sw_params, buf);
	    if (err < 0) {
		mus_print("%s: snd_pcm_hw_params_dump: %s", __FUNCTION__, snd_strerror(err));
	    }
	}
	snd_output_putc(buf, '\0');
	len = snd_output_buffer_string(buf, &str);
	if (len > 1) {
	    mus_print("%s: status of %s\n%s", __FUNCTION__,
		      name, str);
	}
	snd_output_close(buf);
    }
}

/* open an input or output stream */

static int alsa_audio_open(int ur_dev, int srate, int chans, int format, int size)
{
    int card, device, alsa_device;
    snd_pcm_format_t alsa_format;
    snd_pcm_stream_t alsa_stream;
    char *alsa_name;
    int frames, periods;
    int err;
    unsigned int r;
    snd_pcm_t *handle;
    snd_pcm_hw_params_t *hw_params = NULL;
    snd_pcm_sw_params_t *sw_params = NULL;
    if (alsa_trace) mus_print("%s: %x rate=%d, chans=%d, format=%d:%s, size=%d", 
			       __FUNCTION__, ur_dev, srate, chans, format, 
			      mus_audio_format_name(format), size);
    card = MUS_AUDIO_SYSTEM(ur_dev);
    device = MUS_AUDIO_DEVICE(ur_dev);
    if ((err = to_alsa_device(device, &alsa_device, &alsa_stream))<0) {
	return(alsa_mus_error(MUS_AUDIO_DEVICE_NOT_AVAILABLE, 
			      mus_format("%s: cannot translate device %s<%d> to alsa",
					 __FUNCTION__, mus_audio_device_name(device), device)));
    }
    if ((alsa_format = to_alsa_format(format)) == (snd_pcm_format_t)MUS_ERROR) {
	return(alsa_mus_error(MUS_AUDIO_FORMAT_NOT_AVAILABLE, 
			      mus_format("%s: could not change %s<%d> to alsa format", 
					 __FUNCTION__, mus_audio_format_name(format), format)));
    }
    alsa_name = (alsa_stream == SND_PCM_STREAM_PLAYBACK)?
	alsa_playback_device_name:
	alsa_capture_device_name;
    if ((err = snd_pcm_open(&handle, alsa_name, alsa_stream, alsa_open_mode))!=0) {
	snd_pcm_close(handle);
	return(alsa_mus_error(MUS_AUDIO_CANT_OPEN, 
			      mus_format("%s: open pcm %s (%s) stream %s: %s", __FUNCTION__,
					 mus_audio_device_name(device), alsa_name, snd_pcm_stream_name(alsa_stream), 
					 snd_strerror(err))));
    }
    handles[alsa_stream] = handle;
    hw_params = alsa_hw_params[alsa_stream];
    sw_params = alsa_sw_params[alsa_stream];
    if ((err = snd_pcm_hw_params_any(handle, hw_params)) < 0) {
	snd_pcm_close(handle);
	handles[alsa_stream] = NULL;
	alsa_dump_configuration(alsa_name, hw_params, sw_params);
	return(alsa_mus_error(MUS_AUDIO_CONFIGURATION_NOT_AVAILABLE, 
			      mus_format("%s: no parameter configurations available for %s", 
					 __FUNCTION__, alsa_name)));
    }
    err = snd_pcm_hw_params_set_access(handle, hw_params, alsa_interleave);
    if (err < 0) {
	snd_pcm_close(handle);
	handles[alsa_stream] = NULL;
	alsa_dump_configuration(alsa_name, hw_params, sw_params);
	return(alsa_mus_error(MUS_AUDIO_CONFIGURATION_NOT_AVAILABLE, 
			      mus_format("%s: %s: access type %s not available", 
					 __FUNCTION__, alsa_name, snd_pcm_access_name(alsa_interleave))));
    }
    periods = alsa_periods;
    err = snd_pcm_hw_params_set_periods(handle, hw_params, periods, 0);
    if (err < 0) {
	int dir;
	snd_pcm_uframes_t min, max;
	min = snd_pcm_hw_params_get_periods_min(hw_params, &dir);
	max = snd_pcm_hw_params_get_periods_max(hw_params, &dir);
	snd_pcm_close(handle);
	handles[alsa_stream] = NULL;
	alsa_dump_configuration(alsa_name, hw_params, sw_params);
	return(alsa_mus_error(MUS_AUDIO_CONFIGURATION_NOT_AVAILABLE, 
			      mus_format("%s: %s: cannot set number of periods to %d, min is %d, max is %d", 
					 __FUNCTION__, alsa_name, periods, min, max)));
    }
    frames = size/chans/mus_bytes_per_sample(format);
    err = snd_pcm_hw_params_set_buffer_size(handle, hw_params, frames*periods);
    if (err < 0) {
	snd_pcm_uframes_t min, max;
	min = snd_pcm_hw_params_get_buffer_size_min(hw_params);
	max = snd_pcm_hw_params_get_buffer_size_max(hw_params);
	snd_pcm_close(handle);
	handles[alsa_stream] = NULL;
	alsa_dump_configuration(alsa_name, hw_params, sw_params);
	return(alsa_mus_error(MUS_AUDIO_CONFIGURATION_NOT_AVAILABLE, 
			      mus_format("%s: %s: cannot set buffer size to %d periods of %d frames; \
total requested buffer size is %d frames, minimum allowed is %d, maximum is %d", 
					 __FUNCTION__, alsa_name, periods, frames, periods*frames, min, max)));
    }
    err = snd_pcm_hw_params_set_format(handle, hw_params, alsa_format);
    if (err < 0) {
	snd_pcm_close(handle);
	handles[alsa_stream] = NULL;
	alsa_dump_configuration(alsa_name, hw_params, sw_params);
	return(alsa_mus_error(MUS_AUDIO_CONFIGURATION_NOT_AVAILABLE, 
			      mus_format("%s: %s: cannot set format to %s", 
					 __FUNCTION__, alsa_name, snd_pcm_format_name(alsa_format))));
    }
    err = snd_pcm_hw_params_set_channels(handle, hw_params, chans);
    if (err < 0) {
	snd_pcm_close(handle);
	handles[alsa_stream] = NULL;
	alsa_dump_configuration(alsa_name, hw_params, sw_params);
	return(alsa_mus_error(MUS_AUDIO_CONFIGURATION_NOT_AVAILABLE, 
			      mus_format("%s: %s: cannot set channels to %d", 
					 __FUNCTION__, alsa_name, chans)));
    }
    r = snd_pcm_hw_params_set_rate_near(handle, hw_params, srate, 0);
    if (r < 0) {
	snd_pcm_close(handle);
	handles[alsa_stream] = NULL;
	alsa_dump_configuration(alsa_name, hw_params, sw_params);
	return(alsa_mus_error(MUS_AUDIO_CONFIGURATION_NOT_AVAILABLE, 
			      mus_format("%s: %s: cannot set sampling rate near %d", 
					 __FUNCTION__, alsa_name, srate)));
    } else {
	if (r != srate) {
	    mus_print("%s: %s: could not set rate to exactly %d, set to %d instead",
		      __FUNCTION__, alsa_name, srate, r);
	}
    }
    err = snd_pcm_hw_params(handle, hw_params);
    if (err < 0) {
	snd_pcm_close(handle);
	handles[alsa_stream] = NULL;
	alsa_dump_configuration(alsa_name, hw_params, sw_params);
	return(alsa_mus_error(MUS_AUDIO_CONFIGURATION_NOT_AVAILABLE, 
			      mus_format("%s: cannot set hardware parameters for %s", 
					 __FUNCTION__, alsa_name)));
    }
    snd_pcm_sw_params_current(handle, sw_params);
    err = snd_pcm_sw_params(handle, sw_params);
    if (err < 0) {
	snd_pcm_close(handle);
	handles[alsa_stream] = NULL;
	alsa_dump_configuration(alsa_name, hw_params, sw_params);
	return(alsa_mus_error(MUS_AUDIO_CONFIGURATION_NOT_AVAILABLE, 
			      mus_format("%s: cannot set software parameters for %s", 
					 __FUNCTION__, alsa_name)));
    }

    /* for now the id for the stream is the direction identifier, that is
       not a problem because we only advertise one card with two devices */
    return(alsa_stream);
}

/* sndlib support for opening output devices */

static int alsa_mus_audio_open_output(int ur_dev, int srate, int chans, int format, int size)
{
    return(alsa_audio_open(ur_dev, srate, chans, format, size));
}

/* sndlib support for opening input devices */

static int alsa_mus_audio_open_input(int ur_dev, int srate, int chans, int format, int size)
{
    return(alsa_audio_open(ur_dev, srate, chans, format, size));
}

/* sndlib support for closing a device */

static int alsa_mus_audio_close(int id)
{
    int err = 0;
    if (alsa_trace) mus_print( "%s: %d", __FUNCTION__, id); 
    if (handles[id] != NULL) {
	err = snd_pcm_drain(handles[id]);
	if (err != 0) {
	    mus_print("%s: snd_pcm_drain: %s", 
		      __FUNCTION__, snd_strerror(err)); 
	}
	err = snd_pcm_close(handles[id]);
	if (err != 0) {
	    return(alsa_mus_error(MUS_AUDIO_CANT_CLOSE, 
				  mus_format("%s: snd_pcm_close: %s", 
					     __FUNCTION__, snd_strerror(err)))); 
	}
	handles[id] = NULL;
    }
    return(MUS_NO_ERROR);
}

/* recover from underruns or overruns */

static int recover_from_xrun(int id)
{
    int err;
    snd_pcm_status_t *status;
    snd_pcm_state_t state;
    snd_pcm_status_alloca(&status);
    err = snd_pcm_status(handles[id], status);
    if (err < 0) {
	mus_print("%s: snd_pcm_status: %s", __FUNCTION__, snd_strerror(err));
	return(MUS_ERROR);
    }
    state = snd_pcm_status_get_state(status);
    if (state == SND_PCM_STATE_XRUN) {
	mus_print("%s: [under|over]run detected", __FUNCTION__);
	err = snd_pcm_prepare(handles[id]);
	if (err < 0) {
	    mus_print("%s: snd_pcm_prepare: %s", __FUNCTION__, snd_strerror(err));
	} else {
	    return(MUS_NO_ERROR);
	}
    } else {
	mus_print("%s: error, current state is %s", __FUNCTION__, snd_pcm_state_name(state));
    }
    return(MUS_ERROR);
}

/* sndlib support for writing a buffer to an output device */

static int alsa_mus_audio_write(int id, char *buf, int bytes)
{
    snd_pcm_sframes_t status;
    int frames = snd_pcm_bytes_to_frames(handles[id], bytes);
    status = snd_pcm_writei(handles[id], buf, frames);
    if (status == -EAGAIN || (status >= 0 && status < frames)) {
	snd_pcm_wait(handles[id], 1000);
    } else if (status == -EPIPE) {
	return(recover_from_xrun(id));
    } else if (status < 0) {
	mus_print("%s: snd_pcm_writei: %s", __FUNCTION__, snd_strerror(status));
	return(MUS_ERROR);
    }
    return(MUS_NO_ERROR);
}

/* sndlib support for reading a buffer from an input device */

static int alsa_mus_audio_read(int id, char *buf, int bytes)
{
    snd_pcm_sframes_t status;
    int frames = snd_pcm_bytes_to_frames(handles[id], bytes);

    status = snd_pcm_readi(handles[id], buf, frames);
    if (status == -EAGAIN || (status >= 0 && status < frames)) {
	snd_pcm_wait(handles[id], 1000);
    } else if (status == -EPIPE) {
	return(recover_from_xrun(id));
    } else if (status < 0) {
	mus_print("%s: snd_pcm_readi: %s", __FUNCTION__, snd_strerror(status));
	return(MUS_ERROR);
    }
    return(MUS_NO_ERROR);
}

/* read state of the audio hardware */

static int alsa_mus_audio_mixer_read(int ur_dev, int field, int chan, float *val)
{
    int card;
    int device;
    int alsa_device;
    snd_pcm_stream_t alsa_stream;
    int i, f, err;
    int channels;

    card = MUS_AUDIO_SYSTEM(ur_dev);
    device = MUS_AUDIO_DEVICE(ur_dev);
    if (alsa_trace) mus_print( "%s: card=%d, dev=%s<%d>, field=%s<%d>, chan=%d",
			       __FUNCTION__, card, mus_audio_device_name(device), device, 
			       mus_audio_device_name(field), field, 
			       chan);
    /* for now do not implement mixer interface */
    if (device==MUS_AUDIO_MIXER) {
	val[0] = 0;
	return(MUS_NO_ERROR);
    }
    /* MUS_AUDIO_PORT probes for devices and should not depend on the
     * device which was used in the ur_dev argument, we process this
     * before trying to map the device to an alsa device */

    if (field==MUS_AUDIO_PORT) {
	/* under 0.9 we only advertise at most two devices, one for playback 
	   and another one for capture */
	int dev; 
	int i = 1;
	if (alsa_hw_params[SND_PCM_STREAM_PLAYBACK] != NULL) {
	    val[i++] = (float)to_sndlib_device(0, SND_PCM_STREAM_PLAYBACK);
	}
	if (alsa_hw_params[SND_PCM_STREAM_CAPTURE] != NULL) {
	    val[i++] = (float)to_sndlib_device(0, SND_PCM_STREAM_CAPTURE);
	}
	val[0]=(float)(i-1);
	return(MUS_NO_ERROR);
    }
    /* map the mus device to an alsa device and channel */
    if ((err = to_alsa_device(device, &alsa_device, &alsa_stream))<0) {
        /* FIXME: snd-dac still probes some non-existing devices, specifically
	 * MUS_AUDIO_DAC_FILTER, do not report error till that's fixed */
        if (alsa_trace) {
	    mus_print("%s: cannot translate device %s<%d> to alsa, field=%s<%d>",
		      __FUNCTION__, 
		      mus_audio_device_name(device), device, 
		      mus_audio_device_name(field), field);
	}
	return(MUS_ERROR);
    }
    if (alsa_trace) mus_print("%s:         adev=%d, achan=%d",
			       __FUNCTION__, alsa_device, alsa_stream);
    switch (field) {
    case MUS_AUDIO_AMP: 
	/* amplitude value */
	val[0] = 1.0;
	break;
    case MUS_AUDIO_SAMPLES_PER_CHANNEL: 
	/* samples per channel */
	if (card>0 || alsa_device>0) {
	    return(alsa_mus_error(MUS_AUDIO_CANT_READ, NULL));
	} else {
	    val[0] = (float)alsa_samples_per_channel;
	    if (chan > 1) {
	        val[1] = (float)snd_pcm_hw_params_get_buffer_size_min(alsa_hw_params[alsa_stream]); 
		val[2] = (float)snd_pcm_hw_params_get_buffer_size_max(alsa_hw_params[alsa_stream]); 
	    }
	}
	break;
    case MUS_AUDIO_CHANNEL: 
	/* number of channels */
	if (card>0 || alsa_device>0) {
	    return(alsa_mus_error(MUS_AUDIO_CANT_READ, NULL));
	} else {
	    int max_channels = snd_pcm_hw_params_get_channels_max(alsa_hw_params[alsa_stream]);
	    if (alsa_stream == SND_PCM_STREAM_CAPTURE &&
		max_channels > alsa_max_capture_channels) {
		/* limit number of capture channels to a reasonable maximum, if the user
		   specifies a plug pcm as the capture pcm then the returned number of channels
		   would be MAXINT (or whatever the name is for a really big number). At this
		   point there is no support in the alsa api to distinguish between default
		   parameters or those that have been set by a user on purpose, of for querying
		   the hardware pcm device that is hidden by the plug device to see what is the
		   real number of channels for the device we are dealing with. We could also try
		   to flag this as an error to the user and exit the program */
		max_channels = alsa_max_capture_channels;
	    }
	    val[0] = (float)max_channels;
	    if (chan > 1) {
	        val[1] = (float)snd_pcm_hw_params_get_channels_min(alsa_hw_params[alsa_stream]); 
		val[2] = (float)max_channels;
	    }
	}
	break;
    case MUS_AUDIO_SRATE: 
	/* supported sample rates */
	if (card>0 || alsa_device>0) {
	    return(alsa_mus_error(MUS_AUDIO_CANT_READ, NULL));
	} else {
	    int dir = 0;
	    val[0] = 44100;
	    if (chan > 1) {
	        val[1] = (float)snd_pcm_hw_params_get_rate_min(alsa_hw_params[alsa_stream], &dir); 
		val[2] = (float)snd_pcm_hw_params_get_rate_max(alsa_hw_params[alsa_stream], &dir); 
	    }
	}
	break;
    case MUS_AUDIO_FORMAT:
	/* supported formats */
	if (card>0 || alsa_device>0) {
	    return(alsa_mus_error(MUS_AUDIO_CANT_READ, NULL));
	} else {
	    int format;
	    snd_pcm_format_mask_t *mask;
	    snd_pcm_format_mask_alloca(&mask);
	    snd_pcm_hw_params_get_format_mask(alsa_hw_params[alsa_stream], mask); 
	    for (format=0, f=1; format<SND_PCM_FORMAT_LAST; format++) {
		err = snd_pcm_format_mask_test(mask, (snd_pcm_format_t)format);
		if (err>0) {
		    if (f < chan && (to_mus_format(format)!=MUS_ERROR)) {
			val[f++] = (float)to_mus_format(format);
		    }
		}
	    }
	    val[0] = f-1;
	}
	break;                
    case MUS_AUDIO_DIRECTION: 
	/* direction of this device */
	if (card>0 || alsa_device>0) {
	    return(alsa_mus_error(MUS_AUDIO_CANT_READ, NULL));
	} else {
	    /* 0-->playback, 1-->capture */
	    val[0] = (float)alsa_stream;
	}
	break;
    default: 
	return(alsa_mus_error(MUS_AUDIO_CANT_READ, NULL));
	break;
    }
    return(MUS_NO_ERROR);
}

static int alsa_mus_audio_mixer_write(int ur_dev, int field, int chan, float *val)
{
	return(MUS_NO_ERROR);
}

static void alsa_mus_audio_save (void)
{
}

static void alsa_mus_audio_restore (void)
{
}

static void alsa_describe_audio_state_1(void)
{
    int err; 
    char *str;
    size_t len;
    snd_config_t *conf;
    snd_output_t *buf;
    err = snd_config_update();
    if (err < 0) {
	mus_print("%s: snd_config_update: %s", 
		  __FUNCTION__, snd_strerror(err));
	return;
    }
    err = snd_output_buffer_open(&buf);
    if (err < 0) {
	mus_print("%s: could not open dump buffer: %s", 
		  __FUNCTION__, snd_strerror(err));
    } else {
	err = snd_config_search(snd_config, "pcm", &conf);
	if (err < 0) {
	    mus_print("%s: snd_config_search: could not find at least one pcm: %s", 
		      __FUNCTION__, snd_strerror(err));
	    return;
	}
	snd_output_puts(buf, "PCM list:\n");
	snd_config_save(conf, buf);
	snd_output_putc(buf, '\0');
	len = snd_output_buffer_string(buf, &str);
	if (len > 1) {
	    pprint(str);
	}
	snd_output_close(buf);
    }
}
	   
#endif /* HAVE_ALSA */


/* -------------------------------- SUN -------------------------------- */
/*
 * Thanks to Seppo Ingalsuo for several bugfixes.
 * record case improved after perusal of Snack 1.6/src/jkAudio_sun.c
 */

/* apparently input other than 8000 is 16-bit, 8000 is (?) mulaw */
/* apparently Sun-on-Intel reads/writes little-endian */

#if (defined(SUN) || defined(OPENBSD)) && (!(defined(AUDIO_OK)))
#define AUDIO_OK

#include <sys/types.h>
#include <stropts.h>
#include <sys/filio.h>

#ifdef SUNOS
  #include <sun/audioio.h>
#else
  #include <sys/audioio.h>
#endif
#if HAVE_SYS_MIXER_H
  #include <sys/mixer.h>
#endif

int mus_audio_initialize(void) {return(MUS_NO_ERROR);}
int mus_audio_systems(void) {return(1);}
char *mus_audio_system_name(int system) {return("Sun");}

static int sun_default_outputs = (AUDIO_HEADPHONE | AUDIO_LINE_OUT | AUDIO_SPEAKER);

void mus_audio_sun_outputs(int speakers, int headphones, int line_out)
{
  sun_default_outputs = 0;
  if (speakers) sun_default_outputs |= AUDIO_SPEAKER;
  if (headphones) sun_default_outputs |= AUDIO_HEADPHONE;
  if (line_out) sun_default_outputs |= AUDIO_LINE_OUT;
}


#ifdef OPENBSD
  #define DAC_NAME "/dev/sound"
#else
  #define DAC_NAME "/dev/audio"
#endif
#define AUDIODEV_ENV "AUDIODEV"

#define RETURN_ERROR_EXIT(Error_Type, Audio_Line, Ur_Error_Message) \
  do { char *Error_Message; Error_Message = Ur_Error_Message; \
    if (Audio_Line != -1) close(Audio_Line); \
    if (Error_Message) \
      {MUS_STANDARD_ERROR(Error_Type, Error_Message); FREE(Error_Message);} \
    else MUS_STANDARD_ERROR(Error_Type, mus_error_to_string(Error_Type)); \
    return(MUS_ERROR); \
  } while (0)

char *mus_audio_moniker(void) 
{
#ifndef AUDIO_DEV_AMD
  struct audio_device ad;
#else
  int ad;
#endif
  int audio_fd, err;
  char *dev_name;
  if (getenv(AUDIODEV_ENV) != NULL) 
    dev_name = getenv(AUDIODEV_ENV); 
  else dev_name = DAC_NAME;
  audio_fd = open(dev_name, O_RDONLY | O_NONBLOCK, 0);
  if (audio_fd == -1) 
    {
      audio_fd = open("/dev/audioctl", O_RDONLY | O_NONBLOCK, 0);
      if (audio_fd == -1) return("sun probably");
    }
  err = ioctl(audio_fd, AUDIO_GETDEV, &ad); 
  if (err == -1) 
    {
      close(audio_fd); 
      return("sun?");
    }
  mus_audio_close(audio_fd);
#if HAVE_SYS_MIXER_H
  if (version_name == NULL) version_name = (char *)CALLOC(PRINT_BUFFER_SIZE, sizeof(char));
#else
  if (version_name == NULL) version_name = (char *)CALLOC(LABEL_BUFFER_SIZE, sizeof(char));
#endif
#ifndef AUDIO_DEV_AMD
  #if HAVE_SYS_MIXER_H
    mus_snprintf(version_name, PRINT_BUFFER_SIZE, 
		 "audio: %s (%s), %s %s %s", 
		 ad.name, ad.version,
		 MIXER_NAME, MIXER_VERSION, MIXER_CONFIGURATION);
  #else
    mus_snprintf(version_name, LABEL_BUFFER_SIZE, "audio: %s (%s)", ad.name, ad.version);
  #endif
#else
  switch (ad)
    {
    case AUDIO_DEV_AMD:        mus_snprintf(version_name, LABEL_BUFFER_SIZE, "audio: amd");        break;
  #ifdef AUDIO_DEV_CS4231
    case AUDIO_DEV_CS4231:     mus_snprintf(version_name, LABEL_BUFFER_SIZE, "audio: cs4231");     break;
  #endif
    case AUDIO_DEV_SPEAKERBOX: mus_snprintf(version_name, LABEL_BUFFER_SIZE, "audio: speakerbox"); break;
    case AUDIO_DEV_CODEC:      mus_snprintf(version_name, LABEL_BUFFER_SIZE, "audio: codec");      break;
    default:                   mus_snprintf(version_name, LABEL_BUFFER_SIZE, "audio: unknown");    break;
    }
#endif
  return(version_name);
}

static int to_sun_format(int format)
{
  switch (format)
    {
#if MUS_LITTLE_ENDIAN
    case MUS_LSHORT: /* Solaris on Intel? */
#else
    case MUS_BSHORT: 
#endif
#ifdef OPENBSD
      return(AUDIO_ENCODING_PCM16); 
#else
      return(AUDIO_ENCODING_LINEAR); 
#endif
      break;
    case MUS_BYTE: 
#if defined(AUDIO_ENCODING_LINEAR8)
      return(AUDIO_ENCODING_LINEAR8); break;
#else
  #ifdef OPENBSD
      return(AUDIO_ENCODING_PCM8); 
  #else
      return(AUDIO_ENCODING_LINEAR);
  #endif 
      break;
#endif
    case MUS_MULAW: return(AUDIO_ENCODING_ULAW); break;
    case MUS_ALAW:  return(AUDIO_ENCODING_ALAW); break;
      /* there's also AUDIO_ENCODING_DVI */
    }
  return(MUS_ERROR);
}

int mus_audio_open_output(int ur_dev, int srate, int chans, int format, int size)
{
  struct audio_info info;
  char *dev_name;
  int encode, bits, dev;
  int audio_fd, err;
  dev = MUS_AUDIO_DEVICE(ur_dev);
  encode = to_sun_format(format);
  if (encode == MUS_ERROR) 
    RETURN_ERROR_EXIT(MUS_AUDIO_FORMAT_NOT_AVAILABLE, -1,
		      mus_format("format %d (%s) not available",
				 format, 
				 mus_data_format_name(format)));
  if (getenv(AUDIODEV_ENV) != NULL) 
    dev_name = getenv(AUDIODEV_ENV); 
  else dev_name = DAC_NAME;
  if (dev != MUS_AUDIO_DUPLEX_DEFAULT)
    audio_fd = open(dev_name, O_WRONLY, 0);
  else audio_fd = open(dev_name, O_RDWR, 0);
  if (audio_fd == -1) 
    RETURN_ERROR_EXIT(MUS_AUDIO_CANT_OPEN, -1,
		      mus_format("can't open output %s: %s",
				 dev_name, strerror(errno)));
  AUDIO_INITINFO(&info);
  if (dev == MUS_AUDIO_LINE_OUT)
    info.play.port = AUDIO_LINE_OUT;
  else
    {
      if (dev == MUS_AUDIO_SPEAKERS)
	/* OR may not be available */
	info.play.port = AUDIO_SPEAKER | (sun_default_outputs & AUDIO_HEADPHONE);
      else 
	info.play.port = sun_default_outputs;
    }
  info.play.sample_rate = srate; 
  info.play.channels = chans;
  bits = 8 * mus_bytes_per_sample(format);
  info.play.precision = bits;
  info.play.encoding = encode;
  err = ioctl(audio_fd, AUDIO_SETINFO, &info); 
  if (err == -1) 
    RETURN_ERROR_EXIT(MUS_AUDIO_CANT_WRITE, audio_fd,
		      mus_format("can't set srate %d, chans %d, bits %d, and encode %d on output %s (%s)",
				 srate, chans, bits, encode,
				 mus_audio_device_name(dev), dev_name));
  if ((int)info.play.channels != chans) 
    RETURN_ERROR_EXIT(MUS_AUDIO_CHANNELS_NOT_AVAILABLE, audio_fd,
		      mus_format("can't set output %s (%s) channels to %d",
				 mus_audio_device_name(dev), dev_name, chans));
  if (((int)info.play.precision != bits) || 
      ((int)info.play.encoding != encode)) 
    RETURN_ERROR_EXIT(MUS_AUDIO_FORMAT_NOT_AVAILABLE, audio_fd,
		      mus_format("can't set output %s (%s) format to %d bits, %d encode (%s)",
				 mus_audio_device_name(dev), dev_name,
				 bits, encode, 
				 mus_data_format_name(format)));
  /* man audio sez the play.buffer_size field is not currently supported */
  /* but since the default buffer size is 8180! we need ioctl(audio_fd, I_SETSIG, ...) */
  ioctl(audio_fd, I_FLUSH, FLUSHR);
  return(audio_fd);
}

int mus_audio_write(int line, char *buf, int bytes)
{
  if (write(line, buf, bytes) != bytes) 
    RETURN_ERROR_EXIT(MUS_AUDIO_WRITE_ERROR, -1,
		      mus_format("write error: %s", strerror(errno)));
  return(MUS_NO_ERROR);
}

int mus_audio_close(int line)
{
  write(line, (char *)NULL, 0);
  close(line);
  return(MUS_NO_ERROR);
}

int mus_audio_read(int line, char *buf, int bytes)
{
  int bytes_read, bytes_available, total = 0;
  char *curbuf;
  /* ioctl(line, AUDIO_DRAIN, NULL) */
  /* this seems to return 8-12 bytes fewer than requested -- perverse! */
  /* should I buffer data internally? */

  /* apparently we need to loop here ... */
  curbuf = buf;
  while (total < bytes)
    {
      ioctl(line, FIONREAD, &bytes_available);
      if (bytes_available > 0)
	{
	  if ((total + bytes_available) > bytes) bytes_available = bytes - total;
	  bytes_read = read(line, curbuf, bytes_available);
	  if (bytes_read > 0)
	    {
	      total += bytes_read;
	      curbuf = (char *)(buf + total);
	    }
	  /* else return anyway?? */
	}
    }
  return(MUS_NO_ERROR);
}

int mus_audio_open_input(int ur_dev, int srate, int chans, int format, int size)
{
  struct audio_info info;
  int indev, encode, bits, dev, audio_fd, err;
  char *dev_name;
  dev = MUS_AUDIO_DEVICE(ur_dev);
  encode = to_sun_format(format);
  bits = 8 * mus_bytes_per_sample(format);
  if (encode == -1) 
    RETURN_ERROR_EXIT(MUS_AUDIO_FORMAT_NOT_AVAILABLE, -1,
		      mus_format("format %d bits, %d encode (%s) not available",
				 bits, encode, 
				 mus_data_format_name(format)));
  if (getenv(AUDIODEV_ENV) != NULL) 
    dev_name = getenv(AUDIODEV_ENV); 
  else dev_name = DAC_NAME;
  if (dev != MUS_AUDIO_DUPLEX_DEFAULT)
    audio_fd = open(dev_name, O_RDONLY, 0);
  else audio_fd = open(dev_name, O_RDWR, 0);
  if (audio_fd == -1) 
    RETURN_ERROR_EXIT(MUS_AUDIO_CANT_OPEN, -1,
		      mus_format("can't open input %s: %s",
				 dev_name, strerror(errno)));
  AUDIO_INITINFO(&info);
  /*  ioctl(audio_fd, AUDIO_GETINFO, &info); */
  info.record.sample_rate = srate;
  info.record.channels = chans;
  err = ioctl(audio_fd, AUDIO_SETINFO, &info); 
  if (err == -1) 
    RETURN_ERROR_EXIT(MUS_AUDIO_CANT_OPEN, audio_fd,
		      mus_format("can't set srate %d and chans %d for input %s (%s)",
				 srate, chans,
				 dev_name, 
				 mus_audio_device_name(dev)));
  ioctl(audio_fd, AUDIO_GETINFO, &info);
  if (info.record.sample_rate != (unsigned int)srate) 
    mus_print("%s[%d]: sampling rate: %d != %d\n", 
	      __FILE__, __LINE__, 
	      info.record.sample_rate, srate);
  if (info.record.channels != (unsigned int)chans) 
    mus_print("%s[%d]: channels: %d != %d\n", 
	      __FILE__, __LINE__, 
	      info.record.channels, chans);

  info.record.precision = bits; /* was play, changed 10-Jul-03 thanks to Jürgen Keil */
  info.record.encoding = encode;
  err = ioctl(audio_fd, AUDIO_SETINFO, &info); 
  if (err == -1) 
    RETURN_ERROR_EXIT(MUS_AUDIO_CANT_OPEN, audio_fd,
		      mus_format("can't set bits %d, encode %d (format %s) for input %s (%s)",
				 bits, encode, mus_data_format_name(format),
				 dev_name, 
				 mus_audio_device_name(dev)));
  ioctl(audio_fd, AUDIO_GETINFO, &info);

  /* these cannot be OR'd */
  if (dev == MUS_AUDIO_LINE_IN) 
    indev = AUDIO_LINE_IN; 
  else
    {
      if (dev == MUS_AUDIO_CD) 
	indev = AUDIO_INTERNAL_CD_IN;
      else indev = AUDIO_MICROPHONE;
    }
  info.record.port = indev;
  err = ioctl(audio_fd, AUDIO_SETINFO, &info); 
  if (err == -1) 
    RETURN_ERROR_EXIT(MUS_AUDIO_CANT_WRITE, audio_fd,
		      mus_format("can't set record.port to %d for %s (%s)",
				 indev, dev_name, 
				 mus_audio_device_name(dev)));
  err = ioctl(audio_fd, AUDIO_GETINFO, &info);
  if (err == -1) 
    RETURN_ERROR_EXIT(MUS_AUDIO_CANT_READ, audio_fd,
		      mus_format("can't getinfo on input %s (%s, line: %d)",
				 dev_name, 
				 mus_audio_device_name(dev), audio_fd));
  else 
    {
      if ((int)info.record.port != indev) 
	RETURN_ERROR_EXIT(MUS_AUDIO_DEVICE_NOT_AVAILABLE, audio_fd,
			  mus_format("confusion in record.port: %d != %d (%s: %s)",
				     (int)info.record.port, indev,
				     dev_name, 
				     mus_audio_device_name(dev)));
      if ((int)info.record.channels != chans) 
	RETURN_ERROR_EXIT(MUS_AUDIO_CHANNELS_NOT_AVAILABLE, audio_fd,
			  mus_format("confusion in record.channels: %d != %d (%s: %s)",
				     (int)info.record.channels, chans,
				     dev_name, 
				     mus_audio_device_name(dev)));
      if (((int)info.record.precision != bits) || 
	  ((int)info.record.encoding != encode)) 
	RETURN_ERROR_EXIT(MUS_AUDIO_FORMAT_NOT_AVAILABLE, audio_fd,
			  mus_format("confusion in record.precision|encoding: %d != %d or %d != %d (%s: %s)",
				     (int)info.record.precision, bits,
				     (int)info.record.encoding, encode,
				     dev_name, 
				     mus_audio_device_name(dev)));
    }
  /* this may be a bad idea */
  info.record.buffer_size = size;
  err = ioctl(audio_fd, AUDIO_SETINFO, &info); 
  if (err == -1) 
    RETURN_ERROR_EXIT(MUS_AUDIO_CANT_WRITE, audio_fd,
		      mus_format("can't set buffer size to %d on input %s (%s)",
				 size,
				 dev_name, 
				 mus_audio_device_name(dev)));
  return(audio_fd);
}

int mus_audio_mixer_read(int ur_dev, int field, int chan, float *val) 
{
#ifndef AUDIO_DEV_AMD
  struct audio_device ad;
#else 
  int ad;
#endif
  int audio_fd, err;
  struct audio_info info;
  int dev, port;
  char *dev_name;
  dev = MUS_AUDIO_DEVICE(ur_dev);
  AUDIO_INITINFO(&info);
  if (getenv(AUDIODEV_ENV) != NULL) 
    dev_name = getenv(AUDIODEV_ENV); 
  else dev_name = DAC_NAME;
  audio_fd = open(dev_name, O_RDONLY | O_NONBLOCK, 0);
  if (audio_fd == -1) 
    {
      audio_fd = open("/dev/audioctl", O_RDONLY | O_NONBLOCK);
      if (audio_fd == -1) 
	RETURN_ERROR_EXIT(MUS_AUDIO_CANT_READ, -1,
			  mus_format("can't open %s or /dev/audioctl: %s",
				     dev_name, strerror(errno)));
      else dev_name = "/dev/audioctl";
    }
  err = ioctl(audio_fd, AUDIO_GETINFO, &info); 
  if (err == -1) 
    RETURN_ERROR_EXIT(MUS_AUDIO_CANT_READ, audio_fd,
		      mus_format("can't get %s (%s) info",
				 dev_name, 
				 mus_audio_device_name(dev)));
  if (field == MUS_AUDIO_PORT)
    {
      /* info.play|record have a field avail_ports */
      port = 1;
      if ((chan > port) && 
	  (info.record.avail_ports & AUDIO_MICROPHONE)) 
	{
	  val[port] = MUS_AUDIO_MICROPHONE; 
	  port++;
	}
      if ((chan > port) && 
	  (info.record.avail_ports & AUDIO_LINE_IN)) 
	{
	  val[port] = MUS_AUDIO_LINE_IN; 
	  port++;
	}
#ifndef AUDIO_DEV_AMD
      if ((chan > port) && 
	  (info.record.avail_ports & AUDIO_INTERNAL_CD_IN)) 
	{
	  /* this field lies -- there is no such port available on the Ultra */
	  err = ioctl(audio_fd, AUDIO_GETDEV, &ad); 
	  if (err == -1) 
	    RETURN_ERROR_EXIT(MUS_AUDIO_CANT_READ, audio_fd,
			      mus_format("can't get device info on %s (%s)",
					 dev_name, 
					 mus_audio_device_name(dev)));
	  if (((ad.version) && (strcmp(ad.version, "a") == 0)) || /* is it a SparcStation? */
	      ((ad.name) && (strcmp(ad.name, "SUNW,CS4231") == 0)))
	    {
	      val[port] = MUS_AUDIO_CD; 
	      port++;
	    }
	}
#endif
      if ((chan > port) && 
	  (info.play.avail_ports & AUDIO_SPEAKER)) 
	{
	  val[port] = MUS_AUDIO_SPEAKERS; 
	  port++;
	}
      if ((chan > port) &&
	  (info.play.avail_ports & AUDIO_LINE_OUT)) 
	{
	  val[port] = MUS_AUDIO_LINE_OUT; 
	  port++;
	}
      if ((chan > port) && 
	  (info.play.avail_ports & AUDIO_HEADPHONE)) 
	{
	  val[port] = MUS_AUDIO_DAC_OUT; 
	  port++;
	}
      val[0] = port - 1;
    }
  else
    {
      if (field == MUS_AUDIO_FORMAT)  /* this actually depends on the audio device */
        {
	  err = ioctl(audio_fd, AUDIO_GETDEV, &ad); /* SUNW, dbri|am79c30|CS4231|sbpro|sb16 */
	  /* Jurgen Keil's drivers use SUNW,CS4231, but the "real" names are:
	     "TOOLS,sbpci"           SoundBlaster PCI card
	     "TOOLS,EMU10Kx"         SoundBlaster Live! or Audigy
	     "TOOLS,i810"            Intel i8xx audio (and compatible)
	     "TOOLS,via686"          VIA 686 audio
	     "TOOLS,via8233"         VIA 8233 (and compatible)
	  */
	  if (err == -1) 
	    RETURN_ERROR_EXIT(MUS_AUDIO_CANT_READ, audio_fd,
			      mus_format("can't get device info on %s (%s)",
					 dev_name, 
					 mus_audio_device_name(dev)));
	  port = 1;
#ifndef AUDIO_DEV_AMD
	  if ((ad.name) && 
	      (strcmp(ad.name, "SUNW, am79c30") != 0))
#else
	  if (ad == AUDIO_DEV_AMD)
#endif
	    {
	      if (chan > port) val[port] = MUS_BSHORT; 
	      port++;
	    }
#ifndef AUDIO_DEV_AMD
	  if ((ad.name) && 
	      (strcmp(ad.name, "SUNW, sbpro") != 0) && 
	      (strcmp(ad.name, "SUNW, sb16") != 0))
	    {
	      if (chan > port) val[port] = MUS_ALAW; 
	      port++;
	    }
#endif
          if (chan > port) val[port] = MUS_MULAW;
#if MUS_LITTLE_ENDIAN
	  if (chan > (port + 1)) val[++port] = MUS_LSHORT;
#endif
          val[0] = port;
        }
      else
        {
          switch (dev)
            {
            case MUS_AUDIO_DEFAULT:
            case MUS_AUDIO_DAC_OUT:
            case MUS_AUDIO_SPEAKERS:
            case MUS_AUDIO_LINE_OUT:
              switch (field)
                {
                case MUS_AUDIO_AMP: 
		  /* who knows how this really works?  documentation is incomplete, actual behavior seems to be: */
		  if (chan == 0)
		    {
		      if (info.play.balance <= (AUDIO_RIGHT_BALANCE / 2))
			val[0] = info.play.gain / (float)(AUDIO_MAX_GAIN);
		      else val[0] = info.play.gain * (AUDIO_RIGHT_BALANCE - info.play.balance) / (float)(AUDIO_MAX_GAIN * (AUDIO_RIGHT_BALANCE / 2));
		    }
		  else
		    {
		      if (info.play.balance >= (AUDIO_RIGHT_BALANCE / 2))
			val[0] = info.play.gain / (float)(AUDIO_MAX_GAIN);
		      else val[0] = info.play.gain * info.play.balance / (float)(AUDIO_MAX_GAIN * (AUDIO_RIGHT_BALANCE / 2));
		    }
		  break;
                case MUS_AUDIO_CHANNEL: 
		  val[0] = 2; 
		  break;
		  /* appears to depend on data format (mulaw is mono) */
                case MUS_AUDIO_SRATE: 
		  val[0] = (float)info.play.sample_rate; 
		  break;
                default: 
		  RETURN_ERROR_EXIT(MUS_AUDIO_CANT_READ, audio_fd, 
				    mus_format("can't read %s field %d (%s)",
					       mus_audio_device_name(dev),
					       field, 
					       mus_audio_device_name(field)));
		  break;
                }
              break;
            case MUS_AUDIO_MICROPHONE:
            case MUS_AUDIO_LINE_IN:
            case MUS_AUDIO_DUPLEX_DEFAULT:
	    case MUS_AUDIO_CD:
              switch (field)
                {
                case MUS_AUDIO_AMP:
		  if (chan == 0)
		    {
		      if (info.record.balance <= (AUDIO_RIGHT_BALANCE / 2))
			val[0] = info.record.gain / (float)(AUDIO_MAX_GAIN);
		      else val[0] = info.record.gain * (AUDIO_RIGHT_BALANCE - info.record.balance) / (float)(AUDIO_MAX_GAIN * (AUDIO_RIGHT_BALANCE / 2));
		    }
		  else
		    {
		      if (info.record.balance >= (AUDIO_RIGHT_BALANCE / 2))
			val[0] = info.record.gain / (float)(AUDIO_MAX_GAIN);
		      else val[0] = info.record.gain * info.record.balance / (float)(AUDIO_MAX_GAIN * (AUDIO_RIGHT_BALANCE / 2));
		    }
		  break;

                case MUS_AUDIO_CHANNEL: 
		  val[0] = 1; 
		  break;
                case MUS_AUDIO_SRATE: 
		  val[0] = (float)(info.record.sample_rate); 
		  break;
		case MUS_AUDIO_IGAIN: 
		  val[0] = (float)(info.monitor_gain) / (float)AUDIO_MAX_GAIN; 
		  break;
                default: 
		  RETURN_ERROR_EXIT(MUS_AUDIO_CANT_READ, audio_fd,
					mus_format("can't read %s field %d (%s)",
						   mus_audio_device_name(dev),
						   field, 
						   mus_audio_device_name(field)));
		  break;
                }
              break;
            default: 
	      RETURN_ERROR_EXIT(MUS_AUDIO_CANT_READ, audio_fd,
				mus_format("can't read %s field %d (%s)",
					   mus_audio_device_name(dev),
					   field, 
					   mus_audio_device_name(field)));
	      break;
            }
        }
    }
  return(mus_audio_close(audio_fd));
}

int mus_audio_mixer_write(int ur_dev, int field, int chan, float *val) 
{
  struct audio_info info;
  int dev, balance, gain;
  float ratio, lc, rc;
  int audio_fd, err;
  char *dev_name;
  dev = MUS_AUDIO_DEVICE(ur_dev);
  AUDIO_INITINFO(&info);
  if (getenv(AUDIODEV_ENV) != NULL) 
    dev_name = getenv(AUDIODEV_ENV);
  else dev_name = DAC_NAME;
  audio_fd = open(dev_name, O_RDWR | O_NONBLOCK, 0);
  if (audio_fd == -1) 
    {
      audio_fd = open("/dev/audioctl", O_RDWR | O_NONBLOCK);
      if (audio_fd == -1) 
	RETURN_ERROR_EXIT(MUS_AUDIO_CANT_WRITE, -1,
			  mus_format("can't write fields of %s (or /dev/audioctl) (%s): %s",
				     dev_name, 
				     mus_audio_device_name(dev),
				     strerror(errno)));
      else dev_name = "/dev/audioctl";
    }
  err = ioctl(audio_fd, AUDIO_GETINFO, &info); 
  if (err == -1) 
    RETURN_ERROR_EXIT(MUS_AUDIO_CANT_READ, audio_fd,
		      mus_format("can't get %s (%s) info",
				 dev_name, 
				 mus_audio_device_name(dev)));
  switch (dev)
    {
    case MUS_AUDIO_DEFAULT:
    case MUS_AUDIO_DAC_OUT:
    case MUS_AUDIO_SPEAKERS:
    case MUS_AUDIO_LINE_OUT:
      switch (field)
        {
        case MUS_AUDIO_AMP: 
	  balance = info.play.balance;
	  gain = info.play.gain;
	  if (balance <= (AUDIO_RIGHT_BALANCE / 2))
	    {
	      lc = gain;
	      rc = gain * balance / (float)(AUDIO_RIGHT_BALANCE / 2);
	    }
	  else
	    {
	      lc = gain * (AUDIO_RIGHT_BALANCE - balance) / (float)(AUDIO_RIGHT_BALANCE / 2);
	      rc = gain;
	    }
	  if (chan == 0)
	    lc = AUDIO_MAX_GAIN * val[0];
	  else rc = AUDIO_MAX_GAIN * val[0];
	  if ((rc + lc) == 0)
	    info.play.gain = 0;
	  else
	    {
	      ratio = (float)rc / (float)(rc + lc);
	      info.play.balance = (unsigned char)(AUDIO_RIGHT_BALANCE * ratio);
	      if (rc > lc) 
		info.play.gain = (int)rc;
	      else info.play.gain = (int)lc;
	    }
	  break;
        case MUS_AUDIO_CHANNEL: 
	  info.play.channels = (int)val[0]; 
	  break;
	  /* amd device only mono */
        case MUS_AUDIO_SRATE: 
	  info.play.sample_rate = (int)val[0]; 
	  break;
        default: 
	  RETURN_ERROR_EXIT(MUS_AUDIO_CANT_WRITE, audio_fd,
			    mus_format("can't write %s field %d (%s)",
				       mus_audio_device_name(dev),
				       field, 
				       mus_audio_device_name(field)));
	  break;
        }
      break;
    case MUS_AUDIO_MICROPHONE:
      switch (field)
	{
        case MUS_AUDIO_AMP: 
	  info.record.gain = (int)(AUDIO_MAX_GAIN * val[0]);
	  info.record.balance = 0;
	  break;
        case MUS_AUDIO_CHANNEL: 
	  info.record.channels = (int)val[0];
	  break;
        case MUS_AUDIO_SRATE: 
	  info.record.sample_rate = (int)val[0];
	  break;
	case MUS_AUDIO_IGAIN: 
	  info.monitor_gain = (int)(AUDIO_MAX_GAIN * val[0]); 
	  break;
        default: 
	  RETURN_ERROR_EXIT(MUS_AUDIO_CANT_WRITE, audio_fd,
			    mus_format("can't write %s field %d (%s)",
				       mus_audio_device_name(dev),
				       field, 
				       mus_audio_device_name(field)));
	  break;
	}
      break;
    case MUS_AUDIO_LINE_IN:
    case MUS_AUDIO_DUPLEX_DEFAULT:
    case MUS_AUDIO_CD:
      switch (field)
        {
        case MUS_AUDIO_AMP: 
	  balance = info.record.balance;
	  gain = info.record.gain;
	  lc = gain * (float)(AUDIO_RIGHT_BALANCE - balance) / (float)AUDIO_RIGHT_BALANCE;
	  rc = gain - lc;
	  if (chan == 0)
	    lc = AUDIO_MAX_GAIN * val[0];
	  else rc = AUDIO_MAX_GAIN * val[0];
	  gain = (int)(rc + lc);
	  if (gain == 0)
	    info.record.gain = 0;
	  else
	    {
	      info.record.balance = (unsigned char)(AUDIO_RIGHT_BALANCE * ((float)rc / (float)(rc + lc)));
	      if (rc > lc) 
		info.record.gain = (int)rc;
	      else info.record.gain = (int)lc;
	    }
	  break;
        case MUS_AUDIO_CHANNEL: 
	  info.record.channels = (int)val[0]; 
	  break;
        case MUS_AUDIO_SRATE: 
	  info.record.sample_rate = (int)val[0]; 
	  break;
	case MUS_AUDIO_IGAIN: 
	  info.monitor_gain = (int)(AUDIO_MAX_GAIN * val[0]); 
	  break;
        default: 
	  RETURN_ERROR_EXIT(MUS_AUDIO_CANT_WRITE, audio_fd,
			    mus_format("can't write %s field %d (%s)",
				       mus_audio_device_name(dev),
				       field, 
				       mus_audio_device_name(field)));
	  break;
        }
      break;
    default: 
      RETURN_ERROR_EXIT(MUS_AUDIO_CANT_WRITE, audio_fd, 
			mus_format("can't write %s field %d (%s)",
				   mus_audio_device_name(dev),
				   field, 
				   mus_audio_device_name(field)));
      break;
    }
  err = ioctl(audio_fd, AUDIO_SETINFO, &info); 
  if (err == -1) 
    RETURN_ERROR_EXIT(MUS_AUDIO_CANT_WRITE, audio_fd,
		      mus_format("can't write %s field %d (%s) after explicit set",
				 mus_audio_device_name(dev),
				 field, 
				 mus_audio_device_name(field)));
  return(mus_audio_close(audio_fd));
}

/* pause can be implemented with play.pause and record.pause */

static char *sun_format_name(int format)
{
  switch (format)
    {
#ifdef AUDIO_ENCODING_ALAW
    case AUDIO_ENCODING_ALAW: return("alaw"); break;
#endif
#ifdef AUDIO_ENCODING_ULAW
    case AUDIO_ENCODING_ULAW: return("ulaw"); break;
#endif
#ifdef AUDIO_ENCODING_DVI
    case AUDIO_ENCODING_DVI: return("dvi adpcm"); break;
#endif
#ifdef AUDIO_ENCODING_LINEAR8
    case AUDIO_ENCODING_LINEAR8: return("linear"); break;
#else
  #ifdef AUDIO_ENCODING_PCM8
    case AUDIO_ENCODING_PCM8: return("linear"); break;
  #endif
#endif
#ifdef AUDIO_ENCODING_LINEAR
    case AUDIO_ENCODING_LINEAR: return("linear"); break;
#else
  #ifdef AUDIO_ENCODING_PCM16
    case AUDIO_ENCODING_PCM16: return("linear"); break;
  #endif
#endif
#ifdef AUDIO_ENCODING_NONE
    case AUDIO_ENCODING_NONE: return("not audio"); break; /* dbri interface configured for something else */
#endif      
    }
  return("unknown");
}

static char *sun_in_device_name(int dev)
{
  if (dev == AUDIO_MICROPHONE) return("microphone");
  if (dev == AUDIO_LINE_IN) return("line in");
  if (dev == AUDIO_INTERNAL_CD_IN) return("cd");
  if (dev == (AUDIO_MICROPHONE | AUDIO_LINE_IN)) return("microphone + line in");
  if (dev == (AUDIO_MICROPHONE | AUDIO_LINE_IN | AUDIO_INTERNAL_CD_IN)) return("microphone + line in + cd");
  if (dev == (AUDIO_MICROPHONE | AUDIO_INTERNAL_CD_IN)) return("microphone + cd");
  if (dev == (AUDIO_LINE_IN | AUDIO_INTERNAL_CD_IN)) return("line in + cd");
  return("unknown");
}

static char *sun_out_device_name(int dev)
{
  if (dev == AUDIO_SPEAKER) return("speakers");
  if (dev == AUDIO_LINE_OUT) return("line out");
  if (dev == AUDIO_HEADPHONE) return("headphones");
  if (dev == (AUDIO_SPEAKER | AUDIO_LINE_OUT)) return("speakers + line out");
  if (dev == (AUDIO_SPEAKER | AUDIO_LINE_OUT | AUDIO_HEADPHONE)) return("speakers + line out + headphones");
  if (dev == (AUDIO_SPEAKER | AUDIO_HEADPHONE)) return("speakers + headphones");
  if (dev == (AUDIO_LINE_OUT | AUDIO_HEADPHONE)) return("line out + headphones");
  return("unknown");
}

static char *sun_vol_name = NULL;
static char *sun_volume_name(float vol, int balance, int chans)
{
  if (sun_vol_name == NULL) sun_vol_name = (char *)CALLOC(LABEL_BUFFER_SIZE, sizeof(char));
  if (chans != 2)
    mus_snprintf(sun_vol_name, LABEL_BUFFER_SIZE, "%.3f", vol);
  else 
    {
      mus_snprintf(sun_vol_name, LABEL_BUFFER_SIZE, "%.3f %.3f",
		   vol * (float)(AUDIO_RIGHT_BALANCE - balance) / (float)AUDIO_RIGHT_BALANCE,
		   vol * (float)balance / (float)AUDIO_RIGHT_BALANCE);
    }
  return(sun_vol_name);
}

static void describe_audio_state_1(void) 
{
  struct audio_info info;
#ifndef AUDIO_DEV_AMD
  struct audio_device ad;
#else
  int ad;
#endif
  int audio_fd, err;
  char *dev_name;
  AUDIO_INITINFO(&info);
  if (getenv(AUDIODEV_ENV) != NULL) 
    dev_name = getenv(AUDIODEV_ENV); 
  else dev_name = DAC_NAME;
  audio_fd = open(dev_name, O_RDONLY | O_NONBLOCK, 0);
  if (audio_fd == -1)
    audio_fd = open("/dev/audioctl", O_RDONLY | O_NONBLOCK, 0);
  if (audio_fd == -1) return;
  err = ioctl(audio_fd, AUDIO_GETINFO, &info); 
  if (err == -1) return;
  err = ioctl(audio_fd, AUDIO_GETDEV, &ad); 
  if (err == -1) return;
  pprint(mus_audio_moniker());
  pprint("\n");
  mus_snprintf(audio_strbuf, PRINT_BUFFER_SIZE, "output: %s\n    srate: %d, vol: %s, chans: %d, format %d-bit %s\n",
	  sun_out_device_name(info.play.port),
	  info.play.sample_rate,
	  sun_volume_name((float)info.play.gain / (float)AUDIO_MAX_GAIN, info.play.balance, 2),
	  info.play.channels,
	  info.play.precision,
	  sun_format_name(info.play.encoding));
  pprint(audio_strbuf);
  mus_snprintf(audio_strbuf, PRINT_BUFFER_SIZE, "input: %s\n    srate: %d, vol: %s, chans: %d, format %d-bit %s\n",
	  sun_in_device_name(info.record.port),
	  info.record.sample_rate,
	  sun_volume_name((float)info.record.gain / (float)AUDIO_MAX_GAIN, info.record.balance, 2),
	  info.record.channels,
	  info.record.precision,
	  sun_format_name(info.record.encoding));
  pprint(audio_strbuf);
  mus_snprintf(audio_strbuf, PRINT_BUFFER_SIZE, "input->output vol: %.3f\n", (float)info.monitor_gain / (float)AUDIO_MAX_GAIN);
  pprint(audio_strbuf);
  if (info.play.pause) {mus_snprintf(audio_strbuf, PRINT_BUFFER_SIZE, "Playback is paused\n"); pprint(audio_strbuf);}
  if (info.record.pause) {mus_snprintf(audio_strbuf, PRINT_BUFFER_SIZE, "Recording is paused\n"); pprint(audio_strbuf);}
  if (info.output_muted) {mus_snprintf(audio_strbuf, PRINT_BUFFER_SIZE, "Output is muted\n"); pprint(audio_strbuf);}
#ifdef AUDIO_HWFEATURE_DUPLEX
  if (info.hw_features == AUDIO_HWFEATURE_DUPLEX) 
    {mus_snprintf(audio_strbuf, PRINT_BUFFER_SIZE, "Simultaneous play and record supported\n"); pprint(audio_strbuf);}
#endif
#if HAVE_SYS_MIXER_H
  {
    int i, num = 0, choice;
    #define LARGE_NUMBER 100
    am_sample_rates_t *sr = NULL;
    for (choice = 0; choice < 2; choice++)
      {
	for (num = 4; num < LARGE_NUMBER; num += 2) 
	  {
	    sr = (am_sample_rates_t *)
	      malloc(AUDIO_MIXER_SAMP_RATES_STRUCT_SIZE(num));
	    sr->num_samp_rates = num;
	    sr->type = (choice == 0) ? AUDIO_PLAY : AUDIO_RECORD;
	    err = ioctl(audio_fd, AUDIO_MIXER_GET_SAMPLE_RATES, sr);
	    if (sr->num_samp_rates <= num) break;
	    free(sr);
	    sr = NULL;
	  }
	if (sr)
	  {
	    mus_snprintf(audio_strbuf, PRINT_BUFFER_SIZE, "%s srates:", (choice == 0) ? "play" : "record"); 
	    pprint(audio_strbuf);
	    if (sr->type == MIXER_SR_LIMITS)
	      {
		mus_snprintf(audio_strbuf, PRINT_BUFFER_SIZE, " %d to %d", sr->samp_rates[0], sr->samp_rates[sr->num_samp_rates - 1]);
		pprint(audio_strbuf);
	      }
	    else
	      {
		for (i = 0; i < sr->num_samp_rates; i++)
		  {
		    mus_snprintf(audio_strbuf, PRINT_BUFFER_SIZE, " %d", sr->samp_rates[i]);
		    pprint(audio_strbuf);
		  }
	      }
	    pprint("\n");
	  }
	free(sr);
	sr = NULL;
      }
  }
#endif
  mus_audio_close(audio_fd);
}

static struct audio_info saved_info;

void mus_audio_save (void) 
{
  int audio_fd, err=-1;
  audio_fd = open("/dev/audioctl", O_RDONLY | O_NONBLOCK, 0);
  if (audio_fd != -1) 
    {
      err = ioctl(audio_fd, AUDIO_GETINFO, &saved_info); 
      close(audio_fd);
      if (err == -1)
	mus_print("can't get /dev/audioctl info to be saved: %s", strerror(errno));
    }
  else mus_print("can't save audio state via /dev/audioctl: %s", strerror(errno));
}

void mus_audio_restore (void) 
{
  int audio_fd, err;
  audio_fd = open("/dev/audioctl", O_WRONLY | O_NONBLOCK, 0);
  if (audio_fd != -1) 
    {
      err = ioctl(audio_fd, AUDIO_SETINFO, &saved_info); 
      close(audio_fd);
    }
}
#endif



/* ------------------------------- MACOS ----------------------------------------- */

#ifdef MACOS
#define AUDIO_OK

#include <Resources.h>
#include <Sound.h>
#include <SoundInput.h>

int mus_audio_systems(void) {return(1);} /* if Audiomedia, multiple? */
char *mus_audio_system_name(int system) {return("Mac");}

static int available_input_devices(void)
{
  unsigned char *devname;
  OSErr err;
  int i;
  Handle h;
  devname = (unsigned char *)CALLOC(PRINT_BUFFER_SIZE, sizeof(char));
  for (i = 1; i < 16; i++)
    {
      err = SPBGetIndexedDevice(i, devname, &h);
      if (err != noErr) break;
    }
  FREE(devname);
  return(i - 1);
}

static int input_device_is_connected(long refnum)
{
  OSErr err;
  short connected;
  err = SPBGetDeviceInfo(refnum, siDeviceConnected, &connected);
  return(connected);
}

static int input_device_get_source(long refnum)
{
  OSErr err;
  short source;
  err = SPBGetDeviceInfo(refnum, siInputSource, &source);
  return(source);
}

static int input_device_set_source(long refnum, short source)
{
  OSErr err;
  err = SPBSetDeviceInfo(refnum, siInputSource, &source);
  return((err == noErr) ? 0 : -1);
}

static int input_device_get_sources(long refnum, char **names)
{
  OSErr err;
  short sources;
  Handle h;
  err = SPBSetDeviceInfo(refnum, siInputSourceNames, &h);
  if (err == siUnknownInfoType) return(0);
  sources = (short)(*h);
  /* printf("%d sources: %s ", sources, strdup(p2cstr((unsigned char *)(*(h + 2))))); */
  /* need an example to test this silly thing */
  return((err == noErr) ? sources : -1);
}

static int input_device_channels (long refnum)
{
  OSErr err;
  short chans;
  err = SPBGetDeviceInfo(refnum, siChannelAvailable, &chans);
  if (err == noErr) return(chans);
  return(-1);
}

static int input_device_get_async(long refnum)
{
  OSErr err;
  short async;
  err = SPBGetDeviceInfo(refnum, siAsync, &async);
  if (err == noErr) return(async);
  return(-1);
}

static char *input_device_name(long refnum)
{
  char *name;
  OSErr err;  
  name = (char *)CALLOC(PRINT_BUFFER_SIZE, sizeof(char));
  err = SPBGetDeviceInfo(refnum, siDeviceName, name);
  if (err == noErr) return(name);
  FREE(name);
  return(NULL);
}

static float input_device_get_gain(long refnum)
{
  OSErr err;
  unsigned long val;
  err = SPBGetDeviceInfo(refnum, siInputGain, &val);
  /* val is a "4 byte fixed value between .5 and 1.5"!! */
  if (err == noErr)
    return((float)val / 65536.0);
  return(-1);
}

static int input_device_set_gain(long refnum, float gain)
{
  OSErr err;
  int val;
  val = ((int)(gain * 65536));
  err = SPBSetDeviceInfo(refnum, siInputGain, &val);
  return((err == noErr) ? 0 : -1); 
}

static int input_device_get_channels(long refnum)
{
  OSErr err;
  short chans;
  err = SPBGetDeviceInfo(refnum, siNumberChannels, &chans);
  return((err == noErr) ? chans : -1); 
}

static int input_device_set_channels(long refnum, short chans)
{
  OSErr err;
  err = SPBSetDeviceInfo(refnum, siNumberChannels, &chans);
  return((err == noErr) ? 0 : -1); 
}

static int input_device_get_quality(long refnum)
{
  OSErr err;
  OSType val;
  err = SPBGetDeviceInfo(refnum, siRecordingQuality, &val);
  if (err == noErr)
    {
      if (val == siCDQuality) return(3);
      if (val == siBestQuality) return(2);
      if (val == siBetterQuality) return(1);
      if (val == siGoodQuality) return(0);
    }
  return(-1);
}

static int input_device_set_quality(long refnum, int quality)
{
  OSErr err;
  OSType val;
  if (quality == 3) val = siCDQuality;
  else if (quality == 2) val = siBestQuality;
  else if (quality == 1) val = siBetterQuality;
  else val = siGoodQuality;
  err = SPBSetDeviceInfo(refnum, siRecordingQuality, &val);
  return((err == noErr) ? 0 : -1); 
}

static int input_device_get_srate(long refnum)
{
  OSErr err;
  unsigned long fixed_srate;
  err = SPBGetDeviceInfo(refnum, siSampleRate, &fixed_srate);
  if (err == noErr) return(fixed_srate >> 16);
  return(-1);
}

static int input_device_set_srate(long refnum, int srate)
{
  OSErr err;
  unsigned long fixed_srate;
  fixed_srate = (unsigned long)(srate * 65536);
  err = SPBSetDeviceInfo(refnum, siSampleRate, &fixed_srate);
  return((err == noErr) ? 0 : -1); 
}

static int input_device_get_sample_size(long refnum)
{
  OSErr err;
  short size;
  err = SPBGetDeviceInfo(refnum, siSampleSize, &size);
  if (err == noErr) return(size);
  return(-1);
}

static int input_device_set_sample_size(long refnum, short size)
{
  OSErr err;
  err = SPBSetDeviceInfo(refnum, siSampleSize, &size);
  return((err == noErr) ? 0 : -1); 
}

static int input_device_get_signed(long refnum)
{ /* 0 = unsigned */
  OSErr err;
  short sign;
  err = SPBGetDeviceInfo(refnum, siSampleRate, &sign);
  if (err == noErr) return(sign);
  return(-1);
}

static int input_device_set_signed(long refnum, short sign)
{
  OSErr err;
  err = SPBSetDeviceInfo(refnum, siSampleRate, &sign);
  return((err == noErr) ? 0 : -1); 
}

static int input_device_sample_rates(long refnum, int *range, int *rates)
{
  unsigned short num;
  int i, j;
  unsigned long ptr;
  OSErr err;
  unsigned char pp[6];                                            /* can't depend on C compiler to pack a struct correctly here */
  num = 0;
  err = SPBGetDeviceInfo(refnum, siSampleRateAvailable, pp);
  if (err == noErr)
    {
      num = pp[1] + (pp[0] << 8);                                 /* unsigned short is first element */
      if (num == 0) {(*range) = 1; num = 2;} else (*range) = 0;
      ptr = pp[5] + (pp[4] << 8) + (pp[3] << 16) + (pp[2] << 24); /* pointer to "fixed" table is second element */
      for (i = 0, j = 0; i < num; i++, j += 4)                          
        rates[i] = (*(unsigned short *)(j + (*(int *)ptr)));      /* ignore fraction -- this is dubious code */
    }
  return(num);
}

static int input_device_sample_sizes(long refnum, int *sizes)
{
  unsigned short num;
  int i, j;
  unsigned long ptr;
  OSErr err;
  unsigned char pp[6];
  num = 0;
  err = SPBGetDeviceInfo(refnum, siSampleSizeAvailable, pp);
  if (err == noErr)
    {
      num = pp[1] + (pp[0] << 8); 
      ptr = pp[5] + (pp[4] << 8) + (pp[3] << 16) + (pp[2] << 24);
      for (i = 0, j = 0; i < num; i++, j += 2) 
	sizes[i] = (*(unsigned short *)(j + (*(int *)ptr)));
    }
  return(num);
}

static int input_device_get_gains(long refnum, float *gains)
{
  OSErr err;
  long ptr[2];
  err = SPBGetDeviceInfo(refnum, siStereoInputGain, ptr);
  if (err == noErr)
    {
      gains[0] = (float)ptr[0] / 65536.0;
      gains[1] = (float)ptr[1] / 65536.0;
    }
  else return(-1);
  return(0);
}

static int input_device_set_gains(long refnum, float *gains)
{
  OSErr err;
  long val[2];
  val[0] = gains[0] * 65536;
  val[1] = gains[1] * 65536;
  err = SPBSetDeviceInfo(refnum, siStereoInputGain, val);
  return((err == noErr) ? 0 : -1); 
}

char *mus_audio_moniker(void)
{
  NumVersion nv;
  if (version_name == NULL) version_name = (char *)CALLOC(LABEL_BUFFER_SIZE, sizeof(char));
  nv = SndSoundManagerVersion();
  mus_snprintf(version_name, LABEL_BUFFER_SIZE, "Mac audio: %d.%d.%d.%d\n", nv.majorRev, nv.minorAndBugRev, nv.stage, nv.nonRelRev);
  return(version_name);
}

static char *quality_names[5] = {"indescribable", "bad", "not bad", "ok", "good"};

static void describe_audio_state_1(void) 
{
  long response;
  NumVersion nv;
  OSErr err;
  int vals[64];
  float gains[2];
  int have_IO_mgr = 0, have_input_device = 0;
  unsigned char *devname = NULL;
  int i, j, devs, rates, range, sizes, connected;
  long refnum;
  Handle h;
  nv = SndSoundManagerVersion();
  mus_snprintf(audio_strbuf, PRINT_BUFFER_SIZE, "Sound Manager: %d.%d.%d.%d\n", 
	  nv.majorRev, nv.minorAndBugRev, nv.stage, nv.nonRelRev);
  pprint(audio_strbuf);
  err = Gestalt(gestaltSoundAttr, &response);
  have_IO_mgr = (response & gestaltSoundIOMgrPresent);
  have_input_device = (response & gestaltHasSoundInputDevice);
  if (have_IO_mgr)
    {
      nv = SPBVersion();
      mus_snprintf(audio_strbuf, PRINT_BUFFER_SIZE, "Sound Input Manager: %d.%d.%d.%d\n", 
	      nv.majorRev, nv.minorAndBugRev, nv.stage, nv.nonRelRev); 
      pprint(audio_strbuf);
     }
  if (!have_IO_mgr) pprint("Sound IO Manager absent!\n");
  if (!(response & gestaltBuiltInSoundInput)) pprint("no built-in input device!\n");
  if (!have_input_device) pprint("no input devices available!\n");
  if (!(response & gestaltSndPlayDoubleBuffer)) pprint("double buffering not supported!\n");
  if (response & gestalt16BitAudioSupport) pprint("has 16-bit audio ");
  if (response & gestalt16BitSoundIO) pprint("has 16-bit sound ");
  if (response & gestaltStereoInput) pprint("has stereo input\n");
  if (response & gestaltPlayAndRecord) pprint("can play and record simultaneously\n");
  if (response & gestaltMultiChannels) pprint("has multichannel support\n");
  GetSysBeepVolume(&response);
  mus_snprintf(audio_strbuf, PRINT_BUFFER_SIZE, "beep vol: %.3f %.3f\n", 
	  ((float)(response >> 16)) / 255.0, 
	  ((float)(response & 0xffff)) / 255.0); 
  pprint(audio_strbuf);
  GetDefaultOutputVolume(&response);
  mus_snprintf(audio_strbuf, PRINT_BUFFER_SIZE, "output vol: %.3f %.3f\n", 
	  ((float)(response >> 16)) / 255.0, 
	  ((float)(response & 0xffff)) / 255.0); 
  pprint(audio_strbuf);
  if ((have_IO_mgr) && 
      (have_input_device))
    { 
      devs = available_input_devices();
      if (devs > 0)
        {
          devname = (unsigned char *)CALLOC(PRINT_BUFFER_SIZE, sizeof(char));
          mus_snprintf(audio_strbuf, PRINT_BUFFER_SIZE, "input device%s:\n", (devs > 1) ? "s" : ""); 
	  pprint(audio_strbuf);
          for (i = 1; i <= devs; i++)
            {
              for (i = 1; i <= devs; i++)
                {
                  err = SPBGetIndexedDevice(i, devname, &h);
                  if (err == noErr)
                    {
                      err = SPBOpenDevice(devname, siWritePermission, &refnum);
                      if (err == noErr)
                        {
                          range = input_device_get_source(refnum);
                          connected = input_device_is_connected(refnum);
                          mus_snprintf(audio_strbuf, PRINT_BUFFER_SIZE, "  %s: %s%s",
                                  (*devname) ? devname : (unsigned char *)"un-named",
                                  ((input_device_get_async(refnum) == 1) ? "(async) " : ""),
                                  ((connected == siDeviceIsConnected) ? "" : 
                                   ((connected == siDeviceNotConnected) ? 
                                    "(not connected )" : "(might not be connected)")));
                        pprint(audio_strbuf);
                        if (range == 0) pprint("\n");
                        else
                                {
                                mus_snprintf(audio_strbuf, PRINT_BUFFER_SIZE, " (source: %d)\n", range);
                                pprint(audio_strbuf);
                                }
                                mus_snprintf(audio_strbuf, PRINT_BUFFER_SIZE, "    %d chans available, %d active\n",
                                  input_device_channels(refnum),
                                  input_device_get_channels(refnum));
                          pprint(audio_strbuf);

                          /* input_device_get_sources(refnum, NULL); */

                          range = 0;
                          rates = input_device_sample_rates(refnum, &range, vals);
                          if (rates > 1)
                            {
                              mus_snprintf(audio_strbuf, PRINT_BUFFER_SIZE, "    srates available:"); 
                              pprint(audio_strbuf);
                              if (range)
                                {
				  mus_snprintf(audio_strbuf, PRINT_BUFFER_SIZE, "%d to %d", vals[0], vals[1]); 
				  pprint(audio_strbuf);
				}
                              else
                                for (j = 0; j < rates; j++) 
				  {
				    mus_snprintf(audio_strbuf, PRINT_BUFFER_SIZE, " %d", vals[j]); 
				    pprint(audio_strbuf);
				  }
                              mus_snprintf(audio_strbuf, PRINT_BUFFER_SIZE, ", current srate: %d\n",
                                      input_device_get_srate(refnum));
                              pprint(audio_strbuf);
                            }
                          else 
                            {
                              mus_snprintf(audio_strbuf, PRINT_BUFFER_SIZE, "    srate: %d\n", input_device_get_srate(refnum)); 
                              pprint(audio_strbuf);
                            }
                          err = input_device_get_quality(refnum);
                          if (err != -1) 
                            {
                              mus_snprintf(audio_strbuf, PRINT_BUFFER_SIZE, "    quality: %s\n",
				      quality_names[1 + input_device_get_quality(refnum)]);
                              pprint(audio_strbuf);
                            }
                          input_device_get_gains(refnum, gains);
                          mus_snprintf(audio_strbuf, PRINT_BUFFER_SIZE, "    gain: %.3f (%.3f %.3f)\n    sample: %s %d bits",
                                  input_device_get_gain(refnum), 
				  gains[0], gains[1],
                                  ((input_device_get_signed(refnum)) ? "signed" : "unsigned"),
                                  input_device_get_sample_size(refnum));
                          pprint(audio_strbuf);
                          sizes = input_device_sample_sizes(refnum, vals);
                          if (sizes > 0)
                            {
                              mus_snprintf(audio_strbuf, PRINT_BUFFER_SIZE, " (%d", vals[0]); 
			      pprint(audio_strbuf);
                              for (j = 1; j < sizes; j++) 
                                {
				  mus_snprintf(audio_strbuf, PRINT_BUFFER_SIZE, ", %d", vals[j]); 
				  pprint(audio_strbuf);
				}
                              pprint(" bit samples available)");
                            }
                          pprint("\n");
                          SPBCloseDevice(refnum);
                        }
                    }
                }
            }
          FREE(devname);
        }
    }
}

#define BUFFER_FILLED 1
#define BUFFER_EMPTY 2

#define SOUND_UNREADY 0
#define SOUND_INITIALIZED 1
#define SOUND_RUNNING 2

#define INPUT_LINE 1
#define OUTPUT_LINE 2

static int buffer_size = 1024;
static SndDoubleBufferPtr *db = NULL;
static SndDoubleBufferHeader dh;
static SndChannelPtr chan;
static int *db_state = NULL;
static int sound_state = 0;
static int current_chans = 1;
static int current_datum_size = 2;
static int current_srate = 22050;
static int current_buf = 0;
static long in_ref = -1;
static SPB spb;

#define DATA_EMPTY 0
#define DATA_READY 1
#define DATA_WRITTEN 2
static int data_status = DATA_EMPTY;
static int data_bytes = 0;
static char *data = NULL;

/* for CLM */
#ifdef MCL_PPC
  static void reset_db(void) 
    {
      db = NULL; 
      db_state = NULL;
      data = NULL;
      data_bytes = 0;
    }
#endif


static pascal void nextbuffer(SndChannelPtr cp, SndDoubleBufferPtr db)
{
  db_state[current_buf] = BUFFER_EMPTY;
}

#define RETURN_ERROR_EXIT(Error_Type, Ur_Error_Message) \
  do { char *Error_Message; Error_Message = Ur_Error_Message; \
    if (Error_Message) \
      {MUS_STANDARD_ERROR(Error_Type, Error_Message); FREE(Error_Message);} \
    else MUS_STANDARD_ERROR(Error_Type, mus_error_to_string(Error_Type)); \
    return(MUS_ERROR); \
  } while (0)

int mus_audio_open_output(int ur_dev, int srate, int chans, int format, int size) 
{
  OSErr err;
  int dev;
  dev = MUS_AUDIO_DEVICE(ur_dev);
  if (!db) db = (SndDoubleBufferPtr *)CALLOC(2, sizeof(SndDoubleBufferPtr));
  if (!db_state) db_state = (int *)CALLOC(2, sizeof(int));
  chan = nil;
  err = SndNewChannel(&chan, sampledSynth, 0, nil);
  if (err != noErr) 
    RETURN_ERROR_EXIT(MUS_AUDIO_DEVICE_NOT_AVAILABLE,
		      mus_format("can't get new output channel for %d (%s): error %d", /* geez-louise!! Is anything dumber than a goddamn Mac? */
				 dev, 
				 mus_audio_device_name(dev), err));
  dh.dbhNumChannels = chans;
  current_chans = chans;
  if (format == MUS_UBYTE) 
    {
      dh.dbhSampleSize = 8; 
      current_datum_size = 1;
    }
  else 
    {
      dh.dbhSampleSize = 16;
      current_datum_size = 2;
    }
  dh.dbhCompressionID = 0; 
  dh.dbhPacketSize = 0; 
  dh.dbhSampleRate = (srate << 16);
  dh.dbhDoubleBack = NewSndDoubleBackProc(nextbuffer);
  if (size <= 0) buffer_size = 1024; else buffer_size = size;
  db[0] = (SndDoubleBufferPtr)CALLOC(sizeof(SndDoubleBuffer) + buffer_size, sizeof(char));
  if ((db[0] == nil) || (MemError() != 0)) 
    {
      SndDisposeChannel(chan, 0); 
      RETURN_ERROR_EXIT(MUS_AUDIO_SIZE_NOT_AVAILABLE,
			mus_format("can't allocate output buffer, size %d, for %d (%s)",
				   buffer_size, dev, 
				   mus_audio_device_name(dev)));
    }
  dh.dbhBufferPtr[0] = db[0];   
  db[0]->dbNumFrames = 0;
  db[0]->dbFlags = 0;
  db_state[0] = BUFFER_EMPTY;
  db[1] = (SndDoubleBufferPtr)CALLOC(sizeof(SndDoubleBuffer) + buffer_size, sizeof(char));
  if ((db[1] == nil) || (MemError() != 0)) 
    {
      FREE(db[0]); 
      SndDisposeChannel(chan, 0); 
      RETURN_ERROR_EXIT(MUS_AUDIO_SIZE_NOT_AVAILABLE,
			mus_format("can't allocate output buffer, size %d, for %d (%s)",
				   buffer_size, dev, 
				   mus_audio_device_name(dev)));
    }
  dh.dbhBufferPtr[1] = db[1];   
  db[1]->dbNumFrames = 0;
  db[1]->dbFlags = 0;
  db_state[1] = BUFFER_EMPTY;
  sound_state = SOUND_INITIALIZED;
  current_buf = 0;
  return(OUTPUT_LINE);
}

static OSErr fill_buffer(int dbi, char *inbuf, int instart, int bytes)
{
  int i, j;
  OSErr err;
  err = noErr;
  for (i = instart, j = 0; j < bytes; j++, i++) 
    db[dbi]->dbSoundData[j] = inbuf[i];
  db_state[dbi] = BUFFER_FILLED;
  db[dbi]->dbFlags = (db[dbi]->dbFlags | dbBufferReady);
  db[dbi]->dbNumFrames = (bytes / (current_chans * current_datum_size));
  if ((sound_state == SOUND_INITIALIZED) && (dbi == 1))
    {
      sound_state = SOUND_RUNNING;
      err = SndPlayDoubleBuffer(chan, &dh);
    }
  return(err);
}

static OSErr wait_for_empty_buffer(int buf)
{
  SCStatus Stats;
  OSErr err;
  err = noErr;
  while (db_state[buf] != BUFFER_EMPTY)
    {
      err = SndChannelStatus(chan, sizeof(Stats), &Stats);
      if ((err != noErr) || 
	  (!(Stats.scChannelBusy))) 
	break;
    }
  return(err);
}

int mus_audio_write(int line, char *buf, int bytes) 
{
  OSErr err;
  int lim, leftover, start;
  if (line != OUTPUT_LINE) 
    RETURN_ERROR_EXIT(MUS_AUDIO_CANT_WRITE,
		      mus_format("write error: line %d != %d",
				 line, OUTPUT_LINE));
  leftover = bytes;
  start = 0;
  while (leftover > 0)
    {
      lim = leftover;
      if (lim > buffer_size) lim = buffer_size;
      leftover -= lim;
      err = wait_for_empty_buffer(current_buf);
      if (err != noErr) 
	RETURN_ERROR_EXIT(MUS_AUDIO_CANT_WRITE,
			  mus_format("write error during wait on line %d: error %d",
				     line, err));
      err = fill_buffer(current_buf, buf, start, lim);
      if (err != noErr) 
	RETURN_ERROR_EXIT(MUS_AUDIO_CANT_WRITE,
			  mus_format("write error during fill on line %d: error %d",
				     line, err));
      start += lim;
      current_buf++;
      if (current_buf > 1) current_buf = 0;
    }
  return(MUS_NO_ERROR);
}

int mus_audio_close(int line) 
{
  OSErr err;
  int i;
  if (line == OUTPUT_LINE)
    {
      /* fill with a few zeros, wait for empty flag */
      if (sound_state != SOUND_UNREADY)
        {
          wait_for_empty_buffer(current_buf);
          for (i = 0; i < 128; i++) 
	    db[current_buf]->dbSoundData[i] = 0;
          db[current_buf]->dbFlags = (db[current_buf]->dbFlags | dbBufferReady | dbLastBuffer);
          db[current_buf]->dbNumFrames = (128 / (current_chans * current_datum_size));
          wait_for_empty_buffer(current_buf);
          FREE(db[0]); 
	  db[0] = NULL;
          FREE(db[1]); 
	  db[1] = NULL;
          db_state[0] = BUFFER_EMPTY;
          db_state[1] = BUFFER_EMPTY;
          sound_state = SOUND_UNREADY;
          err = SndDisposeChannel(chan, 0);
	  /* this is the line that forced me to use FREE/CALLOC throughout! */
          if (err != noErr) 
	    RETURN_ERROR_EXIT(MUS_AUDIO_CANT_CLOSE,
			      mus_format("can't close output: error %d",
					 err));
        }
    }
  else
    {
      if (line == INPUT_LINE)
        {
          if (in_ref != -1)
            {
              data_status = DATA_EMPTY;
              SPBStopRecording(in_ref);
              if (spb.bufferPtr) FREE(spb.bufferPtr);
              SPBCloseDevice(in_ref);
              in_ref = -1;
            }
        }
      else 
	RETURN_ERROR_EXIT(MUS_AUDIO_CANT_CLOSE,
			  mus_format("can't close unrecognized line: %d",
				     line));
    }
  return(MUS_NO_ERROR);
}

static void read_callback(SPB *spb)
{
  int i, lim;
  if (data_status != DATA_EMPTY)
    {
      if (data_bytes > spb->bufferLength) 
	lim = spb->bufferLength; 
      else lim = data_bytes;
      for (i = 0; i < lim; i++) 
	data[i] = spb->bufferPtr[i]; 
      spb->bufferLength = data_bytes;
      SPBRecord(spb, TRUE);
      data_status = DATA_WRITTEN;
    }
}

int mus_audio_open_input(int ur_dev, int srate, int chans, int format, int size) 
{
  OSErr err;
  short source;
  int dev;
  dev = MUS_AUDIO_DEVICE(ur_dev);
  data_status = DATA_EMPTY;
  if (size <= 0) size = 1024;
  if (in_ref != -1) 
    RETURN_ERROR_EXIT(MUS_AUDIO_DEVICE_NOT_AVAILABLE,
		      mus_format("previous input not closed? line: %d",
				 in_ref));
  err = SPBOpenDevice((unsigned char *)"", siWritePermission, &in_ref);
  if (err != noErr) 
    RETURN_ERROR_EXIT(MUS_AUDIO_CANT_OPEN,
		      mus_format("can't open input %d (%s): error %d",
				 dev, mus_audio_device_name(dev), err));
  spb.inRefNum = in_ref;
  spb.count = size;
  source = 3; /* the microphone ?? (2: CD, 4: modem, 0: none) -- nowhere is this documented! */
  input_device_set_source(in_ref, source);
  input_device_set_srate(in_ref, srate);
  input_device_set_channels(in_ref, (short)chans);
  input_device_set_sample_size(in_ref, (format == MUS_BSHORT) ? 2 : 1);
  input_device_set_signed(in_ref, (format == MUS_BSHORT) ? 1 : 0);
  spb.milliseconds = (int)((float)(size * 1000) / (float)(((format == MUS_BSHORT) ? 2 : 1) * srate));
  spb.bufferLength = size;
  spb.bufferPtr = (char *)CALLOC(size, sizeof(char));
  spb.completionRoutine = NewSICompletionProc(read_callback);
  err = SPBRecord(&spb, TRUE);
  return(INPUT_LINE);
}

int mus_audio_read(int line, char *buf, int bytes) 
{
  OSErr err;
  unsigned long total_samps, num_samps, total_msecs, num_msecs;
  short level, status;
  if (line != INPUT_LINE) 
    RETURN_ERROR_EXIT(MUS_AUDIO_CANT_READ,
		      mus_format("can't read from unrecognized line: %d",
				 line));
  data_status = DATA_READY;
  data_bytes = bytes;
  data = buf;
  while (data_status == DATA_READY)
    {
      err = SPBGetRecordingStatus(in_ref, &status, &level, &total_samps, &num_samps, &total_msecs, &num_msecs);
      if ((err != noErr) || (status <= 0)) break; /* not necessarily an error */
    }
  return(0);
}

int mus_audio_mixer_read(int ur_dev, int field, int chan, float *val) 
{
  OSErr err;
  long response;
  int dev, our_err = MUS_NO_ERROR;
  float in_val[2];
  int our_open = 0;
  dev = MUS_AUDIO_DEVICE(ur_dev);
  switch (field)
    {
    case MUS_AUDIO_CHANNEL:
      val[0] = 2;
      break;
    case MUS_AUDIO_PORT:
      val[0] = 2; 
      if (chan > 1) val[1] = MUS_AUDIO_MICROPHONE; 
      if (chan > 2) val[2] = MUS_AUDIO_DAC_OUT;
      break;
    case MUS_AUDIO_FORMAT:
      val[0] = 2; 
      if (chan > 1) val[1] = MUS_BSHORT;
      if (chan > 2) val[2] = MUS_UBYTE;
      break;
    default:
      switch (dev)
        {
        case MUS_AUDIO_DEFAULT:
        case MUS_AUDIO_DAC_OUT:
        case MUS_AUDIO_SPEAKERS:
        case MUS_AUDIO_LINE_OUT:
          switch (field)
            {
            case MUS_AUDIO_AMP: 
              GetDefaultOutputVolume(&response);
              if (chan == 0)
                val[0] = ((float)(response >> 16)) / 255.0;
              else val[0] = ((float)(response & 0xffff)) / 255.0;
              break;
            case MUS_AUDIO_CHANNEL: 
	      val[0] = 2;
	      break;
            case MUS_AUDIO_SRATE: 
	      val[0] = current_srate;
	      break;
            }
          break;
        case MUS_AUDIO_MICROPHONE:
        case MUS_AUDIO_LINE_IN:
          if (in_ref == -1)
            {
              err = SPBOpenDevice((const unsigned char *)"", siWritePermission, &in_ref);
              if (err != noErr) 
		RETURN_ERROR_EXIT(MUS_AUDIO_CANT_OPEN,
				  mus_format("can't open %d (%s): error %d",
					     dev, 
					     mus_audio_device_name(dev), err));
              our_open = 1;
            }
          switch (field)
            {
            case MUS_AUDIO_AMP:
              err = input_device_get_gains(in_ref, in_val);
              if (chan == 0) val[0] = in_val[0]; else val[0] = in_val[1];
              break;
            case MUS_AUDIO_CHANNEL:
              val[0] = input_device_get_channels(in_ref);
              break;
            case MUS_AUDIO_SRATE:
              val[0] = input_device_get_srate(in_ref);
              break;
            default:
	      our_err = MUS_ERROR;
	      break;
            }
          if (our_open)
            {
              SPBCloseDevice(in_ref);
              in_ref = -1;
            }
          break;
        default: 
	  our_err = MUS_ERROR;
	  break;
        }
    }
  if (our_err == MUS_ERROR)
    RETURN_ERROR_EXIT(MUS_AUDIO_CANT_READ,
		      mus_format("can't read %s field of device %d (%s)",
				 mus_audio_device_name(field),
				 dev, 
				 mus_audio_device_name(dev)));
  return(MUS_NO_ERROR);
}

int mus_audio_mixer_write(int ur_dev, int field, int chan, float *val) 
{
  OSErr err;
  float out_val[2];
  long curval, newval;
  int amp, our_open, dev, our_err = MUS_NO_ERROR;
  dev = MUS_AUDIO_DEVICE(ur_dev);
  switch (dev)
    {
    case MUS_AUDIO_DEFAULT:
    case MUS_AUDIO_DAC_OUT:
    case MUS_AUDIO_SPEAKERS:
    case MUS_AUDIO_LINE_OUT:
      switch (field)
        {
        case MUS_AUDIO_AMP: 
          amp = (int)(255 * val[0]);
          GetDefaultOutputVolume(&curval);
          if (chan == 0)
            newval = ((curval & 0xffff0000) | (amp & 0xffff));
          else newval = (((amp<<16) & 0xffff0000) | (curval & 0xffff));
          SetDefaultOutputVolume(newval);
          break;
        case MUS_AUDIO_CHANNEL: 
        case MUS_AUDIO_SRATE: break;
        default: 
	  our_err = MUS_ERROR;
	  break;
        }
      break;
    case MUS_AUDIO_MICROPHONE:
    case MUS_AUDIO_LINE_IN:
      if (in_ref == -1)
        {
          err = SPBOpenDevice((const unsigned char *)"", siWritePermission, &in_ref);
          if (err != noErr) 
	    RETURN_ERROR_EXIT(MUS_AUDIO_CANT_OPEN,
			      mus_format("can't open %d (%s): error %d",
					 dev, 
					 mus_audio_device_name(dev), err));
          our_open = 1;
        }
      switch (field)
        {
        case MUS_AUDIO_AMP:
          input_device_get_gains(in_ref, out_val);
          if (chan == 0) out_val[0] = val[0]; else out_val[1] = val[0];
          err = input_device_set_gains(in_ref, out_val);
          break;
        case MUS_AUDIO_CHANNEL:
          err = input_device_set_channels(in_ref, (int)val[0]);
          break;
        case MUS_AUDIO_SRATE:
          err = input_device_set_srate(in_ref, (int)val[0]);
          break;
        default:
	  our_err = MUS_ERROR;
	  break;
        }
      if (our_open)
        {
          SPBCloseDevice(in_ref);
          in_ref = -1;
        }
      break;
    default: 
      our_err = MUS_ERROR;
      break;
    }
  if (our_err == MUS_ERROR)
    RETURN_ERROR_EXIT(MUS_AUDIO_CANT_WRITE,
		      mus_format("can't set %s field of device %d (%s)",
				 mus_audio_device_name(field),
				 dev, 
				 mus_audio_device_name(dev)));
  return(MUS_NO_ERROR);
}

static long beep_vol = 0, out_vol = 0;

void mus_audio_save(void) 
{
  GetDefaultOutputVolume(&out_vol);
  GetSysBeepVolume(&beep_vol);
}

void mus_audio_restore(void) 
{
  SetDefaultOutputVolume(out_vol);
  SetSysBeepVolume(beep_vol);
}

int mus_audio_initialize(void) 
{
  return(MUS_NO_ERROR);
}

#endif


/* ------------------------------- HPUX ----------------------------------------- */

/* if this is basically the same as the Sun case with different macro names,
 * then it could perhaps be updated to match the new Sun version above --
 * Sun version changed 28-Jan-99
 */

#if defined(HPUX) && (!(defined(AUDIO_OK)))
#define AUDIO_OK
#include <sys/audio.h>

#define RETURN_ERROR_EXIT(Error_Type, Audio_Line, Ur_Error_Message) \
  do { char *Error_Message; Error_Message = Ur_Error_Message; \
    if (Audio_Line != -1) close(Audio_Line); \
    if (Error_Message) \
      {MUS_STANDARD_ERROR(Error_Type, Error_Message); FREE(Error_Message);} \
    else MUS_STANDARD_ERROR(Error_Type, mus_error_to_string(Error_Type)); \
    return(MUS_ERROR); \
  } while (0)

char *mus_audio_moniker(void) {return("HPUX audio");}

int mus_audio_open_output(int ur_dev, int srate, int chans, int format, int size)
{
  int fd, i, dev;
  struct audio_describe desc;
  dev = MUS_AUDIO_DEVICE(ur_dev);
  fd = open("/dev/audio", O_RDWR);
  if (fd == -1) 
    RETURN_ERROR_EXIT(MUS_AUDIO_CANT_OPEN, -1,
		      mus_format("can't open /dev/audio for output: %s",
				 strerror(errno)));
  ioctl(fd, AUDIO_SET_CHANNELS, chans);
  if (dev == MUS_AUDIO_SPEAKERS)
    ioctl(fd, AUDIO_SET_OUTPUT, AUDIO_OUT_SPEAKER);
  else
    if (dev == MUS_AUDIO_LINE_OUT)
      ioctl(fd, AUDIO_SET_OUTPUT, AUDIO_OUT_LINE);
    else ioctl(fd, AUDIO_SET_OUTPUT, AUDIO_OUT_HEADPHONE);
  if (format == MUS_BSHORT)
    ioctl(fd, AUDIO_SET_DATA_FORMAT, AUDIO_FORMAT_LINEAR16BIT);
  else
    if (format == MUS_MULAW)
      ioctl(fd, AUDIO_SET_DATA_FORMAT, AUDIO_FORMAT_ULAW);
  else 
    if (format == MUS_ALAW)
      ioctl(fd, AUDIO_SET_DATA_FORMAT, AUDIO_FORMAT_ALAW);
    else 
      RETURN_ERROR_EXIT(MUS_AUDIO_FORMAT_NOT_AVAILABLE, fd,
			mus_format("can't set output format to %d (%s) for %d (%s)",
				   format, mus_audio_format_name(format),
				   dev, 
				   mus_audio_device_name(dev)));
  ioctl(fd, AUDIO_DESCRIBE, &desc);
  for(i = 0; i < desc.nrates; i++) 
    if(srate == desc.sample_rate[i]) 
      break;
  if (i == desc.nrates) 
    RETURN_ERROR_EXIT(SRATE_NOT_AVAILABLE, fd,
		      mus_format("can't set srate to %d on %d (%s)",
				 srate, dev, 
				 mus_audio_device_name(dev)));
  ioctl(fd, AUDIO_SET_SAMPLE_RATE, srate);
  return(fd);
}

int mus_audio_write(int line, char *buf, int bytes)
{
  write(line, buf, bytes);
  return(MUS_NO_ERROR);
}

int mus_audio_close(int line) 
{
  close(line);
  return(MUS_NO_ERROR);
}

static void describe_audio_state_1(void)
{
  struct audio_describe desc;
  struct audio_gain gain;
  int mina, maxa, fd, tmp;
  int g[2];
  fd = open("/dev/audio", O_RDWR);
  if (fd == -1) return;
  ioctl(fd, AUDIO_GET_OUTPUT, &tmp);
  switch (tmp)
    {
    case AUDIO_OUT_SPEAKER:   pprint("output: speakers\n"); break;
    case AUDIO_OUT_HEADPHONE: pprint("output: headphone\n"); break;
    case AUDIO_OUT_LINE:      pprint("output: line out\n"); break;
    }
  ioctl(fd, AUDIO_GET_INPUT, &tmp);
  switch (tmp)
    {
    case AUDIO_IN_MIKE: pprint("input: mic\n"); break;
    case AUDIO_IN_LINE: pprint("input: line in\n"); break;
    }
  ioctl(fd, AUDIO_GET_DATA_FORMAT, &tmp);
  switch (tmp)
    {
    case AUDIO_FORMAT_LINEAR16BIT: pprint("format: 16-bit linear\n"); break;
    case AUDIO_FORMAT_ULAW:        pprint("format: mulaw\n"); break;
    case AUDIO_FORMAT_ALAW:        pprint("format: alaw\n"); break;
    }
  ioctl(fd, AUDIO_DESCRIBE, &desc);
  gain.channel_mask = (AUDIO_CHANNEL_LEFT | AUDIO_CHANNEL_RIGHT);
  ioctl(fd, AUDIO_GET_GAINS, &gain);
  close(fd);
  g[0] = gain.cgain[0].transmit_gain; 
  g[1] = gain.cgain[1].transmit_gain;
  mina = desc.min_transmit_gain;  
  maxa = desc.max_transmit_gain;
  mus_snprintf(audio_strbuf, PRINT_BUFFER_SIZE, "out vols: %.3f %.3f\n",
	  (float)(g[0] - mina) / (float)(maxa - mina),
	  (float)(g[1] - mina) / (float)(maxa - mina)); 
  pprint(audio_strbuf);
  g[0] = gain.cgain[0].receive_gain; 
  g[1] = gain.cgain[1].receive_gain;
  mina = desc.min_receive_gain;  
  maxa = desc.max_receive_gain;
  mus_snprintf(audio_strbuf, PRINT_BUFFER_SIZE, "in vols: %.3f %.3f\n",
	  (float)(g[0] - mina) / (float)(maxa - mina),
	  (float)(g[1] - mina) / (float)(maxa - mina)); 
  pprint(audio_strbuf);
  g[0] = gain.cgain[0].monitor_gain; 
  g[1] = gain.cgain[1].monitor_gain;
  mina = desc.min_monitor_gain;  
  maxa = desc.max_monitor_gain;
  mus_snprintf(audio_strbuf, PRINT_BUFFER_SIZE, "monitor vols: %.3f %.3f\n",
	  (float)(g[0] - mina) / (float)(maxa - mina),
	  (float)(g[1] - mina) / (float)(maxa - mina)); 
  pprint(audio_strbuf);
}

int mus_audio_mixer_read(int ur_dev, int field, int chan, float *val) 
{
  struct audio_describe desc;
  struct audio_gain gain;
  int audio_fd = -1, srate, g, maxa, mina, dev, err = MUS_NO_ERROR;
  dev = MUS_AUDIO_DEVICE(ur_dev);
  if (field == MUS_AUDIO_PORT)
    {
      val[0] = 4; 
      if (chan > 1) val[1] = MUS_AUDIO_MICROPHONE; 
      if (chan > 2) val[2] = MUS_AUDIO_DAC_OUT;
      if (chan > 3) val[3] = MUS_AUDIO_LINE_OUT;
      if (chan > 4) val[4] = MUS_AUDIO_LINE_IN;
    }
  else
    {
      if (field == FORMAT_FIELD)
        {
          val[0] = 3; 
          if (chan > 1) val[1] = MUS_BSHORT;
          if (chan > 2) val[2] = MUS_MULAW;
          if (chan > 3) val[3] = MUS_ALAW;
        }
      else
        {
          audio_fd = open("/dev/audio", O_RDWR);
          ioctl(audio_fd, AUDIO_DESCRIBE, &desc);
          switch (dev)
            {
            case MUS_AUDIO_DEFAULT:
            case MUS_AUDIO_DAC_OUT:
            case MUS_AUDIO_SPEAKERS:
            case MUS_AUDIO_LINE_OUT:
              switch (field)
                {
                case MUS_AUDIO_AMP: 
                  ioctl(audio_fd, AUDIO_GET_GAINS, &gain);
                  if (chan == 0) 
		    g = gain.cgain[0].transmit_gain; 
		  else g = gain.cgain[1].transmit_gain;
                  mina = desc.min_transmit_gain;  
		  maxa = desc.max_transmit_gain;
                  val[0] = (float)(g - mina) / (float)(maxa - mina);
                  break;
                case MUS_AUDIO_CHANNEL: 
		  val[0] = 2; 
		  break;
                case MUS_AUDIO_SRATE: 
                  ioctl(audio_fd, AUDIO_GET_SAMPLE_RATE, &srate); 
                  val[0] = srate; 
                  break;
                default: 
		  err = MUS_ERROR; 
		  break;
                }
              break;
            case MUS_AUDIO_MICROPHONE:
            case MUS_AUDIO_LINE_IN:
            case MUS_AUDIO_DUPLEX_DEFAULT:
              switch (field)
                {
                case MUS_AUDIO_AMP: 
                  ioctl(audio_fd, AUDIO_GET_GAINS, &gain);
                  if (chan == 0) 
		    g = gain.cgain[0].receive_gain; 
		  else g = gain.cgain[1].receive_gain;
                  mina = desc.min_receive_gain;  
		  maxa = desc.max_receive_gain;
                  val[0] = (float)(g - mina) / (float)(maxa - mina);
                  break;
                case MUS_AUDIO_CHANNEL: 
		  val[0] = 2; 
		  break;
                case MUS_AUDIO_SRATE: 
                  ioctl(audio_fd, AUDIO_GET_SAMPLE_RATE, &srate); 
                  val[0] = srate; 
                  break;
                default:
		  err = MUS_ERROR;
		  break;
                }
              break;
            default: 
	      err = MUS_ERROR;
	      break;
            }
        }
    }
  if (err == MUS_ERROR)
    RETURN_ERROR_EXIT(MUS_AUDIO_CANT_READ, audio_fd,
		      mus_format("can't read %s field of device %d (%s)",
				 mus_audio_device_name(field),
				 dev, 
				 mus_audio_device_name(dev)));

  if (audio_fd != -1) close(audio_fd);
  return(MUS_NO_ERROR);
}

int mus_audio_mixer_write(int ur_dev, int field, int chan, float *val) 
{
  struct audio_describe desc;
  struct audio_gain gain;
  int audio_fd = -1, srate, g, maxa, mina, dev, err = MUS_NO_ERROR;
  dev = MUS_AUDIO_DEVICE(ur_dev);
  audio_fd = open("/dev/audio", O_RDWR);
  ioctl(audio_fd, AUDIO_DESCRIBE, &desc);
  switch (dev)
    {
    case MUS_AUDIO_DEFAULT:
    case MUS_AUDIO_DAC_OUT:
    case MUS_AUDIO_SPEAKERS:
    case MUS_AUDIO_LINE_OUT:
      switch (field)
        {
        case MUS_AUDIO_AMP: 
          mina = desc.min_transmit_gain;  
	  maxa = desc.max_transmit_gain;
          ioctl(audio_fd, AUDIO_GET_GAINS, &gain);
          g = mina + val[0] * (maxa - mina);
          if (chan == 0) 
	    gain.cgain[0].transmit_gain = g; 
	  else gain.cgain[1].transmit_gain = g;
          ioctl(audio_fd, AUDIO_SET_GAINS, &gain);
          break;
        case MUS_AUDIO_SRATE: 
          srate = val[0];
          ioctl(audio_fd, AUDIO_SET_SAMPLE_RATE, srate); 
          break;
        default:
	  err = MUS_ERROR;
	  break;
        }
      break;
    case MUS_AUDIO_MICROPHONE:
    case MUS_AUDIO_LINE_IN:
    case MUS_AUDIO_DUPLEX_DEFAULT:
      switch (field)
        {
        case MUS_AUDIO_AMP: 
          mina = desc.min_receive_gain;  
	  maxa = desc.max_receive_gain;
          ioctl(audio_fd, AUDIO_GET_GAINS, &gain);
          g = mina + val[0] * (maxa - mina);
          if (chan == 0) 
	    gain.cgain[0].receive_gain = g; 
	  else gain.cgain[1].receive_gain = g;
          ioctl(audio_fd, AUDIO_SET_GAINS, &gain);
          break;
        case MUS_AUDIO_SRATE: 
          srate = val[0];
          ioctl(audio_fd, AUDIO_SET_SAMPLE_RATE, srate); 
          break;
        default:
	  err = MUS_ERROR;
	  break;
        }
      break;
    default:
      err = MUS_ERROR;
      break;
    }
  if (err == MUS_ERROR)
    RETURN_ERROR_EXIT(MUS_AUDIO_CANT_READ, audio_fd,
		      mus_format("can't set %s field of device %d (%s)",
				 mus_audio_device_name(field),
				 dev, 
				 mus_audio_device_name(dev)));

  if (audio_fd != -1) close(audio_fd);
  return(MUS_NO_ERROR);
}

static int saved_gains[6];

void mus_audio_save(void)
{
  int fd;
  struct audio_gain gain;
  gain.channel_mask = (AUDIO_CHANNEL_LEFT | AUDIO_CHANNEL_RIGHT);
  fd = open("/dev/audio", O_RDWR);
  ioctl(fd, AUDIO_GET_GAINS, &gain);
  close(fd);
  saved_gains[0] = gain.cgain[0].transmit_gain;
  saved_gains[1] = gain.cgain[0].receive_gain;
  saved_gains[2] = gain.cgain[0].monitor_gain;
  saved_gains[3] = gain.cgain[1].transmit_gain;
  saved_gains[4] = gain.cgain[1].receive_gain;
  saved_gains[5] = gain.cgain[1].monitor_gain;
}

void mus_audio_restore(void)
{
  int fd;
  struct audio_gain gain;
  gain.channel_mask = (AUDIO_CHANNEL_LEFT | AUDIO_CHANNEL_RIGHT);
  fd = open("/dev/audio", O_RDWR);
  ioctl(fd, AUDIO_GET_GAINS, &gain);
  gain.cgain[0].transmit_gain = saved_gains[0];
  gain.cgain[0].receive_gain = saved_gains[1];
  gain.cgain[0].monitor_gain = saved_gains[2];
  gain.cgain[1].transmit_gain = saved_gains[3];
  gain.cgain[1].receive_gain = saved_gains[4];
  gain.cgain[1].monitor_gain = saved_gains[5];
  ioctl(fd, AUDIO_SET_GAINS, &gain);
}

int mus_audio_initialize(void) {return(MUS_NO_ERROR);}

int mus_audio_systems(void) {return(1);}
char *mus_audio_system_name(int system) {return("HPUX");}

/* struct audio_status status_b;
 * ioctl(devAudio, AUDIO_GET_STATUS, &status_b)
 * not_busy = (status_b.transmit_status == AUDIO_DONE);
*/

int mus_audio_open_input(int ur_dev, int srate, int chans, int format, int size) 
{
  int fd, i, dev;
  struct audio_describe desc;
  dev = MUS_AUDIO_DEVICE(ur_dev);
  fd = open("/dev/audio", O_RDWR);
  if (fd == -1)
    RETURN_ERROR_EXIT(MUS_AUDIO_CANT_OPEN, NULL,
		      mus_format("can't open /dev/audio for input: %s",
				 strerror(errno)));
  ioctl(fd, AUDIO_SET_CHANNELS, chans);
  if (dev == MUS_AUDIO_MICROPHONE)
    ioctl(fd, AUDIO_SET_INPUT, AUDIO_IN_MIKE);
  else ioctl(fd, AUDIO_SET_INPUT, AUDIO_IN_LINE);
  if (format == MUS_BSHORT)
    ioctl(fd, AUDIO_SET_DATA_FORMAT, AUDIO_FORMAT_LINEAR16BIT);
  else
    if (format == MUS_MULAW)
      ioctl(fd, AUDIO_SET_DATA_FORMAT, AUDIO_FORMAT_ULAW);
  else 
    if (format == MUS_ALAW)
      ioctl(fd, AUDIO_SET_DATA_FORMAT, AUDIO_FORMAT_ALAW);
    else 
      RETURN_ERROR_EXIT(MUS_AUDIO_FORMAT_NOT_AVAILABLE, fd,
			mus_format("can't set input format to %d (%s) on %d (%s)",
				   format, mus_audio_format_name(format),
				   dev, 
				   mus_audio_device_name(dev)));
  ioctl(fd, AUDIO_DESCRIBE, &desc);
  for(i = 0; i < desc.nrates; i++) 
    if(srate == desc.sample_rate[i]) 
      break;
  if (i == desc.nrates) 
    RETURN_ERROR_EXIT(MUS_AUDIO_SRATE_NOT_AVAILABLE, fd,
		      mus_format("can't set srate to %d on %d (%s)",
				 srate, dev, 
				 mus_audio_device_name(dev)));
  ioctl(fd, AUDIO_SET_SAMPLE_RATE, srate);
  return(fd);
}

int mus_audio_read(int line, char *buf, int bytes) 
{
  read(line, buf, bytes);
  return(MUS_NO_ERROR);
}

#endif


/* ------------------------------- WINDOZE ----------------------------------------- */

#if defined(WINDOZE) && (!(defined(__CYGWIN__)))
#define AUDIO_OK

#include <windows.h>
#include <mmsystem.h>
#if HAVE_MMREG_H
  #include <mmreg.h>
#endif

#define BUFFER_FILLED 1
#define BUFFER_EMPTY 2

#define OUTPUT_LINE 1
#define INPUT_LINE 2

#define SOUND_UNREADY 0
#define SOUND_INITIALIZED 1
#define SOUND_RUNNING 2

static int buffer_size = 1024;
static int db_state[2];
static int sound_state = 0;
static int current_chans = 1;
static int current_datum_size = 2;
static int current_buf = 0;
WAVEHDR wh[2];
HWAVEOUT fd;
HWAVEIN record_fd;
WAVEHDR rec_wh;
static int rec_state = SOUND_UNREADY;

static MMRESULT win_in_err = 0, win_out_err = 0;
static char errstr[128], getstr[128];

static char *win_err_buf = NULL;
static mus_print_handler_t *old_handler;

static void win_mus_print(char *msg)
{
  if ((win_in_err == 0) && (win_out_err == 0))
    (*old_handler)(msg);
  else
    {
      if (win_in_err) 
	waveInGetErrorText(win_in_err, getstr, PRINT_BUFFER_SIZE);
      else waveOutGetErrorText(win_out_err, getstr, PRINT_BUFFER_SIZE);
      mus_snprintf(errstr, PRINT_BUFFER_SIZE, "%s [%s]", msg, getstr);
      (*old_handler)(errstr);
    }
}

static void start_win_print(void)
{
  if (old_handler != win_mus_print)
    old_handler = mus_print_set_handler(win_mus_print);
}

static void end_win_print(void)
{
  if (old_handler == win_mus_print)
    mus_print_set_handler(NULL);
  else mus_print_set_handler(old_handler);
}

#define RETURN_ERROR_EXIT(Error_Type, Ur_Error_Message) \
  do { char *Error_Message; Error_Message = Ur_Error_Message; \
    if (Error_Message) \
      {MUS_STANDARD_ERROR(Error_Type, Error_Message); FREE(Error_Message);} \
    else MUS_STANDARD_ERROR(Error_Type, mus_error_to_string(Error_Type)); \
    end_win_print(); \
    return(MUS_ERROR); \
  } while (0)

int mus_audio_systems(void) 
{
  /* this number is available -- see below (user mixer number as in linux)->mixerGetNumDevs */
  return(1);
}
char *mus_audio_system_name(int system) {return("Windoze");}

DWORD CALLBACK next_buffer(HWAVEOUT w, UINT msg, DWORD user_data, DWORD p1, DWORD p2)
{
  if (msg == WOM_DONE)
    {
      db_state[current_buf] = BUFFER_EMPTY;
    }
  return(0);
}

int mus_audio_open_output(int ur_dev, int srate, int chans, int format, int size) 
{
  WAVEFORMATEX wf;
  int dev;
  start_win_print();
  dev = MUS_AUDIO_DEVICE(ur_dev);
  wf.nChannels = chans;
  current_chans = chans;
  wf.wFormatTag = WAVE_FORMAT_PCM;
  wf.cbSize = 0;
  if (format == MUS_UBYTE) 
    {
      wf.wBitsPerSample = 8;
      current_datum_size = 1;
    }
  else 
    {
      wf.wBitsPerSample = 16;
      current_datum_size = 2;
    }
  wf.nSamplesPerSec = srate;
  wf.nBlockAlign = chans * current_datum_size;
  wf.nAvgBytesPerSec = wf.nBlockAlign * wf.nSamplesPerSec;
  win_out_err = waveOutOpen(&fd, WAVE_MAPPER, &wf, (DWORD)next_buffer, 0, CALLBACK_FUNCTION); /* 0 here = user_data above, other case = WAVE_FORMAT_QUERY */
  if (win_out_err) 
    RETURN_ERROR_EXIT(MUS_AUDIO_DEVICE_NOT_AVAILABLE,
		      mus_format("can't open %d (%s)",
				 dev, 
				 mus_audio_device_name(dev)));
  waveOutPause(fd);
  if (size <= 0) 
    buffer_size = 1024; 
  else buffer_size = size;
  wh[0].dwBufferLength = buffer_size * current_datum_size;
  wh[0].dwFlags = 0;
  wh[0].dwLoops = 0;
  wh[0].lpData = (char *)CALLOC(wh[0].dwBufferLength, sizeof(char));
  if ((wh[0].lpData) == 0) 
    {
      waveOutClose(fd); 
      RETURN_ERROR_EXIT(MUS_AUDIO_SIZE_NOT_AVAILABLE,
			mus_format("can't allocate buffer size %d for output %d (%s)",
				   buffer_size, dev, 
				   mus_audio_device_name(dev)));
    }
  win_out_err = waveOutPrepareHeader(fd, &(wh[0]), sizeof(WAVEHDR));
  if (win_out_err) 
    {
      FREE(wh[0].lpData); 
      waveOutClose(fd);  
      RETURN_ERROR_EXIT(MUS_AUDIO_CONFIGURATION_NOT_AVAILABLE,
			mus_format("can't setup output 'header' for %d (%s)",
				   dev, 
				   mus_audio_device_name(dev)));
    }
  db_state[0] = BUFFER_EMPTY;
  wh[1].dwBufferLength = buffer_size * current_datum_size;
  wh[1].dwFlags = 0;
  wh[1].dwLoops = 0;
  wh[1].lpData = (char *)CALLOC(wh[0].dwBufferLength, sizeof(char));
  if ((wh[1].lpData) == 0) 
    {
      FREE(wh[0].lpData); 
      waveOutClose(fd); 
      RETURN_ERROR_EXIT(MUS_AUDIO_SIZE_NOT_AVAILABLE,
			mus_format("can't allocate buffer size %d for output %d (%s)",
				   buffer_size, dev,
				   mus_audio_device_name(dev)));
    }
  win_out_err = waveOutPrepareHeader(fd, &(wh[1]), sizeof(WAVEHDR));
  if (win_out_err) 
    {
      waveOutUnprepareHeader(fd, &(wh[0]), sizeof(WAVEHDR)); 
      FREE(wh[0].lpData); 
      FREE(wh[1].lpData); 
      waveOutClose(fd);  
      RETURN_ERROR_EXIT(MUS_AUDIO_CONFIGURATION_NOT_AVAILABLE,
			mus_format("can't setup output 'header' for %d (%s)",
				   dev, 
				   mus_audio_device_name(dev)));
    }
  db_state[1] = BUFFER_EMPTY;
  sound_state = SOUND_INITIALIZED;
  current_buf = 0;
  end_win_print();
  return(OUTPUT_LINE);
}

static MMRESULT fill_buffer(int dbi, char *inbuf, int instart, int bytes)
{
  int i, j;
  win_out_err = 0;
  if (sound_state == SOUND_UNREADY) return(0);
  for (i = instart, j = 0; j < bytes; j++, i++)
    wh[dbi].lpData[j] = inbuf[i];
  wh[dbi].dwBufferLength = bytes;
  db_state[dbi] = BUFFER_FILLED;
  if ((sound_state == SOUND_INITIALIZED) && 
      (dbi == 1))
    {
      sound_state = SOUND_RUNNING;
      win_out_err = waveOutRestart(fd);
    }
  return(win_out_err);
}

static void wait_for_empty_buffer(int buf)
{
  while (db_state[buf] != BUFFER_EMPTY)
    {
      Sleep(1);      /* in millisecs, so even this may be too much if buf = 256 bytes */
    }
}

int mus_audio_write(int line, char *buf, int bytes) 
{
  int lim, leftover, start;
  start_win_print();
  if (line != OUTPUT_LINE) 
    RETURN_ERROR_EXIT(MUS_AUDIO_CANT_WRITE,
		      mus_format("write error: line %d != %d?",
				 line, OUTPUT_LINE));
  win_out_err = 0;
  leftover = bytes;
  start = 0;
  if (sound_state == SOUND_UNREADY) 
    {
      end_win_print(); 
      return(MUS_NO_ERROR);
    }
  while (leftover > 0)
    {
      lim = leftover;
      if (lim > buffer_size) lim = buffer_size;
      leftover -= lim;
      wait_for_empty_buffer(current_buf);
      win_out_err = fill_buffer(current_buf, buf, start, lim);
      if (win_out_err) 
	RETURN_ERROR_EXIT(MUS_AUDIO_CANT_WRITE,
			  mus_format("write error on %d",
				     line));
      win_out_err = waveOutWrite(fd, &wh[current_buf], sizeof(WAVEHDR));
      if (win_out_err) 
	RETURN_ERROR_EXIT(MUS_AUDIO_CANT_WRITE,
			  mus_format("write error on %d",
				     line));
      start += lim;
      current_buf++;
      if (current_buf > 1) current_buf = 0;
    }
  return(MUS_NO_ERROR);
}

static int out_saved = 0, aux_saved = 0;
static DWORD *out_vols = NULL, *aux_vols = NULL;
static int *out_set = NULL, *aux_set = NULL;

void mus_audio_save(void) 
{
  UINT dev;
  MMRESULT err;
  DWORD val;
  HWAVEOUT hd;
  WAVEOUTCAPS wocaps;
  AUXCAPS wacaps;
  WAVEFORMATEX pwfx;
  out_saved = waveOutGetNumDevs();
  if (out_vols) {FREE(out_vols); out_vols = NULL;}
  if (out_set) {FREE(out_set); out_set = NULL;}
  if (out_saved > 0)
    {
      out_vols = (DWORD *)CALLOC(out_saved, sizeof(DWORD));
      out_set = (int *)CALLOC(out_saved, sizeof(int));
      for (dev = 0; dev < out_saved; dev++)
        {
          err = waveOutGetDevCaps(dev, &wocaps, sizeof(wocaps));
          if ((!err) && (wocaps.dwSupport & WAVECAPS_VOLUME))
            {
	      err = waveOutOpen(&hd, dev, &pwfx, 0, 0, WAVE_MAPPER);
	      if (!err)
		{
		  err = waveOutGetVolume(hd, &val);
		  if (!err) 
		    {
		      out_vols[dev] = val;
		      out_set[dev] = 1;
		    }
		  waveOutClose(hd);
                }
            }
        }
    }
  aux_saved = auxGetNumDevs();
  if (aux_vols) {FREE(aux_vols); aux_vols = NULL;}
  if (aux_set) {FREE(aux_set); aux_set = NULL;}
  if (aux_saved > 0)
    {
      aux_vols = (DWORD *)CALLOC(aux_saved, sizeof(unsigned long));
      aux_set = (int *)CALLOC(aux_saved, sizeof(int));
      for (dev = 0; dev < aux_saved; dev++)
        {
          err = auxGetDevCaps(dev, &wacaps, sizeof(wacaps));
          if ((!err) && (wacaps.dwSupport & AUXCAPS_VOLUME))
            {
              err = auxGetVolume(dev, &val);
              if (!err) 
                {
                  aux_vols[dev] = val;
                  aux_set[dev] = 1;
                }
            }
        }
    }
  /* mixer state needs to be saved too, I suppose */
}

void mus_audio_restore(void) 
{
  int i;
  HWAVEOUT hd;
  WAVEFORMATEX pwfx;
  MMRESULT err;
  for (i = 0; i < out_saved; i++)
    if (out_set[i])
      {
	err = waveOutOpen(&hd, i, &pwfx, 0, 0, WAVE_MAPPER);
	if (!err)
	  {
	    waveOutSetVolume(hd, out_vols[i]);
	    waveOutClose(hd);
	  }
      }
  for (i = 0; i < aux_saved; i++) 
    if (aux_set[i]) 
      auxSetVolume(i, aux_vols[i]);
}

static float unlog(unsigned short val)
{
  /* 1.0 linear is 0xffff, rest is said to be "logarithmic", whatever that really means here */
  if (val == 0) return(0.0);
  return((float)val / 65536.0);
  /* return(pow(2.0, amp) - 1.0); */ /* doc seems to be bogus */
}

#define SRATE_11025_BITS (WAVE_FORMAT_1S16 | WAVE_FORMAT_1S08 | WAVE_FORMAT_1M16 | WAVE_FORMAT_1M08)
#define SRATE_22050_BITS (WAVE_FORMAT_2S16 | WAVE_FORMAT_2S08 | WAVE_FORMAT_2M16 | WAVE_FORMAT_2M08)
#define SRATE_44100_BITS (WAVE_FORMAT_4S16 | WAVE_FORMAT_4S08 | WAVE_FORMAT_4M16 | WAVE_FORMAT_4M08)
#define SHORT_SAMPLE_BITS (WAVE_FORMAT_1S16 | WAVE_FORMAT_1M16 | WAVE_FORMAT_2S16 | WAVE_FORMAT_2M16 | WAVE_FORMAT_4S16 | WAVE_FORMAT_4M16)
#define BYTE_SAMPLE_BITS (WAVE_FORMAT_1S08 | WAVE_FORMAT_1M08 | WAVE_FORMAT_2S08 | WAVE_FORMAT_2M08 | WAVE_FORMAT_4S08 | WAVE_FORMAT_4M08)

static char *mfg(int mf)
{
#if HAVE_MMREG_H
  switch (mf)
    {
    case MM_MICROSOFT: return("Microsoft"); break;              case MM_CREATIVE: return("Creative Labs"); break; 
    case MM_MEDIAVISION: return("Media Vision"); break;         case MM_FUJITSU: return("Fujitsu Corp"); break;
    case MM_ARTISOFT: return("Artisoft"); break;                case MM_TURTLE_BEACH: return("Turtle Beach"); break;
    case MM_IBM: return("IBM"); break;                          case MM_VOCALTEC: return("Vocaltec"); break;
    case MM_ROLAND: return("Roland"); break;                    case MM_DSP_SOLUTIONS: return("DSP Solutions"); break;
    case MM_NEC: return("NEC"); break;                          case MM_ATI: return("ATI"); break;
    case MM_WANGLABS: return("Wang Laboratories"); break;       case MM_TANDY: return("Tandy"); break;
    case MM_VOYETRA: return("Voyetra"); break;                  case MM_ANTEX: return("Antex Electronics"); break;
    case MM_ICL_PS: return("ICL Personal Systems"); break;      case MM_INTEL: return("Intel"); break;
    case MM_GRAVIS: return("Advanced Gravis"); break;           case MM_VAL: return("Video Associates Labs"); break;
    case MM_INTERACTIVE: return("InterActive"); break;          case MM_YAMAHA: return("Yamaha"); break;
    case MM_EVEREX: return("Everex Systems"); break;            case MM_ECHO: return("Echo Speech"); break;
    case MM_SIERRA: return("Sierra Semiconductor"); break;      case MM_CAT: return("Computer Aided Technologies"); break;
    case MM_APPS: return("APPS Software"); break;               case MM_DSP_GROUP: return("DSP Group"); break;
    case MM_MELABS: return("microEngineering Labs"); break;     case MM_COMPUTER_FRIENDS: return("Computer Friends"); break;
    case MM_ESS: return("ESS Technology"); break;               case MM_AUDIOFILE: return("Audio"); break;
    case MM_MOTOROLA: return("Motorola"); break;                case MM_CANOPUS: return("Canopus"); break;
    case MM_EPSON: return("Seiko Epson"); break;                case MM_TRUEVISION: return("Truevision"); break;
    case MM_AZTECH: return("Aztech Labs"); break;               case MM_VIDEOLOGIC: return("Videologic"); break;
    case MM_SCALACS: return("SCALACS"); break;                  case MM_KORG: return("Korg"); break;
    case MM_APT: return("Audio Processing Technology"); break;  case MM_ICS: return("Integrated Circuit Systems"); break;
    case MM_ITERATEDSYS: return("Iterated Systems"); break;     case MM_METHEUS: return("Metheus"); break;
    case MM_LOGITECH: return("Logitech"); break;                case MM_WINNOV: return("Winnov"); break;
    case MM_NCR: return("NCR"); break;                          case MM_EXAN: return("EXAN"); break;
    case MM_AST: return("AST Research"); break;                 case MM_WILLOWPOND: return("Willow Pond"); break;
    case MM_SONICFOUNDRY: return("Sonic Foundry"); break;       case MM_VITEC: return("Vitec Multimedia"); break;
    case MM_MOSCOM: return("MOSCOM"); break;                    case MM_SILICONSOFT: return("Silicon Soft"); break;
    case MM_SUPERMAC: return("Supermac"); break;                case MM_AUDIOPT: return("Audio Processing Technology"); break;
    case MM_SPEECHCOMP: return("Speech Compression"); break;    case MM_DOLBY: return("Dolby Laboratories"); break;
    case MM_OKI: return("OKI"); break;                          case MM_AURAVISION: return("AuraVision"); break;
    case MM_OLIVETTI: return("Olivetti"); break;                case MM_IOMAGIC: return("I/O Magic"); break;
    case MM_MATSUSHITA: return("Matsushita Electric"); break;   case MM_CONTROLRES: return("Control Resources"); break;
    case MM_XEBEC: return("Xebec Multimedia Solutions"); break; case MM_NEWMEDIA: return("New Media"); break;
    case MM_NMS: return("Natural MicroSystems"); break;         case MM_LYRRUS: return("Lyrrus"); break;
    case MM_COMPUSIC: return("Compusic"); break;                case MM_OPTI: return("OPTi Computers"); break;
    case MM_DIALOGIC: return("Dialogic"); break;    
    }
#endif
  return("");
}

static char *mixer_status_name(int status)
{
  switch (status)
    {
    case MIXERLINE_LINEF_ACTIVE: return(", (active)"); break;
    case MIXERLINE_LINEF_DISCONNECTED: return(", (disconnected)"); break;
    case MIXERLINE_LINEF_SOURCE: return(", (source)"); break;
    default: return(""); break;
    }
}

static char *mixer_target_name(int type)
{
  switch (type)
    {
    case MIXERLINE_TARGETTYPE_UNDEFINED: return("undefined"); break;
    case MIXERLINE_TARGETTYPE_WAVEOUT: return("output"); break;
    case MIXERLINE_TARGETTYPE_WAVEIN: return("input"); break;
    case MIXERLINE_TARGETTYPE_MIDIOUT: return("midi output"); break;
    case MIXERLINE_TARGETTYPE_MIDIIN: return("midi input"); break;
    case MIXERLINE_TARGETTYPE_AUX: return("aux"); break;
    default: return(""); break;
    }
}

static char *mixer_component_name(int type)
{
  switch (type)
    {
    case MIXERLINE_COMPONENTTYPE_DST_UNDEFINED: return("undefined"); break;
    case MIXERLINE_COMPONENTTYPE_DST_DIGITAL: return("digital"); break;
    case MIXERLINE_COMPONENTTYPE_DST_LINE: return("line"); break;
    case MIXERLINE_COMPONENTTYPE_DST_MONITOR: return("monitor"); break;
    case MIXERLINE_COMPONENTTYPE_DST_SPEAKERS: return("speakers"); break;
    case MIXERLINE_COMPONENTTYPE_DST_HEADPHONES: return("headphones"); break;
    case MIXERLINE_COMPONENTTYPE_DST_TELEPHONE: return("telephone"); break;
    case MIXERLINE_COMPONENTTYPE_DST_WAVEIN: return("wave in"); break;
    case MIXERLINE_COMPONENTTYPE_DST_VOICEIN: return("voice in"); break;
    case MIXERLINE_COMPONENTTYPE_SRC_UNDEFINED: return("undefined"); break;
    case MIXERLINE_COMPONENTTYPE_SRC_DIGITAL: return("digital"); break;
    case MIXERLINE_COMPONENTTYPE_SRC_LINE: return("line"); break;
    case MIXERLINE_COMPONENTTYPE_SRC_MICROPHONE: return("mic"); break;
    case MIXERLINE_COMPONENTTYPE_SRC_SYNTHESIZER: return("synth"); break;
    case MIXERLINE_COMPONENTTYPE_SRC_COMPACTDISC: return("CD"); break;
    case MIXERLINE_COMPONENTTYPE_SRC_TELEPHONE: return("telephone"); break;
    case MIXERLINE_COMPONENTTYPE_SRC_PCSPEAKER: return("speaker"); break;
    case MIXERLINE_COMPONENTTYPE_SRC_WAVEOUT: return("wave out"); break;
    case MIXERLINE_COMPONENTTYPE_SRC_AUXILIARY: return("aux"); break;
    case MIXERLINE_COMPONENTTYPE_SRC_ANALOG: return("analog"); break;
    default: return(""); break;
    }
}

#define MAX_DESCRIBE_CHANS 8
#define MAX_DESCRIBE_CONTROLS 16
/* these actually need to be big enough to handle whatever comes along, since we can't read partial states */
/*   or they need to be expanded as necessary */

char *mus_audio_moniker(void) {return("MS audio");} /* version number of some sort? */

static void describe_audio_state_1(void) 
{
  int devs, dev, srate, chans, format, need_comma, maker;
  MMRESULT err;
  unsigned long val, rate, pitch, version;
  WAVEOUTCAPS wocaps;
  WAVEINCAPS wicaps;
  AUXCAPS wacaps;
  HWAVEOUT hd;
  WAVEFORMATEX pwfx;
#ifdef MIXERR_BASE
  MIXERCAPS wmcaps;
  MIXERLINE mixline;
  MIXERLINECONTROLS linecontrols;
  MIXERCONTROL mc[MAX_DESCRIBE_CONTROLS];
  MIXERCONTROLDETAILS controldetails;
  MIXERCONTROLDETAILS_LISTTEXT clist[MAX_DESCRIBE_CHANS];
  MIXERCONTROLDETAILS_BOOLEAN cbool[MAX_DESCRIBE_CHANS];
  MIXERCONTROLDETAILS_UNSIGNED cline[MAX_DESCRIBE_CHANS];
  MIXERCONTROLDETAILS_SIGNED csign[MAX_DESCRIBE_CHANS];
  HMIXER mfd;
  int control, controls, dest, dests, source, happy, dest_time, chan, mina, maxa, ctype;
#endif
  need_comma = 1;
  chans = 1;
  devs = waveOutGetNumDevs();
  if (devs > 0)
    {
      pprint("Output:\n");
      for (dev = 0; dev < devs; dev++)
        {
          err = waveOutGetDevCaps(dev, &wocaps, sizeof(wocaps));
          if (!err)
            {
              version = wocaps.vDriverVersion;
              maker = wocaps.wMid;
              mus_snprintf(audio_strbuf, PRINT_BUFFER_SIZE, "  %s %s: version %d.%d\n",
                      mfg(maker), wocaps.szPname,
                      version>>8, version&0xff);
              pprint(audio_strbuf);
              if (wocaps.wChannels == 2) {chans = 2; pprint("    stereo");} else {chans = 1; pprint("    mono");}
              if (wocaps.dwFormats & SRATE_11025_BITS)  {srate = 11025; if (need_comma) pprint(", "); pprint(" 11025"); need_comma = 1;}
              if (wocaps.dwFormats & SRATE_22050_BITS)  {srate = 22050; if (need_comma) pprint(", "); pprint(" 22050"); need_comma = 1;}
              if (wocaps.dwFormats & SRATE_44100_BITS)  {srate = 44100; if (need_comma) pprint(", "); pprint(" 44100"); need_comma = 1;}
              if (wocaps.dwFormats & BYTE_SAMPLE_BITS)  {format = 8; if (need_comma) pprint(", "); pprint(" unsigned byte"); need_comma = 1;}
              if (wocaps.dwFormats & SHORT_SAMPLE_BITS) {format = 16; if (need_comma) pprint(", "); pprint(" little-endian short"); need_comma = 1;}
              if (need_comma) pprint("\n");
              need_comma = 0;
              pwfx.wFormatTag = WAVE_FORMAT_PCM;
              pwfx.nChannels = chans;
              pwfx.nSamplesPerSec = srate;
              pwfx.nAvgBytesPerSec = srate;
              pwfx.nBlockAlign = 1;
              pwfx.wBitsPerSample = format;

              err = waveOutOpen(&hd, dev, &pwfx, 0, 0, WAVE_FORMAT_QUERY);

              if (wocaps.dwSupport & WAVECAPS_VOLUME)
                {
                  err = waveOutGetVolume(hd, &val);
                  if (!err)
                    {
                      if (wocaps.dwSupport & WAVECAPS_LRVOLUME)
                        mus_snprintf(audio_strbuf, PRINT_BUFFER_SIZE, 
				     "  vol: %.3f %.3f", 
				     unlog((unsigned short)(val >> 16)), 
				     unlog((unsigned short)(val & 0xffff)));
                      else mus_snprintf(audio_strbuf, PRINT_BUFFER_SIZE, 
					"  vol: %.3f", 
					unlog((unsigned short)(val & 0xffff)));
                      pprint(audio_strbuf);
                      need_comma = 1;
                    }
                }
              if (!err)
                {
                  /* this is just to get the hd data for subsequent info */
                  if (wocaps.dwSupport & WAVECAPS_PLAYBACKRATE)
                    {
                      err = waveOutGetPlaybackRate(hd, &rate);
                      if (!err)
                        {
                          mus_snprintf(audio_strbuf, PRINT_BUFFER_SIZE, 
				       "%s playback rate: %.3f", 
				       (need_comma ? ", " : ""), 
				       (float)rate / 65536.0);
                          pprint(audio_strbuf);
                          need_comma = 1;
                        }
                    }
                  if (wocaps.dwSupport & WAVECAPS_PITCH)
                    {
                      err = waveOutGetPitch(hd, &pitch);
                      if (!err)
                        {
                          mus_snprintf(audio_strbuf, PRINT_BUFFER_SIZE, 
				       "%s pitch: %.3f", 
				       (need_comma ? ", " : ""), 
				       (float)pitch / 65536.0);
                          pprint(audio_strbuf);
                          need_comma = 1;
                        }
                    }
                  waveOutClose(hd);
                }
              if (need_comma) {need_comma = 0; pprint("\n");}
            }
        }
    }
  devs = waveInGetNumDevs();
  if (devs > 0)
    {
      pprint("Input:\n");
      for (dev = 0; dev < devs; dev++)
        {
          err = waveInGetDevCaps(dev, &wicaps, sizeof(wicaps));
          if (!err)
            {
              mus_snprintf(audio_strbuf, PRINT_BUFFER_SIZE, "  %s%s", (wicaps.wMid != maker) ? mfg(wicaps.wMid) : "", wicaps.szPname);
              pprint(audio_strbuf);
              if ((wicaps.wMid != maker) || (version != wicaps.vDriverVersion))
                {
                  mus_snprintf(audio_strbuf, PRINT_BUFFER_SIZE, ": version %d.%d\n", 
			       (wicaps.vDriverVersion >> 8), 
			       wicaps.vDriverVersion & 0xff);
                  pprint(audio_strbuf);
                }
              else pprint("\n");
              if (wicaps.wChannels == 2) pprint("    stereo"); else pprint("    mono");
              if (wicaps.dwFormats & SRATE_11025_BITS)  {pprint(", 11025"); need_comma = 1;}
              if (wicaps.dwFormats & SRATE_22050_BITS)  {if (need_comma) pprint(", "); pprint(" 22050"); need_comma = 1;}
              if (wicaps.dwFormats & SRATE_44100_BITS)  {if (need_comma) pprint(", "); pprint(" 44100"); need_comma = 1;}
              if (wicaps.dwFormats & BYTE_SAMPLE_BITS)  {if (need_comma) pprint(", "); pprint(" unsigned byte"); need_comma = 1;}
              if (wicaps.dwFormats & SHORT_SAMPLE_BITS) {if (need_comma) pprint(", "); pprint(" little-endian short");}
              pprint("\n");
            }
        }
    }
  devs = auxGetNumDevs();
  if (devs > 0)
    {
      pprint("Auxiliary:\n");
      for (dev = 0; dev < devs; dev++)
        {
          err = auxGetDevCaps(dev, &wacaps, sizeof(wacaps));
          if (!err)
            {
              mus_snprintf(audio_strbuf, PRINT_BUFFER_SIZE, "  %s%s", (wacaps.wMid != maker) ? mfg(wacaps.wMid) : "", wacaps.szPname);
              pprint(audio_strbuf);
              if ((wacaps.wMid != maker) || (version != wacaps.vDriverVersion))
                mus_snprintf(audio_strbuf, PRINT_BUFFER_SIZE, ": version %d.%d%s",
                        (wacaps.vDriverVersion>>8), wacaps.vDriverVersion&0xff,
                        (wacaps.wTechnology & AUXCAPS_CDAUDIO) ? " (CD)" : "");
              else mus_snprintf(audio_strbuf, PRINT_BUFFER_SIZE, "%s\n", (wacaps.wTechnology & AUXCAPS_CDAUDIO) ? " (CD)" : "");
              pprint(audio_strbuf);
              if (wacaps.dwSupport & AUXCAPS_VOLUME)
                {
                  err = auxGetVolume(dev, &val);
                  if (!err)
                    {
                      if (wacaps.dwSupport & AUXCAPS_LRVOLUME)
                        mus_snprintf(audio_strbuf, PRINT_BUFFER_SIZE, 
				     "  vol: %.3f %.3f\n", 
				     unlog((unsigned short)(val >> 16)), 
				     unlog((unsigned short)(val & 0xffff)));
                      else mus_snprintf(audio_strbuf, PRINT_BUFFER_SIZE, 
					"  vol: %.3f\n", 
					unlog((unsigned short)(val & 0xffff)));
                      pprint(audio_strbuf);
                    }
                }
            }
        }
    }
#ifdef MIXERR_BASE
  devs = mixerGetNumDevs();
  if (devs > 0)
    {
      pprint("Mixer:\n");
      for (dev = 0; dev < devs; dev++)
        {
          err = mixerGetDevCaps(dev, &wmcaps, sizeof(wmcaps));
          if (!err)
            {
              mus_snprintf(audio_strbuf, PRINT_BUFFER_SIZE, "  %s%s", (wmcaps.wMid != maker) ? mfg(wmcaps.wMid) : "", wmcaps.szPname);
              pprint(audio_strbuf);
              if ((wmcaps.wMid != maker) || (version != wmcaps.vDriverVersion))
                {
                  mus_snprintf(audio_strbuf, PRINT_BUFFER_SIZE,
			       ": version %d.%d\n", 
			       (wmcaps.vDriverVersion >> 8), 
			       wmcaps.vDriverVersion & 0xff);
                  pprint(audio_strbuf);
                }
              else pprint("\n");
              dests = wmcaps.cDestinations;
              
              err = mixerOpen(&mfd, dev, 0, 0, CALLBACK_NULL);
              if (!err)
                {
                  dest = 0;
                  source = 0;
                  dest_time = 1;
                  happy = 1;
                  while (happy)
                    {
                      if (dest_time)
                        {
                          mixline.dwDestination = dest;
                          mixline.cbStruct = sizeof(MIXERLINE);
                          err = mixerGetLineInfo(mfd, &mixline, MIXER_GETLINEINFOF_DESTINATION);
                        }
                      else
                        {
                          mixline.dwSource = source;
                          mixline.cbStruct = sizeof(MIXERLINE);
                          err = mixerGetLineInfo(mfd, &mixline, MIXER_GETLINEINFOF_SOURCE);
                        }
                      if (!err)
                        {
                          if ((source == 0) && (!dest_time)) pprint("  Sources:\n");
                          if ((dest == 0) && (dest_time)) pprint("  Destinations:\n");
                          mus_snprintf(audio_strbuf, PRINT_BUFFER_SIZE, "    %s: %s (%s), %d chan%s",
                                  mixline.szName,
                                  mixer_component_name(mixline.dwComponentType),
                                  mixer_target_name(mixline.Target.dwType),
                                  
                                  mixline.cChannels, ((mixline.cChannels != 1) ? "s" : ""));
                          pprint(audio_strbuf);
                          if (mixline.cConnections > 0)
                                {
                                    mus_snprintf(audio_strbuf, PRINT_BUFFER_SIZE, ", %d connection%s",
                                    mixline.cConnections, ((mixline.cConnections != 1) ? "s" : ""));
                                    pprint(audio_strbuf);
                                }
                          if (dest_time) 
                            {
                              mus_snprintf(audio_strbuf, PRINT_BUFFER_SIZE, "%s\n", mixer_status_name(mixline.fdwLine));
                              pprint(audio_strbuf);
                            }
                          else pprint("\n");
                          if (mixline.cControls > 0)
                            {
                              linecontrols.cbStruct = sizeof(MIXERLINECONTROLS);
                              linecontrols.dwLineID = mixline.dwLineID;
                              linecontrols.dwControlID = MIXER_GETLINECONTROLSF_ONEBYID;
                              if (mixline.cControls > MAX_DESCRIBE_CONTROLS)
                                linecontrols.cControls = MAX_DESCRIBE_CONTROLS;
                              else linecontrols.cControls = mixline.cControls;
                              linecontrols.pamxctrl = mc;
                              linecontrols.cbmxctrl = sizeof(MIXERCONTROL);
                              err = mixerGetLineControls(mfd, &linecontrols, MIXER_GETLINECONTROLSF_ALL);
                              if (!err)
                                {
                                  mus_snprintf(audio_strbuf, PRINT_BUFFER_SIZE, 
					       "      %d control%s:\n", 
					       linecontrols.cControls, 
					       (linecontrols.cControls != 1) ? "s" : "");
                                  pprint(audio_strbuf);
                                  controls = linecontrols.cControls;
                                  if (controls > MAX_DESCRIBE_CONTROLS) controls = MAX_DESCRIBE_CONTROLS;
                                  for (control = 0; control < controls; control++)
                                    {

                                       mus_snprintf(audio_strbuf, PRINT_BUFFER_SIZE, "        %s", mc[control].szName);
                                       pprint(audio_strbuf);
                                       controldetails.cbStruct = sizeof(MIXERCONTROLDETAILS);
                                       controldetails.dwControlID = mc[control].dwControlID;

				       ctype = (mc[control].dwControlType);
				       if ((ctype == MIXERCONTROL_CONTROLTYPE_EQUALIZER) ||
					   (ctype == MIXERCONTROL_CONTROLTYPE_MUX) ||
					   (ctype == MIXERCONTROL_CONTROLTYPE_MIXER) ||
					   (ctype == MIXERCONTROL_CONTROLTYPE_SINGLESELECT) ||
					   (ctype == MIXERCONTROL_CONTROLTYPE_MULTIPLESELECT))
					 {
					   controldetails.cChannels = 1;
                                           controldetails.cMultipleItems = mc[control].cMultipleItems;
                                           controldetails.cbDetails = sizeof(MIXERCONTROLDETAILS_LISTTEXT);
                                           controldetails.paDetails = clist;
					   err = mixerGetControlDetails(mfd, &controldetails, MIXER_GETCONTROLDETAILSF_LISTTEXT);
					   if (!err) 
					     {
					       for (chan = 0; chan < mixline.cChannels; chan++) 
						 {
						   mus_snprintf(audio_strbuf, PRINT_BUFFER_SIZE, " [%s]", clist[chan].szName);
						   pprint(audio_strbuf);
						 }
					     }
					 }
                                       if (mixline.cChannels > MAX_DESCRIBE_CHANS)
                                         controldetails.cChannels = MAX_DESCRIBE_CHANS;
                                       else controldetails.cChannels = mixline.cChannels;
                                       controldetails.cMultipleItems = 0;
                                       err = 0;
                                       switch (mc[control].dwControlType & MIXERCONTROL_CT_UNITS_MASK)
                                         {
                                         case MIXERCONTROL_CT_UNITS_BOOLEAN:
                                           controldetails.cbDetails = sizeof(MIXERCONTROLDETAILS_BOOLEAN);
                                           controldetails.paDetails = cbool;
                                           break;
                                         case MIXERCONTROL_CT_UNITS_SIGNED: case MIXERCONTROL_CT_UNITS_DECIBELS:
                                           controldetails.cbDetails = sizeof(MIXERCONTROLDETAILS_SIGNED);
                                           controldetails.paDetails = csign;
                                           break;
                                         case MIXERCONTROL_CT_UNITS_UNSIGNED: case MIXERCONTROL_CT_UNITS_PERCENT:
                                           controldetails.cbDetails = sizeof(MIXERCONTROLDETAILS_UNSIGNED);
                                           controldetails.paDetails = cline;
                                           break;
                                         default: err = 1; break;
                                         }
                                       if (err) 
                                         pprint("\n");
                                       else
                                         {
                                           err = mixerGetControlDetails(mfd, &controldetails, MIXER_GETCONTROLDETAILSF_VALUE);
                                           if (!err)
                                             {
                                               chans = controldetails.cChannels;
                                               if (chans > MAX_DESCRIBE_CHANS) chans = MAX_DESCRIBE_CHANS;
                                               switch (mc[control].dwControlType & MIXERCONTROL_CT_UNITS_MASK)
                                                 {
                                                 case MIXERCONTROL_CT_UNITS_BOOLEAN:
                                                   for (chan = 0; chan < chans; chan++)
                                                     {
                                                       mus_snprintf(audio_strbuf, PRINT_BUFFER_SIZE, " %s", (cbool[chan].fValue) ? " on" : " off");
                                                       pprint(audio_strbuf);
                                                     }
                                                   break;
                                                 case MIXERCONTROL_CT_UNITS_SIGNED: case MIXERCONTROL_CT_UNITS_DECIBELS:
                                                   mina = mc[control].Bounds.lMinimum;
                                                   maxa = mc[control].Bounds.lMaximum;
                                                   if (maxa > mina)
                                                     {
                                                       for (chan = 0; chan < chans; chan++)
                                                         {
                                                           mus_snprintf(audio_strbuf, PRINT_BUFFER_SIZE, " %.3f", 
									(float)(csign[chan].lValue - mina) / (float)(maxa - mina));
                                                           pprint(audio_strbuf);
                                                         }
                                                     }
                                                   break;
                                                 case MIXERCONTROL_CT_UNITS_UNSIGNED: case MIXERCONTROL_CT_UNITS_PERCENT:
                                                   mina = mc[control].Bounds.dwMinimum;
                                                   maxa = mc[control].Bounds.dwMaximum;
                                                   if (maxa > mina)
                                                     {
                                                       for (chan = 0; chan < chans; chan++)
                                                         {
                                                           mus_snprintf(audio_strbuf, PRINT_BUFFER_SIZE, " %.3f", 
									(float)(cline[chan].dwValue - mina) / (float)(maxa - mina));
                                                           pprint(audio_strbuf);
                                                         }
                                                     }
                                                   break;
                                                 default: break;
                                                 }
                                               pprint("\n");
                                             }
                                           else pprint("\n");
                                         }
                                    }
                                }
                            }
                        }
                      else if (!dest_time) happy = 0;
                      if (dest_time) dest++; else source++;
                      if (dest == dests) dest_time = 0;
                    }
                }
              mixerClose(mfd);
            }
        }
    }
#endif
}

int mus_audio_initialize(void) 
{
  return(MUS_NO_ERROR);
}

int mus_audio_close(int line) 
{
  int i;
  win_out_err = 0; 
  win_in_err = 0;
  if (line == OUTPUT_LINE)
    {
      /* fill with a few zeros, wait for empty flag */
      if (sound_state != SOUND_UNREADY)
        {
          wait_for_empty_buffer(current_buf);
          for (i = 0; i < 128; i++) wh[current_buf].lpData[i] = 0;
          wait_for_empty_buffer(current_buf);
          win_out_err = waveOutClose(fd);
	  i = 0;
          while (win_out_err == WAVERR_STILLPLAYING)
            {
	      Sleep(1);
              win_out_err = waveOutClose(fd);
	      i++;
	      if (i > 1024) break;
            }
          db_state[0] = BUFFER_EMPTY;
          db_state[1] = BUFFER_EMPTY;
          sound_state = SOUND_UNREADY;
          waveOutUnprepareHeader(fd, &(wh[0]), sizeof(WAVEHDR));
          waveOutUnprepareHeader(fd, &(wh[1]), sizeof(WAVEHDR));
          FREE(wh[0].lpData);
          FREE(wh[1].lpData);
          if (win_out_err) 
	    RETURN_ERROR_EXIT(MUS_AUDIO_CANT_CLOSE,
			      mus_format("close failed on %d",
					 line));
        }
    }
  else 
    {
      if (line == INPUT_LINE)
        {
          if (rec_state != SOUND_UNREADY)
            {
              waveInReset(record_fd);
              waveInClose(record_fd);
              waveInUnprepareHeader(record_fd, &rec_wh, sizeof(WAVEHDR));
              if (rec_wh.lpData) 
		{
		  FREE(rec_wh.lpData);
		  rec_wh.lpData = NULL;
		}
              rec_state = SOUND_UNREADY;
            }
        }
      else 
	RETURN_ERROR_EXIT(MUS_AUDIO_CANT_CLOSE,
			  mus_format("can't close unrecognized line %d",
				     line));
    }
  return(MUS_NO_ERROR);
}

  /*
   * waveInAddBuffer sends buffer to get data
   * MM_WIM_DATA lParam->WAVEHDR dwBytesRecorded =>how much data actually in buffer
   */

static int current_record_chans = 0, current_record_datum_size = 0;

DWORD CALLBACK next_input_buffer(HWAVEIN w, UINT msg, DWORD user_data, DWORD p1, DWORD p2)
{
  if (msg == WIM_DATA)
    {
      /* grab data */
      /* p1->dwBytesRecorded */
    }
  return(0);
}

int mus_audio_open_input(int ur_dev, int srate, int chans, int format, int size) 
{
  WAVEFORMATEX wf;
  int dev;
  win_in_err = 0;
  dev = MUS_AUDIO_DEVICE(ur_dev);
  wf.nChannels = chans;
  current_record_chans = chans;

  wf.wFormatTag = WAVE_FORMAT_PCM;
  wf.cbSize = 0;
  if (format == MUS_UBYTE) 
    {
      wf.wBitsPerSample = 8;
      current_record_datum_size = 1;
    }
  else 
    {
      wf.wBitsPerSample = 16;
      current_record_datum_size = 2;
    }
  wf.nSamplesPerSec = srate;
  wf.nBlockAlign = chans * current_datum_size;
  wf.nAvgBytesPerSec = wf.nBlockAlign * wf.nSamplesPerSec;

  rec_wh.dwBufferLength = size * current_record_datum_size;
  rec_wh.dwFlags = 0;
  rec_wh.dwLoops = 0;
  rec_wh.lpData = (char *)CALLOC(rec_wh.dwBufferLength, sizeof(char));
  if ((rec_wh.lpData) == 0) 
    RETURN_ERROR_EXIT(MUS_AUDIO_SIZE_NOT_AVAILABLE,
		      mus_format("can't allocated %d bytes for input buffer of %d (%s)",
				 size, dev, mus_audio_device_name(dev)));
  win_in_err = waveInOpen(&record_fd, WAVE_MAPPER, &wf, (DWORD)next_input_buffer, 0, CALLBACK_FUNCTION);
  if (win_in_err) 
    {
      FREE(rec_wh.lpData);
      RETURN_ERROR_EXIT(MUS_AUDIO_DEVICE_NOT_AVAILABLE,
			mus_format("can't open input device %d (%s)",
				   dev, mus_audio_device_name(dev)));
    }
  win_in_err = waveInPrepareHeader(record_fd, &(rec_wh), sizeof(WAVEHDR));
  if (win_in_err) 
    {
      FREE(rec_wh.lpData);
      waveInClose(record_fd);
      RETURN_ERROR_EXIT(MUS_AUDIO_CONFIGURATION_NOT_AVAILABLE,
			mus_format("can't prepare input 'header' for %d (%s)",
				   dev, mus_audio_device_name(dev))); 
    }
  return(MUS_NO_ERROR);
}

int mus_audio_read(int line, char *buf, int bytes) 
{
  win_in_err = 0;
  return(MUS_ERROR);
}

int mus_audio_mixer_read(int ur_dev, int field, int chan, float *val) 
{
  int dev, sys;
  unsigned long lval;
  MMRESULT err;
  sys = MUS_AUDIO_SYSTEM(ur_dev);
  dev = MUS_AUDIO_DEVICE(ur_dev);
  if (field == MUS_AUDIO_AMP)
    {
      err = auxGetVolume(sys, &lval);
      if (!err)
	{
	  if (chan == 0)
	    val[0] = unlog((unsigned short)(lval>>16));
	  else val[0] = unlog((unsigned short)(lval&0xffff));
	  return(MUS_NO_ERROR);
	}
    }
  RETURN_ERROR_EXIT(MUS_AUDIO_CANT_READ,
		    mus_format("can't read device %d (%s) field %s",
			       dev, mus_audio_device_name(dev),
			       mus_audio_device_name(field)));
}

int mus_audio_mixer_write(int ur_dev, int field, int chan, float *val) 
{
  int dev, sys;
  unsigned long lval;
  MMRESULT err;
  sys = MUS_AUDIO_SYSTEM(ur_dev);
  dev = MUS_AUDIO_DEVICE(ur_dev);
  if (field == MUS_AUDIO_AMP)
    {
      err = auxGetVolume(sys, &lval);
      if (!err)
	{
	  if (chan == 0)
	    lval = (unsigned long)((lval & 0xffff) | (((unsigned short)(val[0]*65535))<<16));
	  else lval = (unsigned long)((lval & 0xffff0000) | ((unsigned short)(val[0]*65535)));
	  err = auxSetVolume(sys, lval);
	  if (err) return(MUS_NO_ERROR);
	}
    }
  RETURN_ERROR_EXIT(MUS_AUDIO_CANT_WRITE,
		    mus_format("can't set device %d (%s) field %s",
			       dev, mus_audio_device_name(dev),
			       mus_audio_device_name(field)));
}

#endif



/* ------------------------------- OSX ----------------------------------------- */

/* this code based primarily on the CoreAudio headers and portaudio pa_mac_core.c,
 *   and to a much lesser extent, coreaudio.pdf and the HAL/Daisy examples.
 */

#ifdef MAC_OSX
#define AUDIO_OK 1

#include <CoreServices/CoreServices.h>
#include <CoreAudio/CoreAudio.h>
/* ./System/Library/Frameworks/CoreAudio.framework/Headers/CoreAudio.h */

static char* osx_error(OSStatus err) 
{
  if (err == noErr) return("no error");
  switch (err) 
    {
    case kAudioHardwareNoError:               return("no error");                         break;
    case kAudioHardwareUnspecifiedError:      return("unspecified audio hardware error"); break;
    case kAudioHardwareNotRunningError:       return("audio hardware not running");       break;
    case kAudioHardwareUnknownPropertyError:  return("unknown property");                 break;
    case kAudioHardwareBadPropertySizeError:  return("bad property");                     break;
    case kAudioHardwareBadDeviceError:        return("bad device");                       break;
    case kAudioHardwareBadStreamError:        return("bad stream");                       break;
    case kAudioHardwareIllegalOperationError: return("illegal operation");                break;
    case kAudioDeviceUnsupportedFormatError:  return("unsupported format");               break;
    case kAudioDevicePermissionsError:        return("device permissions error");         break;
    }
  return("unknown error");
}

char *device_name(AudioDeviceID deviceID, int input_case)
{
  OSStatus err = noErr;
  UInt32 size = 0, msize = 0, trans = 0, trans_size = 0;
  char *name = NULL, *mfg = NULL, *full_name = NULL;
  err =  AudioDeviceGetPropertyInfo(deviceID, 0, false, kAudioDevicePropertyDeviceName, &size, NULL);
  if (err == noErr) err =  AudioDeviceGetPropertyInfo(deviceID, 0, false, kAudioDevicePropertyDeviceManufacturer, &msize, NULL);
  if (err == noErr)
    {
      name = (char *)MALLOC(size + 2);
      err = AudioDeviceGetProperty(deviceID, 0, input_case, kAudioDevicePropertyDeviceName, &size, name);
      mfg = (char *)MALLOC(msize + 2);
      err = AudioDeviceGetProperty(deviceID, 0, input_case, kAudioDevicePropertyDeviceManufacturer, &msize, mfg);
      full_name = (char *)MALLOC(size + msize + 4);
#if HAVE_KAUDIODEVICEPROPERTYTRANSPORTTYPE
      trans_size = sizeof(UInt32);
      err = AudioDeviceGetProperty(deviceID, 0, input_case, kAudioDevicePropertyTransportType, &trans_size, &trans);
      if (err != noErr) 
#endif
	trans = 0;
      if (trans == 0)
	mus_snprintf(full_name, size + msize + 4, "\n  %s: %s", mfg, name);
      else mus_snprintf(full_name, size + msize + 4, "\n  %s: %s ('%c%c%c%c')", 
			mfg, name,
			(char)((trans >> 24) & 0xff), (char)((trans >> 16) & 0xff), (char)((trans >> 8) & 0xff), (char)(trans & 0xff));
      FREE(name);
      FREE(mfg);
    }
  return(full_name);
}	

static void describe_audio_state_1(void) 
{
  OSStatus err = noErr;
  UInt32 num_devices = 0, msize = 0, size = 0, buffer_size = 0, mute = 0, alive = 0;
  Float32 vol;
  int i, j, k;
  pid_t hogger = 0;
  AudioDeviceID *devices = NULL;
  AudioDeviceID device, default_output, default_input;
  AudioStreamBasicDescription desc;
  AudioStreamBasicDescription *descs = NULL;
  int formats = 0, input_case = FALSE, m;
  err = AudioHardwareGetPropertyInfo(kAudioHardwarePropertyDevices, &msize, NULL);
  if (err != noErr) return;
  num_devices = msize / sizeof(AudioDeviceID);
  if (num_devices <= 0) return;
  devices = (AudioDeviceID *)MALLOC(msize);
  size = sizeof(AudioDeviceID);
  err = AudioHardwareGetProperty(kAudioHardwarePropertyDefaultInputDevice, &size, &default_input);
  if (err != noErr) default_input = 55555555; /* unsigned int -- I want some value that won't happen! */
  size = sizeof(AudioDeviceID);
  err = AudioHardwareGetProperty(kAudioHardwarePropertyDefaultOutputDevice, &size, &default_output);
  if (err != noErr) default_output = 55555555;
  err = AudioHardwareGetProperty(kAudioHardwarePropertyDevices, &msize, (void *)devices);	
  mus_snprintf(audio_strbuf, PRINT_BUFFER_SIZE, "found %d audio device%s", 
	       (int)num_devices, (num_devices != 1) ? "s" : "");
  pprint(audio_strbuf);
  for (m = 0; m < 2; m++)
    {
      for (i = 0; i < num_devices; i++)
	{
	  device = devices[i];
	  pprint(device_name(device, input_case));
	  if (input_case)
	    {
	      if (device == default_input) 
		pprint(" (default input)"); 
	      else pprint(" (input)");
	    }
	  else
	    {
	      if (device == default_output) 
		pprint(" (default output)"); 
	      else pprint(" (output)");
	    }
	  size = sizeof(pid_t);
	  err = AudioDeviceGetProperty(device, 0, input_case, kAudioDevicePropertyHogMode, &size, &hogger);
	  if ((err == noErr) && (hogger >= 0))
	    {
	      mus_snprintf(audio_strbuf, PRINT_BUFFER_SIZE, " currently owned (exclusively) by process %d", (int)hogger);
	      pprint(audio_strbuf); 
	    }
	  size = sizeof(UInt32);
	  err = AudioDeviceGetProperty(device, 0, input_case, kAudioDevicePropertyDeviceIsAlive, &size, &alive);
	  if ((err == noErr) && (alive == 0))
	    pprint(" disconnected?");
	  size = sizeof(UInt32);
	  err = AudioDeviceGetProperty(device, 0, input_case, kAudioDevicePropertyBufferSize, &size, &buffer_size);
	  if (err != noErr) buffer_size = 0;
	  size = sizeof(AudioStreamBasicDescription);
	  err = AudioDeviceGetProperty(device, 0, input_case, kAudioDevicePropertyStreamFormat, &size, &desc);
	  if (err == noErr) 
	    {
	      unsigned int trans;
	      trans = (unsigned int)(desc.mFormatID);
	      mus_snprintf(audio_strbuf, PRINT_BUFFER_SIZE, "\n    srate: %d, chans: %d, bits/sample: %d, format: %c%c%c%c",
			   (int)(desc.mSampleRate), 
			   (int)(desc.mChannelsPerFrame), 
			   (int)(desc.mBitsPerChannel),
			   (trans >> 24) & 0xff, (trans >> 16) & 0xff, (trans >> 8) & 0xff, trans & 0xff);
	      pprint(audio_strbuf);
	      if (buffer_size > 0)
		{
		  mus_snprintf(audio_strbuf, PRINT_BUFFER_SIZE, ", buf: %d", (int)buffer_size);
		  pprint(audio_strbuf);
		}
	      if ((int)(desc.mFormatFlags) != 0) /* assuming "PCM" here */
		{
		  int flags;
		  flags = ((int)(desc.mFormatFlags));
		  pprint("\n    flags: ");
		  mus_snprintf(audio_strbuf, PRINT_BUFFER_SIZE, "%s%s%s%s%s%s",
			       (flags & kLinearPCMFormatFlagIsFloat) ? "float " : "",
			       (flags & kLinearPCMFormatFlagIsBigEndian) ? "big-endian " : "",
			       (flags & kLinearPCMFormatFlagIsSignedInteger) ? "signed-int " : "",
			       (flags & kLinearPCMFormatFlagIsPacked) ? "packed " : "",
			       (flags & kLinearPCMFormatFlagIsAlignedHigh) ? "aligned-high " : "",
#if HAVE_KLINEARPCMFORMATFLAGISNONINTERLEAVED
			       (flags & kLinearPCMFormatFlagIsNonInterleaved) ? "non-interleaved " : ""
#else
		               ""
#endif
			       );
		  pprint(audio_strbuf);
		}

	      if ((int)(desc.mChannelsPerFrame) > 0)
		{
		  pprint("\n    vols:");
		  for (j = 0; j <= (int)(desc.mChannelsPerFrame); j++)
		    {
		      size = sizeof(Float32);
		      err = AudioDeviceGetProperty(device, j, input_case, kAudioDevicePropertyVolumeScalar, &size, &vol);
		      if (err == noErr) 
			{
			  mus_snprintf(audio_strbuf, PRINT_BUFFER_SIZE, " %s%.3f", 
				       (j == 0) ? "master: " : "", 
				       vol);
			  pprint(audio_strbuf); 
			}
		      
		      if (j > 0)
			{
			  size = sizeof(UInt32);
			  err = AudioDeviceGetProperty(device, j, input_case, kAudioDevicePropertyMute, &size, &mute);
			  if ((err == noErr) && (mute == 1))
			    {
			      mus_snprintf(audio_strbuf, PRINT_BUFFER_SIZE, " (muted)");
			      pprint(audio_strbuf); 
			    }
			}
		    }
		}
	    }
	  size = 0;
	  err = AudioDeviceGetPropertyInfo(device, 0, input_case, kAudioDevicePropertyStreamFormats, &size, NULL);
	  formats = size / sizeof(AudioStreamBasicDescription);
	  if (formats > 1)
	    {
	      descs = (AudioStreamBasicDescription *)CALLOC(formats, sizeof(AudioStreamBasicDescription));
	      size = formats * sizeof(AudioStreamBasicDescription);
	      err = AudioDeviceGetProperty(device, 0, input_case, kAudioDevicePropertyStreamFormats, &size, descs);
	      if (err == noErr) 
		{
		  mus_snprintf(audio_strbuf, PRINT_BUFFER_SIZE, "\n    This device supports %d formats: ", formats); 
		  pprint(audio_strbuf);
		  for (k = 0; k < formats; k++)
		    {
		      unsigned int trans;
		      trans = (unsigned int)(descs[k].mFormatID);
		      mus_snprintf(audio_strbuf, PRINT_BUFFER_SIZE, "\n      srate: %d, chans: %d, bits/sample: %d, format: %c%c%c%c",
				   (int)(descs[k].mSampleRate), 
				   (int)(descs[k].mChannelsPerFrame), 
				   (int)(descs[k].mBitsPerChannel),
				   (trans >> 24) & 0xff, (trans >> 16) & 0xff, (trans >> 8) & 0xff, trans & 0xff);					 
		      pprint(audio_strbuf);
		    }
		}
	      FREE(descs);
	    }
	  pprint("\n");
	}
      input_case = TRUE;
    }
  if (devices) FREE(devices);
}

#define MAX_BUFS 4
static char **bufs = NULL;
static int in_buf = 0, out_buf = 0;

static OSStatus writer(AudioDeviceID inDevice, 
		       const AudioTimeStamp *inNow, 
		       const AudioBufferList *InputData, const AudioTimeStamp *InputTime, 
		       AudioBufferList *OutputData, const AudioTimeStamp *OutputTime, 
		       void *appGlobals)
{
  AudioBuffer abuf;
  char *aplbuf, *sndbuf;
  abuf = OutputData->mBuffers[0];
  aplbuf = (char *)(abuf.mData);
  sndbuf = bufs[out_buf];
  memmove((void *)aplbuf, (void *)sndbuf, abuf.mDataByteSize);
  out_buf++;
  if (out_buf >= MAX_BUFS) out_buf = 0;
  return(noErr);
}

static OSStatus reader(AudioDeviceID inDevice, 
		       const AudioTimeStamp *inNow, 
		       const AudioBufferList *InputData, const AudioTimeStamp *InputTime, 
		       AudioBufferList *OutputData, const AudioTimeStamp *OutputTime, 
		       void *appGlobals)
{
  AudioBuffer abuf;
  char *aplbuf, *sndbuf;
  abuf = InputData->mBuffers[0];
  aplbuf = (char *)(abuf.mData);
  sndbuf = bufs[out_buf];
  memmove((void *)sndbuf, (void *)aplbuf, abuf.mDataByteSize);
  out_buf++;
  if (out_buf >= MAX_BUFS) out_buf = 0;
  return(noErr);
}


static AudioDeviceID device = kAudioDeviceUnknown;
static int writing = FALSE, open_for_input = FALSE;

int mus_audio_close(int line) 
{
  OSStatus err = noErr;
  UInt32 sizeof_running;
  UInt32 running;
  if (open_for_input)
    {
      in_buf = 0;
      err = AudioDeviceStop(device, (AudioDeviceIOProc)reader);
      if (err == noErr) 
	err = AudioDeviceRemoveIOProc(device, (AudioDeviceIOProc)reader);
    }
  else
    {
      if ((in_buf > 0) && (writing == FALSE))
	{
	  /* short enough sound that we never got started? */
	  err = AudioDeviceAddIOProc(device, (AudioDeviceIOProc)writer, NULL);
	  if (err == noErr)
	    err = AudioDeviceStart(device, (AudioDeviceIOProc)writer); /* writer will be called right away */
	  if (err == noErr)
	    writing = TRUE;
	}
      if (writing)
	{
	  /* send out waiting buffers */
	  sizeof_running = sizeof(UInt32);
	  while (in_buf == out_buf)
	    {
	      err = AudioDeviceGetProperty(device, 0, false, kAudioDevicePropertyDeviceIsRunning, &sizeof_running, &running);
	    }
	  while (in_buf != out_buf)
	    {
	      err = AudioDeviceGetProperty(device, 0, false, kAudioDevicePropertyDeviceIsRunning, &sizeof_running, &running);
	    }
	  in_buf = 0;
	  err = AudioDeviceStop(device, (AudioDeviceIOProc)writer);
	  if (err == noErr) 
	    err = AudioDeviceRemoveIOProc(device, (AudioDeviceIOProc)writer);
	  writing = FALSE;
	}
    }
  device = kAudioDeviceUnknown;
  if (err == noErr)
    return(MUS_NO_ERROR);
  return(MUS_ERROR);
}

enum {CONVERT_NOT, CONVERT_COPY, CONVERT_SKIP, CONVERT_COPY_AND_SKIP, CONVERT_SKIP_N, CONVERT_COPY_AND_SKIP_N};
static int conversion_choice = CONVERT_NOT;
static float conversion_multiplier = 1.0;
static int dac_out_chans, dac_out_srate;
static int incoming_out_chans = 1, incoming_out_srate = 44100;
static int fill_point = 0;
static unsigned int bufsize;

/* I'm getting bogus buffer sizes from the audio conversion stuff from Apple,
 *   and I think AudioConvert doesn't handle cases like 4->6 chans correctly
 *   so, I'll just do the conversions myself -- there is little need here
 *   for non-integer srate conversion anyway, and the rest is trivial.
 */

int mus_audio_open_output(int dev, int srate, int chans, int format, int size) 
{
  OSStatus err = noErr;
  UInt32 sizeof_device, sizeof_format, sizeof_bufsize;
  AudioStreamBasicDescription device_desc;
  sizeof_device = sizeof(AudioDeviceID);
  sizeof_bufsize = sizeof(unsigned int);
  err = AudioHardwareGetProperty(kAudioHardwarePropertyDefaultOutputDevice, &sizeof_device, (void *)(&device));
  bufsize = 4096;
  if (err == noErr) 
    err = AudioDeviceGetProperty(device, 0, false, kAudioDevicePropertyBufferSize, &sizeof_bufsize, &bufsize);
  if (err != noErr) 
    {
      fprintf(stderr,"open audio output err: %d %s\n", (int)err, osx_error(err));
      return(MUS_ERROR);
    }
  /* now check for srate/chan mismatches and so on */
  sizeof_format = sizeof(AudioStreamBasicDescription);
  err = AudioDeviceGetProperty(device, 0, false, kAudioDevicePropertyStreamFormat, &sizeof_format, &device_desc);
  if (err != noErr)
    {
      fprintf(stderr,"open audio output (get device format) err: %d %s\n", (int)err, osx_error(err));
      return(MUS_ERROR);
    }
  /* current DAC state: device_desc.mChannelsPerFrame, (int)(device_desc.mSampleRate) */
  /* apparently get stream format can return noErr but chans == 0?? */
  if ((device_desc.mChannelsPerFrame != chans) || 
      ((int)(device_desc.mSampleRate) != srate))
    {
      /* try to match DAC settings to current sound */
      device_desc.mChannelsPerFrame = chans;
      device_desc.mSampleRate = srate;
      device_desc.mBytesPerPacket = chans * 4; /* assume 1 frame/packet and float32 data */
      device_desc.mBytesPerFrame = chans * 4;
      sizeof_format = sizeof(AudioStreamBasicDescription);
      err = AudioDeviceSetProperty(device, 0, 0, false, kAudioDevicePropertyStreamFormat, sizeof_format, &device_desc);
      if (err != noErr)
	{
	  /* it must have failed for some reason -- look for closest match available */
	  sizeof_format = sizeof(AudioStreamBasicDescription);
	  err = AudioDeviceGetProperty(device, 0, false, kAudioDevicePropertyStreamFormatMatch, &sizeof_format, &device_desc);
	  if (err == noErr)
	    {
	      /* match suggests: device_desc.mChannelsPerFrame, (int)(device_desc.mSampleRate) */
	      /* try to set DAC to reflect that match */
	      /* a bug here in emagic 2|6 -- we can get 6 channel match, but then can't set it?? */
	      sizeof_format = sizeof(AudioStreamBasicDescription);
	      err = AudioDeviceSetProperty(device, 0, 0, false, kAudioDevicePropertyStreamFormat, sizeof_format, &device_desc);
	      if (err != noErr) 
		{
		  /* no luck -- get current DAC settings at least */
		  sizeof_format = sizeof(AudioStreamBasicDescription);
		  err = AudioDeviceGetProperty(device, 0, false, kAudioDevicePropertyStreamFormat, &sizeof_format, &device_desc);
		  if (err != noErr)
		    {
		      fprintf(stderr,"can't get?: %s ", osx_error(err));
		      return(MUS_ERROR);
		    }
		}
	    }
	  else 
	    {
	      /* nothing matches? -- get current DAC settings */
	      sizeof_format = sizeof(AudioStreamBasicDescription);
	      err = AudioDeviceGetProperty(device, 0, false, kAudioDevicePropertyStreamFormat, &sizeof_format, &device_desc);
	    }
	}
    }
  /* now DAC claims it is ready for device_desc.mChannelsPerFrame, (int)(device_desc.mSampleRate) */
  dac_out_chans = device_desc.mChannelsPerFrame; /* use better variable names */
  dac_out_srate = (int)(device_desc.mSampleRate);
  open_for_input = FALSE;
  if (bufs == NULL)
    {
      int i;
      bufs = (char **)CALLOC(MAX_BUFS, sizeof(char *));
      for (i = 0; i < MAX_BUFS; i++)
	bufs[i] = (char *)CALLOC(bufsize, sizeof(char));
    }
  in_buf = 0;
  out_buf = 0;
  fill_point = 0;
  incoming_out_srate = srate;
  incoming_out_chans = chans;
  if (incoming_out_chans == dac_out_chans)
    {
      if (incoming_out_srate == dac_out_srate)
	{
	  conversion_choice = CONVERT_NOT;
	  conversion_multiplier = 1.0;
	}
      else 
	{
	  /* here we don't get very fancy -- assume dac/2=in */
	  conversion_choice = CONVERT_COPY;
	  conversion_multiplier = 2.0;
	}
    }
  else
    {
      if (incoming_out_srate == dac_out_srate)
	{
	  if ((dac_out_chans == 2) &&(incoming_out_chans == 1)) /* the usual case */
	    {
	      conversion_choice = CONVERT_SKIP;
	      conversion_multiplier = 2.0;
	    }
	  else
	    {
	      conversion_choice = CONVERT_SKIP_N;
	      conversion_multiplier = ((float)dac_out_chans / (float)incoming_out_chans);
	    }
	}
      else 
	{
	  if ((dac_out_chans == 2) &&(incoming_out_chans == 1)) /* the usual case */
	    {
	      conversion_choice = CONVERT_COPY_AND_SKIP;
	      conversion_multiplier = 4.0;
	    }
	  else
	    {
	      conversion_choice = CONVERT_COPY_AND_SKIP_N;
	      conversion_multiplier = ((float)dac_out_chans / (float)incoming_out_chans) * 2;
	    }
	}
    }
  return(MUS_NO_ERROR);
}

static void convert_incoming(char *to_buf, int fill_point, int lim, char *buf)
{
  int i, j, k, jc, kc, ic;
  switch (conversion_choice)
    {
    case CONVERT_NOT:
      /* no conversion needed */
      for (i = 0; i < lim; i++)
	to_buf[i + fill_point] = buf[i];
      break;
    case CONVERT_COPY:
      /* copy sample to mimic lower srate */
      for (i = 0, j = fill_point; i < lim; i += 8, j += 16)
	for (k = 0; k < 8; k++)
	  {
	    to_buf[j + k] = buf[i + k];
	    to_buf[j + k + 8] = buf[i + k];
	  }
      break;
    case CONVERT_SKIP:
      /* skip sample for empty chan */
      for (i = 0, j = fill_point; i < lim; i += 4, j += 8)
	for (k = 0; k < 4; k++)
	  {
	    to_buf[j + k] = buf[i + k];
	    to_buf[j + k + 4] = 0;
	  }
      break;
    case CONVERT_SKIP_N:
      /* copy incoming_out_chans then skip up to dac_out_chans */
      jc = dac_out_chans * 4;
      ic = incoming_out_chans * 4;
      for (i = 0, j = fill_point; i < lim; i += ic, j += jc)
	{
	  for (k = 0; k < ic; k++) to_buf[j + k] = buf[i + k];
	  for (k = ic; k < jc; k++) to_buf[j + k] = 0;
	}
      break;
    case CONVERT_COPY_AND_SKIP:
      for (i = 0, j = fill_point; i < lim; i += 4, j += 16)
	for (k = 0; k < 4; k++)
	  {
	    to_buf[j + k] = buf[i + k];
	    to_buf[j + k + 4] = 0;
	    to_buf[j + k + 8] = buf[i + k];
	    to_buf[j + k + 12] = 0;
	  }
      break;
    case CONVERT_COPY_AND_SKIP_N:
      /* copy for each active chan, skip rest */
      jc = dac_out_chans * 8;
      ic = incoming_out_chans * 4;
      kc = dac_out_chans * 4;
      for (i = 0, j = fill_point; i < lim; i += ic, j += jc)
	{
	  for (k = 0; k < ic; k++) 
	    {
	      to_buf[j + k] = buf[i + k];
	      to_buf[j + k + kc] = buf[i + k];	      
	    }
	  for (k = ic; k < kc; k++) 
	    {
	      to_buf[j + k] = 0;
	      to_buf[j + k + kc] = 0;
	    }
	}
      break;
    }
}

int mus_audio_write(int line, char *buf, int bytes) 
{
  OSStatus err = noErr;
  int lim, bp, out_bytes;
  UInt32 sizeof_running;
  UInt32 running;
  char *to_buf;
  to_buf = bufs[in_buf];
  out_bytes = (int)(bytes * conversion_multiplier);
  if ((fill_point + out_bytes) > bufsize)
    out_bytes = bufsize - fill_point;
  lim = (int)(out_bytes / conversion_multiplier);
  if (writing == FALSE)
    {
      convert_incoming(to_buf, fill_point, lim, buf);
      fill_point += out_bytes;
      if (fill_point >= bufsize)
	{
	  in_buf++;
	  fill_point = 0;
	  if (in_buf == MAX_BUFS)
	    {
	      in_buf = 0;
	      err = AudioDeviceAddIOProc(device, (AudioDeviceIOProc)writer, NULL);
	      if (err == noErr)
		err = AudioDeviceStart(device, (AudioDeviceIOProc)writer); /* writer will be called right away */
	      if (err == noErr)
		{
		  writing = TRUE;
		  return(MUS_NO_ERROR);
		}
	      else return(MUS_ERROR);
	    }
	}
      return(MUS_NO_ERROR);
    }
  if ((fill_point == 0) && (in_buf == out_buf))
    {
      bp = out_buf;
      sizeof_running = sizeof(UInt32);
      while (bp == out_buf)
	{
	  /* i.e. just kill time without hanging */
	  err = AudioDeviceGetProperty(device, 0, false, kAudioDevicePropertyDeviceIsRunning, &sizeof_running, &running);
	  /* usleep(10); */
	}
    }
  to_buf = bufs[in_buf];
  if (fill_point == 0) memset((void *)to_buf, 0, bufsize);
  convert_incoming(to_buf, fill_point, lim, buf);
  fill_point += out_bytes;
  if (fill_point >= bufsize)
    {
      in_buf++;
      fill_point = 0;
      if (in_buf >= MAX_BUFS) in_buf = 0;
    }
  return(MUS_NO_ERROR);
}

int mus_audio_open_input(int dev, int srate, int chans, int format, int size) 
{
  OSStatus err = noErr;
  UInt32 sizeof_device;
  UInt32 sizeof_bufsize;
  sizeof_device = sizeof(AudioDeviceID);
  sizeof_bufsize = sizeof(unsigned int);
  err = AudioHardwareGetProperty(kAudioHardwarePropertyDefaultInputDevice, &sizeof_device, (void *)(&device));
  bufsize = 4096;
  if (err == noErr) 
    err = AudioDeviceGetProperty(device, 0, true, kAudioDevicePropertyBufferSize, &sizeof_bufsize, &bufsize);
  if (err != noErr) 
    {
      fprintf(stderr,"open audio input err: %d %s\n", (int)err, osx_error(err));
      return(MUS_ERROR);
    }
  open_for_input = TRUE;
  /* assume for now that recorder (higher level) will enforce match */
  if (bufs == NULL)
    {
      int i;
      bufs = (char **)CALLOC(MAX_BUFS, sizeof(char *));
      for (i = 0; i < MAX_BUFS; i++)
	bufs[i] = (char *)CALLOC(bufsize, sizeof(char));
    }
  in_buf = 0;
  out_buf = 0;
  fill_point = 0;
  incoming_out_srate = srate;
  incoming_out_chans = chans;
  err = AudioDeviceAddIOProc(device, (AudioDeviceIOProc)reader, NULL);
  if (err == noErr)
    err = AudioDeviceStart(device, (AudioDeviceIOProc)reader);
  if (err != noErr) 
    {
      fprintf(stderr,"add open audio input err: %d %s\n", (int)err, osx_error(err));
      return(MUS_ERROR);
    }
  return(MUS_NO_ERROR);
}

int mus_audio_read(int line, char *buf, int bytes) 
{
  OSStatus err = noErr;
  int bp;
  UInt32 sizeof_running;
  UInt32 running;
  char *to_buf;
  if (in_buf == out_buf)
    {
      bp = out_buf;
      sizeof_running = sizeof(UInt32);
      while (bp == out_buf)
	{
	  err = AudioDeviceGetProperty(device, 0, true, kAudioDevicePropertyDeviceIsRunning, &sizeof_running, &running);
	  if (err != noErr) 
	    fprintf(stderr,"wait err: %s ", osx_error(err));
	}
    }
  to_buf = bufs[in_buf];
  if (bytes <= bufsize)
    memmove((void *)buf, (void *)to_buf, bytes);
  else memmove((void *)buf, (void *)to_buf, bufsize);
  in_buf++;
  if (in_buf >= MAX_BUFS) in_buf = 0;
  return(MUS_ERROR);
}

static int max_chans(AudioDeviceID device, int input)
{
  int maxc = 0, formats, k;
  UInt32 size;
  OSStatus err;
  AudioStreamBasicDescription desc;
  AudioStreamBasicDescription *descs;
  size = sizeof(AudioStreamBasicDescription);
  err = AudioDeviceGetProperty(device, 0, input, kAudioDevicePropertyStreamFormat, &size, &desc);
  if (err == noErr) 
    {
      maxc = (int)(desc.mChannelsPerFrame);
      size = 0;
      err = AudioDeviceGetPropertyInfo(device, 0, input, kAudioDevicePropertyStreamFormats, &size, NULL);
      formats = size / sizeof(AudioStreamBasicDescription);
      if (formats > 1)
	{
	  descs = (AudioStreamBasicDescription *)CALLOC(formats, sizeof(AudioStreamBasicDescription));
	  size = formats * sizeof(AudioStreamBasicDescription);
	  err = AudioDeviceGetProperty(device, 0, input, kAudioDevicePropertyStreamFormats, &size, descs);
	  if (err == noErr) 
	    for (k = 0; k < formats; k++)
	      if ((int)(descs[k].mChannelsPerFrame) > maxc) maxc = (int)(descs[k].mChannelsPerFrame);
	  FREE(descs);
	}
    }
  else fprintf(stderr, "read chans hit: %s\n", osx_error(err));
  return(maxc);
}

int mus_audio_mixer_read(int dev1, int field, int chan, float *val)
{
  AudioDeviceID dev = kAudioDeviceUnknown;
  OSStatus err = noErr;
  UInt32 size;
  Float32 amp;
  int i, curdev, in_case = false;
  switch (field) 
    {
    case MUS_AUDIO_AMP:   
      size = sizeof(AudioDeviceID);
      err = AudioHardwareGetProperty(kAudioHardwarePropertyDefaultOutputDevice, &size, &dev);
      size = sizeof(Float32);
      err = AudioDeviceGetProperty(dev, chan + 1, false, kAudioDevicePropertyVolumeScalar, &size, &amp);
      if (err == noErr)
	val[0] = (Float)amp;
      else val[0] = 0.0;
      break;
    case MUS_AUDIO_CHANNEL: 
      curdev = MUS_AUDIO_DEVICE(dev1);
      size = sizeof(AudioDeviceID);
      in_case = ((curdev == MUS_AUDIO_MICROPHONE) || (curdev == MUS_AUDIO_LINE_IN));
      if (in_case)
	err = AudioHardwareGetProperty(kAudioHardwarePropertyDefaultInputDevice, &size, &dev);
      else err = AudioHardwareGetProperty(kAudioHardwarePropertyDefaultOutputDevice, &size, &dev);
      if (err != noErr) fprintf(stderr, "get default: %s\n", osx_error(err));
      val[0] = max_chans(dev, (in_case) ? true : false);
      break;
    case MUS_AUDIO_SRATE: 
      val[0] = 44100;
      break;
    case MUS_AUDIO_FORMAT:
      /* never actually used except perhaps play.scm */
      val[0] = 1.0;
      val[1] = MUS_BFLOAT;
      break;
    case MUS_AUDIO_PORT:
      i = 0;
      if (1 < chan) val[1] = MUS_AUDIO_MICROPHONE;
      if (2 < chan) val[2] = MUS_AUDIO_DAC_OUT;
      val[0] = 2;
      break;
    default: 
      return(MUS_ERROR);
      break;
    }
  return(MUS_NO_ERROR);
}

int mus_audio_mixer_write(int dev1, int field, int chan, float *val) 
{
  AudioDeviceID dev = kAudioDeviceUnknown;
  OSStatus err = noErr;
  Boolean writable;
  UInt32 size;
  Float32 amp;
  switch (field) 
    {
    case MUS_AUDIO_AMP:   
      size = sizeof(AudioDeviceID);
      err = AudioHardwareGetProperty(kAudioHardwarePropertyDefaultOutputDevice, &size, (void *)(&dev));
      err = AudioDeviceGetPropertyInfo(dev, chan + 1, false, kAudioDevicePropertyVolumeScalar, NULL, &writable); /* "false" -> output */
      amp = (Float32)(val[0]);
      if ((err == kAudioHardwareNoError) && (writable))
	err = AudioDeviceSetProperty(dev, NULL, chan + 1, false, kAudioDevicePropertyVolumeScalar, sizeof(Float32), &amp);
      break;
    default: 
      return(MUS_ERROR);
      break;
    }
  return(MUS_NO_ERROR);
}

void mus_audio_save(void) {}
void mus_audio_restore(void) {}
int mus_audio_initialize(void) {return(MUS_NO_ERROR);}
int mus_audio_systems(void) {return(1);}
char *mus_audio_system_name(int system) {return("Mac OSX");}

char *mus_audio_moniker(void) {return("Mac OSX audio");}
#endif



/* -------------------------------- ESD -------------------------------- */

/* ESD audio IO for Linux                   *
 * Nick Bailey <nick@bailey-family.org.uk>  *
 * also n.bailey@elec.gla.ac.uk             */

/* ESD is pretty well undocumented, and I've not looked at snd before, *
 * but here goes...                                                    *
 *                                                                     *
 * History:                                                            *
 * 14th Nov 2000: copied SUN drivers here and started to hack.  NJB.   *
 *                                                                     */

#ifdef ESD
#define AUDIO_OK

#include <esd.h>

static int esd_play_sock = -1;
static int esd_rec_sock  = -1;
static char esd_name[] = "Enlightened Sound Daemon";
static int swap_end, resign; /* How to handle samples on write */

int mus_audio_initialize(void) {return(MUS_NO_ERROR);}
int mus_audio_systems(void) {return(1);}
char *mus_audio_system_name(int system) {return esd_name;}
static char our_name[LABEL_BUFFER_SIZE];
char *mus_audio_moniker(void) 
{
#ifdef ESD_VERSION
  #ifdef AUDIOFILE_VERSION
    mus_snprintf(our_name, LABEL_BUFFER_SIZE, "%s: %s (Audiofile %s)", esd_name, ESD_VERSION, AUDIOFILE_VERSION);
  #else
    mus_snprintf(our_name, LABEL_BUFFER_SIZE, "%s: %s", esd_name, ESD_VERSION);
  #endif
  return(our_name);
#else
  return(esd_name);
#endif
}

int mus_audio_api(void) {return(0);}

#define RETURN_ERROR_EXIT(Error_Type, Audio_Line, Ur_Error_Message) \
  do { char *Error_Message; Error_Message = Ur_Error_Message; \
    if (esd_play_sock != -1) close(esd_play_sock); \
    if (esd_rec_sock != -1) close(esd_rec_sock); \
    if (Error_Message) \
      {MUS_STANDARD_ERROR(Error_Type, Error_Message); FREE(Error_Message);} \
    else MUS_STANDARD_ERROR(Error_Type, mus_error_to_string(Error_Type)); \
    return(MUS_ERROR); \
  } while (0)

/* No we're laughing.  snd think's its talking to a real piece of hardware
   so it'll only try to open it once.  We can just use the socket numbers */

/* REVOLTING HACK!  to_esd_format is called from mus_audio_open, and
   /as a side effect/, sets a flag to tell the write routine whether
   or not to change the endienness of the audio sample data (afaik,
   esd can't do this for us).  Same goes for signed-ness.
   If it gets called from elsewhere, it could be nasty. */

static int to_esd_format(int snd_format)
{
  /* Try this on the Macs: it may be esd expects Bigendian on those */
  switch (snd_format) { /* Only some are supported */
  case MUS_UBYTE:   swap_end = 0; resign = 0; return ESD_BITS8;
  case MUS_LSHORT:  swap_end = 0; resign = 0; return ESD_BITS16;
  case MUS_BSHORT:  swap_end = 1; resign = 0; return ESD_BITS16;
  case MUS_ULSHORT: swap_end = 0; resign = 1; return ESD_BITS16;
  case MUS_UBSHORT: swap_end = 1; resign = 1; return ESD_BITS16;
  }
  return MUS_ERROR;
}

int mus_audio_open_output(int ur_dev, int srate, int chans, int format, int size)
{
  int esd_prop = ESD_STREAM;
  int esd_format;

  if ((esd_format = to_esd_format(format)) == MUS_ERROR)
    RETURN_ERROR_EXIT(MUS_AUDIO_FORMAT_NOT_AVAILABLE, audio_out,
		      mus_format("Can't handle format %d (%s) through esd",
				 format, mus_data_format_name(format)));
  else
    esd_prop |= esd_format;

  if (chans < 1 || chans > 2)
    RETURN_ERROR_EXIT(MUS_AUDIO_CHANNELS_NOT_AVAILABLE, audio_out,
		      mus_format("Can't handle format %d channels through esd",
				 format));
  else 
    esd_prop |= chans == 1 ? ESD_MONO : ESD_STEREO;

  esd_play_sock = esd_play_stream(esd_prop, srate,
				  NULL, "snd playback stream");

  if (esd_play_sock ==  -1)
    RETURN_ERROR_EXIT(MUS_AUDIO_DEVICE_NOT_AVAILABLE, audio_out,
		      mus_format("Sonorus device %d (%s) not available",
				 ur_dev, mus_audio_device_name(ur_dev)));
  else
    return esd_play_sock;
}

int mus_audio_write(int line, char *buf, int bytes)
{
  int written;
  char *to = buf;

  /* Esd can't do endianness or signed/unsigned conversion,
     so it's our problem.  We won't screw up the callers data */

  if (swap_end) {
    char *from = buf;
    char *p;
    int samps = bytes/2;
    p = to = (char *)alloca(bytes);
    while (samps--) {
      *p++ = *(from+1);
      *p++ = *(from);
      from += 2;
    }
  }

  /* Need to do something about sign correction here */

  do {
    written = write(line, to, bytes);
    if (written > 0) {
      bytes -= written;
      to += written;
    }
    else
      RETURN_ERROR_EXIT(MUS_AUDIO_WRITE_ERROR, -1,
			mus_format("write error: %s", strerror(errno)));
  } while (bytes > 0);
  return MUS_NO_ERROR;
}

int mus_audio_close(int line)
{
  esd_close(line);
  if (esd_play_sock == line) esd_play_sock = -1;
  else if (esd_rec_sock == line) esd_rec_sock = -1;
  return MUS_NO_ERROR;
}

int mus_audio_read(int line, char *buf, int bytes)
{
  int bytes_read;

  do {
    bytes_read = read(line, buf, bytes);
    if (bytes_read > 0) { /* 0 -> EOF; we'll regard that as an error */
      bytes -= bytes_read;
      buf += bytes_read;
    } else
      RETURN_ERROR_EXIT(MUS_AUDIO_WRITE_ERROR, -1,
			mus_format("read error: %s", strerror(errno)));
  } while (bytes > 0);
  return MUS_NO_ERROR;
}

int mus_audio_open_input(int ur_dev, int srate, int chans, int format, int size)
{
  int esd_prop = ESD_STREAM;
  int esd_format;

  if ((esd_format = to_esd_format(format)) == MUS_ERROR)
    RETURN_ERROR_EXIT(MUS_AUDIO_FORMAT_NOT_AVAILABLE, audio_out,
		      mus_format("Can't handle format %d (%s) through esd",
				 format, mus_data_format_name(format)));
  else
    esd_prop |= esd_format;

  if (chans < 1 || chans > 2)
    RETURN_ERROR_EXIT(MUS_AUDIO_CHANNELS_NOT_AVAILABLE, audio_out,
		      mus_format("Can't handle format %d channels through esd",
				 chans));
  else 
    esd_prop |= chans == 1 ? ESD_MONO : ESD_STEREO;

  esd_rec_sock = esd_play_stream(esd_prop, srate,
				  NULL, "snd record stream");

  if (esd_rec_sock ==  -1)
    RETURN_ERROR_EXIT(MUS_AUDIO_DEVICE_NOT_AVAILABLE, audio_out,
		      mus_format("Device %d (%s) not available",
				 ur_dev, mus_audio_device_name(ur_dev)));
  else
    return esd_rec_sock;
}

int mus_audio_mixer_read(int ur_dev, int field, int chan, float *val) 
{
  /* Not really sure what to do here.  Mixer is at the other end of the
     socket.  Needs work.  NJB */

  /* int card = MUS_AUDIO_SYSTEM(ur_dev); */
    int device = MUS_AUDIO_DEVICE(ur_dev);

    if (device == MUS_AUDIO_MIXER) {
	val[0] = 0.0;
	return MUS_NO_ERROR;
    }

    if (field == MUS_AUDIO_PORT) {
	val[0] = 1.0;
	return MUS_NO_ERROR;
    }

    switch (field) {
    case MUS_AUDIO_AMP: 
      /* amplitude value */
      val[0] = 1.0;
      break;
    case MUS_AUDIO_SAMPLES_PER_CHANNEL: 
      val[0] = 44100;
      break;
    case MUS_AUDIO_CHANNEL: 
      /* number of channels */
      val[0] = 2.0; 
      if (chan > 1) {
	val[1] = 1.0; 
	val[2] = 2.0; 
      }
      break;
    case MUS_AUDIO_SRATE: 
      /* supported sample rates */
      val[0] = 44100;
      if (chan > 1) {
	val[1] = 8000; 
	val[2] = 48000; 
      }
      break;
    case MUS_AUDIO_FORMAT:
      /* supported formats (ugly...) */
      val[0] = 3.0;
      val[1] = MUS_UBYTE;
      val[2] = MUS_LSHORT;
      val[3] = MUS_BSHORT;
      break;

    case MUS_AUDIO_DIRECTION: /* Needs sorting.  NJB */ 
      /* 0-->playback, 1-->capture */
      val[0] = 0;
      break;

    default: 
      RETURN_ERROR_EXIT(MUS_AUDIO_CANT_READ, -1, NULL);
      /* return(mus_error(MUS_AUDIO_CANT_READ, NULL)); */ /* Bill 14-Nov-02 -- avoid possibly uncaught throw */
      break;
    }
    return(MUS_NO_ERROR);
}


int mus_audio_mixer_write(int ur_dev, int field, int chan, float *val) 
{
  /* Ditto */
  val[0] = 0.0;
  return MUS_NO_ERROR;
}

/* pause can be implemented with play.pause and record.pause */


void mus_audio_save(void)
{
}

void mus_audio_restore(void)
{
}

void describe_audio_state_1(void)
{
  pprint("Enlightened Sound Daemon via socket connexion to default host");
}

#endif


/* ------------------------------- JACK ----------------------------------------- */

#if HAVE_JACK
#define AUDIO_OK
#include <jack/jack.h>

static jack_client_t *dac = NULL;
static jack_port_t *dac_port = NULL;

static int write_buffer(jack_nframes_t nframes, void *arg)
{
  jack_default_audio_sample_t *dac_out = (jack_default_audio_sample_t *)jack_port_get_buffer(dac_port, nframes);
  /* write to dac_out -- ack_default_audio_sample_t is assumed to be float */
  
  return(0);
}

static void quit_dac(void *arg)
{
  mus_audio_close(0);
}

int mus_audio_open_output(int dev, int srate, int chans, int format, int size) 
{
  /* jack_get_sample_rate(dac)
     but no chans?
  */
  dac = jack_client_new(<name...>);
  if (dac == NULL) return(MUS_ERROR);
  jack_set_process_callback(dac, write_buffer, 0);
  jack_on_shutdown(dac, quit_dac, 0);
  dac_port = jack_port_register(dac, "output", JACK_DEFAULT_AUDIO_TYPE, JackPortIsOutput, 0);
  jack_activate(dac);
  jack_connect(dac, "output", "alsa_pcm:out_1"); /* is this channel 0 or something -- how to handle n chans? */
  return(MUS_NO_ERROR);
}

int mus_audio_write(int line, char *buf, int bytes) 
{
  return(MUS_ERROR);
}

int mus_audio_close(int line) 
{
  jack_deactivate(dac);
  jack_port_unregister(dac, dac_port);
  jack_client_close(dac);
  return(MUS_NO_ERROR);
}

/* just implement playing (output) */
static void describe_audio_state_1(void) {pprint("jack audio");}
int mus_audio_open_input(int dev, int srate, int chans, int format, int size) {return(MUS_ERROR);}
int mus_audio_read(int line, char *buf, int bytes) {return(MUS_ERROR);}
int mus_audio_mixer_read(int dev, int field, int chan, float *val) {return(MUS_ERROR);}
int mus_audio_mixer_write(int dev, int field, int chan, float *val) {return(MUS_ERROR);}
void mus_audio_save(void) {}
void mus_audio_restore(void) {}
int mus_audio_initialize(void) {return(MUS_ERROR);}
int mus_audio_systems(void) {return(1);}
char *mus_audio_system_name(int system) {return("linux jack");}
char *mus_audio_moniker(void) {return("jack");}
#endif



/* ------------------------------- STUBS ----------------------------------------- */

#ifndef AUDIO_OK
static void describe_audio_state_1(void) {pprint("audio stubbed out");}
int mus_audio_open_output(int dev, int srate, int chans, int format, int size) {return(MUS_ERROR);}
int mus_audio_open_input(int dev, int srate, int chans, int format, int size) {return(MUS_ERROR);}
int mus_audio_write(int line, char *buf, int bytes) {return(MUS_ERROR);}
int mus_audio_close(int line) {return(MUS_ERROR);}
int mus_audio_read(int line, char *buf, int bytes) {return(MUS_ERROR);}
int mus_audio_mixer_read(int dev, int field, int chan, float *val) {return(MUS_ERROR);}
int mus_audio_mixer_write(int dev, int field, int chan, float *val) {return(MUS_ERROR);}
void mus_audio_save(void) {}
void mus_audio_restore(void) {}
int mus_audio_initialize(void) {return(MUS_ERROR);}
int mus_audio_systems(void) {return(0);}
char *mus_audio_system_name(int system) {return("unknown");}
char *mus_audio_moniker(void) {return("unknown audio");}
#endif



static char *save_it = NULL;
static int print_it = 1;
static int save_it_len = 0;
static int save_it_loc = 0;

static void check_save_it(int loc)
{
  if (loc >= save_it_len)
    {
      save_it_len += 1024;
      save_it = (char *)REALLOC(save_it, save_it_len * sizeof(char));
    }
}

static void pprint(char *str)
{
  int i, len;
  if ((str) && (*str))
    {
      if ((print_it) || (!(save_it)))
	{
	  mus_print(str);
	}
      else
	{
	  len = strlen(str);
	  for (i = 0; i < len; i++, save_it_loc++)
	    {
	      check_save_it(save_it_loc);
	      save_it[save_it_loc] = str[i];
	    }
	  check_save_it(save_it_loc);
	  save_it[save_it_loc] = 0;
	}
    }
}

char *mus_audio_report(void)
{
  mus_audio_initialize();
  if (!(save_it)) 
    {
      save_it_len = 1024;
      save_it = (char *)CALLOC(save_it_len, sizeof(char));
    }
  save_it_loc = 0;
  print_it = 0;
  if (!audio_strbuf) audio_strbuf = (char *)CALLOC(PRINT_BUFFER_SIZE, sizeof(char));
  describe_audio_state_1();
  return(save_it);
}

void mus_audio_describe(void)
{
  mus_audio_initialize();
  print_it = 1;
  if (!audio_strbuf) audio_strbuf = (char *)CALLOC(PRINT_BUFFER_SIZE, sizeof(char));
  describe_audio_state_1();
}

/* for CLM */
void mus_reset_audio_c (void)
{
  audio_initialized = FALSE;
  save_it = NULL;
  version_name = NULL;
#ifdef SUN
  sun_vol_name = NULL;
#endif
  save_it_len = 0;
  audio_strbuf = NULL;
#ifdef MCL_PPC
  reset_db();
#endif
}


int mus_audio_compatible_format(int dev) 
{
#if HAVE_ALSA
  int err, i;
  float val[32];
  int ival[32];
  err = mus_audio_mixer_read(dev, MUS_AUDIO_FORMAT, 32, val);
  if (err != MUS_ERROR)
    {
      for (i = 0; i <= (int)(val[0]); i++) ival[i] = (int)(val[i]);
      /*               ^ this cast is vital!  Memory clobbered otherwise in LinuxPPC */
      for (i = 1; i <= ival[0]; i++)
	if (ival[i] == MUS_COMPATIBLE_FORMAT) 
	  return(MUS_COMPATIBLE_FORMAT);
      for (i = 1; i <= ival[0]; i++) 
	if ((ival[i] == MUS_BINT) || (ival[i] == MUS_LINT) ||
	    (ival[i] == MUS_BFLOAT) || (ival[i] == MUS_LFLOAT) ||
	    (ival[i] == MUS_BSHORT) || (ival[i] == MUS_LSHORT))
	  return(ival[i]);
      for (i = 1; i <= ival[0]; i++) 
	if ((ival[i] == MUS_MULAW) || (ival[i] == MUS_ALAW) ||
	    (ival[i] == MUS_UBYTE) || (ival[i] == MUS_BYTE))
	  return(ival[i]);
      return(ival[1]);
    }
#endif
  return(MUS_COMPATIBLE_FORMAT);
}


/* next two added 17-Dec-02 for non-interleaved audio IO */
static char *output_buffer = NULL;
static int output_buffer_size = 0;

int mus_audio_write_buffers(int port, int frames, int chans, mus_sample_t **bufs, int output_format, int clipped)
{
  int bytes;
  bytes = chans * frames * mus_bytes_per_sample(output_format);
  if (output_buffer_size < bytes)
    {
      if (output_buffer) free(output_buffer);
      output_buffer = (char *)malloc(bytes);
      output_buffer_size = bytes;
    }
  mus_file_write_buffer(output_format, 0, frames - 1, chans, bufs, output_buffer, clipped);
  return(mus_audio_write(port, output_buffer, bytes));
}

static char *input_buffer = NULL;
static int input_buffer_size = 0;

int mus_audio_read_buffers(int port, int frames, int chans, mus_sample_t **bufs, int input_format)
{
  int bytes;
  bytes = chans * frames * mus_bytes_per_sample(input_format);
  if (input_buffer_size < bytes)
    {
      if (input_buffer) free(input_buffer);
      input_buffer = (char *)malloc(bytes);
      input_buffer_size = bytes;
    }
  mus_audio_read(port, input_buffer, bytes);
  return(mus_file_read_buffer(input_format, 0, chans, frames, bufs, input_buffer));
}
