/* Audio hardware handlers (SGI, OSS, ALSA, Sun, NeXT, Mac, Windows, Be, HPUX, AIX) */
/*
 * layout of this file:
 *    error handlers
 *    SGI new and old audio library (find "- SG")
 *    OSS ("- O") (with Sam 9407 support)
 *    ALSA ("- A")
 *    NeXT ("- NE")
 *    Sun ("- SU") (has switches for OPENBSD, but they're untested)
 *    Mac (Mac OS 8.1) ("- M") (apparently there are important differences in subsequent versions)
 *    Be (release 5 underway) ("- B")
 *    HPUX ("- H")
 *    W95/98 ("- WI")
 *    AIX, NEC EWS, SONY NEWS, OS2, AF, NetBSD etc -- untested and incomplete
 *    ARDOUR audio (find "- Ardour") (i/o routed via the ardour API)
 *    audio describers
 */

/* TODO: w95 input, read/write state
 *       sgi/w95/mac? also may have multiple systems/cards
 *       check for Mac OS changes
 */
/* when reading device_field, put default input device first, or mark somehow */

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
 *
 * int mus_audio_error(void) returns the error code indicated by the immediately preceding audio call
 * int mus_audio_initialize(void) does whatever is needed to get set up
 * char *mus_audio_error_name(int err) gives string decription of error code
 *
 * int mus_audio_systems(void) returns number of separate complete audio systems (soundcards essentially)
 * AUDIO_SYSTEM(n) selects the nth card (counting from 0), AUDIO_SYSTEM(0) is always the default
 * char *mus_audio_system_name(int system) returns some user-recognizable (?) name for the given card (don't free)
 * char *mus_audio_moniker(void) returns some brief description of the overall audio setup (don't free return string).
 */

#if defined(HAVE_CONFIG_H)
  #include "config.h"
#endif

#include <math.h>
#include <stdio.h>
#if (!defined(HAVE_CONFIG_H)) || (defined(HAVE_FCNTL_H))
  #include <fcntl.h>
#endif
#include <signal.h>
#if (!defined(HAVE_CONFIG_H)) || (defined(HAVE_LIMITS_H))
  #include <limits.h>
#endif
#include <errno.h>
#include <stdlib.h>

#if (defined(NEXT) || (defined(HAVE_LIBC_H) && (!defined(HAVE_UNISTD_H))))
  #include <libc.h>
#else
  #if (!(defined(_MSC_VER))) && (!(defined(MPW_C)))
    #include <unistd.h>
  #endif
#endif

#define STRBUF_SIZE 4096

#if (defined(HAVE_CONFIG_H)) && (!defined(HAVE_STRERROR))
char *strerror(int errnum)
{
  char *strerrbuf;
  strerrbuf = (char *)CALLOC(16,sizeof(char));
  sprintf(strerrbuf,"io err %d",errnum);
  return(strerrbuf);
}
#else
  #include <string.h>
#endif

#if HAVE_SAM_9407
  #include <sys/sam9407.h>
#endif

#include "sndlib.h"

static char *version_name = NULL;
static int AUDIO_ERROR = MUS_AUDIO_NO_ERROR;
int mus_audio_error(void) {return(AUDIO_ERROR);}
void mus_audio_set_error(int err) {AUDIO_ERROR = err;}
static int audio_initialized = 0;

static char *audio_error_name_1(int err)
{
  switch (err)
    {
    case MUS_AUDIO_NO_ERROR:                    return("");                            break;
    case MUS_AUDIO_CHANNELS_NOT_AVAILABLE:      return("channel(s) not available");    break;
    case MUS_AUDIO_SRATE_NOT_AVAILABLE:         return("srate not available");         break;
    case MUS_AUDIO_FORMAT_NOT_AVAILABLE:        return("format not available");        break;
    case MUS_AUDIO_NO_INPUT_AVAILABLE:          return("no input available");          break;
    case MUS_AUDIO_NO_OUTPUT_AVAILABLE:         return("no output available");         break;
    case MUS_AUDIO_INPUT_BUSY:                  return("input busy");                  break;
    case MUS_AUDIO_OUTPUT_BUSY:                 return("output busy");                 break;
    case MUS_AUDIO_CONFIGURATION_NOT_AVAILABLE: return("configuration not available"); break;
    case MUS_AUDIO_INPUT_CLOSED:                return("input closed");                break;
    case MUS_AUDIO_OUTPUT_CLOSED:               return("output closed");               break;
    case MUS_AUDIO_IO_INTERRUPTED:              return("io interrupted");              break;
    case MUS_AUDIO_NO_LINES_AVAILABLE:          return("no lines available");          break;
    case MUS_AUDIO_WRITE_ERROR:                 return("write error");                 break;
    case MUS_AUDIO_SIZE_NOT_AVAILABLE:          return("size not available");          break;
    case MUS_AUDIO_DEVICE_NOT_AVAILABLE:        return("device not available");        break;
    case MUS_AUDIO_CANT_CLOSE:                  return("close failed");                break;
    case MUS_AUDIO_CANT_OPEN:                   return("open failed");                 break;
    case MUS_AUDIO_READ_ERROR:                  return("read error");                  break;
    case MUS_AUDIO_AMP_NOT_AVAILABLE:           return("amp control not available");   break;
    case MUS_AUDIO_NO_OP:                       return("unimplemented operation");     break;
    case MUS_AUDIO_CANT_WRITE:                  return("write failed");                break;
    case MUS_AUDIO_CANT_READ:                   return("read failed");                 break;
    case MUS_AUDIO_NO_READ_PERMISSION:          return("need read permission on /dev/dsp"); break;
    default:                                    return("unknown error");               break;
    }
}

#if HAVE_ALSA
/* I was going to use this in other cases too, but got sidetracked */
static char *mus_audio_device_names[] = {
  "MUS_AUDIO_DEFAULT","MUS_AUDIO_DUPLEX_DEFAULT","MUS_AUDIO_ADAT_IN","MUS_AUDIO_AES_IN","MUS_AUDIO_LINE_OUT",
  "MUS_AUDIO_LINE_IN","MUS_AUDIO_MICROPHONE","MUS_AUDIO_SPEAKERS","MUS_AUDIO_DIGITAL_IN","MUS_AUDIO_DIGITAL_OUT",
  "MUS_AUDIO_DAC_OUT","MUS_AUDIO_ADAT_OUT","MUS_AUDIO_AES_OUT","MUS_AUDIO_DAC_FILTER","MUS_AUDIO_MIXER",
  "MUS_AUDIO_LINE1","MUS_AUDIO_LINE2","MUS_AUDIO_LINE3","MUS_AUDIO_AUX_INPUT","MUS_AUDIO_CD",
  "MUS_AUDIO_AUX_OUTPUT","MUS_AUDIO_SPDIF_IN","MUS_AUDIO_SPDIF_OUT","MUS_AUDIO_AMP","MUS_AUDIO_SRATE",
  "MUS_AUDIO_CHANNEL","MUS_AUDIO_FORMAT","MUS_AUDIO_IMIX","MUS_AUDIO_IGAIN","MUS_AUDIO_RECLEV",
  "MUS_AUDIO_PCM","MUS_AUDIO_PCM2","MUS_AUDIO_OGAIN","MUS_AUDIO_LINE","MUS_AUDIO_SYNTH",
  "MUS_AUDIO_BASS","MUS_AUDIO_TREBLE","MUS_AUDIO_PORT","MUS_AUDIO_SAMPLES_PER_CHANNEL",
  "MUS_AUDIO_DIRECTION"
};

static char *mus_audio_format_names[] = {
  "MUS_UNKNOWN","MUS_BSHORT","MUS_MULAW","MUS_BYTE","MUS_BFLOAT","MUS_BINT","MUS_ALAW","MUS_UBYTE","MUS_B24INT",
  "MUS_BDOUBLE","MUS_LSHORT","MUS_LINT","MUS_LFLOAT","MUS_LDOUBLE","MUS_UBSHORT","MUS_ULSHORT","MUS_L24INT",
  "MUS_BINTN","MUS_LINTN","MUS_L12INT"
};
#endif

static char *strbuf = NULL;
static void pprint(char *str);

int device_channels(int dev);
int device_gains(int dev);

int device_channels(int dev)
{
  float val[4];
  mus_audio_mixer_read(dev,MUS_AUDIO_CHANNEL,0,val);
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
      err = mus_audio_mixer_read(ur_dev,MUS_AUDIO_CHANNEL,0,val);
#ifdef HAVE_ALSA
      if (err != MUS_AUDIO_NO_ERROR) return(0);
#endif
      return((int)val[0]);
    }
  err = mus_audio_mixer_read(ur_dev,MUS_AUDIO_AMP,0,val);
  if (err != MUS_AUDIO_NO_ERROR) return(0);
  return(device_channels(ur_dev));
}



/* ------------------------------- SGI ----------------------------------------- */

#ifdef SGI
#define AUDIO_OK

#include <audio.h>

static char *errstr = NULL;
static int al_err = 0;

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

#ifdef AL_RESOURCE

char *mus_audio_error_name(int err)
{
  if (!al_err) return(audio_error_name_1(err));
  sprintf(errstr,"%s: %s",audio_error_name_1(err),alGetErrorString(al_err));
  return(errstr);
}

static int check_queue_size (int size, int chans) {return(size);}

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
  ALgetparams(device,sr,2);
  if ((channels == 4) && (sr[1] != AL_4CHANNEL))
    {
      sr[1] = AL_4CHANNEL;
      ALsetparams(device,sr,2);
    }
  else
    {
      if ((channels != 4) && (sr[1] != AL_STEREO))
        {
          sr[1] = AL_STEREO;
          ALsetparams(device,sr,2);
        }
    }
}

static char *AL_GetErrorString(int err)
{
  switch (err)
    {
    case AL_BAD_NOT_IMPLEMENTED:   return("not implemented yet"); break;
    case AL_BAD_PORT:              return("tried to use an invalid port"); break;
    case AL_BAD_CONFIG:            return("tried to use an invalid configuration"); break;
    case AL_BAD_DEVICE:            return("tried to use an invalid device"); break;
    case AL_BAD_DEVICE_ACCESS:     return("unable to access the device"); break;
    case AL_BAD_DIRECTION:         return("illegal direction given for port"); break;
    case AL_BAD_OUT_OF_MEM:        return("operation has run out of memory"); break;
    case AL_BAD_NO_PORTS:          return("not able to allocate a port"); break;
    case AL_BAD_WIDTH:             return("invalid sample width given"); break;
    case AL_BAD_ILLEGAL_STATE:     return("an illegal state has occurred"); break;
    case AL_BAD_QSIZE:             return("attempt to set an invalid queue size"); break;
    case AL_BAD_FILLPOINT:         return("attempt to set an invalid fillpoint"); break;
    case AL_BAD_BUFFER_NULL:       return("null buffer pointer"); break;
    case AL_BAD_COUNT_NEG:         return("negative count"); break;
    case AL_BAD_PVBUFFER:          return("param/val buffer doesn't make sense"); break;
    case AL_BAD_BUFFERLENGTH_NEG:  return("negative buffer length"); break;
    case AL_BAD_BUFFERLENGTH_ODD:  return("odd length parameter/value buffer"); break;
    case AL_BAD_CHANNELS:          return("invalid channel specifier"); break;
    case AL_BAD_PARAM:             return("invalid parameter"); break;
    case AL_BAD_SAMPFMT:           return("attempt to set invalid sample format"); break;
    case AL_BAD_RATE:              return("invalid sample rate token"); break;
    case AL_BAD_TRANSFER_SIZE:     return("invalid size for sample read/write"); break;
    case AL_BAD_FLOATMAX:          return("invalid size for floatmax"); break;
    case AL_BAD_PORTSTYLE:         return("invalid port style"); break;
    default:                       return("");
    }
}

char *mus_audio_error_name(int err)
{
  if (!al_err) return(audio_error_name_1(err));
  sprintf(errstr,"%s: %s",audio_error_name_1(err),AL_GetErrorString(al_err));
  return(errstr);
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
      audio_initialized = 1;
      AUDIO_ERROR = MUS_AUDIO_NO_ERROR; 
      al_err = 0;
      errstr = (char *)CALLOC(256,sizeof(char));
      config = (ALconfig *)CALLOC(IO_LINES,sizeof(ALconfig));
      port = (ALport *)CALLOC(IO_LINES,sizeof(ALport));
      line_in_use = (int *)CALLOC(IO_LINES,sizeof(int));
      channels = (int *)CALLOC(IO_LINES,sizeof(int));
      device = (long *)CALLOC(IO_LINES,sizeof(long));
      datum_size = (int *)CALLOC(IO_LINES,sizeof(int));
      line_out = (int *)CALLOC(IO_LINES,sizeof(int));
    }
  return(0);
}

static int error_exit(int error, int line)
{
  AUDIO_ERROR = error; 
#ifdef AL_RESOURCE
  if (line != -1) alFreeConfig(config[line]); 
#else
  if (line != -1) ALfreeconfig(config[line]); 
#endif
  return(-1);
}

#ifdef AL_RESOURCE
static int to_al_interface_or_device(int dev,int which)
{
  switch (dev)
    {
    case MUS_AUDIO_DEFAULT:
    case MUS_AUDIO_DUPLEX_DEFAULT: return(AL_DEFAULT_OUTPUT); break;
    case MUS_AUDIO_DAC_OUT:
    case MUS_AUDIO_SPEAKERS: return(alGetResourceByName(AL_SYSTEM,"Analog Out",which)); break;
    case MUS_AUDIO_MICROPHONE: return(alGetResourceByName(AL_SYSTEM,"Microphone",which)); break;
    case MUS_AUDIO_ADAT_IN: return(alGetResourceByName(AL_SYSTEM,"ADAT In",which)); break;
    case MUS_AUDIO_AES_IN: return(alGetResourceByName(AL_SYSTEM,"AES In",which)); break;
    case MUS_AUDIO_ADAT_OUT: return(alGetResourceByName(AL_SYSTEM,"ADAT Out",which)); break;
    case MUS_AUDIO_DIGITAL_OUT:
    case MUS_AUDIO_AES_OUT: return(alGetResourceByName(AL_SYSTEM,"AES Out",which)); break;
    case MUS_AUDIO_LINE_IN: return(alGetResourceByName(AL_SYSTEM,"Line In",which)); break;
    case MUS_AUDIO_LINE_OUT: return(alGetResourceByName(AL_SYSTEM,"Line Out2",which)); break; /* ?? */
      /* case MUS_AUDIO_DIGITAL_IN: return(alGetResourceByName(AL_SYSTEM,"DAC2 In",which)); break; */ /* this is analog in ?? */
    }
  return(0);
}

static int to_al_device(int dev) {return(to_al_interface_or_device(dev,AL_DEVICE_TYPE));}
static int to_al_interface(int dev) {return(to_al_interface_or_device(dev,AL_INTERFACE_TYPE));}
#endif

#include <stdio.h>

/* just a placeholder for now */
int find_audio_output(int chans)
{
#ifdef AL_RESOURCE
  ALvalue x[32];
  ALpv y;
  int n,i;
  y.param = AL_INTERFACE;
  y.value.i = AL_DIGITAL_IF_TYPE;
  n = alQueryValues(AL_SYSTEM,AL_DEFAULT_OUTPUT,x,32,&y,1);
  for (i=0;i<n;i++) 
    {
      y.param = AL_CHANNELS;
      alGetParams(x[i].i,&y,1);
      if (chans <= y.value.i) return(x[i].i);
    }
#endif
  return(-1);
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
  return(-1);
}

int mus_audio_open_output(int ur_dev, int srate, int chans, int format, int requested_size)
{
#ifdef AL_RESOURCE
  ALpv z[2];
#endif
  long sr[2];
  int i,line,size,width,sgi_format,dev;
  AUDIO_ERROR = MUS_AUDIO_NO_ERROR; al_err = 0;
  dev = MUS_AUDIO_DEVICE(ur_dev);
  line = -1;
  for (i=0;i<IO_LINES;i++) if (line_in_use[i] == 0) {line=i; break;}
  if (line == -1) {AUDIO_ERROR = MUS_AUDIO_NO_LINES_AVAILABLE; return(-1);}
  channels[line] = chans;
  line_out[line] = 1;
  datum_size[line] = mus_data_format_to_bytes_per_sample(format);
  if (requested_size == 0) 
    size = 4096;
  else size = check_queue_size(requested_size,chans);
  if (datum_size[line] == 3) 
    width = AL_SAMPLE_24;
  else 
    {
      if (datum_size[line] == 1) 
        width = AL_SAMPLE_8;
      else width = AL_SAMPLE_16;
    }
  sgi_format = to_sgi_format(format);
#ifdef AL_RESOURCE
  if (dev == MUS_AUDIO_DEFAULT)
    device[line] = AL_DEFAULT_OUTPUT;
  else device[line] = to_al_device(dev);                if (!(device[line])) return(error_exit(MUS_AUDIO_DEVICE_NOT_AVAILABLE,-1));
  if (chans>2) size = 65536;                            /* for temp adat code */
#if 0
  if (device_channels(dev) < chans)                     /* look for some device that can play this file */
    device[line] = find_audio_output(chans);            if (device[line] == -1) return(error_exit(MUS_AUDIO_CHANNELS_NOT_AVAILABLE,-1));
#endif
  if ((chans == 4) && (dev == MUS_AUDIO_DAC_OUT))
    {
      /* kludge around a bug in the new audio library */
      sr[0] = AL_CHANNEL_MODE;
      sr[1] = AL_4CHANNEL;
      ALsetparams(AL_DEFAULT_DEVICE,sr,2);
    }
  z[0].param = AL_RATE;
  z[0].value.ll = alDoubleToFixed((double)srate);
  z[1].param = AL_MASTER_CLOCK;
  /* z[1].value.i = AL_CRYSTAL_MCLK_TYPE; */
  z[1].value.i = AL_MCLK_TYPE; /* was AL_CRYSTAL_MCLK_TYPE -- digital I/O perhaps needs AL_VARIABLE_MCLK_TYPE */
  al_err = alSetParams(device[line],z,2);               if (al_err == -1) return(error_exit(MUS_AUDIO_SRATE_NOT_AVAILABLE,-1));
  config[line] = alNewConfig();                         if (!(config[line])) return(error_exit(MUS_AUDIO_CONFIGURATION_NOT_AVAILABLE,-1));
  if ((sgi_format == -1) || 
      (alSetSampFmt(config[line],sgi_format) == -1) ||
      (alSetWidth(config[line],width) == -1))           /* this is a no-op in the float and double cases */
    return(error_exit(MUS_AUDIO_FORMAT_NOT_AVAILABLE,line));
  al_err = alSetChannels(config[line],chans);           if (al_err == -1) return(error_exit(MUS_AUDIO_CHANNELS_NOT_AVAILABLE,line));
  al_err = alSetQueueSize(config[line],size);           if (al_err == -1) return(error_exit(MUS_AUDIO_SIZE_NOT_AVAILABLE,line));
  al_err = alSetDevice(config[line],device[line]);      if (al_err == -1) return(error_exit(MUS_AUDIO_DEVICE_NOT_AVAILABLE,line));
  port[line] = alOpenPort("dac","w",config[line]);      if (!(port[line])) return(error_exit(MUS_AUDIO_CONFIGURATION_NOT_AVAILABLE,line));
#else
  device[line] = AL_DEFAULT_DEVICE;
  check_quad(device[line],chans);
  sr[0]=AL_OUTPUT_RATE;
  sr[1]=srate;
  al_err = ALsetparams(device[line],sr,2);              if (al_err == -1) return(error_exit(MUS_AUDIO_SRATE_NOT_AVAILABLE,-1));
  config[line] = ALnewconfig();                         if (!(config[line])) return(error_exit(MUS_AUDIO_CONFIGURATION_NOT_AVAILABLE,-1));
  if ((sgi_format == -1) || 
      (ALsetsampfmt(config[line],sgi_format) == -1) ||
      (ALsetwidth(config[line],width) == -1))           /* this is a no-op in the float and double cases */
    return(error_exit(MUS_AUDIO_FORMAT_NOT_AVAILABLE,line));
  al_err = ALsetchannels(config[line],chans);           if (al_err == -1) return(error_exit(MUS_AUDIO_CHANNELS_NOT_AVAILABLE,line));
  al_err = ALsetqueuesize(config[line],size);           if (al_err == -1) return(error_exit(MUS_AUDIO_SIZE_NOT_AVAILABLE,line));
  port[line] = ALopenport("dac","w",config[line]);      if (!(port[line])) return(error_exit(MUS_AUDIO_CONFIGURATION_NOT_AVAILABLE,line));
#endif
  line_in_use[line] = 1;
  return(line);
}

int mus_audio_write(int line, char *buf, int bytes)
{
  AUDIO_ERROR = MUS_AUDIO_NO_ERROR; al_err = 0;
#ifdef AL_RESOURCE
  al_err = alWriteFrames(port[line],(short *)buf,bytes/(channels[line]*datum_size[line]));
#else
  al_err = ALwritesamps(port[line],(short *)buf,bytes/datum_size[line]);
#endif
  if (al_err) {AUDIO_ERROR = MUS_AUDIO_WRITE_ERROR; return(-1);}
  return(0);
}

int mus_audio_close(int line)
{
  AUDIO_ERROR = MUS_AUDIO_NO_ERROR; al_err = 0;
  if (line_in_use[line])
    {
      if (line_out[line])
        {
#ifdef AL_RESOURCE
          while (alGetFilled(port[line]) > 0) sginap(1);
#else
          while (ALgetfilled(port[line]) > 0) sginap(1);
#endif
        }
#ifdef AL_RESOURCE
      al_err = alClosePort(port[line]);
      if (al_err == 0) al_err = alFreeConfig(config[line]);
#else
      al_err = ALcloseport(port[line]);
      if (al_err == 0) al_err = ALfreeconfig(config[line]);
#endif
      line_in_use[line] = 0;
      if (al_err) {AUDIO_ERROR = MUS_AUDIO_CANT_CLOSE; return(-1);}
    }
  return(0);
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
  int i,line,size;
  AUDIO_ERROR = MUS_AUDIO_NO_ERROR; al_err = 0;
  dev = MUS_AUDIO_DEVICE(ur_dev);
  line = -1;
  for (i=0;i<IO_LINES;i++) if (line_in_use[i] == 0) {line=i; break;}
  if (line == -1) {AUDIO_ERROR = MUS_AUDIO_NO_LINES_AVAILABLE; return(-1);}
  channels[line] = chans;
  line_out[line] = 0;
  datum_size[line] = mus_data_format_to_bytes_per_sample(format);
  if (requested_size == 0) 
    size = 4096;
  else size = check_queue_size(requested_size,chans);
  /* there are lots of ways this may be called in terms of the desired "device" */
  /* in CLM, the caller specifies which device, in Snd we try to open everything available */
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
	  al_err = alSetParams(device[line],&pv,1);    if (al_err == -1) return(error_exit(MUS_AUDIO_CONFIGURATION_NOT_AVAILABLE,-1));
	}
    }
  if (!(device[line])) return(error_exit(MUS_AUDIO_DEVICE_NOT_AVAILABLE,-1));
  x[0].param = AL_RATE;
  x[0].value.ll = alDoubleToFixed((double)srate);
  x[1].param = AL_MASTER_CLOCK;
  x[1].value.i = AL_MCLK_TYPE; /* AL_CRYSTAL_MCLK_TYPE; */
  al_err = alSetParams(device[line],x,2);               if (al_err == -1) return(error_exit(MUS_AUDIO_SRATE_NOT_AVAILABLE,-1));
  config[line] = alNewConfig();                         if (!config[line]) return(error_exit(MUS_AUDIO_CONFIGURATION_NOT_AVAILABLE,-1));
  if ((to_sgi_format(format) == -1) || 
      (alSetSampFmt(config[line],to_sgi_format(format)) == -1) ||
      (alSetWidth(config[line],(datum_size[line] == 2) ? AL_SAMPLE_16 : AL_SAMPLE_8) == -1))
    return(error_exit(MUS_AUDIO_FORMAT_NOT_AVAILABLE,line));
  al_err = alSetChannels(config[line],chans);           if (al_err == -1) return(error_exit(MUS_AUDIO_CHANNELS_NOT_AVAILABLE,line));
  al_err = alSetQueueSize(config[line],size);           if (al_err == -1) return(error_exit(MUS_AUDIO_SIZE_NOT_AVAILABLE,line));
  al_err = alSetDevice(config[line],device[line]);      if (al_err == -1) return(error_exit(MUS_AUDIO_DEVICE_NOT_AVAILABLE,line));
  port[line] = alOpenPort("adc","r",config[line]);      if (!(port[line])) return(error_exit(MUS_AUDIO_CONFIGURATION_NOT_AVAILABLE,line));
#else
  switch (dev)
    {
    case MUS_AUDIO_DEFAULT: 
    case MUS_AUDIO_DUPLEX_DEFAULT: 
    case MUS_AUDIO_MICROPHONE: resind = AL_INPUT_MIC; break;
    case MUS_AUDIO_LINE_IN: resind = AL_INPUT_LINE; break;
    case MUS_AUDIO_DIGITAL_IN: resind = AL_INPUT_DIGITAL; break;
    default: return(error_exit(MUS_AUDIO_CONFIGURATION_NOT_AVAILABLE,-1)); break;
    }
  device[line] = AL_DEFAULT_DEVICE;
  sr[0] = AL_INPUT_SOURCE;
  sr[1] = resind;
  al_err = ALsetparams(device[line],sr,2);              if (al_err == -1) return(error_exit(MUS_AUDIO_CONFIGURATION_NOT_AVAILABLE,-1));
  check_quad(device[line],chans);
  sr[0] = AL_INPUT_RATE;
  sr[1] = srate;
  al_err = ALsetparams(device[line],sr,2);              if (al_err == -1) return(error_exit(MUS_AUDIO_SRATE_NOT_AVAILABLE,-1));
  config[line] = ALnewconfig();                         if (!config[line]) return(error_exit(MUS_AUDIO_CONFIGURATION_NOT_AVAILABLE,-1));
  if ((to_sgi_format(format) == -1) || 
      (ALsetsampfmt(config[line],to_sgi_format(format)) == -1) ||
      (ALsetwidth(config[line],(datum_size[line] == 2) ? AL_SAMPLE_16 : AL_SAMPLE_8) == -1))
    return(error_exit(MUS_AUDIO_FORMAT_NOT_AVAILABLE,line));
  al_err = ALsetchannels(config[line],chans);           if (al_err == -1) return(error_exit(MUS_AUDIO_CHANNELS_NOT_AVAILABLE,line));
  al_err = ALsetqueuesize(config[line],size);           if (al_err == -1) return(error_exit(MUS_AUDIO_SIZE_NOT_AVAILABLE,line));
  port[line] = ALopenport("adc","r",config[line]);      if (!(port[line])) return(error_exit(MUS_AUDIO_CONFIGURATION_NOT_AVAILABLE,line));
#endif
  line_in_use[line] = 1;
  return(line);
}

int mus_audio_read(int line, char *buf, int bytes)
{
  AUDIO_ERROR = MUS_AUDIO_NO_ERROR; al_err = 0;
#ifdef AL_RESOURCE
  al_err = alReadFrames(port[line],(short *)buf,bytes/(channels[line]*datum_size[line]));
#else
  al_err = ALreadsamps(port[line],(short *)buf,bytes/datum_size[line]);
#endif
  if (al_err) {AUDIO_ERROR = MUS_AUDIO_WRITE_ERROR; return(-1);}
  return(0);
}



#ifdef AL_RESOURCE
/* borrowed from /usr/share/src/dmedia/audio/printdevs.c with modifications */

#define MAX_CHANNELS 8

static float dB_to_linear(float val)
{
  if (val == 0.0) return(1.0);
  return(pow(10.0,val/20.0));
}

static float linear_to_dB(float val)
{
  return(20.0 * log10(val));
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
  return(lo_dB + (hi_dB - lo_dB)*val_norm);
}

int mus_audio_mixer_read(int ur_dev, int field, int chan, float *val)
{
  ALpv x[4];
  ALparamInfo pinf;
  ALfixed g[MAX_CHANNELS];
  int rv,i,dev;
  AUDIO_ERROR = MUS_AUDIO_NO_ERROR; al_err = 0;
  dev = MUS_AUDIO_DEVICE(ur_dev);
  if (field != MUS_AUDIO_PORT)
    {
      rv = to_al_device(dev);                          if (!rv) return(error_exit(MUS_AUDIO_DEVICE_NOT_AVAILABLE,-1)); 
    }
  switch (field)
    {
    case MUS_AUDIO_PORT:
      /* in this case, chan == length of incoming val array.  Number of devices is returned as val[0],
       * and the rest of the available area (if needed) is filled with the device ids.
       */

      /* TODO: get current AL_INPUT_SOURCE and output device and put these at the head of the list */

      i = 0;
      if (alGetResourceByName(AL_SYSTEM,"Microphone",AL_DEVICE_TYPE) != 0) {if ((i+1)<chan) val[i+1] = MUS_AUDIO_MICROPHONE; i++;}
      if (alGetResourceByName(AL_SYSTEM,"Analog Out",AL_DEVICE_TYPE) != 0) {if ((i+1)<chan) val[i+1] = MUS_AUDIO_DAC_OUT; i++;}
      if (alGetResourceByName(AL_SYSTEM,"ADAT In",AL_DEVICE_TYPE) != 0) {if ((i+1)<chan) val[i+1] = MUS_AUDIO_ADAT_IN; i++;}
      if (alGetResourceByName(AL_SYSTEM,"AES In",AL_DEVICE_TYPE) != 0) {if ((i+1)<chan) val[i+1] = MUS_AUDIO_AES_IN; i++;}
      if (alGetResourceByName(AL_SYSTEM,"ADAT Out",AL_DEVICE_TYPE) != 0) {if ((i+1)<chan) val[i+1] = MUS_AUDIO_ADAT_OUT; i++;}
      if (alGetResourceByName(AL_SYSTEM,"AES Out",AL_DEVICE_TYPE) != 0) {if ((i+1)<chan) val[i+1] = MUS_AUDIO_AES_OUT; i++;}
      if (alGetResourceByName(AL_SYSTEM,"Line In",AL_DEVICE_TYPE) != 0) {if ((i+1)<chan) val[i+1] = MUS_AUDIO_LINE_IN; i++;}
      /* if (alGetResourceByName(AL_SYSTEM,"DAC2 In",AL_DEVICE_TYPE) != 0) {if ((i+1)<chan) val[i+1] = MUS_AUDIO_DIGITAL_IN; i++;} */
      val[0] = i;
      break;
    case MUS_AUDIO_FORMAT:  val[0] = 1; if (chan>1) val[1] = MUS_BSHORT; break;
    case MUS_AUDIO_CHANNEL:
      x[0].param = AL_CHANNELS;
      al_err = alGetParams(rv,x,1);                if (al_err == -1) return(error_exit(MUS_AUDIO_READ_ERROR,-1));
      val[0] = (float)(x[0].value.i);
      break;
    case MUS_AUDIO_SRATE:
      x[0].param = AL_RATE;
      al_err = alGetParams(rv,x,1);                if (al_err == -1) return(error_exit(MUS_AUDIO_READ_ERROR,-1)); 
      val[0] = (float)(x[0].value.i);
      break;
    case MUS_AUDIO_AMP:
      al_err = alGetParamInfo(rv,AL_GAIN,&pinf);   if (al_err == -1) return(error_exit(MUS_AUDIO_READ_ERROR,-1)); 
      if (pinf.min.ll == pinf.max.ll) return(error_exit(MUS_AUDIO_AMP_NOT_AVAILABLE,-1));
      /* this ridiculous thing is in dB with completely arbitrary min and max values */
      x[0].param = AL_GAIN;
      x[0].value.ptr = g;
      x[0].sizeIn = MAX_CHANNELS;
      al_err = alGetParams(rv,x,1);                if (x[0].sizeOut <= chan) return(error_exit(MUS_AUDIO_CHANNELS_NOT_AVAILABLE,-1));
      val[0] = dB_to_normalized(alFixedToDouble(g[chan]),alFixedToDouble(pinf.min.ll),alFixedToDouble(pinf.max.ll));
      break;
    default: AUDIO_ERROR = MUS_AUDIO_CANT_READ; break;
    }
  if (AUDIO_ERROR != MUS_AUDIO_NO_ERROR) return(-1);
  return(0);
}

int mus_audio_mixer_write(int ur_dev, int field, int chan, float *val)
{
  /* each field coming in assumes 0.0 to 1.0 as the range */
  ALpv x[4];
  ALparamInfo pinf;
  ALfixed g[MAX_CHANNELS];
  int rv,dev;
  AUDIO_ERROR = MUS_AUDIO_NO_ERROR; al_err = 0;
  dev = MUS_AUDIO_DEVICE(ur_dev);
  rv = to_al_device(dev);                          if (!rv) return(error_exit(MUS_AUDIO_DEVICE_NOT_AVAILABLE,-1)); 
  switch (field)
    {
    case MUS_AUDIO_SRATE:
      x[0].param = AL_RATE;
      x[0].value.i = (int)val[0];
      x[1].param = AL_MASTER_CLOCK;
      x[1].value.i = AL_CRYSTAL_MCLK_TYPE;
      alSetParams(rv,x,2); 
      break;
    case MUS_AUDIO_AMP:
      /* need to change normalized linear value into dB between (dB) lo and hi */
      al_err = alGetParamInfo(rv,AL_GAIN,&pinf);  if (al_err == -1) return(error_exit(MUS_AUDIO_READ_ERROR,-1)); 
      /* I think we need to read all channels here, change the one we care about, then write all channels */
      x[0].param = AL_GAIN;
      x[0].value.ptr = g;
      x[0].sizeIn = MAX_CHANNELS;
      al_err = alGetParams(rv,x,1);               if (x[0].sizeOut <= chan) return(error_exit(MUS_AUDIO_CHANNELS_NOT_AVAILABLE,-1));
      g[chan] = alDoubleToFixed(normalized_to_dB(val[0],alFixedToDouble(pinf.min.ll),alFixedToDouble(pinf.max.ll)));
      al_err = alSetParams(rv,x,1);               if (al_err == -1) return(error_exit(MUS_AUDIO_WRITE_ERROR,-1));
      break;
    case MUS_AUDIO_PORT:
      /* TODO: */
      /* how to set digital in??? */
      AUDIO_ERROR = MUS_AUDIO_CANT_WRITE;
      break;

    default: AUDIO_ERROR = MUS_AUDIO_CANT_WRITE; break;
    }
  if (AUDIO_ERROR != MUS_AUDIO_NO_ERROR) return(-1);
  return(0);
}

#define STRING_SIZE 32
static void dump_resources(ALvalue *x,int rv)
{
  ALpv y[4];
  ALpv yy;
  ALparamInfo pinf;
  ALfixed g[MAX_CHANNELS];
  char dn[STRING_SIZE];
  char dl[STRING_SIZE];
  int i,k;
  ALvalue z[16];
  int nres;
  for (i=0;i<rv;i++)
    {
      y[0].param = AL_LABEL;
      y[0].value.ptr = dl;
      y[0].sizeIn = STRING_SIZE;
      y[1].param = AL_NAME;
      y[1].value.ptr = dn;
      y[1].sizeIn = STRING_SIZE;
      y[2].param = AL_CHANNELS;
      y[3].param = AL_RATE;
      alGetParams(x[i].i,y,5);
      if (alIsSubtype(AL_DEVICE_TYPE, x[i].i)) 
        {
          al_err = alGetParamInfo(x[i].i,AL_GAIN,&pinf);
          sprintf(strbuf,"\nDevice: %s (%s), srate: %d, chans: %d",
                  dn,dl,
                  y[3].value.i,y[2].value.i); 
          pprint(strbuf);
          if (pinf.min.ll != pinf.max.ll)
            {
              yy.param = AL_GAIN;
              yy.value.ptr = g;
              yy.sizeIn = MAX_CHANNELS;
              alGetParams(x[i].i,&yy,1);
              pprint(" amps:[");
              for (k=0;k<yy.sizeOut;k++)
                {
                  sprintf(strbuf,"%.2f",dB_to_normalized(alFixedToDouble(g[k]),alFixedToDouble(pinf.min.ll),alFixedToDouble(pinf.max.ll)));
                  pprint(strbuf);
                  if (k<(yy.sizeOut-1)) pprint(" ");
                }
              pprint("]");
            }
          pprint("\n");
          if ((nres= alQueryValues(x[i].i,AL_INTERFACE,z,16,0,0))>=0) 
            dump_resources(z,nres);
          else sprintf(strbuf,"query failed: %s\n",alGetErrorString(oserror())); 
          pprint(strbuf);
        }
      else 
        {
          sprintf(strbuf,"      %s (%s), chans: %d\n",dn,dl,y[2].value.i); 
          pprint(strbuf);
        }
    }
}

static void describe_audio_state_1(void)
{
  int rv;
  ALvalue x[16];
  if (!strbuf) strbuf = (char *)CALLOC(STRBUF_SIZE,sizeof(char));
  pprint("Devices and Interfaces on this system:\n"); 
  rv= alQueryValues(AL_SYSTEM,AL_DEVICES,x,16,0,0);
  if (rv>0) dump_resources(x,rv);
}


typedef struct {
  int dev,has_gains;
  ALfixed gains[MAX_CHANNELS];
  int srate;
} saved_info;

static saved_info **si = NULL;
static int saved_devices = 0;
static int saved_devices_size = 0;

static void save_devices(ALvalue *x,int rv)
{
  ALpv y;
  saved_info *sv;
  ALparamInfo pinf;
  int i;
  ALvalue z[16];
  int nres;
  for (i=0;i<rv;i++)
    {
      if (saved_devices >= saved_devices_size)
	{
	  if (saved_devices_size == 0)
	    {
	      if (saved_devices >= 16) saved_devices_size = 2*saved_devices; else saved_devices_size = 16;
	      si = (saved_info **)CALLOC(saved_devices_size,sizeof(saved_info *));
	    }
	  else
	    {
	      si = (saved_info **)REALLOC(si,2*saved_devices*sizeof(saved_info *));
	      for (i=saved_devices_size;i<2*saved_devices;i++) si[i]=NULL;
	      saved_devices_size = 2*saved_devices;
	    }
	}
      si[saved_devices] = (saved_info *)CALLOC(1,sizeof(saved_info));
      sv = si[saved_devices];
      sv->dev = x[i].i;
      y.param = AL_RATE;
      alGetParams(x[i].i,&y,1);
      sv->srate = y.value.i;
      saved_devices++;
      if (alIsSubtype(AL_DEVICE_TYPE, x[i].i)) 
        {
          alGetParamInfo(x[i].i,AL_GAIN,&pinf);
          if (pinf.min.ll != pinf.max.ll)
            {
              y.param = AL_GAIN;
              y.value.ptr = sv->gains;
              y.sizeIn = MAX_CHANNELS;
              alGetParams(x[i].i,&y,1);
              sv->has_gains = 1;
            }
          else sv->has_gains = 0;
          if ((nres = alQueryValues(x[i].i,AL_INTERFACE,z,16,0,0))>=0) save_devices(z,nres);
        }
    }
}

void mus_audio_save (void) 
{
  int rv,i;
  ALvalue x[16];
  if (saved_devices>0)
    {
      for (i=0;i<saved_devices;i++)
        {
          if (si[i])
            {
              FREE(si[i]);
              si[i] = NULL;
            }
        }
    }
  saved_devices = 0;
  rv= alQueryValues(AL_SYSTEM,AL_DEVICES,x,16,0,0);
  if (rv>0) save_devices(x,rv);
}

void mus_audio_restore (void) 
{
  int i;
  ALpv x;
  if (saved_devices > 0)
    {
      for (i=0;i<saved_devices;i++)
        {
          if (si[i])
            {
              if (si[i]->has_gains)
                {
                  x.param = AL_GAIN;
                  x.value.ptr = si[i]->gains;
                  x.sizeIn = MAX_CHANNELS;
                  alSetParams(si[i]->dev,&x,1);
                }
              x.param = AL_RATE;
              x.value.i = si[i]->srate;
              alSetParams(si[i]->dev,&x,1);
            }
        }
    }
}


#else

/* old audio library */

#define MAX_VOLUME 255

static int decode_field(int dev, int field, int chan)
{
  int fld = -1;
  switch (dev)
    {
    case MUS_AUDIO_DEFAULT: 
    case MUS_AUDIO_DAC_OUT:
    case MUS_AUDIO_DUPLEX_DEFAULT:
    case MUS_AUDIO_SPEAKERS:
      switch (field)
        {
        case MUS_AUDIO_AMP:
          fld = ((chan == 0) ? AL_LEFT_SPEAKER_GAIN : AL_RIGHT_SPEAKER_GAIN);
          break;
        case MUS_AUDIO_SRATE:
          fld = AL_OUTPUT_RATE;
          break;
        }
      break;
    case MUS_AUDIO_ADAT_IN: case MUS_AUDIO_ADAT_OUT:
    case MUS_AUDIO_AES_IN: case MUS_AUDIO_AES_OUT:
      AUDIO_ERROR = MUS_AUDIO_CONFIGURATION_NOT_AVAILABLE;
      break;
    case MUS_AUDIO_LINE_OUT:
      switch (field)
        {
        case MUS_AUDIO_SRATE:
          fld = AL_OUTPUT_RATE; /* ? */
          break;
        }
      break;
    case MUS_AUDIO_DIGITAL_OUT:
      if (field == MUS_AUDIO_SRATE)
        fld = AL_OUTPUT_RATE;
      else AUDIO_ERROR = MUS_AUDIO_CONFIGURATION_NOT_AVAILABLE;
      break;
    case MUS_AUDIO_DIGITAL_IN:
      if (field == MUS_AUDIO_SRATE)
        fld = AL_INPUT_RATE;
      else AUDIO_ERROR = MUS_AUDIO_CONFIGURATION_NOT_AVAILABLE;
      break;
    case MUS_AUDIO_LINE_IN:
      if (field == MUS_AUDIO_AMP)
        fld = ((chan == 0) ? AL_LEFT_INPUT_ATTEN : AL_RIGHT_INPUT_ATTEN);
      else
        {
          if (field == MUS_AUDIO_SRATE)
            fld = AL_INPUT_RATE;
          else AUDIO_ERROR = MUS_AUDIO_CONFIGURATION_NOT_AVAILABLE;
        }
      break;
    case MUS_AUDIO_MICROPHONE:
      if (field == MUS_AUDIO_AMP)
        fld = ((chan == 0) ? AL_LEFT2_INPUT_ATTEN : AL_RIGHT2_INPUT_ATTEN);
      else
        {
          if (field == MUS_AUDIO_SRATE)
            fld = AL_INPUT_RATE;
          else AUDIO_ERROR = MUS_AUDIO_CONFIGURATION_NOT_AVAILABLE;
        }
      break;
    }
  return(fld);
}

int mus_audio_mixer_read(int ur_dev, int field, int chan, float *val)
{
  long pb[4];
  long fld;
  int dev;
  AUDIO_ERROR = MUS_AUDIO_NO_ERROR; al_err = 0;
  dev = MUS_AUDIO_DEVICE(ur_dev);
  switch (field)
    {
    case MUS_AUDIO_CHANNEL:
      val[0] = 2;
      break;
    case MUS_AUDIO_FORMAT:  val[0] = 1; if (chan>1) val[1] = MUS_BSHORT; break;
    case MUS_AUDIO_PORT:
      /* how to tell which machine we're on? */
      /* TODO: put default in/out devices first */
      val[0] = 4; 
      if (chan>1) val[1]=MUS_AUDIO_LINE_IN; 
      if (chan>2) val[2]=MUS_AUDIO_MICROPHONE; 
      if (chan>3) val[3]=MUS_AUDIO_DIGITAL_IN; 
      if (chan>4) val[4]=MUS_AUDIO_DAC_OUT;
      /* does this order work for digital input as well? (i.e. does it replace the microphone)? */
      break;
    case MUS_AUDIO_AMP:
      fld = decode_field(dev,field,chan);
      if (fld != -1)
        {
          pb[0] = fld;
          al_err = ALgetparams(AL_DEFAULT_DEVICE,pb,2);
          if ((fld == AL_LEFT_SPEAKER_GAIN) || (fld == AL_RIGHT_SPEAKER_GAIN))
            val[0] = ((float)pb[1])/((float)MAX_VOLUME);
          else val[0] = 1.0 - ((float)pb[1])/((float)MAX_VOLUME);
          if (al_err) AUDIO_ERROR = MUS_AUDIO_READ_ERROR;    
        }
      break;
    case MUS_AUDIO_SRATE:
      fld = decode_field(dev,field,chan);
      if (fld != -1)
        {
          pb[0] = fld;
          al_err = ALgetparams(AL_DEFAULT_DEVICE,pb,2);
          val[0] = pb[1];
          if (al_err) AUDIO_ERROR = MUS_AUDIO_READ_ERROR;    
        }
      break;
    default: AUDIO_ERROR = MUS_AUDIO_CANT_READ; break;
    }
  if (AUDIO_ERROR != MUS_AUDIO_NO_ERROR) return(-1);
  return(0);
}

int mus_audio_mixer_write(int ur_dev, int field, int chan, float *val)
{
  long pb[4];
  long fld;
  int dev;
  AUDIO_ERROR = MUS_AUDIO_NO_ERROR; al_err = 0;
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
          al_err = ALsetparams(AL_DEFAULT_DEVICE,pb,4);
          if (al_err) AUDIO_ERROR = MUS_AUDIO_WRITE_ERROR;    
        }
      else AUDIO_ERROR = MUS_AUDIO_CANT_WRITE;
      break;
    case MUS_AUDIO_CHANNEL:
      if (dev == MUS_AUDIO_MICROPHONE)
        {
          pb[0] =  AL_MIC_MODE;
          pb[1] = ((chan == 2) ? AL_STEREO : AL_MONO);
          al_err = ALsetparams(AL_DEFAULT_DEVICE,pb,2);
          if (al_err) AUDIO_ERROR = MUS_AUDIO_WRITE_ERROR;    
        }
      else
        {
          if (dev == MUS_AUDIO_DEFAULT)
            {
              pb[0] = AL_CHANNEL_MODE;
              pb[1] = ((chan == 4) ? AL_4CHANNEL : AL_STEREO);
              al_err = ALsetparams(AL_DEFAULT_DEVICE,pb,2);
              if (al_err) AUDIO_ERROR = MUS_AUDIO_WRITE_ERROR;    
            }
          else AUDIO_ERROR = MUS_AUDIO_CANT_WRITE;
        }
      break;
    case MUS_AUDIO_AMP:
      fld = decode_field(dev,field,chan);
      if (fld != -1)
        {
          pb[0] = fld;
          if ((fld == AL_LEFT_SPEAKER_GAIN) || (fld == AL_RIGHT_SPEAKER_GAIN))
            pb[1] = val[0] * MAX_VOLUME;
          else pb[1] =  (1.0 - val[0])*MAX_VOLUME;
          al_err = ALsetparams(AL_DEFAULT_DEVICE,pb,2);
          if (al_err) AUDIO_ERROR = MUS_AUDIO_WRITE_ERROR;    
        }
      else AUDIO_ERROR = MUS_AUDIO_CANT_WRITE;
      break;
    case MUS_AUDIO_SRATE:
      fld = decode_field(dev,field,chan);
      if (fld != -1)
        {
          pb[0] = fld;
          pb[1] = val[0];
          al_err = ALsetparams(AL_DEFAULT_DEVICE,pb,2);
          if (al_err) 
            AUDIO_ERROR = MUS_AUDIO_WRITE_ERROR;    
          else
            {
              if (fld == AL_INPUT_RATE)
                {
                  pb[0] = AL_OUTPUT_RATE;
                  pb[1] = val[0];
                  al_err = ALsetparams(AL_DEFAULT_DEVICE,pb,2);
                  if (al_err) AUDIO_ERROR = MUS_AUDIO_WRITE_ERROR;    
                }
            }
        }
      else AUDIO_ERROR = MUS_AUDIO_CANT_WRITE;
      break;
    default: AUDIO_ERROR = MUS_AUDIO_CANT_WRITE; break;
    }
  if (AUDIO_ERROR != MUS_AUDIO_NO_ERROR) return(-1);
  return(0);
}

static void describe_audio_state_1(void) 
{
  float amps[1];
  int err;
  if (!strbuf) strbuf = (char *)CALLOC(STRBUF_SIZE,sizeof(char));
  err = mus_audio_mixer_read(MUS_AUDIO_SPEAKERS,MUS_AUDIO_SRATE,0,amps);
  if (err == MUS_AUDIO_NO_ERROR) {sprintf(strbuf,"srate: %.2f\n",amps[0]); pprint(strbuf);} else {fprintf(stdout,"err: %d!\n",err); fflush(stdout);}
  err = mus_audio_mixer_read(MUS_AUDIO_SPEAKERS,MUS_AUDIO_AMP,0,amps);
  if (err == MUS_AUDIO_NO_ERROR) {sprintf(strbuf,"speakers: %.2f",amps[0]); pprint(strbuf);}
  err = mus_audio_mixer_read(MUS_AUDIO_SPEAKERS,MUS_AUDIO_AMP,1,amps);
  if (err == MUS_AUDIO_NO_ERROR) {sprintf(strbuf," %.2f\n",amps[0]); pprint(strbuf);}
  err = mus_audio_mixer_read(MUS_AUDIO_LINE_IN,MUS_AUDIO_AMP,0,amps);
  if (err == MUS_AUDIO_NO_ERROR) {sprintf(strbuf,"line in: %.2f",amps[0]); pprint(strbuf);}
  err = mus_audio_mixer_read(MUS_AUDIO_LINE_IN,MUS_AUDIO_AMP,1,amps);
  if (err == MUS_AUDIO_NO_ERROR) {sprintf(strbuf," %.2f\n",amps[0]); pprint(strbuf);}
  err = mus_audio_mixer_read(MUS_AUDIO_MICROPHONE,MUS_AUDIO_AMP,0,amps);
  if (err == MUS_AUDIO_NO_ERROR) {sprintf(strbuf,"microphone: %.2f",amps[0]); pprint(strbuf);}
  err = mus_audio_mixer_read(MUS_AUDIO_MICROPHONE,MUS_AUDIO_AMP,1,amps);
  if (err == MUS_AUDIO_NO_ERROR) {sprintf(strbuf," %.2f\n",amps[0]); pprint(strbuf);}
  err = mus_audio_mixer_read(MUS_AUDIO_LINE_OUT,MUS_AUDIO_AMP,0,amps);
  if (err == MUS_AUDIO_NO_ERROR) {sprintf(strbuf,"line out: %.2f",amps[0]); pprint(strbuf);}
  err = mus_audio_mixer_read(MUS_AUDIO_LINE_OUT,MUS_AUDIO_AMP,1,amps);
  if (err == MUS_AUDIO_NO_ERROR) {sprintf(strbuf," %.2f\n",amps[0]); pprint(strbuf);}
  err = mus_audio_mixer_read(MUS_AUDIO_DIGITAL_OUT,MUS_AUDIO_AMP,0,amps);
  if (err == MUS_AUDIO_NO_ERROR) {sprintf(strbuf,"digital out: %.2f",amps[0]); pprint(strbuf);}
  err = mus_audio_mixer_read(MUS_AUDIO_DIGITAL_OUT,MUS_AUDIO_AMP,1,amps);
  if (err == MUS_AUDIO_NO_ERROR) {sprintf(strbuf," %.2f\n",amps[0]); pprint(strbuf);}
}

static long *init_state = NULL;

void mus_audio_save (void)
{
  /* save channel mode, input source, left/right atten (2), speaker gain, input/output rate, mic_mode */
  AUDIO_ERROR = MUS_AUDIO_NO_ERROR; 
  al_err = 0;
  init_state = (long *)CALLOC(22,sizeof(long));
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
  al_err = ALgetparams(AL_DEFAULT_DEVICE,init_state,22);
  if (al_err) AUDIO_ERROR = MUS_AUDIO_READ_ERROR;
}

void mus_audio_restore (void)
{
  if (init_state)
    {
      AUDIO_ERROR = MUS_AUDIO_NO_ERROR; 
      al_err = 0;
      al_err = ALsetparams(AL_DEFAULT_DEVICE,init_state,22);
      if (al_err) AUDIO_ERROR = MUS_AUDIO_WRITE_ERROR;
    }
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

#if ((SOUND_VERSION > 360) && (defined(OSS_SYSINFO)))
  #define NEW_OSS
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

static int FRAGMENTS = 4;
static int FRAGMENT_SIZE = 12;
static int fragments_locked = 0;

/* defaults here are FRAGMENTS 16 and FRAGMENT_SIZE 12; these values however
 * cause about a .5 second delay, which is not acceptable in "real-time" situations.
 */

static void oss_mus_audio_set_oss_buffers(int num,int size) {FRAGMENTS = num; FRAGMENT_SIZE = size; fragments_locked = 1;}

static char *oss_mus_audio_error_name(int err) {return(audio_error_name_1(err));}

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
enum {NORMAL_CARD,SONORUS_STUDIO,RME_HAMMERFALL,SAM9407_DSP};
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
  if(sys<sound_cards && audio_type[sys]==SAM9407_DSP)
    {
      sprintf(dev_name, "/dev/sam%d_mixer", audio_mixer[sys]);
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
	      sprintf(dev_name,"%s%d",MIXER_NAME,audio_mixer[sys]);
	      return(dev_name);
	    }
	}
    }
  return(DAC_NAME);
}

static char *oss_mus_audio_system_name(int system) 
{
#if HAVE_SAM_9407
  if(system<sound_cards && audio_type[system]==SAM9407_DSP)
    {
      int fd;
      fd = open(mixer_name(system),O_RDONLY,0);
      if(fd != -1) 
	{
	  static SamDriverInfo driverInfo;
	  if(ioctl(fd, SAM_IOC_DRIVER_INFO, &driverInfo)>=0) 
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
  int status,ignored,fd;
  fd = open(mixer_name(system),O_RDONLY,0);
  if (fd != -1)
    {
      status = ioctl(fd,OSS_GETVERSION,&ignored);
      if (status == 0)
	{
	  status = ioctl(fd,SOUND_MIXER_INFO,&mixinfo);
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
  char version[8];
  if (version_name == NULL) version_name = (char *)CALLOC(64,sizeof(char));
  if (SOUND_VERSION < 361)
    {
      sprintf(version,"%d",SOUND_VERSION);
      sprintf(version_name,"OSS %c.%c.%c",version[0],version[1],version[2]);
    }
  else
    sprintf(version_name,"OSS %x.%x.%x",(SOUND_VERSION>>16)&0xff,(SOUND_VERSION>>8)&0xff,SOUND_VERSION&0xff);
  return(version_name);
}
#endif

static char *dac_name(int sys, int offset)
{
#if HAVE_SAM_9407
  if (sys<sound_cards && audio_type[sys]==SAM9407_DSP) 
    {
      sprintf(dev_name, "/dev/sam%d_dsp", audio_dsp[sys]);
      return(dev_name);
    }
#endif
  if ((sys < sound_cards) && (audio_mixer[sys] >= -1))
    {
      sprintf(dev_name,"%s%d",DAC_NAME,audio_dsp[sys]+offset);
      return(dev_name);
    }
  return(DAC_NAME);
}

void mus_audio_set_dsp_devices(int cards, int *dsps, int *mixers)
{
  /* see note below -- there are still bugs in OSS multi-cards support, so sometimes the map has to be made by hand */
  int i;
  if (audio_dsp == NULL) mus_audio_initialize();
  if (cards > MAX_SOUNDCARDS) cards = MAX_SOUNDCARDS;
  sound_cards = cards;
  for (i=0;i<cards;i++)
    {
      audio_dsp[i] = dsps[i];
      audio_mixer[i] = mixers[i];
    }
}

void mus_audio_dsp_devices(int cards, int *dsps, int *mixers)
{
  int i;
  if (audio_dsp == NULL) mus_audio_initialize();
  if (cards > MAX_SOUNDCARDS) cards = MAX_SOUNDCARDS;
  sound_cards = cards;
  for (i=0;i<cards;i++)
    {
      dsps[i] = audio_dsp[i];
      mixers[i] = audio_mixer[i];
    }
}

void mus_audio_clear_soundcard_inputs(void)
{
  /* turn off inputs (they create an unbelievable amount of noise) and maximize outputs */
  int fd,amp,devmask;
  fd = open("/dev/dsp",O_WRONLY,0);
  if (fd == -1) return;
  amp = 0;
  ioctl(fd,SOUND_MIXER_READ_DEVMASK,&devmask);
  if (SOUND_MASK_MIC & devmask) ioctl(fd,MIXER_WRITE(SOUND_MIXER_MIC),&amp);
  if (SOUND_MASK_IGAIN & devmask) ioctl(fd,MIXER_WRITE(SOUND_MIXER_IGAIN),&amp);
  if (SOUND_MASK_LINE & devmask) ioctl(fd,MIXER_WRITE(SOUND_MIXER_LINE),&amp);
  amp = (99<<8) + 99;
  if (SOUND_MASK_VOLUME & devmask) ioctl(fd,MIXER_WRITE(SOUND_MIXER_VOLUME),&amp);
  if (SOUND_MASK_OGAIN & devmask) ioctl(fd,MIXER_WRITE(SOUND_MIXER_OGAIN),&amp);
  if (SOUND_MASK_PCM & devmask) ioctl(fd,MIXER_WRITE(SOUND_MIXER_PCM),&amp);
  close(fd);
}


#define MIXER_SIZE SOUND_MIXER_NRDEVICES
static int **mixer_state = NULL;
static int *init_srate = NULL,*init_chans = NULL,*init_format = NULL;

void set_dsp_reset(int val);
static int dsp_reset = 0; /* trying to find out if DSP_RESET is ever needed */
void set_dsp_reset(int val) {dsp_reset = val;}

static int oss_mus_audio_initialize(void) 
{
  /* here we need to set up the map of /dev/dsp and /dev/mixer to a given system */
  /* since this info is not passed to us by OSS, we have to work at it... */
  /* for the time being, I'll ignore auxiliary dsp and mixer ports (each is a special case) */
  int i,fd = -1,md,err = 0;
  char dname[32];
  int amp,old_mixer_amp,old_dsp_amp,new_mixer_amp,responsive_field;
  int devmask;
#ifdef NEW_OSS
  int status,ignored;
  oss_sysinfo sysinfo;
  static mixer_info mixinfo;
  int sysinfo_ok = 0;
#endif
  int num_mixers,num_dsps,nmix,ndsp;
  if (!audio_initialized)
    {
      audio_initialized = 1;
      AUDIO_ERROR = MUS_AUDIO_NO_ERROR; 
      audio_fd = (int *)CALLOC(MAX_SOUNDCARDS,sizeof(int));
      audio_open_ctr = (int *)CALLOC(MAX_SOUNDCARDS,sizeof(int));
      audio_dsp = (int *)CALLOC(MAX_SOUNDCARDS,sizeof(int));
      audio_mixer = (int *)CALLOC(MAX_SOUNDCARDS,sizeof(int));
      audio_type = (int *)CALLOC(MAX_SOUNDCARDS,sizeof(int));
      audio_mode = (int *)CALLOC(MAX_SOUNDCARDS,sizeof(int));
      dev_name = (char *)CALLOC(64,sizeof(char));
      init_srate = (int *)CALLOC(MAX_SOUNDCARDS,sizeof(int));
      init_chans = (int *)CALLOC(MAX_SOUNDCARDS,sizeof(int));
      init_format = (int *)CALLOC(MAX_SOUNDCARDS,sizeof(int));
      mixer_state = (int **)CALLOC(MAX_SOUNDCARDS,sizeof(int *));
      for (i=0;i<MAX_SOUNDCARDS;i++) mixer_state[i] = (int *)CALLOC(MIXER_SIZE,sizeof(int));
      for (i=0;i<MAX_SOUNDCARDS;i++)
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
	for (i=0;i<MAX_SOUNDCARDS;i++) 
	  {
	    sprintf(dname, "/dev/sam%d_mixer", i);
	    fd = open(dname, O_WRONLY);
	    if(fd<0)
	      break;
	    if(ioctl(fd, SAM_IOC_API_INFO, &apiInfo)<0 ||
	       apiInfo.apiClass!=SAM_API_CLASS_VANILLA ||
	       ioctl(fd, SAM_IOC_DRIVER_INFO, &driverInfo)<0 ||
	       !driverInfo.haveAudio) 
	      {
		close(fd);
		continue;
	      }
	    audio_type[sound_cards]=SAM9407_DSP;
	    audio_dsp[sound_cards]=i;
	    audio_mixer[sound_cards]=i;
	    sound_cards++;
	    close(fd);
	  }
	if(sound_cards>0)
	  return(0);
      }
#endif

      num_mixers = MAX_MIXERS;
      num_dsps = MAX_DSPS;
#ifdef NEW_OSS
      fd = open(DAC_NAME,O_WRONLY,0);
      if (fd == -1) fd = open(SYNTH_NAME,O_RDONLY,0);
      if (fd == -1) fd = open(MIXER_NAME,O_RDONLY,0);
      if (fd != -1)
	{
	  status = ioctl(fd,OSS_GETVERSION,&ignored);
	  new_oss_running = (status == 0);
	  if (new_oss_running)
	    {
	      status = ioctl(fd,OSS_SYSINFO,&sysinfo);
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
      while ((nmix < num_mixers) && (ndsp < num_dsps))
	{
	  /* for each mixer, find associated main dsp (assumed to be first in /dev/dsp ordering) */
	  /*   if mixer's dsp overlaps or we run out of dsps first, ignore it (aux mixer) */
	  /* our by-guess-or-by-gosh method here is to try to open the mixer.
	   *   if that fails, quit (if very first, try at least to get the dsp setup)
	   *   find volume field, if none, go on, else read current volume
	   *   open next unchecked dsp, try to set volume, read current, if different we found a match -- set and go on.
	   *     if no change, move to next dsp and try again, if no more dsps, quit (checking for null case as before)
	   */
	  sprintf(dname,"%s%d",MIXER_NAME,nmix);
	  md = open(dname,O_RDWR,0);
	  if (md == -1)
	    {
	      if (errno == EBUSY) 
		{
		  fprintf(stderr,"%s is busy: can't access it",dname); 
		  nmix++;
		  continue;
		}
	      else break;
	    }
	  sprintf(dname,"%s%d",DAC_NAME,ndsp);
	  fd = open(dname,O_RDWR,0);
	  if (fd == -1) fd = open(dname,O_RDONLY,0);
	  if (fd == -1) fd = open(dname,O_WRONLY,0); /* some output devices need this */
	  if (fd == -1)
	    {
	      close(md); 
	      if (errno == EBUSY) /* in linux /usr/include/asm/errno.h */
		{
		  fprintf(stderr,"%s is busy: can't access it\n",dname); 
		  ndsp++;
		  continue;
		}
	      else 
		{
		  if ((errno != ENXIO) && (errno != ENODEV))
		    fprintf(stderr,"%s: %s! ",dname,strerror(errno));
		  break;
		}
	    }
#ifdef NEW_OSS				  
	  /* can't change volume yet of Sonorus, so the method above won't work --
	   * try to catch this case via the mixer's name
	   */
	  status = ioctl(md,SOUND_MIXER_INFO,&mixinfo);
	  if ((status == 0) && (mixinfo.name) && (*(mixinfo.name)) && 
	      (strlen(mixinfo.name) > 6) && (strncmp("STUDI/O",mixinfo.name,7) == 0))
	    {
	      /* a special case in every regard */
	      audio_type[sound_cards] = SONORUS_STUDIO;
	      audio_mixer[sound_cards] = nmix; 
	      nmix++;
	      audio_dsp[sound_cards] = ndsp; 
	      if (num_dsps >= 21)
		{
		  ndsp+=21;
		  audio_mode[sound_cards] = 1;
		}
	      else
		{
		  ndsp+=9;
		  audio_mode[sound_cards] = 0;
		}
	      sound_cards++;
	      close(fd);
	      close(md);
	      continue;
	    }
	  if ((status == 0) && (mixinfo.name) && (*(mixinfo.name)) && 
	      (strlen(mixinfo.name) > 10) && (strncmp("RME Digi96",mixinfo.name,10) == 0))
	    {
	      /* a special case in every regard */
	      audio_type[sound_cards] = RME_HAMMERFALL;
	      audio_mixer[sound_cards] = nmix; 
	      nmix++;
	      audio_dsp[sound_cards] = ndsp; 
	      sound_cards++;
	      close(fd);
	      close(md);
	      continue;
	    }
#endif
	  err = ioctl(md,SOUND_MIXER_READ_DEVMASK,&devmask);
	  responsive_field = SOUND_MIXER_VOLUME;
	  for (i=0;i<SOUND_MIXER_NRDEVICES;i++)
	    if ((1<<i) & devmask)
	      {
		responsive_field = i;
		break;
	      }
	  if (!err)
	    {
	      err = ioctl(md,MIXER_READ(responsive_field),&old_mixer_amp);
	      if (!err)
		{
		  err = ioctl(fd,MIXER_READ(responsive_field),&old_dsp_amp);
		  if ((!err) && (old_dsp_amp == old_mixer_amp))
		    {
		      if (old_mixer_amp == 0) amp = 50; else amp = 0; /* 0..100 */
		      err = ioctl(fd,MIXER_WRITE(responsive_field),&amp);
		      if (!err)
			{
			  err = ioctl(md,MIXER_READ(responsive_field),&new_mixer_amp);
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
			      err = ioctl(fd,MIXER_WRITE(responsive_field),&old_dsp_amp);
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
	  fd = open(DAC_NAME,O_WRONLY,0);
	  if (fd != -1)
	    {
	      sound_cards = 1;
	      audio_dsp[0] = 0;
	      audio_type[0] = NORMAL_CARD;
	      audio_mixer[0] = -2; /* hmmm -- need a way to see /dev/dsp as lonely outpost */
	      close(fd);
	      fd = open(MIXER_NAME,O_RDONLY,0);
	      if (fd == -1)
		audio_mixer[0] = -3;
	      else close(fd);
	    }
	}
    }
  return(0);
}

/* TODO: fix this to handle aux devices correctly */

static int linux_audio_open(const char *pathname, int flags, mode_t mode, int system)
{
  if (audio_fd[system] == -1) 
    {
      audio_fd[system] = open(pathname,flags,mode);
      audio_open_ctr[system] = 0;
    }
  else audio_open_ctr[system]++;
  return(audio_fd[system]);
}

static int find_system(int line)
{
  int i;
  for (i=0;i<sound_cards;i++)
    {
      if (line == audio_fd[i])
	return(i);
    }
  return(-1);
}

static int linux_audio_close(int fd)
{
  int err = 0,sys;
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
    }
  return(err);
}

static int error_exit(int error, int line)
{
  AUDIO_ERROR = error;
  if (line != -1) linux_audio_close(line);
  return(-1);
}

static int to_oss_format(int snd_format)
{
  switch (snd_format)
    {
    case MUS_BYTE: return(AFMT_S8); break;
    case MUS_BSHORT: return(AFMT_S16_BE); break;
    case MUS_UBYTE: return(AFMT_U8); break;
    case MUS_MULAW: return(AFMT_MU_LAW); break;
    case MUS_ALAW: return(AFMT_A_LAW); break;
    case MUS_LSHORT: return(AFMT_S16_LE); break;
    case MUS_UBSHORT: return(AFMT_U16_BE); break;
    case MUS_ULSHORT: return(AFMT_U16_LE); break;
#ifdef NEW_OSS
    case MUS_LINT: return(AFMT_S32_LE); break;
    case MUS_BINT: return(AFMT_S32_BE); break;
#endif
    }
  return(-1);
}

static char sonorus_buf[16];
static char *sonorus_name(int sys, int offset)
{
  sprintf(sonorus_buf,"/dev/dsp%d",offset + audio_dsp[sys]);
  return(sonorus_buf);
}

static int oss_mus_audio_open_output(int ur_dev, int srate, int chans, int format, int size)
{
  /* ur_dev is in general MUS_AUDIO_PACK_SYSTEM(n) | MUS_AUDIO_DEVICE */
  int oss_format,buffer_info,audio_out = -1,sys,dev;
#ifndef NEW_OSS
  int stereo;
#endif
  AUDIO_ERROR = MUS_AUDIO_NO_ERROR;
  sys = MUS_AUDIO_SYSTEM(ur_dev);
  dev = MUS_AUDIO_DEVICE(ur_dev);
  oss_format = to_oss_format(format); 
  if (oss_format == -1) return(error_exit(MUS_AUDIO_FORMAT_NOT_AVAILABLE,-1));

  if (audio_type[sys] == SONORUS_STUDIO)
    {
      /* in this case the output devices are parcelled out to the /dev/dsp locs */
      /* dev/dsp0 is always stereo */
      switch (dev)
	{
	case MUS_AUDIO_DEFAULT: 
	  if (chans > 2) 
	    audio_out = open(sonorus_name(sys,1),O_WRONLY,0);
	  else audio_out = open(sonorus_name(sys,0),O_WRONLY,0);
	  /* probably should write to both outputs */
	  if (audio_out == -1) audio_out = open("/dev/dsp",O_WRONLY,0);
	  break;
	case MUS_AUDIO_SPEAKERS:
	  audio_out = open(sonorus_name(sys,0),O_WRONLY,0);
	  if (audio_out == -1) audio_out = open("/dev/dsp",O_WRONLY,0);
	  break;
	case MUS_AUDIO_ADAT_OUT: case MUS_AUDIO_SPDIF_OUT:
	  audio_out = open(sonorus_name(sys,1),O_WRONLY,0);
	  break;
	case MUS_AUDIO_AES_OUT:
	  audio_out = open(sonorus_name(sys,9),O_WRONLY,0);
	  break;
	default:
	  return(error_exit(MUS_AUDIO_DEVICE_NOT_AVAILABLE,audio_out));
	  break;
	}
      if (audio_out == -1) return(error_exit(MUS_AUDIO_CANT_OPEN,audio_out));
      if (ioctl(audio_out,SNDCTL_DSP_CHANNELS,&chans) == -1) return(error_exit(MUS_AUDIO_CHANNELS_NOT_AVAILABLE,audio_out));
      return(audio_out);
    }

#if HAVE_SAM_9407
  if (audio_type[sys] == SAM9407_DSP)
    {
      char dname[32];
      sprintf(dname, "/dev/sam%d_dsp", audio_dsp[sys]);
      audio_out=open(dname, O_WRONLY);
      if(audio_out == -1)
	return error_exit(MUS_AUDIO_CANT_OPEN, audio_out); 
      if ((ioctl(audio_out,SNDCTL_DSP_SETFMT,&oss_format) == -1) || (oss_format != to_oss_format(format))) 
	return(error_exit(MUS_AUDIO_FORMAT_NOT_AVAILABLE,audio_out));
      if (ioctl(audio_out,SNDCTL_DSP_CHANNELS,&chans) == -1)
	return(error_exit(MUS_AUDIO_CHANNELS_NOT_AVAILABLE,audio_out));
      if (ioctl(audio_out,SNDCTL_DSP_SPEED,&srate) == -1)
	return(error_exit(MUS_AUDIO_SRATE_NOT_AVAILABLE,audio_out));
      FRAGMENT_SIZE=14;
      buffer_info = (FRAGMENTS<<16) | (FRAGMENT_SIZE);
      ioctl(audio_out, SNDCTL_DSP_SETFRAGMENT, &buffer_info);
      return(audio_out);
    }
#endif

  if (dev == MUS_AUDIO_DEFAULT)
    audio_out = linux_audio_open(dac_name(sys,0),O_WRONLY,0,sys);
  else audio_out = linux_audio_open(dac_name(sys,(dev == MUS_AUDIO_AUX_OUTPUT) ? 1 : 0),O_RDWR,0,sys);
  if (audio_out == -1) return(error_exit(MUS_AUDIO_CANT_OPEN,audio_out));
  /* ioctl(audio_out,SNDCTL_DSP_RESET,0); */ /* causes clicks */
  if ((dev == MUS_AUDIO_DUPLEX_DEFAULT) || (size != 0))
    {
      if (!fragments_locked) {if (srate > 30000) FRAGMENTS = 4; else FRAGMENTS = 2;}
      buffer_info = (FRAGMENTS<<16) | (FRAGMENT_SIZE);
      if (ioctl(audio_out,SNDCTL_DSP_SETFRAGMENT,&buffer_info) == -1)
        {
          /* older Linuces (or OSS's?) refuse to handle the fragment reset if O_RDWR used --
           * someone at OSS forgot to update the version number when this was fixed, so
           * I have no way to get around this except to try and retry...
           */
          linux_audio_close(audio_out);
          audio_out = linux_audio_open(dac_name(sys,(dev == MUS_AUDIO_AUX_OUTPUT) ? 1 : 0),O_WRONLY,0,sys);
	  if (audio_out == -1) return(error_exit(MUS_AUDIO_CANT_OPEN,audio_out));
          buffer_info = (FRAGMENTS<<16) | (FRAGMENT_SIZE);
          if (ioctl(audio_out,SNDCTL_DSP_SETFRAGMENT,&buffer_info) == -1) return(error_exit(MUS_AUDIO_CONFIGURATION_NOT_AVAILABLE,audio_out));
        }
    }
  if ((ioctl(audio_out,SNDCTL_DSP_SETFMT,&oss_format) == -1) || (oss_format != to_oss_format(format))) 
    return(error_exit(MUS_AUDIO_FORMAT_NOT_AVAILABLE,audio_out));
#ifdef NEW_OSS
  if (ioctl(audio_out,SNDCTL_DSP_CHANNELS,&chans) == -1) return(error_exit(MUS_AUDIO_CHANNELS_NOT_AVAILABLE,audio_out));
#else
  if (chans == 2) stereo = 1; else stereo = 0;
  if ((ioctl(audio_out,SNDCTL_DSP_STEREO,&stereo) == -1) || ((chans == 2) && (stereo == 0))) return(error_exit(MUS_AUDIO_CHANNELS_NOT_AVAILABLE,audio_out));
#endif
  if (ioctl(audio_out,SNDCTL_DSP_SPEED,&srate) == -1) return(error_exit(MUS_AUDIO_SRATE_NOT_AVAILABLE,audio_out));
  /* http://www.4front-tech.com/pguide/audio.html says this order has to be followed */
  return(audio_out);
}

static int oss_mus_audio_write(int line, char *buf, int bytes)
{
  AUDIO_ERROR = MUS_AUDIO_NO_ERROR;
  write(line,buf,bytes);
  return(0);
}

static int oss_mus_audio_close(int line)
{
  AUDIO_ERROR = MUS_AUDIO_NO_ERROR;
  return(linux_audio_close(line));
}

static int oss_mus_audio_read(int line, char *buf, int bytes)
{
  AUDIO_ERROR = MUS_AUDIO_NO_ERROR;
  read(line,buf,bytes);
  return(0);
}

static int oss_mus_audio_open_input(int ur_dev, int srate, int chans, int format, int requested_size)
{
  /* dev can be MUS_AUDIO_DEFAULT or MUS_AUDIO_DUPLEX_DEFAULT as well as the obvious others */
  int audio_fd = -1,oss_format,buffer_info,sys,dev,srcbit,cursrc,adat_mode = 0,err;
#ifndef NEW_OSS
  int stereo;
#endif
  AUDIO_ERROR = MUS_AUDIO_NO_ERROR;
  sys = MUS_AUDIO_SYSTEM(ur_dev);
  dev = MUS_AUDIO_DEVICE(ur_dev);
  oss_format = to_oss_format(format);
  if (oss_format == -1) return(error_exit(MUS_AUDIO_FORMAT_NOT_AVAILABLE,-1));

  if (audio_type[sys] == SONORUS_STUDIO)
    {
      adat_mode = (audio_mode[sys] == 1);
      switch (dev)
	{
	case MUS_AUDIO_DEFAULT:
	  if (adat_mode)
	    audio_fd = open(sonorus_name(sys,11),O_RDONLY,0);
	  else audio_fd = open(sonorus_name(sys,5),O_RDONLY,0);
	  break;
	case MUS_AUDIO_ADAT_IN:
	  audio_fd = open(sonorus_name(sys,11),O_RDONLY,0);
	  break;
	case MUS_AUDIO_AES_IN:
	  audio_fd = open(sonorus_name(sys,20),O_RDONLY,0);
	  break;
	case MUS_AUDIO_SPDIF_IN:
	  audio_fd = open(sonorus_name(sys,5),O_RDONLY,0);
	  break;
	default:
	  return(error_exit(MUS_AUDIO_DEVICE_NOT_AVAILABLE,audio_fd));
	  break;
	}
      if (audio_fd == -1) return(error_exit(MUS_AUDIO_NO_INPUT_AVAILABLE,-1));
      if (ioctl(audio_fd,SNDCTL_DSP_CHANNELS,&chans) == -1) return(error_exit(MUS_AUDIO_CHANNELS_NOT_AVAILABLE,audio_fd));
      return(audio_fd);
    }

#if HAVE_SAM_9407
  if (audio_type[sys] == SAM9407_DSP)
    {
      char dname[32];
      sprintf(dname, "/dev/sam%d_dsp", audio_dsp[sys]);
      audio_fd=open(dname, O_RDONLY);
      if(audio_fd == -1)
	return(error_exit(MUS_AUDIO_CANT_OPEN, audio_fd)); 
      if ((ioctl(audio_fd,SNDCTL_DSP_SETFMT,&oss_format) == -1) || (oss_format != to_oss_format(format)))
	return(error_exit(MUS_AUDIO_FORMAT_NOT_AVAILABLE,audio_fd));
      if (ioctl(audio_fd,SNDCTL_DSP_CHANNELS,&chans) == -1)
	return(error_exit(MUS_AUDIO_CHANNELS_NOT_AVAILABLE,audio_fd));
      if (ioctl(audio_fd,SNDCTL_DSP_SPEED,&srate) == -1)
	return(error_exit(MUS_AUDIO_SRATE_NOT_AVAILABLE,audio_fd));
      FRAGMENT_SIZE=14;
      buffer_info = (FRAGMENTS<<16) | (FRAGMENT_SIZE);
      ioctl(audio_fd,SNDCTL_DSP_SETFRAGMENT,&buffer_info);
      return(audio_fd);
    }
#endif

  if (((dev == MUS_AUDIO_DEFAULT) || (dev == MUS_AUDIO_DUPLEX_DEFAULT)) && (sys == 0))
    audio_fd = linux_audio_open(dac_name(sys,0),O_RDWR,0,sys);
  else audio_fd = linux_audio_open(dac_name(sys,(dev == MUS_AUDIO_AUX_INPUT) ? 1 : 0),O_RDONLY,0,sys);
  if (audio_fd == -1)
    {
      if (dev == MUS_AUDIO_DUPLEX_DEFAULT) return(error_exit(MUS_AUDIO_CONFIGURATION_NOT_AVAILABLE,-1));
      if ((audio_fd = linux_audio_open(dac_name(sys,(dev == MUS_AUDIO_AUX_INPUT) ? 1 : 0),O_RDONLY,0,sys)) == -1)
        {
          if ((errno == EACCES) || (errno == ENOENT))
            {
              fprintf(stdout,"(to get input in Linux, we need read permission on /dev/dsp)");
              fflush(stdout);
	      return(error_exit(MUS_AUDIO_NO_READ_PERMISSION,-1));
            }
          return(error_exit(MUS_AUDIO_NO_INPUT_AVAILABLE,-1));
        }
    }
  else 
    {
      err = 0;
      err = ioctl(audio_fd,SNDCTL_DSP_SETDUPLEX,&err); /* not always a no-op! */
      /* if (err == -1) AUDIO_ERROR = MUS_AUDIO_NO_INPUT_AVAILABLE; */
      /* this damned thing returns -1 even when full duplex is available */
    }
  if (audio_type[sys] == RME_HAMMERFALL) return(audio_fd);
  /* need to make sure the desired recording source is active -- does this actually have any effect? */
  switch (dev)
    {
    case MUS_AUDIO_MICROPHONE: srcbit = SOUND_MASK_MIC; break;
    case MUS_AUDIO_LINE_IN:    srcbit = SOUND_MASK_LINE; break;
    case MUS_AUDIO_LINE1:      srcbit = SOUND_MASK_LINE1; break;
    case MUS_AUDIO_LINE2:      srcbit = SOUND_MASK_LINE2; break;
    case MUS_AUDIO_LINE3:      srcbit = SOUND_MASK_LINE3; break; /* also digital1..3 */
    case MUS_AUDIO_DUPLEX_DEFAULT: 
    case MUS_AUDIO_DEFAULT:    srcbit = SOUND_MASK_LINE | SOUND_MASK_MIC; break;
    case MUS_AUDIO_CD:      srcbit = SOUND_MASK_CD; break;
    default:                srcbit = 0; break;
      /* other possibilities: synth, radio, phonein but these apparently bypass the mixer (no gains?) */
    }
  ioctl(audio_fd,MIXER_READ(SOUND_MIXER_RECSRC),&cursrc);
  srcbit = (srcbit | cursrc);
  ioctl(audio_fd,MIXER_WRITE(SOUND_MIXER_RECSRC),&srcbit);
  if (dsp_reset) ioctl(audio_fd,SNDCTL_DSP_RESET,0);
  if (requested_size != 0)
    {
      buffer_info = (FRAGMENTS<<16) | (FRAGMENT_SIZE);
      ioctl(audio_fd,SNDCTL_DSP_SETFRAGMENT,&buffer_info);
    }
  if ((ioctl(audio_fd,SNDCTL_DSP_SETFMT,&oss_format) == -1) || (oss_format != to_oss_format(format)))
    return(error_exit(MUS_AUDIO_FORMAT_NOT_AVAILABLE,audio_fd));
#ifdef NEW_OSS
  if (ioctl(audio_fd,SNDCTL_DSP_CHANNELS,&chans) == -1) return(error_exit(MUS_AUDIO_CHANNELS_NOT_AVAILABLE,audio_fd));
#else
  if (chans == 2) stereo = 1; else stereo = 0;
  if ((ioctl(audio_fd,SNDCTL_DSP_STEREO,&stereo) == -1) || ((chans == 2) && (stereo == 0))) return(error_exit(MUS_AUDIO_CHANNELS_NOT_AVAILABLE,audio_fd));
#endif
  if (ioctl(audio_fd,SNDCTL_DSP_SPEED,&srate) == -1) return(error_exit(MUS_AUDIO_SRATE_NOT_AVAILABLE,audio_fd));
  return(audio_fd);
}


static int oss_mus_audio_mixer_read(int ur_dev, int field, int chan, float *val)
{
  int fd,amp,channels,err=0,devmask,stereodevs,ind,formats,sys,dev,srate,adat_mode=0;
  AUDIO_ERROR = MUS_AUDIO_NO_ERROR;
  sys = MUS_AUDIO_SYSTEM(ur_dev);
  dev = MUS_AUDIO_DEVICE(ur_dev);
  
  if (audio_type[sys] == SONORUS_STUDIO)
    {
      adat_mode = (audio_mode[sys] == 1);
      if (dev == MUS_AUDIO_MIXER) val[0]=0; /* no mixer */
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
			case MUS_AUDIO_SPEAKERS: channels = 2; break;
			case MUS_AUDIO_ADAT_IN: case MUS_AUDIO_ADAT_OUT: channels = 8; break;
			case MUS_AUDIO_AES_IN: case MUS_AUDIO_AES_OUT: channels = 2; break;
			case MUS_AUDIO_SPDIF_IN: case MUS_AUDIO_SPDIF_OUT: channels = 4; break;
			case MUS_AUDIO_DEFAULT: if (adat_mode) channels = 8; else channels = 4; break;
			default: channels = 0; break;
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
      return(MUS_AUDIO_NO_ERROR);
    }

#if HAVE_SAM_9407
  if (audio_type[sys] == SAM9407_DSP)
    {
      switch(field) 
	{
	case MUS_AUDIO_PORT:
	  val[0]=2;
	  val[1]=MUS_AUDIO_SPEAKERS;
	  val[2]=MUS_AUDIO_LINE_IN;
	  break;
	case MUS_AUDIO_FORMAT:
	  val[0]=1;
	  val[1]=MUS_LSHORT;
	  break;
	case MUS_AUDIO_CHANNEL:
	  val[0]=2;
	  break;
	case MUS_AUDIO_AMP:
	  return(error_exit(MUS_AUDIO_DEVICE_NOT_AVAILABLE, -1));
	case MUS_AUDIO_SRATE:
	  val[0]=44100;
	  break;
	default:
	  return(error_exit(MUS_AUDIO_CANT_READ, -1));
	}
      return 0;
    }
#endif

  if (audio_type[sys] == RME_HAMMERFALL)
    {
      if (dev == MUS_AUDIO_MIXER) val[0]=0; /* no mixer */
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
			case MUS_AUDIO_SPEAKERS: channels = 2; break;
			case MUS_AUDIO_ADAT_IN: case MUS_AUDIO_ADAT_OUT: channels = 8; break;
			case MUS_AUDIO_AES_IN: case MUS_AUDIO_AES_OUT: channels = 2; break;
			case MUS_AUDIO_SPDIF_IN: case MUS_AUDIO_SPDIF_OUT: channels = 4; break;
			case MUS_AUDIO_DEFAULT: if (adat_mode) channels = 8; else channels = 4; break;
			default: channels = 0; break;
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
      return(MUS_AUDIO_NO_ERROR);
    }

  fd = linux_audio_open(mixer_name(sys),O_RDONLY | O_NONBLOCK,0,sys);
  if (fd == -1) 
    {
      fd = linux_audio_open(DAC_NAME,O_RDONLY,0,sys);
      if (fd == -1)
	{
	  fd = linux_audio_open(DAC_NAME,O_WRONLY,0,sys);
	  if (fd == -1) return(error_exit(MUS_AUDIO_CANT_OPEN,-1));
	}
    }
  err = ioctl(fd,SOUND_MIXER_READ_DEVMASK,&devmask);
  if (err) return(error_exit(MUS_AUDIO_CONFIGURATION_NOT_AVAILABLE,fd));
  if ((dev == MUS_AUDIO_MIXER) || (dev == MUS_AUDIO_DAC_FILTER)) /* these give access to all the on-board analog input gain controls */
    {
      amp = 0;
      err = ioctl(fd,SOUND_MIXER_READ_DEVMASK,&devmask);
      switch (field)
        { 
          /* also DIGITAL1..3 PHONEIN PHONEOUT VIDEO RADIO MONITOR */
	  /* the digital lines should get their own panes in the recorder */
	  /* not clear whether the phone et al lines are routed to the ADC */
	  /* also, I've never seen a card with any of these devices */
        case MUS_AUDIO_IMIX:   if (SOUND_MASK_IMIX & devmask) err = ioctl(fd,MIXER_READ(SOUND_MIXER_IMIX),&amp);     break;
        case MUS_AUDIO_IGAIN:  if (SOUND_MASK_IGAIN & devmask) err = ioctl(fd,MIXER_READ(SOUND_MIXER_IGAIN),&amp);   break;
        case MUS_AUDIO_RECLEV: if (SOUND_MASK_RECLEV & devmask) err = ioctl(fd,MIXER_READ(SOUND_MIXER_RECLEV),&amp); break;
        case MUS_AUDIO_PCM:    if (SOUND_MASK_PCM & devmask) err = ioctl(fd,MIXER_READ(SOUND_MIXER_PCM),&amp);       break;
        case MUS_AUDIO_PCM2:   if (SOUND_MASK_ALTPCM & devmask) err = ioctl(fd,MIXER_READ(SOUND_MIXER_ALTPCM),&amp); break;
        case MUS_AUDIO_OGAIN:  if (SOUND_MASK_OGAIN & devmask) err = ioctl(fd,MIXER_READ(SOUND_MIXER_OGAIN),&amp);   break;
        case MUS_AUDIO_LINE:   if (SOUND_MASK_LINE & devmask) err = ioctl(fd,MIXER_READ(SOUND_MIXER_LINE),&amp);     break;
        case MUS_AUDIO_MICROPHONE:    if (SOUND_MASK_MIC & devmask) err = ioctl(fd,MIXER_READ(SOUND_MIXER_MIC),&amp);       break;
        case MUS_AUDIO_LINE1:  if (SOUND_MASK_LINE1 & devmask) err = ioctl(fd,MIXER_READ(SOUND_MIXER_LINE1),&amp);   break;
        case MUS_AUDIO_LINE2:  if (SOUND_MASK_LINE2 & devmask) err = ioctl(fd,MIXER_READ(SOUND_MIXER_LINE2),&amp);   break;
        case MUS_AUDIO_LINE3:  if (SOUND_MASK_LINE3 & devmask) err = ioctl(fd,MIXER_READ(SOUND_MIXER_LINE3),&amp);   break;
        case MUS_AUDIO_SYNTH:  if (SOUND_MASK_SYNTH & devmask) err = ioctl(fd,MIXER_READ(SOUND_MIXER_SYNTH),&amp);   break;
        case MUS_AUDIO_BASS:   if (SOUND_MASK_BASS & devmask) err = ioctl(fd,MIXER_READ(SOUND_MIXER_BASS),&amp);     break;
        case MUS_AUDIO_TREBLE: if (SOUND_MASK_TREBLE & devmask) err = ioctl(fd,MIXER_READ(SOUND_MIXER_TREBLE),&amp); break;
        case MUS_AUDIO_CD:     if (SOUND_MASK_CD & devmask) err = ioctl(fd,MIXER_READ(SOUND_MIXER_CD),&amp);         break;
        case MUS_AUDIO_CHANNEL:
          if (dev == MUS_AUDIO_MIXER)
            {
              channels = 0;
              err = ioctl(fd,SOUND_MIXER_READ_STEREODEVS,&stereodevs);
              if (SOUND_MASK_IMIX & devmask) {if (SOUND_MASK_IMIX & stereodevs) channels += 2; else channels += 1;}
              if (SOUND_MASK_IGAIN & devmask) {if (SOUND_MASK_IGAIN & stereodevs) channels += 2; else channels += 1;}
              if (SOUND_MASK_RECLEV & devmask) {if (SOUND_MASK_RECLEV & stereodevs) channels += 2; else channels += 1;}
              if (SOUND_MASK_PCM & devmask) {if (SOUND_MASK_PCM & stereodevs) channels += 2; else channels += 1;}
              if (SOUND_MASK_ALTPCM & devmask) {if (SOUND_MASK_ALTPCM & stereodevs) channels += 2; else channels += 1;}
              if (SOUND_MASK_OGAIN & devmask) {if (SOUND_MASK_OGAIN & stereodevs) channels += 2; else channels += 1;}
              if (SOUND_MASK_LINE & devmask) {if (SOUND_MASK_LINE & stereodevs) channels += 2; else channels += 1;}
              if (SOUND_MASK_MIC & devmask) {if (SOUND_MASK_MIC & stereodevs) channels += 2; else channels += 1;}
              if (SOUND_MASK_LINE1 & devmask) {if (SOUND_MASK_LINE1 & stereodevs) channels += 2; else channels += 1;}
              if (SOUND_MASK_LINE2 & devmask) {if (SOUND_MASK_LINE2 & stereodevs) channels += 2; else channels += 1;}
              if (SOUND_MASK_LINE3 & devmask) {if (SOUND_MASK_LINE3 & stereodevs) channels += 2; else channels += 1;}
              if (SOUND_MASK_SYNTH & devmask) {if (SOUND_MASK_SYNTH & stereodevs) channels += 2; else channels += 1;}
              if (SOUND_MASK_CD & devmask) {if (SOUND_MASK_CD & stereodevs) channels += 2; else channels += 1;}
            }
          else 
            if (SOUND_MASK_TREBLE & devmask) channels = 2; else channels = 0;
          val[0] = channels;
          linux_audio_close(fd);
          return(0);
          break;
        case MUS_AUDIO_FORMAT: /* this is asking for configuration info -- we return an array with per-"device" channels */
          err = ioctl(fd,SOUND_MIXER_READ_STEREODEVS,&stereodevs);
          for (ind=0;ind<=MUS_AUDIO_SYNTH;ind++) {if (chan>ind) val[ind]=0;}
          if (SOUND_MASK_IMIX & devmask) {if (chan>MUS_AUDIO_IMIX) val[MUS_AUDIO_IMIX] = ((SOUND_MASK_IMIX & stereodevs) ? 2 : 1);}
          if (SOUND_MASK_IGAIN & devmask) {if (chan>MUS_AUDIO_IGAIN) val[MUS_AUDIO_IGAIN] = ((SOUND_MASK_IGAIN & stereodevs) ? 2 : 1);}
          if (SOUND_MASK_RECLEV & devmask) {if (chan>MUS_AUDIO_RECLEV) val[MUS_AUDIO_RECLEV] = ((SOUND_MASK_RECLEV & stereodevs) ? 2 : 1);}
          if (SOUND_MASK_PCM & devmask) {if (chan>MUS_AUDIO_PCM) val[MUS_AUDIO_PCM] = ((SOUND_MASK_PCM & stereodevs) ? 2 : 1);}
          if (SOUND_MASK_ALTPCM & devmask) {if (chan>MUS_AUDIO_PCM2) val[MUS_AUDIO_PCM2] = ((SOUND_MASK_ALTPCM & stereodevs) ? 2 : 1);}
          if (SOUND_MASK_OGAIN & devmask) {if (chan>MUS_AUDIO_OGAIN) val[MUS_AUDIO_OGAIN] = ((SOUND_MASK_OGAIN & stereodevs) ? 2 : 1);}
          if (SOUND_MASK_LINE & devmask) {if (chan>MUS_AUDIO_LINE) val[MUS_AUDIO_LINE] = ((SOUND_MASK_LINE & stereodevs) ? 2 : 1);}
          if (SOUND_MASK_MIC & devmask) {if (chan>MUS_AUDIO_MICROPHONE) val[MUS_AUDIO_MICROPHONE] = ((SOUND_MASK_MIC & stereodevs) ? 2 : 1);}
          if (SOUND_MASK_LINE1 & devmask) {if (chan>MUS_AUDIO_LINE1) val[MUS_AUDIO_LINE1] = ((SOUND_MASK_LINE1 & stereodevs) ? 2 : 1);}
          if (SOUND_MASK_LINE2 & devmask) {if (chan>MUS_AUDIO_LINE2) val[MUS_AUDIO_LINE2] = ((SOUND_MASK_LINE2 & stereodevs) ? 2 : 1);}
          if (SOUND_MASK_LINE3 & devmask) {if (chan>MUS_AUDIO_LINE3) val[MUS_AUDIO_LINE3] = ((SOUND_MASK_LINE3 & stereodevs) ? 2 : 1);}
          if (SOUND_MASK_SYNTH & devmask) {if (chan>MUS_AUDIO_SYNTH) val[MUS_AUDIO_SYNTH] = ((SOUND_MASK_SYNTH & stereodevs) ? 2 : 1);}
          if (SOUND_MASK_CD & devmask) {if (chan>MUS_AUDIO_CD) val[MUS_AUDIO_CD] = ((SOUND_MASK_CD & stereodevs) ? 2 : 1);}
          linux_audio_close(fd);
          return(0);
          break;
        default: AUDIO_ERROR = MUS_AUDIO_CANT_READ; break;
        }
      if (chan == 0)
        val[0] = ((float)(amp & 0xff))*0.01;
      else val[0] = (((float)((amp & 0xff00) >> 8))*0.01);
    }
  else
    {
      switch (field)
        {
        case MUS_AUDIO_PORT:
          ind = 1;
          val[1] = MUS_AUDIO_MIXER;
          if ((SOUND_MASK_MIC | SOUND_MASK_LINE | SOUND_MASK_CD) & devmask) {ind++; if (chan>ind) val[ind] = MUS_AUDIO_LINE_IN;}
          /* problem here is that microphone and line_in are mixed before the ADC */
          if (SOUND_MASK_SPEAKER & devmask) {ind++; if (chan>ind) val[ind] = MUS_AUDIO_SPEAKERS;}
          if (SOUND_MASK_VOLUME & devmask) {ind++; if (chan>ind) val[ind] = MUS_AUDIO_DAC_OUT;}
          if (SOUND_MASK_TREBLE & devmask) {ind++; if (chan>ind) val[ind] = MUS_AUDIO_DAC_FILTER;}
          /* DIGITAL1..3 as RECSRC(?) => MUS_AUDIO_DIGITAL_IN */
          val[0] = ind;
          break;
        case MUS_AUDIO_FORMAT:
          err = ioctl(fd,SOUND_PCM_GETFMTS,&formats);
          ind = 0;
          if (formats & (to_oss_format(MUS_BYTE))) {ind++; if (chan>ind) val[ind] = MUS_BYTE;}
          if (formats & (to_oss_format(MUS_BSHORT))) {ind++; if (chan>ind) val[ind] = MUS_BSHORT;}
          if (formats & (to_oss_format(MUS_UBYTE))) {ind++; if (chan>ind) val[ind] = MUS_UBYTE;}
          if (formats & (to_oss_format(MUS_MULAW))) {ind++; if (chan>ind) val[ind] = MUS_MULAW;}
          if (formats & (to_oss_format(MUS_ALAW))) {ind++; if (chan>ind) val[ind] = MUS_ALAW;}
          if (formats & (to_oss_format(MUS_LSHORT))) {ind++; if (chan>ind) val[ind] = MUS_LSHORT;}
          if (formats & (to_oss_format(MUS_UBSHORT))) {ind++; if (chan>ind) val[ind] = MUS_UBSHORT;}
          if (formats & (to_oss_format(MUS_ULSHORT))) {ind++; if (chan>ind) val[ind] = MUS_ULSHORT;}
          val[0] = ind;
          break;
        case MUS_AUDIO_CHANNEL:
          err = ioctl(fd,SOUND_MIXER_READ_STEREODEVS,&stereodevs);
          channels = 0;
          switch (dev)
            {
            case MUS_AUDIO_MICROPHONE: if (SOUND_MASK_MIC & devmask) {if (SOUND_MASK_MIC & stereodevs) channels = 2; else channels = 1;} break;
            case MUS_AUDIO_SPEAKERS: if (SOUND_MASK_SPEAKER & devmask) {if (SOUND_MASK_SPEAKER & stereodevs) channels = 2; else channels = 1;} break;
            case MUS_AUDIO_LINE_IN: if (SOUND_MASK_LINE & devmask) {if (SOUND_MASK_LINE & stereodevs) channels = 2; else channels = 1;} break;
            case MUS_AUDIO_LINE1: if (SOUND_MASK_LINE1 & devmask) {if (SOUND_MASK_LINE1 & stereodevs) channels = 2; else channels = 1;} break;
            case MUS_AUDIO_LINE2: if (SOUND_MASK_LINE2 & devmask) {if (SOUND_MASK_LINE2 & stereodevs) channels = 2; else channels = 1;} break;
            case MUS_AUDIO_LINE3: if (SOUND_MASK_LINE3 & devmask) {if (SOUND_MASK_LINE3 & stereodevs) channels = 2; else channels = 1;} break;
            case MUS_AUDIO_DAC_OUT: if (SOUND_MASK_VOLUME & devmask) {if (SOUND_MASK_VOLUME & stereodevs) channels = 2; else channels = 1;} break;
            case MUS_AUDIO_DEFAULT: if (SOUND_MASK_VOLUME & devmask) {if (SOUND_MASK_VOLUME & stereodevs) channels = 2; else channels = 1;} break;
            case MUS_AUDIO_CD: if (SOUND_MASK_CD & devmask) {if (SOUND_MASK_CD & stereodevs) channels = 2; else channels = 1;} break;
            case MUS_AUDIO_DUPLEX_DEFAULT: 
              err = ioctl(fd,SNDCTL_DSP_GETCAPS,&ind);
              if (err != -1)
                channels = (ind & DSP_CAP_DUPLEX);
              else channels = 0;
              break;
            default: AUDIO_ERROR = MUS_AUDIO_DEVICE_NOT_AVAILABLE; break;
            }
          val[0] = channels;
          break;
        case MUS_AUDIO_AMP:
          amp = 0;
          switch (dev)
            {
            case MUS_AUDIO_MICROPHONE: if (SOUND_MASK_MIC & devmask) err = ioctl(fd,MIXER_READ(SOUND_MIXER_MIC),&amp);       break;
            case MUS_AUDIO_SPEAKERS: if (SOUND_MASK_SPEAKER & devmask) err = ioctl(fd,MIXER_READ(SOUND_MIXER_SPEAKER),&amp); break; 
            case MUS_AUDIO_LINE_IN: if (SOUND_MASK_LINE & devmask) err = ioctl(fd,MIXER_READ(SOUND_MIXER_LINE),&amp);        break; 
            case MUS_AUDIO_LINE1: if (SOUND_MASK_LINE1 & devmask) err = ioctl(fd,MIXER_READ(SOUND_MIXER_LINE1),&amp);        break; 
            case MUS_AUDIO_LINE2: if (SOUND_MASK_LINE2 & devmask) err = ioctl(fd,MIXER_READ(SOUND_MIXER_LINE2),&amp);        break; 
            case MUS_AUDIO_LINE3: if (SOUND_MASK_LINE3 & devmask) err = ioctl(fd,MIXER_READ(SOUND_MIXER_LINE3),&amp);        break; 
            case MUS_AUDIO_DAC_OUT: if (SOUND_MASK_VOLUME & devmask) err = ioctl(fd,MIXER_READ(SOUND_MIXER_VOLUME),&amp);    break;
            case MUS_AUDIO_DEFAULT: if (SOUND_MASK_VOLUME & devmask) err = ioctl(fd,MIXER_READ(SOUND_MIXER_VOLUME),&amp);    break;
            case MUS_AUDIO_CD: if (SOUND_MASK_CD & devmask) err = ioctl(fd,MIXER_READ(SOUND_MIXER_CD),&amp);              break;
            default: AUDIO_ERROR = MUS_AUDIO_DEVICE_NOT_AVAILABLE; break;
            }
          if (chan == 0)
            val[0] = ((float)(amp & 0xff))*0.01;
          else val[0] = (((float)((amp & 0xff00) >> 8))*0.01);
          break;
	case MUS_AUDIO_SRATE:
	  srate = (int)(val[0]);
	  if (ioctl(fd,SNDCTL_DSP_SPEED,&srate) == -1) 
	    AUDIO_ERROR = MUS_AUDIO_SRATE_NOT_AVAILABLE;
	  else val[0] = (float)srate;
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
        default: AUDIO_ERROR = MUS_AUDIO_CANT_READ; break;
        }
    }
  linux_audio_close(fd);
  if (err) {AUDIO_ERROR = MUS_AUDIO_READ_ERROR; return(-1);}
  if (AUDIO_ERROR == MUS_AUDIO_NO_ERROR) return(0);
  return(-1);
}

static int oss_mus_audio_mixer_write(int ur_dev, int field, int chan, float *val)
{
  int fd,err = 0,devmask,vol,sys,dev;
  float amp[1];
  AUDIO_ERROR = MUS_AUDIO_NO_ERROR;
  sys = MUS_AUDIO_SYSTEM(ur_dev);
  dev = MUS_AUDIO_DEVICE(ur_dev);

#if HAVE_SAM_9407
  if (audio_type[sys] == SAM9407_DSP) return(MUS_AUDIO_NO_ERROR); /* XXX */
#endif

  if (audio_type[sys] == SONORUS_STUDIO) return(MUS_AUDIO_NO_ERROR); /* there are apparently volume controls, but they're not accessible yet */
  if (audio_type[sys] == RME_HAMMERFALL) return(MUS_AUDIO_NO_ERROR);

  fd = linux_audio_open(mixer_name(sys),O_RDWR | O_NONBLOCK,0,sys);
  if (fd == -1) 
    {
      fd = linux_audio_open(DAC_NAME,O_WRONLY,0,sys);
      if (fd == -1) return(error_exit(MUS_AUDIO_CANT_OPEN,-1));
    }
  if ((dev == MUS_AUDIO_MIXER) || (dev == MUS_AUDIO_DAC_FILTER)) /* these give access to all the on-board analog input gain controls */
    {
      mus_audio_mixer_read(ur_dev,field,(chan == 0) ? 1 : 0,amp);
      if (val[0] >= 0.99) val[0] = 0.99;  if (val[0] < 0.0) val[0] = 0.0;
      if (amp[0] >= 0.99) amp[0] = 0.99;
      if (chan == 0)
        vol = (((int)(amp[0]*100)) << 8) + ((int)(val[0]*100));
      else vol = (((int)(val[0]*100)) << 8) + ((int)(amp[0]*100));
      err = ioctl(fd,SOUND_MIXER_READ_DEVMASK,&devmask);
      switch (field)
        {
        case MUS_AUDIO_IMIX:   if (SOUND_MASK_IMIX & devmask) err = ioctl(fd,MIXER_WRITE(SOUND_MIXER_IMIX),&vol);     break;
        case MUS_AUDIO_IGAIN:  if (SOUND_MASK_IGAIN & devmask) err = ioctl(fd,MIXER_WRITE(SOUND_MIXER_IGAIN),&vol);   break;
        case MUS_AUDIO_RECLEV: if (SOUND_MASK_RECLEV & devmask) err = ioctl(fd,MIXER_WRITE(SOUND_MIXER_RECLEV),&vol); break;
        case MUS_AUDIO_PCM:    if (SOUND_MASK_PCM & devmask) err = ioctl(fd,MIXER_WRITE(SOUND_MIXER_PCM),&vol);       break;
        case MUS_AUDIO_PCM2:   if (SOUND_MASK_ALTPCM & devmask) err = ioctl(fd,MIXER_WRITE(SOUND_MIXER_ALTPCM),&vol); break;
        case MUS_AUDIO_OGAIN:  if (SOUND_MASK_OGAIN & devmask) err = ioctl(fd,MIXER_WRITE(SOUND_MIXER_OGAIN),&vol);   break;
        case MUS_AUDIO_LINE:   if (SOUND_MASK_LINE & devmask) err = ioctl(fd,MIXER_WRITE(SOUND_MIXER_LINE),&vol);     break;
        case MUS_AUDIO_MICROPHONE:    if (SOUND_MASK_MIC & devmask) err = ioctl(fd,MIXER_WRITE(SOUND_MIXER_MIC),&vol);       break;
        case MUS_AUDIO_LINE1:  if (SOUND_MASK_LINE1 & devmask) err = ioctl(fd,MIXER_WRITE(SOUND_MIXER_LINE1),&vol);   break;
        case MUS_AUDIO_LINE2:  if (SOUND_MASK_LINE2 & devmask) err = ioctl(fd,MIXER_WRITE(SOUND_MIXER_LINE2),&vol);   break;
        case MUS_AUDIO_LINE3:  if (SOUND_MASK_LINE3 & devmask) err = ioctl(fd,MIXER_WRITE(SOUND_MIXER_LINE3),&vol);   break;
        case MUS_AUDIO_SYNTH:  if (SOUND_MASK_SYNTH & devmask) err = ioctl(fd,MIXER_WRITE(SOUND_MIXER_SYNTH),&vol);   break;
        case MUS_AUDIO_BASS:   if (SOUND_MASK_BASS & devmask) err = ioctl(fd,MIXER_WRITE(SOUND_MIXER_BASS),&vol);     break;
        case MUS_AUDIO_TREBLE: if (SOUND_MASK_TREBLE & devmask) err = ioctl(fd,MIXER_WRITE(SOUND_MIXER_TREBLE),&vol); break;
        case MUS_AUDIO_CD:     if (SOUND_MASK_CD & devmask) err = ioctl(fd,MIXER_WRITE(SOUND_MIXER_CD),&vol); break;
        default: AUDIO_ERROR = MUS_AUDIO_CANT_WRITE; break;
        }
    }
  else
    {
      switch (field)
        {
        case MUS_AUDIO_AMP:
          /* need to read both channel amps, then change the one we're concerned with */
          mus_audio_mixer_read(ur_dev,field,(chan == 0) ? 1 : 0,amp);
          if (val[0] >= 0.99) val[0] = 0.99;  if (val[0] < 0.0) val[0] = 0.0;
          if (amp[0] >= 0.99) amp[0] = 0.99;
          if (chan == 0)
            vol = (((int)(amp[0]*100)) << 8) + ((int)(val[0]*100));
          else vol = (((int)(val[0]*100)) << 8) + ((int)(amp[0]*100));
          err = ioctl(fd,SOUND_MIXER_READ_DEVMASK,&devmask);
          switch (dev)
            {
            case MUS_AUDIO_MICROPHONE: if (SOUND_MASK_MIC & devmask) err = ioctl(fd,MIXER_WRITE(SOUND_MIXER_MIC),&vol);       break;
            case MUS_AUDIO_SPEAKERS: if (SOUND_MASK_SPEAKER & devmask) err = ioctl(fd,MIXER_WRITE(SOUND_MIXER_SPEAKER),&vol); break;
            case MUS_AUDIO_LINE_IN: if (SOUND_MASK_LINE & devmask) err = ioctl(fd,MIXER_WRITE(SOUND_MIXER_LINE),&vol);        break;
            case MUS_AUDIO_LINE1: if (SOUND_MASK_LINE1 & devmask) err = ioctl(fd,MIXER_WRITE(SOUND_MIXER_LINE1),&vol);        break;
            case MUS_AUDIO_LINE2: if (SOUND_MASK_LINE2 & devmask) err = ioctl(fd,MIXER_WRITE(SOUND_MIXER_LINE2),&vol);        break;
            case MUS_AUDIO_LINE3: if (SOUND_MASK_LINE3 & devmask) err = ioctl(fd,MIXER_WRITE(SOUND_MIXER_LINE3),&vol);        break;
            case MUS_AUDIO_DAC_OUT: if (SOUND_MASK_VOLUME & devmask) err = ioctl(fd,MIXER_WRITE(SOUND_MIXER_VOLUME),&vol);    break;
            case MUS_AUDIO_DEFAULT: if (SOUND_MASK_VOLUME & devmask) err = ioctl(fd,MIXER_WRITE(SOUND_MIXER_VOLUME),&vol);    break;
            case MUS_AUDIO_CD: if (SOUND_MASK_CD & devmask) err = ioctl(fd,MIXER_WRITE(SOUND_MIXER_CD),&vol);              break;
            default: AUDIO_ERROR = MUS_AUDIO_DEVICE_NOT_AVAILABLE; break;
            }
          break;
        case MUS_AUDIO_SRATE:
          vol = (int)val[0];
          if (dsp_reset) ioctl(fd,SNDCTL_DSP_RESET,0);  /* is this needed? */
          err = ioctl(fd,SNDCTL_DSP_SPEED,&vol);
          break;
        default: AUDIO_ERROR = MUS_AUDIO_CANT_WRITE; break;
          /* case MUS_AUDIO_FORMAT: to force 16-bit input or give up */
          /* case MUS_AUDIO_CHANNEL: to open as stereo if possible?? */
          /* case MUS_AUDIO_PORT: to open digital out? */
        }
    }
  linux_audio_close(fd);
  if (err) {AUDIO_ERROR = MUS_AUDIO_WRITE_ERROR; return(-1);}
  if (AUDIO_ERROR == MUS_AUDIO_NO_ERROR) return(0);
  return(-1);
}

static char *synth_names[] = 
  {"",
   "Adlib","SoundBlaster","ProAudio Spectrum","Gravis UltraSound","MPU 401",
   "SoundBlaster 16","SoundBlaster 16 MIDI","6850 UART","Gravis UltraSound 16","Microsoft",
   "Personal sound system","Ensoniq Soundscape","Personal sound system + MPU","Personal/Microsoft",
   "Mediatrix Pro","MAD16","MAD16 + MPU","CS4232","CS4232 + MPU","Maui",
   "Pseudo-MSS","Gravis Ultrasound PnP","UART 401"};

static char *synth_name(int i)
{
  if ((i>0) && (i<=SNDCARD_UART401)) 
    return(synth_names[i]);
  return("unknown");
}

static char *device_types[] = {"FM","Sampling","MIDI"};

static char *device_type(int i)
{
  if ((i>=0) && (i<=2))
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
  ioctl(fd,SOUND_PCM_WRITE_CHANNELS,&val);
  if (val != channels) return(-1);
  val = bits;
  ioctl(fd,SOUND_PCM_WRITE_BITS,&val);
  if (val != bits) return(-1);
  ioctl(fd,SOUND_PCM_WRITE_RATE,rate);
  return(0);
}

static void oss_describe_audio_state_1(void)
{
  /* this code taken largely from "Linux Multimedia Guide" by Jeff Tranter, O'Reilly & Associates, Inc 1996 */
  /* it is explicitly released under the GPL, so I think I can use it here without elaborate disguises */
  int fd;
  int status = 0,level,i,recsrc,devmask,recmask,stereodevs,caps,numdevs,rate,channels,bits,blocksize,formats,deffmt,min_rate,max_rate;
  struct synth_info sinfo;
  struct midi_info minfo;
  const char *sound_device_names[] = SOUND_DEVICE_LABELS;
  char dsp_name[16];
  char version[4];
  int dsp_num = 0;
#ifdef NEW_OSS
  mixer_info mixinfo;
  oss_sysinfo sysinfo;
#endif
  
  if (sound_cards <= 0) mus_audio_initialize();

#ifdef NEW_OSS
  fd = open(DAC_NAME,O_WRONLY,0);
  if (fd == -1) fd = open(SYNTH_NAME,O_RDONLY,0);
  if (fd == -1) fd = open(MIXER_NAME,O_RDONLY,0);
  if (fd != -1)
    {
      status = ioctl(fd,OSS_GETVERSION,&level);
      new_oss_running = (status == 0);
      status = ioctl(fd,OSS_SYSINFO,&sysinfo);
      close(fd);
    }
#endif

  if (!strbuf) strbuf = (char *)CALLOC(STRBUF_SIZE,sizeof(char));
  
  if (new_oss_running)
    {
#ifdef NEW_OSS
      if (status == 0)
	{
	  sprintf(strbuf,"OSS version: %s\n",sysinfo.version);
	  pprint(strbuf);
	}
      else
	{
	  sprintf(strbuf,"OSS version: %x.%x.%x\n",(level>>16)&0xff,(level>>8)&0xff,level&0xff);
	  pprint(strbuf);
	}
#else
      sprintf(strbuf,"OSS version: %x.%x.%x\n",(level>>16)&0xff,(level>>8)&0xff,level&0xff);
      pprint(strbuf);
#endif
    }
  else 
    {
      /* refers to the version upon compilation */
      sprintf(version,"%d",SOUND_VERSION);
      sprintf(strbuf,"OSS version: %c.%c.%c\n",version[0],version[1],version[2]);
      pprint(strbuf);
    }
  
  sprintf(strbuf,"%d card%s found",sound_cards,(sound_cards != 1) ? "s" : ""); pprint(strbuf);
  if (sound_cards > 1)
    {
      pprint(": ");
      for (i=0;i<sound_cards;i++)
	{
	  sprintf(strbuf,"/dev/dsp%d with /dev/mixer%d%s",audio_dsp[i],audio_mixer[i],(i<(sound_cards-1)) ? ", " : "");
	  pprint(strbuf);
	}
    }
  pprint("\n\n");

  fd = open(SYNTH_NAME,O_RDWR,0);
  if (fd == -1) fd = open(SYNTH_NAME,O_RDONLY,0);
  if (fd == -1) 
    {
      sprintf(strbuf,"%s: %s\n",SYNTH_NAME,strerror(errno)); pprint(strbuf); 
      pprint("no synth found\n"); 
    }
  else
    {
      /* ioctl(fd,SNDCTL_DSP_SETDUPLEX,0); */ /* try to enable "full duplex" mode -- appears to be a no-op */
      status = ioctl(fd,SNDCTL_SEQ_NRSYNTHS,&numdevs);
      if (status == -1) 
	{
	  close(fd); fd=-1;
	  pprint("no sequencer?");
	}
      else
	{
	  sprintf(strbuf,"/dev/sequencer: %d device%s installed\n",numdevs,(numdevs == 1) ? "" : "s"); 
	  pprint(strbuf);
	  for (i=0;i<numdevs;i++)
	    {
	      sinfo.device = i;
	      status = ioctl(fd,SNDCTL_SYNTH_INFO,&sinfo);
	      if (status != -1)
		{
		  sprintf(strbuf,"  device: %d: %s, %s, %d voices\n",i,sinfo.name,device_type(sinfo.synth_type),sinfo.nr_voices); 
		  pprint(strbuf);
		}
	    }
	  status = ioctl(fd,SNDCTL_SEQ_NRMIDIS,&numdevs);
	  if (status == -1) 
	    {
	      close(fd); fd=-1;
	      pprint("no midi");
	    }
	  else
	    {
	      sprintf(strbuf,"  %d midi device%s installed\n",numdevs,(numdevs == 1) ? "" : "s"); 
	      pprint(strbuf);
	      for (i=0;i<numdevs;i++)
		{
		  minfo.device = i;
		  status = ioctl(fd,SNDCTL_MIDI_INFO,&minfo);
		  if (status != -1)
		    {
		      sprintf(strbuf,"  device %d: %s, %s\n",i,minfo.name,synth_name(minfo.dev_type)); 
		      pprint(strbuf);
		    }}}}}
  if (fd != -1) close(fd);
  pprint("--------------------------------\n");

MIXER_INFO:
  sprintf(dsp_name,"%s%d",MIXER_NAME,dsp_num);
  fd = linux_audio_open(dsp_name,O_RDWR,0,0);
  if (fd == -1) 
    {
      /* maybe output only */
      fd = linux_audio_open(dsp_name,O_WRONLY,0,0);
      if (fd == -1)
        {
          if (dsp_num == 0) 
	    {
	      sprintf(dsp_name,"%s",DAC_NAME);
	      fd = linux_audio_open(DAC_NAME,O_RDWR,0,0);
	      if (fd == -1) 
		{
		  /* maybe output only */
		  fd = linux_audio_open(DAC_NAME,O_WRONLY,0,0);
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
  if (new_oss_running) status = ioctl(fd,SOUND_MIXER_INFO,&mixinfo);
#endif
  sprintf(strbuf,"%s",dsp_name);
  pprint(strbuf);
#ifdef NEW_OSS
  if ((new_oss_running) && (status == 0)) 
    {
      sprintf(strbuf," (%s",mixinfo.name);
      pprint(strbuf);
      for (i=0;i<sound_cards;i++)
	{
	  if ((audio_mixer[i] == dsp_num) && (audio_type[i] == SONORUS_STUDIO))
	    {
	      sprintf(strbuf," in mode %d",audio_mode[i]);
	      pprint(strbuf);
	      break;
	    }
	}
      pprint(")");
    }
#endif
  status = ioctl(fd,SOUND_MIXER_READ_RECSRC,&recsrc);
  if (status == -1) 
    {
      linux_audio_close(fd); fd=-1;
      pprint(" no recsrc\n");
    }
  else
    {
      status = ioctl(fd,SOUND_MIXER_READ_DEVMASK,&devmask);
      if ((status == -1) || (devmask == 0))
	{
	  linux_audio_close(fd); fd=-1;
	  if (status == -1) pprint(" no devmask\n"); else pprint(" (no reported devices)");
	}
      else
	{
	  status = ioctl(fd,SOUND_MIXER_READ_RECMASK,&recmask);
	  if (status == -1) 
	    {
	      pprint(" no recmask\n");
	      recmask = 0;
	    }
	  status = ioctl(fd,SOUND_MIXER_READ_STEREODEVS,&stereodevs);
	  if (status == -1) 
	    {
	      pprint(" no stereodevs\n");
	      stereodevs = 0;
	    }
	  status = ioctl(fd,SOUND_MIXER_READ_CAPS,&caps);
	  if (status == -1) 
	    {
	      pprint(" no caps\n");
	      caps = 0;
	    }
	  pprint(":\n\n"
		 "  mixer     recording  active     stereo    current\n"
		 "  channel   source     source     device    level\n"
		 "  --------  --------   --------   --------   -------- \n"); 
	  for (i=0;i<SOUND_MIXER_NRDEVICES;i++)
	    {
	      if ((1<<i) & devmask)
		{
		  sprintf(strbuf,"  %-10s",sound_device_names[i]); 
		  pprint(strbuf);
		  yes_no((1<<i) & recmask);
		  yes_no((1<<i) & recsrc);
		  yes_no((1<<i) & stereodevs);
		  status = ioctl(fd,MIXER_READ(i),&level);
		  if (status != -1)
		    {
		      if ((1<<i) & stereodevs)
			sprintf(strbuf,"  %.2f %.2f",(float)(level&0xff) * 0.01,(float)((level&0xff00)>>8) * 0.01);
		      else sprintf(strbuf,"  %.2f",(float)(level&0xff) * 0.01);
		      /* can't use %% here because subsequent fprintf in pprint evaluates the %! #$@$! */
		      pprint(strbuf);
		    }
		  pprint("\n"); 
		}
	    }
	  pprint("--------------------------------\n");
	}
    }

AUDIO_INFO:
  if (fd != -1) {linux_audio_close(fd); fd=-1;}
  sprintf(dsp_name,"%s%d",DAC_NAME,dsp_num);
  fd = linux_audio_open(dsp_name,O_RDWR,0,0);
  if ((fd == -1) && (dsp_num == 0)) fd = linux_audio_open(DAC_NAME,O_WRONLY,0,0);
  if (fd == -1) return;
  sprintf(strbuf,"%s:\n\n",dsp_name); pprint(strbuf);
  if ((ioctl(fd,SOUND_PCM_READ_RATE,&rate) != -1) &&
      (ioctl(fd,SOUND_PCM_READ_CHANNELS,&channels) != -1) &&
      (ioctl(fd,SOUND_PCM_READ_BITS,&bits) != -1) &&
      (ioctl(fd,SNDCTL_DSP_GETBLKSIZE,&blocksize) != -1))
    {
      sprintf(strbuf,"  defaults:\n    sampling rate: %d, chans: %d, sample size: %d bits, block size: %d bytes",rate,channels,bits,blocksize); 
      pprint(strbuf);

#ifdef SNDCTL_DSP_GETOSPACE
      {
	audio_buf_info abi;
	ioctl(fd,SNDCTL_DSP_GETOSPACE,&abi);
	sprintf(strbuf," (%d fragments)\n",abi.fragments);
	pprint(strbuf);
      }
#else
      pprint("\n");
#endif
 
      deffmt = AFMT_QUERY;
      if ((ioctl(fd,SOUND_PCM_SETFMT,&deffmt) != -1) &&
	  (ioctl(fd,SOUND_PCM_GETFMTS,&formats) != -1))
	{
	  pprint("  supported formats:\n"); 
	  if (formats & AFMT_MU_LAW) {pprint("    mulaw"); if (deffmt == AFMT_MU_LAW) pprint(" (default)"); pprint("\n");}
	  if (formats & AFMT_A_LAW)  {pprint("    alaw"); if (deffmt == AFMT_A_LAW) pprint(" (default)"); pprint("\n");}
	  if (formats & AFMT_IMA_ADPCM) {pprint("    adpcm"); if (deffmt == AFMT_IMA_ADPCM) pprint(" (default)"); pprint("\n");}
	  if (formats & AFMT_U8) {pprint("    unsigned byte"); if (deffmt == AFMT_U8) pprint(" (default)"); pprint("\n");}
	  if (formats & AFMT_S16_LE) {pprint("    signed little-endian short"); if (deffmt == AFMT_S16_LE) pprint(" (default)"); pprint("\n");}
	  if (formats & AFMT_S16_BE) {pprint("    signed big-endian short"); if (deffmt == AFMT_S16_BE) pprint(" (default)"); pprint("\n");}
	  if (formats & AFMT_S8) {pprint("    signed byte"); if (deffmt == AFMT_S8) pprint(" (default)"); pprint("\n");}
	  if (formats & AFMT_U16_LE) {pprint("    unsigned little-endian short"); if (deffmt == AFMT_U16_LE) pprint(" (default)"); pprint("\n");}
	  if (formats & AFMT_U16_BE) {pprint("    unsigned big-endian short"); if (deffmt == AFMT_U16_BE) pprint(" (default)"); pprint("\n");}
	  if (formats & AFMT_MPEG) {pprint("    mpeg 2"); if (deffmt == AFMT_MPEG) pprint(" (default)"); pprint("\n");}
#ifdef NEW_OSS
	  if (formats & AFMT_S32_LE) {pprint("    signed little-endian int"); if (deffmt == AFMT_S32_LE) pprint(" (default)"); pprint("\n");}
	  if (formats & AFMT_S32_BE) {pprint("    signed big-endian int"); if (deffmt == AFMT_S32_BE) pprint(" (default)"); pprint("\n");}
#endif
	  status = ioctl(fd,SNDCTL_DSP_GETCAPS,&caps);
	  if (status != -1)
	    {
	      if (caps & DSP_CAP_DUPLEX) pprint("  full duplex\n");
	      pprint("            sample        srate\n  channels   size      min      max\n");
	      for (channels=1;channels<=2;channels++)
		{
		  for (bits=8;bits<=16;bits+=8)
		    {
		      min_rate = 1;
		      if (set_dsp(fd,channels,bits,&min_rate) == -1) continue;
		      max_rate = 100000;
		      if (set_dsp(fd,channels,bits,&max_rate) == -1) continue;
		      sprintf(strbuf,"  %4d  %8d  %8d  %8d\n",channels,bits,min_rate,max_rate); 
		      pprint(strbuf);
		    }}}}}
  pprint("--------------------------------\n");
  linux_audio_close(fd); fd=-1;
  dsp_num++; 
  if (dsp_num < 16)
    {
      sprintf(dsp_name,"%s%d",MIXER_NAME,dsp_num);
      goto MIXER_INFO;
    }
}

static void oss_mus_audio_save (void)
{
  int afd,i,devmask,err,level,system,systems;
  systems = mus_audio_systems();
  for (system=0;system<systems;system++)
    {
      afd = linux_audio_open(mixer_name(system),O_RDONLY,0,0);
      if (afd == -1) {AUDIO_ERROR = MUS_AUDIO_CANT_OPEN; return;}
      ioctl(afd,SOUND_MIXER_READ_DEVMASK,&devmask);
      for (i=0;i<SOUND_MIXER_NRDEVICES;i++)
	{
	  mixer_state[system][i] = 0;
	  if ((1<<i) & devmask)
	    {
	      err = ioctl(afd,MIXER_READ(i),&level);
	      if (err != -1) mixer_state[system][i] = level;
	    }
	}
      ioctl(afd,SOUND_PCM_READ_RATE,&(init_srate[system]));
      ioctl(afd,SOUND_PCM_READ_CHANNELS,&(init_chans[system]));
      init_format[system] = AFMT_QUERY;
      ioctl(afd,SOUND_PCM_SETFMT,&(init_format[system]));
      linux_audio_close(afd);
    }
}

static void oss_mus_audio_restore (void)
{
  int afd,i,level,devmask,system,systems;
  systems = mus_audio_systems();
  for (system=0;system<systems;system++)
    {
      afd = linux_audio_open(mixer_name(system),O_RDWR,0,0);
      if (afd == -1) {AUDIO_ERROR = MUS_AUDIO_CANT_OPEN; return;}
      ioctl(afd,SOUND_PCM_WRITE_CHANNELS,&(init_chans[system]));
      ioctl(afd,SOUND_PCM_WRITE_RATE,&(init_srate[system]));
      ioctl(afd,SOUND_PCM_SETFMT,&(init_format[system]));
      ioctl(afd,SOUND_MIXER_READ_DEVMASK,&devmask);
      for (i=0;i<SOUND_MIXER_NRDEVICES;i++) 
	{
	  if ((1<<i) & devmask)
	    {
	      level = mixer_state[system][i];
	      ioctl(afd,MIXER_WRITE(i),&level);
	    }
	}
      linux_audio_close(afd);
    }
}

static void oss_mus_audio_mixer_save(const char *file)
{
  int fd,systems,i;
  mus_audio_save();
  fd = creat(file,0666);
  if (fd != -1)
    {
      systems = mus_audio_systems();
      for (i=0;i<systems;i++) 
	write(fd,(unsigned char *)mixer_state[i],MIXER_SIZE * sizeof(int));
      close(fd);
    }
}

static void oss_mus_audio_mixer_restore(const char *file)
{
  int fd,afd,i,level,devmask,system,systems;
  int vals[MAX_SOUNDCARDS][MIXER_SIZE];
  fd = open(file,O_RDONLY,0);
  if (fd != -1)
    {
      systems = mus_audio_systems();
      for (i=0;i<systems;i++)
	read(fd,(unsigned char *)vals[i],MIXER_SIZE * sizeof(int));
      close(fd);
      for (system=0;system<systems;system++)
	{
	  afd = linux_audio_open(mixer_name(system),O_RDONLY,0,0);
	  if (afd != -1)
	    {
	      ioctl(afd,SOUND_MIXER_READ_DEVMASK,&devmask);
	      for (i=0;i<SOUND_MIXER_NRDEVICES;i++) 
		{
		  if ((1<<i) & devmask)
		    {
		      level = vals[system][i];
		      ioctl(afd,MIXER_WRITE(i),&level);
		    }
		}
	      linux_audio_close(afd);
	    }
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
static char* (*vect_mus_audio_error_name)(int err);
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
static void  (*vect_mus_audio_mixer_save)(const char* file);
static void  (*vect_mus_audio_mixer_restore)(const char* file);
static void  (*vect_describe_audio_state_1)(void);

/* vectors for the rest of the sndlib api */
int mus_audio_initialize(void) 
{
  return(probe_api());
}

void mus_audio_set_oss_buffers(int num, int size) 
{
  return(vect_mus_audio_set_oss_buffers(num, size));
}

char* mus_audio_error_name(int err) 
{
  return(vect_mus_audio_error_name(err));
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
  both_names = (char *)CALLOC(128,sizeof(char));
  /* need to be careful here since these use the same constant buffer */
  strcpy(both_names,oss_mus_audio_moniker());
  strcat(both_names,", ");
  strcat(both_names,alsa_mus_audio_moniker());
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
  return(vect_mus_audio_save());
}

void mus_audio_restore(void) 
{
  return(vect_mus_audio_restore());
}

void mus_audio_mixer_save(const char* file) 
{
  return(vect_mus_audio_mixer_save(file));
}

void mus_audio_mixer_restore(const char* file)
{
  return(vect_mus_audio_mixer_restore(file));
}

static void describe_audio_state_1(void) 
{
  return(vect_describe_audio_state_1());
}

#if (!HAVE_ALSA)
static int probe_api(void) 
{
  /* go for the oss api */
  api = OSS_API;
  vect_mus_audio_initialize = oss_mus_audio_initialize;
  vect_mus_audio_set_oss_buffers = oss_mus_audio_set_oss_buffers;
  vect_mus_audio_error_name = oss_mus_audio_error_name;
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
  vect_mus_audio_mixer_save = oss_mus_audio_mixer_save;
  vect_mus_audio_mixer_restore = oss_mus_audio_mixer_restore;
  vect_describe_audio_state_1 = oss_describe_audio_state_1;
  return(vect_mus_audio_initialize());
}
#endif

#endif



/* ------------------------------- ALSA ----------------------------------------- */
/* 
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
 *     dac_buffer_size so that larger alsa fragments can be accomodated (the 
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
#include <sys/asoundlib.h>

/* prototypes for the alsa sndlib functions */
static int   alsa_mus_audio_initialize(void);
static void  alsa_mus_audio_set_oss_buffers(int num, int size);
static char* alsa_mus_audio_error_name(int err);
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
static void  alsa_mus_audio_mixer_save(const char* file);
static void  alsa_mus_audio_mixer_restore(const char* file);
static void  alsa_describe_audio_state_1(void);

/* decide which api to activate */

static int probe_api(void) 
{
    unsigned int mask=snd_cards_mask();
    if (mask) {
	/* the alsa library has detected one or more cards */
	api = ALSA_API;
	vect_mus_audio_initialize = alsa_mus_audio_initialize;
	vect_mus_audio_set_oss_buffers = alsa_mus_audio_set_oss_buffers;
	vect_mus_audio_error_name = alsa_mus_audio_error_name;
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
	vect_mus_audio_mixer_save = alsa_mus_audio_mixer_save;
	vect_mus_audio_mixer_restore = alsa_mus_audio_mixer_restore;
	vect_describe_audio_state_1 = alsa_describe_audio_state_1;
    } else {
	/* go for the oss api */
        api = OSS_API;
	vect_mus_audio_initialize = oss_mus_audio_initialize;
	vect_mus_audio_set_oss_buffers = oss_mus_audio_set_oss_buffers;
	vect_mus_audio_error_name = oss_mus_audio_error_name;
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
	vect_mus_audio_mixer_save = oss_mus_audio_mixer_save;
	vect_mus_audio_mixer_restore = oss_mus_audio_mixer_restore;
	vect_describe_audio_state_1 = oss_describe_audio_state_1;
    }
    /* will the _real_ mus_audio_initialize please stand up? */
    return(vect_mus_audio_initialize());
}

/* size of buffer in number of samples per channel, 
 * at 44100 approximately 5.9mSecs
 */

/* static int samples_per_channel = 512; */
static int samples_per_channel = 1024;

/* static char dev_name[64]; */

/* this should go away as it is oss specific */

static int fragment_size = 512; 
static int fragments = 4;

static void alsa_mus_audio_set_oss_buffers (int num, int size) {
    fragments = num; 
    fragment_size = size; 
#if DEBUGGING
    mus_error(MUS_AUDIO_NO_ERROR,"set_oss_buffers: %d fragments or size %d", num, size);
#endif
}

static char *alsa_mus_audio_error_name(int err) {return(audio_error_name_1(err));}

/* total number of soundcards in our setup, set by initialize_audio */

/* static int sound_cards = 0; */

/* holds information about one device in a system or soundcard */

#if SND_LIB_VERSION < ((0<<16)|(6<<8)|(0))
typedef struct device_info {
    /* next device for this soundcard */
    struct device_info *next;
    /* alsa device number */
    int device;
    /* alsa channel */
    int channel;
    snd_pcm_channel_info_t info;
} device_info_t;
#else
typedef struct device_info {
    /* next device for this soundcard */
    struct device_info *next;
    /* alsa device number */
    int device;
    /* alsa handle */
    snd_pcm_t *handle;
    /* alsa stream */
    int stream;
    int format_width;  /* bytes */
    int channels;  
    snd_pcm_info_t info;
} device_info_t;
#endif

/* holds information about open handles in a system or soundcard */

typedef struct handle_info {
#if SND_LIB_VERSION < ((0<<16)|(6<<8)|(0))
  snd_pcm_t     *handle;
#else
  int in_use;
#endif
  device_info_t *playback_device;
  device_info_t *capture_device;
} handle_info_t;

/* indexed pointers to device information chain */

#define MAX_HANDLES (SND_CARDS*8)
static handle_info_t handles[MAX_HANDLES];

/* holds hardware information about a soundcard */

typedef struct card_info {
    snd_ctl_hw_info_t* info;
} card_info_t;

/* information about all soundcards */

static card_info_t card_info[SND_CARDS];

/* information about all devices */

#if SND_LIB_VERSION < ((0<<16)|(6<<8)|(0))
static device_info_t *device_info[SND_CARDS];
#else
static device_info_t *device_info[SND_CARDS][2];
#endif

/* to switch between pcm and plugin versions... 
 *
 * in the latest version tested (0.5.7 on April 10 2000) the plugin
 * versions of the function don't recover from underruns, this was
 * added so I could easily switch between the plugin and normal
 * functions. I tried the plugin functions to enable the automatic
 * format conversion available...
 */

#if SND_LIB_VERSION < ((0<<16)|(6<<8)|(0))
  static int (*alsa_info)(snd_pcm_t *handle, snd_pcm_channel_info_t *info);
  static int (*alsa_params)(snd_pcm_t *handle, snd_pcm_channel_params_t *params);
  static int (*alsa_prepare)(snd_pcm_t *handle, int channel);
  static int (*alsa_setup)(snd_pcm_t *handle, snd_pcm_channel_setup_t *setup);
  static int (*alsa_status)(snd_pcm_t *handle, snd_pcm_channel_status_t *status);
  static int (*alsa_flush)(snd_pcm_t *handle, int channel);
#else
  static int (*alsa_info)(snd_pcm_t *handle, snd_pcm_info_t *info);
  static int (*alsa_params)(snd_pcm_t *handle, snd_pcm_params_t *params);
  static int (*alsa_prepare)(snd_pcm_t *handle);
  static int (*alsa_setup)(snd_pcm_t *handle, snd_pcm_setup_t *setup);
  static int (*alsa_status)(snd_pcm_t *handle, snd_pcm_status_t *status);
  static int (*alsa_flush)(snd_pcm_t *handle);
#endif
static ssize_t (*alsa_write)(snd_pcm_t *handle, const void *buffer, size_t size);
static ssize_t (*alsa_read)(snd_pcm_t *handle, void *buffer, size_t size);

static int use_plugins=0;

static void alsa_initialize_functions()
{
#if SND_LIB_VERSION < ((0<<16)|(6<<8)|(0))
    if (use_plugins==0) {
	alsa_info=snd_pcm_channel_info;
	alsa_params=snd_pcm_channel_params;
	alsa_prepare=snd_pcm_channel_prepare;
	alsa_setup=snd_pcm_channel_setup;
	alsa_status=snd_pcm_channel_status;
	alsa_flush=snd_pcm_channel_flush;
	alsa_write=snd_pcm_write;
	alsa_read=snd_pcm_read;
    } else {
	alsa_info=snd_pcm_plugin_info;
	alsa_params=snd_pcm_plugin_params;
	alsa_prepare=snd_pcm_plugin_prepare;
	alsa_setup=snd_pcm_plugin_setup;
	alsa_status=snd_pcm_plugin_status;
	alsa_flush=snd_pcm_plugin_flush;
	alsa_write=snd_pcm_plugin_write;
	alsa_read=snd_pcm_plugin_read;
    }
#else
    alsa_info=snd_pcm_info;
    alsa_params=snd_pcm_params;
    alsa_prepare=snd_pcm_prepare;
    alsa_setup=snd_pcm_setup;
    alsa_status=snd_pcm_status;
    alsa_flush=snd_pcm_flush;
    alsa_write=snd_pcm_write;
    alsa_read=snd_pcm_read;
#endif
}

/* what to do on stop 
 *
 * this is here to make it easy to test the two different
 * behaviors on stop
 */

/* static int stop_mode=SND_PCM_STOP_ROLLOVER;*/
#if SND_LIB_VERSION < ((0<<16)|(6<<8)|(0))
  static int stop_mode=SND_PCM_STOP_STOP; 
#else
  static int stop_mode=SND_PCM_XRUN_FLUSH;
#endif

/* whether we want to trace calls 
 *
 * set to "1" to print function trace information in the 
 * snd error window
 */

static int trace_calls=0;

/* decode sndlib data into readable formats */

static const char *decode_mus_audio_field(int field) 
{
  if (MUS_AUDIO_DEVICE_OK(field))
    return(mus_audio_device_names[field]);
  else return("[undefined]");
}

static const char *decode_mus_audio_device(int device) 
{
  if (MUS_AUDIO_DEVICE_OK(device))
    return(mus_audio_device_names[device]);
  else return("[undefined]");
}

static const char *decode_mus_format(int format)
{
  if (MUS_DATA_FORMAT_OK(format))
    return(mus_audio_format_names[format]);
  else return("[undefined]");
}

static const char *decode_alsa_format(int format) 
{
    switch (format) {
    case SND_PCM_FMT_S8:
	return("SND_PCM_FMT_S8");
    case SND_PCM_FMT_U8:
	return("SND_PCM_FMT_U8");
    case SND_PCM_FMT_S16_LE:
	return("SND_PCM_FMT_S16_LE");
    case SND_PCM_FMT_S16_BE:
	return("SND_PCM_FMT_S16_BE");
    case SND_PCM_FMT_U16_LE:
	return("SND_PCM_FMT_U16_LE");
    case SND_PCM_FMT_U16_BE:
	return("SND_PCM_FMT_U16_BE");
    case SND_PCM_FMT_S24_LE:
	return("SND_PCM_FMT_S24_LE");
    case SND_PCM_FMT_S24_BE:
	return("SND_PCM_FMT_S24_BE");
    case SND_PCM_FMT_U24_LE:
	return("SND_PCM_FMT_U24_LE");
    case SND_PCM_FMT_U24_BE:
	return("SND_PCM_FMT_U24_BE");
    case SND_PCM_FMT_S32_LE:
	return("SND_PCM_FMT_S32_LE");
    case SND_PCM_FMT_S32_BE:
	return("SND_PCM_FMT_S32_BE");
    case SND_PCM_FMT_U32_LE:
	return("SND_PCM_FMT_U32_LE");
    case SND_PCM_FMT_U32_BE:
	return("SND_PCM_FMT_U32_BE");
    case SND_PCM_FMT_FLOAT_LE:
	return("SND_PCM_FMT_FLOAT_LE");
    case SND_PCM_FMT_FLOAT_BE:
	return("SND_PCM_FMT_FLOAT_BE");
    case SND_PCM_FMT_FLOAT64_LE:
	return("SND_PCM_FMT_FLOAT64_LE");
    case SND_PCM_FMT_FLOAT64_BE:
	return("SND_PCM_FMT_FLOAT64_BE");
    case SND_PCM_FMT_IEC958_SUBFRAME_LE:
	return("SND_PCM_FMT_IEC958_SUBFRAME_LE");
    case SND_PCM_FMT_IEC958_SUBFRAME_BE:
	return("SND_PCM_FMT_IEC958_SUBFRAME_BE");
    case SND_PCM_FMT_MU_LAW:
	return("SND_PCM_FMT_MU_LAW");
    case SND_PCM_FMT_A_LAW:
	return("SND_PCM_FMT_A_LAW");
    case SND_PCM_FMT_IMA_ADPCM:
	return("SND_PCM_FMT_IMA_ADPCM");
    case SND_PCM_FMT_MPEG:
	return("SND_PCM_FMT_MPEG");
    case SND_PCM_FMT_GSM:
	return("SND_PCM_FMT_GSM");
    case SND_PCM_FMT_SPECIAL:
	return("SND_PCM_FMT_SPECIAL");
    default:
	return("[undefined]");
    }
}

/* set schedulling priority to SCHED_FIFO 
 * this will only work if snd is run as root or is suid root */

#if (!defined(HAVE_CONFIG_H)) || (defined(HAVE_SCHED_H))
#include <sched.h>

static void set_priority() 
{
    struct sched_param p;
    int min, max, priority;

    if (!getuid() || !geteuid()) {
	min=sched_get_priority_min(SCHED_FIFO);
	max=sched_get_priority_max(SCHED_FIFO);
	priority=min+(max-min)/2;
	p.sched_priority=priority;
	if (sched_setscheduler(0, SCHED_FIFO, &p)==-1) {
	    fprintf(stderr, "\ncould not activate scheduling with priority %d\n", priority);
	}
	seteuid(getuid());
    }
}
#else
static void set_priority() {}
#endif

/* count the soundcards and store their hardware info  */

#if SND_LIB_VERSION < ((0<<16)|(6<<8)|(0))
static int alsa_mus_audio_initialize(void) 
{
    device_info_t **dev_info;
    snd_pcm_info_t pcminfo;
    snd_ctl_t *handle;
    int card, dev, err;
    unsigned int mask;

    if (audio_initialized) {
	return(0);
    }
    /* set the process schedulling policy to real-time SCHED_FIFO */
    set_priority();
    /* initialize function pointers for normal or plugin calls */
    alsa_initialize_functions();
    /* allocate various things */
    dev_name = (char *)CALLOC(64,sizeof(char));
    /* scan for soundcards */
    mask=snd_cards_mask();
    if (!mask) {
	return(0);
    }
    for (card=0; card<SND_CARDS; card++) {
	if (mask & (1<<card)) {
	    if ((err=snd_ctl_open(&handle, card))<0) {
		mus_error(MUS_AUDIO_CANT_OPEN, "%s: snd_ctl_open[%d]: %s", 
			  __FUNCTION__, card, snd_strerror(err));
		continue;
	    }
	    if (card_info[card].info==NULL) {
		card_info[card].info=(snd_ctl_hw_info_t*)CALLOC(1,sizeof(snd_ctl_hw_info_t));
	    }
	    if ((err=snd_ctl_hw_info(handle, card_info[card].info))<0) {
		mus_error(MUS_AUDIO_CANT_OPEN, "%s: snd_ctl_hw_info[%d]: %s", 
			  __FUNCTION__, card, snd_strerror(err));
		snd_ctl_close(handle);
		continue;
	    }
	    dev_info=&device_info[card];
	    for (dev=0; dev<card_info[card].info->pcmdevs; dev++) {
		/* get information for the soundcard */
		if ((err=snd_ctl_pcm_info(handle, dev, &pcminfo)) < 0) {
		    mus_error(MUS_AUDIO_CANT_OPEN, "%s: snd_ctl_pcm_info[%d:%d]: %s", 
			      __FUNCTION__, card, dev, snd_strerror(err));
		    continue;
		}
		/* get information for the pcm playback devices */
		if (pcminfo.flags & SND_PCM_INFO_PLAYBACK) {
		    snd_pcm_t *dhandle;
		    if ((err=snd_pcm_open(&dhandle, card, dev, SND_PCM_OPEN_PLAYBACK))!=0) {
			mus_error(MUS_AUDIO_CANT_OPEN, "%s: snd_pcm_open[%d:%d:playback]: %s", 
				  __FUNCTION__, card, dev, snd_strerror(err));
		    } else {
			if(*dev_info==NULL) {
			    (*dev_info)=(device_info_t*)CALLOC(1,sizeof(device_info_t));
			}
			(*dev_info)->device=dev;
			(*dev_info)->channel=SND_PCM_CHANNEL_PLAYBACK;
			(*dev_info)->info.channel=SND_PCM_CHANNEL_PLAYBACK;
			if ((err=alsa_info(dhandle, &(*dev_info)->info))!=0) {
			    mus_error(MUS_AUDIO_CANT_OPEN, "%s: snd_pcm_info[%d:%d:playback]: %s", 
				      __FUNCTION__, card, dev, snd_strerror(err));
			} else {			    
			    dev_info=&(*dev_info)->next;
			}
			snd_pcm_close(dhandle);
		    }
		}
		/* get information for the pcm capture devices */
		if (pcminfo.flags & SND_PCM_INFO_CAPTURE) {
		    snd_pcm_t *dhandle;
		    if ((err=snd_pcm_open(&dhandle, card, dev, SND_PCM_OPEN_CAPTURE))!=0) {
			mus_error(MUS_AUDIO_CANT_OPEN, "%s: snd_pcm_open[%d:%d]: %s", 
				  __FUNCTION__, card, dev, snd_strerror(err));
		    } else {
			if((*dev_info)==NULL) {
			    (*dev_info)=(device_info_t*)CALLOC(1,sizeof(device_info_t));
			}
			(*dev_info)->device=dev;
			(*dev_info)->channel=SND_PCM_CHANNEL_CAPTURE;
			(*dev_info)->info.channel=SND_PCM_CHANNEL_CAPTURE;
			if ((err=alsa_info(dhandle, &(*dev_info)->info))!=0) {
			    mus_error(MUS_AUDIO_CANT_OPEN, "%s: snd_pcm_info[%d:%d:capture]: %s", 
				      __FUNCTION__, card, dev, snd_strerror(err));
			} else {
			    dev_info=&(*dev_info)->next;
			}
			snd_pcm_close(dhandle);
		    }
		}
	    }
	    snd_ctl_close(handle);
	    sound_cards++;
	}
    }
    audio_initialized=1;
    return 0;
}
#else
static int alsa_mus_audio_initialize(void) 
{
    device_info_t **dev_info;
    snd_pcm_info_t pcminfo;
    snd_ctl_t *handle;
    snd_pcm_t *pcm_handle;		    
    int card, dev, subdev, err;
    unsigned int mask;

    if (audio_initialized) {
	return(0);
    }
    /* set the process schedulling policy to real-time SCHED_FIFO */
    set_priority();
    /* initialize function pointers for normal or plugin calls */
    alsa_initialize_functions();
    /* allocate various things */
    dev_name = (char *)CALLOC(64,sizeof(char));
    /* scan for soundcards */
    mask=snd_cards_mask();
    if (!mask) {
	return(0);
    }
    for (card=0; card<SND_CARDS; card++) {
	if (mask & (1<<card)) {
	    if ((err=snd_ctl_open(&handle, card))<0) {
		mus_error(MUS_AUDIO_CANT_OPEN, "%s: snd_ctl_open[%d]: %s", 
			  __FUNCTION__, card, snd_strerror(err));
		continue;
	    }
	    if (card_info[card].info==NULL) {
		card_info[card].info=(snd_ctl_hw_info_t*)CALLOC(1,sizeof(snd_ctl_hw_info_t));
	    }
	    if ((err=snd_ctl_hw_info(handle, card_info[card].info))<0) {
		mus_error(MUS_AUDIO_CANT_OPEN, "%s: snd_ctl_hw_info[%d]: %s", 
			  __FUNCTION__, card, snd_strerror(err));
		snd_ctl_close(handle);
		continue;
	    }
	    dev_info=&device_info[card][SND_PCM_STREAM_PLAYBACK];
	    for (dev=0; dev<card_info[card].info->pcmdevs; dev++) {
		if ((err=snd_pcm_hw_open(&pcm_handle, card, dev, SND_PCM_STREAM_PLAYBACK, 0)) <0) {
			if (err == -ENOENT) {
				/* no support for this stream */
				continue;
			}
			mus_error(MUS_AUDIO_CANT_OPEN, "%s: playback snd_pcm_hw_open[%d:%d:%d]: %s", 
				  __FUNCTION__, card, dev, subdev, snd_strerror(err));
			continue;
		}
			
		if(*dev_info==NULL) {
			(*dev_info)=(device_info_t*)CALLOC(1,sizeof(device_info_t));
		}
		(*dev_info)->device=dev;
		(*dev_info)->stream=SND_PCM_STREAM_PLAYBACK;
		memset(&pcminfo, 0, sizeof(pcminfo));
                pcminfo.device = dev;
		pcminfo.stream = SND_PCM_STREAM_PLAYBACK;
		pcminfo.subdevice = -1;
		if ((err = snd_pcm_info (pcm_handle, &(*dev_info)->info)) < 0) {
			mus_error(MUS_AUDIO_CANT_OPEN, "%s: playback snd_pcm_info[%d:%d]: %s", 
				  __FUNCTION__, card, dev, snd_strerror(err));
		} else {			    
			dev_info=&(*dev_info)->next;
		}
		snd_pcm_close(pcm_handle);
	    }
	    dev_info=&device_info[card][SND_PCM_STREAM_CAPTURE];
	    for (dev=0; dev<card_info[card].info->pcmdevs; dev++) {
		if ((err=snd_pcm_hw_open(&pcm_handle, card, dev, SND_PCM_STREAM_CAPTURE, 0)) <0) {
			if (err == -ENOENT) {
				/* no support for this stream */
				continue;
			}
			mus_error(MUS_AUDIO_CANT_OPEN, "%s: capture snd_pcm_hw_open[%d:%d:%d]: %s", 
				  __FUNCTION__, card, dev, subdev, snd_strerror(err));
			continue;
		}

		if(*dev_info==NULL) {
			(*dev_info)=(device_info_t*)CALLOC(1,sizeof(device_info_t));
		}
		(*dev_info)->device=dev;
		(*dev_info)->stream=SND_PCM_STREAM_CAPTURE;
		memset(&pcminfo, 0, sizeof(pcminfo));
                pcminfo.device = dev;
		pcminfo.stream = SND_PCM_STREAM_CAPTURE;
		pcminfo.subdevice = -1;
		if ((err = snd_pcm_info (pcm_handle, &(*dev_info)->info)) < 0) {
			mus_error(MUS_AUDIO_CANT_OPEN, "%s: capture snd_pcm_info[%d:%d]: %s", 
				  __FUNCTION__, card, dev, snd_strerror(err));
		} else {			    
			dev_info=&(*dev_info)->next;
		}
		snd_pcm_close(pcm_handle);
	    }
	    snd_ctl_close(handle);
	    sound_cards++;
	}
    }
    audio_initialized=1;
    return 0;
}
#endif

/* return the number of cards that are available */

static int alsa_mus_audio_systems(void) 
{
    return(sound_cards);
}

/* return the name of a given system (soundcard) */

static char *alsa_mus_audio_system_name(int system) 
{
    if (card_info[system].info!=NULL) {
	return(card_info[system].info->name);
    } else {
	return("");
    }
}

/* hummm, return the type of driver we're dealing with? */

static char *alsa_mus_audio_moniker(void)
{
  if (version_name == NULL) version_name = (char *)CALLOC(64,sizeof(char));
  sprintf(version_name, "ALSA %s", SND_LIB_VERSION_STR);
  return(version_name);
}

/* convert a sndlib sample format to an alsa sample format */

static int to_alsa_format(int snd_format)
{
    switch (snd_format) {
    case MUS_BYTE: 
	return(SND_PCM_SFMT_S8); 
    case MUS_UBYTE: 
	return(SND_PCM_SFMT_U8); 
    case MUS_MULAW: 
	return(SND_PCM_SFMT_MU_LAW); 
    case MUS_ALAW: 
	return(SND_PCM_SFMT_A_LAW); 
    case MUS_BSHORT: 
	return(SND_PCM_SFMT_S16_BE); 
    case MUS_LSHORT: 
	return(SND_PCM_SFMT_S16_LE); 
    case MUS_UBSHORT: 
	return(SND_PCM_SFMT_U16_BE); 
    case MUS_ULSHORT: 
	return(SND_PCM_SFMT_U16_LE); 
    case MUS_B24INT: 
	return(SND_PCM_SFMT_S24_BE); 
    case MUS_L24INT: 
	return(SND_PCM_SFMT_S24_LE); 
    case MUS_BINT: 
	return(SND_PCM_SFMT_S32_BE); 
    case MUS_LINT: 
	return(SND_PCM_SFMT_S32_LE); 
    case MUS_BINTN: 
	return(SND_PCM_SFMT_S32_BE); 
    case MUS_LINTN: 
	return(SND_PCM_SFMT_S32_LE); 
    case MUS_BFLOAT: 
	return(SND_PCM_SFMT_FLOAT_BE); 
    case MUS_LFLOAT: 
	return(SND_PCM_SFMT_FLOAT_LE); 
    case MUS_BDOUBLE: 
	return(SND_PCM_SFMT_FLOAT64_BE); 
    case MUS_LDOUBLE: 
	return(SND_PCM_SFMT_FLOAT64_LE); 
    }
    return(-1);
}

/* FIXME: this is not taking yet into account the 
 * number of bits that a given alsa format is actually
 * using... */

static int to_mus_format(int alsa_format) 
{
  /* alsa format definitions from asound.h (0.5.7) */
  switch (alsa_format)
    {
    case SND_PCM_FMT_S8:
      return(MUS_BYTE);
    case SND_PCM_FMT_U8:
      return(MUS_UBYTE);
    case SND_PCM_FMT_S16_LE:
      return(MUS_LSHORT);
    case SND_PCM_FMT_S16_BE:
      return(MUS_BSHORT);
    case SND_PCM_FMT_U16_LE:
      return(MUS_ULSHORT);
    case SND_PCM_FMT_U16_BE:
      return(MUS_UBSHORT);
    case SND_PCM_FMT_S24_LE:
      return(MUS_L24INT);
    case SND_PCM_FMT_S24_BE:
      return(MUS_B24INT);
    case SND_PCM_FMT_S32_LE:
      /* choose the 32bit normalized format 
       * plays 24bit and 16bit files with same
       * upper amplitude bound (for 24 bit cards) */
      return(MUS_LINTN);
    case SND_PCM_FMT_S32_BE:
      return(MUS_BINTN);
    case SND_PCM_FMT_FLOAT_LE:
      return(MUS_LFLOAT);
    case SND_PCM_FMT_FLOAT_BE:
      return(MUS_BFLOAT);
    case SND_PCM_FMT_FLOAT64_LE:
      return(MUS_LDOUBLE);
    case SND_PCM_FMT_FLOAT64_BE:
      return(MUS_BDOUBLE);
    case SND_PCM_FMT_MU_LAW:
      return(MUS_MULAW);
    case SND_PCM_FMT_A_LAW:
      return(MUS_ALAW);
    /* formats with no translation in snd */
    case SND_PCM_FMT_U24_LE:
    case SND_PCM_FMT_U24_BE:
    case SND_PCM_FMT_U32_LE:
    case SND_PCM_FMT_U32_BE:
    case SND_PCM_FMT_IEC958_SUBFRAME_LE:
    case SND_PCM_FMT_IEC958_SUBFRAME_BE:
    case SND_PCM_FMT_IMA_ADPCM:
    case SND_PCM_FMT_MPEG:
    case SND_PCM_FMT_GSM:
    case SND_PCM_FMT_SPECIAL:
    default:
      return(-1);
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

static int to_alsa_device(int dev, int *adev, int *achan)
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
	(*adev)=0;
#if SND_LIB_VERSION < ((0<<16)|(6<<8)|(0))
	(*achan)=SND_PCM_CHANNEL_PLAYBACK;
#else
	(*achan)=SND_PCM_STREAM_PLAYBACK;
#endif
	break;
    case MUS_AUDIO_AUX_OUTPUT:
	/* extra analog output */
	(*adev)=1;
#if SND_LIB_VERSION < ((0<<16)|(6<<8)|(0))
	(*achan)=SND_PCM_CHANNEL_PLAYBACK;
#else
	(*achan)=SND_PCM_STREAM_PLAYBACK;
#endif
	break;
    case MUS_AUDIO_DAC_OUT:
	/* analog outputs */
	(*adev)=2;
#if SND_LIB_VERSION < ((0<<16)|(6<<8)|(0))
	(*achan)=SND_PCM_CHANNEL_PLAYBACK;
#else
	(*achan)=SND_PCM_STREAM_PLAYBACK;
#endif
	break;
    case MUS_AUDIO_LINE_IN:
	/* analog input */
	(*adev)=0;
#if SND_LIB_VERSION < ((0<<16)|(6<<8)|(0))
	(*achan)=SND_PCM_CHANNEL_CAPTURE;
#else
	(*achan)=SND_PCM_STREAM_CAPTURE;
#endif
	break;
    case MUS_AUDIO_AUX_INPUT:
	/* extra analog input */
	(*adev)=1;
#if SND_LIB_VERSION < ((0<<16)|(6<<8)|(0))
	(*achan)=SND_PCM_CHANNEL_CAPTURE;
#else
	(*achan)=SND_PCM_STREAM_CAPTURE;
#endif
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
	return(-1);
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
#if SND_LIB_VERSION < ((0<<16)|(6<<8)|(0))
    case SND_PCM_CHANNEL_PLAYBACK:
#else
    case SND_PCM_STREAM_PLAYBACK:
#endif
	switch (dev) {
	/* works only for the first three outputs */
	case 0: return(MUS_AUDIO_LINE_OUT);
	case 1: return(MUS_AUDIO_AUX_OUTPUT);
	case 2: return(MUS_AUDIO_DAC_OUT);
	default:
	    return(-1);
	}
#if SND_LIB_VERSION < ((0<<16)|(6<<8)|(0))
    case SND_PCM_CHANNEL_CAPTURE:
#else
    case SND_PCM_STREAM_CAPTURE:
#endif
	switch (dev) {
	case 0: return(MUS_AUDIO_LINE_IN);
	case 1: return(MUS_AUDIO_AUX_INPUT);
	default:
	    return(-1);
	}
	break;
    }
    return(-1);
}

/* get information block for a card, device and channel */

#if SND_LIB_VERSION < ((0<<16)|(6<<8)|(0))
static device_info_t *alsa_get_info(int card, int device, int stream)
{
    device_info_t *info;
    info=device_info[card];
    while (info!=NULL) {
	if (info->device==device &&
	    info->channel==stream) {
	    return(info);
	    break;
	}
	info=info->next;
    }
    return(NULL);
}
#else
static device_info_t *alsa_get_info(int card, int device, int stream)
{
    device_info_t *info;
    info=device_info[card][stream];
    while (info!=NULL) {
	if (info->device==device) {
 	    return(info);
 	    break;
 	}
	info=info->next;
    }
    return(NULL);
}
#endif

/* flush and close an input or output stream */

static int alsa_audio_close(int id)
{
    int err=0;
    if (trace_calls) mus_error(MUS_AUDIO_NO_ERROR, "%s: %d\n", __FUNCTION__, id);
#if SND_LIB_VERSION < ((0<<16)|(6<<8)|(0))
    if (handles[id].handle!=NULL) {
	snd_pcm_channel_status_t status;
	memset(&status, 0, sizeof(status));
	if (handles[id].playback_device!=NULL) {
	    status.channel=SND_PCM_CHANNEL_PLAYBACK;
	    if ((err=alsa_status(handles[id].handle, &status))<0) {
		mus_error(MUS_AUDIO_CANT_CLOSE, "%s: alsa_status: %s", __FUNCTION__, snd_strerror(err));
	    } else {
		if (status.status==SND_PCM_STATUS_RUNNING) {
		    if ((err=alsa_flush(handles[id].handle, status.channel))<0) {
			mus_error(MUS_AUDIO_CANT_CLOSE, "%s: alsa_flush: %s", __FUNCTION__, snd_strerror(err));
		    }
		}
	    }
	} else {
	    status.channel=SND_PCM_CHANNEL_CAPTURE;
	}
	if ((err=snd_pcm_close(handles[id].handle))!=0) {
	    mus_error(MUS_AUDIO_CANT_CLOSE, "%s: snd_pcm_close: %s", __FUNCTION__, snd_strerror(err));
	}
	handles[id].handle=NULL;
	handles[id].playback_device=NULL;
	handles[id].capture_device=NULL;
    }
#else
    if (handles[id].playback_device!=NULL) {
	snd_pcm_status_t status;
	memset(&status, 0, sizeof(status));
	if (handles[id].playback_device!=NULL) {
	    if ((err=alsa_status(handles[id].playback_device->handle, &status))<0) {
		mus_error(MUS_AUDIO_CANT_CLOSE, "%s: alsa_status: %s", __FUNCTION__, snd_strerror(err));
	    } else {
		if (status.state==SND_PCM_STATE_RUNNING) {
		    if ((err=alsa_flush(handles[id].playback_device->handle))<0) {
			mus_error(MUS_AUDIO_CANT_CLOSE, "%s: alsa_flush: %s", __FUNCTION__, snd_strerror(err));
		    }
		}
	    }
	} 
	if ((err=snd_pcm_close(handles[id].playback_device->handle))!=0) {
	    mus_error(MUS_AUDIO_CANT_CLOSE, "%s: snd_pcm_close: playback: %s", __FUNCTION__, snd_strerror(err));
	}
    }
    if (handles[id].capture_device!=NULL) {
	if ((err=snd_pcm_close(handles[id].capture_device->handle))!=0) {
	    mus_error(MUS_AUDIO_CANT_CLOSE, "%s: snd_pcm_close: capture %s", __FUNCTION__, snd_strerror(err));
	}
    handles[id].in_use=0;
    handles[id].playback_device=NULL;
    handles[id].capture_device=NULL;
    }
#endif
    return(err);
}

/* open an input or output stream */

#if SND_LIB_VERSION < ((0<<16)|(6<<8)|(0))
static int alsa_audio_open(int card, int device)
{
    snd_pcm_t *handle;
    int alsa_device;
    int alsa_channel;
    int err=0;

    if (trace_calls) mus_error(MUS_AUDIO_NO_ERROR, "%s: %d:%d\n", __FUNCTION__, card, device);
    if ((err=to_alsa_device(device, &alsa_device, &alsa_channel))<0) {
	mus_error(MUS_AUDIO_DEVICE_NOT_AVAILABLE, "%s: cannot translate device %s<%d> to alsa",
		  __FUNCTION__, decode_mus_audio_device(device), device);
	return(-1);
    }
    if ((err=snd_pcm_open(&handle, card, alsa_device, 
			  ((alsa_channel==SND_PCM_CHANNEL_PLAYBACK)?
			   SND_PCM_OPEN_PLAYBACK:
			   SND_PCM_OPEN_CAPTURE)))!=0) {
	mus_error(MUS_AUDIO_CANT_OPEN, "%s: open %s<$d> card=%d adev=%d achan=%d: %s", __FUNCTION__,
		  decode_mus_audio_device(device), device, 
		  card, alsa_device, alsa_channel, snd_strerror(err));
    } else {
	int i;
	for (i=0; i<MAX_HANDLES; i++) {
	    if (handles[i].handle==NULL) {
		device_info_t *info;
		handles[i].handle=handle;
		switch (alsa_channel) {
		case SND_PCM_CHANNEL_PLAYBACK:
		    handles[i].playback_device=alsa_get_info(card, alsa_device, alsa_channel);
		    if (handles[i].playback_device==NULL) {
			mus_error(MUS_AUDIO_DEVICE_NOT_AVAILABLE,"%s: could not find info block for %d:%d:%d",
				  __FUNCTION__, card, alsa_device, alsa_channel);
			handles[i].handle=NULL;
			goto close_and_exit;
		    }
		    break;
		case SND_PCM_CHANNEL_CAPTURE:
		    handles[i].capture_device=alsa_get_info(card, alsa_device, alsa_channel);
		    if (handles[i].capture_device==NULL) {
			mus_error(MUS_AUDIO_DEVICE_NOT_AVAILABLE,"%s: could not find info block for %d:%d:%d",
				  __FUNCTION__, card, alsa_device, alsa_channel);
			handles[i].handle=NULL;
			goto close_and_exit;
		    }
		    break;
		default:
		    mus_error(MUS_AUDIO_CANT_OPEN, "%s: alsa channel %d is not playback or capture", 
			      __FUNCTION__, alsa_channel);
		    goto close_and_exit;
		}
		return(i);
	    }
	}
	mus_error(MUS_AUDIO_CANT_OPEN,"%s: ran out of handle storage space", __FUNCTION__);
    close_and_exit:
	if ((err=snd_pcm_close(handle))!=0) {
	    mus_error(MUS_AUDIO_CANT_CLOSE, "%s: snd_pcm_close; %s", __FUNCTION__, snd_strerror(err));
	}
    }
    return(-1);
}
#else
static int alsa_audio_open(int card, int device)
{
    snd_pcm_t *handle;
    int alsa_device;
    int alsa_stream;
    int err=0;

    if (trace_calls) mus_error(MUS_AUDIO_NO_ERROR, "%s: %d:%d\n", __FUNCTION__, card, device);
    if ((err=to_alsa_device(device, &alsa_device, &alsa_stream))<0) {
	mus_error(MUS_AUDIO_DEVICE_NOT_AVAILABLE, "%s: cannot translate device %s<%d> to alsa",
		  __FUNCTION__, decode_mus_audio_device(device), device);
	return(-1);
    }
    if ((err=snd_pcm_hw_open(&handle, card, alsa_device, alsa_stream, 0)) != 0) {
	mus_error(MUS_AUDIO_CANT_OPEN, "%s: open %s<$d> card=%d adev=%d achan=%d: %s", __FUNCTION__,
		  decode_mus_audio_device(device), device, 
		  card, alsa_device, alsa_stream, snd_strerror(err));
    } else {
	int i;
	for (i=0; i<MAX_HANDLES; i++) {
	    if (handles[i].in_use==0) {
		switch (alsa_stream) {
		case SND_PCM_STREAM_PLAYBACK:
		    handles[i].playback_device=alsa_get_info(card, alsa_device, alsa_stream);
		    if (handles[i].playback_device==NULL) {
			mus_error(MUS_AUDIO_DEVICE_NOT_AVAILABLE,"%s: could not find info block for %d:%d:%d",
				  __FUNCTION__, card, alsa_device, alsa_stream);
			goto close_and_exit;
		    }
		    handles[i].playback_device->handle=handle;
		    break;
		case SND_PCM_STREAM_CAPTURE:
		    handles[i].capture_device=alsa_get_info(card, alsa_device, alsa_stream);
		    if (handles[i].capture_device==NULL) {
			mus_error(MUS_AUDIO_DEVICE_NOT_AVAILABLE,"%s: could not find info block for %d:%d:%d",
				  __FUNCTION__, card, alsa_device, alsa_stream);
			goto close_and_exit;
		    }
		    handles[i].capture_device->handle=handle;
		    break;
		default:
		    mus_error(MUS_AUDIO_CANT_OPEN, "%s: alsa stream %d is not playback or capture", 
			      __FUNCTION__, alsa_stream);
		    goto close_and_exit;
		}
		handles[i].in_use = 1;
		return(i);
	    }
	}
	mus_error(MUS_AUDIO_CANT_OPEN,"%s: ran out of handle storage space", __FUNCTION__);
    close_and_exit:
	if ((err=snd_pcm_close(handle))!=0) {
	    mus_error(MUS_AUDIO_CANT_CLOSE, "%s: snd_pcm_close; %s", __FUNCTION__, snd_strerror(err));
	}
    }
    return(-1);
}
#endif

/* sndlib support for opening output devices */

#if SND_LIB_VERSION < ((0<<16)|(6<<8)|(0))
static int alsa_mus_audio_open_output(int ur_dev, int srate, int chans, int format, int size)
{
    int alsa_format,card,dev;
    snd_pcm_channel_params_t params;
    struct snd_pcm_channel_setup setup;
    int id;
    int err;

    if (trace_calls) mus_error(MUS_AUDIO_NO_ERROR, "%s: %x rate=%d, chans=%d, format=%d:%s, size=%d\n", 
			       __FUNCTION__, ur_dev, srate, chans, format, decode_mus_format(format), size);
    AUDIO_ERROR=MUS_AUDIO_NO_ERROR;
    card=MUS_AUDIO_SYSTEM(ur_dev);
    dev=MUS_AUDIO_DEVICE(ur_dev);
    if ((alsa_format=to_alsa_format(format))==-1) {
	mus_error(MUS_AUDIO_CANT_OPEN, "%s: could not change %s<%d> to alsa format", 
		  __FUNCTION__, decode_mus_format(format), format);
	AUDIO_ERROR=MUS_AUDIO_FORMAT_NOT_AVAILABLE;
	return(-1);
    }
    if ((id=alsa_audio_open(card, dev))<0) {
	AUDIO_ERROR=MUS_AUDIO_CANT_OPEN;
	return(-1);
    }
    memset(&params, 0, sizeof(params));
    params.channel=SND_PCM_CHANNEL_PLAYBACK;
    if ((handles[id].playback_device->info.flags) & SND_PCM_CHNINFO_BLOCK) 
        params.mode=SND_PCM_MODE_BLOCK;
    else {
	mus_error(MUS_AUDIO_CANT_OPEN, "%s: block mode not available for card %d, device %d",
		  __FUNCTION__, card, dev);
	AUDIO_ERROR=MUS_AUDIO_CONFIGURATION_NOT_AVAILABLE;
	return(-1);
    }
    params.start_mode=SND_PCM_START_FULL;
    params.stop_mode=stop_mode; 
    params.buf.block.frag_size=size;
    params.buf.block.frags_max=3;
    params.buf.block.frags_min=1;
    if ((handles[id].playback_device->info.flags) & SND_PCM_CHNINFO_INTERLEAVE) 
      params.format.interleave=1;
    if ((handles[id].playback_device->info.flags) & SND_PCM_CHNINFO_NONINTERLEAVE) 
      params.format.interleave=0;
    params.format.format=alsa_format;
    params.format.rate=srate;
    params.format.voices=chans;
    if ((err=alsa_params (handles[id].handle, &params))!=0) {
	mus_error(MUS_AUDIO_CANT_OPEN, "%s: alsa_params: %s", 
		  __FUNCTION__,snd_strerror(err));
	AUDIO_ERROR=MUS_AUDIO_CONFIGURATION_NOT_AVAILABLE;
	return(-1);
    }
    /* FIXME: we are not checking that the buffer size actually
     * allocated by alsa is the size we requested. It is possible
     * that the sizes don't match. Fixing this is not easy as we
     * don't have a way to return a new size to the sndlib caller. 
     * Maybe the sndlib api will have to include a call to check
     * that? Otherwise we'll need a second intermediate buffer
     * to accumulate samples on succesive calls to write (argh!)
     */
    if (trace_calls) {
	/* report real number of fragments allocated */
        int frags;
	memset(&setup, 0, sizeof(setup));
	setup.mode=SND_PCM_MODE_BLOCK;
	setup.channel=SND_PCM_CHANNEL_PLAYBACK;
	if ((err=alsa_setup(handles[id].handle, &setup))<0) {
	    mus_error(MUS_AUDIO_CANT_OPEN,"%s: alsa_setup: %s", 
		      __FUNCTION__,snd_strerror(err));
	} else {
 	    frags=setup.buf.block.frags;
	    mus_error(MUS_AUDIO_NO_ERROR,"%s: frags=%d, total size=%d\n", 
		      __FUNCTION__, frags, frags*size);
	}
    }
    if ((err=alsa_flush(handles[id].handle, SND_PCM_CHANNEL_PLAYBACK))<0) {
	mus_error(MUS_AUDIO_CANT_OPEN,"%s: alsa_flush: %s", 
		  __FUNCTION__,snd_strerror(err));
	AUDIO_ERROR=MUS_AUDIO_CANT_OPEN;
	return(-1);
    }
    if ((err=alsa_prepare(handles[id].handle, params.channel))!=0) {
	alsa_audio_close(id);
	mus_error(MUS_AUDIO_CANT_OPEN,"%s: alsa_prepare: %s", 
		  __FUNCTION__,snd_strerror(err));
	AUDIO_ERROR=MUS_AUDIO_CONFIGURATION_NOT_AVAILABLE;
	return(-1);
    }
    return(id);
}
#else
static int alsa_mus_audio_open_output(int ur_dev, int srate, int chans, int format, int size)
{
    int alsa_format,card,dev;
    snd_pcm_params_t params;
    struct snd_pcm_setup setup;
    int id;
    int err;
    int potential_format_width;

    if (trace_calls) mus_error(MUS_AUDIO_NO_ERROR, "%s: %x rate=%d, chans=%d, format=%d:%s, size=%d\n", 
			       __FUNCTION__, ur_dev, srate, chans, format, decode_mus_format(format), size);
    AUDIO_ERROR=MUS_AUDIO_NO_ERROR;
    card=MUS_AUDIO_SYSTEM(ur_dev);
    dev=MUS_AUDIO_DEVICE(ur_dev);
    if ((alsa_format=to_alsa_format(format))==-1) {
	mus_error(MUS_AUDIO_CANT_OPEN, "%s: could not change %s<%d> to alsa format", 
		  __FUNCTION__, decode_mus_format(format), format);
	AUDIO_ERROR=MUS_AUDIO_FORMAT_NOT_AVAILABLE;
	return(-1);
    }
    potential_format_width = snd_pcm_format_physical_width (alsa_format) / 8;
    if ((id=alsa_audio_open(card, dev))<0) {
	AUDIO_ERROR=MUS_AUDIO_CANT_OPEN;
	return(-1);
    }
    memset(&params, 0, sizeof(params));
    params.mode=SND_PCM_MODE_FRAGMENT;
    params.start_mode=SND_PCM_START_FULL;

    params.xrun_mode=stop_mode; 
    params.frag_size = (size /chans / potential_format_width);
    params.buffer_size = params.frag_size * 3;
    params.frames_xrun_max = ~0U;
    params.frames_min=params.frag_size;

    /* FIXME: we are assuming interleaved samples */
    params.format.interleave=1;
    params.format.format=alsa_format;
    params.format.rate=srate;
    params.format.channels=chans;

    if ((err=alsa_params (handles[id].playback_device->handle, &params))!=0) {
	mus_error(MUS_AUDIO_CANT_OPEN, "%s: alsa_params: %s", 
		  __FUNCTION__,snd_strerror(err));
	AUDIO_ERROR=MUS_AUDIO_CONFIGURATION_NOT_AVAILABLE;
	return(-1);
    }
    /* FIXME: we are not checking that the buffer size actually
     * allocated by alsa is the size we requested. It is possible
     * that the sizes don't match. Fixing this is not easy as we
     * don't have a way to return a new size to the sndlib caller. 
     * Maybe the sndlib api will have to include a call to check
     * that? Otherwise we'll need a second intermediate buffer
     * to accumulate samples on succesive calls to write (argh!)
     */
    memset(&setup, 0, sizeof(setup));
    if ((err=alsa_setup(handles[id].playback_device->handle, &setup))<0) {
	    mus_error(MUS_AUDIO_CANT_OPEN,"%s: alsa_setup: %s", 
		      __FUNCTION__,snd_strerror(err));
    } else {
	    handles[id].playback_device->
		    format_width = snd_pcm_format_physical_width (setup.format.format) / 8;
	    handles[id].playback_device->channels = setup.format.channels;
	    if (trace_calls) {
		    /* report real number of fragments allocated */
		    mus_error(MUS_AUDIO_NO_ERROR,"%s: frags=%d, total size=%d bytes\n", 
			      __FUNCTION__, setup.frags, 
			      setup.frags*setup.frag_size*handles[id].playback_device->format_width);
	    }
    }
    if ((err=alsa_flush(handles[id].playback_device->handle))<0) {
	mus_error(MUS_AUDIO_CANT_OPEN,"%s: alsa_flush: %s", 
		  __FUNCTION__,snd_strerror(err));
	AUDIO_ERROR=MUS_AUDIO_CANT_OPEN;
	return(-1);
    }
    if ((err=alsa_prepare(handles[id].playback_device->handle))!=0) {
	alsa_audio_close(id);
	mus_error(MUS_AUDIO_CANT_OPEN,"%s: alsa_prepare: %s", 
		  __FUNCTION__,snd_strerror(err));
	AUDIO_ERROR=MUS_AUDIO_CONFIGURATION_NOT_AVAILABLE;
	return(-1);
    }
    return(id);
}
#endif

/* sndlib support for opening input devices */

#if SND_LIB_VERSION < ((0<<16)|(6<<8)|(0))
static int alsa_mus_audio_open_input(int ur_dev, int srate, int chans, int format, int requested_size)
{
    int alsa_format, card, dev;
    snd_pcm_channel_params_t params;
    int id;
    int err;

    if (trace_calls) mus_error(MUS_AUDIO_NO_ERROR, "%s: %x rate=%d, chans=%d, format=%d:%s, size=%d\n", 
			       __FUNCTION__, ur_dev, srate, chans, 
			       format, decode_mus_format(format), requested_size);
    AUDIO_ERROR=MUS_AUDIO_NO_ERROR;
    card=MUS_AUDIO_SYSTEM(ur_dev);
    dev=MUS_AUDIO_DEVICE(ur_dev);
    if ((alsa_format=to_alsa_format(format))==-1) {
	mus_error(MUS_AUDIO_CANT_OPEN, "%s: could not change %s<%d> to alsa format", 
		  __FUNCTION__, decode_mus_format(format), format);
	AUDIO_ERROR=MUS_AUDIO_FORMAT_NOT_AVAILABLE;
	return(-1);
    }
    if ((id=alsa_audio_open(card, dev))<0) {
	AUDIO_ERROR=MUS_AUDIO_CANT_OPEN;
	return(-1);
    }
    memset(&params, 0, sizeof(params));
    params.channel=SND_PCM_CHANNEL_CAPTURE;
    if ((handles[id].capture_device->info.flags) & SND_PCM_CHNINFO_BLOCK) 
        params.mode=SND_PCM_MODE_BLOCK;
    else {
	mus_error(MUS_AUDIO_CANT_OPEN, "%s: block mode not available for card %d, device %d",
		  __FUNCTION__, card, dev);
	AUDIO_ERROR=MUS_AUDIO_CONFIGURATION_NOT_AVAILABLE;
	return(-1);
    }
    params.start_mode=SND_PCM_START_DATA;
    params.stop_mode=stop_mode;
    params.buf.block.frag_size=requested_size;
    params.buf.block.frags_max=-1;
    params.buf.block.frags_min=1;
    if ((handles[id].capture_device->info.flags) & SND_PCM_CHNINFO_INTERLEAVE) 
      params.format.interleave=1;
    if ((handles[id].capture_device->info.flags) & SND_PCM_CHNINFO_NONINTERLEAVE) 
      params.format.interleave=0;
    params.format.format=alsa_format;
    params.format.rate=srate;
    params.format.voices=chans;
    if ((err=alsa_params(handles[id].handle, &params))!=0) {
	mus_error(MUS_AUDIO_CANT_OPEN, "%s: alsa_params: %s", 
		  __FUNCTION__, snd_strerror(err));
	AUDIO_ERROR=MUS_AUDIO_CONFIGURATION_NOT_AVAILABLE;
	return(-1);
    }
    if ((err=alsa_prepare(handles[id].handle, params.channel))!=0) {
	alsa_audio_close(id);
	mus_error(MUS_AUDIO_CANT_OPEN, "%s: alsa_prepare; %s", 
		  __FUNCTION__, snd_strerror(err));
	AUDIO_ERROR=MUS_AUDIO_CONFIGURATION_NOT_AVAILABLE;
	return(-1);
    }
    if (trace_calls) {
        int frags;
	struct snd_pcm_channel_setup setup;
	memset(&setup, 0, sizeof(setup));
	setup.mode=SND_PCM_MODE_BLOCK;
	setup.channel=SND_PCM_CHANNEL_CAPTURE;
	if ((err=alsa_setup(handles[id].handle, &setup))<0) {
	    mus_error(MUS_AUDIO_CANT_OPEN, "%s: setup: %s", 
		      __FUNCTION__, snd_strerror(err));
	} else {
 	    frags=setup.buf.block.frags;
	    mus_error(MUS_AUDIO_NO_ERROR, "%s: frags=%d, total size=%d\n", 
		      __FUNCTION__, frags, frags*requested_size);
	}
    }
    return (id);
}
#else
static int alsa_mus_audio_open_input(int ur_dev, int srate, int chans, int format, int requested_size)
{
    int alsa_format, card, dev;
    snd_pcm_params_t params;
    struct snd_pcm_setup setup;
    int id;
    int err;
    int potential_format_width;

    if (trace_calls) mus_error(MUS_AUDIO_NO_ERROR, "%s: %x rate=%d, chans=%d, format=%d:%s, size=%d\n", 
			       __FUNCTION__, ur_dev, srate, chans, 
			       format, decode_mus_format(format), requested_size);
    AUDIO_ERROR=MUS_AUDIO_NO_ERROR;
    card=MUS_AUDIO_SYSTEM(ur_dev);
    dev=MUS_AUDIO_DEVICE(ur_dev);
    if ((alsa_format=to_alsa_format(format))==-1) {
	mus_error(MUS_AUDIO_CANT_OPEN, "%s: could not change %s<%d> to alsa format", 
		  __FUNCTION__, decode_mus_format(format), format);
	AUDIO_ERROR=MUS_AUDIO_FORMAT_NOT_AVAILABLE;
	return(-1);
    }
    potential_format_width = snd_pcm_format_physical_width (alsa_format) / 8;
    if ((id=alsa_audio_open(card, dev))<0) {
	AUDIO_ERROR=MUS_AUDIO_CANT_OPEN;
	return(-1);
    }
    memset(&params, 0, sizeof(params));
    params.mode=SND_PCM_MODE_FRAGMENT;
    params.start_mode=SND_PCM_START_DATA;

    params.xrun_mode=stop_mode; 
    params.frag_size = (requested_size / chans / potential_format_width);
    params.buffer_size = params.frag_size * 3;
    params.frames_xrun_max = ~0U;
    params.frames_min=params.frag_size;

    params.format.interleave=1;
    params.format.format=alsa_format;
    params.format.rate=srate;
    params.format.channels=chans;
    if ((err=alsa_params(handles[id].capture_device->handle, &params))!=0) {
	mus_error(MUS_AUDIO_CANT_OPEN, "%s: alsa_params: %s", 
		  __FUNCTION__, snd_strerror(err));
	AUDIO_ERROR=MUS_AUDIO_CONFIGURATION_NOT_AVAILABLE;
	return(-1);
    }
    if ((err=alsa_prepare(handles[id].capture_device->handle))!=0) {
	alsa_audio_close(id);
	mus_error(MUS_AUDIO_CANT_OPEN, "%s: alsa_prepare; %s", 
		  __FUNCTION__, snd_strerror(err));
	AUDIO_ERROR=MUS_AUDIO_CONFIGURATION_NOT_AVAILABLE;
	return(-1);
    }
    memset(&setup, 0, sizeof(setup));
    if ((err=alsa_setup(handles[id].capture_device->handle, &setup))<0) {
	    mus_error(MUS_AUDIO_CANT_OPEN, "%s: setup: %s", 
		      __FUNCTION__, snd_strerror(err));
    } else {
	    handles[id].capture_device->
		    format_width = snd_pcm_format_physical_width (setup.format.format) / 8;
	    handles[id].capture_device->channels = setup.format.channels;
	    if (trace_calls) {
		    mus_error(MUS_AUDIO_NO_ERROR, "%s: frags=%d, total size=%d bytes\n", 
			      __FUNCTION__, setup.frags, 
			      setup.frags*requested_size*handles[id].capture_device->format_width);
	    }
    }
    return (id);
}
#endif

/* sndlib support for writing to an opened device */

static int underruns = 0;

#if SND_LIB_VERSION < ((0<<16)|(6<<8)|(0))
static int alsa_mus_audio_write(int id, char *buf, int bytes)
{
    int done, err;
    snd_pcm_channel_status_t status;

    AUDIO_ERROR=MUS_AUDIO_NO_ERROR;
    if ((done=alsa_write(handles[id].handle, buf, bytes))!=bytes) {
        /* check status to recover from and report underruns */
        memset(&status, 0, sizeof(status));
	status.channel=SND_PCM_CHANNEL_PLAYBACK;
	if ((err=alsa_status(handles[id].handle, &status))<0) {
	    mus_error(MUS_AUDIO_CANT_WRITE, "%s: status: %s", 
		      __FUNCTION__, snd_strerror(err));
	} else {
	    if (stop_mode!=SND_PCM_STOP_ROLLOVER) {
	        if (status.status==SND_PCM_STATUS_UNDERRUN) {
		    if ((err=alsa_prepare(handles[id].handle, SND_PCM_CHANNEL_PLAYBACK))<0) {
		        mus_error(MUS_AUDIO_CANT_WRITE, "%s: prepare: %s", 
				  __FUNCTION__, snd_strerror(err));
		    }
		}
	    }
 	    if (status.underrun!=0) {
	        /* I'm sure this WILL catch all underruns */
 	        underruns+=status.underrun;
		if (trace_calls) mus_error(MUS_AUDIO_CANT_WRITE, "%s: %d underruns [%d]", 
					   __FUNCTION__, underruns, status.status);
	    }
	}
    }
    return(0);
}
#else
static int alsa_mus_audio_write(int id, char *buf, int bytes)
{
    int done;
    snd_pcm_status_t status;
    int frames;
    device_info_t *devinfo = handles[id].playback_device;

    AUDIO_ERROR=MUS_AUDIO_NO_ERROR;
    frames = bytes / devinfo->channels / devinfo->format_width;
    if ((done=alsa_write(handles[id].playback_device->handle, buf, frames))!=frames) {
	if (done<0) {
	    mus_error(MUS_AUDIO_CANT_WRITE, "%s[%d bytes=%d]: %s <%d>", 
		      __FUNCTION__, id, bytes, snd_strerror(done), done);
	} else {
	    mus_error(MUS_AUDIO_CANT_WRITE, "%s: wrote only %d out of %d bytes", 
		      __FUNCTION__,done, bytes);
	}
	AUDIO_ERROR=MUS_AUDIO_WRITE_ERROR;
	return(-1);
    }
    /* check status to recover from and report underruns */
    memset(&status, 0, sizeof(status));
    if ((done=alsa_status(handles[id].playback_device->handle, &status))<0) {
	mus_error(MUS_AUDIO_CANT_WRITE, "%s: status: %s", 
		  __FUNCTION__, snd_strerror(done));
    } else {
	if (stop_mode!=SND_PCM_XRUN_FLUSH) {
	    if (status.state==SND_PCM_STATE_UNDERRUN) {
		if ((done=alsa_prepare(handles[id].playback_device->handle))<0) {
		    mus_error(MUS_AUDIO_CANT_WRITE, "%s: prepare: %s", 
			      __FUNCTION__, snd_strerror(done));
		}
	    }
	}
	if (status.xruns!=0) {
	    /* I'm sure this WILL catch all underruns */
	    underruns+=status.xruns;
	    if (trace_calls) mus_error(MUS_AUDIO_CANT_WRITE, "%s: %d underruns [%d]", 
				       __FUNCTION__, underruns, status.state);
	}
    }
    return(0);
}
#endif

/* read a buffer from an input device */
#if SND_LIB_VERSION < ((0<<16)|(6<<8)|(0))
static int alsa_mus_audio_read(int id, char *buf, int bytes)
{
  snd_pcm_channel_status_t status;
    int done, err;

    AUDIO_ERROR=MUS_AUDIO_NO_ERROR;
    if ((done=alsa_read(handles[id].handle, buf, bytes))<0) {
	if (done==-EPIPE) {
	    memset(&status, 0, sizeof(status));
	    status.channel=SND_PCM_CHANNEL_CAPTURE;
	    if ((err=alsa_status(handles[id].handle, &status))<0) {
		mus_error(MUS_AUDIO_CANT_READ, "%s: status: %s", 
			  __FUNCTION__, snd_strerror(err));
		AUDIO_ERROR=MUS_AUDIO_READ_ERROR;
		return(-1);
	    }
	    if (status.status==SND_PCM_STATUS_RUNNING) {
	        if (trace_calls)
		    mus_error(MUS_AUDIO_CANT_READ,"%s: driver is waiting for data", 
			      __FUNCTION__);
		return(0);
	    }
	    if (status.status==SND_PCM_STATUS_OVERRUN) {
		if (trace_calls) {
		    mus_error(MUS_AUDIO_NO_ERROR, "%s: overrun at position %u", 
			      __FUNCTION__,
			      status.scount);
		}
		if ((err=alsa_prepare(handles[id].handle, SND_PCM_CHANNEL_CAPTURE))<0) {
		    mus_error(MUS_AUDIO_CANT_READ,"%s: prepare: %s", 
			      __FUNCTION__, snd_strerror(err));
		    AUDIO_ERROR=MUS_AUDIO_READ_ERROR;
		    return(-1);
		}
		return(0);
	    }
	    mus_error(MUS_AUDIO_CANT_READ, "%s: %s <%d>", 
		      __FUNCTION__, snd_strerror(done), done);
	    AUDIO_ERROR=MUS_AUDIO_READ_ERROR;
	    return(-1);
	} else {
	    mus_error(MUS_AUDIO_CANT_READ, "%s[%d bytes=%d]: %s <%d>", 
		      __FUNCTION__, id, bytes, snd_strerror(done), done);
	}
    }
    return(0);
}
#else
static int alsa_mus_audio_read(int id, char *buf, int bytes)
{
    int done;
    int frames;
    device_info_t *devinfo = handles[id].capture_device;

    AUDIO_ERROR=MUS_AUDIO_NO_ERROR;
    frames = bytes / devinfo->channels / devinfo->format_width;
    if ((done=alsa_read(handles[id].capture_device->handle, buf, frames))<0) {
	if (done==-EPIPE) {
	    int err;
	    snd_pcm_status_t status;
	    memset(&status, 0, sizeof(status));
	    mus_error(MUS_AUDIO_CANT_READ, "%s[%d bytes=%d]: %s <%d>", 
		      __FUNCTION__, id, bytes, snd_strerror(done), done);
	    if ((err=alsa_status(handles[id].capture_device->handle, &status))<0) {
		mus_error(MUS_AUDIO_CANT_READ, "%s: alsa_status: %s", 
			  __FUNCTION__, snd_strerror(err));
		AUDIO_ERROR=MUS_AUDIO_READ_ERROR;
		return(-1);
	    }
	    if (status.state==SND_PCM_STATE_RUNNING) {
		mus_error(MUS_AUDIO_CANT_READ,"%s: driver is waiting for data", 
			  __FUNCTION__);
		return;
	    }
	    if (status.state==SND_PCM_STATE_OVERRUN) {
		if (trace_calls) {
		    mus_error(MUS_AUDIO_NO_ERROR, "%s: overrun at position %u", 
			      __FUNCTION__, 
			      status.frame_io * handles[id].capture_device->format_width);
		}
		if ((err=alsa_prepare(handles[id].capture_device->handle))<0) {
		    mus_error(MUS_AUDIO_CANT_READ,"%s: alsa_prepare: %s", 
			      __FUNCTION__, snd_strerror(err));
		    AUDIO_ERROR=MUS_AUDIO_READ_ERROR;
		    return(-1);
		}
	    }
	} else {
	    mus_error(MUS_AUDIO_CANT_READ, "%s[%d bytes=%d]: %s <%d>", 
		      __FUNCTION__, id, bytes, snd_strerror(done), done);
	}
    }
    return(0);
}
#endif

/* sndlib support for closing a device */

static int alsa_mus_audio_close(int id)
{
    int err=0;
    if (trace_calls) mus_error(MUS_AUDIO_NO_ERROR, "%s: %d\n", __FUNCTION__, id); 
    AUDIO_ERROR=MUS_AUDIO_NO_ERROR;
    if ((err=alsa_audio_close(id))!=0) {
	AUDIO_ERROR=MUS_AUDIO_CANT_CLOSE;
	return(-1);
    }
    return 0;
}

/* read state of the audio hardware */

static int alsa_mus_audio_mixer_read(int ur_dev, int field, int chan, float *val)
{
    int card;
    int device;
    int alsa_device;
    int alsa_stream;
    device_info_t *info;
    int i, f, err;
    int channels;

    AUDIO_ERROR=MUS_AUDIO_NO_ERROR;

    card=MUS_AUDIO_SYSTEM(ur_dev);
    device=MUS_AUDIO_DEVICE(ur_dev);
    if (trace_calls) mus_error(MUS_AUDIO_NO_ERROR, "%s: card=%d, dev=%s<%d>, field=%s<%d>, chan=%d\n",
			       __FUNCTION__, card, decode_mus_audio_device(device), device, 
			       decode_mus_audio_field(field), field, 
			       chan);
    /* for now do not implement mixer interface */
    if (device==MUS_AUDIO_MIXER) {
	val[0]=0;
	return(0);
    }
    /* MUS_AUDIO_PORT probes for devices and should not depend on the
     * device which was used in the ur_dev argument, we process this
     * before trying to map the device to an alsa device */

    /* FIXME: this appears to fail for devices that are configured to
     * load but are not found when the sound driver starts, even
     * though there are other devices nothing is returned */
    if (field==MUS_AUDIO_PORT) {
	int count=0;
	int dev;
#if SND_LIB_VERSION < ((0<<16)|(6<<8)|(0))
 	info=device_info[card];
#else
	info=device_info[card][SND_PCM_STREAM_PLAYBACK];
#endif
	while (info!=NULL) {
#if SND_LIB_VERSION < ((0<<16)|(6<<8)|(0))
	    dev=to_sndlib_device(info->device, info->channel);
#else
	    dev=to_sndlib_device(info->device, info->stream);
#endif
	    if (dev!=-1) {
		val[count+1]=(float)dev;
		if (count++>chan) break;
	    }
	    info=info->next;
	}
	val[0]=count;
	return(0);
    }
    /* map the mus device to an alsa device and channel */
    if ((err=to_alsa_device(device, &alsa_device, &alsa_stream))<0) {
        /* FIXME: snd-dac still probes some non-existing devices, specifically
	 * MUS_AUDIO_DAC_FILTER, do not report error till that's fixed */
        if (trace_calls) {
        mus_error(MUS_AUDIO_DEVICE_NOT_AVAILABLE, "%s: cannot translate device %s<%d> to alsa, field=%s<%d>",
		  __FUNCTION__, 
		  decode_mus_audio_device(device), device, 
		  decode_mus_audio_field(field), field);
	}
	AUDIO_ERROR=MUS_AUDIO_CANT_READ; 
	return(-1);
    }
    if (trace_calls) mus_error(MUS_AUDIO_NO_ERROR, "%s:         adev=%d, achan=%d\n",
			       __FUNCTION__, alsa_device, alsa_stream);
    switch (field) {
    case MUS_AUDIO_AMP: 
	/* amplitude value */
	val[0]=1.0;
	break;
    case MUS_AUDIO_SAMPLES_PER_CHANNEL: 
	/* samples per channel */
	if ((info=alsa_get_info(card, alsa_device, alsa_stream))==NULL) {
	    AUDIO_ERROR=MUS_AUDIO_CANT_READ;
	    return(-1);
	} else {
	    val[0]=samples_per_channel;
	    if (chan>1) {
	        val[1]=info->info.min_fragment_size; 
		val[2]=info->info.max_fragment_size; 
	    }
	}
	break;
    case MUS_AUDIO_CHANNEL: 
	/* number of channels */
	if ((info=alsa_get_info(card, alsa_device, alsa_stream))==NULL) {
	    AUDIO_ERROR=MUS_AUDIO_CANT_READ;
	    return(-1);
	} else {
#if SND_LIB_VERSION < ((0<<16)|(6<<8)|(0))
	    val[0]=info->info.max_voices; 
	    if (chan>1) {
	        val[1]=info->info.min_voices; 
		val[2]=info->info.max_voices; 
#else
	    val[0]=info->info.max_channels; 
	    if (chan>1) {
	        val[1]=info->info.min_channels; 
		val[2]=info->info.max_channels; 
#endif
	    }
	}
	break;
    case MUS_AUDIO_SRATE: 
	/* supported sample rates */
	if ((info=alsa_get_info(card, alsa_device, alsa_stream))==NULL) {
	    AUDIO_ERROR=MUS_AUDIO_CANT_READ;
	    return(-1);
	} else {
	    val[0]=44100;
	    if (chan>1) {
	        val[1]=info->info.min_rate; 
		val[2]=info->info.max_rate; 
		/* FIXME: this should be expanded to include all the
		 * rate information that alsa provides so that sndlib
		 * clients can use it to check for all available rates */
	    }
	}
	break;
    case MUS_AUDIO_FORMAT:
	/* supported formats */
	if ((info=alsa_get_info(card, alsa_device, alsa_stream))==NULL) {
	    AUDIO_ERROR=MUS_AUDIO_CANT_READ;
	    return(-1);
	} else {
	    for (i=0, f=1; i<32; i++) {
		if (info->info.formats & (1<<i)) {
		    if (f<chan && (to_mus_format(1<<i)!=-1)) {
			val[f++]=to_mus_format(1<<i);
			/* FIXME: what if chan is not big enough? We end
			 * up not reporting all formats with no error */
		    }
		}
	    }
	    val[0]=f-1;
	}
	break;                
    case MUS_AUDIO_DIRECTION: 
	/* direction of this device */
	if ((info=alsa_get_info(card, alsa_device, alsa_stream))==NULL) {
	    AUDIO_ERROR=MUS_AUDIO_CANT_READ;
	    return(-1);
	} else {
	    /* 0-->playback, 1-->capture */
	    val[0]=alsa_stream;
	}
	break;
    default: 
	AUDIO_ERROR=MUS_AUDIO_CANT_READ; 
	return(-1);
	break;
    }
    return(0);
}

static int alsa_mus_audio_mixer_write(int ur_dev, int field, int chan, float *val)
{
	AUDIO_ERROR=MUS_AUDIO_NO_ERROR;
	return(0);
}

static void alsa_mus_audio_save (void)
{
}

static void alsa_mus_audio_restore (void)
{
}

static void alsa_mus_audio_mixer_save(const char* file)
{
}

static void alsa_mus_audio_mixer_restore(const char* file)
{
}

#if SND_LIB_VERSION < ((0<<16)|(6<<8)|(0))
static void alsa_describe_audio_state_1(void)
{
    snd_ctl_t *handle;
    int card, err, dev, idx, i;
    unsigned int mask;
    struct snd_ctl_hw_info info;
    snd_pcm_info_t pcminfo;
    snd_pcm_channel_info_t strinfo;

    if (!strbuf) strbuf=(char *)CALLOC(STRBUF_SIZE,sizeof(char));
    mask=snd_cards_mask();
    if (!mask) {
	sprintf(strbuf, "no soundcards found...\n");
	pprint(strbuf);
	return;
    }
    for (card=0; card < SND_CARDS; card++) {
	if (!(mask & (1 << card)))
	    continue;
	if ((err=snd_ctl_open(&handle, card))<0) {
	    mus_error(MUS_AUDIO_CANT_OPEN," %s: snd_ctl_open[%d]: %s", 
		      __FUNCTION__, card, snd_strerror(err));
	    continue;
	}
	if ((err=snd_ctl_hw_info(handle, &info)) < 0) {
	    mus_error(MUS_AUDIO_CANT_OPEN, "%s: snd_ctl_hw_info: %s", 
		      __FUNCTION__, card, snd_strerror(err));
	    snd_ctl_close(handle);
	    continue;
	}
	for (dev=0; dev < info.pcmdevs; dev++) {
	    if ((err=snd_ctl_pcm_info(handle, dev, &pcminfo)) < 0) {
		mus_error(MUS_AUDIO_CANT_OPEN, "%s: snd_ctl_pcm_info[%d:%d]: %s", 
			  __FUNCTION__, card, dev, snd_strerror(err));
		continue;
	    }
	    sprintf(strbuf, "Device %s: %i [%s] / #%i: %s\n",
		    info.name, card+1, info.id, dev, pcminfo.name);
	    pprint(strbuf);
	    sprintf(strbuf, "  Directions: %s%s%s\n",
		    pcminfo.flags & SND_PCM_INFO_PLAYBACK ? "playback " : "",
		    pcminfo.flags & SND_PCM_INFO_CAPTURE ? "capture " : "",
		    pcminfo.flags & SND_PCM_INFO_DUPLEX ? "duplex " : "");
	    pprint(strbuf);
	    sprintf(strbuf, "  Playback subdevices: %i\n", pcminfo.playback + 1);
	    pprint(strbuf);
	    if (pcminfo.flags & SND_PCM_INFO_PLAYBACK) {
		/* get hardware information for playback device */
		snd_pcm_t *dhandle;
		snd_pcm_channel_info_t dev_info;
		snd_pcm_channel_params_t dev_params;
		if ((err=snd_pcm_open(&dhandle, card, dev, SND_PCM_OPEN_PLAYBACK))!=0) {
		    mus_error(MUS_AUDIO_CANT_OPEN, "%s: open[%d:%d]: %s", 
			      __FUNCTION__, card, dev, snd_strerror(err));
		} else {
		    memset(&dev_info, 0, sizeof(dev_info));
		    dev_info.channel=SND_PCM_CHANNEL_PLAYBACK;
		    if ((err=alsa_info(dhandle, &dev_info))!=0) {
			mus_error(MUS_AUDIO_CANT_OPEN, "%s: snd_pcm_plugin_info: %s", 
				  __FUNCTION__,snd_strerror(err));
		    } else {
			sprintf(strbuf, "  Playback: formats=%d rates=%d frag-size=%d-%d\n", 
				dev_info.formats, dev_info.rates, 
				dev_info.min_fragment_size, dev_info.max_fragment_size);
			pprint(strbuf);
			sprintf(strbuf, "            rate=%d-%d channels=%d-%d\n", 
				dev_info.min_rate, dev_info.max_rate, 
				dev_info.min_voices, dev_info.max_voices);
			pprint(strbuf);
			for (i=0; i<32; i++) {
			    if (dev_info.formats & (1<<i)) {
				sprintf(strbuf, "              format %s -> %s\n",
					decode_alsa_format(1<<i),
					decode_mus_format(to_mus_format(1<<i)));
				pprint(strbuf);
			    }
			}
			/* list associated mixer element - hum, not very useful info */
			if (snd_mixer_element_has_info(&(dev_info.mixer_eid))) {
			    sprintf(strbuf, "  Associated mixer element #%i: %i\n", 
				    dev_info.mixer_eid, dev_info.mixer_device);
			    pprint(strbuf);
			    sprintf(strbuf, "    name=%s index=%i type=%i\n", 
				    dev_info.mixer_eid.name, dev_info.mixer_eid.index,  dev_info.mixer_eid.type);
			    pprint(strbuf);
			}		    
		    }
		    snd_pcm_close(dhandle);
		}
	    }
	    if (pcminfo.flags & SND_PCM_INFO_PLAYBACK) {
		/* list available subdevices */
		for (idx=0; idx <= pcminfo.playback; idx++) {
		    memset(&strinfo, 0, sizeof(strinfo));
		    strinfo.channel=SND_PCM_CHANNEL_PLAYBACK;
		    if ((err=snd_ctl_pcm_channel_info(handle, dev, SND_PCM_CHANNEL_PLAYBACK, idx, &strinfo))<0) {
			mus_error(MUS_AUDIO_CANT_OPEN, "%s: snd_ctl_pcm_channel_info[%i]: %s", 
				  __FUNCTION__, card, snd_strerror(err));
		    } else {
			sprintf(strbuf, "  Playback subdevice #%i: %s\n", idx, strinfo.subname);
			pprint(strbuf);
		    }
		}
	    }

	    sprintf(strbuf, "  Capture subdevices: %i\n", pcminfo.capture + 1);
	    pprint(strbuf);
	    if (pcminfo.flags & SND_PCM_INFO_CAPTURE) {
		/* get hardware information for capture device */
		snd_pcm_t *dhandle;
		snd_pcm_channel_info_t dev_info;
		snd_pcm_channel_params_t dev_params;
		if ((err=snd_pcm_open(&dhandle, card, dev, SND_PCM_OPEN_CAPTURE))!=0) {
		    mus_error(MUS_AUDIO_CANT_OPEN,"%s: open[%d:%d]: %s", __FUNCTION__,card, dev, snd_strerror(err));
		} else {
		    memset(&dev_info, 0, sizeof(dev_info));
		    dev_info.channel=SND_PCM_CHANNEL_CAPTURE;
		    if ((err=alsa_info(dhandle, &dev_info))!=0) {
			mus_error(MUS_AUDIO_CANT_OPEN,"%s: snd_pcm_plugin_info: %s", __FUNCTION__,snd_strerror(err));
		    } else {
			sprintf(strbuf, "  Capture: formats=%d rates=%d frag-size=%d-%d\n", 
				dev_info.formats, dev_info.rates, 
				dev_info.min_fragment_size, dev_info.max_fragment_size);
			pprint(strbuf);
			sprintf(strbuf, "           rate=%d-%d channels=%d-%d\n", 
				dev_info.min_rate, dev_info.max_rate, 
				dev_info.min_voices, dev_info.max_voices);
			pprint(strbuf);
			for (i=0; i<32; i++) {
			    if (dev_info.formats & (1<<i)) {
				sprintf(strbuf, "             format %s -> %s\n",
					decode_alsa_format(1<<i),
					decode_mus_format(to_mus_format(1<<i)));
				pprint(strbuf);
			    }
			}
		    }
		    snd_pcm_close(dhandle);
		}
	    }
	    if (pcminfo.flags & SND_PCM_INFO_CAPTURE) {
		/* list available subdevices */
		for (idx=0; idx <= pcminfo.capture; idx++) {
		    memset(&strinfo, 0, sizeof(strinfo));
		    strinfo.channel=SND_PCM_CHANNEL_CAPTURE;
		    if ((err=snd_ctl_pcm_channel_info(handle, dev, SND_PCM_CHANNEL_CAPTURE, 0, &strinfo)) < 0) {
			mus_error(MUS_AUDIO_CANT_OPEN, "%s: snd_ctl_pcm_channel_info[%i]: %s", 
				  __FUNCTION__, card, snd_strerror(err));
		    } else {
			sprintf(strbuf, "  Capture subdevice #%i: %s\n", idx, strinfo.subname);
			pprint(strbuf);
		    }
		}
	    }
	}
	snd_ctl_close(handle);
    }
}

#else
static void alsa_describe_audio_state_1(void)
{
	/* its like ... why repeat all the hard work that we did in the
	   initialize routine ? 
	*/
	
	int i;
	for (i=0;i<SND_CARDS;i++) {
		/* look at device_info[i][PLAYBACK] and
		           device_info[i][CAPTURE]
		*/
	}
}
#endif
	   
#endif



/* -------------------------------- NEXT -------------------------------- */

#ifdef NEXT
#define AUDIO_OK

#include <sound/sound.h>
#include <sound/sounddriver.h>
#include <sound/snddriver_client.h>
#include <mach/mach.h>

#if 0
#include <stdlib.h>
#include <stdio.h>
#include <fcntl.h>
#endif

static int low_water = 48*1024;
static int high_water = 64*1024;
static port_t dev_port = 0;
static port_t owner_port = 0;
static port_t write_port = 0;
static port_t read_port = 0;
static port_t reply_port = 0;
static short *rtbuf = NULL;
static int rtbuf_size = 0;
static char *readbuf;
#define WRITE_TAG 1
#define READ_TAG 1
#define BYTES_PER_SAMPLE 2
#define WRITE_COMPLETED_MSG 1
#define OUTPUT_LINE 1
#define INPUT_LINE 2

static int nxt_err = 0;
static char errstr[128];

char *mus_audio_error_name(int err) 
{
  if (!nxt_err) return(audio_error_name_1(err));
  sprintf(errstr,"%s: %s",audio_error_name_1(err),SNDSoundError(nxt_err));
  return(errstr);
}

int mus_audio_systems(void) {return(1);}
char *mus_audio_system_name(int system) {return("NeXT");}
char *mus_audio_moniker(void) {return("NeXT audio");}

static int waiting = 0;

static void write_completed(void *arg, int tag) 
{
  if (tag == WRITE_TAG) waiting--;
}

int mus_audio_initialize(void) {AUDIO_ERROR = MUS_AUDIO_NO_ERROR; return(0);}
static int output_chans = 0;
static snddriver_handlers_t replyHandlers;
static msg_header_t *reply_msg;

int mus_audio_open_output(int ur_dev, int srate, int chans, int format, int size)
{
  int sr,i,dev;
  int protocol = 0;
  AUDIO_ERROR = MUS_AUDIO_NO_ERROR; nxt_err = 0;
  dev = MUS_AUDIO_DEVICE(ur_dev);
  if (format != MUS_COMPATIBLE_FORMAT) {AUDIO_ERROR = MUS_AUDIO_FORMAT_NOT_AVAILABLE; return(-1);}
  if (size == 0) size = 1024;
  /* large buffers (4096) can cause bus errors! */
  if (rtbuf == NULL) 
    {
      rtbuf = (short *)CALLOC(size,sizeof(short));
      rtbuf_size = size;
    }
  else 
    {
      if (size > rtbuf_size)
        {
          FREE(rtbuf);
          rtbuf = (short *)CALLOC(size,sizeof(short));
          rtbuf_size = size;
        }
      for (i=0;i<size;i++) rtbuf[i] = 0;
    }
  nxt_err = SNDAcquire(SND_ACCESS_OUT,0,0,0,NULL_NEGOTIATION_FUN,0,&dev_port,&owner_port); 
  if (nxt_err == 0)
    {
      if (srate > 30000) sr = SNDDRIVER_STREAM_TO_SNDOUT_44; else sr = SNDDRIVER_STREAM_TO_SNDOUT_22;
      nxt_err = snddriver_stream_setup(dev_port,owner_port,sr,size,BYTES_PER_SAMPLE,low_water,high_water,&protocol,&write_port);
      output_chans = chans;
      waiting = 0;
      if (nxt_err == 0)
        {
          reply_msg = (msg_header_t *)malloc(MSG_SIZE_MAX);
          nxt_err = port_allocate(task_self(),&reply_port); 
          if (nxt_err) {AUDIO_ERROR = MUS_AUDIO_CANT_OPEN; return(-1);}
          reply_msg->msg_size = MSG_SIZE_MAX;
          reply_msg->msg_local_port = reply_port;
          replyHandlers.completed = write_completed;
          for (sr=0;sr<4;sr++)
            {
              nxt_err = snddriver_stream_start_writing(write_port,rtbuf,size,WRITE_TAG,0,0,0,WRITE_COMPLETED_MSG,0,0,0,0,reply_port);
              if (nxt_err) {AUDIO_ERROR = MUS_AUDIO_CANT_WRITE; return(-1);}
              waiting++;
            }
        }
    }
  if (nxt_err) {AUDIO_ERROR = MUS_AUDIO_CANT_OPEN; return(-1);}
  return(OUTPUT_LINE);
}

int mus_audio_write(int line, char *buf, int bytes)
{
  int i,j,samps,err;
#ifdef MUS_LITTLE_ENDIAN
  char tmp;
#endif
  short *sbuf;
  AUDIO_ERROR = MUS_AUDIO_NO_ERROR; nxt_err = 0;
  if (line != OUTPUT_LINE) {AUDIO_ERROR = MUS_AUDIO_CANT_WRITE; return(-1);}
  sbuf = (short *)buf;
  samps = (bytes>>1);
#ifdef MUS_LITTLE_ENDIAN
  /* "sound" format in NextStep is always big-endian */
  for (i=0;i<bytes;i+=2)
    {
      tmp = buf[i+1];
      buf[i+1] = buf[i];
      buf[i] = tmp;
    }
#endif
  reply_msg->msg_size = MSG_SIZE_MAX;
  reply_msg->msg_local_port = reply_port;
  if (output_chans == 2)
    {
      nxt_err = snddriver_stream_start_writing(write_port,sbuf,samps,WRITE_TAG,0,0,0,WRITE_COMPLETED_MSG,0,0,0,0,reply_port); 
      if (nxt_err) {AUDIO_ERROR = MUS_AUDIO_CANT_WRITE; return(-1);}
      waiting++;
      nxt_err = msg_receive(reply_msg,RCV_TIMEOUT,0);
      if (nxt_err == RCV_SUCCESS) nxt_err = snddriver_reply_handler(reply_msg,&replyHandlers);
      if (nxt_err) {AUDIO_ERROR = MUS_AUDIO_CANT_WRITE; return(-1);}
    }
  else
    {
      /* since output is always stereo on the Next, we have to fake up the second channel in the 1-channel case */
      j = 0;
      for (i=0;i<samps;i++)
        {
          rtbuf[j++] = sbuf[i];
          rtbuf[j++] = sbuf[i];
          if (j == rtbuf_size)
            {
              j = 0;
              nxt_err = snddriver_stream_start_writing(write_port,rtbuf,rtbuf_size,WRITE_TAG,0,0,0,WRITE_COMPLETED_MSG,0,0,0,0,reply_port); 
              if (nxt_err) {AUDIO_ERROR = MUS_AUDIO_CANT_WRITE; return(-1);}
              waiting++;
              nxt_err = msg_receive(reply_msg,RCV_TIMEOUT,0);
              if (nxt_err == RCV_SUCCESS) nxt_err = snddriver_reply_handler(reply_msg,&replyHandlers);
              if (nxt_err) {AUDIO_ERROR = MUS_AUDIO_CANT_WRITE; return(-1);}
            }
        }
      if (j > 0)
        {
          nxt_err = snddriver_stream_start_writing(write_port,rtbuf,j,WRITE_TAG,0,0,0,WRITE_COMPLETED_MSG,0,0,0,0,reply_port); 
          if (nxt_err) {AUDIO_ERROR = MUS_AUDIO_CANT_WRITE; return(-1);}
          waiting++;
          nxt_err = msg_receive(reply_msg,RCV_TIMEOUT,0);
          if (nxt_err == RCV_SUCCESS) nxt_err = snddriver_reply_handler(reply_msg,&replyHandlers);
          if (nxt_err) {AUDIO_ERROR = MUS_AUDIO_CANT_WRITE; return(-1);}
        }
    }
  return(0);
}

int mus_audio_close(int line)
{
  int ctr,oldwait;
  ctr = 0;
  AUDIO_ERROR = MUS_AUDIO_NO_ERROR; nxt_err = 0;
  if (dev_port) 
    {
      if (line == OUTPUT_LINE)
          {
            while (waiting)
              {
                oldwait = waiting;
                nxt_err = msg_receive(reply_msg,RCV_TIMEOUT,1);
                if (nxt_err == RCV_SUCCESS) 
                  nxt_err = snddriver_reply_handler(reply_msg,&replyHandlers);
                else if (nxt_err == RCV_TIMEOUT) break;
                if (nxt_err) {AUDIO_ERROR = MUS_AUDIO_CANT_CLOSE; return(-1);}
                if (oldwait == waiting) ctr++; else ctr=0;
                /* messages seem to be dropped at random on the Next, so we need a fail-safe way to break out of this loop */
                if (ctr > 1000) break;
              }
            if (reply_msg) FREE(reply_msg);
            nxt_err = SNDRelease(SND_ACCESS_OUT,dev_port,owner_port);
            if (nxt_err) {AUDIO_ERROR = MUS_AUDIO_CANT_CLOSE; return(-1);}
            port_deallocate(task_self(),reply_port);
          }
      else
        {
          if (line == INPUT_LINE)
            {
              if (reply_msg) FREE(reply_msg);
              nxt_err = SNDRelease(SND_ACCESS_IN,dev_port,owner_port);
              if (nxt_err) {AUDIO_ERROR = MUS_AUDIO_CANT_CLOSE; return(-1);}
              port_deallocate(task_self(),reply_port);
            }
          else {AUDIO_ERROR = MUS_AUDIO_CANT_CLOSE; return(-1);}
        }
    }
  if (nxt_err) {AUDIO_ERROR = MUS_AUDIO_CANT_CLOSE; return(-1);}
  return(0);
}


/* here we'll block if data not ready for read */
/* also since it's the next built-in microphone, we get mulaw data at 8KHz */

static void read_completed(void *arg, int tag, void *inp, int bytes)
{
  int i;
  if (tag == READ_TAG)
    {
      for (i=0;i<bytes;i++) readbuf[i] = ((char *)inp)[i];
    }
}

/* as far as I can tell, you only get one read from the NeXT, so it better be a monster...*/

int mus_audio_open_input(int ur_dev, int srate, int chans, int format, int size) 
{
  int protocol,dev;
  AUDIO_ERROR = MUS_AUDIO_NO_ERROR; nxt_err = 0;
  dev = MUS_AUDIO_DEVICE(ur_dev);
  if (format != MUS_MULAW) {AUDIO_ERROR = MUS_AUDIO_FORMAT_NOT_AVAILABLE; return(-1);}
  if ((srate != 8000) && (srate != 8012)) {AUDIO_ERROR = MUS_AUDIO_SRATE_NOT_AVAILABLE; return(-1);}
  if (size == 0) size = 1024;
  nxt_err = SNDAcquire(SND_ACCESS_IN,0,0,0,NULL_NEGOTIATION_FUN,0,&dev_port,&owner_port); 
  if (nxt_err == 0)
    {
      nxt_err = snddriver_stream_setup(dev_port,owner_port,SNDDRIVER_STREAM_FROM_SNDIN,size,1,low_water,high_water,&protocol,&read_port);
      if (nxt_err == 0)
        {
          reply_msg = (msg_header_t *)malloc(MSG_SIZE_MAX);
          reply_msg->msg_size = MSG_SIZE_MAX;
          nxt_err = port_allocate(task_self(),&reply_port); 
          if (nxt_err) {AUDIO_ERROR = MUS_AUDIO_CANT_OPEN; return(-1);}
          reply_msg->msg_local_port = reply_port;
          replyHandlers.recorded_data = read_completed;
          nxt_err = snddriver_stream_start_reading(read_port,0,size,READ_TAG,0,0,0,0,0,0,reply_port);
        }
    }
  if (nxt_err) {AUDIO_ERROR = MUS_AUDIO_CANT_OPEN; return(-1);}
  return(INPUT_LINE);
}

int mus_audio_read(int line, char *buf, int bytes) 
{
  AUDIO_ERROR = MUS_AUDIO_NO_ERROR; nxt_err = 0;
  if (line != INPUT_LINE) {AUDIO_ERROR = MUS_AUDIO_CANT_READ; return(-1);}
  reply_msg->msg_size = MSG_SIZE_MAX;
  reply_msg->msg_local_port = reply_port;
  nxt_err = msg_receive(reply_msg,MSG_OPTION_NONE,0);
  if (nxt_err == RCV_SUCCESS) 
    {
      readbuf = buf;
      nxt_err = snddriver_reply_handler(reply_msg,&replyHandlers);
    }
  if (nxt_err) {AUDIO_ERROR = MUS_AUDIO_CANT_READ; return(-1);}
  return(0);
}

#define MAX_VOLUME 0x2b

int mus_audio_mixer_read(int ur_dev, int field, int chan, float *val)
{
  int left,right,dev;
  int amps[2];
  AUDIO_ERROR = MUS_AUDIO_NO_ERROR; nxt_err = 0;
  dev = MUS_AUDIO_DEVICE(ur_dev);
  if ((dev == MUS_AUDIO_DAC_FILTER) && ((field == MUS_AUDIO_TREBLE) || (field == MUS_AUDIO_BASS)))
    {
      SNDGetFilter(&left);
      val[0] = left;
    }
  else
    {
      if ((dev == MUS_AUDIO_SPEAKERS) && (field == MUS_AUDIO_AMP))
        {
          SNDGetMute(&left);
          val[0] = left;
        }
      else
        {
          switch (field)
            {
            case MUS_AUDIO_PORT:  val[0] = 2; if (chan>1) val[1] = MUS_AUDIO_MICROPHONE; if (chan>2) val[2] = MUS_AUDIO_DAC_OUT; break;
            case MUS_AUDIO_FORMAT:  val[0] = 1; if (chan>1) val[1] = MUS_BSHORT; break;
            case MUS_AUDIO_SRATE:   val[0] = 44100; break;
            case MUS_AUDIO_CHANNEL: val[0] = 2; break;
            case MUS_AUDIO_AMP:
              if ((dev == MUS_AUDIO_DAC_OUT) || (dev == MUS_AUDIO_DEFAULT))
                {
                  SNDGetVolume(&left,&right);
                  /* lower level snddriver_get_device_parms doesn't work in NeXTStep/Intel */
                  if (chan == 0) 
                    val[0] = (float)left/(float)MAX_VOLUME;
                  else val[0] = (float)right/(float)MAX_VOLUME;
                }
              else AUDIO_ERROR = MUS_AUDIO_DEVICE_NOT_AVAILABLE;
              break;
            default: AUDIO_ERROR = MUS_AUDIO_CANT_READ; break;
            }
        }
    }
  if (AUDIO_ERROR != MUS_AUDIO_NO_ERROR) return(-1);
  return(0);
}

int mus_audio_mixer_write(int ur_dev, int field, int chan, float *val)
{
  int left,right,dev;
  AUDIO_ERROR = MUS_AUDIO_NO_ERROR; nxt_err = 0;
  dev = MUS_AUDIO_DEVICE(ur_dev);
  if ((dev == MUS_AUDIO_DAC_FILTER) && ((field == MUS_AUDIO_TREBLE) || (field == MUS_AUDIO_BASS)))
    {
      SNDSetFilter(val[0]);
    }
  else
    {
      if ((dev == MUS_AUDIO_SPEAKERS) && (field == MUS_AUDIO_AMP))
        {
          SNDSetMute(val[0]);
        }
      else
        {
          if (field != MUS_AUDIO_AMP)
            AUDIO_ERROR = MUS_AUDIO_WRITE_ERROR;
          else
            {
              if ((dev == MUS_AUDIO_DAC_OUT) || (dev == MUS_AUDIO_DEFAULT))
                {
                  SNDGetVolume(&left,&right);
                  if (chan == 0)
                    left = (int)(val[0] * MAX_VOLUME);
                  else right = (int)(val[0] * MAX_VOLUME);
                  SNDSetVolume(left,right);
                }
              else AUDIO_ERROR = MUS_AUDIO_CANT_WRITE;
            }
        }
    }
  if (AUDIO_ERROR != MUS_AUDIO_NO_ERROR) return(-1);
  return(0);
}

static void describe_audio_state_1(void)
{
  /* get volume -- not much else we can do here */
  float amps[1];
  float val;
  if (!strbuf) strbuf = (char *)CALLOC(1024,sizeof(char));
  mus_audio_mixer_read(MUS_AUDIO_DEFAULT,MUS_AUDIO_AMP,0,amps);
  val = amps[0];
  mus_audio_mixer_read(MUS_AUDIO_DEFAULT,MUS_AUDIO_AMP,1,amps);
  sprintf(strbuf,"speaker vols: %.2f %.2f\n",val,amps[0]);
  pprint(strbuf);
  mus_audio_mixer_read(MUS_AUDIO_DAC_FILTER,MUS_AUDIO_TREBLE,0,amps);
  pprint("dac lowpass: ");
  pprint((amps[0] == 0.0) ? "off\n" : "on\n");
  mus_audio_mixer_read(MUS_AUDIO_SPEAKERS,MUS_AUDIO_AMP,0,amps);
  pprint("speakers: ");
  pprint((amps[0] == 0) ? "off\n" : "on\n");
}

static int saved_left,saved_right,saved_mute,saved_filter;

void mus_audio_save (void) 
{
  SNDGetVolume(&saved_left,&saved_right);
  SNDGetMute(&saved_mute);
  SNDGetFilter(&saved_filter);
}

void mus_audio_restore (void) 
{
  SNDSetVolume(saved_left,saved_right);
  SNDSetMute(saved_mute);
  SNDSetFilter(saved_filter);
}

#endif



/* -------------------------------- SUN -------------------------------- */
/*
 * Thanks to Seppo Ingalsuo for several bugfixes.
 * record case improved after perusal of Snack 1.6/src/jkAudio_sun.c (but it's still not right)
 */

/* apparently input other than 8000 is 16-bit, 8000 is (?) mulaw */

#if (defined(SUN) || defined(OPENBSD)) && (!(defined(AUDIO_OK)))
#define AUDIO_OK

#include <sys/types.h>
#if 0
#include <unistd.h>
#include <fcntl.h>
#endif
#include <stropts.h>
#include <sys/filio.h>

#ifdef SUNOS
  #include <sun/audioio.h>
#else
  #include <sys/audioio.h>
#endif

int mus_audio_initialize(void) {AUDIO_ERROR = MUS_AUDIO_NO_ERROR; return(0);}
char *mus_audio_error_name(int err) {return(audio_error_name_1(err));}
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

static int error_exit(int error, int line)
{
  AUDIO_ERROR = error;
  if (line != -1) close(line);
  return(-1);
}

char *mus_audio_moniker(void) 
{
#ifndef AUDIO_DEV_AMD
  struct audio_device ad;
#else
  int ad;
#endif
  int audio_fd,err;
  char *dev_name;
  if (getenv(AUDIODEV_ENV) != NULL) dev_name = getenv(AUDIODEV_ENV); else dev_name = DAC_NAME;
  audio_fd = open(dev_name,O_RDONLY,0);
  if (audio_fd == -1) audio_fd = open("dev/audioctl",O_RDONLY | O_NONBLOCK,0);
  err = ioctl(audio_fd,AUDIO_GETDEV,&ad); 
  if (err == -1) {fprintf(stderr,"%s[%d] %s: getdev failed",__FILE__,__LINE__,__FUNCTION__); error_exit(MUS_AUDIO_CANT_READ,audio_fd); return("sun");}
  mus_audio_close(audio_fd);
  if (version_name == NULL) version_name = (char *)CALLOC(32,sizeof(char));
#ifndef AUDIO_DEV_AMD
  sprintf(version_name,"audio: %s (%s)",ad.name,ad.version);
#else
  switch (ad)
    {
    case AUDIO_DEV_AMD: sprintf(version_name,"audio: amd"); break;
  #ifdef AUDIO_DEV_CS4231
    case AUDIO_DEV_CS4231: sprintf(version_name,"audio: cs4231"); break;
  #endif
    case AUDIO_DEV_SPEAKERBOX: sprintf(version_name,"audio: speakerbox"); break;
    case AUDIO_DEV_CODEC: sprintf(version_name,"audio: codec"); break;
    default: sprintf(version_name,"audio: unknown"); break;
    }
#endif
  return(version_name);
}

static int to_sun_format(int format)
{
  switch (format)
    {
    case MUS_BSHORT: 
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
    case MUS_ALAW: return(AUDIO_ENCODING_ALAW); break;
      /* there's also AUDIO_ENCODING_DVI */
    }
  return(-1);
}

int mus_audio_open_output(int ur_dev, int srate, int chans, int format, int size)
{
  struct audio_info info;
  char *dev_name;
  int encode,bits,dev;
  int audio_fd,err;
  dev = MUS_AUDIO_DEVICE(ur_dev);
  AUDIO_ERROR = MUS_AUDIO_NO_ERROR;
  errno = 0;
  encode = to_sun_format(format);
  if (encode == -1) return(error_exit(MUS_AUDIO_FORMAT_NOT_AVAILABLE,-1));
  if (getenv(AUDIODEV_ENV) != NULL) dev_name = getenv(AUDIODEV_ENV); else dev_name = DAC_NAME;
  if (dev != MUS_AUDIO_DUPLEX_DEFAULT)
    audio_fd = open(dev_name,O_WRONLY,0);
  else audio_fd = open(dev_name,O_RDWR,0);
  if (audio_fd == -1) 
    {
      if (errno == EBUSY) return(error_exit(MUS_AUDIO_OUTPUT_BUSY,-1));
      return(error_exit(MUS_AUDIO_CANT_OPEN,-1));
    }
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
  bits = 8 * mus_data_format_to_bytes_per_sample(format);
  info.play.precision = bits;
  info.play.encoding = encode;
  err = ioctl(audio_fd,AUDIO_SETINFO,&info); 
  if (err == -1) {fprintf(stderr,"%s[%d] %s: setinfo failed",__FILE__,__LINE__,__FUNCTION__); return(error_exit(MUS_AUDIO_CANT_WRITE,audio_fd));}
  if ((int)info.play.channels != chans) return(error_exit(MUS_AUDIO_CHANNELS_NOT_AVAILABLE,audio_fd));
  if (((int)info.play.precision != bits) || ((int)info.play.encoding != encode)) return(error_exit(MUS_AUDIO_FORMAT_NOT_AVAILABLE,audio_fd));
  /* man audio sez the play.buffer_size field is not currently supported */
  /* but since the default buffer size is 8180! we need ioctl(audio_fd,I_SETSIG,...) */
  ioctl(audio_fd,I_FLUSH,FLUSHR);
  return(audio_fd);
}

int mus_audio_write(int line, char *buf, int bytes)
{
  AUDIO_ERROR = MUS_AUDIO_NO_ERROR;
  write(line,buf,bytes);
  return(0);
}

int mus_audio_close(int line)
{
  AUDIO_ERROR = MUS_AUDIO_NO_ERROR;
  write(line,(char *)NULL,0);
  close(line);
  return(0);
}

int mus_audio_read(int line, char *buf, int bytes)
{
  int i,bytes_read,bytes_available,total=0;
  char *curbuf;
  AUDIO_ERROR = MUS_AUDIO_NO_ERROR;
  /* ioctl(line,AUDIO_DRAIN,NULL) */
  /* this seems to return 8-12 bytes fewer than requested -- perverse! */
  /* should I buffer data internally? */

  /* apparently we need to loop here ... */
  curbuf = buf;
  while (total < bytes)
    {
      ioctl(line,FIONREAD,&bytes_available);
      if (bytes_available > 0)
	{
	  if ((total+bytes_available) > bytes) bytes_available = bytes-total;
	  bytes_read = read(line,curbuf,bytes_available);
	  total += bytes_read;
	  curbuf = (char *)(buf+total);
	}
    }
  return(0);
}

int mus_audio_open_input(int ur_dev, int srate, int chans, int format, int size)
{
  struct audio_info info;
  int indev,encode,bits,dev,audio_fd,err;
  char *dev_name;
  dev = MUS_AUDIO_DEVICE(ur_dev);
  AUDIO_ERROR = MUS_AUDIO_NO_ERROR;
  encode = to_sun_format(format);
  if (encode == -1) return(error_exit(MUS_AUDIO_FORMAT_NOT_AVAILABLE,-1));
  if (getenv(AUDIODEV_ENV) != NULL) dev_name = getenv(AUDIODEV_ENV); else dev_name = DAC_NAME;
  if (dev != MUS_AUDIO_DUPLEX_DEFAULT)
    audio_fd = open(dev_name,O_RDONLY,0);
  else audio_fd = open(dev_name,O_RDWR,0);
  if (audio_fd == -1) 
    {
      if (errno == EBUSY) return(error_exit(MUS_AUDIO_INPUT_BUSY,-1));
      return(error_exit(MUS_AUDIO_CANT_OPEN,-1));
    }
  AUDIO_INITINFO(&info);
  /*  ioctl(audio_fd,AUDIO_GETINFO,&info); */
  info.record.sample_rate = srate;
  info.record.channels = chans;
  err = ioctl(audio_fd, AUDIO_SETINFO, &info); 
  if (err == -1) {fprintf(stderr,"%s[%d] %s: setinfo failed",__FILE__,__LINE__,__FUNCTION__); }
  ioctl(audio_fd,AUDIO_GETINFO,&info);
  if (info.record.sample_rate != (unsigned int)srate) fprintf(stderr,"%s[%d]: sampling rate: %d != %d\n",__FILE__,__LINE__,info.record.sample_rate,srate);
  if (info.record.channels != (unsigned int)chans) fprintf(stderr,"%s[%d]: channels: %d != %d\n",__FILE__,__LINE__,info.record.channels,chans);

  bits = 8 * mus_data_format_to_bytes_per_sample(format);
  info.play.precision = bits;
  info.play.encoding = encode;
  err = ioctl(audio_fd, AUDIO_SETINFO, &info); 
  if (err == -1) fprintf(stderr,"%s[%d] %s: setinfo failed",__FILE__,__LINE__,__FUNCTION__);
  ioctl(audio_fd,AUDIO_GETINFO,&info);

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
  if (err == -1) {fprintf(stderr,"%s[%d] %s: setinfo failed",__FILE__,__LINE__,__FUNCTION__); return(error_exit(MUS_AUDIO_CANT_WRITE,audio_fd));}
  err = ioctl(audio_fd,AUDIO_GETINFO,&info);
  if (err == -1) {fprintf(stderr,"%s[%d] %s: getinfo failed",__FILE__,__LINE__,__FUNCTION__); return(error_exit(MUS_AUDIO_CANT_READ,audio_fd));}
  else 
    {
      if ((int)info.record.port != indev) return(error_exit(MUS_AUDIO_DEVICE_NOT_AVAILABLE,audio_fd));
      if ((int)info.record.channels != chans) return(error_exit(MUS_AUDIO_CHANNELS_NOT_AVAILABLE,audio_fd));
      if (((int)info.record.precision != bits) || ((int)info.record.encoding != encode)) 
	{
	  fprintf(stderr,"bits %d != %d, or encoding %d != %d\n",bits,info.record.precision,encode,info.record.encoding);
	  return(error_exit(MUS_AUDIO_FORMAT_NOT_AVAILABLE,audio_fd));
	}
    }
  /* this may be a bad idea */
  info.record.buffer_size = size;
  err = ioctl(audio_fd, AUDIO_SETINFO, &info); 
  if (err == -1) {fprintf(stderr,"%s[%d] %s: buffer size setinfo failed",__FILE__,__LINE__,__FUNCTION__); return(error_exit(MUS_AUDIO_CANT_WRITE,audio_fd));}
  return(audio_fd);
}

int mus_audio_mixer_read(int ur_dev, int field, int chan, float *val) 
{
#ifndef AUDIO_DEV_AMD
  struct audio_device ad;
#else 
  int ad;
#endif
  int audio_fd,err;
  struct audio_info info;
  int dev,port;
  char *dev_name;
  AUDIO_ERROR = MUS_AUDIO_NO_ERROR;
  dev = MUS_AUDIO_DEVICE(ur_dev);
  AUDIO_INITINFO(&info);
  if (getenv(AUDIODEV_ENV) != NULL) dev_name = getenv(AUDIODEV_ENV); else dev_name = DAC_NAME;
  audio_fd = open(dev_name,O_RDONLY | O_NONBLOCK,0);
  if (audio_fd == -1) 
    audio_fd = open("/dev/audioctl",O_RDONLY | O_NONBLOCK);
  if (audio_fd == -1)
    {
      AUDIO_ERROR = MUS_AUDIO_CANT_READ;
      return(-1);
    }
  err = ioctl(audio_fd, AUDIO_GETINFO, &info); 
  if (err == -1) {fprintf(stderr,"%s[%d] %s: getinfo failed",__FILE__,__LINE__,__FUNCTION__); return(error_exit(MUS_AUDIO_CANT_READ,audio_fd));}
  if (field == MUS_AUDIO_PORT)
    {
      /* info.play|record have a field avail_ports */
      port = 1;
      if ((chan > port) && (info.record.avail_ports & AUDIO_MICROPHONE)) {val[port] = MUS_AUDIO_MICROPHONE; port++;}
      if ((chan > port) && (info.record.avail_ports & AUDIO_LINE_IN)) {val[port] = MUS_AUDIO_LINE_IN; port++;}
#ifndef AUDIO_DEV_AMD
      if ((chan > port) && (info.record.avail_ports & AUDIO_INTERNAL_CD_IN)) 
	{
	  /* this field lies -- there is no such port available on the Ultra */
	  err = ioctl(audio_fd,AUDIO_GETDEV,&ad); 
	  if (err == -1) {fprintf(stderr,"%s[%d] %s: getdev failed",__FILE__,__LINE__,__FUNCTION__); return(error_exit(MUS_AUDIO_CANT_READ,audio_fd));}
	  if ((ad.version) && (strcmp(ad.version,"a") == 0)) /* is it a SparcStation? */
	    {
	      val[port] = MUS_AUDIO_CD; 
	      port++;
	    }
	}
#endif
      if ((chan > port) && (info.play.avail_ports & AUDIO_SPEAKER)) {val[port] = MUS_AUDIO_SPEAKERS; port++;}
      if ((chan > port) && (info.play.avail_ports & AUDIO_LINE_OUT)) {val[port] = MUS_AUDIO_LINE_OUT; port++;}
      if ((chan > port) && (info.play.avail_ports & AUDIO_HEADPHONE)) {val[port] = MUS_AUDIO_DAC_OUT; port++;}
      val[0] = port-1;
    }
  else
    {
      if (field == MUS_AUDIO_FORMAT)  /* this actually depends on the audio device */
        {
	  err = ioctl(audio_fd,AUDIO_GETDEV,&ad); /* SUNW,dbri|am79c30|CS4231|sbpro|sb16 */
	  if (err == -1) {fprintf(stderr,"%s[%d] %s: getdev failed",__FILE__,__LINE__,__FUNCTION__); return(error_exit(MUS_AUDIO_CANT_READ,audio_fd));}
	  port = 1;
#ifndef AUDIO_DEV_AMD
	  if ((ad.name) && (strcmp(ad.name,"SUNW,am79c30") != 0))
#else
	  if (ad == AUDIO_DEV_AMD)
#endif
	    {
	      if (chan>port) val[port] = MUS_BSHORT; port++;
	    }
#ifndef AUDIO_DEV_AMD
	  if ((ad.name) && (strcmp(ad.name,"SUNW,sbpro") != 0) && (strcmp(ad.name,"SUNW,sb16") != 0))
	    {
	      if (chan>port) val[port] = MUS_ALAW; port++;
	    }
#endif
          if (chan>port) val[port] = MUS_MULAW;
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
		      if (info.play.balance <= (AUDIO_RIGHT_BALANCE/2))
			val[0] = info.play.gain / (float)(AUDIO_MAX_GAIN);
		      else val[0] = info.play.gain * (AUDIO_RIGHT_BALANCE - info.play.balance) / (float)(AUDIO_MAX_GAIN * (AUDIO_RIGHT_BALANCE/2));
		    }
		  else
		    {
		      if (info.play.balance >= (AUDIO_RIGHT_BALANCE/2))
			val[0] = info.play.gain / (float)(AUDIO_MAX_GAIN);
		      else val[0] = info.play.gain * info.play.balance / (float)(AUDIO_MAX_GAIN * (AUDIO_RIGHT_BALANCE/2));
		    }
		  break;
                case MUS_AUDIO_CHANNEL: val[0] = 2; break;
		  /* appears to depend on data format (mulaw is mono) */
                case MUS_AUDIO_SRATE: val[0] = (float)info.play.sample_rate; break;
                default: AUDIO_ERROR = MUS_AUDIO_CANT_READ; break;
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
		      if (info.record.balance <= (AUDIO_RIGHT_BALANCE/2))
			val[0] = info.record.gain / (float)(AUDIO_MAX_GAIN);
		      else val[0] = info.record.gain * (AUDIO_RIGHT_BALANCE - info.record.balance) / (float)(AUDIO_MAX_GAIN * (AUDIO_RIGHT_BALANCE/2));
		    }
		  else
		    {
		      if (info.record.balance >= (AUDIO_RIGHT_BALANCE/2))
			val[0] = info.record.gain / (float)(AUDIO_MAX_GAIN);
		      else val[0] = info.record.gain * info.record.balance / (float)(AUDIO_MAX_GAIN * (AUDIO_RIGHT_BALANCE/2));
		    }
		  break;

                case MUS_AUDIO_CHANNEL: val[0] = 1; break;
                case MUS_AUDIO_SRATE: val[0] = (float)(info.record.sample_rate); break;
		case MUS_AUDIO_IGAIN: val[0] = (float)(info.monitor_gain)/(float)AUDIO_MAX_GAIN; break;
                default: AUDIO_ERROR = MUS_AUDIO_CANT_READ; break;
                }
              break;
            default: AUDIO_ERROR = MUS_AUDIO_CANT_READ; break;
            }
        }
    }
  mus_audio_close(audio_fd);
  if (AUDIO_ERROR != MUS_AUDIO_NO_ERROR) return(-1);
  return(0);
}

int mus_audio_mixer_write(int ur_dev, int field, int chan, float *val) 
{
  struct audio_info info;
  int dev,balance,gain,maxv;
  float ratio,lc,rc;
  int audio_fd,err;
  char *dev_name;
  AUDIO_ERROR = MUS_AUDIO_NO_ERROR;
  dev = MUS_AUDIO_DEVICE(ur_dev);
  AUDIO_INITINFO(&info);
  if (getenv(AUDIODEV_ENV) != NULL) dev_name = getenv(AUDIODEV_ENV); else dev_name = DAC_NAME;
  audio_fd = open(dev_name,O_RDWR | O_NONBLOCK,0);
  if (audio_fd == -1) audio_fd = open("/dev/audioctl",O_RDWR | O_NONBLOCK);
  if (audio_fd == -1) 
    {
      AUDIO_ERROR = MUS_AUDIO_CANT_WRITE;
      return(-1);
    }
  err = ioctl(audio_fd, AUDIO_GETINFO, &info); 
  if (err == -1) {fprintf(stderr,"%s[%d] %s: getinfo failed",__FILE__,__LINE__,__FUNCTION__); return(error_exit(MUS_AUDIO_CANT_READ,audio_fd));}
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
	  if (balance <= (AUDIO_RIGHT_BALANCE/2))
	    {
	      lc = gain;
	      rc = gain * balance / (float)(AUDIO_RIGHT_BALANCE/2);
	    }
	  else
	    {
	      lc = gain * (AUDIO_RIGHT_BALANCE - balance) / (float)(AUDIO_RIGHT_BALANCE/2);
	      rc = gain;
	    }
	  if (chan == 0)
	    lc = AUDIO_MAX_GAIN * val[0];
	  else rc = AUDIO_MAX_GAIN * val[0];
	  if ((rc+lc) == 0)
	    info.play.gain = 0;
	  else
	    {
	      ratio = (float)rc/(float)(rc+lc);
	      info.play.balance = AUDIO_RIGHT_BALANCE * ratio;
	      if (rc>lc) 
		info.play.gain = rc;
	      else info.play.gain = lc;
	    }
	  break;
        case MUS_AUDIO_CHANNEL: info.play.channels = (int)val[0]; break;
	  /* amd device only mono */
        case MUS_AUDIO_SRATE: info.play.sample_rate = (int)val[0]; break;
        default: AUDIO_ERROR = MUS_AUDIO_CANT_WRITE; break;
        }
      break;
    case MUS_AUDIO_MICROPHONE:
      switch (field)
	{
        case MUS_AUDIO_AMP: 
	  info.record.gain = AUDIO_MAX_GAIN * val[0];
	  info.record.balance = 0;
	  break;
        case MUS_AUDIO_CHANNEL: info.record.channels = 1; break;
        case MUS_AUDIO_SRATE: info.record.sample_rate = 8000; break;
	case MUS_AUDIO_IGAIN: info.monitor_gain = AUDIO_MAX_GAIN * val[0]; break;
        default: AUDIO_ERROR = MUS_AUDIO_CANT_WRITE; break;
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
	  gain = (rc + lc);
	  if (gain == 0)
	    info.record.gain = 0;
	  else
	    {
	      info.record.balance = AUDIO_RIGHT_BALANCE * ((float)rc / (float)(rc+lc));
	      if (rc > lc) 
		info.record.gain = rc;
	      else info.record.gain = lc;
	    }
	  break;
        case MUS_AUDIO_CHANNEL: info.record.channels = 1; break;
        case MUS_AUDIO_SRATE: info.record.sample_rate = (int)val[0]; break;
	case MUS_AUDIO_IGAIN: info.monitor_gain = AUDIO_MAX_GAIN * val[0]; break;
        default: AUDIO_ERROR = MUS_AUDIO_CANT_WRITE; break;
        }
      break;
    default: AUDIO_ERROR = MUS_AUDIO_CANT_WRITE; break;
    }
  err = ioctl(audio_fd,AUDIO_SETINFO,&info); 
  if (err == -1) {fprintf(stderr,"%s[%d] %s: setinfo failed",__FILE__,__LINE__,__FUNCTION__); return(error_exit(MUS_AUDIO_CANT_WRITE,audio_fd));}
  mus_audio_close(audio_fd);
  if (AUDIO_ERROR != MUS_AUDIO_NO_ERROR) return(-1);
  return(0);
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
  if (sun_vol_name == NULL) sun_vol_name = (char *)CALLOC(64,sizeof(char));
  if (chans != 2)
    sprintf(sun_vol_name,"%.3f",vol);
  else 
    {
      sprintf(sun_vol_name,"%.3f %.3f",
	      vol*(float)(AUDIO_RIGHT_BALANCE - balance)/(float)AUDIO_RIGHT_BALANCE,
	      vol*(float)balance/(float)AUDIO_RIGHT_BALANCE);
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
  int audio_fd,err;
  char *dev_name;
  AUDIO_INITINFO(&info);
  AUDIO_ERROR = MUS_AUDIO_NO_ERROR;
  if (!strbuf) strbuf = (char *)CALLOC(STRBUF_SIZE,sizeof(char));
  if (getenv(AUDIODEV_ENV) != NULL) dev_name = getenv(AUDIODEV_ENV); else dev_name = DAC_NAME;
  audio_fd = open(dev_name,O_RDONLY | O_NONBLOCK,0);
  if (audio_fd == -1)
    audio_fd = open("/dev/audioctl",O_RDONLY | O_NONBLOCK,0);
  if (audio_fd == -1)
    {
      AUDIO_ERROR = MUS_AUDIO_CANT_READ;
      return;
    }
  err = ioctl(audio_fd,AUDIO_GETINFO,&info); 
  if (err == -1) {fprintf(stderr,"%s[%d] %s: getinfo failed",__FILE__,__LINE__,__FUNCTION__); error_exit(MUS_AUDIO_CANT_READ,audio_fd); return;}
  err = ioctl(audio_fd,AUDIO_GETDEV,&ad); 
  if (err == -1) {fprintf(stderr,"%s[%d] %s: getdev failed",__FILE__,__LINE__,__FUNCTION__); error_exit(MUS_AUDIO_CANT_READ,audio_fd); return;}
  mus_audio_close(audio_fd);
#ifndef AUDIO_DEV_AMD
  sprintf(strbuf,"%s (version %s, configuration: %s)\n",ad.name,ad.version,ad.config);
#else
  sprintf(strbuf,"type: %d\n",ad);
#endif
  pprint(strbuf);
  sprintf(strbuf,"output: %s\n    srate: %d, vol: %s, chans: %d, format %d-bit %s\n",
	  sun_out_device_name(info.play.port),
	  info.play.sample_rate,
	  sun_volume_name((float)info.play.gain / (float)AUDIO_MAX_GAIN,info.play.balance,2),
	  info.play.channels,
	  info.play.precision,
	  sun_format_name(info.play.encoding));
  pprint(strbuf);
  sprintf(strbuf,"input: %s\n    srate: %d, vol: %s, chans: %d, format %d-bit %s\n",
	  sun_in_device_name(info.record.port),
	  info.record.sample_rate,
	  sun_volume_name((float)info.record.gain / (float)AUDIO_MAX_GAIN,info.record.balance,2),
	  info.record.channels,
	  info.record.precision,
	  sun_format_name(info.record.encoding));
  pprint(strbuf);
  sprintf(strbuf,"input->output vol: %.3f\n",(float)info.monitor_gain / (float)AUDIO_MAX_GAIN);
  pprint(strbuf);
  if (info.play.pause) {sprintf(strbuf,"Playback is paused\n"); pprint(strbuf);}
  if (info.record.pause) {sprintf(strbuf,"Recording is paused\n"); pprint(strbuf);}
  if (info.output_muted) {sprintf(strbuf,"Output is muted\n"); pprint(strbuf);}
}

static struct audio_info saved_info;

void mus_audio_save (void) 
{
  int audio_fd,err;
  audio_fd = open("/dev/audioctl",O_RDONLY | O_NONBLOCK,0);
  if (audio_fd != -1) 
    {
      err = ioctl(audio_fd,AUDIO_GETINFO,&saved_info); 
      if (err == -1) {fprintf(stderr,"%s[%d] %s: getinfo failed",__FILE__,__LINE__,__FUNCTION__); error_exit(MUS_AUDIO_CANT_READ,audio_fd); return;}
      close(audio_fd);
    }
}

void mus_audio_restore (void) 
{
  int audio_fd,err;
  audio_fd = open("/dev/audioctl",O_WRONLY | O_NONBLOCK,0);
  if (audio_fd != -1) 
    {
      err = ioctl(audio_fd,AUDIO_SETINFO,&saved_info); 
      if (err == -1) {fprintf(stderr,"%s[%d] %s: setinfo failed",__FILE__,__LINE__,__FUNCTION__); error_exit(MUS_AUDIO_CANT_READ,audio_fd); return;}
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

char *mus_audio_error_name(int err) {return(audio_error_name_1(err));}
int mus_audio_systems(void) {return(1);} /* if Audiomedia, multiple? */
char *mus_audio_system_name(int system) {return("Mac");}

static int available_input_devices(void)
{
  unsigned char *devname;
  OSErr err;
  int i;
  Handle h;
  devname = (unsigned char *)CALLOC(256,sizeof(char));
  for (i=1;i<16;i++)
    {
      err = SPBGetIndexedDevice(i,devname,&h);
      if (err != noErr) break;
    }
  FREE(devname);
  return(i-1);
}

static int input_device_is_connected(long refnum)
{
  OSErr err;
  short connected;
  err = SPBGetDeviceInfo(refnum,siDeviceConnected,&connected);
  return(connected);
}

static int input_device_get_source(long refnum)
{
  OSErr err;
  short source;
  err = SPBGetDeviceInfo(refnum,siInputSource,&source);
  return(source);
}

static int input_device_set_source(long refnum, short source)
{
  OSErr err;
  err = SPBSetDeviceInfo(refnum,siInputSource,&source);
  return((err == noErr) ? 0 : -1);
}

static int input_device_get_sources(long refnum, char **names)
{
  OSErr err;
  short sources;
  Handle h;
  err = SPBSetDeviceInfo(refnum,siInputSourceNames,&h);
  if (err == siUnknownInfoType) return(0);
  sources = (short)(*h);
  /* printf("%d sources: %s ",sources,strdup(p2cstr((unsigned char *)(*(h+2))))); */
  /* need an example to test this silly thing */
  return((err == noErr) ? sources : -1);
}

static int input_device_channels (long refnum)
{
  OSErr err;
  short chans;
  err = SPBGetDeviceInfo(refnum,siChannelAvailable,&chans);
  if (err == noErr) return(chans);
  return(-1);
}

static int input_device_get_async(long refnum)
{
  OSErr err;
  short async;
  err = SPBGetDeviceInfo(refnum,siAsync,&async);
  if (err == noErr) return(async);
  return(-1);
}

static char *input_device_name(long refnum)
{
  char *name;
  OSErr err;  
  name = (char *)CALLOC(256,sizeof(char));
  err = SPBGetDeviceInfo(refnum,siDeviceName,name);
  if (err == noErr) return(name);
  FREE(name);
  return(NULL);
}

static float input_device_get_gain(long refnum)
{
  OSErr err;
  unsigned long val;
  err = SPBGetDeviceInfo(refnum,siInputGain,&val);
  /* val is a "4 byte fixed value between .5 and 1.5"!! */
  if (err == noErr)
    return((float)val/65536.0);
  return(-1);
 }

static int input_device_set_gain(long refnum, float gain)
{
  OSErr err;
  int val;
  val = ((int)(gain*65536));
  err = SPBSetDeviceInfo(refnum,siInputGain,&val);
  return((err == noErr) ? 0 : -1); 
}

static int input_device_get_channels(long refnum)
{
  OSErr err;
  short chans;
  err = SPBGetDeviceInfo(refnum,siNumberChannels,&chans);
  return((err == noErr) ? chans : -1); 
}

static int input_device_set_channels(long refnum, short chans)
{
  OSErr err;
  err = SPBSetDeviceInfo(refnum,siNumberChannels,&chans);
  return((err == noErr) ? 0 : -1); 
}

static int input_device_get_quality(long refnum)
{
  OSErr err;
  OSType val;
  err = SPBGetDeviceInfo(refnum,siRecordingQuality,&val);
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
  err = SPBSetDeviceInfo(refnum,siRecordingQuality,&val);
  return((err == noErr) ? 0 : -1); 
}

static int input_device_get_srate(long refnum)
{
  OSErr err;
  unsigned long fixed_srate;
  err = SPBGetDeviceInfo(refnum,siSampleRate,&fixed_srate);
  if (err == noErr) return(fixed_srate>>16);
  return(-1);
}

static int input_device_set_srate(long refnum, int srate)
{
  OSErr err;
  unsigned long fixed_srate;
  fixed_srate = (unsigned long)(srate * 65536);
  err = SPBSetDeviceInfo(refnum,siSampleRate,&fixed_srate);
  return((err == noErr) ? 0 : -1); 
}

static int input_device_get_sample_size(long refnum)
{
  OSErr err;
  short size;
  err = SPBGetDeviceInfo(refnum,siSampleSize,&size);
  if (err == noErr) return(size);
  return(-1);
}

static int input_device_set_sample_size(long refnum, short size)
{
  OSErr err;
  err = SPBSetDeviceInfo(refnum,siSampleSize,&size);
  return((err == noErr) ? 0 : -1); 
}

static int input_device_get_signed(long refnum)
{ /* 0 = unsigned */
  OSErr err;
  short sign;
  err = SPBGetDeviceInfo(refnum,siSampleRate,&sign);
  if (err == noErr) return(sign);
  return(-1);
}

static int input_device_set_signed(long refnum, short sign)
{
  OSErr err;
  err = SPBSetDeviceInfo(refnum,siSampleRate,&sign);
  return((err == noErr) ? 0 : -1); 
}

static int input_device_sample_rates(long refnum, int *range, int *rates)
{
  unsigned short num;
  int i,j;
  unsigned long ptr;
  OSErr err;
  unsigned char pp[6];                                      /* can't depend on C compiler to pack a struct correctly here */
  num = 0;
  err = SPBGetDeviceInfo(refnum,siSampleRateAvailable,pp);
  if (err == noErr)
    {
      num = pp[1] + (pp[0]<<8);                             /* unsigned short is first element */
      if (num == 0) {(*range) = 1; num = 2;} else (*range) = 0;
      ptr = pp[5] + (pp[4]<<8) + (pp[3]<<16) + (pp[2]<<24); /* pointer to "fixed" table is second element */
      for (i=0,j=0;i<num;i++,j+=4)                          
        rates[i] = (*(unsigned short *)(j+(*(int *)ptr)));  /* ignore fraction -- this is dubious code */
    }
  return(num);
}

static int input_device_sample_sizes(long refnum, int *sizes)
{
  unsigned short num;
  int i,j;
  unsigned long ptr;
  OSErr err;
  unsigned char pp[6];
  num = 0;
  err = SPBGetDeviceInfo(refnum,siSampleSizeAvailable,pp);
  if (err == noErr)
    {
      num = pp[1] + (pp[0]<<8); 
      ptr = pp[5] + (pp[4]<<8) + (pp[3]<<16) + (pp[2]<<24);
      for (i=0,j=0;i<num;i++,j+=2) sizes[i] = (*(unsigned short *)(j+(*(int *)ptr)));
    }
  return(num);
}

static int input_device_get_gains(long refnum, float *gains)
{
  OSErr err;
  long ptr[2];
  err = SPBGetDeviceInfo(refnum,siStereoInputGain,ptr);
  if (err == noErr)
    {
      gains[0] = (float)ptr[0]/65536.0;
      gains[1] = (float)ptr[1]/65536.0;
    }
  else return(-1);
  return(0);
}

static int input_device_set_gains(long refnum, float *gains)
{
  OSErr err;
  long val[2];
  val[0] = gains[0]*65536;
  val[1] = gains[1]*65536;
  err = SPBSetDeviceInfo(refnum,siStereoInputGain,val);
  return((err == noErr) ? 0 : -1); 
}

char *mus_audio_moniker(void)
{
  NumVersion nv;
  if (version_name == NULL) version_name = (char *)CALLOC(32,sizeof(char));
  nv = SndSoundManagerVersion();
  sprintf(version_name,"Mac audio: %d.%d.%d.%d\n",nv.majorRev,nv.minorAndBugRev,nv.stage,nv.nonRelRev);
  return(version_name);
}

static char *quality_names[5] = {"indescribable","bad","not bad","ok","good"};

static void describe_audio_state_1(void) 
{
  long response;
  NumVersion nv;
  OSErr err;
  int vals[64];
  float gains[2];
  int have_IO_mgr = 0, have_input_device = 0;
  unsigned char *devname = NULL;
  int i,j,devs,rates,range,sizes,connected;
  long refnum;
  Handle h;
  if (!strbuf) strbuf = (char *)CALLOC(STRBUF_SIZE,sizeof(char));
  nv = SndSoundManagerVersion();
  sprintf(strbuf,"Sound Manager: %d.%d.%d.%d\n",nv.majorRev,nv.minorAndBugRev,nv.stage,nv.nonRelRev); pprint(strbuf);
  err = Gestalt(gestaltSoundAttr,&response);
  have_IO_mgr = (response & gestaltSoundIOMgrPresent);
  have_input_device = (response & gestaltHasSoundInputDevice);
  if (have_IO_mgr)
    {
      nv = SPBVersion();
      sprintf(strbuf,"Sound Input Manager: %d.%d.%d.%d\n",nv.majorRev,nv.minorAndBugRev,nv.stage,nv.nonRelRev); pprint(strbuf);
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
  sprintf(strbuf,"beep vol: %.3f %.3f\n",((float)(response>>16))/255.0,((float)(response&0xffff))/255.0); pprint(strbuf);
  GetDefaultOutputVolume(&response);
  sprintf(strbuf,"output vol: %.3f %.3f\n",((float)(response>>16))/255.0,((float)(response&0xffff))/255.0); pprint(strbuf);
  if ((have_IO_mgr) && (have_input_device))
    { 
      devs = available_input_devices();
      if (devs>0)
        {
          devname = (unsigned char *)CALLOC(256,sizeof(char));
          sprintf(strbuf,"input device%s:\n",(devs>1) ? "s" : ""); pprint(strbuf);
          for (i=1;i<=devs;i++)
            {
              for (i=1;i<=devs;i++)
                {
                  err = SPBGetIndexedDevice(i,devname,&h);
                  if (err == noErr)
                    {
                      err = SPBOpenDevice(devname,siWritePermission,&refnum);
                      if (err == noErr)
                        {
                          range = input_device_get_source(refnum);
                          connected = input_device_is_connected(refnum);
                          sprintf(strbuf,"  %s: %s%s",
                                  (*devname) ? devname : (unsigned char *)"un-named",
                                  ((input_device_get_async(refnum) == 1) ? "(async) " : ""),
                                  ((connected == siDeviceIsConnected) ? "" : 
                                   ((connected == siDeviceNotConnected) ? 
                                    "(not connected )" : "(might not be connected)")));
                        pprint(strbuf);
                        if (range == 0) pprint("\n");
                        else
                                {
                                sprintf(strbuf," (source: %d)\n",range);
                                pprint(strbuf);
                                }
                                sprintf(strbuf,"    %d chans available, %d active\n",
                                  input_device_channels(refnum),
                                  input_device_get_channels(refnum));
                          pprint(strbuf);

                          /* input_device_get_sources(refnum,NULL); */

                          range = 0;
                          rates = input_device_sample_rates(refnum,&range,vals);
                          if (rates>1)
                            {
                              sprintf(strbuf,"    srates available:"); 
                              pprint(strbuf);
                              if (range)
                                {sprintf(strbuf,"%d to %d",vals[0],vals[1]); pprint(strbuf);}
                              else
                                {for (j=0;j<rates;j++) {sprintf(strbuf," %d",vals[j]); pprint(strbuf);}}
                              sprintf(strbuf,", current srate: %d\n",
                                      input_device_get_srate(refnum));
                              pprint(strbuf);
                            }
                          else 
                            {
                              sprintf(strbuf,"    srate: %d\n",input_device_get_srate(refnum)); 
                              pprint(strbuf);
                            }
                          err = input_device_get_quality(refnum);
                          if (err != -1) 
                            {
                              sprintf(strbuf,"    quality: %s\n",quality_names[1+input_device_get_quality(refnum)]);
                              pprint(strbuf);
                            }
                          input_device_get_gains(refnum,gains);
                          sprintf(strbuf,"    gain: %.3f (%.3f %.3f)\n    sample: %s %d bits",
                                  input_device_get_gain(refnum),gains[0],gains[1],
                                  ((input_device_get_signed(refnum)) ? "signed" : "unsigned"),
                                  input_device_get_sample_size(refnum));
                          pprint(strbuf);
                          sizes = input_device_sample_sizes(refnum,vals);
                          if (sizes > 0)
                            {
                              sprintf(strbuf," (%d",vals[0]); pprint(strbuf);
                              for (j=1;j<sizes;j++) 
                                {sprintf(strbuf,", %d",vals[j]); pprint(strbuf);}
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

#ifdef CLM
#ifdef MCL_PPC
  static void reset_db(void) 
    {
      db=NULL; 
      db_state=NULL;
      data = NULL;
      data_bytes = 0;
    }
#endif
#endif

static pascal void nextbuffer(SndChannelPtr cp, SndDoubleBufferPtr db)
{
  db_state[current_buf] = BUFFER_EMPTY;
}

int mus_audio_open_output(int ur_dev, int srate, int chans, int format, int size) 
{
  OSErr err;
  int dev;
  AUDIO_ERROR = MUS_AUDIO_NO_ERROR;
  dev = MUS_AUDIO_DEVICE(ur_dev);
  if (!db) db = (SndDoubleBufferPtr *)CALLOC(2,sizeof(SndDoubleBufferPtr));
  if (!db_state) db_state = (int *)CALLOC(2,sizeof(int));
  chan = nil;
  err = SndNewChannel(&chan,sampledSynth,0,nil);
  if (err != noErr) {AUDIO_ERROR = MUS_AUDIO_DEVICE_NOT_AVAILABLE; return(-1);}
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
  db[0] = (SndDoubleBufferPtr)CALLOC(sizeof(SndDoubleBuffer) + buffer_size,sizeof(char));
  if ((db[0] == nil) || (MemError() != 0)) {AUDIO_ERROR = MUS_AUDIO_SIZE_NOT_AVAILABLE; SndDisposeChannel(chan,0); return(-1);}
  dh.dbhBufferPtr[0] = db[0];   
  db[0]->dbNumFrames = 0;
  db[0]->dbFlags = 0;
  db_state[0] = BUFFER_EMPTY;
  db[1] = (SndDoubleBufferPtr)CALLOC(sizeof(SndDoubleBuffer) + buffer_size,sizeof(char));
  if ((db[1] == nil) || (MemError() != 0)) {AUDIO_ERROR = MUS_AUDIO_SIZE_NOT_AVAILABLE; FREE(db[0]); SndDisposeChannel(chan,0); return(-1);}
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
  int i,j;
  OSErr err;
  err = noErr;
  for (i=instart,j=0;j<bytes;j++,i++) db[dbi]->dbSoundData[j] = inbuf[i];
  db_state[dbi] = BUFFER_FILLED;
  db[dbi]->dbFlags = (db[dbi]->dbFlags | dbBufferReady);
  db[dbi]->dbNumFrames = (bytes / (current_chans * current_datum_size));
  if ((sound_state == SOUND_INITIALIZED) && (dbi == 1))
    {
      sound_state = SOUND_RUNNING;
      err = SndPlayDoubleBuffer(chan,&dh);
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
      err = SndChannelStatus(chan,sizeof(Stats),&Stats);
      if ((err != noErr) || (!(Stats.scChannelBusy))) break;
    }
  return(err);
}

int mus_audio_write(int line, char *buf, int bytes) 
{
  OSErr err;
  int lim,leftover,start;
  if (line != OUTPUT_LINE) {AUDIO_ERROR = MUS_AUDIO_CANT_WRITE; return(-1);}
  AUDIO_ERROR = MUS_AUDIO_NO_ERROR;
  leftover = bytes;
  start = 0;
  while (leftover > 0)
    {
      lim = leftover;
      if (lim > buffer_size) lim = buffer_size;
      leftover -= lim;
      err = wait_for_empty_buffer(current_buf);
      if (err != noErr) {AUDIO_ERROR = MUS_AUDIO_CANT_WRITE; return(-1);}
      err = fill_buffer(current_buf,buf,start,lim);
      if (err != noErr) {AUDIO_ERROR = MUS_AUDIO_CANT_WRITE; return(-1);}
      start += lim;
      current_buf++;
      if (current_buf>1) current_buf=0;
    }
  return(0);
}

int mus_audio_close(int line) 
{
  OSErr err;
  int i;
  AUDIO_ERROR = MUS_AUDIO_NO_ERROR;
  if (line == OUTPUT_LINE)
    {
      /* fill with a few zeros, wait for empty flag */
      if (sound_state != SOUND_UNREADY)
        {
          wait_for_empty_buffer(current_buf);
          for (i=0;i<128;i++) db[current_buf]->dbSoundData[i] = 0;
          db[current_buf]->dbFlags = (db[current_buf]->dbFlags | dbBufferReady | dbLastBuffer);
          db[current_buf]->dbNumFrames = (128 / (current_chans * current_datum_size));
          wait_for_empty_buffer(current_buf);
          FREE(db[0]); db[0]=NULL;
          FREE(db[1]); db[1]=NULL;
          db_state[0] = BUFFER_EMPTY;
          db_state[1] = BUFFER_EMPTY;
          sound_state = SOUND_UNREADY;
          err = SndDisposeChannel(chan,0);
	  /* this is the line that forced me to use FREE/CALLOC throughout! */
          if (err != noErr) {AUDIO_ERROR = MUS_AUDIO_CANT_CLOSE; return(-1);}
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
      else {AUDIO_ERROR = MUS_AUDIO_CANT_CLOSE; return(-1);}
    }
  return(0);
}

static void read_callback(SPB *spb)
{
  int i,lim;
  if (data_status != DATA_EMPTY)
    {
      if (data_bytes > spb->bufferLength) lim=spb->bufferLength; else lim=data_bytes;
      for (i=0;i<lim;i++) data[i] = spb->bufferPtr[i]; 
      spb->bufferLength = data_bytes;
      SPBRecord(spb,TRUE);
      data_status = DATA_WRITTEN;
    }
}

int mus_audio_open_input(int ur_dev, int srate, int chans, int format, int size) 
{
  OSErr err;
  short source;
  int dev;
  AUDIO_ERROR = MUS_AUDIO_NO_ERROR;
  dev = MUS_AUDIO_DEVICE(ur_dev);
  data_status = DATA_EMPTY;
  if (size<=0) size = 1024;
  if (in_ref != -1) {AUDIO_ERROR = MUS_AUDIO_DEVICE_NOT_AVAILABLE; return(-1);}
  err = SPBOpenDevice((unsigned char *)"",siWritePermission,&in_ref);
  if (err != noErr) {AUDIO_ERROR = MUS_AUDIO_CANT_OPEN; return(-1);}
  spb.inRefNum = in_ref;
  spb.count = size;
  source = 3; /* the microphone ?? (2: CD, 4: modem, 0: none) -- nowhere is this documented! */
  input_device_set_source(in_ref,source);
  input_device_set_srate(in_ref,srate);
  input_device_set_channels(in_ref,(short)chans);
  input_device_set_sample_size(in_ref,(format == MUS_BSHORT) ? 2 : 1);
  input_device_set_signed(in_ref,(format == MUS_BSHORT) ? 1 : 0);
  spb.milliseconds = (int)((float)(size * 1000) / (float)(((format == MUS_BSHORT) ? 2 : 1) * srate));
  spb.bufferLength = size;
  spb.bufferPtr = (char *)CALLOC(size,sizeof(char));
  spb.completionRoutine = NewSICompletionProc(read_callback);
  err = SPBRecord(&spb,TRUE);
  return(INPUT_LINE);
}

int mus_audio_read(int line, char *buf, int bytes) 
{
  OSErr err;
  unsigned long total_samps,num_samps,total_msecs,num_msecs;
  short level,status;
  if (line != INPUT_LINE) {AUDIO_ERROR = MUS_AUDIO_CANT_READ; return(-1);}
  AUDIO_ERROR = MUS_AUDIO_NO_ERROR;
  data_status = DATA_READY;
  data_bytes = bytes;
  data = buf;
  while (data_status == DATA_READY)
    {
      err = SPBGetRecordingStatus(in_ref,&status,&level,&total_samps,&num_samps,&total_msecs,&num_msecs);
      if ((err != noErr) || (status <= 0)) break; /* not necessarily an error */
    }
  return(0);
}

int mus_audio_mixer_read(int ur_dev, int field, int chan, float *val) 
{
  OSErr err;
  long response;
  int dev;
  float in_val[2];
  int our_open = 0;
  AUDIO_ERROR = MUS_AUDIO_NO_ERROR;
  dev = MUS_AUDIO_DEVICE(ur_dev);
  switch (field)
    {
    case MUS_AUDIO_CHANNEL:
      val[0] = 2;
      break;
    case MUS_AUDIO_PORT:
      val[0] = 2; 
      if (chan>1) val[1] = MUS_AUDIO_MICROPHONE; 
      if (chan>2) val[2] = MUS_AUDIO_DAC_OUT;
      break;
    case MUS_AUDIO_FORMAT:
      val[0] = 2; 
      if (chan>1) val[1] = MUS_BSHORT;
      if (chan>2) val[2] = MUS_UBYTE;
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
                val[0] = ((float)(response>>16))/255.0;
              else val[0] = ((float)(response&0xffff))/255.0;
              break;
            case MUS_AUDIO_CHANNEL: val[0] = 2; break;
            case MUS_AUDIO_SRATE: val[0] = current_srate; break;
            }
          break;
        case MUS_AUDIO_MICROPHONE:
        case MUS_AUDIO_LINE_IN:
          if (in_ref == -1)
            {
              err = SPBOpenDevice((const unsigned char *)"",siWritePermission,&in_ref);
              if (err != noErr) {AUDIO_ERROR = MUS_AUDIO_CANT_OPEN; return(-1);}
              our_open = 1;
            }
          switch (field)
            {
            case MUS_AUDIO_AMP:
              err = input_device_get_gains(in_ref,in_val);
              if (chan == 0) val[0] = in_val[0]; else val[0] = in_val[1];
              break;
            case MUS_AUDIO_CHANNEL:
              err = input_device_get_channels(in_ref);
              break;
            case MUS_AUDIO_SRATE:
              err = input_device_get_srate(in_ref);
              break;
            default: AUDIO_ERROR = MUS_AUDIO_CANT_READ; break;
            }
          if (err == -1) 
            AUDIO_ERROR = MUS_AUDIO_CANT_READ;
          else val[0] = err;
          if (our_open)
            {
              SPBCloseDevice(in_ref);
              in_ref = -1;
            }
          break;
        default: AUDIO_ERROR = MUS_AUDIO_CANT_READ; break;
        }
    }
  if (AUDIO_ERROR != MUS_AUDIO_NO_ERROR) return(-1);
  return(0);
}

int mus_audio_mixer_write(int ur_dev, int field, int chan, float *val) 
{
  OSErr err;
  float out_val[2];
  long curval,newval;
  int amp,our_open,dev;
  AUDIO_ERROR = MUS_AUDIO_NO_ERROR;
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
        default: AUDIO_ERROR = MUS_AUDIO_CANT_WRITE; break;
        }
      break;
    case MUS_AUDIO_MICROPHONE:
    case MUS_AUDIO_LINE_IN:
      if (in_ref == -1)
        {
          err = SPBOpenDevice((const unsigned char *)"",siWritePermission,&in_ref);
          if (err != noErr) {AUDIO_ERROR = MUS_AUDIO_CANT_OPEN; return(-1);}
          our_open = 1;
        }
      switch (field)
        {
        case MUS_AUDIO_AMP:
          input_device_get_gains(in_ref,out_val);
          if (chan == 0) out_val[0] = val[0]; else out_val[1] = val[0];
          err = input_device_set_gains(in_ref,out_val);
          break;
        case MUS_AUDIO_CHANNEL:
          err = input_device_set_channels(in_ref,(int)val[0]);
          break;
        case MUS_AUDIO_SRATE:
          err = input_device_set_srate(in_ref,(int)val[0]);
          break;
        default: AUDIO_ERROR = MUS_AUDIO_CANT_WRITE; break;
        }
      if (err == -1) 
        AUDIO_ERROR = MUS_AUDIO_CANT_READ;
      if (our_open)
        {
          SPBCloseDevice(in_ref);
          in_ref = -1;
        }
      break;
    default: AUDIO_ERROR = MUS_AUDIO_CANT_WRITE; break;
    }
  if (AUDIO_ERROR != MUS_AUDIO_NO_ERROR) return(-1);
  return(0);
}

static long beep_vol = 0,out_vol = 0;

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
  AUDIO_ERROR = MUS_AUDIO_NO_ERROR;
  return(0);
}

#endif


/* ------------------------------- BEOS ----------------------------------------- */

/* rewritten 10-Apr-00 to reflect release 5 */

#ifdef BEOS
#define AUDIO_OK

#include <be/media/SoundPlayer.h>

char *mus_audio_error_name(int err) {return(audio_error_name_1(err));}
int mus_audio_systems(void) {return(1);}
char *mus_audio_system_name(int system) {return("Be");}
char *mus_audio_moniker(void) {return("Be audio");}

static void describe_audio_state_1(void) 
{
}
  
#define DATA_EMPTY 0
#define DATA_READY 1
#define DATA_WRITTEN 2
static int data_status;
static int data_bytes;
static float *data = NULL;

#define SOUND_UNREADY 0
#define SOUND_INITIALIZED 1
#define SOUND_RUNNING 2
static int sound_state = SOUND_UNREADY;

static BSoundPlayer player;

static void write_callback(void *ignored, void *buffer, size_t size, const media_raw_audio_format &format)
{
  int i,lim;
  float *buf = (float *)buffer;
  if (data_status != DATA_EMPTY)
    {
      if (data_bytes > size) lim=size/4; else lim=data_bytes/4;
      for (i=0;i<lim;i++) buf[i] += data[i];
      data_status = DATA_WRITTEN;
    }
}

int mus_audio_open_output(int ur_dev, int srate, int chans, int format, int size) 
{
  BSoundPlayer pl("hiho",write_callback,NULL,NULL);
  AUDIO_ERROR = MUS_AUDIO_NO_ERROR;
  player = pl;
  pl.Start();
  pl.SetHasData(true);
  data_status = DATA_EMPTY;
  sound_state = SOUND_INITIALIZED;
  return(1);
}

int mus_audio_open_input(int ur_dev, int srate, int chans, int format, int size) 
{
}

int mus_audio_write(int line, char *buf, int bytes) 
{
  size_t size;
  if (line != 1) {AUDIO_ERROR = MUS_AUDIO_CANT_WRITE; return(-1);}
  AUDIO_ERROR = MUS_AUDIO_NO_ERROR;
  if (sound_state == SOUND_UNREADY) {AUDIO_ERROR = MUS_AUDIO_CANT_WRITE; return(-1);}
  if (sound_state == SOUND_INITIALIZED) sound_state = SOUND_RUNNING;
  data_status = DATA_READY;
  data_bytes = bytes;
  data = (float *)buf;
  while (data_status == DATA_READY)
    {
      
    }
  return(0);

}

int mus_audio_close(int line) 
{
  AUDIO_ERROR = MUS_AUDIO_NO_ERROR;
  if (line == 1)
    {
      player.Stop();
      sound_state = SOUND_UNREADY;
    }
  return(0);
}

int mus_audio_read(int line, char *buf, int bytes) 
{
}

int mus_audio_mixer_read(int ur_dev, int field, int chan, float *val) 
{
}

int mus_audio_mixer_write(int ur_dev, int field, int chan, float *val) 
{
}

void mus_audio_save(void) 
{
}

void mus_audio_restore(void) 
{
}

int mus_audio_initialize(void) 
{
  AUDIO_ERROR = MUS_AUDIO_NO_ERROR;
  return(0);
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

char *mus_audio_moniker(void) {return("HPUX audio");}

int mus_audio_open_output(int ur_dev, int srate, int chans, int format, int size)
{
  int fd,i,dev;
  struct audio_describe desc;
  AUDIO_ERROR = MUS_AUDIO_NO_ERROR;
  dev = MUS_AUDIO_DEVICE(ur_dev);
  fd = open("/dev/audio",O_RDWR);
  if (fd == -1) {AUDIO_ERROR = MUS_AUDIO_CANT_OPEN; return(-1);}
  ioctl(fd,AUDIO_SET_CHANNELS,chans);
  if (dev == MUS_AUDIO_SPEAKERS)
    ioctl(fd,AUDIO_SET_OUTPUT,AUDIO_OUT_SPEAKER);
  else
    if (dev == MUS_AUDIO_LINE_OUT)
      ioctl(fd,AUDIO_SET_OUTPUT,AUDIO_OUT_LINE);
    else ioctl(fd,AUDIO_SET_OUTPUT,AUDIO_OUT_HEADPHONE);
  if (format == MUS_BSHORT)
    ioctl(fd,AUDIO_SET_DATA_FORMAT,AUDIO_FORMAT_LINEAR16BIT);
  else
    if (format == MUS_MULAW)
      ioctl(fd,AUDIO_SET_DATA_FORMAT,AUDIO_FORMAT_ULAW);
  else 
    if (format == MUS_ALAW)
      ioctl(fd,AUDIO_SET_DATA_FORMAT,AUDIO_FORMAT_ALAW);
    else {AUDIO_ERROR = MUS_AUDIO_FORMAT_NOT_AVAILABLE; close(fd); return(-1);}
  ioctl(fd,AUDIO_DESCRIBE,&desc);
  for(i=0;i<desc.nrates;i++) if(srate == desc.sample_rate[i]) break;
  if (i == desc.nrates) {AUDIO_ERROR = SRATE_NOT_AVAILABLE; close(fd); return(-1);}
  ioctl(fd,AUDIO_SET_SAMPLE_RATE,srate);
  return(fd);
}

int mus_audio_write(int line, char *buf, int bytes)
{
  AUDIO_ERROR = MUS_AUDIO_NO_ERROR;
  write(line,buf,bytes);
  return(0);
}

int mus_audio_close(int line) 
{
  AUDIO_ERROR = MUS_AUDIO_NO_ERROR;
  close(line);
  return(0);
}

static void describe_audio_state_1(void)
{
  struct audio_describe desc;
  struct audio_gain gain;
  int mina,maxa,fd,tmp;
  int g[2];
  fd = open("/dev/audio",O_RDWR);
  if (fd == -1) return;
  ioctl(fd,AUDIO_GET_OUTPUT,&tmp);
  switch (tmp)
    {
    case AUDIO_OUT_SPEAKER: pprint("output: speakers\n"); break;
    case AUDIO_OUT_HEADPHONE: pprint("output: headphone\n"); break;
    case AUDIO_OUT_LINE: pprint("output: line out\n"); break;
    }
  ioctl(fd,AUDIO_GET_INPUT,&tmp);
  switch (tmp)
    {
    case AUDIO_IN_MIKE: pprint("input: mic\n"); break;
    case AUDIO_IN_LINE: pprint("input: line in\n"); break;
    }
  ioctl(fd,AUDIO_GET_DATA_FORMAT,&tmp);
  switch (tmp)
    {
    case AUDIO_FORMAT_LINEAR16BIT: pprint("format: 16-bit linear\n"); break;
    case AUDIO_FORMAT_ULAW: pprint("format: mulaw\n"); break;
    case AUDIO_FORMAT_ALAW: pprint("format: alaw\n"); break;
    }
  ioctl(fd,AUDIO_DESCRIBE,&desc);
  gain.channel_mask = (AUDIO_CHANNEL_LEFT | AUDIO_CHANNEL_RIGHT);
  ioctl(fd,AUDIO_GET_GAINS,&gain);
  close(fd);
  if (!strbuf) strbuf = (char *)CALLOC(STRBUF_SIZE,sizeof(char));
  g[0] = gain.cgain[0].transmit_gain; g[1] = gain.cgain[1].transmit_gain;
  mina = desc.min_transmit_gain;  maxa = desc.max_transmit_gain;
  sprintf(strbuf,"out vols: %.3f %.3f\n",(float)(g[0]-mina)/(float)(maxa-mina),(float)(g[1]-mina)/(float)(maxa-mina)); pprint(strbuf);
  g[0] = gain.cgain[0].receive_gain; g[1] = gain.cgain[1].receive_gain;
  mina = desc.min_receive_gain;  maxa = desc.max_receive_gain;
  sprintf(strbuf,"in vols: %.3f %.3f\n",(float)(g[0]-mina)/(float)(maxa-mina),(float)(g[1]-mina)/(float)(maxa-mina)); pprint(strbuf);
  g[0] = gain.cgain[0].monitor_gain; g[1] = gain.cgain[1].monitor_gain;
  mina = desc.min_monitor_gain;  maxa = desc.max_monitor_gain;
  sprintf(strbuf,"monitor vols: %.3f %.3f\n",(float)(g[0]-mina)/(float)(maxa-mina),(float)(g[1]-mina)/(float)(maxa-mina)); pprint(strbuf);
}

int mus_audio_mixer_read(int ur_dev, int field, int chan, float *val) 
{
  struct audio_describe desc;
  struct audio_gain gain;
  int audio_fd,srate,g,maxa,mina,dev;
  AUDIO_ERROR = MUS_AUDIO_NO_ERROR;
  dev = MUS_AUDIO_DEVICE(ur_dev);
  if (field == MUS_AUDIO_PORT)
    {
      val[0] = 4; 
      if (chan>1) val[1] = MUS_AUDIO_MICROPHONE; 
      if (chan>2) val[2] = MUS_AUDIO_DAC_OUT;
      if (chan>3) val[3] = MUS_AUDIO_LINE_OUT;
      if (chan>4) val[4] = MUS_AUDIO_LINE_IN;
    }
  else
    {
      if (field == FORMAT_FIELD)
        {
          val[0] = 3; 
          if (chan>1) val[1] = MUS_BSHORT;
          if (chan>2) val[2] = MUS_MULAW;
          if (chan>3) val[3] = MUS_ALAW;
        }
      else
        {
          audio_fd = open("/dev/audio",O_RDWR);
          ioctl(audio_fd,AUDIO_DESCRIBE,&desc);
          switch (dev)
            {
            case MUS_AUDIO_DEFAULT:
            case MUS_AUDIO_DAC_OUT:
            case MUS_AUDIO_SPEAKERS:
            case MUS_AUDIO_LINE_OUT:
              switch (field)
                {
                case MUS_AUDIO_AMP: 
                  ioctl(audio_fd,AUDIO_GET_GAINS,&gain);
                  if (chan == 0) g = gain.cgain[0].transmit_gain; else g = gain.cgain[1].transmit_gain;
                  mina = desc.min_transmit_gain;  maxa = desc.max_transmit_gain;
                  val[0] = (float)(g-mina)/(float)(maxa-mina);
                  break;
                case MUS_AUDIO_CHANNEL: val[0] = 2; break;
                case MUS_AUDIO_SRATE: 
                  ioctl(audio_fd,AUDIO_GET_SAMPLE_RATE,&srate); 
                  val[0] = srate; 
                  break;
                default: AUDIO_ERROR = MUS_AUDIO_CANT_READ; break;
                }
              break;
            case MUS_AUDIO_MICROPHONE:
            case MUS_AUDIO_LINE_IN:
            case MUS_AUDIO_DUPLEX_DEFAULT:
              switch (field)
                {
                case MUS_AUDIO_AMP: 
                  ioctl(audio_fd,AUDIO_GET_GAINS,&gain);
                  if (chan == 0) g = gain.cgain[0].receive_gain; else g = gain.cgain[1].receive_gain;
                  mina = desc.min_receive_gain;  maxa = desc.max_receive_gain;
                  val[0] = (float)(g-mina)/(float)(maxa-mina);
                  break;
                case MUS_AUDIO_CHANNEL: val[0] = 2; break;
                case MUS_AUDIO_SRATE: 
                  ioctl(audio_fd,AUDIO_GET_SAMPLE_RATE,&srate); 
                  val[0] = srate; 
                  break;
                default: AUDIO_ERROR = MUS_AUDIO_CANT_READ; break;
                }
              break;
            default: AUDIO_ERROR = MUS_AUDIO_CANT_READ; break;
            }
          close(audio_fd);
        }
    }
  if (AUDIO_ERROR != MUS_AUDIO_NO_ERROR) return(-1);
  return(0);
}

int mus_audio_mixer_write(int ur_dev, int field, int chan, float *val) 
{
  struct audio_describe desc;
  struct audio_gain gain;
  int audio_fd,srate,g,maxa,mina,dev;
  AUDIO_ERROR = MUS_AUDIO_NO_ERROR;
  dev = MUS_AUDIO_DEVICE(ur_dev);
  audio_fd = open("/dev/audio",O_RDWR);
  ioctl(audio_fd,AUDIO_DESCRIBE,&desc);
  switch (dev)
    {
    case MUS_AUDIO_DEFAULT:
    case MUS_AUDIO_DAC_OUT:
    case MUS_AUDIO_SPEAKERS:
    case MUS_AUDIO_LINE_OUT:
      switch (field)
        {
        case MUS_AUDIO_AMP: 
          mina = desc.min_transmit_gain;  maxa = desc.max_transmit_gain;
          ioctl(audio_fd,AUDIO_GET_GAINS,&gain);
          g = mina + val[0] * (maxa-mina);
          if (chan == 0) gain.cgain[0].transmit_gain = g; else gain.cgain[1].transmit_gain = g;
          ioctl(audio_fd,AUDIO_SET_GAINS,&gain);
          break;
        case MUS_AUDIO_SRATE: 
          srate = val[0];
          ioctl(audio_fd,AUDIO_SET_SAMPLE_RATE,srate); 
          break;
        default: AUDIO_ERROR = MUS_AUDIO_CANT_WRITE; break;
        }
      break;
    case MUS_AUDIO_MICROPHONE:
    case MUS_AUDIO_LINE_IN:
    case MUS_AUDIO_DUPLEX_DEFAULT:
      switch (field)
        {
        case MUS_AUDIO_AMP: 
          mina = desc.min_receive_gain;  maxa = desc.max_receive_gain;
          ioctl(audio_fd,AUDIO_GET_GAINS,&gain);
          g = mina + val[0] * (maxa-mina);
          if (chan == 0) gain.cgain[0].receive_gain = g; else gain.cgain[1].receive_gain = g;
          ioctl(audio_fd,AUDIO_SET_GAINS,&gain);
          break;
        case MUS_AUDIO_SRATE: 
          srate = val[0];
          ioctl(audio_fd,AUDIO_SET_SAMPLE_RATE,srate); 
          break;
        default: AUDIO_ERROR = MUS_AUDIO_CANT_WRITE; break;
        }
      break;
    default: AUDIO_ERROR = MUS_AUDIO_CANT_WRITE; break;
    }
  close(audio_fd);
  if (AUDIO_ERROR != MUS_AUDIO_NO_ERROR) return(-1);
  return(0);
}

static int saved_gains[6];

void mus_audio_save(void)
{
  int fd;
  struct audio_gain gain;
  gain.channel_mask = (AUDIO_CHANNEL_LEFT | AUDIO_CHANNEL_RIGHT);
  fd = open("/dev/audio",O_RDWR);
  ioctl(fd,AUDIO_GET_GAINS,&gain);
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
  fd = open("/dev/audio",O_RDWR);
  ioctl(fd,AUDIO_GET_GAINS,&gain);
  gain.cgain[0].transmit_gain = saved_gains[0];
  gain.cgain[0].receive_gain = saved_gains[1];
  gain.cgain[0].monitor_gain = saved_gains[2];
  gain.cgain[1].transmit_gain = saved_gains[3];
  gain.cgain[1].receive_gain = saved_gains[4];
  gain.cgain[1].monitor_gain = saved_gains[5];
  ioctl(fd,AUDIO_SET_GAINS,&gain);
}

int mus_audio_initialize(void) {AUDIO_ERROR = MUS_AUDIO_NO_ERROR; return(0);}

char *mus_audio_error_name(int err) {return(audio_error_name_1(err));}
int mus_audio_systems(void) {return(1);}
char *mus_audio_system_name(int system) {return("HPUX");}

/* struct audio_status status_b;
 * ioctl(devAudio, AUDIO_GET_STATUS, &status_b)
 * not_busy = (status_b.transmit_status == AUDIO_DONE);
*/

int mus_audio_open_input(int ur_dev, int srate, int chans, int format, int size) 
{
  int fd,i,dev;
  struct audio_describe desc;
  AUDIO_ERROR = MUS_AUDIO_NO_ERROR;
  dev = MUS_AUDIO_DEVICE(ur_dev);
  fd = open("/dev/audio",O_RDWR);
  if (fd == -1) {AUDIO_ERROR = MUS_AUDIO_CANT_OPEN; return(-1);}
  ioctl(fd,AUDIO_SET_CHANNELS,chans);
  if (dev == MUS_AUDIO_MICROPHONE)
    ioctl(fd,AUDIO_SET_INPUT,AUDIO_IN_MIKE);
  else ioctl(fd,AUDIO_SET_INPUT,AUDIO_IN_LINE);
  if (format == MUS_BSHORT)
    ioctl(fd,AUDIO_SET_DATA_FORMAT,AUDIO_FORMAT_LINEAR16BIT);
  else
    if (format == MUS_MULAW)
      ioctl(fd,AUDIO_SET_DATA_FORMAT,AUDIO_FORMAT_ULAW);
  else 
    if (format == MUS_ALAW)
      ioctl(fd,AUDIO_SET_DATA_FORMAT,AUDIO_FORMAT_ALAW);
    else {AUDIO_ERROR = MUS_AUDIO_FORMAT_NOT_AVAILABLE; close(fd); return(-1);}
  ioctl(fd,AUDIO_DESCRIBE,&desc);
  for(i=0;i<desc.nrates;i++) if(srate == desc.sample_rate[i]) break;
  if (i == desc.nrates) {AUDIO_ERROR = MUS_AUDIO_SRATE_NOT_AVAILABLE; close(fd); return(-1);}
  ioctl(fd,AUDIO_SET_SAMPLE_RATE,srate);
  return(fd);
}

int mus_audio_read(int line, char *buf, int bytes) 
{
  AUDIO_ERROR = MUS_AUDIO_NO_ERROR;
  read(line,buf,bytes);
  return(0);
}

#endif


/* ------------------------------- WINDOZE ----------------------------------------- */

#if defined(WINDOZE) && (!(defined(__CYGWIN__)))
#define AUDIO_OK

#include <windows.h>
#include <mmsystem.h>
#include <mmreg.h>

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

static MMRESULT win_in_err = 0,win_out_err=0;
static char errstr[128],getstr[128];

char *mus_audio_error_name(int err) 
{
  if ((win_in_err == 0) && (win_out_err == 0)) return(audio_error_name_1(err));
  if (win_in_err) 
    waveInGetErrorText(win_in_err,getstr,128);
  else waveOutGetErrorText(win_out_err,getstr,128);
  sprintf(errstr,"%s: %s",audio_error_name_1(err),getstr);
  return(errstr);
}

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
  AUDIO_ERROR = MUS_AUDIO_NO_ERROR; win_out_err = 0;
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
  win_out_err = waveOutOpen(&fd,WAVE_MAPPER,&wf,(DWORD)next_buffer,0,CALLBACK_FUNCTION); /* 0 here = user_data above, other case = WAVE_FORMAT_QUERY */
  if (win_out_err) {AUDIO_ERROR = MUS_AUDIO_DEVICE_NOT_AVAILABLE; return(-1);}
  waveOutPause(fd);
  if (size <= 0) buffer_size = 1024; else buffer_size = size;
  wh[0].dwBufferLength = buffer_size * current_datum_size;
  wh[0].dwFlags = 0;
  wh[0].dwLoops = 0;
  wh[0].lpData = (char *)CALLOC(wh[0].dwBufferLength,sizeof(char));
  if ((wh[0].lpData) == 0) {AUDIO_ERROR = MUS_AUDIO_SIZE_NOT_AVAILABLE; waveOutClose(fd);  return(-1);}
  win_out_err = waveOutPrepareHeader(fd,&(wh[0]),sizeof(WAVEHDR));
  if (win_out_err) 
    {
      AUDIO_ERROR = MUS_AUDIO_CONFIGURATION_NOT_AVAILABLE; 
      FREE(wh[0].lpData); 
      waveOutClose(fd);  
      return(-1);
    }
  db_state[0] = BUFFER_EMPTY;
  wh[1].dwBufferLength = buffer_size * current_datum_size;
  wh[1].dwFlags = 0;
  wh[1].dwLoops = 0;
  wh[1].lpData = (char *)CALLOC(wh[0].dwBufferLength,sizeof(char));
  if ((wh[1].lpData) == 0) {AUDIO_ERROR = MUS_AUDIO_SIZE_NOT_AVAILABLE; FREE(wh[0].lpData); waveOutClose(fd);  return(-1);}
  win_out_err = waveOutPrepareHeader(fd,&(wh[1]),sizeof(WAVEHDR));
  if (win_out_err) 
    {
      AUDIO_ERROR = MUS_AUDIO_CONFIGURATION_NOT_AVAILABLE; 
      waveOutUnprepareHeader(fd,&(wh[0]),sizeof(WAVEHDR)); 
      FREE(wh[0].lpData); 
      FREE(wh[1].lpData); 
      waveOutClose(fd);  
      return(-1);
    }
  db_state[1] = BUFFER_EMPTY;
  sound_state = SOUND_INITIALIZED;
  current_buf = 0;
  return(OUTPUT_LINE);
}

static MMRESULT fill_buffer(int dbi, char *inbuf, int instart, int bytes)
{
  int i,j;
  win_out_err = 0;
  if (sound_state == SOUND_UNREADY) return(0);
  for (i=instart,j=0;j<bytes;j++,i++) wh[dbi].lpData[j] = inbuf[i];
  wh[dbi].dwBufferLength = bytes;
  db_state[dbi] = BUFFER_FILLED;
  if ((sound_state == SOUND_INITIALIZED) && (dbi == 1))
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
      Sleep(1);      /* in millisecs, so even this may be too much if buf=256 bytes */
    }
}

int mus_audio_write(int line, char *buf, int bytes) 
{
  int lim,leftover,start;
  if (line != OUTPUT_LINE) {AUDIO_ERROR = MUS_AUDIO_CANT_WRITE; return(-1);}
  AUDIO_ERROR = MUS_AUDIO_NO_ERROR; win_out_err = 0;
  leftover = bytes;
  start = 0;
  if (sound_state == SOUND_UNREADY) return(0);
  while (leftover > 0)
    {
      lim = leftover;
      if (lim > buffer_size) lim = buffer_size;
      leftover -= lim;
      wait_for_empty_buffer(current_buf);
      win_out_err = fill_buffer(current_buf,buf,start,lim);
      if (win_out_err) 
        {
          AUDIO_ERROR = MUS_AUDIO_CANT_WRITE;
          fprintf(stderr,"error %s upon fill",win_out_err);
        }
      win_out_err = waveOutWrite(fd,&wh[current_buf],sizeof(WAVEHDR));
      if (win_out_err)
        {
          AUDIO_ERROR = MUS_AUDIO_CANT_WRITE;
          fprintf(stderr,"error %s upon write",win_out_err);
        }
      start += lim;
      current_buf++;
      if (current_buf>1) current_buf=0;
    }
  if (AUDIO_ERROR != MUS_AUDIO_NO_ERROR) return(-1);
  return(0);
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
  if (out_saved>0)
    {
      out_vols = (DWORD *)CALLOC(out_saved,sizeof(DWORD));
      out_set = (int *)CALLOC(out_saved,sizeof(int));
      for (dev=0;dev<out_saved;dev++)
        {
          err = waveOutGetDevCaps(dev,&wocaps,sizeof(wocaps));
          if ((!err) && (wocaps.dwSupport & WAVECAPS_VOLUME))
            {
	      err = waveOutOpen(&hd,dev,&pwfx,0,0,WAVE_MAPPER);
	      if (!err)
		{
		  err = waveOutGetVolume(hd,&val);
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
  if (aux_saved>0)
    {
      aux_vols = (DWORD *)CALLOC(aux_saved,sizeof(unsigned long));
      aux_set = (int *)CALLOC(aux_saved,sizeof(int));
      for (dev=0;dev<aux_saved;dev++)
        {
          err = auxGetDevCaps(dev,&wacaps,sizeof(wacaps));
          if ((!err) && (wacaps.dwSupport & AUXCAPS_VOLUME))
            {
              err = auxGetVolume(dev,&val);
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
  for (i=0;i<out_saved;i++)
    if (out_set[i])
      {
	err = waveOutOpen(&hd,i,&pwfx,0,0,WAVE_MAPPER);
	if (!err)
	  {
	    waveOutSetVolume(hd,out_vols[i]);
	    waveOutClose(hd);
	  }
      }
  for (i=0;i<aux_saved;i++) if (aux_set[i]) auxSetVolume(i,aux_vols[i]);
}

static float unlog(unsigned short val)
{
  /* 1.0 linear is 0xffff, rest is said to be "logarithmic", whatever that really means here */
  if (val == 0) return(0.0);
  return((float)val / 65536.0);
  /* return(pow(2.0,amp) - 1.0); */ /* doc seems to be bogus */
}

#define SRATE_11025_BITS (WAVE_FORMAT_1S16 | WAVE_FORMAT_1S08 | WAVE_FORMAT_1M16 | WAVE_FORMAT_1M08)
#define SRATE_22050_BITS (WAVE_FORMAT_2S16 | WAVE_FORMAT_2S08 | WAVE_FORMAT_2M16 | WAVE_FORMAT_2M08)
#define SRATE_44100_BITS (WAVE_FORMAT_4S16 | WAVE_FORMAT_4S08 | WAVE_FORMAT_4M16 | WAVE_FORMAT_4M08)
#define SHORT_SAMPLE_BITS (WAVE_FORMAT_1S16 | WAVE_FORMAT_1M16 | WAVE_FORMAT_2S16 | WAVE_FORMAT_2M16 | WAVE_FORMAT_4S16 | WAVE_FORMAT_4M16)
#define BYTE_SAMPLE_BITS (WAVE_FORMAT_1S08 | WAVE_FORMAT_1M08 | WAVE_FORMAT_2S08 | WAVE_FORMAT_2M08 | WAVE_FORMAT_4S08 | WAVE_FORMAT_4M08)

static char *mfg(int mf)
{
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
  int devs,dev,srate,chans,format,need_comma,maker;
  MMRESULT err;
  unsigned long val,rate,pitch,version;
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
  int control,controls,dest,dests,source,happy,dest_time,chan,mina,maxa,ctype;
#endif
  need_comma = 1;
  chans=1;
  if (!strbuf) strbuf = (char *)CALLOC(STRBUF_SIZE,sizeof(char));
  devs = waveOutGetNumDevs();
  if (devs>0)
    {
      pprint("Output:\n");
      for (dev=0;dev<devs;dev++)
        {
          err = waveOutGetDevCaps(dev,&wocaps,sizeof(wocaps));
          if (!err)
            {
              version = wocaps.vDriverVersion;
              maker = wocaps.wMid;
              sprintf(strbuf,"  %s %s: version %d.%d\n",
                      mfg(maker),wocaps.szPname,
                      version>>8,version&0xff);
              pprint(strbuf);
              if (wocaps.wChannels == 2) {chans=2; pprint("    stereo");} else {chans=1; pprint("    mono");}
              if (wocaps.dwFormats & SRATE_11025_BITS)  {srate = 11025; if (need_comma) pprint(","); pprint(" 11025"); need_comma=1;}
              if (wocaps.dwFormats & SRATE_22050_BITS)  {srate = 22050; if (need_comma) pprint(","); pprint(" 22050"); need_comma=1;}
              if (wocaps.dwFormats & SRATE_44100_BITS)  {srate = 44100; if (need_comma) pprint(","); pprint(" 44100"); need_comma=1;}
              if (wocaps.dwFormats & BYTE_SAMPLE_BITS)  {format = 8; if (need_comma) pprint(","); pprint(" unsigned byte"); need_comma=1;}
              if (wocaps.dwFormats & SHORT_SAMPLE_BITS) {format = 16; if (need_comma) pprint(","); pprint(" little-endian short"); need_comma=1;}
              if (need_comma) pprint("\n");
              need_comma = 0;
              pwfx.wFormatTag = WAVE_FORMAT_PCM;
              pwfx.nChannels = chans;
              pwfx.nSamplesPerSec = srate;
              pwfx.nAvgBytesPerSec = srate;
              pwfx.nBlockAlign = 1;
              pwfx.wBitsPerSample = format;

              err = waveOutOpen(&hd,dev,&pwfx,0,0,WAVE_FORMAT_QUERY);

              if (wocaps.dwSupport & WAVECAPS_VOLUME)
                {
                  err = waveOutGetVolume(hd,&val);
                  if (!err)
                    {
                      if (wocaps.dwSupport & WAVECAPS_LRVOLUME)
                        sprintf(strbuf,"  vol: %.3f %.3f",unlog((unsigned short)(val>>16)),unlog((unsigned short)(val&0xffff)));
                      else sprintf(strbuf,"  vol: %.3f",unlog((unsigned short)(val&0xffff)));
                      pprint(strbuf);
                      need_comma = 1;
                    }
                }
              if (!err)
                {
                  /* this is just to get the hd data for subsequent info */
                  if (wocaps.dwSupport & WAVECAPS_PLAYBACKRATE)
                    {
                      err = waveOutGetPlaybackRate(hd,&rate);
                      if (!err)
                        {
                          sprintf(strbuf,"%s playback rate: %.3f",(need_comma ? "," : ""),(float)rate/65536.0);
                          pprint(strbuf);
                          need_comma = 1;
                        }
                    }
                  if (wocaps.dwSupport & WAVECAPS_PITCH)
                    {
                      err = waveOutGetPitch(hd,&pitch);
                      if (!err)
                        {
                          sprintf(strbuf,"%s pitch: %.3f",(need_comma ? "," : ""),(float)pitch/65536.0);
                          pprint(strbuf);
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
  if (devs>0)
    {
      pprint("Input:\n");
      for (dev=0;dev<devs;dev++)
        {
          err = waveInGetDevCaps(dev,&wicaps,sizeof(wicaps));
          if (!err)
            {
              sprintf(strbuf,"  %s%s",(wicaps.wMid != maker) ? mfg(wicaps.wMid) : "",wicaps.szPname);
              pprint(strbuf);
              if ((wicaps.wMid != maker) || (version != wicaps.vDriverVersion))
                {
                  sprintf(strbuf,": version %d.%d\n",(wicaps.vDriverVersion>>8),wicaps.vDriverVersion&0xff);
                  pprint(strbuf);
                }
              else pprint("\n");
              if (wicaps.wChannels == 2) pprint("    stereo"); else pprint("    mono");
              if (wicaps.dwFormats & SRATE_11025_BITS)  {pprint(", 11025"); need_comma=1;}
              if (wicaps.dwFormats & SRATE_22050_BITS)  {if (need_comma) pprint(","); pprint(" 22050"); need_comma=1;}
              if (wicaps.dwFormats & SRATE_44100_BITS)  {if (need_comma) pprint(","); pprint(" 44100"); need_comma=1;}
              if (wicaps.dwFormats & BYTE_SAMPLE_BITS)  {if (need_comma) pprint(","); pprint(" unsigned byte"); need_comma=1;}
              if (wicaps.dwFormats & SHORT_SAMPLE_BITS) {if (need_comma) pprint(","); pprint(" little-endian short");}
              pprint("\n");
            }
        }
    }
  devs = auxGetNumDevs();
  if (devs>0)
    {
      pprint("Auxiliary:\n");
      for (dev=0;dev<devs;dev++)
        {
          err = auxGetDevCaps(dev,&wacaps,sizeof(wacaps));
          if (!err)
            {
              sprintf(strbuf,"  %s%s",(wacaps.wMid != maker) ? mfg(wacaps.wMid) : "",wacaps.szPname);
              pprint(strbuf);
              if ((wacaps.wMid != maker) || (version != wacaps.vDriverVersion))
                sprintf(strbuf,": version %d.%d%s",
                        (wacaps.vDriverVersion>>8),wacaps.vDriverVersion&0xff,
                        (wacaps.wTechnology & AUXCAPS_CDAUDIO) ? " (CD)" : "");
              else sprintf(strbuf,"%s\n",(wacaps.wTechnology & AUXCAPS_CDAUDIO) ? " (CD)" : "");
              pprint(strbuf);
              if (wacaps.dwSupport & AUXCAPS_VOLUME)
                {
                  err = auxGetVolume(dev,&val);
                  if (!err)
                    {
                      if (wacaps.dwSupport & AUXCAPS_LRVOLUME)
                        sprintf(strbuf,"  vol: %.3f %.3f\n",unlog((unsigned short)(val>>16)),unlog((unsigned short)(val&0xffff)));
                      else sprintf(strbuf,"  vol: %.3f\n",unlog((unsigned short)(val&0xffff)));
                      pprint(strbuf);
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
      for (dev=0;dev<devs;dev++)
        {
          err = mixerGetDevCaps(dev,&wmcaps,sizeof(wmcaps));
          if (!err)
            {
              sprintf(strbuf,"  %s%s",(wmcaps.wMid != maker) ? mfg(wmcaps.wMid) : "",wmcaps.szPname);
              pprint(strbuf);
              if ((wmcaps.wMid != maker) || (version != wmcaps.vDriverVersion))
                {
                  sprintf(strbuf,": version %d.%d\n",(wmcaps.vDriverVersion>>8),wmcaps.vDriverVersion&0xff);
                  pprint(strbuf);
                }
              else pprint("\n");
              dests = wmcaps.cDestinations;
              
              err = mixerOpen(&mfd,dev,0,0,CALLBACK_NULL);
              if (!err)
                {
                  dest=0;
                  source=0;
                  dest_time = 1;
                  happy = 1;
                  while (happy)
                    {
                      if (dest_time)
                        {
                          mixline.dwDestination = dest;
                          mixline.cbStruct = sizeof(MIXERLINE);
                          err = mixerGetLineInfo(mfd,&mixline,MIXER_GETLINEINFOF_DESTINATION);
                        }
                      else
                        {
                          mixline.dwSource = source;
                          mixline.cbStruct = sizeof(MIXERLINE);
                          err = mixerGetLineInfo(mfd,&mixline,MIXER_GETLINEINFOF_SOURCE);
                        }
                      if (!err)
                        {
                          if ((source == 0) && (!dest_time)) pprint("  Sources:\n");
                          if ((dest == 0) && (dest_time)) pprint("  Destinations:\n");
                          sprintf(strbuf,"    %s: %s (%s), %d chan%s",
                                  mixline.szName,
                                  mixer_component_name(mixline.dwComponentType),
                                  mixer_target_name(mixline.Target.dwType),
                                  
                                  mixline.cChannels,((mixline.cChannels != 1) ? "s" : ""));
                          pprint(strbuf);
                          if (mixline.cConnections > 0)
                                {
                                    sprintf(strbuf,", %d connection%s",
                                    mixline.cConnections,((mixline.cConnections != 1) ? "s" : ""));
                                    pprint(strbuf);
                                }
                          if (dest_time) 
                            {
                              sprintf(strbuf,"%s\n",mixer_status_name(mixline.fdwLine));
                              pprint(strbuf);
                            }
                          else pprint("\n");
                          if (mixline.cControls > 0)
                            {
                              linecontrols.cbStruct = sizeof(MIXERLINECONTROLS);
                              linecontrols.dwLineID = mixline.dwLineID;
                              linecontrols.dwControlID = MIXER_GETLINECONTROLSF_ONEBYID;
                              if (linecontrols.cControls > MAX_DESCRIBE_CONTROLS)
                                linecontrols.cControls = MAX_DESCRIBE_CONTROLS;
                              else linecontrols.cControls = mixline.cControls;
                              linecontrols.pamxctrl = mc;
                              linecontrols.cbmxctrl = sizeof(MIXERCONTROL);
                              err = mixerGetLineControls(mfd,&linecontrols,MIXER_GETLINECONTROLSF_ALL);
                              if (!err)
                                {
                                  sprintf(strbuf,"      %d control%s:\n",linecontrols.cControls,(linecontrols.cControls != 1) ? "s" : "");
                                  pprint(strbuf);
                                  controls = linecontrols.cControls;
                                  if (controls > MAX_DESCRIBE_CONTROLS) controls = MAX_DESCRIBE_CONTROLS;
                                  for (control=0;control<controls;control++)
                                    {

                                       sprintf(strbuf,"        %s",mc[control].szName);
                                       pprint(strbuf);
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
					   err = mixerGetControlDetails(mfd,&controldetails,MIXER_GETCONTROLDETAILSF_LISTTEXT);
					   if (!err) 
					     {
					       for (chan=0;chan<mixline.cChannels;chan++) 
						 {
						   sprintf(strbuf," [%s]",clist[chan].szName);
						   pprint(strbuf);
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
                                         default: err=1; break;
                                         }
                                       if (err) 
                                         pprint("\n");
                                       else
                                         {
                                           err = mixerGetControlDetails(mfd,&controldetails,MIXER_GETCONTROLDETAILSF_VALUE);
                                           if (!err)
                                             {
                                               chans = controldetails.cChannels;
                                               if (chans > MAX_DESCRIBE_CHANS) chans = MAX_DESCRIBE_CHANS;
                                               switch (mc[control].dwControlType & MIXERCONTROL_CT_UNITS_MASK)
                                                 {
                                                 case MIXERCONTROL_CT_UNITS_BOOLEAN:
                                                   for (chan=0;chan<chans;chan++)
                                                     {
                                                       sprintf(strbuf," %s",(cbool[chan].fValue) ? " on" : " off");
                                                       pprint(strbuf);
                                                     }
                                                   break;
                                                 case MIXERCONTROL_CT_UNITS_SIGNED: case MIXERCONTROL_CT_UNITS_DECIBELS:
                                                   mina = mc[control].Bounds.lMinimum;
                                                   maxa = mc[control].Bounds.lMaximum;
                                                   if (maxa > mina)
                                                     {
                                                       for (chan=0;chan<chans;chan++)
                                                         {
                                                           sprintf(strbuf," %.3f",(float)(csign[chan].lValue - mina)/(float)(maxa - mina));
                                                           pprint(strbuf);
                                                         }
                                                     }
                                                   break;
                                                 case MIXERCONTROL_CT_UNITS_UNSIGNED: case MIXERCONTROL_CT_UNITS_PERCENT:
                                                   mina = mc[control].Bounds.dwMinimum;
                                                   maxa = mc[control].Bounds.dwMaximum;
                                                   if (maxa > mina)
                                                     {
                                                       for (chan=0;chan<chans;chan++)
                                                         {
                                                           sprintf(strbuf," %.3f",(float)(cline[chan].dwValue - mina)/(float)(maxa - mina));
                                                           pprint(strbuf);
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
  AUDIO_ERROR = MUS_AUDIO_NO_ERROR;
  return(0);
}

int mus_audio_close(int line) 
{
  int i;
  AUDIO_ERROR = MUS_AUDIO_NO_ERROR; win_out_err = 0; win_in_err = 0;
  if (line == OUTPUT_LINE)
    {
      /* fill with a few zeros, wait for empty flag */
      if (sound_state != SOUND_UNREADY)
        {
          wait_for_empty_buffer(current_buf);
          for (i=0;i<128;i++) wh[current_buf].lpData[i] = 0;
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
          if (win_out_err) AUDIO_ERROR = MUS_AUDIO_CANT_CLOSE;
          db_state[0] = BUFFER_EMPTY;
          db_state[1] = BUFFER_EMPTY;
          sound_state = SOUND_UNREADY;
          waveOutUnprepareHeader(fd,&(wh[0]),sizeof(WAVEHDR));
          waveOutUnprepareHeader(fd,&(wh[1]),sizeof(WAVEHDR));
          FREE(wh[0].lpData);
          FREE(wh[1].lpData);
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
              waveInUnprepareHeader(record_fd,&rec_wh,sizeof(WAVEHDR));
              if (rec_wh.lpData) {FREE(rec_wh.lpData); rec_wh.lpData = NULL;}
              rec_state = SOUND_UNREADY;
            }
        }
      else AUDIO_ERROR = MUS_AUDIO_CANT_CLOSE;
    }
  return((AUDIO_ERROR == MUS_AUDIO_NO_ERROR) ? 0 : -1);
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
  AUDIO_ERROR = MUS_AUDIO_NO_ERROR; win_in_err = 0;
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
  rec_wh.lpData = (char *)CALLOC(rec_wh.dwBufferLength,sizeof(char));
  if ((rec_wh.lpData) == 0) {AUDIO_ERROR = MUS_AUDIO_SIZE_NOT_AVAILABLE; return(-1);}
  win_in_err = waveInOpen(&record_fd,WAVE_MAPPER,&wf,(DWORD)next_input_buffer,0,CALLBACK_FUNCTION);
  if (win_in_err) 
    {
      AUDIO_ERROR = MUS_AUDIO_DEVICE_NOT_AVAILABLE; 
      FREE(rec_wh.lpData);
      return(-1);
    }
  win_in_err = waveInPrepareHeader(record_fd,&(rec_wh),sizeof(WAVEHDR));
  if (win_in_err) 
    {
      AUDIO_ERROR = MUS_AUDIO_CONFIGURATION_NOT_AVAILABLE; 
      FREE(rec_wh.lpData);
      waveInClose(record_fd);
      return(-1);
    }
  return(0);
}

int mus_audio_read(int line, char *buf, int bytes) 
{
  AUDIO_ERROR = MUS_AUDIO_NO_ERROR;
  win_in_err = 0;
  return(-1);
}

int mus_audio_mixer_read(int ur_dev, int field, int chan, float *val) 
{
  int dev,sys;
  unsigned long lval;
  MMRESULT err;
  AUDIO_ERROR = MUS_AUDIO_NO_ERROR;
  sys = MUS_AUDIO_SYSTEM(ur_dev);
  dev = MUS_AUDIO_DEVICE(ur_dev);
  if (field == MUS_AUDIO_AMP)
    {
      err = auxGetVolume(sys,&lval);
      if (!err)
	{
	  if (chan == 0)
	    val[0] = unlog((unsigned short)(lval>>16));
	  else val[0] = unlog((unsigned short)(lval&0xffff));
	}
      else AUDIO_ERROR = MUS_AUDIO_CANT_READ; 
    }
  else AUDIO_ERROR = MUS_AUDIO_CANT_READ; 
  return((AUDIO_ERROR == MUS_AUDIO_NO_ERROR) ? 0 : -1);
}

int mus_audio_mixer_write(int ur_dev, int field, int chan, float *val) 
{
  int dev,sys;
  unsigned long lval;
  MMRESULT err;
  AUDIO_ERROR = MUS_AUDIO_NO_ERROR;
  sys = MUS_AUDIO_SYSTEM(ur_dev);
  dev = MUS_AUDIO_DEVICE(ur_dev);
  if (field == MUS_AUDIO_AMP)
    {
      err = auxGetVolume(sys,&lval);
      if (!err)
	{
	  if (chan == 0)
	    lval = (unsigned long)((lval & 0xffff) | (((unsigned short)(val[0]*65535))<<16));
	  else lval = (unsigned long)((lval & 0xffff0000) | ((unsigned short)(val[0]*65535)));
	  err = auxSetVolume(sys,lval);
	  if (!err) AUDIO_ERROR = MUS_AUDIO_CANT_WRITE; 
	}
      else AUDIO_ERROR = MUS_AUDIO_CANT_WRITE; 
    }
  else AUDIO_ERROR = MUS_AUDIO_CANT_WRITE; 
  return((AUDIO_ERROR == MUS_AUDIO_NO_ERROR) ? 0 : -1);
}

#endif



/* ------------------------------- AIX ----------------------------------------- */

#ifdef AIX
#define AUDIO_OK

/* this code taken from Xanim, esound, and MikMod */
#if 0
#include <errno.h>
#include <fcntl.h>
#endif
#include <sys/audio.h>
#include <stropts.h>
#include <sys/types.h>
#include <sys/file.h>
#include <sys/stat.h>
#include <sys/param.h>

/* Some AIX machines have "/dev/acpa0/1" instead. */
/* Some AIX machines have "/dev/baud0/1" instead. UltiMedia Sound system */
#define DAC_NAME   "/dev/paud0/1"

int audio_started = 0;

int mus_audio_open_output(int dev, int srate, int chans, int format, int size)
{
  audio_init init;
  audio_control control;
  audio_change change;
  int line,err;
  AUDIO_ERROR = MUS_AUDIO_NO_ERROR;
  audio_started = 0;
  line = open(DAC_NAME,O_WRONLY | O_NDELAY);
  if (line == -1) 
    AUDIO_ERROR = MUS_AUDIO_CANT_OPEN;
  else
    {
      memset(&init,'\0',sizeof(init));
      init.srate = srate;
      init.channels = chans;
      init.mode = PCM;
      init.operation = PLAY;
      init.flags = FIXED | BIG_ENDIAN | TWOS_COMPLEMENT;
      init.bits_per_sample = 16;
      init.bsize = AUDIO_IGNORE;
      err = ioctl(line,AUDIO_INIT,&init);
      if (err == -1)
	AUDIO_ERROR = MUS_AUDIO_CONFIGURATION_NOT_AVAILABLE;
      else
	{
	  memset (&control,'\0',sizeof(control));
	  memset (&change,'\0',sizeof(change));
	  change.output = EXTERNAL_SPEAKER | INTERNAL_SPEAKER | OUTPUT_1;
	  change.balance       = 0x3fff0000;
	  change.balance_delay = 0;
	  change.volume        = (long)(0x7fff << 16);
	  change.volume_delay  = 0;
	  change.input         = AUDIO_IGNORE;
	  change.monitor       = AUDIO_IGNORE;
	  change.dev_info      = (char *)NULL;
	  control.ioctl_request = AUDIO_CHANGE; /* AUDIO_STOP AUDIO_WAIT AUDIO_START */
	  control.position =      0;
	  control.request_info =  (char *)&change;
	  err = ioctl(line,AUDIO_CONTROL,&control);
	  if (err == -1) AUDIO_ERROR = MUS_AUDIO_CONFIGURATION_NOT_AVAILABLE;
	}
    }
  return(line);
}

int mus_audio_write(int line, char *buf, int bytes) 
{
  audio_control control;
  int err;
  if (audio_started == 0)
    {
      audio_started = 1;
      memset(&control,'\0',sizeof(control));
      control.ioctl_request = AUDIO_START;
      control.request_info  = NULL;
      control.position      = 0;
      err = ioctl(line,AUDIO_CONTROL,&control);
      if (err == -1) AUDIO_ERROR = MUS_AUDIO_CANT_WRITE;
    }
  err = write(line,buf,bytes);
  return(0);
}

int mus_audio_close(int line) 
{
  close(line); 
  audio_started = 0;
  return(0);
}

static void describe_audio_state_1(void) {pprint("audio stubbed out");}
int mus_audio_open_input(int dev, int srate, int chans, int format, int size) {return(-1);} /* no input */
int mus_audio_read(int line, char *buf, int bytes) {return(-1);}
int mus_audio_mixer_read(int dev, int field, int chan, float *val) {return(-1);}
int mus_audio_mixer_write(int dev, int field, int chan, float *val) {return(-1);}
void mus_audio_save(void) {}
void mus_audio_restore(void) {}
int mus_audio_initialize(void) {AUDIO_ERROR = MUS_AUDIO_NO_ERROR; return(-1);}
char *mus_audio_error_name(int err) {return(audio_error_name_1(err));}
int mus_audio_systems(void) {return(1);}
char *mus_audio_system_name(int system) {return("aix");}
void mus_audio_set_oss_buffers(int num,int size) {}
char *mus_audio_moniker(void) {return("aix audio");}

#endif



/* ------------------------------- NEC EWS ----------------------------------------- */

#ifdef NEC_EWS
#define AUDIO_OK

/* this code taken from Xanim */

#include <sys/audio.h>
#include <errno.h>
#define DAC_NAME   "/dev/audio/audio"

static struct AU_Volume audio_vol;
static struct AU_Type audio_type;
static struct AU_Line audio_line;
static struct AU_Status audio_status;

static int valid[] = {AURATE5_5,AURATE6_6,AURATE8_0,
		      AURATE9_6,AURATE11_0,AURATE16_0,AURATE18_9,AURATE22_1,
		      AURATE27_4,AURATE32_0,AURATE33_1,AURATE37_8,AURATE44_1,
		      AURATE48_0,0};

int mus_audio_open_output(int dev, int srate, int chans, int format, int size)
{
  int line,err,i,best;
  AUDIO_ERROR = MUS_AUDIO_NO_ERROR;
  line = open(DAC_NAME,O_WRONLY | O_NDELAY);
  if (line == -1) 
    AUDIO_ERROR = MUS_AUDIO_CANT_OPEN;
  else
    {
      i = 0;
      best = 8000;
      while (valid[i])
	{ 
	  if (srate <= valid[i])
	    {
	      best = i;
	      break;
	    }
	  i++;
	}
      audio_type.rate=best;
      audio_type.bit_type=AU_PCM16;
      audio_type.channel=((chans == 1) ? AU_MONO : AU_STEREO);
      err = ioctl(line, AUIOC_SETTYPE,&audio_type);
      audio_line.outline = AUOUT_SP | AUOUT_PHONE;
      err = ioctl(line,AUIOC_SETLINE,&audio_line);
      if (err == -1) AUDIO_ERROR = MUS_AUDIO_CONFIGURATION_NOT_AVAILABLE;
    }
  return(line);
}

int mus_audio_write(int line, char *buf, int bytes) 
{
  write(line,buf,bytes);
  return(0);
}

int mus_audio_close(int line) 
{
  close(line);
  return(0);
}

/* also AUIOC_GETVOLUME via ioctl(line,AUIOC_GETVOLUME,audio_vol);
   then volume * AUOUT_ATTEMAX) or something
   audio_vol.left = audio_vol.right = vol;
   ioctl(line,AUIOC_SETVOLUME,&autio_vol);
*/

static void describe_audio_state_1(void) {pprint("audio stubbed out");}
int mus_audio_open_input(int dev, int srate, int chans, int format, int size) {return(-1);} /* no input */
int mus_audio_read(int line, char *buf, int bytes) {return(-1);}
int mus_audio_mixer_read(int dev, int field, int chan, float *val) {return(-1);}
int mus_audio_mixer_write(int dev, int field, int chan, float *val) {return(-1);}
void mus_audio_save(void) {}
void mus_audio_restore(void) {}
int mus_audio_initialize(void) {AUDIO_ERROR = MUS_AUDIO_NO_ERROR; return(-1);}
char *mus_audio_error_name(int err) {return(audio_error_name_1(err));}
int mus_audio_systems(void) {return(1);}
char *mus_audio_system_name(int system) {return("nec ews");}
void mus_audio_set_oss_buffers(int num,int size) {}
char *mus_audio_moniker(void) {return("nec ews audio");}

#endif

/* ------------------------------- SONY NEWS ----------------------------------------- */

#ifdef SONY_NEWS
#define AUDIO_OK

/* this code taken from Xanim */
/* this SVR4 check doesn't work in Solaris, so I doubt it works here */
#ifdef SVR4
  #include <sys/sound.h>
#else
  #include <newsiodev/sound.h>
#endif
#include <errno.h>

static int sony_audio_rate1[] =  /* NWS-5000,4000 */
	{ RATE8000, RATE9450, RATE11025, RATE12000, RATE16000,
	  RATE18900, RATE22050, RATE24000, RATE32000, RATE37800,
	  RATE44056, RATE44100, RATE48000, 0 } ;
 
static int sony_audio_rate2[] =  /* NWS-3200, 3400, 3700, 3800 */
	{ RATE8000, RATE9450, RATE18900, RATE37800, 0 } ;
 
static int sony_audio_rate3[] =  /* NWS-3100 */
	{ RATE8000, 0 } ;

int sony_audio_buf_len ;
static struct sbparam	audio_info ;
static int *sony_audio_rate;
static int sony_max_vol, sony_min_vol;

int mus_audio_open_output(int dev, int srate, int chans, int format, int size) 
{
  int line,err;
  AUDIO_ERROR = MUS_AUDIO_NO_ERROR;
  line = open("/dev/sb0", O_WRONLY | O_NDELAY);
  if (line == -1)
    {
      if (errno == EBUSY) 
	AUDIO_ERROR = OUTPUT_BUSY;
      else AUDIO_ERROR = MUS_AUDIO_DEVICE_NOT_AVAILABLE;
      return(-1);
    }
  if (ioctl(line,SBIOCGETTYPE,(char *)&sbtype)<0) 
    {
#ifdef SVR4
      AUDIO_ERROR = MUS_AUDIO_CONFIGURATION_NOT_AVAILABLE;
      close(line);
      return(-1);
#endif 
      sbtype = SBTYPE_AIF2;
    }
  switch(sbtype) 
    {
    case SBTYPE_AIF2: case SBTYPE_AIF2_L: case SBTYPE_AIF2_E:
      sony_max_vol = 0;
      sony_min_vol = -32;
      sony_audio_rate = &sony_audio_rate2[0];
      break;
    case SBTYPE_AIF3:
      sony_max_vol = 8;
      sony_min_vol = -8;
      sony_audio_rate = &sony_audio_rate3[0];
      break;
    case SBTYPE_AIF5: case SBTYPE_AD1848: default:
      sony_max_vol = 16;
      sony_min_vol = -16;
      sony_audio_rate = &sony_audio_rate1[0];
      break;
    }
  audio_info.sb_mode    = LOGPCM ;
  audio_info.sb_format  = 0 ;
  audio_info.sb_compress= MULAW ;
  audio_info.sb_rate    = RATE8000 ;
  audio_info.sb_channel = MONO ;
  audio_info.sb_bitwidth= RES8B ;
  audio_info.sb_emphasis= 0 ;	
  err = ioctl(line,SBIOCSETPARAM,&audio_info);
  if (err == -1) 
    {
      AUDIO_ERROR = MUS_AUDIO_CONFIGURATION_NOT_AVAILABLE;
      close(line);
      return(-1);
    }
  return(line);
}

int mus_audio_write(int line, char *buf, int bytes) 
{
  write(line,buf,bytes);
  ioctl(line,SBIOCFLUSH,0);
  return(0);
}

int mus_audio_close(int line) 
{
  ioctl(line,SBIOCFLUSH,0);
  ioctl(line,SBIOCWAIT,0);
  close(line);
  return(0);
}

/* set volume
  struct sblevel 	gain;
  int			mflg;
  int			o_level;
  mflg = MUTE_OFF;
  ioctl(devAudio, SBIOCMUTE, &mflg);
  o_level = sony_min_vol +
                 ((volume * (sony_max_vol - sony_min_vol)) / XA_AUDIO_MAXVOL);
  if (o_level > sony_max_vol)	o_level = sony_max_vol;
  if (o_level < sony_min_vol)	o_level = sony_min_vol;
 
  if (o_level == sony_min_vol) 
  {
    mflg = MUTE_ON;
    ioctl(devAudio, SBIOCMUTE, &mflg);
  }
  gain.sb_right = gain.sb_left = (o_level << 16);
  ioctl(devAudio, SBIOCSETOUTLVL, &gain);
  */

int mus_audio_open_input(int dev, int srate, int chans, int format, int size) {return(-1);}
static void describe_audio_state_1(void) {pprint("audio stubbed out");}
int mus_audio_read(int line, char *buf, int bytes) {return(-1);}
int mus_audio_mixer_read(int dev, int field, int chan, float *val) {return(-1);}
int mus_audio_mixer_write(int dev, int field, int chan, float *val) {return(-1);}
void mus_audio_save(void) {}
void mus_audio_restore(void) {}
int mus_audio_initialize(void) {return(-1);}
char *mus_audio_error_name(int err) {return(audio_error_name_1(err));}
int mus_audio_systems(void) {return(1);}
char *mus_audio_system_name(int system) {return("Sony news");}
void mus_audio_set_oss_buffers(int num,int size) {}
char *mus_audio_moniker(void) {return("Sony audio");}
#endif


/* ------------------------------- NETBSD ----------------------------------------- */

#if defined(NETBSD) && (!(defined(AUDIO_OK)))
#define AUDIO_OK
/* taken from Xanim */
#include <errno.h>
#include <fcntl.h>
#include <sys/audioio.h>
#include <sys/file.h>
#include <sys/stat.h>
#include <sys/ioctl.h>
#include <sys/ioccom.h>

int mus_audio_open_output(int dev, int srate, int chans, int format, int size) 
{
  audio_info_t a_info;
  line = open("/dev/audio",O_WRONLY | O_NDELAY);
  if (line == -1)
    {
      if (errno == EBUSY) 
	AUDIO_ERROR = OUTPUT_BUSY;
      else AUDIO_ERROR = DEVICE_NOT_AVAILABLE;
      return(-1);
    }
  AUDIO_INITINFO(&a_info);
  a_info.blocksize = 1024;
  ioctl(line, AUDIO_SETINFO, &a_info);
  AUDIO_INITINFO(&a_info);
#ifndef AUDIO_ENCODING_SLINEAR
  a_info.play.encoding = AUDIO_ENCODING_PCM16;
#else
  /* NetBSD-1.3 */
  a_info.play.encoding = AUDIO_ENCODING_SLINEAR; /* Signed, nativeorder */
#endif
  ioctl(line, AUDIO_SETINFO, &a_info);
  AUDIO_INITINFO(&a_info);
  a_info.mode = AUMODE_PLAY | AUMODE_PLAY_ALL;
  ioctl(line, AUDIO_SETINFO, &a_info);
  AUDIO_INITINFO(&a_info);
  a_info.play.precision = 16;
  ioctl(line, AUDIO_SETINFO, &a_info);
  AUDIO_INITINFO(&a_info);
  a_info.play.sample_rate = srate;
  a.info.play.port = AUDIO_SPEAKER | AUDIO_HEADPHONE | AUDIO_LINE_OUT;
  ioctl(line, AUDIO_SETINFO, &a_info);
  return(line);
  /* a.info.blocksize after getinfo */
}

int mus_audio_write(int line, char *buf, int bytes) 
{
  write(line,buf,bytes);
  return(0);
}

int mus_audio_close(int line) 
{
  close(line);
  return(0);
}

/* set volume
  a_info.play.gain = AUDIO_MIN_GAIN + (volume * (AUDIO_MAX_GAIN - AUDIO_MIN_GAIN));
  ioctl(devAudio, AUDIO_SETINFO, &a_info);
  */

int mus_audio_open_input(int dev, int srate, int chans, int format, int size) {return(-1);}
static void describe_audio_state_1(void) {pprint("audio stubbed out");}
int mus_audio_read(int line, char *buf, int bytes) {return(-1);}
int mus_audio_mixer_read(int dev, int field, int chan, float *val) {return(-1);}
int mus_audio_mixer_write(int dev, int field, int chan, float *val) {return(-1);}
void mus_audio_save(void) {}
void mus_audio_restore(void) {}
int mus_audio_initialize(void) {return(-1);}
char *mus_audio_error_name(int err) {return(audio_error_name_1(err));}
int mus_audio_systems(void) {return(1);}
char *mus_audio_system_name(int system) {return("NetBSD");}
void mus_audio_set_oss_buffers(int num,int size) {}
char *mus_audio_moniker(void) {return("NetBSD audio");}

#endif


/* ------------------------------- AudioFile ----------------------------------------- */
#ifdef AUDIOFILE
#define AUDIO_OK
/* taken from Xanim */

#include <AF/audio.h>
#include <AF/AFlib.h>
#include <AF/AFUtils.h>

static AFAudioConn *AFaud;
static AC ac;
static AFSetACAttributes AFattributes;
static ATime AFtime0=0;
static ATime AFbaseT=0;
#define	AF_C_MIN	-30
#define	AF_C_MAX	30
#define AF_MAP_TO_C_GAIN(vol) (( ((vol)*AF_C_MAX)/XA_AUDIO_MAXVOL) + (AF_C_MIN))

#define SPU(type)       AF_sample_sizes[type].samps_per_unit
#define BPU(type)       AF_sample_sizes[type].bytes_per_unit

#define DPHONE(od) (int)(((AAudioDeviceDescriptor(AFaud, (od)))->outputsToPhone))
#define DFREQ(od) (int)(((AAudioDeviceDescriptor(AFaud, (od)))->playSampleFreq))
#define DCHAN(od) (int)((AAudioDeviceDescriptor(AFaud, (od)))->playNchannels)
#define DTYPE(od) ((AAudioDeviceDescriptor(AFaud, (od)))->playBufType)

#define	TOFFSET	250
static int FindDefaultDevice(AFAudioConn *aud)
{
  int i;
  char *ep;
  if ((ep = getenv("AF_DEVICE")) != NULL) 
    { 
      int udevice;
      udevice = atoi(ep);
      if ((udevice >= 0) && (udevice < ANumberOfAudioDevices(aud)))
	return udevice;
    }
  for(i=0; i<ANumberOfAudioDevices(aud); i++) 
    {
      if ( DPHONE(i) == 0 && DCHAN(i) == 1 ) return i;
    }
  return -1;
}

int mus_audio_open_output(int dev, int srate, int chans, int format, int size) 
{
  if ( (AFaud = AFOpenAudioConn("")) == NULL) 
    {
      AUDIO_ERROR = MUS_AUDIO_DEVICE_NOT_AVAILABLE;
      return(-);
    }
  AFdevice = FindDefaultDevice(AFaud);
  AFattributes.type = LIN16;
  AFattributes.endian = ALittleEndian;
  AFattributes.play_gain = AF_MAP_TO_C_GAIN(XAAUD->volume);
  ac = AFCreateAC(AFaud, AFdevice, (ACPlayGain | ACEncodingType | ACEndian), &AFattributes);
  AFSync(AFaud, 0);     /* Make sure we confirm encoding type support. */
  if (ac == NULL)
    {
      AUDIO_ERROR = MUS_AUDIO_CONFIGURATION_NOT_AVAILABLE;
      return(-1);
    }
  AFtime0 = AFbaseT = AFGetTime(ac) + TOFFSET;
  return(0);
}

int mus_audio_write(int line, char *buf, int bytes) 
{
  ATime act, atd = AFtime0;
  act = AFPlaySamples(ac,AFtime0,len,buf);
  if (AFtime0 < act)	
    AFtime0 = act+TOFFSET;
  elseAFtime0 += bytes >> 1; /* Number of samples */
}

int mus_audio_close(int line) 
{
  AF_Audio_Off(0);
  AFCloseAudioConn(AFaud);
}

/* set volume
 Client gain settings range from -30 to +30 dB.
 AFattributes.play_gain = AF_MAP_TO_C_GAIN(volume);
 AFChangeACAttributes(ac, ACPlayGain, &AFattributes);
 AFSync(AFaud,0);
 */

int mus_audio_open_input(int dev, int srate, int chans, int format, int size) {return(-1);}
static void describe_audio_state_1(void) {pprint("audio stubbed out");}
int mus_audio_read(int line, char *buf, int bytes) {return(-1);}
int mus_audio_mixer_read(int dev, int field, int chan, float *val) {return(-1);}
int mus_audio_mixer_write(int dev, int field, int chan, float *val) {return(-1);}
void mus_audio_save(void) {}
void mus_audio_restore(void) {}
int mus_audio_initialize(void) {return(-1);}
char *mus_audio_error_name(int err) {return(audio_error_name_1(err));}
int mus_audio_systems(void) {return(1);}
char *mus_audio_system_name(int system) {return("AudioFile");}
void mus_audio_set_oss_buffers(int num,int size) {}
char *mus_audio_moniker(void) {return("AudioFile audio");}

#endif


/* ------------------------------- OS2 ----------------------------------------- */

#ifdef OS2
#define AUDIO_OK

/* from mpg123 */
#include <stdlib.h>
#include <os2.h>
#define  INCL_OS2MM
#include <os2me.h>
#define BUFNUM     20
#define BUFSIZE 16384
typedef struct {
  ULONG operation;
  ULONG operand1;
  ULONG operand2;
  ULONG operand3;
} ple;
MCI_WAVE_SET_PARMS msp;
MCI_PLAY_PARMS mpp;
int id, pos=0;
ple pl[BUFNUM+2];

static struct audio_info_struct *ai;

int mus_audio_open_output(int dev, int srate, int chans, int format, int size)
{
  char *buf[BUFNUM];
  int i;
  ULONG rc;
  MCI_OPEN_PARMS mop;
  pos = 0;
  pl[0].operation = (ULONG)NOP_OPERATION;
  pl[0].operand1 = 0;
  pl[0].operand2 = 0;
  pl[0].operand3 = 0;
  for(i = 0; i < BUFNUM; i++) 
    {
      buf[i] = (char*)malloc(BUFSIZE);
      memset(buf[i], 0, BUFSIZE);
      pl[i+1].operation = (ULONG)DATA_OPERATION;
      pl[i+1].operand1 = (ULONG)buf[i];
      pl[i+1].operand2 = BUFSIZE/2;
      pl[i+1].operand3 = 0;
    }
  pl[BUFNUM+1].operation = (ULONG)BRANCH_OPERATION;
  pl[BUFNUM+1].operand1 = 0;
  pl[BUFNUM+1].operand2 = 1;
  pl[BUFNUM+1].operand3 = 0;
  mop.pszDeviceType = (PSZ)MCI_DEVTYPE_WAVEFORM_AUDIO_NAME;
  mop.pszElementName = (PSZ)&pl;
  rc = mciSendCommand(0, MCI_OPEN, MCI_WAIT | MCI_OPEN_PLAYLIST, (PVOID)&mop, 0);
  if (rc) 
    {
      AUDIO_ERROR = DEVICE_NOT_AVAILABLE;
      return(-1);
    }
  id = mop.usDeviceID;
  msp.usBitsPerSample = 16;
  msp.ulSamplesPerSec = srate;
  msp.usChannels = chans;
  rc = mciSendCommand(id, MCI_SET, MCI_WAIT | MCI_WAVE_SET_BITSPERSAMPLE | MCI_WAVE_SET_SAMPLESPERSEC | MCI_WAVE_SET_CHANNELS, (PVOID)&msp, 0);
  mciSendCommand(id, MCI_PLAY, 0, (PVOID)&mpp, 0);
  return(0);
}

int mus_audio_write(int line, char *buf, int bytes)
{
  pos = pos + 1;
  if (pos > BUFNUM) pos = 1;
  while (pl[pos].operand3 < pl[pos].operand2) _sleep2(125);
  memcpy((char*)pl[pos].operand1, buf, len);
  pl[pos].operand2 = len;
  pl[pos].operand3 = 0;
  return(0);
}

int mus_audio_close(int line)
{
  ULONG rc;
  pl[pos].operation = (ULONG)EXIT_OPERATION;
  pl[pos].operand2 = 0;
  pl[pos].operand3 = 0;
  pos = pos - 1;
  if(pos == 0) pos = BUFNUM;
  while (pl[pos].operand3 < pl[pos].operand2) _sleep2(250);
  _sleep2(2000);
  rc = mciSendCommand(id, MCI_CLOSE, MCI_WAIT, (PVOID)NULL, 0);
  return(0);
}

int mus_audio_open_input(int dev, int srate, int chans, int format, int size) {return(-1);}
static void describe_audio_state_1(void) {pprint("audio stubbed out");}
int mus_audio_read(int line, char *buf, int bytes) {return(-1);}
int mus_audio_mixer_read(int dev, int field, int chan, float *val) {return(-1);}
int mus_audio_mixer_write(int dev, int field, int chan, float *val) {return(-1);}
void mus_audio_save(void) {}
void mus_audio_restore(void) {}
int mus_audio_initialize(void) {return(-1);}
char *mus_audio_error_name(int err) {return(audio_error_name_1(err));}
int mus_audio_systems(void) {return(1);}
char *mus_audio_system_name(int system) {return("OS2");}
void mus_audio_set_oss_buffers(int num,int size) {}
char *mus_audio_moniker(void) {return("OS2 audio");}

#endif


/* ------------------------------- Ardour Audio----------------------------------- */

#ifdef ARDOUR_AUDIO
#define AUDIO_OK

extern int ardour_write_to_channel (int channel, char *buf, size_t bytes);

void describe_audio_state_1(void)
{
}

int mus_audio_initialize ()
{
	return 0;
}

char *mus_audio_error_name(int err)
{
	return "ardour audio error";
}

int mus_audio_open_output(int dev, int srate, int chans, int format, int size)
{
	return 0;
}

int mus_audio_open_input(int dev, int srate, int chans, int format, int size)
{
	return 0;
}

int mus_audio_write(int line, char *buf, int bytes)
{
	return bytes;
}

int mus_audio_write_channel(int line, char *buf, int bytes, int channel)
{
	ardour_write_to_channel (channel, buf, bytes);
	return bytes;
}

int mus_audio_flush(int line) 
{
	return 0;
}

int mus_audio_close(int line)
{
	return 0;
}

int mus_audio_read(int line, char *buf, int bytes)
{
	memset (buf, 0, bytes);
	return bytes;
}

int mus_audio_mixer_read(int dev, int field, int chan, float *val)
{
	*val = 0;
	return 0;
}

int mus_audio_mixer_write(int dev, int field, int chan, float *val)
{
	return 0;
}

void mus_audio_save(void)
{
}

void mus_audio_restore(void)
{
}

int mus_audio_systems(void) 
{
	return 1;
}

char *mus_audio_system_name(int system) 
{
	return "ardour audio";
}

char *mus_audio_moniker(void) 
{
	return "ardour audio";
}

#endif /*ARDOUR_AUDIO*/
 

/* and DEC using MMSYSTEM?? #include <mme/mmsystem.h> -- see xanim */

/* ------------------------------- STUBS ----------------------------------------- */

#ifndef AUDIO_OK
static void describe_audio_state_1(void) {pprint("audio stubbed out");}
int mus_audio_open_output(int dev, int srate, int chans, int format, int size) {return(-1);}
int mus_audio_open_input(int dev, int srate, int chans, int format, int size) {return(-1);}
int mus_audio_write(int line, char *buf, int bytes) {return(-1);}
int mus_audio_close(int line) {return(-1);}
int mus_audio_read(int line, char *buf, int bytes) {return(-1);}
int mus_audio_mixer_read(int dev, int field, int chan, float *val) {return(-1);}
int mus_audio_mixer_write(int dev, int field, int chan, float *val) {return(-1);}
void mus_audio_save(void) {}
void mus_audio_restore(void) {}
int mus_audio_initialize(void) {return(-1);}
char *mus_audio_error_name(int err) {return(audio_error_name_1(err));}
int mus_audio_systems(void) {return(0);}
char *mus_audio_system_name(int system) {return("unknown");}
void mus_audio_set_oss_buffers(int num,int size) {}
char *mus_audio_moniker(void) {return("unknown audio");}
#endif



static char *save_it = NULL;
static int print_it = 1;
static int save_it_len = 0;
static int save_it_loc = 0;

static void check_save_it(int loc)
{
#ifdef MACOS
  char *ptr;
  int k;
#endif
  if (loc >= save_it_len)
    {
      save_it_len += 1024;
#ifdef MACOS
      ptr = (char *)CALLOC(save_it_len,sizeof(char));
      if (save_it)
	{
	  for (k=0;k<save_it_loc;k++) ptr[k] = save_it[k];
	  FREE(save_it);
	}
      save_it = ptr;
#else
      save_it = (char *)REALLOC(save_it,save_it_len * sizeof(char));
#endif
    }
}

static void pprint(char *str)
{
  int i,len;
  if ((str) && (*str))
    {
      if ((print_it) || (!(save_it)))
	{
	  mus_error(MUS_AUDIO_NO_ERROR,str);
	}
      else
	{
	  len = strlen(str);
	  for (i=0;i<len;i++,save_it_loc++)
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
  AUDIO_ERROR = MUS_AUDIO_NO_ERROR;
  if (!(save_it)) 
    {
      save_it_len = 1024;
      save_it = (char *)CALLOC(save_it_len,sizeof(char));
    }
  save_it_loc = 0;
  print_it = 0;
  describe_audio_state_1();
  return(save_it);
}

void mus_audio_describe(void)
{
  AUDIO_ERROR = MUS_AUDIO_NO_ERROR;
  print_it = 1;
  describe_audio_state_1();
}

#ifdef CLM
void reset_audio_c (void)
{
  audio_initialized = 0;
  AUDIO_ERROR = MUS_AUDIO_NO_ERROR;
  save_it = NULL;
  version_name = NULL;
#ifdef SUN
  sun_vol_name = NULL;
#endif
  save_it_len = 0;
  strbuf = NULL;
#ifdef MCL_PPC
  reset_db();
#endif
}
#endif
