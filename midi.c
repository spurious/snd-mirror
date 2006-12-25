/* (simple, low level) midi support, work in progress... */
/*   based on code of Scott Wilson and Craig Sapp */

/*
 * int mus_midi_open_read(const char *name)
 * int mus_midi_open_write(const char *name)
 * int mus_midi_close(int line)
 * int mus_midi_read(int line, unsigned char *buffer, int bytes)
 * int mus_midi_write(int line, unsigned char *buffer, int bytes)
 * char *mus_midi_device_name(int sysdev)  [don't free result]
 * char *mus_midi_describe(void)           [free result]
 *
 *    not implemented (not sure it's needed): int mus_midi_flush(line)
 *
 * currently OSX, ALSA, SGI (both libraries), OSS
 */

#include <mus-config.h>

#include <stdio.h>
#if HAVE_FCNTL_H
  #include <fcntl.h>
#endif
#include <errno.h>
#include <stdlib.h>
#if (defined(HAVE_LIBC_H) && (!defined(HAVE_UNISTD_H)))
  #include <libc.h>
#else
  #if (!(defined(_MSC_VER)))
    #include <unistd.h>
  #endif
#endif
#if HAVE_STRING_H
  #include <string.h>
#endif

#ifndef NO_SNDLIB
  #include "_sndlib.h"
#else
  int mus_midi_open_read(const char *name);
  int mus_midi_open_write(const char *name);
  int mus_midi_close(int line);
  int mus_midi_read(int line, unsigned char *buffer, int bytes);
  int mus_midi_write(int line, unsigned char *buffer, int bytes);
  char *mus_midi_device_name(int sysdev);
  char *mus_midi_describe(void);
  #define MUS_AUDIO_SYSTEM(n) (((n) >> 16) & 0xffff)
  #define MUS_AUDIO_DEVICE(n) ((n) & 0xffff)
  #if HAVE_EXTENSION_LANGUAGE
    void mus_midi_init(void);
  #endif
  #define CALLOC(a, b)  calloc((size_t)(a), (size_t)(b))
  #define MALLOC(a)     malloc((size_t)(a))
  #define FREE(a)       free(a)
  #define REALLOC(a, b) realloc(a, (size_t)(b))
#endif


/* ---------------- ALSA ---------------- */

#if HAVE_ALSA

#include <sys/ioctl.h>

#define ALSA_PCM_OLD_HW_PARAMS_API
#define ALSA_PCM_OLD_SW_PARAMS_API

#if HAVE_ALSA_ASOUNDLIB_H
  #include <alsa/asoundlib.h>
#else
  #include <sys/asoundlib.h>
#endif

#ifdef SND_RAWMIDI_NONBLOCK

#define MIDI_OK
#define DEV_BUFSIZE 1024  /* what is this for? */
#define MIDI_READ 0     /* do we need MIDI_READ_WRITE? */
#define MIDI_WRITE 1

static snd_rawmidi_t **midi_lines = NULL;
static snd_rawmidi_params_t **midi_params = NULL;
static char **midi_names = NULL;
static int *midi_directions = NULL;
static int midis = 0;

static int new_midi_line(const char *name, snd_rawmidi_t *line, snd_rawmidi_params_t *params, int input)
{
  int i, loc = -1;
  if (midis == 0)
    {
      midis = 4;
      midi_lines = (snd_rawmidi_t **)CALLOC(midis, sizeof(snd_rawmidi_t *));
      midi_params = (snd_rawmidi_params_t **)CALLOC(midis, sizeof(snd_rawmidi_params_t *));
      midi_names = (char **)CALLOC(midis, sizeof(char *));
      midi_directions = (int *)CALLOC(midis, sizeof(int));
      loc = 0;
    }
  else
    {
      for (i = 0; i < midis; i++)
	if (midi_lines[i] == NULL)
	  {
	    loc = i;
	    break;
	  }
      if (loc == -1)
	{
	  loc = midis;
	  midis += 4;
	  midi_lines = (snd_rawmidi_t **)REALLOC(midi_lines, midis * sizeof(snd_rawmidi_t *));
	  midi_params = (snd_rawmidi_params_t **)REALLOC(midi_params, midis * sizeof(snd_rawmidi_params_t *));
	  midi_names = (char **)REALLOC(midi_names, midis * sizeof(char *));
	  midi_directions = (int *)REALLOC(midi_directions, midis * sizeof(int));
	}
    }
  midi_lines[loc] = line;
  midi_params[loc] = params;
  midi_names[loc] = strdup(name);
  midi_directions[loc] = input;
  return(loc);
}

static int midi_open(const char *name, int input)
{
  int err; 
  snd_rawmidi_t *line;
  snd_rawmidi_params_t *params;
  
  if (input == MIDI_READ)
    err = snd_rawmidi_open(&line, NULL, name, SND_RAWMIDI_NONBLOCK);
  else err = snd_rawmidi_open(NULL, &line, name, SND_RAWMIDI_NONBLOCK);
  if (err)
    {
#ifdef NO_SNDLIB
      fprintf(stderr, "can't open %s: %s\n", name, strerror(err));
#else
      return(mus_error(MUS_MIDI_OPEN_ERROR, "can't open %s: %s", name, strerror(err)));
#endif
      return(-1);
    }
  snd_rawmidi_params_malloc(&params);
  err = snd_rawmidi_params_set_buffer_size(line, params, DEV_BUFSIZE);
  if (err)
    {
      snd_rawmidi_params_free(params);
      snd_rawmidi_close(line);
#ifdef NO_SNDLIB
      fprintf(stderr, "can't set %s buffer size to %d: %s\n", name, DEV_BUFSIZE, strerror(err));
#else
      return(mus_error(MUS_MIDI_MISC_ERROR, "can't set %s buffer size to %d: %s", name, DEV_BUFSIZE, strerror(err)));
#endif
      return(-1);
    }
  return(new_midi_line(name, line, params, input));
}

/* name is apparently of the form "hw:%d,%d" card dev  or  "/dev/snd/midiC0D0"  */
int mus_midi_open_read(const char *name)  {return(midi_open(name, MIDI_READ));}
int mus_midi_open_write(const char *name) {return(midi_open(name, MIDI_WRITE));}

int mus_midi_close(int line)
{
  int err = 0;
  if ((line >= 0) && (line < midis))
    {
      free(midi_names[line]);
      midi_names[line] = NULL;
      snd_rawmidi_params_free(midi_params[line]);
      midi_params[line] = NULL;
      err = snd_rawmidi_close(midi_lines[line]);
      midi_lines[line] = NULL;
    }
  if (err)
    {
#ifdef NO_SNDLIB
      fprintf(stderr, "can't close %s: %s\n", midi_names[line], strerror(err));
#else
      return(mus_error(MUS_MIDI_CLOSE_ERROR, "can't close %s: %s", midi_names[line], strerror(err)));
#endif
      return(-1);
    }
  return(0);
}

int mus_midi_read(int line, unsigned char *buffer, int bytes)
{
  if ((line < 0) || (line >= midis)) return(-1);
  if (midi_directions[line] == MIDI_READ)
    return(snd_rawmidi_read(midi_lines[line], buffer, bytes));
#ifdef NO_SNDLIB
  fprintf(stderr, "can't read from output %s\n", midi_names[line]);
#else
  return(mus_error(MUS_MIDI_READ_ERROR, "can't read from output %s", midi_names[line]));
#endif
  return(-1);
}

int mus_midi_write(int line, unsigned char *buffer, int bytes)
{
  if ((line < 0) || (line >= midis)) return(-1);
  if (midi_directions[line] == MIDI_WRITE)
    return(snd_rawmidi_write(midi_lines[line], buffer, bytes));
#ifdef NO_SNDLIB
  fprintf(stderr, "can't write to input %s\n", midi_names[line]);
#else
  return(mus_error(MUS_MIDI_WRITE_ERROR, "can't write to input %s", midi_names[line]));
#endif
  return(-1);
}

static char devname[64];
char *mus_midi_device_name(int sysdev)
{
  sprintf(devname, "hw:%d,%d", MUS_AUDIO_SYSTEM(sysdev), MUS_AUDIO_DEVICE(sysdev));
  return(devname);
}

/* snd_rawmidi_drop to clear or snd_rawmidi_drain|flush_output */

char *mus_midi_describe(void) 
{
  int i, err;
  snd_rawmidi_t *line;
  snd_rawmidi_info_t *info;
  char *buf = NULL;
  char one[256];
  snd_rawmidi_info_malloc(&info);
  buf = (char *)CALLOC(1024, sizeof(char));
  for (i = 0; i < 8; i++)
    {
      /* presumably this should use the card|device from the info struct, not a blind loop */
      err = snd_rawmidi_open(&line, NULL, mus_midi_device_name(i << 16), SND_RAWMIDI_NONBLOCK); /* do the "devices" matter here? */
      if (err < 0) continue;
      err = snd_rawmidi_info(line, info);
      if (err < 0) break;
      sprintf(one, "%s: card: %d, device: %d, stream: %d, flags: %x, id: %s, name: %s[%s; %d]\n",
	      mus_midi_device_name(i << 16),
	      snd_rawmidi_info_get_card(info),    /* card number */
	      snd_rawmidi_info_get_device(info),  /* device number */
	      snd_rawmidi_info_get_stream(info),  /* snd_rawmidi_stream_t */
	      snd_rawmidi_info_get_flags(info),   /* 1=out 2=in 4=dupex */
	      snd_rawmidi_info_get_id(info),      /* hardware id (out in?) */
	      snd_rawmidi_info_get_name(info),
	      snd_rawmidi_info_get_subdevice_name(info),
	      snd_rawmidi_info_get_subdevices_count(info));
      strcat(buf, one);
    }
  snd_rawmidi_info_free(info);
  return(buf);
}

#endif
#endif


/* ---------------- SGI ---------------- */
#ifdef MUS_SGI

#include <dmedia/midi.h>

#define MIDI_OK
#define MIDI_READ 0
#define MIDI_WRITE 1

static MDport *midi_lines = NULL;
static int *midi_directions = NULL;
static char **midi_names = NULL;
static int midis = 0;
static int midi_initialized = 0;
static int midi_ports = 0;

static void mus_midi_initialize(void)
{
  if (midi_initialized == 0)
    {
      midi_ports = mdInit();
      if (midi_ports == -1)
#ifdef NO_SNDLIB
	fprintf(stderr, "startmidi not called?");
#else
      mus_error(MUS_MIDI_INIT_ERROR, "startmidi not called?");
#endif
      midi_initialized = 1;
    }
}

static int new_midi_line(const char *name, MDport line, int input)
{
  int i, loc = -1;
  if (midis == 0)
    {
      midis = 4;
      midi_lines = (MDport *)CALLOC(midis, sizeof(MDport));
      midi_names = (char **)CALLOC(midis, sizeof(char *));
      midi_directions = (int *)CALLOC(midis, sizeof(int));
      loc = 0;
    }
  else
    {
      for (i = 0; i < midis; i++)
	if (midi_lines[i] == NULL)
	  {
	    loc = i;
	    break;
	  }
      if (loc == -1)
	{
	  loc = midis;
	  midis += 4;
	  midi_lines = (MDport *)REALLOC(midi_lines, midis * sizeof(MDport));
	  midi_names = (char **)REALLOC(midi_names, midis * sizeof(char *));
	  midi_directions = (int *)REALLOC(midi_directions, midis * sizeof(int));
	}
    }
  midi_lines[loc] = line;
  midi_names[loc] = strdup(name);
  midi_directions[loc] = input;
  return(loc);
}

int midi_open(const char *name, int input)
{
  MDport md;
  mus_midi_initialize();
  if (input == MIDI_READ)
    md = mdOpenInPort((char *)name);
  else md = mdOpenOutPort((char *)name);
  if (md == NULL)
    {
#ifdef NO_SNDLIB
      fprintf(stderr, "can't open %s\n", name);
      return(-1);
#else
      return(mus_error(MUS_MIDI_OPEN_ERROR, "can't open %s", name));
#endif
    }
  mdSetStampMode(md, MD_NOSTAMP);
  return(new_midi_line(name, md, input));
}

int mus_midi_open_read(const char *name)  {return(midi_open(name, MIDI_READ));}
int mus_midi_open_write(const char *name) {return(midi_open(name, MIDI_WRITE));}

int mus_midi_close(int line) 
{
  if ((line < 0) || (line >= midis)) return(-1);
  mdClosePort(midi_lines[line]); /* this frees? */
  midi_lines[line] = NULL;
  return(0);
}

int mus_midi_read(int line, unsigned char *buffer, int bytes)
{
  MDevent mdv;
  int i;
  if ((line < 0) || (line >= midis)) return(-1);
  if (midi_directions[line] == MIDI_READ)
    {
      for (i = 0; i < bytes; i += 3)
	{
	  mdReceive(midi_lines[line], &mdv, 1);
	  buffer[i] = mdv.msg[0];
	  buffer[i + 1] = mdv.msg[1];
	  buffer[i + 2] = mdv.msg[2];
	}
      return(bytes);
    }
#ifdef NO_SNDLIB
  fprintf(stderr, "can't read from output %s\n", midi_names[line]);
  return(-1);
#else
  return(mus_error(MUS_MIDI_READ_ERROR, "can't read from output %s", midi_names[line]));
#endif
}

int mus_midi_write(int line, unsigned char *buffer, int bytes)
{
  int i;
  MDevent mdv;
  if ((line < 0) || (line >= midis)) return(-1);
  if (midi_directions[line] == MIDI_WRITE)
    {
      /* no sysex for now */
      mdv.sysexmsg = NULL;
      mdv.msglen = 0;

      for (i = 0; i < bytes; i += 3)
	{
	  mdv.msg[0] = buffer[i];
	  mdv.msg[1] = buffer[i + 1];
	  mdv.msg[2] = buffer[i + 2];
	  mdSend(midi_lines[line], &mdv, 1);
	}
      return(bytes);
    }
#ifdef NO_SNDLIB
  fprintf(stderr, "can't write to input %s\n", midi_names[line]);
  return(-1);
#else
  return(mus_error(MUS_MIDI_WRITE_ERROR, "can't write to input %s", midi_names[line]));
#endif
}

char *mus_midi_device_name(int sysdev)
{
  mus_midi_initialize();
  if (midi_ports > 0)
    return(mdGetName(MUS_AUDIO_DEVICE(sysdev)));
  return(NULL);
}

char *mus_midi_describe(void)
{
  int i;
  char *buf = NULL;
  char name[64];
  mus_midi_initialize(); 
  if (midi_ports > 0)
    {
      buf = (char *)CALLOC(midi_ports * 64, sizeof(char));
      for (i = 0; i < midi_ports; i++)
	{
	  sprintf(name, "%s\n", mdGetName(i));
	  strcat(buf, name);
	}
    }
  else sprintf(buf, "no midi");
  return(buf);
}

#endif


/* ---------------- OSS ---------------- */
#if HAVE_OSS && (!HAVE_ALSA)
#define MIDI_OK

#include <sys/ioctl.h>

#if (MUS_HAVE_USR_LIB_OSS)
  #include "/usr/lib/oss/include/sys/soundcard.h"
#else
  #if (MUS_HAVE_USR_LOCAL_LIB_OSS)
    #include "/usr/local/lib/oss/include/sys/soundcard.h"
  #else
    #if (MUS_HAVE_OPT_OSS)
      #include "/opt/oss/include/sys/soundcard.h"
    #else
      #if (MUS_HAVE_VAR_LIB_OSS)
        #include "/var/lib/oss/include/sys/soundcard.h"
      #else
        #if defined(HAVE_SYS_SOUNDCARD_H) || defined(MUS_LINUX)
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

int mus_midi_open_read(const char *name) {return(open(name, O_RDONLY, O_NONBLOCK));}  /* name should be "/dev/sequencer" */
int mus_midi_open_write(const char *name) {return(open(name, O_RDWR, O_NONBLOCK));}   /* O_WRONLY? */
int mus_midi_close(int line) {return(close(line));}
int mus_midi_read(int line, unsigned char *buffer, int bytes) {return(read(line, buffer, bytes));}
int mus_midi_write(int line, unsigned char *buffer, int bytes) {return(write(line, buffer, bytes));}
char *mus_midi_device_name(int sysdev) {return("/dev/sequencer");}

char *mus_midi_describe(void)
{
  /* taken from audio.c 3067 */
  int fd, status, i, numdevs;
  char *buf = NULL;
  char info[256];
  struct midi_info minfo;
  fd = open("/dev/sequencer", O_RDWR, 0);
  if (fd == -1) fd = open("/dev/music", O_RDONLY, 0); 
  if (fd != -1)
    {
      status = ioctl(fd, SNDCTL_SEQ_NRMIDIS, &numdevs);
      if (status == -1) 
	{ 
	  /* not strdup for memcheck consistency */
	  char *newstr;
	  close(fd);
	  newstr = (char *)CALLOC(16, sizeof(char));
	  strcpy(newstr, "no midi");
	  return(newstr);
	}
      else
	{
	  buf = (char *)CALLOC(1024, sizeof(char));
	  sprintf(buf, "%d midi device%s installed\n", numdevs, (numdevs == 1) ? "" : "s"); 
	  for (i = 0; i < numdevs; i++)
	    {
	      minfo.device = i;
	      status = ioctl(fd, SNDCTL_MIDI_INFO, &minfo);
	      if (status != -1)
		{
		  sprintf(info, "  dev %d: %s\n", i, minfo.name); 
		  strcat(buf, info);
		}
	    }
	  close(fd);
	}
    }
  return(buf);
}

#endif


/* ---------------- Mac OSX ---------------- */

#ifdef MUS_MAC_OSX
#define MIDI_OK
#include <CoreMIDI/MIDIServices.h>

/* for now, just one in and one out */

char *mus_midi_describe(void)
{
  int i, n;
  MIDIDeviceRef dev;
  CFStringRef cs1, cs2, cs3;
  char name[64], mfg[64], model[64], all[192];
  char *buf;
  n = MIDIGetNumberOfDevices();
  if (n <= 0)
    buf = (char *)CALLOC(192, sizeof(char));
  else buf = (char *)CALLOC(n * 192, sizeof(char));
  if (n <= 0)
    sprintf(buf, "no midi");
  else
    {
      for (i = 0; i < n; i++) 
	{
	  dev = MIDIGetDevice(i);
	  MIDIObjectGetStringProperty(dev, kMIDIPropertyName, &cs1);
	  MIDIObjectGetStringProperty(dev, kMIDIPropertyManufacturer, &cs2);
	  MIDIObjectGetStringProperty(dev, kMIDIPropertyModel, &cs3);
	  CFStringGetCString(cs1, name, sizeof(name), 0);
	  CFStringGetCString(cs2, mfg, sizeof(mfg), 0);
	  CFStringGetCString(cs3, model, sizeof(model), 0);
	  CFRelease(cs1);
	  CFRelease(cs2);
	  CFRelease(cs3);
	  sprintf(all, "%s (%s): %s\n", name, mfg, model);
	  strcat(buf, all);
	}
    }
  return(buf);
}

#define BUFFER_SIZE 1024
static MIDIClientRef us = NULL;
static MIDIPortRef inp = NULL, outp = NULL;
static int reader = 0, writer = 0;
static unsigned char *buffer = NULL;

static void init_midi(void)
{
  if (buffer == NULL)
    {
      buffer = (unsigned char *)CALLOC(BUFFER_SIZE, sizeof(unsigned char));
      reader = 0;
      writer = 0;
    }
}

static void midi_read_callback(const MIDIPacketList *list, void *ref, void *con)
{
  int i, j;
  MIDIPacket *pk;
  pk = (MIDIPacket *)(list->packet);
  for (i = 0; i < list->numPackets; i++)
    {
      for (j = 0; j < pk->length; j++)
	{
	  buffer[writer++] = pk->data[j];
	  if (writer >= BUFFER_SIZE)
	    writer = 0;
	}
      pk = MIDIPacketNext(pk);
    }
}

int mus_midi_read(int line, unsigned char *buf, int bytes)
{
  int i;
  if ((line < 0) || (buffer == NULL) || (inp == NULL)) return(-1);
  for (i = 0; i < bytes; i++)
    {
      if (reader == writer) return(i);
      buf[i] = buffer[reader++];
      if (reader >= BUFFER_SIZE)
	reader = 0;
    }
  return(bytes);
}

static Byte bb[1024];
int mus_midi_write(int line, unsigned char *buffer, int bytes)
{
  MIDIPacketList *list = (MIDIPacketList *)bb;
  MIDIPacket *pk;
  if ((line < 1) || (buffer == NULL) || (outp == NULL)) return(-1);
  /* create the packetlist and packet */
  pk = MIDIPacketListInit(list);
  MIDIPacketListAdd(list, sizeof(bb), pk, 0, bytes, (Byte *)buffer);  /* 0 is the time stamp = now? */
  MIDISend(outp, MIDIGetDestination(0), list);
  return(0);
}

int mus_midi_open_read(const char *name)
{
  init_midi();
  if (us == NULL) 
    MIDIClientCreate(CFSTR("MIDI Read"), NULL, NULL, &us);
  if (inp == NULL)
    MIDIInputPortCreate(us, CFSTR("Input port"), midi_read_callback, NULL, &inp);
  MIDIPortConnectSource(inp, MIDIGetSource(0), NULL);
  return(0);
}

int mus_midi_open_write(const char *name)
{
  init_midi();
  if (us == NULL) 
    MIDIClientCreate(CFSTR("MIDI Write"), NULL, NULL, &us);
  if (outp == NULL)
    MIDIOutputPortCreate(us, CFSTR("Output port"), &outp);
  return(1);
}

int mus_midi_close(int line)
{
  return(0);
}

char *mus_midi_device_name(int sysdev) {return("none");}

#endif



/* ---------------- stubs ---------------- */
#ifndef MIDI_OK
int mus_midi_open_read(const char *name) {return(-1);}
int mus_midi_open_write(const char *name) {return(-1);}
int mus_midi_close(int line) {return(-1);}
int mus_midi_read(int line, unsigned char *buffer, int bytes) {return(-1);}
int mus_midi_write(int line, unsigned char *buffer, int bytes) {return(-1);}
char *mus_midi_device_name(int sysdev) {return("none");} /* result should not be freed by caller */
char *mus_midi_describe(void) {return(NULL);}            /* result should be freed by caller (if not NULL) */
#endif


/* ---------------- XEN ---------------- */
/* tie foregoing into Xen */

#if HAVE_EXTENSION_LANGUAGE

#include "xen.h"

#define S_mus_midi_open_read   "mus-midi-open-read"
#define S_mus_midi_open_write  "mus-midi-open-write"
#define S_mus_midi_read        "mus-midi-read"
#define S_mus_midi_write       "mus-midi-write"
#define S_mus_midi_close       "mus-midi-close"
#define S_mus_midi_device_name "mus-midi-device-name"
#define S_mus_midi_describe    "mus-midi-describe"

static XEN g_mus_midi_open_read(XEN name)
{
  #define H_mus_midi_open_read "(" S_mus_midi_open_read " name): opens midi input port, returns int id"
  XEN_ASSERT_TYPE(XEN_STRING_P(name), name, XEN_ONLY_ARG, S_mus_midi_open_read, "a string");
  return(C_TO_XEN_INT(mus_midi_open_read(XEN_TO_C_STRING(name))));
}

static XEN g_mus_midi_open_write(XEN name)
{
  #define H_mus_midi_open_write "(" S_mus_midi_open_write " name): opens midi output port, returns int id"
  XEN_ASSERT_TYPE(XEN_STRING_P(name), name, XEN_ONLY_ARG, S_mus_midi_open_write, "a string");
  return(C_TO_XEN_INT(mus_midi_open_write(XEN_TO_C_STRING(name))));
}

static XEN g_mus_midi_close(XEN line)
{
  #define H_mus_midi_close "(" S_mus_midi_close " line): closes midi port"
  XEN_ASSERT_TYPE(XEN_INTEGER_P(line), line, XEN_ONLY_ARG, S_mus_midi_close, "an integer");
  return(C_TO_XEN_INT(mus_midi_close(XEN_TO_C_INT(line))));
}

static XEN g_mus_midi_read(XEN line, XEN bytes) /* returns list of midi bytes */
{
  #define H_mus_midi_read "(" S_mus_midi_read " line bytes): reads bytes from midi port, returns list"
  int i, len, err;
  unsigned char *buf;
  XEN lst = XEN_EMPTY_LIST;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(line), line, XEN_ARG_1, S_mus_midi_read, "an integer");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(bytes), bytes, XEN_ARG_2, S_mus_midi_read, "an integer");
  len = XEN_TO_C_INT(bytes);
  buf = (unsigned char *)CALLOC(len, sizeof(unsigned char));
  err = mus_midi_read(XEN_TO_C_INT(line), buf, len);
  if (err == -1)
    {
      FREE(buf);
      return(XEN_FALSE);
    }
  for (i = err; i >= 0; i--)
    lst = XEN_CONS(C_TO_XEN_INT((int)buf[i]), lst);
  FREE(buf);
  return(lst);
}

static XEN g_mus_midi_write(XEN line, XEN buffer)
{
  #define H_mus_midi_write "(" S_mus_midi_write " line byte-list): sends byte-list to midi port"
  int i, len, err;
  unsigned char *buf;
  XEN lst;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(line), line, XEN_ARG_1, S_mus_midi_write, "an integer");
  XEN_ASSERT_TYPE(XEN_LIST_P(buffer), buffer, XEN_ARG_2, S_mus_midi_write, "a list");
  len = XEN_LIST_LENGTH(buffer);
  buf = (unsigned char *)CALLOC(len, sizeof(unsigned char));
  for (i = 0, lst = XEN_COPY_ARG(buffer); i < len; i++, lst = XEN_CDR(lst))
    buf[i] = (unsigned char)(XEN_TO_C_INT(XEN_CAR(lst)));
  err = mus_midi_write(XEN_TO_C_INT(line), buf, len);
  FREE(buf);
  if (err == -1)
    return(XEN_FALSE);
  return(C_TO_XEN_INT(err));
}

static XEN g_mus_midi_device_name(XEN dev)
{
  /* sndlib style sys|dev packing, dev optional */
  #define H_mus_midi_device_name "(" S_mus_midi_device_name " :optional (sys-dev 0)): returns a name suitable for " S_mus_midi_open_read " and write"
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(dev), dev, XEN_ONLY_ARG, S_mus_midi_device_name, "and integer");
  return(C_TO_XEN_STRING(mus_midi_device_name(XEN_TO_C_INT_OR_ELSE(dev, 0))));
}

static XEN g_mus_midi_describe(void)
{
  #define H_mus_midi_describe "(" S_mus_midi_describe "): returns a description of the midi hardware"
  char *str;
  XEN res = XEN_FALSE;
  str = mus_midi_describe();
  if (str)
    {
      res = C_TO_XEN_STRING(str);
      FREE(str);
    }
  return(res);
}

#ifdef XEN_ARGIFY_1
  XEN_NARGIFY_1(g_mus_midi_open_read_w, g_mus_midi_open_read)
  XEN_NARGIFY_1(g_mus_midi_open_write_w, g_mus_midi_open_write)
  XEN_NARGIFY_2(g_mus_midi_read_w, g_mus_midi_read)
  XEN_NARGIFY_2(g_mus_midi_write_w, g_mus_midi_write)
  XEN_NARGIFY_1(g_mus_midi_close_w, g_mus_midi_close)
  XEN_ARGIFY_1(g_mus_midi_device_name_w, g_mus_midi_device_name)
  XEN_NARGIFY_0(g_mus_midi_describe_w, g_mus_midi_describe)
#else
  #define g_mus_midi_open_read_w g_mus_midi_open_read
  #define g_mus_midi_open_write_w g_mus_midi_open_write
  #define g_mus_midi_read_w g_mus_midi_read
  #define g_mus_midi_write_w g_mus_midi_write
  #define g_mus_midi_close_w g_mus_midi_close
  #define g_mus_midi_device_name_w g_mus_midi_device_name
  #define g_mus_midi_describe_w g_mus_midi_describe
#endif

#if WITH_MODULES
static void midi_init(void *ignore)
#else
void mus_midi_init(void)
#endif
{
  XEN_DEFINE_PROCEDURE(S_mus_midi_open_read,   g_mus_midi_open_read_w, 1, 0, 0,   H_mus_midi_open_read);
  XEN_DEFINE_PROCEDURE(S_mus_midi_open_write,  g_mus_midi_open_write_w, 1, 0, 0,  H_mus_midi_open_write);
  XEN_DEFINE_PROCEDURE(S_mus_midi_close,       g_mus_midi_close_w, 1, 0, 0,       H_mus_midi_close);
  XEN_DEFINE_PROCEDURE(S_mus_midi_read,        g_mus_midi_read_w, 2, 0, 0,        H_mus_midi_read);
  XEN_DEFINE_PROCEDURE(S_mus_midi_write,       g_mus_midi_write_w, 2, 0, 0,       H_mus_midi_write);
  XEN_DEFINE_PROCEDURE(S_mus_midi_device_name, g_mus_midi_device_name_w, 0, 1, 0, H_mus_midi_device_name);
  XEN_DEFINE_PROCEDURE(S_mus_midi_describe,    g_mus_midi_describe_w, 0, 0, 0,    H_mus_midi_describe);

#if WITH_MODULES
  scm_c_export(S_mus_midi_open_read,
	       S_mus_midi_open_write,
	       S_mus_midi_close,
	       S_mus_midi_read,
	       S_mus_midi_write,
	       S_mus_midi_device_name,
	       S_mus_midi_describe,
	       NULL);
#endif
}

#if WITH_MODULES
void mus_midi_init(void)
{
  scm_c_define_module("snd sndlib", midi_init, NULL);  
}
#endif
#endif
