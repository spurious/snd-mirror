/* sndplay plays sounds */

/* TODO: make it possible to set the output device (and tie into CLM) */

#if defined(HAVE_CONFIG_H)
  #include "config.h"
#endif

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#if (defined(NEXT) || (defined(HAVE_LIBC_H) && (!defined(HAVE_UNISTD_H))))
  #include <libc.h>
#else
  #if (!(defined(_MSC_VER))) && (!(defined(MPW_C)))
    #include <unistd.h>
  #endif
  #include <string.h>
#endif
#include <errno.h>

#include "sndlib.h"

#if MACOS
  #include <console.h>
#endif

#ifdef NEXT
  #define BUFFER_SIZE 1024
#else
  #define BUFFER_SIZE 4096
#endif

#if defined(LINUX) || defined(__bsdi__)
static char x_string[2] = {'x','\0'};
static void set_buffers(char *bufs)
{
  char *arg;
  int a, b;
  arg = strtok(bufs, x_string);
  a = atoi(arg);
  arg = strtok(NULL, x_string);
  b = atoi(arg);
  mus_audio_set_oss_buffers(a, b);
}
#endif

/* special case multicard quad code split out for clarity (it could be folded into the main branches) */

#ifdef DEBUG_MEMORY
void *mem_calloc(size_t len, size_t size, const char *func, const char *file, int line) {return(calloc(len, size));}
void *mem_malloc(size_t len, const char *func, const char *file, int line) {return(malloc(len));}
void mem_free(void *ptr, const char *func, const char *file, int line) {free(ptr);}
void *mem_realloc(void *ptr, size_t size, const char *func, const char *file, int line) {return(realloc(ptr, size));}
#endif

/* 22-Nov-00: moved alsa support to separate block */

#if (!HAVE_ALSA)
int main(int argc, char *argv[])
{
  int fd, afd, i, j, n, k, chans, srate, frames, outbytes;
  MUS_SAMPLE_TYPE **bufs;
  short *obuf;
  float val[1];
  int use_multi_card_code = 0;
  int afd0, afd1, buffer_size, curframes;
  MUS_SAMPLE_TYPE **qbufs;
  short *obuf0, *obuf1;
  char *name = NULL;
#if MACOS
  argc = ccommand(&argv);
#endif
  if (argc == 1) 
    {
      printf("usage: sndplay file\n"); 
      exit(0);
    }
  mus_sound_initialize();
#if defined(LINUX) || defined(__bsdi__)
  /* for clisp-based clm use, we need a couple added switches
   * -describe => call mus_audio_describe and exit
   * -buffers axb => set OSS fragment numbers 
   */
  for (i = 1; i < argc; )
    {
      if (strcmp(argv[i], "-describe") == 0) 
	{
	  mus_audio_describe(); 
	  exit(0);
	}
      else 
	if (strcmp(argv[i], "-buffers") == 0) 
	  {
	    set_buffers(argv[i + 1]); 
	    i++;
	  }
      else name = argv[i];
      i++;
    }
#else
  name = argv[1];
#endif
  if (name == NULL) 
    {
      printf("usage: sndplay file\n"); 
      exit(0);
    }

  afd = -1;
  afd0 = -1;
  afd1 = -1;
  if (!(MUS_HEADER_TYPE_OK(mus_sound_header_type(name))))
    {
      fprintf(stderr, "can't play %s (header type: %s?)\n",
	      name,
	      mus_header_type_name(mus_header_type()));
      exit(0);
    }
  if (!(MUS_DATA_FORMAT_OK(mus_sound_data_format(name))))
    {
      fprintf(stderr, "can't play %s (data format: %s (%s)?)\n",
	      name,
	      mus_data_format_name(mus_sound_data_format(name)),
	      mus_header_original_format_name(mus_sound_original_format(name), 
					      mus_sound_header_type(name)));
      exit(0);
    }
  fd = mus_sound_open_input(name);
  if (fd != -1)
    {
      chans = mus_sound_chans(name);
      if (chans > 2)
	{
	  mus_audio_mixer_read(MUS_AUDIO_DEFAULT, MUS_AUDIO_CHANNEL, 0, val);
	  if (val[0] < chans)
	    {
	      if (mus_audio_systems() > 1)
		use_multi_card_code = 1;
	      /* I suppose we could count up all the channels here */
	      else
		{
		  fprintf(stderr, "%s has %d channels, but we can only handle %d\n", name, chans, (int)(val[0]));
		  exit(1);
		}
	    }
	}
      srate = mus_sound_srate(name);
      frames = mus_sound_samples(name)/chans;
      if (!use_multi_card_code)
	{
	  outbytes = BUFFER_SIZE * chans * 2;
	  bufs = (MUS_SAMPLE_TYPE **)CALLOC(chans, sizeof(MUS_SAMPLE_TYPE *));
	  for (i = 0; i < chans; i++) bufs[i] = (MUS_SAMPLE_TYPE *)CALLOC(BUFFER_SIZE, sizeof(MUS_SAMPLE_TYPE));
	  obuf = (short *)CALLOC(BUFFER_SIZE * chans, sizeof(short));
	  /* assume for lafs that our DAC wants 16-bit integers */
	  for (i = 0; i < frames; i+=BUFFER_SIZE)
	    {
	      if ((i+BUFFER_SIZE) <= frames)
		curframes = BUFFER_SIZE;
	      else curframes = frames - i;
	      mus_sound_read(fd, 0, curframes-1, chans, bufs); 
	      /* some systems are happier if we read the file before opening the dac */
	      /* at this point the data is in separate arrays of ints */
	      if (chans == 1)
		{
		  for (k = 0; k < curframes; k++) 
		    obuf[k] = MUS_SAMPLE_TO_SHORT(bufs[0][k]);
		}
	      else
		{
		  if (chans == 2)
		    {
		      for (k = 0, n = 0; k < curframes; k++, n+=2) 
			{
			  obuf[n] = MUS_SAMPLE_TO_SHORT(bufs[0][k]); 
			  obuf[n+1] = MUS_SAMPLE_TO_SHORT(bufs[1][k]);
			}
		    }
		  else
		    {
		      for (k = 0, j = 0; k < curframes; k++, j+=chans)
			{
			  for (n = 0; n < chans; n++) 
			    obuf[j + n] = MUS_SAMPLE_TO_SHORT(bufs[n][k]);
			}
		    }
		}
	      if (afd == -1)
		{
#if defined(LINUX) && defined(PPC)
		  afd = mus_audio_open_output(MUS_AUDIO_DEFAULT, srate, chans, MUS_COMPATIBLE_FORMAT, 0);
#else
		  afd = mus_audio_open_output(MUS_AUDIO_DEFAULT, srate, chans, MUS_COMPATIBLE_FORMAT, outbytes);
#endif
		  if (afd == -1) break;
		}
	      outbytes = curframes * chans * 2;
	      mus_audio_write(afd, (char *)obuf, outbytes);
	    }
	  if (afd != -1) mus_audio_close(afd);
	  mus_sound_close_input(fd);
	  for (i = 0; i < chans; i++) FREE(bufs[i]);
	  FREE(bufs);
	  FREE(obuf);
	}
      else
	{
	  /* code is essentially the same as above, but since this is supposed
	   *   to be a working example of sndlib, I didn't want the basic stuff
	   *   to be complicated by one special case.
	   *
	   * in my test case, I was using a Sound Blaster and an Ensoniq clone.
	   * they had slightly different start-up latencies (not really audible),
	   * and the Ensoniq's dac was running ca. 1 sample per second faster than
	   * the SB's, so by 5 minutes into a sound, the stereo pairs had drifted
	   * .01 seconds apart -- this is probably acceptable in many cases.
	   * In a second test, with two Ensoniq's in one machine, they started
	   * together and drifted apart at about 1 sample per 8 seconds -- about
	   * .001 secs apart after 5 minutes.  ("Ensoniq" was SoundWave Pro PCI
	   * from SIIG Inc -- some sort of clone.)
	   */
	  buffer_size = 256;   /* 128 probably better */
	  outbytes = buffer_size * 2 * 2; 
	  qbufs = (MUS_SAMPLE_TYPE **)CALLOC(chans, sizeof(MUS_SAMPLE_TYPE *));
	  for (i = 0; i < chans; i++) qbufs[i] = (MUS_SAMPLE_TYPE *)CALLOC(buffer_size, sizeof(MUS_SAMPLE_TYPE));
	  obuf0 = (short *)CALLOC(buffer_size * 2, sizeof(short));
	  obuf1 = (short *)CALLOC(buffer_size * 2, sizeof(short));
	  for (i = 0; i < frames; i+=buffer_size)
	    {
	      if ((i+buffer_size) <= frames)
		curframes = buffer_size;
	      else curframes = frames - i;
	      mus_sound_read(fd, 0, curframes - 1, chans, qbufs); 
	      val[0] = 1.0;
	      for (k = 0, n = 0; k < buffer_size; k++, n+=2) 
		{
		  obuf0[n] = MUS_SAMPLE_TO_SHORT(qbufs[0][k]); 
		  obuf0[n+1] = MUS_SAMPLE_TO_SHORT(qbufs[1][k]);
		  obuf1[n] = MUS_SAMPLE_TO_SHORT(qbufs[2][k]); 
		  obuf1[n+1] = MUS_SAMPLE_TO_SHORT(qbufs[3][k]);
		}
	      if (afd0 == -1)
		{
		  afd0 = mus_audio_open_output(MUS_AUDIO_PACK_SYSTEM(0) | MUS_AUDIO_DEFAULT, srate, 2, MUS_COMPATIBLE_FORMAT, outbytes);
		  afd1 = mus_audio_open_output(MUS_AUDIO_PACK_SYSTEM(1) | MUS_AUDIO_DEFAULT, srate, 2, MUS_COMPATIBLE_FORMAT, outbytes);
		  if ((afd0 == -1) || (afd1 == -1)) break;
		}
	      mus_audio_write(afd0, (char *)obuf0, outbytes);
	      mus_audio_write(afd1, (char *)obuf1, outbytes);
	    }
	  mus_audio_close(afd0);
	  mus_audio_close(afd1);
	  mus_sound_close_input(fd);
	  for (i = 0; i < chans; i++) FREE(qbufs[i]);
	  FREE(qbufs);
	  FREE(obuf0);
	  FREE(obuf1);
	}
    }
  return(0);
}

#else

/* HAVE_ALSA here
   there should be a command line argument to control this,
   set to 0 to enable using more than one device. */

static int use_one_device = 1;

#define MAX_SLOTS 64

static int mus_audio_compatible_format(int dev) 
{
#ifndef PPC
  int err, i;
  float val[32];
  int ival[32];
  err = mus_audio_mixer_read(dev, MUS_AUDIO_FORMAT, 32, val);
  if (err != MUS_ERROR)
    {
      for (i = 0; i <=(int)(val[0]); i++) ival[i] = (int)(val[i]);
      /*          ^ this cast is vital!  Memory clobbered otherwise in LinuxPPC */
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

int main(int argc, char *argv[])
{
  int fd, i, chans, srate, frames;
  MUS_SAMPLE_TYPE **read_bufs;
  int afd[MAX_SLOTS];
  short *out_buf[MAX_SLOTS];
  float val[MAX_SLOTS];
  int ival[MAX_SLOTS];
  int afd0, afd1;
  char *name;
  int base, curframes;
  int allocated;
  int out_devs[MAX_SLOTS];
  int out_chans[MAX_SLOTS];
  int out_format[MAX_SLOTS];
  int out_bytes[MAX_SLOTS];
  int samples_per_chan;
  int last_device;
  int devices[MAX_SLOTS];
  int available_chans[MAX_SLOTS];
  int min_chans[MAX_SLOTS];
  int max_chans[MAX_SLOTS];
  int alloc_chans;
  if (argc == 1) 
    {
      printf("usage: sndplay file\n"); 
      exit(0);
    }
  mus_sound_initialize();

  /* for clisp-based clm use, we need a couple added switches
   * -describe => call mus_audio_describe and exit
   * -buffers axb => set OSS fragment numbers 
   */
  for (i = 1; i < argc; )
    {
      if (strcmp(argv[i], "-describe") == 0)
	{
	  mus_audio_describe(); 
	  exit(0);
	}
      else 
	if (strcmp(argv[i], "-buffers") == 0) 
	  {
	    set_buffers(argv[i+1]); 
	    i++;
	  }
      else name = argv[i];
      i++;
    }

  afd0 = -1;
  afd1 = -1;
  if (!(MUS_HEADER_TYPE_OK(mus_sound_header_type(name))))
    {
      fprintf(stderr, "can't play %s (header type: %s?)\n",
	      name,
	      mus_header_type_name(mus_header_type()));
      exit(0);
    }
  if (!(MUS_DATA_FORMAT_OK(mus_sound_data_format(name))))
    {
      fprintf(stderr, "can't play %s (data format: %s (%s)?)\n",
	      name,
	      mus_data_format_name(mus_sound_data_format(name)),
	      mus_header_original_format_name(mus_sound_original_format(name), 
					      mus_sound_header_type(name)));
      exit(0);
    }
  fd = mus_sound_open_input(name);
  if (fd != -1)
    {
      /* try to select proper device */
      float dir;
      int cards, card;
      int sysdev, devs, dev, d, i, im;
      int first_samples_per_chan = -1;
      cards = mus_audio_systems();
      /* deselect all devices */
      for (d = 0; d < MAX_SLOTS; d++) 
	{
	  out_devs[d] = -1;
	  afd[d] = -1;
	}
      /* Scan all cards and build a list of available output devices.
	 This is evil because it second guesses the intentions of the
	 user, best would be to have a command line parameter that
	 points to the device or devices to be used. For things to 
	 work under alsa .5 and multichannel cards it has to be here.
	 Hopefully the need will go away (in alsa .6 it will definitely
	 not be needed).
      */
      i = 0;
      im = 0;
      for (card = 0; card < cards; card++) 
	{
	  /* get the list of all available devices */
	  mus_audio_mixer_read(MUS_AUDIO_PACK_SYSTEM(card), MUS_AUDIO_PORT, MAX_SLOTS, val);
	  devs = (int)(val[0]);
	  for (d = 0; d < devs; d++) 
	    {
	      dev = (int)(val[d + 1]);
	      sysdev = MUS_AUDIO_PACK_SYSTEM(card)|dev;
	      mus_audio_mixer_read(sysdev, MUS_AUDIO_DIRECTION, 0, &dir);
	      /* only consider output devices */
	      if ((int)dir == 0) 
		{
		  float ch[4];
		  /* get the number of channels the device supports */
		  mus_audio_mixer_read(sysdev, MUS_AUDIO_CHANNEL, 4, ch);
		  available_chans[i] = (int)(ch[0]);
		  if ((int)ch[2] != 0) /* alsa also sets min and max channels */
		    {
		      min_chans[i] = (int)(ch[1]);
		      max_chans[i] = (int)(ch[2]);
		    }
		  if (max_chans[i] > max_chans[im]) im = i;
		  /* find out what format we can use with the device */
		  out_format[i] = mus_audio_compatible_format(sysdev);
		  /* find out what buffer size the device wants */
		  mus_audio_mixer_read(sysdev, MUS_AUDIO_SAMPLES_PER_CHANNEL, 2, ch);
		  samples_per_chan = (int)ch[0];
		  /* skip device if it has different buffer size, all must match */
		  if (first_samples_per_chan == -1) 
		    first_samples_per_chan = samples_per_chan;
		  else
		    if (samples_per_chan != first_samples_per_chan) 
		      continue;
		  devices[i++] = sysdev;
		  if (i >= MAX_SLOTS) 
		    goto NO_MORE_DEVICES;
		}
	    }
	}
    NO_MORE_DEVICES:
      last_device = i;
      chans = mus_sound_chans(name);
      allocated = 0;
      if (available_chans[im] >= chans)
	{
	  /* the widest device is wide enough to play all channels so we use it */
	  out_devs[allocated] = im;
	  out_chans[allocated] = chans;
	  if (chans < min_chans[im])
	    out_chans[allocated] = min_chans[im];
	  alloc_chans = out_chans[allocated];
	  allocated++;
	}
      else
	{
	  alloc_chans = 0;
	  if (use_one_device == 0) 
	    {
	      /* allocate devices until all channels can be played */
	      for (i = 0; i < last_device; i++) 
		{
		  out_devs[allocated] = i;
		  out_chans[allocated] = available_chans[i];
		  alloc_chans += available_chans[out_devs[allocated]];
		  allocated++;
		  if (alloc_chans >= chans) 
		    break;
		}
	    }
	  if (alloc_chans < chans)
	    {
	      /* FOR NOW, fail the program, not enough channels... */

	      fprintf(stderr, "not enough channels, %d available, %d needed\n", 
		      available_chans[0], chans);
	      exit(1);

	      /* either not enough channels found or have to use just
		 one device and the widest can't do it, so fold all of
		 them into whatever the first device can do */
	      allocated = 0;
	      out_devs[allocated] = 0;
	      out_chans[allocated] = available_chans[0];
	      alloc_chans = out_chans[allocated];
	      allocated++;
	    }
	}
      srate = mus_sound_srate(name);
      frames = mus_sound_samples(name)/chans;
      base = 0;
      /* allocate the list of read buffers, each buffer will hold one channel
	 of the input soundfile, each sample is going to be MUS_SAMPLE_TYPE
      */
      read_bufs = (MUS_SAMPLE_TYPE **)CALLOC(alloc_chans, sizeof(MUS_SAMPLE_TYPE *));
      for (d = 0; d < allocated; d++)
	{
	  int dev = out_devs[d];
	  for (i = 0; i < out_chans[d]; i++) 
	    read_bufs[base + i] = (MUS_SAMPLE_TYPE *)CALLOC(samples_per_chan, sizeof(MUS_SAMPLE_TYPE));
	  base += out_chans[d];
	  out_bytes[dev] = samples_per_chan * out_chans[d] * mus_data_format_to_bytes_per_sample(out_format[dev]);
	  out_buf[dev] = (short *)CALLOC(out_bytes[dev], 1);
	}
      for (i = 0; i < frames; i+=samples_per_chan)
	{
	  MUS_SAMPLE_TYPE **dev_bufs = read_bufs;
	  if ((i + samples_per_chan) <= frames)
	    curframes = samples_per_chan;
	  else curframes = frames - i;
	  mus_sound_read(fd, 0, curframes - 1, chans, read_bufs); 
	  /* some systems are happier if we read the file before opening the dac */
	  /* at this point the data is in separate arrays of MUS_SAMPLE_TYPE */
	  for (d = 0; d < allocated; d++)
	    {
	      int dev = out_devs[d];
	      mus_file_write_buffer(out_format[dev],
				    0, samples_per_chan - 1,
				    out_chans[d],
				    dev_bufs,
				    (char *)(out_buf[dev]),
				    0);
	      if (afd[dev] == -1)
		{
#if defined(PPC)
		  afd[dev] = mus_audio_open_output(devices[dev], srate, out_chans[d], out_format[dev], 0);
#else
		  afd[dev] = mus_audio_open_output(devices[dev], srate, out_chans[d], out_format[dev], out_bytes[dev]);
#endif
		  if (afd[dev] == -1) break; 
		}
	      mus_audio_write(afd[dev], (char *)out_buf[dev], out_bytes[dev]);
	      dev_bufs+=out_chans[d];
	    }
	}
      for (d = 0; d < allocated; d++)
	{
	  int dev = out_devs[d];
	  if (afd[dev] != -1) mus_audio_close(afd[dev]);
	}
      mus_sound_close_input(fd);
      for (i = 0; i < alloc_chans; i++) FREE(read_bufs[i]);
      FREE(read_bufs);
      for (d = 0; d < allocated; d++)
	{
	  int dev = out_devs[d];
	  FREE(out_buf[dev]);
	}
    }
  return(0);
}


#endif
