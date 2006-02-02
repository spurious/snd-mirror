/* sndplay plays sounds */

#include <math.h>
#include <stdio.h>
#include <stdlib.h>

#include "_sndlib.h"

#if (defined(HAVE_LIBC_H) && (!defined(HAVE_UNISTD_H)))
  #include <libc.h>
#else
  #if (!(defined(_MSC_VER)))
    #include <unistd.h>
  #endif
  #include <string.h>
#endif
#include <errno.h>

#if MUS_MAC_OSX
  #define BUFFER_SIZE 256
#else
#if MUS_WINDOZE
  /* this setting from Scott Middleton (actually used 8096) */
  #define BUFFER_SIZE 8192
#else
  #define BUFFER_SIZE 4096
#endif
#endif

#if MUS_MAC_OSX
  #define OutSample float
  #define MUS_CONVERT(samp) MUS_SAMPLE_TO_FLOAT(samp)
#else
  #define OutSample short
  #define MUS_CONVERT(samp) MUS_SAMPLE_TO_SHORT(samp)
#endif

static void set_buffers(char *bufs)
{
#if (HAVE_OSS || HAVE_ALSA)
static char x_string[2] = {'x','\0'};
  char *arg;
  int a, b;
  arg = strtok(bufs, x_string);
  a = atoi(arg);
  arg = strtok(NULL, x_string);
  b = atoi(arg);
  mus_oss_set_buffers(a, b);
#endif
}

static void set_volume(mus_sample_t **buf, int chans, int length, double volume)
{ 
  int n, ch; 
  for (ch = 0; ch < chans; ch++) 
    for (n = 0; n < length; n++) 
      buf[ch][n] *= volume; 
}

/* special case multicard quad code split out for clarity (it could be folded into the main branches) */

/* 22-Nov-00: moved alsa support to separate block */
/* 8-Apr-04:  added start/end (seconds-based) args */
/* 2-Nov-05:  added -volume arg */

static int main_not_alsa(int argc, char *argv[])
{
  int fd, afd, i, j, n, k, chans, srate;
  off_t frames, m;
  mus_sample_t **bufs;
  OutSample *obuf;
  int use_multi_card_code = 0, use_volume = 0;
  int afd0, afd1, buffer_size = BUFFER_SIZE, curframes, sample_size, out_chans, outbytes;
  mus_sample_t **qbufs;
  short *obuf0, *obuf1;
  char *name = NULL;
  off_t start = 0, end = 0;
  double begin_time = 0.0, end_time = 0.0, volume = 1.0;

  for (i = 1; i < argc; i++)
    {
      if (strcmp(argv[i], "-describe") == 0) 
	{
	  mus_audio_describe(); 
	  exit(0);
	}
      else
	{
	  if (strcmp(argv[i], "-buffers") == 0) 
	    {
	      set_buffers(argv[i + 1]); 
	      i++;
	    }
	  else
	    {
	      if (strcmp(argv[i], "-bufsize") == 0) 
		{
		  buffer_size = atoi(argv[i + 1]);
		  i++;
		}
	      else
		{
		  if (strcmp(argv[i], "-start") == 0) 
		    {
		      begin_time = atof(argv[i + 1]);
		      i++;
		    }
		  else
		    {
		      if (strcmp(argv[i], "-end") == 0) 
			{
			  end_time = atof(argv[i + 1]);
			  i++;
			}
		      else 
			{ 
			  if (strcmp(argv[i], "-volume") == 0)
			    { 
			      volume = atof(argv[i + 1]);
			      use_volume = 1;
			      i++; 
			    } 
			  else name = argv[i];
			}}}}}}
  if (name == NULL) 
    {
      printf("usage: sndplay file [-start 1.0] [-end 1.0] [-bufsize %d] [-buffers 2x12] [-volume 1.0] [-describe]\n", BUFFER_SIZE); 
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
	  float val[8];
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
      out_chans = chans;
      srate = mus_sound_srate(name);
      frames = mus_sound_frames(name);
      sample_size = mus_bytes_per_sample(MUS_AUDIO_COMPATIBLE_FORMAT);
      start = (off_t)(begin_time * srate);
      if (start > 0)
	mus_file_seek_frame(fd, start);
      if (end_time > 0.0)
	end = (off_t)(end_time * srate);
      else end = frames;
      if ((end - start) < frames)
	frames = end - start;
      if (!use_multi_card_code)
	{
	  bufs = (mus_sample_t **)calloc(chans, sizeof(mus_sample_t *));
	  for (i = 0; i < chans; i++) bufs[i] = (mus_sample_t *)calloc(buffer_size, sizeof(mus_sample_t));
	  obuf = (OutSample *)calloc(buffer_size * out_chans, sizeof(OutSample));
	  outbytes = buffer_size * out_chans * sample_size;
	  for (m = 0; m < frames; m += buffer_size)
	    {
	      if ((m + buffer_size) <= frames)
		curframes = buffer_size;
	      else curframes = frames - m;
	      mus_file_read(fd, 0, curframes - 1, chans, bufs); 
	      /* some systems are happier if we read the file before opening the dac */
	      /* at this point the data is in separate arrays of mus_sample_t's */
	      if (use_volume) 
		set_volume(bufs, chans, curframes, volume); 
	      if (chans == 1)
		{
		  for (k = 0; k < curframes; k++) 
		    obuf[k] = MUS_CONVERT(bufs[0][k]);
		}
	      else
		{
		  if (chans == 2)
		    {
		      for (k = 0, n = 0; k < curframes; k++, n += 2) 
			{
			  obuf[n] = MUS_CONVERT(bufs[0][k]); 
			  obuf[n + 1] = MUS_CONVERT(bufs[1][k]);
			}
		    }
		  else
		    {
		      for (k = 0, j = 0; k < curframes; k++, j += chans)
			{
			  for (n = 0; n < chans; n++) 
			    obuf[j + n] = MUS_CONVERT(bufs[n][k]);
			}
		    }
		}
	      if (afd == -1)
		{
#if defined(MUS_LINUX) && defined(PPC)
		  afd = mus_audio_open_output(MUS_AUDIO_DEFAULT, srate, chans, MUS_AUDIO_COMPATIBLE_FORMAT, 0);
#else
		  afd = mus_audio_open_output(MUS_AUDIO_DEFAULT, srate, out_chans, MUS_AUDIO_COMPATIBLE_FORMAT, outbytes);
#endif
		  if (afd == -1) break;
		}
	      outbytes = curframes * out_chans * sample_size;
	      mus_audio_write(afd, (char *)obuf, outbytes);
	    }
	  if (afd != -1) mus_audio_close(afd);
	  mus_sound_close_input(fd);
	  for (i = 0; i < chans; i++) free(bufs[i]);
	  free(bufs);
	  free(obuf);
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
	  qbufs = (mus_sample_t **)calloc(chans, sizeof(mus_sample_t *));
	  for (i = 0; i < chans; i++) qbufs[i] = (mus_sample_t *)calloc(buffer_size, sizeof(mus_sample_t));
	  obuf0 = (short *)calloc(buffer_size * 2, sizeof(short));
	  obuf1 = (short *)calloc(buffer_size * 2, sizeof(short));
	  for (m = 0; m < frames; m += buffer_size)
	    {
	      if ((m + buffer_size) <= frames)
		curframes = buffer_size;
	      else curframes = frames - m;
	      mus_file_read(fd, 0, curframes - 1, chans, qbufs); 
	      if (use_volume) 
		set_volume(qbufs, chans, curframes, volume); 
	      for (k = 0, n = 0; k < buffer_size; k++, n += 2) 
		{
		  obuf0[n] = MUS_SAMPLE_TO_SHORT(qbufs[0][k]); 
		  obuf0[n + 1] = MUS_SAMPLE_TO_SHORT(qbufs[1][k]);
		  obuf1[n] = MUS_SAMPLE_TO_SHORT(qbufs[2][k]); 
		  obuf1[n + 1] = MUS_SAMPLE_TO_SHORT(qbufs[3][k]);
		}
	      if (afd0 == -1)
		{
		  afd0 = mus_audio_open_output(MUS_AUDIO_PACK_SYSTEM(0) | MUS_AUDIO_DEFAULT, srate, 2, MUS_AUDIO_COMPATIBLE_FORMAT, outbytes);
		  afd1 = mus_audio_open_output(MUS_AUDIO_PACK_SYSTEM(1) | MUS_AUDIO_DEFAULT, srate, 2, MUS_AUDIO_COMPATIBLE_FORMAT, outbytes);
		  if ((afd0 == -1) || (afd1 == -1)) break;
		}
	      mus_audio_write(afd0, (char *)obuf0, outbytes);
	      mus_audio_write(afd1, (char *)obuf1, outbytes);
	    }
	  mus_audio_close(afd0);
	  mus_audio_close(afd1);
	  mus_sound_close_input(fd);
	  for (i = 0; i < chans; i++) free(qbufs[i]);
	  free(qbufs);
	  free(obuf0);
	  free(obuf1);
	}
    }
  return(0);
}

#if HAVE_ALSA

/* HAVE_ALSA here
   there should be a command line argument to control this,
   set to 0 to enable using more than one device. */

static int use_one_device = 1;

#define MAX_SLOTS 64

static int main_alsa(int argc, char *argv[])
{
  int fd, i, chans, srate;
  off_t frames, ioff;
  mus_sample_t **read_bufs;
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
  off_t start = 0, end = 0;
  double begin_time = 0.0, end_time = 0.0, volume = 1.0;
  int use_volume = 0;

  /* -describe => call mus_audio_describe and exit
   * -buffers axb => set OSS fragment numbers 
   */
  for (i = 1; i < argc; i++)
    {
      if (strcmp(argv[i], "-describe") == 0)
	{
	  mus_audio_describe(); 
	  exit(0);
	}
      else 
	{
	  if (strcmp(argv[i], "-buffers") == 0) 
	    {
	      set_buffers(argv[i+1]); 
	      i++;
	    }
	  else
	    {
	      if (strcmp(argv[i], "-start") == 0) 
		{
		  begin_time = atof(argv[i + 1]);
		  i++;
		}
	      else
		{
		  if (strcmp(argv[i], "-end") == 0) 
		    {
		      end_time = atof(argv[i + 1]);
		      i++;
		    }
		  else
		    {
		      if (strcmp(argv[i], "-volume") == 0)
			{ 
			  volume = atof(argv[i + 1]);
			  use_volume = 1;
			  i++; 
			} 
		      else name = argv[i];
		    }}}}}
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
      frames = mus_sound_frames(name);
      base = 0;
      start = (off_t)(begin_time * srate);
      if (start > 0)
	mus_file_seek_frame(fd, start);
      if (end_time > 0.0)
	end = (off_t)(end_time * srate);
      else end = frames;
      if ((end - start) < frames)
	frames = end - start;
      /* allocate the list of read buffers, each buffer will hold one channel
	 of the input soundfile, each sample is going to be mus_sample_t
      */
      read_bufs = (mus_sample_t **)calloc(alloc_chans, sizeof(mus_sample_t *));
      for (d = 0; d < allocated; d++)
	{
	  int dev = out_devs[d];
	  for (i = 0; i < out_chans[d]; i++) 
	    read_bufs[base + i] = (mus_sample_t *)calloc(samples_per_chan, sizeof(mus_sample_t));
	  base += out_chans[d];
	  out_bytes[dev] = samples_per_chan * out_chans[d] * mus_bytes_per_sample(out_format[dev]);
	  out_buf[dev] = (short *)calloc(out_bytes[dev], 1);
	}
      for (ioff = 0; ioff < frames; ioff += samples_per_chan)
	{
	  mus_sample_t **dev_bufs = read_bufs;
	  if ((ioff + samples_per_chan) <= frames)
	    curframes = samples_per_chan;
	  else 
	    {
	      curframes = frames - ioff;
	      for (d = 0; d < allocated; d++)
		{
		  int f, dev = out_devs[d];
#if 1
		  /* try to kludge around an ALSA bug... */
		  for (f = 0; f < chans; f++) 
		    memset(read_bufs[f], 0, samples_per_chan * sizeof(mus_sample_t));
#endif
		  out_bytes[dev] = curframes * out_chans[d] * mus_bytes_per_sample(out_format[dev]);
		}
	    }
	  mus_file_read(fd, 0, curframes - 1, chans, read_bufs); 
	  if (use_volume) 
	    set_volume(read_bufs, chans, curframes, volume); 
	  /* some systems are happier if we read the file before opening the dac */
	  /* at this point the data is in separate arrays of mus_sample_t */
	  for (d = 0; d < allocated; d++)
	    {
	      int dev = out_devs[d];
	      mus_file_write_buffer(out_format[dev],
				    0, curframes - 1,
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
	      dev_bufs += out_chans[d];
	    }
	}
      for (d = 0; d < allocated; d++)
	{
	  int dev = out_devs[d];
	  if (afd[dev] != -1) mus_audio_close(afd[dev]);
	}
      mus_sound_close_input(fd);
      for (i = 0; i < alloc_chans; i++) free(read_bufs[i]);
      free(read_bufs);
      for (d = 0; d < allocated; d++)
	{
	  int dev = out_devs[d];
	  free(out_buf[dev]);
	}
    }
  return(0);
}

#endif

int main(int argc, char *argv[])
{
  if (argc == 1) 
    {
      printf("usage: sndplay file [-start 1.0] [-end 1.0] [-bufsize %d] [-buffers 2x12] [-volume 1.0] [-describe]\n", BUFFER_SIZE); 
      exit(0);
    }
  mus_sound_initialize();
 
#if HAVE_ALSA
  if (mus_audio_api() == ALSA_API)
    {
      main_alsa(argc, argv);
      return(0);
    }
#endif
  main_not_alsa(argc, argv);
  return(0);
}

