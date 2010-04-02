/* sndplay plays sounds */

#include <mus-config.h>

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

/* 22-Nov-00:  moved alsa support to separate block */
/* 8-Apr-04:   added start/end (seconds-based) args */
/* 2-Nov-05:   added -volume arg */
/* 22-July-08: added -mutable arg */

static int main_not_alsa(int argc, char *argv[])
{
  int fd, afd, i, j, n, k, chans, srate;
  mus_long_t frames, m;
  mus_sample_t **bufs;
  OutSample *obuf;
  int use_volume = 0;
  int afd0, afd1, buffer_size = BUFFER_SIZE, curframes, sample_size, out_chans, outbytes;
  char *name = NULL;
  mus_long_t start = 0, end = 0;
  double begin_time = 0.0, end_time = 0.0, volume = 1.0;
  int mutate = 1, include_mutate = 0;

  for (i = 1; i < argc; i++)
    {
      if (strcmp(argv[i], "-describe") == 0) 
	{
	  fprintf(stdout, "%s", mus_audio_describe()); 
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
			  if (strcmp(argv[i], "-mutable") == 0) 
			    {
			      mutate = atoi(argv[i + 1]);
			      include_mutate = 1;
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
			    }}}}}}}
  if (name == NULL) 
    {
      printf("usage: sndplay file [-start 1.0] [-end 1.0] [-bufsize %d] [-buffers 2x12] [-volume 1.0] [-mutable 1] [-describe]\n", BUFFER_SIZE); 
      exit(0);
    }

  afd = -1;
  afd0 = -1;
  afd1 = -1;
  if (!(mus_header_type_p(mus_sound_header_type(name))))
    {
      fprintf(stderr, "can't play %s (header type: %s?)\n",
	      name,
	      mus_header_type_name(mus_header_type()));
      exit(0);
    }
  if (!(mus_data_format_p(mus_sound_data_format(name))))
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
	  int available_chans;
	  available_chans = mus_audio_device_channels(MUS_AUDIO_DEFAULT);
	  if (available_chans < chans)
	    {
	      fprintf(stderr, "%s has %d channels, but we can only handle %d\n", name, chans, available_chans);
	      exit(1);
	    }
	}

      out_chans = chans;
      srate = mus_sound_srate(name);
      frames = mus_sound_frames(name);
      sample_size = mus_bytes_per_sample(MUS_AUDIO_COMPATIBLE_FORMAT);
      start = (mus_long_t)(begin_time * srate);
      if (start > 0)
	mus_file_seek_frame(fd, start);
      if (end_time > 0.0)
	end = (mus_long_t)(end_time * srate);
      else end = frames;
      if ((end - start) < frames)
	frames = end - start;

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
#if MUS_MAC_OSX
	  if (include_mutate == 1)
	    mus_audio_output_properties_mutable(mutate);
#endif
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
  return(0);
}

#if HAVE_ALSA

/* HAVE_ALSA here
   there should be a command line argument to control this,
   set to 0 to enable using more than one device. */

static int use_one_device = 1;

void mus_audio_alsa_channel_info(int dev, int *info);
int mus_audio_alsa_samples_per_channel(int dev);
void mus_audio_alsa_device_list(int sys, int size, int *val);
int mus_audio_alsa_device_direction(int dev);

#define MAX_SLOTS 64

static int main_alsa(int argc, char *argv[])
{
  int fd, i, chans, srate;
  mus_long_t frames, ioff;
  mus_sample_t **read_bufs;
  int afd[MAX_SLOTS];
  short *out_buf[MAX_SLOTS];
  int val[MAX_SLOTS];
  int afd0, afd1;
  char *name = NULL;
  int base, curframes;
  int allocated;
  int out_devs[MAX_SLOTS];
  int out_chans[MAX_SLOTS];
  int out_format[MAX_SLOTS];
  int out_bytes[MAX_SLOTS];
  int samples_per_chan = 0;
  int last_device;
  int devices[MAX_SLOTS];
  int available_chans[MAX_SLOTS];
  int min_chans[MAX_SLOTS];
  int max_chans[MAX_SLOTS];
  int alloc_chans;
  mus_long_t start = 0, end = 0;
  double begin_time = 0.0, end_time = 0.0, volume = 1.0;
  int use_volume = 0;

  /* -describe => call mus_audio_describe and exit
   * -buffers axb => set OSS fragment numbers 
   */
  for (i = 1; i < argc; i++)
    {
      if (strcmp(argv[i], "-describe") == 0)
	{
	  fprintf(stdout, "%s", mus_audio_describe()); 
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
  if (!(mus_header_type_p(mus_sound_header_type(name))))
    {
      fprintf(stderr, "can't play %s (header type: %s?)\n",
	      name,
	      mus_header_type_name(mus_header_type()));
      exit(0);
    }
  if (!(mus_data_format_p(mus_sound_data_format(name))))
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
      int dir = 0;
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
	  mus_audio_alsa_device_list(MUS_AUDIO_PACK_SYSTEM(card), MAX_SLOTS, val);
	  devs = val[0];
	  for (d = 0; d < devs; d++) 
	    {
	      dev = val[d + 1];
	      sysdev = MUS_AUDIO_PACK_SYSTEM(card)|dev;
	      dir = mus_audio_alsa_device_direction(sysdev);

	      /* only consider output devices */
	      if (dir == 0) 
		{
		  int ch[4];
		  /* get the number of channels the device supports */
		  mus_audio_alsa_channel_info(sysdev, ch);
		  available_chans[i] = ch[0];
		  if (ch[2] != 0) /* alsa also sets min and max channels */
		    {
		      min_chans[i] = ch[1];
		      max_chans[i] = ch[2];
		    }
		  if (max_chans[i] > max_chans[im]) im = i;

		  /* find out what format we can use with the device */
		  out_format[i] = mus_audio_compatible_format(sysdev);

		  /* find out what buffer size the device wants */
		  samples_per_chan = mus_audio_alsa_samples_per_channel(sysdev);

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
      start = (mus_long_t)(begin_time * srate);
      if (start > 0)
	mus_file_seek_frame(fd, start);
      if (end_time > 0.0)
	end = (mus_long_t)(end_time * srate);
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
  if (mus_audio_api() == MUS_ALSA_API)
    {
      main_alsa(argc, argv);
      return(0);
    }
#endif
  main_not_alsa(argc, argv);
  return(0);
}

