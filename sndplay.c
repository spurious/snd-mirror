/* sndplay plays sounds */

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

#ifdef LINUX
static char x_string[2] = {'x','\0'};
static void set_buffers(char *bufs)
{
  char *arg;
  int a,b;
  arg = strtok(bufs,x_string);
  a = atoi(arg);
  arg = strtok(NULL,x_string);
  b = atoi(arg);
  mus_audio_set_oss_buffers(a,b);
}
#endif

/* special case multicard quad code split out for clarity (it could be folded into the main branches) */

int main(int argc, char *argv[])
{
  int fd,afd,i,j,n,k,chans,srate,frames,outbytes;
  MUS_SAMPLE_TYPE **bufs;
  short *obuf;
  float val[1];
  int use_multi_card_code = 0;
  int afd0,afd1,buffer_size;
  MUS_SAMPLE_TYPE **qbufs;
  short *obuf0,*obuf1;
  char *name;
#if MACOS
  argc = ccommand(&argv);
#endif
  if (argc == 1) {printf("usage: sndplay file\n"); exit(0);}
  mus_sound_initialize();
#if LINUX
  /* for clisp-based clm use, we need a couple added switches
   * -describe => call mus_audio_describe and exit
   * -buffers axb => set OSS fragment numbers 
   */
  for (i=1;i<argc;)
    {
      if (strcmp(argv[i],"-describe") == 0) {mus_audio_describe(); exit(0);}
      else if (strcmp(argv[i],"-buffers") == 0) {set_buffers(argv[i+1]); i++;}
      else name = argv[i];
      i++;
    }
#else
  name = argv[1];
#endif

  afd = -1;
  afd0 = -1;
  afd1 = -1;
  fd = mus_sound_open_input(name);
  if (fd != -1)
    {
      chans = mus_sound_chans(name);
      if (chans > 2)
	{
	  mus_audio_mixer_read(MUS_AUDIO_DEFAULT,MUS_AUDIO_CHANNEL,0,val);
	  if (val[0] < chans)
	    {
	      if (mus_audio_systems() > 1)
		use_multi_card_code = 1;
	      /* I suppose we could count up all the channels here */
	      else
		{
		  fprintf(stderr,"%s has %d channels, but we can only handle %d\n",name,chans,(int)(val[0]));
		  exit(1);
		}
	    }
	}
      srate = mus_sound_srate(name);
      frames = mus_sound_samples(name)/chans;
      if (!use_multi_card_code)
	{
	  outbytes = BUFFER_SIZE * chans * 2;
	  bufs = (MUS_SAMPLE_TYPE **)CALLOC(chans,sizeof(MUS_SAMPLE_TYPE *));
	  for (i=0;i<chans;i++) bufs[i] = (MUS_SAMPLE_TYPE *)CALLOC(BUFFER_SIZE,sizeof(MUS_SAMPLE_TYPE));
	  obuf = (short *)CALLOC(BUFFER_SIZE * chans,sizeof(short));
	  /* assume for lafs that our DAC wants 16-bit integers */
	  mus_sound_read(fd,0,BUFFER_SIZE-1,chans,bufs); 
	  /* some systems are happier if we read the file before opening the dac */
	  /* at this point the data is in separate arrays of ints */
	  for (i=0;i<frames;i+=BUFFER_SIZE)
	    {
	      if (chans == 1)
		{
		  for (k=0;k<BUFFER_SIZE;k++) obuf[k] = MUS_SAMPLE_TO_SHORT(bufs[0][k]);
		}
	      else
		{
		  if (chans == 2)
		    {
		      for (k=0,n=0;k<BUFFER_SIZE;k++,n+=2) 
			{
			  obuf[n] = MUS_SAMPLE_TO_SHORT(bufs[0][k]); 
			  obuf[n+1] = MUS_SAMPLE_TO_SHORT(bufs[1][k]);
			}
		    }
		  else
		    {
		      for (k=0,j=0;k<BUFFER_SIZE;k++,j+=chans)
			{
			  for (n=0;n<chans;n++) obuf[j+n] = MUS_SAMPLE_TO_SHORT(bufs[n][k]);
			}
		    }
		}
	      if (afd == -1)
		{
#if defined(LINUX) && defined(PPC)
		  afd = mus_audio_open_output(MUS_AUDIO_DEFAULT,srate,chans,MUS_COMPATIBLE_FORMAT,0);
#else
		  afd = mus_audio_open_output(MUS_AUDIO_DEFAULT,srate,chans,MUS_COMPATIBLE_FORMAT,outbytes);
#endif
		  if (afd == -1) 
		    {
		      fprintf(stderr,"%s: %s\n",name,mus_audio_error_name(mus_audio_error()));
#ifdef BEOS
		      if ((mus_audio_error() == MUS_AUDIO_FORMAT_NOT_AVAILABLE) || 
			  (mus_audio_error() == MUS_AUDIO_CHANNELS_NOT_AVAILABLE) || 
			  (mus_audio_error() == MUS_AUDIO_SRATE_NOT_AVAILABLE))
			fprintf(stderr,"(Be can only play stereo 16-bit linear 44100 (or 22050) Hz files)\n");
#endif		  
		      break;
		    }
		}
	      mus_audio_write(afd,(char *)obuf,outbytes);
	      mus_sound_read(fd,0,BUFFER_SIZE-1,chans,bufs);
	    }
	  if (afd != -1) mus_audio_close(afd);
	  mus_sound_close_input(fd);
	  for (i=0;i<chans;i++) FREE(bufs[i]);
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
	  qbufs = (MUS_SAMPLE_TYPE **)CALLOC(chans,sizeof(MUS_SAMPLE_TYPE *));
	  for (i=0;i<chans;i++) qbufs[i] = (MUS_SAMPLE_TYPE *)CALLOC(buffer_size,sizeof(MUS_SAMPLE_TYPE));
	  obuf0 = (short *)CALLOC(buffer_size * 2,sizeof(short));
	  obuf1 = (short *)CALLOC(buffer_size * 2,sizeof(short));
	  mus_sound_read(fd,0,buffer_size-1,chans,qbufs); 
	  val[0] = 1.0;
	  for (i=0;i<frames;i+=buffer_size)
	    {
	      for (k=0,n=0;k<buffer_size;k++,n+=2) 
		{
		  obuf0[n] = MUS_SAMPLE_TO_SHORT(qbufs[0][k]); 
		  obuf0[n+1] = MUS_SAMPLE_TO_SHORT(qbufs[1][k]);
		  obuf1[n] = MUS_SAMPLE_TO_SHORT(qbufs[2][k]); 
		  obuf1[n+1] = MUS_SAMPLE_TO_SHORT(qbufs[3][k]);
		}
	      if (afd0 == -1)
		{
		  afd0 = mus_audio_open_output(MUS_AUDIO_PACK_SYSTEM(0) | MUS_AUDIO_DEFAULT,srate,2,MUS_COMPATIBLE_FORMAT,outbytes);
		  afd1 = mus_audio_open_output(MUS_AUDIO_PACK_SYSTEM(1) | MUS_AUDIO_DEFAULT,srate,2,MUS_COMPATIBLE_FORMAT,outbytes);
		  if ((afd0 == -1) || (afd1 == -1))
		    {
		      fprintf(stderr,"%s: %s\n",name,mus_audio_error_name(mus_audio_error()));
		      break;
		    }
		}
	      mus_audio_write(afd0,(char *)obuf0,outbytes);
	      mus_audio_write(afd1,(char *)obuf1,outbytes);
	      mus_sound_read(fd,0,buffer_size-1,chans,qbufs);
	    }
	  mus_audio_close(afd0);
	  mus_audio_close(afd1);
	  mus_sound_close_input(fd);
	  for (i=0;i<chans;i++) FREE(qbufs[i]);
	  FREE(qbufs);
	  FREE(obuf0);
	  FREE(obuf1);
	}
    }
  else
    fprintf(stderr,"%s: %s\n",name,mus_audio_error_name(mus_audio_error()));
  return(0);
}
