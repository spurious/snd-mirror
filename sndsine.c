/* sndsine writes a sound file containing a sine wave */
/* cc -O -DLINUX io.o headers.o audio.o sound.o -o sndsine sndsine.c -lm */
/* sndsine test.snd */

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

#define BUFFER_SIZE 4096

int main(int argc, char *argv[])
{
  int fd,i,k,frames;
  float phase,incr;
  MUS_SAMPLE_TYPE *obuf[1];
#if MACOS
  argc = ccommand(&argv);
#endif
  if (argc == 1) {printf("usage: sndsine outfile\n"); exit(0);}
  mus_sound_initialize();
  fd = mus_sound_open_output(argv[1],22050,1,MUS_BSHORT,MUS_NEXT,"created by sndsine");
  if (fd != -1)
    {
      frames = 22050;
      phase = 0.0;
      incr = TWO_PI*440.0/22050.0;
      obuf[0] = (MUS_SAMPLE_TYPE *)CALLOC(BUFFER_SIZE,sizeof(MUS_SAMPLE_TYPE));
      k=0;
      for (i=0;i<frames;i++)
	{
	  obuf[0][k] = MUS_FLOAT_TO_SAMPLE(0.1 * sin(phase)); /* amp = .1 */
	  phase += incr;
	  k++;
	  if (k == BUFFER_SIZE)
	    {
	      mus_sound_write(fd,0,BUFFER_SIZE-1,1,obuf);
	      k=0;
	    }
	}
      if (k>0) mus_sound_write(fd,0,k-1,1,obuf);
      mus_sound_close_output(fd,22050*mus_data_format_to_bytes_per_sample(MUS_BSHORT));
      FREE(obuf[0]);
    }
  return(0);
}
