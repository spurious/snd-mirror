/* sndxtest.c, part of the Snd test package
 * sndxtest infile outfile doubles all samps in infile writing outfile 
 */
#if defined(HAVE_CONFIG_H)
  #include "config.h"
#endif

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include "sndlib.h"
#define BUFFER_SIZE 4096

int main(int argc, char *argv[])
{
  int ofd, ifd, i, j, k, frames, chans, srate, df, ht, curframes;
  #define INFILE argv[1]
  #define OUTFILE argv[2]
  MUS_SAMPLE_TYPE **obuf;
  mus_sound_initialize();
  chans = mus_sound_chans(INFILE);
  srate = mus_sound_srate(INFILE);
  ht = mus_sound_header_type(INFILE);
  df = mus_sound_data_format(INFILE);
  frames = mus_sound_frames(INFILE);
  ofd = mus_sound_open_output(OUTFILE, srate, chans, df, ht, "created by sndxtest");
  ifd = mus_sound_open_input(INFILE);
  obuf = (MUS_SAMPLE_TYPE **)CALLOC(chans, sizeof(MUS_SAMPLE_TYPE *));
  for (i = 0; i < chans; i++)
    obuf[i] = (MUS_SAMPLE_TYPE *)CALLOC(BUFFER_SIZE, sizeof(MUS_SAMPLE_TYPE));
  for (i = 0; i < frames; i += BUFFER_SIZE)
    {
      if ((i + BUFFER_SIZE) <= frames)
	curframes = BUFFER_SIZE;
      else curframes = frames - i;
      mus_sound_read(ifd, 0, curframes - 1, chans, obuf);
      for (j = 0; j < chans; j++)
	for (k = 0; k < curframes; k++)
	  obuf[j][k] *= 2.0;
      mus_sound_write(ofd, 0, curframes - 1, chans, obuf);
    }
  mus_sound_close_output(ofd, frames * mus_data_format_to_bytes_per_sample(df) * chans);
  mus_sound_close_input(ifd);
  return(0);
}
