/* audinfo decribes the current audio hardware state */

#include <config.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#if (defined(HAVE_LIBC_H) && (!defined(HAVE_UNISTD_H)))
  #include <libc.h>
#else
  #if (!(defined(_MSC_VER))) && (!(defined(MPW_C)))
    #include <unistd.h>
  #endif
  #include <string.h>
#endif

#include "sndlib.h"

int main(int argc, char *argv[])
{
  mus_sound_initialize();
  mus_audio_describe();
  return(0);
}
