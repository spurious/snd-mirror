/* audinfo decribes the current audio hardware state */

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

#include "sndlib.h"

#ifdef DEBUG_MEMORY
void *mem_calloc(size_t len, size_t size, const char *func, const char *file, int line) {return(calloc(len, size));}
void *mem_malloc(size_t len, const char *func, const char *file, int line) {return(malloc(len));}
void mem_free(void *ptr, const char *func, const char *file, int line) {free(ptr);}
void *mem_realloc(void *ptr, size_t size, const char *func, const char *file, int line) {return(realloc(ptr, size));}
#endif

int main(int argc, char *argv[])
{
  mus_sound_initialize();
  mus_audio_describe();
  return(0);
}
