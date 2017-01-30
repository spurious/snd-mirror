#ifndef _SNDLIB_H
#define _SNDLIB_H

#include <mus-config.h>

#ifndef _MSC_VER
  #include <unistd.h>
#endif

#include <sys/types.h>
#include <stdio.h>

#if ((!__NetBSD__) && ((_MSC_VER) || (!defined(__STC__)) || (defined(__STDC_VERSION__) && (__STDC_VERSION__ < 199901L))))
  #define __func__ __FUNCTION__
#endif

#if (!defined(M_PI))
  #define M_PI 3.14159265358979323846264338327
  #define M_PI_2 (M_PI / 2.0)
#endif

#define is_power_of_2(x)	((((x) - 1) & (x)) == 0)

#if 0
#define clear_floats(Arr, Len) memset((void *)(Arr), 0, (Len) * sizeof(mus_float_t))
#define copy_floats(Dst, Src, Len) memcpy((void *)(Dst), (void *)(Src), (Len) * sizeof(mus_float_t))
#else
#define clear_floats(Arr, Len)			\
  do {						\
    mus_long_t K;				\
    mus_float_t *dst;				\
    dst = Arr;				\
    for (K = Len; K > 0; K--)		\
      *dst++ = 0.0;			\
  } while (0)
#define copy_floats(Dst, Src, Len)		\
  do {						\
    mus_long_t K;				\
    mus_float_t *dst, *src;			\
    dst = Dst;					\
    src = Src;					\
    for (K = Len; K > 0; K--)			\
      *dst++ = *src++;				\
    } while (0)
#endif

#define MUS_MAX_MALLOC_DEFAULT (1 << 26)
#define MUS_MAX_TABLE_SIZE_DEFAULT (1024 * 1024 * 20) /* delay line allocation etc */

#ifndef SEEK_SET
  #define SEEK_SET 0
  #define SEEK_END 2
#endif

#ifdef _MSC_VER
  #ifdef FOPEN
    #undef FOPEN
  #endif
  #if USE_SND
    #define OPEN(File, Flags, Mode) snd_open((File), (Flags), 0)
  #else
    #define OPEN(File, Flags, Mode) open((File), (Flags))
  #endif
#else
  #if USE_SND
    #define OPEN(File, Flags, Mode) snd_open((File), (Flags), (Mode))
   #else
    #define OPEN(File, Flags, Mode) open((File), (Flags), (Mode))
  #endif
#endif

#if USE_SND
  #define FOPEN(File, Flags)  snd_fopen((File), (Flags))
  #define CREAT(File, Flags)  snd_creat((File), (Flags))
  #define REMOVE(OldF)        snd_remove(OldF, IGNORE_CACHE)
  #define STRERROR(Err)       snd_io_strerror()
  #define CLOSE(Fd, Name)     snd_close(Fd, Name)
  #define FCLOSE(Fd, Name)    snd_fclose(Fd, Name)
#else
  #define FOPEN(File, Flags)  fopen((File), (Flags))
  #define CREAT(File, Flags)  creat((File), (Flags))
  #define REMOVE(OldF)        remove(OldF)
  #define STRERROR(Err)       strerror(Err)
  #define CLOSE(Fd, Name)     close(Fd)
  #define FCLOSE(Fd, Name)    fclose(Fd)
#endif

#ifndef S_set
  #if (!HAVE_EXTENSION_LANGUAGE)
    #define S_set "set-"
  #else
  #if HAVE_RUBY
    #define S_set "set_"
  #else
  #if HAVE_SCHEME
    #define S_set "set! "
  #else
  #if HAVE_FORTH
    #define S_set "set-"
  #endif
  #endif
  #endif
  #endif
#endif

#define MUS_LOOP_INFO_SIZE 8
#define MUS_ALSA_API 0
#define MUS_OSS_API 1
#define MUS_JACK_API 2

#define G7XX 0

#include "sndlib.h"
#include "xen.h"
#include "vct.h"

#endif
