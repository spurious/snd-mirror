#ifndef _SNDLIB_H
#define _SNDLIB_H

#include <mus-config.h>

#if HAVE_UNISTD_H && (!(defined(_MSC_VER)))
  #include <unistd.h>
#endif

#include <sys/types.h>
#include <stdio.h>

#ifndef __cplusplus
#if HAVE_STDBOOL_H
  #include <stdbool.h>
#else
#ifndef true
  #define bool	int
  #define true	1
  #define false	0
#endif
#endif
#endif

#ifndef mus_float_t
#define mus_float_t double
#define mus_long_t long long int
#endif

#ifdef _MSC_VER
  /* I got these from gmp.h */
  #if defined (__GNUC__)
    #define MUS_EXPORT  __declspec(__dllexport__)
  #else
    #define MUS_EXPORT  __declspec(dllexport)
  #endif
#else
  #define MUS_EXPORT
#endif

#if (SIZEOF_SSIZE_T == 4)
  #if __APPLE__
    #define SSIZE_TD "%ld"
  #else
    #define SSIZE_TD "%d"
  #endif
#else
  #define SSIZE_TD "%lld"
#endif


/* these used to be in configure.ac,  but the 2.62 change to AC_C_BIGENDIAN ruins that */
#ifndef MUS_LITTLE_ENDIAN
  #if WORDS_BIGENDIAN
    #define MUS_LITTLE_ENDIAN 0
  #else
    #define MUS_LITTLE_ENDIAN 1
  #endif
#endif

#ifndef MUS_AUDIO_COMPATIBLE_FORMAT
  #if WORDS_BIGENDIAN
    #if __APPLE__
      #define MUS_AUDIO_COMPATIBLE_FORMAT MUS_BFLOAT
    #else
      #define MUS_AUDIO_COMPATIBLE_FORMAT MUS_BSHORT
    #endif
  #else
    #if __APPLE__
      #define MUS_AUDIO_COMPATIBLE_FORMAT MUS_LFLOAT
    #else
      #define MUS_AUDIO_COMPATIBLE_FORMAT MUS_LSHORT
    #endif
  #endif
#endif

#ifndef MUS_OUT_FORMAT
  #if WORDS_BIGENDIAN
    #define MUS_OUT_FORMAT MUS_BDOUBLE
  #else
    #define MUS_OUT_FORMAT MUS_LDOUBLE
  #endif
#endif


#ifndef c__FUNCTION__
#if (HAVE___FUNC__) || (defined(__STDC__) && defined(__STDC_VERSION__) && (__STDC_VERSION__ >= 199901L))
  #define c__FUNCTION__ __func__
#else
#ifdef __GNUC__
  #define c__FUNCTION__ __FUNCTION__
#else
  #define c__FUNCTION__ ""
#endif
#endif
#endif

#if (!defined(M_PI))
  #define M_PI 3.14159265358979323846264338327
  #define M_PI_2 (M_PI / 2.0)
#endif

#define POWER_OF_2_P(x)	((((x) - 1) & (x)) == 0)
/* from sys/param.h */

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

#ifndef S_setB
  #if HAVE_RUBY
    #define S_setB "set_"
  #endif
  #if HAVE_SCHEME
    #define S_setB "set! "
  #endif
  #if HAVE_FORTH
    #define S_setB "set-"
  #endif
  #if (!HAVE_EXTENSION_LANGUAGE)
    #define S_setB "set-"
  #endif
#endif

#define MUS_LOOP_INFO_SIZE 8
#define MUS_ALSA_API 0
#define MUS_OSS_API 1
#define MUS_JACK_API 2

#if (!HAVE_STRERROR)
  MUS_EXPORT char *strerror(int errnum);
#endif

#include "sndlib.h"
#include "xen.h"
#include "vct.h"

#endif
