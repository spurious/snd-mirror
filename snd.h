#ifndef SND_H
#define SND_H

#include <mus-config.h>

#include <ctype.h>
#include <stddef.h>
#include <math.h>
#include <stdio.h>
#if HAVE_FCNTL_H
  #include <fcntl.h>
#endif
#include <signal.h>
#if HAVE_LIMITS_H
  #include <limits.h>
#endif
#include <errno.h>
#include <stdlib.h>
#if HAVE_LIBC_H && (!HAVE_UNISTD_H)
  #include <libc.h>
#else
  #ifndef _MSC_VER
    #include <unistd.h>
  #endif
#endif
#if HAVE_STRING_H
  #include <string.h>
#endif
#include <stdarg.h>
#include <time.h>
#include <sys/types.h>
#include <sys/stat.h>
#if HAVE_SETLOCALE
  #include <locale.h>
#endif
#if HAVE_FAM_H
  #include <fam.h>
#endif
#if MUS_WITH_THREADS && HAVE_PTHREAD_H
  #include <pthread.h>
#endif

#include "_sndlib.h"
#include "xen.h"
#include "clm.h"
#include "sndlib2xen.h"
#include "vct.h"
#include "snd-0.h"

#ifdef USE_MOTIF
  #include "snd-x0.h"
#else
  #if USE_GTK
    #include "snd-g0.h"
  #else
    #include "snd-nogui0.h"
  #endif
#endif

#include "snd-1.h"

#ifdef USE_MOTIF
  #include "snd-x1.h"
#else
  #if USE_GTK
    #include "snd-g1.h"
  #else
    #include "snd-nogui1.h"
  #endif
#endif

#include "snd-strings.h"

#define SND_DATE "30-Jan-07"
#define SND_VERSION "8.8"
#define SND_MAJOR_VERSION "8"
#define SND_MINOR_VERSION "8"

#endif
