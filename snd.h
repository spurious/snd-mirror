#ifndef SND_H
#define SND_H

#include <config.h>

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
#if HAVE_VPRINTF
  #include <stdarg.h>
#endif
#include <time.h>
#include <sys/types.h>
#include <sys/stat.h>
#if HAVE_LOCALE_H && HAVE_SETLOCALE
  #include <locale.h>
#endif

#include "sndlib.h"
#include "clm.h"
#include "xen.h"
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

#define SND_DATE "25-Feb-05"
#define SND_VERSION "7.11"
#define SND_MAJOR_VERSION "7"
#define SND_MINOR_VERSION "11"

#endif
