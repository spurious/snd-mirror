#ifndef SND_H
#define SND_H

#if defined(HAVE_CONFIG_H)
  #include <config.h>
#else
  #define _FILE_OFFSET_BITS 64
#endif

#include <ctype.h>
#include <stddef.h>
#include <math.h>
#include <stdio.h>
#if (!defined(HAVE_CONFIG_H)) || HAVE_FCNTL_H
  #include <fcntl.h>
#endif
#include <signal.h>
#if (!defined(HAVE_CONFIG_H)) || HAVE_LIMITS_H
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

#if (!defined(HAVE_CONFIG_H)) || HAVE_STRING_H
  #include <string.h>
#endif
#if (!defined(HAVE_CONFIG_H)) || HAVE_VPRINTF
  #include <stdarg.h>
#endif

#include <time.h>
#include <sys/types.h>
#include <sys/stat.h>

#if ((!defined(HAVE_CONFIG_H)) || (HAVE_LOCALE_H && HAVE_SETLOCALE))
  #include <locale.h>
#endif

#ifndef HAVE_EXTENSION_LANGUAGE
  #define HAVE_EXTENSION_LANGUAGE 1
  #if (!HAVE_RUBY)
    #define HAVE_GUILE 1
  #endif
#endif

#if (!(USE_NO_GUI || USE_MOTIF || USE_GTK))
  #define USE_MOTIF 1
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

#define SND_VERSION "10-July-03"
#define SND_RPM_VERSION "6.11"
#define SND_MAJOR_VERSION 6
#define SND_MINOR_VERSION 11

#endif
