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
#include "snd-0.h"
#include "xen.h"
#include "sndlib2xen.h"

#ifdef USE_MOTIF
  #include "snd-x0.h"
#endif
#if USE_GTK
  #include "snd-g0.h"
#endif
#if USE_NO_GUI
  #include "snd-nogui0.h"
#endif

#include "snd-1.h"

#ifdef USE_MOTIF
  #include "snd-x1.h"
#endif
#if USE_GTK
  #include "snd-g1.h"
#endif
#if USE_NO_GUI
  #include "snd-nogui1.h"
#endif

#include "snd-strings.h"

#define SND_VERSION "4-Dec-02"
#define SND_RPM_VERSION "6.4"
#define SND_MAJOR_VERSION 6
#define SND_MINOR_VERSION 4

#endif

