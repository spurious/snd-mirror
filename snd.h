#ifndef SND_H
#define SND_H

/* 
 * there are several compile-time options:
 *
 *   USE_GTK, USE_MOTIF, USE_NO_GUI
 *                 Motif is the default; the Gtk+ port is usable, but still has rough edges.
 *                 In the sources, snd-x* refers to Motif specific code, snd-g* is Gtk+ specific.
 *                   snd-gx* is used by both, but is X-specific.  If neither is defined,
 *                   you get a version of snd with no graphical user-interface (it is
 *                   driven by input from stdin). This version needs the flag USE_NO_GUI.
 *
 *   HAVE_GUILE    define if the guile library is available (adds lisp extension/customization language)
 *                 see http://www.gnu.org/software/guile (this is now a standard part of the Gnu distribution)
 *                 if you have Guile 1.3.4, include -DHAVE_GUILE_1_3,
 *                 (The default is Guile 1.4, versions before 1.3.4 are no longer supported).
 *                 To get a version of Snd without any extension language, -DHAVE_EXTENSION_LANGUAGE=0
 *
 *                 HAVE_LIBREP to use Librep instead of Guile as the extension language.
 *                     This option is currently on hold.
 * 
 *                 HAVE_MZSCHEME to use mzscheme instead of Guile as the extension language.
 *                     This option is not fully implemented yet.
 * 
 *   HAVE_GUILE_GTK
 *                 define if you're using Gtk+ and have libguilegtk.  The configure script
 *                 will define this if it can find the guile-gtk library and you're using Gtk+.
 *                 With guile 1.4, you need guile-gtk-0.19.
 *
 *   HAVE_XPM      define if you have the XPM library (the default in Linux and Gtk+)
 *                 (used to make cute icons in various places)
 *
 *   HAVE_LADSPA   define if you have LADSPA and want support for it included in Snd.
 *
 *   SNDLIB_USE_FLOATS and MUS_SAMPLE_BITS
 *                 These determine the internal representation of sample values. The default
 *                 is 0 for SNDLIB_USE_FLOATS (so 32-bit ints are used), and 24 for
 *                 MUS_SAMPLE_BITS (this sets the size of the "fraction").  Floats are
 *                 slightly faster on the Pentium.
 *
 *   USR_LIB_OSS, USR_LOCAL_LIB_OSS, OPT_OSS, VAR_LIB_OSS
 *                 define if OSS library/header files are in the indicated directory.
 *                 These are sometimes needed to find the correct version of soundcard.h
 *                 OSS is one of the main sound support providers for Linux.
 *                 http://www.4front-tech.com/ or maybe www.4front.com
 *   HAVE_ALSA     In Linux, OSS is the default audio system; HAVE_ALSA overrides this.
 *   HAVE_SAM_9407 Currently, this driver is only supported as a special case under OSS.
 *                 If the OSS code doesn't work as is, try adding -DHAVE_SAM_9407.
 *       there are many other similar audio choices (SGI, ESD, etc)
 *
 *   SND_CONF      global initialization file; under CCRMA switch defaults to /etc/snd.conf.
 *
 *   HAVE_CONFIG_H define if you're trying to build Snd via autoconf config files
 *                 (automatic if you use ./configure followed by make)
 *
 *   WITH_BIG_COLORMAP
 *                 if your video setup can handle 16 or more bits of color, define
 *                 this flag (via -DWITH_BIG_COLORMAP in CFLAGS for example), and
 *                 the sonogram colormaps will involve 512 colors, rather than 64.
 *
 *   HAVE_FPU_CONTROL_H
 *                 In Linux, we sometimes get NaNs which we would rather just set
 *                 to 0.0; if you don't want this to happen, or don't have the
 *                 file fpu_control.h or the function __setfpucw, define this
 *                 flag to be 0
 *
 *   SND_AS_WIDGET This causes the entire Snd editor to become a module loadable
 *                 into some other program as a kind of enormous widget (see saw.c).
 *
 *   HAVE_HTML     define if the XmHTML widget or the gtkhtml library is available 
 *                 (used by help functions -- perhaps better would be to call netscape?)
 *
 *   Float         This can be defined to double in CFLAGS if you want Snd to use 
 *                 doubles throughout (i.e. -DFloat=double).
 *
 *   STR_OR        British spelling enthusiasts can -DSTR_OR=\"our\" (this changes
 *                 every "color" to "colour" even in the resource names).
 *
 *   HAVE_GSL      use GSL (Gnu Scientific Library).
 *                 The intention is to use GSL for most of the standard math stuff that
 *                 isn't in the math library.  Currently this means the Bessel I0 function
 *                 (for the Kaiser window), the Hankel transform, the complex trig support
 *                 (for the Dolph-Chebyshev window), etc.  If you have a version after 0.7,
 *                 and you're making your own makefile, include the flag -DHAVE_GSL_DHT_NEW.
 *
 *   HAVE_GTKEXTRA If USE_GTK is set, and you want the Gtk+extra version of the file
 *                 selection widget, set this flag (it requires libgtkextra).
 *
 *  Many others are set by configure -- see config.h.in.
 */

#if defined(HAVE_CONFIG_H)
  #include "config.h"
#endif

#include <ctype.h>
#include <stddef.h>
#include <math.h>
#include <stdio.h>
#if (!defined(HAVE_CONFIG_H)) || (defined(HAVE_FCNTL_H))
  #include <fcntl.h>
#endif
#include <signal.h>
#if (!defined(HAVE_CONFIG_H)) || (defined(HAVE_LIMITS_H))
  #include <limits.h>
#endif
#include <errno.h>
#include <stdlib.h>

#if defined(HAVE_LIBC_H) && (!defined(HAVE_UNISTD_H))
  #include <libc.h>
#else
  #ifndef _MSC_VER
    #include <unistd.h>
  #endif
#endif

#if (!defined(HAVE_CONFIG_H)) || (defined(HAVE_STRING_H))
  #include <string.h>
#endif
#if (!defined(HAVE_CONFIG_H)) || (defined(HAVE_VPRINTF))
  #include <stdarg.h>
#endif

#include <time.h>
#include <sys/types.h>
#include <sys/stat.h>

#ifndef HAVE_EXTENSION_LANGUAGE
  #define HAVE_EXTENSION_LANGUAGE 1
  #if (!(HAVE_LIBREP || HAVE_MZSCHEME))
    #define HAVE_GUILE 1
  #endif
#endif

#if HAVE_GUILE
  #include <guile/gh.h>
#endif
#if HAVE_LIBREP
  #include <rep.h>
#endif
#if HAVE_MZSCHEME
  #include <scheme.h>
#endif

#if (!(USE_NO_GUI || USE_MOTIF || USE_GTK))
  #define USE_MOTIF 1
#endif

#include "sndlib.h"
#include "clm.h"
#include "snd-0.h"

#if HAVE_GUILE
  #include "sg.h"
#endif
#if HAVE_LIBREP 
  #include "sl.h"
#endif
#if HAVE_MZSCHEME
  #include "sz.h"
#endif
#if (!HAVE_EXTENSION_LANGUAGE)
  #include "noguile.h"
#endif

#include "sndlib2scm.h"

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

#define SND_VERSION "18-May-01"
#define SND_RPM_VERSION "4.13"
#define SND_MAJOR_VERSION 4
#define SND_MINOR_VERSION 13

#endif

